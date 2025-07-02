{
ST Microelectronics STMPE Driver.

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
 
  Linux - \drivers\input\touchscreen\stmpe-ts.c - Copyright (C) 2010 Luotao Fu
  Linux - \drivers\gpio\gpio-stmpe.c - Copyright (C) ST-Ericsson SA 2010
  Linux - \drivers\mfd\stmpe.c - Copyright (C) ST-Ericsson SA 2010
  Linux - \drivers\mfd\stmpe-i2c.c - Copyright (C) ST Microelectronics SA 2011
  Linux - \drivers\mfd\stmpe-spi.c - Copyright (C) ST Microelectronics SA 2011
  Linux - \drivers\input\keyboard\stmpe-keypad.c - Copyright (C) ST-Ericsson SA 2010
  
References
==========

 STMPE610  - https://cdn-shop.adafruit.com/datasheets/STMPE610.pdf
 STMPE801  - http://www.st.com/content/ccc/resource/technical/document/datasheet/cd/4e/f6/e6/2b/25/40/70/CD00144085.pdf/files/CD00144085.pdf/jcr:content/translations/en.CD00144085.pdf
 STMPE811  - https://media.digikey.com/pdf/Data%20Sheets/ST%20Microelectronics%20PDFS/STMPE811.pdf
 STMPE1601 - http://www.st.com/content/ccc/resource/technical/document/datasheet/group2/1d/fa/c9/ac/e1/2d/46/90/CD00181973/files/CD00181973.pdf/jcr:content/translations/en.CD00181973.pdf
 STMPE1801 - http://www.st.com/content/ccc/resource/technical/document/datasheet/d3/59/6b/f2/68/b1/4d/df/CD00283903.pdf/files/CD00283903.pdf/jcr:content/translations/en.CD00283903.pdf
 STMPE2401 - http://www.st.com/content/ccc/resource/technical/document/datasheet/group2/82/01/70/46/c0/e5/4b/db/CD00148671/files/CD00148671.pdf/jcr:content/translations/en.CD00148671.pdf
 STMPE2403 - http://www.st.com/content/ccc/resource/technical/document/datasheet/b5/d7/b3/d2/85/9d/44/b7/CD00162378.pdf/files/CD00162378.pdf/jcr:content/translations/en.CD00162378.pdf
 
ST Microelectronics STMPE
=========================
 
 The ST Microelectronics STMPE devices are a range of multi function chips that include GPIO, ADC, 4 wire touchscreen
 controller and PWM outputs in varying combinations. This driver supports the GPIO and Touchscreen functions of the
 following chips. Many of the functions overlap so a pin can be used for example as a GPIO or a PWM but not both at
 once. The Touchscreen controller also consumes many of the GPIO pins in the models that support it.
 
 STMPE610 - 6 GPIO / 4-wire Touch / I2C / SPI
 
 STMPE801 - 8 GPIO / I2C
 
 STMPE811 - 8 GPIO / 4-wire Touch / I2C / SPI
 
 STMPE1601 - 16 GPIO / 4 PWM / I2C
 
 STMPE1801 - 18 GPIO / I2C
 
 STMPE2401 - 24 GPIO / 3 PWM / I2C
 
 STMPE2403 - 24 GPIO / 3 PWM / I2C 
 
 Note: A number of variations also support Keypad, Temperature and other features which are not currently handled by
 this driver.
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit STMPE;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,GPIO,SPI,I2C,Touch,Mouse,SysUtils;
     
{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {STMPE specific constants}
 STMPE610_GPIO_DESCRIPTION  = 'STMicroelectronics STMPE610 I/O Expander';      {Description of STMPE610 GPIO device}
 STMPE801_GPIO_DESCRIPTION  = 'STMicroelectronics STMPE801 I/O Expander';      {Description of STMPE801 GPIO device}
 STMPE811_GPIO_DESCRIPTION  = 'STMicroelectronics STMPE811 I/O Expander';      {Description of STMPE811 GPIO device}
 STMPE1601_GPIO_DESCRIPTION = 'STMicroelectronics STMPE1601 I/O Expander';     {Description of STMPE1601 GPIO device}
 STMPE1801_GPIO_DESCRIPTION = 'STMicroelectronics STMPE1801 I/O Expander';     {Description of STMPE1801 GPIO device}
 STMPE2401_GPIO_DESCRIPTION = 'STMicroelectronics STMPE2401 I/O Expander';     {Description of STMPE2401 GPIO device}
 STMPE2403_GPIO_DESCRIPTION = 'STMicroelectronics STMPE2403 I/O Expander';     {Description of STMPE2403 GPIO device}

 STMPE610_TOUCH_DESCRIPTION = 'STMicroelectronics STMPE610 Touch Controller';  {Description of STMPE610 Touch device}
 STMPE811_TOUCH_DESCRIPTION = 'STMicroelectronics STMPE811 Touch Controller';  {Description of STMPE811 Touch device}
 
 STMPE610_GPIO_MIN_PIN = GPIO_PIN_2;
 STMPE610_GPIO_MAX_PIN = GPIO_PIN_7;
 STMPE610_GPIO_PIN_COUNT = 6;

 STMPE801_GPIO_MIN_PIN = GPIO_PIN_0;
 STMPE801_GPIO_MAX_PIN = GPIO_PIN_7;
 STMPE801_GPIO_PIN_COUNT = 8;

 STMPE811_GPIO_MIN_PIN = GPIO_PIN_0;
 STMPE811_GPIO_MAX_PIN = GPIO_PIN_7;
 STMPE811_GPIO_PIN_COUNT = 8;
 
 STMPE1601_GPIO_MIN_PIN = GPIO_PIN_0;
 STMPE1601_GPIO_MAX_PIN = GPIO_PIN_15;
 STMPE1601_GPIO_PIN_COUNT = 16;
 
 STMPE1801_GPIO_MIN_PIN = GPIO_PIN_0;
 STMPE1801_GPIO_MAX_PIN = GPIO_PIN_17;
 STMPE1801_GPIO_PIN_COUNT = 18;

 STMPE240X_GPIO_MIN_PIN = GPIO_PIN_0;
 STMPE240X_GPIO_MAX_PIN = GPIO_PIN_23;
 STMPE240X_GPIO_PIN_COUNT = 24;
 
 STMPE_GPIO_MAX_LEVEL = GPIO_LEVEL_HIGH;
 
 STMPE_GPIO_MAX_PULL = GPIO_PULL_DOWN;
 
 STMPE610_GPIO_MIN_FUNCTION = GPIO_FUNCTION_IN;
 STMPE610_GPIO_MAX_FUNCTION = GPIO_FUNCTION_ALT0;
 STMPE610_GPIO_FUNCTION_COUNT = 3;

 STMPE801_GPIO_MIN_FUNCTION = GPIO_FUNCTION_IN;
 STMPE801_GPIO_MAX_FUNCTION = GPIO_FUNCTION_OUT;
 STMPE801_GPIO_FUNCTION_COUNT = 2;

 STMPE811_GPIO_MIN_FUNCTION = GPIO_FUNCTION_IN;
 STMPE811_GPIO_MAX_FUNCTION = GPIO_FUNCTION_ALT0;
 STMPE811_GPIO_FUNCTION_COUNT = 3;
 
 STMPE1601_GPIO_MIN_FUNCTION = GPIO_FUNCTION_IN;
 STMPE1601_GPIO_MAX_FUNCTION = GPIO_FUNCTION_ALT1; {Alternate function 0 = Keypad / Alternate function 1 = PWM}
 STMPE1601_GPIO_FUNCTION_COUNT = 4;
 
 STMPE1801_GPIO_MIN_FUNCTION = GPIO_FUNCTION_IN;
 STMPE1801_GPIO_MAX_FUNCTION = GPIO_FUNCTION_ALT0;
 STMPE1801_GPIO_FUNCTION_COUNT = 3;

 STMPE240X_GPIO_MIN_FUNCTION = GPIO_FUNCTION_IN;
 STMPE240X_GPIO_MAX_FUNCTION = GPIO_FUNCTION_ALT2;
 STMPE240X_GPIO_FUNCTION_COUNT = 5;
 
 STMPE610_MAX_POINTS = 1;
 STMPE610_MAX_X = $FFF;
 STMPE610_MAX_Y = $FFF;
 STMPE610_MAX_Z = $FF;
 
 STMPE811_MAX_POINTS = 1;
 STMPE811_MAX_X = $FFF;
 STMPE811_MAX_Y = $FFF;
 STMPE811_MAX_Z = $FF;
 
 {STMPE Chip constants}
 STMPE_CHIP_STMPE610   = 1;
 STMPE_CHIP_STMPE801   = 2;
 STMPE_CHIP_STMPE811   = 3;
 STMPE_CHIP_STMPE1601  = 4;
 STMPE_CHIP_STMPE1801  = 5;
 STMPE_CHIP_STMPE2401  = 6;
 STMPE_CHIP_STMPE2403  = 7;
 
 {STMPE Direction constants}
 STMPE_DIR_ASCENDING  = 0; {Register addresses in ascending order (Bits go from LSB to MSB)}
 STMPE_DIR_DESCENDING = 1; {Register addresses in descending order (Bits go from MSB to LSB)}
 
 {STMPE I2C constants}
 STMPE_I2C_RATE = 400000; {Default I2C clock rate (Device supports 100KHz and 400KHz)}

 {STMPE SPI constants}
 STMPE_SPI_RATE = 500000; {Default SPI clock rate (Device supports up to 1MHz)}
 
 STMPE_SPI_READ_CMD = (1 shl 7); {First bit of address must be set for a read}
 
 {STMPE Register constants}
 {STMPE610 / STMPE811}
 STMPE811_REG_CHIP_ID        = $00; {Device identification}
 STMPE811_REG_ID_VER         = $02; {Revision number (0x01 for engineering sample / 0x03 for final silicon)}
 STMPE811_REG_SYS_CTRL1      = $03; {Reset control}
 STMPE811_REG_SYS_CTRL2      = $04; {Clock control}
 STMPE811_REG_SPI_CFG        = $08; {SPI interface configuration}
 STMPE811_REG_INT_CTRL       = $09; {Interrupt control register}
 STMPE811_REG_INT_EN         = $0A; {Interrupt enable register}
 STMPE811_REG_INT_STA        = $0B; {Interrupt status register}
 
 STMPE811_REG_GPIO_INT_EN    = $0C; {GPIO interrupt enable register}
 STMPE811_REG_GPIO_INT_STA   = $0D; {GPIO interrupt status register}
 STMPE811_REG_ADC_INT_EN     = $0E; {ADC interrupt enable register}
 STMPE811_REG_ADC_INT_STA    = $0F; {ADC interrupt status register}
 STMPE811_REG_GPIO_SET_PIN   = $10; {GPIO set pin register}
 STMPE811_REG_GPIO_CLR_PIN   = $11; {GPIO clear pin register}
 STMPE811_REG_GPIO_MP_STA    = $12; {GPIO monitor pin state register}
 STMPE811_REG_GPIO_SET_DIR   = $13; {GPIO direction register}
 STMPE811_REG_GPIO_ED        = $14; {GPIO edge detect register}
 STMPE811_REG_GPIO_RE        = $15; {GPIO rising edge register}
 STMPE811_REG_GPIO_FE        = $16; {GPIO falling edge register}
 STMPE811_REG_GPIO_AF        = $17; {Alternate function register}
 
 STMPE811_REG_ADC_CTRL1      = $20; {ADC control}
 STMPE811_REG_ADC_CTRL2      = $21; {ADC control}
 STMPE811_REG_ADC_CAPT       = $22; {To initiate ADC data acquisition}
 STMPE811_REG_ADC_DATA_CH0   = $30; {ADC channel 0}
 STMPE811_REG_ADC_DATA_CH1   = $32; {ADC channel 1}
 STMPE811_REG_ADC_DATA_CH2   = $34; {ADC channel 2}
 STMPE811_REG_ADC_DATA_CH3   = $36; {ADC channel 3}
 STMPE811_REG_ADC_DATA_CH4   = $38; {ADC channel 4}
 STMPE811_REG_ADC_DATA_CH5   = $3A; {ADC channel 5}
 STMPE811_REG_ADC_DATA_CH6   = $3C; {ADC channel 6}
 STMPE811_REG_ADC_DATA_CH7   = $3E; {ADC channel 7}
 
 STMPE811_REG_TSC_CTRL       = $40; {4-wire touchscreen controller setup}
 STMPE811_REG_TSC_CFG        = $41; {Touchscreen controller configuration}
 STMPE811_REG_WDW_TR_X       = $42; {Window setup for top right X}
 STMPE811_REG_WDW_TR_Y       = $44; {Window setup for top right Y}
 STMPE811_REG_WDW_BL_X       = $46; {Window setup for bottom left X}
 STMPE811_REG_WDW_BL_Y       = $48; {Window setup for bottom left Y}
 STMPE811_REG_FIFO_TH        = $4A; {FIFO level to generate interrupt}
 STMPE811_REG_FIFO_STA       = $4B; {Current status of FIFO}
 STMPE811_REG_FIFO_SIZE      = $4C; {Current filled level of FIFO}
 STMPE811_REG_TSC_DATA_X     = $4D; {Data port for touchscreen controller data access}
 STMPE811_REG_TSC_DATA_Y     = $4F; {Data port for touchscreen controller data access}
 STMPE811_REG_TSC_DATA_Z     = $51; {Data port for touchscreen controller data access}
 STMPE811_REG_TSC_DATA_XYZ   = $52; {Data port for touchscreen controller data access}
 STMPE811_REG_TSC_FRACTION_Z = $56; {Touchscreen controller FRACTION_Z}
 STMPE811_REG_TSC_DATA       = $57; {Data port for touchscreen controller data access}
 STMPE811_REG_TSC_I_DRIVE    = $58; {Touchscreen controller drive I}
 STMPE811_REG_TSC_SHIELD     = $59; {Touchscreen controller shield}
 {STMPE811}
 STMPE811_REG_TEMP_CTRL      = $60; {Temperature sensor setup}
 STMPE811_REG_TEMP_DATA      = $61; {Temperature data access port}
 STMPE811_REG_TEMP_TH        = $62; {Threshold for temperature controlled interrupt}
 
 STMPE811_REG_MAX = $62;
 STMPE811_REG_SIZE = 1;
 
 {STMPE801}
 STMPE801_REG_CHIP_ID        = $00; {Device identification}
 STMPE801_REG_ID_VER         = $02; {Revision number (0x01 for engineering sample / 0x02 for final silicon)}
 STMPE801_REG_SYS_CTRL       = $04; {Reset and interrupt control}
 STMPE801_REG_GPIO_INT_EN    = $08; {GPIO interrupt enable register}
 STMPE801_REG_GPIO_INT_STA   = $09; {GPIO interrupt status register}
 STMPE801_REG_GPIO_MP_STA    = $10; {GPIO monitor pin state register}
 STMPE801_REG_GPIO_SET_PIN   = $11; {GPIO set pin state register}
 STMPE801_REG_GPIO_SET_DIR   = $12; {GPIO set pin direction register}
 
 STMPE801_REG_MAX = $12;
 STMPE801_REG_SIZE = 1;
 
 {STMPE1601}
 STMPE1601_REG_CHIP_ID          = $80; {Device identification}
 STMPE1601_REG_ID_VER           = $81; {Revision number}
 
 STMPE1601_REG_SYS_CTRL         = $02; {System control register}
 STMPE1601_REG_SYS_CTRL2        = $03; {System control register 2}
 STMPE1601_REG_INT_CTRL_MSB     = $10; {Interrupt control register}
 STMPE1601_REG_INT_CTRL_LSB     = $11; 
 STMPE1601_REG_INT_EN_MSB       = $12; {Interrupt enable mask register}
 STMPE1601_REG_INT_EN_LSB       = $13; 
 STMPE1601_REG_INT_STA_MSB      = $14; {Interrupt status register}
 STMPE1601_REG_INT_STA_LSB      = $15; 

 STMPE1601_REG_GPIO_INT_EN_MSB  = $16; {Interrupt enable GPIO mask register}
 STMPE1601_REG_GPIO_INT_EN_LSB  = $17; 
 STMPE1601_REG_GPIO_INT_STA_MSB = $18; {Interrupt status GPIO register}
 STMPE1601_REG_GPIO_INT_STA_LSB = $19; 
 
 STMPE1601_REG_GPIO_SET_PIN_MSB = $82; {GPIO set pin state register}
 STMPE1601_REG_GPIO_SET_PIN_LSB = $83; 
 STMPE1601_REG_GPIO_CLR_PIN_MSB = $84; {GPIO clear pin state register}
 STMPE1601_REG_GPIO_CLR_PIN_LSB = $85; 
 STMPE1601_REG_GPIO_MP_STA_MSB  = $86; {GPIO monitor pin state register}
 STMPE1601_REG_GPIO_MP_STA_LSB  = $87; 
 STMPE1601_REG_GPIO_SET_DIR_MSB = $88; {GPIO set pin direction register}
 STMPE1601_REG_GPIO_SET_DIR_LSB = $89; 
 
 STMPE1601_REG_GPIO_ED_MSB      = $8A; {GPIO edge detect status register}
 STMPE1601_REG_GPIO_ED_LSB      = $8B; 
 STMPE1601_REG_GPIO_RE_MSB      = $8C; {GPIO rising edge register}
 STMPE1601_REG_GPIO_RE_LSB      = $8D; 
 STMPE1601_REG_GPIO_FE_MSB      = $8E; {GPIO falling edge register}
 STMPE1601_REG_GPIO_FE_LSB      = $8F; 
 STMPE1601_REG_GPIO_PU_MSB      = $90; {GPIO pull up register}
 STMPE1601_REG_GPIO_PU_LSB      = $91; 
 STMPE1601_REG_GPIO_AF_U_MSB    = $92; {GPIO alternate function register (upper word)}
 STMPE1601_REG_GPIO_AF_U_LSB    = $93; 
 STMPE1601_REG_GPIO_AF_L_MSB    = $94; {GPIO alternate function register (lower word)}
 STMPE1601_REG_GPIO_AF_L_LSB    = $95; 

 STMPE1601_REG_GPIO_LT_EN       = $96; {GPIO level translator enable}
 STMPE1601_REG_GPIO_LT_DIR      = $97; {GPIO level translator direction}
 
 STMPE1601_REG_MAX = $BF;
 STMPE1601_REG_SIZE = 2;
 
 {STMPE1801}
 STMPE1801_REG_CHIP_ID           = $00; {Device identification}
 STMPE1801_REG_ID_VER            = $01; {Revision number}
                                 
 STMPE1801_REG_SYS_CTRL          = $02; {System control}
 STMPE1801_REG_INT_CTRL_LOW      = $04; {Interrupt control}
 STMPE1801_REG_INT_CTRL_HIGH     = $05; 
 STMPE1801_REG_INT_EN_LOW        = $06; {Interrupt enable mask}
 STMPE1801_REG_INT_EN_HIGH       = $07; 
 STMPE1801_REG_INT_STA_LOW       = $08; {Interrupt status}
 STMPE1801_REG_INT_STA_HIGH      = $09; 
 
 STMPE1801_REG_GPIO_INT_EN_LOW   = $0A; {Interrupt enable GPIO mask}
 STMPE1801_REG_GPIO_INT_EN_MID   = $0B; 
 STMPE1801_REG_GPIO_INT_EN_HIGH  = $0C; 
 STMPE1801_REG_GPIO_INT_STA_LOW  = $0D; {Interrupt status GPIO}
 STMPE1801_REG_GPIO_INT_STA_MID  = $0E; 
 STMPE1801_REG_GPIO_INT_STA_HIGH = $0F; 
 STMPE1801_REG_GPIO_SET_PIN_LOW  = $10; {GPIO set pin state}
 STMPE1801_REG_GPIO_SET_PIN_MID  = $11; 
 STMPE1801_REG_GPIO_SET_PIN_HIGH = $12; 
 STMPE1801_REG_GPIO_CLR_PIN_LOW  = $13; {GPIO clear pin state}
 STMPE1801_REG_GPIO_CLR_PIN_MID  = $14; 
 STMPE1801_REG_GPIO_CLR_PIN_HIGH = $15; 
 STMPE1801_REG_GPIO_MP_STA_LOW   = $16; {GPIO monitor pin state}
 STMPE1801_REG_GPIO_MP_STA_MID   = $17; 
 STMPE1801_REG_GPIO_MP_STA_HIGH  = $18; 
 STMPE1801_REG_GPIO_SET_DIR_LOW  = $19; {GPIO set pin direction register}
 STMPE1801_REG_GPIO_SET_DIR_MID  = $1A; 
 STMPE1801_REG_GPIO_SET_DIR_HIGH = $1B; 
 
 STMPE1801_REG_GPIO_RE_LOW       = $1C; {GPIO rising edge}
 STMPE1801_REG_GPIO_RE_MID       = $1D; 
 STMPE1801_REG_GPIO_RE_HIGH      = $1E; 
 STMPE1801_REG_GPIO_FE_LOW       = $1F; {GPIO falling edge}
 STMPE1801_REG_GPIO_FE_MID       = $20; 
 STMPE1801_REG_GPIO_FE_HIGH      = $21; 
 STMPE1801_REG_GPIO_PULL_UP_LOW  = $22; {GPIO pull up}
 STMPE1801_REG_GPIO_PULL_UP_MID  = $23; 
 STMPE1801_REG_GPIO_PULL_UP_HIGH = $24; 
 
 STMPE1801_REG_KPC_ROW           = $30; {Keypad row scanning}
 STMPE1801_REG_KPC_COL_LOW       = $31; {Keypad column scanning}
 STMPE1801_REG_KPC_COL_HIGH      = $32; 
 STMPE1801_REG_KPC_CTRL_LOW      = $33; {Key config: Scan count and dedicated key}
 STMPE1801_REG_KPC_CTRL_MID      = $34; 
 STMPE1801_REG_KPC_CTRL_HIGH     = $35; 
 STMPE1801_REG_KPC_CMD           = $36; {Keypad command}
 STMPE1801_REG_KPC_COMB_KEY_0    = $37; {Keypad combination key mask} 
 STMPE1801_REG_KPC_COMB_KEY_1    = $38; 
 STMPE1801_REG_KPC_COMB_KEY_2    = $39; 
 STMPE1801_REG_KPC_DATA_BYTE0    = $3A; {Keypad data} 
 STMPE1801_REG_KPC_DATA_BYTE1    = $3B; 
 STMPE1801_REG_KPC_DATA_BYTE2    = $3C; 
 STMPE1801_REG_KPC_DATA_BYTE3    = $3D; 
 STMPE1801_REG_KPC_DATA_BYTE4    = $3E; 
 
 STMPE1801_REG_MAX = $3E;
 STMPE1801_REG_SIZE = 3;
 
 {STMPE2401 / STMPE2403}
 STMPE240X_REG_CHIP_ID          = $80; {Device identification}
 STMPE240X_REG_ID_VER           = $81; {Revision number}
                                
 STMPE240X_REG_SYS_CTRL         = $02; {System control register}
 STMPE240X_REG_INT_CTRL_MSB     = $10; {Interrupt Control Register}
 STMPE240X_REG_INT_CTRL_LSB     = $11;
 STMPE240X_REG_INT_EN_MSB       = $12; {Interrupt Enable Mask Register}
 STMPE240X_REG_INT_EN_LSB       = $13;
 STMPE240X_REG_INT_STA_MSB      = $14; {Interrupt Status Register}
 STMPE240X_REG_INT_STA_LSB      = $15;
 
 STMPE240X_REG_GPIO_INT_EN_MSB  = $16; {Interrupt Enable GPIO Mask Register}
 STMPE240X_REG_GPIO_INT_EN_MID  = $17;
 STMPE240X_REG_GPIO_INT_EN_LSB  = $18;
 STMPE240X_REG_GPIO_INT_STA_MSB = $19; {Interrupt Status GPIO Register}
 STMPE240X_REG_GPIO_INT_STA_MID = $1A;
 STMPE240X_REG_GPIO_INT_STA_LSB = $1B;
 STMPE240X_REG_GPIO_MP_STA_MSB  = $A2; {GPIO Monitor Pin State Register}
 STMPE240X_REG_GPIO_MP_STA_MID  = $A3;
 STMPE240X_REG_GPIO_MP_STA_LSB  = $A4;
 STMPE240X_REG_GPIO_SET_PIN_MSB = $83; {GPIO Set Pin State Register}
 STMPE240X_REG_GPIO_SET_PIN_MID = $84;
 STMPE240X_REG_GPIO_SET_PIN_LSB = $85;
 STMPE240X_REG_GPIO_CLR_PIN_MSB = $86; {GPIO Clear Pin State Register}
 STMPE240X_REG_GPIO_CLR_PIN_MID = $87;
 STMPE240X_REG_GPIO_CLR_PIN_LSB = $88;
 STMPE240X_REG_GPIO_SET_DIR_MSB = $89; {GPIO Set Pin Direction Register}
 STMPE240X_REG_GPIO_SET_DIR_MID = $8A;
 STMPE240X_REG_GPIO_SET_DIR_LSB = $8B;
 STMPE240X_REG_GPIO_ED_MSB      = $8C; {GPIO Edge Detect Status Register}
 STMPE240X_REG_GPIO_ED_MID      = $8D;
 STMPE240X_REG_GPIO_ED_LSB      = $8E;
 STMPE240X_REG_GPIO_RE_MSB      = $8F; {GPIO Rising Edge Register}
 STMPE240X_REG_GPIO_RE_MID      = $90;
 STMPE240X_REG_GPIO_RE_LSB      = $91;
 STMPE240X_REG_GPIO_FE_MSB      = $92; {GPIO Falling Edge Register}
 STMPE240X_REG_GPIO_FE_MID      = $93;
 STMPE240X_REG_GPIO_FE_LSB      = $94;
 STMPE240X_REG_GPIO_PULL_UP_MSB = $95; {GPIO Pull Up Register}
 STMPE240X_REG_GPIO_PULL_UP_MID = $96;
 STMPE240X_REG_GPIO_PULL_UP_LSB = $97;
 STMPE240X_REG_GPIO_PULL_DN_MSB = $98; {GPIO Pull Down Register}
 STMPE240X_REG_GPIO_PULL_DN_MID = $99;
 STMPE240X_REG_GPIO_PULL_DN_LSB = $9A;
 STMPE240X_REG_GPIO_AF_U_MSB    = $9B; {GPIO Alternate Function Register (Upper Bit)}
 STMPE240X_REG_GPIO_AF_U_MID    = $9C;
 STMPE240X_REG_GPIO_AF_U_LSB    = $9D;
 STMPE240X_REG_GPIO_AF_L_MSB    = $9E; {GPIO Alternate Function Register(Lower Bit)}
 STMPE240X_REG_GPIO_AF_L_MID    = $9F;
 STMPE240X_REG_GPIO_AF_L_LSB    = $A0;
 
 STMPE240X_REG_PWMCS            = $30; {PWM Control and Status register}
 STMPE240X_REG_PWMIC0           = $38; {PWM instructions are initialized through this data port}
 STMPE240X_REG_PWMIC1           = $39;
 STMPE240X_REG_PWMIC2           = $3A;
 
 STMPE240X_REG_KPC_COL          = $60; {Keypad column scanning register}
 STMPE240X_REG_KPC_ROW_MSB      = $61; {Keypad row scanning register}
 STMPE240X_REG_KPC_ROW_LSB      = $62;
 STMPE240X_REG_KPC_CTRL_MSB     = $63; {Keypad control register}
 STMPE240X_REG_KPC_CTRL_LSB     = $64;
 STMPE240X_REG_KPC_DATA_BYTE0   = $68; {Keypad data register}
 STMPE240X_REG_KPC_DATA_BYTE1   = $69;
 STMPE240X_REG_KPC_DATA_BYTE2   = $6A;

 STMPE240X_REG_MAX = $BF; 
 STMPE240X_REG_SIZE = 3;
 
 STMPE_REG_UNKNOWN = $FF;
 
 {STMPE ID constants}
 {STMPE610 / STMPE811}
 STMPE811_CHIP_ID = $0811;
 
 {STMPE801}
 STMPE801_CHIP_ID = $0801;
 
 {STMPE1601}
 STMPE1601_CHIP_ID = $02;
 
 {STMPE1801}
 STMPE1801_CHIP_ID = $C1;
 
 {STMPE2401 / STMPE2403}
 STMPE240X_CHIP_ID = $01;
 
 {STMPE System Control constants}
 {STMPE610 / STMPE811}
 STMPE811_SYS_CTRL2_TS_OFF    = (1 shl 3); {1: Switches off the clock supply to the temperature sensor}
 STMPE811_SYS_CTRL2_GPIO_OFF  = (1 shl 2); {1: Switches off the clock supply to the GPIO}
 STMPE811_SYS_CTRL2_TSC_OFF   = (1 shl 1); {1: Switches off the clock supply to the touchscreen controller}
 STMPE811_SYS_CTRL2_ADC_OFF   = (1 shl 0); {1: Switches off the clock supply to the ADC} 
 
 {STMPE801}
 STMPE801_SYS_CTRL_RESET  = (1 shl 7); {Writing 1 to this bit causes a soft reset}
 STMPE801_SYS_CTRL_INT_EN = (1 shl 2); {INT Enable 1 to enable, 0 to disable INT output}
 STMPE801_SYS_CTRL_INT_HI = (1 shl 0); {INT Polarity 1 for active HI, 0 for active LOW}
 
 {STMPE1601}
 STMPE1601_SYS_CTRL_RESET       = (1 shl 7); {Writing a 1 to this bit will do a soft reset of the device. Once the reset is done, this bit will be cleared to 0 by the HW}
 STMPE1601_SYS_CTRL_ENABLE_GPIO = (1 shl 3); {Writing a 0 to this bit will gate off the clock to the GPIO module, thus stopping its operation}
 STMPE1601_SYS_CTRL_ENABLE_KPC  = (1 shl 1); {Writing a 0 to this bit will gate off the clock to the keypad controller module, thus stopping its operation}
 STMPE1601_SYS_CTRL_ENABLE_SPWM = (1 shl 0); {Writing a 0 to this bit will gate off the clock to the simple PWM controller module, thus stopping its operation}
 
 {STMPE1801}
 STMPE1801_SYS_CTRL_RESET = (1 shl 7); {Writing a 1 to this bit will do a soft reset of the device. Once the reset is done, this bit will be cleared to 0 by the HW}
 
 {STMPE240X}
 STMPE240X_SYS_CTRL_RESET       = (1 shl 7); {Writing a 1 to this bit will do a soft reset of the device. Once the reset is done, this bit will be cleared to 0 by the HW}
 STMPE240X_SYS_CTRL_ENABLE_GPIO = (1 shl 3); {Writing a 0 to this bit will gate off the clock to the GPIO module, thus stopping its operation}
 STMPE240X_SYS_CTRL_ENABLE_PWM  = (1 shl 2); {Writing a 0 to this bit will gate off the clock to the PWM module, thus stopping its operation}
 STMPE240X_SYS_CTRL_ENABLE_KPC  = (1 shl 1); {Writing a 0 to this bit will gate off the clock to the Keypad Controller module, thus stopping its operation}
 STMPE240X_SYS_CTRL_ENABLE_ROT  = (1 shl 0); {Writing a 0 to this bit will gate off the clock to the Rotator module, thus stopping its operation}
 
 {STMPE Touchscreen Control constants}
 {STMPE610 / STMPE811}
 STMPE811_TSC_CTRL_TSC_STA    = (1 shl 7); {TSC status (Reads '1' when touch is detected / Reads '0' when touch is not detected)}
 STMPE811_TSC_CTRL_TRACK_MASK = (7 shl 4); {TRACK: Tracking index}
 STMPE811_TSC_CTRL_OPMODE_XYZ = (0 shl 1); {OP_MOD: TSC operating mode: X, Y, Z acquisition}
 STMPE811_TSC_CTRL_OPMODE_XY  = (1 shl 1); {OP_MOD: TSC operating mode: X, Y only}
 STMPE811_TSC_CTRL_OPMODE_X   = (2 shl 1); {OP_MOD: TSC operating mode: X only}
 STMPE811_TSC_CTRL_OPMODE_Y   = (3 shl 1); {OP_MOD: TSC operating mode: Y only}
 STMPE811_TSC_CTRL_OPMODE_Z   = (4 shl 1); {OP_MOD: TSC operating mode: Z only}
 STMPE811_TSC_CTRL_TSC_EN     = (1 shl 0); {Enable TSC}
 
 {STMPE Touchscreen Config constants}
 {STMPE610 / STMPE811}
 STMPE811_TSC_CFG_AVE_CTRL_1           = (0 shl 6); {AVE_CTRL: Average control: 1 sample}
 STMPE811_TSC_CFG_AVE_CTRL_2           = (1 shl 6); {AVE_CTRL: Average control: 2 samples}
 STMPE811_TSC_CFG_AVE_CTRL_4           = (2 shl 6); {AVE_CTRL: Average control: 4 samples}
 STMPE811_TSC_CFG_AVE_CTRL_8           = (3 shl 6); {AVE_CTRL: Average control: 8 samples}
 STMPE811_TSC_CFG_TOUCH_DET_DELAY_MASK = (7 shl 3); {TOUCH_DET_DELAY: Touch detect delay}
 STMPE811_TSC_CFG_SETTLING_MASK        = (7 shl 0); {SETTLING: Panel driver settling time}
 
 {STMPE ADC Control 1 constants}
 {STMPE610 / STMPE811}
 STMPE811_ADC_CTRL1_SAMPLE_TIME_MASK = (7 shl 4); {SAMPLE_TIMEn: ADC conversion time in number of clock}
 STMPE811_ADC_CTRL1_MOD_12B          = (1 shl 3); {MOD_12B: Selects 10 or 12-bit ADC operation (1: 12 bit ADC / 0: 10 bit ADC)}
 STMPE811_ADC_CTRL1_REF_SEL          = (1 shl 1); {REF_SEL: Selects between internal or external reference for the ADC (1: External reference / 0: Internal reference)}
 
 {STMPE ADC Control 2 constants}
 {STMPE610 / STMPE811}
 STMPE811_ADC_CTRL2_ADC_FREQ_MASK = (3 shl 0); {ADC_FREQ: Selects the clock speed of ADC}
 
 {STMPE FIFO Control and Status constants}
 {STMPE610 / STMPE811}
 STMPE811_FIFO_STA_RESET = (1 shl 0); {FIFO Reset (Write 0 : FIFO put out of reset mode  / Write 1 : Resets FIFO. All data in FIFO will be cleared)}
 
 {STMPE Interrupt Control constants}
 {STMPE610 / STMPE811}
 STMPE811_INT_CTRL_POLARITY = (1 shl 2); {This bit sets the INT pin polarity (1: Active high/rising edge / 0: Active low/falling edge)}
 STMPE811_INT_CTRL_TYPE     = (1 shl 1); {This bit sets the type of interrupt signal required by the host (1: Edge interrupt / 0: Level interrupt)}
 STMPE811_INT_CTRL_GLOBAL   = (1 shl 0); {This is master enable for the interrupt system (1: Global interrupt / 0: Stops all interrupts)}
 
 {STMPE Interrupt Enable constants}
 {STMPE610 / STMPE811}
 STMPE811_INT_EN_GPIO       = (1 shl 7); {GPIO: Any enabled GPIO interrupts}
 STMPE811_INT_EN_ADC        = (1 shl 6); {ADC: Any enabled ADC interrupts}
 STMPE811_INT_EN_TEMP_SENS  = (1 shl 5); {TEMP_SENS: Temperature threshold triggering}
 STMPE811_INT_EN_FIFO_EMPTY = (1 shl 4); {FIFO_EMPTY: FIFO is empty}
 STMPE811_INT_EN_FIFO_FULL  = (1 shl 3); {FIFO_FULL: FIFO is full}
 STMPE811_INT_EN_FIFO_OFLOW = (1 shl 2); {FIFO_OFLOW: FIFO is overflowed}
 STMPE811_INT_EN_FIFO_TH    = (1 shl 1); {FIFO_TH: FIFO is equal or above threshold value}
 STMPE811_INT_EN_TOUCH_DET  = (1 shl 0); {TOUCH_DET: Touch is detected}
 
 {STMPE Interrupt Status constants}
 {STMPE610 / STMPE811}
 STMPE811_INT_STA_GPIO       = (1 shl 7); {GPIO: Any enabled GPIO interrupts}
 STMPE811_INT_STA_ADC        = (1 shl 6); {ADC: Any enabled ADC interrupts}
 STMPE811_INT_STA_TEMP_SENS  = (1 shl 5); {TEMP_SENS: Temperature threshold triggering}
 STMPE811_INT_STA_FIFO_EMPTY = (1 shl 4); {FIFO_EMPTY: FIFO is empty}
 STMPE811_INT_STA_FIFO_FULL  = (1 shl 3); {FIFO_FULL: FIFO is full}
 STMPE811_INT_STA_FIFO_OFLOW = (1 shl 2); {FIFO_OFLOW: FIFO is overflowed}
 STMPE811_INT_STA_FIFO_TH    = (1 shl 1); {FIFO_TH: FIFO is equal or above threshold value}
 STMPE811_INT_STA_TOUCH_DET  = (1 shl 0); {TOUCH_DET: Touch is detected}
 
 STMPE811_INT_STA_TOUCH_MASK = STMPE811_INT_STA_FIFO_EMPTY or STMPE811_INT_STA_FIFO_FULL or STMPE811_INT_STA_FIFO_OFLOW or STMPE811_INT_STA_FIFO_TH or STMPE811_INT_STA_TOUCH_DET;
 
{==============================================================================}
type
 {STMPE specific types}
 PSTMPEControl = ^TSTMPEControl;
 TSTMPEControl = record
  {General Properties}
  Chip:LongWord;               {The chip type (eg STMPE_CHIP_STMPE610)}
  IRQ:TGPIOInfo;               {The GPIO information for the IRQ line (Optional)}
  {I2C Properties}
  I2C:PI2CDevice;              {The I2C device this device is connected to (Optional)}
  Address:Word;                {The I2C address of the device}
  {SPI Properties}
  SPI:PSPIDevice;              {The SPI device this device is connected to (Optional)}
  ChipSelect:Word;             {The SPI chip select of the device}
  {Register Properties}
  RegMax:LongWord;             {The maximum register address for read or write}
  RegDir:LongWord;             {The register address direction (Ascending / Descending)}
  RegSize:LongWord;            {The standard size of a register read or write}
 end;
 
 PSTMPEOffsets = ^TSTMPEOffsets;
 TSTMPEOffsets = record
  {Control Register offsets}
  SysCtrl:Byte;
  IntCtrl:Byte;
  IntEnable:Byte;
  IntStatus:Byte;
  {GPIO Registers offsets}
  GPIOIntEnable:Byte;
  GPIOIntStatus:Byte;
  GPIOPinGet:Byte;
  GPIOPinSet:Byte;
  GPIOPinClr:Byte;
  GPIODirSet:Byte;
  GPIOFuncSet:Byte;
  GPIOPullUp:Byte;
  GPIOPullDown:Byte;
  GPIOEdgeDetect:Byte;
  GPIORisingEdge:Byte;
  GPIOFallingEdge:Byte;
  {Touchscreen Register offsets (610 and 811 Only)} 
  ADCCtrl1:Byte;
  ADCCtrl2:Byte;
  TSCCtrl:Byte;
  TSCCfg:Byte;
  FIFOThreshold:Byte;
  FIFOStatus:Byte;
  TSCDataXYZ:Byte;
  TSCFractionZ:Byte;
  TSCIDrive:Byte;
 end;
 
 PSTMPEGPIO = ^TSTMPEGPIO;
 TSTMPEGPIO = record
  {GPIO Properties}
  GPIO:TGPIODevice;
  {STMPE Properties}
  Control:TSTMPEControl;                          {The control information (Chip, I2C, SPI etc) for this device}
  Offsets:TSTMPEOffsets;                          {The register offsets for this device}
 end;
 
 PSTMPETouch = ^TSTMPETouch;
 TSTMPETouch = record
  {Touch Properties}
  Touch:TTouchDevice;
  {General Properties}
  MaxX:Word;                                      {Maximum X value for this device}
  MaxY:Word;                                      {Maximum Y value for this device}
  MaxZ:Word;                                      {Maximum Z value for this device}
  Width:Word;                                     {Screen width for this device}
  Height:Word;                                    {Screen height for this device}
  MaxPoints:LongWord;                             {Maximum touch points for this device}
  {STMPE Properties}
  Control:TSTMPEControl;                          {The control information (Chip, I2C, SPI etc) for this device}
  Offsets:TSTMPEOffsets;                          {The register offsets for this device}
  Timer:TTimerHandle;                             {Handle for touch release timer}
 end;
 
const
 {STMPE610 / STMPE811 Offsets}
 STMPE811Offsets:TSTMPEOffsets = (
  {Control Register offsets}
  SysCtrl:STMPE811_REG_SYS_CTRL2;
  IntCtrl:STMPE811_REG_INT_CTRL;
  IntEnable:STMPE811_REG_INT_EN;
  IntStatus:STMPE811_REG_INT_STA;
  {GPIO Registers offsets}
  GPIOIntEnable:STMPE811_REG_GPIO_INT_EN;
  GPIOIntStatus:STMPE811_REG_GPIO_INT_STA;
  GPIOPinGet:STMPE811_REG_GPIO_MP_STA;
  GPIOPinSet:STMPE811_REG_GPIO_SET_PIN;
  GPIOPinClr:STMPE811_REG_GPIO_CLR_PIN;
  GPIODirSet:STMPE811_REG_GPIO_SET_DIR;
  GPIOFuncSet:STMPE811_REG_GPIO_AF;
  GPIOPullUp:STMPE_REG_UNKNOWN;
  GPIOPullDown:STMPE_REG_UNKNOWN;
  GPIOEdgeDetect:STMPE811_REG_GPIO_ED;
  GPIORisingEdge:STMPE811_REG_GPIO_RE;
  GPIOFallingEdge:STMPE811_REG_GPIO_FE;
  {Touchscreen Register offsets (610 and 811 Only)}
  ADCCtrl1:STMPE811_REG_ADC_CTRL1;
  ADCCtrl2:STMPE811_REG_ADC_CTRL2;
  TSCCtrl:STMPE811_REG_TSC_CTRL;
  TSCCfg:STMPE811_REG_TSC_CFG;
  FIFOThreshold:STMPE811_REG_FIFO_TH;
  FIFOStatus:STMPE811_REG_FIFO_STA;
  TSCDataXYZ:STMPE811_REG_TSC_DATA_XYZ;
  TSCFractionZ:STMPE811_REG_TSC_FRACTION_Z;
  TSCIDrive:STMPE811_REG_TSC_I_DRIVE
 );
 
 {STMPE801 Offsets}
 STMPE801Offsets:TSTMPEOffsets = (
  {Control Register offsets}
  SysCtrl:STMPE801_REG_SYS_CTRL;
  IntCtrl:STMPE801_REG_SYS_CTRL;
  IntEnable:STMPE_REG_UNKNOWN;
  IntStatus:STMPE_REG_UNKNOWN;
  {GPIO Registers offsets}
  GPIOIntEnable:STMPE801_REG_GPIO_INT_EN;
  GPIOIntStatus:STMPE801_REG_GPIO_INT_STA;
  GPIOPinGet:STMPE801_REG_GPIO_MP_STA;
  GPIOPinSet:STMPE801_REG_GPIO_SET_PIN;
  GPIOPinClr:STMPE801_REG_GPIO_SET_PIN;
  GPIODirSet:STMPE801_REG_GPIO_SET_DIR;
  GPIOFuncSet:STMPE_REG_UNKNOWN;
  GPIOPullUp:STMPE_REG_UNKNOWN;
  GPIOPullDown:STMPE_REG_UNKNOWN;
  GPIOEdgeDetect:STMPE_REG_UNKNOWN;
  GPIORisingEdge:STMPE_REG_UNKNOWN;
  GPIOFallingEdge:STMPE_REG_UNKNOWN;
  {Touchscreen Register offsets (610 and 811 Only)}
  ADCCtrl1:STMPE_REG_UNKNOWN;
  ADCCtrl2:STMPE_REG_UNKNOWN;
  TSCCtrl:STMPE_REG_UNKNOWN;
  TSCCfg:STMPE_REG_UNKNOWN;
  FIFOThreshold:STMPE_REG_UNKNOWN;
  FIFOStatus:STMPE_REG_UNKNOWN;
  TSCDataXYZ:STMPE_REG_UNKNOWN;
  TSCFractionZ:STMPE_REG_UNKNOWN;
  TSCIDrive:STMPE_REG_UNKNOWN
 );
 
 {STMPE1601 Offsets}
 STMPE1601Offsets:TSTMPEOffsets = (
  {Control Register offsets}
  SysCtrl:STMPE1601_REG_SYS_CTRL;
  IntCtrl:STMPE1601_REG_INT_CTRL_LSB;
  IntEnable:STMPE1601_REG_INT_EN_LSB;
  IntStatus:STMPE1601_REG_INT_STA_LSB;
  {GPIO Registers offsets}
  GPIOIntEnable:STMPE1601_REG_GPIO_INT_EN_LSB;
  GPIOIntStatus:STMPE1601_REG_GPIO_INT_STA_LSB;
  GPIOPinGet:STMPE1601_REG_GPIO_MP_STA_LSB;
  GPIOPinSet:STMPE1601_REG_GPIO_SET_PIN_LSB;
  GPIOPinClr:STMPE1601_REG_GPIO_CLR_PIN_LSB;
  GPIODirSet:STMPE1601_REG_GPIO_SET_DIR_LSB;
  GPIOFuncSet:STMPE1601_REG_GPIO_AF_L_LSB;
  GPIOPullUp:STMPE1601_REG_GPIO_PU_LSB;
  GPIOPullDown:STMPE_REG_UNKNOWN;
  GPIOEdgeDetect:STMPE1601_REG_GPIO_ED_LSB;
  GPIORisingEdge:STMPE1601_REG_GPIO_RE_LSB;
  GPIOFallingEdge:STMPE1601_REG_GPIO_FE_LSB;
  {Touchscreen Register offsets (610 and 811 Only)}
  ADCCtrl1:STMPE_REG_UNKNOWN;
  ADCCtrl2:STMPE_REG_UNKNOWN;
  TSCCtrl:STMPE_REG_UNKNOWN;
  TSCCfg:STMPE_REG_UNKNOWN;
  FIFOThreshold:STMPE_REG_UNKNOWN;
  FIFOStatus:STMPE_REG_UNKNOWN;
  TSCDataXYZ:STMPE_REG_UNKNOWN;
  TSCFractionZ:STMPE_REG_UNKNOWN;
  TSCIDrive:STMPE_REG_UNKNOWN
 );

 {STMPE1801 Offsets}
 STMPE1801Offsets:TSTMPEOffsets = (
  {Control Register offsets}
  SysCtrl:STMPE1801_REG_SYS_CTRL;
  IntCtrl:STMPE1801_REG_INT_CTRL_LOW;
  IntEnable:STMPE1801_REG_INT_EN_LOW;
  IntStatus:STMPE1801_REG_INT_STA_LOW;
  {GPIO Registers offsets}
  GPIOIntEnable:STMPE1801_REG_GPIO_INT_EN_LOW;
  GPIOIntStatus:STMPE1801_REG_GPIO_INT_STA_LOW;
  GPIOPinGet:STMPE1801_REG_GPIO_MP_STA_LOW;
  GPIOPinSet:STMPE1801_REG_GPIO_SET_PIN_LOW;
  GPIOPinClr:STMPE1801_REG_GPIO_CLR_PIN_LOW;
  GPIODirSet:STMPE1801_REG_GPIO_SET_DIR_LOW;
  GPIOFuncSet:STMPE_REG_UNKNOWN;
  GPIOPullUp:STMPE1801_REG_GPIO_PULL_UP_LOW;
  GPIOPullDown:STMPE_REG_UNKNOWN;
  GPIOEdgeDetect:STMPE_REG_UNKNOWN;
  GPIORisingEdge:STMPE1801_REG_GPIO_RE_LOW;
  GPIOFallingEdge:STMPE1801_REG_GPIO_FE_LOW;
  {Touchscreen Register offsets (610 and 811 Only)}
  ADCCtrl1:STMPE_REG_UNKNOWN;
  ADCCtrl2:STMPE_REG_UNKNOWN;
  TSCCtrl:STMPE_REG_UNKNOWN;
  TSCCfg:STMPE_REG_UNKNOWN;
  FIFOThreshold:STMPE_REG_UNKNOWN;
  FIFOStatus:STMPE_REG_UNKNOWN;
  TSCDataXYZ:STMPE_REG_UNKNOWN;
  TSCFractionZ:STMPE_REG_UNKNOWN;
  TSCIDrive:STMPE_REG_UNKNOWN
 );
 
 {STMPE2401 / STMPE2403 Offsets}
 STMPE240XOffsets:TSTMPEOffsets = (
  {Control Register offsets}
  SysCtrl:STMPE240X_REG_SYS_CTRL;
  IntCtrl:STMPE240X_REG_INT_CTRL_LSB;
  IntEnable:STMPE240X_REG_INT_EN_LSB;
  IntStatus:STMPE240X_REG_INT_STA_LSB;
  {GPIO Registers offsets}
  GPIOIntEnable:STMPE240X_REG_GPIO_INT_EN_LSB;
  GPIOIntStatus:STMPE240X_REG_GPIO_INT_STA_LSB;
  GPIOPinGet:STMPE240X_REG_GPIO_MP_STA_LSB;
  GPIOPinSet:STMPE240X_REG_GPIO_SET_PIN_LSB;
  GPIOPinClr:STMPE240X_REG_GPIO_CLR_PIN_LSB;
  GPIODirSet:STMPE240X_REG_GPIO_SET_DIR_LSB;
  GPIOFuncSet:STMPE240X_REG_GPIO_AF_L_LSB;
  GPIOPullUp:STMPE240X_REG_GPIO_PULL_UP_LSB;
  GPIOPullDown:STMPE240X_REG_GPIO_PULL_DN_LSB;
  GPIOEdgeDetect:STMPE240X_REG_GPIO_ED_LSB;
  GPIORisingEdge:STMPE240X_REG_GPIO_RE_LSB;
  GPIOFallingEdge:STMPE240X_REG_GPIO_FE_LSB;
  {Touchscreen Register offsets (610 and 811 Only)}
  ADCCtrl1:STMPE_REG_UNKNOWN;
  ADCCtrl2:STMPE_REG_UNKNOWN;
  TSCCtrl:STMPE_REG_UNKNOWN;
  TSCCfg:STMPE_REG_UNKNOWN;
  FIFOThreshold:STMPE_REG_UNKNOWN;
  FIFOStatus:STMPE_REG_UNKNOWN;
  TSCDataXYZ:STMPE_REG_UNKNOWN;
  TSCFractionZ:STMPE_REG_UNKNOWN;
  TSCIDrive:STMPE_REG_UNKNOWN
 );
 
{==============================================================================}
var
 {STMPE specific variables}
 STMPE_SAMPLE_TIME:Byte = 4;
 STMPE_MOD_12B:Byte = 1;
 STMPE_REF_SEL:Byte = 0;
 STMPE_ADC_FREQ:Byte = 2;
 STMPE_AVE_CTRL:Byte = 3;
 STMPE_TOUCH_DET_DELAY:Byte = 4;
 STMPE_SETTLING:Byte = 2;
 STMPE_FRACTION_Z:Byte = 7;
 STMPE_I_DRIVE:Byte = 0;
 
{==============================================================================}
{Initialization Functions}
procedure STMPEInit;{$IFDEF API_EXPORT_STMPE} stdcall; public name 'stmpe_init';{$ENDIF}
 
{==============================================================================}
{STMPE Functions}
function STMPE610GPIOCreate(I2C:PI2CDevice;SPI:PSPIDevice;Address,ChipSelect:Word;IRQ:PGPIOInfo):PGPIODevice;{$IFDEF API_EXPORT_STMPE} stdcall; public name 'stmpe610_gpio_create';{$ENDIF}
function STMPE801GPIOCreate(I2C:PI2CDevice;Address:Word;IRQ:PGPIOInfo):PGPIODevice;{$IFDEF API_EXPORT_STMPE} stdcall; public name 'stmpe801_gpio_create';{$ENDIF}
function STMPE811GPIOCreate(I2C:PI2CDevice;SPI:PSPIDevice;Address,ChipSelect:Word;IRQ:PGPIOInfo):PGPIODevice;{$IFDEF API_EXPORT_STMPE} stdcall; public name 'stmpe811_gpio_create';{$ENDIF}
function STMPE1601GPIOCreate(I2C:PI2CDevice;Address:Word;IRQ:PGPIOInfo):PGPIODevice;{$IFDEF API_EXPORT_STMPE} stdcall; public name 'stmpe1601_gpio_create';{$ENDIF}
function STMPE1801GPIOCreate(I2C:PI2CDevice;Address:Word;IRQ:PGPIOInfo):PGPIODevice;{$IFDEF API_EXPORT_STMPE} stdcall; public name 'stmpe1801_gpio_create';{$ENDIF}
function STMPE2401GPIOCreate(I2C:PI2CDevice;Address:Word;IRQ:PGPIOInfo):PGPIODevice;{$IFDEF API_EXPORT_STMPE} stdcall; public name 'stmpe2401_gpio_create';{$ENDIF}

function STMPEGPIODestroy(GPIO:PGPIODevice):LongWord;{$IFDEF API_EXPORT_STMPE} stdcall; public name 'stmpe_gpio_destroy';{$ENDIF}

function STMPE610TouchCreate(I2C:PI2CDevice;SPI:PSPIDevice;Address,ChipSelect:Word;Width,Height:LongWord;IRQ:PGPIOInfo):PTouchDevice;{$IFDEF API_EXPORT_STMPE} stdcall; public name 'stmpe610_touch_create';{$ENDIF}
function STMPE811TouchCreate(I2C:PI2CDevice;SPI:PSPIDevice;Address,ChipSelect:Word;Width,Height:LongWord;IRQ:PGPIOInfo):PTouchDevice;{$IFDEF API_EXPORT_STMPE} stdcall; public name 'stmpe811_touch_create';{$ENDIF}

function STMPETouchDestroy(Touch:PTouchDevice):LongWord;{$IFDEF API_EXPORT_STMPE} stdcall; public name 'stmpe_touch_destroy';{$ENDIF}

{==============================================================================}
{STMPE GPIO Functions}
function STMPEGPIOStart(GPIO:PGPIODevice):LongWord;
function STMPEGPIOStop(GPIO:PGPIODevice):LongWord;
 
function STMPEGPIORead(GPIO:PGPIODevice;Reg:LongWord):LongWord;
procedure STMPEGPIOWrite(GPIO:PGPIODevice;Reg,Value:LongWord);
 
function STMPEGPIOInputGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
 
function STMPEGPIOOutputSet(GPIO:PGPIODevice;Pin,Level:LongWord):LongWord;

function STMPEGPIOPullGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
function STMPEGPIOPullSelect(GPIO:PGPIODevice;Pin,Mode:LongWord):LongWord;

function STMPEGPIOFunctionGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
function STMPEGPIOFunctionSelect(GPIO:PGPIODevice;Pin,Mode:LongWord):LongWord;

{==============================================================================}
{STMPE Touch Functions}
function STMPETouchStart(Touch:PTouchDevice):LongWord;
function STMPETouchStop(Touch:PTouchDevice):LongWord;

function STMPETouchUpdate(Touch:PTouchDevice):LongWord;

procedure STMPETouchTimer(Touch:PSTMPETouch);
procedure STMPETouchCallback(Touch:PSTMPETouch;Pin,Trigger:LongWord);

{==============================================================================}
{STMPE Helper Functions}
function STMPEReadByte(Control:PSTMPEControl;Reg:Byte;Value:PByte):LongWord;
function STMPEWriteByte(Control:PSTMPEControl;Reg,Value:Byte):LongWord;

function STMPEReadWord(Control:PSTMPEControl;Reg:Byte;Value:PWord):LongWord;
function STMPEWriteWord(Control:PSTMPEControl;Reg:Byte;Value:Word):LongWord;

function STMPEReadBytes(Control:PSTMPEControl;Reg,Len:Byte;Values:PByte):LongWord;
function STMPEWriteBytes(Control:PSTMPEControl;Reg,Len:Byte;Values:PByte):LongWord;

function STMPESetBits(Control:PSTMPEControl;Reg,Mask,Value:Byte):LongWord;
 
function STMPEResetFIFO(Control:PSTMPEControl;Reg:Byte):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {STMPE specific variables}
 STMPEInitialized:Boolean;

{==============================================================================}
{==============================================================================}
{Forward Declarations}
function STMPETouchUpdateConfig(Touch:PSTMPETouch):LongWord; forward;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure STMPEInit;{$IFDEF API_EXPORT_STMPE} stdcall;{$ENDIF}
{Initialize the STMPE unit and parameters}

{Note: Called internally by other functions}
var
 WorkInt:LongWord;
begin
 {}
 {Check Initialized}
 if STMPEInitialized then Exit;
 
 {Check Environment Variables}
 {STMPE_SAMPLE_TIME}
 WorkInt:=StrToIntDef(EnvironmentGet('STMPE_SAMPLE_TIME'),STMPE_SAMPLE_TIME);
 if WorkInt <> STMPE_SAMPLE_TIME then STMPE_SAMPLE_TIME:=WorkInt;

 {STMPE_MOD_12B}
 WorkInt:=StrToIntDef(EnvironmentGet('STMPE_MOD_12B'),STMPE_MOD_12B);
 if WorkInt <> STMPE_MOD_12B then STMPE_MOD_12B:=WorkInt;
 
 {STMPE_REF_SEL}
 WorkInt:=StrToIntDef(EnvironmentGet('STMPE_REF_SEL'),STMPE_REF_SEL);
 if WorkInt <> STMPE_REF_SEL then STMPE_REF_SEL:=WorkInt;

 {STMPE_ADC_FREQ}
 WorkInt:=StrToIntDef(EnvironmentGet('STMPE_ADC_FREQ'),STMPE_ADC_FREQ);
 if WorkInt <> STMPE_ADC_FREQ then STMPE_ADC_FREQ:=WorkInt;

 {STMPE_AVE_CTRL}
 WorkInt:=StrToIntDef(EnvironmentGet('STMPE_AVE_CTRL'),STMPE_AVE_CTRL);
 if WorkInt <> STMPE_AVE_CTRL then STMPE_AVE_CTRL:=WorkInt;

 {STMPE_TOUCH_DET_DELAY}
 WorkInt:=StrToIntDef(EnvironmentGet('STMPE_TOUCH_DET_DELAY'),STMPE_TOUCH_DET_DELAY);
 if WorkInt <> STMPE_TOUCH_DET_DELAY then STMPE_TOUCH_DET_DELAY:=WorkInt;
 
 {STMPE_SETTLING}
 WorkInt:=StrToIntDef(EnvironmentGet('STMPE_SETTLING'),STMPE_SETTLING);
 if WorkInt <> STMPE_SETTLING then STMPE_SETTLING:=WorkInt;
 
 {STMPE_FRACTION_Z}
 WorkInt:=StrToIntDef(EnvironmentGet('STMPE_FRACTION_Z'),STMPE_FRACTION_Z);
 if WorkInt <> STMPE_FRACTION_Z then STMPE_FRACTION_Z:=WorkInt;
 
 {STMPE_I_DRIVE}
 WorkInt:=StrToIntDef(EnvironmentGet('STMPE_I_DRIVE'),STMPE_I_DRIVE);
 if WorkInt <> STMPE_I_DRIVE then STMPE_I_DRIVE:=WorkInt;
 
 STMPEInitialized:=True;
end;
 
{==============================================================================}
{==============================================================================}
{STMPE Functions}
function STMPE610GPIOCreate(I2C:PI2CDevice;SPI:PSPIDevice;Address,ChipSelect:Word;IRQ:PGPIOInfo):PGPIODevice;{$IFDEF API_EXPORT_STMPE} stdcall;{$ENDIF}
{Create, register and start a new STMPE610 GPIO device connected to the specified I2C or SPI device}
{I2C: The I2C device this STMPE610 is connected to (Optional)}
{SPI: The SPI device this STMPE610 is connected to (Optional)}
{Address: The I2C address for this STMPE610 (or I2C_ADDRESS_INVALID if SPI connected)}
{ChipSelect: The SPI chip select for this STMPE610 (or SPI_CS_NONE if I2C connected)}
{IRQ: The GPIO information for the IRQ line (Optional)}
{Return: Pointer to the new GPIO device or nil on failure}

{Note: Either I2C or SPI must be specified but not both}
var
 Status:LongWord;
 
 STMPEGPIO:PSTMPEGPIO;
begin
 {}
 Result:=nil;
 
 {Initialize}
 STMPEInit;
 
 {$IF DEFINED(STMPE_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(nil,'STMPE610: GPIO Create (Address=' + IntToHex(Address,4) + ' ChipSelect=' + IntToStr(ChipSelect) + ')');
 {$ENDIF}
 
 {Check I2C and SPI}
 if (I2C = nil) and (SPI = nil) then Exit;
 if (I2C <> nil) and (SPI <> nil) then Exit;
 
 {Check Address}
 if (I2C <> nil) and (Address = I2C_ADDRESS_INVALID) then Exit;
 
 {Check ChipSelect}
 if (SPI <> nil) and (ChipSelect = SPI_CS_NONE) then Exit;
 
 {Create GPIO}
 STMPEGPIO:=PSTMPEGPIO(GPIODeviceCreateEx(SizeOf(TSTMPEGPIO)));
 if STMPEGPIO <> nil then
  begin
   {Update GPIO}
   {Device}
   STMPEGPIO.GPIO.Device.DeviceBus:=DEVICE_BUS_I2C; 
   if SPI <> nil then STMPEGPIO.GPIO.Device.DeviceBus:=DEVICE_BUS_SPI;
   STMPEGPIO.GPIO.Device.DeviceType:=GPIO_TYPE_NONE;
   STMPEGPIO.GPIO.Device.DeviceFlags:=GPIO_FLAG_NONE;
   STMPEGPIO.GPIO.Device.DeviceData:=nil;
   STMPEGPIO.GPIO.Device.DeviceDescription:=STMPE610_GPIO_DESCRIPTION;
   {GPIO}
   STMPEGPIO.GPIO.GPIOState:=GPIO_STATE_DISABLED;
   STMPEGPIO.GPIO.DeviceStart:=STMPEGPIOStart;
   STMPEGPIO.GPIO.DeviceStop:=STMPEGPIOStop;
   STMPEGPIO.GPIO.DeviceRead:=STMPEGPIORead;
   STMPEGPIO.GPIO.DeviceWrite:=STMPEGPIOWrite;
   STMPEGPIO.GPIO.DeviceInputGet:=STMPEGPIOInputGet;
   STMPEGPIO.GPIO.DeviceOutputSet:=STMPEGPIOOutputSet;
   STMPEGPIO.GPIO.DevicePullGet:=STMPEGPIOPullGet;
   STMPEGPIO.GPIO.DevicePullSelect:=STMPEGPIOPullSelect;
   STMPEGPIO.GPIO.DeviceFunctionGet:=STMPEGPIOFunctionGet;
   STMPEGPIO.GPIO.DeviceFunctionSelect:=STMPEGPIOFunctionSelect;    
   {Driver}
   STMPEGPIO.GPIO.Address:=nil;
   STMPEGPIO.GPIO.Properties.Flags:=STMPEGPIO.GPIO.Device.DeviceFlags;
   STMPEGPIO.GPIO.Properties.PinMin:=STMPE610_GPIO_MIN_PIN;
   STMPEGPIO.GPIO.Properties.PinMax:=STMPE610_GPIO_MAX_PIN;
   STMPEGPIO.GPIO.Properties.PinCount:=STMPE610_GPIO_PIN_COUNT;
   STMPEGPIO.GPIO.Properties.FunctionMin:=STMPE610_GPIO_MIN_FUNCTION;
   STMPEGPIO.GPIO.Properties.FunctionMax:=STMPE610_GPIO_MAX_FUNCTION;
   STMPEGPIO.GPIO.Properties.FunctionCount:=STMPE610_GPIO_FUNCTION_COUNT;
   {STMPE}
   STMPEGPIO.Control.Chip:=STMPE_CHIP_STMPE610;
   if IRQ <> nil then STMPEGPIO.Control.IRQ:=IRQ^ else STMPEGPIO.Control.IRQ:=GPIO_INFO_UNKNOWN;
   STMPEGPIO.Control.I2C:=I2C;
   STMPEGPIO.Control.Address:=Address;
   STMPEGPIO.Control.SPI:=SPI;
   STMPEGPIO.Control.ChipSelect:=ChipSelect;
   STMPEGPIO.Control.RegMax:=STMPE811_REG_MAX;
   STMPEGPIO.Control.RegDir:=STMPE_DIR_ASCENDING;
   STMPEGPIO.Control.RegSize:=STMPE811_REG_SIZE;
   
   {Register GPIO}
   Status:=GPIODeviceRegister(@STMPEGPIO.GPIO);
   if Status = ERROR_SUCCESS then
    begin
     {Start GPIO}
     Status:=GPIODeviceStart(@STMPEGPIO.GPIO);
     if Status = ERROR_SUCCESS then
      begin
       {Return Result}
       Result:=PGPIODevice(STMPEGPIO);
      end
     else 
      begin
       if GPIO_LOG_ENABLED then GPIOLogError(nil,'STMPE610: Failed to start new GPIO device: ' + ErrorToString(Status));
       
       {Deregister GPIO}
       GPIODeviceDeregister(@STMPEGPIO.GPIO);
       
       {Destroy GPIO}
       GPIODeviceDestroy(@STMPEGPIO.GPIO);
      end;
    end
   else
    begin
     if GPIO_LOG_ENABLED then GPIOLogError(nil,'STMPE610: Failed to register new GPIO device: ' + ErrorToString(Status));
     
     {Destroy GPIO}
     GPIODeviceDestroy(@STMPEGPIO.GPIO);
    end;
  end
 else 
  begin
   if GPIO_LOG_ENABLED then GPIOLogError(nil,'STMPE610: Failed to create new GPIO device');
  end;
end;

{==============================================================================}

function STMPE801GPIOCreate(I2C:PI2CDevice;Address:Word;IRQ:PGPIOInfo):PGPIODevice;{$IFDEF API_EXPORT_STMPE} stdcall;{$ENDIF}
{Create, register and start a new STMPE801 GPIO device connected to the specified I2C device}
{I2C: The I2C device this STMPE801 is connected to}
{Address: The I2C address for this STMPE801}
{IRQ: The GPIO information for the IRQ line (Optional)}
{Return: Pointer to the new GPIO device or nil on failure}
var
 Status:LongWord;
 
 STMPEGPIO:PSTMPEGPIO;
begin
 {}
 Result:=nil;
 
 {Initialize}
 STMPEInit;
 
 {$IF DEFINED(STMPE_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(nil,'STMPE801: GPIO Create (Address=' + IntToHex(Address,4) + ')');
 {$ENDIF}
 
 {Check I2C}
 if I2C = nil then Exit;
 
 {Check Address}
 if Address = I2C_ADDRESS_INVALID then Exit;
 
 {Create GPIO}
 STMPEGPIO:=PSTMPEGPIO(GPIODeviceCreateEx(SizeOf(TSTMPEGPIO)));
 if STMPEGPIO <> nil then
  begin
   {Update GPIO}
   {Device}
   STMPEGPIO.GPIO.Device.DeviceBus:=DEVICE_BUS_I2C; 
   STMPEGPIO.GPIO.Device.DeviceType:=GPIO_TYPE_NONE;
   STMPEGPIO.GPIO.Device.DeviceFlags:=GPIO_FLAG_NONE;
   STMPEGPIO.GPIO.Device.DeviceData:=nil;
   STMPEGPIO.GPIO.Device.DeviceDescription:=STMPE801_GPIO_DESCRIPTION;
   {GPIO}
   STMPEGPIO.GPIO.GPIOState:=GPIO_STATE_DISABLED;
   STMPEGPIO.GPIO.DeviceStart:=STMPEGPIOStart;
   STMPEGPIO.GPIO.DeviceStop:=STMPEGPIOStop;
   STMPEGPIO.GPIO.DeviceRead:=STMPEGPIORead;
   STMPEGPIO.GPIO.DeviceWrite:=STMPEGPIOWrite;
   STMPEGPIO.GPIO.DeviceInputGet:=STMPEGPIOInputGet;
   STMPEGPIO.GPIO.DeviceOutputSet:=STMPEGPIOOutputSet;
   STMPEGPIO.GPIO.DevicePullGet:=STMPEGPIOPullGet;
   STMPEGPIO.GPIO.DevicePullSelect:=STMPEGPIOPullSelect;
   STMPEGPIO.GPIO.DeviceFunctionGet:=STMPEGPIOFunctionGet;
   STMPEGPIO.GPIO.DeviceFunctionSelect:=STMPEGPIOFunctionSelect;    
   {Driver}
   STMPEGPIO.GPIO.Address:=nil;
   STMPEGPIO.GPIO.Properties.Flags:=STMPEGPIO.GPIO.Device.DeviceFlags;
   STMPEGPIO.GPIO.Properties.PinMin:=STMPE801_GPIO_MIN_PIN;
   STMPEGPIO.GPIO.Properties.PinMax:=STMPE801_GPIO_MAX_PIN;
   STMPEGPIO.GPIO.Properties.PinCount:=STMPE801_GPIO_PIN_COUNT;
   STMPEGPIO.GPIO.Properties.FunctionMin:=STMPE801_GPIO_MIN_FUNCTION;
   STMPEGPIO.GPIO.Properties.FunctionMax:=STMPE801_GPIO_MAX_FUNCTION;
   STMPEGPIO.GPIO.Properties.FunctionCount:=STMPE801_GPIO_FUNCTION_COUNT;
   {STMPE}
   STMPEGPIO.Control.Chip:=STMPE_CHIP_STMPE801;
   if IRQ <> nil then STMPEGPIO.Control.IRQ:=IRQ^ else STMPEGPIO.Control.IRQ:=GPIO_INFO_UNKNOWN;
   STMPEGPIO.Control.I2C:=I2C;
   STMPEGPIO.Control.Address:=Address;
   STMPEGPIO.Control.SPI:=nil;
   STMPEGPIO.Control.ChipSelect:=SPI_CS_NONE;
   STMPEGPIO.Control.RegMax:=STMPE801_REG_MAX;
   STMPEGPIO.Control.RegDir:=STMPE_DIR_ASCENDING;
   STMPEGPIO.Control.RegSize:=STMPE801_REG_SIZE;
   
   {Register GPIO}
   Status:=GPIODeviceRegister(@STMPEGPIO.GPIO);
   if Status = ERROR_SUCCESS then
    begin
     {Start GPIO}
     Status:=GPIODeviceStart(@STMPEGPIO.GPIO);
     if Status = ERROR_SUCCESS then
      begin
       {Return Result}
       Result:=PGPIODevice(STMPEGPIO);
      end
     else 
      begin
       if GPIO_LOG_ENABLED then GPIOLogError(nil,'STMPE801: Failed to start new GPIO device: ' + ErrorToString(Status));
       
       {Deregister GPIO}
       GPIODeviceDeregister(@STMPEGPIO.GPIO);
       
       {Destroy GPIO}
       GPIODeviceDestroy(@STMPEGPIO.GPIO);
      end;
    end
   else
    begin
     if GPIO_LOG_ENABLED then GPIOLogError(nil,'STMPE801: Failed to register new GPIO device: ' + ErrorToString(Status));
     
     {Destroy GPIO}
     GPIODeviceDestroy(@STMPEGPIO.GPIO);
    end;
  end
 else 
  begin
   if GPIO_LOG_ENABLED then GPIOLogError(nil,'STMPE801: Failed to create new GPIO device');
  end;
end;

{==============================================================================}

function STMPE811GPIOCreate(I2C:PI2CDevice;SPI:PSPIDevice;Address,ChipSelect:Word;IRQ:PGPIOInfo):PGPIODevice;{$IFDEF API_EXPORT_STMPE} stdcall;{$ENDIF}
{Create, register and start a new STMPE811 GPIO device connected to the specified I2C or SPI device}
{I2C: The I2C device this STMPE811 is connected to (Optional)}
{SPI: The SPI device this STMPE811 is connected to (Optional)}
{Address: The I2C address for this STMPE811 (or I2C_ADDRESS_INVALID if SPI connected)}
{ChipSelect: The SPI chip select for this STMPE811 (or SPI_CS_NONE if I2C connected)}
{IRQ: The GPIO information for the IRQ line (Optional)}
{Return: Pointer to the new GPIO device or nil on failure}

{Note: Either I2C or SPI must be specified but not both}
var
 Status:LongWord;
 
 STMPEGPIO:PSTMPEGPIO;
begin
 {}
 Result:=nil;
 
 {Initialize}
 STMPEInit;
 
 {$IF DEFINED(STMPE_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(nil,'STMPE811: GPIO Create (Address=' + IntToHex(Address,4) + ' ChipSelect=' + IntToStr(ChipSelect) + ')');
 {$ENDIF}
 
 {Check I2C and SPI}
 if (I2C = nil) and (SPI = nil) then Exit;
 if (I2C <> nil) and (SPI <> nil) then Exit;
 
 {Check Address}
 if (I2C <> nil) and (Address = I2C_ADDRESS_INVALID) then Exit;
 
 {Check ChipSelect}
 if (SPI <> nil) and (ChipSelect = SPI_CS_NONE) then Exit;
 
 {Create GPIO}
 STMPEGPIO:=PSTMPEGPIO(GPIODeviceCreateEx(SizeOf(TSTMPEGPIO)));
 if STMPEGPIO <> nil then
  begin
   {Update GPIO}
   {Device}
   STMPEGPIO.GPIO.Device.DeviceBus:=DEVICE_BUS_I2C; 
   if SPI <> nil then STMPEGPIO.GPIO.Device.DeviceBus:=DEVICE_BUS_SPI;
   STMPEGPIO.GPIO.Device.DeviceType:=GPIO_TYPE_NONE;
   STMPEGPIO.GPIO.Device.DeviceFlags:=GPIO_FLAG_NONE;
   STMPEGPIO.GPIO.Device.DeviceData:=nil;
   STMPEGPIO.GPIO.Device.DeviceDescription:=STMPE610_GPIO_DESCRIPTION;
   {GPIO}
   STMPEGPIO.GPIO.GPIOState:=GPIO_STATE_DISABLED;
   STMPEGPIO.GPIO.DeviceStart:=STMPEGPIOStart;
   STMPEGPIO.GPIO.DeviceStop:=STMPEGPIOStop;
   STMPEGPIO.GPIO.DeviceRead:=STMPEGPIORead;
   STMPEGPIO.GPIO.DeviceWrite:=STMPEGPIOWrite;
   STMPEGPIO.GPIO.DeviceInputGet:=STMPEGPIOInputGet;
   STMPEGPIO.GPIO.DeviceOutputSet:=STMPEGPIOOutputSet;
   STMPEGPIO.GPIO.DevicePullGet:=STMPEGPIOPullGet;
   STMPEGPIO.GPIO.DevicePullSelect:=STMPEGPIOPullSelect;
   STMPEGPIO.GPIO.DeviceFunctionGet:=STMPEGPIOFunctionGet;
   STMPEGPIO.GPIO.DeviceFunctionSelect:=STMPEGPIOFunctionSelect;    
   {Driver}
   STMPEGPIO.GPIO.Address:=nil;
   STMPEGPIO.GPIO.Properties.Flags:=STMPEGPIO.GPIO.Device.DeviceFlags;
   STMPEGPIO.GPIO.Properties.PinMin:=STMPE610_GPIO_MIN_PIN;
   STMPEGPIO.GPIO.Properties.PinMax:=STMPE610_GPIO_MAX_PIN;
   STMPEGPIO.GPIO.Properties.PinCount:=STMPE610_GPIO_PIN_COUNT;
   STMPEGPIO.GPIO.Properties.FunctionMin:=STMPE610_GPIO_MIN_FUNCTION;
   STMPEGPIO.GPIO.Properties.FunctionMax:=STMPE610_GPIO_MAX_FUNCTION;
   STMPEGPIO.GPIO.Properties.FunctionCount:=STMPE610_GPIO_FUNCTION_COUNT;
   {STMPE}
   STMPEGPIO.Control.Chip:=STMPE_CHIP_STMPE811;
   if IRQ <> nil then STMPEGPIO.Control.IRQ:=IRQ^ else STMPEGPIO.Control.IRQ:=GPIO_INFO_UNKNOWN;
   STMPEGPIO.Control.I2C:=I2C;
   STMPEGPIO.Control.Address:=Address;
   STMPEGPIO.Control.SPI:=SPI;
   STMPEGPIO.Control.ChipSelect:=ChipSelect;
   STMPEGPIO.Control.RegMax:=STMPE811_REG_MAX;
   STMPEGPIO.Control.RegDir:=STMPE_DIR_ASCENDING;
   STMPEGPIO.Control.RegSize:=STMPE811_REG_SIZE;
   
   {Register GPIO}
   Status:=GPIODeviceRegister(@STMPEGPIO.GPIO);
   if Status = ERROR_SUCCESS then
    begin
     {Start GPIO}
     Status:=GPIODeviceStart(@STMPEGPIO.GPIO);
     if Status = ERROR_SUCCESS then
      begin
       {Return Result}
       Result:=PGPIODevice(STMPEGPIO);
      end
     else 
      begin
       if GPIO_LOG_ENABLED then GPIOLogError(nil,'STMPE811: Failed to start new GPIO device: ' + ErrorToString(Status));
       
       {Deregister GPIO}
       GPIODeviceDeregister(@STMPEGPIO.GPIO);
       
       {Destroy GPIO}
       GPIODeviceDestroy(@STMPEGPIO.GPIO);
      end;
    end
   else
    begin
     if GPIO_LOG_ENABLED then GPIOLogError(nil,'STMPE811: Failed to register new GPIO device: ' + ErrorToString(Status));
     
     {Destroy GPIO}
     GPIODeviceDestroy(@STMPEGPIO.GPIO);
    end;
  end
 else 
  begin
   if GPIO_LOG_ENABLED then GPIOLogError(nil,'STMPE811: Failed to create new GPIO device');
  end;
end;

{==============================================================================}

function STMPE1601GPIOCreate(I2C:PI2CDevice;Address:Word;IRQ:PGPIOInfo):PGPIODevice;{$IFDEF API_EXPORT_STMPE} stdcall;{$ENDIF}
{Create, register and start a new STMPE1601 GPIO device connected to the specified I2C device}
{I2C: The I2C device this STMPE1601 is connected to}
{Address: The I2C address for this STMPE1601}
{IRQ: The GPIO information for the IRQ line (Optional)}
{Return: Pointer to the new GPIO device or nil on failure}
var
 Status:LongWord;
 
 STMPEGPIO:PSTMPEGPIO;
begin
 {}
 Result:=nil;
 
 {Initialize}
 STMPEInit;
 
 {$IF DEFINED(STMPE_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(nil,'STMPE1601: GPIO Create (Address=' + IntToHex(Address,4) + ')');
 {$ENDIF}
 
 {Check I2C}
 if I2C = nil then Exit;
 
 {Check Address}
 if Address = I2C_ADDRESS_INVALID then Exit;
 
 {Create GPIO}
 STMPEGPIO:=PSTMPEGPIO(GPIODeviceCreateEx(SizeOf(TSTMPEGPIO)));
 if STMPEGPIO <> nil then
  begin
   {Update GPIO}
   {Device}
   STMPEGPIO.GPIO.Device.DeviceBus:=DEVICE_BUS_I2C; 
   STMPEGPIO.GPIO.Device.DeviceType:=GPIO_TYPE_NONE;
   STMPEGPIO.GPIO.Device.DeviceFlags:=GPIO_FLAG_PULL_UP;
   STMPEGPIO.GPIO.Device.DeviceData:=nil;
   STMPEGPIO.GPIO.Device.DeviceDescription:=STMPE1601_GPIO_DESCRIPTION;
   {GPIO}
   STMPEGPIO.GPIO.GPIOState:=GPIO_STATE_DISABLED;
   STMPEGPIO.GPIO.DeviceStart:=STMPEGPIOStart;
   STMPEGPIO.GPIO.DeviceStop:=STMPEGPIOStop;
   STMPEGPIO.GPIO.DeviceRead:=STMPEGPIORead;
   STMPEGPIO.GPIO.DeviceWrite:=STMPEGPIOWrite;
   STMPEGPIO.GPIO.DeviceInputGet:=STMPEGPIOInputGet;
   STMPEGPIO.GPIO.DeviceOutputSet:=STMPEGPIOOutputSet;
   STMPEGPIO.GPIO.DevicePullGet:=STMPEGPIOPullGet;
   STMPEGPIO.GPIO.DevicePullSelect:=STMPEGPIOPullSelect;
   STMPEGPIO.GPIO.DeviceFunctionGet:=STMPEGPIOFunctionGet;
   STMPEGPIO.GPIO.DeviceFunctionSelect:=STMPEGPIOFunctionSelect;    
   {Driver}
   STMPEGPIO.GPIO.Address:=nil;
   STMPEGPIO.GPIO.Properties.Flags:=STMPEGPIO.GPIO.Device.DeviceFlags;
   STMPEGPIO.GPIO.Properties.PinMin:=STMPE1601_GPIO_MIN_PIN;
   STMPEGPIO.GPIO.Properties.PinMax:=STMPE1601_GPIO_MAX_PIN;
   STMPEGPIO.GPIO.Properties.PinCount:=STMPE1601_GPIO_PIN_COUNT;
   STMPEGPIO.GPIO.Properties.FunctionMin:=STMPE1601_GPIO_MIN_FUNCTION;
   STMPEGPIO.GPIO.Properties.FunctionMax:=STMPE1601_GPIO_MAX_FUNCTION;
   STMPEGPIO.GPIO.Properties.FunctionCount:=STMPE1601_GPIO_FUNCTION_COUNT;
   {STMPE}
   STMPEGPIO.Control.Chip:=STMPE_CHIP_STMPE1601;
   if IRQ <> nil then STMPEGPIO.Control.IRQ:=IRQ^ else STMPEGPIO.Control.IRQ:=GPIO_INFO_UNKNOWN;
   STMPEGPIO.Control.I2C:=I2C;
   STMPEGPIO.Control.Address:=Address;
   STMPEGPIO.Control.SPI:=nil;
   STMPEGPIO.Control.ChipSelect:=SPI_CS_NONE;
   STMPEGPIO.Control.RegMax:=STMPE1601_REG_MAX;
   STMPEGPIO.Control.RegDir:=STMPE_DIR_DESCENDING;
   STMPEGPIO.Control.RegSize:=STMPE1601_REG_SIZE;
   
   {Register GPIO}
   Status:=GPIODeviceRegister(@STMPEGPIO.GPIO);
   if Status = ERROR_SUCCESS then
    begin
     {Start GPIO}
     Status:=GPIODeviceStart(@STMPEGPIO.GPIO);
     if Status = ERROR_SUCCESS then
      begin
       {Return Result}
       Result:=PGPIODevice(STMPEGPIO);
      end
     else 
      begin
       if GPIO_LOG_ENABLED then GPIOLogError(nil,'STMPE1601: Failed to start new GPIO device: ' + ErrorToString(Status));
       
       {Deregister GPIO}
       GPIODeviceDeregister(@STMPEGPIO.GPIO);
       
       {Destroy GPIO}
       GPIODeviceDestroy(@STMPEGPIO.GPIO);
      end;
    end
   else
    begin
     if GPIO_LOG_ENABLED then GPIOLogError(nil,'STMPE1601: Failed to register new GPIO device: ' + ErrorToString(Status));
     
     {Destroy GPIO}
     GPIODeviceDestroy(@STMPEGPIO.GPIO);
    end;
  end
 else 
  begin
   if GPIO_LOG_ENABLED then GPIOLogError(nil,'STMPE1601: Failed to create new GPIO device');
  end;
end;

{==============================================================================}

function STMPE1801GPIOCreate(I2C:PI2CDevice;Address:Word;IRQ:PGPIOInfo):PGPIODevice;{$IFDEF API_EXPORT_STMPE} stdcall;{$ENDIF}
{Create, register and start a new STMPE1801 GPIO device connected to the specified I2C device}
{I2C: The I2C device this STMPE1801 is connected to}
{Address: The I2C address for this STMPE1801}
{IRQ: The GPIO information for the IRQ line (Optional)}
{Return: Pointer to the new GPIO device or nil on failure}
var
 Status:LongWord;
 
 STMPEGPIO:PSTMPEGPIO;
begin
 {}
 Result:=nil;
 
 {Initialize}
 STMPEInit;
 
 {$IF DEFINED(STMPE_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(nil,'STMPE1801: GPIO Create (Address=' + IntToHex(Address,4) + ')');
 {$ENDIF}
 
 {Check I2C}
 if I2C = nil then Exit;
 
 {Check Address}
 if Address = I2C_ADDRESS_INVALID then Exit;
 
 {Create GPIO}
 STMPEGPIO:=PSTMPEGPIO(GPIODeviceCreateEx(SizeOf(TSTMPEGPIO)));
 if STMPEGPIO <> nil then
  begin
   {Update GPIO}
   {Device}
   STMPEGPIO.GPIO.Device.DeviceBus:=DEVICE_BUS_I2C; 
   STMPEGPIO.GPIO.Device.DeviceType:=GPIO_TYPE_NONE;
   STMPEGPIO.GPIO.Device.DeviceFlags:=GPIO_FLAG_PULL_UP;
   STMPEGPIO.GPIO.Device.DeviceData:=nil;
   STMPEGPIO.GPIO.Device.DeviceDescription:=STMPE1801_GPIO_DESCRIPTION;
   {GPIO}
   STMPEGPIO.GPIO.GPIOState:=GPIO_STATE_DISABLED;
   STMPEGPIO.GPIO.DeviceStart:=STMPEGPIOStart;
   STMPEGPIO.GPIO.DeviceStop:=STMPEGPIOStop;
   STMPEGPIO.GPIO.DeviceRead:=STMPEGPIORead;
   STMPEGPIO.GPIO.DeviceWrite:=STMPEGPIOWrite;
   STMPEGPIO.GPIO.DeviceInputGet:=STMPEGPIOInputGet;
   STMPEGPIO.GPIO.DeviceOutputSet:=STMPEGPIOOutputSet;
   STMPEGPIO.GPIO.DevicePullGet:=STMPEGPIOPullGet;
   STMPEGPIO.GPIO.DevicePullSelect:=STMPEGPIOPullSelect;
   STMPEGPIO.GPIO.DeviceFunctionGet:=STMPEGPIOFunctionGet;
   STMPEGPIO.GPIO.DeviceFunctionSelect:=STMPEGPIOFunctionSelect;    
   {Driver}
   STMPEGPIO.GPIO.Address:=nil;
   STMPEGPIO.GPIO.Properties.Flags:=STMPEGPIO.GPIO.Device.DeviceFlags;
   STMPEGPIO.GPIO.Properties.PinMin:=STMPE1801_GPIO_MIN_PIN;
   STMPEGPIO.GPIO.Properties.PinMax:=STMPE1801_GPIO_MAX_PIN;
   STMPEGPIO.GPIO.Properties.PinCount:=STMPE1801_GPIO_PIN_COUNT;
   STMPEGPIO.GPIO.Properties.FunctionMin:=STMPE1801_GPIO_MIN_FUNCTION;
   STMPEGPIO.GPIO.Properties.FunctionMax:=STMPE1801_GPIO_MAX_FUNCTION;
   STMPEGPIO.GPIO.Properties.FunctionCount:=STMPE1801_GPIO_FUNCTION_COUNT;
   {STMPE}
   STMPEGPIO.Control.Chip:=STMPE_CHIP_STMPE1801;
   if IRQ <> nil then STMPEGPIO.Control.IRQ:=IRQ^ else STMPEGPIO.Control.IRQ:=GPIO_INFO_UNKNOWN;
   STMPEGPIO.Control.I2C:=I2C;
   STMPEGPIO.Control.Address:=Address;
   STMPEGPIO.Control.SPI:=nil;
   STMPEGPIO.Control.ChipSelect:=SPI_CS_NONE;
   STMPEGPIO.Control.RegMax:=STMPE1801_REG_MAX;
   STMPEGPIO.Control.RegDir:=STMPE_DIR_ASCENDING;
   STMPEGPIO.Control.RegSize:=STMPE1801_REG_SIZE;
   
   {Register GPIO}
   Status:=GPIODeviceRegister(@STMPEGPIO.GPIO);
   if Status = ERROR_SUCCESS then
    begin
     {Start GPIO}
     Status:=GPIODeviceStart(@STMPEGPIO.GPIO);
     if Status = ERROR_SUCCESS then
      begin
       {Return Result}
       Result:=PGPIODevice(STMPEGPIO);
      end
     else 
      begin
       if GPIO_LOG_ENABLED then GPIOLogError(nil,'STMPE1801: Failed to start new GPIO device: ' + ErrorToString(Status));
       
       {Deregister GPIO}
       GPIODeviceDeregister(@STMPEGPIO.GPIO);
       
       {Destroy GPIO}
       GPIODeviceDestroy(@STMPEGPIO.GPIO);
      end;
    end
   else
    begin
     if GPIO_LOG_ENABLED then GPIOLogError(nil,'STMPE1801: Failed to register new GPIO device: ' + ErrorToString(Status));
     
     {Destroy GPIO}
     GPIODeviceDestroy(@STMPEGPIO.GPIO);
    end;
  end
 else 
  begin
   if GPIO_LOG_ENABLED then GPIOLogError(nil,'STMPE1801: Failed to create new GPIO device');
  end;
end;

{==============================================================================}

function STMPE2401GPIOCreate(I2C:PI2CDevice;Address:Word;IRQ:PGPIOInfo):PGPIODevice;{$IFDEF API_EXPORT_STMPE} stdcall;{$ENDIF}
{Create, register and start a new STMPE2401 GPIO device connected to the specified I2C device}
{I2C: The I2C device this STMPE2401 is connected to}
{Address: The I2C address for this STMPE2401}
{IRQ: The GPIO information for the IRQ line (Optional)}
{Return: Pointer to the new GPIO device or nil on failure}
var
 Status:LongWord;
 
 STMPEGPIO:PSTMPEGPIO;
begin
 {}
 Result:=nil;
 
 {Initialize}
 STMPEInit;
 
 {$IF DEFINED(STMPE_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(nil,'STMPE2401: GPIO Create (Address=' + IntToHex(Address,4) + ')');
 {$ENDIF}
 
 {Check I2C}
 if I2C = nil then Exit;
 
 {Check Address}
 if Address = I2C_ADDRESS_INVALID then Exit;
 
 {Create GPIO}
 STMPEGPIO:=PSTMPEGPIO(GPIODeviceCreateEx(SizeOf(TSTMPEGPIO)));
 if STMPEGPIO <> nil then
  begin
   {Update GPIO}
   {Device}
   STMPEGPIO.GPIO.Device.DeviceBus:=DEVICE_BUS_I2C; 
   STMPEGPIO.GPIO.Device.DeviceType:=GPIO_TYPE_NONE;
   STMPEGPIO.GPIO.Device.DeviceFlags:=GPIO_FLAG_PULL_UP or GPIO_FLAG_PULL_DOWN;
   STMPEGPIO.GPIO.Device.DeviceData:=nil;
   STMPEGPIO.GPIO.Device.DeviceDescription:=STMPE2401_GPIO_DESCRIPTION;
   {GPIO}
   STMPEGPIO.GPIO.GPIOState:=GPIO_STATE_DISABLED;
   STMPEGPIO.GPIO.DeviceStart:=STMPEGPIOStart;
   STMPEGPIO.GPIO.DeviceStop:=STMPEGPIOStop;
   STMPEGPIO.GPIO.DeviceRead:=STMPEGPIORead;
   STMPEGPIO.GPIO.DeviceWrite:=STMPEGPIOWrite;
   STMPEGPIO.GPIO.DeviceInputGet:=STMPEGPIOInputGet;
   STMPEGPIO.GPIO.DeviceOutputSet:=STMPEGPIOOutputSet;
   STMPEGPIO.GPIO.DevicePullGet:=STMPEGPIOPullGet;
   STMPEGPIO.GPIO.DevicePullSelect:=STMPEGPIOPullSelect;
   STMPEGPIO.GPIO.DeviceFunctionGet:=STMPEGPIOFunctionGet;
   STMPEGPIO.GPIO.DeviceFunctionSelect:=STMPEGPIOFunctionSelect;    
   {Driver}
   STMPEGPIO.GPIO.Address:=nil;
   STMPEGPIO.GPIO.Properties.Flags:=STMPEGPIO.GPIO.Device.DeviceFlags;
   STMPEGPIO.GPIO.Properties.PinMin:=STMPE240X_GPIO_MIN_PIN;
   STMPEGPIO.GPIO.Properties.PinMax:=STMPE240X_GPIO_MAX_PIN;
   STMPEGPIO.GPIO.Properties.PinCount:=STMPE240X_GPIO_PIN_COUNT;
   STMPEGPIO.GPIO.Properties.FunctionMin:=STMPE240X_GPIO_MIN_FUNCTION;
   STMPEGPIO.GPIO.Properties.FunctionMax:=STMPE240X_GPIO_MAX_FUNCTION;
   STMPEGPIO.GPIO.Properties.FunctionCount:=STMPE240X_GPIO_FUNCTION_COUNT;
   {STMPE}
   STMPEGPIO.Control.Chip:=STMPE_CHIP_STMPE2401;
   if IRQ <> nil then STMPEGPIO.Control.IRQ:=IRQ^ else STMPEGPIO.Control.IRQ:=GPIO_INFO_UNKNOWN;
   STMPEGPIO.Control.I2C:=I2C;
   STMPEGPIO.Control.Address:=Address;
   STMPEGPIO.Control.SPI:=nil;
   STMPEGPIO.Control.ChipSelect:=SPI_CS_NONE;
   STMPEGPIO.Control.RegMax:=STMPE240X_REG_MAX;
   STMPEGPIO.Control.RegDir:=STMPE_DIR_DESCENDING;
   STMPEGPIO.Control.RegSize:=STMPE240X_REG_SIZE;
   
   {Register GPIO}
   Status:=GPIODeviceRegister(@STMPEGPIO.GPIO);
   if Status = ERROR_SUCCESS then
    begin
     {Start GPIO}
     Status:=GPIODeviceStart(@STMPEGPIO.GPIO);
     if Status = ERROR_SUCCESS then
      begin
       {Return Result}
       Result:=PGPIODevice(STMPEGPIO);
      end
     else 
      begin
       if GPIO_LOG_ENABLED then GPIOLogError(nil,'STMPE2401: Failed to start new GPIO device: ' + ErrorToString(Status));
       
       {Deregister GPIO}
       GPIODeviceDeregister(@STMPEGPIO.GPIO);
       
       {Destroy GPIO}
       GPIODeviceDestroy(@STMPEGPIO.GPIO);
      end;
    end
   else
    begin
     if GPIO_LOG_ENABLED then GPIOLogError(nil,'STMPE2401: Failed to register new GPIO device: ' + ErrorToString(Status));
     
     {Destroy GPIO}
     GPIODeviceDestroy(@STMPEGPIO.GPIO);
    end;
  end
 else 
  begin
   if GPIO_LOG_ENABLED then GPIOLogError(nil,'STMPE2401: Failed to create new GPIO device');
  end;
end;

{==============================================================================}

function STMPEGPIODestroy(GPIO:PGPIODevice):LongWord;{$IFDEF API_EXPORT_STMPE} stdcall;{$ENDIF}
{Stop, deregister and destroy an STMPE GPIO device created by this driver}
{GPIO: The GPIO device to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check GPIO}
 if GPIO = nil then Exit;

 {$IF DEFINED(STMPE_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'STMPE: GPIO Destroy');
 {$ENDIF}
 
 {Stop GPIO}
 Result:=GPIODeviceStop(GPIO);
 if Result = ERROR_SUCCESS then
  begin
   {Deregister GPIO}
   Result:=GPIODeviceDeregister(GPIO);
   if Result = ERROR_SUCCESS then
    begin
     {Destroy GPIO}
     Result:=GPIODeviceDestroy(GPIO);
     if Result <> ERROR_SUCCESS then
      begin
       if GPIO_LOG_ENABLED then GPIOLogError(nil,'STMPE: Failed to destroy GPIO device: ' + ErrorToString(Result));
      end;      
    end
   else
    begin
     if GPIO_LOG_ENABLED then GPIOLogError(nil,'STMPE: Failed to deregister GPIO device: ' + ErrorToString(Result));
    end;    
  end
 else
  begin
   if GPIO_LOG_ENABLED then GPIOLogError(nil,'STMPE: Failed to stop GPIO device: ' + ErrorToString(Result));
  end;  
end;

{==============================================================================}

function STMPE610TouchCreate(I2C:PI2CDevice;SPI:PSPIDevice;Address,ChipSelect:Word;Width,Height:LongWord;IRQ:PGPIOInfo):PTouchDevice;{$IFDEF API_EXPORT_STMPE} stdcall;{$ENDIF}
{Create, register and start a new STMPE610 Touch device connected to the specified I2C or SPI device}
{I2C: The I2C device this STMPE610 is connected to (Optional)}
{SPI: The SPI device this STMPE610 is connected to (Optional)}
{Address: The I2C address for this STMPE610 (or I2C_ADDRESS_INVALID if SPI connected)}
{ChipSelect: The SPI chip select for this STMPE610 (or SPI_CS_NONE if I2C connected)}
{Width: The width of the screen in pixels}
{Height: The height of the screen in pixels}
{IRQ: The GPIO information for the IRQ line (Optional)}
{Return: Pointer to the new Touch device or nil on failure}

{Note: Either I2C or SPI must be specified but not both}
var
 Status:LongWord;
 
 STMPETouch:PSTMPETouch;
begin
 {}
 Result:=nil;

 {Initialize}
 STMPEInit;
 
 {$IF DEFINED(STMPE_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(nil,'STMPE610: Touch Create (Address=' + IntToHex(Address,4) + ' ChipSelect=' + IntToStr(ChipSelect) + ' Width=' + IntToStr(Width) + ' Height=' + IntToStr(Height) + ')');
 {$ENDIF}
 
 {Check Width and Height}
 if Width < 1 then Exit;
 if Height < 1 then Exit;
 
 {Check I2C and SPI}
 if (I2C = nil) and (SPI = nil) then Exit;
 if (I2C <> nil) and (SPI <> nil) then Exit;
 
 {Check Address}
 if (I2C <> nil) and (Address = I2C_ADDRESS_INVALID) then Exit;
 
 {Check ChipSelect}
 if (SPI <> nil) and (ChipSelect = SPI_CS_NONE) then Exit;
 
 {Create Touch}
 STMPETouch:=PSTMPETouch(TouchDeviceCreateEx(SizeOf(TSTMPETouch)));
 if STMPETouch <> nil then
  begin
   {Update Touch}
   {Device}
   STMPETouch.Touch.Device.DeviceBus:=DEVICE_BUS_I2C; 
   if SPI <> nil then STMPETouch.Touch.Device.DeviceBus:=DEVICE_BUS_SPI;
   STMPETouch.Touch.Device.DeviceType:=TOUCH_TYPE_RESISTIVE;
   STMPETouch.Touch.Device.DeviceFlags:=STMPETouch.Touch.Device.DeviceFlags or TOUCH_FLAG_PRESSURE or TOUCH_FLAG_RELEASE_TIMER;
   STMPETouch.Touch.Device.DeviceData:=nil;
   STMPETouch.Touch.Device.DeviceDescription:=STMPE610_TOUCH_DESCRIPTION;
   {Touch}
   STMPETouch.Touch.TouchState:=TOUCH_STATE_DISABLED;
   STMPETouch.Touch.DeviceStart:=STMPETouchStart;
   STMPETouch.Touch.DeviceStop:=STMPETouchStop;
   STMPETouch.Touch.DeviceUpdate:=STMPETouchUpdate;
   {Driver}
   STMPETouch.Touch.Properties.Flags:=STMPETouch.Touch.Device.DeviceFlags;
   STMPETouch.Touch.Properties.Width:=Width;
   STMPETouch.Touch.Properties.Height:=Height;
   STMPETouch.Touch.Properties.Rotation:=TOUCH_ROTATION_0;
   STMPETouch.Touch.Properties.MaxX:=0;
   STMPETouch.Touch.Properties.MaxY:=0;
   STMPETouch.Touch.Properties.MaxZ:=0;
   STMPETouch.Touch.Properties.MaxPoints:=0;
   {General}
   STMPETouch.Width:=Width;
   STMPETouch.Height:=Height;
   STMPETouch.MaxX:=STMPE610_MAX_X;
   STMPETouch.MaxY:=STMPE610_MAX_Y;
   STMPETouch.MaxZ:=STMPE610_MAX_Z;
   STMPETouch.MaxPoints:=STMPE610_MAX_POINTS;
   {STMPE}
   STMPETouch.Control.Chip:=STMPE_CHIP_STMPE610;
   if IRQ <> nil then STMPETouch.Control.IRQ:=IRQ^ else STMPETouch.Control.IRQ:=GPIO_INFO_UNKNOWN;
   STMPETouch.Control.I2C:=I2C;
   STMPETouch.Control.Address:=Address;
   STMPETouch.Control.SPI:=SPI;
   STMPETouch.Control.ChipSelect:=ChipSelect;
   STMPETouch.Control.RegMax:=STMPE811_REG_MAX;
   STMPETouch.Control.RegDir:=STMPE_DIR_ASCENDING;
   STMPETouch.Control.RegSize:=STMPE811_REG_SIZE;
   STMPETouch.Timer:=INVALID_HANDLE_VALUE;
   
   {Register Touch}
   Status:=TouchDeviceRegister(@STMPETouch.Touch);
   if Status = ERROR_SUCCESS then
    begin
     {Start Touch}
     Status:=TouchDeviceStart(@STMPETouch.Touch);
     if Status = ERROR_SUCCESS then
      begin
       {Return Result}
       Result:=PTouchDevice(STMPETouch);
      end
     else 
      begin
       if TOUCH_LOG_ENABLED then TouchLogError(nil,'STMPE610: Failed to start new Touch device: ' + ErrorToString(Status));
       
       {Deregister Touch}
       TouchDeviceDeregister(@STMPETouch.Touch);
       
       {Destroy Touch}
       TouchDeviceDestroy(@STMPETouch.Touch);
      end;
    end
   else
    begin
     if TOUCH_LOG_ENABLED then TouchLogError(nil,'STMPE610: Failed to register new Touch device: ' + ErrorToString(Status));
     
     {Destroy Touch}
     TouchDeviceDestroy(@STMPETouch.Touch);
    end;
  end
 else 
  begin
   if TOUCH_LOG_ENABLED then TouchLogError(nil,'STMPE610: Failed to create new Touch device');
  end;
end;

{==============================================================================}

function STMPE811TouchCreate(I2C:PI2CDevice;SPI:PSPIDevice;Address,ChipSelect:Word;Width,Height:LongWord;IRQ:PGPIOInfo):PTouchDevice;{$IFDEF API_EXPORT_STMPE} stdcall;{$ENDIF}
{Create, register and start a new STMPE811 Touch device connected to the specified I2C or SPI device}
{I2C: The I2C device this STMPE811 is connected to (Optional)}
{SPI: The SPI device this STMPE811 is connected to (Optional)}
{Address: The I2C address for this STMPE811 (or I2C_ADDRESS_INVALID if SPI connected)}
{ChipSelect: The SPI chip select for this STMPE811 (or SPI_CS_NONE if I2C connected)}
{Width: The width of the screen in pixels}
{Height: The height of the screen in pixels}
{IRQ: The GPIO information for the IRQ line (Optional)}
{Return: Pointer to the new Touch device or nil on failure}

{Note: Either I2C or SPI must be specified but not both}
var
 Status:LongWord;
 
 STMPETouch:PSTMPETouch;
begin
 {}
 Result:=nil;
 
 {Initialize}
 STMPEInit;
 
 {$IF DEFINED(STMPE_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(nil,'STMPE811: Touch Create (Address=' + IntToHex(Address,4) + ' ChipSelect=' + IntToStr(ChipSelect) + ' Width=' + IntToStr(Width) + ' Height=' + IntToStr(Height) + ')');
 {$ENDIF}
 
 {Check Width and Height}
 if Width < 1 then Exit;
 if Height < 1 then Exit;
 
 {Check I2C and SPI}
 if (I2C = nil) and (SPI = nil) then Exit;
 if (I2C <> nil) and (SPI <> nil) then Exit;
 
 {Check Address}
 if (I2C <> nil) and (Address = I2C_ADDRESS_INVALID) then Exit;
 
 {Check ChipSelect}
 if (SPI <> nil) and (ChipSelect = SPI_CS_NONE) then Exit;
 
 {Create Touch}
 STMPETouch:=PSTMPETouch(TouchDeviceCreateEx(SizeOf(TSTMPETouch)));
 if STMPETouch <> nil then
  begin
   {Update Touch}
   {Device}
   STMPETouch.Touch.Device.DeviceBus:=DEVICE_BUS_I2C; 
   if SPI <> nil then STMPETouch.Touch.Device.DeviceBus:=DEVICE_BUS_SPI;
   STMPETouch.Touch.Device.DeviceType:=TOUCH_TYPE_RESISTIVE;
   STMPETouch.Touch.Device.DeviceFlags:=STMPETouch.Touch.Device.DeviceFlags or TOUCH_FLAG_PRESSURE or TOUCH_FLAG_RELEASE_TIMER;
   STMPETouch.Touch.Device.DeviceData:=nil;
   STMPETouch.Touch.Device.DeviceDescription:=STMPE811_TOUCH_DESCRIPTION;
   {Touch}
   STMPETouch.Touch.TouchState:=TOUCH_STATE_DISABLED;
   STMPETouch.Touch.DeviceStart:=STMPETouchStart;
   STMPETouch.Touch.DeviceStop:=STMPETouchStop;
   STMPETouch.Touch.DeviceUpdate:=STMPETouchUpdate;
   {Driver}
   STMPETouch.Touch.Properties.Flags:=STMPETouch.Touch.Device.DeviceFlags;
   STMPETouch.Touch.Properties.Width:=Width;
   STMPETouch.Touch.Properties.Height:=Height;
   STMPETouch.Touch.Properties.Rotation:=TOUCH_ROTATION_0;
   STMPETouch.Touch.Properties.MaxX:=0;
   STMPETouch.Touch.Properties.MaxY:=0;
   STMPETouch.Touch.Properties.MaxZ:=0;
   STMPETouch.Touch.Properties.MaxPoints:=0;
   {General}
   STMPETouch.Width:=Width;
   STMPETouch.Height:=Height;
   STMPETouch.MaxX:=STMPE811_MAX_X;
   STMPETouch.MaxY:=STMPE811_MAX_Y;
   STMPETouch.MaxZ:=STMPE811_MAX_Z;
   STMPETouch.MaxPoints:=STMPE811_MAX_POINTS;
   {STMPE}
   STMPETouch.Control.Chip:=STMPE_CHIP_STMPE811;
   if IRQ <> nil then STMPETouch.Control.IRQ:=IRQ^ else STMPETouch.Control.IRQ:=GPIO_INFO_UNKNOWN;
   STMPETouch.Control.I2C:=I2C;
   STMPETouch.Control.Address:=Address;
   STMPETouch.Control.SPI:=SPI;
   STMPETouch.Control.ChipSelect:=ChipSelect;
   STMPETouch.Control.RegMax:=STMPE811_REG_MAX;
   STMPETouch.Control.RegDir:=STMPE_DIR_ASCENDING;
   STMPETouch.Control.RegSize:=STMPE811_REG_SIZE;
   STMPETouch.Timer:=INVALID_HANDLE_VALUE;
   
   {Register Touch}
   Status:=TouchDeviceRegister(@STMPETouch.Touch);
   if Status = ERROR_SUCCESS then
    begin
     {Start Touch}
     Status:=TouchDeviceStart(@STMPETouch.Touch);
     if Status = ERROR_SUCCESS then
      begin
       {Return Result}
       Result:=PTouchDevice(STMPETouch);
      end
     else 
      begin
       if TOUCH_LOG_ENABLED then TouchLogError(nil,'STMPE811: Failed to start new Touch device: ' + ErrorToString(Status));
       
       {Deregister Touch}
       TouchDeviceDeregister(@STMPETouch.Touch);
       
       {Destroy Touch}
       TouchDeviceDestroy(@STMPETouch.Touch);
      end;
    end
   else
    begin
     if TOUCH_LOG_ENABLED then TouchLogError(nil,'STMPE811: Failed to register new Touch device: ' + ErrorToString(Status));
     
     {Destroy Touch}
     TouchDeviceDestroy(@STMPETouch.Touch);
    end;
  end
 else 
  begin
   if TOUCH_LOG_ENABLED then TouchLogError(nil,'STMPE811: Failed to create new Touch device');
  end;
end;

{==============================================================================}

function STMPETouchDestroy(Touch:PTouchDevice):LongWord;{$IFDEF API_EXPORT_STMPE} stdcall;{$ENDIF}
{Stop, deregister and destroy an STMPE Touch device created by this driver}
{Touch: The Touch device to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Touch}
 if Touch = nil then Exit;
 
 {$IF DEFINED(STMPE_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(Touch,'STMPE: Touch Destroy');
 {$ENDIF}
 
 {Stop Touch}
 Result:=TouchDeviceStop(Touch);
 if Result = ERROR_SUCCESS then
  begin
   {Deregister Touch}
   Result:=TouchDeviceDeregister(Touch);
   if Result = ERROR_SUCCESS then
    begin
     {Destroy Touch}
     Result:=TouchDeviceDestroy(Touch);
     if Result <> ERROR_SUCCESS then
      begin
       if TOUCH_LOG_ENABLED then TouchLogError(nil,'STMPE: Failed to destroy Touch device: ' + ErrorToString(Result));
      end;      
    end
   else
    begin
     if TOUCH_LOG_ENABLED then TouchLogError(nil,'STMPE: Failed to deregister Touch device: ' + ErrorToString(Result));
    end;    
  end
 else
  begin
   if TOUCH_LOG_ENABLED then TouchLogError(nil,'STMPE: Failed to stop Touch device: ' + ErrorToString(Result));
  end;  
end;

{==============================================================================}
{==============================================================================}
{STMPE GPIO Functions}
function STMPEGPIOStart(GPIO:PGPIODevice):LongWord; 
{Implementation of GPIODeviceStart API for STMPE}
{Note: Not intended to be called directly by applications, use GPIODeviceStart instead}
var
 ChipID:Word;
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(STMPE_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'STMPE: GPIO Start');
 {$ENDIF}
 
 {Check I2C/SPI}
 if PSTMPEGPIO(GPIO).Control.I2C <> nil then 
  begin
   {Start I2C Device}
   if I2CDeviceStart(PSTMPEGPIO(GPIO).Control.I2C,STMPE_I2C_RATE) <> ERROR_SUCCESS then
    begin
     Result:=ERROR_OPERATION_FAILED;
     Exit;
    end;
  end
 else if PSTMPEGPIO(GPIO).Control.SPI <> nil then
  begin
   {Set the SPI clock rate}
   if SPIDeviceSetClockRate(PSTMPEGPIO(GPIO).Control.SPI,PSTMPEGPIO(GPIO).Control.ChipSelect,STMPE_SPI_RATE) <> ERROR_SUCCESS then
    begin
     Result:=ERROR_OPERATION_FAILED;
     Exit;
    end;
   
   {Start SPI Device}
   if SPIDeviceStart(PSTMPEGPIO(GPIO).Control.SPI,SPI_MODE_4WIRE,STMPE_SPI_RATE,SPI_CLOCK_PHASE_LOW,SPI_CLOCK_POLARITY_LOW) <> ERROR_SUCCESS then
    begin
     Result:=ERROR_OPERATION_FAILED;
     Exit;
    end;
  end
 else
  begin
   Exit;
  end;  

 {Check Chip}
 case PSTMPEGPIO(GPIO).Control.Chip of
  STMPE_CHIP_STMPE610,STMPE_CHIP_STMPE811:begin
    {Get ID}
    if STMPEReadWord(@PSTMPEGPIO(GPIO).Control,STMPE811_REG_CHIP_ID,@ChipID) <> ERROR_SUCCESS then
     begin
      Result:=ERROR_OPERATION_FAILED;
      Exit;
     end;
    {$IF DEFINED(STMPE_DEBUG) or DEFINED(GPIO_DEBUG)}
    if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'STMPE:  STMPE811_REG_CHIP_ID=' + IntToHex(ChipID,4));
    {$ENDIF}
    
    {Check ID}
    if ChipID <> STMPE811_CHIP_ID then
     begin
      Result:=ERROR_OPERATION_FAILED;
      Exit;
     end;
     
    {Setup Registers}
    PSTMPEGPIO(GPIO).Offsets:=STMPE811Offsets;
    
    {Enable GPIO}
    if STMPESetBits(@PSTMPEGPIO(GPIO).Control,PSTMPEGPIO(GPIO).Offsets.SysCtrl,STMPE811_SYS_CTRL2_GPIO_OFF,0) <> ERROR_SUCCESS then
     begin
      Result:=ERROR_OPERATION_FAILED;
      Exit;
     end;
   end;
  STMPE_CHIP_STMPE801:begin
    {Get ID}
    if STMPEReadWord(@PSTMPEGPIO(GPIO).Control,STMPE801_REG_CHIP_ID,@ChipID) <> ERROR_SUCCESS then
     begin
      Result:=ERROR_OPERATION_FAILED;
      Exit;
     end;
    {$IF DEFINED(STMPE_DEBUG) or DEFINED(GPIO_DEBUG)}
    if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'STMPE:  STMPE801_REG_CHIP_ID=' + IntToHex(ChipID,4));
    {$ENDIF}

    {Check ID}
    if ChipID <> STMPE801_CHIP_ID then
     begin
      Result:=ERROR_OPERATION_FAILED;
      Exit;
     end;
     
    {Setup Registers}
    PSTMPEGPIO(GPIO).Offsets:=STMPE801Offsets;
    
    {Enable GPIO}
    {Nothing}
   end;
  STMPE_CHIP_STMPE1601:begin
    {Get ID}
    if STMPEReadByte(@PSTMPEGPIO(GPIO).Control,STMPE1601_REG_CHIP_ID,PByte(@ChipID)) <> ERROR_SUCCESS then
     begin
      Result:=ERROR_OPERATION_FAILED;
      Exit;
     end;
    {$IF DEFINED(STMPE_DEBUG) or DEFINED(GPIO_DEBUG)}
    if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'STMPE:  STMPE1601_REG_CHIP_ID=' + IntToHex(ChipID,2));
    {$ENDIF}

    {Check ID}
    if ChipID <> STMPE1601_CHIP_ID then
     begin
      Result:=ERROR_OPERATION_FAILED;
      Exit;
     end;
     
    {Setup Registers}
    PSTMPEGPIO(GPIO).Offsets:=STMPE1601Offsets;
    
    {Enable GPIO}
    if STMPESetBits(@PSTMPEGPIO(GPIO).Control,PSTMPEGPIO(GPIO).Offsets.SysCtrl,STMPE1601_SYS_CTRL_ENABLE_GPIO,STMPE1601_SYS_CTRL_ENABLE_GPIO) <> ERROR_SUCCESS then
     begin
      Result:=ERROR_OPERATION_FAILED;
      Exit;
     end;
   end;
  STMPE_CHIP_STMPE1801:begin
    {Get ID}
    if STMPEReadByte(@PSTMPEGPIO(GPIO).Control,STMPE1801_REG_CHIP_ID,PByte(@ChipID)) <> ERROR_SUCCESS then
     begin
      Result:=ERROR_OPERATION_FAILED;
      Exit;
     end;
    {$IF DEFINED(STMPE_DEBUG) or DEFINED(GPIO_DEBUG)}
    if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'STMPE:  STMPE1801_REG_CHIP_ID=' + IntToHex(ChipID,2));
    {$ENDIF}

    {Check ID}
    if ChipID <> STMPE1801_CHIP_ID then
     begin
      Result:=ERROR_OPERATION_FAILED;
      Exit;
     end;
     
    {Setup Registers}
    PSTMPEGPIO(GPIO).Offsets:=STMPE1801Offsets;
    
    {Enable GPIO}
    {Nothing}
   end;
  STMPE_CHIP_STMPE2401,STMPE_CHIP_STMPE2403:begin
    {Get ID}
    if STMPEReadByte(@PSTMPEGPIO(GPIO).Control,STMPE240X_REG_CHIP_ID,PByte(@ChipID)) <> ERROR_SUCCESS then
     begin
      Result:=ERROR_OPERATION_FAILED;
      Exit;
     end;
    {$IF DEFINED(STMPE_DEBUG) or DEFINED(GPIO_DEBUG)}
    if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'STMPE:  STMPE240X_REG_CHIP_ID=' + IntToHex(ChipID,2));
    {$ENDIF}

    {Check ID}
    if ChipID <> STMPE240X_CHIP_ID then
     begin
      Result:=ERROR_OPERATION_FAILED;
      Exit;
     end;
     
    {Setup Registers}
    PSTMPEGPIO(GPIO).Offsets:=STMPE240XOffsets;
    
    {Enable GPIO}
    if STMPESetBits(@PSTMPEGPIO(GPIO).Control,PSTMPEGPIO(GPIO).Offsets.SysCtrl,STMPE240X_SYS_CTRL_ENABLE_GPIO,STMPE240X_SYS_CTRL_ENABLE_GPIO) <> ERROR_SUCCESS then
     begin
      Result:=ERROR_OPERATION_FAILED;
      Exit;
     end;
   end;
  else
   begin
    Result:=ERROR_OPERATION_FAILED;
    Exit;
   end;   
 end;
 
 {Create Pins}
 SetLength(GPIO.Pins,GPIO.Properties.PinCount);
 
 {Setup Pins}
 for Count:=0 to GPIO.Properties.PinCount - 1 do
  begin
   GPIO.Pins[Count].GPIO:=GPIO;
   GPIO.Pins[Count].Pin:=Count;
   GPIO.Pins[Count].Flags:=GPIO_EVENT_FLAG_NONE;
   GPIO.Pins[Count].Trigger:=GPIO_TRIGGER_NONE;
   GPIO.Pins[Count].Count:=0;
   GPIO.Pins[Count].Event:=INVALID_HANDLE_VALUE;
   GPIO.Pins[Count].Events:=nil;
  end;
 
 {Return Result}
 Result:=ERROR_SUCCESS;  
end;

{==============================================================================}

function STMPEGPIOStop(GPIO:PGPIODevice):LongWord; 
{Implementation of GPIODeviceStop API for STMPE}
{Note: Not intended to be called directly by applications, use GPIODeviceStop instead}
var
 Count:LongWord;
 Event:PGPIOEvent;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(STMPE_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'STMPE: GPIO Stop');
 {$ENDIF}
 
 {Release Pins}
 for Count:=0 to GPIO.Properties.PinCount - 1 do
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
 
 {Return Result}
 Result:=ERROR_SUCCESS;  
end;

{==============================================================================}
 
function STMPEGPIORead(GPIO:PGPIODevice;Reg:LongWord):LongWord; 
{Implementation of GPIODeviceRead API for STMPE}
{Note: Not intended to be called directly by applications, use GPIODeviceRead instead}
var
 Value:Byte;
begin
 {}
 Result:=0;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(STMPE_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'STMPE: GPIO Read (Reg=' + IntToHex(Reg,8) + ')');
 {$ENDIF}

 {Check Reg}
 if Reg > PSTMPEGPIO(GPIO).Control.RegMax then Exit;
 
 {Read Register}
 if STMPEReadByte(@PSTMPEGPIO(GPIO).Control,Reg,@Value) = ERROR_SUCCESS then
  begin
   Result:=Value;
  end;
end;

{==============================================================================}

procedure STMPEGPIOWrite(GPIO:PGPIODevice;Reg,Value:LongWord);
{Implementation of GPIODeviceWrite API for STMPE}
{Note: Not intended to be called directly by applications, use GPIODeviceWrite instead}
begin
 {}
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(STMPE_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'STMPE: GPIO Write (Reg=' + IntToHex(Reg,8) + ' Value=' + IntToHex(Value,8) + ')');
 {$ENDIF}

 {Check Reg}
 if Reg > PSTMPEGPIO(GPIO).Control.RegMax then Exit;
 
 {Write Register}
 STMPEWriteByte(@PSTMPEGPIO(GPIO).Control,Reg,Value);
end;

{==============================================================================}
 
function STMPEGPIOInputGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
{Implementation of GPIODeviceInputGet API for STMPE}
{Note: Not intended to be called directly by applications, use GPIODeviceInputGet instead}
var
 Reg:Byte;
 Shift:Byte;
 Value:Byte;
begin
 {}
 Result:=GPIO_LEVEL_UNKNOWN;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(STMPE_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'STMPE: GPIO Input Get (Pin=' + GPIOPinToString(Pin) + ')');
 {$ENDIF}
 
 {Check Pin}
 if Pin > GPIO.Properties.PinMax then Exit;
 
 {Update Statistics}
 Inc(GPIO.GetCount);
 
 {Get Shift}
 Shift:=Pin mod 8;
 
 {Check Direction}
 if PSTMPEGPIO(GPIO).Control.RegDir = STMPE_DIR_DESCENDING then
  begin
   {Descending}
   {Get Register}
   Reg:=PSTMPEGPIO(GPIO).Offsets.GPIOPinGet - (Pin div 8);
  end
 else 
  begin
   {Ascending}
   {Get Register}
   Reg:=PSTMPEGPIO(GPIO).Offsets.GPIOPinGet + (Pin div 8);
  end;
 
 {Read Register}
 if STMPEReadByte(@PSTMPEGPIO(GPIO).Control,Reg,@Value) = ERROR_SUCCESS then
  begin
   {$IF DEFINED(STMPE_DEBUG) or DEFINED(GPIO_DEBUG)}
   if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'STMPE:  Reg(PinGet)=' + IntToHex(Reg,2) + ' Shift=' + IntToStr(Shift) + ' Value=' + IntToHex(Value,2));
   {$ENDIF}

   Result:=(Value shr Shift) and 1;
  end;
end;

{==============================================================================}
 
function STMPEGPIOOutputSet(GPIO:PGPIODevice;Pin,Level:LongWord):LongWord;
{Implementation of GPIODeviceOutputSet API for STMPE}
{Note: Not intended to be called directly by applications, use GPIODeviceOutputSet instead}
var
 Reg:Byte;
 Shift:Byte;
 Offset:Byte;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(STMPE_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'STMPE: GPIO Output Set (Pin=' + GPIOPinToString(Pin) + ' Level=' + GPIOLevelToString(Level) + ')');
 {$ENDIF}
 
 {Check Pin}
 if Pin > GPIO.Properties.PinMax then Exit;
 
 {Check Level}
 if Level > STMPE_GPIO_MAX_LEVEL then Exit;
 
 {Update Statistics}
 Inc(GPIO.SetCount);
 
 {Get Shift}
 Shift:=Pin mod 8;
 
 {Check Level}
 if Level = GPIO_LEVEL_HIGH then
  begin
   {Set}
   Offset:=PSTMPEGPIO(GPIO).Offsets.GPIOPinSet;
  end
 else
  begin
   {Clear}
   Offset:=PSTMPEGPIO(GPIO).Offsets.GPIOPinClr;
  end;
 
 {Check Direction}
 if PSTMPEGPIO(GPIO).Control.RegDir = STMPE_DIR_DESCENDING then
  begin
   {Descending}
   {Get Register}
   Reg:=Offset - (Pin div 8);
  end
 else 
  begin
   {Ascending}
   {Get Register}
   Reg:=Offset + (Pin div 8);
  end;
 
 {$IF DEFINED(STMPE_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'STMPE:  Reg(PinSet/PinClr)=' + IntToHex(Reg,2) + ' Shift=' + IntToStr(Shift));
 {$ENDIF}
 
 {Check Chip}
 case PSTMPEGPIO(GPIO).Control.Chip of
  STMPE_CHIP_STMPE801:begin
    {Check Level}
    if Level = GPIO_LEVEL_HIGH then
     begin
      {Set Bits}
      Result:=STMPESetBits(@PSTMPEGPIO(GPIO).Control,Reg,(1 shl Shift),(1 shl Shift));
     end
    else
     begin
      {Set Bits}
      Result:=STMPESetBits(@PSTMPEGPIO(GPIO).Control,Reg,(1 shl Shift),0);
     end;
   end;
  else
   begin
    {Write Register}
    Result:=STMPEWriteByte(@PSTMPEGPIO(GPIO).Control,Reg,(1 shl Shift));
   end;   
 end;
end;

{==============================================================================}

function STMPEGPIOPullGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
{Implementation of GPIODevicePullGet API for STMPE}
{Note: Not intended to be called directly by applications, use GPIODevicePullGet instead}
var
 Reg:Byte;
 Shift:Byte;
 Value:Byte;
begin
 {}
 Result:=GPIO_PULL_UNKNOWN;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(STMPE_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'STMPE: GPIO Pull Get (Pin=' + GPIOPinToString(Pin) + ')');
 {$ENDIF}
 
 {Check Pin}
 if Pin > GPIO.Properties.PinMax then Exit;
 
 {Check Flags}
 if (GPIO.Device.DeviceFlags and (GPIO_FLAG_PULL_UP or GPIO_FLAG_PULL_DOWN)) <> 0 then
  begin
   {Get Shift}
   Shift:=Pin mod 8;
   
   {Check Pull Up}
   if PSTMPEGPIO(GPIO).Offsets.GPIOPullUp <> STMPE_REG_UNKNOWN then
    begin
     {Check Direction}
     if PSTMPEGPIO(GPIO).Control.RegDir = STMPE_DIR_DESCENDING then
      begin
       {Descending}
       {Get Register}
       Reg:=PSTMPEGPIO(GPIO).Offsets.GPIOPullUp - (Pin div 8);
      end
     else 
      begin
       {Ascending}
       Reg:=PSTMPEGPIO(GPIO).Offsets.GPIOPullUp + (Pin div 8);
      end;
      
     {Read Register}
     if STMPEReadByte(@PSTMPEGPIO(GPIO).Control,Reg,@Value) = ERROR_SUCCESS then
      begin
       {$IF DEFINED(STMPE_DEBUG) or DEFINED(GPIO_DEBUG)}
       if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'STMPE:  Reg(PullUp)=' + IntToHex(Reg,2) + ' Shift=' + IntToStr(Shift) + ' Value=' + IntToHex(Value,2));
       {$ENDIF}
       
       if ((Value shr Shift) and 1) = 1 then
        begin
         Result:=GPIO_PULL_UP;
         Exit;
        end;
      end; 
    end;
    
   {Check Pull Down}
   if PSTMPEGPIO(GPIO).Offsets.GPIOPullDown <> STMPE_REG_UNKNOWN then
    begin
     {Check Direction}
     if PSTMPEGPIO(GPIO).Control.RegDir = STMPE_DIR_DESCENDING then
      begin
       {Descending}
       {Get Register}
       Reg:=PSTMPEGPIO(GPIO).Offsets.GPIOPullDown - (Pin div 8);
      end
     else 
      begin
       {Ascending}
       Reg:=PSTMPEGPIO(GPIO).Offsets.GPIOPullDown + (Pin div 8);
      end;
      
     {Read Register}
     if STMPEReadByte(@PSTMPEGPIO(GPIO).Control,Reg,@Value) = ERROR_SUCCESS then
      begin
       {$IF DEFINED(STMPE_DEBUG) or DEFINED(GPIO_DEBUG)}
       if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'STMPE:  Reg(PullDown)=' + IntToHex(Reg,2) + ' Shift=' + IntToStr(Shift) + ' Value=' + IntToHex(Value,2));
       {$ENDIF}
       
       if ((Value shr Shift) and 1) = 1 then
        begin
         Result:=GPIO_PULL_DOWN;
         Exit;
        end;
      end; 
    end;
   
   Result:=GPIO_PULL_NONE;
  end
 else
  begin
   Result:=GPIO_PULL_NONE;
  end;
end;

{==============================================================================}

function STMPEGPIOPullSelect(GPIO:PGPIODevice;Pin,Mode:LongWord):LongWord;
{Implementation of GPIODevicePullSelect API for STMPE}
{Note: Not intended to be called directly by applications, use GPIODevicePullSelect instead}
var
 Reg:Byte;
 Shift:Byte;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(STMPE_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'STMPE: GPIO Pull Select (Pin=' + GPIOPinToString(Pin) + ' Mode=' + GPIOPullToString(Mode) + ')');
 {$ENDIF}
 
 {Check Pin}
 if Pin > GPIO.Properties.PinMax then Exit;
 
 {Check Mode}
 if Mode > GPIO_PULL_DOWN then Exit;
 if (Mode = GPIO_PULL_UP) and ((GPIO.Device.DeviceFlags and GPIO_FLAG_PULL_UP) = 0) then Exit;
 if (Mode = GPIO_PULL_DOWN) and ((GPIO.Device.DeviceFlags and GPIO_FLAG_PULL_DOWN) = 0) then Exit;
 
 {Check Flags}
 if (GPIO.Device.DeviceFlags and (GPIO_FLAG_PULL_UP or GPIO_FLAG_PULL_DOWN)) <> 0 then
  begin
   {Get Shift}
   Shift:=Pin mod 8;
   
   {Check Mode}
   if (Mode = GPIO_PULL_NONE) or (Mode = GPIO_PULL_UP) then
    begin
     {Check Pull Down}
     if PSTMPEGPIO(GPIO).Offsets.GPIOPullDown <> STMPE_REG_UNKNOWN then
      begin
       {Check Direction}
       if PSTMPEGPIO(GPIO).Control.RegDir = STMPE_DIR_DESCENDING then
        begin
         {Descending}
         {Get Register}
         Reg:=PSTMPEGPIO(GPIO).Offsets.GPIOPullDown - (Pin div 8);
        end
       else 
        begin
         {Ascending}
         Reg:=PSTMPEGPIO(GPIO).Offsets.GPIOPullDown + (Pin div 8);
        end;
        
       {Set Bits (Disable Pull Down)}
       if STMPESetBits(@PSTMPEGPIO(GPIO).Control,Reg,(1 shl Shift),0) <> ERROR_SUCCESS then
        begin
         Result:=ERROR_OPERATION_FAILED;
         Exit;
        end; 
      end;
    end;
    
   {Check Mode}
   if (Mode = GPIO_PULL_NONE) or (Mode = GPIO_PULL_DOWN) then
    begin
     {Check Pull Up}
     if PSTMPEGPIO(GPIO).Offsets.GPIOPullUp <> STMPE_REG_UNKNOWN then
      begin
       {Check Direction}
       if PSTMPEGPIO(GPIO).Control.RegDir = STMPE_DIR_DESCENDING then
        begin
         {Descending}
         {Get Register}
         Reg:=PSTMPEGPIO(GPIO).Offsets.GPIOPullUp - (Pin div 8);
        end
       else 
        begin
         {Ascending}
         Reg:=PSTMPEGPIO(GPIO).Offsets.GPIOPullUp + (Pin div 8);
        end;
       
       {Set Bits (Disable Pull Up)}
       if STMPESetBits(@PSTMPEGPIO(GPIO).Control,Reg,(1 shl Shift),0) <> ERROR_SUCCESS  then
        begin
         Result:=ERROR_OPERATION_FAILED;
         Exit;
        end; 
      end;
    end;
    
   {Check Mode}
   case Mode of
    GPIO_PULL_NONE:begin
      Result:=ERROR_SUCCESS;
     end;
    GPIO_PULL_UP:begin
      {Check Pull Up}
      if PSTMPEGPIO(GPIO).Offsets.GPIOPullUp = STMPE_REG_UNKNOWN then Exit;
      
      {Check Direction}
      if PSTMPEGPIO(GPIO).Control.RegDir = STMPE_DIR_DESCENDING then
       begin
        {Descending}
        {Get Register}
        Reg:=PSTMPEGPIO(GPIO).Offsets.GPIOPullUp - (Pin div 8);
       end
      else 
       begin
        {Ascending}
        Reg:=PSTMPEGPIO(GPIO).Offsets.GPIOPullUp + (Pin div 8);
       end;
      
      {Set Bits (Enable Pull Up}
      Result:=STMPESetBits(@PSTMPEGPIO(GPIO).Control,Reg,(1 shl Shift),(1 shl Shift));
     end;
    GPIO_PULL_DOWN:begin
      {Check Pull Down}
      if PSTMPEGPIO(GPIO).Offsets.GPIOPullDown = STMPE_REG_UNKNOWN then Exit;
      
      {Check Direction}
      if PSTMPEGPIO(GPIO).Control.RegDir = STMPE_DIR_DESCENDING then
       begin
        {Descending}
        {Get Register}
        Reg:=PSTMPEGPIO(GPIO).Offsets.GPIOPullDown - (Pin div 8);
       end
      else 
       begin
        {Ascending}
        Reg:=PSTMPEGPIO(GPIO).Offsets.GPIOPullDown + (Pin div 8);
       end;
      
      {Set Bits (Enable Pull Down)}
      Result:=STMPESetBits(@PSTMPEGPIO(GPIO).Control,Reg,(1 shl Shift),(1 shl Shift));
     end;
   end; 
  end
 else
  begin
   Result:=ERROR_SUCCESS;
  end;
end;

{==============================================================================}

function STMPEGPIOFunctionGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
{Implementation of GPIODeviceFunctionGet API for STMPE}
{Note: Not intended to be called directly by applications, use GPIODeviceFunctionGet instead}
var
 Reg:Byte;
 Shift:Byte;
 Value:Byte;
begin
 {}
 Result:=GPIO_FUNCTION_UNKNOWN;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(STMPE_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'STMPE: GPIO Function Get (Pin=' + GPIOPinToString(Pin) + ')');
 {$ENDIF}
 
 {Check Pin}
 if Pin > GPIO.Properties.PinMax then Exit;
 
 {Check Function}
 if PSTMPEGPIO(GPIO).Offsets.GPIOFuncSet <> STMPE_REG_UNKNOWN then
  begin
   {Check Chip}
   case PSTMPEGPIO(GPIO).Control.Chip of
    STMPE_CHIP_STMPE610,STMPE_CHIP_STMPE801,STMPE_CHIP_STMPE811:begin
      {1 bit per pin}
      {Get Shift}
      Shift:=Pin mod 8;
      
      {Check Direction}
      if PSTMPEGPIO(GPIO).Control.RegDir = STMPE_DIR_DESCENDING then
       begin
        {Descending}
        {Get Register}
        Reg:=PSTMPEGPIO(GPIO).Offsets.GPIOFuncSet - (Pin div 8);
       end
      else 
       begin
        {Ascending}
        Reg:=PSTMPEGPIO(GPIO).Offsets.GPIOFuncSet + (Pin div 8);
       end;
      
      {Read Register}
      if STMPEReadByte(@PSTMPEGPIO(GPIO).Control,Reg,@Value) = ERROR_SUCCESS then
       begin
        {$IF DEFINED(STMPE_DEBUG) or DEFINED(GPIO_DEBUG)}
        if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'STMPE:  Reg(FuncSet)=' + IntToHex(Reg,2) + ' Shift=' + IntToStr(Shift) + ' Value=' + IntToHex(Value,2));
        {$ENDIF}
        
        {Check Value}
        if ((Value shr Shift) and 1) = 0 then
         begin
          Result:=GPIO_FUNCTION_ALT0;
          Exit;
         end;
       end;
     end;
    STMPE_CHIP_STMPE1601,STMPE_CHIP_STMPE1801,STMPE_CHIP_STMPE2401,STMPE_CHIP_STMPE2403:begin
      {2 bits per pin}
      {Get Shift}
      Shift:=(Pin mod 4) * 2;

      {Check Direction}
      if PSTMPEGPIO(GPIO).Control.RegDir = STMPE_DIR_DESCENDING then
       begin
        {Descending}
        {Get Register}
        Reg:=PSTMPEGPIO(GPIO).Offsets.GPIOFuncSet - (Pin div 4);
       end
      else 
       begin
        {Ascending}
        Reg:=PSTMPEGPIO(GPIO).Offsets.GPIOFuncSet + (Pin div 4);
       end;
       
      {Read Register}
      if STMPEReadByte(@PSTMPEGPIO(GPIO).Control,Reg,@Value) = ERROR_SUCCESS then
       begin
        {$IF DEFINED(STMPE_DEBUG) or DEFINED(GPIO_DEBUG)}
        if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'STMPE:  Reg(FuncSet)=' + IntToHex(Reg,2) + ' Shift=' + IntToStr(Shift) + ' Value=' + IntToHex(Value,2));
        {$ENDIF}
        
        {Check Value}
        Value:=((Value shr Shift) and 3);
        if Value <> 0 then
         begin
          if Value = 1 then
           begin
            Result:=GPIO_FUNCTION_ALT0;
           end
          else if Value = 2 then
           begin
            Result:=GPIO_FUNCTION_ALT1;
           end
          else if Value = 3 then
           begin
            Result:=GPIO_FUNCTION_ALT2;
           end;
          Exit;
         end;
       end;
     end;
   end;  
  end;
 
 {Get Shift}
 Shift:=Pin mod 8;
 
 {Check Direction}
 if PSTMPEGPIO(GPIO).Control.RegDir = STMPE_DIR_DESCENDING then
  begin
   {Descending}
   {Get Register}
   Reg:=PSTMPEGPIO(GPIO).Offsets.GPIODirSet - (Pin div 8);
  end
 else 
  begin
   {Ascending}
   Reg:=PSTMPEGPIO(GPIO).Offsets.GPIODirSet + (Pin div 8);
  end;

 {Read Register}
 if STMPEReadByte(@PSTMPEGPIO(GPIO).Control,Reg,@Value) = ERROR_SUCCESS then
  begin
   {$IF DEFINED(STMPE_DEBUG) or DEFINED(GPIO_DEBUG)}
   if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'STMPE:  Reg(DirSet)=' + IntToHex(Reg,2) + ' Shift=' + IntToStr(Shift) + ' Value=' + IntToHex(Value,2));
   {$ENDIF}
 
   if ((Value shr Shift) and 1) = 1 then
    begin
     Result:=GPIO_FUNCTION_OUT;
    end
   else
    begin
     Result:=GPIO_FUNCTION_IN;
    end;    
  end; 
end;

{==============================================================================}

function STMPEGPIOFunctionSelect(GPIO:PGPIODevice;Pin,Mode:LongWord):LongWord;
{Implementation of GPIODeviceFunctionSelect API for STMPE}
{Note: Not intended to be called directly by applications, use GPIODeviceFunctionSelect instead}
var
 Reg:Byte;
 Shift:Byte;
 Value:Byte;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(STMPE_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'STMPE: GPIO Function Select (Pin=' + GPIOPinToString(Pin) + ' Mode=' + GPIOFunctionToString(Mode) + ')');
 {$ENDIF}
 
 {Check Pin}
 if Pin > GPIO.Properties.PinMax then Exit;
 
 {Check Mode}
 if Mode > GPIO.Properties.FunctionMax then Exit;
 
 {Check Mode}
 case Mode of
  GPIO_FUNCTION_IN,GPIO_FUNCTION_OUT:begin
    {Check Function}
    if PSTMPEGPIO(GPIO).Offsets.GPIOFuncSet <> STMPE_REG_UNKNOWN then
     begin
      {Check Chip}
      case PSTMPEGPIO(GPIO).Control.Chip of
       STMPE_CHIP_STMPE610,STMPE_CHIP_STMPE801,STMPE_CHIP_STMPE811:begin
         {1 bit per pin}
         {Get Shift}
         Shift:=Pin mod 8;
         
         {Check Direction}
         if PSTMPEGPIO(GPIO).Control.RegDir = STMPE_DIR_DESCENDING then
          begin
           {Descending}
           {Get Register}
           Reg:=PSTMPEGPIO(GPIO).Offsets.GPIOFuncSet - (Pin div 8);
          end
         else 
          begin
           {Ascending}
           Reg:=PSTMPEGPIO(GPIO).Offsets.GPIOFuncSet + (Pin div 8);
          end;
         
         {Set Bits}
         if STMPESetBits(@PSTMPEGPIO(GPIO).Control,Reg,(1 shl Shift),(1 shl Shift)) <> ERROR_SUCCESS then 
          begin
           Result:=ERROR_OPERATION_FAILED;
           Exit;
          end;
        end;
       STMPE_CHIP_STMPE1601,STMPE_CHIP_STMPE1801,STMPE_CHIP_STMPE2401,STMPE_CHIP_STMPE2403:begin
         {2 bits per pin}
         {Get Shift}
         Shift:=(Pin mod 4) * 2;
         
         {Check Direction}
         if PSTMPEGPIO(GPIO).Control.RegDir = STMPE_DIR_DESCENDING then
          begin
           {Descending}
           {Get Register}
           Reg:=PSTMPEGPIO(GPIO).Offsets.GPIOFuncSet - (Pin div 4);
          end
         else 
          begin
           {Ascending}
           Reg:=PSTMPEGPIO(GPIO).Offsets.GPIOFuncSet + (Pin div 4);
          end;
          
         {Set Bits} 
         if STMPESetBits(@PSTMPEGPIO(GPIO).Control,Reg,(3 shl Shift),0) <> ERROR_SUCCESS then 
          begin
           Result:=ERROR_OPERATION_FAILED;
           Exit;
          end;
        end;
      end;
     end;  
  
    {Get Shift}
    Shift:=Pin mod 8;
  
    {Check Direction}
    if PSTMPEGPIO(GPIO).Control.RegDir = STMPE_DIR_DESCENDING then
     begin
      {Descending}
      {Get Register}
      Reg:=PSTMPEGPIO(GPIO).Offsets.GPIODirSet - (Pin div 8);
     end
    else 
     begin
      {Ascending}
      Reg:=PSTMPEGPIO(GPIO).Offsets.GPIODirSet + (Pin div 8);
     end;
    
    {Check Mode}
    if Mode = GPIO_FUNCTION_OUT then
     begin
      {Set Bits}
      Result:=STMPESetBits(@PSTMPEGPIO(GPIO).Control,Reg,(1 shl Shift),(1 shl Shift));
     end
    else
     begin
      {Set Bits}
      Result:=STMPESetBits(@PSTMPEGPIO(GPIO).Control,Reg,(1 shl Shift),0);
     end;
   end;
  else
   begin
    {Check Function}
    if PSTMPEGPIO(GPIO).Offsets.GPIOFuncSet = STMPE_REG_UNKNOWN then Exit;
   
    {Check Chip}
    case PSTMPEGPIO(GPIO).Control.Chip of
     STMPE_CHIP_STMPE610,STMPE_CHIP_STMPE801,STMPE_CHIP_STMPE811:begin
       {1 bit per pin}
       {Get Shift}
       Shift:=Pin mod 8;
       
       {Check Direction}
       if PSTMPEGPIO(GPIO).Control.RegDir = STMPE_DIR_DESCENDING then
        begin
         {Descending}
         {Get Register}
         Reg:=PSTMPEGPIO(GPIO).Offsets.GPIOFuncSet - (Pin div 8);
        end
       else 
        begin
         {Ascending}
         Reg:=PSTMPEGPIO(GPIO).Offsets.GPIOFuncSet + (Pin div 8);
        end;
       
       {Set Bits}
       Result:=STMPESetBits(@PSTMPEGPIO(GPIO).Control,Reg,(1 shl Shift),0);
      end;
     STMPE_CHIP_STMPE1601,STMPE_CHIP_STMPE1801,STMPE_CHIP_STMPE2401,STMPE_CHIP_STMPE2403:begin
       {2 bits per pin}
       {Get Shift}
       Shift:=(Pin mod 4) * 2;
       
       {Check Direction}
       if PSTMPEGPIO(GPIO).Control.RegDir = STMPE_DIR_DESCENDING then
        begin
         {Descending}
         {Get Register}
         Reg:=PSTMPEGPIO(GPIO).Offsets.GPIOFuncSet - (Pin div 4);
        end
       else 
        begin
         {Ascending}
         Reg:=PSTMPEGPIO(GPIO).Offsets.GPIOFuncSet + (Pin div 4);
        end;
        
       {Get Value}
       Value:=Mode - GPIO_FUNCTION_OUT;
       
       {Set Bits}       
       Result:=STMPESetBits(@PSTMPEGPIO(GPIO).Control,Reg,(3 shl Shift),(Value shl Shift));
      end;
    end;
   end;   
 end;
end;

{==============================================================================}
{==============================================================================}
{STMPE Touch Functions}
function STMPETouchStart(Touch:PTouchDevice):LongWord;
{Implementation of TouchDeviceStart API for STMPE}
{Note: Not intended to be called directly by applications, use TouchDeviceStart instead}

 function STMPE_GET_SAMPLE_TIME(Value:Byte):Byte; inline;
 begin
  {}
  Result:=(Value and $F) shl 4;
 end;
 
 function STMPE_GET_MOD_12B(Value:Byte):Byte; inline;
 begin
  {}
  Result:=(Value and 1) shl 3;
 end;

 function STMPE_GET_REF_SEL(Value:Byte):Byte; inline;
 begin
  {}
  Result:=(Value and 1) shl 1;
 end;

 function STMPE_GET_ADC_FREQ(Value:Byte):Byte; inline;
 begin
  {}
  Result:=(Value and 3);
 end;

 function STMPE_GET_AVE_CTRL(Value:Byte):Byte; inline;
 begin
  {}
  Result:=(Value and 3) shl 6;
 end;

 function STMPE_GET_TOUCH_DET_DELAY(Value:Byte):Byte; inline;
 begin
  {}
  Result:=(Value and 7) shl 3;
 end;
 
 function STMPE_GET_SETTLING(Value:Byte):Byte; inline;
 begin
  {}
  Result:=(Value and 7);
 end;
 
 function STMPE_GET_FRACTION_Z(Value:Byte):Byte; inline;
 begin
  {}
  Result:=(Value and 7);
 end;
 
 function STMPE_GET_I_DRIVE(Value:Byte):Byte; inline;
 begin
  {}
  Result:=(Value and 1);
 end;

 function STMPE_GET_OP_MODE(Value:Byte):Byte; inline;
 begin
  {}
  Result:=(Value and 7) shl 1;
 end;
 
var
 ChipID:Word;
 ADCCtrl1:Byte;
 ADCCtrl1Mask:Byte;
 TSCCfg:Byte;
 TSCCfgMask:Byte;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Touch}
 if Touch = nil then Exit;
 
 {$IF DEFINED(STMPE_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(Touch,'STMPE: Touch Start');
 {$ENDIF}
 
 {Check I2C/SPI}
 if PSTMPETouch(Touch).Control.I2C <> nil then 
  begin
   {Start I2C Device}
   if I2CDeviceStart(PSTMPETouch(Touch).Control.I2C,STMPE_I2C_RATE) <> ERROR_SUCCESS then
    begin
     Result:=ERROR_OPERATION_FAILED;
     Exit;
    end;
  end
 else if PSTMPETouch(Touch).Control.SPI <> nil then
  begin
   {Set the SPI clock rate}
   if SPIDeviceSetClockRate(PSTMPETouch(Touch).Control.SPI,PSTMPETouch(Touch).Control.ChipSelect,STMPE_SPI_RATE) <> ERROR_SUCCESS then
    begin
     Result:=ERROR_OPERATION_FAILED;
     Exit;
    end;

    {Start SPI Device}
   if SPIDeviceStart(PSTMPETouch(Touch).Control.SPI,SPI_MODE_4WIRE,STMPE_SPI_RATE,SPI_CLOCK_PHASE_LOW,SPI_CLOCK_POLARITY_LOW) <> ERROR_SUCCESS then
    begin
     Result:=ERROR_OPERATION_FAILED;
     Exit;
    end;
  end
 else
  begin
   Exit;
  end;  
 
 {Check Chip}
 case PSTMPETouch(Touch).Control.Chip of
  STMPE_CHIP_STMPE610,STMPE_CHIP_STMPE811:begin
    {Get ID}
    if STMPEReadWord(@PSTMPETouch(Touch).Control,STMPE811_REG_CHIP_ID,@ChipID) <> ERROR_SUCCESS then
     begin
      Result:=ERROR_OPERATION_FAILED;
      Exit;
     end;
    {$IF DEFINED(STMPE_DEBUG) or DEFINED(TOUCH_DEBUG)}
    if TOUCH_LOG_ENABLED then TouchLogDebug(Touch,'STMPE:  STMPE811_REG_CHIP_ID=' + IntToHex(ChipID,4));
    {$ENDIF}
    
    {Check ID}
    if ChipID <> STMPE811_CHIP_ID then
     begin
      Result:=ERROR_OPERATION_FAILED;
      Exit;
     end;
     
    {Setup Registers}
    PSTMPETouch(Touch).Offsets:=STMPE811Offsets;
    
    {Enable Touch and ADC}
    if STMPESetBits(@PSTMPETouch(Touch).Control,PSTMPETouch(Touch).Offsets.SysCtrl,STMPE811_SYS_CTRL2_ADC_OFF or STMPE811_SYS_CTRL2_TSC_OFF,0) <> ERROR_SUCCESS then
     begin
      Result:=ERROR_OPERATION_FAILED;
      Exit;
     end;
   end;
  else
   begin
    Result:=ERROR_OPERATION_FAILED;
    Exit;
   end;
 end;
 
 {Initialize Hardware}
 {ADC Control 1}
 ADCCtrl1:=STMPE_GET_SAMPLE_TIME(STMPE_SAMPLE_TIME) or STMPE_GET_MOD_12B(STMPE_MOD_12B) or STMPE_GET_REF_SEL(STMPE_REF_SEL);
 ADCCtrl1Mask:=STMPE_GET_SAMPLE_TIME($FF) or STMPE_GET_MOD_12B($FF) or STMPE_GET_REF_SEL($FF);
 if STMPESetBits(@PSTMPETouch(Touch).Control,PSTMPETouch(Touch).Offsets.ADCCtrl1,ADCCtrl1Mask,ADCCtrl1) <> ERROR_SUCCESS then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
  
 {ADC Control 2}
 if STMPESetBits(@PSTMPETouch(Touch).Control,PSTMPETouch(Touch).Offsets.ADCCtrl2,STMPE_GET_ADC_FREQ($FF),STMPE_GET_ADC_FREQ(STMPE_ADC_FREQ)) <> ERROR_SUCCESS then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
 
 {TSC Config}
 TSCCfg:=STMPE_GET_AVE_CTRL(STMPE_AVE_CTRL) or STMPE_GET_TOUCH_DET_DELAY(STMPE_TOUCH_DET_DELAY) or STMPE_GET_SETTLING(STMPE_SETTLING);
 TSCCfgMask:=STMPE_GET_AVE_CTRL($FF) or STMPE_GET_TOUCH_DET_DELAY($FF) or STMPE_GET_SETTLING($FF);
 if STMPESetBits(@PSTMPETouch(Touch).Control,PSTMPETouch(Touch).Offsets.TSCCfg,TSCCfgMask,TSCCfg) <> ERROR_SUCCESS then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
 
 {TSC Fraction Z}
 if STMPESetBits(@PSTMPETouch(Touch).Control,PSTMPETouch(Touch).Offsets.TSCFractionZ,STMPE_GET_FRACTION_Z($FF),STMPE_GET_FRACTION_Z(STMPE_FRACTION_Z)) <> ERROR_SUCCESS then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
 
 {TSC I Drive}
 if STMPESetBits(@PSTMPETouch(Touch).Control,PSTMPETouch(Touch).Offsets.TSCIDrive,STMPE_GET_I_DRIVE($FF),STMPE_GET_I_DRIVE(STMPE_I_DRIVE)) <> ERROR_SUCCESS then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {TSC FIFO Threshold (1 only)}
 if STMPEWriteByte(@PSTMPETouch(Touch).Control,PSTMPETouch(Touch).Offsets.FIFOThreshold,1) <> ERROR_SUCCESS then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
 
 {TSC OP Mode}
 if STMPESetBits(@PSTMPETouch(Touch).Control,PSTMPETouch(Touch).Offsets.TSCCtrl,STMPE_GET_OP_MODE($FF),STMPE811_TSC_CTRL_OPMODE_XYZ) <> ERROR_SUCCESS then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
 
 {Initialize IRQ}
 if (PSTMPETouch(Touch).Control.IRQ.GPIO <> nil) and (PSTMPETouch(Touch).Control.IRQ.Pin <> GPIO_PIN_UNKNOWN) then
  begin
   {Setup IRQ (GPIO)}
   if GPIODeviceFunctionSelect(PSTMPETouch(Touch).Control.IRQ.GPIO,PSTMPETouch(Touch).Control.IRQ.Pin,PSTMPETouch(Touch).Control.IRQ.Func) <> ERROR_SUCCESS then
    begin
     Result:=ERROR_OPERATION_FAILED;
     Exit;
    end;
   if GPIODevicePullSelect(PSTMPETouch(Touch).Control.IRQ.GPIO,PSTMPETouch(Touch).Control.IRQ.Pin,PSTMPETouch(Touch).Control.IRQ.Pull) <> ERROR_SUCCESS then
    begin
     Result:=ERROR_OPERATION_FAILED;
     Exit;
    end;
   
   {Enable Interrupt (Global/Edge)}
   if STMPESetBits(@PSTMPETouch(Touch).Control,PSTMPETouch(Touch).Offsets.IntCtrl,STMPE811_INT_CTRL_GLOBAL or STMPE811_INT_CTRL_TYPE,STMPE811_INT_CTRL_GLOBAL or STMPE811_INT_CTRL_TYPE) <> ERROR_SUCCESS then
    begin
     Result:=ERROR_OPERATION_FAILED;
     Exit;
    end;
   
   {Create GPIO Event}
   if GPIODeviceInputEvent(PSTMPETouch(Touch).Control.IRQ.GPIO,PSTMPETouch(Touch).Control.IRQ.Pin,PSTMPETouch(Touch).Control.IRQ.Trigger,GPIO_EVENT_FLAG_NONE,INFINITE,TGPIOCallback(STMPETouchCallback),Touch) <> ERROR_SUCCESS then
    begin
     Result:=ERROR_OPERATION_FAILED;
     Exit;
    end;
   
   {Create Timer}
   if (Touch.Device.DeviceFlags and TOUCH_FLAG_RELEASE_TIMER) <> 0 then
    begin
     PSTMPETouch(Touch).Timer:=TimerCreateEx(50,TIMER_STATE_DISABLED,TIMER_FLAG_WORKER,TTimerEvent(STMPETouchTimer),Touch); {Scheduled by GPIO Event Callback}
    end; 
   
   {Enable Interrupt (FIFO Threshold)}
   if STMPESetBits(@PSTMPETouch(Touch).Control,PSTMPETouch(Touch).Offsets.IntEnable,STMPE811_INT_EN_FIFO_TH,STMPE811_INT_EN_FIFO_TH) <> ERROR_SUCCESS then
    begin
     Result:=ERROR_OPERATION_FAILED;
     Exit;
    end;
   
   {Clear Interrupt (Touch Detect)}
   if STMPEWriteByte(@PSTMPETouch(Touch).Control,PSTMPETouch(Touch).Offsets.IntStatus,STMPE811_INT_STA_TOUCH_MASK) <> ERROR_SUCCESS then
    begin
     Result:=ERROR_OPERATION_FAILED;
     Exit;
    end;
  end; 
 
 {Reset FIFO}
 if STMPEResetFIFO(@PSTMPETouch(Touch).Control,PSTMPETouch(Touch).Offsets.FIFOStatus) <> ERROR_SUCCESS then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
 
 {Update Configuration}
 if STMPETouchUpdateConfig(PSTMPETouch(Touch)) <> ERROR_SUCCESS then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
 
 {Enable Touchscreen}
 if STMPESetBits(@PSTMPETouch(Touch).Control,PSTMPETouch(Touch).Offsets.TSCCtrl,STMPE811_TSC_CTRL_TSC_EN,STMPE811_TSC_CTRL_TSC_EN) <> ERROR_SUCCESS then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
 
 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function STMPETouchStop(Touch:PTouchDevice):LongWord;
{Implementation of TouchDeviceStop API for STMPE}
{Note: Not intended to be called directly by applications, use TouchDeviceStop instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Touch}
 if Touch = nil then Exit;
 
 {$IF DEFINED(STMPE_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(Touch,'STMPE: Touch Stop');
 {$ENDIF}
 
 {Disable Touchscreen}
 if STMPESetBits(@PSTMPETouch(Touch).Control,PSTMPETouch(Touch).Offsets.TSCCtrl,STMPE811_TSC_CTRL_TSC_EN,0) <> ERROR_SUCCESS then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
 
 {Disable Interrupt}
 if STMPESetBits(@PSTMPETouch(Touch).Control,PSTMPETouch(Touch).Offsets.IntEnable,STMPE811_INT_EN_FIFO_TH,0) <> ERROR_SUCCESS then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
  
 {Cancel Event}
 GPIODeviceInputCancel(PSTMPETouch(Touch).Control.IRQ.GPIO,PSTMPETouch(Touch).Control.IRQ.Pin);
 
 {Cancel Timer} 
 if (Touch.Device.DeviceFlags and TOUCH_FLAG_RELEASE_TIMER) <> 0 then
  begin
   TimerDestroy(PSTMPETouch(Touch).Timer);
   PSTMPETouch(Touch).Timer:=INVALID_HANDLE_VALUE;
  end; 
  
 {Check Chip}
 case PSTMPETouch(Touch).Control.Chip of
  STMPE_CHIP_STMPE610,STMPE_CHIP_STMPE811:begin
    {Disable Touch and ADC}
    if STMPESetBits(@PSTMPETouch(Touch).Control,PSTMPETouch(Touch).Offsets.SysCtrl,STMPE811_SYS_CTRL2_ADC_OFF or STMPE811_SYS_CTRL2_TSC_OFF,STMPE811_SYS_CTRL2_ADC_OFF or STMPE811_SYS_CTRL2_TSC_OFF) <> ERROR_SUCCESS then
     begin
      Result:=ERROR_OPERATION_FAILED;
      Exit;
     end;
   end;
 end;
 
 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function STMPETouchUpdate(Touch:PTouchDevice):LongWord;
{Implementation of TouchDeviceUpdate API for STMPE}
{Note: Not intended to be called directly by applications, use TouchDeviceUpdate instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(STMPE_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(Touch,'STMPE: Touch Update');
 {$ENDIF}

 {Acquire Lock}
 if MutexLock(Touch.Lock) = ERROR_SUCCESS then
  begin
   try
    {Update Configuration}
    Result:=STMPETouchUpdateConfig(PSTMPETouch(Touch));
   finally
    {Release the Lock}
    MutexUnlock(Touch.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
   Exit;
  end;
end;

{==============================================================================}

procedure STMPETouchTimer(Touch:PSTMPETouch);
{Touch device timer event handler for STMPE}
{Note: Not intended to be called directly by applications}
var
 Status:Byte;
 TouchData:PTouchData;
 MouseData:TMouseData;
begin
 {}
 {Check Touch}
 if Touch = nil then Exit;
 
 {Acquire Lock}
 if MutexLock(Touch.Touch.Lock) = ERROR_SUCCESS then
  begin
   {$IF DEFINED(STMPE_DEBUG) or DEFINED(TOUCH_DEBUG)}
   if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'STMPE: Touch Timer');
   {$ENDIF}

   {Clear Mouse Data}
   FillChar(MouseData,SizeOf(TMouseData),0);

   {Read TSC Control}
   STMPEReadByte(@Touch.Control,Touch.Offsets.TSCCtrl,@Status);
   
   {$IF DEFINED(STMPE_DEBUG) or DEFINED(TOUCH_DEBUG)}
   if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'STMPE:  TSC Control=' + IntToHex(Status,2));
   {$ENDIF}
   
   {Check TSC Status}
   if (Status and STMPE811_TSC_CTRL_TSC_STA) = 0 then
    begin
     {Check Flags}
     if (Touch.Touch.Device.DeviceFlags and TOUCH_FLAG_MOUSE_DATA) = 0 then
      begin
       {Check Buffer}
       if (Touch.Touch.Buffer.Count < TOUCH_BUFFER_SIZE) then
        begin
         TouchData:=@Touch.Touch.Buffer.Buffer[(Touch.Touch.Buffer.Start + Touch.Touch.Buffer.Count) mod TOUCH_BUFFER_SIZE];
         if TouchData <> nil then
          begin
           {Clear Touch Data}
           FillChar(TouchData^,SizeOf(TTouchData),0);

           {Update Touch Data}
           TouchData.Info:=0;
           TouchData.PointID:=1;
           TouchData.PositionX:=TOUCH_X_UNKNOWN;
           TouchData.PositionY:=TOUCH_Y_UNKNOWN;
           TouchData.PositionZ:=TOUCH_Z_UNKNOWN;
       
           {Check Event}
           if Assigned(Touch.Touch.Event) then
            begin
             {Event Parameter}
             TouchData.Parameter:=Touch.Touch.Parameter;

             {Event Callback}
             Touch.Touch.Event(@Touch.Touch,TouchData);
            end
           else
            begin
             {Update Count}
             Inc(Touch.Touch.Buffer.Count);

             {Signal Data Received}
             SemaphoreSignal(Touch.Touch.Buffer.Wait);
            end;
          end;
        end
       else
        begin
         if TOUCH_LOG_ENABLED then TouchLogError(@Touch.Touch,'STMPE: Buffer overflow, packet discarded'); 
           
         {Update Statistics}
         Inc(Touch.Touch.BufferOverruns); 
        end;                           
      end
     else
      begin
       {Create Mouse Data (Release Event)}
       MouseData.Buttons:=0; {No Buttons}
       MouseData.OffsetX:=0; {No Offset X, Y or Wheel}
       MouseData.OffsetY:=0;
       MouseData.OffsetWheel:=0;
       
       {Maximum X, Y and Wheel}
       MouseData.MaximumX:=Touch.Touch.Properties.MaxX;
       MouseData.MaximumY:=Touch.Touch.Properties.MaxY;
       MouseData.MaximumWheel:=0;
       
       {Write Mouse Data}
       if MouseWrite(@MouseData,SizeOf(TMouseData),1) <> ERROR_SUCCESS then
        begin
         if TOUCH_LOG_ENABLED then TouchLogError(@Touch.Touch,'STMPE: Failed to write mouse data, packet discarded'); 
         
         {Update Statistics}
         Inc(Touch.Touch.ReceiveErrors); 
        end;
      end;
    end;  
    
   {Release Lock}
   MutexUnlock(Touch.Touch.Lock);
  end; 
end;

{==============================================================================}

procedure STMPETouchCallback(Touch:PSTMPETouch;Pin,Trigger:LongWord);
{Touch device event callback (Interrupt) handler for STMPE}
{Note: Not intended to be called directly by applications}
var
 X:Word;
 Y:Word;
 Z:Word;
 Temp:Word;
 Count:Byte;
 TouchData:PTouchData;
 MouseData:TMouseData;
 Values:array[0..3] of Byte;
begin
 {}
 {Check Touch}
 if Touch = nil then Exit;
 
 {Acquire Lock}
 if MutexLock(Touch.Touch.Lock) = ERROR_SUCCESS then
  begin
   {$IF DEFINED(STMPE_DEBUG) or DEFINED(TOUCH_DEBUG)}
   if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'STMPE: Touch Callback');
   {$ENDIF}
   
   {Update Statistics}
   Inc(Touch.Touch.ReceiveCount); 
   
   {Disable Timer}
   if (Touch.Touch.Device.DeviceFlags and TOUCH_FLAG_RELEASE_TIMER) <> 0 then
    begin
     TimerDisable(Touch.Timer);
    end; 
   
   {Disable Interrupt}
   STMPESetBits(@Touch.Control,Touch.Offsets.IntEnable,STMPE811_INT_EN_FIFO_TH,0);

   {Clear Mouse Data}
   FillChar(MouseData,SizeOf(TMouseData),0);
   
   {Read FIFO Size}
   STMPEReadByte(@Touch.Control,STMPE811_REG_FIFO_SIZE,@Count);
   
   {$IF DEFINED(STMPE_DEBUG) or DEFINED(TOUCH_DEBUG)}
   if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'STMPE:  FIFO Size=' + IntToStr(Count));
   {$ENDIF}
   
   while Count > 0 do
    begin
     {Read XYZ Data}
     STMPEReadBytes(@Touch.Control,Touch.Offsets.TSCDataXYZ,SizeOf(Values),@Values);
     
     {Get X, Y and Z}
     X:=(Values[0] shl 4) or ((Values[1] and $F0) shr 4);
     Y:=((Values[1] and $0F) shl 8) or Values[2];
     Z:=Values[3];

     {Check Swap}
     if (Touch.Touch.Device.DeviceFlags and TOUCH_FLAG_SWAP_XY) <> 0 then
      begin
       {Swap X/Y}
       Temp:=X;
       X:=Y;
       Y:=Temp;
      end;
  
     {Check Invert}
     if (Touch.Touch.Device.DeviceFlags and TOUCH_FLAG_INVERT_X) <> 0 then
      begin
       {Invert X}
       X:=Touch.MaxX - X;
      end;
     if (Touch.Touch.Device.DeviceFlags and TOUCH_FLAG_INVERT_Y) <> 0 then
      begin
       {Invert Y}
       Y:=Touch.MaxY - Y;
      end;
     
     {$IF DEFINED(STMPE_DEBUG) or DEFINED(TOUCH_DEBUG)}
     if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'STMPE:  X=' + IntToStr(X) + ' Y=' + IntToStr(Y) + ' Z=' + IntToStr(Z));
     {$ENDIF}
     
     {Check Flags}
     if (Touch.Touch.Device.DeviceFlags and TOUCH_FLAG_MOUSE_DATA) = 0 then
      begin
       {Check Buffer}
       if (Touch.Touch.Buffer.Count < TOUCH_BUFFER_SIZE) then
        begin
         TouchData:=@Touch.Touch.Buffer.Buffer[(Touch.Touch.Buffer.Start + Touch.Touch.Buffer.Count) mod TOUCH_BUFFER_SIZE];
         if TouchData <> nil then
          begin
           {Clear Touch Data}
           FillChar(TouchData^,SizeOf(TTouchData),0);

           {Update Touch Data}
           TouchData.Info:=TOUCH_FINGER;
           TouchData.PointID:=1;

           {Check Rotation}
           case Touch.Touch.Properties.Rotation of
            TOUCH_ROTATION_0:begin
              {No Change}
              TouchData.PositionX:=X;
              TouchData.PositionY:=Y;
             end;
            TOUCH_ROTATION_90:begin
              {Swap X and Y, Invert Y}
              TouchData.PositionX:=Y;
              TouchData.PositionY:=Touch.Touch.Properties.MaxY - X;
             end;
            TOUCH_ROTATION_180:begin
              {Invert X and Y}
              TouchData.PositionX:=Touch.Touch.Properties.MaxX - X;
              TouchData.PositionY:=Touch.Touch.Properties.MaxY - Y;
             end;
            TOUCH_ROTATION_270:begin
              {Swap X and Y, Invert X}
              TouchData.PositionX:=Touch.Touch.Properties.MaxX - Y;
              TouchData.PositionY:=X;
             end;
           end;
           TouchData.PositionZ:=Z;
           
           {Check Event}
           if Assigned(Touch.Touch.Event) then
            begin
             {Event Parameter}
             TouchData.Parameter:=Touch.Touch.Parameter;

             {Event Callback}
             Touch.Touch.Event(@Touch.Touch,TouchData);
            end
           else
            begin
             {Update Count}
             Inc(Touch.Touch.Buffer.Count);

             {Signal Data Received}
             SemaphoreSignal(Touch.Touch.Buffer.Wait);
            end;
          end;
        end
       else
        begin
         if TOUCH_LOG_ENABLED then TouchLogError(@Touch.Touch,'STMPE: Buffer overflow, packet discarded'); 
           
         {Update Statistics}
         Inc(Touch.Touch.BufferOverruns); 
        end;                           
      end
     else
      begin     
       {Create Mouse Data}
       MouseData.Buttons:=MOUSE_TOUCH_BUTTON or MOUSE_ABSOLUTE_X or MOUSE_ABSOLUTE_Y; {Touch Button, Absolute X and Y}

       {Check Rotation}
       case Touch.Touch.Properties.Rotation of
        TOUCH_ROTATION_0:begin
          {No Change}
          MouseData.OffsetX:=X;
          MouseData.OffsetY:=Y;
         end;
        TOUCH_ROTATION_90:begin
          {Swap X and Y, Invert Y}
          MouseData.OffsetX:=Y;
          MouseData.OffsetY:=Touch.Touch.Properties.MaxY - X;
         end;
        TOUCH_ROTATION_180:begin
          {Invert X and Y}
          MouseData.OffsetX:=Touch.Touch.Properties.MaxX - X;
          MouseData.OffsetY:=Touch.Touch.Properties.MaxY - Y;
         end;
        TOUCH_ROTATION_270:begin
          {Swap X and Y, Invert X}
          MouseData.OffsetX:=Touch.Touch.Properties.MaxX - Y;
          MouseData.OffsetY:=X;
         end;
       end;
       MouseData.OffsetWheel:=0;
       
       {Maximum X, Y and Wheel}
       MouseData.MaximumX:=Touch.Touch.Properties.MaxX;
       MouseData.MaximumY:=Touch.Touch.Properties.MaxY;
       MouseData.MaximumWheel:=0;
       
       {Write Mouse Data}
       if MouseWrite(@MouseData,SizeOf(TMouseData),1) <> ERROR_SUCCESS then
        begin
         if TOUCH_LOG_ENABLED then TouchLogError(@Touch.Touch,'STMPE: Failed to write mouse data, packet discarded'); 
         
         {Update Statistics}
         Inc(Touch.Touch.ReceiveErrors); 
        end;
      end;
     
     {Read FIFO Size}
     STMPEReadByte(@Touch.Control,STMPE811_REG_FIFO_SIZE,@Count);
     
     {$IF DEFINED(STMPE_DEBUG) or DEFINED(TOUCH_DEBUG)}
     if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'STMPE:  FIFO Size=' + IntToStr(Count));
     {$ENDIF}
     
    end; 
   
   {Reregister Event}
   if GPIODeviceInputEvent(Touch.Control.IRQ.GPIO,Touch.Control.IRQ.Pin,Touch.Control.IRQ.Trigger,GPIO_EVENT_FLAG_NONE,INFINITE,TGPIOCallback(STMPETouchCallback),Touch) <> ERROR_SUCCESS then
    begin
     if TOUCH_LOG_ENABLED then TouchLogError(@Touch.Touch,'STMPE: Failed to re-register touch callback');
    end;
    
   {Enable Interrupt} 
   STMPESetBits(@Touch.Control,Touch.Offsets.IntEnable,STMPE811_INT_EN_FIFO_TH,STMPE811_INT_EN_FIFO_TH);
   
   {Clear Interrupt}
   STMPEWriteByte(@Touch.Control,Touch.Offsets.IntStatus,STMPE811_INT_STA_FIFO_TH);
   
   {Enable Timer}
   if (Touch.Touch.Device.DeviceFlags and TOUCH_FLAG_RELEASE_TIMER) <> 0 then
    begin
     TimerEnable(Touch.Timer);
    end; 
   
   {Release Lock}
   MutexUnlock(Touch.Touch.Lock);
  end; 
end;

{==============================================================================}
{==============================================================================}
{STMPE Helper Functions}
function STMPEReadByte(Control:PSTMPEControl;Reg:Byte;Value:PByte):LongWord;
var
 Cmd:Word;
 Data:Word;
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Control}
 if Control = nil then Exit;

 {Check Value}
 if Value = nil then Exit;
 
 {Check I2C/SPI}
 if Control.I2C <> nil then 
  begin
   Result:=I2CDeviceWriteRead(Control.I2C,Control.Address,@Reg,SizeOf(Byte),Value,SizeOf(Byte),Count);
  end
 else if Control.SPI <> nil then
  begin
   {Create command (8 bits address, 8 bits zero)}
   {Little endian so address will be sent first}
   Cmd:=Reg or STMPE_SPI_READ_CMD;
  
   {Perform Write Read (2 bytes)}  
   Result:=SPIDeviceWriteRead(Control.SPI,Control.ChipSelect,@Cmd,@Data,SizeOf(Word),SPI_TRANSFER_NONE,Count);
   if Result <> ERROR_SUCCESS then Exit;
   
   {Return Data}
   Value^:=(Data shr 8);
  end;
end;

{==============================================================================}

function STMPEWriteByte(Control:PSTMPEControl;Reg,Value:Byte):LongWord;
var
 Cmd:Word;
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Control}
 if Control = nil then Exit;

 {Check I2C/SPI}
 if Control.I2C <> nil then 
  begin
   Result:=I2CDeviceWriteWrite(Control.I2C,Control.Address,@Reg,SizeOf(Byte),@Value,SizeOf(Byte),Count);
  end
 else if Control.SPI <> nil then
  begin
   {Create command (8 bits address, 8 bits data)}
   {Little endian so address will be sent first}
   Cmd:=(Value shl 8) or Reg;
   
   {Perform Write (2 bytes)}
   Result:=SPIDeviceWrite(Control.SPI,Control.ChipSelect,@Cmd,SizeOf(Word),SPI_TRANSFER_NONE,Count);
  end;
end;

{==============================================================================}

function STMPEReadWord(Control:PSTMPEControl;Reg:Byte;Value:PWord):LongWord;
var
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Control}
 if Control = nil then Exit;

 {Check Value}
 if Value = nil then Exit;
 
 {Check I2C/SPI}
 if Control.I2C <> nil then 
  begin
   Result:=I2CDeviceWriteRead(Control.I2C,Control.Address,@Reg,SizeOf(Byte),Value,SizeOf(Word),Count);
  end
 else if Control.SPI <> nil then
  begin
   {Read Bytes}
   Result:=STMPEReadBytes(Control,Reg,SizeOf(Word),PByte(Value));
   if Result <> ERROR_SUCCESS then Exit;
   
   {Swap Data}
   Value^:=WordBEToN(Value^);
  end;
end;

{==============================================================================}

function STMPEWriteWord(Control:PSTMPEControl;Reg:Byte;Value:Word):LongWord;
var
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Control}
 if Control = nil then Exit;

 {Check I2C/SPI}
 if Control.I2C <> nil then 
  begin
   Result:=I2CDeviceWriteWrite(Control.I2C,Control.Address,@Reg,SizeOf(Byte),@Value,SizeOf(Word),Count);
  end
 else if Control.SPI <> nil then
  begin
   {Swap Data}
   Value:=WordNToBE(Value);
   
   {Write Bytes}
   Result:=STMPEWriteBytes(Control,Reg,SizeOf(Word),@Value);
  end;
end;

{==============================================================================}

function STMPEReadBytes(Control:PSTMPEControl;Reg,Len:Byte;Values:PByte):LongWord;
var
 Cmd:Word;
 Data:Word;
 Offset:Byte;
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Control}
 if Control = nil then Exit;

 {Check Values}
 if Values = nil then Exit;

 {Check I2C/SPI}
 if Control.I2C <> nil then 
  begin
   Result:=I2CDeviceWriteRead(Control.I2C,Control.Address,@Reg,SizeOf(Byte),Values,Len,Count);
  end
 else if Control.SPI <> nil then
  begin
   {Create command (8 bits register, 8 bits zero)}
   {Little endian so register will be sent first}
   Cmd:=Reg or STMPE_SPI_READ_CMD;
   
   {Read each register}
   for Offset:=0 to Len - 1 do
    begin
     {Perform Write Read (2 bytes)}  
     Result:=SPIDeviceWriteRead(Control.SPI,Control.ChipSelect,@Cmd,@Data,SizeOf(Word),SPI_TRANSFER_NONE,Count);
     if Result <> ERROR_SUCCESS then Exit;
     
     {Return Data}
     PByte(Values + Offset)^:=(Data shr 8);
     
     {Increment register}
     Inc(Cmd);
    end;
  end;
end;

{==============================================================================}

function STMPEWriteBytes(Control:PSTMPEControl;Reg,Len:Byte;Values:PByte):LongWord;
var
 Cmd:Word;
 Offset:Byte;
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Control}
 if Control = nil then Exit;

 {Check Values}
 if Values = nil then Exit;
 
 {Check I2C/SPI}
 if Control.I2C <> nil then 
  begin
   Result:=I2CDeviceWriteWrite(Control.I2C,Control.Address,@Reg,SizeOf(Byte),Values,Len,Count);
  end
 else if Control.SPI <> nil then
  begin
   //To Do //Continuing //Not currently used, see STMPEReadBytes
  end;
end;
 
{==============================================================================}
 
function STMPESetBits(Control:PSTMPEControl;Reg,Mask,Value:Byte):LongWord;
var
 Current:Byte;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {$IF DEFINED(STMPE_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'STMPE: Set Bits (Reg=' + IntToHex(Reg,2) + ' Mask=' + IntToHex(Mask,2) + ' Value=' + IntToHex(Value,2) + ')');
 {$ENDIF}
 
 {Check Control}
 if Control = nil then Exit;
 
 {Read Byte}
 Result:=STMPEReadByte(Control,Reg,@Current);
 if Result <> ERROR_SUCCESS then Exit;
 
 {$IF DEFINED(STMPE_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'STMPE:  Current(Pre)=' + IntToHex(Current,2));
 {$ENDIF}
 
 {Update Value}
 Current:=Current and not(Mask);
 Current:=Current or Value;
 
 {$IF DEFINED(STMPE_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'STMPE:  Current(Post)=' + IntToHex(Current,2));
 {$ENDIF}
 
 {Write Byte}
 Result:=STMPEWriteByte(Control,Reg,Current);
end;

{==============================================================================}

function STMPEResetFIFO(Control:PSTMPEControl;Reg:Byte):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {$IF DEFINED(STMPE_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'STMPE: Reset FIFO (Reg=' + IntToHex(Reg,2) + ')');
 {$ENDIF}
 
 {Check Control}
 if Control = nil then Exit;

 {Set Bits (Enable FIFO Reset)}
 Result:=STMPESetBits(Control,Reg,STMPE811_FIFO_STA_RESET,STMPE811_FIFO_STA_RESET);
 if Result <> ERROR_SUCCESS then Exit;
 
 {Set Bits (Disable FIFO Reset)}
 Result:=STMPESetBits(Control,Reg,STMPE811_FIFO_STA_RESET,0);
end;

{==============================================================================}
{==============================================================================}
{STMPE Internal Functions}
function STMPETouchUpdateConfig(Touch:PSTMPETouch):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(STMPE_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'STMPE: Update Config');
 {$ENDIF}

 {Check Rotation}
 case Touch.Touch.Properties.Rotation of
  TOUCH_ROTATION_0,TOUCH_ROTATION_180:begin
    {Update Width and Height}
    Touch.Touch.Properties.Width:=Touch.Width;
    Touch.Touch.Properties.Height:=Touch.Height;

    {Update Max X and Y}
    if (Touch.Touch.Device.DeviceFlags and TOUCH_FLAG_SWAP_MAX_XY) = 0 then
     begin
      Touch.Touch.Properties.MaxX:=Touch.MaxX;
      Touch.Touch.Properties.MaxY:=Touch.MaxY;
     end
    else
     begin
      Touch.Touch.Properties.MaxX:=Touch.MaxY;
      Touch.Touch.Properties.MaxY:=Touch.MaxX;
     end;
   end;
  TOUCH_ROTATION_90,TOUCH_ROTATION_270:begin
    {Update Width and Height}
    Touch.Touch.Properties.Width:=Touch.Height;
    Touch.Touch.Properties.Height:=Touch.Width;

    {Update Max X and Y}
    if (Touch.Touch.Device.DeviceFlags and TOUCH_FLAG_SWAP_MAX_XY) = 0 then
     begin
      Touch.Touch.Properties.MaxX:=Touch.MaxY;
      Touch.Touch.Properties.MaxY:=Touch.MaxX;
     end
    else
     begin
      Touch.Touch.Properties.MaxX:=Touch.MaxX;
      Touch.Touch.Properties.MaxY:=Touch.MaxY;
     end;
   end;
 end;

 {Update Max Points}
 Touch.Touch.Properties.MaxPoints:=Touch.MaxPoints;

 {$IF DEFINED(STMPE_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'STMPE:  Width: ' + IntToStr(Touch.Touch.Properties.Width) + ' Height: ' + IntToStr(Touch.Touch.Properties.Height));
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'STMPE:  Max Points: ' + IntToStr(Touch.Touch.Properties.MaxPoints) + ' Max X: ' + IntToStr(Touch.Touch.Properties.MaxX) + ' Max Y: ' + IntToStr(Touch.Touch.Properties.MaxY));
 {$ENDIF}

 {Check Timer}
 if ((Touch.Touch.Device.DeviceFlags and TOUCH_FLAG_RELEASE_TIMER) <> 0) and (Touch.Timer = INVALID_HANDLE_VALUE) then
  begin
   {Create Timer}
   Touch.Timer:=TimerCreateEx(50,TIMER_STATE_DISABLED,TIMER_FLAG_WORKER,TTimerEvent(STMPETouchTimer),Touch); {Scheduled by GPIO Event Callback}
  end
 else if ((Touch.Touch.Device.DeviceFlags and TOUCH_FLAG_RELEASE_TIMER) = 0) and (Touch.Timer <> INVALID_HANDLE_VALUE) then
  begin
   {Cancel Timer}
   TimerDestroy(Touch.Timer);
   Touch.Timer:=INVALID_HANDLE_VALUE;
  end;

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
