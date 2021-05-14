{
ARM PrimeCell PL180/181 Multimedia Card Interface Driver.

Copyright (C) 2017 - SoftOz Pty Ltd.

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

  Linux - \drivers\mmc\host\mmci.c - Copyright (C) 2010 ST-Ericsson SA
  Linux - \arch\arm\mach-versatile\core.c - Copyright (C) 2000 Deep Blue Solutions Ltd
  Linux - \arch\arm\mach-versatile\versatile_pb.c - Copyright (C) 2004 ARM Limited
  
  QEMU - \hw\sd\pl181.c - Copyright (c) 2007 CodeSourcery
  QEMU - \hw\misc\arm_sysctl.c - Copyright (c) 2006-2007 CodeSourcery
  
References
==========
 
 PL18X - http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0172a/index.html
 
ARM PrimeCell PL180/181 Multimedia Card
=======================================
 
 The PL180/181 Multimedia Card is an ARM peripheral that is compatible with MMC multimedia
 cards and SD secure digital cards in memory mapped I/O format compatible with the ARM
 advanced peripheral bus (APB).

 The design of the Pl180/181 allows for multiple cards per controller however this driver
 currently only supports attaching one card.
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit PL18X; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,DMA,Storage,MMC,SysUtils;
 
{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}
                     
{==============================================================================}
const
 {PL18X specific constants}
 PL180_MMCI_DESCRIPTION = 'ARM PrimeCell PL180 MMCI Host';  {Description of PL180 device}
 PL181_MMCI_DESCRIPTION = 'ARM PrimeCell PL181 MMCI Host';  {Description of PL181 device}
 
const
 {PL18X Power control register}
 PL18X_MMCI_POWER  = $000;
 PL18X_MMCI_POWER_OFF = $00;      {Power-off}
 {Bits 1:0 - $01 Reserved}
 PL18X_MMCI_POWER_UP  = $02;      {Power-up}
 PL18X_MMCI_POWER_ON  = $03;      {Power-on}
 {Bits 5:2 - Output Voltage}
 PL18X_MMCI_POWER_OD  = (1 shl 6); {MCICMD output control}
 PL18X_MMCI_POWER_ROD = (1 shl 7); {Rod control}
 
 {The ST Micro version does not have ROD and reuses the voltage registers for direction settings}
 PL18X_MMCI_POWER_ST_DATA2DIREN  = (1 shl 2);
 PL18X_MMCI_POWER_ST_CMDDIREN    = (1 shl 3);
 PL18X_MMCI_POWER_ST_DATA0DIREN  = (1 shl 4);
 PL18X_MMCI_POWER_ST_DATA31DIREN = (1 shl 5);
 PL18X_MMCI_POWER_ST_FBCLKEN     = (1 shl 7);
 PL18X_MMCI_POWER_ST_DATA74DIREN = (1 shl 8);
 
 {PL18X Clock control register}
 PL18X_MMCI_CLOCK  = $004;
 PL18X_MMCI_CLOCK_ENABLE   = (1 shl 8);   {Enable MCI bus clock: 0 = Clock disabled / 1 = Clock enabled}
 PL18X_MMCI_CLOCK_PWRSAVE  = (1 shl 9);   {Disable MCI clock output when bus is idle: 0 = Always enabled / 1 = Clock enabled when bus is active}
 PL18X_MMCI_CLOCK_BYPASS   = (1 shl 10);  {Enable bypass of clock divide logic: 0 = Disable bypass / 1 = Enable bypass}
 PL18X_MMCI_CLOCK_4BIT_BUS = (1 shl 11);  {Enable wide bus mode: 0 = Standard bus mode (only MCIDAT0 used) / 1 = Wide bus mode (MCIDAT3:0 used)}
 
 {8bit wide buses, hardware flow contronl, negative edges and clock inversion supported in ST Micro U300 and Ux500 versions}
 PL18X_MMCI_CLOCK_ST_8BIT_BUS       = (1 shl 12);
 PL18X_MMCI_CLOCK_ST_U300_HWFCEN    = (1 shl 13);
 PL18X_MMCI_CLOCK_ST_UX500_NEG_EDGE = (1 shl 13);
 PL18X_MMCI_CLOCK_ST_UX500_HWFCEN   = (1 shl 14);
 PL18X_MMCI_CLOCK_ST_UX500_CLK_INV  = (1 shl 15);
 {Modified PL180 on Versatile Express platform}
 PL18X_MMCI_CLOCK_ARM_HWFCEN        = (1 shl 12);

 {Modified on Qualcomm Integrations}
 PL18X_MMCI_CLOCK_QCOM_WIDEBUS_8 = (1 shl 10) or (1 shl 11);
 PL18X_MMCI_CLOCK_QCOM_FLOWENA   = (1 shl 12);
 PL18X_MMCI_CLOCK_QCOM_INVERTOUT = (1 shl 13);

 {Select in latch data and command in}
 PL18X_MMCI_CLOCK_QCOM_SELECT_IN_FBCLK    = (1 shl 15);
 PL18X_MMCI_CLOCK_QCOM_SELECT_IN_DDR_MODE = (1 shl 14) or (1 shl 15);

 {PL18X Argument register}
 PL18X_MMCI_ARGUMENT  = $008;
 {Bits 31:0 Command Argument}
 
 {PL18X Command register}
 PL18X_MMCI_COMMAND  = $00c;
 {Bits 5:0 Command Index}
 PL18X_MMCI_CPSM_RESPONSE  = (1 shl 6);  {If set, CPSM waits for a response}
 PL18X_MMCI_CPSM_LONGRSP   = (1 shl 7);  {If set, CPSM receives a 136-bit long response}
 PL18X_MMCI_CPSM_INTERRUPT = (1 shl 8);  {If set, CPSM disables command timer and waits for interrupt request}
 PL18X_MMCI_CPSM_PENDING   = (1 shl 9);  {If set, CPSM waits for CmdPend before it starts sending a command}
 PL18X_MMCI_CPSM_ENABLE    = (1 shl 10); {If set, CPSM is enabled}
 {Argument flag extenstions in the ST Micro versions}
 PL18X_MMCI_CPSM_ST_SDIO_SUSP   = (1 shl 11);
 PL18X_MMCI_CPSM_ST_ENCMD_COMPL = (1 shl 12);
 PL18X_MMCI_CPSM_ST_NIEN        = (1 shl 13);
 PL18X_MMCI_CPSM_ST_CE_ATACMD   = (1 shl 14);

 {Modified on Qualcomm Integrations}
 PL18X_MMCI_CPSM_QCOM_DATCMD     = (1 shl 12);
 PL18X_MMCI_CPSM_QCOM_MCIABORT   = (1 shl 13);
 PL18X_MMCI_CPSM_QCOM_CCSENABLE  = (1 shl 14);
 PL18X_MMCI_CPSM_QCOM_CCSDISABLE = (1 shl 15);
 PL18X_MMCI_CPSM_QCOM_AUTO_CMD19 = (1 shl 16);
 PL18X_MMCI_CPSM_QCOM_AUTO_CMD21 = (1 shl 21);
 
 {PL18X Command response register}
 PL18X_MMCI_RESPCMD  = $010;
 {Bits 5:0  Response command index}
 {Bits 31:6 Reserved}
 
 {PL18X Response registers}
 PL18X_MMCI_RESPONSE0  = $014;
 PL18X_MMCI_RESPONSE1  = $018;
 PL18X_MMCI_RESPONSE2  = $01c;
 PL18X_MMCI_RESPONSE3  = $020;
 {Bits 31:0 Card Status}

 {PL18X Data timer register}
 PL18X_MMCI_DATATIMER  = $024;
 {Bits 31:0 Data Timeout Period}
 
 {PL18X Data length register}
 PL18X_MMCI_DATALENGTH  = $028;
 {Bits 15:0  Data Length Value}
 {Bits 31:16 Reserved}
 
 {PL18X Data control register}
 PL18X_MMCI_DATACTRL  = $02c;
 PL18X_MMCI_DPSM_ENABLE    = (1 shl 0); {Data transfer enabled}
 PL18X_MMCI_DPSM_DIRECTION = (1 shl 1); {Data transfer direction: 0 = From controller to card / 1 = From card to controller}
 PL18X_MMCI_DPSM_MODE      = (1 shl 2); {Data transfer mode: 0 = Block data transfer / 1 = Stream data transfer}
 PL18X_MMCI_DPSM_DMAENABLE = (1 shl 3); {Enable DMA: 0 = DMA disabled / 1 = DMA enabled}
 PL18X_MMCI_DPSM_BLOCKSIZE = (1 shl 4); {Data block length}
 {Control register extensions in the ST Micro U300 and Ux500 versions}
 PL18X_MMCI_ST_DPSM_RWSTART = (1 shl 8);
 PL18X_MMCI_ST_DPSM_RWSTOP  = (1 shl 9);
 PL18X_MMCI_ST_DPSM_RWMOD   = (1 shl 10);
 PL18X_MMCI_ST_DPSM_SDIOEN  = (1 shl 11);
 {Control register extensions in the ST Micro Ux500 versions}
 PL18X_MMCI_ST_DPSM_DMAREQCTL   = (1 shl 12);
 PL18X_MMCI_ST_DPSM_DBOOTMODEEN = (1 shl 13);
 PL18X_MMCI_ST_DPSM_BUSYMODE    = (1 shl 14);
 PL18X_MMCI_ST_DPSM_DDRMODE     = (1 shl 15);

 {PL18X Data counter register}
 PL18X_MMCI_DATACNT  = $030;
 {Bits 15:0  Remaining Data}
 {Bits 31:16 Reserved}

 {PL18X Status register}
 PL18X_MMCI_STATUS  = $034;
 PL18X_MMCI_CMDCRCFAIL      = (1 shl 0);
 PL18X_MMCI_DATACRCFAIL     = (1 shl 1);
 PL18X_MMCI_CMDTIMEOUT      = (1 shl 2);
 PL18X_MMCI_DATATIMEOUT     = (1 shl 3);
 PL18X_MMCI_TXUNDERRUN      = (1 shl 4);
 PL18X_MMCI_RXOVERRUN       = (1 shl 5);
 PL18X_MMCI_CMDRESPEND      = (1 shl 6);
 PL18X_MMCI_CMDSENT         = (1 shl 7);
 PL18X_MMCI_DATAEND         = (1 shl 8);
 PL18X_MMCI_STARTBITERR     = (1 shl 9);
 PL18X_MMCI_DATABLOCKEND    = (1 shl 10);
 PL18X_MMCI_CMDACTIVE       = (1 shl 11);
 PL18X_MMCI_TXACTIVE        = (1 shl 12);
 PL18X_MMCI_RXACTIVE        = (1 shl 13);
 PL18X_MMCI_TXFIFOHALFEMPTY = (1 shl 14);
 PL18X_MMCI_RXFIFOHALFFULL  = (1 shl 15);
 PL18X_MMCI_TXFIFOFULL      = (1 shl 16);
 PL18X_MMCI_RXFIFOFULL      = (1 shl 17);
 PL18X_MMCI_TXFIFOEMPTY     = (1 shl 18);
 PL18X_MMCI_RXFIFOEMPTY     = (1 shl 19);
 PL18X_MMCI_TXDATAAVLBL     = (1 shl 20);
 PL18X_MMCI_RXDATAAVLBL     = (1 shl 21);
 {Extended status bits for the ST Micro variants}
 PL18X_MMCI_ST_SDIOIT       = (1 shl 22);
 PL18X_MMCI_ST_CEATAEND     = (1 shl 23);
 PL18X_MMCI_ST_CARDBUSY     = (1 shl 24);
 
 {PL18X Clear register}
 PL18X_MMCI_CLEAR  = $038;
 PL18X_MMCI_CMDCRCFAILCLR   = (1 shl 0);
 PL18X_MMCI_DATACRCFAILCLR  = (1 shl 1);
 PL18X_MMCI_CMDTIMEOUTCLR   = (1 shl 2);
 PL18X_MMCI_DATATIMEOUTCLR  = (1 shl 3);
 PL18X_MMCI_TXUNDERRUNCLR   = (1 shl 4);
 PL18X_MMCI_RXOVERRUNCLR    = (1 shl 5);
 PL18X_MMCI_CMDRESPENDCLR   = (1 shl 6);
 PL18X_MMCI_CMDSENTCLR      = (1 shl 7);
 PL18X_MMCI_DATAENDCLR      = (1 shl 8);
 PL18X_MMCI_STARTBITERRCLR  = (1 shl 9);
 PL18X_MMCI_DATABLOCKENDCLR = (1 shl 10);
 {Extended status bits for the ST Micro variants}
 PL18X_MMCI_ST_SDIOITC      = (1 shl 22);
 PL18X_MMCI_ST_CEATAENDC    = (1 shl 23);
 PL18X_MMCI_ST_BUSYENDC     = (1 shl 24);

 {PL18X Interrupt mask registers}
 PL18X_MMCI_MASK0  = $03c;
 PL18X_MMCI_MASK1  = $040;
 PL18X_MMCI_CMDCRCFAILMASK      = (1 shl 0);
 PL18X_MMCI_DATACRCFAILMASK     = (1 shl 1);
 PL18X_MMCI_CMDTIMEOUTMASK      = (1 shl 2);
 PL18X_MMCI_DATATIMEOUTMASK     = (1 shl 3);
 PL18X_MMCI_TXUNDERRUNMASK      = (1 shl 4);
 PL18X_MMCI_RXOVERRUNMASK       = (1 shl 5);
 PL18X_MMCI_CMDRESPENDMASK      = (1 shl 6);
 PL18X_MMCI_CMDSENTMASK         = (1 shl 7);
 PL18X_MMCI_DATAENDMASK         = (1 shl 8);
 PL18X_MMCI_STARTBITERRMASK     = (1 shl 9);
 PL18X_MMCI_DATABLOCKENDMASK    = (1 shl 10);
 PL18X_MMCI_CMDACTIVEMASK       = (1 shl 11);
 PL18X_MMCI_TXACTIVEMASK        = (1 shl 12);
 PL18X_MMCI_RXACTIVEMASK        = (1 shl 13);
 PL18X_MMCI_TXFIFOHALFEMPTYMASK = (1 shl 14);
 PL18X_MMCI_RXFIFOHALFFULLMASK  = (1 shl 15);
 PL18X_MMCI_TXFIFOFULLMASK      = (1 shl 16);
 PL18X_MMCI_RXFIFOFULLMASK      = (1 shl 17);
 PL18X_MMCI_TXFIFOEMPTYMASK     = (1 shl 18);
 PL18X_MMCI_RXFIFOEMPTYMASK     = (1 shl 19);
 PL18X_MMCI_TXDATAAVLBLMASK     = (1 shl 20);
 PL18X_MMCI_RXDATAAVLBLMASK     = (1 shl 21);
 {Extended status bits for the ST Micro variants}
 PL18X_MMCI_ST_SDIOITMASK       = (1 shl 22);
 PL18X_MMCI_ST_CEATAENDMASK     = (1 shl 23);
 PL18X_MMCI_ST_BUSYEND          = (1 shl 24);
 
 {PL18X SD card select register}
 PL18X_MMCI_SELECT  = $044;

 {PL18X FIFO counter register}
 PL18X_MMCI_FIFOCNT  = $048;
 
 {PL18X Data FIFO register}
 PL18X_MMCI_FIFO  = $080; {To $0bc}
 
 {PL18X Peripheral identification register}
 PL18X_MMCI_PERIPHID = $FE0;

 {PL18X PrimeCell identification register}
 PL18X_MMCI_PCELLID  = $FF0;
 
 PL18X_MMCI_IRQENABLE = (PL18X_MMCI_CMDCRCFAILMASK or PL18X_MMCI_DATACRCFAILMASK or PL18X_MMCI_CMDTIMEOUTMASK or  PL18X_MMCI_DATATIMEOUTMASK
                      or PL18X_MMCI_TXUNDERRUNMASK or PL18X_MMCI_RXOVERRUNMASK or PL18X_MMCI_CMDRESPENDMASK or PL18X_MMCI_CMDSENTMASK or PL18X_MMCI_STARTBITERRMASK);
 {These interrupts are directed to IRQ1 when two IRQ lines are available}
 PL18X_MMCI_IRQ1MASK = (PL18X_MMCI_RXFIFOHALFFULLMASK or PL18X_MMCI_RXDATAAVLBLMASK or PL18X_MMCI_TXFIFOHALFEMPTYMASK);
 
{==============================================================================}
type
 {PL18X specific types}
 PPL18XVersionData = ^TPL18XVersionData;
 
 PPL18XVersionID = ^TPL18XVersionID;
 TPL18XVersionID = record
  PeripheralID:LongWord;
  PeripheralMask:LongWord;
  VersionData:PPL18XVersionData;
 end;
 
 TPL18XVersionData = record
  Name:String;                      {Name of the device}
  ClockRegister:LongWord;           {Default value for MCICLOCK register}
  ClockEnable:LongWord;             {Enable value for MMCICLOCK register}  
  Clock8BitEnable:LongWord;         {Enable value for 8 bit bus}
  ClockNegativeEdgeEnable:LongWord; {Enable value for inverted data/cmd output}
  DataLengthBits:LongWord;          {Number of bits in the MMCIDATALENGTH register}
  FIFOSize:LongWord;                {Number of bytes that can be written when MMCI_TXFIFOEMPTY is asserted (likewise for RX)}
  FIFOHalfSize:LongWord;            {Number of bytes that can be written when MCI_TXFIFOHALFEMPTY is asserted (likewise for RX)}
  DataCommandEnable:LongWord;       {Enable value for data commands}
  DataControlMaskDDR:LongWord;      {DDR mode mask in MMCIDATACTRL register}
  DataControlMaskSDIO:LongWord;     {SDIO enable mask in MMCIDATACTRL register}
  STSDIO:LongBool;                  {Enable ST specific SDIO logic}
  STClockDivider:LongBool;          {True if using a ST-specific clock divider algorithm}
  BlockSizeDataControl16:LongBool;  {True if Block size is at b16..b30 position in MMCIDATACTRL register}
  BlockSizeDataControl4:LongBool;   {True if Block size is at b4..b16 position in MMCIDATACTRL register}
  PowerPowerUp:LongWord;            {Power up value for MMCIPOWER register}
  ClockMaximum:LongWord;            {Maximum clk frequency supported by the controller}
  SignalDirection:LongBool;         {Input/out direction of bus signals can be indicated}
  PowerClockGate:LongBool;          {MMCIPOWER register must be used to gate the clock}
  BusyDetect:LongBool;              {True if busy detection on dat0 is supported}
  PowerNoPower:LongBool;            {Bits in MMCIPOWER don't control external power supply}
  ExplicitMClockControl:LongBool;   {Enable explicit mclk control in driver}
  QualcommFIFO:LongBool;            {Enable Qualcomm specific FIFO PIO read logic}
  QualcommDMA:LongBool;             {Enable Qualcomm specific DMA glue for DMA transfers}
  ReversedIRQ:LongBool;             {Handle data irq before cmd irq}
 end;
 
 {Layout of the PL18X registers (See: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0172a/i1006458.html)}
 PPL18XMMCIRegisters = ^TPL18XMMCIRegisters;
 TPL18XMMCIRegisters = record
  Power:LongWord;                   {Power control register}
  Clock:LongWord;                   {Clock control register}
  Argument:LongWord;                {Argument register}
  Command:LongWord;                 {Command register}
  RespCmd:LongWord;                 {Response command register}
  Response0:LongWord;               {Response register}
  Response1:LongWord;               {Response register}
  Response2:LongWord;               {Response register}
  Response3:LongWord;               {Response register}
  DataTimer:LongWord;               {Data timer}
  DataLength:LongWord;              {Data length register}
  DataCtrl:LongWord;                {Data control register}
  DataCnt:LongWord;                 {Data counter}
  Status:LongWord;                  {Status register}
  Clear:LongWord;                   {Clear register}
  Mask0:LongWord;                   {Interrupt 0 mask register}
  Mask1:LongWord;                   {Interrupt 1 mask register}
  Select:LongWord;                  {Secure digital memory card select register}
  FifoCnt:LongWord;                 {FIFO counter}
  Reserved:array[$4C..$7C] of Byte; {Reserved}
  FIFO:LongWord;                    {Data FIFO register (0x80 to 0xBC)}
 end; 

 {PL18X SDHCI types}
 PPL18XSDHCIHost = ^TPL18XSDHCIHost;
 
 {PL18X SDHCI Host Methods}
 TPL18XSDHCIGetRXFIFOCount = function(SDHCI:PPL18XSDHCIHost;Status,Remain:LongWord):LongWord;
 
 TPL18XSDHCIHost = record
  {SDHCI Properties}
  SDHCI:TSDHCIHost;
  {PL18X Properties}
  IRQ0:LongWord;
  IRQ1:LongWord;
  Lock:TSpinHandle;                                {Host lock (Differs from lock in Host portion) (Spin lock due to use by interrupt handler)}
  SingleIRQ:LongBool;                              {The host only has a single IRQ line instead of the standard 2 lines}
  Registers:PPL18XMMCIRegisters;                   {Host registers}
  Version:PPL18XVersionData;                       {Host version data}
  ClockRegister:LongWord;                          {Current clock register value}
  PowerRegister:LongWord;                          {Current power register value}
  DataCtrlRegister:LongWord;                       {Current data control register value}
  MaximumBlockSize:LongWord;                       {Host maximum block size}
  MaximumRequestSize:LongWord;                     {Host maximum request size}
  BusyStatus:LongWord;                             {Current Busy Status for ST Micro variants}
  GetRXFIFOCount:TPL18XSDHCIGetRXFIFOCount;        {Model specific GetRXFIFOCount function}
 end;
 
{==============================================================================}
var
 {PL18X specific variables}
 PL18X_MMCI_FIQ_ENABLED:LongBool;        {The SDHCI uses Fast Interrupt Requests (FIQ) instead of IRQ}
 PL18X_MMCI_MIN_FREQ:LongWord = 400000;  {Minimum clock frequency for SDHCI (Default minimum of 400KHz)}
 PL18X_MMCI_MAX_FREQ:LongWord = 400000;  {Maximum clock frequency for SDHCI (Default maximum of 400KHz)}
 
{==============================================================================}
{Initialization Functions}
procedure PL18XInit;
 
{==============================================================================}
{PL18X Functions}
function PL180SDHCICreate(Address:PtrUInt;const Name:String;IRQ0,IRQ1,ClockMinimum,ClockMaximum:LongWord;CardDetect:TMMCDeviceGetCardDetect = nil;WriteProtect:TMMCDeviceGetWriteProtect = nil):PSDHCIHost;
function PL181SDHCICreate(Address:PtrUInt;const Name:String;IRQ0,IRQ1,ClockMinimum,ClockMaximum:LongWord;CardDetect:TMMCDeviceGetCardDetect = nil;WriteProtect:TMMCDeviceGetWriteProtect = nil):PSDHCIHost;

function PL18XSDHCIDestroy(SDHCI:PSDHCIHost):LongWord;

{==============================================================================}
{PL18X MMC Functions}
function PL18XMMCDeviceInitialize(MMC:PMMCDevice):LongWord;

function PL18XMMCDeviceGetCardDetect(MMC:PMMCDevice):LongWord;
function PL18XMMCDeviceGetWriteProtect(MMC:PMMCDevice):LongWord;
function PL18XMMCDeviceSendCommand(MMC:PMMCDevice;Command:PMMCCommand):LongWord; 
function PL18XMMCDeviceSetIOS(MMC:PMMCDevice):LongWord;

{==============================================================================}
{PL18X SDHCI Functions}
function PL18XSDHCIHostStart(SDHCI:PSDHCIHost):LongWord;
function PL18XSDHCIHostStop(SDHCI:PSDHCIHost):LongWord;

function PL18XSDHCIHostReadByte(SDHCI:PSDHCIHost;Reg:LongWord):Byte; 
function PL18XSDHCIHostReadWord(SDHCI:PSDHCIHost;Reg:LongWord):Word; 
function PL18XSDHCIHostReadLong(SDHCI:PSDHCIHost;Reg:LongWord):LongWord; 
procedure PL18XSDHCIHostWriteByte(SDHCI:PSDHCIHost;Reg:LongWord;Value:Byte); 
procedure PL18XSDHCIHostWriteWord(SDHCI:PSDHCIHost;Reg:LongWord;Value:Word); 
procedure PL18XSDHCIHostWriteLong(SDHCI:PSDHCIHost;Reg:LongWord;Value:LongWord); 

procedure PL18XSDHCIInterruptHandler(SDHCI:PPL18XSDHCIHost);

procedure PL18XSDHCIDataInterrupt(SDHCI:PPL18XSDHCIHost;Status:LongWord);
procedure PL18XSDHCICommandInterrupt(SDHCI:PPL18XSDHCIHost;Status:LongWord);

procedure PL18XSDHCIStopData(SDHCI:PPL18XSDHCIHost);

procedure PL18XSDHCIPIOInterruptHandler(SDHCI:PPL18XSDHCIHost);

function PL18XSHDCIReadPIO(SDHCI:PPL18XSDHCIHost;Buffer:Pointer;Remain:LongWord):LongWord;
function PL18XSDHCIWritePIO(SDHCI:PPL18XSDHCIHost;Buffer:Pointer;Remain,Status:LongWord):LongWord;

{==============================================================================}
{PL18X Helper Functions}
function PL18XGetPeripheralID(SDHCI:PPL18XSDHCIHost):LongWord;
function PL18XGetVersionData(SDHCI:PPL18XSDHCIHost):PPL18XVersionData;

function PL18XGetRXFIFOCount(SDHCI:PPL18XSDHCIHost;Status,Remain:LongWord):LongWord;
function PL18XQualcommGetRXFIFOCount(SDHCI:PPL18XSDHCIHost;Status,Remain:LongWord):LongWord;

procedure PL18XRegisterDelay(SDHCI:PPL18XSDHCIHost);

procedure PL18XSetClockRegister(SDHCI:PPL18XSDHCIHost;MMC:PMMCDevice;Desired:LongWord);
procedure PL18XSetPowerRegister(SDHCI:PPL18XSDHCIHost;Value:LongWord);

procedure PL18XSetInterruptMask1(SDHCI:PPL18XSDHCIHost;Mask:LongWord);

procedure PL18XWriteClockRegister(SDHCI:PPL18XSDHCIHost;Clock:LongWord);
procedure PL18XWritePowerRegister(SDHCI:PPL18XSDHCIHost;Power:LongWord);
procedure PL18XWriteDataCtrlRegister(SDHCI:PPL18XSDHCIHost;DataCtrl:LongWord);

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {PL18X specific variables}
 PL18XInitialized:Boolean;
 
var
 PL18X_VERSION_DATA_ARM:TPL18XVersionData;
 PL18X_VERSION_DATA_ARM_EXTENDED_FIFO:TPL18XVersionData;
 PL18X_VERSION_DATA_ARM_EXTENDED_FIFO_HWFC:TPL18XVersionData;
 PL18X_VERSION_DATA_U300:TPL18XVersionData;
 PL18X_VERSION_DATA_NOMADIK:TPL18XVersionData;
 PL18X_VERSION_DATA_UX500:TPL18XVersionData;
 PL18X_VERSION_DATA_UX500V2:TPL18XVersionData;
 PL18X_VERSION_DATA_QCOM:TPL18XVersionData;
 
 PL18X_VERSION_ID:array[0..9] of TPL18XVersionID = (
  (PeripheralID:$00041180;  PeripheralMask:$ff0fffff; VersionData:@PL18X_VERSION_DATA_ARM),
  (PeripheralID:$01041180;  PeripheralMask:$ff0fffff; VersionData:@PL18X_VERSION_DATA_ARM_EXTENDED_FIFO),
  (PeripheralID:$02041180;  PeripheralMask:$ff0fffff; VersionData:@PL18X_VERSION_DATA_ARM_EXTENDED_FIFO_HWFC),
  (PeripheralID:$00041181;  PeripheralMask:$000fffff; VersionData:@PL18X_VERSION_DATA_ARM),
  {ST Micro variants}
  (PeripheralID:$00180180;  PeripheralMask:$00ffffff; VersionData:@PL18X_VERSION_DATA_U300),
  (PeripheralID:$10180180;  PeripheralMask:$f0ffffff; VersionData:@PL18X_VERSION_DATA_NOMADIK),
  (PeripheralID:$00280180;  PeripheralMask:$00ffffff; VersionData:@PL18X_VERSION_DATA_NOMADIK),
  (PeripheralID:$00480180;  PeripheralMask:$f0ffffff; VersionData:@PL18X_VERSION_DATA_UX500),
  (PeripheralID:$10480180;  PeripheralMask:$f0ffffff; VersionData:@PL18X_VERSION_DATA_UX500V2),
  {Qualcomm variants}
  (PeripheralID:$00051180;  PeripheralMask:$000fffff; VersionData:@PL18X_VERSION_DATA_QCOM)
 );
 
{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure PL18XInit;
{Initialize the PL18X unit and version table}

{Note: Called only during system startup}
begin
 {}
 {Check Initialized}
 if PL18XInitialized then Exit;
 
 {Initialize Version Table}
 PL18X_VERSION_DATA_ARM.Name:='ARM';
 PL18X_VERSION_DATA_ARM.DataLengthBits:=16;
 PL18X_VERSION_DATA_ARM.FIFOSize:=16 * 4;
 PL18X_VERSION_DATA_ARM.FIFOHalfSize:=8 * 4;
 PL18X_VERSION_DATA_ARM.PowerPowerUp:=PL18X_MMCI_POWER_UP;
 PL18X_VERSION_DATA_ARM.ClockMaximum:=100000000;
 PL18X_VERSION_DATA_ARM.ReversedIRQ:=True;

 PL18X_VERSION_DATA_ARM_EXTENDED_FIFO.Name:='ARM Extended FIFO';
 PL18X_VERSION_DATA_ARM_EXTENDED_FIFO.DataLengthBits:=16;
 PL18X_VERSION_DATA_ARM_EXTENDED_FIFO.FIFOSize:=128 * 4;
 PL18X_VERSION_DATA_ARM_EXTENDED_FIFO.FIFOHalfSize:=64 * 4;
 PL18X_VERSION_DATA_ARM_EXTENDED_FIFO.PowerPowerUp:=PL18X_MMCI_POWER_UP;
 PL18X_VERSION_DATA_ARM_EXTENDED_FIFO.ClockMaximum:=100000000;
 
 PL18X_VERSION_DATA_ARM_EXTENDED_FIFO_HWFC.Name:='ARM Extended FIFO with Hardware Flow Control';
 PL18X_VERSION_DATA_ARM_EXTENDED_FIFO_HWFC.ClockEnable:=PL18X_MMCI_CLOCK_ARM_HWFCEN;
 PL18X_VERSION_DATA_ARM_EXTENDED_FIFO_HWFC.DataLengthBits:=16;
 PL18X_VERSION_DATA_ARM_EXTENDED_FIFO_HWFC.FIFOSize:=128 * 4;
 PL18X_VERSION_DATA_ARM_EXTENDED_FIFO_HWFC.FIFOHalfSize:=64 * 4;
 PL18X_VERSION_DATA_ARM_EXTENDED_FIFO_HWFC.PowerPowerUp:=PL18X_MMCI_POWER_UP;
 PL18X_VERSION_DATA_ARM_EXTENDED_FIFO_HWFC.ClockMaximum:=100000000;
 
 PL18X_VERSION_DATA_U300.Name:='ST U300';
 PL18X_VERSION_DATA_U300.ClockEnable:=PL18X_MMCI_CLOCK_ST_U300_HWFCEN;
 PL18X_VERSION_DATA_U300.Clock8BitEnable:=PL18X_MMCI_CLOCK_ST_8BIT_BUS;
 PL18X_VERSION_DATA_U300.DataLengthBits:=16;
 PL18X_VERSION_DATA_U300.FIFOSize:=16 * 4;
 PL18X_VERSION_DATA_U300.FIFOHalfSize:=8 * 4;
 PL18X_VERSION_DATA_U300.DataControlMaskSDIO:=PL18X_MMCI_ST_DPSM_SDIOEN;
 PL18X_VERSION_DATA_U300.STSDIO:=True;
 PL18X_VERSION_DATA_U300.PowerPowerUp:=PL18X_MMCI_POWER_ON;
 PL18X_VERSION_DATA_U300.ClockMaximum:=100000000;
 PL18X_VERSION_DATA_U300.SignalDirection:=True;
 PL18X_VERSION_DATA_U300.PowerClockGate:=True;
 PL18X_VERSION_DATA_U300.PowerNoPower:=True;
 
 PL18X_VERSION_DATA_NOMADIK.Name:='ST Nomadik';
 PL18X_VERSION_DATA_NOMADIK.ClockRegister:=PL18X_MMCI_CLOCK_ENABLE;
 PL18X_VERSION_DATA_NOMADIK.DataLengthBits:=24;
 PL18X_VERSION_DATA_NOMADIK.FIFOSize:=16 * 4;
 PL18X_VERSION_DATA_NOMADIK.FIFOHalfSize:=8 * 4;
 PL18X_VERSION_DATA_NOMADIK.DataControlMaskSDIO:=PL18X_MMCI_ST_DPSM_SDIOEN;
 PL18X_VERSION_DATA_NOMADIK.STSDIO:=True;
 PL18X_VERSION_DATA_NOMADIK.STClockDivider:=True;
 PL18X_VERSION_DATA_NOMADIK.PowerPowerUp:=PL18X_MMCI_POWER_ON;
 PL18X_VERSION_DATA_NOMADIK.ClockMaximum:=100000000;
 PL18X_VERSION_DATA_NOMADIK.SignalDirection:=True;
 PL18X_VERSION_DATA_NOMADIK.PowerClockGate:=True;
 PL18X_VERSION_DATA_NOMADIK.PowerNoPower:=True;
 
 PL18X_VERSION_DATA_UX500.Name:='ST UX500';
 PL18X_VERSION_DATA_UX500.ClockRegister:=PL18X_MMCI_CLOCK_ENABLE;
 PL18X_VERSION_DATA_UX500.ClockEnable:=PL18X_MMCI_CLOCK_ST_UX500_HWFCEN;
 PL18X_VERSION_DATA_UX500.Clock8BitEnable:=PL18X_MMCI_CLOCK_ST_8BIT_BUS;
 PL18X_VERSION_DATA_UX500.ClockNegativeEdgeEnable:=PL18X_MMCI_CLOCK_ST_UX500_NEG_EDGE;
 PL18X_VERSION_DATA_UX500.DataLengthBits:=24;
 PL18X_VERSION_DATA_UX500.FIFOSize:=30 * 4;
 PL18X_VERSION_DATA_UX500.FIFOHalfSize:=8 * 4;
 PL18X_VERSION_DATA_UX500.DataControlMaskSDIO:=PL18X_MMCI_ST_DPSM_SDIOEN;
 PL18X_VERSION_DATA_UX500.STSDIO:=True;
 PL18X_VERSION_DATA_UX500.STClockDivider:=True;
 PL18X_VERSION_DATA_UX500.PowerPowerUp:=PL18X_MMCI_POWER_ON;
 PL18X_VERSION_DATA_UX500.ClockMaximum:=100000000;
 PL18X_VERSION_DATA_UX500.SignalDirection:=True;
 PL18X_VERSION_DATA_UX500.PowerClockGate:=True;
 PL18X_VERSION_DATA_UX500.BusyDetect:=True;
 PL18X_VERSION_DATA_UX500.PowerNoPower:=True;
 
 PL18X_VERSION_DATA_UX500V2.Name:='ST UX500V2';
 PL18X_VERSION_DATA_UX500V2.ClockRegister:=PL18X_MMCI_CLOCK_ENABLE;
 PL18X_VERSION_DATA_UX500V2.ClockEnable:=PL18X_MMCI_CLOCK_ST_UX500_HWFCEN;
 PL18X_VERSION_DATA_UX500V2.Clock8BitEnable:=PL18X_MMCI_CLOCK_ST_8BIT_BUS;
 PL18X_VERSION_DATA_UX500V2.ClockNegativeEdgeEnable:=PL18X_MMCI_CLOCK_ST_UX500_NEG_EDGE;
 PL18X_VERSION_DATA_UX500V2.DataLengthBits:=24;
 PL18X_VERSION_DATA_UX500V2.FIFOSize:=30 * 4;
 PL18X_VERSION_DATA_UX500V2.FIFOHalfSize:=8 * 4;
 PL18X_VERSION_DATA_UX500V2.DataControlMaskDDR:=PL18X_MMCI_ST_DPSM_DDRMODE;
 PL18X_VERSION_DATA_UX500V2.DataControlMaskSDIO:=PL18X_MMCI_ST_DPSM_SDIOEN;
 PL18X_VERSION_DATA_UX500V2.STSDIO:=True;
 PL18X_VERSION_DATA_UX500V2.STClockDivider:=True;
 PL18X_VERSION_DATA_UX500V2.BlockSizeDataControl16:=True;
 PL18X_VERSION_DATA_UX500V2.PowerPowerUp:=PL18X_MMCI_POWER_ON;
 PL18X_VERSION_DATA_UX500V2.ClockMaximum:=100000000;
 PL18X_VERSION_DATA_UX500V2.SignalDirection:=True;
 PL18X_VERSION_DATA_UX500V2.PowerClockGate:=True;
 PL18X_VERSION_DATA_UX500V2.BusyDetect:=True;
 PL18X_VERSION_DATA_UX500V2.PowerNoPower:=True;
 
 PL18X_VERSION_DATA_QCOM.Name:='Qualcomm';
 PL18X_VERSION_DATA_QCOM.ClockRegister:=PL18X_MMCI_CLOCK_ENABLE;
 PL18X_VERSION_DATA_QCOM.ClockEnable:=PL18X_MMCI_CLOCK_QCOM_FLOWENA or PL18X_MMCI_CLOCK_QCOM_SELECT_IN_FBCLK;
 PL18X_VERSION_DATA_QCOM.Clock8BitEnable:=PL18X_MMCI_CLOCK_QCOM_WIDEBUS_8;
 PL18X_VERSION_DATA_QCOM.DataLengthBits:=24;
 PL18X_VERSION_DATA_QCOM.FIFOSize:=16 * 4;
 PL18X_VERSION_DATA_QCOM.FIFOHalfSize:=8 * 4;
 PL18X_VERSION_DATA_QCOM.DataCommandEnable:=PL18X_MMCI_CPSM_QCOM_DATCMD;
 PL18X_VERSION_DATA_QCOM.DataControlMaskDDR:=PL18X_MMCI_CLOCK_QCOM_SELECT_IN_DDR_MODE;
 PL18X_VERSION_DATA_QCOM.BlockSizeDataControl4:=True;
 PL18X_VERSION_DATA_QCOM.PowerPowerUp:=PL18X_MMCI_POWER_UP;
 PL18X_VERSION_DATA_QCOM.ClockMaximum:=208000000;
 PL18X_VERSION_DATA_QCOM.ExplicitMClockControl:=True;
 PL18X_VERSION_DATA_QCOM.QualcommFIFO:=True;
 PL18X_VERSION_DATA_QCOM.QualcommDMA:=True;
 
 PL18XInitialized:=True;
end;
 
{==============================================================================}
{==============================================================================}
{PL18X Functions}
function PL180SDHCICreate(Address:PtrUInt;const Name:String;IRQ0,IRQ1,ClockMinimum,ClockMaximum:LongWord;CardDetect:TMMCDeviceGetCardDetect;WriteProtect:TMMCDeviceGetWriteProtect):PSDHCIHost;
{Create and register a new PL180 SDHCI device which can be accessed using the SDHCI API}
{Address: The address of the PL180 registers}
{Name: The text description of this device which will show in the device list (Optional)}
{IRQ0: The interrupt 0 number for the PL180}
{IRQ1: The interrupt 1 number for the PL180}
{ClockMinimum: The minimum frequency for the Pl180 clock}
{ClockMaximum: The maximum frequency for the Pl180 clock}
{Return: Pointer to the new SDHCI device or nil if the SDHCI device could not be created}
var
 Status:LongWord;
 PL18XSDHCI:PPL18XSDHCIHost;
begin
 {}
 Result:=nil;
 
 {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'PL180: SDHCI Create (Address=' + AddrToHex(Address) + ' Name=' + Name + ' IRQ0=' + IntToStr(IRQ0) + ' IRQ1=' + IntToStr(IRQ1) + ')');
 {$ENDIF}
 
 {Check Address}
 if Address = 0 then Exit;
 
 {Check IRQ0}
 {if IRQ0 = 0 then Exit;} {IRQ 0 is valid}

 {Check IRQ1}
 {if IRQ1 = 0 then Exit;} {IRQ 0 is valid}
 
 {Check Clock Minimum}
 if ClockMinimum = 0 then ClockMinimum:=400000;
 
 {Create SDHCI}
 PL18XSDHCI:=PPL18XSDHCIHost(SDHCIHostCreateEx(SizeOf(TPL18XSDHCIHost)));
 if PL18XSDHCI <> nil then
  begin
   {Update SDHCI}
   {Device}
   PL18XSDHCI.SDHCI.Device.DeviceBus:=DEVICE_BUS_MMIO; 
   PL18XSDHCI.SDHCI.Device.DeviceType:=SDHCI_TYPE_MMCI;
   PL18XSDHCI.SDHCI.Device.DeviceFlags:=SDHCI_FLAG_NON_STANDARD or SDHCI_FLAG_AUTO_CMD12;
   PL18XSDHCI.SDHCI.Device.DeviceData:=nil;
   if Length(Name) <> 0 then PL18XSDHCI.SDHCI.Device.DeviceDescription:=Name else PL18XSDHCI.SDHCI.Device.DeviceDescription:=PL180_MMCI_DESCRIPTION;
   {SDHCI}
   PL18XSDHCI.SDHCI.SDHCIState:=SDHCI_STATE_DISABLED;
   PL18XSDHCI.SDHCI.HostStart:=PL18XSDHCIHostStart;
   PL18XSDHCI.SDHCI.HostStop:=PL18XSDHCIHostStop;
   PL18XSDHCI.SDHCI.HostReadByte:=PL18XSDHCIHostReadByte;
   PL18XSDHCI.SDHCI.HostReadWord:=PL18XSDHCIHostReadWord;
   PL18XSDHCI.SDHCI.HostReadLong:=PL18XSDHCIHostReadLong;
   PL18XSDHCI.SDHCI.HostWriteByte:=PL18XSDHCIHostWriteByte;
   PL18XSDHCI.SDHCI.HostWriteWord:=PL18XSDHCIHostWriteWord;
   PL18XSDHCI.SDHCI.HostWriteLong:=PL18XSDHCIHostWriteLong;
   PL18XSDHCI.SDHCI.HostSetClockDivider:=nil;
   PL18XSDHCI.SDHCI.HostSetControlRegister:=nil;
   PL18XSDHCI.SDHCI.DeviceInitialize:=PL18XMMCDeviceInitialize;
   PL18XSDHCI.SDHCI.DeviceDeinitialize:=nil;
   if Assigned(CardDetect) then PL18XSDHCI.SDHCI.DeviceGetCardDetect:=CardDetect else PL18XSDHCI.SDHCI.DeviceGetCardDetect:=PL18XMMCDeviceGetCardDetect;
   if Assigned(WriteProtect) then PL18XSDHCI.SDHCI.DeviceGetWriteProtect:=WriteProtect else PL18XSDHCI.SDHCI.DeviceGetWriteProtect:=PL18XMMCDeviceGetWriteProtect;
   PL18XSDHCI.SDHCI.DeviceSendCommand:=PL18XMMCDeviceSendCommand;
   PL18XSDHCI.SDHCI.DeviceSetIOS:=PL18XMMCDeviceSetIOS;
   {Driver}
   PL18XSDHCI.SDHCI.Address:=Pointer(Address);
   {Configuration}
   PL18XSDHCI.SDHCI.ClockMinimum:=ClockMinimum;
   PL18XSDHCI.SDHCI.ClockMaximum:=ClockMaximum;
   {Pl18X}
   PL18XSDHCI.IRQ0:=IRQ0;
   PL18XSDHCI.IRQ1:=IRQ1;
   PL18XSDHCI.SingleIRQ:=False;
   PL18XSDHCI.Registers:=PPL18XMMCIRegisters(Address);
   PL18XSDHCI.Version:=PL18XGetVersionData(PL18XSDHCI);
 
   {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
   if MMC_LOG_ENABLED then MMCLogDebug(nil,'PL180: Version = ' + PL18XSDHCI.Version.Name);
   {$ENDIF}
 
   {Register SDHCI}
   Status:=SDHCIHostRegister(@PL18XSDHCI.SDHCI);
   if Status <> ERROR_SUCCESS then
    begin
     if MMC_LOG_ENABLED then MMCLogError(nil,'PL180: Failed to register new SDHCI host: ' + ErrorToString(Status));
    end;
  end
 else 
  begin
   if MMC_LOG_ENABLED then MMCLogError(nil,'PL180: Failed to create new SDHCI host');
  end;
end;

{==============================================================================}

function PL181SDHCICreate(Address:PtrUInt;const Name:String;IRQ0,IRQ1,ClockMinimum,ClockMaximum:LongWord;CardDetect:TMMCDeviceGetCardDetect;WriteProtect:TMMCDeviceGetWriteProtect):PSDHCIHost;
{Create and register a new PL181 SDHCI device which can be accessed using the SDHCI API}
{Address: The address of the PL181 registers}
{Name: The text description of this device which will show in the device list (Optional)}
{IRQ0: The interrupt 0 number for the PL181}
{IRQ1: The interrupt 1 number for the PL181}
{ClockMinimum: The minimum frequency for the Pl181 clock}
{ClockMaximum: The maximum frequency for the Pl181 clock}
{Return: Pointer to the new SDHCI device or nil if the SDHCI device could not be created}
var
 Status:LongWord;
 PL18XSDHCI:PPL18XSDHCIHost;
begin
 {}
 Result:=nil;
 
 {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'PL181: SDHCI Create (Address=' + AddrToHex(Address) + ' Name=' + Name + ' IRQ0=' + IntToStr(IRQ0) + ' IRQ1=' + IntToStr(IRQ1) + ')');
 {$ENDIF}
 
 {Check Address}
 if Address = 0 then Exit;
 
 {Check IRQ0}
 {if IRQ0 = 0 then Exit;} {IRQ 0 is valid}

 {Check IRQ1}
 {if IRQ1 = 0 then Exit;} {IRQ 0 is valid}
 
 {Check Clock Minimum}
 if ClockMinimum = 0 then ClockMinimum:=400000;
 
 {Create SDHCI}
 PL18XSDHCI:=PPL18XSDHCIHost(SDHCIHostCreateEx(SizeOf(TPL18XSDHCIHost)));
 if PL18XSDHCI <> nil then
  begin
   {Update SDHCI}
   {Device}
   PL18XSDHCI.SDHCI.Device.DeviceBus:=DEVICE_BUS_MMIO; 
   PL18XSDHCI.SDHCI.Device.DeviceType:=SDHCI_TYPE_MMCI;
   PL18XSDHCI.SDHCI.Device.DeviceFlags:=SDHCI_FLAG_NON_STANDARD or SDHCI_FLAG_AUTO_CMD12;
   PL18XSDHCI.SDHCI.Device.DeviceData:=nil;
   if Length(Name) <> 0 then PL18XSDHCI.SDHCI.Device.DeviceDescription:=Name else PL18XSDHCI.SDHCI.Device.DeviceDescription:=PL181_MMCI_DESCRIPTION;
   {SDHCI}
   PL18XSDHCI.SDHCI.SDHCIState:=SDHCI_STATE_DISABLED;
   PL18XSDHCI.SDHCI.HostStart:=PL18XSDHCIHostStart;
   PL18XSDHCI.SDHCI.HostStop:=PL18XSDHCIHostStop;
   PL18XSDHCI.SDHCI.HostReadByte:=PL18XSDHCIHostReadByte;
   PL18XSDHCI.SDHCI.HostReadWord:=PL18XSDHCIHostReadWord;
   PL18XSDHCI.SDHCI.HostReadLong:=PL18XSDHCIHostReadLong;
   PL18XSDHCI.SDHCI.HostWriteByte:=PL18XSDHCIHostWriteByte;
   PL18XSDHCI.SDHCI.HostWriteWord:=PL18XSDHCIHostWriteWord;
   PL18XSDHCI.SDHCI.HostWriteLong:=PL18XSDHCIHostWriteLong;
   PL18XSDHCI.SDHCI.HostSetClockDivider:=nil;
   PL18XSDHCI.SDHCI.HostSetControlRegister:=nil;
   PL18XSDHCI.SDHCI.DeviceInitialize:=PL18XMMCDeviceInitialize;
   PL18XSDHCI.SDHCI.DeviceDeinitialize:=nil;
   if Assigned(CardDetect) then PL18XSDHCI.SDHCI.DeviceGetCardDetect:=CardDetect else PL18XSDHCI.SDHCI.DeviceGetCardDetect:=PL18XMMCDeviceGetCardDetect;
   if Assigned(WriteProtect) then PL18XSDHCI.SDHCI.DeviceGetWriteProtect:=WriteProtect else PL18XSDHCI.SDHCI.DeviceGetWriteProtect:=PL18XMMCDeviceGetWriteProtect;
   PL18XSDHCI.SDHCI.DeviceSendCommand:=PL18XMMCDeviceSendCommand;
   PL18XSDHCI.SDHCI.DeviceSetIOS:=PL18XMMCDeviceSetIOS;
   {Driver}
   PL18XSDHCI.SDHCI.Address:=Pointer(Address);
   {Configuration}
   PL18XSDHCI.SDHCI.ClockMinimum:=ClockMinimum;
   PL18XSDHCI.SDHCI.ClockMaximum:=ClockMaximum;
   {Pl18X}
   PL18XSDHCI.IRQ0:=IRQ0;
   PL18XSDHCI.IRQ1:=IRQ1;
   PL18XSDHCI.SingleIRQ:=False;
   PL18XSDHCI.Registers:=PPL18XMMCIRegisters(Address);
   PL18XSDHCI.Version:=PL18XGetVersionData(PL18XSDHCI);
 
   {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
   if MMC_LOG_ENABLED then MMCLogDebug(nil,'PL181: Version = ' + PL18XSDHCI.Version.Name);
   {$ENDIF}
 
   {Register SDHCI}
   Status:=SDHCIHostRegister(@PL18XSDHCI.SDHCI);
   if Status <> ERROR_SUCCESS then
    begin
     if MMC_LOG_ENABLED then MMCLogError(nil,'PL181: Failed to register new SDHCI host: ' + ErrorToString(Status));
    end;
  end
 else 
  begin
   if MMC_LOG_ENABLED then MMCLogError(nil,'PL181: Failed to create new SDHCI host');
  end;
end;

{==============================================================================}

function PL18XSDHCIDestroy(SDHCI:PSDHCIHost):LongWord;
{Stop, deregister and destroy a PL18X SDHCI device created by this driver}
{SDHCI: The SDHCI device to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check SDHCI}
 if SDHCI = nil then Exit;
 
 {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'PL18X: SDHCI Destroy');
 {$ENDIF}
 
 {Stop SDHCI}
 Result:=SDHCIHostStop(SDHCI);
 if Result = ERROR_SUCCESS then
  begin
   {Deregister SDHCI}
   Result:=SDHCIHostDeregister(SDHCI);
   if Result = ERROR_SUCCESS then
    begin
     {Destroy SDHCI}
     Result:=SDHCIHostDestroy(SDHCI);
     if Result <> ERROR_SUCCESS then
      begin
       if MMC_LOG_ENABLED then MMCLogError(nil,'PL18X: Failed to destroy SDHCI host: ' + ErrorToString(Result));
      end;
    end
   else
    begin
     if MMC_LOG_ENABLED then MMCLogError(nil,'PL18X: Failed to deregister SDHCI host: ' + ErrorToString(Result));
    end;    
  end
 else
  begin
   if MMC_LOG_ENABLED then MMCLogError(nil,'PL18X: Failed to stop SDHCI host: ' + ErrorToString(Result));
  end;  
end;

{==============================================================================}
{==============================================================================}
{PL18X MMC Functions}
function PL18XMMCDeviceInitialize(MMC:PMMCDevice):LongWord;
{Implementation of MMCDeviceInitialize API for PL18X SDHCI}
{Note: Not intended to be called directly by applications, use MMCDeviceInitialize instead}
var
 SDHCI:PSDHCIHost;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;
 
 {Check MMC}
 if MMC = nil then Exit;

 {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'PL18X: MMC Device Initialize');
 {$ENDIF}
 
 {Get SDHCI}
 SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
 if SDHCI = nil then Exit;
 
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
 PL18XSetPowerRegister(PPL18XSDHCIHost(SDHCI),FirstBitSet(SDHCI.Voltages) - 1);
 
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
   
   {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
   if MMC_LOG_ENABLED then MMCLogDebug(nil,'PL18X: MMC Initialize Card Type is SDIO');
   {$ENDIF}
 
   {Get the Operation Condition}
   Result:=SDIODeviceSendOperationCondition(MMC,False);
   if Result <> MMC_STATUS_SUCCESS then
    begin
     Exit;
    end;
   
   //To Do //See MMC

   {Update Storage}
   MMC.Storage.Device.DeviceBus:=DEVICE_BUS_SD;
   //To Do //See MMC
   
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
   
   {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
   if MMC_LOG_ENABLED then MMCLogDebug(nil,'PL18X: MMC Initialize Card Type is SD');
   {$ENDIF}

   {Get Card Identification}
   Result:=SDDeviceGetCardIdentification(MMC);
   if Result <> MMC_STATUS_SUCCESS then
    begin
     Exit;
    end;      
    
   {Get Relative Address}
   Result:=SDDeviceSendRelativeAddress(MMC);
   if Result <> MMC_STATUS_SUCCESS then
    begin
     Exit;
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
   
   {Select Card}
   Result:=MMCDeviceSelectCard(MMC);
   if Result <> MMC_STATUS_SUCCESS then
    begin
     Exit;
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
   
   {Check Write Protect}
   Result:=MMCDeviceGetWriteProtect(MMC);
   if Result <> MMC_STATUS_SUCCESS then
    begin
     Exit;
    end;

   //To Do //Check for UHS-I and do UHS-I init //See MMC
   
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
   
   {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
   if MMC_LOG_ENABLED then MMCLogDebug(nil,'PL18X: MMC Initialize Card Type is MMC');
   {$ENDIF}
   
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
   
   {Select Card}
   Result:=MMCDeviceSelectCard(MMC);
   if Result <> MMC_STATUS_SUCCESS then
    begin
     Exit;
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
   
   //To Do //See: mmc_init_card etc //See MMC

   //SetClock/SetBusWidth etc
   
   {Update Storage}
   MMC.Storage.Device.DeviceBus:=DEVICE_BUS_MMC;
   //To Do //See MMC
   
   Result:=MMC_STATUS_SUCCESS;
   Exit;
  end;
  
 {Return Result}
 Result:=MMC_STATUS_NO_MEDIA;
end;

{==============================================================================}

function PL18XMMCDeviceGetCardDetect(MMC:PMMCDevice):LongWord;
{Implementation of MMCDeviceGetCardDetect API for PL18X SDHCI}
{Note: Not intended to be called directly by applications, use MMCDeviceGetCardDetect instead}
var
 Mask:LongWord;
 SDHCI:PSDHCIHost;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;
 
 {Check MMC}
 if MMC = nil then Exit;

 {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'PL18X: MMC Device Get Card Detect');
 {$ENDIF}
 
 {Get SDHCI}
 SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
 if SDHCI = nil then Exit;
 
 {Check MMC State}
 if MMC.MMCState = MMC_STATE_INSERTED then
  begin
   {Get Card Status}
   if MMCDeviceSendCardStatus(MMC) <> MMC_STATUS_SUCCESS then
    begin
     {Update Flags}
     MMC.Device.DeviceFlags:=MMC.Device.DeviceFlags and not(MMC_FLAG_CARD_PRESENT);

     {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'PL18X: Get Card Detect (Flags=not MMC_FLAG_CARD_PRESENT)');
     {$ENDIF}
    end;
  end
 else
  begin
   {Check Flags}
   if (MMC.Device.DeviceFlags and MMC_FLAG_CARD_PRESENT) = 0 then
    begin
     {Update Flags}
     MMC.Device.DeviceFlags:=(MMC.Device.DeviceFlags or MMC_FLAG_CARD_PRESENT);
     
     {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'PL18X: Get Card Detect (Flags=MMC_FLAG_CARD_PRESENT)');
     {$ENDIF}
    end; 
  end; 
 
 Result:=MMC_STATUS_SUCCESS; 
end;

{==============================================================================}

function PL18XMMCDeviceGetWriteProtect(MMC:PMMCDevice):LongWord;
{Implementation of MMCDeviceGetWriteProtect API for PL18X SDHCI}
{Note: Not intended to be called directly by applications, use MMCDeviceGetWriteProtect instead}
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;
 
 {Check MMC}
 if MMC = nil then Exit;

 {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'PL18X: MMC Device Get Write Protect');
 {$ENDIF}
 
 Result:=MMC_STATUS_SUCCESS;
end;

{==============================================================================}

function PL18XMMCDeviceSendCommand(MMC:PMMCDevice;Command:PMMCCommand):LongWord; 
{Implementation of MMCDeviceSendCommand API for PL18X SDHCI}
{Note: Not intended to be called directly by applications, use MMCDeviceSendCommand instead}
var
 Size:LongWord;
 Value:LongWord;
 Status:LongWord;
 Control:LongWord;
 Timeout:LongWord;
 IntMask:LongWord;
 SizeBit:LongWord;
 SDHCI:PSDHCIHost;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;
 
 {Check MMC}
 if MMC = nil then Exit;

 {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'PL18X: MMC Device Send Command');
 {$ENDIF}
 
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
    
    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}
    
    {Check Command}
    if (PPL18XSDHCIHost(SDHCI).Registers.Command and PL18X_MMCI_CPSM_ENABLE) <> 0 then
     begin
      {Reset Command}
      PPL18XSDHCIHost(SDHCI).Registers.Command:=0;
      
      {Delay}
      PL18XRegisterDelay(PPL18XSDHCIHost(SDHCI));
     end;
    
    {Setup Command}
    Value:=Command.Command or PL18X_MMCI_CPSM_ENABLE;
    
    {Setup Response}
    if (Command.ResponseType and MMC_RSP_PRESENT) <> 0 then
     begin
      Value:=Value or PL18X_MMCI_CPSM_RESPONSE;
      
      if (Command.ResponseType and MMC_RSP_136) <> 0 then
       begin
        Value:=Value or PL18X_MMCI_CPSM_LONGRSP;
       end;
     end;
     
    //if Command.Command = MMC_CMD_ADTC then //To Do
    // begin
    //  Value:=Value or PPL18XSDHCIHost(SDHCI).Version.DataCommandEnable;
    // end;
    
    {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
    if MMC_LOG_ENABLED then MMCLogDebug(nil,'PL18X: MMC Send Command (Command=' + IntToHex(Value,8) + ')');
    {$ENDIF}
    
    {Check Data}
    if Command.Data = nil then
     begin
      {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
      if MMC_LOG_ENABLED then MMCLogDebug(nil,'PL18X: MMC Send Command (Argument=' + IntToHex(Command.Argument,8) + ')');
      {$ENDIF}
      
      {Write Argument}
      PPL18XSDHCIHost(SDHCI).Registers.Argument:=Command.Argument;
      
      {Setup Interrupt Mask}
      IntMask:=0;
     end
    else 
     begin
      {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
      if MMC_LOG_ENABLED then MMCLogDebug(nil,'PL18X: MMC Send Command (BlockSize=' + IntToStr(Command.Data.BlockSize) + ')');
      if MMC_LOG_ENABLED then MMCLogDebug(nil,'PL18X: MMC Send Command (BlockCount=' + IntToStr(Command.Data.BlockCount) + ')');
      {$ENDIF}
      
      {Setup Data}
      Command.Data.BlockOffset:=0;
      Command.Data.BytesRemaining:=Command.Data.BlockSize * Command.Data.BlockCount;
      Command.Data.BytesTransfered:=0;
      
      {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
      if MMC_LOG_ENABLED then MMCLogDebug(nil,'PL18X: MMC Send Command (Argument=' + IntToHex(Command.Argument,8) + ')');
      {$ENDIF}
      
      {Write Argument}
      PPL18XSDHCIHost(SDHCI).Registers.Argument:=Command.Argument;
      
      {Setup Data Timer}
      Timeout:=0; //To Do //mmci_start_data //Clock div NANOSECONDS_PER_SECOND //Not implemented in QEMU
      {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
      if MMC_LOG_ENABLED then MMCLogDebug(nil,'PL18X: MMC Send Command (Data Timer=' + IntToStr(Timeout) + ')');
      {$ENDIF}
      
      {Write Data Timer}
      PPL18XSDHCIHost(SDHCI).Registers.DataTimer:=Timeout;
      
      {Setup Data Length}
      Size:=Command.Data.BlockSize * Command.Data.BlockCount;
      {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
      if MMC_LOG_ENABLED then MMCLogDebug(nil,'PL18X: MMC Send Command (Data Length=' + IntToStr(Size) + ')');
      {$ENDIF}
      
      {Write Data Length}
      PPL18XSDHCIHost(SDHCI).Registers.DataLength:=Size;
      
      {Setup Data Control}
      if PPL18XSDHCIHost(SDHCI).Version.BlockSizeDataControl16 then
       begin
        Control:=PL18X_MMCI_DPSM_ENABLE or (Command.Data.BlockSize shl 16);
       end
      else if PPL18XSDHCIHost(SDHCI).Version.BlockSizeDataControl4 then 
       begin
        Control:=PL18X_MMCI_DPSM_ENABLE or (Command.Data.BlockSize shl 4);
       end
      else
       begin
        {Get Block Size Bit}
        SizeBit:=FirstBitSet(Command.Data.BlockSize);
        {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
        if MMC_LOG_ENABLED then MMCLogDebug(nil,'PL18X: MMC Send Command (Block Size Bit=' + IntToStr(SizeBit) + ')');
        {$ENDIF}
        
        Control:=PL18X_MMCI_DPSM_ENABLE or (SizeBit shl 4);
       end;       
      if (Command.Data.Flags and MMC_DATA_READ) <> 0 then
       begin
        Control:=Control or PL18X_MMCI_DPSM_DIRECTION;
       end;
      //To Do //mmc_card_sdio //Not implemented in QEMU
      if (MMC.Timing = MMC_TIMING_UHS_DDR50) or (MMC.Timing = MMC_TIMING_MMC_DDR52) then
       begin
        Control:=Control or PPL18XSDHCIHost(SDHCI).Version.DataControlMaskDDR;
       end;
      {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
      if MMC_LOG_ENABLED then MMCLogDebug(nil,'PL18X: MMC Send Command (Data Control=' + IntToHex(Control,8) + ')');
      {$ENDIF}
      
      {Setup DMA Transfer}
      //To Do //mmci_dma_start_data
      
      {Setup Interrupt Mask}
      if (Command.Data.Flags and MMC_DATA_READ) <> 0 then
       begin
        IntMask:=PL18X_MMCI_RXFIFOHALFFULLMASK;
        
        if Size < PPL18XSDHCIHost(SDHCI).Version.FIFOHalfSize then
         begin
          IntMask:=IntMask or PL18X_MMCI_RXDATAAVLBLMASK;
         end;
       end
      else
       begin
        IntMask:=PL18X_MMCI_TXFIFOHALFEMPTYMASK;
       end;       
      {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
      if MMC_LOG_ENABLED then MMCLogDebug(nil,'PL18X: MMC Send Command (Interrupt Mask=' + IntToHex(IntMask,8) + ')');
      {$ENDIF}
      
      {Write Data Control}
      PL18XWriteDataCtrlRegister(PPL18XSDHCIHost(SDHCI),Control);
     end;     
    
    {Setup Command}
    SDHCI.Command:=Command;
    try
     {Acquire the Lock}
     if PL18X_MMCI_FIQ_ENABLED then
      begin
       if SpinLockIRQFIQ(PPL18XSDHCIHost(SDHCI).Lock) <> ERROR_SUCCESS then Exit;
      end
     else
      begin
       if SpinLockIRQ(PPL18XSDHCIHost(SDHCI).Lock) <> ERROR_SUCCESS then Exit;
      end;  
     
     {Write Interrupt Mask}
     PPL18XSDHCIHost(SDHCI).Registers.Mask0:=PPL18XSDHCIHost(SDHCI).Registers.Mask0 and not(PL18X_MMCI_DATAENDMASK);
     PL18XSetInterruptMask1(PPL18XSDHCIHost(SDHCI),IntMask);
     
     {Write Command}
     PPL18XSDHCIHost(SDHCI).Registers.Command:=Value;
     
     {Memory Barrier}
     DataMemoryBarrier; {After the Last Read} 

     {Release the Lock}
     if PL18X_MMCI_FIQ_ENABLED then
      begin
       SpinUnlockIRQFIQ(PPL18XSDHCIHost(SDHCI).Lock);
      end
     else
      begin
       SpinUnlockIRQ(PPL18XSDHCIHost(SDHCI).Lock);
      end;     
      
     {Wait for Completion}
     if Command.Data = nil then
      begin
       {Wait for Signal with Timeout (100ms)}
       Status:=SemaphoreWaitEx(SDHCI.Wait,100);
       if Status <> ERROR_SUCCESS then
        begin
         if Status = ERROR_WAIT_TIMEOUT then
          begin
           if MMC_LOG_ENABLED then MMCLogError(nil,'PL18X: MMC Send Command Response Timeout');
           Command.Status:=MMC_STATUS_TIMEOUT;
           Exit; 
          end
         else
          begin
           if MMC_LOG_ENABLED then MMCLogError(nil,'PL18X: MMC Send Command Response Failure');
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
           if MMC_LOG_ENABLED then MMCLogError(nil,'PL18X: MMC Send Data Response Timeout');
           Command.Status:=MMC_STATUS_TIMEOUT;
           Exit; 
          end
         else
          begin
           if MMC_LOG_ENABLED then MMCLogError(nil,'PL18X: MMC Send Data Response Failure');
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
    {Release the Lock}
    MutexUnlock(MMC.Lock);
   end;
  end;

 {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'PL18X: MMC Send Command completed: ' + MMCStatusToString(Command.Status));
 {$ENDIF}
 if Command.Status = MMC_STATUS_SUCCESS then Result:=MMC_STATUS_SUCCESS;
end;

{==============================================================================}

function PL18XMMCDeviceSetIOS(MMC:PMMCDevice):LongWord;
{Implementation of MMCDeviceSetIOS API for PL18X SDHCI}
{Note: Not intended to be called directly by applications, use MMCDeviceSetIOS instead}
var
 Power:LongWord;
 SDHCI:PSDHCIHost;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;
 
 {Check MMC}
 if MMC = nil then Exit;

 {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'PL18X: MMC Device Set IOS');
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'PL18X: MMC Set IOS (Clock=' + IntToStr(MMC.Clock) + ')');
 {$ENDIF}
 
 {Get SDHCI}
 SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
 if SDHCI = nil then Exit;
 
 {Set Power}
 Power:=PL18X_MMCI_POWER_ON;
 
 {Write Registers}
 PL18XSetClockRegister(PPL18XSDHCIHost(SDHCI),MMC,MMC.Clock);
 PL18XWritePowerRegister(PPL18XSDHCIHost(SDHCI),Power);

 {Delay}
 PL18XRegisterDelay(PPL18XSDHCIHost(SDHCI));
 
 Result:=MMC_STATUS_SUCCESS;
end;

{==============================================================================}
{==============================================================================}
{PL18X SDHCI Functions}
function PL18XSDHCIHostStart(SDHCI:PSDHCIHost):LongWord;
{Implementation of SDHCIHostStart API for PL18X SDHCI}
{Note: Not intended to be called directly by applications, use SDHCIHostStart instead}
var
 MMC:PMMCDevice;
 Status:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check SDHCI}
 if SDHCI = nil then Exit;
 
 {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'PL18X: SDHCI Host Start');
 {$ENDIF}
 
 {Update SDHCI}
 {Driver Properties}
 if PL18X_MMCI_FIQ_ENABLED then
  begin
   SDHCI.Wait:=SemaphoreCreateEx(0,SEMAPHORE_DEFAULT_MAXIMUM,SEMAPHORE_FLAG_IRQFIQ);
  end
 else
  begin
   SDHCI.Wait:=SemaphoreCreateEx(0,SEMAPHORE_DEFAULT_MAXIMUM,SEMAPHORE_FLAG_IRQ);
  end;  
 SDHCI.Quirks:=SDHCI_QUIRK_NO_HISPD_BIT or SDHCI_QUIRK_MISSING_CAPS;
 {Configuration Properties}
 SDHCI.PresetVoltages:=MMC_VDD_32_33 or MMC_VDD_33_34;
 SDHCI.PresetCapabilities:=0;
 {PL18X Properties}
 if PPL18XSDHCIHost(SDHCI).Version.QualcommFIFO then
  begin
   PPL18XSDHCIHost(SDHCI).GetRXFIFOCount:=PL18XQualcommGetRXFIFOCount;
  end
 else
  begin
   PPL18XSDHCIHost(SDHCI).GetRXFIFOCount:=PL18XGetRXFIFOCount;
  end;  
 
 {Get Clock Maximum}
 SDHCI.MaximumFrequency:=SDHCI.ClockMaximum;
 if SDHCI.MaximumFrequency > PPL18XSDHCIHost(SDHCI).Version.ClockMaximum then
  begin
   SDHCI.ClockMaximum:=PPL18XSDHCIHost(SDHCI).Version.ClockMaximum;
  end;
 
 {Get Clock Minimum}
 if PPL18XSDHCIHost(SDHCI).Version.STClockDivider then 
  begin
   {The ARM and ST versions of the block have slightly different clock divider equations which means that the minimum divider differs too}
   SDHCI.MinimumFrequency:=DivRoundUp(SDHCI.ClockMaximum,257);
  end
 else if PPL18XSDHCIHost(SDHCI).Version.ExplicitMClockControl then 
  begin
   {On Qualcomm like controllers get the nearest minimum clock to 100Khz}
   SDHCI.MinimumFrequency:=100000;
  end
 else
  begin
   SDHCI.MinimumFrequency:=DivRoundUp(SDHCI.ClockMaximum,512);
  end;
   
 if SDHCI.MaximumFrequency > 0 then
  begin
   if PPL18XSDHCIHost(SDHCI).Version.ExplicitMClockControl then 
    begin
     SDHCI.MaximumFrequency:=Min(PPL18XSDHCIHost(SDHCI).Version.ClockMaximum,SDHCI.MaximumFrequency);
    end
   else
    begin
     SDHCI.MaximumFrequency:=Min(SDHCI.ClockMaximum,SDHCI.MaximumFrequency);
    end;    
  end
 else
  begin
   SDHCI.MaximumFrequency:=SDHCI.ClockMinimum;
  end;
  
 {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'PL18X: ClockMinimum = ' + IntToStr(SDHCI.ClockMinimum) + ' ClockMaximum = ' + IntToStr(SDHCI.ClockMaximum));
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'PL18X: MinimumFrequency = ' + IntToStr(SDHCI.MinimumFrequency) + ' MaximumFrequency = ' + IntToStr(SDHCI.MaximumFrequency));
 {$ENDIF}
 
 {Get Voltages}
 SDHCI.Voltages:=SDHCI.PresetVoltages;
 
 {Get Capabilities}
 SDHCI.Capabilities:=0; //MMC_MODE_HS or MMC_MODE_HS_52MHz or MMC_MODE_4BIT; //To Do //Not supported in QEMU ?
 
 {Determine Request Size}
 PPL18XSDHCIHost(SDHCI).MaximumRequestSize:=(1 shl PPL18XSDHCIHost(SDHCI).Version.DataLengthBits) - 1;
 
 {Determine Block Size}
 PPL18XSDHCIHost(SDHCI).MaximumBlockSize:=(1 shl 11);
 
 {Determine Block Count}
 SDHCI.MaximumBlockCount:=(PPL18XSDHCIHost(SDHCI).MaximumRequestSize shr 11);
 
 {Create the Lock}
 PPL18XSDHCIHost(SDHCI).Lock:=SpinCreate;
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Reset Interrupts}
 PPL18XSDHCIHost(SDHCI).Registers.Mask0:=0;
 PPL18XSDHCIHost(SDHCI).Registers.Mask1:=0;
 PPL18XSDHCIHost(SDHCI).Registers.Clear:=$fff;
 
 {Request the IRQ/FIQ}
 if PL18X_MMCI_FIQ_ENABLED then
  begin
   {Request IRQ0}
   RequestFIQ(FIQ_ROUTING,PPL18XSDHCIHost(SDHCI).IRQ0,TInterruptHandler(PL18XSDHCIInterruptHandler),SDHCI);
   
   if not PPL18XSDHCIHost(SDHCI).SingleIRQ then
    begin
     {Request IRQ1}
     RequestFIQ(FIQ_ROUTING,PPL18XSDHCIHost(SDHCI).IRQ1,TInterruptHandler(PL18XSDHCIPIOInterruptHandler),SDHCI);
    end;
  end
 else
  begin
   {Request IRQ0}
   RequestIRQ(IRQ_ROUTING,PPL18XSDHCIHost(SDHCI).IRQ0,TInterruptHandler(PL18XSDHCIInterruptHandler),SDHCI);
   
   if not PPL18XSDHCIHost(SDHCI).SingleIRQ then
    begin
     {Request IRQ1}
     RequestIRQ(IRQ_ROUTING,PPL18XSDHCIHost(SDHCI).IRQ1,TInterruptHandler(PL18XSDHCIPIOInterruptHandler),SDHCI);
    end;
  end;  
 
 {Enable Interrupts}
 PPL18XSDHCIHost(SDHCI).Registers.Mask0:=PL18X_MMCI_IRQENABLE;
 
 {Create MMC}
 MMC:=MMCDeviceCreate;
 if MMC = nil then
  begin
   if MMC_LOG_ENABLED then MMCLogError(nil,'PL18X: Failed to create new MMC device');
   Exit;
  end;
 
 {Update MMC}
 {Device}
 MMC.Device.DeviceBus:=DEVICE_BUS_MMC;
 MMC.Device.DeviceType:=MMC_TYPE_MMC;
 MMC.Device.DeviceFlags:=MMC_FLAG_NONE;
 MMC.Device.DeviceData:=SDHCI;
 {MMC}
 MMC.MMCState:=MMC_STATE_EJECTED;
 MMC.DeviceInitialize:=SDHCI.DeviceInitialize;
 MMC.DeviceDeinitialize:=SDHCI.DeviceDeinitialize;
 MMC.DeviceGetCardDetect:=SDHCI.DeviceGetCardDetect;
 MMC.DeviceGetWriteProtect:=SDHCI.DeviceGetWriteProtect;
 MMC.DeviceSendCommand:=SDHCI.DeviceSendCommand;
 MMC.DeviceSetIOS:=SDHCI.DeviceSetIOS;
 {Driver}
 MMC.Capabilities:=MMC_CAP_CMD23;
 if PPL18XSDHCIHost(SDHCI).Version.BusyDetect then 
  begin
    //MMC.DeviceCardBusy:=PL18XMMCDeviceCardBusy; //To Do //mmci_card_busy
    //PL18XWriteDataCtrlRegister(PPL18XSDHCIHost(SDHCI),PL18X_MMCI_ST_DPSM_BUSYMODE) //To Do
    //MMC.Capabilities:=MMC.Capabilities or MMC_CAP_WAIT_WHILE_BUSY; //To Do
    //MMC.MaxBusyTimeout:=0; //
  end;
 
 {Create Storage}
 MMC.Storage:=StorageDeviceCreate;
 if MMC.Storage = nil then
  begin
   if MMC_LOG_ENABLED then MMCLogError(nil,'PL18X: Failed to create new Storage device');
   MMCDeviceDestroy(MMC);
   Exit;
  end;
 
 {Update Storage}
 {Device}
 MMC.Storage.Device.DeviceBus:=DEVICE_BUS_MMC;
 MMC.Storage.Device.DeviceType:=STORAGE_TYPE_REMOVABLE;
 MMC.Storage.Device.DeviceFlags:=STORAGE_FLAG_REMOVABLE or STORAGE_FLAG_NOT_READY or STORAGE_FLAG_NO_MEDIA;
 MMC.Storage.Device.DeviceData:=MMC;
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
   if MMC_LOG_ENABLED then MMCLogError(nil,'PL18X: Failed to initialize new MMC device');
   StorageDeviceDestroy(MMC.Storage);
   MMCDeviceDestroy(MMC);
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
   if MMC_LOG_ENABLED then MMCLogError(nil,'PL18X: Failed to register new MMC device');
   StorageDeviceDestroy(MMC.Storage);
   MMCDeviceDestroy(MMC);
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
     if MMC_LOG_ENABLED then MMCLogError(nil,'PL18X: Failed start status for new MMC device');
    end;
  end;  
 
 Result:=ERROR_SUCCESS;
end;
 
{==============================================================================}

function PL18XSDHCIHostStop(SDHCI:PSDHCIHost):LongWord;
{Implementation of SDHCIHostStop API for PL18X SDHCI}
{Note: Not intended to be called directly by applications, use SDHCIHostStop instead}
var
 MMC:PMMCDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
  
 {Check SDHCI}
 if SDHCI = nil then Exit;
 
 {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'PL18X: SDHCI Host Stop');
 {$ENDIF}
 
 {Get MMC}
 MMC:=MMCDeviceFindByDevice(@SDHCI.Device);
 if MMC <> nil then
  begin 
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
 
 {Disable Host}
 SDHCI.SDHCIState:=SDHCI_STATE_DISABLED;
  
 {Notify Disable}
 NotifierNotify(@SDHCI.Device,DEVICE_NOTIFICATION_DISABLE);
 
 {Release the IRQ/FIQ}
 if PL18X_MMCI_FIQ_ENABLED then
  begin
   {Release IRQ0}
   ReleaseFIQ(FIQ_ROUTING,PPL18XSDHCIHost(SDHCI).IRQ0,TInterruptHandler(PL18XSDHCIInterruptHandler),SDHCI);
   
   if not PPL18XSDHCIHost(SDHCI).SingleIRQ then
    begin
     {Release IRQ1}
     ReleaseFIQ(FIQ_ROUTING,PPL18XSDHCIHost(SDHCI).IRQ1,TInterruptHandler(PL18XSDHCIPIOInterruptHandler),SDHCI);
    end;
  end
 else
  begin
   {Release IRQ0}
   ReleaseIRQ(IRQ_ROUTING,PPL18XSDHCIHost(SDHCI).IRQ0,TInterruptHandler(PL18XSDHCIInterruptHandler),SDHCI);
   
   if not PPL18XSDHCIHost(SDHCI).SingleIRQ then
    begin
     {Release IRQ1}
     ReleaseIRQ(IRQ_ROUTING,PPL18XSDHCIHost(SDHCI).IRQ1,TInterruptHandler(PL18XSDHCIPIOInterruptHandler),SDHCI);
    end;
  end;  
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Reset Interrupts}
 PPL18XSDHCIHost(SDHCI).Registers.Mask0:=0;
 PPL18XSDHCIHost(SDHCI).Registers.Mask1:=0;
 PPL18XSDHCIHost(SDHCI).Registers.Clear:=$fff;
 
 {Reset Command}
 PPL18XSDHCIHost(SDHCI).Registers.Command:=0;
 
 {Reset Data Control}
 PPL18XSDHCIHost(SDHCI).Registers.DataCtrl:=0;
 
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
 
 {Destroy the Lock}
 if PPL18XSDHCIHost(SDHCI).Lock <> INVALID_HANDLE_VALUE then
  begin
   SpinDestroy(PPL18XSDHCIHost(SDHCI).Lock);
   PPL18XSDHCIHost(SDHCI).Lock:=INVALID_HANDLE_VALUE
  end; 
 
 {Destroy the Semaphore}
 if SDHCI.Wait <> INVALID_HANDLE_VALUE then
  begin
   SemaphoreDestroy(SDHCI.Wait);
   SDHCI.Wait:=INVALID_HANDLE_VALUE;
  end; 
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function PL18XSDHCIHostReadByte(SDHCI:PSDHCIHost;Reg:LongWord):Byte; 
{Implementation of SDHCIHostReadByte API for PL18X SDHCI}
{Note: Not intended to be called directly by applications, use SDHCIHostReadByte instead}
begin
 {Not used by MMCI}
 Result:=0;
 
 {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'PL18X: SDHCI Host Read Byte');
 {$ENDIF}
end;

{==============================================================================}

function PL18XSDHCIHostReadWord(SDHCI:PSDHCIHost;Reg:LongWord):Word; 
{Implementation of SDHCIHostReadWord API for PL18X SDHCI}
{Note: Not intended to be called directly by applications, use SDHCIHostReadWord instead}
begin
 {Not used by MMCI}
 Result:=0;
 
 {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'PL18X: SDHCI Host Read Word');
 {$ENDIF}
end;

{==============================================================================}

function PL18XSDHCIHostReadLong(SDHCI:PSDHCIHost;Reg:LongWord):LongWord; 
{Implementation of SDHCIHostReadLong API for PL18X SDHCI}
{Note: Not intended to be called directly by applications, use SDHCIHostReadLong instead}
begin
 {Not used by MMCI}
 Result:=0;
 
 {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'PL18X: SDHCI Host Read Long');
 {$ENDIF}
end;

{==============================================================================}

procedure PL18XSDHCIHostWriteByte(SDHCI:PSDHCIHost;Reg:LongWord;Value:Byte); 
{Implementation of SDHCIHostWriteByte API for PL18X SDHCI}
{Note: Not intended to be called directly by applications, use SDHCIHostWriteByte instead}
begin
 {Not used by MMCI}
 {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'PL18X: SDHCI Host Write Byte');
 {$ENDIF}
end;

{==============================================================================}

procedure PL18XSDHCIHostWriteWord(SDHCI:PSDHCIHost;Reg:LongWord;Value:Word); 
{Implementation of SDHCIHostWriteWord API for PL18X SDHCI}
{Note: Not intended to be called directly by applications, use SDHCIHostWriteWord instead}
begin
 {Not used by MMCI}
 {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'PL18X: SDHCI Host Write Word');
 {$ENDIF}
end;

{==============================================================================}

procedure PL18XSDHCIHostWriteLong(SDHCI:PSDHCIHost;Reg:LongWord;Value:LongWord); 
{Implementation of SDHCIHostWriteLong API for PL18X SDHCI}
{Note: Not intended to be called directly by applications, use SDHCIHostWriteLong instead}
begin
 {Not used by MMCI}
 {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'PL18X: SDHCI Host Write Long');
 {$ENDIF}
end;

{==============================================================================}

procedure PL18XSDHCIInterruptHandler(SDHCI:PPL18XSDHCIHost);
{Interrupt handler for the PL18X SDHCI}
{Note: Not intended to be called directly by applications}
var
 Status:LongWord;
begin
 {}
 {Check SDHCI}
 if SDHCI = nil then Exit;
 
 {Acquire the Lock}
 if PL18X_MMCI_FIQ_ENABLED then
  begin
   if SpinLockIRQFIQ(SDHCI.Lock) <> ERROR_SUCCESS then Exit;
  end
 else
  begin
   if SpinLockIRQ(SDHCI.Lock) <> ERROR_SUCCESS then Exit;
  end;  
 
 {Update Statistics}
 Inc(SDHCI.SDHCI.InterruptCount);
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Check Status}
 repeat
  {Get Status}
  Status:=SDHCI.Registers.Status;
  
  {Check Single IRQ}
  if SDHCI.SingleIRQ then
   begin
    if (Status and SDHCI.Registers.Mask1) <> 0 then
     begin
      PL18XSDHCIPIOInterruptHandler(SDHCI);
     end;
     
    Status:=Status and not(PL18X_MMCI_IRQ1MASK);
   end;
  
  {Clear Status (Including PL18X_MMCI_ST_CARDBUSY if enabled)}
  Status:=(Status and SDHCI.Registers.Mask0);
  SDHCI.Registers.Clear:=Status;
  
  {Check Reversed IRQ Handling}
  if SDHCI.Version.ReversedIRQ then
   begin
    {Data Interrupt}
    PL18XSDHCIDataInterrupt(SDHCI,Status);
    
    {Command Interrupt}
    PL18XSDHCICommandInterrupt(SDHCI,Status);
   end
  else
   begin
    {Command Interrupt}
    PL18XSDHCICommandInterrupt(SDHCI,Status);

    {Data Interrupt}
    PL18XSDHCIDataInterrupt(SDHCI,Status);
   end;    
  
  {Don't poll for busy completion in interrupt}  
  if SDHCI.BusyStatus <> 0 then
   begin
    Status:=Status and not(PL18X_MMCI_ST_CARDBUSY); 
   end;
 until Status = 0;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Release the Lock}
 if PL18X_MMCI_FIQ_ENABLED then
  begin
   SpinUnlockIRQFIQ(SDHCI.Lock);
  end
 else
  begin
   SpinUnlockIRQ(SDHCI.Lock);
  end;     
end;

{==============================================================================}

procedure PL18XSDHCIDataInterrupt(SDHCI:PPL18XSDHCIHost;Status:LongWord);
{Data interrupt handler for the PL18X SDHCI}
{Note: Not intended to be called directly by applications}

{Note: Caller must hold the host lock}
var
 Remain:LongWord;
 Success:LongWord;
begin
 {}
 {Check SDHCI}
 if SDHCI = nil then Exit;
 
 {$IF DEFINED(PL18X_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'PL18X: Data Interrupt');
 {$ENDIF}
 
 {Check Command}
 if SDHCI.SDHCI.Command = nil then
  begin
   {$IF DEFINED(PL18X_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
   if MMC_LOG_ENABLED then MMCLogError(nil,'PL18X: Data Interrupt when no current command (Status=' + IntToHex(Status,8) + ')');
   {$ENDIF}
   Exit; 
  end; 

 {Check Command}
 if SDHCI.SDHCI.Command.Data = nil then
  begin
   {$IF DEFINED(PL18X_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
   if MMC_LOG_ENABLED then MMCLogError(nil,'PL18X: Data Interrupt when no current data (Status=' + IntToHex(Status,8) + ')');
   {$ENDIF}
   Exit; 
  end; 
  
 {Check for Errors}
 if (Status and (PL18X_MMCI_DATACRCFAIL or PL18X_MMCI_DATATIMEOUT or PL18X_MMCI_STARTBITERR or PL18X_MMCI_TXUNDERRUN or PL18X_MMCI_RXOVERRUN)) <> 0 then
  begin
   {Terminate DMA transfer}
   //To Do
   
   {Calculate how far we are into the transfer}
   Remain:=SDHCI.Registers.DataCnt;
   Success:=(SDHCI.SDHCI.Command.Data.BlockSize * SDHCI.SDHCI.Command.Data.BlockCount) - Remain;
   
   if (Status and PL18X_MMCI_DATACRCFAIL) <> 0 then
    begin
     {Last block was not successful}
     Dec(Success);
     
     {Return Status}
     SDHCI.SDHCI.Command.Status:=MMC_STATUS_INVALID_SEQUENCE;
    end
   else if (Status and PL18X_MMCI_DATATIMEOUT) <> 0 then
    begin
     {Return Status}
     SDHCI.SDHCI.Command.Status:=MMC_STATUS_TIMEOUT;
    end
   else if (Status and PL18X_MMCI_STARTBITERR) <> 0 then  
    begin
     {Return Status}
     SDHCI.SDHCI.Command.Status:=MMC_STATUS_HARDWARE_ERROR;
    end
   else if (Status and PL18X_MMCI_TXUNDERRUN) <> 0 then  
    begin
     {Return Status}
     SDHCI.SDHCI.Command.Status:=MMC_STATUS_HARDWARE_ERROR;
    end
   else if (Status and PL18X_MMCI_RXOVERRUN) <> 0 then  
    begin
     if Success > SDHCI.Version.FIFOSize then
      begin
       Dec(Success,SDHCI.Version.FIFOSize);
      end
     else
      begin
       Success:=0;
      end;
      
     {Return Status}
     SDHCI.SDHCI.Command.Status:=MMC_STATUS_HARDWARE_ERROR;
    end;
   
   {Update Bytes Transferred}
   SDHCI.SDHCI.Command.Data.BytesTransfered:=RoundDown(Success,SDHCI.SDHCI.Command.Data.BlockSize);
   
   {Signal Completion}
   SemaphoreSignal(SDHCI.SDHCI.Wait);
  end;
 
 {Check for Data Block End} 
 if (Status and PL18X_MMCI_DATABLOCKEND) <> 0 then
  begin
   {$IF DEFINED(PL18X_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
   if MMC_LOG_ENABLED then MMCLogError(nil,'PL18X: Unrequested PL18X_MMCI_DATABLOCKEND interrupt (Status=' + IntToHex(Status,8) + ')');
   {$ENDIF}
  end;
 
 {Check for Data End}  
 if ((Status and PL18X_MMCI_DATAEND) <> 0) or (SDHCI.SDHCI.Command.Status <> MMC_STATUS_NOT_PROCESSED) then
  begin
   {Stop Data}
   PL18XSDHCIStopData(SDHCI);
   
   {Check Status}
   if SDHCI.SDHCI.Command.Status = MMC_STATUS_NOT_PROCESSED then
    begin
     {Check Data Completed}
     if not SDHCI.SDHCI.Command.DataCompleted then
      begin
       {Update Bytes Transferred}
       SDHCI.SDHCI.Command.Data.BytesTransfered:=(SDHCI.SDHCI.Command.Data.BlockSize * SDHCI.SDHCI.Command.Data.BlockCount);
       
       {Return Status}
       SDHCI.SDHCI.Command.Status:=MMC_STATUS_SUCCESS;
     
       {Set Data Completed}
       SDHCI.SDHCI.Command.DataCompleted:=True;
     
       {Check Command Completed}
       if SDHCI.SDHCI.Command.CommandCompleted then
        begin
         {Signal Completion}
         SemaphoreSignal(SDHCI.SDHCI.Wait);
        end;   
      end;  
    end;
  end;
end;

{==============================================================================}

procedure PL18XSDHCICommandInterrupt(SDHCI:PPL18XSDHCIHost;Status:LongWord);
{Command interrupt handler for the PL18X SDHCI}
{Note: Not intended to be called directly by applications}

{Note: Caller must hold the host lock}
var
  BusyResponse:Boolean;
begin
 {}
 {Check SDHCI}
 if SDHCI = nil then Exit;
 
 {$IF DEFINED(PL18X_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'PL18X: Command Interrupt');
 {$ENDIF}
 
 {Check Command}
 if SDHCI.SDHCI.Command = nil then
  begin
   {$IF DEFINED(PL18X_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
   if MMC_LOG_ENABLED then MMCLogError(nil,'PL18X: Command Interrupt when no current command (Status=' + IntToHex(Status,8) + ')');
   {$ENDIF}
   Exit; 
  end; 
 
 {Check for Busy Response}
 BusyResponse:=SDHCI.Version.BusyDetect and ((SDHCI.SDHCI.Command.ResponseType and MMC_RSP_BUSY) <> 0);
 
 {Check for Command}
 if ((Status or SDHCI.BusyStatus) and (PL18X_MMCI_CMDCRCFAIL or PL18X_MMCI_CMDTIMEOUT or PL18X_MMCI_CMDSENT or PL18X_MMCI_CMDRESPEND)) = 0 then Exit;
 
 {Check if we need to wait for busy completion}
 if (SDHCI.BusyStatus <> 0) and ((Status and PL18X_MMCI_ST_CARDBUSY) <> 0) then Exit;
 
 {Enable busy completion if needed}
 if (SDHCI.BusyStatus = 0) and BusyResponse and ((Status and (PL18X_MMCI_CMDCRCFAIL or PL18X_MMCI_CMDTIMEOUT)) = 0) and ((SDHCI.Registers.Status and PL18X_MMCI_ST_CARDBUSY) <> 0) then
  begin
   SDHCI.Registers.Mask0:=SDHCI.Registers.Mask0 or PL18X_MMCI_ST_BUSYEND;
   SDHCI.BusyStatus:=Status and (PL18X_MMCI_CMDSENT or PL18X_MMCI_CMDRESPEND);
   Exit;
  end;
  
 {At busy completion, mask the IRQ and complete the request}
 if SDHCI.BusyStatus <> 0 then
  begin
   SDHCI.Registers.Mask0:=SDHCI.Registers.Mask0 and not(PL18X_MMCI_ST_BUSYEND);
   SDHCI.BusyStatus:=0;
  end;
 
 {Check Status}
 if (Status and PL18X_MMCI_CMDTIMEOUT) <> 0 then
  begin
   {Return Status}
   SDHCI.SDHCI.Command.Status:=MMC_STATUS_TIMEOUT;
   
   {Signal Completion}
   SemaphoreSignal(SDHCI.SDHCI.Wait);
  end
 else if ((Status and PL18X_MMCI_CMDCRCFAIL) <> 0) and ((SDHCI.SDHCI.Command.ResponseType and MMC_RSP_CRC) <> 0) then
  begin
   {Return Status}
   SDHCI.SDHCI.Command.Status:=MMC_STATUS_INVALID_SEQUENCE;
   
   {Signal Completion}
   SemaphoreSignal(SDHCI.SDHCI.Wait);
  end
 else
  begin
   {Check Command Completed}
   if not SDHCI.SDHCI.Command.CommandCompleted then
    begin
     {Read Response}
     SDHCI.SDHCI.Command.Response[0]:=SDHCI.Registers.Response0;
     SDHCI.SDHCI.Command.Response[1]:=SDHCI.Registers.Response1;
     SDHCI.SDHCI.Command.Response[2]:=SDHCI.Registers.Response2;
     SDHCI.SDHCI.Command.Response[3]:=SDHCI.Registers.Response3;
     
     
     {Set Command Completed}
     SDHCI.SDHCI.Command.CommandCompleted:=True;
  
     {Check Data Completed}
     if (SDHCI.SDHCI.Command.DataCompleted) or (SDHCI.SDHCI.Command.Data = nil) then
      begin
       {Return Status}
       SDHCI.SDHCI.Command.Status:=MMC_STATUS_SUCCESS;
       
       {Signal Completion}
       SemaphoreSignal(SDHCI.SDHCI.Wait);
      end;   
    end;  
  end;  
 
 if SDHCI.SDHCI.Command.Status <> MMC_STATUS_NOT_PROCESSED then
  begin
   {Stop Data}
   PL18XSDHCIStopData(SDHCI);
  end;
end;

{==============================================================================}

procedure PL18XSDHCIStopData(SDHCI:PPL18XSDHCIHost);
{Data stop handler for the PL18X SDHCI}
{Note: Not intended to be called directly by applications}

{Note: Caller must hold the host lock}
begin
 {}
 {Check SDHCI}
 if SDHCI = nil then Exit;
 
 {$IF DEFINED(PL18X_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'PL18X: Stop Data');
 {$ENDIF}
 
 {Write Data Control}
 PL18XWriteDataCtrlRegister(SDHCI,0);
 
 {Write Interrupt Mask1}
 PL18XSetInterruptMask1(SDHCI,0);
end;

{==============================================================================}

procedure PL18XSDHCIPIOInterruptHandler(SDHCI:PPL18XSDHCIHost);
{PIO Interrupt handler for the PL18X SDHCI}
{Note: Not intended to be called directly by applications}
var
 Len:LongWord;
 Buffer:Pointer;
 Remain:LongWord;
 Status:LongWord;
begin
 {}
 {Check SDHCI}
 if SDHCI = nil then Exit;

 {Acquire the Lock}
 if not SDHCI.SingleIRQ then
  begin
   if PL18X_MMCI_FIQ_ENABLED then
    begin
     if SpinLockIRQFIQ(SDHCI.Lock) <> ERROR_SUCCESS then Exit;
    end
   else
    begin
     if SpinLockIRQ(SDHCI.Lock) <> ERROR_SUCCESS then Exit;
    end; 
  end;  
 
 {Update Statistics}
 if not SDHCI.SingleIRQ then Inc(SDHCI.SDHCI.InterruptCount);
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Setup Defaults}
 Remain:=0;
 
 {Get Status}
 Status:=SDHCI.Registers.Status;
 while True do
  begin
   {Check Command}
   if SDHCI.SDHCI.Command = nil then Break;
   
   {Check Command}
   if SDHCI.SDHCI.Command.Data = nil then Break;
   
   {For write, we only need to test the half-empty flag here - if the FIFO is completely empty, then by definition it is more than half empty}
   if (Status and (PL18X_MMCI_TXFIFOHALFEMPTY or PL18X_MMCI_RXDATAAVLBL)) = 0 then Break;

   Len:=0;
   Remain:=SDHCI.SDHCI.Command.Data.BytesRemaining;
   Buffer:=Pointer(SDHCI.SDHCI.Command.Data.Data + SDHCI.SDHCI.Command.Data.BlockOffset);
  
   if (Status and PL18X_MMCI_RXACTIVE) <> 0 then
    begin
     Len:=PL18XSHDCIReadPIO(SDHCI,Buffer,Remain);
    end
   else if (Status and PL18X_MMCI_TXACTIVE) <> 0 then
    begin
     Len:=PL18XSDHCIWritePIO(SDHCI,Buffer,Remain,Status);
    end;
   
   {Update Offset and Remain}
   Inc(SDHCI.SDHCI.Command.Data.BlockOffset,Len);
   Dec(SDHCI.SDHCI.Command.Data.BytesRemaining,Len);
   Dec(Remain,Len);
   
   if Remain <> 0 then Break;
   
   {Get Status}
   Status:=SDHCI.Registers.Status;
  end;
 
 {If we have less than the fifo 'half-full' threshold to transfer, trigger a PIO interrupt as soon as any data is available}
 if ((Status and PL18X_MMCI_RXACTIVE) <> 0) and (Remain < SDHCI.Version.FIFOHalfSize) then
  begin
   PL18XSetInterruptMask1(SDHCI,PL18X_MMCI_RXDATAAVLBLMASK);
  end;
 
 {If we run out of data, disable the data IRQs; this prevents a race where the FIFO becomes empty
  before the chip itself has disabled the data path, and stops us racing with our data end IRQ}
 if Remain = 0 then
  begin
   PL18XSetInterruptMask1(SDHCI,0);
   SDHCI.Registers.Mask0:=SDHCI.Registers.Mask0 or PL18X_MMCI_DATAENDMASK;
  end;  
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Release the Lock}
 if not SDHCI.SingleIRQ then
  begin
   if PL18X_MMCI_FIQ_ENABLED then
    begin
      SpinUnlockIRQFIQ(SDHCI.Lock);
    end
   else
    begin
     SpinUnlockIRQ(SDHCI.Lock);
    end;     
  end; 
end;

{==============================================================================}

function PL18XSHDCIReadPIO(SDHCI:PPL18XSDHCIHost;Buffer:Pointer;Remain:LongWord):LongWord;
{PIO read interrupt handler for the PL18X SDHCI}
{Note: Not intended to be called directly by applications}

{Note: Caller must hold the host lock}
var
 Next:Pointer;
 Size:LongWord;
 Count:LongWord;
 Offset:PtrUInt;
 Status:LongWord;
 Tail:array[0..3] of Byte;
begin
 {}
 Result:=0;
 
 {Check SDHCI}
 if SDHCI = nil then Exit;
 
 {Check Buffer}
 if Buffer = nil then Exit;
 
 {Check Remain}
 if Remain = 0 then Exit;
 
 {Setup Start}
 Next:=Buffer;
 
 {Get Status}
 Status:=SDHCI.Registers.Status;
 while (Status and PL18X_MMCI_RXDATAAVLBL) <> 0 do
  begin
   {Get Size}
   Size:=SDHCI.GetRXFIFOCount(SDHCI,Status,Remain);
   if Size > Remain then
    begin
     Size:=Remain;
    end;
   if Size < 1 then Break;    
   
   {SDIO may want to send something that is not divisible by 4 (as opposed to card sectors etc). 
    Therefore make sure to always read the last bytes while only doing full 32-bit reads form the FIFO}
   if (Size and $03) <> 0 then
    begin
     if Size < 4 then
      begin
       {Read Last Bytes}
       PLongWord(@Tail)^:=SDHCI.Registers.FIFO;
       System.Move(PLongWord(@Tail)^,Next^,Size);
      end
     else
      begin
       {Read Aligned Bytes}
       Count:=Size shr 2;
       Offset:=0;
       while Count > 0 do
        begin
         PLongWord(Next + Offset)^:=SDHCI.Registers.FIFO;
         Dec(Count);
         Inc(Offset,SizeOf(LongWord));
        end;
       
       {Round Size}
       Size:=Size and not($03);
      end;      
    end
   else
    begin
     {Read Aligned Bytes}
     Count:=Size shr 2;
     Offset:=0;
     while Count > 0 do
      begin
       PLongWord(Next + Offset)^:=SDHCI.Registers.FIFO;
       Dec(Count);
       Inc(Offset,SizeOf(LongWord));
      end;
    end;    
    
   {Update Buffer and Remain}
   Inc(Next,Size);
   Dec(Remain,Size);
   if Remain = 0 then Break;
   
   {Get Status}
   Status:=SDHCI.Registers.Status;
  end;

 {Get Result}
 Result:=Next - Buffer;
end;

{==============================================================================}

function PL18XSDHCIWritePIO(SDHCI:PPL18XSDHCIHost;Buffer:Pointer;Remain,Status:LongWord):LongWord;
{PIO read interrupt handler for the PL18X SDHCI}
{Note: Not intended to be called directly by applications}

{Note: Caller must hold the host lock}
var
 Next:Pointer;
 Size:LongWord;
 Count:LongWord;
 Offset:PtrUInt;
 MaxSize:LongWord;
begin
 {}
 Result:=0;
 
 {Check SDHCI}
 if SDHCI = nil then Exit;
 
 {Check Buffer}
 if Buffer = nil then Exit;
 
 {Check Remain}
 if Remain = 0 then Exit;
 
 {Setup Start}
 Next:=Buffer;
 
 {Get Status}
 while (Status and PL18X_MMCI_TXFIFOHALFEMPTY) <> 0 do
  begin
   {Get Max Size}
   if (Status and PL18X_MMCI_TXFIFOEMPTY) <> 0 then
    begin
     MaxSize:=SDHCI.Version.FIFOSize;
    end
   else
    begin
     MaxSize:=SDHCI.Version.FIFOHalfSize;
    end;    
   Size:=Min(Remain,MaxSize);
   
   {SDIO may want to send something that is not divisible by 4 (as opposed to card sectors etc),
    and the FIFO only accept full 32-bit writes. So compensate by adding +3 on the count}
   {Write Aligned Bytes}
   Count:=(Size + 3) shr 2;
   Offset:=0;
   while Count > 0 do
    begin
     SDHCI.Registers.FIFO:=PLongWord(Next + Offset)^;
     Dec(Count);
     Inc(Offset,SizeOf(LongWord));
    end;
   
   {Update Buffer and Remain}
   Inc(Next,Size);
   Dec(Remain,Size);
   if Remain = 0 then Break;
   
   {Get Status}
   Status:=SDHCI.Registers.Status;
  end;
 
 {Get Result}
 Result:=Next - Buffer;
end;

{==============================================================================}
{==============================================================================}
{PL18X Helper Functions}
function PL18XGetPeripheralID(SDHCI:PPL18XSDHCIHost):LongWord;
{Return the Peripheral ID for a PL18X SDHCI device}
var
 Count:Integer;
 Value:LongWord;
begin
 {}
 Result:=0;
 
 {Check SDHCI}
 if SDHCI = nil then Exit;

 {Check Address}
 if SDHCI.SDHCI.Address = nil then Exit;
 
 {Get Peripheral ID}
 for Count:=0 to 3 do
  begin
   Value:=PLongWord(SDHCI.SDHCI.Address + PL18X_MMCI_PERIPHID + (Count shl 2))^;
   
   Result:=Result or ((Value and $FF) shl (Count shl 3));
  end; 
  
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
end;
 

{==============================================================================}

function PL18XGetVersionData(SDHCI:PPL18XSDHCIHost):PPL18XVersionData;
{Return the version data for a PL18X SDHCI device based on the Peripheral ID}
var
 Count:Integer;
 PeripheralID:LongWord;
begin
 {}
 Result:=nil;
 
 {Check SDHCI}
 if SDHCI = nil then Exit;
 
 {Get Peripheral ID}
 PeripheralID:=PL18XGetPeripheralID(SDHCI);
 
 {$IF DEFINED(PL18X_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'PL18x: PeripheralID = ' + IntToHex(PeripheralID,8));
 {$ENDIF}
 
 {Find Peripheral ID}
 for Count:=0 to 9 do
  begin
   if (PeripheralID and PL18X_VERSION_ID[Count].PeripheralMask) = PL18X_VERSION_ID[Count].PeripheralID then
    begin
     Result:=PL18X_VERSION_ID[Count].VersionData;
     Exit;
    end;
  end;
  
 {Default to ARM}
 if Result = nil then Result:=@PL18X_VERSION_DATA_ARM; 
end;

{==============================================================================}

function PL18XGetRXFIFOCount(SDHCI:PPL18XSDHCIHost;Status,Remain:LongWord):LongWord;
{Determine the receive FIFO count available}
begin
 {}
 Result:=0;
 
 {Check SDHCI}
 if SDHCI = nil then Exit;

 Result:=Remain - (SDHCI.Registers.FifoCnt shl 2);
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
end;

{==============================================================================}

function PL18XQualcommGetRXFIFOCount(SDHCI:PPL18XSDHCIHost;Status,Remain:LongWord):LongWord;
{Determine the receive FIFO count available}
begin
 {}
 Result:=0;
 
 {Check SDHCI}
 if SDHCI = nil then Exit;
 
 {On Qualcomm SDCC4 only 8 words are used in each burst so only 8 addresses from the fifo range should be used}
 if (Status and PL18X_MMCI_RXFIFOHALFFULL) <> 0 then
  begin
   Result:=SDHCI.Version.FIFOHalfSize;
  end
 else if (Status and PL18X_MMCI_RXDATAAVLBL) <> 0 then
  begin
   Result:=4;
  end;  
end;

{==============================================================================}

procedure PL18XRegisterDelay(SDHCI:PPL18XSDHCIHost);
{Delay for the required amount of time after a register write}
begin
 {}
 {Check SDHCI}
 if SDHCI = nil then Exit;

 {According to the spec, at least three feedback clock cycles
  of max 52 MHz must pass between two writes to the MMCICLOCK reg.
  Three MCLK clock cycles must pass between two MMCIPOWER reg writes.
  Worst delay time during card init is at 100 kHz => 30 us.
  Worst delay time when up and running is at 25 MHz => 120 ns}
 if SDHCI.SDHCI.Clock < 25000000 then
  begin
   MicrosecondDelay(30);
  end
 else
  begin
   {NanosecondDelay(120);} {Not currently functional}
   MicrosecondDelay(1);
  end;  
end;

{==============================================================================}

procedure PL18XSetClockRegister(SDHCI:PPL18XSDHCIHost;MMC:PMMCDevice;Desired:LongWord);
{Setup the current clock rate in the clock register}

{Note: Caller must hold the host lock}
var
 Clock:LongWord;
begin
 {}
 {Check SDHCI}
 if SDHCI = nil then Exit;

 {Check MMC}
 if MMC = nil then Exit;
 
 {Get Defaults}
 Clock:=SDHCI.Version.ClockRegister;
 
 {Reset Clock}
 SDHCI.SDHCI.Clock:=0;
 
 {Check Desired}
 if Desired > 0 then
  begin
   if SDHCI.Version.ExplicitMClockControl then
    begin
     SDHCI.SDHCI.Clock:=SDHCI.SDHCI.MaximumFrequency;
    end
   else if Desired >= SDHCI.SDHCI.MaximumFrequency then
    begin
     Clock:=PL18X_MMCI_CLOCK_BYPASS;
     if SDHCI.Version.STClockDivider then
      begin
       Clock:=Clock or PL18X_MMCI_CLOCK_ST_UX500_NEG_EDGE;
      end;
     SDHCI.SDHCI.Clock:=SDHCI.SDHCI.MaximumFrequency;
    end
   else if SDHCI.Version.STClockDivider then
    begin
     {DB8500 TRM says f = mclk / (clkdiv + 2) => clkdiv = (mclk / f) - 2
      Round the divider up so we don't exceed the max frequency}
     Clock:=DivRoundUp(SDHCI.SDHCI.MaximumFrequency,Desired) - 2;
     if Clock >= 256 then
      begin
       Clock:=255;
      end;
     SDHCI.SDHCI.Clock:=SDHCI.SDHCI.MaximumFrequency div (Clock + 2);
    end
   else
    begin
     {PL180 TRM says f = mclk / (2 * (clkdiv + 1)) => clkdiv = mclk / (2 * f) - 1}
     Clock:=SDHCI.SDHCI.MaximumFrequency div (2 * Desired) - 1;
     if Clock >= 256 then
      begin
       Clock:=255;
      end;
     SDHCI.SDHCI.Clock:=SDHCI.SDHCI.MaximumFrequency div (2 * (Clock + 1));
    end;
    
   Clock:=Clock or SDHCI.Version.ClockEnable;
   Clock:=Clock or PL18X_MMCI_CLOCK_ENABLE;
  end;
  
 {Set MMC Clock}
 MMC.Clock:=SDHCI.SDHCI.Clock; 
 
 if MMC.BusWidth = MMC_BUS_WIDTH_4 then
  begin
   Clock:=Clock or PL18X_MMCI_CLOCK_4BIT_BUS;
  end
 else if MMC.BusWidth = MMC_BUS_WIDTH_8 then
  begin
   Clock:=Clock or SDHCI.Version.Clock8BitEnable;
  end;
  
 if (MMC.Timing = MMC_TIMING_UHS_DDR50) or (MMC.Timing = MMC_TIMING_MMC_DDR52) then
  begin
   Clock:=Clock or SDHCI.Version.ClockNegativeEdgeEnable;
  end;
  
 PL18XWriteClockRegister(SDHCI,Clock);
end;

{==============================================================================}

procedure PL18XSetPowerRegister(SDHCI:PPL18XSDHCIHost;Value:LongWord);
{Setup the current power state in the power register}

{Note: Caller must hold the host lock}
var
 Power:LongWord;
begin
 {}
 {Check SDHCI}
 if SDHCI = nil then Exit;
 
 {Check Value}
 if Value = 0 then
  begin
   {Power Off}
   Power:=PL18X_MMCI_POWER_OFF; 
   PL18XWritePowerRegister(SDHCI,Power);
   
   {Delay}
   PL18XRegisterDelay(SDHCI);
  end
 else
  begin
   {Power Up}
   Power:=SDHCI.Version.PowerPowerUp; 
   PL18XWritePowerRegister(SDHCI,Power);
   
   {Delay}
   PL18XRegisterDelay(SDHCI);
   
   {Power On}
   Power:=PL18X_MMCI_POWER_ON; 
   PL18XWritePowerRegister(SDHCI,Power);

   {Delay}
   PL18XRegisterDelay(SDHCI);
  end;  
end;


{==============================================================================}

procedure PL18XSetInterruptMask1(SDHCI:PPL18XSDHCIHost;Mask:LongWord);
{Setup the interrupt mask to use either Mask0 or Mask1 depending on configuration}

{Note: Caller must hold the host lock}
var
 Mask0:LongWord;
begin
 {}
 {Check SDHCI}
 if SDHCI = nil then Exit;

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Check Single IRQ}
 if SDHCI.SingleIRQ then
  begin
   {Read Mask0}
   Mask0:=SDHCI.Registers.Mask0;
   
   {Update Mask0}
   Mask0:=Mask0 and not(PL18X_MMCI_IRQ1MASK);
   Mask0:=Mask0 or Mask;
   
   {Write Mask0}
   SDHCI.Registers.Mask0:=Mask0;
  end;
  
 {Write Mask1}
 SDHCI.Registers.Mask1:=Mask;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
end;

{==============================================================================}

procedure PL18XWriteClockRegister(SDHCI:PPL18XSDHCIHost;Clock:LongWord);
{Write to the clock register}

{Note: Caller must hold the host lock}
begin
 {}
 {Check SDHCI}
 if SDHCI = nil then Exit;
 
 {Check Register}
 if SDHCI.ClockRegister <> Clock then
  begin
   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}
   
   {Set Register}
   SDHCI.ClockRegister:=Clock;
   SDHCI.Registers.Clock:=Clock;
  end; 
end;

{==============================================================================}

procedure PL18XWritePowerRegister(SDHCI:PPL18XSDHCIHost;Power:LongWord);
{Write to the power register}

{Note: Caller must hold the host lock}
begin
 {}
 {Check SDHCI}
 if SDHCI = nil then Exit;
 
 {Check Register}
 if SDHCI.PowerRegister <> Power then
  begin
   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}
   
   {Set Register}
   SDHCI.PowerRegister:=Power;
   SDHCI.Registers.Power:=Power;
  end; 
end;

{==============================================================================}

procedure PL18XWriteDataCtrlRegister(SDHCI:PPL18XSDHCIHost;DataCtrl:LongWord);
{Write to the data control register}

{Note: Caller must hold the host lock}
begin
 {}
 {Check SDHCI}
 if SDHCI = nil then Exit;
 
 {Keep ST Micro busy mode if enabled}
 DataCtrl:=DataCtrl or (SDHCI.DataCtrlRegister and PL18X_MMCI_ST_DPSM_BUSYMODE);
 
 {Check Register}
 if SDHCI.DataCtrlRegister <> DataCtrl then
  begin
   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}
   
   {Set Register}
   SDHCI.DataCtrlRegister:=DataCtrl;
   SDHCI.Registers.DataCtrl:=DataCtrl;
  end; 
end;

{==============================================================================}
{==============================================================================}

initialization
 PL18XInit;
 
{==============================================================================}
 
{finalization}
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
