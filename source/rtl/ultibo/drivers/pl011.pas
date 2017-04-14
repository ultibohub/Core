{
ARM PrimeCell PL011 UART Driver.

Copyright (C) 2016 - SoftOz Pty Ltd.

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

  Linux - \drivers\tty\serial\amba-pl011.c - Copyright (C) 2010 ST-Ericsson SA and others
  
References
==========

 PL011 - http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0183g/index.html
         
ARM PrimeCell PL011 UART
========================

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit PL011; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,UART,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {PL011 specific constants}
 PL011_UART_DESCRIPTION = 'ARM PrimeCell PL011 UART';  {Description of PL011 device}
 
 PL011_UART_MIN_BAUD = 300;      {Default minimum of 300 baud}
 PL011_UART_MAX_BAUD = 1500000;  {Default maximum based on 24MHz clock} 
 
 PL011_UART_MIN_DATABITS = SERIAL_DATA_5BIT;
 PL011_UART_MAX_DATABITS = SERIAL_DATA_8BIT;
 
 PL011_UART_MIN_STOPBITS = SERIAL_STOP_1BIT;
 PL011_UART_MAX_STOPBITS = SERIAL_STOP_2BIT;
 
 PL011_UART_MAX_PARITY = SERIAL_PARITY_EVEN;
 
 PL011_UART_MAX_FLOW = SERIAL_FLOW_RTS_CTS;
 
 PL011_UART_CLOCK_RATE = 24000000; 

const
 {PL011 UART Data register bits (See: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0183g/index.html)}
 PL011_UART_DR_OE    = (1 shl 11);   {Overrun error}
 PL011_UART_DR_BE    = (1 shl 10);   {Break error}
 PL011_UART_DR_PE    = (1 shl 9);    {Parity error}
 PL011_UART_DR_FE    = (1 shl 8);    {Framing error}
 PL011_UART_DR_DATA  = ($FF shl 0);  {Receive / Transmit data}
 PL011_UART_DR_ERROR = PL011_UART_DR_OE or PL011_UART_DR_BE or PL011_UART_DR_PE or PL011_UART_DR_FE;
 
 {PL011 UART Receive Status / Error Clear register bits (See: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0183g/index.html)}
 PL011_UART_RSRECR_OE = (1 shl 3); {Overrun error}
 PL011_UART_RSRECR_BE = (1 shl 2); {Break error}
 PL011_UART_RSRECR_PE = (1 shl 1); {Parity error}
 PL011_UART_RSRECR_FE = (1 shl 0); {Framing error}
 
 {PL011 UART Flag register bits (See: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0183g/index.html)}
 PL011_UART_FR_RI   = (1 shl 8); {Unsupported, write zero, read as don't care}
 PL011_UART_FR_TXFE = (1 shl 7); {Transmit FIFO empty}
 PL011_UART_FR_RXFF = (1 shl 6); {Receive FIFO full}
 PL011_UART_FR_TXFF = (1 shl 5); {Transmit FIFO full} 
 PL011_UART_FR_RXFE = (1 shl 4); {Receive FIFO empty} 
 PL011_UART_FR_BUSY = (1 shl 3); {UART busy}
 PL011_UART_FR_DCD  = (1 shl 2); {Unsupported, write zero, read as don't care}
 PL011_UART_FR_DSR  = (1 shl 1); {Unsupported, write zero, read as don't care}
 PL011_UART_FR_CTS  = (1 shl 0); {Clear to send (This bit is the complement of the UART clear to send, nUARTCTS, modem status input. That is, the bit is 1 when nUARTCTS is LOW)}
 
 {PL011 UART IrDA register bits (See: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0183g/index.html)}
  
 {PL011 UART Integer Baud Rate Divisor register bits (See: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0183g/index.html)}
 PL011_UART_IBRD_MASK = ($FFFF shl 0);
 
 {PL011 UART Fractional Baud Rate Divisor register bits (See: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0183g/index.html)}
 PL011_UART_FBRD_MASK = ($3F shl 0);
 
 {PL011 UART Line Control register bits (See: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0183g/index.html)}
 PL011_UART_LCRH_SPS   = (1 shl 7); {Stick parity select}
 PL011_UART_LCRH_WLEN  = (3 shl 5); {Word length}
 PL011_UART_LCRH_WLEN8 = (3 shl 5); { 8 bits}
 PL011_UART_LCRH_WLEN7 = (2 shl 5); { 7 bits} 
 PL011_UART_LCRH_WLEN6 = (1 shl 5); { 6 bits} 
 PL011_UART_LCRH_WLEN5 = (0 shl 5); { 5 bits} 
 PL011_UART_LCRH_FEN   = (1 shl 4); {Enable FIFOs} 
 PL011_UART_LCRH_STP2  = (1 shl 3); {Two stop bits select}
 PL011_UART_LCRH_EPS   = (1 shl 2); {Even parity select (0 = odd parity / 1 = even parity)} 
 PL011_UART_LCRH_PEN   = (1 shl 1); {Parity enable} 
 PL011_UART_LCRH_BRK   = (1 shl 0); {Send break} 
 
 {PL011 UART Control register bits (See: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0183g/index.html)}
 PL011_UART_CR_CTSEN  = (1 shl 15); {CTS hardware flow control enable (If this bit is set to 1 data is only transmitted when the nUARTCTS signal is asserted)}
 PL011_UART_CR_RTSEN  = (1 shl 14); {RTS hardware flow control enable (If this bit is set to 1 data is only requested when there is space in the receive FIFO for it to be received)} 
 PL011_UART_CR_OUT2   = (1 shl 13); {Unsupported, write zero, read as don't care} 
 PL011_UART_CR_OUT1   = (1 shl 12); {Unsupported, write zero, read as don't care} 
 PL011_UART_CR_RTS    = (1 shl 11); {Request to send (This bit is the complement of the UART request to send, nUARTRTS, modem status output. That is, when the bit is programmed to a 1 then nUARTRTS is LOW)} 
 PL011_UART_CR_DTR    = (1 shl 10); {Unsupported, write zero, read as don't care} 
 PL011_UART_CR_RXE    = (1 shl 9);  {Receive enable}
 PL011_UART_CR_TXE    = (1 shl 8);  {Transmit enable} 
 PL011_UART_CR_LBE    = (1 shl 7);  {Loopback enable}
 {Bits 6:3 Reserved - Write as 0, read as don't care}
 PL011_UART_CR_SIRLP  = (1 shl 2);  {Unsupported, write zero, read as don't care} 
 PL011_UART_CR_SIREN  = (1 shl 1);  {Unsupported, write zero, read as don't care}
 PL011_UART_CR_UARTEN = (1 shl 0);  {UART enable}
 
 {PL011 UART Interrupt FIFO Level Select register bits (See: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0183g/index.html)}
 PL011_UART_IFLS_RXIFPSEL    = (7 shl 9); {Unsupported, write zero, read as don't care}
 PL011_UART_IFLS_TXIFPSEL    = (7 shl 6); {Unsupported, write zero, read as don't care} 
 PL011_UART_IFLS_RXIFLSEL    = (7 shl 3); {Receive interrupt FIFO level select} 
 PL011_UART_IFLS_RXIFLSEL1_8 = (0 shl 3); { b000 = Receive FIFO becomes >= 1/8 full}
 PL011_UART_IFLS_RXIFLSEL1_4 = (1 shl 3); { b001 = Receive FIFO becomes >= 1/4 full} 
 PL011_UART_IFLS_RXIFLSEL1_2 = (2 shl 3); { b010 = Receive FIFO becomes >= 1/2 full} 
 PL011_UART_IFLS_RXIFLSEL3_4 = (3 shl 3); { b011 = Receive FIFO becomes >= 3/4 full} 
 PL011_UART_IFLS_RXIFLSEL7_8 = (4 shl 3); { b100 = Receive FIFO becomes >= 7/8 full} 
 PL011_UART_IFLS_TXIFLSEL    = (7 shl 0); {Transmit interrupt FIFO level select} 
 PL011_UART_IFLS_TXIFLSEL1_8 = (0 shl 0); { b000 = Transmit FIFO becomes <= 1/8 full} 
 PL011_UART_IFLS_TXIFLSEL1_4 = (1 shl 0); { b001 = Transmit FIFO becomes <= 1/4 full} 
 PL011_UART_IFLS_TXIFLSEL1_2 = (2 shl 0); { b010 = Transmit FIFO becomes <= 1/2 full} 
 PL011_UART_IFLS_TXIFLSEL3_4 = (3 shl 0); { b011 = Transmit FIFO becomes <= 3/4 full}  
 PL011_UART_IFLS_TXIFLSEL7_8 = (4 shl 0); { b100 = Transmit FIFO becomes <= 7/8 full}  
 
 {PL011 UART Interrupt Mask Set/Clear register bits (See: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0183g/index.html)}
 PL011_UART_IMSC_OEIM   = (1 shl 10); {Overrun error interrupt mask}
 PL011_UART_IMSC_BEIM   = (1 shl 9);  {Break error interrupt mask} 
 PL011_UART_IMSC_PEIM   = (1 shl 8);  {Parity error interrupt mask} 
 PL011_UART_IMSC_FEIM   = (1 shl 7);  {Framing error interrupt mask} 
 PL011_UART_IMSC_RTIM   = (1 shl 6);  {Receive timeout interrupt mask} 
 PL011_UART_IMSC_TXIM   = (1 shl 5);  {Transmit interrupt mask}
 PL011_UART_IMSC_RXIM   = (1 shl 4);  {Receive interrupt mask} 
 PL011_UART_IMSC_DSRMIM = (1 shl 3);  {Unsupported, write zero, read as don't care} 
 PL011_UART_IMSC_DCDMIM = (1 shl 2);  {Unsupported, write zero, read as don't care} 
 PL011_UART_IMSC_CTSMIM = (1 shl 1);  {nUARTCTS modem interrupt mask} 
 PL011_UART_IMSC_RIMIM  = (1 shl 0);  {Unsupported, write zero, read as don't care}
 
 {PL011 UART Raw Interrupt Status register bits (See: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0183g/index.html)}
 PL011_UART_RIS_OERIS   = (1 shl 10); {Overrun error interrupt status}
 PL011_UART_RIS_BERIS   = (1 shl 9);  {Break error interrupt status}
 PL011_UART_RIS_PERIS   = (1 shl 8);  {Parity error interrupt status} 
 PL011_UART_RIS_FERIS   = (1 shl 7);  {Framing error interrupt status} 
 PL011_UART_RIS_RTRIS   = (1 shl 6);  {Receive timeout interrupt status} 
 PL011_UART_RIS_TXRIS   = (1 shl 5);  {Transmit interrupt status}
 PL011_UART_RIS_RXRIS   = (1 shl 4);  {Receive interrupt status} 
 PL011_UART_RIS_DSRMRIS = (1 shl 3);  {Unsupported, write zero, read as don't care} 
 PL011_UART_RIS_DCDMRIS = (1 shl 2);  {Unsupported, write zero, read as don't care} 
 PL011_UART_RIS_CTSMRIS = (1 shl 1);  {nUARTCTS modem interrupt status} 
 PL011_UART_RIS_RIMRIS  = (1 shl 0);  {Unsupported, write zero, read as don't care} 
 
 {PL011 UART Masked Interrupt Status register bits (See: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0183g/index.html)}
 PL011_UART_MIS_OEMIS   = (1 shl 10); {Overrun error masked interrupt status}
 PL011_UART_MIS_BEMIS   = (1 shl 9);  {Break error masked interrupt status} 
 PL011_UART_MIS_PEMIS   = (1 shl 8);  {Parity error masked interrupt status} 
 PL011_UART_MIS_FEMIS   = (1 shl 7);  {Framing error masked interrupt status}  
 PL011_UART_MIS_RTMIS   = (1 shl 6);  {Receive timeout masked interrupt status}  
 PL011_UART_MIS_TXMIS   = (1 shl 5);  {Transmit masked interrupt status}  
 PL011_UART_MIS_RXMIS   = (1 shl 4);  {Receive masked interrupt status}  
 PL011_UART_MIS_DSRMMIS = (1 shl 3);  {Unsupported, write zero, read as don't care}  
 PL011_UART_MIS_DCDMMIS = (1 shl 2);  {Unsupported, write zero, read as don't care}  
 PL011_UART_MIS_CTSMMIS = (1 shl 1);  {nUARTCTS modem masked interrupt status}  
 PL011_UART_MIS_RIMMIS  = (1 shl 0);  {Unsupported, write zero, read as don't care}  
 
 {PL011 UART Interrupt Clear register bits (See: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0183g/index.html)}
 PL011_UART_ICR_OEIC   = (1 shl 10); {Overrun error interrupt clear}
 PL011_UART_ICR_BEIC   = (1 shl 9);  {Break error interrupt clear}
 PL011_UART_ICR_PEIC   = (1 shl 8);  {Parity error interrupt clear} 
 PL011_UART_ICR_FEIC   = (1 shl 7);  {Framing error interrupt clear} 
 PL011_UART_ICR_RTIC   = (1 shl 6);  {Receive timeout interrupt clear} 
 PL011_UART_ICR_TXIC   = (1 shl 5);  {Transmit interrupt clear} 
 PL011_UART_ICR_RXIC   = (1 shl 4);  {Receive interrupt clear} 
 PL011_UART_ICR_DSRMIC = (1 shl 3);  {Unsupported, write zero, read as don't care} 
 PL011_UART_ICR_DCDMIC = (1 shl 2);  {Unsupported, write zero, read as don't care} 
 PL011_UART_ICR_CTSMIC = (1 shl 1);  {nUARTCTS modem interrupt clear} 
 PL011_UART_ICR_RIMIC  = (1 shl 0);  {Unsupported, write zero, read as don't care} 
 
 {PL011 UART DMA Control register bits (See: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0183g/index.html)}
  {This register is disabled, writing to it has no effect and reading returns 0}

 {PL011 UART Test Control register bits (See: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0183g/index.html)}
 
 {PL011 UART Integration Test Input register bits (See: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0183g/index.html)}

 {PL011 UART Integration Test Output register bits (See: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0183g/index.html)}

 {PL011 UART Test Data register bits (See: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0183g/index.html)}
 
{==============================================================================}
type
 {PL011 specific types}
 {Layout of the PL011 registers (See: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0183g/I18702.html)}
 PPL011UARTRegisters = ^TPL011UARTRegisters;
 TPL011UARTRegisters = record
  DR:LongWord;         {Data Register}
  RSRECR:LongWord;     {Receive Status Register / Error Clear Register}
  Reserved01:LongWord;
  Reserved02:LongWord;
  Reserved03:LongWord;
  Reserved04:LongWord;
  FR:LongWord;         {Flag register}
  Reserved05:LongWord;
  ILPR:LongWord;       {IrDA Low-Power Counter Register}
  IBRD:LongWord;       {Integer Baud rate divisor}
  FBRD:LongWord;       {Fractional Baud rate divisor}
  LCRH:LongWord;       {Line Control register}
  CR:LongWord;         {Control register}
  IFLS:LongWord;       {Interrupt FIFO Level Select Register}
  IMSC:LongWord;       {Interrupt Mask Set Clear Register}
  RIS:LongWord;        {Raw Interrupt Status Register}
  MIS:LongWord;        {Masked Interrupt Status Register}
  ICR:LongWord;        {Interrupt Clear Register}
  DMACR:LongWord;      {DMA Control Register}
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
  ITCR:LongWord;       {Test Control Register}
  ITIP:LongWord;       {Integration Test Input Register}
  ITOP:LongWord;       {Integration Test Output Register}
  TDR:LongWord;        {Test Data Register}
 end;
 
 PPL011UART = ^TPL011UART;
 TPL011UART = record
  {UART Properties}
  UART:TUARTDevice;
  {PL011 Properties}
  IRQ:LongWord;
  Lock:TSpinHandle;                                                       {Device lock (Differs from lock in UART device) (Spin lock due to use by interrupt handler)}
  ClockRate:LongWord;                                                     {Device clock rate}
  Registers:PPL011UARTRegisters;                                          {Device registers}
  {Statistics Properties}                                        
  InterruptCount:LongWord;                                                {Number of interrupt requests received by the device}
 end;
 
{==============================================================================}
var
 {PL011 specific variables}
 PL011_RX_IRQ_MASK:Boolean = False; {If True then mask RX interrupts while RX FIFO is not empty (Allows for implementation variations)}
 
{==============================================================================}
{Initialization Functions}
 
{==============================================================================}
{PL011 Functions}
function PL011UARTCreate(Address:LongWord;const Name:String;IRQ,ClockRate:LongWord):PUARTDevice;

function PL011UARTDestroy(UART:PUARTDevice):LongWord;

{==============================================================================}
{PL011 UART Functions}
function PL011UARTOpen(UART:PUARTDevice;BaudRate,DataBits,StopBits,Parity,FlowControl:LongWord):LongWord;
function PL011UARTClose(UART:PUARTDevice):LongWord;
 
function PL011UARTRead(UART:PUARTDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
function PL011UARTWrite(UART:PUARTDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
 
function PL011UARTStatus(UART:PUARTDevice):LongWord;

procedure PL011UARTInterruptHandler(UART:PUARTDevice);

procedure PL011UARTReceive(UART:PUARTDevice);
procedure PL011UARTTransmit(UART:PUARTDevice);

procedure PL011UARTEnableInterrupt(UART:PPL011UART;Interrupt:LongWord); 
procedure PL011UARTDisableInterrupt(UART:PPL011UART;Interrupt:LongWord); 

{==============================================================================}
{PL011 Helper Functions}

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{var}
 {PL011 specific variables}
 
{==============================================================================}
{==============================================================================}
{Initialization Functions}
 
{==============================================================================}
{==============================================================================}
{PL011 Functions}
function PL011UARTCreate(Address:LongWord;const Name:String;IRQ,ClockRate:LongWord):PUARTDevice;
{Create and register a new PL011 UART device which can be accessed using the UART API}
{Address: The address of the PL011 registers}
{Name: The text description of this device which will show in the device list (Optional)}
{IRQ: The interrupt number for the PL011}
{ClockRate: The clock source frequency for the PL011}
{Return: Pointer to the new UART device or nil if the UART device could not be created}
var
 Status:LongWord;
 PL011UART:PPL011UART;
begin
 {}
 Result:=nil;
 
 {$IF DEFINED(PL011_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(nil,'PL011: UART Create (Address=' + IntToHex(Address,8) + ' Name=' + Name + ' IRQ=' + IntToStr(IRQ) + ' ClockRate=' + IntToStr(ClockRate) + ')');
 {$ENDIF}
 
 {Check Address}
 if Address = 0 then Exit;
 
 {Check IRQ}
 {if IRQ = 0 then Exit;} {IRQ 0 is valid}
 
 {Check Clock Rate}
 if ClockRate = 0 then Exit;
 
 {Create UART}
 PL011UART:=PPL011UART(UARTDeviceCreateEx(SizeOf(TPL011UART)));
 if PL011UART <> nil then
  begin
   {Update UART}
   {Device}
   PL011UART.UART.Device.DeviceBus:=DEVICE_BUS_MMIO; 
   PL011UART.UART.Device.DeviceType:=UART_TYPE_16650;
   PL011UART.UART.Device.DeviceFlags:=UART_FLAG_DATA_8BIT or UART_FLAG_DATA_7BIT or UART_FLAG_DATA_6BIT or UART_FLAG_DATA_5BIT or UART_FLAG_STOP_1BIT or UART_FLAG_STOP_2BIT or UART_FLAG_PARITY_ODD or UART_FLAG_PARITY_EVEN or UART_FLAG_FLOW_RTS_CTS;
   PL011UART.UART.Device.DeviceData:=nil;
   if Length(Name) <> 0 then PL011UART.UART.Device.DeviceDescription:=Name else PL011UART.UART.Device.DeviceDescription:=PL011_UART_DESCRIPTION;
   {UART}
   PL011UART.UART.UARTMode:=UART_MODE_NONE;
   PL011UART.UART.UARTState:=UART_STATE_DISABLED;
   PL011UART.UART.UARTStatus:=UART_STATUS_NONE;
   PL011UART.UART.DeviceOpen:=PL011UARTOpen;
   PL011UART.UART.DeviceClose:=PL011UARTClose;
   PL011UART.UART.DeviceRead:=PL011UARTRead;
   PL011UART.UART.DeviceWrite:=PL011UARTWrite;
   PL011UART.UART.DeviceStatus:=PL011UARTStatus;
   {Driver}
   PL011UART.UART.Properties.Flags:=PL011UART.UART.Device.DeviceFlags;
   PL011UART.UART.Properties.MinRate:=PL011_UART_MIN_BAUD;
   PL011UART.UART.Properties.MaxRate:=PL011_UART_MAX_BAUD;
   PL011UART.UART.Properties.BaudRate:=SERIAL_BAUD_RATE_DEFAULT;
   PL011UART.UART.Properties.DataBits:=SERIAL_DATA_8BIT;
   PL011UART.UART.Properties.StopBits:=SERIAL_STOP_1BIT;
   PL011UART.UART.Properties.Parity:=SERIAL_PARITY_NONE;
   PL011UART.UART.Properties.FlowControl:=SERIAL_FLOW_NONE;
   {PL011}
   PL011UART.IRQ:=IRQ;
   PL011UART.Lock:=INVALID_HANDLE_VALUE;
   PL011UART.ClockRate:=ClockRate;
   PL011UART.Registers:=PPL011UARTRegisters(Address);
   
   {Register UART}
   Status:=UARTDeviceRegister(@PL011UART.UART);
   if Status = ERROR_SUCCESS then
    begin
     {Return Result}
     Result:=PUARTDevice(PL011UART); 
    end
   else
    begin
     if UART_LOG_ENABLED then UARTLogError(nil,'PL011: Failed to register new UART device: ' + ErrorToString(Status));
    end;
  end
 else 
  begin
   if UART_LOG_ENABLED then UARTLogError(nil,'PL011: Failed to create new UART device');
  end;
end;

{==============================================================================}

function PL011UARTDestroy(UART:PUARTDevice):LongWord;
{Close, deregister and destroy a PL011 UART device created by this driver}
{UART: The UART device to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check UART}
 if UART = nil then Exit;
 
 {$IF DEFINED(PL011_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'PL011: UART Destroy');
 {$ENDIF}
 
 {Close UART}
 Result:=UARTDeviceClose(UART);
 if Result = ERROR_SUCCESS then
  begin
   {Deregister UART}
   Result:=UARTDeviceDeregister(UART);
   if Result = ERROR_SUCCESS then
    begin
     {Destroy UART}
     Result:=UARTDeviceDestroy(UART);
     if Result <> ERROR_SUCCESS then
      begin
       if UART_LOG_ENABLED then UARTLogError(nil,'PL011: Failed to destroy UART device: ' + ErrorToString(Result));
      end;
    end
   else
    begin
     if UART_LOG_ENABLED then UARTLogError(nil,'PL011: Failed to deregister UART device: ' + ErrorToString(Result));
    end;    
  end
 else
  begin
   if UART_LOG_ENABLED then UARTLogError(nil,'PL011: Failed to close UART device: ' + ErrorToString(Result));
  end;  
end;

{==============================================================================}
{==============================================================================}
{PL011 UART Functions}
function PL011UARTOpen(UART:PUARTDevice;BaudRate,DataBits,StopBits,Parity,FlowControl:LongWord):LongWord;
{Implementation of UARTDeviceOpen API for PL011 UART}
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
 
 {$IF DEFINED(PL011_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'PL011: UART Open (BaudRate=' + IntToStr(BaudRate) + ' DataBits=' + IntToStr(DataBits) + ' StopBits=' + IntToStr(StopBits) + ' Parity=' + IntToStr(Parity) + ' FlowControl=' + IntToStr(FlowControl) + ')');
 {$ENDIF}
 
 {Update Clock Rate}
 if PPL011UART(UART).ClockRate = 0 then PPL011UART(UART).ClockRate:=PL011_UART_CLOCK_RATE; 
 
 {Update Properties}
 UART.Properties.MaxRate:=PPL011UART(UART).ClockRate div 16;
 
 {$IF DEFINED(PL011_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'PL011:  ClockRate=' + IntToStr(PPL011UART(UART).ClockRate) + ' MaxRate=' + IntToStr(UART.Properties.MaxRate));
 {$ENDIF}
 
 {Check Baud Rate}
 if ((BaudRate < PL011_UART_MIN_BAUD) or (BaudRate > UART.Properties.MaxRate)) and (BaudRate <> SERIAL_BAUD_RATE_DEFAULT) then Exit;
 
 {Check Data Bits}
 if (DataBits < PL011_UART_MIN_DATABITS) or (DataBits > PL011_UART_MAX_DATABITS) then Exit;
 
 {Check Stop Bits}
 if (StopBits < PL011_UART_MIN_STOPBITS) or (StopBits > PL011_UART_MAX_STOPBITS) then Exit;
 
 {Check Parity}
 if Parity > PL011_UART_MAX_PARITY then Exit;
 
 {Check Flow Control}
 if FlowControl > PL011_UART_MAX_FLOW then Exit;
 
 {Adjust Baud Rate}
 if BaudRate = SERIAL_BAUD_RATE_DEFAULT then
  begin
   BaudRate:=SERIAL_BAUD_RATE_STANDARD;
   if (BaudRate > UART.Properties.MaxRate) then BaudRate:=SERIAL_BAUD_RATE_FALLBACK;
  end; 
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
  
 {Reset Control (Disable UART)}
 PPL011UART(UART).Registers.CR:=0;
 
 {Reset Interrupt Mask (Disable Interrupts)}
 PPL011UART(UART).Registers.IMSC:=0;
 
 {Ackowledge Interrupts}
 PPL011UART(UART).Registers.ICR:=$7FF;
 
 {Reset Line Control (Flush FIFOs)}
 PPL011UART(UART).Registers.LCRH:=0;
 
 {Calculate Divisor}
 if BaudRate > (PPL011UART(UART).ClockRate div 16) then
  begin
   Divisor:=DivRoundClosest(PPL011UART(UART).ClockRate * 8,BaudRate);
  end
 else
  begin
   Divisor:=DivRoundClosest(PPL011UART(UART).ClockRate * 4,BaudRate);
  end;
  
 {$IF DEFINED(PL011_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'PL011:  BaudRate=' + IntToStr(BaudRate) + ' Divisor=' + IntToStr(Divisor) + ' Divisor shr 6=' + IntToStr(Divisor shr 6) + ' Divisor and $3F=' + IntToStr(Divisor and $3f));
 {$ENDIF}

 {Set Baud Rate}
 PPL011UART(UART).Registers.FBRD:=Divisor and $3f;
 PPL011UART(UART).Registers.IBRD:=Divisor shr 6;
  
 {$IF DEFINED(PL011_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'PL011:  Integer Divisor=' + IntToStr(PPL011UART(UART).Registers.IBRD));
 if UART_LOG_ENABLED then UARTLogDebug(UART,'PL011:  Fractional Divisor=' + IntToStr(PPL011UART(UART).Registers.FBRD));
 {$ENDIF}
  
 {Get Line Control}
 LineControl:=PL011_UART_LCRH_FEN;
 {Data Bits}
 case DataBits of
  SERIAL_DATA_8BIT:LineControl:=LineControl or PL011_UART_LCRH_WLEN8;
  SERIAL_DATA_7BIT:LineControl:=LineControl or PL011_UART_LCRH_WLEN7;
  SERIAL_DATA_6BIT:LineControl:=LineControl or PL011_UART_LCRH_WLEN6;
  SERIAL_DATA_5BIT:LineControl:=LineControl or PL011_UART_LCRH_WLEN5;
 end;
 {Stop Bits}
 case StopBits of
  SERIAL_STOP_2BIT:LineControl:=LineControl or PL011_UART_LCRH_STP2;
 end;
 {Parity}
 case Parity of
  SERIAL_PARITY_ODD:LineControl:=LineControl or PL011_UART_LCRH_PEN;
  SERIAL_PARITY_EVEN:LineControl:=LineControl or PL011_UART_LCRH_PEN or PL011_UART_LCRH_EPS;
 end;
 
 {Set Line Control}
 PPL011UART(UART).Registers.LCRH:=LineControl;
 
 {$IF DEFINED(PL011_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'PL011:  Line Control=' + IntToHex(PPL011UART(UART).Registers.LCRH,8));
 {$ENDIF}
 
 {Set Interrupt FIFO Level}
 PPL011UART(UART).Registers.IFLS:=PL011_UART_IFLS_RXIFLSEL1_8 or PL011_UART_IFLS_TXIFLSEL1_8; {PL011_UART_IFLS_RXIFLSEL1_2 / PL011_UART_IFLS_TXIFLSEL1_2}

 {$IF DEFINED(PL011_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'PL011:  Interrupt FIFO Level=' + IntToHex(PPL011UART(UART).Registers.IFLS,8));
 {$ENDIF}
 
 {Get Control} 
 Control:=PL011_UART_CR_RXE or PL011_UART_CR_TXE or PL011_UART_CR_UARTEN;
 {Flow Control}
 case FlowControl of
  SERIAL_FLOW_RTS_CTS:Control:=Control or PL011_UART_CR_CTSEN or PL011_UART_CR_RTSEN;
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
 PPL011UART(UART).Lock:=SpinCreate;
 if PPL011UART(UART).Lock = INVALID_HANDLE_VALUE then
  begin
   if UART_LOG_ENABLED then UARTLogError(UART,'PL011: Failed to create device lock');

   EventDestroy(UART.TransmitWait);
   EventDestroy(UART.ReceiveWait);
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end; 
 
 {Set Control (Enable UART)}
 PPL011UART(UART).Registers.CR:=Control;
 
 {$IF DEFINED(PL011_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'PL011:  Control=' + IntToHex(PPL011UART(UART).Registers.CR,8));
 {$ENDIF}
 
 {Request IRQ}
 RequestIRQ(IRQ_ROUTING,PPL011UART(UART).IRQ,TInterruptHandler(PL011UARTInterruptHandler),UART);
 
 {Set Interrupt Mask (Enable Interrupts)}
 PPL011UART(UART).Registers.IMSC:=PL011_UART_IMSC_TXIM or PL011_UART_IMSC_RXIM;
 
 {$IF DEFINED(PL011_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'PL011:  Interrupt Mask=' + IntToHex(PPL011UART(UART).Registers.IMSC,8));
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

function PL011UARTClose(UART:PUARTDevice):LongWord;
{Implementation of UARTDeviceClose API for PL011 UART}
{Note: Not intended to be called directly by applications, use UARTDeviceClose instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check UART}
 if UART = nil then Exit;
 
 {$IF DEFINED(PL011_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'PL011: UART Close');
 {$ENDIF}
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Reset Interrupt Mask (Disable Interrupts)}
 PPL011UART(UART).Registers.IMSC:=0;
 
 {Acknowledge Interrupts}
 PPL011UART(UART).Registers.ICR:=$7FF;
 
 {Release IRQ}
 ReleaseIRQ(IRQ_ROUTING,PPL011UART(UART).IRQ,TInterruptHandler(PL011UARTInterruptHandler),UART);
 
 {Reset Control (Disable UART)}
 PPL011UART(UART).Registers.CR:=0;
 
 {Destroy Lock}
 SpinDestroy(PPL011UART(UART).Lock);
 PPL011UART(UART).Lock:=INVALID_HANDLE_VALUE;
 
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
 
function PL011UARTRead(UART:PUARTDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
{Implementation of UARTDeviceRead API for PL011 UART}
{Note: Not intended to be called directly by applications, use UARTDeviceRead instead}
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
 
 {$IF DEFINED(PL011_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'PL011: UART Read (Size=' + IntToStr(Size) + ')');
 {$ENDIF}
 
 {Read to Buffer}
 Offset:=0;
 Total:=Size;
 while Size > 0 do
  begin
   {Check State}
   if (EventState(UART.ReceiveWait) <> EVENT_STATE_SIGNALED) and ((PPL011UART(UART).Registers.FR and PL011_UART_FR_RXFE) = 0) then
    begin
     {Set Event}
     EventSet(UART.ReceiveWait);
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
   if EventWait(UART.ReceiveWait) = ERROR_SUCCESS then
    begin
     {Acquire the Lock}
     if MutexLock(UART.Lock) = ERROR_SUCCESS then
      begin
       {Memory Barrier}
       DataMemoryBarrier; {Before the First Write}
 
       {Get Status}
       Status:=PPL011UART(UART).Registers.FR;
       while ((Status and PL011_UART_FR_RXFE) = 0) and (Size > 0) do
        begin
         {Read Data}
         Value:=PPL011UART(UART).Registers.DR;
         
         {Check for Error}
         if (Value and PL011_UART_DR_ERROR) <> 0 then
          begin
           {Check Error}
           if (Value and PL011_UART_DR_OE) <> 0 then
            begin
             if UART_LOG_ENABLED then UARTLogError(UART,'PL011: Overrun error on receive character'); 
            end;
           if (Value and PL011_UART_DR_BE) <> 0 then
            begin
             if UART_LOG_ENABLED then UARTLogError(UART,'PL011: Break error on receive character'); 
            end;
           if (Value and PL011_UART_DR_PE) <> 0 then
            begin
             if UART_LOG_ENABLED then UARTLogError(UART,'PL011: Parity error on receive character'); 
            end;
           if (Value and PL011_UART_DR_FE) <> 0 then
            begin
             if UART_LOG_ENABLED then UARTLogError(UART,'PL011: Framing error on receive character'); 
            end;
           
           {Update Statistics}
           Inc(UART.ReceiveErrors);
          end;
          
         {Save Data}
         PByte(Buffer + Offset)^:=Value and PL011_UART_DR_DATA;
         
         {Update Statistics}
         Inc(UART.ReceiveCount);
         
         {Update Count}
         Inc(Count);
         
         {Update Size and Offset}
         Dec(Size);
         Inc(Offset);
         
         {Get Status}
         Status:=PPL011UART(UART).Registers.FR;
        end;
        
       {Check Status}
       if (Status and PL011_UART_FR_RXFE) <> 0 then
        begin
         {Check Mask}
         if PL011_RX_IRQ_MASK then
          begin
           {Enable Receive}
           PL011UARTEnableInterrupt(PPL011UART(UART),PL011_UART_IMSC_RXIM);
          end; 
         
         {Reset Event}
         EventReset(UART.ReceiveWait);
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
 
 {$IF DEFINED(PL011_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'PL011:  Return Count=' + IntToStr(Count));
 {$ENDIF}
 
 {Return Result}
 if (Total = Count) then Result:=ERROR_SUCCESS; 
end;

{==============================================================================}

function PL011UARTWrite(UART:PUARTDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
{Implementation of UARTDeviceWrite API for PL011 UART}
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
 
 {$IF DEFINED(PL011_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'PL011: UART Write (Size=' + IntToStr(Size) + ')');
 {$ENDIF}
 
 {Write from Buffer}
 Offset:=0;
 Total:=Size;
 while Size > 0 do
  begin
   {Check State}
   if (EventState(UART.TransmitWait) <> EVENT_STATE_SIGNALED) and ((PPL011UART(UART).Registers.FR and PL011_UART_FR_TXFF) = 0) then
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
       Status:=PPL011UART(UART).Registers.FR;
       while ((Status and PL011_UART_FR_TXFF) = 0) and (Size > 0) do
        begin
         {Write Data}
         PPL011UART(UART).Registers.DR:=PByte(Buffer + Offset)^;
         
         {Update Statistics}
         Inc(UART.TransmitCount);
         
         {Update Count}
         Inc(Count);
         
         {Update Size and Offset}
         Dec(Size);
         Inc(Offset);
         
         {Get Status}
         Status:=PPL011UART(UART).Registers.FR;
        end;
        
       {Check Status}
       if (Status and PL011_UART_FR_TXFF) <> 0 then
        begin
         {Enable Transmit}
         PL011UARTEnableInterrupt(PPL011UART(UART),PL011_UART_IMSC_TXIM);
         
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
  
 {$IF DEFINED(PL011_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'PL011:  Return Count=' + IntToStr(Count));
 {$ENDIF}
 
 {Return Result}
 if (Total = Count) then Result:=ERROR_SUCCESS;
end;

{==============================================================================}
 
function PL011UARTStatus(UART:PUARTDevice):LongWord;
{Implementation of UARTDeviceStatus API for PL011 UART}
{Note: Not intended to be called directly by applications, use UARTDeviceStatus instead}
var
 Flags:LongWord;
 Status:LongWord;
 Control:LongWord;
begin
 {}
 Result:=UART_STATUS_NONE;
 
 {Check UART}
 if UART = nil then Exit;
 
 {$IF DEFINED(PL011_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'PL011: UART Status');
 {$ENDIF}
 
 {Get Flags}
 Flags:=PPL011UART(UART).Registers.FR;
 if (Flags and PL011_UART_FR_CTS) <> 0 then
  begin
   Result:=Result or UART_STATUS_CTS;
  end;
 if (Flags and PL011_UART_FR_RXFF) <> 0 then
  begin
   Result:=Result or UART_STATUS_RX_FULL;
  end;
 if (Flags and PL011_UART_FR_RXFE) <> 0 then
  begin
   Result:=Result or UART_STATUS_RX_EMPTY;
  end;
 if (Flags and PL011_UART_FR_TXFF) <> 0 then
  begin
   Result:=Result or UART_STATUS_TX_FULL;
  end;
 if (Flags and PL011_UART_FR_TXFE) <> 0 then
  begin
   Result:=Result or UART_STATUS_TX_EMPTY;
  end;
 if (Flags and PL011_UART_FR_BUSY) <> 0 then
  begin
   Result:=Result or UART_STATUS_BUSY;
  end;
 
 {Get Status}
 Status:=PPL011UART(UART).Registers.RSRECR;
 if (Status and PL011_UART_RSRECR_OE) <> 0 then
  begin
   Result:=Result or UART_STATUS_OVERRUN_ERROR;
  end;
 if (Status and PL011_UART_RSRECR_BE) <> 0 then
  begin
   Result:=Result or UART_STATUS_BREAK_ERROR;
  end;
 if (Status and PL011_UART_RSRECR_PE) <> 0 then
  begin
   Result:=Result or UART_STATUS_PARITY_ERROR;
  end;
 if (Status and PL011_UART_RSRECR_FE) <> 0 then
  begin
   Result:=Result or UART_STATUS_FRAMING_ERROR;
  end;

 {Get Control}
 Control:=PPL011UART(UART).Registers.CR;
 if (Control and PL011_UART_CR_RTS) <> 0 then
  begin
   Result:=Result or UART_STATUS_RTS;
  end;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
end;

{==============================================================================}

procedure PL011UARTInterruptHandler(UART:PUARTDevice);
{Interrupt handler for the PL011 UART device}
{Note: Not intended to be called directly by applications}
var
 Status:LongWord;
begin
 {}
 {Check UART}
 if UART = nil then Exit;
 
 {Acquire Lock}
 if SpinLockIRQ(PPL011UART(UART).Lock) <> ERROR_SUCCESS then Exit;
 
 {Update Statistics}
 Inc(PPL011UART(UART).InterruptCount);
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Get Interrupt Status}
 Status:=PPL011UART(UART).Registers.MIS;
 if Status <> 0 then
  begin
   {Acknowledge Interrupts}
   PPL011UART(UART).Registers.ICR:=Status and not(PL011_UART_ICR_TXIC or PL011_UART_ICR_RXIC);
   
   {Check Transmit}
   if (Status and PL011_UART_MIS_TXMIS) <> 0 then
    begin
     {Acknowledge Transmit}
     PPL011UART(UART).Registers.ICR:=PL011_UART_ICR_TXIC;
     
     {Send Transmit}
     if WorkerScheduleIRQ(CPU_AFFINITY_NONE,TWorkerTask(PL011UARTTransmit),UART,nil) = ERROR_SUCCESS then
      begin
       {Mask Transmit}
       PPL011UART(UART).Registers.IMSC:=PPL011UART(UART).Registers.IMSC and not(PL011_UART_IMSC_TXIM);
      end; 
    end;
    
   {Check Receive}
   if (Status and PL011_UART_MIS_RXMIS) <> 0 then
    begin
     {Acknowledge Receive}
     PPL011UART(UART).Registers.ICR:=PL011_UART_ICR_RXIC;

     {Send Receive and Check Mask}
     if (WorkerScheduleIRQ(CPU_AFFINITY_NONE,TWorkerTask(PL011UARTReceive),UART,nil) = ERROR_SUCCESS) and (PL011_RX_IRQ_MASK) then
      begin
       {Mask Receive}
       PPL011UART(UART).Registers.IMSC:=PPL011UART(UART).Registers.IMSC and not(PL011_UART_IMSC_RXIM);
      end; 
    end;
  end; 
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Release Lock}
 SpinUnlockIRQ(PPL011UART(UART).Lock);
end;

{==============================================================================}

procedure PL011UARTReceive(UART:PUARTDevice);
{Receive handler for the PL011 UART device}
{Note: Not intended to be called directly by applications}
begin
 {}
 {Check UART}
 if UART = nil then Exit;

 {$IF DEFINED(PL011_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'PL011: UART Receive');
 {$ENDIF}
 
 {Acquire the Lock}
 if MutexLock(UART.Lock) = ERROR_SUCCESS then
  begin
   {Set Event}
   EventSet(UART.ReceiveWait);
   
   {Check Mode}
   if UART.UARTMode = UART_MODE_SERIAL then
    begin
     {Serial Receive}
     UARTSerialDeviceReceive(UART);
    end;

   {Release the Lock}
   MutexUnlock(UART.Lock);
  end;
end;

{==============================================================================}

procedure PL011UARTTransmit(UART:PUARTDevice);
{Transmit handler for the PL011 UART device}
{Note: Not intended to be called directly by applications}
begin
 {}
 {Check UART}
 if UART = nil then Exit;

 {$IF DEFINED(PL011_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'PL011: UART Transmit');
 {$ENDIF}
 
 {Acquire the Lock}
 if MutexLock(UART.Lock) = ERROR_SUCCESS then
  begin
   {Set Event}
   EventSet(UART.TransmitWait);
   
   {Check Mode}
   if UART.UARTMode = UART_MODE_SERIAL then
    begin
     {Serial Transmit}
     UARTSerialDeviceTransmit(UART);
    end;    

   {Release the Lock}
   MutexUnlock(UART.Lock);
  end;
end;

{==============================================================================}

procedure PL011UARTEnableInterrupt(UART:PPL011UART;Interrupt:LongWord);
{Enable the specified interrupt in the interrupt mask register of a PL011 UART device}
{UART: The PL011 UART device to enable the interrupt for}
{Interrupt: The interrupt to enable}

{Note: Caller must hold the UART lock}
begin
 {}
 {Acquire Lock}
 if SpinLockIRQ(UART.Lock) <> ERROR_SUCCESS then Exit;
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Update Interrupt Mask} 
 UART.Registers.IMSC:=UART.Registers.IMSC or Interrupt;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 

 {Release Lock}
 SpinUnlockIRQ(UART.Lock);
end;

{==============================================================================}

procedure PL011UARTDisableInterrupt(UART:PPL011UART;Interrupt:LongWord);
{Disable the specified interrupt in the interrupt mask register of a PL011 UART device}
{Network: The PL011 UART device to disable the interrupt for}
{Interrupt: The interrupt to disable}

{Note: Caller must hold the UART lock}
begin
 {}
 {Acquire Lock}
 if SpinLockIRQ(UART.Lock) <> ERROR_SUCCESS then Exit;

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Update Interrupt Mask} 
 UART.Registers.IMSC:=UART.Registers.IMSC and not(Interrupt);
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Release Lock}
 SpinUnlockIRQ(UART.Lock);
end;

{==============================================================================}
{==============================================================================}
{PL011 Helper Functions}

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

