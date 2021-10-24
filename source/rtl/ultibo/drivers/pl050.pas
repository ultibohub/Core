{
ARM PrimeCell PL050 PS2 Keyboard/Mouse Interface Driver.

Copyright (C) 2021 - SoftOz Pty Ltd.

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

  Linux - \drivers\input\serio\ambakmi.c - Copyright (C) 2000-2003 Deep Blue Solutions Ltd

  Linux - \drivers\input\serio\serio.c - Copyright (c) 1999-2004 Vojtech Pavlik and others

  Linux - \include\linux\amba\kmi.h - Copyright (C) 2000 Deep Blue Solutions Ltd
 
  QEMU - \hw\input\pl050.c - Copyright (c) 2006-2007 CodeSourcery
  
  QEMU - \hw\input\ps2.c - Copyright (c) 2003 Fabrice Bellard
  
References
==========

 PL050 - http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0143c/index.html
 
ARM PrimeCell PL050 PS2 Keyboard/Mouse
======================================
  
 The PL050 is an Advanced Microcontroller Bus Architecture (AMBA) compliant peripheral that
 implements a PS/2 compatible Keyboard and Mouse interface. This driver supports all of the
 standard functionality of the PL050 KMI controller including both Keyboard and Mouse devices
 as well as setting LEDs, repeat rate, repeat delay and sample rate values.
 
 The driver uses interrupt transfers for receiving keyboard and mouse inputs but uses only
 polling mode for transmitting data to the keyboard or mouse device. This driver has only
 been tested with the QEMU Versatile PB emulation of the PL050 device and has not been
 confirmed to work with real hardware.
  
 Notes: 
 
  Currently QEMU only supports Scancode set 2 so scancode sets 1 and 3 are not implemented.
  
  On Windows, QEMU appears to send invalid scancodes for Print Screen and Pause/Break keys.
  
  On Windows, QEMU does not differentiate between the cursor keys (Up, Down, Left, Right,
  Home, End, PgUp, PgDown) and the numeric keypad keys so each of these will always return
  their numeric keypad equivalent. This means that to use the cursor keys you need to turn
  off Number Lock otherwise they are interpreted as number keys instead.
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit PL050; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,Keyboard,Keymap,Mouse,PS2,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}
        
{==============================================================================}
const
 {PL050 specific constants}
 PL050_KEYBOARD_DESCRIPTION = 'ARM PrimeCell PL050 PS2 Keyboard';  {Description of PL050 Keyboard device}
 PL050_MOUSE_DESCRIPTION = 'ARM PrimeCell PL050 PS2 Mouse';        {Description of PL050 Mouse device}

 PL050_KEYBOARD_SCANCODE_COUNT = 256; {Number of keyboard scancode buffers for receive}
 PL050_MOUSE_PACKET_COUNT = 256;      {Number of mouse packet buffers for receive}
 
 PL050_KEYBOARD_CLOCK_RATE = 8000000; {Default clock rate}
 PL050_MOUSE_CLOCK_RATE = 8000000;    {Default clock rate}
 
 PL050_KEYBOARD_SHIFTSTATE_MASK = KEYBOARD_LEFT_CTRL or KEYBOARD_LEFT_SHIFT or KEYBOARD_LEFT_ALT or KEYBOARD_RIGHT_CTRL or KEYBOARD_RIGHT_SHIFT or KEYBOARD_RIGHT_ALT;
 
const
 {PL050 Control register bits}
 PL050_CR_TYPE     = (1 shl 5); {0 = PS2/AT mode / 1 = No line control bit mode}
 PL050_CR_RXINTREN = (1 shl 4); {Enable receiver interrupt. This bit field is used to enable the PrimeCell KMI receiver interrupt (If KMIRXINTREn = 1, the receiver interrupt is enabled)}
 PL050_CR_TXINTREN = (1 shl 3); {Enable transmitter interrupt. This bit field is used to enable the PrimeCell KMI transmitter interrupt (If KMITXINTREn = 1, the transfer interrupt is enabled)}
 PL050_CR_EN       = (1 shl 2); {The enable PrimeCell KMI bit field is used to enable the KMI (If KmiEn = 1, the KMI is enabled)} 
 PL050_CR_FDL      = (1 shl 1); {The force KMI data LOW bit field is used to force the PrimeCell KMI data pad LOW regardless of the state of the KMI finite state machine (FSM) (If FKMID = 1, the PrimeCell KMI data pad is forced LOW)} 
 PL050_CR_FCL      = (1 shl 0); {The force KMI clock LOW bit field is used to force the PrimeCell KMI clock pad LOW regardless of the state of the KMI FSM (If FKMIC = 1, the PrimeCell KMI clock pad is forced LOW)} 
 
 {PL050 Status register bits}
 PL050_STAT_TXEMPTY  = (1 shl 6); {This bit indicates that the transmit register is empty and ready to transmit (0 = Transmit register full / 1 = Transmit register empty, ready to be written)}
 PL050_STAT_TXBUSY   = (1 shl 5); {This bit indicates that the PrimeCell KMI is currently sending data (0 = Idle / 1 = Currently sending data)}
 PL050_STAT_RXFULL   = (1 shl 4); {This bit indicates that the receiver register is full and ready to be read (0 = Receive register empty / 1 = Receive register full, ready to be read)}
 PL050_STAT_RXBUSY   = (1 shl 3); {This bit indicates that the PrimeCell KMI is currently receiving data (0 = Idle / 1 = Currently receiving data)}
 PL050_STAT_RXPARITY = (1 shl 2); {This bit reflects the parity bit for the last received data byte (odd parity)}
 PL050_STAT_IC       = (1 shl 1); {This bit reflects the status of the KMICLKIN line after synchronizing and sampling}
 PL050_STAT_ID       = (1 shl 0); {This bit reflects the status of the KMIDATAIN line after synchronizing}
 
 {PL050 Interrupt register bits}
 PL050_IIR_TXINTR = (1 shl 1); {This bit is set to 1 if the KMITXINTR transmit interrupt is asserted}
 PL050_IIR_RXINTR = (1 shl 0); {This bit is set to 1 if the KMIRXINTR receive interrupt is asserted}
 
{==============================================================================}
type
 {PL050 specific types}
 {Layout of the PL050 registers (See: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0143c/i1005653.html)}
 PPL050KMIRegisters = ^TPL050KMIRegisters;
 TPL050KMIRegisters = record
  CR:LongWord;     {Control register}
  STAT:LongWord;   {Status register}
  DATA:LongWord;   {Received data (read)/ Data to be transmitted (write)}
  CLKDIV:LongWord; {Clock divisor register}
  IIR:LongWord;    {Interrupt status register}
 end; 
 
 PPL050Keyboard = ^TPL050Keyboard;
 
 PPL050KeyboardScancode = ^TPL050KeyboardScancode;
 TPL050KeyboardScancode = record
  Keyboard:PPL050Keyboard;
  Count:LongWord;
  Index:LongInt;
  Scancode:TPS2KeyboardScancode;
 end;
 
 TPL050Keyboard = record
  {Keyboard Properties}
  Keyboard:TKeyboardDevice;
  {PL050 Properties}
  IRQ:LongWord;
  Registers:PPL050KMIRegisters;                                                         {Device registers}
  ScancodeSet:Byte;                                                                     {The currently selected scancode set}
  ScancodeData:array[0..(PL050_KEYBOARD_SCANCODE_COUNT - 1)] of TPL050KeyboardScancode; {Buffers for scancode receiving}
  ScancodeStart:LongWord;                                                               {The scancode buffer to use for the next receive}
  ScancodeCount:LongWord;                                                               {The number of scancode buffers that are in use}
  LastCode:Word;                                                                        {The scan code of the last key pressed}
  ShiftState:LongWord;                                                                  {The modifier flags of the current shift state}
  {Statistics Properties}                                                               
  DiscardCount:LongWord;                                                                {Number of received bytes discarded (due to no buffer or other reasons)}
  InterruptCount:LongWord;                                                              {Number of interrupt requests received by the device}
 end; 
 
 PPL050Mouse = ^TPL050Mouse;
 
 PPL050MousePacket = ^TPL050MousePacket;
 TPL050MousePacket = record
  Mouse:PPL050Mouse;
  Count:LongWord;
  Packet:TPS2MousePacket;
 end;
 
 TPL050Mouse = record
  {Mouse Properties}
  Mouse:TMouseDevice;
  {PL050 Properties}
  IRQ:LongWord;
  Registers:PPL050KMIRegisters;                                             {Device registers}
  PacketData:array[0..(PL050_MOUSE_PACKET_COUNT - 1)] of TPL050MousePacket; {Buffers for mouse packet receiving}
  PacketStart:LongWord;                                                     {The mouse packet buffer to use for the next receive}
  PacketCount:LongWord;                                                     {The number of mouse packet buffers that are in use}
  {Statistics Properties}                                                   
  DiscardCount:LongWord;                                                    {Number of received bytes discarded (due to no buffer or other reasons)}
  InterruptCount:LongWord;                                                  {Number of interrupt requests received by the device}
 end; 
 
{==============================================================================}
{var}
 {PL050 specific variables}
 
{==============================================================================}
{Initialization Functions}
 
{==============================================================================}
{PL050 Functions}
function PL050KeyboardCreate(Address:PtrUInt;const Name:String;IRQ,ClockRate:LongWord):PKeyboardDevice;

function PL050KeyboardDestroy(Keyboard:PKeyboardDevice):LongWord;

function PL050MouseCreate(Address:PtrUInt;const Name:String;IRQ,ClockRate:LongWord):PMouseDevice;

function PL050MouseDestroy(Mouse:PMouseDevice):LongWord;
 
{==============================================================================}
{PL050 Keyboard Functions}
function PL050KeyboardControl(Keyboard:PKeyboardDevice;Request:Integer;Argument1:LongWord;var Argument2:LongWord):LongWord;

procedure PL050KeyboardInterruptHandler(Keyboard:PKeyboardDevice);

procedure PL050KeyboardWorker(Scancode:PPL050KeyboardScancode);

{==============================================================================}
{PL050 Mouse Functions}
function PL050MouseControl(Mouse:PMouseDevice;Request:Integer;Argument1:LongWord;var Argument2:LongWord):LongWord;
 
procedure PL050MouseInterruptHandler(Mouse:PMouseDevice);

procedure PL050MouseWorker(Packet:PPL050MousePacket);

{==============================================================================}
{PL050 Helper Functions}
function PL050KMIClear(Registers:PPL050KMIRegisters):LongWord;
function PL050KMIRead(Registers:PPL050KMIRegisters;var Value:Byte):LongWord;
function PL050KMIWrite(Registers:PPL050KMIRegisters;Value:Byte):LongWord;

function PL050KMICommand(Registers:PPL050KMIRegisters;Command:Byte;Data:PByte;DataSize:LongWord;Response:PByte;ResponseSize:LongWord):LongWord;

function PL050KMIKeyboardReset(Keyboard:PPL050Keyboard):LongWord;
function PL050KMIKeyboardEnable(Keyboard:PPL050Keyboard):LongWord;
function PL050KMIKeyboardDisable(Keyboard:PPL050Keyboard):LongWord;

function PL050KMIKeyboardSetLEDs(Keyboard:PPL050Keyboard;LEDs:LongWord):LongWord;

function PL050KMIKeyboardSetTypematic(Keyboard:PPL050Keyboard;Rate,Delay:LongWord):LongWord;

function PL050KMIKeyboardGetScancodeSet(Keyboard:PPL050Keyboard;var ScancodeSet:Byte):LongWord;
function PL050KMIKeyboardSetScancodeSet(Keyboard:PPL050Keyboard;ScancodeSet:Byte):LongWord;

function PL050KMIKeyboardCheckPressed(Keyboard:PPL050Keyboard;ScanCode:Word):Boolean;
function PL050KMIKeyboardCheckRepeated(Keyboard:PPL050Keyboard;ScanCode:Word):Boolean;

function PL050KMIMouseReset(Mouse:PPL050Mouse):LongWord;
function PL050KMIMouseEnable(Mouse:PPL050Mouse):LongWord;
function PL050KMIMouseDisable(Mouse:PPL050Mouse):LongWord;

function PL050KMIMouseSetSampleRate(Mouse:PPL050Mouse;Rate:Byte):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{var}
 {PL050 specific variables}
 
{==============================================================================}
{==============================================================================}
{Initialization Functions}
 
{==============================================================================}
{==============================================================================}
{PL050 Functions}
function PL050KeyboardCreate(Address:PtrUInt;const Name:String;IRQ,ClockRate:LongWord):PKeyboardDevice;
{Create, register and attach a new PL050 Keyboard device which can be accessed using the keyboard API}
{Address: The address of the PL050 registers}
{Name: The text description of this device which will show in the device list (Optional)}
{IRQ: The interrupt number for the PL050}
{ClockRate: The clock source frequency for the PL050}
{Return: Pointer to the new Keyboard device or nil if the Keyboard device could not be created}
var
 Status:LongWord;
 Divisor:LongWord;
 PL050Keyboard:PPL050Keyboard;
begin
 {}
 Result:=nil;
 
 {$IF DEFINED(PL050_DEBUG) or DEFINED(KEYBOARD_DEBUG)}
 if KEYBOARD_LOG_ENABLED then KeyboardLogDebug(nil,'PL050: Keyboard Create (Address=' + AddrToHex(Address) + ' Name=' + Name + ' IRQ=' + IntToStr(IRQ) + ')');
 {$ENDIF}

 {Check Address}
 if Address = 0 then Exit;
 
 {Check IRQ}
 {if IRQ = 0 then Exit;} {IRQ 0 is valid}
 
 {Create Keyboard}
 PL050Keyboard:=PPL050Keyboard(KeyboardDeviceCreateEx(SizeOf(TPL050Keyboard)));
 if PL050Keyboard <> nil then
  begin
   {Update Keyboard}
   {Device}
   PL050Keyboard.Keyboard.Device.DeviceBus:=DEVICE_BUS_MMIO; 
   PL050Keyboard.Keyboard.Device.DeviceType:=KEYBOARD_TYPE_PS2;
   PL050Keyboard.Keyboard.Device.DeviceFlags:=PL050Keyboard.Keyboard.Device.DeviceFlags; {Don't override defaults}
   PL050Keyboard.Keyboard.Device.DeviceData:=nil;
   if Length(Name) <> 0 then PL050Keyboard.Keyboard.Device.DeviceDescription:=Name else PL050Keyboard.Keyboard.Device.DeviceDescription:=PL050_KEYBOARD_DESCRIPTION;
   {Keyboard}
   PL050Keyboard.Keyboard.KeyboardState:=KEYBOARD_STATE_ATTACHING;
   PL050Keyboard.Keyboard.DeviceControl:=PL050KeyboardControl;
   {PL050}
   PL050Keyboard.IRQ:=IRQ;
   PL050Keyboard.Registers:=PPL050KMIRegisters(Address);
 
   {Register Keyboard}
   Status:=KeyboardDeviceRegister(@PL050Keyboard.Keyboard);
   if Status = ERROR_SUCCESS then
    begin
     {Memory Barrier}
     DataMemoryBarrier; {Before the First Write}
     
     {Check Clock}
     if ClockRate = 0 then
      begin
       ClockRate:=PL050_KEYBOARD_CLOCK_RATE;
      end;
      
     {Calculate Divisor}
     Divisor:=(ClockRate div 8000000) - 1;
     
     {Setup Divisor (Divider ratio is 1/1+CLKDIV so 0 gives a divide by 1)}
     PL050Keyboard.Registers.CLKDIV:=Divisor;
     
     {Request IRQ}
     RequestIRQ(IRQ_ROUTING,PL050Keyboard.IRQ,TInterruptHandler(PL050KeyboardInterruptHandler),PL050Keyboard);
    
     {Setup Control (Enable KMI)}
     PL050Keyboard.Registers.CR:=PL050_CR_EN;
     
     {Reset Keyboard}
     Status:=PL050KMIKeyboardReset(PL050Keyboard);
     if Status <> ERROR_SUCCESS then
      begin
       if KEYBOARD_LOG_ENABLED then KeyboardLogError(nil,'PL050: Failed to reset KMI Keyboard device: ' + ErrorToString(Status));
       Exit;
      end; 
     
     {Enable Keyboard}
     Status:=PL050KMIKeyboardEnable(PL050Keyboard);
     if Status <> ERROR_SUCCESS then
      begin
       if KEYBOARD_LOG_ENABLED then KeyboardLogError(nil,'PL050: Failed to enable KMI Keyboard device: ' + ErrorToString(Status));
       Exit;
      end; 
     
     {Get Scancode Set} {Do not read the current scancode set, may fail on some versions of QEMU}
     {Status:=PL050KMIKeyboardGetScancodeSet(PL050Keyboard,PL050Keyboard.ScancodeSet);
     if Status <> ERROR_SUCCESS then
      begin
       if KEYBOARD_LOG_ENABLED then KeyboardLogError(nil,'PL050: Failed to get scancode set from KMI Keyboard device: ' + ErrorToString(Status));
       Exit;
      end;}
     
     {Check Scancode Set}
     if PL050Keyboard.ScancodeSet <> PS2_KEYBOARD_SCANCODE_SET2 then
      begin
       {Set Scancode Set}
       Status:=PL050KMIKeyboardSetScancodeSet(PL050Keyboard,PS2_KEYBOARD_SCANCODE_SET2);
       if Status <> ERROR_SUCCESS then
        begin
         if KEYBOARD_LOG_ENABLED then KeyboardLogError(nil,'PL050: Failed to set scancode set for KMI Keyboard device: ' + ErrorToString(Status));
         Exit;
        end; 
      end;
      
     {Set Typematic}
     Status:=PL050KMIKeyboardSetTypematic(PL050Keyboard,PL050Keyboard.Keyboard.KeyboardRate,PL050Keyboard.Keyboard.KeyboardDelay);
     if Status <> ERROR_SUCCESS then
      begin
       if KEYBOARD_LOG_ENABLED then KeyboardLogError(nil,'PL050: Failed to set repeat rate and delay for KMI Keyboard device: ' + ErrorToString(Status));
       {Exit;} {Do not fail, will work without this setting}
      end; 
     
     {Set LEDS}
     Status:=PL050KMIKeyboardSetLEDs(PL050Keyboard,PL050Keyboard.Keyboard.KeyboardLEDs);
     if Status <> ERROR_SUCCESS then
      begin
       if KEYBOARD_LOG_ENABLED then KeyboardLogError(nil,'PL050: Failed to set LEDs for KMI Keyboard device: ' + ErrorToString(Status));
       {Exit;} {Do not fail, will work without this setting}
      end; 
     
     {Setup Control (Enable RX Interrupt)}
     PL050Keyboard.Registers.CR:=PL050_CR_EN or PL050_CR_RXINTREN;
     
     {Set State to Attached}
     if KeyboardDeviceSetState(@PL050Keyboard.Keyboard,KEYBOARD_STATE_ATTACHED) <> ERROR_SUCCESS then Exit;
     
     {Return Result}
     Result:=PKeyboardDevice(PL050Keyboard); 
    end
   else
    begin
     if KEYBOARD_LOG_ENABLED then KeyboardLogError(nil,'PL050: Failed to register new Keyboard device: ' + ErrorToString(Status));
    
     {Destroy Keyboard}
     KeyboardDeviceDestroy(@PL050Keyboard.Keyboard);
    end;
  end
 else 
  begin
   if KEYBOARD_LOG_ENABLED then KeyboardLogError(nil,'PL050: Failed to create new Keyboard device');
  end;
end;

{==============================================================================}

function PL050KeyboardDestroy(Keyboard:PKeyboardDevice):LongWord;
{Detach, deregister and destroy a PL050 Keyboard device created by this driver}
{Keyboard: The Keyboard device to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Keyboard}
 if Keyboard = nil then Exit;
 
 {$IF DEFINED(PL050_DEBUG) or DEFINED(KEYBOARD_DEBUG)}
 if KEYBOARD_LOG_ENABLED then KeyboardLogDebug(Keyboard,'PL050: Keyboard Destroy');
 {$ENDIF}

 {Disable Keyboard}
 PL050KMIKeyboardDisable(PPL050Keyboard(Keyboard));
 
 {Reset Control (Disable KMI and Interrupts)}
 PPL050Keyboard(Keyboard).Registers.CR:=0;
 
 {Release IRQ}
 ReleaseIRQ(IRQ_ROUTING,PPL050Keyboard(Keyboard).IRQ,TInterruptHandler(PL050KeyboardInterruptHandler),Keyboard);
 
 {Deregister Keyboard}
 Result:=KeyboardDeviceDeregister(Keyboard);
 if Result <> ERROR_SUCCESS then
  begin
   if KEYBOARD_LOG_ENABLED then KeyboardLogError(nil,'PL050: Failed to deregister Keyboard device: ' + ErrorToString(Result));
   Exit;
  end;

 {Destroy Keyboard}
 Result:=KeyboardDeviceDestroy(Keyboard);
 if Result <> ERROR_SUCCESS then
  begin
   if KEYBOARD_LOG_ENABLED then KeyboardLogError(nil,'PL050: Failed to destroy Keyboard device: ' + ErrorToString(Result));
   Exit;
  end; 
 
 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function PL050MouseCreate(Address:PtrUInt;const Name:String;IRQ,ClockRate:LongWord):PMouseDevice;
{Create, register and attach a new PL050 Mouse device which can be accessed using the mouse API}
{Address: The address of the PL050 registers}
{Name: The text description of this device which will show in the device list (Optional)}
{IRQ: The interrupt number for the PL050}
{ClockRate: The clock source frequency for the PL050}
{Return: Pointer to the new Mouse device or nil if the Mouse device could not be created}
var
 Status:LongWord;
 Divisor:LongWord;
 PL050Mouse:PPL050Mouse;
begin
 {}
 Result:=nil;
 
 {$IF DEFINED(PL050_DEBUG) or DEFINED(MOUSE_DEBUG)}
 if MOUSE_LOG_ENABLED then MouseLogDebug(nil,'PL050: Mouse Create (Address=' + AddrToHex(Address) + ' Name=' + Name + ' IRQ=' + IntToStr(IRQ) + ')');
 {$ENDIF}

 {Check Address}
 if Address = 0 then Exit;

 {Check IRQ}
 {if IRQ = 0 then Exit;} {IRQ 0 is valid}
 
 {Create Mouse}
 PL050Mouse:=PPL050Mouse(MouseDeviceCreateEx(SizeOf(TPL050Mouse)));
 if PL050Mouse <> nil then
  begin
   {Update Mouse}
   {Device}
   PL050Mouse.Mouse.Device.DeviceBus:=DEVICE_BUS_MMIO; 
   PL050Mouse.Mouse.Device.DeviceType:=MOUSE_TYPE_PS2;
   PL050Mouse.Mouse.Device.DeviceFlags:=PL050Mouse.Mouse.Device.DeviceFlags; {Don't override defaults}
   PL050Mouse.Mouse.Device.DeviceData:=nil;
   if Length(Name) <> 0 then PL050Mouse.Mouse.Device.DeviceDescription:=Name else PL050Mouse.Mouse.Device.DeviceDescription:=PL050_MOUSE_DESCRIPTION;
   {Mouse}
   PL050Mouse.Mouse.MouseState:=MOUSE_STATE_ATTACHING;
   PL050Mouse.Mouse.DeviceControl:=PL050MouseControl;
   {PL050}
   PL050Mouse.IRQ:=IRQ;
   PL050Mouse.Registers:=PPL050KMIRegisters(Address);
 
   {Register Mouse}
   Status:=MouseDeviceRegister(@PL050Mouse.Mouse);
   if Status = ERROR_SUCCESS then
    begin
     {Memory Barrier}
     DataMemoryBarrier; {Before the First Write}
     
     {Check Clock}
     if ClockRate = 0 then
      begin
       ClockRate:=PL050_MOUSE_CLOCK_RATE;
      end;
     
     {Calculate Divisor}
     Divisor:=(ClockRate div 8000000) - 1;
     
     {Setup Divider (Divider ratio is 1/1+CLKDIV so 0 gives a divide by 1)}
     PL050Mouse.Registers.CLKDIV:=Divisor;
     
     {Request IRQ}
     RequestIRQ(IRQ_ROUTING,PL050Mouse.IRQ,TInterruptHandler(PL050MouseInterruptHandler),PL050Mouse);
    
     {Setup Control (Enable KMI)}
     PL050Mouse.Registers.CR:=PL050_CR_EN;
     
     {Reset Mouse}
     Status:=PL050KMIMouseReset(PL050Mouse);
     if Status <> ERROR_SUCCESS then
      begin
       if MOUSE_LOG_ENABLED then MouseLogError(nil,'PL050: Failed to reset KMI Mouse device: ' + ErrorToString(Status));
       Exit;
      end; 
     
     {Enable Mouse}
     Status:=PL050KMIMouseEnable(PL050Mouse);
     if Status <> ERROR_SUCCESS then
      begin
       if MOUSE_LOG_ENABLED then MouseLogError(nil,'PL050: Failed to enable KMI Mouse device: ' + ErrorToString(Status));
       Exit;
      end; 
 
     {Set Sample Rate}
     Status:=PL050KMIMouseSetSampleRate(PL050Mouse,PL050Mouse.Mouse.MouseRate);
     if Status <> ERROR_SUCCESS then
      begin
       if MOUSE_LOG_ENABLED then MouseLogError(nil,'PL050: Failed to set sample rate for KMI Mouse device: ' + ErrorToString(Status));
       {Exit;} {Do not fail, will work without this setting}
      end; 
     
     {Setup Control (Enable RX Interrupt)}
     PL050Mouse.Registers.CR:=PL050_CR_EN or PL050_CR_RXINTREN;
     
     {Set State to Attached}
     if MouseDeviceSetState(@PL050Mouse.Mouse,MOUSE_STATE_ATTACHED) <> ERROR_SUCCESS then Exit;
     
     {Return Result}
     Result:=PMouseDevice(PL050Mouse); 
    end
   else
    begin
     if MOUSE_LOG_ENABLED then MouseLogError(nil,'PL050: Failed to register new Mouse device: ' + ErrorToString(Status));

     {Destroy Mouse}
     MouseDeviceDestroy(@PL050Mouse.Mouse);
    end;
  end
 else 
  begin
   if MOUSE_LOG_ENABLED then MouseLogError(nil,'PL050: Failed to create new Mouse device');
  end;
end;

{==============================================================================}

function PL050MouseDestroy(Mouse:PMouseDevice):LongWord;
{Detach, deregister and destroy a PL050 Mouse device created by this driver}
{Mouse: The Mouse device to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Mouse}
 if Mouse = nil then Exit;
 
 {$IF DEFINED(PL050_DEBUG) or DEFINED(MOUSE_DEBUG)}
 if MOUSE_LOG_ENABLED then MouseLogDebug(Mouse,'PL050: Mouse Destroy');
 {$ENDIF}

 {Disable Mouse}
 PL050KMIMouseDisable(PPL050Mouse(Mouse));
 
 {Reset Control (Disable KMI and Interrupts)}
 PPL050Mouse(Mouse).Registers.CR:=0;
 
 {Release IRQ}
 ReleaseIRQ(IRQ_ROUTING,PPL050Mouse(Mouse).IRQ,TInterruptHandler(PL050MouseInterruptHandler),Mouse);
 
 {Deregister Mouse}
 Result:=MouseDeviceDeregister(Mouse);
 if Result <> ERROR_SUCCESS then
  begin
   if MOUSE_LOG_ENABLED then MouseLogError(nil,'PL050: Failed to deregister Mouse device: ' + ErrorToString(Result));
   Exit;
  end;

 {Destroy Mouse}
 Result:=MouseDeviceDestroy(Mouse);
 if Result <> ERROR_SUCCESS then
  begin
   if MOUSE_LOG_ENABLED then MouseLogError(nil,'PL050: Failed to destroy Mouse device: ' + ErrorToString(Result));
   Exit;
  end; 
 
 {Return Result}
 Result:=ERROR_SUCCESS;
end;
 
{==============================================================================}
{==============================================================================}
{PL050 Keyboard Functions}
function PL050KeyboardControl(Keyboard:PKeyboardDevice;Request:Integer;Argument1:LongWord;var Argument2:LongWord):LongWord;
{Implementation of KeyboardDeviceControl API for PL050 Keyboard}
{Note: Not intended to be called directly by applications, use KeyboardDeviceControl instead}
var
 Status:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Keyboard}
 if Keyboard = nil then Exit;
 if Keyboard.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Keyboard Attached}
 if Keyboard.KeyboardState <> KEYBOARD_STATE_ATTACHED then Exit;
 
 {Acquire the Lock}
 if MutexLock(Keyboard.Lock) = ERROR_SUCCESS then
  begin
   try
    case Request of
     KEYBOARD_CONTROL_GET_FLAG:begin
       {Get Flag}
       LongBool(Argument2):=False;
       if (Keyboard.Device.DeviceFlags and Argument1) <> 0 then
        begin
         LongBool(Argument2):=True;
         
         {Return Result}
         Result:=ERROR_SUCCESS;
        end;
      end;
     KEYBOARD_CONTROL_SET_FLAG:begin 
       {Set Flag}
       if (Argument1 and not(KEYBOARD_FLAG_MASK)) = 0 then
        begin
         Keyboard.Device.DeviceFlags:=(Keyboard.Device.DeviceFlags or Argument1);
       
         {Return Result}
         Result:=ERROR_SUCCESS;
        end; 
      end;
     KEYBOARD_CONTROL_CLEAR_FLAG:begin 
       {Clear Flag}
       if (Argument1 and not(KEYBOARD_FLAG_MASK)) = 0 then
        begin
         Keyboard.Device.DeviceFlags:=(Keyboard.Device.DeviceFlags and not(Argument1));
       
         {Return Result}
         Result:=ERROR_SUCCESS;
        end; 
      end;
     KEYBOARD_CONTROL_FLUSH_BUFFER:begin
       {Flush Buffer}
       while Keyboard.Buffer.Count > 0 do 
        begin
         {Wait for Data (Should not Block)}
         if SemaphoreWait(Keyboard.Buffer.Wait) = ERROR_SUCCESS then
          begin
           {Update Start}
           Keyboard.Buffer.Start:=(Keyboard.Buffer.Start + 1) mod KEYBOARD_BUFFER_SIZE;
           
           {Update Count}
           Dec(Keyboard.Buffer.Count);
          end
         else
          begin
           Result:=ERROR_CAN_NOT_COMPLETE;
           Exit;
          end;
        end;
        
       {Return Result} 
       Result:=ERROR_SUCCESS;
      end;
     KEYBOARD_CONTROL_GET_LED:begin
       {Get LED}
       LongBool(Argument2):=False;
       if (Keyboard.KeyboardLEDs and Argument1) <> 0 then
        begin
         LongBool(Argument2):=True;
         
         {Return Result}
         Result:=ERROR_SUCCESS;
        end;
      end;
     KEYBOARD_CONTROL_SET_LED:begin
       {Set LED}
       if (Argument1 and not(KEYBOARD_LED_MASK)) = 0 then
        begin
         Keyboard.KeyboardLEDs:=(Keyboard.KeyboardLEDs or Argument1);
       
         {Set LEDs}
         Status:=PL050KMIKeyboardSetLEDs(PPL050Keyboard(Keyboard),Keyboard.KeyboardLEDs);
         if Status <> ERROR_SUCCESS then
          begin
           Result:=ERROR_OPERATION_FAILED;
           Exit;
          end;
         
         {Return Result}
         Result:=ERROR_SUCCESS;
        end; 
      end;
     KEYBOARD_CONTROL_CLEAR_LED:begin
       {Clear LED}
       if (Argument1 and not(KEYBOARD_LED_MASK)) = 0 then
        begin
         Keyboard.KeyboardLEDs:=(Keyboard.KeyboardLEDs and not(Argument1));
       
         {Set LEDs}
         Status:=PL050KMIKeyboardSetLEDs(PPL050Keyboard(Keyboard),Keyboard.KeyboardLEDs);
         if Status <> ERROR_SUCCESS then
          begin
           Result:=ERROR_OPERATION_FAILED;
           Exit;
          end;
         
         {Return Result}
         Result:=ERROR_SUCCESS;
        end; 
      end;
     KEYBOARD_CONTROL_GET_REPEAT_RATE:begin
       {Get Repeat Rate}
       Argument2:=Keyboard.KeyboardRate;
       
       {Return Result}
       Result:=ERROR_SUCCESS;
      end;
     KEYBOARD_CONTROL_SET_REPEAT_RATE:begin
       {Set Repeat Rate}
       Keyboard.KeyboardRate:=Argument1;
       
       {Set Typematic}
       Status:=PL050KMIKeyboardSetTypematic(PPL050Keyboard(Keyboard),Keyboard.KeyboardRate,Keyboard.KeyboardDelay);
       if Status <> ERROR_SUCCESS then
        begin
         Result:=ERROR_OPERATION_FAILED;
         Exit;
        end;
       
       {Return Result}
       Result:=ERROR_SUCCESS;
      end;
     KEYBOARD_CONTROL_GET_REPEAT_DELAY:begin
       {Get Repeat Delay}
       Argument2:=Keyboard.KeyboardDelay;
       
       {Return Result}
       Result:=ERROR_SUCCESS;
      end;
     KEYBOARD_CONTROL_SET_REPEAT_DELAY:begin
       {Set Repeat Delay}
       Keyboard.KeyboardDelay:=Argument1;
       
       {Set Typematic}
       Status:=PL050KMIKeyboardSetTypematic(PPL050Keyboard(Keyboard),Keyboard.KeyboardRate,Keyboard.KeyboardDelay);
       if Status <> ERROR_SUCCESS then
        begin
         Result:=ERROR_OPERATION_FAILED;
         Exit;
        end;
       
       {Return Result}
       Result:=ERROR_SUCCESS;
      end;
    end;
   finally
    {Release the Lock}
    MutexUnlock(Keyboard.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
   Exit;
  end;
end;

{==============================================================================}

procedure PL050KeyboardInterruptHandler(Keyboard:PKeyboardDevice);
{Interrupt handler for the PL050 keyboard device}
{Note: Not intended to be called directly by applications}
var
 Value:Byte;
 Status:LongWord;
 ResultCode:LongWord;
 Scancode:PPL050KeyboardScancode;
begin
 {}
 {Check Keyboard}
 if Keyboard = nil then Exit;
 
 {Update Statistics}
 Inc(PPL050Keyboard(Keyboard).InterruptCount);

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Get Interrupt Status}
 Status:=PPL050Keyboard(Keyboard).Registers.IIR;
 while (Status and PL050_IIR_RXINTR) <> 0 do
  begin
   {Read Data Value}
   Value:=PPL050Keyboard(Keyboard).Registers.DATA;
   
   {Check Scancode Count (Must allow one spare)}
   if PPL050Keyboard(Keyboard).ScancodeCount < (PL050_KEYBOARD_SCANCODE_COUNT - 1) then
    begin
     {Get Scancode}
     Scancode:=@PPL050Keyboard(Keyboard).ScancodeData[PPL050Keyboard(Keyboard).ScancodeStart];

     {Check Count}
     if Scancode.Count = 0 then
      begin
       {Update Scancode}
       Scancode.Index:=-1;
       FillChar(Scancode.Scancode,SizeOf(TPS2KeyboardScancode),0);
       Scancode.Scancode.ScancodeSet:=PPL050Keyboard(Keyboard).ScancodeSet;
      end;
    
     {Add Data Value}    
     Scancode.Scancode.Scancode[Scancode.Count]:=Value;
     Inc(Scancode.Count);
     
     {Check Scancode}
     ResultCode:=PS2KeyboardScancodeMatch(@Scancode.Scancode,Scancode.Index);
     if ResultCode = ERROR_SUCCESS then
      begin
       {Update Start}
       PPL050Keyboard(Keyboard).ScancodeStart:=(PPL050Keyboard(Keyboard).ScancodeStart + 1) mod PL050_KEYBOARD_SCANCODE_COUNT;
       
       {Increment Count}
       InterlockedIncrement(LongInt(PPL050Keyboard(Keyboard).ScancodeCount));
       
       {Send Scancode}
       Scancode.Keyboard:=PPL050Keyboard(Keyboard);
       if WorkerScheduleIRQ(CPU_AFFINITY_NONE,TWorkerTask(PL050KeyboardWorker),Scancode,nil) <> ERROR_SUCCESS then
        begin
         {Decrement Count}
         InterlockedDecrement(LongInt(PPL050Keyboard(Keyboard).ScancodeCount));
        end;
       
       {Get Scancode}
       Scancode:=@PPL050Keyboard(Keyboard).ScancodeData[PPL050Keyboard(Keyboard).ScancodeStart];
       
       {Update Scancode}
       Scancode.Count:=0;
      end
     else if (ResultCode <> ERROR_NOT_FOUND) or (Scancode.Count > 7) then
      begin
       {Update Statistics}
       Inc(PPL050Keyboard(Keyboard).DiscardCount,Scancode.Count);
        
       {Update Scancode}
       Scancode.Count:=0;
      end;
    end
   else
    begin
     {Update Statistics}
     Inc(PPL050Keyboard(Keyboard).DiscardCount);
    end;    
   
   {Get Interrupt Status}
   Status:=PPL050Keyboard(Keyboard).Registers.IIR;
  end;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
end;

{==============================================================================}
 
procedure PL050KeyboardWorker(Scancode:PPL050KeyboardScancode);
{Worker function for the PL050 keyboard device, called on a worker thread when
 a recognized scancode is received by the interrupt handler}
{Note: Not intended to be called directly by applications}
var
 Index:Byte;
 LEDs:LongWord;
 KeyCode:Word;
 ShiftState:LongWord;
 Status:LongWord;
 Data:TKeyboardData;
 Keymap:TKeymapHandle;
 Keyboard:PPL050Keyboard;
begin
 {}
 {Check Scancode}
 if Scancode = nil then Exit;
 if Scancode.Keyboard = nil then Exit;
 
 {$IF DEFINED(PL050_DEBUG) or DEFINED(KEYBOARD_DEBUG)}
 if KEYBOARD_LOG_ENABLED then KeyboardLogDebug(@Scancode.Keyboard.Keyboard,'PL050: Keyboard Worker');
 {$ENDIF}
 
 {Get Keyboard}
 Keyboard:=Scancode.Keyboard;
 if Keyboard <> nil then
  begin
   {Acquire the Lock}
   if MutexLock(Keyboard.Keyboard.Lock) = ERROR_SUCCESS then
    begin
     try
      {Update Statistics}
      Inc(Keyboard.Keyboard.ReceiveCount); 
   
      {Check Scancode Count}
      if Scancode.Count >= 1 then
       begin
        {Get Keymap}
        Keymap:=KeymapGetDefault;
        
        {Get LEDs}
        LEDs:=Keyboard.Keyboard.KeyboardLEDs;
        
        {Get ScanCode}
        if PS2KeyboardScancodeToScanCode(@Scancode.Scancode,Scancode.Index,Data.ScanCode) = ERROR_SUCCESS then
         begin
          {Get Modifiers}
          if PS2KeyboardScancodeToModifiers(@Scancode.Scancode,Scancode.Index,Data.Modifiers) = ERROR_SUCCESS then
           begin
            {Save Shift State}
            ShiftState:=Data.Modifiers;
            
            {Shift Modifiers}
            Data.Modifiers:=Data.Modifiers or Keyboard.ShiftState;
            
            {LED Modifiers}
            if Keyboard.Keyboard.KeyboardLEDs <> KEYBOARD_LED_NONE then
             begin
              if (Keyboard.Keyboard.KeyboardLEDs and KEYBOARD_LED_NUMLOCK) <> 0 then Data.Modifiers:=Data.Modifiers or KEYBOARD_NUM_LOCK;
              if (Keyboard.Keyboard.KeyboardLEDs and KEYBOARD_LED_CAPSLOCK) <> 0 then Data.Modifiers:=Data.Modifiers or KEYBOARD_CAPS_LOCK;
              if (Keyboard.Keyboard.KeyboardLEDs and KEYBOARD_LED_SCROLLLOCK) <> 0 then Data.Modifiers:=Data.Modifiers or KEYBOARD_SCROLL_LOCK;
              if (Keyboard.Keyboard.KeyboardLEDs and KEYBOARD_LED_COMPOSE) <> 0 then Data.Modifiers:=Data.Modifiers or KEYBOARD_COMPOSE;
              if (Keyboard.Keyboard.KeyboardLEDs and KEYBOARD_LED_KANA) <> 0 then Data.Modifiers:=Data.Modifiers or KEYBOARD_KANA;
             end;
            
            {Get Keymap Index}
            Index:=KEYMAP_INDEX_NORMAL;
          
            {Check for Shift}
            if (Data.Modifiers and (KEYBOARD_LEFT_SHIFT or KEYBOARD_RIGHT_SHIFT)) <> 0 then
             begin
              Index:=KEYMAP_INDEX_SHIFT;
              
              {Check Shift behavior}
              if KEYBOARD_SHIFT_IS_CAPS_LOCK_OFF then
               begin
                {Check for Caps Lock}
                if (Data.Modifiers and (KEYBOARD_CAPS_LOCK)) <> 0 then
                 begin
                  {Update LEDs}
                  Keyboard.Keyboard.KeyboardLEDs:=Keyboard.Keyboard.KeyboardLEDs and not(KEYBOARD_LED_CAPSLOCK);
                 end;
               end;
             end;
          
            {Check AltGr behavior}
            if KeymapCheckFlag(Keymap,KEYMAP_FLAG_ALTGR) then
             begin
              if not(KEYBOARD_CTRL_ALT_IS_ALTGR) then
               begin
                {Check for Right Alt}
                if (Data.Modifiers and (KEYBOARD_RIGHT_ALT)) <> 0 then
                 begin
                  if Index <> KEYMAP_INDEX_SHIFT then Index:=KEYMAP_INDEX_ALTGR else Index:=KEYMAP_INDEX_SHIFT_ALTGR;
                 end;
               end
              else
               begin
                {Check for Ctrl and Alt}
                if ((Data.Modifiers and (KEYBOARD_LEFT_CTRL or KEYBOARD_RIGHT_CTRL)) <> 0) and ((Data.Modifiers and (KEYBOARD_LEFT_ALT or KEYBOARD_RIGHT_ALT)) <> 0) then
                 begin
                  if Index <> KEYMAP_INDEX_SHIFT then Index:=KEYMAP_INDEX_ALTGR else Index:=KEYMAP_INDEX_SHIFT_ALTGR;
                 end;
               end;
              
              {Check Keymap Index}
              if (Index = KEYMAP_INDEX_ALTGR) or (Index = KEYMAP_INDEX_SHIFT_ALTGR) then
               begin
                Data.Modifiers:=Data.Modifiers or KEYBOARD_ALTGR;
               end;
             end;
          
            {Check for Key Pressed} 
            if (Data.Modifiers and KEYBOARD_KEYDOWN) <> 0 then
             begin
              {Update Shift State}
              Keyboard.ShiftState:=Keyboard.ShiftState or (ShiftState and PL050_KEYBOARD_SHIFTSTATE_MASK);
              
              {Ignore SCAN_CODE_NONE to SCAN_CODE_ERROR}
              if Data.ScanCode > SCAN_CODE_ERROR then 
               begin
                {Check for Caps Lock Shifted Key}
                if KeymapCheckCapskey(Keymap,Data.ScanCode) then
                 begin
                  {Check for Caps Lock}
                  if (Data.Modifiers and (KEYBOARD_CAPS_LOCK)) <> 0 then
                   begin
                    {Modify Normal and Shift}
                    if Index = KEYMAP_INDEX_NORMAL then
                     begin
                      Index:=KEYMAP_INDEX_SHIFT; 
                     end
                    else if Index = KEYMAP_INDEX_SHIFT then
                     begin
                      Index:=KEYMAP_INDEX_NORMAL;
                     end
                    {Modify AltGr and Shift}
                    else if Index = KEYMAP_INDEX_ALTGR then
                     begin
                      Index:=KEYMAP_INDEX_SHIFT_ALTGR; 
                     end
                    else if Index = KEYMAP_INDEX_SHIFT_ALTGR then
                     begin
                      Index:=KEYMAP_INDEX_ALTGR; 
                     end;
                   end;
                 end; 
             
                {Check for Numeric Keypad Key}
                if (Data.ScanCode >= SCAN_CODE_KEYPAD_FIRST) and (Data.ScanCode <= SCAN_CODE_KEYPAD_LAST) then
                 begin
                  {Check for Num Lock}
                  if (Data.Modifiers and (KEYBOARD_NUM_LOCK)) <> 0 then
                   begin
                    {Check for Shift}
                    if (Data.Modifiers and (KEYBOARD_LEFT_SHIFT or KEYBOARD_RIGHT_SHIFT)) <> 0 then
                     begin
                      Index:=KEYMAP_INDEX_NORMAL;
                     end
                    else
                     begin
                      Index:=KEYMAP_INDEX_SHIFT;
                     end; 
                   end
                  else
                   begin
                    Index:=KEYMAP_INDEX_NORMAL;
                   end;                 
                 end;
             
                {Check Pressed}
                if PL050KMIKeyboardCheckPressed(Keyboard,Data.ScanCode) then
                 begin
                  {$IF DEFINED(PL050_DEBUG) or DEFINED(KEYBOARD_DEBUG)}
                  if KEYBOARD_LOG_ENABLED then KeyboardLogDebug(@Scancode.Keyboard.Keyboard,'PL050: Key Pressed (ScanCode=' + IntToStr(Data.ScanCode) + ' Modifiers=' + IntToHex(Data.Modifiers,8) + ' Index=' + IntToStr(Index) + ')');
                  {$ENDIF}
             
                  {Check for NumLock}
                  if Data.ScanCode = SCAN_CODE_NUMLOCK then
                   begin
                    {Update LEDs}
                    Keyboard.Keyboard.KeyboardLEDs:=Keyboard.Keyboard.KeyboardLEDs xor KEYBOARD_LED_NUMLOCK;
                   end
                  else if Data.ScanCode = SCAN_CODE_CAPSLOCK then
                   begin              
                    {Update LEDs}
                    Keyboard.Keyboard.KeyboardLEDs:=Keyboard.Keyboard.KeyboardLEDs xor KEYBOARD_LED_CAPSLOCK;
                   end
                  else if Data.ScanCode = SCAN_CODE_SCROLLLOCK then
                   begin
                    {Update LEDs}
                    Keyboard.Keyboard.KeyboardLEDs:=Keyboard.Keyboard.KeyboardLEDs xor KEYBOARD_LED_SCROLLLOCK;
                   end;
                   
                  {Update Last Code}
                  Keyboard.LastCode:=Data.ScanCode;
             
                  {Check for Deadkey}
                  if (Keyboard.Keyboard.Code = SCAN_CODE_NONE) and KeymapCheckDeadkey(Keymap,Data.ScanCode,Index) then
                   begin
                    {$IF DEFINED(PL050_DEBUG) or DEFINED(KEYBOARD_DEBUG)}
                    if KEYBOARD_LOG_ENABLED then KeyboardLogDebug(@Scancode.Keyboard.Keyboard,'PL050: Deadkey Pressed (ScanCode=' + IntToStr(Data.ScanCode) + ' Modifiers=' + IntToHex(Data.Modifiers,8) + ' Index=' + IntToStr(Index) + ')');
                    {$ENDIF}
                    
                    {Update Deadkey}
                    Keyboard.Keyboard.Code:=Data.ScanCode;
                    Keyboard.Keyboard.Index:=Index;
                    Keyboard.Keyboard.Modifiers:=Data.Modifiers;
                   
                    {Get Data}
                    Data.Modifiers:=Data.Modifiers or KEYBOARD_DEADKEY;
                    Data.KeyCode:=KeymapGetKeyCode(Keymap,Data.ScanCode,Index);
                    Data.CharCode:=KeymapGetCharCode(Keymap,Data.KeyCode);
                    Data.CharUnicode:=KeymapGetCharUnicode(Keymap,Data.KeyCode);
    
                    {$IF DEFINED(PL050_DEBUG) or DEFINED(KEYBOARD_DEBUG)}
                    if KEYBOARD_LOG_ENABLED then KeyboardLogDebug(@Scancode.Keyboard.Keyboard,'PL050: Deadkey Pressed (KeyCode=' + IntToHex(Data.KeyCode,4) + ' CharCode=' + IntToHex(Byte(Data.CharCode),2) + ' CharUnicode=' + IntToHex(Word(Data.CharUnicode),4) + ')');
                    {$ENDIF}
    
                    {Insert Data}
                    KeyboardInsertData(@Keyboard.Keyboard,@Data,True);
                   end 
                  else
                   begin
                    {Check Deadkey}
                    KeyCode:=KEY_CODE_NONE;
                    if Keyboard.Keyboard.Code <> SCAN_CODE_NONE then
                     begin
                      {Resolve Deadkey}
                      if not KeymapResolveDeadkey(Keymap,Keyboard.Keyboard.Code,Data.ScanCode,Keyboard.Keyboard.Index,Index,KeyCode) then
                       begin
                        {Get Data}
                        Data.Modifiers:=Keyboard.Keyboard.Modifiers;
                        Data.ScanCode:=Keyboard.Keyboard.Code;
                        Data.KeyCode:=KeymapGetKeyCode(Keymap,Keyboard.Keyboard.Code,Keyboard.Keyboard.Index);
                        Data.CharCode:=KeymapGetCharCode(Keymap,Data.KeyCode);
                        Data.CharUnicode:=KeymapGetCharUnicode(Keymap,Data.KeyCode);
                        
                        {Insert Data}
                        KeyboardInsertData(@Keyboard.Keyboard,@Data,True);
                       end;
                     end;
                    
                    {Reset Deadkey}
                    Keyboard.Keyboard.Code:=SCAN_CODE_NONE;
                  
                    {Get Data}
                    Data.KeyCode:=KeymapGetKeyCode(Keymap,Data.ScanCode,Index);
                    if KeyCode <> KEY_CODE_NONE then Data.KeyCode:=KeyCode;
                    Data.CharCode:=KeymapGetCharCode(Keymap,Data.KeyCode);
                    Data.CharUnicode:=KeymapGetCharUnicode(Keymap,Data.KeyCode);
                  
                    {$IF DEFINED(PL050_DEBUG) or DEFINED(KEYBOARD_DEBUG)}
                    if KEYBOARD_LOG_ENABLED then KeyboardLogDebug(@Scancode.Keyboard.Keyboard,'PL050: Key Pressed (KeyCode=' + IntToHex(Data.KeyCode,4) + ' CharCode=' + IntToHex(Byte(Data.CharCode),2) + ' CharUnicode=' + IntToHex(Word(Data.CharUnicode),4) + ')');
                    {$ENDIF}
                    
                    {Insert Data}
                    KeyboardInsertData(@Keyboard.Keyboard,@Data,True);
                   end;
                 end
                else
                 begin
                  {Check Repeated}
                  if PL050KMIKeyboardCheckRepeated(Keyboard,Data.ScanCode) then
                   begin
                    {$IF DEFINED(PL050_DEBUG) or DEFINED(KEYBOARD_DEBUG)}
                    if KEYBOARD_LOG_ENABLED then KeyboardLogDebug(@Scancode.Keyboard.Keyboard,'PL050: Key Repeated (ScanCode=' + IntToStr(Data.ScanCode) + ' Modifiers=' + IntToHex(Data.Modifiers,8) + ' Index=' + IntToStr(Index) + ')');
                    {$ENDIF}
                    
                    {Get Data}
                    Data.Modifiers:=Data.Modifiers or KEYBOARD_KEYREPEAT;
                    Data.KeyCode:=KeymapGetKeyCode(Keymap,Data.ScanCode,Index);
                    Data.CharCode:=KeymapGetCharCode(Keymap,Data.KeyCode);
                    Data.CharUnicode:=KeymapGetCharUnicode(Keymap,Data.KeyCode);

                    {$IF DEFINED(PL050_DEBUG) or DEFINED(KEYBOARD_DEBUG)}
                    if KEYBOARD_LOG_ENABLED then KeyboardLogDebug(@Scancode.Keyboard.Keyboard,'PL050: Key Repeated (KeyCode=' + IntToHex(Data.KeyCode,4) + ' CharCode=' + IntToHex(Byte(Data.CharCode),2) + ' CharUnicode=' + IntToHex(Word(Data.CharUnicode),4) + ')');
                    {$ENDIF}
                    
                    {Insert Data}
                    KeyboardInsertData(@Keyboard.Keyboard,@Data,True);
                   end;
                 end;             
               end;
             end;
             
            {Check for Key Released}
            if (Data.Modifiers and KEYBOARD_KEYUP) <> 0 then
             begin
              {Update Shift State}
              Keyboard.ShiftState:=Keyboard.ShiftState and not(ShiftState and PL050_KEYBOARD_SHIFTSTATE_MASK);
             
              {Ignore SCAN_CODE_NONE to SCAN_CODE_ERROR}
              if Data.ScanCode > SCAN_CODE_ERROR then 
               begin
                {Check for Caps Lock Shifted Key}
                if KeymapCheckCapskey(Keymap,Data.ScanCode) then
                 begin
                  {Check for Caps Lock}
                  if (Data.Modifiers and (KEYBOARD_CAPS_LOCK)) <> 0 then
                   begin
                    {Modify Normal and Shift}
                    if Index = KEYMAP_INDEX_NORMAL then
                     begin
                      Index:=KEYMAP_INDEX_SHIFT; 
                     end
                    else if Index = KEYMAP_INDEX_SHIFT then
                     begin
                      Index:=KEYMAP_INDEX_NORMAL;
                     end
                    {Modify AltGr and Shift}
                    else if Index = KEYMAP_INDEX_ALTGR then
                     begin
                      Index:=KEYMAP_INDEX_SHIFT_ALTGR; 
                     end
                    else if Index = KEYMAP_INDEX_SHIFT_ALTGR then
                     begin
                      Index:=KEYMAP_INDEX_ALTGR; 
                     end;
                   end;
                 end; 
             
                {Check for Numeric Keypad Key}
                if (Data.ScanCode >= SCAN_CODE_KEYPAD_FIRST) and (Data.ScanCode <= SCAN_CODE_KEYPAD_LAST) then
                 begin
                  {Check for Num Lock}
                  if (Data.Modifiers and (KEYBOARD_NUM_LOCK)) <> 0 then
                   begin
                    {Check for Shift}
                    if (Data.Modifiers and (KEYBOARD_LEFT_SHIFT or KEYBOARD_RIGHT_SHIFT)) <> 0 then
                     begin
                      Index:=KEYMAP_INDEX_NORMAL;
                     end
                    else
                     begin
                      Index:=KEYMAP_INDEX_SHIFT;
                     end; 
                   end
                  else
                   begin
                    Index:=KEYMAP_INDEX_NORMAL;
                   end;                 
                 end;
             
                {$IF DEFINED(PL050_DEBUG) or DEFINED(KEYBOARD_DEBUG)}
                if KEYBOARD_LOG_ENABLED then KeyboardLogDebug(@Scancode.Keyboard.Keyboard,'PL050: Key Released (ScanCode=' + IntToStr(Data.ScanCode) + ' Modifiers=' + IntToHex(Data.Modifiers,8) + ' Index=' + IntToStr(Index)+ ')');
                {$ENDIF}
                
                {Reset Last Code}
                Keyboard.LastCode:=SCAN_CODE_NONE;
                
                {Get Data}
                Data.KeyCode:=KeymapGetKeyCode(Keymap,Data.ScanCode,Index);
                Data.CharCode:=KeymapGetCharCode(Keymap,Data.KeyCode);
                Data.CharUnicode:=KeymapGetCharUnicode(Keymap,Data.KeyCode);
               
                {$IF DEFINED(PL050_DEBUG) or DEFINED(KEYBOARD_DEBUG)}
                if KEYBOARD_LOG_ENABLED then KeyboardLogDebug(@Scancode.Keyboard.Keyboard,'PL050: Key Released (KeyCode=' + IntToHex(Data.KeyCode,4) + ' CharCode=' + IntToHex(Byte(Data.CharCode),2) + ' CharUnicode=' + IntToHex(Word(Data.CharUnicode),4) + ')');
                {$ENDIF}
                
                {Insert Data}
                KeyboardInsertData(@Keyboard.Keyboard,@Data,True);
               end;
             end;
             
            {Check LEDs}
            if LEDs <> Keyboard.Keyboard.KeyboardLEDs then
             begin
              {Update LEDs}
              Status:=PL050KMIKeyboardSetLEDs(Keyboard,Keyboard.Keyboard.KeyboardLEDs);
              if Status <> ERROR_SUCCESS then
               begin
                if KEYBOARD_LOG_ENABLED then KeyboardLogError(@Keyboard.Keyboard,'PL050: Failed to set LEDs: ' + ErrorToString(Status));
               end;
             end;
           end
          else
           begin
            if KEYBOARD_LOG_ENABLED then KeyboardLogError(@Keyboard.Keyboard,'PL050: Scancode modifiers invalid');
            
            {Update Statistics}
            Inc(Keyboard.Keyboard.ReceiveErrors); 
           end;         
         end
        else
         begin
          if KEYBOARD_LOG_ENABLED then KeyboardLogError(@Keyboard.Keyboard,'PL050: Scancode code invalid');
          
          {Update Statistics}
          Inc(Keyboard.Keyboard.ReceiveErrors); 
         end;         
       end
      else
       begin
        if KEYBOARD_LOG_ENABLED then KeyboardLogError(@Keyboard.Keyboard,'PL050: Scancode count invalid');
        
        {Update Statistics}
        Inc(Keyboard.Keyboard.ReceiveErrors); 
       end;    
     finally
      {Release the Lock}
      MutexUnlock(Keyboard.Keyboard.Lock);
     end;
    end
   else
    begin
     if KEYBOARD_LOG_ENABLED then KeyboardLogError(@Keyboard.Keyboard,'PL050: Failed to acquire lock');
    end;
    
   {Decrement Count}
   InterlockedDecrement(LongInt(Keyboard.ScancodeCount));
  end
 else
  begin
   if KEYBOARD_LOG_ENABLED then KeyboardLogError(nil,'PL050: Keyboard scancode invalid');
  end;    
end;
 
{==============================================================================}
{==============================================================================}
{PL050 Mouse Functions}
function PL050MouseControl(Mouse:PMouseDevice;Request:Integer;Argument1:LongWord;var Argument2:LongWord):LongWord;
{Implementation of MouseDeviceControl API for PL050 Mouse}
{Note: Not intended to be called directly by applications, use MouseDeviceControl instead}
var
 Status:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Mouse}
 if Mouse = nil then Exit;
 if Mouse.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {Check Mouse Attached}
 if Mouse.MouseState <> MOUSE_STATE_ATTACHED then Exit;
 
 {Acquire the Lock}
 if MutexLock(Mouse.Lock) = ERROR_SUCCESS then
  begin
   try
    case Request of
     MOUSE_CONTROL_GET_FLAG:begin
       {Get Flag}
       LongBool(Argument2):=False;
       if (Mouse.Device.DeviceFlags and Argument1) <> 0 then
        begin
         LongBool(Argument2):=True;
         
         {Return Result}
         Result:=ERROR_SUCCESS;
        end;
      end;
     MOUSE_CONTROL_SET_FLAG:begin 
       {Set Flag}
       if (Argument1 and not(MOUSE_FLAG_MASK)) = 0 then
        begin
         Mouse.Device.DeviceFlags:=(Mouse.Device.DeviceFlags or Argument1);
       
         {Return Result}
         Result:=ERROR_SUCCESS;
        end; 
      end;
     MOUSE_CONTROL_CLEAR_FLAG:begin 
       {Clear Flag}
       if (Argument1 and not(MOUSE_FLAG_MASK)) = 0 then
        begin
         Mouse.Device.DeviceFlags:=(Mouse.Device.DeviceFlags and not(Argument1));
       
         {Return Result}
         Result:=ERROR_SUCCESS;
        end; 
      end;
     MOUSE_CONTROL_FLUSH_BUFFER:begin
       {Flush Buffer}
       while Mouse.Buffer.Count > 0 do 
        begin
         {Wait for Data (Should not Block)}
         if SemaphoreWait(Mouse.Buffer.Wait) = ERROR_SUCCESS then
          begin
           {Update Start}
           Mouse.Buffer.Start:=(Mouse.Buffer.Start + 1) mod MOUSE_BUFFER_SIZE;
           
           {Update Count}
           Dec(Mouse.Buffer.Count);
          end
         else
          begin
           Result:=ERROR_CAN_NOT_COMPLETE;
           Exit;
          end;
        end;
        
       {Return Result} 
       Result:=ERROR_SUCCESS;
      end;       
     MOUSE_CONTROL_GET_SAMPLE_RATE:begin
       {Get Sample Rate}
       Argument2:=Mouse.MouseRate;
       
       {Return Result}
       Result:=ERROR_SUCCESS;
      end;
     MOUSE_CONTROL_SET_SAMPLE_RATE:begin
       {Set Sample Rate}
       Mouse.MouseRate:=Argument1;
       
       {Set Rate}
       Status:=PL050KMIMouseSetSampleRate(PPL050Mouse(Mouse),Mouse.MouseRate);
       if Status <> ERROR_SUCCESS then
        begin
         Result:=ERROR_OPERATION_FAILED;
         Exit;
        end;
       
       {Return Result}
       Result:=ERROR_SUCCESS;
      end;
     MOUSE_CONTROL_GET_MAX_X:begin
       {Get Maximum X}
       Argument2:=0;
       
       {Return Result}
       Result:=ERROR_SUCCESS;
      end;       
     MOUSE_CONTROL_GET_MAX_Y:begin
       {Get Maximum Y}
       Argument2:=0;
       
       {Return Result}
       Result:=ERROR_SUCCESS;
      end;       
     MOUSE_CONTROL_GET_MAX_WHEEL:begin
       {Get Maximum Wheel}
       Argument2:=0;
       
       {Return Result}
       Result:=ERROR_SUCCESS;
      end;       
     MOUSE_CONTROL_GET_MAX_BUTTONS:begin
       {Get Maximum Buttons mask}
       Argument2:=MOUSE_LEFT_BUTTON or MOUSE_RIGHT_BUTTON or MOUSE_MIDDLE_BUTTON;
       
       {Return Result}
       Result:=ERROR_SUCCESS;
      end;   
     MOUSE_CONTROL_GET_ROTATION:begin
       {Get Rotation}
       Argument2:=MOUSE_ROTATION_0;
       
       {Return Result}
       Result:=ERROR_SUCCESS;
      end;
     MOUSE_CONTROL_SET_ROTATION:begin
       {Set Rotation}
       {Not Supported}
       
       {Return Result}
       Result:=ERROR_NOT_SUPPORTED;
      end;
    end;
   finally
    {Release the Lock}
    MutexUnlock(Mouse.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
   Exit;
  end;
end;
 
{==============================================================================}
 
procedure PL050MouseInterruptHandler(Mouse:PMouseDevice);
{Interrupt handler for the PL050 mouse device}
{Note: Not intended to be called directly by applications}
var
 Value:Byte;
 Status:LongWord;
 Packet:PPL050MousePacket;
begin
 {}
 {Check Mouse}
 if Mouse = nil then Exit;
 
 {Update Statistics}
 Inc(PPL050Mouse(Mouse).InterruptCount);

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Get Interrupt Status}
 Status:=PPL050Mouse(Mouse).Registers.IIR;
 while (Status and PL050_IIR_RXINTR) <> 0 do
  begin
   {Read Data Value}
   Value:=PPL050Mouse(Mouse).Registers.DATA;
   
   {Check Packet Count (Must allow one spare)}
   if PPL050Mouse(Mouse).PacketCount < (PL050_MOUSE_PACKET_COUNT - 1) then
    begin
     {Get Packet}
     Packet:=@PPL050Mouse(Mouse).PacketData[PPL050Mouse(Mouse).PacketStart];
     
     {Check Count}
     case Packet.Count of
      0:begin
        {First Byte}
        Packet.Packet.MouseBits:=Value;
        Inc(Packet.Count);
       end;
      1:begin
        {Second Byte}
        Packet.Packet.MouseX:=Value;
        Inc(Packet.Count);
       end;
      2:begin
        {Third Byte}
        Packet.Packet.MouseY:=Value;
        Inc(Packet.Count);
        
        {Update Start}
        PPL050Mouse(Mouse).PacketStart:=(PPL050Mouse(Mouse).PacketStart + 1) mod PL050_MOUSE_PACKET_COUNT;
        
        {Increment Count}
        InterlockedIncrement(LongInt(PPL050Mouse(Mouse).PacketCount));
        
        {Send Packet}
        Packet.Mouse:=PPL050Mouse(Mouse);
        if WorkerScheduleIRQ(CPU_AFFINITY_NONE,TWorkerTask(PL050MouseWorker),Packet,nil) <> ERROR_SUCCESS then
         begin
          {Decrement Count}
          InterlockedDecrement(LongInt(PPL050Mouse(Mouse).PacketCount));
         end;
        
        {Get Packet}
        Packet:=@PPL050Mouse(Mouse).PacketData[PPL050Mouse(Mouse).PacketStart];
        
        {Update Packet}
        Packet.Count:=0;
       end;
     end; 
    end
   else
    begin
     {Update Statistics}
     Inc(PPL050Mouse(Mouse).DiscardCount);
    end;    
   
   {Get Interrupt Status}
   Status:=PPL050Mouse(Mouse).Registers.IIR;
  end;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
end;
 
{==============================================================================}
 
procedure PL050MouseWorker(Packet:PPL050MousePacket);
{Worker function for the PL050 mouse device, called on a worker thread when a
 new mouse packet is received by the interrupt handler}
{Note: Not intended to be called directly by applications}
var
 Status:LongWord;
 Mouse:PPL050Mouse;
 LocalData:PMouseData;
 GlobalData:TMouseData;
begin
 {}
 {Check Packet}
 if Packet = nil then Exit;
 if Packet.Mouse = nil then Exit;
 
 {$IF DEFINED(PL050_DEBUG) or DEFINED(MOUSE_DEBUG)}
 if MOUSE_LOG_ENABLED then MouseLogDebug(@Packet.Mouse.Mouse,'PL050: Mouse Worker');
 {$ENDIF}
 
 {Get Mouse}
 Mouse:=Packet.Mouse;
 if Mouse <> nil then
  begin
   {Acquire the Lock}
   if MutexLock(Mouse.Mouse.Lock) = ERROR_SUCCESS then
    begin
     try
      {Update Statistics}
      Inc(Mouse.Mouse.ReceiveCount); 
  
      {Check Packet}
      if Packet.Count >= 3 then
       begin
        {Check Flags}
        if (Mouse.Mouse.Device.DeviceFlags and MOUSE_FLAG_DIRECT_READ) = 0 then
         begin
          {Global Buffer}
          {Convert Packet}
          if PS2MousePacketToMouseData(@Packet.Packet,@GlobalData,Mouse.Mouse.Device.DeviceFlags) = ERROR_SUCCESS then
           begin
            {$IF DEFINED(PL050_DEBUG) or DEFINED(MOUSE_DEBUG)}
            if MOUSE_LOG_ENABLED then MouseLogDebug(@Packet.Mouse.Mouse,'PL050: Mouse Data (Buttons=' + IntToHex(GlobalData.Buttons,8) + ' OffsetX=' + IntToStr(GlobalData.OffsetX) + ' OffsetY=' + IntToStr(GlobalData.OffsetY) + ')');
            {$ENDIF}
            
            Status:=MouseWrite(@GlobalData,SizeOf(TMouseData),1);
            if Status <> ERROR_SUCCESS then
             begin
              if Status = ERROR_INSUFFICIENT_BUFFER then
               begin
                if MOUSE_LOG_ENABLED then MouseLogError(@Mouse.Mouse,'Mouse: Buffer overflow, packet discarded'); 
                
                {Update Statistics}
                Inc(Mouse.Mouse.BufferOverruns); 
               end
              else
               begin
                if MOUSE_LOG_ENABLED then MouseLogError(@Mouse.Mouse,'Mouse: Buffer error, packet discarded'); 
                
                {Update Statistics}
                Inc(Mouse.Mouse.ReceiveErrors); 
               end;               
             end;
           end;
         end
        else
         begin              
          {Direct Buffer}
          {Check Buffer}
          if (Mouse.Mouse.Buffer.Count < MOUSE_BUFFER_SIZE) then
           begin
            LocalData:=@Mouse.Mouse.Buffer.Buffer[(Mouse.Mouse.Buffer.Start + Mouse.Mouse.Buffer.Count) mod MOUSE_BUFFER_SIZE];
            if LocalData <> nil then
             begin
              {Convert Packet}
              if PS2MousePacketToMouseData(@Packet.Packet,LocalData,Mouse.Mouse.Device.DeviceFlags) = ERROR_SUCCESS then
               begin
                {$IF DEFINED(PL050_DEBUG) or DEFINED(MOUSE_DEBUG)}
                if MOUSE_LOG_ENABLED then MouseLogDebug(@Packet.Mouse.Mouse,'PL050: Mouse Data (Buttons=' + IntToHex(LocalData.Buttons,8) + ' OffsetX=' + IntToStr(LocalData.OffsetX) + ' OffsetY=' + IntToStr(LocalData.OffsetY) + ')');
                {$ENDIF}
               
                {Update Count}
                Inc(Mouse.Mouse.Buffer.Count);
            
                {Signal Data Received}
                SemaphoreSignal(Mouse.Mouse.Buffer.Wait);
               end; 
             end; 
           end
          else
           begin
            if MOUSE_LOG_ENABLED then MouseLogError(@Mouse.Mouse,'Mouse: Buffer overflow, packet discarded'); 

            {Update Statistics}
            Inc(Mouse.Mouse.BufferOverruns); 
           end;                           
         end;
       end
      else
       begin
        if MOUSE_LOG_ENABLED then MouseLogError(@Mouse.Mouse,'PL050: Packet count invalid');
        
        {Update Statistics}
        Inc(Mouse.Mouse.ReceiveErrors); 
       end;    
     finally
      {Release the Lock}
      MutexUnlock(Mouse.Mouse.Lock);
     end;
    end
   else
    begin
     if MOUSE_LOG_ENABLED then MouseLogError(@Mouse.Mouse,'PL050: Failed to acquire lock');
    end;
    
   {Decrement Count}
   InterlockedDecrement(LongInt(Mouse.PacketCount));
  end
 else
  begin
   if MOUSE_LOG_ENABLED then MouseLogError(nil,'PL050: Mouse packet invalid');
  end;    
end;

{==============================================================================}
{==============================================================================}
{PL050 Helper Functions}
function PL050KMIClear(Registers:PPL050KMIRegisters):LongWord;
{Clear the read buffer on a PL050 KMI device}
{Registers: Pointer to the PL050 KMI registers for the device}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Value:LongWord;
 Status:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Registers}
 if Registers = nil then Exit;

 {$IF DEFINED(PL050_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'PL050: KMI Clear');
 {$ENDIF}
 
 {Get Status}
 Status:=Registers.STAT;
 while (Status and PL050_STAT_RXFULL) <> 0 do
  begin
   {Read Data}
   Value:=Registers.DATA;
 
   {Get Status}
   Status:=Registers.STAT;
  end;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 Result:=ERROR_SUCCESS;  
end;

{==============================================================================}

function PL050KMIRead(Registers:PPL050KMIRegisters;var Value:Byte):LongWord;
{Read one byte of data from a PL050 KMI device}
{Registers: Pointer to the PL050 KMI registers for the device}
{Value: The returned value read from the device}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Status:LongWord;
 Timeout:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Registers}
 if Registers = nil then Exit;

 {$IF DEFINED(PL050_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'PL050: KMI Read');
 {$ENDIF}
 
 {Set Timeout (100ms)}
 Timeout:=10000;
 
 {Get Status}
 Status:=Registers.STAT;
 while (Status and PL050_STAT_RXFULL) = 0 do
  begin
   MicrosecondDelay(10);
   
   Dec(Timeout);
   if Timeout = 0 then
    begin
     Result:=ERROR_TIMEOUT;
     Exit;
    end; 
    
   {Get Status}
   Status:=Registers.STAT;
  end;

 {Read Data}
 Value:=Registers.DATA;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {$IF DEFINED(PL050_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'PL050: KMI Read (Value=' + IntToHex(Value,2) + ')');
 {$ENDIF}
 
 Result:=ERROR_SUCCESS;  
end;
 
{==============================================================================}

function PL050KMIWrite(Registers:PPL050KMIRegisters;Value:Byte):LongWord;
{Write one byte of data to a PL050 KMI device}
{Registers: Pointer to the PL050 KMI registers for the device}
{Value: The data value to write to the device}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Status:LongWord;
 Timeout:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Registers}
 if Registers = nil then Exit;
 
 {$IF DEFINED(PL050_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'PL050: KMI Write (Value=' + IntToHex(Value,2) + ')');
 {$ENDIF}
 
 {Set Timeout (100ms)}
 Timeout:=10000;

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Get Status}
 Status:=Registers.STAT;
 while (Status and PL050_STAT_TXEMPTY) = 0 do
  begin
   MicrosecondDelay(10);
   
   Dec(Timeout);
   if Timeout = 0 then
    begin
     Result:=ERROR_TIMEOUT;
     Exit;
    end; 
    
   {Get Status}
   Status:=Registers.STAT;
  end;

 {Write Data}
 Registers.DATA:=Value;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 Result:=ERROR_SUCCESS; 
end;

{==============================================================================}

function PL050KMICommand(Registers:PPL050KMIRegisters;Command:Byte;Data:PByte;DataSize:LongWord;Response:PByte;ResponseSize:LongWord):LongWord;
{Send a PS/2 command and data to a PL050 KMI device and wait for the required response}
{Registers: Pointer to the PL050 KMI registers for the device}
{Command: The PS/2 command to send to the device}
{Data: Pointer to the data to be sent with the command (Optional)}
{DataSize: The size in bytes of the data to be sent}
{Response: Pointer to a buffer to store the response from the device (Optional)}
{ResponseSize: The size in bytes of the response to be received}

{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Value:Byte;
 Retry:LongWord;
 Count:LongWord;
 Status:LongWord;
 Control:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Registers}
 if Registers = nil then Exit;

 {$IF DEFINED(PL050_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'PL050: KMI Command (Command=' + IntToHex(Command,2) + ' DataSize=' + IntToStr(DataSize) + ' ResponseSize=' + IntToStr(ResponseSize) + ')');
 {$ENDIF}
 
 {Check Data}
 if (DataSize > 0) and (Data = nil) then Exit;
 
 {Check Response}
 if (ResponseSize > 0) and (Response = nil) then Exit;
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Check Enabled}
 Control:=Registers.CR;
 if (Control and PL050_CR_EN) = 0 then
  begin
   Result:=ERROR_NOT_READY;
   Exit;
  end; 
 
 {Disable Receive Interrupt}
 Registers.CR:=Control and not(PL050_CR_RXINTREN);
 try
  {Clear Data}
  Status:=PL050KMIClear(Registers);
  if Status <> ERROR_SUCCESS then Exit;
  
  {Setup Retry}
  Retry:=3;
  while Retry > 0 do
   begin
    {Send Command}
    Status:=PL050KMIWrite(Registers,Command);
    if Status <> ERROR_SUCCESS then Exit;
    
    {Read Reply}
    Status:=PL050KMIRead(Registers,Value);
    if Status <> ERROR_SUCCESS then Exit;
    if Value = PS2_RESPONSE_ACK then
     begin
      Break;
     end
    else if Value <> PS2_RESPONSE_RESEND then
     begin
      Status:=ERROR_OPERATION_FAILED;
      Exit;
     end;
    
    {Update Retry}
    Dec(Retry);
    if Retry = 0 then
     begin
      Status:=ERROR_OPERATION_FAILED;
      Exit;
     end;
   end;  
  
  {Send Data}
  if (Data <> nil) and (DataSize > 0) then
   begin
    for Count:=0 to DataSize - 1 do
     begin
      {Send Data}
      Status:=PL050KMIWrite(Registers,Data[Count]);
      if Status <> ERROR_SUCCESS then Exit;
     end;
   end; 
  
  {Receive Response}
  if (Response <> nil) and (ResponseSize > 0) then
   begin
    for Count:=0 to ResponseSize - 1 do
     begin
      {Read Response}
      Status:=PL050KMIRead(Registers,Response[Count]);
      if Status <> ERROR_SUCCESS then Exit;
     end; 
   end; 
 finally
  {Enable Receive Interrupt}
  Registers.CR:=Control;
 
  {Memory Barrier}
  DataMemoryBarrier; {After the Last Read} 

  {$IF DEFINED(PL050_DEBUG) or DEFINED(DEVICE_DEBUG)}
  if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'PL050: KMI Command (Result=' + ErrorToString(Status) + ')');
  {$ENDIF}
  
  Result:=Status;  
 end; 
end;

{==============================================================================}

function PL050KMIKeyboardReset(Keyboard:PPL050Keyboard):LongWord;
{Reset a PL050 keyboard device}
{Keyboard: The keyboard device to reset}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Value:Byte;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Keyboard}
 if Keyboard = nil then Exit;

 {$IF DEFINED(PL050_DEBUG) or DEFINED(KEYBOARD_DEBUG)}
 if KEYBOARD_LOG_ENABLED then KeyboardLogDebug(@Keyboard.Keyboard,'PL050: KMI Keyboard Reset');
 {$ENDIF}
 
 {Send Reset Command}
 Result:=PL050KMICommand(Keyboard.Registers,PS2_KEYBOARD_COMMAND_RESET,nil,0,@Value,SizeOf(Byte));
 if (Result = ERROR_SUCCESS) and (Value <> PS2_RESPONSE_SELF_TEST_PASS) then
  begin
   Result:=ERROR_OPERATION_FAILED;
  end;
end;
 
{==============================================================================}

function PL050KMIKeyboardEnable(Keyboard:PPL050Keyboard):LongWord;
{Enable a PL050 keyboard device}
{Keyboard: The keyboard device to enable}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Keyboard}
 if Keyboard = nil then Exit;

 {$IF DEFINED(PL050_DEBUG) or DEFINED(KEYBOARD_DEBUG)}
 if KEYBOARD_LOG_ENABLED then KeyboardLogDebug(@Keyboard.Keyboard,'PL050: KMI Keyboard Enable');
 {$ENDIF}
 
 {Send Enable Command}
 Result:=PL050KMICommand(Keyboard.Registers,PS2_KEYBOARD_COMMAND_ENABLE_SCAN,nil,0,nil,0);
end;

{==============================================================================}

function PL050KMIKeyboardDisable(Keyboard:PPL050Keyboard):LongWord;
{Disable a PL050 keyboard device}
{Keyboard: The keyboard device to disable}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Keyboard}
 if Keyboard = nil then Exit;

 {$IF DEFINED(PL050_DEBUG) or DEFINED(KEYBOARD_DEBUG)}
 if KEYBOARD_LOG_ENABLED then KeyboardLogDebug(@Keyboard.Keyboard,'PL050: KMI Keyboard Disable');
 {$ENDIF}
 
 {Send Disable Command}
 Result:=PL050KMICommand(Keyboard.Registers,PS2_KEYBOARD_COMMAND_DISABLE_SCAN,nil,0,nil,0);
end;

{==============================================================================}

function PL050KMIKeyboardSetLEDs(Keyboard:PPL050Keyboard;LEDs:LongWord):LongWord;
{Set the keyboard LEDs for a PL050 keyboard device}
{Keyboard: The keyboard device to set the scancode set for}
{LEDs: Type keyboard LED values to set (eg KEYBOARD_LED_CAPSLOCK)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Caller must hold the keyboard lock}
var
 Data:Byte;
 Value:Byte;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Keyboard}
 if Keyboard = nil then Exit;

 {$IF DEFINED(PL050_DEBUG) or DEFINED(KEYBOARD_DEBUG)}
 if KEYBOARD_LOG_ENABLED then KeyboardLogDebug(@Keyboard.Keyboard,'PL050: KMI Keyboard Set LEDs (LEDs=' + IntToHex(LEDs,8) + ')');
 {$ENDIF}

 {Get LEDs}
 Result:=KeyboardLEDsToPS2LEDs(LEDs,Data);
 if Result <> ERROR_SUCCESS then Exit;
 
 {Send Set LEDs Command}
 Result:=PL050KMICommand(Keyboard.Registers,PS2_KEYBOARD_COMMAND_SET_LEDS,@Data,SizeOf(Byte),@Value,SizeOf(Byte));
 if (Result = ERROR_SUCCESS) and (Value <> PS2_RESPONSE_ACK) then
  begin
   Result:=ERROR_OPERATION_FAILED;
  end;
end;
 
{==============================================================================}

function PL050KMIKeyboardSetTypematic(Keyboard:PPL050Keyboard;Rate,Delay:LongWord):LongWord;
{Set the typematic rate and delay for a PL050 keyboard device}
{Keyboard: The keyboard device to set the scancode set for}
{Rate: Type typematic repeat rate to set (Milliseconds)}
{Delay: The typematic repeat delay to set (Repeat rate intervals before first repeat)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Caller must hold the keyboard lock}
var
 Data:Byte;
 Value:Byte;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Keyboard}
 if Keyboard = nil then Exit;

 {$IF DEFINED(PL050_DEBUG) or DEFINED(KEYBOARD_DEBUG)}
 if KEYBOARD_LOG_ENABLED then KeyboardLogDebug(@Keyboard.Keyboard,'PL050: KMI Keyboard Set Typematic (Rate=' + IntToStr(Rate) + ' Delay=' + IntToStr(Delay) + ')');
 {$ENDIF}

 {Get Typematic}
 Result:=KeyboardRateAndDelayToPS2Typematic(Rate,Delay,Data);
 if Result <> ERROR_SUCCESS then Exit;

 {Send Set Rate And Delay Command}
 Result:=PL050KMICommand(Keyboard.Registers,PS2_KEYBOARD_COMMAND_SET_RATE_DELAY,@Data,SizeOf(Byte),@Value,SizeOf(Byte));
 if (Result = ERROR_SUCCESS) and (Value <> PS2_RESPONSE_ACK) then
  begin
   Result:=ERROR_OPERATION_FAILED;
  end;
end;

{==============================================================================}

function PL050KMIKeyboardGetScancodeSet(Keyboard:PPL050Keyboard;var ScancodeSet:Byte):LongWord;
{Get the current scancode set from a PL050 keyboard device}
{Keyboard: The keyboard device to get the scancode set from}
{ScancodeSet: The returned scancode set value (eg PS2_KEYBOARD_SCANCODE_SET2)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Caller must hold the keyboard lock}
var
 Data:Byte;
 Value:array[0..1] of Byte;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Setup Defaults}
 ScancodeSet:=0;
 
 {Check Keyboard}
 if Keyboard = nil then Exit;

 {$IF DEFINED(PL050_DEBUG) or DEFINED(KEYBOARD_DEBUG)}
 if KEYBOARD_LOG_ENABLED then KeyboardLogDebug(@Keyboard.Keyboard,'PL050: KMI Keyboard Get Scancode Set');
 {$ENDIF}

 {Set Data}
 Data:=PS2_KEYBOARD_SCANCODE_GET; 

 {Send Scancode Command}
 Result:=PL050KMICommand(Keyboard.Registers,PS2_KEYBOARD_COMMAND_SCANCODE,@Data,SizeOf(Byte),@Value,SizeOf(Value));
 if (Result = ERROR_SUCCESS) and (Value[0] <> PS2_RESPONSE_ACK) then
  begin
   Result:=ERROR_OPERATION_FAILED;
  end;
 
 {Get Scancode Set}
 if Result = ERROR_SUCCESS then ScancodeSet:=Value[1];
end;

{==============================================================================}

function PL050KMIKeyboardSetScancodeSet(Keyboard:PPL050Keyboard;ScancodeSet:Byte):LongWord;
{Set the scancode set for a PL050 keyboard device}
{Keyboard: The keyboard device to set the scancode set for}
{ScancodeSet: The scancode set to set (eg PS2_KEYBOARD_SCANCODE_SET2)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Caller must hold the keyboard lock}
var
 Value:Byte;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Keyboard}
 if Keyboard = nil then Exit;

 {$IF DEFINED(PL050_DEBUG) or DEFINED(KEYBOARD_DEBUG)}
 if KEYBOARD_LOG_ENABLED then KeyboardLogDebug(@Keyboard.Keyboard,'PL050: KMI Keyboard Set Scancode Set (ScancodeSet=' + IntToStr(ScancodeSet) + ')');
 {$ENDIF}

 {Send Scancode Command}
 Result:=PL050KMICommand(Keyboard.Registers,PS2_KEYBOARD_COMMAND_SCANCODE,@ScancodeSet,SizeOf(Byte),@Value,SizeOf(Byte));
 if (Result = ERROR_SUCCESS) and (Value <> PS2_RESPONSE_ACK) then
  begin
   Result:=ERROR_OPERATION_FAILED;
  end;
  
 {Update Scancode Set}
 if Result = ERROR_SUCCESS then Keyboard.ScancodeSet:=ScancodeSet;
end;

{==============================================================================}

function PL050KMIKeyboardCheckPressed(Keyboard:PPL050Keyboard;ScanCode:Word):Boolean;
{Check if the passed scan code has been pressed (True if not previously pressed)}
{Keyboard: The keyboard device to check for}
{ScanCode: The keyboard scan code to check}

{Note: Caller must hold the keyboard lock}
begin
 {}
 Result:=False;
 
 {Check Keyboard}
 if Keyboard = nil then Exit;
 
 if ScanCode <> Keyboard.LastCode then
  begin
   Result:=True;
  end;
end;

{==============================================================================}

function PL050KMIKeyboardCheckRepeated(Keyboard:PPL050Keyboard;ScanCode:Word):Boolean;
{Check if the passed scan code was the last key pressed and if the repeat delay has expired}
{Keyboard: The keyboard device to check for}
{ScanCode: The keyboard scan code to check}

{Note: Caller must hold the keyboard lock}
begin
 {}
 Result:=False;
 
 {Check Keyboard}
 if Keyboard = nil then Exit;
 
 if ScanCode = Keyboard.LastCode then
  begin
   Result:=True;
  end;
end;

{==============================================================================}

function PL050KMIMouseReset(Mouse:PPL050Mouse):LongWord;
{Reset a PL050 mouse device}
{Mouse: The mouse device to reset}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Caller must hold the mouse lock}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Mouse}
 if Mouse = nil then Exit;

 {$IF DEFINED(PL050_DEBUG) or DEFINED(MOUSE_DEBUG)}
 if MOUSE_LOG_ENABLED then MouseLogDebug(@Mouse.Mouse,'PL050: KMI Mouse Reset');
 {$ENDIF}
 
 {Send Reset Command}
 Result:=PL050KMICommand(Mouse.Registers,PS2_MOUSE_COMMAND_RESET,nil,0,nil,0);
end;
 
{==============================================================================}

function PL050KMIMouseEnable(Mouse:PPL050Mouse):LongWord;
{Enable a PL050 mouse device}
{Mouse: The mouse device to enable}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Caller must hold the mouse lock}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Mouse}
 if Mouse = nil then Exit;

 {$IF DEFINED(PL050_DEBUG) or DEFINED(MOUSE_DEBUG)}
 if MOUSE_LOG_ENABLED then MouseLogDebug(@Mouse.Mouse,'PL050: KMI Mouse Enable');
 {$ENDIF}
 
 {Send Enable Command}
 Result:=PL050KMICommand(Mouse.Registers,PS2_MOUSE_COMMAND_ENABLE_REPORT,nil,0,nil,0);
end;

{==============================================================================}

function PL050KMIMouseDisable(Mouse:PPL050Mouse):LongWord;
{Disable a PL050 mouse device}
{Mouse: The mouse device to disable}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Caller must hold the mouse lock}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Mouse}
 if Mouse = nil then Exit;

 {$IF DEFINED(PL050_DEBUG) or DEFINED(MOUSE_DEBUG)}
 if MOUSE_LOG_ENABLED then MouseLogDebug(@Mouse.Mouse,'PL050: KMI Mouse Disable');
 {$ENDIF}
 
 {Send Disable Command}
 Result:=PL050KMICommand(Mouse.Registers,PS2_MOUSE_COMMAND_DISABLE_REPORT,nil,0,nil,0);
end;

{==============================================================================}

function PL050KMIMouseSetSampleRate(Mouse:PPL050Mouse;Rate:Byte):LongWord;
{Set the sample rate on a PL050 mouse device}
{Mouse: The mouse device to set the rate for}
{Rate: The sample rate to set (samples per second)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Caller must hold the mouse lock}
var
 Data:Byte;
 Value:Byte;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Mouse}
 if Mouse = nil then Exit;

 {$IF DEFINED(PL050_DEBUG) or DEFINED(MOUSE_DEBUG)}
 if MOUSE_LOG_ENABLED then MouseLogDebug(@Mouse.Mouse,'PL050: KMI Mouse Set Sample Rate (Rate=' + IntToStr(Rate) + ')');
 {$ENDIF}
 
 {Get Rate}
 Result:=MouseSampleRateToPS2SampleRate(Rate,Data);
 if Result <> ERROR_SUCCESS then Exit;
 
 {Send Set Sample Rate Command}
 Result:=PL050KMICommand(Mouse.Registers,PS2_MOUSE_COMMAND_SET_SAMPLE_RATE,@Data,SizeOf(Byte),@Value,SizeOf(Byte));
 if (Result = ERROR_SUCCESS) and (Value <> PS2_RESPONSE_ACK) then
  begin
   Result:=ERROR_OPERATION_FAILED;
  end;
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

