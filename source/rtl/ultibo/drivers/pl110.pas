{
ARM PrimeCell PL110 Color LCD Controller Driver.

Copyright (C) 2020 - SoftOz Pty Ltd.

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

  Linux - \drivers\video\fbdev\amba-clcd.c - Copyright (C) 2001 ARM Limited
  Linux - \drivers\video\fbdev\amba-clcd-versatile.c
  Linux - \include\linux\amba\clcd.h - Copyright (C) 2001 ARM Limited
  
  QEMU - \hw\display\pl110.c - Copyright (c) 2005-2009 CodeSourcery
  QEMU - \hw\display\pl110_template.h - Copyright (c) 2005 CodeSourcery
  
References
==========

 PL110 - http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0161e/I913915.html

ARM PrimeCell PL110 Color LCD
=============================

 The ARM PrimeCell PL110 Color LCD Controller is an AMBA compliant module that provides LCD
 display support for both TFT and STN displays in a variety of configurations.
 
 While the controller supports TFT displays it differs from other TFT display controllers
 because it has a DMA interface to system memory rather than using SPI or other serial transfer
 protocols.
 
 Currently this driver only supports the setup required for the QEMU VersatilePB target
 however it is possible to support additional configurations by adding more variations
 of the PL110FramebufferCreate functions (eg PL110FramebufferCreateSTN etc)
 
 Note: The driver does not include support for FRAMEBUFFER_FLAG_SYNC as the QEMU emulation
 of the device does not provide interrupt support at present.
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit PL110; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,HeapManager,Devices,DMA,Framebuffer,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {PL110 specific constants}
 PL110_FRAMEBUFFER_DESCRIPTION = 'ARM PrimeCell PL110 Color LCD';  {Description of PL110 device}

 PL110_MAX_PHYSICALWIDTH = 1024;
 PL110_MAX_PHYSICALHEIGHT = 1024;
 
 {PL110 mode constants}
 PL110_MODE_UNKNOWN = 0;
 PL110_MODE_VGA     = 1; {Connected to a VGA display}
 PL110_MODE_SVGA    = 2; {Connected to a SVGA display}
 PL110_MODE_TFT     = 3; {Connected to a TFT display}
 PL110_MODE_STN     = 4; {Connected to an STN display}

 {PL110 register offsets (See: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0161e/I913915.html)}
 PL110_CLCD_TIMING0 = $00000000; {Horizontal Axis Panel Control Register}
 PL110_CLCD_TIMING1 = $00000004; {Vertical Axis Panel Control Register}
 PL110_CLCD_TIMING2 = $00000008; {Clock and Signal Polarity Control Register}
 PL110_CLCD_TIMING3 = $0000000c; {Line End Control Register}
 PL110_CLCD_UPBASE  = $00000010; {Upper Panel Frame Base Address Registers}
 PL110_CLCD_LPBASE  = $00000014; {Lower Panel Frame Base Address Registers}
 PL110_CLCD_CONTROL = $00000018; {Control Register}                           {Note: Reversed in VersatilePB implementation, 0x0000001c in PL110 TRM}
 PL110_CLCD_IMSC    = $0000001c; {Interrupt Mask Set/Clear Register}          {Note: Reversed in VersatilePB implementation, 0x00000018 in PL110 TRM}
 PL110_CLCD_RIS     = $00000020; {Raw Interrupt Status Register}
 PL110_CLCD_MIS     = $00000024; {Masked Interrupt Status Register}
 PL110_CLCD_ICR     = $00000028; {Interrupt Clear Register}
 PL110_CLCD_UPCURR  = $0000002C; {Upper Panel Current Address Value Registers}
 PL110_CLCD_LPCURR  = $00000030; {Lower Panel Current Address Value Registers}
 PL110_CLCD_PALETTE = $00000200; {Color Palette Register}

 {PL110 Timing0 register bits (See: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0161e/I913915.html)}
 PL110_CLCD_TIMING0_HBP = ($FF shl 24); {Horizontal back porch}
 PL110_CLCD_TIMING0_HFP = ($FF shl 16); {Horizontal front porch}
 PL110_CLCD_TIMING0_HSW = ($FF shl 8);  {Horizontal synchronization pulse width} 
 PL110_CLCD_TIMING0_PPL = ($FC shl 2);  {Pixels-per-line (Actual pixels-per-line = 16 * (PPL + 1))}

 {PL110 Timing1 register bits (See: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0161e/I913915.html)}
 PL110_CLCD_TIMING1_VBP = ($FF shl 24); {Vertical back porch}
 PL110_CLCD_TIMING1_VFP = ($FF shl 16); {Vertical front porch} 
 PL110_CLCD_TIMING1_VSW = ($FC shl 10); {Vertical synchronization pulse width}
 PL110_CLCD_TIMING1_LPP = ($3FF shl 0); {Lines per panel is the number of active lines per screen (Program to number of lines required minus 1)} 
 
 {PL110 Timing2 register bits (See: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0161e/I913915.html)}
 PL110_CLCD_TIMING2_PCD_HI = ($1F shl 27);  {Upper five bits of Panel Clock Divisor}
 PL110_CLCD_TIMING2_BCD    = (1 shl 26);    {Bypass pixel clock divider}
 PL110_CLCD_TIMING2_CPL    = ($3FF shl 16); {Clocks per line}
 PL110_CLCD_TIMING2_IOE    = (1 shl 14);    {Invert output enable}
 PL110_CLCD_TIMING2_IPC    = (1 shl 13);    {Invert panel clock}
 PL110_CLCD_TIMING2_IHS    = (1 shl 12);    {Invert horizontal synchron}
 PL110_CLCD_TIMING2_IVS    = (1 shl 11);    {Invert vertical synchronization}
 PL110_CLCD_TIMING2_ACB    = ($1F shl 6);   {AC bias pin frequency}
 PL110_CLCD_TIMING2_CLKSEL = (1 shl 5);     {This bit drives the CLCDCLKSEL signal which is used as the select signal for the external LCD clock multiplexor}
 PL110_CLCD_TIMING2_PCD_LO = ($1F shl 0);   {Lower five bits of Panel Clock Divisor}
 
 {PL110 Timing3 register bits (See: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0161e/I913915.html)}
 PL110_CLCD_TIMING3_LEE = (1 shl 16);  {LCD Line end enable: 0 = CLLE disabled (held LOW) / 1 = CLLE signal active}
 PL110_CLCD_TIMING3_LED = ($3F shl 0); {Line-end signal delay from the rising-edge of the last panel clock}
 
 {PL110 Control register bits (See: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0161e/I913915.html)}
 PL110_CLCD_CONTROL_LCDEN           = (1 shl 0);  {}
 PL110_CLCD_CONTROL_LCDBPP1         = (0 shl 1);  {LCD bits per pixel: 000 = 1 bpp}
 PL110_CLCD_CONTROL_LCDBPP2         = (1 shl 1);  {                    001 = 2 bpp}
 PL110_CLCD_CONTROL_LCDBPP4         = (2 shl 1);  {                    010 = 4 bpp}
 PL110_CLCD_CONTROL_LCDBPP8         = (3 shl 1);  {                    011 = 8 bpp}
 PL110_CLCD_CONTROL_LCDBPP16        = (4 shl 1);  {                    100 = 16 bpp}
 PL110_CLCD_CONTROL_LCDBPP16_565    = (6 shl 1);  {                    110 = 16 bpp 565 (PL111 only)}
 PL110_CLCD_CONTROL_LCDBPP16_444    = (7 shl 1);  {                    111 = 16 bpp 444 (PL111 only)}
 PL110_CLCD_CONTROL_LCDBPP24        = (5 shl 1);  {                    101 = 24 bpp}
 PL110_CLCD_CONTROL_LCDBW           = (1 shl 4);  {STN LCD is monochrome (black and white) (0 = STN LCD is color / 1 = STN LCD is monochrome)}
 PL110_CLCD_CONTROL_LCDTFT          = (1 shl 5);  {LCD is TFT (0 = LCD is an STN display, use gray scaler / 1 = LCD is TFT, do not use gray scaler)}
 PL110_CLCD_CONTROL_LCDMONO8        = (1 shl 6);  {Monochrome LCD has an 8-bit interface (0 = mono LCD uses 4-bit interface / 1 = mono LCD uses 8-bit interface)}
 PL110_CLCD_CONTROL_LCDDUAL         = (1 shl 7);  {LCD interface is dual panel STN (0 = single panel LCD is in use / 1 = dual panel LCD is in use)}
 PL110_CLCD_CONTROL_BGR             = (1 shl 8);  {RGB of BGR format selection (0 = RGB normal output / 1 = BGR red and blue swapped.)}
 PL110_CLCD_CONTROL_BEBO            = (1 shl 9);  {Big-endian byte order (0 = little-endian byte order / 1 = big-endian byte order)}
 PL110_CLCD_CONTROL_BEPO            = (1 shl 10); {Big-endian pixel ordering within a byte (0 = little-endian pixel ordering within a byte / 1= big-endian pixel ordering within a byte)}
 PL110_CLCD_CONTROL_LCDPWR          = (1 shl 11); {LCD power enable}
 PL110_CLCD_CONTROL_LCDVCOMP_VSYNC  = (0 shl 12); {Generate interrupt at: 00 = start of vertical synchronization}
 PL110_CLCD_CONTROL_LCDVCOMP_BPORCH = (1 shl 12); {                       01 = start of back porch}
 PL110_CLCD_CONTROL_LCDVCOMP_VIDEO  = (2 shl 12); {                       10 = start of active video}
 PL110_CLCD_CONTROL_LCDVCOMP_FPORCH = (3 shl 12); {                       11 = start of front porch}
 PL110_CLCD_CONTROL_LDMAFIFOTIME    = (1 shl 15); {Unknown}
 PL110_CLCD_CONTROL_WATERMARK       = (1 shl 16); {LCD DMA FIFO Watermark level}

 {PL110 control constants}
 PL110_CONTROL_VGA = PL110_CLCD_CONTROL_LCDTFT or PL110_CLCD_CONTROL_LCDVCOMP_BPORCH;
 PL110_CONTROL_SVGA = PL110_CLCD_CONTROL_LCDTFT or PL110_CLCD_CONTROL_LCDVCOMP_BPORCH;
 
 {PL110 timing0 constants}
 PL110_TIMING0_VGA = $3F1F3F9C;
 PL110_TIMING0_SVGA = $1313A4C4;

 {PL110 timing1 constants}
 PL110_TIMING1_VGA = $090B61DF;
 PL110_TIMING1_SVGA = $0505F657;
  
 {PL110 timing2 constants}
 PL110_TIMING2_VGA = $067F1800;
 PL110_TIMING2_SVGA = $071F1800;
 
{==============================================================================}
type
 {PL110 specific types}
 {Layout of the PL110 registers (See: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0161e/I904421.html)}
 PPL110CLCDRegisters = ^TPL110CLCDRegisters;
 TPL110CLCDRegisters = record
  TIMING0:LongWord; {Horizontal Axis Panel Control Register}
  TIMING1:LongWord; {Vertical Axis Panel Control Register}
  TIMING2:LongWord; {Clock and Signal Polarity Control Register}
  TIMING3:LongWord; {Line End Control Register}
  UPBASE:LongWord;  {Upper Panel Frame Base Address Registers}
  LPBASE:LongWord;  {Lower Panel Frame Base Address Registers}
  CONTROL:LongWord; {Control Register}                           {Note: Reversed in VersatilePB implementation, 0x0000001c in PL110 TRM}
  IMSC:LongWord;    {Interrupt Mask Set/Clear Register}          {Note: Reversed in VersatilePB implementation, 0x00000018 in PL110 TRM}
  RIS:LongWord;     {Raw Interrupt Status Register}
  MIS:LongWord;     {Masked Interrupt Status Register}
  ICR:LongWord;     {Interrupt Clear Register}
  UPCURR:LongWord;  {Upper Panel Current Address Value Registers}
  LPCURR:LongWord;  {Lower Panel Current Address Value Registers}
 end; 
 
 PPL110Framebuffer = ^TPL110Framebuffer;
 TPL110Framebuffer = record
  {Framebuffer Properties}
  Framebuffer:TFramebufferDevice;
  {PL110 Properties}
  Mode:LongWord;                  {PL110 framebuffer mode (eg PL110_MODE_TFT)}
  Depth:LongWord;                 {Framebuffer color depth (eg FRAMEBUFFER_DEPTH_16)}
  Width:LongWord;                 {Framebuffer width in pixels}
  Height:LongWord;                {Framebuffer height in pixels}
  Rotation:LongWord;              {Framebuffer rotation (eg FRAMEBUFFER_ROTATION_180)}
  {Driver Properties}
  Control:LongWord;               {Preset Control register value}
  Timing0:LongWord;               {Preset Timing0 register value}
  Timing1:LongWord;               {Preset Timing1 register value}
  Timing2:LongWord;               {Preset Timing2 register value}
  Timing3:LongWord;               {Preset Timing2 register value}
  Registers:PPL110CLCDRegisters;  {PL110 registers}
 end; 
 
{==============================================================================}
{var}
 {PL110 specific variables}
 
{==============================================================================}
{Initialization Functions}
 
{==============================================================================}
{PL110 Functions}
function PL110FramebufferCreateVGA(Address:PtrUInt;const Name:String;Rotation,Width,Height,Depth:LongWord):PFramebufferDevice;
function PL110FramebufferCreateSVGA(Address:PtrUInt;const Name:String;Rotation,Width,Height,Depth:LongWord):PFramebufferDevice;

function PL110FramebufferDestroy(Framebuffer:PFramebufferDevice):LongWord;

{==============================================================================}
{PL110 Framebuffer Functions}
function PL110FramebufferAllocate(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
function PL110FramebufferRelease(Framebuffer:PFramebufferDevice):LongWord;

function PL110FramebufferBlank(Framebuffer:PFramebufferDevice;Blank:Boolean):LongWord;

function PL110FramebufferCommit(Framebuffer:PFramebufferDevice;Address:PtrUInt;Size,Flags:LongWord):LongWord;

function PL110FramebufferSetOffset(Framebuffer:PFramebufferDevice;X,Y:LongWord;Pan:Boolean):LongWord;

function PL110FramebufferSetProperties(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;

{==============================================================================}
{PL110 Helper Functions}

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{var}
 {PL110 specific variables}
 
{==============================================================================}
{==============================================================================}
{Initialization Functions}
 
{==============================================================================}
{==============================================================================}
{PL110 Functions}
function PL110FramebufferCreateVGA(Address:PtrUInt;const Name:String;Rotation,Width,Height,Depth:LongWord):PFramebufferDevice;
{Create, register and allocate a new PL110 Framebuffer device which can be accessed using the framebuffer API}
{Address: The address of the PL110 registers}
{Name: The text description of this device which will show in the device list (Optional)}
{Rotation: The rotation value for the framebuffer device (eg FRAMEBUFFER_ROTATION_180)}
{Width: The width of the framebuffer in pixels}
{Height: The height of the framebuffer in pixels}
{Depth: The color depth (bits per pixel) for the framebuffer (eg FRAMEBUFFER_DEPTH_16)}
{Return: Pointer to the new Framebuffer device or nil if the framebuffer device could not be created}
var
 Status:LongWord;
 PL110Framebuffer:PPL110Framebuffer;
begin
 {}
 Result:=nil;
 
 {$IF DEFINED(PL110_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'PL110: Framebuffer Create VGA (Address=' + AddrToHex(Address) + ' Name=' + Name + ' Width=' + IntToStr(Width) + ' Height=' + IntToStr(Height) + ')');
 {$ENDIF}

 {Check Address}
 if Address = 0 then Exit;
 
 {Check Rotation}
 if Rotation > FRAMEBUFFER_ROTATION_270 then Exit;
 
 {Check Width and Height}
 if Width < 1 then Exit;
 if Height < 1 then Exit;
 if Width > PL110_MAX_PHYSICALWIDTH then Exit;
 if Height > PL110_MAX_PHYSICALHEIGHT then Exit;
 
 {Check Depth}
 if (Depth <> FRAMEBUFFER_DEPTH_16) and (Depth <> FRAMEBUFFER_DEPTH_32) then Exit;
 
 {Create Framebuffer}
 PL110Framebuffer:=PPL110Framebuffer(FramebufferDeviceCreateEx(SizeOf(TPL110Framebuffer)));
 if PL110Framebuffer <> nil then
  begin
   {Update Framebuffer}
   {Device}
   PL110Framebuffer.Framebuffer.Device.DeviceBus:=DEVICE_BUS_MMIO; 
   PL110Framebuffer.Framebuffer.Device.DeviceType:=FRAMEBUFFER_TYPE_HARDWARE;
   PL110Framebuffer.Framebuffer.Device.DeviceFlags:=FRAMEBUFFER_FLAG_DMA or FRAMEBUFFER_FLAG_COMMIT or FRAMEBUFFER_FLAG_BLANK or FRAMEBUFFER_FLAG_CACHED or FRAMEBUFFER_FLAG_VIRTUAL or FRAMEBUFFER_FLAG_OFFSETY{$IFDEF FPC_BIG_ENDIAN}or FRAMEBUFFER_FLAG_SWAP{$ENDIF FPC_BIG_ENDIAN};
   PL110Framebuffer.Framebuffer.Device.DeviceData:=nil;
   if Length(Name) <> 0 then PL110Framebuffer.Framebuffer.Device.DeviceDescription:=Name else PL110Framebuffer.Framebuffer.Device.DeviceDescription:=PL110_FRAMEBUFFER_DESCRIPTION;
   {Framebuffer}
   PL110Framebuffer.Framebuffer.FramebufferState:=FRAMEBUFFER_STATE_DISABLED;
   PL110Framebuffer.Framebuffer.DeviceAllocate:=PL110FramebufferAllocate;
   PL110Framebuffer.Framebuffer.DeviceRelease:=PL110FramebufferRelease;
   PL110Framebuffer.Framebuffer.DeviceBlank:=PL110FramebufferBlank;
   PL110Framebuffer.Framebuffer.DeviceCommit:=PL110FramebufferCommit;
   PL110Framebuffer.Framebuffer.DeviceSetProperties:=PL110FramebufferSetProperties;
   PL110Framebuffer.Framebuffer.DeviceSetOffset:=PL110FramebufferSetOffset;
   {PL110}
   PL110Framebuffer.Mode:=PL110_MODE_VGA;
   PL110Framebuffer.Depth:=Depth;
   PL110Framebuffer.Width:=Width;
   PL110Framebuffer.Height:=Height;
   PL110Framebuffer.Rotation:=Rotation;
   if (Rotation = FRAMEBUFFER_ROTATION_90) or (Rotation = FRAMEBUFFER_ROTATION_270) then
    begin
     PL110Framebuffer.Width:=Height;
     PL110Framebuffer.Height:=Width;
    end;
   {Driver}
   PL110Framebuffer.Control:=PL110_CONTROL_VGA;
   PL110Framebuffer.Timing0:=PL110_TIMING0_VGA;
   PL110Framebuffer.Timing1:=PL110_TIMING1_VGA;
   PL110Framebuffer.Timing2:=PL110_TIMING2_VGA;
   PL110Framebuffer.Timing3:=0;
   PL110Framebuffer.Registers:=PPL110CLCDRegisters(Address);
   
   {Setup Flags}
   {Nothing}
   
   {Register Framebuffer}
   Status:=FramebufferDeviceRegister(@PL110Framebuffer.Framebuffer);
   if Status = ERROR_SUCCESS then
    begin
     {Allocate Framebuffer}
     Status:=FramebufferDeviceAllocate(@PL110Framebuffer.Framebuffer,nil);
     if Status = ERROR_SUCCESS then
      begin
       {Return Result}
       Result:=PFramebufferDevice(PL110Framebuffer); 
      end
     else
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'PL110: Failed to allocate new framebuffer device: ' + ErrorToString(Status));
      end;
    end
   else
    begin     
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'PL110: Failed to register new framebuffer device: ' + ErrorToString(Status));
    end;
  end
 else
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'PL110: Failed to create new framebuffer device');
  end;
end;

{==============================================================================}

function PL110FramebufferCreateSVGA(Address:PtrUInt;const Name:String;Rotation,Width,Height,Depth:LongWord):PFramebufferDevice;
{Create, register and allocate a new PL110 Framebuffer device which can be accessed using the framebuffer API}
{Address: The address of the PL110 registers}
{Name: The text description of this device which will show in the device list (Optional)}
{Rotation: The rotation value for the framebuffer device (eg FRAMEBUFFER_ROTATION_180)}
{Width: The width of the framebuffer in pixels}
{Height: The height of the framebuffer in pixels}
{Depth: The color depth (bits per pixel) for the framebuffer (eg FRAMEBUFFER_DEPTH_16)}
{Return: Pointer to the new Framebuffer device or nil if the framebuffer device could not be created}
var
 Status:LongWord;
 PL110Framebuffer:PPL110Framebuffer;
begin
 {}
 Result:=nil;
 
 {$IF DEFINED(PL110_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'PL110: Framebuffer Create SVGA (Address=' + AddrToHex(Address) + ' Name=' + Name + ' Width=' + IntToStr(Width) + ' Height=' + IntToStr(Height) + ')');
 {$ENDIF}

 {Check Address}
 if Address = 0 then Exit;
 
 {Check Rotation}
 if Rotation > FRAMEBUFFER_ROTATION_270 then Exit;
 
 {Check Width and Height}
 if Width < 1 then Exit;
 if Height < 1 then Exit;
 if Width > PL110_MAX_PHYSICALWIDTH then Exit;
 if Height > PL110_MAX_PHYSICALHEIGHT then Exit;
 
 {Check Depth}
 if (Depth <> FRAMEBUFFER_DEPTH_16) and (Depth <> FRAMEBUFFER_DEPTH_32) then Exit;
 
 {Create Framebuffer}
 PL110Framebuffer:=PPL110Framebuffer(FramebufferDeviceCreateEx(SizeOf(TPL110Framebuffer)));
 if PL110Framebuffer <> nil then
  begin
   {Update Framebuffer}
   {Device}
   PL110Framebuffer.Framebuffer.Device.DeviceBus:=DEVICE_BUS_MMIO; 
   PL110Framebuffer.Framebuffer.Device.DeviceType:=FRAMEBUFFER_TYPE_HARDWARE;
   PL110Framebuffer.Framebuffer.Device.DeviceFlags:=FRAMEBUFFER_FLAG_DMA or FRAMEBUFFER_FLAG_COMMIT or FRAMEBUFFER_FLAG_BLANK or FRAMEBUFFER_FLAG_CACHED or FRAMEBUFFER_FLAG_VIRTUAL or FRAMEBUFFER_FLAG_OFFSETY{$IFDEF FPC_BIG_ENDIAN}or FRAMEBUFFER_FLAG_SWAP{$ENDIF FPC_BIG_ENDIAN};
   PL110Framebuffer.Framebuffer.Device.DeviceData:=nil;
   if Length(Name) <> 0 then PL110Framebuffer.Framebuffer.Device.DeviceDescription:=Name else PL110Framebuffer.Framebuffer.Device.DeviceDescription:=PL110_FRAMEBUFFER_DESCRIPTION;
   {Framebuffer}
   PL110Framebuffer.Framebuffer.FramebufferState:=FRAMEBUFFER_STATE_DISABLED;
   PL110Framebuffer.Framebuffer.DeviceAllocate:=PL110FramebufferAllocate;
   PL110Framebuffer.Framebuffer.DeviceRelease:=PL110FramebufferRelease;
   PL110Framebuffer.Framebuffer.DeviceBlank:=PL110FramebufferBlank;
   PL110Framebuffer.Framebuffer.DeviceCommit:=PL110FramebufferCommit;
   PL110Framebuffer.Framebuffer.DeviceSetProperties:=PL110FramebufferSetProperties;
   PL110Framebuffer.Framebuffer.DeviceSetOffset:=PL110FramebufferSetOffset;
   {PL110}
   PL110Framebuffer.Mode:=PL110_MODE_SVGA;
   PL110Framebuffer.Depth:=Depth;
   PL110Framebuffer.Width:=Width;
   PL110Framebuffer.Height:=Height;
   PL110Framebuffer.Rotation:=Rotation;
   if (Rotation = FRAMEBUFFER_ROTATION_90) or (Rotation = FRAMEBUFFER_ROTATION_270) then
    begin
     PL110Framebuffer.Width:=Height;
     PL110Framebuffer.Height:=Width;
    end;
   {Driver}
   PL110Framebuffer.Control:=PL110_CONTROL_SVGA;
   PL110Framebuffer.Timing0:=PL110_TIMING0_SVGA;
   PL110Framebuffer.Timing1:=PL110_TIMING1_SVGA;
   PL110Framebuffer.Timing2:=PL110_TIMING2_SVGA;
   PL110Framebuffer.Timing3:=0;
   PL110Framebuffer.Registers:=PPL110CLCDRegisters(Address);
   
   {Setup Flags}
   {Nothing}
   
   {Register Framebuffer}
   Status:=FramebufferDeviceRegister(@PL110Framebuffer.Framebuffer);
   if Status = ERROR_SUCCESS then
    begin
     {Allocate Framebuffer}
     Status:=FramebufferDeviceAllocate(@PL110Framebuffer.Framebuffer,nil);
     if Status = ERROR_SUCCESS then
      begin
       {Return Result}
       Result:=PFramebufferDevice(PL110Framebuffer); 
      end
     else
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'PL110: Failed to allocate new framebuffer device: ' + ErrorToString(Status));
      end;
    end
   else
    begin     
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'PL110: Failed to register new framebuffer device: ' + ErrorToString(Status));
    end;
  end
 else
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'PL110: Failed to create new framebuffer device');
  end;
end;

{==============================================================================}

function PL110FramebufferDestroy(Framebuffer:PFramebufferDevice):LongWord;
{Release, deregister and destroy a PL110 Framebuffer device created by this driver}
{Framebuffer: The Framebuffer device to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 
 {$IF DEFINED(PL110_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'PL110: Framebuffer Destroy');
 {$ENDIF}
 
 {Release Framebuffer}
 Result:=FramebufferDeviceRelease(Framebuffer);
 if Result = ERROR_SUCCESS then
  begin
   {Deregister Framebuffer}
   Result:=FramebufferDeviceDeregister(Framebuffer);
   if Result = ERROR_SUCCESS then
    begin
     {Destroy Framebuffer}
     Result:=FramebufferDeviceDestroy(Framebuffer);
     if Result <> ERROR_SUCCESS then
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'PL110: Failed to destroy framebuffer device: ' + ErrorToString(Result));
      end;
    end
   else
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'PL110: Failed to deregister framebuffer device: ' + ErrorToString(Result));
    end;    
  end
 else
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'PL110: Failed to release framebuffer device: ' + ErrorToString(Result));
  end;  
end;

{==============================================================================}
{==============================================================================}
{PL110 Framebuffer Functions}
function PL110FramebufferAllocate(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
{Implementation of FramebufferDeviceAllocate API for PL110 Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDeviceAllocate instead}
var
 Value:LongWord;
 Bytes:LongWord;
 Buffer:Pointer;
 UpperBase:LongWord;
 LowerBase:LongWord;
 PhysicalWidth:LongWord;
 PhysicalHeight:LongWord;
 VirtualWidth:LongWord;
 VirtualHeight:LongWord;
 Defaults:TFramebufferProperties;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IF DEFINED(PL110_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'PL110: Framebuffer Allocate');
 {$ENDIF}
 
 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Get Defaults}
    Defaults.Depth:=PPL110Framebuffer(Framebuffer).Depth;
    Defaults.Order:=FRAMEBUFFER_ORDER_RGB;
    Defaults.Mode:=FRAMEBUFFER_MODE_IGNORED;
    Defaults.PhysicalWidth:=PPL110Framebuffer(Framebuffer).Width;
    Defaults.PhysicalHeight:=PPL110Framebuffer(Framebuffer).Height;
    Defaults.VirtualWidth:=Defaults.PhysicalWidth; 
    Defaults.VirtualHeight:=Defaults.PhysicalHeight;
    Defaults.OffsetX:=0;                           
    Defaults.OffsetY:=0;                            
    Defaults.OverscanTop:=0;                         
    Defaults.OverscanBottom:=0;                      
    Defaults.OverscanLeft:=0;                        
    Defaults.OverscanRight:=0;                       
    Defaults.Rotation:=PPL110Framebuffer(Framebuffer).Rotation;
 
    {Check Properties}
    if Properties <> nil then
     begin
      {Adjust Physical Width}
      PhysicalWidth:=Defaults.PhysicalWidth;
      if (Properties.PhysicalWidth <> 0) and (Properties.PhysicalWidth <> PhysicalWidth) and (Properties.PhysicalWidth <= PL110_MAX_PHYSICALWIDTH) then
       begin
        PhysicalWidth:=Properties.PhysicalWidth;
        Defaults.PhysicalWidth:=PhysicalWidth;
        Defaults.VirtualWidth:=PhysicalWidth;
       end; 
       
      {Adjust Physical Height}
      PhysicalHeight:=Defaults.PhysicalHeight;
      if (Properties.PhysicalHeight <> 0) and (Properties.PhysicalHeight <> PhysicalHeight) and (Properties.PhysicalHeight <= PL110_MAX_PHYSICALHEIGHT) then
       begin
        PhysicalHeight:=Properties.PhysicalHeight;
        Defaults.PhysicalHeight:=PhysicalHeight;
        Defaults.VirtualHeight:=PhysicalHeight;
       end;
       
      {Adjust Virtual Width}
      VirtualWidth:=Defaults.VirtualWidth;
      if (Properties.VirtualWidth <> 0) and (Properties.VirtualWidth <> VirtualWidth) then
       begin
        {Check Virtual Width}
        if Properties.VirtualWidth < PhysicalWidth then
         begin
          Properties.VirtualWidth:=PhysicalWidth;
         end;
        
        VirtualWidth:=Properties.VirtualWidth;
        Defaults.VirtualWidth:=VirtualWidth;
       end;
       
      {Adjust Virtual Height}
      VirtualHeight:=Defaults.VirtualHeight;
      if (Properties.VirtualHeight <> 0) and (Properties.VirtualHeight <> VirtualHeight) then
       begin
        {Check Virtual Height}
        if Properties.VirtualHeight < PhysicalHeight then
         begin
          Properties.VirtualHeight:=PhysicalHeight;
         end;

        VirtualHeight:=Properties.VirtualHeight;
        Defaults.VirtualHeight:=VirtualHeight;
       end;
      
      {Adjust Offset X} {Not supported}
      (*if Properties.OffsetX <> 0 then  
       begin
        {Check Offset X}
        if Properties.OffsetX > ((VirtualWidth - PhysicalWidth) - 1) then
         begin
          Properties.OffsetX:=0;
         end;
        
        Defaults.OffsetX:=Properties.OffsetX;
       end;*)

      {Adjust Offset Y}
      if Properties.OffsetY <> 0 then  
       begin
        {Check Offset Y}
        if Properties.OffsetY > ((VirtualHeight - PhysicalHeight) - 1) then
         begin
          Properties.OffsetY:=0;
         end;
        
        Defaults.OffsetY:=Properties.OffsetY;
       end; 
      
      {Adjust Depth}
      if (Properties.Depth = FRAMEBUFFER_DEPTH_16) or (Properties.Depth = FRAMEBUFFER_DEPTH_32) then Defaults.Depth:=Properties.Depth;
      
      {Adjust Order} {Do not allow}
      {if Properties.Order <= FRAMEBUFFER_ORDER_RGB then Defaults.Order:=Properties.Order;}
      
      {Adjust Rotation}
      if Properties.Rotation <= FRAMEBUFFER_ROTATION_270 then Defaults.Rotation:=Properties.Rotation;
      
      {Check Rotation}
      if Properties.Rotation <> PPL110Framebuffer(Framebuffer).Rotation then
       begin
        if (Properties.Rotation = FRAMEBUFFER_ROTATION_90) or (Properties.Rotation = FRAMEBUFFER_ROTATION_270) then 
         begin
          if (PPL110Framebuffer(Framebuffer).Rotation <> FRAMEBUFFER_ROTATION_90) and (PPL110Framebuffer(Framebuffer).Rotation <> FRAMEBUFFER_ROTATION_270) then
           begin
            Defaults.PhysicalWidth:=PhysicalHeight;
            Defaults.PhysicalHeight:=PhysicalWidth;
            Defaults.VirtualWidth:=VirtualHeight; 
            Defaults.VirtualHeight:=VirtualWidth;
           end;
         end
        else
         begin
          if (PPL110Framebuffer(Framebuffer).Rotation <> FRAMEBUFFER_ROTATION_0) and (PPL110Framebuffer(Framebuffer).Rotation <> FRAMEBUFFER_ROTATION_180) then
           begin
            Defaults.PhysicalWidth:=PhysicalHeight;
            Defaults.PhysicalHeight:=PhysicalWidth;
            Defaults.VirtualWidth:=VirtualHeight; 
            Defaults.VirtualHeight:=VirtualWidth;
           end;
         end;      
       end;
     end;
    
    {Get Format}  
    case Defaults.Depth of
     FRAMEBUFFER_DEPTH_16:begin
       Defaults.Format:=COLOR_FORMAT_RGB16;
      end;
     FRAMEBUFFER_DEPTH_32:begin
       Defaults.Format:=COLOR_FORMAT_UBGR32; {Note: This is reversed in the hardware}
      end;
    end;
 
    {Get Bytes}
    Bytes:=ColorFormatToBytes(Defaults.Format);
    if Bytes = 0 then Exit;
    
    {Get Size}
    Defaults.Size:=(Defaults.VirtualWidth * Defaults.VirtualHeight) * Bytes;
    
    {Get Pitch}
    Defaults.Pitch:=Defaults.VirtualWidth * Bytes;
 
    {Allocate Framebuffer}
    if ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_DMA) <> 0) and DMAAvailable then
     begin
      {Allocate DMA Buffer}
      Buffer:=DMAAllocateBuffer(Defaults.Size);
     end
    else
     begin
      {Allocate Normal Buffer (No DMA)}
      {Use DMA Alignment and Multiplier if available}
      if (DMA_ALIGNMENT <> 0) and (DMA_MULTIPLIER <> 0) then
       begin
        Buffer:=GetAlignedMem(RoundUp(Defaults.Size,DMA_MULTIPLIER),DMA_ALIGNMENT);
       end
      else
       begin      
        Buffer:=GetMem(Defaults.Size);
       end; 
     end;
    if Buffer = nil then
     begin
      Result:=ERROR_OPERATION_FAILED;
      Exit; 
     end; 
    
    {Check Cache}
    if not(DMA_CACHE_COHERENT) then
     begin
      {Clean Cache (Dest)}
      CleanDataCacheRange(PtrUInt(Buffer),Defaults.Size);
     end;
 
    {Update Framebuffer}
    Framebuffer.Address:=PtrUInt(Buffer);
    Framebuffer.Size:=Defaults.Size;
    Framebuffer.Pitch:=Defaults.Pitch;
    Framebuffer.Depth:=Defaults.Depth;
    Framebuffer.Order:=Defaults.Order;
    Framebuffer.Mode:=Defaults.Mode;
    Framebuffer.Format:=Defaults.Format;
    Framebuffer.PhysicalWidth:=Defaults.PhysicalWidth;
    Framebuffer.PhysicalHeight:=Defaults.PhysicalHeight;
    Framebuffer.VirtualWidth:=Defaults.VirtualWidth;
    Framebuffer.VirtualHeight:=Defaults.VirtualHeight;
    Framebuffer.OffsetX:=Defaults.OffsetX;
    Framebuffer.OffsetY:=Defaults.OffsetY;
    Framebuffer.OverscanTop:=Defaults.OverscanTop;
    Framebuffer.OverscanBottom:=Defaults.OverscanBottom;
    Framebuffer.OverscanLeft:=Defaults.OverscanLeft;
    Framebuffer.OverscanRight:=Defaults.OverscanRight;
    Framebuffer.Rotation:=Defaults.Rotation;
    
    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}
    
    {Setup PL110}
    Value:=PPL110Framebuffer(Framebuffer).Control;
    if Framebuffer.Depth = FRAMEBUFFER_DEPTH_16 then Value:=Value or PL110_CLCD_CONTROL_LCDBPP16;
    if Framebuffer.Depth = FRAMEBUFFER_DEPTH_32 then Value:=Value or PL110_CLCD_CONTROL_LCDBPP24;
    if Framebuffer.Order = FRAMEBUFFER_ORDER_BGR then Value:=Value or PL110_CLCD_CONTROL_BGR;
    PPL110Framebuffer(Framebuffer).Registers.CONTROL:=Value;
    PPL110Framebuffer(Framebuffer).Registers.TIMING0:=(PPL110Framebuffer(Framebuffer).Timing0 and not(PL110_CLCD_TIMING0_PPL)) or (((Framebuffer.PhysicalWidth - 1) div 16) shl 2);
    PPL110Framebuffer(Framebuffer).Registers.TIMING1:=(PPL110Framebuffer(Framebuffer).Timing1 and not(PL110_CLCD_TIMING1_LPP)) or (Framebuffer.PhysicalHeight - 1);
    PPL110Framebuffer(Framebuffer).Registers.TIMING2:=PPL110Framebuffer(Framebuffer).Timing2;
    PPL110Framebuffer(Framebuffer).Registers.TIMING3:=PPL110Framebuffer(Framebuffer).Timing3;
    UpperBase:=Framebuffer.Address + (Framebuffer.OffsetY * Framebuffer.Pitch);
    LowerBase:=UpperBase + ((Framebuffer.PhysicalHeight * Framebuffer.Pitch) div 2); 
    PPL110Framebuffer(Framebuffer).Registers.UPBASE:=UpperBase;
    PPL110Framebuffer(Framebuffer).Registers.LPBASE:=LowerBase;
    
    {Enable PL110}
    Value:=PPL110Framebuffer(Framebuffer).Registers.CONTROL;
    Value:=Value or PL110_CLCD_CONTROL_LCDEN;
    PPL110Framebuffer(Framebuffer).Registers.CONTROL:=Value; 
    MillisecondDelay(20);
    
    Value:=Value or PL110_CLCD_CONTROL_LCDPWR;
    PPL110Framebuffer(Framebuffer).Registers.CONTROL:=Value; 
    
    {Memory Barrier}
    DataMemoryBarrier; {After the Last Read} 
    
    {Update Statistics}
    Inc(Framebuffer.AllocateCount);
    
    {Return Result}
    Result:=ERROR_SUCCESS;
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

function PL110FramebufferRelease(Framebuffer:PFramebufferDevice):LongWord;
{Implementation of FramebufferDeviceRelease API for PL110 Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDeviceRelease instead}
var
 Value:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IF DEFINED(PL110_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'PL110: Framebuffer Release');
 {$ENDIF}
 
 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}
   
    {Disable PL110}
    Value:=PPL110Framebuffer(Framebuffer).Registers.CONTROL;
    if (Value and PL110_CLCD_CONTROL_LCDPWR) <> 0 then
     begin
      Value:=Value and not(PL110_CLCD_CONTROL_LCDPWR);
      PPL110Framebuffer(Framebuffer).Registers.CONTROL:=Value;
      MillisecondDelay(20);
     end;
     
    if (Value and PL110_CLCD_CONTROL_LCDEN) <> 0 then
     begin
      Value:=Value and not(PL110_CLCD_CONTROL_LCDEN);
      PPL110Framebuffer(Framebuffer).Registers.CONTROL:=Value; 
     end;
    
    {Memory Barrier}
    DataMemoryBarrier; {After the Last Read} 
    
    {Release Framebuffer}
    if ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_DMA) <> 0) and DMAAvailable then
     begin
      {Release DMA Buffer}
      Result:=DMAReleaseBuffer(Pointer(Framebuffer.Address));
      if Result <> ERROR_SUCCESS then Exit;
     end
    else
     begin
      {Release Normal Buffer (No DMA)}
      FreeMem(Pointer(Framebuffer.Address));
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
     
    {Return Result}
    Result:=ERROR_SUCCESS;
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

function PL110FramebufferBlank(Framebuffer:PFramebufferDevice;Blank:Boolean):LongWord;
{Implementation of FramebufferDevicBlank API for PL110 Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDevicBlank instead}
var
 Value:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 {$IF DEFINED(PL110_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'PL110: Framebuffer Blank (Blank=' + BooleanToString(Blank) + ')');
 {$ENDIF}
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Check Blank}
 if Blank then
  begin
   {Disable PL110}
   Value:=PPL110Framebuffer(Framebuffer).Registers.CONTROL;
   if (Value and PL110_CLCD_CONTROL_LCDPWR) <> 0 then
    begin
     Value:=Value and not(PL110_CLCD_CONTROL_LCDPWR);
     PPL110Framebuffer(Framebuffer).Registers.CONTROL:=Value;
     MillisecondDelay(20);
    end;
    
   if (Value and PL110_CLCD_CONTROL_LCDEN) <> 0 then
    begin
     Value:=Value and not(PL110_CLCD_CONTROL_LCDEN);
     PPL110Framebuffer(Framebuffer).Registers.CONTROL:=Value; 
    end;
  end
 else
  begin
   {Enable PL110}
   Value:=PPL110Framebuffer(Framebuffer).Registers.CONTROL;
   Value:=Value or PL110_CLCD_CONTROL_LCDEN;
   PPL110Framebuffer(Framebuffer).Registers.CONTROL:=Value; 
   MillisecondDelay(20);
   
   Value:=Value or PL110_CLCD_CONTROL_LCDPWR;
   PPL110Framebuffer(Framebuffer).Registers.CONTROL:=Value; 
  end;  
  
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
  
 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function PL110FramebufferCommit(Framebuffer:PFramebufferDevice;Address:PtrUInt;Size,Flags:LongWord):LongWord;
{Implementation of FramebufferDeviceCommit API for PL110 Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDeviceCommit instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IF DEFINED(PL110_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'PL110: Framebuffer Commit (Address=' + IntToHex(Address,8) + ' Size=' + IntToStr(Size) + ')');
 {$ENDIF}
 
 {Check Cache}
 if not DMA_CACHE_COHERENT then
  begin
   {Check Flags}
   if (Flags and FRAMEBUFFER_TRANSFER_DMA) = 0 then
    begin
     {Clean Cache}
     CleanAndInvalidateDataCacheRange(Address,Size); 
    end
   else
    begin
     {Invalidate Cache}
     InvalidateDataCacheRange(Address,Size);
    end;  
  end;  
 
 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function PL110FramebufferSetOffset(Framebuffer:PFramebufferDevice;X,Y:LongWord;Pan:Boolean):LongWord;
{Implementation of FramebufferDeviceSetOffset API for PL110 Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDeviceSetOffset instead}
var
 UpperBase:LongWord;
 LowerBase:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {$IF DEFINED(PL110_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'PL110: Framebuffer Set Offset (X=' + IntToStr(X) + ' Y=' + IntToStr(Y) + ')');
 {$ENDIF}

 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Check Offset}
    if (Y > 0) and (Y > ((Framebuffer.VirtualHeight - Framebuffer.PhysicalHeight) - 1)) then Exit;
    
    {Set Offset}
    UpperBase:=Framebuffer.Address + (Y * Framebuffer.Pitch);
    LowerBase:=UpperBase + ((Framebuffer.PhysicalHeight * Framebuffer.Pitch) div 2); 
      
    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}
    
    {Update PL110}
    PPL110Framebuffer(Framebuffer).Registers.UPBASE:=UpperBase;
    PPL110Framebuffer(Framebuffer).Registers.LPBASE:=LowerBase;
    
    {Update Offset}
    if not(Pan) then
     begin
      Framebuffer.OffsetY:=Y;
     end; 
    
    {Return Result}
    Result:=ERROR_SUCCESS;
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

function PL110FramebufferSetProperties(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
{Implementation of FramebufferDeviceSetProperties API for PL110 Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDeviceSetProperties instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IF DEFINED(PL110_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'PL110: Framebuffer Set Properties');
 {$ENDIF}
 
 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
   try
 
    //To Do //Check Properties against current, modify if possible, otherwise reallocate ? (and Notify Resize)
    
    {Return Result}
    Result:=ERROR_SUCCESS;
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
{PL110 Helper Functions}

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
