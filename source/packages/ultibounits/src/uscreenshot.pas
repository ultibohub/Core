{
Ultibo Screenshot utility unit.

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

 
References
==========


uScreenshot
===========
      
 Functions to capture all or part of the Ultibo console screen to a bitmap
 file.
 
 These functions originally appeared in the Ultibo forum and examples of
 how to use them can be found there:

 SaveScreen(Ex): https://ultibo.org/forum/viewtopic.php?f=13&t=313
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit uScreenshot; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Console,GraphicsConsole,Classes,SysUtils,BMPcomn;
            
{==============================================================================}

function SaveScreen(const Filename:String;X,Y,Width,Height,BPP:LongWord):Boolean;
function SaveScreenEx(Console:PConsoleDevice;const Filename:String;X,Y,Width,Height,BPP:LongWord):Boolean;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}

function SaveScreen(const Filename:String;X,Y,Width,Height,BPP:LongWord):Boolean; inline;
{A function for saving all or part of the default Ultibo console screen to a standard bitmap file}
{Filename: The name of the file to save the bitmap to}
{X: The column position for the left edge of the bitmap}
{Y: The row position for the top row of the bitmap}
{Width: The width (in pixels) of the bitmap}
{Height: The height (in pixels) of the bitmap}
{BPP: The bits per pixel value for the bitmap (eg 16, 24 or 32)}
{Return: True if successful or False is an error occurred}
begin
 {}
 Result:=SaveScreenEx(ConsoleDeviceGetDefault,Filename,X,Y,Width,Height,BPP);
end;

{==============================================================================}

function SaveScreenEx(Console:PConsoleDevice;const Filename:String;X,Y,Width,Height,BPP:LongWord):Boolean;
{A function for saving all or part of an Ultibo console screen to a standard bitmap file}
{Console: The console device to save the bitmap from}
{Filename: The name of the file to save the bitmap to}
{X: The column position for the left edge of the bitmap}
{Y: The row position for the top row of the bitmap}
{Width: The width (in pixels) of the bitmap}
{Height: The height (in pixels) of the bitmap}
{BPP: The bits per pixel value for the bitmap (eg 16, 24 or 32)}
{Return: True if successful or False is an error occurred}
var
 Size:LongWord;
 Count:LongWord;
 Offset:LongWord;
 Format:LongWord;
 Buffer:Pointer;
 LineSize:LongWord;
 ReadSize:LongWord;
 MemoryStream:TMemoryStream;
 
 BitMapFileHeader:TBitMapFileHeader;
 BitMapInfoHeader:TBitMapInfoHeader;
begin
 {}
 Result:=False;
 try
  {Check the parameters}
  if Console = nil then Exit;
  if Length(Filename) = 0 then Exit;
  if (Width = 0) or (Height = 0) then Exit;
 
  {Check the BPP (Bits Per Pixel) value. It must be 16, 24 or 32 for this function}
  if BPP = 16 then
   begin
    {Get the color format}
    Format:=COLOR_FORMAT_RGB15;
    {Work out the number of bytes per line}
    LineSize:=Width * 2;
    {And the actual number of bytes until the next line}
    ReadSize:=(((Width * 8 * 2) + 31) div 32) shl 2;
   end
  else if BPP = 24 then
   begin
    {Color format, bytes per line and actual bytes again}
    Format:=COLOR_FORMAT_RGB24;
    LineSize:=Width * 3;
    ReadSize:=(((Width * 8 * 3) + 31) div 32) shl 2;
   end
  else if BPP = 32 then
   begin
    {Color format, bytes per line and actual bytes as above}
    Format:=COLOR_FORMAT_URGB32;
    LineSize:=Width * 4;
    ReadSize:=(((Width * 8 * 4) + 31) div 32) shl 2;
   end
  else
   begin
    Exit;
   end;
   
  {Check the file does not exist}
  if FileExists(Filename) then Exit;
 
  {Create the TMemoryStream object}
  MemoryStream:=TMemoryStream.Create;
  try
   {Get the total size of the image in the file (not including the headers)}
   Size:=ReadSize * Height;
   
   {Set the size of the memory stream (Adding the size of the headers)}
   MemoryStream.Size:=Size + SizeOf(TBitMapFileHeader) + SizeOf(TBitMapInfoHeader);
   MemoryStream.Position:=0;
   
   {Create the Bitmap file header}
   FillChar(BitMapFileHeader,SizeOf(TBitMapFileHeader),0);
   BitMapFileHeader.bfType:=BMmagic;
   BitMapFileHeader.bfSize:=Size + SizeOf(TBitMapFileHeader) + SizeOf(TBitMapInfoHeader);
   BitMapFileHeader.bfReserved:=0;
   BitMapFileHeader.bfOffset:=SizeOf(TBitMapFileHeader) + SizeOf(TBitMapInfoHeader);
   if MemoryStream.Write(BitMapFileHeader,SizeOf(TBitMapFileHeader)) <> SizeOf(TBitMapFileHeader) then Exit;
   
   {And create the Bitmap info header}
   FillChar(BitMapInfoHeader,SizeOf(TBitMapInfoHeader),0);
   BitMapInfoHeader.Size:=SizeOf(TBitMapInfoHeader);
   BitMapInfoHeader.Width:=Width;
   BitMapInfoHeader.Height:=Height;
   BitMapInfoHeader.Planes:=1;
   BitMapInfoHeader.BitCount:=BPP;
   BitMapInfoHeader.Compression:=BI_RGB;
   BitMapInfoHeader.SizeImage:=Size;
   BitMapInfoHeader.XPelsPerMeter:=3780; {96 DPI} {(3780 / 1000) * 25.4}
   BitMapInfoHeader.YPelsPerMeter:=3780; {96 DPI} {(3780 / 1000) * 25.4}
   BitMapInfoHeader.ClrUsed:=0;
   BitMapInfoHeader.ClrImportant:=0;
   if MemoryStream.Write(BitMapInfoHeader,SizeOf(TBitMapInfoHeader)) <> SizeOf(TBitMapInfoHeader) then Exit;
 
   {Get the size of the pixels to be copied from the screen}
   Size:=LineSize * BitMapInfoHeader.Height;
     
   {Allocate a buffer to copy to}
   Buffer:=GetMem(Size);
   try
    Offset:=0;
     
    {Get the entire image from the screen into our buffer. The function will translate the colors into the format we asked for}
    if ConsoleDeviceGetImage(Console,X,Y,Buffer,BitMapInfoHeader.Width,BitMapInfoHeader.Height,Format,0) <> ERROR_SUCCESS then Exit;
   
    {Go through each row in the image starting at the bottom because bitmaps are normally upside down}
    for Count:=BitMapInfoHeader.Height - 1 downto 0 do
     begin
      {Update the position of the memory stream for the next row}
      MemoryStream.Position:=BitMapFileHeader.bfOffset + (Count * ReadSize);
     
      {Write a full line of pixels to the memory stream from our buffer}     
      if MemoryStream.Write((Buffer + Offset)^,LineSize) <> LineSize then Exit;
         
      {Update the offset of our buffer}   
      Inc(Offset,LineSize);
     end;
   
    {Write the memory stream to the file}
    MemoryStream.SaveToFile(Filename);
   
    Result:=True;
   finally
    FreeMem(Buffer);
   end;
  finally
   MemoryStream.Free;
  end;
 except
  on E: Exception do
   begin
    {Log an error or return a message etc}
   end;
 end;
end;

{==============================================================================}
{==============================================================================}

end.
