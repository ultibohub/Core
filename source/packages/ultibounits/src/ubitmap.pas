{
Ultibo Bitmap utility unit.

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

 
References
==========


uBitmap
=======
   
 Functions to draw or save a standard bitmap image to or from an Ultibo
 graphics console window. 
 
 These functions originally appeared in the Ultibo forum and examples of
 how to use them can be found there:
 
 DrawBitmap: https://ultibo.org/forum/viewtopic.php?f=13&t=312
 SaveBitmap: https://ultibo.org/forum/viewtopic.php?f=13&t=313

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

{$IFNDEF FPC_DOTTEDUNITS}
unit uBitmap; 
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Core.GlobalConfig,
  Core.GlobalConst,
  Core.GlobalTypes,
  Core.Platform,
  Core.Console,
  Core.GraphicsConsole,
  System.Classes,
  System.SysUtils,
  FpImage.Common.Bitmap;
{$ELSE FPC_DOTTEDUNITS}
uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Console,
  GraphicsConsole,
  Classes,
  SysUtils,
  BMPcomn;
{$ENDIF FPC_DOTTEDUNITS}

{==============================================================================}

function DrawBitmap(Handle:TWindowHandle;const Filename:String;X,Y:LongWord):Boolean;

function DrawBitmapFromStream(Handle:TWindowHandle;Stream:TStream;X,Y:LongWord):Boolean;
function DrawBitmapFromBuffer(Handle:TWindowHandle;const Buf;Len,X,Y:LongWord):Boolean;

function SaveBitmap(Handle:TWindowHandle;const Filename:String;X,Y,Width,Height,BPP:LongWord):Boolean;

function LoadBitmapToStream(const Filename:String;Stream:TStream;out Width,Height,Format:LongWord):Boolean;
function LoadBitmapToBuffer(const Filename:String;var Buf;var Len:LongWord;out Width,Height,Format:LongWord):Boolean;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}

function DrawBitmap(Handle:TWindowHandle;const Filename:String;X,Y:LongWord):Boolean;
{A function for drawing a standard bitmap image onto an Ultibo graphics console window}
{Handle: The handle of an existing graphics console window}
{Filename: The name of the file to load the bitmap from}
{X: The column position for the left edge of the bitmap}
{Y: The row position for the top row of the bitmap}
{Return: True if successful or False if an error occurred}
var
 Size:LongWord;
 Count:LongWord;
 Offset:LongWord;
 Format:LongWord;
 Buffer:Pointer;
 TopDown:Boolean;
 LineSize:LongWord;
 ReadSize:LongWord;
 FileStream:TFileStream;
 
 BitMapFileHeader:TBitMapFileHeader;
 BitMapInfoHeader:TBitMapInfoHeader;
begin
 {}
 Result:=False;
 
 {There are a few different ways to load a bitmap file and draw it on the screen in Ultibo, in this example
  we'll use a TFileStream class to read the file and then load the image data (the pixels) into a memory
  buffer that we allocate. Finally we'll put the pixels onto the screen using the GraphicsWindowDrawImage()
  function from the GraphicsConsole unit}
 
 {Check the parameters}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 if Length(Filename) = 0 then Exit;
 
 {Check if the file exists}
 if not FileExists(Filename) then Exit;
 
 {Open the file using a TFileStream class}
 FileStream:=TFileStream.Create(Filename,fmOpenRead or fmShareDenyNone);
 try
 
  {Check the file size}
  if FileStream.Size < (SizeOf(TBitMapFileHeader) + SizeOf(TBitMapInfoHeader)) then Exit;
 
  {Read the Bitmap file header}
  if FileStream.Read(BitMapFileHeader,SizeOf(TBitMapFileHeader)) <> SizeOf(TBitMapFileHeader) then Exit;
 
  {Check the magic number in the header}
  if BitMapFileHeader.bfType = BMmagic then
   begin
    {Read the Bitmap info header}
    if FileStream.Read(BitMapInfoHeader,SizeOf(TBitMapInfoHeader)) <> SizeOf(TBitMapInfoHeader) then Exit;
   
    {Most Bitmaps are stored upside down in the file, but they can be right way up}
    TopDown:=(BitMapInfoHeader.Height < 0);
    BitMapInfoHeader.Height:=Abs(BitMapInfoHeader.Height);
   
    {Check how many bits per pixel in this Bitmap, we only support 16, 24 and 32 in this function}
    if BitMapInfoHeader.BitCount = 16 then
     begin
      {Check the compression format used, this function only supports raw RGB files so far}
      if BitMapInfoHeader.Compression = BI_RGB then
       begin
        {Get the color format}
        Format:=COLOR_FORMAT_RGB15;
        {Now get the bytes per line}
        LineSize:=BitMapInfoHeader.Width * 2;
        {And also determine the actual number of bytes until the next line}
        ReadSize:=(((BitMapInfoHeader.Width * 8 * 2) + 31) div 32) shl 2;
       end
      else
       begin
        Exit;
       end;
     end
    else if BitMapInfoHeader.BitCount = 24 then
     begin
      {Check the compression}
      if BitMapInfoHeader.Compression = BI_RGB then
       begin
        {Color format, bytes per line and actual bytes as again}
        Format:=COLOR_FORMAT_RGB24;
        LineSize:=BitMapInfoHeader.Width * 3;
        ReadSize:=(((BitMapInfoHeader.Width * 8 * 3) + 31) div 32) shl 2;
       end
      else
       begin
        Exit;
       end;
     end
    else if BitMapInfoHeader.BitCount = 32 then
     begin
      {Check the compression}
      if BitMapInfoHeader.Compression = BI_RGB then
       begin
        {Color format, bytes per line and actual bytes as again}
        Format:=COLOR_FORMAT_URGB32;
        LineSize:=BitMapInfoHeader.Width * 4;
        ReadSize:=(((BitMapInfoHeader.Width * 8 * 4) + 31) div 32) shl 2;
       end
      else
       begin
        Exit;
       end;
     end
    else
     begin
      Exit;
     end;     
 
    {Get the size of the Bitmap image not including the headers, just the actual pixels}
    Size:=LineSize * BitMapInfoHeader.Height;
   
    {Allocate a buffer to hold all the pixels}
    Buffer:=GetMem(Size);
    try
     Offset:=0;
     
     {Check for a which way up}
     if TopDown then
      begin
       {Right way up is a rare case}
       for Count:=0 to BitMapInfoHeader.Height - 1 do
        begin
         {Update the position of the file stream}
         FileStream.Position:=BitMapFileHeader.bfOffset + (Count * ReadSize);
       
         {Read a full line of pixels from the file}     
         if FileStream.Read((Buffer + Offset)^,LineSize) <> LineSize then Exit;
         
         {Update the offset of our buffer}   
         Inc(Offset,LineSize);
        end;
      end
     else
      begin
       {Upside down is the normal case}
       for Count:=BitMapInfoHeader.Height - 1 downto 0 do
        begin
         {Update the position of the file stream}
         FileStream.Position:=BitMapFileHeader.bfOffset + (Count * ReadSize);
         
         {Read a full line of pixels from the file}     
         if FileStream.Read((Buffer + Offset)^,LineSize) <> LineSize then Exit;
         
         {Update the offset of our buffer}   
         Inc(Offset,LineSize);
        end;
      end;       
     
     {Draw the entire image onto our graphics console window in one request}
     if GraphicsWindowDrawImage(Handle,X,Y,Buffer,BitMapInfoHeader.Width,BitMapInfoHeader.Height,Format) <> ERROR_SUCCESS then Exit;
     
     Result:=True;
    finally
     FreeMem(Buffer);
    end;
   end;
 finally
  FileStream.Free;
 end;
end;

{==============================================================================}

function DrawBitmapFromStream(Handle:TWindowHandle;Stream:TStream;X,Y:LongWord):Boolean;
{A function for drawing a standard bitmap image onto an Ultibo graphics console window from a stream}
{Handle: The handle of an existing graphics console window}
{Stream: A stream object containing the bitmap exactly as it would appear in a file (including the header etc)}
{X: The column position for the left edge of the bitmap}
{Y: The row position for the top row of the bitmap}
{Return: True if successful or False if an error occurred}
var
 Format:LongWord;
 TopDown:Boolean;
 LineSize:LongWord;
 ReadSize:LongWord;
 
 BitMapFileHeader:TBitMapFileHeader;
 BitMapInfoHeader:TBitMapInfoHeader;
begin
 {}
 Result:=False;
 
 {This is a variation of the earlier example that showed how to load a bitmap from a file and draw it on the
  screen in Ultibo, this version takes a stream object and loads the bitmap from that instead.
 
  It also demonstrates the use of the GraphicsWindowImageFromStream() function to draw the image without
  copying to a memory buffer first}
 
 {Check the parameters}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 if Stream = nil then Exit;
 
 {Check the stream size}
 if Stream.Size < (SizeOf(TBitMapFileHeader) + SizeOf(TBitMapInfoHeader)) then Exit;
 
 {Read the Bitmap file header}
 if Stream.Read(BitMapFileHeader,SizeOf(TBitMapFileHeader)) <> SizeOf(TBitMapFileHeader) then Exit;
 
 {Check the magic number in the header}
 if BitMapFileHeader.bfType = BMmagic then
  begin
   {Read the Bitmap info header}
   if Stream.Read(BitMapInfoHeader,SizeOf(TBitMapInfoHeader)) <> SizeOf(TBitMapInfoHeader) then Exit;
 
   {Most Bitmaps are stored upside down, but they can be right way up}
   TopDown:=(BitMapInfoHeader.Height < 0);
   BitMapInfoHeader.Height:=Abs(BitMapInfoHeader.Height);
 
   {Check how many bits per pixel in this Bitmap, we only support 16, 24 and 32 in this function}
   if BitMapInfoHeader.BitCount = 16 then
    begin
     {Check the compression format used, this function only supports raw RGB files so far}
     if BitMapInfoHeader.Compression = BI_RGB then
      begin
       {Get the color format}
       Format:=COLOR_FORMAT_RGB15;
       {Now get the bytes per line}
       LineSize:=BitMapInfoHeader.Width * 2;
       {And also determine the actual number of bytes until the next line}
       ReadSize:=(((BitMapInfoHeader.Width * 8 * 2) + 31) div 32) shl 2;
      end
     else
      begin
       Exit;
      end;
    end
   else if BitMapInfoHeader.BitCount = 24 then
    begin
     {Check the compression}
     if BitMapInfoHeader.Compression = BI_RGB then
      begin
       {Color format, bytes per line and actual bytes as again}
       Format:=COLOR_FORMAT_RGB24;
       LineSize:=BitMapInfoHeader.Width * 3;
       ReadSize:=(((BitMapInfoHeader.Width * 8 * 3) + 31) div 32) shl 2;
      end
     else
      begin
       Exit;
      end;
    end
   else if BitMapInfoHeader.BitCount = 32 then
    begin
     {Check the compression}
     if BitMapInfoHeader.Compression = BI_RGB then
      begin
       {Color format, bytes per line and actual bytes as again}
       Format:=COLOR_FORMAT_URGB32;
       LineSize:=BitMapInfoHeader.Width * 4;
       ReadSize:=(((BitMapInfoHeader.Width * 8 * 4) + 31) div 32) shl 2;
      end
     else
      begin
       Exit;
      end;
    end
   else
    begin
     Exit;
    end;     
 
   {Update the position of the stream}
   Stream.Position:=BitMapFileHeader.bfOffset;
   
   {Draw the stream onto our graphics console window}
   if GraphicsWindowImageFromStream(Handle,X,Y,Stream,BitMapInfoHeader.Width,BitMapInfoHeader.Height,Format,ReadSize - LineSize,not(TopDown)) <> ERROR_SUCCESS then Exit;
   
   Result:=True;
  end;
end;

{==============================================================================}

function DrawBitmapFromBuffer(Handle:TWindowHandle;const Buf;Len,X,Y:LongWord):Boolean;
{A function for drawing a standard bitmap image onto an Ultibo graphics console window from a memory buffer}
{Handle: The handle of an existing graphics console window}
{Buf: A memory buffer containing the bitmap exactly as it would appear in a file (including the header etc)}
{X: The column position for the left edge of the bitmap}
{Y: The row position for the top row of the bitmap}
{Return: True if successful or False if an error occurred}
var
 Size:LongWord;
 Count:LongWord;
 Offset:LongWord;
 Format:LongWord;
 Buffer:Pointer;
 TopDown:Boolean;
 LineSize:LongWord;
 ReadSize:LongWord;
 
 BitMapFileHeader:PBitMapFileHeader;
 BitMapInfoHeader:PBitMapInfoHeader;
begin
 {}
 Result:=False;
 
 {This is a variation of the earlier example that showed how to load a bitmap from a file and draw it on the
  screen in Ultibo, this version takes a memory buffer and a length and loads the bitmap from that instead.
 
  It still creates a memory buffer and puts the pixels onto the screen using GraphicsWindowDrawImage() so
  you could make this more efficient by not copying the image to a buffer first}
 
 {Check the parameters}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Check the length}
 if Len < (SizeOf(TBitMapFileHeader) + SizeOf(TBitMapInfoHeader)) then Exit;
 
 {Get the Bitmap file header}
 BitMapFileHeader:=PBitMapFileHeader(@Buf);
 
 {Check the magic number in the header}
 if BitMapFileHeader.bfType = BMmagic then
  begin
   {Get the Bitmap info header}
   BitMapInfoHeader:=PBitMapInfoHeader(@Buf + SizeOf(TBitMapFileHeader));
 
   {Most Bitmaps are stored upside down, but they can be right way up}
   TopDown:=(BitMapInfoHeader.Height < 0);
   BitMapInfoHeader.Height:=Abs(BitMapInfoHeader.Height);
 
   {Check how many bits per pixel in this Bitmap, we only support 16, 24 and 32 in this function}
   if BitMapInfoHeader.BitCount = 16 then
    begin
     {Check the compression format used, this function only supports raw RGB bitmaps so far}
     if BitMapInfoHeader.Compression = BI_RGB then
      begin
       {Get the color format}
       Format:=COLOR_FORMAT_RGB15;
       {Now get the bytes per line}
       LineSize:=BitMapInfoHeader.Width * 2;
       {And also determine the actual number of bytes until the next line}
       ReadSize:=(((BitMapInfoHeader.Width * 8 * 2) + 31) div 32) shl 2;
      end
     else
      begin
       Exit;
      end;
    end
   else if BitMapInfoHeader.BitCount = 24 then
    begin
     {Check the compression}
     if BitMapInfoHeader.Compression = BI_RGB then
      begin
       {Color format, bytes per line and actual bytes as again}
       Format:=COLOR_FORMAT_RGB24;
       LineSize:=BitMapInfoHeader.Width * 3;
       ReadSize:=(((BitMapInfoHeader.Width * 8 * 3) + 31) div 32) shl 2;
      end
     else
      begin
       Exit;
      end;
    end
   else if BitMapInfoHeader.BitCount = 32 then
    begin
     {Check the compression}
     if BitMapInfoHeader.Compression = BI_RGB then
      begin
       {Color format, bytes per line and actual bytes as again}
       Format:=COLOR_FORMAT_URGB32;
       LineSize:=BitMapInfoHeader.Width * 4;
       ReadSize:=(((BitMapInfoHeader.Width * 8 * 4) + 31) div 32) shl 2;
      end
     else
      begin
       Exit;
      end;
    end
   else
    begin
     Exit;
    end;     
 
   {Get the size of the Bitmap image not including the headers, just the actual pixels}
   Size:=LineSize * BitMapInfoHeader.Height;
 
   {Allocate a buffer to hold all the pixels}
   Buffer:=GetMem(Size);
   try
    Offset:=0;
   
    {Check for a which way up}
    if TopDown then
     begin
      {Right way up is a rare case}
      for Count:=0 to BitMapInfoHeader.Height - 1 do
       begin
        {Read a full line of pixels from the buffer}
        System.Move((@Buf + BitMapFileHeader.bfOffset + (Count * ReadSize))^, (Buffer + Offset)^,LineSize);
       
        {Update the offset of our buffer}   
        Inc(Offset,LineSize);
       end;
     end
    else
     begin
      {Upside down is the normal case}
      for Count:=BitMapInfoHeader.Height - 1 downto 0 do
       begin
        {Read a full line of pixels from the buffer}     
        System.Move((@Buf + BitMapFileHeader.bfOffset + (Count * ReadSize))^, (Buffer + Offset)^,LineSize);
       
        {Update the offset of our buffer}   
        Inc(Offset,LineSize);
       end;
     end;       
   
    {Draw the entire image onto our graphics console window in one request}
    if GraphicsWindowDrawImage(Handle,X,Y,Buffer,BitMapInfoHeader.Width,BitMapInfoHeader.Height,Format) <> ERROR_SUCCESS then Exit;
   
    Result:=True;
   finally
    FreeMem(Buffer);
   end;
  end;
end;

{==============================================================================}

function SaveBitmap(Handle:TWindowHandle;const Filename:String;X,Y,Width,Height,BPP:LongWord):Boolean;
{A function for saving all or part of an Ultibo graphics console window to a standard bitmap file}
{Handle: The handle of an existing graphics console window}
{Filename: The name of the file to save the bitmap to}
{X: The column position for the left edge of the bitmap}
{Y: The row position for the top row of the bitmap}
{Width: The width (in pixels) of the bitmap}
{Height: The height (in pixels) of the bitmap}
{BPP: The bits per pixel value for the bitmap (eg 16, 24 or 32)}
{Return: True if successful or False if an error occurred}
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
  {Saving all or part of the screen to a bitmap file can be done very simply using a number of different methods.
   Here we get the image to be saved from the screen in a single to call GraphicsWindowGetImage() which copies the
   image to a memory buffer we provide. From there we use a TMemoryStream class to write each row of pixels in the
   image into the correct format for storing in a BMP file and finally the memory stream is saved to a file using the
   SaveToFile() method}
 
  {Check the parameters}
  if Handle = INVALID_HANDLE_VALUE then Exit;
  if Length(Filename) = 0 then Exit;
  if (Width = 0) or (Height = 0) then Exit;
 
  {Check the BPP (Bits Per Pixel) value. It must be 16, 24 or 32 for this function}
  if BPP = 16 then
   begin
    {Get the color format}
    Format:=COLOR_FORMAT_RGB15;
    {Work ou the number ofbytes per line}
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
    if GraphicsWindowGetImage(Handle,X,Y,Buffer,BitMapInfoHeader.Width,BitMapInfoHeader.Height,Format) <> ERROR_SUCCESS then Exit;
   
    {Go through each row in the image starting at the bottom because bitmaps are normally upside down}
    for Count:=BitMapInfoHeader.Height - 1 downto 0 do
     begin
      {Update the position of the memory stream for the next row}
      MemoryStream.Position:=BitMapFileHeader.bfOffset + (Count * ReadSize);
     
      {Write a full line of pixels to the memory stream from our buffer}     
      if MemoryStream.Write((Buffer + Offset)^,LineSize) <> LineSize then Exit;
         
      {Update the offet of our buffer}   
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

function LoadBitmapToStream(const Filename:String;Stream:TStream;out Width,Height,Format:LongWord):Boolean;
{A function to extract a standard bitmap image into a stream object as a raw image and return the size and format}
{Filename: The name of the file to load the bitmap from}
{Stream: A stream object to receive the extracted image}
{Width: The width in pixels of the extracted image}
{Height: The height in pixels of the extracted image}
{Format: The color format of the extracted image (eg COLOR_FORMAT_RGB24)}
{Return: True if successful or False if an error occurred}
var
 Size:LongWord;
 Count:LongWord;
 Buffer:Pointer;
 TopDown:Boolean;
 LineSize:LongWord;
 ReadSize:LongWord;
 FileStream:TFileStream;
 
 BitMapFileHeader:TBitMapFileHeader;
 BitMapInfoHeader:TBitMapInfoHeader;
begin
 {}
 Result:=False;

 {This function is different from the DrawBitmap versions because it will not draw anything on screen.
  Instead the bitmap will be extracted into a raw image and written to the supplied stream, this could
  be useful for passing to image conversion functions in order to translate an image to another format}
 
 {Check the parameters}
 if Stream = nil then Exit;
 if Length(Filename) = 0 then Exit;
 
 {Check if the file exists}
 if not FileExists(Filename) then Exit;
  
 {Open the file using a TFileStream class}
 FileStream:=TFileStream.Create(Filename,fmOpenRead or fmShareDenyNone);
 try
  {Check the file size}
  if FileStream.Size < (SizeOf(TBitMapFileHeader) + SizeOf(TBitMapInfoHeader)) then Exit;
 
  {Read the Bitmap file header}
  if FileStream.Read(BitMapFileHeader,SizeOf(TBitMapFileHeader)) <> SizeOf(TBitMapFileHeader) then Exit;
 
  {Check the magic number in the header}
  if BitMapFileHeader.bfType = BMmagic then
   begin
    {Read the Bitmap info header}
    if FileStream.Read(BitMapInfoHeader,SizeOf(TBitMapInfoHeader)) <> SizeOf(TBitMapInfoHeader) then Exit;
  
    {Most Bitmaps are stored upside down in the file, but they can be right way up}
    TopDown:=(BitMapInfoHeader.Height < 0);
    BitMapInfoHeader.Height:=Abs(BitMapInfoHeader.Height);
  
    {Check how many bits per pixel in this Bitmap, we only support 16, 24 and 32 in this function}
    if BitMapInfoHeader.BitCount = 16 then
     begin
      {Check the compression format used, this function only supports raw RGB files so far}
      if BitMapInfoHeader.Compression = BI_RGB then
       begin
        {Get the color format}
        Format:=COLOR_FORMAT_RGB15;
        {Now get the bytes per line}
        LineSize:=BitMapInfoHeader.Width * 2;
        {And also determine the actual number of bytes until the next line}
        ReadSize:=(((BitMapInfoHeader.Width * 8 * 2) + 31) div 32) shl 2;
       end
      else
       begin
        Exit;
       end;
     end
    else if BitMapInfoHeader.BitCount = 24 then
     begin
      {Check the compression}
      if BitMapInfoHeader.Compression = BI_RGB then
       begin
        {Color format, bytes per line and actual bytes as again}
        Format:=COLOR_FORMAT_RGB24;
        LineSize:=BitMapInfoHeader.Width * 3;
        ReadSize:=(((BitMapInfoHeader.Width * 8 * 3) + 31) div 32) shl 2;
       end
      else
       begin
        Exit;
       end;
     end
    else if BitMapInfoHeader.BitCount = 32 then
     begin
      {Check the compression}
      if BitMapInfoHeader.Compression = BI_RGB then
       begin
        {Color format, bytes per line and actual bytes as again}
        Format:=COLOR_FORMAT_URGB32;
        LineSize:=BitMapInfoHeader.Width * 4;
        ReadSize:=(((BitMapInfoHeader.Width * 8 * 4) + 31) div 32) shl 2;
       end
      else
       begin
        Exit;
       end;
     end
    else
     begin
      Exit;
     end;     
  
    {Get the size of the Bitmap image not including the headers, just the actual pixels}
    Size:=LineSize * BitMapInfoHeader.Height;
    
    {Resize the stream to accomodate the bitmap}
    Stream.Size:=Size;
 
    {Allocate a buffer to hold one line of pixels}
    Buffer:=GetMem(LineSize);
    try
     {Check for a which way up}
     if TopDown then
      begin
       {Right way up is a rare case}
       for Count:=0 to BitMapInfoHeader.Height - 1 do
        begin
         {Update the position of the file stream}
         FileStream.Position:=BitMapFileHeader.bfOffset + (Count * ReadSize);
       
         {Read a full line of pixels from the file}     
         if FileStream.Read(Buffer^,LineSize) <> LineSize then Exit;
         
         {Write a full line of pixels to the stream}
         if Stream.Write(Buffer^,LineSize) <> LineSize then Exit;
        end;
      end
     else
      begin
       {Upside down is the normal case}
       for Count:=BitMapInfoHeader.Height - 1 downto 0 do
        begin
         {Update the position of the file stream}
         FileStream.Position:=BitMapFileHeader.bfOffset + (Count * ReadSize);
         
         {Read a full line of pixels from the file}     
         if FileStream.Read(Buffer^,LineSize) <> LineSize then Exit;
         
         {Write a full line of pixels to the stream}
         if Stream.Write(Buffer^,LineSize) <> LineSize then Exit;
        end;
      end;       
      
     Result:=True;
    finally
     FreeMem(Buffer);
    end;
   end;
 finally
  FileStream.Free;
 end;
end;

{==============================================================================}

function LoadBitmapToBuffer(const Filename:String;var Buf;var Len:LongWord;out Width,Height,Format:LongWord):Boolean;
{A function to extract a standard bitmap image into a memory buffer as a raw image and return the size and format}
{Filename: The name of the file to load the bitmap from}
{Buf: A memory buffer to receive the extracted image (Must be large enough to contain the extracted image)}
{Len: The size in bytes of the extracted image (Pass 0 to find out how many bytes are required)}
{Width: The width in pixels of the extracted image}
{Height: The height in pixels of the extracted image}
{Format: The color format of the extracted image (eg COLOR_FORMAT_RGB24)}
{Return: True if successful or False if an error occurred}
var
 Size:LongWord;
 Count:LongWord;
 Offset:LongWord;
 Buffer:Pointer;
 TopDown:Boolean;
 LineSize:LongWord;
 ReadSize:LongWord;
 FileStream:TFileStream;
 
 BitMapFileHeader:TBitMapFileHeader;
 BitMapInfoHeader:TBitMapInfoHeader;
begin
 {}
 Result:=False;

 {This function is different from the DrawBitmap versions because it will not draw anything on screen.
  Instead the bitmap will be extracted into a raw image and written to the supplied buffer, this is
  useful for passing to other functions that expect a raw image buffer rather than an image encapsulated
  in a particular format with a header etc.

  In order to determine the size of the buffer you need to allocate you should call this function twice,
  the first time simply pass 0 for Len parameter (and a dummy variable for Buf), the function will
  return False but the Len parameter will be updated to the number of bytes needed to contain the 
  extracted image.
  
  If the function returns False and the Len parameter is still 0 then the bitmap could not be read or
  some other error occurred}
  
 {Check the parameters}
 if Length(Filename) = 0 then Exit;
  
 {Check if the file exists}
 if not FileExists(Filename) then Exit;
  
 {Open the file using a TFileStream class}
 FileStream:=TFileStream.Create(Filename,fmOpenRead or fmShareDenyNone);
 try
  {Check the file size}
  if FileStream.Size < (SizeOf(TBitMapFileHeader) + SizeOf(TBitMapInfoHeader)) then Exit;
 
  {Read the Bitmap file header}
  if FileStream.Read(BitMapFileHeader,SizeOf(TBitMapFileHeader)) <> SizeOf(TBitMapFileHeader) then Exit;
 
  {Check the magic number in the header}
  if BitMapFileHeader.bfType = BMmagic then
   begin
    {Read the Bitmap info header}
    if FileStream.Read(BitMapInfoHeader,SizeOf(TBitMapInfoHeader)) <> SizeOf(TBitMapInfoHeader) then Exit;
  
    {Most Bitmaps are stored upside down in the file, but they can be right way up}
    TopDown:=(BitMapInfoHeader.Height < 0);
    BitMapInfoHeader.Height:=Abs(BitMapInfoHeader.Height);
  
    {Check how many bits per pixel in this Bitmap, we only support 16, 24 and 32 in this function}
    if BitMapInfoHeader.BitCount = 16 then
     begin
      {Check the compression format used, this function only supports raw RGB files so far}
      if BitMapInfoHeader.Compression = BI_RGB then
       begin
        {Get the color format}
        Format:=COLOR_FORMAT_RGB15;
        {Now get the bytes per line}
        LineSize:=BitMapInfoHeader.Width * 2;
        {And also determine the actual number of bytes until the next line}
        ReadSize:=(((BitMapInfoHeader.Width * 8 * 2) + 31) div 32) shl 2;
       end
      else
       begin
        Exit;
       end;
     end
    else if BitMapInfoHeader.BitCount = 24 then
     begin
      {Check the compression}
      if BitMapInfoHeader.Compression = BI_RGB then
       begin
        {Color format, bytes per line and actual bytes as again}
        Format:=COLOR_FORMAT_RGB24;
        LineSize:=BitMapInfoHeader.Width * 3;
        ReadSize:=(((BitMapInfoHeader.Width * 8 * 3) + 31) div 32) shl 2;
       end
      else
       begin
        Exit;
       end;
     end
    else if BitMapInfoHeader.BitCount = 32 then
     begin
      {Check the compression}
      if BitMapInfoHeader.Compression = BI_RGB then
       begin
        {Color format, bytes per line and actual bytes as again}
        Format:=COLOR_FORMAT_URGB32;
        LineSize:=BitMapInfoHeader.Width * 4;
        ReadSize:=(((BitMapInfoHeader.Width * 8 * 4) + 31) div 32) shl 2;
       end
      else
       begin
        Exit;
       end;
     end
    else
     begin
      Exit;
     end;     
  
    {Get the size of the Bitmap image not including the headers, just the actual pixels}
    Size:=LineSize * BitMapInfoHeader.Height;
    
    {Check the size against the supplied buffer length}
    if Len < Size then
     begin
      {Return the required length}
      Len:=Size;
      Exit;
     end;
    
    {Get the buffer to hold all the pixels}
    Buffer:=@Buf;
    Offset:=0;
    
    {Check for a which way up}
    if TopDown then
     begin
      {Right way up is a rare case}
      for Count:=0 to BitMapInfoHeader.Height - 1 do
       begin
        {Update the position of the file stream}
        FileStream.Position:=BitMapFileHeader.bfOffset + (Count * ReadSize);
      
        {Read a full line of pixels from the file}     
        if FileStream.Read((Buffer + Offset)^,LineSize) <> LineSize then Exit;
        
        {Update the offset of our buffer}   
        Inc(Offset,LineSize);
       end;
     end
    else
     begin
      {Upside down is the normal case}
      for Count:=BitMapInfoHeader.Height - 1 downto 0 do
       begin
        {Update the position of the file stream}
        FileStream.Position:=BitMapFileHeader.bfOffset + (Count * ReadSize);
        
        {Read a full line of pixels from the file}     
        if FileStream.Read((Buffer + Offset)^,LineSize) <> LineSize then Exit;
        
        {Update the offset of our buffer}   
        Inc(Offset,LineSize);
       end;
     end;       
    
    {Return width and height}
    Width:=BitMapInfoHeader.Width;
    Height:=BitMapInfoHeader.Height;
    
    {Return the actual length}     
    Len:=Size;
    
    Result:=True;
   end;
 finally
  FileStream.Free;
 end;
end;

{==============================================================================}
{==============================================================================}

end.
