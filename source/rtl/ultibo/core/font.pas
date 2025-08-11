{
Ultibo Font interface unit.

Copyright (C) 2023 - SoftOz Pty Ltd.

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

  Linux - KBD – Linux keyboard tools - http://kbd-project.org/

References
==========

  PC Screen Fonts (PSF) - https://en.wikipedia.org/wiki/PC_Screen_Font

  A number of PSF format fonts in various stlyes and sizes are available from:

   http://v3.sk/~lkundrak/fonts/kbd/

  Note that this site also lists a number of other fonts in raw format (no header)
  which contain the font character data in the same format as the PSF files but
  cannot currently be loaded by this unit.

   http://v3.sk/~lkundrak/fonts/

Fonts
=====

 The fonts supported by Ultibo are a bitmap format that contains a block of data
 where each character is represented by a number of consecutive bytes.

 Fonts can either be statically compiled as a pascal unit and loaded during startup
 or can be dynamically loaded by passing a header and data block to the FontLoad()
 function. A Font Builder tool is available to convert common bitmap font formats
 into a pascal unit for compiling with a project.

 For an 8x16 (8 pixels wide and 16 pixels high) font the data contains 8 bits (1 byte)
 for each of the 16 rows that make up a character and each character would be
 16 bytes long.

 For a 12x22 font the data contains 12 bits padded to 16 bits (2 bytes) for each of
 the 22 rows that make up a character. Therefore each character would be 44 bytes
 in length.

 This unit can support any size font from 8x6 to 32x64 including every combination
 in between.

 For fonts where the bits per row is greater than one byte both little endian and big
 endian format is supported.

 Allowance has been made for including a Unicode translation table with each font so
 that writing of Unicode text to the console can be supported as well as an extended
 bitmap format where character data includes alpha or color information or both. These
 features are yet to be fully implemented.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

{$IFNDEF FPC_DOTTEDUNITS}
unit Font;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Core.GlobalConfig,
  Core.GlobalConst,
  Core.GlobalTypes,
  Core.Platform,
  Core.Threads,
  System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  SysUtils;
{$ENDIF FPC_DOTTEDUNITS}

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {Font specific constants}
 FONT_MIN_COUNT = 256;

 FONT_MIN_WIDTH = 8;
 FONT_MAX_WIDTH = 32;

 FONT_MIN_HEIGHT = 6;
 FONT_MAX_HEIGHT = 64;

 {Font Signature}
 FONT_SIGNATURE = $77DE1BBC;

 {Font name constants}
 FONT_NAME_LENGTH = SIZE_64;   {Length of font name}
 FONT_DESC_LENGTH = SIZE_128;  {Length of font description}

 {Font Mode constants}
 FONT_MODE_NONE   = 0;
 FONT_MODE_PIXEL  = 1; {A font with 1 bit per pixel in the character data}
 FONT_MODE_ALPHA8 = 2; {A font with 8 bits of alpha blending per pixel in the character data}
 FONT_MODE_RGBA32 = 3; {A font with 32 bits RGBA per pixel in the character data}

 {Font Flag constants}
 FONT_FLAG_NONE       = $00000000;
 FONT_FLAG_UNICODE    = $00000001; {Font includes a unicode translation table}
 FONT_FLAG_CODEPAGE   = $00000002; {Font has a specified codepage}
 FONT_FLAG_BIGENDIAN  = $00000004; {Font characters are in big endian order (Only applies to characters larger than one byte)}
 FONT_FLAG_RIGHTALIGN = $00000008; {Font characters are right aligned, no need to shift during load}
 FONT_FLAG_VARIABLE   = $00000010; {Font characters are variable width rather than fixed}

{==============================================================================}
//const
 {PSF Font specific constants}

{==============================================================================}
type
 {Font specific types}

 {Font Header}
 PFontHeader = ^TFontHeader;
 TFontHeader = record
  Width:LongWord;          {Width of each character in pixels}
  Height:LongWord;         {Height of each character in pixels}
  Count:LongWord;          {Number of character glyphs in data}
  Mode:LongWord;           {Font mode (eg FONT_MODE_PIXEL)}
  Flags:LongWord;          {Font flags (eg FONT_FLAG_UNICODE)}
  Mask:LongWord;           {Transparency mask for a bitmap font}
  CodePage:LongWord;       {Font codepage (CP_ACP if not specified)}
  Name:String[63];         {Font name}
  Description:String[127]; {Font description}
 end;

 {Font Data}
 PFontData = ^TFontData;
 TFontData = record
  Data:array[0..0] of Byte;
 end;

 {Font Data (8 bit width) (Byte)}
 PFontData8x6 = ^TFontData8x6;
 TFontData8x6 = record Data:array[0..255,0..5] of Byte; end;

 PFontData8x7 = ^TFontData8x7;
 TFontData8x7 = record Data:array[0..255,0..6] of Byte; end;

 PFontData8x8 = ^TFontData8x8;
 TFontData8x8 = record Data:array[0..255,0..7] of Byte; end;

 PFontData8x9 = ^TFontData8x9;
 TFontData8x9 = record Data:array[0..255,0..8] of Byte; end;

 PFontData8x10 = ^TFontData8x10;
 TFontData8x10 = record Data:array[0..255,0..9] of Byte; end;

 PFontData8x11 = ^TFontData8x11;
 TFontData8x11 = record Data:array[0..255,0..10] of Byte; end;

 PFontData8x12 = ^TFontData8x12;
 TFontData8x12 = record Data:array[0..255,0..11] of Byte; end;

 PFontData8x13 = ^TFontData8x13;
 TFontData8x13 = record Data:array[0..255,0..12] of Byte; end;

 PFontData8x14 = ^TFontData8x14;
 TFontData8x14 = record Data:array[0..255,0..13] of Byte; end;

 PFontData8x15 = ^TFontData8x15;
 TFontData8x15 = record Data:array[0..255,0..14] of Byte; end;

 PFontData8x16 = ^TFontData8x16;
 TFontData8x16 = record Data:array[0..255,0..15] of Byte; end;

 {Font Data (12 bit width) (Word)}
 PFontData12x12 = ^TFontData12x12;
 TFontData12x12 = record Data:array[0..255,0..11] of Word; end;

 PFontData12x14 = ^TFontData12x14;
 TFontData12x14 = record Data:array[0..255,0..13] of Word; end;

 PFontData12x16 = ^TFontData12x16;
 TFontData12x16 = record Data:array[0..255,0..15] of Word; end;

 PFontData12x18 = ^TFontData12x18;
 TFontData12x18 = record Data:array[0..255,0..17] of Word; end;

 PFontData12x20 = ^TFontData12x20;
 TFontData12x20 = record Data:array[0..255,0..19] of Word; end;

 PFontData12x22 = ^TFontData12x22;
 TFontData12x22 = record Data:array[0..255,0..21] of Word; end;

 {Font Data (16 bit width) (Word)}
 PFontData16x16 = ^TFontData16x16;
 TFontData16x16 = record Data:array[0..255,0..15] of Word; end;

 PFontData16x24 = ^TFontData16x24;
 TFontData16x24 = record Data:array[0..255,0..23] of Word; end;

 PFontData16x32 = ^TFontData16x32;
 TFontData16x32 = record Data:array[0..255,0..31] of Word; end;

 {Font Data (32 bit width) (LongWord)}
 PFontData32x32 = ^TFontData32x32;
 TFontData32x32 = record Data:array[0..255,0..31] of LongWord; end;

 PFontData32x48 = ^TFontData32x48;
 TFontData32x48 = record Data:array[0..255,0..47] of LongWord; end;

 PFontData32x64 = ^TFontData32x64;
 TFontData32x64 = record Data:array[0..255,0..63] of LongWord; end;

 {Font Chars}
 PFontChars8 = ^TFontChars8;
 TFontChars8 = array[0..0] of Byte;

 PFontChars16 = ^TFontChars16;
 TFontChars16 = array[0..0] of Word;

 PFontChars32 = ^TFontChars32;
 TFontChars32 = array[0..0] of LongWord;

 {Font Unicode}
 PFontUnicode = ^TFontUnicode;
 TFontUnicode = record
  //To Do
 end;

 {Font Properties}
 PFontProperties = ^TFontProperties;
 TFontProperties = record
  FontMode:LongWord;       {Font mode (eg FONT_MODE_PIXEL)}
  FontFlags:LongWord;      {Font flags (eg FONT_FLAG_UNICODE)}
  FontName:array[0..FONT_NAME_LENGTH - 1] of Char; {Font name}
  FontDescription:array[0..FONT_DESC_LENGTH - 1] of Char; {Font description}
  CharWidth:LongWord;      {Font character width in pixels}
  CharHeight:LongWord;     {Font character height in pixels}
  CharCount:LongWord;      {Number of glyphs in font character table}
  CharMask:LongWord;       {Transparency mask for a bitmap font (Not used for a pixel font)}
  CodePage:LongWord;       {Font codepage (CP_ACP if not specified)}
 end;

 PFontEntry = ^TFontEntry;

 {Font Enumeration Callback}
 TFontEnumerate = function(Handle:TFontHandle;Data:Pointer):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 {Font Entry}
 TFontEntry = record
  {Font Properties}
  Signature:LongWord;            {Signature for entry validation}
  FontMode:LongWord;             {Font mode (eg FONT_MODE_PIXEL)}
  FontFlags:LongWord;            {Font flags (eg FONT_FLAG_UNICODE)}
  FontName:array[0..FONT_NAME_LENGTH - 1] of Char; {Font name}
  FontDescription:array[0..FONT_DESC_LENGTH - 1] of Char; {Font description}
  {Driver Properties}
  CharWidth:LongWord;            {Font character width in pixels}
  CharHeight:LongWord;           {Font character height in pixels}
  CharCount:LongWord;            {Number of glyphs in font character table}
  CharMask:LongWord;             {Transparency mask for a bitmap font (Not used for a pixel font)}
  CodePage:LongWord;             {Font codepage (CP_ACP if not specified)}
  CharData:Pointer;              {Font character pixel or bitmap data}
  UnicodeData:PFontUnicode;      {Font unicode translation data (Only if FONT_FLAG_UNICODE)}
  {Internal Properties}
  Prev:PFontEntry;               {Previous entry in Font table}
  Next:PFontEntry;               {Next entry in Font table}
 end;

{==============================================================================}
//type
 {PSF Font specific types}

{==============================================================================}
{var}
 {Font specific variables}

{==============================================================================}
{Initialization Functions}
procedure FontInit;

{==============================================================================}
{Font Functions}
function FontLoad(Header:PFontHeader;Data:PFontData;Size:LongWord):TFontHandle;
function FontLoadEx(Header:PFontHeader;Data:PFontData;Unicode:PFontUnicode;Size:LongWord;Properties:PFontProperties):TFontHandle;
function FontUnload(Handle:TFontHandle):LongWord;

function FontGetName(Handle:TFontHandle):String;
function FontGetDescription(Handle:TFontHandle):String;

function FontGetWidth(Handle:TFontHandle):LongWord;
function FontGetHeight(Handle:TFontHandle):LongWord;

function FontGetProperties(Handle:TFontHandle;Properties:PFontProperties):LongWord;

function FontCharWidth(Handle:TFontHandle;Character:Word):LongWord;
function FontCharHeight(Handle:TFontHandle;Character:Word):LongWord;

function FontTextWidth(Handle:TFontHandle;const Text:String):LongWord;
function FontTextHeight(Handle:TFontHandle;const Text:String):LongWord;

function FontFindByName(const Name:String):TFontHandle;
function FontFindByDescription(const Description:String):TFontHandle;
function FontEnumerate(Callback:TFontEnumerate;Data:Pointer):LongWord;

{==============================================================================}
{PSF Font Functions}
function PSFFontLoad(const FileName:String):TFontHandle;
function PSFFontLoadEx(Data:Pointer;Size:LongWord):TFontHandle;

{==============================================================================}
{Font Helper Functions}
function FontGetCount:LongWord;
function FontGetDefault:TFontHandle;
function FontSetDefault(Handle:TFontHandle):LongWord;

function FontCheck(Font:PFontEntry):PFontEntry;

{==============================================================================}
{PSF Font Helper Functions}

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Font specific variables}
 FontInitialized:Boolean;

 FontTable:PFontEntry;
 FontTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 FontTableCount:LongWord;

 FontDefault:TFontHandle = INVALID_HANDLE_VALUE;

var
 {Default Font - Latin-1 (8x16) Console Font (ISO-8859-1)}
 FONT_LATIN1_8x16_HEADER:TFontHeader = (
  Width:8;
  Height:16;
  Count:256;
  Mode:FONT_MODE_PIXEL;
  Flags:FONT_FLAG_NONE;
  Mask:0;
  CodePage:CP_ACP;
  Name:('Latin1-8x16');
  Description:('Latin-1 (8x16) Console Font')
  );

 FONT_LATIN1_8X16_DATA:TFontData8x16 = (
  Data:(($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $18, $3C, $3C, $3C, $18, $18, $18, $00, $18, $18, $00, $00, $00, $00),
        ($00, $66, $66, $66, $24, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $6C, $6C, $FE, $6C, $6C, $6C, $FE, $6C, $6C, $00, $00, $00, $00),
        ($00, $10, $10, $7C, $D6, $D0, $D0, $7C, $16, $16, $D6, $7C, $10, $10, $00, $00),
        ($00, $00, $00, $00, $C2, $C6, $0C, $18, $30, $60, $C6, $86, $00, $00, $00, $00),
        ($00, $00, $38, $6C, $6C, $38, $76, $DC, $CC, $CC, $CC, $76, $00, $00, $00, $00),
        ($00, $18, $18, $18, $30, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $0C, $18, $30, $30, $30, $30, $30, $30, $18, $0C, $00, $00, $00, $00),
        ($00, $00, $30, $18, $0C, $0C, $0C, $0C, $0C, $0C, $18, $30, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $66, $3C, $FF, $3C, $66, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $18, $18, $7E, $18, $18, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $18, $18, $30, $00, $00, $00),
        ($00, $00, $00, $00, $00, $00, $00, $FE, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $18, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $06, $0C, $18, $30, $60, $C0, $00, $00, $00, $00, $00),
        ($00, $00, $7C, $C6, $CE, $CE, $D6, $D6, $E6, $E6, $C6, $7C, $00, $00, $00, $00),
        ($00, $00, $18, $38, $78, $18, $18, $18, $18, $18, $18, $7E, $00, $00, $00, $00),
        ($00, $00, $7C, $C6, $06, $0C, $18, $30, $60, $C0, $C6, $FE, $00, $00, $00, $00),
        ($00, $00, $7C, $C6, $06, $06, $3C, $06, $06, $06, $C6, $7C, $00, $00, $00, $00),
        ($00, $00, $0C, $1C, $3C, $6C, $CC, $FE, $0C, $0C, $0C, $1E, $00, $00, $00, $00),
        ($00, $00, $FE, $C0, $C0, $C0, $FC, $06, $06, $06, $C6, $7C, $00, $00, $00, $00),
        ($00, $00, $38, $60, $C0, $C0, $FC, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
        ($00, $00, $FE, $C6, $06, $06, $0C, $18, $30, $30, $30, $30, $00, $00, $00, $00),
        ($00, $00, $7C, $C6, $C6, $C6, $7C, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
        ($00, $00, $7C, $C6, $C6, $C6, $7E, $06, $06, $06, $0C, $78, $00, $00, $00, $00),
        ($00, $00, $00, $00, $18, $18, $00, $00, $00, $18, $18, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $18, $18, $00, $00, $00, $18, $18, $30, $00, $00, $00, $00),
        ($00, $00, $00, $06, $0C, $18, $30, $60, $30, $18, $0C, $06, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $FE, $00, $00, $FE, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $60, $30, $18, $0C, $06, $0C, $18, $30, $60, $00, $00, $00, $00),
        ($00, $00, $7C, $C6, $C6, $0C, $18, $18, $18, $00, $18, $18, $00, $00, $00, $00),
        ($00, $00, $7C, $C6, $C6, $C6, $DE, $DE, $DE, $DC, $C0, $7C, $00, $00, $00, $00),
        ($00, $00, $10, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $00, $00, $00, $00),
        ($00, $00, $FC, $66, $66, $66, $7C, $66, $66, $66, $66, $FC, $00, $00, $00, $00),
        ($00, $00, $3C, $66, $C2, $C0, $C0, $C0, $C0, $C2, $66, $3C, $00, $00, $00, $00),
        ($00, $00, $F8, $6C, $66, $66, $66, $66, $66, $66, $6C, $F8, $00, $00, $00, $00),
        ($00, $00, $FE, $66, $62, $68, $78, $68, $60, $62, $66, $FE, $00, $00, $00, $00),
        ($00, $00, $FE, $66, $62, $68, $78, $68, $60, $60, $60, $F0, $00, $00, $00, $00),
        ($00, $00, $3C, $66, $C2, $C0, $C0, $DE, $C6, $C6, $66, $3A, $00, $00, $00, $00),
        ($00, $00, $C6, $C6, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $C6, $00, $00, $00, $00),
        ($00, $00, $3C, $18, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00),
        ($00, $00, $1E, $0C, $0C, $0C, $0C, $0C, $CC, $CC, $CC, $78, $00, $00, $00, $00),
        ($00, $00, $E6, $66, $66, $6C, $78, $78, $6C, $66, $66, $E6, $00, $00, $00, $00),
        ($00, $00, $F0, $60, $60, $60, $60, $60, $60, $62, $66, $FE, $00, $00, $00, $00),
        ($00, $00, $C6, $EE, $FE, $FE, $D6, $C6, $C6, $C6, $C6, $C6, $00, $00, $00, $00),
        ($00, $00, $C6, $E6, $F6, $FE, $DE, $CE, $C6, $C6, $C6, $C6, $00, $00, $00, $00),
        ($00, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
        ($00, $00, $FC, $66, $66, $66, $7C, $60, $60, $60, $60, $F0, $00, $00, $00, $00),
        ($00, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $D6, $DE, $7C, $0C, $0E, $00, $00),
        ($00, $00, $FC, $66, $66, $66, $7C, $6C, $66, $66, $66, $E6, $00, $00, $00, $00),
        ($00, $00, $7C, $C6, $C6, $64, $38, $0C, $06, $C6, $C6, $7C, $00, $00, $00, $00),
        ($00, $00, $7E, $7E, $5A, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00),
        ($00, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
        ($00, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $6C, $38, $10, $00, $00, $00, $00),
        ($00, $00, $C6, $C6, $C6, $C6, $D6, $D6, $D6, $FE, $EE, $6C, $00, $00, $00, $00),
        ($00, $00, $C6, $C6, $6C, $7C, $38, $38, $7C, $6C, $C6, $C6, $00, $00, $00, $00),
        ($00, $00, $66, $66, $66, $66, $3C, $18, $18, $18, $18, $3C, $00, $00, $00, $00),
        ($00, $00, $FE, $C6, $86, $0C, $18, $30, $60, $C2, $C6, $FE, $00, $00, $00, $00),
        ($00, $00, $3C, $30, $30, $30, $30, $30, $30, $30, $30, $3C, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $C0, $60, $30, $18, $0C, $06, $00, $00, $00, $00, $00),
        ($00, $00, $3C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $3C, $00, $00, $00, $00),
        ($10, $38, $6C, $C6, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FF, $00),
        ($00, $30, $30, $30, $18, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00),
        ($00, $00, $E0, $60, $60, $78, $6C, $66, $66, $66, $66, $7C, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $7C, $C6, $C0, $C0, $C0, $C6, $7C, $00, $00, $00, $00),
        ($00, $00, $1C, $0C, $0C, $3C, $6C, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $00, $00, $00, $00),
        ($00, $00, $38, $6C, $64, $60, $F0, $60, $60, $60, $60, $F0, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $76, $CC, $CC, $CC, $CC, $CC, $7C, $0C, $CC, $78, $00),
        ($00, $00, $E0, $60, $60, $6C, $76, $66, $66, $66, $66, $E6, $00, $00, $00, $00),
        ($00, $00, $18, $18, $00, $38, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00),
        ($00, $00, $06, $06, $00, $0E, $06, $06, $06, $06, $06, $06, $66, $66, $3C, $00),
        ($00, $00, $E0, $60, $60, $66, $6C, $78, $78, $6C, $66, $E6, $00, $00, $00, $00),
        ($00, $00, $38, $18, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $EC, $FE, $D6, $D6, $D6, $D6, $C6, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $DC, $66, $66, $66, $66, $66, $66, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $DC, $66, $66, $66, $66, $66, $7C, $60, $60, $F0, $00),
        ($00, $00, $00, $00, $00, $76, $CC, $CC, $CC, $CC, $CC, $7C, $0C, $0C, $1E, $00),
        ($00, $00, $00, $00, $00, $DC, $76, $66, $60, $60, $60, $F0, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $7C, $C6, $60, $38, $0C, $C6, $7C, $00, $00, $00, $00),
        ($00, $00, $10, $30, $30, $FC, $30, $30, $30, $30, $36, $1C, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $66, $66, $66, $66, $66, $3C, $18, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $C6, $C6, $D6, $D6, $D6, $FE, $6C, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $C6, $6C, $38, $38, $38, $6C, $C6, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $06, $0C, $F8, $00),
        ($00, $00, $00, $00, $00, $FE, $CC, $18, $30, $60, $C6, $FE, $00, $00, $00, $00),
        ($00, $00, $0E, $18, $18, $18, $70, $18, $18, $18, $18, $0E, $00, $00, $00, $00),
        ($00, $00, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $00, $00, $00, $00),
        ($00, $00, $70, $18, $18, $18, $0E, $18, $18, $18, $18, $70, $00, $00, $00, $00),
        ($00, $00, $76, $DC, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
        ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
        ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
        ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
        ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
        ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
        ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
        ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
        ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
        ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
        ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
        ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
        ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
        ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
        ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
        ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
        ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
        ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
        ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
        ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
        ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
        ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
        ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
        ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
        ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
        ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
        ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
        ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
        ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
        ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
        ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
        ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
        ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
        ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $82, $FE, $00, $00, $00, $00),
        ($00, $00, $00, $00, $18, $18, $00, $18, $18, $18, $3C, $3C, $3C, $18, $00, $00),
        ($00, $00, $00, $00, $10, $7C, $D6, $D0, $D0, $D0, $D6, $7C, $10, $00, $00, $00),
        ($00, $00, $38, $6C, $60, $60, $F0, $60, $60, $66, $F6, $6C, $00, $00, $00, $00),
        ($00, $00, $00, $00, $C6, $7C, $6C, $6C, $7C, $C6, $00, $00, $00, $00, $00, $00),
        ($00, $00, $66, $66, $3C, $18, $7E, $18, $7E, $18, $18, $18, $00, $00, $00, $00),
        ($00, $00, $18, $18, $18, $18, $00, $18, $18, $18, $18, $18, $00, $00, $00, $00),
        ($00, $7C, $C6, $60, $38, $6C, $C6, $C6, $6C, $38, $0C, $C6, $7C, $00, $00, $00),
        ($00, $6C, $6C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $3C, $42, $99, $A5, $A1, $A5, $99, $42, $3C, $00, $00, $00, $00, $00),
        ($00, $00, $3C, $6C, $6C, $3E, $00, $7E, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $36, $6C, $D8, $6C, $36, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $00, $FE, $06, $06, $06, $06, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $00, $00, $7E, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $3C, $42, $B9, $A5, $B9, $A5, $A5, $42, $3C, $00, $00, $00, $00, $00),
        ($FF, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $38, $6C, $6C, $38, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $18, $18, $7E, $18, $18, $00, $7E, $00, $00, $00, $00),
        ($38, $6C, $18, $30, $7C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($38, $6C, $18, $6C, $38, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $18, $30, $60, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $CC, $CC, $CC, $CC, $CC, $CC, $F6, $C0, $C0, $C0, $00),
        ($00, $00, $7F, $D6, $D6, $76, $36, $36, $36, $36, $36, $36, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $00, $18, $18, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $6C, $38, $00),
        ($30, $70, $30, $30, $78, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $38, $6C, $6C, $38, $00, $7C, $00, $00, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $D8, $6C, $36, $6C, $D8, $00, $00, $00, $00, $00, $00),
        ($60, $E0, $60, $60, $F6, $0C, $18, $30, $66, $CE, $1A, $3F, $06, $06, $00, $00),
        ($60, $E0, $60, $60, $F6, $0C, $18, $30, $6E, $DB, $06, $0C, $1F, $00, $00, $00),
        ($70, $D8, $30, $D8, $76, $0C, $18, $30, $66, $CE, $1A, $3F, $06, $06, $00, $00),
        ($00, $00, $00, $00, $30, $30, $00, $30, $30, $30, $60, $C6, $C6, $7C, $00, $00),
        ($60, $30, $00, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $00, $00, $00, $00),
        ($0C, $18, $00, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $00, $00, $00, $00),
        ($10, $38, $6C, $00, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $00, $00, $00, $00),
        ($76, $DC, $00, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $00, $00, $00, $00),
        ($00, $6C, $00, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $00, $00, $00, $00),
        ($38, $6C, $38, $00, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $00, $00, $00, $00),
        ($00, $00, $3E, $78, $D8, $D8, $FC, $D8, $D8, $D8, $D8, $DE, $00, $00, $00, $00),
        ($00, $00, $3C, $66, $C2, $C0, $C0, $C0, $C0, $C2, $66, $3C, $0C, $66, $3C, $00),
        ($60, $30, $00, $FE, $66, $60, $60, $7C, $60, $60, $66, $FE, $00, $00, $00, $00),
        ($0C, $18, $00, $FE, $66, $60, $60, $7C, $60, $60, $66, $FE, $00, $00, $00, $00),
        ($10, $38, $6C, $00, $FE, $66, $60, $7C, $60, $60, $66, $FE, $00, $00, $00, $00),
        ($00, $6C, $00, $FE, $66, $60, $60, $7C, $60, $60, $66, $FE, $00, $00, $00, $00),
        ($60, $30, $00, $3C, $18, $18, $18, $18, $18, $18, $18, $3C, $08, $00, $00, $00),
        ($06, $0C, $00, $3C, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00),
        ($18, $3C, $66, $00, $3C, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00),
        ($00, $66, $00, $3C, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00),
        ($00, $00, $F8, $6C, $66, $66, $F6, $66, $66, $66, $6C, $F8, $00, $00, $00, $00),
        ($76, $DC, $00, $C6, $E6, $F6, $FE, $DE, $CE, $C6, $C6, $C6, $00, $00, $00, $00),
        ($60, $30, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
        ($0C, $18, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
        ($10, $38, $6C, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
        ($76, $DC, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
        ($00, $6C, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $66, $3C, $18, $3C, $66, $00, $00, $00, $00, $00, $00),
        ($00, $00, $7E, $C6, $CE, $CE, $DE, $F6, $E6, $E6, $C6, $FC, $00, $00, $00, $00),
        ($60, $30, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
        ($0C, $18, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
        ($10, $38, $6C, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
        ($00, $6C, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
        ($06, $0C, $00, $66, $66, $66, $66, $3C, $18, $18, $18, $3C, $00, $00, $00, $00),
        ($00, $00, $F0, $60, $7C, $66, $66, $66, $66, $7C, $60, $F0, $00, $00, $00, $00),
        ($00, $00, $7C, $C6, $C6, $C6, $CC, $C6, $C6, $C6, $D6, $DC, $80, $00, $00, $00),
        ($00, $60, $30, $18, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00),
        ($00, $18, $30, $60, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00),
        ($00, $10, $38, $6C, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00),
        ($00, $00, $76, $DC, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00),
        ($00, $00, $00, $6C, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00),
        ($00, $38, $6C, $38, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $7E, $DB, $1B, $7F, $D8, $DB, $7E, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $7C, $C6, $C0, $C0, $C0, $C6, $7C, $18, $6C, $38, $00),
        ($00, $60, $30, $18, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $00, $00, $00, $00),
        ($00, $0C, $18, $30, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $00, $00, $00, $00),
        ($00, $10, $38, $6C, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $00, $00, $00, $00),
        ($00, $00, $00, $6C, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $00, $00, $00, $00),
        ($00, $60, $30, $18, $00, $38, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00),
        ($00, $0C, $18, $30, $00, $38, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00),
        ($00, $18, $3C, $66, $00, $38, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00),
        ($00, $00, $00, $6C, $00, $38, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00),
        ($00, $78, $30, $78, $0C, $7E, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
        ($00, $00, $76, $DC, $00, $DC, $66, $66, $66, $66, $66, $66, $00, $00, $00, $00),
        ($00, $60, $30, $18, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
        ($00, $0C, $18, $30, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
        ($00, $10, $38, $6C, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
        ($00, $00, $76, $DC, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
        ($00, $00, $00, $6C, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $18, $00, $7E, $00, $18, $00, $00, $00, $00, $00, $00),
        ($00, $00, $00, $00, $00, $7E, $CE, $DE, $FE, $F6, $E6, $FC, $00, $00, $00, $00),
        ($00, $60, $30, $18, $00, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00),
        ($00, $18, $30, $60, $00, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00),
        ($00, $30, $78, $CC, $00, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00),
        ($00, $00, $00, $CC, $00, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00),
        ($00, $0C, $18, $30, $00, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $06, $0C, $F8, $00),
        ($00, $00, $F0, $60, $60, $7C, $66, $66, $66, $66, $7C, $60, $60, $F0, $00, $00),
        ($00, $00, $00, $6C, $00, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $06, $0C, $F8, $00))
  );


{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure FontInit;
begin
 {}
 {Check Initialized}
 if FontInitialized then Exit;

 {Initialize Font Table}
 FontTable:=nil;
 FontTableLock:=CriticalSectionCreate;
 FontTableCount:=0;
 if FontTableLock = INVALID_HANDLE_VALUE then
  begin
   if PLATFORM_LOG_ENABLED then PlatformLogError('Failed to create font table lock');
  end;
 FontDefault:=INVALID_HANDLE_VALUE;

 {Load Default Font}
 FontDefault:=FontLoad(@FONT_LATIN1_8x16_HEADER,@FONT_LATIN1_8X16_DATA,SizeOf(TFontData8x16));
 if FontDefault = INVALID_HANDLE_VALUE then
  begin
   if PLATFORM_LOG_ENABLED then PlatformLogError('Failed to load default font');
  end;

 FontInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Font Functions}
function FontLoad(Header:PFontHeader;Data:PFontData;Size:LongWord):TFontHandle;
{Load a Font from a font data block and add to the Font table}
{Header: Pointer to the font header}
{Data: Pointer to the font data}
{Size: Size of the font data}
begin
 {}
 Result:=FontLoadEx(Header,Data,nil,Size,nil);
end;

{==============================================================================}

function FontLoadEx(Header:PFontHeader;Data:PFontData;Unicode:PFontUnicode;Size:LongWord;Properties:PFontProperties):TFontHandle;
{Load a Font from a font data block and add to the Font table}
{Header: Pointer to the font header}
{Data: Pointer to the font data}
{Unicode: Pointer to the unicode translation table (Optional)}
{Size: Size of the font data}
{Properties: Pointer to a font properties record to use instead of the header (Optional)}
var
 Font:PFontEntry;
 RowSize:LongWord;
 TotalSize:LongWord;

 Swap:Boolean;
 Shift:LongWord;
 Row:LongWord;
 Count:LongWord;
 Buffer8:PByte;
 Buffer16:PWord;
 Buffer32:PLongWord;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Check Data}
 if Data = nil then Exit;

 {Check Unicode}
 {if Unicode = nil then Exit;} {May be nil}

 {Check Header}
 if (Header = nil) and (Properties = nil) then Exit;

 {Check Properties}
 if Properties = nil then
  begin
   {Check Width}
   if Header.Width < FONT_MIN_WIDTH then Exit;
   if Header.Width > FONT_MAX_WIDTH then Exit;

   {Check Height}
   if Header.Height < FONT_MIN_HEIGHT then Exit;
   if Header.Height > FONT_MAX_HEIGHT then Exit;

   {Check Count}
   if Header.Count < FONT_MIN_COUNT then Exit;

   {Get Size}
   RowSize:=((Header.Width + 7) div 8);
   if RowSize = 3 then Inc(RowSize); {Account for fonts 17 to 24 bits wide}
   TotalSize:=(RowSize * Header.Height) * Header.Count;

   {Check Size}
   if Size < TotalSize then Exit;

   {Create Font}
   Font:=PFontEntry(AllocMem(SizeOf(TFontEntry)));
   if Font = nil then Exit;

   {Update Font}
   Font.Signature:=FONT_SIGNATURE;
   Font.FontMode:=Header.Mode;
   Font.FontFlags:=Header.Flags;
   Font.FontName:=Header.Name;
   Font.FontDescription:=Header.Description;
   Font.CharWidth:=Header.Width;
   Font.CharHeight:=Header.Height;
   Font.CharCount:=Header.Count;
   Font.CharMask:=Header.Mask;
   Font.CodePage:=Header.CodePage;
   Font.CharData:=nil;
   Font.UnicodeData:=nil;
  end
 else
  begin
   {Check Width}
   if Properties.CharWidth < FONT_MIN_WIDTH then Exit;
   if Properties.CharWidth > FONT_MAX_WIDTH then Exit;

   {Check Height}
   if Properties.CharHeight < FONT_MIN_HEIGHT then Exit;
   if Properties.CharHeight > FONT_MAX_HEIGHT then Exit;

   {Check Count}
   if Properties.CharCount < FONT_MIN_COUNT then Exit;

   {Get Size}
   RowSize:=((Properties.CharWidth + 7) div 8);
   if RowSize = 3 then Inc(RowSize); {Account for fonts 17 to 24 bits wide}
   TotalSize:=(RowSize * Properties.CharHeight) * Properties.CharCount;

   {Check Size}
   if Size < TotalSize then Exit;

   {Create Font}
   Font:=PFontEntry(AllocMem(SizeOf(TFontEntry)));
   if Font = nil then Exit;

   {Update Font}
   Font.Signature:=FONT_SIGNATURE;
   Font.FontMode:=Properties.FontMode;
   Font.FontFlags:=Properties.FontFlags;
   StrLCopy(Font.FontName,Properties.FontName,FONT_NAME_LENGTH - 1);
   StrLCopy(Font.FontDescription,Properties.FontDescription,FONT_DESC_LENGTH - 1);
   Font.CharWidth:=Properties.CharWidth;
   Font.CharHeight:=Properties.CharHeight;
   Font.CharCount:=Properties.CharCount;
   Font.CharMask:=Properties.CharMask;
   Font.CodePage:=Properties.CodePage;
   Font.CharData:=nil;
   Font.UnicodeData:=nil;
  end;

 {Update Font}
 Font.CharData:=GetMem(TotalSize);
 if Font.CharData = nil then
  begin
   FreeMem(Font);
   Exit;
  end;

 {Copy Data}
 System.Move(Data.Data[0],Font.CharData^,TotalSize);

 {Update Data}
 case Font.CharWidth of
  1..8:begin
    {Get Buffer}
    Buffer8:=Font.CharData;

    {No Swap}

    {Check Shift}
    Shift:=0;
    if (Font.FontFlags and FONT_FLAG_RIGHTALIGN) = 0 then
     begin
      Shift:=8 - Font.CharWidth;
     end;

    {Update Characters}
    if Shift > 0 then
     begin
      for Count:=0 to Font.CharCount - 1 do
       begin
        for Row:=0 to Font.CharHeight - 1 do
         begin
          Buffer8^:=Buffer8^ shr Shift;

          Inc(Buffer8);
         end;
       end;
     end;
   end;
  9..16:begin
    {Get Buffer}
    Buffer16:=Font.CharData;

    {Check Swap}
    Swap:=True;
    if (Font.FontFlags and FONT_FLAG_BIGENDIAN) <> 0 then
     begin
      Swap:=False;
     end;

    {Check Shift}
    Shift:=0;
    if (Font.FontFlags and FONT_FLAG_RIGHTALIGN) = 0 then
     begin
      Shift:=16 - Font.CharWidth;
     end;

    {Update Characters}
    if Swap or (Shift > 0) then
     begin
      for Count:=0 to Font.CharCount - 1 do
       begin
        for Row:=0 to Font.CharHeight - 1 do
         begin
          if Swap then Buffer16^:=SwapEndian(Buffer16^);
          Buffer16^:=Buffer16^ shr Shift;

          Inc(Buffer16);
         end;
       end;
     end;
   end;
  17..32:begin
    {Get Buffer}
    Buffer32:=Font.CharData;

    {Check Swap}
    Swap:=True;
    if (Font.FontFlags and FONT_FLAG_BIGENDIAN) <> 0 then
     begin
      Swap:=False;
     end;

    {Check Shift}
    Shift:=0;
    if (Font.FontFlags and FONT_FLAG_RIGHTALIGN) = 0 then
     begin
      Shift:=32 - Font.CharWidth;
     end;

    {Update Characters}
    if Swap or (Shift > 0) then
     begin
      for Count:=0 to Font.CharCount - 1 do
       begin
        for Row:=0 to Font.CharHeight - 1 do
         begin
          if Swap then Buffer32^:=SwapEndian(Buffer32^);
          Buffer32^:=Buffer32^ shr Shift;

          Inc(Buffer32);
         end;
       end;
     end;
   end;
 end;

 {Insert Font}
 if CriticalSectionLock(FontTableLock) = ERROR_SUCCESS then
  begin
   try
    {Link Font}
    if FontTable = nil then
     begin
      FontTable:=Font;
     end
    else
     begin
      Font.Next:=FontTable;
      FontTable.Prev:=Font;
      FontTable:=Font;
     end;

    {Increment Count}
    Inc(FontTableCount);

    {Check Default}
    if FontDefault = INVALID_HANDLE_VALUE then
     begin
      FontDefault:=TFontHandle(Font);
     end;

    {Return Result}
    Result:=TFontHandle(Font);
   finally
    CriticalSectionUnlock(FontTableLock);
   end;
  end
 else
  begin
   {Free Unicode data}
   if Font.UnicodeData <> nil then
    begin
     FreeMem(Font.UnicodeData);
    end;

   {Free Pixel or Bitmap data}
   FreeMem(Font.CharData);

   {Free Font}
   FreeMem(Font);
  end;
end;

{==============================================================================}

function FontUnload(Handle:TFontHandle):LongWord;
var
 Font:PFontEntry;
 Prev:PFontEntry;
 Next:PFontEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Font}
 Font:=PFontEntry(Handle);
 if Font = nil then Exit;
 if Font.Signature <> FONT_SIGNATURE then Exit;

 {Check Font}
 Result:=ERROR_NOT_FOUND;
 if FontCheck(Font) <> Font then Exit;

 {Remove Font}
 if CriticalSectionLock(FontTableLock) = ERROR_SUCCESS then
  begin
   try
    {Unlink Font}
    Prev:=Font.Prev;
    Next:=Font.Next;
    if Prev = nil then
     begin
      FontTable:=Next;
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
    Dec(FontTableCount);

    {Check Default}
    if FontDefault = Handle then
     begin
      FontDefault:=TFontHandle(FontTable);
     end;

    {Update Font}
    Font.Signature:=0;

    {Free Unicode data}
    if Font.UnicodeData <> nil then
     begin
      FreeMem(Font.UnicodeData);
     end;

    {Free Pixel or Bitmap data}
    FreeMem(Font.CharData);

    {Free Font}
    FreeMem(Font);

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(FontTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function FontGetName(Handle:TFontHandle):String;
var
 Font:PFontEntry;
begin
 {}
 Result:='';

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Font}
 Font:=PFontEntry(Handle);
 if Font = nil then Exit;
 if Font.Signature <> FONT_SIGNATURE then Exit;

 {Acquire Lock}
 if CriticalSectionLock(FontTableLock) = ERROR_SUCCESS then
  begin
   try
    {Allocate Result}
    SetLength(Result,FONT_NAME_LENGTH - 1);

    {Get Name}
    StrLCopy(PChar(Result),Font.FontName,FONT_NAME_LENGTH - 1);

    {Update Result}
    SetLength(Result,StrLen(PChar(Result)));
   finally
    CriticalSectionUnlock(FontTableLock);
   end;
  end;
end;

{==============================================================================}

function FontGetDescription(Handle:TFontHandle):String;
var
 Font:PFontEntry;
begin
 {}
 Result:='';

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Font}
 Font:=PFontEntry(Handle);
 if Font = nil then Exit;
 if Font.Signature <> FONT_SIGNATURE then Exit;

 {Acquire Lock}
 if CriticalSectionLock(FontTableLock) = ERROR_SUCCESS then
  begin
   try
    {Allocate Result}
    SetLength(Result,FONT_DESC_LENGTH - 1);

    {Get Description}
    StrLCopy(PChar(Result),Font.FontDescription,FONT_DESC_LENGTH - 1);

    {Update Result}
    SetLength(Result,StrLen(PChar(Result)));
   finally
    CriticalSectionUnlock(FontTableLock);
   end;
  end;
end;

{==============================================================================}

function FontGetWidth(Handle:TFontHandle):LongWord;
var
 Font:PFontEntry;
begin
 {}
 Result:=0;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Font}
 Font:=PFontEntry(Handle);
 if Font = nil then Exit;
 if Font.Signature <> FONT_SIGNATURE then Exit;

 {Get Width}
 Result:=Font.CharWidth;
end;

{==============================================================================}

function FontGetHeight(Handle:TFontHandle):LongWord;
var
 Font:PFontEntry;
begin
 {}
 Result:=0;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Font}
 Font:=PFontEntry(Handle);
 if Font = nil then Exit;
 if Font.Signature <> FONT_SIGNATURE then Exit;

 {Get Height}
 Result:=Font.CharHeight;
end;

{==============================================================================}

function FontGetProperties(Handle:TFontHandle;Properties:PFontProperties):LongWord;
var
 Font:PFontEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Check Properties}
 if Properties = nil then Exit;

 {Get Font}
 Font:=PFontEntry(Handle);
 if Font = nil then Exit;
 if Font.Signature <> FONT_SIGNATURE then Exit;

 {Acquire Lock}
 if CriticalSectionLock(FontTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Properties}
    Properties.FontMode:=Font.FontMode;
    Properties.FontFlags:=Font.FontFlags;
    StrLCopy(Properties.FontName,Font.FontName,FONT_NAME_LENGTH - 1);
    StrLCopy(Properties.FontDescription,Font.FontDescription,FONT_DESC_LENGTH - 1);
    Properties.CharWidth:=Font.CharWidth;
    Properties.CharHeight:=Font.CharHeight;
    Properties.CharCount:=Font.CharCount;
    Properties.CharMask:=Font.CharMask;
    Properties.CodePage:=Font.CodePage;

    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(FontTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function FontCharWidth(Handle:TFontHandle;Character:Word):LongWord;
var
 Font:PFontEntry;
begin
 {}
 Result:=0;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Font}
 Font:=PFontEntry(Handle);
 if Font = nil then Exit;
 if Font.Signature <> FONT_SIGNATURE then Exit;

 {Check Flags}
 if (Font.FontFlags and FONT_FLAG_VARIABLE) = 0 then
  begin
   {Get Width}
   Result:=Font.CharWidth;
  end
 else
  begin
   {Calculate Width}
   //To Do
  end;
end;

{==============================================================================}

function FontCharHeight(Handle:TFontHandle;Character:Word):LongWord;
var
 Font:PFontEntry;
begin
 {}
 Result:=0;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Font}
 Font:=PFontEntry(Handle);
 if Font = nil then Exit;
 if Font.Signature <> FONT_SIGNATURE then Exit;

 {Check Flags}
 if (Font.FontFlags and FONT_FLAG_VARIABLE) = 0 then
  begin
   {Get Height}
   Result:=Font.CharHeight;
  end
 else
  begin
   {Calculate Height}
   //To Do
  end;
end;

{==============================================================================}

function FontTextWidth(Handle:TFontHandle;const Text:String):LongWord;
var
 Font:PFontEntry;
begin
 {}
 Result:=0;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Font}
 Font:=PFontEntry(Handle);
 if Font = nil then Exit;
 if Font.Signature <> FONT_SIGNATURE then Exit;

 {Check Flags}
 if (Font.FontFlags and FONT_FLAG_VARIABLE) = 0 then
  begin
   {Get Width}
   Result:=Font.CharWidth * Length(Text);
  end
 else
  begin
   {Calculate Width}
   //To Do
  end;
end;

{==============================================================================}

function FontTextHeight(Handle:TFontHandle;const Text:String):LongWord;
var
 Font:PFontEntry;
begin
 {}
 Result:=0;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Font}
 Font:=PFontEntry(Handle);
 if Font = nil then Exit;
 if Font.Signature <> FONT_SIGNATURE then Exit;

 {Check Flags}
 if (Font.FontFlags and FONT_FLAG_VARIABLE) = 0 then
  begin
   {Get Height}
   Result:=Font.CharHeight;
  end
 else
  begin
   {Calculate Height}
   //To Do
  end;
end;

{==============================================================================}

function FontFindByName(const Name:String):TFontHandle;
var
 Font:PFontEntry;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Acquire Lock}
 if CriticalSectionLock(FontTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Font}
    Font:=FontTable;
    while Font <> nil do
     begin
      {Check State}
      if Font.Signature = FONT_SIGNATURE then
       begin
        {Check Name}
        if Uppercase(Font.FontName) = Uppercase(Name) then
         begin
          Result:=TFontHandle(Font);
          Exit;
         end;
       end;

      {Get Next}
      Font:=Font.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(FontTableLock);
   end;
  end;
end;

{==============================================================================}

function FontFindByDescription(const Description:String):TFontHandle;
var
 Font:PFontEntry;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Acquire Lock}
 if CriticalSectionLock(FontTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Font}
    Font:=FontTable;
    while Font <> nil do
     begin
      {Check State}
      if Font.Signature = FONT_SIGNATURE then
       begin
        {Check Description}
        if Uppercase(Font.FontDescription) = Uppercase(Description) then
         begin
          Result:=TFontHandle(Font);
          Exit;
         end;
       end;

      {Get Next}
      Font:=Font.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(FontTableLock);
   end;
  end;
end;

{==============================================================================}

function FontEnumerate(Callback:TFontEnumerate;Data:Pointer):LongWord;
var
 Font:PFontEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Callback}
 if not Assigned(Callback) then Exit;

 {Acquire Lock}
 if CriticalSectionLock(FontTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Font}
    Font:=FontTable;
    while Font <> nil do
     begin
      {Check State}
      if Font.Signature = FONT_SIGNATURE then
       begin
        if Callback(TFontHandle(Font),Data) <> ERROR_SUCCESS then Exit;
       end;

      {Get Next}
      Font:=Font.Next;
     end;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(FontTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}
{==============================================================================}
{PSF Font Functions}
function PSFFontLoad(const FileName:String):TFontHandle;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 //To Do //Continuing

end;

{==============================================================================}

function PSFFontLoadEx(Data:Pointer;Size:LongWord):TFontHandle;
var
 Header:TFontHeader;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 //To Do //Continuing

end;

{==============================================================================}
{==============================================================================}
{Font Helper Functions}
function FontGetCount:LongWord;
{Get the current font count}
begin
 {}
 Result:=FontTableCount;
end;

{==============================================================================}

function FontGetDefault:TFontHandle;
{Get the current default font}
begin
 {}
 Result:=FontDefault;
end;

{==============================================================================}

function FontSetDefault(Handle:TFontHandle):LongWord;
{Set the current default font}
var
 Font:PFontEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Font}
 Font:=PFontEntry(Handle);
 if Font = nil then Exit;
 if Font.Signature <> FONT_SIGNATURE then Exit;

 {Acquire Lock}
 if CriticalSectionLock(FontTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check Font}
    if FontCheck(Font) <> Font then Exit;

    {Set Font Default}
    FontDefault:=Handle;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(FontTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function FontCheck(Font:PFontEntry):PFontEntry;
{Check if the supplied Font is in the Font table}
var
 Current:PFontEntry;
begin
 {}
 Result:=nil;

 {Check Font}
 if Font = nil then Exit;
 if Font.Signature <> FONT_SIGNATURE then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(FontTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Font}
    Current:=FontTable;
    while Current <> nil do
     begin
      {Check Font}
      if Current = Font then
       begin
        Result:=Font;
        Exit;
       end;

      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(FontTableLock);
   end;
  end;
end;

{==============================================================================}
{==============================================================================}
{PSF Font Helper Functions}

{==============================================================================}
{==============================================================================}

initialization
 FontInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.

