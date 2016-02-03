{
Ultibo Font interface unit.

Copyright (C) 2015 - SoftOz Pty Ltd.

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

  ????

Fonts
=====

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Font;

interface
                      
uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {Font specific constants}
 FONT_MIN_WIDTH = 8;
 FONT_MAX_WIDTH = 32;
 
 FONT_MIN_HEIGHT = 8;
 FONT_MAX_HEIGHT = 64;
 
 {Font Signature}
 FONT_SIGNATURE = $77DE1BBC;
 
 {Font Type constants}
 FONT_TYPE_NONE = 0;
 
 {Font Flag constants}
 FONT_FLAG_NONE = $00000000;
              
{==============================================================================}
type
 {Font specific types}
 
 {Font Header}
 PFontHeader = ^TFontHeader;
 TFontHeader = record
  Width:LongWord;
  Height:LongWord;
  Name:String[255];
 end;
 
 {Font Data}
 PFontData = ^TFontData;
 TFontData = record
  Width:LongWord;
  Height:LongWord;
  Name:String[255];
  Data:array[0..0] of Byte;
 end;
 
 {Font Data (8 bit)}
 PFontData8x8 = ^TFontData8x8;
 TFontData8x8 = record
  Width:LongWord;
  Height:LongWord;
  Name:String[255];
  Data:array[0..255,0..7] of Byte;
 end; 

 PFontData8x12 = ^TFontData8x12;
 TFontData8x12 = record
  Width:LongWord;
  Height:LongWord;
  Name:String[255];
  Data:array[0..255,0..11] of Byte;
 end; 

 PFontData8x16 = ^TFontData8x16;
 TFontData8x16 = record
  Width:LongWord;
  Height:LongWord;
  Name:String[255];
  Data:array[0..255,0..15] of Byte;
 end; 

 {Font Data (16 bit)}
 PFontData16x16 = ^TFontData16x16;
 TFontData16x16 = record
  Width:LongWord;
  Height:LongWord;
  Name:String[255];
  Data:array[0..255,0..15] of Word;
 end; 

 PFontData16x24 = ^TFontData16x24;
 TFontData16x24 = record
  Width:LongWord;
  Height:LongWord;
  Name:String[255];
  Data:array[0..255,0..23] of Word;
 end; 
 
 PFontData16x32 = ^TFontData16x32;
 TFontData16x32 = record
  Width:LongWord;
  Height:LongWord;
  Name:String[255];
  Data:array[0..255,0..31] of Word;
 end; 
 
 {Font Data (32 bit)}
 PFontData32x32 = ^TFontData32x32;
 TFontData32x32 = record
  Width:LongWord;
  Height:LongWord;
  Name:String[255];
  Data:array[0..255,0..31] of LongWord;
 end; 

 PFontData32x48 = ^TFontData32x48;
 TFontData32x48 = record
  Width:LongWord;
  Height:LongWord;
  Name:String[255];
  Data:array[0..255,0..47] of LongWord;
 end; 
 
 PFontData32x64 = ^TFontData32x64;
 TFontData32x64 = record
  Width:LongWord;
  Height:LongWord;
  Name:String[255];
  Data:array[0..255,0..63] of LongWord;
 end; 
 
 {Font Chars}
 PFontChars8 = ^TFontChars8;
 TFontChars8 = array[0..0] of Byte;

 PFontChars16 = ^TFontChars32;
 TFontChars16 = array[0..0] of Word;

 PFontChars32 = ^TFontChars32;
 TFontChars32 = array[0..0] of LongWord;
 
 {Font Chars (8 bit)}
 PFontChars8x8 = ^TFontChars8x8;
 TFontChars8x8 = array[0..255,0..7] of Byte;

 PFontChars8x12 = ^TFontChars8x12;
 TFontChars8x12 = array[0..255,0..11] of Byte;

 PFontChars8x16 = ^TFontChars8x16;
 TFontChars8x16 = array[0..255,0..15] of Byte;
 
 {Font Chars (16 bit)}
 PFontChars16x16 = ^TFontChars16x16;
 TFontChars16x16 = array[0..255,0..15] of Word;

 PFontChars16x24 = ^TFontChars16x24;
 TFontChars16x24 = array[0..255,0..23] of Word;
 
 PFontChars16x32 = ^TFontChars16x32;
 TFontChars16x32 = array[0..255,0..31] of Word;

 {Font Chars (32 bit)}
 PFontChars32x32 = ^TFontChars32x32;
 TFontChars32x32 = array[0..255,0..31] of LongWord;

 PFontChars32x48 = ^TFontChars32x48;
 TFontChars32x48 = array[0..255,0..47] of LongWord;
 
 PFontChars32x64 = ^TFontChars32x64;
 TFontChars32x64 = array[0..255,0..63] of LongWord;
 
 {Font Properties}
 PFontProperties = ^TFontProperties;
 TFontProperties = record
  FontType:LongWord;       {Font type}
  FontFlags:LongWord;      {Font flags}
  FontName:String;         {Font name}
  CharWidth:LongWord;      {Font character width in pixels}
  CharHeight:LongWord;     {Font character height in pixels}
 end;

 PFontEntry = ^TFontEntry;
 
 {Font Enumeration Callback}
 TFontEnumerate = function(Handle:TFontHandle;Data:Pointer):LongWord;
 
 {Font Entry}
 TFontEntry = record
  {Font Properties}
  Signature:LongWord;            {Signature for entry validation}
  FontType:LongWord;             {Font type}
  FontFlags:LongWord;            {Font flags}
  FontName:String;               {Font name}
  {Driver Properties}
  CharWidth:LongWord;            {Font character width in pixels}
  CharHeight:LongWord;           {Font character height in pixels}
  CharData:Pointer;              {Font character pixel data}
  {Internal Properties}
  Prev:PFontEntry;               {Previous entry in Font table}
  Next:PFontEntry;               {Next entry in Font table}
 end;

{==============================================================================}
{var}
 {Font specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure FontInit;

{==============================================================================}
{Font Functions}
function FontLoad(Data:PFontData;Size:LongWord):TFontHandle;
function FontLoadEx(Data:PFontData;Size:LongWord;Properties:PFontProperties):TFontHandle;
function FontUnload(Handle:TFontHandle):LongWord;

function FontGetName(Handle:TFontHandle):String;
function FontGetWidth(Handle:TFontHandle):LongWord;
function FontGetHeight(Handle:TFontHandle):LongWord;

function FontGetProperties(Handle:TFontHandle;Properties:PFontProperties):LongWord;

function FontEnumerate(Callback:TFontEnumerate;Data:Pointer):LongWord;

{==============================================================================}
{Font Helper Functions}
function FontGetCount:LongWord; inline;
function FontGetDefault:TFontHandle; inline;
function FontSetDefault(Handle:TFontHandle):LongWord;

function FontCheck(Font:PFontEntry):PFontEntry;

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
 FONT_LATIN1_8x16:TFontData8x16 = (
  Width:8;
  Height:16;
  Name:('Latin-1 (8x16) Console Font');
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
 FontDefault:=FontLoad(@FONT_LATIN1_8x16,SizeOf(TFontData8x16));
 if FontDefault = INVALID_HANDLE_VALUE then
  begin
   if PLATFORM_LOG_ENABLED then PlatformLogError('Failed to load default font');
  end;
 
 FontInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Font Functions}
function FontLoad(Data:PFontData;Size:LongWord):TFontHandle;
{Load a Font from a font data block and add to the Font table}
begin
 {}
 Result:=FontLoadEx(Data,Size,nil);
end;

{==============================================================================}

function FontLoadEx(Data:PFontData;Size:LongWord;Properties:PFontProperties):TFontHandle;
{Load a Font from a font data block and add to the Font table}
var
 Font:PFontEntry;
 TotalSize:LongWord;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Check Data}
 if Data = nil then Exit;
 
 {Check Size}
 if Size < SizeOf(TFontData) then Exit;
 
 {Check Properties}
 if Properties = nil then
  begin
   {Check Width}
   if Data.Width < FONT_MIN_WIDTH then Exit;
   if Data.Width > FONT_MAX_WIDTH then Exit;
   if (Data.Width mod 8) <> 0 then Exit;
   
   {Check Height}
   if Data.Height < FONT_MIN_HEIGHT then Exit;
   if Data.Height > FONT_MAX_HEIGHT then Exit;
   
   {Get Size}
   TotalSize:=((Data.Width shr 3) * Data.Height) shl 8; {((Width * 8) * Height) * 256}
   
   {Check Size}
   if Size < (TotalSize + SizeOf(TFontHeader)) then Exit;
   
   {Create Font}
   Font:=PFontEntry(AllocMem(SizeOf(TFontEntry)));
   if Font = nil then Exit;
   
   {Update Font}
   Font.Signature:=FONT_SIGNATURE;
   Font.FontType:=FONT_TYPE_NONE;
   Font.FontFlags:=FONT_FLAG_NONE;
   Font.FontName:=Data.Name;
   Font.CharWidth:=Data.Width;
   Font.CharHeight:=Data.Height;
   Font.CharData:=nil;
   
   {Update Font}
   UniqueString(Font.FontName);
   Font.CharData:=GetMem(TotalSize);
   if Font.CharData = nil then
    begin
     FreeMem(Font);
     Exit;
    end;
    
   {Copy Data}
   System.Move(Data.Data[0],Font.CharData^,TotalSize);
  end
 else
  begin 
   {Check Width}
   if Properties.CharWidth < FONT_MIN_WIDTH then Exit;
   if Properties.CharWidth > FONT_MAX_WIDTH then Exit;
   if (Properties.CharWidth mod 8) <> 0 then Exit;
 
   {Check Height}
   if Properties.CharHeight < FONT_MIN_HEIGHT then Exit;
   if Properties.CharHeight > FONT_MAX_HEIGHT then Exit;
 
   {Get Size}
   TotalSize:=((Properties.CharWidth shr 3) * Properties.CharHeight) shl 8; {((Width * 8) * Height) * 256}
   
   {Check Size}
   if Size < (TotalSize + SizeOf(TFontHeader)) then Exit;
   
   {Create Font}
   Font:=PFontEntry(AllocMem(SizeOf(TFontEntry)));
   if Font = nil then Exit;
   
   {Update Font}
   Font.Signature:=FONT_SIGNATURE;
   Font.FontType:=Properties.FontType;
   Font.FontFlags:=Properties.FontFlags;
   Font.FontName:=Properties.FontName;
   Font.CharWidth:=Properties.CharWidth;
   Font.CharHeight:=Properties.CharHeight;
   Font.CharData:=nil;
   
   {Update Font}
   UniqueString(Font.FontName);
   Font.CharData:=GetMem(TotalSize);
   if Font.CharData = nil then
    begin
     FreeMem(Font);
     Exit;
    end;
   
   {Copy Data}
   System.Move(Data.Data[0],Font.CharData^,TotalSize);
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
   {Free Pixels}
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
 
    {Free Pixels}
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
    {Get Name}
    Result:=Font.FontName;
    
    UniqueString(Result);
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
    Properties.FontType:=Font.FontType;
    Properties.FontFlags:=Font.FontFlags;
    Properties.FontName:=Font.FontName;
    Properties.CharWidth:=Font.CharWidth;
    Properties.CharHeight:=Font.CharHeight;
    
    UniqueString(Properties.FontName);
    
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
{Font Helper Functions}
function FontGetCount:LongWord; inline;
{Get the current font count}
begin
 {}
 Result:=FontTableCount;
end;

{==============================================================================}

function FontGetDefault:TFontHandle; inline;
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

initialization
 FontInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
                      