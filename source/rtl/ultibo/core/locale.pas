{
Ultibo Locale interface unit.

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

 
References
==========


Locale
======

 This unit implements the locale support for Ultibo and provides the code page
 support for the WideStringManager/UnicodeStringManager interface for the RTL.
 
 This unit provides compatible implementations of the following functions:
 
        ConvertDefaultLocale    EnumCalendarInfo          EnumCalendarInfoProc
        EnumCodePagesProc       EnumDateFormats           EnumDateFormatsProc
        EnumLocalesProc         EnumSystemCodePages       EnumSystemLocales
        EnumTimeFormats         EnumTimeFormatsProc       GetACP
        GetCPInfo               GetCurrencyFormat         GetDateFormat
        GetNumberFormat         GetLocaleInfo             GetOEMCP
        GetSystemDefaultLangID  GetSystemDefaultLCID      GetTimeFormat
        GetUserDefaultLangID    GetUserDefaultLCID        IsValidCodePage
        IsValidLocale           SetLocaleInfo             GetConsoleCP
        GetConsoleOutputCP      SetConsoleCP              SetConsoleOutputCP
 
        The following function are implemented by the Threads unit:

        GetThreadLocale (ThreadGetLocale) (GetThreadLocale is exposed in the Ultibo unit)
        SetThreadLocale (ThreadSetLocale) (SetThreadLocale is exposed in the Ultibo unit)

        Most of the above are currently not implemented.
  
 OEM Code Page defaults to 437   (OEM United States)
 ANSI Code Page defaults to 1252 (ANSI Latin 1; Western European (Windows))
 
 See also: https://msdn.microsoft.com/en-us/library/windows/desktop/dd319081%28v=vs.85%29.aspx
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Locale;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,SysUtils;

//To Do //Extract a list of default Locale info (LCID etc) from Windows using GetLocaleInfoW
                      //See: https://msdn.microsoft.com/en-us/goglobal/bb896001.aspx

//To Do //Code Page Identifiers are listed at https://msdn.microsoft.com/en-us/library/windows/desktop/dd317756%28v=vs.85%29.aspx
                     
//To Do //See also: \jwa\branches\2.3\Win32API\JwaWinNLS.pas
          
//To Do //See if any of this can be reworked to use the existing charset unit
        //\source\rtl\inc\charset.pp
        //and the units in \source\rtl\charmaps
        
{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}
        
{==============================================================================}
const
 {Locale specific constants}
 
 {Default Code Pages}
 //To Do //Most of these are defined in System (see: \source\rtl\inc\systemh.inc)
 CP_ACP        = 0;     {Default to ANSI code page}
 CP_OEMCP      = 1;     {Default to OEM  code page}
 CP_MACCP      = 2;     {Default to MAC  code page}
 CP_THREAD_ACP = 3;     {Current thread's ANSI code page}
 CP_SYMBOL     = 42;    {SYMBOL translations}
 CP_UTF16      = 1200;  {UTF-16 translation}
 CP_UNICODE    = 1200;  {Unicode translation}
 CP_UTF16_BE   = 1201;  {UTF-16 (unicodeFFFE) translation}
 CP_UTF7       = 65000; {UTF-7 translation}
 CP_UTF8       = 65001; {UTF-8 translation}
 //CP_ASCII      = 20127; {US-ASCII}
 //CP_NONE       = $FFFF; {FPC RawByteString}

 {Code Page Identifiers (See: https://en.wikipedia.org/wiki/Code_page)}
 CP_OEM_437    = 437;  {US}
 CP_OEM_720    = 720;  {Arabic}
 CP_OEM_737    = 737;  {Greek}
 CP_OEM_775    = 775;  {Baltic}
 CP_OEM_850    = 850;  {Latin I}
 CP_OEM_852    = 852;  {Latin II}
 CP_OEM_857    = 857;  {Turkish}
 CP_OEM_862    = 862;  {Hebrew}
 CP_OEM_866    = 866;  {Russian}
 CP_OEM_874    = 874;  {Thai}
 CP_OEM_1258   = 1258; {Vietnam}
 
 CP_ANSI_1250  = 1250;  {Central Europe}
 CP_ANSI_1251  = 1251;  {Cyrillic}
 CP_ANSI_1252  = 1252;  {Latin I}
 CP_ANSI_1253  = 1253;  {Greek}
 CP_ANSI_1254  = 1254;  {Turkish}
 CP_ANSI_1255  = 1255;  {Hebrew}
 CP_ANSI_1256  = 1256;  {Arabic}
 CP_ANSI_1257  = 1257;  {Baltic}
 CP_ANSI_1258  = 1258;  {Vietnam}
 CP_ANSI_874   = 874;   {Thai}

 {Default Locales}
 LOCALE_SYSTEM_DEFAULT = $800;
 LOCALE_USER_DEFAULT   = $400;
 
 {String Length Maximums}
 MAX_LEADBYTES = 12;  {5 ranges, 2 bytes each, 0 terminated}
 MAX_DEFAULTCHAR = 2; {single or double byte}
 
 {MBCS and Unicode Translation Flags}
 MB_PRECOMPOSED       = $00000001; {use precomposed chars}
 MB_COMPOSITE         = $00000002; {use composite chars}
 MB_USEGLYPHCHARS     = $00000004; {use glyph chars, not ctrl chars}
 MB_ERR_INVALID_CHARS = $00000008; {error for invalid chars}

 WC_COMPOSITECHECK = $00000200; {convert composite to precomposed}
 WC_DISCARDNS      = $00000010; {discard non-spacing chars}
 WC_SEPCHARS       = $00000020; {generate separate chars}
 WC_DEFAULTCHAR    = $00000040; {replace w/ default AnsiChar}

 WC_NO_BEST_FIT_CHARS = $00000400; {do not use best fit chars}
 
 {Character Type Flags}
 CT_CTYPE1 = $00000001; // ctype 1 information
 CT_CTYPE2 = $00000002; // ctype 2 information
 CT_CTYPE3 = $00000004; // ctype 3 information
 
 {CType 1 Flag Bits}
 C1_UPPER  = $0001; // upper case
 C1_LOWER  = $0002; // lower case
 C1_DIGIT  = $0004; // decimal digits
 C1_SPACE  = $0008; // spacing characters
 C1_PUNCT  = $0010; // punctuation characters
 C1_CNTRL  = $0020; // control characters
 C1_BLANK  = $0040; // blank characters
 C1_XDIGIT = $0080; // other digits
 C1_ALPHA  = $0100; // any linguistic character
 C1_DEFINED = $0200; // defined character
 
 {CType 2 Flag Bits}
 C2_LEFTTORIGHT = $0001; // left to right
 C2_RIGHTTOLEFT = $0002; // right to left

 C2_EUROPENUMBER     = $0003; // European number, digit
 C2_EUROPESEPARATOR  = $0004; // European numeric separator
 C2_EUROPETERMINATOR = $0005; // European numeric terminator
 C2_ARABICNUMBER     = $0006; // Arabic number
 C2_COMMONSEPARATOR  = $0007; // common numeric separator

 C2_BLOCKSEPARATOR   = $0008; // block separator
 C2_SEGMENTSEPARATOR = $0009; // segment separator
 C2_WHITESPACE       = $000A; // white space
 C2_OTHERNEUTRAL     = $000B; // other neutrals

 C2_NOTAPPLICABLE = $0000; // no implicit directionality
 
 {CType 3 Flag Bits}
 C3_NONSPACING = $0001; // nonspacing character
 C3_DIACRITIC  = $0002; // diacritic mark
 C3_VOWELMARK  = $0004; // vowel mark
 C3_SYMBOL     = $0008; // symbols

 C3_KATAKANA  = $0010; // katakana character
 C3_HIRAGANA  = $0020; // hiragana character
 C3_HALFWIDTH = $0040; // half width character
 C3_FULLWIDTH = $0080; // full width character
 C3_IDEOGRAPH = $0100; // ideographic character
 C3_KASHIDA   = $0200; // Arabic kashida character
 C3_LEXICAL   = $0400; // lexical character

 C3_ALPHA = $8000; // any linguistic AnsiChar (C1_ALPHA)

 C3_NOTAPPLICABLE = $0000; // ctype 3 is not applicable
 
 {String Flags}
 NORM_IGNORECASE     = $00000001; // ignore case
 NORM_IGNORENONSPACE = $00000002; // ignore nonspacing chars
 NORM_IGNORESYMBOLS  = $00000004; // ignore symbols

 NORM_IGNOREKANATYPE = $00010000; // ignore kanatype
 NORM_IGNOREWIDTH    = $00020000; // ignore width
 
 {Locale Independent Mapping Flags}
 MAP_FOLDCZONE   = $00000010; // fold compatibility zone chars
 MAP_PRECOMPOSED = $00000020; // convert to precomposed chars
 MAP_COMPOSITE   = $00000040; // convert to composite chars
 MAP_FOLDDIGITS  = $00000080; // all digits to ASCII 0-9

 MAP_EXPAND_LIGATURES = $00002000; // expand all ligatures
 
 {Locale Dependent Mapping Flags}
 LCMAP_LOWERCASE = $00000100; // lower case letters
 LCMAP_UPPERCASE = $00000200; // upper case letters
 LCMAP_SORTKEY   = $00000400; // WC sort key (normalize)
 LCMAP_BYTEREV   = $00000800; // byte reversal

 LCMAP_HIRAGANA  = $00100000; // map katakana to hiragana
 LCMAP_KATAKANA  = $00200000; // map hiragana to katakana
 LCMAP_HALFWIDTH = $00400000; // map double byte to single byte
 LCMAP_FULLWIDTH = $00800000; // map single byte to double byte

 LCMAP_LINGUISTIC_CASING = $01000000; // use linguistic rules for casing

 LCMAP_SIMPLIFIED_CHINESE  = $02000000; // map traditional chinese to simplified chinese
 LCMAP_TRADITIONAL_CHINESE = $04000000; // map simplified chinese to traditional chinese
 
 {Language Group Enumeration Flags}
 LGRPID_INSTALLED = $00000001; // installed language group ids
 LGRPID_SUPPORTED = $00000002; // supported language group ids

 {Locale Enumeration Flags}
 LCID_INSTALLED       = $00000001; // installed locale ids
 LCID_SUPPORTED       = $00000002; // supported locale ids
 LCID_ALTERNATE_SORTS = $00000004; // alternate sort locale ids

 {Code Page Enumeration Flags}
 CP_INSTALLED = $00000001; // installed code page ids
 CP_SUPPORTED = $00000002; // supported code page ids

 {Sorting Flags}
 //    WORD Sort:    culturally correct sort
 //                  hyphen and apostrophe are special cased
 //                  example: "coop" and "co-op" will sort together in a list
 //
 //                        co_op     <-------  underscore (symbol)
 //                        coat
 //                        comb
 //                        coop
 //                        co-op     <-------  hyphen (punctuation)
 //                        cork
 //                        went
 //                        were
 //                        we're     <-------  apostrophe (punctuation)
 //
 //
 //    STRING Sort:  hyphen and apostrophe will sort with all other symbols
 //
 //                        co-op     <-------  hyphen (punctuation)
 //                        co_op     <-------  underscore (symbol)
 //                        coat
 //                        comb
 //                        coop
 //                        cork
 //                        we're     <-------  apostrophe (punctuation)
 //                        went
 //                        were
 //
 SORT_STRINGSORT = $00001000; // use string sort method
 
 {Compare String Return Values}
 CSTR_LESS_THAN    = 1; // string 1 less than string 2
 CSTR_EQUAL        = 2; // string 1 equal to string 2
 CSTR_GREATER_THAN = 3; // string 1 greater than string 2
 
 {Country/Region Codes}
 CTRY_DEFAULT = 0;

 CTRY_ALBANIA            = 355; // Albania
 CTRY_ALGERIA            = 213; // Algeria
 CTRY_ARGENTINA          = 54; // Argentina
 CTRY_ARMENIA            = 374; // Armenia
 CTRY_AUSTRALIA          = 61; // Australia
 CTRY_AUSTRIA            = 43; // Austria
 CTRY_AZERBAIJAN         = 994; // Azerbaijan
 CTRY_BAHRAIN            = 973; // Bahrain
 CTRY_BELARUS            = 375; // Belarus
 CTRY_BELGIUM            = 32; // Belgium
 CTRY_BELIZE             = 501; // Belize
 CTRY_BOLIVIA            = 591; // Bolivia
 CTRY_BRAZIL             = 55; // Brazil
 CTRY_BRUNEI_DARUSSALAM  = 673; // Brunei Darussalam
 CTRY_BULGARIA           = 359; // Bulgaria
 CTRY_CANADA             = 2; // Canada
 CTRY_CARIBBEAN          = 1; // Caribbean
 CTRY_CHILE              = 56; // Chile
 CTRY_COLOMBIA           = 57; // Colombia
 CTRY_COSTA_RICA         = 506; // Costa Rica
 CTRY_CROATIA            = 385; // Croatia
 CTRY_CZECH              = 420; // Czech Republic
 CTRY_DENMARK            = 45; // Denmark
 CTRY_DOMINICAN_REPUBLIC = 1; // Dominican Republic
 CTRY_ECUADOR            = 593; // Ecuador
 CTRY_EGYPT              = 20; // Egypt
 CTRY_EL_SALVADOR        = 503; // El Salvador
 CTRY_ESTONIA            = 372; // Estonia
 CTRY_FAEROE_ISLANDS     = 298; // Faeroe Islands
 CTRY_FINLAND            = 358; // Finland
 CTRY_FRANCE             = 33; // France
 CTRY_GEORGIA            = 995; // Georgia
 CTRY_GERMANY            = 49; // Germany
 CTRY_GREECE             = 30; // Greece
 CTRY_GUATEMALA          = 502; // Guatemala
 CTRY_HONDURAS           = 504; // Honduras
 CTRY_HONG_KONG          = 852; // Hong Kong S.A.R., P.R.C.
 CTRY_HUNGARY            = 36; // Hungary
 CTRY_ICELAND            = 354; // Iceland
 CTRY_INDIA              = 91; // India
 CTRY_INDONESIA          = 62; // Indonesia
 CTRY_IRAN               = 981; // Iran
 CTRY_IRAQ               = 964; // Iraq
 CTRY_IRELAND            = 353; // Ireland
 CTRY_ISRAEL             = 972; // Israel
 CTRY_ITALY              = 39; // Italy
 CTRY_JAMAICA            = 1; // Jamaica
 CTRY_JAPAN              = 81; // Japan
 CTRY_JORDAN             = 962; // Jordan
 CTRY_KAZAKSTAN          = 7; // Kazakstan
 CTRY_KENYA              = 254; // Kenya
 CTRY_KUWAIT             = 965; // Kuwait
 CTRY_KYRGYZSTAN         = 996; // Kyrgyzstan
 CTRY_LATVIA             = 371; // Latvia
 CTRY_LEBANON            = 961; // Lebanon
 CTRY_LIBYA              = 218; // Libya
 CTRY_LIECHTENSTEIN      = 41; // Liechtenstein
 CTRY_LITHUANIA          = 370; // Lithuania
 CTRY_LUXEMBOURG         = 352; // Luxembourg
 CTRY_MACAU              = 853; // Macau S.A.R., PRC
 CTRY_MACEDONIA          = 389; // Former Yugoslav Republic of Macedonia
 CTRY_MALAYSIA           = 60; // Malaysia
 CTRY_MALDIVES           = 960; // Maldives
 CTRY_MEXICO             = 52; // Mexico
 CTRY_MONACO             = 33; // Principality of Monaco
 CTRY_MONGOLIA           = 976; // Mongolia
 CTRY_MOROCCO            = 212; // Morocco
 CTRY_NETHERLANDS        = 31; // Netherlands
 CTRY_NEW_ZEALAND        = 64; // New Zealand
 CTRY_NICARAGUA          = 505; // Nicaragua
 CTRY_NORWAY             = 47; // Norway
 CTRY_OMAN               = 968; // Oman
 CTRY_PAKISTAN           = 92; // Islamic Republic of Pakistan
 CTRY_PANAMA             = 507; // Panama
 CTRY_PARAGUAY           = 595; // Paraguay
 CTRY_PERU               = 51; // Peru
 CTRY_PHILIPPINES        = 63; // Republic of the Philippines
 CTRY_POLAND             = 48; // Poland
 CTRY_PORTUGAL           = 351; // Portugal
 CTRY_PRCHINA            = 86; // People's Republic of China
 CTRY_PUERTO_RICO        = 1; // Puerto Rico
 CTRY_QATAR              = 974; // Qatar
 CTRY_ROMANIA            = 40; // Romania
 CTRY_RUSSIA             = 7; // Russia
 CTRY_SAUDI_ARABIA       = 966; // Saudi Arabia
 CTRY_SERBIA             = 381; // Serbia
 CTRY_SINGAPORE          = 65; // Singapore
 CTRY_SLOVAK             = 421; // Slovak Republic
 CTRY_SLOVENIA           = 386; // Slovenia
 CTRY_SOUTH_AFRICA       = 27; // South Africa
 CTRY_SOUTH_KOREA        = 82; // Korea
 CTRY_SPAIN              = 34; // Spain
 CTRY_SWEDEN             = 46; // Sweden
 CTRY_SWITZERLAND        = 41; // Switzerland
 CTRY_SYRIA              = 963; // Syria
 CTRY_TAIWAN             = 886; // Taiwan
 CTRY_TATARSTAN          = 7; // Tatarstan
 CTRY_THAILAND           = 66; // Thailand
 CTRY_TRINIDAD_Y_TOBAGO  = 1; // Trinidad y Tobago
 CTRY_TUNISIA            = 216; // Tunisia
 CTRY_TURKEY             = 90; // Turkey
 CTRY_UAE                = 971; // U.A.E.
 CTRY_UKRAINE            = 380; // Ukraine
 CTRY_UNITED_KINGDOM     = 44; // United Kingdom
 CTRY_UNITED_STATES      = 1; // United States
 CTRY_URUGUAY            = 598; // Uruguay
 CTRY_UZBEKISTAN         = 7; // Uzbekistan
 CTRY_VENEZUELA          = 58; // Venezuela
 CTRY_VIET_NAM           = 84; // Viet Nam
 CTRY_YEMEN              = 967; // Yemen
 CTRY_ZIMBABWE           = 263; // Zimbabwe
 
 {Locale Types}
 //To Do

 {Time Flags for GetTimeFormat}
 //To Do

 {Date Flags for GetDateFormat}
 //To Do

 {Calendar Types}
 //To Do

 {Calendar Enumeration Value}
 //To Do

 {Calendar ID Values}
 //To Do

 {Language Group ID Values}
 //To Do
              
{==============================================================================}
type
 {Locale specific types}
 
 {Locale and Language Ids}
 LCID = DWORD;
 PLCID = ^LCID;
 LANGID = Word;
 PLANGID = ^LANGID;
 
 {Language Group ID}
 LGRPID = DWORD;  
 {Locale type constant}
 LCTYPE = DWORD;
 {Calendar type constant}
 CALTYPE = DWORD;
 {Calendar ID}
 CALID = DWORD;
 
type
 {CP Info} 
 LPCPINFO = ^CPINFO;
 _cpinfo = record
   MaxCharSize: UINT; // max length (in bytes) of a AnsiChar
   DefaultChar: array [0..MAX_DEFAULTCHAR - 1] of BYTE; // default character
   LeadByte: array [0..MAX_LEADBYTES - 1] of BYTE; // lead byte ranges
 end;
 CPINFO = _cpinfo;
 TCpInfo = CPINFO;
 PCpInfo = LPCPINFO;

 LPCPINFOEXA = ^CPINFOEXA;
 _cpinfoexA = record
   MaxCharSize: UINT; // max length (in bytes) of a AnsiChar
   DefaultChar: array [0..MAX_DEFAULTCHAR - 1] of BYTE; // default character (MB)
   LeadByte: array [0..MAX_LEADBYTES - 1] of BYTE; // lead byte ranges
   UnicodeDefaultChar: WCHAR; // default character (Unicode)
   CodePage: UINT; // code page id
   CodePageName: array [0..MAX_PATH - 1] of AnsiChar; // code page name (Ansi)
 end;
 CPINFOEXA = _cpinfoexA;
 TCpInfoExA = CPINFOEXA;
 PCpInfoExA = LPCPINFOEXA;

 LPCPINFOEXW = ^CPINFOEXW;
 _cpinfoexW = record
   MaxCharSize: UINT; // max length (in bytes) of a AnsiChar
   DefaultChar: array [0..MAX_DEFAULTCHAR - 1] of BYTE; // default character (MB)
   LeadByte: array [0..MAX_LEADBYTES - 1] of BYTE; // lead byte ranges
   UnicodeDefaultChar: WCHAR; // default character (Unicode)
   CodePage: UINT; // code page id
   CodePageName: array [0..MAX_PATH - 1] of WCHAR; // code page name (Unicode)
 end;
 CPINFOEXW = _cpinfoexW;
 TCpInfoExW = CPINFOEXW;
 PCpInfoExW = LPCPINFOEXW;

 CPINFOEX = CPINFOEXA;
 LPCPINFOEX = LPCPINFOEXA;
 TCpInfoEx = TCpInfoExA;
 PCpInfoEx = PCpInfoExA;
 
type 
 {Code Table - The OEM/ANSI to UNICODE Values of a Code Page}
 PCodeTable = ^TCodeTable;
 TCodeTable = record
  MaxCharSize:UINT;                                  {Max Length (Bytes) of a Char}
  DefaultChar:array[0..MAX_DEFAULTCHAR - 1] of Byte; {Default Character}
  LeadByte:array[0..MAX_LEADBYTES - 1] of Byte;      {Lead Byte Ranges}
  Values:array[$00..$FF] of Word;
 end;

 {Trans Table - The OEM/ANSI to ANSI/OEM Values of a Code Page}
 PTransTable = ^TTransTable;
 TTransTable = record
  TransID:Word;                    {Translate Code Page}
  Values:array[$00..$FF] of Word;  {Word to allow for DBCS}
 end;

 {Lower Table - Upper to Lower case values of a Code Page}
 PLowerTable = ^TLowerTable;
 TLowerTable = record
  LowerID:Word;                    {Lowercase Code Page}
  Values:array[$00..$FF] of Word;  {Word to allow for DBCS}
 end;
 
 {Upper Table - Lower to Upper case values of a Code Page}
 PUpperTable = ^TUpperTable;
 TUpperTable = record
  UpperID:Word;                    {Uppercase Code Page}
  Values:array[$00..$FF] of Word;  {Word to allow for DBCS}
 end;
 
 //To Do //SortTable
 
 {Unicode Table - The UNICODE to OEM/ANSI Values of a Code Page}
 PUnicodeTable = ^TUnicodeTable;
 TUnicodeTable = record
  Values:array[$0000..$FFFF] of Word;  {Word to allow for DBCS}
 end;

 {Lead Bytes - The extended Page Tables for DBCS Pages}
 PLeadBytes = ^TLeadBytes;
 TLeadBytes = record
  Tables:array[$00..$FF] of PCodeTable;
 end;
 {Each Code Table provides the OEM to UNICODE Values}
 {To convert back use the main Unicode Table in the Code Page}
 {which was populated when the Code Page was loaded}

 {Code Page - The actual information for an OEM/ANSI Code Page}
 PCodePage = ^TCodePage;
 TCodePage = record
  PageID:Word;                 {Code Page ID (eg 437 or 1252)}
  Handle:THandle;              {Handle of Code Page Module}
  CodeTable:PCodeTable;        {The OEM/ANSI Values}
  LeadBytes:PLeadBytes;        {The Lead Byte Tables}
  TransTable:PTransTable;      {The OEM <-> ANSI Values}
  LowerTable:PLowerTable;      {The Upper to Lower Values}
  UpperTable:PUpperTable;      {The Lower to Upper Values}
  UnicodeTable:PUnicodeTable;  {The UNICODE Values}
  PrevPage:PCodePage;          {Prev Code Page}
  NextPage:PCodePage;          {Next Code Page}
 end;

{==============================================================================}
var
 {Locale specific variables}
 OemPage:PCodePage;
 AnsiPage:PCodePage;
 DefaultPage:PCodePage;
 
 CodePageLock:TPlatformLock;
 
{==============================================================================}
{Initialization Functions}
procedure LocaleInit;

{==============================================================================}
{Locale Functions}
function IsValidCodePage(CodePage:UINT):BOOL; 

function GetACP:UINT; 
function GetOEMCP:UINT; 

function SetACP(CodePage:UINT):BOOL;
function SetOEMCP(CodePage:UINT):BOOL;

function GetConsoleCP:UINT; 
function SetConsoleCP(wCodePageID:UINT):BOOL; 

function GetConsoleOutputCP:UINT; 
function SetConsoleOutputCP(wCodePageID:UINT):BOOL; 

function GetCPInfo(CodePage:UINT;var lpCPInfo:TCPInfo):BOOL; 

function GetCPInfoEx(CodePage:UINT;dwFlags:DWORD;var lpCPInfoEx:CPINFOEXA):BOOL; 
function GetCPInfoExA(CodePage:UINT;dwFlags:DWORD;var lpCPInfoEx:CPINFOEXA):BOOL; 
function GetCPInfoExW(CodePage:UINT;dwFlags:DWORD;var lpCPInfoEx:CPINFOEXW):BOOL; 

//EnumSystemCodePages //To Do 

function IsValidLocale(Locale:LCID;dwFlags:DWORD):BOOL;

function GetSystemDefaultLCID:LCID; 
function GetUserDefaultLCID:LCID; 

function SetSystemDefaultLCID(Locale:LCID):BOOL;

//GetLocaleInfo //To Do 
//SetLocaleInfo //To Do 

//EnumSystemLocales //To Do 

{==============================================================================}
{RTL Unicode String Manager Functions}
function SysGetStandardCodePage(const stdcp:TStandardCodePageEnum):TSystemCodePage;

{==============================================================================}
{Locale Helper Functions}
function MapPage(CodePage:UINT):Word;
function GetPage(PageID:Word):PCodePage;
function CheckPage(Page:PCodePage):Boolean;

function LinkPage(Page:PCodePage):Boolean;
function UnlinkPage(Page:PCodePage):Boolean;

function LoadPage(PageID:Word;Table:PCodeTable;Lower:PLowerTable;Upper:PUpperTable):Boolean;
function UnloadPage(PageID:Word;Page:PCodePage):Boolean;

function DefaultTrans(PageID,TransID:Word):Boolean;
function InstallTrans(PageID:Word;Table:PTransTable):Boolean;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Locale specific variables}
 LocaleInitialized:Boolean;

 FirstPage:PCodePage;
 LastPage:PCodePage;
 
var
 {Default Code Pages}
 CPOEM437:TCodeTable = (
  MaxCharSize:1;
  DefaultChar:(
  $3F,$00);
  LeadByte:(
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00);
  Values:(
  $0000,$0001,$0002,$0003,$0004,$0005,$0006,$0007,
  $0008,$0009,$000A,$000B,$000C,$000D,$000E,$000F,
  $0010,$0011,$0012,$0013,$0014,$0015,$0016,$0017,
  $0018,$0019,$001A,$001B,$001C,$001D,$001E,$001F,
  $0020,$0021,$0022,$0023,$0024,$0025,$0026,$0027,
  $0028,$0029,$002A,$002B,$002C,$002D,$002E,$002F,
  $0030,$0031,$0032,$0033,$0034,$0035,$0036,$0037,
  $0038,$0039,$003A,$003B,$003C,$003D,$003E,$003F,
  $0040,$0041,$0042,$0043,$0044,$0045,$0046,$0047,
  $0048,$0049,$004A,$004B,$004C,$004D,$004E,$004F,
  $0050,$0051,$0052,$0053,$0054,$0055,$0056,$0057,
  $0058,$0059,$005A,$005B,$005C,$005D,$005E,$005F,
  $0060,$0061,$0062,$0063,$0064,$0065,$0066,$0067,
  $0068,$0069,$006A,$006B,$006C,$006D,$006E,$006F,
  $0070,$0071,$0072,$0073,$0074,$0075,$0076,$0077,
  $0078,$0079,$007A,$007B,$007C,$007D,$007E,$007F,
  $00C7,$00FC,$00E9,$00E2,$00E4,$00E0,$00E5,$00E7,
  $00EA,$00EB,$00E8,$00EF,$00EE,$00EC,$00C4,$00C5,
  $00C9,$00E6,$00C6,$00F4,$00F6,$00F2,$00FB,$00F9,
  $00FF,$00D6,$00DC,$00A2,$00A3,$00A5,$20A7,$0192,
  $00E1,$00ED,$00F3,$00FA,$00F1,$00D1,$00AA,$00BA,
  $00BF,$2310,$00AC,$00BD,$00BC,$00A1,$00AB,$00BB,
  $2591,$2592,$2593,$2502,$2524,$2561,$2562,$2556,
  $2555,$2563,$2551,$2557,$255D,$255C,$255B,$2510,
  $2514,$2534,$252C,$251C,$2500,$253C,$255E,$255F,
  $255A,$2554,$2569,$2566,$2560,$2550,$256C,$2567,
  $2568,$2564,$2565,$2559,$2558,$2552,$2553,$256B,
  $256A,$2518,$250C,$2588,$2584,$258C,$2590,$2580,
  $03B1,$00DF,$0393,$03C0,$03A3,$03C3,$00B5,$03C4,
  $03A6,$0398,$03A9,$03B4,$221E,$03C6,$03B5,$2229,
  $2261,$00B1,$2265,$2264,$2320,$2321,$00F7,$2248,
  $00B0,$2219,$00B7,$221A,$207F,$00B2,$25A0,$00A0)
 );
 
 CPANSI1252:TCodeTable = (
  MaxCharSize:1;
  DefaultChar:(
  $3F,$00);
  LeadByte:(
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00);
  Values:(
  $0000,$0001,$0002,$0003,$0004,$0005,$0006,$0007,
  $0008,$0009,$000A,$000B,$000C,$000D,$000E,$000F,
  $0010,$0011,$0012,$0013,$0014,$0015,$0016,$0017,
  $0018,$0019,$001A,$001B,$001C,$001D,$001E,$001F,
  $0020,$0021,$0022,$0023,$0024,$0025,$0026,$0027,
  $0028,$0029,$002A,$002B,$002C,$002D,$002E,$002F,
  $0030,$0031,$0032,$0033,$0034,$0035,$0036,$0037,
  $0038,$0039,$003A,$003B,$003C,$003D,$003E,$003F,
  $0040,$0041,$0042,$0043,$0044,$0045,$0046,$0047,
  $0048,$0049,$004A,$004B,$004C,$004D,$004E,$004F,
  $0050,$0051,$0052,$0053,$0054,$0055,$0056,$0057,
  $0058,$0059,$005A,$005B,$005C,$005D,$005E,$005F,
  $0060,$0061,$0062,$0063,$0064,$0065,$0066,$0067,
  $0068,$0069,$006A,$006B,$006C,$006D,$006E,$006F,
  $0070,$0071,$0072,$0073,$0074,$0075,$0076,$0077,
  $0078,$0079,$007A,$007B,$007C,$007D,$007E,$007F,
  $20AC,$0081,$201A,$0192,$201E,$2026,$2020,$2021,
  $02C6,$2030,$0160,$2039,$0152,$008D,$017D,$008F,
  $0090,$2018,$2019,$201C,$201D,$2022,$2013,$2014,
  $02DC,$2122,$0161,$203A,$0153,$009D,$017E,$0178,
  $00A0,$00A1,$00A2,$00A3,$00A4,$00A5,$00A6,$00A7,
  $00A8,$00A9,$00AA,$00AB,$00AC,$00AD,$00AE,$00AF,
  $00B0,$00B1,$00B2,$00B3,$00B4,$00B5,$00B6,$00B7,
  $00B8,$00B9,$00BA,$00BB,$00BC,$00BD,$00BE,$00BF,
  $00C0,$00C1,$00C2,$00C3,$00C4,$00C5,$00C6,$00C7,
  $00C8,$00C9,$00CA,$00CB,$00CC,$00CD,$00CE,$00CF,
  $00D0,$00D1,$00D2,$00D3,$00D4,$00D5,$00D6,$00D7,
  $00D8,$00D9,$00DA,$00DB,$00DC,$00DD,$00DE,$00DF,
  $00E0,$00E1,$00E2,$00E3,$00E4,$00E5,$00E6,$00E7,
  $00E8,$00E9,$00EA,$00EB,$00EC,$00ED,$00EE,$00EF,
  $00F0,$00F1,$00F2,$00F3,$00F4,$00F5,$00F6,$00F7,
  $00F8,$00F9,$00FA,$00FB,$00FC,$00FD,$00FE,$00FF)
 );

 CP1252Lower:TLowerTable = (
  LowerID:1252;
  Values:(
  $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0A,$0B,$0C,$0D,$0E,$0F,
  $10,$11,$12,$13,$14,$15,$16,$17,$18,$19,$1A,$1B,$1C,$1D,$1E,$1F,
  $20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2A,$2B,$2C,$2D,$2E,$2F,
  $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3A,$3B,$3C,$3D,$3E,$3F,
  $40,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6A,$6B,$6C,$6D,$6E,$6F,
  $70,$71,$72,$73,$74,$75,$76,$77,$78,$79,$7A,$5B,$5C,$5D,$5E,$5F,
  $60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6A,$6B,$6C,$6D,$6E,$6F,
  $70,$71,$72,$73,$74,$75,$76,$77,$78,$79,$7A,$7B,$7C,$7D,$7E,$7F,
  $80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$9A,$8B,$9C,$8D,$9E,$8F,
  $90,$91,$92,$93,$94,$95,$96,$97,$98,$99,$9A,$9B,$9C,$9D,$9E,$FF,
  $A0,$A1,$A2,$A3,$A4,$A5,$A6,$A7,$A8,$A9,$AA,$AB,$AC,$AD,$AE,$AF,
  $B0,$B1,$B2,$B3,$B4,$B5,$B6,$B7,$B8,$B9,$BA,$BB,$BC,$BD,$BE,$BF,
  $E0,$E1,$E2,$E3,$E4,$E5,$E6,$E7,$E8,$E9,$EA,$EB,$EC,$ED,$EE,$EF,
  $F0,$F1,$F2,$F3,$F4,$F5,$F6,$D7,$F8,$F9,$FA,$FB,$FC,$FD,$FE,$DF,
  $E0,$E1,$E2,$E3,$E4,$E5,$E6,$E7,$E8,$E9,$EA,$EB,$EC,$ED,$EE,$EF,
  $F0,$F1,$F2,$F3,$F4,$F5,$F6,$F7,$F8,$F9,$FA,$FB,$FC,$FD,$FE,$FF)
 );
 
 CP1252Upper:TUpperTable = (
  UpperID:1252;
  Values:(
  $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0A,$0B,$0C,$0D,$0E,$0F,
  $10,$11,$12,$13,$14,$15,$16,$17,$18,$19,$1A,$1B,$1C,$1D,$1E,$1F,
  $20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2A,$2B,$2C,$2D,$2E,$2F,
  $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3A,$3B,$3C,$3D,$3E,$3F,
  $40,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4A,$4B,$4C,$4D,$4E,$4F,
  $50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5A,$5B,$5C,$5D,$5E,$5F,
  $60,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4A,$4B,$4C,$4D,$4E,$4F,
  $50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5A,$7B,$7C,$7D,$7E,$7F,
  $80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8A,$8B,$8C,$8D,$8E,$8F,
  $90,$91,$92,$93,$94,$95,$96,$97,$98,$99,$8A,$9B,$8C,$9D,$8E,$9F,
  $A0,$A1,$A2,$A3,$A4,$A5,$A6,$A7,$A8,$A9,$AA,$AB,$AC,$AD,$AE,$AF,
  $B0,$B1,$B2,$B3,$B4,$B5,$B6,$B7,$B8,$B9,$BA,$BB,$BC,$BD,$BE,$BF,
  $C0,$C1,$C2,$C3,$C4,$C5,$C6,$C7,$C8,$C9,$CA,$CB,$CC,$CD,$CE,$CF,
  $D0,$D1,$D2,$D3,$D4,$D5,$D6,$D7,$D8,$D9,$DA,$DB,$DC,$DD,$DE,$DF,
  $C0,$C1,$C2,$C3,$C4,$C5,$C6,$C7,$C8,$C9,$CA,$CB,$CC,$CD,$CE,$CF,
  $D0,$D1,$D2,$D3,$D4,$D5,$D6,$F7,$D8,$D9,$DA,$DB,$DC,$DD,$DE,$9F)
 );
 
 CP437TO1252:TTransTable = (
  TransID:1252;
  Values:(
  $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0A,$0B,$0C,$0D,$0E,$A4,
  $10,$11,$12,$13,$B6,$A7,$16,$17,$18,$19,$1A,$1B,$1C,$1D,$1E,$1F,
  $20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2A,$2B,$2C,$2D,$2E,$2F,
  $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3A,$3B,$3C,$3D,$3E,$3F,
  $40,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4A,$4B,$4C,$4D,$4E,$4F,
  $50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5A,$5B,$5C,$5D,$5E,$5F,
  $60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6A,$6B,$6C,$6D,$6E,$6F,
  $70,$71,$72,$73,$74,$75,$76,$77,$78,$79,$7A,$7B,$7C,$7D,$7E,$7F,
  $C7,$FC,$E9,$E2,$E4,$E0,$E5,$E7,$EA,$EB,$E8,$EF,$EE,$EC,$C4,$C5,
  $C9,$E6,$C6,$F4,$F6,$F2,$FB,$F9,$FF,$D6,$DC,$A2,$A3,$A5,$50,$83,
  $E1,$ED,$F3,$FA,$F1,$D1,$AA,$BA,$BF,$AC,$AC,$BD,$BC,$A1,$AB,$BB,
  $A6,$A6,$A6,$A6,$A6,$A6,$A6,$2B,$2B,$A6,$A6,$2B,$2B,$2B,$2B,$2B,
  $2B,$2D,$2D,$2B,$2D,$2B,$A6,$A6,$2B,$2B,$2D,$2D,$A6,$2D,$2B,$2D,
  $2D,$2D,$2D,$2B,$2B,$2B,$2B,$2B,$2B,$2B,$2B,$A6,$5F,$A6,$A6,$AF,
  $61,$DF,$47,$70,$53,$73,$B5,$74,$46,$54,$4F,$64,$38,$66,$65,$6E,
  $3D,$B1,$3D,$3D,$28,$29,$F7,$98,$B0,$B7,$B7,$76,$6E,$B2,$A6,$A0)
 );
 
 CP1252TO437:TTransTable = (
  TransID:437;
  Values:(
  $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0A,$0B,$0C,$0D,$0E,$0F,
  $10,$11,$12,$13,$14,$15,$16,$17,$18,$19,$1A,$1B,$1C,$1D,$1E,$1F,
  $20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2A,$2B,$2C,$2D,$2E,$2F,
  $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3A,$3B,$3C,$3D,$3E,$3F,
  $40,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4A,$4B,$4C,$4D,$4E,$4F,
  $50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5A,$5B,$5C,$5D,$5E,$5F,
  $60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6A,$6B,$6C,$6D,$6E,$6F,
  $70,$71,$72,$73,$74,$75,$76,$77,$78,$79,$7A,$7B,$7C,$7D,$7E,$7F,
  $5F,$5F,$2C,$9F,$2C,$2E,$2B,$D8,$5E,$25,$53,$3C,$4F,$5F,$5A,$5F,
  $5F,$60,$27,$22,$22,$07,$2D,$2D,$7E,$54,$73,$3E,$6F,$5F,$7A,$59,
  $FF,$AD,$9B,$9C,$0F,$9D,$DD,$15,$22,$63,$A6,$AE,$AA,$2D,$72,$5F,
  $F8,$F1,$FD,$33,$27,$E6,$14,$FA,$2C,$31,$A7,$AF,$AC,$AB,$5F,$A8,
  $41,$41,$41,$41,$8E,$8F,$92,$80,$45,$90,$45,$45,$49,$49,$49,$49,
  $44,$A5,$4F,$4F,$4F,$4F,$99,$78,$4F,$55,$55,$55,$9A,$59,$5F,$E1,
  $85,$A0,$83,$61,$84,$86,$91,$87,$8A,$82,$88,$89,$8D,$A1,$8C,$8B,
  $64,$A4,$95,$A2,$93,$6F,$94,$F6,$6F,$97,$A3,$96,$81,$79,$5F,$98)
 );

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure LocaleInit;
begin
 {}
 {Check Initialized}
 if LocaleInitialized then Exit;

 {Initialize CodePage Lock}
 CodePageLock.Lock:=INVALID_HANDLE_VALUE;
 CodePageLock.AcquireLock:=nil;
 CodePageLock.ReleaseLock:=nil;
 
 {Setup Default Country}
 //COUNTRY_DEFAULT:= //To Do
 
 {Setup Default Code Pages}
 CODEPAGE_OEM_DEFAULT:=CP_OEM_437;
 CODEPAGE_ANSI_DEFAULT:=CP_ANSI_1252;
 CODEPAGE_CONSOLE_INPUT:=CODEPAGE_ANSI_DEFAULT;
 CODEPAGE_CONSOLE_OUTPUT:=CODEPAGE_ANSI_DEFAULT;
 
 {Setup Default Locale}
 //LOCALE_DEFAULT:= //To Do
 
 {Load Default Code Pages}
 LoadPage(CP_OEM_437,@CPOEM437,nil,nil);
 LoadPage(CP_ANSI_1252,@CPANSI1252,@CP1252Lower,@CP1252Upper);
 InstallTrans(CP_OEM_437,@CP437TO1252);
 InstallTrans(CP_ANSI_1252,@CP1252TO437);
 
 {Get Default Code Pages}
 OemPage:=GetPage(CP_OEM_437);
 AnsiPage:=GetPage(CP_ANSI_1252);
 DefaultPage:=GetPage(CP_ANSI_1252);
 
 {Initialize Code Page Defaults}
 DefaultSystemCodePage:=GetACP;
 DefaultUnicodeCodePage:=CP_UTF16;
 DefaultFileSystemCodePage:=DefaultSystemCodePage; //CP_UTF8; //To Do //When filesystem APIs support native Unicode calling
 DefaultRTLFileSystemCodePage:=DefaultSystemCodePage;
 
 LocaleInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Locale Functions}
function IsValidCodePage(CodePage:UINT):BOOL; 
var
 PageID:Word;
begin
 {Map Page}
 PageID:=MapPage(CodePage);
 
 {Get Page}
 Result:=(GetPage(PageID) <> nil);
end;

{==============================================================================}

function GetACP:UINT; 
begin
 {}
 Result:=CODEPAGE_ANSI_DEFAULT;
end;

{==============================================================================}

function GetOEMCP:UINT; 
begin
 {}
 Result:=CODEPAGE_OEM_DEFAULT;
end;

{==============================================================================}

function SetACP(CodePage:UINT):BOOL;
var
 AnsiID:Word;
 Ansi:PCodePage;
 OemID:Word;
 Oem:PCodePage;
begin
 {}
 Result:=False;
 
 {Map Page}
 AnsiID:=MapPage(CodePage);
 
 {Get Page}
 Ansi:=GetPage(AnsiID);
 if Ansi = nil then Exit;

 {Check Page}
 if AnsiID <> CODEPAGE_ANSI_DEFAULT then
  begin
   {Check ANSI Page}
   case AnsiID of
    CP_ANSI_1250,CP_ANSI_1251,CP_ANSI_1252,CP_ANSI_1253,CP_ANSI_1254,
    CP_ANSI_1255,CP_ANSI_1256,CP_ANSI_1257,CP_ANSI_1258,CP_ANSI_874:begin
      {Get OEM ID}
      OemID:=Ansi.TransTable.TransID;
       
      {Get OEM Page}
      Oem:=GetPage(OemID);
      if Oem = nil then Exit;
    
      {Update Code Page Defaults}
      if DefaultSystemCodePage = CODEPAGE_ANSI_DEFAULT then DefaultSystemCodePage:=AnsiID;
      if DefaultFileSystemCodePage = CODEPAGE_ANSI_DEFAULT then DefaultFileSystemCodePage:=AnsiID;
      if DefaultRTLFileSystemCodePage = CODEPAGE_ANSI_DEFAULT then DefaultRTLFileSystemCodePage:=AnsiID;

      {Set Default Code Pages}
      OemPage:=Oem;
      AnsiPage:=Ansi;
      DefaultPage:=Ansi;
      
      {Update Default Code Pages}
      if CODEPAGE_CONSOLE_INPUT = CODEPAGE_ANSI_DEFAULT then CODEPAGE_CONSOLE_INPUT:=AnsiID;
      if CODEPAGE_CONSOLE_OUTPUT = CODEPAGE_ANSI_DEFAULT then CODEPAGE_CONSOLE_OUTPUT:=AnsiID;
      CODEPAGE_OEM_DEFAULT:=OemID;
      CODEPAGE_ANSI_DEFAULT:=AnsiID;
      
      Result:=True;
     end;
   end;
  end
 else
  begin
   Result:=True;
  end;  
end;

{==============================================================================}

function SetOEMCP(CodePage:UINT):BOOL;
var
 OemID:Word;
 Oem:PCodePage;
 AnsiID:Word;
 Ansi:PCodePage;
begin
 {}
 Result:=False;
 
 {Map Page}
 OemID:=MapPage(CodePage);
 
 {Get Page}
 Oem:=GetPage(OemID);
 if Oem = nil then Exit;

 {Check Page}
 if OemID <> CODEPAGE_OEM_DEFAULT then
  begin
   {Check OEM Page}
   case OemID of
    CP_OEM_437,CP_OEM_720,CP_OEM_737,CP_OEM_775,CP_OEM_850,CP_OEM_852,
    CP_OEM_857,CP_OEM_862,CP_OEM_866,CP_OEM_874,CP_OEM_1258:begin
      {Get ANSI ID}
      AnsiID:=Oem.TransTable.TransID;
       
      {Get ANSI Page}
      Ansi:=GetPage(AnsiID);
      if Ansi = nil then Exit;
   
      {Update Code Page Defaults}
      if DefaultSystemCodePage = CODEPAGE_ANSI_DEFAULT then DefaultSystemCodePage:=AnsiID;
      if DefaultFileSystemCodePage = CODEPAGE_ANSI_DEFAULT then DefaultFileSystemCodePage:=AnsiID;
      if DefaultRTLFileSystemCodePage = CODEPAGE_ANSI_DEFAULT then DefaultRTLFileSystemCodePage:=AnsiID;

      {Set Default Code Pages}
      OemPage:=Oem;
      AnsiPage:=Ansi;
      DefaultPage:=Ansi;
      
      {Update Default Code Pages}
      if CODEPAGE_CONSOLE_INPUT = CODEPAGE_ANSI_DEFAULT then CODEPAGE_CONSOLE_INPUT:=AnsiID;
      if CODEPAGE_CONSOLE_OUTPUT = CODEPAGE_ANSI_DEFAULT then CODEPAGE_CONSOLE_OUTPUT:=AnsiID;
      CODEPAGE_OEM_DEFAULT:=OemID;
      CODEPAGE_ANSI_DEFAULT:=AnsiID;
   
      {Check translation}
      if OemID = CP_OEM_437 then
       begin
        {Reinstall CP1252 to CP437 Trans table}
        InstallTrans(CP_ANSI_1252,@CP1252TO437);
       end
      else if OemID = CP_OEM_850 then 
       begin
        {Reinstall CP1252 to CP850 Trans table}
        //InstallTrans(CP_ANSI_1252,@CP1252TO850); //To Do //Move CP1252TO850 to this unit ?
       end;
       
      Result:=True;
     end;
   end;
  end
 else
  begin
   Result:=True;
  end;  
end;

{==============================================================================}

function GetConsoleCP:UINT; 
begin
 {}
 Result:=CODEPAGE_CONSOLE_INPUT;
end;

{==============================================================================}

function SetConsoleCP(wCodePageID:UINT):BOOL; 
begin
 {}
 Result:=IsValidCodePage(wCodePageID);
 if Result then
  begin
   CODEPAGE_CONSOLE_INPUT:=wCodePageID;
  end;
end;

{==============================================================================}

function GetConsoleOutputCP:UINT; 
begin
 {}
 Result:=CODEPAGE_CONSOLE_OUTPUT;
end;

{==============================================================================}

function SetConsoleOutputCP(wCodePageID:UINT):BOOL; 
begin
 {}
 Result:=IsValidCodePage(wCodePageID);
 if Result then
  begin
   CODEPAGE_CONSOLE_OUTPUT:=wCodePageID;
  end;
end;

{==============================================================================}

function GetCPInfo(CodePage:UINT;var lpCPInfo:TCPInfo):BOOL; 
var
 PageID:Word;
 Page:PCodePage;
begin
 {}
 Result:=False;
 
 {Map Page}
 PageID:=MapPage(CodePage);
 
 {Get Page}
 Page:=GetPage(PageID);
 if Page = nil then Exit;
 
 {Get Info}
 lpCPInfo.MaxCharSize:=Page.CodeTable.MaxCharSize;
 System.Move(Page.CodeTable.DefaultChar[0],lpCPInfo.DefaultChar[0],MAX_DEFAULTCHAR);
 System.Move(Page.CodeTable.LeadByte[0],lpCPInfo.LeadByte[0],MAX_LEADBYTES);
 
 Result:=True;
end;

{==============================================================================}

function GetCPInfoEx(CodePage:UINT;dwFlags:DWORD;var lpCPInfoEx:CPINFOEXA):BOOL; 
begin
 {}
 Result:=GetCPInfoExA(CodePage,dwFlags,lpCPInfoEx);
end;

{==============================================================================}

function GetCPInfoExA(CodePage:UINT;dwFlags:DWORD;var lpCPInfoEx:CPINFOEXA):BOOL; 
var
 PageID:Word;
 Page:PCodePage;
begin
 {}
 Result:=False;
 
 {Map Page}
 PageID:=MapPage(CodePage);
 
 {Get Page}
 Page:=GetPage(PageID);
 if Page = nil then Exit;
 
 {Get Info Ex}
 lpCPInfoEx.MaxCharSize:=Page.CodeTable.MaxCharSize;
 System.Move(Page.CodeTable.DefaultChar[0],lpCPInfoEx.DefaultChar[0],MAX_DEFAULTCHAR);
 System.Move(Page.CodeTable.LeadByte[0],lpCPInfoEx.LeadByte[0],MAX_LEADBYTES);
 //To Do //UnicodeDefaultChar
 lpCPInfoEx.CodePage:=CodePage;
 //To Do //CodePageName
 
 Result:=True;
end;
 
{==============================================================================}

function GetCPInfoExW(CodePage:UINT;dwFlags:DWORD;var lpCPInfoEx:CPINFOEXW):BOOL; 
var
 PageID:Word;
 Page:PCodePage;
begin
 {}
 Result:=False;
 
 {Map Page}
 PageID:=MapPage(CodePage);
 
 {Get Page}
 Page:=GetPage(PageID);
 if Page = nil then Exit;
 
 {Get Info Ex}
 lpCPInfoEx.MaxCharSize:=Page.CodeTable.MaxCharSize;
 System.Move(Page.CodeTable.DefaultChar[0],lpCPInfoEx.DefaultChar[0],MAX_DEFAULTCHAR);
 System.Move(Page.CodeTable.LeadByte[0],lpCPInfoEx.LeadByte[0],MAX_LEADBYTES);
 //To Do //UnicodeDefaultChar
 lpCPInfoEx.CodePage:=CodePage;
 //To Do //CodePageName
 
 Result:=True;
end;

{==============================================================================}

function IsValidLocale(Locale:LCID;dwFlags:DWORD):BOOL;
begin
 {}
 Result:=False;
 
 //To Do 
 
 Result:=True;
end;

{==============================================================================}

function GetSystemDefaultLCID:LCID; 
begin
 {}
 Result:=LOCALE_DEFAULT;
end;

{==============================================================================}

function GetUserDefaultLCID:LCID; 
begin
 {}
 Result:=LOCALE_DEFAULT;
end;

{==============================================================================}

function SetSystemDefaultLCID(Locale:LCID):BOOL;
begin
 {}
 Result:=False;
 
 LOCALE_DEFAULT:=Locale; //To Do 

 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{RTL Unicode String Manager Functions}
function SysGetStandardCodePage(const stdcp:TStandardCodePageEnum):TSystemCodePage;
begin
 {}
 case stdcp of
  scpAnsi,scpFileSystemSingleByte:Result:=GetACP;
  scpConsoleInput:Result:=GetConsoleCP;
  scpConsoleOutput:Result:=GetConsoleOutputCP;
 end;
end;

{==============================================================================}
{==============================================================================}
{Locale Helper Functions}
function MapPage(CodePage:UINT):Word;
{Map a default code page to the actual current page}
begin
 {}
 Result:=CodePage;
 
 case CodePage of
  CP_ACP:Result:=CODEPAGE_ANSI_DEFAULT;
  CP_OEMCP:Result:=CODEPAGE_OEM_DEFAULT;
  CP_MACCP:Result:=CODEPAGE_ANSI_DEFAULT;  //To Do ??       
  CP_THREAD_ACP:Result:=CODEPAGE_ANSI_DEFAULT; //To Do ??
  CP_SYMBOL:Result:=CODEPAGE_OEM_DEFAULT; //To Do ??
  CP_UTF7:Result:=CODEPAGE_OEM_DEFAULT;  //To Do ??        
  CP_UTF8:Result:=CODEPAGE_OEM_DEFAULT;  //To Do ??        
  CP_NONE:Result:=CODEPAGE_ANSI_DEFAULT; //To Do ??
 end;
end;

{==============================================================================}

function GetPage(PageID:Word):PCodePage;
{Find the requested page in the linked list}
var
 NextPage:PCodePage;
begin
 {}
 Result:=nil;
 
 {Acquire Lock}
 if CodePageLock.Lock <> INVALID_HANDLE_VALUE then CodePageLock.AcquireLock(CodePageLock.Lock);
 try 
  NextPage:=FirstPage;
  while NextPage <> nil do
   begin
    if NextPage.PageID = PageID then
     begin
      Result:=NextPage;
      Exit;
     end;
    
    NextPage:=NextPage.NextPage;
   end;
 finally
  {Release Lock}
  if CodePageLock.Lock <> INVALID_HANDLE_VALUE then CodePageLock.ReleaseLock(CodePageLock.Lock);
 end;
end;

{==============================================================================}

function CheckPage(Page:PCodePage):Boolean;
{Check that the Page supplied is part of the Linked list}
var
 NextPage:PCodePage;
begin
 {}
 Result:=True;
 
 {Acquire Lock}
 if CodePageLock.Lock <> INVALID_HANDLE_VALUE then CodePageLock.AcquireLock(CodePageLock.Lock);
 try 
  NextPage:=FirstPage;
  while NextPage <> nil do
   begin
    if NextPage = Page then
     begin
      Exit;
     end;
    
    NextPage:=NextPage.NextPage;
   end;
  
  Result:=False;
 finally
  {Release Lock}
  if CodePageLock.Lock <> INVALID_HANDLE_VALUE then CodePageLock.ReleaseLock(CodePageLock.Lock);
 end;
end;

{==============================================================================}

function LinkPage(Page:PCodePage):Boolean;
{Link Page to Prev,Next Siblings and Adjust First/Last}
var
 PrevPage:PCodePage;
begin
 {}
 Result:=False;
 
 {Acquire Lock}
 if CodePageLock.Lock <> INVALID_HANDLE_VALUE then CodePageLock.AcquireLock(CodePageLock.Lock);
 try 
  if Page = nil then Exit;
 
  PrevPage:=LastPage;
  if PrevPage = nil then
   begin
    {Is First Object}
    Page.PrevPage:=nil;
    Page.NextPage:=nil;
    FirstPage:=Page;
    LastPage:=Page;
    
    Result:=True;
   end
  else
   begin
    {Not First Object}
    PrevPage.NextPage:=Page;
    Page.PrevPage:=PrevPage;
    Page.NextPage:=nil;
    LastPage:=Page;
    
    Result:=True;
   end;
 finally
  {Release Lock}
  if CodePageLock.Lock <> INVALID_HANDLE_VALUE then CodePageLock.ReleaseLock(CodePageLock.Lock);
 end;
end;

{==============================================================================}

function UnlinkPage(Page:PCodePage):Boolean;
{Unlink Page from Prev,Next Siblings and Adjust First/Last}
var
 PrevPage,NextPage:PCodePage;
begin
 {}
 Result:=False;
 
 {Acquire Lock}
 if CodePageLock.Lock <> INVALID_HANDLE_VALUE then CodePageLock.AcquireLock(CodePageLock.Lock);
 try 
  if Page = nil then Exit;
  
  if Page.PrevPage <> nil then
   begin
    {Not First Object}
    PrevPage:=Page.PrevPage;
    if Page.NextPage <> nil then
     begin
      {Not Last Object}
      NextPage:=Page.NextPage;
      PrevPage.NextPage:=NextPage;
      NextPage.PrevPage:=PrevPage;
     end
    else
     begin
      {Is Last Object}
      PrevPage.NextPage:=nil;
      LastPage:=PrevPage;
     end;
   end
  else
   begin
    {Is First Object}
    if Page.NextPage <> nil then
     begin
      {Not Last Object}
      NextPage:=Page.NextPage;
      NextPage.PrevPage:=nil;
      FirstPage:=NextPage;
     end
    else
     begin
      {Is Last Object}
      FirstPage:=nil;
      LastPage:=nil;
     end;
   end;
  Page.PrevPage:=nil;
  Page.NextPage:=nil;
  
  Result:=True;
 finally
  {Release Lock}
  if CodePageLock.Lock <> INVALID_HANDLE_VALUE then CodePageLock.ReleaseLock(CodePageLock.Lock);
 end;
end;

{==============================================================================}

function LoadPage(PageID:Word;Table:PCodeTable;Lower:PLowerTable;Upper:PUpperTable):Boolean;
{Load a code table and allocate memory to create a code page}
var
 Count:Word;
 Page:PCodePage;
begin
 {}
 Result:=False;
 
 if Table = nil then Exit; {Lower and Upper may be nil}
 if GetPage(PageID) <> nil then Exit;

 {Create Page}
 Page:=AllocMem(SizeOf(TCodePage));
 if Page = nil then Exit;
 
 {Load Code Table}
 Page.PageID:=PageID;
 Page.Handle:=INVALID_HANDLE_VALUE;
 Page.CodeTable:=Table;

 {Load Lower Table}
 Page.LowerTable:=Lower;
 
 {Load Upper Table}
 Page.UpperTable:=Upper;
 
 {Create Lead Bytes}
 Page.LeadBytes:=AllocMem(SizeOf(TLeadBytes));
 if Page.LeadBytes = nil then Exit;

 {Create Trans Table}
 Page.TransTable:=AllocMem(SizeOf(TTransTable));
 if Page.TransTable = nil then Exit;

 {Create Unicode Table}
 Page.UnicodeTable:=AllocMem(SizeOf(TUnicodeTable));
 if Page.UnicodeTable = nil then Exit;
 
 {Load Default Chars}
 for Count:=$0000 to $FFFF do
  begin
   Page.UnicodeTable.Values[Count]:=Word(Page.CodeTable.DefaultChar);
  end;
 
 {Load Unicode Table}
 for Count:=$00 to $FF do
  begin
   if (Page.CodeTable.Values[Count] <> 0) or (Count = 0) then
    begin
     {Set the Table value in the Unicode Table}
     Page.UnicodeTable.Values[Page.CodeTable.Values[Count]]:=Count;
    end
   else
    begin
     {Set Default Char for any zero values in Table}
     Page.CodeTable.Values[Count]:=Word(Page.CodeTable.DefaultChar);
    end;
  end;

 {Link Page}
 Result:=LinkPage(Page);
end;

{==============================================================================}

function UnloadPage(PageID:Word;Page:PCodePage):Boolean;
{Unload a code page and release allocated memory}
begin
 {}
 Result:=False;
 
 if Page = nil then Exit;
 if GetPage(PageID) = nil then Exit;
 if not CheckPage(Page) then Exit;

 {Unlink Page}
 if not UnlinkPage(Page) then Exit;
 
 {Free Unicode Table}
 FreeMem(Page.UnicodeTable);
 
 {Free Trans Table}
 FreeMem(Page.TransTable);
 
 {Free Lead Bytes}
 FreeMem(Page.LeadBytes);
 
 {Free Page}
 FreeMem(Page);
 
 {Check Oem}
 if PageID = CODEPAGE_OEM_DEFAULT then CODEPAGE_OEM_DEFAULT:=CP_OEM_437;
 if not CheckPage(OemPage) then OemPage:=GetPage(CP_OEM_437);
 
 {Check Ansi}
 if PageID = CODEPAGE_ANSI_DEFAULT then CODEPAGE_ANSI_DEFAULT:=CP_ANSI_1252;
 if not CheckPage(AnsiPage) then AnsiPage:=GetPage(CP_ANSI_1252);
 
 {Check Default}
 if not CheckPage(DefaultPage) then DefaultPage:=GetPage(CP_ANSI_1252);
 
 Result:=True;
end;

{==============================================================================}

function DefaultTrans(PageID,TransID:Word):Boolean;
var
 Count:Word;
 Value:Word;
 Page:PCodePage;
 TransPage:PCodePage;
begin
 {}
 Result:=False;
 
 {Get Page}
 Page:=GetPage(PageID);
 if Page = nil then Exit;
 
 {Get Trans Page}
 TransPage:=GetPage(TransID);
 if TransPage =  nil then Exit;
 
 {Load Trans}
 Page.TransTable.TransID:=TransPage.PageID;
 for Count:=$00 to $FF do
  begin
   {Get Value}
   Value:=Page.CodeTable.Values[Count];
   
   {Set Value}
   Page.TransTable.Values[Count]:=TransPage.UnicodeTable.Values[Value];
  end;
  
 Result:=True;
end;

{==============================================================================}

function InstallTrans(PageID:Word;Table:PTransTable):Boolean;
var
 Count:Word;
 Page:PCodePage;
begin
 {}
 Result:=False;
 
 if Table = nil then Exit;

 {Get Page}
 Page:=GetPage(PageID);
 if Page = nil then Exit;
 
 {Load Trans}
 Page.TransTable.TransID:=Table.TransID;
 for Count:=$00 to $FF do
  begin
   Page.TransTable.Values[Count]:=Table.Values[Count];
  end;
  
 Result:=True;
end;

{==============================================================================}
{==============================================================================}

initialization
 LocaleInit;
 
{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
