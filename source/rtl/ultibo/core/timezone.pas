{
Ultibo Timezone interface unit.

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


References
==========


Timezones
=========

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Timezone;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {Timezone specific constants}
 TIME_ZONE_ID_UNKNOWN  = 0;
 TIME_ZONE_ID_STANDARD = 1;
 TIME_ZONE_ID_DAYLIGHT = 2;
 TIME_ZONE_ID_INVALID  = DWORD($FFFFFFFF);

 {Timezone Signature}
 TIMEZONE_SIGNATURE = $ED9A1BC3;

 {Timezone name constants}
 TIMEZONE_NAME_LENGTH = SIZE_64;   {Length of timezone name}
 TIMEZONE_DESC_LENGTH = SIZE_128;  {Length of timezone description}

{==============================================================================}
type
 {Timezone specific types}

 {System Time types}
 LPSYSTEMTIME = SysUtils.LPSYSTEMTIME;
 _SYSTEMTIME = SysUtils.SYSTEMTIME;
 {_SYSTEMTIME = record
   wYear:Word;
   wMonth:Word;
   wDayOfWeek:Word;
   wDay:Word;
   wHour:Word;
   wMinute:Word;
   wSecond:Word;
   wMilliseconds:Word;
 end;} {SYSTEMTIME is now defined in SysUtils}
 SYSTEMTIME = SysUtils.SYSTEMTIME;
 TSystemTime = SysUtils.TSystemTime;
 PSystemTime = SysUtils.PSystemTime;

 {Timezone types}
 PTIME_ZONE_INFORMATION = ^TIME_ZONE_INFORMATION;
 _TIME_ZONE_INFORMATION = record
   Bias:LONG;
   StandardName:array [0..31] of WCHAR;
   StandardDate:SYSTEMTIME;
   StandardBias:LONG;
   DaylightName:array [0..31] of WCHAR;
   DaylightDate:SYSTEMTIME;
   DaylightBias:LONG;
 end;
 TIME_ZONE_INFORMATION = _TIME_ZONE_INFORMATION;
 LPTIME_ZONE_INFORMATION = ^TIME_ZONE_INFORMATION;
 TTimeZoneInformation = TIME_ZONE_INFORMATION;
 PTimeZoneInformation = PTIME_ZONE_INFORMATION;

 {Timezone Data}
 PTimezoneData = ^TTimezoneData;
 TTimezoneData = record
  Name:String[32];
  Description:String[64];
  Bias:LongInt;
  StandardName:String[32];
  StandardBias:LongInt;
  StandardStart:SYSTEMTIME;
  DaylightName:String[32];
  DaylightBias:LongInt;
  DaylightStart:SYSTEMTIME;
 end;

 PTimezoneEntry = ^TTimezoneEntry;

 {Timezone Enumeration Callback}
 TTimezoneEnumerate = function(Timezone:PTimezoneEntry;Data:Pointer):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 {Timezone Entry}
 TTimezoneEntry = record
  {Timezone Properties}
  Signature:LongWord;            {Signature for entry validation}
  Name:array[0..TIMEZONE_NAME_LENGTH - 1] of Char; {Timezone name}
  Description:array[0..TIMEZONE_DESC_LENGTH - 1] of Char; {Timezone description}
  Bias:LongInt;
  StandardName:array[0..TIMEZONE_NAME_LENGTH - 1] of Char; {Standard name}
  StandardBias:LongInt;
  StandardStart:SYSTEMTIME;
  DaylightName:array[0..TIMEZONE_NAME_LENGTH - 1] of Char; {Daylight name}
  DaylightBias:LongInt;
  DaylightStart:SYSTEMTIME;
  {Internal Properties}
  Prev:PTimezoneEntry;           {Previous entry in Timezone table}
  Next:PTimezoneEntry;           {Next entry in Timezone table}
 end;

{==============================================================================}
{var}
 {Timezone specific variables}

{==============================================================================}
{Initialization Functions}
procedure TimezoneInit;

{==============================================================================}
{Timezone Functions}
function TimezoneAdd(Data:PTimezoneData;Default:Boolean):LongWord;
function TimezoneDelete(Timezone:PTimezoneEntry):LongWord;

function TimezoneGetName(Timezone:PTimezoneEntry):String;
function TimezoneGetDescription(Timezone:PTimezoneEntry):String;

function TimezoneGetBias(Timezone:PTimezoneEntry):LongInt;
function TimezoneGetState(Timezone:PTimezoneEntry):LongWord; inline;
function TimezoneGetStateEx(Timezone:PTimezoneEntry;const DateTime:TDateTime):LongWord;
function TimezoneGetActiveBias(Timezone:PTimezoneEntry):LongInt; inline;
function TimezoneGetActiveBiasEx(Timezone:PTimezoneEntry;const DateTime:TDateTime):LongInt;

function TimezoneGetStandardName(Timezone:PTimezoneEntry):String;
function TimezoneGetStandardBias(Timezone:PTimezoneEntry):LongInt;
function TimezoneGetStandardDate(Timezone:PTimezoneEntry;Next:Boolean):TDateTime;
function TimezoneGetStandardStart(Timezone:PTimezoneEntry):SYSTEMTIME;

function TimezoneGetDaylightName(Timezone:PTimezoneEntry):String;
function TimezoneGetDaylightBias(Timezone:PTimezoneEntry):LongInt;
function TimezoneGetDaylightDate(Timezone:PTimezoneEntry;Next:Boolean):TDateTime;
function TimezoneGetDaylightStart(Timezone:PTimezoneEntry):SYSTEMTIME;

function TimezoneFind(const Name:String):PTimezoneEntry;
function TimezoneFindByStandard(const StandardName:String):PTimezoneEntry;
function TimezoneFindByDaylight(const DaylightName:String):PTimezoneEntry;

function TimezoneEnumerate(Callback:TTimezoneEnumerate;Data:Pointer):LongWord;

{==============================================================================}
{Timezone Helper Functions}
function TimezoneGetCount:LongWord;
function TimezoneGetDefault:PTimezoneEntry;
function TimezoneSetDefault(Timezone:PTimezoneEntry):LongWord;

function TimezoneCheck(Timezone:PTimezoneEntry):PTimezoneEntry;

function TimezoneUpdateOffset:LongWord;
function TimezoneUpdateEnvironment:LongWord;

function TimezoneCalculateOffset(const DateTime:TDateTime;var Offset:LongInt;var Daylight:Boolean):LongWord;

function TimezoneStartToDateTime(const AStart:SYSTEMTIME;AYear:Word):TDateTime;
function TimezoneStartToDescription(const AStart:SYSTEMTIME):String;

function TimezoneNameReplaceChar(const AName:String;AChar,AReplace:Char):String;
function TimezoneNameToAbbreviation(const AName:String):String;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Timezone specific variables}
 TimezoneInitialized:Boolean;

 TimezoneTable:PTimezoneEntry;
 TimezoneTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 TimezoneTableCount:LongWord;

 TimezoneDefault:PTimezoneEntry;

 {Begin timezone builder data}
const
 {Timezone count}
 TIMEZONE_COUNT = 141;

type
 {Timezone List}
 PTimezoneList = ^TTimezoneList;
 TTimezoneList = record
  TimezoneCount:LongWord;
  TimezoneData:array[0..(TIMEZONE_COUNT - 1)] of TTimezoneData;
 end;

var
 {Timezone List}
 TimezoneList:TTimezoneList = (
  TimezoneCount:141;
  TimezoneData:(

   {Afghanistan Standard Time}
   (Name:('Afghanistan Standard Time');
    Description:('(UTC+04:30) Kabul');
    Bias:-270;
    StandardName:('Afghanistan Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Afghanistan Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Alaskan Standard Time}
   (Name:('Alaskan Standard Time');
    Description:('(UTC-09:00) Alaska');
    Bias:540;
    StandardName:('Alaskan Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:11;wDayOfWeek:0;wDay:1;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Alaskan Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:3;wDayOfWeek:0;wDay:2;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Aleutian Standard Time}
   (Name:('Aleutian Standard Time');
    Description:('(UTC-10:00) Aleutian Islands');
    Bias:600;
    StandardName:('Aleutian Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:11;wDayOfWeek:0;wDay:1;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Aleutian Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:3;wDayOfWeek:0;wDay:2;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Altai Standard Time}
   (Name:('Altai Standard Time');
    Description:('(UTC+07:00) Barnaul, Gorno-Altaysk');
    Bias:-420;
    StandardName:('Altai Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Altai Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Arab Standard Time}
   (Name:('Arab Standard Time');
    Description:('(UTC+03:00) Kuwait, Riyadh');
    Bias:-180;
    StandardName:('Arab Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Arab Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Arabian Standard Time}
   (Name:('Arabian Standard Time');
    Description:('(UTC+04:00) Abu Dhabi, Muscat');
    Bias:-240;
    StandardName:('Arabian Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Arabian Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Arabic Standard Time}
   (Name:('Arabic Standard Time');
    Description:('(UTC+03:00) Baghdad');
    Bias:-180;
    StandardName:('Arabic Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Arabic Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Argentina Standard Time}
   (Name:('Argentina Standard Time');
    Description:('(UTC-03:00) City of Buenos Aires');
    Bias:180;
    StandardName:('Argentina Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Argentina Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Astrakhan Standard Time}
   (Name:('Astrakhan Standard Time');
    Description:('(UTC+04:00) Astrakhan, Ulyanovsk');
    Bias:-240;
    StandardName:('Astrakhan Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Astrakhan Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Atlantic Standard Time}
   (Name:('Atlantic Standard Time');
    Description:('(UTC-04:00) Atlantic Time (Canada)');
    Bias:240;
    StandardName:('Atlantic Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:11;wDayOfWeek:0;wDay:1;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Atlantic Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:3;wDayOfWeek:0;wDay:2;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {AUS Central Standard Time}
   (Name:('AUS Central Standard Time');
    Description:('(UTC+09:30) Darwin');
    Bias:-570;
    StandardName:('AUS Central Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('AUS Central Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Aus Central W. Standard Time}
   (Name:('Aus Central W. Standard Time');
    Description:('(UTC+08:45) Eucla');
    Bias:-525;
    StandardName:('Aus Central W. Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Aus Central W. Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {AUS Eastern Standard Time}
   (Name:('AUS Eastern Standard Time');
    Description:('(UTC+10:00) Canberra, Melbourne, Sydney');
    Bias:-600;
    StandardName:('AUS Eastern Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:4;wDayOfWeek:0;wDay:1;wHour:3;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('AUS Eastern Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:10;wDayOfWeek:0;wDay:1;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Azerbaijan Standard Time}
   (Name:('Azerbaijan Standard Time');
    Description:('(UTC+04:00) Baku');
    Bias:-240;
    StandardName:('Azerbaijan Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Azerbaijan Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Azores Standard Time}
   (Name:('Azores Standard Time');
    Description:('(UTC-01:00) Azores');
    Bias:60;
    StandardName:('Azores Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:10;wDayOfWeek:0;wDay:5;wHour:1;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Azores Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:3;wDayOfWeek:0;wDay:5;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Bahia Standard Time}
   (Name:('Bahia Standard Time');
    Description:('(UTC-03:00) Salvador');
    Bias:180;
    StandardName:('Bahia Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Bahia Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Bangladesh Standard Time}
   (Name:('Bangladesh Standard Time');
    Description:('(UTC+06:00) Dhaka');
    Bias:-360;
    StandardName:('Bangladesh Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Bangladesh Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Belarus Standard Time}
   (Name:('Belarus Standard Time');
    Description:('(UTC+03:00) Minsk');
    Bias:-180;
    StandardName:('Belarus Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Belarus Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Bougainville Standard Time}
   (Name:('Bougainville Standard Time');
    Description:('(UTC+11:00) Bougainville Island');
    Bias:-660;
    StandardName:('Bougainville Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Bougainville Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Canada Central Standard Time}
   (Name:('Canada Central Standard Time');
    Description:('(UTC-06:00) Saskatchewan');
    Bias:360;
    StandardName:('Canada Central Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Canada Central Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Cape Verde Standard Time}
   (Name:('Cape Verde Standard Time');
    Description:('(UTC-01:00) Cabo Verde Is.');
    Bias:60;
    StandardName:('Cabo Verde Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Cabo Verde Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Caucasus Standard Time}
   (Name:('Caucasus Standard Time');
    Description:('(UTC+04:00) Yerevan');
    Bias:-240;
    StandardName:('Caucasus Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Caucasus Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Cen. Australia Standard Time}
   (Name:('Cen. Australia Standard Time');
    Description:('(UTC+09:30) Adelaide');
    Bias:-570;
    StandardName:('Cen. Australia Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:4;wDayOfWeek:0;wDay:1;wHour:3;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Cen. Australia Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:10;wDayOfWeek:0;wDay:1;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Central America Standard Time}
   (Name:('Central America Standard Time');
    Description:('(UTC-06:00) Central America');
    Bias:360;
    StandardName:('Central America Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Central America Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Central Asia Standard Time}
   (Name:('Central Asia Standard Time');
    Description:('(UTC+06:00) Astana');
    Bias:-360;
    StandardName:('Central Asia Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Central Asia Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Central Brazilian Standard Time}
   (Name:('Central Brazilian Standard Time');
    Description:('(UTC-04:00) Cuiaba');
    Bias:240;
    StandardName:('Central Brazilian Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Central Brazilian Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Central Europe Standard Time}
   (Name:('Central Europe Standard Time');
    Description:('(UTC+01:00) Belgrade, Bratislava, Budapest, Ljubljana, Prague');
    Bias:-60;
    StandardName:('Central Europe Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:10;wDayOfWeek:0;wDay:5;wHour:3;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Central Europe Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:3;wDayOfWeek:0;wDay:5;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Central European Standard Time}
   (Name:('Central European Standard Time');
    Description:('(UTC+01:00) Sarajevo, Skopje, Warsaw, Zagreb');
    Bias:-60;
    StandardName:('Central European Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:10;wDayOfWeek:0;wDay:5;wHour:3;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Central European Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:3;wDayOfWeek:0;wDay:5;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Central Pacific Standard Time}
   (Name:('Central Pacific Standard Time');
    Description:('(UTC+11:00) Solomon Is., New Caledonia');
    Bias:-660;
    StandardName:('Central Pacific Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Central Pacific Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Central Standard Time}
   (Name:('Central Standard Time');
    Description:('(UTC-06:00) Central Time (US & Canada)');
    Bias:360;
    StandardName:('Central Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:11;wDayOfWeek:0;wDay:1;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Central Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:3;wDayOfWeek:0;wDay:2;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Central Standard Time (Mexico)}
   (Name:('Central Standard Time (Mexico)');
    Description:('(UTC-06:00) Guadalajara, Mexico City, Monterrey');
    Bias:360;
    StandardName:('Central Standard Time (Mexico)');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Central Daylight Time (Mexico)');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Chatham Islands Standard Time}
   (Name:('Chatham Islands Standard Time');
    Description:('(UTC+12:45) Chatham Islands');
    Bias:-765;
    StandardName:('Chatham Islands Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:4;wDayOfWeek:0;wDay:1;wHour:3;wMinute:45;wSecond:0;wMilliseconds:0);
    DaylightName:('Chatham Islands Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:9;wDayOfWeek:0;wDay:5;wHour:2;wMinute:45;wSecond:0;wMilliseconds:0);
   ),

   {China Standard Time}
   (Name:('China Standard Time');
    Description:('(UTC+08:00) Beijing, Chongqing, Hong Kong, Urumqi');
    Bias:-480;
    StandardName:('China Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('China Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Cuba Standard Time}
   (Name:('Cuba Standard Time');
    Description:('(UTC-05:00) Havana');
    Bias:300;
    StandardName:('Cuba Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:11;wDayOfWeek:0;wDay:1;wHour:1;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Cuba Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:3;wDayOfWeek:0;wDay:2;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Dateline Standard Time}
   (Name:('Dateline Standard Time');
    Description:('(UTC-12:00) International Date Line West');
    Bias:720;
    StandardName:('Dateline Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Dateline Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {E. Africa Standard Time}
   (Name:('E. Africa Standard Time');
    Description:('(UTC+03:00) Nairobi');
    Bias:-180;
    StandardName:('E. Africa Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('E. Africa Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {E. Australia Standard Time}
   (Name:('E. Australia Standard Time');
    Description:('(UTC+10:00) Brisbane');
    Bias:-600;
    StandardName:('E. Australia Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('E. Australia Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {E. Europe Standard Time}
   (Name:('E. Europe Standard Time');
    Description:('(UTC+02:00) Chisinau');
    Bias:-120;
    StandardName:('E. Europe Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:10;wDayOfWeek:0;wDay:5;wHour:3;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('E. Europe Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:3;wDayOfWeek:0;wDay:5;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {E. South America Standard Time}
   (Name:('E. South America Standard Time');
    Description:('(UTC-03:00) Brasilia');
    Bias:180;
    StandardName:('E. South America Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('E. South America Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Easter Island Standard Time}
   (Name:('Easter Island Standard Time');
    Description:('(UTC-06:00) Easter Island');
    Bias:360;
    StandardName:('Easter Island Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:4;wDayOfWeek:6;wDay:1;wHour:22;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Easter Island Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:9;wDayOfWeek:6;wDay:1;wHour:22;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Eastern Standard Time}
   (Name:('Eastern Standard Time');
    Description:('(UTC-05:00) Eastern Time (US & Canada)');
    Bias:300;
    StandardName:('Eastern Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:11;wDayOfWeek:0;wDay:1;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Eastern Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:3;wDayOfWeek:0;wDay:2;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Eastern Standard Time (Mexico)}
   (Name:('Eastern Standard Time (Mexico)');
    Description:('(UTC-05:00) Chetumal');
    Bias:300;
    StandardName:('Eastern Standard Time (Mexico)');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Eastern Daylight Time (Mexico)');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Egypt Standard Time}
   (Name:('Egypt Standard Time');
    Description:('(UTC+02:00) Cairo');
    Bias:-120;
    StandardName:('Egypt Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:10;wDayOfWeek:4;wDay:5;wHour:23;wMinute:59;wSecond:59;wMilliseconds:999);
    DaylightName:('Egypt Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:4;wDayOfWeek:4;wDay:5;wHour:23;wMinute:59;wSecond:59;wMilliseconds:999);
   ),

   {Ekaterinburg Standard Time}
   (Name:('Ekaterinburg Standard Time');
    Description:('(UTC+05:00) Ekaterinburg');
    Bias:-300;
    StandardName:('Russia TZ 4 Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Russia TZ 4 Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Fiji Standard Time}
   (Name:('Fiji Standard Time');
    Description:('(UTC+12:00) Fiji');
    Bias:-720;
    StandardName:('Fiji Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Fiji Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {FLE Standard Time}
   (Name:('FLE Standard Time');
    Description:('(UTC+02:00) Helsinki, Kyiv, Riga, Sofia, Tallinn, Vilnius');
    Bias:-120;
    StandardName:('FLE Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:10;wDayOfWeek:0;wDay:5;wHour:4;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('FLE Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:3;wDayOfWeek:0;wDay:5;wHour:3;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Georgian Standard Time}
   (Name:('Georgian Standard Time');
    Description:('(UTC+04:00) Tbilisi');
    Bias:-240;
    StandardName:('Georgian Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Georgian Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {GMT Standard Time}
   (Name:('GMT Standard Time');
    Description:('(UTC+00:00) Dublin, Edinburgh, Lisbon, London');
    Bias:0;
    StandardName:('GMT Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:10;wDayOfWeek:0;wDay:5;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('GMT Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:3;wDayOfWeek:0;wDay:5;wHour:1;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Greenland Standard Time}
   (Name:('Greenland Standard Time');
    Description:('(UTC-03:00) Greenland');
    Bias:180;
    StandardName:('Greenland Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:10;wDayOfWeek:6;wDay:5;wHour:23;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Greenland Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:3;wDayOfWeek:6;wDay:5;wHour:22;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Greenwich Standard Time}
   (Name:('Greenwich Standard Time');
    Description:('(UTC+00:00) Monrovia, Reykjavik');
    Bias:0;
    StandardName:('Greenwich Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Greenwich Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {GTB Standard Time}
   (Name:('GTB Standard Time');
    Description:('(UTC+02:00) Athens, Bucharest');
    Bias:-120;
    StandardName:('GTB Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:10;wDayOfWeek:0;wDay:5;wHour:4;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('GTB Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:3;wDayOfWeek:0;wDay:5;wHour:3;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Haiti Standard Time}
   (Name:('Haiti Standard Time');
    Description:('(UTC-05:00) Haiti');
    Bias:300;
    StandardName:('Haiti Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:11;wDayOfWeek:0;wDay:1;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Haiti Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:3;wDayOfWeek:0;wDay:2;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Hawaiian Standard Time}
   (Name:('Hawaiian Standard Time');
    Description:('(UTC-10:00) Hawaii');
    Bias:600;
    StandardName:('Hawaiian Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Hawaiian Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {India Standard Time}
   (Name:('India Standard Time');
    Description:('(UTC+05:30) Chennai, Kolkata, Mumbai, New Delhi');
    Bias:-330;
    StandardName:('India Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('India Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Iran Standard Time}
   (Name:('Iran Standard Time');
    Description:('(UTC+03:30) Tehran');
    Bias:-210;
    StandardName:('Iran Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Iran Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Israel Standard Time}
   (Name:('Israel Standard Time');
    Description:('(UTC+02:00) Jerusalem');
    Bias:-120;
    StandardName:('Jerusalem Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:10;wDayOfWeek:0;wDay:5;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Jerusalem Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:3;wDayOfWeek:5;wDay:4;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Jordan Standard Time}
   (Name:('Jordan Standard Time');
    Description:('(UTC+03:00) Amman');
    Bias:-180;
    StandardName:('Jordan Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Jordan Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Kaliningrad Standard Time}
   (Name:('Kaliningrad Standard Time');
    Description:('(UTC+02:00) Kaliningrad');
    Bias:-120;
    StandardName:('Russia TZ 1 Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Russia TZ 1 Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Kamchatka Standard Time}
   (Name:('Kamchatka Standard Time');
    Description:('(UTC+12:00) Petropavlovsk-Kamchatsky - Old');
    Bias:-720;
    StandardName:('Kamchatka Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:10;wDayOfWeek:0;wDay:5;wHour:3;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Kamchatka Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:3;wDayOfWeek:0;wDay:5;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Korea Standard Time}
   (Name:('Korea Standard Time');
    Description:('(UTC+09:00) Seoul');
    Bias:-540;
    StandardName:('Korea Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Korea Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Libya Standard Time}
   (Name:('Libya Standard Time');
    Description:('(UTC+02:00) Tripoli');
    Bias:-120;
    StandardName:('Libya Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Libya Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Line Islands Standard Time}
   (Name:('Line Islands Standard Time');
    Description:('(UTC+14:00) Kiritimati Island');
    Bias:-840;
    StandardName:('Line Islands Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Line Islands Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Lord Howe Standard Time}
   (Name:('Lord Howe Standard Time');
    Description:('(UTC+10:30) Lord Howe Island');
    Bias:-630;
    StandardName:('Lord Howe Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:4;wDayOfWeek:0;wDay:1;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Lord Howe Daylight Time');
    DaylightBias:-30;
    DaylightStart:(wYear:0;wMonth:10;wDayOfWeek:0;wDay:1;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Magadan Standard Time}
   (Name:('Magadan Standard Time');
    Description:('(UTC+11:00) Magadan');
    Bias:-660;
    StandardName:('Magadan Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Magadan Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Magallanes Standard Time}
   (Name:('Magallanes Standard Time');
    Description:('(UTC-03:00) Punta Arenas');
    Bias:180;
    StandardName:('Magallanes Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Magallanes Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Marquesas Standard Time}
   (Name:('Marquesas Standard Time');
    Description:('(UTC-09:30) Marquesas Islands');
    Bias:570;
    StandardName:('Marquesas Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Marquesas Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Mauritius Standard Time}
   (Name:('Mauritius Standard Time');
    Description:('(UTC+04:00) Port Louis');
    Bias:-240;
    StandardName:('Mauritius Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Mauritius Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Mid-Atlantic Standard Time}
   (Name:('Mid-Atlantic Standard Time');
    Description:('(UTC-02:00) Mid-Atlantic - Old');
    Bias:120;
    StandardName:('Mid-Atlantic Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:9;wDayOfWeek:0;wDay:5;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Mid-Atlantic Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:3;wDayOfWeek:0;wDay:5;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Middle East Standard Time}
   (Name:('Middle East Standard Time');
    Description:('(UTC+02:00) Beirut');
    Bias:-120;
    StandardName:('Middle East Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:10;wDayOfWeek:6;wDay:5;wHour:23;wMinute:59;wSecond:59;wMilliseconds:999);
    DaylightName:('Middle East Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:3;wDayOfWeek:6;wDay:5;wHour:23;wMinute:59;wSecond:59;wMilliseconds:999);
   ),

   {Montevideo Standard Time}
   (Name:('Montevideo Standard Time');
    Description:('(UTC-03:00) Montevideo');
    Bias:180;
    StandardName:('Montevideo Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Montevideo Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Morocco Standard Time}
   (Name:('Morocco Standard Time');
    Description:('(UTC+01:00) Casablanca');
    Bias:0;
    StandardName:('Morocco Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:3;wDayOfWeek:0;wDay:3;wHour:3;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Morocco Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:4;wDayOfWeek:0;wDay:4;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Mountain Standard Time}
   (Name:('Mountain Standard Time');
    Description:('(UTC-07:00) Mountain Time (US & Canada)');
    Bias:420;
    StandardName:('Mountain Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:11;wDayOfWeek:0;wDay:1;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Mountain Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:3;wDayOfWeek:0;wDay:2;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Mountain Standard Time (Mexico)}
   (Name:('Mountain Standard Time (Mexico)');
    Description:('(UTC-07:00) La Paz, Mazatlan');
    Bias:420;
    StandardName:('Mountain Standard Time (Mexico)');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Mountain Daylight Time (Mexico)');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Myanmar Standard Time}
   (Name:('Myanmar Standard Time');
    Description:('(UTC+06:30) Yangon (Rangoon)');
    Bias:-390;
    StandardName:('Myanmar Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Myanmar Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {N. Central Asia Standard Time}
   (Name:('N. Central Asia Standard Time');
    Description:('(UTC+07:00) Novosibirsk');
    Bias:-420;
    StandardName:('Novosibirsk Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Novosibirsk Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Namibia Standard Time}
   (Name:('Namibia Standard Time');
    Description:('(UTC+02:00) Windhoek');
    Bias:-120;
    StandardName:('Namibia Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Namibia Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Nepal Standard Time}
   (Name:('Nepal Standard Time');
    Description:('(UTC+05:45) Kathmandu');
    Bias:-345;
    StandardName:('Nepal Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Nepal Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {New Zealand Standard Time}
   (Name:('New Zealand Standard Time');
    Description:('(UTC+12:00) Auckland, Wellington');
    Bias:-720;
    StandardName:('New Zealand Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:4;wDayOfWeek:0;wDay:1;wHour:3;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('New Zealand Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:9;wDayOfWeek:0;wDay:5;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Newfoundland Standard Time}
   (Name:('Newfoundland Standard Time');
    Description:('(UTC-03:30) Newfoundland');
    Bias:210;
    StandardName:('Newfoundland Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:11;wDayOfWeek:0;wDay:1;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Newfoundland Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:3;wDayOfWeek:0;wDay:2;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Norfolk Standard Time}
   (Name:('Norfolk Standard Time');
    Description:('(UTC+11:00) Norfolk Island');
    Bias:-660;
    StandardName:('Norfolk Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:4;wDayOfWeek:0;wDay:1;wHour:3;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Norfolk Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:10;wDayOfWeek:0;wDay:1;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {North Asia East Standard Time}
   (Name:('North Asia East Standard Time');
    Description:('(UTC+08:00) Irkutsk');
    Bias:-480;
    StandardName:('Russia TZ 7 Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Russia TZ 7 Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {North Asia Standard Time}
   (Name:('North Asia Standard Time');
    Description:('(UTC+07:00) Krasnoyarsk');
    Bias:-420;
    StandardName:('Russia TZ 6 Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Russia TZ 6 Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {North Korea Standard Time}
   (Name:('North Korea Standard Time');
    Description:('(UTC+09:00) Pyongyang');
    Bias:-540;
    StandardName:('North Korea Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('North Korea Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Omsk Standard Time}
   (Name:('Omsk Standard Time');
    Description:('(UTC+06:00) Omsk');
    Bias:-360;
    StandardName:('Omsk Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Omsk Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Pacific SA Standard Time}
   (Name:('Pacific SA Standard Time');
    Description:('(UTC-04:00) Santiago');
    Bias:240;
    StandardName:('Pacific SA Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:4;wDayOfWeek:6;wDay:1;wHour:23;wMinute:59;wSecond:59;wMilliseconds:999);
    DaylightName:('Pacific SA Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:9;wDayOfWeek:6;wDay:1;wHour:23;wMinute:59;wSecond:59;wMilliseconds:999);
   ),

   {Pacific Standard Time}
   (Name:('Pacific Standard Time');
    Description:('(UTC-08:00) Pacific Time (US & Canada)');
    Bias:480;
    StandardName:('Pacific Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:11;wDayOfWeek:0;wDay:1;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Pacific Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:3;wDayOfWeek:0;wDay:2;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Pacific Standard Time (Mexico)}
   (Name:('Pacific Standard Time (Mexico)');
    Description:('(UTC-08:00) Baja California');
    Bias:480;
    StandardName:('Pacific Standard Time (Mexico)');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:11;wDayOfWeek:0;wDay:1;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Pacific Daylight Time (Mexico)');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:3;wDayOfWeek:0;wDay:2;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Pakistan Standard Time}
   (Name:('Pakistan Standard Time');
    Description:('(UTC+05:00) Islamabad, Karachi');
    Bias:-300;
    StandardName:('Pakistan Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Pakistan Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Paraguay Standard Time}
   (Name:('Paraguay Standard Time');
    Description:('(UTC-04:00) Asuncion');
    Bias:240;
    StandardName:('Paraguay Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:3;wDayOfWeek:0;wDay:4;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Paraguay Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:10;wDayOfWeek:0;wDay:1;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Qyzylorda Standard Time}
   (Name:('Qyzylorda Standard Time');
    Description:('(UTC+05:00) Qyzylorda');
    Bias:-300;
    StandardName:('Qyzylorda Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Qyzylorda Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Romance Standard Time}
   (Name:('Romance Standard Time');
    Description:('(UTC+01:00) Brussels, Copenhagen, Madrid, Paris');
    Bias:-60;
    StandardName:('Romance Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:10;wDayOfWeek:0;wDay:5;wHour:3;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Romance Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:3;wDayOfWeek:0;wDay:5;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Russia Time Zone 10}
   (Name:('Russia Time Zone 10');
    Description:('(UTC+11:00) Chokurdakh');
    Bias:-660;
    StandardName:('Russia TZ 10 Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Russia TZ 10 Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Russia Time Zone 11}
   (Name:('Russia Time Zone 11');
    Description:('(UTC+12:00) Anadyr, Petropavlovsk-Kamchatsky');
    Bias:-720;
    StandardName:('Russia TZ 11 Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Russia TZ 11 Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Russia Time Zone 3}
   (Name:('Russia Time Zone 3');
    Description:('(UTC+04:00) Izhevsk, Samara');
    Bias:-240;
    StandardName:('Russia TZ 3 Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Russia TZ 3 Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Russian Standard Time}
   (Name:('Russian Standard Time');
    Description:('(UTC+03:00) Moscow, St. Petersburg');
    Bias:-180;
    StandardName:('Russia TZ 2 Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Russia TZ 2 Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {SA Eastern Standard Time}
   (Name:('SA Eastern Standard Time');
    Description:('(UTC-03:00) Cayenne, Fortaleza');
    Bias:180;
    StandardName:('SA Eastern Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('SA Eastern Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {SA Pacific Standard Time}
   (Name:('SA Pacific Standard Time');
    Description:('(UTC-05:00) Bogota, Lima, Quito, Rio Branco');
    Bias:300;
    StandardName:('SA Pacific Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('SA Pacific Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {SA Western Standard Time}
   (Name:('SA Western Standard Time');
    Description:('(UTC-04:00) Georgetown, La Paz, Manaus, San Juan');
    Bias:240;
    StandardName:('SA Western Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('SA Western Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Saint Pierre Standard Time}
   (Name:('Saint Pierre Standard Time');
    Description:('(UTC-03:00) Saint Pierre and Miquelon');
    Bias:180;
    StandardName:('Saint Pierre Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:11;wDayOfWeek:0;wDay:1;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Saint Pierre Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:3;wDayOfWeek:0;wDay:2;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Sakhalin Standard Time}
   (Name:('Sakhalin Standard Time');
    Description:('(UTC+11:00) Sakhalin');
    Bias:-660;
    StandardName:('Sakhalin Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Sakhalin Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Samoa Standard Time}
   (Name:('Samoa Standard Time');
    Description:('(UTC+13:00) Samoa');
    Bias:-780;
    StandardName:('Samoa Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:4;wDayOfWeek:0;wDay:1;wHour:4;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Samoa Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:9;wDayOfWeek:0;wDay:5;wHour:3;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Sao Tome Standard Time}
   (Name:('Sao Tome Standard Time');
    Description:('(UTC+00:00) Sao Tome');
    Bias:0;
    StandardName:('Sao Tome Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Sao Tome Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Saratov Standard Time}
   (Name:('Saratov Standard Time');
    Description:('(UTC+04:00) Saratov');
    Bias:-240;
    StandardName:('Saratov Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Saratov Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {SE Asia Standard Time}
   (Name:('SE Asia Standard Time');
    Description:('(UTC+07:00) Bangkok, Hanoi, Jakarta');
    Bias:-420;
    StandardName:('SE Asia Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('SE Asia Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Singapore Standard Time}
   (Name:('Singapore Standard Time');
    Description:('(UTC+08:00) Kuala Lumpur, Singapore');
    Bias:-480;
    StandardName:('Malay Peninsula Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Malay Peninsula Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {South Africa Standard Time}
   (Name:('South Africa Standard Time');
    Description:('(UTC+02:00) Harare, Pretoria');
    Bias:-120;
    StandardName:('South Africa Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('South Africa Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {South Sudan Standard Time}
   (Name:('South Sudan Standard Time');
    Description:('(UTC+02:00) Juba');
    Bias:-120;
    StandardName:('South Sudan Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('South Sudan Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Sri Lanka Standard Time}
   (Name:('Sri Lanka Standard Time');
    Description:('(UTC+05:30) Sri Jayawardenepura');
    Bias:-330;
    StandardName:('Sri Lanka Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Sri Lanka Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Sudan Standard Time}
   (Name:('Sudan Standard Time');
    Description:('(UTC+02:00) Khartoum');
    Bias:-120;
    StandardName:('Sudan Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Sudan Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Syria Standard Time}
   (Name:('Syria Standard Time');
    Description:('(UTC+02:00) Damascus');
    Bias:-120;
    StandardName:('Syria Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:10;wDayOfWeek:4;wDay:5;wHour:23;wMinute:59;wSecond:59;wMilliseconds:999);
    DaylightName:('Syria Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:3;wDayOfWeek:5;wDay:5;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Taipei Standard Time}
   (Name:('Taipei Standard Time');
    Description:('(UTC+08:00) Taipei');
    Bias:-480;
    StandardName:('Taipei Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Taipei Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Tasmania Standard Time}
   (Name:('Tasmania Standard Time');
    Description:('(UTC+10:00) Hobart');
    Bias:-600;
    StandardName:('Tasmania Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:4;wDayOfWeek:0;wDay:1;wHour:3;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Tasmania Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:10;wDayOfWeek:0;wDay:1;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Tocantins Standard Time}
   (Name:('Tocantins Standard Time');
    Description:('(UTC-03:00) Araguaina');
    Bias:180;
    StandardName:('Tocantins Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Tocantins Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Tokyo Standard Time}
   (Name:('Tokyo Standard Time');
    Description:('(UTC+09:00) Osaka, Sapporo, Tokyo');
    Bias:-540;
    StandardName:('Tokyo Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Tokyo Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Tomsk Standard Time}
   (Name:('Tomsk Standard Time');
    Description:('(UTC+07:00) Tomsk');
    Bias:-420;
    StandardName:('Tomsk Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Tomsk Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Tonga Standard Time}
   (Name:('Tonga Standard Time');
    Description:('(UTC+13:00) Nuku''alofa');
    Bias:-780;
    StandardName:('Tonga Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Tonga Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Transbaikal Standard Time}
   (Name:('Transbaikal Standard Time');
    Description:('(UTC+09:00) Chita');
    Bias:-540;
    StandardName:('Transbaikal Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Transbaikal Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Turkey Standard Time}
   (Name:('Turkey Standard Time');
    Description:('(UTC+03:00) Istanbul');
    Bias:-180;
    StandardName:('Turkey Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Turkey Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Turks And Caicos Standard Time}
   (Name:('Turks And Caicos Standard Time');
    Description:('(UTC-05:00) Turks and Caicos');
    Bias:300;
    StandardName:('Turks and Caicos Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:11;wDayOfWeek:0;wDay:1;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Turks and Caicos Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:3;wDayOfWeek:0;wDay:2;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Ulaanbaatar Standard Time}
   (Name:('Ulaanbaatar Standard Time');
    Description:('(UTC+08:00) Ulaanbaatar');
    Bias:-480;
    StandardName:('Ulaanbaatar Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Ulaanbaatar Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {US Eastern Standard Time}
   (Name:('US Eastern Standard Time');
    Description:('(UTC-05:00) Indiana (East)');
    Bias:300;
    StandardName:('US Eastern Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:11;wDayOfWeek:0;wDay:1;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('US Eastern Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:3;wDayOfWeek:0;wDay:2;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {US Mountain Standard Time}
   (Name:('US Mountain Standard Time');
    Description:('(UTC-07:00) Arizona');
    Bias:420;
    StandardName:('US Mountain Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('US Mountain Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {UTC}
   (Name:('UTC');
    Description:('(UTC) Coordinated Universal Time');
    Bias:0;
    StandardName:('Coordinated Universal Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Coordinated Universal Time');
    DaylightBias:0;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {UTC+12}
   (Name:('UTC+12');
    Description:('(UTC+12:00) Coordinated Universal Time+12');
    Bias:-720;
    StandardName:('UTC+12');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('UTC+12');
    DaylightBias:0;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {UTC+13}
   (Name:('UTC+13');
    Description:('(UTC+13:00) Coordinated Universal Time+13');
    Bias:-780;
    StandardName:('UTC+13');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('UTC+13');
    DaylightBias:0;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {UTC-02}
   (Name:('UTC-02');
    Description:('(UTC-02:00) Coordinated Universal Time-02');
    Bias:120;
    StandardName:('UTC-02');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('UTC-02');
    DaylightBias:0;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {UTC-08}
   (Name:('UTC-08');
    Description:('(UTC-08:00) Coordinated Universal Time-08');
    Bias:480;
    StandardName:('UTC-08');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('UTC-08');
    DaylightBias:0;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {UTC-09}
   (Name:('UTC-09');
    Description:('(UTC-09:00) Coordinated Universal Time-09');
    Bias:540;
    StandardName:('UTC-09');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('UTC-09');
    DaylightBias:0;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {UTC-11}
   (Name:('UTC-11');
    Description:('(UTC-11:00) Coordinated Universal Time-11');
    Bias:660;
    StandardName:('UTC-11');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('UTC-11');
    DaylightBias:0;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Venezuela Standard Time}
   (Name:('Venezuela Standard Time');
    Description:('(UTC-04:00) Caracas');
    Bias:240;
    StandardName:('Venezuela Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Venezuela Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Vladivostok Standard Time}
   (Name:('Vladivostok Standard Time');
    Description:('(UTC+10:00) Vladivostok');
    Bias:-600;
    StandardName:('Russia TZ 9 Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Russia TZ 9 Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Volgograd Standard Time}
   (Name:('Volgograd Standard Time');
    Description:('(UTC+03:00) Volgograd');
    Bias:-180;
    StandardName:('Volgograd Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Volgograd Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {W. Australia Standard Time}
   (Name:('W. Australia Standard Time');
    Description:('(UTC+08:00) Perth');
    Bias:-480;
    StandardName:('W. Australia Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('W. Australia Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {W. Central Africa Standard Time}
   (Name:('W. Central Africa Standard Time');
    Description:('(UTC+01:00) West Central Africa');
    Bias:-60;
    StandardName:('W. Central Africa Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('W. Central Africa Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {W. Europe Standard Time}
   (Name:('W. Europe Standard Time');
    Description:('(UTC+01:00) Amsterdam, Berlin, Bern, Rome, Stockholm, Vienna');
    Bias:-60;
    StandardName:('W. Europe Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:10;wDayOfWeek:0;wDay:5;wHour:3;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('W. Europe Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:3;wDayOfWeek:0;wDay:5;wHour:2;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {W. Mongolia Standard Time}
   (Name:('W. Mongolia Standard Time');
    Description:('(UTC+07:00) Hovd');
    Bias:-420;
    StandardName:('W. Mongolia Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('W. Mongolia Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {West Asia Standard Time}
   (Name:('West Asia Standard Time');
    Description:('(UTC+05:00) Ashgabat, Tashkent');
    Bias:-300;
    StandardName:('West Asia Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('West Asia Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {West Bank Standard Time}
   (Name:('West Bank Standard Time');
    Description:('(UTC+02:00) Gaza, Hebron');
    Bias:-120;
    StandardName:('West Bank Gaza Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:10;wDayOfWeek:6;wDay:5;wHour:1;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('West Bank Gaza Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:3;wDayOfWeek:6;wDay:5;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {West Pacific Standard Time}
   (Name:('West Pacific Standard Time');
    Description:('(UTC+10:00) Guam, Port Moresby');
    Bias:-600;
    StandardName:('West Pacific Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('West Pacific Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Yakutsk Standard Time}
   (Name:('Yakutsk Standard Time');
    Description:('(UTC+09:00) Yakutsk');
    Bias:-540;
    StandardName:('Russia TZ 8 Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Russia TZ 8 Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   ),

   {Yukon Standard Time}
   (Name:('Yukon Standard Time');
    Description:('(UTC-07:00) Yukon');
    Bias:420;
    StandardName:('Yukon Standard Time');
    StandardBias:0;
    StandardStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
    DaylightName:('Yukon Daylight Time');
    DaylightBias:-60;
    DaylightStart:(wYear:0;wMonth:0;wDayOfWeek:0;wDay:0;wHour:0;wMinute:0;wSecond:0;wMilliseconds:0);
   )

   )
  );
 {End timezone builder data}

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure TimezoneInit;
var
 Count:LongWord;
 WorkBuffer:String;
 Data:PTimezoneData;
begin
 {}
 {Check Initialized}
 if TimezoneInitialized then Exit;

 {Initialize Timezone Table}
 TimezoneTable:=nil;
 TimezoneTableLock:=CriticalSectionCreate;
 TimezoneTableCount:=0;
 if TimezoneTableLock = INVALID_HANDLE_VALUE then
  begin
   if PLATFORM_LOG_ENABLED then PlatformLogError('Failed to create timezone table lock');
  end;
 TimezoneDefault:=nil;

 {Set Timezone Defaults}
 TIMEZONE_TIME_OFFSET:=0;
 TIMEZONE_TIME_ADJUST:=0;

 TIMEZONE_DEFAULT_NAME:='UTC';

 {Check Environment Variables}
 {TIMEZONE_DEFAULT_NAME}
 WorkBuffer:=EnvironmentGet('TIMEZONE_DEFAULT_NAME');
 if Length(WorkBuffer) <> 0 then TIMEZONE_DEFAULT_NAME:=TimezoneNameReplaceChar(WorkBuffer,'_',' ');

 {Load Timezone Table}
 for Count:=0 to TimezoneList.TimezoneCount - 1 do
  begin
   {Get Data}
   Data:=@TimezoneList.TimezoneData[Count];

   {Add Timezone}
   TimezoneAdd(Data,Data.Name = TIMEZONE_DEFAULT_NAME);
  end;

 {Setup Platform Handlers}
 ClockUpdateOffsetHandler:=TimezoneUpdateOffset;
 ClockCalculateOffsetHandler:=TimezoneCalculateOffset;

 TimezoneInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Timezone Functions}
function TimezoneAdd(Data:PTimezoneData;Default:Boolean):LongWord;
{Add a Timezone from a timezone data block and to the Timezone table}
var
 Timezone:PTimezoneEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Data}
 if Data = nil then Exit;

 {Create Timezone}
 Timezone:=PTimezoneEntry(AllocMem(SizeOf(TTimezoneEntry)));
 if Timezone = nil then Exit;

 {Update Timezone}
 Timezone.Signature:=TIMEZONE_SIGNATURE;
 Timezone.Name:=Data.Name;
 Timezone.Description:=Data.Description;
 Timezone.Bias:=Data.Bias;
 Timezone.StandardName:=Data.StandardName;
 Timezone.StandardBias:=Data.StandardBias;
 Timezone.StandardStart:=Data.StandardStart;
 Timezone.DaylightName:=Data.DaylightName;
 Timezone.DaylightBias:=Data.DaylightBias;
 Timezone.DaylightStart:=Data.DaylightStart;

 {Insert Timezone}
 if CriticalSectionLock(TimezoneTableLock) = ERROR_SUCCESS then
  begin
   try
    {Link Timezone}
    if TimezoneTable = nil then
     begin
      TimezoneTable:=Timezone;
     end
    else
     begin
      Timezone.Next:=TimezoneTable;
      TimezoneTable.Prev:=Timezone;
      TimezoneTable:=Timezone;
     end;

    {Increment Count}
    Inc(TimezoneTableCount);

    {Check Default}
    if (TimezoneDefault = nil) and (Default) then
     begin
      TimezoneDefault:=Timezone;

      {Allocate Default Name}
      SetLength(TIMEZONE_DEFAULT_NAME,TIMEZONE_NAME_LENGTH - 1);

      {Set Timezone Defaults}
      StrLCopy(PChar(TIMEZONE_DEFAULT_NAME),TimezoneDefault.Name,TIMEZONE_NAME_LENGTH - 1);

      {Update Default Name}
      SetLength(TIMEZONE_DEFAULT_NAME,StrLen(PChar(TIMEZONE_DEFAULT_NAME)));

      {Update TZ Environment}
      TimezoneUpdateEnvironment;

      {Update Timezone Offset}
      TimezoneUpdateOffset;
     end;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(TimezoneTableLock);
   end;
  end
 else
  begin
   {Free Timezone}
   FreeMem(Timezone);

   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function TimezoneDelete(Timezone:PTimezoneEntry):LongWord;
var
 Prev:PTimezoneEntry;
 Next:PTimezoneEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Timezone}
 if Timezone = nil then Exit;
 if Timezone.Signature <> TIMEZONE_SIGNATURE then Exit;

 {Acquire Lock}
 if CriticalSectionLock(TimezoneTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check Timezone}
    if TimezoneCheck(Timezone) <> Timezone then Exit;

    {Unlink Timezone}
    Prev:=Timezone.Prev;
    Next:=Timezone.Next;
    if Prev = nil then
     begin
      TimezoneTable:=Next;
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
    Dec(TimezoneTableCount);

    {Check Default}
    if TimezoneDefault = Timezone then
     begin
      TimezoneDefault:=TimezoneTable;

      if TimezoneDefault <> nil then
       begin
        {Allocate Default Name}
        SetLength(TIMEZONE_DEFAULT_NAME,TIMEZONE_NAME_LENGTH - 1);

        {Set Timezone Defaults}
        StrLCopy(PChar(TIMEZONE_DEFAULT_NAME),TimezoneDefault.Name,TIMEZONE_NAME_LENGTH - 1);

        {Update Default Name}
        SetLength(TIMEZONE_DEFAULT_NAME,StrLen(PChar(TIMEZONE_DEFAULT_NAME)));

        {Update TZ Environment}
        TimezoneUpdateEnvironment;

        {Update Timezone Offset}
        TimezoneUpdateOffset;
       end;
     end;

    {Update Timezone}
    Timezone.Signature:=0;

    {Free Timezone}
    FreeMem(Timezone);

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(TimezoneTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function TimezoneGetName(Timezone:PTimezoneEntry):String;
begin
 {}
 Result:='';

 {Check Timezone}
 if Timezone = nil then Exit;
 if Timezone.Signature <> TIMEZONE_SIGNATURE then Exit;

 {Acquire Lock}
 if CriticalSectionLock(TimezoneTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check Timezone}
    if TimezoneCheck(Timezone) <> Timezone then Exit;

    {Allocate Result}
    SetLength(Result,TIMEZONE_NAME_LENGTH - 1);

    {Get Name}
    StrLCopy(PChar(Result),Timezone.Name,TIMEZONE_NAME_LENGTH - 1);

    {Update Result}
    SetLength(Result,StrLen(PChar(Result)));
   finally
    {Release the Lock}
    CriticalSectionUnlock(TimezoneTableLock);
   end;
  end;
end;

{==============================================================================}

function TimezoneGetDescription(Timezone:PTimezoneEntry):String;
begin
 {}
 Result:='';

 {Check Timezone}
 if Timezone = nil then Exit;
 if Timezone.Signature <> TIMEZONE_SIGNATURE then Exit;

 {Acquire Lock}
 if CriticalSectionLock(TimezoneTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check Timezone}
    if TimezoneCheck(Timezone) <> Timezone then Exit;

    {Allocate Result}
    SetLength(Result,TIMEZONE_DESC_LENGTH - 1);

    {Get Description}
    StrLCopy(PChar(Result),Timezone.Description,TIMEZONE_DESC_LENGTH - 1);

    {Update Result}
    SetLength(Result,StrLen(PChar(Result)));
   finally
    {Release the Lock}
    CriticalSectionUnlock(TimezoneTableLock);
   end;
  end;
end;

{==============================================================================}

function TimezoneGetBias(Timezone:PTimezoneEntry):LongInt;
begin
 {}
 Result:=0;

 {Check Timezone}
 if Timezone = nil then Exit;
 if Timezone.Signature <> TIMEZONE_SIGNATURE then Exit;

 {Acquire Lock}
 if CriticalSectionLock(TimezoneTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check Timezone}
    if TimezoneCheck(Timezone) <> Timezone then Exit;

    {Get Bias}
    Result:=Timezone.Bias;
   finally
    {Release the Lock}
    CriticalSectionUnlock(TimezoneTableLock);
   end;
  end;
end;

{==============================================================================}

function TimezoneGetState(Timezone:PTimezoneEntry):LongWord; inline;
{Get the state of the supplied Timezone at the current date and time}
{Timezone: The timezone entry to get the state for}
{Return: The TIME_ZONE_ID_* constant representing the standard / daylight state of the timezone}
begin
 {}
 Result:=TimezoneGetStateEx(Timezone,Now);
end;

{==============================================================================}

function TimezoneGetStateEx(Timezone:PTimezoneEntry;const DateTime:TDateTime):LongWord;
{Get the state of the supplied Timezone at the specified date and time}
{Timezone: The timezone entry to get the state for}
{DateTime: The date and time to get the state of the timezone at (Assumed to be Local)}
{Return: The TIME_ZONE_ID_* constant representing the standard / daylight state of the timezone}
var
 Day:Word;
 Month:Word;
 Year:Word;
 EndDate:TDateTime;
 StartDate:TDateTime;
 CurrentDate:TDateTime;
begin
 {}
 Result:=TIME_ZONE_ID_INVALID;

 {Check Timezone}
 if Timezone = nil then Exit;
 if Timezone.Signature <> TIMEZONE_SIGNATURE then Exit;

 {Acquire Lock}
 if CriticalSectionLock(TimezoneTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check Timezone}
    if TimezoneCheck(Timezone) <> Timezone then Exit;

    {Get Year}
    DecodeDate(Trunc(DateTime),Year,Month,Day);
    if Year > 0 then
     begin
      {Get Current}
      CurrentDate:=DateTime;

      {Get Daylight Start and End}
      StartDate:=TimezoneStartToDateTime(Timezone.DaylightStart,Year);
      EndDate:=TimezoneStartToDateTime(Timezone.StandardStart,Year);

      {Check Start and End}
      if (StartDate = 0) or (EndDate = 0) then
       begin
        {Unknown}
        Result:=TIME_ZONE_ID_UNKNOWN;
       end
      else
       begin
        if (StartDate > CurrentDate) and (EndDate > CurrentDate) then
         begin
          {Start and End in future}
          if StartDate < EndDate then
           begin
            {Standard}
            Result:=TIME_ZONE_ID_STANDARD;
           end
          else if EndDate < StartDate then
           begin
            {Daylight}
            Result:=TIME_ZONE_ID_DAYLIGHT;
           end;
         end
        else if (StartDate < CurrentDate) and (EndDate < CurrentDate) then
         begin
          {Start and End in past}
          if StartDate < EndDate then
           begin
            {Standard}
            Result:=TIME_ZONE_ID_STANDARD;
           end
          else if EndDate < StartDate then
           begin
            {Daylight}
            Result:=TIME_ZONE_ID_DAYLIGHT;
           end;
         end
        else if (StartDate > CurrentDate) and (EndDate < CurrentDate) then
         begin
          {Start in future and End in past}
          {Standard}
          Result:=TIME_ZONE_ID_STANDARD;
         end
        else if (StartDate < CurrentDate) and (EndDate > CurrentDate) then
         begin
          {Start in past and End in future}
          {Daylight}
          Result:=TIME_ZONE_ID_DAYLIGHT;
         end;
       end;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(TimezoneTableLock);
   end;
  end;
end;

{==============================================================================}

function TimezoneGetActiveBias(Timezone:PTimezoneEntry):LongInt; inline;
{Get the bias (offset between UTC and Local) of the supplied Timezone at the current date and time}
{Timezone: The timezone entry to get the bias for}
{Return: The bias in minutes offset between UTC and Local including any daylight bias if active}
begin
 {}
 Result:=TimezoneGetActiveBiasEx(Timezone,Now);
end;

{==============================================================================}

function TimezoneGetActiveBiasEx(Timezone:PTimezoneEntry;const DateTime:TDateTime):LongInt;
{Get the bias (offset between UTC and Local) of the supplied Timezone at the specified date and time}
{Timezone: The timezone entry to get the bias for}
{DateTime: The date and time to get the bias of the timezone at (Assumed to be Local)}
{Return: The bias in minutes offset between UTC and Local}
var
 Day:Word;
 Month:Word;
 Year:Word;
 EndDate:TDateTime;
 StartDate:TDateTime;
 CurrentDate:TDateTime;
begin
 {}
 Result:=0;

 {Check Timezone}
 if Timezone = nil then Exit;
 if Timezone.Signature <> TIMEZONE_SIGNATURE then Exit;

 {Acquire Lock}
 if CriticalSectionLock(TimezoneTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check Timezone}
    if TimezoneCheck(Timezone) <> Timezone then Exit;

    {Get Year}
    DecodeDate(Trunc(DateTime),Year,Month,Day);
    if Year > 0 then
     begin
      {Get Current}
      CurrentDate:=DateTime;

      {Get Daylight Start and End}
      StartDate:=TimezoneStartToDateTime(Timezone.DaylightStart,Year);
      EndDate:=TimezoneStartToDateTime(Timezone.StandardStart,Year);

      {Check Start and End}
      if (StartDate = 0) or (EndDate = 0) then
       begin
        {Standard}
        Result:=Timezone.Bias;
       end
      else
       begin
        if (StartDate > CurrentDate) and (EndDate > CurrentDate) then
         begin
          {Start and End in future}
          if StartDate < EndDate then
           begin
            {Standard}
            Result:=Timezone.Bias;
           end
          else if EndDate < StartDate then
           begin
            {Daylight}
            Result:=Timezone.Bias + Timezone.DaylightBias;
           end;
         end
        else if (StartDate < CurrentDate) and (EndDate < CurrentDate) then
         begin
          {Start and End in past}
          if StartDate < EndDate then
           begin
            {Standard}
            Result:=Timezone.Bias;
           end
          else if EndDate < StartDate then
           begin
            {Daylight}
            Result:=Timezone.Bias + Timezone.DaylightBias;
           end;
         end
        else if (StartDate > CurrentDate) and (EndDate < CurrentDate) then
         begin
          {Start in future and End in past}
          {Standard}
          Result:=Timezone.Bias;
         end
        else if (StartDate < CurrentDate) and (EndDate > CurrentDate) then
         begin
          {Start in past and End in future}
          {Daylight}
          Result:=Timezone.Bias + Timezone.DaylightBias;
         end;
       end;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(TimezoneTableLock);
   end;
  end;
end;

{==============================================================================}

function TimezoneGetStandardName(Timezone:PTimezoneEntry):String;
begin
 {}
 Result:='';

 {Check Timezone}
 if Timezone = nil then Exit;
 if Timezone.Signature <> TIMEZONE_SIGNATURE then Exit;

 {Acquire Lock}
 if CriticalSectionLock(TimezoneTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check Timezone}
    if TimezoneCheck(Timezone) <> Timezone then Exit;

    {Allocate Result}
    SetLength(Result,TIMEZONE_NAME_LENGTH - 1);

    {Get Standard Name}
    StrLCopy(PChar(Result),Timezone.StandardName,TIMEZONE_NAME_LENGTH - 1);

    {Update Result}
    SetLength(Result,StrLen(PChar(Result)));
   finally
    {Release the Lock}
    CriticalSectionUnlock(TimezoneTableLock);
   end;
  end;
end;

{==============================================================================}

function TimezoneGetStandardBias(Timezone:PTimezoneEntry):LongInt;
begin
 {}
 Result:=0;

 {Check Timezone}
 if Timezone = nil then Exit;
 if Timezone.Signature <> TIMEZONE_SIGNATURE then Exit;

 {Acquire Lock}
 if CriticalSectionLock(TimezoneTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check Timezone}
    if TimezoneCheck(Timezone) <> Timezone then Exit;

    {Get Standard Bias}
    Result:=Timezone.StandardBias;
   finally
    {Release the Lock}
    CriticalSectionUnlock(TimezoneTableLock);
   end;
  end;
end;

{==============================================================================}

function TimezoneGetStandardDate(Timezone:PTimezoneEntry;Next:Boolean):TDateTime;
var
 Day:Word;
 Month:Word;
 Year:Word;
begin
 {}
 Result:=0;

 {Check Timezone}
 if Timezone = nil then Exit;
 if Timezone.Signature <> TIMEZONE_SIGNATURE then Exit;

 {Acquire Lock}
 if CriticalSectionLock(TimezoneTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check Timezone}
    if TimezoneCheck(Timezone) <> Timezone then Exit;

    {Get Year}
    DecodeDate(Date,Year,Month,Day);
    if Year > 0 then
     begin
      {Get Date Time}
      Result:=TimezoneStartToDateTime(Timezone.StandardStart,Year);

      {Check Date Time}
      if Next and (Result < Now) then
       begin
        {Get Next Date Time}
        Result:=TimezoneStartToDateTime(Timezone.StandardStart,Year + 1);
       end;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(TimezoneTableLock);
   end;
  end;
end;

{==============================================================================}

function TimezoneGetStandardStart(Timezone:PTimezoneEntry):SYSTEMTIME;
begin
 {}
 FillChar(Result,SizeOf(SYSTEMTIME),0);

 {Check Timezone}
 if Timezone = nil then Exit;
 if Timezone.Signature <> TIMEZONE_SIGNATURE then Exit;

 {Acquire Lock}
 if CriticalSectionLock(TimezoneTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check Timezone}
    if TimezoneCheck(Timezone) <> Timezone then Exit;

    {Get Standard Start}
    Result:=Timezone.StandardStart;
   finally
    {Release the Lock}
    CriticalSectionUnlock(TimezoneTableLock);
   end;
  end;
end;

{==============================================================================}

function TimezoneGetDaylightName(Timezone:PTimezoneEntry):String;
begin
 {}
 Result:='';

 {Check Timezone}
 if Timezone = nil then Exit;
 if Timezone.Signature <> TIMEZONE_SIGNATURE then Exit;

 {Acquire Lock}
 if CriticalSectionLock(TimezoneTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check Timezone}
    if TimezoneCheck(Timezone) <> Timezone then Exit;

    {Allocate Result}
    SetLength(Result,TIMEZONE_NAME_LENGTH - 1);

    {Get Daylight Name}
    StrLCopy(PChar(Result),Timezone.DaylightName,TIMEZONE_NAME_LENGTH - 1);

    {Update Result}
    SetLength(Result,StrLen(PChar(Result)));
   finally
    {Release the Lock}
    CriticalSectionUnlock(TimezoneTableLock);
   end;
  end;
end;

{==============================================================================}

function TimezoneGetDaylightBias(Timezone:PTimezoneEntry):LongInt;
begin
 {}
 Result:=0;

 {Check Timezone}
 if Timezone = nil then Exit;
 if Timezone.Signature <> TIMEZONE_SIGNATURE then Exit;

 {Acquire Lock}
 if CriticalSectionLock(TimezoneTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check Timezone}
    if TimezoneCheck(Timezone) <> Timezone then Exit;

    {Get Daylight Bias}
    Result:=Timezone.DaylightBias;
   finally
    {Release the Lock}
    CriticalSectionUnlock(TimezoneTableLock);
   end;
  end;
end;

{==============================================================================}

function TimezoneGetDaylightDate(Timezone:PTimezoneEntry;Next:Boolean):TDateTime;
var
 Day:Word;
 Month:Word;
 Year:Word;
begin
 {}
 Result:=0;

 {Check Timezone}
 if Timezone = nil then Exit;
 if Timezone.Signature <> TIMEZONE_SIGNATURE then Exit;

 {Acquire Lock}
 if CriticalSectionLock(TimezoneTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check Timezone}
    if TimezoneCheck(Timezone) <> Timezone then Exit;

    {Get Year}
    DecodeDate(Date,Year,Month,Day);
    if Year > 0 then
     begin
      {Get Date Time}
      Result:=TimezoneStartToDateTime(Timezone.DaylightStart,Year);

      {Check Date Time}
      if Next and (Result < Now) then
       begin
        {Get Next Date Time}
        Result:=TimezoneStartToDateTime(Timezone.DaylightStart,Year + 1);
       end;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(TimezoneTableLock);
   end;
  end;
end;

{==============================================================================}

function TimezoneGetDaylightStart(Timezone:PTimezoneEntry):SYSTEMTIME;
begin
 {}
 FillChar(Result,SizeOf(SYSTEMTIME),0);

 {Check Timezone}
 if Timezone = nil then Exit;
 if Timezone.Signature <> TIMEZONE_SIGNATURE then Exit;

 {Acquire Lock}
 if CriticalSectionLock(TimezoneTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check Timezone}
    if TimezoneCheck(Timezone) <> Timezone then Exit;

    {Get Daylight Start}
    Result:=Timezone.DaylightStart;
   finally
    {Release the Lock}
    CriticalSectionUnlock(TimezoneTableLock);
   end;
  end;
end;

{==============================================================================}

function TimezoneFind(const Name:String):PTimezoneEntry;
var
 Timezone:PTimezoneEntry;
begin
 {}
 Result:=nil;

 {Acquire the Lock}
 if CriticalSectionLock(TimezoneTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Timezone}
    Timezone:=TimezoneTable;
    while Timezone <> nil do
     begin
      {Check State}
      if Timezone.Signature = TIMEZONE_SIGNATURE then
       begin
        {Check Name}
        if Uppercase(Timezone.Name) = Uppercase(Name) then
         begin
          {Return Result}
          Result:=Timezone;
          Exit;
         end;
       end;
      {Get Next}
      Timezone:=Timezone.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(TimezoneTableLock);
   end;
  end;
end;

{==============================================================================}

function TimezoneFindByStandard(const StandardName:String):PTimezoneEntry;
var
 Timezone:PTimezoneEntry;
begin
 {}
 Result:=nil;

 {Acquire the Lock}
 if CriticalSectionLock(TimezoneTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Timezone}
    Timezone:=TimezoneTable;
    while Timezone <> nil do
     begin
      {Check State}
      if Timezone.Signature = TIMEZONE_SIGNATURE then
       begin
        {Check Standard Name}
        if Uppercase(Timezone.StandardName) = Uppercase(StandardName) then
         begin
          {Return Result}
          Result:=Timezone;
          Exit;
         end;
       end;
      {Get Next}
      Timezone:=Timezone.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(TimezoneTableLock);
   end;
  end;
end;

{==============================================================================}

function TimezoneFindByDaylight(const DaylightName:String):PTimezoneEntry;
var
 Timezone:PTimezoneEntry;
begin
 {}
 Result:=nil;

 {Acquire the Lock}
 if CriticalSectionLock(TimezoneTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Timezone}
    Timezone:=TimezoneTable;
    while Timezone <> nil do
     begin
      {Check State}
      if Timezone.Signature = TIMEZONE_SIGNATURE then
       begin
        {Check Daylight Name}
        if Uppercase(Timezone.DaylightName) = Uppercase(DaylightName) then
         begin
          {Return Result}
          Result:=Timezone;
          Exit;
         end;
       end;
      {Get Next}
      Timezone:=Timezone.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(TimezoneTableLock);
   end;
  end;
end;

{==============================================================================}

function TimezoneEnumerate(Callback:TTimezoneEnumerate;Data:Pointer):LongWord;
var
 Timezone:PTimezoneEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Callback}
 if not Assigned(Callback) then Exit;

 {Acquire Lock}
 if CriticalSectionLock(TimezoneTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Timezone}
    Timezone:=TimezoneTable;
    while Timezone <> nil do
     begin
      {Check State}
      if Timezone.Signature = TIMEZONE_SIGNATURE then
       begin
        if Callback(Timezone,Data) <> ERROR_SUCCESS then Exit;
       end;

      {Get Next}
      Timezone:=Timezone.Next;
     end;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(TimezoneTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}
{==============================================================================}
{Timezone Helper Functions}
function TimezoneGetCount:LongWord;
{Get the current timezone count}
begin
 {}
 Result:=TimezoneTableCount;
end;

{==============================================================================}

function TimezoneGetDefault:PTimezoneEntry;
{Get the current default timezone}
begin
 {}
 Result:=TimezoneDefault;
end;

{==============================================================================}

function TimezoneSetDefault(Timezone:PTimezoneEntry):LongWord;
{Set the current default timezone}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Timezone}
 if Timezone = nil then Exit;
 if Timezone.Signature <> TIMEZONE_SIGNATURE then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(TimezoneTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check Timezone}
    if TimezoneCheck(Timezone) <> Timezone then Exit;

    {Set Timezone Default}
    TimezoneDefault:=Timezone;

    {Allocate Default Name}
    SetLength(TIMEZONE_DEFAULT_NAME,TIMEZONE_NAME_LENGTH - 1);

    {Set Timezone Defaults}
    StrLCopy(PChar(TIMEZONE_DEFAULT_NAME),Timezone.Name,TIMEZONE_NAME_LENGTH - 1);

    {Update Default Name}
    SetLength(TIMEZONE_DEFAULT_NAME,StrLen(PChar(TIMEZONE_DEFAULT_NAME)));

    {Update TZ Environment}
    TimezoneUpdateEnvironment;

    {Update Timezone Offset}
    TimezoneUpdateOffset;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(TimezoneTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function TimezoneCheck(Timezone:PTimezoneEntry):PTimezoneEntry;
{Check if the supplied Timezone is in the Timezone table}
var
 Current:PTimezoneEntry;
begin
 {}
 Result:=nil;

 {Check Timezone}
 if Timezone = nil then Exit;
 if Timezone.Signature <> TIMEZONE_SIGNATURE then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(TimezoneTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Timezone}
    Current:=TimezoneTable;
    while Current <> nil do
     begin
      {Check Timezone}
      if Current = Timezone then
       begin
        Result:=Timezone;
        Exit;
       end;

      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(TimezoneTableLock);
   end;
  end;
end;

{==============================================================================}

function TimezoneUpdateOffset:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Default}
 if TimezoneDefault = nil then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(TimezoneTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check Update}
    Result:=ERROR_IN_PROGRESS;
    if TIMEZONE_UPDATE_CURRENT then Exit;

    {Start Update}
    TIMEZONE_UPDATE_CURRENT:=True;
    try
     {Check Timezone}
     if TimezoneDefault = nil then Exit;
     if TimezoneDefault.Signature <> TIMEZONE_SIGNATURE then Exit;

     {Check Timezone}
     if TimezoneCheck(TimezoneDefault) <> TimezoneDefault then Exit;

     {Check State}
     if TimezoneGetState(TimezoneDefault) = TIME_ZONE_ID_DAYLIGHT then
      begin
       {Daylight Offset}
       TIMEZONE_TIME_OFFSET:=TimezoneDefault.Bias + TimezoneDefault.DaylightBias;
      end
     else
      begin
       {Standard Offset}
       TIMEZONE_TIME_OFFSET:=TimezoneDefault.Bias;
      end;

     {Return Result}
     Result:=ERROR_SUCCESS;
    finally
     {End Update}
     TIMEZONE_UPDATE_CURRENT:=False;
    end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(TimezoneTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function TimezoneUpdateEnvironment:LongWord;
{Update the TZ environment variable to represent the current timezone}
{See: https://www.gnu.org/software/libc/manual/html_node/TZ-Variable.html}
var
 Bias:LongInt;
 StandardName:String;
 DaylightName:String;
 StandardBias:String;
 DaylightBias:String;
 StandardStart:String;
 DaylightStart:String;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Default}
 if TimezoneDefault = nil then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(TimezoneTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check Timezone}
    if TimezoneDefault = nil then Exit;
    if TimezoneDefault.Signature <> TIMEZONE_SIGNATURE then Exit;

    {Check Timezone}
    if TimezoneCheck(TimezoneDefault) <> TimezoneDefault then Exit;

    {Get Standard Name}
    StandardName:=TimezoneNameToAbbreviation(TimezoneDefault.StandardName);

    {Get Standard Bias}
    StandardBias:='+';
    if TimezoneDefault.Bias < 0 then StandardBias:='-';
    StandardBias:=StandardBias + IntToStr(Abs(TimezoneDefault.Bias) div 60);
    if Abs(TimezoneDefault.Bias) mod 60 > 0 then StandardBias:=StandardBias + ':' + IntToStr(Abs(TimezoneDefault.Bias) mod 60);

    {Get Daylight Defaults}
    DaylightName:='';
    DaylightBias:='';
    DaylightStart:='';
    StandardStart:='';
    if (TimezoneDefault.DaylightBias <> 0) and (TimezoneDefault.DaylightStart.wMonth > 0) and (TimezoneDefault.DaylightStart.wDay > 0) then
     begin
      {Get Daylight Name}
      DaylightName:=TimezoneNameToAbbreviation(TimezoneDefault.DaylightName);

      {Get Daylight Bias}
      DaylightBias:='';
      if Abs(TimezoneDefault.DaylightBias) <> 60 then
       begin
        Bias:=TimezoneDefault.Bias + TimezoneDefault.DaylightBias;

        DaylightBias:='+';
        if Bias < 0 then DaylightBias:='-';
        DaylightBias:=DaylightBias + IntToStr(Abs(Bias) div 60);
        if Abs(Bias) mod 60 > 0 then DaylightBias:=DaylightBias + ':' + IntToStr(Abs(Bias) mod 60);
       end;

      {Get Daylight Start}
      DaylightStart:=',M' + IntToStr(TimezoneDefault.DaylightStart.wMonth) + '.' + IntToStr(TimezoneDefault.DaylightStart.wDay) + '.' + IntToStr(TimezoneDefault.DaylightStart.wDayOfWeek);
      if TimezoneDefault.DaylightStart.wHour <> 2 then
       begin
        DaylightStart:=DaylightStart + '/' + IntToStr(TimezoneDefault.DaylightStart.wHour);
        if TimezoneDefault.DaylightStart.wMinute <> 0 then DaylightStart:=DaylightStart + ':' + IntToStr(TimezoneDefault.DaylightStart.wMinute);
       end;

      {Get Standard Start}
      StandardStart:=',M' + IntToStr(TimezoneDefault.StandardStart.wMonth) + '.' + IntToStr(TimezoneDefault.StandardStart.wDay) + '.' + IntToStr(TimezoneDefault.StandardStart.wDayOfWeek);
      if TimezoneDefault.StandardStart.wHour <> 2 then
       begin
        StandardStart:=StandardStart + '/' + IntToStr(TimezoneDefault.StandardStart.wHour);
        if TimezoneDefault.StandardStart.wMinute <> 0 then StandardStart:=StandardStart + ':' + IntToStr(TimezoneDefault.StandardStart.wMinute);
       end;
     end;

    {Set TZ Environment}
    EnvironmentSet('TZ',StandardName + StandardBias + DaylightName + DaylightBias + DaylightStart + StandardStart);

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(TimezoneTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function TimezoneCalculateOffset(const DateTime:TDateTime;var Offset:LongInt;var Daylight:Boolean):LongWord;
{Calculate the Timezone Offset at the given date and time in the current timezone}
{DateTime: The date and time to calculate the offset for (Assumed to be Local)}
{Offset: The returned Offset in minutes}
{Daylight: True on return if daylight savings is in effect at the specified date and time}
{Return: ERROR_SUCCESS if the offset was calculated or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Default}
 if TimezoneDefault = nil then Exit;

 {Get Offset}
 Offset:=TimezoneGetActiveBiasEx(TimezoneDefault,DateTime);

 {Get Daylight}
 Daylight:=TimezoneGetStateEx(TimezoneDefault,DateTime) = TIME_ZONE_ID_DAYLIGHT;

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function TimezoneStartToDateTime(const AStart:SYSTEMTIME;AYear:Word):TDateTime;
{Calculate the start date and time from the start date of a timezone}
const
 MonthDays:array[Boolean,1..12] of Byte =
  ((31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31),
   (31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31));
var
 Count:Integer;
 DayInMonth:Integer;
 Difference:Integer;
 DaysInMonth:Integer;
 StartOfMonth:TDateTime;
begin
 {}
 Result:=0;

 {Check Year}
 if AStart.wYear = 0 then
  begin
   {Check Day, Month, DayOfWeek}
   if not (AStart.wDay > 0) then Exit;
   if not AStart.wMonth in [1..12] then Exit;
   if not (AStart.wDayOfWeek + 1) in [1..7] then Exit;

   {Get First Day of Month}
   StartOfMonth:=EncodeDate(AYear,AStart.wMonth,1);

   {Get Difference}
   Difference:=(AStart.wDayOfWeek + 1) - DayOfWeek(StartOfMonth);
   if Difference < 0 then Difference:=7 + Difference;
   DayInMonth:=1 + Difference;

   {Get Days in Month}
   DaysInMonth:=MonthDays[IsLeapYear(AYear),AStart.wMonth];
   DaysInMonth:=DaysInMonth - 7;

   {Find Day of Month}
   for Count:=2 to AStart.wDay do
    begin
     if DayInMonth > DaysInMonth then
      begin
       Break;
      end
     else
      begin
       Inc(DayInMonth,7);
      end;
    end;

   {Encode to DateTime}
   Result:=EncodeDate(AYear,AStart.wMonth,DayInMonth) + EncodeTime(AStart.wHour,AStart.wMinute,AStart.wSecond,AStart.wMilliseconds);
  end;
end;

{==============================================================================}

function TimezoneStartToDescription(const AStart:SYSTEMTIME):String;
{Get the description of the start date of a timezone}
var
 WorkBuffer:String;
begin
 {}
 Result:='';

 {Check Year}
 if AStart.wYear = 0 then
  begin
   {Check Month}
   if AStart.wMonth = 0 then
    begin
     Result:='None';
    end
   else
    begin
     {Get Day}
     case AStart.wDay of
      1:Result:='First ';
      2:Result:='Second ';
      3:Result:='Third ';
      4:Result:='Forth ';
      5:Result:='Last ';
     else
      Result:='Unknown ';
     end;

     {Get DayOfWeek}
     case AStart.wDayOfWeek of
      0:Result:=Result + 'Sunday in ';
      1:Result:=Result + 'Monday in ';
      2:Result:=Result + 'Tuesday in ';
      3:Result:=Result + 'Wednesday in ';
      4:Result:=Result + 'Thursday in ';
      5:Result:=Result + 'Friday in ';
      6:Result:=Result + 'Saturday in ';
     else
      Result:=Result + 'Unknown in ';
     end;

     {Get Month}
     case AStart.wMonth of
      1:Result:=Result + 'January at ';
      2:Result:=Result + 'February at ';
      3:Result:=Result + 'March at ';
      4:Result:=Result + 'April at ';
      5:Result:=Result + 'May at ';
      6:Result:=Result + 'June at ';
      7:Result:=Result + 'July at ';
      8:Result:=Result + 'August at ';
      9:Result:=Result + 'September at ';
      10:Result:=Result + 'October at ';
      11:Result:=Result + 'November at ';
      12:Result:=Result + 'December at ';
     else
      Result:=Result + 'Unknown at ';
     end;

     {Get Hour}
     WorkBuffer:=IntToStr(AStart.wHour);
     if Length(WorkBuffer) < 2 then WorkBuffer:='0' + WorkBuffer;
     Result:=Result + WorkBuffer + ':';

     {Get Minute}
     WorkBuffer:=IntToStr(AStart.wMinute);
     if Length(WorkBuffer) < 2 then WorkBuffer:='0' + WorkBuffer;
     Result:=Result + WorkBuffer + ':';

     {Get Second}
     WorkBuffer:=IntToStr(AStart.wSecond);
     if Length(WorkBuffer) < 2 then WorkBuffer:='0' + WorkBuffer;
     Result:=Result + WorkBuffer;
    end;
  end;
end;

{==============================================================================}

function TimezoneNameReplaceChar(const AName:String;AChar,AReplace:Char):String;
{Same as ReplaceChar in UltiboUtils, reproduced here to avoid including extra units}
var
 Count:Integer;
 WorkBuffer:String;
begin
 {}
 {Check Name}
 WorkBuffer:=AName;
 for Count:=1 to Length(WorkBuffer) do
  begin
   {Replace Char}
   if WorkBuffer[Count] = AChar then
    begin
     WorkBuffer[Count]:=AReplace;
    end;
  end;

 {Return Result}
 Result:=WorkBuffer;
end;

{==============================================================================}

function TimezoneNameToAbbreviation(const AName:String):String;
{Get the abbreviation of a timezone name (eg Central Standard Time = CST)}
var
 Count:Integer;
 WorkBuffer:String;
begin
 {}
 {Abbreviate Name}
 WorkBuffer:='';
 for Count:=1 to Length(AName) do
  begin
   {Check for first character or space}
   if Count = 1 then
    begin
     WorkBuffer:=AName[Count];
    end
   else if (AName[Count] = ' ') and (Count < Length(AName)) then
    begin
     WorkBuffer:=WorkBuffer + AName[Count + 1];
    end;
  end;

 {Return Result}
 Result:=Uppercase(WorkBuffer);
end;

{==============================================================================}
{==============================================================================}

initialization
 TimezoneInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
