{
Ultibo Global Type Definitions.

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


Global Types
============


}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

{$IFNDEF FPC_DOTTEDUNITS}
unit GlobalTypes;
{$ENDIF FPC_DOTTEDUNITS}

interface

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
{Global types}
type
 {Handle types}
 TSpinHandle = THandle;
 TMutexHandle = THandle;
 TCriticalSectionHandle = THandle;
 TSemaphoreHandle = THandle;
 TSynchronizerHandle = THandle;
 TConditionHandle = THandle;
 TCompletionHandle = THandle;
 TListHandle = THandle;
 TQueueHandle = THandle;
 TThreadHandle = THandle;
 TMessageslotHandle = THandle;
 TMailslotHandle = THandle;
 TBufferHandle = THandle;
 TEventHandle = THandle;

 TTimerHandle = THandle;
 TWorkerHandle = THandle;
 TWindowHandle = THandle;
 TFontHandle = THandle;
 TKeymapHandle = THandle;

{==============================================================================}
type
 {Pointer types}
 PMutexHandle = ^TMutexHandle;
 PCriticalSectionHandle = ^TCriticalSectionHandle;
 PSemaphoreHandle = ^TSemaphoreHandle;
 PSynchronizerHandle = ^TSynchronizerHandle;
 PConditionHandle = ^TConditionHandle;
 PCompletionHandle = ^TCompletionHandle;
 PListHandle = ^TListHandle;
 PQueueHandle = ^TQueueHandle;
 PThreadHandle = ^TThreadHandle;
 PMessageslotHandle = ^TMessageslotHandle;
 PMailslotHandle = ^TMailslotHandle;
 PBufferHandle = ^TBufferHandle;
 PEventHandle = ^TEventHandle;

 PTimerHandle = ^TTimerHandle;
 PWorkerHandle = ^TWorkerHandle;
 PWindowHandle = ^TWindowHandle;
 PFontHandle = ^TFontHandle;
 PKeymapHandle = ^TKeymapHandle;

{==============================================================================}
type
 {Method types}
 {Prototype for Get/SetLastError Handlers}
 TGetLastError = function:LongWord;
 TSetLastError = procedure(LastError:LongWord);

 {Prototype for First/LastBitSet Handlers}
 TFirstBitSet = function(Value:LongWord):LongWord;
 TLastBitSet = function(Value:LongWord):LongWord;

 {Prototype for CountLeading/TrailingZeros Handlers}
 TCountLeadingZeros = function(Value:LongWord):LongWord;
 TCountTrailingZeros = function(Value:LongWord):LongWord;

{==============================================================================}
type
 {Color Format types (Generic)}
 PColorFormat32 = ^TColorFormat32;
 TColorFormat32 = packed record
  {$IFDEF FPC_BIG_ENDIAN}
  case Integer of
   0:(A:Byte;
      B:Byte;
      C:Byte;
      D:Byte);
   1:(Value:LongWord);
  {$ELSE FPC_BIG_ENDIAN}
  case Integer of
   0:(D:Byte;
      C:Byte;
      B:Byte;
      A:Byte);
   1:(Value:LongWord);
  {$ENDIF FPC_BIG_ENDIAN}
 end;

 PColorFormat24 = ^TColorFormat24;
 TColorFormat24 = packed record
  {$IFDEF FPC_BIG_ENDIAN}
  A:Byte;
  B:Byte;
  C:Byte;
  {$ELSE FPC_BIG_ENDIAN}
  C:Byte;
  B:Byte;
  A:Byte;
  {$ENDIF FPC_BIG_ENDIAN}
 end;

 PColorFormat16 = ^TColorFormat16;
 TColorFormat16 = packed record
  Value:Word;
 end;

 PColorFormat8 = ^TColorFormat8;
 TColorFormat8 = packed record
  Value:Byte;
 end;

 {Color Format types (RGB)}
 PColorFormatARGB32 = ^TColorFormatARGB32;
 TColorFormatARGB32 = packed record
  {$IFDEF FPC_BIG_ENDIAN}
  Alpha:Byte;
  Red:Byte;
  Green:Byte;
  Blue:Byte;
  {$ELSE FPC_BIG_ENDIAN}
  Blue:Byte;
  Green:Byte;
  Red:Byte;
  Alpha:Byte;
  {$ENDIF FPC_BIG_ENDIAN}
 end;

 PColorFormatRGBA32 = ^TColorFormatRGBA32;
 TColorFormatRGBA32 = packed record
  {$IFDEF FPC_BIG_ENDIAN}
  Red:Byte;
  Green:Byte;
  Blue:Byte;
  Alpha:Byte;
  {$ELSE FPC_BIG_ENDIAN}
  Alpha:Byte;
  Blue:Byte;
  Green:Byte;
  Red:Byte;
  {$ENDIF FPC_BIG_ENDIAN}
 end;

 PColorFormatRGB24 = ^TColorFormatRGB24;
 TColorFormatRGB24 = packed record
  {$IFDEF FPC_BIG_ENDIAN}
  Red:Byte;
  Green:Byte;
  Blue:Byte;
  {$ELSE FPC_BIG_ENDIAN}
  Blue:Byte;
  Green:Byte;
  Red:Byte;
  {$ENDIF FPC_BIG_ENDIAN}
 end;

 {Color Format types (BGR)}
 PColorFormatABGR32 = ^TColorFormatABGR32;
 TColorFormatABGR32 = packed record
  {$IFDEF FPC_BIG_ENDIAN}
  Alpha:Byte;
  Blue:Byte;
  Green:Byte;
  Red:Byte;
  {$ELSE FPC_BIG_ENDIAN}
  Red:Byte;
  Green:Byte;
  Blue:Byte;
  Alpha:Byte;
  {$ENDIF FPC_BIG_ENDIAN}
 end;

 PColorFormatBGRA32 = ^TColorFormatBGRA32;
 TColorFormatBGRA32 = packed record
  {$IFDEF FPC_BIG_ENDIAN}
  Blue:Byte;
  Green:Byte;
  Red:Byte;
  Alpha:Byte;
  {$ELSE FPC_BIG_ENDIAN}
  Alpha:Byte;
  Red:Byte;
  Green:Byte;
  Blue:Byte;
  {$ENDIF FPC_BIG_ENDIAN}
 end;

 PColorFormatBGR24 = ^TColorFormatBGR24;
 TColorFormatBGR24 = packed record
  {$IFDEF FPC_BIG_ENDIAN}
  Blue:Byte;
  Green:Byte;
  Red:Byte;
  {$ELSE FPC_BIG_ENDIAN}
  Red:Byte;
  Green:Byte;
  Blue:Byte;
  {$ENDIF FPC_BIG_ENDIAN}
 end;

 {Note: RGB16/RGB15/RGB8 and BGR equivalents cannot be completely represented as a record type}

{==============================================================================}
type
 {Display Settings type (Generic)}
 TDisplaySettings = record
  DisplayNumber:LongWord;
  Width:LongWord;
  Height:LongWord;
  Depth:LongWord;
  Pitch:LongWord;
  VirtualWidth:LongWord;
  VirtualHeight:LongWord;
  VirtualWidthOffset:LongWord;
  VirtualHeightOffset:LongWord;
  FramebufferAddress:PtrUInt;
 end;

{==============================================================================}
type
 {Compatibility types}
 {Boolean types}
 BOOL = ByteBool; {LongBool;} {Compatible with built in GCC _Bool type}

 {Signed types}
 INT = Integer;
 SHORT = Smallint;
 LONG = LongInt;

 {Unsigned types}
 UINT = LongWord;
 UCHAR = Byte;
 USHORT = Word;
 ULONG = LongWord;

 {Pointer types}
 PVOID = Pointer;
 LPVOID = Pointer;
 LPCVOID = Pointer;
 LPBOOL = ^BOOL;
 PINT = ^INT;
 PSHORT = ^SHORT;
 PLONG = ^LONG;
 LPLONG = ^LONG;
 LPDWORD = ^DWORD;
 PUINT = ^UINT;
 PUCHAR = ^UCHAR;
 PUSHORT = ^USHORT;
 PULONG = ^ULONG;
 INT_PTR = PtrInt;    {Integer}
 UINT_PTR = PtrUInt;  {LongWord}
 LONG_PTR = PtrInt;   {LongInt}

 ULONG_PTR = PtrUInt; {LongWord}
 DWORD_PTR = ULONG_PTR;
 PDWORD_PTR = ^DWORD_PTR;

 TFarProc = Pointer;

 {64bit types}
 LONGLONG = Int64;
 ULONGLONG = UInt64;
 DWORDLONG = ULONGLONG;

 {Size types}
 SIZE_T = SizeUInt; {ULONG_PTR;}
 SSIZE_T = SizeInt; {LONG_PTR;}

 {String types}
 LPSTR = ^AnsiCHAR;
 LPCSTR = ^AnsiCHAR;

 {Wide String types}
 WCHAR = WideChar;
 PWCHAR = ^WCHAR;
 LPWSTR = ^WCHAR;
 LPCWSTR = ^WCHAR;

 {Handle types}
 HANDLE = PtrUInt;  {LongWord;}
 PHANDLE = ^HANDLE;
 LPHANDLE = ^HANDLE;
 {THandle = HANDLE;} {Declared in system unit}
 HLOCAL = HANDLE;
 HGLOBAL = HANDLE;
 HWND = HANDLE;

 {Parameter types}
 WPARAM = UINT_PTR;
 LPARAM = LONG_PTR;
 LRESULT = LONG_PTR;

 {Procedure types}
 FARPROC = function: Integer;

 {C types}
 int8_t = Shortint;
 pint8_t = PShortint;

 uint8_t = Byte;
 puint8_t = PByte;

 int16_t = Smallint;
 pint16_t = PSmallint;

 uint16_t = Word;
 puint16_t = PWord;

 int32_t = LongInt;
 pint32_t = PLongInt;

 uint32_t = LongWord;
 puint32_t = PLongWord;

 int64_t = Int64;
 pint64_t = PInt64;

 uint64_t = QWord;
 puint64_t = PQWord;

 float_t = Single;
 pfloat_t = PSingle;

 double_t = Double;
 pdouble_t = PDouble;

 patomic_t = ^atomic_t;
 atomic_t = record
  counter: LongInt;
 end;

 patomic64_t = ^atomic64_t;
 atomic64_t = record
  counter: Int64;
 end;

 uintptr_t = DWORD_PTR;
 puintptr_t = PDWORD_PTR;

 {$IFDEF SYSCALLS_USE_LONG_TIME_T}
 time_t = PtrInt;   {long}
 {$ELSE}
 time_t = Int64;    {int64_t}
 {$ENDIF}

{==============================================================================}
type
 {Socket types}
 TSocket = UINT_PTR;

 WSAEVENT = THandle;

{==============================================================================}
type
 {Structure types}
 {Overlapped}
 LPOVERLAPPED = ^OVERLAPPED;
 _OVERLAPPED = record
  Internal: ULONG_PTR;
  InternalHigh: ULONG_PTR;
  Union: record
  case Integer of
    0: (
      Offset: DWORD;
      OffsetHigh: DWORD);
    1: (
      Pointer: PVOID);
  end;
  hEvent: HANDLE;
 end;
 OVERLAPPED = _OVERLAPPED;
 TOverlapped = OVERLAPPED;
 POverlapped = LPOVERLAPPED;

 //To Do //A place to put all of the compatibility type definitions
 //eg BOOL, LONG, PVOID etc etc

 //See: Ultibo, Unicode, Locale, Security etc

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}

end.
