{
Ultibo Global Type Definitions.

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


Global Types
============


}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit GlobalTypes; 

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

type
 {Method types}
 TGetLastError = function:LongWord;
 TSetLastError = procedure(LastError:LongWord);

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
 {Compatibility types} 
 {Boolean types}
 BOOL = LongBool;

 {Signed types}
 {INT = Integer;} {Declared in system unit}
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
 PLONG = ^LONG;
 LPLONG = ^LONG;
 LPDWORD = ^DWORD;
 INT_PTR = PtrInt;    {Integer}
 UINT_PTR = PtrUInt;  {LongWord}
 LONG_PTR = PtrInt;   {LongInt}
 
 ULONG_PTR = PtrUInt; {LongWord}
 DWORD_PTR = ULONG_PTR;
 PDWORD_PTR = ^DWORD_PTR;
 
 TFarProc = Pointer;
 
 {64bit types}
 LONGLONG = Int64;
 ULONGLONG = Int64;
 DWORDLONG = ULONGLONG;
 
 {Size types}
 SIZE_T = ULONG_PTR;
 
 {String types}
 LPSTR = ^CHAR;
 LPCSTR = ^CHAR;
 
 {Wide String types}
 WCHAR = WideChar;
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
