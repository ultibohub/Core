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
 {Color Format types} 
 PColorFormatARGB32 = ^TColorFormatARGB32;
 TColorFormatARGB32 = packed record
  Alpha:Byte;
  Red:Byte;
  Green:Byte;
  Blue:Byte;
 end;
 
 //To Do //Continuing
 
 PColorFormatRGB24 = ^TColorFormatRGB24;
 TColorFormatRGB24 = packed record
  Red:Byte;
  Green:Byte;
  Blue:Byte;
 end;
 
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
 HANDLE = LongWord;
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
