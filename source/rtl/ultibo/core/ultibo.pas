{
Ultibo interface unit.

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


Ultibo Interface
================

  Notes:   TIMEZONE_TIME_OFFSET Usage:

            UTC = LocalTime + TIMEZONE_TIME_OFFSET
            LocalTime = UTC - TIMEZONE_TIME_OFFSET
            (TIMEZONE_TIME_OFFSET can be negative)

           TIMEZONE_TIME_ADJUST Usage:

            Time = Adjusted + TIMEZONE_TIME_ADJUST
            Adjusted = Time - TIMEZONE_TIME_ADJUST
            (TIMEZONE_TIME_ADJUST can be negative)

            TIMEZONE_TIME_ADJUST = ActiveOffset - StandardOffset

            Ultibo returns Time (UTC)
            FileSystem stores Adjusted (UTC)
            ActiveOffset is the value used by Ultibo to return UTC or Local (Normally TIMEZONE_TIME_OFFSET)

           Examples:

             Australian Eastern Time
             =======================
             +1000 (AEST) = -600 ActiveOffset
             +1100 (AEDT) = -660 ActiveOffset

             ActiveOffset = -660 / StandardOffset = -600 / TIMEZONE_TIME_ADJUST = -60

               Local         Offset      UTC           Adjust    Adjusted
             720 (12.00)  +   -660   =   60 (1.00)  -   -60   =   120 (2.00)

               Adjusted      Adjust      UTC           Offset    Local
             120 (2.00)   +   -60    =   60 (1.00)  -   -660  =   720 (12.00)

             Pacific Time
             ============
             -0800 (STD) = 480 ActiveOffset
             -0700 (DLT) = 420 ActiveOffset

             ActiveOffset = 420 / StandardOffset = 480 / TIMEZONE_TIME_ADJUST = -60

               Local         Offset     UTC            Adjust    Adjusted
             720 (12.00)  +   420   =  1140 (19.00)  -  -60   =   1200 (20.00)

               Adjusted      Adjust     UTC            Offset    Local
             1200 (2.00)  +   -60   =  1140 (19.00)  -  420   =   720 (12.00)

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

{$IFNDEF FPC_DOTTEDUNITS}
unit Ultibo;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Core.GlobalConfig,
  Core.GlobalConst,
  Core.GlobalTypes,
  Core.Platform,
  Core.Threads,
  Core.HeapManager,
  Core.Devices,
  Core.Locale,
  Core.Timezone,
  Core.Unicode,
  Core.Security,
  System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  HeapManager,
  Devices,
  Locale,
  Timezone,
  Unicode,
  Security,
  SysUtils;
{$ENDIF FPC_DOTTEDUNITS}

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
{Local definitions}
{--$DEFINE ULTIBO_USE_SYSUTILS_GUID}  {Use the GUID functions from SysUtils instead of the built it functions (Default: Off)}

{==============================================================================}
{Compatibility constants}
const
 {64bit Constants}
 MAXLONGLONG = $7fffffffffffffff;

const
 {Thread Constants}
 CREATE_SUSPENDED = $00000004;

const
 {Message Constants}
 PM_NOREMOVE       = $0000;
 PM_REMOVE         = $0001;
 PM_NOYIELD        = $0002;

const
 {Timezone Constants}
 TIME_ZONE_ID_UNKNOWN  = Timezone.TIME_ZONE_ID_UNKNOWN;
 TIME_ZONE_ID_STANDARD = Timezone.TIME_ZONE_ID_STANDARD;
 TIME_ZONE_ID_DAYLIGHT = Timezone.TIME_ZONE_ID_DAYLIGHT;
 TIME_ZONE_ID_INVALID  = Timezone.TIME_ZONE_ID_INVALID;

const
 {Shutdown/Restart Constants}
 EWX_LOGOFF       = $00000000;
 EWX_SHUTDOWN     = $00000001;
 EWX_REBOOT       = $00000002;
 EWX_FORCE        = $00000004;
 EWX_POWEROFF     = $00000008;
 EWX_FORCEIFHUNG  = $00000010;

const
 {Drive Type Constants}
 DRIVE_UNKNOWN     = 0;
 DRIVE_NO_ROOT_DIR = 1;
 DRIVE_REMOVABLE   = 2;
 DRIVE_FIXED       = 3;
 DRIVE_REMOTE      = 4;
 DRIVE_CDROM       = 5;
 DRIVE_RAMDISK     = 6;

const
 {Symbolic Link Constants}
 SYMBOLIC_LINK_FLAG_FILE      = $00000000; {The link target is a file}
 SYMBOLIC_LINK_FLAG_DIRECTORY = $00000001; {The link target is a directory}

const
 {Local Memory Constants}
 LMEM_FIXED          = $0000;
 LMEM_MOVEABLE       = $0002;
 LMEM_NOCOMPACT      = $0010;
 LMEM_NODISCARD      = $0020;
 LMEM_ZEROINIT       = $0040;
 LMEM_MODIFY         = $0080;
 LMEM_DISCARDABLE    = $0F00;
 LMEM_VALID_FLAGS    = $0F72;
 LMEM_INVALID_HANDLE = $8000;

 LHND = (LMEM_MOVEABLE or LMEM_ZEROINIT);
 LPTR = (LMEM_FIXED or LMEM_ZEROINIT);

 NONZEROLHND = (LMEM_MOVEABLE);
 NONZEROLPTR = (LMEM_FIXED);

const
 {Global Memory Constants}
 GMEM_FIXED          = $0000;
 GMEM_MOVEABLE       = $0002;
 GMEM_NOCOMPACT      = $0010;
 GMEM_NODISCARD      = $0020;
 GMEM_ZEROINIT       = $0040;
 GMEM_MODIFY         = $0080;
 GMEM_DISCARDABLE    = $0100;
 GMEM_NOT_BANKED     = $1000;
 GMEM_SHARE          = $2000;
 GMEM_DDESHARE       = $2000;
 GMEM_NOTIFY         = $4000;
 GMEM_LOWER          = GMEM_NOT_BANKED;
 GMEM_VALID_FLAGS    = $7F72;
 GMEM_INVALID_HANDLE = $8000;

 GHND = (GMEM_MOVEABLE or GMEM_ZEROINIT);
 GPTR = (GMEM_FIXED or GMEM_ZEROINIT);

const
 {Virtual Page Constants}
 PAGE_NOACCESS          = $01;
 PAGE_READONLY          = $02;
 PAGE_READWRITE         = $04;
 PAGE_WRITECOPY         = $08;
 PAGE_EXECUTE           = $10;
 PAGE_EXECUTE_READ      = $20;
 PAGE_EXECUTE_READWRITE = $40;
 PAGE_EXECUTE_WRITECOPY = $80;
 PAGE_GUARD             = $100;
 PAGE_NOCACHE           = $200;
 PAGE_WRITECOMBINE      = $400;

 {Virtual Memory Constants}
 MEM_COMMIT             = $1000;
 MEM_RESERVE            = $2000;
 MEM_DECOMMIT           = $4000;
 MEM_RELEASE            = $8000;
 MEM_FREE               = $10000;
 MEM_PRIVATE            = $20000;
 MEM_MAPPED             = $40000;
 MEM_RESET              = $80000;
 MEM_TOP_DOWN           = $100000;
 MEM_WRITE_WATCH        = $200000;
 MEM_PHYSICAL           = $400000;
 MEM_4MB_PAGES          = DWORD($80000000);
 SEC_FILE               = $800000;
 SEC_IMAGE              = $1000000;
 SEC_RESERVE            = $4000000;
 SEC_COMMIT             = DWORD($8000000);
 SEC_NOCACHE            = $10000000;
 MEM_IMAGE              = SEC_IMAGE;
 WRITE_WATCH_FLAG_RESET = $01;
 MEM_LARGE_PAGES        = $20000000;

const
 {Processor Architechture Constants}
 PROCESSOR_ARCHITECTURE_INTEL   = 0;
 PROCESSOR_ARCHITECTURE_MIPS    = 1;
 PROCESSOR_ARCHITECTURE_ALPHA   = 2;
 PROCESSOR_ARCHITECTURE_PPC     = 3;
 PROCESSOR_ARCHITECTURE_SHX     = 4;
 PROCESSOR_ARCHITECTURE_ARM     = 5;
 PROCESSOR_ARCHITECTURE_IA64    = 6;
 PROCESSOR_ARCHITECTURE_ALPHA64 = 7;
 PROCESSOR_ARCHITECTURE_MSIL    = 8;
 PROCESSOR_ARCHITECTURE_AMD64   = 9;

 PROCESSOR_ARCHITECTURE_UNKNOWN = $FFFF;

 {Processor Type Constants}
 PROCESSOR_INTEL_386     = 386;
 PROCESSOR_INTEL_486     = 486;
 PROCESSOR_INTEL_PENTIUM = 586;
 PROCESSOR_INTEL_IA64    = 2200;
 PROCESSOR_ARM_6         = 60999;
 PROCESSOR_ARM_7         = 70999;
 PROCESSOR_ARM_8         = 80999;

const
 {Version Info Constants}
 VER_PLATFORM_ULTIBO     = 10011;

const
 {File Attribute Constants}
 FILE_ATTRIBUTE_READONLY            = $00000001;
 FILE_ATTRIBUTE_HIDDEN              = $00000002;
 FILE_ATTRIBUTE_SYSTEM              = $00000004;
 FILE_ATTRIBUTE_DIRECTORY           = $00000010;
 FILE_ATTRIBUTE_ARCHIVE             = $00000020;
 FILE_ATTRIBUTE_DEVICE              = $00000040;
 FILE_ATTRIBUTE_NORMAL              = $00000080;
 FILE_ATTRIBUTE_TEMPORARY           = $00000100;
 FILE_ATTRIBUTE_SPARSE_FILE         = $00000200;
 FILE_ATTRIBUTE_REPARSE_POINT       = $00000400;
 FILE_ATTRIBUTE_COMPRESSED          = $00000800;
 FILE_ATTRIBUTE_OFFLINE             = $00001000;
 FILE_ATTRIBUTE_NOT_CONTENT_INDEXED = $00002000;
 FILE_ATTRIBUTE_ENCRYPTED           = $00004000;

const
 {Volume Info Constants}
 FILE_CASE_SENSITIVE_SEARCH         = $00000001;
 FILE_CASE_PRESERVED_NAMES          = $00000002;
 FILE_UNICODE_ON_DISK               = $00000004;
 FILE_PERSISTENT_ACLS               = $00000008;
 FILE_FILE_COMPRESSION              = $00000010;
 FILE_VOLUME_QUOTAS                 = $00000020;
 FILE_SUPPORTS_SPARSE_FILES         = $00000040;
 FILE_SUPPORTS_REPARSE_POINTS       = $00000080;
 FILE_SUPPORTS_REMOTE_STORAGE       = $00000100;
 FILE_VOLUME_IS_COMPRESSED          = $00008000;
 FILE_SUPPORTS_OBJECT_IDS           = $00010000;
 FILE_SUPPORTS_ENCRYPTION           = $00020000;
 FILE_NAMED_STREAMS                 = $00040000;
 FILE_READ_ONLY_VOLUME              = $00080000;

const
 {Dos Device Constants}
 DDD_RAW_TARGET_PATH       = $00000001;
 DDD_REMOVE_DEFINITION     = $00000002;
 DDD_EXACT_MATCH_ON_REMOVE = $00000004;
 DDD_NO_BROADCAST_SYSTEM   = $00000008;
 DDD_LUID_BROADCAST_DRIVE  = $00000010;

const
 {Final Path Flag Constants}
 FILE_NAME_NORMALIZED = $00000000; {Return the normalized file name. This is the default}
 FILE_NAME_OPENED     = $00000008; {Return the opened file name (not normalized) (Not Supported by Ultibo)}

 VOLUME_NAME_DOS      = $00000000; {Return the path with the drive letter. This is the default}
 VOLUME_NAME_GUID     = $00000001; {Return the path with a volume GUID path instead of the drive name (Ultibo returns the volume path, not GUID path)}
 VOLUME_NAME_NONE     = $00000004; {Return the path with no drive information (Not Supported by Ultibo)}
 VOLUME_NAME_NT       = $00000002; {Return the NT device object path (Ultibo returns the device path)}

{==============================================================================}
{Compatibility types}
type
 {Signed types}
 INT = GlobalTypes.INT;
 BOOL = GlobalTypes.BOOL; {Note: Declared in Windows as LongBool but declared here as ByteBool for GCC compatibility}
 LONG = GlobalTypes.LONG;

 {Unsigned types}
 UINT = GlobalTypes.UINT;
 UCHAR = GlobalTypes.UCHAR;
 USHORT = GlobalTypes.USHORT;
 ULONG = GlobalTypes.ULONG;

 {Pointer types}
 PVOID = GlobalTypes.PVOID;
 LPVOID = GlobalTypes.LPVOID;
 LPCVOID = GlobalTypes.LPCVOID;
 LPBOOL = GlobalTypes.LPBOOL; {See note above for BOOL declaration}
 PLONG = GlobalTypes.PLONG;
 LPLONG = GlobalTypes.LPLONG;
 LPDWORD = GlobalTypes.LPDWORD;
 INT_PTR = GlobalTypes.INT_PTR;
 UINT_PTR = GlobalTypes.UINT_PTR;
 LONG_PTR = GlobalTypes.LONG_PTR;

 ULONG_PTR = GlobalTypes.ULONG_PTR;
 DWORD_PTR = GlobalTypes.DWORD_PTR;
 PDWORD_PTR = GlobalTypes.PDWORD_PTR;

 {64bit types}
 LONGLONG = GlobalTypes.LONGLONG;
 ULONGLONG = GlobalTypes.ULONGLONG;
 DWORDLONG = GlobalTypes.DWORDLONG;

 PQuad = ^TQuad;
 _QUAD = record                // QUAD is for those times we want
   DoNotUseThisField:Double;   // an 8 byte aligned 8 byte long structure
 end;                          // which is NOT really a floating point
 QUAD = _QUAD;                 // number.  Use DOUBLE if you want an FP number.
 TQuad = _QUAD;
 UQUAD = QUAD;

 {Locale and Language Ids}
 LCID = Locale.LCID;
 PLCID = Locale.PLCID;
 LANGID = Locale.LANGID;
 PLANGID = Locale.PLANGID;

 {Language Group ID}
 LGRPID = Locale.LGRPID;
 {Locale type constant}
 LCTYPE = Locale.LCTYPE;
 {Calendar type constant}
 CALTYPE = Locale.CALTYPE;
 {Calendar ID}
 CALID = Locale.CALID;

type
 {Large Integer types}
 LPLARGE_INTEGER = ^LARGE_INTEGER;
 _LARGE_INTEGER = record
   case Integer of
   0: (
     LowPart: DWORD;
     HighPart: LONG);
   1: (
     QuadPart: LONGLONG);
 end;
 LARGE_INTEGER = _LARGE_INTEGER;
 TLargeInteger = LARGE_INTEGER;
 PLARGE_INTEGER = ^LARGE_INTEGER;
 PLargeInteger = LPLARGE_INTEGER;

 LPULARGE_INTEGER = ^ULARGE_INTEGER;
 ULARGE_INTEGER = record
   case Integer of
     0: (
       LowPart: DWORD;
       HighPart: DWORD);
     1: (
       QuadPart: ULONGLONG);
 end;
 TULargeInteger = ULARGE_INTEGER;
 PULargeInteger = LPULARGE_INTEGER;
 PULARGE_INTEGER = ^ULARGE_INTEGER;

type
 {Version Info types}
 LPOSVERSIONINFOA = ^OSVERSIONINFOA;
 _OSVERSIONINFOA = record
   dwOSVersionInfoSize:DWORD;
   dwMajorVersion:DWORD;
   dwMinorVersion:DWORD;
   dwBuildNumber:DWORD;
   dwPlatformId:DWORD;
   szCSDVersion:array [0..127] of AnsiCHAR;
 end;
 OSVERSIONINFOA = _OSVERSIONINFOA;
 TOsVersionInfoA = OSVERSIONINFOA;
 POsVersionInfoA = LPOSVERSIONINFOA;

 LPOSVERSIONINFOW = ^OSVERSIONINFOW;
 _OSVERSIONINFOW = record
   dwOSVersionInfoSize:DWORD;
   dwMajorVersion:DWORD;
   dwMinorVersion:DWORD;
   dwBuildNumber:DWORD;
   dwPlatformId:DWORD;
   szCSDVersion:array [0..127] of WCHAR;
 end;
 OSVERSIONINFOW = _OSVERSIONINFOW;
 TOsVersionInfoW = OSVERSIONINFOW;
 POsVersionInfoW = LPOSVERSIONINFOW;

 OSVERSIONINFO = OSVERSIONINFOA;
 POSVERSIONINFO = POSVERSIONINFOA;
 LPOSVERSIONINFO = LPOSVERSIONINFOA;
 TOSVersionInfo = TOSVersionInfoA;

type
 {System Info types}
 LPSYSTEM_INFO = ^SYSTEM_INFO;
 _SYSTEM_INFO = record
   wProcessorArchitecture:WORD;
   wReserved:WORD;
   dwPageSize:DWORD;
   lpMinimumApplicationAddress:LPVOID;
   lpMaximumApplicationAddress:LPVOID;
   dwActiveProcessorMask:DWORD_PTR;
   dwNumberOfProcessors:DWORD;
   dwProcessorType:DWORD;
   dwAllocationGranularity:DWORD;
   wProcessorLevel:WORD;
   wProcessorRevision:WORD;
 end;
 SYSTEM_INFO = _SYSTEM_INFO;
 TSystemInfo = SYSTEM_INFO;
 PSystemInfo = LPSYSTEM_INFO;

type
 {Computer Name types}
 _COMPUTER_NAME_FORMAT = (
   ComputerNameNetBIOS,
   ComputerNameDnsHostname,
   ComputerNameDnsDomain,
   ComputerNameDnsFullyQualified,
   ComputerNamePhysicalNetBIOS,
   ComputerNamePhysicalDnsHostname,
   ComputerNamePhysicalDnsDomain,
   ComputerNamePhysicalDnsFullyQualified,
   ComputerNameMax);
 COMPUTER_NAME_FORMAT = _COMPUTER_NAME_FORMAT;
 TComputerNameFormat = COMPUTER_NAME_FORMAT;

type
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

type
 {File Time types}
 LPFILETIME = ^FILETIME;
 _FILETIME = SysUtils.FILETIME;
 {_FILETIME = record
   dwLowDateTime:DWORD;
   dwHighDateTime:DWORD;
 end;} {FILETIME is now defined in SysUtils}
 FILETIME = _FILETIME;
 TFileTime = FILETIME;
 PFileTime = LPFILETIME;

type
 {File Search types}
 PWIN32_FIND_DATAA = ^WIN32_FIND_DATAA;
 _WIN32_FIND_DATAA = SysUtils.TWin32FindDataA;
 {_WIN32_FIND_DATAA = record
   dwFileAttributes:DWORD;
   ftCreationTime:FILETIME;
   ftLastAccessTime:FILETIME;
   ftLastWriteTime:FILETIME;
   nFileSizeHigh:DWORD;
   nFileSizeLow:DWORD;
   dwReserved0:DWORD;
   dwReserved1:DWORD;
   cFileName:array [0..MAX_PATH - 1] of AnsiCHAR;
   cAlternateFileName:array [0..13] of AnsiCHAR;
 end;} {TWin32FindDataA is now defined in SysUtils}
 WIN32_FIND_DATAA = _WIN32_FIND_DATAA;
 LPWIN32_FIND_DATAA = ^WIN32_FIND_DATAA;
 {TWin32FindDataA = WIN32_FIND_DATAA;} {TWin32FindDataA is now defined in SysUtils}
 PWin32FindDataA = PWIN32_FIND_DATAA;

 PWIN32_FIND_DATAW = ^WIN32_FIND_DATAW;
 _WIN32_FIND_DATAW = record
   dwFileAttributes: DWORD;
   ftCreationTime: FILETIME;
   ftLastAccessTime: FILETIME;
   ftLastWriteTime: FILETIME;
   nFileSizeHigh: DWORD;
   nFileSizeLow: DWORD;
   dwReserved0: DWORD;
   dwReserved1: DWORD;
   cFileName: array [0..MAX_PATH - 1] of WCHAR;
   cAlternateFileName: array [0..13] of WCHAR;
 end;
 WIN32_FIND_DATAW = _WIN32_FIND_DATAW;
 LPWIN32_FIND_DATAW = ^WIN32_FIND_DATAW;
 TWin32FindDataW = WIN32_FIND_DATAW;
 PWin32FindDataW = PWIN32_FIND_DATAW;

 WIN32_FIND_DATA = WIN32_FIND_DATAA;
 PWIN32_FIND_DATA = PWIN32_FIND_DATAA;
 LPWIN32_FIND_DATA = LPWIN32_FIND_DATAA;
 {TWin32FindData = TWin32FindDataA;} {TWin32FindData is now defined in SysUtils}
 PWin32FindData = PWin32FindDataA;

type
 {Stream Search types}
 _STREAM_INFO_LEVELS = (FindStreamInfoStandard, FindStreamInfoMaxInfoLevel);
 STREAM_INFO_LEVELS = _STREAM_INFO_LEVELS;
 TStreamInfoLevels = STREAM_INFO_LEVELS;

 _WIN32_FIND_STREAM_DATA = record
   StreamSize: LARGE_INTEGER;
   cStreamName: array [0..MAX_PATH + 35] of WCHAR;
 end;
 WIN32_FIND_STREAM_DATA = _WIN32_FIND_STREAM_DATA;
 PWIN32_FIND_STREAM_DATA = ^WIN32_FIND_STREAM_DATA;
 TWin32FindStreamData = WIN32_FIND_STREAM_DATA;
 PWin32FindStreamData = PWIN32_FIND_STREAM_DATA;

type
 {Timezone types}
 PTIME_ZONE_INFORMATION = Timezone.PTIME_ZONE_INFORMATION;
 _TIME_ZONE_INFORMATION = Timezone._TIME_ZONE_INFORMATION;
 TIME_ZONE_INFORMATION = Timezone.TIME_ZONE_INFORMATION;
 LPTIME_ZONE_INFORMATION = Timezone.LPTIME_ZONE_INFORMATION;
 TTimeZoneInformation = Timezone.TTimeZoneInformation;
 PTimeZoneInformation = Timezone.PTimeZoneInformation;

type
 {Memory Status types}
 LPMEMORYSTATUS = ^MEMORYSTATUS;
 _MEMORYSTATUS = record
   dwLength:DWORD;
   dwMemoryLoad:DWORD;
   dwTotalPhys:SIZE_T;
   dwAvailPhys:SIZE_T;
   dwTotalPageFile:SIZE_T;
   dwAvailPageFile:SIZE_T;
   dwTotalVirtual:SIZE_T;
   dwAvailVirtual:SIZE_T;
 end;
 MEMORYSTATUS = _MEMORYSTATUS;
 TMemoryStatus = MEMORYSTATUS;
 PMemoryStatus = LPMEMORYSTATUS;

 LPMEMORYSTATUSEX = ^MEMORYSTATUSEX;
 _MEMORYSTATUSEX = record
   dwLength:DWORD;
   dwMemoryLoad:DWORD;
   ullTotalPhys:DWORDLONG;
   ullAvailPhys:DWORDLONG;
   ullTotalPageFile:DWORDLONG;
   ullAvailPageFile:DWORDLONG;
   ullTotalVirtual:DWORDLONG;
   ullAvailVirtual:DWORDLONG;
   ullAvailExtendedVirtual:DWORDLONG;
 end;
 MEMORYSTATUSEX = _MEMORYSTATUSEX;
 TMemoryStatusEx = MEMORYSTATUSEX;
 PMemoryStatusEx = LPMEMORYSTATUSEX;

type
 {Security Attributes types}
 PSECURITY_ATTRIBUTES = ^SECURITY_ATTRIBUTES;
 _SECURITY_ATTRIBUTES = record
   nLength:DWORD;
   lpSecurityDescriptor:LPVOID;
   bInheritHandle:BOOL;
 end;
 SECURITY_ATTRIBUTES = _SECURITY_ATTRIBUTES;
 LPSECURITY_ATTRIBUTES = ^SECURITY_ATTRIBUTES;
 TSecurityAttributes = SECURITY_ATTRIBUTES;
 PSecurityAttributes = PSECURITY_ATTRIBUTES;

type
 {Thread Start types}
 PTHREAD_START_ROUTINE = function (lpThreadParameter:LPVOID):DWORD;
 LPTHREAD_START_ROUTINE = PTHREAD_START_ROUTINE;
 TThreadStartRoutine = PTHREAD_START_ROUTINE;

type
 {Point and Rect types}
 LPRECT = ^RECT;
 tagRECT = record
  left:LONG;
  top:LONG;
  right:LONG;
  bottom:LONG;
 end;
 RECT = tagRECT;
 NPRECT = ^tagRect;
 LPCRECT = ^tagRect;
 TRect = RECT;
 PRect = LPRECT;

 LPPOINT = ^POINT;
 tagPOINT = record
  x:LONG;
  y:LONG;
 end;
 NPPOINT = ^tagPoint;
 POINT = tagPOINT;
 TPoint = POINT;
 PPoint = LPPOINT;

type
 {Message types}
 LPMSG = ^MSG;
 tagMSG = record
  hThread:HANDLE;
  message:UINT;
  wParam:WPARAM;
  lParam:LPARAM;
  time:DWORD;
  pt:POINT;
 end;
 MSG = tagMSG;
 NPMSG = ^MSG;
 TMsg = MSG;
 PMsg = LPMSG;

type
 {List types}
 PLIST_ENTRY = ^LIST_ENTRY;
 _LIST_ENTRY = record
   Flink:PLIST_ENTRY;
   Blink:PLIST_ENTRY;
 end;
 LIST_ENTRY = _LIST_ENTRY;
 TListEntry = LIST_ENTRY;
 PListEntry = PLIST_ENTRY;
 PRLIST_ENTRY = ^LIST_ENTRY;

type
 {Critical Section types}
 PRTL_CRITICAL_SECTION_DEBUG = ^RTL_CRITICAL_SECTION_DEBUG;
 _RTL_CRITICAL_SECTION_DEBUG = record
   _Type:WORD;
   CreatorBackTraceIndex:WORD;
   CriticalSection:PRTL_CRITICAL_SECTION_DEBUG;
   ProcessLocksList:LIST_ENTRY;
   EntryCount:DWORD;
   ContentionCount:DWORD;
   Spare:array [0..1] of DWORD;
 end;
 RTL_CRITICAL_SECTION_DEBUG = _RTL_CRITICAL_SECTION_DEBUG;
 TRtlCriticalSectionDebug = RTL_CRITICAL_SECTION_DEBUG;
 PRtlCriticalSectionDebug = PRTL_CRITICAL_SECTION_DEBUG;

 PRTL_CRITICAL_SECTION = ^RTL_CRITICAL_SECTION;
 _RTL_CRITICAL_SECTION = record
   DebugInfo:PRTL_CRITICAL_SECTION_DEBUG;
   //
   //  The following three fields control entering and exiting the critical
   //  section for the resource
   //
   LockCount:LONG;
   RecursionCount:LONG;
   OwningThread:HANDLE;        // from the thread's ClientId->UniqueThread
   LockSemaphore:HANDLE;
   SpinCount:ULONG_PTR;        // force size on 64-bit systems when packed
 end;
 RTL_CRITICAL_SECTION = _RTL_CRITICAL_SECTION;
 TRtlCriticalSection = RTL_CRITICAL_SECTION;
 PRtlCriticalSection = PRTL_CRITICAL_SECTION;

 CRITICAL_SECTION = RTL_CRITICAL_SECTION;
 PCRITICAL_SECTION = PRTL_CRITICAL_SECTION;
 LPCRITICAL_SECTION = PRTL_CRITICAL_SECTION;
 TCriticalSection = CRITICAL_SECTION;
 PCriticalSection = PCRITICAL_SECTION;

type
 {Condition Variable types}
 PRTL_CONDITION_VARIABLE = ^RTL_CONDITION_VARIABLE;
 _RTL_CONDITION_VARIABLE = record
  Ptr:Pointer;
 end;
 RTL_CONDITION_VARIABLE = _RTL_CONDITION_VARIABLE;
 TRtlConditionVariable = RTL_CONDITION_VARIABLE;
 PRtlConditionVariable = PRTL_CONDITION_VARIABLE;

 CONDITION_VARIABLE = RTL_CONDITION_VARIABLE;
 PCONDITION_VARIABLE = PRTL_CONDITION_VARIABLE;

type
 {Counter types}
 PIO_COUNTERS = ^IO_COUNTERS;
 _IO_COUNTERS = record
   ReadOperationCount:Int64;
   WriteOperationCount:Int64;
   OtherOperationCount:Int64;
   ReadTransferCount:Int64;
   WriteTransferCount:Int64;
   OtherTransferCount:Int64;
 end;
 IO_COUNTERS = _IO_COUNTERS;
 TIoCounters = IO_COUNTERS;
 PIoCounters = PIO_COUNTERS;

type
 {Code Page types}
 LPCPINFO = Locale.LPCPINFO;
 _cpinfo = Locale._cpinfo;
 CPINFO = Locale.CPINFO;
 TCpInfo = Locale.TCpInfo;
 PCpInfo = Locale.PCpInfo;

 LPCPINFOEXA = Locale.LPCPINFOEXA;
 _cpinfoexA = Locale._cpinfoexA;
 CPINFOEXA = Locale.CPINFOEXA;
 TCpInfoExA = Locale.TCpInfoExA;
 PCpInfoExA = Locale.PCpInfoExA;

 LPCPINFOEXW = Locale.LPCPINFOEXW;
 _cpinfoexW = Locale._cpinfoexW;
 CPINFOEXW = Locale.CPINFOEXW;
 TCpInfoExW = Locale.TCpInfoExW;
 PCpInfoExW = Locale.PCpInfoExW;

 CPINFOEX = Locale.CPINFOEX;
 LPCPINFOEX = Locale.LPCPINFOEX;
 TCpInfoEx = Locale.TCpInfoEx;
 PCpInfoEx = Locale.PCpInfoEx;

type
 {Memory Information Types}
 PMEMORY_BASIC_INFORMATION = ^MEMORY_BASIC_INFORMATION;
 _MEMORY_BASIC_INFORMATION = record
   BaseAddress: Pointer;
   AllocationBase: Pointer;
   AllocationProtect: DWORD;
   RegionSize: SIZE_T;
   State: DWORD;
   Protect: DWORD;
   Type_: DWORD;
 end;
 MEMORY_BASIC_INFORMATION = _MEMORY_BASIC_INFORMATION;
 TMemoryBasicInformation = MEMORY_BASIC_INFORMATION;
 PMemoryBasicInformation = PMEMORY_BASIC_INFORMATION;

type
 {RGB Color Types}
 FXPT16DOT16 = Longint;
 LPFXPT16DOT16 = ^FXPT16DOT16;

 FXPT2DOT30 = Longint;
 LPFXPT2DOT30 = ^FXPT2DOT30;

 PCieXyz = ^TCieXyz;
 tagCIEXYZ = record
   ciexyzX: FXPT2DOT30;
   ciexyzY: FXPT2DOT30;
   ciexyzZ: FXPT2DOT30;
 end;
 CIEXYZ = tagCIEXYZ;
 LPCIEXYZ = ^CIEXYZ;
 TCieXyz = CIEXYZ;

 PCieXyzTriple = ^TCieXyzTriple;
 tagCIEXYZTRIPLE = record
   ciexyzRed: CIEXYZ;
   ciexyzGreen: CIEXYZ;
   ciexyzBlue: CIEXYZ;
 end;
 CIEXYZTRIPLE = tagCIEXYZTRIPLE;
 LPCIEXYZTRIPLE = ^CIEXYZTRIPLE;
 TCieXyzTriple = CIEXYZTRIPLE;

type
 {Bitmap Types}
 PBitmap = ^TBitmap;
 tagBITMAP = record
   bmType: LONG;
   bmWidth: LONG;
   bmHeight: LONG;
   bmWidthBytes: LONG;
   bmPlanes: WORD;
   bmBitsPixel: WORD;
   bmBits: LPVOID;
 end;
 BITMAP = tagBITMAP;
 LPBITMAP = ^BITMAP;
 NPBITMAP = ^BITMAP;
 TBitmap = BITMAP;

 PRgbTriple = ^TRgbTriple;
 tagRGBTRIPLE = packed record
   rgbtBlue: BYTE;
   rgbtGreen: BYTE;
   rgbtRed: BYTE;
 end;
 RGBTRIPLE = tagRGBTRIPLE;
 TRgbTriple = RGBTRIPLE;

 PRgbQuad = ^TRgbQuad;
 tagRGBQUAD = record
   rgbBlue: BYTE;
   rgbGreen: BYTE;
   rgbRed: BYTE;
   rgbReserved: BYTE;
 end;
 RGBQUAD = tagRGBQUAD;
 LPRGBQUAD = ^RGBQUAD;
 TRgbQuad = RGBQUAD;

type
 {Bitmap Header Types}
 PBitmapCoreHeader = ^TBitmapCoreHeader;
 tagBITMAPCOREHEADER = record
   bcSize: DWORD;
   bcWidth: WORD;
   bcHeight: WORD;
   bcPlanes: WORD;
   bcBitCount: WORD;
 end;
 BITMAPCOREHEADER = tagBITMAPCOREHEADER;
 LPBITMAPCOREHEADER = ^BITMAPCOREHEADER;
 TBitmapCoreHeader = BITMAPCOREHEADER;

 PBitmapInfoHeader = ^TBitmapInfoHeader;
 tagBITMAPINFOHEADER = record
   biSize: DWORD;
   biWidth: LONG;
   biHeight: LONG;
   biPlanes: WORD;
   biBitCount: WORD;
   biCompression: DWORD;
   biSizeImage: DWORD;
   biXPelsPerMeter: LONG;
   biYPelsPerMeter: LONG;
   biClrUsed: DWORD;
   biClrImportant: DWORD;
 end;
 BITMAPINFOHEADER = tagBITMAPINFOHEADER;
 LPBITMAPINFOHEADER = ^BITMAPINFOHEADER;
 TBitmapInfoHeader = BITMAPINFOHEADER;

 PBitmapV4Header = ^TBitmapV4Header;
 BITMAPV4HEADER = record
   bV4Size: DWORD;
   bV4Width: LONG;
   bV4Height: LONG;
   bV4Planes: WORD;
   bV4BitCount: WORD;
   bV4V4Compression: DWORD;
   bV4SizeImage: DWORD;
   bV4XPelsPerMeter: LONG;
   bV4YPelsPerMeter: LONG;
   bV4ClrUsed: DWORD;
   bV4ClrImportant: DWORD;
   bV4RedMask: DWORD;
   bV4GreenMask: DWORD;
   bV4BlueMask: DWORD;
   bV4AlphaMask: DWORD;
   bV4CSType: DWORD;
   bV4Endpoints: CIEXYZTRIPLE;
   bV4GammaRed: DWORD;
   bV4GammaGreen: DWORD;
   bV4GammaBlue: DWORD;
 end;
 LPBITMAPV4HEADER = ^BITMAPV4HEADER;
 TBitmapV4Header = BITMAPV4HEADER;

 PBitmapV5Header = ^TBitmapV5Header;
 BITMAPV5HEADER = record
   bV5Size: DWORD;
   bV5Width: LONG;
   bV5Height: LONG;
   bV5Planes: WORD;
   bV5BitCount: WORD;
   bV5Compression: DWORD;
   bV5SizeImage: DWORD;
   bV5XPelsPerMeter: LONG;
   bV5YPelsPerMeter: LONG;
   bV5ClrUsed: DWORD;
   bV5ClrImportant: DWORD;
   bV5RedMask: DWORD;
   bV5GreenMask: DWORD;
   bV5BlueMask: DWORD;
   bV5AlphaMask: DWORD;
   bV5CSType: DWORD;
   bV5Endpoints: CIEXYZTRIPLE;
   bV5GammaRed: DWORD;
   bV5GammaGreen: DWORD;
   bV5GammaBlue: DWORD;
   bV5Intent: DWORD;
   bV5ProfileData: DWORD;
   bV5ProfileSize: DWORD;
   bV5Reserved: DWORD;
 end;
 LPBITMAPV5HEADER = ^BITMAPV5HEADER;
 TBitmapV5Header = BITMAPV5HEADER;

type
 {Bitmap Info Types}
 PBitmapInfo = ^TBitmapInfo;
 tagBITMAPINFO = record
   bmiHeader: BITMAPINFOHEADER;
   bmiColors: array [0..0] of RGBQUAD;
 end;
 BITMAPINFO = tagBITMAPINFO;
 LPBITMAPINFO = ^BITMAPINFO;
 TBitmapInfo = BITMAPINFO;

 PBitmapCoreInfo = ^TBitmapCoreInfo;
 tagBITMAPCOREINFO = record
   bmciHeader: BITMAPCOREHEADER;
   bmciColors: array [0..0] of RGBTRIPLE;
 end;
 BITMAPCOREINFO = tagBITMAPCOREINFO;
 LPBITMAPCOREINFO = ^BITMAPCOREINFO;
 TBitmapCoreInfo = BITMAPCOREINFO;

 PBitmapFileHeader = ^TBitmapFileHeader;
 tagBITMAPFILEHEADER = packed record
   bfType: WORD;
   bfSize: DWORD;
   bfReserved1: WORD;
   bfReserved2: WORD;
   bfOffBits: DWORD;
 end;
 BITMAPFILEHEADER = tagBITMAPFILEHEADER;
 LPBITMAPFILEHEADER = ^BITMAPFILEHEADER;
 TBitmapFileHeader = BITMAPFILEHEADER;

type
 {File Information Types}
 PBY_HANDLE_FILE_INFORMATION = ^BY_HANDLE_FILE_INFORMATION;
 _BY_HANDLE_FILE_INFORMATION = record
   dwFileAttributes: DWORD;
   ftCreationTime: FILETIME;
   ftLastAccessTime: FILETIME;
   ftLastWriteTime: FILETIME;
   dwVolumeSerialNumber: DWORD;
   nFileSizeHigh: DWORD;
   nFileSizeLow: DWORD;
   nNumberOfLinks: DWORD;
   nFileIndexHigh: DWORD;
   nFileIndexLow: DWORD;
 end;
 BY_HANDLE_FILE_INFORMATION = _BY_HANDLE_FILE_INFORMATION;
 LPBY_HANDLE_FILE_INFORMATION = ^BY_HANDLE_FILE_INFORMATION;
 TByHandleFileInformation = BY_HANDLE_FILE_INFORMATION;
 PByHandleFileInformation = PBY_HANDLE_FILE_INFORMATION;

{==============================================================================}
{Compatibility variables}
var
 CONDITION_VARIABLE_INIT:CONDITION_VARIABLE = (Ptr:nil);

{==============================================================================}
{Ultibo constants}
const
 TIME_NULL_TIME:TFileTime = (dwLowDateTime:$00000000;dwHighDateTime:$00000000);

const
 {Drive constants}
 DEFAULT_DRIVE = 0; {0 is Current Drive}
 INVALID_DRIVE = 0; {Where Current is not appropriate}
 MIN_DRIVE = 1;     {1 = A:, 2 = B:, 3 = C:, 26 = Z: etc}
 MAX_DRIVE = 26;
 NON_DRIVE = 27;    {27 for Non Drive (UNC) Path}

 DRIVE_NAMES:array[DEFAULT_DRIVE..NON_DRIVE] of String = (
  '','A:\','B:\','C:\','D:\','E:\','F:\','G:\','H:\','I:\','J:\','K:\','L:\','M:\',
  'N:\','O:\','P:\','Q:\','R:\','S:\','T:\','U:\','V:\','W:\','X:\','Y:\','Z:\','\\');

 DRIVE_ROOTS:array[DEFAULT_DRIVE..NON_DRIVE] of String = (
  '','A:','B:','C:','D:','E:','F:','G:','H:','I:','J:','K:','L:','M:',
  'N:','O:','P:','Q:','R:','S:','T:','U:','V:','W:','X:','Y:','Z:','\\');

 DRIVE_MASKS:array[MIN_DRIVE..MAX_DRIVE] of LongWord = (
  $00000001,$00000002,$00000004,$00000008,
  $00000010,$00000020,$00000040,$00000080,
  $00000100,$00000200,$00000400,$00000800,
  $00001000,$00002000,$00004000,$00008000,
  $00010000,$00020000,$00040000,$00080000,
  $00100000,$00200000,$00400000,$00800000,
  $01000000,$02000000);

 DRIVE_A = 1;
 DRIVE_B = 2;
 DRIVE_C = 3;

const
 {Path and File constants}
 MAX_FAT_PATH = 64;
 MAX_FAT_FILE = 12;
 MAX_VFAT_PATH = 260;
 MAX_VFAT_FILE = 255;

 FAT_PATH_CHAR = '\';   {Path separator}
 FAT_NAME_CHAR = ':';   {Stream separator}
 FAT_FILE_CHAR = '.';   {Extension separator}
 FAT_DRIVE_CHAR = ':';  {Drive separator}

 UNIX_PATH_CHAR = '/';  {Path separator}
 UNIX_NAME_CHAR = ':';  {Stream separator}
 UNIX_FILE_CHAR = '.';  {Extension separator}

 {Invalid Filename Chars etc}
 INVALID_FILENAME_CHARS:set of Char = ['\','/','*','?','"','<','>','|',':'];
 SHORT_FILENAME_SUBST_CHARS:set of Char = ['+','=',',','[',']',';'];
 INVALID_FILENAME_STRING = '\ / : * ? " < > |';

 {Invalid Stream name Chars etc}
 INVALID_STREAM_NAME_CHARS:set of Char = ['\','/','*','?','<','>','|',':'];
 INVALID_STREAM_NAME_STRING = '\ / : * ? < > |';

 {Wildcard Filename Chars}
 MACRO_FILENAME_CHAR:Char = '%';
 WILDCARD_FILENAME_CHAR:Char = '*';
 INVALID_FILENAME_CHARS_EX:set of Char = ['\','/','?','"','<','>','|',':'];

{==============================================================================}
{Ultibo types}
type
 TUnixTime = time_t;

type
 TDriveType = (dtUNKNOWN,dtINVALID,dtFIXED,dtFLOPPY,dtREMOVABLE,dtNETWORK,dtSUBSTITUTED,dtRAMDRIVE,dtCDROM);

 TFileSysType = (fsUNKNOWN,fsINVALID,fsFAT12,fsFAT16,fsFAT32,fsEXFAT,fsHPFS,fsNTFS,fsNTFS5,fsNTFS51,fsCDFS,fsUDF,fsEXT2,fsEXT3,fsEXT4);

type
 {Drive types}
 TDriveData = record
  Drive:Byte;
  DriveType:TDriveType;
  FileSysType:TFileSysType;
  MaxFile:Integer;
  MaxPath:Integer;
  Attributes:LongWord;
  SystemName:String[32];
  VolumeName:String[32];
  VolumeSerial:LongWord;
 end;

type
 {Drive Functions (Compatibility)}
 TUltiboGetDriveTypeA = function(const ARootPath:String):LongWord;
 TUltiboGetLogicalDrives = function:LongWord;
 TUltiboGetLogicalDriveStringsA = function:String;
 TUltiboDefineDosDeviceA = function(const ADeviceName,ATargetPath:String;AFlags:LongWord):Boolean;
 TUltiboQueryDosDeviceA = function(const ARootPath:String):String;
 TUltiboSetVolumeLabelA = function(const AVolume:String;const ALabel:String):Boolean;
 TUltiboGetVolumeInformationA = function(const ARootPath:String;var AVolumeName:String;var VolumeSerialNumber,AMaximumComponentLength,AFileSystemFlags:LongWord;var SystemName:String):Boolean;
 TUltiboGetDiskFreeSpaceA = function(const ARootPath:String;var ASectorsPerCluster,ABytesPerSector,ANumberOfFreeClusters,ATotalNumberOfClusters:LongWord):Boolean;
 TUltiboGetDiskFreeSpaceExA = function(const APathName:String;var AFreeBytesAvailableToCaller,ATotalNumberOfBytes,ATotalNumberOfFreeBytes:QWord):Boolean;

 {Drive Functions (Ultibo)}
 TUltiboGetDriveType = function(ADrive:Byte):TDriveType;
 TUltiboGetDriveData = function(ADrive:Byte):TDriveData;
 TUltiboGetDriveAttr = function(ADrive:Byte):LongWord;
 TUltiboGetDriveLabel = function(ADrive:Byte):String;
 TUltiboSetDriveLabel = function(ADrive:Byte;const ALabel:String):Boolean;
 TUltiboGetDriveSerial = function(ADrive:Byte):LongWord;
 TUltiboSetDriveSerial = function(ADrive:Byte;ASerial:LongWord):Boolean;
 TUltiboIsDriveValid = function(ADrive:Byte):Boolean;
 TUltiboGetValidDrives = function:LongWord;
 TUltiboGetValidDriveNames = function:String;
 TUltiboGetDriveFreeSpace = function(ADrive:Byte):LongWord;
 TUltiboGetDriveFreeSpaceEx = function(ADrive:Byte):Int64;
 TUltiboGetDriveTotalSpace = function(ADrive:Byte):LongWord;
 TUltiboGetDriveTotalSpaceEx = function(ADrive:Byte):Int64;

 TUltiboGetDriveInformation = function(const APath:String;var AClusterSize:LongWord;var ATotalClusterCount,AFreeClusterCount:Int64):Boolean;

 TUltiboGetCurrentDrive = function:Byte;
 TUltiboSetCurrentDrive = function(const ADrive:String):Boolean;

 {File Functions (Compatibility)}
 TUltiboAreFileApisANSI = function:Boolean;
 TUltiboSetFileApisToOEM = function:Boolean;
 TUltiboSetFileApisToANSI = function:Boolean;
 TUltiboCreateFileA = function(const AFileName:AnsiString;AAccessMode,AShareMode,ACreateFlags,AFileAttributes:LongWord):THandle;
 TUltiboCloseFile = function(AHandle:THandle):Boolean;
 TUltiboSetFileAttributesA = function(const AFileName:String;AFileAttributes:LongWord):Boolean;
 TUltiboGetFileAttributesA = function(const AFileName:String):LongWord;
 TUltiboDeleteFileA = function(const AFileName:String):Boolean;
 TUltiboMoveFileA = function(const AExistingName,ANewName:String):Boolean;
 TUltiboFindFirstFileA = function(const AFileName:String;var AFindData:TWin32FindData):THandle;
 TUltiboFindNextFileA = function(AHandle:THandle;var AFindData:TWin32FindData):Boolean;
 TUltiboFindCloseFile = function(AHandle:THandle):Boolean;
 TUltiboGetFileSize = function(AHandle:THandle;var AFileSizeHigh:LongWord):LongWord;
 TUltiboGetFileSizeEx = function(AHandle:THandle):Int64;
 TUltiboGetFileTime = function(AHandle:THandle;ACreateTime,AAccessTime,AWriteTime:PFileTime):Boolean;
 TUltiboSetFileTime = function(AHandle:THandle;ACreateTime,AAccessTime,AWriteTime:PFileTime):Boolean;
 TUltiboReadFile = function(AHandle:THandle;var ABuffer;ABytesToRead:LongWord;var ABytesRead:LongWord):Boolean;
 TUltiboWriteFile = function(AHandle:THandle;const ABuffer;ABytesToWrite:LongWord;var ABytesWritten:LongWord):Boolean;
 TUltiboSetEndOfFile = function(AHandle:THandle):Boolean;
 TUltiboSetFilePointer = function(AHandle:THandle;ADistanceToMove:LongInt;var ADistanceToMoveHigh:LongInt;AMoveMethod:LongWord):LongWord;
 TUltiboSetFilePointerEx = function(AHandle:THandle;const ADistanceToMove:Int64;var ANewFilePointer:Int64;AMoveMethod:LongWord):Boolean;
 TUltiboFlushFileBuffers = function(AHandle:THandle):Boolean;
 TUltiboCopyFileA = function(const AExistingName,ANewName:String;AFailIfExists:Boolean):Boolean;
 TUltiboSetFileShortNameA = function(AHandle:THandle;const AShortName:String):Boolean;
 TUltiboCreateHardLinkA = function(const ALinkName,AFileName:String):Boolean;
 TUltiboCreateSymbolicLinkA = function(const ALinkName,ATargetName:String;ADirectory:Boolean):Boolean;
 TUltiboGetFileInformationByHandle = function(AHandle:THandle;var AFileInformation:TByHandleFileInformation):Boolean;
 TUltiboGetFinalPathNameByHandleA = function(AHandle:THandle;AFlags:LongWord):String;

 {Handle Functions (Compatibility)}
 TUltiboDuplicateHandle = function(AHandle:THandle):THandle;

 {Directory Functions (Compatibility)}
 TUltiboCreateDirectoryA = function(const APathName:String):Boolean;
 TUltiboRemoveDirectoryA = function(const APathName:String):Boolean;
 TUltiboSetCurrentDirectoryA = function(const APathName:String):Boolean;
 TUltiboGetCurrentDirectoryA = function:String;
 TUltiboGetLongPathNameA = function(const AShortPath:String):String;
 TUltiboGetShortPathNameA = function(const ALongPath:String):String;
 TUltiboGetFullPathNameA = function(const AFileName:String):String;

{==============================================================================}
{Ultibo variables}
var
 {Drive Functions (Compatibility)}
 UltiboGetDriveTypeAHandler:TUltiboGetDriveTypeA;
 UltiboGetLogicalDrivesHandler:TUltiboGetLogicalDrives;
 UltiboGetLogicalDriveStringsAHandler:TUltiboGetLogicalDriveStringsA;
 UltiboDefineDosDeviceAHandler:TUltiboDefineDosDeviceA;
 UltiboQueryDosDeviceAHandler:TUltiboQueryDosDeviceA;
 UltiboSetVolumeLabelAHandler:TUltiboSetVolumeLabelA;
 UltiboGetVolumeInformationAHandler:TUltiboGetVolumeInformationA;
 UltiboGetDiskFreeSpaceAHandler:TUltiboGetDiskFreeSpaceA;
 UltiboGetDiskFreeSpaceExAHandler:TUltiboGetDiskFreeSpaceExA;

 {Drive Functions (Ultibo)}
 UltiboGetDriveTypeHandler:TUltiboGetDriveType;
 UltiboGetDriveDataHandler:TUltiboGetDriveData;
 UltiboGetDriveAttrHandler:TUltiboGetDriveAttr;
 UltiboGetDriveLabelHandler:TUltiboGetDriveLabel;
 UltiboSetDriveLabelHandler:TUltiboSetDriveLabel;
 UltiboGetDriveSerialHandler:TUltiboGetDriveSerial;
 UltiboSetDriveSerialHandler:TUltiboSetDriveSerial;
 UltiboIsDriveValidHandler:TUltiboIsDriveValid;
 UltiboGetValidDrivesHandler:TUltiboGetValidDrives;
 UltiboGetValidDriveNamesHandler:TUltiboGetValidDriveNames;
 UltiboGetDriveFreeSpaceHandler:TUltiboGetDriveFreeSpace;
 UltiboGetDriveFreeSpaceExHandler:TUltiboGetDriveFreeSpaceEx;
 UltiboGetDriveTotalSpaceHandler:TUltiboGetDriveTotalSpace;
 UltiboGetDriveTotalSpaceExHandler:TUltiboGetDriveTotalSpaceEx;

 UltiboGetDriveInformationHandler:TUltiboGetDriveInformation;

 UltiboGetCurrentDriveHandler:TUltiboGetCurrentDrive;
 UltiboSetCurrentDriveHandler:TUltiboSetCurrentDrive;

 {File Functions (Compatibility)}
 UltiboAreFileApisANSIHandler:TUltiboAreFileApisANSI;
 UltiboSetFileApisToOEMHandler:TUltiboSetFileApisToOEM;
 UltiboSetFileApisToANSIHandler:TUltiboSetFileApisToANSI;
 UltiboCreateFileAHandler:TUltiboCreateFileA;
 UltiboCloseFileHandler:TUltiboCloseFile;
 UltiboSetFileAttributesAHandler:TUltiboSetFileAttributesA;
 UltiboGetFileAttributesAHandler:TUltiboGetFileAttributesA;
 UltiboDeleteFileAHandler:TUltiboDeleteFileA;
 UltiboMoveFileAHandler:TUltiboMoveFileA;
 UltiboFindFirstFileAHandler:TUltiboFindFirstFileA;
 UltiboFindNextFileAHandler:TUltiboFindNextFileA;
 UltiboFindCloseFileHandler:TUltiboFindCloseFile;
 UltiboGetFileSizeHandler:TUltiboGetFileSize;
 UltiboGetFileSizeExHandler:TUltiboGetFileSizeEx;
 UltiboGetFileTimeHandler:TUltiboGetFileTime;
 UltiboSetFileTimeHandler:TUltiboSetFileTime;
 UltiboReadFileHandler:TUltiboReadFile;
 UltiboWriteFileHandler:TUltiboWriteFile;
 UltiboSetEndOfFileHandler:TUltiboSetEndOfFile;
 UltiboSetFilePointerHandler:TUltiboSetFilePointer;
 UltiboSetFilePointerExHandler:TUltiboSetFilePointerEx;
 UltiboFlushFileBuffersHandler:TUltiboFlushFileBuffers;
 UltiboCopyFileAHandler:TUltiboCopyFileA;
 UltiboSetFileShortNameAHandler:TUltiboSetFileShortNameA;
 UltiboCreateHardLinkAHandler:TUltiboCreateHardLinkA;
 UltiboCreateSymbolicLinkAHandler:TUltiboCreateSymbolicLinkA;
 UltiboGetFileInformationByHandleHandler:TUltiboGetFileInformationByHandle;
 UltiboGetFinalPathNameByHandleAHandler:TUltiboGetFinalPathNameByHandleA;

 {Handle Functions (Compatibility)}
 UltiboDuplicateHandleHandler:TUltiboDuplicateHandle;

 {Directory Functions (Compatibility)}
 UltiboCreateDirectoryAHandler:TUltiboCreateDirectoryA;
 UltiboRemoveDirectoryAHandler:TUltiboRemoveDirectoryA;
 UltiboSetCurrentDirectoryAHandler:TUltiboSetCurrentDirectoryA;
 UltiboGetCurrentDirectoryAHandler:TUltiboGetCurrentDirectoryA;
 UltiboGetLongPathNameAHandler:TUltiboGetLongPathNameA;
 UltiboGetShortPathNameAHandler:TUltiboGetShortPathNameA;
 UltiboGetFullPathNameAHandler:TUltiboGetFullPathNameA;

{==============================================================================}
{Initialization Functions}
procedure UltiboInit;

{==============================================================================}
{General Functions (Compatibility)}
function GetVersion:DWORD;

function GetVersionEx(lpVersionInformation:LPOSVERSIONINFOA):BOOL; inline;
function GetVersionExA(lpVersionInformation:LPOSVERSIONINFOA):BOOL;
function GetVersionExW(lpVersionInformation:LPOSVERSIONINFOW):BOOL;

procedure GetSystemInfo(var lpSystemInfo:SYSTEM_INFO);
procedure GetNativeSystemInfo(lpSystemInfo:LPSYSTEM_INFO);
function GetLargePageMinimum:SIZE_T;

function GetComputerName(lpBuffer:LPSTR;var nSize:DWORD):BOOL; inline;
function GetComputerNameA(lpBuffer:LPSTR;var nSize:DWORD):BOOL;
function GetComputerNameW(lpBuffer:LPWSTR;var nSize:DWORD):BOOL;

function SetComputerName(const lpComputerName:LPCSTR):BOOL; inline;
function SetComputerNameA(const lpComputerName:LPCSTR):BOOL;
function SetComputerNameW(const lpComputerName:LPCWSTR):BOOL;

function GetComputerNameEx(NameType:COMPUTER_NAME_FORMAT;lpBuffer:LPSTR;var nSize:DWORD):BOOL; inline;
function GetComputerNameExA(NameType:COMPUTER_NAME_FORMAT;lpBuffer:LPSTR;var nSize:DWORD):BOOL;
function GetComputerNameExW(NameType:COMPUTER_NAME_FORMAT;lpBuffer:LPWSTR;var nSize:DWORD):BOOL;

function SetComputerNameEx(NameType:COMPUTER_NAME_FORMAT;const lpBuffer:LPCSTR):BOOL; inline;
function SetComputerNameExA(NameType:COMPUTER_NAME_FORMAT;const lpBuffer:LPCSTR):BOOL;
function SetComputerNameExW(NameType:COMPUTER_NAME_FORMAT;const lpBuffer:LPCWSTR):BOOL;

function ExitUltibo(dwReserved:DWORD;uReserved:UINT):BOOL;
function ExitUltiboEx(uFlags:UINT;dwReserved:DWORD):BOOL;

{==============================================================================}
{General Functions (Ultibo)}
function RestartComputer(Delay:LongWord):Boolean;
function ShutdownComputer(Delay:LongWord):Boolean;

function Uptime:TFileTime;

{==============================================================================}
{Time Functions (Compatibility)}
function GetTickCount:DWORD;
function GetTickCount64:ULONGLONG;

procedure GetSystemTime(var lpSystemTime:SYSTEMTIME);
procedure GetSystemTimeAsFileTime(var lpSystemTimeAsFileTime:FILETIME);
function SetSystemTime(var lpSystemTime:SYSTEMTIME):BOOL;

procedure GetLocalTime(var lpSystemTime:SYSTEMTIME);
function SetLocalTime(var lpSystemTime:SYSTEMTIME):BOOL;

function SystemTimeToTzSpecificLocalTime(lpTimeZoneInformation:LPTIME_ZONE_INFORMATION;var lpUniversalTime,lpLocalTime:SYSTEMTIME):BOOL;
function TzSpecificLocalTimeToSystemTime(lpTimeZoneInformation:LPTIME_ZONE_INFORMATION;const lpLocalTime:SYSTEMTIME;var lpUniversalTime:SYSTEMTIME):BOOL;

function GetTimeZoneInformation(var lpTimeZoneInformation:TIME_ZONE_INFORMATION):DWORD;
function SetTimeZoneInformation(const lpTimeZoneInformation:TIME_ZONE_INFORMATION):BOOL;

function SystemTimeToFileTime(const lpSystemTime:SYSTEMTIME;var lpFileTime:FILETIME):BOOL;
function FileTimeToSystemTime(const lpFileTime:FILETIME;var lpSystemTime:SYSTEMTIME):BOOL;

function FileTimeToLocalFileTime(const lpFileTime:FILETIME;var lpLocalFileTime:FILETIME):BOOL;
function LocalFileTimeToFileTime(const lpLocalFileTime:FILETIME;var lpFileTime:FILETIME):BOOL;

function CompareFileTime(const lpFileTime1,lpFileTime2:FILETIME):LONG;

function FileTimeToDosDateTime(const lpFileTime:FILETIME;var lpFatDate,lpFatTime:WORD):BOOL;
function DosDateTimeToFileTime(wFatDate,wFatTime:WORD;var lpFileTime:FILETIME):BOOL;

function QueryPerformanceCounter(var lpPerformanceCount: LARGE_INTEGER): BOOL;
function QueryPerformanceFrequency(var lpFrequency: LARGE_INTEGER): BOOL;

{==============================================================================}
{Time Functions (Ultibo)}
function GetCurrentTime:TFileTime;
procedure SetCurrentTime(const ATime:TFileTime);

function GetTimeAdjust:LongInt;
procedure SetTimeAdjust(AAdjust:LongInt);

function GetCurrentTimezone:String;
function SetCurrentTimezone(const AName:String):Boolean;

function GetTimezoneActiveOffset:LongInt;
function GetTimezoneStandardOffset:LongInt;
procedure SetTimezoneStandardOffset(AOffset:LongInt);
function GetTimezoneDaylightOffset:LongInt;
procedure SetTimezoneDaylightOffset(AOffset:LongInt);

function GetTimezoneStandardStart:String;
function GetTimezoneDaylightStart:String;

function GetTimezoneStandardDate:TDateTime;
function GetTimezoneDaylightDate:TDateTime;

function FileTimeToDateTime(const AFileTime:TFileTime):TDateTime;
function DateTimeToFileTime(ADateTime:TDateTime):TFileTime;

function LocalFileTimeToDateTime(const AFileTime:TFileTime):TDateTime;
function DateTimeToLocalFileTime(ADateTime:TDateTime):TFileTime;

function SystemFileTimeToDateTime(const AFileTime:TFileTime):TDateTime;
function DateTimeToSystemFileTime(ADateTime:TDateTime):TFileTime;

function FileTimeToUnixTime(const AFileTime:TFileTime):TUnixTime;
function UnixTimeToFileTime(AUnixTime:TUnixTime):TFileTime;

function UnixTimeToDateTime(AUnixTime:TUnixTime):TDateTime;
function DateTimeToUnixTime(ADateTime:TDateTime):TUnixTime;

function FileTimeToFileDate(const AFileTime:TFileTime):Integer;
function FileDateToFileTime(AFileDate:Integer):TFileTime;

function FileTimeToAdjustedTime(const AFileTime:TFileTime):TFileTime;
function AdjustedTimeToFileTime(const AFileTime:TFileTime):TFileTime;

function RoundFileTime(const AFileTime:TFileTime):TFileTime;

function ConvertFileTime(const AFileTime:TFileTime;AOffset:Integer;ALocal:Boolean):TFileTime;
function ConvertDateTime(ADateTime:TDateTime;AOffset:Integer;ALocal:Boolean):TDateTime;

{==============================================================================}
{Drive Functions (Compatibility)}
function GetDiskType(const lpRootPathName:LPCSTR):UINT; inline; {GetDriveType - Already defined below}
function GetDriveTypeA(const lpRootPathName:LPCSTR):UINT;
function GetDriveTypeW(const lpRootPathName:LPCWSTR):UINT;

function GetLogicalDrives:DWORD;

function GetLogicalDriveStrings(nBufferLength:DWORD;lpBuffer:LPSTR):DWORD; inline;
function GetLogicalDriveStringsA(nBufferLength:DWORD;lpBuffer:LPSTR):DWORD;
function GetLogicalDriveStringsW(nBufferLength:DWORD;lpBuffer:LPWSTR):DWORD;

function DefineDosDevice(dwFlags:DWORD;const lpDeviceName,lpTargetPath:LPCSTR):BOOL;  inline;
function DefineDosDeviceA(dwFlags:DWORD;const lpDeviceName,lpTargetPath:LPCSTR):BOOL;
function DefineDosDeviceW(dwFlags:DWORD;const lpDeviceName,lpTargetPath:LPCWSTR):BOOL;

function QueryDosDevice(const lpDeviceName:LPCSTR;lpTargetPath:LPSTR;ucchMax:DWORD):DWORD; inline;
function QueryDosDeviceA(const lpDeviceName:LPCSTR;lpTargetPath:LPSTR;ucchMax:DWORD):DWORD;
function QueryDosDeviceW(const lpDeviceName:LPCWSTR;lpTargetPath:LPWSTR;ucchMax:DWORD):DWORD;

function SetVolumeLabel(const lpRootPathName,lpVolumeName:LPCSTR):BOOL;  inline;
function SetVolumeLabelA(const lpRootPathName,lpVolumeName:LPCSTR):BOOL;
function SetVolumeLabelW(const lpRootPathName,lpVolumeName:LPCWSTR):BOOL;

function GetVolumeInformation(const lpRootPathName:LPCSTR;lpVolumeNameBuffer:LPSTR;nVolumeNameSize:DWORD;lpVolumeSerialNumber:LPDWORD;var lpMaximumComponentLength,lpFileSystemFlags:DWORD;lpFileSystemNameBuffer:LPSTR;nFileSystemNameSize:DWORD):BOOL; inline;
function GetVolumeInformationA(const lpRootPathName:LPCSTR;lpVolumeNameBuffer:LPSTR;nVolumeNameSize:DWORD;lpVolumeSerialNumber:LPDWORD;var lpMaximumComponentLength,lpFileSystemFlags:DWORD;lpFileSystemNameBuffer:LPSTR;nFileSystemNameSize:DWORD):BOOL;
function GetVolumeInformationW(const lpRootPathName:LPCWSTR;lpVolumeNameBuffer:LPWSTR;nVolumeNameSize:DWORD;lpVolumeSerialNumber:LPDWORD;var lpMaximumComponentLength,lpFileSystemFlags:DWORD;lpFileSystemNameBuffer:LPWSTR;nFileSystemNameSize:DWORD):BOOL;

function GetDiskFreeSpace(const lpRootPathName:LPCSTR;var lpSectorsPerCluster,lpBytesPerSector,lpNumberOfFreeClusters,lpTotalNumberOfClusters:DWORD):BOOL; inline;
function GetDiskFreeSpaceA(const lpRootPathName:LPCSTR;var lpSectorsPerCluster,lpBytesPerSector,lpNumberOfFreeClusters,lpTotalNumberOfClusters:DWORD):BOOL;
function GetDiskFreeSpaceW(const lpRootPathName:LPCWSTR;var lpSectorsPerCluster,lpBytesPerSector,lpNumberOfFreeClusters,lpTotalNumberOfClusters:DWORD):BOOL;

function GetDiskFreeSpaceEx(const lpDirectoryName:LPCSTR;var lpFreeBytesAvailableToCaller,lpTotalNumberOfBytes:ULARGE_INTEGER;lpTotalNumberOfFreeBytes:PULARGE_INTEGER):BOOL; inline;
function GetDiskFreeSpaceExA(const lpDirectoryName:LPCSTR;var lpFreeBytesAvailableToCaller,lpTotalNumberOfBytes:ULARGE_INTEGER;lpTotalNumberOfFreeBytes:PULARGE_INTEGER):BOOL;
function GetDiskFreeSpaceExW(const lpDirectoryName:LPCWSTR;var lpFreeBytesAvailableToCaller,lpTotalNumberOfBytes:ULARGE_INTEGER;lpTotalNumberOfFreeBytes:PULARGE_INTEGER):BOOL;

{==============================================================================}
{Drive Functions (Ultibo)}
function GetPathDrive(const APath:String):Byte;
function GetDriveType(ADrive:Byte):TDriveType;
function GetDriveData(ADrive:Byte):TDriveData;
function GetDriveAttr(ADrive:Byte):LongWord;
function GetDriveLabel(ADrive:Byte):String;
function SetDriveLabel(ADrive:Byte;const ALabel:String):Boolean;
function GetDriveSerial(ADrive:Byte):LongWord;
function SetDriveSerial(ADrive:Byte;ASerial:LongWord):Boolean;
function IsDriveValid(ADrive:Byte):Boolean;
function GetValidDrives:LongWord;
function GetValidDriveNames:String;
function GetDriveFreeSpace(ADrive:Byte):LongWord;
function GetDriveFreeSpaceEx(ADrive:Byte):Int64;
function GetDriveTotalSpace(ADrive:Byte):LongWord;
function GetDriveTotalSpaceEx(ADrive:Byte):Int64;

function GetDriveInformation(const APath:String;var AClusterSize:LongWord;var ATotalClusterCount,AFreeClusterCount:Int64):Boolean;

function GetCurrentDrive:Byte;
function SetCurrentDrive(const ADrive:String):Boolean;

{==============================================================================}
{File Functions (Compatibility)}
function AreFileApisANSI:BOOL;
procedure SetFileApisToOEM;
procedure SetFileApisToANSI;

function CreateFile(const lpFileName:LPCSTR;dwDesiredAccess,dwShareMode:DWORD;lpSecurityAttributes:LPSECURITY_ATTRIBUTES;dwCreationDisposition:DWORD;dwFlagsAndAttributes:DWORD;hTemplateFile:HANDLE):HANDLE; inline;
function CreateFileA(const lpFileName:LPCSTR;dwDesiredAccess,dwShareMode:DWORD;lpSecurityAttributes:LPSECURITY_ATTRIBUTES;dwCreationDisposition:DWORD;dwFlagsAndAttributes:DWORD;hTemplateFile:HANDLE):HANDLE;
function CreateFileW(const lpFileName:LPCWSTR;dwDesiredAccess,dwShareMode:DWORD;lpSecurityAttributes:LPSECURITY_ATTRIBUTES;dwCreationDisposition:DWORD;dwFlagsAndAttributes:DWORD;hTemplateFile:HANDLE):HANDLE;

function SetFileAttributes(const lpFileName:LPCSTR;dwFileAttributes:DWORD):BOOL; inline;
function SetFileAttributesA(const lpFileName:LPCSTR;dwFileAttributes:DWORD):BOOL;
function SetFileAttributesW(const lpFileName:LPCWSTR;dwFileAttributes:DWORD):BOOL;

function GetFileAttributes(const lpFileName:LPCSTR):DWORD; inline;
function GetFileAttributesA(const lpFileName:LPCSTR):DWORD;
function GetFileAttributesW(const lpFileName:LPCWSTR):DWORD;

function DeleteFile(const lpFileName:LPCSTR):BOOL; inline;
function DeleteFileA(const lpFileName:LPCSTR):BOOL;
function DeleteFileW(const lpFileName:LPCWSTR):BOOL;

function MoveFile(const lpExistingFileName,lpNewFileName:LPCSTR):BOOL; inline;
function MoveFileA(const lpExistingFileName,lpNewFileName:LPCSTR):BOOL;
function MoveFileW(const lpExistingFileName,lpNewFileName:LPCWSTR):BOOL;

function FindFirstFile(const lpFileName:LPCSTR;var lpFindFileData:WIN32_FIND_DATAA):HANDLE; inline;
function FindFirstFileA(const lpFileName:LPCSTR;var lpFindFileData:WIN32_FIND_DATAA):HANDLE;
function FindFirstFileW(const lpFileName:LPCWSTR;var lpFindFileData:WIN32_FIND_DATAW):HANDLE;

function FindNextFile(hFindFile:HANDLE;var lpFindFileData:WIN32_FIND_DATAA):BOOL; inline;
function FindNextFileA(hFindFile:HANDLE;var lpFindFileData:WIN32_FIND_DATAA):BOOL;
function FindNextFileW(hFindFile:HANDLE;var lpFindFileData:WIN32_FIND_DATAW):BOOL;

function FindCloseFile(hFindFile:HANDLE):BOOL;

function GetFileSize(hFile:HANDLE;lpFileSizeHigh:LPDWORD):DWORD;
function GetFileSizeEx(hFile:HANDLE;var lpFileSize:LARGE_INTEGER):BOOL;

function GetFileTime(hFile:HANDLE;lpCreationTime,lpLastAccessTime,lpLastWriteTime:PFILETIME):BOOL;
function SetFileTime(hFile:HANDLE;lpCreationTime,lpLastAccessTime,lpLastWriteTime:PFILETIME):BOOL;

function ReadFile(hFile:HANDLE;lpBuffer:LPVOID;nNumberOfBytesToRead:DWORD;lpNumberOfBytesRead:LPDWORD;lpOverlapped:LPOVERLAPPED):BOOL;
function WriteFile(hFile:HANDLE;lpBuffer:LPCVOID;nNumberOfBytesToWrite:DWORD;lpNumberOfBytesWritten:LPDWORD;lpOverlapped:LPOVERLAPPED):BOOL;

function SetEndOfFile(hFile:HANDLE):BOOL;

function SetFilePointer(hFile:HANDLE;lDistanceToMove:LONG;lpDistanceToMoveHigh:PLONG;dwMoveMethod:DWORD):DWORD;
function SetFilePointerEx(hFile:HANDLE;liDistanceToMove:LARGE_INTEGER;lpNewFilePointer:PLARGE_INTEGER;dwMoveMethod:DWORD):BOOL;

function FlushFileBuffers(hFile:HANDLE):BOOL;

function CopyFile(const lpExistingFileName,lpNewFileName:LPCSTR;bFailIfExists:BOOL):BOOL; inline;
function CopyFileA(const lpExistingFileName,lpNewFileName:LPCSTR;bFailIfExists:BOOL):BOOL;
function CopyFileW(const lpExistingFileName,lpNewFileName:LPCWSTR;bFailIfExists:BOOL):BOOL;

function SetFileShortName(hFile:HANDLE;const lpShortName:LPCSTR):BOOL; inline;
function SetFileShortNameA(hFile:HANDLE;const lpShortName:LPCSTR):BOOL;
function SetFileShortNameW(hFile:HANDLE;const lpShortName:LPCWSTR):BOOL;

function CreateHardLink(const lpFileName,lpExistingFileName:LPCSTR;lpSecurityAttributes:LPSECURITY_ATTRIBUTES):BOOL; inline;
function CreateHardLinkA(const lpFileName,lpExistingFileName:LPCSTR;lpSecurityAttributes:LPSECURITY_ATTRIBUTES):BOOL;
function CreateHardLinkW(const lpFileName,lpExistingFileName:LPCWSTR;lpSecurityAttributes:LPSECURITY_ATTRIBUTES):BOOL;

function CreateSymbolicLink(const lpSymlinkFileName,lpTargetFileName:LPCSTR;dwFlags:DWORD):BOOL; inline;
function CreateSymbolicLinkA(const lpSymlinkFileName,lpTargetFileName:LPCSTR;dwFlags:DWORD):BOOL;
function CreateSymbolicLinkW(const lpSymlinkFileName,lpTargetFileName:LPCWSTR;dwFlags:DWORD):BOOL;

function GetFileInformationByHandle(hFile:HANDLE;var lpFileInformation:BY_HANDLE_FILE_INFORMATION):BOOL;

function GetFinalPathNameByHandle(hFile:HANDLE;lpszFilePath:LPSTR;cchFilePath,dwFlags:DWORD):DWORD;
function GetFinalPathNameByHandleA(hFile:HANDLE;lpszFilePath:LPSTR;cchFilePath,dwFlags:DWORD):DWORD;
function GetFinalPathNameByHandleW(hFile:HANDLE;lpszFilePath:LPWSTR;cchFilePath,dwFlags:DWORD):DWORD;

{==============================================================================}
{File Functions (Ultibo)}

{==============================================================================}
{Directory Functions (Compatibility)}
function CreateDirectory(const lpPathName:LPCSTR;lpSecurityAttributes:LPSECURITY_ATTRIBUTES):BOOL; inline;
function CreateDirectoryA(const lpPathName:LPCSTR;lpSecurityAttributes:LPSECURITY_ATTRIBUTES):BOOL;
function CreateDirectoryW(const lpPathName:LPCWSTR;lpSecurityAttributes:LPSECURITY_ATTRIBUTES):BOOL;

function RemoveDirectory(const lpPathName:LPCSTR):BOOL; inline;
function RemoveDirectoryA(const lpPathName:LPCSTR):BOOL;
function RemoveDirectoryW(const lpPathName:LPCWSTR):BOOL;

function SetCurrentDirectory(const lpPathName:LPCSTR):BOOL; inline;
function SetCurrentDirectoryA(const lpPathName:LPCSTR):BOOL;
function SetCurrentDirectoryW(const lpPathName:LPCWSTR):BOOL;

function GetCurrentDirectory(nBufferLength:DWORD;lpBuffer:LPSTR):DWORD; inline;
function GetCurrentDirectoryA(nBufferLength:DWORD;lpBuffer:LPSTR):DWORD;
function GetCurrentDirectoryW(nBufferLength:DWORD;lpBuffer:LPWSTR):DWORD;

function GetLongPathName(const lpszShortPath:LPCSTR;lpszLongPath:LPSTR;cchBuffer:DWORD):DWORD; inline;
function GetLongPathNameA(const lpszShortPath:LPCSTR;lpszLongPath:LPSTR;cchBuffer:DWORD):DWORD;
function GetLongPathNameW(const lpszShortPath:LPCWSTR;lpszLongPath:LPWSTR;cchBuffer:DWORD):DWORD;

function GetShortPathName(const lpszLongPath:LPCSTR;lpszShortPath:LPSTR;cchBuffer:DWORD):DWORD;  inline;
function GetShortPathNameA(const lpszLongPath:LPCSTR;lpszShortPath:LPSTR;cchBuffer:DWORD):DWORD;
function GetShortPathNameW(const lpszLongPath:LPCWSTR;lpszShortPath:LPWSTR;cchBuffer:DWORD):DWORD;

function GetFullPathName(const lpFileName:LPCSTR;nBufferLength:DWORD;lpBuffer:LPSTR;var lpFilePart:LPSTR):DWORD; inline;
function GetFullPathNameA(const lpFileName:LPCSTR;nBufferLength:DWORD;lpBuffer:LPSTR;var lpFilePart:LPSTR):DWORD;
function GetFullPathNameW(const lpFileName:LPCWSTR;nBufferLength:DWORD;lpBuffer:LPWSTR;var lpFilePart:LPWSTR):DWORD;

{==============================================================================}
{Directory Functions (Ultibo)}

{==============================================================================}
{Command Line Functions (RTL)}
function SysParamCount:LongInt;
function SysParamStr(Index:LongInt):String;

{==============================================================================}
{Command Line Functions (Compatibility)}
function GetCommandLine:LPSTR; inline;
function GetCommandLineA:LPSTR;
function GetCommandLineW:LPWSTR;

{==============================================================================}
{Command Line Functions (Ultibo)}
function IsParamPresent(const AParam:String):Boolean;
function GetParamIndex(const AParam:String):Integer;
function GetParamValue(const AParam:String):String;

{==============================================================================}
{Environment Functions (Compatibility)}
function GetEnvironmentStrings:LPSTR; inline;
function GetEnvironmentStringsA:LPSTR;
function GetEnvironmentStringsW:LPWSTR;

function FreeEnvironmentStrings(pstr:LPSTR):BOOL; inline;
function FreeEnvironmentStringsA(pstr:LPSTR):BOOL;
function FreeEnvironmentStringsW(pstr:LPWSTR):BOOL;

function GetEnvironmentVariable(const lpName:LPCSTR;lpBuffer:LPSTR;nSize:DWORD):DWORD; inline;
function GetEnvironmentVariableA(const lpName:LPCSTR;lpBuffer:LPSTR;nSize:DWORD):DWORD;
function GetEnvironmentVariableW(const lpName:LPCWSTR;lpBuffer:LPWSTR;nSize:DWORD):DWORD;

function SetEnvironmentVariable(const lpName,lpValue:LPCSTR):BOOL; inline;
function SetEnvironmentVariableA(const lpName,lpValue:LPCSTR):BOOL;
function SetEnvironmentVariableW(const lpName,lpValue:LPCWSTR):BOOL;

function ExpandEnvironmentStrings(const lpSrc:LPCSTR;lpDst:LPSTR;nSize:DWORD):DWORD; inline;
function ExpandEnvironmentStringsA(const lpSrc:LPCSTR;lpDst:LPSTR;nSize:DWORD):DWORD;
function ExpandEnvironmentStringsW(const lpSrc:LPCWSTR;lpDst:LPWSTR;nSize:DWORD):DWORD;

{==============================================================================}
{Error Functions (Compatibility)}
function GetLastError:DWORD; inline;
procedure SetLastError(dwErrCode:DWORD); inline;

{==============================================================================}
{String Functions (Ultibo)}

{==============================================================================}
{GUID Functions (Ultibo)}
function CreateGUID:TGUID;
function GUIDToString(const Value:TGUID;Braces:Boolean = False):String;
function StringToGUID(const Value:String):TGUID;
function NullGUID(const GUID:TGUID):Boolean;
function CompareGUID(const GUID1,GUID2:TGUID):Boolean;

{==============================================================================}
{SID Functions (Ultibo)}
function SIDToString(ASID:PSID):String;
function StringToSID(const Value:String):PSID;

{==============================================================================}
{Date Functions (Ultibo)}

{==============================================================================}
{Numeric Functions (Ultibo)}
function Min(A,B:Integer):Integer; inline; //To Do //The FPC Math unit has Min/Max etc with overloads for 32/64 etc //Check, confirm and remove
function Max(A,B:Integer):Integer; inline; //To Do //The FPC Math unit has Min/Max etc with overloads for 32/64 etc //Check, confirm and remove

function MinEx(A,B:LongWord):LongWord; inline; //To Do //The FPC Math unit has Min/Max etc with overloads for 32/64 etc //Check, confirm and remove
function MaxEx(A,B:LongWord):LongWord; inline; //To Do //The FPC Math unit has Min/Max etc with overloads for 32/64 etc //Check, confirm and remove

function Min64(const A,B:Int64):Int64; inline; //To Do //The FPC Math unit has Min/Max etc with overloads for 32/64 etc //Check, confirm and remove
function Max64(const A,B:Int64):Int64; inline; //To Do //The FPC Math unit has Min/Max etc with overloads for 32/64 etc //Check, confirm and remove

function Or64(const Value1,Value2:Int64):Int64; inline; //To Do //Can the inbuilt and/or/not/xor handle Int64 ?  //Check, confirm and remove
function And64(const Value1,Value2:Int64):Int64; inline; //To Do //Can the inbuilt and/or/not/xor handle Int64 ?  //Check, confirm and remove
function Xor64(const Value1,Value2:Int64):Int64; inline; //To Do //Can the inbuilt and/or/not/xor handle Int64 ?  //Check, confirm and remove
function Not64(const Value:Int64):Int64; inline; //To Do //Can the inbuilt and/or/not/xor handle Int64 ?  //Check, confirm and remove

function Rol32(Value:LongWord;Count:Byte):LongWord; inline; //To Do //Is there an inbuilt Rol/Ror in the RTL that can handle both 32 and 64 ? //Check, confirm and remove
function Ror32(Value:LongWord;Count:Byte):LongWord; inline; //To Do //Is there an inbuilt Rol/Ror in the RTL that can handle both 32 and 64 ? //Check, confirm and remove

function WordSwap(AValue:Word):Word; inline; //To Do //Check which of the RTL functions to replace with, confirm and remove
function LongSwap(AValue:LongWord):LongWord; inline; //To Do //Check which of the RTL functions to replace with, confirm and remove
function Int64Swap(const AValue:Int64):Int64; inline; //To Do //Check which of the RTL functions to replace with, confirm and remove
function BufferSwap(ABuffer:Pointer;ASize:LongWord):Boolean;

{==============================================================================}
{Hash Functions (Ultibo)}
function GenerateNameHash(const Name:String;Size:Integer):LongWord;
function GeneratePasswordHash(const Password:String):LongWord; deprecated;
function GenerateStringHash(const Value:String;CaseSensitive:Boolean):LongWord;

{==============================================================================}
{Locale Functions (Compatibility)}
function IsValidLocale(LocaleID:LCID;dwFlags:DWORD):BOOL; inline;

function GetSystemDefaultLCID:LCID; inline;
function GetUserDefaultLCID:LCID; inline;

//GetLocaleInfo //To Do
//SetLocaleInfo //To Do

function GetThreadLocale:LCID;
function SetThreadLocale(LocaleID:LCID):BOOL;

{==============================================================================}
{Locale Functions (Ultibo)}
function SetSystemDefaultLCID(LocaleID:LCID):BOOL; inline;

function WideCharToString(const ABuffer:PWideChar):String;
function WideCharLenToString(const ABuffer:PWideChar;ALength:Integer):String;
function StringToWideChar(const AString:String;ABuffer:PWideChar;ASize:Integer):Boolean;

{==============================================================================}
{Code Page Functions (Compatibility)}
function IsValidCodePage(CodePage:UINT):BOOL; inline;

function GetACP:UINT; inline;
function GetOEMCP:UINT; inline;

function GetConsoleCP:UINT; inline;
function SetConsoleCP(wCodePageID:UINT):BOOL; inline;

function GetConsoleOutputCP:UINT; inline;
function SetConsoleOutputCP(wCodePageID:UINT):BOOL; inline;

function GetCPInfo(CodePage:UINT;var lpCPInfo:TCPInfo):BOOL; inline;

function GetCPInfoEx(CodePage:UINT;dwFlags:DWORD;var lpCPInfoEx:CPINFOEXA):BOOL; inline;
function GetCPInfoExA(CodePage:UINT;dwFlags:DWORD;var lpCPInfoEx:CPINFOEXA):BOOL; inline;
function GetCPInfoExW(CodePage:UINT;dwFlags:DWORD;var lpCPInfoEx:CPINFOEXW):BOOL; inline;

{==============================================================================}
{Code Page Functions (Ultibo)}
function SetACP(CodePage:UINT):BOOL; inline;
function SetOEMCP(CodePage:UINT):BOOL; inline;

{==============================================================================}
{Translation Functions (Compatibility)}
function MultiByteToWideChar(CodePage:UINT;dwFlags:DWORD;lpMultiByteStr:LPCSTR;cbMultiByte:Integer;lpWideCharStr:LPWSTR;cchWideChar:Integer):Integer; inline;
function WideCharToMultiByte(CodePage:UINT;dwFlags:DWORD;lpWideCharStr:LPCWSTR;cchWideChar:Integer;lpMultiByteStr:LPSTR;cbMultiByte:Integer;lpDefaultChar:LPCSTR;lpUsedDefaultChar:LPBOOL):Integer; inline;

function CompareString(Locale:LCID;dwCmpFlags:DWORD;lpString1:LPCSTR;cchCount1:Integer;lpString2:LPCSTR;cchCount2:Integer):Integer; inline;
function CompareStringA(Locale:LCID;dwCmpFlags:DWORD;lpString1:LPCSTR;cchCount1:Integer;lpString2:LPCSTR;cchCount2:Integer):Integer; inline;
function CompareStringW(Locale:LCID;dwCmpFlags:DWORD;lpString1:LPCWSTR;cchCount1:Integer;lpString2:LPCWSTR;cchCount2:Integer):Integer; inline;

function CharUpper(lpsz:LPSTR):LPSTR; inline;
function CharUpperA(lpsz:LPSTR):LPSTR; inline;
function CharUpperW(lpsz:LPWSTR):LPWSTR; inline;

function CharUpperBuff(lpsz:LPSTR;cchLength:DWORD):DWORD; inline;
function CharUpperBuffA(lpsz:LPSTR;cchLength:DWORD):DWORD; inline;
function CharUpperBuffW(lpsz:LPWSTR;cchLength:DWORD):DWORD; inline;

function CharLower(lpsz:LPSTR):LPSTR; inline;
function CharLowerA(lpsz:LPSTR):LPSTR; inline;
function CharLowerW(lpsz:LPWSTR):LPWSTR; inline;

function CharLowerBuff(lpsz:LPSTR;cchLength:DWORD):DWORD; inline;
function CharLowerBuffA(lpsz:LPSTR;cchLength:DWORD):DWORD; inline;
function CharLowerBuffW(lpsz:LPWSTR;cchLength:DWORD):DWORD; inline;

function AnsiToOem(lpszSrc:LPCSTR;lpszDst:LPSTR):BOOL; inline;
function AnsiToOemBuff(lpszSrc:LPCSTR;lpszDst:LPSTR;cchDstLength:DWORD):BOOL; inline;
function OemToAnsi(lpszSrc:LPCSTR;lpszDst:LPSTR):BOOL; inline;
function OemToAnsiBuff(lpszSrc:LPCSTR;lpszDst:LPSTR;cchDstLength:DWORD):BOOL; inline;

function CharToOem(lpszSrc:LPCSTR;lpszDst:LPSTR):BOOL; inline;
function CharToOemA(lpszSrc:LPCSTR;lpszDst:LPSTR):BOOL; inline;
function CharToOemW(lpszSrc:LPCWSTR;lpszDst:LPSTR):BOOL; inline;

function OemToChar(lpszSrc:LPCSTR;lpszDst:LPSTR):BOOL; inline;
function OemToCharA(lpszSrc:LPCSTR;lpszDst:LPSTR):BOOL; inline;
function OemToCharW(lpszSrc:LPCSTR;lpszDst:LPWSTR):BOOL; inline;

function CharToOemBuff(lpszSrc:LPCSTR;lpszDst:LPSTR;cchDstLength:DWORD):BOOL; inline;
function CharToOemBuffA(lpszSrc:LPCSTR;lpszDst:LPSTR;cchDstLength:DWORD):BOOL; inline;
function CharToOemBuffW(lpszSrc:LPCWSTR;lpszDst:LPSTR;cchDstLength:DWORD):BOOL; inline;

function OemToCharBuff(lpszSrc:LPCSTR;lpszDst:LPSTR;cchDstLength:DWORD):BOOL; inline;
function OemToCharBuffA(lpszSrc:LPCSTR;lpszDst:LPSTR;cchDstLength:DWORD):BOOL; inline;
function OemToCharBuffW(lpszSrc:LPCSTR;lpszDst:LPWSTR;cchDstLength:DWORD):BOOL; inline;


//To Do
//IsCharAlpha
//IsCharAlphaNumeric
//etc

//OemKeyScan
//VkKeyScan

//See: JwaWinUser

//Others...

//SetTimer  //Hook up to TimerCreate/Destroy in Threads
//KillTimer
//etc

{==============================================================================}
{Handle Functions (Compatibility)}
function CloseHandle(hObject:HANDLE):BOOL;
function DuplicateHandle(hSourceProcessHandle:HANDLE;hSourceHandle:HANDLE;hTargetProcessHandle:HANDLE;lpTargetHandle:LPHANDLE;dwDesiredAccess:DWORD;bInheritHandle:BOOL;dwOptions:DWORD):BOOL;

function GetHandleInformation(hObject:HANDLE;var lpdwFlags:DWORD):BOOL;
function SetHandleInformation(hObject:HANDLE;dwMask:DWORD;dwFlags:DWORD):BOOL;

{==============================================================================}
{Memory Functions (Compatibility)}
procedure MoveMemory(Destination,Source:PVOID;Length:SIZE_T); inline;
procedure CopyMemory(Destination,Source:PVOID;Length:SIZE_T); inline;
procedure FillMemory(Destination:PVOID;Length:SIZE_T;Fill:BYTE); inline;
procedure ZeroMemory(Destination:PVOID;Length:SIZE_T); inline;

function GlobalAlloc(uFlags:UINT;dwBytes:SIZE_T):HGLOBAL;
function GlobalReAlloc(hMem:HGLOBAL;dwBytes:SIZE_T;uFlags:UINT):HGLOBAL;
function GlobalFree(hMem:HGLOBAL):HGLOBAL;

function GlobalSize(hMem:HGLOBAL):SIZE_T;
function GlobalFlags(hMem:HGLOBAL):UINT;

function GlobalLock(hMem:HGLOBAL):LPVOID;
function GlobalUnlock(hMem:HGLOBAL):BOOL;

function GlobalHandle(pMem:LPCVOID):HGLOBAL;

procedure GlobalMemoryStatus(var lpBuffer:MEMORYSTATUS);
function GlobalMemoryStatusEx(var lpBuffer:MEMORYSTATUSEX):BOOL;

function LocalAlloc(uFlags:UINT;uBytes:SIZE_T):HLOCAL;
function LocalReAlloc(hMem:HLOCAL;uBytes:SIZE_T;uFlags:UINT):HLOCAL;
function LocalFree(hMem:HLOCAL):HLOCAL;

function LocalSize(hMem:HLOCAL):SIZE_T;
function LocalFlags(hMem:HLOCAL):UINT;

function LocalLock(hMem:HLOCAL):LPVOID;
function LocalUnlock(hMem:HLOCAL):BOOL;

function LocalHandle(pMem:LPCVOID):HLOCAL;

function VirtualAlloc(lpAddress:LPVOID;dwSize:SIZE_T;flAllocationType:DWORD;flProtect:DWORD):LPVOID;
function VirtualFree(lpAddress:LPVOID;dwSize:SIZE_T;dwFreeType:DWORD):BOOL;

function VirtualQuery(lpAddress:LPCVOID;var lpBuffer:MEMORY_BASIC_INFORMATION;dwLength:DWORD):DWORD;

function VirtualLock(lpAddress:LPVOID;dwSize:SIZE_T):BOOL;
function VirtualUnlock(lpAddress:LPVOID;dwSize:SIZE_T):BOOL;

function FlushInstructionCache(hProcess:HANDLE;lpBaseAddress:LPCVOID;dwSize:DWORD):BOOL;

function GetNumaHighestNodeNumber(var HighestNodeNumber:ULONG):BOOL;
function GetNumaProcessorNode(const Processor:Byte;var NodeNumber:Byte):BOOL;

{==============================================================================}
{Tls Functions (Compatibility)}
function TlsAlloc:DWORD;
function TlsAllocEx(bFree:BOOL):DWORD;
function TlsGetValue(dwTlsIndex:DWORD):LPVOID;
function TlsSetValue(dwTlsIndex:DWORD;lpTlsValue:LPVOID):BOOL;
function TlsFree(dwTlsIndex:DWORD):BOOL;

{==============================================================================}
{Thread Functions (Compatibility)}
function SwitchToThread:BOOL; inline;

procedure Sleep(dwMilliseconds:DWORD); inline;
function SleepEx(dwMilliseconds:DWORD;bAlertable:BOOL):DWORD; inline;

function GetCurrentThread:HANDLE;
function GetCurrentThreadId:DWORD;

function GetThreadPriority(hThread:HANDLE):Integer;
function SetThreadPriority(hThread:HANDLE;nPriority:Integer):BOOL;

function GetExitCodeThread(hThread:HANDLE;var lpExitCode:DWORD):BOOL;

function GetThreadAffinityMask(hThread:HANDLE):DWORD_PTR;
function SetThreadAffinityMask(hThread:HANDLE;dwThreadAffinityMask:DWORD_PTR):DWORD_PTR;

function GetThreadTimes(hThread:HANDLE;var lpCreationTime,lpExitTime,lpKernelTime,lpUserTime:FILETIME):BOOL;

function CreateThread(lpThreadAttributes:LPSECURITY_ATTRIBUTES;dwStackSize:DWORD;lpStartAddress:LPTHREAD_START_ROUTINE;lpParameter:LPVOID;dwCreationFlags:DWORD;lpThreadId:LPDWORD):HANDLE;
function OpenThread(dwDesiredAccess:DWORD;bInheritHandle:BOOL;dwThreadId:DWORD):HANDLE;

function SuspendThread(hThread:HANDLE):DWORD;
function ResumeThread(hThread:HANDLE):DWORD;

function TerminateThread(hThread:HANDLE;dwExitCode:DWORD):BOOL;

procedure ExitThread(dwExitCode:DWORD);

function WaitForSingleObject(hHandle:HANDLE;dwMilliseconds:DWORD):DWORD;
function WaitForMultipleObjects(nCount:DWORD;lpHandles:PHANDLE;bWaitAll:BOOL;dwMilliseconds:DWORD):DWORD;

function WaitForSingleObjectEx(hHandle:HANDLE;dwMilliseconds:DWORD;bAlertable:BOOL):DWORD;
function WaitForMultipleObjectsEx(nCount:DWORD;lpHandles:PHANDLE;bWaitAll:BOOL;dwMilliseconds:DWORD;bAlertable:BOOL):DWORD;

{==============================================================================}
{Thread Functions (Ultibo)}
function BeginThreadEx(ThreadFunction:TThreadFunc;Parameter:Pointer;var ThreadId:TThreadID;const StackSize:SizeUInt;Priority,Affinity,CPU:LongWord;const Name:PChar):TThreadID;

{==============================================================================}
{Message Functions (Compatibility)}
function WaitMessage:BOOL;

function GetMessage(lpMsg:LPMSG;hThread:HANDLE;wMsgFilterMin,wMsgFilterMax:UINT):BOOL;
function PeekMessage(var lpMsg:MSG;hThread:HANDLE;wMsgFilterMin,wMsgFilterMax,wRemoveMsg:UINT):BOOL;

function PostMessage(hThread:HANDLE;Msg:UINT;wParam:WPARAM;lParam:LPARAM):BOOL;
function SendMessage(hThread:HANDLE;Msg:UINT;wParam:WPARAM;lParam:LPARAM):LRESULT;
function SendMessageTimeout(hThread:HANDLE;Msg:UINT;wParam:WPARAM;lParam:LPARAM;fuFlags,uTimeout:UINT;var lpdwResult:DWORD_PTR):LRESULT;

{==============================================================================}
{Notification Functions (Compatibility)}
//To Do
//RegisterDeviceNotification
//
//See: JwaWinUser.pas

{==============================================================================}
{Interlocked Functions (Compatibility)}
function InterlockedIncrement(var lpAddend:LONG):LONG; inline;
function InterlockedDecrement(var lpAddend:LONG):LONG; inline;

function InterlockedExchange(var Target:LONG;Value:LONG):LONG; inline;
function InterlockedExchangePointer(var Target:PVOID;Value:PVOID):PVOID; inline;

function InterlockedExchangeAdd(var Addend: LONG; Value: LONG): LONG; inline;
function InterlockedCompareExchange(var Destination: LONG; Exchange: LONG; Comperand: LONG): LONG; inline;
function InterlockedCompareExchangePointer(var Destination: PVOID; Exchange, Comperand: PVOID): PVOID; inline;

{==============================================================================}
{Mutex Functions (Compatibility)}
function CreateMutex(lpMutexAttributes:LPSECURITY_ATTRIBUTES;bInitialOwner:BOOL;const lpName:LPCSTR):HANDLE; inline;
function CreateMutexA(lpMutexAttributes:LPSECURITY_ATTRIBUTES;bInitialOwner:BOOL;const lpName:LPCSTR):HANDLE;
function CreateMutexW(lpMutexAttributes:LPSECURITY_ATTRIBUTES;bInitialOwner:BOOL;const lpName:LPCWSTR):HANDLE;

function OpenMutex(dwDesiredAccess:DWORD;bInheritHandle:BOOL;const lpName:LPCSTR):HANDLE; inline;
function OpenMutexA(dwDesiredAccess:DWORD;bInheritHandle:BOOL;const lpName:LPCSTR):HANDLE;
function OpenMutexW(dwDesiredAccess:DWORD;bInheritHandle:BOOL;const lpName:LPCWSTR):HANDLE;

function ReleaseMutex(hMutex:HANDLE):BOOL;

{==============================================================================}
{Semaphore Functions (Compatibility)}
function CreateSemaphore(lpSemaphoreAttributes:LPSECURITY_ATTRIBUTES;lInitialCount,lMaximumCount:LONG;const lpName:LPCSTR):HANDLE; inline;
function CreateSemaphoreA(lpSemaphoreAttributes:LPSECURITY_ATTRIBUTES;lInitialCount,lMaximumCount:LONG;const lpName:LPCSTR):HANDLE;
function CreateSemaphoreW(lpSemaphoreAttributes:LPSECURITY_ATTRIBUTES;lInitialCount,lMaximumCount:LONG;const lpName:LPCWSTR):HANDLE;

function OpenSemaphore(dwDesiredAccess:DWORD;bInheritHandle:BOOL;const lpName:LPCSTR):HANDLE; inline;
function OpenSemaphoreA(dwDesiredAccess:DWORD;bInheritHandle:BOOL;const lpName:LPCSTR):HANDLE;
function OpenSemaphoreW(dwDesiredAccess:DWORD;bInheritHandle:BOOL;const lpName:LPCWSTR):HANDLE;

function ReleaseSemaphore(hSemaphore:HANDLE;lReleaseCount:LONG;lpPreviousCount:LPLONG):BOOL;

{==============================================================================}
{Critical Section Functions (Compatibility)}
procedure InitializeCriticalSection(var lpCriticalSection:CRITICAL_SECTION);
procedure EnterCriticalSection(var lpCriticalSection:CRITICAL_SECTION);
procedure LeaveCriticalSection(var lpCriticalSection:CRITICAL_SECTION);
function TryEnterCriticalSection(var lpCriticalSection:CRITICAL_SECTION):BOOL;

function InitializeCriticalSectionAndSpinCount(var lpCriticalSection:CRITICAL_SECTION;dwSpinCount:DWORD):BOOL;
function SetCriticalSectionSpinCount(var lpCriticalSection:CRITICAL_SECTION;dwSpinCount:DWORD):DWORD;

procedure DeleteCriticalSection(var lpCriticalSection:CRITICAL_SECTION);

{==============================================================================}
{Condition Variable Functions (Compatibility)}
procedure InitializeConditionVariable(var ConditionVariable:CONDITION_VARIABLE);

procedure WakeConditionVariable(var ConditionVariable:CONDITION_VARIABLE);
procedure WakeAllConditionVariable(var ConditionVariable:CONDITION_VARIABLE);

function SleepConditionVariableCS(var ConditionVariable:CONDITION_VARIABLE; var CriticalSection:CRITICAL_SECTION;dwMilliseconds:DWORD):BOOL;

procedure DeleteConditionVariable(var ConditionVariable:CONDITION_VARIABLE);

{==============================================================================}
{Event Functions (Compatibility)}
function CreateEvent(lpEventAttributes:LPSECURITY_ATTRIBUTES;bManualReset,bInitialState:BOOL;const lpName:LPCSTR):HANDLE; inline;
function CreateEventA(lpEventAttributes:LPSECURITY_ATTRIBUTES;bManualReset,bInitialState:BOOL;const lpName:LPCSTR):HANDLE;
function CreateEventW(lpEventAttributes:LPSECURITY_ATTRIBUTES;bManualReset,bInitialState:BOOL;const lpName:LPCWSTR):HANDLE;

function OpenEvent(dwDesiredAccess:DWORD;bInheritHandle:BOOL;const lpName:LPCSTR):HANDLE; inline;
function OpenEventA(dwDesiredAccess:DWORD;bInheritHandle:BOOL;const lpName:LPCSTR):HANDLE;
function OpenEventW(dwDesiredAccess:DWORD;bInheritHandle:BOOL;const lpName:LPCWSTR):HANDLE;

function SetEvent(hEvent:HANDLE):BOOL;
function ResetEvent(hEvent:HANDLE):BOOL;
function PulseEvent(hEvent:HANDLE):BOOL;

{==============================================================================}
{Process Functions (Compatibility)}
function GetProcessAffinityMask(hProcess:HANDLE;var lpProcessAffinityMask,lpSystemAffinityMask:DWORD_PTR):BOOL;
function SetProcessAffinityMask(hProcess:HANDLE;dwProcessAffinityMask:DWORD_PTR):BOOL;

function GetProcessTimes(hProcess:HANDLE;var lpCreationTime,lpExitTime,lpKernelTime,lpUserTime:FILETIME):BOOL;
function GetProcessIoCounters(hProcess:HANDLE;var lpIoCounters:IO_COUNTERS):BOOL;

function GetCurrentProcess:HANDLE;
function GetCurrentProcessId:DWORD;

procedure ExitProcess(uExitCode:UINT);
procedure FatalExit(ExitCode:Integer);

function TerminateProcess(hProcess:HANDLE;uExitCode:UINT):BOOL;

{==============================================================================}
{Debug Functions (Compatibility)}
procedure OutputDebugString(const lpOutputString:LPCSTR); inline;
procedure OutputDebugStringA(const lpOutputString:LPCSTR);
procedure OutputDebugStringW(const lpOutputString:LPCWSTR);

{==============================================================================}
{Library Functions (Compatibility)}
function lstrcmp(lpString1,lpString2:LPCSTR):Integer; inline;
function lstrcmpA(lpString1,lpString2:LPCSTR):Integer;
function lstrcmpW(lpString1,lpString2:LPCWSTR):Integer;

function lstrcmpi(lpString1,lpString2:LPCSTR):Integer; inline;
function lstrcmpiA(lpString1,lpString2:LPCSTR):Integer;
function lstrcmpiW(lpString1,lpString2:LPCWSTR):Integer;

function lstrcpy(lpString1:LPSTR;lpString2:LPCSTR):LPSTR;  inline;
function lstrcpyA(lpString1:LPSTR;lpString2:LPCSTR):LPSTR;
function lstrcpyW(lpString1:LPWSTR;lpString2:LPCWSTR):LPWSTR;

function lstrcpyn(lpString1:LPSTR;lpString2:LPCSTR;iMaxLength:Integer):LPSTR; inline;
function lstrcpynA(lpString1:LPSTR;lpString2:LPCSTR;iMaxLength:Integer):LPSTR;
function lstrcpynW(lpString1:LPWSTR;lpString2:LPCWSTR;iMaxLength:Integer):LPWSTR;

function lstrcat(lpString1:LPSTR;lpString2:LPCSTR):LPSTR; inline;
function lstrcatA(lpString1:LPSTR;lpString2:LPCSTR):LPSTR;
function lstrcatW(lpString1:LPWSTR;lpString2:LPCWSTR):LPWSTR;

function lstrlen(lpString:LPCSTR):Integer; inline;
function lstrlenA(lpString:LPCSTR):Integer;
function lstrlenW(lpString:LPCWSTR):Integer;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Ultibo variables}
var
 GUIDRandomized:Boolean;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure UltiboInit;
begin
 {}
 {Setup System Handlers}
  {Nothing}

 {Setup SysUtils Handlers}
 SysUtilsFileTimeToSystemTimeHandler:=FileTimeToSystemTime;
 SysUtilsFileTimeToLocalFileTimeHandler:=FileTimeToLocalFileTime;
end;

{==============================================================================}
{==============================================================================}
{General Functions (Compatibility)}
function GetVersion:DWORD;
begin
 {}
 Result:=$80000000 or (ULTIBO_RELEASE_VERSION_REVISION shl 16) or (ULTIBO_RELEASE_VERSION_MINOR shl 8) or ULTIBO_RELEASE_VERSION_MAJOR;
end;

{==============================================================================}

function GetVersionEx(lpVersionInformation:LPOSVERSIONINFOA):BOOL; inline;
begin
 {}
 Result:=GetVersionExA(lpVersionInformation);
end;

{==============================================================================}

function GetVersionExA(lpVersionInformation:LPOSVERSIONINFOA):BOOL;
begin
 {}
 Result:=False;

 if lpVersionInformation = nil then Exit;
 if lpVersionInformation.dwOSVersionInfoSize < SizeOf(TOsVersionInfoA) then Exit;

 {Get Version Information}
 lpVersionInformation.dwMajorVersion:=ULTIBO_RELEASE_VERSION_MAJOR;
 lpVersionInformation.dwMinorVersion:=ULTIBO_RELEASE_VERSION_MINOR;
 lpVersionInformation.dwBuildNumber:=ULTIBO_RELEASE_VERSION_REVISION;
 lpVersionInformation.dwPlatformId:=VER_PLATFORM_ULTIBO;
 StrLCopy(lpVersionInformation.szCSDVersion,ULTIBO_RELEASE_NAME,128);

 Result:=True;
end;

{==============================================================================}

function GetVersionExW(lpVersionInformation:LPOSVERSIONINFOW):BOOL;
begin
 {}
 Result:=False;

 if lpVersionInformation = nil then Exit;
 if lpVersionInformation.dwOSVersionInfoSize < SizeOf(TOsVersionInfoW) then Exit;

 {Get Version Information}
 lpVersionInformation.dwMajorVersion:=ULTIBO_RELEASE_VERSION_MAJOR;
 lpVersionInformation.dwMinorVersion:=ULTIBO_RELEASE_VERSION_MINOR;
 lpVersionInformation.dwBuildNumber:=ULTIBO_RELEASE_VERSION_REVISION;
 lpVersionInformation.dwPlatformId:=VER_PLATFORM_ULTIBO;
 StringToWideChar(ULTIBO_RELEASE_NAME,lpVersionInformation.szCSDVersion,128 shl 1); {Multiply by SizeOf(WideChar)}

 Result:=True;
end;

{==============================================================================}

procedure GetSystemInfo(var lpSystemInfo:SYSTEM_INFO);
var
 CPUType:LongWord;
begin
 {}
 FillChar(lpSystemInfo,SizeOf(SYSTEM_INFO),0);

 {Get System Info}
 lpSystemInfo.wProcessorArchitecture:=0;
 lpSystemInfo.wReserved:=0;
 lpSystemInfo.dwPageSize:=Platform.MemoryGetPageSize;
 lpSystemInfo.lpMinimumApplicationAddress:=Pointer(Platform.MemoryGetBase);
 lpSystemInfo.lpMaximumApplicationAddress:=Pointer(Platform.MemoryGetSize);
 {$IFDEF CPU32}
 if Platform.MemoryGetSize >= SIZE_4G then lpSystemInfo.lpMaximumApplicationAddress:=Pointer($FFFFFFFF);
 {$ENDIF CPU32}
 lpSystemInfo.dwActiveProcessorMask:=Platform.CPUGetMask;
 lpSystemInfo.dwNumberOfProcessors:=Platform.CPUGetCount;
 lpSystemInfo.dwProcessorType:=0;
 lpSystemInfo.dwAllocationGranularity:=Platform.MemoryGetPageSize;
 lpSystemInfo.wProcessorLevel:=0;
 lpSystemInfo.wProcessorRevision:=Platform.CPUGetRevision;

 {Get CPU Type}
 CPUType:=Platform.CPUGetType;

 {Get wProcessorArchitecture / dwProcessorType}
 case CPUType of
  CPU_TYPE_ARMV6:begin
    lpSystemInfo.wProcessorArchitecture:=PROCESSOR_ARCHITECTURE_ARM;
    lpSystemInfo.dwProcessorType:=PROCESSOR_ARM_6;
   end;
  CPU_TYPE_ARMV7:begin
    lpSystemInfo.wProcessorArchitecture:=PROCESSOR_ARCHITECTURE_ARM;
    lpSystemInfo.dwProcessorType:=PROCESSOR_ARM_7;
   end;
  CPU_TYPE_ARMV8:begin
    lpSystemInfo.wProcessorArchitecture:=PROCESSOR_ARCHITECTURE_ARM;
    lpSystemInfo.dwProcessorType:=PROCESSOR_ARM_8;
   end;
 end;
end;

{==============================================================================}

procedure GetNativeSystemInfo(lpSystemInfo:LPSYSTEM_INFO);
begin
 {}
 GetSystemInfo(lpSystemInfo^);
end;

{==============================================================================}

function GetLargePageMinimum:SIZE_T;
begin
 {}
 Result:=Platform.MemoryGetLargePageSize;
end;

{==============================================================================}

function GetComputerName(lpBuffer:LPSTR;var nSize:DWORD):BOOL; inline;
begin
 {}
 Result:=GetComputerNameA(lpBuffer,nSize);
end;

{==============================================================================}

function GetComputerNameA(lpBuffer:LPSTR;var nSize:DWORD):BOOL;
var
 Value:String;
begin
 {}
 Result:=False;

 if lpBuffer = nil then
  begin
   if nSize <> 0 then Exit;

   {Get Host Name}
   Value:=Platform.HostGetName;
   nSize:=Length(Value);
  end
 else
  begin
   if nSize = 0 then Exit;

   {Get Host Name}
   Value:=Platform.HostGetName;
   StrLCopy(lpBuffer,PChar(Value),nSize);
   nSize:=Length(Value);
  end;

 Result:=True;
end;

{==============================================================================}

function GetComputerNameW(lpBuffer:LPWSTR;var nSize:DWORD):BOOL;
var
 Value:String;
begin
 {}
 Result:=False;

 if lpBuffer = nil then
  begin
   if nSize <> 0 then Exit;

   {Get Host Name}
   Value:=Platform.HostGetName;
   nSize:=Length(Value);

   Result:=True;
  end
 else
  begin
   if nSize = 0 then Exit;

   {Get Host Name}
   Value:=Platform.HostGetName;
   if StringToWideChar(Value,lpBuffer,nSize shl 1) then {Buffer length in chars, Multiply by SizeOf(WideChar)} //To Do //Use default StringToWideChar from System ? //Watch for difference in size parameter (Bytes/Chars)
    begin
     nSize:=Length(Value);

     Result:=True;
    end;
  end;
end;

{==============================================================================}

function SetComputerName(const lpComputerName:LPCSTR):BOOL; inline;
begin
 {}
 Result:=SetComputerNameA(lpComputerName);
end;

{==============================================================================}

function SetComputerNameA(const lpComputerName:LPCSTR):BOOL;
begin
 {}
 Result:=False;

 if lpComputerName = nil then Exit;

 {Set Host Name}
 Result:=Platform.HostSetName(String(lpComputerName));
end;

{==============================================================================}

function SetComputerNameW(const lpComputerName:LPCWSTR):BOOL;
var
 ComputerName:String;
begin
 {}
 Result:=False;

 if lpComputerName = nil then Exit;

 ComputerName:=WideCharToString(lpComputerName);

 {Set Host Name}
 Result:=Platform.HostSetName(ComputerName);
end;

{==============================================================================}

function GetComputerNameEx(NameType:COMPUTER_NAME_FORMAT;lpBuffer:LPSTR;var nSize:DWORD):BOOL; inline;
begin
 {}
 Result:=GetComputerNameExA(NameType,lpBuffer,nSize);
end;

{==============================================================================}

function GetComputerNameExA(NameType:COMPUTER_NAME_FORMAT;lpBuffer:LPSTR;var nSize:DWORD):BOOL;
var
 Value:String;
 Name:String;
 Domain:String;
begin
 {}
 Result:=False;

 if lpBuffer = nil then
  begin
   if nSize <> 0 then Exit;

   {Get Host Name/Domain}
   Name:=Platform.HostGetName;
   Domain:=Platform.HostGetDomain;

   {Check Name Type}
   case NameType of
    ComputerNameNetBIOS,ComputerNamePhysicalNetBIOS,ComputerNameDnsHostname,ComputerNamePhysicalDnsHostname:begin
      nSize:=Length(Name);
     end;
    ComputerNameDnsDomain,ComputerNamePhysicalDnsDomain:begin
      nSize:=Length(Domain);
     end;
    ComputerNameDnsFullyQualified,ComputerNamePhysicalDnsFullyQualified:begin
      Value:=Name;
      if Length(Domain) > 0 then Value:=Value + '.' + Domain;
      nSize:=Length(Value);
     end;
   end;

   Result:=True;
  end
 else
  begin
   if nSize = 0 then Exit;

   {Get Host Name/Domain}
   Name:=Platform.HostGetName;
   Domain:=Platform.HostGetDomain;

   {Check Name Type}
   case NameType of
    ComputerNameNetBIOS,ComputerNamePhysicalNetBIOS,ComputerNameDnsHostname,ComputerNamePhysicalDnsHostname:begin
      StrLCopy(lpBuffer,PChar(Name),nSize);
      nSize:=Length(Name);
     end;
    ComputerNameDnsDomain,ComputerNamePhysicalDnsDomain:begin
      StrLCopy(lpBuffer,PChar(Domain),nSize);
      nSize:=Length(Domain);
     end;
    ComputerNameDnsFullyQualified,ComputerNamePhysicalDnsFullyQualified:begin
      Value:=Name;
      if Length(Domain) > 0 then Value:=Value + '.' + Domain;
      StrLCopy(lpBuffer,PChar(Value),nSize);
      nSize:=Length(Value);
     end;
   end;

   Result:=True;
  end;
end;

{==============================================================================}

function GetComputerNameExW(NameType:COMPUTER_NAME_FORMAT;lpBuffer:LPWSTR;var nSize:DWORD):BOOL;
var
 Value:String;
 Name:String;
 Domain:String;
begin
 {}
 Result:=False;

 if lpBuffer = nil then Exit;
 if nSize = 0 then Exit;

 if lpBuffer = nil then
  begin
   if nSize <> 0 then Exit;

   {Get Host Name/Domain}
   Name:=Platform.HostGetName;
   Domain:=Platform.HostGetDomain;

   {Check Name Type}
   case NameType of
    ComputerNameNetBIOS,ComputerNamePhysicalNetBIOS,ComputerNameDnsHostname,ComputerNamePhysicalDnsHostname:begin
      nSize:=Length(Name);
     end;
    ComputerNameDnsDomain,ComputerNamePhysicalDnsDomain:begin
      nSize:=Length(Domain);
     end;
    ComputerNameDnsFullyQualified,ComputerNamePhysicalDnsFullyQualified:begin
      Value:=Name;
      if Length(Domain) > 0 then Value:=Value + '.' + Domain;
      nSize:=Length(Value);
     end;
   end;

   Result:=True;
  end
 else
  begin
   if nSize = 0 then Exit;

   {Get Host Name/Domain}
   Name:=Platform.HostGetName;
   Domain:=Platform.HostGetDomain;

   {Check Name Type}
   case NameType of
    ComputerNameNetBIOS,ComputerNamePhysicalNetBIOS,ComputerNameDnsHostname,ComputerNamePhysicalDnsHostname:begin
      if StringToWideChar(Name,lpBuffer,nSize shl 1) then {Buffer length in chars, Multiply by SizeOf(WideChar)} //To Do //Use default StringToWideChar from System ? //Watch for difference in size parameter (Bytes/Chars)
       begin
        nSize:=Length(Name);

        Result:=True;
       end;
     end;
    ComputerNameDnsDomain,ComputerNamePhysicalDnsDomain:begin
      if StringToWideChar(Domain,lpBuffer,nSize shl 1) then {Buffer length in chars, Multiply by SizeOf(WideChar)} //To Do //Use default StringToWideChar from System ? //Watch for difference in size parameter (Bytes/Chars)
       begin
        nSize:=Length(Domain);

        Result:=True;
       end;
     end;
    ComputerNameDnsFullyQualified,ComputerNamePhysicalDnsFullyQualified:begin
      Value:=Name;
      if Length(Domain) > 0 then Value:=Value + '.' + Domain;
      if StringToWideChar(Value,lpBuffer,nSize shl 1) then {Buffer length in chars, Multiply by SizeOf(WideChar)} //To Do //Use default StringToWideChar from System ? //Watch for difference in size parameter (Bytes/Chars)
       begin
        nSize:=Length(Value);

        Result:=True;
       end;
     end;
   end;
  end;
end;

{==============================================================================}

function SetComputerNameEx(NameType:COMPUTER_NAME_FORMAT;const lpBuffer:LPCSTR):BOOL; inline;
begin
 {}
 Result:=SetComputerNameExA(NameType,lpBuffer);
end;

{==============================================================================}

function SetComputerNameExA(NameType:COMPUTER_NAME_FORMAT;const lpBuffer:LPCSTR):BOOL;
var
 Name:String;
 Domain:String;
 PosIdx:Integer;
begin
 {}
 Result:=False;

 if lpBuffer = nil then Exit;

 {Check Name Type}
 case NameType of
  ComputerNameNetBIOS,ComputerNamePhysicalNetBIOS,ComputerNameDnsHostname,ComputerNamePhysicalDnsHostname:begin
   {Set Host Name}
   Result:=Platform.HostSetName(String(lpBuffer));
  end;
  ComputerNameDnsDomain,ComputerNamePhysicalDnsDomain:begin
   {Set Host Domain}
   Result:=Platform.HostSetDomain(String(lpBuffer));
  end;
  ComputerNameDnsFullyQualified,ComputerNamePhysicalDnsFullyQualified:begin
   {Set Host Name/Domain}
   Name:=String(lpBuffer);
   PosIdx:=Pos('.',Name);
   if PosIdx > 0 then
    begin
     Domain:=Copy(Name,PosIdx + 1,Length(Name));
     Name:=Copy(Name,1,PosIdx);

     Result:=Platform.HostSetName(Name);
     if Result then Result:=Platform.HostSetDomain(Domain);
    end;
  end;
 end;
end;

{==============================================================================}

function SetComputerNameExW(NameType:COMPUTER_NAME_FORMAT;const lpBuffer:LPCWSTR):BOOL;
var
 Name:String;
 Domain:String;
 Buffer:String;
 PosIdx:Integer;
begin
 {}
 Result:=False;

 if lpBuffer = nil then Exit;

 Buffer:=WideCharToString(lpBuffer);

 {Check Name Type}
 case NameType of
  ComputerNameNetBIOS,ComputerNamePhysicalNetBIOS,ComputerNameDnsHostname,ComputerNamePhysicalDnsHostname:begin
   {Set Host Name}
   Result:=Platform.HostSetName(Buffer);
  end;
  ComputerNameDnsDomain,ComputerNamePhysicalDnsDomain:begin
   {Set Host Domain}
   Result:=Platform.HostSetDomain(Buffer);
  end;
  ComputerNameDnsFullyQualified,ComputerNamePhysicalDnsFullyQualified:begin
   {Set Host Name/Domain}
   Name:=Buffer;
   PosIdx:=Pos('.',Name);
   if PosIdx > 0 then
    begin
     Domain:=Copy(Name,PosIdx + 1,Length(Name));
     Name:=Copy(Name,1,PosIdx);

     Result:=Platform.HostSetName(Name);
     if Result then Result:=Platform.HostSetDomain(Domain);
    end;
  end;
 end;
end;

{==============================================================================}

function ExitUltibo(dwReserved:DWORD;uReserved:UINT):BOOL;
begin
 {}
 Result:=(SystemRestart(0) = ERROR_SUCCESS);
end;

{==============================================================================}

function ExitUltiboEx(uFlags:UINT;dwReserved:DWORD):BOOL;
begin
 {}
 Result:=False;

 if (uFlags and (EWX_SHUTDOWN or EWX_POWEROFF)) <> 0 then
  begin
   {Shutdown}
   Result:=(SystemShutdown(0) = ERROR_SUCCESS);
  end
 else
  begin
   {Restart}
   Result:=(SystemRestart(0) = ERROR_SUCCESS);
  end;
end;

{==============================================================================}
{==============================================================================}
{General Functions (Ultibo)}
function RestartComputer(Delay:LongWord):Boolean;
{Delay: Milliseconds to delay before restart}
begin
 {}
 Result:=(SystemRestart(Delay) = ERROR_SUCCESS);
end;

{==============================================================================}

function ShutdownComputer(Delay:LongWord):Boolean;
{Delay: Milliseconds to delay before shutdown}
begin
 {}
 Result:=(SystemShutdown(Delay) = ERROR_SUCCESS);
end;

{==============================================================================}

function Uptime:TFileTime;
{Get the current system up time as a FileTime value}
begin
 {}
 Int64(Result):=SystemGetUptime;
end;

{==============================================================================}
{==============================================================================}
{Time Functions (Compatibility)}
function GetTickCount:DWORD;
begin
 {}
 if CLOCK_CYCLES_PER_MILLISECOND > 0 then
  begin
   Result:=ClockGetTotal div CLOCK_CYCLES_PER_MILLISECOND;
  end
 else
  begin
   Result:=ClockGetTotal;
  end;
end;

{==============================================================================}

function GetTickCount64:ULONGLONG;
begin
 {}
 if CLOCK_CYCLES_PER_MILLISECOND > 0 then
  begin
   Result:=ClockGetTotal div CLOCK_CYCLES_PER_MILLISECOND;
  end
 else
  begin
   Result:=ClockGetTotal;
  end;
end;

{==============================================================================}

procedure GetSystemTime(var lpSystemTime:SYSTEMTIME);
{Get the current system time in UTC}
var
 FileTime:TFileTime;
begin
 {}
 Int64(FileTime):=ClockGetTime;
 FileTimeToSystemTime(FileTime,lpSystemTime);
end;

{==============================================================================}

procedure GetSystemTimeAsFileTime(var lpSystemTimeAsFileTime:FILETIME);
{Get the current system time in UTC as a FileTime value}
begin
 {}
 Int64(lpSystemTimeAsFileTime):=ClockGetTime;
end;

{==============================================================================}

function SetSystemTime(var lpSystemTime:SYSTEMTIME):BOOL;
{Set the current system time in UTC}
var
 FileTime:TFileTime;
begin
 {}
 Result:=False;
 if SystemTimeToFileTime(lpSystemTime,FileTime) then
  begin
   ClockSetTime(Int64(FileTime),True);

   Result:=True;
  end;
end;

{==============================================================================}

procedure GetLocalTime(var lpSystemTime:SYSTEMTIME);
{Get the current local time}
var
 FileTime:TFileTime;
 LocalFileTime:TFileTime;
begin
 {}
 Int64(FileTime):=ClockGetTime;
 FileTimeToLocalFileTime(FileTime,LocalFileTime);
 FileTimeToSystemTime(LocalFileTime,lpSystemTime);
end;

{==============================================================================}

function SetLocalTime(var lpSystemTime:SYSTEMTIME):BOOL;
{Set the current local time}
var
 FileTime:TFileTime;
 LocalFileTime:TFileTime;
begin
 {}
 Result:=False;
 if SystemTimeToFileTime(lpSystemTime,LocalFileTime) then
  begin
   if LocalFileTimeToFileTime(LocalFileTime,FileTime) then
    begin
     ClockSetTime(Int64(FileTime),True);

     Result:=True;
    end;
  end;
end;

{==============================================================================}

function SystemTimeToTzSpecificLocalTime(lpTimeZoneInformation:LPTIME_ZONE_INFORMATION;var lpUniversalTime,lpLocalTime:SYSTEMTIME):BOOL;
var
 Offset:Int64;
 LocalTime:TFileTime;
 UniversalTime:TFileTime;
 Timezone:PTimezoneEntry;
begin
 {}
 Result:=False;

 if lpTimeZoneInformation = nil then
  begin
   {Get Default Timezone}
   Timezone:=TimezoneGetDefault;
  end
 else
  begin
   {Find Timezone by Standard Name}
   Timezone:=TimezoneFindByStandard(WideCharToString(@lpTimeZoneInformation.StandardName));
  end;
 if Timezone = nil then Exit;

 {Get Universal Time}
 if SystemTimeToFileTime(lpUniversalTime,UniversalTime) then
  begin
   {Get Offset}
   Offset:=TimezoneGetActiveBiasEx(Timezone,SystemFileTimeToDateTime(UniversalTime));
   Offset:=Offset * TIME_TICKS_PER_MINUTE;

   {Convert Time}
   Int64(LocalTime):=Int64(UniversalTime) - (Offset);

   {Return Local Time}
   Result:=FileTimeToSystemTime(LocalTime,lpLocalTime);
  end;
end;

{==============================================================================}

function TzSpecificLocalTimeToSystemTime(lpTimeZoneInformation:LPTIME_ZONE_INFORMATION;const lpLocalTime:SYSTEMTIME;var lpUniversalTime:SYSTEMTIME):BOOL;
var
 Offset:Int64;
 LocalTime:TFileTime;
 UniversalTime:TFileTime;
 Timezone:PTimezoneEntry;
begin
 {}
 Result:=False;

 if lpTimeZoneInformation = nil then
  begin
   {Get Default Timezone}
   Timezone:=TimezoneGetDefault;
  end
 else
  begin
   {Find Timezone by Standard Name}
   Timezone:=TimezoneFindByStandard(WideCharToString(@lpTimeZoneInformation.StandardName));
  end;
 if Timezone = nil then Exit;

 {Get Local Time}
 if SystemTimeToFileTime(lpLocalTime,LocalTime) then
  begin
   {Get Offset}
   Offset:=TimezoneGetActiveBiasEx(Timezone,SystemFileTimeToDateTime(LocalTime));
   Offset:=Offset * TIME_TICKS_PER_MINUTE;

   {Convert Time}
   Int64(UniversalTime):=Int64(LocalTime) + (Offset);

   {Return Universal Time}
   Result:=FileTimeToSystemTime(UniversalTime,lpUniversalTime);
  end;
end;

{==============================================================================}

function GetTimeZoneInformation(var lpTimeZoneInformation:TIME_ZONE_INFORMATION):DWORD;
var
 Timezone:PTimezoneEntry;
begin
 {}
 Result:=TIME_ZONE_ID_INVALID;

 FillChar(lpTimeZoneInformation,SizeOf(TIME_ZONE_INFORMATION),0);

 {Get Default Timezone}
 Timezone:=TimezoneGetDefault;
 if Timezone = nil then Exit;

 {Get Timezone Information}
 lpTimeZoneInformation.Bias:=Timezone.Bias;
 StringToWideChar(Timezone.StandardName,@lpTimeZoneInformation.StandardName,32 shl 1); {Multiply by SizeOf(WideChar)}
 lpTimeZoneInformation.StandardDate:=Timezone.StandardStart;
 lpTimeZoneInformation.StandardBias:=Timezone.StandardBias;
 StringToWideChar(Timezone.DaylightName,@lpTimeZoneInformation.DaylightName,32 shl 1); {Multiply by SizeOf(WideChar)}
 lpTimeZoneInformation.DaylightDate:=Timezone.DaylightStart;
 lpTimeZoneInformation.DaylightBias:=Timezone.DaylightBias;

 {Get Timezone State}
 Result:=TimezoneGetState(Timezone);
end;

{==============================================================================}

function SetTimeZoneInformation(const lpTimeZoneInformation:TIME_ZONE_INFORMATION):BOOL;
var
 Timezone:PTimezoneEntry;
begin
 {}
 Result:=False;

 {Get Default Timezone}
 Timezone:=TimezoneGetDefault;
 if Timezone = nil then Exit;

 {Set Timezone Information}
 Timezone.Bias:=lpTimeZoneInformation.Bias;
 Timezone.StandardName:=WideCharToString(@lpTimeZoneInformation.StandardName);
 Timezone.StandardStart:=lpTimeZoneInformation.StandardDate;
 Timezone.StandardBias:=lpTimeZoneInformation.StandardBias;
 Timezone.DaylightName:=WideCharToString(@lpTimeZoneInformation.DaylightName);
 Timezone.DaylightStart:=lpTimeZoneInformation.DaylightDate;
 Timezone.DaylightBias:=lpTimeZoneInformation.DaylightBias;

 Result:=True;
end;

{==============================================================================}

function SystemTimeToFileTime(const lpSystemTime:SYSTEMTIME;var lpFileTime:FILETIME):BOOL;
{Convert a SystemTime value to a FileTime value}
{Note: lpSystemTime is assumed to be UTC / lpFileTime is returned as UTC}
var
 DateTime:TDateTime;
begin
 {}
 Result:=False;
 try
  Int64(lpFileTime):=0;

  {Convert to DateTime}
  DateTime:=ComposeDateTime(EncodeDate(lpSystemTime.wYear,lpSystemTime.wMonth,lpSystemTime.wDay),EncodeTime(lpSystemTime.wHour,lpSystemTime.wMinute,lpSystemTime.wSecond,lpSystemTime.wMilliseconds));

  {Convert to FileTime}
  lpFileTime:=DateTimeToSystemFileTime(DateTime); {No Conversion}

  Result:=True;
 except
  {EncodeDate and EncodeTime can raise Exceptions}
 end;
end;

{==============================================================================}

function FileTimeToSystemTime(const lpFileTime:FILETIME;var lpSystemTime:SYSTEMTIME):BOOL;
{Convert a FileTime value to a SystemTime value}
{Note: lpFileTime is assumed to be UTC / lpSystemTime is returned as UTC}
{Note: If lpFileTime is less than 30/12/1899 then SystemTime will be zero}
var
 DateTime:TDateTime;
begin
 {}
 Result:=False;
 FillChar(lpSystemTime,SizeOf(SYSTEMTIME),0);

 if Int64(lpFileTime) < TIME_TICKS_TO_1899 then Exit;

 {Convert to DateTime}
 DateTime:=SystemFileTimeToDateTime(lpFileTime); {No Conversion}

 {Convert to SystemTime}
 DecodeDate(DateTime,lpSystemTime.wYear,lpSystemTime.wMonth,lpSystemTime.wDay);
 DecodeTime(DateTime,lpSystemTime.wHour,lpSystemTime.wMinute,lpSystemTime.wSecond,lpSystemTime.wMilliseconds);
 lpSystemTime.wDayOfWeek:=DayOfWeek(DateTime) - 1;

 Result:=True;
end;

{==============================================================================}

function FileTimeToLocalFileTime(const lpFileTime:FILETIME;var lpLocalFileTime:FILETIME):BOOL;
{Convert a FileTime in UTC to a FileTime in Local time}
{Note: TIMEZONE_TIME_OFFSET is the number of minutes offset from UTC}
var
 Offset:Int64;
begin
 {}
 Result:=True;

 {Check and Get Timezone Offset}
 Offset:=GetLocalTimeOffset; {TIMEZONE_TIME_OFFSET} {Avoid 32 bit overflow}
 Offset:=Offset * TIME_TICKS_PER_MINUTE;

 {Convert Time}
 Int64(lpLocalFileTime):=Int64(lpFileTime) - (Offset);
end;

{==============================================================================}

function LocalFileTimeToFileTime(const lpLocalFileTime:FILETIME;var lpFileTime:FILETIME):BOOL;
{Convert a FileTime in Local time to a FileTime in UTC}
{Note: TIMEZONE_TIME_OFFSET is the number of minutes offset from UTC}
var
 Offset:Int64;
begin
 {}
 Result:=True;

 {Check and Get Timezone Offset}
 Offset:=GetLocalTimeOffset; {TIMEZONE_TIME_OFFSET} {Avoid 32 bit overflow}
 Offset:=Offset * TIME_TICKS_PER_MINUTE;

 {Convert Time}
 Int64(lpFileTime):=Int64(lpLocalFileTime) + (Offset);
end;

{==============================================================================}

function CompareFileTime(const lpFileTime1,lpFileTime2:FILETIME):LONG;
begin
 {}
 if Int64(lpFileTime1) = Int64(lpFileTime2) then
  begin
   Result:=0;
  end
 else if Int64(lpFileTime1) < Int64(lpFileTime2) then
  begin
   Result:=-1;
  end
 else if Int64(lpFileTime1) > Int64(lpFileTime2) then
  begin
   Result:=1;
  end;
end;

{==============================================================================}

function FileTimeToDosDateTime(const lpFileTime:FILETIME;var lpFatDate,lpFatTime:WORD):BOOL;
{Convert a FileTime value to a DOS date and time value}
{Note: FileTime is assumed to be Local / DOS date and time is returned as Local}
{Note: If FileTime is less than 1/1/1980 then DOS date and time will be 1/1/1980}
var
 FileDate:LongInt;
 DateTime:TDateTime;
begin
 {}
 Result:=False;

 lpFatDate:=(PASCAL_TIME_DOS_TIME_START shr 16);
 lpFatTime:=0;

 {Check FileTime}
 if Int64(lpFileTime) < TIME_TICKS_TO_1980 then Exit;

 {Convert to DateTime}
 DateTime:=LocalFileTimeToDateTime(lpFileTime); {No Conversion}

 {Convert to FileDate}
 FileDate:=DateTimeToFileDate(DateTime);

 {Get DOS date and time}
 lpFatDate:=FileDate shr 16;
 lpFatTime:=FileDate and $FFFF;

 Result:=True;
end;

{==============================================================================}

function DosDateTimeToFileTime(wFatDate,wFatTime:WORD;var lpFileTime:FILETIME):BOOL;
{Convert a DOS date and time value to a FileTime value}
{Note: DOS date and time is assumed to be Local / FileTime is returned as Local}
{Note: If DOS date and time is less than 1/1/1980 then FileTime will be 1/1/1980}
var
 FileDate:LongInt;
 DateTime:TDateTime;
begin
 {}
 Result:=False;
 try
  Int64(lpFileTime):=TIME_TICKS_TO_1980;

  {Get File Date}
  FileDate:=(wFatDate shl 16) or wFatTime;
  if FileDate < PASCAL_TIME_DOS_TIME_START then Exit;

  {Convert to DateTime}
  DateTime:=FileDateToDateTime(FileDate);

  {Convert to FileTime}
  lpFileTime:=DateTimeToLocalFileTime(DateTime); {No Conversion}

  Result:=True;
 except
  {FileDateToDateTime can raise Exceptions}
 end;
end;

{==============================================================================}

function QueryPerformanceCounter(var lpPerformanceCount: LARGE_INTEGER): BOOL;
{Retrieves the current value of the performance counter, which is a high
 resolution (<1us) time stamp that can be used for time-interval measurements}
begin
 {}
 Result:=True;

 lpPerformanceCount.QuadPart:=ClockGetTotal;
end;

{==============================================================================}

function QueryPerformanceFrequency(var lpFrequency: LARGE_INTEGER): BOOL;
{Retrieves the frequency of the performance counter.
 The frequency of the performance counter is fixed at system boot and is
 consistent across all processors. Therefore, the frequency need only be
 queried upon application initialization, and the result can be cached}
begin
 {}
 Result:=True;

 lpFrequency.QuadPart:=CLOCK_FREQUENCY;
end;

{==============================================================================}
{==============================================================================}
{Time Functions (Ultibo)}
function GetCurrentTime:TFileTime;
{Get the current system time in UTC as a FileTime value}
begin
 {}
 Int64(Result):=ClockGetTime;
end;

{==============================================================================}

procedure SetCurrentTime(const ATime:TFileTime);
{Set the current system time in UTC from a FileTime value}
begin
 {}
 ClockSetTime(Int64(ATime),True);
end;

{==============================================================================}

function GetTimeAdjust:LongInt;
{Gets the time adjustment used internally}
begin
 {}
 Result:=TIMEZONE_TIME_ADJUST;
end;

{==============================================================================}

procedure SetTimeAdjust(AAdjust:LongInt);
{Sets the time adjustment used internally}
begin
 {}
 TIMEZONE_TIME_ADJUST:=AAdjust;
end;

{==============================================================================}

function GetCurrentTimezone:String;
{Get the name of the current Timezone}
var
 Timezone:PTimezoneEntry;
begin
 {}
 Result:='';

 {Get Timezone}
 Timezone:=TimezoneGetDefault;
 if Timezone = nil then Exit;

 {Get Name}
 Result:=TimezoneGetName(Timezone);
end;

{==============================================================================}

function SetCurrentTimezone(const AName:String):Boolean;
{Set the current Timezone by name}
var
 Timezone:PTimezoneEntry;
begin
 {}
 Result:=False;

 {Find Timezone}
 Timezone:=TimezoneFind(AName);
 if Timezone = nil then Exit;

 {Set Timezone}
 Result:=(TimezoneSetDefault(Timezone) = ERROR_SUCCESS);
end;

{==============================================================================}

function GetTimezoneActiveOffset:LongInt;
{Gets the Active Offset from the current Timezone}
var
 ResultCode:LongWord;
 TimeZoneInformation:TTimeZoneInformation;
begin
 {}
 Result:=0;

 FillChar(TimeZoneInformation,SizeOf(TTimeZoneInformation),0);

 ResultCode:=GetTimeZoneInformation(TimeZoneInformation);
 if ResultCode <> TIME_ZONE_ID_INVALID then
  begin
   if ResultCode = TIME_ZONE_ID_DAYLIGHT then
    begin
     {Get Daylight Offset}
     Result:=TimeZoneInformation.Bias + TimeZoneInformation.DaylightBias;
    end
   else
    begin
     {Get Standard Offset}
     Result:=TimeZoneInformation.Bias;
    end;
  end;
end;

{==============================================================================}

function GetTimezoneStandardOffset:LongInt;
{Gets the Standard Offset from the current Timezone}
var
 TimeZoneInformation:TTimeZoneInformation;
begin
 {}
 Result:=0;

 FillChar(TimeZoneInformation,SizeOf(TTimeZoneInformation),0);

 if GetTimeZoneInformation(TimeZoneInformation) <> TIME_ZONE_ID_INVALID then
  begin
   Result:=TimeZoneInformation.Bias;
  end;
end;

{==============================================================================}

procedure SetTimezoneStandardOffset(AOffset:LongInt);
{Sets the Standard Offset for the current Timezone}
var
 TimeZoneInformation:TTimeZoneInformation;
begin
 {}
 FillChar(TimeZoneInformation,SizeOf(TTimeZoneInformation),0);

 if GetTimeZoneInformation(TimeZoneInformation) <> TIME_ZONE_ID_INVALID then
  begin
   TimeZoneInformation.Bias:=AOffset;

   SetTimeZoneInformation(TimeZoneInformation);
  end;
end;

{==============================================================================}

function GetTimezoneDaylightOffset:LongInt;
{Gets the Daylight Offset from the current Timezone}
var
 TimeZoneInformation:TTimeZoneInformation;
begin
 {}
 Result:=0;

 FillChar(TimeZoneInformation,SizeOf(TTimeZoneInformation),0);

 if GetTimeZoneInformation(TimeZoneInformation) <> TIME_ZONE_ID_INVALID then
  begin
   Result:=TimeZoneInformation.DaylightBias;
  end;
end;

{==============================================================================}

procedure SetTimezoneDaylightOffset(AOffset:LongInt);
{Sets the Daylight Offset for the current Timezone}
var
 TimeZoneInformation:TTimeZoneInformation;
begin
 {}
 FillChar(TimeZoneInformation,SizeOf(TTimeZoneInformation),0);

 if GetTimeZoneInformation(TimeZoneInformation) <> TIME_ZONE_ID_INVALID then
  begin
   TimeZoneInformation.DaylightBias:=AOffset;

   SetTimeZoneInformation(TimeZoneInformation);
  end;
end;

{==============================================================================}

function GetTimezoneStandardStart:String;
{Get the description of the standard time start for the current Timezone}
var
 Timezone:PTimezoneEntry;
begin
 {}
 Result:='';

 {Get Timezone}
 Timezone:=TimezoneGetDefault;
 if Timezone = nil then Exit;

 {Get Standard Start}
 Result:=TimezoneStartToDescription(TimezoneGetStandardStart(Timezone));
end;

{==============================================================================}

function GetTimezoneDaylightStart:String;
{Get the description of the daylight time start for the current Timezone}
var
 Timezone:PTimezoneEntry;
begin
 {}
 Result:='';

 {Get Timezone}
 Timezone:=TimezoneGetDefault;
 if Timezone = nil then Exit;

 {Get Daylight Start}
 Result:=TimezoneStartToDescription(TimezoneGetDaylightStart(Timezone));
end;

{==============================================================================}

function GetTimezoneStandardDate:TDateTime;
{Get the next date of the standard time start for the current Timezone}
var
 Timezone:PTimezoneEntry;
begin
 {}
 Result:=0;

 {Get Timezone}
 Timezone:=TimezoneGetDefault;
 if Timezone = nil then Exit;

 {Get Standard Date}
 Result:=TimezoneGetStandardDate(Timezone,True);
end;

{==============================================================================}

function GetTimezoneDaylightDate:TDateTime;
{Get the next date of the daylight time start for the current Timezone}
var
 Timezone:PTimezoneEntry;
begin
 {}
 Result:=0;

 {Get Timezone}
 Timezone:=TimezoneGetDefault;
 if Timezone = nil then Exit;

 {Get Daylight Date}
 Result:=TimezoneGetDaylightDate(Timezone,True);
end;

{==============================================================================}

function FileTimeToDateTime(const AFileTime:TFileTime):TDateTime;
{Convert a FileTime value to a DateTime value}
{Note: FileTime is assumed to be UTC / DateTime is returned as Local}
{Note: If FileTime is less than 30/12/1899 then Result will be zero}
var
 LocalFileTime:TFileTime;
begin
 {}
 Result:=0;

 if FileTimeToLocalFileTime(AFileTime,LocalFileTime) then
  begin
   if Int64(LocalFileTime) < TIME_TICKS_TO_1899 then Exit;

   Result:=((Int64(LocalFileTime) - TIME_TICKS_TO_1899) div TIME_TICKS_PER_DAY) + (((Int64(LocalFileTime) - TIME_TICKS_TO_1899) mod TIME_TICKS_PER_DAY) / TIME_TICKS_PER_DAY);
  end;
end;

{==============================================================================}

function DateTimeToFileTime(ADateTime:TDateTime):TFileTime;
{Convert a DateTime value to a FileTime value}
{Note: DateTime is assumed to be Local / FileTime is returned as UTC}
var
 LocalFileTime:TFileTime;
begin
 {}
 {Int64(LocalFileTime):=((Trunc(ADateTime) * TIME_TICKS_PER_DAY) + TIME_TICKS_TO_1899) + (Round(Frac(ADateTime) * TIME_TICKS_PER_DAY));}

 {Modified to restrict resolution to 1 millisecond}
 Int64(LocalFileTime):=((Trunc(ADateTime) * TIME_TICKS_PER_DAY) + TIME_TICKS_TO_1899) + ((Round(Frac(ADateTime) * PASCAL_TIME_MILLISECONDS_PER_DAY) * TIME_TICKS_PER_MILLISECOND));

 LocalFileTimeToFileTime(LocalFileTime,Result);
end;

{==============================================================================}

function LocalFileTimeToDateTime(const AFileTime:TFileTime):TDateTime;
{Convert a FileTime value to a DateTime value}
{Note: FileTime is assumed to be Local / DateTime is returned as Local}
{Note: If FileTime is less than 30/12/1899 then Result will be zero}
begin
 {}
 Result:=0;

 if Int64(AFileTime) < TIME_TICKS_TO_1899 then Exit;

 Result:=((Int64(AFileTime) - TIME_TICKS_TO_1899) div TIME_TICKS_PER_DAY) + (((Int64(AFileTime) - TIME_TICKS_TO_1899) mod TIME_TICKS_PER_DAY) / TIME_TICKS_PER_DAY);
end;

{==============================================================================}

function DateTimeToLocalFileTime(ADateTime:TDateTime):TFileTime;
{Convert a DateTime value to a FileTime value}
{Note: DateTime is assumed to be Local / FileTime is returned as Local}
begin
 {}
 {Int64(Result):=((Trunc(ADateTime) * TIME_TICKS_PER_DAY) + TIME_TICKS_TO_1899) + (Round(Frac(ADateTime) * TIME_TICKS_PER_DAY));}

 {Modified to restrict resolution to 1 millisecond}
 Int64(Result):=((Trunc(ADateTime) * TIME_TICKS_PER_DAY) + TIME_TICKS_TO_1899) + ((Round(Frac(ADateTime) * PASCAL_TIME_MILLISECONDS_PER_DAY) * TIME_TICKS_PER_MILLISECOND));
end;

{==============================================================================}

function SystemFileTimeToDateTime(const AFileTime:TFileTime):TDateTime;
{Convert a FileTime value to a DateTime value}
{Note: FileTime is assumed to be UTC / DateTime is returned as UTC}
{Note: If FileTime is less than 30/12/1899 then Result will be zero}
{Note: Same as LocalFileTimeToDateTime but renamed for clarity}
begin
 {}
 Result:=0;

 if Int64(AFileTime) < TIME_TICKS_TO_1899 then Exit;

 Result:=((Int64(AFileTime) - TIME_TICKS_TO_1899) div TIME_TICKS_PER_DAY) + (((Int64(AFileTime) - TIME_TICKS_TO_1899) mod TIME_TICKS_PER_DAY) / TIME_TICKS_PER_DAY);
end;

{==============================================================================}

function DateTimeToSystemFileTime(ADateTime:TDateTime):TFileTime;
{Convert a DateTime value to a FileTime value}
{Note: DateTime is assumed to be UTC / FileTime is returned as UTC}
{Note: Same as DateTimeToLocalFileTime but renamed for clarity}
begin
 {}
 {Int64(Result):=((Trunc(ADateTime) * TIME_TICKS_PER_DAY) + TIME_TICKS_TO_1899) + (Round(Frac(ADateTime) * TIME_TICKS_PER_DAY));}

 {Modified to restrict resolution to 1 millisecond}
 Int64(Result):=((Trunc(ADateTime) * TIME_TICKS_PER_DAY) + TIME_TICKS_TO_1899) + ((Round(Frac(ADateTime) * PASCAL_TIME_MILLISECONDS_PER_DAY) * TIME_TICKS_PER_MILLISECOND));
end;

{==============================================================================}

function FileTimeToUnixTime(const AFileTime:TFileTime):TUnixTime;
{Convert a FileTime value to a Unix/Linux time value}
{Note: FileTime is assumed to be Local / UnixTime is returned as Local}
{Note: If FileTime is less than 1/1/1970 then Result will be zero}
begin
 {}
 Result:=0;

 if Int64(AFileTime) < TIME_TICKS_TO_1970 then Exit;

 Result:=(Int64(AFileTime) - TIME_TICKS_TO_1970) div TIME_TICKS_PER_SECOND;
end;

{==============================================================================}

function UnixTimeToFileTime(AUnixTime:TUnixTime):TFileTime;
{Convert a Unix/Linux time value to a FileTime value}
{Note: UnixTime is assumed to be Local / FileTime is returned as Local}
begin
 {}
 Int64(Result):=AUnixTime; {Avoid 32 bit overflow}
 Int64(Result):=(Int64(Result) * TIME_TICKS_PER_SECOND) + TIME_TICKS_TO_1970;
end;

{==============================================================================}

function UnixTimeToDateTime(AUnixTime:TUnixTime):TDateTime;
{Convert a Unix/Linux time value to a DateTime value}
{Note: UnixTime is assumed to be Local / DateTime is returned as Local}
begin
 {}
 Result:=((AUnixTime div UNIX_TIME_SECONDS_PER_DAY) + UNIX_TIME_DAYS_TO_1970) + ((AUnixTime mod UNIX_TIME_SECONDS_PER_DAY) / UNIX_TIME_SECONDS_PER_DAY);
end;

{==============================================================================}

function DateTimeToUnixTime(ADateTime:TDateTime):TUnixTime;
{Convert a DateTime value to a Unix/Linux time value}
{Note: DateTime is assumed to be Local / UnixTime is returned as Local}
{Note: If DateTime is less than 1/1/1970 then Result will be zero}
begin
 {}
 Result:=0;

 if Trunc(ADateTime) < UNIX_TIME_DAYS_TO_1970 then Exit;

 Result:=(Trunc(ADateTime - UNIX_TIME_DAYS_TO_1970) * UNIX_TIME_SECONDS_PER_DAY) + (Round(Frac(ADateTime) * UNIX_TIME_SECONDS_PER_DAY));
end;

{==============================================================================}

function FileTimeToFileDate(const AFileTime:TFileTime):Integer;
{Convert a FileTime value to a DOS date value}
{Note: FileTime is assumed to be UTC / FileDate is returned as Local}
{Note: If FileTime is less than 1/1/1980 then Result will be zero}
var
 DateTime:TDateTime;
begin
 {}
 Result:=0;

 {Check FileTime}
 if Int64(AFileTime) < TIME_TICKS_TO_1980 then Exit; {Note: Could be wrong if within TIMEZONE_TIME_OFFSET of TIME_TICKS_TO_1980}

 {Convert to DateTime}
 DateTime:=FileTimeToDateTime(AFileTime); {Converted to Local}

 {Convert to FileDate}
 Result:=DateTimeToFileDate(DateTime);
end;

{==============================================================================}

function FileDateToFileTime(AFileDate:Integer):TFileTime;
{Convert a DOS date value to a FileTime value}
{Note: FileDate is assumed to be Local / FileTime is returned as UTC}
{Note: If FileDate is less than 1/1/1980 then Result will be zero}
var
 DateTime:TDateTime;
begin
 {}
 Int64(Result):=0;
 try
  {Check FileDate}
  if AFileDate < PASCAL_TIME_DOS_TIME_START then Exit;

  {Convert to DateTime}
  DateTime:=FileDateToDateTime(AFileDate);

  {Convert to FileTime}
  Result:=DateTimeToFileTime(DateTime); {Converted to UTC}
 except
  {FileDateToDateTime can raise Exceptions}
 end;
end;

{==============================================================================}

function FileTimeToAdjustedTime(const AFileTime:TFileTime):TFileTime;
{Convert a local FileTime value to a DST adjusted FileTime value}
{Note: TIMEZONE_TIME_ADJUST is the number of minutes difference from TIMEZONE_TIME_OFFSET}
var
 Offset:Int64;
begin
 {}
 Result:=AFileTime;

 if TIMEZONE_TIME_ADJUST = 0 then Exit;

 if Int64(Result) = 0 then Exit; {Account for zero time}

 Offset:=TIMEZONE_TIME_ADJUST;
 Offset:=Offset * TIME_TICKS_PER_MINUTE;

 Int64(Result):=Int64(AFileTime) - (Offset);
end;

{==============================================================================}

function AdjustedTimeToFileTime(const AFileTime:TFileTime):TFileTime;
{Convert a DST adjusted FileTime value to a local FileTime value}
{Note: TIMEZONE_TIME_ADJUST is the number of minutes difference from TIMEZONE_TIME_OFFSET}
var
 Offset:Int64;
begin
 {}
 Result:=AFileTime;

 if TIMEZONE_TIME_ADJUST = 0 then Exit;

 if Int64(Result) = 0 then Exit; {Account for zero time}

 Offset:=TIMEZONE_TIME_ADJUST;
 Offset:=Offset * TIME_TICKS_PER_MINUTE;

 Int64(Result):=Int64(AFileTime) + (Offset);
end;

{==============================================================================}

function RoundFileTime(const AFileTime:TFileTime):TFileTime;
{Round FileTime to nearest 2 seconds for compatibility with FileDate}
var
 Remain:Int64;
begin
 {}
 Result:=AFileTime;
 Remain:=Int64(Result) mod (2 * TIME_TICKS_PER_SECOND);
 if Remain = 0 then
  begin
   {No Round}
   Exit;
  end
 else if Remain > TIME_TICKS_PER_SECOND then
  begin
   {Round Up}
   Int64(Result):=Int64(Result) + ((2 * TIME_TICKS_PER_SECOND) - Remain);
  end
 else
  begin
   {Round Down}
   Int64(Result):=Int64(Result) - Remain;
  end;
end;

{==============================================================================}

function ConvertFileTime(const AFileTime:TFileTime;AOffset:Integer;ALocal:Boolean):TFileTime;
{Convert a FileTime value to a specified offset (Local or UTC)}
{Note: Offset is the number of minutes to adjust in conversion}
{      Local indicates whether the source time is Local or UTC}
var
 Offset:Int64;
begin
 {}
 Result:=AFileTime;

 if AOffset = 0 then Exit;
 if Int64(Result) = 0 then Exit; {Account for zero time}

 Offset:=AOffset;
 Offset:=Offset * TIME_TICKS_PER_MINUTE;

 if ALocal then
  Int64(Result):=Int64(AFileTime) + (Offset)
 else
  Int64(Result):=Int64(AFileTime) - (Offset);
end;

{==============================================================================}

function ConvertDateTime(ADateTime:TDateTime;AOffset:Integer;ALocal:Boolean):TDateTime;
{Convert a DateTime value to a specified offset (Local or UTC)}
{Note: Offset is the number of minutes to adjust in conversion}
{      Local indicates whether the source time is Local or UTC}
var
 Offset:TDateTime;
begin
 {}
 Result:=ADateTime;

 if AOffset = 0 then Exit;
 if Result = 0 then Exit; {Account for zero time}

 Offset:=AOffset;
 Offset:=Offset * PASCAL_MINUTE_OFFSET;

 if ALocal then
  Result:=ADateTime + Offset
 else
  Result:=ADateTime - Offset;
end;

{==============================================================================}
{==============================================================================}
{Drive Functions (Compatibility)}
function GetDiskType(const lpRootPathName:LPCSTR):UINT; inline;
begin
 {}
 Result:=GetDriveTypeA(lpRootPathName);
end;

{==============================================================================}

function GetDriveTypeA(const lpRootPathName:LPCSTR):UINT;
begin
 {}
 if Assigned(UltiboGetDriveTypeAHandler) then
  begin
   Result:=UltiboGetDriveTypeAHandler(String(lpRootPathName));
  end
 else
  begin
   Result:=DRIVE_UNKNOWN;
  end;
end;

{==============================================================================}

function GetDriveTypeW(const lpRootPathName:LPCWSTR):UINT;
var
 RootPath:String;
begin
 {}
 if Assigned(UltiboGetDriveTypeAHandler) then
  begin
   RootPath:=WideCharToString(lpRootPathName);

   Result:=UltiboGetDriveTypeAHandler(RootPath);
  end
 else
  begin
   Result:=DRIVE_UNKNOWN;
  end;
end;

{==============================================================================}

function GetLogicalDrives:DWORD;
begin
 {}
 if Assigned(UltiboGetLogicalDrivesHandler) then
  begin
   Result:=UltiboGetLogicalDrivesHandler;
  end
 else
  begin
   Result:=0;
  end;
end;

{==============================================================================}

function GetLogicalDriveStrings(nBufferLength:DWORD;lpBuffer:LPSTR):DWORD; inline;
begin
 {}
 Result:=GetLogicalDriveStringsA(nBufferLength,lpBuffer)
end;

{==============================================================================}

function GetLogicalDriveStringsA(nBufferLength:DWORD;lpBuffer:LPSTR):DWORD;
var
 Value:String;
begin
 {}
 if Assigned(UltiboGetLogicalDriveStringsAHandler) then
  begin
   Result:=0;
   if lpBuffer = nil then Exit;

   Value:=UltiboGetLogicalDriveStringsAHandler;
   StrLCopy(lpBuffer,PChar(Value),nBufferLength);

   Result:=Length(Value);
  end
 else
  begin
   Result:=0;
  end;
end;

{==============================================================================}

function GetLogicalDriveStringsW(nBufferLength:DWORD;lpBuffer:LPWSTR):DWORD;
var
 Value:String;
begin
 {}
 if Assigned(UltiboGetLogicalDriveStringsAHandler) then
  begin
   Result:=0;
   if lpBuffer = nil then Exit;
   if nBufferLength = 0 then Exit;

   Value:=UltiboGetLogicalDriveStringsAHandler;
   if StringToWideChar(Value,lpBuffer,nBufferLength shl 1) then {Buffer length in chars, Multiply by SizeOf(WideChar)} //To Do //Use default StringToWideChar from System ? //Watch for difference in size parameter (Bytes/Chars)
    begin
     Result:=Length(Value);
    end;
  end
 else
  begin
   Result:=0;
  end;
end;

{==============================================================================}

function DefineDosDevice(dwFlags:DWORD;const lpDeviceName,lpTargetPath:LPCSTR):BOOL; inline;
begin
 {}
 Result:=DefineDosDeviceA(dwFlags,lpDeviceName,lpTargetPath);
end;

{==============================================================================}

function DefineDosDeviceA(dwFlags:DWORD;const lpDeviceName,lpTargetPath:LPCSTR):BOOL;
begin
 {}
 if Assigned(UltiboDefineDosDeviceAHandler) then
  begin
   Result:=UltiboDefineDosDeviceAHandler(String(lpDeviceName),String(lpTargetPath),dwFlags);
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function DefineDosDeviceW(dwFlags:DWORD;const lpDeviceName,lpTargetPath:LPCWSTR):BOOL;
var
 DeviceName:String;
 TargetPath:String;
begin
 {}
 if Assigned(UltiboDefineDosDeviceAHandler) then
  begin
   DeviceName:=WideCharToString(lpDeviceName);
   TargetPath:=WideCharToString(lpTargetPath);

   Result:=UltiboDefineDosDeviceAHandler(DeviceName,TargetPath,dwFlags);
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function QueryDosDevice(const lpDeviceName:LPCSTR;lpTargetPath:LPSTR;ucchMax:DWORD):DWORD; inline;
begin
 {}
 Result:=QueryDosDeviceA(lpDeviceName,lpTargetPath,ucchMax);
end;

{==============================================================================}

function QueryDosDeviceA(const lpDeviceName:LPCSTR;lpTargetPath:LPSTR;ucchMax:DWORD):DWORD;
var
 Value:String;
begin
 {}
 if Assigned(UltiboQueryDosDeviceAHandler) then
  begin
   Result:=0;
   if lpTargetPath = nil then Exit;

   Value:=UltiboQueryDosDeviceAHandler(String(lpDeviceName));
   StrLCopy(lpTargetPath,PChar(Value),ucchMax);

   Result:=Length(Value);
  end
 else
  begin
   Result:=0;
  end;
end;

{==============================================================================}

function QueryDosDeviceW(const lpDeviceName:LPCWSTR;lpTargetPath:LPWSTR;ucchMax:DWORD):DWORD;
var
 Value:String;
 DeviceName:String;
begin
 {}
 if Assigned(UltiboQueryDosDeviceAHandler) then
  begin
   Result:=0;
   if lpTargetPath = nil then Exit;

   DeviceName:=WideCharToString(lpDeviceName);

   Value:=UltiboQueryDosDeviceAHandler(DeviceName);
   if StringToWideChar(Value,lpTargetPath,ucchMax shl 1) then {Buffer length in chars, Multiply by SizeOf(WideChar)} //To Do //Use default StringToWideChar from System ? //Watch for difference in size parameter (Bytes/Chars)
    begin
     Result:=Length(Value);
    end;
  end
 else
  begin
   Result:=0;
  end;
end;

{==============================================================================}

function SetVolumeLabel(const lpRootPathName,lpVolumeName:LPCSTR):BOOL; inline;
begin
 {}
 Result:=SetVolumeLabelA(lpRootPathName,lpVolumeName);
end;

{==============================================================================}

function SetVolumeLabelA(const lpRootPathName,lpVolumeName:LPCSTR):BOOL;
begin
 {}
 if Assigned(UltiboSetVolumeLabelAHandler) then
  begin
   Result:=UltiboSetVolumeLabelAHandler(String(lpRootPathName),String(lpVolumeName));
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function SetVolumeLabelW(const lpRootPathName,lpVolumeName:LPCWSTR):BOOL;
var
 RootPath:String;
 VolumeName:String;
begin
 {}
 if Assigned(UltiboSetVolumeLabelAHandler) then
  begin
   RootPath:=WideCharToString(lpRootPathName);
   VolumeName:=WideCharToString(lpVolumeName);

   Result:=UltiboSetVolumeLabelAHandler(RootPath,VolumeName);
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function GetVolumeInformation(const lpRootPathName:LPCSTR;lpVolumeNameBuffer:LPSTR;nVolumeNameSize:DWORD;lpVolumeSerialNumber:LPDWORD;var lpMaximumComponentLength,lpFileSystemFlags:DWORD;lpFileSystemNameBuffer:LPSTR;nFileSystemNameSize:DWORD):BOOL; inline;
begin
 {}
 Result:=GetVolumeInformationA(lpRootPathName,lpVolumeNameBuffer,nVolumeNameSize,lpVolumeSerialNumber,lpMaximumComponentLength,lpFileSystemFlags,lpFileSystemNameBuffer,nFileSystemNameSize);
end;

{==============================================================================}

function GetVolumeInformationA(const lpRootPathName:LPCSTR;lpVolumeNameBuffer:LPSTR;nVolumeNameSize:DWORD;lpVolumeSerialNumber:LPDWORD;var lpMaximumComponentLength,lpFileSystemFlags:DWORD;lpFileSystemNameBuffer:LPSTR;nFileSystemNameSize:DWORD):BOOL;
var
 VolumeName:String;
 SystemName:String;
begin
 {}
 if Assigned(UltiboGetVolumeInformationAHandler) then
  begin
   Result:=False;
   if lpVolumeNameBuffer = nil then Exit;
   if lpFileSystemNameBuffer = nil then Exit;

   VolumeName:='';
   SystemName:='';

   Result:=UltiboGetVolumeInformationAHandler(String(lpRootPathName),VolumeName,lpVolumeSerialNumber^,lpMaximumComponentLength,lpFileSystemFlags,SystemName);
   if Result then
    begin
     StrLCopy(lpVolumeNameBuffer,PChar(VolumeName),nVolumeNameSize);
     StrLCopy(lpFileSystemNameBuffer,PChar(SystemName),nFileSystemNameSize);
    end;
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function GetVolumeInformationW(const lpRootPathName:LPCWSTR;lpVolumeNameBuffer:LPWSTR;nVolumeNameSize:DWORD;lpVolumeSerialNumber:LPDWORD;var lpMaximumComponentLength,lpFileSystemFlags:DWORD;lpFileSystemNameBuffer:LPWSTR;nFileSystemNameSize:DWORD):BOOL;
var
 RootPath:String;
 VolumeName:String;
 SystemName:String;
begin
 {}
 if Assigned(UltiboGetVolumeInformationAHandler) then
  begin
   Result:=False;
   if lpVolumeNameBuffer = nil then Exit;
   if lpFileSystemNameBuffer = nil then Exit;

   RootPath:=WideCharToString(lpRootPathName);
   VolumeName:='';
   SystemName:='';

   Result:=UltiboGetVolumeInformationAHandler(RootPath,VolumeName,lpVolumeSerialNumber^,lpMaximumComponentLength,lpFileSystemFlags,SystemName);
   if Result then
    begin
     StringToWideChar(VolumeName,lpVolumeNameBuffer,nVolumeNameSize shl 1); {Buffer length in chars, Multiply by SizeOf(WideChar)} //To Do //Use default StringToWideChar from System ? //Watch for difference in size parameter (Bytes/Chars)
     StringToWideChar(SystemName,lpFileSystemNameBuffer,nFileSystemNameSize shl 1); {Buffer length in chars, Multiply by SizeOf(WideChar)} //To Do //Use default StringToWideChar from System ? //Watch for difference in size parameter (Bytes/Chars)
    end;
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function GetDiskFreeSpace(const lpRootPathName:LPCSTR;var lpSectorsPerCluster,lpBytesPerSector,lpNumberOfFreeClusters,lpTotalNumberOfClusters:DWORD):BOOL; inline;
begin
 {}
 Result:=GetDiskFreeSpaceA(lpRootPathName,lpSectorsPerCluster,lpBytesPerSector,lpNumberOfFreeClusters,lpTotalNumberOfClusters);
end;

{==============================================================================}

function GetDiskFreeSpaceA(const lpRootPathName:LPCSTR;var lpSectorsPerCluster,lpBytesPerSector,lpNumberOfFreeClusters,lpTotalNumberOfClusters:DWORD):BOOL;
begin
 {}
 if Assigned(UltiboGetDiskFreeSpaceAHandler) then
  begin
   Result:=UltiboGetDiskFreeSpaceAHandler(String(lpRootPathName),lpSectorsPerCluster,lpBytesPerSector,lpNumberOfFreeClusters,lpTotalNumberOfClusters);
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function GetDiskFreeSpaceW(const lpRootPathName:LPCWSTR;var lpSectorsPerCluster,lpBytesPerSector,lpNumberOfFreeClusters,lpTotalNumberOfClusters:DWORD):BOOL;
var
 RootPath:String;
begin
 {}
 if Assigned(UltiboGetDiskFreeSpaceAHandler) then
  begin
   RootPath:=WideCharToString(lpRootPathName);

   Result:=UltiboGetDiskFreeSpaceAHandler(RootPath,lpSectorsPerCluster,lpBytesPerSector,lpNumberOfFreeClusters,lpTotalNumberOfClusters);
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function GetDiskFreeSpaceEx(const lpDirectoryName:LPCSTR;var lpFreeBytesAvailableToCaller,lpTotalNumberOfBytes:ULARGE_INTEGER;lpTotalNumberOfFreeBytes:PULARGE_INTEGER):BOOL; inline;
begin
 {}
 Result:=GetDiskFreeSpaceExA(lpDirectoryName,lpFreeBytesAvailableToCaller,lpTotalNumberOfBytes,lpTotalNumberOfFreeBytes);
end;

{==============================================================================}

function GetDiskFreeSpaceExA(const lpDirectoryName:LPCSTR;var lpFreeBytesAvailableToCaller,lpTotalNumberOfBytes:ULARGE_INTEGER;lpTotalNumberOfFreeBytes:PULARGE_INTEGER):BOOL;
var
 TotalNumberOfFreeBytes:QWord;
begin
 {}
 if Assigned(UltiboGetDiskFreeSpaceExAHandler) then
  begin
   Result:=UltiboGetDiskFreeSpaceExAHandler(String(lpDirectoryName),QWord(lpFreeBytesAvailableToCaller),QWord(lpTotalNumberOfBytes),TotalNumberOfFreeBytes);
   if Result and (lpTotalNumberOfFreeBytes <> nil) then
    begin
     PQWord(lpTotalNumberOfFreeBytes)^:=TotalNumberOfFreeBytes;
    end;
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function GetDiskFreeSpaceExW(const lpDirectoryName:LPCWSTR;var lpFreeBytesAvailableToCaller,lpTotalNumberOfBytes:ULARGE_INTEGER;lpTotalNumberOfFreeBytes:PULARGE_INTEGER):BOOL;
var
 DirectoryName:String;
 TotalNumberOfFreeBytes:QWord;
begin
 {}
 if Assigned(UltiboGetDiskFreeSpaceExAHandler) then
  begin
   DirectoryName:=WideCharToString(lpDirectoryName);

   Result:=UltiboGetDiskFreeSpaceExAHandler(DirectoryName,QWord(lpFreeBytesAvailableToCaller),QWord(lpTotalNumberOfBytes),TotalNumberOfFreeBytes);
   if Result and (lpTotalNumberOfFreeBytes <> nil) then
    begin
     PQWord(lpTotalNumberOfFreeBytes)^:=TotalNumberOfFreeBytes;
    end;
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}
{==============================================================================}
{Drive Functions (Ultibo)}
function GetPathDrive(const APath:String):Byte;
begin
 {}
 Result:=DEFAULT_DRIVE;

 if Length(APath) > 1 then
  begin
   {Check for Drive Letter}
   if APath[2] = ':' then
    begin
     if APath[1] in ['A'..'Z','a'..'z'] then
      begin
       Result:=(Ord(UpCase(APath[1])) - Ord('A')) + 1;
       Exit;
      end;
    end;

   {Check for Unc Path}
   if (APath[1] = '\') and (APath[2] = '\') then
    begin
     Result:=NON_DRIVE;
     Exit;
    end;

   {Default to Current Directory}
   Result:=DEFAULT_DRIVE;
  end;
end;

{==============================================================================}

function GetDriveType(ADrive:Byte):TDriveType;
begin
 {}
 if Assigned(UltiboGetDriveTypeHandler) then
  begin
   Result:=UltiboGetDriveTypeHandler(ADrive);
  end
 else
  begin
   Result:=dtINVALID;
  end;
end;

{==============================================================================}

function GetDriveData(ADrive:Byte):TDriveData;
begin
 {}
 if Assigned(UltiboGetDriveDataHandler) then
  begin
   Result:=UltiboGetDriveDataHandler(ADrive);
  end
 else
  begin
   FillChar(Result,SizeOf(TDriveData),0);
  end;
end;

{==============================================================================}

function GetDriveAttr(ADrive:Byte):LongWord;
begin
 {}
 if Assigned(UltiboGetDriveAttrHandler) then
  begin
   Result:=UltiboGetDriveAttrHandler(ADrive);
  end
 else
  begin
   Result:=0;
  end;
end;

{==============================================================================}

function GetDriveLabel(ADrive:Byte):String;
begin
 {}
 if Assigned(UltiboGetDriveLabelHandler) then
  begin
   Result:=UltiboGetDriveLabelHandler(ADrive);
  end
 else
  begin
   Result:='';
  end;
end;

{==============================================================================}

function SetDriveLabel(ADrive:Byte;const ALabel:String):Boolean;
begin
 {}
 if Assigned(UltiboSetDriveLabelHandler) then
  begin
   Result:=UltiboSetDriveLabelHandler(ADrive,ALabel);
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function GetDriveSerial(ADrive:Byte):LongWord;
begin
 {}
 if Assigned(UltiboGetDriveSerialHandler) then
  begin
   Result:=UltiboGetDriveSerialHandler(ADrive);
  end
 else
  begin
   Result:=0;
  end;
end;

{==============================================================================}

function SetDriveSerial(ADrive:Byte;ASerial:LongWord):Boolean;
begin
 {}
 if Assigned(UltiboSetDriveSerialHandler) then
  begin
   Result:=UltiboSetDriveSerialHandler(ADrive,ASerial);
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function IsDriveValid(ADrive:Byte):Boolean;
begin
 {}
 if Assigned(UltiboIsDriveValidHandler) then
  begin
   Result:=UltiboIsDriveValidHandler(ADrive);
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function GetValidDrives:LongWord;
begin
 {}
 if Assigned(UltiboGetValidDrivesHandler) then
  begin
   Result:=UltiboGetValidDrivesHandler;
  end
 else
  begin
   Result:=0;
  end;
end;

{==============================================================================}

function GetValidDriveNames:String;
begin
 {}
 if Assigned(UltiboGetValidDriveNamesHandler) then
  begin
   Result:=UltiboGetValidDriveNamesHandler;
  end
 else
  begin
   Result:='';
  end;
end;

{==============================================================================}

function GetDriveFreeSpace(ADrive:Byte):LongWord;
begin
 {}
 if Assigned(UltiboGetDriveFreeSpaceHandler) then
  begin
   Result:=UltiboGetDriveFreeSpaceHandler(ADrive);
  end
 else
  begin
   Result:=0;
  end;
end;

{==============================================================================}

function GetDriveFreeSpaceEx(ADrive:Byte):Int64;
begin
 {}
 if Assigned(UltiboGetDriveFreeSpaceExHandler) then
  begin
   Result:=UltiboGetDriveFreeSpaceExHandler(ADrive);
  end
 else
  begin
   Result:=0;
  end;
end;

{==============================================================================}

function GetDriveTotalSpace(ADrive:Byte):LongWord;
begin
 {}
 if Assigned(UltiboGetDriveTotalSpaceHandler) then
  begin
   Result:=UltiboGetDriveTotalSpaceHandler(ADrive);
  end
 else
  begin
   Result:=0;
  end;
end;

{==============================================================================}

function GetDriveTotalSpaceEx(ADrive:Byte):Int64;
begin
 {}
 if Assigned(UltiboGetDriveTotalSpaceExHandler) then
  begin
   Result:=UltiboGetDriveTotalSpaceExHandler(ADrive);
  end
 else
  begin
   Result:=0;
  end;
end;

{==============================================================================}

function GetDriveInformation(const APath:String;var AClusterSize:LongWord;var ATotalClusterCount,AFreeClusterCount:Int64):Boolean;
begin
 {}
 if Assigned(UltiboGetDriveInformationHandler) then
  begin
   Result:=UltiboGetDriveInformationHandler(APath,AClusterSize,ATotalClusterCount,AFreeClusterCount);
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function GetCurrentDrive:Byte;
begin
 {}
 if Assigned(UltiboGetCurrentDriveHandler) then
  begin
   Result:=UltiboGetCurrentDriveHandler;
  end
 else
  begin
   Result:=0;
  end;
end;

{==============================================================================}

function SetCurrentDrive(const ADrive:String):Boolean;
begin
 {}
 if Assigned(UltiboSetCurrentDriveHandler) then
  begin
   Result:=UltiboSetCurrentDriveHandler(ADrive);
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}
{==============================================================================}
{File Functions (Compatibility)}
function AreFileApisANSI:BOOL;
begin
 {}
 if Assigned(UltiboAreFileApisANSIHandler) then
  begin
   Result:=UltiboAreFileApisANSIHandler;
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

procedure SetFileApisToOEM;
begin
 {}
 if Assigned(UltiboSetFileApisToOEMHandler) then
  begin
   UltiboSetFileApisToOEMHandler;
  end;
end;

{==============================================================================}

procedure SetFileApisToANSI;
begin
 {}
 if Assigned(UltiboSetFileApisToANSIHandler) then
  begin
   UltiboSetFileApisToANSIHandler;
  end;
end;

{==============================================================================}

function CreateFile(const lpFileName:LPCSTR;dwDesiredAccess,dwShareMode:DWORD;lpSecurityAttributes:LPSECURITY_ATTRIBUTES;dwCreationDisposition:DWORD;dwFlagsAndAttributes:DWORD;hTemplateFile:HANDLE):HANDLE; inline;
begin
 {}
 Result:=CreateFileA(lpFileName,dwDesiredAccess,dwShareMode,lpSecurityAttributes,dwCreationDisposition,dwFlagsAndAttributes,hTemplateFile);
end;

{==============================================================================}

function CreateFileA(const lpFileName:LPCSTR;dwDesiredAccess,dwShareMode:DWORD;lpSecurityAttributes:LPSECURITY_ATTRIBUTES;dwCreationDisposition:DWORD;dwFlagsAndAttributes:DWORD;hTemplateFile:HANDLE):HANDLE;
{Note: lpSecurityAttributes and hTemplateFile are currently ignored by Ultibo}
begin
 {}
 if Assigned(UltiboCreateFileAHandler) then
  begin
   Result:=UltiboCreateFileAHandler(String(lpFileName),dwDesiredAccess,dwShareMode,dwCreationDisposition,dwFlagsAndAttributes);
  end
 else
  begin
   Result:=HANDLE(INVALID_HANDLE_VALUE);
  end;
end;

{==============================================================================}

function CreateFileW(const lpFileName:LPCWSTR;dwDesiredAccess,dwShareMode:DWORD;lpSecurityAttributes:LPSECURITY_ATTRIBUTES;dwCreationDisposition:DWORD;dwFlagsAndAttributes:DWORD;hTemplateFile:HANDLE):HANDLE;
{Note: lpSecurityAttributes and hTemplateFile are currently ignored by Ultibo}
var
 FileName:String;
begin
 {}
 if Assigned(UltiboCreateFileAHandler) then
  begin
   FileName:=WideCharToString(lpFileName);

   Result:=UltiboCreateFileAHandler(FileName,dwDesiredAccess,dwShareMode,dwCreationDisposition,dwFlagsAndAttributes);
  end
 else
  begin
   Result:=HANDLE(INVALID_HANDLE_VALUE);
  end;
end;

{==============================================================================}

function SetFileAttributes(const lpFileName:LPCSTR;dwFileAttributes:DWORD):BOOL; inline;
begin
 {}
 Result:=SetFileAttributesA(lpFileName,dwFileAttributes);
end;

{==============================================================================}

function SetFileAttributesA(const lpFileName:LPCSTR;dwFileAttributes:DWORD):BOOL;
begin
 {}
 if Assigned(UltiboSetFileAttributesAHandler) then
  begin
   Result:=UltiboSetFileAttributesAHandler(String(lpFileName),dwFileAttributes);
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function SetFileAttributesW(const lpFileName:LPCWSTR;dwFileAttributes:DWORD):BOOL;
var
 FileName:String;
begin
 {}
 if Assigned(UltiboSetFileAttributesAHandler) then
  begin
   FileName:=WideCharToString(lpFileName);

   Result:=UltiboSetFileAttributesAHandler(FileName,dwFileAttributes);
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function GetFileAttributes(const lpFileName:LPCSTR):DWORD; inline;
begin
 {}
 Result:=GetFileAttributesA(lpFileName);
end;

{==============================================================================}

function GetFileAttributesA(const lpFileName:LPCSTR):DWORD;
begin
 {}
 if Assigned(UltiboGetFileAttributesAHandler) then
  begin
   Result:=UltiboGetFileAttributesAHandler(String(lpFileName));
  end
 else
  begin
   Result:=INVALID_FILE_ATTRIBUTES;
  end;
end;

{==============================================================================}

function GetFileAttributesW(const lpFileName:LPCWSTR):DWORD;
var
 FileName:String;
begin
 {}
 if Assigned(UltiboGetFileAttributesAHandler) then
  begin
   FileName:=WideCharToString(lpFileName);

   Result:=UltiboGetFileAttributesAHandler(FileName);
  end
 else
  begin
   Result:=INVALID_FILE_ATTRIBUTES;
  end;
end;

{==============================================================================}

function DeleteFile(const lpFileName:LPCSTR):BOOL; inline;
begin
 {}
 Result:=DeleteFileA(lpFileName);
end;

{==============================================================================}

function DeleteFileA(const lpFileName:LPCSTR):BOOL;
begin
 {}
 if Assigned(UltiboDeleteFileAHandler) then
  begin
   Result:=UltiboDeleteFileAHandler(String(lpFileName));
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function DeleteFileW(const lpFileName:LPCWSTR):BOOL;
var
 FileName:String;
begin
 {}
 if Assigned(UltiboDeleteFileAHandler) then
  begin
   FileName:=WideCharToString(lpFileName);

   Result:=UltiboDeleteFileAHandler(FileName);
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function MoveFile(const lpExistingFileName,lpNewFileName:LPCSTR):BOOL; inline;
begin
 {}
 Result:=MoveFileA(lpExistingFileName,lpNewFileName);
end;

{==============================================================================}

function MoveFileA(const lpExistingFileName,lpNewFileName:LPCSTR):BOOL;
begin
 {}
 if Assigned(UltiboMoveFileAHandler) then
  begin
   Result:=UltiboMoveFileAHandler(String(lpExistingFileName),String(lpNewFileName));
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function MoveFileW(const lpExistingFileName,lpNewFileName:LPCWSTR):BOOL;
var
 NewFileName:String;
 ExistingFileName:String;
begin
 {}
 if Assigned(UltiboMoveFileAHandler) then
  begin
   NewFileName:=WideCharToString(lpNewFileName);
   ExistingFileName:=WideCharToString(lpExistingFileName);

   Result:=UltiboMoveFileAHandler(ExistingFileName,NewFileName);
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function FindFirstFile(const lpFileName:LPCSTR;var lpFindFileData:WIN32_FIND_DATAA):HANDLE; inline;
begin
 {}
 Result:=FindFirstFileA(lpFileName,lpFindFileData);
end;

{==============================================================================}

function FindFirstFileA(const lpFileName:LPCSTR;var lpFindFileData:WIN32_FIND_DATAA):HANDLE;
begin
 {}
 if Assigned(UltiboFindFirstFileAHandler) then
  begin
   Result:=UltiboFindFirstFileAHandler(String(lpFileName),lpFindFileData);
  end
 else
  begin
   Result:=HANDLE(INVALID_HANDLE_VALUE);
  end;
end;

{==============================================================================}

function FindFirstFileW(const lpFileName:LPCWSTR;var lpFindFileData:WIN32_FIND_DATAW):HANDLE;
var
 FileName:String;
 FindFileData:TWin32FindDataA;
begin
 {}
 if Assigned(UltiboFindFirstFileAHandler) then
  begin
   FileName:=WideCharToString(lpFileName);

   Result:=UltiboFindFirstFileAHandler(FileName,FindFileData);
   if Result <> HANDLE(INVALID_HANDLE_VALUE) then
    begin
     lpFindFileData.dwFileAttributes:=FindFileData.dwFileAttributes;
     lpFindFileData.ftCreationTime:=FindFileData.ftCreationTime;
     lpFindFileData.ftLastAccessTime:=FindFileData.ftLastAccessTime;
     lpFindFileData.ftLastWriteTime:=FindFileData.ftLastWriteTime;
     lpFindFileData.nFileSizeHigh:=FindFileData.nFileSizeHigh;
     lpFindFileData.nFileSizeLow:=FindFileData.nFileSizeLow;
     lpFindFileData.dwReserved0:=FindFileData.dwReserved0;
     lpFindFileData.dwReserved1:=FindFileData.dwReserved1;
     Unicode.MultiByteToWideChar(CP_ACP,0,PChar(@FindFileData.cFileName),MAX_PATH,PWideChar(@lpFindFileData.cFileName),MAX_PATH);
     Unicode.MultiByteToWideChar(CP_ACP,0,PChar(@FindFileData.cAlternateFileName),14,PWideChar(@lpFindFileData.cAlternateFileName),14);
    end;
  end
 else
  begin
   Result:=HANDLE(INVALID_HANDLE_VALUE);
  end;
end;

{==============================================================================}

function FindNextFile(hFindFile:HANDLE;var lpFindFileData:WIN32_FIND_DATAA):BOOL; inline;
begin
 {}
 Result:=FindNextFileA(hFindFile,lpFindFileData);
end;

{==============================================================================}

function FindNextFileA(hFindFile:HANDLE;var lpFindFileData:WIN32_FIND_DATAA):BOOL;
begin
 {}
 if Assigned(UltiboFindNextFileAHandler) then
  begin
   Result:=UltiboFindNextFileAHandler(hFindFile,lpFindFileData);
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function FindNextFileW(hFindFile:HANDLE;var lpFindFileData:WIN32_FIND_DATAW):BOOL;
var
 FindFileData:TWin32FindDataA;
begin
 {}
 if Assigned(UltiboFindNextFileAHandler) then
  begin
   Result:=UltiboFindNextFileAHandler(hFindFile,FindFileData);
   if Result then
    begin
     lpFindFileData.dwFileAttributes:=FindFileData.dwFileAttributes;
     lpFindFileData.ftCreationTime:=FindFileData.ftCreationTime;
     lpFindFileData.ftLastAccessTime:=FindFileData.ftLastAccessTime;
     lpFindFileData.ftLastWriteTime:=FindFileData.ftLastWriteTime;
     lpFindFileData.nFileSizeHigh:=FindFileData.nFileSizeHigh;
     lpFindFileData.nFileSizeLow:=FindFileData.nFileSizeLow;
     lpFindFileData.dwReserved0:=FindFileData.dwReserved0;
     lpFindFileData.dwReserved1:=FindFileData.dwReserved1;
     Unicode.MultiByteToWideChar(CP_ACP,0,PChar(@FindFileData.cFileName),MAX_PATH,PWideChar(@lpFindFileData.cFileName),MAX_PATH);
     Unicode.MultiByteToWideChar(CP_ACP,0,PChar(@FindFileData.cAlternateFileName),14,PWideChar(@lpFindFileData.cAlternateFileName),14);
    end;
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function FindCloseFile(hFindFile:HANDLE):BOOL;
begin
 {}
 if Assigned(UltiboFindCloseFileHandler) then
  begin
   Result:=UltiboFindCloseFileHandler(hFindFile);
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function GetFileSize(hFile:HANDLE;lpFileSizeHigh:LPDWORD):DWORD;
begin
 {}
 if Assigned(UltiboGetFileSizeHandler) then
  begin
   Result:=UltiboGetFileSizeHandler(hFile,lpFileSizeHigh^);
  end
 else
  begin
   Result:=INVALID_FILE_SIZE;
  end;
end;

{==============================================================================}

function GetFileSizeEx(hFile:HANDLE;var lpFileSize:LARGE_INTEGER):BOOL;
begin
 {}
 if Assigned(UltiboGetFileSizeExHandler) then
  begin
   lpFileSize.QuadPart:=UltiboGetFileSizeExHandler(hFile);
   if lpFileSize.QuadPart <> -1 then
    begin
     Result:=True;
    end;
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function GetFileTime(hFile:HANDLE;lpCreationTime,lpLastAccessTime,lpLastWriteTime:PFILETIME):BOOL;
begin
 {}
 if Assigned(UltiboGetFileTimeHandler) then
  begin
   Result:=UltiboGetFileTimeHandler(hFile,lpCreationTime,lpLastAccessTime,lpLastWriteTime);
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function SetFileTime(hFile:HANDLE;lpCreationTime,lpLastAccessTime,lpLastWriteTime:PFILETIME):BOOL;
begin
 {}
 if Assigned(UltiboSetFileTimeHandler) then
  begin
   Result:=UltiboSetFileTimeHandler(hFile,lpCreationTime,lpLastAccessTime,lpLastWriteTime);
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function ReadFile(hFile:HANDLE;lpBuffer:LPVOID;nNumberOfBytesToRead:DWORD;lpNumberOfBytesRead:LPDWORD;lpOverlapped:LPOVERLAPPED):BOOL;
var
 BytesRead:LongWord;
begin
 {}
 if Assigned(UltiboReadFileHandler) then
  begin
   Result:=UltiboReadFileHandler(hFile,lpBuffer^,nNumberOfBytesToRead,BytesRead);
   if Result and (lpNumberOfBytesRead <> nil) then
    begin
     lpNumberOfBytesRead^:=BytesRead;
    end;
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function WriteFile(hFile:HANDLE;lpBuffer:LPCVOID;nNumberOfBytesToWrite:DWORD;lpNumberOfBytesWritten:LPDWORD;lpOverlapped:LPOVERLAPPED):BOOL;
var
 BytesWritten:LongWord;
begin
 {}
 if Assigned(UltiboWriteFileHandler) then
  begin
   Result:=UltiboWriteFileHandler(hFile,lpBuffer^,nNumberOfBytesToWrite,BytesWritten);
   if Result and (lpNumberOfBytesWritten <> nil) then
    begin
     lpNumberOfBytesWritten^:=BytesWritten;
    end;
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function SetEndOfFile(hFile:HANDLE):BOOL;
begin
 {}
 if Assigned(UltiboSetEndOfFileHandler) then
  begin
   Result:=UltiboSetEndOfFileHandler(hFile);
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function SetFilePointer(hFile:HANDLE;lDistanceToMove:LONG;lpDistanceToMoveHigh:PLONG;dwMoveMethod:DWORD):DWORD;
begin
 {}
 if Assigned(UltiboSetFilePointerHandler) then
  begin
   Result:=UltiboSetFilePointerHandler(hFile,lDistanceToMove,lpDistanceToMoveHigh^,dwMoveMethod);
  end
 else
  begin
   Result:=INVALID_SET_FILE_POINTER;
  end;
end;

{==============================================================================}

function SetFilePointerEx(hFile:HANDLE;liDistanceToMove:LARGE_INTEGER;lpNewFilePointer:PLARGE_INTEGER;dwMoveMethod:DWORD):BOOL;
begin
 {}
 if Assigned(UltiboSetFilePointerExHandler) then
  begin
   Result:=UltiboSetFilePointerExHandler(hFile,liDistanceToMove.QuadPart,PInt64(lpNewFilePointer)^,dwMoveMethod);
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function FlushFileBuffers(hFile:HANDLE):BOOL;
begin
 {}
 if Assigned(UltiboFlushFileBuffersHandler) then
  begin
   Result:=UltiboFlushFileBuffersHandler(hFile);
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function CopyFile(const lpExistingFileName,lpNewFileName:LPCSTR;bFailIfExists:BOOL):BOOL; inline;
begin
 {}
 Result:=CopyFileA(lpExistingFileName,lpNewFileName,bFailIfExists);
end;

{==============================================================================}

function CopyFileA(const lpExistingFileName,lpNewFileName:LPCSTR;bFailIfExists:BOOL):BOOL;
begin
 {}
 if Assigned(UltiboCopyFileAHandler) then
  begin
   Result:=UltiboCopyFileAHandler(String(lpExistingFileName),String(lpNewFileName),bFailIfExists);
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function CopyFileW(const lpExistingFileName,lpNewFileName:LPCWSTR;bFailIfExists:BOOL):BOOL;
var
 NewFileName:String;
 ExistingFileName:String;
begin
 {}
 if Assigned(UltiboCopyFileAHandler) then
  begin
   NewFileName:=WideCharToString(lpNewFileName);
   ExistingFileName:=WideCharToString(lpExistingFileName);

   Result:=UltiboCopyFileAHandler(ExistingFileName,NewFileName,bFailIfExists);
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function SetFileShortName(hFile:HANDLE;const lpShortName:LPCSTR):BOOL; inline;
begin
 {}
 Result:=SetFileShortNameA(hFile,lpShortName);
end;

{==============================================================================}

function SetFileShortNameA(hFile:HANDLE;const lpShortName:LPCSTR):BOOL;
begin
 {}
 if Assigned(UltiboSetFileShortNameAHandler) then
  begin
   Result:=UltiboSetFileShortNameAHandler(hFile,String(lpShortName));
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function SetFileShortNameW(hFile:HANDLE;const lpShortName:LPCWSTR):BOOL;
var
 ShortName:String;
begin
 {}
 if Assigned(UltiboSetFileShortNameAHandler) then
  begin
   ShortName:=WideCharToString(lpShortName);

   Result:=UltiboSetFileShortNameAHandler(hFile,ShortName);
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function CreateHardLink(const lpFileName,lpExistingFileName:LPCSTR;lpSecurityAttributes:LPSECURITY_ATTRIBUTES):BOOL; inline;
begin
 {}
 Result:=CreateHardLinkA(lpFileName,lpExistingFileName,lpSecurityAttributes);
end;

{==============================================================================}

function CreateHardLinkA(const lpFileName,lpExistingFileName:LPCSTR;lpSecurityAttributes:LPSECURITY_ATTRIBUTES):BOOL;
{Note: lpSecurityAttributes is currently ignored by Ultibo}
begin
 {}
 if Assigned(UltiboCreateHardLinkAHandler) then
  begin
   Result:=UltiboCreateHardLinkAHandler(String(lpFileName),String(lpExistingFileName));
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function CreateHardLinkW(const lpFileName,lpExistingFileName:LPCWSTR;lpSecurityAttributes:LPSECURITY_ATTRIBUTES):BOOL;
{Note: lpSecurityAttributes is currently ignored by Ultibo}
var
 FileName:String;
 ExistingFileName:String;
begin
 {}
 if Assigned(UltiboCreateHardLinkAHandler) then
  begin
   FileName:=WideCharToString(lpFileName);
   ExistingFileName:=WideCharToString(lpExistingFileName);

   Result:=UltiboCreateHardLinkAHandler(FileName,ExistingFileName);
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function CreateSymbolicLink(const lpSymlinkFileName,lpTargetFileName:LPCSTR;dwFlags:DWORD):BOOL; inline;
begin
 {}
 Result:=CreateSymbolicLinkA(lpSymlinkFileName,lpTargetFileName,dwFlags);
end;

{==============================================================================}

function CreateSymbolicLinkA(const lpSymlinkFileName,lpTargetFileName:LPCSTR;dwFlags:DWORD):BOOL;
begin
 {}
 if Assigned(UltiboCreateSymbolicLinkAHandler) then
  begin
   Result:=UltiboCreateSymbolicLinkAHandler(String(lpSymlinkFileName),String(lpTargetFileName),(dwFlags = SYMBOLIC_LINK_FLAG_DIRECTORY));
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function CreateSymbolicLinkW(const lpSymlinkFileName,lpTargetFileName:LPCWSTR;dwFlags:DWORD):BOOL;
var
 TargetFileName:String;
 SymlinkFileName:String;
begin
 {}
 if Assigned(UltiboCreateSymbolicLinkAHandler) then
  begin
   TargetFileName:=WideCharToString(lpTargetFileName);
   SymlinkFileName:=WideCharToString(lpSymlinkFileName);

   Result:=UltiboCreateSymbolicLinkAHandler(SymlinkFileName,TargetFileName,(dwFlags = SYMBOLIC_LINK_FLAG_DIRECTORY));
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function GetFileInformationByHandle(hFile:HANDLE;var lpFileInformation:BY_HANDLE_FILE_INFORMATION):BOOL;
begin
 {}
 if Assigned(UltiboGetFileInformationByHandleHandler) then
  begin
   Result:=UltiboGetFileInformationByHandleHandler(hFile,lpFileInformation);
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function GetFinalPathNameByHandle(hFile:HANDLE;lpszFilePath:LPSTR;cchFilePath,dwFlags:DWORD):DWORD;
{Retrieves the final path for the specified open file handle}
begin
 {}
 Result:=GetFinalPathNameByHandleA(hFile,lpszFilePath,cchFilePath,dwFlags);
end;

{==============================================================================}

function GetFinalPathNameByHandleA(hFile:HANDLE;lpszFilePath:LPSTR;cchFilePath,dwFlags:DWORD):DWORD;
{Retrieves the final path for the specified open file handle}
var
 Value:String;
begin
 {}
 if Assigned(UltiboGetFinalPathNameByHandleAHandler) then
  begin
   SetLastError(ERROR_INVALID_PARAMETER);

   Result:=0;
   if lpszFilePath = nil then Exit;

   Value:=UltiboGetFinalPathNameByHandleAHandler(hFile,dwFlags);
   StrLCopy(lpszFilePath,PChar(Value),cchFilePath);

   Result:=Length(Value);
  end
 else
  begin
   SetLastError(ERROR_CALL_NOT_IMPLEMENTED);

   Result:=0;
  end;
end;

{==============================================================================}

function GetFinalPathNameByHandleW(hFile:HANDLE;lpszFilePath:LPWSTR;cchFilePath,dwFlags:DWORD):DWORD;
{Retrieves the final path for the specified open file handle}
var
 Value:String;
begin
 {}
 if Assigned(UltiboGetFinalPathNameByHandleAHandler) then
  begin
   SetLastError(ERROR_INVALID_PARAMETER);

   Result:=0;
   if lpszFilePath = nil then Exit;

   Value:=UltiboGetFinalPathNameByHandleAHandler(hFile,dwFlags);
   if StringToWideChar(Value,lpszFilePath,cchFilePath shl 1) then {Buffer length in chars, Multiply by SizeOf(WideChar)} //To Do //Use default StringToWideChar from System ? //Watch for difference in size parameter (Bytes/Chars)
    begin
     Result:=Length(Value);
    end;
  end
 else
  begin
   SetLastError(ERROR_CALL_NOT_IMPLEMENTED);

   Result:=0;
  end;
end;

{==============================================================================}
{==============================================================================}
{File Functions (Ultibo)}

{==============================================================================}
{Directory Functions (Compatibility)}
function CreateDirectory(const lpPathName:LPCSTR;lpSecurityAttributes:LPSECURITY_ATTRIBUTES):BOOL; inline;
begin
 {}
 Result:=CreateDirectoryA(lpPathName,lpSecurityAttributes);
end;

{==============================================================================}

function CreateDirectoryA(const lpPathName:LPCSTR;lpSecurityAttributes:LPSECURITY_ATTRIBUTES):BOOL;
{Note: lpSecurityAttributes is currently ignored by Ultibo}
begin
 {}
 if Assigned(UltiboCreateDirectoryAHandler) then
  begin
   Result:=UltiboCreateDirectoryAHandler(String(lpPathName));
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function CreateDirectoryW(const lpPathName:LPCWSTR;lpSecurityAttributes:LPSECURITY_ATTRIBUTES):BOOL;
{Note: lpSecurityAttributes is currently ignored by Ultibo}
var
 PathName:String;
begin
 {}
 if Assigned(UltiboCreateDirectoryAHandler) then
  begin
   PathName:=WideCharToString(lpPathName);

   Result:=UltiboCreateDirectoryAHandler(PathName);
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function RemoveDirectory(const lpPathName:LPCSTR):BOOL; inline;
begin
 {}
 Result:=RemoveDirectoryA(lpPathName);
end;

{==============================================================================}

function RemoveDirectoryA(const lpPathName:LPCSTR):BOOL;
begin
 {}
 if Assigned(UltiboRemoveDirectoryAHandler) then
  begin
   Result:=UltiboRemoveDirectoryAHandler(String(lpPathName));
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function RemoveDirectoryW(const lpPathName:LPCWSTR):BOOL;
var
 PathName:String;
begin
 {}
 if Assigned(UltiboRemoveDirectoryAHandler) then
  begin
   PathName:=WideCharToString(lpPathName);

   Result:=UltiboRemoveDirectoryAHandler(PathName);
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function SetCurrentDirectory(const lpPathName:LPCSTR):BOOL; inline;
begin
 {}
 Result:=SetCurrentDirectoryA(lpPathName);
end;

{==============================================================================}

function SetCurrentDirectoryA(const lpPathName:LPCSTR):BOOL;
begin
 {}
 if Assigned(UltiboSetCurrentDirectoryAHandler) then
  begin
   Result:=UltiboSetCurrentDirectoryAHandler(String(lpPathName));
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function SetCurrentDirectoryW(const lpPathName:LPCWSTR):BOOL;
var
 PathName:String;
begin
 {}
 if Assigned(UltiboSetCurrentDirectoryAHandler) then
  begin
   PathName:=WideCharToString(lpPathName);

   Result:=UltiboSetCurrentDirectoryAHandler(PathName);
  end
 else
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function GetCurrentDirectory(nBufferLength:DWORD;lpBuffer:LPSTR):DWORD; inline;
begin
 {}
 Result:=GetCurrentDirectoryA(nBufferLength,lpBuffer);
end;

{==============================================================================}

function GetCurrentDirectoryA(nBufferLength:DWORD;lpBuffer:LPSTR):DWORD;
var
 Value:String;
begin
 {}
 if Assigned(UltiboGetCurrentDirectoryAHandler) then
  begin
   Result:=0;
   if lpBuffer = nil then Exit;

   Value:=UltiboGetCurrentDirectoryAHandler;
   StrLCopy(lpBuffer,PChar(Value),nBufferLength);

   Result:=Length(Value);
  end
 else
  begin
   Result:=0;
  end;
end;

{==============================================================================}

function GetCurrentDirectoryW(nBufferLength:DWORD;lpBuffer:LPWSTR):DWORD;
var
 Value:String;
begin
 {}
 if Assigned(UltiboGetCurrentDirectoryAHandler) then
  begin
   Result:=0;
   if lpBuffer = nil then Exit;

   Value:=UltiboGetCurrentDirectoryAHandler;
   if StringToWideChar(Value,lpBuffer,nBufferLength shl 1) then {Buffer length in chars, Multiply by SizeOf(WideChar)} //To Do //Use default StringToWideChar from System ? //Watch for difference in size parameter (Bytes/Chars)
    begin
     Result:=Length(Value);
    end;
  end
 else
  begin
   Result:=0;
  end;
end;

{==============================================================================}

function GetLongPathName(const lpszShortPath:LPCSTR;lpszLongPath:LPSTR;cchBuffer:DWORD):DWORD; inline;
begin
 {}
 Result:=GetLongPathNameA(lpszShortPath,lpszLongPath,cchBuffer);
end;

{==============================================================================}

function GetLongPathNameA(const lpszShortPath:LPCSTR;lpszLongPath:LPSTR;cchBuffer:DWORD):DWORD;
var
 Value:String;
begin
 {}
 if Assigned(UltiboGetLongPathNameAHandler) then
  begin
   Result:=0;
   if lpszLongPath = nil then Exit;

   Value:=UltiboGetLongPathNameAHandler(String(lpszShortPath));
   StrLCopy(lpszLongPath,PChar(Value),cchBuffer);

   Result:=Length(Value);
  end
 else
  begin
   Result:=0;
  end;
end;

{==============================================================================}

function GetLongPathNameW(const lpszShortPath:LPCWSTR;lpszLongPath:LPWSTR;cchBuffer:DWORD):DWORD;
var
 Value:String;
 ShortPath:String;
begin
 {}
 if Assigned(UltiboGetLongPathNameAHandler) then
  begin
   Result:=0;
   if lpszLongPath = nil then Exit;

   ShortPath:=WideCharToString(lpszShortPath);

   Value:=UltiboGetLongPathNameAHandler(ShortPath);
   if StringToWideChar(Value,lpszLongPath,cchBuffer shl 1) then {Buffer length in chars, Multiply by SizeOf(WideChar)} //To Do //Use default StringToWideChar from System ? //Watch for difference in size parameter (Bytes/Chars)
    begin
     Result:=Length(Value);
    end;
  end
 else
  begin
   Result:=0;
  end;
end;

{==============================================================================}

function GetShortPathName(const lpszLongPath:LPCSTR;lpszShortPath:LPSTR;cchBuffer:DWORD):DWORD; inline;
begin
 {}
 Result:=GetShortPathNameA(lpszLongPath,lpszShortPath,cchBuffer);
end;

{==============================================================================}

function GetShortPathNameA(const lpszLongPath:LPCSTR;lpszShortPath:LPSTR;cchBuffer:DWORD):DWORD;
var
 Value:String;
begin
 {}
 if Assigned(UltiboGetShortPathNameAHandler) then
  begin
   Result:=0;
   if lpszShortPath = nil then Exit;

   Value:=UltiboGetShortPathNameAHandler(String(lpszLongPath));
   StrLCopy(lpszShortPath,PChar(Value),cchBuffer);

   Result:=Length(Value);
  end
 else
  begin
   Result:=0;
  end;
end;

{==============================================================================}

function GetShortPathNameW(const lpszLongPath:LPCWSTR;lpszShortPath:LPWSTR;cchBuffer:DWORD):DWORD;
var
 Value:String;
 LongPath:String;
begin
 {}
 if Assigned(UltiboGetShortPathNameAHandler) then
  begin
   Result:=0;
   if lpszShortPath = nil then Exit;

   LongPath:=WideCharToString(lpszLongPath);

   Value:=UltiboGetShortPathNameAHandler(LongPath);
   if StringToWideChar(Value,lpszShortPath,cchBuffer shl 1) then {Buffer length in chars, Multiply by SizeOf(WideChar)} //To Do //Use default StringToWideChar from System ? //Watch for difference in size parameter (Bytes/Chars)
    begin
     Result:=Length(Value);
    end;
  end
 else
  begin
   Result:=0;
  end;
end;

{==============================================================================}

function GetFullPathName(const lpFileName:LPCSTR;nBufferLength:DWORD;lpBuffer:LPSTR;var lpFilePart:LPSTR):DWORD; inline;
begin
 {}
 Result:=GetFullPathNameA(lpFileName,nBufferLength,lpBuffer,lpFilePart);
end;

{==============================================================================}

function GetFullPathNameA(const lpFileName:LPCSTR;nBufferLength:DWORD;lpBuffer:LPSTR;var lpFilePart:LPSTR):DWORD;
{Note: lpFilePart is currently ignored by Ultibo}
var
 Value:String;
begin
 {}
 if Assigned(UltiboGetFullPathNameAHandler) then
  begin
   Result:=0;
   if lpBuffer = nil then Exit;

   Value:=UltiboGetFullPathNameAHandler(String(lpFileName));
   StrLCopy(lpBuffer,PChar(Value),nBufferLength);

   Result:=Length(Value);
  end
 else
  begin
   Result:=0;
  end;
end;

{==============================================================================}

function GetFullPathNameW(const lpFileName:LPCWSTR;nBufferLength:DWORD;lpBuffer:LPWSTR;var lpFilePart:LPWSTR):DWORD;
{Note: lpFilePart is currently ignored by Ultibo}
var
 Value:String;
 FileName:String;
begin
 {}
 if Assigned(UltiboGetFullPathNameAHandler) then
  begin
   Result:=0;
   if lpBuffer = nil then Exit;

   FileName:=WideCharToString(lpFileName);

   Value:=UltiboGetFullPathNameAHandler(FileName);
   if StringToWideChar(Value,lpBuffer,nBufferLength shl 1) then {Buffer length in chars, Multiply by SizeOf(WideChar)} //To Do //Use default StringToWideChar from System ? //Watch for difference in size parameter (Bytes/Chars)
    begin
     Result:=Length(Value);
    end;
  end
 else
  begin
   Result:=0;
  end;
end;

{==============================================================================}
{Directory Functions (Ultibo)}

{==============================================================================}
{==============================================================================}
{Command Line Functions (RTL)}
function SysParamCount:LongInt;
{Note:This should be in System however the implementation there will need to be modified}
begin
 {}
 Result:=argc;
end;

{==============================================================================}

function SysParamStr(Index:LongInt):String;
{Note:This should be in System however the implementation there will need to be modified}
begin
 {ParamStr(0) is normally the executable filename,there is no such thing in embedded}
 if Index = 0 then
  begin
   Result:=KERNEL_NAME;
  end
 else if (Index < argc) then
  begin
   Result:=argv[Index];
  end
 else
  begin
   Result:='';
  end;
end;

{==============================================================================}
{==============================================================================}
{Command Line Functions (Compatibility)}
function GetCommandLine:LPSTR;
begin
 {}
 Result:=GetCommandLineA;
end;

{==============================================================================}

function GetCommandLineA:LPSTR;
{Note: The returned string must be freed with SysUtils.StrDispose}
var
 Size:LongWord;
begin
 {}
 Result:=nil;

 if cmdline <> nil then
  begin
   Size:=StrLen(cmdline) + 1;

   Result:=StrAlloc(Size);
   if Result <> nil then
    begin
     StrLCopy(Result,cmdline,StrLen(cmdline));
    end;
  end;
end;

{==============================================================================}

function GetCommandLineW:LPWSTR;
{Note: The returned string must be freed with SysUtils.StrDispose}
var
 Size:LongWord;
begin
 {}
 Result:=nil;

 if cmdline <> nil then
  begin
   Size:=StrLen(cmdline) + 1;

   Result:=WideStrAlloc(Size);
   if Result <> nil then
    begin
     StringToWideChar(cmdline,Result,Size shl 1); {Multiply by SizeOf(WideChar)} //To Do //Use default StringToWideChar from System ? //Watch for difference in size parameter (Bytes/Chars)
    end;
  end;
end;

{==============================================================================}
{==============================================================================}
{Command Line Functions (Ultibo)}
function IsParamPresent(const AParam:String):Boolean;
{Check if the specified parameter is present in the command line}
begin
 {}
 Result:=(GetParamIndex(AParam) <> -1);
end;

{==============================================================================}

function GetParamIndex(const AParam:String):Integer;
{Get the index of the specified parameter in the command line}
var
 Count:Integer;
 WorkBuffer:String;
begin
 {}
 Result:=-1;

 if ParamCount > 0 then
  begin
   if Trim(AParam) <> '' then
    begin
     for Count:=1 to ParamCount do
      begin
       WorkBuffer:=Trim(ParamStr(Count));
       if WorkBuffer <> '' then
        begin
         if Uppercase(Copy(WorkBuffer,1,Length(AParam) + 1)) = '/' + Uppercase(AParam) then
          begin
           if (Copy(WorkBuffer,Length(AParam) + 2,1) = ':') or (Copy(WorkBuffer,Length(AParam) + 2,1) = '=') or (Length(WorkBuffer) = (Length(AParam) + 1)) then
            begin
             Result:=Count;
             Exit;
            end;
          end;
         if Uppercase(Copy(WorkBuffer,1,Length(AParam) + 1)) = '-' + Uppercase(AParam) then
          begin
           if (Copy(WorkBuffer,Length(AParam) + 2,1) = ':') or (Copy(WorkBuffer,Length(AParam) + 2,1) = '=') or (Length(WorkBuffer) = (Length(AParam) + 1)) then
            begin
             Result:=Count;
             Exit;
            end;
          end;
        end;
      end;
    end;
  end;
end;

{==============================================================================}

function GetParamValue(const AParam:String):String;
{Get the value of the specified parameter from the command line}
var
 ParamIndex:Integer;
 WorkBuffer:String;
begin
 {}
 Result:='';

 ParamIndex:=GetParamIndex(AParam);
 if ParamIndex <> -1 then
  begin
   WorkBuffer:=Trim(ParamStr(ParamIndex));

   Result:=Copy(WorkBuffer,Length(AParam) + 3,Length(WorkBuffer));
  end;
end;

{==============================================================================}
{==============================================================================}
{Environment Functions (Compatibility)}
function GetEnvironmentStrings:LPSTR; inline;
begin
 {}
 Result:=GetEnvironmentStringsA;
end;

{==============================================================================}

function GetEnvironmentStringsA:LPSTR;
var
 Size:LongWord;
 Index:LongWord;
 Count:LongWord;
 Offset:PtrUInt;
 Buffer:String;
begin
 {}
 Result:=nil;

 {Get Count}
 Count:=EnvironmentCount(False);
 if Count = 0 then Exit;

 Size:=1 * SizeOf(AnsiChar);
 Offset:=0;

 {Calculate Size}
 for Index:=1 to Count do
  begin
   Buffer:=EnvironmentString(Index);
   Inc(Size,(Length(Buffer) + 1) * SizeOf(AnsiChar));
  end;

 {Allocate Environment}
 Result:=AllocMem(Size);
 if Result = nil then Exit;

 {Copy Environment}
 for Index:=1 to Count do
  begin
   Buffer:=EnvironmentString(Index);
   StrLCopy(PAnsiChar(Result + Offset),PAnsiChar(AnsiString(Buffer)),Length(Buffer));
   Inc(Offset,(Length(Buffer) + 1) * SizeOf(AnsiChar));
  end;

 {Add Final Null}
 PByte(Result + Offset)^:=0;
end;

{==============================================================================}

function GetEnvironmentStringsW:LPWSTR;
var
 Size:LongWord;
 Index:LongWord;
 Count:LongWord;
 Offset:PtrUInt;
 Buffer:String;
begin
 {}
 Result:=nil;

 {Get Count}
 Count:=EnvironmentCount(False);
 if Count = 0 then Exit;

 Size:=1 * SizeOf(WideChar);
 Offset:=0;

 {Calculate Size}
 for Index:=1 to Count do
  begin
   Buffer:=EnvironmentString(Index);
   Inc(Size,(Length(Buffer) + 1) * SizeOf(WideChar));
  end;

 {Allocate Environment}
 Result:=AllocMem(Size);
 if Result = nil then Exit;

 {Copy Environment}
 for Index:=1 to Count do
  begin
   Buffer:=EnvironmentString(Index);
   StringToWideChar(Buffer,PWideChar(Result + Offset),(Length(Buffer) + 1) shl 1); {Buffer length in chars, Multiply by SizeOf(WideChar)}
   Inc(Offset,(Length(Buffer) + 1) * SizeOf(WideChar));
  end;

 {Add Final Null}
 PWord(Result + Offset)^:=0;
end;

{==============================================================================}

function FreeEnvironmentStrings(pstr:LPSTR):BOOL; inline;
begin
 {}
 Result:=FreeEnvironmentStringsA(pstr);
end;

{==============================================================================}

function FreeEnvironmentStringsA(pstr:LPSTR):BOOL;
begin
 {}
 Result:=False;

 if pstr = nil then Exit;

 {Free Environment}
 FreeMem(pstr);

 Result:=True;
end;

{==============================================================================}

function FreeEnvironmentStringsW(pstr:LPWSTR):BOOL;
begin
 {}
 Result:=False;

 if pstr = nil then Exit;

 {Free Environment}
 FreeMem(pstr);

 Result:=True;
end;

{==============================================================================}

function GetEnvironmentVariable(const lpName:LPCSTR;lpBuffer:LPSTR;nSize:DWORD):DWORD; inline;
begin
 {}
 Result:=GetEnvironmentVariableA(lpName,lpBuffer,nSize);
end;

{==============================================================================}

function GetEnvironmentVariableA(const lpName:LPCSTR;lpBuffer:LPSTR;nSize:DWORD):DWORD;
var
 Value:String;
begin
 {}
 Result:=0;

 if lpBuffer = nil then
  begin
   if nSize <> 0 then Exit;

   Value:=EnvironmentGet(String(lpName));
   if Length(Value) <> 0 then
    begin
     Result:=Length(Value);
    end;
  end
 else
  begin
   if nSize = 0 then Exit;

   Value:=EnvironmentGet(String(lpName));
   if Length(Value) <> 0 then
    begin
     if Length(Value) < nSize then
      begin
       StrLCopy(lpBuffer,PChar(Value),nSize);
      end;

     Result:=Length(Value);
    end;
  end;
end;

{==============================================================================}

function GetEnvironmentVariableW(const lpName:LPCWSTR;lpBuffer:LPWSTR;nSize:DWORD):DWORD;
var
 Name:String;
 Value:String;
begin
 {}
 Result:=0;

 if lpBuffer = nil then
  begin
   if nSize <> 0 then Exit;

   Name:=WideCharToString(lpName);

   Value:=EnvironmentGet(Name);
   if Length(Value) <> 0 then
    begin
     Result:=Length(Value);
    end;
  end
 else
  begin
   if nSize = 0 then Exit;

   Name:=WideCharToString(lpName);

   Value:=EnvironmentGet(Name);
   if Length(Value) <> 0 then
    begin
     if Length(Value) < nSize then
      begin
       if StringToWideChar(Value,lpBuffer,nSize shl 1) then {Buffer length in chars, Multiply by SizeOf(WideChar)} //To Do //Use default StringToWideChar from System ? //Watch for difference in size parameter (Bytes/Chars)
        begin
         Result:=Length(Value);
        end;
      end
     else
      begin
       Result:=Length(Value);
      end;
    end;
  end;
end;

{==============================================================================}

function SetEnvironmentVariable(const lpName,lpValue:LPCSTR):BOOL; inline;
begin
 {}
 Result:=SetEnvironmentVariableA(lpName,lpValue);
end;

{==============================================================================}

function SetEnvironmentVariableA(const lpName,lpValue:LPCSTR):BOOL;
var
 Status:LongWord;
begin
 {}
 Status:=EnvironmentSet(String(lpName),String(lpValue));

 SetLastError(Status);
 Result:=(Status = ERROR_SUCCESS);
end;

{==============================================================================}

function SetEnvironmentVariableW(const lpName,lpValue:LPCWSTR):BOOL;
var
 Name:String;
 Value:String;
 Status:LongWord;
begin
 {}
 Name:=WideCharToString(lpName);
 Value:=WideCharToString(lpValue);

 Status:=EnvironmentSet(Name,Value);

 SetLastError(Status);
 Result:=(Status = ERROR_SUCCESS);
end;

{==============================================================================}

function ExpandEnvironmentStrings(const lpSrc:LPCSTR;lpDst:LPSTR;nSize:DWORD):DWORD; inline;
begin
 {}
 Result:=ExpandEnvironmentStringsA(lpSrc,lpDst,nSize);
end;

{==============================================================================}

function ExpandEnvironmentStringsA(const lpSrc:LPCSTR;lpDst:LPSTR;nSize:DWORD):DWORD;
begin
 {Not Supported}
 Result:=0;

 SetLastError(ERROR_NOT_SUPPORTED);
end;

{==============================================================================}

function ExpandEnvironmentStringsW(const lpSrc:LPCWSTR;lpDst:LPWSTR;nSize:DWORD):DWORD;
begin
 {Not Supported}
 Result:=0;

 SetLastError(ERROR_NOT_SUPPORTED);
end;

{==============================================================================}
{==============================================================================}
{Error Functions (Compatibility)}
function GetLastError:DWORD; inline;
{Get the last error value for the calling thread}
begin
 {}
 Result:=ThreadGetLastError;
end;

{==============================================================================}

procedure SetLastError(dwErrCode:DWORD); inline;
{Set the last error value for the calling thread}
begin
 {}
 ThreadSetLastError(dwErrCode);
end;

{==============================================================================}
{==============================================================================}
{String Functions (Ultibo)}

{==============================================================================}
{==============================================================================}
{GUID Functions (Ultibo)}
function CreateGUID:TGUID;
{Create a new GUID}
{GUID has the following format DWORD-WORD-WORD-WORD-WORDDWORD }
{                                             | Not Swapped  |}
{$IFDEF ULTIBO_USE_SYSUTILS_GUID}
begin
 {}
 SysUtils.CreateGUID(Result);
end;
{$ELSE}
var
 Count:Integer;
begin
 {}
 {FillChar(Result,SizeOf(TGUID),0);} {Not required}

 if not GUIDRandomized then
  begin
   Randomize;
   GUIDRandomized:=True;
  end;

 {Generate Random Bytes}
 for Count:=0 to SizeOf(TGUID) - 1 do
  begin
   Byte(Pointer(PtrUInt(@Result) + LongWord(Count))^):=Random(256);
  end;

 {Set Version (Version 4 Random - RFC 9562)}
 Result.D3:=(Result.D3 and $0FFF) or $4000;

 {Set Variant (Variant 8/9/A/B - RFC 9562)}
 Result.D4[0]:=(Result.D4[0] and $3F) or $80;
end;
{$ENDIF}
{==============================================================================}

function GUIDToString(const Value:TGUID;Braces:Boolean = False):String;
{Convert a TGUID to a string representation}
var
 Len:Integer;
 Format:String;
begin
 {}
 {$IFDEF ULTIBO_USE_SYSUTILS_GUID}
 if Braces then
  begin
   Result:=SysUtils.GUIDToString(Value);
   Exit;
  end;
 {$ENDIF}

 {Set Length and Format}
 Len:=36;
 Format:='%.8x-%.4x-%.4x-%.2x%.2x-%.2x%.2x%.2x%.2x%.2x%.2x';

 {Check Braces}
 if Braces then
  begin
   Len:=38;
   Format:='{' + Format + '}';
  end;

 {Create String}
 SetLength(Result,Len);
 StrLFmt(PChar(Result),Len,PChar(Format),[Value.D1,Value.D2,Value.D3,Value.D4[0],Value.D4[1],Value.D4[2],Value.D4[3],Value.D4[4],Value.D4[5],Value.D4[6],Value.D4[7]]);
end;

{==============================================================================}

function StringToGUID(const Value:String):TGUID;
{Convert a string to a native TGUID type}
{$IFDEF ULTIBO_USE_SYSUTILS_GUID}
begin
 {}
 Result:=SysUtils.StringToGUID(Value);
end;
{$ELSE}
 function HexChar(Value:Char):Byte;
 begin
  {}
  Result:=0;
  case Value of
   '0'..'9':Result:=Byte(Value) - Byte('0');
   'a'..'f':Result:=(Byte(Value) - Byte('a')) + 10;
   'A'..'F':Result:=(Byte(Value) - Byte('A')) + 10;
  end;
 end;

 function HexByte(Value:PChar):Byte;
 begin
  {}
  Result:=(HexChar(Value[0]) shl 4) + HexChar(Value[1]);
 end;

var
 Count:Integer;
 Source:PChar;
 Offset:LongWord;
begin
 {}
 FillChar(Result,SizeOf(TGUID),0);

 Offset:=0;
 Source:=PChar(Value);
 if Value[1] = '{' then
  begin
   if Length(Value) <> 38 then Exit;
   if Value[Length(Value)] <> '}' then Exit;
   Inc(Source);
  end
 else
  begin
   if Length(Value) <> 36 then Exit;
  end;

 {Get D1} {DWORD (Swapped)}
 for Count:=0 to 3 do
  begin
   Byte(Pointer(PtrUInt(@Result.D1) + LongWord(3 - Count))^):=HexByte(Source);
   Inc(Offset);
   Inc(Source,2);
  end;
 if Source[0] <> '-' then Exit;
 Inc(Source);

 {Get D2} {WORD (Swapped)}
 for Count:=0 to 1 do
  begin
   Byte(Pointer(PtrUInt(@Result.D2) + LongWord(1 - Count))^):=HexByte(Source);
   Inc(Offset);
   Inc(Source,2);
  end;
 if Source[0] <> '-' then Exit;
 Inc(Source);

 {Get D3} {WORD (Swapped)}
 for Count:=0 to 1 do
  begin
   Byte(Pointer(PtrUInt(@Result.D3) + LongWord(1 - Count))^):=HexByte(Source);
   Inc(Offset);
   Inc(Source,2);
  end;
 if Source[0] <> '-' then Exit;
 Inc(Source);

 {Get D4} {WORD-WORDDWORD (Not Swapped)}
 for Count:=0 to 1 do
  begin
   Byte(Pointer(PtrUInt(@Result.D4) + LongWord(Count))^):=HexByte(Source);
   Inc(Offset);
   Inc(Source,2);
  end;
 if Source[0] <> '-' then Exit;
 Inc(Source);
 for Count:=0 to 5 do
  begin
   Byte(Pointer(PtrUInt(@Result.D4) + LongWord(Count + 2))^):=HexByte(Source);
   Inc(Offset);
   Inc(Source,2);
  end;
end;
{$ENDIF}
{==============================================================================}

function NullGUID(const GUID:TGUID):Boolean;
{Check if a TGUID is empty (All zeroes)}
var
 Value:TGUID;
begin
 {}
 FillChar(Value,SizeOf(TGUID),0);

 {$IFDEF ULTIBO_USE_SYSUTILS_GUID}
 Result:=SysUtils.IsEqualGUID(GUID,Value);
 {$ELSE}
 Result:=CompareMem(@GUID,@Value,SizeOf(TGUID));
 {$ENDIF}
end;

{==============================================================================}

function CompareGUID(const GUID1,GUID2:TGUID):Boolean;
{Check whether two TGUID variables are equal}
begin
 {}
 {$IFDEF ULTIBO_USE_SYSUTILS_GUID}
 Result:=SysUtils.IsEqualGUID(GUID1,GUID2);
 {$ELSE}
 Result:=CompareMem(@GUID1,@GUID2,SizeOf(TGUID));
 {$ENDIF}
end;

{==============================================================================}
{SID Functions (Ultibo)}
function SIDToString(ASID:PSID):String;
var
 StringSid:PChar;
begin
 {}
 Result:='';

 StringSid:=nil;
 if Security.ConvertSidToStringSid(ASID,StringSid) then
  begin
   Result:=StringSid;

   FreeMem(StringSid);
  end;
end;

{==============================================================================}

function StringToSID(const Value:String):PSID;
var
 Sid:PSID;
 SidSize:LongWord;
begin
 {}
 Result:=nil;

 Sid:=nil;
 if Security.ConvertStringSidToSid(PChar(Value),Sid) then
  begin
   SidSize:=Security.GetLengthSid(Sid);

   Result:=GetMem(SidSize);
   if Result = nil then Exit;

   System.Move(Sid^,Result^,SidSize);

   FreeMem(Sid);
  end;
end;

{==============================================================================}
{==============================================================================}
{Date Functions (Ultibo)}

{==============================================================================}
{==============================================================================}
{Numeric Functions (Ultibo)}
function Min(A,B:Integer):Integer; inline;
begin
 {}
 if A < B then Result:=A else Result:=B;
end;

{==============================================================================}

function Max(A,B:Integer):Integer; inline;
begin
 {}
 if A > B then Result:=A else Result:=B;
end;

{==============================================================================}

function MinEx(A,B:LongWord):LongWord; inline;
begin
 {}
 if A < B then Result:=A else Result:=B;
end;

{==============================================================================}

function MaxEx(A,B:LongWord):LongWord; inline;
begin
 {}
 if A > B then Result:=A else Result:=B;
end;

{==============================================================================}

function Min64(const A,B:Int64):Int64; inline;
begin
 {}
 if A < B then Result:=A else Result:=B;
end;

{==============================================================================}

function Max64(const A,B:Int64):Int64; inline;
begin
 {}
 if A > B then Result:=A else Result:=B;
end;

{==============================================================================}

function Or64(const Value1,Value2:Int64):Int64; inline;
begin
 {}
 Int64Rec(Result).Lo:=(Int64Rec(Value1).Lo or Int64Rec(Value2).Lo);
 Int64Rec(Result).Hi:=(Int64Rec(Value1).Hi or Int64Rec(Value2).Hi);
end;

{==============================================================================}

function And64(const Value1,Value2:Int64):Int64; inline;
begin
 {}
 Int64Rec(Result).Lo:=(Int64Rec(Value1).Lo and Int64Rec(Value2).Lo);
 Int64Rec(Result).Hi:=(Int64Rec(Value1).Hi and Int64Rec(Value2).Hi);
end;

{==============================================================================}

function Xor64(const Value1,Value2:Int64):Int64; inline;
begin
 {}
 Int64Rec(Result).Lo:=(Int64Rec(Value1).Lo xor Int64Rec(Value2).Lo);
 Int64Rec(Result).Hi:=(Int64Rec(Value1).Hi xor Int64Rec(Value2).Hi);
end;

{==============================================================================}

function Not64(const Value:Int64):Int64; inline;
begin
 {}
 Int64Rec(Result).Lo:=not(Int64Rec(Value).Lo);
 Int64Rec(Result).Hi:=not(Int64Rec(Value).Hi);
end;

{==============================================================================}

function Rol32(Value:LongWord;Count:Byte):LongWord; inline;
begin
 {}
 Result:=RolDWord(Value,Count);
end;

{==============================================================================}

function Ror32(Value:LongWord;Count:Byte):LongWord; inline;
begin
 {}
 Result:=RorDWord(Value,Count);
end;

{==============================================================================}

function WordSwap(AValue:Word):Word; inline;
begin
 {}
 Result:=(AValue and $FF) shl 8 + (AValue shr 8);
end;

{==============================================================================}

function LongSwap(AValue:LongWord):LongWord; inline;
begin
 {}
 Result:=SwapEndian(AValue);
end;

{==============================================================================}

function Int64Swap(const AValue:Int64):Int64; inline;
begin
 {}
 Int64Rec(Result).Hi:=SwapEndian(Int64Rec(AValue).Lo);
 Int64Rec(Result).Lo:=SwapEndian(Int64Rec(AValue).Hi);
end;

{==============================================================================}

function BufferSwap(ABuffer:Pointer;ASize:LongWord):Boolean;
{Swap each word in the buffer supplied up to size}
{Note: Size is the number of Bytes in the buffer to swap}
var
 Count:LongWord;
 Offset:LongWord;
begin
 {}
 Result:=False;

 if ABuffer = nil then Exit;

 {Set Count}
 Count:=0;
 Offset:=0;
 while Count < (ASize - 1) do
  begin
   Word(Pointer(PtrUInt(ABuffer) + Offset)^):=SwapEndian(Word(Pointer(PtrUInt(ABuffer) + Offset)^));

   Inc(Count,2);  {SizeOf(Word)}
   Inc(Offset,2); {SizeOf(Word)}
  end;

 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{Hash Functions (Ultibo)}
function GenerateNameHash(const Name:String;Size:Integer):LongWord;
{Sum of (byte value + 1) * (position + 257) for all bytes in uppercase string}
{Note: Case Insensitive Hash}
var
 Count:Integer;
 Value:LongWord;
begin
 {}
 Result:=0;

 if Length(Name) > 0 then
  begin
   for Count:=1 to Length(Name) do
    begin
     Value:=Ord(Name[Count]);
     if (Value >= 97) and (Value <= 122) then
      begin
       Result:=Result + (((Value - 32) + 1) * (LongWord(Count) + 257));
      end
     else
      begin
       Result:=Result + ((Value + 1) * (LongWord(Count) + 257));
      end;
    end;
  end;
end;

{==============================================================================}

function GeneratePasswordHash(const Password:String):LongWord;
{Deprecated function, use GenerateStringHash instead}
begin
 {}
 Result:=GenerateStringHash(Password,False);
end;

{==============================================================================}

function GenerateStringHash(const Value:String;CaseSensitive:Boolean):LongWord;
{Sum of (byte value + 1) * (position + 257) for all bytes in string}
var
 Count:Integer;
 WorkBuffer:String;
begin
 {}
 Result:=0;

 if CaseSensitive then WorkBuffer:=Value else WorkBuffer:=Uppercase(Value);

 if Length(WorkBuffer) > 0 then
  begin
   for Count:=1 to Length(WorkBuffer) do
    begin
     Result:=Result + ((Ord(WorkBuffer[Count]) + 1) * (LongWord(Count) + 257));
    end;
  end;
end;

{==============================================================================}
{==============================================================================}
{Locale Functions (Compatibility)}
function IsValidLocale(LocaleID:LCID;dwFlags:DWORD):BOOL; inline;
begin
 {}
 Result:=Locale.IsValidLocale(LocaleID,dwFlags);
end;

{==============================================================================}

function GetSystemDefaultLCID:LCID; inline;
begin
 {}
 Result:=Locale.GetSystemDefaultLCID;
end;

{==============================================================================}

function GetUserDefaultLCID:LCID; inline;
begin
 {}
 Result:=Locale.GetUserDefaultLCID;
end;

{==============================================================================}

function GetThreadLocale:LCID;
begin
 {}
 Result:=Threads.ThreadGetLocale(Threads.ThreadGetCurrent)
end;

{==============================================================================}

function SetThreadLocale(LocaleID:LCID):BOOL;
begin
 {}
 Result:=(Threads.ThreadSetLocale(Threads.ThreadGetCurrent,LocaleID) = ERROR_SUCCESS);
end;

{==============================================================================}
{==============================================================================}
{Locale Functions (Ultibo)}
function SetSystemDefaultLCID(LocaleID:LCID):BOOL; inline;
begin
 {}
 Result:=Locale.SetSystemDefaultLCID(LocaleID);
end;

{==============================================================================}

function WideCharToString(const ABuffer:PWideChar):String;
{A replacement for WideCharToString in System unit to allow cross platform compatibility}
{Note: The WideStringManager installed by the Unicode unit should make the System version equivalent}
var
 Size:Integer;
begin
 {}
 Result:='';

 if ABuffer = nil then Exit;

 {Get Length}
 Size:=Unicode.WideCharToMultiByte(CP_ACP,0,PWideChar(ABuffer),-1,nil,0,nil,nil);
 if Size > 0 then
  begin
   {Allocate String}
   SetString(Result,nil,Size - 1); {Returned size includes null terminator because length was not specified}

   {Convert String}
   Unicode.WideCharToMultiByte(CP_ACP,0,PWideChar(ABuffer),-1,PChar(Result),Size,nil,nil);
  end;
end;

{==============================================================================}

function WideCharLenToString(const ABuffer:PWideChar;ALength:Integer):String;
{A replacement for WideCharLenToString in System unit to allow cross platform compatibility}
{Note: Length is the size of the Buffer in WideChars (not Bytes)}
{Note: The WideStringManager installed by the Unicode unit should make the System version equivalent}
var
 Size:Integer;
begin
 {}
 Result:='';

 if ABuffer = nil then Exit;

 {Check Length}
 if ALength > 0 then
  begin
   {Get Length}
   Size:=Unicode.WideCharToMultiByte(CP_ACP,0,PWideChar(ABuffer),ALength,nil,0,nil,nil);
   if Size <= ALength then
    begin
     {Allocate String}
     SetString(Result,nil,Size); {Returned size does not include null terminator because length was specified}

     {Convert String}
     Unicode.WideCharToMultiByte(CP_ACP,0,PWideChar(ABuffer),ALength,PChar(Result),Size,nil,nil);
    end;
  end;
end;

{==============================================================================}

function StringToWideChar(const AString:String;ABuffer:PWideChar;ASize:Integer):Boolean;
{A replacement for StringToWideChar in System unit to allow cross platform compatibility}
{Note: Size is the size of the Buffer in Bytes (not WideChars)}
{Note: The WideStringManager installed by the Unicode unit should make the System version equivalent}
var
 Size:Integer;
begin
 {}
 Result:=False;

 if ABuffer = nil then Exit;

 {Fill Buffer}
 FillChar(ABuffer^,ASize,0);
 {Check Length}
 if Length(AString) > 0 then
  begin
   {Convert String}
   Size:=Unicode.MultiByteToWideChar(CP_ACP,0,PChar(AString),-1,PWideChar(ABuffer),ASize shr 1); {Divide by SizeOf(WideChar)}
   if Size > 0 then
    begin
     {Set Length}
     PWord(PtrUInt(ABuffer) + LongWord((Size - 1) shl 1))^:=0; {Set null on end} {Returned size includes null terminator because length was specified}

     Result:=True;
    end;
  end;

 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{Code Page Functions (Compatibility)}
function IsValidCodePage(CodePage:UINT):BOOL; inline;
begin
 {}
 Result:=Locale.IsValidCodePage(CodePage);
end;

{==============================================================================}

function GetACP:UINT; inline;
begin
 {}
 Result:=Locale.GetACP;
end;

{==============================================================================}

function GetOEMCP:UINT; inline;
begin
 {}
 Result:=Locale.GetOEMCP;
end;

{==============================================================================}

function GetConsoleCP:UINT; inline;
begin
 {}
 Result:=Locale.GetConsoleCP;
end;

{==============================================================================}

function SetConsoleCP(wCodePageID:UINT):BOOL; inline;
begin
 {}
 Result:=Locale.SetConsoleCP(wCodePageID);
end;

{==============================================================================}

function GetConsoleOutputCP:UINT; inline;
begin
 {}
 Result:=Locale.GetConsoleOutputCP;
end;

{==============================================================================}

function SetConsoleOutputCP(wCodePageID:UINT):BOOL; inline;
begin
 {}
 Result:=Locale.SetConsoleOutputCP(wCodePageID);
end;

{==============================================================================}

function GetCPInfo(CodePage:UINT;var lpCPInfo:TCPInfo):BOOL; inline;
begin
 {}
 Result:=Locale.GetCPInfo(CodePage,lpCPInfo);
end;

{==============================================================================}

function GetCPInfoEx(CodePage:UINT;dwFlags:DWORD;var lpCPInfoEx:CPINFOEXA):BOOL; inline;
begin
 {}
 Result:=Locale.GetCPInfoEx(CodePage,dwFlags,lpCPInfoEx);
end;

{==============================================================================}

function GetCPInfoExA(CodePage:UINT;dwFlags:DWORD;var lpCPInfoEx:CPINFOEXA):BOOL; inline;
begin
 {}
 Result:=Locale.GetCPInfoExA(CodePage,dwFlags,lpCPInfoEx);
end;

{==============================================================================}

function GetCPInfoExW(CodePage:UINT;dwFlags:DWORD;var lpCPInfoEx:CPINFOEXW):BOOL; inline;
begin
 {}
 Result:=Locale.GetCPInfoExW(CodePage,dwFlags,lpCPInfoEx);
end;

{==============================================================================}
{==============================================================================}
{Code Page Functions (Ultibo)}
function SetACP(CodePage:UINT):BOOL; inline;
begin
 {}
 Result:=Locale.SetACP(CodePage);
end;

{==============================================================================}

function SetOEMCP(CodePage:UINT):BOOL; inline;
begin
 {}
 Result:=Locale.SetOEMCP(CodePage);
end;

{==============================================================================}
{==============================================================================}
{Translation Functions (Compatibility)}
function MultiByteToWideChar(CodePage:UINT;dwFlags:DWORD;lpMultiByteStr:LPCSTR;cbMultiByte:Integer;lpWideCharStr:LPWSTR;cchWideChar:Integer):Integer; inline;
begin
 {}
 Result:=Unicode.MultiByteToWideChar(CodePage,dwFlags,lpMultiByteStr,cbMultiByte,lpWideCharStr,cchWideChar);
end;

{==============================================================================}

function WideCharToMultiByte(CodePage:UINT;dwFlags:DWORD;lpWideCharStr:LPCWSTR;cchWideChar:Integer;lpMultiByteStr:LPSTR;cbMultiByte:Integer;lpDefaultChar:LPCSTR;lpUsedDefaultChar:LPBOOL):Integer; inline;
begin
 {}
 Result:=Unicode.WideCharToMultiByte(CodePage,dwFlags,lpWideCharStr,cchWideChar,lpMultiByteStr,cbMultiByte,lpDefaultChar,lpUsedDefaultChar);
end;

{==============================================================================}

function CompareString(Locale:LCID;dwCmpFlags:DWORD;lpString1:LPCSTR;cchCount1:Integer;lpString2:LPCSTR;cchCount2:Integer):Integer; inline;
begin
 {}
 Result:=Unicode.CompareString(Locale,dwCmpFlags,lpString1,cchCount1,lpString2,cchCount2);
end;

{==============================================================================}

function CompareStringA(Locale:LCID;dwCmpFlags:DWORD;lpString1:LPCSTR;cchCount1:Integer;lpString2:LPCSTR;cchCount2:Integer):Integer; inline;
begin
 {}
 Result:=Unicode.CompareStringA(Locale,dwCmpFlags,lpString1,cchCount1,lpString2,cchCount2);
end;

{==============================================================================}

function CompareStringW(Locale:LCID;dwCmpFlags:DWORD;lpString1:LPCWSTR;cchCount1:Integer;lpString2:LPCWSTR;cchCount2:Integer):Integer; inline;
begin
 {}
 Result:=Unicode.CompareStringW(Locale,dwCmpFlags,lpString1,cchCount1,lpString2,cchCount2);
end;

{==============================================================================}

function CharUpper(lpsz:LPSTR):LPSTR; inline;
begin
 {}
 Result:=Unicode.CharUpper(lpsz);
end;

{==============================================================================}

function CharUpperA(lpsz:LPSTR):LPSTR; inline;
begin
 {}
 Result:=Unicode.CharUpperA(lpsz);
end;

{==============================================================================}

function CharUpperW(lpsz:LPWSTR):LPWSTR; inline;
begin
 {}
 Result:=Unicode.CharUpperW(lpsz);
end;

{==============================================================================}

function CharUpperBuff(lpsz:LPSTR;cchLength:DWORD):DWORD; inline;
begin
 {}
 Result:=Unicode.CharUpperBuff(lpsz,cchLength);
end;

{==============================================================================}

function CharUpperBuffA(lpsz:LPSTR;cchLength:DWORD):DWORD; inline;
begin
 {}
 Result:=Unicode.CharUpperBuffA(lpsz,cchLength);
end;

{==============================================================================}

function CharUpperBuffW(lpsz:LPWSTR;cchLength:DWORD):DWORD; inline;
begin
 {}
 Result:=Unicode.CharUpperBuffW(lpsz,cchLength);
end;

{==============================================================================}

function CharLower(lpsz:LPSTR):LPSTR; inline;
begin
 {}
 Result:=Unicode.CharLower(lpsz);
end;

{==============================================================================}

function CharLowerA(lpsz:LPSTR):LPSTR; inline;
begin
 {}
 Result:=Unicode.CharLowerA(lpsz);
end;

{==============================================================================}

function CharLowerW(lpsz:LPWSTR):LPWSTR; inline;
begin
 {}
 Result:=Unicode.CharLowerW(lpsz);
end;

{==============================================================================}

function CharLowerBuff(lpsz:LPSTR;cchLength:DWORD):DWORD; inline;
begin
 {}
 Result:=Unicode.CharLowerBuff(lpsz,cchLength);
end;

{==============================================================================}

function CharLowerBuffA(lpsz:LPSTR;cchLength:DWORD):DWORD; inline;
begin
 {}
 Result:=Unicode.CharLowerBuffA(lpsz,cchLength);
end;

{==============================================================================}

function CharLowerBuffW(lpsz:LPWSTR;cchLength:DWORD):DWORD; inline;
begin
 {}
 Result:=Unicode.CharLowerBuffW(lpsz,cchLength);
end;

{==============================================================================}

function AnsiToOem(lpszSrc:LPCSTR;lpszDst:LPSTR):BOOL; inline;
begin
 {}
 Result:=Unicode.AnsiToOem(lpszSrc,lpszDst);
end;

{==============================================================================}

function AnsiToOemBuff(lpszSrc:LPCSTR;lpszDst:LPSTR;cchDstLength:DWORD):BOOL; inline;
begin
 {}
 Result:=Unicode.AnsiToOemBuff(lpszSrc,lpszDst,cchDstLength);
end;

{==============================================================================}

function OemToAnsi(lpszSrc:LPCSTR;lpszDst:LPSTR):BOOL; inline;
begin
 {}
 Result:=Unicode.OemToAnsi(lpszSrc,lpszDst);
end;

{==============================================================================}

function OemToAnsiBuff(lpszSrc:LPCSTR;lpszDst:LPSTR;cchDstLength:DWORD):BOOL; inline;
begin
 {}
 Result:=Unicode.OemToAnsiBuff(lpszSrc,lpszDst,cchDstLength);
end;

{==============================================================================}

function CharToOem(lpszSrc:LPCSTR;lpszDst:LPSTR):BOOL; inline;
begin
 {}
 Result:=Unicode.CharToOem(lpszSrc,lpszDst);
end;

{==============================================================================}

function CharToOemA(lpszSrc:LPCSTR;lpszDst:LPSTR):BOOL; inline;
begin
 {}
 Result:=Unicode.CharToOemA(lpszSrc,lpszDst);
end;

{==============================================================================}

function CharToOemW(lpszSrc:LPCWSTR;lpszDst:LPSTR):BOOL; inline;
begin
 {}
 Result:=Unicode.CharToOemW(lpszSrc,lpszDst);
end;

{==============================================================================}

function OemToChar(lpszSrc:LPCSTR;lpszDst:LPSTR):BOOL; inline;
begin
 {}
 Result:=Unicode.OemToChar(lpszSrc,lpszDst);
end;

{==============================================================================}

function OemToCharA(lpszSrc:LPCSTR;lpszDst:LPSTR):BOOL; inline;
begin
 {}
 Result:=Unicode.OemToCharA(lpszSrc,lpszDst);
end;

{==============================================================================}

function OemToCharW(lpszSrc:LPCSTR;lpszDst:LPWSTR):BOOL; inline;
begin
 {}
 Result:=Unicode.OemToCharW(lpszSrc,lpszDst);
end;

{==============================================================================}

function CharToOemBuff(lpszSrc:LPCSTR;lpszDst:LPSTR;cchDstLength:DWORD):BOOL; inline;
begin
 {}
 Result:=Unicode.CharToOemBuff(lpszSrc,lpszDst,cchDstLength);
end;

{==============================================================================}

function CharToOemBuffA(lpszSrc:LPCSTR;lpszDst:LPSTR;cchDstLength:DWORD):BOOL; inline;
begin
 {}
 Result:=Unicode.CharToOemBuffA(lpszSrc,lpszDst,cchDstLength);
end;

{==============================================================================}

function CharToOemBuffW(lpszSrc:LPCWSTR;lpszDst:LPSTR;cchDstLength:DWORD):BOOL; inline;
begin
 {}
 Result:=Unicode.CharToOemBuffW(lpszSrc,lpszDst,cchDstLength);
end;

{==============================================================================}

function OemToCharBuff(lpszSrc:LPCSTR;lpszDst:LPSTR;cchDstLength:DWORD):BOOL; inline;
begin
 {}
 Result:=Unicode.OemToCharBuff(lpszSrc,lpszDst,cchDstLength);
end;

{==============================================================================}

function OemToCharBuffA(lpszSrc:LPCSTR;lpszDst:LPSTR;cchDstLength:DWORD):BOOL; inline;
begin
 {}
 Result:=Unicode.OemToCharBuffA(lpszSrc,lpszDst,cchDstLength);
end;

{==============================================================================}

function OemToCharBuffW(lpszSrc:LPCSTR;lpszDst:LPWSTR;cchDstLength:DWORD):BOOL; inline;
begin
 {}
 Result:=Unicode.OemToCharBuffW(lpszSrc,lpszDst,cchDstLength);
end;

{==============================================================================}
{==============================================================================}
{Handle Functions (Compatibility)}
function CloseHandle(hObject:HANDLE):BOOL;
var
 HandleEntry:PHandleEntry;
begin
 {}
 Result:=False;

 {Check Handle}
 if hObject = HANDLE(INVALID_HANDLE_VALUE) then Exit;

 {Get Handle}
 HandleEntry:=PHandleEntry(hObject);
 if HandleEntry = nil then Exit;

 {Check Handle Type}
 case HandleEntry.Signature of
  SPIN_SIGNATURE:Result:=(Threads.SpinDestroy(hObject) = ERROR_SUCCESS);
  MUTEX_SIGNATURE:Result:=(Threads.MutexDestroy(hObject) = ERROR_SUCCESS);
  CRITICAL_SECTION_SIGNATURE:Result:=(Threads.CriticalSectionDestroy(hObject) = ERROR_SUCCESS);
  SEMAPHORE_SIGNATURE:Result:=(Threads.SemaphoreDestroy(hObject) = ERROR_SUCCESS);
  SYNCHRONIZER_SIGNATURE:Result:=(Threads.SynchronizerDestroy(hObject) = ERROR_SUCCESS);
  CONDITION_SIGNATURE:Result:=(Threads.ConditionDestroy(hObject) = ERROR_SUCCESS);
  COMPLETION_SIGNATURE:Result:=(Threads.CompletionDestroy(hObject) = ERROR_SUCCESS);
  THREAD_SIGNATURE:Result:=True; {No action for Threads}
  MESSAGESLOT_SIGNATURE:Result:=(Threads.MessageslotDestroy(hObject) = ERROR_SUCCESS);
  MAILSLOT_SIGNATURE:Result:=(Threads.MailslotDestroy(hObject) = ERROR_SUCCESS);
  BUFFER_SIGNATURE:Result:=(Threads.BufferDestroy(hObject) = ERROR_SUCCESS);
  EVENT_SIGNATURE:Result:=(Threads.EventDestroy(hObject) = ERROR_SUCCESS);
  TIMER_SIGNATURE:Result:=(Threads.TimerDestroy(hObject) = ERROR_SUCCESS);
  WORKER_SIGNATURE:Result:=(Threads.WorkerCancel(hObject) = ERROR_SUCCESS);
 else
  begin
   {File Close}
   if Assigned(UltiboCloseFileHandler) then
    begin
     Result:=UltiboCloseFileHandler(hObject);
    end;
  end;
 end;

 //To Do //Create a Handle list in Platform with reference count
end;

{==============================================================================}

function DuplicateHandle(hSourceProcessHandle:HANDLE;hSourceHandle:HANDLE;hTargetProcessHandle:HANDLE;lpTargetHandle:LPHANDLE;dwDesiredAccess:DWORD;bInheritHandle:BOOL;dwOptions:DWORD):BOOL;
begin
 {}
 Result:=False;

 if lpTargetHandle = nil then Exit;

 {Copy Handle}
 lpTargetHandle^:=hSourceHandle;

 Result:=True;

 //To Do //Create a Handle list in Platform with reference count
end;

{==============================================================================}

function GetHandleInformation(hObject:HANDLE;var lpdwFlags:DWORD):BOOL;
begin
 {}
 Result:=False;

 //To Do //Create a Handle list in Platform with reference count
end;

{==============================================================================}

function SetHandleInformation(hObject:HANDLE;dwMask:DWORD;dwFlags:DWORD):BOOL;
begin
 {}
 Result:=False;

 //To Do //Create a Handle list in Platform with reference count
end;

{==============================================================================}
{==============================================================================}
{Memory Functions (Compatibility)}
procedure MoveMemory(Destination,Source:PVOID;Length:SIZE_T); inline;
begin
 {}
 System.Move(Source^,Destination^,Length);
end;

{==============================================================================}

procedure CopyMemory(Destination,Source:PVOID;Length:SIZE_T); inline;
begin
 {}
 System.Move(Source^,Destination^,Length);
end;

{==============================================================================}

procedure FillMemory(Destination:PVOID;Length:SIZE_T;Fill:BYTE); inline;
begin
 {}
 FillChar(Destination^,Length,Fill);
end;

{==============================================================================}

procedure ZeroMemory(Destination:PVOID;Length:SIZE_T); inline;
begin
 {}
 FillChar(Destination^,Length,0);
end;

{==============================================================================}

function GlobalAlloc(uFlags:UINT;dwBytes:SIZE_T):HGLOBAL;
{Note: All flags except GMEM_ZEROINIT are ignored by Ultibo}
begin
 {}
 Result:=HGLOBAL(nil);

 {Check Flags}
 if (uFlags and GMEM_ZEROINIT) = 0 then
  begin
   {Get Mem}
   Result:=HGLOBAL(GetMem(dwBytes));
  end
 else
  begin
   {Alloc Mem}
   Result:=HGLOBAL(AllocMem(dwBytes));
  end;
end;

{==============================================================================}

function GlobalReAlloc(hMem:HGLOBAL;dwBytes:SIZE_T;uFlags:UINT):HGLOBAL;
begin
 {}
 Result:=HGLOBAL(nil);

 {Check Flags}
 if (uFlags and GMEM_MODIFY) <> 0 then
  begin
   {Nothing}
   Result:=hMem;
  end
 else
  begin
   {ReAlloc Mem}
   Result:=HGLOBAL(ReAllocMem(Pointer(hMem),dwBytes));
  end;
end;

{==============================================================================}

function GlobalFree(hMem:HGLOBAL):HGLOBAL;
begin
 {}
 Result:=hMem;

 if FreeMem(Pointer(hMem)) > 0 then
  begin
   Result:=HGLOBAL(nil);
  end;
end;

{==============================================================================}

function GlobalSize(hMem:HGLOBAL):SIZE_T;
begin
 {}
 Result:=MemSize(Pointer(hMem));
end;

{==============================================================================}

function GlobalFlags(hMem:HGLOBAL):UINT;
var
 Flags:LongWord;
begin
 {}
 Result:=GMEM_INVALID_HANDLE;

 Flags:=MemFlags(Pointer(hMem));
 if Flags <> HEAP_FLAG_INVALID then
  begin
   Result:=GMEM_FIXED;
  end;
end;

{==============================================================================}

function GlobalLock(hMem:HGLOBAL):LPVOID;
begin
 {}
 Result:=Pointer(hMem);
end;

{==============================================================================}

function GlobalUnlock(hMem:HGLOBAL):BOOL;
begin
 {}
 Result:=True;
end;

{==============================================================================}

function GlobalHandle(pMem:LPCVOID):HGLOBAL;
begin
 {}
 Result:=HGLOBAL(pMem);
end;

{==============================================================================}

procedure GlobalMemoryStatus(var lpBuffer:MEMORYSTATUS);
var
 MemoryLoad:Double;
 FPCStatus:TFPCHeapStatus;
begin
 {}
 FillChar(lpBuffer,SizeOf(MEMORYSTATUS),0);

 {Get Heap Status}
 FPCStatus:=GetFPCHeapStatus;

 {Get Memory Load}
 MemoryLoad:=(FPCStatus.CurrHeapFree / FPCStatus.CurrHeapSize) * 100;

 {Get Global Status}
 lpBuffer.dwLength:=SizeOf(MEMORYSTATUS);
 lpBuffer.dwMemoryLoad:=Trunc(MemoryLoad);
 lpBuffer.dwTotalPhys:=Platform.MemoryGetSize;
 {$IFDEF CPU32}
 if Platform.MemoryGetSize >= SIZE_4G then lpBuffer.dwTotalPhys:=$FFFFFFFF;
 {$ENDIF CPU32}
 lpBuffer.dwAvailPhys:=FPCStatus.CurrHeapFree;
 lpBuffer.dwTotalPageFile:=FPCStatus.CurrHeapSize;
 lpBuffer.dwAvailPageFile:=FPCStatus.CurrHeapFree;
 lpBuffer.dwTotalVirtual:=FPCStatus.CurrHeapSize;
 lpBuffer.dwAvailVirtual:=FPCStatus.CurrHeapFree;
end;

{==============================================================================}

function GlobalMemoryStatusEx(var lpBuffer:MEMORYSTATUSEX):BOOL;
var
 MemoryLoad:Double;
 FPCStatus:TFPCHeapStatus;
begin
 {}
 Result:=False;

 if lpBuffer.dwLength <> SizeOf(MEMORYSTATUSEX) then Exit;

 FillChar(lpBuffer,SizeOf(MEMORYSTATUSEX),0);

 {Get Heap Status}
 FPCStatus:=GetFPCHeapStatus;

 {Get Memory Load}
 MemoryLoad:=(FPCStatus.CurrHeapFree / FPCStatus.CurrHeapSize) * 100;

 {Get Global Status}
 lpBuffer.dwLength:=SizeOf(MEMORYSTATUSEX);
 lpBuffer.dwMemoryLoad:=Trunc(MemoryLoad);
 lpBuffer.ullTotalPhys:=Platform.MemoryGetSize;
 lpBuffer.ullAvailPhys:=FPCStatus.CurrHeapFree;
 lpBuffer.ullTotalPageFile:=FPCStatus.CurrHeapSize;
 lpBuffer.ullAvailPageFile:=FPCStatus.CurrHeapFree;
 lpBuffer.ullTotalVirtual:=FPCStatus.CurrHeapSize;
 lpBuffer.ullAvailVirtual:=FPCStatus.CurrHeapFree;
 lpBuffer.ullAvailExtendedVirtual:=0;

 Result:=True;
end;

{==============================================================================}

function LocalAlloc(uFlags:UINT;uBytes:SIZE_T):HLOCAL;
{Note: All flags except LMEM_ZEROINIT are ignored by Ultibo}
begin
 {}
 Result:=HLOCAL(nil);

 {Check Flags}
 if (uFlags and LMEM_ZEROINIT) = 0 then
  begin
   {Get Mem}
   Result:=HLOCAL(GetMem(uBytes));
  end
 else
  begin
   {Alloc Mem}
   Result:=HLOCAL(AllocMem(uBytes));
  end;
end;

{==============================================================================}

function LocalReAlloc(hMem:HLOCAL;uBytes:SIZE_T;uFlags:UINT):HLOCAL;
begin
 {}
 Result:=HLOCAL(nil);

 {Check Flags}
 if (uFlags and LMEM_MODIFY) <> 0 then
  begin
   {Nothing}
   Result:=hMem;
  end
 else
  begin
   {ReAlloc Mem}
   Result:=HLOCAL(ReAllocMem(Pointer(hMem),uBytes));
  end;
end;

{==============================================================================}

function LocalFree(hMem:HLOCAL):HLOCAL;
begin
 {}
 Result:=hMem;

 if FreeMem(Pointer(hMem)) > 0 then
  begin
   Result:=HLOCAL(nil);
  end;
end;

{==============================================================================}

function LocalSize(hMem:HLOCAL):SIZE_T;
begin
 {}
 Result:=MemSize(Pointer(hMem));
end;

{==============================================================================}

function LocalFlags(hMem:HLOCAL):UINT;
var
 Flags:LongWord;
begin
 {}
 Result:=LMEM_INVALID_HANDLE;

 Flags:=MemFlags(Pointer(hMem));
 if Flags <> HEAP_FLAG_INVALID then
  begin
   Result:=LMEM_FIXED;
  end;
end;

{==============================================================================}

function LocalLock(hMem:HLOCAL):LPVOID;
begin
 {}
 Result:=Pointer(hMem);
end;

{==============================================================================}

function LocalUnlock(hMem:HLOCAL):BOOL;
begin
 {}
 Result:=True;
end;

{==============================================================================}

function LocalHandle(pMem:LPCVOID):HLOCAL;
begin
 {}
 Result:=HLOCAL(pMem);
end;

{==============================================================================}

function VirtualAlloc(lpAddress:LPVOID;dwSize:SIZE_T;flAllocationType:DWORD;flProtect:DWORD):LPVOID;
{Note: The value of lpAddress on entry is currently ignored by Ultibo}
{Note: The value of flProtect is currently ignored by Ultibo}
{Note: As per Win32, the value of dwSize is rounded to the next page multiple}
var
 Size:LongWord;
begin
 {}
 Result:=nil;

 {Check Allocation Type}
 if (flAllocationType and (MEM_COMMIT or MEM_RESERVE)) <> 0 then
  begin
   {Check Allocation Type}
   if (flAllocationType and MEM_LARGE_PAGES) = 0 then
    begin
     {Align Size}
     Size:=Align(dwSize,Platform.MemoryGetPageSize);

     {Get Aligned Mem}
     Result:=GetAlignedMem(Size,Platform.MemoryGetPageSize);
    end
   else
    begin
     {Align Size}
     Size:=Align(dwSize,Platform.MemoryGetLargePageSize);

     {Get Aligned Mem}
     Result:=GetAlignedMem(Size,Platform.MemoryGetLargePageSize);
    end;
  end;
end;

{==============================================================================}

function VirtualFree(lpAddress:LPVOID;dwSize:SIZE_T;dwFreeType:DWORD):BOOL;
begin
 {}
 Result:=False;

 {Check Free Type}
 if (dwFreeType and MEM_DECOMMIT) <> 0 then
  begin
   {Check Mem}
   if MemFlags(lpAddress) = HEAP_FLAG_INVALID then Exit;

   Result:=True;
  end
 else if (dwFreeType and MEM_RELEASE) <> 0 then
  begin
   {Free Mem}
   if FreeMem(lpAddress) = 0 then Exit;

   Result:=True;
  end;
end;

{==============================================================================}

function VirtualQuery(lpAddress:LPCVOID;var lpBuffer:MEMORY_BASIC_INFORMATION;dwLength:DWORD):DWORD;
begin
 {}
 Result:=0;

 {Check Mem}
 if MemFlags(lpAddress) = HEAP_FLAG_INVALID then Exit;

 {Check Length}
 if dwLength <> SizeOf(MEMORY_BASIC_INFORMATION) then Exit;

 FillChar(lpBuffer,SizeOf(MEMORY_BASIC_INFORMATION),0);

 {Get Memory Information}
 lpBuffer.BaseAddress:=lpAddress;
 lpBuffer.AllocationBase:=lpAddress;
 lpBuffer.AllocationProtect:=PAGE_READWRITE;
 lpBuffer.RegionSize:=MemSize(lpAddress);
 lpBuffer.State:=MEM_COMMIT;
 lpBuffer.Protect:=PAGE_READWRITE;
 lpBuffer.Type_:=MEM_MAPPED;

 Result:=SizeOf(MEMORY_BASIC_INFORMATION);
end;

{==============================================================================}

function VirtualLock(lpAddress:LPVOID;dwSize:SIZE_T):BOOL;
begin
 {}
 Result:=False;

 {Check Mem}
 if MemFlags(lpAddress) = HEAP_FLAG_INVALID then Exit;

 Result:=True;
end;

{==============================================================================}

function VirtualUnlock(lpAddress:LPVOID;dwSize:SIZE_T):BOOL;
begin
 {}
 Result:=False;

 {Check Mem}
 if MemFlags(lpAddress) = HEAP_FLAG_INVALID then Exit;

 Result:=True;
end;

{==============================================================================}

function FlushInstructionCache(hProcess:HANDLE;lpBaseAddress:LPCVOID;dwSize:DWORD):BOOL;
begin
 {}
 Result:=False;

 if lpBaseAddress = nil then
  begin
   Platform.InvalidateInstructionCache;
  end
 else
  begin
   Platform.InvalidateInstructionCacheRange(PtrUInt(lpBaseAddress),dwSize);
  end;

 Result:=True;
end;

{==============================================================================}

function GetNumaHighestNodeNumber(var HighestNodeNumber:ULONG):BOOL;
begin
 {Not implemented, compatibility only}
 HighestNodeNumber:=0; {Previously 1, changed to comply with example code at https://msdn.microsoft.com/en-us/library/windows/desktop/aa965223(v=vs.85).aspx}
 Result:=True;
end;

{==============================================================================}

function GetNumaProcessorNode(const Processor:Byte;var NodeNumber:Byte):BOOL;
begin
 {Not implemented, compatibility only}
 NodeNumber:=0;
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{Tls Functions (Compatibility)}
function TlsAlloc:DWORD;
begin
 {}
 Result:=ThreadAllocTlsIndex;
end;

{==============================================================================}

function TlsAllocEx(bFree:BOOL):DWORD;
{bFree: If true then TlsValue will be freed on TlsFree or thread terminate}
var
 Flags:LongWord;
begin
 {}
 Flags:=THREAD_TLS_FLAG_NONE;
 if bFree then Flags:=THREAD_TLS_FLAG_FREE;

 Result:=ThreadAllocTlsIndexEx(Flags);
end;

{==============================================================================}

function TlsGetValue(dwTlsIndex:DWORD):LPVOID;
begin
 {}
 Result:=ThreadGetTlsValue(dwTlsIndex);
end;

{==============================================================================}

function TlsSetValue(dwTlsIndex:DWORD;lpTlsValue:LPVOID):BOOL;
begin
 {}
 Result:=(ThreadSetTlsValue(dwTlsIndex,lpTlsValue) = ERROR_SUCCESS);
end;

{==============================================================================}

function TlsFree(dwTlsIndex:DWORD):BOOL;
begin
 {}
 Result:=(ThreadReleaseTlsIndex(dwTlsIndex) = ERROR_SUCCESS);
end;

{==============================================================================}
{==============================================================================}
{Thread Functions (Compatibility)}
function SwitchToThread:BOOL; inline;
begin
 {}
 Result:=(Threads.ThreadYield = ERROR_SUCCESS);
end;

{==============================================================================}

procedure Sleep(dwMilliseconds:DWORD); inline;
begin
 {}
 Threads.ThreadSleep(dwMilliseconds);
end;

{==============================================================================}

function SleepEx(dwMilliseconds:DWORD;bAlertable:BOOL):DWORD; inline;
{Note: The bAlertable parameter is not currently used but is intended for I/O completion callback from ReadFileEx/WriteFileEx}
begin
 {}
 Result:=Threads.ThreadSleep(dwMilliseconds);
end;

{==============================================================================}

function GetCurrentThread:HANDLE;
begin
 {}
 Result:=Threads.ThreadGetCurrent;
end;

{==============================================================================}

function GetCurrentThreadId:DWORD;
{Note: Thread Id and Thread Handle are currently equivalent}
begin
 {}
 Result:=Threads.ThreadGetCurrent;
end;

{==============================================================================}

function GetThreadPriority(hThread:HANDLE):Integer;
{Note: Returns priority values in the range -15..+15, must use the FPC RTL function}
begin
 {}
 Result:=System.ThreadGetPriority(hThread);
end;

{==============================================================================}

function SetThreadPriority(hThread:HANDLE;nPriority:Integer):BOOL;
{Note: Expects priority values in the range -15..+15, must use the FPC RTL function}
begin
 {}
 Result:=System.ThreadSetPriority(hThread,nPriority);
end;

{==============================================================================}

function GetExitCodeThread(hThread:HANDLE;var lpExitCode:DWORD):BOOL;
begin
 {}
 Result:=False;
 lpExitCode:=Threads.ThreadGetExitCode(hThread);
 if (lpExitCode <> STILL_ACTIVE) and (lpExitCode <> LongWord(INVALID_HANDLE_VALUE)) then
  begin
   Result:=True;
  end;
end;

{==============================================================================}

function GetThreadAffinityMask(hThread:HANDLE):DWORD_PTR;
begin
 {}
 Result:=Threads.ThreadGetAffinity(hThread);
end;

{==============================================================================}

function SetThreadAffinityMask(hThread:HANDLE;dwThreadAffinityMask:DWORD_PTR):DWORD_PTR;
begin
 {}
 Result:=Threads.ThreadSetAffinity(hThread,dwThreadAffinityMask);
end;

{==============================================================================}

function GetThreadTimes(hThread:HANDLE;var lpCreationTime,lpExitTime,lpKernelTime,lpUserTime:FILETIME):BOOL;
begin
 {}
 Int64(lpUserTime):=0;

 Result:=(Threads.ThreadGetTimes(hThread,Int64(lpCreationTime),Int64(lpExitTime),Int64(lpKernelTime)) = ERROR_SUCCESS);
end;

{==============================================================================}

function CreateThread(lpThreadAttributes:LPSECURITY_ATTRIBUTES;dwStackSize:DWORD;lpStartAddress:LPTHREAD_START_ROUTINE;lpParameter:LPVOID;dwCreationFlags:DWORD;lpThreadId:LPDWORD):HANDLE;
begin
 {}
 Result:=0;

 //To Do //Use BeginThread so that FPC initialization is done
end;

{==============================================================================}

function OpenThread(dwDesiredAccess:DWORD;bInheritHandle:BOOL;dwThreadId:DWORD):HANDLE;
begin
 {}
 Result:=0;

 //To Do
end;

{==============================================================================}

function SuspendThread(hThread:HANDLE):DWORD;
begin
 {}
 Result:=Threads.ThreadSuspend(hThread);
end;

{==============================================================================}

function ResumeThread(hThread:HANDLE):DWORD;
begin
 {}
 Result:=Threads.ThreadResume(hThread);
end;

{==============================================================================}

function TerminateThread(hThread:HANDLE;dwExitCode:DWORD):BOOL;
begin
 {}
 //To Do //Should this be SysKillThread (or KillThread) so that FPC completion is done ?
         //SysKillThread just calls ThreadTerminate anyway

 Result:=(Threads.ThreadTerminate(hThread,dwExitCode) = ERROR_SUCCESS);
end;

{==============================================================================}

procedure ExitThread(dwExitCode:DWORD);
begin
 {}
 EndThread(dwExitCode); {Use EndThread instead of ThreadEnd so that FPC RTL completion is done}
end;

{==============================================================================}

function WaitForSingleObject(hHandle:HANDLE;dwMilliseconds:DWORD):DWORD;
begin
 {}
 Result:=WaitForSingleObjectEx(hHandle,dwMilliseconds,False);
end;

{==============================================================================}

function WaitForMultipleObjects(nCount:DWORD;lpHandles:PHANDLE;bWaitAll:BOOL;dwMilliseconds:DWORD):DWORD;
begin
 {}
 Result:=WaitForMultipleObjectsEx(nCount,lpHandles,bWaitAll,dwMilliseconds,False);
end;

{==============================================================================}

function WaitForSingleObjectEx(hHandle:HANDLE;dwMilliseconds:DWORD;bAlertable:BOOL):DWORD;
{Note: The bAlertable parameter is not currently used but is intended for I/O completion callback from ReadFileEx/WriteFileEx}
var
 HandleEntry:PHandleEntry;
begin
 {}
 Result:=WAIT_FAILED;

 {Check Handle}
 if hHandle = HANDLE(INVALID_HANDLE_VALUE) then Exit;

 {Get Handle}
 HandleEntry:=PHandleEntry(hHandle);
 if HandleEntry = nil then Exit;

 {Check Handle Type}
 case HandleEntry.Signature of
  MUTEX_SIGNATURE:Result:=Threads.MutexLock(hHandle); {Cannot wait with Timeout for Mutex}
  CRITICAL_SECTION_SIGNATURE:Result:=Threads.CriticalSectionLockEx(hHandle,dwMilliseconds);
  SEMAPHORE_SIGNATURE:Result:=Threads.SemaphoreWaitEx(hHandle,dwMilliseconds);
  THREAD_SIGNATURE:Result:=Threads.ThreadWaitTerminate(hHandle,dwMilliseconds);
  EVENT_SIGNATURE:Result:=Threads.EventWaitEx(hHandle,dwMilliseconds);
 end;

 //To Do //Create a Handle list in Platform with reference count
end;

{==============================================================================}

function WaitForMultipleObjectsEx(nCount:DWORD;lpHandles:PHANDLE;bWaitAll:BOOL;dwMilliseconds:DWORD;bAlertable:BOOL):DWORD;
{Note: The bAlertable parameter is not currently used but is intended for I/O completion callback from ReadFileEx/WriteFileEx}
begin
 {}
 Result:=0;

 //To Do //Create a Handle list in Platform with reference count
 //See also: RegisterWaitForSingleObject/UnregisterWait/UnregisterWaitEx
end;

{==============================================================================}
{==============================================================================}
{Thread Functions (Ultibo)}
function BeginThreadEx(ThreadFunction:TThreadFunc;Parameter:Pointer;var ThreadId:TThreadID;const StackSize:SizeUInt;Priority,Affinity,CPU:LongWord;const Name:PChar):TThreadID;
begin
 {}
 Result:=SysBeginThreadEx(nil,StackSize,ThreadFunction,Parameter,0,Priority,Affinity,CPU,Name,ThreadId);
end;

{==============================================================================}
{==============================================================================}
{Message Functions (Compatibility)}
function WaitMessage:BOOL;
begin
 {}
 Result:=(Threads.ThreadWaitMessage = ERROR_SUCCESS);
end;

{==============================================================================}

function GetMessage(lpMsg:LPMSG;hThread:HANDLE;wMsgFilterMin,wMsgFilterMax:UINT):BOOL;
var
 Message:TMessage;
begin
 {}
 Result:=(Threads.ThreadReceiveMessage(Message) = ERROR_SUCCESS);
 if Result then
  begin
   lpMsg.hThread:=ThreadGetCurrent;
   lpMsg.message:=Message.Msg;
   lpMsg.wParam:=Message.wParam;
   lpMsg.lParam:=Message.lParam;
   lpMsg.time:=Message.Time;
  end;
end;

{==============================================================================}

function PeekMessage(var lpMsg:MSG;hThread:HANDLE;wMsgFilterMin,wMsgFilterMax,wRemoveMsg:UINT):BOOL;
var
 Message:TMessage;
begin
 {}
 Result:=(Threads.ThreadReceiveMessageEx(Message,0,(wRemoveMsg and PM_REMOVE) <> 0) = ERROR_SUCCESS);
 if Result then
  begin
   lpMsg.hThread:=ThreadGetCurrent;
   lpMsg.message:=Message.Msg;
   lpMsg.wParam:=Message.wParam;
   lpMsg.lParam:=Message.lParam;
   lpMsg.time:=Message.Time;
  end;
end;

{==============================================================================}

function PostMessage(hThread:HANDLE;Msg:UINT;wParam:WPARAM;lParam:LPARAM):BOOL;
var
 Message:TMessage;
begin
 {}
 Message.Msg:=Msg;
 Message.wParam:=wParam;
 Message.lParam:=lParam;
 Message.Time:=0;
 Result:=(Threads.ThreadSendMessage(hThread,Message) = ERROR_SUCCESS);

 //To Do //Need a ThreadPostMessage/Ex ?
end;

{==============================================================================}

function SendMessage(hThread:HANDLE;Msg:UINT;wParam:WPARAM;lParam:LPARAM):LRESULT;
var
 Message:TMessage;
begin
 {}
 Message.Msg:=Msg;
 Message.wParam:=wParam;
 Message.lParam:=lParam;
 Message.Time:=0;
 Result:=Threads.ThreadSendMessage(hThread,Message);

 //To Do //Need a ThreadSendMessageEx with Wait parameter which waits for a response to the message
end;

{==============================================================================}

function SendMessageTimeout(hThread:HANDLE;Msg:UINT;wParam:WPARAM;lParam:LPARAM;fuFlags,uTimeout:UINT;var lpdwResult:DWORD_PTR):LRESULT;
var
 Message:TMessage;
begin
 {}
 Message.Msg:=Msg;
 Message.wParam:=wParam;
 Message.lParam:=lParam;
 Message.Time:=0;
 Result:=Threads.ThreadSendMessage(hThread,Message);

 //To Do //Need a ThreadSendMessageEx with Wait and Timeout parameters which waits for a response to the message
end;

{==============================================================================}
{==============================================================================}
{Notification Functions (Compatibility)}
//To Do

{==============================================================================}
{==============================================================================}
{Interlocked Functions (Compatibility)}
function InterlockedIncrement(var lpAddend:LONG):LONG; inline;
begin
 {}
 Result:=Platform.InterLockedIncrement(lpAddend);
end;

{==============================================================================}

function InterlockedDecrement(var lpAddend:LONG):LONG; inline;
begin
 {}
 Result:=Platform.InterLockedDecrement(lpAddend);
end;

{==============================================================================}

function InterlockedExchange(var Target:LONG;Value:LONG):LONG; inline;
begin
 {}
 Result:=Platform.InterLockedExchange(Target,Value);
end;

{==============================================================================}

function InterlockedExchangePointer(var Target:PVOID;Value:PVOID):PVOID; inline;
begin
 {}
 //Result:=PVOID(Platform.InterLockedExchange(PtrInt(Target),PtrInt(Value))); //TestingAARCH64
 Result:=System.InterLockedExchange(Target,Value); {Pointer version to allow for 32/64bit}
end;

{==============================================================================}

function InterlockedExchangeAdd(var Addend:LONG;Value:LONG):LONG; inline;
begin
 {}
 Result:=Platform.InterLockedAddExchange(Addend,Value);
end;

{==============================================================================}

function InterlockedCompareExchange(var Destination:LONG;Exchange:LONG;Comperand:LONG):LONG; inline;
begin
 {}
 Result:=Platform.InterlockedCompareExchange(Destination,Exchange,Comperand);
end;

{==============================================================================}

function InterlockedCompareExchangePointer(var Destination:PVOID;Exchange,Comperand:PVOID):PVOID; inline;
begin
 {}
 //Result:=PVOID(Platform.InterlockedCompareExchange(PtrInt(Destination),PtrInt(Exchange),PtrInt(Comperand))); //TestingAARCH64
 Result:=System.InterlockedCompareExchange(Destination,Exchange,Comperand); {Pointer version to allow for 32/64bit}
end;

{==============================================================================}
{==============================================================================}
{Mutex Functions (Compatibility)}
function CreateMutex(lpMutexAttributes:LPSECURITY_ATTRIBUTES;bInitialOwner:BOOL;const lpName:LPCSTR):HANDLE; inline;
begin
 {}
 Result:=CreateMutexA(lpMutexAttributes,bInitialOwner,lpName);
end;

{==============================================================================}

function CreateMutexA(lpMutexAttributes:LPSECURITY_ATTRIBUTES;bInitialOwner:BOOL;const lpName:LPCSTR):HANDLE;
begin
 {}
 Result:=Threads.MutexCreateEx(bInitialOwner,MUTEX_DEFAULT_SPINCOUNT,MUTEX_FLAG_NONE);

 //To Do //lpName to allow Open
         //Use new HandleFind/HandleOpen functions in Platform
end;

{==============================================================================}

function CreateMutexW(lpMutexAttributes:LPSECURITY_ATTRIBUTES;bInitialOwner:BOOL;const lpName:LPCWSTR):HANDLE;
begin
 {}
 Result:=Threads.MutexCreateEx(bInitialOwner,MUTEX_DEFAULT_SPINCOUNT,MUTEX_FLAG_NONE);

 //To Do //lpName to allow Open
         //Use new HandleFind/HandleOpen functions in Platform
end;

{==============================================================================}

function OpenMutex(dwDesiredAccess:DWORD;bInheritHandle:BOOL;const lpName:LPCSTR):HANDLE; inline;
begin
 {}
 Result:=OpenMutexA(dwDesiredAccess,bInheritHandle,lpName);
end;

{==============================================================================}

function OpenMutexA(dwDesiredAccess:DWORD;bInheritHandle:BOOL;const lpName:LPCSTR):HANDLE;
begin
 {}
 Result:=0;

 //To Do
         //Use new HandleFind/HandleOpen functions in Platform
end;

{==============================================================================}

function OpenMutexW(dwDesiredAccess:DWORD;bInheritHandle:BOOL;const lpName:LPCWSTR):HANDLE;
begin
 {}
 Result:=0;

 //To Do
         //Use new HandleFind/HandleOpen functions in Platform
end;

{==============================================================================}

function ReleaseMutex(hMutex:HANDLE):BOOL;
begin
 {}
 Result:=(Threads.MutexUnlock(hMutex) = ERROR_SUCCESS);
end;

{==============================================================================}
{==============================================================================}
{Semaphore Functions (Compatibility)}
function CreateSemaphore(lpSemaphoreAttributes:LPSECURITY_ATTRIBUTES;lInitialCount,lMaximumCount:LONG;const lpName:LPCSTR):HANDLE; inline;
begin
 {}
 Result:=CreateSemaphoreA(lpSemaphoreAttributes,lInitialCount,lMaximumCount,lpName);
end;

{==============================================================================}

function CreateSemaphoreA(lpSemaphoreAttributes:LPSECURITY_ATTRIBUTES;lInitialCount,lMaximumCount:LONG;const lpName:LPCSTR):HANDLE;
begin
 {}
 Result:=Threads.SemaphoreCreateEx(lInitialCount,lMaximumCount,SEMAPHORE_FLAG_NONE);

 //To Do //lpName to allow Open
         //Use new HandleFind/HandleOpen functions in Platform
end;

{==============================================================================}

function CreateSemaphoreW(lpSemaphoreAttributes:LPSECURITY_ATTRIBUTES;lInitialCount,lMaximumCount:LONG;const lpName:LPCWSTR):HANDLE;
begin
 {}
 Result:=Threads.SemaphoreCreateEx(lInitialCount,lMaximumCount,SEMAPHORE_FLAG_NONE);

 //To Do //lpName to allow Open
         //Use new HandleFind/HandleOpen functions in Platform
end;

{==============================================================================}

function OpenSemaphore(dwDesiredAccess:DWORD;bInheritHandle:BOOL;const lpName:LPCSTR):HANDLE; inline;
begin
 {}
 Result:=OpenSemaphoreA(dwDesiredAccess,bInheritHandle,lpName);
end;

{==============================================================================}

function OpenSemaphoreA(dwDesiredAccess:DWORD;bInheritHandle:BOOL;const lpName:LPCSTR):HANDLE;
begin
 {}
 Result:=0;

 //To Do
         //Use new HandleFind/HandleOpen functions in Platform
end;

{==============================================================================}

function OpenSemaphoreW(dwDesiredAccess:DWORD;bInheritHandle:BOOL;const lpName:LPCWSTR):HANDLE;
begin
 {}
 Result:=0;

 //To Do
         //Use new HandleFind/HandleOpen functions in Platform
end;

{==============================================================================}

function ReleaseSemaphore(hSemaphore:HANDLE;lReleaseCount:LONG;lpPreviousCount:LPLONG):BOOL;
begin
 {}
 Result:=(Threads.SemaphoreSignalEx(hSemaphore,lReleaseCount,PLongWord(lpPreviousCount)) = ERROR_SUCCESS);
end;

{==============================================================================}
{==============================================================================}
{Critical Section Functions (Compatibility)}
procedure InitializeCriticalSection(var lpCriticalSection:CRITICAL_SECTION);
begin
 {}
 FillChar(lpCriticalSection,SizeOf(CRITICAL_SECTION),0);
 lpCriticalSection.LockSemaphore:=Threads.CriticalSectionCreate;
end;

{==============================================================================}

procedure EnterCriticalSection(var lpCriticalSection:CRITICAL_SECTION);
begin
 {}
 Threads.CriticalSectionLockEx(lpCriticalSection.LockSemaphore,INFINITE);
end;

{==============================================================================}

procedure LeaveCriticalSection(var lpCriticalSection:CRITICAL_SECTION);
begin
 {}
 Threads.CriticalSectionUnlock(lpCriticalSection.LockSemaphore);
end;

{==============================================================================}

function TryEnterCriticalSection(var lpCriticalSection:CRITICAL_SECTION):BOOL;
begin
 {}
 Result:=(Threads.CriticalSectionTryLock(lpCriticalSection.LockSemaphore) = ERROR_SUCCESS);
end;

{==============================================================================}

function InitializeCriticalSectionAndSpinCount(var lpCriticalSection:CRITICAL_SECTION;dwSpinCount:DWORD):BOOL;
begin
 {}
 FillChar(lpCriticalSection,SizeOf(CRITICAL_SECTION),0);
 lpCriticalSection.LockSemaphore:=Threads.CriticalSectionCreateEx(False,dwSpinCount);
 Result:=(lpCriticalSection.LockSemaphore <> HANDLE(INVALID_HANDLE_VALUE));
end;

{==============================================================================}

function SetCriticalSectionSpinCount(var lpCriticalSection:CRITICAL_SECTION;dwSpinCount:DWORD):DWORD;
begin
 {}
 Result:=Threads.CriticalSectionSetSpinCount(lpCriticalSection.LockSemaphore,dwSpinCount);
end;

{==============================================================================}

procedure DeleteCriticalSection(var lpCriticalSection:CRITICAL_SECTION);
begin
 {}
 Threads.CriticalSectionDestroy(lpCriticalSection.LockSemaphore);
end;

{==============================================================================}
{==============================================================================}
{Condition Variable Functions (Compatibility)}
procedure InitializeConditionVariable(var ConditionVariable:CONDITION_VARIABLE);
{Initializes a condition variable}
{ConditionVariable: The condition variable to initialize}
begin
 {}
 FillChar(ConditionVariable,SizeOf(CONDITION_VARIABLE),0);
 THandle(PtrUInt(ConditionVariable.Ptr)):=Threads.ConditionCreate;
end;

{==============================================================================}

procedure WakeConditionVariable(var ConditionVariable:CONDITION_VARIABLE);
{Wake a single thread waiting on the specified condition variable}
{ConditionVariable: The condition variable to wake}
begin
 {}
 if ConditionVariable.Ptr = nil then
  begin
   InitializeConditionVariable(ConditionVariable);
  end;

 Threads.ConditionWake(THandle(ConditionVariable.Ptr));
end;

{==============================================================================}

procedure WakeAllConditionVariable(var ConditionVariable:CONDITION_VARIABLE);
{Wake all threads waiting on the specified condition variable}
{ConditionVariable: The condition variable to wake}
begin
 {}
 if ConditionVariable.Ptr = nil then
  begin
   InitializeConditionVariable(ConditionVariable);
  end;

 Threads.ConditionWakeAll(THandle(ConditionVariable.Ptr));
end;

{==============================================================================}

function SleepConditionVariableCS(var ConditionVariable:CONDITION_VARIABLE; var CriticalSection:CRITICAL_SECTION;dwMilliseconds:DWORD):BOOL;
{Sleeps on the specified condition variable and releases the specified critical section as an atomic operation}
{ConditionVariable: The condition variable to sleep on}
{CriticalSection: The critical section object to release (This critical section must be entered exactly once by the caller at the time SleepConditionVariableCS is called)}
{dwMilliseconds: The time-out interval, in milliseconds. (If the time-out interval elapses, the function re-acquires the critical section and returns false)}
begin
 {}
 if ConditionVariable.Ptr = nil then
  begin
   InitializeConditionVariable(ConditionVariable);
  end;

 Result:=(Threads.ConditionWaitCriticalSection(THandle(ConditionVariable.Ptr),CriticalSection.LockSemaphore,dwMilliseconds) = ERROR_SUCCESS);
end;

{==============================================================================}

procedure DeleteConditionVariable(var ConditionVariable:CONDITION_VARIABLE);
{Destroy a condition variable}
{ConditionVariable: The condition variable to destroy}

{Note: This function is Ultibo specific and is not part of the normal Windows API}
begin
 {}
 if ConditionVariable.Ptr <> nil then
  begin
   Threads.ConditionDestroy(THandle(ConditionVariable.Ptr));
  end;
end;

{==============================================================================}
{==============================================================================}
{Event Functions (Compatibility)}
function CreateEvent(lpEventAttributes:LPSECURITY_ATTRIBUTES;bManualReset,bInitialState:BOOL;const lpName:LPCSTR):HANDLE; inline;
begin
 {}
 Result:=CreateEventA(lpEventAttributes,bManualReset,bInitialState,lpName);
end;

{==============================================================================}

function CreateEventA(lpEventAttributes:LPSECURITY_ATTRIBUTES;bManualReset,bInitialState:BOOL;const lpName:LPCSTR):HANDLE;
begin
 {}
 Result:=Threads.EventCreate(bManualReset,bInitialState);

 //To Do //lpName to allow OpenEvent
         //Use new HandleFind/HandleOpen functions in Platform
end;

{==============================================================================}

function CreateEventW(lpEventAttributes:LPSECURITY_ATTRIBUTES;bManualReset,bInitialState:BOOL;const lpName:LPCWSTR):HANDLE;
begin
 {}
 Result:=Threads.EventCreate(bManualReset,bInitialState);

 //To Do //lpName to allow OpenEvent
         //Use new HandleFind/HandleOpen functions in Platform
end;

{==============================================================================}

function OpenEvent(dwDesiredAccess:DWORD;bInheritHandle:BOOL;const lpName:LPCSTR):HANDLE; inline;
begin
 {}
 Result:=OpenEventA(dwDesiredAccess,bInheritHandle,lpName);
end;

{==============================================================================}

function OpenEventA(dwDesiredAccess:DWORD;bInheritHandle:BOOL;const lpName:LPCSTR):HANDLE;
begin
 {}
 Result:=0;

 //To Do
         //Use new HandleFind/HandleOpen functions in Platform
end;

{==============================================================================}

function OpenEventW(dwDesiredAccess:DWORD;bInheritHandle:BOOL;const lpName:LPCWSTR):HANDLE;
begin
 {}
 Result:=0;

 //To Do
         //Use new HandleFind/HandleOpen functions in Platform
end;

{==============================================================================}

function SetEvent(hEvent:HANDLE):BOOL;
begin
 {}
 Result:=(Threads.EventSet(hEvent) = ERROR_SUCCESS);
end;

{==============================================================================}

function ResetEvent(hEvent:HANDLE):BOOL;
begin
 {}
 Result:=(Threads.EventReset(hEvent) = ERROR_SUCCESS);
end;

{==============================================================================}

function PulseEvent(hEvent:HANDLE):BOOL;
begin
 {}
 Result:=(Threads.EventPulse(hEvent) = ERROR_SUCCESS);
end;

{==============================================================================}
{==============================================================================}
{Process Functions (Compatibility)}
function GetProcessAffinityMask(hProcess:HANDLE;var lpProcessAffinityMask,lpSystemAffinityMask:DWORD_PTR):BOOL;
{Note: Ultibo has no concept of a Process so this function is mapped to Threads instead}
begin
 {}
 Result:=False;

 {Get Affinity}
 lpSystemAffinityMask:=Platform.CPUGetMask;
 lpProcessAffinityMask:=Threads.ThreadGetAffinity(hProcess);
 if lpProcessAffinityMask <> LongWord(INVALID_HANDLE_VALUE) then
  begin
   Result:=True;
  end;
end;

{==============================================================================}

function SetProcessAffinityMask(hProcess:HANDLE;dwProcessAffinityMask:DWORD_PTR):BOOL;
{Note: Ultibo has no concept of a Process so this function is mapped to Threads instead}
begin
 {}
 Result:=False;

 if Threads.ThreadSetAffinity(hProcess,dwProcessAffinityMask) <> LongWord(INVALID_HANDLE_VALUE) then
  begin
   Result:=True;
  end;
end;

{==============================================================================}

function GetProcessTimes(hProcess:HANDLE;var lpCreationTime,lpExitTime,lpKernelTime,lpUserTime:FILETIME):BOOL;
{Note: Ultibo has no concept of a Process so this function is mapped to Threads instead}
begin
 {}
 Int64(lpUserTime):=0;

 Result:=(Threads.ThreadGetTimes(hProcess,Int64(lpCreationTime),Int64(lpExitTime),Int64(lpKernelTime)) = ERROR_SUCCESS);
end;

{==============================================================================}

function GetProcessIoCounters(hProcess:HANDLE;var lpIoCounters:IO_COUNTERS):BOOL;
{Note: Ultibo has no concept of a Process so this function is mapped to Threads instead}
begin
 {}
 Result:=False;

 //To Do
end;

{==============================================================================}

function GetCurrentProcess:HANDLE;
{Note: Ultibo has no concept of a Process so this function is mapped to Threads instead}
begin
 {}
 Result:=Threads.ThreadGetCurrent;
end;

{==============================================================================}

function GetCurrentProcessId:DWORD;
{Note: Ultibo has no concept of a Process so this function is mapped to Threads instead}
{Note: Thread Id and Thread Handle are currently equivalent}
begin
 {}
 Result:=Threads.ThreadGetCurrent;
end;

{==============================================================================}

procedure ExitProcess(uExitCode:UINT);
{Note: Ultibo has no concept of a Process so this function is mapped to Threads instead}
begin
 {}
 EndThread(uExitCode); {Use EndThread instead of ThreadEnd so that FPC RTL completion is done}
end;

{==============================================================================}

procedure FatalExit(ExitCode:Integer);
{Note: Ultibo has no concept of a Process so this function is mapped to Threads instead}
begin
 {}
 ThreadHalt(ExitCode);
end;

{==============================================================================}

function TerminateProcess(hProcess:HANDLE;uExitCode:UINT):BOOL;
{Note: Ultibo has no concept of a Process so this function is mapped to Threads instead}
begin
 {}
 Result:=(Threads.ThreadTerminate(hProcess,uExitCode) = ERROR_SUCCESS);
end;

{==============================================================================}
{==============================================================================}
{Debug Functions (Compatibility)}
procedure OutputDebugString(const lpOutputString:LPCSTR); inline;
begin
 {}
 OutputDebugStringA(lpOutputString);
end;

{==============================================================================}

procedure OutputDebugStringA(const lpOutputString:LPCSTR);
begin
 {}
 LoggingOutput(String(lpOutputString));
end;

{==============================================================================}

procedure OutputDebugStringW(const lpOutputString:LPCWSTR);
var
 Value:String;
begin
 {}
 Value:=WideCharToString(lpOutputString);

 LoggingOutput(Value);
end;

{==============================================================================}
{==============================================================================}
{Library Functions (Compatibility)}
function lstrcmp(lpString1,lpString2:LPCSTR):Integer; inline;
begin
 {}
 Result:=lstrcmpA(lpString1,lpString2);
end;

{==============================================================================}

function lstrcmpA(lpString1,lpString2:LPCSTR):Integer;
begin
 {}
 Result:=StrComp(lpString1,lpString2);
end;

{==============================================================================}

function lstrcmpW(lpString1,lpString2:LPCWSTR):Integer;
begin
 {}
 Result:=StrComp(lpString1,lpString2);
end;

{==============================================================================}

function lstrcmpi(lpString1,lpString2:LPCSTR):Integer; inline;
begin
 {}
 Result:=lstrcmpiA(lpString1,lpString2);
end;

{==============================================================================}

function lstrcmpiA(lpString1,lpString2:LPCSTR):Integer;
begin
 {}
 Result:=StrIComp(lpString1,lpString2);
end;

{==============================================================================}

function lstrcmpiW(lpString1,lpString2:LPCWSTR):Integer;
begin
 {}
 Result:=StrIComp(lpString1,lpString2);
end;

{==============================================================================}

function lstrcpy(lpString1:LPSTR;lpString2:LPCSTR):LPSTR;  inline;
begin
 {}
 Result:=lstrcpyA(lpString1,lpString2);
end;

{==============================================================================}

function lstrcpyA(lpString1:LPSTR;lpString2:LPCSTR):LPSTR;
begin
 {}
 Result:=StrCopy(lpString1,lpString2);
end;

{==============================================================================}

function lstrcpyW(lpString1:LPWSTR;lpString2:LPCWSTR):LPWSTR;
begin
 {}
 Result:=StrCopy(lpString1,lpString2);
end;

{==============================================================================}

function lstrcpyn(lpString1:LPSTR;lpString2:LPCSTR;iMaxLength:Integer):LPSTR; inline;
begin
 {}
 Result:=lstrcpynA(lpString1,lpString2,iMaxLength);
end;

{==============================================================================}

function lstrcpynA(lpString1:LPSTR;lpString2:LPCSTR;iMaxLength:Integer):LPSTR;
begin
 {}
 Result:=StrLCopy(lpString1,lpString2,iMaxLength);
end;

{==============================================================================}

function lstrcpynW(lpString1:LPWSTR;lpString2:LPCWSTR;iMaxLength:Integer):LPWSTR;
begin
 {}
 Result:=StrLCopy(lpString1,lpString2,iMaxLength);
end;

{==============================================================================}

function lstrcat(lpString1:LPSTR;lpString2:LPCSTR):LPSTR; inline;
begin
 {}
 Result:=lstrcatA(lpString1,lpString2);
end;

{==============================================================================}

function lstrcatA(lpString1:LPSTR;lpString2:LPCSTR):LPSTR;
begin
 {}
 Result:=StrCat(lpString1,lpString2);
end;

{==============================================================================}

function lstrcatW(lpString1:LPWSTR;lpString2:LPCWSTR):LPWSTR;
begin
 {}
 Result:=StrCat(lpString1,lpString2);
end;

{==============================================================================}

function lstrlen(lpString:LPCSTR):Integer; inline;
begin
 {}
 Result:=lstrlenA(lpString);
end;

{==============================================================================}

function lstrlenA(lpString:LPCSTR):Integer;
begin
 {}
 Result:=StrLen(lpString);
end;

{==============================================================================}

function lstrlenW(lpString:LPCWSTR):Integer;
begin
 {}
 Result:=StrLen(lpString);
end;

{==============================================================================}
{==============================================================================}

initialization
 UltiboInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
