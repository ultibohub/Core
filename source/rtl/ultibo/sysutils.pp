{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2024 by the Free Pascal development team

    Sysutils unit for Ultibo target.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$MODESWITCH OUT}
{$IFDEF UNICODERTL}
{$MODESWITCH UNICODESTRINGS}
{$ELSE}
{$H+}
{$ENDIF}

{$IFNDEF FPC_DOTTEDUNITS}
unit sysutils;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$DEFINE HAS_SLEEP}
{$DEFINE HAS_OSERROR}
{$DEFINE HAS_SYSTEMTIME}
{$DEFINE HAS_GETTICKCOUNT}
{$DEFINE HAS_GETTICKCOUNT64}
{$DEFINE HAS_LOCALTIMEZONEOFFSET}
{$DEFINE HAS_FILEGETDATETIMEINFO}
{$DEFINE HAS_INVALIDHANDLE}
{$modeswitch typehelpers}
{$modeswitch advancedrecords}

{ used OS file system APIs use ansistring }
{$define SYSUTILS_HAS_ANSISTR_FILEUTIL_IMPL}
{ OS has an ansistring/single byte environment variable API }
{$define SYSUTILS_HAS_ANSISTR_ENVVAR_IMPL}

{const}
  {Max Path (Exactly equivalent to Win32}
  {MAX_PATH = 260;} {Defined in sysutilh.inc}

const
  INVALID_HANDLE_VALUE = THandle(-1);

type
  {File Time (Exactly equivalent to Win32}
  FILETIME = record
   dwLowDateTime:DWORD;
   dwHighDateTime:DWORD;
  end;
  
type
  {Find Data (Exactly equivalent to Win32}
  TWin32FindDataA = record
   dwFileAttributes:DWORD;
   ftCreationTime:FILETIME;
   ftLastAccessTime:FILETIME;
   ftLastWriteTime:FILETIME;
   nFileSizeHigh:DWORD;
   nFileSizeLow:DWORD;
   dwReserved0:DWORD;
   dwReserved1:DWORD;
   cFileName:array[0..({System.}MaxPathLen) - 1] of AnsiCHAR;
   cAlternateFileName:array[0..13] of AnsiCHAR;
  end;
  TWin32FindData = TWin32FindDataA;

type
  {System Time (Equivalent to Win32 with FPC compatability}
  SYSTEMTIME = record
   case Integer of
    1:(
     wYear:Word;
     wMonth:Word;
     wDayOfWeek:Word;
     wDay:Word;
     wHour:Word;
     wMinute:Word;
     wSecond:Word;
     wMilliseconds:Word;
     );
    {FPC compatibility}
    2:(
     Year:WORD;
     Month:WORD;
     DayOfWeek:WORD;
     Day:WORD;
     Hour:WORD;
     Minute:WORD;
     Second:WORD;
     Millisecond:WORD;
     );
  end;
  LPSYSTEMTIME = ^SYSTEMTIME;
  _SYSTEMTIME = SYSTEMTIME;
  TSystemTime = SYSTEMTIME;
  PSystemTime = ^SYSTEMTIME;

  { Include platform independent interface part }
  {$i sysutilh.inc}

type
 {File Functions}
 TSysUtilsFileOpen = function(const FileName:RawByteString;Mode:Integer):THandle;
 TSysUtilsFileCreate = function(const FileName:RawByteString;ShareMode:Integer):THandle;
 TSysUtilsDeleteFile = function(const FileName:RawByteString):Boolean;
 TSysUtilsFileClose = procedure(Handle:THandle);
 TSysUtilsRenameFile = function(const OldName,NewName:RawByteString):Boolean;
 TSysUtilsFileSeek = function(Handle:THandle;FOffset,Origin:LongInt):LongInt;
 TSysUtilsFileTruncate = function(Handle:THandle;Size:Int64):Boolean;
 TSysUtilsFileAge = function(const FileName:RawByteString):LongInt;
 {$ifndef FPC_LEGACY}
 TSysUtilsFileGetSymLinkTarget = function(const FileName:RawByteString;out SymLinkRec:TRawbyteSymLinkRec):Boolean;
 {$endif}
 TSysUtilsFileExists = function(const FileName:RawByteString;FollowLink:Boolean):Boolean;
 TSysUtilsFileGetAttr = function(const FileName:RawByteString):LongInt;
 TSysUtilsFileGetDate = function(Handle:THandle):LongInt;
 TSysUtilsFileSetAttr = function(const FileName:RawByteString;Attr:LongInt):LongInt;
 TSysUtilsFileSetDate = function(Handle:THandle;Age:LongInt):LongInt;
 TSysUtilsFileRead = function(Handle:THandle;out Buffer;Count:LongInt):LongInt;
 TSysUtilsFileWrite = function(Handle:THandle;const Buffer;Count:LongInt):LongInt;
 TSysUtilsFileSeekEx = function(Handle:THandle;FOffset:Int64;Origin:LongInt):Int64;
 
 TSysUtilsInternalFindFirst = function(const Path:RawByteString;Attr:LongInt;out SearchRec:TSearchRec;var Name:RawByteString):LongInt;
 TSysUtilsInternalFindNext = function(var SearchRec:TSearchRec;var Name:RawByteString):LongInt;
 TSysUtilsInternalFindClose = procedure(var Handle:THandle);
 {Disk Functions}
 TSysUtilsDiskFree = function(Drive:Byte):Int64;
 TSysUtilsDiskSize = function(Drive:Byte):Int64;
 TSysUtilsDirectoryExists = function(const Directory:RawByteString;FollowLink:Boolean):Boolean;
 {Thread Functions}
 TSysUtilsSleep = function(Milliseconds:LongWord):LongWord;
 {Tick Functions}
 TSysUtilsGetTickCount = function:LongWord;
 TSysUtilsGetTickCount64 = function:QWord;
 {Misc Functions}
 TSysUtilsGetLastError = function:LongWord;
 {Locale Functions}
 TSysUtilsGetLocalTime = procedure(var SystemTime:TSystemTime);
 TSysUtilsSetLocalTime = procedure(const SystemTime:TSystemTime);
 TSysUtilsGetUniversalTime = function(var SystemTime:TSystemTime):Boolean;
 TSysUtilsGetLocalTimeOffset = function:Integer;
 TSysUtilsGetLocalTimeOffsetEx = function(const DateTime:TDateTime;const InputIsUTC:Boolean;out Offset:Integer;out IsDST:Boolean):Boolean;
 TSysUtilsSysErrorMessage = function(ErrorCode:Integer):String;
 {FileTime Functions}
 TSysUtilsFileTimeToSystemTime = function(const lpFileTime:FILETIME;var lpSystemTime:SYSTEMTIME):ByteBool;
 TSysUtilsFileTimeToLocalFileTime = function(const lpFileTime:FILETIME;var lpLocalFileTime:FILETIME):ByteBool;
 
var
 {File Functions}
 SysUtilsFileOpenHandler:TSysUtilsFileOpen;
 SysUtilsFileCreateHandler:TSysUtilsFileCreate;
 SysUtilsDeleteFileHandler:TSysUtilsDeleteFile;
 SysUtilsFileCloseHandler:TSysUtilsFileClose;
 SysUtilsRenameFileHandler:TSysUtilsRenameFile;
 SysUtilsFileSeekHandler:TSysUtilsFileSeek;
 SysUtilsFileTruncateHandler:TSysUtilsFileTruncate;
 SysUtilsFileAgeHandler:TSysUtilsFileAge;
 {$ifndef FPC_LEGACY}
 SysUtilsFileGetSymLinkTargetHandler:TSysUtilsFileGetSymLinkTarget;
 {$endif}
 SysUtilsFileExistsHandler:TSysUtilsFileExists;
 SysUtilsFileGetAttrHandler:TSysUtilsFileGetAttr;
 SysUtilsFileGetDateHandler:TSysUtilsFileGetDate;
 SysUtilsFileSetAttrHandler:TSysUtilsFileSetAttr;
 SysUtilsFileSetDateHandler:TSysUtilsFileSetDate;
 SysUtilsFileReadHandler:TSysUtilsFileRead;
 SysUtilsFileWriteHandler:TSysUtilsFileWrite;
 SysUtilsFileSeekExHandler:TSysUtilsFileSeekEx;
 
 SysUtilsInternalFindFirstHandler:TSysUtilsInternalFindFirst;
 SysUtilsInternalFindNextHandler:TSysUtilsInternalFindNext;
 SysUtilsInternalFindCloseHandler:TSysUtilsInternalFindClose;
 {Disk Functions}
 SysUtilsDiskFreeHandler:TSysUtilsDiskFree;
 SysUtilsDiskSizeHandler:TSysUtilsDiskSize;
 SysUtilsDirectoryExistsHandler:TSysUtilsDirectoryExists;
 {Thread Functions}
 SysUtilsSleepHandler:TSysUtilsSleep;
 {Tick Functions}
 SysUtilsGetTickCountHandler:TSysUtilsGetTickCount;
 SysUtilsGetTickCount64Handler:TSysUtilsGetTickCount64;
 {Misc Functions}
 SysUtilsGetLastErrorHandler:TSysUtilsGetLastError;
 {Locale Functions}
 SysUtilsGetLocalTimeHandler:TSysUtilsGetLocalTime;
 SysUtilsSetLocalTimeHandler:TSysUtilsSetLocalTime;
 SysUtilsGetUniversalTimeHandler:TSysUtilsGetUniversalTime;
 SysUtilsGetLocalTimeOffsetHandler:TSysUtilsGetLocalTimeOffset;
 SysUtilsGetLocalTimeOffsetExHandler:TSysUtilsGetLocalTimeOffsetEx;
 SysUtilsSysErrorMessageHandler:TSysUtilsSysErrorMessage;
 {FileTime Functions}
 SysUtilsFileTimeToSystemTimeHandler:TSysUtilsFileTimeToSystemTime;
 SysUtilsFileTimeToLocalFileTimeHandler:TSysUtilsFileTimeToLocalFileTime;
 
procedure ResetEnvironmentVariableCount;
function GetEnvironmentIndex(Const EnvVar : AnsiString) : Integer;

procedure SysUtilsInitExceptions;
 
implementation

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysConst;
{$ELSE FPC_DOTTEDUNITS}
uses
  sysconst;
{$ENDIF FPC_DOTTEDUNITS}

{****************************************************************************
                              Time Functions
****************************************************************************}

function FileTimeToSystemTime(const lpFileTime:FILETIME;var lpSystemTime:SYSTEMTIME):ByteBool;
begin
 if Assigned(SysUtilsFileTimeToSystemTimeHandler) then
  begin
   Result:=SysUtilsFileTimeToSystemTimeHandler(lpFileTime,lpSystemTime);
  end
 else
  begin
   Result:=False;
  end;
end;

function FileTimeToLocalFileTime(const lpFileTime:FILETIME;var lpLocalFileTime:FILETIME):ByteBool;
begin
 if Assigned(SysUtilsFileTimeToLocalFileTimeHandler) then
  begin
   Result:=SysUtilsFileTimeToLocalFileTimeHandler(lpFileTime,lpLocalFileTime);
  end
 else
  begin
   Result:=False;
  end;
end;

  { Include platform independent implementation part }
  {$i sysutils.inc}

var
 SysUtilsExceptionsInitialized:Boolean;
 
{****************************************************************************
                              File Functions
****************************************************************************}

function FileOpen(const FileName: RawByteString; Mode: Integer): THandle;
begin
 if Assigned(SysUtilsFileOpenHandler) then
  begin
   Result:=SysUtilsFileOpenHandler(FileName,Mode);
  end
 else
  begin
   Result:=-1;
  end; 
end;

function FileGetDate(Handle: THandle) : TOSTimestamp;
begin
 if Assigned(SysUtilsFileGetDateHandler) then
  begin
   Result:=SysUtilsFileGetDateHandler(Handle);
  end
 else
  begin
   Result:=-1;
  end;
end;

function FileSetDate(Handle : THandle;Age : TOSTimestamp) : Longint;
begin
 if Assigned(SysUtilsFileSetDateHandler) then
  begin
   Result:=SysUtilsFileSetDateHandler(Handle,Age);
  end
 else
  begin
   Result:=-1;
  end;
end;

function FileCreate(const FileName: RawByteString) : THandle;
begin
 if Assigned(SysUtilsFileCreateHandler) then
  begin
   Result:=SysUtilsFileCreateHandler(FileName,fmShareExclusive);
  end
 else
  begin
   Result:=-1;
  end;
end;

function FileCreate(const FileName: RawByteString; Rights: integer): THandle;
begin
 if Assigned(SysUtilsFileCreateHandler) then
  begin
   Result:=SysUtilsFileCreateHandler(FileName,fmShareExclusive); {Rights is ignored by Ultibo}
  end
 else
  begin
   Result:=-1;
  end;
end;

function FileCreate(const FileName: RawByteString; ShareMode: integer; Rights : integer): THandle;
begin
 if Assigned(SysUtilsFileCreateHandler) then
  begin
   Result:=SysUtilsFileCreateHandler(FileName,ShareMode); {Rights is ignored by Ultibo}
  end
 else
  begin
   Result:=-1;
  end;
end;

function FileRead(Handle: THandle; Out Buffer; Count: LongInt): LongInt;
begin
 if Assigned(SysUtilsFileReadHandler) then
  begin
   Result:=SysUtilsFileReadHandler(Handle,Buffer,Count);
  end
 else
  begin
   Result:=-1;
  end;
end;

function FileWrite(Handle: THandle; const Buffer; Count: LongInt): LongInt;
begin
 if Assigned(SysUtilsFileWriteHandler) then
  begin
   Result:=SysUtilsFileWriteHandler(Handle,Buffer,Count);
  end
 else
  begin
   Result:=-1;
  end;
end;

function FileSeek(Handle : THandle; FOffset, Origin: Longint) : Longint;
begin
 if Assigned(SysUtilsFileSeekHandler) then
  begin
   Result:=SysUtilsFileSeekHandler(Handle,FOffset,Origin);
  end
 else
  begin
   Result:=-1;
  end;
end;

function FileSeek(Handle : THandle; FOffset: Int64; Origin: Longint) : Int64;
begin
 if Assigned(SysUtilsFileSeekExHandler) then
  begin
   Result:=SysUtilsFileSeekExHandler(Handle,FOffset,Origin);
  end
 else
  begin
   Result:=-1;
  end;
end;

procedure FileClose(Handle: THandle);
begin
 if Assigned(SysUtilsFileCloseHandler) then
  begin
   SysUtilsFileCloseHandler(Handle);
  end;
end;

function FileTruncate(Handle: THandle; Size: Int64): Boolean;
begin
 if Assigned(SysUtilsFileTruncateHandler) then
  begin
   Result:=SysUtilsFileTruncateHandler(Handle,Size);
  end
 else
  begin
   Result:=False;
  end;
end;

function DeleteFile(const FileName: RawByteString) : Boolean;
begin
 if Assigned(SysUtilsDeleteFileHandler) then
  begin
   Result:=SysUtilsDeleteFileHandler(FileName);
  end
 else
  begin
   Result:=False;
  end;
end;

function RenameFile(const OldName, NewName: RawByteString): Boolean;
begin
 if Assigned(SysUtilsRenameFileHandler) then
  begin
   Result:=SysUtilsRenameFileHandler(OldName,NewName);
  end
 else
  begin
   Result:=False;
  end;
end;

Function FileAge (Const FileName : RawByteString): TOSTimestamp;
begin
 if Assigned(SysUtilsFileAgeHandler) then
  begin
   Result:=SysUtilsFileAgeHandler(FileName);
  end
 else
  begin
   Result:=-1;
  end;
end;

{$ifndef FPC_LEGACY}
function FileGetSymLinkTarget (const FileName: RawByteString; out SymLinkRec: TRawbyteSymLinkRec): Boolean;
begin
 if Assigned(SysUtilsFileGetSymLinkTargetHandler) then
  begin
   Result:=SysUtilsFileGetSymLinkTargetHandler(FileName,SymLinkRec);
  end
 else
  begin
   Result:=False;
  end;
end;
{$endif}

Function FileExists (Const FileName : RawByteString{$ifndef FPC_LEGACY}; FollowLink : Boolean = True{$endif}) : Boolean;
Begin
 if Assigned(SysUtilsFileExistsHandler) then
  begin
   Result:=SysUtilsFileExistsHandler(FileName,{$ifdef FPC_LEGACY}True{$else}FollowLink{$endif});
  end
 else
  begin
   Result:=False;
  end;
end;

Function InternalFindFirst (Const Path : RawByteString; Attr : Longint; out Rslt : TAbstractSearchRec; var Name: RawByteString) : Longint;
var
 SearchRec:TSearchRec;
begin
 if Assigned(SysUtilsInternalFindFirstHandler) then
  begin
   Name:=Path;
   Rslt.Attr:=Attr;
   Rslt.ExcludeAttr:=(not Attr) and ($1e); { $1e = faHidden or faSysFile or faVolumeID or faDirectory }
   
   Result:=SysUtilsInternalFindFirstHandler(Path,Attr,SearchRec,Name);
   if Result = 0 then
    begin
     Rslt.Time:=SearchRec.Time;
     Rslt.Size:=SearchRec.Size;
     Rslt.Attr:=SearchRec.Attr;
     Rslt.FindData:=SearchRec.FindData;
     Rslt.FindHandle:=SearchRec.FindHandle;
    end;
  end
 else
  begin
   Result:=-1;
  end; 
end;

Function InternalFindNext (var Rslt : TAbstractSearchRec; var Name : RawByteString) : Longint;
var
 SearchRec:TSearchRec;
begin
 if Assigned(SysUtilsInternalFindNextHandler) then
  begin
   SearchRec.FindHandle:=Rslt.FindHandle;
   SearchRec.ExcludeAttr:=Rslt.ExcludeAttr;
   
   Result:=SysUtilsInternalFindNextHandler(SearchRec,Name);
   if Result = 0 then
    begin
     Rslt.Time:=SearchRec.Time;
     Rslt.Size:=SearchRec.Size;
     Rslt.Attr:=SearchRec.Attr;
     Rslt.FindData:=SearchRec.FindData;
    end;
  end
 else
  begin
   Result:=-1;
  end; 
end;

{$ifdef FPC_LEGACY}
Procedure InternalFindClose(var Handle: THandle{$ifdef USEFINDDATA};var FindData: TFindData{$endif});
{$else}
Procedure InternalFindClose(var Handle: THandle{$ifdef SEARCHREC_USEFINDDATA};var FindData: TFindData{$endif});
{$endif}
begin 
 if Assigned(SysUtilsInternalFindCloseHandler) then
  begin
   SysUtilsInternalFindCloseHandler(Handle);
  end
end;

Function FileGetAttr (Const FileName : RawByteString) : Longint;
begin
 if Assigned(SysUtilsFileGetAttrHandler) then
  begin
   Result:=SysUtilsFileGetAttrHandler(FileName);
  end
 else
  begin
   Result:=-1;
  end;
end;

Function FileSetAttr (Const Filename : RawByteString; Attr: longint) : Longint;
begin
 if Assigned(SysUtilsFileSetAttrHandler) then
  begin
   Result:=SysUtilsFileSetAttrHandler(Filename,Attr);
  end
 else
  begin
   Result:=-1;
  end;
end;

{$IF NOT DEFINED(FPC_STABLE) AND NOT DEFINED(FPC_LEGACY)}
function FileGetDateTimeInfo(const FileName: string; out DateTime: TDateTimeInfoRec; FollowLink: Boolean = True): Boolean;
var
 Info:TSearchRec;
begin
 Result:=FindFirst(FileName,0,Info) = 0;
 if Result then
  begin
   DateTime.data:=Info.FindData;
   
   FindClose(Info);
  end;
end;
{$ENDIF}

{****************************************************************************
                              Disk Functions
****************************************************************************}

Procedure AddDisk(const path:string);
begin
 {Not required by Ultibo}
end;

Function DiskFree(Drive: Byte): int64;
Begin
 if Assigned(SysUtilsDiskFreeHandler) then
  begin
   Result:=SysUtilsDiskFreeHandler(Drive);
  end
 else
  begin
   Result:=-1;
  end;
End;


Function DiskSize(Drive: Byte): int64;
Begin
 if Assigned(SysUtilsDiskSizeHandler) then
  begin
   Result:=SysUtilsDiskSizeHandler(Drive);
  end
 else
  begin
   Result:=-1;
  end;
End;

Function DirectoryExists (Const Directory : RawByteString{$ifndef FPC_LEGACY}; FollowLink: Boolean = True{$endif}) : Boolean;
begin
 if Assigned(SysUtilsDirectoryExistsHandler) then
  begin
   Result:=SysUtilsDirectoryExistsHandler(Directory,{$ifdef FPC_LEGACY}True{$else}FollowLink{$endif});
  end
 else
  begin
   Result:=False;
  end;
end;

{****************************************************************************
                            Thread Functions
****************************************************************************}

procedure Sleep(milliseconds: Cardinal);
begin
  if Assigned(SysUtilsSleepHandler) then
   begin
    SysUtilsSleepHandler(milliseconds);
   end;
end;

{****************************************************************************
                             Tick Functions
****************************************************************************}

function GetTickCount:LongWord;
begin
  if Assigned(SysUtilsGetTickCountHandler) then
   begin
    Result:=SysUtilsGetTickCountHandler();
   end
  else
   begin
    Result:=0;
   end;   
end;

function GetTickCount64:QWord;
begin
  if Assigned(SysUtilsGetTickCount64Handler) then
   begin
    Result:=SysUtilsGetTickCount64Handler();
   end
  else
   begin
    Result:=0;
   end;
end;

{****************************************************************************
                              Misc Functions
****************************************************************************}

procedure SysBeep;
begin
 {No SysBeep for Ultibo} 
end;

Function GetLastOSError : Integer;
begin
  Result:=-1;
  if Assigned(SysUtilsGetLastErrorHandler) then
   begin
    Result:=SysUtilsGetLastErrorHandler();
   end;
end;

{****************************************************************************
                              Locale Functions
****************************************************************************}

Procedure GetLocalTime(var SystemTime: TSystemTime);
begin
 if Assigned(SysUtilsGetLocalTimeHandler) then
  begin
   SysUtilsGetLocalTimeHandler(SystemTime);
  end;
end;

Procedure SetLocalTime(const SystemTime: TSystemTime);
begin
 if Assigned(SysUtilsSetLocalTimeHandler) then
  begin
   SysUtilsSetLocalTimeHandler(SystemTime);
  end;
end;

function GetUniversalTime(var SystemTime: TSystemTime): Boolean;
begin
  Result:=False;

 if Assigned(SysUtilsGetUniversalTimeHandler) then
  begin
   Result:=SysUtilsGetUniversalTimeHandler(SystemTime);
  end;
end;

function GetLocalTimeOffset: Integer;
begin
 if Assigned(SysUtilsGetLocalTimeOffsetHandler) then
  begin
   Result:=SysUtilsGetLocalTimeOffsetHandler();
  end
 else
  begin
   Result:=0;
  end;  
end;

{$if defined(FPC_STABLE) or defined(FPC_FIXES) or defined(FPC_LEGACY)}
function GetLocalTimeOffset(const DateTime: TDateTime; const InputIsUTC: Boolean; out Offset: Integer): Boolean;
var
 IsDST:Boolean;
{$else}
function GetLocalTimeOffset(const DateTime: TDateTime; const InputIsUTC: Boolean; out Offset: Integer; out IsDST : Boolean): Boolean;
{$endif}
begin
 if Assigned(SysUtilsGetLocalTimeOffsetExHandler) then
  begin
   Result:=SysUtilsGetLocalTimeOffsetExHandler(DateTime,InputIsUTC,Offset,IsDST);
  end
 else
  begin
   Result:=False;
  end;  
end;

function SysErrorMessage(ErrorCode: Integer): String;
begin
 {Result:=StrError(ErrorCode);}
 if Assigned(SysUtilsSysErrorMessageHandler) then
  begin
   Result:=SysUtilsSysErrorMessageHandler(ErrorCode);
  end
 else
  begin 
   Result:='';
  end; 
end;

{****************************************************************************
                              OS utility functions
****************************************************************************}

Function GetEnvironmentVariable(Const EnvVar : AnsiString) : AnsiString;
begin
 Result:=FPCGetEnvVarFromP(envp,EnvVar);
end;

Function GetEnvironmentVariableCount : Integer;
begin
 Result:=FPCCountEnvVar(envp);
end;

Procedure ResetEnvironmentVariableCount;
begin
 FPC_EnvCount:=-1;
end;

Function FPCGetEnvIdxFromP(EP : PPAnsiChar; EnvVar : String) : Integer;
// Modified from FPCGetEnvVarFromP
var
  hp         : PPAnsiChar;
  lenvvar,hs : string;
  eqpos      : longint;
  idx        : Integer;

begin
  lenvvar:=upcase(envvar);
  hp:=EP;
  idx:=0;
  Result:=idx;
  If (hp<>Nil) then
    while assigned(hp^) do
     begin
       inc(idx);
       hs:=strpas(hp^);
       eqpos:=pos('=',hs);
       if upcase(copy(hs,1,eqpos-1))=lenvvar then
        begin
          Result:=idx;
          exit;
        end;
       inc(hp);
     end;
end;

Function GetEnvironmentIndex(Const EnvVar : AnsiString) : Integer;
begin
 Result:=FPCGetEnvIdxFromP(envp,EnvVar);
end;

Function GetEnvironmentString(Index : Integer) : {$ifdef FPC_RTL_UNICODE}UnicodeString{$else}AnsiString{$endif};
begin
 Result:='';
 
 if Index >= 1 then
   Result:=FPCGetEnvStrFromP(envp,Index);
end;

{$ifdef FPC_LEGACY}
function ExecuteProcess (const Path: AnsiString; const ComLine: AnsiString;Flags:TExecuteFlags=[]): integer;
begin
 Result:=-1;
end;

function ExecuteProcess (const Path: AnsiString; const ComLine: array of AnsiString;Flags:TExecuteFlags=[]): integer;
begin
 Result:=-1;
end;
{$else}
function ExecuteProcess (const Path: RawByteString; const ComLine: RawByteString;Flags:TExecuteFlags=[]): integer;
begin
 Result:=-1;
end;

function ExecuteProcess (const Path: RawByteString; const ComLine: array of RawByteString;Flags:TExecuteFlags=[]): integer;
begin
 Result:=-1;
end;

function ExecuteProcess (const Path: UnicodeString; const ComLine: UnicodeString;Flags:TExecuteFlags=[]): integer;
begin
 Result:=-1;
end;

function ExecuteProcess (const Path: UnicodeString; const ComLine: array of UnicodeString;Flags:TExecuteFlags=[]): integer;
begin
 Result:=-1;
end;
{$endif}

{****************************************************************************
                            Initialization functions
****************************************************************************}

procedure SysUtilsInitExceptions;
begin
 if SysUtilsExceptionsInitialized then Exit;
 
 InitExceptions;
 
 SysUtilsExceptionsInitialized:=True;
end;
 
{****************************************************************************
                              Initialization code
****************************************************************************}

Initialization
  SysUtilsInitExceptions;
  OnBeep:=@SysBeep;
Finalization
  DoneExceptions;
end.
