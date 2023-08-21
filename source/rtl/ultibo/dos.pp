{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2015 by Free Pascal development team

    Dos unit for Ultibo target.
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
unit Dos;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$MODE objfpc}

{const}
  {Max Path (Exactly equivalent to Win32}
  {MAX_PATH = 260;} {Defined in sysutilh.inc}
  
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

  SearchRec = record
   FindHandle  : THandle;
   FindData : TWin32FindDataA;
   ExcludeAttr : Longint;
   Time : Longint;
   Size : Longint;
   Attr : Longint;
   Name : String;
  end;

{$I dosh.inc}

type
 {Conversion Functions}
 TDosGetMsCount = function:Int64;
 {Info/Date/Time Functions}
 TDosDosVersion = function:Word;
 TDosGetDate = procedure(var Year, Month, MDay, WDay: Word);
 TDosSetDate = procedure(Year, Month, Day: Word);
 TDosGetTime = procedure(var Hour, Minute, Second, Sec100: Word);
 TDosSetTime = procedure(Hour, Minute, Second, Sec100: Word);
 {Disk Functions}
 TDosDiskFree = function(Drive:Byte):Int64;
 TDosDiskSize = function(Drive:Byte):Int64;
 {FindFirst/FindNext Functions}
 TDosFindFirst = function(const Path: PathStr; Attr: Word; var f: SearchRec):Integer;
 TDosFindNext = function(var f: SearchRec):Integer;
 TDosFindClose = procedure(var f: SearchRec);
 {File Functions}
 TDosGetFTime = function(var f; var time : longint):Integer;
 TDosSetFTime = function(var f; time : longint):Integer;
 TDosGetFAttr = function(var f; var attr : word):Integer;
 TDosSetFAttr = function(var f; attr : word):Integer;
 TDosGetShortName = function(var p : String) : boolean;
 TDosGetLongName = function(var p : String) : boolean;
 {Environment Functions}
 TDosEnvCount = function:Longint;
 TDosEnvStr = function(Index:LongInt):String;
 TDosGetEnv = function(envvar:String):String; 

var
 {Conversion Functions}
 DosGetMsCountHandler:TDosGetMsCount;
 {Info/Date/Time Functions}
 DosDosVersionHandler:TDosDosVersion;
 DosGetDateHandler:TDosGetDate;
 DosSetDateHandler:TDosSetDate;
 DosGetTimeHandler:TDosGetTime;
 DosSetTimeHandler:TDosSetTime;
 {Disk Functions}
 DosDiskFreeHandler:TDosDiskFree;
 DosDiskSizeHandler:TDosDiskSize;
 {FindFirst/FindNext Functions}
 DosFindFirstHandler:TDosFindFirst;
 DosFindNextHandler:TDosFindNext;
 DosFindCloseHandler:TDosFindClose;
 {File Functions}
 DosGetFTimeHandler:TDosGetFTime;
 DosSetFTimeHandler:TDosSetFTime;
 DosGetFAttrHandler:TDosGetFAttr;
 DosSetFAttrHandler:TDosSetFAttr;
 DosGetShortNameHandler:TDosGetShortName;
 DosGetLongNameHandler:TDosGetLongName;
 {Environment Functions}
 DosEnvCountHandler:TDosEnvCount;
 DosEnvStrHandler:TDosEnvStr;
 DosGetEnvHandler:TDosGetEnv;
 
implementation

{$DEFINE HAS_GETMSCOUNT}
{$DEFINE HAS_GETSHORTNAME}
{$DEFINE HAS_GETLONGNAME}

{--$DEFINE FPC_FEXPAND_UNC} (* UNC paths are supported *)
{$DEFINE FPC_FEXPAND_DRIVES} (* Full paths begin with drive specification *)

{$I dos.inc}

{******************************************************************************
                           --- Conversion ---
******************************************************************************}

function GetMsCount: Int64;
begin
 if Assigned(DosGetMsCountHandler) then
  begin
   Result:=DosGetMsCountHandler();
  end
 else
  begin
   Result:=0;
  end; 
end;

{******************************************************************************
                        --- Info / Date / Time ---
******************************************************************************}

function DosVersion: Word;
begin
 if Assigned(DosDosVersionHandler) then
  begin
   Result:=DosDosVersionHandler();
  end
 else
  begin
   Result:=0;
  end; 
end;

procedure GetDate(var Year, Month, MDay, WDay: Word);
begin
 if Assigned(DosGetDateHandler) then
  begin
   DosGetDateHandler(Year, Month, MDay, WDay);
  end;
end;

procedure SetDate(Year, Month, Day: Word);
begin
 if Assigned(DosSetDateHandler) then
  begin
   DosSetDateHandler(Year, Month, Day);
  end;
end;

procedure GetTime(var Hour, Minute, Second, Sec100: Word);
begin
 if Assigned(DosGetTimeHandler) then
  begin
   DosGetTimeHandler(Hour, Minute, Second, Sec100);
  end;
end;

procedure SetTime(Hour, Minute, Second, Sec100: Word);
begin
 if Assigned(DosSetTimeHandler) then
  begin
   DosSetTimeHandler(Hour, Minute, Second, Sec100);
  end;
end;

{******************************************************************************
                               --- Exec ---
******************************************************************************}

procedure Exec(const Path: PathStr; const ComLine: ComStr);
begin
 {Not supported by Ultibo}
end;

{******************************************************************************
                               --- Disk ---
******************************************************************************}

function DiskFree(Drive: Byte): Int64;
begin
 if Assigned(DosDiskFreeHandler) then
  begin
   Result:=DosDiskFreeHandler(Drive);
  end
 else
  begin
   Result:=-1;
  end; 
end;

function DiskSize(Drive: Byte): Int64;
begin
 if Assigned(DosDiskSizeHandler) then
  begin
   Result:=DosDiskSizeHandler(Drive);
  end
 else
  begin
   Result:=-1;
  end; 
end;

{******************************************************************************
                         --- FindFirst / FindNext ---
******************************************************************************}

procedure FindFirst(const Path: PathStr; Attr: Word; var f: SearchRec);
begin
 FillChar(f,SizeOf(SearchRec),0);
 
 if Assigned(DosFindFirstHandler) then
  begin
   DosError:=DosFindFirstHandler(Path,Attr,f);
  end
 else
  begin
   DosError:=18;
  end; 
end;

procedure FindNext(var f: SearchRec);
begin
 if Assigned(DosFindNextHandler) then
  begin
   DosError:=DosFindNextHandler(f);
  end
 else
  begin
   DosError:=18;
  end; 
end;

procedure FindClose(var f: SearchRec);
begin
 if Assigned(DosFindCloseHandler) then
  begin
   DosFindCloseHandler(f);
  end;
end;

{******************************************************************************
                               --- File ---
******************************************************************************}

function FSearch(path: PathStr; dirlist: String) : PathStr;
var
 P1     : Longint;
 S      : SearchRec;
 NewDir : PathStr;
begin
 {Check if the file specified exists}
 FindFirst(path,anyfile and not(directory),S);
 if DosError = 0 then
  begin
   FindClose(S);
   Result:=path;
   Exit;
  end;
  
 {No wildcards allowed in these things}
 if (Pos('?',path) <> 0) or (Pos('*',path) <> 0) then
  begin
   Result:=''
  end 
 else
  begin
   {Allow slash as backslash}
   DoDirSeparators(dirlist);
   repeat
    P1:=Pos(';',dirlist);
    if p1 <> 0 then
     begin
      NewDir:=Copy(dirlist,1,P1 - 1);
      Delete(dirlist,1,P1);
     end
    else
     begin
      NewDir:=dirlist;
      dirlist:='';
     end;
    
    if (NewDir <> '') and (not(NewDir[Length(NewDir)] in ['\',':'])) then
     NewDir:=NewDir + '\';
    
    FindFirst(NewDir + path,anyfile and not(directory),S);
    if DosError = 0 then
     NewDir:=NewDir + path
    else
     NewDir:='';
      
   until (dirlist = '') or (NewDir <> '');
   
   Result:=NewDir;
  end;
   
 FindClose(S);
end;

procedure GetFTime (var f; var time : longint);
begin
 if Assigned(DosGetFTimeHandler) then
  begin
   DosError:=DosGetFTimeHandler(f,time);
  end
 else
  begin
   DosError:=18;
  end; 
end;

procedure SetFTime(var f; time : longint);
begin
 if Assigned(DosSetFTimeHandler) then
  begin
   DosError:=DosSetFTimeHandler(f,time);
  end
 else
  begin
   DosError:=18;
  end; 
end;

procedure GetFAttr(var f; var attr : word);
begin
 if Assigned(DosGetFAttrHandler) then
  begin
   DosError:=DosGetFAttrHandler(f,attr);
  end
 else
  begin
   DosError:=18;
  end; 
end;

procedure SetFAttr(var f; attr : word);
begin
 {Fail for setting VolumeId}
 if (attr and VolumeID) <> 0 then
  begin
   DosError:=5
  end 
 else
  begin
   if Assigned(DosSetFAttrHandler) then
    begin
     DosError:=DosSetFAttrHandler(f,attr);
    end
   else
    begin
     DosError:=18;
    end; 
  end; 
end;

function GetShortName(var p : String) : boolean;
begin
 if Assigned(DosGetShortNameHandler) then
  begin
   Result:=DosGetShortNameHandler(p);
  end
 else
  begin
   Result:=False;
  end; 
end;

function GetLongName(var p : String) : boolean;
begin
 if Assigned(DosGetLongNameHandler) then
  begin
   Result:=DosGetLongNameHandler(p);
  end
 else
  begin
   Result:=False;
  end; 
end;

{******************************************************************************
                             --- Environment ---
******************************************************************************}

function EnvCount: Longint;
begin
 if Assigned(DosEnvCountHandler) then
  begin
   Result:=DosEnvCountHandler();
  end
 else
  begin
   Result:=-1;
  end; 
end;

function EnvStr(Index: LongInt): String;
begin
 if Assigned(DosEnvStrHandler) then
  begin
   Result:=DosEnvStrHandler(Index);
  end
 else
  begin
   Result:='';
  end; 
end;

function GetEnv(envvar : String): String;
begin
 if Assigned(DosGetEnvHandler) then
  begin
   Result:=DosGetEnvHandler(envvar);
  end
 else
  begin
   Result:='';
  end; 
end;

begin
 DosError:=0;
end.
