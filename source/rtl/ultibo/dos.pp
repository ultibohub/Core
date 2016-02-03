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

unit Dos;

interface

{$MODE objfpc}

type
  SearchRec = Packed Record
	AnchorPtr : Pointer;    { Pointer to the Anchorpath structure }
	Fill: Array[1..15] of Byte; {future use}
    {End of replacement for fill}
    Attr : BYTE;        {attribute of found file}
    Time : LongInt;     {last modify date of found file}
    Size : LongInt;     {file size of found file}
    Name : String[255]; {name of found file}
  End;

{$I dosh.inc}

implementation

{$I dos.inc}

{******************************************************************************
                           --- Internal routines ---
******************************************************************************}

function dosLock(const name: String; accessmode: Longint) : LongInt;
begin
  result := -1;
end;

function IsLeapYear(Source : Word) : Boolean;
begin
  result := false;
end;

function dosSetProtection(const name: string; mask:longint): Boolean;
begin
  result := false;
end;

function dosSetFileDate(name: string): Boolean;
begin
  result := false;
end;


{******************************************************************************
                        --- Info / Date / Time ---
******************************************************************************}

function DosVersion: Word;
begin
  result := 0;
end;

procedure NewList ();
begin
end;

function CreateExtIO (size: Longint): integer;
begin
  result := -1;
end;

procedure DeleteExtIO ();
begin
end;

function Createport(name : PChar; pri : longint): integer;
begin
  result := -1;
end;

procedure DeletePort ();
begin
end;


function Create_Timer(theUnit : longint) : integer;
begin
  result := -1;
end;

Procedure Delete_Timer();
begin
end;

function set_new_time(secs, micro : longint): longint;
begin
  result := -1;
end;

function get_sys_time(): longint;
begin
  result := -1;
end;

procedure GetDate(Var Year, Month, MDay, WDay: Word);
begin
end;

procedure SetDate(Year, Month, Day: Word);
begin
end;

procedure GetTime(Var Hour, Minute, Second, Sec100: Word);
begin
end;


Procedure SetTime(Hour, Minute, Second, Sec100: Word);
begin
end;



{******************************************************************************
                               --- Exec ---
******************************************************************************}
procedure Exec(const Path: PathStr; const ComLine: ComStr);
begin
end;


{******************************************************************************
                               --- Disk ---
******************************************************************************}

Function DiskFree(Drive: Byte): int64;
Begin
  result := -1;
end;



Function DiskSize(Drive: Byte): int64;
Begin
  result := -1;
end;


procedure FindFirst(const Path: PathStr; Attr: Word; Var f: SearchRec);
begin
end;


procedure FindNext(Var f: SearchRec);
begin
end;

procedure FindClose(Var f: SearchRec);
begin
end;


{******************************************************************************
                               --- File ---
******************************************************************************}


function FSearch(path: PathStr; dirlist: String) : PathStr;
begin
  result := '';
end;


Procedure getftime (var f; var time : longint);
begin
end;


Procedure setftime(var f; time : longint);
Begin
End;

procedure getfattr(var f; var attr : word);
begin
End;


procedure setfattr(var f; attr : word);
begin
end;


{******************************************************************************
                             --- Environment ---
******************************************************************************}

function getpathstring: string;
begin
  result := '';
end;


function EnvCount: Longint;
begin
  result := -1;
end;


function EnvStr(Index: LongInt): String;
begin
  result := '';
end;



function GetEnv(envvar : String): String;
begin
  result := '';
end;


procedure AddDevice(str : String);
begin
end;

function MakeDeviceName(str : pchar): string;
begin
  result := '';
end;

function IsInDeviceList(str : string): boolean;
begin
  result := false;
end;

procedure ReadInDevices;
begin
end;

begin
//  DosError:=0;
//  numberofdevices := 0;
//  StrOfPaths := '';
//  ReadInDevices;
end.
