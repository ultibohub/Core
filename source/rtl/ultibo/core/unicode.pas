{
Ultibo Widestring Manager interface unit.

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


Unicode
=======

 This unit implements the WideString and UnicodeString support for Ultibo
 including the WideStringManager/UnicodeStringManager interface for the RTL.
 
 This unit provides compatible implementations of the following functions:

        CharLower            CharLowerBuff             CharNext
        CharPrev             CharToOem                 CharToOemBuff
        CharUpper            CharUpperBuff             CompareStringA
        FoldString           FormatMessage             GetStringTypeA
        GetStringTypeEx      GetStringTypeW            IsCharAlpha
        IsCharAlphaNumeric   IsCharLower               IsCharUpper
        IsDBCSLeadByte       IsTextUnicode             LCMapString
        LoadString           lstrcat                   lstrcmp
        lstrcmpi             lstrcpy                   lstrcpyn
        lstrlen              MultiByteToWideChar       OemToChar
        OemToCharBuff        WideCharToMultiByte       wsprintf
        wvsprintf            CompareStringW            AnsiToOem
        OemToAnsi
        
        The following function are implemented by the FileSystem unit:

        AreFileApisANSI    (AreFileApisANSI is exposed in the Ultibo unit)
        SetFileApisToANSI  (SetFileApisToANSI is exposed in the Ultibo unit)
        SetFileApisToOEM   (SetFileApisToOEM is exposed in the Ultibo unit)

        Most of the above are currently not implemented.
        
 See also: https://msdn.microsoft.com/en-us/library/windows/desktop/dd319081%28v=vs.85%29.aspx
        
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Unicode;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Locale,SysUtils;

//Note: Embedded system unit comments out the initunicodestringmanager call, need to call during Initialization - Need to modify System.pp
//      Also need to define {$define HAS_WIDESTRINGMANAGER} in System.pp
//See: \source\rtl\embedded\system.pp

//See: \source\rtl\win\syswin.inc (Windows Widestring manager) for starting point
//     \source\rtl\win\sysutils.pp
// 

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
//const
 {Unicode specific constants}

 //To Do

{==============================================================================}
type
 {Unicode specific types}

 {String types}
 LPSTR = PAnsiChar;
 LPCSTR = PAnsiChar;
 
 {Wide String types}
 LPWSTR = PWideChar;
 LPCWSTR = PWideChar;
 
 //To Do
 
{==============================================================================}
{var}
 {Unicode specific variables}

{==============================================================================}
{Initialization Functions}
procedure UnicodeInit;

{==============================================================================}
{Unicode Functions}
function MultiByteToWideChar(CodePage:UINT;dwFlags:DWORD;lpMultiByteStr:LPCSTR;cbMultiByte:Integer;lpWideCharStr:LPWSTR;cchWideChar:Integer):Integer;
function WideCharToMultiByte(CodePage:UINT;dwFlags:DWORD;lpWideCharStr:LPCWSTR;cchWideChar:Integer;lpMultiByteStr:LPSTR;cbMultiByte:Integer;lpDefaultChar:LPCSTR;lpUsedDefaultChar:LPBOOL):Integer;

function CompareString(Locale:LCID;dwCmpFlags:DWORD;lpString1:LPCSTR;cchCount1:Integer;lpString2:LPCSTR;cchCount2:Integer):Integer; 
function CompareStringA(Locale:LCID;dwCmpFlags:DWORD;lpString1:LPCSTR;cchCount1:Integer;lpString2:LPCSTR;cchCount2:Integer):Integer; 
function CompareStringW(Locale:LCID;dwCmpFlags:DWORD;lpString1:LPCWSTR;cchCount1:Integer;lpString2:LPCWSTR;cchCount2:Integer):Integer; 
  
function CharUpperBuff(lpsz:LPSTR;cchLength:DWORD):DWORD;
function CharUpperBuffA(lpsz:LPSTR;cchLength:DWORD):DWORD;
function CharUpperBuffW(lpsz:LPWSTR;cchLength:DWORD):DWORD;

function CharLowerBuff(lpsz:LPSTR;cchLength:DWORD):DWORD;
function CharLowerBuffA(lpsz:LPSTR;cchLength:DWORD):DWORD;
function CharLowerBuffW(lpsz:LPWSTR;cchLength:DWORD):DWORD;

function AnsiToOem(lpszSrc:LPCSTR;lpszDst:LPSTR):BOOL;
function AnsiToOemBuff(lpszSrc:LPCSTR;lpszDst:LPSTR;cchDstLength:DWORD):BOOL;
function OemToAnsi(lpszSrc:LPCSTR;lpszDst:LPSTR):BOOL;
function OemToAnsiBuff(lpszSrc:LPCSTR;lpszDst:LPSTR;cchDstLength:DWORD):BOOL;

function CharToOem(lpszSrc:LPCSTR;lpszDst:LPSTR):BOOL; 
function CharToOemA(lpszSrc:LPCSTR;lpszDst:LPSTR):BOOL; 
function CharToOemW(lpszSrc:LPCWSTR;lpszDst:LPSTR):BOOL; 

function OemToChar(lpszSrc:LPCSTR;lpszDst:LPSTR):BOOL; 
function OemToCharA(lpszSrc:LPCSTR;lpszDst:LPSTR):BOOL; 
function OemToCharW(lpszSrc:LPCSTR;lpszDst:LPWSTR):BOOL;

function CharToOemBuff(lpszSrc:LPCSTR;lpszDst:LPSTR;cchDstLength:DWORD):BOOL;
function CharToOemBuffA(lpszSrc:LPCSTR;lpszDst:LPSTR;cchDstLength:DWORD):BOOL;
function CharToOemBuffW(lpszSrc:LPCWSTR;lpszDst:LPSTR;cchDstLength:DWORD):BOOL;

function OemToCharBuff(lpszSrc:LPCSTR;lpszDst:LPSTR;cchDstLength:DWORD):BOOL;
function OemToCharBuffA(lpszSrc:LPCSTR;lpszDst:LPSTR;cchDstLength:DWORD):BOOL;
function OemToCharBuffW(lpszSrc:LPCSTR;lpszDst:LPWSTR;cchDstLength:DWORD):BOOL;

{==============================================================================}
{RTL Unicode String Manager Functions}
procedure SysWide2AnsiMove(Source:PWideChar;var Dest:RawByteString;cp:TSystemCodePage;Len:SizeInt);
procedure SysAnsi2WideMove(Source:PChar;cp:TSystemCodePage;var Dest:WideString;Len:SizeInt);
function SysUpperWideString(const S:WideString):WideString;
function SysLowerWideString(const S:WideString):WideString;

function SysCompareWideString(const s1,s2:WideString):PtrInt;
function SysCompareTextWideString(const s1,s2:WideString):PtrInt;

procedure SysUnicode2AnsiMove(Source:PUnicodeChar;var Dest:RawByteString;cp:TSystemCodePage;Len:SizeInt);
procedure SysAnsi2UnicodeMove(Source:PChar;cp:TSystemCodePage;var Dest:UnicodeString;Len:SizeInt);
function SysUpperUnicodeString(const S:UnicodeString):UnicodeString;
function SysLowerUnicodeString(const S:UnicodeString):UnicodeString;

function SysCompareUnicodeString(const s1,s2:UnicodeString):PtrInt;
function SysCompareTextUnicodeString(const s1,s2:UnicodeString):PtrInt;

//To Do

//To Do //The Win32 SysUtils unit sets the following "non unicode" functions as well, do we need to ? //Yes, things like AnsiCompareText / AnsiCompareStr etc use them
//UpperAnsiStringProc
//LowerAnsiStringProc
//CompareStrAnsiStringProc
//CompareTextAnsiStringProc

//StrCompAnsiStringProc
//StrICompAnsiStringProc
//StrLCompAnsiStringProc
//StrLICompAnsiStringProc
//StrLowerAnsiStringProc
//StrUpperAnsiStringProc

{==============================================================================}
{Unicode Helper Functions}
 
{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Unicode specific variables}
 UnicodeInitialized:Boolean;
 
 UnicodeStringManager:TUnicodeStringManager;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure UnicodeInit;
begin
 {}
 {Check Initialized}
 if UnicodeInitialized then Exit;
 
 {Get Unicode String Manager}
 GetUnicodeStringManager(UnicodeStringManager);
 
 {Initialize Unicode String Manager}
 {WideString}
 UnicodeStringManager.Wide2AnsiMoveProc:=@SysWide2AnsiMove;
 UnicodeStringManager.Ansi2WideMoveProc:=@SysAnsi2WideMove;
 UnicodeStringManager.UpperWideStringProc:=@SysUpperWideString;
 UnicodeStringManager.LowerWideStringProc:=@SysLowerWideString;
 UnicodeStringManager.CompareWideStringProc:=@SysCompareWideString;
 {UnicodeStringManager.CompareTextWideStringProc:=@SysCompareTextWideString;} {No longer required by RTL}
 {UnicodeString}
 UnicodeStringManager.Unicode2AnsiMoveProc:=@SysUnicode2AnsiMove;
 UnicodeStringManager.Ansi2UnicodeMoveProc:=@SysAnsi2UnicodeMove;
 UnicodeStringManager.UpperUnicodeStringProc:=@SysUpperUnicodeString;
 UnicodeStringManager.LowerUnicodeStringProc:=@SysLowerUnicodeString;
 UnicodeStringManager.CompareUnicodeStringProc:=@SysCompareUnicodeString;
 {UnicodeStringManager.CompareTextUnicodeStringProc:=@SysCompareTextUnicodeString;} {No longer required by RTL}
 {CodePage}
 UnicodeStringManager.GetStandardCodePageProc:=@SysGetStandardCodePage;
 
 {Set Unicode String Manager}
 SetUnicodeStringManager(UnicodeStringManager);
 
 UnicodeInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Unicode Functions}
function MultiByteToWideChar(CodePage:UINT;dwFlags:DWORD;lpMultiByteStr:LPCSTR;cbMultiByte:Integer;lpWideCharStr:LPWSTR;cchWideChar:Integer):Integer;
{SBCS/DBCS OEM or ANSI string to Unicode string}
{Note: cchWideChar is the size of lpWideCharStr in WideChars (not Bytes)}
{Note: Currently ignores the Flags parameter}
{Note: Currently only supports SBCS}
var
 Value:Word;
 Dest:LongWord;
 Count:Integer;
 Source:LongWord;

 PageID:Word;
 Page:PCodePage;
begin
 {}
 Result:=0;
 
 if lpMultiByteStr = nil then Exit;

 {Map Page}
 PageID:=MapPage(CodePage);
 
 {Get Page}
 Page:=GetPage(PageID);
 if Page = nil then Exit;

 Count:=0;
 Source:=0;
 {Check Buffser Size}
 if cchWideChar > 0 then
  begin
   if lpWideCharStr = nil then Exit;
   
   {Convert String}
   Dest:=0;
   while (Count < cbMultiByte) or (cbMultiByte = -1) do
    begin
     {Get Value}
     Value:=Page.CodeTable.Values[PByte(PtrUInt(lpMultiByteStr) + Source)^];
     
     {Set Value}
     PWord(PtrUInt(lpWideCharStr) + Dest)^:=Value;
     
     {Update Count}
     Inc(Count,1);
     
     {Check for zero}
     if PByte(PtrUInt(lpMultiByteStr) + Source)^ = 0 then Break;
     
     {Update Offset}
     Inc(Dest,2);
     Inc(Source,1);
    end;
  end
 else
  begin
   {Count String}
   while (Count < cbMultiByte) or (cbMultiByte = -1) do
    begin
     {Update Count}
     Inc(Count,1);
     
     {Check for zero}
     if PByte(PtrUInt(lpMultiByteStr) + Source)^ = 0 then Break;
     
     {Update Offset}
     Inc(Source,1);
    end;
  end;
  
 Result:=Count;
end;

{==============================================================================}

function WideCharToMultiByte(CodePage:UINT;dwFlags:DWORD;lpWideCharStr:LPCWSTR;cchWideChar:Integer;lpMultiByteStr:LPSTR;cbMultiByte:Integer;lpDefaultChar:LPCSTR;lpUsedDefaultChar:LPBOOL):Integer;
{Unicode string to SBCS/DBCS OEM or ANSI string}
{Note: cchWideChar is the size of lpWideCharStr in WideChars (not Bytes)}
{Note: Currently ignores the Flags and DefaultChar parameters}
{Note: Currently only supports SBCS}
var
 Value:Word;
 Dest:LongWord;
 Count:Integer;
 Source:LongWord;

 PageID:Word;
 Page:PCodePage;
begin
 {}
 Result:=0;
 
 if lpWideCharStr = nil then Exit;

 {Map Page}
 PageID:=MapPage(CodePage);
 
 {Get Page}
 Page:=GetPage(PageID);
 if Page = nil then Exit;

 Count:=0;
 Source:=0;
 {Check Buffser Size}
 if cbMultiByte > 0 then
  begin
   if lpMultiByteStr = nil then Exit;
   
   {Convert String}
   Dest:=0;
   while (Count < cchWideChar) or (cchWideChar = -1) do
    begin
     {Get Value}
     Value:=Page.UnicodeTable.Values[PWord(PtrUInt(lpWideCharStr) + Source)^];
     
     {Set Value}
     PByte(PtrUInt(lpMultiByteStr) + Dest)^:=Value;
     
     {Update Count}
     Inc(Count,1);
     
     {Check for zero}
     if PWord(PtrUInt(lpWideCharStr) + Source)^ = 0 then Break;
     
     {Update Offset}
     Inc(Dest,1);
     Inc(Source,2);
    end;
  end
 else
  begin
   {Count String}
   while (Count < cchWideChar) or (cchWideChar = -1) do
    begin
     {Update Count}
     Inc(Count,1);
     
     {Check for zero}
     if PWord(PtrUInt(lpWideCharStr) + Source)^ = 0 then Break;
     
     {Update Offset}
     Inc(Source,2);
    end;
  end;
  
 Result:=Count;
end;

{==============================================================================}

function CompareString(Locale:LCID;dwCmpFlags:DWORD;lpString1:LPCSTR;cchCount1:Integer;lpString2:LPCSTR;cchCount2:Integer):Integer; 
begin
 {}
 Result:=CompareStringA(Locale,dwCmpFlags,lpString1,cchCount1,lpString2,cchCount2);
end;

{==============================================================================}

function CompareStringA(Locale:LCID;dwCmpFlags:DWORD;lpString1:LPCSTR;cchCount1:Integer;lpString2:LPCSTR;cchCount2:Integer):Integer; 
begin
 {}
 Result:=0;
 
 //To Do
end;

{==============================================================================}

function CompareStringW(Locale:LCID;dwCmpFlags:DWORD;lpString1:LPCWSTR;cchCount1:Integer;lpString2:LPCWSTR;cchCount2:Integer):Integer; 
begin
 {}
 Result:=0;
 
 //To Do
end;

{==============================================================================}

function CharUpperBuff(lpsz:LPSTR;cchLength:DWORD):DWORD;
begin
 {}
 Result:=CharUpperBuffA(lpsz,cchLength);
end;

{==============================================================================}
  
function CharUpperBuffA(lpsz:LPSTR;cchLength:DWORD):DWORD;
begin
 {}
 Result:=0;
 
 //To Do
end;

{==============================================================================}

function CharUpperBuffW(lpsz:LPWSTR;cchLength:DWORD):DWORD;
begin
 {}
 Result:=0;
 
 //To Do
end;

{==============================================================================}

function CharLowerBuff(lpsz:LPSTR;cchLength:DWORD):DWORD;
begin
 {}
 Result:=CharLowerBuffA(lpsz,cchLength);
end;

{==============================================================================}

function CharLowerBuffA(lpsz:LPSTR;cchLength:DWORD):DWORD;
begin
 {}
 Result:=0;
 
 //To Do
end;

{==============================================================================}

function CharLowerBuffW(lpsz:LPWSTR;cchLength:DWORD):DWORD;
begin
 {}
 Result:=0;
 
 //To Do
end;

{==============================================================================}

function AnsiToOem(lpszSrc:LPCSTR;lpszDst:LPSTR):BOOL;
begin
 {}
 Result:=CharToOemA(lpszSrc,lpszDst);
end;

{==============================================================================}

function AnsiToOemBuff(lpszSrc:LPCSTR;lpszDst:LPSTR;cchDstLength:DWORD):BOOL;
begin
 {}
 Result:=CharToOemBuffA(lpszSrc,lpszDst,cchDstLength);
end;

{==============================================================================}

function OemToAnsi(lpszSrc:LPCSTR;lpszDst:LPSTR):BOOL;
begin
 {}
 Result:=OemToCharA(lpszSrc,lpszDst);
end;

{==============================================================================}

function OemToAnsiBuff(lpszSrc:LPCSTR;lpszDst:LPSTR;cchDstLength:DWORD):BOOL;
begin
 {}
 Result:=OemToCharBuffA(lpszSrc,lpszDst,cchDstLength);
end;

{==============================================================================}

function CharToOem(lpszSrc:LPCSTR;lpszDst:LPSTR):BOOL; 
begin
 {}
 Result:=CharToOemA(lpszSrc,lpszDst);
end;

{==============================================================================}

function CharToOemA(lpszSrc:LPCSTR;lpszDst:LPSTR):BOOL; 
{ANSI to OEM conversion (Char to Char)}
{Note: Currently only supports SBCS}
var
 Value:Word;
 Offset:LongWord;
begin
 {}
 Result:=False;
 
 if lpszSrc = nil then Exit;
 if lpszDst = nil then Exit;
 if AnsiPage = nil then Exit;

 {Translate Table Method}
 Offset:=0;
 while True do {Loop until Source equals zero}
  begin
   {Get Source}
   Value:=AnsiPage.TransTable.Values[PByte(PtrUInt(lpszSrc) + Offset)^];
   
   {Set Dest}
   PByte(PtrUInt(lpszDst) + Offset)^:=Value;
   
   {Check for zero}
   if PByte(PtrUInt(lpszSrc) + Offset)^ = 0 then Break;
   Inc(Offset,1);
  end;

 Result:=True; {Always returns True}
end;

{==============================================================================}

function CharToOemW(lpszSrc:LPCWSTR;lpszDst:LPSTR):BOOL; 
{Unicode to OEM conversion (WideChar to Char)}
{Note: Currently only supports SBCS}
var
 Value:Word;
 Dest:LongWord;
 Source:LongWord;
begin
 {}
 Result:=False;
 
 if lpszSrc = nil then Exit;
 if lpszDst = nil then Exit;
 if OemPage = nil then Exit;

 Dest:=0;
 Source:=0;
 while True do {Loop until Source equals zero}
  begin
   {Get Source}
   Value:=PWord(PtrUInt(lpszSrc) + Source)^;
   
   {Set Dest}
   PByte(PtrUInt(lpszDst) + Dest)^:=OemPage.UnicodeTable.Values[Value];
   
   {Check for zero}
   if PWord(PtrUInt(lpszSrc) + Source)^ = 0 then Break;
   Inc(Dest,1);
   Inc(Source,2);
  end;
  
 Result:=True; {Always returns True}
end;

{==============================================================================}

function OemToChar(lpszSrc:LPCSTR;lpszDst:LPSTR):BOOL; 
begin
 {}
 Result:=OemToCharA(lpszSrc,lpszDst);
end;

{==============================================================================}

function OemToCharA(lpszSrc:LPCSTR;lpszDst:LPSTR):BOOL; 
{OEM to ANSI conversion (Char to Char)}
{Note: Currently only supports SBCS}
var
 Value:Word;
 Offset:LongWord;
begin
 {}
 Result:=False;
 
 if lpszSrc = nil then Exit;
 if lpszDst = nil then Exit;
 if OemPage = nil then Exit;

 {Translate Table Method}
 Offset:=0;
 while True do {Loop until Source equals zero}
  begin
   {Get Source}
   Value:=OemPage.TransTable.Values[PByte(PtrUInt(lpszSrc) + Offset)^];
   
   {Set Dest}
   PByte(PtrUInt(lpszDst) + Offset)^:=Value;
   
   {Check for zero}
   if PByte(PtrUInt(lpszSrc) + Offset)^ = 0 then Break;
   Inc(Offset,1);
  end;

 Result:=True; {Always returns True}
end;

{==============================================================================}

function OemToCharW(lpszSrc:LPCSTR;lpszDst:LPWSTR):BOOL;
{OEM to Unicode conversion (Char to WideChar)}
{Note: Currently only supports SBCS}
var
 Value:Word;
 Dest:LongWord;
 Source:LongWord;
begin
 {}
 Result:=False;
 
 if lpszSrc = nil then Exit;
 if lpszDst = nil then Exit;
 if OemPage = nil then Exit;

 Dest:=0;
 Source:=0;
 while True do {Loop until Source equals zero}
  begin
   {Get Source}
   Value:=OemPage.CodeTable.Values[PByte(PtrUInt(lpszSrc) + Source)^];
   
   {Set Dest}
   PWord(PtrUInt(lpszDst) + Dest)^:=Value;
   
   {Check for zero}
   if PByte(PtrUInt(lpszSrc) + Source)^ = 0 then Break;
   Inc(Dest,2);
   Inc(Source,1);
  end;
  
 Result:=True; {Always returns True}
end;

{==============================================================================}

function CharToOemBuff(lpszSrc:LPCSTR;lpszDst:LPSTR;cchDstLength:DWORD):BOOL;
begin
 {}
 Result:=CharToOemBuffA(lpszSrc,lpszDst,cchDstLength);
end;

{==============================================================================}

function CharToOemBuffA(lpszSrc:LPCSTR;lpszDst:LPSTR;cchDstLength:DWORD):BOOL;
{ANSI to OEM conversion (Char to Char)}
{Note: Currently only supports SBCS}
var
 Value:Word;
 Offset:LongWord;
begin
 {}
 Result:=False;
 
 if lpszSrc = nil then Exit;
 if lpszDst = nil then Exit;
 if AnsiPage = nil then Exit;

 {Translate Table Method}
 Offset:=0;
 while Offset < cchDstLength do {Loop until Offset equals length}
  begin
   {Get Source}
   Value:=AnsiPage.TransTable.Values[PByte(PtrUInt(lpszSrc) + Offset)^];
   
   {Set Dest}
   PByte(PtrUInt(lpszDst) + Offset)^:=Value;
   Inc(Offset,1);
  end;

 Result:=True; {Always returns True}
end;

{==============================================================================}

function CharToOemBuffW(lpszSrc:LPCWSTR;lpszDst:LPSTR;cchDstLength:DWORD):BOOL;
{Unicode to OEM conversion (WideChar to Char)}
{Note: Currently only supports SBCS}
var
 Value:Word;
 Dest:LongWord;
 Count:LongWord;
 Source:LongWord;
begin
 {}
 Result:=False;
 
 if lpszSrc = nil then Exit;
 if lpszDst = nil then Exit;
 if OemPage = nil then Exit;

 Dest:=0;
 Count:=0;
 Source:=0;
 while Count < cchDstLength do {Loop until Count equals length}
  begin
   {Get Source}
   Value:=PWord(PtrUInt(lpszSrc) + Source)^;
   
   {Set Dest}
   PByte(PtrUInt(lpszDst) + Dest)^:=OemPage.UnicodeTable.Values[Value];
   Inc(Dest,1);
   Inc(Count,1);
   Inc(Source,2);
  end;
  
 Result:=True; {Always returns True}
end;

{==============================================================================}

function OemToCharBuff(lpszSrc:LPCSTR;lpszDst:LPSTR;cchDstLength:DWORD):BOOL;
begin
 {}
 Result:=OemToCharBuffA(lpszSrc,lpszDst,cchDstLength);
end;

{==============================================================================}

function OemToCharBuffA(lpszSrc:LPCSTR;lpszDst:LPSTR;cchDstLength:DWORD):BOOL;
{OEM to ANSI conversion (Char to Char)}
{Note: Currently only supports SBCS}
var
 Value:Word;
 Offset:LongWord;
begin
 {}
 Result:=False;
 
 if lpszSrc = nil then Exit;
 if lpszDst = nil then Exit;
 if OemPage = nil then Exit;

 {Translate Table Method}
 Offset:=0;
 while Offset < cchDstLength do {Loop until Offset equals length}
  begin
   {Get Source}
   Value:=OemPage.TransTable.Values[PByte(PtrUInt(lpszSrc) + Offset)^];
   
   {Set Dest}
   PByte(PtrUInt(lpszDst) + Offset)^:=Value;
   Inc(Offset,1);
  end;

 Result:=True; {Always returns True}
end;

{==============================================================================}

function OemToCharBuffW(lpszSrc:LPCSTR;lpszDst:LPWSTR;cchDstLength:DWORD):BOOL;
{OEM to Unicode conversion (Char to WideChar)}
{Note: Currently only supports SBCS}
var
 Value:Word;
 Dest:LongWord;
 Count:LongWord;
 Source:LongWord;
begin
 {}
 Result:=False;
 
 if lpszSrc = nil then Exit;
 if lpszDst = nil then Exit;
 if OemPage = nil then Exit;

 Dest:=0;
 Count:=0;
 Source:=0;
 while Count < cchDstLength do {Loop until Count equals length}
  begin
   {Get Source}
   Value:=OemPage.CodeTable.Values[PByte(PtrUInt(lpszSrc) + Source)^];
   
   {Set Dest}
   PWord(PtrUInt(lpszDst) + Dest)^:=Value;
   Inc(Dest,2);
   Inc(Count,1);
   Inc(Source,1);
  end;
  
 Result:=True; {Always returns True}
end;

{==============================================================================}
{==============================================================================}
{RTL Unicode String Manager Functions}
procedure SysWide2AnsiMove(Source:PWideChar;var Dest:RawByteString;cp:TSystemCodePage;Len:SizeInt);
var
 DestLen:SizeInt;
begin
 {}
 DestLen:=WideCharToMultiByte(cp,0,Source,Len,nil,0,nil,nil);
 SetLength(Dest,DestLen);
 if DestLen > 0 then
  begin
   WideCharToMultiByte(cp,0,Source,Len,@Dest[1],DestLen,nil,nil);
   SetCodePage(Dest,cp,False);
  end;
end;

{==============================================================================}

procedure SysAnsi2WideMove(Source:PChar;cp:TSystemCodePage;var Dest:WideString;Len:SizeInt);
var
 dwFlags:DWORD;
 DestLen:SizeInt;
begin
 {}
 if cp = CP_UTF8 then
  begin
   dwFlags:=0;
  end
 else
  begin
   dwFlags:=MB_PRECOMPOSED;
  end; 
 DestLen:=MultiByteToWideChar(cp,dwFlags,Source,Len,nil,0);
 SetLength(Dest,DestLen);
 if DestLen > 0 then
  begin
   MultiByteToWideChar(cp,dwFlags,Source,Len,@Dest[1],DestLen);
   {SetCodePage(Dest,CP_UTF16,False);} {Not required, Dest will always have the value of DefaultUnicodeCodePage (CP_UTF16)}
  end; 
end;

{==============================================================================}

function SysUpperWideString(const S:WideString):WideString;
begin
 {}
 Result:=S;
 
 //To Do
end;

{==============================================================================}

function SysLowerWideString(const S:WideString):WideString;
begin
 {}
 Result:=S;
 
 //To Do
end;

{==============================================================================}

function SysCompareWideString(const s1,s2:WideString):PtrInt;
begin
 {}
 Result:=0;
 
 //To Do
end;

{==============================================================================}

function SysCompareTextWideString(const s1,s2:WideString):PtrInt;
begin
 {}
 Result:=0;
 
 //To Do
end;

{==============================================================================}

procedure SysUnicode2AnsiMove(Source:PUnicodeChar;var Dest:RawByteString;cp:TSystemCodePage;Len:SizeInt);
var
 DestLen:SizeInt;
begin
 {}
 DestLen:=WideCharToMultiByte(cp,0,Source,Len,nil,0,nil,nil);
 SetLength(Dest,DestLen);
 if DestLen > 0 then
  begin
   WideCharToMultiByte(cp,0,Source,Len,@Dest[1],DestLen,nil,nil);
   SetCodePage(Dest,cp,False);
  end;
end;

{==============================================================================}

procedure SysAnsi2UnicodeMove(Source:PChar;cp:TSystemCodePage;var Dest:UnicodeString;Len:SizeInt);
var
 dwFlags:DWORD;
 DestLen:SizeInt;
begin
 {}
 if cp = CP_UTF8 then
  begin
   dwFlags:=0;
  end 
 else
  begin
   dwFlags:=MB_PRECOMPOSED;
  end; 
 DestLen:=MultiByteToWideChar(cp,dwFlags,Source,Len,nil,0);
 SetLength(Dest,DestLen);
 if DestLen > 0 then
  begin
   MultiByteToWideChar(cp,dwFlags,Source,Len,@Dest[1],DestLen);
   {SetCodePage(Dest,CP_UTF16,False);} {Not required, Dest will always have the value of DefaultUnicodeCodePage (CP_UTF16)}
  end;
end;

{==============================================================================}

function SysUpperUnicodeString(const S:UnicodeString):UnicodeString;
begin
 {}
 Result:=S;
 
 //To Do
end;

{==============================================================================}

function SysLowerUnicodeString(const S:UnicodeString):UnicodeString;
begin
 {}
 Result:=S;
 
 //To Do
end;

{==============================================================================}

function SysCompareUnicodeString(const s1,s2:UnicodeString):PtrInt;
begin
 {}
 Result:=0;
 
 //To Do
end;

{==============================================================================}

function SysCompareTextUnicodeString(const s1,s2:UnicodeString):PtrInt;
begin
 {}
 Result:=0;
 
 //To Do
end;

{==============================================================================}
{==============================================================================}
{Unicode Helper Functions}

{==============================================================================}
{==============================================================================}

initialization
 UnicodeInit;

 {Call SetUnicodeStringManager again because initialization of the embedded system unit (InitUnicodeStringManager) sets the default manager}
 {Get Unicode String Manager}
 GetUnicodeStringManager(UnicodeStringManager);
 
 {Initialize Unicode String Manager}
 {WideString}
 UnicodeStringManager.Wide2AnsiMoveProc:=@SysWide2AnsiMove;
 UnicodeStringManager.Ansi2WideMoveProc:=@SysAnsi2WideMove;
 UnicodeStringManager.UpperWideStringProc:=@SysUpperWideString;
 UnicodeStringManager.LowerWideStringProc:=@SysLowerWideString;
 UnicodeStringManager.CompareWideStringProc:=@SysCompareWideString;
 {UnicodeStringManager.CompareTextWideStringProc:=@SysCompareTextWideString;} {No longer required by RTL}
 {UnicodeString}
 UnicodeStringManager.Unicode2AnsiMoveProc:=@SysUnicode2AnsiMove;
 UnicodeStringManager.Ansi2UnicodeMoveProc:=@SysAnsi2UnicodeMove;
 UnicodeStringManager.UpperUnicodeStringProc:=@SysUpperUnicodeString;
 UnicodeStringManager.LowerUnicodeStringProc:=@SysLowerUnicodeString;
 UnicodeStringManager.CompareUnicodeStringProc:=@SysCompareUnicodeString;
 {UnicodeStringManager.CompareTextUnicodeStringProc:=@SysCompareTextUnicodeString;} {No longer required by RTL}
 {CodePage}
 UnicodeStringManager.GetStandardCodePageProc:=@SysGetStandardCodePage;
 
 {Set Unicode String Manager}
 SetUnicodeStringManager(UnicodeStringManager);
 
{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
