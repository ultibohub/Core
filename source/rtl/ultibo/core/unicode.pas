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

{$IFNDEF FPC_DOTTEDUNITS}
unit Unicode;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Core.GlobalConfig,
  Core.GlobalConst,
  Core.GlobalTypes,
  Core.Platform,
  Core.Locale,
  System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Locale,
  SysUtils;
{$ENDIF FPC_DOTTEDUNITS}

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
{const}
 {Unicode specific constants}

{==============================================================================}
type
 {Unicode specific types}

 {String types}
 LPSTR = PAnsiChar;
 LPCSTR = PAnsiChar;

 {Wide String types}
 LPWSTR = PWideChar;
 LPCWSTR = PWideChar;

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

function CompareString(Locale:LCID;dwCmpFlags:DWORD;lpString1:LPCSTR;cchCount1:Integer;lpString2:LPCSTR;cchCount2:Integer):Integer; inline;
function CompareStringA(Locale:LCID;dwCmpFlags:DWORD;lpString1:LPCSTR;cchCount1:Integer;lpString2:LPCSTR;cchCount2:Integer):Integer;
function CompareStringW(Locale:LCID;dwCmpFlags:DWORD;lpString1:LPCWSTR;cchCount1:Integer;lpString2:LPCWSTR;cchCount2:Integer):Integer;

function CharUpper(lpsz:LPSTR):LPSTR; inline;
function CharUpperA(lpsz:LPSTR):LPSTR;
function CharUpperW(lpsz:LPWSTR):LPWSTR;

function CharUpperBuff(lpsz:LPSTR;cchLength:DWORD):DWORD; inline;
function CharUpperBuffA(lpsz:LPSTR;cchLength:DWORD):DWORD;
function CharUpperBuffW(lpsz:LPWSTR;cchLength:DWORD):DWORD;

function CharLower(lpsz:LPSTR):LPSTR; inline;
function CharLowerA(lpsz:LPSTR):LPSTR;
function CharLowerW(lpsz:LPWSTR):LPWSTR;

function CharLowerBuff(lpsz:LPSTR;cchLength:DWORD):DWORD; inline;
function CharLowerBuffA(lpsz:LPSTR;cchLength:DWORD):DWORD;
function CharLowerBuffW(lpsz:LPWSTR;cchLength:DWORD):DWORD;

function AnsiToOem(lpszSrc:LPCSTR;lpszDst:LPSTR):BOOL; inline;
function AnsiToOemBuff(lpszSrc:LPCSTR;lpszDst:LPSTR;cchDstLength:DWORD):BOOL; inline;
function OemToAnsi(lpszSrc:LPCSTR;lpszDst:LPSTR):BOOL; inline;
function OemToAnsiBuff(lpszSrc:LPCSTR;lpszDst:LPSTR;cchDstLength:DWORD):BOOL; inline;

function CharToOem(lpszSrc:LPCSTR;lpszDst:LPSTR):BOOL; inline;
function CharToOemA(lpszSrc:LPCSTR;lpszDst:LPSTR):BOOL;
function CharToOemW(lpszSrc:LPCWSTR;lpszDst:LPSTR):BOOL;

function OemToChar(lpszSrc:LPCSTR;lpszDst:LPSTR):BOOL; inline;
function OemToCharA(lpszSrc:LPCSTR;lpszDst:LPSTR):BOOL;
function OemToCharW(lpszSrc:LPCSTR;lpszDst:LPWSTR):BOOL;

function CharToOemBuff(lpszSrc:LPCSTR;lpszDst:LPSTR;cchDstLength:DWORD):BOOL; inline;
function CharToOemBuffA(lpszSrc:LPCSTR;lpszDst:LPSTR;cchDstLength:DWORD):BOOL;
function CharToOemBuffW(lpszSrc:LPCWSTR;lpszDst:LPSTR;cchDstLength:DWORD):BOOL;

function OemToCharBuff(lpszSrc:LPCSTR;lpszDst:LPSTR;cchDstLength:DWORD):BOOL; inline;
function OemToCharBuffA(lpszSrc:LPCSTR;lpszDst:LPSTR;cchDstLength:DWORD):BOOL;
function OemToCharBuffW(lpszSrc:LPCSTR;lpszDst:LPWSTR;cchDstLength:DWORD):BOOL;

{==============================================================================}
{RTL Unicode Functions}
function SysCodePageToWideChar(Ch:Char):WideChar;
function SysWideCharToCodePage(Ch:WideChar):Char;

{==============================================================================}
{RTL Unicode String Manager Functions}
procedure SysWide2AnsiMove(Source:PWideChar;var Dest:RawByteString;cp:TSystemCodePage;Len:SizeInt);
procedure SysAnsi2WideMove(Source:PChar;cp:TSystemCodePage;var Dest:WideString;Len:SizeInt);
function SysUpperWideString(const S:WideString):WideString;
function SysLowerWideString(const S:WideString):WideString;

function SysCompareWideString(const s1,s2:WideString;Options:TCompareOptions):PtrInt;
function SysCompareTextWideString(const s1,s2:WideString):PtrInt;

procedure SysUnicode2AnsiMove(Source:PUnicodeChar;var Dest:RawByteString;cp:TSystemCodePage;Len:SizeInt);
procedure SysAnsi2UnicodeMove(Source:PChar;cp:TSystemCodePage;var Dest:UnicodeString;Len:SizeInt);
function SysUpperUnicodeString(const S:UnicodeString):UnicodeString;
function SysLowerUnicodeString(const S:UnicodeString):UnicodeString;

function SysCompareUnicodeString(const s1,s2:UnicodeString;Options:TCompareOptions):PtrInt;
function SysCompareTextUnicodeString(const s1,s2:UnicodeString):PtrInt;

function SysUpperAnsiString(const S:String):String;
function SysLowerAnsiString(const S:String):String;

function SysCompareStrAnsiString(const S1,S2:String):PtrInt;
function SysCompareTextAnsiString(const S1,S2:String):PtrInt;

function SysStrCompAnsiString(S1,S2:PChar):PtrInt;
function SysStrICompAnsiString(S1,S2:PChar):PtrInt;
function SysStrLCompAnsiString(S1,S2:PChar;MaxLen:PtrUInt):PtrInt;
function SysStrLICompAnsiString(S1,S2:PChar;MaxLen:PtrUInt):PtrInt;
function SysStrLowerAnsiString(Str:PChar):PChar;
function SysStrUpperAnsiString(Str:PChar):PChar;

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
 {AnsiString}
 UnicodeStringManager.UpperAnsiStringProc:=@SysUpperAnsiString;
 UnicodeStringManager.LowerAnsiStringProc:=@SysLowerAnsiString;
 UnicodeStringManager.CompareStrAnsiStringProc:=@SysCompareStrAnsiString;
 UnicodeStringManager.CompareTextAnsiStringProc:=@SysCompareTextAnsiString;
 UnicodeStringManager.StrCompAnsiStringProc:=@SysStrCompAnsiString;
 UnicodeStringManager.StrICompAnsiStringProc:=@SysStrICompAnsiString;
 UnicodeStringManager.StrLCompAnsiStringProc:=@SysStrLCompAnsiString;
 UnicodeStringManager.StrLICompAnsiStringProc:=@SysStrLICompAnsiString;
 UnicodeStringManager.StrLowerAnsiStringProc:=@SysStrLowerAnsiString;
 UnicodeStringManager.StrUpperAnsiStringProc:=@SysStrUpperAnsiString;
 {CodePage}
 UnicodeStringManager.GetStandardCodePageProc:=@SysGetStandardCodePage;

 {Set Unicode String Manager}
 SetUnicodeStringManager(UnicodeStringManager);

 {Setup Platform Unicode Handlers}
 CodePageToWideCharHandler:=SysCodePageToWideChar;
 WideCharToCodePageHandler:=SysWideCharToCodePage;

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

function CompareString(Locale:LCID;dwCmpFlags:DWORD;lpString1:LPCSTR;cchCount1:Integer;lpString2:LPCSTR;cchCount2:Integer):Integer; inline;
begin
 {}
 Result:=CompareStringA(Locale,dwCmpFlags,lpString1,cchCount1,lpString2,cchCount2);
end;

{==============================================================================}

function CompareStringA(Locale:LCID;dwCmpFlags:DWORD;lpString1:LPCSTR;cchCount1:Integer;lpString2:LPCSTR;cchCount2:Integer):Integer;
{ANSI compare of two strings for equivalence. If both strings are equal returns CSTR_EQUAL, if string 1 is less than string 2
 returns CSTR_LESS_THAN and if string 1 is greater than string 2 returns CSTR_GREATER_THAN, on error returns 0}
{Note: Currently the Locale value is ignored and the ANSI code page is used for comparison}
{Note: Currently only supports SBCS}

 function CompareStringALowercase(lpString:PByte):Byte; inline;
 begin
  {}
  if lpString^ in [65..90] then
   begin
    Result:=lpString^ + 32;
   end
  else
   begin
    Result:=lpString^;
   end;
 end;

var
 Offset:LongWord;
begin
 {}
 Result:=0;

 //To Do //Correct implementation for ANSI with SortTable

 {Check allocated}
 if (lpString1 = nil) or (lpString2 = nil) then
  begin
   if lpString1 <> nil then
    begin
     Result:=CSTR_GREATER_THAN;
    end
   else if lpString2 <> nil then
    begin
     Result:=CSTR_LESS_THAN;
    end
   else
    begin
     Result:=CSTR_EQUAL;
    end;

   Exit;
  end;

 if AnsiPage = nil then Exit;

 {Loop until end of string or offset equals count}
 Result:=CSTR_EQUAL;
 Offset:=0;
 while ((Offset < cchCount1) and (Offset < cchCount2)) or (cchCount1 < 0) or (cchCount2 < 0) do
  begin
   if (dwCmpFlags and NORM_IGNORECASE) = 0 then
    begin
     {Case Sensitive}
     if PByte(PtrUInt(lpString1) + Offset)^ <> PByte(PtrUInt(lpString2) + Offset)^ then
      begin
       if PByte(PtrUInt(lpString1) + Offset)^ > PByte(PtrUInt(lpString2) + Offset)^ then
        begin
         Result:=CSTR_GREATER_THAN;
        end
       else if PByte(PtrUInt(lpString1) + Offset)^ < PByte(PtrUInt(lpString2) + Offset)^ then
        begin
         Result:=CSTR_LESS_THAN;
        end;

       Exit;
      end;
    end
   else
    begin
     {Case Insensitive}
     if AnsiPage.LowerTable = nil then
      begin
       {ASCII Method}
       if PByte(PtrUInt(lpString1) + Offset)^ <> PByte(PtrUInt(lpString2) + Offset)^ then
        begin
         if CompareStringALowercase(PByte(PtrUInt(lpString1) + Offset)) <> CompareStringALowercase(PByte(PtrUInt(lpString2) + Offset)) then
          begin
           if CompareStringALowercase(PByte(PtrUInt(lpString1) + Offset)) > CompareStringALowercase(PByte(PtrUInt(lpString2) + Offset)) then
            begin
             Result:=CSTR_GREATER_THAN;
            end
           else if CompareStringALowercase(PByte(PtrUInt(lpString1) + Offset)) < CompareStringALowercase(PByte(PtrUInt(lpString2) + Offset)) then
            begin
             Result:=CSTR_LESS_THAN;
            end;

           Exit;
          end;
        end;
      end
     else
      begin
       {Lowercase Table Method}
       if AnsiPage.LowerTable.Values[PByte(PtrUInt(lpString1) + Offset)^] <> AnsiPage.LowerTable.Values[PByte(PtrUInt(lpString2) + Offset)^] then
        begin
         if AnsiPage.LowerTable.Values[PByte(PtrUInt(lpString1) + Offset)^] > AnsiPage.LowerTable.Values[PByte(PtrUInt(lpString2) + Offset)^] then
          begin
           Result:=CSTR_GREATER_THAN;
          end
         else if AnsiPage.LowerTable.Values[PByte(PtrUInt(lpString1) + Offset)^] < AnsiPage.LowerTable.Values[PByte(PtrUInt(lpString2) + Offset)^] then
          begin
           Result:=CSTR_LESS_THAN;
          end;

         Exit;
        end;
      end;
    end;

   {Check end of string 1}
   if (cchCount1 < 0) and (PByte(PtrUInt(lpString1) + Offset)^ = 0) then Exit; {Exit because both strings are at the null terminator (or else they would not be equal above)}

   {Check end of string 2}
   if (cchCount2 < 0) and (PByte(PtrUInt(lpString2) + Offset)^ = 0) then Exit; {Exit because both strings are at the null terminator (or else they would not be equal above)}

   {Update offset}
   Inc(Offset,1);

   {Check offset of string 1}
   if (cchCount1 >= 0) and (Offset >= cchCount1) then Break;

   {Check offset of string 2}
   if (cchCount2 >= 0) and (Offset >= cchCount2) then Break;
  end;

 {Check length}
 if Result = CSTR_EQUAL then
  begin
   {The offset must have reached the count value for at least one of the strings or the strings are equal}
   if ((cchCount1 >= 0) and (Offset < cchCount1)) or ((cchCount2 >= 0) and (Offset < cchCount2)) then
    begin
     if ((cchCount1 >= 0) and (Offset < cchCount1)) and ((cchCount2 >= 0) and (Offset >= cchCount2)) then
      begin
       Result:=CSTR_GREATER_THAN;
      end
     else if ((cchCount1 >= 0) and (Offset >= cchCount1)) and ((cchCount2 >= 0) and (Offset < cchCount2)) then
      begin
       Result:=CSTR_LESS_THAN;
      end;
    end;
  end;
end;

{==============================================================================}

function CompareStringW(Locale:LCID;dwCmpFlags:DWORD;lpString1:LPCWSTR;cchCount1:Integer;lpString2:LPCWSTR;cchCount2:Integer):Integer;
{Unicode compare of two strings for equivalence. If both strings are equal returns CSTR_EQUAL, if string 1 is less than string 2
 returns CSTR_LESS_THAN and if string 1 is greater than string 2 returns CSTR_GREATER_THAN, on error returns 0}
{Note: Currently the Locale value is ignored}

 function CompareStringWLowercase(lpString:PWord):Word; inline;
 begin
  {}
  if lpString^ in [65..90] then
   begin
    Result:=lpString^ + 32;
   end
  else
   begin
    Result:=lpString^;
   end;
 end;

var
 Count:LongWord;
 Offset:LongWord;
begin
 {}
 Result:=0;

 //To Do //Correct implementation for Unicode

 {Check allocated}
 if (lpString1 = nil) or (lpString2 = nil) then
  begin
   if lpString1 <> nil then
    begin
     Result:=CSTR_GREATER_THAN;
    end
   else if lpString2 <> nil then
    begin
     Result:=CSTR_LESS_THAN;
    end
   else
    begin
     Result:=CSTR_EQUAL;
    end;

   Exit;
  end;

 {Loop until end of string or offset equals count}
 Result:=CSTR_EQUAL;
 Count:=0;
 Offset:=0;
 while ((Count < cchCount1) and (Count < cchCount2)) or (cchCount1 < 0) or (cchCount2 < 0) do
  begin
   if (dwCmpFlags and NORM_IGNORECASE) = 0 then
    begin
     {Case Sensitive}
     if PWord(PtrUInt(lpString1) + Offset)^ <> PWord(PtrUInt(lpString2) + Offset)^ then
      begin
       if PWord(PtrUInt(lpString1) + Offset)^ > PWord(PtrUInt(lpString2) + Offset)^ then
        begin
         Result:=CSTR_GREATER_THAN;
        end
       else if PWord(PtrUInt(lpString1) + Offset)^ < PWord(PtrUInt(lpString2) + Offset)^ then
        begin
         Result:=CSTR_LESS_THAN;
        end;

       Exit;
      end;
    end
   else
    begin
     {Case Insensitive}
     if PWord(PtrUInt(lpString1) + Offset)^ <> PWord(PtrUInt(lpString2) + Offset)^ then
      begin
       if CompareStringWLowercase(PWord(PtrUInt(lpString1) + Offset)) <> CompareStringWLowercase(PWord(PtrUInt(lpString2) + Offset)) then
        begin
         if CompareStringWLowercase(PWord(PtrUInt(lpString1) + Offset)) > CompareStringWLowercase(PWord(PtrUInt(lpString2) + Offset)) then
          begin
           Result:=CSTR_GREATER_THAN;
          end
         else if CompareStringWLowercase(PWord(PtrUInt(lpString1) + Offset)) < CompareStringWLowercase(PWord(PtrUInt(lpString2) + Offset)) then
          begin
           Result:=CSTR_LESS_THAN;
          end;

         Exit;
        end;
      end;
    end;

   {Check end of string 1}
   if (cchCount1 < 0) and (PWord(PtrUInt(lpString1) + Offset)^ = 0) then Exit; {Exit because both strings are at the null terminator (or else they would not be equal above)}

   {Check end of string 2}
   if (cchCount2 < 0) and (PWord(PtrUInt(lpString2) + Offset)^ = 0) then Exit; {Exit because both strings are at the null terminator (or else they would not be equal above)}

   {Update count}
   Inc(Count,1);
   Inc(Offset,2);

   {Check count of string 1}
   if (cchCount1 >= 0) and (Count >= cchCount1) then Break;

   {Check count of string 2}
   if (cchCount2 >= 0) and (Count >= cchCount2) then Break;
  end;

 {Check length}
 if Result = CSTR_EQUAL then
  begin
   {The count must have reached the count value for at least one of the strings or the strings are equal}
   if ((cchCount1 >= 0) and (Count < cchCount1)) or ((cchCount2 >= 0) and (Count < cchCount2)) then
    begin
     if ((cchCount1 >= 0) and (Count < cchCount1)) and ((cchCount2 >= 0) and (Count >= cchCount2)) then
      begin
       Result:=CSTR_GREATER_THAN;
      end
     else if ((cchCount1 >= 0) and (Count >= cchCount1)) and ((cchCount2 >= 0) and (Count < cchCount2)) then
      begin
       Result:=CSTR_LESS_THAN;
      end;
    end;
  end;
end;

{==============================================================================}

function CharUpper(lpsz:LPSTR):LPSTR; inline;
begin
 {}
 Result:=CharUpperA(lpsz);
end;

{==============================================================================}

function CharUpperA(lpsz:LPSTR):LPSTR;
{Lower to Upper case conversion in ANSI code page (Char)}
{Note: Unlike Windows this function does not differentiate a single character by
 the high order word of the passed pointer. To convert a single character call
 CharUpperBuffA instead with the length as 1.}
{Note: Currently only supports SBCS}
var
 Offset:LongWord;
begin
 {}
 Result:=lpsz;

 if lpsz = nil then Exit;
 if AnsiPage = nil then Exit;

 {Check Uppercase Table}
 if AnsiPage.UpperTable = nil then
  begin
   {ASCII Method}
   Offset:=0;
   while True do {Loop until string equals zero}
    begin
     {Check value}
     if PByte(PtrUInt(lpsz) + Offset)^ in [97..122] then
      begin
       {Uppercase value}
       PByte(PtrUInt(lpsz) + Offset)^:=PByte(PtrUInt(lpsz) + Offset)^ - 32;
      end;

     {Check for zero}
     if PByte(PtrUInt(lpsz) + Offset)^ = 0 then Break;
     Inc(Offset,1);
    end;
  end
 else
  begin
   {Uppercase Table Method}
   Offset:=0;
   while True do {Loop until string equals zero}
    begin
     {Uppercase value}
     PByte(PtrUInt(lpsz) + Offset)^:=AnsiPage.UpperTable.Values[PByte(PtrUInt(lpsz) + Offset)^];

     {Check for zero}
     if PByte(PtrUInt(lpsz) + Offset)^ = 0 then Break;
     Inc(Offset,1);
    end;
  end;
end;

{==============================================================================}

function CharUpperW(lpsz:LPWSTR):LPWSTR;
{Lower to Upper case conversion in Unicode (WideChar)}
{Note: Unlike Windows this function does not differentiate a single character by
 the high order word of the passed pointer. To convert a single character call
 CharUpperBuffW instead with the length as 1.}
var
 Offset:LongWord;
begin
 {}
 Result:=lpsz;

 if lpsz = nil then Exit;

 {ASCII Method} //To Do //Correct implementation for Unicode
 Offset:=0;
 while True do {Loop until string equals zero}
  begin
   {Check value}
   if PWord(PtrUInt(lpsz) + Offset)^ in [97..122] then
    begin
     {Uppercase value}
     PWord(PtrUInt(lpsz) + Offset)^:=PWord(PtrUInt(lpsz) + Offset)^ - 32;
    end;

   {Check for zero}
   if PWord(PtrUInt(lpsz) + Offset)^ = 0 then Break;
   Inc(Offset,2);
  end;
end;

{==============================================================================}

function CharUpperBuff(lpsz:LPSTR;cchLength:DWORD):DWORD; inline;
begin
 {}
 Result:=CharUpperBuffA(lpsz,cchLength);
end;

{==============================================================================}

function CharUpperBuffA(lpsz:LPSTR;cchLength:DWORD):DWORD;
{Lower to Upper case conversion in ANSI code page (Char)}
{Note: Currently only supports SBCS}
var
 Count:LongWord;
 Offset:LongWord;
begin
 {}
 Result:=0;

 if lpsz = nil then Exit;
 if AnsiPage = nil then Exit;

 {Check Uppercase Table}
 if AnsiPage.UpperTable = nil then
  begin
   {ASCII Method}
   Count:=0;
   Offset:=0;
   while Offset < cchLength do {Loop until Offset equals length}
    begin
     {Check value}
     if PByte(PtrUInt(lpsz) + Offset)^ in [97..122] then
      begin
       {Uppercase value}
       PByte(PtrUInt(lpsz) + Offset)^:=PByte(PtrUInt(lpsz) + Offset)^ - 32;
      end;

     Inc(Count,1);
     Inc(Offset,1);
    end;

   Result:=Count;
  end
 else
  begin
   {Uppercase Table Method}
   Count:=0;
   Offset:=0;
   while Offset < cchLength do {Loop until Offset equals length}
    begin
     {Uppercase value}
     PByte(PtrUInt(lpsz) + Offset)^:=AnsiPage.UpperTable.Values[PByte(PtrUInt(lpsz) + Offset)^];

     Inc(Count,1);
     Inc(Offset,1);
    end;

   Result:=Count;
  end;
end;

{==============================================================================}

function CharUpperBuffW(lpsz:LPWSTR;cchLength:DWORD):DWORD;
{Lower to Upper case conversion in Unicode (WideChar)}
var
 Count:LongWord;
 Offset:LongWord;
begin
 {}
 Result:=0;

 if lpsz = nil then Exit;

 {ASCII Method} //To Do //Correct implementation for Unicode
 Count:=0;
 Offset:=0;
 while Count < cchLength do {Loop until Count equals length}
  begin
   {Check value}
   if PWord(PtrUInt(lpsz) + Offset)^ in [97..122] then
    begin
     {Uppercase value}
     PWord(PtrUInt(lpsz) + Offset)^:=PWord(PtrUInt(lpsz) + Offset)^ - 32;
    end;

   Inc(Count,1);
   Inc(Offset,2);
  end;

 Result:=Count;
end;

{==============================================================================}

function CharLower(lpsz:LPSTR):LPSTR; inline;
begin
 {}
 Result:=CharLowerA(lpsz);
end;

{==============================================================================}

function CharLowerA(lpsz:LPSTR):LPSTR;
{Upper to Lower case conversion in ANSI code page (Char)}
{Note: Unlike Windows this function does not differentiate a single character by
 the high order word of the passed pointer. To convert a single character call
 CharLowerBuffA instead with the length as 1.}
{Note: Currently only supports SBCS}
var
 Offset:LongWord;
begin
 {}
 Result:=lpsz;

 if lpsz = nil then Exit;
 if AnsiPage = nil then Exit;

 {Check Lowercase Table}
 if AnsiPage.LowerTable = nil then
  begin
   {ASCII Method}
   Offset:=0;
   while True do {Loop until string equals zero}
    begin
     {Check value}
     if PByte(PtrUInt(lpsz) + Offset)^ in [65..90] then
      begin
       {Lowercase value}
       PByte(PtrUInt(lpsz) + Offset)^:=PByte(PtrUInt(lpsz) + Offset)^ + 32;
      end;

     {Check for zero}
     if PByte(PtrUInt(lpsz) + Offset)^ = 0 then Break;
     Inc(Offset,1);
    end;
  end
 else
  begin
   {Lowercase Table Method}
   Offset:=0;
   while True do {Loop until string equals zero}
    begin
     {Lowercase value}
     PByte(PtrUInt(lpsz) + Offset)^:=AnsiPage.LowerTable.Values[PByte(PtrUInt(lpsz) + Offset)^];

     {Check for zero}
     if PByte(PtrUInt(lpsz) + Offset)^ = 0 then Break;
     Inc(Offset,1);
    end;
  end;
end;

{==============================================================================}

function CharLowerW(lpsz:LPWSTR):LPWSTR;
{Upper to Lower case conversion in Unicode (WideChar)}
{Note: Unlike Windows this function does not differentiate a single character by
 the high order word of the passed pointer. To convert a single character call
 CharLowerBuffW instead with the length as 1.}
var
 Offset:LongWord;
begin
 {}
 Result:=lpsz;

 if lpsz = nil then Exit;

 {ASCII Method} //To Do //Correct implementation for Unicode
 Offset:=0;
 while True do {Loop until string equals zero}
  begin
   {Check value}
   if PWord(PtrUInt(lpsz) + Offset)^ in [65..90] then
    begin
     {Lowercase value}
     PWord(PtrUInt(lpsz) + Offset)^:=PWord(PtrUInt(lpsz) + Offset)^ + 32;
    end;

   {Check for zero}
   if PWord(PtrUInt(lpsz) + Offset)^ = 0 then Break;
   Inc(Offset,2);
  end;
end;

{==============================================================================}

function CharLowerBuff(lpsz:LPSTR;cchLength:DWORD):DWORD; inline;
begin
 {}
 Result:=CharLowerBuffA(lpsz,cchLength);
end;

{==============================================================================}

function CharLowerBuffA(lpsz:LPSTR;cchLength:DWORD):DWORD;
{Upper to Lower case conversion in ANSI code page (Char)}
{Note: Currently only supports SBCS}
var
 Count:LongWord;
 Offset:LongWord;
begin
 {}
 Result:=0;

 if lpsz = nil then Exit;
 if AnsiPage = nil then Exit;

 {Check Lowercase Table}
 if AnsiPage.LowerTable = nil then
  begin
   {ASCII Method}
   Count:=0;
   Offset:=0;
   while Offset < cchLength do {Loop until Offset equals length}
    begin
     {Check value}
     if PByte(PtrUInt(lpsz) + Offset)^ in [65..90] then
      begin
       {Lowercase value}
       PByte(PtrUInt(lpsz) + Offset)^:=PByte(PtrUInt(lpsz) + Offset)^ + 32;
      end;

     Inc(Count,1);
     Inc(Offset,1);
    end;

   Result:=Count;
  end
 else
  begin
   {Lowercase Table Method}
   Count:=0;
   Offset:=0;
   while Offset < cchLength do {Loop until Offset equals length}
    begin
     {Lowercase value}
     PByte(PtrUInt(lpsz) + Offset)^:=AnsiPage.LowerTable.Values[PByte(PtrUInt(lpsz) + Offset)^];

     Inc(Count,1);
     Inc(Offset,1);
    end;

   Result:=Count;
  end;
end;

{==============================================================================}

function CharLowerBuffW(lpsz:LPWSTR;cchLength:DWORD):DWORD;
{Upper to Lower case conversion in Unicode (WideChar)}
var
 Count:LongWord;
 Offset:LongWord;
begin
 {}
 Result:=0;

 if lpsz = nil then Exit;

 {ASCII Method} //To Do //Correct implementation for Unicode
 Count:=0;
 Offset:=0;
 while Count < cchLength do {Loop until Count equals length}
  begin
   {Check value}
   if PWord(PtrUInt(lpsz) + Offset)^ in [65..90] then
    begin
     {Lowercase value}
     PWord(PtrUInt(lpsz) + Offset)^:=PWord(PtrUInt(lpsz) + Offset)^ + 32;
    end;

   Inc(Count,1);
   Inc(Offset,2);
  end;

 Result:=Count;
end;

{==============================================================================}

function AnsiToOem(lpszSrc:LPCSTR;lpszDst:LPSTR):BOOL; inline;
begin
 {}
 Result:=CharToOemA(lpszSrc,lpszDst);
end;

{==============================================================================}

function AnsiToOemBuff(lpszSrc:LPCSTR;lpszDst:LPSTR;cchDstLength:DWORD):BOOL; inline;
begin
 {}
 Result:=CharToOemBuffA(lpszSrc,lpszDst,cchDstLength);
end;

{==============================================================================}

function OemToAnsi(lpszSrc:LPCSTR;lpszDst:LPSTR):BOOL; inline;
begin
 {}
 Result:=OemToCharA(lpszSrc,lpszDst);
end;

{==============================================================================}

function OemToAnsiBuff(lpszSrc:LPCSTR;lpszDst:LPSTR;cchDstLength:DWORD):BOOL; inline;
begin
 {}
 Result:=OemToCharBuffA(lpszSrc,lpszDst,cchDstLength);
end;

{==============================================================================}

function CharToOem(lpszSrc:LPCSTR;lpszDst:LPSTR):BOOL; inline;
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

function OemToChar(lpszSrc:LPCSTR;lpszDst:LPSTR):BOOL; inline;
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

function CharToOemBuff(lpszSrc:LPCSTR;lpszDst:LPSTR;cchDstLength:DWORD):BOOL; inline;
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

function OemToCharBuff(lpszSrc:LPCSTR;lpszDst:LPSTR;cchDstLength:DWORD):BOOL; inline;
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
{RTL Unicode Functions}
function SysCodePageToWideChar(Ch:Char):WideChar;
{ANSI to Unicode conversion (Char to WideChar)}
{Note: Currently only supports SBCS}
begin
 {}
 Result:=#0;

 if AnsiPage = nil then Exit;

 Result:=WideChar(AnsiPage.CodeTable.Values[Byte(Ch)]);
end;

{==============================================================================}

function SysWideCharToCodePage(Ch:WideChar):Char;
{Unicode to ANSI conversion (WideChar to Char)}
{Note: Currently only supports SBCS}
begin
 {}
 Result:=#0;

 if AnsiPage = nil then Exit;

 Result:=Char(AnsiPage.UnicodeTable.Values[Word(Ch)]);
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
 if Length(Result) > 0 then
  begin
   CharUpperBuffW(LPWSTR(Result),Length(Result));
  end;
end;

{==============================================================================}

function SysLowerWideString(const S:WideString):WideString;
begin
 {}
 Result:=S;
 if Length(Result) > 0 then
  begin
   CharLowerBuffW(LPWSTR(Result),Length(Result));
  end;
end;

{==============================================================================}

function SysCompareWideString(const s1,s2:WideString;Options:TCompareOptions):PtrInt;
var
 Flags:DWORD;
begin
 {}
 Flags:=0;
 if coIgnoreCase in Options then Flags:=Flags or NORM_IGNORECASE;

 Result:=CompareStringW(LOCALE_USER_DEFAULT,Flags,PWideChar(s1),Length(s1),PWideChar(s2),Length(s2)) - 2;
end;

{==============================================================================}

function SysCompareTextWideString(const s1,s2:WideString):PtrInt;
begin
 {Not Used}
 Result:=0;
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
 UniqueString(Result); {Unicode string is reference counted}
 if Length(Result) > 0 then
  begin
   CharUpperBuffW(LPWSTR(Result),Length(Result));
  end;
end;

{==============================================================================}

function SysLowerUnicodeString(const S:UnicodeString):UnicodeString;
begin
 {}
 Result:=S;
 UniqueString(Result); {Unicode string is reference counted}
 if Length(Result) > 0 then
  begin
   CharLowerBuffW(LPWSTR(Result),Length(Result));
  end;
end;

{==============================================================================}

function SysCompareUnicodeString(const s1,s2:UnicodeString;Options:TCompareOptions):PtrInt;
var
 Flags:DWORD;
begin
 {}
 Flags:=0;
 if coIgnoreCase in Options then Flags:=Flags or NORM_IGNORECASE;

 Result:=CompareStringW(LOCALE_USER_DEFAULT,Flags,PWideChar(s1),Length(s1),PWideChar(s2),Length(s2)) - 2;
end;

{==============================================================================}

function SysCompareTextUnicodeString(const s1,s2:UnicodeString):PtrInt;
begin
 {Not Used}
 Result:=0;
end;

{==============================================================================}

function SysUpperAnsiString(const S:String):String;
begin
 {}
 if Length(S) > 0 then
  begin
   Result:=S;
   UniqueString(Result); {ANSI string is reference counted}
   CharUpperBuffA(PChar(Result),Length(Result));
  end
 else
  begin
   Result:='';
  end;
end;

{==============================================================================}

function SysLowerAnsiString(const S:String):String;
begin
 {}
 if Length(S) > 0 then
  begin
   Result:=S;
   UniqueString(Result); {ANSI string is reference counted}
   CharLowerBuffA(PChar(Result),Length(Result));
  end
 else
  begin
   Result:='';
  end;
end;

{==============================================================================}

function SysCompareStrAnsiString(const S1,S2:String):PtrInt;
begin
 {}
 Result:=CompareStringA(LOCALE_USER_DEFAULT,0,PChar(S1),Length(S1),PChar(S2),Length(S2)) - 2;
end;

{==============================================================================}

function SysCompareTextAnsiString(const S1,S2:String):PtrInt;
begin
 {}
 Result:=CompareStringA(LOCALE_USER_DEFAULT,NORM_IGNORECASE,PChar(S1),Length(S1),PChar(S2),Length(S2)) - 2;
end;

{==============================================================================}

function SysStrCompAnsiString(S1,S2:PChar):PtrInt;
begin
 {}
 Result:=CompareStringA(LOCALE_USER_DEFAULT,0,S1,-1,S2,-1) - 2;
end;

{==============================================================================}

function SysStrICompAnsiString(S1,S2:PChar):PtrInt;
begin
 {}
 Result:=CompareStringA(LOCALE_USER_DEFAULT,NORM_IGNORECASE,S1,-1,S2,-1) - 2;
end;

{==============================================================================}

function SysStrLCompAnsiString(S1,S2:PChar;MaxLen:PtrUInt):PtrInt;
begin
 {}
 Result:=CompareStringA(LOCALE_USER_DEFAULT,0,S1,MaxLen,S2,MaxLen) - 2;
end;

{==============================================================================}

function SysStrLICompAnsiString(S1,S2:PChar;MaxLen:PtrUInt):PtrInt;
begin
 {}
 Result:=CompareStringA(LOCALE_USER_DEFAULT,NORM_IGNORECASE,S1,MaxLen,S2,MaxLen) - 2;
end;

{==============================================================================}

function SysStrLowerAnsiString(Str:PChar):PChar;
begin
 {}
 CharLowerA(Str);
 Result:=Str;
end;

{==============================================================================}

function SysStrUpperAnsiString(Str:PChar):PChar;
begin
 {}
 CharUpperA(Str);
 Result:=Str;
end;

{==============================================================================}
{==============================================================================}
{Unicode Helper Functions}

{==============================================================================}
{==============================================================================}

initialization
 UnicodeInit;

 {Call SetUnicodeStringManager again because initialization of the system unit (InitUnicodeStringManager) sets the default manager}
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
 {AnsiString}
 UnicodeStringManager.UpperAnsiStringProc:=@SysUpperAnsiString;
 UnicodeStringManager.LowerAnsiStringProc:=@SysLowerAnsiString;
 UnicodeStringManager.CompareStrAnsiStringProc:=@SysCompareStrAnsiString;
 UnicodeStringManager.CompareTextAnsiStringProc:=@SysCompareTextAnsiString;
 UnicodeStringManager.StrCompAnsiStringProc:=@SysStrCompAnsiString;
 UnicodeStringManager.StrICompAnsiStringProc:=@SysStrICompAnsiString;
 UnicodeStringManager.StrLCompAnsiStringProc:=@SysStrLCompAnsiString;
 UnicodeStringManager.StrLICompAnsiStringProc:=@SysStrLICompAnsiString;
 UnicodeStringManager.StrLowerAnsiStringProc:=@SysStrLowerAnsiString;
 UnicodeStringManager.StrUpperAnsiStringProc:=@SysStrUpperAnsiString;
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
