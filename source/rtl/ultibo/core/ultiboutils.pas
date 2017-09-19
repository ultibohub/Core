{
Ultibo utils unit.

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


Ultibo Utils
============

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit UltiboUtils;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Threads,Unicode,Security,Ultibo,SysUtils,Classes;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
{Ultibo Util constants}
const
 {BoolToStr Constants}
 BOOLSTR_TRUEFALSE = 0;
 BOOLSTR_YESNO = 1;
 BOOLSTR_ZEROONE = 2;
  
const
 {Email Constants}
 InvalidEmailChars:set of char = [' ','!','"','#','$','%','(',')',',','/',':',';','<','>','[',']','\','`','|'];
  
{==============================================================================}
{Ultibo Util types}
{type}

{==============================================================================}
{Ultibo Util variables}
{var}

{==============================================================================}
{Initialization Functions}

{==============================================================================}
{String Functions} 
function GetFirstWord(var AValue:String;const ADelimiter:String):String;
function GetLastWord(var AValue:String;const ADelimiter:String):String;

function PadString(const AValue:String;AChar:Char;ALength:Integer):String;
function StrOfChar(const AValue:String;ALength:Integer):String;
function StrToBool(const AValue:String):Boolean;
function IntToStrLen(AValue:Integer;ADigits:Integer):String;
function StrToLongWord(const AValue:String):LongWord;
function BoolToStr(AValue:Boolean):String;
function BoolToStrEx(AValue:Boolean;AType:Integer):String;
function InFixStr(const Value:String):String;
function InFixStrEx(const Value:String):String;

function RemoveSpaces(const Text:String):String;
function SpaceCount(const Text:String):Integer;
function NormaliseSpaces(const Text:String):String;
function NormaliseCase(const Text:String):String;
function StringFill(const Text:String;Count:Integer):String;

function RemoveQuotes(const AValue:String):String;
function RemoveBraces(const AValue:String):String;
function RemoveBrackets(const AValue:String):String;
function RemoveSingleQuotes(const AValue:String):String;
function RemoveCurlyBraces(const AValue:String):String;

function IsAlpha(const AValue:String):Boolean;
function IsNumeric(const AValue:String):Boolean;

function RemoveNonAlpha(const Value:String):String;

function AddQuotes(const AValue:String):String;
function AddBraces(const AValue:String):String;
function AddBrackets(const AValue:String):String;
function AddSingleQuotes(const AValue:String):String;
function AddCurlyBraces(const AValue:String):String;

function NoOfChars(const AValue:String;AChar:Char):Integer;
function ReplaceChar(const AValue:String;AChar,AReplace:Char):String;

function EncodeString(const AValue,AKey:String):String;
function DecodeString(const AValue,AKey:String):String;

procedure DelimitStrings(AStrings:TStrings;var AString:String;const ADelimiter:String);
procedure UndelimitString(const AString:String;AStrings:TStrings;const ADelimiter:String);

procedure UndelimitQuotedString(const AString:String;AStrings:TStrings;const ADelimiter:String);

function AddSlashes(const AFilePath:String):String;
function AddTrailingSlash(const AFilePath:String):String;
function AddLeadingSlash(const AFilePath:String):String;

function StripSlashes(const AFilePath:String):String;
function StripTrailingSlash(const AFilePath:String):String;
function StripLeadingSlash(const AFilePath:String):String;

function AddTrailingDot(const AValue:String):String;
function AddLeadingDot(const AValue:String):String;
function StripTrailingDot(const AValue:String):String; 
function StripLeadingDot(const AValue:String):String; 

function AddTrailingChar(const AFilePath,ASlashChar:String):String;
function AddLeadingChar(const AFilePath,ASlashChar:String):String; 
function StripTrailingChar(const AFilePath,ASlashChar:String):String;
function StripLeadingChar(const AFilePath,ASlashChar:String):String; 

function IsWildcard(const Value:String):Boolean;
function UniqueName(const BaseName:String;CurrentNames:TStrings):String;
function WildcardNameMatch(const Name,Wildcard:String;CaseSensitive:Boolean):Boolean;

function AddQuotesIfSpaced(const AValue:String):String;
function ExtractCommand(const ACommandLine:String):String;
function ExtractParameters(const ACommandLine:String):String;

function AllocateCommandLine(const ACommandLine:String;out AArgC:Integer):PPChar;
procedure ReleaseCommandLine(AArgV:PPChar);

function MultiStringToStrings(ABuffer:Pointer;ASize:Integer;AStrings:TStrings):Boolean;
function StringsToMultiString(AStrings:TStrings;var ABuffer:Pointer;var ASize:Integer):Boolean;

function MultiStringToDelimited(ABuffer:PChar;ADelimiter:String):String;
function DelimitedToMultiString(const AString:String;ADelimiter:String):PChar;

function DuplicateString(AString:PChar):PChar;
function DuplicateWideString(AString:PWideChar):PWideChar;

function DuplicateMultiString(AString:PChar):PChar;
function DuplicateMultiWideString(AString:PWideChar):PWideChar;

{==============================================================================}
{File Functions} 
function IsRootDirectory(const Path:String):Boolean;
function IsEightDotThree(const FileName:String):Boolean;
function TruncateLongName(const FileName:String):String;
function GenerateShortName(const FileName:String;AliasCount:Integer):String;
function GenerateShortNameEx(const FileName:String;AliasCount:Integer;Hash:Word;UseHash:Boolean):String;
function WildcardFileMatch(const FileName,Wildcard:String;CaseSensitive:Boolean):Boolean;

function GetFileInfo(const FileName:String;var FileTime,FileSize:Integer;var FileAttr:LongWord):Boolean;
function CompareFileInfo(const SourceFile,DestFile:String):Boolean;

{==============================================================================}
{Date Functions} 
function IsCurrentDate(ADate:TDateTime):Boolean;
function IsCurrentTime(ATime:TDateTime):Boolean;
function IsCurrentMinute(AMinute:Integer):Boolean;
function IsCurrentHour(AHour:Integer):Boolean;
function IsCurrentWeekDay(AWeekDay:Integer):Boolean;
function IsCurrentDay(ADay:Integer):Boolean;
function IsCurrentMonth(AMonth:Integer):Boolean;
function IsCurrentYear(AYear:Integer):Boolean;
function GetMinute(ADateTime:TDateTime):Integer;
function GetHour(ADateTime:TDateTime):Integer;
function GetWeekday(ADateTime:TDateTime):Integer;
function GetDay(ADateTime:TDateTime):Integer;
function GetMonth(ADateTime:TDateTime):Integer;
function GetYear(ADateTime:TDateTime):Integer;
function SecondsSince(ADateTime:TDateTime):LongWord;
function MinutesSince(ADateTime:TDateTime):LongWord;
function HoursSince(ADateTime:TDateTime):LongWord;
function DaysSince(ADateTime:TDateTime):LongWord;
function MonthsSince(ADateTime:TDateTime):LongWord;
function YearsSince(ADateTime:TDateTime):LongWord;
function DayOfYear(ADateTime:TDateTime):LongWord;

function SubtractSeconds(ADateTime:TDateTime;ASeconds:LongWord):TDateTime;

function TickCount:LongWord;
function NormaliseDate(const ADate:String):String;

function FormatEmailDate(ADateTime:TDateTime):String;

function ShortMonthToMonth(const AShortMonth:String):LongWord;
function LongMonthToMonth(const ALongMonth:String):LongWord;

function TwoDigitYearToYear(AYear:LongWord):LongWord;

{==============================================================================}
{Time Functions} 

{==============================================================================}
{Numeric Functions} 

{==============================================================================}
{Network Functions} 
function CheckEmailAddress(const EmailAddress:String):Boolean;
function StripEmailAddress(const EmailAddress:String):String;
function FormatEmailAddress(const FullName,EmailAddress:String):String;

function GetUserFromEmail(const EmailAddress:String):String;
function GetDomainFromEmail(const EmailAddress:String):String;

{==============================================================================}
{Misc Functions} 
function CloneSid(ASid:Pointer):Pointer;
function CloneDescriptor(ADescriptor:Pointer):Pointer;

function CompareSid(ASid1,ASid2:Pointer):Boolean;
function CompareDescriptor(ADescriptor1,ADescriptor2:Pointer):Boolean;

function GenerateSidHash(ASid:Pointer;ASize:LongWord):LongWord;
function GenerateDescriptorHash(ADescriptor:Pointer;ASize:LongWord):LongWord;

{==============================================================================}
{Helper Functions} 

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}

{==============================================================================}
{==============================================================================}
{String Functions} 
function GetFirstWord(var AValue:String;const ADelimiter:String):String;
var
 PosIdx:Integer;
begin
 {}
 PosIdx:=Pos(ADelimiter,AValue);
 if PosIdx = 0 then
  begin
   Result:=AValue;
   AValue:='';
  end
 else
  begin	
   Result:=Copy(AValue,1,PosIdx - 1);
   Delete(AValue,1,PosIdx + (Length(ADelimiter) - 1));		
  end;
end;

{==============================================================================}

function GetLastWord(var AValue:String;const ADelimiter:String):String;
begin
 {}
 Result:=''; //To Do //RPos ? //See: \source\packages\rtl-objpas\src\inc\strutils.pp
end;

{==============================================================================}

function PadString(const AValue:String;AChar:Char;ALength:Integer):String;
{System StringOfChar may not handle Length < 0 correctly}
begin
 {}
 Result:=AValue;
 
 if ALength <= 0 then Exit;
 if Length(AValue) >= ALength then Exit;
 
 Result:=Result + StringOfChar(AChar,ALength - Length(AValue));
end;

{==============================================================================}

function StrOfChar(const AValue:String;ALength:Integer):String;
{Use System StringOfChar instead}
begin
 {}
 Result:='';
 
 while Length(Result) < ALength do
  begin
   Result:=Result + AValue;
  end;
end;

{==============================================================================}

function StrToBool(const AValue:String):Boolean;
begin
 {}
 Result:=False;
 
 if Trim(AValue) = '' then Exit;
 
 if Uppercase(Trim(AValue)) = 'TRUE' then
  begin
   Result:=True;
   Exit;
  end;
 if Uppercase(Trim(AValue)) = 'YES' then
  begin
   Result:=True;
   Exit;
  end;
 if Uppercase(Trim(AValue)) = '1' then
  begin
   Result:=True;
   Exit;
  end;
end;

{==============================================================================}

function IntToStrLen(AValue:Integer;ADigits:Integer):String;
begin
 {}
 if AValue >= 0 then
  begin
   Result:=IntToStr(AValue);
   while Length(Result) < ADigits do
    begin
     Result:='0' + Result;
    end;
  end
 else
  begin
   Result:=IntToStr(AValue);
   Result:=Copy(Result,2,Length(Result));
   while Length(Result) < ADigits do
    begin
     Result:='0' + Result;
    end;
    
   Result:='-' + Result;
  end;
end;

{==============================================================================}

function StrToLongWord(const AValue:String):LongWord;
begin
 {}
 Result:=StrToInt(AValue);
end;

{==============================================================================}

function BoolToStr(AValue:Boolean):String;
begin
 {}
 Result:=BoolToStrEx(AValue,BOOLSTR_TRUEFALSE);
end;

{==============================================================================}

function BoolToStrEx(AValue:Boolean;AType:Integer):String;
begin
 {}
 case AType of
  BOOLSTR_YESNO:begin
    Result:='No';
    if AValue then Result:='Yes';
   end;
  BOOLSTR_ZEROONE:begin
    Result:='0';
    if AValue then Result:='1';
   end;
  else
   begin
    {Default to BOOLSTR_TRUEFALSE}
    Result:='False';
    if AValue then Result:='True';
   end;
 end;
end;

{==============================================================================}

function InFixStr(const Value:String):String;
begin
 {}
 Result:=Trim(Value);
 
 if Length(Result) > 0 then
  begin
   if Length(Result) > 1 then
    begin
     Result:=Uppercase(Copy(Result,1,1)) + Lowercase(Copy(Result,2,Length(Result)));
    end
   else
    begin
     Result:=Uppercase(Result);
    end;
  end;
end;

{==============================================================================}

function InFixStrEx(const Value:String):String;
var
 Count:Integer;
begin
 {}
 Result:=Lowercase(Value);
 if Length(Result) > 0 then
  begin
   for Count:=1 to Length(Result) do
    begin
     if Count = 1 then
      begin
       {First Letter}
       Result[Count]:=Upcase(Result[Count]);
      end
     else if Result[Count] = ' ' then
      begin
       {First Letter after Space}
       if Count < Length(Result) then
        begin
         Result[Count + 1]:=Upcase(Result[Count + 1]);
        end;
      end
     else if Result[Count] = '-' then
      begin
       {First Letter after Dash}
       if Count < Length(Result) then
        begin
         Result[Count + 1]:=Upcase(Result[Count + 1]);
        end;
      end
     else if Result[Count] = '''' then
      begin
       {First Letter after Apotrophe}
       if Count < Length(Result) then
        begin
         Result[Count + 1]:=Upcase(Result[Count + 1]);
        end;
      end;

     if Uppercase(Copy(Result,Count,2)) = 'MC' then
      begin
       {First Letter after Mc}
       if Count < (Length(Result) + 1) then
        begin
         Result[Count + 2]:=Upcase(Result[Count + 2]);
        end;
      end;

     {if Uppercase(Copy(Result,Count,3)) = 'MAC' then
      begin}
       {First Letter after Mac} {Unreliable Test}
       {if Count < (Length(Result) + 2) then
        begin
         Result[Count + 3]:=Upcase(Result[Count + 3]);
        end;
      end;}
    end;
  end;
end;

{==============================================================================}

function RemoveSpaces(const Text:String):String;
var
 WorkBuffer:String;
begin
 {}
 WorkBuffer:=Text;
 while Pos(' ',WorkBuffer) <> 0 do
  begin
   Delete(WorkBuffer,Pos(' ',WorkBuffer),1);
  end;
  
 Result:=WorkBuffer;
end;

{==============================================================================}

function SpaceCount(const Text:String):Integer;
var
 WorkBuffer:String;
 WorkCount:Integer;
begin
 {}
 Result:=0;
 if Trim(Text) = '' then Exit;
 WorkBuffer:=Text; {Trim(Text);} {Dont Trim Space is Allowed}
 WorkCount:=0;
 while Pos(' ',WorkBuffer) <> 0 do
  begin
   Inc(WorkCount);
   Delete(WorkBuffer,1,Pos(' ',WorkBuffer));
  end;
  
 Result:=WorkCount;
end;

{==============================================================================}

function NormaliseSpaces(const Text:String):String;
var
 WorkBuffer:String;
begin
 {}
 WorkBuffer:=Text;
 while Pos('  ',WorkBuffer) <> 0 do
  begin
   Delete(WorkBuffer,Pos('  ',WorkBuffer),1);
  end;
  
 Result:=WorkBuffer;
end;

{==============================================================================}

function NormaliseCase(const Text:String):String;
var
 NextSpace:Integer;
 InBuffer,OutBuffer,ChBuffer:String;
begin
 {}
 InBuffer:=LowerCase(Trim(NormaliseSpaces(Text)));
 OutBuffer:='';
 if Length(InBuffer) > 0 then
  begin
   ChBuffer:=Uppercase(Copy(InBuffer,1,1));
   OutBuffer:=OutBuffer + ChBuffer;
   Delete(InBuffer,1,1);
   NextSpace:=Pos(' ',InBuffer);
   while NextSpace <> 0 do
    begin
     OutBuffer:=OutBuffer + Copy(InBuffer,1,NextSpace);
     Delete(InBuffer,1,NextSpace);
     ChBuffer:=Uppercase(Copy(InBuffer,1,1));
     OutBuffer:=OutBuffer + ChBuffer;
     Delete(InBuffer,1,1);
     NextSpace:=Pos(' ',InBuffer);
    end;
  end;
  
 Result:=OutBuffer + InBuffer;
end;

{==============================================================================}

function StringFill(const Text:String;Count:Integer):String;
begin
 {}
 Result:=Text;
 
 while Length(Result) < Count do
  begin
   Result:=Result + ' ';
  end;
end;

{==============================================================================}

function RemoveQuotes(const AValue:String):String;
var
 WorkBuffer:String;
begin
 {}
 WorkBuffer:=Trim(AValue);
 if Length(WorkBuffer) > 0 then
  begin
   if WorkBuffer[1] = '"' then
    begin
     Delete(WorkBuffer,1,1);
    end;
  end;
 if Length(WorkBuffer) > 0 then
  begin
   if WorkBuffer[Length(WorkBuffer)] = '"' then
    begin
     Delete(WorkBuffer,Length(WorkBuffer),1);
    end
  end;
  
 Result:=WorkBuffer;
end;

{==============================================================================}

function RemoveBraces(const AValue:String):String;
var
 WorkBuffer:String;
begin
 {}
 WorkBuffer:=Trim(AValue);
 if Length(WorkBuffer) > 0 then
  begin
   if WorkBuffer[1] = '[' then
    begin
     Delete(WorkBuffer,1,1);
    end;
  end;
 if Length(WorkBuffer) > 0 then
  begin
   if WorkBuffer[Length(WorkBuffer)] = ']' then
    begin
     Delete(WorkBuffer,Length(WorkBuffer),1);
    end
  end;
  
 Result:=WorkBuffer;
end;

{==============================================================================}

function RemoveBrackets(const AValue:String):String;
var
 WorkBuffer:String;
begin
 {}
 WorkBuffer:=Trim(AValue);
 if Length(WorkBuffer) > 0 then
  begin
   if WorkBuffer[1] = '(' then
    begin
     Delete(WorkBuffer,1,1);
    end;
  end;
 if Length(WorkBuffer) > 0 then
  begin
   if WorkBuffer[Length(WorkBuffer)] = ')' then
    begin
     Delete(WorkBuffer,Length(WorkBuffer),1);
    end
  end;
  
 Result:=WorkBuffer;
end;

{==============================================================================}

function RemoveSingleQuotes(const AValue:String):String;
var
 WorkBuffer:String;
begin
 {}
 WorkBuffer:=Trim(AValue);
 if Length(WorkBuffer) > 0 then
  begin
   if WorkBuffer[1] = '''' then
    begin
     Delete(WorkBuffer,1,1);
    end;
  end;
 if Length(WorkBuffer) > 0 then
  begin
   if WorkBuffer[Length(WorkBuffer)] = '''' then
    begin
     Delete(WorkBuffer,Length(WorkBuffer),1);
    end
  end;
  
 Result:=WorkBuffer;
end;

{==============================================================================}

function RemoveCurlyBraces(const AValue:String):String;
var
 WorkBuffer:String;
begin
 {}
 WorkBuffer:=Trim(AValue);
 if Length(WorkBuffer) > 0 then
  begin
   if WorkBuffer[1] = '{' then
    begin
     Delete(WorkBuffer,1,1);
    end;
  end;
 if Length(WorkBuffer) > 0 then
  begin
   if WorkBuffer[Length(WorkBuffer)] = '}' then
    begin
     Delete(WorkBuffer,Length(WorkBuffer),1);
    end
  end;
  
 Result:=WorkBuffer;
end;

{==============================================================================}

function IsAlpha(const AValue:String):Boolean;
var
 Count:Integer;
begin
 {}
 Result:=False;
 
 if Length(AValue) = 0 then Exit;
 
 for Count:=1 to Length(AValue) do
  begin
   if not(AValue[Count] in ['a'..'z','A'..'Z']) then Exit;
  end;
  
 Result:=True;
end;

{==============================================================================}

function IsNumeric(const AValue:String):Boolean;
var
 Count:Integer;
begin
 {}
 Result:=False;
 
 if Length(AValue) = 0 then Exit;
 
 for Count:=1 to Length(AValue) do
  begin
   if not(AValue[Count] in ['0'..'9']) then Exit;
  end;
  
 Result:=True;
end;

{==============================================================================}

function RemoveNonAlpha(const Value:String):String;
var
 Count:Integer;
begin
 {}
 Result:=Trim(Value);
 
 if Length(Result) > 0 then
  begin
   for Count:=1 to Length(Result) do
    begin
     if not(Result[Count] in ['a'..'z','A'..'Z','-']) then
      begin
       Result[Count]:=' ';
      end;
    end;
    
   Result:=RemoveSpaces(Result);
  end;
end;

{==============================================================================}

function AddQuotes(const AValue:String):String;
var
 WorkBuffer:String;
begin
 {}
 WorkBuffer:=AValue; {Trim(AValue);} {Dont Trim Space is Allowed}
 if Length(WorkBuffer) > 0 then
  begin
   if WorkBuffer[1] <> '"' then
    begin
     WorkBuffer:='"' + WorkBuffer;
    end;
  end
 else
  begin
   WorkBuffer:='"';
  end;
 if Length(WorkBuffer) > 1 then
  begin
   if WorkBuffer[Length(WorkBuffer)] <> '"' then
    begin
     WorkBuffer:=WorkBuffer + '"';
    end;
  end
 else
  begin
   WorkBuffer:=WorkBuffer + '"';
  end;
 Result:=WorkBuffer;
end;

{==============================================================================}

function AddBraces(const AValue:String):String;
var
 WorkBuffer:String;
begin
 {}
 WorkBuffer:=AValue; {Trim(AValue);} {Dont Trim Space is Allowed}
 if Length(WorkBuffer) > 0 then
  begin
   if WorkBuffer[1] <> '[' then
    begin
     WorkBuffer:='[' + WorkBuffer;
    end;
  end
 else
  begin
   WorkBuffer:='[';
  end;
 if Length(WorkBuffer) > 1 then
  begin
   if WorkBuffer[Length(WorkBuffer)] <> ']' then
    begin
     WorkBuffer:=WorkBuffer + ']';
    end;
  end
 else
  begin
   WorkBuffer:=WorkBuffer + ']';
  end;
 Result:=WorkBuffer;
end;

{==============================================================================}

function AddBrackets(const AValue:String):String;
var
 WorkBuffer:String;
begin
 {}
 WorkBuffer:=AValue; {Trim(AValue);} {Dont Trim Space is Allowed}
 if Length(WorkBuffer) > 0 then
  begin
   if WorkBuffer[1] <> '(' then
    begin
     WorkBuffer:='(' + WorkBuffer;
    end;
  end
 else
  begin
   WorkBuffer:='(';
  end;
 if Length(WorkBuffer) > 1 then
  begin
   if WorkBuffer[Length(WorkBuffer)] <> ')' then
    begin
     WorkBuffer:=WorkBuffer + ')';
    end;
  end
 else
  begin
   WorkBuffer:=WorkBuffer + ')';
  end;
 Result:=WorkBuffer;
end;

{==============================================================================}

function AddSingleQuotes(const AValue:String):String;
var
 WorkBuffer:String;
begin
 {}
 WorkBuffer:=AValue; {Trim(AValue);} {Dont Trim Space is Allowed}
 if Length(WorkBuffer) > 0 then
  begin
   if WorkBuffer[1] <> '''' then
    begin
     WorkBuffer:='''' + WorkBuffer;
    end;
  end
 else
  begin
   WorkBuffer:='''';
  end;
 if Length(WorkBuffer) > 1 then
  begin
   if WorkBuffer[Length(WorkBuffer)] <> '''' then
    begin
     WorkBuffer:=WorkBuffer + '''';
    end;
  end
 else
  begin
   WorkBuffer:=WorkBuffer + '''';
  end;
 Result:=WorkBuffer;
end;

{==============================================================================}

function AddCurlyBraces(const AValue:String):String;
var
 WorkBuffer:String;
begin
 {}
 WorkBuffer:=AValue; {Trim(AValue);} {Dont Trim Space is Allowed}
 if Length(WorkBuffer) > 0 then
  begin
   if WorkBuffer[1] <> '{' then
    begin
     WorkBuffer:='{' + WorkBuffer;
    end;
  end
 else
  begin
   WorkBuffer:='{';
  end;
 if Length(WorkBuffer) > 1 then
  begin
   if WorkBuffer[Length(WorkBuffer)] <> '}' then
    begin
     WorkBuffer:=WorkBuffer + '}';
    end;
  end
 else
  begin
   WorkBuffer:=WorkBuffer + '}';
  end;
 Result:=WorkBuffer;
end;

{==============================================================================}

function NoOfChars(const AValue:String;AChar:Char):Integer;
var
 Count:Integer;
begin
 {}
 Result:=0;
 if Length(AValue) > 0 then
  begin
   for Count:=1 to Length(AValue) do
    begin
     if AValue[Count] = AChar then
      begin
       Inc(Result);
      end;
    end;
  end;
end;

{==============================================================================}

function ReplaceChar(const AValue:String;AChar,AReplace:Char):String;
var
 Count:Integer;
 WorkBuffer:String;
begin
 {}
 WorkBuffer:=AValue;
 for Count:=1 to Length(WorkBuffer) do
  begin
   if WorkBuffer[Count] = AChar then
    begin
     WorkBuffer[Count]:=AReplace;
    end;
  end;
  
 Result:=WorkBuffer;
end;

{==============================================================================}

function EncodeString(const AValue,AKey:String):String;
var
 Count:Integer;
 LongKey:String;
begin
 {}
 Result:='';
 if AKey = '' then Exit;
 if AValue = '' then Exit;

 {Build a LongKey by concatenating the supplied Key}
 LongKey:=AKey;
 while Length(LongKey) < Length(AValue) do
  begin
   LongKey:=LongKey + AKey;
  end;
  
 {XOR each Byte in the String using the same byte in the LongKey}
 for Count:=1 to Length(AValue) do
  begin
   if Ord(AValue[Count]) = Ord(LongKey[Count]) then
    begin
     {If the xor would produce a zero then skip it}
     Result:=Result + AValue[Count];
    end
   else
    begin
     Result:=Result + Chr((Ord(AValue[Count]) xor Ord(LongKey[Count])));
    end;
  end;
end;

{==============================================================================}

function DecodeString(const AValue,AKey:String):String;
begin
 {}
 Result:=EncodeString(AValue,AKey);
end;

{==============================================================================}

procedure DelimitStrings(AStrings:TStrings;var AString:String;const ADelimiter:String);
var
 Count:Integer;
begin
 {}
 if AStrings = nil then Exit;
 if Length(ADelimiter) = 0 then Exit; {Dont Trim Space is Allowed}
 
 AString:='';
 
 for Count:=0 to AStrings.Count - 1 do
  begin
   AString:=AString + AStrings.Strings[Count];
   if Count <> AStrings.Count - 1 then
    begin
     AString:=AString + ADelimiter;
    end;
  end;
end;

{==============================================================================}

procedure UndelimitString(const AString:String;AStrings:TStrings;const ADelimiter:String);
var
 PosIdx:Integer;
 WorkBuffer:String;
begin
 {}
 if Length(AString) = 0 then Exit;
 if AStrings = nil then Exit;
 if Length(ADelimiter) = 0 then Exit; {Dont Trim Space is Allowed}
 
 WorkBuffer := AString;
 
 PosIdx:=Pos(ADelimiter,WorkBuffer);
 while PosIdx > 0 do
  begin
   AStrings.Add(Copy(WorkBuffer,1,PosIdx - 1));
   Delete(WorkBuffer,1,PosIdx);
   
   PosIdx:=Pos(ADelimiter,WorkBuffer);
  end;
  
 AStrings.Add(WorkBuffer);
end;

{==============================================================================}

procedure UndelimitQuotedString(const AString:String;AStrings:TStrings;const ADelimiter:String);
var
 Count:Integer;
 WorkBuffer:String;
 StringBuffer:String;
 TempStrings:TStringList;
begin
 {}
 if ADelimiter = '' then Exit; {Trim(ADelimiter)} {Dont Trim Space is Allowed}
 
 TempStrings:=TStringList.Create;
 try
  UndelimitString(AString,TempStrings,ADelimiter);
  StringBuffer:='';
  for Count:=0 to TempStrings.Count - 1 do
   begin
    WorkBuffer:=TempStrings.Strings[Count];
    if Length(WorkBuffer) = 0 then
     begin
      {Null String}
      if Length(StringBuffer) = 0 then
       begin
        {Nothing Pending}
        AStrings.Add(WorkBuffer);
       end
      else
       begin
        {Pending Strings}
        {Continuation}
        StringBuffer:=StringBuffer + ADelimiter + WorkBuffer;
       end;
     end
    else if Length(WorkBuffer) = 1 then
     begin
      {Single Character String}
      if Length(StringBuffer) = 0 then
       begin
        {Nothing Pending}
        if WorkBuffer[1] = '"' then
         begin
          {Start}
          StringBuffer:=WorkBuffer;
         end
        else
         begin
          AStrings.Add(WorkBuffer);
         end;
       end
      else
       begin
        {Pending Strings}
        if WorkBuffer[1] = '"' then
         begin
          {Completion}
          StringBuffer:=StringBuffer + ADelimiter + WorkBuffer;
          AStrings.Add(StringBuffer);
          StringBuffer:='';
         end
        else
         begin
          {Continuation}
          StringBuffer:=StringBuffer + ADelimiter + WorkBuffer;
         end;
       end;
     end
    else
     begin
      {Multi Character String}
      if Length(StringBuffer) = 0 then
       begin
        {Nothing Pending}
        if WorkBuffer[1] = '"' then
         begin
          if WorkBuffer[Length(WorkBuffer)] = '"' then
           begin
            AStrings.Add(WorkBuffer);
           end
          else
           begin
            {Start}
            StringBuffer:=WorkBuffer;
           end;
         end
        else
         begin
          AStrings.Add(WorkBuffer);
         end;
       end
      else
       begin
        {Pending Strings}
        if WorkBuffer[Length(WorkBuffer)] = '"' then
         begin
          {Completion}
          StringBuffer:=StringBuffer + ADelimiter + WorkBuffer;
          AStrings.Add(StringBuffer);
          StringBuffer:='';
         end
        else
         begin
          {Continuation}
          StringBuffer:=StringBuffer + ADelimiter + WorkBuffer;
         end;
       end;
     end;
   end;
   
  if Length(StringBuffer) > 0 then AStrings.Add(StringBuffer);
 finally
  TempStrings.Free;
 end;
end;

{==============================================================================}

function AddSlashes(const AFilePath:String):String;
var
 WorkBuffer:String;
begin
 {}
 WorkBuffer:=Trim(AFilePath);
 
 if Length(WorkBuffer) > 0 then
  begin
   if WorkBuffer[1] <> '\' then
    begin
     WorkBuffer:='\' + WorkBuffer;
    end;
   if WorkBuffer[Length(WorkBuffer)] <> '\' then
    begin
     WorkBuffer:=WorkBuffer + '\';
    end;
  end;
  
 Result:=WorkBuffer;
end;

{==============================================================================}

function AddTrailingSlash(const AFilePath:String):String;
var
 WorkBuffer:String;
begin
 {}
 WorkBuffer:=Trim(AFilePath);
 
 if Length(WorkBuffer) > 0 then
  begin
   if WorkBuffer[Length(WorkBuffer)] <> '\' then
    begin
     WorkBuffer:=WorkBuffer + '\';
    end;
  end
 else
  begin
   {WorkBuffer:='\';} {Do not add if blank}
  end;  
  
 Result:=WorkBuffer;
end;

{==============================================================================}

function AddLeadingSlash(const AFilePath:String):String;
var
 WorkBuffer:String;
begin
 {}
 WorkBuffer:=Trim(AFilePath);
 
 if Length(WorkBuffer) > 0 then
  begin
   if WorkBuffer[1] <> '\' then
    begin
     WorkBuffer:='\' + WorkBuffer;
    end;
  end
 else
  begin
   {WorkBuffer:='\';} {Do not add if blank}
  end;
  
 Result:=WorkBuffer;
end;

{==============================================================================}

function StripSlashes(const AFilePath:String):String;
var
 WorkBuffer:String;
begin
 {}
 WorkBuffer:=Trim(AFilePath);
 
 if Length(WorkBuffer) > 0 then
  begin
   if WorkBuffer[1] = '\' then
    begin
     Delete(WorkBuffer,1,1);
    end;
   if Length(WorkBuffer) > 0 then
    begin
     if WorkBuffer[Length(WorkBuffer)] = '\' then
      begin
       Delete(WorkBuffer,Length(WorkBuffer),1);
      end;
    end;
  end;
  
 Result:=WorkBuffer;
end;

{==============================================================================}

function StripTrailingSlash(const AFilePath:String):String;
var
 WorkBuffer:String;
begin
 {}
 WorkBuffer:=Trim(AFilePath);
 
 if Length(WorkBuffer) > 0 then
  begin
   if WorkBuffer[Length(WorkBuffer)] = '\' then
    begin
     Delete(WorkBuffer,Length(WorkBuffer),1);
    end;
  end;

 Result:=WorkBuffer;
end;

{==============================================================================}

function StripLeadingSlash(const AFilePath:String):String;
var
 WorkBuffer:String;
begin
 {}
 WorkBuffer:=Trim(AFilePath);
 
 if Length(WorkBuffer) > 0 then
  begin
   if WorkBuffer[1] = '\' then
    begin
     Delete(WorkBuffer,1,1);
    end;
  end;
  
 Result:=WorkBuffer;
end;

{==============================================================================}

function AddTrailingDot(const AValue:String):String;
var
 WorkBuffer:String;
begin
 {}
 WorkBuffer:=Trim(AValue);
 
 if Length(WorkBuffer) > 0 then
  begin
   if WorkBuffer[Length(WorkBuffer)] <> '.' then
    begin
     WorkBuffer:=WorkBuffer + '.';
    end;
  end
 else
  begin
   {WorkBuffer:='.';} {Do not add if blank}
  end;
  
 Result:=WorkBuffer;
end;

{==============================================================================}

function AddLeadingDot(const AValue:String):String;
var
 WorkBuffer:String;
begin
 {}
 WorkBuffer:=Trim(AValue);
 
 if Length(WorkBuffer) > 0 then
  begin
   if WorkBuffer[1] <> '.' then
    begin
     WorkBuffer:='.' + WorkBuffer;
    end;
  end
 else
  begin
   {WorkBuffer:='.';} {Do not add if blank}
  end;
  
 Result:=WorkBuffer;
end;

{==============================================================================}

function StripTrailingDot(const AValue:String):String;
var
 WorkBuffer:String;
begin
 {}
 WorkBuffer:=Trim(AValue);
 
 if Length(WorkBuffer) > 0 then
  begin
   if WorkBuffer[Length(WorkBuffer)] = '.' then
    begin
     Delete(WorkBuffer,Length(WorkBuffer),1);
    end;
  end;
  
 Result:=WorkBuffer;
end;

{==============================================================================}

function StripLeadingDot(const AValue:String):String;
var
 WorkBuffer:String;
begin
 {}
 WorkBuffer:=Trim(AValue);
 
 if Length(WorkBuffer) > 0 then
  begin
   if WorkBuffer[1] = '.' then
    begin
     Delete(WorkBuffer,1,1);
    end;
  end;
  
 Result:=WorkBuffer;
end;

{==============================================================================}

function AddTrailingChar(const AFilePath,ASlashChar:String):String;
var
 WorkBuffer:String;
begin
 {}
 WorkBuffer:=Trim(AFilePath);
 
 if Length(WorkBuffer) > 0 then
  begin
   if WorkBuffer[Length(WorkBuffer)] <> ASlashChar then
    begin
     WorkBuffer:=WorkBuffer + ASlashChar;
    end;
  end
 else
  begin
   {WorkBuffer:=ASlashChar;} {Do not add if blank}
  end;
  
 Result:=WorkBuffer;
end;

{==============================================================================}

function AddLeadingChar(const AFilePath,ASlashChar:String):String;
var
 WorkBuffer:String;
begin
 {}
 WorkBuffer:=Trim(AFilePath);
 
 if Length(WorkBuffer) > 0 then
  begin
   if WorkBuffer[1] <> ASlashChar then
    begin
     WorkBuffer:=ASlashChar + WorkBuffer;
    end;
  end
 else
  begin
   {WorkBuffer:=ASlashChar;} {Do not add if blank}
  end;
  
 Result:=WorkBuffer;
end;

{==============================================================================}

function StripTrailingChar(const AFilePath,ASlashChar:String):String;
var
 WorkBuffer:String;
begin
 {}
 WorkBuffer:=Trim(AFilePath);
 
 if Length(WorkBuffer) > 0 then
  begin
   if WorkBuffer[Length(WorkBuffer)] = ASlashChar then
    begin
     Delete(WorkBuffer,Length(WorkBuffer),1);
    end;
  end;
  
 Result:=WorkBuffer;
end;

{==============================================================================}

function StripLeadingChar(const AFilePath,ASlashChar:String):String;
var
 WorkBuffer:String;
begin
 {}
 WorkBuffer:=Trim(AFilePath);
 
 if Length(WorkBuffer) > 0 then
  begin
   if WorkBuffer[1] = ASlashChar then
    begin
     Delete(WorkBuffer,1,1);
    end;
  end;
  
 Result:=WorkBuffer;
end;

{==============================================================================}

function IsWildcard(const Value:String):Boolean;
begin
 {}
 Result:=False;
 
 if (Pos('*',Value) = 0) and (Pos('?',Value) = 0) then Exit;
 
 Result:=True;
end;

{==============================================================================}

function UniqueName(const BaseName:String;CurrentNames:TStrings):String;
var
 Count:Integer;
 WorkBuffer:String;
begin
 {}
 Count:=0;
 WorkBuffer:=BaseName;
 while CurrentNames.IndexOf(WorkBuffer) <> -1 do
  begin
   Inc(Count);
   WorkBuffer:=BaseName + ' ' + IntToStr(Count);
  end;
  
 Result:=WorkBuffer;
end;

{==============================================================================}

function WildcardNameMatch(const Name,Wildcard:String;CaseSensitive:Boolean):Boolean;
var
 Count:Integer;
 WildPos,NamePos:Integer;
 WildByte,NameByte:Integer;
 WildLength,NameLength:Integer;
 WildBuffer,NameBuffer:String;
begin
 {}
 //To Do //This routine is not Unicode compliant
 
 Result:=False;
 
 if Length(Name) = 0 then Exit;
 if Length(Wildcard) = 0 then Exit; 

 WildPos:=1;
 NamePos:=1;
 WildByte:=0;
 NameByte:=0;
 WildLength:=Length(Wildcard);
 NameLength:=Length(Name);

 {Check for a Wildcard of all * (eg **** or just *)}
 for Count:=1 to WildLength do
  begin
   if Wildcard[Count] <> '*' then
    begin
     Break;
    end;
   if Count = WildLength then
    begin
     Result:=True;
     Exit;
    end;
  end;

 {Check for Case Sensitive Match}
 if not CaseSensitive then
  begin
   NameBuffer:=Uppercase(Name);
   WildBuffer:=Uppercase(Wildcard);
  end
 else
  begin
   NameBuffer:=Name;
   WildBuffer:=Wildcard;
  end;

 while True do
  begin
   if WildPos > WildLength then
    begin
     Result:=(NamePos >= NameLength);
     Exit;
    end
   else
    begin
     WildByte:=Ord(WildBuffer[WildPos]);
    end;

   if NamePos > NameLength then
    begin
     Result:=False;
     Exit;
    end
   else
    begin
     NameByte:=Ord(NameBuffer[NamePos]);
    end;

   if (WildByte = NameByte) then
    begin
     Inc(NamePos);
     Inc(WildPos);
    end
   else if WildByte = 63 then   {Check for ?}
    begin
     Inc(NamePos);
     Inc(WildPos);
    end
   else if WildByte = 42 then   {Check for *}
    begin
     if WildPos = WildLength then  {If the Last Char in Wildcard is * then match}
      begin
       Result:=True;
       Exit;
      end
     else
      begin
       Inc(WildPos);                      {Else step forward in Wildcard}
       WildByte:=Ord(WildBuffer[WildPos]);
       if NamePos = NameLength then
        begin
         //To Do //What to do here ?
        end
       else
        begin                             {Then Step forward in FileName until starts matching again}
         Inc(NamePos);
         NameByte:=Ord(NameBuffer[NamePos]);
         while (NameByte <> WildByte) and (NamePos < NameLength) do
          begin
           Inc(NamePos);
           NameByte:=Ord(NameBuffer[NamePos]);
          end;
        end;
      end;
    end
   else
    begin
     Result:=False;
     Exit;
    end;
  end;
end;

{==============================================================================}

function AddQuotesIfSpaced(const AValue:String):String;
begin
 {}
 Result:=AValue;
 if Pos(' ',Result) <> 0 then
  begin
   Result:=AddQuotes(AValue);
  end;
end;

{==============================================================================}

function ExtractCommand(const ACommandLine:String):String;
var
 PosIdx:Integer;
begin
 {}
 Result:=ACommandLine; {Default to command}
 
 if Length(ACommandLine) <> 0 then
  begin
   {Check for Quotes}
   PosIdx:=Pos('"',ACommandLine);
   if PosIdx <> 0 then
    begin
     {Check for Space}
     PosIdx:=Pos('" ',ACommandLine);
     if PosIdx <> 0 then
      begin
       Result:=RemoveQuotes(Copy(ACommandLine,1,PosIdx - 1));
      end;
    end
   else
    begin
     {Check for Space}
     PosIdx:=Pos(' ',ACommandLine);
     if PosIdx <> 0 then
      begin
       Result:=Copy(ACommandLine,1,PosIdx - 1);
      end;
    end;
  end;
end;

{==============================================================================}

function ExtractParameters(const ACommandLine:String):String;
var
 PosIdx:Integer;
begin
 {}
 Result:=''; {Default to nothing}
 
 if Length(ACommandLine) <> 0 then
  begin
   {Check for Quotes}
   PosIdx:=Pos('"',ACommandLine);
   if PosIdx <> 0 then
    begin
     {Check for Space}
     PosIdx:=Pos('" ',ACommandLine);
     if PosIdx <> 0 then
      begin
       Result:=Copy(ACommandLine,PosIdx + 1,Length(ACommandLine));
      end;
    end
   else
    begin
     {Check for Space}
     PosIdx:=Pos(' ',ACommandLine);
     if PosIdx <> 0 then
      begin
       Result:=Copy(ACommandLine,PosIdx + 1,Length(ACommandLine));
      end;
    end;
  end;
end;

{==============================================================================}

function AllocateCommandLine(const ACommandLine:String;out AArgC:Integer):PPChar;
{Allocate a C style command line and return ArgC and ArgV in the correct format}
{CommandLine: The command line to be formatted into C style ArgV and ArgC}
{ArgC: The count of command line parameters in the result}
var
 Name:String;
 Param:String;
 Size:LongWord;
 Index:LongWord;
 Offset:LongWord;
 Buffer:PPChar;
 Counter:Integer;
 Params:TStringList;
begin
 {}
 Result:=nil;
 
 {Get Name}
 Name:=ParamStr(0);
 
 {Setup Defaults (Include argv[0])}
 AArgC:=1; 
 Size:=Length(Name) + SizeOf(Char);
 
 Params:=TStringList.Create;
 try
  {Get Params}
  UndelimitString(ACommandLine,Params,' ');
  
  {Check Params}
  for Counter:=0 to Params.Count - 1 do
   begin
    Param:=Params.Strings[Counter];
    if Length(Param) > 0 then
     begin
      Inc(AArgC);
      Inc(Size,Length(Param) + SizeOf(Char));
     end;
   end;
   
  {Allocate Command Line}
  Buffer:=AllocMem((AArgC * SizeOf(PChar)) + Size);

  {Copy Name}
  Index:=0;
  Offset:=AArgC * SizeOf(PChar);
  Buffer[Index]:=PChar(Pointer(Buffer) + Offset);
  StrLCopy(Buffer[Index],PChar(Name),Length(Name));
  Inc(Offset,Length(Name) + SizeOf(Char));
  
  {Copy Params}
  for Counter:=0 to Params.Count - 1 do
   begin
    Param:=Params.Strings[Counter];
    if Length(Param) > 0 then
     begin
      Inc(Index);
      Buffer[Index]:=PChar(Pointer(Buffer) + Offset);
      StrLCopy(Buffer[Index],PChar(Param),Length(Param));
      Inc(Offset,Length(Param) + SizeOf(Char));
     end;
   end;
   
  Result:=Buffer; 
 finally
  Params.Free;
 end;
end;

{==============================================================================}

procedure ReleaseCommandLine(AArgV:PPChar);
{Free a C command line allocated by AllocateCommandLine}
{ArgV: The command line to be freed}
begin
 {}
 {Check ArgV}
 if AArgV = nil then Exit;
 
 {Free ArgV}
 FreeMem(AArgV);
end;

{==============================================================================}

function MultiStringToStrings(ABuffer:Pointer;ASize:Integer;AStrings:TStrings):Boolean;
var
 Offset:Integer;
 WorkBuffer:String;
begin
 {}
 Result:=False;
 try
  if ABuffer = nil then Exit;
  if AStrings = nil then Exit;

  Offset:=0;
  AStrings.Clear;
  if ASize > 1 then
   begin
    while Offset < ASize do
     begin
      WorkBuffer:=PChar(LongWord(ABuffer) + LongWord(Offset));
      if Length(WorkBuffer) <> 0 then {Dont Trim, space is allowed}
       begin
        AStrings.Add(WorkBuffer);
       end;
      Offset:=Offset + Length(WorkBuffer) + 1;
     end;
   end;
  Result:=True;
 except
  {}
 end;
end;

{==============================================================================}

function StringsToMultiString(AStrings:TStrings;var ABuffer:Pointer;var ASize:Integer):Boolean;
{Note: The returned buffer must be freed using FreeMem}
var
 Count:Integer;
 Offset:Integer;
 WorkBuffer:String;
begin
 {}
 Result:=False;
 try
  if AStrings = nil then Exit;

  ASize:=1;
  Offset:=0;
  ABuffer:=AllocMem(ASize);
  for Count:=0 to AStrings.Count - 1 do
   begin
    if Length(AStrings.Strings[Count]) <> 0 then  {Dont Trim, space is allowed}
     begin
      WorkBuffer:=AStrings.Strings[Count];
      ASize:=ASize + Length(WorkBuffer) + 1;
      ReallocMem(ABuffer,ASize);
      System.Move(PChar(WorkBuffer)^,Pointer(LongWord(ABuffer) + LongWord(Offset))^,Length(WorkBuffer) + 1);
      Offset:=Offset + Length(WorkBuffer) + 1;
     end;
   end;
  FillChar(Pointer(LongWord(ABuffer) + LongWord(Offset))^,1,#0);
  Result:=True;
 except
  {}
 end;
end;

{==============================================================================}

function MultiStringToDelimited(ABuffer:PChar;ADelimiter:String):String;
var
 Size:Integer;
 Offset:Integer;
 WorkBuffer:String;
begin
 {}
 Result:='';
 try
  if ABuffer = nil then Exit;
  if Length(ADelimiter) = 0 then Exit;

  {Calculate Size}
  Size:=0;
  if Byte(Pointer(LongWord(ABuffer))^) <> 0 then
   begin
    while Word(Pointer(LongWord(ABuffer) + LongWord(Size))^) <> 0 do
     begin
      Inc(Size,SizeOf(Byte));
     end;
   end;
   
  {Copy Strings}
  Offset:=0;
  if Size > 1 then
   begin
    while Offset < Size do
     begin
      WorkBuffer:=PChar(LongWord(ABuffer) + LongWord(Offset));
      if Length(WorkBuffer) <> 0 then {Dont Trim, space is allowed}
       begin
        if Length(Result) = 0 then
         Result:=WorkBuffer
        else
         Result:=Result + ADelimiter + WorkBuffer;
       end;
      Offset:=Offset + Length(WorkBuffer) + 1;
     end;
   end;
 except
  {}
 end;
end;

{==============================================================================}

function DelimitedToMultiString(const AString:String;ADelimiter:String):PChar;
{Note: The returned string must be freed using FreeMem}
var
 Size:Integer;
 Offset:Integer;
 PosIdx:Integer;
 Current:String;
 WorkBuffer:String;
begin
 {}
 Result:=nil;
 try
  if Length(AString) = 0 then Exit;
  if Length(ADelimiter) = 0 then Exit;

  Size:=1;
  Offset:=0;
  Result:=AllocMem(Size);
  
  {Copy Strings}
  WorkBuffer:=AString;
  PosIdx:=Pos(ADelimiter,WorkBuffer);
  while PosIdx <> 0 do
   begin
    Current:=Copy(WorkBuffer,1,PosIdx - 1);
    if Length(Current) <> 0 then  {Dont Trim, space is allowed}
     begin
      Size:=Size + Length(Current) + 1;
      ReallocMem(Result,Size);
      System.Move(PChar(Current)^,Pointer(LongWord(Result) + LongWord(Offset))^,Length(Current) + 1);
      Offset:=Offset + Length(Current) + 1;
     end;
    Delete(WorkBuffer,1,PosIdx);
    PosIdx:=Pos(ADelimiter,WorkBuffer);
   end;
  FillChar(Pointer(LongWord(Result) + LongWord(Offset))^,1,#0);
 except
  {}
 end;
end;

{==============================================================================}

function DuplicateString(AString:PChar):PChar;
{Note: The returned string must be freed using LocalFree}
var
 Len:DWORD;
begin
 {}
 Result:=nil;
 if AString = nil then Exit;

 Len:=(lstrlenA(AString) + 1) * SizeOf(CHAR);
 Result:=PChar(LocalAlloc(LMEM_FIXED,Len));
 if (Result <> nil) then
  begin
   CopyMemory(Result,AString,Len);
  end;
end;

{==============================================================================}

function DuplicateWideString(AString:PWideChar):PWideChar;
{Note: The returned string must be freed using LocalFree}
var
 Len:DWORD;
begin
 {}
 Result:=nil;
 
 if AString = nil then Exit;

 Len:=(lstrlenW(AString) + 1) * SizeOf(WCHAR);
 Result:=PWideChar(LocalAlloc(LMEM_FIXED,Len));
 if (Result <> nil) then
  begin
   CopyMemory(Result,AString,Len);
  end;
end;

{==============================================================================}

function DuplicateMultiString(AString:PChar):PChar;
{Note: The returned string must be freed using LocalFree}
var
 Size:LongWord;
begin
 {}
 Result:=nil;
 
 if AString = nil then Exit;

 {Calculate Size}
 Size:=0;
 if Byte(Pointer(LongWord(AString))^) <> 0 then
  begin
   while Word(Pointer(LongWord(AString) + Size)^) <> 0 do
    begin
     Inc(Size,SizeOf(Byte));
    end;
  end;
  
 {Duplicate String}
 Result:=PChar(LocalAlloc(LMEM_FIXED,Size + SizeOf(Byte)));
 if (Result <> nil) then
  begin
   CopyMemory(Result,AString,Size + SizeOf(Byte));
  end;
end;

{==============================================================================}

function DuplicateMultiWideString(AString:PWideChar):PWideChar;
{Note: The returned string must be freed using LocalFree}
var
 Size:LongWord;
begin
 {}
 Result:=nil;
 
 if AString = nil then Exit;

 {Calculate Size}
 Size:=0;
 if Word(Pointer(LongWord(AString))^) <> 0 then
  begin
   while LongWord(Pointer(LongWord(AString) + Size)^) <> 0 do
    begin
     Inc(Size,SizeOf(Word));
    end;
  end;
  
 {Duplicate String}
 Result:=PWideChar(LocalAlloc(LMEM_FIXED,Size + SizeOf(Word)));
 if (Result <> nil) then
  begin
   CopyMemory(Result,AString,Size + SizeOf(Word));
  end;
end;

{==============================================================================}
{==============================================================================}
{File Functions} 
function IsRootDirectory(const Path:String):Boolean;
var
 WorkBuffer:String;
 SlashCount:Integer;
begin
 {}
 //To Do //Use the GetPathType etc functions and GetPathRoot etc //Need to build these into UltiboUtils ?
 
 Result:=False;
 
 WorkBuffer:=Trim(Path);
 if Length(WorkBuffer) > 0 then
  begin
   {Make sure we have a trailing \}
   if WorkBuffer[Length(WorkBuffer)] <> '\' then
    begin
     WorkBuffer:=WorkBuffer + '\';
    end;
   {Check for a Drive relative path (ie C:\)}
   if Length(WorkBuffer) > 2 then
    begin
     if Copy(WorkBuffer,Length(WorkBuffer) - 1,2) = ':\' then
      begin
       Result:=True;
       Exit;
      end;
    end;
   {Check for a UNC Path (ie \\SERVER\PATH\}
   if Length(WorkBuffer) > 2 then
    begin
     if Copy(WorkBuffer,1,2) = '\\' then
      begin
       {Check for How many \ characters there are, must be more than 4}
       SlashCount:=0;
       while Pos('\',WorkBuffer) <> 0 do
        begin
         Inc(SlashCount);
         Delete(WorkBuffer,Pos('\',WorkBuffer),1);
        end;
       if SlashCount < 5 then
        begin
         Result:=True;
        end;
      end;
    end;
  end;
end;

{==============================================================================}

function IsEightDotThree(const FileName:String):Boolean;
var
 Count:Integer;
 DotPos:Integer;
 DotCount:Integer;
 NameLength:Integer;
begin
 {}
 Result:=False;
 
 {Check for zero length or series of spaces}
 if Length(Trim(FileName)) = 0 then Exit;

 {Check for length greater than allowed}
 if Length(FileName) > MAX_FAT_FILE then Exit;

 {Check for leading dot}
 if FileName[1] = '.' then Exit;

 {Check for leading space}
 if FileName[1] = ' ' then Exit;

 {Check for case match}
 if Uppercase(FileName) <> FileName then Exit;

 {Check for Invalid Chars, Substituted Chars, Spaces, Multiple Dots}
 DotPos:=0;
 DotCount:=0;
 NameLength:=Length(FileName);
 for Count:=1 to NameLength do
  begin
   {Check for Invalid Chars}
   if FileName[Count] in INVALID_FILENAME_CHARS then Exit;
   {Check for Substituted Chars}
   if FileName[Count] in SHORT_FILENAME_SUBST_CHARS then Exit;
   {Check for Spaces}
   if FileName[Count] = ' ' then Exit;
   {Check for Multiple Dots}
   if FileName[Count] = '.' then Inc(DotCount);
   if DotCount > 1 then Exit;
   {Check for Dot Position}
   if FileName[Count] = '.' then DotPos:=Count;
  end;

 {Check for Name and Extension length}
 if DotPos > 0 then
  begin
   {Check a Filename with a dot in it}
   if DotPos < 10 then
    begin
     if (NameLength - DotPos) < 4 then Result:=True;
    end;
  end
 else
  begin
   {Check a Filename without a dot in it}
   if NameLength < 9 then Result:=True;
  end;
end;

{==============================================================================}

function TruncateLongName(const FileName:String):String;
begin
 {}
 Result:=GenerateShortName(FileName,-1);
end;

{==============================================================================}

function GenerateShortName(const FileName:String;AliasCount:Integer):String;
{Note: If AliasCount is less than 0 then dont generate the numeric tail}
var
 Count:Integer;
 DotPos:Integer;
 LastDot:Integer;
 WorkShort:String;
 WorkBuffer:String;
 WorkFileExt:String;
 WorkFileName:String;
begin
 {}
 //To Do //This needs to be rewritten somewhat - done a little already
 
 Result:='';
 
 {Check for Empty String or Series of Spaces}
 WorkShort:=TrimRight(Uppercase(FileName));
 if Length(WorkShort) = 0 then Exit;

 {Check for Alias Count > 9999} {Needs to be improved}
 if AliasCount > 9999 then Exit;

 {Check for Already Eight Dot Three}
 if IsEightDotThree(WorkShort) then
  begin
   Result:=WorkShort;
   Exit;
  end;

 {Check for Invalid and Substituted Chars}
 for Count:=1 to Length(WorkShort) do
  begin
   {Invalid Chars}
   if WorkShort[Count] in INVALID_FILENAME_CHARS then Exit;
   {Substituted Chars}
   if WorkShort[Count] in SHORT_FILENAME_SUBST_CHARS then WorkShort[Count]:='_';
  end;

 {Check for and Remove Spaces}
 WorkBuffer:='';
 for Count:=1 to Length(WorkShort) do
  begin
   if WorkShort[Count] <> ' ' then WorkBuffer:=WorkBuffer + WorkShort[Count];
  end;
 WorkShort:=WorkBuffer;

 {Check for and Remove Leading Dots}
 while WorkShort[1] = '.' do
  begin
   Delete(WorkShort,1,1);
   if Length(WorkShort) = 0 then Exit;
  end;

 {Remove all Dots except the last one}
 if Pos('.',WorkShort) <> 0 then
  begin
   {Find Last Dot}
   LastDot:=0;
   for Count:=1 to Length(WorkShort) do
    begin
     if WorkShort[Count] = '.' then LastDot:=Count;
    end;
   {Remove all other Dots}
   WorkBuffer:='';
   for Count:=1 to Length(WorkShort) do
    begin
     if Count <> LastDot then
      begin
       if WorkShort[Count] <> '.' then WorkBuffer:=WorkBuffer + WorkShort[Count];
      end
     else
      begin
       WorkBuffer:=WorkBuffer + WorkShort[Count];
      end;
    end;
   WorkShort:=WorkBuffer;
  end;

 {Create ShortName}
 {Get First 3, 4, 5, 6 or 8 Chars up to dot and First 3 Chars after dot}
 {Find Dot}
 DotPos:=Pos('.',WorkShort);
 if DotPos > 0 then
  begin
   {With Dot}
   if AliasCount < 0 then
    begin
     {No Numeric Tail}
     WorkFileName:=Copy(WorkShort,1,Min(8,DotPos - 1));
    end
   else if AliasCount < 10 then
    begin
     {1 - 9}
     WorkFileName:=Copy(WorkShort,1,Min(6,DotPos - 1));
    end
   else if AliasCount < 100 then
    begin
     {10 - 99}
     WorkFileName:=Copy(WorkShort,1,Min(5,DotPos - 1));
    end
   else if AliasCount < 1000 then
    begin
     {100 - 999}
     WorkFileName:=Copy(WorkShort,1,Min(4,DotPos - 1));
    end
   else
    begin
     {1000 - 9999}
     WorkFileName:=Copy(WorkShort,1,Min(3,DotPos - 1));
    end;
   WorkFileExt:=Copy(WorkShort,DotPos + 1,3);
  end
 else
  begin
   {Without Dot}
   if AliasCount < 0 then
    begin
     {No Numeric Tail}
     WorkFileName:=Copy(WorkShort,1,8);
    end
   else if AliasCount < 10 then
    begin
     {1 - 9}
     WorkFileName:=Copy(WorkShort,1,6);
    end
   else if AliasCount < 100 then
    begin
     {10 - 99}
     WorkFileName:=Copy(WorkShort,1,5);
    end
   else if AliasCount < 1000 then
    begin
     {100 - 999}
     WorkFileName:=Copy(WorkShort,1,4);
    end
   else
    begin
     {1000 - 9999}
     WorkFileName:=Copy(WorkShort,1,3);
    end;
   WorkFileExt:='';
  end;

 {Add the Dot if there is an Extension}
 if WorkFileExt <> '' then WorkFileExt:='.' + WorkFileExt;

 if AliasCount < 0 then
  begin
   {Format the Result}
   Result:=WorkFileName + WorkFileExt;
  end
 else
  begin
   {Format the Result ~x}
   if AliasCount = 0 then AliasCount:=1;
   Result:=WorkFileName + '~' + IntToStr(AliasCount) + WorkFileExt;
  end;
end;

{==============================================================================}

function GenerateShortNameEx(const FileName:String;AliasCount:Integer;Hash:Word;UseHash:Boolean):String;
{Note: If AliasCount is less than 0 then dont generate the numeric tail}
var
 Count:Integer;
 DotPos:Integer;
 LastDot:Integer;
 WorkHash:String;
 WorkShort:String;
 WorkBuffer:String;
 WorkFileExt:String;
 WorkFileName:String;
begin
 {}
 //To Do //This needs to be rewritten somewhat - done a little already
 
 if not UseHash then
  begin
   Result:=GenerateShortName(FileName,AliasCount);
  end
 else
  begin
   Result:='';
   
   {Check for Empty String or Series of Spaces}
   WorkShort:=TrimRight(Uppercase(FileName));
   if Length(WorkShort) = 0 then Exit;

   {Check for Alias Count > 999}
   if AliasCount > 999 then Exit;

   {Check for Already Eight Dot Three}
   if IsEightDotThree(WorkShort) then
    begin
     Result:=WorkShort;
     Exit;
    end;

   {Check for Invalid and Substituted Chars}
   for Count:=1 to Length(WorkShort) do
    begin
     {Invalid Chars}
     if WorkShort[Count] in INVALID_FILENAME_CHARS then Exit;
     {Substituted Chars}
     if WorkShort[Count] in SHORT_FILENAME_SUBST_CHARS then WorkShort[Count]:='_';
    end;

   {Check for and Remove Spaces}
   WorkBuffer:='';
   for Count:=1 to Length(WorkShort) do
    begin
     if WorkShort[Count] <> ' ' then WorkBuffer:=WorkBuffer + WorkShort[Count];
    end;
   WorkShort:=WorkBuffer;

   {Check for and Remove Leading Dots}
   while WorkShort[1] = '.' do
    begin
     Delete(WorkShort,1,1);
     if Length(WorkShort) = 0 then Exit;
    end;

   {Remove all Dots except the last one}
   if Pos('.',WorkShort) <> 0 then
    begin
     {Find Last Dot}
     LastDot:=0;
     for Count:=1 to Length(WorkShort) do
      begin
       if WorkShort[Count] = '.' then LastDot:=Count;
      end;
     {Remove all other Dots}
     WorkBuffer:='';
     for Count:=1 to Length(WorkShort) do
      begin
       if Count <> LastDot then
        begin
         if WorkShort[Count] <> '.' then WorkBuffer:=WorkBuffer + WorkShort[Count];
        end
       else
        begin
         WorkBuffer:=WorkBuffer + WorkShort[Count];
        end;
      end;
     WorkShort:=WorkBuffer;
    end;

   {Create ShortName}
   WorkHash:=IntToHex(Hash,4);
   {Find Dot}
   DotPos:=Pos('.',WorkShort);
   if DotPos > 0 then
    begin
     {With Dot}
     if AliasCount < 0 then
      begin
       {No Numeric Tail (FFFFHHHH)}
       WorkFileName:=Copy(WorkShort,1,Min(4,DotPos - 1)) + WorkHash;
      end
     else if AliasCount < 10 then
      begin
       {1 - 9 (FFHHHH~N)}
       WorkFileName:=Copy(WorkShort,1,Min(2,DotPos - 1)) + WorkHash;
      end
     else if AliasCount < 100 then
      begin
       {10 - 99 (FHHHH~NN)}
       WorkFileName:=Copy(WorkShort,1,Min(1,DotPos - 1)) + WorkHash;
      end
     else if AliasCount < 1000 then
      begin
       {100 - 999 (HHHH~NNN)}
       WorkFileName:=WorkHash;
      end;
     WorkFileExt:=Copy(WorkShort,DotPos + 1,3);
    end
   else
    begin
     {Without Dot}
     if AliasCount < 0 then
      begin
       {No Numeric Tail (FFFFHHHH)}
       WorkFileName:=Copy(WorkShort,1,4) + WorkHash;
      end
     else if AliasCount < 10 then
      begin
       {1 - 9 (FFHHHH~N)}
       WorkFileName:=Copy(WorkShort,1,2) + WorkHash;
      end
     else if AliasCount < 100 then
      begin
       {10 - 99 (FHHHH~NN)}
       WorkFileName:=Copy(WorkShort,1,1) + WorkHash;
      end
     else if AliasCount < 1000 then
      begin
       {100 - 999 (HHHH~NNN)}
       WorkFileName:=WorkHash;
      end;
     WorkFileExt:='';
    end;

   {Add the Dot if there is an Extension}
   if WorkFileExt <> '' then WorkFileExt:='.' + WorkFileExt;

   if AliasCount < 0 then
    begin
     {Format the Result}
     Result:=WorkFileName + WorkFileExt;
    end
   else
    begin
     {Format the Result ~x}
     if AliasCount = 0 then AliasCount:=1;
     Result:=WorkFileName + '~' + IntToStr(AliasCount) + WorkFileExt;
    end;
  end;
end;

{==============================================================================}

function WildcardFileMatch(const FileName,Wildcard:String;CaseSensitive:Boolean):Boolean;
var
 Count:Integer;
 WildPos,FilePos:Integer;
 WildByte,FileByte:Integer;
 WildLength,FileLength:Integer;
 WildBuffer,FileBuffer:String;
begin
 {}
 //To Do //This needs to be rewritten somewhat
 //To Do //This routine is not Unicode compliant
 
 Result:=False;
 
 if Length(FileName) = 0 then Exit;
 if Length(Wildcard) = 0 then Exit; 

 WildPos:=1;
 FilePos:=1;
 WildByte:=0;
 FileByte:=0;
 WildLength:=Length(Wildcard);
 FileLength:=Length(FileName);

 {Check for *.*}
 if (Wildcard = '*.*') or (Wildcard = '*') then
  begin
   Result:=True;
   Exit;
  end;
 {Check for a Wildcard of all * (eg **** or just *)}
 for Count:=1 to WildLength do
  begin
   if Wildcard[Count] <> '*' then
    begin
     Break;
    end;
   if Count = WildLength then
    begin
     Result:=True;
     Exit;
    end;
  end;

 {Check for Case Sensitive Match}
 if not CaseSensitive then
  begin
   FileBuffer:=Uppercase(FileName);
   WildBuffer:=Uppercase(Wildcard);
  end
 else
  begin
   FileBuffer:=FileName;
   WildBuffer:=Wildcard;
  end;

 while True do
  begin
   if WildPos > WildLength then
    begin
     Result:=(FilePos >= FileLength);
     Exit;
    end
   else
    begin
     WildByte:=Ord(WildBuffer[WildPos]);
    end;

   if FilePos > FileLength then
    begin
     Result:=False;
     Exit;
    end
   else
    begin
     FileByte:=Ord(FileBuffer[FilePos]);
    end;

   if (WildByte = FileByte) then
    begin
     Inc(FilePos);
     Inc(WildPos);
    end
   else if WildByte = 63 then   {Check for ?}
    begin
     if FileByte <> 46 then  {Check for . ? does not match for .}
      begin
       Inc(FilePos);
       Inc(WildPos);
      end
     else
      begin
       Result:=False;
       Exit;
      end;
    end
   else if WildByte = 42 then   {Check for *}
    begin
     if WildPos = WildLength then  {If the Last Char in Wildcard is * then match}
      begin
       Result:=True;
       Exit;
      end
     else
      begin
       Inc(WildPos);                      {Else step forward in Wildcard}
       WildByte:=Ord(WildBuffer[WildPos]);
       if FilePos = FileLength then
        begin
         //To Do //What to do here ?
        end
       else
        begin                             {Then Step forward in FileName until starts matching again}
         Inc(FilePos);
         FileByte:=Ord(FileBuffer[FilePos]);
         while (FileByte <> WildByte) and (FilePos < FileLength) do
          begin
           Inc(FilePos);
           FileByte:=Ord(FileBuffer[FilePos]);
          end;
        end;
      end;
    end
   else
    begin
     Result:=False;
     Exit;
    end;
  end;
end;

{==============================================================================}

function GetFileInfo(const FileName:String;var FileTime,FileSize:Integer;var FileAttr:LongWord):Boolean;
var
 SearchRec:TSearchRec;
begin
 {}
 Result:=False;
 if Trim(FileName) = '' then Exit;

 if FindFirst(StripTrailingSlash(FileName),faAnyFile,SearchRec) = 0 then
  begin
   if (SearchRec.Attr and faDirectory) <> faDirectory then
    begin
     FileTime:=SearchRec.Time;
     FileSize:=SearchRec.Size;
     FileAttr:=SearchRec.Attr;
     
     Result:=True;
    end;
    
   FindClose(SearchRec);
  end;
end;

{==============================================================================}

function CompareFileInfo(const SourceFile,DestFile:String):Boolean;
var
 DestSearchRec:TSearchRec;
 SourceSearchRec:TSearchRec;
begin
 {}
 Result:=False;
 
 if Trim(DestFile) = '' then Exit;
 if Trim(SourceFile) = '' then Exit;

 if FindFirst(StripTrailingSlash(SourceFile),faAnyFile,SourceSearchRec) = 0 then
  begin
   if (SourceSearchRec.Attr and faDirectory) <> faDirectory then
    begin
     if FindFirst(StripTrailingSlash(SourceFile),faAnyFile,DestSearchRec) = 0 then
      begin
       if (DestSearchRec.Attr and faDirectory) <> faDirectory then
        begin
         if (SourceSearchRec.Time = DestSearchRec.Time) and (SourceSearchRec.Size = DestSearchRec.Size) and (SourceSearchRec.Attr = DestSearchRec.Attr) then
          begin
           Result:=True;
          end; 
        end;
        
       FindClose(DestSearchRec);
      end;
    end;
    
   FindClose(SourceSearchRec);
  end;
end;

{==============================================================================}
{==============================================================================}
{Date Functions} 
function IsCurrentDate(ADate:TDateTime):Boolean;
var
 ADay,AMonth,AYear:Word;
 BDay,BMonth,BYear:Word;
begin
 {}
 Result:=False;
 
 DecodeDate(ADate,AYear,AMonth,ADay);
 DecodeDate(Date,BYear,BMonth,BDay);
 if AYear = BYear then
  begin
   if AMonth = BMonth then
    begin
     if ADay = BDay then
      begin
       Result:=True;
      end;
    end;
  end;
end;

{==============================================================================}

function IsCurrentTime(ATime:TDateTime):Boolean;
var
 AHour,AMinute,ASec,AMSec:Word;
 BHour,BMinute,BSec,BMSec:Word;
begin
 {}
 Result:=False;
 
 DecodeTime(ATime,AHour,AMinute,ASec,AMSec);
 DecodeTime(Time,BHour,BMinute,BSec,BMSec);
 if AHour = BHour then
  begin
   if AMinute = BMinute then
    begin
     Result:=True;
    end;
  end;
end;

{==============================================================================}

function IsCurrentMinute(AMinute:Integer):Boolean;
var
 BHour,BMinute,BSec,BMSec:Word;
begin
 {}
 Result:=False;
 
 DecodeTime(Time,BHour,BMinute,BSec,BMSec);
 if AMinute = BMinute then
  begin
   Result:=True;
  end;
end;

{==============================================================================}

function IsCurrentHour(AHour:Integer):Boolean;
var
 BHour,BMinute,BSec,BMSec:Word;
begin
 {}
 Result:=False;
 
 DecodeTime(Time,BHour,BMinute,BSec,BMSec);
 if AHour = BHour then
  begin
   Result:=True;
  end;
end;

{==============================================================================}

function IsCurrentWeekDay(AWeekDay:Integer):Boolean;
var
 BWeekDay:Integer;
begin
 {}
 Result:=False;
 
 BWeekDay:=DayOfWeek(Date);
 if AWeekDay = BWeekDay then
  begin
   Result:=True;
  end;
end;

{==============================================================================}

function IsCurrentDay(ADay:Integer):Boolean;
var
 BDay,BMonth,BYear:Word;
begin
 {}
 Result:=False;
 
 DecodeDate(Date,BYear,BMonth,BDay);
 if ADay = BDay then
  begin
   Result:=True;
  end;
end;

{==============================================================================}

function IsCurrentMonth(AMonth:Integer):Boolean;
var
 BDay,BMonth,BYear:Word;
begin
 {}
 Result:=False;
 
 DecodeDate(Date,BYear,BMonth,BDay);
 if AMonth = BMonth then
  begin
   Result:=True;
  end;
end;

{==============================================================================}

function IsCurrentYear(AYear:Integer):Boolean;
var
 BDay,BMonth,BYear:Word;
begin
 {}
 Result:=False;
 
 DecodeDate(Date,BYear,BMonth,BDay);
 if AYear = BYear then
  begin
   Result:=True;
  end;
end;

{==============================================================================}

function GetMinute(ADateTime:TDateTime):Integer;
var
 BHour,BMinute,BSec,BMSec:Word;
begin
 {}
 Result:=0;
 try
  DecodeTime(ADateTime,BHour,BMinute,BSec,BMSec);
  
  Result:=BMinute;
 except
  {}
 end;
end;

{==============================================================================}

function GetHour(ADateTime:TDateTime):Integer;
var
 BHour,BMinute,BSec,BMSec:Word;
begin
 {}
 Result:=0;
 try
  DecodeTime(ADateTime,BHour,BMinute,BSec,BMSec);
  
  Result:=BHour;
 except
  {}
 end;
end;

{==============================================================================}

function GetWeekday(ADateTime:TDateTime):Integer;
begin
 {}
 Result:=0;
 try
  Result:=DayOfWeek(ADateTime);
 except
  {}
 end;
end;

{==============================================================================}

function GetDay(ADateTime:TDateTime):Integer;
var
 BDay,BMonth,BYear:Word;
begin
 {}
 Result:=0;
 try
  DecodeDate(ADateTime,BYear,BMonth,BDay);
  
  Result:=BDay;
 except
  {}
 end;
end;

{==============================================================================}

function GetMonth(ADateTime:TDateTime):Integer;
var
 BDay,BMonth,BYear:Word;
begin
 {}
 Result:=0;
 try
  DecodeDate(ADateTime,BYear,BMonth,BDay);
  
  Result:=BMonth;
 except
  {}
 end;
end;

{==============================================================================}

function GetYear(ADateTime:TDateTime):Integer;
var
 BDay,BMonth,BYear:Word;
begin
 {}
 Result:=0;
 try
  DecodeDate(ADateTime,BYear,BMonth,BDay);
  
  Result:=BYear;
 except
  {}
 end;
end;

{==============================================================================}

function SecondsSince(ADateTime:TDateTime):LongWord;
var
 ATimeStamp:TTimeStamp;
 BTimeStamp:TTimeStamp;
 ADays:Integer;
 ASeconds,BSeconds:Integer;
begin
 {}
 Result:=0;
 try
  ATimeStamp:=DateTimeToTimeStamp(Now);
  BTimeStamp:=DateTimeToTimeStamp(ADateTime);
  ADays:=ATimeStamp.Date - BTimeStamp.Date;
  ASeconds:=(ATimeStamp.Time div 1000);
  BSeconds:=(BTimeStamp.Time div 1000);
  
  Result:=(ADays * 86400) + (ASeconds - BSeconds);
 except
  {}
 end;
end;

{==============================================================================}

function MinutesSince(ADateTime:TDateTime):LongWord;
var
 ATimeStamp:TTimeStamp;
 BTimeStamp:TTimeStamp;
 ADays:Integer;
 AMinutes,BMinutes:Integer;
begin
 {}
 Result:=0;
 try
  ATimeStamp:=DateTimeToTimeStamp(Now);
  BTimeStamp:=DateTimeToTimeStamp(ADateTime);
  ADays:=ATimeStamp.Date - BTimeStamp.Date;
  AMinutes:=((ATimeStamp.Time div 1000) div 60);
  BMinutes:=((BTimeStamp.Time div 1000) div 60);
  
  Result:=(ADays * 1440) + (AMinutes - BMinutes);
 except
  {}
 end;
end;

{==============================================================================}

function HoursSince(ADateTime:TDateTime):LongWord;
var
 ATimeStamp:TTimeStamp;
 BTimeStamp:TTimeStamp;
 ADays:Integer;
 AHours,BHours:Integer;
begin
 {}
 Result:=0;
 try
  ATimeStamp:=DateTimeToTimeStamp(Now);
  BTimeStamp:=DateTimeToTimeStamp(ADateTime);
  ADays:=ATimeStamp.Date - BTimeStamp.Date;
  AHours:=(((ATimeStamp.Time div 1000) div 60) div 60);
  BHours:=(((BTimeStamp.Time div 1000) div 60) div 60);
  
  Result:=(ADays * 24) + (AHours - BHours);
 except
  {}
 end;
end;

{==============================================================================}

function DaysSince(ADateTime:TDateTime):LongWord;
var
 ATimeStamp:TTimeStamp;
 BTimeStamp:TTimeStamp;
begin
 {}
 Result:=0;
 try
  ATimeStamp:=DateTimeToTimeStamp(Now);
  BTimeStamp:=DateTimeToTimeStamp(ADateTime);
  
  Result:=ATimeStamp.Date - BTimeStamp.Date;
 except
  {}
 end;
end;

{==============================================================================}

function MonthsSince(ADateTime:TDateTime):LongWord;
var
 ADay,AMonth,AYear:Word;
 BDay,BMonth,BYear:Word;
begin
 {}
 Result:=0;
 try
  DecodeDate(Date,AYear,AMonth,ADay);
  DecodeDate(ADateTime,BYear,BMonth,BDay);
  
  Result:=((AYear * 12) + AMonth) - ((BYear * 12) + BMonth);
 except
  {}
 end;
end;

{==============================================================================}

function YearsSince(ADateTime:TDateTime):LongWord;
var
 ADay,AMonth,AYear:Word;
 BDay,BMonth,BYear:Word;
begin
 {}
 Result:=0;
 try
  DecodeDate(Date,AYear,AMonth,ADay);
  DecodeDate(ADateTime,BYear,BMonth,BDay);
  
  Result:=AYear - BYear;
 except
  {}
 end;
end;

{==============================================================================}

function DayOfYear(ADateTime:TDateTime):LongWord;
var
 ADay,AMonth,AYear:Word;
 ADays:Word;
begin
 {}
 Result:=0;
 try
  DecodeDate(ADateTime,AYear,AMonth,ADay);
  
  ADays:=0;
  case AMonth of
   1:ADays:=0;
   2:ADays:=31;
   3:ADays:=59;
   4:ADays:=90;
   5:ADays:=120;
   6:ADays:=151;
   7:ADays:=181;
   8:ADays:=212;
   9:ADays:=243;
   10:ADays:=273;
   11:ADays:=204;
   12:ADays:=334;
  end;
  
  if (IsLeapYear(AYear)) and (AMonth > 2) then
   begin
    ADays:=ADays + 1;
   end;
   
  Result:=ADays + ADay;
 except
  {}
 end;
end;

{==============================================================================}

function SubtractSeconds(ADateTime:TDateTime;ASeconds:LongWord):TDateTime;
var
 Hours,Minutes,Seconds:Word;
begin
 {}
 Result:=ADateTime;
 try
  if ADateTime < 1 then Exit;
  if ASeconds < 1 then Exit;
  if ASeconds > 86399 then ASeconds:=86399; {Maximum 23:59:59}
  
  Hours:=ASeconds div 3600;
  ASeconds:=ASeconds mod 3600;
  Minutes:=ASeconds div 60;
  Seconds:=ASeconds mod 60;
  
  Result:=(Result - EncodeTime(Hours,Minutes,Seconds,0));
 except
  {}
 end;
end;

{==============================================================================}

function TickCount:LongWord;
{Returns number of milliseconds since Ultibo started using GetTickCount}
begin
 {}
 Result:=Ultibo.GetTickCount;
end;

{==============================================================================}

function NormaliseDate(const ADate:String):String;
begin
 {}
 Result:=RemoveSpaces(ADate);
 
 if DefaultFormatSettings.DateSeparator <> '/' then
  begin
   Result:=ReplaceChar(Result,'/',DefaultFormatSettings.DateSeparator);
  end;
 if DefaultFormatSettings.DateSeparator <> '.' then
  begin
   Result:=ReplaceChar(Result,'.',DefaultFormatSettings.DateSeparator);
  end;
 if DefaultFormatSettings.DateSeparator <> ',' then
  begin
   Result:=ReplaceChar(Result,',',DefaultFormatSettings.DateSeparator);
  end;
 if DefaultFormatSettings.DateSeparator <> '-' then
  begin
   Result:=ReplaceChar(Result,'-',DefaultFormatSettings.DateSeparator);
  end;
 if DefaultFormatSettings.DateSeparator <> '\' then
  begin
   Result:=ReplaceChar(Result,'\',DefaultFormatSettings.DateSeparator);
  end;
end;

{==============================================================================}

function FormatEmailDate(ADateTime:TDateTime):String;
begin
 {}
 Result:=FormatDateTime('ddd, dd mmm yyyy hh:nn:ss "+0000"',ADateTime);
end;

{==============================================================================}

function ShortMonthToMonth(const AShortMonth:String):LongWord;
var
 Count:LongWord;
begin
 {}
 Result:=0;
 
 for Count:=1 to 12 do
  begin
   if Uppercase(DefaultFormatSettings.ShortMonthNames[Count]) = Uppercase(AShortMonth) then
    begin
     Result:=Count;
     Exit;
    end;
  end;
end;

{==============================================================================}

function LongMonthToMonth(const ALongMonth:String):LongWord;
var
 Count:LongWord;
begin
 {}
 Result:=0;
 
 for Count:=1 to 12 do
  begin
   if Uppercase(DefaultFormatSettings.LongMonthNames[Count]) = Uppercase(ALongMonth) then
    begin
     Result:=Count;
     Exit;
    end;
  end;
end;

{==============================================================================}

function TwoDigitYearToYear(AYear:LongWord):LongWord;
var
 Year:LongWord;
 Threshold:LongWord;
begin
 {}
 Result:=AYear;
 
 {Check Year}
 if AYear > 99 then Exit;
 
 {Get Year}
 Year:=GetYear(Date);
 
 {Get Threshold}
 Threshold:=(Year - DefaultFormatSettings.TwoDigitYearCenturyWindow) mod 100;
 
 {Check Threshold}
 if (AYear > Threshold) and (DefaultFormatSettings.TwoDigitYearCenturyWindow > 0) then
  begin
   {Previous Century} 
   Result:=(((Year - DefaultFormatSettings.TwoDigitYearCenturyWindow) div 100) * 100) + AYear;
  end
 else
  begin
   {Current Century}
   Result:=((Year div 100) * 100) + AYear; 
  end;  
end;

{==============================================================================}
{==============================================================================}
{Time Functions} 

{==============================================================================}
{==============================================================================}
{Numeric Functions} 

{==============================================================================}
{==============================================================================}
{Network Functions} 
function CheckEmailAddress(const EmailAddress:String):Boolean;
var
 Count:Integer;
 WorkBuffer:String;
begin
 {}
 Result:=False;
 
 {Get Email}
 WorkBuffer:=StripEmailAddress(EmailAddress);
 if Length(WorkBuffer) = 0 then Exit;
 
 {Check for at least one @}
 if Pos('@',WorkBuffer) = 0 then Exit;
 
 {Check for at least one dot}
 if Pos('.',WorkBuffer) = 0 then Exit;
 
 {Check for any invalid characters}
 for Count:=1 to Length(WorkBuffer) do
  begin
   if WorkBuffer[Count] in InvalidEmailChars then Exit;
  end;

 Result:=True;
end;
  
{==============================================================================}

function StripEmailAddress(const EmailAddress:String):String;
var
 PosIdx:Integer;
 WorkBuffer:String;
begin
 {}
 WorkBuffer:=EmailAddress;
 
 PosIdx:=Pos('<',WorkBuffer);
 if PosIdx <> 0 then
  begin
   WorkBuffer:=Copy(WorkBuffer,PosIdx + 1,Length(WorkBuffer));
  end;
  
 PosIdx:=Pos('>',WorkBuffer);
 if PosIdx <> 0 then
  begin
   WorkBuffer:=Copy(WorkBuffer,1,PosIdx - 1);
  end;
  
 Result:=WorkBuffer;
end;

{==============================================================================}

function FormatEmailAddress(const FullName,EmailAddress:String):String;
begin
 {}
 if Trim(FullName) <> '' then
  begin
   Result:='"' + FullName + '" <' + StripEmailAddress(EmailAddress) + '>';
  end
 else
  begin
   Result:='<' + StripEmailAddress(EmailAddress) + '>';
  end;
end;

{==============================================================================}

function GetUserFromEmail(const EmailAddress:String):String;
var
 PosIdx:Integer;
 WorkBuffer:String;
begin
 {}
 WorkBuffer:=StripEmailAddress(EmailAddress);
 
 PosIdx:=Pos('@',WorkBuffer);
 if PosIdx <> 0 then
  begin
   {Return everything before the @}
   Result:=Copy(WorkBuffer,1,PosIdx - 1);
  end
 else
  begin
   {If there is no @ then return everything}
   Result:=WorkBuffer;
  end;
end;

{==============================================================================}

function GetDomainFromEmail(const EmailAddress:String):String;
var
 PosIdx:Integer;
 WorkBuffer:String;
begin
 {}
 WorkBuffer:=StripEmailAddress(EmailAddress);
 
 PosIdx:=Pos('@',WorkBuffer);
 if PosIdx <> 0 then
  begin
   {Return everything after the @}
   Result:=Copy(WorkBuffer,PosIdx + 1,Length(WorkBuffer));
  end
 else
  begin
   {If there is no @ then return empty}
   Result:='';
  end;
end;

{==============================================================================}
{==============================================================================}
{Misc Functions} 
function CloneSid(ASid:Pointer):Pointer;
var
 Size:LongWord;
begin
 {}
 Result:=nil;
 
 if ASid = nil then Exit;
 
 if Security.IsValidSid(ASid) then
  begin
   Size:=Security.GetLengthSid(ASid);
   if Size = 0 then Exit;
   
   Result:=AllocMem(Size);
   System.Move(ASid^,Result^,Size);
  end;
end;

{==============================================================================}

function CloneDescriptor(ADescriptor:Pointer):Pointer;
var
 Size:LongWord;
 Descriptor:Pointer;
begin
 {}
 Result:=nil;
 
 if ADescriptor = nil then Exit;
 
 if Security.IsValidSecurityDescriptor(ADescriptor) then
  begin
   {Check Control}
   if (PSecurityDescriptor(ADescriptor).Control and SE_SELF_RELATIVE) = SE_SELF_RELATIVE then
    begin
     {Relative Descriptor}
     Size:=Security.GetSecurityDescriptorLength(ADescriptor);
     if Size = 0 then Exit;
     
     Result:=AllocMem(Size);
     System.Move(ADescriptor^,Result^,Size);
    end
   else
    begin
     {Absolute Descriptor}
     Size:=Security.GetSecurityDescriptorLength(ADescriptor);
     if Size = 0 then Exit;
     Descriptor:=AllocMem(Size);
     try
      if Security.MakeSelfRelativeSD(ADescriptor,Descriptor,Size) then
       begin
        Result:=AllocMem(Size);
        System.Move(Descriptor^,Result^,Size);
       end;
     finally
      FreeMem(Descriptor);
     end;
    end;
  end;
end;

{==============================================================================}

function CompareSid(ASid1,ASid2:Pointer):Boolean;
{Caller should first compare the Hashes}
begin
 {}
 Result:=False;
 
 if ASid1 = nil then Exit;
 if ASid2 = nil then Exit;
 
 if Security.IsValidSid(ASid1) then
  begin
   if Security.IsValidSid(ASid2) then
    begin
     Result:=Security.EqualSid(ASid1,ASid2);
    end;
  end;
end;

{==============================================================================}

function CompareDescriptor(ADescriptor1,ADescriptor2:Pointer):Boolean;
{Caller should first compare the Hashes}
var
 Size1:LongWord;
 Size2:LongWord;
 Absolute1:Boolean;
 Absolute2:Boolean;
 Descriptor1:Pointer;
 Descriptor2:Pointer;
begin
 {}
 Result:=False;
 
 if ADescriptor1 = nil then Exit;
 if ADescriptor2 = nil then Exit;
 
 if Security.IsValidSecurityDescriptor(ADescriptor1) then
  begin
   if Security.IsValidSecurityDescriptor(ADescriptor2) then
    begin
     {Check Descriptor1 Control}
     if (PSecurityDescriptor(ADescriptor1).Control and SE_SELF_RELATIVE) = SE_SELF_RELATIVE then
      begin
       {Relative Descriptor}
       Absolute1:=False;
       Descriptor1:=ADescriptor1;
       Size1:=Security.GetSecurityDescriptorLength(ADescriptor1);
       if Size1 = 0 then Exit;
      end
     else
      begin
       {Absolute Descriptor}
       Size1:=Security.GetSecurityDescriptorLength(ADescriptor1);
       if Size1 = 0 then Exit;
       Absolute1:=True;
       Descriptor1:=AllocMem(Size1);
       if not Security.MakeSelfRelativeSD(ADescriptor1,Descriptor1,Size1) then
        begin
         FreeMem(Descriptor1);
         Exit;
        end;
      end;
     {Check Descriptor2 Control}
     if (PSecurityDescriptor(ADescriptor2).Control and SE_SELF_RELATIVE) = SE_SELF_RELATIVE then
      begin
       {Relative Descriptor}
       Absolute2:=False;
       Descriptor2:=ADescriptor2;
       Size2:=Security.GetSecurityDescriptorLength(ADescriptor2);
       if (Size2 = 0) and (Absolute1) then FreeMem(Descriptor1);
       if Size2 = 0 then Exit;
      end
     else
      begin
       {Absolute Descriptor}
       Size2:=Security.GetSecurityDescriptorLength(ADescriptor2);
       if (Size2 = 0) and (Absolute1) then FreeMem(Descriptor1);
       if Size2 = 0 then Exit;
       Absolute2:=True;
       Descriptor2:=AllocMem(Size2);
       if not Security.MakeSelfRelativeSD(ADescriptor2,Descriptor2,Size2) then
        begin
         if Absolute1 then FreeMem(Descriptor1);
         FreeMem(Descriptor2);
         Exit;
        end;
      end;
     if Size1 = Size2 then Result:=SysUtils.CompareMem(Descriptor1,Descriptor2,Size1);
     if Absolute1 then FreeMem(Descriptor1);
     if Absolute2 then FreeMem(Descriptor2);
    end;
  end;
end;

{==============================================================================}

function GenerateSidHash(ASid:Pointer;ASize:LongWord):LongWord;
var
 Hash:LongWord;
 Count:LongWord;
begin
 {}
 Result:=0;
 try
  if ASid = nil then Exit;
  if ASize < SECURITY_MIN_SID_SIZE then Exit;
  
  {Check Revision}
  if PSID(ASid).Revision <> SID_REVISION then Exit;
  
  {Generate Hash}
  Hash:=0;
  Count:=0;
  while Count < ASize do
   begin
    Hash:=LongWord(Pointer(LongWord(ASid) + Count)^) + Rol32(Hash,3);
    Inc(Count,4);
   end;
  
  Result:=Hash;
 except
  {}
 end;
end;

{==============================================================================}

function GenerateDescriptorHash(ADescriptor:Pointer;ASize:LongWord):LongWord;
var
 Hash:LongWord;
 Size:LongWord;
 Count:LongWord;
 Descriptor:Pointer;
begin
 {}
 Result:=0;
 try
  if ADescriptor = nil then Exit;
  if ASize < SECURITY_DESCRIPTOR_MIN_LENGTH then Exit;
  
  {Check Revision}
  if PSecurityDescriptor(ADescriptor).Sbz1 <> 0 then Exit;
  if PSecurityDescriptor(ADescriptor).Revision <> SECURITY_DESCRIPTOR_REVISION1 then Exit;
  
  {Check Control}
  if (PSecurityDescriptor(ADescriptor).Control and SE_SELF_RELATIVE) = SE_SELF_RELATIVE then
   begin
    {Relative Descriptor}
    Hash:=0;
    Count:=0;
    while Count < ASize do
     begin
      Hash:=LongWord(Pointer(LongWord(ADescriptor) + Count)^) + Rol32(Hash,3);
      Inc(Count,4);
     end;
     
    Result:=Hash;
   end
  else
   begin
    {Absolute Descriptor}
    Hash:=0;
    Size:=ASize;
    Descriptor:=AllocMem(Size);
    try
     if Security.MakeSelfRelativeSD(ADescriptor,Descriptor,Size) then
      begin
       Count:=0;
       while Count < Size do
        begin
         Hash:=LongWord(Pointer(LongWord(Descriptor) + Count)^) + Rol32(Hash,3);
         Inc(Count,4);
        end;
        
       Result:=Hash;
      end;
    finally
     FreeMem(Descriptor);
    end;
   end;
 except
  {}
 end;
end;

{==============================================================================}
{==============================================================================}
{Helper Functions} 

{==============================================================================}
{==============================================================================}

initialization
 {Nothing}

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
