{
Ultibo ASN.1 interface unit.

Copyright (C) 2018 - SoftOz Pty Ltd.

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

  AXTLS - \ssl\asn1.c - Copyright (c) 2007-2016, Cameron Rich
  WPA Supplicant - \src\tls\asn1.c - Copyright (c) 2006-2014, Jouni Malinen

References
==========

 ASN.1 - https://en.wikipedia.org/wiki/Abstract_Syntax_Notation_One

 DER - https://en.wikipedia.org/wiki/Distinguished_Encoding_Rules

 DER Transfer Syntax - https://msdn.microsoft.com/en-us/library/windows/desktop/bb540801(v=vs.85).aspx

 DER Encoding of ASN.1 Types - https://msdn.microsoft.com/en-us/library/windows/desktop/bb648640(v=vs.85).aspx

Abstract Syntax Notation 1 (ASN.1)
==================================

 Abstract Syntax Notation One (ASN.1) is an interface description language for
 defining data structures that can be serialized and deserialized in a standard,
 cross-platform way. It's broadly used in telecommunications and computer networking,
 and especially in cryptography.

 This unit currently only provides the basic functionality required by X509 to
 read and parse a certificate in DER format.

 It is expected that this unit will be expanded to incorporate additional functions
 over time.

}

{$mode delphi}    {Default to Delphi compatible syntax}
{$H+}             {Default to AnsiString}
{$inline on}      {Allow use of Inline procedures}
{$pointermath on} {Allow pointer arithmetic}

{$IFNDEF FPC_DOTTEDUNITS}
unit ASN1;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Core.GlobalConfig,
  Core.GlobalConst,
  Core.GlobalTypes,
  Core.Platform,
  Core.Threads,
  System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  SysUtils;
{$ENDIF FPC_DOTTEDUNITS}

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {ASN1 specific constants}
 ASN1_TAG_EOC  = $00; {Not used with DER}
 ASN1_TAG_BOOLEAN = $01;
 ASN1_TAG_INTEGER = $02;
 ASN1_TAG_BITSTRING = $03;
 ASN1_TAG_OCTETSTRING = $04;
 ASN1_TAG_NULL  = $05;
 ASN1_TAG_OID  = $06;
 ASN1_TAG_OBJECT_DESCRIPTOR = $07; {Not supported}
 ASN1_TAG_EXTERNAL = $08; {Not supported}
 ASN1_TAG_REAL  = $09; {Not supported}
 ASN1_TAG_ENUMERATED = $0A; {Not supported}
 ASN1_TAG_EMBEDDED_PDV = $0B; {Not supported}
 ASN1_TAG_UTF8STRING = $0C; {Not supported}
 ANS1_TAG_RELATIVE_OID = $0D;
 ASN1_TAG_SEQUENCE = $10; {Constructed}
 ASN1_TAG_SET  = $11;
 ASN1_TAG_NUMERICSTRING = $12; {Not supported}
 ASN1_TAG_PRINTABLESTRING = $13;
 ASN1_TAG_TG1STRING = $14; {Not supported}
 ASN1_TAG_VIDEOTEXSTRING = $15; {Not supported}
 ASN1_TAG_IA5STRING = $16;
 ASN1_TAG_UTCTIME = $17;
 ASN1_TAG_GENERALIZEDTIME = $18; {Not supported}
 ASN1_TAG_GRAPHICSTRING = $19; {Not supported}
 ASN1_TAG_VISIBLESTRING = $1A;
 ASN1_TAG_GENERALSTRING = $1B; {Not supported}
 ASN1_TAG_UNIVERSALSTRING = $1C; {Not supported}
 ASN1_TAG_CHARACTERSTRING = $1D; {Not supported}
 ASN1_TAG_BMPSTRING = $1E; {Not supported}

 ASN1_TAG_LONGFORM = $1F; {Tag is encoded in long form}

 ASN1_CLASS_UNIVERSAL        = 0;
 ASN1_CLASS_APPLICATION      = 1;
 ASN1_CLASS_CONTEXT_SPECIFIC = 2;
 ASN1_CLASS_PRIVATE          = 3;

 ASN1_MAX_OID_LEN = 20;

{==============================================================================}
type
 {ASN1 specific types}
 PASN1Tag = ^TASN1Tag;
 TASN1Tag = record
  TagNumber:LongWord;
  TagClass:LongWord;
  Length:LongWord;
  Constructed:Boolean;
  Contents:PByte;
 end;

 PASN1OID = ^TASN1OID;
 TASN1OID = record
  Len:Integer;
  OID:array[0..ASN1_MAX_OID_LEN - 1] of LongWord;
 end;

{==============================================================================}
{var}
 {ASN1 specific variables}

{==============================================================================}
{ASN1 Functions}
function ASN1GetTag(Buffer:PByte;Len:Integer;var Tag:TASN1Tag):Boolean;

function ASN1ParseOID(Buffer:PByte;Len:Integer;var OID:TASN1OID):Boolean;
function ASN1GetOID(Buffer:PByte;Len:Integer;var OID:TASN1OID;var Next:PByte):Boolean;

function ASN1GetNull(Buffer:PByte;Len:Integer;var Next:PByte):Boolean;

function ASN1ParseInt(Buffer:PByte;Len:Integer;var Value:Integer):Boolean;
function ASN1ParseBigInt(Buffer:PByte;Len:Integer;var Value:PByte;var Size:Integer):Boolean;
function ASN1GetInt(Buffer:PByte;Len:Integer;var Value:Integer;var Next:PByte):Boolean;

function ASN1ParseBoolean(Buffer:PByte;Len:Integer;var Value:Boolean):Boolean;
function ASN1GetBoolean(Buffer:PByte;Len:Integer;var Value:Boolean;var Next:PByte):Boolean;

{==============================================================================}
{ASN1 Helper Functions}
function ASN1OIDEqual(const OID1,OID2:TASN1OID):Boolean;
function ASN1OIDEqualPrefix(const OIDPrefix,OID:TASN1OID):Boolean;

function ASN1OIDToString(const OID:TASN1OID):String;

function ASN1BitStringToLongWord(Buffer:PByte;Len:Integer):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}

{var}
 {ASN1 specific variables}

{==============================================================================}
{==============================================================================}
{ASN1 Functions}
function ASN1GetTag(Buffer:PByte;Len:Integer;var Tag:TASN1Tag):Boolean;
var
 Temp:Byte;
 Next:PByte;
 Last:PByte;
 Value:Byte;
begin
 {}
 Result:=False;

 {Check Buffer}
 if Buffer = nil then Exit;

 {Reset Tag}
 FillChar(Tag,SizeOf(TASN1Tag),0);

 {Get Pointers}
 Next:=Buffer;
 Last:=PByte(Buffer + Len);

 {Get Value}
 Value:=Next^;
 Inc(Next);

 {Get Class and Constructed}
 Tag.TagClass:=Value shr 6;
 Tag.Constructed:=(Value and (1 shl 5)) <> 0;

 {Get Tag}
 if (Value and ASN1_TAG_LONGFORM) = ASN1_TAG_LONGFORM then
  begin
   {Long Form Tag}
   Tag.TagNumber:=0;

   repeat
    if Next >= Last then Exit;

    Temp:=Next^;
    Inc(Next);

    Tag.TagNumber:=(Tag.TagNumber shl 7) or (Temp and $7F);
   until (Temp and $80) = 0;
  end
 else
  begin
   {Short Form Tag}
   Tag.TagNumber:=Value and ASN1_TAG_LONGFORM;
  end;

 {Get Length}
 Temp:=Next^;
 Inc(Next);

 if (Temp and $80) <> 0 then
  begin
   {Check Reserved Length}
   if Temp = $FF then Exit;

   {Get number of subsequent octets (1 to 126)}
   Temp:=Temp and $7F;
   if Temp > 4 then Exit;

   {Long Form Length}
   Tag.Length:=0;
   while Temp > 0 do
    begin
     if Next >= Last then Exit;

     Tag.Length:=(Tag.Length shl 8) or Next^;
     Inc(Next);

     Dec(Temp);
    end;
  end
 else
  begin
   {Short Form Length (0..127 in one octet)}
   Tag.Length:=Temp;
  end;

 {Check Length}
 if (Last < Next) or (Tag.Length > PtrUInt(Last - Next)) then Exit;

 {Get Contents}
 Tag.Contents:=Next;

 Result:=True;
end;

{==============================================================================}

function ASN1ParseOID(Buffer:PByte;Len:Integer;var OID:TASN1OID):Boolean;
{Parse an OID value from the buffer}
var
 Temp:Byte;
 Next:PByte;
 Last:PByte;
 Value:LongWord;
begin
 {}
 Result:=False;

 {Check Buffer}
 if Buffer = nil then Exit;

 {Reset OID}
 FillChar(OID,SizeOf(TASN1OID),0);

 {Get Pointers}
 Next:=Buffer;
 Last:=PByte(Buffer + Len);

 {Decode OID}
 while Next < Last do
  begin
   Value:=0;

   {Decode Value}
   repeat
    if Next >= Last then Exit;

    Temp:=Next^;
    Inc(Next);
    Value:=(Value shl 7) or (Temp and $7F);
   until (Temp and $80) = 0;

   {Check Length}
   if OID.Len >= ASN1_MAX_OID_LEN then Exit;

   {Store Value}
   if OID.Len = 0 then
    begin
     {The first octet encodes the first two object identifier components in (X * 40) + Y formula (X = 0..2)}
     OID.OID[0]:=Value div 40;
     if OID.OID[0] > 2 then OID.OID[0]:=2;
     OID.OID[1]:=Value - (OID.OID[0] * 40);
     OID.Len:=2;
    end
   else
    begin
     OID.OID[OID.Len]:=Value;
     Inc(OID.Len);
    end;
  end;

 Result:=True;
end;

{==============================================================================}

function ASN1GetOID(Buffer:PByte;Len:Integer;var OID:TASN1OID;var Next:PByte):Boolean;
{Read the next ASN1 tag from buffer and decode it as an OID value}
var
 Tag:TASN1Tag;
begin
 {}
 Result:=False;

 {Get Tag}
 if not ASN1GetTag(Buffer,Len,Tag) then Exit;

 {Check Length}
 if Tag.Length = 0 then Exit;

 {Check Class and Number}
 if (Tag.TagClass <> ASN1_CLASS_UNIVERSAL) or (Tag.TagNumber <> ASN1_TAG_OID) then Exit;

 {Get Next}
 Next:=PByte(Tag.Contents + Tag.Length);

 {Parse OID}
 Result:=ASN1ParseOID(Tag.Contents,Tag.Length,OID);
end;

{==============================================================================}

function ASN1GetNull(Buffer:PByte;Len:Integer;var Next:PByte):Boolean;
{Read the next ASN1 tag from buffer and decode it as a null value}
var
 Tag:TASN1Tag;
begin
 {}
 Result:=False;

 {Get Tag}
 if not ASN1GetTag(Buffer,Len,Tag) then Exit;

 {Check Length}
 if Tag.Length = 0 then Exit;

 {Check Class and Number}
 if (Tag.TagClass <> ASN1_CLASS_UNIVERSAL) or (Tag.TagNumber <> ASN1_TAG_NULL) then Exit;

 {Get Next}
 Next:=PByte(Tag.Contents + Tag.Length);

 Result:=True;
end;

{==============================================================================}

function ASN1ParseInt(Buffer:PByte;Len:Integer;var Value:Integer):Boolean;
{Parse an integer value from the buffer}
var
 Count:Integer;
begin
 {}
 Result:=False;

 {Setup Default}
 Value:=0;

 {Check Buffer}
 if Buffer = nil then Exit;

 {Check Length}
 if Len > SizeOf(Integer) then Exit;

 {Get Value}
 for Count:=0 to Len - 1 do
  begin
   Value:=Value shl 8;
   Value:=Value or Buffer[Count];
  end;

 Result:=True;
end;

{==============================================================================}

function ASN1ParseBigInt(Buffer:PByte;Len:Integer;var Value:PByte;var Size:Integer):Boolean;
{Parse a big integer value from the buffer (Does not include the leading negative byte if present)}
{Memory allocated for Value must be freed by the caller when no longer required}
begin
 {}
 Result:=False;

 {Setup Defaults}
 Value:=nil;
 Size:=0;

 {Check Buffer}
 if Buffer = nil then Exit;

 {Check Length}
 if (Len > 1) and (Buffer^ = 0) then
  begin
   {Skip Negative Byte}
   Dec(Len);
   Inc(Buffer);
  end;

 {Allocate Value}
 Value:=GetMem(Len);

 {Import Value}
 System.Move(Buffer^,Value^,Len);
 Size:=Len;

 Result:=True;
end;

{==============================================================================}

function ASN1GetInt(Buffer:PByte;Len:Integer;var Value:Integer;var Next:PByte):Boolean;
{Read the next ASN1 tag from buffer and decode it as an integer value}
var
 Tag:TASN1Tag;
 Count:Integer;
begin
 {}
 Result:=False;

 {Setup Default}
 Value:=0;

 {Get Tag}
 if not ASN1GetTag(Buffer,Len,Tag) then Exit;

 {Check Length}
 if Tag.Length = 0 then Exit;

 {Check Class and Number}
 if (Tag.TagClass <> ASN1_CLASS_UNIVERSAL) or (Tag.TagNumber <> ASN1_TAG_INTEGER) or (Tag.Length > SizeOf(Integer)) then Exit;

 {Get Next}
 Next:=PByte(Tag.Contents + Tag.Length);

 {Get Value}
 for Count:=0 to Tag.Length - 1 do
  begin
   Value:=Value shl 8;
   Value:=Value or Tag.Contents[Count];
  end;

 Result:=True;
end;

{==============================================================================}

function ASN1ParseBoolean(Buffer:PByte;Len:Integer;var Value:Boolean):Boolean;
{Parse a boolean value from the buffer}
begin
 {}
 Result:=False;

 {Setup Default}
 Value:=False;

 {Check Buffer}
 if Buffer = nil then Exit;

 {Check Length}
 if Len = 0 then Exit;

 {Get Value}
 if Buffer^ <> 0 then Value:=True;

 Result:=True;
end;

{==============================================================================}

function ASN1GetBoolean(Buffer:PByte;Len:Integer;var Value:Boolean;var Next:PByte):Boolean;
{Read the next ASN1 tag from buffer and decode it as a boolean value}
var
 Tag:TASN1Tag;
begin
 {}
 Result:=False;

 {Setup Default}
 Value:=False;

 {Get Tag}
 if not ASN1GetTag(Buffer,Len,Tag) then Exit;

 {Check Length}
 if Tag.Length = 0 then Exit;

 {Check Class and Number}
 if (Tag.TagClass <> ASN1_CLASS_UNIVERSAL) or (Tag.TagNumber <> ASN1_TAG_BOOLEAN) then Exit;

 {Get Next}
 Next:=PByte(Tag.Contents + Tag.Length);

 {Get Value}
 if Tag.Contents^ <> 0 then Value:=True;

 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{ASN1 Helper Functions}
function ASN1OIDEqual(const OID1,OID2:TASN1OID):Boolean;
var
 Count:Integer;
begin
 {}
 Result:=False;

 if OID1.Len <> OID2.Len then Exit;

 for Count:=0 to OID1.Len - 1 do
  begin
   if OID1.OID[Count] <> OID2.OID[Count] then
    begin
     Exit;
    end;
  end;

 Result:=True;
end;

{==============================================================================}

function ASN1OIDEqualPrefix(const OIDPrefix,OID:TASN1OID):Boolean;
var
 Count:Integer;
begin
 {}
 Result:=False;

 if OIDPrefix.Len > OID.Len then Exit;

 for Count:=0 to OIDPrefix.Len - 1 do
  begin
   if OID.OID[Count] <> OIDPrefix.OID[Count] then
    begin
     Exit;
    end;
  end;

 Result:=True;
end;

{==============================================================================}

function ASN1OIDToString(const OID:TASN1OID):String;
var
 Count:Integer;
begin
 {}
 Result:='';

 for Count:=0 to OID.Len - 1 do
  begin
   if Count = 0 then
    begin
     Result:=IntToStr(OID.OID[Count]);
    end
   else
    begin
     Result:=Result + '.' + IntToStr(OID.OID[Count]);
    end;
  end;
end;

{==============================================================================}

function ASN1BitStringToLongWord(Buffer:PByte;Len:Integer):LongWord;

 function ASN1RotateBits(Octet:Byte):Byte;
 var
  Value:Byte;
  Count:Integer;
 begin
  {}
  Value:=0;
  for Count:=0 to 7 do
   begin
    Value:=Value shl 1;
    if (Octet and 1) <> 0 then
     begin
      Value:=Value or 1;
     end;
    Octet:=Octet shr 1;
   end;

  Result:=Value;
 end;

var
 Next:PByte;
 Value:LongWord;
begin
 {}
 Next:=Buffer;
 Value:=0;

 {BER requires that unused bits are zero, so we can ignore the number of unused bits}
 Inc(Next);

 if Len >= 2 then
  begin
   Value:=Value or ASN1RotateBits(Next^);
   Inc(Next);
  end;
 if Len >= 3 then
  begin
   Value:=Value or (LongWord(ASN1RotateBits(Next^)) shl 8);
   Inc(Next);
  end;
 if Len >= 4 then
  begin
   Value:=Value or (LongWord(ASN1RotateBits(Next^)) shl 16);
   Inc(Next);
  end;
 if Len >= 5 then
  begin
   Value:=Value or (LongWord(ASN1RotateBits(Next^)) shl 24);
   Inc(Next);
  end;
 if Len >= 6 then
  begin
   //To Do //Error ?
  end;

 Result:=Value;
end;

{==============================================================================}
{==============================================================================}

initialization
 {Nothing}

{==============================================================================}

finalization
 {Nothing}

end.
