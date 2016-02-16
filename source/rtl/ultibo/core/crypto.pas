{
Ultibo Crypto interface unit.

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

 \wpa_supplicant-2.5\src\crypto\md5-internal.c - Copyright (c) 2003-2005, Jouni Malinen
                                                 (Public domain implementation of MD5 by Colin Plumb 1993)
 
 \wpa_supplicant-2.5\src\crypto\sha1-internal.c - Copyright (c) 2003-2005, Jouni Malinen
                                                  (Public domain implementation of SHA1 by Steve Reid and others)
 
 \wpa_supplicant-2.5\src\crypto\sha256-internal.c - Copyright (c) 2003-2011, Jouni Malinen
                                                    (Public domain implementation of SHA256 by Tom St Denis)

 \wpa_supplicant-2.5\src\crypto\aes-internal.c - Copyright (c) 2003-2012, Jouni Malinen
                                                 (Public domain implementation of AES by Vincent Rijmen, Antoon Bosselaers and Paulo Barreto)
 
 \wpa_supplicant-2.5\src\crypto\des-internal.c - Copyright (c) 2006-2009, Jouni Malinen
                                                 (Public domain implementation of DES by Tom St Denis)
 
References
==========

 Based on sources from OpenSSL, WPA supplicant, the Linux kernel and others
 
 RFC3174 - US Secure Hash Algorithm 1 (SHA1) - https://tools.ietf.org/html/rfc3174
 
 RFC2104 - HMAC: Keyed-Hashing for Message Authentication - https://www.ietf.org/rfc/rfc2104.txt

 
Cryptography
============

 MD5 - https://en.wikipedia.org/wiki/MD5
 
 AES (Rijndael) - https://en.wikipedia.org/wiki/Advanced_Encryption_Standard
 
 DES - https://en.wikipedia.org/wiki/Data_Encryption_Standard
 
 3DES - https://en.wikipedia.org/wiki/Triple_DES
 
 RC4 - https://en.wikipedia.org/wiki/RC4
 
 SHA1 - https://en.wikipedia.org/wiki/SHA-1
 
 SHA256 - https://en.wikipedia.org/wiki/SHA-2
 
 RSA - bignum.c / rsa.c
 
 HMAC (Hash based message authenitcation code) - https://en.wikipedia.org/wiki/Hash-based_message_authentication_code
 
 CRC
 
 MIME64 - https://en.wikipedia.org/wiki/MIME
          https://en.wikipedia.org/wiki/Base64
          MIME Base64 is not a cryptographic algorithm but an encoding scheme for binary to text conversion.
          It is included here for convenience as it is used by protocols such as SMTP for encoding plain text passwords.
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Crypto;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,SysUtils;

//To do //See: \wpa_supplicant-2.4\src\crypto
        //See: \wpa_supplicant-2.4\src\tls

//To Do //See also: \source\packages\hash
        //Implementations of CRC, MD5, NTLM, SHA1 etc
        
{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {Crypto specific constants}
 
 {MD5 constants}

 {AES constants}
 
 {DES constants}
 
 {RC4 constants}
 
 {SHA1 constants}
 SHA1_K20 = $5A827999;
 SHA1_K40 = $6ED9EBA1;
 SHA1_K60 = $8F1BBCDC;
 SHA1_K80 = $CA62C1D6;
 
 {SHA256 constants}
 SHA256_K:array[0..63] of LongWord = (
  $428a2f98, $71374491, $b5c0fbcf, $e9b5dba5, $3956c25b, $59f111f1, $923f82a4, $ab1c5ed5,
  $d807aa98, $12835b01, $243185be, $550c7dc3, $72be5d74, $80deb1fe, $9bdc06a7, $c19bf174,
  $e49b69c1, $efbe4786, $0fc19dc6, $240ca1cc, $2de92c6f, $4a7484aa, $5cb0a9dc, $76f988da,
  $983e5152, $a831c66d, $b00327c8, $bf597fc7, $c6e00bf3, $d5a79147, $06ca6351, $14292967,
  $27b70a85, $2e1b2138, $4d2c6dfc, $53380d13, $650a7354, $766a0abb, $81c2c92e, $92722c85,
  $a2bfe8a1, $a81a664b, $c24b8b70, $c76c51a3, $d192e819, $d6990624, $f40e3585, $106aa070,
  $19a4c116, $1e376c08, $2748774c, $34b0bcb5, $391c0cb3, $4ed8aa4a, $5b9cca4f, $682e6ff3,
  $748f82ee, $78a5636f, $84c87814, $8cc70208, $90befffa, $a4506ceb, $bef9a3f7, $c67178f2); 
  {This is the precalculated K array for the SHA256 algorithm}

 {RSA constants}
 
 {CRC constants}
 
 {MIME64 constants}
 MIME64EncodingTable:String = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
              
{==============================================================================}
type
 {Crypto specific types}
 
 {MD5 types}
 PMD5Digest = ^TMD5Digest;
 TMD5Digest = array[0..15] of Byte;
 
 PMD5Block = ^TMD5Block;
 TMD5Block = record
  Data:Pointer;
  Size:LongWord;
  Next:PMD5Block;
 end;
 
 PMD5Context = ^TMD5Context;
 TMD5Context = record
  Data:array[0..63] of Byte;
  Buffer:array[0..3] of LongWord;
  Count:QWord;
 end;

 PMD5ByteBuffer = ^TMD5ByteBuffer;
 TMD5ByteBuffer = array[0..63] of Byte;
 
 PMD5LongBuffer = ^TMD5LongBuffer;
 TMD5LongBuffer = array[0..15] of LongWord;
 
 {AES types}
 
 {DES types}
 
 {RC4 types}
 
 {SHA1 types}
 PSHA1Digest = ^TSHA1Digest;
 TSHA1Digest = array[0..19] of Byte;
 
 PSHA1Block = ^TSHA1Block;
 TSHA1Block = record
  Data:Pointer;
  Size:LongWord;
  Next:PSHA1Block;
 end;
 
 PSHA1Context = ^TSHA1Context;
 TSHA1Context = record
  Data:array[0..63] of Byte;
  State:array[0..4] of LongWord;
  Count:QWord;
 end;
 
 PSHA1ByteBuffer = ^TSHA1ByteBuffer;
 TSHA1ByteBuffer = array[0..63] of Byte;
 
 PSHA1LongBuffer = ^TSHA1LongBuffer;
 TSHA1LongBuffer = array[0..15] of LongWord;
 
 {SHA256 types}
 PSHA256Digest = ^TSHA256Digest;
 TSHA256Digest = array[0..31] of Byte;

 PSHA256Block = ^TSHA256Block;
 TSHA256Block = record
  Data:Pointer;
  Size:LongWord;
  Next:PSHA256Block;
 end;
 
 PSHA256Context = ^TSHA256Context;
 TSHA256Context = record
  Data:array[0..63] of Byte;
  State:array[0..7] of LongWord;
  Count:QWord;
 end;

 PSHA256ByteBuffer = ^TSHA256ByteBuffer;
 TSHA256ByteBuffer = array[0..63] of Byte;
 
 PSHA256LongBuffer = ^TSHA256LongBuffer;
 TSHA256LongBuffer = array[0..15] of LongWord;
 
 PSHA256_W = ^TSHA256_W;
 TSHA256_W = array[0..63] of LongWord; {This is the W array for the SHA256 algorithm}
 
 {RSA types}
 
 {CRC types}
 
 {MIME64 types}
 
{==============================================================================}
var
 {Crypto specific variables}
 CryptoInitialized:Boolean;

 {MIME64 variables}
 MIME64DecodingTable:String;
 
{==============================================================================}
{Initialization Functions}
procedure CryptoInit;

{==============================================================================}
{Crypto Functions}

{==============================================================================}
{MD5 Functions}
function MD5DigestData(Data:PMD5Block;Digest:PMD5Digest):Boolean;
function MD5DigestString(const Value:String;Digest:PMD5Digest):Boolean;

function HMACMD5DigestData(const Key:String;Data:PMD5Block;Digest:PMD5Digest):Boolean;
function HMACMD5DigestString(const Key,Value:String;Digest:PMD5Digest):Boolean;

{==============================================================================}
{AES Functions}
 
{==============================================================================}
{DES Functions}
 
{==============================================================================}
{RC4 Functions}
 
{==============================================================================}
{SHA1 Functions}
function SHA1DigestData(Data:PSHA1Block;Digest:PSHA1Digest):Boolean;
function SHA1DigestString(const Value:String;Digest:PSHA1Digest):Boolean;
 
function HMACSHA1DigestData(const Key:String;Data:PSHA1Block;Digest:PSHA1Digest):Boolean;
function HMACSHA1DigestString(const Key,Value:String;Digest:PSHA1Digest):Boolean;

{==============================================================================}
{SHA256 Functions}
function SHA256DigestData(Data:PSHA256Block;Digest:PSHA256Digest):Boolean;
function SHA256DigestString(const Value:String;Digest:PSHA256Digest):Boolean;
 
function HMACSHA256DigestData(const Key:String;Data:PSHA256Block;Digest:PSHA256Digest):Boolean;
function HMACSHA256DigestString(const Key,Value:String;Digest:PSHA256Digest):Boolean;
 
{==============================================================================}
{RSA Functions}

{==============================================================================}
{CRC Functions}

{==============================================================================}
{MIME64 Functions}
function MIME64EncodeString(const AValue:String):String;
function MIME64DecodeString(const AValue:String):String;

{==============================================================================}
{Crypto Helper Functions}
procedure BytesToLE(Buffer:PByte;Count:LongWord);
procedure BytesToBE(Buffer:PByte;Count:LongWord);

{==============================================================================}
{MD5 Helper Functions}
procedure MD5Init(var Context:TMD5Context);
procedure MD5Update(var Context:TMD5Context;Data:Pointer;Size:LongWord);
procedure MD5Final(var Context:TMD5Context;var Digest:TMD5Digest);
procedure MD5Transform(var Context:TMD5Context;Buffer:Pointer);

function MD5DigestToString(Digest:PMD5Digest):String;

{==============================================================================}
{AES Helper Functions}
 
{==============================================================================}
{DES Helper Functions}
 
{==============================================================================}
{RC4 Helper Functions}
 
{==============================================================================}
{SHA1 Helper Functions}
procedure SHA1Init(var Context:TSHA1Context);
procedure SHA1Update(var Context:TSHA1Context;Data:Pointer;Size:LongWord);
procedure SHA1Final(var Context:TSHA1Context;var Digest:TSHA1Digest);
procedure SHA1Transform(var Context:TSHA1Context;Buffer:Pointer);
 
function SHA1DigestToString(Digest:PSHA1Digest):String;
 
{==============================================================================}
{SHA256 Helper Functions}
procedure SHA256Init(var Context:TSHA256Context);
procedure SHA256Process(var Context:TSHA256Context;Data:Pointer;Size:LongWord);
procedure SHA256Complete(var Context:TSHA256Context;var Digest:TSHA256Digest);
procedure SHA256Compress(var Context:TSHA256Context;Buffer:Pointer);

function SHA256DigestToString(Digest:PSHA256Digest):String;
 
{==============================================================================}
{RSA Helper Functions}

{==============================================================================}
{CRC Helper Functions}

{==============================================================================}
{MIME64 Helper Functions}
procedure MIME64InitTables;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure CryptoInit;
begin
 {}
 {Check Initialized}
 if CryptoInitialized then Exit;
 
 {Init MIME64 Tables}
 MIME64InitTables;
 
 //To Do
 
 CryptoInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Crypto Functions}

{==============================================================================}
{==============================================================================}
{MD5 Functions}
function MD5DigestData(Data:PMD5Block;Digest:PMD5Digest):Boolean;
{Generate a 128 bit MD5 digest (Hash) from the supplied data}
{Data is a linked list which can contain multiple independant blocks to be
 included in the hash. The data block itself does not form part of the hash}
var
 Block:PMD5Block;
 Context:TMD5Context;
begin
 {}
 Result:=False;
 
 {Check Params}
 if Data = nil then Exit;
 if Digest = nil then Exit;
 
 {Init Context}
 MD5Init(Context);

 {Get Block}
 Block:=Data;
 while Block <> nil do
  begin
   {Add Data}
   MD5Update(Context,Block.Data,Block.Size);

   {Get Next}   
   Block:=Block.Next;
  end; 

 {Return Digest}
 MD5Final(Context,Digest^);
 
 Result:=True;
end;

{==============================================================================}

function MD5DigestString(const Value:String;Digest:PMD5Digest):Boolean;
{Generate a 128 bit MD5 digest (Hash) from the supplied string value}
var
 Context:TMD5Context;
begin
 {}
 Result:=False;
 
 {Check Params}
 {if Length(Value) = 0 then Exit;} {Allow blank string}
 if Digest = nil then Exit;
 
 {Init Context}
 MD5Init(Context);
 
 {Add Data}
 MD5Update(Context,PChar(Value),Length(Value));
 
 {Return Digest}
 MD5Final(Context,Digest^);
 
 Result:=True;
end;

{==============================================================================}

function HMACMD5DigestData(const Key:String;Data:PMD5Block;Digest:PMD5Digest):Boolean;
{Generate an MD5 HMAC (Hashed Message Authentication Code) using the Key and Data}
{The MD5 HMAC algorithm is:

 MD5(Key xor oPad, MD5(Key xor iPad, Data))
 
 Where iPad is the byte $36 repeated 64 times
       oPad is the byte $5c repeated 64 times
  
 If Key is more than 64 bytes it will be hashed to Key = MD5(Key) instead
 If Key is less than 64 bytes it will be padded with zeros
 
}
var
 Count:LongWord;
 Block:TMD5Block;
 KeyBlock:TMD5Block;
 KeyLength:PtrUInt;
 KeyDigest:TMD5Digest;
 KeyBuffer:TMD5ByteBuffer;
 PadBuffer:TMD5ByteBuffer;
begin
 {}
 Result:=False;
 
 {Check Params}
 if Data = nil then Exit;
 if Digest = nil then Exit;

 {Get key length}
 KeyLength:=Length(Key);
 
 {Check key length}
 if KeyLength > 64 then
  begin
   {MD5 the key}
   if not MD5DigestString(Key,@KeyDigest) then Exit;
   
   {Zero key buffer}
   FillChar(KeyBuffer[0],SizeOf(TMD5ByteBuffer),0);
   
   {Copy digest to key buffer}
   System.Move(KeyDigest[0],KeyBuffer[0],16);
   
   {Update key length}
   KeyLength:=16;
  end
 else
  begin
   {Zero key buffer}
   FillChar(KeyBuffer[0],SizeOf(TMD5ByteBuffer),0);

   {Copy key to key buffer}
   System.Move(PChar(Key)^,KeyBuffer[0],KeyLength);
  end;  

 {XOR the key buffer with the iPad value}
 for Count:=0 to 63 do
  begin
   PadBuffer[Count]:=KeyBuffer[Count] xor $36;
  end;
 
 {MD5 the key buffer and the data (Inner)}
 KeyBlock.Data:=@PadBuffer;
 KeyBlock.Size:=64;
 KeyBlock.Next:=Data;
 
 if not MD5DigestData(@KeyBlock,Digest) then Exit;
 
 {XOR the key buffer with the oPad value}
 for Count:=0 to 63 do
  begin
   PadBuffer[Count]:=KeyBuffer[Count] xor $5c;
  end;
 
 {MD5 the key buffer and the result of the inner MD5 (Outer)}
 KeyBlock.Data:=@PadBuffer;
 KeyBlock.Size:=64;
 KeyBlock.Next:=@Block;
 
 Block.Data:=Digest;
 Block.Size:=16;
 Block.Next:=nil;
 
 if not MD5DigestData(@KeyBlock,Digest) then Exit;
 
 {Wipe the buffers to prevent leakage}
 FillChar(KeyDigest,SizeOf(TMD5Digest),0);
 FillChar(KeyBuffer,SizeOf(TMD5ByteBuffer),0);
 FillChar(PadBuffer,SizeOf(TMD5ByteBuffer),0);
 
 Result:=True;
end;

{==============================================================================}

function HMACMD5DigestString(const Key,Value:String;Digest:PMD5Digest):Boolean;
{Generate an MD5 HMAC (Hashed Message Authentication Code) using the Key and Value}
{The MD5 HMAC algorithm is:

 MD5(Key xor oPad, MD5(Key xor iPad, Value))
 
 Where iPad is the byte $36 repeated 64 times
       oPad is the byte $5c repeated 64 times
  
 If Key is more than 64 bytes it will be hashed to Key = MD5(Key) instead
 If Key is less than 64 bytes it will be padded with zeros
 
}
var
 Count:LongWord;
 Block:TMD5Block;
 KeyBlock:TMD5Block;
 KeyLength:PtrUInt;
 KeyDigest:TMD5Digest;
 KeyBuffer:TMD5ByteBuffer;
 PadBuffer:TMD5ByteBuffer;
begin
 {}
 Result:=False;
 
 {Check Params}
 {if Length(Key) = 0 then Exit;} {Allow blank string}
 {if Length(Value) = 0 then Exit;} {Allow blank string}
 if Digest = nil then Exit;
 
 {Get key length}
 KeyLength:=Length(Key);
 
 {Check key length}
 if KeyLength > 64 then
  begin
   {MD5 the key}
   if not MD5DigestString(Key,@KeyDigest) then Exit;
   
   {Zero key buffer}
   FillChar(KeyBuffer[0],SizeOf(TMD5ByteBuffer),0);
   
   {Copy digest to key buffer}
   System.Move(KeyDigest[0],KeyBuffer[0],16);
   
   {Update key length}
   KeyLength:=16;
  end
 else
  begin
   {Zero key buffer}
   FillChar(KeyBuffer[0],SizeOf(TMD5ByteBuffer),0);

   {Copy key to key buffer}
   System.Move(PChar(Key)^,KeyBuffer[0],KeyLength);
  end;  

 {XOR the key buffer with the iPad value}
 for Count:=0 to 63 do
  begin
   PadBuffer[Count]:=KeyBuffer[Count] xor $36;
  end;
 
 {MD5 the key buffer and the value (Inner)}
 KeyBlock.Data:=@PadBuffer;
 KeyBlock.Size:=64;
 KeyBlock.Next:=@Block;
 
 Block.Data:=PChar(Value);
 Block.Size:=Length(Value);
 Block.Next:=nil;
 
 if not MD5DigestData(@KeyBlock,Digest) then Exit;
 
 {XOR the key buffer with the oPad value}
 for Count:=0 to 63 do
  begin
   PadBuffer[Count]:=KeyBuffer[Count] xor $5c;
  end;
 
 {MD5 the key buffer and the result of the inner MD5 (Outer)}
 KeyBlock.Data:=@PadBuffer;
 KeyBlock.Size:=64;
 KeyBlock.Next:=@Block;
 
 Block.Data:=Digest;
 Block.Size:=16;
 Block.Next:=nil;
 
 if not MD5DigestData(@KeyBlock,Digest) then Exit;
 
 {Wipe the buffers to prevent leakage}
 FillChar(KeyDigest,SizeOf(TMD5Digest),0);
 FillChar(KeyBuffer,SizeOf(TMD5ByteBuffer),0);
 FillChar(PadBuffer,SizeOf(TMD5ByteBuffer),0);
 
 Result:=True;
end;
 
{==============================================================================}
{==============================================================================}
{AES Functions}
 
{==============================================================================}
{==============================================================================}
{DES Functions}
 
{==============================================================================}
{==============================================================================}
{RC4 Functions}
 
{==============================================================================}
{==============================================================================}
{SHA1 Functions}
function SHA1DigestData(Data:PSHA1Block;Digest:PSHA1Digest):Boolean;
{Generate a 160 bit SHA1 digest (Hash) from the supplied data}
{Data is a linked list which can contain multiple independant blocks to be
 included in the hash. The data block itself does not form part of the hash}
var
 Block:PSHA1Block;
 Context:TSHA1Context;
begin
 {}
 Result:=False;
 
 {Check Params}
 if Data = nil then Exit;
 if Digest = nil then Exit;
 
 {Init Context}
 SHA1Init(Context);

 {Get Block}
 Block:=Data;
 while Block <> nil do
  begin
   {Add Data}
   SHA1Update(Context,Block.Data,Block.Size);

   {Get Next}   
   Block:=Block.Next;
  end; 

 {Return Digest}
 SHA1Final(Context,Digest^);
 
 Result:=True;
end;

{==============================================================================}

function SHA1DigestString(const Value:String;Digest:PSHA1Digest):Boolean;
{Generate a 160 bit SHA1 digest (Hash) from the supplied string value}
var
 Context:TSHA1Context;
begin
 {}
 Result:=False;
 
 {Check Params}
 {if Length(Value) = 0 then Exit;} {Allow blank string}
 if Digest = nil then Exit;
 
 {Init Context}
 SHA1Init(Context);
 
 {Add Data}
 SHA1Update(Context,PChar(Value),Length(Value));
 
 {Return Digest}
 SHA1Final(Context,Digest^);
 
 Result:=True;
end;

{==============================================================================}
 
function HMACSHA1DigestData(const Key:String;Data:PSHA1Block;Digest:PSHA1Digest):Boolean;
{Generate a SHA1 HMAC (Hashed Message Authentication Code) using the Key and Data}
{The SHA1 HMAC algorithm is:

 SHA1(Key xor oPad, SHA1(Key xor iPad, Data))
 
 Where iPad is the byte $36 repeated 64 times
       oPad is the byte $5c repeated 64 times
  
 If Key is more than 64 bytes it will be hashed to Key = SHA1(Key) instead
 If Key is less than 64 bytes it will be padded with zeros
 
}
var
 Count:LongWord;
 Block:TSHA1Block;
 KeyBlock:TSHA1Block;
 KeyLength:PtrUInt;
 KeyDigest:TSHA1Digest;
 KeyBuffer:TSHA1ByteBuffer;
 PadBuffer:TSHA1ByteBuffer;
begin
 {}
 Result:=False;
 
 {Check Params}
 if Data = nil then Exit;
 if Digest = nil then Exit;

 {Get key length}
 KeyLength:=Length(Key);
 
 {Check key length}
 if KeyLength > 64 then
  begin
   {SHA1 the key}
   if not SHA1DigestString(Key,@KeyDigest) then Exit;
   
   {Zero key buffer}
   FillChar(KeyBuffer[0],SizeOf(TSHA1ByteBuffer),0);
   
   {Copy digest to key buffer}
   System.Move(KeyDigest[0],KeyBuffer[0],20);
   
   {Update key length}
   KeyLength:=20;
  end
 else
  begin
   {Zero key buffer}
   FillChar(KeyBuffer[0],SizeOf(TSHA1ByteBuffer),0);

   {Copy key to key buffer}
   System.Move(PChar(Key)^,KeyBuffer[0],KeyLength);
  end;  

 {XOR the key buffer with the iPad value}
 for Count:=0 to 63 do
  begin
   PadBuffer[Count]:=KeyBuffer[Count] xor $36;
  end;
 
 {SHA1 the key buffer and the data (Inner)}
 KeyBlock.Data:=@PadBuffer;
 KeyBlock.Size:=64;
 KeyBlock.Next:=Data;
 
 if not SHA1DigestData(@KeyBlock,Digest) then Exit;
 
 {XOR the key buffer with the oPad value}
 for Count:=0 to 63 do
  begin
   PadBuffer[Count]:=KeyBuffer[Count] xor $5c;
  end;
 
 {SHA1 the key buffer and the result of the inner SHA1 (Outer)}
 KeyBlock.Data:=@PadBuffer;
 KeyBlock.Size:=64;
 KeyBlock.Next:=@Block;
 
 Block.Data:=Digest;
 Block.Size:=20;
 Block.Next:=nil;
 
 if not SHA1DigestData(@KeyBlock,Digest) then Exit;
 
 {Wipe the buffers to prevent leakage}
 FillChar(KeyDigest,SizeOf(TSHA1Digest),0);
 FillChar(KeyBuffer,SizeOf(TSHA1ByteBuffer),0);
 FillChar(PadBuffer,SizeOf(TSHA1ByteBuffer),0);
 
 Result:=True;
end;

{==============================================================================}

function HMACSHA1DigestString(const Key,Value:String;Digest:PSHA1Digest):Boolean;
{Generate a SHA1 HMAC (Hashed Message Authentication Code) using the Key and Value}
{The SHA1 HMAC algorithm is:

 SHA1(Key xor oPad, SHA1(Key xor iPad, Value))
 
 Where iPad is the byte $36 repeated 64 times
       oPad is the byte $5c repeated 64 times
  
 If Key is more than 64 bytes it will be hashed to Key = SHA1(Key) instead
 If Key is less than 64 bytes it will be padded with zeros
 
}
var
 Count:LongWord;
 Block:TSHA1Block;
 KeyBlock:TSHA1Block;
 KeyLength:PtrUInt;
 KeyDigest:TSHA1Digest;
 KeyBuffer:TSHA1ByteBuffer;
 PadBuffer:TSHA1ByteBuffer;
begin
 {}
 Result:=False;
 
 {Check Params}
 {if Length(Key) = 0 then Exit;} {Allow blank string}
 {if Length(Value) = 0 then Exit;} {Allow blank string}
 if Digest = nil then Exit;
 
 {Get key length}
 KeyLength:=Length(Key);
 
 {Check key length}
 if KeyLength > 64 then
  begin
   {SHA1 the key}
   if not SHA1DigestString(Key,@KeyDigest) then Exit;
   
   {Zero key buffer}
   FillChar(KeyBuffer[0],SizeOf(TSHA1ByteBuffer),0);
   
   {Copy digest to key buffer}
   System.Move(KeyDigest[0],KeyBuffer[0],20);
   
   {Update key length}
   KeyLength:=20;
  end
 else
  begin
   {Zero key buffer}
   FillChar(KeyBuffer[0],SizeOf(TSHA1ByteBuffer),0);

   {Copy key to key buffer}
   System.Move(PChar(Key)^,KeyBuffer[0],KeyLength);
  end;  

 {XOR the key buffer with the iPad value}
 for Count:=0 to 63 do
  begin
   PadBuffer[Count]:=KeyBuffer[Count] xor $36;
  end;
 
 {SHA1 the key buffer and the value (Inner)}
 KeyBlock.Data:=@PadBuffer;
 KeyBlock.Size:=64;
 KeyBlock.Next:=@Block;
 
 Block.Data:=PChar(Value);
 Block.Size:=Length(Value);
 Block.Next:=nil;
 
 if not SHA1DigestData(@KeyBlock,Digest) then Exit;
 
 {XOR the key buffer with the oPad value}
 for Count:=0 to 63 do
  begin
   PadBuffer[Count]:=KeyBuffer[Count] xor $5c;
  end;
 
 {SHA1 the key buffer and the result of the inner SHA1 (Outer)}
 KeyBlock.Data:=@PadBuffer;
 KeyBlock.Size:=64;
 KeyBlock.Next:=@Block;
 
 Block.Data:=Digest;
 Block.Size:=20;
 Block.Next:=nil;
 
 if not SHA1DigestData(@KeyBlock,Digest) then Exit;
 
 {Wipe the buffers to prevent leakage}
 FillChar(KeyDigest,SizeOf(TSHA1Digest),0);
 FillChar(KeyBuffer,SizeOf(TSHA1ByteBuffer),0);
 FillChar(PadBuffer,SizeOf(TSHA1ByteBuffer),0);
 
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{SHA256 Functions}
function SHA256DigestData(Data:PSHA256Block;Digest:PSHA256Digest):Boolean;
{Generate a 256 bit SHA256 digest (Hash) from the supplied data}
{Data is a linked list which can contain multiple independant blocks to be
 included in the hash. The data block itself does not form part of the hash}
var
 Block:PSHA256Block;
 Context:TSHA256Context;
begin
 {}
 Result:=False;
 
 {Check Params}
 if Data = nil then Exit;
 if Digest = nil then Exit;
 
 {Init Context}
 SHA256Init(Context);

 {Get Block}
 Block:=Data;
 while Block <> nil do
  begin
   {Add Data}
   SHA256Process(Context,Block.Data,Block.Size);

   {Get Next}   
   Block:=Block.Next;
  end; 

 {Return Digest}
 SHA256Complete(Context,Digest^);
 
 Result:=True;
end;

{==============================================================================}

function SHA256DigestString(const Value:String;Digest:PSHA256Digest):Boolean;
{Generate a 256 bit SHA256 digest (Hash) from the supplied string value}
var
 Context:TSHA256Context;
begin
 {}
 Result:=False;
 
 {Check Params}
 {if Length(Value) = 0 then Exit;} {Allow blank string}
 if Digest = nil then Exit;
 
 {Init Context}
 SHA256Init(Context);
 
 {Add Data}
 SHA256Process(Context,PChar(Value),Length(Value));
 
 {Return Digest}
 SHA256Complete(Context,Digest^);
 
 Result:=True;
end;

{==============================================================================}
 
function HMACSHA256DigestData(const Key:String;Data:PSHA256Block;Digest:PSHA256Digest):Boolean;
{Generate a SHA256 HMAC (Hashed Message Authentication Code) using the Key and Data}
{The SHA256 HMAC algorithm is:

 SHA256(Key xor oPad, SHA256(Key xor iPad, Data))
 
 Where iPad is the byte $36 repeated 64 times
       oPad is the byte $5c repeated 64 times
  
 If Key is more than 64 bytes it will be hashed to Key = SHA256(Key) instead
 If Key is less than 64 bytes it will be padded with zeros
 
}
var
 Count:LongWord;
 Block:TSHA256Block;
 KeyBlock:TSHA256Block;
 KeyLength:PtrUInt;
 KeyDigest:TSHA256Digest;
 KeyBuffer:TSHA256ByteBuffer;
 PadBuffer:TSHA256ByteBuffer;
begin
 {}
 Result:=False;
 
 {Check Params}
 if Data = nil then Exit;
 if Digest = nil then Exit;

 {Get key length}
 KeyLength:=Length(Key);
 
 {Check key length}
 if KeyLength > 64 then
  begin
   {SHA256 the key}
   if not SHA256DigestString(Key,@KeyDigest) then Exit;
   
   {Zero key buffer}
   FillChar(KeyBuffer[0],SizeOf(TSHA256ByteBuffer),0);
   
   {Copy digest to key buffer}
   System.Move(KeyDigest[0],KeyBuffer[0],32);
   
   {Update key length}
   KeyLength:=32;
  end
 else
  begin
   {Zero key buffer}
   FillChar(KeyBuffer[0],SizeOf(TSHA256ByteBuffer),0);

   {Copy key to key buffer}
   System.Move(PChar(Key)^,KeyBuffer[0],KeyLength);
  end;  

 {XOR the key buffer with the iPad value}
 for Count:=0 to 63 do
  begin
   PadBuffer[Count]:=KeyBuffer[Count] xor $36;
  end;
 
 {SHA256 the key buffer and the data (Inner)}
 KeyBlock.Data:=@PadBuffer;
 KeyBlock.Size:=64;
 KeyBlock.Next:=Data;
 
 if not SHA256DigestData(@KeyBlock,Digest) then Exit;
 
 {XOR the key buffer with the oPad value}
 for Count:=0 to 63 do
  begin
   PadBuffer[Count]:=KeyBuffer[Count] xor $5c;
  end;
 
 {SHA256 the key buffer and the result of the inner SHA256 (Outer)}
 KeyBlock.Data:=@PadBuffer;
 KeyBlock.Size:=64;
 KeyBlock.Next:=@Block;
 
 Block.Data:=Digest;
 Block.Size:=32;
 Block.Next:=nil;
 
 if not SHA256DigestData(@KeyBlock,Digest) then Exit;
 
 {Wipe the buffers to prevent leakage}
 FillChar(KeyDigest,SizeOf(TSHA256Digest),0);
 FillChar(KeyBuffer,SizeOf(TSHA256ByteBuffer),0);
 FillChar(PadBuffer,SizeOf(TSHA256ByteBuffer),0);
 
 Result:=True;
end;

{==============================================================================}

function HMACSHA256DigestString(const Key,Value:String;Digest:PSHA256Digest):Boolean;
{Generate a SHA256 HMAC (Hashed Message Authentication Code) using the Key and Value}
{The SHA256 HMAC algorithm is:

 SHA256(Key xor oPad, SHA256(Key xor iPad, Value))
 
 Where iPad is the byte $36 repeated 64 times
       oPad is the byte $5c repeated 64 times
  
 If Key is more than 64 bytes it will be hashed to Key = SHA256(Key) instead
 If Key is less than 64 bytes it will be padded with zeros
 
}
var
 Count:LongWord;
 Block:TSHA256Block;
 KeyBlock:TSHA256Block;
 KeyLength:PtrUInt;
 KeyDigest:TSHA256Digest;
 KeyBuffer:TSHA256ByteBuffer;
 PadBuffer:TSHA256ByteBuffer;
begin
 {}
 Result:=False;
 
 {Check Params}
 {if Length(Key) = 0 then Exit;} {Allow blank string}
 {if Length(Value) = 0 then Exit;} {Allow blank string}
 if Digest = nil then Exit;
 
 {Get key length}
 KeyLength:=Length(Key);
 
 {Check key length}
 if KeyLength > 64 then
  begin
   {SHA256 the key}
   if not SHA256DigestString(Key,@KeyDigest) then Exit;
   
   {Zero key buffer}
   FillChar(KeyBuffer[0],SizeOf(TSHA256ByteBuffer),0);
   
   {Copy digest to key buffer}
   System.Move(KeyDigest[0],KeyBuffer[0],32);
   
   {Update key length}
   KeyLength:=32;
  end
 else
  begin
   {Zero key buffer}
   FillChar(KeyBuffer[0],SizeOf(TSHA256ByteBuffer),0);

   {Copy key to key buffer}
   System.Move(PChar(Key)^,KeyBuffer[0],KeyLength);
  end;  

 {XOR the key buffer with the iPad value}
 for Count:=0 to 63 do
  begin
   PadBuffer[Count]:=KeyBuffer[Count] xor $36;
  end;
 
 {SHA256 the key buffer and the value (Inner)}
 KeyBlock.Data:=@PadBuffer;
 KeyBlock.Size:=64;
 KeyBlock.Next:=@Block;
 
 Block.Data:=PChar(Value);
 Block.Size:=Length(Value);
 Block.Next:=nil;
 
 if not SHA256DigestData(@KeyBlock,Digest) then Exit;
 
 {XOR the key buffer with the oPad value}
 for Count:=0 to 63 do
  begin
   PadBuffer[Count]:=KeyBuffer[Count] xor $5c;
  end;
 
 {SHA256 the key buffer and the result of the inner SHA256 (Outer)}
 KeyBlock.Data:=@PadBuffer;
 KeyBlock.Size:=64;
 KeyBlock.Next:=@Block;
 
 Block.Data:=Digest;
 Block.Size:=32;
 Block.Next:=nil;
 
 if not SHA256DigestData(@KeyBlock,Digest) then Exit;
 
 {Wipe the buffers to prevent leakage}
 FillChar(KeyDigest,SizeOf(TSHA256Digest),0);
 FillChar(KeyBuffer,SizeOf(TSHA256ByteBuffer),0);
 FillChar(PadBuffer,SizeOf(TSHA256ByteBuffer),0);
 
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{RSA Functions}

{==============================================================================}
{==============================================================================}
{CRC Functions}
 
{==============================================================================}
{==============================================================================}
{MIME64 Functions}
function MIME64EncodeString(const AValue:String):String;
begin
 {}
 Result:=AValue; //To Do
 
end;

{==============================================================================}

function MIME64DecodeString(const AValue:String):String;
begin
 {}
 Result:=AValue; //To Do
 
end;

{==============================================================================}
{==============================================================================}
{Crypto Helper Functions}
procedure BytesToLE(Buffer:PByte;Count:LongWord);
{Change the byte order of count longwords in the supplied buffer to little endian}
{$IFDEF FPC_BIG_ENDIAN}
var
 Value:LongWord;
begin
 {}
 while Count > 0 do
  begin
   {Reverse LongWord}
   Value:=(Buffer[3] shl 24) or (Buffer[2] shl 16) or (Buffer[1] shl 8) or Buffer[0]; 
   //To Do //RolDWord implementation as BytesToBE
   
   {Save to buffer}
   PLongWord(Buffer)^:=Value;
   
   {Update buffer}
   Inc(Buffer,4);
   Dec(Count);
  end;
end;
{$ELSE FPC_BIG_ENDIAN}
begin
 {Nothing}
end;
{$ENDIF FPC_BIG_ENDIAN}
{==============================================================================}

procedure BytesToBE(Buffer:PByte;Count:LongWord);
{Change the byte order of count longwords in the supplied buffer to big endian}
{$IFDEF FPC_BIG_ENDIAN}
begin
 {Nothing}
end;
{$ELSE FPC_BIG_ENDIAN}
var
 Value:LongWord;
begin
 {}
 while Count > 0 do
  begin
   {Reverse LongWord}
   //Value:=(Buffer[0] shl 24) or (Buffer[1] shl 16) or (Buffer[2] shl 8) or Buffer[3]; //To Do //Which is better
   Value:=(RolDWord(PLongWord(Buffer)^,24) and $FF00FF00) or (RolDWord(PLongWord(Buffer)^,8) and $00FF00FF);
   
   {Save to buffer}
   PLongWord(Buffer)^:=Value;
   
   {Update buffer}
   Inc(Buffer,4);
   Dec(Count);
  end;
end;
{$ENDIF FPC_BIG_ENDIAN}
{==============================================================================}
{==============================================================================}
{MD5 Helper Functions}
procedure MD5Init(var Context:TMD5Context);
{Initialize an MD5 context with constants}
begin
 {}
 {Set the buffer constants}
 Context.Buffer[0]:=$67452301;
 Context.Buffer[1]:=$efcdab89;
 Context.Buffer[2]:=$98badcfe;
 Context.Buffer[3]:=$10325476;

 {Zero the bit count}
 Context.Count:=0;
end;

{==============================================================================}

procedure MD5Update(var Context:TMD5Context;Data:Pointer;Size:LongWord);
{Add more bytes to the data buffer, add to the hash in 64 byte chunks}
var
 Count:PtrUInt;
 Buffer:Pointer;
begin
 {}
 if Data = nil then Exit;
 if Size = 0 then Exit;
 
 {Get the data buffer count}
 Count:=(Context.Count shr 3) and $3F;
 
 {Update the total bit count}
 Inc(Context.Count,(Size shl 3));
 
 {Check for bytes already buffered}
 if Count > 0 then
  begin
   {Get the data buffer pointer}
   Buffer:=Pointer(PtrUInt(@Context.Data[0]) + Count);
   
   {Get bytes remaining to fill the buffer}
   Count:=64 - Count;
   
   {Check size}
   if Size < Count then
    begin
     {Copy to data buffer}
     System.Move(Data^,Buffer^,Size);
     
     Exit;
    end
   else
    begin
     {Copy to data buffer}
     System.Move(Data^,Buffer^,Count);
     
     {Reverse the bytes}
     BytesToLE(@Context.Data[0],16);
     
     {Add to hash}
     MD5Transform(Context,@Context.Data[0]);
     
     {Update data}
     Inc(Data,Count);
     Dec(Size,Count);
    end; 
  end;
  
 {Process 64 byte chunks of data}
 while Size >= 64 do
  begin
   {Copy to data buffer (Avoid reversing bytes in the source data)}
   System.Move(Data^,Context.Data[0],64);

   {Reverse the bytes}
   BytesToLE(@Context.Data[0],16);

   {Add to hash}
   MD5Transform(Context,@Context.Data[0]);
   
   {Update data}
   Inc(Data,64);
   Dec(Size,64);
  end;
  
 {Copy remaining bytes to data buffer}
 System.Move(Data^,Context.Data[0],Size);
end;

{==============================================================================}

procedure MD5Final(var Context:TMD5Context;var Digest:TMD5Digest);
{Finalize the MD5 context by padding to a 64 Byte boundary, adding 
 QWord count of bits processed and copying the hash to the digest}
var
 Count:PtrUInt;
 Buffer:Pointer;
begin
 {}
 {Get the data buffer count}
 Count:=(Context.Count shr 3) and $3F;
 
 {Get the data buffer pointer}
 Buffer:=Pointer(PtrUInt(@Context.Data[0]) + Count);
 
 {Set the first padding byte to $80}
 PByte(Buffer)^:=$80;
 
 {Update buffer}
 Inc(Buffer);
 
 {Determine bytes of padding to make 64 byte buffer}
 Count:=64 - (Count + 1);
 
 {Pad the buffer to 56 bytes}
 if Count < 8 then
  begin
   {Not enough room for count}
   {Fill with padding to 64 bytes}
   FillChar(Buffer^,Count,0);
   
   {Reverse the bytes}
   BytesToLE(@Context.Data[0],16);
   
   {Add to hash}
   MD5Transform(Context,@Context.Data[0]);
   
   {Add 56 bytes of padding to the buffer}
   FillChar(Context.Data[0],56,0);
  end
 else
  begin
   {Fill with padding to 56 bytes}
   FillChar(Buffer^,Count - 8,0);
  end;  

 {Reverse the bytes}
 BytesToLE(@Context.Data[0],14);
 
 {Add count of bits}
 PLongWord(@Context.Data[56])^:=Int64Rec(Context.Count).Lo; //To Do //LongWordNToLE //See SHA1/256
 PLongWord(@Context.Data[60])^:=Int64Rec(Context.Count).Hi; //To Do //LongWordNToLE //See SHA1/256
 
 {Add to hash}
 MD5Transform(Context,@Context.Data[0]);
 
 {Reverse the hash}
 BytesToLE(@Context.Buffer[0],4);
 
 {Copy the hash to the digest}
 System.Move(Context.Buffer[0],Digest,16);
 
 {Clear the context to prevent data leakage}
 FillChar(Context,SizeOf(TMD5Context),0);
end;

{==============================================================================}

procedure MD5Transform(var Context:TMD5Context;Buffer:Pointer);
{The core MD5 algorithm, adds an additional 64 Bytes (16 LongWords) to the hash}

{$PUSH} {Save compiler settings}
{$R-}   {Disable range checking}
{$Q-}   {Disable overflow checking}

 {MD5Step(W, X, Y, Z, Data, Shift)
  W = W + F?(X, Y, Z) + Data
  W = (W shl Shift) or (W shr (32 - Shift))   //Same as ROL(W,Shift)
  W = W + X
 }
 procedure MD5Step1(var W:LongWord;X,Y,Z:LongWord;Data:LongWord;Shift:Byte); inline;
 {F1(X, Y, Z) = (X and Y) or ((not X) and Z)}
 {F1(X, Y, Z) = (Z xor (X and (Y xor Z)))} {Optimized version}
 begin
  {}
  {W:=X + RolDWord(DWord(W + ((X and Y) or ((not X) and Z)) + Data),Shift);}
  W:=X + RolDWord(DWord(W + (Z xor (X and (Y xor Z))) + Data),Shift); {Optimized version}
 end;

 procedure MD5Step2(var W:LongWord;X,Y,Z:LongWord;Data:LongWord;Shift:Byte); inline;
 {F2(X, Y, Z) = (X and Z) or (Y and (not Z))}
 {F2(X, Y, Z) = F1(Z, X, Y)} {Alternate version}
 begin
  {}
  W:=X + RolDWord(DWord(W + ((X and Z) or (Y and (not Z))) + Data),Shift);
 end;
 
 procedure MD5Step3(var W:LongWord;X,Y,Z:LongWord;Data:LongWord;Shift:Byte); inline;
 {F3(X, Y, Z) = X xor Y xor Z}
 begin
  {}
  W:=X + RolDWord(DWord(W + (X xor Y xor Z) + Data),Shift);
 end;
 
 procedure MD5Step4(var W:LongWord;X,Y,Z:LongWord;Data:LongWord;Shift:Byte); inline;
 {F4(X,Y,Z) = Y xor (X or (not Z))}
 begin
  {}
  W:=X + RolDWord(DWord(W + (Y xor (X or (not Z))) + Data),Shift);
 end;
 
{$POP}  {Restore compiler settings}
var
 A:LongWord;
 B:LongWord;
 C:LongWord;
 D:LongWord;
 Data:PMD5LongBuffer;
begin
 {}
 if Buffer = nil then Exit;
 
 {Get current hash}
 A:=Context.Buffer[0];
 B:=Context.Buffer[1];
 C:=Context.Buffer[2];
 D:=Context.Buffer[3];
 
 {Get data buffer}
 Data:=PMD5LongBuffer(Buffer);
 
 {Step 1}
 MD5Step1(A,B,C,D,Data[0] + $d76aa478,7);
 MD5Step1(D,A,B,C,Data[1] + $e8c7b756,12);
 MD5Step1(C,D,A,B,Data[2] + $242070db,17);
 MD5Step1(B,C,D,A,Data[3] + $c1bdceee,22);
 MD5Step1(A,B,C,D,Data[4] + $f57c0faf,7);
 MD5Step1(D,A,B,C,Data[5] + $4787c62a,12);
 MD5Step1(C,D,A,B,Data[6] + $a8304613,17);
 MD5Step1(B,C,D,A,Data[7] + $fd469501,22);
 MD5Step1(A,B,C,D,Data[8] + $698098d8,7);
 MD5Step1(D,A,B,C,Data[9] + $8b44f7af,12);
 MD5Step1(C,D,A,B,Data[10] + $ffff5bb1,17);
 MD5Step1(B,C,D,A,Data[11] + $895cd7be,22);
 MD5Step1(A,B,C,D,Data[12] + $6b901122,7);
 MD5Step1(D,A,B,C,Data[13] + $fd987193,12);
 MD5Step1(C,D,A,B,Data[14] + $a679438e,17);
 MD5Step1(B,C,D,A,Data[15] + $49b40821,22);

 {Step 2}
 MD5Step2(A,B,C,D,Data[1] + $f61e2562,5);
 MD5Step2(D,A,B,C,Data[6] + $c040b340,9);
 MD5Step2(C,D,A,B,Data[11] + $265e5a51,14);
 MD5Step2(B,C,D,A,Data[0] + $e9b6c7aa,20);
 MD5Step2(A,B,C,D,Data[5] + $d62f105d,5);
 MD5Step2(D,A,B,C,Data[10] + $02441453,9);
 MD5Step2(C,D,A,B,Data[15] + $d8a1e681,14);
 MD5Step2(B,C,D,A,Data[4] + $e7d3fbc8,20);
 MD5Step2(A,B,C,D,Data[9] + $21e1cde6,5);
 MD5Step2(D,A,B,C,Data[14] + $c33707d6,9);
 MD5Step2(C,D,A,B,Data[3] + $f4d50d87,14);
 MD5Step2(B,C,D,A,Data[8] + $455a14ed,20);
 MD5Step2(A,B,C,D,Data[13] + $a9e3e905,5);
 MD5Step2(D,A,B,C,Data[2] + $fcefa3f8,9);
 MD5Step2(C,D,A,B,Data[7] + $676f02d9,14);
 MD5Step2(B,C,D,A,Data[12] + $8d2a4c8a,20);

 {Step 3}
 MD5Step3(A,B,C,D,Data[5] + $fffa3942,4);
 MD5Step3(D,A,B,C,Data[8] + $8771f681,11);
 MD5Step3(C,D,A,B,Data[11] + $6d9d6122,16);
 MD5Step3(B,C,D,A,Data[14] + $fde5380c,23);
 MD5Step3(A,B,C,D,Data[1] + $a4beea44,4);
 MD5Step3(D,A,B,C,Data[4] + $4bdecfa9,11);
 MD5Step3(C,D,A,B,Data[7] + $f6bb4b60,16);
 MD5Step3(B,C,D,A,Data[10] + $bebfbc70,23);
 MD5Step3(A,B,C,D,Data[13] + $289b7ec6,4);
 MD5Step3(D,A,B,C,Data[0] + $eaa127fa,11);
 MD5Step3(C,D,A,B,Data[3] + $d4ef3085,16);
 MD5Step3(B,C,D,A,Data[6] + $04881d05,23);
 MD5Step3(A,B,C,D,Data[9] + $d9d4d039,4);
 MD5Step3(D,A,B,C,Data[12] + $e6db99e5,11);
 MD5Step3(C,D,A,B,Data[15] + $1fa27cf8,16);
 MD5Step3(B,C,D,A,Data[2] + $c4ac5665,23);

 {Step 4}
 MD5Step4(A,B,C,D,Data[0] + $f4292244,6);
 MD5Step4(D,A,B,C,Data[7] + $432aff97,10);
 MD5Step4(C,D,A,B,Data[14] + $ab9423a7,15);
 MD5Step4(B,C,D,A,Data[5] + $fc93a039,21);
 MD5Step4(A,B,C,D,Data[12] + $655b59c3,6);
 MD5Step4(D,A,B,C,Data[3] + $8f0ccc92,10);
 MD5Step4(C,D,A,B,Data[10] + $ffeff47d,15);
 MD5Step4(B,C,D,A,Data[1] + $85845dd1,21);
 MD5Step4(A,B,C,D,Data[8] + $6fa87e4f,6);
 MD5Step4(D,A,B,C,Data[15] + $fe2ce6e0,10);
 MD5Step4(C,D,A,B,Data[6] + $a3014314,15);
 MD5Step4(B,C,D,A,Data[13] + $4e0811a1,21);
 MD5Step4(A,B,C,D,Data[4] + $f7537e82,6);
 MD5Step4(D,A,B,C,Data[11] + $bd3af235,10);
 MD5Step4(C,D,A,B,Data[2] + $2ad7d2bb,15);
 MD5Step4(B,C,D,A,Data[9] + $eb86d391,21);
 
 {Save new hash}
 Inc(Context.Buffer[0],A);
 Inc(Context.Buffer[1],B);
 Inc(Context.Buffer[2],C);
 Inc(Context.Buffer[3],D);
end;

{==============================================================================}

function MD5DigestToString(Digest:PMD5Digest):String;
var
 Count:Integer;
begin
 {}
 Result:='';
 
 if Digest = nil then Exit;
 
 for Count:=0 to 15 do
  begin
   Result:=Result + HexStr(Digest[Count],2);
  end;
 
 Result:=Lowercase(Result);
end;

{==============================================================================}
{==============================================================================}
{AES Helper Functions}
 
{==============================================================================}
{==============================================================================}
{DES Helper Functions}
 
{==============================================================================}
{==============================================================================}
{RC4 Helper Functions}
 
{==============================================================================}
{==============================================================================}
{SHA1 Helper Functions}
procedure SHA1Init(var Context:TSHA1Context);
{Initialize a SHA1 context with constants}
begin
 {}
 {Set the state constants}
 Context.State[0]:=$67452301;
 Context.State[1]:=$EFCDAB89;
 Context.State[2]:=$98BADCFE;
 Context.State[3]:=$10325476;
 Context.State[4]:=$C3D2E1F0;

 {Zero the bit count}
 Context.Count:=0;
end;
 
{==============================================================================}

procedure SHA1Update(var Context:TSHA1Context;Data:Pointer;Size:LongWord);
{Add more bytes to the data buffer, add to the hash in 64 byte chunks}
var
 Count:PtrUInt;
 Offset:PtrUInt;
 Buffer:Pointer;
begin
 {}
 if Data = nil then Exit;
 if Size = 0 then Exit;

 {Get the data buffer count}
 Count:=(Context.Count shr 3) and $3F;
 
 {Update the total bit count}
 Inc(Context.Count,(Size shl 3));
 
 {Check size and buffer count}
 if (Count + Size) > 63 then
  begin
   {Get bytes remaining to fill the buffer}
   Offset:=64 - Count;
   
   {Copy to data buffer}
   System.Move(Data^,Context.Data[Count],Offset);
  
   {Reverse the bytes}
   {BytesToBE(@Context.Data[0],16);} {Done in SHA1Transform}
   
   {Add to hash}
   SHA1Transform(Context,@Context.Data[0]);
  
   {Reset buffer count}
   Count:=0;
   
   {Process 64 byte chunks of data}
   while (Offset + 63) < Size do
    begin
     {Copy to data buffer}
     System.Move(Pointer(PtrUInt(Data) + Offset)^,Context.Data[0],64);
     
     {Reverse the bytes}
     {BytesToBE(@Context.Data[0],16);} {Done in SHA1Transform}
     
     {Add to hash}
     SHA1Transform(Context,@Context.Data[0]);
     
     {Update offset}
     Inc(Offset,64);
    end;
  end
 else
  begin
   {Reset offset}
   Offset:=0;
  end;  
  
 {Copy remaining bytes to data buffer}
 System.Move(Pointer(PtrUInt(Data) + Offset)^,Context.Data[Count],Size - Offset);
end;
 
{==============================================================================}

procedure SHA1Final(var Context:TSHA1Context;var Digest:TSHA1Digest);
{Finalize the SHA1 context by padding to a 64 Byte boundary, adding 
 QWord count of bits processed and copying the hash to the digest}
var
 Total:QWord;
 Count:PtrUInt;
 Buffer:Pointer;
 Padding:TSHA1ByteBuffer;
begin
 {}
 {Get the total bit count}
 Total:=Int64NtoBE(Context.Count);
 
 {Get the data buffer count}
 Count:=(Context.Count shr 3) and $3F;
 
 {Fill the padding with zeros}
 FillChar(Padding[0],SizeOf(TSHA1ByteBuffer),0);

 {Set the first padding byte to $80}
 Padding[0]:=$80;
 
 {Determine bytes of padding to make 64 byte buffer}
 if Count >=56 then
  begin
   Count:=120 - Count;
  end
 else
  begin
   Count:=56 - Count;
  end;
  
 {Pad the buffer to 56 bytes}
 SHA1Update(Context,@Padding[0],Count);
 
 {Add total count of bits}
 SHA1Update(Context,@Total,8);
  
 {Reverse the hash}
 BytesToBE(@Context.State[0],5);
 
 {Copy the hash to the digest}
 System.Move(Context.State[0],Digest,20);
 
 {Clear the context to prevent data leakage}
 FillChar(Context,SizeOf(TSHA1Context),0);
end;
 
{==============================================================================}

procedure SHA1Transform(var Context:TSHA1Context;Buffer:Pointer); 
{The core SHA1 algorithm, adds an additional 64 Bytes (512 bits) to the hash}

 {$IFDEF FPC_BIG_ENDIAN}
 function SHA1Block0(Data:PSHA1LongBuffer;Index:LongWord):LongWord; inline;
 begin
  {}
  Result:=Data[Index];
 end;
 {$ELSE FPC_BIG_ENDIAN}
 function SHA1Block0(Data:PSHA1LongBuffer;Index:LongWord):LongWord; inline;
 begin
  {}
  Result:=(RolDWord(Data[Index],24) and $FF00FF00) or (RolDWord(Data[Index],8) and $00FF00FF);
  Data[Index]:=Result;
 end;
 {$ENDIF FPC_BIG_ENDIAN}
 function SHA1Block(Data:PSHA1LongBuffer;Index:LongWord):LongWord; inline;
 begin
  Result:=RolDWord(Data[(Index + 13) and 15] xor Data[(Index + 8) and 15] xor Data[(Index + 2) and 15] xor Data[Index and 15],1);
  Data[Index and 15]:=Result;
 end;
 
 procedure SHA1Round0(V:LongWord;var W:LongWord;X,Y:LongWord;var Z:LongWord;Data:PSHA1LongBuffer;Index:LongWord); inline;
 begin
  {}
  Z:=Z + ((W and (X xor Y)) xor Y) + SHA1Block0(Data,Index) + $5A827999 + RolDWord(V,5);
  W:=RolDWord(W,30);
 end;

 procedure SHA1Round1(V:LongWord;var W:LongWord;X,Y:LongWord;var Z:LongWord;Data:PSHA1LongBuffer;Index:LongWord); inline;
 begin
  {}
  Z:=Z + ((W and (X xor Y)) xor Y) + SHA1Block(Data,Index) + $5A827999 + RolDWord(V,5);
  W:=RolDWord(W,30);
 end;
 
 procedure SHA1Round2(V:LongWord;var W:LongWord;X,Y:LongWord;var Z:LongWord;Data:PSHA1LongBuffer;Index:LongWord); inline;
 begin
  {}
  Z:=Z + (W xor X xor Y) + SHA1Block(Data,Index) + $6ED9EBA1 + RolDWord(V,5);
  W:=RolDWord(W,30);
 end;
 
 procedure SHA1Round3(V:LongWord;var W:LongWord;X,Y:LongWord;var Z:LongWord;Data:PSHA1LongBuffer;Index:LongWord); inline;
 begin
  {}
  Z:=Z + (((W or X) and Y) or (W and X)) + SHA1Block(Data,Index) + $8F1BBCDC + RolDWord(V,5);
  W:=RolDWord(W,30);
 end;
 
 procedure SHA1Round4(V:LongWord;var W:LongWord;X,Y:LongWord;var Z:LongWord;Data:PSHA1LongBuffer;Index:LongWord); inline;
 begin
  {}
  Z:=Z + (W xor X xor Y) + SHA1Block(Data,Index) + $CA62C1D6 + RolDWord(V,5);
  W:=RolDWord(W,30);
 end;
 
var
 A:LongWord;
 B:LongWord;
 C:LongWord;
 D:LongWord;
 E:LongWord;
 Data:PSHA1LongBuffer;
begin
 {}
 if Buffer = nil then Exit;

 {Get current hash}
 A:=Context.State[0];
 B:=Context.State[1];
 C:=Context.State[2];
 D:=Context.State[3];
 E:=Context.State[4];
 
 {Get data buffer}
 Data:=PSHA1LongBuffer(Buffer);
 
 {Round 0}
 SHA1Round0(A,B,C,D,E,Data,0);
 SHA1Round0(e,a,b,c,d,Data,1);
 SHA1Round0(d,e,a,b,c,Data,2);
 SHA1Round0(c,d,e,a,b,Data,3);
 SHA1Round0(b,c,d,e,a,Data,4);
 SHA1Round0(a,b,c,d,e,Data,5);
 SHA1Round0(e,a,b,c,d,Data,6);
 SHA1Round0(d,e,a,b,c,Data,7);
 SHA1Round0(c,d,e,a,b,Data,8);
 SHA1Round0(b,c,d,e,a,Data,9);
 SHA1Round0(a,b,c,d,e,Data,10);
 SHA1Round0(e,a,b,c,d,Data,11);
 SHA1Round0(d,e,a,b,c,Data,12);
 SHA1Round0(c,d,e,a,b,Data,13);
 SHA1Round0(b,c,d,e,a,Data,14);
 SHA1Round0(a,b,c,d,e,Data,15);
 
 {Round 1}
 SHA1Round1(e,a,b,c,d,Data,16);
 SHA1Round1(d,e,a,b,c,Data,17);
 SHA1Round1(c,d,e,a,b,Data,18);
 SHA1Round1(b,c,d,e,a,Data,19);
 
 {Round 2}
 SHA1Round2(a,b,c,d,e,Data,20);
 SHA1Round2(e,a,b,c,d,Data,21);
 SHA1Round2(d,e,a,b,c,Data,22);
 SHA1Round2(c,d,e,a,b,Data,23);
 SHA1Round2(b,c,d,e,a,Data,24);
 SHA1Round2(a,b,c,d,e,Data,25);
 SHA1Round2(e,a,b,c,d,Data,26);
 SHA1Round2(d,e,a,b,c,Data,27);
 SHA1Round2(c,d,e,a,b,Data,28);
 SHA1Round2(b,c,d,e,a,Data,29);
 SHA1Round2(a,b,c,d,e,Data,30);
 SHA1Round2(e,a,b,c,d,Data,31);
 SHA1Round2(d,e,a,b,c,Data,32);
 SHA1Round2(c,d,e,a,b,Data,33);
 SHA1Round2(b,c,d,e,a,Data,34);
 SHA1Round2(a,b,c,d,e,Data,35);
 SHA1Round2(e,a,b,c,d,Data,36);
 SHA1Round2(d,e,a,b,c,Data,37);
 SHA1Round2(c,d,e,a,b,Data,38);
 SHA1Round2(b,c,d,e,a,Data,39);
 
 {Round 3}
 SHA1Round3(a,b,c,d,e,Data,40);
 SHA1Round3(e,a,b,c,d,Data,41);
 SHA1Round3(d,e,a,b,c,Data,42);
 SHA1Round3(c,d,e,a,b,Data,43);
 SHA1Round3(b,c,d,e,a,Data,44);
 SHA1Round3(a,b,c,d,e,Data,45);
 SHA1Round3(e,a,b,c,d,Data,46);
 SHA1Round3(d,e,a,b,c,Data,47);
 SHA1Round3(c,d,e,a,b,Data,48);
 SHA1Round3(b,c,d,e,a,Data,49);
 SHA1Round3(a,b,c,d,e,Data,50);
 SHA1Round3(e,a,b,c,d,Data,51);
 SHA1Round3(d,e,a,b,c,Data,52);
 SHA1Round3(c,d,e,a,b,Data,53);
 SHA1Round3(b,c,d,e,a,Data,54);
 SHA1Round3(a,b,c,d,e,Data,55);
 SHA1Round3(e,a,b,c,d,Data,56);
 SHA1Round3(d,e,a,b,c,Data,57);
 SHA1Round3(c,d,e,a,b,Data,58);
 SHA1Round3(b,c,d,e,a,Data,59);
 
 {Round 4}
 SHA1Round4(a,b,c,d,e,Data,60);
 SHA1Round4(e,a,b,c,d,Data,61);
 SHA1Round4(d,e,a,b,c,Data,62);
 SHA1Round4(c,d,e,a,b,Data,63);
 SHA1Round4(b,c,d,e,a,Data,64);
 SHA1Round4(a,b,c,d,e,Data,65);
 SHA1Round4(e,a,b,c,d,Data,66);
 SHA1Round4(d,e,a,b,c,Data,67);
 SHA1Round4(c,d,e,a,b,Data,68); 
 SHA1Round4(b,c,d,e,a,Data,69);
 SHA1Round4(a,b,c,d,e,Data,70);
 SHA1Round4(e,a,b,c,d,Data,71);
 SHA1Round4(d,e,a,b,c,Data,72);
 SHA1Round4(c,d,e,a,b,Data,73);
 SHA1Round4(b,c,d,e,a,Data,74);
 SHA1Round4(a,b,c,d,e,Data,75);
 SHA1Round4(e,a,b,c,d,Data,76);
 SHA1Round4(d,e,a,b,c,Data,77);
 SHA1Round4(c,d,e,a,b,Data,78);
 SHA1Round4(b,c,d,e,a,Data,79);
 
 {Save new hash}
 Inc(Context.State[0],A);
 Inc(Context.State[1],B);
 Inc(Context.State[2],C);
 Inc(Context.State[3],D);
 Inc(Context.State[4],E);
end;
 
{==============================================================================}
 
function SHA1DigestToString(Digest:PSHA1Digest):String;
var
 Count:Integer;
begin
 {}
 Result:='';
 
 if Digest = nil then Exit;
 
 for Count:=0 to 19 do
  begin
   Result:=Result + HexStr(Digest[Count],2);
  end;
 
 Result:=Lowercase(Result);
end;
 
{==============================================================================}
{==============================================================================}
{SHA256 Helper Functions}
procedure SHA256Init(var Context:TSHA256Context);
{Initialize a SHA256 context with constants}
begin
 {}
 {Set the state constants}
 Context.State[0]:=$6A09E667;
 Context.State[1]:=$BB67AE85;
 Context.State[2]:=$3C6EF372;
 Context.State[3]:=$A54FF53A;
 Context.State[4]:=$510E527F;
 Context.State[5]:=$9B05688C;
 Context.State[6]:=$1F83D9AB;
 Context.State[7]:=$5BE0CD19;
 
 {Zero the bit count}
 Context.Count:=0;
end;

{==============================================================================}

procedure SHA256Process(var Context:TSHA256Context;Data:Pointer;Size:LongWord);
{Add more bytes to the data buffer, add to the hash in 64 byte chunks}
var
 Count:PtrUInt;
 Offset:PtrUInt;
begin
 {}
 if Data = nil then Exit;
 if Size = 0 then Exit;

 {Get the data buffer count}
 Count:=(Context.Count shr 3) and $3F;
 
 {Update the total bit count}
 Inc(Context.Count,(Size shl 3));
 
 {Check size and buffer count}
 if (Count + Size) > 63 then
  begin
   {Get bytes remaining to fill the buffer}
   Offset:=64 - Count;
   
   {Copy to data buffer}
   System.Move(Data^,Context.Data[Count],Offset);
  
   {Reverse the bytes}
   BytesToBE(@Context.Data[0],16);
   
   {Add to hash}
   SHA256Compress(Context,@Context.Data[0]);
  
   {Reset buffer count}
   Count:=0;
   
   {Process 64 byte chunks of data}
   while (Offset + 63) < Size do
    begin
     {Copy to data buffer}
     System.Move(Pointer(PtrUInt(Data) + Offset)^,Context.Data[0],64);
     
     {Reverse the bytes}
     BytesToBE(@Context.Data[0],16);
     
     {Add to hash}
     SHA256Compress(Context,@Context.Data[0]);
     
     {Update offset}
     Inc(Offset,64);
    end;
  end
 else
  begin
   {Reset offset}
   Offset:=0;
  end;  
  
 {Copy remaining bytes to data buffer}
 System.Move(Pointer(PtrUInt(Data) + Offset)^,Context.Data[Count],Size - Offset);
end;

{==============================================================================}

procedure SHA256Complete(var Context:TSHA256Context;var Digest:TSHA256Digest);
{Finalize the SHA256 context by padding to a 64 Byte boundary, adding 
 QWord count of bits processed and copying the hash to the digest}
var
 Total:QWord;
 Count:PtrUInt;
 Buffer:Pointer;
 Padding:TSHA256ByteBuffer;
begin
 {}
 {Get the total bit count}
 Total:=Int64NtoBE(Context.Count);
 
 {Get the data buffer count}
 Count:=(Context.Count shr 3) and $3F;
 
 {Fill the padding with zeros}
 FillChar(Padding[0],SizeOf(TSHA256ByteBuffer),0);

 {Set the first padding byte to $80}
 Padding[0]:=$80;
 
 {Determine bytes of padding to make 64 byte buffer}
 if Count >=56 then
  begin
   Count:=120 - Count;
  end
 else
  begin
   Count:=56 - Count;
  end;
  
 {Pad the buffer to 56 bytes}
 SHA256Process(Context,@Padding[0],Count);
 
 {Add total count of bits}
 SHA256Process(Context,@Total,8);
  
 {Reverse the hash}
 BytesToBE(@Context.State[0],8);
 
 {Copy the hash to the digest}
 System.Move(Context.State[0],Digest,32);
 
 {Clear the context to prevent data leakage}
 FillChar(Context,SizeOf(TSHA256Context),0);
end;

{==============================================================================}

procedure SHA256Compress(var Context:TSHA256Context;Buffer:Pointer);
{The core SHA256 algorithm, adds an additional 64 Bytes (512 bits) to the hash}
 
 function SHA256RORc(X,Y:LongWord):LongWord; inline; {Not Used}
 begin
  {}
  Result:=(((X and $FFFFFFFF) shr (Y and 31)) or (X shl (32 - (Y and 31)))) and $FFFFFFFF;
 end;
 
 function SHA256Ch(X,Y,Z:LongWord):LongWord; inline;
 begin
  {}
  Result:=Z xor (X and (Y xor Z));
 end;
 
 function SHA256Maj(X,Y,Z:LongWord):LongWord; inline;
 begin
  {}
  Result:=((X or Y) and Z) or (X and Y);
 end;
 
 function SHA256S(X,N:LongWord):LongWord; inline;
 begin
  {}
  {Result:=SHA256RORc(X,N);}
  Result:=RorDWord(X,N);
 end;

 function SHA256R(X,N:LongWord):LongWord; inline;
 begin
  {}
  Result:=(X and $FFFFFFFF) shr N;
 end;

 function SHA256Sigma0(X:LongWord):LongWord; inline;
 begin
  {}
  Result:=SHA256S(X,2) xor SHA256S(X,13) xor SHA256S(X,22);
 end;

 function SHA256Sigma1(X:LongWord):LongWord; inline;
 begin
  {}
  Result:=SHA256S(X,6) xor SHA256S(X,11) xor SHA256S(X,25)
 end;

 function SHA256Gamma0(X:LongWord):LongWord; inline;
 begin
  {}
  Result:=SHA256S(X,7) xor SHA256S(X,18) xor SHA256R(X,3);
 end;

 function SHA256Gamma1(X:LongWord):LongWord; inline;
 begin
  {}
  Result:=SHA256S(X,17) xor SHA256S(X,19) xor SHA256R(X,10);
 end;

 procedure SHA256Round(A,B,C:LongWord;var D:LongWord;E,F,G:LongWord;var H:LongWord;Data:PSHA256_W;Index:LongWord); inline;
 var
  Temp0:LongWord;
  Temp1:LongWord;
 begin
  {}
  Temp0:=H + SHA256Sigma1(E) + SHA256Ch(E,F,G) + SHA256_K[Index] + Data[Index];
  Temp1:=SHA256Sigma0(A) + SHA256Maj(A,B,C);
  D:=D + Temp0;
  H:=Temp0 + Temp1;
 end;
 
var 
 Temp:LongWord;
 Count:LongWord;
 Offset:PtrUInt;
 Data:TSHA256_W;
 State:array[0..7] of LongWord;
begin
 {}
 if Buffer = nil then Exit;
 
 {Copy the state}
 for Count:=0 to 7 do
  begin
   State[Count]:=Context.State[Count];
  end;
  
 {Fill W[0..15] (Data) with the data} 
 Offset:=0;
 for Count:=0 to 15 do
  begin
   {Data[Count]:=LongWordNtoBE(PLongWord(PtrUInt(Buffer) + Offset)^);} {Big endian swap moved to SHA256Process}
   Data[Count]:=PLongWord(PtrUInt(Buffer) + Offset)^;
   Inc(Offset,4);
  end;
 
 {Fill W[16..63] (Data) with extended values}
 for Count:=16 to 63 do
  begin
   Data[Count]:=SHA256Gamma1(Data[Count - 2]) + Data[Count - 7] + SHA256Gamma0(Data[Count - 15]) + Data[Count - 16];
  end;
 
 {Perform the compression rounds}
 for Count:=0 to 63 do
  begin
   {Round X}
   SHA256Round(State[0],State[1],State[2],State[3],State[4],State[5],State[6],State[7],@Data,Count);
   
   {Rotate state}
   Temp:=State[7];
   State[7]:=State[6];
   State[6]:=State[5];
   State[5]:=State[4]; 
   State[4]:=State[3];
   State[3]:=State[2];
   State[2]:=State[1];
   State[1]:=State[0];
   State[0]:=Temp;
  end;
 
 {Save the state}
 for Count:=0 to 7 do
  begin
   Inc(Context.State[Count],State[Count]);
  end;
end;

{==============================================================================}

function SHA256DigestToString(Digest:PSHA256Digest):String;
var
 Count:Integer;
begin
 {}
 Result:='';
 
 if Digest = nil then Exit;
 
 for Count:=0 to 31 do
  begin
   Result:=Result + HexStr(Digest[Count],2);
  end;
 
 Result:=Lowercase(Result);
end;
 
{==============================================================================}
{==============================================================================}
{RSA Helper Functions}

{==============================================================================}
{==============================================================================}
{CRC Helper Functions}

{==============================================================================}
{==============================================================================}
{MIME64 Helper Functions}
procedure MIME64InitTables;
begin
 {}
 //To Do
end;

{==============================================================================}
{==============================================================================}

initialization
 CryptoInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
