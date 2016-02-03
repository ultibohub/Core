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

 ?????
 
References
==========

 Based on sources from OpenSSL, the Linux kernel and others
 
Cryptography
============

 MD5 - md5-internal.c
 AES - aes-internal.c
 DES - des-internal.c
 RC4 - rc4.c
 SHA1 - sha1-internal.c
 SHA256 - sha256-internal.c
 
 RSA - bignum.c / rsa.c
 
 MIME
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Crypto;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads;

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
 //To Do

 {MIME64 constants}
 MIME64EncodingTable:String = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
              
{==============================================================================}
//type
 {Crypto specific types}
 //To Do

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
{MIME64 Functions}
function MIME64EncodeString(const AValue:String):String;
function MIME64DecodeString(const AValue:String):String;

{==============================================================================}
{Crypto Helper Functions}

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
