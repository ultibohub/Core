{
Ultibo Crypto interface unit.

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

 WPA Supplicant - \src\crypto\md5-internal.c - Copyright (c) 2003-2005, Jouni Malinen
                                                 (Public domain implementation of MD5 by Colin Plumb 1993)

 WPA Supplicant - \src\crypto\sha1-internal.c - Copyright (c) 2003-2005, Jouni Malinen
                                                  (Public domain implementation of SHA1 by Steve Reid and others)

 WPA Supplicant - \src\crypto\sha256-internal.c - Copyright (c) 2003-2011, Jouni Malinen
                                                    (Public domain implementation of SHA256 by Tom St Denis)

 WPA Supplicant - \src\crypto\sha384-internal.c - Copyright (c) 2015, Pali Rohar
                                                    (Public domain implementation of SHA384 by Tom St Denis)

 WPA Supplicant - \src\crypto\sha512-internal.c - Copyright (c) 2015, Pali Rohar
                                                    (Public domain implementation of SHA512 by Tom St Denis)

 WPA Supplicant - \src\crypto\aes-internal.c - Copyright (c) 2003-2012, Jouni Malinen
                                                 (Public domain implementation of AES by Vincent Rijmen, Antoon Bosselaers and Paulo Barreto)

 WPA Supplicant - \src\crypto\des-internal.c - Copyright (c) 2006-2009, Jouni Malinen
                                                 (Public domain implementation of DES by Tom St Denis)

 WPA Supplicant - \src\crypto\rc4.c - Copyright (c) 2002-2005, Jouni Malinen

 WPA Supplicant - \src\utils\base64.c - Copyright (c) 2005-2011, Jouni Malinen

 WPA Supplicant - \src\crypto\aes-ctr.c - Copyright (c) 2003-2007, Jouni Malinen

 WPA Supplicant - \src\crypto\aes-gcm.c - Copyright (c) 2012, Jouni Malinen

 AXTLS - \crypto\rsa.c - Copyright (c) 2007-2014, Cameron Rich

 WPA Supplicant - \src\tls\rsa.c - Copyright (c) 2006-2014, Jouni Malinen

 WPA Supplicant - \src\tls\pkcs1.c - Copyright (c) 2006-2014, Jouni Malinen

References
==========

 Based on sources from OpenSSL, WPA supplicant, LibTomCrypt the Linux kernel and others

 RFC3174 - US Secure Hash Algorithm 1 (SHA1) - https://tools.ietf.org/html/rfc3174

 RFC4634 - US Secure Hash Algorithms (SHA and HMAC-SHA) - https://tools.ietf.org/html/rfc4634

 RFC2104 - HMAC: Keyed-Hashing for Message Authentication - https://www.ietf.org/rfc/rfc2104.txt

 RFC1341 - MIME  (Multipurpose Internet Mail Extensions) - https://tools.ietf.org/html/rfc1341

 RFC2313 - PKCS #1: RSA Encryption Version 1.5 - https://tools.ietf.org/html/rfc2313

Cryptography
============

 MD5 - https://en.wikipedia.org/wiki/MD5

 AES (Rijndael) - https://en.wikipedia.org/wiki/Advanced_Encryption_Standard

 DES - https://en.wikipedia.org/wiki/Data_Encryption_Standard
       https://en.wikipedia.org/wiki/DES_supplementary_material
       https://en.wikipedia.org/wiki/Block_cipher_mode_of_operation

 3DES - https://en.wikipedia.org/wiki/Triple_DES
        https://en.wikipedia.org/wiki/DES_supplementary_material
        https://en.wikipedia.org/wiki/Block_cipher_mode_of_operation

 RC4 - https://en.wikipedia.org/wiki/RC4

 SHA1 - https://en.wikipedia.org/wiki/SHA-1

 SHA256 - https://en.wikipedia.org/wiki/SHA-2

 SHA384 - https://en.wikipedia.org/wiki/SHA-2

 SHA512 - https://en.wikipedia.org/wiki/SHA-2

 RSA - https://simple.wikipedia.org/wiki/RSA_algorithm
       https://en.wikipedia.org/wiki/PKCS
       https://en.wikipedia.org/wiki/PKCS_1

 ECC - https://en.wikipedia.org/wiki/Elliptic-curve_cryptography

 DSA - https://en.wikipedia.org/wiki/Digital_Signature_Algorithm

 ECDSA - https://en.wikipedia.org/wiki/Elliptic_Curve_Digital_Signature_Algorithm

 DH (Diffie Hellman) - https://en.wikipedia.org/wiki/Diffie%E2%80%93Hellman_key_exchange

 ECDH / ECDHE - https://en.wikipedia.org/wiki/Elliptic-curve_Diffie%E2%80%93Hellman

 HMAC (Hash based message authenitcation code) - https://en.wikipedia.org/wiki/Hash-based_message_authentication_code

 CRC - https://en.wikipedia.org/wiki/Cyclic_redundancy_check
       CRC is not a cryptographic algorithm but an error checking scheme for binary data.
       It is included here for convenience as it is used by some drivers such as Wireless USB drivers for firmware checking.

 Base64 - https://en.wikipedia.org/wiki/MIME
          https://en.wikipedia.org/wiki/Base64
          MIME Base64 is not a cryptographic algorithm but an encoding scheme for binary to text conversion.
          It is included here for convenience as it is used by protocols such as SMTP for encoding plain text passwords.

 Cipher Modes - https://en.wikipedia.org/wiki/Block_cipher_mode_of_operation

 Galois/Counter Mode (GCM) - https://en.wikipedia.org/wiki/Galois/Counter_Mode

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Crypto;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,BigInt,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {Crypto specific constants}

 {Cipher algorithms}
 CRYPTO_CIPHER_ALG_NONE = 0;
 CRYPTO_CIPHER_ALG_AES  = 1;
 CRYPTO_CIPHER_ALG_DES  = 2;
 CRYPTO_CIPHER_ALG_3DES = 3;
 CRYPTO_CIPHER_ALG_RC4  = 4;

 {Cipher modes}
 CRYPTO_CIPHER_MODE_ECB = 0; {Electronic Codebook}
 CRYPTO_CIPHER_MODE_CBC = 1; {Cipher Block Chaining}
 CRYPTO_CIPHER_MODE_CFB = 2; {Cipher Feedback}
 CRYPTO_CIPHER_MODE_OFB = 3; {Output Feedback}
 CRYPTO_CIPHER_MODE_CTR = 4; {Counter}
 CRYPTO_CIPHER_MODE_CCM = 5; {Counter with CBC-MAC}
 CRYPTO_CIPHER_MODE_GCM = 5; {Galois/Counter Mode}

 {Hash algorithms}
 CRYPTO_HASH_ALG_NONE        = 0;
 CRYPTO_HASH_ALG_MD5         = 1;
 CRYPTO_HASH_ALG_SHA1        = 2;
 CRYPTO_HASH_ALG_SHA256      = 3;
 CRYPTO_HASH_ALG_HMAC_MD5    = 4;
 CRYPTO_HASH_ALG_HMAC_SHA1   = 5;
 CRYPTO_HASH_ALG_HMAC_SHA256 = 6;
 CRYPTO_HASH_ALG_SHA384      = 7;
 CRYPTO_HASH_ALG_SHA512      = 8;
 CRYPTO_HASH_ALG_HMAC_SHA384 = 9;
 CRYPTO_HASH_ALG_HMAC_SHA512 = 10;

 {MD5 constants}

 {AES constants}
 AES_BLOCK_SIZE = 16;  {128 bit blocks}

 AES_KEY_SIZE128 = 16; {128 bit keys}
 AES_KEY_SIZE192 = 24; {192 bit keys}
 AES_KEY_SIZE256 = 32; {256 bit keys}

 {Precomputed tables for AES}
 {Encrypting}
 {Te0[x] = S [x].[02, 01, 01, 03]}
 {Te1[x] = S [x].[03, 02, 01, 01]}
 {Te2[x] = S [x].[01, 03, 02, 01]}
 {Te3[x] = S [x].[01, 01, 03, 02]}
 {Te4[x] = S [x].[01, 01, 01, 01]}

 {Decrypting}
 {Td0[x] = Si[x].[0e, 09, 0d, 0b]}
 {Td1[x] = Si[x].[0b, 0e, 09, 0d]}
 {Td2[x] = Si[x].[0d, 0b, 0e, 09]}
 {Td3[x] = Si[x].[09, 0d, 0b, 0e]}
 {Td4[x] = Si[x].[01, 01, 01, 01]}

 AES_TE0:array[0..255] of LongWord = (
  $c66363a5, $f87c7c84, $ee777799, $f67b7b8d,
  $fff2f20d, $d66b6bbd, $de6f6fb1, $91c5c554,
  $60303050, $02010103, $ce6767a9, $562b2b7d,
  $e7fefe19, $b5d7d762, $4dababe6, $ec76769a,
  $8fcaca45, $1f82829d, $89c9c940, $fa7d7d87,
  $effafa15, $b25959eb, $8e4747c9, $fbf0f00b,
  $41adadec, $b3d4d467, $5fa2a2fd, $45afafea,
  $239c9cbf, $53a4a4f7, $e4727296, $9bc0c05b,
  $75b7b7c2, $e1fdfd1c, $3d9393ae, $4c26266a,
  $6c36365a, $7e3f3f41, $f5f7f702, $83cccc4f,
  $6834345c, $51a5a5f4, $d1e5e534, $f9f1f108,
  $e2717193, $abd8d873, $62313153, $2a15153f,
  $0804040c, $95c7c752, $46232365, $9dc3c35e,
  $30181828, $379696a1, $0a05050f, $2f9a9ab5,
  $0e070709, $24121236, $1b80809b, $dfe2e23d,
  $cdebeb26, $4e272769, $7fb2b2cd, $ea75759f,
  $1209091b, $1d83839e, $582c2c74, $341a1a2e,
  $361b1b2d, $dc6e6eb2, $b45a5aee, $5ba0a0fb,
  $a45252f6, $763b3b4d, $b7d6d661, $7db3b3ce,
  $5229297b, $dde3e33e, $5e2f2f71, $13848497,
  $a65353f5, $b9d1d168, $00000000, $c1eded2c,
  $40202060, $e3fcfc1f, $79b1b1c8, $b65b5bed,
  $d46a6abe, $8dcbcb46, $67bebed9, $7239394b,
  $944a4ade, $984c4cd4, $b05858e8, $85cfcf4a,
  $bbd0d06b, $c5efef2a, $4faaaae5, $edfbfb16,
  $864343c5, $9a4d4dd7, $66333355, $11858594,
  $8a4545cf, $e9f9f910, $04020206, $fe7f7f81,
  $a05050f0, $783c3c44, $259f9fba, $4ba8a8e3,
  $a25151f3, $5da3a3fe, $804040c0, $058f8f8a,
  $3f9292ad, $219d9dbc, $70383848, $f1f5f504,
  $63bcbcdf, $77b6b6c1, $afdada75, $42212163,
  $20101030, $e5ffff1a, $fdf3f30e, $bfd2d26d,
  $81cdcd4c, $180c0c14, $26131335, $c3ecec2f,
  $be5f5fe1, $359797a2, $884444cc, $2e171739,
  $93c4c457, $55a7a7f2, $fc7e7e82, $7a3d3d47,
  $c86464ac, $ba5d5de7, $3219192b, $e6737395,
  $c06060a0, $19818198, $9e4f4fd1, $a3dcdc7f,
  $44222266, $542a2a7e, $3b9090ab, $0b888883,
  $8c4646ca, $c7eeee29, $6bb8b8d3, $2814143c,
  $a7dede79, $bc5e5ee2, $160b0b1d, $addbdb76,
  $dbe0e03b, $64323256, $743a3a4e, $140a0a1e,
  $924949db, $0c06060a, $4824246c, $b85c5ce4,
  $9fc2c25d, $bdd3d36e, $43acacef, $c46262a6,
  $399191a8, $319595a4, $d3e4e437, $f279798b,
  $d5e7e732, $8bc8c843, $6e373759, $da6d6db7,
  $018d8d8c, $b1d5d564, $9c4e4ed2, $49a9a9e0,
  $d86c6cb4, $ac5656fa, $f3f4f407, $cfeaea25,
  $ca6565af, $f47a7a8e, $47aeaee9, $10080818,
  $6fbabad5, $f0787888, $4a25256f, $5c2e2e72,
  $381c1c24, $57a6a6f1, $73b4b4c7, $97c6c651,
  $cbe8e823, $a1dddd7c, $e874749c, $3e1f1f21,
  $964b4bdd, $61bdbddc, $0d8b8b86, $0f8a8a85,
  $e0707090, $7c3e3e42, $71b5b5c4, $cc6666aa,
  $904848d8, $06030305, $f7f6f601, $1c0e0e12,
  $c26161a3, $6a35355f, $ae5757f9, $69b9b9d0,
  $17868691, $99c1c158, $3a1d1d27, $279e9eb9,
  $d9e1e138, $ebf8f813, $2b9898b3, $22111133,
  $d26969bb, $a9d9d970, $078e8e89, $339494a7,
  $2d9b9bb6, $3c1e1e22, $15878792, $c9e9e920,
  $87cece49, $aa5555ff, $50282878, $a5dfdf7a,
  $038c8c8f, $59a1a1f8, $09898980, $1a0d0d17,
  $65bfbfda, $d7e6e631, $844242c6, $d06868b8,
  $824141c3, $299999b0, $5a2d2d77, $1e0f0f11,
  $7bb0b0cb, $a85454fc, $6dbbbbd6, $2c16163a);

 {$IFDEF CRYPTO_AES_PRECOMPUTED_TABLES}
 AES_TE1:array[0..255] of LongWord = (
  $a5c66363, $84f87c7c, $99ee7777, $8df67b7b,
  $0dfff2f2, $bdd66b6b, $b1de6f6f, $5491c5c5,
  $50603030, $03020101, $a9ce6767, $7d562b2b,
  $19e7fefe, $62b5d7d7, $e64dabab, $9aec7676,
  $458fcaca, $9d1f8282, $4089c9c9, $87fa7d7d,
  $15effafa, $ebb25959, $c98e4747, $0bfbf0f0,
  $ec41adad, $67b3d4d4, $fd5fa2a2, $ea45afaf,
  $bf239c9c, $f753a4a4, $96e47272, $5b9bc0c0,
  $c275b7b7, $1ce1fdfd, $ae3d9393, $6a4c2626,
  $5a6c3636, $417e3f3f, $02f5f7f7, $4f83cccc,
  $5c683434, $f451a5a5, $34d1e5e5, $08f9f1f1,
  $93e27171, $73abd8d8, $53623131, $3f2a1515,
  $0c080404, $5295c7c7, $65462323, $5e9dc3c3,
  $28301818, $a1379696, $0f0a0505, $b52f9a9a,
  $090e0707, $36241212, $9b1b8080, $3ddfe2e2,
  $26cdebeb, $694e2727, $cd7fb2b2, $9fea7575,
  $1b120909, $9e1d8383, $74582c2c, $2e341a1a,
  $2d361b1b, $b2dc6e6e, $eeb45a5a, $fb5ba0a0,
  $f6a45252, $4d763b3b, $61b7d6d6, $ce7db3b3,
  $7b522929, $3edde3e3, $715e2f2f, $97138484,
  $f5a65353, $68b9d1d1, $00000000, $2cc1eded,
  $60402020, $1fe3fcfc, $c879b1b1, $edb65b5b,
  $bed46a6a, $468dcbcb, $d967bebe, $4b723939,
  $de944a4a, $d4984c4c, $e8b05858, $4a85cfcf,
  $6bbbd0d0, $2ac5efef, $e54faaaa, $16edfbfb,
  $c5864343, $d79a4d4d, $55663333, $94118585,
  $cf8a4545, $10e9f9f9, $06040202, $81fe7f7f,
  $f0a05050, $44783c3c, $ba259f9f, $e34ba8a8,
  $f3a25151, $fe5da3a3, $c0804040, $8a058f8f,
  $ad3f9292, $bc219d9d, $48703838, $04f1f5f5,
  $df63bcbc, $c177b6b6, $75afdada, $63422121,
  $30201010, $1ae5ffff, $0efdf3f3, $6dbfd2d2,
  $4c81cdcd, $14180c0c, $35261313, $2fc3ecec,
  $e1be5f5f, $a2359797, $cc884444, $392e1717,
  $5793c4c4, $f255a7a7, $82fc7e7e, $477a3d3d,
  $acc86464, $e7ba5d5d, $2b321919, $95e67373,
  $a0c06060, $98198181, $d19e4f4f, $7fa3dcdc,
  $66442222, $7e542a2a, $ab3b9090, $830b8888,
  $ca8c4646, $29c7eeee, $d36bb8b8, $3c281414,
  $79a7dede, $e2bc5e5e, $1d160b0b, $76addbdb,
  $3bdbe0e0, $56643232, $4e743a3a, $1e140a0a,
  $db924949, $0a0c0606, $6c482424, $e4b85c5c,
  $5d9fc2c2, $6ebdd3d3, $ef43acac, $a6c46262,
  $a8399191, $a4319595, $37d3e4e4, $8bf27979,
  $32d5e7e7, $438bc8c8, $596e3737, $b7da6d6d,
  $8c018d8d, $64b1d5d5, $d29c4e4e, $e049a9a9,
  $b4d86c6c, $faac5656, $07f3f4f4, $25cfeaea,
  $afca6565, $8ef47a7a, $e947aeae, $18100808,
  $d56fbaba, $88f07878, $6f4a2525, $725c2e2e,
  $24381c1c, $f157a6a6, $c773b4b4, $5197c6c6,
  $23cbe8e8, $7ca1dddd, $9ce87474, $213e1f1f,
  $dd964b4b, $dc61bdbd, $860d8b8b, $850f8a8a,
  $90e07070, $427c3e3e, $c471b5b5, $aacc6666,
  $d8904848, $05060303, $01f7f6f6, $121c0e0e,
  $a3c26161, $5f6a3535, $f9ae5757, $d069b9b9,
  $91178686, $5899c1c1, $273a1d1d, $b9279e9e,
  $38d9e1e1, $13ebf8f8, $b32b9898, $33221111,
  $bbd26969, $70a9d9d9, $89078e8e, $a7339494,
  $b62d9b9b, $223c1e1e, $92158787, $20c9e9e9,
  $4987cece, $ffaa5555, $78502828, $7aa5dfdf,
  $8f038c8c, $f859a1a1, $80098989, $171a0d0d,
  $da65bfbf, $31d7e6e6, $c6844242, $b8d06868,
  $c3824141, $b0299999, $775a2d2d, $111e0f0f,
  $cb7bb0b0, $fca85454, $d66dbbbb, $3a2c1616);

 AES_TE2:array[0..255] of LongWord = (
  $63a5c663, $7c84f87c, $7799ee77, $7b8df67b,
  $f20dfff2, $6bbdd66b, $6fb1de6f, $c55491c5,
  $30506030, $01030201, $67a9ce67, $2b7d562b,
  $fe19e7fe, $d762b5d7, $abe64dab, $769aec76,
  $ca458fca, $829d1f82, $c94089c9, $7d87fa7d,
  $fa15effa, $59ebb259, $47c98e47, $f00bfbf0,
  $adec41ad, $d467b3d4, $a2fd5fa2, $afea45af,
  $9cbf239c, $a4f753a4, $7296e472, $c05b9bc0,
  $b7c275b7, $fd1ce1fd, $93ae3d93, $266a4c26,
  $365a6c36, $3f417e3f, $f702f5f7, $cc4f83cc,
  $345c6834, $a5f451a5, $e534d1e5, $f108f9f1,
  $7193e271, $d873abd8, $31536231, $153f2a15,
  $040c0804, $c75295c7, $23654623, $c35e9dc3,
  $18283018, $96a13796, $050f0a05, $9ab52f9a,
  $07090e07, $12362412, $809b1b80, $e23ddfe2,
  $eb26cdeb, $27694e27, $b2cd7fb2, $759fea75,
  $091b1209, $839e1d83, $2c74582c, $1a2e341a,
  $1b2d361b, $6eb2dc6e, $5aeeb45a, $a0fb5ba0,
  $52f6a452, $3b4d763b, $d661b7d6, $b3ce7db3,
  $297b5229, $e33edde3, $2f715e2f, $84971384,
  $53f5a653, $d168b9d1, $00000000, $ed2cc1ed,
  $20604020, $fc1fe3fc, $b1c879b1, $5bedb65b,
  $6abed46a, $cb468dcb, $bed967be, $394b7239,
  $4ade944a, $4cd4984c, $58e8b058, $cf4a85cf,
  $d06bbbd0, $ef2ac5ef, $aae54faa, $fb16edfb,
  $43c58643, $4dd79a4d, $33556633, $85941185,
  $45cf8a45, $f910e9f9, $02060402, $7f81fe7f,
  $50f0a050, $3c44783c, $9fba259f, $a8e34ba8,
  $51f3a251, $a3fe5da3, $40c08040, $8f8a058f,
  $92ad3f92, $9dbc219d, $38487038, $f504f1f5,
  $bcdf63bc, $b6c177b6, $da75afda, $21634221,
  $10302010, $ff1ae5ff, $f30efdf3, $d26dbfd2,
  $cd4c81cd, $0c14180c, $13352613, $ec2fc3ec,
  $5fe1be5f, $97a23597, $44cc8844, $17392e17,
  $c45793c4, $a7f255a7, $7e82fc7e, $3d477a3d,
  $64acc864, $5de7ba5d, $192b3219, $7395e673,
  $60a0c060, $81981981, $4fd19e4f, $dc7fa3dc,
  $22664422, $2a7e542a, $90ab3b90, $88830b88,
  $46ca8c46, $ee29c7ee, $b8d36bb8, $143c2814,
  $de79a7de, $5ee2bc5e, $0b1d160b, $db76addb,
  $e03bdbe0, $32566432, $3a4e743a, $0a1e140a,
  $49db9249, $060a0c06, $246c4824, $5ce4b85c,
  $c25d9fc2, $d36ebdd3, $acef43ac, $62a6c462,
  $91a83991, $95a43195, $e437d3e4, $798bf279,
  $e732d5e7, $c8438bc8, $37596e37, $6db7da6d,
  $8d8c018d, $d564b1d5, $4ed29c4e, $a9e049a9,
  $6cb4d86c, $56faac56, $f407f3f4, $ea25cfea,
  $65afca65, $7a8ef47a, $aee947ae, $08181008,
  $bad56fba, $7888f078, $256f4a25, $2e725c2e,
  $1c24381c, $a6f157a6, $b4c773b4, $c65197c6,
  $e823cbe8, $dd7ca1dd, $749ce874, $1f213e1f,
  $4bdd964b, $bddc61bd, $8b860d8b, $8a850f8a,
  $7090e070, $3e427c3e, $b5c471b5, $66aacc66,
  $48d89048, $03050603, $f601f7f6, $0e121c0e,
  $61a3c261, $355f6a35, $57f9ae57, $b9d069b9,
  $86911786, $c15899c1, $1d273a1d, $9eb9279e,
  $e138d9e1, $f813ebf8, $98b32b98, $11332211,
  $69bbd269, $d970a9d9, $8e89078e, $94a73394,
  $9bb62d9b, $1e223c1e, $87921587, $e920c9e9,
  $ce4987ce, $55ffaa55, $28785028, $df7aa5df,
  $8c8f038c, $a1f859a1, $89800989, $0d171a0d,
  $bfda65bf, $e631d7e6, $42c68442, $68b8d068,
  $41c38241, $99b02999, $2d775a2d, $0f111e0f,
  $b0cb7bb0, $54fca854, $bbd66dbb, $163a2c16);

 AES_TE3:array[0..255] of LongWord = (
  $6363a5c6, $7c7c84f8, $777799ee, $7b7b8df6,
  $f2f20dff, $6b6bbdd6, $6f6fb1de, $c5c55491,
  $30305060, $01010302, $6767a9ce, $2b2b7d56,
  $fefe19e7, $d7d762b5, $ababe64d, $76769aec,
  $caca458f, $82829d1f, $c9c94089, $7d7d87fa,
  $fafa15ef, $5959ebb2, $4747c98e, $f0f00bfb,
  $adadec41, $d4d467b3, $a2a2fd5f, $afafea45,
  $9c9cbf23, $a4a4f753, $727296e4, $c0c05b9b,
  $b7b7c275, $fdfd1ce1, $9393ae3d, $26266a4c,
  $36365a6c, $3f3f417e, $f7f702f5, $cccc4f83,
  $34345c68, $a5a5f451, $e5e534d1, $f1f108f9,
  $717193e2, $d8d873ab, $31315362, $15153f2a,
  $04040c08, $c7c75295, $23236546, $c3c35e9d,
  $18182830, $9696a137, $05050f0a, $9a9ab52f,
  $0707090e, $12123624, $80809b1b, $e2e23ddf,
  $ebeb26cd, $2727694e, $b2b2cd7f, $75759fea,
  $09091b12, $83839e1d, $2c2c7458, $1a1a2e34,
  $1b1b2d36, $6e6eb2dc, $5a5aeeb4, $a0a0fb5b,
  $5252f6a4, $3b3b4d76, $d6d661b7, $b3b3ce7d,
  $29297b52, $e3e33edd, $2f2f715e, $84849713,
  $5353f5a6, $d1d168b9, $00000000, $eded2cc1,
  $20206040, $fcfc1fe3, $b1b1c879, $5b5bedb6,
  $6a6abed4, $cbcb468d, $bebed967, $39394b72,
  $4a4ade94, $4c4cd498, $5858e8b0, $cfcf4a85,
  $d0d06bbb, $efef2ac5, $aaaae54f, $fbfb16ed,
  $4343c586, $4d4dd79a, $33335566, $85859411,
  $4545cf8a, $f9f910e9, $02020604, $7f7f81fe,
  $5050f0a0, $3c3c4478, $9f9fba25, $a8a8e34b,
  $5151f3a2, $a3a3fe5d, $4040c080, $8f8f8a05,
  $9292ad3f, $9d9dbc21, $38384870, $f5f504f1,
  $bcbcdf63, $b6b6c177, $dada75af, $21216342,
  $10103020, $ffff1ae5, $f3f30efd, $d2d26dbf,
  $cdcd4c81, $0c0c1418, $13133526, $ecec2fc3,
  $5f5fe1be, $9797a235, $4444cc88, $1717392e,
  $c4c45793, $a7a7f255, $7e7e82fc, $3d3d477a,
  $6464acc8, $5d5de7ba, $19192b32, $737395e6,
  $6060a0c0, $81819819, $4f4fd19e, $dcdc7fa3,
  $22226644, $2a2a7e54, $9090ab3b, $8888830b,
  $4646ca8c, $eeee29c7, $b8b8d36b, $14143c28,
  $dede79a7, $5e5ee2bc, $0b0b1d16, $dbdb76ad,
  $e0e03bdb, $32325664, $3a3a4e74, $0a0a1e14,
  $4949db92, $06060a0c, $24246c48, $5c5ce4b8,
  $c2c25d9f, $d3d36ebd, $acacef43, $6262a6c4,
  $9191a839, $9595a431, $e4e437d3, $79798bf2,
  $e7e732d5, $c8c8438b, $3737596e, $6d6db7da,
  $8d8d8c01, $d5d564b1, $4e4ed29c, $a9a9e049,
  $6c6cb4d8, $5656faac, $f4f407f3, $eaea25cf,
  $6565afca, $7a7a8ef4, $aeaee947, $08081810,
  $babad56f, $787888f0, $25256f4a, $2e2e725c,
  $1c1c2438, $a6a6f157, $b4b4c773, $c6c65197,
  $e8e823cb, $dddd7ca1, $74749ce8, $1f1f213e,
  $4b4bdd96, $bdbddc61, $8b8b860d, $8a8a850f,
  $707090e0, $3e3e427c, $b5b5c471, $6666aacc,
  $4848d890, $03030506, $f6f601f7, $0e0e121c,
  $6161a3c2, $35355f6a, $5757f9ae, $b9b9d069,
  $86869117, $c1c15899, $1d1d273a, $9e9eb927,
  $e1e138d9, $f8f813eb, $9898b32b, $11113322,
  $6969bbd2, $d9d970a9, $8e8e8907, $9494a733,
  $9b9bb62d, $1e1e223c, $87879215, $e9e920c9,
  $cece4987, $5555ffaa, $28287850, $dfdf7aa5,
  $8c8c8f03, $a1a1f859, $89898009, $0d0d171a,
  $bfbfda65, $e6e631d7, $4242c684, $6868b8d0,
  $4141c382, $9999b029, $2d2d775a, $0f0f111e,
  $b0b0cb7b, $5454fca8, $bbbbd66d, $16163a2c);
 {$ENDIF CRYPTO_AES_PRECOMPUTED_TABLES}

 AES_TE4:array[0..255] of LongWord = (
  $63636363, $7c7c7c7c, $77777777, $7b7b7b7b,
  $f2f2f2f2, $6b6b6b6b, $6f6f6f6f, $c5c5c5c5,
  $30303030, $01010101, $67676767, $2b2b2b2b,
  $fefefefe, $d7d7d7d7, $abababab, $76767676,
  $cacacaca, $82828282, $c9c9c9c9, $7d7d7d7d,
  $fafafafa, $59595959, $47474747, $f0f0f0f0,
  $adadadad, $d4d4d4d4, $a2a2a2a2, $afafafaf,
  $9c9c9c9c, $a4a4a4a4, $72727272, $c0c0c0c0,
  $b7b7b7b7, $fdfdfdfd, $93939393, $26262626,
  $36363636, $3f3f3f3f, $f7f7f7f7, $cccccccc,
  $34343434, $a5a5a5a5, $e5e5e5e5, $f1f1f1f1,
  $71717171, $d8d8d8d8, $31313131, $15151515,
  $04040404, $c7c7c7c7, $23232323, $c3c3c3c3,
  $18181818, $96969696, $05050505, $9a9a9a9a,
  $07070707, $12121212, $80808080, $e2e2e2e2,
  $ebebebeb, $27272727, $b2b2b2b2, $75757575,
  $09090909, $83838383, $2c2c2c2c, $1a1a1a1a,
  $1b1b1b1b, $6e6e6e6e, $5a5a5a5a, $a0a0a0a0,
  $52525252, $3b3b3b3b, $d6d6d6d6, $b3b3b3b3,
  $29292929, $e3e3e3e3, $2f2f2f2f, $84848484,
  $53535353, $d1d1d1d1, $00000000, $edededed,
  $20202020, $fcfcfcfc, $b1b1b1b1, $5b5b5b5b,
  $6a6a6a6a, $cbcbcbcb, $bebebebe, $39393939,
  $4a4a4a4a, $4c4c4c4c, $58585858, $cfcfcfcf,
  $d0d0d0d0, $efefefef, $aaaaaaaa, $fbfbfbfb,
  $43434343, $4d4d4d4d, $33333333, $85858585,
  $45454545, $f9f9f9f9, $02020202, $7f7f7f7f,
  $50505050, $3c3c3c3c, $9f9f9f9f, $a8a8a8a8,
  $51515151, $a3a3a3a3, $40404040, $8f8f8f8f,
  $92929292, $9d9d9d9d, $38383838, $f5f5f5f5,
  $bcbcbcbc, $b6b6b6b6, $dadadada, $21212121,
  $10101010, $ffffffff, $f3f3f3f3, $d2d2d2d2,
  $cdcdcdcd, $0c0c0c0c, $13131313, $ecececec,
  $5f5f5f5f, $97979797, $44444444, $17171717,
  $c4c4c4c4, $a7a7a7a7, $7e7e7e7e, $3d3d3d3d,
  $64646464, $5d5d5d5d, $19191919, $73737373,
  $60606060, $81818181, $4f4f4f4f, $dcdcdcdc,
  $22222222, $2a2a2a2a, $90909090, $88888888,
  $46464646, $eeeeeeee, $b8b8b8b8, $14141414,
  $dededede, $5e5e5e5e, $0b0b0b0b, $dbdbdbdb,
  $e0e0e0e0, $32323232, $3a3a3a3a, $0a0a0a0a,
  $49494949, $06060606, $24242424, $5c5c5c5c,
  $c2c2c2c2, $d3d3d3d3, $acacacac, $62626262,
  $91919191, $95959595, $e4e4e4e4, $79797979,
  $e7e7e7e7, $c8c8c8c8, $37373737, $6d6d6d6d,
  $8d8d8d8d, $d5d5d5d5, $4e4e4e4e, $a9a9a9a9,
  $6c6c6c6c, $56565656, $f4f4f4f4, $eaeaeaea,
  $65656565, $7a7a7a7a, $aeaeaeae, $08080808,
  $babababa, $78787878, $25252525, $2e2e2e2e,
  $1c1c1c1c, $a6a6a6a6, $b4b4b4b4, $c6c6c6c6,
  $e8e8e8e8, $dddddddd, $74747474, $1f1f1f1f,
  $4b4b4b4b, $bdbdbdbd, $8b8b8b8b, $8a8a8a8a,
  $70707070, $3e3e3e3e, $b5b5b5b5, $66666666,
  $48484848, $03030303, $f6f6f6f6, $0e0e0e0e,
  $61616161, $35353535, $57575757, $b9b9b9b9,
  $86868686, $c1c1c1c1, $1d1d1d1d, $9e9e9e9e,
  $e1e1e1e1, $f8f8f8f8, $98989898, $11111111,
  $69696969, $d9d9d9d9, $8e8e8e8e, $94949494,
  $9b9b9b9b, $1e1e1e1e, $87878787, $e9e9e9e9,
  $cececece, $55555555, $28282828, $dfdfdfdf,
  $8c8c8c8c, $a1a1a1a1, $89898989, $0d0d0d0d,
  $bfbfbfbf, $e6e6e6e6, $42424242, $68686868,
  $41414141, $99999999, $2d2d2d2d, $0f0f0f0f,
  $b0b0b0b0, $54545454, $bbbbbbbb, $16161616);

 {$IFDEF CRYPTO_AES_PRECOMPUTED_TABLES}
 AES_TE4_0:array[0..255] of LongWord = (
  $00000063, $0000007c, $00000077, $0000007b, $000000f2, $0000006b, $0000006f, $000000c5,
  $00000030, $00000001, $00000067, $0000002b, $000000fe, $000000d7, $000000ab, $00000076,
  $000000ca, $00000082, $000000c9, $0000007d, $000000fa, $00000059, $00000047, $000000f0,
  $000000ad, $000000d4, $000000a2, $000000af, $0000009c, $000000a4, $00000072, $000000c0,
  $000000b7, $000000fd, $00000093, $00000026, $00000036, $0000003f, $000000f7, $000000cc,
  $00000034, $000000a5, $000000e5, $000000f1, $00000071, $000000d8, $00000031, $00000015,
  $00000004, $000000c7, $00000023, $000000c3, $00000018, $00000096, $00000005, $0000009a,
  $00000007, $00000012, $00000080, $000000e2, $000000eb, $00000027, $000000b2, $00000075,
  $00000009, $00000083, $0000002c, $0000001a, $0000001b, $0000006e, $0000005a, $000000a0,
  $00000052, $0000003b, $000000d6, $000000b3, $00000029, $000000e3, $0000002f, $00000084,
  $00000053, $000000d1, $00000000, $000000ed, $00000020, $000000fc, $000000b1, $0000005b,
  $0000006a, $000000cb, $000000be, $00000039, $0000004a, $0000004c, $00000058, $000000cf,
  $000000d0, $000000ef, $000000aa, $000000fb, $00000043, $0000004d, $00000033, $00000085,
  $00000045, $000000f9, $00000002, $0000007f, $00000050, $0000003c, $0000009f, $000000a8,
  $00000051, $000000a3, $00000040, $0000008f, $00000092, $0000009d, $00000038, $000000f5,
  $000000bc, $000000b6, $000000da, $00000021, $00000010, $000000ff, $000000f3, $000000d2,
  $000000cd, $0000000c, $00000013, $000000ec, $0000005f, $00000097, $00000044, $00000017,
  $000000c4, $000000a7, $0000007e, $0000003d, $00000064, $0000005d, $00000019, $00000073,
  $00000060, $00000081, $0000004f, $000000dc, $00000022, $0000002a, $00000090, $00000088,
  $00000046, $000000ee, $000000b8, $00000014, $000000de, $0000005e, $0000000b, $000000db,
  $000000e0, $00000032, $0000003a, $0000000a, $00000049, $00000006, $00000024, $0000005c,
  $000000c2, $000000d3, $000000ac, $00000062, $00000091, $00000095, $000000e4, $00000079,
  $000000e7, $000000c8, $00000037, $0000006d, $0000008d, $000000d5, $0000004e, $000000a9,
  $0000006c, $00000056, $000000f4, $000000ea, $00000065, $0000007a, $000000ae, $00000008,
  $000000ba, $00000078, $00000025, $0000002e, $0000001c, $000000a6, $000000b4, $000000c6,
  $000000e8, $000000dd, $00000074, $0000001f, $0000004b, $000000bd, $0000008b, $0000008a,
  $00000070, $0000003e, $000000b5, $00000066, $00000048, $00000003, $000000f6, $0000000e,
  $00000061, $00000035, $00000057, $000000b9, $00000086, $000000c1, $0000001d, $0000009e,
  $000000e1, $000000f8, $00000098, $00000011, $00000069, $000000d9, $0000008e, $00000094,
  $0000009b, $0000001e, $00000087, $000000e9, $000000ce, $00000055, $00000028, $000000df,
  $0000008c, $000000a1, $00000089, $0000000d, $000000bf, $000000e6, $00000042, $00000068,
  $00000041, $00000099, $0000002d, $0000000f, $000000b0, $00000054, $000000bb, $00000016);

 AES_TE4_1:array[0..255] of LongWord = (
  $00006300, $00007c00, $00007700, $00007b00, $0000f200, $00006b00, $00006f00, $0000c500,
  $00003000, $00000100, $00006700, $00002b00, $0000fe00, $0000d700, $0000ab00, $00007600,
  $0000ca00, $00008200, $0000c900, $00007d00, $0000fa00, $00005900, $00004700, $0000f000,
  $0000ad00, $0000d400, $0000a200, $0000af00, $00009c00, $0000a400, $00007200, $0000c000,
  $0000b700, $0000fd00, $00009300, $00002600, $00003600, $00003f00, $0000f700, $0000cc00,
  $00003400, $0000a500, $0000e500, $0000f100, $00007100, $0000d800, $00003100, $00001500,
  $00000400, $0000c700, $00002300, $0000c300, $00001800, $00009600, $00000500, $00009a00,
  $00000700, $00001200, $00008000, $0000e200, $0000eb00, $00002700, $0000b200, $00007500,
  $00000900, $00008300, $00002c00, $00001a00, $00001b00, $00006e00, $00005a00, $0000a000,
  $00005200, $00003b00, $0000d600, $0000b300, $00002900, $0000e300, $00002f00, $00008400,
  $00005300, $0000d100, $00000000, $0000ed00, $00002000, $0000fc00, $0000b100, $00005b00,
  $00006a00, $0000cb00, $0000be00, $00003900, $00004a00, $00004c00, $00005800, $0000cf00,
  $0000d000, $0000ef00, $0000aa00, $0000fb00, $00004300, $00004d00, $00003300, $00008500,
  $00004500, $0000f900, $00000200, $00007f00, $00005000, $00003c00, $00009f00, $0000a800,
  $00005100, $0000a300, $00004000, $00008f00, $00009200, $00009d00, $00003800, $0000f500,
  $0000bc00, $0000b600, $0000da00, $00002100, $00001000, $0000ff00, $0000f300, $0000d200,
  $0000cd00, $00000c00, $00001300, $0000ec00, $00005f00, $00009700, $00004400, $00001700,
  $0000c400, $0000a700, $00007e00, $00003d00, $00006400, $00005d00, $00001900, $00007300,
  $00006000, $00008100, $00004f00, $0000dc00, $00002200, $00002a00, $00009000, $00008800,
  $00004600, $0000ee00, $0000b800, $00001400, $0000de00, $00005e00, $00000b00, $0000db00,
  $0000e000, $00003200, $00003a00, $00000a00, $00004900, $00000600, $00002400, $00005c00,
  $0000c200, $0000d300, $0000ac00, $00006200, $00009100, $00009500, $0000e400, $00007900,
  $0000e700, $0000c800, $00003700, $00006d00, $00008d00, $0000d500, $00004e00, $0000a900,
  $00006c00, $00005600, $0000f400, $0000ea00, $00006500, $00007a00, $0000ae00, $00000800,
  $0000ba00, $00007800, $00002500, $00002e00, $00001c00, $0000a600, $0000b400, $0000c600,
  $0000e800, $0000dd00, $00007400, $00001f00, $00004b00, $0000bd00, $00008b00, $00008a00,
  $00007000, $00003e00, $0000b500, $00006600, $00004800, $00000300, $0000f600, $00000e00,
  $00006100, $00003500, $00005700, $0000b900, $00008600, $0000c100, $00001d00, $00009e00,
  $0000e100, $0000f800, $00009800, $00001100, $00006900, $0000d900, $00008e00, $00009400,
  $00009b00, $00001e00, $00008700, $0000e900, $0000ce00, $00005500, $00002800, $0000df00,
  $00008c00, $0000a100, $00008900, $00000d00, $0000bf00, $0000e600, $00004200, $00006800,
  $00004100, $00009900, $00002d00, $00000f00, $0000b000, $00005400, $0000bb00, $00001600);

 AES_TE4_2:array[0..255] of LongWord = (
  $00630000, $007c0000, $00770000, $007b0000, $00f20000, $006b0000, $006f0000, $00c50000,
  $00300000, $00010000, $00670000, $002b0000, $00fe0000, $00d70000, $00ab0000, $00760000,
  $00ca0000, $00820000, $00c90000, $007d0000, $00fa0000, $00590000, $00470000, $00f00000,
  $00ad0000, $00d40000, $00a20000, $00af0000, $009c0000, $00a40000, $00720000, $00c00000,
  $00b70000, $00fd0000, $00930000, $00260000, $00360000, $003f0000, $00f70000, $00cc0000,
  $00340000, $00a50000, $00e50000, $00f10000, $00710000, $00d80000, $00310000, $00150000,
  $00040000, $00c70000, $00230000, $00c30000, $00180000, $00960000, $00050000, $009a0000,
  $00070000, $00120000, $00800000, $00e20000, $00eb0000, $00270000, $00b20000, $00750000,
  $00090000, $00830000, $002c0000, $001a0000, $001b0000, $006e0000, $005a0000, $00a00000,
  $00520000, $003b0000, $00d60000, $00b30000, $00290000, $00e30000, $002f0000, $00840000,
  $00530000, $00d10000, $00000000, $00ed0000, $00200000, $00fc0000, $00b10000, $005b0000,
  $006a0000, $00cb0000, $00be0000, $00390000, $004a0000, $004c0000, $00580000, $00cf0000,
  $00d00000, $00ef0000, $00aa0000, $00fb0000, $00430000, $004d0000, $00330000, $00850000,
  $00450000, $00f90000, $00020000, $007f0000, $00500000, $003c0000, $009f0000, $00a80000,
  $00510000, $00a30000, $00400000, $008f0000, $00920000, $009d0000, $00380000, $00f50000,
  $00bc0000, $00b60000, $00da0000, $00210000, $00100000, $00ff0000, $00f30000, $00d20000,
  $00cd0000, $000c0000, $00130000, $00ec0000, $005f0000, $00970000, $00440000, $00170000,
  $00c40000, $00a70000, $007e0000, $003d0000, $00640000, $005d0000, $00190000, $00730000,
  $00600000, $00810000, $004f0000, $00dc0000, $00220000, $002a0000, $00900000, $00880000,
  $00460000, $00ee0000, $00b80000, $00140000, $00de0000, $005e0000, $000b0000, $00db0000,
  $00e00000, $00320000, $003a0000, $000a0000, $00490000, $00060000, $00240000, $005c0000,
  $00c20000, $00d30000, $00ac0000, $00620000, $00910000, $00950000, $00e40000, $00790000,
  $00e70000, $00c80000, $00370000, $006d0000, $008d0000, $00d50000, $004e0000, $00a90000,
  $006c0000, $00560000, $00f40000, $00ea0000, $00650000, $007a0000, $00ae0000, $00080000,
  $00ba0000, $00780000, $00250000, $002e0000, $001c0000, $00a60000, $00b40000, $00c60000,
  $00e80000, $00dd0000, $00740000, $001f0000, $004b0000, $00bd0000, $008b0000, $008a0000,
  $00700000, $003e0000, $00b50000, $00660000, $00480000, $00030000, $00f60000, $000e0000,
  $00610000, $00350000, $00570000, $00b90000, $00860000, $00c10000, $001d0000, $009e0000,
  $00e10000, $00f80000, $00980000, $00110000, $00690000, $00d90000, $008e0000, $00940000,
  $009b0000, $001e0000, $00870000, $00e90000, $00ce0000, $00550000, $00280000, $00df0000,
  $008c0000, $00a10000, $00890000, $000d0000, $00bf0000, $00e60000, $00420000, $00680000,
  $00410000, $00990000, $002d0000, $000f0000, $00b00000, $00540000, $00bb0000, $00160000);

 AES_TE4_3:array[0..255] of LongWord = (
  $63000000, $7c000000, $77000000, $7b000000, $f2000000, $6b000000, $6f000000, $c5000000,
  $30000000, $01000000, $67000000, $2b000000, $fe000000, $d7000000, $ab000000, $76000000,
  $ca000000, $82000000, $c9000000, $7d000000, $fa000000, $59000000, $47000000, $f0000000,
  $ad000000, $d4000000, $a2000000, $af000000, $9c000000, $a4000000, $72000000, $c0000000,
  $b7000000, $fd000000, $93000000, $26000000, $36000000, $3f000000, $f7000000, $cc000000,
  $34000000, $a5000000, $e5000000, $f1000000, $71000000, $d8000000, $31000000, $15000000,
  $04000000, $c7000000, $23000000, $c3000000, $18000000, $96000000, $05000000, $9a000000,
  $07000000, $12000000, $80000000, $e2000000, $eb000000, $27000000, $b2000000, $75000000,
  $09000000, $83000000, $2c000000, $1a000000, $1b000000, $6e000000, $5a000000, $a0000000,
  $52000000, $3b000000, $d6000000, $b3000000, $29000000, $e3000000, $2f000000, $84000000,
  $53000000, $d1000000, $00000000, $ed000000, $20000000, $fc000000, $b1000000, $5b000000,
  $6a000000, $cb000000, $be000000, $39000000, $4a000000, $4c000000, $58000000, $cf000000,
  $d0000000, $ef000000, $aa000000, $fb000000, $43000000, $4d000000, $33000000, $85000000,
  $45000000, $f9000000, $02000000, $7f000000, $50000000, $3c000000, $9f000000, $a8000000,
  $51000000, $a3000000, $40000000, $8f000000, $92000000, $9d000000, $38000000, $f5000000,
  $bc000000, $b6000000, $da000000, $21000000, $10000000, $ff000000, $f3000000, $d2000000,
  $cd000000, $0c000000, $13000000, $ec000000, $5f000000, $97000000, $44000000, $17000000,
  $c4000000, $a7000000, $7e000000, $3d000000, $64000000, $5d000000, $19000000, $73000000,
  $60000000, $81000000, $4f000000, $dc000000, $22000000, $2a000000, $90000000, $88000000,
  $46000000, $ee000000, $b8000000, $14000000, $de000000, $5e000000, $0b000000, $db000000,
  $e0000000, $32000000, $3a000000, $0a000000, $49000000, $06000000, $24000000, $5c000000,
  $c2000000, $d3000000, $ac000000, $62000000, $91000000, $95000000, $e4000000, $79000000,
  $e7000000, $c8000000, $37000000, $6d000000, $8d000000, $d5000000, $4e000000, $a9000000,
  $6c000000, $56000000, $f4000000, $ea000000, $65000000, $7a000000, $ae000000, $08000000,
  $ba000000, $78000000, $25000000, $2e000000, $1c000000, $a6000000, $b4000000, $c6000000,
  $e8000000, $dd000000, $74000000, $1f000000, $4b000000, $bd000000, $8b000000, $8a000000,
  $70000000, $3e000000, $b5000000, $66000000, $48000000, $03000000, $f6000000, $0e000000,
  $61000000, $35000000, $57000000, $b9000000, $86000000, $c1000000, $1d000000, $9e000000,
  $e1000000, $f8000000, $98000000, $11000000, $69000000, $d9000000, $8e000000, $94000000,
  $9b000000, $1e000000, $87000000, $e9000000, $ce000000, $55000000, $28000000, $df000000,
  $8c000000, $a1000000, $89000000, $0d000000, $bf000000, $e6000000, $42000000, $68000000,
  $41000000, $99000000, $2d000000, $0f000000, $b0000000, $54000000, $bb000000, $16000000);
 {$ENDIF CRYPTO_AES_PRECOMPUTED_TABLES}

 AES_TD0:array[0..255] of LongWord = (
  $51f4a750, $7e416553, $1a17a4c3, $3a275e96,
  $3bab6bcb, $1f9d45f1, $acfa58ab, $4be30393,
  $2030fa55, $ad766df6, $88cc7691, $f5024c25,
  $4fe5d7fc, $c52acbd7, $26354480, $b562a38f,
  $deb15a49, $25ba1b67, $45ea0e98, $5dfec0e1,
  $c32f7502, $814cf012, $8d4697a3, $6bd3f9c6,
  $038f5fe7, $15929c95, $bf6d7aeb, $955259da,
  $d4be832d, $587421d3, $49e06929, $8ec9c844,
  $75c2896a, $f48e7978, $99583e6b, $27b971dd,
  $bee14fb6, $f088ad17, $c920ac66, $7dce3ab4,
  $63df4a18, $e51a3182, $97513360, $62537f45,
  $b16477e0, $bb6bae84, $fe81a01c, $f9082b94,
  $70486858, $8f45fd19, $94de6c87, $527bf8b7,
  $ab73d323, $724b02e2, $e31f8f57, $6655ab2a,
  $b2eb2807, $2fb5c203, $86c57b9a, $d33708a5,
  $302887f2, $23bfa5b2, $02036aba, $ed16825c,
  $8acf1c2b, $a779b492, $f307f2f0, $4e69e2a1,
  $65daf4cd, $0605bed5, $d134621f, $c4a6fe8a,
  $342e539d, $a2f355a0, $058ae132, $a4f6eb75,
  $0b83ec39, $4060efaa, $5e719f06, $bd6e1051,
  $3e218af9, $96dd063d, $dd3e05ae, $4de6bd46,
  $91548db5, $71c45d05, $0406d46f, $605015ff,
  $1998fb24, $d6bde997, $894043cc, $67d99e77,
  $b0e842bd, $07898b88, $e7195b38, $79c8eedb,
  $a17c0a47, $7c420fe9, $f8841ec9, $00000000,
  $09808683, $322bed48, $1e1170ac, $6c5a724e,
  $fd0efffb, $0f853856, $3daed51e, $362d3927,
  $0a0fd964, $685ca621, $9b5b54d1, $24362e3a,
  $0c0a67b1, $9357e70f, $b4ee96d2, $1b9b919e,
  $80c0c54f, $61dc20a2, $5a774b69, $1c121a16,
  $e293ba0a, $c0a02ae5, $3c22e043, $121b171d,
  $0e090d0b, $f28bc7ad, $2db6a8b9, $141ea9c8,
  $57f11985, $af75074c, $ee99ddbb, $a37f60fd,
  $f701269f, $5c72f5bc, $44663bc5, $5bfb7e34,
  $8b432976, $cb23c6dc, $b6edfc68, $b8e4f163,
  $d731dcca, $42638510, $13972240, $84c61120,
  $854a247d, $d2bb3df8, $aef93211, $c729a16d,
  $1d9e2f4b, $dcb230f3, $0d8652ec, $77c1e3d0,
  $2bb3166c, $a970b999, $119448fa, $47e96422,
  $a8fc8cc4, $a0f03f1a, $567d2cd8, $223390ef,
  $87494ec7, $d938d1c1, $8ccaa2fe, $98d40b36,
  $a6f581cf, $a57ade28, $dab78e26, $3fadbfa4,
  $2c3a9de4, $5078920d, $6a5fcc9b, $547e4662,
  $f68d13c2, $90d8b8e8, $2e39f75e, $82c3aff5,
  $9f5d80be, $69d0937c, $6fd52da9, $cf2512b3,
  $c8ac993b, $10187da7, $e89c636e, $db3bbb7b,
  $cd267809, $6e5918f4, $ec9ab701, $834f9aa8,
  $e6956e65, $aaffe67e, $21bccf08, $ef15e8e6,
  $bae79bd9, $4a6f36ce, $ea9f09d4, $29b07cd6,
  $31a4b2af, $2a3f2331, $c6a59430, $35a266c0,
  $744ebc37, $fc82caa6, $e090d0b0, $33a7d815,
  $f104984a, $41ecdaf7, $7fcd500e, $1791f62f,
  $764dd68d, $43efb04d, $ccaa4d54, $e49604df,
  $9ed1b5e3, $4c6a881b, $c12c1fb8, $4665517f,
  $9d5eea04, $018c355d, $fa877473, $fb0b412e,
  $b3671d5a, $92dbd252, $e9105633, $6dd64713,
  $9ad7618c, $37a10c7a, $59f8148e, $eb133c89,
  $cea927ee, $b761c935, $e11ce5ed, $7a47b13c,
  $9cd2df59, $55f2733f, $1814ce79, $73c737bf,
  $53f7cdea, $5ffdaa5b, $df3d6f14, $7844db86,
  $caaff381, $b968c43e, $3824342c, $c2a3405f,
  $161dc372, $bce2250c, $283c498b, $ff0d9541,
  $39a80171, $080cb3de, $d8b4e49c, $6456c190,
  $7bcb8461, $d532b670, $486c5c74, $d0b85742);

 {$IFDEF CRYPTO_AES_PRECOMPUTED_TABLES}
 AES_TD1:array[0..255] of LongWord = (
  $5051f4a7, $537e4165, $c31a17a4, $963a275e,
  $cb3bab6b, $f11f9d45, $abacfa58, $934be303,
  $552030fa, $f6ad766d, $9188cc76, $25f5024c,
  $fc4fe5d7, $d7c52acb, $80263544, $8fb562a3,
  $49deb15a, $6725ba1b, $9845ea0e, $e15dfec0,
  $02c32f75, $12814cf0, $a38d4697, $c66bd3f9,
  $e7038f5f, $9515929c, $ebbf6d7a, $da955259,
  $2dd4be83, $d3587421, $2949e069, $448ec9c8,
  $6a75c289, $78f48e79, $6b99583e, $dd27b971,
  $b6bee14f, $17f088ad, $66c920ac, $b47dce3a,
  $1863df4a, $82e51a31, $60975133, $4562537f,
  $e0b16477, $84bb6bae, $1cfe81a0, $94f9082b,
  $58704868, $198f45fd, $8794de6c, $b7527bf8,
  $23ab73d3, $e2724b02, $57e31f8f, $2a6655ab,
  $07b2eb28, $032fb5c2, $9a86c57b, $a5d33708,
  $f2302887, $b223bfa5, $ba02036a, $5ced1682,
  $2b8acf1c, $92a779b4, $f0f307f2, $a14e69e2,
  $cd65daf4, $d50605be, $1fd13462, $8ac4a6fe,
  $9d342e53, $a0a2f355, $32058ae1, $75a4f6eb,
  $390b83ec, $aa4060ef, $065e719f, $51bd6e10,
  $f93e218a, $3d96dd06, $aedd3e05, $464de6bd,
  $b591548d, $0571c45d, $6f0406d4, $ff605015,
  $241998fb, $97d6bde9, $cc894043, $7767d99e,
  $bdb0e842, $8807898b, $38e7195b, $db79c8ee,
  $47a17c0a, $e97c420f, $c9f8841e, $00000000,
  $83098086, $48322bed, $ac1e1170, $4e6c5a72,
  $fbfd0eff, $560f8538, $1e3daed5, $27362d39,
  $640a0fd9, $21685ca6, $d19b5b54, $3a24362e,
  $b10c0a67, $0f9357e7, $d2b4ee96, $9e1b9b91,
  $4f80c0c5, $a261dc20, $695a774b, $161c121a,
  $0ae293ba, $e5c0a02a, $433c22e0, $1d121b17,
  $0b0e090d, $adf28bc7, $b92db6a8, $c8141ea9,
  $8557f119, $4caf7507, $bbee99dd, $fda37f60,
  $9ff70126, $bc5c72f5, $c544663b, $345bfb7e,
  $768b4329, $dccb23c6, $68b6edfc, $63b8e4f1,
  $cad731dc, $10426385, $40139722, $2084c611,
  $7d854a24, $f8d2bb3d, $11aef932, $6dc729a1,
  $4b1d9e2f, $f3dcb230, $ec0d8652, $d077c1e3,
  $6c2bb316, $99a970b9, $fa119448, $2247e964,
  $c4a8fc8c, $1aa0f03f, $d8567d2c, $ef223390,
  $c787494e, $c1d938d1, $fe8ccaa2, $3698d40b,
  $cfa6f581, $28a57ade, $26dab78e, $a43fadbf,
  $e42c3a9d, $0d507892, $9b6a5fcc, $62547e46,
  $c2f68d13, $e890d8b8, $5e2e39f7, $f582c3af,
  $be9f5d80, $7c69d093, $a96fd52d, $b3cf2512,
  $3bc8ac99, $a710187d, $6ee89c63, $7bdb3bbb,
  $09cd2678, $f46e5918, $01ec9ab7, $a8834f9a,
  $65e6956e, $7eaaffe6, $0821bccf, $e6ef15e8,
  $d9bae79b, $ce4a6f36, $d4ea9f09, $d629b07c,
  $af31a4b2, $312a3f23, $30c6a594, $c035a266,
  $37744ebc, $a6fc82ca, $b0e090d0, $1533a7d8,
  $4af10498, $f741ecda, $0e7fcd50, $2f1791f6,
  $8d764dd6, $4d43efb0, $54ccaa4d, $dfe49604,
  $e39ed1b5, $1b4c6a88, $b8c12c1f, $7f466551,
  $049d5eea, $5d018c35, $73fa8774, $2efb0b41,
  $5ab3671d, $5292dbd2, $33e91056, $136dd647,
  $8c9ad761, $7a37a10c, $8e59f814, $89eb133c,
  $eecea927, $35b761c9, $ede11ce5, $3c7a47b1,
  $599cd2df, $3f55f273, $791814ce, $bf73c737,
  $ea53f7cd, $5b5ffdaa, $14df3d6f, $867844db,
  $81caaff3, $3eb968c4, $2c382434, $5fc2a340,
  $72161dc3, $0cbce225, $8b283c49, $41ff0d95,
  $7139a801, $de080cb3, $9cd8b4e4, $906456c1,
  $617bcb84, $70d532b6, $74486c5c, $42d0b857);

 AES_TD2:array[0..255] of LongWord = (
  $a75051f4, $65537e41, $a4c31a17, $5e963a27,
  $6bcb3bab, $45f11f9d, $58abacfa, $03934be3,
  $fa552030, $6df6ad76, $769188cc, $4c25f502,
  $d7fc4fe5, $cbd7c52a, $44802635, $a38fb562,
  $5a49deb1, $1b6725ba, $0e9845ea, $c0e15dfe,
  $7502c32f, $f012814c, $97a38d46, $f9c66bd3,
  $5fe7038f, $9c951592, $7aebbf6d, $59da9552,
  $832dd4be, $21d35874, $692949e0, $c8448ec9,
  $896a75c2, $7978f48e, $3e6b9958, $71dd27b9,
  $4fb6bee1, $ad17f088, $ac66c920, $3ab47dce,
  $4a1863df, $3182e51a, $33609751, $7f456253,
  $77e0b164, $ae84bb6b, $a01cfe81, $2b94f908,
  $68587048, $fd198f45, $6c8794de, $f8b7527b,
  $d323ab73, $02e2724b, $8f57e31f, $ab2a6655,
  $2807b2eb, $c2032fb5, $7b9a86c5, $08a5d337,
  $87f23028, $a5b223bf, $6aba0203, $825ced16,
  $1c2b8acf, $b492a779, $f2f0f307, $e2a14e69,
  $f4cd65da, $bed50605, $621fd134, $fe8ac4a6,
  $539d342e, $55a0a2f3, $e132058a, $eb75a4f6,
  $ec390b83, $efaa4060, $9f065e71, $1051bd6e,
  $8af93e21, $063d96dd, $05aedd3e, $bd464de6,
  $8db59154, $5d0571c4, $d46f0406, $15ff6050,
  $fb241998, $e997d6bd, $43cc8940, $9e7767d9,
  $42bdb0e8, $8b880789, $5b38e719, $eedb79c8,
  $0a47a17c, $0fe97c42, $1ec9f884, $00000000,
  $86830980, $ed48322b, $70ac1e11, $724e6c5a,
  $fffbfd0e, $38560f85, $d51e3dae, $3927362d,
  $d9640a0f, $a621685c, $54d19b5b, $2e3a2436,
  $67b10c0a, $e70f9357, $96d2b4ee, $919e1b9b,
  $c54f80c0, $20a261dc, $4b695a77, $1a161c12,
  $ba0ae293, $2ae5c0a0, $e0433c22, $171d121b,
  $0d0b0e09, $c7adf28b, $a8b92db6, $a9c8141e,
  $198557f1, $074caf75, $ddbbee99, $60fda37f,
  $269ff701, $f5bc5c72, $3bc54466, $7e345bfb,
  $29768b43, $c6dccb23, $fc68b6ed, $f163b8e4,
  $dccad731, $85104263, $22401397, $112084c6,
  $247d854a, $3df8d2bb, $3211aef9, $a16dc729,
  $2f4b1d9e, $30f3dcb2, $52ec0d86, $e3d077c1,
  $166c2bb3, $b999a970, $48fa1194, $642247e9,
  $8cc4a8fc, $3f1aa0f0, $2cd8567d, $90ef2233,
  $4ec78749, $d1c1d938, $a2fe8cca, $0b3698d4,
  $81cfa6f5, $de28a57a, $8e26dab7, $bfa43fad,
  $9de42c3a, $920d5078, $cc9b6a5f, $4662547e,
  $13c2f68d, $b8e890d8, $f75e2e39, $aff582c3,
  $80be9f5d, $937c69d0, $2da96fd5, $12b3cf25,
  $993bc8ac, $7da71018, $636ee89c, $bb7bdb3b,
  $7809cd26, $18f46e59, $b701ec9a, $9aa8834f,
  $6e65e695, $e67eaaff, $cf0821bc, $e8e6ef15,
  $9bd9bae7, $36ce4a6f, $09d4ea9f, $7cd629b0,
  $b2af31a4, $23312a3f, $9430c6a5, $66c035a2,
  $bc37744e, $caa6fc82, $d0b0e090, $d81533a7,
  $984af104, $daf741ec, $500e7fcd, $f62f1791,
  $d68d764d, $b04d43ef, $4d54ccaa, $04dfe496,
  $b5e39ed1, $881b4c6a, $1fb8c12c, $517f4665,
  $ea049d5e, $355d018c, $7473fa87, $412efb0b,
  $1d5ab367, $d25292db, $5633e910, $47136dd6,
  $618c9ad7, $0c7a37a1, $148e59f8, $3c89eb13,
  $27eecea9, $c935b761, $e5ede11c, $b13c7a47,
  $df599cd2, $733f55f2, $ce791814, $37bf73c7,
  $cdea53f7, $aa5b5ffd, $6f14df3d, $db867844,
  $f381caaf, $c43eb968, $342c3824, $405fc2a3,
  $c372161d, $250cbce2, $498b283c, $9541ff0d,
  $017139a8, $b3de080c, $e49cd8b4, $c1906456,
  $84617bcb, $b670d532, $5c74486c, $5742d0b8);

 AES_TD3:array[0..255] of LongWord = (
  $f4a75051, $4165537e, $17a4c31a, $275e963a,
  $ab6bcb3b, $9d45f11f, $fa58abac, $e303934b,
  $30fa5520, $766df6ad, $cc769188, $024c25f5,
  $e5d7fc4f, $2acbd7c5, $35448026, $62a38fb5,
  $b15a49de, $ba1b6725, $ea0e9845, $fec0e15d,
  $2f7502c3, $4cf01281, $4697a38d, $d3f9c66b,
  $8f5fe703, $929c9515, $6d7aebbf, $5259da95,
  $be832dd4, $7421d358, $e0692949, $c9c8448e,
  $c2896a75, $8e7978f4, $583e6b99, $b971dd27,
  $e14fb6be, $88ad17f0, $20ac66c9, $ce3ab47d,
  $df4a1863, $1a3182e5, $51336097, $537f4562,
  $6477e0b1, $6bae84bb, $81a01cfe, $082b94f9,
  $48685870, $45fd198f, $de6c8794, $7bf8b752,
  $73d323ab, $4b02e272, $1f8f57e3, $55ab2a66,
  $eb2807b2, $b5c2032f, $c57b9a86, $3708a5d3,
  $2887f230, $bfa5b223, $036aba02, $16825ced,
  $cf1c2b8a, $79b492a7, $07f2f0f3, $69e2a14e,
  $daf4cd65, $05bed506, $34621fd1, $a6fe8ac4,
  $2e539d34, $f355a0a2, $8ae13205, $f6eb75a4,
  $83ec390b, $60efaa40, $719f065e, $6e1051bd,
  $218af93e, $dd063d96, $3e05aedd, $e6bd464d,
  $548db591, $c45d0571, $06d46f04, $5015ff60,
  $98fb2419, $bde997d6, $4043cc89, $d99e7767,
  $e842bdb0, $898b8807, $195b38e7, $c8eedb79,
  $7c0a47a1, $420fe97c, $841ec9f8, $00000000,
  $80868309, $2bed4832, $1170ac1e, $5a724e6c,
  $0efffbfd, $8538560f, $aed51e3d, $2d392736,
  $0fd9640a, $5ca62168, $5b54d19b, $362e3a24,
  $0a67b10c, $57e70f93, $ee96d2b4, $9b919e1b,
  $c0c54f80, $dc20a261, $774b695a, $121a161c,
  $93ba0ae2, $a02ae5c0, $22e0433c, $1b171d12,
  $090d0b0e, $8bc7adf2, $b6a8b92d, $1ea9c814,
  $f1198557, $75074caf, $99ddbbee, $7f60fda3,
  $01269ff7, $72f5bc5c, $663bc544, $fb7e345b,
  $4329768b, $23c6dccb, $edfc68b6, $e4f163b8,
  $31dccad7, $63851042, $97224013, $c6112084,
  $4a247d85, $bb3df8d2, $f93211ae, $29a16dc7,
  $9e2f4b1d, $b230f3dc, $8652ec0d, $c1e3d077,
  $b3166c2b, $70b999a9, $9448fa11, $e9642247,
  $fc8cc4a8, $f03f1aa0, $7d2cd856, $3390ef22,
  $494ec787, $38d1c1d9, $caa2fe8c, $d40b3698,
  $f581cfa6, $7ade28a5, $b78e26da, $adbfa43f,
  $3a9de42c, $78920d50, $5fcc9b6a, $7e466254,
  $8d13c2f6, $d8b8e890, $39f75e2e, $c3aff582,
  $5d80be9f, $d0937c69, $d52da96f, $2512b3cf,
  $ac993bc8, $187da710, $9c636ee8, $3bbb7bdb,
  $267809cd, $5918f46e, $9ab701ec, $4f9aa883,
  $956e65e6, $ffe67eaa, $bccf0821, $15e8e6ef,
  $e79bd9ba, $6f36ce4a, $9f09d4ea, $b07cd629,
  $a4b2af31, $3f23312a, $a59430c6, $a266c035,
  $4ebc3774, $82caa6fc, $90d0b0e0, $a7d81533,
  $04984af1, $ecdaf741, $cd500e7f, $91f62f17,
  $4dd68d76, $efb04d43, $aa4d54cc, $9604dfe4,
  $d1b5e39e, $6a881b4c, $2c1fb8c1, $65517f46,
  $5eea049d, $8c355d01, $877473fa, $0b412efb,
  $671d5ab3, $dbd25292, $105633e9, $d647136d,
  $d7618c9a, $a10c7a37, $f8148e59, $133c89eb,
  $a927eece, $61c935b7, $1ce5ede1, $47b13c7a,
  $d2df599c, $f2733f55, $14ce7918, $c737bf73,
  $f7cdea53, $fdaa5b5f, $3d6f14df, $44db8678,
  $aff381ca, $68c43eb9, $24342c38, $a3405fc2,
  $1dc37216, $e2250cbc, $3c498b28, $0d9541ff,
  $a8017139, $0cb3de08, $b4e49cd8, $56c19064,
  $cb84617b, $32b670d5, $6c5c7448, $b85742d0);
 {$ENDIF CRYPTO_AES_PRECOMPUTED_TABLES}

 AES_TD4:array[0..255] of LongWord = (
  $52525252, $09090909, $6a6a6a6a, $d5d5d5d5,
  $30303030, $36363636, $a5a5a5a5, $38383838,
  $bfbfbfbf, $40404040, $a3a3a3a3, $9e9e9e9e,
  $81818181, $f3f3f3f3, $d7d7d7d7, $fbfbfbfb,
  $7c7c7c7c, $e3e3e3e3, $39393939, $82828282,
  $9b9b9b9b, $2f2f2f2f, $ffffffff, $87878787,
  $34343434, $8e8e8e8e, $43434343, $44444444,
  $c4c4c4c4, $dededede, $e9e9e9e9, $cbcbcbcb,
  $54545454, $7b7b7b7b, $94949494, $32323232,
  $a6a6a6a6, $c2c2c2c2, $23232323, $3d3d3d3d,
  $eeeeeeee, $4c4c4c4c, $95959595, $0b0b0b0b,
  $42424242, $fafafafa, $c3c3c3c3, $4e4e4e4e,
  $08080808, $2e2e2e2e, $a1a1a1a1, $66666666,
  $28282828, $d9d9d9d9, $24242424, $b2b2b2b2,
  $76767676, $5b5b5b5b, $a2a2a2a2, $49494949,
  $6d6d6d6d, $8b8b8b8b, $d1d1d1d1, $25252525,
  $72727272, $f8f8f8f8, $f6f6f6f6, $64646464,
  $86868686, $68686868, $98989898, $16161616,
  $d4d4d4d4, $a4a4a4a4, $5c5c5c5c, $cccccccc,
  $5d5d5d5d, $65656565, $b6b6b6b6, $92929292,
  $6c6c6c6c, $70707070, $48484848, $50505050,
  $fdfdfdfd, $edededed, $b9b9b9b9, $dadadada,
  $5e5e5e5e, $15151515, $46464646, $57575757,
  $a7a7a7a7, $8d8d8d8d, $9d9d9d9d, $84848484,
  $90909090, $d8d8d8d8, $abababab, $00000000,
  $8c8c8c8c, $bcbcbcbc, $d3d3d3d3, $0a0a0a0a,
  $f7f7f7f7, $e4e4e4e4, $58585858, $05050505,
  $b8b8b8b8, $b3b3b3b3, $45454545, $06060606,
  $d0d0d0d0, $2c2c2c2c, $1e1e1e1e, $8f8f8f8f,
  $cacacaca, $3f3f3f3f, $0f0f0f0f, $02020202,
  $c1c1c1c1, $afafafaf, $bdbdbdbd, $03030303,
  $01010101, $13131313, $8a8a8a8a, $6b6b6b6b,
  $3a3a3a3a, $91919191, $11111111, $41414141,
  $4f4f4f4f, $67676767, $dcdcdcdc, $eaeaeaea,
  $97979797, $f2f2f2f2, $cfcfcfcf, $cececece,
  $f0f0f0f0, $b4b4b4b4, $e6e6e6e6, $73737373,
  $96969696, $acacacac, $74747474, $22222222,
  $e7e7e7e7, $adadadad, $35353535, $85858585,
  $e2e2e2e2, $f9f9f9f9, $37373737, $e8e8e8e8,
  $1c1c1c1c, $75757575, $dfdfdfdf, $6e6e6e6e,
  $47474747, $f1f1f1f1, $1a1a1a1a, $71717171,
  $1d1d1d1d, $29292929, $c5c5c5c5, $89898989,
  $6f6f6f6f, $b7b7b7b7, $62626262, $0e0e0e0e,
  $aaaaaaaa, $18181818, $bebebebe, $1b1b1b1b,
  $fcfcfcfc, $56565656, $3e3e3e3e, $4b4b4b4b,
  $c6c6c6c6, $d2d2d2d2, $79797979, $20202020,
  $9a9a9a9a, $dbdbdbdb, $c0c0c0c0, $fefefefe,
  $78787878, $cdcdcdcd, $5a5a5a5a, $f4f4f4f4,
  $1f1f1f1f, $dddddddd, $a8a8a8a8, $33333333,
  $88888888, $07070707, $c7c7c7c7, $31313131,
  $b1b1b1b1, $12121212, $10101010, $59595959,
  $27272727, $80808080, $ecececec, $5f5f5f5f,
  $60606060, $51515151, $7f7f7f7f, $a9a9a9a9,
  $19191919, $b5b5b5b5, $4a4a4a4a, $0d0d0d0d,
  $2d2d2d2d, $e5e5e5e5, $7a7a7a7a, $9f9f9f9f,
  $93939393, $c9c9c9c9, $9c9c9c9c, $efefefef,
  $a0a0a0a0, $e0e0e0e0, $3b3b3b3b, $4d4d4d4d,
  $aeaeaeae, $2a2a2a2a, $f5f5f5f5, $b0b0b0b0,
  $c8c8c8c8, $ebebebeb, $bbbbbbbb, $3c3c3c3c,
  $83838383, $53535353, $99999999, $61616161,
  $17171717, $2b2b2b2b, $04040404, $7e7e7e7e,
  $babababa, $77777777, $d6d6d6d6, $26262626,
  $e1e1e1e1, $69696969, $14141414, $63636363,
  $55555555, $21212121, $0c0c0c0c, $7d7d7d7d);

 AES_RCON:array[0..9] of LongWord = ( {for 128-bit blocks, Rijndael never uses more than 10 rcon values}
  $01000000, $02000000, $04000000, $08000000,
  $10000000, $20000000, $40000000, $80000000,
  $1B000000, $36000000);

 {$IFDEF CRYPTO_AES_PRECOMPUTED_TABLES}
 AES_TKS0:array[0..255] of LongWord = (
  $00000000, $0e090d0b, $1c121a16, $121b171d, $3824342c, $362d3927, $24362e3a, $2a3f2331,
  $70486858, $7e416553, $6c5a724e, $62537f45, $486c5c74, $4665517f, $547e4662, $5a774b69,
  $e090d0b0, $ee99ddbb, $fc82caa6, $f28bc7ad, $d8b4e49c, $d6bde997, $c4a6fe8a, $caaff381,
  $90d8b8e8, $9ed1b5e3, $8ccaa2fe, $82c3aff5, $a8fc8cc4, $a6f581cf, $b4ee96d2, $bae79bd9,
  $db3bbb7b, $d532b670, $c729a16d, $c920ac66, $e31f8f57, $ed16825c, $ff0d9541, $f104984a,
  $ab73d323, $a57ade28, $b761c935, $b968c43e, $9357e70f, $9d5eea04, $8f45fd19, $814cf012,
  $3bab6bcb, $35a266c0, $27b971dd, $29b07cd6, $038f5fe7, $0d8652ec, $1f9d45f1, $119448fa,
  $4be30393, $45ea0e98, $57f11985, $59f8148e, $73c737bf, $7dce3ab4, $6fd52da9, $61dc20a2,
  $ad766df6, $a37f60fd, $b16477e0, $bf6d7aeb, $955259da, $9b5b54d1, $894043cc, $87494ec7,
  $dd3e05ae, $d33708a5, $c12c1fb8, $cf2512b3, $e51a3182, $eb133c89, $f9082b94, $f701269f,
  $4de6bd46, $43efb04d, $51f4a750, $5ffdaa5b, $75c2896a, $7bcb8461, $69d0937c, $67d99e77,
  $3daed51e, $33a7d815, $21bccf08, $2fb5c203, $058ae132, $0b83ec39, $1998fb24, $1791f62f,
  $764dd68d, $7844db86, $6a5fcc9b, $6456c190, $4e69e2a1, $4060efaa, $527bf8b7, $5c72f5bc,
  $0605bed5, $080cb3de, $1a17a4c3, $141ea9c8, $3e218af9, $302887f2, $223390ef, $2c3a9de4,
  $96dd063d, $98d40b36, $8acf1c2b, $84c61120, $aef93211, $a0f03f1a, $b2eb2807, $bce2250c,
  $e6956e65, $e89c636e, $fa877473, $f48e7978, $deb15a49, $d0b85742, $c2a3405f, $ccaa4d54,
  $41ecdaf7, $4fe5d7fc, $5dfec0e1, $53f7cdea, $79c8eedb, $77c1e3d0, $65daf4cd, $6bd3f9c6,
  $31a4b2af, $3fadbfa4, $2db6a8b9, $23bfa5b2, $09808683, $07898b88, $15929c95, $1b9b919e,
  $a17c0a47, $af75074c, $bd6e1051, $b3671d5a, $99583e6b, $97513360, $854a247d, $8b432976,
  $d134621f, $df3d6f14, $cd267809, $c32f7502, $e9105633, $e7195b38, $f5024c25, $fb0b412e,
  $9ad7618c, $94de6c87, $86c57b9a, $88cc7691, $a2f355a0, $acfa58ab, $bee14fb6, $b0e842bd,
  $ea9f09d4, $e49604df, $f68d13c2, $f8841ec9, $d2bb3df8, $dcb230f3, $cea927ee, $c0a02ae5,
  $7a47b13c, $744ebc37, $6655ab2a, $685ca621, $42638510, $4c6a881b, $5e719f06, $5078920d,
  $0a0fd964, $0406d46f, $161dc372, $1814ce79, $322bed48, $3c22e043, $2e39f75e, $2030fa55,
  $ec9ab701, $e293ba0a, $f088ad17, $fe81a01c, $d4be832d, $dab78e26, $c8ac993b, $c6a59430,
  $9cd2df59, $92dbd252, $80c0c54f, $8ec9c844, $a4f6eb75, $aaffe67e, $b8e4f163, $b6edfc68,
  $0c0a67b1, $02036aba, $10187da7, $1e1170ac, $342e539d, $3a275e96, $283c498b, $26354480,
  $7c420fe9, $724b02e2, $605015ff, $6e5918f4, $44663bc5, $4a6f36ce, $587421d3, $567d2cd8,
  $37a10c7a, $39a80171, $2bb3166c, $25ba1b67, $0f853856, $018c355d, $13972240, $1d9e2f4b,
  $47e96422, $49e06929, $5bfb7e34, $55f2733f, $7fcd500e, $71c45d05, $63df4a18, $6dd64713,
  $d731dcca, $d938d1c1, $cb23c6dc, $c52acbd7, $ef15e8e6, $e11ce5ed, $f307f2f0, $fd0efffb,
  $a779b492, $a970b999, $bb6bae84, $b562a38f, $9f5d80be, $91548db5, $834f9aa8, $8d4697a3);

 AES_TKS1:array[0..255] of LongWord = (
  $00000000, $0b0e090d, $161c121a, $1d121b17, $2c382434, $27362d39, $3a24362e, $312a3f23,
  $58704868, $537e4165, $4e6c5a72, $4562537f, $74486c5c, $7f466551, $62547e46, $695a774b,
  $b0e090d0, $bbee99dd, $a6fc82ca, $adf28bc7, $9cd8b4e4, $97d6bde9, $8ac4a6fe, $81caaff3,
  $e890d8b8, $e39ed1b5, $fe8ccaa2, $f582c3af, $c4a8fc8c, $cfa6f581, $d2b4ee96, $d9bae79b,
  $7bdb3bbb, $70d532b6, $6dc729a1, $66c920ac, $57e31f8f, $5ced1682, $41ff0d95, $4af10498,
  $23ab73d3, $28a57ade, $35b761c9, $3eb968c4, $0f9357e7, $049d5eea, $198f45fd, $12814cf0,
  $cb3bab6b, $c035a266, $dd27b971, $d629b07c, $e7038f5f, $ec0d8652, $f11f9d45, $fa119448,
  $934be303, $9845ea0e, $8557f119, $8e59f814, $bf73c737, $b47dce3a, $a96fd52d, $a261dc20,
  $f6ad766d, $fda37f60, $e0b16477, $ebbf6d7a, $da955259, $d19b5b54, $cc894043, $c787494e,
  $aedd3e05, $a5d33708, $b8c12c1f, $b3cf2512, $82e51a31, $89eb133c, $94f9082b, $9ff70126,
  $464de6bd, $4d43efb0, $5051f4a7, $5b5ffdaa, $6a75c289, $617bcb84, $7c69d093, $7767d99e,
  $1e3daed5, $1533a7d8, $0821bccf, $032fb5c2, $32058ae1, $390b83ec, $241998fb, $2f1791f6,
  $8d764dd6, $867844db, $9b6a5fcc, $906456c1, $a14e69e2, $aa4060ef, $b7527bf8, $bc5c72f5,
  $d50605be, $de080cb3, $c31a17a4, $c8141ea9, $f93e218a, $f2302887, $ef223390, $e42c3a9d,
  $3d96dd06, $3698d40b, $2b8acf1c, $2084c611, $11aef932, $1aa0f03f, $07b2eb28, $0cbce225,
  $65e6956e, $6ee89c63, $73fa8774, $78f48e79, $49deb15a, $42d0b857, $5fc2a340, $54ccaa4d,
  $f741ecda, $fc4fe5d7, $e15dfec0, $ea53f7cd, $db79c8ee, $d077c1e3, $cd65daf4, $c66bd3f9,
  $af31a4b2, $a43fadbf, $b92db6a8, $b223bfa5, $83098086, $8807898b, $9515929c, $9e1b9b91,
  $47a17c0a, $4caf7507, $51bd6e10, $5ab3671d, $6b99583e, $60975133, $7d854a24, $768b4329,
  $1fd13462, $14df3d6f, $09cd2678, $02c32f75, $33e91056, $38e7195b, $25f5024c, $2efb0b41,
  $8c9ad761, $8794de6c, $9a86c57b, $9188cc76, $a0a2f355, $abacfa58, $b6bee14f, $bdb0e842,
  $d4ea9f09, $dfe49604, $c2f68d13, $c9f8841e, $f8d2bb3d, $f3dcb230, $eecea927, $e5c0a02a,
  $3c7a47b1, $37744ebc, $2a6655ab, $21685ca6, $10426385, $1b4c6a88, $065e719f, $0d507892,
  $640a0fd9, $6f0406d4, $72161dc3, $791814ce, $48322bed, $433c22e0, $5e2e39f7, $552030fa,
  $01ec9ab7, $0ae293ba, $17f088ad, $1cfe81a0, $2dd4be83, $26dab78e, $3bc8ac99, $30c6a594,
  $599cd2df, $5292dbd2, $4f80c0c5, $448ec9c8, $75a4f6eb, $7eaaffe6, $63b8e4f1, $68b6edfc,
  $b10c0a67, $ba02036a, $a710187d, $ac1e1170, $9d342e53, $963a275e, $8b283c49, $80263544,
  $e97c420f, $e2724b02, $ff605015, $f46e5918, $c544663b, $ce4a6f36, $d3587421, $d8567d2c,
  $7a37a10c, $7139a801, $6c2bb316, $6725ba1b, $560f8538, $5d018c35, $40139722, $4b1d9e2f,
  $2247e964, $2949e069, $345bfb7e, $3f55f273, $0e7fcd50, $0571c45d, $1863df4a, $136dd647,
  $cad731dc, $c1d938d1, $dccb23c6, $d7c52acb, $e6ef15e8, $ede11ce5, $f0f307f2, $fbfd0eff,
  $92a779b4, $99a970b9, $84bb6bae, $8fb562a3, $be9f5d80, $b591548d, $a8834f9a, $a38d4697);

 AES_TKS2:array[0..255] of LongWord = (
  $00000000, $0d0b0e09, $1a161c12, $171d121b, $342c3824, $3927362d, $2e3a2436, $23312a3f,
  $68587048, $65537e41, $724e6c5a, $7f456253, $5c74486c, $517f4665, $4662547e, $4b695a77,
  $d0b0e090, $ddbbee99, $caa6fc82, $c7adf28b, $e49cd8b4, $e997d6bd, $fe8ac4a6, $f381caaf,
  $b8e890d8, $b5e39ed1, $a2fe8cca, $aff582c3, $8cc4a8fc, $81cfa6f5, $96d2b4ee, $9bd9bae7,
  $bb7bdb3b, $b670d532, $a16dc729, $ac66c920, $8f57e31f, $825ced16, $9541ff0d, $984af104,
  $d323ab73, $de28a57a, $c935b761, $c43eb968, $e70f9357, $ea049d5e, $fd198f45, $f012814c,
  $6bcb3bab, $66c035a2, $71dd27b9, $7cd629b0, $5fe7038f, $52ec0d86, $45f11f9d, $48fa1194,
  $03934be3, $0e9845ea, $198557f1, $148e59f8, $37bf73c7, $3ab47dce, $2da96fd5, $20a261dc,
  $6df6ad76, $60fda37f, $77e0b164, $7aebbf6d, $59da9552, $54d19b5b, $43cc8940, $4ec78749,
  $05aedd3e, $08a5d337, $1fb8c12c, $12b3cf25, $3182e51a, $3c89eb13, $2b94f908, $269ff701,
  $bd464de6, $b04d43ef, $a75051f4, $aa5b5ffd, $896a75c2, $84617bcb, $937c69d0, $9e7767d9,
  $d51e3dae, $d81533a7, $cf0821bc, $c2032fb5, $e132058a, $ec390b83, $fb241998, $f62f1791,
  $d68d764d, $db867844, $cc9b6a5f, $c1906456, $e2a14e69, $efaa4060, $f8b7527b, $f5bc5c72,
  $bed50605, $b3de080c, $a4c31a17, $a9c8141e, $8af93e21, $87f23028, $90ef2233, $9de42c3a,
  $063d96dd, $0b3698d4, $1c2b8acf, $112084c6, $3211aef9, $3f1aa0f0, $2807b2eb, $250cbce2,
  $6e65e695, $636ee89c, $7473fa87, $7978f48e, $5a49deb1, $5742d0b8, $405fc2a3, $4d54ccaa,
  $daf741ec, $d7fc4fe5, $c0e15dfe, $cdea53f7, $eedb79c8, $e3d077c1, $f4cd65da, $f9c66bd3,
  $b2af31a4, $bfa43fad, $a8b92db6, $a5b223bf, $86830980, $8b880789, $9c951592, $919e1b9b,
  $0a47a17c, $074caf75, $1051bd6e, $1d5ab367, $3e6b9958, $33609751, $247d854a, $29768b43,
  $621fd134, $6f14df3d, $7809cd26, $7502c32f, $5633e910, $5b38e719, $4c25f502, $412efb0b,
  $618c9ad7, $6c8794de, $7b9a86c5, $769188cc, $55a0a2f3, $58abacfa, $4fb6bee1, $42bdb0e8,
  $09d4ea9f, $04dfe496, $13c2f68d, $1ec9f884, $3df8d2bb, $30f3dcb2, $27eecea9, $2ae5c0a0,
  $b13c7a47, $bc37744e, $ab2a6655, $a621685c, $85104263, $881b4c6a, $9f065e71, $920d5078,
  $d9640a0f, $d46f0406, $c372161d, $ce791814, $ed48322b, $e0433c22, $f75e2e39, $fa552030,
  $b701ec9a, $ba0ae293, $ad17f088, $a01cfe81, $832dd4be, $8e26dab7, $993bc8ac, $9430c6a5,
  $df599cd2, $d25292db, $c54f80c0, $c8448ec9, $eb75a4f6, $e67eaaff, $f163b8e4, $fc68b6ed,
  $67b10c0a, $6aba0203, $7da71018, $70ac1e11, $539d342e, $5e963a27, $498b283c, $44802635,
  $0fe97c42, $02e2724b, $15ff6050, $18f46e59, $3bc54466, $36ce4a6f, $21d35874, $2cd8567d,
  $0c7a37a1, $017139a8, $166c2bb3, $1b6725ba, $38560f85, $355d018c, $22401397, $2f4b1d9e,
  $642247e9, $692949e0, $7e345bfb, $733f55f2, $500e7fcd, $5d0571c4, $4a1863df, $47136dd6,
  $dccad731, $d1c1d938, $c6dccb23, $cbd7c52a, $e8e6ef15, $e5ede11c, $f2f0f307, $fffbfd0e,
  $b492a779, $b999a970, $ae84bb6b, $a38fb562, $80be9f5d, $8db59154, $9aa8834f, $97a38d46);

 AES_TKS3:array[0..255] of LongWord = (
  $00000000, $090d0b0e, $121a161c, $1b171d12, $24342c38, $2d392736, $362e3a24, $3f23312a,
  $48685870, $4165537e, $5a724e6c, $537f4562, $6c5c7448, $65517f46, $7e466254, $774b695a,
  $90d0b0e0, $99ddbbee, $82caa6fc, $8bc7adf2, $b4e49cd8, $bde997d6, $a6fe8ac4, $aff381ca,
  $d8b8e890, $d1b5e39e, $caa2fe8c, $c3aff582, $fc8cc4a8, $f581cfa6, $ee96d2b4, $e79bd9ba,
  $3bbb7bdb, $32b670d5, $29a16dc7, $20ac66c9, $1f8f57e3, $16825ced, $0d9541ff, $04984af1,
  $73d323ab, $7ade28a5, $61c935b7, $68c43eb9, $57e70f93, $5eea049d, $45fd198f, $4cf01281,
  $ab6bcb3b, $a266c035, $b971dd27, $b07cd629, $8f5fe703, $8652ec0d, $9d45f11f, $9448fa11,
  $e303934b, $ea0e9845, $f1198557, $f8148e59, $c737bf73, $ce3ab47d, $d52da96f, $dc20a261,
  $766df6ad, $7f60fda3, $6477e0b1, $6d7aebbf, $5259da95, $5b54d19b, $4043cc89, $494ec787,
  $3e05aedd, $3708a5d3, $2c1fb8c1, $2512b3cf, $1a3182e5, $133c89eb, $082b94f9, $01269ff7,
  $e6bd464d, $efb04d43, $f4a75051, $fdaa5b5f, $c2896a75, $cb84617b, $d0937c69, $d99e7767,
  $aed51e3d, $a7d81533, $bccf0821, $b5c2032f, $8ae13205, $83ec390b, $98fb2419, $91f62f17,
  $4dd68d76, $44db8678, $5fcc9b6a, $56c19064, $69e2a14e, $60efaa40, $7bf8b752, $72f5bc5c,
  $05bed506, $0cb3de08, $17a4c31a, $1ea9c814, $218af93e, $2887f230, $3390ef22, $3a9de42c,
  $dd063d96, $d40b3698, $cf1c2b8a, $c6112084, $f93211ae, $f03f1aa0, $eb2807b2, $e2250cbc,
  $956e65e6, $9c636ee8, $877473fa, $8e7978f4, $b15a49de, $b85742d0, $a3405fc2, $aa4d54cc,
  $ecdaf741, $e5d7fc4f, $fec0e15d, $f7cdea53, $c8eedb79, $c1e3d077, $daf4cd65, $d3f9c66b,
  $a4b2af31, $adbfa43f, $b6a8b92d, $bfa5b223, $80868309, $898b8807, $929c9515, $9b919e1b,
  $7c0a47a1, $75074caf, $6e1051bd, $671d5ab3, $583e6b99, $51336097, $4a247d85, $4329768b,
  $34621fd1, $3d6f14df, $267809cd, $2f7502c3, $105633e9, $195b38e7, $024c25f5, $0b412efb,
  $d7618c9a, $de6c8794, $c57b9a86, $cc769188, $f355a0a2, $fa58abac, $e14fb6be, $e842bdb0,
  $9f09d4ea, $9604dfe4, $8d13c2f6, $841ec9f8, $bb3df8d2, $b230f3dc, $a927eece, $a02ae5c0,
  $47b13c7a, $4ebc3774, $55ab2a66, $5ca62168, $63851042, $6a881b4c, $719f065e, $78920d50,
  $0fd9640a, $06d46f04, $1dc37216, $14ce7918, $2bed4832, $22e0433c, $39f75e2e, $30fa5520,
  $9ab701ec, $93ba0ae2, $88ad17f0, $81a01cfe, $be832dd4, $b78e26da, $ac993bc8, $a59430c6,
  $d2df599c, $dbd25292, $c0c54f80, $c9c8448e, $f6eb75a4, $ffe67eaa, $e4f163b8, $edfc68b6,
  $0a67b10c, $036aba02, $187da710, $1170ac1e, $2e539d34, $275e963a, $3c498b28, $35448026,
  $420fe97c, $4b02e272, $5015ff60, $5918f46e, $663bc544, $6f36ce4a, $7421d358, $7d2cd856,
  $a10c7a37, $a8017139, $b3166c2b, $ba1b6725, $8538560f, $8c355d01, $97224013, $9e2f4b1d,
  $e9642247, $e0692949, $fb7e345b, $f2733f55, $cd500e7f, $c45d0571, $df4a1863, $d647136d,
  $31dccad7, $38d1c1d9, $23c6dccb, $2acbd7c5, $15e8e6ef, $1ce5ede1, $07f2f0f3, $0efffbfd,
  $79b492a7, $70b999a9, $6bae84bb, $62a38fb5, $5d80be9f, $548db591, $4f9aa883, $4697a38d);
 {$ENDIF CRYPTO_AES_PRECOMPUTED_TABLES}

 {DES constants}
 DES_BLOCK_SIZE = 8;  {64 bit blocks}

 DES_KEY_SIZE = 8; {64 bit keys}

 DES_KEYTYPE_ENCRYPT = 0;
 DES_KEYTYPE_DECRYPT = 1;

 DES_BYTEBIT:array[0..7] of LongWord = (
  $80, $40, $20, $10, $08, $04, $02, $01);

 DES_BIGBYTE:array[0..23] of LongWord = (
  $00800000, $00400000, $00200000, $00100000,
  $00080000, $00040000, $00020000, $00010000,
  $00008000, $00004000, $00002000, $00001000,
  $00000800, $00000400, $00000200, $00000100,
  $00000080, $00000040, $00000020, $00000010,
  $00000008, $00000004, $00000002, $00000001);

 DES_TOTROT:array[0..15] of Byte = (
  1,   2,  4,  6,
  8,  10, 12, 14,
  15, 17, 19, 21,
  23, 25, 27, 28);

 DES_PC1:array[0..55] of Byte = (       {Permuted choice 1 (PC-1)}
  56, 48, 40, 32, 24, 16,  8,  0, 57, 49, 41, 33, 25, 17,
   9,  1, 58, 50, 42, 34, 26, 18, 10,  2, 59, 51, 43, 35,
  62, 54, 46, 38, 30, 22, 14,  6, 61, 53, 45, 37, 29, 21,
  13,  5, 60, 52, 44, 36, 28, 20, 12,  4, 27, 19, 11,  3);

 DES_PC2:array[0..47] of Byte = (       {Permuted choice 2 (PC-2)}
  13, 16, 10, 23,  0,  4,  2, 27, 14,  5, 20,  9,
  22, 18, 11,  3, 25,  7, 15,  6, 26, 19, 12,  1,
  40, 51, 30, 36, 46, 54, 29, 39, 50, 44, 32, 47,
  43, 48, 38, 55, 33, 52, 45, 41, 49, 35, 28, 31);

 DES_S1:array[0..63] of LongWord = (    {Substitution boxes (S-boxes)(S1)}
  $01010400, $00000000, $00010000, $01010404,
  $01010004, $00010404, $00000004, $00010000,
  $00000400, $01010400, $01010404, $00000400,
  $01000404, $01010004, $01000000, $00000004,
  $00000404, $01000400, $01000400, $00010400,
  $00010400, $01010000, $01010000, $01000404,
  $00010004, $01000004, $01000004, $00010004,
  $00000000, $00000404, $00010404, $01000000,
  $00010000, $01010404, $00000004, $01010000,
  $01010400, $01000000, $01000000, $00000400,
  $01010004, $00010000, $00010400, $01000004,
  $00000400, $00000004, $01000404, $00010404,
  $01010404, $00010004, $01010000, $01000404,
  $01000004, $00000404, $00010404, $01010400,
  $00000404, $01000400, $01000400, $00000000,
  $00010004, $00010400, $00000000, $01010004);

 DES_S2:array[0..63] of LongWord = (    {Substitution boxes (S-boxes)(S2)}
  $80108020, $80008000, $00008000, $00108020,
  $00100000, $00000020, $80100020, $80008020,
  $80000020, $80108020, $80108000, $80000000,
  $80008000, $00100000, $00000020, $80100020,
  $00108000, $00100020, $80008020, $00000000,
  $80000000, $00008000, $00108020, $80100000,
  $00100020, $80000020, $00000000, $00108000,
  $00008020, $80108000, $80100000, $00008020,
  $00000000, $00108020, $80100020, $00100000,
  $80008020, $80100000, $80108000, $00008000,
  $80100000, $80008000, $00000020, $80108020,
  $00108020, $00000020, $00008000, $80000000,
  $00008020, $80108000, $00100000, $80000020,
  $00100020, $80008020, $80000020, $00100020,
  $00108000, $00000000, $80008000, $00008020,
  $80000000, $80100020, $80108020, $00108000);

 DES_S3:array[0..63] of LongWord = (    {Substitution boxes (S-boxes)(S3)}
  $00000208, $08020200, $00000000, $08020008,
  $08000200, $00000000, $00020208, $08000200,
  $00020008, $08000008, $08000008, $00020000,
  $08020208, $00020008, $08020000, $00000208,
  $08000000, $00000008, $08020200, $00000200,
  $00020200, $08020000, $08020008, $00020208,
  $08000208, $00020200, $00020000, $08000208,
  $00000008, $08020208, $00000200, $08000000,
  $08020200, $08000000, $00020008, $00000208,
  $00020000, $08020200, $08000200, $00000000,
  $00000200, $00020008, $08020208, $08000200,
  $08000008, $00000200, $00000000, $08020008,
  $08000208, $00020000, $08000000, $08020208,
  $00000008, $00020208, $00020200, $08000008,
  $08020000, $08000208, $00000208, $08020000,
  $00020208, $00000008, $08020008, $00020200);

 DES_S4:array[0..63] of LongWord = (    {Substitution boxes (S-boxes)(S4)}
  $00802001, $00002081, $00002081, $00000080,
  $00802080, $00800081, $00800001, $00002001,
  $00000000, $00802000, $00802000, $00802081,
  $00000081, $00000000, $00800080, $00800001,
  $00000001, $00002000, $00800000, $00802001,
  $00000080, $00800000, $00002001, $00002080,
  $00800081, $00000001, $00002080, $00800080,
  $00002000, $00802080, $00802081, $00000081,
  $00800080, $00800001, $00802000, $00802081,
  $00000081, $00000000, $00000000, $00802000,
  $00002080, $00800080, $00800081, $00000001,
  $00802001, $00002081, $00002081, $00000080,
  $00802081, $00000081, $00000001, $00002000,
  $00800001, $00002001, $00802080, $00800081,
  $00002001, $00002080, $00800000, $00802001,
  $00000080, $00800000, $00002000, $00802080);

 DES_S5:array[0..63] of LongWord = (    {Substitution boxes (S-boxes)(S5)}
  $00000100, $02080100, $02080000, $42000100,
  $00080000, $00000100, $40000000, $02080000,
  $40080100, $00080000, $02000100, $40080100,
  $42000100, $42080000, $00080100, $40000000,
  $02000000, $40080000, $40080000, $00000000,
  $40000100, $42080100, $42080100, $02000100,
  $42080000, $40000100, $00000000, $42000000,
  $02080100, $02000000, $42000000, $00080100,
  $00080000, $42000100, $00000100, $02000000,
  $40000000, $02080000, $42000100, $40080100,
  $02000100, $40000000, $42080000, $02080100,
  $40080100, $00000100, $02000000, $42080000,
  $42080100, $00080100, $42000000, $42080100,
  $02080000, $00000000, $40080000, $42000000,
  $00080100, $02000100, $40000100, $00080000,
  $00000000, $40080000, $02080100, $40000100);

 DES_S6:array[0..63] of LongWord = (    {Substitution boxes (S-boxes)(S6)}
  $20000010, $20400000, $00004000, $20404010,
  $20400000, $00000010, $20404010, $00400000,
  $20004000, $00404010, $00400000, $20000010,
  $00400010, $20004000, $20000000, $00004010,
  $00000000, $00400010, $20004010, $00004000,
  $00404000, $20004010, $00000010, $20400010,
  $20400010, $00000000, $00404010, $20404000,
  $00004010, $00404000, $20404000, $20000000,
  $20004000, $00000010, $20400010, $00404000,
  $20404010, $00400000, $00004010, $20000010,
  $00400000, $20004000, $20000000, $00004010,
  $20000010, $20404010, $00404000, $20400000,
  $00404010, $20404000, $00000000, $20400010,
  $00000010, $00004000, $20400000, $00404010,
  $00004000, $00400010, $20004010, $00000000,
  $20404000, $20000000, $00400010, $20004010);

 DES_S7:array[0..63] of LongWord = (    {Substitution boxes (S-boxes)(S7)}
  $00200000, $04200002, $04000802, $00000000,
  $00000800, $04000802, $00200802, $04200800,
  $04200802, $00200000, $00000000, $04000002,
  $00000002, $04000000, $04200002, $00000802,
  $04000800, $00200802, $00200002, $04000800,
  $04000002, $04200000, $04200800, $00200002,
  $04200000, $00000800, $00000802, $04200802,
  $00200800, $00000002, $04000000, $00200800,
  $04000000, $00200800, $00200000, $04000802,
  $04000802, $04200002, $04200002, $00000002,
  $00200002, $04000000, $04000800, $00200000,
  $04200800, $00000802, $00200802, $04200800,
  $00000802, $04000002, $04200802, $04200000,
  $00200800, $00000000, $00000002, $04200802,
  $00000000, $00200802, $04200000, $00000800,
  $04000002, $04000800, $00000800, $00200002);

 DES_S8:array[0..63] of LongWord = (    {Substitution boxes (S-boxes)(S8)}
  $10001040, $00001000, $00040000, $10041040,
  $10000000, $10001040, $00000040, $10000000,
  $00040040, $10040000, $10041040, $00041000,
  $10041000, $00041040, $00001000, $00000040,
  $10040000, $10000040, $10001000, $00001040,
  $00041000, $00040040, $10040040, $10041000,
  $00001040, $00000000, $00000000, $10040040,
  $10000040, $10001000, $00041040, $00040000,
  $00041040, $00040000, $10041000, $00001000,
  $00000040, $10040040, $00001000, $00041040,
  $10001000, $00000040, $10000040, $10040000,
  $10040040, $10000000, $00040000, $10001040,
  $00000000, $10041040, $00040040, $10000040,
  $10040000, $10001000, $10001040, $00000000,
  $10041040, $00041000, $00041000, $00001040,
  $00001040, $00040040, $10000000, $10041000);

 {$IFDEF CRYPTO_DES_PRECOMPUTED_PERMUTATIONS}
 DES_IP:array[0..7,0..255] of QWord = ( {Initial permutation (IP)}
( $0000000000000000, $0000001000000000, $0000000000000010, $0000001000000010,
  $0000100000000000, $0000101000000000, $0000100000000010, $0000101000000010,
  $0000000000001000, $0000001000001000, $0000000000001010, $0000001000001010,
  $0000100000001000, $0000101000001000, $0000100000001010, $0000101000001010,
  $0010000000000000, $0010001000000000, $0010000000000010, $0010001000000010,
  $0010100000000000, $0010101000000000, $0010100000000010, $0010101000000010,
  $0010000000001000, $0010001000001000, $0010000000001010, $0010001000001010,
  $0010100000001000, $0010101000001000, $0010100000001010, $0010101000001010,
  $0000000000100000, $0000001000100000, $0000000000100010, $0000001000100010,
  $0000100000100000, $0000101000100000, $0000100000100010, $0000101000100010,
  $0000000000101000, $0000001000101000, $0000000000101010, $0000001000101010,
  $0000100000101000, $0000101000101000, $0000100000101010, $0000101000101010,
  $0010000000100000, $0010001000100000, $0010000000100010, $0010001000100010,
  $0010100000100000, $0010101000100000, $0010100000100010, $0010101000100010,
  $0010000000101000, $0010001000101000, $0010000000101010, $0010001000101010,
  $0010100000101000, $0010101000101000, $0010100000101010, $0010101000101010,
  $1000000000000000, $1000001000000000, $1000000000000010, $1000001000000010,
  $1000100000000000, $1000101000000000, $1000100000000010, $1000101000000010,
  $1000000000001000, $1000001000001000, $1000000000001010, $1000001000001010,
  $1000100000001000, $1000101000001000, $1000100000001010, $1000101000001010,
  $1010000000000000, $1010001000000000, $1010000000000010, $1010001000000010,
  $1010100000000000, $1010101000000000, $1010100000000010, $1010101000000010,
  $1010000000001000, $1010001000001000, $1010000000001010, $1010001000001010,
  $1010100000001000, $1010101000001000, $1010100000001010, $1010101000001010,
  $1000000000100000, $1000001000100000, $1000000000100010, $1000001000100010,
  $1000100000100000, $1000101000100000, $1000100000100010, $1000101000100010,
  $1000000000101000, $1000001000101000, $1000000000101010, $1000001000101010,
  $1000100000101000, $1000101000101000, $1000100000101010, $1000101000101010,
  $1010000000100000, $1010001000100000, $1010000000100010, $1010001000100010,
  $1010100000100000, $1010101000100000, $1010100000100010, $1010101000100010,
  $1010000000101000, $1010001000101000, $1010000000101010, $1010001000101010,
  $1010100000101000, $1010101000101000, $1010100000101010, $1010101000101010,
  $0000000010000000, $0000001010000000, $0000000010000010, $0000001010000010,
  $0000100010000000, $0000101010000000, $0000100010000010, $0000101010000010,
  $0000000010001000, $0000001010001000, $0000000010001010, $0000001010001010,
  $0000100010001000, $0000101010001000, $0000100010001010, $0000101010001010,
  $0010000010000000, $0010001010000000, $0010000010000010, $0010001010000010,
  $0010100010000000, $0010101010000000, $0010100010000010, $0010101010000010,
  $0010000010001000, $0010001010001000, $0010000010001010, $0010001010001010,
  $0010100010001000, $0010101010001000, $0010100010001010, $0010101010001010,
  $0000000010100000, $0000001010100000, $0000000010100010, $0000001010100010,
  $0000100010100000, $0000101010100000, $0000100010100010, $0000101010100010,
  $0000000010101000, $0000001010101000, $0000000010101010, $0000001010101010,
  $0000100010101000, $0000101010101000, $0000100010101010, $0000101010101010,
  $0010000010100000, $0010001010100000, $0010000010100010, $0010001010100010,
  $0010100010100000, $0010101010100000, $0010100010100010, $0010101010100010,
  $0010000010101000, $0010001010101000, $0010000010101010, $0010001010101010,
  $0010100010101000, $0010101010101000, $0010100010101010, $0010101010101010,
  $1000000010000000, $1000001010000000, $1000000010000010, $1000001010000010,
  $1000100010000000, $1000101010000000, $1000100010000010, $1000101010000010,
  $1000000010001000, $1000001010001000, $1000000010001010, $1000001010001010,
  $1000100010001000, $1000101010001000, $1000100010001010, $1000101010001010,
  $1010000010000000, $1010001010000000, $1010000010000010, $1010001010000010,
  $1010100010000000, $1010101010000000, $1010100010000010, $1010101010000010,
  $1010000010001000, $1010001010001000, $1010000010001010, $1010001010001010,
  $1010100010001000, $1010101010001000, $1010100010001010, $1010101010001010,
  $1000000010100000, $1000001010100000, $1000000010100010, $1000001010100010,
  $1000100010100000, $1000101010100000, $1000100010100010, $1000101010100010,
  $1000000010101000, $1000001010101000, $1000000010101010, $1000001010101010,
  $1000100010101000, $1000101010101000, $1000100010101010, $1000101010101010,
  $1010000010100000, $1010001010100000, $1010000010100010, $1010001010100010,
  $1010100010100000, $1010101010100000, $1010100010100010, $1010101010100010,
  $1010000010101000, $1010001010101000, $1010000010101010, $1010001010101010,
  $1010100010101000, $1010101010101000, $1010100010101010, $1010101010101010),

( $0000000000000000, $0000000800000000, $0000000000000008, $0000000800000008,
  $0000080000000000, $0000080800000000, $0000080000000008, $0000080800000008,
  $0000000000000800, $0000000800000800, $0000000000000808, $0000000800000808,
  $0000080000000800, $0000080800000800, $0000080000000808, $0000080800000808,
  $0008000000000000, $0008000800000000, $0008000000000008, $0008000800000008,
  $0008080000000000, $0008080800000000, $0008080000000008, $0008080800000008,
  $0008000000000800, $0008000800000800, $0008000000000808, $0008000800000808,
  $0008080000000800, $0008080800000800, $0008080000000808, $0008080800000808,
  $0000000000080000, $0000000800080000, $0000000000080008, $0000000800080008,
  $0000080000080000, $0000080800080000, $0000080000080008, $0000080800080008,
  $0000000000080800, $0000000800080800, $0000000000080808, $0000000800080808,
  $0000080000080800, $0000080800080800, $0000080000080808, $0000080800080808,
  $0008000000080000, $0008000800080000, $0008000000080008, $0008000800080008,
  $0008080000080000, $0008080800080000, $0008080000080008, $0008080800080008,
  $0008000000080800, $0008000800080800, $0008000000080808, $0008000800080808,
  $0008080000080800, $0008080800080800, $0008080000080808, $0008080800080808,
  $0800000000000000, $0800000800000000, $0800000000000008, $0800000800000008,
  $0800080000000000, $0800080800000000, $0800080000000008, $0800080800000008,
  $0800000000000800, $0800000800000800, $0800000000000808, $0800000800000808,
  $0800080000000800, $0800080800000800, $0800080000000808, $0800080800000808,
  $0808000000000000, $0808000800000000, $0808000000000008, $0808000800000008,
  $0808080000000000, $0808080800000000, $0808080000000008, $0808080800000008,
  $0808000000000800, $0808000800000800, $0808000000000808, $0808000800000808,
  $0808080000000800, $0808080800000800, $0808080000000808, $0808080800000808,
  $0800000000080000, $0800000800080000, $0800000000080008, $0800000800080008,
  $0800080000080000, $0800080800080000, $0800080000080008, $0800080800080008,
  $0800000000080800, $0800000800080800, $0800000000080808, $0800000800080808,
  $0800080000080800, $0800080800080800, $0800080000080808, $0800080800080808,
  $0808000000080000, $0808000800080000, $0808000000080008, $0808000800080008,
  $0808080000080000, $0808080800080000, $0808080000080008, $0808080800080008,
  $0808000000080800, $0808000800080800, $0808000000080808, $0808000800080808,
  $0808080000080800, $0808080800080800, $0808080000080808, $0808080800080808,
  $0000000008000000, $0000000808000000, $0000000008000008, $0000000808000008,
  $0000080008000000, $0000080808000000, $0000080008000008, $0000080808000008,
  $0000000008000800, $0000000808000800, $0000000008000808, $0000000808000808,
  $0000080008000800, $0000080808000800, $0000080008000808, $0000080808000808,
  $0008000008000000, $0008000808000000, $0008000008000008, $0008000808000008,
  $0008080008000000, $0008080808000000, $0008080008000008, $0008080808000008,
  $0008000008000800, $0008000808000800, $0008000008000808, $0008000808000808,
  $0008080008000800, $0008080808000800, $0008080008000808, $0008080808000808,
  $0000000008080000, $0000000808080000, $0000000008080008, $0000000808080008,
  $0000080008080000, $0000080808080000, $0000080008080008, $0000080808080008,
  $0000000008080800, $0000000808080800, $0000000008080808, $0000000808080808,
  $0000080008080800, $0000080808080800, $0000080008080808, $0000080808080808,
  $0008000008080000, $0008000808080000, $0008000008080008, $0008000808080008,
  $0008080008080000, $0008080808080000, $0008080008080008, $0008080808080008,
  $0008000008080800, $0008000808080800, $0008000008080808, $0008000808080808,
  $0008080008080800, $0008080808080800, $0008080008080808, $0008080808080808,
  $0800000008000000, $0800000808000000, $0800000008000008, $0800000808000008,
  $0800080008000000, $0800080808000000, $0800080008000008, $0800080808000008,
  $0800000008000800, $0800000808000800, $0800000008000808, $0800000808000808,
  $0800080008000800, $0800080808000800, $0800080008000808, $0800080808000808,
  $0808000008000000, $0808000808000000, $0808000008000008, $0808000808000008,
  $0808080008000000, $0808080808000000, $0808080008000008, $0808080808000008,
  $0808000008000800, $0808000808000800, $0808000008000808, $0808000808000808,
  $0808080008000800, $0808080808000800, $0808080008000808, $0808080808000808,
  $0800000008080000, $0800000808080000, $0800000008080008, $0800000808080008,
  $0800080008080000, $0800080808080000, $0800080008080008, $0800080808080008,
  $0800000008080800, $0800000808080800, $0800000008080808, $0800000808080808,
  $0800080008080800, $0800080808080800, $0800080008080808, $0800080808080808,
  $0808000008080000, $0808000808080000, $0808000008080008, $0808000808080008,
  $0808080008080000, $0808080808080000, $0808080008080008, $0808080808080008,
  $0808000008080800, $0808000808080800, $0808000008080808, $0808000808080808,
  $0808080008080800, $0808080808080800, $0808080008080808, $0808080808080808),

( $0000000000000000, $0000000400000000, $0000000000000004, $0000000400000004,
  $0000040000000000, $0000040400000000, $0000040000000004, $0000040400000004,
  $0000000000000400, $0000000400000400, $0000000000000404, $0000000400000404,
  $0000040000000400, $0000040400000400, $0000040000000404, $0000040400000404,
  $0004000000000000, $0004000400000000, $0004000000000004, $0004000400000004,
  $0004040000000000, $0004040400000000, $0004040000000004, $0004040400000004,
  $0004000000000400, $0004000400000400, $0004000000000404, $0004000400000404,
  $0004040000000400, $0004040400000400, $0004040000000404, $0004040400000404,
  $0000000000040000, $0000000400040000, $0000000000040004, $0000000400040004,
  $0000040000040000, $0000040400040000, $0000040000040004, $0000040400040004,
  $0000000000040400, $0000000400040400, $0000000000040404, $0000000400040404,
  $0000040000040400, $0000040400040400, $0000040000040404, $0000040400040404,
  $0004000000040000, $0004000400040000, $0004000000040004, $0004000400040004,
  $0004040000040000, $0004040400040000, $0004040000040004, $0004040400040004,
  $0004000000040400, $0004000400040400, $0004000000040404, $0004000400040404,
  $0004040000040400, $0004040400040400, $0004040000040404, $0004040400040404,
  $0400000000000000, $0400000400000000, $0400000000000004, $0400000400000004,
  $0400040000000000, $0400040400000000, $0400040000000004, $0400040400000004,
  $0400000000000400, $0400000400000400, $0400000000000404, $0400000400000404,
  $0400040000000400, $0400040400000400, $0400040000000404, $0400040400000404,
  $0404000000000000, $0404000400000000, $0404000000000004, $0404000400000004,
  $0404040000000000, $0404040400000000, $0404040000000004, $0404040400000004,
  $0404000000000400, $0404000400000400, $0404000000000404, $0404000400000404,
  $0404040000000400, $0404040400000400, $0404040000000404, $0404040400000404,
  $0400000000040000, $0400000400040000, $0400000000040004, $0400000400040004,
  $0400040000040000, $0400040400040000, $0400040000040004, $0400040400040004,
  $0400000000040400, $0400000400040400, $0400000000040404, $0400000400040404,
  $0400040000040400, $0400040400040400, $0400040000040404, $0400040400040404,
  $0404000000040000, $0404000400040000, $0404000000040004, $0404000400040004,
  $0404040000040000, $0404040400040000, $0404040000040004, $0404040400040004,
  $0404000000040400, $0404000400040400, $0404000000040404, $0404000400040404,
  $0404040000040400, $0404040400040400, $0404040000040404, $0404040400040404,
  $0000000004000000, $0000000404000000, $0000000004000004, $0000000404000004,
  $0000040004000000, $0000040404000000, $0000040004000004, $0000040404000004,
  $0000000004000400, $0000000404000400, $0000000004000404, $0000000404000404,
  $0000040004000400, $0000040404000400, $0000040004000404, $0000040404000404,
  $0004000004000000, $0004000404000000, $0004000004000004, $0004000404000004,
  $0004040004000000, $0004040404000000, $0004040004000004, $0004040404000004,
  $0004000004000400, $0004000404000400, $0004000004000404, $0004000404000404,
  $0004040004000400, $0004040404000400, $0004040004000404, $0004040404000404,
  $0000000004040000, $0000000404040000, $0000000004040004, $0000000404040004,
  $0000040004040000, $0000040404040000, $0000040004040004, $0000040404040004,
  $0000000004040400, $0000000404040400, $0000000004040404, $0000000404040404,
  $0000040004040400, $0000040404040400, $0000040004040404, $0000040404040404,
  $0004000004040000, $0004000404040000, $0004000004040004, $0004000404040004,
  $0004040004040000, $0004040404040000, $0004040004040004, $0004040404040004,
  $0004000004040400, $0004000404040400, $0004000004040404, $0004000404040404,
  $0004040004040400, $0004040404040400, $0004040004040404, $0004040404040404,
  $0400000004000000, $0400000404000000, $0400000004000004, $0400000404000004,
  $0400040004000000, $0400040404000000, $0400040004000004, $0400040404000004,
  $0400000004000400, $0400000404000400, $0400000004000404, $0400000404000404,
  $0400040004000400, $0400040404000400, $0400040004000404, $0400040404000404,
  $0404000004000000, $0404000404000000, $0404000004000004, $0404000404000004,
  $0404040004000000, $0404040404000000, $0404040004000004, $0404040404000004,
  $0404000004000400, $0404000404000400, $0404000004000404, $0404000404000404,
  $0404040004000400, $0404040404000400, $0404040004000404, $0404040404000404,
  $0400000004040000, $0400000404040000, $0400000004040004, $0400000404040004,
  $0400040004040000, $0400040404040000, $0400040004040004, $0400040404040004,
  $0400000004040400, $0400000404040400, $0400000004040404, $0400000404040404,
  $0400040004040400, $0400040404040400, $0400040004040404, $0400040404040404,
  $0404000004040000, $0404000404040000, $0404000004040004, $0404000404040004,
  $0404040004040000, $0404040404040000, $0404040004040004, $0404040404040004,
  $0404000004040400, $0404000404040400, $0404000004040404, $0404000404040404,
  $0404040004040400, $0404040404040400, $0404040004040404, $0404040404040404),

( $0000000000000000, $0000000200000000, $0000000000000002, $0000000200000002,
  $0000020000000000, $0000020200000000, $0000020000000002, $0000020200000002,
  $0000000000000200, $0000000200000200, $0000000000000202, $0000000200000202,
  $0000020000000200, $0000020200000200, $0000020000000202, $0000020200000202,
  $0002000000000000, $0002000200000000, $0002000000000002, $0002000200000002,
  $0002020000000000, $0002020200000000, $0002020000000002, $0002020200000002,
  $0002000000000200, $0002000200000200, $0002000000000202, $0002000200000202,
  $0002020000000200, $0002020200000200, $0002020000000202, $0002020200000202,
  $0000000000020000, $0000000200020000, $0000000000020002, $0000000200020002,
  $0000020000020000, $0000020200020000, $0000020000020002, $0000020200020002,
  $0000000000020200, $0000000200020200, $0000000000020202, $0000000200020202,
  $0000020000020200, $0000020200020200, $0000020000020202, $0000020200020202,
  $0002000000020000, $0002000200020000, $0002000000020002, $0002000200020002,
  $0002020000020000, $0002020200020000, $0002020000020002, $0002020200020002,
  $0002000000020200, $0002000200020200, $0002000000020202, $0002000200020202,
  $0002020000020200, $0002020200020200, $0002020000020202, $0002020200020202,
  $0200000000000000, $0200000200000000, $0200000000000002, $0200000200000002,
  $0200020000000000, $0200020200000000, $0200020000000002, $0200020200000002,
  $0200000000000200, $0200000200000200, $0200000000000202, $0200000200000202,
  $0200020000000200, $0200020200000200, $0200020000000202, $0200020200000202,
  $0202000000000000, $0202000200000000, $0202000000000002, $0202000200000002,
  $0202020000000000, $0202020200000000, $0202020000000002, $0202020200000002,
  $0202000000000200, $0202000200000200, $0202000000000202, $0202000200000202,
  $0202020000000200, $0202020200000200, $0202020000000202, $0202020200000202,
  $0200000000020000, $0200000200020000, $0200000000020002, $0200000200020002,
  $0200020000020000, $0200020200020000, $0200020000020002, $0200020200020002,
  $0200000000020200, $0200000200020200, $0200000000020202, $0200000200020202,
  $0200020000020200, $0200020200020200, $0200020000020202, $0200020200020202,
  $0202000000020000, $0202000200020000, $0202000000020002, $0202000200020002,
  $0202020000020000, $0202020200020000, $0202020000020002, $0202020200020002,
  $0202000000020200, $0202000200020200, $0202000000020202, $0202000200020202,
  $0202020000020200, $0202020200020200, $0202020000020202, $0202020200020202,
  $0000000002000000, $0000000202000000, $0000000002000002, $0000000202000002,
  $0000020002000000, $0000020202000000, $0000020002000002, $0000020202000002,
  $0000000002000200, $0000000202000200, $0000000002000202, $0000000202000202,
  $0000020002000200, $0000020202000200, $0000020002000202, $0000020202000202,
  $0002000002000000, $0002000202000000, $0002000002000002, $0002000202000002,
  $0002020002000000, $0002020202000000, $0002020002000002, $0002020202000002,
  $0002000002000200, $0002000202000200, $0002000002000202, $0002000202000202,
  $0002020002000200, $0002020202000200, $0002020002000202, $0002020202000202,
  $0000000002020000, $0000000202020000, $0000000002020002, $0000000202020002,
  $0000020002020000, $0000020202020000, $0000020002020002, $0000020202020002,
  $0000000002020200, $0000000202020200, $0000000002020202, $0000000202020202,
  $0000020002020200, $0000020202020200, $0000020002020202, $0000020202020202,
  $0002000002020000, $0002000202020000, $0002000002020002, $0002000202020002,
  $0002020002020000, $0002020202020000, $0002020002020002, $0002020202020002,
  $0002000002020200, $0002000202020200, $0002000002020202, $0002000202020202,
  $0002020002020200, $0002020202020200, $0002020002020202, $0002020202020202,
  $0200000002000000, $0200000202000000, $0200000002000002, $0200000202000002,
  $0200020002000000, $0200020202000000, $0200020002000002, $0200020202000002,
  $0200000002000200, $0200000202000200, $0200000002000202, $0200000202000202,
  $0200020002000200, $0200020202000200, $0200020002000202, $0200020202000202,
  $0202000002000000, $0202000202000000, $0202000002000002, $0202000202000002,
  $0202020002000000, $0202020202000000, $0202020002000002, $0202020202000002,
  $0202000002000200, $0202000202000200, $0202000002000202, $0202000202000202,
  $0202020002000200, $0202020202000200, $0202020002000202, $0202020202000202,
  $0200000002020000, $0200000202020000, $0200000002020002, $0200000202020002,
  $0200020002020000, $0200020202020000, $0200020002020002, $0200020202020002,
  $0200000002020200, $0200000202020200, $0200000002020202, $0200000202020202,
  $0200020002020200, $0200020202020200, $0200020002020202, $0200020202020202,
  $0202000002020000, $0202000202020000, $0202000002020002, $0202000202020002,
  $0202020002020000, $0202020202020000, $0202020002020002, $0202020202020002,
  $0202000002020200, $0202000202020200, $0202000002020202, $0202000202020202,
  $0202020002020200, $0202020202020200, $0202020002020202, $0202020202020202),

( $0000000000000000, $0000010000000000, $0000000000000100, $0000010000000100,
  $0001000000000000, $0001010000000000, $0001000000000100, $0001010000000100,
  $0000000000010000, $0000010000010000, $0000000000010100, $0000010000010100,
  $0001000000010000, $0001010000010000, $0001000000010100, $0001010000010100,
  $0100000000000000, $0100010000000000, $0100000000000100, $0100010000000100,
  $0101000000000000, $0101010000000000, $0101000000000100, $0101010000000100,
  $0100000000010000, $0100010000010000, $0100000000010100, $0100010000010100,
  $0101000000010000, $0101010000010000, $0101000000010100, $0101010000010100,
  $0000000001000000, $0000010001000000, $0000000001000100, $0000010001000100,
  $0001000001000000, $0001010001000000, $0001000001000100, $0001010001000100,
  $0000000001010000, $0000010001010000, $0000000001010100, $0000010001010100,
  $0001000001010000, $0001010001010000, $0001000001010100, $0001010001010100,
  $0100000001000000, $0100010001000000, $0100000001000100, $0100010001000100,
  $0101000001000000, $0101010001000000, $0101000001000100, $0101010001000100,
  $0100000001010000, $0100010001010000, $0100000001010100, $0100010001010100,
  $0101000001010000, $0101010001010000, $0101000001010100, $0101010001010100,
  $0000000100000000, $0000010100000000, $0000000100000100, $0000010100000100,
  $0001000100000000, $0001010100000000, $0001000100000100, $0001010100000100,
  $0000000100010000, $0000010100010000, $0000000100010100, $0000010100010100,
  $0001000100010000, $0001010100010000, $0001000100010100, $0001010100010100,
  $0100000100000000, $0100010100000000, $0100000100000100, $0100010100000100,
  $0101000100000000, $0101010100000000, $0101000100000100, $0101010100000100,
  $0100000100010000, $0100010100010000, $0100000100010100, $0100010100010100,
  $0101000100010000, $0101010100010000, $0101000100010100, $0101010100010100,
  $0000000101000000, $0000010101000000, $0000000101000100, $0000010101000100,
  $0001000101000000, $0001010101000000, $0001000101000100, $0001010101000100,
  $0000000101010000, $0000010101010000, $0000000101010100, $0000010101010100,
  $0001000101010000, $0001010101010000, $0001000101010100, $0001010101010100,
  $0100000101000000, $0100010101000000, $0100000101000100, $0100010101000100,
  $0101000101000000, $0101010101000000, $0101000101000100, $0101010101000100,
  $0100000101010000, $0100010101010000, $0100000101010100, $0100010101010100,
  $0101000101010000, $0101010101010000, $0101000101010100, $0101010101010100,
  $0000000000000001, $0000010000000001, $0000000000000101, $0000010000000101,
  $0001000000000001, $0001010000000001, $0001000000000101, $0001010000000101,
  $0000000000010001, $0000010000010001, $0000000000010101, $0000010000010101,
  $0001000000010001, $0001010000010001, $0001000000010101, $0001010000010101,
  $0100000000000001, $0100010000000001, $0100000000000101, $0100010000000101,
  $0101000000000001, $0101010000000001, $0101000000000101, $0101010000000101,
  $0100000000010001, $0100010000010001, $0100000000010101, $0100010000010101,
  $0101000000010001, $0101010000010001, $0101000000010101, $0101010000010101,
  $0000000001000001, $0000010001000001, $0000000001000101, $0000010001000101,
  $0001000001000001, $0001010001000001, $0001000001000101, $0001010001000101,
  $0000000001010001, $0000010001010001, $0000000001010101, $0000010001010101,
  $0001000001010001, $0001010001010001, $0001000001010101, $0001010001010101,
  $0100000001000001, $0100010001000001, $0100000001000101, $0100010001000101,
  $0101000001000001, $0101010001000001, $0101000001000101, $0101010001000101,
  $0100000001010001, $0100010001010001, $0100000001010101, $0100010001010101,
  $0101000001010001, $0101010001010001, $0101000001010101, $0101010001010101,
  $0000000100000001, $0000010100000001, $0000000100000101, $0000010100000101,
  $0001000100000001, $0001010100000001, $0001000100000101, $0001010100000101,
  $0000000100010001, $0000010100010001, $0000000100010101, $0000010100010101,
  $0001000100010001, $0001010100010001, $0001000100010101, $0001010100010101,
  $0100000100000001, $0100010100000001, $0100000100000101, $0100010100000101,
  $0101000100000001, $0101010100000001, $0101000100000101, $0101010100000101,
  $0100000100010001, $0100010100010001, $0100000100010101, $0100010100010101,
  $0101000100010001, $0101010100010001, $0101000100010101, $0101010100010101,
  $0000000101000001, $0000010101000001, $0000000101000101, $0000010101000101,
  $0001000101000001, $0001010101000001, $0001000101000101, $0001010101000101,
  $0000000101010001, $0000010101010001, $0000000101010101, $0000010101010101,
  $0001000101010001, $0001010101010001, $0001000101010101, $0001010101010101,
  $0100000101000001, $0100010101000001, $0100000101000101, $0100010101000101,
  $0101000101000001, $0101010101000001, $0101000101000101, $0101010101000101,
  $0100000101010001, $0100010101010001, $0100000101010101, $0100010101010101,
  $0101000101010001, $0101010101010001, $0101000101010101, $0101010101010101),

( $0000000000000000, $0000008000000000, $0000000000000080, $0000008000000080,
  $0000800000000000, $0000808000000000, $0000800000000080, $0000808000000080,
  $0000000000008000, $0000008000008000, $0000000000008080, $0000008000008080,
  $0000800000008000, $0000808000008000, $0000800000008080, $0000808000008080,
  $0080000000000000, $0080008000000000, $0080000000000080, $0080008000000080,
  $0080800000000000, $0080808000000000, $0080800000000080, $0080808000000080,
  $0080000000008000, $0080008000008000, $0080000000008080, $0080008000008080,
  $0080800000008000, $0080808000008000, $0080800000008080, $0080808000008080,
  $0000000000800000, $0000008000800000, $0000000000800080, $0000008000800080,
  $0000800000800000, $0000808000800000, $0000800000800080, $0000808000800080,
  $0000000000808000, $0000008000808000, $0000000000808080, $0000008000808080,
  $0000800000808000, $0000808000808000, $0000800000808080, $0000808000808080,
  $0080000000800000, $0080008000800000, $0080000000800080, $0080008000800080,
  $0080800000800000, $0080808000800000, $0080800000800080, $0080808000800080,
  $0080000000808000, $0080008000808000, $0080000000808080, $0080008000808080,
  $0080800000808000, $0080808000808000, $0080800000808080, $0080808000808080,
  $8000000000000000, $8000008000000000, $8000000000000080, $8000008000000080,
  $8000800000000000, $8000808000000000, $8000800000000080, $8000808000000080,
  $8000000000008000, $8000008000008000, $8000000000008080, $8000008000008080,
  $8000800000008000, $8000808000008000, $8000800000008080, $8000808000008080,
  $8080000000000000, $8080008000000000, $8080000000000080, $8080008000000080,
  $8080800000000000, $8080808000000000, $8080800000000080, $8080808000000080,
  $8080000000008000, $8080008000008000, $8080000000008080, $8080008000008080,
  $8080800000008000, $8080808000008000, $8080800000008080, $8080808000008080,
  $8000000000800000, $8000008000800000, $8000000000800080, $8000008000800080,
  $8000800000800000, $8000808000800000, $8000800000800080, $8000808000800080,
  $8000000000808000, $8000008000808000, $8000000000808080, $8000008000808080,
  $8000800000808000, $8000808000808000, $8000800000808080, $8000808000808080,
  $8080000000800000, $8080008000800000, $8080000000800080, $8080008000800080,
  $8080800000800000, $8080808000800000, $8080800000800080, $8080808000800080,
  $8080000000808000, $8080008000808000, $8080000000808080, $8080008000808080,
  $8080800000808000, $8080808000808000, $8080800000808080, $8080808000808080,
  $0000000080000000, $0000008080000000, $0000000080000080, $0000008080000080,
  $0000800080000000, $0000808080000000, $0000800080000080, $0000808080000080,
  $0000000080008000, $0000008080008000, $0000000080008080, $0000008080008080,
  $0000800080008000, $0000808080008000, $0000800080008080, $0000808080008080,
  $0080000080000000, $0080008080000000, $0080000080000080, $0080008080000080,
  $0080800080000000, $0080808080000000, $0080800080000080, $0080808080000080,
  $0080000080008000, $0080008080008000, $0080000080008080, $0080008080008080,
  $0080800080008000, $0080808080008000, $0080800080008080, $0080808080008080,
  $0000000080800000, $0000008080800000, $0000000080800080, $0000008080800080,
  $0000800080800000, $0000808080800000, $0000800080800080, $0000808080800080,
  $0000000080808000, $0000008080808000, $0000000080808080, $0000008080808080,
  $0000800080808000, $0000808080808000, $0000800080808080, $0000808080808080,
  $0080000080800000, $0080008080800000, $0080000080800080, $0080008080800080,
  $0080800080800000, $0080808080800000, $0080800080800080, $0080808080800080,
  $0080000080808000, $0080008080808000, $0080000080808080, $0080008080808080,
  $0080800080808000, $0080808080808000, $0080800080808080, $0080808080808080,
  $8000000080000000, $8000008080000000, $8000000080000080, $8000008080000080,
  $8000800080000000, $8000808080000000, $8000800080000080, $8000808080000080,
  $8000000080008000, $8000008080008000, $8000000080008080, $8000008080008080,
  $8000800080008000, $8000808080008000, $8000800080008080, $8000808080008080,
  $8080000080000000, $8080008080000000, $8080000080000080, $8080008080000080,
  $8080800080000000, $8080808080000000, $8080800080000080, $8080808080000080,
  $8080000080008000, $8080008080008000, $8080000080008080, $8080008080008080,
  $8080800080008000, $8080808080008000, $8080800080008080, $8080808080008080,
  $8000000080800000, $8000008080800000, $8000000080800080, $8000008080800080,
  $8000800080800000, $8000808080800000, $8000800080800080, $8000808080800080,
  $8000000080808000, $8000008080808000, $8000000080808080, $8000008080808080,
  $8000800080808000, $8000808080808000, $8000800080808080, $8000808080808080,
  $8080000080800000, $8080008080800000, $8080000080800080, $8080008080800080,
  $8080800080800000, $8080808080800000, $8080800080800080, $8080808080800080,
  $8080000080808000, $8080008080808000, $8080000080808080, $8080008080808080,
  $8080800080808000, $8080808080808000, $8080800080808080, $8080808080808080),

( $0000000000000000, $0000004000000000, $0000000000000040, $0000004000000040,
  $0000400000000000, $0000404000000000, $0000400000000040, $0000404000000040,
  $0000000000004000, $0000004000004000, $0000000000004040, $0000004000004040,
  $0000400000004000, $0000404000004000, $0000400000004040, $0000404000004040,
  $0040000000000000, $0040004000000000, $0040000000000040, $0040004000000040,
  $0040400000000000, $0040404000000000, $0040400000000040, $0040404000000040,
  $0040000000004000, $0040004000004000, $0040000000004040, $0040004000004040,
  $0040400000004000, $0040404000004000, $0040400000004040, $0040404000004040,
  $0000000000400000, $0000004000400000, $0000000000400040, $0000004000400040,
  $0000400000400000, $0000404000400000, $0000400000400040, $0000404000400040,
  $0000000000404000, $0000004000404000, $0000000000404040, $0000004000404040,
  $0000400000404000, $0000404000404000, $0000400000404040, $0000404000404040,
  $0040000000400000, $0040004000400000, $0040000000400040, $0040004000400040,
  $0040400000400000, $0040404000400000, $0040400000400040, $0040404000400040,
  $0040000000404000, $0040004000404000, $0040000000404040, $0040004000404040,
  $0040400000404000, $0040404000404000, $0040400000404040, $0040404000404040,
  $4000000000000000, $4000004000000000, $4000000000000040, $4000004000000040,
  $4000400000000000, $4000404000000000, $4000400000000040, $4000404000000040,
  $4000000000004000, $4000004000004000, $4000000000004040, $4000004000004040,
  $4000400000004000, $4000404000004000, $4000400000004040, $4000404000004040,
  $4040000000000000, $4040004000000000, $4040000000000040, $4040004000000040,
  $4040400000000000, $4040404000000000, $4040400000000040, $4040404000000040,
  $4040000000004000, $4040004000004000, $4040000000004040, $4040004000004040,
  $4040400000004000, $4040404000004000, $4040400000004040, $4040404000004040,
  $4000000000400000, $4000004000400000, $4000000000400040, $4000004000400040,
  $4000400000400000, $4000404000400000, $4000400000400040, $4000404000400040,
  $4000000000404000, $4000004000404000, $4000000000404040, $4000004000404040,
  $4000400000404000, $4000404000404000, $4000400000404040, $4000404000404040,
  $4040000000400000, $4040004000400000, $4040000000400040, $4040004000400040,
  $4040400000400000, $4040404000400000, $4040400000400040, $4040404000400040,
  $4040000000404000, $4040004000404000, $4040000000404040, $4040004000404040,
  $4040400000404000, $4040404000404000, $4040400000404040, $4040404000404040,
  $0000000040000000, $0000004040000000, $0000000040000040, $0000004040000040,
  $0000400040000000, $0000404040000000, $0000400040000040, $0000404040000040,
  $0000000040004000, $0000004040004000, $0000000040004040, $0000004040004040,
  $0000400040004000, $0000404040004000, $0000400040004040, $0000404040004040,
  $0040000040000000, $0040004040000000, $0040000040000040, $0040004040000040,
  $0040400040000000, $0040404040000000, $0040400040000040, $0040404040000040,
  $0040000040004000, $0040004040004000, $0040000040004040, $0040004040004040,
  $0040400040004000, $0040404040004000, $0040400040004040, $0040404040004040,
  $0000000040400000, $0000004040400000, $0000000040400040, $0000004040400040,
  $0000400040400000, $0000404040400000, $0000400040400040, $0000404040400040,
  $0000000040404000, $0000004040404000, $0000000040404040, $0000004040404040,
  $0000400040404000, $0000404040404000, $0000400040404040, $0000404040404040,
  $0040000040400000, $0040004040400000, $0040000040400040, $0040004040400040,
  $0040400040400000, $0040404040400000, $0040400040400040, $0040404040400040,
  $0040000040404000, $0040004040404000, $0040000040404040, $0040004040404040,
  $0040400040404000, $0040404040404000, $0040400040404040, $0040404040404040,
  $4000000040000000, $4000004040000000, $4000000040000040, $4000004040000040,
  $4000400040000000, $4000404040000000, $4000400040000040, $4000404040000040,
  $4000000040004000, $4000004040004000, $4000000040004040, $4000004040004040,
  $4000400040004000, $4000404040004000, $4000400040004040, $4000404040004040,
  $4040000040000000, $4040004040000000, $4040000040000040, $4040004040000040,
  $4040400040000000, $4040404040000000, $4040400040000040, $4040404040000040,
  $4040000040004000, $4040004040004000, $4040000040004040, $4040004040004040,
  $4040400040004000, $4040404040004000, $4040400040004040, $4040404040004040,
  $4000000040400000, $4000004040400000, $4000000040400040, $4000004040400040,
  $4000400040400000, $4000404040400000, $4000400040400040, $4000404040400040,
  $4000000040404000, $4000004040404000, $4000000040404040, $4000004040404040,
  $4000400040404000, $4000404040404000, $4000400040404040, $4000404040404040,
  $4040000040400000, $4040004040400000, $4040000040400040, $4040004040400040,
  $4040400040400000, $4040404040400000, $4040400040400040, $4040404040400040,
  $4040000040404000, $4040004040404000, $4040000040404040, $4040004040404040,
  $4040400040404000, $4040404040404000, $4040400040404040, $4040404040404040),

( $0000000000000000, $0000002000000000, $0000000000000020, $0000002000000020,
  $0000200000000000, $0000202000000000, $0000200000000020, $0000202000000020,
  $0000000000002000, $0000002000002000, $0000000000002020, $0000002000002020,
  $0000200000002000, $0000202000002000, $0000200000002020, $0000202000002020,
  $0020000000000000, $0020002000000000, $0020000000000020, $0020002000000020,
  $0020200000000000, $0020202000000000, $0020200000000020, $0020202000000020,
  $0020000000002000, $0020002000002000, $0020000000002020, $0020002000002020,
  $0020200000002000, $0020202000002000, $0020200000002020, $0020202000002020,
  $0000000000200000, $0000002000200000, $0000000000200020, $0000002000200020,
  $0000200000200000, $0000202000200000, $0000200000200020, $0000202000200020,
  $0000000000202000, $0000002000202000, $0000000000202020, $0000002000202020,
  $0000200000202000, $0000202000202000, $0000200000202020, $0000202000202020,
  $0020000000200000, $0020002000200000, $0020000000200020, $0020002000200020,
  $0020200000200000, $0020202000200000, $0020200000200020, $0020202000200020,
  $0020000000202000, $0020002000202000, $0020000000202020, $0020002000202020,
  $0020200000202000, $0020202000202000, $0020200000202020, $0020202000202020,
  $2000000000000000, $2000002000000000, $2000000000000020, $2000002000000020,
  $2000200000000000, $2000202000000000, $2000200000000020, $2000202000000020,
  $2000000000002000, $2000002000002000, $2000000000002020, $2000002000002020,
  $2000200000002000, $2000202000002000, $2000200000002020, $2000202000002020,
  $2020000000000000, $2020002000000000, $2020000000000020, $2020002000000020,
  $2020200000000000, $2020202000000000, $2020200000000020, $2020202000000020,
  $2020000000002000, $2020002000002000, $2020000000002020, $2020002000002020,
  $2020200000002000, $2020202000002000, $2020200000002020, $2020202000002020,
  $2000000000200000, $2000002000200000, $2000000000200020, $2000002000200020,
  $2000200000200000, $2000202000200000, $2000200000200020, $2000202000200020,
  $2000000000202000, $2000002000202000, $2000000000202020, $2000002000202020,
  $2000200000202000, $2000202000202000, $2000200000202020, $2000202000202020,
  $2020000000200000, $2020002000200000, $2020000000200020, $2020002000200020,
  $2020200000200000, $2020202000200000, $2020200000200020, $2020202000200020,
  $2020000000202000, $2020002000202000, $2020000000202020, $2020002000202020,
  $2020200000202000, $2020202000202000, $2020200000202020, $2020202000202020,
  $0000000020000000, $0000002020000000, $0000000020000020, $0000002020000020,
  $0000200020000000, $0000202020000000, $0000200020000020, $0000202020000020,
  $0000000020002000, $0000002020002000, $0000000020002020, $0000002020002020,
  $0000200020002000, $0000202020002000, $0000200020002020, $0000202020002020,
  $0020000020000000, $0020002020000000, $0020000020000020, $0020002020000020,
  $0020200020000000, $0020202020000000, $0020200020000020, $0020202020000020,
  $0020000020002000, $0020002020002000, $0020000020002020, $0020002020002020,
  $0020200020002000, $0020202020002000, $0020200020002020, $0020202020002020,
  $0000000020200000, $0000002020200000, $0000000020200020, $0000002020200020,
  $0000200020200000, $0000202020200000, $0000200020200020, $0000202020200020,
  $0000000020202000, $0000002020202000, $0000000020202020, $0000002020202020,
  $0000200020202000, $0000202020202000, $0000200020202020, $0000202020202020,
  $0020000020200000, $0020002020200000, $0020000020200020, $0020002020200020,
  $0020200020200000, $0020202020200000, $0020200020200020, $0020202020200020,
  $0020000020202000, $0020002020202000, $0020000020202020, $0020002020202020,
  $0020200020202000, $0020202020202000, $0020200020202020, $0020202020202020,
  $2000000020000000, $2000002020000000, $2000000020000020, $2000002020000020,
  $2000200020000000, $2000202020000000, $2000200020000020, $2000202020000020,
  $2000000020002000, $2000002020002000, $2000000020002020, $2000002020002020,
  $2000200020002000, $2000202020002000, $2000200020002020, $2000202020002020,
  $2020000020000000, $2020002020000000, $2020000020000020, $2020002020000020,
  $2020200020000000, $2020202020000000, $2020200020000020, $2020202020000020,
  $2020000020002000, $2020002020002000, $2020000020002020, $2020002020002020,
  $2020200020002000, $2020202020002000, $2020200020002020, $2020202020002020,
  $2000000020200000, $2000002020200000, $2000000020200020, $2000002020200020,
  $2000200020200000, $2000202020200000, $2000200020200020, $2000202020200020,
  $2000000020202000, $2000002020202000, $2000000020202020, $2000002020202020,
  $2000200020202000, $2000202020202000, $2000200020202020, $2000202020202020,
  $2020000020200000, $2020002020200000, $2020000020200020, $2020002020200020,
  $2020200020200000, $2020202020200000, $2020200020200020, $2020202020200020,
  $2020000020202000, $2020002020202000, $2020000020202020, $2020002020202020,
  $2020200020202000, $2020202020202000, $2020200020202020, $2020202020202020)
  );

 DES_FP:array[0..7,0..255] of QWord = ( {Final permutation (IP-1)}
( $0000000000000000, $0000008000000000, $0000000002000000, $0000008002000000,
  $0000000000020000, $0000008000020000, $0000000002020000, $0000008002020000,
  $0000000000000200, $0000008000000200, $0000000002000200, $0000008002000200,
  $0000000000020200, $0000008000020200, $0000000002020200, $0000008002020200,
  $0000000000000002, $0000008000000002, $0000000002000002, $0000008002000002,
  $0000000000020002, $0000008000020002, $0000000002020002, $0000008002020002,
  $0000000000000202, $0000008000000202, $0000000002000202, $0000008002000202,
  $0000000000020202, $0000008000020202, $0000000002020202, $0000008002020202,
  $0200000000000000, $0200008000000000, $0200000002000000, $0200008002000000,
  $0200000000020000, $0200008000020000, $0200000002020000, $0200008002020000,
  $0200000000000200, $0200008000000200, $0200000002000200, $0200008002000200,
  $0200000000020200, $0200008000020200, $0200000002020200, $0200008002020200,
  $0200000000000002, $0200008000000002, $0200000002000002, $0200008002000002,
  $0200000000020002, $0200008000020002, $0200000002020002, $0200008002020002,
  $0200000000000202, $0200008000000202, $0200000002000202, $0200008002000202,
  $0200000000020202, $0200008000020202, $0200000002020202, $0200008002020202,
  $0002000000000000, $0002008000000000, $0002000002000000, $0002008002000000,
  $0002000000020000, $0002008000020000, $0002000002020000, $0002008002020000,
  $0002000000000200, $0002008000000200, $0002000002000200, $0002008002000200,
  $0002000000020200, $0002008000020200, $0002000002020200, $0002008002020200,
  $0002000000000002, $0002008000000002, $0002000002000002, $0002008002000002,
  $0002000000020002, $0002008000020002, $0002000002020002, $0002008002020002,
  $0002000000000202, $0002008000000202, $0002000002000202, $0002008002000202,
  $0002000000020202, $0002008000020202, $0002000002020202, $0002008002020202,
  $0202000000000000, $0202008000000000, $0202000002000000, $0202008002000000,
  $0202000000020000, $0202008000020000, $0202000002020000, $0202008002020000,
  $0202000000000200, $0202008000000200, $0202000002000200, $0202008002000200,
  $0202000000020200, $0202008000020200, $0202000002020200, $0202008002020200,
  $0202000000000002, $0202008000000002, $0202000002000002, $0202008002000002,
  $0202000000020002, $0202008000020002, $0202000002020002, $0202008002020002,
  $0202000000000202, $0202008000000202, $0202000002000202, $0202008002000202,
  $0202000000020202, $0202008000020202, $0202000002020202, $0202008002020202,
  $0000020000000000, $0000028000000000, $0000020002000000, $0000028002000000,
  $0000020000020000, $0000028000020000, $0000020002020000, $0000028002020000,
  $0000020000000200, $0000028000000200, $0000020002000200, $0000028002000200,
  $0000020000020200, $0000028000020200, $0000020002020200, $0000028002020200,
  $0000020000000002, $0000028000000002, $0000020002000002, $0000028002000002,
  $0000020000020002, $0000028000020002, $0000020002020002, $0000028002020002,
  $0000020000000202, $0000028000000202, $0000020002000202, $0000028002000202,
  $0000020000020202, $0000028000020202, $0000020002020202, $0000028002020202,
  $0200020000000000, $0200028000000000, $0200020002000000, $0200028002000000,
  $0200020000020000, $0200028000020000, $0200020002020000, $0200028002020000,
  $0200020000000200, $0200028000000200, $0200020002000200, $0200028002000200,
  $0200020000020200, $0200028000020200, $0200020002020200, $0200028002020200,
  $0200020000000002, $0200028000000002, $0200020002000002, $0200028002000002,
  $0200020000020002, $0200028000020002, $0200020002020002, $0200028002020002,
  $0200020000000202, $0200028000000202, $0200020002000202, $0200028002000202,
  $0200020000020202, $0200028000020202, $0200020002020202, $0200028002020202,
  $0002020000000000, $0002028000000000, $0002020002000000, $0002028002000000,
  $0002020000020000, $0002028000020000, $0002020002020000, $0002028002020000,
  $0002020000000200, $0002028000000200, $0002020002000200, $0002028002000200,
  $0002020000020200, $0002028000020200, $0002020002020200, $0002028002020200,
  $0002020000000002, $0002028000000002, $0002020002000002, $0002028002000002,
  $0002020000020002, $0002028000020002, $0002020002020002, $0002028002020002,
  $0002020000000202, $0002028000000202, $0002020002000202, $0002028002000202,
  $0002020000020202, $0002028000020202, $0002020002020202, $0002028002020202,
  $0202020000000000, $0202028000000000, $0202020002000000, $0202028002000000,
  $0202020000020000, $0202028000020000, $0202020002020000, $0202028002020000,
  $0202020000000200, $0202028000000200, $0202020002000200, $0202028002000200,
  $0202020000020200, $0202028000020200, $0202020002020200, $0202028002020200,
  $0202020000000002, $0202028000000002, $0202020002000002, $0202028002000002,
  $0202020000020002, $0202028000020002, $0202020002020002, $0202028002020002,
  $0202020000000202, $0202028000000202, $0202020002000202, $0202028002000202,
  $0202020000020202, $0202028000020202, $0202020002020202, $0202028002020202),

( $0000000000000000, $0000000200000000, $0000000008000000, $0000000208000000,
  $0000000000080000, $0000000200080000, $0000000008080000, $0000000208080000,
  $0000000000000800, $0000000200000800, $0000000008000800, $0000000208000800,
  $0000000000080800, $0000000200080800, $0000000008080800, $0000000208080800,
  $0000000000000008, $0000000200000008, $0000000008000008, $0000000208000008,
  $0000000000080008, $0000000200080008, $0000000008080008, $0000000208080008,
  $0000000000000808, $0000000200000808, $0000000008000808, $0000000208000808,
  $0000000000080808, $0000000200080808, $0000000008080808, $0000000208080808,
  $0800000000000000, $0800000200000000, $0800000008000000, $0800000208000000,
  $0800000000080000, $0800000200080000, $0800000008080000, $0800000208080000,
  $0800000000000800, $0800000200000800, $0800000008000800, $0800000208000800,
  $0800000000080800, $0800000200080800, $0800000008080800, $0800000208080800,
  $0800000000000008, $0800000200000008, $0800000008000008, $0800000208000008,
  $0800000000080008, $0800000200080008, $0800000008080008, $0800000208080008,
  $0800000000000808, $0800000200000808, $0800000008000808, $0800000208000808,
  $0800000000080808, $0800000200080808, $0800000008080808, $0800000208080808,
  $0008000000000000, $0008000200000000, $0008000008000000, $0008000208000000,
  $0008000000080000, $0008000200080000, $0008000008080000, $0008000208080000,
  $0008000000000800, $0008000200000800, $0008000008000800, $0008000208000800,
  $0008000000080800, $0008000200080800, $0008000008080800, $0008000208080800,
  $0008000000000008, $0008000200000008, $0008000008000008, $0008000208000008,
  $0008000000080008, $0008000200080008, $0008000008080008, $0008000208080008,
  $0008000000000808, $0008000200000808, $0008000008000808, $0008000208000808,
  $0008000000080808, $0008000200080808, $0008000008080808, $0008000208080808,
  $0808000000000000, $0808000200000000, $0808000008000000, $0808000208000000,
  $0808000000080000, $0808000200080000, $0808000008080000, $0808000208080000,
  $0808000000000800, $0808000200000800, $0808000008000800, $0808000208000800,
  $0808000000080800, $0808000200080800, $0808000008080800, $0808000208080800,
  $0808000000000008, $0808000200000008, $0808000008000008, $0808000208000008,
  $0808000000080008, $0808000200080008, $0808000008080008, $0808000208080008,
  $0808000000000808, $0808000200000808, $0808000008000808, $0808000208000808,
  $0808000000080808, $0808000200080808, $0808000008080808, $0808000208080808,
  $0000080000000000, $0000080200000000, $0000080008000000, $0000080208000000,
  $0000080000080000, $0000080200080000, $0000080008080000, $0000080208080000,
  $0000080000000800, $0000080200000800, $0000080008000800, $0000080208000800,
  $0000080000080800, $0000080200080800, $0000080008080800, $0000080208080800,
  $0000080000000008, $0000080200000008, $0000080008000008, $0000080208000008,
  $0000080000080008, $0000080200080008, $0000080008080008, $0000080208080008,
  $0000080000000808, $0000080200000808, $0000080008000808, $0000080208000808,
  $0000080000080808, $0000080200080808, $0000080008080808, $0000080208080808,
  $0800080000000000, $0800080200000000, $0800080008000000, $0800080208000000,
  $0800080000080000, $0800080200080000, $0800080008080000, $0800080208080000,
  $0800080000000800, $0800080200000800, $0800080008000800, $0800080208000800,
  $0800080000080800, $0800080200080800, $0800080008080800, $0800080208080800,
  $0800080000000008, $0800080200000008, $0800080008000008, $0800080208000008,
  $0800080000080008, $0800080200080008, $0800080008080008, $0800080208080008,
  $0800080000000808, $0800080200000808, $0800080008000808, $0800080208000808,
  $0800080000080808, $0800080200080808, $0800080008080808, $0800080208080808,
  $0008080000000000, $0008080200000000, $0008080008000000, $0008080208000000,
  $0008080000080000, $0008080200080000, $0008080008080000, $0008080208080000,
  $0008080000000800, $0008080200000800, $0008080008000800, $0008080208000800,
  $0008080000080800, $0008080200080800, $0008080008080800, $0008080208080800,
  $0008080000000008, $0008080200000008, $0008080008000008, $0008080208000008,
  $0008080000080008, $0008080200080008, $0008080008080008, $0008080208080008,
  $0008080000000808, $0008080200000808, $0008080008000808, $0008080208000808,
  $0008080000080808, $0008080200080808, $0008080008080808, $0008080208080808,
  $0808080000000000, $0808080200000000, $0808080008000000, $0808080208000000,
  $0808080000080000, $0808080200080000, $0808080008080000, $0808080208080000,
  $0808080000000800, $0808080200000800, $0808080008000800, $0808080208000800,
  $0808080000080800, $0808080200080800, $0808080008080800, $0808080208080800,
  $0808080000000008, $0808080200000008, $0808080008000008, $0808080208000008,
  $0808080000080008, $0808080200080008, $0808080008080008, $0808080208080008,
  $0808080000000808, $0808080200000808, $0808080008000808, $0808080208000808,
  $0808080000080808, $0808080200080808, $0808080008080808, $0808080208080808),

( $0000000000000000, $0000000800000000, $0000000020000000, $0000000820000000,
  $0000000000200000, $0000000800200000, $0000000020200000, $0000000820200000,
  $0000000000002000, $0000000800002000, $0000000020002000, $0000000820002000,
  $0000000000202000, $0000000800202000, $0000000020202000, $0000000820202000,
  $0000000000000020, $0000000800000020, $0000000020000020, $0000000820000020,
  $0000000000200020, $0000000800200020, $0000000020200020, $0000000820200020,
  $0000000000002020, $0000000800002020, $0000000020002020, $0000000820002020,
  $0000000000202020, $0000000800202020, $0000000020202020, $0000000820202020,
  $2000000000000000, $2000000800000000, $2000000020000000, $2000000820000000,
  $2000000000200000, $2000000800200000, $2000000020200000, $2000000820200000,
  $2000000000002000, $2000000800002000, $2000000020002000, $2000000820002000,
  $2000000000202000, $2000000800202000, $2000000020202000, $2000000820202000,
  $2000000000000020, $2000000800000020, $2000000020000020, $2000000820000020,
  $2000000000200020, $2000000800200020, $2000000020200020, $2000000820200020,
  $2000000000002020, $2000000800002020, $2000000020002020, $2000000820002020,
  $2000000000202020, $2000000800202020, $2000000020202020, $2000000820202020,
  $0020000000000000, $0020000800000000, $0020000020000000, $0020000820000000,
  $0020000000200000, $0020000800200000, $0020000020200000, $0020000820200000,
  $0020000000002000, $0020000800002000, $0020000020002000, $0020000820002000,
  $0020000000202000, $0020000800202000, $0020000020202000, $0020000820202000,
  $0020000000000020, $0020000800000020, $0020000020000020, $0020000820000020,
  $0020000000200020, $0020000800200020, $0020000020200020, $0020000820200020,
  $0020000000002020, $0020000800002020, $0020000020002020, $0020000820002020,
  $0020000000202020, $0020000800202020, $0020000020202020, $0020000820202020,
  $2020000000000000, $2020000800000000, $2020000020000000, $2020000820000000,
  $2020000000200000, $2020000800200000, $2020000020200000, $2020000820200000,
  $2020000000002000, $2020000800002000, $2020000020002000, $2020000820002000,
  $2020000000202000, $2020000800202000, $2020000020202000, $2020000820202000,
  $2020000000000020, $2020000800000020, $2020000020000020, $2020000820000020,
  $2020000000200020, $2020000800200020, $2020000020200020, $2020000820200020,
  $2020000000002020, $2020000800002020, $2020000020002020, $2020000820002020,
  $2020000000202020, $2020000800202020, $2020000020202020, $2020000820202020,
  $0000200000000000, $0000200800000000, $0000200020000000, $0000200820000000,
  $0000200000200000, $0000200800200000, $0000200020200000, $0000200820200000,
  $0000200000002000, $0000200800002000, $0000200020002000, $0000200820002000,
  $0000200000202000, $0000200800202000, $0000200020202000, $0000200820202000,
  $0000200000000020, $0000200800000020, $0000200020000020, $0000200820000020,
  $0000200000200020, $0000200800200020, $0000200020200020, $0000200820200020,
  $0000200000002020, $0000200800002020, $0000200020002020, $0000200820002020,
  $0000200000202020, $0000200800202020, $0000200020202020, $0000200820202020,
  $2000200000000000, $2000200800000000, $2000200020000000, $2000200820000000,
  $2000200000200000, $2000200800200000, $2000200020200000, $2000200820200000,
  $2000200000002000, $2000200800002000, $2000200020002000, $2000200820002000,
  $2000200000202000, $2000200800202000, $2000200020202000, $2000200820202000,
  $2000200000000020, $2000200800000020, $2000200020000020, $2000200820000020,
  $2000200000200020, $2000200800200020, $2000200020200020, $2000200820200020,
  $2000200000002020, $2000200800002020, $2000200020002020, $2000200820002020,
  $2000200000202020, $2000200800202020, $2000200020202020, $2000200820202020,
  $0020200000000000, $0020200800000000, $0020200020000000, $0020200820000000,
  $0020200000200000, $0020200800200000, $0020200020200000, $0020200820200000,
  $0020200000002000, $0020200800002000, $0020200020002000, $0020200820002000,
  $0020200000202000, $0020200800202000, $0020200020202000, $0020200820202000,
  $0020200000000020, $0020200800000020, $0020200020000020, $0020200820000020,
  $0020200000200020, $0020200800200020, $0020200020200020, $0020200820200020,
  $0020200000002020, $0020200800002020, $0020200020002020, $0020200820002020,
  $0020200000202020, $0020200800202020, $0020200020202020, $0020200820202020,
  $2020200000000000, $2020200800000000, $2020200020000000, $2020200820000000,
  $2020200000200000, $2020200800200000, $2020200020200000, $2020200820200000,
  $2020200000002000, $2020200800002000, $2020200020002000, $2020200820002000,
  $2020200000202000, $2020200800202000, $2020200020202000, $2020200820202000,
  $2020200000000020, $2020200800000020, $2020200020000020, $2020200820000020,
  $2020200000200020, $2020200800200020, $2020200020200020, $2020200820200020,
  $2020200000002020, $2020200800002020, $2020200020002020, $2020200820002020,
  $2020200000202020, $2020200800202020, $2020200020202020, $2020200820202020),

( $0000000000000000, $0000002000000000, $0000000080000000, $0000002080000000,
  $0000000000800000, $0000002000800000, $0000000080800000, $0000002080800000,
  $0000000000008000, $0000002000008000, $0000000080008000, $0000002080008000,
  $0000000000808000, $0000002000808000, $0000000080808000, $0000002080808000,
  $0000000000000080, $0000002000000080, $0000000080000080, $0000002080000080,
  $0000000000800080, $0000002000800080, $0000000080800080, $0000002080800080,
  $0000000000008080, $0000002000008080, $0000000080008080, $0000002080008080,
  $0000000000808080, $0000002000808080, $0000000080808080, $0000002080808080,
  $8000000000000000, $8000002000000000, $8000000080000000, $8000002080000000,
  $8000000000800000, $8000002000800000, $8000000080800000, $8000002080800000,
  $8000000000008000, $8000002000008000, $8000000080008000, $8000002080008000,
  $8000000000808000, $8000002000808000, $8000000080808000, $8000002080808000,
  $8000000000000080, $8000002000000080, $8000000080000080, $8000002080000080,
  $8000000000800080, $8000002000800080, $8000000080800080, $8000002080800080,
  $8000000000008080, $8000002000008080, $8000000080008080, $8000002080008080,
  $8000000000808080, $8000002000808080, $8000000080808080, $8000002080808080,
  $0080000000000000, $0080002000000000, $0080000080000000, $0080002080000000,
  $0080000000800000, $0080002000800000, $0080000080800000, $0080002080800000,
  $0080000000008000, $0080002000008000, $0080000080008000, $0080002080008000,
  $0080000000808000, $0080002000808000, $0080000080808000, $0080002080808000,
  $0080000000000080, $0080002000000080, $0080000080000080, $0080002080000080,
  $0080000000800080, $0080002000800080, $0080000080800080, $0080002080800080,
  $0080000000008080, $0080002000008080, $0080000080008080, $0080002080008080,
  $0080000000808080, $0080002000808080, $0080000080808080, $0080002080808080,
  $8080000000000000, $8080002000000000, $8080000080000000, $8080002080000000,
  $8080000000800000, $8080002000800000, $8080000080800000, $8080002080800000,
  $8080000000008000, $8080002000008000, $8080000080008000, $8080002080008000,
  $8080000000808000, $8080002000808000, $8080000080808000, $8080002080808000,
  $8080000000000080, $8080002000000080, $8080000080000080, $8080002080000080,
  $8080000000800080, $8080002000800080, $8080000080800080, $8080002080800080,
  $8080000000008080, $8080002000008080, $8080000080008080, $8080002080008080,
  $8080000000808080, $8080002000808080, $8080000080808080, $8080002080808080,
  $0000800000000000, $0000802000000000, $0000800080000000, $0000802080000000,
  $0000800000800000, $0000802000800000, $0000800080800000, $0000802080800000,
  $0000800000008000, $0000802000008000, $0000800080008000, $0000802080008000,
  $0000800000808000, $0000802000808000, $0000800080808000, $0000802080808000,
  $0000800000000080, $0000802000000080, $0000800080000080, $0000802080000080,
  $0000800000800080, $0000802000800080, $0000800080800080, $0000802080800080,
  $0000800000008080, $0000802000008080, $0000800080008080, $0000802080008080,
  $0000800000808080, $0000802000808080, $0000800080808080, $0000802080808080,
  $8000800000000000, $8000802000000000, $8000800080000000, $8000802080000000,
  $8000800000800000, $8000802000800000, $8000800080800000, $8000802080800000,
  $8000800000008000, $8000802000008000, $8000800080008000, $8000802080008000,
  $8000800000808000, $8000802000808000, $8000800080808000, $8000802080808000,
  $8000800000000080, $8000802000000080, $8000800080000080, $8000802080000080,
  $8000800000800080, $8000802000800080, $8000800080800080, $8000802080800080,
  $8000800000008080, $8000802000008080, $8000800080008080, $8000802080008080,
  $8000800000808080, $8000802000808080, $8000800080808080, $8000802080808080,
  $0080800000000000, $0080802000000000, $0080800080000000, $0080802080000000,
  $0080800000800000, $0080802000800000, $0080800080800000, $0080802080800000,
  $0080800000008000, $0080802000008000, $0080800080008000, $0080802080008000,
  $0080800000808000, $0080802000808000, $0080800080808000, $0080802080808000,
  $0080800000000080, $0080802000000080, $0080800080000080, $0080802080000080,
  $0080800000800080, $0080802000800080, $0080800080800080, $0080802080800080,
  $0080800000008080, $0080802000008080, $0080800080008080, $0080802080008080,
  $0080800000808080, $0080802000808080, $0080800080808080, $0080802080808080,
  $8080800000000000, $8080802000000000, $8080800080000000, $8080802080000000,
  $8080800000800000, $8080802000800000, $8080800080800000, $8080802080800000,
  $8080800000008000, $8080802000008000, $8080800080008000, $8080802080008000,
  $8080800000808000, $8080802000808000, $8080800080808000, $8080802080808000,
  $8080800000000080, $8080802000000080, $8080800080000080, $8080802080000080,
  $8080800000800080, $8080802000800080, $8080800080800080, $8080802080800080,
  $8080800000008080, $8080802000008080, $8080800080008080, $8080802080008080,
  $8080800000808080, $8080802000808080, $8080800080808080, $8080802080808080),

( $0000000000000000, $0000004000000000, $0000000001000000, $0000004001000000,
  $0000000000010000, $0000004000010000, $0000000001010000, $0000004001010000,
  $0000000000000100, $0000004000000100, $0000000001000100, $0000004001000100,
  $0000000000010100, $0000004000010100, $0000000001010100, $0000004001010100,
  $0000000000000001, $0000004000000001, $0000000001000001, $0000004001000001,
  $0000000000010001, $0000004000010001, $0000000001010001, $0000004001010001,
  $0000000000000101, $0000004000000101, $0000000001000101, $0000004001000101,
  $0000000000010101, $0000004000010101, $0000000001010101, $0000004001010101,
  $0100000000000000, $0100004000000000, $0100000001000000, $0100004001000000,
  $0100000000010000, $0100004000010000, $0100000001010000, $0100004001010000,
  $0100000000000100, $0100004000000100, $0100000001000100, $0100004001000100,
  $0100000000010100, $0100004000010100, $0100000001010100, $0100004001010100,
  $0100000000000001, $0100004000000001, $0100000001000001, $0100004001000001,
  $0100000000010001, $0100004000010001, $0100000001010001, $0100004001010001,
  $0100000000000101, $0100004000000101, $0100000001000101, $0100004001000101,
  $0100000000010101, $0100004000010101, $0100000001010101, $0100004001010101,
  $0001000000000000, $0001004000000000, $0001000001000000, $0001004001000000,
  $0001000000010000, $0001004000010000, $0001000001010000, $0001004001010000,
  $0001000000000100, $0001004000000100, $0001000001000100, $0001004001000100,
  $0001000000010100, $0001004000010100, $0001000001010100, $0001004001010100,
  $0001000000000001, $0001004000000001, $0001000001000001, $0001004001000001,
  $0001000000010001, $0001004000010001, $0001000001010001, $0001004001010001,
  $0001000000000101, $0001004000000101, $0001000001000101, $0001004001000101,
  $0001000000010101, $0001004000010101, $0001000001010101, $0001004001010101,
  $0101000000000000, $0101004000000000, $0101000001000000, $0101004001000000,
  $0101000000010000, $0101004000010000, $0101000001010000, $0101004001010000,
  $0101000000000100, $0101004000000100, $0101000001000100, $0101004001000100,
  $0101000000010100, $0101004000010100, $0101000001010100, $0101004001010100,
  $0101000000000001, $0101004000000001, $0101000001000001, $0101004001000001,
  $0101000000010001, $0101004000010001, $0101000001010001, $0101004001010001,
  $0101000000000101, $0101004000000101, $0101000001000101, $0101004001000101,
  $0101000000010101, $0101004000010101, $0101000001010101, $0101004001010101,
  $0000010000000000, $0000014000000000, $0000010001000000, $0000014001000000,
  $0000010000010000, $0000014000010000, $0000010001010000, $0000014001010000,
  $0000010000000100, $0000014000000100, $0000010001000100, $0000014001000100,
  $0000010000010100, $0000014000010100, $0000010001010100, $0000014001010100,
  $0000010000000001, $0000014000000001, $0000010001000001, $0000014001000001,
  $0000010000010001, $0000014000010001, $0000010001010001, $0000014001010001,
  $0000010000000101, $0000014000000101, $0000010001000101, $0000014001000101,
  $0000010000010101, $0000014000010101, $0000010001010101, $0000014001010101,
  $0100010000000000, $0100014000000000, $0100010001000000, $0100014001000000,
  $0100010000010000, $0100014000010000, $0100010001010000, $0100014001010000,
  $0100010000000100, $0100014000000100, $0100010001000100, $0100014001000100,
  $0100010000010100, $0100014000010100, $0100010001010100, $0100014001010100,
  $0100010000000001, $0100014000000001, $0100010001000001, $0100014001000001,
  $0100010000010001, $0100014000010001, $0100010001010001, $0100014001010001,
  $0100010000000101, $0100014000000101, $0100010001000101, $0100014001000101,
  $0100010000010101, $0100014000010101, $0100010001010101, $0100014001010101,
  $0001010000000000, $0001014000000000, $0001010001000000, $0001014001000000,
  $0001010000010000, $0001014000010000, $0001010001010000, $0001014001010000,
  $0001010000000100, $0001014000000100, $0001010001000100, $0001014001000100,
  $0001010000010100, $0001014000010100, $0001010001010100, $0001014001010100,
  $0001010000000001, $0001014000000001, $0001010001000001, $0001014001000001,
  $0001010000010001, $0001014000010001, $0001010001010001, $0001014001010001,
  $0001010000000101, $0001014000000101, $0001010001000101, $0001014001000101,
  $0001010000010101, $0001014000010101, $0001010001010101, $0001014001010101,
  $0101010000000000, $0101014000000000, $0101010001000000, $0101014001000000,
  $0101010000010000, $0101014000010000, $0101010001010000, $0101014001010000,
  $0101010000000100, $0101014000000100, $0101010001000100, $0101014001000100,
  $0101010000010100, $0101014000010100, $0101010001010100, $0101014001010100,
  $0101010000000001, $0101014000000001, $0101010001000001, $0101014001000001,
  $0101010000010001, $0101014000010001, $0101010001010001, $0101014001010001,
  $0101010000000101, $0101014000000101, $0101010001000101, $0101014001000101,
  $0101010000010101, $0101014000010101, $0101010001010101, $0101014001010101),

( $0000000000000000, $0000000100000000, $0000000004000000, $0000000104000000,
  $0000000000040000, $0000000100040000, $0000000004040000, $0000000104040000,
  $0000000000000400, $0000000100000400, $0000000004000400, $0000000104000400,
  $0000000000040400, $0000000100040400, $0000000004040400, $0000000104040400,
  $0000000000000004, $0000000100000004, $0000000004000004, $0000000104000004,
  $0000000000040004, $0000000100040004, $0000000004040004, $0000000104040004,
  $0000000000000404, $0000000100000404, $0000000004000404, $0000000104000404,
  $0000000000040404, $0000000100040404, $0000000004040404, $0000000104040404,
  $0400000000000000, $0400000100000000, $0400000004000000, $0400000104000000,
  $0400000000040000, $0400000100040000, $0400000004040000, $0400000104040000,
  $0400000000000400, $0400000100000400, $0400000004000400, $0400000104000400,
  $0400000000040400, $0400000100040400, $0400000004040400, $0400000104040400,
  $0400000000000004, $0400000100000004, $0400000004000004, $0400000104000004,
  $0400000000040004, $0400000100040004, $0400000004040004, $0400000104040004,
  $0400000000000404, $0400000100000404, $0400000004000404, $0400000104000404,
  $0400000000040404, $0400000100040404, $0400000004040404, $0400000104040404,
  $0004000000000000, $0004000100000000, $0004000004000000, $0004000104000000,
  $0004000000040000, $0004000100040000, $0004000004040000, $0004000104040000,
  $0004000000000400, $0004000100000400, $0004000004000400, $0004000104000400,
  $0004000000040400, $0004000100040400, $0004000004040400, $0004000104040400,
  $0004000000000004, $0004000100000004, $0004000004000004, $0004000104000004,
  $0004000000040004, $0004000100040004, $0004000004040004, $0004000104040004,
  $0004000000000404, $0004000100000404, $0004000004000404, $0004000104000404,
  $0004000000040404, $0004000100040404, $0004000004040404, $0004000104040404,
  $0404000000000000, $0404000100000000, $0404000004000000, $0404000104000000,
  $0404000000040000, $0404000100040000, $0404000004040000, $0404000104040000,
  $0404000000000400, $0404000100000400, $0404000004000400, $0404000104000400,
  $0404000000040400, $0404000100040400, $0404000004040400, $0404000104040400,
  $0404000000000004, $0404000100000004, $0404000004000004, $0404000104000004,
  $0404000000040004, $0404000100040004, $0404000004040004, $0404000104040004,
  $0404000000000404, $0404000100000404, $0404000004000404, $0404000104000404,
  $0404000000040404, $0404000100040404, $0404000004040404, $0404000104040404,
  $0000040000000000, $0000040100000000, $0000040004000000, $0000040104000000,
  $0000040000040000, $0000040100040000, $0000040004040000, $0000040104040000,
  $0000040000000400, $0000040100000400, $0000040004000400, $0000040104000400,
  $0000040000040400, $0000040100040400, $0000040004040400, $0000040104040400,
  $0000040000000004, $0000040100000004, $0000040004000004, $0000040104000004,
  $0000040000040004, $0000040100040004, $0000040004040004, $0000040104040004,
  $0000040000000404, $0000040100000404, $0000040004000404, $0000040104000404,
  $0000040000040404, $0000040100040404, $0000040004040404, $0000040104040404,
  $0400040000000000, $0400040100000000, $0400040004000000, $0400040104000000,
  $0400040000040000, $0400040100040000, $0400040004040000, $0400040104040000,
  $0400040000000400, $0400040100000400, $0400040004000400, $0400040104000400,
  $0400040000040400, $0400040100040400, $0400040004040400, $0400040104040400,
  $0400040000000004, $0400040100000004, $0400040004000004, $0400040104000004,
  $0400040000040004, $0400040100040004, $0400040004040004, $0400040104040004,
  $0400040000000404, $0400040100000404, $0400040004000404, $0400040104000404,
  $0400040000040404, $0400040100040404, $0400040004040404, $0400040104040404,
  $0004040000000000, $0004040100000000, $0004040004000000, $0004040104000000,
  $0004040000040000, $0004040100040000, $0004040004040000, $0004040104040000,
  $0004040000000400, $0004040100000400, $0004040004000400, $0004040104000400,
  $0004040000040400, $0004040100040400, $0004040004040400, $0004040104040400,
  $0004040000000004, $0004040100000004, $0004040004000004, $0004040104000004,
  $0004040000040004, $0004040100040004, $0004040004040004, $0004040104040004,
  $0004040000000404, $0004040100000404, $0004040004000404, $0004040104000404,
  $0004040000040404, $0004040100040404, $0004040004040404, $0004040104040404,
  $0404040000000000, $0404040100000000, $0404040004000000, $0404040104000000,
  $0404040000040000, $0404040100040000, $0404040004040000, $0404040104040000,
  $0404040000000400, $0404040100000400, $0404040004000400, $0404040104000400,
  $0404040000040400, $0404040100040400, $0404040004040400, $0404040104040400,
  $0404040000000004, $0404040100000004, $0404040004000004, $0404040104000004,
  $0404040000040004, $0404040100040004, $0404040004040004, $0404040104040004,
  $0404040000000404, $0404040100000404, $0404040004000404, $0404040104000404,
  $0404040000040404, $0404040100040404, $0404040004040404, $0404040104040404),

( $0000000000000000, $0000000400000000, $0000000010000000, $0000000410000000,
  $0000000000100000, $0000000400100000, $0000000010100000, $0000000410100000,
  $0000000000001000, $0000000400001000, $0000000010001000, $0000000410001000,
  $0000000000101000, $0000000400101000, $0000000010101000, $0000000410101000,
  $0000000000000010, $0000000400000010, $0000000010000010, $0000000410000010,
  $0000000000100010, $0000000400100010, $0000000010100010, $0000000410100010,
  $0000000000001010, $0000000400001010, $0000000010001010, $0000000410001010,
  $0000000000101010, $0000000400101010, $0000000010101010, $0000000410101010,
  $1000000000000000, $1000000400000000, $1000000010000000, $1000000410000000,
  $1000000000100000, $1000000400100000, $1000000010100000, $1000000410100000,
  $1000000000001000, $1000000400001000, $1000000010001000, $1000000410001000,
  $1000000000101000, $1000000400101000, $1000000010101000, $1000000410101000,
  $1000000000000010, $1000000400000010, $1000000010000010, $1000000410000010,
  $1000000000100010, $1000000400100010, $1000000010100010, $1000000410100010,
  $1000000000001010, $1000000400001010, $1000000010001010, $1000000410001010,
  $1000000000101010, $1000000400101010, $1000000010101010, $1000000410101010,
  $0010000000000000, $0010000400000000, $0010000010000000, $0010000410000000,
  $0010000000100000, $0010000400100000, $0010000010100000, $0010000410100000,
  $0010000000001000, $0010000400001000, $0010000010001000, $0010000410001000,
  $0010000000101000, $0010000400101000, $0010000010101000, $0010000410101000,
  $0010000000000010, $0010000400000010, $0010000010000010, $0010000410000010,
  $0010000000100010, $0010000400100010, $0010000010100010, $0010000410100010,
  $0010000000001010, $0010000400001010, $0010000010001010, $0010000410001010,
  $0010000000101010, $0010000400101010, $0010000010101010, $0010000410101010,
  $1010000000000000, $1010000400000000, $1010000010000000, $1010000410000000,
  $1010000000100000, $1010000400100000, $1010000010100000, $1010000410100000,
  $1010000000001000, $1010000400001000, $1010000010001000, $1010000410001000,
  $1010000000101000, $1010000400101000, $1010000010101000, $1010000410101000,
  $1010000000000010, $1010000400000010, $1010000010000010, $1010000410000010,
  $1010000000100010, $1010000400100010, $1010000010100010, $1010000410100010,
  $1010000000001010, $1010000400001010, $1010000010001010, $1010000410001010,
  $1010000000101010, $1010000400101010, $1010000010101010, $1010000410101010,
  $0000100000000000, $0000100400000000, $0000100010000000, $0000100410000000,
  $0000100000100000, $0000100400100000, $0000100010100000, $0000100410100000,
  $0000100000001000, $0000100400001000, $0000100010001000, $0000100410001000,
  $0000100000101000, $0000100400101000, $0000100010101000, $0000100410101000,
  $0000100000000010, $0000100400000010, $0000100010000010, $0000100410000010,
  $0000100000100010, $0000100400100010, $0000100010100010, $0000100410100010,
  $0000100000001010, $0000100400001010, $0000100010001010, $0000100410001010,
  $0000100000101010, $0000100400101010, $0000100010101010, $0000100410101010,
  $1000100000000000, $1000100400000000, $1000100010000000, $1000100410000000,
  $1000100000100000, $1000100400100000, $1000100010100000, $1000100410100000,
  $1000100000001000, $1000100400001000, $1000100010001000, $1000100410001000,
  $1000100000101000, $1000100400101000, $1000100010101000, $1000100410101000,
  $1000100000000010, $1000100400000010, $1000100010000010, $1000100410000010,
  $1000100000100010, $1000100400100010, $1000100010100010, $1000100410100010,
  $1000100000001010, $1000100400001010, $1000100010001010, $1000100410001010,
  $1000100000101010, $1000100400101010, $1000100010101010, $1000100410101010,
  $0010100000000000, $0010100400000000, $0010100010000000, $0010100410000000,
  $0010100000100000, $0010100400100000, $0010100010100000, $0010100410100000,
  $0010100000001000, $0010100400001000, $0010100010001000, $0010100410001000,
  $0010100000101000, $0010100400101000, $0010100010101000, $0010100410101000,
  $0010100000000010, $0010100400000010, $0010100010000010, $0010100410000010,
  $0010100000100010, $0010100400100010, $0010100010100010, $0010100410100010,
  $0010100000001010, $0010100400001010, $0010100010001010, $0010100410001010,
  $0010100000101010, $0010100400101010, $0010100010101010, $0010100410101010,
  $1010100000000000, $1010100400000000, $1010100010000000, $1010100410000000,
  $1010100000100000, $1010100400100000, $1010100010100000, $1010100410100000,
  $1010100000001000, $1010100400001000, $1010100010001000, $1010100410001000,
  $1010100000101000, $1010100400101000, $1010100010101000, $1010100410101000,
  $1010100000000010, $1010100400000010, $1010100010000010, $1010100410000010,
  $1010100000100010, $1010100400100010, $1010100010100010, $1010100410100010,
  $1010100000001010, $1010100400001010, $1010100010001010, $1010100410001010,
  $1010100000101010, $1010100400101010, $1010100010101010, $1010100410101010),

( $0000000000000000, $0000001000000000, $0000000040000000, $0000001040000000,
  $0000000000400000, $0000001000400000, $0000000040400000, $0000001040400000,
  $0000000000004000, $0000001000004000, $0000000040004000, $0000001040004000,
  $0000000000404000, $0000001000404000, $0000000040404000, $0000001040404000,
  $0000000000000040, $0000001000000040, $0000000040000040, $0000001040000040,
  $0000000000400040, $0000001000400040, $0000000040400040, $0000001040400040,
  $0000000000004040, $0000001000004040, $0000000040004040, $0000001040004040,
  $0000000000404040, $0000001000404040, $0000000040404040, $0000001040404040,
  $4000000000000000, $4000001000000000, $4000000040000000, $4000001040000000,
  $4000000000400000, $4000001000400000, $4000000040400000, $4000001040400000,
  $4000000000004000, $4000001000004000, $4000000040004000, $4000001040004000,
  $4000000000404000, $4000001000404000, $4000000040404000, $4000001040404000,
  $4000000000000040, $4000001000000040, $4000000040000040, $4000001040000040,
  $4000000000400040, $4000001000400040, $4000000040400040, $4000001040400040,
  $4000000000004040, $4000001000004040, $4000000040004040, $4000001040004040,
  $4000000000404040, $4000001000404040, $4000000040404040, $4000001040404040,
  $0040000000000000, $0040001000000000, $0040000040000000, $0040001040000000,
  $0040000000400000, $0040001000400000, $0040000040400000, $0040001040400000,
  $0040000000004000, $0040001000004000, $0040000040004000, $0040001040004000,
  $0040000000404000, $0040001000404000, $0040000040404000, $0040001040404000,
  $0040000000000040, $0040001000000040, $0040000040000040, $0040001040000040,
  $0040000000400040, $0040001000400040, $0040000040400040, $0040001040400040,
  $0040000000004040, $0040001000004040, $0040000040004040, $0040001040004040,
  $0040000000404040, $0040001000404040, $0040000040404040, $0040001040404040,
  $4040000000000000, $4040001000000000, $4040000040000000, $4040001040000000,
  $4040000000400000, $4040001000400000, $4040000040400000, $4040001040400000,
  $4040000000004000, $4040001000004000, $4040000040004000, $4040001040004000,
  $4040000000404000, $4040001000404000, $4040000040404000, $4040001040404000,
  $4040000000000040, $4040001000000040, $4040000040000040, $4040001040000040,
  $4040000000400040, $4040001000400040, $4040000040400040, $4040001040400040,
  $4040000000004040, $4040001000004040, $4040000040004040, $4040001040004040,
  $4040000000404040, $4040001000404040, $4040000040404040, $4040001040404040,
  $0000400000000000, $0000401000000000, $0000400040000000, $0000401040000000,
  $0000400000400000, $0000401000400000, $0000400040400000, $0000401040400000,
  $0000400000004000, $0000401000004000, $0000400040004000, $0000401040004000,
  $0000400000404000, $0000401000404000, $0000400040404000, $0000401040404000,
  $0000400000000040, $0000401000000040, $0000400040000040, $0000401040000040,
  $0000400000400040, $0000401000400040, $0000400040400040, $0000401040400040,
  $0000400000004040, $0000401000004040, $0000400040004040, $0000401040004040,
  $0000400000404040, $0000401000404040, $0000400040404040, $0000401040404040,
  $4000400000000000, $4000401000000000, $4000400040000000, $4000401040000000,
  $4000400000400000, $4000401000400000, $4000400040400000, $4000401040400000,
  $4000400000004000, $4000401000004000, $4000400040004000, $4000401040004000,
  $4000400000404000, $4000401000404000, $4000400040404000, $4000401040404000,
  $4000400000000040, $4000401000000040, $4000400040000040, $4000401040000040,
  $4000400000400040, $4000401000400040, $4000400040400040, $4000401040400040,
  $4000400000004040, $4000401000004040, $4000400040004040, $4000401040004040,
  $4000400000404040, $4000401000404040, $4000400040404040, $4000401040404040,
  $0040400000000000, $0040401000000000, $0040400040000000, $0040401040000000,
  $0040400000400000, $0040401000400000, $0040400040400000, $0040401040400000,
  $0040400000004000, $0040401000004000, $0040400040004000, $0040401040004000,
  $0040400000404000, $0040401000404000, $0040400040404000, $0040401040404000,
  $0040400000000040, $0040401000000040, $0040400040000040, $0040401040000040,
  $0040400000400040, $0040401000400040, $0040400040400040, $0040401040400040,
  $0040400000004040, $0040401000004040, $0040400040004040, $0040401040004040,
  $0040400000404040, $0040401000404040, $0040400040404040, $0040401040404040,
  $4040400000000000, $4040401000000000, $4040400040000000, $4040401040000000,
  $4040400000400000, $4040401000400000, $4040400040400000, $4040401040400000,
  $4040400000004000, $4040401000004000, $4040400040004000, $4040401040004000,
  $4040400000404000, $4040401000404000, $4040400040404000, $4040401040404000,
  $4040400000000040, $4040401000000040, $4040400040000040, $4040401040000040,
  $4040400000400040, $4040401000400040, $4040400040400040, $4040401040400040,
  $4040400000004040, $4040401000004040, $4040400040004040, $4040401040004040,
  $4040400000404040, $4040401000404040, $4040400040404040, $4040401040404040)
  );
 {$ENDIF}

 {3DES constants}
 DES3_BLOCK_SIZE = 8;  {64 bit blocks}

 DES3_KEY_SIZE = 24; {192 bit (3 x 64 bit) keys}

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

 {SHA512 constants}
 SHA512_K:array[0..79] of QWord = (
  $428a2f98d728ae22, $7137449123ef65cd,
  $b5c0fbcfec4d3b2f, $e9b5dba58189dbbc,
  $3956c25bf348b538, $59f111f1b605d019,
  $923f82a4af194f9b, $ab1c5ed5da6d8118,
  $d807aa98a3030242, $12835b0145706fbe,
  $243185be4ee4b28c, $550c7dc3d5ffb4e2,
  $72be5d74f27b896f, $80deb1fe3b1696b1,
  $9bdc06a725c71235, $c19bf174cf692694,
  $e49b69c19ef14ad2, $efbe4786384f25e3,
  $0fc19dc68b8cd5b5, $240ca1cc77ac9c65,
  $2de92c6f592b0275, $4a7484aa6ea6e483,
  $5cb0a9dcbd41fbd4, $76f988da831153b5,
  $983e5152ee66dfab, $a831c66d2db43210,
  $b00327c898fb213f, $bf597fc7beef0ee4,
  $c6e00bf33da88fc2, $d5a79147930aa725,
  $06ca6351e003826f, $142929670a0e6e70,
  $27b70a8546d22ffc, $2e1b21385c26c926,
  $4d2c6dfc5ac42aed, $53380d139d95b3df,
  $650a73548baf63de, $766a0abb3c77b2a8,
  $81c2c92e47edaee6, $92722c851482353b,
  $a2bfe8a14cf10364, $a81a664bbc423001,
  $c24b8b70d0f89791, $c76c51a30654be30,
  $d192e819d6ef5218, $d69906245565a910,
  $f40e35855771202a, $106aa07032bbd1b8,
  $19a4c116b8d2d0c8, $1e376c085141ab53,
  $2748774cdf8eeb99, $34b0bcb5e19b48a8,
  $391c0cb3c5c95a63, $4ed8aa4ae3418acb,
  $5b9cca4f7763e373, $682e6ff3d6b2b8a3,
  $748f82ee5defb2fc, $78a5636f43172f60,
  $84c87814a1f0ab72, $8cc702081a6439ec,
  $90befffa23631e28, $a4506cebde82bde9,
  $bef9a3f7b2c67915, $c67178f2e372532b,
  $ca273eceea26619c, $d186b8c721c0c207,
  $eada7dd6cde0eb1e, $f57d4f7fee6ed178,
  $06f067aa72176fba, $0a637dc5a2c898a6,
  $113f9804bef90dae, $1b710b35131c471b,
  $28db77f523047d84, $32caab7b40c72493,
  $3c9ebe0a15c9bebc, $431d67c49c100d4c,
  $4cc5d4becb3e42b6, $597f299cfc657e2a,
  $5fcb6fab3ad6faec, $6c44198c4a475817);
  {This is the precalculated K array for the SHA512 algorithm}

 {RSA constants}
 RSA_MODULUS_BYTES_MAX = 512; {4096 bit maximum}

 {Random constants}

 {CRC constants}
 CRC16_CCITT:array[0..255] of Word = (
  $0000, $1189, $2312, $329b, $4624, $57ad, $6536, $74bf,
  $8c48, $9dc1, $af5a, $bed3, $ca6c, $dbe5, $e97e, $f8f7,
  $1081, $0108, $3393, $221a, $56a5, $472c, $75b7, $643e,
  $9cc9, $8d40, $bfdb, $ae52, $daed, $cb64, $f9ff, $e876,
  $2102, $308b, $0210, $1399, $6726, $76af, $4434, $55bd,
  $ad4a, $bcc3, $8e58, $9fd1, $eb6e, $fae7, $c87c, $d9f5,
  $3183, $200a, $1291, $0318, $77a7, $662e, $54b5, $453c,
  $bdcb, $ac42, $9ed9, $8f50, $fbef, $ea66, $d8fd, $c974,
  $4204, $538d, $6116, $709f, $0420, $15a9, $2732, $36bb,
  $ce4c, $dfc5, $ed5e, $fcd7, $8868, $99e1, $ab7a, $baf3,
  $5285, $430c, $7197, $601e, $14a1, $0528, $37b3, $263a,
  $decd, $cf44, $fddf, $ec56, $98e9, $8960, $bbfb, $aa72,
  $6306, $728f, $4014, $519d, $2522, $34ab, $0630, $17b9,
  $ef4e, $fec7, $cc5c, $ddd5, $a96a, $b8e3, $8a78, $9bf1,
  $7387, $620e, $5095, $411c, $35a3, $242a, $16b1, $0738,
  $ffcf, $ee46, $dcdd, $cd54, $b9eb, $a862, $9af9, $8b70,
  $8408, $9581, $a71a, $b693, $c22c, $d3a5, $e13e, $f0b7,
  $0840, $19c9, $2b52, $3adb, $4e64, $5fed, $6d76, $7cff,
  $9489, $8500, $b79b, $a612, $d2ad, $c324, $f1bf, $e036,
  $18c1, $0948, $3bd3, $2a5a, $5ee5, $4f6c, $7df7, $6c7e,
  $a50a, $b483, $8618, $9791, $e32e, $f2a7, $c03c, $d1b5,
  $2942, $38cb, $0a50, $1bd9, $6f66, $7eef, $4c74, $5dfd,
  $b58b, $a402, $9699, $8710, $f3af, $e226, $d0bd, $c134,
  $39c3, $284a, $1ad1, $0b58, $7fe7, $6e6e, $5cf5, $4d7c,
  $c60c, $d785, $e51e, $f497, $8028, $91a1, $a33a, $b2b3,
  $4a44, $5bcd, $6956, $78df, $0c60, $1de9, $2f72, $3efb,
  $d68d, $c704, $f59f, $e416, $90a9, $8120, $b3bb, $a232,
  $5ac5, $4b4c, $79d7, $685e, $1ce1, $0d68, $3ff3, $2e7a,
  $e70e, $f687, $c41c, $d595, $a12a, $b0a3, $8238, $93b1,
  $6b46, $7acf, $4854, $59dd, $2d62, $3ceb, $0e70, $1ff9,
  $f78f, $e606, $d49d, $c514, $b1ab, $a022, $92b9, $8330,
  $7bc7, $6a4e, $58d5, $495c, $3de3, $2c6a, $1ef1, $0f78);
  {Precomputed table for CRC16 CCITT values}

 {Base64 constants}
 Base64EncodeTable:String = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

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
 PAESKey = ^TAESKey;
 TAESKey = record
  Rounds:LongWord;
  EncryptKey:array[0..59] of LongWord;
  DecryptKey:array[0..59] of LongWord;
 end;

 PAESContext = ^TAESContext;
 TAESContext = record
  AESKey:TAESKey;
  CBC:array[0..AES_BLOCK_SIZE - 1] of Byte;
  CTR:array[0..AES_BLOCK_SIZE - 1] of Byte;
 end;

 {DES types}
 PDESKey = ^TDESKey;
 TDESKey = array[0..31] of LongWord;

 PDESContext = ^TDESContext;
 TDESContext = record
  EncryptKey:TDESKey;
  DecryptKey:TDESKey;
  CBC:array[0..DES_BLOCK_SIZE - 1] of Byte;
 end;

 {3DES types}
 PDES3Key = ^TDES3Key;
 TDES3Key = record
  EncryptKey:array[0..2] of TDESKey;
  DecryptKey:array[0..2] of TDESKey;
 end;

 PDES3Context = ^TDES3Context;
 TDES3Context = record
  DES3Key:TDES3Key;
  CBC:array[0..DES3_BLOCK_SIZE - 1] of Byte;
 end;

 {RC4 types}
 PRC4State = ^TRC4State;
 TRC4State = array[0..255] of Byte;

 PRC4Context = ^TRC4Context;
 TRC4Context = record
  Key:array[0..31] of Byte;
  KeySize:LongWord;
  Start:LongWord;
 end;

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

 {SHA512 types}
 PSHA512Digest = ^TSHA512Digest;
 TSHA512Digest = array[0..63] of Byte;

 PSHA512Block = ^TSHA512Block;
 TSHA512Block = record
  Data:Pointer;
  Size:LongWord;
  Next:PSHA512Block;
 end;

 PSHA512Context = ^TSHA512Context;
 TSHA512Context = record
  Data:array[0..127] of Byte;
  State:array[0..7] of QWord;
  Count:QWord;
 end;

 PSHA512ByteBuffer = ^TSHA512ByteBuffer;
 TSHA512ByteBuffer = array[0..127] of Byte;

 PSHA512_W = ^TSHA512_W;
 TSHA512_W = array[0..79] of QWord; {This is the W array for the SHA512 algorithm}

 {SHA384 types}
 PSHA384Digest = ^TSHA384Digest;
 TSHA384Digest = array[0..47] of Byte;

 PSHA384Block = PSHA512Block;
 TSHA384Block = TSHA512Block;

 PSHA384Context = PSHA512Context;
 TSHA384Context = TSHA512Context;

 PSHA384ByteBuffer = PSHA512ByteBuffer;
 TSHA384ByteBuffer = TSHA512ByteBuffer;

 {RSA types}
 PRSAContext = ^TRSAContext;
 TRSAContext = record
  M:PBigInt;             {Modulus}
  E:PBigInt;             {Public exponent}
  D:PBigInt;             {Private exponent}
  P:PBigInt;             {p in m = pq}
  Q:PBigInt;             {q in m = pq}
  DP:PBigInt;            {d mod (p-1)}
  DQ:PBigInt;            {d mod (q-1)}
  QInv:PBigInt;          {q^-1 mod p}
  ModulusLen:Integer;
  Context:PBigIntContext;
 end;

 {Random types}
 TGetRandomBytes = function(Buffer:PByte;Count:Integer):Boolean;

 {CRC types}

 {Base64 types}
 TBase64DecodeTable = array[0..255] of Char;

 {Hash context}
 PHashContext = ^THashContext;
 THashContext = record
  Algorithm:LongWord;
  Key:array[0..127] of Byte;
  KeySize:LongWord;
  case Integer of
   0:(MD5:TMD5Context);
   1:(SHA1:TSHA1Context);
   2:(SHA256:TSHA256Context);
   3:(SHA384:TSHA384Context);
   4:(SHA512:TSHA512Context);
 end;

 {Cipher context}
 PCipherContext = ^TCipherContext;
 TCipherContext = record
  Algorithm:LongWord;
  Mode:LongWord;
  case Integer of
   0:(RC4:TRC4Context);
   1:(DES:TDESContext);
   2:(DES3:TDES3Context);
   3:(AES:TAESContext);
 end;

{==============================================================================}
var
 {Crypto specific variables}
 CryptoGetRandomBytesHandler:TGetRandomBytes;

{==============================================================================}
{Initialization Functions}
procedure CryptoInit;

{==============================================================================}
{Crypto Functions}
function HashCreate(Algorithm:LongWord;Key:Pointer;KeySize:LongWord):PHashContext;
function HashDestroy(Context:PHashContext):Boolean;

function HashUpdate(Context:PHashContext;Data:Pointer;Size:LongWord):Boolean;
function HashFinish(Context:PHashContext;Digest:Pointer;Size:LongWord):Boolean;

function CipherCreate(Algorithm:LongWord;Vector,Key:Pointer;KeySize:LongWord):PCipherContext; overload;
function CipherCreate(Algorithm,Mode:LongWord;Nonce,Vector,Key:Pointer;VectorSize,KeySize:LongWord):PCipherContext; overload;
function CipherDestroy(Context:PCipherContext):Boolean;

function CipherEncrypt(Context:PCipherContext;Plain,Crypt:Pointer;Size:LongWord):Boolean;
function CipherDecrypt(Context:PCipherContext;Crypt,Plain:Pointer;Size:LongWord):Boolean;

{==============================================================================}
{MD5 Functions}
function MD5DigestData(Data:PMD5Block;Digest:PMD5Digest):Boolean;
function MD5DigestString(const Value:String;Digest:PMD5Digest):Boolean;

function HMACMD5DigestData(const Key:String;Data:PMD5Block;Digest:PMD5Digest):Boolean;
function HMACMD5DigestString(const Key,Value:String;Digest:PMD5Digest):Boolean;

{==============================================================================}
{AES Functions}
function AESEncryptData(Key:Pointer;KeySize:LongWord;Vector,Plain,Crypt:Pointer;Size:LongWord):Boolean;
function AESDecryptData(Key:Pointer;KeySize:LongWord;Vector,Crypt,Plain:Pointer;Size:LongWord):Boolean;

function AESCTREncryptData(Key:Pointer;KeySize:LongWord;Nonce,Plain,Crypt:Pointer;Size:LongWord):Boolean;
function AESCTRDecryptData(Key:Pointer;KeySize:LongWord;Nonce,Crypt,Plain:Pointer;Size:LongWord):Boolean; inline;

function AESGCMEncryptData(Key:Pointer;KeySize:LongWord;IV,AAD,Plain,Crypt:Pointer;IVSize,AADSize,Size:LongWord;Tag:Pointer):Boolean;
function AESGCMDecryptData(Key:Pointer;KeySize:LongWord;IV,AAD,Crypt,Plain:Pointer;IVSize,AADSize,Size:LongWord;const Tag:Pointer):Boolean;

function AESGCMGMAC(Key:Pointer;KeySize:LongWord;IV,AAD:Pointer;IVSize,AADSize:LongWord;Tag:Pointer):Boolean;

{==============================================================================}
{DES Functions}
function DESEncryptData(Key:Pointer;KeySize:LongWord;Vector,Plain,Crypt:Pointer;Size:LongWord):Boolean;
function DESDecryptData(Key:Pointer;KeySize:LongWord;Vector,Crypt,Plain:Pointer;Size:LongWord):Boolean;

{==============================================================================}
{3DES Functions}
function DES3EncryptData(Key:Pointer;KeySize:LongWord;Vector,Plain,Crypt:Pointer;Size:LongWord):Boolean;
function DES3DecryptData(Key:Pointer;KeySize:LongWord;Vector,Crypt,Plain:Pointer;Size:LongWord):Boolean;

{==============================================================================}
{RC4 Functions}
function RC4EncryptData(Key:Pointer;KeySize:LongWord;Plain,Crypt:Pointer;Size,Start:LongWord):Boolean;
function RC4DecryptData(Key:Pointer;KeySize:LongWord;Crypt,Plain:Pointer;Size,Start:LongWord):Boolean; inline;

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
{SHA384 Functions}
function SHA384DigestData(Data:PSHA384Block;Digest:PSHA384Digest):Boolean;
function SHA384DigestString(const Value:String;Digest:PSHA384Digest):Boolean;

function HMACSHA384DigestData(const Key:String;Data:PSHA384Block;Digest:PSHA384Digest):Boolean;
function HMACSHA384DigestString(const Key,Value:String;Digest:PSHA384Digest):Boolean;

{==============================================================================}
{SHA512 Functions}
function SHA512DigestData(Data:PSHA512Block;Digest:PSHA512Digest):Boolean;
function SHA512DigestString(const Value:String;Digest:PSHA512Digest):Boolean;

function HMACSHA512DigestData(const Key:String;Data:PSHA512Block;Digest:PSHA512Digest):Boolean;
function HMACSHA512DigestString(const Key,Value:String;Digest:PSHA512Digest):Boolean;

{==============================================================================}
{RSA Functions}
function RSAInitPrivateKey(Modulus,PublicExp,PrivateExp,P,Q,DP,DQ,QInv:PByte;ModulusLen,PublicExpLen,PrivateExpLen,PLen,QLen,DPLen,DQLen,QInvLen:Integer):PRSAContext;
function RSAInitPublicKey(Modulus,PublicExp:PByte;ModulusLen,PublicExpLen:Integer):PRSAContext;
function RSAFreeKey(Context:PRSAContext):Boolean;

function RSAEncryptSign(Context:PRSAContext;const Input:PByte;Len:Word;Output:PByte;Sign:Boolean):Integer;
function RSADecryptVerify(Context:PRSAContext;const Input:PByte;Output:PByte;Len:Integer;Verify:Boolean):Integer;

{==============================================================================}
{Random Functions}
function GetRandomBytes(Buffer:PByte;Count:Integer):Boolean;
function GetRandomBytesNonZero(Buffer:PByte;Count:Integer):Boolean;

{==============================================================================}
{CRC Functions}
function CRC16CCITT(CRC:Word;Data:PByte;Size:LongWord):Word;

{==============================================================================}
{Base64 Functions}
function Base64EncodeString(const Value:String):String;
function Base64DecodeString(const Value:String):String;

function Base64EncodeBuffer(const Source:PChar;SourceLen:Integer;Dest:PChar;var DestLen:Integer):Boolean;
function Base64DecodeBuffer(const Source:PChar;SourceLen:Integer;Dest:PChar;var DestLen:Integer):Boolean;

{==============================================================================}
{Crypto Helper Functions}
procedure BytesToLE32(Buffer:PByte;Count:LongWord);
procedure BytesToBE32(Buffer:PByte;Count:LongWord);

procedure BytesToLE64(Buffer:PByte;Count:LongWord);
procedure BytesToBE64(Buffer:PByte;Count:LongWord);

function LongWordToBE(Buffer:PByte):LongWord; inline;
procedure BEToLongWord(Value:LongWord;Buffer:PByte); inline;

function QWordToBE(Buffer:PByte):QWord; inline;
procedure BEToQWord(Value:QWord;Buffer:PByte); inline;

{==============================================================================}
{MD5 Helper Functions}
procedure MD5Init(var Context:TMD5Context);
procedure MD5Update(var Context:TMD5Context;Data:Pointer;Size:LongWord);
procedure MD5Final(var Context:TMD5Context;var Digest:TMD5Digest);

function MD5DigestToString(Digest:PMD5Digest):String;

{==============================================================================}
{AES Helper Functions}
function AESKeySetup(Key:Pointer;KeySize:LongWord;AESKey:PAESKey):Boolean;
procedure AESEncryptBlock(Plain,Crypt:Pointer;AESKey:PAESKey);
procedure AESDecryptBlock(Crypt,Plain:Pointer;AESKey:PAESKey);

{==============================================================================}
{DES Helper Functions}
function DESKeySetup(Key:Pointer;KeySize:LongWord;EncryptKey,DecryptKey:PDESKey):Boolean;
procedure DESEncryptBlock(Plain,Crypt:Pointer;EncryptKey:PDESKey);
procedure DESDecryptBlock(Crypt,Plain:Pointer;DecryptKey:PDESKey);

{==============================================================================}
{3DES Helper Functions}
function DES3KeySetup(Key:Pointer;KeySize:LongWord;DES3Key:PDES3Key):Boolean;
procedure DES3EncryptBlock(Plain,Crypt:Pointer;DES3Key:PDES3Key);
procedure DES3DecryptBlock(Crypt,Plain:Pointer;DES3Key:PDES3Key);

{==============================================================================}
{RC4 Helper Functions}

{==============================================================================}
{SHA1 Helper Functions}
procedure SHA1Init(var Context:TSHA1Context);
procedure SHA1Update(var Context:TSHA1Context;Data:Pointer;Size:LongWord);
procedure SHA1Final(var Context:TSHA1Context;var Digest:TSHA1Digest);

function SHA1DigestToString(Digest:PSHA1Digest):String;

{==============================================================================}
{SHA256 Helper Functions}
procedure SHA256Init(var Context:TSHA256Context);
procedure SHA256Process(var Context:TSHA256Context;Data:Pointer;Size:LongWord);
procedure SHA256Complete(var Context:TSHA256Context;var Digest:TSHA256Digest);

function SHA256DigestToString(Digest:PSHA256Digest):String;

{==============================================================================}
{SHA384 Helper Functions}
procedure SHA384Init(var Context:TSHA384Context);
procedure SHA384Process(var Context:TSHA384Context;Data:Pointer;Size:LongWord);
procedure SHA384Complete(var Context:TSHA384Context;var Digest:TSHA384Digest);

function SHA384DigestToString(Digest:PSHA384Digest):String;

{==============================================================================}
{SHA512 Helper Functions}
procedure SHA512Init(var Context:TSHA512Context);
procedure SHA512Process(var Context:TSHA512Context;Data:Pointer;Size:LongWord);
procedure SHA512Complete(var Context:TSHA512Context;var Digest:TSHA512Digest);

function SHA512DigestToString(Digest:PSHA512Digest):String;

{==============================================================================}
{RSA Helper Functions}

{==============================================================================}
{Random Helper Functions}

{==============================================================================}
{CRC Helper Functions}

{==============================================================================}
{Base64 Helper Functions}
procedure Base64InitTables;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Crypto specific variables}
 CryptoInitialized:Boolean;

 {Base64 variables}
 Base64DecodeTable:TBase64DecodeTable;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure CryptoInit;
begin
 {}
 {Check Initialized}
 if CryptoInitialized then Exit;

 {Init Base64 Tables}
 Base64InitTables;

 //To Do //Init Random

 CryptoInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Crypto Functions}
function HashCreate(Algorithm:LongWord;Key:Pointer;KeySize:LongWord):PHashContext;
{Initialize a hash context based on an algorithm and an optional key}
var
 Count:LongWord;
 Context:PHashContext;
 MD5KeyDigest:TMD5Digest;
 MD5PadBuffer:TMD5ByteBuffer;
 SHA1KeyDigest:TSHA1Digest;
 SHA1PadBuffer:TSHA1ByteBuffer;
 SHA256KeyDigest:TSHA256Digest;
 SHA256PadBuffer:TSHA256ByteBuffer;
 SHA384KeyDigest:TSHA384Digest;
 SHA384PadBuffer:TSHA384ByteBuffer;
 SHA512KeyDigest:TSHA512Digest;
 SHA512PadBuffer:TSHA512ByteBuffer;
begin
 {}
 Result:=nil;

 {Allocate context}
 Context:=AllocMem(SizeOf(THashContext));
 if Context = nil then Exit;

 {Update context}
 Context.Algorithm:=Algorithm;

 {Check algorithm}
 case Algorithm of
  CRYPTO_HASH_ALG_MD5:begin
    {Init MD5}
    MD5Init(Context.MD5);
   end;
  CRYPTO_HASH_ALG_SHA1:begin
    {Init SHA1}
    SHA1Init(Context.SHA1);
   end;
  CRYPTO_HASH_ALG_SHA256:begin
    {Init SHA256}
    SHA256Init(Context.SHA256);
   end;
  CRYPTO_HASH_ALG_SHA384:begin
    {Init SHA384}
    SHA384Init(Context.SHA384);
   end;
  CRYPTO_HASH_ALG_SHA512:begin
    {Init SHA512}
    SHA512Init(Context.SHA512);
   end;
  CRYPTO_HASH_ALG_HMAC_MD5:begin
    {Get key length}
    Context.KeySize:=KeySize;

    {Check key length}
    if Context.KeySize > 64 then
     begin
      {Init MD5}
      MD5Init(Context.MD5);

      {Update MD5 with Key}
      MD5Update(Context.MD5,Key,KeySize);

      {Finish MD5 with Key}
      MD5Final(Context.MD5,MD5KeyDigest);

      {Copy digest to key buffer}
      System.Move(MD5KeyDigest[0],Context.Key[0],16);

      {Update key length}
      Context.KeySize:=16;
     end
    else
     begin
      {Copy key to key buffer}
      System.Move(Key^,Context.Key,Context.KeySize);
     end;

    {XOR the key buffer with the iPad value}
    for Count:=0 to 63 do
     begin
      MD5PadBuffer[Count]:=Context.Key[Count] xor $36;
     end;

    {Init MD5}
    MD5Init(Context.MD5);

    {Update MD5 with iPad Key}
    MD5Update(Context.MD5,@MD5PadBuffer,SizeOf(TMD5ByteBuffer));
   end;
  CRYPTO_HASH_ALG_HMAC_SHA1:begin
    {Get key length}
    Context.KeySize:=KeySize;

    {Check key length}
    if Context.KeySize > 64 then
     begin
      {Init SHA1}
      SHA1Init(Context.SHA1);

      {Update SHA1 with Key}
      SHA1Update(Context.SHA1,Key,KeySize);

      {Finish SHA1 with Key}
      SHA1Final(Context.SHA1,SHA1KeyDigest);

      {Copy digest to key buffer}
      System.Move(SHA1KeyDigest[0],Context.Key[0],20);

      {Update key length}
      Context.KeySize:=20;
     end
    else
     begin
      {Copy key to key buffer}
      System.Move(Key^,Context.Key[0],Context.KeySize);
     end;

    {XOR the key buffer with the iPad value}
    for Count:=0 to 63 do
     begin
      SHA1PadBuffer[Count]:=Context.Key[Count] xor $36;
     end;

    {Init SHA1}
    SHA1Init(Context.SHA1);

    {Update SHA1 with iPad Key}
    SHA1Update(Context.SHA1,@SHA1PadBuffer,SizeOf(TSHA1ByteBuffer));
   end;
  CRYPTO_HASH_ALG_HMAC_SHA256:begin
    {Get key length}
    Context.KeySize:=KeySize;

    {Check key length}
    if Context.KeySize > 64 then
     begin
      {Init SHA256}
      SHA256Init(Context.SHA256);

      {Update SHA256 with Key}
      SHA256Process(Context.SHA256,Key,KeySize);

      {Finish SHA256 with Key}
      SHA256Complete(Context.SHA256,SHA256KeyDigest);

      {Copy digest to key buffer}
      System.Move(SHA256KeyDigest[0],Context.Key[0],32);

      {Update key length}
      Context.KeySize:=32;
     end
    else
     begin
      {Copy key to key buffer}
      System.Move(Key^,Context.Key[0],Context.KeySize);
     end;

    {XOR the key buffer with the iPad value}
    for Count:=0 to 63 do
     begin
      SHA256PadBuffer[Count]:=Context.Key[Count] xor $36;
     end;

    {Init SHA256}
    SHA256Init(Context.SHA256);

    {Update SHA256 with iPad Key}
    SHA256Process(Context.SHA256,@SHA256PadBuffer,SizeOf(TSHA256ByteBuffer));
   end;
  CRYPTO_HASH_ALG_HMAC_SHA384:begin
    {Get key length}
    Context.KeySize:=KeySize;

    {Check key length}
    if Context.KeySize > 128 then
     begin
      {Init SHA384}
      SHA384Init(Context.SHA384);

      {Update SHA384 with Key}
      SHA384Process(Context.SHA384,Key,KeySize);

      {Finish SHA384 with Key}
      SHA384Complete(Context.SHA384,SHA384KeyDigest);

      {Copy digest to key buffer}
      System.Move(SHA384KeyDigest[0],Context.Key[0],48);

      {Update key length}
      Context.KeySize:=48;
     end
    else
     begin
      {Copy key to key buffer}
      System.Move(Key^,Context.Key[0],Context.KeySize);
     end;

    {XOR the key buffer with the iPad value}
    for Count:=0 to 127 do
     begin
      SHA384PadBuffer[Count]:=Context.Key[Count] xor $36;
     end;

    {Init SHA384}
    SHA384Init(Context.SHA384);

    {Update SHA384 with iPad Key}
    SHA384Process(Context.SHA384,@SHA384PadBuffer,SizeOf(TSHA384ByteBuffer));
   end;
  CRYPTO_HASH_ALG_HMAC_SHA512:begin
    {Get key length}
    Context.KeySize:=KeySize;

    {Check key length}
    if Context.KeySize > 128 then
     begin
      {Init SHA512}
      SHA512Init(Context.SHA512);

      {Update SHA512 with Key}
      SHA512Process(Context.SHA512,Key,KeySize);

      {Finish SHA512 with Key}
      SHA512Complete(Context.SHA512,SHA512KeyDigest);

      {Copy digest to key buffer}
      System.Move(SHA512KeyDigest[0],Context.Key[0],64);

      {Update key length}
      Context.KeySize:=64;
     end
    else
     begin
      {Copy key to key buffer}
      System.Move(Key^,Context.Key[0],Context.KeySize);
     end;

    {XOR the key buffer with the iPad value}
    for Count:=0 to 127 do
     begin
      SHA512PadBuffer[Count]:=Context.Key[Count] xor $36;
     end;

    {Init SHA512}
    SHA512Init(Context.SHA512);

    {Update SHA512 with iPad Key}
    SHA512Process(Context.SHA512,@SHA512PadBuffer,SizeOf(TSHA512ByteBuffer));
   end;
  else
   begin
    {Invalid algorithm}
    FreeMem(Context);
    Exit;
   end;
 end;

 Result:=Context;
end;

{==============================================================================}

function HashDestroy(Context:PHashContext):Boolean;
{Free a hash context allocated by HashCreate}
begin
 {}
 Result:=False;

 if Context = nil then Exit;

 {Check algorithm}
 case Context.Algorithm of
  CRYPTO_HASH_ALG_MD5:begin
    {Nothing}
   end;
  CRYPTO_HASH_ALG_SHA1:begin
    {Nothing}
   end;
  CRYPTO_HASH_ALG_SHA256:begin
    {Nothing}
   end;
  CRYPTO_HASH_ALG_SHA384:begin
    {Nothing}
   end;
  CRYPTO_HASH_ALG_SHA512:begin
    {Nothing}
   end;
  CRYPTO_HASH_ALG_HMAC_MD5:begin
    {Nothing}
   end;
  CRYPTO_HASH_ALG_HMAC_SHA1:begin
    {Nothing}
   end;
  CRYPTO_HASH_ALG_HMAC_SHA256:begin
    {Nothing}
   end;
  CRYPTO_HASH_ALG_HMAC_SHA384:begin
    {Nothing}
   end;
  CRYPTO_HASH_ALG_HMAC_SHA512:begin
    {Nothing}
   end;
  else
   begin
    Exit;
   end;
 end;

 {Zero context}
 FillChar(Context^,SizeOf(THashContext),0);

 {Free context}
 FreeMem(Context);

 Result:=True;
end;

{==============================================================================}

function HashUpdate(Context:PHashContext;Data:Pointer;Size:LongWord):Boolean;
{Add a block of data to a hash context}
begin
 {}
 Result:=False;

 if Context = nil then Exit;
 if Data = nil then Exit;

 {Check algorithm}
 case Context.Algorithm of
  CRYPTO_HASH_ALG_MD5,CRYPTO_HASH_ALG_HMAC_MD5:begin
    {Update MD5}
    MD5Update(Context.MD5,Data,Size);
   end;
  CRYPTO_HASH_ALG_SHA1,CRYPTO_HASH_ALG_HMAC_SHA1:begin
    {Update SHA1}
    SHA1Update(Context.SHA1,Data,Size);
   end;
  CRYPTO_HASH_ALG_SHA256,CRYPTO_HASH_ALG_HMAC_SHA256:begin
    {Update SHA256}
    SHA256Process(Context.SHA256,Data,Size);
   end;
  CRYPTO_HASH_ALG_SHA384,CRYPTO_HASH_ALG_HMAC_SHA384:begin
    {Update SHA384}
    SHA384Process(Context.SHA384,Data,Size);
   end;
  CRYPTO_HASH_ALG_SHA512,CRYPTO_HASH_ALG_HMAC_SHA512:begin
    {Update SHA512}
    SHA512Process(Context.SHA512,Data,Size);
   end;
  else
   begin
    Exit;
   end;
 end;

 Result:=True;
end;

{==============================================================================}

function HashFinish(Context:PHashContext;Digest:Pointer;Size:LongWord):Boolean;
{Finalize a hash context and return the digest (Hash) value}
var
 Count:LongWord;
 MD5PadBuffer:TMD5ByteBuffer;
 SHA1PadBuffer:TSHA1ByteBuffer;
 SHA256PadBuffer:TSHA256ByteBuffer;
 SHA384PadBuffer:TSHA384ByteBuffer;
 SHA512PadBuffer:TSHA512ByteBuffer;
begin
 {}
 Result:=False;

 if Context = nil then Exit;
 if Digest = nil then Exit;

 {Check algorithm}
 case Context.Algorithm of
  CRYPTO_HASH_ALG_MD5:begin
    {Check size}
    if Size < 16 then Exit;

    {Finish MD5}
    MD5Final(Context.MD5,PMD5Digest(Digest)^);
   end;
  CRYPTO_HASH_ALG_SHA1:begin
    {Check size}
    if Size < 20 then Exit;

    {Finish SHA1}
    SHA1Final(Context.SHA1,PSHA1Digest(Digest)^);
   end;
  CRYPTO_HASH_ALG_SHA256:begin
    {Check size}
    if Size < 32 then Exit;

    {Finish SHA256}
    SHA256Complete(Context.SHA256,PSHA256Digest(Digest)^);
   end;
  CRYPTO_HASH_ALG_SHA384:begin
    {Check size}
    if Size < 48 then Exit;

    {Finish SHA384}
    SHA384Complete(Context.SHA384,PSHA384Digest(Digest)^);
   end;
  CRYPTO_HASH_ALG_SHA512:begin
    {Check size}
    if Size < 64 then Exit;

    {Finish SHA512}
    SHA512Complete(Context.SHA512,PSHA512Digest(Digest)^);
   end;
  CRYPTO_HASH_ALG_HMAC_MD5:begin
    {Check size}
    if Size < 16 then Exit;

    {Finish MD5}
    MD5Final(Context.MD5,PMD5Digest(Digest)^);

    {XOR the key buffer with the oPad value}
    for Count:=0 to 63 do
     begin
      MD5PadBuffer[Count]:=Context.Key[Count] xor $5c;
     end;

    {Init MD5}
    MD5Init(Context.MD5);

    {Update MD5 with oPad Key}
    MD5Update(Context.MD5,@MD5PadBuffer,SizeOf(TMD5ByteBuffer));

    {Update MD5 with Digest}
    MD5Update(Context.MD5,Digest,16);

    {Finish MD5}
    MD5Final(Context.MD5,PMD5Digest(Digest)^);
   end;
  CRYPTO_HASH_ALG_HMAC_SHA1:begin
    {Check size}
    if Size < 20 then Exit;

    {Finish SHA1}
    SHA1Final(Context.SHA1,PSHA1Digest(Digest)^);

    {XOR the key buffer with the oPad value}
    for Count:=0 to 63 do
     begin
      SHA1PadBuffer[Count]:=Context.Key[Count] xor $5c;
     end;

    {Init SHA1}
    SHA1Init(Context.SHA1);

    {Update SHA1 with oPad Key}
    SHA1Update(Context.SHA1,@SHA1PadBuffer,SizeOf(TSHA1ByteBuffer));

    {Update SHA1 with Digest}
    SHA1Update(Context.SHA1,Digest,20);

    {Finish SHA1}
    SHA1Final(Context.SHA1,PSHA1Digest(Digest)^);
   end;
  CRYPTO_HASH_ALG_HMAC_SHA256:begin
    {Check size}
    if Size < 32 then Exit;

    {Finish SHA256}
    SHA256Complete(Context.SHA256,PSHA256Digest(Digest)^);

    {XOR the key buffer with the oPad value}
    for Count:=0 to 63 do
     begin
      SHA256PadBuffer[Count]:=Context.Key[Count] xor $5c;
     end;

    {Init SHA256}
    SHA256Init(Context.SHA256);

    {Update SHA256 with oPad Key}
    SHA256Process(Context.SHA256,@SHA256PadBuffer,SizeOf(TSHA256ByteBuffer));

    {Update SHA256 with Digest}
    SHA256Process(Context.SHA256,Digest,32);

    {Finish SHA256}
    SHA256Complete(Context.SHA256,PSHA256Digest(Digest)^);
   end;
  CRYPTO_HASH_ALG_HMAC_SHA384:begin
    {Check size}
    if Size < 48 then Exit;

    {Finish SHA384}
    SHA384Complete(Context.SHA384,PSHA384Digest(Digest)^);

    {XOR the key buffer with the oPad value}
    for Count:=0 to 127 do
     begin
      SHA384PadBuffer[Count]:=Context.Key[Count] xor $5c;
     end;

    {Init SHA384}
    SHA384Init(Context.SHA384);

    {Update SHA384 with oPad Key}
    SHA384Process(Context.SHA384,@SHA384PadBuffer,SizeOf(TSHA384ByteBuffer));

    {Update SHA384 with Digest}
    SHA384Process(Context.SHA384,Digest,48);

    {Finish SHA384}
    SHA384Complete(Context.SHA384,PSHA384Digest(Digest)^);
   end;
  CRYPTO_HASH_ALG_HMAC_SHA512:begin
    {Check size}
    if Size < 64 then Exit;

    {Finish SHA512}
    SHA512Complete(Context.SHA512,PSHA512Digest(Digest)^);

    {XOR the key buffer with the oPad value}
    for Count:=0 to 127 do
     begin
      SHA512PadBuffer[Count]:=Context.Key[Count] xor $5c;
     end;

    {Init SHA512}
    SHA512Init(Context.SHA512);

    {Update SHA512 with oPad Key}
    SHA512Process(Context.SHA512,@SHA512PadBuffer,SizeOf(TSHA512ByteBuffer));

    {Update SHA512 with Digest}
    SHA512Process(Context.SHA512,Digest,64);

    {Finish SHA512}
    SHA512Complete(Context.SHA512,PSHA512Digest(Digest)^);
   end;
  else
   begin
    Exit;
   end;
 end;

 Result:=True;
end;

{==============================================================================}

function CipherCreate(Algorithm:LongWord;Vector,Key:Pointer;KeySize:LongWord):PCipherContext;
{Initialize a cipher context based on an algorithm and a key}
{For block ciphers an initialization vector can be passed to enable CBC mode}
begin
 {}
 if Vector = nil then
  begin
   Result:=CipherCreate(Algorithm,CRYPTO_CIPHER_MODE_ECB,nil,nil,Key,0,KeySize);
  end
 else
  begin
   Result:=CipherCreate(Algorithm,CRYPTO_CIPHER_MODE_CBC,nil,Vector,Key,0,KeySize);
  end;
end;

{==============================================================================}

function CipherCreate(Algorithm,Mode:LongWord;Nonce,Vector,Key:Pointer;VectorSize,KeySize:LongWord):PCipherContext;
{Initialize a cipher context based on an algorithm, mode and key}
{For block ciphers a Nonce or IV can be passed if the selected mode requires it}
var
 Context:PCipherContext;
begin
 {}
 Result:=nil;

 //To Do //Mode, Nonce and VectorSize

 {Allocate context}
 Context:=AllocMem(SizeOf(TCipherContext));
 if Context = nil then Exit;

 {Update context}
 Context.Algorithm:=Algorithm;

 {Check algorithm}
 case Algorithm of
  CRYPTO_CIPHER_ALG_RC4:begin
    {Mode not supported}

    {Check Key}
    if Key = nil then
     begin
      FreeMem(Context);
      Exit;
     end;

    {Check Key Size}
    if (KeySize = 0) or (KeySize > SizeOf(Context.RC4.Key)) then
     begin
      FreeMem(Context);
      Exit;
     end;

    {Copy Key}
    Context.RC4.KeySize:=KeySize;
    System.Move(Key^,Context.RC4.Key[0],KeySize);
   end;
  CRYPTO_CIPHER_ALG_AES:begin
    {Check Key}
    if Key = nil then
     begin
      FreeMem(Context);
      Exit;
     end;

    {Check Vector}
    if Vector = nil then
     begin
      FreeMem(Context);
      Exit;
     end;

    {Setup Keys}
    if not AESKeySetup(Key,KeySize,@Context.AES.AESKey) then
     begin
      FreeMem(Context);
      Exit;
     end;

    {Copy Vector}
    System.Move(Vector^,Context.AES.CBC[0],AES_BLOCK_SIZE);
   end;
  CRYPTO_CIPHER_ALG_3DES:begin
    {Check Key}
    if Key = nil then
     begin
      FreeMem(Context);
      Exit;
     end;

    {Check Vector}
    if Vector = nil then
     begin
      FreeMem(Context);
      Exit;
     end;

    {Setup Keys}
    if not DES3KeySetup(Key,KeySize,@Context.DES3.DES3Key) then
     begin
      FreeMem(Context);
      Exit;
     end;

    {Copy Vector}
    System.Move(Vector^,Context.DES3.CBC[0],DES3_BLOCK_SIZE);
   end;
  CRYPTO_CIPHER_ALG_DES:begin
    {Check Key}
    if Key = nil then
     begin
      FreeMem(Context);
      Exit;
     end;

    {Check Vector}
    if Vector = nil then
     begin
      FreeMem(Context);
      Exit;
     end;

    {Setup Keys}
    if not DESKeySetup(Key,KeySize,@Context.DES.EncryptKey,@Context.DES.DecryptKey) then
     begin
      FreeMem(Context);
      Exit;
     end;

    {Copy Vector}
    System.Move(Vector^,Context.DES.CBC[0],DES_BLOCK_SIZE);
   end;
  else
   begin
    {Invalid algorithm}
    FreeMem(Context);
    Exit;
   end;
 end;

 Result:=Context;
end;

{==============================================================================}

function CipherDestroy(Context:PCipherContext):Boolean;
{Free a cipher context allocated by CipherCreate}
begin
 {}
 Result:=False;

 if Context = nil then Exit;

 {Check algorithm}
 case Context.Algorithm of
  CRYPTO_CIPHER_ALG_RC4:begin
    {Nothing}
   end;
  CRYPTO_CIPHER_ALG_AES:begin
    {Nothing}
   end;
  CRYPTO_CIPHER_ALG_3DES:begin
    {Nothing}
   end;
  CRYPTO_CIPHER_ALG_DES:begin
    {Nothing}
   end;
  else
   begin
    Exit;
   end;
 end;

 {Zero context}
 FillChar(Context^,SizeOf(TCipherContext),0);

 {Free context}
 FreeMem(Context);

 Result:=True;
end;

{==============================================================================}

function CipherEncrypt(Context:PCipherContext;Plain,Crypt:Pointer;Size:LongWord):Boolean;
{Encrypt a block of data using an existing cipher context}
var
 I:LongWord;
 J:LongWord;
 Blocks:LongWord;
begin
 {}
 Result:=False;

 if Context = nil then Exit;
 if Plain = nil then Exit;
 if Crypt = nil then Exit;
 if Size = 0 then Exit;

 {Check algorithm}
 case Context.Algorithm of
  CRYPTO_CIPHER_ALG_RC4:begin
    {Encrypt Data}
    if not RC4EncryptData(@Context.RC4.Key[0],Context.RC4.KeySize,Plain,Crypt,Size,Context.RC4.Start) then Exit;

    {Update Start}
    Inc(Context.RC4.Start,Size);
   end;
  CRYPTO_CIPHER_ALG_AES:begin
    {Check Size}
    if (Size mod AES_BLOCK_SIZE) <> 0 then Exit;

    {Get Blocks}
    Blocks:=Size div AES_BLOCK_SIZE;

    {Process Blocks}
    for I:=0 to Blocks - 1 do
     begin
      {Update Vector}
      for J:=0 to AES_BLOCK_SIZE - 1 do
       begin
        Context.AES.CBC[J]:=Context.AES.CBC[J] xor PByte(Plain)[J];
       end;

      {Encrypt Block}
      AESEncryptBlock(@Context.AES.CBC[0],@Context.AES.CBC[0],@Context.AES.AESKey);

      {Copy Crypt}
      System.Move(Context.AES.CBC[0],Crypt^,AES_BLOCK_SIZE);

      {Update Pointers}
      Inc(Plain,AES_BLOCK_SIZE); {Increment untyped pointer increments in bytes}
      Inc(Crypt,AES_BLOCK_SIZE); {Increment untyped pointer increments in bytes}
     end;
   end;
  CRYPTO_CIPHER_ALG_3DES:begin
    {Check Size}
    if (Size mod DES3_BLOCK_SIZE) <> 0 then Exit;

    {Get Blocks}
    Blocks:=Size div DES3_BLOCK_SIZE;

    {Process Blocks}
    for I:=0 to Blocks - 1 do
     begin
      {Update Vector}
      for J:=0 to DES3_BLOCK_SIZE - 1 do
       begin
        Context.DES3.CBC[J]:=Context.DES3.CBC[J] xor PByte(Plain)[J];
       end;

      {Encrypt Block}
      DES3EncryptBlock(@Context.DES3.CBC[0],@Context.DES3.CBC[0],@Context.DES3.DES3Key);

      {Copy Crypt}
      System.Move(Context.DES3.CBC[0],Crypt^,DES3_BLOCK_SIZE);

      {Update Pointers}
      Inc(Plain,DES3_BLOCK_SIZE); {Increment untyped pointer increments in bytes}
      Inc(Crypt,DES3_BLOCK_SIZE); {Increment untyped pointer increments in bytes}
     end;
   end;
  CRYPTO_CIPHER_ALG_DES:begin
    {Check Size}
    if (Size mod DES_BLOCK_SIZE) <> 0 then Exit;

    {Get Blocks}
    Blocks:=Size div DES_BLOCK_SIZE;

    {Process Blocks}
    for I:=0 to Blocks - 1 do
     begin
      {Update Vector}
      for J:=0 to DES_BLOCK_SIZE - 1 do
       begin
        Context.DES.CBC[J]:=Context.DES.CBC[J] xor PByte(Plain)[J];
       end;

      {Encrypt Block}
      DESEncryptBlock(@Context.DES.CBC[0],@Context.DES.CBC[0],@Context.DES.EncryptKey);

      {Copy Crypt}
      System.Move(Context.DES.CBC[0],Crypt^,DES_BLOCK_SIZE);

      {Update Pointers}
      Inc(Plain,DES_BLOCK_SIZE); {Increment untyped pointer increments in bytes}
      Inc(Crypt,DES_BLOCK_SIZE); {Increment untyped pointer increments in bytes}
     end;
   end;
  else
   begin
    Exit;
   end;
 end;

 Result:=True;
end;

{==============================================================================}

function CipherDecrypt(Context:PCipherContext;Crypt,Plain:Pointer;Size:LongWord):Boolean;
{Decrypt a block of data using an existing cipher context}
var
 I:LongWord;
 J:LongWord;
 Blocks:LongWord;
 Temp:array[0..AES_BLOCK_SIZE - 1] of Byte;
begin
 {}
 Result:=False;

 if Context = nil then Exit;
 if Crypt = nil then Exit;
 if Plain = nil then Exit;
 if Size = 0 then Exit;

 {Check algorithm}
 case Context.Algorithm of
  CRYPTO_CIPHER_ALG_RC4:begin
    {Decrypt Data}
    if not RC4DecryptData(@Context.RC4.Key[0],Context.RC4.KeySize,Crypt,Plain,Size,Context.RC4.Start) then Exit;

    {Update Start}
    Inc(Context.RC4.Start,Size);
   end;
  CRYPTO_CIPHER_ALG_AES:begin
    {Check Size}
    if (Size mod AES_BLOCK_SIZE) <> 0 then Exit;

    {Get Blocks}
    Blocks:=Size div AES_BLOCK_SIZE;

    {Process Blocks}
    for I:=0 to Blocks - 1 do
     begin
      {Copy Crypt}
      System.Move(Crypt^,Temp[0],AES_BLOCK_SIZE);

      {Decrypt Block}
      AESDecryptBlock(Crypt,Plain,@Context.AES.AESKey);

      {Update Vector}
      for J:=0 to AES_BLOCK_SIZE - 1 do
       begin
        PByte(Plain)[J]:=PByte(Plain)[J] xor Context.AES.CBC[J];
       end;

      {Copy Vector}
      System.Move(Temp[0],Context.AES.CBC[0],AES_BLOCK_SIZE);

      {Update Pointers}
      Inc(Crypt,AES_BLOCK_SIZE); {Increment untyped pointer increments in bytes}
      Inc(Plain,AES_BLOCK_SIZE); {Increment untyped pointer increments in bytes}
     end;
   end;
  CRYPTO_CIPHER_ALG_3DES:begin
    {Check Size}
    if (Size mod DES3_BLOCK_SIZE) <> 0 then Exit;

    {Get Blocks}
    Blocks:=Size div DES3_BLOCK_SIZE;

    {Process Blocks}
    for I:=0 to Blocks - 1 do
     begin
      {Copy Crypt}
      System.Move(Crypt^,Temp[0],DES3_BLOCK_SIZE);

      {Decrypt Block}
      DES3DecryptBlock(Crypt,Plain,@Context.DES3.DES3Key);

      {Update Vector}
      for J:=0 to DES3_BLOCK_SIZE - 1 do
       begin
        PByte(Plain)[J]:=PByte(Plain)[J] xor Context.DES3.CBC[J];
       end;

      {Copy Vector}
      System.Move(Temp[0],Context.DES3.CBC[0],DES3_BLOCK_SIZE);

      {Update Pointers}
      Inc(Crypt,DES3_BLOCK_SIZE); {Increment untyped pointer increments in bytes}
      Inc(Plain,DES3_BLOCK_SIZE); {Increment untyped pointer increments in bytes}
     end;
   end;
  CRYPTO_CIPHER_ALG_DES:begin
    {Check Size}
    if (Size mod DES_BLOCK_SIZE) <> 0 then Exit;

    {Get Blocks}
    Blocks:=Size div DES_BLOCK_SIZE;

    {Process Blocks}
    for I:=0 to Blocks - 1 do
     begin
      {Copy Crypt}
      System.Move(Crypt^,Temp[0],DES_BLOCK_SIZE);

      {Decrypt Block}
      DESDecryptBlock(Crypt,Plain,@Context.DES.DecryptKey);

      {Update Vector}
      for J:=0 to DES_BLOCK_SIZE - 1 do
       begin
        PByte(Plain)[J]:=PByte(Plain)[J] xor Context.DES.CBC[J];
       end;

      {Copy Vector}
      System.Move(Temp[0],Context.DES.CBC[0],DES_BLOCK_SIZE);

      {Update Pointers}
      Inc(Crypt,DES_BLOCK_SIZE); {Increment untyped pointer increments in bytes}
      Inc(Plain,DES_BLOCK_SIZE); {Increment untyped pointer increments in bytes}
     end;
   end;
  else
   begin
    Exit;
   end;
 end;

 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{MD5 Functions}
function MD5DigestData(Data:PMD5Block;Digest:PMD5Digest):Boolean;
{Generate a 128 bit MD5 digest (Hash) from the supplied data}
{Data is a linked list which can contain multiple independent blocks to be
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
function AESEncryptData(Key:Pointer;KeySize:LongWord;Vector,Plain,Crypt:Pointer;Size:LongWord):Boolean;
{Encrypt a block of data using the AES cipher, if Vector is supplied use CBC mode else use ECB}
{Key size must be 16, 24 or 32 bytes (128, 192 or 256 bits)}
{Vector must be 16 bytes (128 bits) long if supplied}
{Size must be a multiple of 16 bytes long}
{Plain text and Crypt data pointers must be the same length (and can point to the same value)}
var
 I:LongWord;
 J:LongWord;
 AESKey:TAESKey;
 Blocks:LongWord;
 CBC:array[0..AES_BLOCK_SIZE - 1] of Byte;
begin
 {}
 Result:=False;

 {Check Params}
 if Key = nil then Exit;
 if KeySize = 0 then Exit;
 if Plain = nil then Exit;
 if Crypt = nil then Exit;
 if Size = 0 then Exit;

 {Check Size}
 if (Size mod AES_BLOCK_SIZE) <> 0 then Exit;

 {Setup Keys}
 if not AESKeySetup(Key,KeySize,@AESKey) then Exit;

 {Check Vector}
 if Vector = nil then
  begin
   {Get Blocks}
   Blocks:=Size div AES_BLOCK_SIZE;

   //To Do //ECB mode
  end
 else
  begin
   {Copy Vector}
   System.Move(Vector^,CBC[0],AES_BLOCK_SIZE);

   {Get Blocks}
   Blocks:=Size div AES_BLOCK_SIZE;

   {Process Blocks}
   for I:=0 to Blocks - 1 do
    begin
     {Update Vector}
     for J:=0 to AES_BLOCK_SIZE - 1 do
      begin
       CBC[J]:=CBC[J] xor PByte(Plain)[J];
      end;

     {Encrypt Block}
     AESEncryptBlock(@CBC[0],@CBC[0],@AESKey);

     {Copy Crypt}
     System.Move(CBC[0],Crypt^,AES_BLOCK_SIZE);

     {Update Pointers}
     Inc(Plain,AES_BLOCK_SIZE); {Increment untyped pointer increments in bytes}
     Inc(Crypt,AES_BLOCK_SIZE); {Increment untyped pointer increments in bytes}
    end;

   {Zero Key and Vector}
   FillChar(AESKey,SizeOf(TAESKey),0);
   FillChar(CBC,SizeOf(CBC),0);

   Result:=True;
  end;
end;

{==============================================================================}

function AESDecryptData(Key:Pointer;KeySize:LongWord;Vector,Crypt,Plain:Pointer;Size:LongWord):Boolean;
{Decrypt a block of data using the AES cipher, if Vector is supplied use CBC mode else use ECB}
{Key size must be 16, 24 or 32 bytes (128, 192 or 256 bits)}
{Vector must be 16 bytes (128 bits) long if supplied}
{Size must be a multiple of 16 bytes long}
{Plain text and Crypt data pointers must be the same length (and can point to the same value)}
var
 I:LongWord;
 J:LongWord;
 AESKey:TAESKey;
 Blocks:LongWord;
 CBC:array[0..AES_BLOCK_SIZE - 1] of Byte;
 Temp:array[0..AES_BLOCK_SIZE - 1] of Byte;
begin
 {}
 Result:=False;

 {Check Params}
 if Key = nil then Exit;
 if KeySize = 0 then Exit;
 if Crypt = nil then Exit;
 if Plain = nil then Exit;
 if Size = 0 then Exit;

 {Check Size}
 if (Size mod AES_BLOCK_SIZE) <> 0 then Exit;

 {Setup Keys}
 if not AESKeySetup(Key,KeySize,@AESKey) then Exit;

 {Check Vector}
 if Vector = nil then
  begin
   {Get Blocks}
   Blocks:=Size div AES_BLOCK_SIZE;

   //To Do //ECB mode
  end
 else
  begin
   {Copy Vector}
   System.Move(Vector^,CBC[0],AES_BLOCK_SIZE);

   {Get Blocks}
   Blocks:=Size div AES_BLOCK_SIZE;

   {Process Blocks}
   for I:=0 to Blocks - 1 do
    begin
     {Copy Crypt}
     System.Move(Crypt^,Temp[0],AES_BLOCK_SIZE);

     {Decrypt Block}
     AESDecryptBlock(Crypt,Plain,@AESKey);

     {Update Vector}
     for J:=0 to AES_BLOCK_SIZE - 1 do
      begin
       PByte(Plain)[J]:=PByte(Plain)[J] xor CBC[J];
      end;

     {Copy Vector}
     System.Move(Temp[0],CBC[0],AES_BLOCK_SIZE);

     {Update Pointers}
     Inc(Crypt,AES_BLOCK_SIZE); {Increment untyped pointer increments in bytes}
     Inc(Plain,AES_BLOCK_SIZE); {Increment untyped pointer increments in bytes}
    end;

   {Zero Key and Vector}
   FillChar(AESKey,SizeOf(TAESKey),0);
   FillChar(Temp,SizeOf(Temp),0);
   FillChar(CBC,SizeOf(CBC),0);

   Result:=True;
  end;
end;

{==============================================================================}

function AESCTREncryptData(Key:Pointer;KeySize:LongWord;Nonce,Plain,Crypt:Pointer;Size:LongWord):Boolean;
{Encrypt a block of data with the supplied Key and Nonce using AES CTR mode}
{Key size must be 16, 24 or 32 bytes (128, 192 or 256 bits)}
{Nonce must be 16 bytes (128 bits) long}
{Plain text and Crypt data pointers must be the same length (and can point to the same value)}
var
 I:LongWord;
 J:LongWord;
 AESKey:TAESKey;
 Block:LongWord;
 Remain:LongWord;
 CTR:array[0..AES_BLOCK_SIZE - 1] of Byte;
 Temp:array[0..AES_BLOCK_SIZE - 1] of Byte;
begin
 {}
 Result:=False;

 {Check Params}
 if Key = nil then Exit;
 if KeySize = 0 then Exit;
 if Nonce = nil then Exit;
 if Plain = nil then Exit;
 if Crypt = nil then Exit;
 if Size = 0 then Exit;

 {Setup Keys}
 if not AESKeySetup(Key,KeySize,@AESKey) then Exit;

 {Copy Nonce}
 System.Move(Nonce^,CTR[0],AES_BLOCK_SIZE);

 {Process Blocks}
 Remain:=Size;
 while Remain > 0 do
  begin
   {Encrypt Block}
   AESEncryptBlock(@CTR[0],@Temp[0],@AESKey);

   {Get Block Size}
   Block:=AES_BLOCK_SIZE;
   if Remain < Block then Block:=Remain;

   {XOR Plain Text}
   for J:=0 to Block - 1 do
    begin
     PByte(Crypt)[J]:=PByte(Plain)[J] xor Temp[J];
    end;

   {Update Pointers}
   Inc(PByte(Plain),Block); {Increment untyped pointer increments in bytes}
   Inc(PByte(Crypt),Block); {Increment untyped pointer increments in bytes}

   {Update Remain}
   Dec(Remain,Block);

   {Increment Counter}
   for I:=AES_BLOCK_SIZE - 1 downto 0 do
    begin
     Inc(CTR[I]);
     if CTR[I] <> 0 then Break;
    end;
  end;

 {Zero Key and Counter}
 FillChar(AESKey,SizeOf(TAESKey),0);
 FillChar(Temp,SizeOf(Temp),0);
 FillChar(CTR,SizeOf(CTR),0);

 Result:=True;
end;

{==============================================================================}

function AESCTRDecryptData(Key:Pointer;KeySize:LongWord;Nonce,Crypt,Plain:Pointer;Size:LongWord):Boolean; inline;
{Decrypt a block of data with the supplied Key and Nonce using AES CTR mode}
{Key size must be 16, 24 or 32 bytes (128, 192 or 256 bits)}
{Nonce must be 16 bytes (128 bits) long}
{Plain text and Crypt data pointers must be the same length (and can point to the same value)}
begin
 {}
 Result:=AESCTREncryptData(Key,Keysize,Nonce,Plain,Crypt,Size);
end;

{==============================================================================}

procedure GCMInc32(Block:PByte); inline;
{Internal function used by AESGCMEncryptData/AESGCMDecryptData}
var
 Value:LongWord;
begin
 {}
 Value:=LongWordToBE(PByte(PtrUInt(Block) + AES_BLOCK_SIZE - 4));
 Inc(Value);
 BEToLongWord(Value,PByte(PtrUInt(Block) + AES_BLOCK_SIZE - 4));
end;

{==============================================================================}

procedure GCMXORBlock(Dest:PByte;const Source:PByte); inline;
{Internal function used by AESGCMEncryptData/AESGCMDecryptData}
var
 D:PLongWord;
 S:PLongWord;
begin
 {}
 D:=PLongWord(Dest);
 S:=PLongWord(Source);

 D^:=D^ xor S^;
 Inc(D);  {Increment PLongWord increments by SizeOf(LongWord)}
 Inc(S);

 D^:=D^ xor S^;
 Inc(D);  {Increment PLongWord increments by SizeOf(LongWord)}
 Inc(S);

 D^:=D^ xor S^;
 Inc(D);  {Increment PLongWord increments by SizeOf(LongWord)}
 Inc(S);

 D^:=D^ xor S^;
 Inc(D);  {Increment PLongWord increments by SizeOf(LongWord)}
 Inc(S);
end;

{==============================================================================}

procedure GCMXORBlocks(Dest:PByte;const Source1,Source2:PByte); inline;
{Internal function used by AESGCMEncryptData/AESGCMDecryptData}
var
 D:PLongWord;
 S1:PLongWord;
 S2:PLongWord;
begin
 {}
 D:=PLongWord(Dest);
 S1:=PLongWord(Source1);
 S2:=PLongWord(Source2);

 D^:=S1^ xor S2^;
 Inc(D);  {Increment PLongWord increments by SizeOf(LongWord)}
 Inc(S1);
 Inc(S2);

 D^:=S1^ xor S2^;
 Inc(D);  {Increment PLongWord increments by SizeOf(LongWord)}
 Inc(S1);
 Inc(S2);

 D^:=S1^ xor S2^;
 Inc(D);  {Increment PLongWord increments by SizeOf(LongWord)}
 Inc(S1);
 Inc(S2);

 D^:=S1^ xor S2^;
 Inc(D);  {Increment PLongWord increments by SizeOf(LongWord)}
 Inc(S1);
 Inc(S2);
end;

{==============================================================================}

procedure GCMShiftRightBlock(V:PByte);
{Internal function used by AESGCMEncryptData/AESGCMDecryptData}
var
 Value:LongWord;
begin
 {}
 Value:=LongWordToBE(PByte(PtrUInt(PtrUInt(V) + 12)));
 Value:=Value shr 1;
 if (V[11] and $01) <> 0 then
  begin
   Value:=Value or $80000000;
  end;
 BEToLongWord(Value,PByte(PtrUInt(PtrUInt(V) + 12)));

 Value:=LongWordToBE(PByte(PtrUInt(PtrUInt(V) + 8)));
 Value:=Value shr 1;
 if (V[7] and $01) <> 0 then
  begin
   Value:=Value or $80000000;
  end;
 BEToLongWord(Value,PByte(PtrUInt(PtrUInt(V) + 8)));

 Value:=LongWordToBE(PByte(PtrUInt(PtrUInt(V) + 4)));
 Value:=Value shr 1;
 if (V[3] and $01) <> 0 then
  begin
   Value:=Value or $80000000;
  end;
 BEToLongWord(Value,PByte(PtrUInt(PtrUInt(V) + 4)));

 Value:=LongWordToBE(V);
 Value:=Value shr 1;
 BEToLongWord(Value,V);
end;

{==============================================================================}

procedure GCMGFMult(const X,Y:PByte;Z:PByte);
{Multiplication in GF(2^128)}
{Internal function used by AESGCMEncryptData/AESGCMDecryptData}
var
 I:Integer;
 J:Integer;
 V:array[0..AES_BLOCK_SIZE - 1] of Byte;
begin
 {}
 {Z_0 = 0^128}
 FillChar(Z^,AES_BLOCK_SIZE,0);

 {V_0 = Y}
 System.Move(Y^,V,AES_BLOCK_SIZE);

 for I := 0 to AES_BLOCK_SIZE - 1 do
  begin
   for J := 0 to 7 do
    begin
     if (X[I] and (1 shl (7 - J))) <> 0 then
      begin
       {Z_(i + 1) = Z_i XOR V_i}
       GCMXORBlock(Z,@V);
      end
     else
      begin
       {Z_(i + 1) = Z_i}
       {Nothing}
      end;

     if (V[15] and $01) <> 0 then
      begin
       {V_(i + 1) = (V_i >> 1) XOR R}
       GCMShiftRightBlock(@V);

       {R = 11100001 || 0^120}
       V[0]:=V[0] xor $e1;
      end
     else
      begin
       {V_(i + 1) = V_i >> 1}
       GCMShiftRightBlock(@V);
      end;
    end;
  end;
end;

{==============================================================================}

procedure GCMGHashStart(Y:PByte);
{Internal function used by AESGCMEncryptData/AESGCMDecryptData}
begin
 {}
 {Y_0 = 0^128}
 FillChar(Y^,AES_BLOCK_SIZE,0);
end;

{==============================================================================}

procedure GCMGHash(const H,X:PByte;XSize:PtrUInt;Y:PByte);
{Internal function used by AESGCMEncryptData/AESGCMDecryptData}
var
 I:Integer;
 M:PtrUInt;
 Last:PtrUInt;
 Temp:array[0..AES_BLOCK_SIZE - 1] of Byte;
 XPos:PByte;
begin
 {}
 M:=XSize div 16;
 XPos:=X;

 for I := 0 to M - 1 do
  begin
   {Y_i = (Y^(i-1) XOR X_i) dot H}
   GCMXORBlock(Y,XPos);

   Inc(XPos,AES_BLOCK_SIZE);

   {Dot operation, multiplication operation for binary Galois (finite) field of 2^128 elements}
   GCMGFMult(Y,H,@Temp);
   System.Move(Temp,Y^,AES_BLOCK_SIZE);
  end;

 if (PtrUInt(X) + XSize) > PtrUInt(XPos) then
  begin
   {Add zero padded last block}
   Last:=(PtrUInt(X) + XSize) - PtrUInt(XPos);
   System.Move(XPos^,Temp,Last);
   FillChar(Pointer(PtrUInt(@Temp) + Last)^,AES_BLOCK_SIZE - Last,0);

   {Y_i = (Y^(i-1) XOR X_i) dot H}
   GCMXORBlock(Y,@Temp);

   {Dot operation, multiplication operation for binary Galois (finite) field of 2^128 elements}
   GCMGFMult(Y,H,@Temp);
   System.Move(Temp,Y^,AES_BLOCK_SIZE);
  end;

 {Return Y_m}
end;

{==============================================================================}

procedure AESGCTR(AESKey:PAESKey;const ICB,X:PByte;XSize:PtrUInt;Y:PByte);
{Internal function used by AESGCMEncryptData/AESGCMDecryptData}
var
 I:Integer;
 N:PtrUInt;
 Last:PtrUInt;
 CB:array[0..AES_BLOCK_SIZE - 1] of Byte;
 Temp:array[0..AES_BLOCK_SIZE - 1] of Byte;
 XPos:PByte;
 YPos:PByte;
begin
 {}
 XPos:=X;
 YPos:=Y;

 {Check Size}
 if XSize = 0 then Exit;

 N:=XSize div 16;

 System.Move(ICB^,CB,AES_BLOCK_SIZE);

 {Full Blocks}
 for I := 0 to N - 1 do
  begin
   if X = Y then
    begin
     AESEncryptBlock(@CB,@Temp,AESKey);
     GCMXORBlocks(YPos,XPos,@Temp);
    end
   else
    begin
     AESEncryptBlock(@CB,YPos,AESKey);
     GCMXORBlock(YPos,XPos);
    end;

   Inc(XPos,AES_BLOCK_SIZE);
   Inc(YPos,AES_BLOCK_SIZE);

   GCMInc32(@CB);
  end;

 Last:=PtrUInt(X) + XSize - PtrUInt(XPos);
 if Last > 0 then
  begin
   {Last, partial block}
   AESEncryptBlock(@CB,@Temp,AESKey);
   for I := 0 to Last - 1 do
    begin
     YPos^:=XPos^ xor Temp[I];

     Inc(XPos); {Increment PByte increments by SizeOf(Byte)}
     Inc(YPos); {Increment PByte increments by SizeOf(Byte)}
    end;
  end;
end;

{==============================================================================}

function AESGCMHashSubkey(Key:Pointer;KeySize:LongWord;AESKey:PAESKey;H:PByte):Boolean;
{Internal function used by AESGCMEncryptData/AESGCMDecryptData}
begin
 {}
 Result:=False;

 {Setup Keys}
 if not AESKeySetup(Key,KeySize,AESKey) then Exit;

 {Generate hash subkey H = AES_K(0^128)}
 FillChar(H^,AES_BLOCK_SIZE,0);
 AESEncryptBlock(H,H,AESKey);

 Result:=True;
end;

{==============================================================================}

procedure AESGCMPrepareJ0(const IV:PByte;IVSize:PtrUInt;const H:PByte;J0:PByte);
{Internal function used by AESGCMEncryptData/AESGCMDecryptData}
var
 LEN:array[0..AES_BLOCK_SIZE - 1] of Byte;
begin
 {}
 if IVSize = 12 then
  begin
   {Prepare block J_0 = IV || 0^31 || 1 [len(IV) = 96]}
   System.Move(IV^,J0^,IVSize);
   FillChar(PByte(PtrUInt(J0) + IVSize)^,AES_BLOCK_SIZE - IVSize,0);
   J0[AES_BLOCK_SIZE - 1]:=$01;
  end
 else
  begin
   {s = 128 * ceil(len(IV)/128) - len(IV)
		J_0 = GHASH_H(IV || 0^(s+64) || [len(IV)]_64)}

   GCMGHashStart(J0);
   GCMGHash(H,IV,IVSize,J0);
   BEToQWord(0,@LEN);
   BEToQWord(IVSize * 8,PByte(PtrUInt(@LEN) + 8));
   GCMGHash(H,@LEN,AES_BLOCK_SIZE,J0);
  end;
end;

{==============================================================================}

procedure AESGCMGCTR(AESKey:PAESKey;const J0,Input:PByte;Size:PtrUInt;Output:PByte);
{Internal function used by AESGCMEncryptData/AESGCMDecryptData}
var
 J0Inc:array[0..AES_BLOCK_SIZE - 1] of Byte;
begin
 {}
 {Check Size}
 if Size = 0 then Exit;

 System.Move(J0^,J0Inc,AES_BLOCK_SIZE);
 GCMInc32(@J0Inc);
 AESGCTR(AESKey,@J0Inc,Input,Size,Output);
end;

{==============================================================================}

procedure AESGCMGHash(const H,AAD:PByte;AADSize:PtrUInt;const Crypt:PByte;CryptSize:PtrUInt;S:PByte);
{Internal function used by AESGCMEncryptData/AESGCMDecryptData}
var
 LEN:array[0..AES_BLOCK_SIZE - 1] of Byte;
begin
 {}
 {u = 128 * ceil[len(C)/128] - len(C)
	v = 128 * ceil[len(A)/128] - len(A)
	S = GHASH_H(A || 0^v || C || 0^u || [len(A)]64 || [len(C)]64)
	(i.e., zero padded to block size A || C and lengths of each in bits)}

 GCMGHashStart(S);
 GCMGHash(H,AAD,AADSize,S);
 GCMGHash(H,Crypt,CryptSize,S);
 BEToQWord(AADSize * 8,@LEN);
 BEToQWord(CryptSize * 8,PByte(PtrUInt(@LEN) + 8));
 GCMGHash(H,@LEN,AES_BLOCK_SIZE,S);
end;

{==============================================================================}

function AESGCMEncryptData(Key:Pointer;KeySize:LongWord;IV,AAD,Plain,Crypt:Pointer;IVSize,AADSize,Size:LongWord;Tag:Pointer):Boolean;
{Encrypt a block of data with the supplied Key, IV and AAD using AES GCM mode, return the authentication Tag}
{Plain text and Crypt data pointers must be the same length (and can point to the same value)}
{Tag must be 16 bytes (128 bits) long}
var
 AESKey:TAESKey;
 H:array[0..AES_BLOCK_SIZE - 1] of Byte;
 S:array[0..AES_BLOCK_SIZE - 1] of Byte;
 J0:array[0..AES_BLOCK_SIZE - 1] of Byte;
begin
 {}
 Result:=False;

 {Check Params}
 if Key = nil then Exit;
 if KeySize = 0 then Exit;
 if IV = nil then Exit;
 if AAD = nil then Exit;
 if (Plain = nil) and (Crypt <> nil) then Exit; {Allow for GMAC}
 if (Crypt = nil) and (Plain <> nil) then Exit; {Allow for GMAC}
 if IVSize = 0 then Exit;
 if AADSize = 0 then Exit;
 if (Plain <> nil) and (Size = 0) then Exit;    {Allow for GMAC}
 if Tag = nil then Exit;

 {Setup Keys}
 if not AESGCMHashSubkey(Key,KeySize,@AESKey,@H) then Exit;

 AESGCMPrepareJ0(IV,IVSize,@H,@J0);

 {C = GCTR_K(inc_32(J_0), P)}
 AESGCMGCTR(@AESKey,@J0,Plain,Size,Crypt);

 AESGCMGHash(@H,AAD,AADSize,Crypt,Size,@S);

 {T = MSB_t(GCTR_K(J_0, S))}
 AESGCTR(@AESKey,@J0,@S,AES_BLOCK_SIZE,Tag);

 Result:=True;
end;

{==============================================================================}

function AESGCMDecryptData(Key:Pointer;KeySize:LongWord;IV,AAD,Crypt,Plain:Pointer;IVSize,AADSize,Size:LongWord;const Tag:Pointer):Boolean;
{Decrypt a block of data with the supplied Key, IV and AAD using AES GCM mode, validate the authentication Tag}
{Plain text and Crypt data pointers must be the same length (and can point to the same value)}
{Tag must be 16 bytes (128 bits) long}
var
 AESKey:TAESKey;
 H:array[0..AES_BLOCK_SIZE - 1] of Byte;
 S:array[0..AES_BLOCK_SIZE - 1] of Byte;
 T:array[0..AES_BLOCK_SIZE - 1] of Byte;
 J0:array[0..AES_BLOCK_SIZE - 1] of Byte;
begin
 {}
 Result:=False;

 {Check Params}
 if Key = nil then Exit;
 if KeySize = 0 then Exit;
 if IV = nil then Exit;
 if AAD = nil then Exit;
 if Plain = nil then Exit;
 if Crypt = nil then Exit;
 if IVSize = 0 then Exit;
 if AADSize = 0 then Exit;
 if Size = 0 then Exit;
 if Tag = nil then Exit;

 {Setup Keys}
 if not AESGCMHashSubkey(Key,KeySize,@AESKey,@H) then Exit;

 AESGCMPrepareJ0(IV,IVSize,@H,@J0);

 {P = GCTR_K(inc_32(J_0), C)}
 AESGCMGCTR(@AESKey,@J0,Crypt,Size,Plain);

 AESGCMGHash(@H,AAD,AADSize,Crypt,Size,@S);

 {T' = MSB_t(GCTR_K(J_0, S))}
 AESGCTR(@AESKey,@J0,@S,AES_BLOCK_SIZE,@T);

 if not CompareMem(Tag,@T,AES_BLOCK_SIZE) then Exit;

 Result:=True;
end;

{==============================================================================}

function AESGCMGMAC(Key:Pointer;KeySize:LongWord;IV,AAD:Pointer;IVSize,AADSize:LongWord;Tag:Pointer):Boolean;
{Generate the GMAC authentication Tag for a given Key, IV and AAD using AES GCM mode}
{Tag must be 16 bytes (128 bits) long}
begin
 {}
 Result:=AESGCMEncryptData(Key,KeySize,IV,AAD,nil,nil,IVSize,AADSize,0,Tag);
end;

{==============================================================================}
{==============================================================================}
{DES Functions}
function DESEncryptData(Key:Pointer;KeySize:LongWord;Vector,Plain,Crypt:Pointer;Size:LongWord):Boolean;
var
 I:LongWord;
 J:LongWord;
 Blocks:LongWord;
 EncryptKey:TDESKey;
 DecryptKey:TDESKey;
 CBC:array[0..DES_BLOCK_SIZE - 1] of Byte;
begin
 {}
 Result:=False;

 if Key = nil then Exit;
 if KeySize = 0 then Exit;
 if Plain = nil then Exit;
 if Crypt = nil then Exit;
 if Size = 0 then Exit;

 {Check Size}
 if (Size mod DES_BLOCK_SIZE) <> 0 then Exit;

 {Setup Keys}
 if not DESKeySetup(Key,KeySize,@EncryptKey,@DecryptKey) then Exit;

 {Check Vector}
 if Vector = nil then
  begin
   {Get Blocks}
   Blocks:=Size div DES_BLOCK_SIZE;

   //To Do //ECB mode
  end
 else
  begin
   {Copy Vector}
   System.Move(Vector^,CBC[0],DES_BLOCK_SIZE);

   {Get Blocks}
   Blocks:=Size div DES_BLOCK_SIZE;

   {Process Blocks}
   for I:=0 to Blocks - 1 do
    begin
     {Update Vector}
     for J:=0 to DES_BLOCK_SIZE - 1 do
      begin
       CBC[J]:=CBC[J] xor PByte(Plain)[J];
      end;

     {Encrypt Block}
     DESEncryptBlock(@CBC[0],@CBC[0],@EncryptKey);

     {Copy Crypt}
     System.Move(CBC[0],Crypt^,DES_BLOCK_SIZE);

     {Update Pointers}
     Inc(Plain,DES_BLOCK_SIZE); {Increment untyped pointer increments in bytes}
     Inc(Crypt,DES_BLOCK_SIZE); {Increment untyped pointer increments in bytes}
    end;

   {Zero Keys and Vector}
   FillChar(EncryptKey,SizeOf(TDESKey),0);
   FillChar(DecryptKey,SizeOf(TDESKey),0);
   FillChar(CBC,SizeOf(CBC),0);

   Result:=True;
  end;
end;

{==============================================================================}

function DESDecryptData(Key:Pointer;KeySize:LongWord;Vector,Crypt,Plain:Pointer;Size:LongWord):Boolean;
var
 I:LongWord;
 J:LongWord;
 Blocks:LongWord;
 EncryptKey:TDESKey;
 DecryptKey:TDESKey;
 CBC:array[0..DES_BLOCK_SIZE - 1] of Byte;
 Temp:array[0..DES_BLOCK_SIZE - 1] of Byte;
begin
 {}
 Result:=False;

 if Key = nil then Exit;
 if KeySize = 0 then Exit;
 if Crypt = nil then Exit;
 if Plain = nil then Exit;
 if Size = 0 then Exit;

 {Check Size}
 if (Size mod DES_BLOCK_SIZE) <> 0 then Exit;

 {Setup Keys}
 if not DESKeySetup(Key,KeySize,@EncryptKey,@DecryptKey) then Exit;

 {Check Vector}
 if Vector = nil then
  begin
   {Get Blocks}
   Blocks:=Size div DES_BLOCK_SIZE;

   //To Do //ECB mode
  end
 else
  begin
   {Copy Vector}
   System.Move(Vector^,CBC[0],DES_BLOCK_SIZE);

   {Get Blocks}
   Blocks:=Size div DES_BLOCK_SIZE;

   {Process Blocks}
   for I:=0 to Blocks - 1 do
    begin
     {Copy Crypt}
     System.Move(Crypt^,Temp[0],DES_BLOCK_SIZE);

     {Decrypt Block}
     DESDecryptBlock(Crypt,Plain,@DecryptKey);

     {Update Vector}
     for J:=0 to DES_BLOCK_SIZE - 1 do
      begin
       PByte(Plain)[J]:=PByte(Plain)[J] xor CBC[J];
      end;

     {Copy Vector}
     System.Move(Temp[0],CBC[0],DES_BLOCK_SIZE);

     {Update Pointers}
     Inc(Crypt,DES_BLOCK_SIZE); {Increment untyped pointer increments in bytes}
     Inc(Plain,DES_BLOCK_SIZE); {Increment untyped pointer increments in bytes}
    end;

   {Zero Keys and Vector}
   FillChar(EncryptKey,SizeOf(TDESKey),0);
   FillChar(DecryptKey,SizeOf(TDESKey),0);
   FillChar(Temp,SizeOf(Temp),0);
   FillChar(CBC,SizeOf(CBC),0);

   Result:=True;
  end;
end;

{==============================================================================}
{==============================================================================}
{3DES Functions}
function DES3EncryptData(Key:Pointer;KeySize:LongWord;Vector,Plain,Crypt:Pointer;Size:LongWord):Boolean;
var
 I:LongWord;
 J:LongWord;
 Blocks:LongWord;
 DES3Key:TDES3Key;
 CBC:array[0..DES3_BLOCK_SIZE - 1] of Byte;
begin
 {}
 Result:=False;

 if Key = nil then Exit;
 if KeySize = 0 then Exit;
 if Plain = nil then Exit;
 if Crypt = nil then Exit;
 if Size = 0 then Exit;

 {Check Size}
 if (Size mod DES3_BLOCK_SIZE) <> 0 then Exit;

 {Setup Keys}
 if not DES3KeySetup(Key,KeySize,@DES3Key) then Exit;

 {Check Vector}
 if Vector = nil then
  begin
   {Get Blocks}
   Blocks:=Size div DES3_BLOCK_SIZE;

   //To Do //ECB mode
  end
 else
  begin
   {Copy Vector}
   System.Move(Vector^,CBC[0],DES3_BLOCK_SIZE);

   {Get Blocks}
   Blocks:=Size div DES3_BLOCK_SIZE;

   {Process Blocks}
   for I:=0 to Blocks - 1 do
    begin
     {Update Vector}
     for J:=0 to DES3_BLOCK_SIZE - 1 do
      begin
       CBC[J]:=CBC[J] xor PByte(Plain)[J];
      end;

     {Encrypt Block}
     DES3EncryptBlock(@CBC[0],@CBC[0],@DES3Key);

     {Copy Crypt}
     System.Move(CBC[0],Crypt^,DES3_BLOCK_SIZE);

     {Update Pointers}
     Inc(Plain,DES3_BLOCK_SIZE); {Increment untyped pointer increments in bytes}
     Inc(Crypt,DES3_BLOCK_SIZE); {Increment untyped pointer increments in bytes}
    end;

   {Zero Keys and Vector}
   FillChar(DES3Key,SizeOf(TDES3Key),0);
   FillChar(CBC,SizeOf(CBC),0);

   Result:=True;
  end;
end;

{==============================================================================}

function DES3DecryptData(Key:Pointer;KeySize:LongWord;Vector,Crypt,Plain:Pointer;Size:LongWord):Boolean;
var
 I:LongWord;
 J:LongWord;
 Blocks:LongWord;
 DES3Key:TDES3Key;
 CBC:array[0..DES3_BLOCK_SIZE - 1] of Byte;
 Temp:array[0..DES3_BLOCK_SIZE - 1] of Byte;
begin
 {}
 Result:=False;

 if Key = nil then Exit;
 if KeySize = 0 then Exit;
 if Crypt = nil then Exit;
 if Plain = nil then Exit;
 if Size = 0 then Exit;

 {Check Size}
 if (Size mod DES3_BLOCK_SIZE) <> 0 then Exit;

 {Setup Keys}
 if not DES3KeySetup(Key,KeySize,@DES3Key) then Exit;

 {Check Vector}
 if Vector = nil then
  begin
   {Get Blocks}
   Blocks:=Size div DES3_BLOCK_SIZE;

   //To Do //ECB mode
  end
 else
  begin
   {Copy Vector}
   System.Move(Vector^,CBC[0],DES3_BLOCK_SIZE);

   {Get Blocks}
   Blocks:=Size div DES3_BLOCK_SIZE;

   {Process Blocks}
   for I:=0 to Blocks - 1 do
    begin
     {Copy Crypt}
     System.Move(Crypt^,Temp[0],DES3_BLOCK_SIZE);

     {Decrypt Block}
     DES3DecryptBlock(Crypt,Plain,@DES3Key);

     {Update Vector}
     for J:=0 to DES3_BLOCK_SIZE - 1 do
      begin
       PByte(Plain)[J]:=PByte(Plain)[J] xor CBC[J];
      end;

     {Copy Vector}
     System.Move(Temp[0],CBC[0],DES3_BLOCK_SIZE);

     {Update Pointers}
     Inc(Crypt,DES3_BLOCK_SIZE); {Increment untyped pointer increments in bytes}
     Inc(Plain,DES3_BLOCK_SIZE); {Increment untyped pointer increments in bytes}
    end;

   {Zero Keys and Vector}
   FillChar(DES3Key,SizeOf(TDES3Key),0);
   FillChar(Temp,SizeOf(Temp),0);
   FillChar(CBC,SizeOf(CBC),0);

   Result:=True;
  end;
end;

{==============================================================================}
{==============================================================================}
{RC4 Functions}
function RC4EncryptData(Key:Pointer;KeySize:LongWord;Plain,Crypt:Pointer;Size,Start:LongWord):Boolean;
{Encrypt the supplied data with a key using the RC4 cipher algorithm}
{Start specifies how many bytes of the RC4 cipher stream to skip to allow for previous blocks of data
 or to comply with RFC4345 requirements to discard the first 1536 bytes of the RC4 cipher stream}

 procedure RC4SwapState(A,B:LongWord;State:PRC4State); inline;
 var
  Temp:Byte;
 begin
  {}
  Temp:=State[A];
  State[A]:=State[B];
  State[B]:=Temp;
 end;

var
 I:LongWord;
 J:LongWord;
 K:LongWord;
 State:TRC4State;
 Offset:LongWord;
 PlainBuffer:PByte;
 CryptBuffer:PByte;
begin
 {}
 Result:=False;

 if Key = nil then Exit;
 if KeySize = 0 then Exit;
 if Plain = nil then Exit;
 if Crypt = nil then Exit;
 if Size = 0 then Exit;

 {Create state (KSA)}
 for I:=0 to 255 do
  begin
   State[I]:=I;
  end;

 J:=0;
 Offset:=0;
 for I:=0 to 255 do
  begin
   J:=(J + State[I] + PByte(Key)[Offset]) and $FF;

   Inc(Offset);
   if Offset >= KeySize then Offset:=0;

   RC4SwapState(I,J,@State);
  end;

 {Skip start bytes of the RC4 stream (PRGA)}
 I:=0;
 J:=0;
 if Start > 0 then
  begin
   for K:=0 to Start - 1 do
    begin
     I:=(I + 1) and $FF;
     J:=(J + State[I]) and $FF;

     RC4SwapState(I,J,@State);
    end;
  end;

 {Encrypt the data with the RC4 stream (PRGA)}
 PlainBuffer:=Plain;
 CryptBuffer:=Crypt;
 for K:=0 to Size - 1 do
  begin
   I:=(I + 1) and $FF;
   J:=(J + State[I]) and $FF;

   RC4SwapState(I,J,@State);

   CryptBuffer^:=PlainBuffer^ xor State[(State[I] + State[J]) and $FF];

   Inc(PlainBuffer);
   Inc(CryptBuffer);
  end;

 Result:=True;
end;

{==============================================================================}

function RC4DecryptData(Key:Pointer;KeySize:LongWord;Crypt,Plain:Pointer;Size,Start:LongWord):Boolean; inline;
{Encrypt the supplied data with a key using the RC4 cipher algorithm}
begin
 {}
 Result:=RC4EncryptData(Key,KeySize,Crypt,Plain,Size,Start);
end;

{==============================================================================}
{==============================================================================}
{SHA1 Functions}
function SHA1DigestData(Data:PSHA1Block;Digest:PSHA1Digest):Boolean;
{Generate a 160 bit SHA1 digest (Hash) from the supplied data}
{Data is a linked list which can contain multiple independent blocks to be
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
{Data is a linked list which can contain multiple independent blocks to be
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
{SHA384 Functions}
function SHA384DigestData(Data:PSHA384Block;Digest:PSHA384Digest):Boolean;
{Generate a 384 bit SHA384 digest (Hash) from the supplied data}
{Data is a linked list which can contain multiple independent blocks to be
 included in the hash. The data block itself does not form part of the hash}
var
 Block:PSHA384Block;
 Context:TSHA384Context;
begin
 {}
 Result:=False;

 {Check Params}
 if Data = nil then Exit;
 if Digest = nil then Exit;

 {Init Context}
 SHA384Init(Context);

 {Get Block}
 Block:=Data;
 while Block <> nil do
  begin
   {Add Data}
   SHA384Process(Context,Block.Data,Block.Size);

   {Get Next}
   Block:=Block.Next;
  end;

 {Return Digest}
 SHA384Complete(Context,Digest^);

 Result:=True;
end;

{==============================================================================}

function SHA384DigestString(const Value:String;Digest:PSHA384Digest):Boolean;
{Generate a 384 bit SHA384 digest (Hash) from the supplied string value}
var
 Context:TSHA384Context;
begin
 {}
 Result:=False;

 {Check Params}
 {if Length(Value) = 0 then Exit;} {Allow blank string}
 if Digest = nil then Exit;

 {Init Context}
 SHA384Init(Context);

 {Add Data}
 SHA384Process(Context,PChar(Value),Length(Value));

 {Return Digest}
 SHA384Complete(Context,Digest^);

 Result:=True;
end;

{==============================================================================}

function HMACSHA384DigestData(const Key:String;Data:PSHA384Block;Digest:PSHA384Digest):Boolean;
{Generate a SHA384 HMAC (Hashed Message Authentication Code) using the Key and Data}
{The SHA384 HMAC algorithm is:

 SHA384(Key xor oPad, SHA384(Key xor iPad, Data))

 Where iPad is the byte $36 repeated 128 times
       oPad is the byte $5c repeated 128 times

 If Key is more than 128 bytes it will be hashed to Key = SHA384(Key) instead
 If Key is less than 128 bytes it will be padded with zeros

}
var
 Count:LongWord;
 Block:TSHA384Block;
 KeyBlock:TSHA384Block;
 KeyLength:PtrUInt;
 KeyDigest:TSHA384Digest;
 KeyBuffer:TSHA384ByteBuffer;
 PadBuffer:TSHA384ByteBuffer;
begin
 {}
 Result:=False;

 {Check Params}
 if Data = nil then Exit;
 if Digest = nil then Exit;

 {Get key length}
 KeyLength:=Length(Key);

 {Check key length}
 if KeyLength > 128 then
  begin
   {SHA384 the key}
   if not SHA384DigestString(Key,@KeyDigest) then Exit;

   {Zero key buffer}
   FillChar(KeyBuffer[0],SizeOf(TSHA384ByteBuffer),0);

   {Copy digest to key buffer}
   System.Move(KeyDigest[0],KeyBuffer[0],48);

   {Update key length}
   KeyLength:=48;
  end
 else
  begin
   {Zero key buffer}
   FillChar(KeyBuffer[0],SizeOf(TSHA384ByteBuffer),0);

   {Copy key to key buffer}
   System.Move(PChar(Key)^,KeyBuffer[0],KeyLength);
  end;

 {XOR the key buffer with the iPad value}
 for Count:=0 to 127 do
  begin
   PadBuffer[Count]:=KeyBuffer[Count] xor $36;
  end;

 {SHA384 the key buffer and the data (Inner)}
 KeyBlock.Data:=@PadBuffer;
 KeyBlock.Size:=128;
 KeyBlock.Next:=Data;

 if not SHA384DigestData(@KeyBlock,Digest) then Exit;

 {XOR the key buffer with the oPad value}
 for Count:=0 to 127 do
  begin
   PadBuffer[Count]:=KeyBuffer[Count] xor $5c;
  end;

 {SHA384 the key buffer and the result of the inner SHA384 (Outer)}
 KeyBlock.Data:=@PadBuffer;
 KeyBlock.Size:=128;
 KeyBlock.Next:=@Block;

 Block.Data:=Digest;
 Block.Size:=48;
 Block.Next:=nil;

 if not SHA384DigestData(@KeyBlock,Digest) then Exit;

 {Wipe the buffers to prevent leakage}
 FillChar(KeyDigest,SizeOf(TSHA384Digest),0);
 FillChar(KeyBuffer,SizeOf(TSHA384ByteBuffer),0);
 FillChar(PadBuffer,SizeOf(TSHA384ByteBuffer),0);

 Result:=True;
end;

{==============================================================================}

function HMACSHA384DigestString(const Key,Value:String;Digest:PSHA384Digest):Boolean;
{Generate a SHA384 HMAC (Hashed Message Authentication Code) using the Key and Value}
var
 Data:TSHA384Block;
begin
 {}
 Data.Data:=PChar(Value);
 Data.Size:=Length(Value);
 Data.Next:=nil;

 Result:=HMACSHA384DigestData(Key,@Data,Digest);
end;

{==============================================================================}
{==============================================================================}
{SHA512 Functions}
function SHA512DigestData(Data:PSHA512Block;Digest:PSHA512Digest):Boolean;
{Generate a 512 bit SHA512 digest (Hash) from the supplied data}
{Data is a linked list which can contain multiple independent blocks to be
 included in the hash. The data block itself does not form part of the hash}
var
 Block:PSHA512Block;
 Context:TSHA512Context;
begin
 {}
 Result:=False;

 {Check Params}
 if Data = nil then Exit;
 if Digest = nil then Exit;

 {Init Context}
 SHA512Init(Context);

 {Get Block}
 Block:=Data;
 while Block <> nil do
  begin
   {Add Data}
   SHA512Process(Context,Block.Data,Block.Size);

   {Get Next}
   Block:=Block.Next;
  end;

 {Return Digest}
 SHA512Complete(Context,Digest^);

 Result:=True;
end;

{==============================================================================}

function SHA512DigestString(const Value:String;Digest:PSHA512Digest):Boolean;
{Generate a 512 bit SHA512 digest (Hash) from the supplied string value}
var
 Context:TSHA512Context;
begin
 {}
 Result:=False;

 {Check Params}
 {if Length(Value) = 0 then Exit;} {Allow blank string}
 if Digest = nil then Exit;

 {Init Context}
 SHA512Init(Context);

 {Add Data}
 SHA512Process(Context,PChar(Value),Length(Value));

 {Return Digest}
 SHA512Complete(Context,Digest^);

 Result:=True;
end;

{==============================================================================}

function HMACSHA512DigestData(const Key:String;Data:PSHA512Block;Digest:PSHA512Digest):Boolean;
{Generate a SHA512 HMAC (Hashed Message Authentication Code) using the Key and Data}
{The SHA512 HMAC algorithm is:

 SHA512(Key xor oPad, SHA512(Key xor iPad, Data))

 Where iPad is the byte $36 repeated 128 times
       oPad is the byte $5c repeated 128 times

 If Key is more than 128 bytes it will be hashed to Key = SHA512(Key) instead
 If Key is less than 128 bytes it will be padded with zeros

}
var
 Count:LongWord;
 Block:TSHA512Block;
 KeyBlock:TSHA512Block;
 KeyLength:PtrUInt;
 KeyDigest:TSHA512Digest;
 KeyBuffer:TSHA512ByteBuffer;
 PadBuffer:TSHA512ByteBuffer;
begin
 {}
 Result:=False;

 {Check Params}
 if Data = nil then Exit;
 if Digest = nil then Exit;

 {Get key length}
 KeyLength:=Length(Key);

 {Check key length}
 if KeyLength > 128 then
  begin
   {SHA512 the key}
   if not SHA512DigestString(Key,@KeyDigest) then Exit;

   {Zero key buffer}
   FillChar(KeyBuffer[0],SizeOf(TSHA512ByteBuffer),0);

   {Copy digest to key buffer}
   System.Move(KeyDigest[0],KeyBuffer[0],64);

   {Update key length}
   KeyLength:=64;
  end
 else
  begin
   {Zero key buffer}
   FillChar(KeyBuffer[0],SizeOf(TSHA512ByteBuffer),0);

   {Copy key to key buffer}
   System.Move(PChar(Key)^,KeyBuffer[0],KeyLength);
  end;

 {XOR the key buffer with the iPad value}
 for Count:=0 to 127 do
  begin
   PadBuffer[Count]:=KeyBuffer[Count] xor $36;
  end;

 {SHA512 the key buffer and the data (Inner)}
 KeyBlock.Data:=@PadBuffer;
 KeyBlock.Size:=128;
 KeyBlock.Next:=Data;

 if not SHA512DigestData(@KeyBlock,Digest) then Exit;

 {XOR the key buffer with the oPad value}
 for Count:=0 to 127 do
  begin
   PadBuffer[Count]:=KeyBuffer[Count] xor $5c;
  end;

 {SHA512 the key buffer and the result of the inner SHA512 (Outer)}
 KeyBlock.Data:=@PadBuffer;
 KeyBlock.Size:=128;
 KeyBlock.Next:=@Block;

 Block.Data:=Digest;
 Block.Size:=64;
 Block.Next:=nil;

 if not SHA512DigestData(@KeyBlock,Digest) then Exit;

 {Wipe the buffers to prevent leakage}
 FillChar(KeyDigest,SizeOf(TSHA512Digest),0);
 FillChar(KeyBuffer,SizeOf(TSHA512ByteBuffer),0);
 FillChar(PadBuffer,SizeOf(TSHA512ByteBuffer),0);

 Result:=True;
end;

{==============================================================================}

function HMACSHA512DigestString(const Key,Value:String;Digest:PSHA512Digest):Boolean;
{Generate a SHA512 HMAC (Hashed Message Authentication Code) using the Key and Value}
var
 Data:TSHA512Block;
begin
 {}
 Data.Data:=PChar(Value);
 Data.Size:=Length(Value);
 Data.Next:=nil;

 Result:=HMACSHA512DigestData(Key,@Data,Digest);
end;

{==============================================================================}
{==============================================================================}
{RSA Functions}
function RSAInitPrivateKey(Modulus,PublicExp,PrivateExp,P,Q,DP,DQ,QInv:PByte;ModulusLen,PublicExpLen,PrivateExpLen,PLen,QLen,DPLen,DQLen,QInvLen:Integer):PRSAContext;
begin
 {}
 {Initialize Public Key}
 Result:=RSAInitPublicKey(Modulus,PublicExp,ModulusLen,PublicExpLen);
 if Result = nil then Exit;

 if PrivateExp = nil then Exit;
 if P = nil then Exit;
 if Q = nil then Exit;
 if DP = nil then Exit;
 if DQ = nil then Exit;
 if QInv = nil then Exit;

 {Import Private Exponent}
 Result.D:=BIImport(Result.Context,PrivateExp,PrivateExpLen);
 BIPermanent(Result.D);

 {Import P, Q DP, DP, QInv}
 Result.P:=BIImport(Result.Context,P,PLen);
 Result.Q:=BIImport(Result.Context,Q,QLen);
 Result.DP:=BIImport(Result.Context,DP,DPLen);
 Result.DQ:=BIImport(Result.Context,DQ,DQLen);
 Result.QInv:=BIImport(Result.Context,QInv,QInvLen);

 BIPermanent(Result.DP);
 BIPermanent(Result.DQ);
 BIPermanent(Result.QInv);
 BISetMod(Result.Context,Result.P,BIGINT_P_OFFSET);
 BISetMod(Result.Context,Result.Q,BIGINT_Q_OFFSET);
end;

{==============================================================================}

function RSAInitPublicKey(Modulus,PublicExp:PByte;ModulusLen,PublicExpLen:Integer):PRSAContext;
var
 Context:PRSAContext;
begin
 {}
 Result:=nil;

 if Modulus = nil then Exit;
 if PublicExp = nil then Exit;

 {Allocate Context}
 Context:=AllocMem(SizeOf(TRSAContext));
 if Context = nil then Exit;

 {Initialize BigInt}
 Context.Context:=BIInitialize;
 if Context.Context = nil then
  begin
   FreeMem(Context);
   Exit;
  end;

 {Import Modulus}
 Context.ModulusLen:=ModulusLen;
 Context.M:=BIImport(Context.Context,Modulus,ModulusLen);
 BISetMod(Context.Context,Context.M,BIGINT_M_OFFSET);

 {Import Public Exponent}
 Context.E:=BIImport(Context.Context,PublicExp,PublicExpLen);
 BIPermanent(Context.E);

 Result:=Context;
end;

{==============================================================================}

function RSAFreeKey(Context:PRSAContext):Boolean;
{Free an RSA context containing Private and/or Public keys}
begin
 {}
 Result:=False;

 if Context = nil then Exit;

 {Free Public Exponent}
 BIDepermanent(Context.e);
 BIFree(Context.Context,Context.e);

 {Free Modulus}
 BIFreeMod(Context.Context,BIGINT_M_OFFSET);

 {Check Private Exponent}
 if Context.D <> nil then
  begin
   {Free Private Exponent}
   BIDepermanent(Context.D);
   BIFree(Context.Context,Context.D);

   {Free DP, DQ, QInv}
   BIDepermanent(Context.DP);
   BIDepermanent(Context.DQ);
   BIDepermanent(Context.QInv);

   BIFree(Context.Context,Context.DP);
   BIFree(Context.Context,Context.DQ);
   BIFree(Context.Context,Context.QInv);

   {Free P, Q}
   BIFreeMod(Context.Context,BIGINT_P_OFFSET);
   BIFreeMod(Context.Context,BIGINT_Q_OFFSET);
  end;

 {Free Context}
 BITerminate(Context.Context);
 FreeMem(Context);

 Result:=True;
end;

{==============================================================================}

function RSAEncryptSign(Context:PRSAContext;const Input:PByte;Len:Word;Output:PByte;Sign:Boolean):Integer;
{Perform PKCS1.5 Encryption or Signing}
{Context: The RSA context containing Private and/or Public keys}
{Input: The data to be encrypted}
{Len: The size of the input data in bytes (Must be <= Modulus length - 11 to make the padding at least 8 bytes as recommended by RFC2313)}
{Output: The buffer for the encrypted result (Must always be Modulus length)}
{Sign: If true then sign instead of encrypting}
{Return: The number of bytes encrypted or -1 on error}
var
 Size:Integer;
 Padding:Integer;
 Block:PBigInt;
 Encrypted:PBigInt;
begin
 {}
 Result:=-1;

 {Check Context}
 if Context = nil then Exit;

 {Check Parameters}
 if Input = nil then Exit;
 if Output = nil then Exit;

 {Get Size and Padding}
 Size:=Context.ModulusLen;
 Padding:=Size - Len - 3;
 if Len > Size - 11 then Exit;

 {Leading zero to ensure encryption block is less than modulus (when converted to an integer)}
 Output[0]:=0;

 {Check Sign}
 if Sign then
  begin
   {Block Type 1}
   Output[1]:=1;

   {Pad with 0xff bytes}
   FillChar(Output[2],Padding,$FF);
  end
 else
  begin
   {Block Type 2}
   Output[1]:=2;

   {Pad with random non-zero bytes}
   if not GetRandomBytesNonZero(@Output[2],Padding) then Exit;
  end;

 {Trailing zero after padding bytes}
 Output[2 + Padding]:=0;

 {Copy Input to Output}
 System.Move(Input^,PByte(@Output[3 + Padding])^,Len);

 {Encrypt the Block}
 Block:=BIImport(Context.Context,Output,Size);
 if Sign then
  begin
   {Sign with Private Key}
   Encrypted:=BICRT(Context.Context,Block,Context.DP,Context.DQ,Context.P,Context.Q,Context.QInv);
  end
 else
  begin
   {Encrypt with Public Key}
   Context.Context.ModOffset:=BIGINT_M_OFFSET;
   Encrypted:=BIModPower(Context.Context,Block,Context.E);
  end;
 BIExport(Context.Context,Encrypted,Output,Size);

 {Return Result}
 Result:=Size;
end;

{==============================================================================}

function RSADecryptVerify(Context:PRSAContext;const Input:PByte;Output:PByte;Len:Integer;Verify:Boolean):Integer;
{Perform PKCS1.5 Decryption or Verification}
{Context: The RSA context containing Private and/or Public keys}
{Input: The data to be decrypted (Must always be Modulus length)}
{Output: The buffer for the decrypted result}
{Len: The size of the output buffer in bytes}
{Verify: If true then verify instead of decrypting}
{Return: The number of bytes decrypted or -1 on error}
var
 Size:Integer;
 Count:Integer;
 Padding:Integer;
 Exported:PByte;
 Encrypted:PBigInt;
 Decrypted:PBigInt;
 Block:array[0..RSA_MODULUS_BYTES_MAX - 1] of Byte;
begin
 {}
 Result:=-1;

 {Check Context}
 if Context = nil then Exit;

 {Check Parameters}
 if Input = nil then Exit;
 if Output = nil then Exit;

 {Get Size}
 Size:=Context.ModulusLen;
 Count:=0;
 Padding:=0;

 {Decrypt the Block}
 Encrypted:=BIImport(Context.Context,Input,Size);
 if Verify then
  begin
   {Verify with Public Key}
   Context.Context.ModOffset:=BIGINT_M_OFFSET;
   Decrypted:=BIModPower(Context.Context,Encrypted,Context.E);
  end
 else
  begin
   {Decrypt with Private Key}
   Decrypted:=BICRT(Context.Context,Encrypted,Context.DP,Context.DQ,Context.P,Context.Q,Context.QInv);
  end;
 Exported:=@Block;
 if Size > RSA_MODULUS_BYTES_MAX then Exported:=GetMem(Size);
 if Exported = nil then Exit;
 try
  BIExport(Context.Context,Decrypted,Exported,Size);

  {Check Leading Zero}
  if Exported[Count] <> 0 then Exit;
  Inc(Count);

  {Check Verify}
  if Verify then
   begin
    {Check Block Type 1}
    if Exported[Count] <> 1 then Exit;
    Inc(Count);

    {Padded with 0xff bytes}
    while (Exported[Count] = $FF) and (Count < Size) do
     begin
      Inc(Count);
      Inc(Padding);
     end;
   end
  else
   begin
    {Check Block Type 2}
    if Exported[Count] <> 2 then Exit;
    Inc(Count);

    {Padded with random non-zero bytes}
    while (Exported[Count] <> 0) and (Count < Size) do
     begin
      Inc(Count);
      Inc(Padding);
     end;
   end;

  {Check trailing zero byte and padding size}
  if (Count = Size) or (Padding < 8) then Exit;
  if Exported[Count] <> 0 then Exit;
  Inc(Count);

  {Get Result}
  Result:=(Size - Count);

  {Check length}
  if Len < Result then
   begin
    Result:=-1;
    Exit;
   end;

  {Zero Output}
  if Len > Result then
   begin
    FillChar(Output[Result],Len - Result,0);
   end;

  {Copy Block to Output}
  System.Move(Exported[Count],Output^,Result);
 finally
  if Size > RSA_MODULUS_BYTES_MAX then FreeMem(Exported);
 end;
end;

{==============================================================================}
{==============================================================================}
{Random Functions}
function GetRandomBytes(Buffer:PByte;Count:Integer):Boolean;
var
 Data:PLongWord;
 Offset:LongWord;
 Remain:LongWord;
begin
 {}
 if Assigned(CryptoGetRandomBytesHandler) then
  begin
   Result:=CryptoGetRandomBytesHandler(Buffer,Count);
  end
 else
  begin
   {Default Method}
   Result:=False;
   
   if RandomAvailable then
    begin
     {Get Start}
     Offset:=0;
     Remain:=Count;
     Data:=PLongWord(Buffer);
     
     {Get Data}
     while Remain >= SizeOf(LongWord) do
      begin
       {Read LongWord}
       Data[Offset]:=RandomReadLongInt(0);
       
       {Update Position}
       Inc(Offset);
       Dec(Remain,SizeOf(LongWord));
      end;
    
     {Check Remain}
     while Remain > 0 do
      begin
       {Read Byte}
       Buffer[Count - Remain]:=RandomReadLongInt(0);
       
       {Update Position}
       Dec(Remain);
      end;
      
     Result:=True; 
    end;
  end;
end;

{==============================================================================}

function GetRandomBytesNonZero(Buffer:PByte;Count:Integer):Boolean;
var
 Counter:Integer;
begin
 {}
 Result:=GetRandomBytes(Buffer,Count);
 if not Result then Exit;

 Randomize;

 for Counter := 0 to Count - 1 do
  begin
   {Check for Zero}
   while Buffer[Counter] = 0 do
    begin
     Buffer[Counter]:=Random(256);
    end;
  end;
end;

{==============================================================================}
{==============================================================================}
{CRC Functions}
function CRC16CCITT(CRC:Word;Data:PByte;Size:LongWord):Word;

 function CRC16CCITTByte(CRC:Word;Data:Byte):Word; inline;
 begin
  {}
  Result:=(CRC shr 8) xor CRC16_CCITT[(CRC xor Data) and $FF];
 end;

begin
 {}
 Result:=0;

 if Data = nil then Exit;

 while Size > 0 do
  begin
   {CRC the next byte}
   CRC:=CRC16CCITTByte(CRC,Data^);

   {Update Size and Data}
   Dec(Size);
   Inc(Data);
  end;

 Result:=CRC;
end;

{==============================================================================}
{==============================================================================}
{Base64 Functions}
function Base64EncodeString(const Value:String):String;
{Encode a string using Base64 encoding and return the encoded result}
var
 DestLen:Integer;
begin
 {}
 Result:='';

 {Get Length}
 if Base64EncodeBuffer(PChar(Value),Length(Value),nil,DestLen) then
  begin
   {Setup Result}
   SetLength(Result,DestLen - 1); {Subtract null terminator}

   {Encode Buffer}
   if Base64EncodeBuffer(PChar(Value),Length(Value),PChar(Result),DestLen) then
    begin
     {Update Length}
     SetLength(Result,DestLen - 1); {Subtract null terminator}
    end
   else
    begin
     {Reset Result}
     SetLength(Result,0);
    end;
  end;
end;

{==============================================================================}

function Base64DecodeString(const Value:String):String;
{Decode a Base64 encoded string and return the decoded result}
var
 DestLen:Integer;
begin
 {}
 Result:='';

 {Get Length}
 if Base64DecodeBuffer(PChar(Value),Length(Value),nil,DestLen) then
  begin
   {Setup Result}
   SetLength(Result,DestLen); {Does not include null terminator}

   {Decode Buffer}
   if Base64DecodeBuffer(PChar(Value),Length(Value),PChar(Result),DestLen) then
    begin
     {Update Length}
     SetLength(Result,DestLen); {Does not include null terminator}
    end
   else
    begin
     {Reset Result}
     SetLength(Result,0);
    end;
  end;
end;

{==============================================================================}

function Base64EncodeBuffer(const Source:PChar;SourceLen:Integer;Dest:PChar;var DestLen:Integer):Boolean;
{Encode the source buffer using Base64 encoding and return the encoded result in the destination buffer}
{On success DestLen contains the actual encoded length including a null terminator}
{If Dest is nil then return success (True) with the required size in DestLen}
var
 Input:PByte;
 Output:PByte;
 Last:PByte;
 Next:PChar;
 LineLen:Integer;
 LineCount:Integer;
 OutputLen:Integer;
begin
 {}
 Result:=False;

 {Check Source}
 if Source = nil then Exit;

 {Get Output Length}
 OutputLen:=SourceLen * 4 div 3 + 4;      {Convert 3-byte blocks to 4-byte}
 Inc(OutputLen,2 * (OutputLen div 64));   {Add space for end of line (CRLF)}
 Inc(OutputLen);                          {Add null terminator}

 {Check Overflow}
 if OutputLen < SourceLen then Exit;

 {Check DestLen}
 if (OutputLen > DestLen) and (Dest <> nil) then Exit;

 {Setup DestLen}
 DestLen:=OutputLen;

 {Return Result}
 Result:=True;

 {Check Dest (Return Success)}
 if Dest = nil then Exit;

 {Get Pointers}
 Input:=PByte(Source);
 Output:=PByte(Dest);
 Last:=PByte(PtrUInt(Source) + SourceLen);
 Next:=PChar(Output);

 {Setup Start}
 LineLen:=0;
 LineCount:=1;

 {Check Remaining}
 while (Last - Input) >= 3 do
  begin
   {Encode 3 characters}
   Next^:=Base64EncodeTable[((Input[0] shr 2) and $3F) + 1];
   Inc(Next);

   Next^:=Base64EncodeTable[((((Input[0] and $03) shl 4) or (Input[1] shr 4)) and $3F) + 1];
   Inc(Next);

   Next^:=Base64EncodeTable[((((Input[1] and $0F) shl 2) or (Input[2] shr 6)) and $3F) + 1];
   Inc(Next);

   Next^:=Base64EncodeTable[(Input[2] and $3F) + 1];
   Inc(Next);

   {Update Position}
   Inc(Input,3);

   {Update Line Length}
   Inc(LineLen,4);

   {Check Line Length}
   if LineLen >= 64 then
    begin
     {Add CRLF}
     Next^:=#13;
     Inc(Next);
     Next^:=#10;
     Inc(Next);

     {Reset Line}
     LineLen:=0;

     {Update Count}
     Inc(LineCount);
    end;
  end;

 {Check Remaining}
 if (Last - Input) > 0 then
  begin
   {Encode 1 or 2 characters}
   Next^:=Base64EncodeTable[((Input[0] shr 2) and $3F) + 1];
   Inc(Next);

   if (Last - Input) = 1 then
    begin
     {Encode 1 character}
     Next^:=Base64EncodeTable[(((Input[0] and $03) shl 4) and $3F) + 1];
     Inc(Next);

     {Add Padding}
     Next^:='=';
     Inc(Next);
    end
   else
    begin
     {Encode 2 characters}
     Next^:=Base64EncodeTable[((((Input[0] and $03) shl 4) or (Input[1] shr 4)) and $3F) + 1];
     Inc(Next);

     Next^:=Base64EncodeTable[(((Input[1] and $0F) shl 2) and $3F) + 1];
     Inc(Next);
    end;

   {Add Padding}
   Next^:='=';
   Inc(Next);

   {Update Line Length}
   Inc(LineLen,4);
  end;

 {Check Line Length}
 if (LineCount > 1) and (LineLen > 0) then
  begin
   {Add CRLF}
   Next^:=#13;
   Inc(Next);
   Next^:=#10;
   Inc(Next);
  end;

 {Add Terminator}
 Next^:=#0;
 Inc(Next);

 {Finalize DestLen}
 DestLen:=Next - PChar(Output);
end;

{==============================================================================}

function Base64DecodeBuffer(const Source:PChar;SourceLen:Integer;Dest:PChar;var DestLen:Integer):Boolean;
{Decode a Base64 encoded source buffer and return the decoded result in the destination buffer}
{On success DestLen contains the actual decoded length (Null terminator is not added to this value)}
{If Dest is nil then return success (True) with the required size in DestLen}
var
 Count:Integer;
 Total:Integer;
 Input:PByte;
 Output:PByte;
 Temp:Char;
 Next:PChar;
 Padding:Integer;
 OutputLen:Integer;
 Block:array[0..3] of Char;
begin
 {}
 Result:=False;

 {Check Source}
 if Source = nil then Exit;

 {Get Input}
 Input:=PByte(Source);

 {Check Input}
 Total:=0;
 for Count:=0 to SourceLen - 1 do
  begin
   if Base64DecodeTable[Input[Count]] <> #128 then
    begin
     Inc(Total);
    end;
  end;
 if (Total = 0) or ((Total mod 4) <> 0) then Exit;

 {Get Output Length}
 OutputLen:=Total div 4 * 3;

 {Check DestLen}
 if (OutputLen > DestLen) and (Dest <> nil) then Exit;

 {Setup DestLen}
 DestLen:=OutputLen;

 {Return Result}
 Result:=True;

 {Check Dest (Return Success)}
 if Dest = nil then Exit;

 {Get Pointers}
 Output:=PByte(Dest);
 Next:=PChar(Output);

 {Setup Start}
 Padding:=0;

 Total:=0;
 for Count:=0 to SourceLen - 1 do
  begin
   Temp:=Base64DecodeTable[Input[Count]];
   if Temp <> #128 then
    begin
     if Char(Input[Count]) = '=' then Inc(Padding);

     {Update Block}
     Block[Total]:=Temp;
     Inc(Total);

     if Total = 4 then
      begin
       {Decode 4 character block}
       Next^:=Char((Ord(Block[0]) shl 2) or (Ord(Block[1]) shr 4));
       Inc(Next);

       Next^:=Char((Ord(Block[1]) shl 4) or (Ord(Block[2]) shr 2));
       Inc(Next);

       Next^:=Char((Ord(Block[2]) shl 6) or Ord(Block[3]));
       Inc(Next);

       {Reset Block}
       Total:=0;

       {Check Padding}
       if Padding > 0 then
        begin
         if Padding = 1 then
          begin
           Dec(Next);
          end
         else if Padding = 2 then
          begin
           Dec(Next,2);
          end
         else
          begin
           {Invalid padding}
           Result:=False;
           Exit;
          end;

         {Completed}
         Break;
        end;
      end;
    end;
  end;

 {Finalize DestLen}
 DestLen:=Next - PChar(Output);
end;

{==============================================================================}
{==============================================================================}
{Crypto Helper Functions}
procedure BytesToLE32(Buffer:PByte;Count:LongWord);
{Change the byte order of count longwords in the supplied buffer to little endian}
{$IFDEF FPC_BIG_ENDIAN}
var
 Value:LongWord;
begin
 {}
 while Count > 0 do
  begin
   {Reverse LongWord}
   Value:=(RolDWord(PLongWord(Buffer)^,24) and $FF00FF00) or (RolDWord(PLongWord(Buffer)^,8) and $00FF00FF);

   {Save to buffer}
   PLongWord(Buffer)^:=Value;

   {Update buffer}
   Inc(Buffer,SizeOf(LongWord)); {Increment PByte increments by SizeOf(Byte)}
   Dec(Count);
  end;
end;
{$ELSE FPC_BIG_ENDIAN}
begin
 {Nothing}
end;
{$ENDIF FPC_BIG_ENDIAN}
{==============================================================================}

procedure BytesToBE32(Buffer:PByte;Count:LongWord);
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
   Value:=(RolDWord(PLongWord(Buffer)^,24) and $FF00FF00) or (RolDWord(PLongWord(Buffer)^,8) and $00FF00FF);

   {Save to buffer}
   PLongWord(Buffer)^:=Value;

   {Update buffer}
   Inc(Buffer,SizeOf(LongWord)); {Increment PByte increments by SizeOf(Byte)}
   Dec(Count);
  end;
end;
{$ENDIF FPC_BIG_ENDIAN}
{==============================================================================}

procedure BytesToLE64(Buffer:PByte;Count:LongWord);
{Change the byte order of count quadwords in the supplied buffer to little endian}
{$IFDEF FPC_BIG_ENDIAN}
var
 Value:QWord;
begin
 {}
 while Count > 0 do
  begin
   {Reverse QWord}
   Value:=(QWord((RolDWord(PLongWord(Buffer)^,24) and $FF00FF00) or (RolDWord(PLongWord(Buffer)^,8) and $00FF00FF)) shl 32)
           or ((RolDWord(PLongWord(Buffer + 4)^,24) and $FF00FF00) or (RolDWord(PLongWord(Buffer + 4)^,8) and $00FF00FF));

   {Save to buffer}
   PQWord(Buffer)^:=Value;

   {Update buffer}
   Inc(Buffer,SizeOf(QWord)); {Increment PByte increments by SizeOf(Byte)}
   Dec(Count);
  end;
end;
{$ELSE FPC_BIG_ENDIAN}
begin
 {Nothing}
end;
{$ENDIF FPC_BIG_ENDIAN}
{==============================================================================}

procedure BytesToBE64(Buffer:PByte;Count:LongWord);
{Change the byte order of count quadwords in the supplied buffer to big endian}
{$IFDEF FPC_BIG_ENDIAN}
begin
 {Nothing}
end;
{$ELSE FPC_BIG_ENDIAN}
var
 Value:QWord;
begin
 {}
 while Count > 0 do
  begin
   {Reverse QWord}
   Value:=(QWord((RolDWord(PLongWord(Buffer)^,24) and $FF00FF00) or (RolDWord(PLongWord(Buffer)^,8) and $00FF00FF)) shl 32)
           or ((RolDWord(PLongWord(Buffer + 4)^,24) and $FF00FF00) or (RolDWord(PLongWord(Buffer + 4)^,8) and $00FF00FF));

   {Save to buffer}
   PQWord(Buffer)^:=Value;

   {Update buffer}
   Inc(Buffer,SizeOf(QWord)); {Increment PByte increments by SizeOf(Byte)}
   Dec(Count);
  end;
end;
{$ENDIF FPC_BIG_ENDIAN}
{==============================================================================}

function LongWordToBE(Buffer:PByte):LongWord; inline;
begin
 {}
 Result:=((Buffer[0] and 255) shl 24) or ((Buffer[1] and 255) shl 16) or ((Buffer[2] and 255) shl 8) or (Buffer[3] and 255);
end;

{==============================================================================}

procedure BEToLongWord(Value:LongWord;Buffer:PByte); inline;
begin
 {}
 Buffer[0]:=((Value shr 24) and 255);
 Buffer[1]:=((Value shr 16) and 255);
 Buffer[2]:=((Value shr 8) and 255);
 Buffer[3]:=(Value and 255);
end;

{==============================================================================}

function QWordToBE(Buffer:PByte):QWord; inline;
begin
 {}
 Result:=(QWord(Buffer[0] and 255) shl 56)
      or (QWord(Buffer[1] and 255) shl 48)
      or (QWord(Buffer[2] and 255) shl 40)
      or (QWord(Buffer[3] and 255) shl 32)
      or (QWord(Buffer[4] and 255) shl 24)
      or (QWord(Buffer[5] and 255) shl 16)
      or (QWord(Buffer[6] and 255) shl 8)
      or QWord(Buffer[7] and 255);
end;

{==============================================================================}

procedure BEToQWord(Value:QWord;Buffer:PByte); inline;
begin
 {}
 Buffer[0]:=((Value shr 56) and 255);
 Buffer[1]:=((Value shr 48) and 255);
 Buffer[2]:=((Value shr 40) and 255);
 Buffer[3]:=((Value shr 32) and 255);
 Buffer[4]:=((Value shr 24) and 255);
 Buffer[5]:=((Value shr 16) and 255);
 Buffer[6]:=((Value shr 8) and 255);
 Buffer[7]:=(Value and 255);
end;

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
     BytesToLE32(@Context.Data[0],16);

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
   BytesToLE32(@Context.Data[0],16);

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
   BytesToLE32(@Context.Data[0],16);

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
 BytesToLE32(@Context.Data[0],14);

 {Add count of bits}
 PLongWord(@Context.Data[56])^:=Int64Rec(Context.Count).Lo; //To Do //LongWordNToLE //See SHA1/256
 PLongWord(@Context.Data[60])^:=Int64Rec(Context.Count).Hi; //To Do //LongWordNToLE //See SHA1/256

 {Add to hash}
 MD5Transform(Context,@Context.Data[0]);

 {Reverse the hash}
 BytesToLE32(@Context.Buffer[0],4);

 {Copy the hash to the digest}
 System.Move(Context.Buffer[0],Digest,16);

 {Clear the context to prevent data leakage}
 FillChar(Context,SizeOf(TMD5Context),0);
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
function AESTE0(I:Byte):LongWord; inline;
{Internal function used by AESEncryptBlock/AESDecryptBlock}
begin
 {}
 Result:=AES_TE0[I];
end;

{==============================================================================}

function AESTE1(I:Byte):LongWord; inline;
{Internal function used by AESEncryptBlock/AESDecryptBlock}
begin
 {}
 {$IFDEF CRYPTO_AES_PRECOMPUTED_TABLES}
 Result:=AES_TE1[I];
 {$ELSE}
 Result:=RorDWord(AES_TE0[I],8);
 {$ENDIF}
end;

{==============================================================================}

function AESTE2(I:Byte):LongWord; inline;
{Internal function used by AESEncryptBlock/AESDecryptBlock}
begin
 {}
 {$IFDEF CRYPTO_AES_PRECOMPUTED_TABLES}
 Result:=AES_TE2[I];
 {$ELSE}
 Result:=RorDWord(AES_TE0[I],16);
 {$ENDIF}
end;

{==============================================================================}

function AESTE3(I:Byte):LongWord; inline;
{Internal function used by AESEncryptBlock/AESDecryptBlock}
begin
 {}
 {$IFDEF CRYPTO_AES_PRECOMPUTED_TABLES}
 Result:=AES_TE3[I];
 {$ELSE}
 Result:=RorDWord(AES_TE0[I],24);
 {$ENDIF}
end;

{==============================================================================}

function AESTD0(I:Byte):LongWord; inline;
{Internal function used by AESEncryptBlock/AESDecryptBlock}
begin
 {}
 Result:=AES_TD0[I];
end;

{==============================================================================}

function AESTD1(I:Byte):LongWord; inline;
{Internal function used by AESEncryptBlock/AESDecryptBlock}
begin
 {}
 {$IFDEF CRYPTO_AES_PRECOMPUTED_TABLES}
 Result:=AES_TD1[I];
 {$ELSE}
 Result:=RorDWord(AES_TD0[I],8)
 {$ENDIF}
end;

{==============================================================================}

function AESTD2(I:Byte):LongWord; inline;
{Internal function used by AESEncryptBlock/AESDecryptBlock}
begin
 {}
 {$IFDEF CRYPTO_AES_PRECOMPUTED_TABLES}
 Result:=AES_TD2[I];
 {$ELSE}
 Result:=RorDWord(AES_TD0[I],16)
 {$ENDIF}
end;

{==============================================================================}

function AESTD3(I:Byte):LongWord; inline;
{Internal function used by AESEncryptBlock/AESDecryptBlock}
begin
 {}
 {$IFDEF CRYPTO_AES_PRECOMPUTED_TABLES}
 Result:=AES_TD3[I];
 {$ELSE}
 Result:=RorDWord(AES_TD0[I],24)
 {$ENDIF}
end;

{==============================================================================}

function AESTE4_0(I:Byte):LongWord; inline;
{Internal function used by AESEncryptBlock/AESDecryptBlock}
begin
 {}
 {$IFDEF CRYPTO_AES_PRECOMPUTED_TABLES}
 Result:=AES_TE4_0[I];
 {$ELSE}
 Result:=AES_TE4[I] and $000000FF;
 {$ENDIF}
end;

{==============================================================================}

function AESTE4_1(I:Byte):LongWord; inline;
{Internal function used by AESEncryptBlock/AESDecryptBlock}
begin
 {}
 {$IFDEF CRYPTO_AES_PRECOMPUTED_TABLES}
 Result:=AES_TE4_1[I];
 {$ELSE}
 Result:=AES_TE4[I] and $0000FF00;
 {$ENDIF}
end;

{==============================================================================}

function AESTE4_2(I:Byte):LongWord; inline;
{Internal function used by AESEncryptBlock/AESDecryptBlock}
begin
 {}
 {$IFDEF CRYPTO_AES_PRECOMPUTED_TABLES}
 Result:=AES_TE4_2[I];
 {$ELSE}
 Result:=AES_TE4[I] and $00FF0000;
 {$ENDIF}
end;

{==============================================================================}

function AESTE4_3(I:Byte):LongWord; inline;
{Internal function used by AESEncryptBlock/AESDecryptBlock}
begin
 {}
 {$IFDEF CRYPTO_AES_PRECOMPUTED_TABLES}
 Result:=AES_TE4_3[I];
 {$ELSE}
 Result:=AES_TE4[I] and $FF000000;
 {$ENDIF}
end;

{==============================================================================}

function AESByte(X,N:LongWord):Byte; inline;
{Internal function used by AESEncryptBlock/AESDecryptBlock}
begin
 {}
 Result:=(X shr (N shl 3)) and 255;
end;

{==============================================================================}

function AESKeySetup(Key:Pointer;KeySize:LongWord;AESKey:PAESKey):Boolean;
{Create a pair of AES keys using the supplied key}
{Key size must be 16, 24 or 32 bytes (128, 192 or 256 bits)}

 function AESSetupMix(Temp:LongWord):LongWord; inline;
 begin
  {}
  Result:=AESTE4_3(AESByte(Temp,2)) xor AESTE4_2(AESByte(Temp,1)) xor AESTE4_1(AESByte(Temp,0)) xor AESTE4_0(AESByte(Temp,3));

 end;

 function AESSetupMix2(Temp:LongWord):LongWord; inline;
 begin
  {}
  Result:=AESTD0(255 and AES_TE4[AESByte(Temp,3)]) xor AESTD1(255 and AES_TE4[AESByte(Temp,2)]) xor AESTD2(255 and AES_TE4[AESByte(Temp,1)]) xor AESTD3(255 and AES_TE4[AESByte(Temp,0)]);
 end;

var
 Temp:LongWord;
 Count:LongWord;
 RK:PLongWord;
 RRK:PLongWord;
begin
 {}
 Result:=False;

 {Check Params}
 if Key = nil then Exit;
 if AESKey = nil then Exit;

 {Check size}
 if (KeySize <> AES_KEY_SIZE128) and (KeySize <> AES_KEY_SIZE192) and (KeySize <> AES_KEY_SIZE256) then Exit;

 {Setup rounds}
 AESKey.Rounds:=10 + ((KeySize div 8) - 2) * 2;

 {Setup encryption key}
 RK:=@AESKey.EncryptKey;
 RK[0]:=LongWordToBE(Key);
 RK[1]:=LongWordToBE(Key + 4);
 RK[2]:=LongWordToBE(Key + 8);
 RK[3]:=LongWordToBE(Key + 12);

 {Check size}
 if KeySize = AES_KEY_SIZE128 then
  begin
   for Count:=0 to 9 do
    begin
     Temp:=RK[3];
     RK[4]:=RK[0] xor AESSetupMix(Temp) xor AES_RCON[Count];
     RK[5]:=RK[1] xor RK[4];
     RK[6]:=RK[2] xor RK[5];
     RK[7]:=RK[3] xor RK[6];

     Inc(RK,4); {Increment PLongWord increments by SizeOf(LongWord)}
    end;
  end
 else if KeySize = AES_KEY_SIZE192 then
  begin
   RK[4]:=LongWordToBE(Key + 16);
   RK[5]:=LongWordToBE(Key + 20);

   for Count:=0 to 7 do
    begin
     Temp:=RK[5];
     RK[6]:=RK[0] xor AESSetupMix(Temp) xor AES_RCON[Count];
     RK[7]:=RK[1] xor RK[6];
     RK[8]:=RK[2] xor RK[7];
     RK[9]:=RK[3] xor RK[8];

     if Count = 7 then Break;

     RK[10]:=RK[4] xor RK[9];
     RK[11]:=RK[5] xor RK[10];

     Inc(RK,6); {Increment PLongWord increments by SizeOf(LongWord)}
    end;
  end
 else if KeySize = AES_KEY_SIZE256 then
  begin
   RK[4]:=LongWordToBE(Key + 16);
   RK[5]:=LongWordToBE(Key + 20);
   RK[6]:=LongWordToBE(Key + 24);
   RK[7]:=LongWordToBE(Key + 28);

   for Count:=0 to 6 do
    begin
     Temp:=RK[7];
     RK[8]:=RK[0] xor AESSetupMix(Temp) xor AES_RCON[Count];
     RK[9]:=RK[1] xor RK[8];
     RK[10]:=RK[2] xor RK[9];
     RK[11]:=RK[3] xor RK[10];

     if Count = 6 then Break;

     Temp:=RK[11];
     RK[12]:=RK[4] xor AESSetupMix(RorDWord(Temp,8));
     RK[13]:=RK[5] xor RK[12];
     RK[14]:=RK[6] xor RK[13];
     RK[15]:=RK[7] xor RK[14];

     Inc(RK,8); {Increment PLongWord increments by SizeOf(LongWord)}
    end;
  end;

 {Setup decryption key}
 RK:=@AESKey.DecryptKey;
 RRK:=PLongWord(@AESKey.EncryptKey + ((28 + KeySize) * SizeOf(LongWord)) - (4 * SizeOf(LongWord)));

 {Apply the inverse MixColumn transform to all round keys but the first and the last}
 {Copy first round}
 RK^:=RRK^;
 Inc(RK);  {Increment PLongWord increments by SizeOf(LongWord)}
 Inc(RRK);

 RK^:=RRK^;
 Inc(RK);  {Increment PLongWord increments by SizeOf(LongWord)}
 Inc(RRK);

 RK^:=RRK^;
 Inc(RK);  {Increment PLongWord increments by SizeOf(LongWord)}
 Inc(RRK);

 RK^:=RRK^;

 Dec(RK,3); {Decrement PLongWord decrements by SizeOf(LongWord)}
 Dec(RRK,3);

 for Count:=1 to AESKey.Rounds - 1 do
  begin
   Dec(RRK,4); {Increment PLongWord increments by SizeOf(LongWord)}
   Inc(RK,4);  {Decrement PLongWord decrements by SizeOf(LongWord)}

   {$IFNDEF CRYPTO_AES_PRECOMPUTED_TABLES}
   Temp:=RRK[0];
   RK[0]:=AESSetupMix2(Temp);

   Temp:=RRK[1];
   RK[1]:=AESSetupMix2(Temp);

   Temp:=RRK[2];
   RK[2]:=AESSetupMix2(Temp);

   Temp:=RRK[3];
   RK[3]:=AESSetupMix2(Temp);

   {$ELSE}
   Temp:=RRK[0];
   RK[0]:=AES_TKS0[AESByte(Temp,3)] xor AES_TKS1[AESByte(Temp,2)] xor AES_TKS2[AESByte(Temp,1)] xor AES_TKS3[AESByte(Temp,0)];

   Temp:=RRK[1];
   RK[1]:=AES_TKS0[AESByte(Temp,3)] xor AES_TKS1[AESByte(Temp,2)] xor AES_TKS2[AESByte(Temp,1)] xor AES_TKS3[AESByte(Temp,0)];

   Temp:=RRK[2];
   RK[2]:=AES_TKS0[AESByte(Temp,3)] xor AES_TKS1[AESByte(Temp,2)] xor AES_TKS2[AESByte(Temp,1)] xor AES_TKS3[AESByte(Temp,0)];

   Temp:=RRK[3];
   RK[3]:=AES_TKS0[AESByte(Temp,3)] xor AES_TKS1[AESByte(Temp,2)] xor AES_TKS2[AESByte(Temp,1)] xor AES_TKS3[AESByte(Temp,0)];

   {$ENDIF}
  end;

 {Copy last round}
 Dec(RRK,4);
 Inc(RK,4);

 RK^:=RRK^;
 Inc(RK);  {Increment PLongWord increments by SizeOf(LongWord)}
 Inc(RRK);

 RK^:=RRK^;
 Inc(RK);  {Increment PLongWord increments by SizeOf(LongWord)}
 Inc(RRK);

 RK^:=RRK^;
 Inc(RK);  {Increment PLongWord increments by SizeOf(LongWord)}
 Inc(RRK);

 RK^:=RRK^;

 Result:=True;
end;

{==============================================================================}

procedure AESEncryptBlock(Plain,Crypt:Pointer;AESKey:PAESKey);
{AES Encrypt a 16 byte (128 bit) block of data using the supplied key}
var
 S0:LongWord;
 S1:LongWord;
 S2:LongWord;
 S3:LongWord;
 T0:LongWord;
 T1:LongWord;
 T2:LongWord;
 T3:LongWord;
 RK:PLongWord;
 Count:LongWord;
 Rounds:LongWord;
begin
 {}
 {Check Params}
 if Plain = nil then Exit;
 if Crypt = nil then Exit;
 if AESKey = nil then Exit;

 {Get rounds and key}
 Rounds:=AESKey.Rounds;
 RK:=@AESKey.EncryptKey;

 {Map byte array block to cipher state and add initial round key}
 S0:=LongWordToBE(Plain) xor RK[0];
 S1:=LongWordToBE(Plain +  4) xor RK[1];
 S2:=LongWordToBE(Plain +  8) xor RK[2];
 S3:=LongWordToBE(Plain + 12) xor RK[3];

 {$IFNDEF CRYPTO_AES_PRECOMPUTED_TABLES}
 Count:=0;
 while True do
  begin
   Inc(RK,4); {Increment PLongWord increments by SizeOf(LongWord)}

   T0:=AESTE0(AESByte(S0,3)) xor AESTE1(AESByte(S1,2)) xor AESTE2(AESByte(S2,1)) xor AESTE3(AESByte(S3,0)) xor RK[0];
   T1:=AESTE0(AESByte(S1,3)) xor AESTE1(AESByte(S2,2)) xor AESTE2(AESByte(S3,1)) xor AESTE3(AESByte(S0,0)) xor RK[1];
   T2:=AESTE0(AESByte(S2,3)) xor AESTE1(AESByte(S3,2)) xor AESTE2(AESByte(S0,1)) xor AESTE3(AESByte(S1,0)) xor RK[2];
   T3:=AESTE0(AESByte(S3,3)) xor AESTE1(AESByte(S0,2)) xor AESTE2(AESByte(S1,1)) xor AESTE3(AESByte(S2,0)) xor RK[3];

   if Count = (Rounds - 2) then Break;

   S0:=T0;
   S1:=T1;
   S2:=T2;
   S3:=T3;

   Inc(Count);
  end;

 Inc(RK,4); {Increment PLongWord increments by SizeOf(LongWord)}

 {$ELSE}
 {Rounds - 1 full rounds}
 Count:=Rounds shr 1;
 while True do
  begin
   T0:=AESTE0(AESByte(S0,3)) xor AESTE1(AESByte(S1,2)) xor AESTE2(AESByte(S2,1)) xor AESTE3(AESByte(S3,0)) xor RK[4];
   T1:=AESTE0(AESByte(S1,3)) xor AESTE1(AESByte(S2,2)) xor AESTE2(AESByte(S3,1)) xor AESTE3(AESByte(S0,0)) xor RK[5];
   T2:=AESTE0(AESByte(S2,3)) xor AESTE1(AESByte(S3,2)) xor AESTE2(AESByte(S0,1)) xor AESTE3(AESByte(S1,0)) xor RK[6];
   T3:=AESTE0(AESByte(S3,3)) xor AESTE1(AESByte(S0,2)) xor AESTE2(AESByte(S1,1)) xor AESTE3(AESByte(S2,0)) xor RK[7];

   Inc(RK,8); {Increment PLongWord increments by SizeOf(LongWord)}

   Dec(Count);
   if Count = 0 then Break;

   S0:=AESTE0(AESByte(T0,3)) xor AESTE1(AESByte(T1,2)) xor AESTE2(AESByte(T2,1)) xor AESTE3(AESByte(T3,0)) xor RK[0];
   S1:=AESTE0(AESByte(T1,3)) xor AESTE1(AESByte(T2,2)) xor AESTE2(AESByte(T3,1)) xor AESTE3(AESByte(T0,0)) xor RK[1];
   S2:=AESTE0(AESByte(T2,3)) xor AESTE1(AESByte(T3,2)) xor AESTE2(AESByte(T0,1)) xor AESTE3(AESByte(T1,0)) xor RK[2];
   S3:=AESTE0(AESByte(T3,3)) xor AESTE1(AESByte(T0,2)) xor AESTE2(AESByte(T1,1)) xor AESTE3(AESByte(T2,0)) xor RK[3];
  end;

 {$ENDIF}

 {Apply last round and map cipher state to byte array block}
 S0:=AESTE4_3(AESByte(T0,3)) xor AESTE4_2(AESByte(T1,2)) xor AESTE4_1(AESByte(T2,1)) xor AESTE4_0(AESByte(T3,0)) xor RK[0];
 BEToLongWord(S0,Crypt);

 S1:=AESTE4_3(AESByte(T1,3)) xor AESTE4_2(AESByte(T2,2)) xor AESTE4_1(AESByte(T3,1)) xor AESTE4_0(AESByte(T0,0)) xor RK[1];
 BEToLongWord(S1,Crypt + 4);

 S2:=AESTE4_3(AESByte(T2,3)) xor AESTE4_2(AESByte(T3,2)) xor AESTE4_1(AESByte(T0,1)) xor AESTE4_0(AESByte(T1,0)) xor RK[2];
 BEToLongWord(S2,Crypt + 8);

 S3:=AESTE4_3(AESByte(T3,3)) xor AESTE4_2(AESByte(T0,2)) xor AESTE4_1(AESByte(T1,1)) xor AESTE4_0(AESByte(T2,0)) xor RK[3];
 BEToLongWord(S3,Crypt + 12);
end;

{==============================================================================}

procedure AESDecryptBlock(Crypt,Plain:Pointer;AESKey:PAESKey);
{AES Decrypt a 16 byte (128 bit) block of data using the supplied key}
var
 S0:LongWord;
 S1:LongWord;
 S2:LongWord;
 S3:LongWord;
 T0:LongWord;
 T1:LongWord;
 T2:LongWord;
 T3:LongWord;
 RK:PLongWord;
 Count:LongWord;
 Rounds:LongWord;
begin
 {}
 {Check Params}
 if Crypt = nil then Exit;
 if Plain = nil then Exit;
 if AESKey = nil then Exit;

 {Get rounds and key}
 Rounds:=AESKey.Rounds;
 RK:=@AESKey.DecryptKey;

 {Map byte array block to cipher state and add initial round key}
 S0:=LongWordToBE(Crypt) xor RK[0];
 S1:=LongWordToBE(Crypt +  4) xor RK[1];
 S2:=LongWordToBE(Crypt +  8) xor RK[2];
 S3:=LongWordToBE(Crypt + 12) xor RK[3];

 {$IFNDEF CRYPTO_AES_PRECOMPUTED_TABLES}
 Count:=0;
 while True do
  begin
   Inc(RK,4); {Increment PLongWord increments by SizeOf(LongWord)}

   T0:=AESTD0(AESByte(S0,3)) xor AESTD1(AESByte(S3,2)) xor AESTD2(AESByte(S2,1)) xor AESTD3(AESByte(S1,0)) xor RK[0];
   T1:=AESTD0(AESByte(S1,3)) xor AESTD1(AESByte(S0,2)) xor AESTD2(AESByte(S3,1)) xor AESTD3(AESByte(S2,0)) xor RK[1];
   T2:=AESTD0(AESByte(S2,3)) xor AESTD1(AESByte(S1,2)) xor AESTD2(AESByte(S0,1)) xor AESTD3(AESByte(S3,0)) xor RK[2];
   T3:=AESTD0(AESByte(S3,3)) xor AESTD1(AESByte(S2,2)) xor AESTD2(AESByte(S1,1)) xor AESTD3(AESByte(S0,0)) xor RK[3];

   if Count = (Rounds - 2) then Break;

   S0:=T0;
   S1:=T1;
   S2:=T2;
   S3:=T3;

   Inc(Count);
  end;

 Inc(RK,4); {Increment PLongWord increments by SizeOf(LongWord)}

 {$ELSE}
 {Rounds - 1 full rounds}
 Count:=Rounds shr 1;
 while True do
  begin
   T0:=AESTD0(AESByte(S0,3)) xor AESTD1(AESByte(S3,2)) xor AESTD2(AESByte(S2,1)) xor AESTD3(AESByte(S1,0)) xor RK[4];
   T1:=AESTD0(AESByte(S1,3)) xor AESTD1(AESByte(S0,2)) xor AESTD2(AESByte(S3,1)) xor AESTD3(AESByte(S2,0)) xor RK[5];
   T2:=AESTD0(AESByte(S2,3)) xor AESTD1(AESByte(S1,2)) xor AESTD2(AESByte(S0,1)) xor AESTD3(AESByte(S3,0)) xor RK[6];
   T3:=AESTD0(AESByte(S3,3)) xor AESTD1(AESByte(S2,2)) xor AESTD2(AESByte(S1,1)) xor AESTD3(AESByte(S0,0)) xor RK[7];

   Inc(RK,8); {Increment PLongWord increments by SizeOf(LongWord)}

   Dec(Count);
   if Count = 0 then Break;

   S0:=AESTD0(AESByte(T0,3)) xor AESTD1(AESByte(T3,2)) xor AESTD2(AESByte(T2,1)) xor AESTD3(AESByte(T1,0)) xor RK[0];
   S1:=AESTD0(AESByte(T1,3)) xor AESTD1(AESByte(T0,2)) xor AESTD2(AESByte(T3,1)) xor AESTD3(AESByte(T2,0)) xor RK[1];
   S2:=AESTD0(AESByte(T2,3)) xor AESTD1(AESByte(T1,2)) xor AESTD2(AESByte(T0,1)) xor AESTD3(AESByte(T3,0)) xor RK[2];
   S3:=AESTD0(AESByte(T3,3)) xor AESTD1(AESByte(T2,2)) xor AESTD2(AESByte(T1,1)) xor AESTD3(AESByte(T0,0)) xor RK[3];
  end;

 {$ENDIF}

 {Apply last round and map cipher state to byte array block}
 S0:=(AES_TD4[AESByte(T0,3)] and $ff000000) xor (AES_TD4[AESByte(T3,2)] and $00ff0000) xor (AES_TD4[AESByte(T2,1)] and $0000ff00) xor (AES_TD4[AESByte(T1,0)] and $000000ff) xor RK[0];
 BEToLongWord(S0,Plain);

 S1:=(AES_TD4[AESByte(T1,3)] and $ff000000) xor (AES_TD4[AESByte(T0,2)] and $00ff0000) xor (AES_TD4[AESByte(T3,1)] and $0000ff00) xor (AES_TD4[AESByte(T2,0)] and $000000ff) xor RK[1];
 BEToLongWord(S1,Plain + 4);

 S2:=(AES_TD4[AESByte(T2,3)] and $ff000000) xor (AES_TD4[AESByte(T1,2)] and $00ff0000) xor (AES_TD4[AESByte(T0,1)] and $0000ff00) xor (AES_TD4[AESByte(T3,0)] and $000000ff) xor RK[2];
 BEToLongWord(S2,Plain + 8);

 S3:=(AES_TD4[AESByte(T3,3)] and $ff000000) xor (AES_TD4[AESByte(T2,2)] and $00ff0000) xor (AES_TD4[AESByte(T1,1)] and $0000ff00) xor (AES_TD4[AESByte(T0,0)] and $000000ff) xor RK[3];
 BEToLongWord(S3,Plain + 12);
end;

{==============================================================================}
{==============================================================================}
{DES Helper Functions}
procedure DESCook(Raw1:PLongWord;Key:PDESKey);
var
 Count:LongWord;
 Raw0:PLongWord;
 Cook:PLongWord;
 Dough:TDESkey;
begin
 {}
 Cook:=@Dough;

 for Count:=0 to 15 do
  begin
   Raw0:=Raw1;
   Inc(Raw1); {Increment PLongWord increments by SizeOf(LongWord)}

   Cook^:=(Raw0^ and $00fc0000) shl 6;
   Cook^:=Cook^ or ((Raw0^ and $00000fc0) shl 10);
   Cook^:=Cook^ or ((Raw1^ and $00fc0000) shr 10);
   Cook^:=Cook^ or ((Raw1^ and $00000fc0) shr 6);
   Inc(Cook); {Increment PLongWord increments by SizeOf(LongWord)}

   Cook^:=(Raw0^ and $0003f000) shl 12;
   Cook^:=Cook^ or ((Raw0^ and $0000003f) shl 16);
   Cook^:=Cook^ or ((Raw1^ and $0003f000) shr 4);
   Cook^:=Cook^ or (Raw1^ and $0000003f);
   Inc(Cook); {Increment PLongWord increments by SizeOf(LongWord)}

   Inc(Raw1); {Increment PLongWord increments by SizeOf(LongWord)}
  end;

 System.Move(Dough[0],Key^,SizeOf(TDESkey));
end;

{==============================================================================}

procedure DESKey(Key:PByte;KeyType:LongWord;CryptKey:PDESKey);
var
 I:LongWord;
 J:LongWord;
 L:LongWord;
 M:LongWord;
 N:LongWord;
 Raw:TDESkey;
 PCR:array[0..55] of Byte;
 PC1M:array[0..55] of Byte;
begin
 {}
 for J:=0 to 55 do
  begin
   L:=DES_PC1[J];
   M:=L and 7;
   if (Key[L shr 3] and DES_BYTEBIT[M]) = DES_BYTEBIT[M] then
    begin
     PC1M[J]:=1;
    end
   else
    begin
     PC1M[J]:=0;
    end;
  end;

 for I:=0 to 15 do
  begin
   if KeyType = DES_KEYTYPE_DECRYPT then
    begin
     M:=(15 - I) shl 1;
    end
   else
    begin
     M:=I shl 1;
    end;

   N:=M + 1;
   Raw[M]:=0;
   Raw[N]:=0;

   for J:=0 to 27 do
    begin
     L:=J + DES_TOTROT[I];
     if L < 28 then
      begin
       PCR[J]:=PC1M[L];
      end
     else
      begin
       PCR[J]:=PC1M[L - 28];
      end;
    end;

   for J:=28 to 55 do
    begin
     L:=J + DES_TOTROT[I];
     if L < 56 then
      begin
       PCR[J]:=PC1M[L];
      end
     else
      begin
       PCR[J]:=PC1M[L - 28];
      end;
    end;

   for J:=0 to 23 do
    begin
     if PCR[DES_PC2[J]] <> 0 then Raw[M]:=Raw[M] or DES_BIGBYTE[J];
     if PCR[DES_PC2[J + 24]] <> 0 then Raw[N]:=Raw[N] or DES_BIGBYTE[J];
    end;
  end;

 DESCook(@Raw,CryptKey);
end;

{==============================================================================}

procedure DESProcess(Block,Key:PLongWord);

 function DESROLc(X,Y:LongWord):LongWord; inline;{Not Used}
 begin
  {}
  Result:=((X shl (Y and 31)) or ((X and $FFFFFFFF) shr (32 - (Y and 31)))) and $FFFFFFFF;
 end;

 function DESRORc(X,Y:LongWord):LongWord; inline; {Not Used}
 begin
  {}
  Result:=(((X and $FFFFFFFF) shr (Y and 31)) or (X shl (32 - (Y and 31)))) and $FFFFFFFF;
 end;

 {$IFDEF CRYPTO_DES_PRECOMPUTED_PERMUTATIONS}
 function DESByte(X,N:LongWord):Byte; inline;
 begin
  {}
  Result:=(X shr (N shl 3)) and 255;
 end;
 {$ENDIF}

var
 {$IFDEF CRYPTO_DES_PRECOMPUTED_PERMUTATIONS}
 Temp:QWord;
 {$ENDIF}
 Work:LongWord;
 Right:LongWord;
 Left:LongWord;
 Round:LongWord;
begin
 {}
 Left:=Block[0];
 Right:=Block[1];

 {$IFNDEF CRYPTO_DES_PRECOMPUTED_PERMUTATIONS}
 Work:=((Left shr 4) xor Right) and $0f0f0f0f;
 Right:=Right xor Work;
 Left:=Left xor (Work shl 4);

 Work:=((Left shr 16) xor Right) and $0000ffff;
 Right:=Right xor Work;
 Left:=Left xor (Work shl 16);

 Work:=((Right shr 2) xor Left) and $33333333;
 Left:=Left xor Work;
 Right:=Right xor (Work shl 2);

 Work:=((Right shr 8) xor Left) and $00ff00ff;
 Left:=Left xor Work;
 Right:=Right xor (Work shl 8);

 {Right:=DESROLc(Right,1);}
 Right:=RolDWord(Right,1);
 Work:=(Left xor Right) and $aaaaaaaa;

 Left:=Left xor Work;
 Right:=Right xor Work;
 {Left:=DESROLc(Left,1);}
 Left:=RolDWord(Left,1);

 {$ELSE}
 Temp:=DES_IP[0][DESByte(Left,0)] xor DES_IP[1][DESByte(Left,1)] xor DES_IP[2][DESByte(Left,2)] xor DES_IP[3][DESByte(Left,3)] xor
       DES_IP[4][DESByte(Right,0)] xor DES_IP[5][DESByte(Right,1)] xor DES_IP[6][DESByte(Right,2)] xor DES_IP[7][DESByte(Right,3)];

 Left:=Int64Rec(Temp).Hi;
 Right:=Int64Rec(Temp).Lo;

 {$ENDIF}

 for Round:=0 to 7 do
  begin
   {Work:=DESRORc(Right,4) xor Key^;}
   Work:=RorDWord(Right,4) xor Key^;
   Inc(Key); {Increment PLongWord increments by SizeOf(LongWord)}

   Left:=Left xor (DES_S7[Work and $3f] xor DES_S5[(Work shr  8) and $3f] xor DES_S3[(Work shr 16) and $3f] xor DES_S1[(Work shr 24) and $3f]);

   Work:=Right xor Key^;
   Inc(Key); {Increment PLongWord increments by SizeOf(LongWord)}

   Left:=Left xor (DES_S8[Work and $3f] xor DES_S6[(Work shr  8) and $3f] xor DES_S4[(Work shr 16) and $3f] xor DES_S2[(Work shr 24) and $3f]);

   {Work:=DESRORc(Left,4) xor Key^;}
   Work:=RorDWord(Left,4) xor Key^;
   Inc(Key); {Increment PLongWord increments by SizeOf(LongWord)}

   Right:=Right xor (DES_S7[Work and $3f] xor DES_S5[(Work shr  8) and $3f] xor DES_S3[(Work shr 16) and $3f] xor DES_S1[(Work shr 24) and $3f]);

   Work:=Left xor Key^;
   Inc(Key); {Increment PLongWord increments by SizeOf(LongWord)}

   Right:=Right xor (DES_S8[Work and $3f] xor DES_S6[(Work shr  8) and $3f] xor DES_S4[(Work shr 16) and $3f] xor DES_S2[(Work shr 24) and $3f]);
  end;

 {$IFNDEF CRYPTO_DES_PRECOMPUTED_PERMUTATIONS}
 {Right:=DESRORc(Right,1);}
 Right:=RorDWord(Right,1);
 Work:=(Left xor Right) and $aaaaaaaa;
 Left:=Left xor Work;
 Right:=Right xor Work;

 {Left:=DESRORc(Left,1);}
 Left:=RorDWord(Left,1);
 Work:=((Left shr 8) xor Right) and $00ff00ff;
 Right:=Right xor Work;
 Left:=Left xor (Work shl 8);

 Work:=((Left shr 2) xor Right) and $33333333;
 Right:=Right xor Work;
 Left:=Left xor (Work shl 2);

 Work:=((Right shr 16) xor Left) and $0000ffff;
 Left:=Left xor Work;
 Right:=Right xor (Work shl 16);

 Work:=((Right shr 4) xor Left) and $0f0f0f0f;
 Left:=Left xor Work;
 Right:=Right xor (Work shl 4);

 {$ELSE}
 Temp:=DES_FP[0][DESByte(Left,0)] xor DES_FP[1][DESByte(Left,1)] xor DES_FP[2][DESByte(Left,2)] xor DES_FP[3][DESByte(Left,3)] xor
       DES_FP[4][DESByte(Right,0)] xor DES_FP[5][DESByte(Right,1)] xor DES_FP[6][DESByte(Right,2)] xor DES_FP[7][DESByte(Right,3)];

 Left:=Int64Rec(Temp).Hi;
 Right:=Int64Rec(Temp).Lo;

 {$ENDIF}

 Block[0]:=Right;
 Block[1]:=Left;
end;

{==============================================================================}

function DESKeySetup(Key:Pointer;KeySize:LongWord;EncryptKey,DecryptKey:PDESKey):Boolean;
begin
 {}
 Result:=False;

 if Key = nil then Exit;
 if EncryptKey = nil then Exit;
 if DecryptKey = nil then Exit;

 {Check size}
 if KeySize <> DES_KEY_SIZE then Exit;

 DESKey(Key,DES_KEYTYPE_ENCRYPT,EncryptKey);
 DESKey(Key,DES_KEYTYPE_DECRYPT,DecryptKey);

 Result:=True;
end;

{==============================================================================}

procedure DESEncryptBlock(Plain,Crypt:Pointer;EncryptKey:PDESKey);
var
 Work:array[0..1] of LongWord;
begin
 {}
 if Plain = nil then Exit;
 if Crypt = nil then Exit;
 if EncryptKey = nil then Exit;

 Work[0]:=LongWordToBE(PByte(Plain));
 Work[1]:=LongWordToBE(PByte(Plain + 4));

 DESProcess(@Work,PLongWord(EncryptKey));

 BEToLongWord(Work[0],PByte(Crypt));
 BEToLongWord(Work[1],PByte(Crypt + 4));
end;

{==============================================================================}

procedure DESDecryptBlock(Crypt,Plain:Pointer;DecryptKey:PDESKey);
var
 Work:array[0..1] of LongWord;
begin
 {}
 if Crypt = nil then Exit;
 if Plain = nil then Exit;
 if DecryptKey = nil then Exit;

 Work[0]:=LongWordToBE(PByte(Crypt));
 Work[1]:=LongWordToBE(PByte(Crypt + 4));

 DESProcess(@Work,PLongWord(DecryptKey));

 BEToLongWord(Work[0],PByte(Plain));
 BEToLongWord(Work[1],PByte(Plain + 4));
end;

{==============================================================================}
{==============================================================================}
{3DES Helper Functions}
function DES3KeySetup(Key:Pointer;KeySize:LongWord;DES3Key:PDES3Key):Boolean;
begin
 {}
 Result:=False;

 if Key = nil then Exit;
 if DES3Key = nil then Exit;

 {Check size}
 if KeySize <> DES3_KEY_SIZE then Exit;

 DESKey(Key,DES_KEYTYPE_ENCRYPT,@DES3Key.EncryptKey[0]);
 DESKey(Key + 8,DES_KEYTYPE_DECRYPT,@DES3Key.EncryptKey[1]);
 DESKey(Key + 16,DES_KEYTYPE_ENCRYPT,@DES3Key.EncryptKey[2]);

 DESKey(Key,DES_KEYTYPE_DECRYPT,@DES3Key.DecryptKey[2]);
 DESKey(Key + 8,DES_KEYTYPE_ENCRYPT,@DES3Key.DecryptKey[1]);
 DESKey(Key + 16,DES_KEYTYPE_DECRYPT,@DES3Key.DecryptKey[0]);

 Result:=True;
end;

{==============================================================================}

procedure DES3EncryptBlock(Plain,Crypt:Pointer;DES3Key:PDES3Key);
var
 Work:array[0..1] of LongWord;
begin
 {}
 if Plain = nil then Exit;
 if Crypt = nil then Exit;
 if DES3Key = nil then Exit;

 Work[0]:=LongWordToBE(PByte(Plain));
 Work[1]:=LongWordToBE(PByte(Plain + 4));

 DESProcess(@Work,PLongWord(@DES3Key.EncryptKey[0]));
 DESProcess(@Work,PLongWord(@DES3Key.EncryptKey[1]));
 DESProcess(@Work,PLongWord(@DES3Key.EncryptKey[2]));

 BEToLongWord(Work[0],PByte(Crypt));
 BEToLongWord(Work[1],PByte(Crypt + 4));
end;

{==============================================================================}

procedure DES3DecryptBlock(Crypt,Plain:Pointer;DES3Key:PDES3Key);
var
 Work:array[0..1] of LongWord;
begin
 {}
 if Crypt = nil then Exit;
 if Plain = nil then Exit;
 if DES3Key = nil then Exit;

 Work[0]:=LongWordToBE(PByte(Crypt));
 Work[1]:=LongWordToBE(PByte(Crypt + 4));

 DESProcess(@Work,PLongWord(@DES3Key.DecryptKey[0]));
 DESProcess(@Work,PLongWord(@DES3Key.DecryptKey[1]));
 DESProcess(@Work,PLongWord(@DES3Key.DecryptKey[2]));

 BEToLongWord(Work[0],PByte(Plain));
 BEToLongWord(Work[1],PByte(Plain + 4));
end;

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

procedure SHA1Update(var Context:TSHA1Context;Data:Pointer;Size:LongWord);
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
   {BytesToBE32(@Context.Data[0],16);} {Done in SHA1Transform}

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
     {BytesToBE32(@Context.Data[0],16);} {Done in SHA1Transform}

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
 if Count >= 56 then
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
 BytesToBE32(@Context.State[0],5);

 {Copy the hash to the digest}
 System.Move(Context.State[0],Digest,20);

 {Clear the context to prevent data leakage}
 FillChar(Context,SizeOf(TSHA1Context),0);
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
  Result:=SHA256S(X,6) xor SHA256S(X,11) xor SHA256S(X,25);
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
   BytesToBE32(@Context.Data[0],16);

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
     BytesToBE32(@Context.Data[0],16);

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
 if Count >= 56 then
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
 BytesToBE32(@Context.State[0],8);

 {Copy the hash to the digest}
 System.Move(Context.State[0],Digest,32);

 {Clear the context to prevent data leakage}
 FillChar(Context,SizeOf(TSHA256Context),0);
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
{SHA384 Helper Functions}
procedure SHA384Init(var Context:TSHA384Context);
{Initialize a SHA384 context with constants}
begin
 {}
 {Set the state constants}
 Context.State[0]:=$cbbb9d5dc1059ed8;
 Context.State[1]:=$629a292a367cd507;
 Context.State[2]:=$9159015a3070dd17;
 Context.State[3]:=$152fecd8f70e5939;
 Context.State[4]:=$67332667ffc00b31;
 Context.State[5]:=$8eb44a8768581511;
 Context.State[6]:=$db0c2e0d64f98fa7;
 Context.State[7]:=$47b5481dbefa4fa4;

 {Zero the bit count}
 Context.Count:=0;
end;

{==============================================================================}

procedure SHA384Process(var Context:TSHA384Context;Data:Pointer;Size:LongWord);
{Add more bytes to the data buffer, add to the hash in 128 byte chunks}
begin
 {}
 SHA512Process(Context,Data,Size);
end;

{==============================================================================}

procedure SHA384Complete(var Context:TSHA384Context;var Digest:TSHA384Digest);
{Finalize the SHA384 context by padding to a 128 Byte boundary, adding
 QWord count of bits processed and copying the hash to the digest}
var
 SHA512Digest:TSHA512Digest;
begin
 {}
 SHA512Complete(Context,SHA512Digest);

 System.Move(SHA512Digest,Digest,SizeOf(TSHA384Digest));
end;

{==============================================================================}

function SHA384DigestToString(Digest:PSHA384Digest):String;
var
 Count:Integer;
begin
 {}
 Result:='';

 if Digest = nil then Exit;

 for Count:=0 to 47 do
  begin
   Result:=Result + HexStr(Digest[Count],2);
  end;

 Result:=Lowercase(Result);
end;

{==============================================================================}
{==============================================================================}
{SHA512 Helper Functions}
procedure SHA512Init(var Context:TSHA512Context);
{Initialize a SHA512 context with constants}
begin
 {}
 {Set the state constants}
 Context.State[0]:=$6a09e667f3bcc908;
 Context.State[1]:=$bb67ae8584caa73b;
 Context.State[2]:=$3c6ef372fe94f82b;
 Context.State[3]:=$a54ff53a5f1d36f1;
 Context.State[4]:=$510e527fade682d1;
 Context.State[5]:=$9b05688c2b3e6c1f;
 Context.State[6]:=$1f83d9abfb41bd6b;
 Context.State[7]:=$5be0cd19137e2179;

 {Zero the bit count}
 Context.Count:=0;
end;

{==============================================================================}

procedure SHA512Compress(var Context:TSHA512Context;Buffer:Pointer);
{The core SHA512 algorithm, adds an additional 128 Bytes (1024 bits) to the hash}

 function SHA512ROR64c(X,Y:QWord):QWord; inline; {Not Used}
 begin
  {}
  Result:=(((X and $FFFFFFFFFFFFFFFF) shr (Y and 63)) or (X shl (64 - (Y and 63)))) and $FFFFFFFFFFFFFFFF;
 end;

 function SHA512Ch(X,Y,Z:QWord):QWord; inline;
 begin
  {}
  Result:=Z xor (X and (Y xor Z));
 end;

 function SHA512Maj(X,Y,Z:QWord):QWord; inline;
 begin
  {}
  Result:=((X or Y) and Z) or (X and Y);
 end;

 function SHA512S(X,N:QWord):QWord; inline;
 begin
  {}
  {Result:=SHA512ROR64c(X,N);}
  Result:=RorQWord(X,N);
 end;

 function SHA512R(X,N:QWord):QWord; inline;
 begin
  {}
  Result:=(X and $FFFFFFFFFFFFFFFF) shr N;
 end;

 function SHA512Sigma0(X:QWord):QWord; inline;
 begin
  {}
  Result:=SHA512S(X,28) xor SHA512S(X,34) xor SHA512S(X,39);
 end;

 function SHA512Sigma1(X:QWord):QWord; inline;
 begin
  {}
  Result:=SHA512S(X,14) xor SHA512S(X,18) xor SHA512S(X,41);
 end;

 function SHA512Gamma0(X:QWord):QWord; inline;
 begin
  {}
  Result:=SHA512S(X,1) xor SHA512S(X,8) xor SHA512R(X,7);
 end;

 function SHA512Gamma1(X:QWord):QWord; inline;
 begin
  {}
  Result:=SHA512S(X,19) xor SHA512S(X,61) xor SHA512R(X,6);
 end;
var
 Count:LongWord;
 Offset:PtrUInt;

 Temp0:QWord;
 Temp1:QWord;
 Data:TSHA512_W;
 State:array[0..7] of QWord;
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
   {Data[Count]:=QWordNtoBE(PQWord(PtrUInt(Buffer) + Offset)^);} {Big endian swap moved to SHA512Process}
   Data[Count]:=PQWord(PtrUInt(Buffer) + Offset)^;
   Inc(Offset,8);
  end;

 {Fill W[16..79] (Data) with extended values}
 for Count:=16 to 79 do
  begin
   Data[Count]:=SHA512Gamma1(Data[Count - 2]) + Data[Count - 7] + SHA512Gamma0(Data[Count - 15]) + Data[Count - 16];
  end;

 {Perform the compression rounds}
 for Count:=0 to 79 do
  begin
   Temp0:=State[7] + SHA512Sigma1(State[4]) + SHA512Ch(State[4],State[5],State[6]) + SHA512_K[Count] + Data[Count];
   Temp1:=SHA512Sigma0(State[0]) + SHA512Maj(State[0],State[1],State[2]);
   State[7]:=State[6];
   State[6]:=State[5];
   State[5]:=State[4];
   State[4]:=State[3] + Temp0;
   State[3]:=State[2];
   State[2]:=State[1];
   State[1]:=State[0];
   State[0]:=Temp0 + Temp1;
  end;

 {Save the state}
 for Count:=0 to 7 do
  begin
   Inc(Context.State[Count],State[Count]);
  end;
end;

{==============================================================================}

procedure SHA512Process(var Context:TSHA512Context;Data:Pointer;Size:LongWord);
{Add more bytes to the data buffer, add to the hash in 128 byte chunks}
var
 Count:PtrUInt;
 Offset:PtrUInt;
begin
 {}
 if Data = nil then Exit;
 if Size = 0 then Exit;

 {Get the data buffer count}
 Count:=(Context.Count shr 3) and $7F;

 {Update the total bit count}
 Inc(Context.Count,(Size shl 3));

 {Check size and buffer count}
 if (Count + Size) > 127 then
  begin
   {Get bytes remaining to fill the buffer}
   Offset:=128 - Count;

   {Copy to data buffer}
   System.Move(Data^,Context.Data[Count],Offset);

   {Reverse the bytes}
   BytesToBE64(@Context.Data[0],16);

   {Add to hash}
   SHA512Compress(Context,@Context.Data[0]);

   {Reset buffer count}
   Count:=0;

   {Process 128 byte chunks of data}
   while (Offset + 127) < Size do
    begin
     {Copy to data buffer}
     System.Move(Pointer(PtrUInt(Data) + Offset)^,Context.Data[0],128);

     {Reverse the bytes}
     BytesToBE64(@Context.Data[0],16);

     {Add to hash}
     SHA512Compress(Context,@Context.Data[0]);

     {Update offset}
     Inc(Offset,128);
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

procedure SHA512Complete(var Context:TSHA512Context;var Digest:TSHA512Digest);
{Finalize the SHA512 context by padding to a 128 Byte boundary, adding
 QWord count of bits processed and copying the hash to the digest}
var
 Total:QWord;
 Count:PtrUInt;
 Extra:PtrUInt;
 Padding:TSHA512ByteBuffer;
begin
 {}
 {Get the total bit count}
 Total:=Int64NtoBE(Context.Count);

 {Get the data buffer count}
 Count:=(Context.Count shr 3) and $7F;

 {Fill the padding with zeros}
 FillChar(Padding[0],SizeOf(TSHA512ByteBuffer),0);

 {Set the first padding byte to $80}
 Padding[0]:=$80;

 {Determine bytes of padding to make 128 byte buffer}
 {Total count of bits is a 128 bit integer but we only ever use 64 bits}
 if Count >= 112 then
  begin
   {Pad the buffer to 128 bytes}
   SHA512Process(Context,@Padding[0],128 - Count);

   {Update Count}
   Count:=0;

   {Clear the padding byte}
   Padding[0]:=0;
  end;

 {Pad the buffer to 120 bytes}
 Count:=120 - Count;
 SHA512Process(Context,@Padding[0],Count);

 {Add total count of bits}
 SHA512Process(Context,@Total,8);

 {Reverse the hash}
 BytesToBE64(@Context.State[0],8);

 {Copy the hash to the digest}
 System.Move(Context.State[0],Digest,64);

 {Clear the context to prevent data leakage}
 FillChar(Context,SizeOf(TSHA512Context),0);
end;

{==============================================================================}

function SHA512DigestToString(Digest:PSHA512Digest):String;
var
 Count:Integer;
begin
 {}
 Result:='';

 if Digest = nil then Exit;

 for Count:=0 to 63 do
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
{Random Helper Functions}

{==============================================================================}
{==============================================================================}
{CRC Helper Functions}

{==============================================================================}
{==============================================================================}
{Base64 Helper Functions}
procedure Base64InitTables;
var
 Count:Integer;
begin
 {}
 FillChar(Base64DecodeTable,SizeOf(TBase64DecodeTable),#128); {0x80}

 for Count:=1 to Length(Base64EncodeTable) do
  begin
   Base64DecodeTable[Ord(Base64EncodeTable[Count])]:=Char(Count - 1);
  end;

 Base64DecodeTable[Ord('=')]:=#0;
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
