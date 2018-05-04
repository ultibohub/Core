{
Ultibo X.509 interface unit.

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

  AXTLS - \ssl\x509.c - Copyright (c) 2007-2017,Cameron Rich
  WPA Supplicant - \src\tls\x509v3.c - Copyright (c) 2006-2015,Jouni Malinen

References
==========

 X.509 - https://en.wikipedia.org/wiki/X.509

 RFC3280 - https://tools.ietf.org/html/rfc3280

 RFC5280 - https://tools.ietf.org/html/rfc5280

X.509 Public Key Certificates
=============================

 X.509 is a standard that defines the format of public key certificates.  An X.509
 certificate contains a public key and an identity (a hostname, or an organization,
 or an individual), and is either signed by a certificate authority or self-signed.

 When a certificate is signed by a trusted certificate authority, or validated by
 other means, someone holding that certificate can rely on the public key it contains
 to establish secure communications with another party, or validate documents digitally
 signed by the corresponding private key.

 This unit currently only provides the basic functionality required to read and parse
 an X.509 certificate in DER or PEM format and extract basic information such as
 the issuer, subject, validity, algorithm and public key.

 It is expected that this unit will be expanded to incorporate additional functions
 over time.

}

{$mode delphi}    {Default to Delphi compatible syntax}
{$H+}             {Default to AnsiString}
{$inline on}      {Allow use of Inline procedures}
{$pointermath on} {Allow pointer arithmetic}

unit X509;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Crypto,ASN1,Classes,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {X509 specific constants}
 X509_MAX_NAME_ATTRIBUTES = 20;
 X509_MAX_SERIAL_NUM_LEN = 20;

 X509_NAME_ATTR_NONE = 0;
 X509_NAME_ATTR_DC = 1;
 X509_NAME_ATTR_CN = 2;
 X509_NAME_ATTR_C = 3;
 X509_NAME_ATTR_L = 4;
 X509_NAME_ATTR_ST = 5;
 X509_NAME_ATTR_O = 6;
 X509_NAME_ATTR_OU = 7;

 X509_VALIDATE_OK = 0;
 X509_VALIDATE_BAD_CERTIFICATE = 1;
 X509_VALIDATE_UNSUPPORTED_CERTIFICATE = 2;
 X509_VALIDATE_CERTIFICATE_REVOKED = 3;
 X509_VALIDATE_CERTIFICATE_EXPIRED = 4;
 X509_VALIDATE_CERTIFICATE_UNKNOWN = 5;
 X509_VALIDATE_UNKNOWN_CA = 6;

 {Certificate versions}
 X509_CERT_V1 = 0;
 X509_CERT_V2 = 1;
 X509_CERT_V3 = 2;

 {Certificate extensions}
 X509_EXT_BASIC_CONSTRAINTS   = (1 shl 0);
 X509_EXT_PATH_LEN_CONSTRAINT = (1 shl 1);
 X509_EXT_KEY_USAGE           = (1 shl 2);
 X509_EXT_SUBJECT_ALT_NAME    = (1 shl 3);
 X509_EXT_ISSUER_ALT_NAME     = (1 shl 4);
 X509_EXT_EXT_KEY_USAGE       = (1 shl 5);

 {Certificate Key Usage (RFC5280 Section 4.2.1.3. - Key Usage)}
 X509_KEY_USAGE_DIGITAL_SIGNATURE = (1 shl 0); {digitalSignature - The digitalSignature bit is asserted when the subject public key
                                                                   is used for verifying digital signatures, other than signatures
                                                                   on certificates}
 X509_KEY_USAGE_NON_REPUDIATION   = (1 shl 1); {nonRepudiation - The nonRepudiation bit is asserted when the subject public key is
                                                                 used to verify digital signatures, other than signatures on certificates}
 X509_KEY_USAGE_KEY_ENCIPHERMENT  = (1 shl 2); {keyEncipherment - The keyEncipherment bit is asserted when the subject public key is
                                                                  used for enciphering private or secret keys, i.e., for key transport}
 X509_KEY_USAGE_DATA_ENCIPHERMENT = (1 shl 3); {dataEncipherment - The dataEncipherment bit is asserted when the subject public key
                                                                   is used for directly enciphering raw user data without the use of
                                                                   an intermediate symmetric cipher}
 X509_KEY_USAGE_KEY_AGREEMENT     = (1 shl 4); {keyAgreement - The keyAgreement bit is asserted when the subject public key is used
                                                               for key agreement}
 X509_KEY_USAGE_KEY_CERT_SIGN     = (1 shl 5); {keyCertSign - The keyCertSign bit is asserted when the subject public key is
                                                              used for verifying signatures on public key certificates}
 X509_KEY_USAGE_CRL_SIGN          = (1 shl 6); {cRLSign - The cRLSign bit is asserted when the subject public key is used
                                                          for verifying signatures on certificate revocation lists}
 X509_KEY_USAGE_ENCIPHER_ONLY     = (1 shl 7); {encipherOnly -  When the encipherOnly bit is asserted and the keyAgreement bit
                                                                is also set, the subject public key may be used only for
                                                                enciphering data while performing key agreement}
 X509_KEY_USAGE_DECIPHER_ONLY     = (1 shl 8); {decipherOnly - When the decipherOnly bit is asserted and the keyAgreement bit
                                                               is also set, the subject public key may be used only for
                                                               deciphering data while performing key agreement}

 {Certificate Extended Key Usage}
 X509_EXT_KEY_USAGE_ANY         = (1 shl 0);
 X509_EXT_KEY_USAGE_SERVER_AUTH = (1 shl 1);
 X509_EXT_KEY_USAGE_CLIENT_AUTH = (1 shl 2);
 X509_EXT_KEY_USAGE_OCSP        = (1 shl 3);

 {Certificate PEM Encoding}
 X509_PEM_LINE_END = #13#10;

 X509_PEM_CERTIFICATE_BEGIN = '-----BEGIN CERTIFICATE-----';
 X509_PEM_CERTIFICATE_END = '-----END CERTIFICATE-----';

 X509_PEM_PRIVATE_KEY_BEGIN = '-----BEGIN PRIVATE KEY-----'; {Private Key in PKCS#8 format (RFC5208 - Section 5)}
 X509_PEM_PRIVATE_KEY_END = '-----END PRIVATE KEY-----';

 X509_PEM_PUBLIC_KEY_BEGIN = '-----BEGIN PUBLIC KEY-----';
 X509_PEM_PUBLIC_KEY_END = '-----END PUBLIC KEY-----';

 X509_PEM_RSA_PRIVATE_KEY_BEGIN = '-----BEGIN RSA PRIVATE KEY-----'; {Private Key in PKCS#1 format (RFC3447 - Appendix A.1.2)}
 X509_PEM_RSA_PRIVATE_KEY_END = '-----END RSA PRIVATE KEY-----';

 X509_PEM_RSA_PUBLIC_KEY_BEGIN = '-----BEGIN RSA PUBLIC KEY-----';
 X509_PEM_RSA_PUBLIC_KEY_END = '-----END RSA PUBLIC KEY-----';

 X509_PEM_EC_PRIVATE_KEY_BEGIN = '-----BEGIN EC PRIVATE KEY-----';
 X509_PEM_EC_PRIVATE_KEY_END = '-----END EC PRIVATE KEY-----';
 
 X509_PEM_ENCRYPTED_PRIVATE_KEY_BEGIN = '-----BEGIN ENCRYPTED PRIVATE KEY-----'; {Encrypted Private Key in PKCS#8 format (RFC5208 - Section 6)}
 X509_PEM_ENCRYPTED_PRIVATE_KEY_END = '-----END ENCRYPTED PRIVATE KEY-----';

 X509_PEM_CERTIFICATE_REQUEST_BEGIN = '-----BEGIN CERTIFICATE REQUEST-----';
 X509_PEM_CERTIFICATE_REQUEST_END = '-----END CERTIFICATE REQUEST-----';

 X509_PEM_DH_PARAMETERS_BEGIN = '-----BEGIN DH PARAMETERS-----';
 X509_PEM_DH_PARAMETERS_END = '-----END DH PARAMETERS-----';

 X509_PEM_EC_PARAMETERS_BEGIN = '-----BEGIN EC PARAMETERS-----';
 X509_PEM_EC_PARAMETERS_END = '-----END EC PARAMETERS-----';
 
 {Certificate File Types}
 X509_FILETYPE_PEM     = 1;
 X509_FILETYPE_ASN1    = 2;

{==============================================================================}
type
 {X509 specific types}
 PX509SerialNumber = ^TX509SerialNumber;
 TX509SerialNumber = record
  Value:array[0..X509_MAX_SERIAL_NUM_LEN - 1] of Byte;
  Length:Integer;

  function ToString:String;
 end;

 PX509NameAttribute = ^TX509NameAttribute;
 TX509NameAttribute = record
  Value:String;
  _Type:LongWord;

  function ToString:String;
 end;

 PX509NameAttributes = ^TX509NameAttributes;
 TX509NameAttributes = array[0..X509_MAX_NAME_ATTRIBUTES - 1] of TX509NameAttribute;

 PX509AlgorithmIdentifier = ^TX509AlgorithmIdentifier;
 TX509AlgorithmIdentifier = record
  OID:TASN1OID;

  function ToString:String;
 end;

 PX509PublicKey = ^TX509PublicKey;
 TX509PublicKey = record
  Algorithm:TX509AlgorithmIdentifier;
  Key:PByte;
  Length:Integer;

  procedure Release;

  function ToString:String;
 end;

 PX509PrivateKey = ^TX509PrivateKey;
 TX509PrivateKey = record
  Algorithm:TX509AlgorithmIdentifier;
  Key:PByte;
  Length:Integer;

  procedure Release;

  function ToString:String;
 end;

 PX509Signature = ^TX509Signature;
 TX509Signature = record
  Algorithm:TX509AlgorithmIdentifier;
  Value:PByte;
  Length:Integer;

  procedure Release;
 end;

 {RSA Keys}
 PX509RSAPublicKey = ^TX509RSAPublicKey;
 TX509RSAPublicKey = record
  Modulus:PByte;              {M}
  PublicExponent:PByte;       {E}

  ModulusLen:Integer;
  PublicExponentLen:Integer;

  function ImportDER(ABuffer:Pointer;ASize:Integer):Boolean;
  procedure Release;
 end;

 PX509RSAPrivateKey = ^TX509RSAPrivateKey; {RFC3447 - Appendix A.1.2 - RSA private key syntax}
 TX509RSAPrivateKey = record
  Version:Integer;
  Modulus:PByte;             {M}
  PublicExponent:PByte;      {E}
  PrivateExponent:PByte;     {D}
  Prime1:PByte;              {P}
  Prime2:PByte;              {Q}
  Exponent1:PByte;           {D mod (P - 1)}
  Exponent2:PByte;           {D mod (Q - 1)}
  Coefficient:PByte;         {(Inverse of Q) mod P}

  ModulusLen:Integer;
  PublicExponentLen:Integer;
  PrivateExponentLen:Integer;
  Prime1Len:Integer;
  Prime2Len:Integer;
  Exponent1Len:Integer;
  Exponent2Len:Integer;
  CoefficientLen:Integer;

  function ImportDER(ABuffer:Pointer;ASize:Integer):Boolean;
  procedure Release;
 end;

 {ECC Keys}
 PX509ECDSAPublicKey = ^TX509ECDSAPublicKey;
 TX509ECDSAPublicKey = record
  //To Do //ECDSA/ECDH Public Key  //https://tools.ietf.org/html/rfc3279

  function ImportDER(ABuffer:Pointer;ASize:Integer):Boolean;
  procedure Release;
 end;

 PX509ECDSAPrivateKey = ^TX509ECDSAPrivateKey;
 TX509ECDSAPrivateKey = record
  //To Do //ECDSA/ECDH Private Key

  function ImportDER(ABuffer:Pointer;ASize:Integer):Boolean;
  procedure Release;
 end;

{==============================================================================}
type
 {X509 specific classes}
 TX509Name = class;
 TX509Certificate = class;

 TX509CertificateList = class(TObject)
 protected
  FList:TList;
  FLock: TCriticalSectionHandle;

  procedure Clear;
  
  function AcquireLock:Boolean;
  function ReleaseLock:Boolean;
 public
  constructor Create;
  destructor Destroy; override;

  function First:TX509Certificate; virtual;
  function Last:TX509Certificate; virtual;

  function Prev(ACertificate:TX509Certificate):TX509Certificate; virtual;
  function Next(ACertificate:TX509Certificate):TX509Certificate; virtual;

  function Add(ACertificate:TX509Certificate):Boolean; virtual;
  function Remove(ACertificate:TX509Certificate):Boolean; virtual;

  function FindByIssuer(AName:TX509Name):TX509Certificate; virtual;
  function FindBySubject(AName:TX509Name):TX509Certificate; virtual;

  function ImportDER(ABuffer:Pointer;ASize:Integer):TX509Certificate; virtual;
  function ImportPEM(ABuffer:Pointer;var ASize:Integer):TX509Certificate; virtual;

  function ExportDER(ABuffer:Pointer;var ASize:Integer;ACertificate:TX509Certificate):Boolean; virtual;
  function ExportPEM(ABuffer:Pointer;var ASize:Integer;AStart:TX509Certificate):Boolean; virtual;
 end;

 TX509CertificateChain = class(TObject)
 protected
  FRoot:TX509Certificate;
  FLock: TCriticalSectionHandle;

  procedure Clear; virtual;
  
  function AcquireLock:Boolean;
  function ReleaseLock:Boolean;
 public
  property Root:TX509Certificate read FRoot;

  constructor Create(ARoot:TX509Certificate); virtual;
  destructor Destroy; override;

  function Last:TX509Certificate; virtual;

  function Prev(ACertificate:TX509Certificate):TX509Certificate; virtual;
  function Next(ACertificate:TX509Certificate):TX509Certificate; virtual;

  function InsertAfter(AParent,ACertificate:TX509Certificate):Boolean; virtual;
  function InsertBefore(AChild,ACertificate:TX509Certificate):Boolean; virtual;
  function Remove(ACertificate:TX509Certificate):Boolean; virtual;

  function FindByIssuer(AName:TX509Name):TX509Certificate; virtual;
  function FindBySubject(AName:TX509Name):TX509Certificate; virtual;

  function FindBySubjectCN(const AName:String):TX509Certificate; virtual;
  function FindBySubjectDN(const AName:String):TX509Certificate; virtual;

  function ImportDER(ABuffer:Pointer;ASize:Integer;AParent:TX509Certificate):TX509Certificate; virtual;
  function ImportPEM(ABuffer:Pointer;var ASize:Integer;AParent:TX509Certificate):TX509Certificate; virtual;
  //function ImportPKCS8(ABuffer:Pointer;ASize:Integer;AParent:TX509Certificate):TX509Certificate; virtual; //To Do

  function ExportDER(ABuffer:Pointer;var ASize:Integer;ACertificate:TX509Certificate):Boolean; virtual;
  function ExportPEM(ABuffer:Pointer;var ASize:Integer;AStart:TX509Certificate):Boolean; virtual;

  function GetPathLength(ACertificate:TX509Certificate):LongWord; virtual;

  function ValidateChain(ATrust:TX509CertificateList):Integer; virtual;
 end;

 TX509Certificate = class(TObject)
 protected
  FList:TX509CertificateList;
  FChain:TX509CertificateChain;

  FParent:TX509Certificate;
  FChild:TX509Certificate;

  FData:PByte;       //Copy of certificate data from import
  FSize:LongWord;    //Total size of certificate data

  FTBSData:PByte;    //Pointer to start of TBS (To Be Signed) data
  FTBSSize:LongWord; //Length of TBS (To Be Signed) data

  function ImportTime(ABuffer:PByte;ASize:Integer;ATag:LongWord;var ADateTime:TDateTime):Boolean;

  function ImportName(ABuffer:PByte;ASize:Integer;AName:TX509Name;var ANext:PByte):Boolean;
  function ImportExtensionAltName(ABuffer:PByte;ASize:Integer;AName:TX509Name):Boolean;

  function ImportAlgorithmIdentifier(ABuffer:PByte;ASize:Integer;var AIdentifier:TX509AlgorithmIdentifier;var ANext:PByte):Boolean;

  function ImportValidity(ABuffer:PByte;ASize:Integer;var ANext:PByte):Boolean;

  function ImportPublicKey(ABuffer:PByte;ASize:Integer;var ANext:PByte):Boolean;

  function ImportExtension(ABuffer:PByte;ASize:Integer;var ANext:PByte):Boolean;
  function ImportExtensionData(ABuffer:PByte;ASize:Integer;const AOID:TASN1OID):Boolean;
  function ImportExtensionKeyUsage(ABuffer:PByte;ASize:Integer):Boolean;
  function ImportExtensionSubjectAltName(ABuffer:PByte;ASize:Integer):Boolean;
  function ImportExtensionIssuerAltName(ABuffer:PByte;ASize:Integer):Boolean;
  function ImportExtensionBasicContraints(ABuffer:PByte;ASize:Integer):Boolean;
  function ImportExtensionExtKeyUsage(ABuffer:PByte;ASize:Integer):Boolean;

  function ImportExtensions(ABuffer:PByte;ASize:Integer):Boolean;

  function ImportTBSCertificate(ABuffer:PByte;ASize:Integer;var ANext:PByte):Boolean;

  function VerifyRSASignature(AIssuer:TX509Certificate):Boolean;

  function VerifyMD5Digest(ABuffer:PByte;ASize:Integer):Boolean;
  function VerifySHA1Digest(ABuffer:PByte;ASize:Integer):Boolean;
  function VerifySHA256Digest(ABuffer:PByte;ASize:Integer):Boolean;
  function VerifySHA384Digest(ABuffer:PByte;ASize:Integer):Boolean;
  function VerifySHA512Digest(ABuffer:PByte;ASize:Integer):Boolean;
 public
  {Properties}
  Version:LongWord;
  SerialNumber:TX509SerialNumber;
  SignatureAlgorithm:TX509AlgorithmIdentifier;

  Issuer:TX509Name;
  Subject:TX509Name;
  SubjectDN:String;

  NotBefore:TDateTime;
  NotAfter:TDateTime;

  PublicKey:TX509PublicKey;

  Signature:TX509Signature;

  {Extensions}
  ExtensionsPresent:LongWord;

  CA:Boolean;
  PathLenConstraint:LongWord;

  KeyUsage:LongWord;
  ExtendedKeyUsage:LongWord;

  property List:TX509CertificateList read FList;
  property Chain:TX509CertificateChain read FChain;
  property Parent:TX509Certificate read FParent;
  property Child:TX509Certificate read FChild;

  constructor Create(AChain:TX509CertificateChain;AParent:TX509Certificate); virtual;
  destructor Destroy; override;

  function ImportDER(ABuffer:Pointer;ASize:Integer):Boolean; virtual;
  function ImportPEM(ABuffer:Pointer;var ASize:Integer):Boolean; virtual;

  function ExportDER(ABuffer:Pointer;var ASize:Integer):Boolean; virtual;
  function ExportPEM(ABuffer:Pointer;var ASize:Integer):Boolean; virtual;

  function IsValidIssuer:Boolean; virtual;
  function IsSelfSigned:Boolean; virtual;

  function VerifySignature(AIssuer:TX509Certificate):Boolean; virtual;
  function ValidateCertificate(AIssuer:TX509Certificate):Integer; virtual;
 end;

 TX509Name = class(TObject)
 private
  function StringCompare(const AValue1,AValue2:String):Integer;
 public
  NameAttributes:TX509NameAttributes;
  NameAttributeCount:LongWord;

  Email:String;          {emailAddress}

  {From subjectAltName extension}
  AltEmail:String;       {rfc822Name}
  DNS:String;            {dNSName}
  URI:String;            {uniformResourceIdentifier}
  IP:PByte;              {iPAddress}
  IPLen:Longword;        {IPv4 = 4,IPv6 = 16}
  RegisteredID:TASN1OID; {registeredID}

  destructor Destroy; override;

  function GetCN:String;
  function GetDN:String;

  function Compare(AName:TX509Name):Integer;
 end;

{==============================================================================}
{var}
 {X509 specific variables}

const
 {X509 specific constants}
 {Attribute Type Names}
 {OID 2.5.4 (Attribute Types)}
 OID_ATTRIBUTE_TYPES:TASN1OID = (Len: 3; OID: (2,5,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0));

 {OID 2.5.4.3 (commonName)}
 OID_ATTRIBUTE_TYPE_CN:TASN1OID = (Len: 4; OID: (2,5,4,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0));

 {OID 2.5.4.6 (countryName)}
 OID_ATTRIBUTE_TYPE_C:TASN1OID = (Len: 4; OID: (2,5,4,6,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0));

 {OID 2.5.4.7 (localityName)}
 OID_ATTRIBUTE_TYPE_L:TASN1OID = (Len: 4; OID: (2,5,4,7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0));

 {OID 2.5.4.8 (stateOrProvinceName)}
 OID_ATTRIBUTE_TYPE_ST:TASN1OID = (Len: 4; OID: (2,5,4,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0));

 {OID 2.5.4.10 (organizationName)}
 OID_ATTRIBUTE_TYPE_O:TASN1OID = (Len: 4; OID: (2,5,4,10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0));

 {OID 2.5.4.11 (organizationalUnitName)}
 OID_ATTRIBUTE_TYPE_OU:TASN1OID = (Len: 4; OID: (2,5,4,11,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0));

 {OID 1.2.840.113549.1.9.1 (e-mailAddress)}
 OID_EMAIL_ADDRESS:TASN1OID = (Len: 7; OID: (1,2,840,113549,1,9,1,0,0,0,0,0,0,0,0,0,0,0,0,0));

 {OID 0.9.2342.19200300.100.1.25 (domainComponent)}
 OID_ATTRIBUTE_TYPE_DC:TASN1OID = (Len: 7; OID: (0,9,2342,19200300,100,1,250,0,0,0,0,0,0,0,0,0,0,0,0,0));

 {Certificate Extensions}
 {OID 2.5.29 (id-ce)}
 OID_ID_CE:TASN1OID = (Len: 3; OID: (2,5,29,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0));

 {OID 2.5.29.15 (id-ce-keyUsage)}
 OID_ID_CE_KEY_USAGE:TASN1OID = (Len: 4; OID: (2,5,29,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0));

 {OID 2.5.29.17 (id-ce-subjectAltName)}
 OID_ID_CE_SUBJECT_ALT_NAME:TASN1OID = (Len: 4; OID: (2,5,29,17,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0));

 {OID 2.5.29.18 (id-ce-issuerAltName)}
 OID_ID_CE_ISSUER_ALT_NAME:TASN1OID = (Len: 4; OID: (2,5,29,18,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0));

 {OID 2.5.29.19 (id-ce-basicConstraints)}
 OID_ID_CE_BASIC_CONTRAINTS:TASN1OID = (Len: 4; OID: (2,5,29,19,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0));

 {OID 2.5.29.31 (id-ce-cRLDistributionPoints)}
 OID_ID_CE_CRL_DISTRIBUTION_POINTS:TASN1OID = (Len: 4; OID: (2,5,29,31,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0));

 {OID 2.5.29.32 (id-ce-certificatePolicies)}
 OID_ID_CE_CERTIFICATE_POLICIES:TASN1OID = (Len: 4; OID: (2,5,29,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0));

 {OID 2.5.29.37 (id-ce-extKeyUsage)}
 OID_ID_CE_EXT_KEY_USAGE:TASN1OID = (Len: 4; OID: (2,5,29,37,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0));

 {OID 2.5.29.37.0 (id-ce-anyExtendedKeyUsage)}
 OID_ID_CE_EXT_KEY_USAGE_ANY:TASN1OID = (Len: 5; OID: (2,5,29,37,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0));

 {Private Certificate Extensions}
 {OID 1.3.6.1.5.5.7.1 (id-pe)}
 OID_ID_PE:TASN1OID = (Len: 8; OID: (1,3,6,1,5,5,7,1,0,0,0,0,0,0,0,0,0,0,0,0));

 {OID 1.3.6.1.5.5.7.1.1 (id-pe-authorityInfoAccess)}
 OID_ID_PE_AUTHORITY_INFO_ACCESS:TASN1OID = (Len: 9; OID: (1,3,6,1,5,5,7,1,1,0,0,0,0,0,0,0,0,0,0,0));

 {Extended Key Purposes}
 {OID 1.3.6.1.5.5.7.3.1 (id-kp-serverAuth)}
 OID_ID_KP_SERVER_AUTH:TASN1OID = (Len: 9; OID: (1,3,6,1,5,5,7,3,1,0,0,0,0,0,0,0,0,0,0,0));

 {OID 1.3.6.1.5.5.7.3.2 (id-kp-clientAuth)}
 OID_ID_KP_CLIENT_AUTH:TASN1OID = (Len: 9; OID: (1,3,6,1,5,5,7,3,2,0,0,0,0,0,0,0,0,0,0,0));

 {OID 1.3.6.1.5.5.7.3.9 (id-kp-OCSPSigning)}
 OID_ID_KP_OCSP_SIGNING:TASN1OID = (Len: 9; OID: (1,3,6,1,5,5,7,3,9,0,0,0,0,0,0,0,0,0,0,0));

 {Hash Algorithms}
 {OID 2.16.840.1.101.3.4.2 (Hash Algorithms)}
 OID_HASH_ALGORITHMS:TASN1OID = (Len: 8; OID: (2,16,840,1,101,3,4,2,0,0,0,0,0,0,0,0,0,0,0,0));

 {OID 2.16.840.1.101.3.4.2.1 (SHA256)}
 OID_HASH_SHA256:TASN1OID = (Len: 9; OID: (2,16,840,1,101,3,4,2,1,0,0,0,0,0,0,0,0,0,0,0));

 {OID 2.16.840.1.101.3.4.2.2 (SHA384)}
 OID_HASH_SHA384:TASN1OID = (Len: 9; OID: (2,16,840,1,101,3,4,2,2,0,0,0,0,0,0,0,0,0,0,0));

 {OID 2.16.840.1.101.3.4.2.3 (SHA512)}
 OID_HASH_SHA512:TASN1OID = (Len: 9; OID: (2,16,840,1,101,3,4,2,3,0,0,0,0,0,0,0,0,0,0,0));

 {Algorithms}
 {OID 1.3.14.3.2.26 (SHA1)}
 OID_HASH_SHA1:TASN1OID = (Len: 6; OID: (1,3,14,3,2,26,0,0,0,0,0,0,0,0,0,0,0,0,0,0));

 {OID 1.3.14.3.2.29 (sha1WithRSAEncryption)}
 OID_HASH_SHA1_RSA:TASN1OID = (Len: 6; OID: (1,3,14,3,2,29,0,0,0,0,0,0,0,0,0,0,0,0,0,0));

 {PKCS-1 Algorithms}
 {OID 1.2.840.113549.1.1 (PKCS-1)}
 OID_PKCS1_ALGORITHMS:TASN1OID = (Len: 6; OID: (1,2,840,113549,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0));

 {OID 1.2.840.113549.1.1.1 (rsaEncryption)}
 OID_PKCS1_RSA:TASN1OID = (Len: 7; OID: (1,2,840,113549,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0));

 {OID 1.2.840.113549.1.1.4 (md5WithRSAEncryption)}
 OID_PKCS1_MD5_RSA:TASN1OID = (Len: 7; OID: (1,2,840,113549,1,1,4,0,0,0,0,0,0,0,0,0,0,0,0,0));

 {OID 1.2.840.113549.1.1.5 (sha1WithRSAEncryption)}
 OID_PKCS1_SHA1_RSA:TASN1OID = (Len: 7; OID: (1,2,840,113549,1,1,5,0,0,0,0,0,0,0,0,0,0,0,0,0));

 {OID 1.2.840.113549.1.1.11 (sha256WithRSAEncryption)}
 OID_PKCS1_SHA256_RSA:TASN1OID = (Len: 7; OID: (1,2,840,113549,1,1,11,0,0,0,0,0,0,0,0,0,0,0,0,0));

 {OID 1.2.840.113549.1.1.12 (sha384WithRSAEncryption)}
 OID_PKCS1_SHA384_RSA:TASN1OID = (Len: 7; OID: (1,2,840,113549,1,1,12,0,0,0,0,0,0,0,0,0,0,0,0,0));

 {OID 1.2.840.113549.1.1.13 (sha512WithRSAEncryption)}
 OID_PKCS1_SHA512_RSA:TASN1OID = (Len: 7; OID: (1,2,840,113549,1,1,12,0,0,0,0,0,0,0,0,0,0,0,0,0));

 {PKCS-2 Algorithms}
 {OID 1.2.840.113549.2 (PKCS-2)}
 OID_PKCS2_ALGORITHMS:TASN1OID = (Len: 5; OID: (1,2,840,113549,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0));

 {OID 1.2.840.113549.2.5 (MD5)}
 OID_PKCS2_MD5:TASN1OID = (Len: 6; OID: (1,2,840,113549,2,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0));

{==============================================================================}
{X509 Functions}

{==============================================================================}
{X509 Helper Functions}
function X509NameAttributeTypeToString(AType:LongWord):String;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{var}
 {X509 specific variables}

{==============================================================================}
{==============================================================================}
{TX509SerialNumber}
function TX509SerialNumber.ToString:String;
var
  Count:Integer;
begin
 {}
 Result:='';

 for Count:=0 to Length - 1 do
  begin
   Result:=Result + IntToHex(Value[Count],2);
  end;
end;

{==============================================================================}
{==============================================================================}
{TX509NameAttribute}
function TX509NameAttribute.ToString:String;
begin
 {}
 Result:=X509NameAttributeTypeToString(_Type) + '=' + Value;
end;

{==============================================================================}
{==============================================================================}
{TX509AlgorithmIdentifier}
function TX509AlgorithmIdentifier.ToString:String;
begin
 {}
 Result:=ASN1OIDToString(OID);
end;

{==============================================================================}
{==============================================================================}
{TX509PublicKey}
procedure TX509PublicKey.Release;
begin
 {}
 {Clear Algorithm}
 FillChar(Algorithm.OID,SizeOf(TASN1OID),0);

 {Release Key}
 if Key <> nil then FreeMem(Key);
 Key:=nil;

 {Reset Length}
 Length:=0;
end;

{==============================================================================}

function TX509PublicKey.ToString:String;
var
 Count:Integer;
begin
 {}
 Result:='';

 if Key = nil then Exit;

 for Count:=0 to Length - 1 do
  begin
   Result:=Result + IntToHex(Key[Count],2);
  end;
end;

{==============================================================================}
{==============================================================================}
{TX509PrivateKey}
procedure TX509PrivateKey.Release;
begin
 {}
 {Clear Algorithm}
 FillChar(Algorithm.OID,SizeOf(TASN1OID),0);

 {Release Key}
 if Key <> nil then FreeMem(Key);
 Key:=nil;

 {Reset Length}
 Length:=0;
end;

{==============================================================================}

function TX509PrivateKey.ToString:String;
var
 Count:Integer;
begin
 {}
 Result:='';

 if Key = nil then Exit;

 for Count:=0 to Length - 1 do
  begin
   Result:=Result + IntToHex(Key[Count],2);
  end;
end;

{==============================================================================}
{==============================================================================}
{TX509Signature}
procedure TX509Signature.Release;
begin
 {}
 {Clear Algorithm}
 FillChar(Algorithm.OID,SizeOf(TASN1OID),0);

 {Release Value}
 if Value <> nil then FreeMem(Value);
 Value:=nil;

 {Reset Length}
 Length:=0;
end;

{==============================================================================}
{==============================================================================}
{TX509RSAPublicKey}
function TX509RSAPublicKey.ImportDER(ABuffer:Pointer;ASize:Integer):Boolean;
{Import an RSA Public Key from a DER encoded buffer}
var
 Next:PByte;
 Last:PByte;
 Tag:TASN1Tag;
begin
 {}
 Result:=False;

 {RSAPublicKey ::= SEQUENCE (
     modulus INTEGER, -- n
     publicExponent INTEGER -- e)}

 {$IFDEF X509_DEBUG}       
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509RSAPublicKey.ImportDER Size = ' + IntToStr(ASize));
 {$ENDIF}

 if ABuffer = nil then Exit;

 {Get RSAPublicKey (Sequence)}
 if not ASN1GetTag(ABuffer,ASize,Tag) then Exit;

 {Check Tag}
 if (Tag.TagClass <> ASN1_CLASS_UNIVERSAL) or (Tag.TagNumber <> ASN1_TAG_SEQUENCE) then Exit;

 {Get Pointers}
 Next:=Tag.Contents;
 Last:=Next + Tag.Length;

 {Get Modulus (Integer)}
 if not ASN1GetTag(Next,Last - Next,Tag) then Exit;

 {Check Tag}
 if (Tag.TagClass <> ASN1_CLASS_UNIVERSAL) or (Tag.TagNumber <> ASN1_TAG_INTEGER) then Exit;

 {Import Modulus}
 if not ASN1ParseBigInt(Tag.Contents,Tag.Length,Modulus,ModulusLen) then Exit;

 Next:=Tag.Contents + Tag.Length;

 {Get PublicExponent (Integer)}
 if not ASN1GetTag(Next,Last - Next,Tag) then Exit;

 {Check Tag}
 if (Tag.TagClass <> ASN1_CLASS_UNIVERSAL) or (Tag.TagNumber <> ASN1_TAG_INTEGER) then Exit;

 {Import PublicExponent}
 if not ASN1ParseBigInt(Tag.Contents,Tag.Length,PublicExponent,PublicExponentLen) then Exit;

 Next:=Tag.Contents + Tag.Length;

 {Check End}
 if Next <> Last then Exit;

 Result:=True;
end;

{==============================================================================}

procedure TX509RSAPublicKey.Release;
begin
 {}
 {Release Modulus}
 if Modulus <> nil then FreeMem(Modulus);
 Modulus:=nil;

 {Release PublicExponent}
 if PublicExponent <> nil then FreeMem(PublicExponent);
 PublicExponent:=nil;

 {Reset Lengths}
 ModulusLen:=0;
 PublicExponentLen:=0;
end;

{==============================================================================}
{==============================================================================}
{TX509RSAPrivateKey}
function TX509RSAPrivateKey.ImportDER(ABuffer:Pointer;ASize:Integer):Boolean;
{Import an RSA Private Key from a DER encoded buffer}
var
 Next:PByte;
 Last:PByte;
 Tag:TASN1Tag;
begin
 {}
 Result:=False;

 {RSAPrivateKey ::= SEQUENCE (
     version Version,
     modulus INTEGER, -- n
     publicExponent INTEGER, -- e
     privateExponent INTEGER, -- d
     prime1 INTEGER, -- p
     prime2 INTEGER, -- q
     exponent1 INTEGER, -- d mod (p-1)
     exponent2 INTEGER, -- d mod (q-1)
     coefficient INTEGER -- (inverse of q) mod p
  )

  Version ::= INTEGER -- shall be 0 for this version of the standard}

 {$IFDEF X509_DEBUG}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509RSAPrivateKey.ImportDER Size = ' + IntToStr(ASize));
 {$ENDIF}
    
 if ABuffer = nil then Exit;

 {Get RSAPublicKey (Sequence)}
 if not ASN1GetTag(ABuffer,ASize,Tag) then Exit;

 {Check Tag}
 if (Tag.TagClass <> ASN1_CLASS_UNIVERSAL) or (Tag.TagNumber <> ASN1_TAG_SEQUENCE) then Exit;

 {Get Pointers}
 Next:=Tag.Contents;
 Last:=Next + Tag.Length;

 {Get Version (Integer)}
 if not ASN1GetTag(Next,Last - Next,Tag) then Exit;

 {Check Tag}
 if (Tag.TagClass <> ASN1_CLASS_UNIVERSAL) or (Tag.TagNumber <> ASN1_TAG_INTEGER) then Exit;

 {Check Version}
 if not ASN1ParseInt(Tag.Contents,Tag.Length,Version) then Exit;
 if Version <> 0 then Exit;

 Next:=Tag.Contents + Tag.Length;

 {Get Modulus (Integer)}
 if not ASN1GetTag(Next,Last - Next,Tag) then Exit;

 {Check Tag}
 if (Tag.TagClass <> ASN1_CLASS_UNIVERSAL) or (Tag.TagNumber <> ASN1_TAG_INTEGER) then Exit;

 {Import Modulus}
 if not ASN1ParseBigInt(Tag.Contents,Tag.Length,Modulus,ModulusLen) then Exit;

 Next:=Tag.Contents + Tag.Length;

 {Get PublicExponent (Integer)}
 if not ASN1GetTag(Next,Last - Next,Tag) then Exit;

 {Check Tag}
 if (Tag.TagClass <> ASN1_CLASS_UNIVERSAL) or (Tag.TagNumber <> ASN1_TAG_INTEGER) then Exit;

 {Import PublicExponent}
 if not ASN1ParseBigInt(Tag.Contents,Tag.Length,PublicExponent,PublicExponentLen) then Exit;

 Next:=Tag.Contents + Tag.Length;

 {Get PrivateExponent (Integer)}
 if not ASN1GetTag(Next,Last - Next,Tag) then Exit;

 {Check Tag}
 if (Tag.TagClass <> ASN1_CLASS_UNIVERSAL) or (Tag.TagNumber <> ASN1_TAG_INTEGER) then Exit;

 {Import PrivateExponent}
 if not ASN1ParseBigInt(Tag.Contents,Tag.Length,PrivateExponent,PrivateExponentLen) then Exit;

 Next:=Tag.Contents + Tag.Length;

 {Get Prime1 (Integer)}
 if not ASN1GetTag(Next,Last - Next,Tag) then Exit;

 {Check Tag}
 if (Tag.TagClass <> ASN1_CLASS_UNIVERSAL) or (Tag.TagNumber <> ASN1_TAG_INTEGER) then Exit;

 {Import Prime1}
 if not ASN1ParseBigInt(Tag.Contents,Tag.Length,Prime1,Prime1Len) then Exit;

 Next:=Tag.Contents + Tag.Length;

 {Get Prime2 (Integer)}
 if not ASN1GetTag(Next,Last - Next,Tag) then Exit;

 {Check Tag}
 if (Tag.TagClass <> ASN1_CLASS_UNIVERSAL) or (Tag.TagNumber <> ASN1_TAG_INTEGER) then Exit;

 {Import Prime2}
 if not ASN1ParseBigInt(Tag.Contents,Tag.Length,Prime2,Prime2Len) then Exit;

 Next:=Tag.Contents + Tag.Length;

 {Get Exponent1 (Integer)}
 if not ASN1GetTag(Next,Last - Next,Tag) then Exit;

 {Check Tag}
 if (Tag.TagClass <> ASN1_CLASS_UNIVERSAL) or (Tag.TagNumber <> ASN1_TAG_INTEGER) then Exit;

 {Import Exponent1}
 if not ASN1ParseBigInt(Tag.Contents,Tag.Length,Exponent1,Exponent1Len) then Exit;

 Next:=Tag.Contents + Tag.Length;

 {Get Exponent2 (Integer)}
 if not ASN1GetTag(Next,Last - Next,Tag) then Exit;

 {Check Tag}
 if (Tag.TagClass <> ASN1_CLASS_UNIVERSAL) or (Tag.TagNumber <> ASN1_TAG_INTEGER) then Exit;

 {Import Exponent2}
 if not ASN1ParseBigInt(Tag.Contents,Tag.Length,Exponent2,Exponent2Len) then Exit;

 Next:=Tag.Contents + Tag.Length;

 {Get Coefficient (Integer)}
 if not ASN1GetTag(Next,Last - Next,Tag) then Exit;

 {Check Tag}
 if (Tag.TagClass <> ASN1_CLASS_UNIVERSAL) or (Tag.TagNumber <> ASN1_TAG_INTEGER) then Exit;

 {Import Coefficient}
 if not ASN1ParseBigInt(Tag.Contents,Tag.Length,Coefficient,CoefficientLen) then Exit;

 Next:=Tag.Contents + Tag.Length;

 {Check End}
 if Next <> Last then Exit;
 
 Result:=True;
end;

{==============================================================================}

procedure TX509RSAPrivateKey.Release;
begin
 {}
 {Reset Version}
 Version:=0;

 {Release Modulus}
 if Modulus <> nil then FreeMem(Modulus);
 Modulus:=nil;

 {Release PublicExponent and PrivateExponent}
 if PublicExponent <> nil then FreeMem(PublicExponent);
 PublicExponent:=nil;

 if PrivateExponent <> nil then FreeMem(PrivateExponent);
 PrivateExponent:=nil;

 {Release Prime1 and Prime2}
 if Prime1 <> nil then FreeMem(Prime1);
 Prime1:=nil;

 if Prime2 <> nil then FreeMem(Prime2);
 Prime2:=nil;

 {Release Exponent1 and Exponent2}
 if Exponent1 <> nil then FreeMem(Exponent1);
 Exponent1:=nil;

 if Exponent2 <> nil then FreeMem(Exponent2);
 Exponent2:=nil;

 {Release Coefficient}
 if Coefficient <> nil then FreeMem(Coefficient);
 Coefficient:=nil;

 {Reset Lengths}
 ModulusLen:=0;
 PublicExponentLen:=0;
 PrivateExponentLen:=0;
 Prime1Len:=0;
 Prime2Len:=0;
 Exponent1Len:=0;
 Exponent2Len:=0;
 CoefficientLen:=0;
end;

{==============================================================================}
{==============================================================================}
{TX509ECDSAPublicKey}
function TX509ECDSAPublicKey.ImportDER(ABuffer:Pointer;ASize:Integer):Boolean;
begin
 {}
 Result:=False;

 if ABuffer = nil then Exit;

 //To Do

end;

{==============================================================================}

procedure TX509ECDSAPublicKey.Release;
begin
 {}

 //To Do

end;

{==============================================================================}
{==============================================================================}
{TX509ECDSAPrivateKey}
function TX509ECDSAPrivateKey.ImportDER(ABuffer:Pointer;ASize:Integer):Boolean;
begin
 {}
 Result:=False;

 if ABuffer = nil then Exit;

 //To Do

end;

{==============================================================================}

procedure TX509ECDSAPrivateKey.Release;
begin
 {}

 //To Do

end;

{==============================================================================}
{==============================================================================}
{TX509CertificateList}
procedure TX509CertificateList.Clear;
var
 Count:Integer;
 Certificate:TX509Certificate;
begin
 {}
 AcquireLock;

 for Count:=FList.Count - 1 downto 0 do
  begin
   Certificate:=TX509Certificate(FList.Items[Count]);
   Certificate.Free;
  end;

 ReleaseLock;
end;

{==============================================================================}

function TX509CertificateList.AcquireLock:Boolean;
begin
 {}
 Result:=(CriticalSectionLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TX509CertificateList.ReleaseLock:Boolean;
begin
 {}
 Result:=(CriticalSectionUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

constructor TX509CertificateList.Create;
begin
 {}
 inherited Create;
 FList:=TList.Create;
 FLock:=CriticalSectionCreate;
end;

{==============================================================================}

destructor TX509CertificateList.Destroy;
begin
 {}
 AcquireLock;
 try
  Clear;
  FList.Free;
  inherited Destroy;
 finally
  {ReleaseLock;} {Can destroy Critical Section while holding lock} 
  CriticalSectionDestroy(FLock);
 end;
end;

{==============================================================================}

function TX509CertificateList.First:TX509Certificate;
begin
 {}
 Result := nil;

 AcquireLock;

 if FList.Count > 0 then
  begin
   Result:=FList.Items[0];
  end;

 ReleaseLock;
end;

{==============================================================================}

function TX509CertificateList.Last:TX509Certificate;
begin
 {}
 Result := nil;

 AcquireLock;

 if FList.Count > 0 then
  begin
   Result:=FList.Items[FList.Count - 1];
  end;

 ReleaseLock;
end;

{==============================================================================}

function TX509CertificateList.Prev(ACertificate:TX509Certificate):TX509Certificate;
var
 Index:Integer;
begin
 {}
 Result := nil;

 if ACertificate = nil then Exit;

 AcquireLock;

 if FList.Count > 0 then
  begin
   Index:=FList.IndexOf(ACertificate);
   if Index > 1 then
    begin
     Result:=FList.Items[Index - 1];
    end;
  end;

 ReleaseLock;
end;

{==============================================================================}

function TX509CertificateList.Next(ACertificate:TX509Certificate):TX509Certificate;
var
 Index:Integer;
begin
 {}
 Result := nil;

 if ACertificate = nil then Exit;

 AcquireLock;

 if FList.Count > 0 then
  begin
   Index:=FList.IndexOf(ACertificate);
   if (Index <> -1) and ((FList.Count) > Index + 1) then
    begin
     Result:=FList.Items[Index + 1];
    end;
  end;

 ReleaseLock;
end;

{==============================================================================}

function TX509CertificateList.Add(ACertificate:TX509Certificate):Boolean;
begin
 {}
 Result:=False;

 if ACertificate = nil then Exit;

 if ACertificate.FList <> nil then Exit;

 AcquireLock;

 Result:=FList.Add(ACertificate) >= 0;

 if Result then ACertificate.FList:=Self;

 ReleaseLock;
end;

{==============================================================================}

function TX509CertificateList.Remove(ACertificate:TX509Certificate):Boolean;
begin
 {}
 Result:=False;

 if ACertificate = nil then Exit;

 if ACertificate.FList <> Self then Exit;

 AcquireLock;

 Result:=FList.Remove(ACertificate) >= 0;

 if Result then ACertificate.FList:=nil;

 ReleaseLock;
end;

{==============================================================================}

function TX509CertificateList.FindByIssuer(AName:TX509Name):TX509Certificate;
var
 Count:Integer;
 Certificate:TX509Certificate;
begin
 {}
 Result:=nil;

 if AName = nil then Exit;

 AcquireLock;

 {Check Certificates}
 for Count:=0 to FList.Count - 1 do
  begin
   Certificate:=FList.Items[Count];
   if (Certificate <> nil) and (Certificate.Issuer.Compare(AName) = 0) then
    begin
     Result:=Certificate;
     Break;
    end;
  end;

 ReleaseLock;
end;

{==============================================================================}

function TX509CertificateList.FindBySubject(AName:TX509Name):TX509Certificate;
var
 Count:Integer;
 Certificate:TX509Certificate;
begin
 {}
 Result:=nil;

 if AName = nil then Exit;

 AcquireLock;

 {Check Certificates}
 for Count:=0 to FList.Count - 1 do
  begin
   Certificate:=FList.Items[Count];
   if (Certificate <> nil) and (Certificate.Subject.Compare(AName) = 0) then
    begin
     Result:=Certificate;
     Break;
    end;
  end;

 ReleaseLock;
end;

{==============================================================================}

function TX509CertificateList.ImportDER(ABuffer:Pointer;ASize:Integer):TX509Certificate;
var
 Certificate:TX509Certificate;
begin
 {}
 Result:=nil;

 if ABuffer = nil then Exit;

 {Create and Import Certificate from DER}
 Certificate:=TX509Certificate.Create(nil,nil);
 if not Certificate.ImportDER(ABuffer,ASize) then
  begin
   Certificate.Free;
   Exit;
  end;

 {Add Certificate}
 Add(Certificate);

 Result:=Certificate;
end;

{==============================================================================}

function TX509CertificateList.ImportPEM(ABuffer:Pointer;var ASize:Integer):TX509Certificate;
var
 Certificate:TX509Certificate;
begin
 {}
 Result:=nil;

 if ABuffer = nil then Exit;

 {Create and Import Certificate from PEM}
 Certificate:=TX509Certificate.Create(nil,nil);
 if not Certificate.ImportPEM(ABuffer,ASize) then
  begin
   Certificate.Free;
   Exit;
  end;

 {Add Certificate}
 Add(Certificate);

 Result:=Certificate;
end;

{==============================================================================}

function TX509CertificateList.ExportDER(ABuffer:Pointer;var ASize:Integer;ACertificate:TX509Certificate):Boolean;
begin
 {}
 Result:=False;

 if ACertificate = nil then Exit;

 if ACertificate.FList <> Self then Exit;

 {Export Certificate}
 Result:=ACertificate.ExportDER(ABuffer,ASize);
end;

{==============================================================================}

function TX509CertificateList.ExportPEM(ABuffer:Pointer;var ASize:Integer;AStart:TX509Certificate):Boolean;
var
 Size:Integer;
 Start:Integer;
 Count:Integer;
 Remain:Integer;
 Offset:PtrUInt;
 Certificate:TX509Certificate;
begin
 {}
 Result:=False;

 if AStart = nil then Exit;

 if AStart.FList <> Self then Exit;

 if ASize = 0 then Exit;

 AcquireLock;

 {Get Start}
 Start:=0;
 Remain:=ASize;
 Offset:=0;

 {Find Start}
 for Count:=0 to FList.Count - 1 do
  begin
   Certificate:=FList.Items[Count];
   if Certificate = AStart then
    begin
     Start:=Count;
     Break;
    end;
  end;

 {Export Certificates}
 for Count:=Start to FList.Count - 1 do
  begin
   Certificate:=FList.Items[Count];
   if Certificate <> nil then
    begin
     {Get Size}
     Size:=Remain;

     {Export Certificate}
     Result:=Certificate.ExportPEM(Pointer(PtrUInt(ABuffer) + Offset),Size);
     if not Result then Break;

     {Update Position}
     Dec(Remain,Size);
     Inc(Offset,Size);
    end;
  end;

 ReleaseLock;

 {Update Size}
 if Result then ASize:=Offset;
end;

{==============================================================================}
{==============================================================================}
{TX509CertificateChain}
procedure TX509CertificateChain.Clear;
var
 Next:TX509Certificate;
 Current:TX509Certificate;
begin
 {}
 AcquireLock;

 Next:=FRoot;
 while Next <> nil do
  begin
   Current:=Next;
   Next:=Current.Child;

   Current.FChain:=nil;
   Current.Free;
  end;

 ReleaseLock;
end;

{==============================================================================}

function TX509CertificateChain.AcquireLock:Boolean;
begin
 {}
 Result:=(CriticalSectionLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TX509CertificateChain.ReleaseLock:Boolean;
begin
 {}
 Result:=(CriticalSectionUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

constructor TX509CertificateChain.Create(ARoot:TX509Certificate);
begin
 {}
 inherited Create;
 FRoot:=ARoot;
 FLock:=CriticalSectionCreate;
end;

{==============================================================================}

destructor TX509CertificateChain.Destroy;
begin
 {}
 AcquireLock;
 try
  Clear;

  inherited Destroy;
 finally
  {ReleaseLock;} {Can destroy Critical Section while holding lock} 
  CriticalSectionDestroy(FLock);
 end;
end;

{==============================================================================}

function TX509CertificateChain.Last:TX509Certificate;
var
 Certificate:TX509Certificate;
begin
 {}
 Result:=nil;

 AcquireLock;

 {Get Root}
 Certificate:=Root;
 while Certificate <> nil do
  begin
   {Check Child}
   if Certificate.Child = nil then
    begin
     Result:=Certificate;
     Break;
    end;

   {Get Next}
   Certificate:=Next(Certificate);
  end;

 ReleaseLock;
end;

{==============================================================================}

function TX509CertificateChain.Prev(ACertificate:TX509Certificate):TX509Certificate;
begin
 {}
 Result:=nil;

 if ACertificate = nil then Exit;

 if ACertificate.Chain <> Self then Exit;

 Result:=ACertificate.Parent;
end;

{==============================================================================}

function TX509CertificateChain.Next(ACertificate:TX509Certificate):TX509Certificate;
begin
 {}
 Result:=nil;

 if ACertificate = nil then Exit;

 if ACertificate.Chain <> Self then Exit;

 Result:=ACertificate.Child;
end;

{==============================================================================}

function TX509CertificateChain.InsertAfter(AParent,ACertificate:TX509Certificate):Boolean;
{Insert a certificate in the chain after the supplied parent, if parent is nil then insert
 after the root of the chain}
begin
 {}
 Result:=False;

 if ACertificate = nil then Exit;

 if ACertificate.FChain <> nil then Exit;

 if AParent <> nil then
  begin
   {Insert after parent}
   AcquireLock;

   if AParent.Child <> nil then
    begin
     ACertificate.FChild:=AParent.Child;
     ACertificate.Child.FParent:=ACertificate;
    end;

   AParent.FChild:=ACertificate;
   ACertificate.FParent:=AParent;
   ACertificate.FChain:=Self;

   ReleaseLock;
  end
 else
  begin
   {Insert after root}
   AcquireLock;

   if FRoot = nil then
    begin
     FRoot:=ACertificate;
    end
   else
    begin
     if FRoot.Child <> nil then
      begin
       ACertificate.FChild:=FRoot.Child;
       ACertificate.Child.FParent:=ACertificate;
      end;

     FRoot.FChild:=ACertificate;
     ACertificate.FParent:=FRoot;
     ACertificate.FChain:=Self;
    end;

   ACertificate.FChain:=Self;

   ReleaseLock;
  end;

 Result:=True;
end;

{==============================================================================}

function TX509CertificateChain.InsertBefore(AChild,ACertificate:TX509Certificate):Boolean;
{Insert a certificate in the chain before the supplied child, if child is nil then insert
 before the root of the chain}
begin
 {}
 Result:=False;

 if ACertificate = nil then Exit;

 if ACertificate.FChain <> nil then Exit;

 if AChild <> nil then
  begin
   {Insert before child}
   if AChild.Parent <> nil then
    begin
     ACertificate.FParent:=AChild.Parent;
     ACertificate.Parent.FChild:=ACertificate;
    end;

   if AChild = FRoot then
    begin
     FRoot:=ACertificate;
    end;

   AChild.FParent:=ACertificate;
   ACertificate.FChild:=AChild;
   ACertificate.FChain:=Self;
  end
 else
  begin
   {Insert before root}
   if FRoot <> nil then
    begin
     FRoot.FParent:=ACertificate;
     ACertificate.FChild:=FRoot;
    end;

   FRoot:=ACertificate;
   ACertificate.FChain:=Self;
  end;

 Result:=True;
end;

{==============================================================================}

function TX509CertificateChain.Remove(ACertificate:TX509Certificate):Boolean;
begin
 {}
 Result:=False;

 if ACertificate = nil then Exit;

 if ACertificate.FChain <> Self then Exit;

 if ACertificate.Parent <> nil then
  begin
   AcquireLock;

   ACertificate.Parent.FChild:=ACertificate.Child;
   if ACertificate.Child <> nil then
    begin
     ACertificate.Child.FParent:=ACertificate.Parent;
    end;

   ACertificate.FParent:=nil;
   ACertificate.FChild:=nil;
   ACertificate.FChain:=nil;

   ReleaseLock;
  end
 else
  begin
   AcquireLock;

   FRoot:=ACertificate.Child;
   if ACertificate.Child <> nil then
    begin
     ACertificate.Child.FParent:=nil;
    end;

   ACertificate.FChild:=nil;
   ACertificate.FChain:=nil;

   ReleaseLock;
  end;

 Result:=True;
end;

{==============================================================================}

function TX509CertificateChain.FindByIssuer(AName:TX509Name):TX509Certificate;
var
 Certificate:TX509Certificate;
begin
 {}
 Result:=nil;

 if AName = nil then Exit;

 AcquireLock;

 {Get Root}
 Certificate:=Root;
 while Certificate <> nil do
  begin
   if Certificate.Issuer.Compare(AName) = 0 then
    begin
     Result:=Certificate;
     Break;
    end;

   {Get Next}
   Certificate:=Next(Certificate);
  end;

 ReleaseLock;
end;

{==============================================================================}

function TX509CertificateChain.FindBySubject(AName:TX509Name):TX509Certificate;
var
 Certificate:TX509Certificate;
begin
 {}
 Result:=nil;

 if AName = nil then Exit;

 AcquireLock;

 {Get Root}
 Certificate:=Root;
 while Certificate <> nil do
  begin
   if Certificate.Subject.Compare(AName) = 0 then
    begin
     Result:=Certificate;
     Break;
    end;

   {Get Next}
   Certificate:=Next(Certificate);
  end;

 ReleaseLock;
end;

{==============================================================================}

function TX509CertificateChain.FindBySubjectCN(const AName:String):TX509Certificate;
var
 Certificate:TX509Certificate;
begin
 {}
 Result:=nil;

 AcquireLock;

 {Get Root}
 Certificate:=Root;
 while Certificate <> nil do
  begin
   if Uppercase(Certificate.Subject.GetCN) = Uppercase(AName) then
    begin
     Result:=Certificate;
     Break;
    end;

   {Get Next}
   Certificate:=Next(Certificate);
  end;

 ReleaseLock;
end;

{==============================================================================}

function TX509CertificateChain.FindBySubjectDN(const AName:String):TX509Certificate;
var
 Certificate:TX509Certificate;
begin
 {}
 Result:=nil;

 AcquireLock;

 {Get Root}
 Certificate:=Root;
 while Certificate <> nil do
  begin
   if Uppercase(Certificate.Subject.GetDN) = Uppercase(AName) then
    begin
     Result:=Certificate;
     Break;
    end;

   {Get Next}
   Certificate:=Next(Certificate);
  end;

 ReleaseLock;
end;

{==============================================================================}

function TX509CertificateChain.ImportDER(ABuffer:Pointer;ASize:Integer;AParent:TX509Certificate):TX509Certificate;
var
 Certificate:TX509Certificate;
begin
 {}
 Result:=nil;

 if ABuffer = nil then Exit;

 Certificate:=TX509Certificate.Create(Self,AParent);
 if not Certificate.ImportDER(ABuffer,ASize) then
  begin
   Certificate.Free;
   Exit;
  end;

 Result:=Certificate;
end;

{==============================================================================}

function TX509CertificateChain.ImportPEM(ABuffer:Pointer;var ASize:Integer;AParent:TX509Certificate):TX509Certificate;
var
 Certificate:TX509Certificate;
begin
 {}
 Result:=nil;

 if ABuffer = nil then Exit;

 Certificate:=TX509Certificate.Create(Self,AParent);
 if not Certificate.ImportPEM(ABuffer,ASize) then
  begin
   Certificate.Free;
   Exit;
  end;

 Result:=Certificate;
end;

{==============================================================================}

function TX509CertificateChain.ExportDER(ABuffer:Pointer;var ASize:Integer;ACertificate:TX509Certificate):Boolean;
begin
 {}
 Result:=False;

 if ACertificate = nil then Exit;

 if ACertificate.FChain <> Self then Exit;

 {Export Certificate}
 Result:=ACertificate.ExportDER(ABuffer,ASize);
end;

{==============================================================================}

function TX509CertificateChain.ExportPEM(ABuffer:Pointer;var ASize:Integer;AStart:TX509Certificate):Boolean;
var
 Size:Integer;
 Remain:Integer;
 Offset:PtrUInt;
 Certificate:TX509Certificate;
begin
 {}
 Result:=False;

 if AStart = nil then Exit;

 if AStart.FChain <> Self then Exit;

 if ASize = 0 then Exit;

 AcquireLock;

 {Get Start}
 Remain:=ASize;
 Offset:=0;
 Certificate:=AStart;
 while Certificate <> nil do
  begin
   {Get Size}
   Size:=Remain;

   {Export Certificate}
   Result:=Certificate.ExportPEM(Pointer(PtrUInt(ABuffer) + Offset),Size);
   if not Result then Break;

   {Update Position}
   Dec(Remain,Size);
   Inc(Offset,Size);

   {Get Previous}
   Certificate:=Prev(Certificate);
  end;

 ReleaseLock;

 {Update Size}
 if Result then ASize:=Offset;
end;

{==============================================================================}

function TX509CertificateChain.GetPathLength(ACertificate:TX509Certificate):LongWord;
{Count the path length (number of certificates in the chain) from the last item up
 to and including the supplied certificate}
var
 Certificate:TX509Certificate;
begin
 {}
 Result:=0;

 if ACertificate = nil then Exit;

 {Get Last}
 Certificate:=Last;
 while Certificate <> nil do
  begin
   Inc(Result);

   if Certificate = ACertificate then Exit;

   {Get Previous}
   Certificate:=Prev(Certificate);
  end;
end;


{==============================================================================}

function TX509CertificateChain.ValidateChain(ATrust:TX509CertificateList):Integer;
{Validate all certificates in the chain for expiry, extensions, issuer and signature}
{Trust: List of trusted root certificates, if nil then validate all certificates up
        to the root certificate but do not validate issuer and signature for the root}
var
 Trusted:TX509Certificate;
 Certificate:TX509Certificate;
begin
 {}
 {Get Root}
 Certificate:=FRoot;
 if Certificate = nil then
  begin
   Result:=X509_VALIDATE_CERTIFICATE_UNKNOWN;
   Exit;
  end;

 {Get Trusted}
 Trusted:=nil;
 if ATrust <> nil then
  begin
   Trusted:=ATrust.FindBySubject(Certificate.Issuer);
  end;

 {Validate Root}
 Result:=Certificate.ValidateCertificate(Trusted);
 if Result <> X509_VALIDATE_OK then Exit;

 {Get Next}
 Certificate:=Next(Certificate);
 while Certificate <> nil do
  begin
   {Validate Certificate}
   Result:=Certificate.ValidateCertificate(Certificate.Parent);
   if Result <> X509_VALIDATE_OK then Exit;

   {Get Next}
   Certificate:=Next(Certificate);
  end;
end;

{==============================================================================}
{==============================================================================}
{TX509Certificate}
function TX509Certificate.ImportTime(ABuffer:PByte;ASize:Integer;ATag:LongWord;var ADateTime:TDateTime):Boolean;
var
 Next:PByte;
 Year:Integer;
 Month:Integer;
 Day:Integer;
 Hour:Integer;
 Minute:Integer;
 Second:Integer;
 Buffer:String;
 DateValue:TDateTime;
 TimeValue:TDateTime;
begin
 {}
 Result:=False;

 {Time ::= CHOICE (
     utcTime        UTCTime,
     generalTime    GeneralizedTime
  )

  UTCTime: YYMMDDHHMMSSZ
  GeneralizedTime: YYYYMMDDHHMMSSZ}

 {$IFDEF X509_DEBUG}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.ImportTime Size = ' + IntToStr(ASize));
 {$ENDIF}

 if ABuffer = nil then Exit;

 {Get Pointer}
 Next:=ABuffer;

 {Check Tag}
 case ATag of
  ASN1_TAG_UTCTIME:begin
    {UTCTime}
    {Certificate validity dates through the year 2049 MUST be encoded as UTCTime (RFC 3280)}
    if (ASize <> 13) or (Next[12] <> Ord('Z')) then Exit;

    {Get Year}
    SetLength(Buffer,2);
    System.Move(Next^,Buffer[1],2);
    Year:=StrToIntDef(Buffer,-1);
    if Year = -1 then Exit;

    {Check Year}
    if Year < 50 then
     begin
      Inc(Year,2000);
     end
    else
     begin
      Inc(Year,1900);
     end;

    Inc(Next,2);
   end;
  ASN1_TAG_GENERALIZEDTIME:begin
    {GeneralizedTime}
    {Certificate validity dates in 2050 or later MUST be encoded as GeneralizedTime (RFC 3280)}
    if (ASize <> 15) or (Next[14] <> Ord('Z')) then Exit;

    {Get Year}
    SetLength(Buffer,4);
    System.Move(Next^,Buffer[1],4);
    Year:=StrToIntDef(Buffer,-1);
    if Year = -1 then Exit;

    Inc(Next,4);
   end;
  else
   begin
    Exit;
   end;
 end;

 {Get Month}
 SetLength(Buffer,2);
 System.Move(Next^,Buffer[1],2);
 Month:=StrToIntDef(Buffer,-1);
 if Month = -1 then Exit;

 Inc(Next,2);

 {Get Day}
 System.Move(Next^,Buffer[1],2);
 Day:=StrToIntDef(Buffer,-1);
 if Day = -1 then Exit;

 Inc(Next,2);

 {Get Hour}
 System.Move(Next^,Buffer[1],2);
 Hour:=StrToIntDef(Buffer,-1);
 if Hour = -1 then Exit;

 Inc(Next,2);

 {Get Minute}
 System.Move(Next^,Buffer[1],2);
 Minute:=StrToIntDef(Buffer,-1);
 if Minute = -1 then Exit;

 Inc(Next,2);

 {Get Second}
 System.Move(Next^,Buffer[1],2);
 Second:=StrToIntDef(Buffer,-1);
 if Second = -1 then Exit;

 {Convert to Date Time}
 if TryEncodeDate(Year,Month,Day,DateValue) and TryEncodeTime(Hour,Minute,Second,0,TimeValue) then
  begin
   ADateTime:=ComposeDateTime(DateValue,TimeValue);
   
   Result:=True;  
  end;
end;

{==============================================================================}

function TX509Certificate.ImportName(ABuffer:PByte;ASize:Integer;AName:TX509Name;var ANext:PByte):Boolean;
var
 Next:PByte;
 Last:PByte;
 Tag:TASN1Tag;
 OID:TASN1OID;
 SetNext:PByte;
 SetLast:PByte;
 SequenceNext:PByte;
 SequenceLast:PByte;
begin
 {}
 Result:=False;

 {Name ::= CHOICE ( RDNSequence )
  RDNSequence ::= SEQUENCE OF RelativeDistinguishedName
  RelativeDistinguishedName ::= SET OF AttributeTypeAndValue
  AttributeTypeAndValue ::= SEQUENCE (
     type     AttributeType,
     value    AttributeValue
  )
  AttributeType ::= OBJECT IDENTIFIER
  AttributeValue ::= ANY DEFINED BY AttributeType}

 {$IFDEF X509_DEBUG}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.ImportName Size = ' + IntToStr(ASize));
 {$ENDIF}

 if ABuffer = nil then Exit;

 if AName = nil then Exit;

 {Get Name (Sequence)}
 if not ASN1GetTag(ABuffer,ASize,Tag) then Exit;

 {Check Tag}
 if (Tag.TagClass <> ASN1_CLASS_UNIVERSAL) or (Tag.TagNumber <> ASN1_TAG_SEQUENCE) then Exit;

 {Check Length}
 if Tag.Length > LongWord((ABuffer + ASize) - Tag.Contents) then Exit;

 {Get Pointers}
 Next:=Tag.Contents;
 Last:=Next + Tag.Length;
 ANext:=Last;

 while Next < Last do
  begin
   {Get RelativeDistinguishedName (Set)}
   if not ASN1GetTag(Next,Last - Next,Tag) then Exit;

   {Check Tag}
   if (Tag.TagClass <> ASN1_CLASS_UNIVERSAL) or (Tag.TagNumber <> ASN1_TAG_SET) then Exit;

   SetNext:=Tag.Contents;
   SetLast:=SetNext + Tag.Length;
   Next:=SetLast;

   {Get AttributeTypeAndValue (Sequence)}
   if not ASN1GetTag(SetNext,SetLast - SetNext,Tag) then Exit;

   {Check Tag}
   if (Tag.TagClass <> ASN1_CLASS_UNIVERSAL) or (Tag.TagNumber <> ASN1_TAG_SEQUENCE) then Exit;

   SequenceNext:=Tag.Contents;
   SequenceLast:=SequenceNext + Tag.Length;

   {Get OID}
   if not ASN1GetOID(SequenceNext,SequenceLast - SequenceNext,OID,SequenceNext) then Exit;

   {Get AttributeValue}
   if not ASN1GetTag(SequenceNext,SequenceLast - SequenceNext,Tag) then Exit;

   {Check Tag}
   if Tag.TagClass <> ASN1_CLASS_UNIVERSAL then Exit;

   {Check Count}
   if AName.NameAttributeCount = X509_MAX_NAME_ATTRIBUTES then Exit;

   AName.NameAttributes[AName.NameAttributeCount]._Type:=X509_NAME_ATTR_NONE;
   if ASN1OIDEqualPrefix(OID_ATTRIBUTE_TYPES,OID) then
    begin
     if ASN1OIDEqual(OID_ATTRIBUTE_TYPE_CN,OID) then
      begin
       {commonName}
       AName.NameAttributes[AName.NameAttributeCount]._Type:=X509_NAME_ATTR_CN;
      end
     else if ASN1OIDEqual(OID_ATTRIBUTE_TYPE_C,OID) then
      begin
       {countryName}
       AName.NameAttributes[AName.NameAttributeCount]._Type:=X509_NAME_ATTR_C;
      end
     else if ASN1OIDEqual(OID_ATTRIBUTE_TYPE_L,OID) then
      begin
       {localityName}
       AName.NameAttributes[AName.NameAttributeCount]._Type:=X509_NAME_ATTR_L;
      end
     else if ASN1OIDEqual(OID_ATTRIBUTE_TYPE_ST,OID) then
      begin
       {stateOrProvinceName}
       AName.NameAttributes[AName.NameAttributeCount]._Type:=X509_NAME_ATTR_ST;
      end
     else if ASN1OIDEqual(OID_ATTRIBUTE_TYPE_O,OID) then
      begin
       {organizationName}
       AName.NameAttributes[AName.NameAttributeCount]._Type:=X509_NAME_ATTR_O;
      end
     else if ASN1OIDEqual(OID_ATTRIBUTE_TYPE_OU,OID) then
      begin
       {organizationalUnitName}
       AName.NameAttributes[AName.NameAttributeCount]._Type:=X509_NAME_ATTR_OU;
      end;
    end
   else if ASN1OIDEqual(OID_ATTRIBUTE_TYPE_DC,OID) then
    begin
     {domainComponent}
     AName.NameAttributes[AName.NameAttributeCount]._Type:=X509_NAME_ATTR_DC;
    end
   else if ASN1OIDEqual(OID_EMAIL_ADDRESS,OID) then
    begin
     {e-mailAddress}
     SetLength(AName.Email,Tag.Length);
     System.Move(Tag.Contents^,AName.Email[1],Tag.Length);
    end;

   {Check Attribute Type}
   if AName.NameAttributes[AName.NameAttributeCount]._Type <> X509_NAME_ATTR_NONE then
    begin
     SetLength(AName.NameAttributes[AName.NameAttributeCount].Value,Tag.Length);
     System.Move(Tag.Contents^,AName.NameAttributes[AName.NameAttributeCount].Value[1],Tag.Length);

     Inc(AName.NameAttributeCount);
    end;
  end;

 Result:=True;
end;

{==============================================================================}

function TX509Certificate.ImportExtensionAltName(ABuffer:PByte;ASize:Integer;AName:TX509Name):Boolean;
var
 Next:PByte;
 Last:PByte;
 Tag:TASN1Tag;
begin
 {}
 Result:=False;

 {GeneralNames ::= SEQUENCE SIZE (1..MAX) OF GeneralName

  GeneralName ::= CHOICE (
     otherName                       [0]     OtherName,
     rfc822Name                      [1]     IA5String,
     dNSName                         [2]     IA5String,
     x400Address                     [3]     ORAddress,
     directoryName                   [4]     Name,
     ediPartyName                    [5]     EDIPartyName,
     uniformResourceIdentifier       [6]     IA5String,
     iPAddress                       [7]     OCTET STRING,
     registeredID                    [8]     OBJECT IDENTIFIER )

  OtherName ::= SEQUENCE (
     type-id    OBJECT IDENTIFIER,
     value      [0] EXPLICIT ANY DEFINED BY type-id )

  EDIPartyName ::= SEQUENCE (
     nameAssigner            [0]     DirectoryString OPTIONAL,
     partyName               [1]     DirectoryString )}

 {$IFDEF X509_DEBUG}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.ImportExtensionAltName Size = ' + IntToStr(ASize));
 {$ENDIF}

 if ABuffer = nil then Exit;

 if AName = nil then Exit;

 {Get Pointers}
 Next:=ABuffer;
 Last:=Next + ASize;

 while Next < Last do
  begin
   {Get Next}
   if not ASN1GetTag(Next,Last - Next,Tag) then Exit;

   {Check Tag}
   if Tag.TagClass = ASN1_CLASS_CONTEXT_SPECIFIC then
    begin
     {Check Tag}
     case Tag.TagNumber of
      0:begin
        {otherName}
        {Not yet implemented}
       end;
      1:begin
        {rfc822Name (IA5String)}
        SetLength(AName.AltEmail,Tag.Length);
        System.Move(Tag.Contents^,AName.AltEmail[1],Tag.Length);
        end;
      2:begin
        {dNSName (IA5String)}
        SetLength(AName.DNS,Tag.Length);
        System.Move(Tag.Contents^,AName.DNS[1],Tag.Length);
        //To Do //Allow multiple DNS names
       end;
      3:begin
        {x500Address}
        {Not yet implemented}
       end;
      4:begin
        {directoryName}
        {Not yet implemented}
       end;
      5:begin
        {ediPartyName}
        {Not yet implemented}
       end;
      6:begin
        {uniformResourceIdentifier (IA5String)}
        SetLength(AName.URI,Tag.Length);
        System.Move(Tag.Contents^,AName.URI[1],Tag.Length);
       end;
      7:begin
        {iPAddress (OctetString)}
        AName.IP:=GetMem(Tag.Length);
        System.Move(Tag.Contents^,AName.IP^,Tag.Length);
        AName.IPLen:=Tag.Length;
       end;
      8:begin
        {registeredID (ObjectIdentifier)}
        if not ASN1ParseOID(Tag.Contents,Tag.Length,AName.RegisteredID) then Exit;
       end;
     end;
    end;

   Next:=Tag.Contents + Tag.Length;
  end;

 Result:=True;
end;

{==============================================================================}

function TX509Certificate.ImportAlgorithmIdentifier(ABuffer:PByte;ASize:Integer;var AIdentifier:TX509AlgorithmIdentifier;var ANext:PByte):Boolean;
var
 Next:PByte;
 Last:PByte;
 Tag:TASN1Tag;
begin
 {}
 Result:=False;

 {AlgorithmIdentifier ::= SEQUENCE {
     algorithm            OBJECT IDENTIFIER,
     parameters           ANY DEFINED BY algorithm OPTIONAL}

 {$IFDEF X509_DEBUG}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.ImportAlgorithmIdentifier Size = ' + IntToStr(ASize));
 {$ENDIF}

 if ABuffer = nil then Exit;

 {Get AlgorithmIdentifier (Sequence)}
 if not ASN1GetTag(ABuffer,ASize,Tag) then Exit;

 {Check Tag}
 if (Tag.TagClass <> ASN1_CLASS_UNIVERSAL) or (Tag.TagNumber <> ASN1_TAG_SEQUENCE) then Exit;

 {Check Length}
 if Tag.Length > LongWord((ABuffer + ASize) - Tag.Contents) then Exit;

 {Get Pointers}
 Next:=Tag.Contents;
 Last:=Next + Tag.Length;
 ANext:=Last;

 {Get Algorithm (OID)}
 if not ASN1GetOID(Next,Last - Next,AIdentifier.OID,Next) then Exit;

 Result:=True;
end;

{==============================================================================}

function TX509Certificate.ImportValidity(ABuffer:PByte;ASize:Integer;var ANext:PByte):Boolean;
var
 Next:PByte;
 Len:Integer;
 Tag:TASN1Tag;
begin
 {}
 Result:=False;

 {$IFDEF X509_DEBUG}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.ImportValidity Size = ' + IntToStr(ASize));
 {$ENDIF}

 {Validity ::= SEQUENCE (
     notBefore      Time,
     notAfter       Time)}

 if ABuffer = nil then Exit;

 {Get Validity (Sequence)}
 if not ASN1GetTag(ABuffer,ASize,Tag) then Exit;

 {Check Tag}
 if (Tag.TagClass <> ASN1_CLASS_UNIVERSAL) or (Tag.TagNumber <> ASN1_TAG_SEQUENCE) then Exit;

 {Check Length}
 if Tag.Length > LongWord((ABuffer + ASize) - Tag.Contents) then Exit;

 {Get Pointers}
 Next:=Tag.Contents;
 Len:=Tag.Length;
 ANext:=Next + Len;

 {Get notBefore (Time)}
 if not ASN1GetTag(Next,Len,Tag) then Exit;

 {Check Tag}
 if Tag.TagClass <> ASN1_CLASS_UNIVERSAL then Exit;

 {Get Time}
 if not ImportTime(Tag.Contents,Tag.Length,Tag.TagNumber,NotBefore) then Exit;

 {Get Pointers}
 Next:=Tag.Contents + Tag.Length;
 Len:=ANext - Next;

 {Get notAfter (Time)}
 if not ASN1GetTag(Next,Len,Tag) then Exit;

 {Check Tag}
 if Tag.TagClass <> ASN1_CLASS_UNIVERSAL then Exit;

 {Get Time}
 if not ImportTime(Tag.Contents,Tag.Length,Tag.TagNumber,NotAfter) then Exit;

 Result:=True;
end;

{==============================================================================}

function TX509Certificate.ImportPublicKey(ABuffer:PByte;ASize:Integer;var ANext:PByte):Boolean;
var
 Next:PByte;
 Last:PByte;
 Tag:TASN1Tag;
begin
 {}
 Result:=False;

 {SubjectPublicKeyInfo ::= SEQUENCE (
    algorithm            AlgorithmIdentifier,
    subjectPublicKey     BIT STRING)}

 {$IFDEF X509_DEBUG}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.ImportPublicKey Size = ' + IntToStr(ASize));
 {$ENDIF}

 if ABuffer = nil then Exit;

 {Get Pointers}
 Next:=ABuffer;
 Last:=ABuffer + ASize;

 {Get SubjectPublicKeyInfo (Sequence)}
 if not ASN1GetTag(Next,Last - Next,Tag) then Exit;

 {Check Tag}
 if (Tag.TagClass <> ASN1_CLASS_UNIVERSAL) or (Tag.TagNumber <> ASN1_TAG_SEQUENCE) then Exit;

 {Get Pointers}
 Next:=Tag.Contents;
 if Tag.Length > LongWord(Last - Next) then Exit;
 Last:=Next + Tag.Length;
 ANext:=Last;

 {Get algorithm (AlgorithmIdentifier)}
 if not ImportAlgorithmIdentifier(Next,Last - Next,PublicKey.Algorithm,Next) then Exit;

 {Get subjectPublicKey (BitString)}
 if not ASN1GetTag(Next,Last - Next,Tag) then Exit;

 {Check Tag}
 if (Tag.TagClass <> ASN1_CLASS_UNIVERSAL) or (Tag.TagNumber <> ASN1_TAG_BITSTRING) then Exit;

 {Check Length}
 if Tag.Length < 1 then Exit;

 Next:=Tag.Contents;

 {Check that the BitString "unused bits" value (The first byte) is zero}
 if Next^ <> 0 then Exit;

 {Allocate Key}
 PublicKey.Key:=GetMem(Tag.Length - 1);

 {Import Key}
 System.Move(PByte(Next + 1)^,PublicKey.Key^,Tag.Length - 1);
 PublicKey.Length:=Tag.Length - 1;

 Result:=True;
end;

{==============================================================================}

function TX509Certificate.ImportExtension(ABuffer:PByte;ASize:Integer;var ANext:PByte):Boolean;
var
 Next:PByte;
 Last:PByte;
 Tag:TASN1Tag;
 OID:TASN1OID;
 Critical:Boolean;
begin
 {}
 Result:=False;

 {Extension  ::=  SEQUENCE  (
     extnID      OBJECT IDENTIFIER,
     critical    BOOLEAN DEFAULT FALSE,
     extnValue   OCTET STRING)}

 {$IFDEF X509_DEBUG}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.ImportExtension Size = ' + IntToStr(ASize));
 {$ENDIF}

 if ABuffer = nil then Exit;

 {Get Extension (Sequence)}
 if not ASN1GetTag(ABuffer,ASize,Tag) then Exit;

 {Check Tag}
 if (Tag.TagClass <> ASN1_CLASS_UNIVERSAL) or (Tag.TagNumber <> ASN1_TAG_SEQUENCE) then Exit;

 {Get Pointers}
 Next:=Tag.Contents;
 Last:=Next + Tag.Length;
 ANext:=Last;

 {Get OID}
 if not ASN1GetOID(Next,Last - Next,OID,Next) then Exit;

 {Get Next}
 if not ASN1GetTag(Next,Last - Next,Tag) then Exit;

 {Check Tag}
 if (Tag.TagClass <> ASN1_CLASS_UNIVERSAL) or ((Tag.TagNumber <> ASN1_TAG_BOOLEAN) and (Tag.TagNumber <> ASN1_TAG_OCTETSTRING)) then Exit;

 {Check Critical}
 Critical:=False;
 if Tag.TagNumber = ASN1_TAG_BOOLEAN then
  begin
   {Check Length}
   if Tag.Length <> 1 then Exit;

   Critical:=Tag.Contents[0] <> 0;

   Next:=Tag.Contents;

   {Get Next}
   if not ASN1GetTag(Next,Last - Next,Tag) then Exit;

   {Check Tag}
   if ((Tag.TagClass <> ASN1_CLASS_UNIVERSAL) and (Tag.TagClass <> ASN1_CLASS_PRIVATE)) or (Tag.TagNumber <> ASN1_TAG_OCTETSTRING) then Exit;
  end;

 {$IFDEF X509_DEBUG}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.ImportExtension ExtensionID = ' + ASN1OIDToString(OID) + ' Critical = ' + BoolToStr(Critical));
 {$ENDIF}

 {Import Extension Data}
 if not ImportExtensionData(Tag.Contents,Tag.Length,OID) and Critical then Exit;

 Result:=True;
end;

{==============================================================================}

function TX509Certificate.ImportExtensionData(ABuffer:PByte;ASize:Integer;const AOID:TASN1OID):Boolean;
begin
 {}
 Result:=False;

 {$IFDEF X509_DEBUG}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.ImportExtensionData Size = ' + IntToStr(ASize));
 {$ENDIF}

 if ABuffer = nil then Exit;

 if not ASN1OIDEqualPrefix(OID_ID_CE,AOID) then Exit;

 {Check keyUsage}
 if ASN1OIDEqual(OID_ID_CE_KEY_USAGE,AOID) then
  begin
   Result:=ImportExtensionKeyUsage(ABuffer,ASize);
  end
 {Check subjectAltName}
 else if ASN1OIDEqual(OID_ID_CE_SUBJECT_ALT_NAME,AOID) then
  begin
   Result:=ImportExtensionSubjectAltName(ABuffer,ASize);
  end
 {Check issuerAltName}
 else if ASN1OIDEqual(OID_ID_CE_ISSUER_ALT_NAME,AOID) then
  begin
   Result:=ImportExtensionIssuerAltName(ABuffer,ASize);
  end
 {Check basicConstraints}
 else if ASN1OIDEqual(OID_ID_CE_BASIC_CONTRAINTS,AOID) then
  begin
   Result:=ImportExtensionBasicContraints(ABuffer,ASize);
  end
 {Check cRLDistributionPoints}
 else if ASN1OIDEqual(OID_ID_CE_CRL_DISTRIBUTION_POINTS,AOID) then
  begin
   {Not yet implemented}

   Result:=True;
  end
 {Check certificatePolicies}
 else if ASN1OIDEqual(OID_ID_CE_CERTIFICATE_POLICIES,AOID) then
  begin
   {Not yet implemented}

   Result:=True;
  end
 {Check extKeyUsage}
 else if ASN1OIDEqual(OID_ID_CE_EXT_KEY_USAGE,AOID) then
  begin
   Result:=ImportExtensionExtKeyUsage(ABuffer,ASize);
  end;
end;

{==============================================================================}

function TX509Certificate.ImportExtensionKeyUsage(ABuffer:PByte;ASize:Integer):Boolean;
var
 Tag:TASN1Tag;
begin
 {}
 Result:=False;

 {KeyUsage ::= BIT STRING (
     digitalSignature        (0),
     nonRepudiation          (1),
     keyEncipherment         (2),
     dataEncipherment        (3),
     keyAgreement            (4),
     keyCertSign             (5),
     cRLSign                 (6),
     encipherOnly            (7),
     decipherOnly            (8) )}

 {$IFDEF X509_DEBUG}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.ImportExtensionKeyUsage Size = ' + IntToStr(ASize));
 {$ENDIF}

 if ABuffer = nil then Exit;

 {Get KeyUsage (BitString)}
 if not ASN1GetTag(ABuffer,ASize,Tag) then Exit;

 {Check Tag}
 if (Tag.TagClass <> ASN1_CLASS_UNIVERSAL) or (Tag.TagNumber <> ASN1_TAG_BITSTRING) then Exit;

 {Import KeyUsage}
 ExtensionsPresent:=ExtensionsPresent or X509_EXT_KEY_USAGE;
 KeyUsage:=ASN1BitStringToLongWord(Tag.Contents,Tag.Length);

 Result:=True;
end;

{==============================================================================}

function TX509Certificate.ImportExtensionSubjectAltName(ABuffer:PByte;ASize:Integer):Boolean;
var
 Tag:TASN1Tag;
begin
 {}
 Result:=False;

 {SubjectAltName ::= GeneralNames}

 {$IFDEF X509_DEBUG}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.ImportExtensionSubjectAltName Size = ' + IntToStr(ASize));
 {$ENDIF}

 if ABuffer = nil then Exit;

 {Get SubjectAltName (GeneralNames)(Sequence)}
 if not ASN1GetTag(ABuffer,ASize,Tag) then Exit;

 {Check Tag}
 if (Tag.TagClass <> ASN1_CLASS_UNIVERSAL) or (Tag.TagNumber <> ASN1_TAG_SEQUENCE) then Exit;

 {Import SubjectAltName}
 ExtensionsPresent:=ExtensionsPresent or X509_EXT_SUBJECT_ALT_NAME;
 Result:=ImportExtensionAltName(Tag.Contents,Tag.Length,Subject);
end;

{==============================================================================}

function TX509Certificate.ImportExtensionIssuerAltName(ABuffer:PByte;ASize:Integer):Boolean;
var
 Tag:TASN1Tag;
begin
 {}
 Result:=False;

 {IssuerAltName ::= GeneralNames}

 {$IFDEF X509_DEBUG}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.ImportExtensionIssuerAltName Size = ' + IntToStr(ASize));
 {$ENDIF}

 if ABuffer = nil then Exit;

 {Get IssuerAltName (GeneralNames)(Sequence)}
 if not ASN1GetTag(ABuffer,ASize,Tag) then Exit;

 {Check Tag}
 if (Tag.TagClass <> ASN1_CLASS_UNIVERSAL) or (Tag.TagNumber <> ASN1_TAG_SEQUENCE) then Exit;

 {Import IssuerAltName}
 ExtensionsPresent:=ExtensionsPresent or X509_EXT_ISSUER_ALT_NAME;
 Result:=ImportExtensionAltName(Tag.Contents,Tag.Length,Subject);
end;

{==============================================================================}

function TX509Certificate.ImportExtensionBasicContraints(ABuffer:PByte;ASize:Integer):Boolean;
var
 Tag:TASN1Tag;
begin
 {}
 Result:=False;

 {BasicConstraints ::= SEQUENCE (
  cA                      BOOLEAN DEFAULT FALSE,
  pathLenConstraint       INTEGER (0..MAX) OPTIONAL )}

 {$IFDEF X509_DEBUG}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.ImportExtensionBasicContraints Size = ' + IntToStr(ASize));
 {$ENDIF}

 if ABuffer = nil then Exit;

 {Get BasicConstraints (Sequence)}
 if not ASN1GetTag(ABuffer,ASize,Tag) then Exit;

 {Check Tag}
 if (Tag.TagClass <> ASN1_CLASS_UNIVERSAL) or (Tag.TagNumber <> ASN1_TAG_SEQUENCE) then Exit;

 {Import BasicConstraints}
 ExtensionsPresent:=ExtensionsPresent or X509_EXT_BASIC_CONSTRAINTS;
 if Tag.Length > 0 then
  begin
   {Get Next}
   if not ASN1GetTag(Tag.Contents,Tag.Length,Tag) then Exit;

   {Check Tag}
   if Tag.TagClass <> ASN1_CLASS_UNIVERSAL then Exit;

   {Check cA (Boolean)}
   if Tag.TagNumber = ASN1_TAG_BOOLEAN then
    begin
     {Import cA}
     if not ASN1ParseBoolean(Tag.Contents,Tag.Length,CA) then Exit;

     {Check End}
     if Tag.Length = LongWord((ABuffer + ASize) - Tag.Contents) then
      begin
       Result:=True;
       Exit;
      end;
     
     {Get Next}
     if not ASN1GetTag(Tag.Contents + Tag.Length,LongWord(ASize) - Tag.Length,Tag) then Exit;

     {Check Tag}
     if Tag.TagClass <> ASN1_CLASS_UNIVERSAL then Exit;
    end;

   {Check pathLenConstraint (Integer)}
   if Tag.TagNumber <> ASN1_TAG_INTEGER then Exit;

   {Import pathLenConstraint}
   ExtensionsPresent:=ExtensionsPresent or X509_EXT_PATH_LEN_CONSTRAINT;
   if not ASN1ParseInt(Tag.Contents,Tag.Length,Integer(PathLenConstraint)) then Exit;
  end;

 Result:=True;
end;

{==============================================================================}

function TX509Certificate.ImportExtensionExtKeyUsage(ABuffer:PByte;ASize:Integer):Boolean;
var
 Next:PByte;
 Last:PByte;
 Tag:TASN1Tag;
 OID:TASN1OID;
begin
 {}
 Result:=False;

 {ExtKeyUsageSyntax ::= SEQUENCE SIZE (1..MAX) OF KeyPurposeId

  KeyPurposeId ::= OBJECT IDENTIFIER}

 {$IFDEF X509_DEBUG}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.ImportExtensionExtKeyUsage Size = ' + IntToStr(ASize));
 {$ENDIF}

 if ABuffer = nil then Exit;

 {Get ExtKeyUsageSyntax (Sequence)}
 if not ASN1GetTag(ABuffer,ASize,Tag) then Exit;

 {Check Tag}
 if (Tag.TagClass <> ASN1_CLASS_UNIVERSAL) or (Tag.TagNumber <> ASN1_TAG_SEQUENCE) then Exit;

 {Check Length}
 if Tag.Length > LongWord((ABuffer + ASize) - Tag.Contents) then Exit;

 {Get Pointers}
 Next:=Tag.Contents;
 Last:=Next + Tag.Length;

 {Import ExtKeyUsageSyntax}
 while Next < Last do
  begin
   {Get OID}
   if not ASN1GetOID(Next,Last - Next,OID,Next) then Exit;

   {Check anyExtendedKeyUsage}
   if ASN1OIDEqual(OID_ID_CE_EXT_KEY_USAGE_ANY,OID) then
    begin
     ExtendedKeyUsage:=ExtendedKeyUsage or X509_EXT_KEY_USAGE_ANY;
    end
   {Check serverAuth}
   else if ASN1OIDEqual(OID_ID_KP_SERVER_AUTH,OID) then
    begin
     ExtendedKeyUsage:=ExtendedKeyUsage or X509_EXT_KEY_USAGE_SERVER_AUTH;
    end
   {Check clientAuth}
   else if ASN1OIDEqual(OID_ID_KP_CLIENT_AUTH,OID) then
    begin
     ExtendedKeyUsage:=ExtendedKeyUsage or X509_EXT_KEY_USAGE_CLIENT_AUTH;
    end
   {Check OCSPSigning}
   else if ASN1OIDEqual(OID_ID_KP_OCSP_SIGNING,OID) then
    begin
     ExtendedKeyUsage:=ExtendedKeyUsage or X509_EXT_KEY_USAGE_OCSP;
    end;
  end;

 ExtensionsPresent:=ExtensionsPresent or X509_EXT_EXT_KEY_USAGE;

 Result:=True;
end;

{==============================================================================}

function TX509Certificate.ImportExtensions(ABuffer:PByte;ASize:Integer):Boolean;
var
 Next:PByte;
 Last:PByte;
 Tag:TASN1Tag;
begin
 {}
 Result:=False;

 {Extensions  ::=  SEQUENCE SIZE (1..MAX) OF Extension}

 {$IFDEF X509_DEBUG}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.ImportExtensions Size = ' + IntToStr(ASize));
 {$ENDIF}

 if ABuffer = nil then Exit;

 {Get Extensions (Sequence)}
 if not ASN1GetTag(ABuffer,ASize,Tag) then Exit;

 {Check Tag}
 if (Tag.TagClass <> ASN1_CLASS_UNIVERSAL) or (Tag.TagNumber <> ASN1_TAG_SEQUENCE) then Exit;

 {Get Pointers}
 Next:=Tag.Contents;
 Last:=Next + Tag.Length;

 {Import Extensions}
 while Next < Last do
  begin
   if not ImportExtension(Next,Last - Next,Next) then Exit;
  end;

 Result:=True;
end;

{==============================================================================}

function TX509Certificate.ImportTBSCertificate(ABuffer:PByte;ASize:Integer;var ANext:PByte):Boolean;
var
 Next:PByte;
 Last:PByte;
 Tag:TASN1Tag;
begin
 {}
 Result:=False;

 {tbsCertificate TBSCertificate ::= SEQUENCE}

 {$IFDEF X509_DEBUG}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.ImportTBSCertificate Size = ' + IntToStr(ASize));
 {$ENDIF}

 if ABuffer = nil then Exit;

 {Get tbsCertificate (Sequence)}
 if not ASN1GetTag(ABuffer,ASize,Tag) then Exit;

 {Check Tag}
 if (Tag.TagClass <> ASN1_CLASS_UNIVERSAL) or (Tag.TagNumber <> ASN1_TAG_SEQUENCE) then Exit;

 {Get Pointers}
 Next:=Tag.Contents;
 Last:=Next + Tag.Length;
 ANext:=Last;

 {version [0]  EXPLICIT Version DEFAULT v1
  Version  ::=  INTEGER  (  v1(0), v2(1), v3(2)  )}

 {Get version (Integer)}
 if not ASN1GetTag(Next,Last - Next,Tag) then Exit;

 Next:=Tag.Contents;

 {Check Tag}
 if Tag.TagClass = ASN1_CLASS_CONTEXT_SPECIFIC then
  begin
   if not ASN1GetInt(Next,Last - Next,Integer(Version),Next) then Exit;

   if not ASN1GetTag(Next,Last - Next,Tag) then Exit;
  end
 else
  begin
   Version:=X509_CERT_V1;
  end;
 {$IFDEF X509_DEBUG}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.ImportTBSCertificate Version = ' + IntToStr(Version));
 {$ENDIF}

 {serialNumber CertificateSerialNumber ::= INTEGER}

 {Get serialNumber (Integer)}
 if (Tag.TagClass <> ASN1_CLASS_UNIVERSAL) or (Tag.TagNumber <> ASN1_TAG_INTEGER) then Exit;

 {Check Length}
 if (Tag.Length < 1) or (Tag.Length > X509_MAX_SERIAL_NUM_LEN) then Exit;

 Next:=Tag.Contents + Tag.Length;
 {Remove leading zeros}
 while (Tag.Length > 1) and (Tag.Contents[0] = 0) do
  begin
   Inc(Tag.Contents);
   Dec(Tag.Length);
  end;
 System.Move(Tag.Contents^,SerialNumber.Value,Tag.Length);
 SerialNumber.Length:=Tag.Length;
 {$IFDEF X509_DEBUG}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.ImportTBSCertificate SerialNumber = ' + SerialNumber.ToString);
 {$ENDIF}

 {Get signature (AlgorithmIdentifier)}
 if not ImportAlgorithmIdentifier(Next,Last - Next,SignatureAlgorithm,Next) then Exit;
 {$IFDEF X509_DEBUG}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.ImportTBSCertificate SignatureAlgorithm = ' + SignatureAlgorithm.ToString);
 {$ENDIF}

 {Get issuer (Name)}
 if not ImportName(Next,Last - Next,Issuer,Next) then Exit;
 {$IFDEF X509_DEBUG}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.ImportTBSCertificate Issuer = ' + Issuer.GetDN);
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.ImportTBSCertificate Issuer Email = ' + Issuer.Email);
 {$ENDIF}

 {Get validity (Validity)}
 if not ImportValidity(Next,Last - Next,Next) then Exit;
 {$IFDEF X509_DEBUG}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.ImportTBSCertificate NotBefore = ' + DateTimeToStr(NotBefore));
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.ImportTBSCertificate NotAfter = ' + DateTimeToStr(NotAfter));
 {$ENDIF}

 {Get subject (Name)}
 if not ImportName(Next,Last - Next,Subject,Next) then Exit;
 SubjectDN:=Subject.GetDN;
 {$IFDEF X509_DEBUG}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.ImportTBSCertificate Subject = ' + Subject.GetDN);
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.ImportTBSCertificate Subject Email = ' + Subject.Email);
 {$ENDIF}

 {Get subjectPublicKeyInfo (SubjectPublicKeyInfo)}
 if not ImportPublicKey(Next,Last - Next,Next) then Exit;
 {$IFDEF X509_DEBUG}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.ImportTBSCertificate PublicKey = ' + PublicKey.ToString);
 {$ENDIF}

 {Check End}
 if (Next = Last) or (Version = X509_CERT_V1) then
  begin
   Result:=True;
   Exit;
  end;

 {Get Optional Fields}
 if not ASN1GetTag(Next,Last - Next,Tag) then Exit;

 if Tag.TagClass <> ASN1_CLASS_CONTEXT_SPECIFIC then Exit;

 {Get issuerUniqueID (UniqueIdentifier) (Optional)}
 if Tag.TagNumber = 1 then
  begin
   {Parse UniqueIdentifier (BitString)}
   {Not Implemented}

   Next:=Tag.Contents + Tag.Length;

   {Check End}
   if Next = Last then
    begin
     Result:=True;
     Exit;
    end;

   {Get Next}
   if not ASN1GetTag(Next,Last - Next,Tag) then Exit;
  end;

 {Get subjectUniqueID (UniqueIdentifier) (Optional)}
 if Tag.TagNumber = 2 then
  begin
   {Parse UniqueIdentifier (BitString)}
   {Not Implemented}

   {Check End}
   Next:=Tag.Contents + Tag.Length;
   if Next = Last then
    begin
     Result:=True;
     Exit;
    end;

   {Get Next}
   if not ASN1GetTag(Next,Last - Next,Tag) then Exit;
  end;

 if Tag.TagNumber <> 3 then
  begin
   Result:=True;
   Exit;
  end;

 {Check Version}
 if Version <> X509_CERT_V3 then Exit;

 {Get extensions (Extensions)}
 if not ImportExtensions(Tag.Contents,Tag.Length) then Exit;

 {$IFDEF X509_DEBUG}
 if (ExtensionsPresent and X509_EXT_BASIC_CONSTRAINTS) <> 0 then
  begin
   if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.ImportTBSCertificate CA = ' + BoolToStr(CA));
  end;
 if (ExtensionsPresent and X509_EXT_PATH_LEN_CONSTRAINT) <> 0 then
  begin
   if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.ImportTBSCertificate PathLenConstraint = ' + IntToStr(PathLenConstraint));
  end;
 if (ExtensionsPresent and X509_EXT_KEY_USAGE) <> 0 then
  begin
   if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.ImportTBSCertificate KeyUsage = ' + IntToHex(KeyUsage,8));
  end;
 if (ExtensionsPresent and X509_EXT_SUBJECT_ALT_NAME) <> 0 then
  begin
   if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.ImportTBSCertificate Subject AltEmail = ' + Subject.AltEmail + ' / DNS = ' + Subject.DNS + ' / URI = ' + Subject.URI + ' / RegisteredID = ' + ASN1OIDToString(Subject.RegisteredID));
  end;
 if (ExtensionsPresent and X509_EXT_ISSUER_ALT_NAME) <> 0 then
  begin
   if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.ImportTBSCertificate Issuer AltEmail = ' + Issuer.AltEmail + ' / DNS = ' + Issuer.DNS + ' / URI = ' + Issuer.URI + ' / RegisteredID = ' + ASN1OIDToString(Issuer.RegisteredID));
  end;
 if (ExtensionsPresent and X509_EXT_EXT_KEY_USAGE) <> 0 then
  begin
   if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.ImportTBSCertificate ExtendedKeyUsage = ' + IntToHex(ExtendedKeyUsage,8));
  end;
 {$ENDIF} 

 Result:=True;
end;

{==============================================================================}

function TX509Certificate.VerifyRSASignature(AIssuer:TX509Certificate):Boolean;
var
 Next:PByte;
 Last:PByte;
 Data:PByte;
 Size:Integer;
 Tag:TASN1Tag;
 OID:TASN1OID;
 RSAContext:PRSAContext;
 RSAPublicKey:TX509RSAPublicKey;
begin
 {}
 Result:=False;

 {$IFDEF X509_DEBUG}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.VerifyRSASignature');
 {$ENDIF}

 if AIssuer = nil then Exit;

 {$IFDEF X509_DEBUG}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.VerifyRSASignature - Issuer Public Key Algorithm: ' + ASN1OIDToString(AIssuer.PublicKey.Algorithm.OID));
 {$ENDIF}

 {Check the Issuer Public Key Algorithm}
 if not ASN1OIDEqual(OID_PKCS1_RSA,AIssuer.PublicKey.Algorithm.OID) then Exit;

 {Import the Issuer Public Key}
 if not RSAPublicKey.ImportDER(AIssuer.PublicKey.Key,AIssuer.PublicKey.Length) then Exit;
 try
  {$IFDEF X509_DEBUG}
  if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.VerifyRSASignature - Modulus Length: ' + IntToStr(RSAPublicKey.ModulusLen));
  {$ENDIF}

  {Check the Size of the Issuer Public Key Modulus}
  if RSAPublicKey.ModulusLen <> Signature.Length then Exit;

  {Initialize the Public Key}
  RSAContext:=RSAInitPublicKey(RSAPublicKey.Modulus,RSAPublicKey.PublicExponent,RSAPublicKey.ModulusLen,RSAPublicKey.PublicExponentLen);
  if RSAContext = nil then Exit;
  try
   {$IFDEF X509_DEBUG}
   if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.VerifyRSASignature - Signature Length: ' + IntToStr(Signature.Length));
   {$ENDIF}

   {Allocate Digest}
   Data:=GetMem(Signature.Length);
   if Data = nil then Exit;
   try
    {Decrypt the Signature}
    Size:=RSADecryptVerify(RSAContext,Signature.Value,Data,Signature.Length,True);
    if Size = -1 then Exit;

    {DigestInfo ::= SEQUENCE (
         digestAlgorithm DigestAlgorithmIdentifier,
         digest Digest
     )

     DigestAlgorithmIdentifier ::= AlgorithmIdentifier

     Digest ::= OCTET STRING}

    {Get DigestInfo (Sequence)}
    if not ASN1GetTag(Data,Size,Tag) then Exit;

    {Check Tag}
    if (Tag.TagClass <> ASN1_CLASS_UNIVERSAL) or (Tag.TagNumber <> ASN1_TAG_SEQUENCE) then Exit;

    Next:=Tag.Contents;
    Last:=Next + Tag.Length;

    {AlgorithmIdentifier ::= SEQUENCE (
        algorithm            OBJECT IDENTIFIER,
        parameters           ANY DEFINED BY algorithm OPTIONAL)}

    {Get AlgorithmIdentifier (Sequence)}
    if not ASN1GetTag(Next,Last - Next,Tag) then Exit;

    {Check Tag}
    if (Tag.TagClass <> ASN1_CLASS_UNIVERSAL) or (Tag.TagNumber <> ASN1_TAG_SEQUENCE) then Exit;

    {Get Digest Algorithm OID}
    if not ASN1GetOID(Tag.Contents,Tag.Length,OID,Next) then Exit;

    {$IFDEF X509_DEBUG}
    if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.VerifyRSASignature - Digest Algorithm: ' + ASN1OIDToString(OID));
    {$ENDIF}

    Next:=Tag.Contents + Tag.Length;
    Last:=Data + Size;

    {Digest ::= OCTET STRING}

    {Get Digest (OctetString)}
    if not ASN1GetTag(Next,Last - Next,Tag) then Exit;

    {Check Tag}
    if (Tag.TagClass <> ASN1_CLASS_UNIVERSAL) or (Tag.TagNumber <> ASN1_TAG_OCTETSTRING) then Exit;

    {Compare Digest Algorithm to Signature Algoritm}
    if ASN1OIDEqual(OID_HASH_SHA1,OID) then
     begin
      {SHA1}
      if not ASN1OIDEqual(OID_PKCS1_SHA1_RSA,Signature.Algorithm.OID) then Exit;

      {Check Length}
      if Tag.Length <> SizeOf(TSHA1Digest) then Exit;
      if (Tag.Contents + Tag.Length) < (Data + Size) then Exit;

      {Check Hash}
      Result:=VerifySHA1Digest(Tag.Contents,Tag.Length);
     end
    else if ASN1OIDEqual(OID_HASH_SHA256,OID) then
     begin
      {SHA256}
      if not ASN1OIDEqual(OID_PKCS1_SHA256_RSA,Signature.Algorithm.OID) then Exit;

      {Check Length}
      if Tag.Length <> SizeOf(TSHA256Digest) then Exit;

      {Check Hash}
      Result:=VerifySHA256Digest(Tag.Contents,Tag.Length);
     end
    else if ASN1OIDEqual(OID_HASH_SHA384,OID) then
     begin
      {SHA384}
      if not ASN1OIDEqual(OID_PKCS1_SHA384_RSA,Signature.Algorithm.OID) then Exit;

      {Check Length}
      if Tag.Length <> SizeOf(TSHA384Digest) then Exit;

      {Check Hash}
      Result:=VerifySHA384Digest(Tag.Contents,Tag.Length);
     end
    else if ASN1OIDEqual(OID_HASH_SHA512,OID) then
     begin
      {SHA512}
      if not ASN1OIDEqual(OID_PKCS1_SHA512_RSA,Signature.Algorithm.OID) then Exit;

      {Check Length}
      if Tag.Length <> SizeOf(TSHA512Digest) then Exit;

      {Check Hash}
      Result:=VerifySHA512Digest(Tag.Contents,Tag.Length);
     end
    else if ASN1OIDEqual(OID_PKCS2_MD5,OID) then
     begin
      {MD5}
      if not ASN1OIDEqual(OID_PKCS1_MD5_RSA,Signature.Algorithm.OID) then Exit;

      {Check Length}
      if Tag.Length <> SizeOf(TMD5Digest) then Exit;

      {Check Hash}
      Result:=VerifyMD5Digest(Tag.Contents,Tag.Length);
     end
    else
     begin
      Exit;
     end;

    {$IFDEF X509_DEBUG}
    if Result and PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.VerifyRSASignature - Verify Success');
    {$ENDIF}
   finally
    FreeMem(Data);
   end;
  finally
   {Free the RSA Key}
   RSAFreeKey(RSAContext);
  end;
 finally
  {Release the Public Key}
  RSAPublicKey.Release;
 end;
end;

{==============================================================================}

function TX509Certificate.VerifyMD5Digest(ABuffer:PByte;ASize:Integer):Boolean;
var
 Block:TMD5Block;
 Digest:TMD5Digest;
begin
 {}
 Result:=False;

 {$IFDEF X509_DEBUG}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.VerifyMD5Digest');
 {$ENDIF}

 {Check Buffer}
 if ABuffer = nil then Exit;

 {Check Length}
 if ASize <> SizeOf(TMD5Digest) then Exit;

 {Create Block}
 Block.Data:=FTBSData;
 Block.Size:=FTBSSize;
 Block.Next:=nil;

 {Create Digest}
 if not MD5DigestData(@Block,@Digest) then Exit;

 {Compare Digest}
 Result:=CompareMem(ABuffer,@Digest,ASize);
end;

{==============================================================================}

function TX509Certificate.VerifySHA1Digest(ABuffer:PByte;ASize:Integer):Boolean;
var
 Block:TSHA1Block;
 Digest:TSHA1Digest;
begin
 {}
 Result:=False;

 {$IFDEF X509_DEBUG}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.VerifySHA1Digest');
 {$ENDIF}

 {Check Buffer}
 if ABuffer = nil then Exit;

 {Check Length}
 if ASize <> SizeOf(TSHA1Digest) then Exit;

 {Create Block}
 Block.Data:=FTBSData;
 Block.Size:=FTBSSize;
 Block.Next:=nil;

 {Create Digest}
 if not SHA1DigestData(@Block,@Digest) then Exit;

 {Compare Digest}
 Result:=CompareMem(ABuffer,@Digest,ASize);
end;

{==============================================================================}

function TX509Certificate.VerifySHA256Digest(ABuffer:PByte;ASize:Integer):Boolean;
var
 Block:TSHA256Block;
 Digest:TSHA256Digest;
begin
 {}
 Result:=False;

 {$IFDEF X509_DEBUG}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.VerifySHA256Digest');
 {$ENDIF}

 {Check Buffer}
 if ABuffer = nil then Exit;

 {Check Length}
 if ASize <> SizeOf(TSHA256Digest) then Exit;

 {Create Block}
 Block.Data:=FTBSData;
 Block.Size:=FTBSSize;
 Block.Next:=nil;

 {Create Digest}
 if not SHA256DigestData(@Block,@Digest) then Exit;

 {Compare Digest}
 Result:=CompareMem(ABuffer,@Digest,ASize);
end;

{==============================================================================}

function TX509Certificate.VerifySHA384Digest(ABuffer:PByte;ASize:Integer):Boolean;
var
 Block:TSHA384Block;
 Digest:TSHA384Digest;
begin
 {}
 Result:=False;

 {$IFDEF X509_DEBUG}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.VerifySHA384Digest');
 {$ENDIF}

 {Check Buffer}
 if ABuffer = nil then Exit;

 {Check Length}
 if ASize <> SizeOf(TSHA384Digest) then Exit;

 {Create Block}
 Block.Data:=FTBSData;
 Block.Size:=FTBSSize;
 Block.Next:=nil;

 {Create Digest}
 if not SHA384DigestData(@Block,@Digest) then Exit;

 {Compare Digest}
 Result:=CompareMem(ABuffer,@Digest,ASize);
end;

{==============================================================================}

function TX509Certificate.VerifySHA512Digest(ABuffer:PByte;ASize:Integer):Boolean;
var
 Block:TSHA512Block;
 Digest:TSHA512Digest;
begin
 {}
 Result:=False;

 {$IFDEF X509_DEBUG}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.VerifySHA512Digest');
 {$ENDIF}

 {Check Buffer}
 if ABuffer = nil then Exit;

 {Check Length}
 if ASize <> SizeOf(TSHA512Digest) then Exit;

 {Create Block}
 Block.Data:=FTBSData;
 Block.Size:=FTBSSize;
 Block.Next:=nil;

 {Create Digest}
 if not SHA512DigestData(@Block,@Digest) then Exit;

 {Compare Digest}
 Result:=CompareMem(ABuffer,@Digest,ASize);
end;

{==============================================================================}

constructor TX509Certificate.Create(AChain:TX509CertificateChain;AParent:TX509Certificate);
begin
 {}
 inherited Create;

 if AChain <> nil then AChain.InsertAfter(AParent,Self);

 Issuer:=TX509Name.Create;
 Subject:=TX509Name.Create;
end;

{==============================================================================}

destructor TX509Certificate.Destroy;
begin
 {}
 if FList <> nil then FList.Remove(Self);

 if FChain <> nil then FChain.Remove(Self);

 if FData <> nil then FreeMem(FData);

 Issuer.Free;
 Subject.Free;

 PublicKey.Release;
 Signature.Release;

 inherited Destroy;
end;

{==============================================================================}

function TX509Certificate.ImportDER(ABuffer:Pointer;ASize:Integer):Boolean;
var
 Next:PByte;
 Last:PByte;
 Tag:TASN1Tag;
 HashStart:PByte;
begin
 {}
 Result:=False;

 {Certificate ::= SEQUENCE}

 {$IFDEF X509_DEBUG}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.ImportDER Size = ' + IntToStr(ASize));
 {$ENDIF}

 if ABuffer = nil then Exit;

 if ASize = 0 then Exit;

 {Copy Buffer}
 FData:=GetMem(ASize);
 if FData = nil then Exit;
 FSize:=ASize;
 System.Move(ABuffer^,FData^,FSize);

 {Get Pointers}
 Next:=ABuffer;
 Last:=PByte(ABuffer) + ASize;

 {Get Certificate (Sequence)}
 if not ASN1GetTag(Next,ASize,Tag) then Exit;

 {Check Tag}
 if (Tag.TagClass <> ASN1_CLASS_UNIVERSAL) or (Tag.TagNumber <> ASN1_TAG_SEQUENCE) then  Exit;

 Next:=Tag.Contents;

 {Check Length}
 if Tag.Length > LongWord(Last - Next) then Exit;

 {Update Length}
 if Tag.Length < LongWord(Last - Next) then
  begin
   Last:=Next + Tag.Length;
  end;

 {Import TBS (To Be Signed) Certificate}
 HashStart:=Next;
 FTBSData:=FData + (HashStart - PByte(ABuffer));
 if not ImportTBSCertificate(Next,Last - Next,Next) then Exit;
 FTBSSize:=Next - HashStart;

 {Get signatureAlgorithm (AlgorithmIdentifier)}
 if not ImportAlgorithmIdentifier(Next,Last - Next,Signature.Algorithm,Next) then Exit;

 {Get signatureValue (BitString)}
 if not ASN1GetTag(Next,Last - Next,Tag) then Exit;

 {Check Tag}
 if (Tag.TagClass <> ASN1_CLASS_UNIVERSAL) or (Tag.TagNumber <> ASN1_TAG_BITSTRING) then  Exit;

 {Check Length}
 if Tag.Length < 1 then Exit;

 Next:=Tag.Contents;

 {Check that the BitString "unused bits" value (The first byte) is zero}
 if Next^ <> 0 then Exit;

 {Allocate Signature}
 Signature.Value:=GetMem(Tag.Length - 1);

 {Import Signature}
 System.Move(PByte(Next + 1)^,Signature.Value^,Tag.Length - 1);
 Signature.Length:=Tag.Length - 1;

 Result:=True;
end;

{==============================================================================}

function TX509Certificate.ImportPEM(ABuffer:Pointer;var ASize:Integer):Boolean;

 function FindTag(const Tag:String;Buffer:PByte;Size:Integer):PByte;
 var
  Len:Integer;
  Count:Integer;
 begin
   {}
   Result:=nil;

   {Get Tag Length}
   Len:=Length(Tag);
   if Size < Len then Exit;

   {Search the Buffer}
   for Count:=0 to Size - Len do
    begin
     {Check for Tag}
     if CompareMem(Buffer + Count,PChar(Tag),Len) then
      begin
       Result:=Buffer + Count;
       Exit;
      end;
    end;
 end;

var
 Next:PByte;
 Remain:Integer;
 BeginTag:PByte;
 EndTag:PByte;
 DecodedSize:Integer;
 DecodedData:Pointer;
begin
 {}
 Result:=False;

 if ABuffer = nil then Exit;

 if ASize = 0 then Exit;

 {Get Start}
 Next:=ABuffer;
 Remain:=ASize;

 {Find Begin Tag}
 BeginTag:=FindTag(X509_PEM_CERTIFICATE_BEGIN,Next,Remain);
 if BeginTag = nil then Exit;

 {Update Position}
 Inc(BeginTag,Length(X509_PEM_CERTIFICATE_BEGIN));
 Dec(Remain,Length(X509_PEM_CERTIFICATE_BEGIN));
 Next:=BeginTag;

 {Find End Tag}
 EndTag:=FindTag(X509_PEM_CERTIFICATE_END,Next,Remain);
 if EndTag = nil then Exit;

 {Update Position}
 Dec(Remain,Length(X509_PEM_CERTIFICATE_END));

 {Size Certificate}
 if not Base64DecodeBuffer(PChar(BeginTag),EndTag - BeginTag,nil,DecodedSize) then Exit;

 {Allocate Data}
 DecodedData:=GetMem(DecodedSize);
 if DecodedData = nil then Exit;
 try
  {Decode Certificate}
  if not Base64DecodeBuffer(PChar(BeginTag),EndTag - BeginTag,DecodedData,DecodedSize) then Exit;

  {Import Certificate}
  if not ImportDER(DecodedData,DecodedSize) then Exit;

  {Update Size}
  Next:=ABuffer;
  ASize:=(EndTag + Length(X509_PEM_CERTIFICATE_END)) - Next;

  Result:=True;
 finally
  FreeMem(DecodedData);
 end;
end;

{==============================================================================}

function TX509Certificate.ExportDER(ABuffer:Pointer;var ASize:Integer):Boolean;
begin
 {}
 Result:=False;

 if ABuffer = nil then Exit;

 if ASize = 0 then Exit;

 if LongWord(ASize) < FSize then Exit;

 {Export Data}
 System.Move(FData^,ABuffer^,FSize);

 {Update Size}
 ASize:=FSize;

 Result:=True;
end;

{==============================================================================}

function TX509Certificate.ExportPEM(ABuffer:Pointer;var ASize:Integer):Boolean;
var
 Size:Integer;
 Remain:Integer;
 Offset:PtrUInt;
begin
 {}
 Result:=False;

 if ABuffer = nil then Exit;

 if ASize = 0 then Exit;

 {Get Start}
 Offset:=0;
 Remain:=ASize;

 {Check Begin Certificate}
 Size:=Length(X509_PEM_CERTIFICATE_BEGIN + X509_PEM_LINE_END);
 if Remain < Size then Exit;

 {Add Begin Certificate}
 System.Move(PChar(X509_PEM_CERTIFICATE_BEGIN + X509_PEM_LINE_END)^,Pointer(PtrUInt(ABuffer) + Offset)^,Size);

 {Update Position}
 Inc(Offset,Size);
 Dec(Remain,Size);

 {Check Encoded Certificate}
 if not Base64EncodeBuffer(PChar(FData),FSize,nil,Size) then Exit;
 if Remain < Size then Exit;

 {Add Encoded Certificate}
 if not Base64EncodeBuffer(PChar(FData),FSize,PChar(PtrUInt(ABuffer) + Offset),Size) then Exit;

 {Update Position (Remove Terminator)}
 Inc(Offset,Size - 1);
 Dec(Remain,Size - 1);

 {Check End Certificate}
 Size:=Length(X509_PEM_CERTIFICATE_END + X509_PEM_LINE_END);
 if Remain < Size then Exit;

 {Add End Certificate}
 System.Move(PChar(X509_PEM_CERTIFICATE_END + X509_PEM_LINE_END)^,Pointer(PtrUInt(ABuffer) + Offset)^,Size);

 {Update Position}
 Inc(Offset,Size);

 {Update Size}
 ASize:=Offset;

 Result:=True;
end;

{==============================================================================}

function TX509Certificate.IsValidIssuer:Boolean;
begin
 {}
 Result:=False;

 {Check BasicContraints and CA}
 if ((ExtensionsPresent and X509_EXT_BASIC_CONSTRAINTS) <> 0) and not(CA) then Exit;

 {Check Version and BasicContraints}
 if (Version = X509_CERT_V3) and ((ExtensionsPresent and X509_EXT_BASIC_CONSTRAINTS) = 0) then Exit;

 {Check KeyUsage and keyCertSign}
 if ((ExtensionsPresent and X509_EXT_KEY_USAGE) <> 0) and ((KeyUsage and X509_KEY_USAGE_KEY_CERT_SIGN) = 0) then Exit;

 Result:=True;
end;

{==============================================================================}

function TX509Certificate.IsSelfSigned:Boolean;
begin
 {}
 Result:=Issuer.Compare(Subject) = 0;
end;

{==============================================================================}

function TX509Certificate.VerifySignature(AIssuer:TX509Certificate):Boolean;
{Validate the certificate signature}
{Issuer: The issuing certificate (will be same as parent except for the root)}
begin
 {}
 Result:=False;

 {Check Signature Algorithm}
 if ASN1OIDEqualPrefix(OID_PKCS1_ALGORITHMS,Signature.Algorithm.OID) then
  begin
   {$IFDEF X509_DEBUG}
   if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Certificate.VerifySignature - Certificate ' + Subject.GetCN + ' - Signature Algorithm: ' + ASN1OIDToString(Signature.Algorithm.OID));
   {$ENDIF}

   Result:=VerifyRSASignature(AIssuer);
  end
 else
  begin
   if PLATFORM_LOG_ENABLED then PlatformLogError('TX509Certificate.VerifySignature - Certificate ' + Subject.GetCN + ' - Unsupported Signature Algorithm: ' + ASN1OIDToString(Signature.Algorithm.OID));
  end;
end;

{==============================================================================}

function TX509Certificate.ValidateCertificate(AIssuer:TX509Certificate):Integer;
{Validate a certificate for expiry, extensions, issuer and signature}
{Issuer: The issuing certificate (will be same as parent except for the root)}
begin
 {}
 Result:=X509_VALIDATE_OK;

 {Check Issuer and Parent}
 if (AIssuer = nil) and (Parent <> nil) then
  begin
   Result:=X509_VALIDATE_CERTIFICATE_UNKNOWN;
   Exit;
  end;

 {Check Validity}
 if (Now < NotBefore) or (Now > NotAfter) then
  begin
   if PLATFORM_LOG_ENABLED then PlatformLogError('TX509Certificate.ValidateCertificate - Certificate ' + Subject.GetCN + ' - Expired');
   
   Result:=X509_VALIDATE_CERTIFICATE_EXPIRED;
   Exit;
  end;

 {Check Issuer}
 if AIssuer <> nil then
  begin
   {Check Subject Match}
   if Issuer.Compare(AIssuer.Subject) <> 0 then
    begin
     if PLATFORM_LOG_ENABLED then PlatformLogError('TX509Certificate.ValidateCertificate - Certificate ' + Subject.GetCN + ' -  Issuer subject mismatch');
     
     Result:=X509_VALIDATE_CERTIFICATE_UNKNOWN;
     Exit;
    end;

   {Check Valid Issuer}
   if not AIssuer.IsValidIssuer then
    begin
     if PLATFORM_LOG_ENABLED then PlatformLogError('TX509Certificate.ValidateCertificate - Certificate ' + Subject.GetCN + ' -  Issuer not valid');
     
     Result:=X509_VALIDATE_BAD_CERTIFICATE;
     Exit;
    end;

   {Check Extensions}
   if ((AIssuer.ExtensionsPresent and X509_EXT_PATH_LEN_CONSTRAINT) <> 0) and (AIssuer.PathLenConstraint > 0) then
    begin
     if (Chain <> nil) and (Chain.GetPathLength(Self) > AIssuer.PathLenConstraint) then
      begin
       if PLATFORM_LOG_ENABLED then PlatformLogError('TX509Certificate.ValidateCertificate - Certificate ' + Subject.GetCN + ' -  pathLenConstraint exceeded');
       
       Result:=X509_VALIDATE_BAD_CERTIFICATE;
       Exit;
      end;
    end;

   {Check Signature}
   if not VerifySignature(AIssuer) then
    begin
     if PLATFORM_LOG_ENABLED then PlatformLogError('TX509Certificate.ValidateCertificate - Certificate ' + Subject.GetCN + ' -  Invalid signature');
     
     Result:=X509_VALIDATE_BAD_CERTIFICATE;
     Exit;
    end;
  end;
end;

{==============================================================================}
{==============================================================================}
{TX509Name}
function TX509Name.StringCompare(const AValue1,AValue2:String):Integer;

 function StripWhitespace(const AValue:String):String;
 var
  Count:Integer;
 begin
  {}
  Result:='';

  for Count := 1 to Length(AValue) do
   begin
    {Check for Space or Tab}
    if (AValue[Count] <> #32) and (AValue[Count] <> #9) then
     begin
      Result:=Result + AValue[Count];
     end
   end;
 end;

var
 Value1:String;
 Value2:String;
begin
 {}
 if (Length(AValue1) = 0) and (Length(AValue2) > 0) then
  begin
   Result:=-1;
   Exit;
  end;

 if (Length(AValue1) > 0) and (Length(AValue2) = 0) then
  begin
   Result:=1;
   Exit;
  end;

 if (Length(AValue1) = 0) and (Length(AValue2) = 0) then
  begin
   Result:=0;
   Exit;
  end;

 {Strip Whitespace}
 Value1:=StripWhitespace(AValue1);
 Value2:=StripWhitespace(AValue2);

 {Compare Text (Case Insensitive)}
 Result:=CompareText(Value1,Value2);
end;

{==============================================================================}

destructor TX509Name.Destroy;
begin
 {}
 if IP <> nil then FreeMem(IP);
 IP:=nil;

 inherited Destroy;
end;

{==============================================================================}

function TX509Name.GetCN:String;
var
 Count:Integer;
begin
 {}
 Result:='';

 {$IFDEF X509_DEBUG}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Name.GetCN');
 {$ENDIF}

 for Count:=NameAttributeCount - 1 downto 0 do
  begin
   if NameAttributes[Count]._Type = X509_NAME_ATTR_CN then
    begin
     Result:=NameAttributes[Count].Value;
     Exit;
    end;
  end;
end;

{==============================================================================}

function TX509Name.GetDN:String;
var
 Count:Integer;
begin
 {}
 Result:='';

 {$IFDEF X509_DEBUG}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('TX509Name.GetDN');
 {$ENDIF}

 for Count:=NameAttributeCount - 1 downto 0 do
  begin
   if NameAttributes[Count]._Type <> X509_NAME_ATTR_NONE then
    begin
     if LongWord(Count) = (NameAttributeCount - 1) then
      begin
       Result:=NameAttributes[Count].ToString;
      end
     else
      begin
       Result:=Result + ',' + NameAttributes[Count].ToString;
      end;
    end;
  end;
end;

{==============================================================================}

function TX509Name.Compare(AName:TX509Name):Integer;
var
 Count:Integer;
begin
 {}
 if AName = nil then
  begin
   Result:=1;
   Exit;
  end;

 {Check Attribute Count}
 if NameAttributeCount < AName.NameAttributeCount then
  begin
   Result:=-1;
   Exit;
  end;

 if NameAttributeCount > AName.NameAttributeCount then
  begin
   Result:=1;
   Exit;
  end;

 {Check Attributes}
 for Count:=0 to NameAttributeCount - 1 do
  begin
   {Check Type}
   if NameAttributes[Count]._Type < AName.NameAttributes[Count]._Type then
    begin
     Result:=-1;
     Exit;
    end;

   if NameAttributes[Count]._Type > AName.NameAttributes[Count]._Type then
    begin
     Result:=1;
     Exit;
    end;

   {Check Value}
   Result:=StringCompare(NameAttributes[Count].Value,AName.NameAttributes[Count].Value);
   if Result <> 0 then Exit;
  end;

 {Check Email}
 Result:=StringCompare(Email,AName.Email);
 if Result <> 0 then Exit;

 Result:=0;
end;

{==============================================================================}
{==============================================================================}
{X509 Functions}

{==============================================================================}
{==============================================================================}
{X509 Helper Functions}
function X509NameAttributeTypeToString(AType:LongWord):String;
begin
 {}
 Result:='Unknown';

 case AType of
  X509_NAME_ATTR_DC:Result:='DC';
  X509_NAME_ATTR_CN:Result:='CN';
  X509_NAME_ATTR_C:Result:='C';
  X509_NAME_ATTR_L:Result:='L';
  X509_NAME_ATTR_ST:Result:='ST';
  X509_NAME_ATTR_O:Result:='O';
  X509_NAME_ATTR_OU:Result:='OU';
 end;
end;

{==============================================================================}
{==============================================================================}

initialization
 {Nothing}

{==============================================================================}

finalization
 {Nothing}

end.
