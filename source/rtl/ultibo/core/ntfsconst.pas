{
Ultibo NTFS constants unit.

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

 <See NTFS.pas>

References
==========

 <See NTFS.pas>

NT Filesystem
=============

 <See NTFS.pas>

 Notes: NTFS uses 64 bit cluster values in all cases

        All structures in NTFS are 8 byte aligned

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

{$IFNDEF FPC_DOTTEDUNITS}
unit NTFSConst;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Core.GlobalConfig,
  Core.GlobalConst,
  Core.GlobalTypes,
  Core.FileSystem,
  System.SysUtils,
  System.Classes,
  Core.Unicode,
  Core.Security,
  Core.Ultibo,
  Core.UltiboUtils,
  Core.UltiboClasses;
{$ELSE FPC_DOTTEDUNITS}
uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  FileSystem,
  SysUtils,
  Classes,
  Unicode,
  Security,
  Ultibo,
  UltiboUtils,
  UltiboClasses;
{$ENDIF FPC_DOTTEDUNITS}

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
type
 {NTFS specific types}
 TNTFSAttrDefine = record {Not Packed}     {Attribute Definition defaults}
  AttributeType:LongWord;
  AttributeName:String;
  AttrDefFlags:LongWord;
  MinimumSize:Int64;
  MaximumSize:Int64;
 end;

 TNTFSUpcaseConvert = record {Not Packed}  {Uppercase Conversion defaults}
  Count:Word;
  Value:Word;
 end;

 TNTFSSectorCount = record {Not Packed}    {Sectors per Cluster for various disk sizes (Fixed Disk only)}
  SectorCount:LongWord;                    {Based on 512 bytes per sector only}
  SectorsPerCluster:LongWord;
 end;

 TNTFSLogSize = record {Not Packed}        {Log File Sizes for various disk sizes}
  DiskSize:Int64;
  LogSize:Int64;
 end;

 PNTFSDefaultSid = ^TNTFSDefaultSid;
 TNTFSDefaultSid = record {Not Packed}        {SID defaults}
  Sid:TWellKnownSidType;
 end;

 TNTFSDefaultAce = record {Not Packed}        {Descriptor Ace defaults}
  AceType:Byte;
  AceFlags:Byte;
  AceSize:Word;
  Mask:LongWord;
  Sid:TWellKnownSidType;
 end;

 TNTFSDefaultAcl = record {Not Packed}        {Descriptor Acl defaults}
  AclRevision:Byte;
  AclSize:Word;
  AceCount:Word;
  Aces:array[0..7] of TNTFSDefaultAce;
 end;

 PNTFSDefaultDescriptor = ^TNTFSDefaultDescriptor;
 TNTFSDefaultDescriptor = record {Not Packed} {Descriptor defaults}
  Size:LongWord;
  Revision:Byte;             {Revision}
  Control:Word;              {Control Flags}
  OwnerOffset:LongWord;      {Offset to Owner SID}
  GroupOffset:LongWord;      {Offset to Group SID}
  SaclOffset:LongWord;       {Offset to SACL}
  DaclOffset:LongWord;       {Offset to DACL}
  Owner:TWellKnownSidType;
  Group:TWellKnownSidType;
  Sacl:TNTFSDefaultAcl;
  Dacl:TNTFSDefaultAcl;
 end;

{==============================================================================}
const
 {NTFS specific constants}
 ntfsNTFS12 = 0;  {4.0}             {WinNT}
 ntfsNTFS30 = 1;  {5.0}             {Win2000}
 ntfsNTFS31 = 2;  {5.1/5.2/6.0/6.1} {WinXP/Win2003/WinVista/Win2008/Win7}

 ntfsNames:array[0..2] of String = (
  'NTFS',
  'NTFS',
  'NTFS');

 ntfsAnyName     = '*';
 ntfsDotName     = '.';
 ntfsDotDotName  = '..';
 ntfsBlankName   = '';

 ntfsAnyNameLength     = 1;
 ntfsDotNameLength     = 1;
 ntfsDotDotNameLength  = 2;
 ntfsBlankNameLength   = 0;

 ntfsOemName     = 'NTFS'; {Padded with spaces to 8 bytes}

 ntfsEntryPadding = $20;   {Padded with spaces (32)}

 ntfsSecurityHashPadding = $00490049; {Unicode string II}
 ntfsSecurityHashPaddingSize = 4;     {SizeOf(LongWord)}

 ntfsSecurityMirrorOffset  = $00040000; {256K Offset between primary and mirror entries}
 ntfsSecuritySectionOffset = $00040000; {256K Offset between sections (End of one section to Start of next}
 ntfsSecuritySectionShift  = 18;        {Not Used}
 ntfsSecurityOffsetMask    = $0003FFFF; {Used to determine the offset within a section from the offset}
 ntfsSecuritySectionMask   = $FFFC0000; {Used to determine the start of a section from the offset}
 ntfsSecurityMirrorTest    = $00040000; {Mirror blocks will always be 40000 or C0000}

 ntfsMaxPath = 260;
 ntfsMaxFile = 255;

 ntfsMaxAlias = 999999;   {Max number of lfn aliases (ie ~999999)}
 ntfsMaxNameAlias = 4;
 ntfsMaxHashAlias = 999;
 ntfsMaxHashShift = 16;

 ntfsMaxVolumeName = 255;
 ntfsMaxAttributeName = 255;

 ntfsMinClusterSize   = 512;
 ntfs12MaxClusterSize = 4096;
 ntfs30MaxClusterSize = 65536;

 ntfsBootSector    = 0;
 ntfsStartSector   = 0;  {Int64}
 ntfsUnknownSector = -1; {Int64}

 ntfsStartCluster   = 0;  {Int64}
 ntfsUnevenCluster  = 1;  {Int64}
 ntfsUnknownCluster = -1; {Int64}

 ntfsStatusNone      = $00000000;
 ntfsStatusAdded     = $00000001;  {Used by Node}
 ntfsStatusDeleted   = $00000002;  {Used by Node}
 ntfsStatusChanged   = $00000004;  {Used by Key, Node, Index}
 ntfsStatusLoaded    = $00000008;  {Used by Index, List}
 ntfsStatusInvalid   = $00000010;  {Used by Key, Item}
 ntfsStatusResizing  = $00000020;  {Used by Record}
 ntfsStatusRemoving  = $00000040;  {Used by Record}
 ntfsStatusUpdating  = $00000080;  {Used by Attribute}

 ntfsStatusMirrored  = $00000100;  {Metafile Records (0 to 3)}
 ntfsStatusMetafile  = $00000200;  {Metafile Records (0 to 11)}
 ntfsStatusReserved  = $00000400;  {Reserved MFT Records (12 to 15)(0C to 0F)}
 ntfsStatusExpansion = $00000800;  {Expansion MFT Records (16 to 23)(10 to 17)}

 ntfsStatusOverflow  = $00001000;  {Record has an attribute list attribute}
 ntfsStatusExtension = $00002000;  {Record is an extension of the base record}

 ntfsStatusFixed     = $00004000;  {Attribute is a fixed size (can write single item)} {Not used due to Fixups}
 ntfsStatusSingle    = $00008000;  {Attribute only allows a single instance (Standard Information, Attribute List, End etc)}
 ntfsStatusUnlisted  = $00010000;  {Attribute is not listed in an Attribute List (Attribute List, End)}
 ntfsStatusUnmovable = $00020000;  {Attribute cannot be moved from the Base Record (Standard Information, Attribute List)}
 ntfsStatusManaged   = $00040000;  {Attribute is handled as a record or list, data member is not valid (Standard Information, Attribute List, FileName etc)}

 ntfsStatusAny       = $FFFFFFFF;

 ntfsCompareLess    = -1;
 ntfsCompareEqual   = 0;
 ntfsCompareGreater = 1;

 ntfsInstanceFirst = 0;
 ntfsInstanceLast  = -1;

 ntfsAttrDefNoMinimum = 0;  {Int64}
 ntfsAttrDefNoMaximum = -1; {Int64}

 ntfsFileReferenceNone   = 0;  {Int64}
 ntfsRecordNumberMask    = $0000FFFFFFFFFFFF; {Get a record number from a file reference}
 ntfsUnknownRecordNumber = -1; {Int64}

 ntfsBitmapUnknown    = LongWord(-1);

 ntfsBlockCountMask8  = $FFFFFFFFFFFFFFF8; {0000000000000007}
 ntfsBlockCountMask64 = $FFFFFFFFFFFFFFC0; {000000000000003F}

 {NTFS MFT Zone} {HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\FileSystem - NtfsMftZoneReservation - DWORD}
 ntfsMftZone50percent = 500;  {Registry value 4}
 ntfsMftZone37percent = 375;  {Registry value 3}
 ntfsMftZone25percent = 250;  {Registry value 2}
 ntfsMftZone12percent = 125;  {Registry value 1} {Default}
 ntfsMftZone0percent  = 0;    {Registry value 0} {Not allowed}

 {NTFS MFT Fixed Zone (Vista/2008)}
 ntfsMftFixedZone1000MB = 1000;{Registry value 0} {Not allowed}
 ntfsMftFixedZone800MB  = 800; {Registry value 4}
 ntfsMftFixedZone600MB  = 600; {Registry value 3}
 ntfsMftFixedZone400MB  = 400; {Registry value 2}
 ntfsMftFixedZone200MB  = 200; {Registry value 1} {Default}
 ntfsMftFixedZone0MB    = 0;   {Registry value 0} {Not allowed}

 ntfsMftFixedZone1000MBSize = 1048576000;
 ntfsMftFixedZone800MBSize  = 838860800;
 ntfsMftFixedZone600MBSize  = 629145600;
 ntfsMftFixedZone400MBSize  = 419430400;
 ntfsMftFixedZone200MBSize  = 209715200;
 ntfsMftFixedZone0MBSize    = 0;

 {NTFS Run Masks}
 ntfsRunOffsetMask  = $F0;  {shr 4}
 ntfsRunLengthMask  = $0F;

 {NTFS Run Encode Masks (and)}
 ntfsRunEncodeMasks:array[0..8] of Int64 = (
  $0000000000000000,
  $00000000000000FF,
  $000000000000FFFF,
  $0000000000FFFFFF,
  $00000000FFFFFFFF,
  $000000FFFFFFFFFF,
  $0000FFFFFFFFFFFF,
  $00FFFFFFFFFFFFFF,
  $FFFFFFFFFFFFFFFF); {Not Used}

 {NTFS Run Encode Tests (and)}
 (*ntfsRunEncodeTests:array[0..8] of Int64 = (
  $FFFFFFFFFFFFFFFF,  {Not Used}
  $FFFFFFFFFFFFFF00,
  $FFFFFFFFFFFF0000,
  $FFFFFFFFFF000000,
  $FFFFFFFF00000000,
  $FFFFFF0000000000,
  $FFFF000000000000,
  $FF00000000000000,
  $0000000000000000);*)

 ntfsRunEncodeTests:array[0..8] of Int64 = (
  $0000000000000000,
  $000000000000007F,
  $0000000000007FFF,
  $00000000007FFFFF,
  $000000007FFFFFFF,
  $0000007FFFFFFFFF,
  $00007FFFFFFFFFFF,
  $007FFFFFFFFFFFFF,
  $7FFFFFFFFFFFFFFF); {Not Used}

 ntfsRunNegativeTests:array[0..8] of Int64 = (
  $FFFFFFFFFFFFFFFF,  {Not Used}
  $FFFFFFFFFFFFFF80,
  $FFFFFFFFFFFF8000,
  $FFFFFFFFFF800000,
  $FFFFFFFF80000000,
  $FFFFFF8000000000,
  $FFFF800000000000,
  $FF80000000000000,
  $8000000000000000);

 {NTFS Run Decode Masks (and) (or not)}
 ntfsRunDecode1Masks:array[1..1] of Byte = (
  $FF);

 ntfsRunDecode2Masks:array[2..2] of Word = (
  $FFFF);

 ntfsRunDecode4Masks:array[3..4] of LongWord = (
  $00FFFFFF,
  $FFFFFFFF);

 ntfsRunDecode8Masks:array[5..8] of Int64 = (
  $000000FFFFFFFFFF,
  $0000FFFFFFFFFFFF,
  $00FFFFFFFFFFFFFF,
  $FFFFFFFFFFFFFFFF);

 {NTFS Run Decode Tests (>=)}
 ntfsRunDecode1Tests:array[1..1] of Byte = (
  $80);

 ntfsRunDecode2Tests:array[2..2] of Word = (
  $8000);

 ntfsRunDecode4Tests:array[3..4] of LongWord = (
  $00800000,
  $80000000);

 ntfsRunDecode8Tests:array[5..8] of Int64 = (
  $0000008000000000,
  $0000800000000000,
  $0080000000000000,
  $8000000000000000);

 {NTFS Compression Tags}
 ntfsCompressionTags:array[1..8] of Byte = (
  $01,
  $02,
  $04,
  $08,
  $10,
  $20,
  $40,
  $80);

 ntfsCompressionTagMask = $0FFF;         {The default mask for encoding and decoding the offset and length of a tag}
 ntfsCompressionTagShift = 12;           {The default shift for encoding and decoding the offset and length of a tag}
                                         {Tag is encoded as Offset:Length by varying the mask and shift based on the position}

 ntfsCompressionUnitSize  = 4;           {The default shift count for compression unit to cluster conversion}
 ntfsCompressionBlockSize = 4096;        {The fixed size of a compression block inside the variable compression unit}

 ntfsCompressionLengthMask = $0FFF;      {The mask for the maximum length of a compressed block}
 ntfsCompressionMarkerMask = $B000;      {The mask for the marker of a compressed block}
 ntfsCompressionMarkerTest = $8000;      {The test for the marker of a compressed block}

 ntfsCompressionTableInit   = $FF;       {The initialization value for the compression table}
 ntfsCompressionTableUnused = $FFFF;     {The unused or unseen value for the compression table}

 {NTFS Bitmap Masks}
 ntfsBitmapMaskBits = 64;                {Int64 Bitmap Masks}
 ntfsBitmapMaskNone = $0000000000000000; {Int64} {Used for fast counting of free blocks}
 ntfsBitmapMaskAll  = $FFFFFFFFFFFFFFFF; {Int64} {Used for fast counting of used blocks}

 ntfsBitmapMasks:array[0..63] of Int64 = (
  $0000000000000001,$0000000000000002,$0000000000000004,$0000000000000008,$0000000000000010,$0000000000000020,$0000000000000040,$0000000000000080,
  $0000000000000100,$0000000000000200,$0000000000000400,$0000000000000800,$0000000000001000,$0000000000002000,$0000000000004000,$0000000000008000,
  $0000000000010000,$0000000000020000,$0000000000040000,$0000000000080000,$0000000000100000,$0000000000200000,$0000000000400000,$0000000000800000,
  $0000000001000000,$0000000002000000,$0000000004000000,$0000000008000000,$0000000010000000,$0000000020000000,$0000000040000000,$0000000080000000,
  $0000000100000000,$0000000200000000,$0000000400000000,$0000000800000000,$0000001000000000,$0000002000000000,$0000004000000000,$0000008000000000,
  $0000010000000000,$0000020000000000,$0000040000000000,$0000080000000000,$0000100000000000,$0000200000000000,$0000400000000000,$0000800000000000,
  $0001000000000000,$0002000000000000,$0004000000000000,$0008000000000000,$0010000000000000,$0020000000000000,$0040000000000000,$0080000000000000,
  $0100000000000000,$0200000000000000,$0400000000000000,$0800000000000000,$1000000000000000,$2000000000000000,$4000000000000000,$8000000000000000);

 {ntfsBitmapOverlays:array[0..63] of Int64 = (
  $8000000000000000,$C000000000000000,$E000000000000000,$F000000000000000,$F800000000000000,$FC00000000000000,$FE00000000000000,$FF00000000000000,
  $FF80000000000000,$FFC0000000000000,$FFE0000000000000,$FFF0000000000000,$FFF8000000000000,$FFFC000000000000,$FFFE000000000000,$FFFF000000000000,
  $FFFF800000000000,$FFFFC00000000000,$FFFFE00000000000,$FFFFF00000000000,$FFFFF80000000000,$FFFFFC0000000000,$FFFFFE0000000000,$FFFFFF0000000000,
  $FFFFFF8000000000,$FFFFFFC000000000,$FFFFFFE000000000,$FFFFFFF000000000,$FFFFFFF800000000,$FFFFFFFC00000000,$FFFFFFFE00000000,$FFFFFFFF00000000,
  $FFFFFFFF80000000,$FFFFFFFFC0000000,$FFFFFFFFE0000000,$FFFFFFFFF0000000,$FFFFFFFFF8000000,$FFFFFFFFFC000000,$FFFFFFFFFE000000,$FFFFFFFFFF000000,
  $FFFFFFFFFF800000,$FFFFFFFFFFC00000,$FFFFFFFFFFE00000,$FFFFFFFFFFF00000,$FFFFFFFFFFF80000,$FFFFFFFFFFFC0000,$FFFFFFFFFFFE0000,$FFFFFFFFFFFF0000,
  $FFFFFFFFFFFF8000,$FFFFFFFFFFFFC000,$FFFFFFFFFFFFE000,$FFFFFFFFFFFFF000,$FFFFFFFFFFFFF800,$FFFFFFFFFFFFFC00,$FFFFFFFFFFFFFE00,$FFFFFFFFFFFFFF00,
  $FFFFFFFFFFFFFF80,$FFFFFFFFFFFFFFC0,$FFFFFFFFFFFFFFE0,$FFFFFFFFFFFFFFF0,$FFFFFFFFFFFFFFF8,$FFFFFFFFFFFFFFFC,$FFFFFFFFFFFFFFFE,$FFFFFFFFFFFFFFFF);}

 ntfsBitmapOverlays:array[0..63] of Int64 = (
  $0000000000000001,$0000000000000003,$0000000000000007,$000000000000000F,$000000000000001F,$000000000000003F,$000000000000007F,$00000000000000FF,
  $00000000000001FF,$00000000000003FF,$00000000000007FF,$0000000000000FFF,$0000000000001FFF,$0000000000003FFF,$0000000000007FFF,$000000000000FFFF,
  $000000000001FFFF,$000000000003FFFF,$000000000007FFFF,$00000000000FFFFF,$00000000001FFFFF,$00000000003FFFFF,$00000000007FFFFF,$0000000000FFFFFF,
  $0000000001FFFFFF,$0000000003FFFFFF,$0000000007FFFFFF,$000000000FFFFFFF,$000000001FFFFFFF,$000000003FFFFFFF,$000000007FFFFFFF,$00000000FFFFFFFF,
  $00000001FFFFFFFF,$00000003FFFFFFFF,$00000007FFFFFFFF,$0000000FFFFFFFFF,$0000001FFFFFFFFF,$0000003FFFFFFFFF,$0000007FFFFFFFFF,$000000FFFFFFFFFF,
  $000001FFFFFFFFFF,$000003FFFFFFFFFF,$000007FFFFFFFFFF,$00000FFFFFFFFFFF,$00001FFFFFFFFFFF,$00003FFFFFFFFFFF,$00007FFFFFFFFFFF,$0000FFFFFFFFFFFF,
  $0001FFFFFFFFFFFF,$0003FFFFFFFFFFFF,$0007FFFFFFFFFFFF,$000FFFFFFFFFFFFF,$001FFFFFFFFFFFFF,$003FFFFFFFFFFFFF,$007FFFFFFFFFFFFF,$00FFFFFFFFFFFFFF,
  $01FFFFFFFFFFFFFF,$03FFFFFFFFFFFFFF,$07FFFFFFFFFFFFFF,$0FFFFFFFFFFFFFFF,$1FFFFFFFFFFFFFFF,$3FFFFFFFFFFFFFFF,$7FFFFFFFFFFFFFFF,$FFFFFFFFFFFFFFFF);

 {NTFS Signatures}
 ntfsFileSignature    = $454C4946; {FILE}
 ntfsIndexSignature   = $58444E49; {INDX}
 ntfsRecordSignature  = $44524352; {RCRD}
 ntfsRestartSignature = $52545352; {RSTR}
 ntfsCheckedSignature = $00000000; {CHKD}  //To Do //To tell Windows not to look at the log file - see notes
                                                         //See http://www.gelato.unsw.edu.au/lxr/source/fs/ntfs/logfile.c

 ntfsNullGUID:TGUID = (D1:$00000000;D2:$0000;D3:$0000;D4:($00,$00,$00,$00,$00,$00,$00,$00));

 ntfsNullFileTime:TFileTime = (dwLowDateTime:$00000000;dwHighDateTime:$00000000);

 {NTFS Table Types}
 ntfsTableTypeMft       = $00000000;
 ntfsTableTypeMftMirr   = $00000001;

 {NTFS File Types}
 ntfsFileTypeMft        = $00000000;
 ntfsFileTypeMftMirr    = $00000001;
 ntfsFileTypeLogFile    = $00000002;
 ntfsFileTypeVolume     = $00000003;
 ntfsFileTypeAttrDef    = $00000004;
 ntfsFileTypeRoot       = $00000005;
 ntfsFileTypeBitmap     = $00000006;
 ntfsFileTypeBoot       = $00000007;
 ntfsFileTypeBadClus    = $00000008;
 ntfsFileTypeSecure     = $00000009;
 ntfs12FileTypeQuota    = $00000009;   {Version 1.2 only}
 ntfsFileTypeUpCase     = $0000000A;
 ntfsFileTypeExtend     = $0000000B;

 ntfsFileTypeObjId      = $10000000;   {Any record number}
 ntfsFileTypeQuota      = $20000000;   {Any record number}
 ntfsFileTypeReparse    = $30000000;   {Any record number}
 ntfsFileTypeUsnJrnl    = $40000000;   {Any record number}

 ntfsFileTypeFile       = $50000000;   {Any record number} {Not Used}
 ntfsFileTypeDirectory  = $60000000;   {Any record number} {Not Used}

 ntfsFileTypeReserved1  = $0000000C;   {Reserved Records}
 ntfsFileTypeReserved2  = $0000000D;
 ntfsFileTypeReserved3  = $0000000E;
 ntfsFileTypeReserved4  = $0000000F;

 ntfsFileTypeExpansion1 = $00000010;   {Expansion Records}
 ntfsFileTypeExpansion2 = $00000011;
 ntfsFileTypeExpansion3 = $00000012;
 ntfsFileTypeExpansion4 = $00000013;
 ntfsFileTypeExpansion5 = $00000014;
 ntfsFileTypeExpansion6 = $00000015;
 ntfsFileTypeExpansion7 = $00000016;
 ntfsFileTypeExpansion8 = $00000017;

 {NTFS Record Numbers}
 ntfsQuotaRecordNumber   = $00000018;   {Only used during Format}
 ntfsObjIdRecordNumber   = $00000019;   {Only used during Format}
 ntfsReparseRecordNumber = $0000001A;   {Only used during Format}
 {ntfsUsnJrnlRecordNumber = $0000001B;} {Not Used}

 ntfsDefaultRecordCount  = $0000001B;  {Files up to $Reparse}
 ntfsDefaultMirrorCount  = $00000004;  {Files up to $Volume}

 {NTFS File References}
 ntfsRootFileReference   = $0005000000000005; {Only used during Format}
 ntfsExtendFileReference = $000B00000000000B; {Only used during Format}
 ntfsVolumeFileReference = $0003000000000003; {Only used during Format}

 {NTFS File Names}
 ntfsFileNameMft       = '$MFT';
 ntfsFileNameMftMirr   = '$MFTMirr';
 ntfsFileNameLogFile   = '$LogFile';
 ntfsFileNameVolume    = '$Volume';
 ntfsFileNameAttrDef   = '$AttrDef';
 ntfsFileNameRoot      = '.';
 ntfsFileNameBitmap    = '$Bitmap';
 ntfsFileNameBoot      = '$Boot';
 ntfsFileNameBadClus   = '$BadClus';
 ntfsFileNameSecure    = '$Secure';
 ntfsFileNameUpCase    = '$UpCase';
 ntfsFileNameExtend    = '$Extend';

 ntfsFileNameObjId     = '$ObjId';
 ntfsFileNameQuota     = '$Quota';
 ntfsFileNameReparse   = '$Reparse';
 ntfsFileNameUsnJrnl   = '$UsnJrnl';

 {NTFS Attribute Types}
 ntfsAttrTypeNone                      = $00000000;  {Marker only}
 ntfsAttrTypeStandardInformation       = $00000010;
 ntfsAttrTypeAttributeList             = $00000020;
 ntfsAttrTypeFileName                  = $00000030;
 ntfsAttrTypeObjectId                  = $00000040;
 ntfsAttrTypeVolumeVersion             = $00000040;  {Version 1.2 only}
 ntfsAttrTypeSecurityDescriptor        = $00000050;
 ntfsAttrTypeVolumeName                = $00000060;
 ntfsAttrTypeVolumeInformation         = $00000070;
 ntfsAttrTypeData                      = $00000080;
 ntfsAttrTypeIndexRoot                 = $00000090;
 ntfsAttrTypeIndexAllocation           = $000000A0;
 ntfsAttrTypeBitmap                    = $000000B0;
 ntfsAttrTypeReparsePoint              = $000000C0;
 ntfsAttrTypeSymbolicLink              = $000000C0;  {Version 1.2 only}
 ntfsAttrTypeExtendedAttrInformation   = $000000D0;
 ntfsAttrTypeExtendedAttr              = $000000E0;
 ntfsAttrTypePropertySet               = $000000F0;  {Version 3.0 only}
 ntfsAttrTypeLoggedUtilityStream       = $00000100;
 ntfsAttrTypeEnd                       = $FFFFFFFF;  {Marker only}

 ntfsAttrTypeAny                       = $00000000;  {Used to return all attributes}

 {NTFS Attribute Names}
 ntfsAttrNameNone                      = '$NONE';                 {Marker only}
 ntfsAttrNameStandardInformation       = '$STANDARD_INFORMATION';
 ntfsAttrNameAttributeList             = '$ATTRIBUTE_LIST';
 ntfsAttrNameFileName                  = '$FILE_NAME';
 ntfsAttrNameObjectId                  = '$OBJECT_ID';
 ntfsAttrNameVolumeVersion             = '$VOLUME_VERSION';       {Version 1.2 only}
 ntfsAttrNameSecurityDescriptor        = '$SECURITY_DESCRIPTOR';
 ntfsAttrNameVolumeName                = '$VOLUME_NAME';
 ntfsAttrNameVolumeInformation         = '$VOLUME_INFORMATION';
 ntfsAttrNameData                      = '$DATA';
 ntfsAttrNameIndexRoot                 = '$INDEX_ROOT';
 ntfsAttrNameIndexAllocation           = '$INDEX_ALLOCATION';
 ntfsAttrNameBitmap                    = '$BITMAP';
 ntfsAttrNameReparsePoint              = '$REPARSE_POINT';
 ntfsAttrNameSymbolicLink              = '$SYMBOLIC_LINK';        {Version 1.2 only}
 ntfsAttrNameExtendedAttrInformation   = '$EA_INFORMATION';
 ntfsAttrNameExtendedAttr              = '$EA';
 ntfsAttrNamePropertySet               = '$PROPERTY_SET';         {Version 3.0 only}
 ntfsAttrNameLoggedUtilityStream       = '$LOGGED_UTILITY_STREAM';
 ntfsAttrNameEnd                       = '$END';                  {Marker only}

 {NTFS Owner Ids}
 ntfsOwnerIdNull    = $00000001;
 ntfsOwnerIdFirst   = $00000100;

 ntfsOwnerIdUnknown = $FFFFFFFF;

 {NTFS Default Owner Ids}
 ntfsDefaultOwnerId1   = $00000001;
 ntfsDefaultOwnerId100 = $00000100;

 {NTFS Default Sids}
 ntfsMaxDefaultSid = 1;

 ntfsDefaultSidNone  = 0;
 ntfsDefaultSid100   = 1;

 ntfsDefaultSids:array[1..ntfsMaxDefaultSid] of TNTFSDefaultSid = (
  {100}
  (Sid:WinBuiltinAdministratorsSid));

 {NTFS Security Ids}
 ntfsSecurityIdNull    = $00000001;
 ntfsSecurityIdFirst   = $00000100;

 ntfsSecurityIdUnknown = $FFFFFFFF;

 {NTFS Default Security Ids}
 ntfsDefaultSecurityId100  = $00000100;
 ntfsDefaultSecurityId101  = $00000101;
 ntfsDefaultSecurityId102  = $00000102;
 ntfsDefaultSecurityId103  = $00000103;

 {NTFS Default Descriptors}
 ntfsMaxDefaultDescriptor = 7;

 ntfsDefaultDescriptorNone     = 0;
 ntfsDefaultDescriptorVolume   = 1;
 ntfsDefaultDescriptorAttrDef  = 2;
 ntfsDefaultDescriptorRoot     = 3;
 ntfsDefaultDescriptor102      = 4;
 ntfsDefaultDescriptor103      = 5;
 ntfsDefaultDescriptorFile     = 6; {Used by NTFSDefaultSecurity option}
 ntfsDefaultDescriptorFolder   = 7; {Used by NTFSDefaultSecurity option}

 ntfsDefaultDescriptorBoot     = 2; {Same as ntfsDefaultDescriptorAttrDef}
 ntfsDefaultDescriptor100      = 2; {Same as ntfsDefaultDescriptorAttrDef}
 ntfsDefaultDescriptor101      = 1; {Same as ntfsDefaultDescriptorVolume}
 ntfsDefaultDescriptorReserved = 1; {Same as ntfsDefaultDescriptorVolume}

 ntfsDefaultDescriptors:array[1..ntfsMaxDefaultDescriptor] of TNTFSDefaultDescriptor = (
  {Volume}{101}{Reserved}
  (Size:$00000068;
   Revision:$01;
   Control:$8004;
   OwnerOffset:$00000048;
   GroupOffset:$00000058;
   SaclOffset:$00000000;
   DaclOffset:$00000014;
   Owner:WinBuiltinAdministratorsSid;
   Group:WinBuiltinAdministratorsSid;
   Sacl:(AclRevision:$00;
         AclSize:$0000;
         AceCount:$0000;
         Aces:((AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid)
              )
        );
   Dacl:(AclRevision:$02;
         AclSize:$0034;
         AceCount:$0002;
         Aces:((AceType:$00;AceFlags:$00;AceSize:$0014;Mask:$0012019F;Sid:WinLocalSystemSid),
               (AceType:$00;AceFlags:$00;AceSize:$0018;Mask:$0012019F;Sid:WinBuiltinAdministratorsSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid)
              )
        );
  ),
  {AttrDef}{Boot}{100}
  (Size:$00000068;
   Revision:$01;
   Control:$8004;
   OwnerOffset:$00000048;
   GroupOffset:$00000058;
   SaclOffset:$00000000;
   DaclOffset:$00000014;
   Owner:WinBuiltinAdministratorsSid;
   Group:WinBuiltinAdministratorsSid;
   Sacl:(AclRevision:$00;
         AclSize:$0000;
         AceCount:$0000;
         Aces:((AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid)
              )
        );
   Dacl:(AclRevision:$02;
         AclSize:$0034;
         AceCount:$0002;
         Aces:((AceType:$00;AceFlags:$00;AceSize:$0014;Mask:$00120089;Sid:WinLocalSystemSid),
               (AceType:$00;AceFlags:$00;AceSize:$0018;Mask:$00120089;Sid:WinBuiltinAdministratorsSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid)
              )
        );
  ),
  {Root}
  (Size:$00001030;
   Revision:$01;
   Control:$8004;
   OwnerOffset:$00001014;
   GroupOffset:$00001024;
   SaclOffset:$00000000;
   DaclOffset:$00000014;
   Owner:WinBuiltinAdministratorsSid;
   Group:WinLocalSystemSid;
   Sacl:(AclRevision:$00;
         AclSize:$0000;
         AceCount:$0000;
         Aces:((AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid)
              )
        );
   Dacl:(AclRevision:$02;
         AclSize:$1000;
         AceCount:$0007;
         Aces:((AceType:$00;AceFlags:$03;AceSize:$0018;Mask:$001F01FF;Sid:WinBuiltinAdministratorsSid),
               (AceType:$00;AceFlags:$03;AceSize:$0014;Mask:$001F01FF;Sid:WinLocalSystemSid),
               (AceType:$00;AceFlags:$0B;AceSize:$0014;Mask:$10000000;Sid:WinCreatorOwnerSid),
               (AceType:$00;AceFlags:$03;AceSize:$0018;Mask:$001200A9;Sid:WinBuiltinUsersSid),
               (AceType:$00;AceFlags:$02;AceSize:$0018;Mask:$00000004;Sid:WinBuiltinUsersSid),
               (AceType:$00;AceFlags:$0A;AceSize:$0018;Mask:$00000002;Sid:WinBuiltinUsersSid),
               (AceType:$00;AceFlags:$00;AceSize:$0014;Mask:$001200A9;Sid:WinWorldSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid)
              )
        );
  ),
  {102}
  (Size:$0000004C;
   Revision:$01;
   Control:$8004;
   OwnerOffset:$00000030;
   GroupOffset:$00000040;
   SaclOffset:$00000000;
   DaclOffset:$00000014;
   Owner:WinBuiltinAdministratorsSid;
   Group:WinLocalSystemSid;
   Sacl:(AclRevision:$00;
         AclSize:$0000;
         AceCount:$0000;
         Aces:((AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid)
              )
        );
   Dacl:(AclRevision:$02;
         AclSize:$001C;
         AceCount:$0001;
         Aces:((AceType:$00;AceFlags:$00;AceSize:$0014;Mask:$001F01FF;Sid:WinLocalSystemSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid)
              )
        );
  ),
  {103}
  (Size:$00000064;
   Revision:$01;
   Control:$8004;
   OwnerOffset:$00000048;
   GroupOffset:$00000058;
   SaclOffset:$00000000;
   DaclOffset:$00000014;
   Owner:WinBuiltinAdministratorsSid;
   Group:WinLocalSystemSid;
   Sacl:(AclRevision:$00;
         AclSize:$0000;
         AceCount:$0000;
         Aces:((AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid)
              )
        );
   Dacl:(AclRevision:$02;
         AclSize:$0034;
         AceCount:$0002;
         Aces:((AceType:$00;AceFlags:$00;AceSize:$0018;Mask:$001F01FF;Sid:WinBuiltinAdministratorsSid),
               (AceType:$00;AceFlags:$00;AceSize:$0014;Mask:$001F01FF;Sid:WinLocalSystemSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid)
              )
        );
  ),
  {File}
  (Size:$0000004C;
   Revision:$01;
   Control:$9404;
   OwnerOffset:$00000030;
   GroupOffset:$00000040;
   SaclOffset:$00000000;
   DaclOffset:$00000014;
   Owner:WinBuiltinAdministratorsSid;
   Group:WinLocalSystemSid;
   Sacl:(AclRevision:$00;
         AclSize:$0000;
         AceCount:$0000;
         Aces:((AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid)
              )
        );
   Dacl:(AclRevision:$02;
         AclSize:$001C;
         AceCount:$0001;
         Aces:((AceType:$00;AceFlags:$00;AceSize:$0014;Mask:$001F01FF;Sid:WinWorldSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid)
              )
        );
  ),
  {Folder}
  (Size:$0000004C;
   Revision:$01;
   Control:$9404;
   OwnerOffset:$00000030;
   GroupOffset:$00000040;
   SaclOffset:$00000000;
   DaclOffset:$00000014;
   Owner:WinBuiltinAdministratorsSid;
   Group:WinLocalSystemSid;
   Sacl:(AclRevision:$00;
         AclSize:$0000;
         AceCount:$0000;
         Aces:((AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid)
              )
        );
   Dacl:(AclRevision:$02;
         AclSize:$001C;
         AceCount:$0001;
         Aces:((AceType:$00;AceFlags:$03;AceSize:$0014;Mask:$001F01FF;Sid:WinWorldSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid)
              )
        );
  ));

 {NTFS Security Offsets}
 ntfsSecurityOffsetFirst   = 0;  {Int64}

 ntfsSecurityOffsetUnknown = -1; {Int64}

 {NTFS Index Names}
 ntfsIndexNameFileName         = '$I30';
 ntfsIndexNameSecurityId       = '$SII';
 ntfsIndexNameSecurityHash     = '$SDH';
 ntfsIndexNameObjectId         = '$O';
 ntfsIndexNameOwnerId          = '$O';
 ntfsIndexNameQuota            = '$Q';
 ntfsIndexNameReparse          = '$R';

 {NTFS Index Order}
 ntfsIndexOrderFileName        = 38;
 ntfsIndexOrderSecurityId      = 100;
 ntfsIndexOrderSecurityHash    = 100;
 ntfsIndexOrderObjectId        = 100;
 ntfsIndexOrderOwnerId         = 100;
 ntfsIndexOrderQuota           = 100;
 ntfsIndexOrderReparse         = 100;

 {NTFS Stream Names}
 ntfsStreamNameSecurity        = '$SDS';
 ntfsStreamNameBadClus         = '$Bad';
 ntfsStreamNameMountMgr        = '$MountMgrDatabase';
 ntfsStreamNameJournal         = '$J';
 ntfsStreamNameJrnlMax         = '$Max';

 {NTFS File Namespaces}
 ntfsNameSpacePosix  = $00;
 ntfsNameSpaceWin32  = $01;
 ntfsNameSpaceDos    = $02;
 ntfsNameSpaceBoth   = ntfsNamespaceWin32 or ntfsNamespaceDos;

 {NTFS Display Types}
 ntfsDisplayTypeNone         = $00000000;

 {NTFS Collate Types}
 ntfsCollateTypeBinary       = $00000000; {Binary, where the first byte is most significant}
 ntfsCollateTypeFileName     = $00000001; {Filename Unicode strings}                                          {This is a case insensitive compare}
 ntfsCollateTypeUnicode      = $00000002; {Unicode strings, except that upper case letters should come first} {This is a case sensitive compare}
 ntfsCollateTypeLongWord     = $00000010; {An unsigned long (32 bits, little endian)}
 ntfsCollateTypeSID          = $00000011; {A Security Identifier}
 ntfsCollateTypeSecurityHash = $00000012; {First compare by the Security Hash, then by Security Descriptor (Sorting is by Security Hash then Security Id)}
 ntfsCollateTypeGUID         = $00000013; {Object GUIDs}

 {NTFS Flag Constants}
 ntfsVolumeFlagNone            = $0000;
 ntfsVolumeFlagDirty           = $0001;
 ntfsVolumeFlagResizeLogFile   = $0002;
 ntfsVolumeFlagUpgradeMount    = $0004;
 ntfsVolumeFlagMountedNT4      = $0008;
 ntfsVolumeFlagDeleteUSN       = $0010;
 ntfsVolumeFlagRepairObjectIds = $0020;
 ntfsVolumeFlagModifiedChkdsk  = $8000;

 ntfsFileRecordFlagNone         = $0000;
 ntfsFileRecordFlagInUse        = $0001;
 ntfsFileRecordFlagDirectory    = $0002;
 ntfsFileRecordFlagUnknown1     = $0004; {Used by $Quota,$ObjId,$Reparse}         //To Do //Probably something to do with no Data attribute ?
 ntfsFileRecordFlagIndexView    = $0008; {Used by $Secure,$Quota,$ObjId,$Reparse} //Maybe call it IndexOnly or something ?

 ntfsAttrDefFlagNone            = $00000000;
 ntfsAttrDefFlagIndexed         = $00000002;
 ntfsAttrDefFlagResident        = $00000040;  {Always resident}
 ntfsAttrDefFlagUncompressed    = $00000080;  {Cannot Compress} {Note: Resident Attributes cannot be compressed anyway}

 ntfsAttrDefFlagResidentIndex   = $00000042;  {Used by FileName (Resident and Indexed)}

 ntfsAttributeFlagNone          = $0000;
 ntfsAttributeFlagCompressed    = $0001;
 ntfsAttributeFlagEncrypted     = $4000;
 ntfsAttributeFlagSparse        = $8000;

 ntfsAttributeIndexed     = $01;
 ntfsAttributeNonIndexed  = $00;
 ntfsAttributeResident    = $00;
 ntfsAttributeNonResident = $01;

 ntfsIndexHeaderFlagNone     = $0000;
 ntfsIndexHeaderFlagSubNodes = $0001;  {Index has Sub Nodes}

 ntfsIndexEntryFlagNone     = $0000;
 ntfsIndexEntryFlagSubNode  = $0001;   {Entry has Sub Node}
 ntfsIndexEntryFlagLastNode = $0002;   {Entry is Last Node} {Always the Blank Node}

 ntfsQuotaFlagNone               = $0000;
 ntfsQuotaFlagDefaultLimits      = $0001;
 ntfsQuotaFlagLimitReached       = $0002;
 ntfsQuotaFlagIdDeleted          = $0004;
 ntfsQuotaFlagTrackingEnabled    = $0010;
 ntfsQuotaFlagEnforcementEnabled = $0020;
 ntfsQuotaFlagTrackingRequested  = $0040;
 ntfsQuotaFlagLogThreshold       = $0080;
 ntfsQuotaFlagLogLimit           = $0100;
 ntfsQuotaFlagOutOfDate          = $0200;
 ntfsQuotaFlagCorrupt            = $0400;
 ntfsQuotaFlagPendingDeletes     = $0800;

 ntfsReparseTagNone           = $00000000;
 ntfsReparseTagMountPoint     = $A0000003;
 ntfsReparseTagHSM            = $C0000004;
 ntfsReparseTagNSS            = $80000005;
 ntfsReparseTagNSSRecover     = $80000006; {ntfsReparseTagHSM2}
 ntfsReparseTagSIS            = $80000007;
{ntfsReparseTagDFS            = $80000008;} {Shown in some documentation}
 ntfsReparseTagDFS            = $8000000A;
 ntfsReparseTagFilterManager  = $8000000B;
 ntfsReparseTagSymbolicLink   = $A000000C;
 ntfsReparseTagDFSR           = $80000012;

 ntfsReparseTagFlagNone           = $00000000;
 ntfsReparseTagFlagIsAlias        = $20000000; {ntfsReparseTagFlagIsNameSurrogate}
 ntfsReparseTagFlagIsHighLatency  = $40000000;
 ntfsReparseTagFlagIsMicrosoft    = $80000000;

 ntfsReparseTagTypeNone           = $00000000;
 ntfsReparseTagTypeMountPoint     = $00000003;
 ntfsReparseTagTypeHSM            = $00000004;
 ntfsReparseTagTypeNSS            = $00000005;
 ntfsReparseTagTypeNSSRecover     = $00000006; {ntfsReparseTagHSM2}
 ntfsReparseTagTypeSIS            = $00000007;
{ntfsReparseTagTypeDFS            = $00000008;} {Shown in some documentation}
 ntfsReparseTagTypeDFS            = $0000000A;
 ntfsReparseTagTypeFilterManager  = $0000000B;
 ntfsReparseTagTypeSymbolicLink   = $0000000C;
 ntfsReparseTagTypeDFSR           = $00000012;

 ntfsReparsePointPrefix = '\??\';

 ntfsExtendedAttributeFlagNone    = $00;
 ntfsExtendedAttributeFlagNeedEA  = $80;

 {NTFS Default Sizes}
 ntfsDefaultBootRecordSize  = 8192;
 ntfsDefaultFileRecordSize  = 1024;
 ntfsDefaultIndexRecordSize = 4096;

 ntfsDefaultSecureSdsSize = 262396; {262616 if including Id 102 and 103}

 {NTFS Log File Constants}
 ntfsRestartPageSize = $1000;

 ntfsLogFileClientFirst   = $0000;
 ntfsLogFileClientUnknown = $FFFF;

 ntfsLogFileRestartFlagClean = $0002;

 {NTFS Log File Sizes (Old)}
 ntfsMaxOldLogSize = 10;
 ntfsOldLogSizes:array[0..ntfsMaxOldLogSize] of TNTFSLogSize = (
  {DiskSize,LogSize}
  (DiskSize:8388608;LogSize:262144),        {greater than 8MB,   256KB log}
  (DiskSize:16777216;LogSize:262144),       {greater than 16MB,  256KB log}
  (DiskSize:33554432;LogSize:524288),       {greater than 32MB,  512KB log} {Previously 262144}
  (DiskSize:67108864;LogSize:1048576),      {greater than 64MB,  1MB log}   {Previously 524288}
  (DiskSize:134217728;LogSize:2097152),     {greater than 128MB, 2MB log}   {Previously 1048576}
  (DiskSize:268435456;LogSize:2097152),     {greater than 256MB, 2MB log}
  (DiskSize:536870912;LogSize:4194304),     {greater than 512MB, 4MB log}
  (DiskSize:1073741824;LogSize:8388608),    {greater than 1GB,   8MB log}
  (DiskSize:2147483648;LogSize:16777216),   {greater than 2GB,   16MB log}
  (DiskSize:4294967296;LogSize:33554432),   {greater than 4GB,   32MB log}
  (DiskSize:8589934592;LogSize:67108864));  {greater than 8GB,   64MB log}

 {NTFS Log File Sizes}
 ntfsMaxLogSize = 8;
 ntfsLogSizes:array[0..ntfsMaxLogSize] of TNTFSLogSize = (
  {DiskSize,LogSize}
  (DiskSize:0;LogSize:2097152),             {less than 128MB, 2MB log}
  (DiskSize:134217728;LogSize:4194304),     {greater than 128MB, 4MB log}
  (DiskSize:268435456;LogSize:5242880),     {greater than 256MB, 5MB log}
  (DiskSize:536870912;LogSize:6291456),     {greater than 512MB, 6MB log}
  (DiskSize:805306368;LogSize:7340032),     {greater than 768MB, 7MB log}
  (DiskSize:1073741824;LogSize:8388608),    {greater than 1GB,   8MB log}
  (DiskSize:2147483648;LogSize:16777216),   {greater than 2GB,   16MB log}
  (DiskSize:4294967296;LogSize:33554432),   {greater than 4GB,   32MB log}
  (DiskSize:8589934592;LogSize:67108864));  {greater than 8GB,   64MB log}

 {NTFS MFT/MFTMirr Start}
 ntfsMftStartCluster = 786432;
 ntfsMftCutoverCount = 4194304;  {4194304 * 4096 = 16GB} {The point at which the MFT becomes fixed in location}

 {NTFS File Sizes}
 ntfsFileSizeMft       = ntfsUnknownCluster;    {Variable size}
 ntfsFileSizeMftMirr   = ntfsUnknownCluster;    {Variable size}
 ntfsFileSizeLogFile   = ntfsUnknownCluster;    {Variable size}
 ntfsFileSizeVolume    = 0;                     {Constant size}
 ntfsFileSize12AttrDef = 36864;                 {Constant size} //To Do //Check this size against WinNT4
 ntfsFileSize30AttrDef = 2560;                  {Constant size} //To Do //Check this size against Win2K
 ntfsFileSize31AttrDef = 2560;                  {Constant size}
 ntfsFileSizeRoot      = ntfsUnknownCluster;    {Variable size}
 ntfsFileSizeBitmap    = ntfsUnknownCluster;    {Variable size}
 ntfsFileSizeBoot      = 8192;                  {Constant size}
 ntfsFileSizeBadClus   = 0;                     {Starting size}
 ntfsFileSizeSecure    = 0;                     {Constant size}
 ntfsFileSizeUpCase    = 131072;                {Constant size}
 ntfsFileSizeExtend    = 0;                     {Constant size}

 ntfsFileSizeObjId     = 0;                     {Constant size}
 ntfsFileSizeQuota     = 0;                     {Constant size}
 ntfsFileSizeReparse   = 0;                     {Constant size}
 ntfsFileSizeUsnJrnl   = ntfsUnknownCluster;    {Variable size}

 {NTFS Stream Sizes}
 ntfsStreamSizeSecurity = ntfsUnknownCluster;   {Variable size}
 ntfsStreamSizeBadClus  = ntfsUnknownCluster;   {Variable size}
 ntfsStreamSizeMountMgr = ntfsUnknownCluster;   {Variable size}
 ntfsStreamSizeJournal  = ntfsUnknownCluster;   {Variable size}
 ntfsStreamSizeJrnlMax  = ntfsUnknownCluster;   {Variable size}

 {NTFS Header Sizes}
 ntfsFileRecord12Size        = 42;              {Constant size}
 ntfsFileRecord31Size        = 48;              {Constant size}

 ntfsIndexRecordSize         = 24;              {Constant size}
 ntfsIndexHeaderSize         = 16;              {Constant size}
 ntfsIndexEntrySize          = 16;              {Minimum size}

 ntfsAttributeTypeSize       = 4;               {Constant size}
 ntfsAttributeHeaderSize     = 16;              {Minimum size}
 ntfsResidentHeaderSize      = 24;              {Minimum size}
 ntfsNonResidentHeaderSize   = 64;              {Minimum size}
 ntfsCompressedHeaderSize    = 72;              {Minimum size}

 {NTFS Attribute Sizes} {Minimum sizes}
 ntfsStandardInformation12Size   = 48;          {Constant size}
 ntfsStandardInformation30Size   = 72;          {Constant size}
 ntfsAttributeListSize           = 0;           {Variable size}
 ntfsFileNameSize                = 66;          {Minimum size}
 ntfsObjectIdSize                = 16;          {Minimum size}
 ntfsObjectIdSize2               = 32;          {Minimum size}
 ntfsObjectIdSize3               = 48;          {Minimum size}
 ntfsObjectIdSize4               = 64;          {Minimum size}
 ntfsVolumeVersionSize           = 8;           {Constant size}
 ntfsSecurityDescriptorSize      = 0;           {Variable size}
 ntfsVolumeNameSize              = 0;           {Variable size}
 ntfsVolumeInformationSize       = 12;          {Constant size}
 ntfsDataSize                    = 0;           {Variable size}
 ntfsIndexRootSize               = 16;          {Constant size}
 ntfsIndexAllocationSize         = 0;           {Variable size}
 ntfsBitmapSize                  = 0;           {Variable size}
 ntfsReparsePointSize            = 8;           {Minimum size}
 ntfsReparsePointMicrosoftSize   = 8;           {Minimum size}
 ntfsReparsePointOtherSize       = 24;          {Minimum size}
 ntfsSymbolicLinkSize            = 0;           {Variable size}
 ntfsExtendedAttrInformationSize = 8;           {Constant size}
 ntfsExtendedAttrSize            = 0;           {Variable size}
 ntfsPropertySetSize             = 0;           {Variable size}
 ntfsLoggedUtilityStreamSize     = 0;           {Variable size}
 ntfsUnknownSize                 = 0;           {Variable size}
 ntfsEndSize                     = 4;           {Constant size}

 {NTFS Data Sizes} {Minimum sizes}
 ntfsRunSize           = 1;                     {Minimum size}
 ntfsItemSize          = 26;                    {Minimum size}
 ntfsObjIdSize         = 56;                    {Constant size}
 ntfsQuotaSize         = 48;                    {Minimum size}
 ntfsOwnerSize         = 4;                     {Constant size}
 ntfsUpCaseSize        = 131072;                {Constant size}
 ntfsAttrDefSize       = 160;                   {Constant size}
 ntfsReparseSize       = 12;                    {Minimum size}
 ntfsExtendedSize      = 8;                     {Minimum size}
 ntfsSecuritySize      = 20;                    {Minimum size}
 ntfsSidSize           = 8;                     {Minimum size}
 ntfsAclSize           = 8;                     {Minimum size}
 ntfsAceSize           = 8;                     {Minimum size}
 ntfsSecurityItemSize  = 40;                    {Minimum size}{Includes ntfsSecuritySize}
 ntfsSecurityIdSize    = 20;                    {Minimum size}
 ntfsSecurityHashSize  = 20;                    {Minimum size}{Previously 24 - Modified to remove Padding}

 {NTFS Key Sizes} {Minimum sizes}
 ntfsObjIdKeySize         = 16;                 {Minimum size}
 ntfsQuotaKeySize         = 4;                  {Minimum size}
 ntfsOwnerKeySize         = 8;                  {Minimum size}{Includes ntfsSidSize}
 ntfsSecurityIdKeySize    = 4;                  {Minimum size}
 ntfsSecurityHashKeySize  = 8;                  {Minimum size}
 ntfsReparseKeySize       = 12;                 {Constant size}{Not 16 even though CollateType is GUID}

 {NTFS Update Sequence Sizes}
 ntfsUpdateSequenceSize = 512;                  {Constant size}{Always 512 regardless of the sector size}

const
 {NTFS Attribute Definitions}
 ntfs12MaxAttrDefine = 13;
 ntfs12AttrDefines:array[0..ntfs12MaxAttrDefine] of TNTFSAttrDefine = (
  {AttributeType, AttributeName, AttrDefFlags, MinimumSize, MaximumSize}
  (AttributeType:ntfsAttrTypeStandardInformation;     AttributeName:ntfsAttrNameStandardInformation;     AttrDefFlags:ntfsAttrDefFlagResident;      MinimumSize:$30;                  MaximumSize:$30                 ),
  (AttributeType:ntfsAttrTypeAttributeList;           AttributeName:ntfsAttrNameAttributeList;           AttrDefFlags:ntfsAttrDefFlagUncompressed;  MinimumSize:ntfsAttrDefNoMinimum; MaximumSize:ntfsAttrDefNoMaximum),
  (AttributeType:ntfsAttrTypeFileName;                AttributeName:ntfsAttrNameFileName;                AttrDefFlags:ntfsAttrDefFlagResidentIndex; MinimumSize:$44;                  MaximumSize:$242                ),
  (AttributeType:ntfsAttrTypeVolumeVersion;           AttributeName:ntfsAttrNameVolumeVersion;           AttrDefFlags:ntfsAttrDefFlagResident;      MinimumSize:$8;                   MaximumSize:$8                  ),
  (AttributeType:ntfsAttrTypeSecurityDescriptor;      AttributeName:ntfsAttrNameSecurityDescriptor;      AttrDefFlags:ntfsAttrDefFlagUncompressed;  MinimumSize:ntfsAttrDefNoMinimum; MaximumSize:ntfsAttrDefNoMaximum),
  (AttributeType:ntfsAttrTypeVolumeName;              AttributeName:ntfsAttrNameVolumeName;              AttrDefFlags:ntfsAttrDefFlagResident;      MinimumSize:$2;                   MaximumSize:$100                ),
  (AttributeType:ntfsAttrTypeVolumeInformation;       AttributeName:ntfsAttrNameVolumeInformation;       AttrDefFlags:ntfsAttrDefFlagResident;      MinimumSize:$C;                   MaximumSize:$C                  ),
  (AttributeType:ntfsAttrTypeData;                    AttributeName:ntfsAttrNameData;                    AttrDefFlags:ntfsAttrDefFlagNone;          MinimumSize:ntfsAttrDefNoMinimum; MaximumSize:ntfsAttrDefNoMaximum),
  (AttributeType:ntfsAttrTypeIndexRoot;               AttributeName:ntfsAttrNameIndexRoot;               AttrDefFlags:ntfsAttrDefFlagResident;      MinimumSize:ntfsAttrDefNoMinimum; MaximumSize:ntfsAttrDefNoMaximum),
  (AttributeType:ntfsAttrTypeIndexAllocation;         AttributeName:ntfsAttrNameIndexAllocation;         AttrDefFlags:ntfsAttrDefFlagUncompressed;  MinimumSize:ntfsAttrDefNoMinimum; MaximumSize:ntfsAttrDefNoMaximum),
  (AttributeType:ntfsAttrTypeBitmap;                  AttributeName:ntfsAttrNameBitmap;                  AttrDefFlags:ntfsAttrDefFlagUncompressed;  MinimumSize:ntfsAttrDefNoMinimum; MaximumSize:ntfsAttrDefNoMaximum),
  (AttributeType:ntfsAttrTypeSymbolicLink;            AttributeName:ntfsAttrNameSymbolicLink;            AttrDefFlags:ntfsAttrDefFlagUncompressed;  MinimumSize:ntfsAttrDefNoMinimum; MaximumSize:ntfsAttrDefNoMaximum),
  (AttributeType:ntfsAttrTypeExtendedAttrInformation; AttributeName:ntfsAttrNameExtendedAttrInformation; AttrDefFlags:ntfsAttrDefFlagResident;      MinimumSize:$8;                   MaximumSize:$8                  ),
  (AttributeType:ntfsAttrTypeExtendedAttr;            AttributeName:ntfsAttrNameExtendedAttr;            AttrDefFlags:ntfsAttrDefFlagNone;          MinimumSize:ntfsAttrDefNoMinimum; MaximumSize:$10000              ));

 ntfs30MaxAttrDefine = 15;
 ntfs30AttrDefines:array[0..ntfs30MaxAttrDefine] of TNTFSAttrDefine = (
  {AttributeType, AttributeName, AttrDefFlags, MinimumSize, MaximumSize}
  (AttributeType:ntfsAttrTypeStandardInformation;     AttributeName:ntfsAttrNameStandardInformation;     AttrDefFlags:ntfsAttrDefFlagResident;      MinimumSize:$30;                  MaximumSize:$48                 ),
  (AttributeType:ntfsAttrTypeAttributeList;           AttributeName:ntfsAttrNameAttributeList;           AttrDefFlags:ntfsAttrDefFlagUncompressed;  MinimumSize:ntfsAttrDefNoMinimum; MaximumSize:ntfsAttrDefNoMaximum),
  (AttributeType:ntfsAttrTypeFileName;                AttributeName:ntfsAttrNameFileName;                AttrDefFlags:ntfsAttrDefFlagResidentIndex; MinimumSize:$44;                  MaximumSize:$242                ),
  (AttributeType:ntfsAttrTypeObjectId;                AttributeName:ntfsAttrNameObjectId;                AttrDefFlags:ntfsAttrDefFlagResident;      MinimumSize:ntfsAttrDefNoMinimum; MaximumSize:$100                ),
  (AttributeType:ntfsAttrTypeSecurityDescriptor;      AttributeName:ntfsAttrNameSecurityDescriptor;      AttrDefFlags:ntfsAttrDefFlagUncompressed;  MinimumSize:ntfsAttrDefNoMinimum; MaximumSize:ntfsAttrDefNoMaximum),
  (AttributeType:ntfsAttrTypeVolumeName;              AttributeName:ntfsAttrNameVolumeName;              AttrDefFlags:ntfsAttrDefFlagResident;      MinimumSize:$2;                   MaximumSize:$100                ),
  (AttributeType:ntfsAttrTypeVolumeInformation;       AttributeName:ntfsAttrNameVolumeInformation;       AttrDefFlags:ntfsAttrDefFlagResident;      MinimumSize:$C;                   MaximumSize:$C                  ),
  (AttributeType:ntfsAttrTypeData;                    AttributeName:ntfsAttrNameData;                    AttrDefFlags:ntfsAttrDefFlagNone;          MinimumSize:ntfsAttrDefNoMinimum; MaximumSize:ntfsAttrDefNoMaximum),
  (AttributeType:ntfsAttrTypeIndexRoot;               AttributeName:ntfsAttrNameIndexRoot;               AttrDefFlags:ntfsAttrDefFlagResident;      MinimumSize:ntfsAttrDefNoMinimum; MaximumSize:ntfsAttrDefNoMaximum),
  (AttributeType:ntfsAttrTypeIndexAllocation;         AttributeName:ntfsAttrNameIndexAllocation;         AttrDefFlags:ntfsAttrDefFlagUncompressed;  MinimumSize:ntfsAttrDefNoMinimum; MaximumSize:ntfsAttrDefNoMaximum),
  (AttributeType:ntfsAttrTypeBitmap;                  AttributeName:ntfsAttrNameBitmap;                  AttrDefFlags:ntfsAttrDefFlagUncompressed;  MinimumSize:ntfsAttrDefNoMinimum; MaximumSize:ntfsAttrDefNoMaximum),
  (AttributeType:ntfsAttrTypeReparsePoint;            AttributeName:ntfsAttrNameReparsePoint;            AttrDefFlags:ntfsAttrDefFlagUncompressed;  MinimumSize:ntfsAttrDefNoMinimum; MaximumSize:$4000               ),
  (AttributeType:ntfsAttrTypeExtendedAttrInformation; AttributeName:ntfsAttrNameExtendedAttrInformation; AttrDefFlags:ntfsAttrDefFlagResident;      MinimumSize:$8;                   MaximumSize:$8                  ),
  (AttributeType:ntfsAttrTypeExtendedAttr;            AttributeName:ntfsAttrNameExtendedAttr;            AttrDefFlags:ntfsAttrDefFlagNone;          MinimumSize:ntfsAttrDefNoMinimum; MaximumSize:$10000              ),
  (AttributeType:ntfsAttrTypePropertySet;             AttributeName:ntfsAttrNamePropertySet;             AttrDefFlags:ntfsAttrDefFlagNone;          MinimumSize:ntfsAttrDefNoMinimum; MaximumSize:ntfsAttrDefNoMaximum),
  (AttributeType:ntfsAttrTypeLoggedUtilityStream;     AttributeName:ntfsAttrNameLoggedUtilityStream;     AttrDefFlags:ntfsAttrDefFlagUncompressed;  MinimumSize:ntfsAttrDefNoMinimum; MaximumSize:$10000              ));

 ntfs31MaxAttrDefine = 14;
 ntfs31AttrDefines:array[0..ntfs31MaxAttrDefine] of TNTFSAttrDefine = (
  {AttributeType, AttributeName, AttrDefFlags, MinimumSize, MaximumSize}
  (AttributeType:ntfsAttrTypeStandardInformation;     AttributeName:ntfsAttrNameStandardInformation;     AttrDefFlags:ntfsAttrDefFlagResident;      MinimumSize:$30;                  MaximumSize:$48                 ),
  (AttributeType:ntfsAttrTypeAttributeList;           AttributeName:ntfsAttrNameAttributeList;           AttrDefFlags:ntfsAttrDefFlagUncompressed;  MinimumSize:ntfsAttrDefNoMinimum; MaximumSize:ntfsAttrDefNoMaximum),
  (AttributeType:ntfsAttrTypeFileName;                AttributeName:ntfsAttrNameFileName;                AttrDefFlags:ntfsAttrDefFlagResidentIndex; MinimumSize:$44;                  MaximumSize:$242                ),
  (AttributeType:ntfsAttrTypeObjectId;                AttributeName:ntfsAttrNameObjectId;                AttrDefFlags:ntfsAttrDefFlagResident;      MinimumSize:ntfsAttrDefNoMinimum; MaximumSize:$100                ),
  (AttributeType:ntfsAttrTypeSecurityDescriptor;      AttributeName:ntfsAttrNameSecurityDescriptor;      AttrDefFlags:ntfsAttrDefFlagUncompressed;  MinimumSize:ntfsAttrDefNoMinimum; MaximumSize:ntfsAttrDefNoMaximum),
  (AttributeType:ntfsAttrTypeVolumeName;              AttributeName:ntfsAttrNameVolumeName;              AttrDefFlags:ntfsAttrDefFlagResident;      MinimumSize:$2;                   MaximumSize:$100                ),
  (AttributeType:ntfsAttrTypeVolumeInformation;       AttributeName:ntfsAttrNameVolumeInformation;       AttrDefFlags:ntfsAttrDefFlagResident;      MinimumSize:$C;                   MaximumSize:$C                  ),
  (AttributeType:ntfsAttrTypeData;                    AttributeName:ntfsAttrNameData;                    AttrDefFlags:ntfsAttrDefFlagNone;          MinimumSize:ntfsAttrDefNoMinimum; MaximumSize:ntfsAttrDefNoMaximum),
  (AttributeType:ntfsAttrTypeIndexRoot;               AttributeName:ntfsAttrNameIndexRoot;               AttrDefFlags:ntfsAttrDefFlagResident;      MinimumSize:ntfsAttrDefNoMinimum; MaximumSize:ntfsAttrDefNoMaximum),
  (AttributeType:ntfsAttrTypeIndexAllocation;         AttributeName:ntfsAttrNameIndexAllocation;         AttrDefFlags:ntfsAttrDefFlagUncompressed;  MinimumSize:ntfsAttrDefNoMinimum; MaximumSize:ntfsAttrDefNoMaximum),
  (AttributeType:ntfsAttrTypeBitmap;                  AttributeName:ntfsAttrNameBitmap;                  AttrDefFlags:ntfsAttrDefFlagUncompressed;  MinimumSize:ntfsAttrDefNoMinimum; MaximumSize:ntfsAttrDefNoMaximum),
  (AttributeType:ntfsAttrTypeReparsePoint;            AttributeName:ntfsAttrNameReparsePoint;            AttrDefFlags:ntfsAttrDefFlagUncompressed;  MinimumSize:ntfsAttrDefNoMinimum; MaximumSize:$4000               ),
  (AttributeType:ntfsAttrTypeExtendedAttrInformation; AttributeName:ntfsAttrNameExtendedAttrInformation; AttrDefFlags:ntfsAttrDefFlagResident;      MinimumSize:$8;                   MaximumSize:$8                  ),
  (AttributeType:ntfsAttrTypeExtendedAttr;            AttributeName:ntfsAttrNameExtendedAttr;            AttrDefFlags:ntfsAttrDefFlagNone;          MinimumSize:ntfsAttrDefNoMinimum; MaximumSize:$10000              ),
  (AttributeType:ntfsAttrTypeLoggedUtilityStream;     AttributeName:ntfsAttrNameLoggedUtilityStream;     AttrDefFlags:ntfsAttrDefFlagUncompressed;  MinimumSize:ntfsAttrDefNoMinimum; MaximumSize:$10000              ));

 {NTFS Uppercase Conversions}  {This table was generated from an NTFS 3.1 (Windows XP) volume}
 ntfsMaxUpcaseConvert = 635;
 ntfsUpcaseConverts:array[0..ntfsMaxUpcaseConvert] of TNTFSUpcaseConvert = (
  {Count,Value}
  (Count:$0061;Value:$0041),(Count:$0062;Value:$0042),(Count:$0063;Value:$0043),(Count:$0064;Value:$0044),(Count:$0065;Value:$0045),(Count:$0066;Value:$0046),
  (Count:$0067;Value:$0047),(Count:$0068;Value:$0048),(Count:$0069;Value:$0049),(Count:$006A;Value:$004A),(Count:$006B;Value:$004B),(Count:$006C;Value:$004C),
  (Count:$006D;Value:$004D),(Count:$006E;Value:$004E),(Count:$006F;Value:$004F),(Count:$0070;Value:$0050),(Count:$0071;Value:$0051),(Count:$0072;Value:$0052),
  (Count:$0073;Value:$0053),(Count:$0074;Value:$0054),(Count:$0075;Value:$0055),(Count:$0076;Value:$0056),(Count:$0077;Value:$0057),(Count:$0078;Value:$0058),
  (Count:$0079;Value:$0059),(Count:$007A;Value:$005A),(Count:$00E0;Value:$00C0),(Count:$00E1;Value:$00C1),(Count:$00E2;Value:$00C2),(Count:$00E3;Value:$00C3),
  (Count:$00E4;Value:$00C4),(Count:$00E5;Value:$00C5),(Count:$00E6;Value:$00C6),(Count:$00E7;Value:$00C7),(Count:$00E8;Value:$00C8),(Count:$00E9;Value:$00C9),
  (Count:$00EA;Value:$00CA),(Count:$00EB;Value:$00CB),(Count:$00EC;Value:$00CC),(Count:$00ED;Value:$00CD),(Count:$00EE;Value:$00CE),(Count:$00EF;Value:$00CF),
  (Count:$00F0;Value:$00D0),(Count:$00F1;Value:$00D1),(Count:$00F2;Value:$00D2),(Count:$00F3;Value:$00D3),(Count:$00F4;Value:$00D4),(Count:$00F5;Value:$00D5),
  (Count:$00F6;Value:$00D6),(Count:$00F8;Value:$00D8),(Count:$00F9;Value:$00D9),(Count:$00FA;Value:$00DA),(Count:$00FB;Value:$00DB),(Count:$00FC;Value:$00DC),
  (Count:$00FD;Value:$00DD),(Count:$00FE;Value:$00DE),(Count:$00FF;Value:$0178),(Count:$0101;Value:$0100),(Count:$0103;Value:$0102),(Count:$0105;Value:$0104),
  (Count:$0107;Value:$0106),(Count:$0109;Value:$0108),(Count:$010B;Value:$010A),(Count:$010D;Value:$010C),(Count:$010F;Value:$010E),(Count:$0111;Value:$0110),
  (Count:$0113;Value:$0112),(Count:$0115;Value:$0114),(Count:$0117;Value:$0116),(Count:$0119;Value:$0118),(Count:$011B;Value:$011A),(Count:$011D;Value:$011C),
  (Count:$011F;Value:$011E),(Count:$0121;Value:$0120),(Count:$0123;Value:$0122),(Count:$0125;Value:$0124),(Count:$0127;Value:$0126),(Count:$0129;Value:$0128),
  (Count:$012B;Value:$012A),(Count:$012D;Value:$012C),(Count:$012F;Value:$012E),(Count:$0133;Value:$0132),(Count:$0135;Value:$0134),(Count:$0137;Value:$0136),
  (Count:$013A;Value:$0139),(Count:$013C;Value:$013B),(Count:$013E;Value:$013D),(Count:$0140;Value:$013F),(Count:$0142;Value:$0141),(Count:$0144;Value:$0143),
  (Count:$0146;Value:$0145),(Count:$0148;Value:$0147),(Count:$014B;Value:$014A),(Count:$014D;Value:$014C),(Count:$014F;Value:$014E),(Count:$0151;Value:$0150),
  (Count:$0153;Value:$0152),(Count:$0155;Value:$0154),(Count:$0157;Value:$0156),(Count:$0159;Value:$0158),(Count:$015B;Value:$015A),(Count:$015D;Value:$015C),
  (Count:$015F;Value:$015E),(Count:$0161;Value:$0160),(Count:$0163;Value:$0162),(Count:$0165;Value:$0164),(Count:$0167;Value:$0166),(Count:$0169;Value:$0168),
  (Count:$016B;Value:$016A),(Count:$016D;Value:$016C),(Count:$016F;Value:$016E),(Count:$0171;Value:$0170),(Count:$0173;Value:$0172),(Count:$0175;Value:$0174),
  (Count:$0177;Value:$0176),(Count:$017A;Value:$0179),(Count:$017C;Value:$017B),(Count:$017E;Value:$017D),(Count:$0183;Value:$0182),(Count:$0185;Value:$0184),
  (Count:$0188;Value:$0187),(Count:$018C;Value:$018B),(Count:$0192;Value:$0191),(Count:$0199;Value:$0198),(Count:$01A1;Value:$01A0),(Count:$01A3;Value:$01A2),
  (Count:$01A5;Value:$01A4),(Count:$01A8;Value:$01A7),(Count:$01AD;Value:$01AC),(Count:$01B0;Value:$01AF),(Count:$01B4;Value:$01B3),(Count:$01B6;Value:$01B5),
  (Count:$01B9;Value:$01B8),(Count:$01BD;Value:$01BC),(Count:$01C6;Value:$01C4),(Count:$01C9;Value:$01C7),(Count:$01CC;Value:$01CA),(Count:$01CE;Value:$01CD),
  (Count:$01D0;Value:$01CF),(Count:$01D2;Value:$01D1),(Count:$01D4;Value:$01D3),(Count:$01D6;Value:$01D5),(Count:$01D8;Value:$01D7),(Count:$01DA;Value:$01D9),
  (Count:$01DC;Value:$01DB),(Count:$01DD;Value:$018E),(Count:$01DF;Value:$01DE),(Count:$01E1;Value:$01E0),(Count:$01E3;Value:$01E2),(Count:$01E5;Value:$01E4),
  (Count:$01E7;Value:$01E6),(Count:$01E9;Value:$01E8),(Count:$01EB;Value:$01EA),(Count:$01ED;Value:$01EC),(Count:$01EF;Value:$01EE),(Count:$01F3;Value:$01F1),
  (Count:$01F5;Value:$01F4),(Count:$01FB;Value:$01FA),(Count:$01FD;Value:$01FC),(Count:$01FF;Value:$01FE),(Count:$0201;Value:$0200),(Count:$0203;Value:$0202),
  (Count:$0205;Value:$0204),(Count:$0207;Value:$0206),(Count:$0209;Value:$0208),(Count:$020B;Value:$020A),(Count:$020D;Value:$020C),(Count:$020F;Value:$020E),
  (Count:$0211;Value:$0210),(Count:$0213;Value:$0212),(Count:$0215;Value:$0214),(Count:$0217;Value:$0216),(Count:$0253;Value:$0181),(Count:$0254;Value:$0186),
  (Count:$0256;Value:$0189),(Count:$0257;Value:$018A),(Count:$0259;Value:$018F),(Count:$025B;Value:$0190),(Count:$0260;Value:$0193),(Count:$0263;Value:$0194),
  (Count:$0268;Value:$0197),(Count:$0269;Value:$0196),(Count:$026F;Value:$019C),(Count:$0272;Value:$019D),(Count:$0275;Value:$019F),(Count:$0283;Value:$01A9),
  (Count:$0288;Value:$01AE),(Count:$028A;Value:$01B1),(Count:$028B;Value:$01B2),(Count:$0292;Value:$01B7),(Count:$03AC;Value:$0386),(Count:$03AD;Value:$0388),
  (Count:$03AE;Value:$0389),(Count:$03AF;Value:$038A),(Count:$03B1;Value:$0391),(Count:$03B2;Value:$0392),(Count:$03B3;Value:$0393),(Count:$03B4;Value:$0394),
  (Count:$03B5;Value:$0395),(Count:$03B6;Value:$0396),(Count:$03B7;Value:$0397),(Count:$03B8;Value:$0398),(Count:$03B9;Value:$0399),(Count:$03BA;Value:$039A),
  (Count:$03BB;Value:$039B),(Count:$03BC;Value:$039C),(Count:$03BD;Value:$039D),(Count:$03BE;Value:$039E),(Count:$03BF;Value:$039F),(Count:$03C0;Value:$03A0),
  (Count:$03C1;Value:$03A1),(Count:$03C2;Value:$03A3),(Count:$03C3;Value:$03A3),(Count:$03C4;Value:$03A4),(Count:$03C5;Value:$03A5),(Count:$03C6;Value:$03A6),
  (Count:$03C7;Value:$03A7),(Count:$03C8;Value:$03A8),(Count:$03C9;Value:$03A9),(Count:$03CA;Value:$03AA),(Count:$03CB;Value:$03AB),(Count:$03CC;Value:$038C),
  (Count:$03CD;Value:$038E),(Count:$03CE;Value:$038F),(Count:$03E3;Value:$03E2),(Count:$03E5;Value:$03E4),(Count:$03E7;Value:$03E6),(Count:$03E9;Value:$03E8),
  (Count:$03EB;Value:$03EA),(Count:$03ED;Value:$03EC),(Count:$03EF;Value:$03EE),(Count:$0430;Value:$0410),(Count:$0431;Value:$0411),(Count:$0432;Value:$0412),
  (Count:$0433;Value:$0413),(Count:$0434;Value:$0414),(Count:$0435;Value:$0415),(Count:$0436;Value:$0416),(Count:$0437;Value:$0417),(Count:$0438;Value:$0418),
  (Count:$0439;Value:$0419),(Count:$043A;Value:$041A),(Count:$043B;Value:$041B),(Count:$043C;Value:$041C),(Count:$043D;Value:$041D),(Count:$043E;Value:$041E),
  (Count:$043F;Value:$041F),(Count:$0440;Value:$0420),(Count:$0441;Value:$0421),(Count:$0442;Value:$0422),(Count:$0443;Value:$0423),(Count:$0444;Value:$0424),
  (Count:$0445;Value:$0425),(Count:$0446;Value:$0426),(Count:$0447;Value:$0427),(Count:$0448;Value:$0428),(Count:$0449;Value:$0429),(Count:$044A;Value:$042A),
  (Count:$044B;Value:$042B),(Count:$044C;Value:$042C),(Count:$044D;Value:$042D),(Count:$044E;Value:$042E),(Count:$044F;Value:$042F),(Count:$0451;Value:$0401),
  (Count:$0452;Value:$0402),(Count:$0453;Value:$0403),(Count:$0454;Value:$0404),(Count:$0455;Value:$0405),(Count:$0456;Value:$0406),(Count:$0457;Value:$0407),
  (Count:$0458;Value:$0408),(Count:$0459;Value:$0409),(Count:$045A;Value:$040A),(Count:$045B;Value:$040B),(Count:$045C;Value:$040C),(Count:$045E;Value:$040E),
  (Count:$045F;Value:$040F),(Count:$0461;Value:$0460),(Count:$0463;Value:$0462),(Count:$0465;Value:$0464),(Count:$0467;Value:$0466),(Count:$0469;Value:$0468),
  (Count:$046B;Value:$046A),(Count:$046D;Value:$046C),(Count:$046F;Value:$046E),(Count:$0471;Value:$0470),(Count:$0473;Value:$0472),(Count:$0475;Value:$0474),
  (Count:$0477;Value:$0476),(Count:$0479;Value:$0478),(Count:$047B;Value:$047A),(Count:$047D;Value:$047C),(Count:$047F;Value:$047E),(Count:$0481;Value:$0480),
  (Count:$0491;Value:$0490),(Count:$0493;Value:$0492),(Count:$0495;Value:$0494),(Count:$0497;Value:$0496),(Count:$0499;Value:$0498),(Count:$049B;Value:$049A),
  (Count:$049D;Value:$049C),(Count:$049F;Value:$049E),(Count:$04A1;Value:$04A0),(Count:$04A3;Value:$04A2),(Count:$04A5;Value:$04A4),(Count:$04A7;Value:$04A6),
  (Count:$04A9;Value:$04A8),(Count:$04AB;Value:$04AA),(Count:$04AD;Value:$04AC),(Count:$04AF;Value:$04AE),(Count:$04B1;Value:$04B0),(Count:$04B3;Value:$04B2),
  (Count:$04B5;Value:$04B4),(Count:$04B7;Value:$04B6),(Count:$04B9;Value:$04B8),(Count:$04BB;Value:$04BA),(Count:$04BD;Value:$04BC),(Count:$04BF;Value:$04BE),
  (Count:$04C2;Value:$04C1),(Count:$04C4;Value:$04C3),(Count:$04C8;Value:$04C7),(Count:$04CC;Value:$04CB),(Count:$04D1;Value:$04D0),(Count:$04D3;Value:$04D2),
  (Count:$04D5;Value:$04D4),(Count:$04D7;Value:$04D6),(Count:$04D9;Value:$04D8),(Count:$04DB;Value:$04DA),(Count:$04DD;Value:$04DC),(Count:$04DF;Value:$04DE),
  (Count:$04E1;Value:$04E0),(Count:$04E3;Value:$04E2),(Count:$04E5;Value:$04E4),(Count:$04E7;Value:$04E6),(Count:$04E9;Value:$04E8),(Count:$04EB;Value:$04EA),
  (Count:$04EF;Value:$04EE),(Count:$04F1;Value:$04F0),(Count:$04F3;Value:$04F2),(Count:$04F5;Value:$04F4),(Count:$04F9;Value:$04F8),(Count:$0561;Value:$0531),
  (Count:$0562;Value:$0532),(Count:$0563;Value:$0533),(Count:$0564;Value:$0534),(Count:$0565;Value:$0535),(Count:$0566;Value:$0536),(Count:$0567;Value:$0537),
  (Count:$0568;Value:$0538),(Count:$0569;Value:$0539),(Count:$056A;Value:$053A),(Count:$056B;Value:$053B),(Count:$056C;Value:$053C),(Count:$056D;Value:$053D),
  (Count:$056E;Value:$053E),(Count:$056F;Value:$053F),(Count:$0570;Value:$0540),(Count:$0571;Value:$0541),(Count:$0572;Value:$0542),(Count:$0573;Value:$0543),
  (Count:$0574;Value:$0544),(Count:$0575;Value:$0545),(Count:$0576;Value:$0546),(Count:$0577;Value:$0547),(Count:$0578;Value:$0548),(Count:$0579;Value:$0549),
  (Count:$057A;Value:$054A),(Count:$057B;Value:$054B),(Count:$057C;Value:$054C),(Count:$057D;Value:$054D),(Count:$057E;Value:$054E),(Count:$057F;Value:$054F),
  (Count:$0580;Value:$0550),(Count:$0581;Value:$0551),(Count:$0582;Value:$0552),(Count:$0583;Value:$0553),(Count:$0584;Value:$0554),(Count:$0585;Value:$0555),
  (Count:$0586;Value:$0556),(Count:$1E01;Value:$1E00),(Count:$1E03;Value:$1E02),(Count:$1E05;Value:$1E04),(Count:$1E07;Value:$1E06),(Count:$1E09;Value:$1E08),
  (Count:$1E0B;Value:$1E0A),(Count:$1E0D;Value:$1E0C),(Count:$1E0F;Value:$1E0E),(Count:$1E11;Value:$1E10),(Count:$1E13;Value:$1E12),(Count:$1E15;Value:$1E14),
  (Count:$1E17;Value:$1E16),(Count:$1E19;Value:$1E18),(Count:$1E1B;Value:$1E1A),(Count:$1E1D;Value:$1E1C),(Count:$1E1F;Value:$1E1E),(Count:$1E21;Value:$1E20),
  (Count:$1E23;Value:$1E22),(Count:$1E25;Value:$1E24),(Count:$1E27;Value:$1E26),(Count:$1E29;Value:$1E28),(Count:$1E2B;Value:$1E2A),(Count:$1E2D;Value:$1E2C),
  (Count:$1E2F;Value:$1E2E),(Count:$1E31;Value:$1E30),(Count:$1E33;Value:$1E32),(Count:$1E35;Value:$1E34),(Count:$1E37;Value:$1E36),(Count:$1E39;Value:$1E38),
  (Count:$1E3B;Value:$1E3A),(Count:$1E3D;Value:$1E3C),(Count:$1E3F;Value:$1E3E),(Count:$1E41;Value:$1E40),(Count:$1E43;Value:$1E42),(Count:$1E45;Value:$1E44),
  (Count:$1E47;Value:$1E46),(Count:$1E49;Value:$1E48),(Count:$1E4B;Value:$1E4A),(Count:$1E4D;Value:$1E4C),(Count:$1E4F;Value:$1E4E),(Count:$1E51;Value:$1E50),
  (Count:$1E53;Value:$1E52),(Count:$1E55;Value:$1E54),(Count:$1E57;Value:$1E56),(Count:$1E59;Value:$1E58),(Count:$1E5B;Value:$1E5A),(Count:$1E5D;Value:$1E5C),
  (Count:$1E5F;Value:$1E5E),(Count:$1E61;Value:$1E60),(Count:$1E63;Value:$1E62),(Count:$1E65;Value:$1E64),(Count:$1E67;Value:$1E66),(Count:$1E69;Value:$1E68),
  (Count:$1E6B;Value:$1E6A),(Count:$1E6D;Value:$1E6C),(Count:$1E6F;Value:$1E6E),(Count:$1E71;Value:$1E70),(Count:$1E73;Value:$1E72),(Count:$1E75;Value:$1E74),
  (Count:$1E77;Value:$1E76),(Count:$1E79;Value:$1E78),(Count:$1E7B;Value:$1E7A),(Count:$1E7D;Value:$1E7C),(Count:$1E7F;Value:$1E7E),(Count:$1E81;Value:$1E80),
  (Count:$1E83;Value:$1E82),(Count:$1E85;Value:$1E84),(Count:$1E87;Value:$1E86),(Count:$1E89;Value:$1E88),(Count:$1E8B;Value:$1E8A),(Count:$1E8D;Value:$1E8C),
  (Count:$1E8F;Value:$1E8E),(Count:$1E91;Value:$1E90),(Count:$1E93;Value:$1E92),(Count:$1E95;Value:$1E94),(Count:$1EA1;Value:$1EA0),(Count:$1EA3;Value:$1EA2),
  (Count:$1EA5;Value:$1EA4),(Count:$1EA7;Value:$1EA6),(Count:$1EA9;Value:$1EA8),(Count:$1EAB;Value:$1EAA),(Count:$1EAD;Value:$1EAC),(Count:$1EAF;Value:$1EAE),
  (Count:$1EB1;Value:$1EB0),(Count:$1EB3;Value:$1EB2),(Count:$1EB5;Value:$1EB4),(Count:$1EB7;Value:$1EB6),(Count:$1EB9;Value:$1EB8),(Count:$1EBB;Value:$1EBA),
  (Count:$1EBD;Value:$1EBC),(Count:$1EBF;Value:$1EBE),(Count:$1EC1;Value:$1EC0),(Count:$1EC3;Value:$1EC2),(Count:$1EC5;Value:$1EC4),(Count:$1EC7;Value:$1EC6),
  (Count:$1EC9;Value:$1EC8),(Count:$1ECB;Value:$1ECA),(Count:$1ECD;Value:$1ECC),(Count:$1ECF;Value:$1ECE),(Count:$1ED1;Value:$1ED0),(Count:$1ED3;Value:$1ED2),
  (Count:$1ED5;Value:$1ED4),(Count:$1ED7;Value:$1ED6),(Count:$1ED9;Value:$1ED8),(Count:$1EDB;Value:$1EDA),(Count:$1EDD;Value:$1EDC),(Count:$1EDF;Value:$1EDE),
  (Count:$1EE1;Value:$1EE0),(Count:$1EE3;Value:$1EE2),(Count:$1EE5;Value:$1EE4),(Count:$1EE7;Value:$1EE6),(Count:$1EE9;Value:$1EE8),(Count:$1EEB;Value:$1EEA),
  (Count:$1EED;Value:$1EEC),(Count:$1EEF;Value:$1EEE),(Count:$1EF1;Value:$1EF0),(Count:$1EF3;Value:$1EF2),(Count:$1EF5;Value:$1EF4),(Count:$1EF7;Value:$1EF6),
  (Count:$1EF9;Value:$1EF8),(Count:$1F00;Value:$1F08),(Count:$1F01;Value:$1F09),(Count:$1F02;Value:$1F0A),(Count:$1F03;Value:$1F0B),(Count:$1F04;Value:$1F0C),
  (Count:$1F05;Value:$1F0D),(Count:$1F06;Value:$1F0E),(Count:$1F07;Value:$1F0F),(Count:$1F10;Value:$1F18),(Count:$1F11;Value:$1F19),(Count:$1F12;Value:$1F1A),
  (Count:$1F13;Value:$1F1B),(Count:$1F14;Value:$1F1C),(Count:$1F15;Value:$1F1D),(Count:$1F20;Value:$1F28),(Count:$1F21;Value:$1F29),(Count:$1F22;Value:$1F2A),
  (Count:$1F23;Value:$1F2B),(Count:$1F24;Value:$1F2C),(Count:$1F25;Value:$1F2D),(Count:$1F26;Value:$1F2E),(Count:$1F27;Value:$1F2F),(Count:$1F30;Value:$1F38),
  (Count:$1F31;Value:$1F39),(Count:$1F32;Value:$1F3A),(Count:$1F33;Value:$1F3B),(Count:$1F34;Value:$1F3C),(Count:$1F35;Value:$1F3D),(Count:$1F36;Value:$1F3E),
  (Count:$1F37;Value:$1F3F),(Count:$1F40;Value:$1F48),(Count:$1F41;Value:$1F49),(Count:$1F42;Value:$1F4A),(Count:$1F43;Value:$1F4B),(Count:$1F44;Value:$1F4C),
  (Count:$1F45;Value:$1F4D),(Count:$1F51;Value:$1F59),(Count:$1F53;Value:$1F5B),(Count:$1F55;Value:$1F5D),(Count:$1F57;Value:$1F5F),(Count:$1F60;Value:$1F68),
  (Count:$1F61;Value:$1F69),(Count:$1F62;Value:$1F6A),(Count:$1F63;Value:$1F6B),(Count:$1F64;Value:$1F6C),(Count:$1F65;Value:$1F6D),(Count:$1F66;Value:$1F6E),
  (Count:$1F67;Value:$1F6F),(Count:$1F70;Value:$1FBA),(Count:$1F71;Value:$1FBB),(Count:$1F72;Value:$1FC8),(Count:$1F73;Value:$1FC9),(Count:$1F74;Value:$1FCA),
  (Count:$1F75;Value:$1FCB),(Count:$1F76;Value:$1FDA),(Count:$1F77;Value:$1FDB),(Count:$1F78;Value:$1FF8),(Count:$1F79;Value:$1FF9),(Count:$1F7A;Value:$1FEA),
  (Count:$1F7B;Value:$1FEB),(Count:$1F7C;Value:$1FFA),(Count:$1F7D;Value:$1FFB),(Count:$1FB0;Value:$1FB8),(Count:$1FB1;Value:$1FB9),(Count:$1FD0;Value:$1FD8),
  (Count:$1FD1;Value:$1FD9),(Count:$1FE0;Value:$1FE8),(Count:$1FE1;Value:$1FE9),(Count:$1FE5;Value:$1FEC),(Count:$2170;Value:$2160),(Count:$2171;Value:$2161),
  (Count:$2172;Value:$2162),(Count:$2173;Value:$2163),(Count:$2174;Value:$2164),(Count:$2175;Value:$2165),(Count:$2176;Value:$2166),(Count:$2177;Value:$2167),
  (Count:$2178;Value:$2168),(Count:$2179;Value:$2169),(Count:$217A;Value:$216A),(Count:$217B;Value:$216B),(Count:$217C;Value:$216C),(Count:$217D;Value:$216D),
  (Count:$217E;Value:$216E),(Count:$217F;Value:$216F),(Count:$24D0;Value:$24B6),(Count:$24D1;Value:$24B7),(Count:$24D2;Value:$24B8),(Count:$24D3;Value:$24B9),
  (Count:$24D4;Value:$24BA),(Count:$24D5;Value:$24BB),(Count:$24D6;Value:$24BC),(Count:$24D7;Value:$24BD),(Count:$24D8;Value:$24BE),(Count:$24D9;Value:$24BF),
  (Count:$24DA;Value:$24C0),(Count:$24DB;Value:$24C1),(Count:$24DC;Value:$24C2),(Count:$24DD;Value:$24C3),(Count:$24DE;Value:$24C4),(Count:$24DF;Value:$24C5),
  (Count:$24E0;Value:$24C6),(Count:$24E1;Value:$24C7),(Count:$24E2;Value:$24C8),(Count:$24E3;Value:$24C9),(Count:$24E4;Value:$24CA),(Count:$24E5;Value:$24CB),
  (Count:$24E6;Value:$24CC),(Count:$24E7;Value:$24CD),(Count:$24E8;Value:$24CE),(Count:$24E9;Value:$24CF),(Count:$FF41;Value:$FF21),(Count:$FF42;Value:$FF22),
  (Count:$FF43;Value:$FF23),(Count:$FF44;Value:$FF24),(Count:$FF45;Value:$FF25),(Count:$FF46;Value:$FF26),(Count:$FF47;Value:$FF27),(Count:$FF48;Value:$FF28),
  (Count:$FF49;Value:$FF29),(Count:$FF4A;Value:$FF2A),(Count:$FF4B;Value:$FF2B),(Count:$FF4C;Value:$FF2C),(Count:$FF4D;Value:$FF2D),(Count:$FF4E;Value:$FF2E),
  (Count:$FF4F;Value:$FF2F),(Count:$FF50;Value:$FF30),(Count:$FF51;Value:$FF31),(Count:$FF52;Value:$FF32),(Count:$FF53;Value:$FF33),(Count:$FF54;Value:$FF34),
  (Count:$FF55;Value:$FF35),(Count:$FF56;Value:$FF36),(Count:$FF57;Value:$FF37),(Count:$FF58;Value:$FF38),(Count:$FF59;Value:$FF39),(Count:$FF5A;Value:$FF3A));

 {NTFS Sector Counts - Used for NTFS formatting (Fixed Disk only)}
 ntfs12MaxSectorCount = 4;
 ntfs12SectorCounts:array[0..ntfs12MaxSectorCount] of TNTFSSectorCount = (
  {SectorCount,SectorsPerCluster}
  (SectorCount:16384;SectorsPerCluster:0),      {up to 8 MB, the 0 value for SectorsPerCluster trips an error}
  (SectorCount:1048576;SectorsPerCluster:1),    {up to 512 MB, sector size cluster}
  (SectorCount:2097152;SectorsPerCluster:2),    {up to 1 GB, 1k cluster}
  (SectorCount:4194304;SectorsPerCluster:4),    {up to 2 GB, 2k cluster}
  (SectorCount:$FFFFFFFF;SectorsPerCluster:8)); {greater than 2 GB, 4k cluster}

 ntfs30MaxSectorCount = 4;
 ntfs30SectorCounts:array[0..ntfs30MaxSectorCount] of TNTFSSectorCount = (
  {SectorCount,SectorsPerCluster}
  (SectorCount:16384;SectorsPerCluster:0),      {up to 8 MB, the 0 value for SectorsPerCluster trips an error}
  (SectorCount:1048576;SectorsPerCluster:1),    {up to 512 MB, sector size cluster}
  (SectorCount:2097152;SectorsPerCluster:2),    {up to 1 GB, 1k cluster}
  (SectorCount:4194304;SectorsPerCluster:4),    {up to 2 GB, 2k cluster}
  (SectorCount:$FFFFFFFF;SectorsPerCluster:8)); {greater than 2 GB, 4k cluster}

{==============================================================================}
var
 {NTFS specific variables}
 SecurityHashPadding:LongWord = ntfsSecurityHashPadding;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}

end.
