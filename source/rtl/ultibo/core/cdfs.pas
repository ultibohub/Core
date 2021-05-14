{
Ultibo CDFS interface unit.

Copyright (C) 2020 - SoftOz Pty Ltd.

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

 
CD FileSystem
=============


}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit CDFS;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,FileSystem,SysUtils,Classes,Unicode,Ultibo,UltiboUtils,UltiboClasses;

//To Do //How to protect AllocCluster/ReleaseCluster ? (ClusterLock ? / FBlocks !)

//To Do //Use of FClusterBuffer

//To Do //Change ReadEntry to use FReadBuffer and ReadLock/Unlock ? - Done
//To Do //Change WriteEntry to use FWriteBuffer and ReadLock/Unlock ? - Done

//To Do //Change Descriptor functions to use FDescriptorBuffer and DescriptorLock/Unlock ?
//To Do //Change Entry functions to use FEntryBuffer and EntryLock/Unlock ?
                      //Who else uses the ClusterBuffer ?

//To Do //Locks around FHeaders, FExtensions (partially tied in with FCatalogs) (LoadCatalogs/SetCatalogs etc ?)

//To Do //Look for:

//Int64(Pointer()^) -> PInt64()^
//LongWord(Pointer()^) -> PLongWord()^
//Word(Pointer()^) -> PWord()^
//Byte(Pointer()^) -> PByte()^

//Critical

//Int64

//Lock

//) = Uppercase(  //Use WorkBuffer

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {CDFS specific constants}
 cdfsIso9660 = 0;
 cdfsJoilet = 1;
 cdfsSierra = 2;
 cdfsRockRidge = 3;
 cdfsAudio = 4;
 cdfsUDF = 5;

 cdfsNames:array[0..5] of String = (
  'CD001',
  'CD001',
  'CDROM',
  '',
  'CDAUDIO',
  '');

 cdfsAnyName        = '*';
 cdfsDotName        = '.';
 cdfsDotDotName     = '..';
 cdfsBlankName      = '';
 cdfsVersionName    = '1';  {Used by version information on end of identifier}  {Not Used}
 cdfsSeparator1Name = '.';  {Dot on end of identifier when no extension exists} {File only}
 cdfsSeparator2Name = ';';  {Used by version information on end of identifier}  {File only}

 cdfsJolietMaxPath = 240; {120} {240 bytes}
 cdfsJolietMaxFile = 64;  {128 bytes}
 cdfsISO9660MaxPath = 240;
 cdfsISO9660MaxFile = 30;  {31 for Directories}

 cdfsISO9660StandardIdentifier = 'CD001';
 cdfsELTORITOSystemIdentifier  = 'EL TORITO SPECIFICATION';

 cdfsFirstSector = 0;
 cdfsReservedSectors = 16;
 cdfsISO9660StartSector = 16;
 cdfsELTORITOBootSector = 17;

 cdfsISO9660StructureVersion = 1;
 cdfsISO9660DescriptorVersion = 1;
 cdfsELTORITODescriptorVersion = 1;

 cdfsJolietUCS2Sequence1 = #37 + #47 + #64;  {'%/@'}
 cdfsJolietUCS2Sequence2 = #37 + #47 + #67;  {'%/C'}
 cdfsJolietUCS2Sequence3 = #37 + #47 + #69;  {'%/E'}

 cdfsDotIdentifier    = $00;
 cdfsDotDotIdentifier = $01;

 cdfsSeparator1Value = $2E; {'.'}
 cdfsSeparator2Value = $3B; {';'}

 cdfsTimeStartYear = 1900;     {CDFS Time starts at 1900}
 cdfsTimeOffsetInterval = 15;  {Offset is in 15 minute intervals}

 cdfsNullFileTime:TFileTime = (dwLowDateTime:$00000000;dwHighDateTime:$00000000);

 cdfsStartCluster = 16;
 cdfsUnknownCluster = LongWord(-1);

 cdfsRootPathNumber = 1;

 cdfsPathRecordSize      = 9;   {Minimum}
 cdfsExtendedRecordSize  = 250; {Minimum}
 cdfsDirectoryRecordSize = 34;  {Minimum}

 cdfsFileIdentifierSize  = 1;   {Minimum}
 cdfsPathIdentifierSize  = 1;   {Minimum}

 cdfsHeaderRecordSize = 32;     {Fixed}
 cdfsCatalogRecordSize = 32;    {Fixed}
 cdfsExtensionRecordSize = 32;  {Fixed}

 cdfsEvenSize = 0;
 cdfsUnevenSize = 1;

 cdfsRootRecordOffset = 156; {Offset of Root Directory in Descriptor}

 cdfsInstanceFirst = 0;
 cdfsInstanceLast  = Byte(-1);

 cdfsTableTypeMask        = $00FF0000;
 cdfsTableTypeShift       = 16;
 cdfsTableInstanceMask    = $0000FF00;
 cdfsTableInstanceShift   = 8;
 cdfsTableDescriptorMask  = $000000FF;
 cdfsTableDescriptorShift = 0;

 cdfsBitmapUnknown    = LongWord(-1);

 {Bitmap Masks}
 cdfsBitmapMaskBits = 64;                {Int64 Bitmap Masks}
 cdfsBitmapMaskNone = $0000000000000000; {Int64} {Used for fast counting of free blocks}
 cdfsBitmapMaskAll  = $FFFFFFFFFFFFFFFF; {Int64} {Used for fast counting of used blocks}

 cdfsBitmapMasks:array[0..63] of Int64 = (
  $0000000000000001,$0000000000000002,$0000000000000004,$0000000000000008,$0000000000000010,$0000000000000020,$0000000000000040,$0000000000000080,
  $0000000000000100,$0000000000000200,$0000000000000400,$0000000000000800,$0000000000001000,$0000000000002000,$0000000000004000,$0000000000008000,
  $0000000000010000,$0000000000020000,$0000000000040000,$0000000000080000,$0000000000100000,$0000000000200000,$0000000000400000,$0000000000800000,
  $0000000001000000,$0000000002000000,$0000000004000000,$0000000008000000,$0000000010000000,$0000000020000000,$0000000040000000,$0000000080000000,
  $0000000100000000,$0000000200000000,$0000000400000000,$0000000800000000,$0000001000000000,$0000002000000000,$0000004000000000,$0000008000000000,
  $0000010000000000,$0000020000000000,$0000040000000000,$0000080000000000,$0000100000000000,$0000200000000000,$0000400000000000,$0000800000000000,
  $0001000000000000,$0002000000000000,$0004000000000000,$0008000000000000,$0010000000000000,$0020000000000000,$0040000000000000,$0080000000000000,
  $0100000000000000,$0200000000000000,$0400000000000000,$0800000000000000,$1000000000000000,$2000000000000000,$4000000000000000,$8000000000000000);

 //To Do //Should this be the opposite way around ? See NTFS //probably does not matter since it is not on disk
 cdfsBitmapOverlays:array[0..63] of Int64 = (
  $8000000000000000,$C000000000000000,$E000000000000000,$F000000000000000,$F800000000000000,$FC00000000000000,$FE00000000000000,$FF00000000000000,
  $FF80000000000000,$FFC0000000000000,$FFE0000000000000,$FFF0000000000000,$FFF8000000000000,$FFFC000000000000,$FFFE000000000000,$FFFF000000000000,
  $FFFF800000000000,$FFFFC00000000000,$FFFFE00000000000,$FFFFF00000000000,$FFFFF80000000000,$FFFFFC0000000000,$FFFFFE0000000000,$FFFFFF0000000000,
  $FFFFFF8000000000,$FFFFFFC000000000,$FFFFFFE000000000,$FFFFFFF000000000,$FFFFFFF800000000,$FFFFFFFC00000000,$FFFFFFFE00000000,$FFFFFFFF00000000,
  $FFFFFFFF80000000,$FFFFFFFFC0000000,$FFFFFFFFE0000000,$FFFFFFFFF0000000,$FFFFFFFFF8000000,$FFFFFFFFFC000000,$FFFFFFFFFE000000,$FFFFFFFFFF000000,
  $FFFFFFFFFF800000,$FFFFFFFFFFC00000,$FFFFFFFFFFE00000,$FFFFFFFFFFF00000,$FFFFFFFFFFF80000,$FFFFFFFFFFFC0000,$FFFFFFFFFFFE0000,$FFFFFFFFFFFF0000,
  $FFFFFFFFFFFF8000,$FFFFFFFFFFFFC000,$FFFFFFFFFFFFE000,$FFFFFFFFFFFFF000,$FFFFFFFFFFFFF800,$FFFFFFFFFFFFFC00,$FFFFFFFFFFFFFE00,$FFFFFFFFFFFFFF00,
  $FFFFFFFFFFFFFF80,$FFFFFFFFFFFFFFC0,$FFFFFFFFFFFFFFE0,$FFFFFFFFFFFFFFF0,$FFFFFFFFFFFFFFF8,$FFFFFFFFFFFFFFFC,$FFFFFFFFFFFFFFFE,$FFFFFFFFFFFFFFFF);

 {Volume Flag Constants}
 cdfsVolumeFlagNone         = $00;
 cdfsVolumeFlagUnregistered = $01; {If set this SVD contains an unregistered escape sequence}

 {File Flag Constants}
 cdfsFileFlagNone       = $00;
 cdfsFileFlagExistence  = $01;  {If set this file is hidden}
 cdfsFileFlagDirectory  = $02;  {If set this record is a directory}
 cdfsFileFlagAssociated = $04;  {If set this file is an associated file}
 cdfsFileFlagRecord     = $08;  {If set record information is recorded for this file}
 cdfsFileFlagProtection = $10;  {If set permissions are recorded for this file}
 cdfsFileFlagReserved1  = $20;  {Reserved}
 cdfsFileFlagReserved2  = $40;  {Reserved}
 cdfsFileFlagMore       = $80;  {If set there are more directory records for this file}

 {Permissions Constants}
 cdfsPermissionSystemRead    = $0001;  {If set system cannot Read}
 cdfsPermissionSystemWrite   = $0002;  {If set system cannot Write}
 cdfsPermissionSystemExecute = $0004;  {If set system cannot Execute}
 cdfsPermissionReserved1     = $0008;  {Reserved}
 cdfsPermissionOwnerRead     = $0010;  {If set owner cannot Read}
 cdfsPermissionOwnerWrite    = $0020;  {If set owner cannot Write}
 cdfsPermissionOwnerExecute  = $0040;  {If set owner cannot Execute}
 cdfsPermissionReserved2     = $0080;  {Reserved}
 cdfsPermissionGroupRead     = $0100;  {If set group cannot Read}
 cdfsPermissionGroupWrite    = $0200;  {If set group cannot Write}
 cdfsPermissionGroupExecute  = $0400;  {If set group cannot Execute}
 cdfsPermissionReserved3     = $0800;  {Reserved}
 cdfsPermissionOtherRead     = $1000;  {If set other cannot Read}
 cdfsPermissionOtherWrite    = $2000;  {If set other cannot Write}
 cdfsPermissionOtherExecute  = $4000;  {If set other cannot Execute}
 cdfsPermissionReserved4     = $8000;  {Reserved}

 {Path Table Constants}
 cdfsPathTableTypePrimary    = $00;
 cdfsPathTableTypeAlternate  = $01;
 cdfsPathTableTypePrimaryM   = $02;
 cdfsPathTableTypeAlternateM = $03;

 {Volume Descriptor Constants}
 cdfsVolumeDescriptorTypeBoot          = $00;
 cdfsVolumeDescriptorTypePrimary       = $01;
 cdfsVolumeDescriptorTypeSupplementary = $02;
 cdfsVolumeDescriptorTypePartition     = $03;
 cdfsVolumeDescriptorTypeTerminator    = $FF;

 {Header Constants}
 cdfsElToritoHeaderId = $01;
 cdfsElToritoBootIndicator = $88;
 cdfsElToritoNoBootIndicator = $00;
 cdfsElToritoHeaderIndicator = $90; {91 if final header}
 cdfsElToritoHeaderTerminator = $91;
 cdfsElToritoExtensionIndicator = $44;

 {Signature Constants}
 cdfsElToritoSignature = $AA55;

 {Platform Id Constants}
 cdfsElToritoPlatformIdx86  = $00;
 cdfsElToritoPlatformIdPPC  = $01;
 cdfsElToritoPlatformIdMac  = $02;

 {Media Type Constants}
 cdfsElToritoMediaTypeNone = $00; {No Emulation}
 cdfsElToritoMediaType12M  = $01; {1.2 meg diskette}
 cdfsElToritoMediaType144M = $02; {1.44 meg diskette}
 cdfsElToritoMediaType288M = $03; {2.88 meg diskette}
 cdfsElToritoMediaTypeHDD  = $04; {Hard Disk (drive 80)}
 cdfsElToritoMediaTypeMask = $0F; {Values 5-F Reserved, invalid at this time}

 cdfsElToritoMediaFlagReserved     = $10; {Reserved, must be 0}
 cdfsElToritoMediaFlagContinuation = $20; {Continuation Entry Follows}
 cdfsElToritoMediaFlagAtapi        = $40; {Image contains ATAPI driver}
 cdfsElToritoMediaFlagScsi         = $80; {Image contains SCSI drivers}
 cdfsElToritoMediaFlagMask         = $F0;

 {Extension Flag Constants}
 cdfsElToritoExtensionFlagExtension = $20; {Extension Record Follows}
              
{==============================================================================}
type
 {CDFS specific types}
 TCDFSType = (ctNONE,ctISO9660,ctJOLIET,ctSIERRA,ctROCKRIDGE,ctAUDIO,ctUDF);

 PCDFSTime = ^TCDFSTime;    {7 Bytes}
 TCDFSTime = packed record
  Years:Byte;      {Number of years since 1900}
  Month:Byte;      {Month of the year from 1 to 12}
  Day:Byte;        {Day of the month from 1 to 31}
  Hour:Byte;       {Hour of the day from 0 to 23}
  Minute:Byte;     {Minute of the hour from 0 to 59}
  Second:Byte;     {Second of the minute from 0 to 59}
  Offset:ShortInt; {Offset from Greenwich Mean Time in number of 15 min intervals from -48 (West) to +52 (East)}
 end;

 PCDFSDateTime = ^TCDFSDateTime; {17 Bytes}
 TCDFSDateTime = packed record
  Year:array[0..3] of Char;        {Year from 1 to 9999}
  Month:array[0..1] of Char;       {Month of the year from 1 to 12}
  Day:array[0..1] of Char;         {Day of the month from 1 to 31}
  Hour:array[0..1] of Char;        {Hour of the day from 0 to 23}
  Minute:array[0..1] of Char;      {Minute of the hour from 0 to 59}
  Second:array[0..1] of Char;      {Second of the minute from 0 to 59}
  Hundredths:array[0..1] of Char;  {Hundredths of a second}
  Offset:ShortInt;                 {Offset from Greenwich Mean Time in number of 15 min intervals from -48 (West) to +52 (East)}
 end;

 PCDFSDirectoryRecord = ^TCDFSDirectoryRecord; {34 Bytes (Minimum not including padding)}
 TCDFSDirectoryRecord = packed record
  RecordSize:Byte;                    {Length of Directory Record (LEN-DR)}
  ExtendedSize:Byte;                  {Extended Attribute Record Length}
  FirstBlock:LongWord;                {Location of Extent}
  FirstBlockM:LongWord;
  DataSize:LongWord;                  {Data Length}
  DataSizeM:LongWord;
  CreateTime:TCDFSTime;               {Recording Date and Time}
  FileFlags:Byte;                     {File Flags 8 bits}
  UnitSize:Byte;                      {File Unit Size}
  InterleaveSize:Byte;                {Interleave Gap Size}
  SequenceNumber:Word;                {Volume Sequence Number}
  SequenceNumberM:Word;
  FileIdentifierSize:Byte;            {Length of File Identifier (LEN_FI)}
  FileIdentifier:array[0..0] of Char; {File Identifier d-characters,d1-characters,SEPARATOR 1,SEPARATOR 2,(00) or (01) byte}
  {Padding:Byte;}                     {Padding Field (00) byte} {Only present if FileIdentifierSize is even (Total size would be odd)}
  {SystemData:array[0..0] of Byte;}   {System Use LEN_SU bytes}
 end;

 PCDFSPathRecord = ^TCDFSPathRecord; {9 Bytes (Minimum not including padding)}
 TCDFSPathRecord = packed record
  PathIdentifierSize:Byte;                  {Length of Directory Identifier (LEN_DI)}
  ExtendedSize:Byte;                        {Extended Attribute Record Length}
  FirstBlock:LongWord;                      {Location of Extent}
  ParentNumber:Word;                        {Parent Directory Number}
  PathIdentifier:array[0..0] of Char;       {Directory Identifier d-characters, d1-characters, (00) byte}
  {Padding:Byte;}                           {Padding Field (00) byte} {Only present if PathIdentifierSize is odd (Total size would be odd)}
 end;

 PCDFSExtendedRecord = ^TCDFSExtendedRecord; {250 Bytes (Minimum)}
 TCDFSExtendedRecord = packed record
  OwnerId:Word;                          {Owner Identification}
  OwnerIdM:Word;
  GroupId:Word;                          {Group Identification}
  GroupIdM:Word;
  Permissions:Word;                      {Permissions 16 bits}
  CreateTime:TCDFSDateTime;              {File Creation Date and Time}
  ModifyTime:TCDFSDateTime;              {File Modification Date and Time}
  ExpireTime:TCDFSDateTime;              {File Expiration Date and Time}
  EffectiveTime:TCDFSDateTime;           {File Effective Date and Time}
  RecordFormat:Byte;                     {Record Format}
  RecordAttributes:Byte;                 {Record Attributes}
  RecordLength:Word;                     {Record Length}
  RecordLengthM:Word;
  SystemIdentifier:array[0..31] of Char; {System Identifier a-characters, a1-characters}
  SystemData:array[0..63] of Byte;       {System Use not specified}
  ExtendedVersion:Byte;                  {Extended Attribute Record Version}
  EscapeSequenceSize:Byte;               {Length of Escape Sequences (LEN_ESC)}
  Reserved1:array[0..63] of Byte;        {Reserved for future standardization (00) bytes}
  ApplicationDataSize:Word;              {Length of Application Use (LEN_AU)}
  ApplicationDataSizeM:Word;
  {ApplicationData:array[0..0] of Byte;} {Application Use LEN_AU bytes}
  {EscapeSequence:array[0..0] of Byte;} {Escape Sequences LEN_ESC bytes}
 end;

 PCDFSVolumeDescriptorHeader = ^TCDFSVolumeDescriptorHeader; {2048 Bytes}
 TCDFSVolumeDescriptorHeader = packed record
  DescriptorType:Byte;                     {Descriptor Type 00 = Boot, 01 = Primary etc}
  StandardIdentifier:array[0..4] of Char;  {Standard Identifier ('CD001')}
  DescriptorVersion:Byte;                  {Descriptor Version (1)}
  Reserved:array[0..2040] of Byte;         {Content depends on Descriptor Type}
 end;

 PCDFSVolumeDescriptorBoot = ^TCDFSVolumeDescriptorBoot; {2048 Bytes}
 TCDFSVolumeDescriptorBoot = packed record
  DescriptorType:Byte;                     {Descriptor Type (00 = Boot)}
  StandardIdentifier:array[0..4] of Char;  {Standard Identifier ('CD001')}
  DescriptorVersion:Byte;                  {Descriptor Version (1)}
  SystemIdentifier:array[0..31] of Char;   {32 a-characters (eg EL TORITO SPECIFICATION)}
  BootIdentifier:array[0..31] of Char;     {32 a-characters}
  BootData:array[0..1976] of Byte;
 end;

 PCDFSVolumeDescriptorPrimary = ^TCDFSVolumeDescriptorPrimary; {2048 Bytes}
 TCDFSVolumeDescriptorPrimary = packed record
  DescriptorType:Byte;                          {Descriptor Type (01 = Primary)}
  StandardIdentifier:array[0..4] of Char;       {Standard Identifier ('CD001')}
  DescriptorVersion:Byte;                       {Descriptor Version (1)}
  Reserved1:Byte;                               {Unused Field (00) byte}
  SystemIdentifier:array[0..31] of Char;        {System Identifier a-characters}
  VolumeIdentifier:array[0..31] of Char;        {Volume Identifier d-characters}
  Reserved2:array[0..7] of Byte;                {Unused Field (00) bytes}
  VolumeSpaceSize:LongWord;                     {Volume Space Size}
  VolumeSpaceSizeM:LongWord;
  Reserved3:array[0..31] of Byte;               {Unused Field (00) bytes}
  VolumeSetSize:Word;                           {Volume Set Size}
  VolumeSetSizeM:Word;
  VolumeSequenceNumber:Word;                    {Volume Sequence Number}
  VolumeSequenceNumberM:Word;
  LogicalBlockSize:Word;                        {Logical Block Size}
  LogicalBlockSizeM:Word;
  PathTableSize:LongWord;                       {Path Table Size}
  PathTableSizeM:LongWord;
  PrimaryPathTable:LongWord;                    {Location of Occurrence of Type L Path Table}
  AlternatePathTable:LongWord;                  {Location of Optional Occurrence of Type L Path Table}
  PrimaryPathTableM:LongWord;                   {Location of Occurrence of Type M Path Table}
  AlternatePathTableM:LongWord;                 {Location of Optional Occurrence of Type M Path Table}
  RootDirectory:TCDFSDirectoryRecord;           {Directory Record for Root Directory 34 bytes}
  VolumeSetIdentifier:array[0..127] of Char;    {Volume Set Identifier d-characters}
  PublisherIdentifier:array[0..127] of Char;    {Publisher Identifier a-characters}
  PreparerIdentifier:array[0..127] of Char;     {Data Preparer Identifier a-characters}
  ApplicationIdentifier:array[0..127] of Char;  {Application Identifier a-characters}
  CopyrightIdentifier:array[0..36] of Char;     {Copyright File Identifier d-characters, SEPARATOR 1, SEPARATOR 2}
  AbstractIdentifier:array[0..36] of Char;      {Abstract File Identifier d-characters, SEPARATOR 1, SEPARATOR 2}
  BibliographicIdentifier:array[0..36] of Char; {Bibliographic File Identifier d-characters, SEPARATOR 1, SEPARATOR 2}
  CreateTime:TCDFSDateTime;                     {Volume Creation Date and Time}
  ModifyTime:TCDFSDateTime;                     {Volume Modification Date and Time}
  ExpireTime:TCDFSDateTime;                     {Volume Expiration Date and Time}
  EffectiveTime:TCDFSDateTime;                  {Volume Effective Date and Time}
  FileStructureVersion:Byte;                    {File Structure Version}
  Reserved4:Byte;                               {Reserved for future standardization (00) byte}
  ApplicationData:array[0..511] of Byte;        {Application Use not specified}
  Reserved5:array[0..652] of Byte;              {Reserved for future standardization (00) bytes}
 end;

 PCDFSVolumeDescriptorSupplementary = ^TCDFSVolumeDescriptorSupplementary; {2048 Bytes}
 TCDFSVolumeDescriptorSupplementary = packed record
  DescriptorType:Byte;                          {Descriptor Type (02 = Supplementary)}
  StandardIdentifier:array[0..4] of Char;       {Standard Identifier ('CD001')}
  DescriptorVersion:Byte;                       {Descriptor Version (1)}
  VolumeFlags:Byte;                             {Volume Flags 8 bits}
  SystemIdentifier:array[0..31] of Char;        {System Identifier a-characters}
  VolumeIdentifier:array[0..31] of Char;        {Volume Identifier d-characters}
  Reserved2:array[0..7] of Byte;                {Unused Field (00) bytes}
  VolumeSpaceSize:LongWord;                     {Volume Space Size}
  VolumeSpaceSizeM:LongWord;
  EscapeSequences:array[0..31] of Byte;         {Escape Sequences 32 bytes}
  VolumeSetSize:Word;                           {Volume Set Size}
  VolumeSetSizeM:Word;
  VolumeSequenceNumber:Word;                    {Volume Sequence Number}
  VolumeSequenceNumberM:Word;
  LogicalBlockSize:Word;                        {Logical Block Size}
  LogicalBlockSizeM:Word;
  PathTableSize:LongWord;                       {Path Table Size}
  PathTableSizeM:LongWord;
  PrimaryPathTable:LongWord;                    {Location of Occurrence of Type L Path Table}
  AlternatePathTable:LongWord;                  {Location of Optional Occurrence of Type L Path Table}
  PrimaryPathTableM:LongWord;                   {Location of Occurrence of Type M Path Table}
  AlternatePathTableM:LongWord;                 {Location of Optional Occurrence of Type M Path Table}
  RootDirectory:TCDFSDirectoryRecord;           {Directory Record for Root Directory 34 bytes}
  VolumeSetIdentifier:array[0..127] of Char;    {Volume Set Identifier d-characters}
  PublisherIdentifier:array[0..127] of Char;    {Publisher Identifier a-characters}
  PreparerIdentifier:array[0..127] of Char;     {Data Preparer Identifier a-characters}
  ApplicationIdentifier:array[0..127] of Char;  {Application Identifier a-characters}
  CopyrightIdentifier:array[0..36] of Char;     {Copyright File Identifier d-characters, SEPARATOR 1, SEPARATOR 2}
  AbstractIdentifier:array[0..36] of Char;      {Abstract File Identifier d-characters, SEPARATOR 1, SEPARATOR 2}
  BibliographicIdentifier:array[0..36] of Char; {Bibliographic File Identifier d-characters, SEPARATOR 1, SEPARATOR 2}
  CreateTime:TCDFSDateTime;                     {Volume Creation Date and Time}
  ModifyTime:TCDFSDateTime;                     {Volume Modification Date and Time}
  ExpireTime:TCDFSDateTime;                     {Volume Expiration Date and Time}
  EffectiveTime:TCDFSDateTime;                  {Volume Effective Date and Time}
  FileStructureVersion:Byte;                    {File Structure Version}
  Reserved4:Byte;                               {Reserved for future standardization (00) byte}
  ApplicationData:array[0..511] of Byte;        {Application Use not specified}
  Reserved5:array[0..652] of Byte;              {Reserved for future standardization (00) bytes}
 end;

 PCDFSVolumeDescriptorPartition = ^TCDFSVolumeDescriptorPartition; {2048 Bytes}
 TCDFSVolumeDescriptorPartition = packed record
  DescriptorType:Byte;                      {Descriptor Type (03 = Partition)}
  StandardIdentifier:array[0..4] of Char;   {Standard Identifier ('CD001')}
  DescriptorVersion:Byte;                   {Descriptor Version (1)}
  Reserved1:Byte;                           {Unused Field (00) byte}
  SystemIdentifier:array[0..31] of Char;    {System Identifier a-characters}
  PartitionIdentifier:array[0..31] of Char; {Volume Partition Identifier d-characters}
  PartitionStart:LongWord;                  {Volume Partition Location (first Logical Block)}
  PartitionStartM:LongWord;
  PartitionSize:LongWord;                   {Volume Partition Size (in Logical Blocks)}
  PartitionSizeM:LongWord;
  SystemData:array[0..1959] of Byte;        {System Use not specified}
 end;

 PCDFSVolumeDescriptorTerminator = ^TCDFSVolumeDescriptorTerminator; {2048 Bytes}
 TCDFSVolumeDescriptorTerminator = packed record
  DescriptorType:Byte;                     {Descriptor Type (FF = Terminator)}
  StandardIdentifier:array[0..4] of Char;  {Standard Identifier ('CD001')}
  DescriptorVersion:Byte;                  {Descriptor Version (1)}
  Reserved:array[0..2040] of Byte;         {Reserved (Always 00)}
 end;

 PELTORITOValidationRecord = ^TELTORITOValidationRecord; {32 Bytes}
 TELTORITOValidationRecord = packed record
  HeaderId:Byte;                 {Header ID, must be 01}
  PlatformId:Byte;               {Platform ID 0 = 80x86, 1 = PowerPC, 2 = Mac}
  Reserved1:Word;                {Reserved, must be 0}
  VendorId:array[0..23] of Char; {ID string. This is intended to identify the manufacturer/developer of the CD-ROM}
  Checksum:Word;                 {Checksum Word. This sum of all the words in this record should be 0}
  Signature:Word;                {Key byte, must be 55. Key byte, must be AA. This value is included in the checksum}
 end;

 PELTORITODefaultRecord = ^TELTORITODefaultRecord; {32 Bytes}
 TELTORITODefaultRecord = packed record
  BootIndicator:Byte;             {Boot Indicator. 88 = Bootable, 00 = Not Bootable}
  BootMedia:Byte;                 {Boot media type. This specifies what media the boot image is intended to emulate}
  LoadSegment:Word;               {This is the load segment for the initial boot image. If this value is 0 the system will use the traditional segment of 7C0}
  SystemType:Byte;                {This must be a copy of byte 5 (System Type) from the Partition Table found in the boot image} {PartitionId}
  Reserved1:Byte;                 {Unused, must be 0}
  LoadCount:Word;                 {Sector Count. This is the number of virtual/emulated sectors the system will store at Load Segment during the initial boot procedure}
  LoadRBA:LongWord;               {Load RBA. This is the start address of the virtual disk. CD’s use Relative/Logical block addressing}
  Reserved2:array[0..19] of Byte; {Unused, must be 0}
 end;

 PELTORITOSectionHeader = ^TELTORITOSectionHeader; {32 Bytes}
 TELTORITOSectionHeader = packed record
  HeaderIndicator:Byte;           {Header Indicator, 90 - Header, more headers follow, 91 - Final Header}
  PlatformId:Byte;                {Platform ID 0 = 80x86, 1 = PowerPC, 2 = Mac}
  SectionCount:Word;              {Number of section entries following this header}
  SectionId:array[0..27] of Char; {ID string. This identifies a section}
 end;

 PELTORITOSectionRecord = ^TELTORITOSectionRecord; {32 Bytes}
 TELTORITOSectionRecord = packed record
  BootIndicator:Byte;                     {Boot Indicator. 88 = Bootable, 00 = Not Bootable}
  BootMedia:Byte;                         {Boot media type. This specifies what media the boot image emulates}
  LoadSegment:Word;                       {Load Segment. This is the load segment for the initial boot image. If this value is 0 the system will use the traditional segment of 7C0}
  SystemType:Byte;                        {System Type. This must be a copy of byte 5 (System Type) from the Partition Table found in the boot image} {PartitionId}
  Reserved1:Byte;                         {Unused, must be 0}
  LoadCount:Word;                         {Sector Count. This is the number of virtual/emulated sectors the system will store at Load Segment during the initial boot procedure}
  LoadRBA:LongWord;                       {Load RBA. This is the start address of the virtual disk. CD’s use Relative/Logical block addressing}
  SelectionType:Byte;                     {Selection criteria type. 0 - No selection criteria, 1- Language and Version Information (IBM), 2-FF - Reserved}
  SelectionData:array[0..18] of Byte;     {Vendor unique selection criteria}
 end;

 PELTORITOSectionExtension = ^TELTORITOSectionExtension; {32 Bytes}
 TELTORITOSectionExtension = packed record
  ExtensionIndicator:Byte;                {Extension Indicator. Must be 44}
  ExtensionFlag:Byte;                     {Bits 1-4 - Unused, Bit 5 (1 = Extension Record follows, 0 = This is final Extension), Bits 6-7 - Unused}
  SelectionData:array[0..29] of Byte;     {Vendor unique selection criteria}
 end;

 PELTORITOVolumeDescriptorBoot = ^TELTORITOVolumeDescriptorBoot; {2048 Bytes}
 TELTORITOVolumeDescriptorBoot = packed record
  DescriptorType:Byte;                    {Boot Record Indicator, must be 0}
  StandardIdentifier:array[0..4] of Char; {ISO-9660 Identifier, must be 'CD001'}
  DescriptorVersion:Byte;                 {Version of this descriptor, must be 1}
  SystemIdentifier:array[0..31] of Char;  {Boot System Identifier, must be 'EL TORITO SPECIFICATION' padded with 0’s}
  Reserved1:array[0..31] of Byte;         {Unused, must be 0}
  CatalogStart:LongWord;                  {Absolute pointer to first sector of Boot Catalog}
  Reserved2:array[0..1972] of Byte;       {Unused, must be 0}
 end;

{==============================================================================}
type
 {CDFS specific classes}
 TCDFSRecognizer = class(TRecognizer)
   constructor Create(ADriver:TFileSysDriver);
  private
   {Private Variables}
   FLongNames:Boolean;
   FSwapSerial:Boolean;

   {Private Methods}
   function CheckPrimaryDescriptor(ASector:PDiskSector;const AStartSector:Int64;ASectorCount:LongWord):Boolean;
  protected
   {Protected Variables}

   {Protected Methods}
   function GetName:String; override;
  public
   {Public Variables}
   property LongNames:Boolean read FLongNames write FLongNames;
   property SwapSerial:Boolean read FSwapSerial write FSwapSerial;

   {Public Methods}
   function RecognizePartitionId(APartitionId:Byte):Boolean; override;
   function RecognizeBootSector(ABootSector:PBootSector;const AStartSector,ASectorCount:Int64):Boolean; override;
   
   function RecognizePartition(APartition:TDiskPartition):Boolean; override;
   function RecognizeVolume(AVolume:TDiskVolume):Boolean; override;
   function MountVolume(AVolume:TDiskVolume;ADrive:TDiskDrive):Boolean; override;
 end;

 TCDFSFormatter = class(TDiskFormatter)
  private
   {Private Variables}

   {Private Methods}
   function CheckDevice(AVolume:TDiskVolume;ADrive:TDiskDrive;AFloppyType:TFloppyType):Boolean;
   function CheckPartition(AVolume:TDiskVolume;ADrive:TDiskDrive;AFileSysType:TFileSysType):Boolean;

   function GetPathTableSize(AVolume:TDiskVolume;ADrive:TDiskDrive;ACDFSType:TCDFSType;ASupplementary:Boolean):LongWord;
   function GetPathTableStart(AVolume:TDiskVolume;ADrive:TDiskDrive;ACDFSType:TCDFSType;ASupplementary,AEndian:Boolean):LongWord;

   function GetRootDirectorySize(AVolume:TDiskVolume;ADrive:TDiskDrive;ACDFSType:TCDFSType;ASupplementary:Boolean):LongWord;
   function GetRootDirectoryStart(AVolume:TDiskVolume;ADrive:TDiskDrive;ACDFSType:TCDFSType;ASupplementary:Boolean):LongWord;

   function CreatePrimaryDescriptor(AVolume:TDiskVolume;ADrive:TDiskDrive;ACDFSType:TCDFSType;ADescriptor:PCDFSVolumeDescriptorPrimary):Boolean;
   function WritePrimaryDescriptor(AVolume:TDiskVolume;ADrive:TDiskDrive;ACDFSType:TCDFSType;ADescriptor:PCDFSVolumeDescriptorPrimary):Boolean;
   function WritePrimaryPathTables(AVolume:TDiskVolume;ADrive:TDiskDrive;ACDFSType:TCDFSType;ADescriptor:PCDFSVolumeDescriptorPrimary):Boolean;
   function WritePrimaryRootDirectory(AVolume:TDiskVolume;ADrive:TDiskDrive;ACDFSType:TCDFSType;ADescriptor:PCDFSVolumeDescriptorPrimary):Boolean;

   function CreateSupplementaryDescriptor(AVolume:TDiskVolume;ADrive:TDiskDrive;ACDFSType:TCDFSType;ADescriptor:PCDFSVolumeDescriptorSupplementary):Boolean;
   function WriteSupplementaryDescriptor(AVolume:TDiskVolume;ADrive:TDiskDrive;ACDFSType:TCDFSType;ADescriptor:PCDFSVolumeDescriptorSupplementary):Boolean;
   function WriteSupplementaryPathTables(AVolume:TDiskVolume;ADrive:TDiskDrive;ACDFSType:TCDFSType;ADescriptor:PCDFSVolumeDescriptorSupplementary):Boolean;
   function WriteSupplementaryRootDirectory(AVolume:TDiskVolume;ADrive:TDiskDrive;ACDFSType:TCDFSType;ADescriptor:PCDFSVolumeDescriptorSupplementary):Boolean;

   function CreateTerminatorDescriptor(AVolume:TDiskVolume;ADrive:TDiskDrive;ACDFSType:TCDFSType;ADescriptor:PCDFSVolumeDescriptorTerminator):Boolean;
   function WriteTerminatorDescriptor(AVolume:TDiskVolume;ADrive:TDiskDrive;ACDFSType:TCDFSType;ADescriptor:PCDFSVolumeDescriptorTerminator):Boolean;
  public
   {Public Variables}

   {Public Methods}
   function AcceptVolume(AVolume:TDiskVolume;AFloppyType:TFloppyType;AFileSysType:TFileSysType):Boolean; override;
   function FormatVolume(AVolume:TDiskVolume;AFloppyType:TFloppyType;AFileSysType:TFileSysType):Boolean; override;
 end;
 
 TCDFSResizer = class(TDiskResizer)
  private
   {Private Variables}

   {Private Methods}

  public
   {Public Variables}

   {Public Methods}
   function AcceptVolume(AVolume:TDiskVolume;const AStart,ASize:Int64):Boolean; override;

   function ShrinkVolume(AVolume:TDiskVolume;const AStart,ASize:Int64):Boolean; override;
   function ExpandVolume(AVolume:TDiskVolume;const AStart,ASize:Int64):Boolean; override;
 end;
 
 TCDFSCopier = class(TDiskCopier)
  private
   {Private Variables}

   {Private Methods}

  public
   {Public Variables}

   {Public Methods}
   function AcceptVolume(AVolume,ADest:TDiskVolume):Boolean; override;

   function CopyVolume(AVolume,ADest:TDiskVolume):Boolean; override;
 end;
 
 {Inherited Classes}
 TCDFSDiskTable = class;
 TCDFSDiskBlock = class;
 TCDFSDiskEntry = class;
 TCDFSDiskCatalog = class;
 {Non Inherited Classes}
 TCDFSDiskPath = class;
 TCDFSDiskExtended = class;
 TCDFSDiskDescriptor = class;
 TCDFSDiskHeader = class;
 TCDFSDiskExtension = class;
 TCDFSFileSystem = class(TFileSystem)
   constructor Create(ADriver:TFileSysDriver;AVolume:TDiskVolume;ADrive:TDiskDrive);
   destructor Destroy; override;
  private
   {Private Variables}
   FCDFSType:TCDFSType;
   FSwapSerial:Boolean;
   FVolumeFlags:LongWord;        {Volume flags}

   FTreesLoaded:Boolean;
   FBlocksLoaded:Boolean;
   FCatalogsLoaded:Boolean;
   FCatalogsLoading:Boolean;

   FBlockWrite:Boolean;
   FTreesPrepared:Boolean;
   FCatalogsChecked:Boolean;
   
   {CDFS Variables}
   FLogicalBlockSize:Word;       {Size of a logical block on disk}
   FLogicalBlockCount:LongWord;  {Number of logical blocks on disk}

   FSectorsPerCluster:LongWord;  {Usually 1,2,4,8,16,32,64,128 etc}

   FDataStartCluster:LongWord;   {Usually 16 (First cluster of Data Area)}
   FBootStartCluster:LongWord;   {Usually 17 (Cluster of the Boot Descriptor)}
   FRootStartCluster:LongWord;   {Pointed to by Primary Descriptor}

   FBlockShiftCount:Word;        {Shift count for Cluster <-> BlockNo}
   FSectorShiftCount:Word;       {Shift count for Sector <-> Cluster}
   FClusterShiftCount:Word;      {Shift count for Cluster <-> Bytes}

   FEntriesPerBlock:LongWord;    {Number of Cluster entries per Block of bitmap}
   FClustersPerBlock:LongWord;   {Number of Clusters per Block of bitmap entries}
   FTotalBlockCount:LongWord;    {Total number of Blocks in bitmap}

   FTotalClusterCount:LongWord;  {Total number of Clusters on volume}

   FLastFreeCluster:LongWord;    {Or cdfsUnknownCluster if not known}
   FFreeClusterCount:LongWord;   {Or cdfsUnknownCluster if not known}

   FClusterSize:LongWord;        {Size of a Cluster in Bytes (Max 65536 > Word)}

   FBoot:TCDFSDiskDescriptor;
   FPrimary:TCDFSDiskDescriptor;
   FSupplementary:TCDFSDiskDescriptor;

   FHeaders:TFileSysList;        {List of Catalog Headers}
   FExtensions:TFileSysList;     {List of Catalog Extensions}
   FDescriptors:TFileSysList;    {List of Volume Descriptors}

   FHeaderLocal:TMutexHandle;    {Local Lock shared by all Catalog Headers}
   FExtensionLocal:TMutexHandle; {Local Lock shared by all Catalog Extensions}
   FDescriptorLocal:TMutexHandle;{Local Lock shared by all Volume Descriptors}

   FExtendedLocal:TMutexHandle;  {Local Lock shared by all Extended Attributes}  

   FPathLock:TSynchronizerHandle;{Lock shared by all Path lists}
   
   FReadBuffer:Pointer;          {Buffer for partial cluster entry reads (Cluster size)}
   FReadLock:TMutexHandle;       {Lock for read buffer}

   FWriteBuffer:Pointer;         {Buffer for partial cluster entry writes (Cluster size)}
   FWriteLock:TMutexHandle;      {Lock for write buffer}
   
   FClusterBuffer:Pointer;       {Buffer of exactly cluster size}
   FClusterLock:TMutexHandle;    {Lock for cluster buffer}
   
   {Private Methods}
   function ReadLock:Boolean;
   function ReadUnlock:Boolean;

   function WriteLock:Boolean;
   function WriteUnlock:Boolean;

   function ClusterLock:Boolean;
   function ClusterUnlock:Boolean;
   
   function LoadTree(AEntry:TDiskEntry):Boolean;
   function MarkTree(AEntry:TDiskEntry):Boolean;
   function PrepareTree(AEntry:TDiskEntry):Boolean;
   function PrepareTable(ATable:TDiskTable):Boolean;

   function PrepareTrees:Boolean;
   function CheckCatalogs:Boolean;

   function LocatePath(ATable:TCDFSDiskTable;APathNumber:Word):TCDFSDiskPath;
   function LocateEntry(AEntry:TCDFSDiskEntry;AStartCluster:LongWord):TCDFSDiskEntry;

   {Flag Methods}
   function LoadVolumeFlags:LongWord;

   {Cluster Methods}
   function FillClusters(ACluster:LongWord;ACount:Word;AValue:Byte):Boolean;

   function ReadClusters(ACluster:LongWord;ACount:Word;var ABuffer):Boolean;
   function WriteClusters(ACluster:LongWord;ACount:Word;const ABuffer):Boolean;

   function TestClusters(ACluster,ACount:LongWord):Boolean;
   function MarkClusters(ACluster,ACount:LongWord;AUsed:Boolean):Boolean;

   function AllocClusters(var ACluster:LongWord;ACount:LongWord):Boolean;
   function ReleaseClusters(ACluster,ACount:LongWord):Boolean;

   function GetNextFreeCluster:LongWord;       {Return is a Cluster}
   function GetFreeClusterCount:LongWord;

   {Block Methods}
   function TestBlock(ABlock:TCDFSDiskBlock;AStart,ACount:LongWord):Boolean;
   function MarkBlock(ABlock:TCDFSDiskBlock;AStart,ACount:LongWord;AUsed:Boolean):Boolean;

   function AllocBlock(ABlock:TCDFSDiskBlock;AStart,ACount:LongWord):Boolean;
   function ReleaseBlock(ABlock:TCDFSDiskBlock;AStart,ACount:LongWord):Boolean;

   function GetBlockNextFree(ABlock:TCDFSDiskBlock;AStart:LongWord):LongWord;
   function GetBlockFreeCount(ABlock:TCDFSDiskBlock):LongWord;

   {Bitmap Methods}
   function TestBitmap(ABuffer:Pointer;ASize,AStart,ACount:LongWord):Boolean;
   function MarkBitmap(ABuffer:Pointer;ASize,AStart,ACount:LongWord;AUsed:Boolean):Boolean;

   function AllocBitmap(ABuffer:Pointer;ASize:LongWord;AStart,ACount:LongWord):Boolean;
   function ReleaseBitmap(ABuffer:Pointer;ASize:LongWord;AStart,ACount:LongWord):Boolean;

   function GetBitmapNextFree(ABuffer:Pointer;ASize,AStart:LongWord):LongWord;
   function GetBitmapFreeCount(ABuffer:Pointer;ASize:LongWord):LongWord;

   {Misc Methods}
   function GetTableDataFree(ATable:TDiskTable):LongWord;
   function GetEntryDataFree(AEntry:TDiskEntry):LongWord;
   function GetCatalogDataFree(ADescriptor:TCDFSDiskDescriptor):LongWord;
   
   function GetPreviousPath(ATable:TCDFSDiskTable;APath:TCDFSDiskPath):TCDFSDiskPath;
   function GetPreviousEntry(AParent,AEntry:TCDFSDiskEntry):TCDFSDiskEntry;

   function GetSectorsPerCluster(AClusterSize:LongWord):LongWord;
   function GetBlockShiftCount(AClusterSize:LongWord):Word;
   function GetSectorShiftCount(AClusterSize:LongWord):Word;
   function GetClusterShiftCount(AClusterSize:LongWord):Word;

   function GetEntriesPerBlock(AClusterSize:LongWord):LongWord;
   function GetClustersPerBlock(AClusterSize:LongWord):LongWord;
   function GetTotalBlockCount(ATotalClusterCount:LongWord):LongWord;

   function RenumberPaths(ATable:TDiskTable):Boolean;

   {Sorting Methods}
   function ComparePath(APath1,APath2:TCDFSDiskPath;AUnicode:Boolean):Integer;
   function CompareEntry(AEntry1,AEntry2:TCDFSDiskEntry;AUnicode:Boolean):Integer;

   function PadString(const AString:String;ALength:Integer;APrefix,AUnicode:Boolean):String;
   function CompareString(const AString1,AString2:String;AReverse:Boolean):Integer;

   {Checksum Methods}
   function ChecksumValidationRecord(AValidation:PELTORITOValidationRecord):Word;

   {Conversion Methods}
   function RecordToPath(ARecord:Pointer;APath:TCDFSDiskPath;AUnicode,AEndian:Boolean):Boolean;
   function PathToRecord(APath:TCDFSDiskPath;ARecord:Pointer;AUnicode,AEndian:Boolean):Boolean;

   function RecordToEntry(ARecord:Pointer;AEntry:TCDFSDiskEntry;AUnicode:Boolean):Boolean;
   function EntryToRecord(AEntry:TCDFSDiskEntry;ARecord:Pointer;AUnicode:Boolean):Boolean;

   function CDFSTypeToFileSysType(ACDFSType:TCDFSType):TFileSysType;
  protected
   {Protected Variables}

   {Protected Methods}
   function LoadMaxFile:Integer; override;
   function LoadMaxPath:Integer; override;
   function LoadAttributes:LongWord; override;
   function LoadSystemName:String; override;
   function LoadVolumeName:String; override;
   function LoadVolumeSerial:LongWord; override;
   function LoadFileSysType:TFileSysType; override;

   function SetVolumeName(const AName:String):Boolean;
   function SetVolumeSerial(ASerial:LongWord):Boolean;

   function ReadEntry(AParent,AEntry:TDiskEntry;var ABuffer;const AStart:Int64;ACount:LongWord;var AOffset,AValue:LongWord):Integer; override;
   function WriteEntry(AParent,AEntry:TDiskEntry;const ABuffer;const AStart:Int64;ACount:LongWord;var AOffset,AValue:LongWord):Integer; override;

   function LoadTrees:Boolean; {Not Override}
   function LoadTables:Boolean; override;
   function LoadBlocks:Boolean; override;
   function LoadDescriptors:Boolean; {Not override}
   function LoadRoots:Boolean; {Not override}
   function LoadPaths(ATable:TCDFSDiskTable):Boolean; {Not override}
   function LoadEntries(AParent:TDiskEntry):Boolean; override;
   function LoadCatalogs:Boolean; override;

   function SetPaths(ATable:TCDFSDiskTable):Boolean; {Not override}
   function SetEntries(AParent:TDiskEntry):Boolean; {Not override}
   function SetCatalogs:Boolean; {Not override}

   function LoadTable(ATableNo:LongWord):Boolean; override;
   function LoadBlock(ABlockNo:LongWord):Boolean; override;
   function LoadDescriptor(ABuffer:Pointer;ACluster:LongWord):Boolean; {Not override}
   function LoadRoot(ADescriptor:TCDFSDiskDescriptor):Boolean; {Not override}
   function LoadPath(ATable:TCDFSDiskTable;ABuffer:Pointer;AOffset:LongWord;ANumber:Word):Boolean; {Not override}
   function LoadEntry(AParent:TCDFSDiskEntry;ABuffer:Pointer;AOffset,ACluster:LongWord):Boolean; {Not override}
   function LoadHeader(ABuffer:Pointer;AOffset,ACluster:LongWord;AHeaderNo:LongWord):Boolean; {Not Override}
   function LoadCatalog(ABuffer:Pointer;AOffset,ACluster:LongWord;ACatalogNo:LongWord;AInitial:Boolean):Boolean; {Not override}
   function LoadExtension(ABuffer:Pointer;AOffset,ACluster:LongWord;AExtensionNo:LongWord):Boolean; {Not Override}
   function LoadExtended(AEntry:TDiskEntry):Boolean; {Not override}

   function AddDescriptor(AType:Byte):TCDFSDiskDescriptor; {Not override}
   function RemoveDescriptor(ADescriptor:TCDFSDiskDescriptor):Boolean; {Not override}

   function AddPath(ATable:TDiskTable;AParentNumber:Word;const AName:String;AStartCluster:LongWord):TCDFSDiskPath; {Not override}
   function RemovePath(ATable:TDiskTable;APath:TCDFSDiskPath):Boolean; {Not override}
   function RenamePath(ATable:TDiskTable;APath:TCDFSDiskPath;const AName:String):Boolean; {Not override}
   function MovePath(ATable:TDiskTable;ADestNumber:Word;APath:TCDFSDiskPath):Boolean; {Not override}

   function AddEntry(AParent:TDiskEntry;const AName:String;AAttributes:LongWord;AReference:Boolean):TDiskEntry; override;
   function AddEntryEx(AParent:TDiskEntry;const AName,AAltName:String;AAttributes:LongWord;AReference:Boolean):TDiskEntry; override;
   function RemoveEntry(AParent,AEntry:TDiskEntry):Boolean; override;
   function RenameEntry(AParent,AEntry:TDiskEntry;const AName:String):Boolean; override;
   function RenameEntryEx(AParent,AEntry:TDiskEntry;const AAltName:String):Boolean; override;
   function MoveEntry(ASource,ADest,AEntry:TDiskEntry):Boolean; override;

   function AddCatalog(AEntry:TDiskEntry;AMediaType:TMediaType;AFloppyType:TFloppyType;AAttributes:LongWord;ASectorSize:Word;const ASectorCount:Int64):TDiskCatalog; override;
   function RemoveCatalog(ACatalog:TDiskCatalog):Boolean; override;
   function GetNextHeaderNo:LongWord; {Not override}
   function GetMaxHeaderNo:LongWord; {Not override}
   function GetNextHeaderOffset:LongWord; {Not override}
   function GetNextHeaderCluster:LongWord; {Not override}
   function GetNextCatalogOffset:LongWord; {Not override}
   function GetNextCatalogCluster:LongWord; {Not override}

   function SetTable(ATable:TDiskTable):Boolean; override;
   function SetBlock(ABlock:TDiskBlock):Boolean; override;
   function SetDescriptor(ADescriptor:TCDFSDiskDescriptor):Boolean; {Not override}
   function SetRoot(ADescriptor:TCDFSDiskDescriptor;ARoot:TDiskEntry):Boolean; {Not override}
   function SetPath(ATable:TDiskTable;APath:TCDFSDiskPath;ABuffer:Pointer):Boolean; {Not override}
   function SetEntry(AParent,AEntry:TDiskEntry):Boolean; override;
   function SetHeader(AHeader:TCDFSDiskHeader):Boolean; {Not override}
   function SetCatalog(ACatalog:TDiskCatalog):Boolean; override;
   function SetExtension(AExtension:TCDFSDiskExtension):Boolean; {Not override}
   function SetExtended(AEntry:TDiskEntry;AExtended:TCDFSDiskExtended):Boolean; {Not override}

   function SizeEntry(AParent,AEntry:TDiskEntry;const ASize:Int64):Boolean; override;

   function GetBlock(ABlockNo:LongWord):TDiskBlock; override;
   function GetBlockEx(ABlockNo:LongWord;AWrite:Boolean):TDiskBlock; override;
   function GetDescriptor(AType,AInstance:Byte):TCDFSDiskDescriptor; {Not override}
   function GetDescriptorEx(AType,AInstance:Byte;AWrite:Boolean):TCDFSDiskDescriptor; {Not override}
   function GetRoot(ADescriptor:TCDFSDiskDescriptor):TDiskEntry; {Not override}
   function GetPath(ATable:TDiskTable;AParentNumber:Word;const AName:String):TCDFSDiskPath; {Not override}
   function GetHeader(AHeaderNo:LongWord):TCDFSDiskHeader; {Not override}
   function GetHeaderEx(AHeaderNo:LongWord;AWrite:Boolean):TCDFSDiskHeader; {Not override}
   function GetExtension(AExtensionNo:LongWord):TCDFSDiskExtension; {Not override}
   function GetExtensionEx(AExtensionNo:LongWord;AWrite:Boolean):TCDFSDiskExtension; {Not override}
   function GetExtended(AEntry:TDiskEntry):TCDFSDiskExtended; {Not override}

   function GetVersion(const AName:String):String;

   function StripDot(const AName:String):String;
   function StripVersion(const AName:String):String;

   function CompareName(const AName,AMatch:String;AWildcard:Boolean):Boolean; override;
   function CompareSequence(const ASequence;const ACompare;ASize:Integer):Boolean;
   function CompareIdentifier(const AIdentifier;const ACompare;ASize:Integer):Boolean;

   function CalculateVolumeSerial(ABuffer:Pointer;ASize:Integer):LongWord;
  public
   {Public Variables}
   property ReadOnly:Boolean read FReadOnly write FReadOnly;
   property LongNames:Boolean read FLongNames write FLongNames;
   property CasePreserved:Boolean read FCasePreserved write FCasePreserved;
   property UnicodeNames:Boolean read FUnicodeNames write FUnicodeNames;
   property SwapSerial:Boolean read FSwapSerial write FSwapSerial;

   {Public Methods}
   function FileSystemInit:Boolean; override;

   function MountFileSystem:Boolean; override;
   function DismountFileSystem:Boolean; override;
   function InitializeFileSystem(ASectorsPerCluster:LongWord;AFileSysType:TFileSysType):Boolean; override;

    {System Functions}
   function GetDriveLabel:String; override;
   function SetDriveLabel(const ALabel:String):Boolean; override;
   function SetDriveSerial(ASerial:LongWord):Boolean; override;

   function GetDriveFreeSpaceEx:Int64; override;
   function GetDriveTotalSpaceEx:Int64; override;
   
   function GetDriveInformation(var AClusterSize:LongWord;var ATotalClusterCount,AFreeClusterCount:Int64):Boolean; override;
 end;
 
 TCDFSDiskTable = class(TDiskTable)       {Represents a CDFS path table}
   constructor Create(ALock:TSynchronizerHandle;ALocalLock:TMutexHandle;ADescriptor:TCDFSDiskDescriptor);
   destructor Destroy; override;
  private
   {Private Variables}
   FTableType:Byte;
   FEndian:Boolean;
   FUnicode:Boolean;

   FPathsLoaded:Boolean;

   FStartCluster:LongWord;
   FClusterCount:LongWord;

   {Table Variables}
   FDataFree:LongWord;       {Data Free}
   FDataSize:LongWord;       {Data Length}

   {Object Variables}
   FPaths:TFileSysListEx; 
   FPathLocal:TMutexHandle;
   
   FDescriptor:TCDFSDiskDescriptor;
   
   {Private Methods}
   
  public
   {Public Properties}
   property TableType:Byte read FTableType write FTableType;
   property Endian:Boolean read FEndian write FEndian;
   property Unicode:Boolean read FUnicode write FUnicode;

   property PathsLoaded:Boolean read FPathsLoaded write FPathsLoaded;

   property StartCluster:LongWord read FStartCluster write FStartCluster;
   property ClusterCount:LongWord read FClusterCount write FClusterCount;

   {Table Properties}
   property DataFree:LongWord read FDataFree write FDataFree;
   property DataSize:LongWord read FDataSize write FDataSize;

   {Object Properties}
   property Paths:TFileSysListEx read FPaths;
   property PathLocal:TMutexHandle read FPathLocal;
   property Descriptor:TCDFSDiskDescriptor read FDescriptor;
 end;
 
 TCDFSDiskBlock = class(TDiskBlock)       {Represents a block of clusters}
   constructor Create(ALocalLock:TMutexHandle);
   destructor Destroy; override;
  private
   {Private Variables}
   FBlockCount:LongWord;            {Number of cluster entries in this block}
   FBlockBuffer:Pointer;
   FBlockCluster:LongWord;          {First cluster represented by this block}

   {Object Variables}
  public
   {Public Properties}
   property BlockCount:LongWord read FBlockCount write FBlockCount;
   property BlockBuffer:Pointer read FBlockBuffer write FBlockBuffer;
   property BlockCluster:LongWord read FBlockCluster write FBlockCluster;
 end;
 
 TCDFSDiskEntry = class(TDiskEntry)       {Represents a CDFS directory entry}
   constructor Create(ALocalLock:TMutexHandle);
   destructor Destroy; override;
  private
   {Private Variables}
   FUnicode:Boolean;
   FVersion:String;

   FEntryOffset:LongWord;
   FEntryCluster:LongWord;
   FStartCluster:LongWord;
   FClusterCount:LongWord;

   {Entry Variables}                    {TCDFSDirectoryRecord}
   FRecordSize:Byte;                    {Length of Directory Record (LEN-DR)}
   FExtendedSize:Byte;                  {Extended Attribute Record Length}
   FDataSize:LongWord;                  {Data Length}
   FUnitSize:Byte;                      {File Unit Size}
   FInterleaveSize:Byte;                {Interleave Gap Size}
   FSequenceNumber:Word;                {Volume Sequence Number}

   FDataFree:LongWord;                  {Data Free}

   {Object Variables}
   FPath:TCDFSDiskPath;
   FPathM:TCDFSDiskPath;
   FAltPath:TCDFSDiskPath;
   FAltPathM:TCDFSDiskPath;
   FExtended:TCDFSDiskExtended;
   
   {Private Methods}
   function GetVersion:String;
   procedure SetVersion(const AVersion:String);
  public
   {Public Properties}
   property Unicode:Boolean read FUnicode write FUnicode;
   property Version:String read GetVersion write SetVersion;

   property EntryOffset:LongWord read FEntryOffset write FEntryOffset;
   property EntryCluster:LongWord read FEntryCluster write FEntryCluster;
   property StartCluster:LongWord read FStartCluster write FStartCluster;
   property ClusterCount:LongWord read FClusterCount write FClusterCount;

   {Entry Properties}
   property RecordSize:Byte read FRecordSize write FRecordSize;
   property ExtendedSize:Byte read FExtendedSize write FExtendedSize;
   property DataSize:LongWord read FDataSize write FDataSize;
   property FirstBlock:LongWord read FStartCluster write FStartCluster;
   property UnitSize:Byte read FUnitSize write FUnitSize;
   property InterleaveSize:Byte read FInterleaveSize write FInterleaveSize;
   property SequenceNumber:Word read FSequenceNumber write FSequenceNumber;

   property DataFree:LongWord read FDataFree write FDataFree;

   {Object Properties}
   property Path:TCDFSDiskPath read FPath write FPath;
   property PathM:TCDFSDiskPath read FPathM write FPathM;
   property AltPath:TCDFSDiskPath read FAltPath write FAltPath;
   property AltPathM:TCDFSDiskPath read FAltPathM write FAltPathM;
   property Extended:TCDFSDiskExtended read FExtended write FExtended;

   {Public Methods}
   function PathNumber:Word;
   function ParentNumber:Word;

   function FileIdentifier:String;
   function FileIdentifierSize:Byte;
   function DirectoryRecordSize:Byte;
   function ExtendedRecordSize:Byte;
 end;
 
 TCDFSDiskCatalog = class(TDiskCatalog)   {Represents a CDFS boot catalog entry (Also the Initial/Default entry)}
   constructor Create(ALocalLock:TMutexHandle);
   destructor Destroy; override;
  private
   {Private Variables}
   FCatalogOffset:LongWord;
   FCatalogCluster:LongWord;
   FStartCluster:LongWord;
   FClusterCount:LongWord;

   {Catalog Variables}
   FInitial:Boolean;
   FBootIndicator:Byte;
   FBootMedia:Byte;
   FLoadSegment:Word;
   FSystemType:Byte;
   FLoadCount:Word;

   {Catalog Variables}
   FSelectionType:Byte;
   FSelectionData:Pointer;

   {Object Variables}
   FEntry:TCDFSDiskEntry;
   FHeader:TCDFSDiskHeader;
  protected
   {Protected Methods}
   function GetName:String; override;
   procedure SetName(const AName:String); override;
   function GetMediaType:TMediaType; override;
   procedure SetMediaType(AMediaType:TMediaType); override;
   function GetFloppyType:TFloppyType; override;
   procedure SetFloppyType(AFloppyType:TFloppyType); override;
   function GetAttributes:LongWord; override;
   procedure SetAttributes(AAttributes:LongWord); override;
   function GetSectorSize:Word; override;
   procedure SetSectorSize(ASectorSize:Word); override;
   function GetSectorCount:Int64; override;
   procedure SetSectorCount(const ASectorCount:Int64); override;

   function GetStartCluster:LongWord;
   procedure SetStartCluster(AStartCluster:LongWord);
   function GetClusterCount:LongWord;
   procedure SetClusterCount(AClusterCount:LongWord);
  public
   {Public Properties}
   property CatalogOffset:LongWord read FCatalogOffset write FCatalogOffset;
   property CatalogCluster:LongWord read FCatalogCluster write FCatalogCluster;
   property StartCluster:LongWord read GetStartCluster write SetStartCluster;
   property ClusterCount:LongWord read GetClusterCount write SetClusterCount;

   {Catalog Properties}
   property Initial:Boolean read FInitial write FInitial;
   property BootIndicator:Byte read FBootIndicator write FBootIndicator;
   property BootMedia:Byte read FBootMedia write FBootMedia;
   property LoadSegment:Word read FLoadSegment write FLoadSegment;
   property SystemType:Byte read FSystemType write FSystemType;
   property LoadCount:Word read FLoadCount write FLoadCount;

   {Catalog Properties}
   property SelectionType:Byte read FSelectionType write FSelectionType;
   property SelectionData:Pointer read FSelectionData write FSelectionData;

   {Object Properties}
   property Entry:TCDFSDiskEntry read FEntry write FEntry;
   property Header:TCDFSDiskHeader read FHeader write FHeader;
   property Validation:TCDFSDiskHeader read FHeader write FHeader;

   {Public Methods}
  end;
 
 TCDFSDiskPath = class(TListObject)       {Represents a CDFS path table entry}
   constructor Create(ALocalLock:TMutexHandle);
   destructor Destroy; override;
  private
   {Private Variables}
   FLocalLock:TMutexHandle;
   
   FName:String;
   FHash:LongWord;

   FPathNumber:Word;
   FPathOffset:LongWord;
   FStartCluster:LongWord;

   {Path Variables}                     {TCDFSPathTableRecord}
   FExtendedSize:Byte;                  {Extended Attribute Record Length}
   FParentNumber:Word;                  {Parent Directory Number}

   {Object Variables}
   FParent:TCDFSDiskPath;

   {Private Methods}
   function AcquireLock:Boolean;
   function ReleaseLock:Boolean;

   function GetName:String;
   procedure SetName(const AName:String);
  public
   {Public Properties}
   property Name:String read GetName write SetName;
   property Hash:LongWord read FHash;

   property PathNumber:Word read FPathNumber write FPathNumber;
   property PathOffset:LongWord read FPathOffset write FPathOffset;
   property StartCluster:LongWord read FStartCluster write FStartCluster;

   {Path Properties}
   property ExtendedSize:Byte read FExtendedSize write FExtendedSize;
   property FirstBlock:LongWord read FStartCluster write FStartCluster;
   property ParentNumber:Word read FParentNumber write FParentNumber;

   {Object Properties}
   property Parent:TCDFSDiskPath read FParent write FParent;

   {Public Methods}
   function PathIdentifier:String;
   function PathIdentifierSize(AUnicode:Boolean):Byte;
   function PathRecordSize(AUnicode:Boolean):Byte;
 end;

 TCDFSDiskExtended = class(TObject)      {Represents a CDFS extended attribute}
   constructor Create(ALocalLock:TMutexHandle);
   destructor Destroy; override;
  private
   {Private Variables}
   FLocalLock:TMutexHandle;
   
   FStartCluster:LongWord;
   FClusterCount:LongWord;

   {Extended  Variables}            {TCDFSExtendedAttributeRecord}
   FOwnerId:Word;                   {Owner Identification}
   FGroupId:Word;                   {Group Identification}
   FPermissions:Word;               {Permissions 16 bits}
   FCreateTime:TFileTime;           {File Creation Date and Time}
   FModifyTime:TFileTime;           {File Modification Date and Time}
   FExpireTime:TFileTime;           {File Expiration Date and Time}
   FEffectiveTime:TFileTime;        {File Effective Date and Time}
   FRecordFormat:Byte;              {Record Format}
   FRecordAttributes:Byte;          {Record Attributes}
   FRecordLength:Word;              {Record Length}
   FSystemIdentifier:String;        {System Identifier a-characters, a1-characters}
   FSystemData:Pointer;             {System Use not specified}
   FExtendedVersion:Byte;           {Extended Attribute Record Version}
   FEscapeSequenceSize:Byte;        {Length of Escape Sequences (LEN_ESC)}
   FApplicationDataSize:Word;       {Length of Application Use (LEN_AU)}
   FApplicationData:Pointer;        {Application Use LEN_AU bytes}
   FEscapeSequence:Pointer;         {Escape Sequences LEN_ESC bytes}

   {Object Variables}
   
   {Private Methods}
   function AcquireLock:Boolean;
   function ReleaseLock:Boolean;

   function GetSystemIdentifier:String;
   procedure SetSystemIdentifier(const ASystemIdentifier:String);
  public
   {Public Properties}
   property StartCluster:LongWord read FStartCluster write FStartCluster;
   property ClusterCount:LongWord read FClusterCount write FClusterCount;

   {Extended Properties}
   property OwnerId:Word read FOwnerId write FOwnerId;
   property GroupId:Word read FGroupId write FGroupId;
   property Permissions:Word read FPermissions write FPermissions;
   property CreateTime:TFileTime read FCreateTime write FCreateTime;
   property ModifyTime:TFileTime read FModifyTime write FModifyTime;
   property ExpireTime:TFileTime read FExpireTime write FExpireTime;
   property EffectiveTime:TFileTime read FEffectiveTime write FEffectiveTime;
   property RecordFormat:Byte read FRecordFormat write FRecordFormat;
   property RecordAttributes:Byte read FRecordAttributes write FRecordAttributes;
   property RecordLength:Word read FRecordLength write FRecordLength;
   property SystemIdentifier:String read GetSystemIdentifier write SetSystemIdentifier;
   property SystemData:Pointer read FSystemData write FSystemData;
   property ExtendedVersion:Byte read FExtendedVersion write FExtendedVersion;
   property EscapeSequenceSize:Byte read FEscapeSequenceSize write FEscapeSequenceSize;
   property ApplicationDataSize:Word read FApplicationDataSize write FApplicationDataSize;
   property ApplicationData:Pointer read FApplicationData write FApplicationData;
   property EscapeSequence:Pointer read FEscapeSequence write FEscapeSequence;

   {Public Methods}
   function ExtendedRecordSize:Byte;
 end;

 TCDFSDiskDescriptor = class(TListObject) {Represents a CDFS volume descriptor}
   constructor Create(ALocalLock:TMutexHandle);
   destructor Destroy; override;
  private
   {Private Variables}
   FLocalLock:TMutexHandle;
   
   FUnicode:Boolean;

   FStartCluster:LongWord;

   {Descriptor Variables}           {TCDFSVolumeDescriptorHeader/TCDFSVolumeDescriptorTerminator}
   FDescriptorType:Byte;            {Descriptor Type (00 = Boot, 01 = Primary etc)}
   FDescriptorVersion:Byte;         {Descriptor Version (1)}
   FStandardIdentifier:String;      {Standard Identifier ('CD001')}

   {Descriptor Variables}           {TCDFSVolumeDescriptorPrimary/TCDFSVolumeDescriptorSupplementary}
   FSystemIdentifier:String;        {System Identifier a-characters (eg EL TORITO SPECIFICATION)}
   FVolumeIdentifier:String;        {Volume Identifier d-characters}
   FVolumeSpaceSize:LongWord;       {Volume Space Size}
   FVolumeSetSize:Word;             {Volume Set Size}
   FVolumeSequenceNumber:Word;      {Volume Sequence Number}
   FLogicalBlockSize:Word;          {Logical Block Size}
   FPathTableSize:LongWord;         {Path Table Size}
   FPrimaryPathTable:LongWord;      {Location of Occurrence of Type L Path Table}
   FAlternatePathTable:LongWord;    {Location of Optional Occurrence of Type L Path Table}
   FPrimaryPathTableM:LongWord;     {Location of Occurrence of Type M Path Table}
   FAlternatePathTableM:LongWord;   {Location of Optional Occurrence of Type M Path Table}
   FVolumeSetIdentifier:String;     {Volume Set Identifier d-characters}
   FPublisherIdentifier:String;     {Publisher Identifier a-characters}
   FPreparerIdentifier:String;      {Data Preparer Identifier a-characters}
   FApplicationIdentifier:String;   {Application Identifier a-characters}
   FCopyrightIdentifier:String;     {Copyright File Identifier d-characters, SEPARATOR 1, SEPARATOR 2}
   FAbstractIdentifier:String;      {Abstract File Identifier d-characters, SEPARATOR 1, SEPARATOR 2}
   FBibliographicIdentifier:String; {Bibliographic File Identifier d-characters, SEPARATOR 1, SEPARATOR 2}
   FCreateTime:TFileTime;           {Volume Creation Date and Time}
   FModifyTime:TFileTime;           {Volume Modification Date and Time}
   FExpireTime:TFileTime;           {Volume Expiration Date and Time}
   FEffectiveTime:TFileTime;        {Volume Effective Date and Time}
   FFileStructureVersion:Byte;      {File Structure Version}
   FApplicationData:Pointer;        {Application Use not specified}

   FVolumeFlags:Byte;               {Volume Flags 8 bits}
   FEscapeSequences:Pointer;        {Escape Sequences 32 bytes}

   {Descriptor Variables}           {TCDFSVolumeDescriptorBoot/TELTORITOVolumeDescriptorBoot}
   FBootIdentifier:String;          {32 a-characters}
   FCatalogStart:LongWord;          {Absolute pointer to first sector of Boot Catalog}
   FCatalogCount:LongWord;          {Number of Sectors in Boot Catalog (Not Stored)}
   
   {Descriptor Variables}           {TCDFSVolumeDescriptorPartition}
   FPartitionIdentifier:String;     {Volume Partition Identifier d-characters}
   FPartitionStart:LongWord;        {Volume Partition Location (first Logical Block)}
   FPartitionSize:LongWord;         {Volume Partition Size (in Logical Blocks)}
   FSystemData:Pointer;             {System Use not specified}

   {Object Variables}
   FRoot:TCDFSDiskEntry;
   FPrimary:TCDFSDiskTable;
   FPrimaryM:TCDFSDiskTable;
   FAlternate:TCDFSDiskTable;
   FAlternateM:TCDFSDiskTable;

   FInitial:TCDFSDiskCatalog;
   FValidation:TCDFSDiskHeader;
   
   {Private Methods}
   function AcquireLock:Boolean;
   function ReleaseLock:Boolean;

   function GetStandardIdentifier:String;
   procedure SetStandardIdentifier(const AStandardIdentifier:String);
   
   function GetSystemIdentifier:String;
   procedure SetSystemIdentifier(const ASystemIdentifier:String);
   function GetVolumeIdentifier:String;
   procedure SetVolumeIdentifier(const AVolumeIdentifier:String);
   
   function GetVolumeSetIdentifier:String;
   procedure SetVolumeSetIdentifier(const AVolumeSetIdentifier:String);
   function GetPublisherIdentifier:String;
   procedure SetPublisherIdentifier(const APublisherIdentifier:String);
   function GetPreparerIdentifier:String;
   procedure SetPreparerIdentifier(const APreparerIdentifier:String);
   function GetApplicationIdentifier:String;
   procedure SetApplicationIdentifier(const AApplicationIdentifier:String);
   function GetCopyrightIdentifier:String;
   procedure SetCopyrightIdentifier(const ACopyrightIdentifier:String);
   function GetAbstractIdentifier:String;
   procedure SetAbstractIdentifier(const AAbstractIdentifier:String);
   function GetBibliographicIdentifier:String;
   procedure SetBibliographicIdentifier(const ABibliographicIdentifier:String);
   
   function GetBootIdentifier:String;
   procedure SetBootIdentifier(const ABootIdentifier:String);
   
   function GetPartitionIdentifier:String;
   procedure SetPartitionIdentifier(const APartitionIdentifier:String);
  public
   {Public Properties}
   property Unicode:Boolean read FUnicode write FUnicode;

   property StartCluster:LongWord read FStartCluster write FStartCluster;

   {Descriptor Properties}
   property DescriptorType:Byte read FDescriptorType write FDescriptorType;
   property DescriptorVersion:Byte read FDescriptorVersion write FDescriptorVersion;
   property StandardIdentifier:String read GetStandardIdentifier write SetStandardIdentifier;

   {Descriptor Properties}
   property SystemIdentifier:String read GetSystemIdentifier write SetSystemIdentifier;
   property VolumeIdentifier:String read GetVolumeIdentifier write SetVolumeIdentifier;
   property VolumeSpaceSize:LongWord read FVolumeSpaceSize write FVolumeSpaceSize;
   property VolumeSetSize:Word read FVolumeSetSize write FVolumeSetSize;
   property VolumeSequenceNumber:Word read FVolumeSequenceNumber write FVolumeSequenceNumber;
   property LogicalBlockSize:Word read FLogicalBlockSize write FLogicalBlockSize;
   property PathTableSize:LongWord read FPathTableSize write FPathTableSize;
   property PrimaryPathTable:LongWord read FPrimaryPathTable write FPrimaryPathTable;
   property AlternatePathTable:LongWord read FAlternatePathTable write FAlternatePathTable;
   property PrimaryPathTableM:LongWord read FPrimaryPathTableM write FPrimaryPathTableM;
   property AlternatePathTableM:LongWord read FAlternatePathTableM write FAlternatePathTableM;
   property VolumeSetIdentifier:String read GetVolumeSetIdentifier write SetVolumeSetIdentifier;
   property PublisherIdentifier:String read GetPublisherIdentifier write SetPublisherIdentifier;
   property PreparerIdentifier:String read GetPreparerIdentifier write SetPreparerIdentifier;
   property ApplicationIdentifier:String read GetApplicationIdentifier write SetApplicationIdentifier;
   property CopyrightIdentifier:String read GetCopyrightIdentifier write SetCopyrightIdentifier;
   property AbstractIdentifier:String read GetAbstractIdentifier write SetAbstractIdentifier;
   property BibliographicIdentifier:String read GetBibliographicIdentifier write SetBibliographicIdentifier;
   property CreateTime:TFileTime read FCreateTime write FCreateTime;
   property ModifyTime:TFileTime read FModifyTime write FModifyTime;
   property ExpireTime:TFileTime read FExpireTime write FExpireTime;
   property EffectiveTime:TFileTime read FEffectiveTime write FEffectiveTime;
   property FileStructureVersion:Byte read FFileStructureVersion write FFileStructureVersion;
   property ApplicationData:Pointer read FApplicationData write FApplicationData;

   property VolumeFlags:Byte read FVolumeFlags write FVolumeFlags;
   property EscapeSequences:Pointer read FEscapeSequences write FEscapeSequences;

   {Descriptor Properties}
   property BootIdentifier:String read GetBootIdentifier write SetBootIdentifier;
   property CatalogStart:LongWord read FCatalogStart write FCatalogStart;
   property CatalogCount:LongWord read FCatalogCount write FCatalogCount;

   {Descriptor Properties}
   property PartitionIdentifier:String read GetPartitionIdentifier write SetPartitionIdentifier;
   property PartitionStart:LongWord read FPartitionStart write FPartitionStart;
   property PartitionSize:LongWord read FPartitionSize write FPartitionSize;
   property SystemData:Pointer read FSystemData write FSystemData;

   {Object Properties}
   property Root:TCDFSDiskEntry read FRoot write FRoot;
   property Primary:TCDFSDiskTable read FPrimary write FPrimary;
   property PrimaryM:TCDFSDiskTable read FPrimaryM write FPrimaryM;
   property Alternate:TCDFSDiskTable read FAlternate write FAlternate;
   property AlternateM:TCDFSDiskTable read FAlternateM write FAlternateM;

   property Initial:TCDFSDiskCatalog read FInitial write FInitial;
   property Validation:TCDFSDiskHeader read FValidation write FValidation;
 end;

 TCDFSDiskHeader = class(TListObject) {Represents a CDFS catalog header (Also the Validation entry)}
   constructor Create(ALocalLock:TMutexHandle);
   destructor Destroy; override;
  private
   {Private Variables}
   FLocalLock:TMutexHandle;
   
   FHeaderNo:LongWord;

   FHeaderOffset:LongWord;
   FHeaderCluster:LongWord;

   {Header Variables}
   FValidation:Boolean;
   FHeaderId:Byte;
   FPlatformId:Byte;
   FVendorId:String;
   FChecksum:Word;
   FSignature:Word;

   {Header Variables}
   FSectionCount:Word;

   {Object Variables}
   
   {Private Methods}
   function AcquireLock:Boolean;
   function ReleaseLock:Boolean;
   
   function GetVendorId:String;
   procedure SetVendorId(const AVendorId:String);
  public
   {Public Properties}
   property HeaderNo:LongWord read FHeaderNo write FHeaderNo;

   property HeaderOffset:LongWord read FHeaderOffset write FHeaderOffset;
   property HeaderCluster:LongWord read FHeaderCluster write FHeaderCluster;

   {Header Properties}
   property Validation:Boolean read FValidation write FValidation;
   property HeaderId:Byte read FHeaderId write FHeaderId;
   property PlatformId:Byte read FPlatformId write FPlatformId;
   property VendorId:String read GetVendorId write SetVendorId;
   property Checksum:Word read FChecksum write FChecksum;
   property Signature:Word read FSignature write FSignature;

   {Header Properties}
   property HeaderIndicator:Byte read FHeaderId write FHeaderId;
   property SectionCount:Word read FSectionCount write FSectionCount;
   property SectionId:String read GetVendorId write SetVendorId;

   {Object Properties}

   {Public Methods}
  end;

 TCDFSDiskExtension = class(TListObject) {Represents a CDFS catalog extension}
   constructor Create(ALocalLock:TMutexHandle);
   destructor Destroy; override;
  private
   {Private Variables}
   FLocalLock:TMutexHandle;
   
   FExtensionNo:LongWord;

   FExtensionOffset:LongWord;
   FExtensionCluster:LongWord;

   {Extension Variables}
   FExtensionIndicator:Byte;
   FExtensionFlag:Byte;
   FSelectionData:Pointer;

   {Object Variables}
   FCatalog:TCDFSDiskCatalog;
   
   {Private Methods}
   function AcquireLock:Boolean;
   function ReleaseLock:Boolean;
  public
   {Public Properties}
   property ExtensionNo:LongWord read FExtensionNo write FExtensionNo;

   property ExtensionOffset:LongWord read FExtensionOffset write FExtensionOffset;
   property ExtensionCluster:LongWord read FExtensionCluster write FExtensionCluster;

   {Extension Properties}
   property ExtensionIndicator:Byte read FExtensionIndicator write FExtensionIndicator;
   property ExtensionFlag:Byte read FExtensionFlag write FExtensionFlag;
   property SelectionData:Pointer read FSelectionData write FSelectionData;

   {Object Properties}
   property Catalog:TCDFSDiskCatalog read FCatalog write FCatalog;

   {Public Methods}
  end;
 
{==============================================================================}
{var}
 {CDFS specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure CDFSInit;
procedure CDFSQuit;

{==============================================================================}
{CDFS Functions}

{==============================================================================}
{CDFS Helper Functions}
function CDFSDataToPointer(const AData;ASize:Integer;ASwap:Boolean):Pointer;
function CDFSPointerToData(APointer:Pointer;var AData;ASize:Integer;ASwap:Boolean):Boolean;

function CDFSIdentifierToString(const AIdentifier;ASize:Integer;AUnicode:Boolean):String;
function CDFSStringToIdentifier(const AString:String;var AIdentifier;ASize:Integer;AUnicode:Boolean):Boolean;

function CDFSIdentifierToFileName(const AIdentifier;ASize:Integer;AUnicode:Boolean):String;
function CDFSFileNameToIdentifier(const AFileName:String;var AIdentifier;ASize:Integer;AUnicode:Boolean):Boolean;

function CDFSTimeToFileTime(const ATime:TCDFSTime):TFileTime;
function FileTimeToCDFSTime(const AFileTime:TFileTime;var ATime:TCDFSTime):Boolean;

function CDFSDateTimeToFileTime(const ADateTime:TCDFSDateTime):TFileTime;
function FileTimeToCDFSDateTime(const AFileTime:TFileTime;var ADateTime:TCDFSDateTime):Boolean;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {CDFS specific variables}
 CDFSInitialized:Boolean;

{==============================================================================}
{==============================================================================}
{TCDFSRecognizer}
constructor TCDFSRecognizer.Create(ADriver:TFileSysDriver);
begin
 {}
 inherited Create(ADriver);
 FLongNames:=True;
 FSwapSerial:=False; {Set to True for Win9x compatibility}
 
 FAllowDrive:=True;
 FAllowDefault:=False;
 
 FFormatter:=TCDFSFormatter.Create(FDriver,Self);
 FResizer:=TCDFSResizer.Create(FDriver,Self);
 FCopier:=TCDFSCopier.Create(FDriver,Self);
end;

{==============================================================================}

function TCDFSRecognizer.CheckPrimaryDescriptor(ASector:PDiskSector;const AStartSector:Int64;ASectorCount:LongWord):Boolean;
begin
 {}
 Result:=False;
 
 if ASector = nil then Exit;
 
 {$IFDEF CDFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSRecognizer.CheckPrimaryDescriptor - StartSector = ' + IntToStr(AStartSector) + ' SectorCount = ' + IntToStr(ASectorCount));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('                                  Descriptor.DescriptorType           = ' + IntToStr(PCDFSVolumeDescriptorHeader(ASector).DescriptorType));
 {if FILESYS_LOG_ENABLED then FileSysLogDebug('                                  Descriptor.StandardIdentifier       = ' + IntToStr(PCDFSVolumeDescriptorHeader(ASector).StandardIdentifier));}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('                                  Descriptor.DescriptorVersion        = ' + IntToStr(PCDFSVolumeDescriptorHeader(ASector).DescriptorVersion));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('                                  Descriptor.VolumeSpaceSize          = ' + IntToStr(PCDFSVolumeDescriptorPrimary(ASector).VolumeSpaceSize));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('                                  Descriptor.LogicalBlockSize         = ' + IntToStr(PCDFSVolumeDescriptorPrimary(ASector).LogicalBlockSize));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('                                  Descriptor.PathTableSize            = ' + IntToStr(PCDFSVolumeDescriptorPrimary(ASector).PathTableSize));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('                                  Descriptor.RootDirectory.FirstBlock = ' + IntToStr(PCDFSVolumeDescriptorPrimary(ASector).RootDirectory.FirstBlock));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('                                  Descriptor.RootDirectory.DataSize   = ' + IntToStr(PCDFSVolumeDescriptorPrimary(ASector).RootDirectory.DataSize));
 {$ENDIF}
 
 {Check for CDFS}
 if PCDFSVolumeDescriptorHeader(ASector).DescriptorVersion <> cdfsISO9660DescriptorVersion then Exit;
 if PCDFSVolumeDescriptorHeader(ASector).DescriptorType <> cdfsVolumeDescriptorTypePrimary then Exit;
 if not CompareMem(@PCDFSVolumeDescriptorHeader(ASector).StandardIdentifier[0],@cdfsISO9660StandardIdentifier[1],5) then Exit;
 if PCDFSVolumeDescriptorPrimary(ASector).VolumeSpaceSize = 0 then Exit;
 if PCDFSVolumeDescriptorPrimary(ASector).VolumeSpaceSize > ASectorCount then Exit;
 if PCDFSVolumeDescriptorPrimary(ASector).LogicalBlockSize = 0 then Exit;
 if PCDFSVolumeDescriptorPrimary(ASector).LogicalBlockSize mod MIN_SECTOR_SIZE <> 0 then Exit;
 
 Result:=True;
end;

{==============================================================================}

function TCDFSRecognizer.GetName:String;
begin
 {}
 Result:='CDFS';
end;

{==============================================================================}

function TCDFSRecognizer.RecognizePartitionId(APartitionId:Byte):Boolean; 
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
 
  {Nothing - CDFS does not support Partitions}
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TCDFSRecognizer.RecognizeBootSector(ABootSector:PBootSector;const AStartSector,ASectorCount:Int64):Boolean; 
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
 
  {Nothing - CDFS does not support "Removable" media}
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TCDFSRecognizer.RecognizePartition(APartition:TDiskPartition):Boolean;
{Note: Caller must hold the partition lock}
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if APartition = nil then Exit;
  
  {Nothing - CDFS does not support Partitions}
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TCDFSRecognizer.RecognizeVolume(AVolume:TDiskVolume):Boolean;
{Note: Caller must hold the volume writer lock}
var
 Sector:PDiskSector;
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if AVolume = nil then Exit;
  if AVolume.Device = nil then Exit;
  if AVolume.Device.Controller = nil then Exit;
 
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSRecognizer.RecognizeVolume - Volume = ' + AVolume.Name);
  {$ENDIF}

  {Check for Partitioned Media}
  if AVolume.Partition <> nil then Exit;
  
  {Non Partitioned Media}
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSRecognizer.RecognizeVolume - Non Partitioned Media');
  {$ENDIF}
  
  {Check Device Type}
  case AVolume.Device.MediaType of
   mtCDROM,mtDVD:begin  {CDFS only on CDROM/DVD}
     {Check Default}
     if not AllowDefault then
      begin
       {Check Media}
       if not AVolume.Device.Controller.MediaReady(AVolume.Device) then Exit; {was Volume.Device.MediaReady}
       
       {Init Device}
       if not AVolume.Device.DeviceInit then Exit;

       {Init Volume}
       if not AVolume.VolumeInit then Exit;
       
       {Allocate Sector}
       Sector:=GetMem(AVolume.Device.SectorSize);
       if Sector = nil then Exit;
       try
        {Read Sector}
        if not FDriver.Cache.DeviceRead(AVolume.Device,AVolume.StartSector + cdfsISO9660StartSector,1,Sector^) then Exit;
        
        {Check Descriptor}
        if not CheckPrimaryDescriptor(Sector,AVolume.StartSector + cdfsISO9660StartSector,AVolume.SectorCount) then Exit;
        
        {$IFDEF CDFS_DEBUG}
        if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSRecognizer.RecognizeVolume - Volume Recognized');
        {$ENDIF}
        
        AVolume.Recognized:=True;
        
        Result:=True;
       finally
        FreeMem(Sector);
       end;
      end
     else
      begin
       {Default Recognizer}
       {$IFDEF CDFS_DEBUG}
       if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSRecognizer.RecognizeVolume - Volume Recognized (Default)');
       {$ENDIF}
       
       AVolume.Recognized:=True;
       
       Result:=True;
      end;
    end;
  end;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TCDFSRecognizer.MountVolume(AVolume:TDiskVolume;ADrive:TDiskDrive):Boolean;
{Note: Caller must hold the volume writer lock}
var
 FileSystem:TCDFSFileSystem;
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if AVolume = nil then Exit;
  
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSRecognizer.MountVolume - Volume = ' + AVolume.Name);
  {$ENDIF}
  
  {Check Recognized}
  if not RecognizeVolume(AVolume) then Exit;

  {Create FileSystem}
  FileSystem:=TCDFSFileSystem.Create(FDriver,AVolume,ADrive);
  FileSystem.SwapSerial:=FSwapSerial;
  FileSystem.LongNames:=FLongNames;
  FileSystem.CasePreserved:=FLongNames; {Enabled only if LongNames Enabled}
  FileSystem.UnicodeNames:=FLongNames;  {Enabled only if LongNames Enabled}
  FileSystem.FileSystemInit;
  FileSystem.MountFileSystem;
  
  Result:=True;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{TCDFSFormatter}
function TCDFSFormatter.CheckDevice(AVolume:TDiskVolume;ADrive:TDiskDrive;AFloppyType:TFloppyType):Boolean;
{Checks Device and Floppy types are suitable for formatting}
{Note: Caller must hold the volume lock}
begin
 {}
 Result:=False;
 
 if AVolume = nil then Exit;
 if AVolume.Device = nil then Exit;

 case AVolume.Device.MediaType of
  mtCDROM,mtDVD:begin {CDFS only supported on CDROM/DVD media}
    if AFloppyType <> ftUNKNOWN then Exit;
    if not AVolume.Device.Writeable then Exit;
    
    Result:=True;
   end;
 end;
end;

{==============================================================================}

function TCDFSFormatter.CheckPartition(AVolume:TDiskVolume;ADrive:TDiskDrive;AFileSysType:TFileSysType):Boolean;
{Checks Partition type is suitable for formatting}
{Note: Also check File System type for non partitioned media}
{Note: Caller must hold the volume lock}
begin
 {}
 Result:=False;
 
 if AVolume = nil then Exit;

 {Check FileSystem}
 case AFileSysType of
  fsCDFS,fsUDF:begin
    {CDFS/UDF File System}
    if AVolume.Device = nil then Exit;
    if AVolume.Partition <> nil then Exit; {CDFS only supported on Non Partitioned media}
    
    {Non Partitioned Media}
    Result:=True;
   end;
 end;
end;

{==============================================================================}

function TCDFSFormatter.GetPathTableSize(AVolume:TDiskVolume;ADrive:TDiskDrive;ACDFSType:TCDFSType;ASupplementary:Boolean):LongWord;
{Note: Caller must hold the volume lock}
begin
 {}
 Result:=0;
 
 if AVolume = nil then Exit;

 {Check Type}
 case ACDFSType of
  ctISO9660,ctJOLIET:begin
    Result:=cdfsPathRecordSize + 1; {Always start as one record} {TCDFSPathRecord + Padding Byte}
   end;
 end;
end;

{==============================================================================}

function TCDFSFormatter.GetPathTableStart(AVolume:TDiskVolume;ADrive:TDiskDrive;ACDFSType:TCDFSType;ASupplementary,AEndian:Boolean):LongWord;
{Note: Caller must hold the volume lock}
begin
 {}
 Result:=0;
 
 if AVolume = nil then Exit;

 {Check Type}
 case ACDFSType of
  ctISO9660,ctJOLIET:begin
    if ASupplementary then
     begin
      {Supplementary Descriptor}
      if AEndian then
       begin
        {M Type Table}
        Result:=34; //To Do //Calculate based on Volume.SectorCount
       end
      else
       begin
        {L Type Table}
        Result:=32; //To Do //Calculate based on Volume.SectorCount
       end;
     end
    else
     begin
      {Primary Descriptor}
      if AEndian then
       begin
        {M Type Table}
        Result:=24; //To Do //Calculate based on Volume.SectorCount
       end
      else
       begin
        {L Type Table}
        Result:=22; //To Do //Calculate based on Volume.SectorCount
       end;
     end;
   end;
 end;
end;

{==============================================================================}

function TCDFSFormatter.GetRootDirectorySize(AVolume:TDiskVolume;ADrive:TDiskDrive;ACDFSType:TCDFSType;ASupplementary:Boolean):LongWord;
{Note: Caller must hold the volume lock}
begin
 {}
 Result:=0;
 
 if AVolume = nil then Exit;

 {Check Type}
 case ACDFSType of
  ctISO9660,ctJOLIET:begin
    Result:=AVolume.SectorSize; {Always start as one Cluster}
   end;
 end;
end;

{==============================================================================}

function TCDFSFormatter.GetRootDirectoryStart(AVolume:TDiskVolume;ADrive:TDiskDrive;ACDFSType:TCDFSType;ASupplementary:Boolean):LongWord;
{Note: Caller must hold the volume lock}
begin
 {}
 Result:=0;
 
 if AVolume = nil then Exit;

 {Check Type}
 case ACDFSType of
  ctISO9660,ctJOLIET:begin
    if ASupplementary then
     begin
      {Supplementary Descriptor}
      Result:=36; //To Do //Calculate based on Volume.SectorCount
     end
    else
     begin
      {Primary Descriptor}
      Result:=26; //To Do //Calculate based on Volume.SectorCount
     end;
   end;
 end;
end;

{==============================================================================}

function TCDFSFormatter.CreatePrimaryDescriptor(AVolume:TDiskVolume;ADrive:TDiskDrive;ACDFSType:TCDFSType;ADescriptor:PCDFSVolumeDescriptorPrimary):Boolean;
{Fill the Primary Descriptor with values based on CDFS type}
{Note: Caller must hold the volume lock}
var
 PathTableSize:LongWord;
 PathTableStart:LongWord;
 PathTableStartM:LongWord;
 RootDirectorySize:LongWord;
 RootDirectoryStart:LongWord;

 SystemIdentifier:String;
 VolumeIdentifier:String;
 VolumeSetIdentifier:String;
 PublisherIdentifier:String;
 PreparerIdentifier:String;
 ApplicationIdentifier:String;
 CopyrightIdentifier:String;
 AbstractIdentifier:String;
 BibliographicIdentifier:String;
begin
 {}
 Result:=False;
 
 if FDriver = nil then Exit;
 if AVolume = nil then Exit;
 if ADescriptor = nil then Exit;

 {$IFDEF CDFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFormatter.CreatePrimaryDescriptor');
 {$ENDIF}
 
 {Get Values}
 PathTableSize:=GetPathTableSize(AVolume,ADrive,ACDFSType,False);
 PathTableStart:=GetPathTableStart(AVolume,ADrive,ACDFSType,False,False);
 PathTableStartM:=GetPathTableStart(AVolume,ADrive,ACDFSType,False,True);
 RootDirectorySize:=GetRootDirectorySize(AVolume,ADrive,ACDFSType,False);
 RootDirectoryStart:=GetRootDirectoryStart(AVolume,ADrive,ACDFSType,False);

 SystemIdentifier:='UNKNOWN';
 VolumeIdentifier:='CDROM';
 VolumeSetIdentifier:='CDROM';
 PublisherIdentifier:='ULTIBO';
 PreparerIdentifier:='ULTIBO';
 ApplicationIdentifier:='ULTIBO CDFS';
 CopyrightIdentifier:='';
 AbstractIdentifier:='';
 BibliographicIdentifier:='';

 {Check Type}
 case ACDFSType of
  ctISO9660,ctJOLIET:begin
    {Create PrimaryDescriptor}
    ADescriptor.DescriptorType:=cdfsVolumeDescriptorTypePrimary;
    if not CDFSStringToIdentifier(cdfsISO9660StandardIdentifier,ADescriptor.StandardIdentifier[0],5,False) then Exit;
    ADescriptor.DescriptorVersion:=cdfsISO9660DescriptorVersion;
    {ADescriptor.Reserved1} {Must be Zero}
    if not CDFSStringToIdentifier(SystemIdentifier + StringOfChar(' ',32 - Length(SystemIdentifier)),ADescriptor.SystemIdentifier[0],32,False) then Exit;
    if not CDFSStringToIdentifier(VolumeIdentifier + StringOfChar(' ',32 - Length(VolumeIdentifier)),ADescriptor.VolumeIdentifier[0],32,False) then Exit;
    {ADescriptor.Reserved2} {Must be Zero}
    ADescriptor.VolumeSpaceSize:=AVolume.SectorCount;
    ADescriptor.VolumeSpaceSizeM:=LongWordNToBE(AVolume.SectorCount);
    {ADescriptor.Reserved3} {Must be Zero}
    ADescriptor.VolumeSetSize:=1;
    ADescriptor.VolumeSetSizeM:=WordNToBE(1);
    ADescriptor.VolumeSequenceNumber:=1;
    ADescriptor.VolumeSequenceNumberM:=WordNToBE(1);
    ADescriptor.LogicalBlockSize:=AVolume.SectorSize;
    ADescriptor.LogicalBlockSizeM:=WordNToBE(AVolume.SectorSize);
    ADescriptor.PathTableSize:=PathTableSize;
    ADescriptor.PathTableSizeM:=LongWordNToBE(PathTableSize);
    ADescriptor.PrimaryPathTable:=PathTableStart;
    ADescriptor.AlternatePathTable:=0;
    ADescriptor.PrimaryPathTableM:=LongWordNToBE(PathTableStartM);
    ADescriptor.AlternatePathTableM:=LongWordNToBE(0);

    ADescriptor.RootDirectory.RecordSize:=cdfsDirectoryRecordSize;
    ADescriptor.RootDirectory.ExtendedSize:=0;
    ADescriptor.RootDirectory.FirstBlock:=RootDirectoryStart;
    ADescriptor.RootDirectory.FirstBlockM:=LongWordNToBE(RootDirectoryStart);
    ADescriptor.RootDirectory.DataSize:=RootDirectorySize;
    ADescriptor.RootDirectory.DataSizeM:=LongWordNToBE(RootDirectorySize);
    if not FileTimeToCDFSTime(DateTimeToFileTime(Now),ADescriptor.RootDirectory.CreateTime) then Exit;
    ADescriptor.RootDirectory.FileFlags:=cdfsFileFlagDirectory;
    ADescriptor.RootDirectory.UnitSize:=0;
    ADescriptor.RootDirectory.InterleaveSize:=0;
    ADescriptor.RootDirectory.SequenceNumber:=1;
    ADescriptor.RootDirectory.SequenceNumberM:=WordNToBE(1);
    ADescriptor.RootDirectory.FileIdentifierSize:=1;
    if not CDFSFileNameToIdentifier(cdfsDotName,ADescriptor.RootDirectory.FileIdentifier[0],ADescriptor.RootDirectory.FileIdentifierSize,False) then Exit;

    if not CDFSStringToIdentifier(VolumeSetIdentifier + StringOfChar(' ',128 - Length(VolumeSetIdentifier)),ADescriptor.VolumeSetIdentifier[0],128,False) then Exit;
    if not CDFSStringToIdentifier(PublisherIdentifier + StringOfChar(' ',128 - Length(PublisherIdentifier)),ADescriptor.PublisherIdentifier[0],128,False) then Exit;
    if not CDFSStringToIdentifier(PreparerIdentifier + StringOfChar(' ',128 - Length(PreparerIdentifier)),ADescriptor.PreparerIdentifier[0],128,False) then Exit;
    if not CDFSStringToIdentifier(ApplicationIdentifier + StringOfChar(' ',128 - Length(ApplicationIdentifier)),ADescriptor.ApplicationIdentifier[0],128,False) then Exit;
    if not CDFSStringToIdentifier(CopyrightIdentifier + StringOfChar(' ',37 - Length(CopyrightIdentifier)),ADescriptor.CopyrightIdentifier[0],37,False) then Exit;
    if not CDFSStringToIdentifier(AbstractIdentifier + StringOfChar(' ',37 - Length(AbstractIdentifier)),ADescriptor.AbstractIdentifier[0],37,False) then Exit;
    if not CDFSStringToIdentifier(BibliographicIdentifier + StringOfChar(' ',37 - Length(BibliographicIdentifier)),ADescriptor.BibliographicIdentifier[0],37,False) then Exit;

    if not FileTimeToCDFSDateTime(DateTimeToFileTime(Now),ADescriptor.CreateTime) then Exit;
    if not FileTimeToCDFSDateTime(DateTimeToFileTime(Now),ADescriptor.ModifyTime) then Exit;
    if not FileTimeToCDFSDateTime(cdfsNullFileTime,ADescriptor.ExpireTime) then Exit;
    if not FileTimeToCDFSDateTime(DateTimeToFileTime(Now),ADescriptor.EffectiveTime) then Exit;

    ADescriptor.FileStructureVersion:=cdfsISO9660StructureVersion;
    {ADescriptor.Reserved4} {Must be Zero}
    {ADescriptor.ApplicationData} {Must be Zero}
    {ADescriptor.Reserved5} {Must be Zero}

    Result:=True;
   end;
  ctUDF:begin
    {To be completed}
   end;
 end;
end;

{==============================================================================}

function TCDFSFormatter.WritePrimaryDescriptor(AVolume:TDiskVolume;ADrive:TDiskDrive;ACDFSType:TCDFSType;ADescriptor:PCDFSVolumeDescriptorPrimary):Boolean;
{Write the created Primary Descriptor to disk}
{Note: Caller must hold the volume lock}
var
 Cluster:LongWord;
begin
 {}
 Result:=False;

 if FDriver = nil then Exit;
 if AVolume = nil then Exit;
 if ADescriptor = nil then Exit;

 {$IFDEF CDFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFormatter.WritePrimaryDescriptor');
 {$ENDIF}
 
 {Check Type}
 case ACDFSType of
  ctISO9660,ctJOLIET:begin
    Cluster:=cdfsISO9660StartSector;
    
    {Write ReservedSectors}
    if not FillSectors(AVolume,ADrive,0,cdfsReservedSectors,0) then Exit;
    
    {Write PrimaryDescriptor}
    if not WriteSectors(AVolume,ADrive,Cluster,1,ADescriptor^) then Exit;
    
    Result:=True;
   end;
  ctUDF:begin
    {To be completed}
   end;
 end;
end;

{==============================================================================}

function TCDFSFormatter.WritePrimaryPathTables(AVolume:TDiskVolume;ADrive:TDiskDrive;ACDFSType:TCDFSType;ADescriptor:PCDFSVolumeDescriptorPrimary):Boolean;
{Note: Caller must hold the volume lock}
var
 Cluster:LongWord;
 PathRecord:PCDFSPathRecord;
begin
 {}
 Result:=False;
 
 if FDriver = nil then Exit;
 if AVolume = nil then Exit;
 if ADescriptor = nil then Exit;

 {$IFDEF CDFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFormatter.WritePrimaryPathTables');
 {$ENDIF}
 
 {Check Type}
 case ACDFSType of
  ctISO9660,ctJOLIET:begin
    {Allocate Path Table}
    PathRecord:=AllocMem(ADescriptor.LogicalBlockSize);
    if PathRecord = nil then Exit;
    try
     Cluster:=ADescriptor.PrimaryPathTable;
     
     {Create L Path Table}
     PCDFSPathRecord(PathRecord).PathIdentifierSize:=1;
     PCDFSPathRecord(PathRecord).ExtendedSize:=0;
     PCDFSPathRecord(PathRecord).FirstBlock:=ADescriptor.RootDirectory.FirstBlock;
     PCDFSPathRecord(PathRecord).ParentNumber:=1;
     if not CDFSFileNameToIdentifier(cdfsDotName,PCDFSPathRecord(PathRecord).PathIdentifier[0],PCDFSPathRecord(PathRecord).PathIdentifierSize,False) then Exit;

     {Write L Path Table}
     if not WriteSectors(AVolume,ADrive,Cluster,1,PathRecord^) then Exit;

     FillChar(PathRecord^,ADescriptor.LogicalBlockSize,0);
     Cluster:=LongWordBEtoN(ADescriptor.PrimaryPathTableM);
     
     {Create M Path Table}
     PCDFSPathRecord(PathRecord).PathIdentifierSize:=1;
     PCDFSPathRecord(PathRecord).ExtendedSize:=0;
     PCDFSPathRecord(PathRecord).FirstBlock:=LongWordNtoBE(ADescriptor.RootDirectory.FirstBlock);
     PCDFSPathRecord(PathRecord).ParentNumber:=WordNToBE(1);
     if not CDFSFileNameToIdentifier(cdfsDotName,PCDFSPathRecord(PathRecord).PathIdentifier[0],PCDFSPathRecord(PathRecord).PathIdentifierSize,False) then Exit;

     {Write M Path Table}
     if not WriteSectors(AVolume,ADrive,Cluster,1,PathRecord^) then Exit;

     Result:=True;
    finally
     FreeMem(PathRecord);
    end;
   end;
  ctUDF:begin
    {To be completed}
   end;
 end;
end;

{==============================================================================}

function TCDFSFormatter.WritePrimaryRootDirectory(AVolume:TDiskVolume;ADrive:TDiskDrive;ACDFSType:TCDFSType;ADescriptor:PCDFSVolumeDescriptorPrimary):Boolean;
{Note: Caller must hold the volume lock}
var
 Offset:LongWord;
 Cluster:LongWord;
 DirectoryRecord:PCDFSDirectoryRecord;
begin
 {}
 Result:=False;
 
 if FDriver = nil then Exit;
 if AVolume = nil then Exit;
 if ADescriptor = nil then Exit;

 {$IFDEF CDFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFormatter.WritePrimaryRootDirectory');
 {$ENDIF}
 
 {Check Type}
 case ACDFSType of
  ctISO9660,ctJOLIET:begin
    {Allocate Root Directory}
    DirectoryRecord:=AllocMem(ADescriptor.LogicalBlockSize);
    if DirectoryRecord = nil then Exit;
    try
     Cluster:=ADescriptor.RootDirectory.FirstBlock;

     {Set Offset}
     Offset:=0;
     {Create Root Directory} {Dot Record}
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).RecordSize:=ADescriptor.RootDirectory.RecordSize;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).ExtendedSize:=ADescriptor.RootDirectory.ExtendedSize;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).FirstBlock:=ADescriptor.RootDirectory.FirstBlock;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).FirstBlockM:=ADescriptor.RootDirectory.FirstBlockM;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).DataSize:=ADescriptor.RootDirectory.DataSize;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).DataSizeM:=ADescriptor.RootDirectory.DataSizeM;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).CreateTime:=ADescriptor.RootDirectory.CreateTime;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).FileFlags:=ADescriptor.RootDirectory.FileFlags;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).UnitSize:=ADescriptor.RootDirectory.UnitSize;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).InterleaveSize:=ADescriptor.RootDirectory.InterleaveSize;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).SequenceNumber:=ADescriptor.RootDirectory.SequenceNumber;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).SequenceNumberM:=ADescriptor.RootDirectory.SequenceNumberM;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).FileIdentifierSize:=ADescriptor.RootDirectory.FileIdentifierSize;
     if not CDFSFileNameToIdentifier(cdfsDotName,PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).FileIdentifier[0],ADescriptor.RootDirectory.FileIdentifierSize,False) then Exit;

     {Set Offset}
     Offset:=PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).RecordSize;
     {Create Root Directory} {DotDot Record}
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).RecordSize:=ADescriptor.RootDirectory.RecordSize;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).ExtendedSize:=ADescriptor.RootDirectory.ExtendedSize;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).FirstBlock:=ADescriptor.RootDirectory.FirstBlock;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).FirstBlockM:=ADescriptor.RootDirectory.FirstBlockM;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).DataSize:=ADescriptor.RootDirectory.DataSize;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).DataSizeM:=ADescriptor.RootDirectory.DataSizeM;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).CreateTime:=ADescriptor.RootDirectory.CreateTime;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).FileFlags:=ADescriptor.RootDirectory.FileFlags;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).UnitSize:=ADescriptor.RootDirectory.UnitSize;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).InterleaveSize:=ADescriptor.RootDirectory.InterleaveSize;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).SequenceNumber:=ADescriptor.RootDirectory.SequenceNumber;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).SequenceNumberM:=ADescriptor.RootDirectory.SequenceNumberM;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).FileIdentifierSize:=ADescriptor.RootDirectory.FileIdentifierSize;
     if not CDFSFileNameToIdentifier(cdfsDotDotName,PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).FileIdentifier[0],ADescriptor.RootDirectory.FileIdentifierSize,False) then Exit;

     {Write Root Directory} {Whole Cluster}
     if not WriteSectors(AVolume,ADrive,Cluster,1,DirectoryRecord^) then Exit;

     Result:=True;
    finally
     FreeMem(DirectoryRecord);
    end;
   end;
  ctUDF:begin
    {To be completed}
   end;
 end;
end;

{==============================================================================}

function TCDFSFormatter.CreateSupplementaryDescriptor(AVolume:TDiskVolume;ADrive:TDiskDrive;ACDFSType:TCDFSType;ADescriptor:PCDFSVolumeDescriptorSupplementary):Boolean;
{Note: Caller must hold the volume lock}
var
 PathTableSize:LongWord;
 PathTableStart:LongWord;
 PathTableStartM:LongWord;
 RootDirectorySize:LongWord;
 RootDirectoryStart:LongWord;

 SystemIdentifier:String;
 VolumeIdentifier:String;
 VolumeSetIdentifier:String;
 PublisherIdentifier:String;
 PreparerIdentifier:String;
 ApplicationIdentifier:String;
 CopyrightIdentifier:String;
 AbstractIdentifier:String;
 BibliographicIdentifier:String;
begin
 {}
 Result:=False;
 
 if FDriver = nil then Exit;
 if AVolume = nil then Exit;
 if ADescriptor = nil then Exit;

 {$IFDEF CDFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFormatter.CreateSupplementaryDescriptor');
 {$ENDIF}
 
 {Get Values}
 PathTableSize:=GetPathTableSize(AVolume,ADrive,ACDFSType,True);
 PathTableStart:=GetPathTableStart(AVolume,ADrive,ACDFSType,True,False);
 PathTableStartM:=GetPathTableStart(AVolume,ADrive,ACDFSType,True,True);
 RootDirectorySize:=GetRootDirectorySize(AVolume,ADrive,ACDFSType,True);
 RootDirectoryStart:=GetRootDirectoryStart(AVolume,ADrive,ACDFSType,True);

 SystemIdentifier:='UNKNOWN';
 VolumeIdentifier:='CDROM';
 VolumeSetIdentifier:='CDROM';
 PublisherIdentifier:='Ultibo';
 PreparerIdentifier:='Ultibo';
 ApplicationIdentifier:='Ultibo CDFS';
 CopyrightIdentifier:='';
 AbstractIdentifier:='';
 BibliographicIdentifier:='';

 {Check Type}
 case ACDFSType of
  ctJOLIET:begin
    {Create SupplementaryDescriptor}
    ADescriptor.DescriptorType:=cdfsVolumeDescriptorTypeSupplementary;
    if not CDFSStringToIdentifier(cdfsISO9660StandardIdentifier,ADescriptor.StandardIdentifier[0],5,False) then Exit;
    ADescriptor.DescriptorVersion:=cdfsISO9660DescriptorVersion;
    ADescriptor.VolumeFlags:=cdfsVolumeFlagNone;
    if not CDFSStringToIdentifier(SystemIdentifier + StringOfChar(' ',16 - Length(SystemIdentifier)),ADescriptor.SystemIdentifier[0],32,True) then Exit;
    if not CDFSStringToIdentifier(VolumeIdentifier + StringOfChar(' ',16 - Length(VolumeIdentifier)),ADescriptor.VolumeIdentifier[0],32,True) then Exit;
    {ADescriptor.Reserved2} {Must be Zero}
    ADescriptor.VolumeSpaceSize:=AVolume.SectorCount;
    ADescriptor.VolumeSpaceSizeM:=LongWordNtoBE(AVolume.SectorCount);
    if not CDFSStringToIdentifier(cdfsJolietUCS2Sequence3,ADescriptor.EscapeSequences[0],Length(cdfsJolietUCS2Sequence3),False) then Exit;
    ADescriptor.VolumeSetSize:=1;
    ADescriptor.VolumeSetSizeM:=WordNToBE(1);
    ADescriptor.VolumeSequenceNumber:=1;
    ADescriptor.VolumeSequenceNumberM:=WordNToBE(1);
    ADescriptor.LogicalBlockSize:=AVolume.SectorSize;
    ADescriptor.LogicalBlockSizeM:=WordNToBE(AVolume.SectorSize);
    ADescriptor.PathTableSize:=PathTableSize;
    ADescriptor.PathTableSizeM:=LongWordNtoBE(PathTableSize);
    ADescriptor.PrimaryPathTable:=PathTableStart;
    ADescriptor.AlternatePathTable:=0;
    ADescriptor.PrimaryPathTableM:=LongWordNtoBE(PathTableStartM);
    ADescriptor.AlternatePathTableM:=LongWordNtoBE(0);

    ADescriptor.RootDirectory.RecordSize:=cdfsDirectoryRecordSize;
    ADescriptor.RootDirectory.ExtendedSize:=0;
    ADescriptor.RootDirectory.FirstBlock:=RootDirectoryStart;
    ADescriptor.RootDirectory.FirstBlockM:=LongWordNtoBE(RootDirectoryStart);
    ADescriptor.RootDirectory.DataSize:=RootDirectorySize;
    ADescriptor.RootDirectory.DataSizeM:=LongWordNtoBE(RootDirectorySize);
    if not FileTimeToCDFSTime(DateTimeToFileTime(Now),ADescriptor.RootDirectory.CreateTime) then Exit;
    ADescriptor.RootDirectory.FileFlags:=cdfsFileFlagDirectory;
    ADescriptor.RootDirectory.UnitSize:=0;
    ADescriptor.RootDirectory.InterleaveSize:=0;
    ADescriptor.RootDirectory.SequenceNumber:=1;
    ADescriptor.RootDirectory.SequenceNumberM:=WordNToBE(1);
    ADescriptor.RootDirectory.FileIdentifierSize:=1;
    if not CDFSFileNameToIdentifier(cdfsDotName,ADescriptor.RootDirectory.FileIdentifier[0],ADescriptor.RootDirectory.FileIdentifierSize,True) then Exit;

    if not CDFSStringToIdentifier(VolumeSetIdentifier + StringOfChar(' ',64 - Length(VolumeSetIdentifier)),ADescriptor.VolumeSetIdentifier[0],128,True) then Exit;
    if not CDFSStringToIdentifier(PublisherIdentifier + StringOfChar(' ',64 - Length(PublisherIdentifier)),ADescriptor.PublisherIdentifier[0],128,True) then Exit;
    if not CDFSStringToIdentifier(PreparerIdentifier + StringOfChar(' ',64 - Length(PreparerIdentifier)),ADescriptor.PreparerIdentifier[0],128,True) then Exit;
    if not CDFSStringToIdentifier(ApplicationIdentifier + StringOfChar(' ',64 - Length(ApplicationIdentifier)),ADescriptor.ApplicationIdentifier[0],128,True) then Exit;
    if not CDFSStringToIdentifier(CopyrightIdentifier + StringOfChar(' ',18 - Length(CopyrightIdentifier)),ADescriptor.CopyrightIdentifier[0],37,True) then Exit;
    if not CDFSStringToIdentifier(AbstractIdentifier + StringOfChar(' ',18 - Length(AbstractIdentifier)),ADescriptor.AbstractIdentifier[0],37,True) then Exit;
    if not CDFSStringToIdentifier(BibliographicIdentifier + StringOfChar(' ',18 - Length(BibliographicIdentifier)),ADescriptor.BibliographicIdentifier[0],37,True) then Exit;

    if not FileTimeToCDFSDateTime(DateTimeToFileTime(Now),ADescriptor.CreateTime) then Exit;
    if not FileTimeToCDFSDateTime(DateTimeToFileTime(Now),ADescriptor.ModifyTime) then Exit;
    if not FileTimeToCDFSDateTime(cdfsNullFileTime,ADescriptor.ExpireTime) then Exit;
    if not FileTimeToCDFSDateTime(DateTimeToFileTime(Now),ADescriptor.EffectiveTime) then Exit;

    ADescriptor.FileStructureVersion:=cdfsISO9660StructureVersion;
    {ADescriptor.Reserved4} {Must be Zero}
    {ADescriptor.ApplicationData} {Must be Zero}
    {ADescriptor.Reserved5} {Must be Zero}

    Result:=True;
   end;
  ctUDF:begin
    {To be completed}
   end;
 end;
end;

{==============================================================================}

function TCDFSFormatter.WriteSupplementaryDescriptor(AVolume:TDiskVolume;ADrive:TDiskDrive;ACDFSType:TCDFSType;ADescriptor:PCDFSVolumeDescriptorSupplementary):Boolean;
{Note: Caller must hold the volume lock}
var
 Cluster:LongWord;
begin
 {}
 Result:=False;
 
 if FDriver = nil then Exit;
 if AVolume = nil then Exit;
 if ADescriptor = nil then Exit;

 {$IFDEF CDFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFormatter.WriteSupplementaryDescriptor');
 {$ENDIF}
 
 {Check Type}
 case ACDFSType of
  ctJOLIET:begin
    Cluster:=17;
    
    {Write SupplementaryDescriptor}
    if not WriteSectors(AVolume,ADrive,Cluster,1,ADescriptor^) then Exit;
    
    Result:=True;
   end;
  ctUDF:begin
    {To be completed}
   end;
 end;
end;

{==============================================================================}

function TCDFSFormatter.WriteSupplementaryPathTables(AVolume:TDiskVolume;ADrive:TDiskDrive;ACDFSType:TCDFSType;ADescriptor:PCDFSVolumeDescriptorSupplementary):Boolean;
{Note: Caller must hold the volume lock}
var
 Cluster:LongWord;
 PathRecord:PCDFSPathRecord;
begin
 {}
 Result:=False;
 
 if FDriver = nil then Exit;
 if AVolume = nil then Exit;
 if ADescriptor = nil then Exit;

 {$IFDEF CDFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFormatter.WriteSupplementaryPathTables');
 {$ENDIF}
 
 {Check Type}
 case ACDFSType of
  ctJOLIET:begin
    {Allocate Path Table}
    PathRecord:=AllocMem(ADescriptor.LogicalBlockSize);
    if PathRecord = nil then Exit;
    try
     Cluster:=ADescriptor.PrimaryPathTable;
     
     {Create L Path Table}
     PCDFSPathRecord(PathRecord).PathIdentifierSize:=1;
     PCDFSPathRecord(PathRecord).ExtendedSize:=0;
     PCDFSPathRecord(PathRecord).FirstBlock:=ADescriptor.RootDirectory.FirstBlock;
     PCDFSPathRecord(PathRecord).ParentNumber:=1;
     if not CDFSFileNameToIdentifier(cdfsDotName,PCDFSPathRecord(PathRecord).PathIdentifier[0],PCDFSPathRecord(PathRecord).PathIdentifierSize,True) then Exit;

     {Write L Path Table}
     if not WriteSectors(AVolume,ADrive,Cluster,1,PathRecord^) then Exit;

     FillChar(PathRecord^,ADescriptor.LogicalBlockSize,0);
     Cluster:=LongWordBEtoN(ADescriptor.PrimaryPathTableM);
     
     {Create M Path Table}
     PCDFSPathRecord(PathRecord).PathIdentifierSize:=1;
     PCDFSPathRecord(PathRecord).ExtendedSize:=0;
     PCDFSPathRecord(PathRecord).FirstBlock:=LongWordNtoBE(ADescriptor.RootDirectory.FirstBlock);
     PCDFSPathRecord(PathRecord).ParentNumber:=WordNToBE(1);
     if not CDFSFileNameToIdentifier(cdfsDotName,PCDFSPathRecord(PathRecord).PathIdentifier[0],PCDFSPathRecord(PathRecord).PathIdentifierSize,True) then Exit;

     {Write M Path Table}
     if not WriteSectors(AVolume,ADrive,Cluster,1,PathRecord^) then Exit;

     Result:=True;
    finally
     FreeMem(PathRecord);
    end;
   end;
  ctUDF:begin
    {To be completed}
   end;
 end;
end;

{==============================================================================}

function TCDFSFormatter.WriteSupplementaryRootDirectory(AVolume:TDiskVolume;ADrive:TDiskDrive;ACDFSType:TCDFSType;ADescriptor:PCDFSVolumeDescriptorSupplementary):Boolean;
{Note: Caller must hold the volume lock}
var
 Offset:LongWord;
 Cluster:LongWord;
 DirectoryRecord:PCDFSDirectoryRecord;
begin
 {}
 Result:=False;
 
 if FDriver = nil then Exit;
 if AVolume = nil then Exit;
 if ADescriptor = nil then Exit;

 {$IFDEF CDFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFormatter.WriteSupplementaryRootDirectory');
 {$ENDIF}
 
 {Check Type}
 case ACDFSType of
  ctJOLIET:begin
    {Allocate Root Directory}
    DirectoryRecord:=AllocMem(ADescriptor.LogicalBlockSize);
    if DirectoryRecord = nil then Exit;
    try
     Cluster:=ADescriptor.RootDirectory.FirstBlock;

     {Set Offset}
     Offset:=0;
     
     {Create Root Directory} {Dot Record}
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).RecordSize:=ADescriptor.RootDirectory.RecordSize;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).ExtendedSize:=ADescriptor.RootDirectory.ExtendedSize;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).FirstBlock:=ADescriptor.RootDirectory.FirstBlock;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).FirstBlockM:=ADescriptor.RootDirectory.FirstBlockM;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).DataSize:=ADescriptor.RootDirectory.DataSize;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).DataSizeM:=ADescriptor.RootDirectory.DataSizeM;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).CreateTime:=ADescriptor.RootDirectory.CreateTime;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).FileFlags:=ADescriptor.RootDirectory.FileFlags;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).UnitSize:=ADescriptor.RootDirectory.UnitSize;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).InterleaveSize:=ADescriptor.RootDirectory.InterleaveSize;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).SequenceNumber:=ADescriptor.RootDirectory.SequenceNumber;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).SequenceNumberM:=ADescriptor.RootDirectory.SequenceNumberM;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).FileIdentifierSize:=ADescriptor.RootDirectory.FileIdentifierSize;
     if not CDFSFileNameToIdentifier(cdfsDotName,PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).FileIdentifier[0],ADescriptor.RootDirectory.FileIdentifierSize,True) then Exit;

     {Set Offset}
     Offset:=PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).RecordSize;
     
     {Create Root Directory} {DotDot Record}
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).RecordSize:=ADescriptor.RootDirectory.RecordSize;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).ExtendedSize:=ADescriptor.RootDirectory.ExtendedSize;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).FirstBlock:=ADescriptor.RootDirectory.FirstBlock;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).FirstBlockM:=ADescriptor.RootDirectory.FirstBlockM;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).DataSize:=ADescriptor.RootDirectory.DataSize;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).DataSizeM:=ADescriptor.RootDirectory.DataSizeM;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).CreateTime:=ADescriptor.RootDirectory.CreateTime;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).FileFlags:=ADescriptor.RootDirectory.FileFlags;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).UnitSize:=ADescriptor.RootDirectory.UnitSize;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).InterleaveSize:=ADescriptor.RootDirectory.InterleaveSize;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).SequenceNumber:=ADescriptor.RootDirectory.SequenceNumber;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).SequenceNumberM:=ADescriptor.RootDirectory.SequenceNumberM;
     PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).FileIdentifierSize:=ADescriptor.RootDirectory.FileIdentifierSize;
     if not CDFSFileNameToIdentifier(cdfsDotDotName,PCDFSDirectoryRecord(PtrUInt(DirectoryRecord) + Offset).FileIdentifier[0],ADescriptor.RootDirectory.FileIdentifierSize,True) then Exit;

     {Write Root Directory} {Whole Cluster}
     if not WriteSectors(AVolume,ADrive,Cluster,1,DirectoryRecord^) then Exit;

     Result:=True;
    finally
     FreeMem(DirectoryRecord);
    end;
   end;
  ctUDF:begin
    {To be completed}
   end;
 end;
end;

{==============================================================================}

function TCDFSFormatter.CreateTerminatorDescriptor(AVolume:TDiskVolume;ADrive:TDiskDrive;ACDFSType:TCDFSType;ADescriptor:PCDFSVolumeDescriptorTerminator):Boolean;
{Note: Caller must hold the volume lock}
begin
 {}
 Result:=False;
 
 if FDriver = nil then Exit;
 if AVolume = nil then Exit;
 if ADescriptor = nil then Exit;

 {$IFDEF CDFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFormatter.CreateTerminatorDescriptor');
 {$ENDIF}
 
 {Check Type}
 case ACDFSType of
  ctISO9660,ctJOLIET:begin
    {Create TerminatorDescriptor}
    ADescriptor.DescriptorType:=cdfsVolumeDescriptorTypeTerminator;
    if not CDFSStringToIdentifier(cdfsISO9660StandardIdentifier,ADescriptor.StandardIdentifier[0],5,False) then Exit;
    ADescriptor.DescriptorVersion:=cdfsISO9660DescriptorVersion;
    {ADescriptor.Reserved} {Must be Zero}

    Result:=True;
   end;
  ctUDF:begin
    {To be completed}
   end;
 end;
end;

{==============================================================================}

function TCDFSFormatter.WriteTerminatorDescriptor(AVolume:TDiskVolume;ADrive:TDiskDrive;ACDFSType:TCDFSType;ADescriptor:PCDFSVolumeDescriptorTerminator):Boolean;
{Note: Caller must hold the volume lock}
var
 Cluster:LongWord;
begin
 {}
 Result:=False;
 
 if FDriver = nil then Exit;
 if AVolume = nil then Exit;
 if ADescriptor = nil then Exit;

 {$IFDEF CDFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFormatter.WriteTerminatorDescriptor');
 {$ENDIF}
 
 {Check Type}
 case ACDFSType of
  ctISO9660:begin
    Cluster:=17;
    
    {Write TerminatorDescriptor}
    if not WriteSectors(AVolume,ADrive,Cluster,1,ADescriptor^) then Exit;
    
    Result:=True;
   end;
  ctJOLIET:begin
    Cluster:=18;
    
    {Write TerminatorDescriptor}
    if not WriteSectors(AVolume,ADrive,Cluster,1,ADescriptor^) then Exit;
    
    Result:=True;
   end;
  ctUDF:begin
    {To be completed}
   end;
 end;
end;

{==============================================================================}

function TCDFSFormatter.AcceptVolume(AVolume:TDiskVolume;AFloppyType:TFloppyType;AFileSysType:TFileSysType):Boolean;
{Note: Caller must hold the volume lock}
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FRecognizer = nil then Exit;
  if AVolume = nil then Exit;
  if AVolume.Device = nil then Exit;

  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFormatter.AcceptVolume VolumeName = ' + AVolume.Name + ' SectorSize = ' + IntToStr(AVolume.SectorSize) + ' SectorCount = ' + IntToStr(AVolume.SectorCount));
  {$ENDIF}
  
  {Accept Format Volume}
  {Check Volume}
  if AVolume.SectorCount = 0 then Exit;
  if AVolume.SectorSize <> ISO_SECTOR_SIZE then Exit;
  
  {Check Device}
  if not CheckDevice(AVolume,nil,AFloppyType) then Exit;
  
  {Check Partition}
  if not CheckPartition(AVolume,nil,AFileSysType) then Exit;
  
  Result:=True;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TCDFSFormatter.FormatVolume(AVolume:TDiskVolume;AFloppyType:TFloppyType;AFileSysType:TFileSysType):Boolean;
{Note: Caller must hold the volume writer lock}
var
 DriveNo:Integer;
 Drive:TDiskDrive;
 CDFSType:TCDFSType;
 PrimaryDescriptor:PCDFSVolumeDescriptorPrimary;
 TerminatorDescriptor:PCDFSVolumeDescriptorTerminator;
 SupplementaryDescriptor:PCDFSVolumeDescriptorSupplementary;
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FRecognizer = nil then Exit;
  if AVolume = nil then Exit;
  if AVolume.Device = nil then Exit;

  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFormatter.FormatVolume VolumeName = ' + AVolume.Name + ' SectorSize = ' + IntToStr(AVolume.SectorSize) + ' SectorCount = ' + IntToStr(AVolume.SectorCount));
  {$ENDIF}
  
  {Check Accepted}
  if not AcceptVolume(AVolume,AFloppyType,AFileSysType) then Exit;

  {Get DriveNo}
  DriveNo:=INVALID_DRIVE;
  Drive:=FDriver.GetDriveByVolume(AVolume,True,FILESYS_LOCK_READ);
  if Drive <> nil then
   begin
    DriveNo:=Drive.DriveNo;
    
    {Unlock Drive}
    Drive.ReaderUnlock;
   end;  

  {Dismount Volume}
  AVolume.DismountVolume;

  {Allocate Primary Descriptor}
  PrimaryDescriptor:=AllocMem(SizeOf(TCDFSVolumeDescriptorPrimary));
  if PrimaryDescriptor = nil then Exit;
  try
   {Allocate Terminator Descriptor}
   TerminatorDescriptor:=AllocMem(SizeOf(TCDFSVolumeDescriptorTerminator));
   if TerminatorDescriptor = nil then Exit;
   try
    {Get CDFS Type}
    CDFSType:=ctISO9660;
    if TCDFSRecognizer(FRecognizer).FLongNames then CDFSType:=ctJOLIET;
    if AFileSysType = fsUDF then CDFSType:=ctUDF;
    
    {Check CDFS Type}
    case CDFSType of
     ctISO9660:begin
       {Create Primary Descriptor}
       if not CreatePrimaryDescriptor(AVolume,nil,CDFSType,PrimaryDescriptor) then Exit;
       
       {Write Primary Descriptor}
       if not WritePrimaryDescriptor(AVolume,nil,CDFSType,PrimaryDescriptor) then Exit;
       
       {Write Primary Path Tables}
       if not WritePrimaryPathTables(AVolume,nil,CDFSType,PrimaryDescriptor) then Exit;
       
       {Write Primary Root Directory}
       if not WritePrimaryRootDirectory(AVolume,nil,CDFSType,PrimaryDescriptor) then Exit;

       {Create Terminator Descriptor}
       if not CreateTerminatorDescriptor(AVolume,nil,CDFSType,TerminatorDescriptor) then Exit;
       
       {Write Terminator Descriptor}
       if not WriteTerminatorDescriptor(AVolume,nil,CDFSType,TerminatorDescriptor) then Exit;

       {Mount Volume}
       if not AVolume.MountVolume(DriveNo) then Exit;

       Result:=True;
      end;
     ctJOLIET:begin
       {Allocate Supplementary Descriptor}
       SupplementaryDescriptor:=AllocMem(SizeOf(TCDFSVolumeDescriptorSupplementary));
       if SupplementaryDescriptor = nil then Exit;
       try
        {Create Primary Descriptor}
        if not CreatePrimaryDescriptor(AVolume,nil,CDFSType,PrimaryDescriptor) then Exit;
        
        {Write Primary Descriptor}
        if not WritePrimaryDescriptor(AVolume,nil,CDFSType,PrimaryDescriptor) then Exit;
        
        {Write Primary Path Tables}
        if not WritePrimaryPathTables(AVolume,nil,CDFSType,PrimaryDescriptor) then Exit;
        
        {Write Primary Root Directory}
        if not WritePrimaryRootDirectory(AVolume,nil,CDFSType,PrimaryDescriptor) then Exit;

        {Create Supplementary Descriptor}
        if not CreateSupplementaryDescriptor(AVolume,nil,CDFSType,SupplementaryDescriptor) then Exit;
        
        {Write Supplementary Descriptor}
        if not WriteSupplementaryDescriptor(AVolume,nil,CDFSType,SupplementaryDescriptor) then Exit;
        
        {Write Supplementary Path Tables}
        if not WriteSupplementaryPathTables(AVolume,nil,CDFSType,SupplementaryDescriptor) then Exit;
        
        {Write Supplementary Root Directory}
        if not WriteSupplementaryRootDirectory(AVolume,nil,CDFSType,SupplementaryDescriptor) then Exit;

        {Create Terminator Descriptor}
        if not CreateTerminatorDescriptor(AVolume,nil,CDFSType,TerminatorDescriptor) then Exit;
        
        {Write Terminator Descriptor}
        if not WriteTerminatorDescriptor(AVolume,nil,CDFSType,TerminatorDescriptor) then Exit;

        {Mount Volume}
        if not AVolume.MountVolume(DriveNo) then Exit;

        Result:=True;
       finally
        FreeMem(SupplementaryDescriptor);
       end;
      end;
     ctUDF:begin
       {To be completed}
       
       {Mount Volume}
       if not AVolume.MountVolume(DriveNo) then Exit;
       
       {Result:=True;}
      end;
    end;
   finally
    FreeMem(TerminatorDescriptor);
   end;
  finally
   FreeMem(PrimaryDescriptor);
  end;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{TCDFSResizer}
function TCDFSResizer.AcceptVolume(AVolume:TDiskVolume;const AStart,ASize:Int64):Boolean;
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FRecognizer = nil then Exit;
  if AVolume = nil then Exit;
  if AVolume.Device = nil then Exit;

  //To Do //
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TCDFSResizer.ShrinkVolume(AVolume:TDiskVolume;const AStart,ASize:Int64):Boolean;
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FRecognizer = nil then Exit;
  if AVolume = nil then Exit;
  if AVolume.Device = nil then Exit;

  {Check Size}
  if ASize = 0 then Exit;

  {Check Accepted}
  if not AcceptVolume(AVolume,AStart,ASize) then Exit;

  //To Do //
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TCDFSResizer.ExpandVolume(AVolume:TDiskVolume;const AStart,ASize:Int64):Boolean;
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FRecognizer = nil then Exit;
  if AVolume = nil then Exit;
  if AVolume.Device = nil then Exit;

  {Check Size}
  if ASize = 0 then Exit;

  {Check Accepted}
  if not AcceptVolume(AVolume,AStart,ASize) then Exit;

  //To Do //
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{TCDFSCopier}
function TCDFSCopier.AcceptVolume(AVolume,ADest:TDiskVolume):Boolean;
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FRecognizer = nil then Exit;
  if AVolume = nil then Exit;
  if AVolume.Device = nil then Exit;

  //To Do //
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TCDFSCopier.CopyVolume(AVolume,ADest:TDiskVolume):Boolean;
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FRecognizer = nil then Exit;
  if AVolume = nil then Exit;
  if AVolume.Device = nil then Exit;

  {Check Destination}
  if ADest = nil then Exit;
  if ADest.Device = nil then Exit;

  {Check Accepted}
  if not AcceptVolume(AVolume,ADest) then Exit;

  //To Do //
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{TCDFSFileSystem}
constructor TCDFSFileSystem.Create(ADriver:TFileSysDriver;AVolume:TDiskVolume;ADrive:TDiskDrive);
begin
 {}
 inherited Create(ADriver,AVolume,ADrive);
 FCDFSType:=ctNONE;
 FVolumeFlags:=cdfsVolumeFlagNone;

 FSwapSerial:=False;   {Set to True for Win9x compatibility}

 FReadOnly:=True;      {Default to True, can be False on ISO image}
 FLongNames:=True;     {Default to True, can be False if not Joliet}
 FDataStreams:=False;
 FReparsePoints:=False;
 FCaseSensitive:=False;
 FCasePreserved:=True; {Enabled only if LongNames Enabled}
 FUnicodeNames:=True;  {Enabled only if LongNames Enabled}
 FPersistentAcls:=False;
 FFileCompression:=False;
 FVolumeQuotas:=False;
 FSparseFiles:=False;
 FRemoteStorage:=False;
 FVolumeCompressed:=False;
 FObjectIds:=False;
 FEncryption:=False;

 FBootCatalog:=False;  {Default to False, only set to True if boot descriptor loaded or Writeable}
 FVirtualVolume:=False;
 FFolderEncryption:=False;
 FFolderCompression:=False;

 FLastFreeCluster:=cdfsUnknownCluster;
 FFreeClusterCount:=cdfsUnknownCluster;

 FBoot:=nil;
 FPrimary:=nil;
 FSupplementary:=nil;

 FHeaders:=TFileSysList.Create;
 FExtensions:=TFileSysList.Create;
 FDescriptors:=TFileSysList.Create;

 FHeaderLocal:=MutexCreate;
 FExtensionLocal:=MutexCreate;
 FDescriptorLocal:=MutexCreate;
 
 FExtendedLocal:=MutexCreate;
 
 FPathLock:=SynchronizerCreate;
 
 FReadBuffer:=nil;
 FReadLock:=MutexCreate;

 FWriteBuffer:=nil;
 FWriteLock:=MutexCreate;
 
 FClusterBuffer:=nil;
 FClusterLock:=MutexCreate;
end;

{==============================================================================}

destructor TCDFSFileSystem.Destroy;
begin
 {}
 WriterLock;
 try
  if FClusterBuffer <> nil then FreeMem(FClusterBuffer);
  FClusterBuffer:=nil;
  MutexDestroy(FClusterLock);
 
  if FWriteBuffer <> nil then FreeMem(FWriteBuffer);
  FWriteBuffer:=nil;
  MutexDestroy(FWriteLock);

  if FReadBuffer <> nil then FreeMem(FReadBuffer);
  FReadBuffer:=nil;
  MutexDestroy(FReadLock);
 
  FDescriptors.Free;
  FExtensions.Free;
  FHeaders.Free;

  SynchronizerDestroy(FPathLock);
  
  MutexDestroy(FExtendedLocal);
  
  MutexDestroy(FDescriptorLocal);
  MutexDestroy(FExtensionLocal);
  MutexDestroy(FHeaderLocal);
  
  FSupplementary:=nil;
  FPrimary:=nil;
  FBoot:=nil;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  inherited Destroy;
 end; 
end;

{==============================================================================}

function TCDFSFileSystem.ReadLock:Boolean;
begin
 {}
 Result:=(MutexLock(FReadLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TCDFSFileSystem.ReadUnlock:Boolean;
begin
 {}
 Result:=(MutexUnlock(FReadLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TCDFSFileSystem.WriteLock:Boolean;
begin
 {}
 Result:=(MutexLock(FWriteLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TCDFSFileSystem.WriteUnlock:Boolean;
begin
 {}
 Result:=(MutexUnlock(FWriteLock) = ERROR_SUCCESS);
end;

{=============================================================================}

function TCDFSFileSystem.ClusterLock:Boolean;
begin
 {}
 Result:=(MutexLock(FClusterLock) = ERROR_SUCCESS);
end;

{=============================================================================}

function TCDFSFileSystem.ClusterUnlock:Boolean;
begin
 {}
 Result:=(MutexUnlock(FClusterLock) = ERROR_SUCCESS);
end;

{=============================================================================}

function TCDFSFileSystem.LoadTree(AEntry:TDiskEntry):Boolean;
{Load all Entries in a given Tree}
{Note: Should only by called by LoadTrees}
var
 Entry:TDiskEntry;
begin
 {}
 Result:=False;
 
 if not FEntries.WriterLock then Exit;
 try
  if AEntry = nil then Exit;

  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.LoadTree - Entry = ' + AEntry.Name);
  {$ENDIF}
 
  {Load Parent}
  if not LoadEntries(AEntry) then Exit;
 
  {Load Children}
  Entry:=TDiskEntry(AEntry.FirstChild);
  while Entry <> nil do
   begin
    if (Entry.Attributes and (faDot or faDotDot)) = faNone then
     begin
      if not LoadTree(Entry) then Exit;
     end;
    
    Entry:=TDiskEntry(Entry.Next);
   end;
  
  Result:=True;
 finally
  FEntries.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.MarkTree(AEntry:TDiskEntry):Boolean;
{Note: Should only by called by LoadBlocks}
var
 Entry:TDiskEntry;
begin
 {}
 Result:=False;
 
 if not FEntries.WriterLock then Exit;
 try
  if AEntry = nil then Exit;

  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.MarkTree - Entry = ' + AEntry.Name);
  {$ENDIF}
 
  {Mark Parent}
  if TCDFSDiskEntry(AEntry).StartCluster > 0 then {No need to check ClusterCount as MarkCluster returns success on zero count}
   begin
    if not MarkClusters(TCDFSDiskEntry(AEntry).StartCluster,TCDFSDiskEntry(AEntry).ClusterCount,True) then Exit;
   end;
  
  {Mark Children}
  Entry:=TDiskEntry(AEntry.FirstChild);
  while Entry <> nil do
   begin
    if (Entry.Attributes and (faDot or faDotDot)) = faNone then
     begin
      if not MarkTree(Entry) then Exit;
     end;
    
    Entry:=TDiskEntry(Entry.Next);
   end;
  
  Result:=True;
 finally
  FEntries.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.PrepareTree(AEntry:TDiskEntry):Boolean;
{Note: Should only by called by PrepareTrees}
var
 Entry:TDiskEntry;
 Current:TDiskEntry;
begin
 {}
 Result:=False;
 
 if not FEntries.WriterLock then Exit;
 try
  if AEntry = nil then Exit;

  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.PrepareTree - Entry = ' + AEntry.Name);
  {$ENDIF}
 
  Entry:=TDiskEntry(AEntry.FirstChild);
  while Entry <> nil do
   begin
    Current:=Entry;
    Entry:=TDiskEntry(Entry.Next);
   
    if (Current.Attributes and faDirectory) = faDirectory then
     begin
      if (Current.Attributes and (faDot or faDotDot)) = faNone then
       begin
        if not PrepareTree(Current) then Exit;
        if not ReleaseClusters(TCDFSDiskEntry(Current).StartCluster,TCDFSDiskEntry(Current).ClusterCount) then Exit;
        if not FillClusters(TCDFSDiskEntry(Current).StartCluster,TCDFSDiskEntry(Current).ClusterCount,0) then Exit;
       end;
     end;
    
    {Remove Entry}
    FEntries.Remove(Current);
   
    {Schedule Entry}
    FDriver.ScheduleEntry(Current,FILESYS_ENTRY_DELETE_TIMEOUT);
   end;
  
  Result:=True;
 finally
  FEntries.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.PrepareTable(ATable:TDiskTable):Boolean;
{Note: Should only by called by PrepareTrees}
{Note: Caller must hold the tables lock}
var
 Path:TCDFSDiskPath;
 Current:TCDFSDiskPath;
begin
 {}
 Result:=False;
 
 if ATable = nil then Exit;
 if TCDFSDiskTable(ATable).Descriptor = nil then Exit;

 if not TCDFSDiskTable(ATable).Paths.WriterLock then Exit;
 try
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.PrepareTable - TableNo = ' + IntToHex(ATable.TableNo,8));
  {$ENDIF}
 
  Path:=TCDFSDiskPath(TCDFSDiskTable(ATable).Paths.First);
  while Path <> nil do
   begin
    Current:=Path;
    Path:=TCDFSDiskPath(Path.Next);
   
    if Current.Name <> cdfsDotName then
     begin
      {Update Descriptor} {Only on Primary Table}
      if ATable = TCDFSDiskTable(ATable).Descriptor.Primary then
       begin
        TCDFSDiskTable(ATable).Descriptor.PathTableSize:=(TCDFSDiskTable(ATable).Descriptor.PathTableSize - Current.PathRecordSize(TCDFSDiskTable(ATable).Unicode));
       end;
     
      TCDFSDiskTable(ATable).Paths.Remove(Current);
     
      Current.Free;
     end;
   end;
  
  Result:=SetPaths(TCDFSDiskTable(ATable));
 finally
  TCDFSDiskTable(ATable).Paths.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.PrepareTrees:Boolean;
{Prepare Trees releases all entries and paths in the Primary Descriptor except Root}
{Releasing the entry or path simply implies setting the bytes on disk to zero}
{Uses PrepareTree and PrepareTable above}
{Note: Should only be called by Add/Remove/Rename/MovePath/Entry}
var
 Entry:TDiskEntry;
 Current:TDiskEntry;
begin
 {}
 Result:=False;
 
 {Check Prepared}
 if not FTreesPrepared then
  begin
   if not FEntries.WriterLock then Exit;
   try
    {Check Prepared (After Lock)}
    if not FTreesPrepared then
     begin
      if not FDescriptors.WriterLock then Exit;
      try
       if FPrimary = nil then Exit;
       
       if not FTables.ReaderLock then Exit;
       try
        {$IFDEF CDFS_DEBUG}
        if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.PrepareTrees');
        {$ENDIF}
       
        {Load Trees}
        if not LoadTrees then Exit;
      
        {Check Descriptor}
        if FSupplementary <> nil then
         begin
          {Prepare Entries}
          if FPrimary.Root = nil then Exit;
        
          Entry:=TDiskEntry(FPrimary.Root.FirstChild);
          while Entry <> nil do
           begin
            Current:=Entry;
            Entry:=TDiskEntry(Entry.Next);
           
            if (Current.Attributes and (faDot or faDotDot)) = faNone then
             begin
              if (Current.Attributes and faDirectory) = faDirectory then
               begin
                if not PrepareTree(Current) then Exit;
                if not ReleaseClusters(TCDFSDiskEntry(Current).StartCluster,TCDFSDiskEntry(Current).ClusterCount) then Exit;
                if not FillClusters(TCDFSDiskEntry(Current).StartCluster,TCDFSDiskEntry(Current).ClusterCount,0) then Exit;
               end;
             
              {Remove Entry}
              FEntries.Remove(Current);
             
              {Schedule Entry}
              FDriver.ScheduleEntry(Current,FILESYS_ENTRY_DELETE_TIMEOUT);
             end;
           end;
          
          if not SetEntries(FPrimary.Root) then Exit;
         
          {Prepare Tables}
          if FPrimary.Primary <> nil then
           begin
            if not PrepareTable(FPrimary.Primary) then Exit; {SetPaths will zero fill all existing clusters}
            if FPrimary.Primary.ClusterCount > 1 then if not ReleaseClusters(FPrimary.Primary.StartCluster + 1,FPrimary.Primary.ClusterCount - 1) then Exit;
           
            {Update Table}
            FPrimary.Primary.ClusterCount:=1;
            FPrimary.Primary.DataFree:=0;
            FPrimary.Primary.DataSize:=(FPrimary.Primary.ClusterCount shl FClusterShiftCount);
           end;
           
          if FPrimary.PrimaryM <> nil then
           begin
            if not PrepareTable(FPrimary.PrimaryM) then Exit; {SetPaths will zero fill all existing clusters}
            if FPrimary.PrimaryM.ClusterCount > 1 then if not ReleaseClusters(FPrimary.PrimaryM.StartCluster + 1,FPrimary.PrimaryM.ClusterCount - 1) then Exit;
          
            {Update Table}
            FPrimary.PrimaryM.ClusterCount:=1;
            FPrimary.PrimaryM.DataFree:=0;
            FPrimary.PrimaryM.DataSize:=(FPrimary.PrimaryM.ClusterCount shl FClusterShiftCount);
           end;
          
          if FPrimary.Alternate <> nil then
           begin
            if not PrepareTable(FPrimary.Alternate) then Exit; {SetPaths will zero fill all existing clusters}
            if FPrimary.Alternate.ClusterCount > 1 then if not ReleaseClusters(FPrimary.Alternate.StartCluster + 1,FPrimary.Alternate.ClusterCount - 1) then Exit;
           
            {Update Table}
            FPrimary.Alternate.ClusterCount:=1;
            FPrimary.Alternate.DataFree:=0;
            FPrimary.Alternate.DataSize:=(FPrimary.Alternate.ClusterCount shl FClusterShiftCount);
           end;
          
          if FPrimary.AlternateM <> nil then
           begin
            if not PrepareTable(FPrimary.AlternateM) then Exit; {SetPaths will zero fill all existing clusters}
            if FPrimary.AlternateM.ClusterCount > 1 then if not ReleaseClusters(FPrimary.AlternateM.StartCluster + 1,FPrimary.AlternateM.ClusterCount - 1) then Exit;
           
            {Update Table}
            FPrimary.AlternateM.ClusterCount:=1;
            FPrimary.AlternateM.DataFree:=0;
            FPrimary.AlternateM.DataSize:=(FPrimary.AlternateM.ClusterCount shl FClusterShiftCount);
           end;
          
          {Set Descriptor}
          if not SetDescriptor(FPrimary) then Exit;
         end;
      
        FTreesPrepared:=True;
       finally
        FTables.ReaderUnlock;
       end;       
      finally
       FDescriptors.WriterUnlock;
      end; 
     end;
   finally
    FEntries.WriterUnlock;
   end; 
  end;
  
 Result:=True;
end;

{=============================================================================}

function TCDFSFileSystem.CheckCatalogs:Boolean;
{Check Catalogs checks all catalogs for a non zero cluster count}
{If any catalogs have zero clusters allocated then LoadBlocks will not present an
 accurate map of the disk and AllocClusters cannot reliably assign clusters}
{Note: Should only be called by AllocClusters}
var
 Catalog:TCDFSDiskCatalog;
begin
 {}
 Result:=False;

 {Check Checked}
 if not FCatalogsChecked then
  begin
   if not FCatalogs.WriterLock then Exit;
   try
    {Check Checked(After Lock)}
    if not FCatalogsChecked then
     begin
      {$IFDEF CDFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.CheckCatalogs');
      {$ENDIF}
   
      {Load Catalogs}
      if not LoadCatalogs then Exit;
   
      {Check Catalogs}
      Catalog:=TCDFSDiskCatalog(FCatalogs.First);
      while Catalog <> nil do
       begin
        if Catalog.ClusterCount = 0 then Exit;
     
        Catalog:=TCDFSDiskCatalog(Catalog.Next);
       end;
    
      FCatalogsChecked:=True;
     end;
   finally
    FCatalogs.WriterUnlock;
   end; 
  end;
  
 Result:=True;
end;

{=============================================================================}

function TCDFSFileSystem.LocatePath(ATable:TCDFSDiskTable;APathNumber:Word):TCDFSDiskPath;
{Note: Should only be called by LoadPath, AddPath and MovePath}
{Note: Caller must hold the tables and paths lock}

var
 Path:TCDFSDiskPath;
begin
 {}
 Result:=nil;
 
 if FDriver = nil then Exit;
 if ATable = nil then Exit;

 {Load Paths}
 {if not LoadPaths(TCDFSDiskTable(ATable)) then Exit;} {Do not call, called by LoadPath}
 
 {Check Paths}
 Path:=TCDFSDiskPath(TCDFSDiskTable(ATable).Paths.First); {Protected by caller held paths lock}
 while Path <> nil do
  begin
   if Path.PathNumber = APathNumber then
    begin
     Result:=Path;
     Exit;
    end;
    
   Path:=TCDFSDiskPath(Path.Next); {Protected by caller held paths lock}
  end;
end;

{=============================================================================}

function TCDFSFileSystem.LocateEntry(AEntry:TCDFSDiskEntry;AStartCluster:LongWord):TCDFSDiskEntry;
{Note: Should only be called by LoadCatalog}
{Note: Caller must hold the entries lock}
var
 Entry:TCDFSDiskEntry;
begin
 {}
 Result:=nil;
 
 //To Do //Lock ?
 
 if FDriver = nil then Exit;
 if AEntry = nil then Exit;

 Entry:=AEntry;
 while Entry <> nil do
  begin
   if (Entry.Attributes and (faDot or faDotDot)) = faNone then
    begin
     {Check Entry}
     if Entry.StartCluster = AStartCluster then
      begin
       Result:=Entry;
       Exit;
      end;
      
     {Check Children}
     if Entry.FirstChild <> nil then
      begin
       Result:=LocateEntry(TCDFSDiskEntry(Entry.FirstChild),AStartCluster);
       if Result <> nil then Exit;
      end;
    end;
    
   Entry:=TCDFSDiskEntry(Entry.Next);
  end;
end;

{=============================================================================}

function TCDFSFileSystem.LoadVolumeFlags:LongWord;
begin
 {}
 Result:=0;
 
 if FSupplementary = nil then Exit;
 
 Result:=FSupplementary.VolumeFlags;
end;

{=============================================================================}

function TCDFSFileSystem.FillClusters(ACluster:LongWord;ACount:Word;AValue:Byte):Boolean;
{Fill count clusters with the supplied value}
var
 Count:Word;
 Cluster:LongWord;
begin
 {}
 Result:=False;
 
 if ACount = 0 then Exit;

 if not ClusterLock then Exit; 
 try
  if FClusterBuffer = nil then Exit;

  {Setup Count}
  Count:=ACount;
  Cluster:=ACluster;
  while Count > 0 do
   begin
    {Read Cluster} {Don't need to read first}
    {if not ReadClusters(Cluster,1,FClusterBuffer^) then Exit;}
   
    {Fill Cluster}
    FillChar(FClusterBuffer^,FClusterSize,AValue);
   
    {Write Cluster}
    if not WriteClusters(Cluster,1,FClusterBuffer^) then Exit;
   
    {Update Count}
    Dec(Count);
    Inc(Cluster);
   end;
  
  Result:=True;
 finally  
  ClusterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.ReadClusters(ACluster:LongWord;ACount:Word;var ABuffer):Boolean;
{Read count Clusters from the Volume using Cache}
{Performs conversion of Cluster to Sector based on Offsets}
{Note: Sector is relative to StartSector of the FileSystem}
var
 Sector:LongWord; //To Do //Int64 ?
begin
 {}
 Result:=False;
 
 if ACount = 0 then Exit;
 if FDriver = nil then Exit;
 if ACluster > (FTotalClusterCount - 1) then Exit;

 if FVolume = nil then Exit;
 if FVolume.Device = nil then Exit;
 if FSectorsPerCluster = 0 then Exit;

 {Calculate Sector }
 Sector:=(ACluster shl FSectorShiftCount);
 
 Result:=FDriver.Cache.DeviceRead(FVolume.Device,FStartSector + Sector,FSectorsPerCluster * ACount,ABuffer);
end;

{=============================================================================}

function TCDFSFileSystem.WriteClusters(ACluster:LongWord;ACount:Word;const ABuffer):Boolean;
{Write count Clusters to the Volume using Cache}
{Performs conversion of Cluster to Sector based on Offsets}
{Note: Sector is relative to StartSector of the FileSystem}
var
 Sector:LongWord; //To Do //Int64 ?
begin
 {}
 Result:=False;
 
 if ACount = 0 then Exit;
 if FDriver = nil then Exit;
 if ACluster > (FTotalClusterCount - 1) then Exit;

 if FVolume = nil then Exit;
 if FVolume.Device = nil then Exit;
 if FSectorsPerCluster = 0 then Exit;

 {Check ReadOnly}
 if FReadOnly then Exit;

 {Calculate Sector}
 Sector:=(ACluster shl FSectorShiftCount);
 
 Result:=FDriver.Cache.DeviceWrite(FVolume.Device,FStartSector + Sector,FSectorsPerCluster * ACount,ABuffer);
end;

{=============================================================================}

function TCDFSFileSystem.TestClusters(ACluster,ACount:LongWord):Boolean;
{Test Count clusters from Cluster in the Bitmap Blocks for Free}
{Returns success if Count is zero}
{Note: The Bitmap is not stored in CDFS and is only used when ReadOnly is False}
{Note: Should only be called by AllocClusters}
var
 Start:LongWord;
 Count:LongWord;
 Remain:LongWord;
 BlockNo:LongWord;
 Block:TCDFSDiskBlock;
begin
 {}
 Result:=False;

 if not FBlocks.WriterLock then Exit;
 try
  if FDriver = nil then Exit;

  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.TestClusters - Cluster = ' + IntToStr(ACluster) + ' Count = ' + IntToStr(ACount));
  {$ENDIF}
  
  {Check ReadOnly}
  if FReadOnly then Exit;
  
  {Load Blocks}
  {if not LoadBlocks then Exit;} {Do not call, can be called by LoadBlocks}
  
  {Get Params}
  Start:=ACluster;
  Remain:=ACount;
  
  {Get Block}
  BlockNo:=(Start shr FBlockShiftCount);
  Block:=TCDFSDiskBlock(GetBlockEx(BlockNo,True));
  if Block = nil then Exit;
  
  while Remain > 0 do
   begin
    {Get Count}
    Count:=Min(Remain,Block.BlockCount);
    if (Start > Block.BlockCluster) then Count:=Min(Remain,(Block.BlockCount - (Start - Block.BlockCluster)));
    
    {Test Block}
    if not TestBlock(Block,Start,Count) then Exit;
    
    {Update Params}
    Inc(Start,Count);
    Dec(Remain,Count);
    if Remain = 0 then Break;
    
    {Get Block}
    BlockNo:=(Start shr FBlockShiftCount);
    Block:=TCDFSDiskBlock(GetBlockEx(BlockNo,True));
    if Block = nil then Exit;
   end;
   
  Result:=True;
 finally
  FBlocks.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.MarkClusters(ACluster,ACount:LongWord;AUsed:Boolean):Boolean;
{Mark Count clusters from Cluster in the Bitmap Blocks as Free or Used}
{Returns success if Count is zero}
{Note: The Bitmap is not stored in CDFS and is only used when ReadOnly is False}
{Note: Should only be called by AllocClusters/ReleaseClusters/LoadBlocks}
var
 Start:LongWord;
 Count:LongWord;
 Remain:LongWord;
 BlockNo:LongWord;
 Block:TCDFSDiskBlock;
begin
 {}
 Result:=False;
 
 if not FBlocks.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
 
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.MarkClusters - Cluster = ' + IntToStr(ACluster) + ' Count = ' + IntToStr(ACount));
  {$ENDIF}
  
  {Check ReadOnly}
  if FReadOnly then Exit;
  
  {Load Blocks}
  {if not LoadBlocks then Exit;} {Do not call, can be called by LoadBlocks}
  
  {Get Params}
  Start:=ACluster;
  Remain:=ACount;
  
  {Get Block}
  BlockNo:=(Start shr FBlockShiftCount);
  Block:=TCDFSDiskBlock(GetBlockEx(BlockNo,True));
  if Block = nil then Exit;
  
  while Remain > 0 do
   begin
    {Get Count}
    Count:=Min(Remain,Block.BlockCount);
    if (Start > Block.BlockCluster) then Count:=Min(Remain,(Block.BlockCount - (Start - Block.BlockCluster)));
    
    {Mark Block}
    if not MarkBlock(Block,Start,Count,AUsed) then Exit;
    
    {Update Params}
    Inc(Start,Count);
    Dec(Remain,Count);
    if Remain = 0 then Break;
    
    {Get Block}
    BlockNo:=(Start shr FBlockShiftCount);
    Block:=TCDFSDiskBlock(GetBlockEx(BlockNo,True));
    if Block = nil then Exit;
   end;
   
  Result:=True;
 finally
  FBlocks.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.AllocClusters(var ACluster:LongWord;ACount:LongWord):Boolean;
{Allocate Count Clusters from Next Free in the Bitmap Blocks}
{Or Allocate the requested Cluster and Count Clusters if available}
{Note: The Bitmap is not stored in CDFS and is only used when ReadOnly is False}
var
 Start:LongWord;
 Count:LongWord;
 Remain:LongWord;
 Cluster:LongWord;
 BlockNo:LongWord;
 Block:TCDFSDiskBlock;
begin
 {}
 Result:=False;
 
 if not FBlocks.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
 
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.AllocClusters - Cluster = ' + IntToStr(ACluster) + ' Count = ' + IntToStr(ACount));
  {$ENDIF}
  
  {Check Start}
  if ACluster <> cdfsUnknownCluster then if ACluster < cdfsStartCluster then Exit;
  
  {Check Count}
  if ACount = 0 then Exit;
  
  {Check ReadOnly}
  if FReadOnly then Exit;
  
  {Load Blocks}
  if not LoadBlocks then Exit;
  
  {Check Catalogs}
  if not CheckCatalogs then Exit;
  
  {Check Cluster}
  if ACluster <> cdfsUnknownCluster then
   begin
    {Requested Cluster(s)}
    if ACount = 1 then
     begin
      {Single Cluster}
      if TestClusters(ACluster,ACount) then
       begin
        {Mark Clusters}
        if not MarkClusters(ACluster,ACount,True) then Exit;
        
        {Update Free}
        if FFreeClusterCount <> cdfsUnknownCluster then Dec(FFreeClusterCount,ACount);
        
        Result:=True;
       end;
     end
    else
     begin
      {Multiple Clusters}
      Start:=ACluster;
      Remain:=ACount;
      
      {Get Block}
      BlockNo:=(Start shr FBlockShiftCount);
      Block:=TCDFSDiskBlock(GetBlockEx(BlockNo,True));
      if Block = nil then Exit;
      
      while Remain > 0 do
       begin
        {Get Count}
        Count:=Min(Remain,Block.BlockCount);
        if (Start > Block.BlockCluster) then Count:=Min(Remain,(Block.BlockCount - (Start - Block.BlockCluster)));
        
        {Test Block}
        if not TestBlock(Block,Start,Count) then Exit;
        
        {Update Params}
        Inc(Start,Count);
        Dec(Remain,Count);
        if Remain = 0 then Break;
        
        {Get Block}
        BlockNo:=(Start shr FBlockShiftCount);
        Block:=TCDFSDiskBlock(GetBlockEx(BlockNo,True));
        if Block = nil then Exit;
       end;
       
      {Mark Clusters}
      if not MarkClusters(ACluster,ACount,True) then Exit;
      
      {Update Free}
      if FFreeClusterCount <> cdfsUnknownCluster then Dec(FFreeClusterCount,ACount);
      
      Result:=True;
     end;
   end
  else
   begin
    {Next Free Cluster(s)}
    Cluster:=GetNextFreeCluster;
    
    {$IFDEF CDFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.AllocClusters - Next Free Cluster = ' + IntToStr(Cluster));
    {$ENDIF}
    
    if ACount = 1 then
     begin
      {Single Cluster}
      if Cluster <> cdfsUnknownCluster then
       begin
        {Mark Clusters}
        if not MarkClusters(Cluster,ACount,True) then Exit;
        
        {Update Free}
        if FFreeClusterCount <> cdfsUnknownCluster then Dec(FFreeClusterCount,ACount);
        
        {Return Cluster}
        ACluster:=Cluster;
        
        Result:=True;
       end;
     end
    else
     begin
      {Multiple Clusters}
      while Cluster <> cdfsUnknownCluster do
       begin
        Start:=Cluster;
        Remain:=ACount;
        
        {Get Block}
        BlockNo:=(Start shr FBlockShiftCount);
        Block:=TCDFSDiskBlock(GetBlockEx(BlockNo,True));
        if Block = nil then Exit;
        
        while Remain > 0 do
         begin
          {Get Count}
          Count:=Min(Remain,Block.BlockCount);
          if (Start > Block.BlockCluster) then Count:=Min(Remain,(Block.BlockCount - (Start - Block.BlockCluster)));
          
          {Test Block}
          if not TestBlock(Block,Start,Count) then Break;
          
          {Update Params}
          Inc(Start,Count);
          Dec(Remain,Count);
          if Remain = 0 then Break;
          
          {Get Block}
          BlockNo:=(Start shr FBlockShiftCount);
          Block:=TCDFSDiskBlock(GetBlockEx(BlockNo,True));
          if Block = nil then Exit;
         end;
        
        {Check Remain}
        if Remain = 0 then
         begin
          {Mark Clusters}
          if not MarkClusters(Cluster,ACount,True) then Exit;
          
          {Update Free}
          if FFreeClusterCount <> cdfsUnknownCluster then Dec(FFreeClusterCount,ACount);
          
          {Return Cluster}
          ACluster:=Cluster;
          
          Result:=True;
         end;
        
        {Check result}
        if Result then Break;
        Cluster:=GetNextFreeCluster;
        
        {$IFDEF CDFS_DEBUG}
        if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.AllocClusters - Next Free Cluster = ' + IntToStr(Cluster));
        {$ENDIF}
       end;
     end;
   end;
 finally
  FBlocks.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.ReleaseClusters(ACluster,ACount:LongWord):Boolean;
{Release Count Clusters in the Bitmap Blocks and return to Free}
{Returns success if Count is zero}
{Note: The Bitmap is not stored in CDFS and is only used when ReadOnly is False}
var
 Start:LongWord;
 Count:LongWord;
 Remain:LongWord;
 BlockNo:LongWord;
 Block:TCDFSDiskBlock;
begin
 {}
 Result:=False;
 
 if not FBlocks.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
 
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.ReleaseClusters - Cluster = ' + IntToStr(ACluster) + ' Count = ' + IntToStr(ACount));
  {$ENDIF}
  
  {Check ReadOnly}
  if FReadOnly then Exit;
  
  {Load Blocks}
  if not LoadBlocks then Exit;
  
  {Get Params}
  Start:=ACluster;
  Remain:=ACount;
  
  {Get Block}
  BlockNo:=(Start shr FBlockShiftCount);
  Block:=TCDFSDiskBlock(GetBlockEx(BlockNo,True));
  if Block = nil then Exit;
  
  while Remain > 0 do
   begin
    {Get Count}
    Count:=Min(Remain,Block.BlockCount);
    if (Start > Block.BlockCluster) then Count:=Min(Remain,(Block.BlockCount - (Start - Block.BlockCluster)));
    
    {Mark Block}
    if not MarkBlock(Block,Start,Count,False) then Exit;
    
    {Update Free}
    if FFreeClusterCount <> cdfsUnknownCluster then Inc(FFreeClusterCount,Count);
    
    {Update Params}
    Inc(Start,Count);
    Dec(Remain,Count);
    if Remain = 0 then Break;
    
    {Get Block}
    BlockNo:=(Start shr FBlockShiftCount);
    Block:=TCDFSDiskBlock(GetBlockEx(BlockNo,True));
    if Block = nil then Exit;
   end;
   
  Result:=True;
 finally
  FBlocks.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.GetNextFreeCluster:LongWord;
var
 Next:LongWord;
 Start:LongWord;
 Origin:LongWord;
 Cluster:LongWord;
 Wrapped:Boolean;
 BlockNo:LongWord;
 Block:TCDFSDiskBlock;
begin
 {}
 Result:=cdfsUnknownCluster;
 
 if not FBlocks.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FEntriesPerBlock = 0 then Exit;
  if FTotalClusterCount = 0 then Exit;
 
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.GetNextFreeCluster');
  {$ENDIF}
  
  {Check ReadOnly}
  if FReadOnly then Exit;
  
  {Load Blocks}
  if not LoadBlocks then Exit;
  
  {Get Params}
  Cluster:=0;
  Wrapped:=False;
  if FLastFreeCluster <> cdfsUnknownCluster then Inc(FLastFreeCluster); {Increment on each Get Next as CDFS may reject the last returned value}
  if FLastFreeCluster >= FTotalClusterCount then FLastFreeCluster:=0;   {Check for Wraparound of Last Free}
  if FLastFreeCluster <> cdfsUnknownCluster then Cluster:=FLastFreeCluster;
  Origin:=Cluster;
  
  {Check each Block}
  while Cluster < FTotalClusterCount do
   begin
    {Get Block}
    BlockNo:=(Cluster shr FBlockShiftCount);
    Block:=TCDFSDiskBlock(GetBlockEx(BlockNo,True));
    if Block = nil then Exit;
    
    {Get Start}
    Start:=Block.BlockCluster;
    if Origin = Cluster then Start:=Origin;
    
    {Get Next Free}
    Next:=GetBlockNextFree(Block,Start);
    if Next <> cdfsUnknownCluster then
     begin
      FLastFreeCluster:=Next;
      Result:=FLastFreeCluster;
      Exit;
     end;
    
    {Check for Origin}
    if (Origin > 0) and (Wrapped) and (Start >= Origin) then Exit;
    
    {Move next Block}
    Inc(Cluster,Block.BlockCount);
    
    {Check for Wrap}
    if (Origin > 0) and (Cluster >= FTotalClusterCount) then Cluster:=0;
    if (Origin > 0) and (Cluster = 0) then Wrapped:=True;
   end;
 finally
  FBlocks.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.GetFreeClusterCount:LongWord;
var
 Count:LongWord;
 Cluster:LongWord;
 BlockNo:LongWord;
 Block:TCDFSDiskBlock;
begin
 {}
 Result:=cdfsUnknownCluster;
 
 if FDriver = nil then Exit;
 if FEntriesPerBlock = 0 then Exit;
 if FTotalClusterCount = 0 then Exit;
 
 {$IFDEF CDFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.GetFreeClusterCount');
 {$ENDIF}
 
 {Check ReadOnly}
 if FReadOnly then Exit;
 
 {Load Blocks}
 if not LoadBlocks then Exit;
 
 {Check Free Count}
 if FFreeClusterCount = cdfsUnknownCluster then
  begin
   if not FBlocks.WriterLock then Exit;
   try
    {Get Params}
    Cluster:=0;
    FFreeClusterCount:=0;
    
    {Check each Block}
    while Cluster < FTotalClusterCount do
     begin
      {Get Block}
      BlockNo:=(Cluster shr FBlockShiftCount);
      Block:=TCDFSDiskBlock(GetBlockEx(BlockNo,True));
      if Block = nil then Exit;
      
      {Get Free Count}
      Count:=GetBlockFreeCount(Block);
      if Count <> cdfsUnknownCluster then Inc(FFreeClusterCount,Count);
      
      {Move next Block}
      Inc(Cluster,Block.BlockCount);
     end;
   finally
    FBlocks.WriterUnlock;
   end; 
  end;
   
 Result:=FFreeClusterCount;
end;

{=============================================================================}

function TCDFSFileSystem.TestBlock(ABlock:TCDFSDiskBlock;AStart,ACount:LongWord):Boolean;
{Test Count clusters from Start in the Block bitmap for Free}
{Start is the cluster number in the block to start from}
{Start must be greater than or equal to block cluster}
{Note: Should only be called by TestClusters}
{Note: Caller must hold the blocks lock}
var
 Start:LongWord;
begin
 {}
 Result:=False;
 
 if ABlock = nil then Exit;
 
 {$IFDEF CDFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.TestBlock - BlockNo = ' + IntToHex(ABlock.BlockNo,8) + ' Start = ' + IntToStr(AStart) + ' Count = ' + IntToStr(ACount));
 {$ENDIF}
 
 {Check Start}
 if AStart < ABlock.BlockCluster then Exit;
 if AStart >= (ABlock.BlockCluster + ABlock.BlockCount) then Exit;
 
 {Check Count}
 if ACount = 0 then Exit;
 if ACount > ABlock.BlockCount then Exit;
 if (AStart + ACount) > (ABlock.BlockCluster + ABlock.BlockCount) then Exit;
 
 {Get Start}
 Start:=(AStart - ABlock.BlockCluster);
 
 {Test Bitmap}
 Result:=TestBitmap(ABlock.BlockBuffer,ABlock.BlockCount,Start,ACount);
end;

{=============================================================================}

function TCDFSFileSystem.MarkBlock(ABlock:TCDFSDiskBlock;AStart,ACount:LongWord;AUsed:Boolean):Boolean;
{Mark Count clusters from Start in the Block bitmap as Free or Used}
{Start is the cluster number in the block to start from}
{Start must be greater than or equal to block cluster}
{Note: Should only be called by MarkClusters}
{Note: Caller must hold the blocks lock}
var
 Start:LongWord;
begin
 {}
 Result:=False;
 
 if ABlock = nil then Exit;
 
 {$IFDEF CDFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.MarkBlock - BlockNo = ' + IntToHex(ABlock.BlockNo,8) + ' Start = ' + IntToStr(AStart) + ' Count = ' + IntToStr(ACount));
 {$ENDIF}
 
 {Check Start}
 if AStart < ABlock.BlockCluster then Exit;
 if AStart >= (ABlock.BlockCluster + ABlock.BlockCount) then Exit;
 
 {Check Count}
 if ACount = 0 then Exit;
 if ACount > ABlock.BlockCount then Exit;
 if (AStart + ACount) > (ABlock.BlockCluster + ABlock.BlockCount) then Exit;
 
 {Get Start}
 Start:=(AStart - ABlock.BlockCluster);
 
 {Mark Bitmap}
 Result:=MarkBitmap(ABlock.BlockBuffer,ABlock.BlockCount,Start,ACount,AUsed);
end;

{=============================================================================}

function TCDFSFileSystem.AllocBlock(ABlock:TCDFSDiskBlock;AStart,ACount:LongWord):Boolean;
{Note: Caller must hold the blocks lock}
begin
 {AllocBlock simply calls MarkBlock with True}
 Result:=MarkBlock(ABlock,AStart,ACount,True);
end;

{=============================================================================}

function TCDFSFileSystem.ReleaseBlock(ABlock:TCDFSDiskBlock;AStart,ACount:LongWord):Boolean;
{Note: Caller must hold the blocks lock}
begin
 {ReleaseBlock simply calls MarkBlock with False}
 Result:=MarkBlock(ABlock,AStart,ACount,False);
end;

{=============================================================================}

function TCDFSFileSystem.GetBlockNextFree(ABlock:TCDFSDiskBlock;AStart:LongWord):LongWord;
{Start is the cluster number in the block to start from}
{Start must be greater than or equal to block cluster}
{Note: Should only be called by GetNextFreeCluster}
{Note: Caller must hold the blocks lock}
var
 Next:LongWord;
 Start:LongWord;
begin
 {}
 Result:=cdfsUnknownCluster;
 
 if ABlock = nil then Exit;
 
 {$IFDEF CDFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.GetBlockNextFree - BlockNo = ' + IntToHex(ABlock.BlockNo,8) + ' Start = ' + IntToStr(AStart));
 {$ENDIF}
 
 {Check Start}
 if AStart < ABlock.BlockCluster then Exit;
 if AStart >= (ABlock.BlockCluster + ABlock.BlockCount) then Exit;
 
 {Get Start}
 Start:=(AStart - ABlock.BlockCluster);
 
 {Get Next Free}
 Next:=GetBitmapNextFree(ABlock.BlockBuffer,ABlock.BlockCount,Start);
 if Next <> cdfsBitmapUnknown then Result:=(ABlock.BlockCluster + Next);
end;

{=============================================================================}

function TCDFSFileSystem.GetBlockFreeCount(ABlock:TCDFSDiskBlock):LongWord;
{Note: Should only be called by GetFreeClusterCount}
{Note: Caller must hold the blocks lock}
var
 Count:LongWord;
begin
 {}
 Result:=cdfsUnknownCluster;
 
 if ABlock = nil then Exit;
 
 {$IFDEF CDFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.GetBlockFreeCount - BlockNo = ' + IntToHex(ABlock.BlockNo,8));
 {$ENDIF}
 
 {Get Free Count}
 Count:=GetBitmapFreeCount(ABlock.BlockBuffer,ABlock.BlockCount);
 if Count <> cdfsBitmapUnknown then Result:=Count;
end;

{=============================================================================}

function TCDFSFileSystem.TestBitmap(ABuffer:Pointer;ASize,AStart,ACount:LongWord):Boolean;
{Test Count bits from Start in the bitmap for Free}
{Size is the total number of bits in the bitmap}
{Start is the bit number in the bitmap to start from}
{Count is the number of bits in the bitmap to be marked}
{Note: Should only be called by TestBlock}
{Note: Bitmaps are a multiple of 64 bits in size but free and used
       blocks are marked using an array of bytes in little endian order}
{Note: Caller must hold the blocks lock}       
var
 Size:LongWord;   {Number of 64 bit blocks in bitmap}
 Start:LongWord;  {Starting block in the bitmap data}
 Block:LongWord;  {Current block in the bitmap data}
 Offset:LongWord; {Current offset into the bitmap data}
 Bit:LongWord;    {Starting bit to test in current block (0 if first bit)}
 Bits:LongWord;   {Number of bits to test in current block (64 if all bits)}
 Remain:LongWord; {Number of bits remaining to be tested}
 Current:LongWord;{Current bit to test in current block}
begin
 {} //To Do //See Changes / Improvments in NTFS
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {$IFDEF CDFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.TestBitmap - Size = ' + IntToStr(ASize) + ' Start = ' + IntToStr(AStart) + ' Count = ' + IntToStr(ACount));
 {$ENDIF}
 
 {Get Size}
 Size:=(ASize shr 6); {Divide by 64}
 if (Size shl 6) < ASize then Inc(Size);
 
 {Get Start}
 Start:=(AStart shr 6); {Divide by 64}
 
 {Get Params}
 Block:=Start;
 Offset:=(Block shl 3); {Multiply by 8}
 Bit:=AStart - ((AStart shr 6) shl 6); {Divide by 64 / Multiply by 64} {If less than 64 then will subtract 0 from start}
 Remain:=ACount;
 while Block < Size do
  begin
   {Get Bits}
   Bits:=Min(Remain,cdfsBitmapMaskBits);
   if Bit > 0 then Bits:=Min(Remain,(cdfsBitmapMaskBits - Bit));
   
   {$IFDEF CDFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.TestBitmap - Block = ' + IntToStr(Block) + ' Offset = ' + IntToStr(Offset) + ' Remain = ' + IntToStr(Remain) + ' Bit = ' + IntToStr(Bit) + ' Bits = ' + IntToStr(Bits));
   {$ENDIF}
   
   {Test Bits} //To Do //Can we optimize to test all bits at once as per VirtualDiskVpcImage.TestBitmap ?
   for Current:=Bit to (Bit + (Bits - 1)) do
    begin
     {Test Free}
     if And64(Int64(Pointer(PtrUInt(ABuffer) + Offset)^),cdfsBitmapMasks[Current]) = cdfsBitmapMasks[Current] then Exit;
     //To Do //Replace these with inbuilt routines which handle Int64 without problems.
    end;
   
   {Update Params}
   Bit:=0;
   Inc(Block);
   Inc(Offset,8);
   Dec(Remain,Bits);
   
   if Remain = 0 then Break;
  end;
  
 Result:=True;
end;

{=============================================================================}

function TCDFSFileSystem.MarkBitmap(ABuffer:Pointer;ASize,AStart,ACount:LongWord;AUsed:Boolean):Boolean;
{Mark Count bits from Start in the bitmap as Free or Used}
{Size is the total number of bits in the bitmap}
{Start is the bit number in the bitmap to start from (0 if first bit)}
{Count is the number of bits in the bitmap to be marked}
{Note: Should only be called by MarkBlock}
{Note: Bitmaps are a multiple of 64 bits in size but free and used
       blocks are marked using an array of bytes in little endian order}
{Note: Caller must hold the blocks lock}       
var
 Size:LongWord;   {Number of 64 bit blocks in bitmap}
 Start:LongWord;  {Starting block in the bitmap data}
 Block:LongWord;  {Current block in the bitmap data}
 Offset:LongWord; {Current offset into the bitmap data}
 Bit:LongWord;    {Starting bit to mark in current block (0 if first bit)}
 Bits:LongWord;   {Number of bits to mark in current block (64 if all bits)}
 Remain:LongWord; {Number of bits remaining to be marked}
 Current:LongWord;{Current bit to mark in current block}
begin
 {} //To Do //See Changes / Improvments in NTFS
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {$IFDEF CDFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.MarkBitmap - Size = ' + IntToStr(ASize) + ' Start = ' + IntToStr(AStart) + ' Count = ' + IntToStr(ACount));
 {$ENDIF}
 
 {Get Size}
 Size:=(ASize shr 6); {Divide by 64}
 if (Size shl 6) < ASize then Inc(Size);
 
 {Get Start}
 Start:=(AStart shr 6); {Divide by 64}
 
 {Get Params}
 Block:=Start;
 Offset:=(Block shl 3); {Multiply by 8}
 Bit:=AStart - ((AStart shr 6) shl 6); {Divide by 64 / Multiply by 64} {If less than 64 then will subtract 0 from start}
 Remain:=ACount;
 while Block < Size do
  begin
   {Get Bits}
   Bits:=Min(Remain,cdfsBitmapMaskBits);
   if Bit > 0 then Bits:=Min(Remain,(cdfsBitmapMaskBits - Bit));
   
   {$IFDEF CDFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.MarkBitmap - Block = ' + IntToStr(Block) + ' Offset = ' + IntToStr(Offset) + ' Remain = ' + IntToStr(Remain) + ' Bit = ' + IntToStr(Bit) + ' Bits = ' + IntToStr(Bits));
   {$ENDIF}
   
   {Mark Bits} //To Do //Can we optimize to mark all bits at once as per VirtualDiskVpcImage.TestBitmap ?
   for Current:=Bit to (Bit + (Bits - 1)) do
    begin
     if AUsed then
      begin
       {Mark Used}
       Int64(Pointer(PtrUInt(ABuffer) + Offset)^):=Or64(Int64(Pointer(PtrUInt(ABuffer) + Offset)^),cdfsBitmapMasks[Current]);
       //To Do //Replace these with inbuilt routines which handle Int64 without problems.
      end
     else
      begin
       {Mark Free}
       Int64(Pointer(PtrUInt(ABuffer) + Offset)^):=And64(Int64(Pointer(PtrUInt(ABuffer) + Offset)^),Not64(cdfsBitmapMasks[Current]));
       //To Do //Replace these with inbuilt routines which handle Int64 without problems.
      end;
    end;
   
   {Update Params}
   Bit:=0;
   Inc(Block);
   Inc(Offset,8);
   Dec(Remain,Bits);
   
   if Remain = 0 then Break;
  end;
 Result:=True;
end;

{=============================================================================}

function TCDFSFileSystem.AllocBitmap(ABuffer:Pointer;ASize:LongWord;AStart,ACount:LongWord):Boolean;
{Note: Caller must hold the blocks lock}       
begin
 {AllocBitmap simply calls MarkBitmap with True}
 Result:=MarkBitmap(ABuffer,ASize,AStart,ACount,True);
end;

{=============================================================================}

function TCDFSFileSystem.ReleaseBitmap(ABuffer:Pointer;ASize:LongWord;AStart,ACount:LongWord):Boolean;
{Note: Caller must hold the blocks lock}       
begin
 {ReleaseBitmap simply calls MarkBitmap with False}
 Result:=MarkBitmap(ABuffer,ASize,AStart,ACount,False);
end;

{=============================================================================}

function TCDFSFileSystem.GetBitmapNextFree(ABuffer:Pointer;ASize,AStart:LongWord):LongWord;
{Size is the total number of bits in the bitmap}
{Start is the bit number in the bitmap to start from}
{Note: Should only be called by GetBlockNextFree}
{Note: Bitmaps are a multiple of 64 bits in size but free and used
       blocks are marked using an array of bytes in little endian order}
{Note: Caller must hold the blocks lock}              
var
 Size:LongWord;   {Number of 64 bit blocks in bitmap}
 Start:LongWord;  {Starting block in the bitmap data}
 Block:LongWord;  {Current block in the bitmap data}
 Offset:LongWord; {Current offset into the bitmap data}
 Bit:LongWord;    {Starting bit to check in current block (0 if first bit)}
 Bits:LongWord;   {Number of bits to check in current block (64 if all bits)}
 Remain:LongWord; {Number of Bits remaining to be checked}
 Current:LongWord;{Current bit to check in current block}
begin
 {} //To Do //See Changes / Improvments in NTFS (And64 etc)
 Result:=cdfsBitmapUnknown;
 
 if ABuffer = nil then Exit;

 {$IFDEF CDFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.GetBitmapNextFree - Size = ' + IntToStr(ASize) + ' Start = ' + IntToStr(AStart));
 {$ENDIF}
 
 {Get Size}
 Size:=(ASize shr 6); {Divide by 64}
 if (Size shl 6) < ASize then Inc(Size);
 
 {Get Start}
 Start:=(AStart shr 6); {Divide by 64}
 
 {Get Params}
 Block:=Start;
 Offset:=(Block shl 3); {Multiply by 8}
 Bit:=AStart - ((AStart shr 6) shl 6); {Divide by 64 / Multiply by 64} {If less than 64 then will subtract 0 from start}
 Remain:=ASize - AStart;
 while Block < Size do
  begin
   {Get Bits}
   Bits:=Min(Remain,cdfsBitmapMaskBits);
   if Bit > 0 then Bits:=Min(Remain,(cdfsBitmapMaskBits - Bit));
   
   {$IFDEF CDFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.GetBitmapNextFree - Block = ' + IntToStr(Block) + ' Offset = ' + IntToStr(Offset) + ' Remain = ' + IntToStr(Remain) + ' Bit = ' + IntToStr(Bit) + ' Bits = ' + IntToStr(Bits));
   {$ENDIF}
   
   {Mark Bits}
   if (Bit = 0) and (Int64(Pointer(PtrUInt(ABuffer) + Offset)^) = cdfsBitmapMaskNone) then
    begin
     {All Free}
     Result:=(Block shl 6); {Multiply by 64}
     Exit;
    end
   else if (Bit = 0) and (Int64(Pointer(PtrUInt(ABuffer) + Offset)^) = cdfsBitmapMaskAll) then
    begin
     {All Used}
     {Nothing}
    end
   else
    begin
     {Used and Free}
     for Current:=Bit to (Bit + (Bits - 1)) do
      begin
       if And64(Int64(Pointer(PtrUInt(ABuffer) + Offset)^),cdfsBitmapMasks[Current]) = cdfsBitmapMaskNone then
        begin
         {$IFDEF CDFS_DEBUG}
         if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.GetBitmapNextFree - Block = ' + IntToStr(Block) + ' Current = ' + IntToStr(Current) + ' Bits = ' + IntToHex(Int64(Pointer(PtrUInt(ABuffer) + Offset)^),16));
         {$ENDIF}
         Result:=(Block shl 6) + Current; {Multiply by 64 / Add Current Bit}
         Exit;
        end;
      end;
    end;
    
   {Update Params}
   Bit:=0;
   Inc(Block);
   Inc(Offset,8);
   
   Dec(Remain,Bits);
  end;
end;

{=============================================================================}

function TCDFSFileSystem.GetBitmapFreeCount(ABuffer:Pointer;ASize:LongWord):LongWord;
{Size is the total number of bits in the bitmap}
{Note: Should only be called by GetBlockFreeCount}
{Note: Bitmaps are a multiple of 64 bits in size but free and used
       blocks are marked using an array of bytes in little endian order}
{Note: Caller must hold the blocks lock}              
var
 Size:LongWord;   {Number of 64 bit blocks in bitmap}
 Value:LongWord;
 Block:LongWord;  {Current block in the bitmap data}
 Offset:LongWord; {Current offset into the bitmap data}
 Bits:LongWord;   {Number of Bits to check in current block}
 Remain:LongWord; {Number of Bits remaining to be checked}
begin
 {} //To Do //See Changes / Improvments in NTFS
 Result:=cdfsBitmapUnknown;
 
 if ABuffer = nil then Exit;
 
 Result:=0;
 
 {$IFDEF CDFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.GetBitmapFreeCount - Size = ' + IntToStr(ASize));
 {$ENDIF}
 
 {Get Size}
 Size:=(ASize shr 6); {Divide by 64}
 if (Size shl 6) < ASize then Inc(Size);
 
 {Get Params}
 Block:=0;
 Offset:=0;
 Remain:=ASize;
 while Block < Size do
  begin
   {Get Bits}
   Bits:=Min(Remain,cdfsBitmapMaskBits);
   
   {$IFDEF CDFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.GetBitmapFreeCount - Bits = ' + IntToStr(Bits) + ' cdfsBitmapOverlays = ' + IntToHex(cdfsBitmapOverlays[Bits - 1],16) + ' Hi = ' + IntToHex(Int64Rec(cdfsBitmapOverlays[Bits - 1]).Hi,8) + ' Lo = ' + IntToHex(Int64Rec(cdfsBitmapOverlays[Bits - 1]).Lo,8));
   {$ENDIF}
   
   if Int64(Pointer(PtrUInt(ABuffer) + Offset)^) = cdfsBitmapMaskNone then
    begin
     {All Free}
     Inc(Result,Bits);
    end
   else if Int64(Pointer(PtrUInt(ABuffer) + Offset)^) = cdfsBitmapMaskAll then
    begin
     {All Used}
     {Nothing}
    end
   else
    begin
     {Used and Free}
     Value:=(not(LongWord(Pointer(PtrUInt(ABuffer) + Offset)^))) and (Int64Rec(cdfsBitmapOverlays[Bits - 1]).Hi);  //To Do //Lo/Hi to be confirmed
     while Value > 0 do
      begin
       Inc(Result);
       Value:=(Value and (Value - 1));
      end;
     Value:=(not(LongWord(Pointer(PtrUInt(ABuffer) + Offset + 4)^))) and (Int64Rec(cdfsBitmapOverlays[Bits - 1]).Lo); //To Do //Lo/Hi to be confirmed
     while Value > 0 do
      begin
       Inc(Result);
       Value:=(Value and (Value - 1));
      end;
    end;
    
   {Update Params}
   Inc(Block);
   Inc(Offset,8);
   
   Dec(Remain,Bits);
  end;
end;

{=============================================================================}

function TCDFSFileSystem.GetTableDataFree(ATable:TDiskTable):LongWord;
{Note: Caller must hold the tables and paths lock}
var
 DataFree:LongWord;
 Path:TCDFSDiskPath;
begin
 {}
 Result:=0;
 
 if FDriver = nil then Exit;
 if ATable = nil then Exit;

 {Check Free}
 if TCDFSDiskTable(ATable).DataFree = 0 then
  begin
   {Load Paths}
   if not LoadPaths(TCDFSDiskTable(ATable)) then Exit;
   
   {Get Size}
   DataFree:=TCDFSDiskTable(ATable).DataSize;
   
   {Get Paths}
   Path:=TCDFSDiskPath(TCDFSDiskTable(ATable).Paths.First); {Protected by caller held paths lock}
   while Path <> nil do
    begin
     Dec(DataFree,Path.PathRecordSize(TCDFSDiskTable(ATable).Unicode));
     
     Path:=TCDFSDiskPath(Path.Next); {Protected by caller held paths lock}
    end;
    
   {Set Free}
   TCDFSDiskTable(ATable).DataFree:=DataFree;
  end;
  
 Result:=TCDFSDiskTable(ATable).DataFree;
end;

{=============================================================================}

function TCDFSFileSystem.GetEntryDataFree(AEntry:TDiskEntry):LongWord;
{Note: Caller must hold the entries lock}
var
 DataFree:LongWord;
 Entry:TCDFSDiskEntry;
begin
 {}
 Result:=0;
 if FDriver = nil then Exit;
 if AEntry = nil then Exit;

 {Check Relative} {Allow Dot only on Root Directory}
 if ((AEntry.Attributes and (faDot or faDotDot)) <> faNone) and (AEntry <> FRoot) then Exit;
 
 {Check Directory}
 if (AEntry.Attributes and faDirectory) = faDirectory then
  begin
   {Check Free}
   if TCDFSDiskEntry(AEntry).DataFree = 0 then
    begin
     {Load Entries}
     if not LoadEntries(AEntry) then Exit;
     
     {Get Size}
     DataFree:=TCDFSDiskEntry(AEntry).DataSize;
     
     {Get Entries}
     Entry:=TCDFSDiskEntry(TCDFSDiskEntry(AEntry).FirstChild); {Protected by caller held entries lock}
     while Entry <> nil do
      begin
       Dec(DataFree,Entry.DirectoryRecordSize);
       
       Entry:=TCDFSDiskEntry(Entry.Next); {Protected by caller held entries lock}
      end;
     
     {Set Free}
     TCDFSDiskEntry(AEntry).DataFree:=DataFree;
    end;
    
   Result:=TCDFSDiskEntry(AEntry).DataFree;
  end;
end;

{=============================================================================}

function TCDFSFileSystem.GetCatalogDataFree(ADescriptor:TCDFSDiskDescriptor):LongWord;
{Note: Caller must hold the descriptors, catalogs, headers and extensions lock}
var
 DataFree:LongWord;
 Header:TCDFSDiskHeader;
 Catalog:TCDFSDiskCatalog;
 Extension:TCDFSDiskExtension;
begin
 {}
 Result:=0;
 if FDriver = nil then Exit;
 if ADescriptor = nil then Exit;

 {Load Catalogs}
 if not LoadCatalogs then Exit;
 
 {Check Descriptor}
 if ADescriptor.CatalogStart = 0 then Exit;
 if ADescriptor.CatalogCount = 0 then Exit;
 
 {Get Size}
 DataFree:=(ADescriptor.CatalogCount shl FClusterShiftCount);
 
 {Get Headers}
 Header:=TCDFSDiskHeader(FHeaders.First); {Protected by caller held headers lock}
 while Header <> nil do
  begin
   Dec(DataFree,cdfsHeaderRecordSize);
   
   Header:=TCDFSDiskHeader(Header.Next); {Protected by caller held headers lock}
  end;
 
 {Get Catalogs}
 Catalog:=TCDFSDiskCatalog(FCatalogs.First); {Protected by caller held catalogs lock}
 while Catalog <> nil do
  begin
   Dec(DataFree,cdfsCatalogRecordSize);
   
   Catalog:=TCDFSDiskCatalog(Catalog.Next); {Protected by caller held catalogs lock}
  end;
 
 {Get Extensions}
 Extension:=TCDFSDiskExtension(FExtensions.First); {Protected by caller held extensions lock}
 while Extension <> nil do
  begin
   Dec(DataFree,cdfsExtensionRecordSize);
   
   Extension:=TCDFSDiskExtension(Extension.Next); {Protected by caller held extensions lock}
  end;
  
 Result:=DataFree;
end;

{=============================================================================}

function TCDFSFileSystem.GetPreviousPath(ATable:TCDFSDiskTable;APath:TCDFSDiskPath):TCDFSDiskPath;
{Note: Caller must hold the tables and paths lock}
var
 Path:TCDFSDiskPath;
begin
 {}
 Result:=nil;
 if FDriver = nil then Exit;
 if ATable = nil then Exit;
 if APath = nil then Exit;

 {Load Paths}
 if not LoadPaths(TCDFSDiskTable(ATable)) then Exit;
 
 {Check Paths}
 Path:=TCDFSDiskPath(TCDFSDiskTable(ATable).Paths.Last); {Search Backwards} {Protected by caller held paths lock}
 while Path <> nil do
  begin
   if ComparePath(Path,APath,ATable.Unicode) = -1 then
    begin
     Result:=Path;
     Exit;
    end;
    
   Path:=TCDFSDiskPath(Path.Prev); {Protected by caller held paths lock}
  end;
end;

{=============================================================================}

function TCDFSFileSystem.GetPreviousEntry(AParent,AEntry:TCDFSDiskEntry):TCDFSDiskEntry;
{Note: Caller must hold the entries lock}
var
 Entry:TCDFSDiskEntry;
begin
 {}
 Result:=nil;
 if FDriver = nil then Exit;
 if AParent = nil then Exit;
 if AEntry = nil then Exit;

 {Load Entries}
 if not LoadEntries(AParent) then Exit;
 
 {Get Entries}
 Entry:=TCDFSDiskEntry(TCDFSDiskEntry(AParent).LastChild); {Search Backwards} {Protected by caller held entries lock}
 while Entry <> nil do
  begin
   if CompareEntry(Entry,AEntry,AParent.Unicode) = -1 then
    begin
     Result:=Entry;
     Exit;
    end;
    
   Entry:=TCDFSDiskEntry(Entry.Prev); {Protected by caller held entries lock}
  end;
end;

{=============================================================================}

function TCDFSFileSystem.GetSectorsPerCluster(AClusterSize:LongWord):LongWord;
{Calculate the number of sectors per cluster}
{Only called during volume mount and initialization}
begin
 {}
 Result:=0;
 
 if FDriver = nil then Exit;
 if AClusterSize = 0 then Exit;
 
 Result:=1;
 
 if AClusterSize <= FSectorSize then Exit;
 
 Result:=(AClusterSize div FSectorSize);
end;

{=============================================================================}

function TCDFSFileSystem.GetBlockShiftCount(AClusterSize:LongWord):Word;
{Calculate the block shift count for cluster to block conversion}
{Only called during volume mount and initialization}
begin
 {}
 Result:=0;
 
 if FDriver = nil then Exit;
 if AClusterSize = 0 then Exit;

 {Get the Shift Count}
 while ((AClusterSize * 8) shr Result) > 1 do
  begin
   Inc(Result);
  end;
end;

{=============================================================================}

function TCDFSFileSystem.GetSectorShiftCount(AClusterSize:LongWord):Word;
{Calculate the sector shift count for sector to cluster conversion}
{Only called during volume mount and initialization}
begin
 {}
 Result:=0;
 
 if FDriver = nil then Exit;
 if FSectorSize = 0 then Exit;
 if AClusterSize = 0 then Exit;

 {Get the Shift Count}
 while (FSectorSize shl Result) < AClusterSize do
  begin
   Inc(Result);
  end;
end;

{=============================================================================}

function TCDFSFileSystem.GetClusterShiftCount(AClusterSize:LongWord):Word;
{Calculate the cluster shift count for cluster to bytes conversion}
{Only called during volume mount and initialization}
begin
 {}
 Result:=0;
 
 if FDriver = nil then Exit;
 if AClusterSize = 0 then Exit;

 {Get the Shift Count}
 while (1 shl Result) < AClusterSize do
  begin
   Inc(Result);
  end;
end;

{=============================================================================}

function TCDFSFileSystem.GetEntriesPerBlock(AClusterSize:LongWord):LongWord;
{Calculate the number of cluster entries per block of bitmap entries}
{Only called during volume mount and initialization}
begin
 {}
 Result:=(AClusterSize * 8); {Cluster sized blocks}
end;

{=============================================================================}

function TCDFSFileSystem.GetClustersPerBlock(AClusterSize:LongWord):LongWord;
{Calculate the number of clusters per block of bitmap entries}
{Only called during volume mount and initialization}
begin
 {}
 Result:=1; {Cluster sized blocks}
end;

{=============================================================================}

function TCDFSFileSystem.GetTotalBlockCount(ATotalClusterCount:LongWord):LongWord;
{Calculate the total number of bitmap entry blocks}
{Only called during volume mount and initialization}
var
 ClusterCount:LongWord;
begin
 {}
 Result:=((ATotalClusterCount - 1) shr FBlockShiftCount);
 
 ClusterCount:=Result;
 if (ClusterCount shl FBlockShiftCount) < (ATotalClusterCount - 1) then
  begin
   Inc(Result);
  end;
end;

{=============================================================================}

function TCDFSFileSystem.RenumberPaths(ATable:TDiskTable):Boolean;
{Note: Caller must hold the tables lock}
var
 Number:Word;
 Path:TCDFSDiskPath;
begin
 {}
 Result:=False;
 
 if ATable = nil then Exit;

 if not TCDFSDiskTable(ATable).Paths.WriterLock then Exit;
 try
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.RenumberPaths - TableNo = ' + IntToHex(ATable.TableNo,8));
  {$ENDIF}
 
  {Renumber Paths}
  Number:=cdfsRootPathNumber;
  Path:=TCDFSDiskPath(TCDFSDiskTable(ATable).Paths.First);
  while Path <> nil do
   begin
    {Update Path Number}
    Path.PathNumber:=Number;
   
    {Update Parent Number}
    if Path.Parent <> nil then Path.ParentNumber:=Path.Parent.PathNumber;
    Inc(Number);
   
    Path:=TCDFSDiskPath(Path.Next);
   end;
  
  Result:=True; 
 finally
  TCDFSDiskTable(ATable).Paths.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.ComparePath(APath1,APath2:TCDFSDiskPath;AUnicode:Boolean):Integer;
{Path Sort Ordering:
 - Only Directories are represented.
 - Root (Dot) is always first.
 - DotDot for Root is not represented.
 - Dot and DotDot for other Directories are not represented.
 - In order of Parent Number  (Root Directory is 0001 and Roots ParentNumber is 0001)
 - In order of Byte Value in characters of Name with padding of 20 (32) (00 if Unicode)
 - In order of Byte Value of characters of Extension with padding of 00 (00) (Only if Unicode))
}
{Note: Caller must hold the paths lock}
var
 Name1:String;
 Name2:String;
 WorkBuffer1:String;
 WorkBuffer2:String;
begin
 {}
 Result:=0;
 
 if APath1 = nil then Exit;
 if APath2 = nil then Exit;
 
 {Check Root}
 if APath1.Name = cdfsDotName then
  begin
   Result:=-1;
   Exit;
  end
 else if APath2.Name = cdfsDotName then
  begin
   Result:=1;
   Exit;
  end
 else
  begin
   {Check Parent Number}
   if APath1.ParentNumber < APath2.ParentNumber then
    begin
     Result:=-1;
     Exit;
    end
   else if APath1.ParentNumber > APath2.ParentNumber then
    begin
     Result:=1;
     Exit;
    end
   else
    begin
     {Check Name}
     Name1:=APath1.Name;
     Name2:=APath2.Name;
     if (Length(Name1) <> 0) or (Length(Name2) <> 0) then
      begin
       WorkBuffer1:=PadString(Name1,Max(Length(Name1),Length(Name2)),False,AUnicode);
       WorkBuffer2:=PadString(Name2,Max(Length(Name1),Length(Name2)),False,AUnicode);
       Result:=CompareString(WorkBuffer1,WorkBuffer2,False);
      end;
    end;
  end;
end;

{=============================================================================}

function TCDFSFileSystem.CompareEntry(AEntry1,AEntry2:TCDFSDiskEntry;AUnicode:Boolean):Integer;
{Entry Sort Ordering:
 - Dot is always first.
 - DotDot is always second.
 - In order of Byte Value of characters of Name with padding of 20 (32) (00 if Unicode)
 - In order of Byte Value of characters of Extension with padding of 20 (32) (00 if Unicode)
 - In Reverse order of Byte Value of characters of Version with padding of 30 (48) (00 if Unicode)
}
{Note: Caller must hold the entries lock}
var
 Ext1:String;
 Ext2:String;
 Name1:String;
 Name2:String;
 WorkBuffer1:String;
 WorkBuffer2:String;
begin
 {}
 Result:=0;
 
 if AEntry1 = nil then Exit;
 if AEntry2 = nil then Exit;
 
 {Check Dot}
 if (AEntry1.Attributes and faDot) = faDot then
  begin
   Result:=-1;
   Exit;
  end
 else if (AEntry2.Attributes and faDot) = faDot then
  begin
   Result:=1;
   Exit;
  end
 else
  begin
   {Check DotDot}
   if (AEntry1.Attributes and faDotDot) = faDotDot then
    begin
     Result:=-1;
     Exit;
    end
   else if (AEntry2.Attributes and faDotDot) = faDotDot then
    begin
     Result:=1;
     Exit;
    end
   else
    begin
     {Split Name}
     Name1:='';
     Name2:='';
     Ext1:='';
     Ext2:='';
     SplitFile(AEntry1.Name,Name1,Ext1);
     SplitFile(AEntry2.Name,Name2,Ext2);
     
     {Check Name}
     if (Length(Name1) <> 0) or (Length(Name2) <> 0) then
      begin
       WorkBuffer1:=PadString(Name1,Max(Length(Name1),Length(Name2)),False,AUnicode);
       WorkBuffer2:=PadString(Name2,Max(Length(Name1),Length(Name2)),False,AUnicode);
       Result:=CompareString(WorkBuffer1,WorkBuffer2,False);
       if Result <> 0 then Exit;
      end;
     
     {Check Extension}
     if (Length(Ext1) <> 0) or (Length(Ext2) <> 0) then
      begin
       WorkBuffer1:=PadString(Ext1,Max(Length(Ext1),Length(Ext2)),False,AUnicode);
       WorkBuffer2:=PadString(Ext2,Max(Length(Ext1),Length(Ext2)),False,AUnicode);
       Result:=CompareString(WorkBuffer1,WorkBuffer2,False);
       if Result <> 0 then Exit;
      end;
     
     {Check Version}
     if (Length(AEntry1.Version) <> 0) or (Length(AEntry2.Version) <> 0) then
      begin
       WorkBuffer1:=PadString(AEntry1.Version,Max(Length(AEntry1.Version),Length(AEntry2.Version)),True,AUnicode);
       WorkBuffer2:=PadString(AEntry2.Version,Max(Length(AEntry1.Version),Length(AEntry2.Version)),True,AUnicode);
       Result:=CompareString(WorkBuffer1,WorkBuffer2,True);
       if Result <> 0 then Exit;
      end;
    end;
  end;
end;

{=============================================================================}

function TCDFSFileSystem.PadString(const AString:String;ALength:Integer;APrefix,AUnicode:Boolean):String;
begin
 {}
 Result:=AString;
 
 while Length(Result) < ALength do
  begin
   if APrefix then
    begin
     if AUnicode then
      begin
       Result:=#0 + Result;
      end
     else
      begin
       Result:=#48 + Result;
      end;
    end
   else
    begin
     if AUnicode then
      begin
       Result:=Result + #0;
      end
     else
      begin
       Result:=Result + #32;
      end;
    end;
  end;
end;

{=============================================================================}

function TCDFSFileSystem.CompareString(const AString1,AString2:String;AReverse:Boolean):Integer;
var
 Count:Integer;
begin
 {}
 Result:=0;
 
 if Length(AString1) <> Length(AString2) then Exit;
 
 for Count:=1 to Length(AString1) do
  begin
   if AString1[Count] < AString2[Count] then
    begin
     Result:=-1;
     Exit;
    end
   else if AString1[Count] > AString2[Count] then
    begin
     Result:=1;
     Exit;
    end;
  end;
end;

{=============================================================================}

function TCDFSFileSystem.ChecksumValidationRecord(AValidation:PELTORITOValidationRecord):Word;
var
 Checksum:Word;
 Offset:LongWord;
begin
 {}
 Result:=0;
 
 Offset:=0;
 Checksum:=0;
 
 while Offset < SizeOf(TELTORITOValidationRecord) do
  begin
   Inc(Checksum,Word(Pointer(PtrUInt(AValidation) + Offset)^));
   Inc(Offset,SizeOf(Word));
  end;
  
 if Checksum > 0 then
  begin
   Result:=($FFFF - Checksum) + 1;
  end;
end;

{=============================================================================}

function TCDFSFileSystem.RecordToPath(ARecord:Pointer;APath:TCDFSDiskPath;AUnicode,AEndian:Boolean):Boolean;
{Loads a Path from a Path record}
{Note: Should only be called by LoadPath}
{Note: Caller must hold the tables and paths lock}
var
 Path:PCDFSPathRecord;
begin
 {}
 Result:=False;
 
 if ARecord = nil then Exit;
 if APath = nil then Exit;

 {Get Path}
 Path:=PCDFSPathRecord(ARecord);
 
 {Check Swap}
 if AEndian then
  begin
   {Read Path}
   APath.ExtendedSize:=Path.ExtendedSize;
   APath.ParentNumber:=WordBEToN(Path.ParentNumber);
   
   {Get Start}
   APath.StartCluster:=LongWordBEtoN(Path.FirstBlock);
   
   {Get Name}
   APath.Name:=CDFSIdentifierToFileName(Path.PathIdentifier[0],Path.PathIdentifierSize,AUnicode);
   
   Result:=True;
  end
 else
  begin
   {Read Path}
   APath.ExtendedSize:=Path.ExtendedSize;
   APath.ParentNumber:=Path.ParentNumber;
   
   {Get Start}
   APath.StartCluster:=Path.FirstBlock;
   
   {Get Name}
   APath.Name:=CDFSIdentifierToFileName(Path.PathIdentifier[0],Path.PathIdentifierSize,AUnicode);
   
   Result:=True;
  end;
end;

{=============================================================================}

function TCDFSFileSystem.PathToRecord(APath:TCDFSDiskPath;ARecord:Pointer;AUnicode,AEndian:Boolean):Boolean;
{Loads a Path record from a Path}
{Note: Should only be called by SetPath}
{Note: Caller must hold the tables and paths lock}
var
 Path:PCDFSPathRecord;
begin
 {}
 Result:=False;
 
 if APath = nil then Exit;
 if ARecord = nil then Exit;

 {Get Path}
 Path:=PCDFSPathRecord(ARecord);
 
 {Check Swap}
 if AEndian then
  begin
   {Write Path}
   Path.ExtendedSize:=APath.ExtendedSize;
   Path.ParentNumber:=WordNToBE(APath.ParentNumber);
   
   {Set Start}
   Path.FirstBlock:=LongWordNtoBE(APath.StartCluster);
   
   {Set Name}
   Path.PathIdentifierSize:=APath.PathIdentifierSize(AUnicode);
   if not CDFSFileNameToIdentifier(APath.PathIdentifier,Path.PathIdentifier[0],Path.PathIdentifierSize,AUnicode) then Exit;
   
   Result:=True;
  end
 else
  begin
   {Write Path}
   Path.ExtendedSize:=APath.ExtendedSize;
   Path.ParentNumber:=APath.ParentNumber;
   
   {Set Start}
   Path.FirstBlock:=APath.StartCluster;
   
   {Set Name}
   Path.PathIdentifierSize:=APath.PathIdentifierSize(AUnicode);
   if not CDFSFileNameToIdentifier(APath.PathIdentifier,Path.PathIdentifier[0],Path.PathIdentifierSize,AUnicode) then Exit;
   
   Result:=True;
  end;
end;

{=============================================================================}

function TCDFSFileSystem.RecordToEntry(ARecord:Pointer;AEntry:TCDFSDiskEntry;AUnicode:Boolean):Boolean;
{Loads an Entry from a Directory record}
{Note: Should only be called by LoadEntry and LoadRoot}
{Note: Caller must hold the entries lock}
var
 Count:LongWord;
 WorkBuffer:String;
 Directory:PCDFSDirectoryRecord;
begin
 {}
 Result:=False;
 
 if ARecord = nil then Exit;
 if AEntry = nil then Exit;
 
 {Get Directory}
 Directory:=PCDFSDirectoryRecord(ARecord);
 
 {Read Entry}
 AEntry.RecordSize:=Directory.RecordSize;
 AEntry.ExtendedSize:=Directory.ExtendedSize;
 AEntry.DataFree:=0;
 AEntry.DataSize:=Directory.DataSize;
 AEntry.UnitSize:=Directory.UnitSize;
 AEntry.InterleaveSize:=Directory.InterleaveSize;
 AEntry.SequenceNumber:=Directory.SequenceNumber;
 
 {Get Count}
 Count:=(Directory.DataSize shr FClusterShiftCount);
 if (Count shl FClusterShiftCount) < Directory.DataSize then Inc(Count);
 AEntry.ClusterCount:=Count;
 
 {Get Start}
 AEntry.StartCluster:=Directory.FirstBlock;
 
 {Get Times}
 AEntry.WriteTime:=CDFSTimeToFileTime(Directory.CreateTime);
 AEntry.CreateTime:=CDFSTimeToFileTime(Directory.CreateTime);
 AEntry.AccessTime:=cdfsNullFileTime;
 
 {Get Attributes}
 if FReadOnly then AEntry.Attributes:=(AEntry.Attributes or faReadOnly);
 if (Directory.FileFlags and cdfsFileFlagExistence) = cdfsFileFlagExistence then AEntry.Attributes:=(AEntry.Attributes or faHidden);
 if (Directory.FileFlags and cdfsFileFlagDirectory) = cdfsFileFlagDirectory then AEntry.Attributes:=(AEntry.Attributes or faDirectory);
 
 {Check Type}
 if (AEntry.Attributes and faDirectory) = faNone then
  begin
   {File}
   AEntry.Attributes:=(AEntry.Attributes or faFile);
   AEntry.EntriesLoaded:=True;
   
   {Get Size}
   AEntry.Size:=AEntry.DataSize;
   
   {Get Name}
   {AEntry.Name:=StripDot(StripVersion(CDFSIdentifierToFileName(Directory.FileIdentifier[0],Directory.FileIdentifierSize,AUnicode)));}
   WorkBuffer:=CDFSIdentifierToFileName(Directory.FileIdentifier[0],Directory.FileIdentifierSize,AUnicode);
   AEntry.Name:=StripDot(StripVersion(WorkBuffer));
   
   {Get Version}
   AEntry.Version:=GetVersion(WorkBuffer);
   
   Result:=True;
  end
 else if (AEntry.Attributes and faDirectory) = faDirectory then
  begin
   {Folder}
   AEntry.Attributes:=(AEntry.Attributes or faDirectory);
   
   {Get Size}
   AEntry.Size:=0;
   
   {Get Name}
   {AEntry.Name:=StripDot(StripVersion(CDFSIdentifierToFileName(Directory.FileIdentifier[0],Directory.FileIdentifierSize,AUnicode)));}
   WorkBuffer:=CDFSIdentifierToFileName(Directory.FileIdentifier[0],Directory.FileIdentifierSize,AUnicode);
   AEntry.Name:=StripDot(StripVersion(WorkBuffer));
   
   {Get Version}
   AEntry.Version:=GetVersion(WorkBuffer);
   
   {Check Relative}
   if AEntry.Name = cdfsDotName then AEntry.Attributes:=(AEntry.Attributes or faDot);
   if AEntry.Name = cdfsDotDotName then AEntry.Attributes:=(AEntry.Attributes or faDotDot);
   
   Result:=True;
  end;
end;

{=============================================================================}

function TCDFSFileSystem.EntryToRecord(AEntry:TCDFSDiskEntry;ARecord:Pointer;AUnicode:Boolean):Boolean;
{Loads a Directory record from an Entry}
{Note: Should only be called by SetEntry and SetRoot}
{Note: Caller must hold the entries lock}
var
 Directory:PCDFSDirectoryRecord;
begin
 {}
 Result:=False;
 
 if AEntry = nil then Exit;
 if ARecord = nil then Exit;
 
 {Get Directory}
 Directory:=PCDFSDirectoryRecord(ARecord);
 
 {Write Entry}
 Directory.RecordSize:=AEntry.DirectoryRecordSize;  {AEntry.RecordSize}
 Directory.ExtendedSize:=AEntry.ExtendedRecordSize; {AEntry.ExtendedSize}
 Directory.DataSize:=AEntry.DataSize;
 Directory.DataSizeM:=LongWordNtoBE(AEntry.DataSize);
 Directory.UnitSize:=AEntry.UnitSize;
 Directory.InterleaveSize:=AEntry.InterleaveSize;
 Directory.SequenceNumber:=AEntry.SequenceNumber;
 Directory.SequenceNumberM:=WordNToBE(AEntry.SequenceNumber);
 
 {Set Start}
 Directory.FirstBlock:=AEntry.StartCluster;
 Directory.FirstBlockM:=LongWordNtoBE(AEntry.StartCluster);
 
 {Set Time}
 if not FileTimeToCDFSTime(AEntry.WriteTime,Directory.CreateTime) then Exit;
 
 {Set Attributes}
 Directory.FileFlags:=cdfsFileFlagNone;
 if (AEntry.Attributes and faHidden) = faHidden then Directory.FileFlags:=(Directory.FileFlags or cdfsFileFlagExistence);
 if (AEntry.Attributes and faDirectory) = faDirectory then Directory.FileFlags:=(Directory.FileFlags or cdfsFileFlagDirectory);
 
 {Check Type}
 if (AEntry.Attributes and faDirectory) = faNone then
  begin
   {File}
   {Set Name}
   Directory.FileIdentifierSize:=AEntry.FileIdentifierSize;
   if not CDFSFileNameToIdentifier(AEntry.FileIdentifier,Directory.FileIdentifier[0],Directory.FileIdentifierSize,AUnicode) then Exit;
   
   Result:=True;
  end
 else if (AEntry.Attributes and faDirectory) = faDirectory then
  begin
   {Folder}
   {Check Relative}
   if (AEntry.Attributes and faDot) = faDot then
    begin
     {Set Name}
     Directory.FileIdentifierSize:=AEntry.FileIdentifierSize;
     if not CDFSFileNameToIdentifier(cdfsDotName,Directory.FileIdentifier[0],Directory.FileIdentifierSize,AUnicode) then Exit;
    end
   else if (AEntry.Attributes and faDotDot) = faDotDot then
    begin
     {Set Name}
     Directory.FileIdentifierSize:=AEntry.FileIdentifierSize;
     if not CDFSFileNameToIdentifier(cdfsDotDotName,Directory.FileIdentifier[0],Directory.FileIdentifierSize,AUnicode) then Exit;
    end
   else
    begin
     {Set Name}
     Directory.FileIdentifierSize:=AEntry.FileIdentifierSize;
     if not CDFSFileNameToIdentifier(AEntry.FileIdentifier,Directory.FileIdentifier[0],Directory.FileIdentifierSize,AUnicode) then Exit;
    end;
    
   Result:=True;
  end;
end;

{=============================================================================}

function TCDFSFileSystem.CDFSTypeToFileSysType(ACDFSType:TCDFSType):TFileSysType;
begin
 {}
 Result:=fsUNKNOWN;
 
 case ACDFSType of
  ctISO9660,ctJOLIET,ctSIERRA,ctROCKRIDGE:Result:=fsCDFS;
  ctUDF:Result:=fsUDF;
 end;
end;

{=============================================================================}

function TCDFSFileSystem.LoadMaxFile:Integer;
begin
 {}
 Result:=cdfsISO9660MaxFile;
 
 if FLongNames then Result:=cdfsJolietMaxFile;
end;

{=============================================================================}

function TCDFSFileSystem.LoadMaxPath:Integer;
begin
 {}
 Result:=cdfsISO9660MaxPath;
 
 if FLongNames then Result:=cdfsJolietMaxPath;
end;

{=============================================================================}

function TCDFSFileSystem.LoadAttributes:LongWord;
begin
 {}
 Result:=inherited LoadAttributes;
 
 {if FLongNames then Result:=(Result or vaCasePreserved or vaUnicode);} {Now inbuilt}
end;

{=============================================================================}

function TCDFSFileSystem.LoadSystemName:String;
begin
 {}
 Result:=cdfsBlankName;
 
 if FDriver = nil then Exit;
 
 case FCDFSType of
  ctISO9660,ctJOLIET,ctSIERRA,ctROCKRIDGE:Result:='CDFS';
  ctUDF:Result:='UDF';
 end;
end;

{=============================================================================}

function TCDFSFileSystem.LoadVolumeName:String;
var
 Descriptor:TCDFSDiskDescriptor;
begin
 {}
 Result:=cdfsBlankName;
 
 if not FDescriptors.ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
 
  Descriptor:=FPrimary;
  if FSupplementary <> nil then Descriptor:=FSupplementary;
  if Descriptor = nil then Exit;
 
  Result:=Trim(Descriptor.VolumeIdentifier);
 finally
  FDescriptors.ReaderUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.LoadVolumeSerial:LongWord;
var
 Descriptor:TCDFSDiskDescriptor;
begin
 {}
 Result:=0;
 
 if not FDescriptors.ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FClusterBuffer = nil then Exit;
 
  Descriptor:=FPrimary;
  if FSupplementary <> nil then Descriptor:=FSupplementary;
  if Descriptor = nil then Exit;
 
  {Get Cluster}
  if not ReadClusters(Descriptor.StartCluster,1,FClusterBuffer^) then Exit;
 
  {Calculate Volume Serial}
  Result:=CalculateVolumeSerial(FClusterBuffer,FClusterSize);
 finally
  FDescriptors.ReaderUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.LoadFileSysType:TFileSysType;
begin
 {}
 Result:=CDFSTypeToFileSysType(FCDFSType);
end;

{=============================================================================}

function TCDFSFileSystem.SetVolumeName(const AName:String):Boolean;
var
 Descriptor:TCDFSDiskDescriptor;
begin
 {}
 Result:=False;
 
 if not FDescriptors.WriterLock then Exit;
 try
  if FDriver = nil then Exit;

  {Check ReadOnly}
  if FReadOnly then Exit;
 
  {Get Primary}
  Descriptor:=FPrimary;
  if Descriptor = nil then Exit;
  Descriptor.VolumeIdentifier:=Uppercase(AName);
  Descriptor.VolumeSetIdentifier:=Uppercase(AName);
 
  {Set Primary}
  Result:=SetDescriptor(Descriptor);
 
  {Get Supplementary}
  if FSupplementary <> nil then
   begin
    Descriptor:=FSupplementary;
    if Descriptor = nil then Exit;
   
    Descriptor.VolumeIdentifier:=AName;
    Descriptor.VolumeSetIdentifier:=AName;
   
    {Set Supplementary}
    Result:=SetDescriptor(Descriptor);
   end;
 finally
  FDescriptors.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.SetVolumeSerial(ASerial:LongWord):Boolean;
var
 Descriptor:TCDFSDiskDescriptor;
begin
 {}
 Result:=False;
 
 if not FDescriptors.WriterLock then Exit;
 try
  if FDriver = nil then Exit;

  {Check ReadOnly}
  if FReadOnly then Exit;
 
  {Get Primary}
  Descriptor:=FPrimary;
  if FSupplementary <> nil then Descriptor:=FSupplementary;
  if Descriptor = nil then Exit;
 
  //To Do //Add value to ApplicationData to setup sum to be passed value ?
  //Result:=SetDescriptor(Descriptor); //To Do  //Primary and Supplementary Descriptors ? //See above
 finally
  FDescriptors.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.ReadEntry(AParent,AEntry:TDiskEntry;var ABuffer;const AStart:Int64;ACount:LongWord;var AOffset,AValue:LongWord):Integer;
{Read count bytes from the supplied entry beginning at the supplied start}
{Note: The caller must ensure the entry is large enough or the read will fail}
{Note: The offset and value parameters are not used by CDFS}
var
 Start:LongWord;        {Starting offset for Read from Cluster}
 Count:LongWord;        {Count of bytes to Read from Cluster}
 Remain:LongWord;       {Remaining bytes to Write to Buffer}
 Offset:LongWord;       {Offset for Write to Buffer}

 Length:LongWord;       {Number of whole clusters to Read from Entry}
 Cluster:LongWord;      {Absolute cluster number for Read from Entry}
begin
 {}
 Result:=0;

 if not FEntries.ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if AParent = nil then Exit;
  if AEntry = nil then Exit;
  if AEntry = FRoot then Exit; {Cannot read root}

  //To Do //Account for AEntry.ExtendedRecordSize

  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.ReadEntry Name = ' + AEntry.Name + ' Start = ' + IntToStr(AStart) + ' Count = ' + IntToStr(ACount));
  {$ENDIF}
  
  {Check Relative}
  if (AEntry.Attributes and (faDot or faDotDot)) <> faNone then Exit;
  
  {Check Entry}
  if (AEntry.Attributes and faMatchMask) <> faFile then Exit;
  
  {Get Cluster}
  Cluster:=TCDFSDiskEntry(AEntry).StartCluster + (AStart shr FClusterShiftCount);
  
  {Get Position}
  Offset:=0;
  Remain:=ACount;
  Start:=(AStart - ((AStart shr FClusterShiftCount) shl FClusterShiftCount));
  Count:=Min(ACount,(FClusterSize - Start));
  
  {Get Length}
  Length:=(ACount shr FClusterShiftCount);
  
  {Read Clusters}
  while Remain > 0 do
   begin
    if Start > 0 then
     begin
      {Partial Cluster} {First}
      if not ReadLock then Exit; 
      try
       if FReadBuffer = nil then Exit;

       {Read Clusters}
       if not ReadClusters(Cluster,1,FReadBuffer^) then Break;
      
       {Read Buffer}
       System.Move(Pointer(PtrUInt(FReadBuffer) + Start)^,Pointer(PtrUInt(@ABuffer) + Offset)^,Count);
      
       {Update Cluster}
       Inc(Cluster);
      
       {Update Position}
       Inc(Offset,Count);
       Dec(Remain,Count);
       Start:=0;
       Count:=Min(Remain,FClusterSize);
      
       {Update Length}
       Length:=(Remain shr FClusterShiftCount);
      finally  
       ReadUnlock;
      end; 
     end
    else
     begin
      if Length > 0 then
       begin
        {Full Clusters}
        {Read Clusters}
        if not ReadClusters(Cluster,Length,Pointer(PtrUInt(@ABuffer) + Offset)^) then Break;
        
        {Update Cluster}
        Inc(Cluster,Length);
        
        {Update Position}
        Inc(Offset,(Length shl FClusterShiftCount));
        Dec(Remain,(Length shl FClusterShiftCount));
        
        {Start:=0;} {Already 0}
        Count:=Min(Remain,FClusterSize);
        
        {Update Length}
        Length:=0;
       end
      else
       begin
        {Partial Cluster} {Last}
        if not ReadLock then Exit; 
        try
         if FReadBuffer = nil then Exit;
         
         {Read Clusters}
         if not ReadClusters(Cluster,1,FReadBuffer^) then Break;
        
         {Read Buffer}
         System.Move(FReadBuffer^,Pointer(PtrUInt(@ABuffer) + Offset)^,Count);
        
         {Update Cluster}
         {Inc(Cluster);} {Must be last Read}
        
         {Update Position}
         Inc(Offset,Count);
         Dec(Remain,Count);
         {Start:=0;} {Already 0}
         Count:=Min(Remain,FClusterSize);
        
         {Update Length}
         {Length:=0;} {Already 0}
        finally  
         ReadUnlock;
        end; 
       end;
     end;
   end;
   
  Result:=(ACount - Remain);
 finally
  FEntries.ReaderUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.WriteEntry(AParent,AEntry:TDiskEntry;const ABuffer;const AStart:Int64;ACount:LongWord;var AOffset,AValue:LongWord):Integer;
{Write count bytes to the supplied entry beginning at the supplied start}
{Note: The caller must ensure the entry is large enough or the write will fail}
{Note: The offset and value parameters are not used by CDFS}
var
 Start:LongWord;        {Starting offset for Write to Cluster}
 Count:LongWord;        {Count of bytes to Write to Cluster}
 Remain:LongWord;       {Remaining bytes to Read from Buffer}
 Offset:LongWord;       {Offset for Read from Buffer}

 Length:LongWord;       {Number of whole clusters to Write to Entry}
 Cluster:LongWord;      {Absolute cluster number for Write to Entry}
begin
 {}
 Result:=0;

 if not FEntries.ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if AParent = nil then Exit;
  if AEntry = nil then Exit;
  if AEntry = FRoot then Exit; {Cannot write root}

  //To Do //Account for AEntry.ExtendedRecordSize

  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.WriteEntry Name = ' + AEntry.Name + ' Start = ' + IntToStr(AStart) + ' Count = ' + IntToStr(ACount));
  {$ENDIF}
  
  {Check ReadOnly}
  if FReadOnly then Exit;
  
  {Check Relative}
  if (AEntry.Attributes and (faDot or faDotDot)) <> faNone then Exit;
  
  {Check Entry}
  if (AEntry.Attributes and faMatchMask) <> faFile then Exit;
  
  {Get Cluster}
  Cluster:=TCDFSDiskEntry(AEntry).StartCluster + (AStart shr FClusterShiftCount);
  
  {Get Position}
  Offset:=0;
  Remain:=ACount;
  Start:=(AStart - ((AStart shr FClusterShiftCount) shl FClusterShiftCount));
  Count:=Min(ACount,(FClusterSize - Start));
  
  {Get Length}
  Length:=(ACount shr FClusterShiftCount);
  
  {Write Clusters}
  while Remain > 0 do
   begin
    if Start > 0 then
     begin
      {Partial Cluster} {First}
      if not WriteLock then Exit; 
      try
       if FWriteBuffer = nil then Exit;
 
       {Read Clusters}
       if not ReadClusters(Cluster,1,FWriteBuffer^) then Break;
      
       {Write Buffer}
       System.Move(Pointer(PtrUInt(@ABuffer) + Offset)^,Pointer(PtrUInt(FWriteBuffer) + Start)^,Count);
      
       {Write Clusters}
       if not WriteClusters(Cluster,1,FWriteBuffer^) then Break;
      
       {Update Cluster}
       Inc(Cluster);
      
       {Update Position}
       Inc(Offset,Count);
       Dec(Remain,Count);
       Start:=0;
       Count:=Min(Remain,FClusterSize);
      
       {Update Length}
       Length:=(Remain shr FClusterShiftCount);
      finally  
       WriteUnlock;
      end; 
     end
    else
     begin
      if Length > 0 then
       begin
        {Full Clusters}
        {Write Clusters}
        if not WriteClusters(Cluster,Length,Pointer(PtrUInt(@ABuffer) + Offset)^) then Break;
        
        {Update Cluster}
        Inc(Cluster,Length);
        
        {Update Position}
        Inc(Offset,(Length shl FClusterShiftCount));
        Dec(Remain,(Length shl FClusterShiftCount));
        
        {Start:=0;} {Already 0}
        Count:=Min(Remain,FClusterSize);
        
        {Update Length}
        Length:=0;
       end
      else
       begin
        {Partial Cluster} {Last}
        if not WriteLock then Exit; 
        try
         if FWriteBuffer = nil then Exit;
         
         {Read Clusters}
         if not ReadClusters(Cluster,1,FWriteBuffer^) then Break;
        
         {Write Buffer}
         System.Move(Pointer(PtrUInt(@ABuffer) + Offset)^,FWriteBuffer^,Count);
        
         {Write Clusters}
         if not WriteClusters(Cluster,1,FWriteBuffer^) then Break;
        
         {Update Cluster}
         {Inc(Cluster);} {Must be last Read}
        
         {Update Position}
         Inc(Offset,Count);
         Dec(Remain,Count);
         {Start:=0;} {Already 0}
         Count:=Min(Remain,FClusterSize);
        
         {Update Length}
         {Length:=0;} {Already 0}
        finally  
         WriteUnlock;
        end; 
       end;
     end;
   end;
   
  Result:=(ACount - Remain);
 finally
  FEntries.ReaderUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.LoadTrees:Boolean;
{Load all entries in all trees}
{Note: Should only be called by PrepareTrees and LoadBlocks}
var
 Entry:TDiskEntry;
begin
 {}
 Result:=False;
 
 if FDriver = nil then Exit;

 {Check Loaded}
 if not FTreesLoaded then
  begin
   if not FEntries.WriterLock then Exit;
   try
    {Check Loaded (After Lock)}
    if not FTreesLoaded then
     begin
      {$IFDEF CDFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.LoadTrees');
      {$ENDIF}
    
      Entry:=TDiskEntry(FEntries.First);
      while Entry <> nil do
       begin
        if not LoadTree(Entry) then Exit;
     
        Entry:=TDiskEntry(Entry.Next);
       end;
    
      FTreesLoaded:=True;
     end;
   finally
    FEntries.WriterUnlock;
   end; 
  end;
  
 Result:=True;
end;

{=============================================================================}

function TCDFSFileSystem.LoadTables:Boolean;
{Load each of the path tables}
var
 Instance:Byte;
 TableNo:LongWord;
 Descriptor:TCDFSDiskDescriptor;
begin
 {}
 Result:=False;
 
 if not FTables.WriterLock then Exit;
 try
  if FDriver = nil then Exit;

  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.LoadTables');
  {$ENDIF}
 
  if not FDescriptors.ReaderLock then Exit;
  try
   {Get Primary Descriptors}
   Instance:=1;
   Descriptor:=GetDescriptor(cdfsVolumeDescriptorTypePrimary,Instance);
   while Descriptor <> nil do
    begin
     {Load Primary Tables}
     if Descriptor.PrimaryPathTable <> 0 then
      begin
       TableNo:=(cdfsPathTableTypePrimary shl cdfsTableTypeShift) or (Instance shl cdfsTableInstanceShift) or (cdfsVolumeDescriptorTypePrimary shl cdfsTableDescriptorShift);
       if GetTableEx(TableNo,True) = nil then Exit;
      end;
      
     if Descriptor.PrimaryPathTableM <> 0 then
      begin
       TableNo:=(cdfsPathTableTypePrimaryM shl cdfsTableTypeShift) or (Instance shl cdfsTableInstanceShift) or (cdfsVolumeDescriptorTypePrimary shl cdfsTableDescriptorShift);
       if GetTableEx(TableNo,True) = nil then Exit;
      end;
     
     {Load Alternate Tables}
     if Descriptor.AlternatePathTable <> 0 then
      begin
       TableNo:=(cdfsPathTableTypeAlternate shl cdfsTableTypeShift) or (Instance shl cdfsTableInstanceShift) or (cdfsVolumeDescriptorTypePrimary shl cdfsTableDescriptorShift);
       if GetTableEx(TableNo,True) = nil then Exit;
      end;
      
     if Descriptor.AlternatePathTableM <> 0 then
      begin
       TableNo:=(cdfsPathTableTypeAlternateM shl cdfsTableTypeShift) or (Instance shl cdfsTableInstanceShift) or (cdfsVolumeDescriptorTypePrimary shl cdfsTableDescriptorShift);
       if GetTableEx(TableNo,True) = nil then Exit;
      end;
     
     Inc(Instance);
   
     Descriptor:=GetDescriptor(cdfsVolumeDescriptorTypePrimary,Instance);
    end;
  
   {Get Supplementary Descriptors}
   Instance:=1;
   Descriptor:=GetDescriptor(cdfsVolumeDescriptorTypeSupplementary,Instance);
   while Descriptor <> nil do
    begin
     {Load Primary Tables}
     if Descriptor.PrimaryPathTable <> 0 then
      begin
       TableNo:=(cdfsPathTableTypePrimary shl cdfsTableTypeShift) or (Instance shl cdfsTableInstanceShift) or (cdfsVolumeDescriptorTypeSupplementary shl cdfsTableDescriptorShift);
       if GetTableEx(TableNo,True) = nil then Exit;
      end;
      
     if Descriptor.PrimaryPathTableM <> 0 then
      begin
       TableNo:=(cdfsPathTableTypePrimaryM shl cdfsTableTypeShift) or (Instance shl cdfsTableInstanceShift) or (cdfsVolumeDescriptorTypeSupplementary shl cdfsTableDescriptorShift);
       if GetTableEx(TableNo,True) = nil then Exit;
      end;
     
     {Load Alternate Tables}
     if Descriptor.AlternatePathTable <> 0 then
      begin
       TableNo:=(cdfsPathTableTypeAlternate shl cdfsTableTypeShift) or (Instance shl cdfsTableInstanceShift) or (cdfsVolumeDescriptorTypeSupplementary shl cdfsTableDescriptorShift);
       if GetTableEx(TableNo,True) = nil then Exit;
      end;
      
     if Descriptor.AlternatePathTableM <> 0 then
      begin
       TableNo:=(cdfsPathTableTypeAlternateM shl cdfsTableTypeShift) or (Instance shl cdfsTableInstanceShift) or (cdfsVolumeDescriptorTypeSupplementary shl cdfsTableDescriptorShift);
       if GetTableEx(TableNo,True) = nil then Exit;
      end;
      
     Inc(Instance);
   
     Descriptor:=GetDescriptor(cdfsVolumeDescriptorTypeSupplementary,Instance);
    end;
  finally
   FDescriptors.ReaderUnlock;
  end; 
  
  Result:=True;
 finally
  FTables.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.LoadBlocks:Boolean;
{Note: Should only be called by Alloc/ReleaseClusters and GetNextFreeCluster/GetFreeClusterCount}
var
 BlockNo:LongWord;
 Block:TCDFSDiskBlock;
 Path:TCDFSDiskPath;
 Entry:TCDFSDiskEntry;
 Header:TCDFSDiskHeader;
 Catalog:TCDFSDiskCatalog;
 Extension:TCDFSDiskExtension;
 Descriptor:TCDFSDiskDescriptor;
begin
 {}
 Result:=False;
 
 if FDriver = nil then Exit;
 if FTotalBlockCount = 0 then Exit;

 {Check Loaded}
 if not FBlocksLoaded then
  begin
   if not FBlocks.WriterLock then Exit;
   try
    {Check Loaded (After Lock)}
    if not FBlocksLoaded then
     begin
      {$IFDEF CDFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.LoadBlocks');
      {$ENDIF}
   
      {Load Trees}
      if not LoadTrees then Exit;
   
      {Load Catalogs}
      if not LoadCatalogs then Exit;
   
      {Get each Block}
      BlockNo:=0;
      while BlockNo < FTotalBlockCount do
       begin
        if GetBlockEx(BlockNo,True) = nil then Exit;
        
        Inc(BlockNo);
       end;
      
      {Mark used Clusters}
      {Reserved Clusters (0 to 15)}
      {Mark Clusters}
      if not MarkClusters(cdfsFirstSector,cdfsReservedSectors,True) then Exit;

      if not FDescriptors.WriterLock then Exit;
      try
       {Descriptor Clusters}
       Descriptor:=TCDFSDiskDescriptor(FDescriptors.First);
       while Descriptor <> nil do
        begin
         {Mark Clusters}
         if not MarkClusters(Descriptor.StartCluster,1,True) then Exit;
     
         Descriptor:=TCDFSDiskDescriptor(Descriptor.Next); 
        end;

       {Table Clusters}
       Descriptor:=TCDFSDiskDescriptor(FDescriptors.First);
       while Descriptor <> nil do
        begin
         if Descriptor.Primary <> nil then
          begin
           {Mark Clusters}
           if not MarkClusters(Descriptor.Primary.StartCluster,Descriptor.Primary.ClusterCount,True) then Exit;
          end;
         if Descriptor.PrimaryM <> nil then
          begin
           {Mark Clusters}
           if not MarkClusters(Descriptor.PrimaryM.StartCluster,Descriptor.PrimaryM.ClusterCount,True) then Exit;
          end;
         if Descriptor.Alternate <> nil then
          begin
           {Mark Clusters}
           if not MarkClusters(Descriptor.Alternate.StartCluster,Descriptor.Alternate.ClusterCount,True) then Exit;
          end;
         if Descriptor.AlternateM <> nil then
          begin
           {Mark Clusters}
           if not MarkClusters(Descriptor.AlternateM.StartCluster,Descriptor.AlternateM.ClusterCount,True) then Exit;
          end;
       
         Descriptor:=TCDFSDiskDescriptor(Descriptor.Next); 
        end;

       {Entry Clusters}
       Descriptor:=TCDFSDiskDescriptor(FDescriptors.First);
       while Descriptor <> nil do
        begin
         if Descriptor.Root <> nil then
          begin
           if not FEntries.WriterLock then Exit;
           try
            {Mark Clusters}
            if not MarkClusters(Descriptor.Root.StartCluster,Descriptor.Root.ClusterCount,True) then Exit;
       
            {Get Entries}
            Entry:=TCDFSDiskEntry(Descriptor.Root.FirstChild); 
            while Entry <> nil do
             begin
              if (Entry.Attributes and (faDot or faDotDot)) = faNone then
               begin
                if not MarkTree(Entry) then Exit;
               end;
         
              Entry:=TCDFSDiskEntry(Entry.Next);
             end;
           finally
            FEntries.WriterUnlock;
           end; 
          end;
      
         Descriptor:=TCDFSDiskDescriptor(Descriptor.Next);
        end; 

       {Catalog Clusters}
       Descriptor:=FBoot;
       if Descriptor <> nil then
        begin
         if Descriptor.CatalogStart <> 0 then
          begin
           if not FHeaders.WriterLock then Exit;
           try
            {Headers}
            Header:=TCDFSDiskHeader(FHeaders.First); 
            while Header <> nil do
             begin
              {Mark Clusters}
              if not MarkClusters(Header.HeaderCluster,1,True) then Exit;
             
              Header:=TCDFSDiskHeader(Header.Next); 
             end;
           finally
            FHeaders.WriterUnlock;
           end;          
           
           if not FCatalogs.WriterLock then Exit;
           try
            {Catalogs}
            Catalog:=TCDFSDiskCatalog(FCatalogs.First); 
            while Catalog <> nil do
             begin
              {Mark Clusters}
              if not MarkClusters(Catalog.CatalogCluster,1,True) then Exit;
              if not MarkClusters(Catalog.StartCluster,Catalog.ClusterCount,True) then Exit;
              
              Catalog:=TCDFSDiskCatalog(Catalog.Next);
             end;
           finally
            FCatalogs.WriterUnlock;
           end;          
          
           if not FExtensions.WriterLock then Exit;
           try
            {Extensions}
            Extension:=TCDFSDiskExtension(FExtensions.First); 
            while Extension <> nil do
             begin
              {Mark Clusters}
              if not MarkClusters(Extension.ExtensionCluster,1,True) then Exit; 
            
              Extension:=TCDFSDiskExtension(Extension.Next); 
             end;
           finally
            FExtensions.WriterUnlock;
           end;          
          end;
        end; 
      finally
       FDescriptors.WriterUnlock;
      end; 
       
      FBlocksLoaded:=True;
     end;
   finally
    FBlocks.WriterUnlock;
   end; 
  end;
  
 Result:=True;
end;

{=============================================================================}

function TCDFSFileSystem.LoadDescriptors:Boolean;
{Load each of the volume descriptors}
var
 Cluster:LongWord;
begin
 {}
 Result:=False;
 
 if not FDescriptors.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FClusterBuffer = nil then Exit;

  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.LoadDescriptors');
  {$ENDIF}
  
  {Get Start Cluster}
  Cluster:=cdfsISO9660StartSector;
  if not ReadClusters(Cluster,1,FClusterBuffer^) then Exit;
  
  while LoadDescriptor(FClusterBuffer,Cluster) do
   begin
    {Check for Terminator}
    if PCDFSVolumeDescriptorHeader(FClusterBuffer).DescriptorType = cdfsVolumeDescriptorTypeTerminator then Break;

    {Get Next Cluster}
    Inc(Cluster);
    
    if not ReadClusters(Cluster,1,FClusterBuffer^) then Exit;
   end;
   
  Result:=True;
 finally
  FDescriptors.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.LoadRoots:Boolean;
{Note: Caller must hold the descriptors lock}
begin
 {}
 Result:=False;
 
 if not FEntries.WriterLock then Exit;
 try
  if FDriver = nil then Exit;

  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.LoadRoots');
  {$ENDIF}
 
  {Load Primary}
  if FPrimary <> nil then
   begin
    if GetRoot(FPrimary) = nil then Exit;
   end;
  
  {Load Supplementary}
  if FSupplementary <> nil then
   begin
    if GetRoot(FSupplementary) = nil then Exit;
   end;
  
  Result:=True;
 finally
  FEntries.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.LoadPaths(ATable:TCDFSDiskTable):Boolean;
{Load the path records of the table as one single block}
{Note: Caller must hold the tables lock}
var
 Number:Word;
 Uneven:Boolean;

 Size:LongWord;
 Start:LongWord;
 Count:LongWord;
 Buffer:Pointer;
 Offset:LongWord;
begin
 {}
 Result:=False;
 
 if FDriver = nil then Exit;
 if ATable = nil then Exit;

 {Check Loaded}
 if not ATable.PathsLoaded then
  begin
   if not ATable.Paths.WriterLock then Exit;
   try
    {Check Loaded (After Lock)}
    if not ATable.PathsLoaded then
     begin
      {$IFDEF CDFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.LoadPaths - Table = ' + IntToHex(ATable.TableNo,8) + ' Start = ' + IntToStr(ATable.StartCluster));
      {$ENDIF}
      
      {Get Start}
      Start:=TCDFSDiskTable(ATable).StartCluster;
      Count:=TCDFSDiskTable(ATable).ClusterCount;
      if (Start = 0) or (Count = 0) then Exit;
      
      {Get Size}
      if ATable.Descriptor = nil then Exit;
      Size:=ATable.Descriptor.PathTableSize;
      
      {Get Buffer}
      Buffer:=GetMem((Count shl FClusterShiftCount));
      if Buffer = nil then Exit;
      try
       {Read Clusters}
       if not ReadClusters(Start,Count,Buffer^) then Exit;
       
       {Get Offset}
       Offset:=0;
       Number:=cdfsRootPathNumber;
       while Offset < Size do
        begin
         {Check Record}
         if PCDFSPathRecord(PtrUInt(Buffer) + Offset).PathIdentifierSize < cdfsPathIdentifierSize then Break;
         
         {Load Path}
         if not LoadPath(ATable,Buffer,Offset,Number) then Exit;
         
         {Update Offset}
         Uneven:=((PCDFSPathRecord(PtrUInt(Buffer) + Offset).PathIdentifierSize and cdfsUnevenSize) = cdfsUnevenSize);
         Inc(Offset,PCDFSPathRecord(PtrUInt(Buffer) + Offset).PathIdentifierSize + (cdfsPathRecordSize - 1));
         if Uneven then Inc(Offset);
         Inc(Number);
        end;
      finally
       FreeMem(Buffer);
      end;
      
      ATable.PathsLoaded:=True;
     end; 
   finally
    ATable.Paths.WriterUnlock;
   end; 
  end;
  
 Result:=True;
end;

{=============================================================================}

function TCDFSFileSystem.LoadEntries(AParent:TDiskEntry):Boolean;
{Load the directory entries of the parent one cluster at a time}
var
 Start:LongWord;
 Count:LongWord;
 Offset:LongWord;
 Cluster:LongWord;
begin
 {}
 Result:=False;
 
 if FDriver = nil then Exit;
 if AParent = nil then Exit;
 if FClusterBuffer = nil then Exit;

 {Check Loaded}
 if not AParent.EntriesLoaded then
  begin
   if not FEntries.WriterLock then Exit;
   try
    {Check Loaded (After Lock)}
    if not AParent.EntriesLoaded then
     begin
      {$IFDEF CDFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.LoadEntries - Parent = ' + AParent.Name);
      {$ENDIF}
      
      {Check Directory}
      if (AParent.Attributes and faDirectory) = faDirectory then
       begin
        {Get Start}
        Start:=TCDFSDiskEntry(AParent).StartCluster;
        Count:=TCDFSDiskEntry(AParent).ClusterCount;
        if (Start = 0) or (Count = 0) then Exit;
        
        {Get Cluster}
        Cluster:=Start;
        while Cluster < (Start + Count) do
         begin
          {Read Cluster}
          if not ReadClusters(Cluster,1,FClusterBuffer^) then Exit;
          
          {Get Offset}
          Offset:=0;
          while (Offset < FClusterSize) and (Offset < TCDFSDiskEntry(AParent).DataSize) do
           begin
            {Check Record}
            if PCDFSDirectoryRecord(PtrUInt(FClusterBuffer) + Offset).RecordSize < cdfsDirectoryRecordSize then Break;
            
            {Load Entry}
            if not LoadEntry(TCDFSDiskEntry(AParent),FClusterBuffer,Offset,Cluster) then Exit;
            
            {Update Offset}
            Inc(Offset,PCDFSDirectoryRecord(PtrUInt(FClusterBuffer) + Offset).RecordSize);
           end;
          
          {Update Cluster}
          Inc(Cluster);
         end;
       end;
    
      AParent.EntriesLoaded:=True;
     end;
   finally
    FEntries.WriterUnlock;
   end; 
  end;
  
 Result:=True;
end;

{=============================================================================}

function TCDFSFileSystem.LoadCatalogs:Boolean;
{Includes LoadHeaders/LoadExtensions}
var
 Count:Word;
 Start:LongWord;
 Offset:LongWord;
 Cluster:LongWord;
 Completed:Boolean;

 HeaderNo:LongWord;
 CatalogNo:LongWord;
 ExtensionNo:LongWord;

 Initial:TCDFSDiskCatalog;
 Validation:TCDFSDiskHeader;
 Header:TCDFSDiskHeader;
 Catalog:TCDFSDiskCatalog;
 Extension:TCDFSDiskExtension;

 Descriptor:TCDFSDiskDescriptor;
begin
 {}
 Result:=False;
 
 if FDriver = nil then Exit;
 if FClusterBuffer = nil then Exit;

 {Check Loaded}
 if not FCatalogsLoaded then
  begin
   if not FCatalogsLoading then
    begin
     if not FCatalogs.WriterLock then Exit;
     try
      if not FHeaders.WriterLock then Exit;
      try
       if not FExtensions.WriterLock then Exit;
       try
        {Check Loaded (After Lock)}
        if not FCatalogsLoaded then
         begin
          FCatalogsLoading:=True;
          try
           {$IFDEF CDFS_DEBUG}
           if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.LoadCatalogs');
           {$ENDIF}
           
           {Get Boot}
           Descriptor:=FBoot;
           if Descriptor <> nil then
            begin
             {Get Start}
             Start:=Descriptor.CatalogStart;
             if Start = 0 then Exit;
           
             {Setup Parameters}
             Count:=0;
             Completed:=False;
             HeaderNo:=1;
             CatalogNo:=1;
             ExtensionNo:=1;
             Initial:=nil;
             Validation:=nil;
             Header:=nil;
             Catalog:=nil;
             Extension:=nil;
           
             {Get Cluster}
             Cluster:=Start;
             Descriptor.CatalogCount:=1;
             while Cluster > 0 do
              begin
               {Read Cluster}
               if not ReadClusters(Cluster,1,FClusterBuffer^) then Exit;
             
               {Get Offset}
               Offset:=0;
               while Offset < FClusterSize do
                begin
                 if Validation = nil then
                  begin
                   {Check Validation}
                   if PELTORITOValidationRecord(PtrUInt(FClusterBuffer) + Offset).HeaderId <> cdfsElToritoHeaderId then Completed:=True;
                   if Completed then Break;
                 
                   {Load Header}
                   if not LoadHeader(FClusterBuffer,Offset,Cluster,HeaderNo) then Completed:=True;
                   if Completed then Break;
                 
                   Validation:=GetHeaderEx(HeaderNo,True);
                   if Validation = nil then Exit;
                   Descriptor.Validation:=Validation;
                 
                   Inc(HeaderNo);
                  end
                 else
                  begin
                   if Initial = nil then
                    begin
                     {Check Initial}
                     if (PELTORITODefaultRecord(PtrUInt(FClusterBuffer) + Offset).BootIndicator <> cdfsElToritoBootIndicator) and (PELTORITODefaultRecord(PtrUInt(FClusterBuffer) + Offset).BootIndicator <> cdfsElToritoNoBootIndicator) then Completed:=True;
                     if Completed then Break;
                     
                     {Load Catalog}
                     if not LoadCatalog(FClusterBuffer,Offset,Cluster,CatalogNo,True) then Completed:=True;
                     if Completed then Break;
                   
                     Initial:=TCDFSDiskCatalog(GetCatalogEx(CatalogNo,True)); 
                     if Initial = nil then Exit;
                     Descriptor.Initial:=Initial;
                     Initial.Validation:=Validation;
                    
                     Inc(CatalogNo);
                    end
                   else
                    begin
                     {Check Header/Catalog/Extension}
                     if (PELTORITOSectionHeader(PtrUInt(FClusterBuffer) + Offset).HeaderIndicator = cdfsElToritoHeaderIndicator) or (PELTORITOSectionHeader(PtrUInt(FClusterBuffer) + Offset).HeaderIndicator = cdfsElToritoHeaderTerminator) then
                      begin
                       {Check Header}
                       if Count > 0 then Completed:=True;
                       if Header <> nil then if Header.HeaderIndicator = cdfsElToritoHeaderTerminator then Completed:=True;
                       if Completed then Break;
                       Header:=nil;
                       Catalog:=nil;
                       Extension:=nil;
                       
                       {Load Header}
                       if not LoadHeader(FClusterBuffer,Offset,Cluster,HeaderNo) then Completed:=True;
                       if Completed then Break;
                     
                       Header:=GetHeaderEx(HeaderNo,True);
                       if Header = nil then Exit;
                       Count:=Header.SectionCount;
                     
                       Inc(HeaderNo);
                      end
                     else if (PELTORITOSectionRecord(PtrUInt(FClusterBuffer) + Offset).BootIndicator = cdfsElToritoBootIndicator) or (PELTORITOSectionRecord(PtrUInt(FClusterBuffer) + Offset).BootIndicator = cdfsElToritoNoBootIndicator) then
                      begin
                       {Check Catalog}
                       if Count = 0 then Completed:=True;
                       if Header = nil then Completed:=True;
                       if Completed then Break;
                       Catalog:=nil;
                       Extension:=nil;
                     
                       {Load Catalog}
                       if not LoadCatalog(FClusterBuffer,Offset,Cluster,CatalogNo,False) then Completed:=True;
                       if Completed then Break;
                     
                       Catalog:=TCDFSDiskCatalog(GetCatalogEx(CatalogNo,True)); 
                       if Catalog = nil then Exit;
                       Catalog.Header:=Header;
                     
                       Dec(Count);
                       Inc(CatalogNo);
                      end
                     else if PELTORITOSectionExtension(PtrUInt(FClusterBuffer) + Offset).ExtensionIndicator = cdfsElToritoExtensionIndicator then
                      begin
                       {Check Extension}
                       if Extension <> nil then if (Extension.ExtensionFlag and cdfsElToritoExtensionFlagExtension) <> cdfsElToritoExtensionFlagExtension then Completed:=True;
                       if Catalog = nil then Completed:=True;
                       if Completed then Break;
                       Extension:=nil;
                     
                       {Load Extension}
                       if not LoadExtension(FClusterBuffer,Offset,Cluster,ExtensionNo) then Completed:=True;
                       if Completed then Break;
                     
                       Extension:=GetExtensionEx(ExtensionNo,True);
                       if Extension = nil then Exit;
                       Extension.Catalog:=Catalog;
                     
                       Inc(ExtensionNo);
                      end;
                    end;
                  end;
               
                 {Update Offset}
                 Inc(Offset,cdfsCatalogRecordSize); {Same as cdfsHeaderRecordSize, cdfsExtensionRecordSize}
                end;
             
               if Completed then Break;
             
               {Update Cluster}
               Inc(Cluster);
             
               Descriptor.CatalogCount:=Descriptor.CatalogCount + 1;
              end;
            end;
     
           FCatalogsLoaded:=True;
          finally
           FCatalogsLoading:=False;
          end;
         end;
       finally
        FExtensions.WriterUnlock;
       end; 
      finally
       FHeaders.WriterUnlock;
      end; 
     finally
      FCatalogs.WriterUnlock;
     end; 
    end;
  end;
  
 Result:=True;
end;

{=============================================================================}

function TCDFSFileSystem.SetPaths(ATable:TCDFSDiskTable):Boolean;
{Note: Caller must hold the tables lock}
var
 Size:LongWord;
 Start:LongWord;
 Count:LongWord;
 Buffer:Pointer;
 Path:TCDFSDiskPath;
begin
 {}
 Result:=False;
 
 if FDriver = nil then Exit;
 if ATable = nil then Exit;

 {Check Loaded}
 if ATable.PathsLoaded then
  begin
   if not ATable.Paths.WriterLock then Exit;
   try
    {$IFDEF CDFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.SetPaths - Table = ' + IntToHex(ATable.TableNo,8) + ' Start = ' + IntToStr(ATable.StartCluster));
    {$ENDIF}
    
    {Get Start}
    Start:=TCDFSDiskTable(ATable).StartCluster;
    Count:=TCDFSDiskTable(ATable).ClusterCount;
    if (Start = 0) or (Count = 0) then Exit;
    
    {Get Size}
    if ATable.Descriptor = nil then Exit;
    Size:=ATable.Descriptor.PathTableSize;
    
    {Get Buffer}
    Buffer:=GetMem((Count shl FClusterShiftCount));
    if Buffer = nil then Exit;
    try
     {Read Clusters}
     if not ReadClusters(Start,Count,Buffer^) then Exit;
     
     {Zero Clusters}
     ZeroMemory(Buffer,(Count shl FClusterShiftCount));
     
     {Get Paths}
     Path:=TCDFSDiskPath(TCDFSDiskTable(ATable).Paths.First);
     while Path <> nil do
      begin
       {Set Path}
       if not SetPath(ATable,Path,Buffer) then Exit;
       
       Path:=TCDFSDiskPath(Path.Next);
      end;
     
     {Write Clusters}
     if not WriteClusters(Start,Count,Buffer^) then Exit;
     
     Result:=True;
    finally
     FreeMem(Buffer);
    end;
   finally
    ATable.Paths.WriterUnlock;
   end; 
  end;
end;

{=============================================================================}

function TCDFSFileSystem.SetEntries(AParent:TDiskEntry):Boolean;
var
 Start:LongWord;
 Count:LongWord;
 Cluster:LongWord;
 Entry:TCDFSDiskEntry;
begin
 {}
 Result:=False;
 
 if FDriver = nil then Exit;
 if AParent = nil then Exit;
 if FClusterBuffer = nil then Exit;

 {Check Loaded}
 if AParent.EntriesLoaded then
  begin
   if not FEntries.WriterLock then Exit;
   try
    {$IFDEF CDFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.SetEntries - Parent = ' + AParent.Name);
    {$ENDIF}
    
    {Check Directory}
    if (AParent.Attributes and faDirectory) = faDirectory then
     begin
      FBlockWrite:=True;
      try
       {Get Start}
       Start:=TCDFSDiskEntry(AParent).StartCluster;
       Count:=TCDFSDiskEntry(AParent).ClusterCount;
       if (Start = 0) or (Count = 0) then Exit;
       
       {Get Cluster}
       Cluster:=Start;
       
       {Read Cluster}
       if not ReadClusters(Cluster,1,FClusterBuffer^) then Exit;
       
       {Zero Cluster}
       ZeroMemory(FClusterBuffer,FClusterSize);
       
       {Get Entries}
       Entry:=TCDFSDiskEntry(AParent.FirstChild);
       while Entry <> nil do
        begin
         {Check Cluster}
         if Entry.EntryCluster > Cluster then
          begin
           {Write Cluster}
           if not WriteClusters(Cluster,1,FClusterBuffer^) then Exit;
           
           {Update Cluster}
           Inc(Cluster);
           
           {Read Cluster}
           if not ReadClusters(Cluster,1,FClusterBuffer^) then Exit;
           
           {Zero Cluster}
           ZeroMemory(FClusterBuffer,FClusterSize);
          end;
          
         {Set Entry}
         if not SetEntry(AParent,Entry) then Exit;
         
         Entry:=TCDFSDiskEntry(Entry.Next);
        end;
        
       {Write Cluster}
       if not WriteClusters(Cluster,1,FClusterBuffer^) then Exit;
       
       Result:=True;
      finally
       FBlockWrite:=False;
      end;
     end;
   finally
    FEntries.WriterUnlock;
   end; 
  end;
end;

{=============================================================================}

function TCDFSFileSystem.SetCatalogs:Boolean;
{Includes SetHeaders/SetExtensions}
var
 Start:LongWord;
 Cluster:LongWord;
 Initial:TCDFSDiskCatalog;
 Validation:TCDFSDiskHeader;
 Header:TCDFSDiskHeader;
 Catalog:TCDFSDiskCatalog;
 Extension:TCDFSDiskExtension;
 Descriptor:TCDFSDiskDescriptor;
begin
 {}
 Result:=False;
 
 if FDriver = nil then Exit;
 if FClusterBuffer = nil then Exit;

 {Check Loaded}
 if FCatalogsLoaded then
  begin
   if not FCatalogs.WriterLock then Exit;
   try
    if not FHeaders.WriterLock then Exit;
    try
     if not FExtensions.WriterLock then Exit;
     try
      {$IFDEF CDFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.SetCatalogs');
      {$ENDIF}
    
      FBlockWrite:=True;
      try
       {Get Boot}
       Descriptor:=FBoot;
       if Descriptor = nil then Exit;
     
       {Get Start}
       Start:=Descriptor.CatalogStart;
       if Start = 0 then Exit;
     
       {Get Cluster}
       Cluster:=Start;
     
       {Read Cluster}
       if not ReadClusters(Cluster,1,FClusterBuffer^) then Exit;
     
       {Zero Cluster}
       ZeroMemory(FClusterBuffer,FClusterSize);
     
       {Get Validation}
       Validation:=Descriptor.Validation;
       if Validation = nil then Exit;
     
       {Set Validation}
       if not SetHeader(Validation) then Exit;
     
       {Get Initial}
       Initial:=Descriptor.Initial;
       if Initial = nil then Exit;
     
       {Set Initial}
       if not SetCatalog(Initial) then Exit;
     
       {Get Headers}
       Header:=TCDFSDiskHeader(FHeaders.First);
       while Header <> nil do
        begin
         if Header <> Validation then
          begin
           {Check Cluster}
           if Header.HeaderCluster > Cluster then
            begin
             {Write Cluster}
             if not WriteClusters(Cluster,1,FClusterBuffer^) then Exit;
           
             {Update Cluster}
             Inc(Cluster);
           
             {Read Cluster}
             if not ReadClusters(Cluster,1,FClusterBuffer^) then Exit;
           
             {Zero Cluster}
             ZeroMemory(FClusterBuffer,FClusterSize);
            end;
          
           {Set Header}
           if not SetHeader(Header) then Exit;
         
           {Get Catalogs}
           Catalog:=TCDFSDiskCatalog(FCatalogs.First); 
           while Catalog <> nil do
            begin
             if (Catalog.Header = Header) and (Catalog <> Initial) then
              begin
               {Check Cluster}
               if Catalog.CatalogCluster > Cluster then
                begin
                 {Write Cluster}
                 if not WriteClusters(Cluster,1,FClusterBuffer^) then Exit;
               
                 {Update Cluster}
                 Inc(Cluster);
               
                 {Read Cluster}
                 if not ReadClusters(Cluster,1,FClusterBuffer^) then Exit;
               
                 {Zero Cluster}
                 ZeroMemory(FClusterBuffer,FClusterSize);
                end;
              
               {Set Catalog}
               if not SetCatalog(Catalog) then Exit;
             
               {Get Extensions}
               Extension:=TCDFSDiskExtension(FExtensions.First); 
               while Extension <> nil do
                begin
                 if (Extension.Catalog = Catalog) then
                  begin
                   {Check Cluster}
                   if Extension.ExtensionCluster > Cluster then
                    begin
                     {Write Cluster}
                     if not WriteClusters(Cluster,1,FClusterBuffer^) then Exit;
                    
                     {Update Cluster}
                     Inc(Cluster);
                   
                     {Read Cluster}
                     if not ReadClusters(Cluster,1,FClusterBuffer^) then Exit;
                   
                     {Zero Cluster}
                     ZeroMemory(FClusterBuffer,FClusterSize);
                    end;
                  
                   {Set Extension}
                   if not SetExtension(Extension) then Exit;
                  end;
                
                 Extension:=TCDFSDiskExtension(Extension.Next);
                end;
              end;
            
             Catalog:=TCDFSDiskCatalog(Catalog.Next);
            end;
          end;
         Header:=TCDFSDiskHeader(Header.Next); 
        end;
     
       {Write Cluster}
       if not WriteClusters(Cluster,1,FClusterBuffer^) then Exit;
     
       Result:=True;
      finally
       FBlockWrite:=False;
      end;
     finally
      FExtensions.WriterUnlock;
     end; 
    finally
     FHeaders.WriterUnlock;
    end; 
   finally
    FCatalogs.WriterUnlock;
   end; 
  end;
end;

{=============================================================================}

function TCDFSFileSystem.LoadTable(ATableNo:LongWord):Boolean;
var
 Instance:Byte;
 TableType:Byte;
 Count:LongWord;
 Table:TCDFSDiskTable;
 Descriptor:TCDFSDiskDescriptor;
begin
 {}
 Result:=False;

 if not FTables.WriterLock then Exit;
 try
  if FDriver = nil then Exit;

  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.LoadTable - TableNo = ' + IntToHex(ATableNo,8));
  {$ENDIF}
 
  {Get Descriptor}
  Instance:=(ATableNo and cdfsTableInstanceMask) shr cdfsTableInstanceShift;
  TableType:=(ATableNo and cdfsTableTypeMask) shr cdfsTableTypeShift;
  Descriptor:=GetDescriptor((ATableNo and cdfsTableDescriptorMask) shr cdfsTableDescriptorShift,Instance);
  if Descriptor = nil then Exit;
 
  {Create Table}
  Table:=TCDFSDiskTable.Create(FPathLock,FTableLocal,Descriptor);
  Table.TableNo:=ATableNo;
  Table.TableType:=TableType;
  Table.Unicode:=Descriptor.Unicode;
  
  {Get Count}
  Count:=(Descriptor.PathTableSize shr FClusterShiftCount);
  if (Count shl FClusterShiftCount) < Descriptor.PathTableSize then Inc(Count);
  Table.ClusterCount:=Count;
 
  {Get Size}
  Table.DataFree:=0;
  Table.DataSize:=(Table.ClusterCount shl FClusterShiftCount);
 
  {Check Type}
  case TableType of
   cdfsPathTableTypePrimary:begin
     {Setup Table}
     Table.StartCluster:=Descriptor.PrimaryPathTable;
     Table.Endian:=False;
     
     Descriptor.Primary:=Table;
    end;
   cdfsPathTableTypeAlternate:begin
     {Setup Table}
     Table.StartCluster:=Descriptor.AlternatePathTable;
     Table.Endian:=False;
     
     Descriptor.Alternate:=Table;
    end;
   cdfsPathTableTypePrimaryM:begin
     {Setup Table}
     Table.StartCluster:=Descriptor.PrimaryPathTableM;
     Table.Endian:=True;
     
     Descriptor.PrimaryM:=Table;
    end;
   cdfsPathTableTypeAlternateM:begin
     {Setup Table}
     Table.StartCluster:=Descriptor.AlternatePathTableM;
     Table.Endian:=True;
     
     Descriptor.AlternateM:=Table;
    end;
  end;
 
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.LoadTable - Type = ' + IntToStr(Table.TableType) + ' Start = ' + IntToStr(Table.StartCluster) + ' Count = ' + IntToStr(Table.ClusterCount));
  {$ENDIF}
  
  FTables.Add(Table);
 
  Result:=True;
 finally
  FTables.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.LoadBlock(ABlockNo:LongWord):Boolean;
{Note: LoadBlock does not populate the Free/Used data in the Bitmap, all bits begin as Free}
var
 Buffer:Pointer;
 Count:LongWord;
 Cluster:LongWord;
 Block:TCDFSDiskBlock;
begin
 {}
 Result:=False;

 if not FBlocks.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FEntriesPerBlock = 0 then Exit;
  if ABlockNo >= FTotalBlockCount then Exit;

  {Get Cluster}
  Cluster:=(ABlockNo shl FBlockShiftCount);
 
  {Get Count}
  Count:=Min((FTotalClusterCount - Cluster),FEntriesPerBlock);
 
  {Allocate Buffer}
  Buffer:=AllocMem((FClustersPerBlock shl FClusterShiftCount));
  if Buffer = nil then Exit;
 
  {Load Block}
  Block:=TCDFSDiskBlock.Create(FBlockLocal);
  Block.BlockNo:=ABlockNo;
  Block.BlockCount:=Count;
  Block.BlockBuffer:=Buffer;
  Block.BlockCluster:=Cluster;
 
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.LoadBlock - BlockNo = ' + IntToHex(Block.BlockNo,8) + ' BlockCluster = ' + IntToStr(Block.BlockCluster) + ' BlockCount = ' + IntToStr(Block.BlockCount));
  {$ENDIF}
 
  Result:=FBlocks.Add(Block);
 finally
  FBlocks.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.LoadDescriptor(ABuffer:Pointer;ACluster:LongWord):Boolean;
var
 Descriptor:TCDFSDiskDescriptor;
begin
 {}
 Result:=False;
 
 if not FDescriptors.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ABuffer = nil then Exit;

  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.LoadDescriptor - Cluster = ' + IntToStr(ACluster));
  {$ENDIF}
 
  {Check Header}
  if PCDFSVolumeDescriptorHeader(ABuffer).DescriptorVersion <> cdfsISO9660DescriptorVersion then Exit;
  if not CompareIdentifier(PCDFSVolumeDescriptorHeader(ABuffer).StandardIdentifier[0],cdfsISO9660StandardIdentifier[1],5) then Exit;
  
  {Create Descriptor}
  Descriptor:=TCDFSDiskDescriptor.Create(FDescriptorLocal);
  Descriptor.StartCluster:=ACluster;
  Descriptor.DescriptorType:=PCDFSVolumeDescriptorHeader(ABuffer).DescriptorType;
  Descriptor.DescriptorVersion:=PCDFSVolumeDescriptorHeader(ABuffer).DescriptorVersion;
  Descriptor.StandardIdentifier:=CDFSIdentifierToString(PCDFSVolumeDescriptorHeader(ABuffer).StandardIdentifier[0],5,False);
  
  {Check Type}
  case PCDFSVolumeDescriptorHeader(ABuffer).DescriptorType of
   cdfsVolumeDescriptorTypeBoot:begin
     {$IFDEF CDFS_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.LoadDescriptor - cdfsVolumeDescriptorTypeBoot');
     {$ENDIF}
     
     {Get Type}
     FBootCatalog:=True;
     
     {Read Descriptor}
     Descriptor.SystemIdentifier:=CDFSIdentifierToString(PCDFSVolumeDescriptorBoot(ABuffer).SystemIdentifier[0],32,False);
     Descriptor.BootIdentifier:=CDFSIdentifierToString(PCDFSVolumeDescriptorBoot(ABuffer).BootIdentifier[0],32,False);
     Descriptor.CatalogStart:=PELTORITOVolumeDescriptorBoot(ABuffer).CatalogStart;
     
     if FBoot = nil then FBoot:=Descriptor;
    end;
   cdfsVolumeDescriptorTypePrimary:begin
     {$IFDEF CDFS_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.LoadDescriptor - cdfsVolumeDescriptorTypePrimary');
     {$ENDIF}

     {Get Type}
     FCDFSType:=ctISO9660;
     FLongNames:=False;
     
     {Read Descriptor}
     Descriptor.SystemIdentifier:=CDFSIdentifierToString(PCDFSVolumeDescriptorPrimary(ABuffer).SystemIdentifier[0],32,False);
     Descriptor.VolumeIdentifier:=CDFSIdentifierToString(PCDFSVolumeDescriptorPrimary(ABuffer).VolumeIdentifier[0],32,False);
     Descriptor.VolumeSpaceSize:=PCDFSVolumeDescriptorPrimary(ABuffer).VolumeSpaceSize;
     Descriptor.VolumeSetSize:=PCDFSVolumeDescriptorPrimary(ABuffer).VolumeSetSize;
     Descriptor.VolumeSequenceNumber:=PCDFSVolumeDescriptorPrimary(ABuffer).VolumeSequenceNumber;
     Descriptor.LogicalBlockSize:=PCDFSVolumeDescriptorPrimary(ABuffer).LogicalBlockSize;
     Descriptor.PathTableSize:=PCDFSVolumeDescriptorPrimary(ABuffer).PathTableSize;
     Descriptor.PrimaryPathTable:=PCDFSVolumeDescriptorPrimary(ABuffer).PrimaryPathTable;
     Descriptor.AlternatePathTable:=PCDFSVolumeDescriptorPrimary(ABuffer).AlternatePathTable;
     Descriptor.PrimaryPathTableM:=LongWordBEtoN(PCDFSVolumeDescriptorPrimary(ABuffer).PrimaryPathTableM);
     Descriptor.AlternatePathTableM:=LongWordBEtoN(PCDFSVolumeDescriptorPrimary(ABuffer).AlternatePathTableM);
     Descriptor.VolumeSetIdentifier:=CDFSIdentifierToString(PCDFSVolumeDescriptorPrimary(ABuffer).VolumeSetIdentifier[0],128,False);
     Descriptor.PublisherIdentifier:=CDFSIdentifierToString(PCDFSVolumeDescriptorPrimary(ABuffer).PublisherIdentifier[0],128,False);
     Descriptor.PreparerIdentifier:=CDFSIdentifierToString(PCDFSVolumeDescriptorPrimary(ABuffer).PreparerIdentifier[0],128,False);
     Descriptor.ApplicationIdentifier:=CDFSIdentifierToString(PCDFSVolumeDescriptorPrimary(ABuffer).ApplicationIdentifier[0],128,False);
     Descriptor.CopyrightIdentifier:=CDFSIdentifierToString(PCDFSVolumeDescriptorPrimary(ABuffer).CopyrightIdentifier[0],37,False);
     Descriptor.AbstractIdentifier:=CDFSIdentifierToString(PCDFSVolumeDescriptorPrimary(ABuffer).AbstractIdentifier[0],37,False);
     Descriptor.BibliographicIdentifier:=CDFSIdentifierToString(PCDFSVolumeDescriptorPrimary(ABuffer).BibliographicIdentifier[0],37,False);
     Descriptor.CreateTime:=CDFSDateTimeToFileTime(PCDFSVolumeDescriptorPrimary(ABuffer).CreateTime);
     Descriptor.ModifyTime:=CDFSDateTimeToFileTime(PCDFSVolumeDescriptorPrimary(ABuffer).ModifyTime);
     Descriptor.ExpireTime:=CDFSDateTimeToFileTime(PCDFSVolumeDescriptorPrimary(ABuffer).ExpireTime);
     Descriptor.EffectiveTime:=CDFSDateTimeToFileTime(PCDFSVolumeDescriptorPrimary(ABuffer).EffectiveTime);
     Descriptor.FileStructureVersion:=PCDFSVolumeDescriptorPrimary(ABuffer).FileStructureVersion;
     Descriptor.ApplicationData:=CDFSDataToPointer(PCDFSVolumeDescriptorPrimary(ABuffer).ApplicationData[0],512,False);
     
     {Get Primary}
     if FPrimary = nil then FPrimary:=Descriptor;
    end;
   cdfsVolumeDescriptorTypeSupplementary:begin
     {$IFDEF CDFS_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.LoadDescriptor - cdfsVolumeDescriptorTypeSupplementary');
     {$ENDIF}

     {Get Type}
     if CompareSequence(PCDFSVolumeDescriptorSupplementary(ABuffer).EscapeSequences[0],cdfsJolietUCS2Sequence1[1],3) then Descriptor.Unicode:=True;
     if CompareSequence(PCDFSVolumeDescriptorSupplementary(ABuffer).EscapeSequences[0],cdfsJolietUCS2Sequence2[1],3) then Descriptor.Unicode:=True;
     if CompareSequence(PCDFSVolumeDescriptorSupplementary(ABuffer).EscapeSequences[0],cdfsJolietUCS2Sequence3[1],3) then Descriptor.Unicode:=True;
     if Descriptor.Unicode then FCDFSType:=ctJOLIET;
     if Descriptor.Unicode then FLongNames:=True;
     
     {Read Descriptor}
     Descriptor.SystemIdentifier:=CDFSIdentifierToString(PCDFSVolumeDescriptorSupplementary(ABuffer).SystemIdentifier[0],32,Descriptor.Unicode);
     Descriptor.VolumeIdentifier:=CDFSIdentifierToString(PCDFSVolumeDescriptorSupplementary(ABuffer).VolumeIdentifier[0],32,Descriptor.Unicode);
     Descriptor.VolumeSpaceSize:=PCDFSVolumeDescriptorSupplementary(ABuffer).VolumeSpaceSize;
     Descriptor.VolumeSetSize:=PCDFSVolumeDescriptorSupplementary(ABuffer).VolumeSetSize;
     Descriptor.VolumeSequenceNumber:=PCDFSVolumeDescriptorSupplementary(ABuffer).VolumeSequenceNumber;
     Descriptor.LogicalBlockSize:=PCDFSVolumeDescriptorSupplementary(ABuffer).LogicalBlockSize;
     Descriptor.PathTableSize:=PCDFSVolumeDescriptorSupplementary(ABuffer).PathTableSize;
     Descriptor.PrimaryPathTable:=PCDFSVolumeDescriptorSupplementary(ABuffer).PrimaryPathTable;
     Descriptor.AlternatePathTable:=PCDFSVolumeDescriptorSupplementary(ABuffer).AlternatePathTable;
     Descriptor.PrimaryPathTableM:=LongWordBEtoN(PCDFSVolumeDescriptorSupplementary(ABuffer).PrimaryPathTableM);
     Descriptor.AlternatePathTableM:=LongWordBEtoN(PCDFSVolumeDescriptorSupplementary(ABuffer).AlternatePathTableM);
     Descriptor.VolumeSetIdentifier:=CDFSIdentifierToString(PCDFSVolumeDescriptorSupplementary(ABuffer).VolumeSetIdentifier[0],128,Descriptor.Unicode);
     Descriptor.PublisherIdentifier:=CDFSIdentifierToString(PCDFSVolumeDescriptorSupplementary(ABuffer).PublisherIdentifier[0],128,Descriptor.Unicode);
     Descriptor.PreparerIdentifier:=CDFSIdentifierToString(PCDFSVolumeDescriptorSupplementary(ABuffer).PreparerIdentifier[0],128,Descriptor.Unicode);
     Descriptor.ApplicationIdentifier:=CDFSIdentifierToString(PCDFSVolumeDescriptorSupplementary(ABuffer).ApplicationIdentifier[0],128,Descriptor.Unicode);
     Descriptor.CopyrightIdentifier:=CDFSIdentifierToString(PCDFSVolumeDescriptorSupplementary(ABuffer).CopyrightIdentifier[0],37,Descriptor.Unicode);
     Descriptor.AbstractIdentifier:=CDFSIdentifierToString(PCDFSVolumeDescriptorSupplementary(ABuffer).AbstractIdentifier[0],37,Descriptor.Unicode);
     Descriptor.BibliographicIdentifier:=CDFSIdentifierToString(PCDFSVolumeDescriptorSupplementary(ABuffer).BibliographicIdentifier[0],37,Descriptor.Unicode);
     Descriptor.CreateTime:=CDFSDateTimeToFileTime(PCDFSVolumeDescriptorSupplementary(ABuffer).CreateTime);
     Descriptor.ModifyTime:=CDFSDateTimeToFileTime(PCDFSVolumeDescriptorSupplementary(ABuffer).ModifyTime);
     Descriptor.ExpireTime:=CDFSDateTimeToFileTime(PCDFSVolumeDescriptorSupplementary(ABuffer).ExpireTime);
     Descriptor.EffectiveTime:=CDFSDateTimeToFileTime(PCDFSVolumeDescriptorSupplementary(ABuffer).EffectiveTime);
     Descriptor.FileStructureVersion:=PCDFSVolumeDescriptorSupplementary(ABuffer).FileStructureVersion;
     Descriptor.ApplicationData:=CDFSDataToPointer(PCDFSVolumeDescriptorSupplementary(ABuffer).ApplicationData[0],512,False);
     Descriptor.VolumeFlags:=PCDFSVolumeDescriptorSupplementary(ABuffer).VolumeFlags;
     Descriptor.EscapeSequences:=CDFSDataToPointer(PCDFSVolumeDescriptorSupplementary(ABuffer).EscapeSequences[0],32,False);
     
     {Get Supplementary}
     if (FSupplementary = nil) and (Descriptor.Unicode) then FSupplementary:=Descriptor;
    end;
   cdfsVolumeDescriptorTypePartition:begin
     {$IFDEF CDFS_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.LoadDescriptor - cdfsVolumeDescriptorTypePartition');
     {$ENDIF}

     {Read Descriptor}
     Descriptor.SystemIdentifier:=CDFSIdentifierToString(PCDFSVolumeDescriptorPartition(ABuffer).SystemIdentifier[0],32,False);
     Descriptor.PartitionIdentifier:=CDFSIdentifierToString(PCDFSVolumeDescriptorPartition(ABuffer).PartitionIdentifier[0],32,False);
     Descriptor.PartitionStart:=PCDFSVolumeDescriptorPartition(ABuffer).PartitionStart;
     Descriptor.PartitionSize:=PCDFSVolumeDescriptorPartition(ABuffer).PartitionSize;
     Descriptor.SystemData:=CDFSDataToPointer(PCDFSVolumeDescriptorPartition(ABuffer).SystemData[0],1960,False);
    end;
   cdfsVolumeDescriptorTypeTerminator:begin
     {Nothing}
    end;
  end;
  
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.LoadDescriptor - Type = ' + IntToStr(Descriptor.DescriptorType));
  {$ENDIF}
  
  FDescriptors.Add(Descriptor);
  
  Result:=True;
 finally
  FDescriptors.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.LoadRoot(ADescriptor:TCDFSDiskDescriptor):Boolean;
{Note: Caller must hold the descriptors lock}
var
 Entry:TCDFSDiskEntry;
begin
 {}
 Result:=False;
 
 if not FEntries.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ADescriptor = nil then Exit;
  if FClusterBuffer = nil then Exit;

  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.LoadRoot - Descriptor Type = ' + IntToStr(ADescriptor.DescriptorType));
  {$ENDIF}
 
  {Load Paths}
  if not FTables.ReaderLock then Exit;
  try
   if ADescriptor.Primary <> nil then if not LoadPaths(ADescriptor.Primary) then Exit;
   if ADescriptor.PrimaryM <> nil then if not LoadPaths(ADescriptor.PrimaryM) then Exit;
   if ADescriptor.Alternate <> nil then if not LoadPaths(ADescriptor.Alternate) then Exit;
   if ADescriptor.AlternateM <> nil then if not LoadPaths(ADescriptor.AlternateM) then Exit;
  finally
   FTables.ReaderUnlock;
  end;       
 
  {Read Cluster}
  if not ReadClusters(ADescriptor.StartCluster,1,FClusterBuffer^) then Exit;
 
  {Create Entry}
  Entry:=TCDFSDiskEntry.Create(FEntryLocal);
  Entry.Unicode:=ADescriptor.Unicode;
  Entry.EntryOffset:=cdfsRootRecordOffset;
  Entry.EntryCluster:=ADescriptor.StartCluster;
 
  {Load Entry}
  if RecordToEntry(@PCDFSVolumeDescriptorPrimary(FClusterBuffer).RootDirectory,Entry,Entry.Unicode) then
   begin
    {Setup Defaults}
    Entry.Name:=FRootName;
    Entry.AltName:=FRootPath;
    ADescriptor.Root:=Entry;
   
    {$IFDEF CDFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.LoadRoot - Name = ' + Entry.Name + ' Start = ' + IntToStr(Entry.StartCluster) + ' Count = ' + IntToStr(Entry.ClusterCount)); 
    {$ENDIF}
   
    {Get Paths}
    if not FTables.ReaderLock then Exit;
    try
     if ADescriptor.Primary <> nil then Entry.Path:=GetPath(ADescriptor.Primary,cdfsRootPathNumber,cdfsDotName);
     if ADescriptor.PrimaryM <> nil then Entry.PathM:=GetPath(ADescriptor.PrimaryM,cdfsRootPathNumber,cdfsDotName);
     if ADescriptor.Alternate <> nil then Entry.AltPath:=GetPath(ADescriptor.Alternate,cdfsRootPathNumber,cdfsDotName);
     if ADescriptor.AlternateM <> nil then Entry.AltPathM:=GetPath(ADescriptor.AlternateM,cdfsRootPathNumber,cdfsDotName);
   
     {$IFDEF CDFS_DEBUG}
     if FILESYS_LOG_ENABLED then if Entry.Path <> nil then FileSysLogDebug('TCDFSFileSystem.LoadRoot - Path = ' + Entry.Path.Name + ' Number = ' + IntToStr(Entry.Path.PathNumber));
     {$ENDIF}
    finally
     FTables.ReaderUnlock;
    end;       
   
    {Add Entry}
    FEntries.Add(Entry,nil);
   
    FRoot:=Entry;
    FRoot.AddReference; {Prevent Deletion}
   
    Result:=True;
   end
  else
   begin
    Entry.Free;
   end;
 finally
  FEntries.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.LoadPath(ATable:TCDFSDiskTable;ABuffer:Pointer;AOffset:LongWord;ANumber:Word):Boolean;
{Note: Caller must hold the tables lock}
var
 Path:TCDFSDiskPath;
begin
 {}
 Result:=False;

 if FDriver = nil then Exit;
 if ATable = nil then Exit;
 if ABuffer = nil then Exit;

 if not ATable.Paths.WriterLock then Exit;
 try
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.LoadPath - Offset = ' + IntToStr(AOffset));
  {$ENDIF}
 
  {Create Path}
  Path:=TCDFSDiskPath.Create(ATable.PathLocal);
  Path.PathNumber:=ANumber;
  Path.PathOffset:=AOffset;
 
  {Load Path}
  if RecordToPath(Pointer(PtrUInt(ABuffer) + AOffset),Path,ATable.Unicode,ATable.Endian) then
   begin
    {$IFDEF CDFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.LoadPath - Name = ' + Path.Name + ' Number = ' + IntToStr(Path.PathNumber) + ' Parent = ' + IntToStr(Path.ParentNumber));
    {$ENDIF}
   
    {Get Parent}
    Path.Parent:=LocatePath(ATable,Path.ParentNumber);
   
    {$IFDEF CDFS_DEBUG}
    if FILESYS_LOG_ENABLED then if Path.Parent <> nil then FileSysLogDebug('TCDFSFileSystem.LoadPath - Parent = ' + Path.Parent.Name + ' Number = ' + IntToStr(Path.Parent.PathNumber));
    {$ENDIF}
   
    {Add Path}
    ATable.Paths.Add(Path);
   
    Result:=True;
   end
  else
   begin
    Path.Free;
   end;
 finally
  ATable.Paths.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.LoadEntry(AParent:TCDFSDiskEntry;ABuffer:Pointer;AOffset,ACluster:LongWord):Boolean;
var
 Entry:TCDFSDiskEntry;
 Descriptor:TCDFSDiskDescriptor;
begin
 {}
 Result:=False;
 
 if not FEntries.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if AParent = nil then Exit;
  if ABuffer = nil then Exit;

  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.LoadEntry - Cluster = ' + IntToStr(ACluster) + ' Offset = ' + IntToStr(AOffset));
  {$ENDIF}
  
  {Create Entry}
  Entry:=TCDFSDiskEntry.Create(FEntryLocal);
  Entry.Unicode:=AParent.Unicode;
  Entry.EntryOffset:=AOffset;
  Entry.EntryCluster:=ACluster;
  
  {Load Entry}
  if RecordToEntry(Pointer(PtrUInt(ABuffer) + AOffset),Entry,Entry.Unicode) then
   begin
    {$IFDEF CDFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.LoadEntry - Name = ' + Entry.Name + ' Start = ' + IntToStr(Entry.StartCluster) + ' Count = ' + IntToStr(Entry.ClusterCount));
    {$ENDIF}
    
    {Check Folder}
    if (Entry.Attributes and faDirectory) = faDirectory then
     begin
      //To Do //Lock ? Descriptors ? (Reader / Writer ?) only called by LoadEntries but who can call LoadEntries (Prefer Reader, could be Writer)
      
      {Get Descriptor}
      Descriptor:=FPrimary;
      if Entry.Unicode then Descriptor:=FSupplementary;
      
      {Get Paths}
      if Descriptor <> nil then
       begin
        if not FTables.ReaderLock then Exit;
        try
         if Descriptor.Primary <> nil then Entry.Path:=GetPath(Descriptor.Primary,AParent.PathNumber,Entry.Name);
         if Descriptor.PrimaryM <> nil then Entry.PathM:=GetPath(Descriptor.PrimaryM,AParent.PathNumber,Entry.Name);
         if Descriptor.Alternate <> nil then Entry.AltPath:=GetPath(Descriptor.Alternate,AParent.PathNumber,Entry.Name);
         if Descriptor.AlternateM <> nil then Entry.AltPathM:=GetPath(Descriptor.AlternateM,AParent.PathNumber,Entry.Name);
        
         {$IFDEF CDFS_DEBUG}
         if FILESYS_LOG_ENABLED then if Entry.Path <> nil then FileSysLogDebug('TCDFSFileSystem.LoadEntry - Path = ' + Entry.Path.Name + ' Number = ' + IntToStr(Entry.Path.PathNumber));
         {$ENDIF}
        finally
         FTables.ReaderUnlock;
        end;       
       end;
     end;
    
    {Add Entry}
    FEntries.Add(Entry,AParent);
    
    Result:=True;
   end
  else
   begin
    Entry.Free;
   end;
 finally
  FEntries.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.LoadHeader(ABuffer:Pointer;AOffset,ACluster:LongWord;AHeaderNo:LongWord):Boolean;
{Note: Should only be called by LoadCatalogs}
var
 Header:TCDFSDiskHeader;
 Validation:TCDFSDiskHeader;
 SectionHeader:PELTORITOSectionHeader;
 ValidationRecord:PELTORITOValidationRecord;
begin
 {}
 Result:=False;
 
 if not FHeaders.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ABuffer = nil then Exit;

  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.LoadHeader - Cluster = ' + IntToStr(ACluster) + ' Offset = ' + IntToStr(AOffset));
  {$ENDIF}
  
  {Check Header}
  SectionHeader:=PELTORITOSectionHeader(PtrUInt(ABuffer) + AOffset);
  if (SectionHeader.HeaderIndicator = cdfsElToritoHeaderIndicator) or (SectionHeader.HeaderIndicator = cdfsElToritoHeaderTerminator) then
   begin
    {Create Header}
    Header:=TCDFSDiskHeader.Create(FHeaderLocal);
    Header.HeaderNo:=AHeaderNo;
    Header.HeaderOffset:=AOffset;
    Header.HeaderCluster:=ACluster; 

    Header.Validation:=False;
    Header.HeaderIndicator:=SectionHeader.HeaderIndicator;
    Header.PlatformId:=SectionHeader.PlatformId;
    Header.SectionCount:=SectionHeader.SectionCount;
    Header.SectionId:=CDFSIdentifierToString(SectionHeader.SectionId[0],28,False);
    
    {Add Header}
    FHeaders.Add(Header);
    
    Result:=True;
   end
  else
   begin
    {Check Validation}
    ValidationRecord:=PELTORITOValidationRecord(PtrUInt(ABuffer) + AOffset);
    if ValidationRecord.HeaderId = cdfsElToritoHeaderId then
     begin
      {Create Validation}
      Validation:=TCDFSDiskHeader.Create(FHeaderLocal);
      Validation.HeaderNo:=AHeaderNo;
      Validation.HeaderOffset:=AOffset;
      Validation.HeaderCluster:=ACluster;  

      Validation.Validation:=True;
      Validation.HeaderId:=ValidationRecord.HeaderId;
      Validation.PlatformId:=ValidationRecord.PlatformId;
      Validation.VendorId:=CDFSIdentifierToString(ValidationRecord.VendorId[0],24,False);
      Validation.Checksum:=ValidationRecord.Checksum;
      Validation.Signature:=ValidationRecord.Signature;
      
      {Check Signature}
      if Validation.Signature = cdfsElToritoSignature then
       begin
        {Add Validation}
        FHeaders.Add(Validation);
        
        Result:=True;
       end
      else
       begin
        Validation.Free;
       end;
     end;
   end;
 finally
  FHeaders.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.LoadCatalog(ABuffer:Pointer;AOffset,ACluster:LongWord;ACatalogNo:LongWord;AInitial:Boolean):Boolean;
{Note: Should only be called by LoadCatalogs}
var
 Initial:TCDFSDiskCatalog;
 Catalog:TCDFSDiskCatalog;
 DefaultRecord:PELTORITODefaultRecord;
 SectionRecord:PELTORITOSectionRecord;
begin
 {}
 Result:=False;
 
 if not FCatalogs.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ABuffer = nil then Exit;

  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.LoadCatalog - Cluster = ' + IntToStr(ACluster) + ' Offset = ' + IntToStr(AOffset));
  {$ENDIF}
  if AInitial then
   begin
    {Check Initial}
    DefaultRecord:=PELTORITODefaultRecord(PtrUInt(ABuffer) + AOffset);
    if (DefaultRecord.BootIndicator = cdfsElToritoBootIndicator) or (DefaultRecord.BootIndicator = cdfsElToritoNoBootIndicator) then
     begin
      {Create Initial}
      Initial:=TCDFSDiskCatalog.Create(FCatalogLocal);
      Initial.CatalogNo:=ACatalogNo;
      Initial.CatalogOffset:=AOffset;
      Initial.CatalogCluster:=ACluster;

      Initial.Initial:=True;
      Initial.BootIndicator:=DefaultRecord.BootIndicator;
      Initial.BootMedia:=DefaultRecord.BootMedia;
      Initial.LoadSegment:=DefaultRecord.LoadSegment;
      Initial.SystemType:=DefaultRecord.SystemType;
      Initial.LoadCount:=DefaultRecord.LoadCount;

      Initial.StartCluster:=DefaultRecord.LoadRBA;
      Initial.ClusterCount:=0; {Will be set to correct value if Entry is Located}

      if LoadTrees then Initial.Entry:=LocateEntry(TCDFSDiskEntry(FRoot),Initial.StartCluster); //To Do //Lock
      if Initial.Entry <> nil then Initial.Path:=GetEntryPath(Initial.Entry,False) else Initial.Path:=FRoot.Name;
      
      {Add Initial}
      FCatalogs.Add(Initial);
      
      Result:=True;
     end;
   end
  else
   begin
    {Check Catalog}
    SectionRecord:=PELTORITOSectionRecord(PtrUInt(ABuffer) + AOffset);
    if (SectionRecord.BootIndicator = cdfsElToritoBootIndicator) or (SectionRecord.BootIndicator = cdfsElToritoNoBootIndicator) then
     begin
      {Create Catalog}
      Catalog:=TCDFSDiskCatalog.Create(FCatalogLocal);
      Catalog.CatalogNo:=ACatalogNo;
      Catalog.CatalogOffset:=AOffset;
      Catalog.CatalogCluster:=ACluster;

      Catalog.Initial:=False;
      Catalog.BootIndicator:=SectionRecord.BootIndicator;
      Catalog.BootMedia:=SectionRecord.BootMedia;
      Catalog.LoadSegment:=SectionRecord.LoadSegment;
      Catalog.SystemType:=SectionRecord.SystemType;
      Catalog.LoadCount:=SectionRecord.LoadCount;
      Catalog.SelectionType:=SectionRecord.SelectionType;
      Catalog.SelectionData:=CDFSDataToPointer(SectionRecord.SelectionData[0],19,False);

      Catalog.StartCluster:=SectionRecord.LoadRBA; 
      Catalog.ClusterCount:=0; {Will be set to correct value if Entry is Located}

      if LoadTrees then Catalog.Entry:=LocateEntry(TCDFSDiskEntry(FRoot),Catalog.StartCluster); //To Do //Lock
      if Catalog.Entry <> nil then Catalog.Path:=GetEntryPath(Catalog.Entry,False) else Catalog.Path:=FRoot.Name;
      
      {Add Catalog}
      FCatalogs.Add(Catalog);
      
      Result:=True;
     end;
   end;
 finally
  FCatalogs.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.LoadExtension(ABuffer:Pointer;AOffset,ACluster:LongWord;AExtensionNo:LongWord):Boolean;
{Note: Should only be called by LoadCatalogs}
var
 Extension:TCDFSDiskExtension;
 SectionExtension:PELTORITOSectionExtension;
begin
 {}
 Result:=False;
 
 if not FExtensions.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ABuffer = nil then Exit;

  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.LoadExtension - Cluster = ' + IntToStr(ACluster) + ' Offset = ' + IntToStr(AOffset));
  {$ENDIF}
  
  {Check Extension}
  SectionExtension:=PELTORITOSectionExtension(PtrUInt(ABuffer) + AOffset);
  if SectionExtension.ExtensionIndicator = cdfsElToritoExtensionIndicator then
   begin
    {Create Extension}
    Extension:=TCDFSDiskExtension.Create(FExtensionLocal);
    Extension.ExtensionNo:=AExtensionNo;
    Extension.ExtensionOffset:=AOffset;
    Extension.ExtensionCluster:=ACluster; 

    Extension.ExtensionIndicator:=SectionExtension.ExtensionIndicator;
    Extension.ExtensionFlag:=SectionExtension.ExtensionFlag;
    Extension.SelectionData:=CDFSDataToPointer(SectionExtension.SelectionData[0],30,False);
    
    {Add Extension}
    FExtensions.Add(Extension);
    
    Result:=True;
   end;
 finally
  FExtensions.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.LoadExtended(AEntry:TDiskEntry):Boolean;
begin
 {}
 Result:=False;
 
 if not FEntries.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if AEntry = nil then Exit;

  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.LoadExtended - Entry = ' + AEntry.Name);
  {$ENDIF}
 
  //To Do
 finally
  FEntries.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.AddDescriptor(AType:Byte):TCDFSDiskDescriptor;
var
 Cluster:LongWord;
 Current:TCDFSDiskDescriptor;
 Descriptor:TCDFSDiskDescriptor;
 Terminator:TCDFSDiskDescriptor;
begin
 {}
 Result:=nil;
 
 if not FDescriptors.WriterLock then Exit;
 try
  if FDriver = nil then Exit;

  {Check ReadOnly}
  if FReadOnly then Exit;
  
  {Check Descriptor}
  if GetDescriptorEx(AType,cdfsInstanceFirst,True) <> nil then Exit; 
  
  {Get Terminator}
  Terminator:=GetDescriptorEx(cdfsVolumeDescriptorTypeTerminator,cdfsInstanceLast,True);
  if Terminator = nil then Exit;
  
  {Check Cluster}
  Cluster:=Terminator.StartCluster + 1;
  if not AllocClusters(Cluster,1) then Exit;
  
  {Create Descriptor}
  Descriptor:=TCDFSDiskDescriptor.Create(FDescriptorLocal);
  Descriptor.DescriptorType:=AType;
  Descriptor.DescriptorVersion:=cdfsISO9660DescriptorVersion; 
  Descriptor.StandardIdentifier:=cdfsISO9660StandardIdentifier;
  
  {Check Type}
  case AType of
   cdfsVolumeDescriptorTypeBoot:begin
     Descriptor.Unicode:=False;
     Descriptor.StartCluster:=cdfsELTORITOBootSector;
     Descriptor.SystemIdentifier:=cdfsELTORITOSystemIdentifier;
     
     {Allocate Cluster}
     Cluster:=cdfsUnknownCluster;
     if not AllocClusters(Cluster,1) then Exit;
     Descriptor.CatalogStart:=Cluster;
     Descriptor.CatalogCount:=1;
     
     {Move Descriptors}
     Current:=TCDFSDiskDescriptor(FDescriptors.First);
     while Current <> nil do
      begin
       if Current.StartCluster >= Descriptor.StartCluster then
        begin
         {Update Descriptor}
         Current.StartCluster:=Current.StartCluster + 1;
         
         {Set Descriptor}
         if not SetDescriptor(Current) then Exit;
        end;
        
       Current:=TCDFSDiskDescriptor(Current.Next);
      end;
      
     {Add Descriptor}
     if not FDescriptors.Add(Descriptor) then Exit;
     
     {Set Descriptor}
     if not SetDescriptor(Descriptor) then Exit;
     
     FBootCatalog:=True;
     FBoot:=Descriptor;
     
     Result:=Descriptor;
    end;
   cdfsVolumeDescriptorTypePrimary:begin
     {Not Allowed}
     Descriptor.Free;
    end;
   cdfsVolumeDescriptorTypeSupplementary:begin
     {Not Allowed}
     Descriptor.Free;
    end;
   cdfsVolumeDescriptorTypePartition:begin
     {Not Allowed}
     Descriptor.Free;
    end;
   cdfsVolumeDescriptorTypeTerminator:begin
     {Not Allowed}
     Descriptor.Free;
    end;
  end;
 finally
  FDescriptors.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.RemoveDescriptor(ADescriptor:TCDFSDiskDescriptor):Boolean;
var
 Cluster:LongWord;
 Current:TCDFSDiskDescriptor;
 Terminator:TCDFSDiskDescriptor;
begin
 {}
 Result:=False;
 
 if not FDescriptors.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ADescriptor = nil then Exit;

  {Check ReadOnly}
  if FReadOnly then Exit;
  
  {Get Terminator}
  Terminator:=GetDescriptorEx(cdfsVolumeDescriptorTypeTerminator,cdfsInstanceLast,True);
  if Terminator = nil then Exit;
  
  {Get Cluster} {For ReleaseClusters}
  Cluster:=Terminator.StartCluster;
  
  {Check Type}
  case ADescriptor.DescriptorType of
   cdfsVolumeDescriptorTypeBoot:begin
     {Release Clusters}
     if not ReleaseClusters(ADescriptor.CatalogStart,ADescriptor.CatalogCount) then Exit;
     
     {Move Descriptors}
     Current:=TCDFSDiskDescriptor(FDescriptors.Last);
     while Current <> nil do
      begin
       if Current.StartCluster > ADescriptor.StartCluster then
        begin
         {Update Descriptor}
         Current.StartCluster:=Current.StartCluster - 1;
         
         {Set Descriptor}
         if not SetDescriptor(Current) then Exit;
        end;
        
       Current:=TCDFSDiskDescriptor(Current.Prev);
      end;
      
     {Delete Descriptor}
     if not FDescriptors.Remove(ADescriptor) then Exit;
     
     {Release Clusters}
     if not ReleaseClusters(Cluster,1) then Exit;
     
     ADescriptor.Free;
     
     FBootCatalog:=True; {Remains True as Volume is Writeable}
     FBoot:=nil;
     
     Result:=True;
    end;
   cdfsVolumeDescriptorTypePrimary:begin
     {Not Allowed}
    end;
   cdfsVolumeDescriptorTypeSupplementary:begin
     {Not Allowed}
    end;
   cdfsVolumeDescriptorTypePartition:begin
     {Not Allowed}
    end;
   cdfsVolumeDescriptorTypeTerminator:begin
     {Not Allowed}
    end;
  end;
 finally
  FDescriptors.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.AddPath(ATable:TDiskTable;AParentNumber:Word;const AName:String;AStartCluster:LongWord):TCDFSDiskPath;
{Note: Caller must hold the tables lock}
var
 Offset:LongWord;
 Cluster:LongWord;
 Path:TCDFSDiskPath;
 Parent:TCDFSDiskPath;
 Current:TCDFSDiskPath;
 Previous:TCDFSDiskPath;
begin
 {}
 Result:=nil;
 
 if FDriver = nil then Exit;
 if ATable = nil then Exit;
 if TCDFSDiskTable(ATable).Descriptor = nil then Exit;

 if not TCDFSDiskTable(ATable).Paths.WriterLock then Exit;
 try
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.AddPath TableNo = ' + IntToHex(ATable.TableNo,8) + ' Name = ' + AName);
  {$ENDIF}
  
  {Check ReadOnly}
  if FReadOnly then Exit;
  
  {Prepare Trees}
  if not PrepareTrees then Exit;
  
  {Get Parent}
  Parent:=LocatePath(TCDFSDiskTable(ATable),AParentNumber);
  if Parent = nil then Exit;
  
  {Check Relative} {Dot and DotDot not allowed}
  if Length(AName) = 1 then if AName = cdfsDotName then Exit;
  if Length(AName) = 2 then if AName = cdfsDotDotName then Exit;
  
  {Create Path}
  Path:=TCDFSDiskPath.Create(TCDFSDiskTable(ATable).PathLocal);
  Path.Name:=AName;
  Path.PathNumber:=0;
  Path.StartCluster:=AStartCluster;
  Path.ExtendedSize:=0;
  Path.ParentNumber:=AParentNumber;
  Path.Parent:=Parent;

  {Check Free}
  if GetTableDataFree(ATable) < Path.PathRecordSize(TCDFSDiskTable(ATable).Unicode) then
   begin
    {$IFDEF CDFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.AddPath DataSize = ' + IntToStr(TCDFSDiskTable(ATable).DataSize) + ' DataFree = ' + IntToStr(TCDFSDiskTable(ATable).DataFree) + ' PathRecordSize = ' + IntToStr(Path.PathRecordSize(TCDFSDiskTable(ATable).Unicode)));
    {$ENDIF}
    
    {Allocate Cluster (Next Contiguous)}
    Cluster:=TCDFSDiskTable(ATable).StartCluster + TCDFSDiskTable(ATable).ClusterCount;
    if AllocClusters(Cluster,1) then
     begin
      {Update Table}
      //ClusterCount
      //DataSize
      //DataFree
      //To Do
      
      {Set Table}
      //To Do
     end
    else
     begin
      {Allocate Clusters (Contiguous Block)}
      Cluster:=cdfsUnknownCluster;
      if not AllocClusters(Cluster,TCDFSDiskTable(ATable).ClusterCount + 1) then Exit;
      
      {Update Table}
      //ClusterCount
      //DataSize
      //DataFree
      //To Do
      
      {Set Table}
      //To Do
      
      {Update Descriptor}
      //To Do
      
      {Set Descriptor}
      //To Do
      
      {Update Paths}
      //To Do
      
      {Set Paths}
      //To Do
     end;
   end;
   
  {Update Free}
  TCDFSDiskTable(ATable).DataFree:=TCDFSDiskTable(ATable).DataFree - Path.PathRecordSize(TCDFSDiskTable(ATable).Unicode);
  
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.AddPath DataSize = ' + IntToStr(TCDFSDiskTable(ATable).DataSize) + ' DataFree = ' + IntToStr(TCDFSDiskTable(ATable).DataFree));
  {$ENDIF}

  {Get Previous}
  Previous:=GetPreviousPath(TCDFSDiskTable(ATable),Path);
  if Previous <> nil then
   begin
    {$IFDEF CDFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.AddPath Previous = ' + Previous.Name);
    {$ENDIF}
    
    {Get Start}
    Offset:=Previous.PathOffset + Previous.PathRecordSize(TCDFSDiskTable(ATable).Unicode);
    
    {Update Path}
    Path.PathOffset:=Offset;
    
    {Update Offset}
    Inc(Offset,Path.PathRecordSize(TCDFSDiskTable(ATable).Unicode));
    
    {Move Paths}
    Current:=TCDFSDiskPath(TCDFSDiskPath(Previous).Next);
    while Current <> nil do
     begin
      {Update Path}
      Current.PathOffset:=Offset;
      
      {Update Offset}
      Inc(Offset,Current.PathRecordSize(TCDFSDiskTable(ATable).Unicode));
      
      Current:=TCDFSDiskPath(Current.Next);
     end;
   end
  else
   begin
    {$IFDEF CDFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.AddPath First Path');
    {$ENDIF}
    
    Path.PathOffset:=0; {This should never occur}
   end;

  {Insert Path}
  if not TCDFSDiskTable(ATable).Paths.Insert(Previous,Path) then Exit;
  
  {Renumber Paths}
  if not RenumberPaths(ATable) then Exit;
  
  {Set Paths}
  if not SetPaths(TCDFSDiskTable(ATable)) then Exit;
  
  {Update Descriptor} {Only on Primary Table}
  if ATable = TCDFSDiskTable(ATable).Descriptor.Primary then
   begin
    TCDFSDiskTable(ATable).Descriptor.PathTableSize:=TCDFSDiskTable(ATable).Descriptor.PathTableSize + Path.PathRecordSize(TCDFSDiskTable(ATable).Unicode);
    
    {Set Descriptor}
    if not SetDescriptor(TCDFSDiskTable(ATable).Descriptor) then Exit;
   end;
  
  Result:=Path;
 finally
  TCDFSDiskTable(ATable).Paths.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.RemovePath(ATable:TDiskTable;APath:TCDFSDiskPath):Boolean;
{Note: Caller must hold the tables lock}
var
 Offset:LongWord;
 Current:TCDFSDiskPath;
begin
 {}
 Result:=False;
 
 if FDriver = nil then Exit;
 if ATable = nil then Exit;
 if APath = nil then Exit;
 if TCDFSDiskTable(ATable).Descriptor = nil then Exit;

 if not TCDFSDiskTable(ATable).Paths.WriterLock then Exit;
 try
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.RemovePath Path = ' + APath.Name);
  {$ENDIF}
  
  {Check ReadOnly}
  if FReadOnly then Exit;
  
  {Prepare Trees}
  if not PrepareTrees then Exit;
  
  {Check Relative} {Dot not allowed}
  if Length(APath.Name) = 1 then if APath.Name = cdfsDotName then Exit;

  {Update Free}
  GetTableDataFree(ATable);
  TCDFSDiskTable(ATable).DataFree:=TCDFSDiskTable(ATable).DataFree + APath.PathRecordSize(TCDFSDiskTable(ATable).Unicode);
  
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.RemovePath DataSize = ' + IntToStr(TCDFSDiskTable(ATable).DataSize) + ' DataFree = ' + IntToStr(TCDFSDiskTable(ATable).DataFree));
  {$ENDIF} 

  {Move Paths}
  Current:=TCDFSDiskPath(TCDFSDiskPath(APath).Next);
  if Current <> nil then
   begin
    {Get Start}
    Offset:=TCDFSDiskPath(APath).PathOffset;
    while Current <> nil do
     begin
      {Update Path}
      Current.PathOffset:=Offset;
      
      {Update Offset}
      Inc(Offset,Current.PathRecordSize(TCDFSDiskTable(ATable).Unicode));
      
      Current:=TCDFSDiskPath(Current.Next);
     end;
   end;

  {Remove Path}
  if not TCDFSDiskTable(ATable).Paths.Remove(APath) then Exit;
  
  {Renumber Paths}
  if not RenumberPaths(ATable) then Exit;
  
  {Set Paths}
  if not SetPaths(TCDFSDiskTable(ATable)) then Exit;
  
  {Update Descriptor} {Only on Primary Table}
  if ATable = TCDFSDiskTable(ATable).Descriptor.Primary then
   begin
    TCDFSDiskTable(ATable).Descriptor.PathTableSize:=TCDFSDiskTable(ATable).Descriptor.PathTableSize - APath.PathRecordSize(TCDFSDiskTable(ATable).Unicode);
    
    {Set Descriptor}
    if not SetDescriptor(TCDFSDiskTable(ATable).Descriptor) then Exit;
   end;
   
  APath.Free;
  
  Result:=True;
 finally
  TCDFSDiskTable(ATable).Paths.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.RenamePath(ATable:TDiskTable;APath:TCDFSDiskPath;const AName:String):Boolean;
{Note: Caller must hold the tables lock}
var
 Offset:LongWord;
 Cluster:LongWord;
 Current:TCDFSDiskPath;
 Previous:TCDFSDiskPath;
begin
 {}
 Result:=False;
 
 if FDriver = nil then Exit;
 if ATable = nil then Exit;
 if APath = nil then Exit;
 if TCDFSDiskTable(ATable).Descriptor = nil then Exit;

 if not TCDFSDiskTable(ATable).Paths.WriterLock then Exit;
 try
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.RenamePath TableNo = ' + IntToHex(ATable.TableNo,8) +  ' Path = ' + APath.Name + ' New = ' + AName);
  {$ENDIF}
  
  {Check ReadOnly}
  if FReadOnly then Exit;
  
  {Prepare Trees}
  if not PrepareTrees then Exit;
  
  {Check Relative} {Dot not allowed}
  if Length(APath.Name) = 1 then if APath.Name = cdfsDotName then Exit;

  {Update Free}
  GetTableDataFree(ATable);
  TCDFSDiskTable(ATable).DataFree:=TCDFSDiskTable(ATable).DataFree + APath.PathRecordSize(TCDFSDiskTable(ATable).Unicode);
  
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.RenamePath DataSize = ' + IntToStr(TCDFSDiskTable(ATable).DataSize) + ' DataFree = ' + IntToStr(TCDFSDiskTable(ATable).DataFree));
  {$ENDIF}

  {Move Paths}
  Current:=TCDFSDiskPath(TCDFSDiskPath(APath).Next);
  if Current <> nil then
   begin
    {Get Start}
    Offset:=TCDFSDiskPath(APath).PathOffset;
    while Current <> nil do
     begin
      {Update Path}
      Current.PathOffset:=Offset;
      
      {Update Offset}
      Inc(Offset,Current.PathRecordSize(TCDFSDiskTable(ATable).Unicode));
      
      Current:=TCDFSDiskPath(Current.Next);
     end;
   end;

  {Update Descriptor} {Only on Primary Table}
  if ATable = TCDFSDiskTable(ATable).Descriptor.Primary then
   begin
    TCDFSDiskTable(ATable).Descriptor.PathTableSize:=TCDFSDiskTable(ATable).Descriptor.PathTableSize - APath.PathRecordSize(TCDFSDiskTable(ATable).Unicode);
    
    {Set Descriptor}
    if not SetDescriptor(TCDFSDiskTable(ATable).Descriptor) then Exit;
   end;

  {Remove Path}
  if not TCDFSDiskTable(ATable).Paths.Remove(APath) then Exit;

  {Update Path}
  APath.Name:=AName;

  {Check Free}
  if GetTableDataFree(ATable) < APath.PathRecordSize(TCDFSDiskTable(ATable).Unicode) then
   begin
    {$IFDEF CDFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.RenamePath DataSize = ' + IntToStr(TCDFSDiskTable(ATable).DataSize) + ' DataFree = ' + IntToStr(TCDFSDiskTable(ATable).DataFree) + ' PathRecordSize = ' + IntToStr(APath.PathRecordSize(TCDFSDiskTable(ATable).Unicode)));
    {$ENDIF}
    
    {Allocate Cluster (Next Contiguous)}
    Cluster:=TCDFSDiskTable(ATable).StartCluster + TCDFSDiskTable(ATable).ClusterCount;
    if AllocClusters(Cluster,1) then
     begin
      {Update Table}
      //ClusterCount
      //DataSize
      //DataFree
      //To Do
      
      {Set Table}
      //To Do
     end
    else
     begin
      {Allocate Clusters (Contiguous Block)}
      Cluster:=cdfsUnknownCluster;
      if not AllocClusters(Cluster,TCDFSDiskTable(ATable).ClusterCount + 1) then Exit;
      
      {Update Table}
      //ClusterCount
      //DataSize
      //DataFree
      //To Do
      
      {Set Table}
      //To Do
      
      {Update Descriptor}
      //To Do
      
      {Set Descriptor}
      //To Do
      
      {Update Paths}
      //To Do
      
      {Set Paths}
      //To Do
     end;
   end;
   
  {Update Free}
  TCDFSDiskTable(ATable).DataFree:=TCDFSDiskTable(ATable).DataFree - APath.PathRecordSize(TCDFSDiskTable(ATable).Unicode);
  
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.RenamePath DataSize = ' + IntToStr(TCDFSDiskTable(ATable).DataSize) + ' DataFree = ' + IntToStr(TCDFSDiskTable(ATable).DataFree));
  {$ENDIF}

  {Get Previous}
  Previous:=GetPreviousPath(TCDFSDiskTable(ATable),APath);
  if Previous <> nil then
   begin
    {$IFDEF CDFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.RenamePath Previous = ' + Previous.Name);
    {$ENDIF}
    
    {Get Start}
    Offset:=Previous.PathOffset + Previous.PathRecordSize(TCDFSDiskTable(ATable).Unicode);
    
    {Update Path}
    APath.PathOffset:=Offset;
    
    {Update Offset}
    Inc(Offset,APath.PathRecordSize(TCDFSDiskTable(ATable).Unicode));
    
    {Move Paths}
    Current:=TCDFSDiskPath(TCDFSDiskPath(Previous).Next);
    while Current <> nil do
     begin
      {Update Path}
      Current.PathOffset:=Offset;
      
      {Update Offset}
      Inc(Offset,Current.PathRecordSize(TCDFSDiskTable(ATable).Unicode));
      
      Current:=TCDFSDiskPath(Current.Next);
     end;
   end
  else
   begin
    {$IFDEF CDFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.RenamePath First Path');
    {$ENDIF}
    
    APath.PathOffset:=0; {This should never occur}
   end;

  {Insert Path}
  if not TCDFSDiskTable(ATable).Paths.Insert(Previous,APath) then Exit;
  
  {Renumber Paths}
  if not RenumberPaths(ATable) then Exit;
  
  {Set Paths}
  if not SetPaths(TCDFSDiskTable(ATable)) then Exit;
  
  {Update Descriptor} {Only on Primary Table}
  if ATable = TCDFSDiskTable(ATable).Descriptor.Primary then
   begin
    TCDFSDiskTable(ATable).Descriptor.PathTableSize:=TCDFSDiskTable(ATable).Descriptor.PathTableSize + APath.PathRecordSize(TCDFSDiskTable(ATable).Unicode);
    
    {Set Descriptor}
    if not SetDescriptor(TCDFSDiskTable(ATable).Descriptor) then Exit;
   end;
   
  Result:=True;
 finally
  TCDFSDiskTable(ATable).Paths.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.MovePath(ATable:TDiskTable;ADestNumber:Word;APath:TCDFSDiskPath):Boolean;
{Note: Caller must hold the tables lock}
var
 Offset:LongWord;
 Cluster:LongWord;
 Parent:TCDFSDiskPath;
 Current:TCDFSDiskPath;
 Previous:TCDFSDiskPath;
begin
 {}
 Result:=False;
 
 if FDriver = nil then Exit;
 if ATable = nil then Exit;
 if APath = nil then Exit;
 if TCDFSDiskTable(ATable).Descriptor = nil then Exit;

 if not TCDFSDiskTable(ATable).Paths.WriterLock then Exit;
 try
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.MovePath TableNo = ' + IntToHex(ATable.TableNo,8) +  ' Path = ' + APath.Name);
  {$ENDIF}
  
  {Check ReadOnly}
  if FReadOnly then Exit;
  
  {Prepare Trees}
  if not PrepareTrees then Exit;
  
  {Get Parent}
  Parent:=LocatePath(TCDFSDiskTable(ATable),ADestNumber);
  if Parent = nil then Exit;
  
  {Check Relative} {Dot not allowed}
  if Length(APath.Name) = 1 then if APath.Name = cdfsDotName then Exit;

  {Update Free}
  GetTableDataFree(ATable);
  TCDFSDiskTable(ATable).DataFree:=TCDFSDiskTable(ATable).DataFree + APath.PathRecordSize(TCDFSDiskTable(ATable).Unicode);
  
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.MovePath DataSize = ' + IntToStr(TCDFSDiskTable(ATable).DataSize) + ' DataFree = ' + IntToStr(TCDFSDiskTable(ATable).DataFree));
  {$ENDIF}

  {Move Paths}
  Current:=TCDFSDiskPath(TCDFSDiskPath(APath).Next);
  if Current <> nil then
   begin
    {Get Start}
    Offset:=TCDFSDiskPath(APath).PathOffset;
    while Current <> nil do
     begin
      {Update Path}
      Current.PathOffset:=Offset;
      
      {Update Offset}
      Inc(Offset,Current.PathRecordSize(TCDFSDiskTable(ATable).Unicode));
      
      Current:=TCDFSDiskPath(Current.Next);
     end;
   end;

  {Update Descriptor} {Only on Primary Table}
  if ATable = TCDFSDiskTable(ATable).Descriptor.Primary then
   begin
    TCDFSDiskTable(ATable).Descriptor.PathTableSize:=TCDFSDiskTable(ATable).Descriptor.PathTableSize - APath.PathRecordSize(TCDFSDiskTable(ATable).Unicode);
    
    {Set Descriptor}
    if not SetDescriptor(TCDFSDiskTable(ATable).Descriptor) then Exit;
   end;

  {Remove Path}
  if not TCDFSDiskTable(ATable).Paths.Remove(APath) then Exit;

  {Update Path}
  APath.ParentNumber:=ADestNumber;
  APath.Parent:=Parent;

  {Check Free}
  if GetTableDataFree(ATable) < APath.PathRecordSize(TCDFSDiskTable(ATable).Unicode) then
   begin
    {$IFDEF CDFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.MovePath DataSize = ' + IntToStr(TCDFSDiskTable(ATable).DataSize) + ' DataFree = ' + IntToStr(TCDFSDiskTable(ATable).DataFree) + ' PathRecordSize = ' + IntToStr(APath.PathRecordSize(TCDFSDiskTable(ATable).Unicode)));
    {$ENDIF}
    
    {Allocate Cluster (Next Contiguous)}
    Cluster:=TCDFSDiskTable(ATable).StartCluster + TCDFSDiskTable(ATable).ClusterCount;
    if AllocClusters(Cluster,1) then
     begin
      {Update Table}
      //ClusterCount
      //DataSize
      //DataFree
      //To Do
      
      {Set Table}
      //To Do
     end
    else
     begin
      {Allocate Clusters (Contiguous Block)}
      Cluster:=cdfsUnknownCluster;
      if not AllocClusters(Cluster,TCDFSDiskTable(ATable).ClusterCount + 1) then Exit;
      
      {Update Table}
      //ClusterCount
      //DataSize
      //DataFree
      //To Do
      
      {Set Table}
      //To Do
      
      {Update Descriptor}
      //To Do
      
      {Set Descriptor}
      //To Do
      
      {Update Paths}
      //To Do
      
      {Set Paths}
      //To Do
     end;
   end;
   
  {Update Free}
  TCDFSDiskTable(ATable).DataFree:=TCDFSDiskTable(ATable).DataFree - APath.PathRecordSize(TCDFSDiskTable(ATable).Unicode);
  
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.MovePath DataSize = ' + IntToStr(TCDFSDiskTable(ATable).DataSize) + ' DataFree = ' + IntToStr(TCDFSDiskTable(ATable).DataFree));
  {$ENDIF}

  {Get Previous}
  Previous:=GetPreviousPath(TCDFSDiskTable(ATable),APath);
  if Previous <> nil then
   begin
    {$IFDEF CDFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.MovePath Previous = ' + Previous.Name);
    {$ENDIF}
    
    {Get Start}
    Offset:=Previous.PathOffset + Previous.PathRecordSize(TCDFSDiskTable(ATable).Unicode);
    
    {Update Path}
    APath.PathOffset:=Offset;
    
    {Update Offset}
    Inc(Offset,APath.PathRecordSize(TCDFSDiskTable(ATable).Unicode));
    
    {Move Paths}
    Current:=TCDFSDiskPath(TCDFSDiskPath(Previous).Next);
    while Current <> nil do
     begin
      {Update Path}
      Current.PathOffset:=Offset;
      
      {Update Offset}
      Inc(Offset,Current.PathRecordSize(TCDFSDiskTable(ATable).Unicode));
      
      Current:=TCDFSDiskPath(Current.Next);
     end;
   end
  else
   begin
    {$IFDEF CDFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.MovePath First Path');
    {$ENDIF}
    
    APath.PathOffset:=0; {This should never occur}
   end;

  {Insert Path}
  if not TCDFSDiskTable(ATable).Paths.Insert(Previous,APath) then Exit;
  
  {Renumber Paths}
  if not RenumberPaths(ATable) then Exit;
  
  {Set Paths}
  if not SetPaths(TCDFSDiskTable(ATable)) then Exit;
  
  {Update Descriptor} {Only on Primary Table}
  if ATable = TCDFSDiskTable(ATable).Descriptor.Primary then
   begin
    TCDFSDiskTable(ATable).Descriptor.PathTableSize:=TCDFSDiskTable(ATable).Descriptor.PathTableSize + APath.PathRecordSize(TCDFSDiskTable(ATable).Unicode);
    
    {Set Descriptor}
    if not SetDescriptor(TCDFSDiskTable(ATable).Descriptor) then Exit;
   end;
   
  Result:=True;
 finally
  TCDFSDiskTable(ATable).Paths.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.AddEntry(AParent:TDiskEntry;const AName:String;AAttributes:LongWord;AReference:Boolean):TDiskEntry;
{Reference: Add a reference on the returned entry if True}
var
 Name:String;
 Offset:LongWord;
 Cluster:LongWord;
 Entry:TCDFSDiskEntry;
 Current:TCDFSDiskEntry;
 Previous:TCDFSDiskEntry;
 Descriptor:TCDFSDiskDescriptor;
begin
 {}
 Result:=nil;

 if not FEntries.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if AParent = nil then Exit;

  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.AddEntry Parent = ' + AParent.Name + ' Name = ' + AName + ' Attributes = ' + IntToHex(AAttributes,8));
  {$ENDIF}
  
  {Check ReadOnly}
  if FReadOnly then Exit;
  
  {Prepare Trees}
  if not PrepareTrees then Exit;
  
  {Check Relative} {Allow Dot only on Root Directory}
  if ((AParent.Attributes and (faDot or faDotDot)) <> faNone) and (AParent <> FRoot) then Exit;
  
  {Check Parent}
  if (AParent.Attributes and faMatchMask) <> faDirectory then Exit;

  {Check Attribtues (Exclude Dot/DotDot)}
  if (AAttributes and (faDot or faDotDot)) = faNone then
   begin
    {Check Existing}
    Result:=GetEntryEx(AParent,AName,faDirectory or faFile,AReference,False,True);
    if Result <> nil then
     begin
      if (AAttributes and faMatchMask) <> (Result.Attributes and faMatchMask) then
       begin
        {Remove Reference}
        if AReference then Result.RemoveReference;
        Result:=nil;
       end;
      Exit;
     end;     
   end; 
  
  {Check for Short}
  if IsEightDotThree(AName) then
   begin
    Name:=AName
   end
  else
   begin
    {Check Supported}
    if not FLongNames then
     begin
      if IsEightDotThree(Uppercase(AName)) then
       begin
        Name:=Uppercase(AName);
       end
      else
       begin
        Exit;
       end;
     end
    else
     begin
      Name:=AName;
     end;
   end;

  {Create Entry}
  Entry:=TCDFSDiskEntry.Create(FEntryLocal);
  Entry.Unicode:=TCDFSDiskEntry(AParent).Unicode;
  Entry.Version:=cdfsBlankName;
  Entry.DataFree:=0;
  Entry.DataSize:=0;
  Entry.UnitSize:=0;
  Entry.InterleaveSize:=0;
  Entry.SequenceNumber:=TCDFSDiskEntry(AParent).SequenceNumber;
  Entry.Name:=Name;
  Entry.AltName:=cdfsBlankName;
  Entry.Size:=0;
  Entry.Attributes:=AAttributes;
  Entry.WriteTime:=Ultibo.DateTimeToFileTime(Now);
  Entry.CreateTime:=Entry.WriteTime;
  Entry.AccessTime:=Entry.WriteTime;
  Entry.EntriesLoaded:=True;
  Entry.RecordSize:=Entry.DirectoryRecordSize;
  Entry.ExtendedSize:=Entry.ExtendedRecordSize;

  {Check Free}
  if GetEntryDataFree(AParent) < Entry.DirectoryRecordSize then
   begin
    {$IFDEF CDFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.AddEntry DataSize = ' + IntToStr(TCDFSDiskEntry(AParent).DataSize) + ' DataFree = ' + IntToStr(TCDFSDiskEntry(AParent).DataFree) + ' DirectoryRecordSize = ' + IntToStr(Entry.DirectoryRecordSize));
    {$ENDIF}
    
    {Allocate Cluster (Next Contiguous)}
    Cluster:=TCDFSDiskEntry(AParent).StartCluster + TCDFSDiskEntry(AParent).ClusterCount;
    if AllocClusters(Cluster,1) then
     begin
      {Update Parent}
      //ClusterCount
      //DataSize
      //DateFree
      //To Do
      
      {Set Entry / Set Root}
      //if AParent = FRoot then SetRoot else SetEntry
      //To Do
     end
    else
     begin
      {Allocate Clusters (Contiguous Block)}
      Cluster:=cdfsUnknownCluster;
      if not AllocClusters(Cluster,TCDFSDiskEntry(AParent).ClusterCount + 1) then Exit;
      
      {Update Parent}
      //ClusterCount
      //DataSize
      //DateFree
      //To Do
      
      {Set Entry / Set Root}
      //if AParent = FRoot then SetRoot else SetEntry
      //To Do
      
      {Update Entries}
      //To Do
      
      {Set Entries}
      //To Do
     end;
   end;
   
  {Update Free}
  TCDFSDiskEntry(AParent).DataFree:=TCDFSDiskEntry(AParent).DataFree - Entry.DirectoryRecordSize;
  
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.AddEntry DataSize = ' + IntToStr(TCDFSDiskEntry(AParent).DataSize) + ' DataFree = ' + IntToStr(TCDFSDiskEntry(AParent).DataFree));
  {$ENDIF}

  {Get Previous}
  Previous:=GetPreviousEntry(TCDFSDiskEntry(AParent),Entry);
  if Previous <> nil then
   begin
    {$IFDEF CDFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.AddEntry Previous = ' + Previous.Name);
    {$ENDIF}
    
    {Get Start}
    Offset:=Previous.EntryOffset + Previous.DirectoryRecordSize;
    Cluster:=Previous.EntryCluster;
    
    {Update Cluster}
    if (Offset + Entry.DirectoryRecordSize) >= FClusterSize then
     begin
      Offset:=0;
      Inc(Cluster);
     end;
     
    {Update Entry}
    Entry.EntryOffset:=Offset;
    Entry.EntryCluster:=Cluster;
    
    {Update Offset}
    Inc(Offset,Entry.DirectoryRecordSize);
    
    {Move Entries}
    Current:=TCDFSDiskEntry(TCDFSDiskEntry(Previous).Next);
    if Current <> nil then
     begin
      while Current <> nil do
       begin
        {Update Cluster}
        if (Offset + Current.DirectoryRecordSize) >= FClusterSize then
         begin
          Offset:=0;
          Inc(Cluster);
         end;
         
        {Update Entry}
        Current.EntryOffset:=Offset;
        Current.EntryCluster:=Cluster;
        
        {Update Offset}
        Inc(Offset,Current.DirectoryRecordSize);
        
        Current:=TCDFSDiskEntry(Current.Next);
       end;
       
      {Set Entries}
      if not SetEntries(AParent) then Exit;
     end;
   end
  else
   begin
    {$IFDEF CDFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.AddEntry First Entry');
    {$ENDIF}
    
    Entry.EntryOffset:=0;
    Entry.EntryCluster:=TCDFSDiskEntry(AParent).StartCluster;
   end;

  {Check for Folder}
  if (AAttributes and faMatchMask) = faDirectory then
   begin
    {Folder}
    if (AAttributes and faDotDot) = faDotDot then
     begin
      {Dot Dot}
      if AParent = FRoot then
       begin
        {Dot Dot Refers to Parent (Root)}
        Entry.StartCluster:=TCDFSDiskEntry(AParent).StartCluster;
        Entry.ClusterCount:=TCDFSDiskEntry(AParent).ClusterCount;
        Entry.DataFree:=TCDFSDiskEntry(AParent).DataFree;
        Entry.DataSize:=TCDFSDiskEntry(AParent).DataSize;
        Entry.Name:=cdfsDotDotName;
        Entry.Attributes:=(AParent.Attributes or faDotDot);
        Entry.Attributes:=(Entry.Attributes and not(faDot)); {Mask off Dot from Root}
        Entry.WriteTime:=AParent.WriteTime;
        Entry.CreateTime:=AParent.CreateTime;
        Entry.AccessTime:=AParent.AccessTime;
       end
      else
       begin
        {Dot Dot Refers to Parents Parent}
        if AParent.Parent = nil then Exit;
        Entry.StartCluster:=TCDFSDiskEntry(AParent.Parent).StartCluster;
        Entry.ClusterCount:=TCDFSDiskEntry(AParent.Parent).ClusterCount;
        Entry.DataFree:=TCDFSDiskEntry(AParent.Parent).DataFree;
        Entry.DataSize:=TCDFSDiskEntry(AParent.Parent).DataSize;
        Entry.Name:=cdfsDotDotName;
        Entry.Attributes:=(TCDFSDiskEntry(AParent.Parent).Attributes or faDotDot);
        Entry.Attributes:=(Entry.Attributes and not(faDot)); {Mask off Dot from Root}
        Entry.WriteTime:=TCDFSDiskEntry(AParent.Parent).WriteTime;
        Entry.CreateTime:=TCDFSDiskEntry(AParent.Parent).CreateTime;
        Entry.AccessTime:=TCDFSDiskEntry(AParent.Parent).AccessTime;
       end;
       
      {Insert Entry}
      if not FEntries.Insert(Previous,Entry,AParent) then Exit;
      
      {Set Entry}
      if not SetEntry(AParent,Entry) then Exit;
     end
    else if (AAttributes and faDot) = faDot then
     begin
      {Dot}
      Entry.StartCluster:=TCDFSDiskEntry(AParent).StartCluster;
      Entry.ClusterCount:=TCDFSDiskEntry(AParent).ClusterCount;
      Entry.DataFree:=TCDFSDiskEntry(AParent).DataFree;
      Entry.DataSize:=TCDFSDiskEntry(AParent).DataSize;
      Entry.Name:=cdfsDotName;
      Entry.Attributes:=(AParent.Attributes or faDot);
      Entry.WriteTime:=AParent.WriteTime;
      Entry.CreateTime:=AParent.CreateTime;
      Entry.AccessTime:=AParent.AccessTime;
      
      {Insert Entry}
      if not FEntries.Insert(Previous,Entry,AParent) then Exit;
      
      {Set Entry}
      if not SetEntry(AParent,Entry) then Exit;
     end
    else
     begin
      {Folder}
      {Allocate Cluster}
      Cluster:=cdfsUnknownCluster;
      if not AllocClusters(Cluster,2) then Exit; {Allocate 2 Clusters, only use one (To prevent small gaps at start of CD being filled)}
      
      {$IFDEF CDFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.AddEntry Cluster = ' + IntToStr(Cluster));
      {$ENDIF}
      
      Entry.StartCluster:=Cluster;
      Entry.ClusterCount:=1;
      Entry.DataSize:=(Entry.ClusterCount shl FClusterShiftCount);
      
      {Zero Cluster}
      if not FillClusters(Entry.StartCluster,Entry.ClusterCount,0) then Exit;
      
      {Insert Entry}
      if not FEntries.Insert(Previous,Entry,AParent) then Exit;
      
      {Set Entry}
      if not SetEntry(AParent,Entry) then Exit;
      
      {Get Descriptor}
      if not FDescriptors.WriterLock then Exit;
      try
       Descriptor:=FPrimary;
       if Entry.Unicode then Descriptor:=FSupplementary;
       if Descriptor = nil then Exit;
      
       {Create Paths}
       if not FTables.ReaderLock then Exit;
       try
        if Descriptor.Primary <> nil then Entry.Path:=AddPath(Descriptor.Primary,TCDFSDiskEntry(AParent).PathNumber,Name,Entry.StartCluster);
        if Descriptor.PrimaryM <> nil then Entry.PathM:=AddPath(Descriptor.PrimaryM,TCDFSDiskEntry(AParent).PathNumber,Name,Entry.StartCluster);
        if Descriptor.Alternate <> nil then Entry.AltPath:=AddPath(Descriptor.Alternate,TCDFSDiskEntry(AParent).PathNumber,Name,Entry.StartCluster);
        if Descriptor.AlternateM <> nil then Entry.AltPathM:=AddPath(Descriptor.AlternateM,TCDFSDiskEntry(AParent).PathNumber,Name,Entry.StartCluster);
       finally
        FTables.ReaderUnlock;
       end;       
      finally
       FDescriptors.WriterUnlock;
      end; 
      
      {Create Dot}
      if AddEntry(Entry,cdfsDotName,(AAttributes or faDot),False) = nil then Exit;
      
      {Create DotDot}
      if AddEntry(Entry,cdfsDotDotName,(AAttributes or faDotDot),False) = nil then Exit;
     end;
    
    Result:=Entry;
   
    {Add Reference}
    if AReference then Result.AddReference;
   end
  else if (AAttributes and faMatchMask) = faFile then
   begin
    {File}
    {Allocate Cluster}
    Cluster:=cdfsUnknownCluster;
    if not AllocClusters(Cluster,2) then Exit; {Allocate 2 Clusters, only use one (To prevent small gaps at start of CD being filled)}
    
    {$IFDEF CDFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.AddEntry Cluster = ' + IntToStr(Cluster));
    {$ENDIF}
    
    Entry.StartCluster:=Cluster;
    Entry.ClusterCount:=1;
    
    {Zero Cluster}
    if not FillClusters(Entry.StartCluster,Entry.ClusterCount,0) then Exit;
    
    {Insert Entry}
    if not FEntries.Insert(Previous,Entry,AParent) then Exit;
    
    {Set Entry}
    if not SetEntry(AParent,Entry) then Exit;
    
    Result:=Entry;
   
    {Add Reference}
    if AReference then Result.AddReference;
   end;
 finally
  FEntries.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.AddEntryEx(AParent:TDiskEntry;const AName,AAltName:String;AAttributes:LongWord;AReference:Boolean):TDiskEntry;
{Reference: Add a reference on the returned entry if True}
{If AltName already exists then a generated one will be provided, will not fail due to AltName}
begin
 {}
 Result:=AddEntry(AParent,AName,AAttributes,AReference);
 //To Do //Temporary //To be fixed //See NTFS etc
end;

{=============================================================================}

function TCDFSFileSystem.RemoveEntry(AParent,AEntry:TDiskEntry):Boolean;
//To Do //What happens to Dot and DotDot for a Directory ?
        //They do not need to be deleted but they need to be freed ?
var
 Offset:LongWord;
 Cluster:LongWord;
 Current:TCDFSDiskEntry;
 Descriptor:TCDFSDiskDescriptor;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if AParent = nil then Exit;
  if AEntry = nil then Exit;
  if AEntry = FRoot then Exit; {Cannot remove Root}

  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.RemoveEntry Name = ' + AEntry.Name);
  {$ENDIF}
  
  {Check ReadOnly}
  if FReadOnly then Exit;
  
  {Prepare Trees}
  if not PrepareTrees then Exit;
  
  {Check Parent}
  if AEntry.Parent <> AParent then Exit;
  
  {Check Relative}
  if (AEntry.Attributes and (faDot or faDotDot)) <> faNone then Exit;
  
  {Release Clusters}
  if not ReleaseClusters(TCDFSDiskEntry(AEntry).StartCluster,TCDFSDiskEntry(AEntry).ClusterCount) then Exit;

  {Update Free}
  GetEntryDataFree(AParent);
  TCDFSDiskEntry(AParent).DataFree:=TCDFSDiskEntry(AParent).DataFree + TCDFSDiskEntry(AEntry).DirectoryRecordSize;
  
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.RemoveEntry DataSize = ' + IntToStr(TCDFSDiskEntry(AParent).DataSize) + ' DataFree = ' + IntToStr(TCDFSDiskEntry(AParent).DataFree));
  {$ENDIF}

  {Move Entries}
  Current:=TCDFSDiskEntry(TCDFSDiskEntry(AEntry).Next);
  if Current <> nil then
   begin
    {Get Start}
    Offset:=TCDFSDiskEntry(AEntry).EntryOffset;
    Cluster:=TCDFSDiskEntry(AEntry).EntryCluster;
    while Current <> nil do
     begin
      {Update Cluster}
      if (Offset + Current.DirectoryRecordSize) >= FClusterSize then
       begin
        Offset:=0;
        Inc(Cluster);
       end;
     
      {Update Entry}
      Current.EntryOffset:=Offset;
      Current.EntryCluster:=Cluster;
     
      {Update Offset}
      Inc(Offset,Current.DirectoryRecordSize);
     
      Current:=TCDFSDiskEntry(Current.Next);
     end;
   end;

  {Check Type}
  if (AEntry.Attributes and faMatchMask) = faDirectory then
   begin
    {Folder}
    {Zero Clusters}
    if not FillClusters(TCDFSDiskEntry(AEntry).StartCluster,TCDFSDiskEntry(AEntry).ClusterCount,0) then Exit;
    
    {Remove Entry}
    if not FEntries.Remove(AEntry) then Exit;
    
    {Set Entries}
    if not SetEntries(AParent) then Exit;
    
    {Get Descriptor}
    if not FDescriptors.WriterLock then Exit;
    try
     Descriptor:=FPrimary;
     if TCDFSDiskEntry(AEntry).Unicode then Descriptor:=FSupplementary;
     if Descriptor = nil then Exit;
    
     {Remove Paths}
     if not FTables.ReaderLock then Exit;
     try
      if Descriptor.Primary <> nil then if not RemovePath(Descriptor.Primary,TCDFSDiskEntry(AEntry).Path) then Exit;
      if Descriptor.PrimaryM <> nil then if not RemovePath(Descriptor.PrimaryM,TCDFSDiskEntry(AEntry).PathM) then Exit;
      if Descriptor.Alternate <> nil then if not RemovePath(Descriptor.Alternate,TCDFSDiskEntry(AEntry).AltPath) then Exit;
      if Descriptor.AlternateM <> nil then if not RemovePath(Descriptor.AlternateM,TCDFSDiskEntry(AEntry).AltPathM) then Exit;
     finally
      FTables.ReaderUnlock;
     end;       
    finally
     FDescriptors.WriterUnlock;
    end; 
   
    {Schedule Entry}
    if not FDriver.ScheduleEntry(AEntry,FILESYS_ENTRY_DELETE_TIMEOUT) then Exit;
   
    Result:=True;
   end
  else if (AEntry.Attributes and faMatchMask) = faFile then
   begin
    {File}
    {Zero Clusters}
    if TCDFSDiskEntry(AEntry).ClusterCount > 0 then if not FillClusters(TCDFSDiskEntry(AEntry).StartCluster,TCDFSDiskEntry(AEntry).ClusterCount,0) then Exit;
    
    {Remove Entry}
    if not FEntries.Remove(AEntry) then Exit;
    
    {Set Entries}
    if not SetEntries(AParent) then Exit;
   
    {Schedule Entry}
    if not FDriver.ScheduleEntry(AEntry,FILESYS_ENTRY_DELETE_TIMEOUT) then Exit;
   
    Result:=True;
   end;
 finally
  FEntries.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.RenameEntry(AParent,AEntry:TDiskEntry;const AName:String):Boolean;
var
 Name:String;
 Offset:LongWord;
 Cluster:LongWord;
 Current:TCDFSDiskEntry;
 Previous:TCDFSDiskEntry;
 Descriptor:TCDFSDiskDescriptor;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if AParent = nil then Exit;
  if AEntry = nil then Exit;
  if AEntry = FRoot then Exit; {Cannot rename Root}

  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.RenameEntry Name = ' + AEntry.Name + ' New = ' + AName);
  {$ENDIF}
  
  {Check ReadOnly}
  if FReadOnly then Exit;
  
  {Prepare Trees}
  if not PrepareTrees then Exit;
  
  {Check Relative}
  if (AEntry.Attributes and (faDot or faDotDot)) <> faNone then Exit;
  
  {Check Parent}
  if (AParent.Attributes and faMatchMask) <> faDirectory then Exit;

  {Check Parent}
  if AEntry.Parent <> AParent then Exit;
  
  {Check Attribtues (Include Folder/File)}
  if (AEntry.Attributes and (faDirectory or faFile)) <> faNone then
   begin
    {Check Existing}
    if GetEntryEx(AParent,AName,faDirectory or faFile,False,False,True) <> nil then Exit;
   end; 
  
  {Check for Short}
  if IsEightDotThree(AName) then
   begin
    Name:=AName
   end
  else
   begin
    {Check Supported}
    if not FLongNames then
     begin
      if IsEightDotThree(Uppercase(AName)) then
       begin
        Name:=Uppercase(AName);
       end
      else
       begin
        Exit;
       end;
     end
    else
     begin
      Name:=AName;
     end;
   end;

  {Update Free}
  GetEntryDataFree(AParent);
  TCDFSDiskEntry(AParent).DataFree:=TCDFSDiskEntry(AParent).DataFree + TCDFSDiskEntry(AEntry).DirectoryRecordSize;
  
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.RenameEntry DataSize = ' + IntToStr(TCDFSDiskEntry(AParent).DataSize) + ' DataFree = ' + IntToStr(TCDFSDiskEntry(AParent).DataFree));
  {$ENDIF}

  {Move Entries}
  Current:=TCDFSDiskEntry(TCDFSDiskEntry(AEntry).Next);
  if Current <> nil then
   begin
    {Get Start}
    Offset:=TCDFSDiskEntry(AEntry).EntryOffset;
    Cluster:=TCDFSDiskEntry(AEntry).EntryCluster;
    while Current <> nil do
     begin
      {Update Cluster}
      if (Offset + Current.DirectoryRecordSize) >= FClusterSize then
       begin
        Offset:=0;
        Inc(Cluster);
       end;
      
      {Update Entry}
      Current.EntryOffset:=Offset;
      Current.EntryCluster:=Cluster;
      
      {Update Offset}
      Inc(Offset,Current.DirectoryRecordSize);
      
      Current:=TCDFSDiskEntry(Current.Next);
     end;
   end;

  {Remove Entry}
  if not FEntries.Remove(AEntry) then Exit;

  {Update Entry}
  AEntry.Name:=Name;

  {Check Free}
  if GetEntryDataFree(AParent) < TCDFSDiskEntry(AEntry).DirectoryRecordSize then
   begin
    {$IFDEF CDFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.RenameEntry DataSize = ' + IntToStr(TCDFSDiskEntry(AParent).DataSize) + ' DataFree = ' + IntToStr(TCDFSDiskEntry(AParent).DataFree) + ' DirectoryRecordSize = ' + IntToStr(TCDFSDiskEntry(AEntry).DirectoryRecordSize));
    {$ENDIF}
    
    {Allocate Cluster (Next Contiguous)}
    Cluster:=TCDFSDiskEntry(AParent).StartCluster + TCDFSDiskEntry(AParent).ClusterCount;
    if AllocClusters(Cluster,1) then
     begin
      {Update Parent}
      //ClusterCount
      //DataSize
      //DateFree
      //To Do
      
      {Set Entry / Set Root}
      //if AParent = FRoot then SetRoot else SetEntry
      //To Do
     end
    else
     begin
      {Allocate Clusters (Contiguous Block)}
      Cluster:=cdfsUnknownCluster;
      if not AllocClusters(Cluster,TCDFSDiskEntry(AParent).ClusterCount + 1) then Exit;
      
      {Update Parent}
      //ClusterCount
      //DataSize
      //DateFree
      //To Do
      
      {Set Entry / Set Root}
      //if AParent = FRoot then SetRoot else SetEntry
      //To Do
      
      {Update Entries}
      //To Do
      
      {Set Entries}
      //To Do
     end;
   end;
   
  {Update Free}
  TCDFSDiskEntry(AParent).DataFree:=TCDFSDiskEntry(AParent).DataFree - TCDFSDiskEntry(AEntry).DirectoryRecordSize;
  
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.RenameEntry DataSize = ' + IntToStr(TCDFSDiskEntry(AParent).DataSize) + ' DataFree = ' + IntToStr(TCDFSDiskEntry(AParent).DataFree));
  {$ENDIF}

  {Get Previous}
  Previous:=GetPreviousEntry(TCDFSDiskEntry(AParent),TCDFSDiskEntry(AEntry));
  if Previous <> nil then
   begin
    {$IFDEF CDFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.RenameEntry Previous = ' + Previous.Name);
    {$ENDIF}
    
    {Get Start}
    Offset:=Previous.EntryOffset + Previous.DirectoryRecordSize;
    Cluster:=Previous.EntryCluster;
    
    {Update Cluster}
    if (Offset + TCDFSDiskEntry(AEntry).DirectoryRecordSize) >= FClusterSize then
     begin
      Offset:=0;
      Inc(Cluster);
     end;
     
    {Update Entry}
    TCDFSDiskEntry(AEntry).EntryOffset:=Offset;
    TCDFSDiskEntry(AEntry).EntryCluster:=Cluster;
    
    {Update Offset}
    Inc(Offset,TCDFSDiskEntry(AEntry).DirectoryRecordSize);
    
    {Move Entries}
    Current:=TCDFSDiskEntry(TCDFSDiskEntry(Previous).Next);
    if Current <> nil then
     begin
      while Current <> nil do
       begin
        {Update Cluster}
        if (Offset + Current.DirectoryRecordSize) >= FClusterSize then
         begin
          Offset:=0;
          Inc(Cluster);
         end;
         
        {Update Entry}
        Current.EntryOffset:=Offset;
        Current.EntryCluster:=Cluster;
        
        {Update Offset}
        Inc(Offset,Current.DirectoryRecordSize);
        
        Current:=TCDFSDiskEntry(Current.Next);
       end;
     end;
   end
  else
   begin
    {$IFDEF CDFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.RenameEntry First Entry');
    {$ENDIF}
    
    TCDFSDiskEntry(AEntry).EntryOffset:=0;  {This should never occur (Dot is always first and cannot be renamed)}
    TCDFSDiskEntry(AEntry).EntryCluster:=TCDFSDiskEntry(AParent).StartCluster;
   end;

  {Check for Folder}
  if (AEntry.Attributes and faMatchMask) = faDirectory then
   begin
    {Folder}
    {Insert Entry}
    if not FEntries.Insert(Previous,AEntry,AParent) then Exit;
    
    {Set Entries}
    if not SetEntries(AParent) then Exit;
    
    {Get Descriptor}
    if not FDescriptors.WriterLock then Exit;
    try
     Descriptor:=FPrimary;
     if TCDFSDiskEntry(AEntry).Unicode then Descriptor:=FSupplementary;
     if Descriptor = nil then Exit;
    
     {Rename Paths}
     if not FTables.ReaderLock then Exit;
     try
      if (Descriptor.Primary <> nil) and (TCDFSDiskEntry(AEntry).Path <> nil) then if not RenamePath(Descriptor.Primary,TCDFSDiskEntry(AEntry).Path,Name) then Exit;
      if (Descriptor.PrimaryM <> nil) and (TCDFSDiskEntry(AEntry).PathM <> nil) then if not RenamePath(Descriptor.PrimaryM,TCDFSDiskEntry(AEntry).PathM,Name) then Exit;
      if (Descriptor.Alternate <> nil) and (TCDFSDiskEntry(AEntry).AltPath <> nil) then if not RenamePath(Descriptor.Alternate,TCDFSDiskEntry(AEntry).AltPath,Name) then Exit;
      if (Descriptor.AlternateM <> nil) and (TCDFSDiskEntry(AEntry).AltPathM <> nil) then if not RenamePath(Descriptor.AlternateM,TCDFSDiskEntry(AEntry).AltPathM,Name) then Exit;
     finally
      FTables.ReaderUnlock;
     end;       
    finally
     FDescriptors.WriterUnlock;
    end; 
    
    Result:=True;
   end
  else if (AEntry.Attributes and faMatchMask) = faFile then
   begin
    {File}
    {Insert Entry}
    if not FEntries.Insert(Previous,AEntry,AParent) then Exit;
    
    {Set Entries}
    if not SetEntries(AParent) then Exit;
    
    Result:=True;
   end;
 finally
  FEntries.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.RenameEntryEx(AParent,AEntry:TDiskEntry;const AAltName:String):Boolean; 
begin
 {}
 Result:=False;
 
 if not FEntries.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if AParent = nil then Exit;
  if AEntry = nil then Exit;
  if AEntry = FRoot then Exit; {Cannot rename root}

  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.RenameEntryEx - Entry = ' + AEntry.Name + ' AltName = ' + AAltName);
  {$ENDIF}
  
  {Check ReadOnly}
  if FReadOnly then Exit;
  
  {Prepare Trees}
  if not PrepareTrees then Exit;
  
  {Check Relative}
  if (AEntry.Attributes and (faDot or faDotDot)) <> faNone then Exit;
  
  {Check Parent}
  if (AParent.Attributes and faMatchMask) <> faDirectory then Exit;
 
  {Check Parent}
  if AEntry.Parent <> AParent then Exit;
 
  //To Do //See Above and AddEntryEx
 finally
  FEntries.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.MoveEntry(ASource,ADest,AEntry:TDiskEntry):Boolean;
var
 Name:String;
 Offset:LongWord;
 Cluster:LongWord;
 Current:TCDFSDiskEntry;
 Previous:TCDFSDiskEntry;
 Descriptor:TCDFSDiskDescriptor;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ASource = nil then Exit;
  if ADest = nil then Exit;
  if AEntry = nil then Exit;
  if AEntry = FRoot then Exit; {Cannot move Root}
  if ASource = ADest then Exit;

  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.MoveEntry Name = ' + AEntry.Name + ' Dest = ' + ADest.Name);
  {$ENDIF}
  
  {Check ReadOnly}
  if FReadOnly then Exit;
  
  {Prepare Trees}
  if not PrepareTrees then Exit;
  
  {Check Relative}
  if (AEntry.Attributes and (faDot or faDotDot)) <> faNone then Exit;
  
  {Check Source}
  if (ASource.Attributes and faMatchMask) <> faDirectory then Exit;
  
  {Check Dest}
  if (ADest.Attributes and faMatchMask) <> faDirectory then Exit;

  {Check Parent}
  if AEntry.Parent <> ASource then Exit;
  
  {Check Attribtues (Include Folder/File)}
  if (AEntry.Attributes and (faDirectory or faFile)) <> faNone then
   begin
    {Check Existing}
    if GetEntryEx(ADest,AEntry.Name,faDirectory or faFile,False,False,True) <> nil then Exit;
   end; 
  
  {Update Free}
  GetEntryDataFree(ASource);
  TCDFSDiskEntry(ASource).DataFree:=TCDFSDiskEntry(ASource).DataFree + TCDFSDiskEntry(AEntry).DirectoryRecordSize;
  
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.MoveEntry DataSize = ' + IntToStr(TCDFSDiskEntry(ASource).DataSize) + ' DataFree = ' + IntToStr(TCDFSDiskEntry(ASource).DataFree));
  {$ENDIF}

  {Move Entries}
  Current:=TCDFSDiskEntry(TCDFSDiskEntry(AEntry).Next);
  if Current <> nil then
   begin
    {Get Start}
    Offset:=TCDFSDiskEntry(AEntry).EntryOffset;
    Cluster:=TCDFSDiskEntry(AEntry).EntryCluster;
    while Current <> nil do
     begin
      {Update Cluster}
      if (Offset + Current.DirectoryRecordSize) >= FClusterSize then
       begin
        Offset:=0;
        Inc(Cluster);
       end;
      
      {Update Entry}
      Current.EntryOffset:=Offset;
      Current.EntryCluster:=Cluster;
      
      {Update Offset}
      Inc(Offset,Current.DirectoryRecordSize);
      
      Current:=TCDFSDiskEntry(Current.Next);
     end;
   end;

  {Remove Entry}
  if not FEntries.Remove(AEntry) then Exit;
  
  {Set Entries}
  if not SetEntries(ASource) then Exit;

  {Check Free}
  if GetEntryDataFree(ADest) < TCDFSDiskEntry(AEntry).DirectoryRecordSize then
   begin
    {$IFDEF CDFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.MoveEntry DataSize = ' + IntToStr(TCDFSDiskEntry(ADest).DataSize) + ' DataFree = ' + IntToStr(TCDFSDiskEntry(ADest).DataFree) + ' DirectoryRecordSize = ' + IntToStr(TCDFSDiskEntry(AEntry).DirectoryRecordSize));
    {$ENDIF}
    
    {Allocate Cluster (Next Contiguous)}
    Cluster:=TCDFSDiskEntry(ADest).StartCluster + TCDFSDiskEntry(ADest).ClusterCount;
    if AllocClusters(Cluster,1) then
     begin
      {Update Parent}
      //ClusterCount
      //DataSize
      //DateFree
      //To Do
      
      {Set Entry / Set Root}
      //if AParent = FRoot then SetRoot else SetEntry
      //To Do
     end
    else
     begin
      {Allocate Clusters (Contiguous Block)}
      Cluster:=cdfsUnknownCluster;
      if not AllocClusters(Cluster,TCDFSDiskEntry(ADest).ClusterCount + 1) then Exit;
      
      {Update Parent}
      //ClusterCount
      //DataSize
      //DateFree
      //To Do
      
      {Set Entry / Set Root}
      //if AParent = FRoot then SetRoot else SetEntry
      //To Do
      
      {Update Entries}
      //To Do
      
      {Set Entries}
      //To Do
     end;
   end;
  
  {Update Free}
  TCDFSDiskEntry(ADest).DataFree:=TCDFSDiskEntry(ADest).DataFree - TCDFSDiskEntry(AEntry).DirectoryRecordSize;
  
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.MoveEntry DataSize = ' + IntToStr(TCDFSDiskEntry(ADest).DataSize) + ' DataFree = ' + IntToStr(TCDFSDiskEntry(ADest).DataFree));
  {$ENDIF}

  {Get Previous}
  Previous:=GetPreviousEntry(TCDFSDiskEntry(ADest),TCDFSDiskEntry(AEntry));
  if Previous <> nil then
   begin
    {$IFDEF CDFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.MoveEntry Previous = ' + Previous.Name);
    {$ENDIF}
    
    {Get Start}
    Offset:=Previous.EntryOffset + Previous.DirectoryRecordSize;
    Cluster:=Previous.EntryCluster;
    
    {Update Cluster}
    if (Offset + TCDFSDiskEntry(AEntry).DirectoryRecordSize) >= FClusterSize then
     begin
      Offset:=0;
      Inc(Cluster);
     end;
    
    {Update Entry}
    TCDFSDiskEntry(AEntry).EntryOffset:=Offset;
    TCDFSDiskEntry(AEntry).EntryCluster:=Cluster;
    
    {Update Offset}
    Inc(Offset,TCDFSDiskEntry(AEntry).DirectoryRecordSize);
    
    {Move Entries}
    Current:=TCDFSDiskEntry(TCDFSDiskEntry(Previous).Next);
    if Current <> nil then
     begin
      while Current <> nil do
       begin
        {Update Cluster}
        if (Offset + Current.DirectoryRecordSize) >= FClusterSize then
         begin
          Offset:=0;
          Inc(Cluster);
         end;
        
        {Update Entry}
        Current.EntryOffset:=Offset;
        Current.EntryCluster:=Cluster;
        
        {Update Offset}
        Inc(Offset,Current.DirectoryRecordSize);
        
        Current:=TCDFSDiskEntry(Current.Next);
       end;
     end;
   end
  else
   begin
    {$IFDEF CDFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.MoveEntry First Entry');
    {$ENDIF}
    
    TCDFSDiskEntry(AEntry).EntryOffset:=0;  {This should never occur (Dot is always first and cannot be moved)}
    TCDFSDiskEntry(AEntry).EntryCluster:=TCDFSDiskEntry(ADest).StartCluster;
   end;

  {Check for Folder}
  if (AEntry.Attributes and faMatchMask) = faDirectory then
   begin
    {Folder}
    {Insert Entry}
    if not FEntries.Insert(Previous,AEntry,ADest) then Exit;
    
    {Set Entries}
    if not SetEntries(ADest) then Exit;
    
    {Get Descriptor}
    if not FDescriptors.WriterLock then Exit;
    try
     Descriptor:=FPrimary;
     if TCDFSDiskEntry(AEntry).Unicode then Descriptor:=FSupplementary;
     if Descriptor = nil then Exit;
    
     {Rename Paths}
     if not FTables.ReaderLock then Exit;
     try
      if (Descriptor.Primary <> nil) and (TCDFSDiskEntry(AEntry).Path <> nil) then if not MovePath(Descriptor.Primary,TCDFSDiskEntry(ADest).PathNumber,TCDFSDiskEntry(AEntry).Path) then Exit;
      if (Descriptor.PrimaryM <> nil) and (TCDFSDiskEntry(AEntry).PathM <> nil) then if not MovePath(Descriptor.PrimaryM,TCDFSDiskEntry(ADest).PathNumber,TCDFSDiskEntry(AEntry).PathM) then Exit;
      if (Descriptor.Alternate <> nil) and (TCDFSDiskEntry(AEntry).AltPath <> nil) then if not MovePath(Descriptor.Alternate,TCDFSDiskEntry(ADest).PathNumber,TCDFSDiskEntry(AEntry).AltPath) then Exit;
      if (Descriptor.AlternateM <> nil) and (TCDFSDiskEntry(AEntry).AltPathM <> nil) then if not MovePath(Descriptor.AlternateM,TCDFSDiskEntry(ADest).PathNumber,TCDFSDiskEntry(AEntry).AltPathM) then Exit;
     finally
      FTables.ReaderUnlock;
     end;       
    finally
     FDescriptors.WriterUnlock;
    end; 
    
    {Get DotDot}
    Current:=TCDFSDiskEntry(AEntry.FirstChild);
    while Current <> nil do
     begin
      if (Current.Attributes and faDotDot) = faDotDot then
       begin
        if ADest = FRoot then
         begin
          {Dot Dot Refers to Parent (Root)}
          TCDFSDiskEntry(AEntry).StartCluster:=TCDFSDiskEntry(ADest).StartCluster;
          TCDFSDiskEntry(AEntry).ClusterCount:=TCDFSDiskEntry(ADest).ClusterCount;
          TCDFSDiskEntry(AEntry).DataFree:=TCDFSDiskEntry(ADest).DataFree;
          TCDFSDiskEntry(AEntry).DataSize:=TCDFSDiskEntry(ADest).DataSize;
          TCDFSDiskEntry(AEntry).Name:=cdfsDotDotName;
          TCDFSDiskEntry(AEntry).Attributes:=(ADest.Attributes or faDotDot);
          TCDFSDiskEntry(AEntry).Attributes:=(AEntry.Attributes and not(faDot)); {Mask off Dot from Root}
          TCDFSDiskEntry(AEntry).WriteTime:=ADest.WriteTime;
          TCDFSDiskEntry(AEntry).CreateTime:=ADest.CreateTime;
          TCDFSDiskEntry(AEntry).AccessTime:=ADest.AccessTime;
         end
        else
         begin
          {Dot Dot Refers to Parents Parent}
          if ADest.Parent = nil then Exit;
          TCDFSDiskEntry(AEntry).StartCluster:=TCDFSDiskEntry(ADest.Parent).StartCluster;
          TCDFSDiskEntry(AEntry).ClusterCount:=TCDFSDiskEntry(ADest.Parent).ClusterCount;
          TCDFSDiskEntry(AEntry).DataFree:=TCDFSDiskEntry(ADest.Parent).DataFree;
          TCDFSDiskEntry(AEntry).DataSize:=TCDFSDiskEntry(ADest.Parent).DataSize;
          TCDFSDiskEntry(AEntry).Name:=cdfsDotDotName;
          TCDFSDiskEntry(AEntry).Attributes:=(TCDFSDiskEntry(ADest.Parent).Attributes or faDotDot);
          TCDFSDiskEntry(AEntry).Attributes:=(AEntry.Attributes and not(faDot)); {Mask off Dot from Root}
          TCDFSDiskEntry(AEntry).WriteTime:=TCDFSDiskEntry(ADest.Parent).WriteTime;
          TCDFSDiskEntry(AEntry).CreateTime:=TCDFSDiskEntry(ADest.Parent).CreateTime;
          TCDFSDiskEntry(AEntry).AccessTime:=TCDFSDiskEntry(ADest.Parent).AccessTime;
         end;
         
        if not SetEntry(AEntry,Current) then Exit;
       end;
       
      Current:=TCDFSDiskEntry(Current.Next);
     end;
     
    Result:=True;
   end
  else if (AEntry.Attributes and faMatchMask) = faFile then
   begin
    {File}
    {Insert Entry}
    if not FEntries.Insert(Previous,AEntry,ADest) then Exit;
    
    {Set Entries}
    if not SetEntries(ADest) then Exit;
    
    Result:=True;
   end;
 finally
  FEntries.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.AddCatalog(AEntry:TDiskEntry;AMediaType:TMediaType;AFloppyType:TFloppyType;AAttributes:LongWord;ASectorSize:Word;const ASectorCount:Int64):TDiskCatalog;
var
 Offset:LongWord;
 Cluster:LongWord;
 Header:TCDFSDiskHeader;
 Catalog:TCDFSDiskCatalog;
 Descriptor:TCDFSDiskDescriptor;
begin
 {}
 Result:=nil;
 
 if not FEntries.WriterLock then Exit;
 try
  if not FCatalogs.WriterLock then Exit;
  try
   if not FHeaders.WriterLock then Exit;
   try
    if not FExtensions.WriterLock then Exit;
    try
     if FDriver = nil then Exit;
     if AEntry = nil then Exit;

     {$IFDEF CDFS_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.AddCatalog -  Name = ' + AEntry.Name);
     {$ENDIF}
  
     {Check ReadOnly}
     if FReadOnly then Exit;
  
     {Prepare Trees}
     {if not PrepareTrees then Exit;} {Handled by AddEntry if needed}
  
     {Check Entry}
     if (AEntry.Attributes and faMatchMask) <> faFile then Exit;
  
     {Check Params}
     //To Do

     {Get Descriptor}
     if not FDescriptors.WriterLock then Exit;
     try
      Descriptor:=FBoot;
      if Descriptor = nil then
       begin
        {Add Descriptor}
        Descriptor:=AddDescriptor(cdfsVolumeDescriptorTypeBoot);
        if Descriptor = nil then Exit;
       end; 
  
      {Check Free}
      if GetCatalogDataFree(Descriptor) < (cdfsHeaderRecordSize + cdfsCatalogRecordSize) then
       begin
        {Allocate Cluster (Next Contiguous)}
        Cluster:=Descriptor.CatalogStart + Descriptor.CatalogCount;
        if AllocClusters(Cluster,1) then
         begin
          {Update Descriptor}
          Descriptor.CatalogCount:=Descriptor.CatalogCount + 1;
         end
        else
         begin
          {Allocate Clusters (Contiguous Block)}
          Cluster:=cdfsUnknownCluster;
          if not AllocClusters(Cluster,Descriptor.CatalogCount + 1) then Exit;
       
          {Update Descriptor}
          Descriptor.CatalogStart:=Cluster;
          Descriptor.CatalogCount:=Descriptor.CatalogCount + 1;
      
          {Set Descriptor}
          if not SetDescriptor(Descriptor) then Exit;
      
          {Update Validation}
          //To Do
      
          {Update Initial}
          //To Do
        
          {Update Headers}
          //To Do
      
          {Update Catalogs}
          //To Do
      
          {Update Extensions}
          //To Do
       
          {Set Catalogs}
          if not SetCatalogs then Exit;
         end;
       end;

      {Create Header}
      Header:=TCDFSDiskHeader.Create(FHeaderLocal);
      Header.HeaderNo:=GetNextHeaderNo;
      Header.PlatformId:=cdfsElToritoPlatformIdx86;

      {Create Catalog}
      Catalog:=TCDFSDiskCatalog.Create(FCatalogLocal);
      Catalog.LoadSegment:=$07C0;
      Catalog.SystemType:=$06;
      Catalog.LoadCount:=$01;
      Catalog.Entry:=TCDFSDiskEntry(AEntry);
      Catalog.Header:=Header;
      Catalog.Path:=GetEntryPath(AEntry,False);
      Catalog.CatalogNo:=GetNextCatalogNo;
      Catalog.MediaType:=AMediaType;
      Catalog.FloppyType:=AFloppyType;
      Catalog.Attributes:=AAttributes;
      Catalog.SectorSize:=ASectorSize;
      Catalog.SectorCount:=ASectorCount;

      {Check Validation}
      if Descriptor.Validation = nil then
       begin
        if Descriptor.Initial <> nil then Exit;
      
        {Add Validation}
        Header.Validation:=True;
        Header.HeaderId:=cdfsElToritoHeaderId;
        Header.VendorId:='Ultibo Boot Record';
        Header.Checksum:=0; {Recalculated by SetHeader}
        Header.Signature:=cdfsElToritoSignature;
        Header.HeaderOffset:=0;
        Header.HeaderCluster:=Descriptor.CatalogStart;
        Descriptor.Validation:=Header;

        {Add Initial}
        Catalog.Initial:=True;
        Catalog.CatalogOffset:=cdfsHeaderRecordSize;
        Catalog.CatalogCluster:=Descriptor.CatalogStart;
        Descriptor.Initial:=Catalog;
       end
      else
       begin
        if Descriptor.Initial = nil then Exit;
    
        {Add Header}
        Header.Validation:=False;
        Header.HeaderIndicator:=cdfsElToritoHeaderTerminator;
        Header.SectionCount:=1;
        Header.SectionId:='Ultibo Boot Catalog';

        {Get Previous}
        //Header.HeaderOffset
        //Header.HeaderCluster
        //To Do

        {Add Catalog}
        Catalog.Initial:=False;
        Catalog.SelectionType:=0;
        Catalog.SelectionData:=nil;

        {Get Previous}
        //Catalog.CatalogOffset
        //Catalog.CatalogCluster
        //To Do
       end;

      {Add Header}
      if not FHeaders.Add(Header) then Exit;
  
      {Add Catalog}
      if not FCatalogs.Add(Catalog) then Exit;
  
      {Set Catalogs}
      if not SetCatalogs then Exit;
  
      Result:=Catalog;
     finally
      FDescriptors.WriterUnlock;
     end; 
    finally
     FExtensions.WriterUnlock;
    end; 
   finally
    FHeaders.WriterUnlock;
   end; 
  finally
   FCatalogs.WriterUnlock;
  end; 
 finally
  FEntries.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.RemoveCatalog(ACatalog:TDiskCatalog):Boolean;
var
 Header:TCDFSDiskHeader;
 Extension:TCDFSDiskExtension;
 Descriptor:TCDFSDiskDescriptor;
begin
 {}
 Result:=False;
 
 if not FEntries.WriterLock then Exit;
 try
  if not FCatalogs.WriterLock then Exit;
  try
   if not FHeaders.WriterLock then Exit;
   try
    if not FExtensions.WriterLock then Exit;
    try
     if FDriver = nil then Exit;
     if ACatalog = nil then Exit;

     {$IFDEF CDFS_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.RemoveCatalog -  CatalogNo = ' + IntToStr(ACatalog.CatalogNo) + ' Name = ' + ACatalog.Name);
     {$ENDIF}
  
     {Check ReadOnly}
     if FReadOnly then Exit;
  
     {Prepare Trees}
     {if not PrepareTrees then Exit;} {Handled by RemoveEntry if needed}
  
     {Check Count}
     if TCDFSDiskCatalog(ACatalog).ClusterCount = 0 then Exit;
  
     {Get Descriptor}
     if not FDescriptors.WriterLock then Exit;
     try
      Descriptor:=FBoot;
      if Descriptor = nil then Exit;

      //To Do

      //Remember this bit

      //{Update Enum Handles}
      //UpdateEnumHandles(nil,nil,nil,nil,nil,ACatalog);
      //{Remove Catalog}
  
     finally
      FDescriptors.WriterUnlock;
     end; 
    finally
     FExtensions.WriterUnlock;
    end; 
   finally
    FHeaders.WriterUnlock;
   end; 
  finally
   FCatalogs.WriterUnlock;
  end; 
 finally
  FEntries.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.GetNextHeaderNo:LongWord;
begin
 {}
 Result:=1;
 
 while GetHeader(Result) <> nil do
  begin
   Inc(Result);
  end;
end;

{=============================================================================}

function TCDFSFileSystem.GetMaxHeaderNo:LongWord;
var
 Header:TCDFSDiskHeader;
begin
 {}
 Result:=0;
 
 if not FHeaders.ReaderLock then Exit;
 try
  Header:=TCDFSDiskHeader(FHeaders.First);
  while Header <> nil do
   begin
    if Header.HeaderNo > Result then
     begin
      Result:=Header.HeaderNo;
     end;
    
    Header:=TCDFSDiskHeader(Header.Next);
   end;
 finally
  FHeaders.ReaderUnlock;
 end;   
end;

{=============================================================================}

function TCDFSFileSystem.GetNextHeaderOffset:LongWord;
begin
 {}
 Result:=0;
 
 //To Do
end;

{=============================================================================}

function TCDFSFileSystem.GetNextHeaderCluster:LongWord;
begin
 {}
 Result:=0;
 
 //To Do
end;

{=============================================================================}

function TCDFSFileSystem.GetNextCatalogOffset:LongWord;
begin
 {}
 Result:=0;
 
 //To Do
end;

{=============================================================================}

function TCDFSFileSystem.GetNextCatalogCluster:LongWord;
begin
 {}
 Result:=0;
 
 //To Do
end;

{=============================================================================}

function TCDFSFileSystem.SetTable(ATable:TDiskTable):Boolean;
{Note: SetTable does not perform any disk writes}
begin
 {}
 Result:=False;
 
 if not FTables.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ATable = nil then Exit;

  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.SetTable - TableNo = ' + IntToStr(ATable.TableNo));
  {$ENDIF}
 
  {Check ReadOnly}
  if FReadOnly then Exit;
 
  Result:=True;
 finally
  FTables.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.SetBlock(ABlock:TDiskBlock):Boolean;
{Note: SetBlock does not perform any disk writes (Blocks are not stored in filesystem)}
begin
 {}
 Result:=False;
 
 if not FBlocks.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ABlock = nil then Exit;

  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.SetBlock - BlockNo = ' + IntToHex(ABlock.BlockNo,8));
  {$ENDIF}
 
  {Check ReadOnly}
  if FReadOnly then Exit;
 
  Result:=True;
 finally
  FBlocks.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.SetDescriptor(ADescriptor:TCDFSDiskDescriptor):Boolean;
var
 Descriptor:PCDFSVolumeDescriptorHeader;
 BootDescriptor:PELTORITOVolumeDescriptorBoot;
 PrimaryDescriptor:PCDFSVolumeDescriptorPrimary;
 PartitionDescriptor:PCDFSVolumeDescriptorPartition;
 TerminatorDescriptor:PCDFSVolumeDescriptorTerminator;
 SupplementaryDescriptor:PCDFSVolumeDescriptorSupplementary;
begin
 {}
 Result:=False;
 
 if not FDescriptors.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ADescriptor = nil then Exit;
  if FClusterBuffer = nil then Exit;

  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.SetDescriptor - Type = ' + IntToStr(ADescriptor.DescriptorType));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.SetDescriptor - StartCluster = ' + IntToStr(ADescriptor.StartCluster));
  {$ENDIF}
  
  {Check ReadOnly}
  if FReadOnly then Exit;
  
  FBlockWrite:=True;
  try
   {Read Cluster}
   if not ReadClusters(ADescriptor.StartCluster,1,FClusterBuffer^) then Exit;
   
   {Zero Cluster}
   ZeroMemory(FClusterBuffer,FClusterSize);
   
   {Get Descriptor}
   Descriptor:=PCDFSVolumeDescriptorHeader(FClusterBuffer);
   
   {Set Descriptor}
   Descriptor.DescriptorType:=ADescriptor.DescriptorType;
   if not CDFSStringToIdentifier(ADescriptor.StandardIdentifier,Descriptor.StandardIdentifier[0],5,False) then Exit;
   Descriptor.DescriptorVersion:=ADescriptor.DescriptorVersion;
   
   {Check Type}
   case ADescriptor.DescriptorType of
    cdfsVolumeDescriptorTypeBoot:begin
      {Get Descriptor}
      BootDescriptor:=PELTORITOVolumeDescriptorBoot(FClusterBuffer);
      
      {Set Descriptor}
      if not CDFSStringToIdentifier(ADescriptor.SystemIdentifier + StringOfChar(#0,32 - Length(ADescriptor.SystemIdentifier)),BootDescriptor.SystemIdentifier[0],32,False) then Exit;
      BootDescriptor.CatalogStart:=ADescriptor.CatalogStart;
      
      {Write Clusters}
      if not WriteClusters(ADescriptor.StartCluster,1,FClusterBuffer^) then Exit;
      
      Result:=True;
     end;
    cdfsVolumeDescriptorTypePrimary:begin
      {Get Descriptor}
      PrimaryDescriptor:=PCDFSVolumeDescriptorPrimary(FClusterBuffer);
      
      {Set Descriptor}
      if not CDFSStringToIdentifier(ADescriptor.SystemIdentifier + StringOfChar(' ',32 - Length(ADescriptor.SystemIdentifier)),PrimaryDescriptor.SystemIdentifier[0],32,False) then Exit;
      if not CDFSStringToIdentifier(ADescriptor.VolumeIdentifier + StringOfChar(' ',32 - Length(ADescriptor.VolumeIdentifier)),PrimaryDescriptor.VolumeIdentifier[0],32,False) then Exit;
      PrimaryDescriptor.VolumeSpaceSize:=ADescriptor.VolumeSpaceSize;
      PrimaryDescriptor.VolumeSpaceSizeM:=LongWordNtoBE(ADescriptor.VolumeSpaceSize);
      PrimaryDescriptor.VolumeSetSize:=ADescriptor.VolumeSetSize;
      PrimaryDescriptor.VolumeSetSizeM:=WordNToBE(ADescriptor.VolumeSetSize);
      PrimaryDescriptor.VolumeSequenceNumber:=ADescriptor.VolumeSequenceNumber;
      PrimaryDescriptor.VolumeSequenceNumberM:=WordNToBE(ADescriptor.VolumeSequenceNumber);
      PrimaryDescriptor.LogicalBlockSize:=ADescriptor.LogicalBlockSize;
      PrimaryDescriptor.LogicalBlockSizeM:=WordNToBE(ADescriptor.LogicalBlockSize);
      PrimaryDescriptor.PathTableSize:=ADescriptor.PathTableSize;
      PrimaryDescriptor.PathTableSizeM:=LongWordNtoBE(ADescriptor.PathTableSize);
      PrimaryDescriptor.PrimaryPathTable:=ADescriptor.PrimaryPathTable;
      PrimaryDescriptor.AlternatePathTable:=ADescriptor.AlternatePathTable;
      PrimaryDescriptor.PrimaryPathTableM:=LongWordNtoBE(ADescriptor.PrimaryPathTableM);
      PrimaryDescriptor.AlternatePathTableM:=LongWordNtoBE(ADescriptor.AlternatePathTableM);
      if not CDFSStringToIdentifier(ADescriptor.VolumeSetIdentifier + StringOfChar(' ',128 - Length(ADescriptor.VolumeSetIdentifier)),PrimaryDescriptor.VolumeSetIdentifier[0],128,False) then Exit;
      if not CDFSStringToIdentifier(ADescriptor.PublisherIdentifier + StringOfChar(' ',128 - Length(ADescriptor.PublisherIdentifier)),PrimaryDescriptor.PublisherIdentifier[0],128,False) then Exit;
      if not CDFSStringToIdentifier(ADescriptor.PreparerIdentifier + StringOfChar(' ',128 - Length(ADescriptor.PreparerIdentifier)),PrimaryDescriptor.PreparerIdentifier[0],128,False) then Exit;
      if not CDFSStringToIdentifier(ADescriptor.ApplicationIdentifier + StringOfChar(' ',128 - Length(ADescriptor.ApplicationIdentifier)),PrimaryDescriptor.ApplicationIdentifier[0],128,False) then Exit;
      if not CDFSStringToIdentifier(ADescriptor.CopyrightIdentifier + StringOfChar(' ',37 - Length(ADescriptor.CopyrightIdentifier)),PrimaryDescriptor.CopyrightIdentifier[0],37,False) then Exit;
      if not CDFSStringToIdentifier(ADescriptor.AbstractIdentifier + StringOfChar(' ',37 - Length(ADescriptor.AbstractIdentifier)),PrimaryDescriptor.AbstractIdentifier[0],37,False) then Exit;
      if not CDFSStringToIdentifier(ADescriptor.BibliographicIdentifier + StringOfChar(' ',37 - Length(ADescriptor.BibliographicIdentifier)),PrimaryDescriptor.BibliographicIdentifier[0],37,False) then Exit;
      if not FileTimeToCDFSDateTime(ADescriptor.CreateTime,PrimaryDescriptor.CreateTime) then Exit;
      if not FileTimeToCDFSDateTime(ADescriptor.ModifyTime,PrimaryDescriptor.ModifyTime) then Exit;
      if not FileTimeToCDFSDateTime(ADescriptor.ExpireTime,PrimaryDescriptor.ExpireTime) then Exit;
      if not FileTimeToCDFSDateTime(ADescriptor.EffectiveTime,PrimaryDescriptor.EffectiveTime) then Exit;
      PrimaryDescriptor.FileStructureVersion:=ADescriptor.FileStructureVersion;
      if not CDFSPointerToData(ADescriptor.ApplicationData,PrimaryDescriptor.ApplicationData[0],512,False) then Exit;
      
      {Check Root}
      if ADescriptor.Root <> nil then
       begin
        {Update Root}
        ADescriptor.Root.EntryCluster:=ADescriptor.StartCluster;
        
        {Set Root}
        if not SetRoot(ADescriptor,ADescriptor.Root) then Exit;
       end;
      
      {Write Clusters}
      if not WriteClusters(ADescriptor.StartCluster,1,FClusterBuffer^) then Exit;
      
      Result:=True;
     end;
    cdfsVolumeDescriptorTypeSupplementary:begin
      {Get Descriptor}
      SupplementaryDescriptor:=PCDFSVolumeDescriptorSupplementary(FClusterBuffer);
      
      {Set Descriptor}
      SupplementaryDescriptor.VolumeFlags:=ADescriptor.VolumeFlags;
      if not CDFSStringToIdentifier(ADescriptor.SystemIdentifier + StringOfChar(' ',16 - Length(ADescriptor.SystemIdentifier)),SupplementaryDescriptor.SystemIdentifier[0],32,True) then Exit;
      if not CDFSStringToIdentifier(ADescriptor.VolumeIdentifier + StringOfChar(' ',16 - Length(ADescriptor.VolumeIdentifier)),SupplementaryDescriptor.VolumeIdentifier[0],32,True) then Exit;
      SupplementaryDescriptor.VolumeSpaceSize:=ADescriptor.VolumeSpaceSize;
      SupplementaryDescriptor.VolumeSpaceSizeM:=LongWordNtoBE(ADescriptor.VolumeSpaceSize);
      if not CDFSPointerToData(ADescriptor.EscapeSequences,SupplementaryDescriptor.EscapeSequences[0],32,False) then Exit;
      SupplementaryDescriptor.VolumeSetSize:=ADescriptor.VolumeSetSize;
      SupplementaryDescriptor.VolumeSetSizeM:=WordNToBE(ADescriptor.VolumeSetSize);
      SupplementaryDescriptor.VolumeSequenceNumber:=ADescriptor.VolumeSequenceNumber;
      SupplementaryDescriptor.VolumeSequenceNumberM:=WordNToBE(ADescriptor.VolumeSequenceNumber);
      SupplementaryDescriptor.LogicalBlockSize:=ADescriptor.LogicalBlockSize;
      SupplementaryDescriptor.LogicalBlockSizeM:=WordNToBE(ADescriptor.LogicalBlockSize);
      SupplementaryDescriptor.PathTableSize:=ADescriptor.PathTableSize;
      SupplementaryDescriptor.PathTableSizeM:=LongWordNtoBE(ADescriptor.PathTableSize);
      SupplementaryDescriptor.PrimaryPathTable:=ADescriptor.PrimaryPathTable;
      SupplementaryDescriptor.AlternatePathTable:=ADescriptor.AlternatePathTable;
      SupplementaryDescriptor.PrimaryPathTableM:=LongWordNtoBE(ADescriptor.PrimaryPathTableM);
      SupplementaryDescriptor.AlternatePathTableM:=LongWordNtoBE(ADescriptor.AlternatePathTableM);
      if not CDFSStringToIdentifier(ADescriptor.VolumeSetIdentifier + StringOfChar(' ',64 - Length(ADescriptor.VolumeSetIdentifier)),SupplementaryDescriptor.VolumeSetIdentifier[0],128,True) then Exit;
      if not CDFSStringToIdentifier(ADescriptor.PublisherIdentifier + StringOfChar(' ',64 - Length(ADescriptor.PublisherIdentifier)),SupplementaryDescriptor.PublisherIdentifier[0],128,True) then Exit;
      if not CDFSStringToIdentifier(ADescriptor.PreparerIdentifier + StringOfChar(' ',64 - Length(ADescriptor.PreparerIdentifier)),SupplementaryDescriptor.PreparerIdentifier[0],128,True) then Exit;
      if not CDFSStringToIdentifier(ADescriptor.ApplicationIdentifier + StringOfChar(' ',64 - Length(ADescriptor.ApplicationIdentifier)),SupplementaryDescriptor.ApplicationIdentifier[0],128,True) then Exit;
      if not CDFSStringToIdentifier(ADescriptor.CopyrightIdentifier + StringOfChar(' ',18 - Length(ADescriptor.CopyrightIdentifier)),SupplementaryDescriptor.CopyrightIdentifier[0],37,True) then Exit;
      if not CDFSStringToIdentifier(ADescriptor.AbstractIdentifier + StringOfChar(' ',18 - Length(ADescriptor.AbstractIdentifier)),SupplementaryDescriptor.AbstractIdentifier[0],37,True) then Exit;
      if not CDFSStringToIdentifier(ADescriptor.BibliographicIdentifier + StringOfChar(' ',18 - Length(ADescriptor.BibliographicIdentifier)),SupplementaryDescriptor.BibliographicIdentifier[0],37,True) then Exit;
      if not FileTimeToCDFSDateTime(ADescriptor.CreateTime,SupplementaryDescriptor.CreateTime) then Exit;
      if not FileTimeToCDFSDateTime(ADescriptor.ModifyTime,SupplementaryDescriptor.ModifyTime) then Exit;
      if not FileTimeToCDFSDateTime(ADescriptor.ExpireTime,SupplementaryDescriptor.ExpireTime) then Exit;
      if not FileTimeToCDFSDateTime(ADescriptor.EffectiveTime,SupplementaryDescriptor.EffectiveTime) then Exit;
      SupplementaryDescriptor.FileStructureVersion:=ADescriptor.FileStructureVersion;
      if not CDFSPointerToData(ADescriptor.ApplicationData,SupplementaryDescriptor.ApplicationData[0],512,False) then Exit;
      
      {Check Root}
      if ADescriptor.Root <> nil then
       begin
        {Update Root}
        ADescriptor.Root.EntryCluster:=ADescriptor.StartCluster;
        
        {Set Root}
        if not SetRoot(ADescriptor,ADescriptor.Root) then Exit;
       end;
       
      {Write Clusters}
      if not WriteClusters(ADescriptor.StartCluster,1,FClusterBuffer^) then Exit;
      
      Result:=True;
     end;
    cdfsVolumeDescriptorTypePartition:begin
      {Get Descriptor}
      PartitionDescriptor:=PCDFSVolumeDescriptorPartition(FClusterBuffer);
      
      {Set Descriptor}
      if not CDFSStringToIdentifier(ADescriptor.SystemIdentifier + StringOfChar(' ',32 - Length(ADescriptor.SystemIdentifier)),PartitionDescriptor.SystemIdentifier[0],32,False) then Exit;
      if not CDFSStringToIdentifier(ADescriptor.PartitionIdentifier + StringOfChar(' ',32 - Length(ADescriptor.PartitionIdentifier)),PartitionDescriptor.PartitionIdentifier[0],32,False) then Exit;
      PartitionDescriptor.PartitionStart:=ADescriptor.PartitionStart;
      PartitionDescriptor.PartitionStartM:=LongWordNtoBE(ADescriptor.PartitionStart);
      PartitionDescriptor.PartitionSize:=ADescriptor.PartitionSize;
      PartitionDescriptor.PartitionSizeM:=LongWordNtoBE(ADescriptor.PartitionSize);
      if not CDFSPointerToData(ADescriptor.SystemData,PartitionDescriptor.SystemData[0],1960,False) then Exit;
      
      {Write Clusters}
      if not WriteClusters(ADescriptor.StartCluster,1,FClusterBuffer^) then Exit;
      
      Result:=True;
     end;
    cdfsVolumeDescriptorTypeTerminator:begin
      {Get Descriptor}
      TerminatorDescriptor:=PCDFSVolumeDescriptorTerminator(FClusterBuffer);
      
      {Write Clusters}
      if not WriteClusters(ADescriptor.StartCluster,1,FClusterBuffer^) then Exit;
      
      Result:=True;
     end;
   end;
  finally
   FBlockWrite:=False;
  end;
 finally
  FDescriptors.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.SetRoot(ADescriptor:TCDFSDiskDescriptor;ARoot:TDiskEntry):Boolean;
{Note: Caller must hold the descriptors lock}
var
 Directory:PCDFSDirectoryRecord;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ADescriptor = nil then Exit;
  if ARoot = nil then Exit;
  if FClusterBuffer = nil then Exit; //To Do //Critical //Called by SetDescriptor which is also using FClusterBuffer

  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.SetRoot - Type = ' + IntToStr(ADescriptor.DescriptorType));
  {$ENDIF}
  
  {Check ReadOnly}
  if FReadOnly then Exit;
  
  {Read Clusters} {If not called by SetDescriptor}
  if not FBlockWrite then if not ReadClusters(TCDFSDiskEntry(ARoot).EntryCluster,1,FClusterBuffer^) then Exit;
  
  {Get Root}
  Directory:=PCDFSDirectoryRecord(PtrUInt(FClusterBuffer) + TCDFSDiskEntry(ARoot).EntryOffset);
  
  {Set Root}
  if EntryToRecord(TCDFSDiskEntry(ARoot),Directory,TCDFSDiskEntry(ARoot).Unicode) then
   begin
    {Write Clusters} {If not called by SetDescriptor}
    if not FBlockWrite then if not WriteClusters(TCDFSDiskEntry(ARoot).EntryCluster,1,FClusterBuffer^) then Exit;
    
    Result:=True;
   end;
 finally
  FEntries.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.SetPath(ATable:TDiskTable;APath:TCDFSDiskPath;ABuffer:Pointer):Boolean;
{Note: SetPath does not use BlockWrite as Buffer is passed by caller}
{Note: Caller must hold the tables lock}
var
 Path:PCDFSPathRecord;
begin
 {}
 Result:=False;
 
 if FDriver = nil then Exit;
 if ATable = nil then Exit;
 if APath = nil then Exit;
 if ABuffer = nil then Exit;

 if not TCDFSDiskTable(ATable).Paths.WriterLock then Exit;
 try
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.SetPath - TableNo = ' + IntToHex(ATable.TableNo,8) + ' Name = ' + APath.Name);
  {$ENDIF}
  
  {Check ReadOnly}
  if FReadOnly then Exit;
  
  {Get Path}
  Path:=PCDFSPathRecord(PtrUInt(ABuffer) + APath.PathOffset);
  
  {Set Path}
  if PathToRecord(APath,Path,TCDFSDiskTable(ATable).Unicode,TCDFSDiskTable(ATable).Endian) then
   begin
    Result:=True;
   end;
 finally
  TCDFSDiskTable(ATable).Paths.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.SetEntry(AParent,AEntry:TDiskEntry):Boolean;
var
 Directory:PCDFSDirectoryRecord;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if AParent = nil then Exit;
  if AEntry = nil then Exit;
  if FClusterBuffer = nil then Exit;

  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.SetEntry - Name = ' + AEntry.Name);
  {$ENDIF}
  
  {Check ReadOnly}
  if FReadOnly then Exit;
  
  {Read Clusters} {If not called by SetEntries}
  if not FBlockWrite then if not ReadClusters(TCDFSDiskEntry(AEntry).EntryCluster,1,FClusterBuffer^) then Exit;
  
  {Get Entry}
  Directory:=PCDFSDirectoryRecord(PtrUInt(FClusterBuffer) + TCDFSDiskEntry(AEntry).EntryOffset);
  
  {Set Entry}
  if EntryToRecord(TCDFSDiskEntry(AEntry),Directory,TCDFSDiskEntry(AEntry).Unicode) then
   begin
    {Write Clusters} {If not called by SetEntries}
    if not FBlockWrite then if not WriteClusters(TCDFSDiskEntry(AEntry).EntryCluster,1,FClusterBuffer^) then Exit;
    
    Result:=True;
   end;
 finally
  FEntries.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.SetHeader(AHeader:TCDFSDiskHeader):Boolean;
var
 SectionHeader:PELTORITOSectionHeader;
 ValidationRecord:PELTORITOValidationRecord;
begin
 {}
 Result:=False;
 
 if not FHeaders.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if AHeader = nil then Exit;
  if FClusterBuffer = nil then Exit;

  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.SetHeader');
  {$ENDIF}
  
  {Check ReadOnly}
  if FReadOnly then Exit;
  
  {Read Clusters} {If not called by SetCatalogs}
  if not FBlockWrite then if not ReadClusters(AHeader.HeaderCluster,1,FClusterBuffer^) then Exit;
  
  {Check Header}
  if (AHeader.HeaderIndicator = cdfsElToritoHeaderIndicator) or (AHeader.HeaderIndicator = cdfsElToritoHeaderTerminator) then
   begin
    {Get Header}
    SectionHeader:=PELTORITOSectionHeader(PtrUInt(FClusterBuffer) + AHeader.HeaderOffset);
    
    {Set Header}
    SectionHeader.HeaderIndicator:=AHeader.HeaderIndicator;
    SectionHeader.PlatformId:=AHeader.PlatformId;
    SectionHeader.SectionCount:=AHeader.SectionCount;
    if not CDFSStringToIdentifier(AHeader.SectionId + StringOfChar(#0,28 - Length(AHeader.SectionId)),SectionHeader.SectionId[0],28,False) then Exit;
    
    {Write Clusters} {If not called by SetCatalogs}
    if not FBlockWrite then if not WriteClusters(AHeader.HeaderCluster,1,FClusterBuffer^) then Exit;
    
    Result:=True;
   end
  else
   begin
    {Check Validation}
    if AHeader.HeaderId = cdfsElToritoHeaderId then
     begin
      {Get Validation}
      ValidationRecord:=PELTORITOValidationRecord(PtrUInt(FClusterBuffer) + AHeader.HeaderOffset);
      
      {Set Validation}
      ValidationRecord.HeaderId:=AHeader.HeaderId;
      ValidationRecord.PlatformId:=AHeader.PlatformId;
      if not CDFSStringToIdentifier(AHeader.VendorId + StringOfChar(#0,24 - Length(AHeader.VendorId)),ValidationRecord.VendorId[0],24,False) then Exit;
      ValidationRecord.Checksum:=0;
      ValidationRecord.Signature:=AHeader.Signature;
      
      {Update Checksum}
      ValidationRecord.Checksum:=ChecksumValidationRecord(ValidationRecord);
      AHeader.Checksum:=ValidationRecord.Checksum;
      
      {Write Clusters} {If not called by SetCatalogs}
      if not FBlockWrite then if not WriteClusters(AHeader.HeaderCluster,1,FClusterBuffer^) then Exit;
      
      Result:=True;
     end;
   end;
 finally
  FHeaders.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.SetCatalog(ACatalog:TDiskCatalog):Boolean;
var
 DefaultRecord:PELTORITODefaultRecord;
 SectionRecord:PELTORITOSectionRecord;
begin
 {}
 Result:=False;
 
 if not FCatalogs.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ACatalog = nil then Exit;
  if FClusterBuffer = nil then Exit;

  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.SetCatalog - Name = ' + ACatalog.Name);
  {$ENDIF}
  
  {Check ReadOnly}
  if FReadOnly then Exit;
  
  {Read Clusters} {If not called by SetCatalogs}
  if not FBlockWrite then if not ReadClusters(TCDFSDiskCatalog(ACatalog).CatalogCluster,1,FClusterBuffer^) then Exit;
  
  {Check Initial}
  if TCDFSDiskCatalog(ACatalog).Initial then
   begin
    {Get Initial}
    DefaultRecord:=PELTORITODefaultRecord(PtrUInt(FClusterBuffer) + TCDFSDiskCatalog(ACatalog).CatalogOffset);
    
    {Set Initial}
    DefaultRecord.BootIndicator:=TCDFSDiskCatalog(ACatalog).BootIndicator;
    DefaultRecord.BootMedia:=TCDFSDiskCatalog(ACatalog).BootMedia;
    DefaultRecord.LoadSegment:=TCDFSDiskCatalog(ACatalog).LoadSegment;
    DefaultRecord.SystemType:=TCDFSDiskCatalog(ACatalog).SystemType;
    DefaultRecord.LoadCount:=TCDFSDiskCatalog(ACatalog).LoadCount;
    DefaultRecord.LoadRBA:=TCDFSDiskCatalog(ACatalog).StartCluster;
    
    {Write Clusters} {If not called by SetCatalogs}
    if not FBlockWrite then if not WriteClusters(TCDFSDiskCatalog(ACatalog).CatalogCluster,1,FClusterBuffer^) then Exit;
    
    Result:=True;
   end
  else
   begin
    {Get Catalog}
    SectionRecord:=PELTORITOSectionRecord(PtrUInt(FClusterBuffer) + TCDFSDiskCatalog(ACatalog).CatalogOffset);
    
    {Set Catalog}
    SectionRecord.BootIndicator:=TCDFSDiskCatalog(ACatalog).BootIndicator;
    SectionRecord.BootMedia:=TCDFSDiskCatalog(ACatalog).BootMedia;
    SectionRecord.LoadSegment:=TCDFSDiskCatalog(ACatalog).LoadSegment;
    SectionRecord.SystemType:=TCDFSDiskCatalog(ACatalog).SystemType;
    SectionRecord.LoadCount:=TCDFSDiskCatalog(ACatalog).LoadCount;
    SectionRecord.SelectionType:=TCDFSDiskCatalog(ACatalog).SelectionType;
    if not CDFSPointerToData(TCDFSDiskCatalog(ACatalog).SelectionData,SectionRecord.SelectionData[0],19,False) then Exit;
    SectionRecord.LoadRBA:=TCDFSDiskCatalog(ACatalog).StartCluster;
    
    {Write Clusters} {If not called by SetCatalogs}
    if not FBlockWrite then if not WriteClusters(TCDFSDiskCatalog(ACatalog).CatalogCluster,1,FClusterBuffer^) then Exit;
    
    Result:=True;
   end;
 finally
  FCatalogs.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.SetExtension(AExtension:TCDFSDiskExtension):Boolean;
var
 SectionExtension:PELTORITOSectionExtension;
begin
 {}
 Result:=False;
 
 if not FExtensions.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if AExtension = nil then Exit;
  if FClusterBuffer = nil then Exit;

  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.SetExtension');
  {$ENDIF}
  
  {Check ReadOnly}
  if FReadOnly then Exit;
  
  {Read Clusters} {If not called by SetCatalogs}
  if not FBlockWrite then if not ReadClusters(AExtension.ExtensionCluster,1,FClusterBuffer^) then Exit;
  
  {Get Extension}
  SectionExtension:=PELTORITOSectionExtension(PtrUInt(FClusterBuffer) + AExtension.ExtensionOffset);
  
  {Set Extension}
  SectionExtension.ExtensionIndicator:=AExtension.ExtensionIndicator;
  SectionExtension.ExtensionFlag:=AExtension.ExtensionFlag;
  if not CDFSPointerToData(AExtension.SelectionData,SectionExtension.SelectionData[0],30,False) then Exit;
  
  {Write Clusters} {If not called by SetCatalogs}
  if not FBlockWrite then if not WriteClusters(AExtension.ExtensionCluster,1,FClusterBuffer^) then Exit;
  
  Result:=True;
 finally
  FExtensions.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.SetExtended(AEntry:TDiskEntry;AExtended:TCDFSDiskExtended):Boolean;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if AEntry = nil then Exit;
  if AExtended = nil then Exit;
  if FClusterBuffer = nil then Exit;

  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.SetExtended');
  {$ENDIF}
  
  {Check ReadOnly}
  if FReadOnly then Exit;

  //To Do //To Do
 finally
  FEntries.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.SizeEntry(AParent,AEntry:TDiskEntry;const ASize:Int64):Boolean;
var
 Count:LongWord;
 Remain:LongWord;
 Cluster:LongWord;
 Current:LongWord;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if AParent = nil then Exit;
  if AEntry = nil then Exit;
  if AEntry = FRoot then Exit; {Cannot size Root}

  //To Do //Account for AEntry.ExtendedRecordSize

  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.SizeEntry EntryName = ' + AEntry.Name + ' Size = ' + IntToStr(ASize));
  {$ENDIF}
  
  {Check ReadOnly}
  if FReadOnly then Exit;
  
  {Prepare Trees}
  if not PrepareTrees then Exit;
  
  {Check Relative}
  if (AEntry.Attributes and (faDot or faDotDot)) <> faNone then Exit;
  
  {Check Entry}
  if (AEntry.Attributes and faMatchMask) <> faFile then Exit;
  
  {Check Size}
  if ASize = 0 then
   begin
    {Zero}
    {Check Count}
    if TCDFSDiskEntry(AEntry).ClusterCount > 1 then
     begin
      {Release Clusters (All except First)}
      if not ReleaseClusters(TCDFSDiskEntry(AEntry).StartCluster + 1,TCDFSDiskEntry(AEntry).ClusterCount - 1) then Exit;
     end;
    
    {Check Count}
    if TCDFSDiskEntry(AEntry).ClusterCount > 0 then
     begin
      {Zero Clusters (All)}
      if not FillClusters(TCDFSDiskEntry(AEntry).StartCluster,TCDFSDiskEntry(AEntry).ClusterCount,0) then Exit;
     end;
    
    {Update Entry}
    TCDFSDiskEntry(AEntry).ClusterCount:=1;
    TCDFSDiskEntry(AEntry).DataSize:=ASize;
    TCDFSDiskEntry(AEntry).Size:=ASize;

    {Set Entry}
    if not SetEntry(AParent,AEntry) then Exit;
   end
  else if ASize < AEntry.Size then
   begin
    {Smaller}
    {Get Count}
    Count:=(ASize shr FClusterShiftCount);
    if (Count shl FClusterShiftCount) < ASize then Inc(Count);

    {$IFDEF CDFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.SizeEntry Count = ' + IntToStr(Count) + ' ClusterCount = ' + IntToStr(TCDFSDiskEntry(AEntry).ClusterCount));
    {$ENDIF}

    {Check Count}
    if Count < TCDFSDiskEntry(AEntry).ClusterCount then
     begin
      if (TCDFSDiskEntry(AEntry).ClusterCount - Count) > 0 then
       begin
        {Release Clusters}
        if not ReleaseClusters(TCDFSDiskEntry(AEntry).StartCluster + Count,TCDFSDiskEntry(AEntry).ClusterCount - Count) then Exit;
        
        {Zero Clusters}
        if not FillClusters(TCDFSDiskEntry(AEntry).StartCluster + Count,TCDFSDiskEntry(AEntry).ClusterCount - Count,0) then Exit;
       end;
     end;
     
    {Update Entry}
    TCDFSDiskEntry(AEntry).ClusterCount:=Count;
    TCDFSDiskEntry(AEntry).DataSize:=ASize;
    TCDFSDiskEntry(AEntry).Size:=ASize;
    
    {Set Entry}
    if not SetEntry(AParent,AEntry) then Exit;
   end
  else if ASize > AEntry.Size then
   begin
    {Larger}
    {Get Count}
    Count:=(ASize shr FClusterShiftCount);
    if (Count shl FClusterShiftCount) < ASize then Inc(Count);
    
    {$IFDEF CDFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.SizeEntry Count = ' + IntToStr(Count) + ' ClusterCount = ' + IntToStr(TCDFSDiskEntry(AEntry).ClusterCount));
    {$ENDIF}
    
    {Check Count}
    if Count > TCDFSDiskEntry(AEntry).ClusterCount then
     begin
      {Alloc Clusters (Next Contiguous)}
      Cluster:=TCDFSDiskEntry(AEntry).StartCluster + TCDFSDiskEntry(AEntry).ClusterCount;
      if not AllocClusters(Cluster,Count - TCDFSDiskEntry(AEntry).ClusterCount) then
       begin
        {Alloc Clusters (Contiguous Block)}
        Cluster:=cdfsUnknownCluster;
        if not AllocClusters(Cluster,Count) then Exit;
        
        {Move Clusters}
        if TCDFSDiskEntry(AEntry).ClusterCount > 0 then
         begin
          if FClusterBuffer = nil then Exit;
          Remain:=TCDFSDiskEntry(AEntry).ClusterCount;
          Current:=0;
          while Remain > 0 do
           begin
            if not ReadClusters(TCDFSDiskEntry(AEntry).StartCluster + Current,1,FClusterBuffer^) then Exit;
            if not WriteClusters(Cluster + Current,1,FClusterBuffer^) then Exit;
            Dec(Remain);
            Inc(Current);
           end;
          
          {Release Clusters}
          if not ReleaseClusters(TCDFSDiskEntry(AEntry).StartCluster,TCDFSDiskEntry(AEntry).ClusterCount) then Exit;
         end;
         
        {Update Entry}
        TCDFSDiskEntry(AEntry).StartCluster:=Cluster;
       end;
     end;
     
    {Update Entry}
    TCDFSDiskEntry(AEntry).ClusterCount:=Count;
    TCDFSDiskEntry(AEntry).DataSize:=ASize;
    TCDFSDiskEntry(AEntry).Size:=ASize;
    
    {Set Entry}
    if not SetEntry(AParent,AEntry) then Exit;
   end;
  
  Result:=True; {Note: If Size is same then just succeed}
 finally
  FEntries.WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.GetBlock(ABlockNo:LongWord):TDiskBlock;
{Overidden to implement multiple entry blocks}
begin
 {}
 Result:=nil;
 
 if FDriver = nil then Exit;
 if ABlockNo >= FTotalBlockCount then Exit;

 {Call Inherited Method}
 Result:=inherited GetBlock(ABlockNo);
end;

{=============================================================================}

function TCDFSFileSystem.GetBlockEx(ABlockNo:LongWord;AWrite:Boolean):TDiskBlock;
{Overidden to implement multiple entry blocks}
begin
 {}
 Result:=nil;
 
 if FDriver = nil then Exit;
 if ABlockNo >= FTotalBlockCount then Exit;

 {Call Inherited Method}
 Result:=inherited GetBlockEx(ABlockNo,AWrite);
end;

{=============================================================================}

function TCDFSFileSystem.GetDescriptor(AType,AInstance:Byte):TCDFSDiskDescriptor;
var
 Count:Byte;
 Previous:TCDFSDiskDescriptor;
 Descriptor:TCDFSDiskDescriptor;
begin
 {}
 Result:=nil;
 
 if not FDescriptors.ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
 
  Count:=0;
  Previous:=nil;
 
  {Check Descriptors}
  Descriptor:=TCDFSDiskDescriptor(FDescriptors.First);
  while Descriptor <> nil do
   begin
    {Check Type}
    if Descriptor.DescriptorType = AType then
     begin
      Inc(Count);
      if AInstance = cdfsInstanceLast then
       begin
        Previous:=Descriptor;
       end
      else
       begin
        if (AInstance = cdfsInstanceFirst) or (Count = AInstance) then  {Instance 0 equals first}
         begin
          Result:=Descriptor;
          Exit;
         end;
       end;
     end;
    
    Descriptor:=TCDFSDiskDescriptor(Descriptor.Next);
   end;
  
  {Get Last}
  if AInstance = cdfsInstanceLast then Result:=Previous;
 finally
  FDescriptors.ReaderUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.GetDescriptorEx(AType,AInstance:Byte;AWrite:Boolean):TCDFSDiskDescriptor; 
var
 Count:Byte;
 Previous:TCDFSDiskDescriptor;
 Descriptor:TCDFSDiskDescriptor;
begin
 {}
 Result:=nil;
 
 if AWrite then
  begin
   if not FDescriptors.WriterLock then Exit;
  end
 else
  begin   
   if not FDescriptors.ReaderLock then Exit;
  end; 
 try
  if FDriver = nil then Exit;
 
  Count:=0;
  Previous:=nil;
 
  {Check Descriptors}
  Descriptor:=TCDFSDiskDescriptor(FDescriptors.First);
  while Descriptor <> nil do
   begin
    {Check Type}
    if Descriptor.DescriptorType = AType then
     begin
      Inc(Count);
      if AInstance = cdfsInstanceLast then
       begin
        Previous:=Descriptor;
       end
      else
       begin
        if (AInstance = cdfsInstanceFirst) or (Count = AInstance) then  {Instance 0 equals first}
         begin
          Result:=Descriptor;
          Exit;
         end;
       end;
     end;
    
    Descriptor:=TCDFSDiskDescriptor(Descriptor.Next);
   end;
  
  {Get Last}
  if AInstance = cdfsInstanceLast then Result:=Previous;
 finally
  if AWrite then
   begin
    FDescriptors.WriterUnlock;
   end
  else
   begin
    FDescriptors.ReaderUnlock;
   end;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.GetRoot(ADescriptor:TCDFSDiskDescriptor):TDiskEntry;
{Note: Caller must hold the descriptors lock}
begin
 {}
 Result:=nil;
 
 if FDriver = nil then Exit;
 if ADescriptor = nil then Exit;

 {Check Root}
 if ADescriptor.Root = nil then
  begin
   {Load Root}
   if not LoadRoot(ADescriptor) then Exit;
  end;
  
 {Get Root}
 Result:=ADescriptor.Root;
end;

{=============================================================================}

function TCDFSFileSystem.GetPath(ATable:TDiskTable;AParentNumber:Word;const AName:String):TCDFSDiskPath;
{Note: Caller must hold the tables lock}
var
 Hash:LongWord; 
 Next:TCDFSDiskPath;
begin
 {}
 Result:=nil;
 
 if FDriver = nil then Exit;
 if ATable = nil then Exit;
 
 {Load Paths}
 if LoadPaths(TCDFSDiskTable(ATable)) then
  begin
   if not TCDFSDiskTable(ATable).Paths.ReaderLock then Exit;
   try
    Hash:=GenerateNameHash(AName,NAME_HASH_SIZE); 
   
    {Check Paths}
    Next:=TCDFSDiskPath(TCDFSDiskTable(ATable).Paths.First);
    while Next <> nil do
     begin
      if Next.ParentNumber = AParentNumber then
       begin
        if Next.Hash = Hash then
         begin
          if Uppercase(Next.Name) = Uppercase(AName) then
           begin
            Result:=Next;
            Exit;
           end;
         end;
       end;
       
      Next:=TCDFSDiskPath(Next.Next);
     end;
   finally
    TCDFSDiskTable(ATable).Paths.ReaderUnlock;
   end; 
  end;
end;

{=============================================================================}

function TCDFSFileSystem.GetHeader(AHeaderNo:LongWord):TCDFSDiskHeader;
{Note: HeaderNo is one based}
var
 Next:TCDFSDiskHeader;
begin
 {}
 Result:=nil;
 
 if FDriver = nil then Exit;
 if AHeaderNo = 0 then Exit;
 
 {Load Catalogs}
 if LoadCatalogs then
  begin
   if not FHeaders.ReaderLock then Exit;
   try
    {Check Headers}
    Next:=TCDFSDiskHeader(FHeaders.First);
    while Next <> nil do
     begin
      if Next.HeaderNo = AHeaderNo then
       begin
        Result:=Next;
        Exit;
       end;
      
      Next:=TCDFSDiskHeader(Next.Next);
     end;
   finally
    FHeaders.ReaderUnlock;
   end; 
  end;
end;

{=============================================================================}

function TCDFSFileSystem.GetHeaderEx(AHeaderNo:LongWord;AWrite:Boolean):TCDFSDiskHeader;
{Note: HeaderNo is one based}
var
 Next:TCDFSDiskHeader;
begin
 {}
 Result:=nil;
 
 if FDriver = nil then Exit;
 if AHeaderNo = 0 then Exit;
 
 {Load Catalogs}
 if LoadCatalogs then
  begin
   if AWrite then
    begin
     if not FHeaders.WriterLock then Exit;
    end
   else
    begin   
     if not FHeaders.ReaderLock then Exit;
    end; 
   try
    {Check Headers}
    Next:=TCDFSDiskHeader(FHeaders.First);
    while Next <> nil do
     begin
      if Next.HeaderNo = AHeaderNo then
       begin
        Result:=Next;
        Exit;
       end;
      
      Next:=TCDFSDiskHeader(Next.Next);
     end;
   finally
    if AWrite then
     begin
      FHeaders.WriterUnlock;
     end
    else
     begin
      FHeaders.ReaderUnlock;
     end;
   end; 
  end;
end;

{=============================================================================}

function TCDFSFileSystem.GetExtension(AExtensionNo:LongWord):TCDFSDiskExtension;
{Note: ExtensionNo is one based}
var
 Next:TCDFSDiskExtension;
begin
 {}
 Result:=nil;
 if FDriver = nil then Exit;
 if AExtensionNo = 0 then Exit;
 
 {Load Catalogs}
 if LoadCatalogs then
  begin
   if not FExtensions.ReaderLock then Exit;
   try
    {Check Extensions}
    Next:=TCDFSDiskExtension(FExtensions.First);
    while Next <> nil do
     begin
      if Next.ExtensionNo = AExtensionNo then
       begin
        Result:=Next;
        Exit;
       end;
      
      Next:=TCDFSDiskExtension(Next.Next);
     end;
   finally
    FExtensions.ReaderUnlock;
   end; 
  end;
end;

{=============================================================================}

function TCDFSFileSystem.GetExtensionEx(AExtensionNo:LongWord;AWrite:Boolean):TCDFSDiskExtension;
{Note: ExtensionNo is one based}
var
 Next:TCDFSDiskExtension;
begin
 {}
 Result:=nil;
 if FDriver = nil then Exit;
 if AExtensionNo = 0 then Exit;
 
 {Load Catalogs}
 if LoadCatalogs then
  begin
   if AWrite then
    begin
     if not FExtensions.WriterLock then Exit;
    end
   else
    begin   
     if not FExtensions.ReaderLock then Exit;
    end; 
   try
    {Check Extensions}
    Next:=TCDFSDiskExtension(FExtensions.First);
    while Next <> nil do
     begin
      if Next.ExtensionNo = AExtensionNo then
       begin
        Result:=Next;
        Exit;
       end;
      
      Next:=TCDFSDiskExtension(Next.Next);
     end;
   finally
    if AWrite then
     begin
      FExtensions.WriterUnlock;
     end
    else
     begin
      FExtensions.ReaderUnlock;
     end;
   end; 
  end;
end;

{=============================================================================}

function TCDFSFileSystem.GetExtended(AEntry:TDiskEntry):TCDFSDiskExtended;
begin
 {}
 Result:=nil;
 
 if FDriver = nil then Exit;
 if AEntry = nil then Exit;

 {Check Extended}
 if TCDFSDiskEntry(AEntry).Extended = nil then
  begin
   {Load Extended}
   if not LoadExtended(AEntry) then Exit;
  end;
 
 {Get Extended}
 Result:=TCDFSDiskEntry(AEntry).Extended;
end;

{=============================================================================}

function TCDFSFileSystem.GetVersion(const AName:String):String;
var
 Marker:PChar;
begin
 {}
 Result:=cdfsBlankName;
 
 {Check Length}
 if Length(Result) < 2 then Exit;
 
 {Check Separator 2}
 Marker:=StrScan(PChar(AName),cdfsSeparator2Name);
 if Marker = nil then Exit;
 
 Result:=Copy(AName,(PtrUInt(Marker) - PtrUInt(AName)) + 2,Length(AName));
end;

{=============================================================================}

function TCDFSFileSystem.StripDot(const AName:String):String;
begin
 {}
 Result:=AName;
 
 {Check Length}
 if Length(Result) < 2 then Exit;
 
 {Check Separator 1}
 if (Result[Length(Result)] = cdfsSeparator1Name) and (Result[1] <> cdfsSeparator1Name) then
  begin
   Delete(Result,Length(Result),1);
  end;
end;

{=============================================================================}

function TCDFSFileSystem.StripVersion(const AName:String):String;
var
 Marker:PChar;
begin
 {}
 Result:=AName;
 
 {Check Length}
 if Length(Result) < 2 then Exit;
 
 {Check Separator 2}
 Marker:=StrScan(PChar(AName),cdfsSeparator2Name);
 if Marker = nil then Exit;
 Result:=Copy(Result,1,PtrUInt(Marker) - PtrUInt(AName));

 {Check Separator 2} {Modified to handle version greater than 1}
 {if (Result[Length(Result) - 1] = cdfsSeparator2Name) and (Result[Length(Result)] = cdfsVersionName) then
  begin
   Delete(Result,Length(Result) - 1,2);
  end;}
end;

{=============================================================================}

function TCDFSFileSystem.CompareName(const AName,AMatch:String;AWildcard:Boolean):Boolean;
begin
 {}
 Result:=False;

 //To Do 
end;

{=============================================================================}

function TCDFSFileSystem.CompareSequence(const ASequence;const ACompare;ASize:Integer):Boolean;
begin
 {}
 Result:=CompareMem(@ASequence,@ACompare,ASize);
end;

{=============================================================================}

function TCDFSFileSystem.CompareIdentifier(const AIdentifier;const ACompare;ASize:Integer):Boolean;
begin
 {}
 Result:=CompareMem(@AIdentifier,@ACompare,ASize);
end;

{=============================================================================}

function TCDFSFileSystem.CalculateVolumeSerial(ABuffer:Pointer;ASize:Integer):LongWord;
var
 Byte1:Byte;
 Byte2:Byte;
 Byte3:Byte;
 Byte4:Byte;
 Count:Integer;
begin
 {}
 Result:=0;
 if ABuffer = nil then Exit;
 
 {Set Start}
 Byte1:=0;
 Byte2:=0;
 Byte3:=0;
 Byte4:=0;
 Count:=0;
 while (Count + 3) < ASize do
  begin
   {Sum all Bytes}
   Byte1:=Byte1 + Byte(Pointer(PtrUInt(ABuffer) + LongWord(Count))^);
   Byte2:=Byte2 + Byte(Pointer(PtrUInt(ABuffer) + LongWord(Count + 1))^);
   Byte3:=Byte3 + Byte(Pointer(PtrUInt(ABuffer) + LongWord(Count + 2))^);
   Byte4:=Byte4 + Byte(Pointer(PtrUInt(ABuffer) + LongWord(Count + 3))^);
   Inc(Count,4); {SizeOf(LongWord)}
  end;
  
 {Calculate Result}
 Result:=(Byte1 shl 24) + (Byte2 shl 16) + (Byte3 shl 8) + Byte4;
 
 {Check for Swap}
 if FSwapSerial then Result:=SwapEndian(Result);
end;

{=============================================================================}

function TCDFSFileSystem.FileSystemInit:Boolean;
begin
 {}
 Result:=False;

 if not WriterLock then Exit;
 try
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.FileSystemInit');
  {$ENDIF}

  if FDriver = nil then Exit;

  {Clear Current}
  FCDFSType:=ctNONE;
  FVolumeFlags:=cdfsVolumeFlagNone;
  FTreesLoaded:=False;
  FBlocksLoaded:=False;
  FCatalogsLoaded:=False;
  FCatalogsLoading:=False;
  FBlockWrite:=False;
  FTreesPrepared:=False;
  FCatalogsChecked:=False;
  
  FRoot:=nil;
  
  SetCurrent(nil);
  
  FChunks.ClearList;
  FTables.ClearList;
  FBlocks.ClearList;
  FEntries.ClearList;
  FCatalogs.ClearList;
  
  FLastFreeCluster:=cdfsUnknownCluster;
  FFreeClusterCount:=cdfsUnknownCluster;
  
  FPrimary:=nil;
  FSupplementary:=nil;
  
  FDescriptors.ClearList;
  
  if FClusterBuffer <> nil then FreeMem(FClusterBuffer);
  FClusterBuffer:=nil;

  if FWriteBuffer <> nil then FreeMem(FWriteBuffer);
  FWriteBuffer:=nil;

  if FReadBuffer <> nil then FreeMem(FReadBuffer);
  FReadBuffer:=nil;
  
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                Settings Cleared');
  {$ENDIF}
  
  {Setup Defaults}
  FPathChar:=LoadPathChar;
  FNameChar:=LoadNameChar;
  FFileChar:=LoadFileChar;
  FRootChar:=LoadRootChar;
  FRootName:=LoadRootName;
  FRootPath:=LoadRootPath; 
  FMaxFile:=LoadMaxFile;
  FMaxPath:=LoadMaxPath;
  FMaxAltFile:=LoadMaxAltFile;
  FMaxAltPath:=LoadMaxAltPath;
  FAttributes:=LoadAttributes;
  FMaxAttributes:=LoadMaxAttributes;
  FMaskAttributes:=LoadMaskAttributes;
  FMountPointTag:=LoadMountPointTag;
  FSymbolicLinkTag:=LoadSymbolicLinkTag;

  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                Defaults Set');
  {$ENDIF}

  {Check Volume}
  if FVolume = nil then Exit;
  if FVolume.Device = nil then Exit;
  FReadOnly:=not(FVolume.Device.Writeable);
  FBootCatalog:=not(FReadOnly);
  
  {Clear Cache}
  if not FDriver.Cache.ReleaseDevicePages(FVolume.Device) then Exit;
  
  {Load Sector Size, Start and Count}
  FSectorSize:=LoadSectorSize;
  FStartSector:=LoadStartSector;
  FSectorCount:=LoadSectorCount;
  
  {Bind to Volume}
  FVolume.FileSystem:=Self;
  
  {Bind to Drive}
  if FDrive <> nil then FDrive.FileSystem:=Self;

  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                Volume Checked');
  {$ENDIF}
  
  {Get Sector}
  if not ReadSectors(cdfsISO9660StartSector,1,FSectorBuffer^) then Exit;

  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                Volume Descriptor Read');
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                 PCDFSVolumeDescriptorHeader(FSectorBuffer).DescriptorVersion=' + IntToStr(PCDFSVolumeDescriptorHeader(FSectorBuffer).DescriptorVersion));
  {$ENDIF}
  
  {Check for CDFS}
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                Check for CDFS');
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                 PCDFSVolumeDescriptorHeader(FSectorBuffer).DescriptorVersion=' + IntToStr(PCDFSVolumeDescriptorHeader(FSectorBuffer).DescriptorVersion));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                 PCDFSVolumeDescriptorHeader(FSectorBuffer).DescriptorType=' + IntToStr(PCDFSVolumeDescriptorHeader(FSectorBuffer).DescriptorType));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                 PCDFSVolumeDescriptorPrimary(FSectorBuffer).VolumeSpaceSize=' + IntToStr(PCDFSVolumeDescriptorPrimary(FSectorBuffer).VolumeSpaceSize));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                 PCDFSVolumeDescriptorPrimary(FSectorBuffer).LogicalBlockSize=' + IntToStr(PCDFSVolumeDescriptorPrimary(FSectorBuffer).LogicalBlockSize));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                 PCDFSVolumeDescriptorPrimary(FSectorBuffer).PathTableSize=' + IntToStr(PCDFSVolumeDescriptorPrimary(FSectorBuffer).PathTableSize));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                 PCDFSVolumeDescriptorPrimary(FSectorBuffer).RootDirectory.FirstBlock = ' + IntToStr(PCDFSVolumeDescriptorPrimary(FSectorBuffer).RootDirectory.FirstBlock));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                 PCDFSVolumeDescriptorPrimary(FSectorBuffer).RootDirectory.DataSize   = ' + IntToStr(PCDFSVolumeDescriptorPrimary(FSectorBuffer).RootDirectory.DataSize));
  {$ENDIF}
  
  if PCDFSVolumeDescriptorHeader(FSectorBuffer).DescriptorVersion <> cdfsISO9660DescriptorVersion then Exit;
  if PCDFSVolumeDescriptorHeader(FSectorBuffer).DescriptorType <> cdfsVolumeDescriptorTypePrimary then Exit;
  if not CompareMem(@PCDFSVolumeDescriptorHeader(FSectorBuffer).StandardIdentifier[0],@cdfsISO9660StandardIdentifier[1],5) then Exit;
  if PCDFSVolumeDescriptorPrimary(FSectorBuffer).VolumeSpaceSize = 0 then Exit;
  if PCDFSVolumeDescriptorPrimary(FSectorBuffer).VolumeSpaceSize > FSectorCount then Exit;
  if PCDFSVolumeDescriptorPrimary(FSectorBuffer).LogicalBlockSize = 0 then Exit;
  if PCDFSVolumeDescriptorPrimary(FSectorBuffer).LogicalBlockSize mod MIN_SECTOR_SIZE <> 0 then Exit;

  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                CDFS Detected');
  {$ENDIF}
  
  {Calculate Parameters}
  {FSectorSize:=PCDFSVolumeDescriptorPrimary(FSectorBuffer).LogicalBlockSize;} {Not Used}
  {FSectorCount:=PCDFSVolumeDescriptorPrimary(FSectorBuffer).VolumeSpaceSize;} {Not Used}
  FLogicalBlockSize:=PCDFSVolumeDescriptorPrimary(FSectorBuffer).LogicalBlockSize;
  FLogicalBlockCount:=PCDFSVolumeDescriptorPrimary(FSectorBuffer).VolumeSpaceSize;

  FSectorsPerCluster:=GetSectorsPerCluster(FLogicalBlockSize);

  FDataStartCluster:=cdfsISO9660StartSector;
  FBootStartCluster:=cdfsELTORITOBootSector;
  FRootStartCluster:=PCDFSVolumeDescriptorPrimary(FSectorBuffer).RootDirectory.FirstBlock;

  FBlockShiftCount:=GetBlockShiftCount(FLogicalBlockSize);
  FSectorShiftCount:=GetSectorShiftCount(FLogicalBlockSize);
  FClusterShiftCount:=GetClusterShiftCount(FLogicalBlockSize);

  FEntriesPerBlock:=GetEntriesPerBlock(FLogicalBlockSize);
  FClustersPerBlock:=GetClustersPerBlock(FLogicalBlockSize);
  FTotalBlockCount:=GetTotalBlockCount(FLogicalBlockCount);

  FTotalClusterCount:=FLogicalBlockCount;

  FClusterSize:=FLogicalBlockSize;

  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                Parameters Calculated');
  {$ENDIF}
  
  {Create Buffers}
  FReadBuffer:=GetMem(FClusterSize);
  if FReadBuffer = nil then Exit;
  FWriteBuffer:=GetMem(FClusterSize);
  if FWriteBuffer = nil then Exit;
  FClusterBuffer:=GetMem(FClusterSize);
  if FClusterBuffer = nil then Exit;

  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then
   begin
    FileSysLogDebug('TCDFSFileSystem.FileSystemInit');
    FileSysLogDebug(' SectorSize = ' + IntToStr(FSectorSize));
    FileSysLogDebug(' StartSector = ' + IntToStr(FStartSector));
    FileSysLogDebug(' SectorCount = ' + IntToStr(FSectorCount));
    FileSysLogDebug('');
    FileSysLogDebug(' LogicalBlockSize = ' + IntToStr(FLogicalBlockSize));
    FileSysLogDebug(' LogicalBlockCount = ' + IntToStr(FLogicalBlockCount));
    FileSysLogDebug('');
    FileSysLogDebug(' SectorsPerCluster = ' + IntToStr(FSectorsPerCluster));
    FileSysLogDebug('');
    FileSysLogDebug(' DataStartCluster = ' + IntToStr(FDataStartCluster));
    FileSysLogDebug(' BootStartCluster = ' + IntToStr(FBootStartCluster));
    FileSysLogDebug(' RootStartCluster = ' + IntToStr(FRootStartCluster));
    FileSysLogDebug('');
    FileSysLogDebug(' BlockShiftCount = ' + IntToStr(FBlockShiftCount));
    FileSysLogDebug(' SectorShiftCount = ' + IntToStr(FSectorShiftCount));
    FileSysLogDebug(' ClusterShiftCount = ' + IntToStr(FClusterShiftCount));
    FileSysLogDebug('');
    FileSysLogDebug(' EntriesPerBlock = ' + IntToStr(FEntriesPerBlock));
    FileSysLogDebug(' ClustersPerBlock = ' + IntToStr(FClustersPerBlock));
    FileSysLogDebug(' TotalBlockCount = ' + IntToStr(FTotalBlockCount));
    FileSysLogDebug('');
    FileSysLogDebug(' TotalClusterCount = ' + IntToStr(FTotalClusterCount));
    FileSysLogDebug('');
    FileSysLogDebug(' ClusterSize = ' + IntToStr(FClusterSize));
    FileSysLogDebug('');
   end;
  {$ENDIF}

  {Load Descriptors}
  if not LoadDescriptors then Exit;
  
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                Descriptors Loaded');
  {$ENDIF}
  
  {Load Tables}
  LoadTables;
  
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                Tables Loaded');
  {$ENDIF}
  
  {Load Roots}
  LoadRoots;
  
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                Roots Loaded');
  {$ENDIF}
  
  {Set Current}
  SetCurrent(FRoot);
  
  {Setup Values}
  FSystemName:=LoadSystemName;
  FVolumeName:=LoadVolumeName;
  FVolumeSerial:=LoadVolumeSerial;
  FFileSysType:=LoadFileSysType;
  FVolumeFlags:=LoadVolumeFlags;
  
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.FileSystemInit Completed');
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                SystemName = ' + FSystemName);
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                VolumeName = ' + FVolumeName);
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                VolumeSerial = ' + IntToHex(FVolumeSerial,8));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                FileSysType = ' + FileSysTypeToString(FFileSysType));
  {$ENDIF}
  
  Result:=True;
 finally  
  WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.MountFileSystem:Boolean;
begin
 {}
 Result:=False;

 if not WriterLock then Exit;
 try
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.MountFileSystem');
  {$ENDIF}
  
  if FDriver = nil then Exit;

  //To do - Allows for Mount/Dismount by formatter without dismounting Drive/Volume
  //See NTFS
  
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.MountFileSystem Completed');
  {$ENDIF}
  
  //Result:=True; //To do
 finally  
  WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.DismountFileSystem:Boolean;
begin
 {}
 Result:=False;

 if not WriterLock then Exit;
 try
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.DismountFileSystem');
  {$ENDIF}
  
  if FDriver = nil then Exit;

  //To do - Allows for Mount/Dismount by formatter without dismounting Drive/Volume
  //See NTFS
  
  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.DismountFileSystem Completed');
  {$ENDIF}
  
  //Result:=True; //To do
 finally  
  WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.InitializeFileSystem(ASectorsPerCluster:LongWord;AFileSysType:TFileSysType):Boolean;
begin
 {}
 Result:=False;

 if not WriterLock then Exit;
 try
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.InitializeFileSystem');
  {$ENDIF}
 
  if FDriver = nil then Exit;

  //To do - Allows for formatter to init a new volume with the default data
  //See NTFS
  
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.InitializeFileSystem Completed');
  {$ENDIF}
  
  //Result:=True; //To do
 finally  
  WriterUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.GetDriveLabel:String;
begin
 {}
 Result:=cdfsBlankName;

 if not ReaderLock then Exit;
 try
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.GetDriveLabel');
  {$ENDIF}

  Result:=LoadVolumeName;
 finally  
  ReaderUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.SetDriveLabel(const ALabel:String):Boolean;
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.SetDriveLabel');
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                Label = ' + ALabel);
  {$ENDIF}

  Result:=SetVolumeName(ALabel);
 finally  
  ReaderUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.SetDriveSerial(ASerial:LongWord):Boolean;
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.SetDriveSerial');
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                Serial = ' + IntToHex(ASerial,8));
  {$ENDIF}

  Result:=SetVolumeSerial(ASerial);
 finally  
  ReaderUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.GetDriveFreeSpaceEx:Int64;
begin
 {}
 Result:=0;

 if not ReaderLock then Exit;
 try
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.GetDriveFreeSpaceEx');
  {$ENDIF}
 
  if FDriver = nil then Exit;

  {Check Free Cluster Count}
  if GetFreeClusterCount = cdfsUnknownCluster then Exit;
  
  {Return Free Space}
  Result:=FFreeClusterCount;
  Result:=(Result * FSectorsPerCluster);
  Result:=(Result * FSectorSize);
  {Modified to ensure Int64 mutliply}
  {Result:=((FFreeClusterCount * FSectorsPerCluster) * FSectorSize);}
 finally  
  ReaderUnlock;
 end; 
end;

{=============================================================================}

function TCDFSFileSystem.GetDriveTotalSpaceEx:Int64;
begin
 {}
 Result:=0;

 if not ReaderLock then Exit;
 try
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.GetDriveTotalSpaceEx');
  {$ENDIF}

  if FDriver = nil then Exit;

  {Return Total Space}
  Result:=FTotalClusterCount;
  Result:=(Result * FSectorsPerCluster);
  Result:=(Result * FSectorSize);
  {Modified to ensure Int64 mutliply}
  {Result:=((FTotalClusterCount * FSectorsPerCluster) * FSectorSize);}
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TCDFSFileSystem.GetDriveInformation(var AClusterSize:LongWord;var ATotalClusterCount,AFreeClusterCount:Int64):Boolean;
{Get Drive Information from internal CDFS data}
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  {$IFDEF CDFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TCDFSFileSystem.GetDriveInformation');
  {$ENDIF}

  if FDriver = nil then Exit;

  {Check Free Cluster Count}
  if GetFreeClusterCount = cdfsUnknownCluster then Exit;
  
  {Return Drive Information}
  AClusterSize:=FClusterSize;
  ATotalClusterCount:=FTotalClusterCount;
  AFreeClusterCount:=FFreeClusterCount;
  
  Result:=True;
 finally  
  ReaderUnlock;
 end; 
end;
  
{==============================================================================}
{==============================================================================}
{TCDFSDiskTable}
constructor TCDFSDiskTable.Create(ALock:TSynchronizerHandle;ALocalLock:TMutexHandle;ADescriptor:TCDFSDiskDescriptor);
begin
 {}
 inherited Create(ALocalLock);
 FPaths:=TFileSysListEx.Create(ALock);
 FPathLocal:=MutexCreate;
 
 FDescriptor:=ADescriptor;
end;

{=============================================================================}

destructor TCDFSDiskTable.Destroy;
begin
 {}
 FPaths.Free;
 MutexDestroy(FPathLocal);
 
 FDescriptor:=nil;
 inherited Destroy;
end;

{==============================================================================}
{==============================================================================}
{TCDFSDiskBlock}
constructor TCDFSDiskBlock.Create(ALocalLock:TMutexHandle);
begin
 {}
 inherited Create(ALocalLock);
 FBlockCount:=0;
 FBlockBuffer:=nil;
 FBlockCluster:=cdfsUnknownCluster;
end;

{=============================================================================}

destructor TCDFSDiskBlock.Destroy;
begin
 {}
 if FBlockBuffer <> nil then FreeMem(FBlockBuffer);
 FBlockBuffer:=nil;
 inherited Destroy;
end;

{==============================================================================}
{==============================================================================}
{TCDFSDiskEntry}
constructor TCDFSDiskEntry.Create(ALocalLock:TMutexHandle);
begin
 {}
 inherited Create(ALocalLock);
 FPath:=nil;
 FPathM:=nil;
 FAltPath:=nil;
 FAltPathM:=nil;
 FExtended:=nil;
end;

{=============================================================================}

destructor TCDFSDiskEntry.Destroy;
begin
 {}
 FPath:=nil;
 FPathM:=nil;
 FAltPath:=nil;
 FAltPathM:=nil;
 if FExtended <> nil then FExtended.Free;
 FExtended:=nil;
 inherited Destroy;
end;

{=============================================================================}

function TCDFSDiskEntry.GetVersion:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;
 
 Result:=FVersion;
 UniqueString(Result);
 
 ReleaseLock;
end;

{=============================================================================}

procedure TCDFSDiskEntry.SetVersion(const AVersion:String);
begin
 {}
 if not AcquireLock then Exit;
 
 FVersion:=AVersion;
 UniqueString(FVersion);
 
 ReleaseLock;
end;

{=============================================================================}

function TCDFSDiskEntry.PathNumber:Word;
begin
 {}
 Result:=0;
 
 if (Attributes and faDirectory) = faDirectory then
  begin
   {Folder}
   if FPath = nil then Exit;
   
   Result:=FPath.PathNumber;
  end;
end;

{=============================================================================}

function TCDFSDiskEntry.ParentNumber:Word;
begin
 {}
 Result:=0;
 
 if (Attributes and faDirectory) = faDirectory then
  begin
   {Folder}
   if FPath = nil then Exit;
   
   if FPath.Parent = nil then
    begin
     {Root}
     if Parent <> nil then Exit;
     
     Result:=FPath.PathNumber;
    end
   else
    begin
     {Others}
     Result:=FPath.Parent.PathNumber;
    end;
  end;
end;

{=============================================================================}

function TCDFSDiskEntry.FileIdentifier:String;
begin
 {}
 Result:=cdfsBlankName;
 
 if (Attributes and faDirectory) = faNone then
  begin
   {File}
   Result:=Name;
   
   {Check Dot}
   if StrScan(PChar(Name),cdfsSeparator1Name) = nil then Result:=Result + cdfsSeparator1Name;
   
   {Check Version}
   if Length(Version) > 0 then Result:=Result + cdfsSeparator2Name + Version;
  end
 else if (Attributes and faDirectory) = faDirectory then
  begin
   {Folder}
   if (Attributes and faDot) = faDot then
    begin
     Result:=cdfsDotName;
    end
   else if (Attributes and faDotDot) = faDotDot then
    begin
     Result:=cdfsDotDotName;
    end
   else
    begin
     Result:=Name;
    end;
  end;
end;

{=============================================================================}

function TCDFSDiskEntry.FileIdentifierSize:Byte;
begin
 {}
 Result:=0;
 
 if (Attributes and faDirectory) = faNone then
  begin
   {File}
   Result:=Length(Name);
   
   {Check Dot}
   if StrScan(PChar(Name),cdfsSeparator1Name) = nil then Inc(Result);
   
   {Check Version}
   if Length(Version) > 0 then Inc(Result,Length(cdfsSeparator2Name) + Length(Version));
   
   {Check Unicode}
   if Unicode then Result:=(Result shl 1);
  end
 else if (Attributes and faDirectory) = faDirectory then
  begin
   {Folder}
   if (Attributes and faDot) = faDot then
    begin
     Result:=cdfsFileIdentifierSize;
    end
   else if (Attributes and faDotDot) = faDotDot then
    begin
     Result:=cdfsFileIdentifierSize;
    end
   else
    begin
     Result:=Length(Name);
     
     {Check Unicode}
     if Unicode then Result:=(Result shl 1);
    end;
  end;
end;

{=============================================================================}

function TCDFSDiskEntry.DirectoryRecordSize:Byte;
var
 IdentifierSize:Byte;
begin
 {Record Size}
 Result:=cdfsDirectoryRecordSize - 1; {Default includes 1 byte identifier}
 
 {Identifier Size}
 IdentifierSize:=FileIdentifierSize;
 Inc(Result,IdentifierSize);
 
 {Identifier Padding}
 if (IdentifierSize and cdfsUnevenSize) = cdfsEvenSize then Inc(Result);
end;

{=============================================================================}

function TCDFSDiskEntry.ExtendedRecordSize:Byte;
begin
 {}
 Result:=0;
 
 if FExtended = nil then Exit;
 
 Result:=FExtended.ExtendedRecordSize;
end;

{==============================================================================}
{==============================================================================}
{TCDFSDiskCatalog}
constructor TCDFSDiskCatalog.Create(ALocalLock:TMutexHandle);
begin
 {}
 inherited Create(ALocalLock);
 FSelectionData:=nil;

 FEntry:=nil;
 FHeader:=nil;
end;

{=============================================================================}

destructor TCDFSDiskCatalog.Destroy;
begin
 {}
 WriterLock;
 try
  if FSelectionData <> nil then FreeMem(FSelectionData);
  FSelectionData:=nil;

  FEntry:=nil;
  FHeader:=nil;
 finally
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  inherited Destroy;
 end; 
end;

{=============================================================================}

function TCDFSDiskCatalog.GetName:String;
begin
 {}
 Result:='';
 
 if FEntry <> nil then
  begin
   Result:=FEntry.Name;
  end;
end;

{=============================================================================}

procedure TCDFSDiskCatalog.SetName(const AName:String);
begin
 {Nothing}
end;

{=============================================================================}

function TCDFSDiskCatalog.GetMediaType:TMediaType;
begin
 {}
 Result:=mtUNKNOWN;
 
 if (FBootMedia and cdfsElToritoMediaTypeMask) = cdfsElToritoMediaType12M then
  begin
   Result:=mtFLOPPY;
  end
 else if (FBootMedia and cdfsElToritoMediaTypeMask) = cdfsElToritoMediaType144M then
  begin
   Result:=mtFLOPPY;
  end
 else if (FBootMedia and cdfsElToritoMediaTypeMask) = cdfsElToritoMediaType288M then
  begin
   Result:=mtFLOPPY;
  end
 else if (FBootMedia and cdfsElToritoMediaTypeMask) = cdfsElToritoMediaTypeHDD then
  begin
   Result:=mtFIXED;
  end;
end;

{=============================================================================}

procedure TCDFSDiskCatalog.SetMediaType(AMediaType:TMediaType);
begin
 {}
 case AMediaType of
  mtUNKNOWN:begin
    FBootMedia:=(FBootMedia and cdfsElToritoMediaFlagMask) or cdfsElToritoMediaTypeNone;
   end;
  mtFIXED:begin
    FBootMedia:=(FBootMedia and cdfsElToritoMediaFlagMask) or cdfsElToritoMediaTypeHDD;
   end;
 end;
end;

{=============================================================================}

function TCDFSDiskCatalog.GetFloppyType:TFloppyType;
begin
 {}
 Result:=ftUNKNOWN;
 
 if (FBootMedia and cdfsElToritoMediaTypeMask) = cdfsElToritoMediaType12M then
  begin
   Result:=ft12M;
  end
 else if (FBootMedia and cdfsElToritoMediaTypeMask) = cdfsElToritoMediaType144M then
  begin
   Result:=ft144M;
  end
 else if (FBootMedia and cdfsElToritoMediaTypeMask) = cdfsElToritoMediaType288M then
  begin
   Result:=ft288M;
  end
 else if (FBootMedia and cdfsElToritoMediaTypeMask) = cdfsElToritoMediaTypeHDD then
  begin
   Result:=ftINVALID;
  end;
end;

{=============================================================================}

procedure TCDFSDiskCatalog.SetFloppyType(AFloppyType:TFloppyType);
begin
 {}
 case AFloppyType of
  ft12M:begin
    FBootMedia:=(FBootMedia and cdfsElToritoMediaFlagMask) or cdfsElToritoMediaType12M;
   end;
  ft144M:begin
    FBootMedia:=(FBootMedia and cdfsElToritoMediaFlagMask) or cdfsElToritoMediaType144M;
   end;
  ft288M:begin
    FBootMedia:=(FBootMedia and cdfsElToritoMediaFlagMask) or cdfsElToritoMediaType288M;
   end;
 end;
end;

{=============================================================================}

function TCDFSDiskCatalog.GetAttributes:LongWord;
begin
 {}
 Result:=caNone;
 
 {Default}
 if FInitial then
  begin
   Result:=Result or caDefault;
  end;
 {Bootable}
 if FBootIndicator = cdfsElToritoBootIndicator then
  begin
   Result:=Result or caBootable;
  end;
 {Emulation}
 if (FBootMedia and cdfsElToritoMediaTypeMask) = cdfsElToritoMediaTypeNone then
  begin
   Result:=Result or caNoEmulation;
  end
 else if (FBootMedia and cdfsElToritoMediaTypeMask) = cdfsElToritoMediaType12M then
  begin
   Result:=Result or caFloppy12M;
  end
 else if (FBootMedia and cdfsElToritoMediaTypeMask) = cdfsElToritoMediaType144M then
  begin
   Result:=Result or caFloppy144M;
  end
 else if (FBootMedia and cdfsElToritoMediaTypeMask) = cdfsElToritoMediaType288M then
  begin
   Result:=Result or caFloppy288M;
  end
 else if (FBootMedia and cdfsElToritoMediaTypeMask) = cdfsElToritoMediaTypeHDD then
  begin
   Result:=Result or caHardDisk;
  end;
end;

{=============================================================================}

procedure TCDFSDiskCatalog.SetAttributes(AAttributes:LongWord);
begin
 {Default}
 if (AAttributes and caDefault) = caDefault then
  begin
   FInitial:=True;
  end
 else
  begin
   FInitial:=False;
  end;
  
 {Bootable}
 if (AAttributes and caBootable) = caBootable then
  begin
   FBootIndicator:=cdfsElToritoBootIndicator;
  end
 else
  begin
   FBootIndicator:=cdfsElToritoNoBootIndicator;
  end;
  
 {Emulation}
 if (AAttributes and caNoEmulation) = caNoEmulation then
  begin
   FBootMedia:=(FBootMedia and cdfsElToritoMediaFlagMask) or cdfsElToritoMediaTypeNone;
  end
 else if (AAttributes and caFloppy12M) = caFloppy12M then
  begin
   FBootMedia:=(FBootMedia and cdfsElToritoMediaFlagMask) or cdfsElToritoMediaType12M;
  end
 else if (AAttributes and caFloppy144M) = caFloppy144M then
  begin
   FBootMedia:=(FBootMedia and cdfsElToritoMediaFlagMask) or cdfsElToritoMediaType144M;
  end
 else if (AAttributes and caFloppy288M) = caFloppy288M then
  begin
   FBootMedia:=(FBootMedia and cdfsElToritoMediaFlagMask) or cdfsElToritoMediaType288M;
  end
 else if (AAttributes and caHardDisk) = caHardDisk then
  begin
   FBootMedia:=(FBootMedia and cdfsElToritoMediaFlagMask) or cdfsElToritoMediaTypeHDD;
  end;
end;

{=============================================================================}

function TCDFSDiskCatalog.GetSectorSize:Word;
begin
 {}
 Result:=MIN_SECTOR_SIZE; {Sector Size is always 512 bytes for ElTorito}
end;

{=============================================================================}

procedure TCDFSDiskCatalog.SetSectorSize(ASectorSize:Word);
begin
 {Nothing}
end;

{=============================================================================}

function TCDFSDiskCatalog.GetSectorCount:Int64;
begin
 {}
 Result:=0;
 
 if (FBootMedia and cdfsElToritoMediaTypeMask) = cdfsElToritoMediaTypeNone then
  begin
   if FEntry <> nil then
    begin
     Result:=(FEntry.ClusterCount shl 2); {Sector Size is always 512 bytes for ElTorito}
    end;
  end
 else if (FBootMedia and cdfsElToritoMediaTypeMask) = cdfsElToritoMediaType12M then
  begin
   Result:=2400;
  end
 else if (FBootMedia and cdfsElToritoMediaTypeMask) = cdfsElToritoMediaType144M then
  begin
   Result:=2880;
  end
 else if (FBootMedia and cdfsElToritoMediaTypeMask) = cdfsElToritoMediaType288M then
  begin
   Result:=5760;
  end
 else if (FBootMedia and cdfsElToritoMediaTypeMask) = cdfsElToritoMediaTypeHDD then
  begin
   if FEntry <> nil then
    begin
     Result:=(FEntry.ClusterCount shl 2); {Sector Size is always 512 bytes for ElTorito}
    end;
  end;
end;

{=============================================================================}

procedure TCDFSDiskCatalog.SetSectorCount(const ASectorCount:Int64);
begin
 {Nothing}
end;

{=============================================================================}

function TCDFSDiskCatalog.GetStartCluster:LongWord;
begin
 {}
 Result:=FStartCluster;
 
 if FEntry <> nil then
  begin
   Result:=FEntry.StartCluster;
  end;
end;

{=============================================================================}

procedure TCDFSDiskCatalog.SetStartCluster(AStartCluster:LongWord);
begin
 {}
 FStartCluster:=AStartCluster
end;

{=============================================================================}

function TCDFSDiskCatalog.GetClusterCount:LongWord;
begin
 {}
 Result:=FClusterCount;
 
 if (FBootMedia and cdfsElToritoMediaTypeMask) = cdfsElToritoMediaTypeNone then
  begin
   if FEntry <> nil then
    begin
     Result:=FEntry.ClusterCount;
    end;
  end
 else if (FBootMedia and cdfsElToritoMediaTypeMask) = cdfsElToritoMediaType12M then
  begin
   Result:=600; {2400 Sectors}
  end
 else if (FBootMedia and cdfsElToritoMediaTypeMask) = cdfsElToritoMediaType144M then
  begin
   Result:=720; {2880 Sectors}
  end
 else if (FBootMedia and cdfsElToritoMediaTypeMask) = cdfsElToritoMediaType288M then
  begin
   Result:=1440; {5760 Sectors}
  end
 else if (FBootMedia and cdfsElToritoMediaTypeMask) = cdfsElToritoMediaTypeHDD then
  begin
   if FEntry <> nil then
    begin
     Result:=FEntry.ClusterCount;
    end;
  end;
end;

{=============================================================================}

procedure TCDFSDiskCatalog.SetClusterCount(AClusterCount:LongWord);
begin
 {}
 FClusterCount:=AClusterCount;
end;

{==============================================================================}
{==============================================================================}
{TCDFSDiskPath}
constructor TCDFSDiskPath.Create(ALocalLock:TMutexHandle);
begin
 {}
 inherited Create;
 FLocalLock:=ALocalLock;
 
 FParent:=nil;
end;

{=============================================================================}

destructor TCDFSDiskPath.Destroy;
begin
 {}
 FParent:=nil;
 
 FLocalLock:=INVALID_HANDLE_VALUE;
 inherited Destroy;
end;

{==============================================================================}

function TCDFSDiskPath.AcquireLock:Boolean;
begin
 {}
 Result:=(MutexLock(FLocalLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TCDFSDiskPath.ReleaseLock:Boolean;
begin
 {}
 Result:=(MutexUnlock(FLocalLock) = ERROR_SUCCESS);
end;

{=============================================================================}

function TCDFSDiskPath.GetName:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;
 
 Result:=FName;
 UniqueString(Result);
 
 ReleaseLock;
end;

{=============================================================================}

procedure TCDFSDiskPath.SetName(const AName:String);
begin
 {}
 if not AcquireLock then Exit;
 
 FName:=AName;
 UniqueString(FName);
 FHash:=GenerateNameHash(FName,NAME_HASH_SIZE); 
 
 ReleaseLock;
end;

{=============================================================================}

function TCDFSDiskPath.PathIdentifier:String;
begin
 {}
 Result:=Name; {No Dot or Version on Paths (Folders only)}
end;

{=============================================================================}

function TCDFSDiskPath.PathIdentifierSize(AUnicode:Boolean):Byte;
begin
 {}
 Result:=Length(Name);
 
 {Check for Dots}
 if (Result = 1) and (Name = cdfsDotName) then
  begin
   Result:=cdfsPathIdentifierSize;
  end
 else if (Result = 2) and (Name = cdfsDotDotName) then
  begin
   Result:=cdfsPathIdentifierSize;
  end
 else
  begin
   {Check Unicode}
   if AUnicode then Result:=(Result shl 1);
  end;
end;

{=============================================================================}

function TCDFSDiskPath.PathRecordSize(AUnicode:Boolean):Byte;
var
 IdentifierSize:Byte;
begin
 {Record Size}
 Result:=cdfsPathRecordSize - 1; {Default includes 1 byte identifier}
 
 {Identifier Size}
 IdentifierSize:=PathIdentifierSize(AUnicode);
 Inc(Result,IdentifierSize);
 
 {Identifier Padding}
 if (IdentifierSize and cdfsUnevenSize) = cdfsUnevenSize then Inc(Result);
end;

{==============================================================================}
{==============================================================================}
{TCDFSDiskExtended}
constructor TCDFSDiskExtended.Create(ALocalLock:TMutexHandle);
begin
 {}
 inherited Create;
 FLocalLock:=ALocalLock;
 
 FSystemData:=nil;
 FApplicationData:=nil;
 FEscapeSequence:=nil;
end;

{=============================================================================}

destructor TCDFSDiskExtended.Destroy;
begin
 {}
 if FSystemData <> nil then FreeMem(FSystemData);
 FSystemData:=nil;
 if FApplicationData <> nil then FreeMem(FApplicationData);
 FApplicationData:=nil;
 if FEscapeSequence <> nil then FreeMem(FEscapeSequence);
 FEscapeSequence:=nil;
 
 FLocalLock:=INVALID_HANDLE_VALUE;
 inherited Destroy;
end;

{==============================================================================}

function TCDFSDiskExtended.AcquireLock:Boolean;
begin
 {}
 Result:=(MutexLock(FLocalLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TCDFSDiskExtended.ReleaseLock:Boolean;
begin
 {}
 Result:=(MutexUnlock(FLocalLock) = ERROR_SUCCESS);
end;

{=============================================================================}

function TCDFSDiskExtended.GetSystemIdentifier:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;
 
 Result:=FSystemIdentifier;
 UniqueString(Result);
 
 ReleaseLock;
end;

{=============================================================================}

procedure TCDFSDiskExtended.SetSystemIdentifier(const ASystemIdentifier:String);
begin
 {}
 if not AcquireLock then Exit;
 
 FSystemIdentifier:=ASystemIdentifier;
 UniqueString(FSystemIdentifier);
 
 ReleaseLock;
end;

{=============================================================================}

function TCDFSDiskExtended.ExtendedRecordSize:Byte;
begin
 {}
 Result:=0; //To Do //Account for ApplicationDataSize/EscapeSequenceSize
end;

{==============================================================================}
{==============================================================================}
{TCDFSDiskDescriptor}
constructor TCDFSDiskDescriptor.Create(ALocalLock:TMutexHandle);
begin
 {}
 inherited Create;
 FLocalLock:=ALocalLock;
 
 FApplicationData:=nil;
 FEscapeSequences:=nil;
 FSystemData:=nil;

 FRoot:=nil;
 FPrimary:=nil;
 FPrimaryM:=nil;
 FAlternate:=nil;
 FAlternateM:=nil;

 FInitial:=nil;
 FValidation:=nil;
end;

{=============================================================================}

destructor TCDFSDiskDescriptor.Destroy;
begin
 {}
 if FApplicationData <> nil then FreeMem(FApplicationData);
 FApplicationData:=nil;
 if FEscapeSequences <> nil then FreeMem(FEscapeSequences);
 FEscapeSequences:=nil;
 if FSystemData <> nil then FreeMem(FSystemData);
 FSystemData:=nil;

 FRoot:=nil;
 FPrimary:=nil;
 FPrimaryM:=nil;
 FAlternate:=nil;
 FAlternateM:=nil;

 FInitial:=nil;
 FValidation:=nil;
 
 FLocalLock:=INVALID_HANDLE_VALUE;
 inherited Destroy;
end;

{==============================================================================}

function TCDFSDiskDescriptor.AcquireLock:Boolean;
begin
 {}
 Result:=(MutexLock(FLocalLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TCDFSDiskDescriptor.ReleaseLock:Boolean;
begin
 {}
 Result:=(MutexUnlock(FLocalLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TCDFSDiskDescriptor.GetStandardIdentifier:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;
 
 Result:=FStandardIdentifier;
 UniqueString(Result);
 
 ReleaseLock;
end;

{==============================================================================}

procedure TCDFSDiskDescriptor.SetStandardIdentifier(const AStandardIdentifier:String);
begin
 {}
 if not AcquireLock then Exit;
 
 FStandardIdentifier:=AStandardIdentifier;
 UniqueString(FStandardIdentifier);
 
 ReleaseLock;
end;

{==============================================================================}

function TCDFSDiskDescriptor.GetSystemIdentifier:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;
 
 Result:=FSystemIdentifier;
 UniqueString(Result);
 
 ReleaseLock;
end;

{==============================================================================}

procedure TCDFSDiskDescriptor.SetSystemIdentifier(const ASystemIdentifier:String);
begin
 {}
 if not AcquireLock then Exit;
 
 FSystemIdentifier:=ASystemIdentifier;
 UniqueString(FSystemIdentifier);
 
 ReleaseLock;
end;

{==============================================================================}

function TCDFSDiskDescriptor.GetVolumeIdentifier:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;
 
 Result:=FVolumeIdentifier;
 UniqueString(Result);
 
 ReleaseLock;
end;

{==============================================================================}

procedure TCDFSDiskDescriptor.SetVolumeIdentifier(const AVolumeIdentifier:String);
begin
 {}
 if not AcquireLock then Exit;
 
 FVolumeIdentifier:=AVolumeIdentifier;
 UniqueString(FVolumeIdentifier);
 
 ReleaseLock;
end;

{==============================================================================}

function TCDFSDiskDescriptor.GetVolumeSetIdentifier:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;
 
 Result:=FVolumeSetIdentifier;
 UniqueString(Result);
 
 ReleaseLock;
end;

{==============================================================================}

procedure TCDFSDiskDescriptor.SetVolumeSetIdentifier(const AVolumeSetIdentifier:String);
begin
 {}
 if not AcquireLock then Exit;
 
 FVolumeSetIdentifier:=AVolumeSetIdentifier;
 UniqueString(FVolumeSetIdentifier);
 
 ReleaseLock;
end;

{==============================================================================}

function TCDFSDiskDescriptor.GetPublisherIdentifier:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;
 
 Result:=FPublisherIdentifier;
 UniqueString(Result);
 
 ReleaseLock;
end;

{==============================================================================}

procedure TCDFSDiskDescriptor.SetPublisherIdentifier(const APublisherIdentifier:String);
begin
 {}
 if not AcquireLock then Exit;
 
 FPublisherIdentifier:=APublisherIdentifier;
 UniqueString(FPublisherIdentifier);
 
 ReleaseLock;
end;

{==============================================================================}

function TCDFSDiskDescriptor.GetPreparerIdentifier:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;
 
 Result:=FPreparerIdentifier;
 UniqueString(Result);
 
 ReleaseLock;
end;

{==============================================================================}

procedure TCDFSDiskDescriptor.SetPreparerIdentifier(const APreparerIdentifier:String);
begin
 {}
 if not AcquireLock then Exit;
 
 FPreparerIdentifier:=APreparerIdentifier;
 UniqueString(FPreparerIdentifier);
 
 ReleaseLock;
end;

{==============================================================================}

function TCDFSDiskDescriptor.GetApplicationIdentifier:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;
 
 Result:=FApplicationIdentifier;
 UniqueString(Result);
 
 ReleaseLock;
end;

{==============================================================================}

procedure TCDFSDiskDescriptor.SetApplicationIdentifier(const AApplicationIdentifier:String);
begin
 {}
 if not AcquireLock then Exit;
 
 FApplicationIdentifier:=AApplicationIdentifier;
 UniqueString(FApplicationIdentifier);
 
 ReleaseLock;
end;

{==============================================================================}

function TCDFSDiskDescriptor.GetCopyrightIdentifier:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;
 
 Result:=FCopyrightIdentifier;
 UniqueString(Result);
 
 ReleaseLock;
end;

{==============================================================================}

procedure TCDFSDiskDescriptor.SetCopyrightIdentifier(const ACopyrightIdentifier:String);
begin
 {}
 if not AcquireLock then Exit;
 
 FCopyrightIdentifier:=ACopyrightIdentifier;
 UniqueString(FCopyrightIdentifier);
 
 ReleaseLock;
end;

{==============================================================================}

function TCDFSDiskDescriptor.GetAbstractIdentifier:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;
 
 Result:=FAbstractIdentifier;
 UniqueString(Result);
 
 ReleaseLock;
end;

{==============================================================================}

procedure TCDFSDiskDescriptor.SetAbstractIdentifier(const AAbstractIdentifier:String);
begin
 {}
 if not AcquireLock then Exit;
 
 FAbstractIdentifier:=AAbstractIdentifier;
 UniqueString(FAbstractIdentifier);
 
 ReleaseLock;
end;

{==============================================================================}

function TCDFSDiskDescriptor.GetBibliographicIdentifier:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;
 
 Result:=FBibliographicIdentifier;
 UniqueString(Result);
 
 ReleaseLock;
end;

{==============================================================================}

procedure TCDFSDiskDescriptor.SetBibliographicIdentifier(const ABibliographicIdentifier:String);
begin
 {}
 if not AcquireLock then Exit;
 
 FBibliographicIdentifier:=ABibliographicIdentifier;
 UniqueString(FBibliographicIdentifier);
 
 ReleaseLock;
end;

{==============================================================================}

function TCDFSDiskDescriptor.GetBootIdentifier:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;
 
 Result:=FBootIdentifier;
 UniqueString(Result);
 
 ReleaseLock;
end;

{==============================================================================}

procedure TCDFSDiskDescriptor.SetBootIdentifier(const ABootIdentifier:String);
begin
 {}
 if not AcquireLock then Exit;
 
 FBootIdentifier:=ABootIdentifier;
 UniqueString(FBootIdentifier);
 
 ReleaseLock;
end;

{==============================================================================}

function TCDFSDiskDescriptor.GetPartitionIdentifier:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;
 
 Result:=FPartitionIdentifier;
 UniqueString(Result);
 
 ReleaseLock;
end;

{==============================================================================}

procedure TCDFSDiskDescriptor.SetPartitionIdentifier(const APartitionIdentifier:String);
begin
 {}
 if not AcquireLock then Exit;
 
 FPartitionIdentifier:=APartitionIdentifier;
 UniqueString(FPartitionIdentifier);
 
 ReleaseLock;
end;

{==============================================================================}
{==============================================================================}
{TCDFSDiskHeader}
constructor TCDFSDiskHeader.Create(ALocalLock:TMutexHandle);
begin
 {}
 inherited Create;
 FLocalLock:=ALocalLock;
end;

{=============================================================================}

destructor TCDFSDiskHeader.Destroy;
begin
 {}
 FLocalLock:=INVALID_HANDLE_VALUE;
 inherited Destroy;
end;

{==============================================================================}

function TCDFSDiskHeader.AcquireLock:Boolean;
begin
 {}
 Result:=(MutexLock(FLocalLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TCDFSDiskHeader.ReleaseLock:Boolean;
begin
 {}
 Result:=(MutexUnlock(FLocalLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TCDFSDiskHeader.GetVendorId:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;

 Result:=FVendorId;
 UniqueString(Result);
 
 ReleaseLock;
end;

{==============================================================================}

procedure TCDFSDiskHeader.SetVendorId(const AVendorId:String);
begin
 {}
 if not AcquireLock then Exit;

 FVendorId:=AVendorId;
 UniqueString(FVendorId);
 
 ReleaseLock;
end;

{==============================================================================}
{==============================================================================}
{TCDFSDiskExtension}
constructor TCDFSDiskExtension.Create(ALocalLock:TMutexHandle);
begin
 {}
 inherited Create;
 FLocalLock:=ALocalLock;
 
 FSelectionData:=nil;

 FCatalog:=nil;
end;

{=============================================================================}

destructor TCDFSDiskExtension.Destroy;
begin
 {}
 if FSelectionData <> nil then FreeMem(FSelectionData);
 FSelectionData:=nil;

 FCatalog:=nil;
 
 FLocalLock:=INVALID_HANDLE_VALUE;
 inherited Destroy;
end;

{==============================================================================}

function TCDFSDiskExtension.AcquireLock:Boolean;
begin
 {}
 Result:=(MutexLock(FLocalLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TCDFSDiskExtension.ReleaseLock:Boolean;
begin
 {}
 Result:=(MutexUnlock(FLocalLock) = ERROR_SUCCESS);
end;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure CDFSInit;
var
 Recognizer:TCDFSRecognizer;
begin
 {}
 {Check Initialized}
 if CDFSInitialized then Exit;
 
 {Check Driver}
 if FileSysDriver = nil then Exit;
 
 {Create CDFS Recognizer}
 if FILESYS_CDFS_ENABLED then
  begin
   Recognizer:=TCDFSRecognizer.Create(FileSysDriver);
   Recognizer.AllowDrive:=FILESYS_DRIVES_ENABLED;
   Recognizer.AllowDefault:=CDFS_DEFAULT;
   Recognizer.LongNames:=CDFS_LONG_NAMES;
   Recognizer.SwapSerial:=CDFS_SWAP_SERIAL;
  end;
 
 CDFSInitialized:=True;
end;

{==============================================================================}

procedure CDFSQuit;
var
 NextRecognizer:TRecognizer;
 CurrentRecognizer:TRecognizer;
 NextFileSystem:TFileSystem;
 CurrentFileSystem:TFileSystem;
begin
 {}
 {Check Initialized}
 if not CDFSInitialized then Exit;
 
 {Check Driver}
 if FileSysDriver = nil then Exit;
 
 {Terminate FileSystems}
 NextFileSystem:=FileSysDriver.GetFileSystemByNext(nil,True,False,FILESYS_LOCK_READ); 
 while NextFileSystem <> nil do
  begin
   CurrentFileSystem:=NextFileSystem;
   NextFileSystem:=FileSysDriver.GetFileSystemByNext(CurrentFileSystem,True,False,FILESYS_LOCK_READ); 
   
   if CurrentFileSystem is TCDFSFileSystem then
    begin
     {Convert FileSystem}
     CurrentFileSystem.ReaderConvert;

     {FileSysDriver.RemoveFileSystem(CurrentFileSystem);} {Done by Destroy}
     CurrentFileSystem.DismountFileSystem;
     CurrentFileSystem.Free;
    end
   else
    begin
     {Unlock FileSystem}
     CurrentFileSystem.ReaderUnlock;
    end;
  end;

 {Terminate Recognizer}
 NextRecognizer:=FileSysDriver.GetRecognizerByNext(nil,True,False,FILESYS_LOCK_READ); 
 while NextRecognizer <> nil do
  begin
   CurrentRecognizer:=NextRecognizer;
   NextRecognizer:=FileSysDriver.GetRecognizerByNext(CurrentRecognizer,True,False,FILESYS_LOCK_READ);
   
   if CurrentRecognizer is TCDFSRecognizer then
    begin
     {Convert Recognizer}
     CurrentRecognizer.ReaderConvert;

     {FileSysDriver.RemoveRecognizer(CurrentRecognizer);} {Done by Destroy}
     CurrentRecognizer.Free;
    end
   else
    begin
     {Unlock Recognizer}
     CurrentRecognizer.ReaderUnlock;
    end;
  end;
 
 CDFSInitialized:=False;
end;

{==============================================================================}
{==============================================================================}
{CDFS Functions}

{==============================================================================}
{==============================================================================}
{CDFS Helper Functions}
function CDFSDataToPointer(const AData;ASize:Integer;ASwap:Boolean):Pointer;
{Creates a pointer and copies data from a buffer}
{Allows byte order swapping on copy}
var
 Count:Integer;
begin
 {}
 Result:=nil;
 
 {Check Size}
 if ASize > 0 then
  begin
   Result:=GetMem(ASize);
   if Result = nil then Exit;
   
   System.Move(AData,Result^,ASize);
   
   {Check Swap}
   if ASwap then
    begin
     Count:=0;
     while (Count + 1) < ASize do  {Account for odd size}
      begin
       PWord(PtrUInt(Result) + LongWord(Count))^:=SwapEndian(PWord(PtrUInt(Result) + LongWord(Count))^);
       Inc(Count,2); {SizeOf(Word)}
      end;
    end;
  end;
end;

{=============================================================================}

function CDFSPointerToData(APointer:Pointer;var AData;ASize:Integer;ASwap:Boolean):Boolean;
{Copies data to a buffer from supplied pointer}
{Allows byte order swapping on copy}
var
 Count:Integer;
begin
 {}
 Result:=False;
 
 if APointer = nil then Exit;
 
 {Check Size}
 if ASize > 0 then
  begin
   System.Move(APointer^,AData,ASize);
   
   {Check Swap}
   if ASwap then
    begin
     Count:=0;
     while (Count + 1) < ASize do  {Account for odd size}
      begin
       PWord(PtrUInt(@AData) + LongWord(Count))^:=SwapEndian(PWord(PtrUInt(@AData) + LongWord(Count))^);
       Inc(Count,2); {SizeOf(Word)}
      end;
    end;
  end;
  
 Result:=True;
end;

{=============================================================================}

function CDFSIdentifierToString(const AIdentifier;ASize:Integer;AUnicode:Boolean):String;
{Converts the supplied indentifier to a string}
{Accounts for unicode and byte ordering}
var
 Count:Integer;
 Length:Integer;
 Buffer:Pointer;
 Terminator:PChar;
begin
 {}
 Result:=cdfsBlankName;
 
 {Check Size}
 if ASize > 0 then
  begin
   {Check Unicode}
   if AUnicode then
    begin
     {Get Buffer}
     Buffer:=CDFSDataToPointer(AIdentifier,ASize,True);
     if Buffer = nil then Exit;
     try
      {Get Length}
      Length:=(ASize shr 1);
      
      {Check Count}
      Count:=Unicode.WideCharToMultiByte(CP_ACP,0,PWideChar(Buffer),Length,nil,0,nil,nil);
      
      {$IFDEF CDFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('CDFSIdentifierToString - Size = ' + IntToStr(ASize) + ' Length = ' + IntToStr(Length) + ' Count = ' + IntToStr(Count));
      {$ENDIF}
      
      if Count <= Length then
       begin
        SetString(Result,nil,Count); {Count does not include null terminator}
        Unicode.WideCharToMultiByte(CP_ACP,0,PWideChar(Buffer),Length,PChar(Result),Count,nil,nil);
        {if Byte(Result[Count]) = 0 then SetLength(Result,Count - 1);} {Some CDs contain illegal null terminators}
        Terminator:=StrScan(PChar(Result),#0);
        if PtrUInt(Terminator) < (PtrUInt(Result) + LongWord(Count)) then
         begin
          {$IFDEF CDFS_DEBUG}
          if FILESYS_LOG_ENABLED then FileSysLogDebug('CDFSIdentifierToString - Length = ' + IntToStr(PtrUInt(Terminator) - PtrUInt(Result)));
          {$ENDIF}
          
          SetLength(Result,PtrUInt(Terminator) - PtrUInt(Result));
         end;
       end;
     finally
      FreeMem(Buffer);
     end;
    end
   else
    begin
     {$IFDEF CDFS_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('CDFSIdentifierToString - Size = ' + IntToStr(ASize));
     {$ENDIF}
     
     SetString(Result,nil,ASize); {Size does not include null terminator}
     Unicode.OemToCharBuff(PChar(@AIdentifier),PChar(Result),ASize);
     {if Byte(Result[ASize]) = 0 then SetLength(Result,ASize - 1);} {Some CDs contain illegal null terminators}
     Terminator:=StrScan(PChar(Result),#0);
     if PtrUInt(Terminator) < (PtrUInt(Result) + LongWord(ASize)) then
      begin
       {$IFDEF CDFS_DEBUG}
       if FILESYS_LOG_ENABLED then FileSysLogDebug('CDFSIdentifierToString - Length = ' + IntToStr(PtrUInt(Terminator) - PtrUInt(Result)));
       {$ENDIF}
       
       SetLength(Result,PtrUInt(Terminator) - PtrUInt(Result));
      end;
    end;
  end;
end;

{=============================================================================}

function CDFSStringToIdentifier(const AString:String;var AIdentifier;ASize:Integer;AUnicode:Boolean):Boolean;
{Converts the supplied string to an identifier}
{Accounts for unicode and byte ordering}
var
 Size:Integer;
 Count:Integer;
 Buffer:Pointer;
begin
 {}
 Result:=False;
 
 {Check Size}
 if ASize > 0 then
  begin
   {Check Unicode}
   if AUnicode then
    begin
     FillChar(AIdentifier,ASize,0);
     if Length(AString) > 0 then
      begin
       {Get Buffer}
       Buffer:=GetMem(Length(AString) shl 1);
       if Buffer = nil then Exit;
       try
        {Get Length}
        Size:=(ASize shr 1);
        
        {Check Count}
        Count:=Unicode.MultiByteToWideChar(CP_ACP,0,PChar(AString),Length(AString),nil,0);
        
        {$IFDEF CDFS_DEBUG}
        if FILESYS_LOG_ENABLED then FileSysLogDebug('CDFSStringToIdentifier - Size = ' + IntToStr(ASize) + ' Length = ' + IntToStr(Size) + ' Count = ' + IntToStr(Count));
        {$ENDIF}
        
        if Count > Size then Exit;
        if Unicode.MultiByteToWideChar(CP_ACP,0,PChar(AString),Length(AString),PWideChar(Buffer),Size) = 0 then Exit;
        if not CDFSPointerToData(Buffer,AIdentifier,ASize,True) then Exit;
       finally
        FreeMem(Buffer);
       end;
      end;
      
     Result:=True;
    end
   else
    begin
     {$IFDEF CDFS_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('CDFSStringToIdentifier - Size = ' + IntToStr(ASize));
     {$ENDIF}
     
     FillChar(AIdentifier,ASize,0);
     if Length(AString) > 0 then
      begin
       Unicode.CharToOemBuff(PChar(AString),PChar(@AIdentifier),ASize);
      end;
      
     Result:=True;
    end;
  end;
end;

{=============================================================================}

function CDFSIdentifierToFileName(const AIdentifier;ASize:Integer;AUnicode:Boolean):String;
{Converts the supplied identifier to a filename}
{Accounts for dot and dot dot special entries}
begin
 {}
 Result:=cdfsBlankName;
 
 {Check for Dots}
 if (ASize = 1) and (PByte(@AIdentifier)^ = cdfsDotIdentifier) then
  begin
   {$IFDEF CDFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('CDFSIdentifierToFileName - cdfsDotName');
   {$ENDIF}
   
   Result:=cdfsDotName;
  end
 else if (ASize = 1) and (PByte(@AIdentifier)^ = cdfsDotDotIdentifier) then
  begin
   {$IFDEF CDFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('CDFSIdentifierToFileName - cdfsDotDotName');
   {$ENDIF}
   
   Result:=cdfsDotDotName;
  end
 else
  begin
   Result:=CDFSIdentifierToString(AIdentifier,ASize,AUnicode);
  end;
end;

{=============================================================================}

function CDFSFileNameToIdentifier(const AFileName:String;var AIdentifier;ASize:Integer;AUnicode:Boolean):Boolean;
{Converts the supplied filename to an identifier}
{Accounts for dot and dot dot special entries}
begin
 {}
 Result:=False;
 
 {Check for Dots}
 if (Length(AFileName) = 1) and  (AFileName = cdfsDotName) then
  begin
   {$IFDEF CDFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('CDFSFileNameToIdentifier - cdfsDotName');
   {$ENDIF}
   
   FillChar(AIdentifier,ASize,0);
   PByte(@AIdentifier)^:=cdfsDotIdentifier;
   
   Result:=True;
  end
 else if (Length(AFileName) = 2) and (AFileName = cdfsDotDotName) then
  begin
   {$IFDEF CDFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('CDFSFileNameToIdentifier - cdfsDotDotName');
   {$ENDIF}
   
   FillChar(AIdentifier,ASize,0);
   PByte(@AIdentifier)^:=cdfsDotDotIdentifier;
   
   Result:=True;
  end
 else
  begin
   Result:=CDFSStringToIdentifier(AFileName,AIdentifier,ASize,AUnicode);
  end;
end;

{=============================================================================}

function CDFSTimeToFileTime(const ATime:TCDFSTime):TFileTime;
{Converts a CDFS Time in Local to a File Time in UTC}
var
 Offset:Integer;
 FileTime:TFileTime;
 SystemTime:_SYSTEMTIME;
begin
 {}
 Int64(Result):=0;
 
 {Get System Time}
 SystemTime.wYear:=cdfsTimeStartYear + ATime.Years;
 SystemTime.wMonth:=ATime.Month;
 SystemTime.wDayOfWeek:=0;
 SystemTime.wDay:=ATime.Day;
 SystemTime.wHour:=ATime.Hour;
 SystemTime.wMinute:=ATime.Minute;
 SystemTime.wSecond:=ATime.Second;
 SystemTime.wMilliseconds:=0;
 
 {Convert to File Time}
 if Ultibo.SystemTimeToFileTime(SystemTime,FileTime) then
  begin
   {Convert to UTC}
   Offset:=(ATime.Offset * cdfsTimeOffsetInterval);
   
   Result:=ConvertFileTime(FileTime,Offset,False); {False due to inverse Offset value (eg AEST is +600 not -600}
  end;
end;

{=============================================================================}

function FileTimeToCDFSTime(const AFileTime:TFileTime;var ATime:TCDFSTime):Boolean;
{Converts a File Time in UTC to a CDFS Time in Local}
var
 Offset:Integer;
 SystemTime:_SYSTEMTIME;
 LocalFileTime:TFileTime;
begin
 {}
 Result:=False;
 
 FillChar(ATime,SizeOf(TCDFSTime),0);
 
 if Int64(AFileTime) = 0 then
  begin
   {Return 0}
   Result:=True;
  end
 else
  begin
   {Convert to UTC}
   if Ultibo.FileTimeToLocalFileTime(AFileTime,LocalFileTime) then
    begin
     {Convert to System Time}
     if Ultibo.FileTimeToSystemTime(LocalFileTime,SystemTime) then
      begin
       Offset:=(0 - GetTimezoneActiveOffset) div cdfsTimeOffsetInterval;
       ATime.Years:=SystemTime.wYear - cdfsTimeStartYear;
       ATime.Month:=SystemTime.wMonth;
       ATime.Day:=SystemTime.wDay;
       ATime.Hour:=SystemTime.wHour;
       ATime.Minute:=SystemTime.wMinute;
       ATime.Second:=SystemTime.wSecond;
       ATime.Offset:=Offset;
       
       Result:=True;
      end;
    end;
  end;
end;

{==============================================================================}

function CDFSDateTimeToFileTime(const ADateTime:TCDFSDateTime):TFileTime;
{Converts a CDFS Date Time in Local to a File Time in UTC}
var
 Offset:Integer;
 FileTime:TFileTime;
 SystemTime:_SYSTEMTIME;
begin
 {}
 Int64(Result):=0;
 
 {Get System Time}
 SystemTime.wYear:=StrToIntDef(ADateTime.Year,cdfsTimeStartYear);
 SystemTime.wMonth:=StrToIntDef(ADateTime.Month,0);
 SystemTime.wDay:=StrToIntDef(ADateTime.Day,0);
 SystemTime.wHour:=StrToIntDef(ADateTime.Hour,0);
 SystemTime.wMinute:=StrToIntDef(ADateTime.Minute,0);
 SystemTime.wSecond:=StrToIntDef(ADateTime.Second,0);
 SystemTime.wMilliseconds:=StrToIntDef(ADateTime.Hundredths,0) * 10;
 
 {Convert to File Time}
 if Ultibo.SystemTimeToFileTime(SystemTime,FileTime) then
  begin
   {Convert to UTC}
   Offset:=(ADateTime.Offset * cdfsTimeOffsetInterval);
   
   Result:=ConvertFileTime(FileTime,Offset,False); {False due to inverse Offset value (eg AEST is +600 not -600}
  end;
end;

{=============================================================================}

function FileTimeToCDFSDateTime(const AFileTime:TFileTime;var ADateTime:TCDFSDateTime):Boolean;
{Converts a File Time in UTC to a CDFS Date Time in Local}
var
 Offset:Integer;
 SystemTime:_SYSTEMTIME;
 LocalFileTime:TFileTime;
begin
 {}
 Result:=False;
 
 FillChar(ADateTime,SizeOf(TCDFSDateTime),0);
 
 if Int64(AFileTime) = 0 then
  begin
   {Return 0}
   Result:=True;
  end
 else
  begin
   {Convert to UTC}
   if Ultibo.FileTimeToLocalFileTime(AFileTime,LocalFileTime) then
    begin
     {Convert to System Time}
     if Ultibo.FileTimeToSystemTime(LocalFileTime,SystemTime) then
      begin
       Offset:=(0 - GetTimezoneActiveOffset) div cdfsTimeOffsetInterval;
       StrLCopy(ADateTime.Year,PChar(IntToStrLen(SystemTime.wYear,4)),4);
       StrLCopy(ADateTime.Month,PChar(IntToStrLen(SystemTime.wMonth,2)),2);
       StrLCopy(ADateTime.Day,PChar(IntToStrLen(SystemTime.wDay,2)),2);
       StrLCopy(ADateTime.Hour,PChar(IntToStrLen(SystemTime.wHour,2)),2);
       StrLCopy(ADateTime.Minute,PChar(IntToStrLen(SystemTime.wMinute,2)),2);
       StrLCopy(ADateTime.Second,PChar(IntToStrLen(SystemTime.wSecond,2)),2);
       StrLCopy(ADateTime.Hundredths,PChar(IntToStrLen(SystemTime.wMilliseconds div 10,2)),2);
       ADateTime.Offset:=Offset;
       
       Result:=True;
      end;
    end;
  end;
end;

{==============================================================================}
{==============================================================================}

initialization
 CDFSInit;

{==============================================================================}
 
finalization
 CDFSQuit;

{==============================================================================}
{==============================================================================}

end.



