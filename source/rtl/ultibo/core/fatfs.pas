{
Ultibo FAT12/16/32/exFAT interface unit.

Copyright (C) 2025 - SoftOz Pty Ltd.

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


FAT FileSystem
==============

 Notes: Based on information from numerous sources,
        primarily the document fatgen103.pdf from
        Microsoft.

        FAT Disk Blocks are based on reading a group of
        sectors into a block at once. The BlockNo of each
        block is therefore the FAT entry no of the first
        FAT in the block.

 Notes: The use of IsEightDotThree and GenerateShortName(Ex) from
        the UltiboUtils unit should be replaced by internal routines
        optimised for maximum performance

        All Block and Entry Offsets have been expanded to
        32bit to avoid 16bit overflow on 64K clusters

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

{$IFNDEF FPC_DOTTEDUNITS}
unit FATFS;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Core.GlobalConfig,
  Core.GlobalConst,
  Core.GlobalTypes,
  Core.Platform,
  Core.Threads,
  Core.FileSystem,
  System.SysUtils,
  System.Classes,
  Core.Unicode,
  Core.Ultibo,
  Core.UltiboUtils,
  Core.UltiboClasses;
{$ELSE FPC_DOTTEDUNITS}
uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  FileSystem,
  SysUtils,
  Classes,
  Unicode,
  Ultibo,
  UltiboUtils,
  UltiboClasses;
{$ENDIF FPC_DOTTEDUNITS}

//To Do //How to protect AllocCluster/ReleaseCluster ? (ClusterLock ? / FBlocks !)

//To Do //Change AllocDirectory/ReleaseDirectory to use an FDirectoryBuffer and DirectoryLock/Unlock ?

//To Do //What should SetEntry/SetLong use ? (FDirectoryBuffer and DirectoryLock/Unlock ?) (FEntryBuffer and EntryLock/Unlock ?)

//To Do //Look for:

//LongWord(Pointer()^) -> PLongWord()^
//Word(Pointer()^) -> PWord()^

//Critical

//Int64

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {FATFS specific constants}
 fatFAT12 = 0;
 fatFAT16 = 1;
 fatFAT32 = 2;

 fatNames:array[0..2] of String = (
  'FAT12',
  'FAT16',
  'FAT32');

 fatDotName     = '.';
 fatDotDotName  = '..';
 fatBlankName   = '';
 fatNameCount   = 8;
 fatExtCount    = 3;

 fatLabelMask   = '*';
 fatLabelCount  = 11;

 fatOemName     = 'MSWIN4.1';
 fatDefaultName = 'NO NAME';

 fatFlagNone = $00;
 fatFlagName = $08;
 fatFlagExt  = $10;
 fatFlagBoth = $18;

 lfnMaxName = 2048;      {Buffer size for lfn handling}
 lfnMaxAlias = 999999;   {Max number of lfn aliases (ie ~999999)}
 lfnMaxNameAlias = 4;
 lfnMaxHashAlias = 999;
 lfnMaxHashShift = 16;

 fatMaxFileSize = 4294967295;    {Max size in bytes of a File (4GB)}
 fatMaxDirectorySize = 2097152;  {Max size in bytes of a Directory (2MB = 65536 * 32)}

 fatMinClusterSize = 512;
 fatMaxClusterSize = 65536;

 fat12MinClusters = 3;
 fat16MinClusters = 4085;   {Smallest Number of Clusters on a FAT16 volume, is FAT12 if less}
 fat32MinClusters = 65525;  {Smallest Number of Clusters on a FAT32 volume, is FAT16 if less}

 {fat12MinSectors = 6;}       {Number of Sectors at which we cannot create a FAT12}{Not possible due to 512 Root entries}
 fat12MinSectors = 798;       {Number of Sectors at which we cannot create a FAT12}
 fat12MaxSectors = 32623;     {Number of Sectors at which we insist on creating FAT16}

 fat16MinSectors = 8400;      {Number of Sectors at which we cannot create a FAT16}
 fat16MaxSectors = 1048576;   {Number of Sectors at which we insist on creating FAT32}

 fat32MinSectors = 66600;     {Number of Sectors at which we cannot create a FAT32}
 fat32MaxSectors = $FFFFFFFF; {No limit on maximum size of a FAT32}

                              {Note: In FAT12 EOC will be >= $0FF0 due to Media ID on Floppy}
 fat12EndOfFile = $0FF0;      {Some Code sets values from $0FF0 - $0FFF as EOF}
 {fat12EndOfFile = $0FF8;}    {Some Code sets values from $0FF8 - $0FFF as EOF}
 fat12EndOfCluster = $0FFF;   {EOF Mark, Check for >= $0FF0 - See Above}
 fat12FreeCluster = $0000;    {Available for Use}
 fat12BadCluster = $0FF7;     {Bad Cluster Mark - Do Not Use}
 fat12MediaCluster = $0F00;   {First Cluster - $0F00 + MediaID from BPB Usually $0FF8}
 fat12Reserved0 = $0FF0;      {Reserved Cluster Nos - Do Not Use (Due to Media ID on Floppy)}
 fat12Reserved1 = $0FF1;
 fat12Reserved2 = $0FF2;
 fat12Reserved3 = $0FF3;
 fat12Reserved4 = $0FF4;
 fat12Reserved5 = $0FF5;
 fat12Reserved6 = $0FF6;

 fat16EndOfFile = $FFF8;      {Some Code sets values from $FFF8 - $FFFF as EOF}
 fat16EndOfCluster = $FFFF;   {EOF Mark, Check for >= $FFF8 - See Above}
 fat16FreeCluster = $0000;    {Available for Use}
 fat16BadCluster = $FFF7;     {Bad Cluster Mark - Do Not Use}
 fat16MediaCluster = $FF00;   {First Cluster - $FF00 + MediaID from BPB Usually $FFF8}

 fat32EndOfFile = $0FFFFFF8;    {Some Code sets values from $0FFFFFF8 - $0FFFFFFF as EOF}
 fat32EndOfCluster = $0FFFFFFF; {EOF Mark, Check for >= $0FFFFFF8 - See Above}
 fat32FreeCluster = $00000000;  {Available for Use}
 fat32BadCluster = $0FFFFFF7;   {Bad Cluster Mark - Do Not Use}
 fat32MediaCluster = $0FFFFF00; {First Cluster - $0FFFFF00 + MediaID from BPB Usually $0FFFFFF8}

 fat16CleanShutdown = $8000; {Second Cluster - EndOfCluster + Bits}
 fat16HardError = $4000;     {Second Cluster - EndOfCluster + Bits}

 fat32CleanShutdown = $08000000; {Second Cluster - EndOfCluster + Bits}
 fat32HardError = $04000000;     {Second Cluster - EndOfCluster + Bits}
 fat32ReservedBits = $F0000000;  {Bits 31-29 must be preserved on FAT32}

 fatBootSector = 0;
 fatFreeCluster = 0;
 fatStartCluster = 2;            {Lowest allocatable cluster number}
 fatUnevenCluster = $00000001;
 fatUnknownCluster = $FFFFFFFF;

 fatEntryFree    = $E5;
 fatEntryFreeAll = $00;
 fatEntrySpecial = $05;  {If real first char is $E5}
 fatEntryPadding = $20;  {Padded with spaces (32)}

 fatEntrySize = 32;      {Size of a FAT Directory Entry (or LFNDirectoryEntry)}

 lfnEntryMax   = $3F;  {Maximum count of an LFN entry}
 lfnEntryLast  = $40;  {Marker for LFN last entry}
 lfnEntryMask  = $BF;  {Mask for LFN entry count} //To Do //Use Max (3F) instead ?? No ?
 lfnEntryChars = 13;   {Number of characters per LFN entry}

 lfnEntryNull    = $0000;  {Null terminator on LFN when applicable}
 lfnEntryPadding = $FFFF;  {Empty LFN chars padded with (65535)}

 lfnAttributeMask = (faReadOnly or faHidden or faSysFile or faVolumeID); {Not Used - See faLongName}

 fatBootSignature = $29;

 fat32LeadSignature   = $41615252; {RRaA}
 fat32StructSignature = $61417272; {rrAa}
 fat32TrailSignature  = $AA550000; {Standard Boot Sector signature}

 fatDotChar:Byte = $2E; {Checked separately from Invalid chars due to . and .. entries}
 fatSpaceChar:Byte = $20;
 fatBaseChars:set of Byte = [$00..$19]; {0..31} {Dont include $20 as that is checked separately}
 fatInvalidChars:set of Byte = [$22,$2A,$2B,$2C,$2F,$3A,$3B,$3C,$3D,$3E,$3F,$5B,$5C,$5D,$7C]; {34,42,43,44,47,58,59,60,61,62,63,91,92,93,124} {Dont include $2E as that is checked separately}
 fatLowercaseChars:set of Byte = [$61..$7A]; {97..122} {These dont account for the code page}

{==============================================================================}
type
 {FATFS specific types}
 TFATType = (ftNONE,ftFAT12,ftFAT16,ftFAT32);

 TFATParams = record {Not Packed} {Block values for various Sector sizes}
  SectorSize:Word;
  FATType:TFATType;
  BlockShiftCount:Word;
  EntriesPerBlock:LongWord;
  SectorsPerBlock:LongWord;
 end;

 TFATGeometry = record {Not Packed} {FAT12 values for various Floppy Types}
  FloppyType:TFloppyType;
  SectorsPerFat:LongWord;
  SectorsPerCluster:LongWord;
  RootEntryCount:LongWord;
  MediaId:Byte;
 end;

 TFATClusterSize = record {Not Packed} {Sectors per Cluster for various disk sizes (Fixed Disk only)}
  //To Do //Add SectorSize to allow for up to 4K sectors
  SectorCount:LongWord;                {Based on 512 bytes per sector only}
  SectorsPerCluster:LongWord;
 end;

 TFATPartitionType = record {Not Packed} {Partition Type for various disk sizes (Fixed Disk only)}
  Excluded:Boolean;                      {Entry is excluded from FAT Partition Type check}
  Extended:Boolean;
  //To Do //Add SectorSize to allow for up to 4K sectors
  SectorCount:LongWord;                  {Based on 512 bytes per sector only}
  PartitionId:Byte;
  LBAType:Byte;
 end;

 PFATName = ^TFATName;
 TFATName = packed record  {11 Bytes}
  Name:array[0..7] of Char; {File Name}
  Ext:array[0..2] of Char;  {File Extension}
 end;

 PFATDirectory = ^TFATDirectory;
 TFATDirectory = packed record  {32 Bytes}
  Name:array[0..7] of Char; {File Name}
  Ext:array[0..2] of Char;  {File Extension}
  Attribute:Byte;           {File Attributes - See Standard Values}
  {Reserved:Byte;}          {Always 0} {See below}
  CaseFlags:Byte;           {Set to $08 if Name, $10 if Extension or $18 if both}
  CreateTimeMsecs:Byte;
  CreateTime:Word;
  CreateDate:Word;
  LastAccessDate:Word;
  FirstClusterHigh:Word;    {High word of the start cluster (FAT32 Only)}
  WriteTime:Word;
  WriteDate:Word;
  FirstClusterLow:Word;     {Low Word of Start Cluster all versions}
  Length:LongWord;
 end;

 PLFNDirectory = ^TLFNDirectory;
 TLFNDirectory = packed record  {32 Bytes}
  Order:Byte;                      {Bit 6 = 1 if last LFN in chain, Bits 5-0 = Order of LFN chunks for this File}
  Name1:array[0..4] of WideChar;   {First 5 Characters in this chunk}
  Attribute:Byte;                  {Always 0Fh - Volume, System, Hidden, ReadOnly}
  FileType:Byte;                   {Usually 00h}
  Checksum:Byte;                   {Checksum calculated on 8.3 name}
  Name2:array[0..5] of WideChar;   {Next 6 Characters in this chunk}
  Reserved:Word;                   {Always 0000h - FirstCluster in Standard Entry}
  Name3:array[0..1] of WideChar;   {Next 2 Characters in this chunk}
 end;

 PFATInfoSector = ^TFATInfoSector;
 TFATInfoSector = packed record
  LeadSignature:LongWord;          {Signature "RRaA" or 41615252h}
  Reserved1:array[0..479] of Byte; {Always 0}
  StructureSignature:LongWord;     {Signature "rrAa" or 61417272h}
  FreeClusterCount:LongWord;       {Free Clusters $FFFFFFFF if unknown}
  LastFreeCluster:LongWord;        {Most recently allocated cluster $FFFFFFFF if unknown}
  Reserved2:array[0..11] of Byte;  {Always 0}
  TrailSignature:LongWord;         {Signature AA550000h}
 end;

{==============================================================================}
type
 {FATFS specific classes}
 TFATRecognizer = class(TRecognizer)
   constructor Create(ADriver:TFileSysDriver);
  private
   {Private Variables}
   FCaseFlags:Boolean;
   FLongNames:Boolean;
   FOemConvert:Boolean;
   FNumericTail:Boolean;

   FInfoSectorEnable:Boolean;
   FInfoImmediateUpdate:Boolean;

   {Private Methods}
   function CheckLBA:Boolean;
   function CheckFAT32:Boolean;
   function CheckBootSector(ABootSector:PBootSector;const AStartSector:Int64;ASectorCount:LongWord):Boolean;
  protected
   {Protected Variables}

   {Protected Methods}
   function GetName:String; override;
  public
   {Public Variables}
   property CaseFlags:Boolean read FCaseFlags write FCaseFlags;
   property LongNames:Boolean read FLongNames write FLongNames;
   property OemConvert:Boolean read FOemConvert write FOemConvert;
   property NumericTail:Boolean read FNumericTail write FNumericTail;

   property InfoSectorEnable:Boolean read FInfoSectorEnable write FInfoSectorEnable;
   property InfoImmediateUpdate:Boolean read FInfoImmediateUpdate write FInfoImmediateUpdate;

   {Public Methods}
   function RecognizePartitionId(APartitionId:Byte):Boolean; override;
   function RecognizeBootSector(ABootSector:PBootSector;const AStartSector,ASectorCount:Int64):Boolean; override;

   function RecognizePartition(APartition:TDiskPartition):Boolean; override;
   function RecognizeVolume(AVolume:TDiskVolume):Boolean; override;
   function MountVolume(AVolume:TDiskVolume;ADrive:TDiskDrive):Boolean; override;
 end;

 TFATPartitioner = class(TDiskPartitioner)
   constructor Create(ADriver:TFileSysDriver;ARecognizer:TRecognizer);
  private
   {Private Variables}

   {Private Methods}
  protected
   {Protected Variables}

   {Protected Methods}
   function CheckLogical(ADevice:TDiskDevice;AParent:TDiskPartition;APartitionId:Byte):Boolean; override;
   function CheckExtended(ADevice:TDiskDevice;AParent:TDiskPartition;APartitionId:Byte):Boolean; override;

   function GetPartitionId(ADevice:TDiskDevice;AParent:TDiskPartition;AStart,ACount:LongWord;APartitionId:Byte):Byte; override;

   function InitPartition(ADevice:TDiskDevice;AParent:TDiskPartition;AStart,ACount:LongWord;APartitionId:Byte):Boolean; override;
  public
   {Public Variables}

   {Public Methods}
   function AcceptPartition(ADevice:TDiskDevice;APartition,AParent:TDiskPartition;APartitionId:Byte):Boolean; override;
 end;

 TFATFormatter = class(TDiskFormatter)
  private
   {Private Variables}
   //To Do //Create a list of FormatHandles to allow Callback and multiple simultaneous formats.
           //The handle would contain all the variables of the formatting operation in progress

   {Private Methods}
   function CheckDevice(AVolume:TDiskVolume;ADrive:TDiskDrive;AFloppyType:TFloppyType):Boolean;
   function CheckPartition(AVolume:TDiskVolume;ADrive:TDiskDrive;AFileSysType:TFileSysType):Boolean;

   function GetPartitionId(AVolume:TDiskVolume;ADrive:TDiskDrive;AFATType:TFATType):Byte;
   function UpdatePartitionId(AVolume:TDiskVolume;ADrive:TDiskDrive;AFATType:TFATType):Boolean;

   function GetSectorsPerCluster(AVolume:TDiskVolume;ADrive:TDiskDrive;AFloppyType:TFloppyType;AFileSysType:TFileSysType;ABootSector:PBootSector;var AFATType:TFATType):LongWord;

   function GetSectorsPerFat(AVolume:TDiskVolume;ADrive:TDiskDrive;AFATType:TFATType;ABootSector:PBootSector):LongWord;

   function CreateBootSector(AVolume:TDiskVolume;ADrive:TDiskDrive;AFATType:TFATType;ABootSector:PBootSector):Boolean;
   function WriteBootSector(AVolume:TDiskVolume;ADrive:TDiskDrive;AFATType:TFATType;ABootSector:PBootSector):Boolean;
   function WriteFatTable(AVolume:TDiskVolume;ADrive:TDiskDrive;AFATType:TFATType;ABootSector:PBootSector):Boolean;
   function WriteRootDirectory(AVolume:TDiskVolume;ADrive:TDiskDrive;AFATType:TFATType;ABootSector:PBootSector):Boolean;

   function CreateInfoSector(AVolume:TDiskVolume;ADrive:TDiskDrive;AFATType:TFATType;ABootSector:PBootSector;AInfoSector:PFATInfoSector):Boolean;
   function WriteInfoSector(AVolume:TDiskVolume;ADrive:TDiskDrive;AFATType:TFATType;ABootSector:PBootSector;AInfoSector:PFATInfoSector):Boolean;
  public
   {Public Variables}

   {Public Methods}
   function AcceptVolume(AVolume:TDiskVolume;AFloppyType:TFloppyType;AFileSysType:TFileSysType):Boolean; override;
   function FormatVolume(AVolume:TDiskVolume;AFloppyType:TFloppyType;AFileSysType:TFileSysType):Boolean; override;
 end;

 TFATDefragger = class(TDiskDefragger)
  private
   {Private Variables}

   {Private Methods}
  public
   {Public Variables}

   {Public Methods}
 end;

 TFATRepairer = class(TDiskRepairer)
  private
   {Private Variables}
   //To Do //This could make good use of a Btree to verify cluster allocation and cross link etc
   {Private Methods}
  public
   {Public Variables}

   {Public Methods}
 end;

 TFATDiskTable = class;
 TFATDiskBlock = class;
 TFATDiskEntry = class;
 TFATFileSystem = class(TFileSystem)
   constructor Create(ADriver:TFileSysDriver;AVolume:TDiskVolume;ADrive:TDiskDrive);
   destructor Destroy; override;
  private
   {Private Variables}
   FFATType:TFATType;
   FCaseFlags:Boolean;       {Allow writing entries with the Lowercase Flags set (Reading is always supported)}
   FVolumeFlags:LongWord;    {Volume Dirty/Error flags}

   FInfoSectorEnable:Boolean;    {Enable use of FAT32 info sector for free cluster count and next free cluster}
   FInfoImmediateUpdate:Boolean; {Enable immediate update of FAT32 info sector on allocate or release of clusters}

   {FAT Markers}
   FEndOfFile:LongWord;
   FEndOfCluster:LongWord;
   FFreeCluster:LongWord;
   FBadCluster:LongWord;
   FMediaCluster:LongWord;
   FStartCluster:LongWord;

   {FAT Masks}
   FHardError:LongWord;
   FCleanShutdown:LongWord;
   FReservedBits:LongWord;

   {FAT Variables}
   FNumberOfFats:Word;           {Usually 2}
   FSectorsPerFat:LongWord;
   FSectorsPerCluster:LongWord;  {Usually 1,2,4,8,16,32,64,128 etc}

   FReservedSectors:LongWord;

   {FAT12/16 Variables}
   FRootEntryCount:LongWord;     {Number of Directory entries in Root Directory}
   FRootSectorCount:LongWord;    {Number of Sectors occupied by Root Directory}
   FRootStartSector:LongWord;    {First Sector of Root Directory (Relative to StartSector)}

   {FAT32 Variables}
   FInfoSector:LongWord;         {Relative to StartSector}
   FInfoBackup:LongWord;         {Relative to StartSector}

   FRootStartCluster:LongWord;   {First Cluster of Root Directory}

   {FAT Variables}
   FActiveFat:Word;              {Zero based number of active FAT (Always 0 for FAT12/16)}
   FFatMirroring:Boolean;        {FAT Mirroring Enabled if True (Always True for FAT12/16)}

   FEntriesPerSector:LongWord;   {Number of Directory entries in a Sector}
   FEntriesPerCluster:LongWord;  {Number of Directory entries in a Cluster}

   FEntriesPerBlock:LongWord;    {Number of FAT entries per Block}
   FSectorsPerBlock:LongWord;    {Number of Sectors per Block of FAT entries}

   FBlockShiftCount:Word;        {Shift count for Cluster <-> BlockNo}
   FSectorShiftCount:Word;       {Shift count for Sector <-> Cluster}
   FClusterShiftCount:Word;      {Shift count for Cluster <-> Bytes}

   FDataStartSector:LongWord;    {First Sector of First Data Cluster (Relative to StartSector)}
   FDataClusterCount:LongWord;   {Number of usable data clusters}
   FTotalClusterCount:LongWord;  {Total number of clusters including reserved clusters}

   FLastFreeCluster:LongWord;    {Or $FFFFFFFF if not known}
   FFreeClusterCount:LongWord;   {Or $FFFFFFFF if not known}

   FClusterSize:LongWord;        {Size of a Cluster in Bytes (Max 65536 > Word)}

   FInfoBuffer:Pointer;          {Buffer for info sector handling (Sector size)}
   FInfoLock:TMutexHandle;       {Lock for info buffer}

   FNameBuffer:Pointer;          {Buffer for long name handling}
   FNameLock:TMutexHandle;       {Lock for name buffer}

   FReadBuffer:Pointer;          {Buffer for partial cluster entry reads (Cluster size)}
   FReadLock:TMutexHandle;       {Lock for read buffer}

   FWriteBuffer:Pointer;         {Buffer for partial cluster entry writes (Cluster size)}
   FWriteLock:TMutexHandle;      {Lock for write buffer}

   FClusterBuffer:Pointer;       {Buffer of exactly cluster size}
   FClusterLock:TMutexHandle;    {Lock for cluster buffer}

   {Private Methods}
   function InfoLock:Boolean;
   function InfoUnlock:Boolean;

   function NameLock:Boolean;
   function NameUnlock:Boolean;

   function ReadLock:Boolean;
   function ReadUnlock:Boolean;

   function WriteLock:Boolean;
   function WriteUnlock:Boolean;

   function ClusterLock:Boolean;
   function ClusterUnlock:Boolean;

   function IsRemovable:Boolean;

   {Flag Methods}
   function GetHardError:Boolean;
   procedure SetHardError(AValue:Boolean);

   function GetCleanShutdown:Boolean;
   procedure SetCleanShutdown(AValue:Boolean);

   function GetVolumeFlags:LongWord;
   function SetVolumeFlags(AFlags:LongWord):Boolean;

   {Sector Methods}
   function UpdateInfoSector:Boolean;

   {Cluster Methods}
   function FillCluster(ACluster:LongWord;AValue:Byte):Boolean;

   function ReadCluster(ACluster:LongWord;var ABuffer):Boolean;
   function WriteCluster(ACluster:LongWord;const ABuffer):Boolean;

   function GetNextFreeCluster:LongWord;
   function GetFreeClusterCount:LongWord;

   function SetNextFreeCluster(ACluster:LongWord):Boolean;
   function SetFreeClusterCount(ACount:LongWord):Boolean;

   function GetStartCluster(AEntry:TDiskEntry):LongWord;
   function GetParentCluster(AParent:TDiskEntry):LongWord;
   function CheckClusterBlock(ACluster,ANext:LongWord):Boolean;

   function GetNextChainCluster(AParent:LongWord):LongWord;
   function GetLastChainCluster(AParent:LongWord):LongWord;
   //function GetOffsetChainCluster(AParent,AOffset:LongWord):LongWord; //To Do - Use for ReadEntry/WriteEntry
   function GetChainClusterCount(AParent:LongWord):LongWord;

   function GetCluster(ACluster:LongWord):LongWord;
   function SetCluster(ACluster,AValue:LongWord;ACommit:Boolean):Boolean;

   function AllocCluster(AParent:LongWord;var ACluster:LongWord;ACount:LongWord):Boolean;
   function ReleaseCluster(AParent,ACluster:LongWord):Boolean;

   {Directory Methods}
   function CheckDirectoryRoot(AParent:TDiskEntry):Boolean;
   function CheckDirectoryStart(AParent:TDiskEntry):Boolean;

   function GetFirstDirectorySector(AParent:TDiskEntry;var ASector:LongWord):Boolean;
   function GetNextDirectorySector(AParent:TDiskEntry;var ASector:LongWord;AWrite:Boolean):Boolean;
   function GetDirectorySectorCount(AParent:TDiskEntry;AWrite:Boolean):LongWord;
   function GetDirectorySectorOffset(AParent:TDiskEntry):LongWord;

   function AllocDirectory(AParent:TDiskEntry;ACount:Byte;var AOffset,ASector:LongWord):Boolean;
   function ReleaseDirectory(AParent:TDiskEntry;ACount:Byte;AOffset,ASector:LongWord):Boolean;

   {Misc Methods}
   function GetBlockShiftCount(ASize:Word;AType:TFATType):Word;
   function GetSectorShiftCount(ASectorsPerCluster:LongWord):Word;
   function GetClusterShiftCount(AClusterSize:LongWord):Word;

   function GetEntriesPerBlock(ASize:Word;AType:TFATType):LongWord;
   function GetSectorsPerBlock(ASize:Word;AType:TFATType):LongWord;

   {Conversion Methods}
   procedure ReadConvert(ADirectory:PFATDirectory);
   procedure WriteConvert(ADirectory:PFATDirectory);

   function NameToEntry(AName:Pointer;AEntry:TFATDiskEntry;AShort:Boolean):Boolean;
   function EntryToName(AEntry:TFATDiskEntry;AName:Pointer;AShort:Boolean):Boolean;

   function BufferToName(ABuffer:Pointer;var AName:String):Boolean;
   function NameToBuffer(const AName:String;ABuffer:Pointer):Boolean;

   function DirectoryToBuffer(ADirectory,ABuffer:Pointer;ACount,AChecksum:Byte;ALast:Boolean):Boolean;
   function BufferToDirectory(ABuffer,ADirectory:Pointer;ACount,AChecksum:Byte;ALast:Boolean):Boolean;

   function DirectoryToEntry(ADirectory:Pointer;AEntry:TFATDiskEntry;AShort:Boolean):Boolean;
   function EntryToDirectory(AEntry:TFATDiskEntry;ADirectory:Pointer;AShort:Boolean):Boolean;

   function FATTypeToFileSysType(AFATType:TFATType):TFileSysType;
  protected
   {Protected Variables}

   {Protected Methods}
   function LoadMaxFile:Integer; override;
   function LoadMaxPath:Integer; override;
   function LoadAttributes:LongWord; override;
   function LoadMaxAttributes:LongWord; override;
   function LoadMinFileTime:TFileTime; override;
   function LoadSystemName:String; override;
   function LoadVolumeName:String; override;
   function LoadVolumeSerial:LongWord; override;
   function LoadFileSysType:TFileSysType; override;

   function SetVolumeName(const AName:String):Boolean;
   function SetVolumeSerial(ASerial:LongWord):Boolean;

   function ReadEntry(AParent,AEntry:TDiskEntry;var ABuffer;const AStart:Int64;ACount:LongWord;var AOffset,AValue:LongWord):Integer; override;
   function WriteEntry(AParent,AEntry:TDiskEntry;const ABuffer;const AStart:Int64;ACount:LongWord;var AOffset,AValue:LongWord):Integer; override;

   function LoadTables:Boolean; override;
   function LoadBlocks:Boolean; override;
   function LoadEntries(AParent:TDiskEntry):Boolean; override;

   function LoadTable(ATableNo:LongWord):Boolean; override;
   function LoadBlock(ABlockNo:LongWord):Boolean; override;
   function LoadEntry(AParent:TDiskEntry;ABuffer:Pointer;var ABlockOffset,AEntryOffset,ABlockSector,AEntrySector:LongWord):Boolean; {Not override}
   function LoadLong(AParent:TDiskEntry;ABuffer:Pointer;var ABlockOffset,AEntryOffset,ABlockSector,AEntrySector:LongWord):Boolean; {Not override}

   function AddEntry(AParent:TDiskEntry;const AName:String;AAttributes:LongWord;AReference:Boolean):TDiskEntry; override;
   function AddEntryEx(AParent:TDiskEntry;const AName,AAltName:String;AAttributes:LongWord;AReference:Boolean):TDiskEntry; override;
   function RemoveEntry(AParent,AEntry:TDiskEntry):Boolean; override;
   function RenameEntry(AParent,AEntry:TDiskEntry;const AName:String):Boolean; override;
   function RenameEntryEx(AParent,AEntry:TDiskEntry;const AAltName:String):Boolean; override;
   function MoveEntry(ASource,ADest,AEntry:TDiskEntry):Boolean; override;

   function SetBlock(ABlock:TDiskBlock):Boolean; override;
   function SetEntry(AParent,AEntry:TDiskEntry):Boolean; override;
   function SetLong(AParent,AEntry:TDiskEntry):Boolean; {Not override}

   function SizeEntry(AParent,AEntry:TDiskEntry;const ASize:Int64):Boolean; override;

   function GetBlock(ABlockNo:LongWord):TDiskBlock; override;
   function GetBlockEx(ABlockNo:LongWord;AWrite:Boolean):TDiskBlock; override;

   function CheckName(const AName:String):Boolean; override;
   function CountName(const AName:String):Byte;
   function CompareName(const AName,AMatch:String;AWildcard:Boolean):Boolean; override;
   function ChecksumName(AEntry:TDiskEntry):Byte;
   function GenerateName(AParent,AEntry:TDiskEntry;const AName:String):String;

   function GetNameFlags(const AName:String):LongWord;
   function CheckFlagName(const AName:String):Boolean;

   function ValidateName(AName:Pointer):Boolean;
   function ValidateDirectory(ADirectory:Pointer):Boolean;
  public
   {Public Variables}
   property CaseFlags:Boolean read FCaseFlags write FCaseFlags;
   property OemConvert:Boolean read FOemConvert write FOemConvert;
   property NumericTail:Boolean read FNumericTail write FNumericTail;

   property ReadOnly:Boolean read FReadOnly write FReadOnly;
   property LongNames:Boolean read FLongNames write FLongNames;
   property CasePreserved:Boolean read FCasePreserved write FCasePreserved;
   property UnicodeNames:Boolean read FUnicodeNames write FUnicodeNames;

   property InfoSectorEnable:Boolean read FInfoSectorEnable write FInfoSectorEnable;
   property InfoImmediateUpdate:Boolean read FInfoImmediateUpdate write FInfoImmediateUpdate;

   property HardError:Boolean read GetHardError write SetHardError;
   property CleanShutdown:Boolean read GetCleanShutdown write SetCleanShutdown;

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

 TFATDiskTable = class(TDiskTable)  {Represents a FAT table}
  private
   {Private Variables}
   FStartSector:LongWord;
   FSectorCount:LongWord;
  public
   {Public Variables}
   property StartSector:LongWord read FStartSector write FStartSector;
   property SectorCount:LongWord read FSectorCount write FSectorCount;
 end;

 TFATDiskBlock = class(TDiskBlock)  {Represents a block of FAT entries}
   constructor Create(ALocalLock:TMutexHandle);
   destructor Destroy; override;
  private
   {Private Variables}
   FBlockBuffer:Pointer;
   FBlockSector:LongWord; {Relative to the StartSector in Table}
   FSectorCount:LongWord;
  public
   {Public Variables}
   property BlockBuffer:Pointer read FBlockBuffer write FBlockBuffer;
   property BlockSector:LongWord read FBlockSector write FBlockSector;
   property SectorCount:LongWord read FSectorCount write FSectorCount;
 end;

 TFATDiskEntry = class(TDiskEntry)  {Represents a FAT directory entry}
  private
   {Private Variables}
   FEntryCount:Byte;        {Always 1 if short name only}
   FNameOffset:LongWord;    {Same as EntryOffset if short name only}
   FNameSector:LongWord;    {Same as EntrySector if short name only}
   FEntryOffset:LongWord;
   FEntrySector:LongWord;
   FStartCluster:LongWord;
   //FClusterCount:LongWord; //To Do ? - Use for Size Entry ?
  public
   {Public Variables}
   property EntryCount:Byte read FEntryCount write FEntryCount;
   property NameOffset:LongWord read FNameOffset write FNameOffset;
   property NameSector:LongWord read FNameSector write FNameSector;
   property EntryOffset:LongWord read FEntryOffset write FEntryOffset;
   property EntrySector:LongWord read FEntrySector write FEntrySector;
   property StartCluster:LongWord read FStartCluster write FStartCluster;

   {FAT12/16 Root directory only}
   property StartSector:LongWord read FStartCluster write FStartCluster;
 end;

{==============================================================================}
{var}
 {FATFS specific variables}

const
 {FAT Params Table - Used for FAT entry block loading}
 fatMaxParams = 11;
 fatParams:array[0..fatMaxParams] of TFATParams = (
  {SectorSize,FATType,BlockShiftCount,EntriesPerBlock,SectorsPerBlock}
   {FAT12}
  (SectorSize:512;FATType:ftFAT12;BlockShiftCount:10;EntriesPerBlock:1024;SectorsPerBlock:3),
  (SectorSize:1024;FATType:ftFAT12;BlockShiftCount:11;EntriesPerBlock:2048;SectorsPerBlock:3),
  (SectorSize:2048;FATType:ftFAT12;BlockShiftCount:12;EntriesPerBlock:4096;SectorsPerBlock:3),
  (SectorSize:4096;FATType:ftFAT12;BlockShiftCount:13;EntriesPerBlock:8192;SectorsPerBlock:3),
   {FAT16}
  (SectorSize:512;FATType:ftFAT16;BlockShiftCount:10;EntriesPerBlock:1024;SectorsPerBlock:4),
  (SectorSize:1024;FATType:ftFAT16;BlockShiftCount:11;EntriesPerBlock:2048;SectorsPerBlock:4),
  (SectorSize:2048;FATType:ftFAT16;BlockShiftCount:12;EntriesPerBlock:4096;SectorsPerBlock:4),
  (SectorSize:4096;FATType:ftFAT16;BlockShiftCount:13;EntriesPerBlock:8192;SectorsPerBlock:4),
   {FAT32}
  (SectorSize:512;FATType:ftFAT32;BlockShiftCount:10;EntriesPerBlock:1024;SectorsPerBlock:8),
  (SectorSize:1024;FATType:ftFAT32;BlockShiftCount:11;EntriesPerBlock:2048;SectorsPerBlock:8),
  (SectorSize:2048;FATType:ftFAT32;BlockShiftCount:12;EntriesPerBlock:4096;SectorsPerBlock:8),
  (SectorSize:4096;FATType:ftFAT32;BlockShiftCount:13;EntriesPerBlock:8192;SectorsPerBlock:8));

 {FAT Geometry Tables - Used for FAT formatting (Floppy Disk only)}
 fat12MaxGeometry = 4;
 fat12Geometry:array[0..fat12MaxGeometry] of TFATGeometry = (
  {FloppyType,SectorsPerFat,SectorsPerCluster,RootEntryCount,MediaId}
  (FloppyType:ft360K;SectorsPerFat:2;SectorsPerCluster:2;RootEntryCount:112;MediaId:$FD),
  (FloppyType:ft12M;SectorsPerFat:7;SectorsPerCluster:1;RootEntryCount:224;MediaId:$F9),
  (FloppyType:ft720K;SectorsPerFat:3;SectorsPerCluster:2;RootEntryCount:112;MediaId:$F9),
  (FloppyType:ft144M;SectorsPerFat:9;SectorsPerCluster:1;RootEntryCount:224;MediaId:$F0),
  (FloppyType:ft288M;SectorsPerFat:9;SectorsPerCluster:2;RootEntryCount:240;MediaId:$F0));

 {FAT Cluster Size Tables - Used for FAT formatting (Fixed Disk only)}
 fat12MaxClusterSize = 2;
 fat12ClusterSize:array[0..fat12MaxClusterSize] of TFATClusterSize = (
  {SectorCount,SectorsPerCluster}
  {(SectorCount:6;SectorsPerCluster:0),}        {up to 3 KB, the 0 value for SectorsPerCluster trips an error}{Not possible due to 512 Root entries}
  (SectorCount:798;SectorsPerCluster:0),        {up to 399 KB, the 0 value for SectorsPerCluster trips an error}
  (SectorCount:32623;SectorsPerCluster:8),      {up to 15.9 MB, 4k cluster}
  (SectorCount:$FFFFFFFF;SectorsPerCluster:0)); {greater than 15.9 MB, 0 value for SectorsPerCluster trips an error}
  {See FAT12 Notes for Min/Max and Values}

 fat16MaxClusterSize = 8;
 fat16ClusterSize:array[0..fat16MaxClusterSize] of TFATClusterSize = (
  {SectorCount,SectorsPerCluster}
  (SectorCount:8400;SectorsPerCluster:0),       {up to 4.1 MB, the 0 value for SectorsPerCluster trips an error}
  (SectorCount:32680;SectorsPerCluster:2),      {up to 16 MB, 1k cluster}
  (SectorCount:262144;SectorsPerCluster:4),     {up to 128 MB, 2k cluster}
  (SectorCount:524288;SectorsPerCluster:8),     {up to 256 MB, 4k cluster}
  (SectorCount:1048576;SectorsPerCluster:16),   {up to 512 MB, 8k cluster}
  {The entries after this point are not used unless FAT16 is forced}
  (SectorCount:2097152;SectorsPerCluster:32),   {up to 1 GB, 16k cluster}
  (SectorCount:4194304;SectorsPerCluster:64),   {up to 2 GB, 32k cluster}
  (SectorCount:8388608;SectorsPerCluster:128),  {up to 4 GB, 64k cluster}
  (SectorCount:$FFFFFFFF;SectorsPerCluster:0)); {greater than 4 GB, 0 value for SectorsPerCluster trips an error}

 fat32MaxClusterSize = 5;
 fat32ClusterSize:array[0..fat32MaxClusterSize] of TFATClusterSize = (
  {SectorCount,SectorsPerCluster}
  (SectorCount:66600;SectorsPerCluster:0),       {up to 32.5 MB, the 0 value for SectorsPerCluster trips an error}
  (SectorCount:532480;SectorsPerCluster:1),      {up to 260 MB, .5k cluster}
  {The entries before this point are not used unless FAT32 is forced}
  (SectorCount:16777216;SectorsPerCluster:8),    {up to 8 GB, 4k cluster}
  (SectorCount:33554432;SectorsPerCluster:16),   {up to 16 GB, 8k cluster}
  (SectorCount:67108864;SectorsPerCluster:32),   {up to 32 GB, 16k cluster}
  (SectorCount:$FFFFFFFF;SectorsPerCluster:64)); {greater than 32 GB, 32k cluster}
  //To Do //Need to add support for 64k clusters ? - What is the cutover point ?

 fatMaxPartitionType = 5;
 fatPartitionType:array[0..fatMaxPartitionType] of TFATPartitionType = (
  {Excluded,Extended,SectorCount,PartitionId,LBAType}
   {Extended Partitions}
  (Excluded:False;Extended:True;SectorCount:$FFFFFFFF;PartitionId:pidExtended;LBAType:pidExtLBA),   {any size, Extended}
   {Primary and Logical Partitions}
   {Unused}
  (Excluded:False;Extended:False;SectorCount:798;PartitionId:pidUnused;LBAType:pidUnused),          {up to 399 KB, Unused} {Unused type trips an error}
   {FAT12}
  (Excluded:False;Extended:False;SectorCount:32680;PartitionId:pidFAT12;LBAType:pidFAT12),          {up to 16 MB, FAT12}
   {FAT16}
  (Excluded:False;Extended:False;SectorCount:65360;PartitionId:pidFAT16;LBAType:pidFAT16LBA),       {up to 32 MB, FAT16}
  (Excluded:False;Extended:False;SectorCount:1048576;PartitionId:pidFAT16HUGE;LBAType:pidFAT16LBA), {up to 512 MB, FAT16 HUGE}
   {FAT32}
  (Excluded:False;Extended:False;SectorCount:$FFFFFFFF;PartitionId:pidFAT32;LBAType:pidFAT32LBA));  {above 512 MB, FAT32}

var
 {Formatting Variables}
 {fat12BootJump:TBootSectorJump = ($EB,$3C,$90);}
 {fat12BootCode:TBootSectorCode = (
  $FA,$FC,$31,$C0,$8E,$D8,$BD,$00,$7C,$E8,$2F,$00,$49,$6E,$73,$65,
  $72,$74,$20,$62,$6F,$6F,$74,$20,$64,$69,$73,$6B,$20,$61,$6E,$64,
  $20,$70,$72,$65,$73,$73,$20,$61,$6E,$79,$20,$6B,$65,$79,$00,$30,
  $E4,$CD,$16,$CD,$19,$31,$DB,$B4,$0E,$CD,$10,$5E,$AC,$56,$3C,$00,
  $75,$F3,$C3,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$44,$57,$4C,$44,$52,$20,$20,$20,$20,$20,$20,$00,$00
 );} {Currently same as FAT16}

 fat16BootJump:TBootSectorJump = ($EB,$3C,$90);
 fat16BootCode:TBootSectorCode = (
  $FA,$FC,$31,$C0,$8E,$D8,$BD,$00,$7C,$E8,$2F,$00,$49,$6E,$73,$65,
  $72,$74,$20,$62,$6F,$6F,$74,$20,$64,$69,$73,$6B,$20,$61,$6E,$64,
  $20,$70,$72,$65,$73,$73,$20,$61,$6E,$79,$20,$6B,$65,$79,$00,$30,
  $E4,$CD,$16,$CD,$19,$31,$DB,$B4,$0E,$CD,$10,$5E,$AC,$56,$3C,$00,
  $75,$F3,$C3,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$44,$57,$4C,$44,$52,$20,$20,$20,$20,$20,$20,$00,$00
 );

 fat32BootJump:TExtBootSectorJump = ($EB,$58,$90);
 fat32BootCode:TExtBootSectorCode = (
  $FA,$FC,$31,$C0,$8E,$D8,$BD,$00,$7C,$E8,$2F,$00,$49,$6E,$73,$65,
  $72,$74,$20,$62,$6F,$6F,$74,$20,$64,$69,$73,$6B,$20,$61,$6E,$64,
  $20,$70,$72,$65,$73,$73,$20,$61,$6E,$79,$20,$6B,$65,$79,$00,$30,
  $E4,$CD,$16,$CD,$19,$31,$DB,$B4,$0E,$CD,$10,$5E,$AC,$56,$3C,$00,
  $75,$F3,$C3,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
  $00,$00,$00,$00,$00,$00,$00,$44,$57,$4C,$44,$52,$20,$20,$20,$20,
  $20,$20,$00,$00
 );

{==============================================================================}
{Initialization Functions}
procedure FATFSInit;
procedure FATFSQuit;

{==============================================================================}
{FATFS Functions}

{==============================================================================}
{FATFS Helper Functions}
function FATTypeToString(AType:TFATType):String;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {FATFS specific variables}
 FATFSInitialized:Boolean;

{==============================================================================}
{==============================================================================}
{TFATRecognizer}
constructor TFATRecognizer.Create(ADriver:TFileSysDriver);
begin
 {}
 inherited Create(ADriver);
 FCaseFlags:=True;
 FLongNames:=True;
 FOemConvert:=True;
 FNumericTail:=True;

 FInfoSectorEnable:=True;
 FInfoImmediateUpdate:=False;

 FAllowDrive:=True;
 FAllowDefault:=True;

 FPartitioner:=TFATPartitioner.Create(FDriver,Self);
 FFormatter:=TFATFormatter.Create(FDriver,Self);
 FDefragger:=TFATDefragger.Create(FDriver,Self);
 FRepairer:=TFATRepairer.Create(FDriver,Self);
end;

{==============================================================================}

function TFATRecognizer.CheckLBA:Boolean;
begin
 {}
 Result:=False;

 if FDriver = nil then Exit;

 Result:=True; //To Do //Make Configurable via GlobalConfig
end;

{==============================================================================}

function TFATRecognizer.CheckFAT32:Boolean;
begin
 {}
 Result:=False;

 if FDriver = nil then Exit;

 Result:=True; //To Do //Make Configurable via GlobalConfig
end;

{==============================================================================}

function TFATRecognizer.CheckBootSector(ABootSector:PBootSector;const AStartSector:Int64;ASectorCount:LongWord):Boolean;
begin
 {}
 Result:=False;

 if ABootSector = nil then Exit;

 {$IFDEF FAT_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATRecognizer.CheckBootSector - StartSector = ' + IntToStr(AStartSector) + ' SectorCount = ' + IntToStr(ASectorCount));
 {$ENDIF}

 {Check Boot Sector (Accept 0 due to some faulty devices)}
 if (ABootSector.Signature <> BOOT_SECTOR_SIGNATURE) and (ABootSector.Signature <> 0) then Exit;

 {Check for FAT}
 if ABootSector.BootSignature = fatBootSignature then
  begin
   {Check for FAT12/16}
   if ABootSector.BPB.BytesPerSector = 0 then Exit;
   if (ABootSector.BPB.BytesPerSector mod MIN_SECTOR_SIZE) <> 0 then Exit;
   if ABootSector.BPB.SectorsPerCluster = 0 then Exit;
   if (ABootSector.BPB.SectorsPerCluster <> 1) and ((ABootSector.BPB.SectorsPerCluster mod 2) <> 0) then Exit;
   if (ABootSector.BPB.TotalSectors16 = 0) then
    begin
     if ABootSector.BPB.TotalSectors32 = 0 then Exit;
     if ABootSector.BPB.TotalSectors32 > ASectorCount then Exit;
    end
   else
    begin
     if (ABootSector.BPB.TotalSectors32 <> 0) and (ABootSector.BPB.TotalSectors32 <> ABootSector.BPB.TotalSectors16) then Exit; {Modified 12/8/2007 to account for TotalSectors16 = TotalSectors32}
     if ABootSector.BPB.TotalSectors16 > ASectorCount then Exit;
    end;
   if ABootSector.BPB.NumberOfFats = 0 then Exit;
   if ABootSector.BPB.RootEntryCount = 0 then Exit;
   {if ABootSector.BPB.HiddenSectors <> AStartSector then Exit;} {Doesnt work for Extended or Logical}
   if ((ABootSector.BPB.RootEntryCount * fatEntrySize) mod ABootSector.BPB.BytesPerSector) <> 0 then Exit;
  end
 else
  begin
   {Check for FAT32}
   if PExtBootSector(ABootSector).BootSignature <> fatBootSignature then Exit;
   if PExtBootSector(ABootSector).BPB.BytesPerSector = 0 then Exit;
   if (PExtBootSector(ABootSector).BPB.BytesPerSector mod MIN_SECTOR_SIZE) <> 0 then Exit;
   if PExtBootSector(ABootSector).BPB.SectorsPerCluster = 0 then Exit;
   if (PExtBootSector(ABootSector).BPB.SectorsPerCluster <> 1) and ((PExtBootSector(ABootSector).BPB.SectorsPerCluster mod 2) <> 0) then Exit;
   if (PExtBootSector(ABootSector).BPB.TotalSectors16 = 0) then
    begin
     if PExtBootSector(ABootSector).BPB.TotalSectors32 = 0 then Exit;
     if PExtBootSector(ABootSector).BPB.TotalSectors32 > ASectorCount then Exit;
    end
   else
    begin
     if (PExtBootSector(ABootSector).BPB.TotalSectors32 <> 0) and (PExtBootSector(ABootSector).BPB.TotalSectors32 <> PExtBootSector(ABootSector).BPB.TotalSectors16) then Exit; {Modified 12/8/2007 to account for TotalSectors16 = TotalSectors32}
     if PExtBootSector(ABootSector).BPB.TotalSectors16 > ASectorCount then Exit;
    end;
   if PExtBootSector(ABootSector).BPB.NumberOfFats = 0 then Exit;
   if PExtBootSector(ABootSector).BPB.FileSysVersion <> 0 then Exit;
   if PExtBootSector(ABootSector).BPB.RootEntryCount <> 0 then Exit;
   if PExtBootSector(ABootSector).BPB.SectorsPerFat16 <> 0 then Exit;
   {if PExtBootSector(ABootSector).BPB.HiddenSectors <> AStartSector then Exit;} {Doesnt work for Extended or Logical}
  end;

 Result:=True;
end;

{==============================================================================}

function TFATRecognizer.GetName:String;
begin
 {}
 Result:='FAT';
end;

{==============================================================================}

function TFATRecognizer.RecognizePartitionId(APartitionId:Byte):Boolean;
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;

  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATRecognizer.RecognizePartitionId (PartitionId = ' + IntToStr(APartitionId) + ')');
  {$ENDIF}

  case APartitionId of
   pidExtended:begin
     {DOS Extended Partition}
     Result:=True;
    end;
   pidExtLBA:begin
     {DOS Extended LBA Partition}
     if not CheckLBA then Exit;

     Result:=True;
    end;
   pidFAT12:begin
     {FAT 12 Partition}
     Result:=True;
    end;
   pidFAT16:begin
     {FAT 16 Partition (under 32M)}
     Result:=True;
    end;
   pidFAT16HUGE:begin
     {FAT 16 Partition (over 32M)}
     Result:=True;
    end;
   pidFAT32:begin
     {FAT 32 Partition}
     if not CheckFAT32 then Exit;

     Result:=True;
    end;
   pidFAT32LBA:begin
     {FAT 32 Partition LBA}
     if not CheckLBA then Exit;
     if not CheckFAT32 then Exit;

     Result:=True;
    end;
   pidFAT16LBA:begin
     {FAT 16 Partition LBA}
     if not CheckLBA then Exit;

     Result:=True;
    end;
  end;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TFATRecognizer.RecognizeBootSector(ABootSector:PBootSector;const AStartSector,ASectorCount:Int64):Boolean;
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;

  Result:=CheckBootSector(ABootSector,AStartSector,ASectorCount);
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TFATRecognizer.RecognizePartition(APartition:TDiskPartition):Boolean;
{Note: Caller must hold the partition lock}
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if APartition = nil then Exit;

  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATRecognizer.RecognizePartition (Partition = ' + APartition.Name + ')');
  {$ENDIF}

  case APartition.PartitionId of
   pidExtended:begin
     {DOS Extended Partition}
     APartition.Extended:=True;
     APartition.Recognized:=True;

     Result:=True;
    end;
   pidExtLBA:begin
     {DOS Extended LBA Partition}
     if not CheckLBA then Exit;
     APartition.Extended:=True;
     APartition.Recognized:=True;

     Result:=True;
    end;
   pidFAT12:begin
     {FAT 12 Partition}
     APartition.Recognized:=True;

     Result:=True;
    end;
   pidFAT16:begin
     {FAT 16 Partition (under 32M)}
     APartition.Recognized:=True;

     Result:=True;
    end;
   pidFAT16HUGE:begin
     {FAT 16 Partition (over 32M)}
     APartition.Recognized:=True;

     Result:=True;
    end;
   pidFAT32:begin
     {FAT 32 Partition}
     if not CheckFAT32 then Exit;
     APartition.Recognized:=True;

     Result:=True;
    end;
   pidFAT32LBA:begin
     {FAT 32 Partition LBA}
     if not CheckLBA then Exit;
     if not CheckFAT32 then Exit;
     APartition.Recognized:=True;

     Result:=True;
    end;
   pidFAT16LBA:begin
     {FAT 16 Partition LBA}
     if not CheckLBA then Exit;
     APartition.Recognized:=True;

     Result:=True;
    end;
  end;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TFATRecognizer.RecognizeVolume(AVolume:TDiskVolume):Boolean;
{Note: Caller must hold the volume writer lock}
var
 BootSector:PBootSector;
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if AVolume = nil then Exit;
  if AVolume.Device = nil then Exit;
  if AVolume.Device.Controller = nil then Exit;

  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATRecognizer.RecognizeVolume (Volume = ' + AVolume.Name + ')');
  {$ENDIF}

  if AVolume.Partition <> nil then
   begin
    {Partitioned Media}
    {$IFDEF FAT_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATRecognizer.RecognizeVolume - Partitioned Media');
    {$ENDIF}
    {Check Partition Id}
    case AVolume.Partition.PartitionId of
     pidFAT12,pidFAT16,pidFAT16HUGE,pidFAT32,pidFAT32LBA,pidFAT16LBA:begin
       {$IFDEF FAT_DEBUG}
       if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATRecognizer.RecognizeVolume - Volume Recognized');
       {$ENDIF}

       AVolume.Recognized:=True;

       Result:=True;
      end;
    end;
   end
  else
   begin
    {Non Partitioned Media}
    {$IFDEF FAT_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATRecognizer.RecognizeVolume - Non Partitioned Media');
    {$ENDIF}

    {Check Device Type}
    case AVolume.Device.MediaType of
     mtFLOPPY,mtREMOVABLE,mtOTHER:begin  {No FAT on CDROM/DVD}
       {Check Default}
       if not AllowDefault then
        begin
         {Check Media}
         if not AVolume.Device.Controller.MediaReady(AVolume.Device) then Exit; {was Volume.Device.MediaReady}

         {Init Device}
         if not AVolume.Device.DeviceInit then Exit;

         {Init Volume}
         if not AVolume.VolumeInit then Exit;

         {Allocate Boot Sector}
         BootSector:=GetMem(AVolume.Device.SectorSize);
         if BootSector = nil then Exit;
         try
          {Read Boot Sector}
          if not FDriver.Cache.DeviceRead(AVolume.Device,AVolume.StartSector,1,BootSector^) then Exit;

          {Check Boot Sector}
          if not CheckBootSector(BootSector,AVolume.StartSector,AVolume.SectorCount) then Exit;

          {$IFDEF FAT_DEBUG}
          if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATRecognizer.RecognizeVolume - Volume Recognized');
          {$ENDIF}

          AVolume.Recognized:=True;

          Result:=True;
         finally
          FreeMem(BootSector);
         end;
        end
       else
        begin
         {Default Recognizer}
         {$IFDEF FAT_DEBUG}
         if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATRecognizer.RecognizeVolume - Volume Recognized (Default)');
         {$ENDIF}

         AVolume.Recognized:=True;

         Result:=True;
        end;
      end;
    end;
   end;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TFATRecognizer.MountVolume(AVolume:TDiskVolume;ADrive:TDiskDrive):Boolean;
{Note: Caller must hold the volume writer lock}
var
 FileSystem:TFATFileSystem;
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if AVolume = nil then Exit;

  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATRecognizer.MountVolume (Volume = ' + AVolume.Name + ')');
  {$ENDIF}

  {Check Recognized}
  if not RecognizeVolume(AVolume) then Exit;

  {Create FileSystem}
  FileSystem:=TFATFileSystem.Create(FDriver,AVolume,ADrive);
  FileSystem.CaseFlags:=FCaseFlags;
  FileSystem.OemConvert:=FOemConvert;
  FileSystem.NumericTail:=FNumericTail;
  FileSystem.LongNames:=FLongNames;
  FileSystem.CasePreserved:=FLongNames; {Enabled only if LongNames Enabled}
  FileSystem.UnicodeNames:=FLongNames;  {Enabled only if LongNames Enabled}
  FileSystem.InfoSectorEnable:=FInfoSectorEnable;
  FileSystem.InfoImmediateUpdate:=FInfoImmediateUpdate;
  FileSystem.FileSystemInit;
  FileSystem.MountFileSystem;

  Result:=True;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}
{==============================================================================}
{TFATPartitioner}
constructor TFATPartitioner.Create(ADriver:TFileSysDriver;ARecognizer:TRecognizer);
begin
 {}
 inherited Create(ADriver,ARecognizer);
 FInitChar:=$F6;  {Fill Sectors with F6 on Create Partition}
end;

{==============================================================================}

function TFATPartitioner.CheckLogical(ADevice:TDiskDevice;AParent:TDiskPartition;APartitionId:Byte):Boolean;
{Note: Caller must hold the device and parent lock}
begin
 {}
 Result:=False;

 if ADevice = nil then Exit;

 {Check Type}
 case APartitionId of
  pidFAT12,pidFAT16,pidFAT16HUGE,pidFAT16LBA,pidFAT32,pidFAT32LBA:begin
    if AParent = nil then Exit;
    Result:=True;
   end;
 end;
end;

{==============================================================================}

function TFATPartitioner.CheckExtended(ADevice:TDiskDevice;AParent:TDiskPartition;APartitionId:Byte):Boolean;
{Note: Caller must hold the device and parent lock}
begin
 {}
 Result:=False;

 if ADevice = nil then Exit;

 {Check Type}
 case APartitionId of
  pidExtended,pidExtLBA:begin
    Result:=True;
   end;
 end;
end;

{==============================================================================}

function TFATPartitioner.GetPartitionId(ADevice:TDiskDevice;AParent:TDiskPartition;AStart,ACount:LongWord;APartitionId:Byte):Byte;
{Note: Start is the absolute start sector on the device}
{Note: This currently uses a fixed table for determining type based on size}
{Note: Caller must hold the device and parent lock}
var
 LBA:Boolean;
 Count:Integer;
 Extended:Boolean;
begin
 {}
 Result:=pidUnused;

 if ACount = 0 then Exit;
 if ADevice = nil then Exit;

 {Get LBA}
 LBA:=(ADevice.LBA and ((ADevice.PhysicalCylinders > 1024) or (ADevice.PhysicalCylinders = 0)));

 {Check Type}
 case APartitionId of
  pidFAT12,pidFAT16,pidFAT16HUGE,pidFAT16LBA,pidFAT32,pidFAT32LBA,pidExtended,pidExtLBA:begin
    {Get Extended}
    Extended:=(APartitionId = pidExtended) or (APartitionId = pidExtLBA);

    {Get Type}
    for Count:=0 to fatMaxPartitionType do
     begin
      if not fatPartitionType[Count].Excluded then
       begin
        if (fatPartitionType[Count].Extended = Extended) and (fatPartitionType[Count].SectorCount >= ACount) then
         begin
          Result:=fatPartitionType[Count].PartitionId;
          if LBA then Result:=fatPartitionType[Count].LBAType;

          {DOS only allows standard type for second level Extended}
          if (Extended) and (AParent <> nil) then Result:=fatPartitionType[Count].PartitionId;
          Exit;
         end;
       end;
     end;
   end;
 end;
end;

{==============================================================================}

function TFATPartitioner.InitPartition(ADevice:TDiskDevice;AParent:TDiskPartition;AStart,ACount:LongWord;APartitionId:Byte):Boolean;
{Note: Start is the absolute start sector on the device}
{Note: Caller must hold the device and parent lock}
begin
 {}
 Result:=False;

 if ACount = 0 then Exit;
 if ADevice = nil then Exit;

 {Check Type}
 case APartitionId of
  pidExtended,pidExtLBA:begin
    {Initialize Partition Record}
    Result:=FillSectors(ADevice,nil,AStart,1,FInitChar);
   end;
  pidFAT12,pidFAT16,pidFAT16HUGE,pidFAT16LBA:begin
    {Initialize Boot Sector}
    Result:=FillSectors(ADevice,nil,AStart,1,FInitChar);
   end;
  pidFAT32,pidFAT32LBA:begin
    {Initialize Boot Sectors}
    Result:=FillSectors(ADevice,nil,AStart,16,FInitChar);
   end;
 end;
end;

{==============================================================================}

function TFATPartitioner.AcceptPartition(ADevice:TDiskDevice;APartition,AParent:TDiskPartition;APartitionId:Byte):Boolean;
{Note: Caller must hold the device, partition and parent lock}
var
 Volume:TDiskVolume;
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FRecognizer = nil then Exit;

  if APartition = nil then
   begin
    {Accept Create Partition}
    if ADevice = nil then Exit;

    {Check Device}
    if (ADevice.MediaType <> mtFIXED) and (ADevice.MediaType <> mtREMOVABLE) then Exit;

    {Check Partition and Volume}
    if FDriver.GetPartitionByDevice(ADevice,False,FILESYS_LOCK_NONE) = nil then {Do not lock}
     begin
      Volume:=FDriver.GetVolumeByDevice(ADevice,True,FILESYS_LOCK_READ);
      if Volume <> nil then
       begin
        try
         {Check File System Type}
         if Volume.FileSysType <> fsUNKNOWN then Exit;
        finally
         Volume.ReaderUnlock;
        end;
       end;
     end;

    {Check Parent}
    if AParent <> nil then
     begin
      {Check Extended}
      if not AParent.Extended then Exit;

      {Check First Level}
      if AParent.Partition <> nil then Exit;
     end;

    {Check Type}
    case APartitionId of
     pidExtended,pidExtLBA:begin
       {Check Parent}
       if AParent <> nil then Exit;

       Result:=True;
      end;
     pidFAT12,pidFAT16,pidFAT16HUGE,pidFAT32,pidFAT32LBA,pidFAT16LBA:begin
       Result:=True;
      end;
    end;
   end
  else
   begin
    if APartitionId = pidUnused then
     begin
      {Accept Delete Partition}
      {Check Children}
      if (AParent = nil) and (FDriver.GetPartitionByPartition(APartition,False,FILESYS_LOCK_NONE) <> nil) then Exit; {Do not lock}

      Result:=True;
     end
    else if APartitionId <> APartition.PartitionId then
     begin
      {Accept Modify Partition}
      {Check Extended}
      if APartition.Extended then Exit;

      {Check Current Type}
      case APartition.PartitionId of
       pidFAT12,pidFAT16,pidFAT16HUGE,pidFAT32,pidFAT32LBA,pidFAT16LBA,pidHPFSNTFS:begin
         {Check New Type}
         case APartitionId of
          pidFAT12,pidFAT16,pidFAT16HUGE,pidFAT32,pidFAT32LBA,pidFAT16LBA:begin
            Result:=True;
           end;
         end;
        end;
      end;
     end
    else if APartitionId = APartition.PartitionId then
     begin
      {Accept Activate Partition}
      {Check Primary}
      if not APartition.Primary then Exit;

      {Check Type}
      case APartitionId of
       pidFAT12,pidFAT16,pidFAT16HUGE,pidFAT32,pidFAT32LBA,pidFAT16LBA:begin
         Result:=True;
        end;
      end;
     end;
   end;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}
{==============================================================================}
{TFATFormatter}
function TFATFormatter.CheckDevice(AVolume:TDiskVolume;ADrive:TDiskDrive;AFloppyType:TFloppyType):Boolean;
{Checks Device and Floppy types are suitable for formatting}
{Note: Caller must hold the volume lock}
begin
 {}
 Result:=False;

 if AVolume = nil then Exit;
 if AVolume.Device = nil then Exit;

 case AVolume.Device.MediaType of
  mtFIXED,mtREMOVABLE,mtOTHER:begin
    if AFloppyType <> ftUNKNOWN then Exit;
    if not AVolume.Device.Writeable then Exit;

    Result:=True;
   end;
  mtFLOPPY:begin
    //if not AVolume.Device.DeviceInit then Exit; //To Do //Do this here to allow for change of Media ?
    if AFloppyType <> AVolume.Device.FloppyType then Exit; //To Do //Need a way to allow formatting different size floppies //why not case AVolume.Device.FloppyType of ?
    if not AVolume.Device.Writeable then Exit;

    Result:=True;
   end;
 end;
end;

{==============================================================================}

function TFATFormatter.CheckPartition(AVolume:TDiskVolume;ADrive:TDiskDrive;AFileSysType:TFileSysType):Boolean;
{Checks Partition type is suitable for formatting}
{Note: Also check File System type for non partitioned media}
{Note: Caller must hold the volume lock}
begin
 {}
 Result:=False;

 if AVolume = nil then Exit;

 {Check FileSystem}
 case AFileSysType of
  fsFAT12,fsFAT16,fsFAT32:begin
    {FAT File System}
    if AVolume.Device = nil then Exit;
    if AVolume.Partition <> nil then
     begin
      {Partitioned Media}
      case AVolume.Partition.PartitionId of
       pidFAT12,pidFAT16,pidFAT16HUGE,pidFAT16LBA,pidFAT32,pidFAT32LBA:begin
         {FAT Partition}
         Result:=True;
        end;
      end;
     end
    else
     begin
      {Non Partitioned Media}
      Result:=True;
     end;
   end;
 end;
end;

{==============================================================================}

function TFATFormatter.GetPartitionId(AVolume:TDiskVolume;ADrive:TDiskDrive;AFATType:TFATType):Byte;
{Gets the actual PartitonId to match the FAT type for formatting}
{Note: Caller must hold the volume lock}
var
 LBA:Boolean;
begin
 {}
 Result:=pidUnused;

 if AVolume = nil then Exit;
 if AVolume.Device = nil then Exit;
 if AVolume.Partition = nil then Exit;

 {Get LBA}
 LBA:=(AVolume.Device.LBA and ((AVolume.Device.PhysicalCylinders > 1024) or (AVolume.Device.PhysicalCylinders = 0)));
 {Check Type}
 case AFATType of
  ftFAT12:begin
    Result:=pidFAT12;
   end;
  ftFAT16:begin
    Result:=pidFAT16;
    {Check for FAT16 > 32MB}
    if AVolume.Partition.SectorCount > 65360 then Result:=pidFAT16HUGE;
    if LBA then Result:=pidFAT16LBA;
   end;
  ftFAT32:begin
    Result:=pidFAT32;
    if LBA then Result:=pidFAT32LBA;
   end;
 end;
end;

{==============================================================================}

function TFATFormatter.UpdatePartitionId(AVolume:TDiskVolume;ADrive:TDiskDrive;AFATType:TFATType):Boolean;
{Updates the PartitionId to match the FAT type for formatting}
{Note: Caller must hold the volume lock}
var
 PartitionId:Byte;
begin
 {}
 Result:=False;

 if AVolume = nil then Exit;
 if AVolume.Device = nil then Exit;

 if AVolume.Partition <> nil then
  begin
   {Partitioned Media}
   if AVolume.Partition.EntryNo = -1 then Exit;
   {Get PartitionId}
   PartitionId:=GetPartitionId(AVolume,ADrive,AFATType);
   if PartitionId = pidUnused then Exit;
   {Check PartitionId}
   if AVolume.Partition.PartitionId <> PartitionId then
    begin
     {Modify Partition} {Override Default PartitionId}
     if FRecognizer = nil then Exit;
     if FRecognizer.Partitioner = nil then Exit;
     if not FRecognizer.Partitioner.ModifyPartition(AVolume.Partition,PartitionId,True) then Exit;
    end;
   Result:=True;
  end
 else
  begin
   {Non Partitioned Media}
   Result:=True;
  end;
end;

{==============================================================================}

function TFATFormatter.GetSectorsPerCluster(AVolume:TDiskVolume;ADrive:TDiskDrive;AFloppyType:TFloppyType;AFileSysType:TFileSysType;ABootSector:PBootSector;var AFATType:TFATType):LongWord;
{Determine SectorsPerCluster and FAT type from the tables based on passed parameters}
{Also sets RootEntryCount, SectorsPerFat, MediaId etc in BootSector}
{Note: Caller must hold the volume lock}
var
 Count:Integer;
begin
 {}
 Result:=0;

 if ABootSector = nil then Exit;
 if AVolume = nil then Exit;
 if AVolume.Device = nil then Exit;

 {Check Device}
 case AVolume.Device.MediaType of
  mtFIXED,mtREMOVABLE,mtOTHER:begin
    {Deterine FAT Type}
    AFATType:=ftNONE;
    {Check FAT12}
    if ((Result = 0) and (AFATType = ftNONE)) or (AFileSysType = fsFAT12) then
     begin
      for Count:=0 to fat12MaxClusterSize do
       begin
        if AVolume.SectorCount <= fat12ClusterSize[Count].SectorCount then
         begin
          if fat12ClusterSize[Count].SectorsPerCluster > 0 then
           begin
            AFATType:=ftFAT12;
            ABootSector.BPB.SectorsPerCluster:=fat12ClusterSize[Count].SectorsPerCluster;
            ABootSector.BPB.ReservedSectors:=1;
            ABootSector.BPB.RootEntryCount:=512;
            ABootSector.BPB.BytesPerSector:=AVolume.SectorSize;
            ABootSector.BPB.NumberOfFats:=2;
            ABootSector.BPB.SectorsPerFat16:=GetSectorsPerFat(AVolume,nil,AFATType,ABootSector);
            ABootSector.BPB.MediaId:=$F8;

            Result:=ABootSector.BPB.SectorsPerCluster;
           end;
          Break;
         end;
       end;
     end;
    {Check FAT16}
    if ((Result = 0) and (AFATType = ftNONE)) or (AFileSysType = fsFAT16) then
     begin
      for Count:=0 to fat16MaxClusterSize do
       begin
        if AVolume.SectorCount <= fat16ClusterSize[Count].SectorCount then
         begin
          if fat16ClusterSize[Count].SectorsPerCluster > 0 then
           begin
            AFATType:=ftFAT16;
            ABootSector.BPB.SectorsPerCluster:=fat16ClusterSize[Count].SectorsPerCluster;
            ABootSector.BPB.ReservedSectors:=1;
            ABootSector.BPB.RootEntryCount:=512;
            ABootSector.BPB.BytesPerSector:=AVolume.SectorSize;
            ABootSector.BPB.NumberOfFats:=2;
            ABootSector.BPB.SectorsPerFat16:=GetSectorsPerFat(AVolume,nil,AFATType,ABootSector);
            ABootSector.BPB.MediaId:=$F8;

            Result:=ABootSector.BPB.SectorsPerCluster;
           end;

          Break;
         end;
       end;
     end;
    {Check FAT32}
    if ((Result = 0) and (AFATType = ftNONE)) or (AFileSysType = fsFAT32) then
     begin
      for Count:=0 to fat32MaxClusterSize do
       begin
        if AVolume.SectorCount <= fat32ClusterSize[Count].SectorCount then
         begin
          if fat32ClusterSize[Count].SectorsPerCluster > 0 then
           begin
            AFATType:=ftFAT32;
            PExtBootSector(ABootSector).BPB.SectorsPerCluster:=fat32ClusterSize[Count].SectorsPerCluster;
            PExtBootSector(ABootSector).BPB.ReservedSectors:=32;
            PExtBootSector(ABootSector).BPB.RootEntryCount:=0;
            PExtBootSector(ABootSector).BPB.BytesPerSector:=AVolume.SectorSize;
            PExtBootSector(ABootSector).BPB.NumberOfFats:=2;
            PExtBootSector(ABootSector).BPB.SectorsPerFat16:=0;
            PExtBootSector(ABootSector).BPB.SectorsPerFat32:=GetSectorsPerFat(AVolume,nil,AFATType,ABootSector);
            PExtBootSector(ABootSector).BPB.MediaId:=$F8;

            Result:=ABootSector.BPB.SectorsPerCluster;
           end;

          Break;
         end;
       end;
     end;
   end;
  mtFLOPPY:begin
    {Determine Floppy Type}
    for Count:=0 to fat12MaxGeometry do
     begin
      if fat12Geometry[Count].FloppyType = AFloppyType then
       begin
        {Get Geometry}
        AFATType:=ftFAT12;
        ABootSector.BPB.SectorsPerCluster:=fat12Geometry[Count].SectorsPerCluster;
        ABootSector.BPB.ReservedSectors:=1;
        ABootSector.BPB.RootEntryCount:=fat12Geometry[Count].RootEntryCount;
        ABootSector.BPB.BytesPerSector:=AVolume.SectorSize;
        ABootSector.BPB.NumberOfFats:=2;
        ABootSector.BPB.SectorsPerFat16:=fat12Geometry[Count].SectorsPerFat;
        ABootSector.BPB.MediaId:=fat12Geometry[Count].MediaId;

        Result:=ABootSector.BPB.SectorsPerCluster;
        Break;
       end;
     end;
   end;
 end;
end;

{==============================================================================}

function TFATFormatter.GetSectorsPerFat(AVolume:TDiskVolume;ADrive:TDiskDrive;AFATType:TFATType;ABootSector:PBootSector):LongWord;
{Calculate SectorsPerFat based on FAT type and Boot Sector values}
{Note: Caller must hold the volume lock}
var
 WorkValue1:LongWord;
 WorkValue2:LongWord;
 SectorCount:LongWord;
 RootSectorCount:LongWord;
begin
 {}
 Result:=0;

 if ABootSector = nil then Exit;

 {Get SectorCount}
 SectorCount:=0;
 if ADrive <> nil then SectorCount:=ADrive.SectorCount;
 if AVolume <> nil then SectorCount:=AVolume.SectorCount;

 {Get RootSectorCount}
 RootSectorCount:=((ABootSector.BPB.RootEntryCount * fatEntrySize) + (ABootSector.BPB.BytesPerSector - 1)) div ABootSector.BPB.BytesPerSector;

 {Get SectorsPerFat}
 WorkValue1:=SectorCount - (ABootSector.BPB.ReservedSectors + RootSectorCount);
 WorkValue2:=(256 * ABootSector.BPB.SectorsPerCluster) + ABootSector.BPB.NumberOfFats;
 if AFATType = ftFAT32 then WorkValue2:=(WorkValue2 div 2);

 Result:=(WorkValue1 + (WorkValue2 - 1)) div WorkValue2;
end;

{==============================================================================}

function TFATFormatter.CreateBootSector(AVolume:TDiskVolume;ADrive:TDiskDrive;AFATType:TFATType;ABootSector:PBootSector):Boolean;
{Fill the Boot Sector with values based on FAT type}
{Note: Caller must hold the volume lock}
var
 DriveNumber:Byte;
 SystemName:String;
 NumberOfHeads:Word;
 TotalSectors16:Word;
 SectorsPerTrack:Word;
 HiddenSectors:LongWord;
 TotalSectors32:LongWord;
begin
 {}
 Result:=False;

 if ABootSector = nil then Exit;

 {Get Values}
 if AVolume = nil then Exit;
 if AVolume.Device = nil then Exit;

 {Get DriveNumber}
 DriveNumber:=$FF; {previously $00;}
 if (AVolume.Device.MediaType = mtFIXED) or (AVolume.Device.MediaType = mtREMOVABLE) then DriveNumber:=AVolume.Device.DeviceNo;

 {Get Heads and Sectors}
 NumberOfHeads:=AVolume.Device.LogicalHeads;
 SectorsPerTrack:=AVolume.Device.LogicalSectors;

 {Get HiddenSectors}
 HiddenSectors:=0;
 if AVolume.Partition <> nil then HiddenSectors:=AVolume.Partition.SectorOffset;

 {Get TotalSectors}
 TotalSectors16:=0;
 TotalSectors32:=0;
 if AVolume.SectorCount < 65536 then TotalSectors16:=AVolume.SectorCount;
 if AVolume.SectorCount > 65535 then TotalSectors32:=AVolume.SectorCount;

 {Check Type}
 case AFATType of
  ftFAT12,ftFAT16:begin
    {Get System Name}
    SystemName:=fatNames[fatFAT12];
    if AFATType = ftFAT16 then SystemName:=fatNames[fatFAT16];

    {Create BootSector}
    ABootSector.BootJump:=fat16BootJump;
    FillChar(ABootSector.OEMName[0],8,fatEntryPadding);
    System.Move(fatOemName[1],ABootSector.OEMName[0],Min(Length(fatOemName),8));
    {ABootSector.BPB.BytesPerSector}    {Set by GetSectorsPerCluster}
    {ABootSector.BPB.SectorsPerCluster} {Set by GetSectorsPerCluster}
    {ABootSector.BPB.ReservedSectors}    {Set by GetSectorsPerCluster}
    {ABootSector.BPB.NumberOfFats}       {Set by GetSectorsPerCluster}
    {ABootSector.BPB.RootEntryCount}     {Set by GetSectorsPerCluster}
    ABootSector.BPB.TotalSectors16:=TotalSectors16;
    {ABootSector.BPB.MediaId}            {Set by GetSectorsPerCluster}
    {ABootSector.BPB.SectorsPerFat16}    {Set by GetSectorsPerCluster}
    ABootSector.BPB.SectorsPerTrack:=SectorsPerTrack;
    ABootSector.BPB.NumberOfHeads:=NumberOfHeads;
    ABootSector.BPB.HiddenSectors:=HiddenSectors;
    ABootSector.BPB.TotalSectors32:=TotalSectors32;
    ABootSector.DriveNumber:=DriveNumber;
    ABootSector.Reserved1:=0;
    ABootSector.BootSignature:=fatBootSignature;
    ABootSector.VolumeSerial:=DateTimeToFileDate(Now);
    FillChar(ABootSector.VolumeName[0],11,fatEntryPadding);
    System.Move(fatDefaultName[1],ABootSector.VolumeName[0],Min(Length(fatDefaultName),11));
    FillChar(ABootSector.SystemName[0],8,fatEntryPadding);
    System.Move(SystemName[1],ABootSector.SystemName[0],Min(Length(SystemName),8));
    ABootSector.BootCode:=fat16BootCode;
    ABootSector.Signature:=BOOT_SECTOR_SIGNATURE;

    Result:=True;
   end;
  ftFAT32:begin
    {Get System Name}
    SystemName:=fatNames[fatFAT32];

    {Create BootSector}
    PExtBootSector(ABootSector).BootJump:=fat32BootJump;
    FillChar(PExtBootSector(ABootSector).OEMName[0],8,fatEntryPadding);
    System.Move(fatOemName[1],PExtBootSector(ABootSector).OEMName[0],Min(Length(fatOemName),8));
    {PExtBootSector(ABootSector).BPB.BytesPerSector}    {Set by GetSectorsPerCluster}
    {PExtBootSector(ABootSector).BPB.SectorsPerCluster} {Set by GetSectorsPerCluster}
    {PExtBootSector(ABootSector).BPB.ReservedSectors}   {Set by GetSectorsPerCluster}
    {PExtBootSector(ABootSector).BPB.NumberOfFats}      {Set by GetSectorsPerCluster}
    {PExtBootSector(ABootSector).BPB.RootEntryCount}    {Set by GetSectorsPerCluster}
    PExtBootSector(ABootSector).BPB.TotalSectors16:=TotalSectors16;
    {PExtBootSector(ABootSector).BPB.MediaId}           {Set by GetSectorsPerCluster}
    {PExtBootSector(ABootSector).BPB.SectorsPerFat16}   {Set by GetSectorsPerCluster}
    PExtBootSector(ABootSector).BPB.SectorsPerTrack:=SectorsPerTrack;
    PExtBootSector(ABootSector).BPB.NumberOfHeads:=NumberOfHeads;
    PExtBootSector(ABootSector).BPB.HiddenSectors:=HiddenSectors;
    PExtBootSector(ABootSector).BPB.TotalSectors32:=TotalSectors32;
    {PExtBootSector(ABootSector).BPB.SectorsPerFat32}   {Set by GetSectorsPerCluster}
    PExtBootSector(ABootSector).BPB.ExtendedFlags:=0;
    PExtBootSector(ABootSector).BPB.FileSysVersion:=0;
    PExtBootSector(ABootSector).BPB.RootCluster:=2;
    PExtBootSector(ABootSector).BPB.FileSysInfoSector:=1;
    PExtBootSector(ABootSector).BPB.BackupBootSector:=6;
    {PExtBootSector(ABootSector).BPB.Reserved}
    PExtBootSector(ABootSector).DriveNumber:=DriveNumber;
    PExtBootSector(ABootSector).Reserved1:=0;
    PExtBootSector(ABootSector).BootSignature:=fatBootSignature;
    PExtBootSector(ABootSector).VolumeSerial:=DateTimeToFileDate(Now);
    FillChar(PExtBootSector(ABootSector).VolumeName[0],11,fatEntryPadding);
    System.Move(fatDefaultName[1],PExtBootSector(ABootSector).VolumeName[0],Min(Length(fatDefaultName),11));
    FillChar(PExtBootSector(ABootSector).SystemName[0],8,fatEntryPadding);
    System.Move(SystemName[1],PExtBootSector(ABootSector).SystemName[0],Min(Length(SystemName),8));
    PExtBootSector(ABootSector).BootCode:=fat32BootCode;
    PExtBootSector(ABootSector).Signature:=BOOT_SECTOR_SIGNATURE;

    Result:=True;
   end;
 end;
end;

{==============================================================================}

function TFATFormatter.WriteBootSector(AVolume:TDiskVolume;ADrive:TDiskDrive;AFATType:TFATType;ABootSector:PBootSector):Boolean;
{Write the created Boot sector to disk and to backup boot if needed}
{Note: Caller must hold the volume lock}
begin
 {}
 Result:=False;

 if ABootSector = nil then Exit;

 {Check Type}
 case AFATType of
  ftFAT12,ftFAT16:begin
    {Write ReservedSectors}
    if not FillSectors(AVolume,ADrive,0,ABootSector.BPB.ReservedSectors,0) then Exit;

    {Write BootSector}
    if not WriteSectors(AVolume,ADrive,0,1,ABootSector^) then Exit;
    Result:=True;
   end;
  ftFAT32:begin
    {Write ReservedSectors}
    if not FillSectors(AVolume,ADrive,0,PExtBootSector(ABootSector).BPB.ReservedSectors,0) then Exit;

    {Write BootSector}
    if not WriteSectors(AVolume,ADrive,0,1,ABootSector^) then Exit;

    {Write BootBackup}
    if not WriteSectors(AVolume,ADrive,PExtBootSector(ABootSector).BPB.BackupBootSector,1,ABootSector^) then Exit;
    Result:=True;
   end;
 end;
end;

{==============================================================================}

function TFATFormatter.WriteFatTable(AVolume:TDiskVolume;ADrive:TDiskDrive;AFATType:TFATType;ABootSector:PBootSector):Boolean;
{Write an empty FAT table to disk}
{Note: Caller must hold the volume lock}
var
 Count:Word;
 Buffer:Pointer;
 SectorSize:Word;
 NumberOfFats:Word;
 SectorsPerFat:LongWord;
 ReservedSectors:LongWord;
begin
 {}
 Result:=False;

 if ABootSector = nil then Exit;

 {Check Type}
 case AFATType of
  ftFAT12,ftFAT16:begin
    {Get Values}
    SectorSize:=ABootSector.BPB.BytesPerSector;
    NumberOfFats:=ABootSector.BPB.NumberOfFats;
    SectorsPerFat:=ABootSector.BPB.SectorsPerFat16;
    ReservedSectors:=ABootSector.BPB.ReservedSectors;

    {Allocate Buffer}
    Buffer:=GetMem(SectorSize);
    if Buffer = nil then Exit;
    try
     {Write FatTables}
     for Count:=0 to NumberOfFats - 1 do
      begin
       if not FillSectors(AVolume,ADrive,ReservedSectors + (SectorsPerFat * Count),SectorsPerFat,0) then Exit;

       {Read First Sector}
       if not ReadSectors(AVolume,ADrive,ReservedSectors + (SectorsPerFat * Count),1,Buffer^) then Exit;

       {Set Default Entries} {2 Reserved Clusters}
       case AFATType of
        ftFAT12:begin
          LongWord(Pointer(PtrUInt(Buffer) + 0)^):=($00FFFF00 or ABootSector.BPB.MediaId);
         end;
        ftFAT16:begin
          LongWord(Pointer(PtrUInt(Buffer) + 0)^):=($FFFFFF00 or ABootSector.BPB.MediaId);
         end;
       end;

       {Write First Sector}
       if not WriteSectors(AVolume,ADrive,ReservedSectors + (SectorsPerFat * Count),1,Buffer^) then Exit;
      end;

     Result:=True;
    finally
     FreeMem(Buffer);
    end;
   end;
  ftFAT32:begin
    {Get Values}
    SectorSize:=PExtBootSector(ABootSector).BPB.BytesPerSector;
    NumberOfFats:=PExtBootSector(ABootSector).BPB.NumberOfFats;
    SectorsPerFat:=PExtBootSector(ABootSector).BPB.SectorsPerFat32;
    ReservedSectors:=PExtBootSector(ABootSector).BPB.ReservedSectors;

    {Allocate Buffer}
    Buffer:=GetMem(SectorSize);
    if Buffer = nil then Exit;
    try
     {Write FatTables}
     for Count:=0 to NumberOfFats - 1 do
      begin
       if not FillSectors(AVolume,ADrive,ReservedSectors + (SectorsPerFat * Count),SectorsPerFat,0) then Exit;

       {Read First Sector}
       if not ReadSectors(AVolume,ADrive,ReservedSectors + (SectorsPerFat * Count),1,Buffer^) then Exit;

       {Set Default Entries} {2 Reserved Clusters and 1 Root Cluster}
       LongWord(Pointer(PtrUInt(Buffer) + 0)^):=($0FFFFF00 or PExtBootSector(ABootSector).BPB.MediaId);
       LongWord(Pointer(PtrUInt(Buffer) + 4)^):=($0FFFFFFF);
       LongWord(Pointer(PtrUInt(Buffer) + 8)^):=(fat32EndOfCluster);

       {Write First Sector}
       if not WriteSectors(AVolume,ADrive,ReservedSectors + (SectorsPerFat * Count),1,Buffer^) then Exit;
      end;

     Result:=True;
    finally
     FreeMem(Buffer);
    end;
   end;
 end;
end;

{==============================================================================}

function TFATFormatter.WriteRootDirectory(AVolume:TDiskVolume;ADrive:TDiskDrive;AFATType:TFATType;ABootSector:PBootSector):Boolean;
{Write an empty root directory to disk}
{Note: Caller must hold the volume lock}
var
 SectorSize:Word;
 NumberOfFats:Word;
 SectorsPerFat:LongWord;
 RootEntryCount:LongWord;
 ReservedSectors:LongWord;
 RootSectorCount:LongWord;
 RootStartSector:LongWord;
 DataStartSector:LongWord;
 SectorsPerCluster:LongWord;
begin
 {}
 Result:=False;

 if ABootSector = nil then Exit;

 {Check Type}
 case AFATType of
  ftFAT12,ftFAT16:begin
    {Get Values}
    SectorSize:=ABootSector.BPB.BytesPerSector;
    NumberOfFats:=ABootSector.BPB.NumberOfFats;
    SectorsPerFat:=ABootSector.BPB.SectorsPerFat16;
    RootEntryCount:=ABootSector.BPB.RootEntryCount;
    ReservedSectors:=ABootSector.BPB.ReservedSectors;

    {Get RootSectorCount}
    RootSectorCount:=((RootEntryCount * fatEntrySize) + SectorSize - 1) div SectorSize;

    {Get RootStartSector}
    RootStartSector:=(ReservedSectors + (SectorsPerFat * NumberOfFats));

    {Write RootDirectory}
    if not FillSectors(AVolume,ADrive,RootStartSector,RootSectorCount,0) then Exit;

    Result:=True;
   end;
  ftFAT32:begin
    {Get Values}
    NumberOfFats:=PExtBootSector(ABootSector).BPB.NumberOfFats;
    SectorsPerFat:=PExtBootSector(ABootSector).BPB.SectorsPerFat32;
    ReservedSectors:=PExtBootSector(ABootSector).BPB.ReservedSectors;
    SectorsPerCluster:=PExtBootSector(ABootSector).BPB.SectorsPerCluster;

    {Get DataStartSector}
    DataStartSector:=(ReservedSectors + (SectorsPerFat * NumberOfFats));

    {Write RootCluster}
    if not FillSectors(AVolume,ADrive,DataStartSector,SectorsPerCluster,0) then Exit;

    Result:=True;
   end;
 end;
end;

{==============================================================================}

function TFATFormatter.CreateInfoSector(AVolume:TDiskVolume;ADrive:TDiskDrive;AFATType:TFATType;ABootSector:PBootSector;AInfoSector:PFATInfoSector):Boolean;
{Fill the Info Sector with values based on FAT type}
{Note: Caller must hold the volume lock}
var
 NumberOfFats:Word;
 SectorCount:LongWord;
 SectorsPerFat:LongWord;
 ReservedSectors:LongWord;
 DataStartSector:LongWord;
 DataClusterCount:LongWord;
 SectorsPerCluster:LongWord;
begin
 {}
 Result:=False;

 if ABootSector = nil then Exit;
 if AInfoSector = nil then Exit;

 {Get SectorCount}
 SectorCount:=0;
 if ADrive <> nil then SectorCount:=ADrive.SectorCount;
 if AVolume <> nil then SectorCount:=AVolume.SectorCount;

 {Check Type}
 case AFATType of
  ftFAT12,ftFAT16:begin
    Result:=True;
   end;
  ftFAT32:begin
    {Get Values}
    NumberOfFats:=PExtBootSector(ABootSector).BPB.NumberOfFats;
    SectorsPerFat:=PExtBootSector(ABootSector).BPB.SectorsPerFat32;
    ReservedSectors:=PExtBootSector(ABootSector).BPB.ReservedSectors;
    SectorsPerCluster:=PExtBootSector(ABootSector).BPB.SectorsPerCluster;

    {Get DataStartSector}
    DataStartSector:=(ReservedSectors + (SectorsPerFat * NumberOfFats));

    {Get DataClusterCount}
    DataClusterCount:=((SectorCount - DataStartSector) div SectorsPerCluster);

    {Create InfoSector}
    AInfoSector.LeadSignature:=fat32LeadSignature;
    {AInfoSector.Reserved1}
    AInfoSector.StructureSignature:=fat32StructSignature;
    AInfoSector.FreeClusterCount:=(DataClusterCount - 1);
    AInfoSector.LastFreeCluster:=fatUnknownCluster;
    {AInfoSector.Reserved2}
    AInfoSector.TrailSignature:=fat32TrailSignature;

    Result:=True;
   end;
 end;
end;

{==============================================================================}

function TFATFormatter.WriteInfoSector(AVolume:TDiskVolume;ADrive:TDiskDrive;AFATType:TFATType;ABootSector:PBootSector;AInfoSector:PFATInfoSector):Boolean;
{Write the created Info sector to disk and to backup info if needed}
{Note: Caller must hold the volume lock}
begin
 {}
 Result:=False;

 if ABootSector = nil then Exit;
 if AInfoSector = nil then Exit;

 {Check Type}
 case AFATType of
  ftFAT12,ftFAT16:begin
    Result:=True;
   end;
  ftFAT32:begin
    {Write InfoSector}
    if not WriteSectors(AVolume,ADrive,PExtBootSector(ABootSector).BPB.FileSysInfoSector,1,AInfoSector^) then Exit;

    {Write InfoBackup}
    if not WriteSectors(AVolume,ADrive,PExtBootSector(ABootSector).BPB.BackupBootSector + PExtBootSector(ABootSector).BPB.FileSysInfoSector,1,AInfoSector^) then Exit;

    Result:=True;
   end;
 end;
end;

{==============================================================================}

function TFATFormatter.AcceptVolume(AVolume:TDiskVolume;AFloppyType:TFloppyType;AFileSysType:TFileSysType):Boolean;
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

  {Accept Format Volume}
  {Check Volume}
  if AVolume.SectorCount = 0 then Exit;
  if AVolume.SectorSize <> MIN_SECTOR_SIZE then Exit;

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

function TFATFormatter.FormatVolume(AVolume:TDiskVolume;AFloppyType:TFloppyType;AFileSysType:TFileSysType):Boolean;
{Note: Caller must hold the volume writer lock}
var
 DriveNo:Integer;
 Drive:TDiskDrive;
 FATType:TFATType;
 BootSector:PBootSector;
 InfoSector:PFATInfoSector;
 SectorsPerCluster:LongWord;
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FRecognizer = nil then Exit;
  if AVolume = nil then Exit;
  if AVolume.Device = nil then Exit;

  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFormatter.FormatVolume (Name= ' + AVolume.Name + ')');
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

  {Allocate Boot Sector}
  BootSector:=GetMem(AVolume.SectorSize);
  if BootSector = nil then Exit;
  try
   FillChar(BootSector^,AVolume.SectorSize,0);

   SectorsPerCluster:=GetSectorsPerCluster(AVolume,nil,AFloppyType,AFileSysType,BootSector,FATType);
   if SectorsPerCluster = 0 then Exit;

   {Check FAT Type}
   case FATType of
    ftFAT12,ftFAT16:begin
      {Update Partition}
      if not UpdatePartitionId(AVolume,nil,FATType) then Exit;

      {Create Boot Sector}
      if not CreateBootSector(AVolume,nil,FATType,BootSector) then Exit;

      {Write Boot Sector}
      if not WriteBootSector(AVolume,nil,FATType,BootSector) then Exit;

      {Write Fat Table}
      if not WriteFatTable(AVolume,nil,FATType,BootSector) then Exit;

      {Write Root Directory}
      if not WriteRootDirectory(AVolume,nil,FATType,BootSector) then Exit;

      {Mount Volume}
      if not AVolume.MountVolume(DriveNo) then Exit;

      Result:=True;
     end;
    ftFAT32:begin
      {Allocate Info Sector}
      InfoSector:=GetMem(AVolume.SectorSize);
      if InfoSector = nil then Exit;
      try
       FillChar(InfoSector^,AVolume.SectorSize,0);

       {Update Partition}
       if not UpdatePartitionId(AVolume,nil,FATType) then Exit;

       {Create Boot Sector}
       if not CreateBootSector(AVolume,nil,FATType,BootSector) then Exit;

       {Write Boot Sector}
       if not WriteBootSector(AVolume,nil,FATType,BootSector) then Exit;

       {Write Fat Table}
       if not WriteFatTable(AVolume,nil,FATType,BootSector) then Exit;

       {Write Root Directory}
       if not WriteRootDirectory(AVolume,nil,FATType,BootSector) then Exit;

       {Create Info Sector}
       if not CreateInfoSector(AVolume,nil,FATType,BootSector,InfoSector) then Exit;

       {Write Info Sector}
       if not WriteInfoSector(AVolume,nil,FATType,BootSector,InfoSector) then Exit;

       {Mount Volume}
       if not AVolume.MountVolume(DriveNo) then Exit;

       Result:=True;
      finally
       FreeMem(InfoSector);
      end;
     end;
   end;
  finally
   FreeMem(BootSector);
  end;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}
{==============================================================================}
{TFATDefragger}

{==============================================================================}
{==============================================================================}
{TFATRepairer}

{==============================================================================}
{==============================================================================}
{TFATFileSystem}
constructor TFATFileSystem.Create(ADriver:TFileSysDriver;AVolume:TDiskVolume;ADrive:TDiskDrive);
begin
 {}
 inherited Create(ADriver,AVolume,ADrive);
 FFATType:=ftNONE;
 FVolumeFlags:=0;

 FCaseFlags:=True;
 FOemConvert:=True;
 FNumericTail:=True;

 FInfoSectorEnable:=True;
 FInfoImmediateUpdate:=False;

 FReadOnly:=False;
 FLongNames:=True;
 FDataStreams:=False;
 FReparsePoints:=False;
 FCaseSensitive:=False;
 FCasePreserved:=True;  {Enabled only if LongNames Enabled}
 FUnicodeNames:=True;   {Enabled only if LongNames Enabled}
 FPersistentAcls:=False;
 FFileCompression:=False;
 FVolumeQuotas:=False;
 FSparseFiles:=False;
 FRemoteStorage:=False;
 FVolumeCompressed:=False;
 FObjectIds:=False;
 FEncryption:=False;

 FBootCatalog:=False;
 FVirtualVolume:=False;
 FFolderEncryption:=False;
 FFolderCompression:=False;

 FLastFreeCluster:=fatUnknownCluster;
 FFreeClusterCount:=fatUnknownCluster;

 FInfoBuffer:=nil;
 FInfoLock:=MutexCreate;

 FNameBuffer:=nil;
 FNameLock:=MutexCreate;

 FReadBuffer:=nil;
 FReadLock:=MutexCreate;

 FWriteBuffer:=nil;
 FWriteLock:=MutexCreate;

 FClusterBuffer:=nil;
 FClusterLock:=MutexCreate;
end;

{==============================================================================}

destructor TFATFileSystem.Destroy;
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

  if FNameBuffer <> nil then FreeMem(FNameBuffer);
  FNameBuffer:=nil;
  MutexDestroy(FNameLock);

  if FInfoBuffer <> nil then FreeMem(FInfoBuffer);
  FInfoBuffer:=nil;
  MutexDestroy(FInfoLock);
 finally
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  inherited Destroy;
 end;
end;

{==============================================================================}

function TFATFileSystem.InfoLock:Boolean;
begin
 {}
 Result:=(MutexLock(FInfoLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TFATFileSystem.InfoUnlock:Boolean;
begin
 {}
 Result:=(MutexUnlock(FInfoLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TFATFileSystem.NameLock:Boolean;
begin
 {}
 Result:=(MutexLock(FNameLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TFATFileSystem.NameUnlock:Boolean;
begin
 {}
 Result:=(MutexUnlock(FNameLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TFATFileSystem.ReadLock:Boolean;
begin
 {}
 Result:=(MutexLock(FReadLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TFATFileSystem.ReadUnlock:Boolean;
begin
 {}
 Result:=(MutexUnlock(FReadLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TFATFileSystem.WriteLock:Boolean;
begin
 {}
 Result:=(MutexLock(FWriteLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TFATFileSystem.WriteUnlock:Boolean;
begin
 {}
 Result:=(MutexUnlock(FWriteLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TFATFileSystem.ClusterLock:Boolean;
begin
 {}
 Result:=(MutexLock(FClusterLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TFATFileSystem.ClusterUnlock:Boolean;
begin
 {}
 Result:=(MutexUnlock(FClusterLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TFATFileSystem.IsRemovable:Boolean;
begin
 {}
 Result:=False;

 if FDriver = nil then Exit;

 if FVolume = nil then Exit;

 Result:=FVolume.Removable;
end;

{==============================================================================}

function TFATFileSystem.GetHardError:Boolean;
{Note: Bit is on for Good and off for Error} {Opposite to CleanShutdown}
begin
 {}
 Result:=False;

 if not AcquireLock then Exit;
 try
  if FFATType = ftNONE then Exit;

  {Get Hard Error}
  Result:=not((FVolumeFlags and FHardError) = FHardError);
 finally
  ReleaseLock;
 end;
end;

{==============================================================================}

procedure TFATFileSystem.SetHardError(AValue:Boolean);
{Note: Bit is on for Good and off for Error} {Opposite to CleanShutdown}
begin
 {}
 if not AcquireLock then Exit;
 try
  if FFATType = ftNONE then Exit;

  {Set Hard Error}
  if AValue then
   begin
    FVolumeFlags:=(FVolumeFlags and not(FHardError));  {Turn Off}
   end
  else
   begin
    FVolumeFlags:=(FVolumeFlags or FHardError); {Turn On}
   end;
 finally
  ReleaseLock;
 end;
end;

{==============================================================================}

function TFATFileSystem.GetCleanShutdown:Boolean;
{Note: Bit is on for Clean and off for Dirty} {Opposite to HardError}
begin
 {}
 Result:=False;

 if not AcquireLock then Exit;
 try
  if FFATType = ftNONE then Exit;

  {Get Clean Shutdown}
  Result:=((FVolumeFlags and FCleanShutdown) = FCleanShutdown);
 finally
  ReleaseLock;
 end;
end;

{==============================================================================}

procedure TFATFileSystem.SetCleanShutdown(AValue:Boolean);
{Note: Bit is on for Clean and off for Dirty} {Opposite to HardError}
begin
 {}
 if not AcquireLock then Exit;
 try
  if FFATType = ftNONE then Exit;

  {Set Clean Shutdown}
  if AValue then
   begin
    FVolumeFlags:=(FVolumeFlags or FCleanShutdown); {Turn On}
   end
  else
   begin
    FVolumeFlags:=(FVolumeFlags and not(FCleanShutdown)); {Turn Off}
   end;
 finally
  ReleaseLock;
 end;
end;

{==============================================================================}

function TFATFileSystem.GetVolumeFlags:LongWord;
{Get the volume flags from cluster zero of the FAT}
begin
 {}
 Result:=0;

 if not AcquireLock then Exit;
 try
  if FFATType = ftNONE then Exit;

  //To Do //

 finally
  ReleaseLock;
 end;
end;

{==============================================================================}

function TFATFileSystem.SetVolumeFlags(AFlags:LongWord):Boolean;
{Set the volume flags in cluster zero of the FAT}
begin
 {}
 Result:=False;

 if not AcquireLock then Exit;
 try
  if FFATType = ftNONE then Exit;

  //To Do //

 finally
  ReleaseLock;
 end;
end;

{==============================================================================}

function TFATFileSystem.UpdateInfoSector:Boolean;
{Update the FAT32 Info Sector with current values}
begin
 {}
 Result:=False;

 {Check Buffer}
 if FInfoBuffer = nil then Exit;

 {Check ReadOnly}
 if FReadOnly then Exit;

 {Check Type}
 case FFATType of
  ftFAT12,ftFAT16:begin
    Result:=True;
   end;
  ftFAT32:begin
    if not InfoLock then Exit;
    try
     {Check Buffer}
     if (PFATInfoSector(FInfoBuffer).LeadSignature <> fat32LeadSignature) or
        (PFATInfoSector(FInfoBuffer).StructureSignature <> fat32StructSignature) or
        (PFATInfoSector(FInfoBuffer).TrailSignature <> fat32TrailSignature) then
      begin
       {$IFDEF FAT_DEBUG}
       if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.UpdateInfoSector - Reading Info Sector');
       {$ENDIF}

       {Read Info Sector}
       if not ReadSectors(FInfoSector,1,FInfoBuffer^) then Exit;
      end;

     {Check Enable}
     if InfoSectorEnable then
      begin
       {Update Free Count and Last Free}
       if FFreeClusterCount <> fatUnknownCluster then PFATInfoSector(FInfoBuffer).FreeClusterCount:=FFreeClusterCount;
       if FLastFreeCluster <> fatUnknownCluster then PFATInfoSector(FInfoBuffer).LastFreeCluster:=FLastFreeCluster;

       {$IFDEF FAT_DEBUG}
       if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.UpdateInfoSector - Writing Info Sector');
       {$ENDIF}

       {Write Info Sector}
       if not WriteSectors(FInfoSector,1,FInfoBuffer^) then Exit;

       {$IFDEF FAT_DEBUG}
       if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.UpdateInfoSector - Info Sector Updated (Free = ' + IntToHex(PFATInfoSector(FInfoBuffer).FreeClusterCount,8) + ' Last = ' + IntToHex(PFATInfoSector(FInfoBuffer).LastFreeCluster,8) + ')');
       {$ENDIF}
      end
     else
      begin
       {Check for Unknown Cluster}
       if (PFATInfoSector(FInfoBuffer).FreeClusterCount <> fatUnknownCluster) or
          (PFATInfoSector(FInfoBuffer).LastFreeCluster <> fatUnknownCluster) then
        begin
         {Reset Free Count and Last Free}
         PFATInfoSector(FInfoBuffer).FreeClusterCount:=fatUnknownCluster;
         PFATInfoSector(FInfoBuffer).LastFreeCluster:=fatUnknownCluster;

         {$IFDEF FAT_DEBUG}
         if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.UpdateInfoSector - Writing Info Sector');
         {$ENDIF}

         {Write Info Sector}
         if not WriteSectors(FInfoSector,1,FInfoBuffer^) then Exit;

         {$IFDEF FAT_DEBUG}
         if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.UpdateInfoSector - Info Sector Reset');
         {$ENDIF}
        end;
      end;

     Result:=True;
    finally
     InfoUnlock;
    end;
   end;
 end;
end;

{==============================================================================}

function TFATFileSystem.FillCluster(ACluster:LongWord;AValue:Byte):Boolean;
{Fill one cluster with the supplied value}
begin
 {}
 Result:=False;

 if not ClusterLock then Exit;
 try
  if FClusterBuffer = nil then Exit;

  {Read Cluster} {Dont need to read first}
  {if not ReadCluster(ACluster,FClusterBuffer^) then Exit;}

  {Fill Cluster}
  FillChar(FClusterBuffer^,FClusterSize,AValue);

  {Write Cluster}
  if not WriteCluster(ACluster,FClusterBuffer^) then Exit;

  Result:=True;
 finally
  ClusterUnlock;
 end;
end;

{==============================================================================}

function TFATFileSystem.ReadCluster(ACluster:LongWord;var ABuffer):Boolean;
{Read one Cluster from the Volume or Drive using Cache}
{Performs conversion of Cluster to Sector based on Offsets}
{Note: Sector is relative to StartSector and DataStartSector of the FileSystem}
var
 Sector:LongWord; //To Do //Int64
begin
 {}
 Result:=False;

 if FDriver = nil then Exit;
 if ACluster < FStartCluster then Exit;
 if FVolume = nil then Exit;
 if FVolume.Device = nil then Exit;
 if FSectorsPerCluster = 0 then Exit;

 {Calculate Sector (DataStartSector is Cluster 2)}
 {Sector:=((ACluster - FStartCluster) * FSectorsPerCluster);}
 Sector:=((ACluster - FStartCluster) shl FSectorShiftCount);

 Result:=FDriver.Cache.DeviceRead(FVolume.Device,FStartSector + FDataStartSector + Sector,FSectorsPerCluster,ABuffer);
end;

{==============================================================================}

function TFATFileSystem.WriteCluster(ACluster:LongWord;const ABuffer):Boolean;
{Write one Cluster to the Volume or Drive using Cache}
{Performs conversion of Cluster to Sector based on Offsets}
{Note: Sector is relative to StartSector and DataStartSector of the FileSystem}
var
 Sector:LongWord; //To Do //Int64
begin
 {}
 Result:=False;

 if FDriver = nil then Exit;
 if ACluster < FStartCluster then Exit;
 if FVolume = nil then Exit;
 if FVolume.Device = nil then Exit;
 if FSectorsPerCluster = 0 then Exit;

 {Check ReadOnly}
 if FReadOnly then Exit;

 {Calculate Sector (DataStartSector is Cluster 2)}
 {Sector:=((ACluster - FStartCluster) * FSectorsPerCluster);}
 Sector:=((ACluster - FStartCluster) shl FSectorShiftCount);

 Result:=FDriver.Cache.DeviceWrite(FVolume.Device,FStartSector + FDataStartSector + Sector,FSectorsPerCluster,ABuffer);
end;

{==============================================================================}

function TFATFileSystem.GetNextFreeCluster:LongWord;
{Note: For speed does direct FAT lookup instead of GetCluster}
{Note: For speed uses the LastFreeCluster after first lookup}
var
 Start:LongWord;
 Cluster:LongWord;

 BlockNo:LongWord;
 EntryCount:LongWord;
 ClusterOffset:LongWord;
 DiskBlock:TFATDiskBlock;
begin
 {}
 Result:=fatUnknownCluster;

 if not FBlocks.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FTotalClusterCount = 0 then Exit;

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Last Allocated}
  if FLastFreeCluster = fatUnknownCluster then
   begin
    {Check Info Sector}
    if FFATType = ftFAT32 then
     begin
      if InfoSectorEnable then
       begin
        if not SectorLock then Exit;
        try
         if FSectorBuffer = nil then Exit;

         {Get Info Sector}
         if not ReadSectors(FInfoSector,1,FSectorBuffer^) then Exit;
         if PFATInfoSector(FSectorBuffer).LeadSignature <> fat32LeadSignature then Exit;
         if PFATInfoSector(FSectorBuffer).StructureSignature <> fat32StructSignature then Exit;
         if PFATInfoSector(FSectorBuffer).TrailSignature <> fat32TrailSignature then Exit;
         if PFATInfoSector(FSectorBuffer).LastFreeCluster <> fatUnknownCluster then
          begin
           {Get Last Free Cluster}
           FLastFreeCluster:=PFATInfoSector(FSectorBuffer).LastFreeCluster;
          end;
        finally
         SectorUnlock;
        end;
       end
      else
       begin
        {Update Info Sector}
        UpdateInfoSector;
       end;
     end;
   end;

  {Get Start}
  Cluster:=0;
  if FLastFreeCluster <> fatUnknownCluster then Cluster:=FLastFreeCluster;
  Start:=Cluster;

  {Check each Cluster}
  while Cluster < FTotalClusterCount do
   begin
    if Cluster >= FStartCluster then
     begin
      {Get Block No}
      if FEntriesPerBlock = 0 then Exit;
      BlockNo:=((Cluster shr FBlockShiftCount) shl FBlockShiftCount);

      {Get Block}
      DiskBlock:=TFATDiskBlock(GetBlockEx(BlockNo,True));
      if DiskBlock = nil then Exit;

      {Check each Entry}
      EntryCount:=(Cluster - DiskBlock.BlockNo);
      while EntryCount < FEntriesPerBlock do
       begin
        {Check Type}
        ClusterOffset:=0;
        case FFATType of
         ftFAT12:begin
           {Get Offset}
           ClusterOffset:=((Cluster - DiskBlock.BlockNo) + ((Cluster - DiskBlock.BlockNo) div 2));  {Mutliply by 1.5 (Round Down)}
           if (Cluster and fatUnevenCluster) = 0 then
            begin
             {Entry is Even}
             if (Word(Pointer(PtrUInt(DiskBlock.BlockBuffer) + ClusterOffset)^) and $0FFF) and not(FReservedBits) = FFreeCluster then
              begin
               FLastFreeCluster:=Cluster;
               Result:=FLastFreeCluster;
               Exit;
              end;
            end
           else
            begin
             {Entry is Odd}
             if (Word(Pointer(PtrUInt(DiskBlock.BlockBuffer) + ClusterOffset)^) shr 4) and not(FReservedBits) = FFreeCluster then
              begin
               FLastFreeCluster:=Cluster;
               Result:=FLastFreeCluster;
               Exit;
              end;
            end;
          end;
         ftFAT16:begin
           {Get Offset}
           ClusterOffset:=((Cluster - DiskBlock.BlockNo) shl 1); {Multiply by SizeOf(Word)}
           if Word(Pointer(PtrUInt(DiskBlock.BlockBuffer) + ClusterOffset)^) and not(FReservedBits) = FFreeCluster then
            begin
             FLastFreeCluster:=Cluster;
             Result:=FLastFreeCluster;
             Exit;
            end;
          end;
         ftFAT32:begin
           {Get Offset}
           ClusterOffset:=((Cluster - DiskBlock.BlockNo) shl 2); {Multiply by SizeOf(LongWord)}
           if LongWord(Pointer(PtrUInt(DiskBlock.BlockBuffer) + ClusterOffset)^) and not(FReservedBits) = FFreeCluster then
            begin
             FLastFreeCluster:=Cluster;
             Result:=FLastFreeCluster;
             Exit;
            end;
          end;
        end;

        {Move next Entry}
        Inc(EntryCount);

        {Move next Cluster}
        Inc(Cluster);

        {Check for Wrap}
        if (Start > 0) and (Cluster = Start) then Exit;
        if (Start > 0) and (Cluster >= FTotalClusterCount) then Cluster:=0;
        if (Start > 0) and (Cluster = 0) then Break; {Break when end reached}
       end;
     end
    else
     begin
      {Move next Cluster}
      Inc(Cluster);

      {Check for Wrap}
      if (Start > 0) and (Cluster = Start) then Exit;
      if (Start > 0) and (Cluster >= FTotalClusterCount) then Cluster:=0;
     end;
   end;
 finally
  FBlocks.WriterUnlock;
 end;
end;

{==============================================================================}

function TFATFileSystem.GetFreeClusterCount:LongWord;
{Note: For speed does direct FAT lookup instead of GetCluster}
{Note: For speed uses the FreeClusterCount after first lookup}
var
 Cluster:LongWord;

 BlockNo:LongWord;
 EntryCount:LongWord;
 ClusterOffset:LongWord;
 DiskBlock:TFATDiskBlock;
begin
 {}
 Result:=0;

 if FDriver = nil then Exit;
 if FTotalClusterCount = 0 then Exit;

 {Check ReadOnly}
 if FReadOnly then Exit;

 {Check Free Count}
 if FFreeClusterCount = fatUnknownCluster then
  begin
   if not FBlocks.WriterLock then Exit;
   try
    {Check Info Sector}
    if FFATType = ftFAT32 then
     begin
      if InfoSectorEnable then
       begin
        if not SectorLock then Exit;
        try
         if FSectorBuffer = nil then Exit;

         {Get Info Sector}
         if not ReadSectors(FInfoSector,1,FSectorBuffer^) then Exit;
         if PFATInfoSector(FSectorBuffer).LeadSignature <> fat32LeadSignature then Exit;
         if PFATInfoSector(FSectorBuffer).StructureSignature <> fat32StructSignature then Exit;
         if PFATInfoSector(FSectorBuffer).TrailSignature <> fat32TrailSignature then Exit;
         if PFATInfoSector(FSectorBuffer).FreeClusterCount <> fatUnknownCluster then
          begin
           {Get Free Cluster Count}
           FFreeClusterCount:=PFATInfoSector(FSectorBuffer).FreeClusterCount;
           Result:=FFreeClusterCount;

           Exit;
          end;
        finally
         SectorUnlock;
        end;
       end
      else
       begin
        {Update Info Sector}
        UpdateInfoSector;
       end;
     end;

    {Get Start}
    Cluster:=0;
    FFreeClusterCount:=0;

    {Check each Cluster}
    while Cluster < FTotalClusterCount do
     begin
      if Cluster >= FStartCluster then
       begin
        {Get Block No}
        if FEntriesPerBlock = 0 then Exit;
        BlockNo:=((Cluster shr FBlockShiftCount) shl FBlockShiftCount);

        {Get Block}
        DiskBlock:=TFATDiskBlock(GetBlockEx(BlockNo,True));
        if DiskBlock = nil then Exit;

        {Check each Entry}
        EntryCount:=(Cluster - DiskBlock.BlockNo);
        while EntryCount < FEntriesPerBlock do
         begin
          {Check Type}
          ClusterOffset:=0;
          case FFATType of
           ftFAT12:begin
             {Get Offset}
             ClusterOffset:=((Cluster - DiskBlock.BlockNo) + ((Cluster - DiskBlock.BlockNo) div 2));  {Mutliply by 1.5 (Round Down)}
             if (Cluster and fatUnevenCluster) = 0 then
              begin
               {Entry is Even}
               if (Word(Pointer(PtrUInt(DiskBlock.BlockBuffer) + ClusterOffset)^) and $0FFF) and not(FReservedBits) = FFreeCluster then
                begin
                 Inc(FFreeClusterCount);
                end;
              end
             else
              begin
               {Entry is Odd}
               if (Word(Pointer(PtrUInt(DiskBlock.BlockBuffer) + ClusterOffset)^) shr 4) and not(FReservedBits) = FFreeCluster then
                begin
                 Inc(FFreeClusterCount);
                end;
              end;
            end;
           ftFAT16:begin
             {Get Offset}
             ClusterOffset:=((Cluster - DiskBlock.BlockNo) shl 1); {Multiply by SizeOf(Word)}
             if Word(Pointer(PtrUInt(DiskBlock.BlockBuffer) + ClusterOffset)^) and not(FReservedBits) = FFreeCluster then
              begin
               Inc(FFreeClusterCount);
              end;
            end;
           ftFAT32:begin
             {Get Offset}
             ClusterOffset:=((Cluster - DiskBlock.BlockNo) shl 2); {Multiply by SizeOf(LongWord)}
             if LongWord(Pointer(PtrUInt(DiskBlock.BlockBuffer) + ClusterOffset)^) and not(FReservedBits) = FFreeCluster then
              begin
               Inc(FFreeClusterCount);
              end;
            end;
          end;

          {Move next Entry}
          Inc(EntryCount);

          {Move next Cluster}
          Inc(Cluster);

          {Check Cluster}
          if Cluster >= FTotalClusterCount then Break;
         end;
       end
      else
       begin
        {Move next Cluster}
        Inc(Cluster);
       end;
     end;
   finally
    FBlocks.WriterUnlock;
   end;
  end;

 Result:=FFreeClusterCount;
end;

{==============================================================================}

function TFATFileSystem.SetNextFreeCluster(ACluster:LongWord):Boolean;
{Sets the last free cluster in the info sector on FAT32}
begin
 {}
 Result:=False;

 if FDriver = nil then Exit;
 if FTotalClusterCount = 0 then Exit;

 {Check Cluster}
 if (ACluster >= FStartCluster) and (ACluster < FTotalClusterCount) then
  begin
   {Check Type}
   if FFATType = ftFAT32 then
    begin
     if not SectorLock then Exit;
     try
      if FSectorBuffer = nil then Exit;

      {Get Info Sector}
      if not ReadSectors(FInfoSector,1,FSectorBuffer^) then Exit;
      if PFATInfoSector(FSectorBuffer).LeadSignature <> fat32LeadSignature then Exit;
      if PFATInfoSector(FSectorBuffer).StructureSignature <> fat32StructSignature then Exit;
      if PFATInfoSector(FSectorBuffer).TrailSignature <> fat32TrailSignature then Exit;

      {Set Last Free Cluster}
      PFATInfoSector(FSectorBuffer).LastFreeCluster:=ACluster;

      {Set Info Sector}
      if not WriteSectors(FInfoSector,1,FSectorBuffer^) then Exit;
     finally
      SectorUnlock;
     end;
    end;
  end;

 Result:=True;
end;

{==============================================================================}

function TFATFileSystem.SetFreeClusterCount(ACount:LongWord):Boolean;
{Sets the free cluster count in the info sector on FAT32}
begin
 {}
 Result:=False;

 if FDriver = nil then Exit;
 if FTotalClusterCount = 0 then Exit;

 {Check Count}
 if ACount < FTotalClusterCount then
  begin
   {Check Type}
   if FFATType = ftFAT32 then
    begin
     if not SectorLock then Exit;
     try
      if FSectorBuffer = nil then Exit;

      {Get Info Sector}
      if not ReadSectors(FInfoSector,1,FSectorBuffer^) then Exit;
      if PFATInfoSector(FSectorBuffer).LeadSignature <> fat32LeadSignature then Exit;
      if PFATInfoSector(FSectorBuffer).StructureSignature <> fat32StructSignature then Exit;
      if PFATInfoSector(FSectorBuffer).TrailSignature <> fat32TrailSignature then Exit;

      {Set Free Cluster Count}
      PFATInfoSector(FSectorBuffer).FreeClusterCount:=ACount;

      {Set Info Sector}
      if not WriteSectors(FInfoSector,1,FSectorBuffer^) then Exit;
     finally
      SectorUnlock;
     end;
    end;
  end;

 Result:=True;
end;

{==============================================================================}

function TFATFileSystem.GetStartCluster(AEntry:TDiskEntry):LongWord;
{Get the starting cluster number or zero for FAT12/16 root}
{Handles FAT12/16 Root special case}
{Note: Differs from GetParentCluster below. Used by AllocDirectory}
{Note: The Root entry on FAT32 has an actual Start cluster unlike FAT12/16}
begin
 {}
 Result:=FFreeCluster;

 if AEntry = nil then Exit;

 {Check for Root}
 if (FFATType = ftFAT32) or (AEntry <> FRoot) then
  begin
   Result:=TFATDiskEntry(AEntry).StartCluster;
  end;
end;

{==============================================================================}

function TFATFileSystem.GetParentCluster(AParent:TDiskEntry):LongWord;
{Get the starting cluster number of the supplied parent or zero for root}
{Handles FAT12/16 Root special case}
{Note: Differs from GetStartCluster above. Used by AddEntry}
{Note: The DotDot entry must always point to zero for Root even on FAT32}
begin
 {}
 Result:=FFreeCluster;

 if AParent = nil then Exit;

 {Check for Root}
 if AParent = FRoot then Exit;

 {Get Start Cluster}
 Result:=TFATDiskEntry(AParent).StartCluster;
end;

{==============================================================================}

function TFATFileSystem.CheckClusterBlock(ACluster,ANext:LongWord):Boolean;
{Check if 2 clusters are in the same block for SetCluster commit}
var
 NextBlock:LongWord;
 ClusterBlock:LongWord;
begin
 {}
 Result:=False;

 if ANext < FStartCluster then Exit;
 if ACluster < FStartCluster then Exit;

 {Get Next Block}
 if FEntriesPerBlock = 0 then Exit;
 NextBlock:=((ANext shr FBlockShiftCount) shl FBlockShiftCount);

 {Get Cluster Block}
 ClusterBlock:=((ACluster shr FBlockShiftCount) shl FBlockShiftCount);
 if NextBlock <> ClusterBlock then Exit;

 Result:=True;
end;

{==============================================================================}

function TFATFileSystem.GetNextChainCluster(AParent:LongWord):LongWord;
{Get the next cluster in the chain after the supplied cluster}
{Returns zero on end of chain or error}
var
 Cluster:LongWord;
begin
 {}
 Result:=0;

 {$IFDEF FAT_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.GetNextChainCluster Parent = ' + IntToStr(AParent));
 {$ENDIF}

 if AParent < FStartCluster then Exit;

 {Get Next}
 Cluster:=GetCluster(AParent);

 {$IFDEF FAT_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.GetNextChainCluster Cluster = ' + IntToStr(Cluster));
 {$ENDIF}

 {Check Next}
 if Cluster >= FEndOfFile then Exit;
 if Cluster = FBadCluster then Exit;
 if Cluster = FFreeCluster then Exit;

 {Return Next}
 Result:=Cluster;

 {$IFDEF FAT_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.GetNextChainCluster completed Result = ' + IntToStr(Result));
 {$ENDIF}
end;

{==============================================================================}

function TFATFileSystem.GetLastChainCluster(AParent:LongWord):LongWord;
{Get the last cluster in the chain of the supplied cluster}
{Returns zero on error or parent on end of chain}
var
 Cluster:LongWord;
begin
 {}
 Result:=0;

 {$IFDEF FAT_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.GetLastChainCluster Parent = ' + IntToStr(AParent));
 {$ENDIF}

 if AParent < FStartCluster then Exit;

 {Check Parent}
 if AParent = FBadCluster then Exit;
 if AParent = FFreeCluster then Exit;

 {Return Parent}
 Result:=AParent;

 {Get Next}
 Cluster:=GetCluster(AParent);
 while Cluster < FEndOfFile do
  begin
   {Check Next}
   if Cluster = FBadCluster then Exit;
   if Cluster = FFreeCluster then Exit;

   {Return Next}
   Result:=Cluster;

   {Get Next}
   Cluster:=GetCluster(Cluster);
  end;

 {$IFDEF FAT_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.GetLastChainCluster completed Result = ' + IntToStr(Result));
 {$ENDIF}
end;

{==============================================================================}

function TFATFileSystem.GetChainClusterCount(AParent:LongWord):LongWord;
{Get the cluster count in the chain of the supplied cluster}
{Returns zero on error}
var
 Cluster:LongWord;
begin
 {}
 Result:=0;

 {$IFDEF FAT_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.GetChainClusterCount Parent = ' + IntToStr(AParent));
 {$ENDIF}

 if AParent < FStartCluster then Exit;

 {Get Next}
 Cluster:=GetCluster(AParent);
 while Cluster < FEndOfFile do
  begin
   {Check Next}
   if Cluster = FBadCluster then Exit;
   if Cluster = FFreeCluster then Exit;

   {Count Next}
   Inc(Result);

   {Get Next}
   Cluster:=GetCluster(Cluster);
  end;

 {Check Last (Includes Parent)}
 if Cluster = FBadCluster then Exit;
 if Cluster = FFreeCluster then Exit;

 {Count Last (Includes Parent)}
 Inc(Result);

 {$IFDEF FAT_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.GetChainClusterCount completed Result = ' + IntToStr(Result));
 {$ENDIF}
end;

{==============================================================================}

function TFATFileSystem.GetCluster(ACluster:LongWord):LongWord;
var
 BlockNo:LongWord;
 ClusterOffset:LongWord;
 DiskBlock:TFATDiskBlock;
begin
 {}
 Result:=fatUnknownCluster;

 {$IFDEF FAT_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.GetCluster Cluster = ' + IntToStr(ACluster));
 {$ENDIF}

 if not FBlocks.WriterLock then Exit;
 try
  if ACluster >= FTotalClusterCount then Exit;

  {Get Block No}
  if FEntriesPerBlock = 0 then Exit;
  {BlockNo:=((ACluster div FEntriesPerBlock) * FEntriesPerBlock);}
  BlockNo:=((ACluster shr FBlockShiftCount) shl FBlockShiftCount);

  {Get Block}
  DiskBlock:=TFATDiskBlock(GetBlockEx(BlockNo,True));
  if DiskBlock = nil then Exit;

  {Check Type}
  ClusterOffset:=0;
  case FFATType of
   ftFAT12:begin
     {Get Offset}
     ClusterOffset:=((ACluster - DiskBlock.BlockNo) + ((ACluster - DiskBlock.BlockNo) div 2));  {Mutliply by 1.5 (Round Down)}
     if (ACluster and fatUnevenCluster) = 0 then
      begin
       {Entry is Even}
       Result:=(Word(Pointer(PtrUInt(DiskBlock.BlockBuffer) + ClusterOffset)^) and $0FFF) and not(FReservedBits);
      end
     else
      begin
       {Entry is Odd}
       Result:=(Word(Pointer(PtrUInt(DiskBlock.BlockBuffer) + ClusterOffset)^) shr 4) and not(FReservedBits);
      end;
    end;
   ftFAT16:begin
     {Get Offset}
     ClusterOffset:=((ACluster - DiskBlock.BlockNo) shl 1); {Multiply by SizeOf(Word)}
     Result:=Word(Pointer(PtrUInt(DiskBlock.BlockBuffer) + ClusterOffset)^) and not(FReservedBits);
    end;
   ftFAT32:begin
     {Get Offset}
     ClusterOffset:=((ACluster - DiskBlock.BlockNo) shl 2); {Multiply by SizeOf(LongWord)}
     Result:=LongWord(Pointer(PtrUInt(DiskBlock.BlockBuffer) + ClusterOffset)^) and not(FReservedBits);
    end;
  end;

  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.GetCluster Result = ' + IntToStr(Result));

  {First and Last Clusters}
  if (FILESYS_LOG_ENABLED) and (ACluster = 0) then FileSysLogDebug('TFATFileSystem.GetCluster - Media Cluster = ' + IntToHex(ACluster,8) + ' ClusterOffset = ' + IntToStr(ClusterOffset) + ' BlockSector = ' + IntToStr(DiskBlock.BlockSector) + ' NextCluster = ' + IntToHex(Result,8));
  if (FILESYS_LOG_ENABLED) and (ACluster = 1) then FileSysLogDebug('TFATFileSystem.GetCluster - Reserved Cluster = ' + IntToHex(ACluster,8) + ' ClusterOffset = ' + IntToStr(ClusterOffset) + ' BlockSector = ' + IntToStr(DiskBlock.BlockSector) + ' NextCluster = ' + IntToHex(Result,8));
  if (FILESYS_LOG_ENABLED) and (ACluster = FStartCluster) then FileSysLogDebug('TFATFileSystem.GetCluster - First Cluster = ' + IntToHex(ACluster,8) + ' ClusterOffset = ' + IntToStr(ClusterOffset) + ' BlockSector = ' + IntToStr(DiskBlock.BlockSector) + ' NextCluster = ' + IntToHex(Result,8));
  if (FILESYS_LOG_ENABLED) and (ACluster = (FTotalClusterCount - 1)) then FileSysLogDebug('TFATFileSystem.GetCluster - Last Cluster = ' + IntToHex(ACluster,8) + ' ClusterOffset = ' + IntToStr(ClusterOffset) + ' BlockSector = ' + IntToStr(DiskBlock.BlockSector) + ' NextCluster = ' + IntToHex(Result,8));

  {Start and End Clusters}
  if (FILESYS_LOG_ENABLED) and (ACluster = DiskBlock.BlockNo) then FileSysLogDebug('TFATFileSystem.GetCluster - Start Cluster = ' + IntToHex(ACluster,8) + ' ClusterOffset = ' + IntToStr(ClusterOffset) + ' BlockSector = ' + IntToStr(DiskBlock.BlockSector) + ' NextCluster = ' + IntToHex(Result,8));
  if (FILESYS_LOG_ENABLED) and (ACluster = (DiskBlock.BlockNo + FEntriesPerBlock - 1)) then FileSysLogDebug('TFATFileSystem.GetCluster - End Cluster = ' + IntToHex(ACluster,8) + ' ClusterOffset = ' + IntToStr(ClusterOffset) + ' BlockSector = ' + IntToStr(DiskBlock.BlockSector) + ' NextCluster = ' + IntToHex(Result,8));
  {$ENDIF}
 finally
  FBlocks.WriterUnlock;
 end;
end;

{==============================================================================}

function TFATFileSystem.SetCluster(ACluster,AValue:LongWord;ACommit:Boolean):Boolean;
var
 BlockNo:LongWord;
 ClusterOffset:LongWord;
 DiskBlock:TFATDiskBlock;
begin
 {}
 Result:=False;

 if not FBlocks.WriterLock then Exit;
 try
  if ACluster >= FTotalClusterCount then Exit;

  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.SetCluster - Cluster = ' + IntToHex(ACluster,8) + ' Value = ' + IntToHex(AValue,8));
  {$ENDIF}

  {Get Block No}
  if FEntriesPerBlock = 0 then Exit;
  {BlockNo:=((ACluster div FEntriesPerBlock) * FEntriesPerBlock);}
  BlockNo:=((ACluster shr FBlockShiftCount) shl FBlockShiftCount);

  {Get Block}
  DiskBlock:=TFATDiskBlock(GetBlockEx(BlockNo,True));
  if DiskBlock = nil then Exit;

  {Check Type}
  case FFATType of
   ftFAT12:begin
     {Get Offset}
     ClusterOffset:=((ACluster - DiskBlock.BlockNo) + ((ACluster - DiskBlock.BlockNo) div 2));  {Mutliply by 1.5 (Round Down)}
     if (ACluster and fatUnevenCluster) = 0 then
      begin
       {Entry is Even}
       AValue:=AValue and $0FFF;
       Word(Pointer(PtrUInt(DiskBlock.BlockBuffer) + ClusterOffset)^):=Word(Pointer(PtrUInt(DiskBlock.BlockBuffer) + ClusterOffset)^) and $F000;
      end
     else
      begin
       {Entry is Odd}
       AValue:=AValue shl 4;
       Word(Pointer(PtrUInt(DiskBlock.BlockBuffer) + ClusterOffset)^):=Word(Pointer(PtrUInt(DiskBlock.BlockBuffer) + ClusterOffset)^) and $000F;
      end;

     Word(Pointer(PtrUInt(DiskBlock.BlockBuffer) + ClusterOffset)^):=Word(Pointer(PtrUInt(DiskBlock.BlockBuffer) + ClusterOffset)^) or AValue;
    end;
   ftFAT16:begin
     {Get Offset}
     ClusterOffset:=((ACluster - DiskBlock.BlockNo) shl 1); {Multiply by 2}
     Word(Pointer(PtrUInt(DiskBlock.BlockBuffer) + ClusterOffset)^):=AValue;
    end;
   ftFAT32:begin
     {Get Offset}
     ClusterOffset:=((ACluster - DiskBlock.BlockNo) shl 2); {Multiply by 4}
     AValue:=AValue and not(FReservedBits);
     LongWord(Pointer(PtrUInt(DiskBlock.BlockBuffer) + ClusterOffset)^):=LongWord(Pointer(PtrUInt(DiskBlock.BlockBuffer) + ClusterOffset)^) and FReservedBits;
     LongWord(Pointer(PtrUInt(DiskBlock.BlockBuffer) + ClusterOffset)^):=LongWord(Pointer(PtrUInt(DiskBlock.BlockBuffer) + ClusterOffset)^) or AValue;
    end;
  end;

  {Set Block}
  if ACommit then Result:=SetBlock(DiskBlock) else Result:=True;
 finally
  FBlocks.WriterUnlock;
 end;
end;

{==============================================================================}

function TFATFileSystem.AllocCluster(AParent:LongWord;var ACluster:LongWord;ACount:LongWord):Boolean;
{Allocate count clusters from next free}
{Add to the parent chain if supplied}
var
 AllocCount:LongWord;
 PrevCluster:LongWord;
 NextCluster:LongWord;
 StartCluster:LongWord;
begin
 {}
 Result:=False;

 if not FBlocks.WriterLock then Exit;
 try
  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.AllocCluster Parent = ' + IntToHex(AParent,8) + ' Count = ' + IntToStr(ACount));
  {$ENDIF}

  {Check Count}
  if ACount < 1 then Exit;

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Free}
  if FFreeClusterCount = fatUnknownCluster then GetFreeClusterCount;

  {Set Count}
  AllocCount:=0; //To Do //Could use Result if Result was a Count of allocated
  PrevCluster:=0;
  StartCluster:=0;
  while AllocCount < ACount do
   begin
    {Get Free}
    NextCluster:=GetNextFreeCluster;
    if NextCluster < FStartCluster then Exit;
    if NextCluster = fatUnknownCluster then Exit;

    {$IFDEF FAT_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.AllocCluster Cluster = ' + IntToHex(NextCluster,8) + ' Count = ' + IntToStr(AllocCount));
    {$ENDIF}

    {Check Count}
    if AllocCount = 0 then
     begin
      {First}
      {Set Start}
      StartCluster:=NextCluster;

      {Allocate Cluster}
      if not SetCluster(NextCluster,FEndOfCluster,(ACount = 1)) then Exit;

      {Set Previous}
      PrevCluster:=NextCluster;
     end
    else if (AllocCount = (ACount - 1)) then
     begin
      {Last}
      {Allocate Cluster}
      if not SetCluster(NextCluster,FEndOfCluster,not(CheckClusterBlock(PrevCluster,NextCluster))) then Exit;

      {Update Previous}
      if not SetCluster(PrevCluster,NextCluster,True) then Exit;

      {Set Previous}
      PrevCluster:=NextCluster;
     end
    else
     begin
      {Other}
      {Allocate Cluster}
      if not SetCluster(NextCluster,FEndOfCluster,False) then Exit;

      {Update Previous}
      if not SetCluster(PrevCluster,NextCluster,not(CheckClusterBlock(PrevCluster,NextCluster))) then Exit;

      {Set Previous}
      PrevCluster:=NextCluster;
     end;

    {Update Free}
    if FFreeClusterCount < FTotalClusterCount then Dec(FFreeClusterCount);

    {Update Count}
    Inc(AllocCount);
   end;

  {Return Start}
  ACluster:=StartCluster;

  {Check Parent}
  if AParent >=FStartCluster then
   begin
    {Get Last}
    AParent:=GetLastChainCluster(AParent);
    if AParent < FStartCluster then Exit;

    {$IFDEF FAT_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.AllocCluster Last = ' + IntToHex(AParent,8));
    {$ENDIF}

    {Update Chain}
    if not SetCluster(AParent,ACluster,True) then Exit;
   end;

  {Update Info Sector}
  if (InfoSectorEnable and InfoImmediateUpdate) then UpdateInfoSector;

  Result:=True;
 finally
  FBlocks.WriterUnlock;
 end;
end;

{==============================================================================}

function TFATFileSystem.ReleaseCluster(AParent,ACluster:LongWord):Boolean;
{Release this cluster and all clusters to the end of the chain}
{Update the parent cluster if supplied}
var
 Current:LongWord;
begin
 {}
 Result:=False;

 if not FBlocks.WriterLock then Exit;
 try
  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Free}
  if FFreeClusterCount = fatUnknownCluster then GetFreeClusterCount;

  {Check Cluster} {Allows for zero length files}
  if ACluster >= FStartCluster then
   begin
    {$IFDEF FAT_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.ReleaseCluster Parent = ' + IntToHex(AParent,8) + ' Cluster = ' + IntToHex(ACluster,8));
    {$ENDIF}

    {Check Parent}
    if AParent >=FStartCluster then
     begin
      {Check Chain}
      if GetNextChainCluster(AParent) <> ACluster then Exit;

      {Update Chain}
      if not SetCluster(AParent,FEndOfCluster,True) then Exit;
     end;

    {Check Cluster}
    Current:=ACluster;
    while Current > 0 do {GetNextChainCluster returns 0 on failure (Bad/Free/Last)}
     begin
      {Get Next}
      ACluster:=GetNextChainCluster(ACluster);

      {Release Cluster}
      if not SetCluster(Current,FFreeCluster,not(CheckClusterBlock(Current,ACluster))) then Exit;

      {Update Next and Free}
      if Current < FLastFreeCluster then FLastFreeCluster:=Current;
      if FFreeClusterCount < FTotalClusterCount then Inc(FFreeClusterCount);

      {Check Next}
      if ACluster < FStartCluster then Break; {Break to allow completion}

      Current:=ACluster;
     end;
   end;

  {Update Info Sector}
  if (InfoSectorEnable and InfoImmediateUpdate) then UpdateInfoSector;

  Result:=True;
 finally
  FBlocks.WriterUnlock;
 end;
end;

{==============================================================================}

function TFATFileSystem.CheckDirectoryRoot(AParent:TDiskEntry):Boolean;
{Check for the FAT12/16 Root directory}
{Handles FAT12/16 Root special case}
begin
 {}
 Result:=False;

 if AParent = nil then Exit;

 {Check for FAT32}
 if FFATType = ftFAT32 then Exit;

 {Check for Root}
 if AParent <> FRoot then Exit;

 Result:=True;
end;

{==============================================================================}

function TFATFileSystem.CheckDirectoryStart(AParent:TDiskEntry):Boolean;
{Check that the start cluster/sector of the directory is valid}
{Handles FAT12/16 Root special case}
begin
 {}
 Result:=False;

 if AParent = nil then Exit;

 {Check for Root}
 if (FFATType <> ftFAT32) and (AParent = FRoot) then
  begin
   {FAT12/16 Root}
   if TFATDiskEntry(AParent).StartSector <> FRootStartSector then Exit;
  end
 else
  begin
   {FAT32 or FAT12/16 non Root}
   if TFATDiskEntry(AParent).StartCluster < FStartCluster then Exit;
  end;

 Result:=True;
end;

{==============================================================================}

function TFATFileSystem.GetFirstDirectorySector(AParent:TDiskEntry;var ASector:LongWord):Boolean;
{Returns the First Sector of the Directory entries for the parent}
{Handles FAT12/16 Root special case}
begin
 {}
 Result:=False;

 if AParent = nil then Exit;

 {$IFDEF FAT_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.GetFirstDirectorySector Parent = ' + AParent.Name);
 {$ENDIF}

 {Check for Root}
 if (FFATType <> ftFAT32) and (AParent = FRoot) then
  begin
   {FAT12/16 Root}
   if TFATDiskEntry(AParent).StartSector <> FRootStartSector then Exit;
   ASector:=0;

   Result:=True;
  end
 else
  begin
   {FAT32 or FAT12/16 non Root}
   if TFATDiskEntry(AParent).StartCluster < FStartCluster then Exit;
   ASector:=((TFATDiskEntry(AParent).StartCluster - FStartCluster) shl FSectorShiftCount);

   Result:=True;
  end;

 {$IFDEF FAT_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.GetFirstDirectorySector completed Sector = ' + IntToStr(ASector));
 {$ENDIF}
end;

{==============================================================================}

function TFATFileSystem.GetNextDirectorySector(AParent:TDiskEntry;var ASector:LongWord;AWrite:Boolean):Boolean;
{Returns the Next Sector of the Directory entries for the parent}
{Handles FAT12/16 Root special case}
var
 Sector:LongWord;
 Cluster:LongWord;
begin
 {}
 Result:=False;

 if AParent = nil then Exit;

 {$IFDEF FAT_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.GetNextDirectorySector Parent = ' + AParent.Name + ' Sector = ' + IntToStr(ASector));
 {$ENDIF}

 {Check for Root}
 if (FFATType <> ftFAT32) and (AParent = FRoot) then
  begin
   {FAT12/16 Root}
   if ASector = (FRootSectorCount - 1) then Exit;
   Inc(ASector);

   Result:=True;
  end
 else
  begin
   {FAT32 or FAT12/16 non Root}
   Cluster:=((ASector shr FSectorShiftCount) + FStartCluster);
   Sector:=((Cluster - FStartCluster) shl FSectorShiftCount);
   if (AWrite) and ((ASector - Sector) < (FSectorsPerCluster - 1)) then
    begin
     Inc(ASector);

     Result:=True;
    end
   else
    begin
     Cluster:=GetNextChainCluster(Cluster);
     if Cluster < FStartCluster then Exit;
     ASector:=((Cluster - FStartCluster) shl FSectorShiftCount);

     Result:=True;
    end;
  end;

 {$IFDEF FAT_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.GetNextDirectorySector completed Sector = ' + IntToStr(ASector));
 {$ENDIF}
end;

{==============================================================================}

function TFATFileSystem.GetDirectorySectorCount(AParent:TDiskEntry;AWrite:Boolean):LongWord;
{Returns the Sector Count per block of Directory entries for the parent}
{Handles FAT12/16 Root special case}
begin
 {}
 Result:=0;

 if AParent = nil then Exit;

 {Check for Root or Write}
 if ((FFATType <> ftFAT32) and (AParent = FRoot) or AWrite) then
  begin
   {FAT12/16 Root or Write}
   Result:=1; {Only read 1 sector in FAT12/16 Root} {Always Write 1 sector}
  end
 else
  begin
   {FAT32 or FAT12/16 non Root Read}
   Result:=FSectorsPerCluster;
  end;
end;

{==============================================================================}

function TFATFileSystem.GetDirectorySectorOffset(AParent:TDiskEntry):LongWord;
{Returns the Sector Offset of Directory entries for the parent}
{Handles FAT12/16 Root special case}
begin
 {}
 Result:=0;

 if AParent = nil then Exit;

 {Check for Root}
 if (FFATType <> ftFAT32) and (AParent = FRoot) then
  begin
   {FAT12/16 Root}
   Result:=FRootStartSector;
  end
 else
  begin
   {FAT32 or FAT12/16 non Root}
   Result:=FDataStartSector;
  end;
end;

{==============================================================================}

function TFATFileSystem.AllocDirectory(AParent:TDiskEntry;ACount:Byte;var AOffset,ASector:LongWord):Boolean;
{Allocate Count contiguous Directories from free in the Parent entries}
{Allocate new Cluster if no free available (and not FAT12/16 Root)}
{Note: Offset and Sector are undefined on call}
{Note: Offset and Sector point to the first directory entry on return}
var
 Cluster:LongWord;
 StartCluster:LongWord;

 EntryCount:Byte;
 EntryOffset:LongWord;
 BlockOffset:LongWord;
 EntrySector:LongWord;
 BlockSector:LongWord;
 SectorCount:LongWord;
 SectorOffset:LongWord;
 Directory:PLFNDirectory;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if ACount < 1 then Exit;
  if FDriver = nil then Exit;
  if AParent = nil then Exit;

  {Check ReadOnly}
  if FReadOnly then Exit;

  if not ClusterLock then Exit;
  try
   if FClusterBuffer = nil then Exit;

   {$IFDEF FAT_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.AllocDirectory Parent = ' + AParent.Name);
   {$ENDIF}

   {Check Start}
   if not CheckDirectoryStart(AParent) then Exit;

   {Get Offsets}
   SectorCount:=GetDirectorySectorCount(AParent,False);
   SectorOffset:=GetDirectorySectorOffset(AParent);

   {Get Start Sector}
   EntryCount:=ACount;
   BlockOffset:=0;
   if not GetFirstDirectorySector(AParent,BlockSector) then Exit;

   repeat {First Sector will be zero for first data cluster}
    {Get Start Entry}
    EntryOffset:=0;
    EntrySector:=BlockSector;

    {$IFDEF FAT_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.AllocDirectory DirectorySector = ' + IntToStr(BlockSector));
    {$ENDIF}

    {Read Sectors}
    if not ReadSectors(SectorOffset + BlockSector,SectorCount,FClusterBuffer^) then Exit; {Note: SectorCount will never be more than SectorPerCluster}

    {Read Block}
    while BlockOffset < (FSectorSize * SectorCount) do
     begin
      {Read Entries}
      while EntryOffset < FSectorSize do
       begin
        {Get Directory}
        Directory:=PLFNDirectory(PtrUInt(FClusterBuffer) + BlockOffset + EntryOffset);

        {Check for Free}
        if (Directory.Order = fatEntryFreeAll) or (Directory.Order = fatEntryFree) then
         begin
          {$IFDEF FAT_DEBUG}
          if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.AllocDirectory FreeSector = ' + IntToStr(EntrySector) + ' Offset = ' + IntToStr(EntryOffset));
          {$ENDIF}

          {Save Entry}
          if EntryCount = ACount then
           begin
            AOffset:=EntryOffset;
            ASector:=EntrySector;
           end;

          {Update Count}
          Dec(EntryCount);

          {Return Entry}
          if EntryCount = 0 then
           begin
            Result:=True;
            Exit;
           end;
         end
        else
         begin
          {Reset Count}
          EntryCount:=ACount;
         end;

        {Get Next Entry}
        Inc(EntryOffset,fatEntrySize);
       end;

      {Get Next Block}
      EntryOffset:=0;
      Inc(EntrySector);
      Inc(BlockOffset,FSectorSize);
     end;

    {Get Next Sector}
    BlockOffset:=0;
   until not GetNextDirectorySector(AParent,BlockSector,False); {GetNextDirectorySector returns False on failure (Invalid/Last)}
  finally
   ClusterUnlock;
  end;

  {Check for Root}
  if not CheckDirectoryRoot(AParent) then
   begin
    {Get Start}
    StartCluster:=GetStartCluster(AParent);
    if StartCluster < FStartCluster then Exit;
    //To Do //Check Chain Count - Not more than ?? (64KB ?) - See docs

    {Allocate Cluster}
    if not AllocCluster(StartCluster,Cluster,1) then Exit;

    {$IFDEF FAT_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.AllocDirectory DirectoryStart = ' + IntToHex(StartCluster,8) + ' Cluster = ' + IntToHex(Cluster,8));
    {$ENDIF}

    {Zero Cluster}
    if not FillCluster(Cluster,0) then Exit;

    {Cluster Allocated Call AllocDirectory}
    Result:=AllocDirectory(AParent,ACount,AOffset,ASector);
   end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{==============================================================================}

function TFATFileSystem.ReleaseDirectory(AParent:TDiskEntry;ACount:Byte;AOffset,ASector:LongWord):Boolean;
{Release Count contiguous Directories to free in the Parent entries}
{Note: Offset and Sector point to the first directory entry on call}
var
 EntryCount:Byte;
 EntryOffset:LongWord;
 EntrySector:LongWord;
 SectorCount:LongWord;
 SectorOffset:LongWord;
 Directory:PLFNDirectory;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if ACount < 1 then Exit;
  if FDriver = nil then Exit;
  if AParent = nil then Exit;

  {Check ReadOnly}
  if FReadOnly then Exit;

  if not ClusterLock then Exit;
  try
   if FClusterBuffer = nil then Exit;

   {$IFDEF FAT_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.ReleaseDirectory Parent = ' + AParent.Name);
   {$ENDIF}

   {Check Start}
   if not CheckDirectoryStart(AParent) then Exit;

   {Get Offsets}
   SectorCount:=GetDirectorySectorCount(AParent,True); {SectorCount is always one on Write}
   SectorOffset:=GetDirectorySectorOffset(AParent);
   if (SectorCount = 0) or (SectorOffset = 0) then Exit;

   {Get Start Sector}
   EntryCount:=ACount;
   EntryOffset:=AOffset;
   EntrySector:=ASector;
   while EntryCount > 0 do
    begin
     {Get Sectors}
     if not ReadSectors(SectorOffset + EntrySector,SectorCount,FClusterBuffer^) then Exit; {Note: SectorCount will never be more than SectorPerCluster}

     {Read Entries}
     while EntryOffset < FSectorSize do
      begin
       {$IFDEF FAT_DEBUG}
       if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.ReleaseDirectory EntrySector = ' + IntToStr(EntrySector) + ' Offset = ' + IntToStr(EntryOffset));
       {$ENDIF}

       {Modify Directory}
       Directory:=PLFNDirectory(PtrUInt(FClusterBuffer) + EntryOffset);
       Directory.Order:=fatEntryFree;
       Dec(EntryCount);
       if EntryCount = 0 then Break; {Break to allow completion}

       {Get Next Entry}
       Inc(EntryOffset,fatEntrySize);
      end;

     {Set Sectors}
     if not WriteSectors(SectorOffset + EntrySector,SectorCount,FClusterBuffer^) then Exit;

     {Get Next Sector}
     EntryOffset:=0;

     if not GetNextDirectorySector(AParent,EntrySector,True) then Break; {GetNextDirectorySector returns False on failure (Invalid/Last)} {Break to allow completion}
    end;

   Result:=True;
 finally
  ClusterUnlock;
 end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{==============================================================================}

function TFATFileSystem.GetBlockShiftCount(ASize:Word;AType:TFATType):Word;
{From the table get the block shift count value for this sector size}
var
 Count:Integer;
begin
 {}
 Result:=0;

 if FDriver = nil then Exit;

 {Check the Table}
 for Count:=0 to fatMaxParams do
  begin
   if (fatParams[Count].SectorSize = ASize) and (fatParams[Count].FATType = AType) then
    begin
     Result:=fatParams[Count].BlockShiftCount;
     Exit;
    end;
  end;
end;

{==============================================================================}

function TFATFileSystem.GetSectorShiftCount(ASectorsPerCluster:LongWord):Word;
{Calculate the sector shift count for sector to cluster conversion}
begin
 {}
 Result:=0;

 if FDriver = nil then Exit;

 {Get the Shift Count}
 while (1 shl Result) < ASectorsPerCluster do
  begin
   Inc(Result);
  end;
end;

{==============================================================================}

function TFATFileSystem.GetClusterShiftCount(AClusterSize:LongWord):Word;
{Calculate the cluster shift count for cluster to bytes conversion}
begin
 {}
 Result:=0;

 if FDriver = nil then Exit;

 {Get the Shift Count}
 while (1 shl Result) < AClusterSize do
  begin
   Inc(Result);
  end;
end;

{==============================================================================}

function TFATFileSystem.GetEntriesPerBlock(ASize:Word;AType:TFATType):LongWord;
{From the table get the entries per block value for this sector size}
var
 Count:Integer;
begin
 {}
 Result:=0;

 if FDriver = nil then Exit;

 {Check the Table}
 for Count:=0 to fatMaxParams do
  begin
   if (fatParams[Count].SectorSize = ASize) and (fatParams[Count].FATType = AType) then
    begin
     Result:=fatParams[Count].EntriesPerBlock;
     Exit;
    end;
  end;
end;

{==============================================================================}

function TFATFileSystem.GetSectorsPerBlock(ASize:Word;AType:TFATType):LongWord;
{From the table get the sectors per block value for this sector size}
var
 Count:Integer;
begin
 {}
 Result:=0;

 if FDriver = nil then Exit;

 {Check the Table}
 for Count:=0 to fatMaxParams do
  begin
   if (fatParams[Count].SectorSize = ASize) and (fatParams[Count].FATType = AType) then
    begin
     Result:=fatParams[Count].SectorsPerBlock;
     Exit;
    end;
  end;
end;

{==============================================================================}

procedure TFATFileSystem.ReadConvert(ADirectory:PFATDirectory);
begin
 {}
 if FDriver = nil then Exit;
 if ADirectory = nil then Exit;

 {Check for Convert}
 if FDriver.OemConvert then
  begin
   {Convert Name}
   Unicode.OemToCharBuffA(PChar(@ADirectory.Name[0]),PChar(@ADirectory.Name[0]),8);

   {Convert Ext}
   Unicode.OemToCharBuffA(PChar(@ADirectory.Ext[0]),PChar(@ADirectory.Ext[0]),3);
  end;
end;

{==============================================================================}

procedure TFATFileSystem.WriteConvert(ADirectory:PFATDirectory);
begin
 {}
 if FDriver = nil then Exit;
 if ADirectory = nil then Exit;

 {Check for Convert}
 if FDriver.OemConvert then
  begin
   {Convert Name}
   Unicode.CharToOemBuff(PChar(@ADirectory.Name[0]),PChar(@ADirectory.Name[0]),8);

   {Convert Ext}
   Unicode.CharToOemBuff(PChar(@ADirectory.Ext[0]),PChar(@ADirectory.Ext[0]),3);
  end;
end;

{==============================================================================}

function TFATFileSystem.NameToEntry(AName:Pointer;AEntry:TFATDiskEntry;AShort:Boolean):Boolean;
{Loads an Entry from a Name}
{Note: Should only be called by DirectoryToEntry}
var
 Name:String;
 FATDirectory:PFATDirectory;
 LFNDirectory:PLFNDirectory;
begin
 {}
 Result:=False;

 if AName = nil then Exit;
 if AEntry = nil then Exit;
 if AEntry = FRoot then Exit;

 {Get Directory}
 FATDirectory:=PFATDirectory(AName);
 LFNDirectory:=PLFNDirectory(AName);

 {Check Name}
 if LFNDirectory.Order = fatEntrySpecial then LFNDirectory.Order:=fatEntryFree; {Substitute special character}

 {Validate Name}
 if not ValidateName(FATDirectory) then Exit;

 {Convert Name}
 ReadConvert(FATDirectory);

 {Check Type}
 if (AEntry.Attributes and faMatchMask) = faDirectory then
  begin
   {Folder}
   {Get Name}
   Name:=Trim(FATDirectory.Name);
   if Trim(FATDirectory.Ext) <> fatBlankName then Name:=Name + GetFileChar + Trim(FATDirectory.Ext);

   {Check Relative}
   if Name = fatDotName then AEntry.Attributes:=(AEntry.Attributes or faDot);
   if Name = fatDotDotName then AEntry.Attributes:=(AEntry.Attributes or faDotDot);
   if (AEntry.Attributes and (faDot or faDotDot)) <> faNone then
    begin
     {Dot or DotDot}
     {Check Short}
     if AShort then AEntry.AltName:=Name else AEntry.Name:=Name;
    end
   else
    begin
     {Folder}
     {Check Short}
     if AShort then
      begin
       {Set Name}
       AEntry.AltName:=Name;
      end
     else
      begin
       {Check Supported}
       if (FLongNames) and (FCaseFlags) and ((AEntry.Attributes and faFlagBoth) = faFlagBoth) then
        begin
         {Both}
         {Set Name (Short)}
         AEntry.AltName:=Name;

         {Get Name}
         Name:=Lowercase(Trim(FATDirectory.Name));
         if Trim(FATDirectory.Ext) <> fatBlankName then Name:=Name + GetFileChar + Lowercase(Trim(FATDirectory.Ext));

         {Set Name}
         AEntry.Name:=Name;
        end
       else if (FLongNames) and (FCaseFlags) and ((AEntry.Attributes and faFlagName) = faFlagName) then
        begin
         {Name}
         {Set Name (Short)}
         AEntry.AltName:=Name;

         {Get Name}
         Name:=Lowercase(Trim(FATDirectory.Name));
         if Trim(FATDirectory.Ext) <> fatBlankName then Name:=Name + GetFileChar + Trim(FATDirectory.Ext);

         {Set Name}
         AEntry.Name:=Name;
        end
       else if (FLongNames) and (FCaseFlags) and ((AEntry.Attributes and faFlagExt) = faFlagExt) then
        begin
         {Ext}
         {Load Name (Short)}
         AEntry.AltName:=Name;

         {Get Name}
         Name:=Trim(FATDirectory.Name);
         if Trim(FATDirectory.Ext) <> fatBlankName then Name:=Name + GetFileChar + Lowercase(Trim(FATDirectory.Ext));

         {Set Name}
         AEntry.Name:=Name;
        end
       else
        begin
         {Set Name}
         AEntry.Name:=Name;
        end;
      end;
    end;

   Result:=True;
  end
 else if (AEntry.Attributes and faMatchMask) = faFile then
  begin
   {File}
   {Get Name}
   Name:=Trim(FATDirectory.Name);
   if Trim(FATDirectory.Ext) <> fatBlankName then Name:=Name + GetFileChar + Trim(FATDirectory.Ext);

   {Check Short}
   if AShort then
    begin
     {Set Name}
     AEntry.AltName:=Name;
    end
   else
    begin
     {Check Supported}
     if (FLongNames) and (FCaseFlags) and ((AEntry.Attributes and faFlagBoth) = faFlagBoth) then
      begin
       {Both}
       {Set Name (Short)}
       AEntry.AltName:=Name;

       {Get Name}
       Name:=Lowercase(Trim(FATDirectory.Name));
       if Trim(FATDirectory.Ext) <> fatBlankName then Name:=Name + GetFileChar + Lowercase(Trim(FATDirectory.Ext));

       {Set Name}
       AEntry.Name:=Name;
      end
     else if (FLongNames) and (FCaseFlags) and ((AEntry.Attributes and faFlagName) = faFlagName) then
      begin
       {Name}
       {Set Name (Short)}
       AEntry.AltName:=Name;

       {Get Name}
       Name:=Lowercase(Trim(FATDirectory.Name));
       if Trim(FATDirectory.Ext) <> fatBlankName then Name:=Name + GetFileChar + Trim(FATDirectory.Ext);

       {Set Name}
       AEntry.Name:=Name;
      end
     else if (FLongNames) and (FCaseFlags) and ((AEntry.Attributes and faFlagExt) = faFlagExt) then
      begin
       {Ext}
       {Load Name (Short)}
       AEntry.AltName:=Name;

       {Get Name}
       Name:=Trim(FATDirectory.Name);
       if Trim(FATDirectory.Ext) <> fatBlankName then Name:=Name + GetFileChar + Lowercase(Trim(FATDirectory.Ext));

       {Set Name}
       AEntry.Name:=Name;
      end
     else
      begin
       {Set Name}
       AEntry.Name:=Name;
      end;
    end;

   Result:=True;
  end
 else if (AEntry.Attributes and faMatchMask) = faVolumeId then
  begin
   {Label}
   {Get Name}
   Name:=Trim(FATDirectory.Name) + Trim(FATDirectory.Ext);

   {Check Short}
   if AShort then AEntry.AltName:=Name else AEntry.Name:=Name;

   Result:=True;
  end;
end;

{==============================================================================}

function TFATFileSystem.EntryToName(AEntry:TFATDiskEntry;AName:Pointer;AShort:Boolean):Boolean;
{Loads a Name from an Entry}
{Note: Should only be called by EntryToDirectory and ChecksumName}
var
 Ext:String;
 Name:String;
 FATDirectory:PFATDirectory;
 LFNDirectory:PLFNDirectory;
begin
 {}
 Result:=False;

 if AName = nil then Exit;
 if AEntry = nil then Exit;
 if AEntry = FRoot then Exit;

 {Get Directory}
 FATDirectory:=PFATDirectory(AName);
 LFNDirectory:=PLFNDirectory(AName);

 {Blank Name}
 FillChar(FATDirectory.Name[0],8,fatEntryPadding);
 FillChar(FATDirectory.Ext[0],3,fatEntryPadding);

 {Check Short}
 if AShort then
  begin
   {Get Name}
   Name:=AEntry.AltName;
  end
 else
  begin
   {Check Supported}
   if (FLongNames) and (FCaseFlags) and ((AEntry.Attributes and faFlagBoth) <> faNone) then
    begin
     {Get Name (Short)}
     Name:=AEntry.AltName;
    end
   else
    begin
     {Get Name}
     Name:=AEntry.Name;
    end;
  end;
 if Length(Name) = 0 then Exit;

 {Check Type}
 if (AEntry.Attributes and faMatchMask) = faDirectory then
  begin
   {Folder}
   if (AEntry.Attributes and (faDot or faDotDot)) <> faNone then
    begin
     {Dot or DotDot}
     {Set Name}
     System.Move(Name[1],FATDirectory.Name[0],Min(Length(Name),8));
    end
   else
    begin
     {Folder}
     {Split Name}
     if not SplitFile(Name,Name,Ext) then Exit;

     {Set Name}
     System.Move(Name[1],FATDirectory.Name[0],Min(Length(Name),8));
     if Length(Ext) <> 0 then System.Move(Ext[1],FATDirectory.Ext[0],Min(Length(Ext),3));
    end;

   {Convert Name}
   WriteConvert(FATDirectory);

   {Validate Name}
   {if not ValidateName(FATDirectory) then Exit;} {To be added after testing}

   {Check Name}
   if LFNDirectory.Order = fatEntryFree then LFNDirectory.Order:=fatEntrySpecial; {Substitute special character}

   Result:=True;
  end
 else if (AEntry.Attributes and faMatchMask) = faFile then
  begin
   {File}
   {Split Name}
   if not SplitFile(Name,Name,Ext) then Exit;

   {Set Name}
   System.Move(Name[1],FATDirectory.Name[0],Min(Length(Name),8));
   if Length(Ext) <> 0 then System.Move(Ext[1],FATDirectory.Ext[0],Min(Length(Ext),3));

   {Convert Name}
   WriteConvert(FATDirectory);

   {Validate Name}
   {if not ValidateName(FATDirectory) then Exit;} {To be added after testing}

   {Check Name}
   if LFNDirectory.Order = fatEntryFree then LFNDirectory.Order:=fatEntrySpecial; {Substitute special character}

   Result:=True;
  end
 else if (AEntry.Attributes and faMatchMask) = faVolumeId then
  begin
   {Label}
   {Set Name}
   System.Move(Name[1],FATDirectory.Name[0],Min(Length(Name),11));

   {Convert Name}
   WriteConvert(FATDirectory);

   {Validate Name}
   {if not ValidateName(FATDirectory) then Exit;} {To be added after testing}

   {Check Name}
   if LFNDirectory.Order = fatEntryFree then LFNDirectory.Order:=fatEntrySpecial; {Substitute special character}

   Result:=True;
  end;
end;

{==============================================================================}

function TFATFileSystem.BufferToName(ABuffer:Pointer;var AName:String):Boolean;
{Converts a Long Name Buffer to a String}
{Note: Should only be called by LoadLong}
var
 Size:Integer;
begin
 {}
 Result:=False;

 if ABuffer = nil then Exit;

 {Get Length}
 Size:=Unicode.WideCharToMultiByte(CP_ACP,0,PWideChar(ABuffer),-1,nil,0,nil,nil);
 if Size > 0 then
  begin
   {Allocate String}
   SetString(AName,nil,Size - 1); {Returned size includes null terminator}

   {Convert String}
   Unicode.WideCharToMultiByte(CP_ACP,0,PWideChar(ABuffer),-1,PChar(AName),Size,nil,nil);

   Result:=True;
  end;
end;

{==============================================================================}

function TFATFileSystem.NameToBuffer(const AName:String;ABuffer:Pointer):Boolean;
{Converts a String to a Long Name Buffer}
{Note: Should only be called by SetLong}
var
 Size:Integer;
begin
 {}
 Result:=False;

 if ABuffer = nil then Exit;

 {Fill Buffer}
 FillChar(ABuffer^,lfnMaxName,255); {Fill with FF by default}

 {Convert String}
 Size:=Unicode.MultiByteToWideChar(CP_ACP,0,PChar(AName),-1,PWideChar(ABuffer),(lfnMaxName shr 1));
 if Size > 0 then
  begin
   {Set Length}
   PWord(PtrUInt(ABuffer) + LongWord((Size - 1) shl 1))^:=0; {Set null on end} {Returned size includes null terminator}

   Result:=True;
  end;
end;

{==============================================================================}

function TFATFileSystem.DirectoryToBuffer(ADirectory,ABuffer:Pointer;ACount,AChecksum:Byte;ALast:Boolean):Boolean;
{Loads a Long Name buffer from a Directory entry}
{Note: Should only be called by LoadLong}
{Note: Caller must check the lfnEntryLast flag}
var
 Next:WideChar;
 Count:Word;
 Offset:Word;
 Directory:PLFNDirectory;
begin
 {}
 Result:=False;

 if ACount < 1 then Exit;
 if ADirectory = nil then Exit;

 {Get Directory}
 Directory:=PLFNDirectory(ADirectory);

 {Check Order}
 if (Directory.Order and lfnEntryMask) <> ACount then Exit;

 {Check Checksum}
 if Directory.Checksum <> AChecksum then Exit;

 {Validate Directory}
 if not ValidateDirectory(Directory) then Exit;

 {Check Last - Fill Buffer}
 if ALast then FillChar(ABuffer^,((ACount * lfnEntryChars) + 1) shl 1,0); {Fill with 0 by default}

 {Get Offset}
 Offset:=((ACount - 1) * lfnEntryChars) shl 1;

 {Load Name}
 Next:=#0;
 Count:=0;
 while Count < lfnEntryChars do
  begin
   {Get Next}
   case Count of
    0..4:Next:=Directory.Name1[Count];
    5..10:Next:=Directory.Name2[Count - 5];
    11..12:Next:=Directory.Name3[Count - 11];
   end;

   {Copy Char}
   PWideChar(PtrUInt(ABuffer) + Offset)^:=Next;

   {Check Null}
   if Word(Next) = lfnEntryPadding then Break;
   if Word(Next) = lfnEntryNull then Break;

   {Move Next}
   Inc(Count);
   Inc(Offset,2);
  end;

 Result:=True;
end;

{==============================================================================}

function TFATFileSystem.BufferToDirectory(ABuffer,ADirectory:Pointer;ACount,AChecksum:Byte;ALast:Boolean):Boolean;
{Loads a Directory entry from a Long Name Buffer}
{Note: Should only be called by SetLong}
var
 Next:WideChar;
 Count:Word;
 Offset:Word;
 Directory:PLFNDirectory;
begin
 {}
 Result:=False;

 if ACount < 1 then Exit;
 if ADirectory = nil then Exit;

 {Get Directory}
 Directory:=PLFNDirectory(ADirectory);

 {Set Order}
 Directory.Order:=ACount;

 {Check Last - Set Last}
 if ALast then Directory.Order:=(Directory.Order or lfnEntryLast);

 {Set Attributes}
 Directory.Attribute:=faLongName;

 {Set FileType}
 {Directory.FileType:=0;} {Leave as found}

 {Set Checksum}
 Directory.Checksum:=AChecksum;

 {Set Reserved}
 Directory.Reserved:=0; {Always 0}

 {Get Offset}
 Offset:=((ACount - 1) * lfnEntryChars) shl 1;

 {Set Name}
 Next:=#0;
 Count:=0;
 while Count < lfnEntryChars do
  begin
   {Get Next}
   Next:=PWideChar(PtrUInt(ABuffer) + Offset)^;
   {Copy Char}
   case Count of
    0..4:Directory.Name1[Count]:=Next;
    5..10:Directory.Name2[Count - 5]:=Next;
    11..12:Directory.Name3[Count - 11]:=Next;
   end;

   {Move Next}
   Inc(Count);
   Inc(Offset,2);
  end;

 Result:=True;
end;

{==============================================================================}

function TFATFileSystem.DirectoryToEntry(ADirectory:Pointer;AEntry:TFATDiskEntry;AShort:Boolean):Boolean;
{Loads an Entry from a Directory entry}
{Note: Should only be called by LoadEntry and LoadLong}
var
 Size:Int64;
 FileTime:TFileTime;
 LocalTime:TFileTime;
 StartCluster:LongWord;
 FATDirectory:PFATDirectory;
 LFNDirectory:PLFNDirectory;
begin
 {}
 Result:=False;

 if AEntry = nil then Exit;
 if AEntry = FRoot then Exit;
 if ADirectory = nil then Exit;

 {Get Directory}
 FATDirectory:=PFATDirectory(ADirectory);
 LFNDirectory:=PLFNDirectory(ADirectory);

 {Validate Directory}
 if not ValidateDirectory(FATDirectory) then Exit;

 {Get Start Cluster}
 LongRec(StartCluster).Lo:=FATDirectory.FirstClusterLow;
 LongRec(StartCluster).Hi:=FATDirectory.FirstClusterHigh;
 AEntry.StartCluster:=StartCluster;

 {Get Size}
 Int64Rec(Size).Lo:=FATDirectory.Length;
 Int64Rec(Size).Hi:=0;
 AEntry.Size:=Size;

 {Get Write Time}
 Int64(FileTime):=0;
 Ultibo.DosDateTimeToFileTime(FATDirectory.WriteDate,FATDirectory.WriteTime,LocalTime);
 if Int64(LocalTime) > 0 then Ultibo.LocalFileTimeToFileTime(LocalTime,FileTime) else Int64(FileTime):=TIME_TICKS_TO_1980;
 if Int64(FileTime) < TIME_TICKS_TO_1980 then Int64(FileTime):=TIME_TICKS_TO_1980;
 AEntry.WriteTime:=FileTime;

 {Get Create Time}
 Int64(FileTime):=0;
 Ultibo.DosDateTimeToFileTime(FATDirectory.CreateDate,FATDirectory.CreateTime,LocalTime);
 if Int64(LocalTime) > 0 then Ultibo.LocalFileTimeToFileTime(LocalTime,FileTime) else Int64(FileTime):=TIME_TICKS_TO_1980;
 if Int64(FileTime) < TIME_TICKS_TO_1980 then Int64(FileTime):=TIME_TICKS_TO_1980;
 Int64(FileTime):=Int64(FileTime) + (TIME_TICKS_PER_10MILLISECONDS * FATDirectory.CreateTimeMsecs);
 AEntry.CreateTime:=FileTime;

 {Get Access Time}
 Int64(FileTime):=0;
 Ultibo.DosDateTimeToFileTime(FATDirectory.LastAccessDate,0,LocalTime);
 if Int64(LocalTime) > 0 then Ultibo.LocalFileTimeToFileTime(LocalTime,FileTime) else Int64(FileTime):=TIME_TICKS_TO_1980;
 if Int64(FileTime) < TIME_TICKS_TO_1980 then Int64(FileTime):=TIME_TICKS_TO_1980;
 AEntry.AccessTime:=FileTime;

 {Get Attributes}
 AEntry.Attributes:=FATDirectory.Attribute;
 if FReadOnly then AEntry.Attributes:=(AEntry.Attributes or faReadOnly);

 {Get CaseFlags}
 if (FATDirectory.CaseFlags and fatFlagName) = fatFlagName then AEntry.Attributes:=(AEntry.Attributes or faFlagName);
 if (FATDirectory.CaseFlags and fatFlagExt) = fatFlagExt then AEntry.Attributes:=(AEntry.Attributes or faFlagExt);

 {Check Type}
 if (AEntry.Attributes and (faDirectory or faVolumeID)) = faNone then
  begin
   {File}
   AEntry.Attributes:=(AEntry.Attributes or faFile);
   AEntry.EntriesLoaded:=True;

   {Get Name}
   Result:=NameToEntry(FATDirectory,AEntry,AShort);
  end
 else if (AEntry.Attributes and (faDirectory or faVolumeID)) = faDirectory then
  begin
   {Folder}
   AEntry.Attributes:=(AEntry.Attributes or faDirectory);

   {Get Name}
   Result:=NameToEntry(FATDirectory,AEntry,AShort);
  end
 else if (AEntry.Attributes and (faDirectory or faVolumeID)) = faVolumeID then
  begin
   {Label}
   AEntry.Attributes:=(AEntry.Attributes or faVolumeID);
   AEntry.EntriesLoaded:=True;

   {Get Name}
   Result:=NameToEntry(FATDirectory,AEntry,AShort);
  end;
end;

{==============================================================================}

function TFATFileSystem.EntryToDirectory(AEntry:TFATDiskEntry;ADirectory:Pointer;AShort:Boolean):Boolean;
{Loads a Directory entry from an Entry}
{Note: Should only be called by SetEntry and SetLong}
var
 FatTime:Word;
 LocalTime:TFileTime;
 FATDirectory:PFATDirectory;
 LFNDirectory:PLFNDirectory;
begin
 {}
 Result:=False;

 if AEntry = nil then Exit;
 if AEntry = FRoot then Exit;
 if ADirectory = nil then Exit;

 {Get Directory}
 FATDirectory:=PFATDirectory(ADirectory);
 LFNDirectory:=PLFNDirectory(ADirectory);

 {Set Start Cluster}
 FATDirectory.FirstClusterLow:=LongRec(AEntry.StartCluster).Lo;
 FATDirectory.FirstClusterHigh:=LongRec(AEntry.StartCluster).Hi;

 {Set Size}
 FATDirectory.Length:=Int64Rec(AEntry.Size).Lo;

 {Set Write Time}
 Int64(LocalTime):=0;
 if Int64(AEntry.WriteTime) > 0 then Ultibo.FileTimeToLocalFileTime(AEntry.WriteTime,LocalTime);
 if not Ultibo.FileTimeToDosDateTime(LocalTime,FATDirectory.WriteDate,FATDirectory.WriteTime) then FATDirectory.WriteDate:=(PASCAL_TIME_DOS_TIME_START shr 16);

 {Set Create Time}
 Int64(LocalTime):=0;
 if Int64(AEntry.CreateTime) > 0 then Ultibo.FileTimeToLocalFileTime(AEntry.CreateTime,LocalTime);
 if not Ultibo.FileTimeToDosDateTime(LocalTime,FATDirectory.CreateDate,FATDirectory.CreateTime) then FATDirectory.CreateDate:=(PASCAL_TIME_DOS_TIME_START shr 16);
 FATDirectory.CreateTimeMsecs:=(Int64(LocalTime) mod (2 * TIME_TICKS_PER_SECOND)) div TIME_TICKS_PER_10MILLISECONDS;

 {Set Access Time}
 Int64(LocalTime):=0;
 if Int64(AEntry.AccessTime) > 0 then Ultibo.FileTimeToLocalFileTime(AEntry.AccessTime,LocalTime);
 if not Ultibo.FileTimeToDosDateTime(LocalTime,FATDirectory.LastAccessDate,FatTime) then FATDirectory.LastAccessDate:=(PASCAL_TIME_DOS_TIME_START shr 16);

 {Set Attributes}
 FATDirectory.Attribute:=(AEntry.Attributes and faFindMask);

 {Set CaseFlags}
 FATDirectory.CaseFlags:=fatFlagNone;
 if (AEntry.Attributes and faFlagName) = faFlagName then FATDirectory.CaseFlags:=(FATDirectory.CaseFlags or fatFlagName);
 if (AEntry.Attributes and faFlagExt) = faFlagExt then FATDirectory.CaseFlags:=(FATDirectory.CaseFlags or fatFlagExt);

 {Set Name}
 Result:=EntryToName(AEntry,FATDirectory,AShort);
end;

{==============================================================================}

function TFATFileSystem.FATTypeToFileSysType(AFATType:TFATType):TFileSysType;
begin
 {}
 Result:=fsUNKNOWN;

 case AFATType of
  ftFAT12:Result:=fsFAT12;
  ftFAT16:Result:=fsFAT16;
  ftFAT32:Result:=fsFAT32;
 end;
end;

{==============================================================================}

function TFATFileSystem.LoadMaxFile:Integer;
begin
 {}
 Result:=MAX_FAT_FILE;

 if FLongNames then Result:=MAX_VFAT_FILE;
end;

{==============================================================================}

function TFATFileSystem.LoadMaxPath:Integer;
begin
 {}
 Result:=MAX_FAT_PATH;

 if FLongNames then Result:=MAX_VFAT_PATH;
end;

{==============================================================================}

function TFATFileSystem.LoadAttributes:LongWord;
begin
 {}
 Result:=inherited LoadAttributes;

 {if FLongNames then Result:=(Result or vaCasePreserved or vaUnicode);} {Now inbuilt}
end;

{==============================================================================}

function TFATFileSystem.LoadMaxAttributes:LongWord;
{Load the Maximum File Attributes (FileSetAttr)}
begin
 {}
 Result:=faAnyFile; {Excludes faDevice and faNormal}
end;

{==============================================================================}

function TFATFileSystem.LoadMinFileTime:TFileTime;
{Load the Minimum File Time value (WriteTime/CreateTime/AccessTime)}
begin
 {}
 Int64(Result):=TIME_TICKS_TO_1980;
end;

{==============================================================================}

function TFATFileSystem.LoadSystemName:String;
{Load System Name from Boot Sector}
begin
 {}
 Result:=fatBlankName;

 if FDriver = nil then Exit;

 if not SectorLock then Exit;
 try
  if FSectorBuffer = nil then Exit;

  {Check Type}
  case FFATType of
   ftFAT12,ftFAT16:begin
     {Get BootSector}
     if not ReadSectors(FBootSector,1,FSectorBuffer^) then Exit;
     if PBootSector(FSectorBuffer).Signature <> BOOT_SECTOR_SIGNATURE then
      begin
       if PBootSector(FSectorBuffer).BootSignature <> fatBootSignature then Exit;
      end;

     SetString(Result,PBootSector(FSectorBuffer).SystemName,8);

     Result:=Trim(Result);
    end;
   ftFAT32:begin
     {Get BootSector}
     if not ReadSectors(FBootSector,1,FSectorBuffer^) then Exit;
     if PExtBootSector(FSectorBuffer).Signature <> BOOT_SECTOR_SIGNATURE then
      begin
       if PExtBootSector(FSectorBuffer).BootSignature <> fatBootSignature then Exit;
      end;

     SetString(Result,PExtBootSector(FSectorBuffer).SystemName,8);

     Result:=Trim(Result);
    end;
  end;
 finally
  SectorUnlock;
 end;
end;

{==============================================================================}

function TFATFileSystem.LoadVolumeName:String;
{Load Volume Name from Boot Sector}
begin
 {}
 Result:=fatBlankName;

 if FDriver = nil then Exit;

 if not SectorLock then Exit;
 try
  if FSectorBuffer = nil then Exit;

  {Check Type}
  case FFATType of
   ftFAT12,ftFAT16:begin
     {Get BootSector}
     if not ReadSectors(FBootSector,1,FSectorBuffer^) then Exit;
     if PBootSector(FSectorBuffer).Signature <> BOOT_SECTOR_SIGNATURE then
      begin
       if PBootSector(FSectorBuffer).BootSignature <> fatBootSignature then Exit;
      end;

     SetString(Result,PBootSector(FSectorBuffer).VolumeName,11);

     Result:=Trim(Result);
    end;
   ftFAT32:begin
     {Get BootSector}
     if not ReadSectors(FBootSector,1,FSectorBuffer^) then Exit;
     if PExtBootSector(FSectorBuffer).Signature <> BOOT_SECTOR_SIGNATURE then
      begin
       if PExtBootSector(FSectorBuffer).BootSignature <> fatBootSignature then Exit;
      end;

     SetString(Result,PExtBootSector(FSectorBuffer).VolumeName,11);

     Result:=Trim(Result);
    end;
  end;
 finally
  SectorUnlock;
 end;
end;

{==============================================================================}

function TFATFileSystem.LoadVolumeSerial:LongWord;
{Load Volume Serial from Boot Sector}
begin
 {}
 Result:=0;

 if FDriver = nil then Exit;

 if not SectorLock then Exit;
 try
  if FSectorBuffer = nil then Exit;

  {Check Type}
  case FFATType of
   ftFAT12,ftFAT16:begin
     {Get BootSector}
     if not ReadSectors(FBootSector,1,FSectorBuffer^) then Exit;
     if PBootSector(FSectorBuffer).Signature <> BOOT_SECTOR_SIGNATURE then
      begin
       if PBootSector(FSectorBuffer).BootSignature <> fatBootSignature then Exit;
      end;

     Result:=PBootSector(FSectorBuffer).VolumeSerial;
    end;
   ftFAT32:begin
     {Get BootSector}
     if not ReadSectors(FBootSector,1,FSectorBuffer^) then Exit;
     if PExtBootSector(FSectorBuffer).Signature <> BOOT_SECTOR_SIGNATURE then
      begin
       if PExtBootSector(FSectorBuffer).BootSignature <> fatBootSignature then Exit;
      end;

     Result:=PExtBootSector(FSectorBuffer).VolumeSerial;
    end;
  end;
 finally
  SectorUnlock;
 end;
end;

{==============================================================================}

function TFATFileSystem.LoadFileSysType:TFileSysType;
begin
 {}
 Result:=FATTypeToFileSysType(FFATType);
end;

{==============================================================================}

function TFATFileSystem.SetVolumeName(const AName:String):Boolean;
{Set Volume Name in Boot Sector}
begin
 {}
 Result:=False;

 if FDriver = nil then Exit;

 {Check ReadOnly}
 if FReadOnly then Exit;

 if not SectorLock then Exit;
 try
  if FSectorBuffer = nil then Exit;

  {Check Type}
  case FFATType of
   ftFAT12,ftFAT16:begin
     {Get/Set BootSector}
     if not ReadSectors(FBootSector,1,FSectorBuffer^) then Exit;
     if PBootSector(FSectorBuffer).Signature <> BOOT_SECTOR_SIGNATURE then
      begin
       if PBootSector(FSectorBuffer).BootSignature <> fatBootSignature then Exit;
      end;

     FillChar(PBootSector(FSectorBuffer).VolumeName[0],11,fatEntryPadding);
     System.Move(AName[1],PBootSector(FSectorBuffer).VolumeName[0],Min(Length(AName),11));
     if not WriteSectors(FBootSector,1,FSectorBuffer^) then Exit;

     {Get/Set BootBackup}
     if FBootBackup <> FBootSector then
      begin
       if not ReadSectors(FBootBackup,1,FSectorBuffer^) then Exit;
       if PBootSector(FSectorBuffer).Signature <> BOOT_SECTOR_SIGNATURE then
        begin
         if PBootSector(FSectorBuffer).BootSignature <> fatBootSignature then Exit;
        end;

       FillChar(PBootSector(FSectorBuffer).VolumeName[0],11,fatEntryPadding);
       System.Move(AName[1],PBootSector(FSectorBuffer).VolumeName[0],Min(Length(AName),11));
       if not WriteSectors(FBootBackup,1,FSectorBuffer^) then Exit;
      end;

     FVolumeName:=AName;
     UniqueString(FVolumeName);

     Result:=True;
    end;
   ftFAT32:begin
     {Get/Set BootSector}
     if not ReadSectors(FBootSector,1,FSectorBuffer^) then Exit;
     if PExtBootSector(FSectorBuffer).Signature <> BOOT_SECTOR_SIGNATURE then
      begin
       if PExtBootSector(FSectorBuffer).BootSignature <> fatBootSignature then Exit;
      end;

     FillChar(PExtBootSector(FSectorBuffer).VolumeName[0],11,fatEntryPadding);
     System.Move(AName[1],PExtBootSector(FSectorBuffer).VolumeName[0],Min(Length(AName),11));
     if not WriteSectors(FBootSector,1,FSectorBuffer^) then Exit;

     {Get/Set BootBackup}
     if FBootBackup <> FBootSector then
      begin
       if not ReadSectors(FBootBackup,1,FSectorBuffer^) then Exit;
       if PExtBootSector(FSectorBuffer).Signature <> BOOT_SECTOR_SIGNATURE then
        begin
         if PExtBootSector(FSectorBuffer).BootSignature <> fatBootSignature then Exit;
        end;

       FillChar(PExtBootSector(FSectorBuffer).VolumeName[0],11,fatEntryPadding);
       System.Move(AName[1],PExtBootSector(FSectorBuffer).VolumeName[0],Min(Length(AName),11));
       if not WriteSectors(FBootBackup,1,FSectorBuffer^) then Exit;
      end;

     FVolumeName:=AName;
     UniqueString(FVolumeName);

     Result:=True;
    end;
  end;
 finally
  SectorUnlock;
 end;
end;

{==============================================================================}

function TFATFileSystem.SetVolumeSerial(ASerial:LongWord):Boolean;
{Set Volume Serial in Boot Sector}
begin
 {}
 Result:=False;

 if FDriver = nil then Exit;

 {Check ReadOnly}
 if FReadOnly then Exit;

 if not SectorLock then Exit;
 try
  if FSectorBuffer = nil then Exit;

  {Check Type}
  case FFATType of
   ftFAT12,ftFAT16:begin
     {Get/Set BootSector}
     if not ReadSectors(FBootSector,1,FSectorBuffer^) then Exit;
     if PBootSector(FSectorBuffer).Signature <> BOOT_SECTOR_SIGNATURE then
      begin
       if PBootSector(FSectorBuffer).BootSignature <> fatBootSignature then Exit;
      end;

     PBootSector(FSectorBuffer).VolumeSerial:=ASerial;
     if not WriteSectors(FBootSector,1,FSectorBuffer^) then Exit;

     {Get/Set BootBackup}
     if FBootBackup <> FBootSector then
      begin
       if not ReadSectors(FBootBackup,1,FSectorBuffer^) then Exit;
       if PBootSector(FSectorBuffer).Signature <> BOOT_SECTOR_SIGNATURE then
        begin
         if PBootSector(FSectorBuffer).BootSignature <> fatBootSignature then Exit;
        end;

       PBootSector(FSectorBuffer).VolumeSerial:=ASerial;
       if not WriteSectors(FBootBackup,1,FSectorBuffer^) then Exit;
      end;

     FVolumeSerial:=ASerial;

     Result:=True;
    end;
   ftFAT32:begin
     {Get/Set BootSector}
     if not ReadSectors(FBootSector,1,FSectorBuffer^) then Exit;
     if PExtBootSector(FSectorBuffer).Signature <> BOOT_SECTOR_SIGNATURE then
      begin
       if PExtBootSector(FSectorBuffer).BootSignature <> fatBootSignature then Exit;
      end;

     PExtBootSector(FSectorBuffer).VolumeSerial:=ASerial;
     if not WriteSectors(FBootSector,1,FSectorBuffer^) then Exit;

     {Get/Set BootBackup}
     if FBootBackup <> FBootSector then
      begin
       if not ReadSectors(FBootBackup,1,FSectorBuffer^) then Exit;
       if PExtBootSector(FSectorBuffer).Signature <> BOOT_SECTOR_SIGNATURE then
        begin
         if PExtBootSector(FSectorBuffer).BootSignature <> fatBootSignature then Exit;
        end;

       PExtBootSector(FSectorBuffer).VolumeSerial:=ASerial;
       if not WriteSectors(FBootBackup,1,FSectorBuffer^) then Exit;
      end;

     FVolumeSerial:=ASerial;

     Result:=True;
    end;
  end;
 finally
  SectorUnlock;
 end;
end;

{==============================================================================}

function TFATFileSystem.ReadEntry(AParent,AEntry:TDiskEntry;var ABuffer;const AStart:Int64;ACount:LongWord;var AOffset,AValue:LongWord):Integer;
{Note: The caller must ensure the entry is large enough or the read will fail}
var
 Start:LongWord;        {Starting offset for Write to Cluster}
 Count:LongWord;        {Count of bytes to Write to Cluster}
 Remain:LongWord;       {Remaining bytes to Read from Buffer}
 Offset:LongWord;       {Offset for Read from Buffer}

 OffsetCount:LongWord;
 CurrentCount:LongWord;

 StartOffset:Int64;
 StartCluster:LongWord;
begin
 {}
 Result:=0;

 if not FEntries.ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if AParent = nil then Exit;
  if AEntry = nil then Exit;
  if AEntry = FRoot then Exit; {Cannot read Root}

  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.ReadEntry EntryName = ' + AEntry.Name + ' Start = ' + IntToStr(AStart) + ' Count = ' + IntToStr(ACount));
  {$ENDIF}

  {Check Relative}
  if (AEntry.Attributes and (faDot or faDotDot)) <> faNone then Exit;

  {Check Entry}
  if (AEntry.Attributes and faMatchMask) <> faFile then Exit;

  {Get Start}
  StartCluster:=TFATDiskEntry(AEntry).StartCluster;

  {Get Count}
  OffsetCount:=(AStart shr FClusterShiftCount); {Zero equals first Cluster}
  CurrentCount:=0;

  {Check Saved}
  if (AOffset = OffsetCount) and (AValue >= StartCluster) then {AValue will be zero if last Read/Write reached end of chain}
   begin
    {Get Cluster}
    StartCluster:=AValue;
   end
  else
   begin
    {Get Cluster}
    while CurrentCount < OffsetCount do
     begin
      StartCluster:=GetNextChainCluster(StartCluster);
      if StartCluster = 0 then Exit;
      Inc(CurrentCount);
     end;
   end;

  {Get Offset}
  StartOffset:=(OffsetCount shl FClusterShiftCount); {Zero equals first Byte}

  {Get Position}
  Offset:=0;
  Remain:=ACount;
  Start:=(AStart - StartOffset);
  Count:=Min(ACount,(FClusterSize - Start));

  {Read Clusters}
  while Remain > 0 do
   begin
    {Check Count}
    if (Count < FClusterSize) then
     begin
      if not ReadLock then Exit;
      try
       if FReadBuffer = nil then Exit;

       {Read Cluster}
       if not ReadCluster(StartCluster,FReadBuffer^) then Exit;

       {Read Buffer}
       System.Move(Pointer(PtrUInt(FReadBuffer) + Start)^,Pointer(PtrUInt(@ABuffer) + Offset)^,Count);
      finally
       ReadUnlock;
      end;
     end
    else
     begin
      {Read Cluster}
      if not ReadCluster(StartCluster,Pointer(PtrUInt(@ABuffer) + Offset)^) then Exit;
     end;

    {Get Cluster}
    if ((Start + Count) = FClusterSize) then StartCluster:=GetNextChainCluster(StartCluster);

    {Update Position}
    Inc(Offset,Count);
    Dec(Remain,Count);
    Start:=0;
    Count:=Min(Remain,FClusterSize);
   end;

  Result:=(ACount - Remain);

  {Save Cluster}
  AValue:=StartCluster;

  {Save Offset}
  AOffset:=((AStart + Result) shr FClusterShiftCount);
 finally
  FEntries.ReaderUnlock;
 end;
end;

{==============================================================================}

function TFATFileSystem.WriteEntry(AParent,AEntry:TDiskEntry;const ABuffer;const AStart:Int64;ACount:LongWord;var AOffset,AValue:LongWord):Integer;
{Note: The caller must ensure the entry is large enough or the write will fail}
var
 Start:LongWord;        {Starting offset for Write to Cluster}
 Count:LongWord;        {Count of bytes to Write to Cluster}
 Remain:LongWord;       {Remaining bytes to Read from Buffer}
 Offset:LongWord;       {Offset for Read from Buffer}

 OffsetCount:LongWord;
 CurrentCount:LongWord;

 StartOffset:Int64;
 StartCluster:LongWord;
begin
 {}
 Result:=0;

 if not FEntries.ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if AParent = nil then Exit;
  if AEntry = nil then Exit;
  if AEntry = FRoot then Exit; {Cannot write Root}

  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.WriteEntry EntryName = ' + AEntry.Name + ' Start = ' + IntToStr(AStart) + ' Count = ' + IntToStr(ACount));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Relative}
  if (AEntry.Attributes and (faDot or faDotDot)) <> faNone then Exit;

  {Check Entry}
  if (AEntry.Attributes and faMatchMask) <> faFile then Exit;

  {Get Start}
  StartCluster:=TFATDiskEntry(AEntry).StartCluster;

  {Get Count}
  OffsetCount:=(AStart shr FClusterShiftCount); {Zero equals first Cluster}
  CurrentCount:=0;

  {Check Saved}
  if (AOffset = OffsetCount) and (AValue >= StartCluster) then {AValue will be zero if last Read/Write reached end of chain}
   begin
    {Get Cluster}
    StartCluster:=AValue;
   end
  else
   begin
    {Get Cluster}
    while CurrentCount < OffsetCount do
     begin
      StartCluster:=GetNextChainCluster(StartCluster);
      if StartCluster = 0 then Exit;
      Inc(CurrentCount);
     end;
   end;

  {Get Offset}
  StartOffset:=(OffsetCount shl FClusterShiftCount); {Zero equals first Byte}

  {Get Position}
  Offset:=0;
  Remain:=ACount;
  Start:=(AStart - StartOffset);
  Count:=Min(ACount,(FClusterSize - Start));

  {Write Clusters}
  while Remain > 0 do
   begin
    {Check Count}
    if (Count < FClusterSize) then
     begin
      if not WriteLock then Exit;
      try
       if FWriteBuffer = nil then Exit;

       {Read Cluster (if not cluster sized write)}
       if not ReadCluster(StartCluster,FWriteBuffer^) then Exit;

       {Write Buffer}
       System.Move(Pointer(PtrUInt(@ABuffer) + Offset)^,Pointer(PtrUInt(FWriteBuffer) + Start)^,Count);

       {Write Cluster}
       if not WriteCluster(StartCluster,FWriteBuffer^) then Exit;
      finally
       WriteUnlock;
      end;
     end
    else
     begin
      {Write Cluster}
      if not WriteCluster(StartCluster,Pointer(PtrUInt(@ABuffer) + Offset)^) then Exit;
     end;

    {Get Cluster}
    if ((Start + Count) = FClusterSize) then StartCluster:=GetNextChainCluster(StartCluster);

    {Update Position}
    Inc(Offset,Count);
    Dec(Remain,Count);
    Start:=0;
    Count:=Min(Remain,FClusterSize);
   end;

  Result:=(ACount - Remain);

  {Save Cluster}
  AValue:=StartCluster;

  {Save Offset}
  AOffset:=((AStart + Result) shr FClusterShiftCount);
 finally
  FEntries.ReaderUnlock;
 end;
end;

{==============================================================================}

function TFATFileSystem.LoadTables:Boolean;
var
 TableNo:LongWord;
begin
 {}
 Result:=False;

 if not FTables.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FNumberOfFats = 0 then Exit;

  {Get each Table}
  TableNo:=0;
  while TableNo < FNumberOfFats do
   begin
    {Check for FAT Mirroring}
    if (FFatMirroring) or (FActiveFat = TableNo) then
     begin
      {Get Table}
      if GetTableEx(TableNo,True) = nil then Exit;
     end;

    Inc(TableNo);
   end;

  Result:=True;
 finally
  FTables.WriterUnlock;
 end;
end;

{==============================================================================}

function TFATFileSystem.LoadBlocks:Boolean;
var
 BlockNo:LongWord;
begin
 {}
 Result:=False;

 if not FBlocks.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FTotalClusterCount = 0 then Exit;

  {Get each Block}
  BlockNo:=0;
  while BlockNo < FTotalClusterCount do
   begin
    if GetBlockEx(BlockNo,True) = nil then Exit;

    Inc(BlockNo,FEntriesPerBlock);
   end;

  Result:=True;
 finally
  FBlocks.WriterUnlock;
 end;
end;

{==============================================================================}

function TFATFileSystem.LoadEntries(AParent:TDiskEntry):Boolean;
var
 EntryOffset:LongWord;
 BlockOffset:LongWord;
 EntrySector:LongWord;
 BlockSector:LongWord;
 SectorCount:LongWord;
 SectorOffset:LongWord;
 Directory:PLFNDirectory;
begin
 {}
 Result:=False;

 if FDriver = nil then Exit;
 if AParent = nil then Exit;

 {Check Loaded}
 if not AParent.EntriesLoaded then
  begin
   if not FEntries.WriterLock then Exit;
   try
    {Check Loaded (After Lock)}
    if not AParent.EntriesLoaded then
     begin
      {$IFDEF FAT_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.LoadEntries Parent = ' + AParent.Name);
      {$ENDIF}

      {Check Directory}
      if (AParent.Attributes and faDirectory) = faDirectory then
       begin
        if not ClusterLock then Exit;
        try
         if FClusterBuffer = nil then Exit;

         {Check Start}
         if not CheckDirectoryStart(AParent) then Exit;

         {Get Offsets}
         SectorCount:=GetDirectorySectorCount(AParent,False);
         SectorOffset:=GetDirectorySectorOffset(AParent);
         if (SectorCount = 0) or (SectorOffset = 0) then Exit;

         {Get Start Sector}
         BlockOffset:=0;
         if not GetFirstDirectorySector(AParent,BlockSector) then Exit;

         repeat {First Sector will be zero for first data cluster}
          {Get Start Entry}
          EntryOffset:=0;
          EntrySector:=BlockSector;

          {$IFDEF FAT_DEBUG}
          if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.LoadEntries DirectorySector = ' + IntToStr(BlockSector));
          {$ENDIF}

          {Read Sectors}
          if not ReadSectors(SectorOffset + BlockSector,SectorCount,FClusterBuffer^) then Exit; {Note: SectorCount will never be more than SectorPerCluster}

          {Read Block}
          while BlockOffset < (FSectorSize * SectorCount) do
           begin
            {Read Entries}
            while EntryOffset < FSectorSize do
             begin
              {Get Directory}
              Directory:=PLFNDirectory(PtrUInt(FClusterBuffer) + BlockOffset + EntryOffset);

              {Check all Free}
              if Directory.Order = fatEntryFreeAll then Break; {Break to allow next Sector}

              {Check for Free}
              if Directory.Order <> fatEntryFree then
               begin
                {Load Entry}
                LoadEntry(AParent,FClusterBuffer,BlockOffset,EntryOffset,BlockSector,EntrySector);
                //To Do //Could set DirtyFlag (ForceCheckFlag ?) if LoadEntry returns false ?
                //Could then be used by Dismount to indicate that a check is needed
                //Also, LoadEntry needs to return something to indicate if it reached the end of the cluster chain ?
               end
              else
               begin
                {$IFDEF FAT_DEBUG}
                if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.LoadEntries FreeSector = ' + IntToStr(EntrySector) + ' Offset = ' + IntToStr(EntryOffset));
                {$ENDIF}
               end;

              {Get Next Entry}
              Inc(EntryOffset,fatEntrySize);

              {$IFDEF FAT_DEBUG}
              if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.LoadEntries EntryOffset = ' + IntToStr(EntryOffset));
              {$ENDIF}
             end;

            {Get Next Block}
            EntryOffset:=0;
            Inc(EntrySector);
            Inc(BlockOffset,FSectorSize);

            {$IFDEF FAT_DEBUG}
            if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.LoadEntries EntrySector = ' + IntToStr(EntrySector) + ' BlockOffset = ' + IntToStr(BlockOffset));
            {$ENDIF}
           end;

          {Get Next Sector}
          BlockOffset:=0;
         until not GetNextDirectorySector(AParent,BlockSector,False); {GetNextDirectorySector returns False on failure (Invalid/Last)}

         {$IFDEF FAT_DEBUG}
         if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.LoadEntries GetNextDirectorySector returns False');
         {$ENDIF}
        finally
         ClusterUnlock;
        end;
       end;

      AParent.EntriesLoaded:=True;

      {$IFDEF FAT_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.LoadEntries completed Parent = ' + AParent.Name);
      {$ENDIF}
     end;
   finally
    FEntries.WriterUnlock;
   end;
  end;

 Result:=True;
end;

{==============================================================================}

function TFATFileSystem.LoadTable(ATableNo:LongWord):Boolean;
var
 DiskTable:TFATDiskTable;
begin
 {}
 Result:=False;

 if not FTables.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ATableNo >= FNumberOfFats then Exit;

  {Load Table}
  DiskTable:=TFATDiskTable.Create(FTableLocal);
  DiskTable.TableNo:=ATableNo;
  DiskTable.StartSector:=(FReservedSectors + (FSectorsPerFat * ATableNo));
  DiskTable.SectorCount:=FSectorsPerFat;

  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.LoadTable - Table = ' + IntToHex(DiskTable.TableNo,8) + ' StartSector = ' + IntToStr(DiskTable.StartSector) + ' SectorCount = ' + IntToStr(DiskTable.SectorCount));
  {$ENDIF}

  Result:=FTables.Add(DiskTable);
 finally
  FTables.WriterUnlock;
 end;
end;

{==============================================================================}

function TFATFileSystem.LoadBlock(ABlockNo:LongWord):Boolean;
{Note: LoadBlock reads up to SectorsPerBlock sectors from the disk}
var
 BlockNo:LongWord;
 BlockBuffer:Pointer;
 BlockSector:LongWord;
 SectorCount:LongWord;
 DiskTable:TFATDiskTable;
 DiskBlock:TFATDiskBlock;
begin
 {}
 Result:=False;

 {$IFDEF FAT_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.LoadBlock BlockNo = ' + IntToStr(ABlockNo));
 {$ENDIF}

 if not FBlocks.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ABlockNo >= FTotalClusterCount then Exit;

  {Get Table}
  DiskTable:=TFATDiskTable(GetTable(FActiveFat));
  if DiskTable = nil then Exit;

  {Get Block No}
  if FEntriesPerBlock = 0 then Exit;
  {BlockNo:=((ABlockNo div FEntriesPerBlock) * FEntriesPerBlock);}
  BlockNo:=((ABlockNo shr FBlockShiftCount) shl FBlockShiftCount);

  {Get Sector and Count}
  {BlockSector:=((BlockNo div FEntriesPerBlock) * FSectorsPerBlock);}
  BlockSector:=((BlockNo shr FBlockShiftCount) * FSectorsPerBlock);
  SectorCount:=Min(FSectorsPerBlock,(FSectorsPerFat - BlockSector));

  {Allocate Buffer}
  BlockBuffer:=GetMem(SectorCount * FSectorSize);
  if BlockBuffer = nil then Exit;

  {Read Sectors}
  if ReadSectors(DiskTable.StartSector + BlockSector,SectorCount,BlockBuffer^) then
   begin
    {Load Block}
    DiskBlock:=TFATDiskBlock.Create(FBlockLocal);
    DiskBlock.BlockNo:=BlockNo;
    DiskBlock.BlockBuffer:=BlockBuffer;
    DiskBlock.BlockSector:=BlockSector;
    DiskBlock.SectorCount:=SectorCount;

    {$IFDEF FAT_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.LoadBlock - Block = ' + IntToHex(DiskBlock.BlockNo,8) + ' BlockSector = ' + IntToStr(DiskBlock.BlockSector) + ' SectorCount = ' + IntToStr(DiskBlock.SectorCount));
    {$ENDIF}

    Result:=FBlocks.Add(DiskBlock);
   end;
 finally
  FBlocks.WriterUnlock;
 end;
end;

{==============================================================================}

function TFATFileSystem.LoadEntry(AParent:TDiskEntry;ABuffer:Pointer;var ABlockOffset,AEntryOffset,ABlockSector,AEntrySector:LongWord):Boolean;
{Note: Offset and Sector point to the first directory entry of the entry on call}
{Note: Offset and Sector point to the last directory entry of the entry on return}
{Note: Should only be called by LoadEntries or LoadLong}
{Note: Calls LoadLong if the passed entry is a long name}

{Note: Caller must hold the entries writer lock}
var
 DiskEntry:TFATDiskEntry;
 Directory:PLFNDirectory;
begin
 {}
 Result:=False;

 if FDriver = nil then Exit;
 if AParent = nil then Exit;
 if ABuffer = nil then Exit;

 {Get Directory}
 Directory:=PLFNDirectory(PtrUInt(ABuffer) + ABlockOffset + AEntryOffset);

 {Check for Long}
 if Directory.Attribute = faLongName then
  begin
   {$IFDEF FAT_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.LoadEntry LongEntry = ' + IntToHex(Directory.Order,2));
   {$ENDIF}

   {Check for Supported}
   if not FLongNames then Exit; {Exit to return to LoadEntries for next entry}

   {Load Long}
   Result:=LoadLong(AParent,ABuffer,ABlockOffset,AEntryOffset,ABlockSector,AEntrySector);
  end
 else
  begin
   {$IFDEF FAT_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.LoadEntry EntrySector = ' + IntToStr(AEntrySector) + ' Offset = ' + IntToStr(AEntryOffset));
   {$ENDIF}

   {Check for Space}
   if Directory.Order = fatEntryPadding then Exit; {Exit to return to LoadEntries for next entry}

   {Create Entry}
   DiskEntry:=TFATDiskEntry.Create(FEntryLocal);
   DiskEntry.EntryCount:=1;
   DiskEntry.NameOffset:=AEntryOffset;
   DiskEntry.EntryOffset:=AEntryOffset;
   DiskEntry.NameSector:=AEntrySector;
   DiskEntry.EntrySector:=AEntrySector;

   {Load Entry} {No AltName}
   if DirectoryToEntry(Directory,DiskEntry,False) then
    begin
     {$IFDEF FAT_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.LoadEntry EntryName = ' + DiskEntry.Name);
     {$ENDIF}

     FEntries.Add(DiskEntry,AParent); {Caller holds the entries writer lock}

     Result:=True;
    end
   else
    begin
     DiskEntry.Free;
    end;
  end;
end;

{==============================================================================}

function TFATFileSystem.LoadLong(AParent:TDiskEntry;ABuffer:Pointer;var ABlockOffset,AEntryOffset,ABlockSector,AEntrySector:LongWord):Boolean;
{Note: Offset and Sector point to the first directory entry of the entry on call}
{Note: Offset and Sector point to the last directory entry of the entry on return}
{Note: Should only be called by LoadEntries via LoadEntry}
{Note: Calls LoadEntry if a short entry is found before long entry loaded}

{Note: Caller must hold the entries writer lock}
var
 Name:String;
 Checksum:Byte;
 NameCount:Byte;
 EntryCount:Byte;
 NameOffset:LongWord;
 NameSector:LongWord;
 SectorCount:LongWord;
 SectorOffset:LongWord;
 DiskEntry:TFATDiskEntry;
 Directory:PLFNDirectory;
begin
 {}
 Result:=False;

 if FDriver = nil then Exit;
 if AParent = nil then Exit;
 if ABuffer = nil then Exit;
 if FNameBuffer =  nil then Exit;

 {$IFDEF FAT_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.LoadLong LongSector = ' + IntToStr(AEntrySector) + ' Offset = ' + IntToStr(AEntryOffset));
 {$ENDIF}

 {Get Directory}
 Directory:=PLFNDirectory(PtrUInt(ABuffer) + ABlockOffset + AEntryOffset);

 {Check for Last}
 if (Directory.Order and lfnEntryLast) <> lfnEntryLast then Exit;

 {Get Name Count}
 NameCount:=(Directory.Order and lfnEntryMask);
 if (NameCount < 1) or (NameCount > lfnEntryMax) then Exit;

 {Get Name Offsets}
 NameOffset:=AEntryOffset;
 NameSector:=AEntrySector;

 {Get Offsets}
 SectorCount:=GetDirectorySectorCount(AParent,False);
 SectorOffset:=GetDirectorySectorOffset(AParent);
 if (SectorCount = 0) or (SectorOffset = 0) then Exit;

 {Get Checksum}
 Checksum:=Directory.Checksum;

 {Load Entry}
 EntryCount:=NameCount;
 if DirectoryToBuffer(Directory,FNameBuffer,EntryCount,Checksum,True) then
  begin
   {Get Next Entry}
   Dec(EntryCount);
   Inc(AEntryOffset,fatEntrySize);

   {Continue Entries}
   while True do {First Sector will be zero for first data cluster}
    begin
     {Read Block}
     while ABlockOffset < (FSectorSize * SectorCount) do
      begin
       {Read Entries}
       while AEntryOffset < FSectorSize do
        begin
         {Get Directory}
         Directory:=PLFNDirectory(PtrUInt(ABuffer) + ABlockOffset + AEntryOffset);

         {Check All Free}
         if Directory.Order = fatEntryFreeAll then Exit; {Exit to return to LoadEntries for next entry}

         {Check for Free}
         if Directory.Order = fatEntryFree then Exit; {Exit to return to LoadEntries for next entry}

         {Check Entry Count}
         if EntryCount > 0 then
          begin
           {Check for Long}
           if Directory.Attribute <> faLongName then
            begin
             {Load Entry} {Note: Try to load the short name entry for an orphaned long name entry}
             Result:=LoadEntry(AParent,ABuffer,ABlockOffset,AEntryOffset,ABlockSector,AEntrySector);
             Exit; {Exit to return to LoadEntries for next entry}
            end
           else
            begin
             {Check for Last}
             if (Directory.Order and lfnEntryLast) = lfnEntryLast then
              begin
               {Load Entry} {Note: Try to restart the long name search with a new long name entry}
               Result:=LoadEntry(AParent,ABuffer,ABlockOffset,AEntryOffset,ABlockSector,AEntrySector);
               Exit; {Exit to return to LoadEntries for next entry}
              end
             else
              begin
               {Check for Order} {Note: This is also done by DirectoryToBuffer}
               if (Directory.Order and lfnEntryMask) <> EntryCount then Exit; {Exit to return to LoadEntries for next entry}

               {$IFDEF FAT_DEBUG}
               if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.LoadLong LongEntry = ' + IntToHex(Directory.Order,2));
               {$ENDIF}

               {Load Entry}
               if not DirectoryToBuffer(Directory,FNameBuffer,EntryCount,Checksum,False) then Exit; {Exit to return to LoadEntries for next entry}
               Dec(EntryCount);
              end;
            end;
          end
         else
          begin
           {Check for Long}
           if Directory.Attribute = faLongName then
            begin
             {Load Entry} {Note: Try to restart the long name search with a new long name entry}
             Result:=LoadEntry(AParent,ABuffer,ABlockOffset,AEntryOffset,ABlockSector,AEntrySector);
             Exit; {Exit to return to LoadEntries for next entry}
            end
           else
            begin
             {$IFDEF FAT_DEBUG}
             if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.LoadLong EntrySector = ' + IntToStr(AEntrySector) + ' Offset = ' + IntToStr(AEntryOffset));
             {$ENDIF}

             {Convert Name}
             if not BufferToName(FNameBuffer,Name) then Exit; {Exit to return to LoadEntries for next entry}

             {Create Entry}
             DiskEntry:=TFATDiskEntry.Create(FEntryLocal);
             DiskEntry.EntryCount:=NameCount + 1;
             DiskEntry.NameOffset:=NameOffset;
             DiskEntry.EntryOffset:=AEntryOffset;
             DiskEntry.NameSector:=NameSector;
             DiskEntry.EntrySector:=AEntrySector;
             DiskEntry.Name:=Name;

             {Load Entry} {Use AltName}
             if DirectoryToEntry(Directory,DiskEntry,True) then
              begin
               {Checksum Name}
               if ChecksumName(DiskEntry) <> Checksum then
                begin
                 {Discard Long Name}
                 DiskEntry.EntryCount:=1;
                 DiskEntry.NameOffset:=AEntryOffset;
                 DiskEntry.NameSector:=AEntrySector;
                 DiskEntry.Name:=DiskEntry.AltName;
                 DiskEntry.AltName:='';
                end;

               {$IFDEF FAT_DEBUG}
               if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.LoadLong LongName = ' + DiskEntry.Name + ' / ShortName = ' + DiskEntry.AltName);
               {$ENDIF}

               FEntries.Add(DiskEntry,AParent); {Caller holds the entries writer lock}

               Result:=True;
              end
             else
              begin
               DiskEntry.Free;
              end;

             Exit; {Exit to return to LoadEntries for next entry}
            end;
          end;

         {Get Next Entry}
         Inc(AEntryOffset,fatEntrySize);
        end;

       {Get Next Block}
       AEntryOffset:=0;
       Inc(AEntrySector);
       Inc(ABlockOffset,FSectorSize);
      end;

     {Get Next Sector}   //To Do //This could cause problems with LoadEntries trying to reload already loaded entries ?
     ABlockOffset:=0;
     if not GetNextDirectorySector(AParent,ABlockSector,False) then Exit; {GetNextDirectorySector returns False on failure (Invalid/Last)} {Exit to return to LoadEntries for next entry}

     {Get First Entry}
     AEntryOffset:=0;
     AEntrySector:=ABlockSector;

     {$IFDEF FAT_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.LoadLong DirectorySector = ' + IntToStr(ABlockSector));
     {$ENDIF}

     {Read Sectors}     //To Do //This could cause problems with LoadEntries trying to reload already loaded entries ?
     if not ReadSectors(SectorOffset + ABlockSector,SectorCount,ABuffer^) then Exit; {Note: SectorCount will never be more than SectorPerCluster} {Exit to return to LoadEntries for next entry}
    end;
  end;
end;

{==============================================================================}

function TFATFileSystem.AddEntry(AParent:TDiskEntry;const AName:String;AAttributes:LongWord;AReference:Boolean):TDiskEntry;
{Reference: Add a reference on the returned entry if True}
begin
 {}
 Result:=AddEntryEx(AParent,AName,fatBlankName,AAttributes,AReference);
end;

{==============================================================================}

function TFATFileSystem.AddEntryEx(AParent:TDiskEntry;const AName,AAltName:String;AAttributes:LongWord;AReference:Boolean):TDiskEntry;
{Reference: Add a reference on the returned entry if True}
{If AltName already exists then a generated one will be provided, will not fail due to AltName}
var
 AltName:String;
 Flags:LongWord;
 EntryCount:Byte;
 EntryOffset:LongWord;
 EntrySector:LongWord;
 StartCluster:LongWord;
 FileTime:TFileTime;
 DiskEntry:TFATDiskEntry;
begin
 {}
 Result:=nil;

 if not FEntries.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if AParent = nil then Exit;

  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.AddEntryEx Parent = ' + AParent.Name + ' Name = ' + AName + ' AltName = ' + AAltName);
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Relative}
  if (AParent.Attributes and (faDot or faDotDot)) <> faNone then Exit;

  {Check Parent}
  if (AParent.Attributes and faMatchMask) <> faDirectory then Exit;

  {Check Attribtues (Exclude Label/Dot/DotDot)}
  if (AAttributes and (faVolumeID or faDot or faDotDot)) = faNone then
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

  {Check for Short (or Label/Dot/DotDot)}
  if (IsEightDotThree(AName)) or ((AAttributes and (faVolumeID or faDot or faDotDot)) <> faNone) then
   begin
    {Get Count}
    EntryCount:=1;

    {Get AltName}
    AltName:=fatBlankName;

    {Get Flags}
    Flags:=faNone;
   end
  else
   begin
    if CheckFlagName(AName) then
     begin
      {Get Count}
      EntryCount:=1;

      {Get AltName}
      AltName:=GenerateName(AParent,nil,AName);
      if Length(AltName) = 0 then Exit;

      {Get Flags}
      Flags:=GetNameFlags(AName);
     end
    else
     begin
      {Check Supported}
      if not FLongNames then Exit;

      {Get Count}
      EntryCount:=CountName(AName);
      if EntryCount = 0 then Exit;
      Inc(EntryCount);

      {Check AltName}
      if Length(AAltName) <> 0 then
       begin
        {Get AltName}
        AltName:=Uppercase(AAltName);
        if not(IsEightDotThree(AltName)) or (GetEntryEx(AParent,AltName,faDirectory or faFile,False,False,True) <> nil) then AltName:=GenerateName(AParent,nil,AName);
        if Length(AltName) = 0 then Exit;
       end
      else
       begin
        {Get AltName}
        AltName:=GenerateName(AParent,nil,AName);
        if Length(AltName) = 0 then Exit;
       end;

      {Get Flags}
      Flags:=faNone;
     end;
   end;

  {Allocate Directory}
  if not AllocDirectory(AParent,EntryCount,EntryOffset,EntrySector) then Exit;

  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.AddEntryEx EntrySector = ' + IntToStr(EntrySector) + ' Offset = ' + IntToStr(EntryOffset));
  {$ENDIF}

  {Create Entry}
  DiskEntry:=TFATDiskEntry.Create(FEntryLocal);
  DiskEntry.EntryCount:=EntryCount;
  DiskEntry.NameOffset:=EntryOffset;   {SetLong will update if needed}
  DiskEntry.EntryOffset:=EntryOffset;
  DiskEntry.NameSector:=EntrySector;   {SetLong will update if needed}
  DiskEntry.EntrySector:=EntrySector;
  DiskEntry.StartCluster:=FFreeCluster;
  DiskEntry.Name:=AName;
  DiskEntry.AltName:=AltName;
  DiskEntry.Size:=0;
  DiskEntry.Attributes:=(AAttributes or Flags);
  FileTime:=Ultibo.DateTimeToFileTime(Now);
  if Int64(FileTime) < TIME_TICKS_TO_1980 then Int64(FileTime):=TIME_TICKS_TO_1980;
  DiskEntry.WriteTime:=FileTime;
  DiskEntry.CreateTime:=DiskEntry.WriteTime;
  DiskEntry.AccessTime:=DiskEntry.WriteTime;
  DiskEntry.EntriesLoaded:=True;

  {Check for Folder}
  if (AAttributes and faMatchMask) = faDirectory then
   begin
    {Folder}
    if (AAttributes and faDotDot) = faDotDot then
     begin
      {Dot Dot}
      {Get Cluster}
      StartCluster:=GetParentCluster(TDiskEntry(AParent.Parent));
      DiskEntry.StartCluster:=StartCluster;

      {$IFDEF FAT_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.AddEntryEx StartCluster = ' + IntToHex(StartCluster,8));
      {$ENDIF}

      DiskEntry.Name:=fatDotDotName;
      DiskEntry.Attributes:=((AParent.Attributes or faDotDot) and not(faFlagBoth));
      DiskEntry.WriteTime:=AParent.WriteTime;
      DiskEntry.CreateTime:=AParent.CreateTime;
      DiskEntry.AccessTime:=AParent.AccessTime;
      DiskEntry.EntriesLoaded:=True;

      {Set Entry}
      if not SetEntry(AParent,DiskEntry) then Exit;

      if not FEntries.Add(DiskEntry,AParent) then Exit;
     end
    else if (AAttributes and faDot) = faDot then
     begin
      {Dot}
      {Get Cluster}
      StartCluster:=GetParentCluster(AParent);
      DiskEntry.StartCluster:=StartCluster;

      {$IFDEF FAT_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.AddEntryEx StartCluster = ' + IntToHex(StartCluster,8));
      {$ENDIF}

      DiskEntry.Name:=fatDotName;
      DiskEntry.Attributes:=((AParent.Attributes or faDot) and not(faFlagBoth));
      DiskEntry.WriteTime:=AParent.WriteTime;
      DiskEntry.CreateTime:=AParent.CreateTime;
      DiskEntry.AccessTime:=AParent.AccessTime;
      DiskEntry.EntriesLoaded:=True;

      {Set Entry}
      if not SetEntry(AParent,DiskEntry) then Exit;

      if not FEntries.Add(DiskEntry,AParent) then Exit;
     end
    else
     begin
      {Folder}
      {Allocate Cluster}
      if not AllocCluster(FFreeCluster,StartCluster,1) then Exit;
      DiskEntry.StartCluster:=StartCluster;

      {$IFDEF FAT_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.AddEntryEx StartCluster = ' + IntToHex(StartCluster,8));
      {$ENDIF}

      {Zero Cluster}
      if not FillCluster(StartCluster,0) then Exit;

      {Set Entry}
      if not SetLong(AParent,DiskEntry) then Exit; {SetLong will call SetEntry}
      if not FEntries.Add(DiskEntry,AParent) then Exit;

      {Create Dot}
      if AddEntry(DiskEntry,fatDotName,(AAttributes or faDot),False) = nil then Exit;

      {Create DotDot}
      if AddEntry(DiskEntry,fatDotDotName,(AAttributes or faDotDot),False) = nil then Exit;
     end;

    Result:=DiskEntry;

    {Add Reference}
    if AReference then Result.AddReference;
   end
  else if (AAttributes and faMatchMask) = faFile then
   begin
    {File}
    {Set Entry}
    if not SetLong(AParent,DiskEntry) then Exit; {SetLong will call SetEntry}

    if not FEntries.Add(DiskEntry,AParent) then Exit;

    Result:=DiskEntry;

    {Add Reference}
    if AReference then Result.AddReference;
   end
  else if (AAttributes and faMatchMask) = faVolumeID then
   begin
    {Label}
    {Set Entry}
    if not SetEntry(AParent,DiskEntry) then Exit;

    if not FEntries.Add(DiskEntry,AParent) then Exit;

    Result:=DiskEntry;

    {Add Reference}
    if AReference then Result.AddReference;
   end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{==============================================================================}

function TFATFileSystem.RemoveEntry(AParent,AEntry:TDiskEntry):Boolean;
//To Do //What happens to Dot and DotDot for a Directory ?
        //They do not need to be deleted but they need to be freed ?
var
 EntryCount:Byte;
 EntryOffset:LongWord;
 EntrySector:LongWord;
 StartCluster:LongWord;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if AParent = nil then Exit;
  if AEntry = nil then Exit;
  if AEntry = FRoot then Exit; {Cannot remove Root}

  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.RemoveEntry EntryName = ' + AEntry.Name);
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Parent}
  if AEntry.Parent <> AParent then Exit;

  {Check Relative}
  if (AEntry.Attributes and (faDot or faDotDot)) <> faNone then Exit;

  {Get Offsets} {Always use the NameOffset/NameSector}
  EntryCount:=TFATDiskEntry(AEntry).EntryCount;
  EntryOffset:=TFATDiskEntry(AEntry).NameOffset;
  EntrySector:=TFATDiskEntry(AEntry).NameSector;
  StartCluster:=TFATDiskEntry(AEntry).StartCluster;

  {Release Cluster}
  if not ReleaseCluster(FFreeCluster,StartCluster) then Exit;

  {Release Directory}
  if not ReleaseDirectory(AParent,EntryCount,EntryOffset,EntrySector) then Exit;

  {Remove Entry}
  if not FEntries.Remove(AEntry) then Exit;

  {Schedule Entry}
  if not FDriver.ScheduleEntry(AEntry,FILESYS_ENTRY_DELETE_TIMEOUT) then Exit;

  Result:=True;
 finally
  FEntries.WriterUnlock;
 end;
end;

{==============================================================================}

function TFATFileSystem.RenameEntry(AParent,AEntry:TDiskEntry;const AName:String):Boolean;
var
 AltName:String;
 Flags:LongWord;
 DestCount:Byte;
 SourceCount:Byte;
 DestOffset:LongWord;
 DestSector:LongWord;
 SourceOffset:LongWord;
 SourceSector:LongWord;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if AParent = nil then Exit;
  if AEntry = nil then Exit;
  if AEntry = FRoot then Exit; {Cannot rename Root}

  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.RenameEntry EntryName = ' + AEntry.Name + ' Name = ' + AName);
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Relative}
  if (AEntry.Attributes and (faDot or faDotDot)) <> faNone then Exit;

  {Check Parent}
  if (AParent.Attributes and faMatchMask) <> faDirectory then Exit;

  {Check Parent}
  if AEntry.Parent <> AParent then Exit;

  {Check Attribtues (Exclude Label)}
  if (AEntry.Attributes and faVolumeID) = faNone then
   begin
    {Check Existing}
    if GetEntryEx(AParent,AName,faDirectory or faFile,False,False,True) <> nil then Exit;
   end;

  {Get Offsets} {Always use the NameOffset/NameSector}
  SourceCount:=TFATDiskEntry(AEntry).EntryCount;
  SourceOffset:=TFATDiskEntry(AEntry).NameOffset;
  SourceSector:=TFATDiskEntry(AEntry).NameSector;

  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.RenameEntry SourceSector = ' + IntToStr(SourceSector) + ' Offset = ' + IntToStr(SourceOffset));
  {$ENDIF}

  {Check for Short (or Label)}
  if (IsEightDotThree(AName)) or ((AEntry.Attributes and faVolumeID) <> faNone) then
   begin
    {Get Count}
    DestCount:=1;

    {Get AltName}
    AltName:=fatBlankName;

    {Get Flags}
    Flags:=faNone;
   end
  else
   begin
    if CheckFlagName(AName) then
     begin
      {Get Count}
      DestCount:=1;

      {Get AltName}
      AltName:=GenerateName(AParent,AEntry,AName);
      if Length(AltName) = 0 then Exit;

      {Get Flags}
      Flags:=GetNameFlags(AName);
     end
    else
     begin
      {Check Supported}
      if not FLongNames then Exit;

      {Get Count}
      DestCount:=CountName(AName);
      if DestCount = 0 then Exit;
      Inc(DestCount);

      {Get AltName}
      AltName:=GenerateName(AParent,AEntry,AName);
      if Length(AltName) = 0 then Exit;

      {Get Flags}
      Flags:=faNone;
     end;
   end;

  {Check Counts}
  if DestCount <> SourceCount then
   begin
    {Allocate Directory}
    if not AllocDirectory(AParent,DestCount,DestOffset,DestSector) then Exit;

    {$IFDEF FAT_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.RenameEntry DestSector = ' + IntToStr(DestSector) + ' Offset = ' + IntToStr(DestOffset));
    {$ENDIF}

    {Rename Entry}
    AEntry.Name:=AName;
    AEntry.AltName:=AltName;
    AEntry.Attributes:=(AEntry.Attributes or Flags);
    if Flags = faNone then AEntry.Attributes:=(AEntry.Attributes and not(faFlagBoth));

    {Update Entry}
    TFATDiskEntry(AEntry).EntryCount:=DestCount;
    TFATDiskEntry(AEntry).NameOffset:=DestOffset;   {SetLong will update if needed}
    TFATDiskEntry(AEntry).EntryOffset:=DestOffset;
    TFATDiskEntry(AEntry).NameSector:=DestSector;   {SetLong will update if needed}
    TFATDiskEntry(AEntry).EntrySector:=DestSector;

    {Set Entry}
    if not SetLong(AParent,AEntry) then Exit; {SetLong will call SetEntry}

    {Release Directory}
    if not ReleaseDirectory(AParent,SourceCount,SourceOffset,SourceSector) then Exit;

    Result:=True;
   end
  else
   begin
    {Rename Entry}
    AEntry.Name:=AName;
    AEntry.AltName:=AltName;
    AEntry.Attributes:=(AEntry.Attributes or Flags);
    if Flags = faNone then AEntry.Attributes:=(AEntry.Attributes and not(faFlagBoth));

    {Set Entry}
    if not SetLong(AParent,AEntry) then Exit; {SetLong will call SetEntry}

    Result:=True;
   end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{==============================================================================}

function TFATFileSystem.RenameEntryEx(AParent,AEntry:TDiskEntry;const AAltName:String):Boolean;
var
 AltName:String;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if AParent = nil then Exit;
  if AEntry = nil then Exit;
  if AEntry = FRoot then Exit; {Cannot rename root}

  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.RenameEntryEx - Entry = ' + AEntry.Name + ' AltName = ' + AAltName);
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Relative}
  if (AEntry.Attributes and (faDot or faDotDot)) <> faNone then Exit;

  {Check Parent}
  if (AParent.Attributes and faMatchMask) <> faDirectory then Exit;

  {Check Parent}
  if AEntry.Parent <> AParent then Exit;

  {Check for Short (or Label)}
  if (IsEightDotThree(AEntry.Name)) or ((AEntry.Attributes and faVolumeID) <> faNone) then
   begin
    Exit;
   end
  else
   begin
    if CheckFlagName(AEntry.Name) then
     begin
      Exit;
     end
    else
     begin
      {Check AltName}
      if Length(AAltName) <> 0 then
       begin
        {Get AltName}
        AltName:=Uppercase(AAltName);
        if not(IsEightDotThree(AltName)) or (GetEntryEx(AParent,AltName,faDirectory or faFile,False,False,True) <> nil) then Exit;
       end
      else
       begin
        {Get AltName}
        AltName:=GenerateName(AParent,AEntry,AEntry.Name);
        if Length(AltName) = 0 then Exit;
       end;
     end;
   end;

  {Rename Entry}
  AEntry.AltName:=AltName;

  {Set Entry}
  if not SetLong(AParent,AEntry) then Exit; {SetLong will call SetEntry}

  Result:=True;
 finally
  FEntries.WriterUnlock;
 end;
end;

{==============================================================================}

function TFATFileSystem.MoveEntry(ASource,ADest,AEntry:TDiskEntry):Boolean;
var
 EntryCount:Byte;
 DestOffset:LongWord;
 DestSector:LongWord;
 SourceOffset:LongWord;
 SourceSector:LongWord;
 DiskEntry:TFATDiskEntry;
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

  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.MoveEntry EntryName = ' + AEntry.Name + ' Dest = ' + ADest.Name);
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Relative}
  if (AEntry.Attributes and (faDot or faDotDot)) <> faNone then Exit;

  {Check Source}
  if (ASource.Attributes and faMatchMask) <> faDirectory then Exit;

  {Check Dest}
  if (ADest.Attributes and faMatchMask) <> faDirectory then Exit;

  {Check Parent}
  if AEntry.Parent <> ASource then Exit;

  {Check Attribtues (Exclude Label)}
  if (AEntry.Attributes and faVolumeID) = faNone then
   begin
    {Check Existing}
    if GetEntryEx(ADest,AEntry.Name,faDirectory or faFile,False,False,True) <> nil then Exit;
   end;

  {Get Offsets} {Always use the NameOffset/NameSector}
  EntryCount:=TFATDiskEntry(AEntry).EntryCount;
  SourceOffset:=TFATDiskEntry(AEntry).NameOffset;
  SourceSector:=TFATDiskEntry(AEntry).NameSector;

  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.MoveEntry SourceSector = ' + IntToStr(SourceSector) + ' Offset = ' + IntToStr(SourceOffset));
  {$ENDIF}

  {Allocate Directory}
  if not AllocDirectory(ADest,EntryCount,DestOffset,DestSector) then Exit;

  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.MoveEntry DestSector = ' + IntToStr(DestSector) + ' Offset = ' + IntToStr(DestOffset));
  {$ENDIF}

  {Update Entry}
  TFATDiskEntry(AEntry).NameOffset:=DestOffset;   {SetLong will update if needed}
  TFATDiskEntry(AEntry).EntryOffset:=DestOffset;
  TFATDiskEntry(AEntry).NameSector:=DestSector;   {SetLong will update if needed}
  TFATDiskEntry(AEntry).EntrySector:=DestSector;

  {Set Entry}
  if not SetLong(ADest,AEntry) then Exit; {SetLong will call SetEntry}

  {Release Directory}
  if not ReleaseDirectory(ASource,EntryCount,SourceOffset,SourceSector) then Exit;

  {Check for Folder}
  if (AEntry.Attributes and faMatchMask) = faDirectory then
   begin
    {Folder}
    {Get DotDot}
    DiskEntry:=TFATDiskEntry(AEntry.FirstChild);
    while DiskEntry <> nil do
     begin
      if (DiskEntry.Attributes and faDotDot) = faDotDot then
       begin
        {Update DotDot}
        DiskEntry.StartCluster:=GetParentCluster(ADest);

        if not SetEntry(AEntry,DiskEntry) then Exit;
        Break;
       end;

      DiskEntry:=TFATDiskEntry(DiskEntry.Next);
     end;

    {Move Entry}
    FEntries.Move(AEntry,ADest);

    Result:=True;
   end
  else
   begin
    {File/Label}
    {Move Entry}
    FEntries.Move(AEntry,ADest);

    Result:=True;
   end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{==============================================================================}

function TFATFileSystem.SetBlock(ABlock:TDiskBlock):Boolean;
{Note: SetBlock writes up to SectorsPerBlock sectors to the disk}
var
 BlockBuffer:Pointer;
 BlockSector:LongWord;
 SectorCount:LongWord;
 NextTable:TFATDiskTable;
begin
 {}
 Result:=False;

 if not FBlocks.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ABlock = nil then Exit;

  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.SetBlock - Block = ' + IntToHex(ABlock.BlockNo,8));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get Offsets}
  BlockBuffer:=TFATDiskBlock(ABlock).BlockBuffer;
  BlockSector:=TFATDiskBlock(ABlock).BlockSector;
  SectorCount:=TFATDiskBlock(ABlock).SectorCount;

  if not FTables.WriterLock then Exit;
  try
   {Get each Table}
   NextTable:=TFATDiskTable(FTables.First);
   while NextTable <> nil do
    begin
     {Write Sectors}
     if not WriteSectors(NextTable.StartSector + BlockSector,SectorCount,BlockBuffer^) then Exit;

     NextTable:=TFATDiskTable(NextTable.Next);
    end;

   Result:=True;
  finally
   FTables.WriterUnlock;
  end;
 finally
  FBlocks.WriterUnlock;
 end;
end;

{==============================================================================}

function TFATFileSystem.SetEntry(AParent,AEntry:TDiskEntry):Boolean;
{Note: Should only be called by SetTime/SetAttr/SetSize/Rename/Move/Add}
{Note: Calls SetLong if EntryCount > 1}
var
 EntryOffset:LongWord;
 EntrySector:LongWord;
 SectorCount:LongWord;
 SectorOffset:LongWord;
 Directory:PLFNDirectory;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if AParent = nil then Exit;
  if AEntry = nil then Exit;
  if AEntry = FRoot then Exit; {Cannot set Root}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check for Long}
  if TFATDiskEntry(AEntry).EntryCount > 1 then
   begin
    Result:=SetLong(AParent,AEntry);
   end
  else
   begin
    if not ClusterLock then Exit;
    try
     if FClusterBuffer = nil then Exit;

     {$IFDEF FAT_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.SetEntry EntryName = ' + AEntry.Name);
     {$ENDIF}

     {Check Start}
     if not CheckDirectoryStart(AParent) then Exit;

     {Get Offsets}
     SectorCount:=GetDirectorySectorCount(AParent,True); {SectorCount is always one on Write}
     SectorOffset:=GetDirectorySectorOffset(AParent);
     if (SectorCount = 0) or (SectorOffset = 0) then Exit;

     {Get Sector}
     EntryOffset:=TFATDiskEntry(AEntry).EntryOffset;
     EntrySector:=TFATDiskEntry(AEntry).EntrySector;

     {$IFDEF FAT_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.SetEntry EntrySector = ' + IntToStr(EntrySector) + ' Offset = ' + IntToStr(EntryOffset));
     {$ENDIF}

     {Read Sectors}
     if not ReadSectors(SectorOffset + EntrySector,SectorCount,FClusterBuffer^) then Exit; {Note: SectorCount will never be more than SectorPerCluster}

     {Get Directory}
     Directory:=PLFNDirectory(PtrUInt(FClusterBuffer) + EntryOffset);

     {Set Entry} {No AltName}
     if not EntryToDirectory(TFATDiskEntry(AEntry),Directory,False) then Exit;

     {Write Sectors}
     if not WriteSectors(SectorOffset + EntrySector,SectorCount,FClusterBuffer^) then Exit;

     Result:=True;
    finally
     ClusterUnlock;
    end;
   end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{==============================================================================}

function TFATFileSystem.SetLong(AParent,AEntry:TDiskEntry):Boolean;
{Note: Should only be called by Rename/Move/Add}
{Note: SetLong updates the EntryOffset/Sector but EntryCount must be correct}
{Note: Calls SetEntry if EntryCount = 1}
var
 Checksum:Byte;
 NameCount:Byte;
 EntryCount:Byte;
 EntryOffset:LongWord;
 EntrySector:LongWord;
 SectorCount:LongWord;
 SectorOffset:LongWord;
 Directory:PLFNDirectory;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if AParent = nil then Exit;
  if AEntry = nil then Exit;
  if AEntry = FRoot then Exit; {Cannot set Root}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check for Short}
  if TFATDiskEntry(AEntry).EntryCount = 1 then
   begin
    Result:=SetEntry(AParent,AEntry);
   end
  else
   begin
    if not ClusterLock then Exit;
    try
     if FClusterBuffer = nil then Exit;

     {$IFDEF FAT_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.SetLong LongName = ' + AEntry.Name + ' / ShortName = ' + AEntry.AltName);
     {$ENDIF}

     {Check Start}
     if not CheckDirectoryStart(AParent) then Exit;

     {Get Offsets}
     SectorCount:=GetDirectorySectorCount(AParent,True); {SectorCount is always one on Write}
     SectorOffset:=GetDirectorySectorOffset(AParent);
     if (SectorCount = 0) or (SectorOffset = 0) then Exit;

     {Get Start Sector} {Always use the NameOffset/NameSector}
     EntryCount:=TFATDiskEntry(AEntry).EntryCount;
     EntryOffset:=TFATDiskEntry(AEntry).NameOffset;
     EntrySector:=TFATDiskEntry(AEntry).NameSector;

     {$IFDEF FAT_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.SetLong LongSector = ' + IntToStr(EntrySector) + ' Offset = ' + IntToStr(EntryOffset));
     {$ENDIF}

     {Convert Name}
     if not NameToBuffer(AEntry.Name,FNameBuffer) then Exit;

     {Checksum Name}
     Checksum:=ChecksumName(AEntry);
     NameCount:=EntryCount;
     while EntryCount > 0 do
      begin
       {Get Sectors}
       if not ReadSectors(SectorOffset + EntrySector,SectorCount,FClusterBuffer^) then Exit; {Note: SectorCount will never be more than SectorPerCluster}

       {Read Entries}
       while EntryOffset < FSectorSize do
        begin
         {Get Directory}
         Directory:=PLFNDirectory(PtrUInt(FClusterBuffer) + EntryOffset);

         {Check Count}
         if EntryCount > 1 then
          begin
           {Set Entry}
           if not BufferToDirectory(FNameBuffer,Directory,EntryCount - 1,Checksum,(EntryCount = NameCount)) then Exit;
          end
         else
          begin
           {$IFDEF FAT_DEBUG}
           if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.SetLong EntrySector = ' + IntToStr(EntrySector) + ' Offset = ' + IntToStr(EntryOffset));
           {$ENDIF}

           {Set Entry} {Use AltName}
           if not EntryToDirectory(TFATDiskEntry(AEntry),Directory,True) then Exit;

           {Set Offsets}
           TFATDiskEntry(AEntry).EntryOffset:=EntryOffset;
           TFATDiskEntry(AEntry).EntrySector:=EntrySector;
          end;

         Dec(EntryCount);
         if EntryCount = 0 then Break; {Break to allow completion}

         {Get Next Entry}
         Inc(EntryOffset,fatEntrySize);
        end;

       {Set Sectors}
       if not WriteSectors(SectorOffset + EntrySector,SectorCount,FClusterBuffer^) then Exit;

       {Get Next Sector}
       EntryOffset:=0;

       if not GetNextDirectorySector(AParent,EntrySector,True) then Break; {GetNextDirectorySector returns False on failure (Invalid/Last)} {Break to allow completion}
      end;

     Result:=True;
    finally
     ClusterUnlock;
    end;
   end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{==============================================================================}

function TFATFileSystem.SizeEntry(AParent,AEntry:TDiskEntry;const ASize:Int64):Boolean;
var
 CurrentSize:Int64;
 EntryCount:LongWord;
 CurrentCount:LongWord;

 LastCluster:LongWord;
 NextCluster:LongWord;
 StartCluster:LongWord;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if AParent = nil then Exit;
  if AEntry = nil then Exit;
  if AEntry = FRoot then Exit; {Cannot size Root}

  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.SizeEntry EntryName = ' + AEntry.Name + ' Size = ' + IntToStr(ASize));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Relative}
  if (AEntry.Attributes and (faDot or faDotDot)) <> faNone then Exit;

  {Check Entry}
  if (AEntry.Attributes and faMatchMask) <> faFile then Exit;

  {Get Start}
  StartCluster:=TFATDiskEntry(AEntry).StartCluster;

  {Check Count}
  CurrentCount:=GetChainClusterCount(StartCluster);
  CurrentSize:=(CurrentCount shl FClusterShiftCount); //To Do //Int64 ?? - FAT is only 32bit ?
  if CurrentSize < AEntry.Size then Exit;
  if CurrentSize > (AEntry.Size + FClusterSize) then Exit; //To Do //Improve this check ?? - cases where size is multiple of cluster (Use shr etc) (More accurate)

  {Check Size}
  if ASize = 0 then
   begin
    {Zero}
    {Release Clusters}
    if not ReleaseCluster(FFreeCluster,StartCluster) then Exit;

    {Update Entry}
    AEntry.Size:=ASize;
    TFATDiskEntry(AEntry).StartCluster:=FFreeCluster;

    if not SetEntry(AParent,AEntry) then Exit;
   end
  else if ASize < AEntry.Size then
   begin
    {Smaller}
    {Get Count}
    EntryCount:=((ASize - 1) shr FClusterShiftCount) + 1; {Minus 1 to account for size equals cluster}
    if EntryCount < CurrentCount then
     begin
      {Get New Last Cluster}
      CurrentCount:=0;
      LastCluster:=StartCluster;
      NextCluster:=GetNextChainCluster(LastCluster);
      if NextCluster = 0 then Exit;
      Inc(CurrentCount);
      while CurrentCount < EntryCount do
       begin
        LastCluster:=NextCluster;
        NextCluster:=GetNextChainCluster(LastCluster);
        if NextCluster = 0 then Exit;

        Inc(CurrentCount);
       end;

      {Release Clusters}
      if not ReleaseCluster(LastCluster,NextCluster) then Exit;
     end;

    {Update Entry}
    AEntry.Size:=ASize;

    if not SetEntry(AParent,AEntry) then Exit;
   end
  else if ASize > AEntry.Size then
   begin
    {Larger}
    {Get Count}
    EntryCount:=((ASize - 1) shr FClusterShiftCount) + 1; {Minus 1 to account for size equals cluster}
    if EntryCount > CurrentCount then
     begin
      {Check for Empty}
      if StartCluster = 0 then
       begin
        {Allocate Cluster}
        if not AllocCluster(FFreeCluster,StartCluster,1) then Exit;
        Inc(CurrentCount);

        {Update Entry}
        TFATDiskEntry(AEntry).StartCluster:=StartCluster;
       end;

      {Get Current Last Cluster}
      LastCluster:=GetLastChainCluster(StartCluster);
      if LastCluster = 0 then Exit;

      {Allocate Clusters}
      if CurrentCount < EntryCount then
       begin
        if not AllocCluster(LastCluster,NextCluster,(EntryCount - CurrentCount)) then Exit;
       end;
     end;

    {Update Entry}
    AEntry.Size:=ASize;

    if not SetEntry(AParent,AEntry) then Exit;
   end;

  Result:=True; {Note: If Size is same then just succeed}
 finally
  FEntries.WriterUnlock;
 end;
end;

{==============================================================================}

function TFATFileSystem.GetBlock(ABlockNo:LongWord):TDiskBlock;
{Overidden to implement multiple entry blocks}
var
 BlockNo:LongWord;
begin
 {}
 Result:=nil;

 {$IFDEF FAT_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.GetBlock BlockNo = ' + IntToStr(ABlockNo));
 {$ENDIF}

 if FDriver = nil then Exit;
 if ABlockNo >= FTotalClusterCount then Exit;

 {Get Block No}
 if FEntriesPerBlock = 0 then Exit;
 {BlockNo:=((ABlockNo div FEntriesPerBlock) * FEntriesPerBlock);}
 BlockNo:=((ABlockNo shr FBlockShiftCount) shl FBlockShiftCount);

 {Call Inherited Method}
 Result:=inherited GetBlock(BlockNo);
end;

{==============================================================================}

function TFATFileSystem.GetBlockEx(ABlockNo:LongWord;AWrite:Boolean):TDiskBlock;
{Overidden to implement multiple entry blocks}
var
 BlockNo:LongWord;
begin
 {}
 Result:=nil;

 if FDriver = nil then Exit;
 if ABlockNo >= FTotalClusterCount then Exit;

 {Get Block No}
 if FEntriesPerBlock = 0 then Exit;
 {BlockNo:=((ABlockNo div FEntriesPerBlock) * FEntriesPerBlock);}
 BlockNo:=((ABlockNo shr FBlockShiftCount) shl FBlockShiftCount);

 {Call Inherited Method}
 Result:=inherited GetBlockEx(BlockNo,AWrite);
end;

{==============================================================================}

function TFATFileSystem.CheckName(const AName:String):Boolean;
//To Do //CheckName should have Attributes to allow varying behaviour based on type (eg Label)
var
 Count:Integer;
begin
 {}
 Result:=False;

 {Check for Length}
 if Length(AName) = 0 then Exit;
 if Length(AName) > FMaxFile then Exit;

 {Check for Long}
 if FLongNames then
  begin
   {Check Invalid Chars}
   for Count:=1 to Length(AName) do
    begin
     if AName[Count] in INVALID_FILENAME_CHARS then Exit;
    end;
  end
 else
  begin
   {Check Invalid and Substitute Chars}
   for Count:=1 to Length(AName) do
    begin
     if AName[Count] in INVALID_FILENAME_CHARS then Exit;
     if AName[Count] in SHORT_FILENAME_SUBST_CHARS then Exit;
    end;
  end;

 Result:=True;
end;

{==============================================================================}

function TFATFileSystem.CountName(const AName:String):Byte;
begin
 {}
 Result:=0;

 if Length(AName) = 0 then Exit;

 Result:=1;
 while (Result * lfnEntryChars) < Length(AName) do
  begin
   Inc(Result);

   if Result = lfnEntryMax then Exit; {Maximum Order value for LFN is $3F}
  end;
end;

{==============================================================================}

function TFATFileSystem.CompareName(const AName,AMatch:String;AWildcard:Boolean):Boolean;
begin
 {}
 Result:=False;

 //To Do //Not used yet - See TFileSystem.MatchEntry/CompareName
 //Result:=True; //The base method will probably do for this
end;

{==============================================================================}

function TFATFileSystem.ChecksumName(AEntry:TDiskEntry):Byte;
var
 Name:TFATName;
 Count:LongWord;
begin
 {}
 Result:=0;

 {Get Name} {Use AltName}
 if EntryToName(TFATDiskEntry(AEntry),@Name,True) then
  begin
   {Checksum Name}
   for Count:=0 to 10 do
    begin
     if (Result and $01) = 0 then
      begin
       {Even number}
       Result:=(Result shr 1) + PByte(PtrUInt(@Name) + Count)^;
      end
     else
      begin
       {Odd number}
       Result:=($80 or (Result shr 1)) + PByte(PtrUInt(@Name) + Count)^;
      end;
    end;
  end;
end;

{==============================================================================}

function TFATFileSystem.GenerateName(AParent,AEntry:TDiskEntry;const AName:String):String;
var
 Hash:LongWord;
 Shift:LongWord;
 Step:Integer;
 Count:Integer;
 AltName:String;
 Current:TDiskEntry;
begin
 {}
 Result:=fatBlankName;

 if FDriver = nil then Exit;
 if AParent = nil then Exit;
 if Length(AName) = 0 then Exit;

 Step:=0;
 Shift:=0;
 Count:=1;
 Hash:=GenerateNameHash(AName,NAME_HASH_SIZE);
 AltName:=GenerateShortNameEx(AName,Count,Hash,False);

 Current:=GetEntryEx(AParent,AltName,faDirectory or faFile,False,False,True);
 while (Current <> nil) and (Current <> AEntry) do
  begin
   if Count < lfnMaxNameAlias then
    begin
     Inc(Count);
     AltName:=GenerateShortNameEx(AName,Count,Hash,False);

     Current:=GetEntryEx(AParent,AltName,faDirectory or faFile,False,False,True);
    end
   else
    begin
     if (Step >= lfnMaxNameAlias) and (Shift < lfnMaxHashShift) then
      begin
       Step:=0;
       Inc(Shift);
       Hash:=(Hash shr 1);
      end;
     if Step = lfnMaxHashAlias then Exit;
     Inc(Step);
     AltName:=GenerateShortNameEx(AName,Step,Hash,True);

     Current:=GetEntryEx(AParent,AltName,faDirectory or faFile,False,False,True);
    end;
  end;

 Result:=AltName;
end;

{==============================================================================}

function TFATFileSystem.GetNameFlags(const AName:String):LongWord;
var
 Ext:String;
 Name:String;
begin
 {}
 Result:=faNone;

 if not FLongNames then Exit;
 if not FCaseFlags then Exit;

 {Split Name}
 if not SplitFile(AName,Name,Ext) then Exit;

 {Check Name}
 if Lowercase(Name) = Name then Result:=Result or faFlagName;

 {Check Ext}
 if Lowercase(Ext) = Ext then Result:=Result or faFlagExt;
end;

{==============================================================================}

function TFATFileSystem.CheckFlagName(const AName:String):Boolean;
var
 Ext:String;
 Name:String;
begin
 {}
 Result:=False;

 if not FLongNames then Exit;
 if not FCaseFlags then Exit;

 {Check Short}
 if not IsEightDotThree(Uppercase(AName)) then Exit;

 {Split Name}
 if not SplitFile(AName,Name,Ext) then Exit;

 Result:=True;

 {Check Both}
 if (Lowercase(Name) = Name) and (Lowercase(Ext) = Ext) then Exit;

 {Check Name}
 if (Uppercase(Ext) = Ext) and (Lowercase(Name) = Name) then Exit;

 {Check Ext}
 if (Uppercase(Name) = Name) and (Lowercase(Ext) = Ext) then Exit;

 Result:=False;
end;

{==============================================================================}

function TFATFileSystem.ValidateName(AName:Pointer):Boolean;
var
 Value:Byte;
 Count:Integer;
 Space:Boolean;
 Directory:Boolean;
 FATDirectory:PFATDirectory;
begin
 {}
 Result:=False;

 if AName = nil then Exit;

 {Get Directory}
 FATDirectory:=PFATDirectory(AName);

 {Check Directory}
 Directory:=((FATDirectory.Attribute and faDirectory) = faDirectory);

 {Check Name}
 Space:=False;
 for Count:=1 to fatNameCount do
  begin
   Value:=Byte(FATDirectory.Name[Count - 1]);
   if Space then
    begin
     {Check space char}
     if Value <> fatSpaceChar then Exit;
    end
   else
    begin
     {Check base chars}
     if Value in fatBaseChars then Exit;

     {Check invalid chars}
     if Value in fatInvalidChars then Exit;

     {Check lowercase chars}
     if Value in fatLowercaseChars then Exit; {These dont account for the code page}

     {Check space char}
     if (Count = 1) and (Value = fatSpaceChar) then Exit;
     if (Count > 1) and (Value = fatSpaceChar) then Space:=True;

     {Check dot char}
     if (not Directory) and (Value = fatDotChar) then Exit;
     if (Directory) and (Count > 2) and (Value = fatDotChar) then Exit;
    end;
  end;

 {Check Ext}
 Space:=False;
 for Count:=1 to fatExtCount do
  begin
   Value:=Byte(FATDirectory.Ext[Count - 1]);
   if Space then
    begin
     {Check space char}
     if Value <> fatSpaceChar then Exit;
    end
   else
    begin
     {Check dot char}
     if Value = fatDotChar then Exit;

     {Check base chars}
     if Value in fatBaseChars then Exit;

     {Check invalid chars}
     if Value in fatInvalidChars then Exit;

     {Check lowercase chars}
     if Value in fatLowercaseChars then Exit; {These dont account for the code page}

     {Check space char}
     if Value = fatSpaceChar then Space:=True;
    end;
  end;

 Result:=True;
end;

{==============================================================================}

function TFATFileSystem.ValidateDirectory(ADirectory:Pointer):Boolean;
var
 FATDirectory:PFATDirectory;
 LFNDirectory:PLFNDirectory;
begin
 {}
 Result:=False;

 if ADirectory = nil then Exit;

 {Get Directory}
 FATDirectory:=PFATDirectory(ADirectory);
 LFNDirectory:=PLFNDirectory(ADirectory);

 {Check Directory}
 if FATDirectory.Attribute = faLongName then
  begin
   {Long Name}
   if LFNDirectory.Reserved <> 0 then Exit;
  end
 else if (FATDirectory.Attribute and faDirectory) = faDirectory then
  begin
   {Directory}
   if FATDirectory.Length <> 0 then Exit;
   if (FATDirectory.Attribute and faVolumeID) = faVolumeID then Exit;
  end
 else if (FATDirectory.Attribute and faVolumeID) = faVolumeID then
  begin
   {Label}
   if FATDirectory.Length <> 0 then Exit;
   if FATDirectory.FirstClusterLow <> 0 then Exit;
   if FATDirectory.FirstClusterHigh <> 0 then Exit;
  end
 else
  begin
   {File}
   {Nothing}
  end;

 Result:=True;
end;

{==============================================================================}

function TFATFileSystem.FileSystemInit:Boolean;
begin
 {}
 Result:=False;

 if not WriterLock then Exit;
 try
  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.FileSystemInit');
  {$ENDIF}

  if FDriver = nil then Exit;

  {Clear Current}
  FFATType:=ftNONE;
  FVolumeFlags:=0;

  FRoot:=nil;

  SetCurrent(nil);

  FChunks.ClearList;
  FTables.ClearList;
  FBlocks.ClearList;
  FEntries.ClearList;

  FLastFreeCluster:=fatUnknownCluster;
  FFreeClusterCount:=fatUnknownCluster;

  if FClusterBuffer <> nil then FreeMem(FClusterBuffer);
  FClusterBuffer:=nil;

  if FWriteBuffer <> nil then FreeMem(FWriteBuffer);
  FWriteBuffer:=nil;

  if FReadBuffer <> nil then FreeMem(FReadBuffer);
  FReadBuffer:=nil;

  if FNameBuffer <> nil then FreeMem(FNameBuffer);
  FNameBuffer:=nil;

  if FInfoBuffer <> nil then FreeMem(FInfoBuffer);
  FInfoBuffer:=nil;

  FInfoSector:=0;
  FInfoBackup:=0;

  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                Settings Cleared');
  {$ENDIF}

  {Setup Defaults}
  FPathChar:=LoadPathChar;
  FNameChar:=LoadNameChar;
  FFileChar:=LoadFileChar;
  FRootChar:=LoadRootChar;
  FRootName:=LoadRootName(False);
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
  FMinFileTime:=LoadMinFileTime;

  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                Defaults Set');
  {$ENDIF}

  {Check Volume}
  if FVolume = nil then Exit;
  if FVolume.Device = nil then Exit;
  FReadOnly:=not(FVolume.Device.Writeable);

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

  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                Volume Checked');
  {$ENDIF}

  {Get Boot Sector}
  if not ReadSectors(fatBootSector,1,FSectorBuffer^) then Exit;

  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                Boot Sector Read');
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                 PBootSector(FSectorBuffer).Signature=' + IntToHex(PBootSector(FSectorBuffer).Signature,4));
  {$ENDIF}

  {Check Boot Sector (Accept 0 due to some faulty devices)}
  if (PBootSector(FSectorBuffer).Signature <> BOOT_SECTOR_SIGNATURE) and (PBootSector(FSectorBuffer).Signature <> 0) then Exit;

  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                Boot Sector Checked');
  {$ENDIF}

  {Check for FAT}
  if PBootSector(FSectorBuffer).BootSignature = fatBootSignature then
   begin
    {$IFDEF FAT_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                Check for FAT12/16');
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                 PBootSector(FSectorBuffer).BootSignature=' + IntToHex(PBootSector(FSectorBuffer).BootSignature,2));
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                 PBootSector(FSectorBuffer).BPB.BytesPerSector=' + IntToStr(PBootSector(FSectorBuffer).BPB.BytesPerSector));
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                 PBootSector(FSectorBuffer).BPB.SectorsPerCluster=' + IntToStr(PBootSector(FSectorBuffer).BPB.SectorsPerCluster));
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                 PBootSector(FSectorBuffer).BPB.TotalSectors16=' + IntToStr(PBootSector(FSectorBuffer).BPB.TotalSectors16));
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                 PBootSector(FSectorBuffer).BPB.TotalSectors32=' + IntToStr(PBootSector(FSectorBuffer).BPB.TotalSectors32));
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                 PBootSector(FSectorBuffer).BPB.NumberOfFats=' + IntToStr(PBootSector(FSectorBuffer).BPB.NumberOfFats));
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                 PBootSector(FSectorBuffer).BPB.RootEntryCount=' + IntToStr(PBootSector(FSectorBuffer).BPB.RootEntryCount));
    {$ENDIF}

    {Check for FAT12/16}
    if PBootSector(FSectorBuffer).BPB.BytesPerSector = 0 then Exit;
    if (PBootSector(FSectorBuffer).BPB.BytesPerSector mod MIN_SECTOR_SIZE) <> 0 then Exit;
    if (PBootSector(FSectorBuffer).BPB.SectorsPerCluster = 0) then Exit;
    if (PBootSector(FSectorBuffer).BPB.SectorsPerCluster <> 1) and ((PBootSector(FSectorBuffer).BPB.SectorsPerCluster mod 2) <> 0) then Exit;
    if (PBootSector(FSectorBuffer).BPB.TotalSectors16 = 0) then
     begin
      if PBootSector(FSectorBuffer).BPB.TotalSectors32 = 0 then Exit;
      if PBootSector(FSectorBuffer).BPB.TotalSectors32 > FSectorCount then Exit;
     end
    else
     begin
      if (PBootSector(FSectorBuffer).BPB.TotalSectors32 <> 0) and (PBootSector(FSectorBuffer).BPB.TotalSectors32 <> PBootSector(FSectorBuffer).BPB.TotalSectors16) then Exit; {Modified 12/8/2007 to account for TotalSectors16 = TotalSectors32}
      if PBootSector(FSectorBuffer).BPB.TotalSectors16 > FSectorCount then Exit;
     end;
    if PBootSector(FSectorBuffer).BPB.NumberOfFats = 0 then Exit;
    if PBootSector(FSectorBuffer).BPB.RootEntryCount = 0 then Exit;
    {if PBootSector(FSectorBuffer).BPB.HiddenSectors <> FStartSector then Exit;} {Doesnt work for Extended or Logical}
    if ((PBootSector(FSectorBuffer).BPB.RootEntryCount * fatEntrySize) mod PBootSector(FSectorBuffer).BPB.BytesPerSector) <> 0 then Exit;

    {$IFDEF FAT_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                FAT12/16 Detected');
    {$ENDIF}

    {Calculate Parameters}
    FSectorSize:=PBootSector(FSectorBuffer).BPB.BytesPerSector;
    FSectorCount:=PBootSector(FSectorBuffer).BPB.TotalSectors16;
    if FSectorCount = 0 then FSectorCount:=PBootSector(FSectorBuffer).BPB.TotalSectors32;

    FBootSector:=fatBootSector;
    FBootBackup:=fatBootSector;

    FNumberOfFats:=PBootSector(FSectorBuffer).BPB.NumberOfFats;
    FSectorsPerFat:=PBootSector(FSectorBuffer).BPB.SectorsPerFat16;
    FSectorsPerCluster:=PBootSector(FSectorBuffer).BPB.SectorsPerCluster;

    FReservedSectors:=PBootSector(FSectorBuffer).BPB.ReservedSectors;

    FRootEntryCount:=PBootSector(FSectorBuffer).BPB.RootEntryCount;
    FRootSectorCount:=((FRootEntryCount * fatEntrySize) + FSectorSize - 1) div FSectorSize;
    FRootStartSector:=(FReservedSectors + (FSectorsPerFat * FNumberOfFats));

    FActiveFat:=0;       {Always 0 on FAT12/16}
    FFatMirroring:=True; {Always True on FAT12/16}

    FEntriesPerSector:=(FSectorSize div fatEntrySize);
    FEntriesPerCluster:=((FSectorSize * FSectorsPerCluster) div fatEntrySize);

    FEntriesPerBlock:=GetEntriesPerBlock(FSectorSize,ftFAT16);
    FSectorsPerBlock:=GetSectorsPerBlock(FSectorSize,ftFAT16);

    FBlockShiftCount:=GetBlockShiftCount(FSectorSize,ftFAT16);
    FSectorShiftCount:=GetSectorShiftCount(FSectorsPerCluster);
    FClusterShiftCount:=GetClusterShiftCount(FSectorSize * FSectorsPerCluster);

    FDataStartSector:=(FReservedSectors + (FSectorsPerFat * FNumberOfFats) + FRootSectorCount);
    FDataClusterCount:=((FSectorCount - FDataStartSector) div FSectorsPerCluster);
    FTotalClusterCount:=FDataClusterCount + 2;

    FClusterSize:=(FSectorSize * FSectorsPerCluster);
    FInfoBuffer:=GetMem(FSectorSize);
    if FInfoBuffer = nil then Exit;
    FNameBuffer:=GetMem(lfnMaxName);
    if FNameBuffer = nil then Exit;
    FReadBuffer:=GetMem(FClusterSize);
    if FReadBuffer = nil then Exit;
    FWriteBuffer:=GetMem(FClusterSize);
    if FWriteBuffer = nil then Exit;
    FClusterBuffer:=GetMem(FClusterSize);
    if FClusterBuffer = nil then Exit;

    {Reset Info Buffer}
    PFATInfoSector(FInfoBuffer).LeadSignature:=0;
    PFATInfoSector(FInfoBuffer).StructureSignature:=0;
    PFATInfoSector(FInfoBuffer).TrailSignature:=0;

    {$IFDEF FAT_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                Parameters Calculated');
    {$ENDIF}

    {Determine Type}
    FFATType:=ftFAT16;
    FEndOfFile:=fat16EndOfFile;
    FEndOfCluster:=fat16EndOfCluster;
    FFreeCluster:=fat16FreeCluster;
    FBadCluster:=fat16BadCluster;
    FMediaCluster:=fat16MediaCluster;
    FStartCluster:=fatStartCluster;
    FHardError:=fat16HardError;
    FCleanShutdown:=fat16CleanShutdown;
    FReservedBits:=0; {Not applicable on FAT16}
    if FDataClusterCount < fat16MinClusters then
     begin
      {Adjust Block Parameters}
      FEntriesPerBlock:=GetEntriesPerBlock(FSectorSize,ftFAT12);
      FSectorsPerBlock:=GetSectorsPerBlock(FSectorSize,ftFAT12);
      FBlockShiftCount:=GetBlockShiftCount(FSectorSize,ftFAT12);
      FFATType:=ftFAT12;
      FEndOfFile:=fat12EndOfFile;
      FEndOfCluster:=fat12EndOfCluster;
      FFreeCluster:=fat12FreeCluster;
      FBadCluster:=fat12BadCluster;
      FMediaCluster:=fat12MediaCluster;
      FStartCluster:=fatStartCluster;
      FHardError:=0;     {Not applicable on FAT12}
      FCleanShutdown:=0; {Not applicable on FAT12}
      FReservedBits:=0;  {Not applicable on FAT12}
     end;

    {$IFDEF FAT_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                Type Determined');
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                 FATType = ' + FATTypeToString(FFATType));
    {$ENDIF}

    {Create Root}
    FRoot:=TFATDiskEntry.Create(FEntryLocal);
    FRoot.Name:=FRootName;
    FRoot.AltName:=FRootPath;
    FRoot.Attributes:=faDirectory; {Must be a Directory}
    TFATDiskEntry(FRoot).StartSector:=FRootStartSector;
    FRoot.AddReference; {Prevent Deletion}

    FEntries.Add(FRoot,nil);

    {$IFDEF FAT_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                Root Created');
    {$ENDIF}
   end
  else
   begin
    {$IFDEF FAT_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                Check for FAT32');
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                 PExtBootSector(FSectorBuffer).BootSignature=' + IntToHex(PExtBootSector(FSectorBuffer).BootSignature,2));
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                 PExtBootSector(FSectorBuffer).BPB.BytesPerSector=' + IntToStr(PExtBootSector(FSectorBuffer).BPB.BytesPerSector));
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                 PExtBootSector(FSectorBuffer).BPB.SectorsPerCluster=' + IntToStr(PExtBootSector(FSectorBuffer).BPB.SectorsPerCluster));
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                 PExtBootSector(FSectorBuffer).BPB.TotalSectors16=' + IntToStr(PExtBootSector(FSectorBuffer).BPB.TotalSectors16));
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                 PExtBootSector(FSectorBuffer).BPB.TotalSectors32=' + IntToStr(PExtBootSector(FSectorBuffer).BPB.TotalSectors32));
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                 PExtBootSector(FSectorBuffer).BPB.NumberOfFats=' + IntToStr(PExtBootSector(FSectorBuffer).BPB.NumberOfFats));
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                 PExtBootSector(FSectorBuffer).BPB.FileSysVersion=' + IntToStr(PExtBootSector(FSectorBuffer).BPB.FileSysVersion));
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                 PExtBootSector(FSectorBuffer).BPB.RootEntryCount=' + IntToStr(PExtBootSector(FSectorBuffer).BPB.RootEntryCount));
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                 PExtBootSector(FSectorBuffer).BPB.SectorsPerFat16=' + IntToStr(PExtBootSector(FSectorBuffer).BPB.SectorsPerFat16));
    {$ENDIF}

    {Check for FAT32}
    if PExtBootSector(FSectorBuffer).BootSignature <> fatBootSignature then Exit;
    if PExtBootSector(FSectorBuffer).BPB.BytesPerSector = 0 then Exit;
    if (PExtBootSector(FSectorBuffer).BPB.BytesPerSector mod MIN_SECTOR_SIZE) <> 0 then Exit;
    if (PExtBootSector(FSectorBuffer).BPB.SectorsPerCluster = 0) then Exit;
    if (PExtBootSector(FSectorBuffer).BPB.SectorsPerCluster <> 1) and ((PExtBootSector(FSectorBuffer).BPB.SectorsPerCluster mod 2) <> 0) then Exit;
    if (PExtBootSector(FSectorBuffer).BPB.TotalSectors16 = 0) then
     begin
      if PExtBootSector(FSectorBuffer).BPB.TotalSectors32 = 0 then Exit;
      if PExtBootSector(FSectorBuffer).BPB.TotalSectors32 > FSectorCount then Exit;
     end
    else
     begin
      if (PExtBootSector(FSectorBuffer).BPB.TotalSectors32 <> 0) and (PExtBootSector(FSectorBuffer).BPB.TotalSectors32 <> PExtBootSector(FSectorBuffer).BPB.TotalSectors16) then Exit; {Modified 12/8/2007 to account for TotalSectors16 = TotalSectors32}
      if PExtBootSector(FSectorBuffer).BPB.TotalSectors16 > FSectorCount then Exit;
     end;
    if PExtBootSector(FSectorBuffer).BPB.NumberOfFats = 0 then Exit;
    if PExtBootSector(FSectorBuffer).BPB.FileSysVersion <> 0 then Exit;
    if PExtBootSector(FSectorBuffer).BPB.RootEntryCount <> 0 then Exit;
    if PExtBootSector(FSectorBuffer).BPB.SectorsPerFat16 <> 0 then Exit;
    {if PExtBootSector(FSectorBuffer).BPB.HiddenSectors <> FStartSector then Exit;} {Doesnt work for Extended or Logical}

    {$IFDEF FAT_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                FAT32 Detected');
    {$ENDIF}

    {Calculate Parameters}
    FSectorSize:=PExtBootSector(FSectorBuffer).BPB.BytesPerSector;
    FSectorCount:=PExtBootSector(FSectorBuffer).BPB.TotalSectors16;
    if FSectorCount = 0 then FSectorCount:=PExtBootSector(FSectorBuffer).BPB.TotalSectors32;

    FBootSector:=fatBootSector;
    FBootBackup:=PExtBootSector(FSectorBuffer).BPB.BackupBootSector;

    FNumberOfFats:=PExtBootSector(FSectorBuffer).BPB.NumberOfFats;
    FSectorsPerFat:=PExtBootSector(FSectorBuffer).BPB.SectorsPerFat32;
    FSectorsPerCluster:=PExtBootSector(FSectorBuffer).BPB.SectorsPerCluster;

    FReservedSectors:=PExtBootSector(FSectorBuffer).BPB.ReservedSectors;

    FInfoSector:=PExtBootSector(FSectorBuffer).BPB.FileSysInfoSector;
    FInfoBackup:=PExtBootSector(FSectorBuffer).BPB.BackupBootSector + 1;

    FRootStartCluster:=PExtBootSector(FSectorBuffer).BPB.RootCluster;

    FActiveFat:=(PExtBootSector(FSectorBuffer).BPB.ExtendedFlags and $000F);
    FFatMirroring:=(PExtBootSector(FSectorBuffer).BPB.ExtendedFlags and $0080) = 0;

    FEntriesPerSector:=(FSectorSize div fatEntrySize);
    FEntriesPerCluster:=((FSectorSize * FSectorsPerCluster) div fatEntrySize);

    FEntriesPerBlock:=GetEntriesPerBlock(FSectorSize,ftFAT32);
    FSectorsPerBlock:=GetSectorsPerBlock(FSectorSize,ftFAT32);

    FBlockShiftCount:=GetBlockShiftCount(FSectorSize,ftFAT32);
    FSectorShiftCount:=GetSectorShiftCount(FSectorsPerCluster);
    FClusterShiftCount:=GetClusterShiftCount(FSectorSize * FSectorsPerCluster);

    FDataStartSector:=(FReservedSectors + (FSectorsPerFat * FNumberOfFats));
    FDataClusterCount:=((FSectorCount - FDataStartSector) div FSectorsPerCluster);
    FTotalClusterCount:=FDataClusterCount + 2;

    FClusterSize:=(FSectorSize * FSectorsPerCluster);
    FInfoBuffer:=GetMem(FSectorSize);
    if FInfoBuffer = nil then Exit;
    FNameBuffer:=GetMem(lfnMaxName);
    if FNameBuffer = nil then Exit;
    FReadBuffer:=GetMem(FClusterSize);
    if FReadBuffer = nil then Exit;
    FWriteBuffer:=GetMem(FClusterSize);
    if FWriteBuffer = nil then Exit;
    FClusterBuffer:=GetMem(FClusterSize);
    if FClusterBuffer = nil then Exit;

    {Reset Info Buffer}
    PFATInfoSector(FInfoBuffer).LeadSignature:=0;
    PFATInfoSector(FInfoBuffer).StructureSignature:=0;
    PFATInfoSector(FInfoBuffer).TrailSignature:=0;

    {$IFDEF FAT_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                Parameters Calculated');
    {$ENDIF}

    {Determine Type}
    FFATType:=ftFAT32;
    FEndOfFile:=fat32EndOfFile;
    FEndOfCluster:=fat32EndOfCluster;
    FFreeCluster:=fat32FreeCluster;
    FBadCluster:=fat32BadCluster;
    FMediaCluster:=fat32MediaCluster;
    FStartCluster:=fatStartCluster;
    FHardError:=fat32HardError;
    FCleanShutdown:=fat32CleanShutdown;
    FReservedBits:=fat32ReservedBits;

    {$IFDEF FAT_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                Type Determined');
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                 FATType = ' + FATTypeToString(FFATType));
    {$ENDIF}

    {Create Root}
    FRoot:=TFATDiskEntry.Create(FEntryLocal);
    FRoot.Name:=FRootName;
    FRoot.AltName:=FRootPath;
    FRoot.Attributes:=faDirectory; {Must be a Directory}
    TFATDiskEntry(FRoot).StartCluster:=FRootStartCluster;
    FRoot.AddReference; {Prevent Deletion}

    FEntries.Add(FRoot,nil);

    {$IFDEF FAT_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                Root Created');
    {$ENDIF}
   end;

  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then
   begin
    FileSysLogDebug('TFATFileSystem.FileSystemInit');
    FileSysLogDebug(' NumberOfFats = ' + IntToStr(FNumberOfFats));
    FileSysLogDebug(' SectorsPerFat = ' + IntToStr(FSectorsPerFat));
    FileSysLogDebug(' SectorsPerCluster = ' + IntToStr(FSectorsPerCluster));
    FileSysLogDebug('');
    FileSysLogDebug(' ReservedSectors = ' + IntToStr(FReservedSectors));
    FileSysLogDebug('');
    FileSysLogDebug(' RootEntryCount = ' + IntToStr(FRootEntryCount));
    FileSysLogDebug(' RootSectorCount = ' + IntToStr(FRootSectorCount));
    FileSysLogDebug(' RootStartSector = ' + IntToStr(FRootStartSector));
    FileSysLogDebug(' RootStartCluster = ' + IntToStr(FRootStartCluster));
    FileSysLogDebug('');
    FileSysLogDebug(' ActiveFat = ' + IntToStr(FActiveFat));
    FileSysLogDebug(' FatMirroring = ' + BoolToStr(FFatMirroring));
    FileSysLogDebug('');
    FileSysLogDebug(' EntriesPerSector = ' + IntToStr(FEntriesPerSector));
    FileSysLogDebug(' EntriesPerCluster = ' + IntToStr(FEntriesPerCluster));
    FileSysLogDebug('');
    FileSysLogDebug(' EntriesPerBlock = ' + IntToStr(FEntriesPerBlock));
    FileSysLogDebug(' SectorsPerBlock = ' + IntToStr(FSectorsPerBlock));
    FileSysLogDebug('');
    FileSysLogDebug(' BlockShiftCount = ' + IntToStr(FBlockShiftCount));
    FileSysLogDebug(' SectorShiftCount = ' + IntToStr(FSectorShiftCount));
    FileSysLogDebug(' ClusterShiftCount = ' + IntToStr(FClusterShiftCount));
    FileSysLogDebug('');
    FileSysLogDebug(' DataStartSector = ' + IntToStr(FDataStartSector));
    FileSysLogDebug(' DataClusterCount = ' + IntToStr(FDataClusterCount));
    FileSysLogDebug(' TotalClusterCount = ' + IntToStr(FTotalClusterCount));
    FileSysLogDebug('');
    FileSysLogDebug(' ClusterSize = ' + IntToStr(FClusterSize));
   end;
  {$ENDIF}

  {Load Tables}
  LoadTables;

  {Load Block}
  LoadBlock(0);

  {Set Current}
  SetCurrent(FRoot);

  {Setup Values}
  FSystemName:=LoadSystemName;
  FVolumeName:=LoadVolumeName;
  FVolumeSerial:=LoadVolumeSerial;
  FFileSysType:=LoadFileSysType;

  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.FileSystemInit Completed');
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

{==============================================================================}

function TFATFileSystem.MountFileSystem:Boolean;
begin
 {}
 Result:=False;

 if not WriterLock then Exit;
 try
  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.MountFileSystem');
  {$ENDIF}

  if FDriver = nil then Exit;

  //To do - Allows for Mount/Dismount by formatter without dismounting Drive/Volume
  //Also allows updating the VolumeFlags (Dirty on Mount) on FAT32
  //See NTFS

  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.MountFileSystem Completed');
  {$ENDIF}

  //Result:=True; //To do
 finally
  WriterUnlock;
 end;
end;

{==============================================================================}

function TFATFileSystem.DismountFileSystem:Boolean;
begin
 {}
 Result:=False;

 if not WriterLock then Exit;
 try
  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.DismountFileSystem');
  {$ENDIF}

  if FDriver = nil then Exit;

  //To do - Allows for Mount/Dismount by formatter without dismounting Drive/Volume
  //Also allows updating the InfoSector and VolumeFlags (Clean on Dismount) on FAT32
  //See NTFS

  {Update Info Sector}
  UpdateInfoSector;

  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.DismountFileSystem Completed');
  {$ENDIF}

  //Result:=True; //To do
 finally
  WriterUnlock;
 end;
end;

{==============================================================================}

function TFATFileSystem.InitializeFileSystem(ASectorsPerCluster:LongWord;AFileSysType:TFileSysType):Boolean;
begin
 {}
 Result:=False;

 if not WriterLock then Exit;
 try
  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.InitializeFileSystem');
  {$ENDIF}

  if FDriver = nil then Exit;

  //To do - Allows for formatter to init a new volume with the default information
  //See NTFS

  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.InitializeFileSystem Completed');
  {$ENDIF}

  //Result:=True; //To do
 finally
  WriterUnlock;
 end;
end;

{==============================================================================}

function TFATFileSystem.GetDriveLabel:String;
{Get Volume Label from Root Directory and Boot Sector}
{Overidden to Account for Volume Label entry in Root}
var
 Current:TDiskEntry;
begin
 {}
 Result:=fatBlankName;

 if not ReaderLock then Exit;
 try
  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.GetDriveLabel');
  {$ENDIF}

  if FDriver = nil then Exit;

  {Get Label}
  Current:=MatchEntryEx(FRoot,nil,fatLabelMask,faVolumeID,True,True,False,False);
  if Current <> nil then
   begin
    Result:=Current.Name;

    {Remove Reference}
    Current.RemoveReference;
   end
  else
   begin
    if GetVolumeName <> fatDefaultName then
     begin
      Result:=GetVolumeName;
     end;
   end;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TFATFileSystem.SetDriveLabel(const ALabel:String):Boolean;
{Set Volume Label in Root Directory and Boot Sector}
var
 Current:TDiskEntry;
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.SetDriveLabel');
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                Label = ' + ALabel);
  {$ENDIF}

  if FDriver = nil then Exit;

  {Get Label}
  Current:=MatchEntryEx(FRoot,nil,fatLabelMask,faVolumeID,True,True,False,False);
  if Current <> nil then
   begin
    try
     {Check Name}
     if Length(ALabel) <> 0 then
      begin
       {No need to check File or Folder as Label can have same name}
       {Check Name}
       if not CheckName(ALabel) then Exit;

       {Rename Label}
       if not RenameEntry(FRoot,Current,ALabel) then Exit;

       {Set Volume Name}
       if not SetVolumeName(ALabel) then Exit;

       Result:=True;
      end
     else
      begin
       {Remove Label}
       if not RemoveEntry(FRoot,Current) then Exit;

       {Set Volume Name}
       if not SetVolumeName(fatDefaultName) then Exit;

       Result:=True;
      end;
    finally
     {Remove Reference}
     Current.RemoveReference;
    end;
   end
  else
   begin
    {Check Name}
    if Length(ALabel) <> 0 then
     begin
      {No need to check File or Folder as Label can have same name}
      {Check Name}
      if not CheckName(ALabel) then Exit;

      {Add Label}
      Current:=AddEntry(FRoot,ALabel,faVolumeID,False);
      if Current = nil then Exit;

      {Set Volume Name}
      if not SetVolumeName(ALabel) then Exit;

      Result:=True;
     end
    else
     begin
      {Set Volume Name}
      if not SetVolumeName(fatDefaultName) then Exit;

      Result:=True;
     end;
   end;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TFATFileSystem.SetDriveSerial(ASerial:LongWord):Boolean;
{Set Volume Serial in Boot Sector}
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.SetDriveSerial');
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                Serial = ' + IntToHex(ASerial,8));
  {$ENDIF}

  {Set Volume Serial}
  if not SetVolumeSerial(ASerial) then Exit;

  Result:=True;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TFATFileSystem.GetDriveFreeSpaceEx:Int64;
{Calculate Free space from FAT (or FileSysInfo for FAT32)}
begin
 {}
 Result:=0;

 if not ReaderLock then Exit;
 try
  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.GetDriveFreeSpaceEx');
  {$ENDIF}

  if FDriver = nil then Exit;

  {Check Free Cluster Count}
  if GetFreeClusterCount = fatUnknownCluster then Exit;

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

function TFATFileSystem.GetDriveTotalSpaceEx:Int64;
{Calculate Total space from internal FAT data}
begin
 {}
 Result:=0;

 if not ReaderLock then Exit;
 try
  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.GetDriveTotalSpaceEx');
  {$ENDIF}

  if FDriver = nil then Exit;

  {Return Total Space}
  Result:=FDataClusterCount;
  Result:=(Result * FSectorsPerCluster);
  Result:=(Result * FSectorSize);
  {Modified to ensure Int64 mutliply}
  {Result:=((FDataClusterCount * FSectorsPerCluster) * FSectorSize);}
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TFATFileSystem.GetDriveInformation(var AClusterSize:LongWord;var ATotalClusterCount,AFreeClusterCount:Int64):Boolean;
{Get Drive Information from internal FAT data}
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  {$IFDEF FAT_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TFATFileSystem.GetDriveInformation');
  {$ENDIF}

  if FDriver = nil then Exit;

  {Check Free Cluster Count}
  if GetFreeClusterCount = fatUnknownCluster then Exit;

  {Return Drive Information}
  AClusterSize:=FClusterSize;
  ATotalClusterCount:=FDataClusterCount;
  AFreeClusterCount:=FFreeClusterCount;

  Result:=True;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}
{==============================================================================}
{TFATDiskBlock}
constructor TFATDiskBlock.Create(ALocalLock:TMutexHandle);
begin
 {}
 inherited Create(ALocalLock);
 FBlockBuffer:=nil;
end;

{==============================================================================}

destructor TFATDiskBlock.Destroy;
begin
 {}
 if FBlockBuffer <> nil then FreeMem(FBlockBuffer);
 FBlockBuffer:=nil;
 inherited Destroy;
end;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure FATFSInit;
var
 Recognizer:TFATRecognizer;
begin
 {}
 {Check Initialized}
 if FATFSInitialized then Exit;

 {Check Driver}
 if FileSysDriver = nil then Exit;

 {Create FAT Recognizer}
 if FILESYS_FAT_ENABLED then
  begin
   Recognizer:=TFATRecognizer.Create(FileSysDriver);
   Recognizer.AllowDrive:=FILESYS_DRIVES_ENABLED;
   Recognizer.AllowDefault:=FAT_DEFAULT;
   Recognizer.CaseFlags:=FAT_CASE_FLAGS;
   Recognizer.LongNames:=FAT_LONG_NAMES;
   Recognizer.OemConvert:=FAT_OEM_CONVERT;
   Recognizer.NumericTail:=FAT_NUMERIC_TAIL;
   Recognizer.DirtyCheck:=FAT_DIRTY_CHECK;
   Recognizer.QuickCheck:=FAT_QUICK_CHECK;
   Recognizer.InfoSectorEnable:=FAT_INFO_SECTOR_ENABLE;
   Recognizer.InfoImmediateUpdate:=FAT_INFO_IMMEDIATE_UPDATE;
  end;

 FATFSInitialized:=True;
end;

{==============================================================================}

procedure FATFSQuit;
var
 NextRecognizer:TRecognizer;
 CurrentRecognizer:TRecognizer;
 NextFileSystem:TFileSystem;
 CurrentFileSystem:TFileSystem;
begin
 {}
 {Check Initialized}
 if not FATFSInitialized then Exit;

 {Check Driver}
 if FileSysDriver = nil then Exit;

 {Terminate FileSystems}
 NextFileSystem:=FileSysDriver.GetFileSystemByNext(nil,True,False,FILESYS_LOCK_READ);
 while NextFileSystem <> nil do
  begin
   CurrentFileSystem:=NextFileSystem;
   NextFileSystem:=FileSysDriver.GetFileSystemByNext(CurrentFileSystem,True,False,FILESYS_LOCK_READ);

   if CurrentFileSystem is TFATFileSystem then
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

   if CurrentRecognizer is TFATRecognizer then
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

 FATFSInitialized:=False;
end;

{==============================================================================}
{==============================================================================}
{FATFS Functions}

{==============================================================================}
{==============================================================================}
{FATFS Helper Functions}
function FATTypeToString(AType:TFATType):String;
begin
 {}
 Result:='ftNONE';

 {Check Type}
 case AType of
  ftFAT12:Result:='ftFAT12';
  ftFAT16:Result:='ftFAT16';
  ftFAT32:Result:='ftFAT32';
 end;
end;

{==============================================================================}
{==============================================================================}

initialization
 FATFSInit;

{==============================================================================}

finalization
 FATFSQuit;

{==============================================================================}
{==============================================================================}

end.



