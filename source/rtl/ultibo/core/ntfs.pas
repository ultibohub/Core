{
Ultibo NTFS interface unit.

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


NT FileSystem
=============

 Notes: NTFS uses 64 bit cluster values in all cases

        All structures in NTFS are 8 or 16 byte aligned

        All file record numbers and file references are dealt with as Int64
        but can be overlayed with RecordNumber or FileReference structures
        if needed to access the individual members

 Notes: Attribute data stream sizing:

        LastVCN is updated by AllocRun/ReleaseRun
        AttributeSize is updated by AddRun/RemoveRun
        StreamSize is updated by SizeRun
        StreamAllocated is updated by SizeRun
        InitializedSize is updated by SizeRun/WriteRun
        StreamUsed is updated by SizeRun/CompressRun/DecompressRun

 Notes: The use of IsEightDotThree and GenerateShortName(Ex) from
        the UltiboUtils unit should be replaced by internal routines
        optimised for maximum performance

 Questions: Can a data run in a multiple instance attribute include instances that
            are resident or does the non resident state of the first instance cause
            all instances to be non resident? Can we cope with either ?

            Can an attribute have multiple instances (same type/name) without an
            attribute list existing. If so how are they sorted, is it simply based
            on StartVCN list attribute list items ?
            Currently implemented as StartVCN for third sort criteria, need to confirm

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit NTFS;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,FileSystem,SysUtils,Classes,Unicode,Security,Ultibo,UltiboUtils,UltiboClasses,
     NTFSConst,NTFSTypes,NTFSClass,NTFSUtils;

//To Do //Look for:

//Int64(Pointer()^) -> PInt64()^
//Word(Pointer()^) -> PWord()^

//Testing

//WideString
  //Change to UnicodeString for FPC

//Critical

//Int64

//Lock

//Unlock

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
{const}
 {NTFS specific constants}
 {See NTFSConst.pas}

{==============================================================================}
{type}
 {NTFS specific types}
 {See NTFSStruct.pas}

{==============================================================================}
type
 {NTFS specific classes}
 TNTFSRecognizer = class(TRecognizer)
   constructor Create(ADriver:TFileSysDriver);
  private
   {Private Variables}
   FLongNames:Boolean;
   FDataStreams:Boolean;
   FReparsePoints:Boolean;
   FCaseSensitive:Boolean;

   FResetLog:Boolean;        {Reset the Log File if it was dirty on mount}
   FFixedZone:Boolean;       {Use the Vista/2008/7 Fixed MFT Zone values (not the NT/2000/XP percentages)}
   FAltLayout:Boolean;       {Use the Vista/2008/7 Volume Layout values (not the NT/2000/XP layout)}
   FLenient:Boolean;         {Allow certain non fatal errors to be ignored}
   FDefensive:Boolean;       {Perform more defensive checking of structures and values}
   FAggressive:Boolean;      {Attempt to correct certain errors during operation}
   FNoShortNames:Boolean;    {Do not create short file names}
   FNullSecurity:Boolean;    {Do not apply security when creating files and folders (only apply security when SetSecurity called)}
   FDefaultSecurity:Boolean; {Apply default permissions (Everyone, Full Control) when creating files and folders}

   {Private Methods}
   function CheckLBA:Boolean;
   function CheckNTFS:Boolean;
   function CheckBootSector(ABootSector:PBootSector;const AStartSector:Int64;ASectorCount:LongWord):Boolean;
  protected
   {Protected Variables}

   {Protected Methods}
   function GetName:String; override;
  public
   {Public Variables}
   property LongNames:Boolean read FLongNames write FLongNames;
   property DataStreams:Boolean read FDataStreams write FDataStreams;
   property ReparsePoints:Boolean read FReparsePoints write FReparsePoints;
   property CaseSensitive:Boolean read FCaseSensitive write FCaseSensitive;

   property ResetLog:Boolean read FResetLog write FResetLog;
   property FixedZone:Boolean read FFixedZone write FFixedZone;
   property AltLayout:Boolean read FAltLayout write FAltLayout;
   property Lenient:Boolean read FLenient write FLenient;
   property Defensive:Boolean read FDefensive write FDefensive;
   property Aggressive:Boolean read FAggressive write FAggressive;
   property NoShortNames:Boolean read FNoShortNames write FNoShortNames;
   property NullSecurity:Boolean read FNullSecurity write FNullSecurity;
   property DefaultSecurity:Boolean read FDefaultSecurity write FDefaultSecurity;

   {Public Methods}
   function RecognizePartitionId(APartitionId:Byte):Boolean; override;
   function RecognizeBootSector(ABootSector:PBootSector;const AStartSector,ASectorCount:Int64):Boolean; override;

   function RecognizePartition(APartition:TDiskPartition):Boolean; override;
   function RecognizeVolume(AVolume:TDiskVolume):Boolean; override;
   function MountVolume(AVolume:TDiskVolume;ADrive:TDiskDrive):Boolean; override;
 end;

 TNTFSPartitioner = class(TDiskPartitioner)
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

 TNTFSFormatter = class(TDiskFormatter)
  private
   {Private Variables}
   //To Do //Create a list of FormatHandles to allow Callback
   //and multiple simultaneous formats. The handle would contain all the variables
   //of the formatting operation in progress

   //Also, the formatter here (and probably should be for others) will use
   //Mount/Dismount/InitializeFileSystem to avoid code duplication
   //The FileSystem can be obtained from the volume or created during format ?

   //Note: When formatting an NTFS volume, the first 16 MFT records (0 to F) have the
   //sequence number set to the same value as the record number except for MFT (record 0) which
   //has sequence number 1 (and so does MFTMirr, record 1)
   //These values never change in NTFS

   //For the formatter we need to add to the filesystem, CreateMetafiles, CreateMetafile etc

   {Private Methods}
   function CheckDevice(AVolume:TDiskVolume;ADrive:TDiskDrive;AFloppyType:TFloppyType):Boolean;
   function CheckPartition(AVolume:TDiskVolume;ADrive:TDiskDrive;AFileSysType:TFileSysType):Boolean;

   function GetSectorsPerCluster(AVolume:TDiskVolume;ADrive:TDiskDrive;AFloppyType:TFloppyType;AFileSysType:TFileSysType):LongWord;
  public
   {Public Variables}

   {Public Methods}
   function AcceptVolume(AVolume:TDiskVolume;AFloppyType:TFloppyType;AFileSysType:TFileSysType):Boolean; override;
   function FormatVolume(AVolume:TDiskVolume;AFloppyType:TFloppyType;AFileSysType:TFileSysType):Boolean; override;
 end;

 TNTFSDefragger = class(TDiskDefragger)
  private
   {Private Variables}

   {Private Methods}
  public
   {Public Variables}

   {Public Methods}
 end;

 TNTFSRepairer = class(TDiskRepairer)
  private
   {Private Variables}

   {Private Methods}
  public
   {Public Variables}

   {Public Methods}
 end;

 TNTFSFileSystem = class(TFileSystem)
   constructor Create(ADriver:TFileSysDriver;AVolume:TDiskVolume;ADrive:TDiskDrive);
   destructor Destroy; override;
  private
   {Private Variables}
   FNTFSType:TNTFSType;
   FVolumeVersion:Word;
   FVolumeFlags:LongWord;        {Volume flags}

   FResetLog:Boolean;            {Reset the Log File if it was dirty on mount}
   FFixedZone:Boolean;           {Use the Vista/2008/7 Fixed MFT Zone values (not the NT/2000/XP percentages)}
   FAltLayout:Boolean;           {Use the Vista/2008/7 Volume Layout values (not the NT/2000/XP layout)}
   FLenient:Boolean;             {Allow certain non fatal errors to be ignored}
   FDefensive:Boolean;           {Perform more defensive checking of structures and values}
   FAggressive:Boolean;          {Attempt to correct certain errors during operation}
   FNoShortNames:Boolean;        {Do not create short file names}
   FNullSecurity:Boolean;        {Do not apply security when creating files and folders (only apply security when SetSecurity called)}
   FDefaultSecurity:Boolean;     {Apply default permissions (Everyone, Full Control) when creating files and folders}

   {NTFS Variables}
   FSectorsPerCluster:LongWord;  {Usually 1,2,4,8,16,32,64,128 etc}

   FMftStartCluster:Int64;       {Start Cluster of $MFT}
   FMftMirrorCluster:Int64;      {Start Cluster of $MFTMirr}

   FMftZoneStart:Int64;          {Start Cluster of the MftZone}
   FMftZoneCluster:Int64;        {End Cluster of the MftZone}
   FMftMirrorCount:LongWord;     {Number of Mft entries to mirror}
   FMftZoneReservation:Word;     {Size of the Mft reserved zone}

   FFileRecordSize:LongWord;     {Size of a FILE Record in Bytes}
   FIndexRecordSize:LongWord;    {Size of an INDX Record in Bytes}

   FClustersPerFile:LongWord;    {Clusters per FILE Record or 0 if less than cluster size}
   FClustersPerIndex:LongWord;   {Clusters per INDX Record or 0 if less than cluster size}

   FFilesPerCluster:LongWord;    {FILE Records per Cluster or 0 if more than cluster size}
   FIndexsPerCluster:LongWord;   {INDX Records per Cluster or 0 if more than cluster size}

   FEntriesPerBlock:LongWord;    {Number of Cluster entries per Block of bitmap}
   FClustersPerBlock:LongWord;   {Number of Clusters per Block of bitmap entries}
   FTotalBlockCount:LongWord;    {Total number of Blocks in bitmap}

   FBlockShiftCount:Word;        {Shift count for Cluster <-> BlockNo}
   FSectorShiftCount:Word;       {Shift count for Sector <-> Cluster}
   FClusterShiftCount:Word;      {Shift count for Cluster <-> Bytes}

   FFileRecordShiftCount:Word;   {Shift count for Record -> VCN}
   FFileRecordOffsetMask:Int64;  {Mask for Record offset calculation}

   FIndexCounterShift:Word;      {Shift count for Record -> Counter}
   FIndexCounterOffset:LongWord; {Offset between Index Record Numbers}

   FIndexRecordShiftCount:Word;  {Shift count for Record -> VCN}
   FIndexRecordOffsetMask:Int64; {Mask for Record offset calculation}

   FTotalClusterCount:Int64;     {Total number of clusters on volume}

   FLastMftCluster:Int64;        {Or ntfsUnknownCluster if not known}
   FLastFreeCluster:Int64;       {Or ntfsUnknownCluster if not known}
   FFreeClusterCount:Int64;      {Or ntfsUnknownCluster if not known}

   FTotalFileRecordCount:Int64;  {Total number of file records on volume}

   FLastFreeFileRecord:Int64;     {Or ntfsUnknownRecordNumber if not known}
   FFreeFileRecordCount:Int64;    {Or ntfsUnknownRecordNumber if not known}
   FReservedFileRecordCount:Int64;{Or ntfsUnknownRecordNumber if not known}

   FClusterSize:LongWord;        {Size of a Cluster in Bytes (Max 65536 > Word)}

   FMft:TNTFSDiskEntry;          {Note: Root already provided by TFileSystem}
   FMftMirr:TNTFSDiskEntry;
   FLogFile:TNTFSDiskEntry;
   FVolInfo:TNTFSDiskEntry;
   FAttrDef:TNTFSDiskEntry;
   FBitmap:TNTFSDiskEntry;
   FBoot:TNTFSDiskEntry;
   FBadClus:TNTFSDiskEntry;
   FSecure:TNTFSDiskEntry;
   FUpCase:TNTFSDiskEntry;
   FExtend:TNTFSDiskEntry;
   FObjId:TNTFSDiskEntry;
   FQuota:TNTFSDiskEntry;
   FReparse:TNTFSDiskEntry;
   FUsnJrnl:TNTFSDiskEntry;

   FMaster:TNTFSDiskRecord;
   FMirror:TNTFSDiskRecord;

   FUpCases:TNTFSUpCase;          {Uppercase table for doing case conversions (from $UpCase)}
   FAttrDefs:TNTFSAttrDefs;       {Attribute definitions for doing schema checks (from $AttrDef)}
   FSecuritys:TNTFSSecurityItems; {Security Descriptors for handling security (from $Secure:$SDS)}

   FRecords:TNTFSRecordIndex;     {Index of file record numbers for lookups}

   FFileBuffer:Pointer;          {Buffer of exactly FILE record size (or Cluster size if smaller than 1 cluster)} {Used by LoadRecord, SetRecord, GetVolumeType}
   FFileLock:TMutexHandle;       {Lock for file buffer}

   FIndexBuffer:Pointer;         {Buffer of exactly INDX record size (or Cluster size if smaller than 1 cluster)} {Used by LoadNode}

   FReadBuffer:Pointer;          {Buffer for partial cluster attribute reads (Cluster size)}                      {Used by ReadAttribute}
   FReadLock:TMutexHandle;       {Lock for read buffer}

   FWriteBuffer:Pointer;         {Buffer for partial cluster attribute writes (Cluster size)}                     {Used by WriteAttribute}
   FWriteLock:TMutexHandle;      {Lock for write buffer}

   FClusterBuffer:Pointer;       {Buffer of exactly cluster size}                                                 {Used by FillClusters}
   FClusterLock:TMutexHandle;    {Lock for cluster buffer}

   FCompressionBuffer:Pointer;   {Buffer of exactly compression unit size (Normally 16 clusters)}                 {Used by ReadRun, WriteRun, CompressRun, DecompressRun}
   FCompressionLock:TMutexHandle;{Lock for compression buffer}

   FDecompressionBuffer:Pointer; {Compression unit size is obtained by doing (FClusterSize shl CompressionUnit)}
   FDecompressionLock:TMutexHandle;{Lock for deccompression buffer}
                                 {VCN to Unit can be obtained by doing (VCN shr CompressionUnit}
                                 {Unit to StartVCN can be obtained by doing (Unit shl CompressionUnit)}
                                 {VCN to StartVCN can be obtained by doing ((VCN shr CompressionUnit) shl CompressionUnit)}

   {Private Methods}
   function FileLock:Boolean;
   function FileUnlock:Boolean;

   function ReadLock:Boolean;
   function ReadUnlock:Boolean;

   function WriteLock:Boolean;
   function WriteUnlock:Boolean;

   function ClusterLock:Boolean;
   function ClusterUnlock:Boolean;

   function AllocCompressionBuffer(ACompressionUnit:Word;AForce:Boolean):Pointer;
   function ReleaseCompressionBuffer(ACompressionUnit:Word;ABuffer:Pointer):Boolean;

   function AllocDecompressionBuffer(ACompressionUnit:Word;AForce:Boolean):Pointer;
   function ReleaseDecompressionBuffer(ACompressionUnit:Word;ABuffer:Pointer):Boolean;

   {Flag Methods}
   function GetDirty:Boolean;
   procedure SetDirty(AValue:Boolean);

   function GetVolumeFlag(AFlag:LongWord):Boolean;
   procedure SetVolumeFlag(AFlag:LongWord;AValue:Boolean);

   function LoadVolumeFlags:LongWord;
   function SetVolumeFlags(AFlags:LongWord):Boolean;

   {Version Methods}
   function GetVolumeType:TNTFSType;
   function GetVolumeVersion:Word;

   {Cluster Methods}
   function FillClusters(const ACluster:Int64;ACount:Word;AValue:Byte):Boolean;
   function ReadClusters(const ACluster:Int64;ACount:Word;var ABuffer):Boolean;
   function WriteClusters(const ACluster:Int64;ACount:Word;const ABuffer):Boolean;

   function MarkClusters(const ACluster,ACount:Int64):Boolean;
   function AllocClusters(var ACluster,ACount:Int64;AMft:Boolean):Boolean;
   function ReleaseClusters(const ACluster,ACount:Int64):Boolean;

   function GetNextFreeCluster(AMft:Boolean):Int64; {Return is a Cluster}
   function GetFreeClusterCount:Int64;

   {Run Methods}
   function AllocRun(AAttribute:TNTFSDiskAttribute;const ACount:Int64;AMft,ASparse:Boolean):Boolean;
   function ReleaseRun(AAttribute:TNTFSDiskAttribute;const ACount:Int64):Boolean;

   function GetRunUnit(AAttribute:TNTFSDiskAttribute;const AVCN:Int64;var AUnit,ALength:Int64):Boolean;
   function GetRunCount(AAttribute:TNTFSDiskAttribute;const AVCN:Int64;var ACount:Int64):Boolean;
   function GetRunLength(AAttribute:TNTFSDiskAttribute;const AVCN:Int64;var ALength:Int64):Boolean;
   function GetRunCluster(AAttribute:TNTFSDiskAttribute;const AVCN:Int64;var ACluster,ALength:Int64):Boolean;

   {Unit Methods}
   function GetUnitVCN(AAttribute:TNTFSDiskAttribute;const AUnit:Int64;var AVCN,ALength:Int64):Boolean;
   function GetUnitCompressed(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute;const AUnit:Int64;var AVCN,ALength:Int64):Boolean;

   {File Methods}
   function AllocFileRecord(var ARecordNumber:Int64;AMft:Boolean):Boolean;
   function ReleaseFileRecord(const ARecordNumber:Int64):Boolean;
   function GetFileRecordVCN(const ARecordNumber:Int64;var AVCN:Int64;var AOffset:LongWord):Boolean;

   function GetNextFreeFileRecord(AMft:Boolean):Int64; {Return is a Record Number}
   function GetFreeFileRecordCount:Int64;
   function GetReservedFileRecordCount:Int64;

   {Index Methods}
   function AllocIndexRecord(ARecord:TNTFSDiskRecord;AAllocation,ABitmap:TNTFSDiskAttribute;AIndex:TNTFSDiskIndex;var ARecordNumber:Int64):Boolean;
   function ReleaseIndexRecord(ARecord:TNTFSDiskRecord;AAllocation,ABitmap:TNTFSDiskAttribute;AIndex:TNTFSDiskIndex;const ARecordNumber:Int64):Boolean;
   function GetIndexRecordVCN(AIndex:TNTFSDiskIndex;const ARecordNumber:Int64;var AVCN:Int64;var AOffset:LongWord):Boolean;

   function GetNextFreeIndexRecord(ARecord:TNTFSDiskRecord;AAllocation,ABitmap:TNTFSDiskAttribute;AIndex:TNTFSDiskIndex):Int64; {Return is a Record Number}
   function GetFreeIndexRecordCount(ARecord:TNTFSDiskRecord;AAllocation,ABitmap:TNTFSDiskAttribute;AIndex:TNTFSDiskIndex):Int64;

   {Block Methods}
   function AllocBlock(ABlock:TNTFSDiskBlock;const AStart:Int64;var ACount:LongWord):Boolean;
   function ReleaseBlock(ABlock:TNTFSDiskBlock;const AStart:Int64;ACount:LongWord):Boolean;

   function GetBlockNextFree(ABlock:TNTFSDiskBlock;const AStart:Int64):Int64; {Return is a Cluster}
   function GetBlockFreeCount(ABlock:TNTFSDiskBlock):Int64;

   {Bitmap Methods}
   function AllocBitmap(ABuffer:Pointer;ASize:LongWord;AStart:LongWord;var ACount:LongWord):Boolean;
   function ReleaseBitmap(ABuffer:Pointer;ASize:LongWord;AStart:LongWord;ACount:LongWord):Boolean;

   function GetBitmapNextFree(ABuffer:Pointer;ASize,AStart:LongWord):LongWord;
   function GetBitmapFreeCount(ABuffer:Pointer;ASize:LongWord):LongWord;

   {Misc Methods}
   function ReadFixup(ABuffer:Pointer;AOffset:LongWord;ASequenceNumber,ASequenceOffset,ASequenceLength:Word;AFree:Boolean):Boolean;
   function WriteFixup(ABuffer:Pointer;AOffset:LongWord;ASequenceNumber,ASequenceOffset,ASequenceLength:Word):Boolean;

   function AddObjId(const AObjectId:TGUID;const AFileReference:Int64;ABirthVolumeId,ABirthObjectId,ADomainId:PGUID):Boolean;
   function RemoveObjId(const AObjectId:TGUID):Boolean;

   function AddQuota(AOwner:LongWord;AQuota:PNTFSQuotaData):Boolean;
   function RemoveQuota(AOwner:LongWord):Boolean;
   function SetQuota(AOwner:LongWord;AQuota:PNTFSQuotaData):Boolean;

   function AddOwner(ASID:PSID):LongWord;                                                   {Return is an OwnerId}
   function RemoveOwner(ASID:PSID):Boolean;

   function AddReparse(AReparseTag:LongWord;const AFileReference:Int64):Boolean;
   function RemoveReparse(AReparseTag:LongWord;const AFileReference:Int64):Boolean;

   function AddSecurity(ASecurity:TNTFSSecurity):LongWord;                                  {Return is a SecurityId}
   function RemoveSecurity(ASecurity:TNTFSSecurity):Boolean;

   function GetObjId(const AObjectId:TGUID):Int64;                                          {Return is a FileReference}
   function GetQuota(AOwner:LongWord):PNTFSQuotaData;
   function GetOwner(ASID:PSID):LongWord;                                                   {Return is an OwnerId}
   function GetReparseByReference(AReparseTag:LongWord;const AFileReference:Int64):Int64;   {Return is a FileReference}
   function GetSecurityId(ASecurity:TNTFSSecurity;AWrite:Boolean):LongWord;                 {Return is a SecurityId}
   function GetSecurityById(ASecurityId:LongWord):TNTFSSecurity;
   function GetDescriptorId(ADescriptor:Pointer;AWrite:Boolean):LongWord;                   {Return is a SecurityId}

   function GetNextOwnerId:LongWord;                                                        {Return is an OwnerId}
   function GetNextSecurityId:LongWord;                                                     {Return is a SecurityId}
   function GetNextSecurityOffset:Int64;                                                    {Return is an Offset}
   function GetNextSecurityOffsetEx(ASecuritySize:LongWord;var AStreamSize:Int64):Int64;    {Return is an Offset}

   function GetNextMoveAttribute(ARecord:TNTFSDiskRecord):TNTFSDiskAttribute;
   function GetNextSplitAttribute(ARecord:TNTFSDiskRecord):TNTFSDiskAttribute;
   function GetNextConvertAttribute(ARecord:TNTFSDiskRecord):TNTFSDiskAttribute;

   function CreateObjIds:Boolean;
   function CreateOwners:Boolean;
   function CreateQuotas:Boolean;
   function CreateReparses:Boolean;
   function CreateFileNames:Boolean;

   function CreateBoots:Boolean;
   function CreateBitmaps:Boolean;
   function CreateLogFiles:Boolean;

   function CreateUpCases:Boolean;
   function CreateAttrDefs:Boolean;
   function CreateSecuritys:Boolean;

   function LoadUpCases:Boolean;
   function LoadAttrDefs:Boolean;
   function LoadSecuritys:Boolean;

   function GetMftZoneStart:Int64;
   function GetMftZoneCluster:Int64;
   function GetMftMirrorCount:LongWord;
   function GetMftZoneReservation:Word;
   function ShrinkMftZoneReservation:Boolean;

   function GetFileRecordSize(AClustersPerFile:LongInt):LongWord;
   function GetIndexRecordSize(AClustersPerIndex:LongInt):LongWord;

   function GetClustersPerFile(AClustersPerFile:LongInt):LongWord;
   function GetClustersPerIndex(AClustersPerIndex:LongInt):LongWord;

   function GetFilesPerCluster(AClustersPerFile:LongInt):LongWord;
   function GetIndexsPerCluster(AClustersPerIndex:LongInt):LongWord;

   function GetBlockShiftCount(AClusterSize:LongWord):Word;
   function GetSectorShiftCount(ASectorsPerCluster:LongWord):Word;
   function GetClusterShiftCount(AClusterSize:LongWord):Word;

   function GetFileRecordShiftCount(AClusterSize,AFileRecordSize:LongWord):Word;
   function GetFileRecordOffsetMask(AFilesPerCluster:LongWord):Int64;

   function GetIndexCounterShift(AIndexCounterOffset:LongWord):Word;
   function GetIndexCounterOffset(AClustersPerIndex:LongWord):LongWord;

   function GetIndexRecordShiftCount(AClusterSize,AIndexRecordSize:LongWord):Word;
   function GetIndexRecordOffsetMask(AIndexsPerCluster:LongWord):Int64;

   function GetEntriesPerBlock(AClusterSize:LongWord):LongWord;
   function GetClustersPerBlock(AClusterSize:LongWord):LongWord;
   function GetTotalBlockCount(const ATotalClusterCount:Int64):LongWord;

   function CalculateStartCluster(const ATotalClusterCount:Int64):Int64;
   function CalculateMirrorCluster(const ATotalClusterCount:Int64):Int64;

   function CalculateClustersPerFile(AFileRecordSize,AClusterSize:LongWord):LongWord;
   function CalculateClustersPerIndex(AIndexRecordSize,AClusterSize:LongWord):LongWord;

   function CalculateFilesPerCluster(AFileRecordSize,AClusterSize:LongWord):LongWord;
   function CalculateIndexsPerCluster(AIndexRecordSize,AClusterSize:LongWord):LongWord;

   function CalculateBpbClustersPerFile:LongInt;
   function CalculateBpbClustersPerIndex:LongInt;

   function CalculateMftSize:Int64;
   function CalculateMftMirrSize:Int64;
   function CalculateLogFileSize:Int64;
   function CalculateAttrDefSize:Int64;
   function CalculateBitmapSize:Int64;
   function CalculateBootSize:Int64;
   function CalculateBadClusBadSize:Int64;
   function CalculateSecureSdsSize:Int64;
   function CalculateUpCaseSize:Int64;
   function CalculateRootAllocationSize:Int64;
   function CalculateRootSecurityDescriptorSize:Int64;

   function CalculateBitmapStartCluster:Int64;
   function CalculateUpCaseStartCluster:Int64;
   function CalculateAttrDefStartCluster:Int64;
   function CalculateLogFileStartCluster:Int64;
   function CalculateSecureSdsStartCluster:Int64;
   function CalculateRootAllocationStartCluster:Int64;
   function CalculateRootSecurityDescriptorStartCluster:Int64;

   {Log File Methods}
   function CheckLog:Boolean;
   function ClearLog(AForce:Boolean):Boolean;

   function MarkDirty:Boolean;
   function MarkClean:Boolean;
   function CheckClean:Boolean;

   function MarkMount:Boolean;
   function MarkDismount:Boolean;

   {Convert / Compare Methods}
   //function ConvertCase   //To Do //Use the $UpCase data to convert to uppercase  //Must be WideString / UnicodeString
   //function CompareCase   //To Do //Use the $UpCase data to do uppercase compare  //Must be WideString / UnicodeString

   function CheckVolumeName(const AName:String):Boolean;
   function CheckAttributeName(const AName:String):Boolean;

   function CompareSecurityDescriptor(ASecurityId:LongWord;ADescriptor:Pointer;ASize:Word):Boolean;

   function NTFSTypeToNTFSVersion(ANTFSType:TNTFSType):Word;
   function NTFSTypeToFileSysType(ANTFSType:TNTFSType):TFileSysType;
   function FileSysTypeToNTFSType(AFileSysType:TFileSysType):TNTFSType;
  protected
   {Protected Variables}

   {Protected Methods}
   function LoadMaxFile:Integer; override;
   function LoadMaxPath:Integer; override;
   function LoadAttributes:LongWord; override;
   function LoadMountPointTag:LongWord; override;
   function LoadSymbolicLinkTag:LongWord; override;
   function LoadSystemName:String; override;
   function LoadVolumeName:String; override;
   function LoadVolumeGUID:String; override;
   function LoadVolumeSerial:LongWord; override;
   function LoadFileSysType:TFileSysType; override;

   function SetVolumeName(const AName:String):Boolean;
   function SetVolumeSerial(ASerial:LongWord):Boolean;

   function ReadEntry(AParent,AEntry:TDiskEntry;var ABuffer;const AStart:Int64;ACount:LongWord;var AOffset,AValue:LongWord):Integer; override;
   function WriteEntry(AParent,AEntry:TDiskEntry;const ABuffer;const AStart:Int64;ACount:LongWord;var AOffset,AValue:LongWord):Integer; override;

   function ReadAttribute(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute;var ABuffer;const AStart:Int64;ACount:LongWord;var AInstance:LongWord;AWrite:Boolean):Integer; {Not override}
   function WriteAttribute(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute;const ABuffer;const AStart:Int64;ACount:LongWord;var AInstance:LongWord;AUpdate:Boolean):Integer; {Not override}

   function ReadRun(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute;var ABuffer;const AVCN:Int64;ACount:LongWord;var AInstance:LongWord;ARaw,AWrite:Boolean):LongWord; {Not override}
   function WriteRun(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute;const ABuffer;const AVCN:Int64;ACount:LongWord;var AInstance:LongWord;ARaw,AUpdate:Boolean):LongWord; {Not override}

   function ReadFileRecord(const ARecordNumber:Int64;var ABuffer;const AVCN:Int64;ACount:LongWord):Boolean; {Not override}
   function WriteFileRecord(const ARecordNumber:Int64;const ABuffer;const AVCN:Int64;ACount:LongWord;AMirror:Boolean):Boolean; {Not override}

   function CreateTables:Boolean; {Not override}
   function CreateBlocks:Boolean; {Not override}
   function CreateMetafiles:Boolean; {Not override}

   function CreateTable(ATableNo:LongWord):Boolean; {Not override}
   function CreateBlock(ABlockNo:LongWord):Boolean; {Not override}
   function CreateMetafile(AFileNo:LongWord):Boolean; {Not override}

   function CreateMft:Boolean; {Not override}
   function CreateMftMirr:Boolean; {Not override}
   function CreateLogFile:Boolean; {Not override}
   function CreateVolume:Boolean; {Not override}
   function CreateAttrDef:Boolean; {Not override}
   function CreateRoot:Boolean; {Not override}
   function CreateBitmap:Boolean; {Not override}
   function CreateBoot:Boolean; {Not override}
   function CreateBadClus:Boolean; {Not override}
   function CreateSecure:Boolean; {Not override}
   function CreateUpCase:Boolean; {Not override}
   function CreateExtend:Boolean; {Not override}
   function CreateObjId:Boolean; {Not override}
   function CreateQuota:Boolean; {Not override}
   function CreateReparse:Boolean; {Not override}
   function CreateUsnJrnl:Boolean; {Not override}
   function CreateReserved:Boolean; {Not override}
   function CreateExpansion:Boolean; {Not override}

   function LoadTables:Boolean; override;
   function LoadBlocks:Boolean; override;
   function LoadMetafiles:Boolean; {Not override}
   function LoadEntriesNew(AParent:TDiskEntry):Boolean; //Remove
   function LoadEntries(AParent:TDiskEntry):Boolean; override;
   function LoadRecords(ARecord:TNTFSDiskRecord;ARoot:TNTFSDiskAttribute):Boolean; {Not override}

   function LoadLists(ARecord:TNTFSDiskRecord):Boolean; {Not override}
   function LoadIndexes(ARecord:TNTFSDiskRecord):Boolean; {Not override}

   function LoadTable(ATableNo:LongWord):Boolean; override;
   function LoadBlock(ABlockNo:LongWord):Boolean; override;
   function LoadMetafile(AFileNo:LongWord):Boolean; {Not override}
   function LoadEntry(AParent:TNTFSDiskEntry;ARecord:TNTFSDiskRecord;AAttribute,AAlternate:TNTFSDiskAttribute):Boolean; {Not override}
   function LoadRecord(ABase:TNTFSDiskRecord;const ARecordNumber:Int64;AFree:Boolean):Boolean; {Not override}

   function LoadList(ARecord:TNTFSDiskRecord;AList:TNTFSDiskAttribute):Boolean; {Not override}
   function LoadIndex(ARecord:TNTFSDiskRecord;ARoot:TNTFSDiskAttribute):Boolean; {Not override}

   function LoadNode(ARecord:TNTFSDiskRecord;AAllocation:TNTFSDiskAttribute;AIndex:TNTFSDiskIndex;AParent:TNTFSDiskKey):Boolean; {Not override}

   function UnloadEntries(AParent:TDiskEntry):Boolean; override;
   //To Do //UnloadRecords etc

   function AddEntry(AParent:TDiskEntry;const AName:String;AAttributes:LongWord;AReference:Boolean):TDiskEntry; override;
   function AddEntryEx(AParent:TDiskEntry;const AName,AAltName:String;AAttributes:LongWord;AReference:Boolean):TDiskEntry; override;
   function RemoveEntry(AParent,AEntry:TDiskEntry):Boolean; override;
   function RenameEntry(AParent,AEntry:TDiskEntry;const AName:String):Boolean; override;
   function RenameEntryEx(AParent,AEntry:TDiskEntry;const AAltName:String):Boolean; override;
   function MoveEntry(ASource,ADest,AEntry:TDiskEntry):Boolean; override;

   function AddHardlink(AEntry,AParent:TDiskEntry;const AName:String;AReference:Boolean):TDiskEntry; override;

   function AddMountPoint(AEntry:TDiskEntry;const ATarget:String):Boolean; override;
   function RemoveMountPoint(AEntry:TDiskEntry):Boolean; override;

   function AddJunctionPoint(AEntry:TDiskEntry;const ATarget:String):Boolean; override;
   function RemoveJunctionPoint(AEntry:TDiskEntry):Boolean; override;

   function AddSymbolicLink(AParent:TDiskEntry;const AName,ATarget:String;AFolder,AReference:Boolean):TDiskEntry; override;
   function AddSymbolicLinkEx(AParent:TDiskEntry;const AName,AAltName,ATarget:String;AFolder,AReference:Boolean):TDiskEntry; override;

   function AddRecord(ABase:TNTFSDiskRecord;AFolder:Boolean):TNTFSDiskRecord; {Not override}
   function RemoveRecord(ARecord:TNTFSDiskRecord):Boolean; {Not override}

   function AddAttribute(ARecord:TNTFSDiskRecord;AType:LongWord;const AName:String):TNTFSDiskAttribute; {Not override}
   function RemoveAttribute(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute):Boolean; {Not override}
   function RenameAttribute(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute;const AName:String):Boolean; {Not override}
   function MoveAttribute(ASource,ADest:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute):Boolean; {Not override}

   function AddRun(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute;const ACount:Int64;AMft,ASparse:Boolean):Boolean; {Not override}
   function RemoveRun(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute;const AVCN,ACount:Int64;ADelete:Boolean):Boolean; {Not override}
   function MoveRun(ARecord:TNTFSDiskRecord;ASource,ADest:TNTFSDiskAttribute;const ACount:Int64):Boolean; {Not override}

   function AddList(ARecord:TNTFSDiskRecord):Boolean; {Not override}
   function RemoveList(ARecord:TNTFSDiskRecord;AList:TNTFSDiskAttribute):Boolean; {Not override}

   function AddIndex(ARecord:TNTFSDiskRecord;AType,ARule:LongWord;const AName:String):Boolean; {Not override}
   function RemoveIndex(ARecord:TNTFSDiskRecord;ARoot:TNTFSDiskAttribute):Boolean; {Not override}

   function AddNode(ARecord:TNTFSDiskRecord;AAllocation,ABitmap:TNTFSDiskAttribute;AIndex:TNTFSDiskIndex;ANode:TNTFSDiskNode):Boolean; {Not override}
   function RemoveNode(ARecord:TNTFSDiskRecord;AAllocation,ABitmap:TNTFSDiskAttribute;AIndex:TNTFSDiskIndex;ANode:TNTFSDiskNode):Boolean; {Not override}

   function AddItem(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute):Boolean; {Not override}
   function RemoveItem(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute):Boolean; {Not override}
   function RenameItem(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute):Boolean; {Not override}
   function MoveItem(ASource,ADest:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute):Boolean; {Not override}

   function AddKey(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute):Boolean; {Not override}
   function RemoveKey(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute):Boolean; {Not override}
   function RenameKey(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute;const AName:String):Boolean; {Not override}
   function MoveKey(ASource,ADest:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute):Boolean; {Not override}

   function SetRecords(ARecord:TNTFSDiskRecord):Boolean; {Not override}

   function SetBlock(ABlock:TDiskBlock):Boolean; override;
   function SetEntry(AParent,AEntry:TDiskEntry):Boolean; override;
   function SetSecurity(AParent,AEntry:TDiskEntry;ASecurity:TDiskSecurity):Boolean; override;
   function SetRecord(ARecord:TNTFSDiskRecord):Boolean; {Not override}
   function SetAttribute(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute):Boolean; {Not override}

   function SetList(ARecord:TNTFSDiskRecord;AList:TNTFSDiskAttribute):Boolean; {Not override}
   function SetIndex(ARecord:TNTFSDiskRecord;ARoot:TNTFSDiskAttribute):Boolean; {Not override}

   function SetNode(ARecord:TNTFSDiskRecord;AAllocation:TNTFSDiskAttribute;AIndex:TNTFSDiskIndex;ANode:TNTFSDiskNode;AParent:TNTFSDiskKey;AEmpty:Boolean):Boolean; {Not override}

   function SetItem(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute):Boolean; {Not override}

   function SetKey(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute):Boolean; {Not override}

   function SizeEntry(AParent,AEntry:TDiskEntry;const ASize:Int64):Boolean; override;
   function SizeRecord(ARecord:TNTFSDiskRecord;ASize:LongWord):Boolean; {Not override}
   function SizeAttribute(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute;const ASize:Int64):Boolean; {Not override}
   function SizeRun(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute;const ASize:Int64):Boolean; {Not override}

   function EncryptEntry(AParent,AEntry:TDiskEntry):Boolean; {Not override}
   function DecryptEntry(AParent,AEntry:TDiskEntry):Boolean; {Not override}
   function CompressEntry(AParent,AEntry:TDiskEntry):Boolean; {Not override}
   function DecompressEntry(AParent,AEntry:TDiskEntry):Boolean; {Not override}

   function ConvertAttribute(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute):Boolean; {Not override}
   function EncryptAttribute(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute):Boolean; {Not override}
   function DecryptAttribute(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute):Boolean; {Not override}
   function CompressAttribute(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute):Boolean; {Not override}
   function DecompressAttribute(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute):Boolean; {Not override}

   function ConvertRun(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute;const AVCN,ACount:Int64;ASparse:Boolean):Boolean; {Not override}
   function CompressRun(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute;const AVCN,ACount:Int64):Boolean; {Not override}
   function DecompressRun(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute;const AVCN,ACount:Int64):Boolean; {Not override}

   function GetBlock(ABlockNo:LongWord):TDiskBlock; override;
   function GetBlockEx(ABlockNo:LongWord;AWrite:Boolean):TDiskBlock; override;
   function GetReparse(AEntry:TDiskEntry):TDiskReparse; override;
   function GetSecurity(AEntry:TDiskEntry):TDiskSecurity; override;
   function GetSecurityEx(AEntry:TDiskEntry;AInherited:Boolean;var ASecurityId:LongWord;AWrite:Boolean):TDiskSecurity; {Not override}
   function GetMetafile(AFileNo:LongWord):TDiskEntry; {Not override}
   function GetMetafileEx(AFileNo:LongWord;AWrite:Boolean):TDiskEntry; {Not override}
   function GetRecord(ABase:TNTFSDiskRecord;const ARecordNumber:Int64;AFree:Boolean):TNTFSDiskRecord; {Not override}
   function GetRecordEx(ABase:TNTFSDiskRecord;const ARecordNumber:Int64;AFree,AWrite:Boolean):TNTFSDiskRecord; {Not override}
   function GetReference(ABase:TNTFSDiskRecord;const AFileReference:Int64):TNTFSDiskRecord; {Not override}
   function GetReferenceEx(ABase:TNTFSDiskRecord;const AFileReference:Int64;AWrite:Boolean):TNTFSDiskRecord; {Not override}

   function IsShort(const AName:String):Boolean; {Not override}
   function CheckName(const AName:String):Boolean; override;
   function CompareName(const AName,AMatch:String;AWildcard:Boolean):Boolean; override;
   function CheckAltName(const AAltName:String):Boolean; override;
   function CompareAltName(const AAltName,AMatch:String;AWildcard:Boolean):Boolean; override;

   function GenerateName(AParent,AEntry:TDiskEntry;const AName:String):String; {Not override}

   function GetSecurityFromDescriptor(ADescriptor:Pointer):TDiskSecurity; override;
  public
   {Public Variables}
   property ReadOnly:Boolean read FReadOnly write FReadOnly;
   property LongNames:Boolean read FLongNames write FLongNames;
   property DataStreams:Boolean read FDataStreams write FDataStreams;
   property ReparsePoints:Boolean read FReparsePoints write FReparsePoints;
   property CaseSensitive:Boolean read FCaseSensitive write FCaseSensitive;

   property Dirty:Boolean read GetDirty write SetDirty;
   property VolumeFlag[Idx:LongWord]:Boolean read GetVolumeFlag write SetVolumeFlag;

   property ResetLog:Boolean read FResetLog write FResetLog;
   property FixedZone:Boolean read FFixedZone write FFixedZone;
   property AltLayout:Boolean read FAltLayout write FAltLayout;
   property Lenient:Boolean read FLenient write FLenient;
   property Defensive:Boolean read FDefensive write FDefensive;
   property Aggressive:Boolean read FAggressive write FAggressive;
   property NoShortNames:Boolean read FNoShortNames write FNoShortNames;
   property NullSecurity:Boolean read FNullSecurity write FNullSecurity;
   property DefaultSecurity:Boolean read FDefaultSecurity write FDefaultSecurity;

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

{==============================================================================}
{var}
 {NTFS specific variables}

var
 {Formatting Variables}
 ntfsBootJump:TNtfsBootSectorJump = ($EB,$52,$90);
 ntfsBootCode:TNtfsBootSectorCode = (
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
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$44,$57,$4C,
  $44,$52,$20,$20,$20,$20,$20,$20,$00,$00
 );

{==============================================================================}
{Initialization Functions}
procedure NTFSInit;
procedure NTFSQuit;

{==============================================================================}
{NTFS Functions}

{==============================================================================}
{NTFS Helper Functions}
{See NTFSUtils.pas}

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {NTFS specific variables}
 NTFSInitialized:Boolean;

{==============================================================================}
{==============================================================================}
{TNTFSRecognizer}
constructor TNTFSRecognizer.Create(ADriver:TFileSysDriver);
begin
 {}
 inherited Create(ADriver);
 FLongNames:=True;
 FDataStreams:=True;
 FReparsePoints:=True;
 FCaseSensitive:=False; {Should be True according to XP}
 FResetLog:=False;
 FFixedZone:=False;
 FAltLayout:=False;
 FLenient:=False;
 FDefensive:=False;
 FAggressive:=False;
 FNoShortNames:=False;
 FNullSecurity:=False;
 FDefaultSecurity:=False;

 FAllowDrive:=True;
 FAllowDefault:=False;

 FPartitioner:=TNTFSPartitioner.Create(FDriver,Self);
 FFormatter:=TNTFSFormatter.Create(FDriver,Self);
 FDefragger:=TNTFSDefragger.Create(FDriver,Self);
 FRepairer:=TNTFSRepairer.Create(FDriver,Self);
end;

{==============================================================================}

function TNTFSRecognizer.CheckLBA:Boolean;
begin
 {}
 Result:=False;

 if FDriver = nil then Exit;

 Result:=True; //To Do //Make Configurable via GlobalConfig
end;

{==============================================================================}

function TNTFSRecognizer.CheckNTFS:Boolean;
begin
 {}
 Result:=False;

 if FDriver = nil then Exit;

 Result:=True; //To Do //Make Configurable via GlobalConfig
end;

{==============================================================================}

function TNTFSRecognizer.CheckBootSector(ABootSector:PBootSector;const AStartSector:Int64;ASectorCount:LongWord):Boolean;
begin
 {}
 Result:=False;

 if ABootSector = nil then Exit;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSRecognizer.CheckBootSector - StartSector = ' + IntToStr(AStartSector) + ' SectorCount = ' + IntToStr(ASectorCount));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('                                  BPB.BytesPerSector    = ' + IntToStr(PNtfsBootSector(ABootSector).BPB.BytesPerSector));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('                                  BPB.SectorsPerCluster = ' + IntToStr(PNtfsBootSector(ABootSector).BPB.SectorsPerCluster));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('                                  BPB.TotalSectors      = ' + IntToStr(PNtfsBootSector(ABootSector).BPB.TotalSectors));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('                                  BPB.MFTCluster        = ' + IntToStr(PNtfsBootSector(ABootSector).BPB.MFTCluster));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('                                  BPB.MFTMirror         = ' + IntToStr(PNtfsBootSector(ABootSector).BPB.MFTMirror));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('                                  BPB.ClustersPerFile   = ' + IntToStr(PNtfsBootSector(ABootSector).BPB.ClustersPerFile));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('                                  BPB.ClustersPerIndex  = ' + IntToStr(PNtfsBootSector(ABootSector).BPB.ClustersPerIndex));
 {$ENDIF}

 {Check Boot Sector}
 if PNtfsBootSector(ABootSector).Signature <> BOOT_SECTOR_SIGNATURE then Exit;

 {Check for NTFS}
 if PNtfsBootSector(ABootSector).BPB.BytesPerSector = 0 then Exit;
 if (PNtfsBootSector(ABootSector).BPB.BytesPerSector mod MIN_SECTOR_SIZE) <> 0 then Exit;
 if (PNtfsBootSector(ABootSector).BPB.SectorsPerCluster = 0) then Exit;
 if (PNtfsBootSector(ABootSector).BPB.SectorsPerCluster <> 1) and ((PNtfsBootSector(ABootSector).BPB.SectorsPerCluster mod 2) <> 0) then Exit;
 if PNtfsBootSector(ABootSector).BPB.TotalSectors = 0 then Exit;
 {if PNtfsBootSector(ABootSector).BPB.TotalSectors > ASectorCount then Exit;}
 if (PNtfsBootSector(ABootSector).BPB.TotalSectors > ASectorCount) and ((PNtfsBootSector(ABootSector).BPB.TotalSectors and ntfsBlockCountMask8) > ASectorCount) and ((PNtfsBootSector(ABootSector).BPB.TotalSectors and ntfsBlockCountMask64) > ASectorCount) then Exit; {Allow for NTFS rounding to block multiple}
 if PNtfsBootSector(ABootSector).BPB.MFTCluster < 1 then Exit;
 if PNtfsBootSector(ABootSector).BPB.MFTMirror < 1 then Exit;
 if PNtfsBootSector(ABootSector).BPB.ClustersPerFile = 0 then Exit;
 if PNtfsBootSector(ABootSector).BPB.ClustersPerIndex = 0 then Exit;

 Result:=True;
end;

{==============================================================================}

function TNTFSRecognizer.GetName:String;
begin
 {}
 Result:='NTFS';
end;

{==============================================================================}

function TNTFSRecognizer.RecognizePartitionId(APartitionId:Byte):Boolean;
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSRecognizer.RecognizePartitionId (PartitionId = ' + IntToStr(APartitionId) + ')');
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
   pidHPFSNTFS:begin
     {NTFS or HPFS Partition}
     if not CheckNTFS then Exit;

     Result:=True;
    end;
  end;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TNTFSRecognizer.RecognizeBootSector(ABootSector:PBootSector;const AStartSector,ASectorCount:Int64):Boolean;
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

function TNTFSRecognizer.RecognizePartition(APartition:TDiskPartition):Boolean;
{Note: Caller must hold the partition lock}
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if APartition = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSRecognizer.RecognizePartition (Partition = ' + APartition.Name + ')');
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
   pidHPFSNTFS:begin
     {NTFS or HPFS Partition}
     if not CheckNTFS then Exit;
     APartition.Recognized:=True;

     Result:=True;
    end;
  end;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TNTFSRecognizer.RecognizeVolume(AVolume:TDiskVolume):Boolean;
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

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSRecognizer.RecognizeVolume - Volume = ' + AVolume.Name);
  {$ENDIF}

  if AVolume.Partition <> nil then
   begin
    {Partitioned Media}
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSRecognizer.RecognizeVolume - Partitioned Media');
    {$ENDIF}

    {Check Partition Id}
    case AVolume.Partition.PartitionId of
     pidHPFSNTFS:begin
       {$IFDEF NTFS_DEBUG}
       if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSRecognizer.RecognizeVolume - Volume Recognized');
       {$ENDIF}

       AVolume.Recognized:=True;

       Result:=True;
      end;
    end;
   end
  else
   begin
    {Non Partitioned Media}
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSRecognizer.RecognizeVolume - Non Partitioned Media');
    {$ENDIF}

    {Check Device Type}
    case AVolume.Device.MediaType of
     mtFLOPPY,mtREMOVABLE,mtOTHER:begin  {No NTFS on CDROM/DVD}
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

          {$IFDEF NTFS_DEBUG}
          if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSRecognizer.RecognizeVolume - Volume Recognized');
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
         {$IFDEF NTFS_DEBUG}
         if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSRecognizer.RecognizeVolume - Volume Recognized (Default)');
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

function TNTFSRecognizer.MountVolume(AVolume:TDiskVolume;ADrive:TDiskDrive):Boolean;
{Note: Caller must hold the volume writer lock}
var
 FileSystem:TNTFSFileSystem;
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if AVolume = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSRecognizer.MountVolume (Volume = ' + AVolume.Name + ')');
  {$ENDIF}

  {Check Recognized}
  if not RecognizeVolume(AVolume) then Exit;

  {Create FileSystem}
  FileSystem:=TNTFSFileSystem.Create(FDriver,AVolume,ADrive);
  FileSystem.ResetLog:=FResetLog;
  FileSystem.FixedZone:=FFixedZone;
  FileSystem.AltLayout:=FAltLayout;
  FileSystem.Lenient:=FLenient;
  FileSystem.Defensive:=FDefensive;
  FileSystem.Aggressive:=FAggressive;
  FileSystem.NoShortNames:=FNoShortNames;
  FileSystem.NullSecurity:=FNullSecurity;
  FileSystem.DefaultSecurity:=FDefaultSecurity;
  FileSystem.FileSystemInit;
  FileSystem.MountFileSystem;

  Result:=True;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}
{==============================================================================}
{TNTFSPartitioner}
function TNTFSPartitioner.CheckLogical(ADevice:TDiskDevice;AParent:TDiskPartition;APartitionId:Byte):Boolean;
{Note: Caller must hold the device and parent lock}
begin
 {}
 Result:=False;

 if ADevice = nil then Exit;

 {Check Type}
 case APartitionId of
  pidHPFSNTFS:begin
    if AParent = nil then Exit;

    Result:=True;
   end;
 end;
end;

{==============================================================================}

function TNTFSPartitioner.CheckExtended(ADevice:TDiskDevice;AParent:TDiskPartition;APartitionId:Byte):Boolean;
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

function TNTFSPartitioner.GetPartitionId(ADevice:TDiskDevice;AParent:TDiskPartition;AStart,ACount:LongWord;APartitionId:Byte):Byte;
{Note: Start is the absolute start sector on the device}
{Note: Caller must hold the device and parent lock}
var
 LBA:Boolean;
begin
 {}
 Result:=pidUnused;

 if ACount = 0 then Exit;
 if ADevice = nil then Exit;

 {Get LBA}
 LBA:=(ADevice.LBA and ((ADevice.PhysicalCylinders > 1024) or (ADevice.PhysicalCylinders = 0)));

 {Check Type}
 case APartitionId of
  pidExtended,pidExtLBA:begin
    Result:=APartitionId;

    {DOS only allows standard type for second level Extended}
    if (LBA) and (AParent = nil) then Result:=pidExtLBA;
   end;
  pidHPFSNTFS:begin
    Result:=APartitionId;
   end;
 end;
end;

{==============================================================================}

function TNTFSPartitioner.InitPartition(ADevice:TDiskDevice;AParent:TDiskPartition;AStart,ACount:LongWord;APartitionId:Byte):Boolean;
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
  pidHPFSNTFS:begin
    {Initialize Boot Sectors}
    Result:=FillSectors(ADevice,nil,AStart,16,FInitChar);
   end;
 end;
end;

{==============================================================================}

function TNTFSPartitioner.AcceptPartition(ADevice:TDiskDevice;APartition,AParent:TDiskPartition;APartitionId:Byte):Boolean;
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
     pidHPFSNTFS:begin
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
       pidFAT12,pidFAT16,pidFAT16HUGE,pidFAT32,pidFAT32LBA,pidFAT16LBA:begin
         {Check New Type}
         case APartitionId of
          pidHPFSNTFS:begin
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
       pidHPFSNTFS:begin
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
{TNTFSFormatter}
function TNTFSFormatter.CheckDevice(AVolume:TDiskVolume;ADrive:TDiskDrive;AFloppyType:TFloppyType):Boolean;
{Checks Device and Floppy types are suitable for formatting}
{Note: Caller must hold the volume lock}
begin
 {}
 Result:=False;

 if AVolume = nil then Exit;
 if AVolume.Device = nil then Exit;

 case AVolume.Device.MediaType of
  mtFIXED,mtREMOVABLE:begin {NTFS was only supported on FIXED media but Microsoft now allow on REMOVABLE for Windows IoT etc}
    if AFloppyType <> ftUNKNOWN then Exit;
    if not AVolume.Device.Writeable then Exit;

    Result:=True;
   end;
 end;
end;

{==============================================================================}

function TNTFSFormatter.CheckPartition(AVolume:TDiskVolume;ADrive:TDiskDrive;AFileSysType:TFileSysType):Boolean;
{Checks Partition type is suitable for formatting}
{Note: Also check File System type for non partitioned media}
{Note: Caller must hold the volume lock}
begin
 {}
 Result:=False;

 if AVolume = nil then Exit;

 {Check FileSystem}
 case AFileSysType of
  fsNTFS,fsNTFS5,fsNTFS51:begin
    {NTFS File System}
    if AVolume.Device = nil then Exit;
    if AVolume.Partition = nil then Exit; {NTFS only supported on Partitioned media}

    {Partitioned Media}
    case AVolume.Partition.PartitionId of
     pidHPFSNTFS:begin
       {NTFS Partition}
       Result:=True;
      end;
    end;
   end;
 end;
end;

{==============================================================================}

function TNTFSFormatter.GetSectorsPerCluster(AVolume:TDiskVolume;ADrive:TDiskDrive;AFloppyType:TFloppyType;AFileSysType:TFileSysType):LongWord;
{Determine SectorsPerCluster from the tables based on passed parameters}
{Note: Caller must hold the volume lock}
var
 Count:Integer;
begin
 {}
 Result:=0;

 if AVolume = nil then Exit;
 if AVolume.Device = nil then Exit;

 {Check Device}
 case AVolume.Device.MediaType of
  mtFIXED,mtREMOVABLE:begin {NTFS was only supported on FIXED media but Microsoft now allow on REMOVABLE for Windows IoT etc}
    if AFloppyType <> ftUNKNOWN then Exit;

    {Check NTFS}
    if AFileSysType = fsNTFS then
     begin
      for Count:=0 to ntfs12MaxSectorCount do
       begin
        if AVolume.SectorCount <= ntfs12SectorCounts[Count].SectorCount then
         begin
          if ntfs12SectorCounts[Count].SectorsPerCluster > 0 then
           begin
            Result:=ntfs12SectorCounts[Count].SectorsPerCluster;
           end;

          Break;
         end;
       end;
     end;

    {Check NTFS5}
    if (AFileSysType = fsNTFS5) or (AFileSysType = fsNTFS51) then
     begin
      for Count:=0 to ntfs30MaxSectorCount do
       begin
        if AVolume.SectorCount <= ntfs30SectorCounts[Count].SectorCount then
         begin
          if ntfs30SectorCounts[Count].SectorsPerCluster > 0 then
           begin
            Result:=ntfs30SectorCounts[Count].SectorsPerCluster;
           end;

          Break;
         end;
       end;
     end;
   end;
 end;
end;

{==============================================================================}

function TNTFSFormatter.AcceptVolume(AVolume:TDiskVolume;AFloppyType:TFloppyType;AFileSysType:TFileSysType):Boolean;
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

function TNTFSFormatter.FormatVolume(AVolume:TDiskVolume;AFloppyType:TFloppyType;AFileSysType:TFileSysType):Boolean;
{Note: Caller must hold the volume writer lock}
var
 Drive:TDiskDrive;
 FileSystem:TNTFSFileSystem;
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

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFormatter.FormatVolume - Volume = ' + AVolume.Name);
  {$ENDIF}

  {Check Accepted}
  if not AcceptVolume(AVolume,AFloppyType,AFileSysType) then Exit;

  {Get Drive}
  Drive:=FDriver.GetDriveByVolume(AVolume,True,FILESYS_LOCK_WRITE);
  try
   {Get FileSystem}
   FileSystem:=TNTFSFileSystem(AVolume.FileSystem);
   if FileSystem = nil then
    begin
     {Create FileSystem}
     FileSystem:=TNTFSFileSystem.Create(FDriver,AVolume,Drive);
     FileSystem.ResetLog:=TNTFSRecognizer(FRecognizer).ResetLog;
     FileSystem.FixedZone:=TNTFSRecognizer(FRecognizer).FixedZone;
     FileSystem.AltLayout:=TNTFSRecognizer(FRecognizer).AltLayout;
     FileSystem.Lenient:=TNTFSRecognizer(FRecognizer).Lenient;
     FileSystem.Defensive:=TNTFSRecognizer(FRecognizer).Defensive;
     FileSystem.Aggressive:=TNTFSRecognizer(FRecognizer).Aggressive;
     FileSystem.NoShortNames:=TNTFSRecognizer(FRecognizer).NoShortNames;
     FileSystem.NullSecurity:=TNTFSRecognizer(FRecognizer).NullSecurity;
     FileSystem.DefaultSecurity:=TNTFSRecognizer(FRecognizer).DefaultSecurity;
     FileSystem.FileSystemInit;
    end
   else
    begin
     if (FileSystem.FileSysType <> fsNTFS) and (FileSystem.FileSysType <> fsNTFS5) and (FileSystem.FileSysType <> fsNTFS51) then
      begin
       {Destroy FileSystem}
       FileSystem.DismountFileSystem;
       FileSystem.Free;

       {Create FileSystem}
       FileSystem:=TNTFSFileSystem.Create(FDriver,AVolume,Drive);
       FileSystem.ResetLog:=TNTFSRecognizer(FRecognizer).ResetLog;
       FileSystem.FixedZone:=TNTFSRecognizer(FRecognizer).FixedZone;
       FileSystem.AltLayout:=TNTFSRecognizer(FRecognizer).AltLayout;
       FileSystem.Lenient:=TNTFSRecognizer(FRecognizer).Lenient;
       FileSystem.Defensive:=TNTFSRecognizer(FRecognizer).Defensive;
       FileSystem.Aggressive:=TNTFSRecognizer(FRecognizer).Aggressive;
       FileSystem.NoShortNames:=TNTFSRecognizer(FRecognizer).NoShortNames;
       FileSystem.NullSecurity:=TNTFSRecognizer(FRecognizer).NullSecurity;
       FileSystem.DefaultSecurity:=TNTFSRecognizer(FRecognizer).DefaultSecurity;
       FileSystem.FileSystemInit;
      end
     else
      begin
       {Dismount FileSystem}
       FileSystem.DismountFileSystem;
       FileSystem.FileSystemInit;
      end;
    end;

   {Get Sectors Per Cluster}
   SectorsPerCluster:=GetSectorsPerCluster(AVolume,nil,AFloppyType,AFileSysType);
   if SectorsPerCluster = 0 then Exit;

   {Initialize FileSystem}
   if not FileSystem.InitializeFileSystem(SectorsPerCluster,AFileSysType) then
    begin
     FileSystem.DismountFileSystem;
     FileSystem.FileSystemInit;
     Exit;
    end;

   Result:=True;
  finally
   {Unlock Drive}
   if Drive <> nil then Drive.WriterUnlock;
  end;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}
{==============================================================================}
{TNTFSDefragger}

{==============================================================================}
{==============================================================================}
{TNTFSRepairer}

{==============================================================================}
{==============================================================================}
{TNTFSFileSystem}
constructor TNTFSFileSystem.Create(ADriver:TFileSysDriver;AVolume:TDiskVolume;ADrive:TDiskDrive);
begin
 {}
 inherited Create(ADriver,AVolume,ADrive);
 FNTFSType:=ntNONE;
 FVolumeVersion:=ntfsNTFS12;
 FVolumeFlags:=ntfsVolumeFlagNone;

 FResetLog:=False;
 FFixedZone:=False;
 FAltLayout:=False;
 FLenient:=False;
 FDefensive:=False;
 FAggressive:=False;
 FNoShortNames:=False;
 FNullSecurity:=False;
 FDefaultSecurity:=False;

 FReadOnly:=False;
 FLongNames:=True;
 FDataStreams:=True;
 FReparsePoints:=True;
 FCaseSensitive:=False; {Should be True according to XP}
 FCasePreserved:=True;
 FUnicodeNames:=True;
 FPersistentAcls:=True;
 FFileCompression:=True;
 FVolumeQuotas:=True;
 FSparseFiles:=True;
 FRemoteStorage:=False;
 FVolumeCompressed:=False;
 FObjectIds:=True;
 FEncryption:=True;

 FBootCatalog:=False;
 FVirtualVolume:=False;
 FFolderEncryption:=True;
 FFolderCompression:=True;

 FSectorsPerCluster:=0;

 FMftStartCluster:=ntfsUnknownCluster;
 FMftMirrorCluster:=ntfsUnknownCluster;

 FMftZoneStart:=ntfsUnknownCluster;
 FMftZoneCluster:=ntfsUnknownCluster;
 FMftMirrorCount:=0;
 FMftZoneReservation:=0;

 FFileRecordSize:=0;
 FIndexRecordSize:=0;

 FClustersPerFile:=0;
 FClustersPerIndex:=0;

 FFilesPerCluster:=0;
 FIndexsPerCluster:=0;

 FEntriesPerBlock:=0;
 FClustersPerBlock:=0;
 FTotalBlockCount:=0;

 FBlockShiftCount:=0;
 FSectorShiftCount:=0;
 FClusterShiftCount:=0;

 FFileRecordShiftCount:=0;
 FFileRecordOffsetMask:=0;

 FIndexCounterShift:=0;
 FIndexCounterOffset:=0;

 FIndexRecordShiftCount:=0;
 FIndexRecordOffsetMask:=0;

 FTotalClusterCount:=0;

 FLastMftCluster:=ntfsUnknownCluster;
 FLastFreeCluster:=ntfsUnknownCluster;
 FFreeClusterCount:=ntfsUnknownCluster;

 FTotalFileRecordCount:=0;

 FLastFreeFileRecord:=ntfsUnknownRecordNumber;
 FFreeFileRecordCount:=ntfsUnknownRecordNumber;
 FReservedFileRecordCount:=ntfsUnknownRecordNumber;

 FClusterSize:=0;

 FMft:=nil;
 FMftMirr:=nil;
 FLogFile:=nil;
 FVolInfo:=nil;
 FAttrDef:=nil;
 FBitmap:=nil;
 FBoot:=nil;
 FBadClus:=nil;
 FSecure:=nil;
 FUpCase:=nil;
 FExtend:=nil;
 FObjId:=nil;
 FQuota:=nil;
 FReparse:=nil;
 FUsnJrnl:=nil;

 FMaster:=nil;
 FMirror:=nil;

 FUpCases:=TNTFSUpCase.Create;
 FAttrDefs:=TNTFSAttrDefs.Create;
 FSecuritys:=TNTFSSecurityItems.Create;

 FRecords:=TNTFSRecordIndex.Create;

 FFileBuffer:=nil;
 FFileLock:=MutexCreate;

 FIndexBuffer:=nil;

 FReadBuffer:=nil;
 FReadLock:=MutexCreate;

 FWriteBuffer:=nil;
 FWriteLock:=MutexCreate;

 FClusterBuffer:=nil;
 FClusterLock:=MutexCreate;

 FCompressionBuffer:=nil;
 FCompressionLock:=MutexCreate;

 FDecompressionBuffer:=nil;
 FDecompressionLock:=MutexCreate;
end;

{==============================================================================}

destructor TNTFSFileSystem.Destroy;
begin
 {}
 WriterLock;
 try
  if FDecompressionBuffer <> nil then FreeMem(FDecompressionBuffer);
  FDecompressionBuffer:=nil;
  MutexDestroy(FDecompressionLock);

  if FCompressionBuffer <> nil then FreeMem(FCompressionBuffer);
  FCompressionBuffer:=nil;
  MutexDestroy(FCompressionLock);

  if FClusterBuffer <> nil then FreeMem(FClusterBuffer);
  FClusterBuffer:=nil;
  MutexDestroy(FClusterLock);

  if FWriteBuffer <> nil then FreeMem(FWriteBuffer);
  FWriteBuffer:=nil;
  MutexDestroy(FWriteLock);

  if FReadBuffer <> nil then FreeMem(FReadBuffer);
  FReadBuffer:=nil;
  MutexDestroy(FReadLock);

  if FIndexBuffer <> nil then FreeMem(FIndexBuffer);
  FIndexBuffer:=nil;

  if FFileBuffer <> nil then FreeMem(FFileBuffer);
  FFileBuffer:=nil;
  MutexDestroy(FFileLock);

  FRecords.Free;

  FSecuritys.Free;
  FAttrDefs.Free;
  FUpCases.Free;

  FMirror:=nil;
  FMaster:=nil;

  FUsnJrnl:=nil;
  FReparse:=nil;
  FQuota:=nil;
  FObjId:=nil;
  FExtend:=nil;
  FUpCase:=nil;
  FSecure:=nil;
  FBadClus:=nil;
  FBoot:=nil;
  FBitmap:=nil;
  FAttrDef:=nil;
  FVolInfo:=nil;
  FLogFile:=nil;
  FMftMirr:=nil;
  FMft:=nil;
 finally
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  inherited Destroy;
 end;
end;

{==============================================================================}

function TNTFSFileSystem.FileLock:Boolean;
begin
 {}
 Result:=(MutexLock(FFileLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSFileSystem.FileUnlock:Boolean;
begin
 {}
 Result:=(MutexUnlock(FFileLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSFileSystem.ReadLock:Boolean;
begin
 {}
 Result:=(MutexLock(FReadLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSFileSystem.ReadUnlock:Boolean;
begin
 {}
 Result:=(MutexUnlock(FReadLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSFileSystem.WriteLock:Boolean;
begin
 {}
 Result:=(MutexLock(FWriteLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSFileSystem.WriteUnlock:Boolean;
begin
 {}
 Result:=(MutexUnlock(FWriteLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSFileSystem.ClusterLock:Boolean;
begin
 {}
 Result:=(MutexLock(FClusterLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSFileSystem.ClusterUnlock:Boolean;
begin
 {}
 Result:=(MutexUnlock(FClusterLock) = ERROR_SUCCESS);
end;

{=============================================================================}

function TNTFSFileSystem.AllocCompressionBuffer(ACompressionUnit:Word;AForce:Boolean):Pointer;
{Return the global compression buffer or allocate a new one if unit size is not default}
{Force means allocate a new buffer even if the global buffer is the correct size}
begin
 {}
 Result:=nil;

 if FClusterSize = 0 then Exit;
 if ACompressionUnit = 0 then Exit;

 if ACompressionUnit = ntfsCompressionUnitSize then
  begin
   if AForce then
    begin
     Result:=AllocMem((FClusterSize shl ACompressionUnit) + FClusterSize); {One extra cluster for overrun}
    end
   else
    begin
     if MutexLock(FCompressionLock) <> ERROR_SUCCESS then Exit;

     if FCompressionBuffer = nil then FCompressionBuffer:=GetMem((FClusterSize shl ACompressionUnit) + FClusterSize); {One extra cluster for overrun}
     Result:=FCompressionBuffer;
    end;
  end
 else
  begin
   Result:=AllocMem((FClusterSize shl ACompressionUnit) + FClusterSize); {One extra cluster for overrun}
  end;
end;

{=============================================================================}

function TNTFSFileSystem.ReleaseCompressionBuffer(ACompressionUnit:Word;ABuffer:Pointer):Boolean;
{Free an allocated compression buffer that was not the global buffer}
begin
 {}
 Result:=False;

 if ABuffer = nil then Exit;
 if ACompressionUnit = 0 then Exit;

 if ABuffer = FCompressionBuffer then
  begin
   if MutexUnlock(FCompressionLock) <> ERROR_SUCCESS then Exit;
  end
 else
  begin
   FreeMem(ABuffer);
  end;

 Result:=True;
end;

{=============================================================================}

function TNTFSFileSystem.AllocDecompressionBuffer(ACompressionUnit:Word;AForce:Boolean):Pointer;
{Return the global decompression buffer or allocate a new one if unit size is not default}
{Force means allocate a new buffer even if the global buffer is the correct size}
begin
 {}
 Result:=nil;

 if FClusterSize = 0 then Exit;
 if ACompressionUnit = 0 then Exit;

 if ACompressionUnit = ntfsCompressionUnitSize then
  begin
   if AForce then
    begin
     Result:=AllocMem((FClusterSize shl ACompressionUnit) + FClusterSize); {One extra cluster for overrun}
    end
   else
    begin
     if MutexLock(FDecompressionLock) <> ERROR_SUCCESS then Exit;

     if FDecompressionBuffer = nil then FDecompressionBuffer:=GetMem((FClusterSize shl ACompressionUnit) + FClusterSize); {One extra cluster for overrun}
     Result:=FDecompressionBuffer;
    end;
  end
 else
  begin
   Result:=AllocMem((FClusterSize shl ACompressionUnit) + FClusterSize); {One extra cluster for overrun}
  end;
end;

{=============================================================================}

function TNTFSFileSystem.ReleaseDecompressionBuffer(ACompressionUnit:Word;ABuffer:Pointer):Boolean;
{Free an allocated decompression buffer that was not the global buffer}
begin
 {}
 Result:=False;

 if ABuffer = nil then Exit;
 if ACompressionUnit = 0 then Exit;

 if ABuffer = FDecompressionBuffer then
  begin
   if MutexUnlock(FDecompressionLock) <> ERROR_SUCCESS then Exit;
  end
 else
  begin
   FreeMem(ABuffer);
  end;

 Result:=True;
end;

{=============================================================================}

function TNTFSFileSystem.GetDirty:Boolean;
{Note: Bit is on for Dirty and off for Clean}
begin
 {}
 Result:=GetVolumeFlag(ntfsVolumeFlagDirty);
end;

{=============================================================================}

procedure TNTFSFileSystem.SetDirty(AValue:Boolean);
{Note: Bit is on for Dirty and off for Clean}
begin
 {}
 SetVolumeFlag(ntfsVolumeFlagDirty,AValue);
end;

{=============================================================================}

function TNTFSFileSystem.GetVolumeFlag(AFlag:LongWord):Boolean;
begin
 {}
 Result:=False;

 if not AcquireLock then Exit;
 try
  if FNTFSType = ntNONE then Exit;

  {Get Flag}
  Result:=((FVolumeFlags and AFlag) = AFlag);
 finally
  ReleaseLock;
 end;
end;

{=============================================================================}

procedure TNTFSFileSystem.SetVolumeFlag(AFlag:LongWord;AValue:Boolean);
begin
 {}
 if not AcquireLock then Exit;
 try
  if FNTFSType = ntNONE then Exit;

  {Set Flag}
  if AValue then
   begin
    FVolumeFlags:=(FVolumeFlags or AFlag);       {Turn On}
   end
  else
   begin
    FVolumeFlags:=(FVolumeFlags and not(AFlag)); {Turn Off}
   end;
 finally
  ReleaseLock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.LoadVolumeFlags:LongWord;
{Load the volume flags from $VOLUME_INFORMATION}
var
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=ntfsVolumeFlagNone;

 if not FRecords.ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FVolInfo = nil then Exit;
  if FVolInfo.Origin = nil then Exit;

  {Get Origin}
  Origin:=FVolInfo.Origin.Origin;
  if Origin = nil then Exit;

  {Get Attribute}
  Attribute:=Origin.GetAttribute(ntfsAttrTypeVolumeInformation,ntfsBlankName,ntfsInstanceFirst);
  if Attribute = nil then Exit;

  {Get Volume Flags}
  Result:=TNTFSVolumeInformationAttribute(Attribute).VolumeFlags;
 finally
  FRecords.ReaderUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.SetVolumeFlags(AFlags:LongWord):Boolean;
{Set the volume flags in $VOLUME_INFORMATION}
var
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FRecords.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FVolInfo = nil then Exit;
  if FVolInfo.Origin = nil then Exit;

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get Origin}
  Origin:=FVolInfo.Origin.Origin;
  if Origin = nil then Exit;

  {Get Attribute}
  Attribute:=Origin.GetAttribute(ntfsAttrTypeVolumeInformation,ntfsBlankName,ntfsInstanceFirst);
  if Attribute = nil then Exit;

  {Set Volume Flags}
  TNTFSVolumeInformationAttribute(Attribute).VolumeFlags:=AFlags;
  FVolumeFlags:=AFlags;

  {Set Records}
  Result:=SetRecords(Origin);
 finally
  FRecords.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.GetVolumeType:TNTFSType;
{Get the volume version from $VOLUME_INFORMATION}
var
 VCN:Int64;
 Version:Byte;
 Offset:LongWord;
 RecordNumber:Int64;

 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;

 FileRecord:PNTFS12FileRecord;
 VolumeInformation:PNTFSVolumeInformation;
 UpdateSequenceRecord:PNTFSUpdateSequenceRecord;
 ResidentAttributeHeader:PNTFSResidentAttributeHeader;
begin
 {}
 Result:=FNTFSType;

 if Result = ntNONE then
  begin
   Version:=0;
   {Check File}
   if FVolInfo <> nil then
    begin
     {$IFDEF NTFS_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetVolumeType reading from VolInfo');
     {$ENDIF}

     if not FRecords.ReaderLock then Exit;
     try
      if FVolInfo.Origin = nil then Exit;

      {Get Origin}
      Origin:=FVolInfo.Origin.Origin;
      if Origin = nil then Exit;

      {Get Attribute}
      Attribute:=Origin.GetAttribute(ntfsAttrTypeVolumeInformation,ntfsBlankName,ntfsInstanceFirst);
      if Attribute = nil then Exit;

      Version:=(TNTFSVolumeInformationAttribute(Attribute).MajorVersion shl 4) or (TNTFSVolumeInformationAttribute(Attribute).MinorVersion);
     finally
      FRecords.ReaderUnlock;
     end;
    end
   else
    begin
     {$IFDEF NTFS_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetVolumeType reading from file record');
     {$ENDIF}

     if not FileLock then Exit;
     try
      if FFileBuffer = nil then Exit;

      {Get VCN}
      RecordNumber:=ntfsFileTypeVolume;
      if not GetFileRecordVCN(RecordNumber,VCN,Offset) then Exit;

      {Read Run}
      if not ReadFileRecord(RecordNumber,FFileBuffer^,VCN,Max(FClustersPerFile,1)) then Exit;

      {Get Record}
      FileRecord:=PNTFS12FileRecord(PtrUInt(FFileBuffer) + Offset);
      if FileRecord.MagicNumber <> ntfsFileSignature then Exit;

      {Get Update}
      UpdateSequenceRecord:=PNTFSUpdateSequenceRecord(PtrUInt(FFileBuffer) + Offset + FileRecord.UpdateSequenceOffset);
      if not ReadFixup(FFileBuffer,Offset,UpdateSequenceRecord.UpdateSequenceNumber,FileRecord.UpdateSequenceOffset,FileRecord.UpdateSequenceLength,False) then Exit;

      {Get Offset}
      Inc(Offset,FileRecord.AttributeOffset);

      {Get Header}
      ResidentAttributeHeader:=PNTFSResidentAttributeHeader(PtrUInt(FFileBuffer) + Offset);
      while ResidentAttributeHeader.AttributeType <> ntfsAttrTypeEnd do
       begin
        {Check Resident}
        if ResidentAttributeHeader.NonResident = ntfsAttributeResident then
         begin
          {Check Type}
          if ResidentAttributeHeader.AttributeType = ntfsAttrTypeVolumeInformation then
           begin

            {Get Attribute}
            VolumeInformation:=PNTFSVolumeInformation(PtrUInt(FFileBuffer) + Offset + ResidentAttributeHeader.DataOffset);
            Version:=(VolumeInformation.MajorVersion shl 4) or (VolumeInformation.MinorVersion);

            Break;
           end;
         end;

        {Get Offset}
        Inc(Offset,ResidentAttributeHeader.AttributeSize);

        {Get Header}
        ResidentAttributeHeader:=PNTFSResidentAttributeHeader(PtrUInt(FFileBuffer) + Offset);
       end;
     finally
      FileUnlock;
     end;
    end;

   {Check Version}
   case Version of
    $12:FNTFSType:=ntNTFS12; {NT4}
    $30:FNTFSType:=ntNTFS30; {Windows2000}
    $31:FNTFSType:=ntNTFS31; {WindowsXP/Windows2003/WindowsVista/Windows2008/Windows7}
   end;

   Result:=FNTFSType;

   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetVolumeType - Version = ' + IntToHex(Version,2));
   {$ENDIF}
  end;
end;

{=============================================================================}

function TNTFSFileSystem.GetVolumeVersion:Word;
{Get the volume version from $VOLUME_INFORMATION}
begin
 {}
 Result:=NTFSTypeToNTFSVersion(GetVolumeType);
end;

{=============================================================================}

function TNTFSFileSystem.FillClusters(const ACluster:Int64;ACount:Word;AValue:Byte):Boolean;
{Fill count clusters with the supplied value}
var
 Count:Word;     //To Do //Int64
 Cluster:Int64;
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
    {Read Cluster} {Dont need to read first}
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

function TNTFSFileSystem.ReadClusters(const ACluster:Int64;ACount:Word;var ABuffer):Boolean;
{Read count Clusters from the Volume using Cache}
{Performs conversion of Cluster to Sector based on Offsets}
{Note: Sector is relative to StartSector of the FileSystem}
var
 Sector:Int64;
 Count:LongWord; //To Do //Int64
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
 Count:=(ACount shl FSectorShiftCount);

 Result:=FDriver.Cache.DeviceRead(FVolume.Device,FStartSector + Sector,Count,ABuffer);
end;

{=============================================================================}

function TNTFSFileSystem.WriteClusters(const ACluster:Int64;ACount:Word;const ABuffer):Boolean;
{Write count Clusters to the Volume using Cache}
{Performs conversion of Cluster to Sector based on Offsets}
{Note: Sector is relative to StartSector of the FileSystem}
var
 Sector:Int64;
 Count:LongWord; //To Do //Int64
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
 Count:=(ACount shl FSectorShiftCount);

 Result:=FDriver.Cache.DeviceWrite(FVolume.Device,FStartSector + Sector,Count,ABuffer);
end;

{=============================================================================}

function TNTFSFileSystem.MarkClusters(const ACluster,ACount:Int64):Boolean;
{Mark count clusters in the bitmap blocks}
{Only called during volume format and initialization}
{Note: Caller must hold the records lock}
var
 VCN:Int64;
 Remain:Int64;
 Cluster:Int64;
 Count:LongWord;
 Length:LongWord;
 Instance:LongWord;

 BlockNo:LongWord;
 Block:TNTFSDiskBlock;
 Origin:TNTFSDiskRecord;
 Current:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FBlocks.WriterLock then Exit;
 try
  if FDriver = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.MarkClusters - Cluster = ' + IntToStr(ACluster) + ' Count = ' + IntToStr(ACount));
  {$ENDIF}

  {Check Count}
  if ACount = 0 then Exit;

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get Bitmap}
  Current:=GetRecordEx(nil,ntfsFileTypeBitmap,False,True);
  if Current = nil then Exit;

  {Get Origin}
  Origin:=Current.Origin;
  if Origin = nil then Exit;

  {Get Attribute}
  Attribute:=Origin.GetAttribute(ntfsAttrTypeData,ntfsBlankName,ntfsInstanceFirst);
  if Attribute = nil then Exit;

  {Get Params}
  Cluster:=ACluster;
  Remain:=ACount;
  while Remain > 0 do
   begin
    {Get Block}
    BlockNo:=(Cluster shr FBlockShiftCount);
    Block:=TNTFSDiskBlock(GetBlockEx(BlockNo,True));
    if Block = nil then Exit;

    {Get Count}
    Count:=Min64(Remain,Block.BlockCount - (Cluster - Block.BlockCluster));
    Length:=Count;

    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.MarkClusters - BlockNo = ' + IntToHex(BlockNo,8) + ' Cluster = ' + IntToStr(Cluster) + ' Count = ' + IntToStr(Count));
    {$ENDIF}

    {Alloc Block}
    if not AllocBlock(Block,Cluster,Count) then Exit;

    {Check Count}
    if Count < Length then Exit;  {Exit to fail the operation} {Allocated less than requested}

    {Write Run} {No Update}
    VCN:=BlockNo;
    Instance:=ntfsInstanceFirst;
    if WriteRun(Origin,Attribute,Block.BlockBuffer^,VCN,FClustersPerBlock,Instance,False,False) <> FClustersPerBlock then Exit;

    {Update Params}
    Inc(Cluster,Count);
    Dec(Remain,Count);
    if FFreeClusterCount <> ntfsUnknownCluster then Dec(FFreeClusterCount,Count);

    {Check Count}
    if Cluster < (Block.BlockCluster + Block.BlockCount) then Break; {Break to allow success} {Next Cluster is within the same Block}

    {Check Remain}
    if Remain = 0 then Break;
   end;

  Result:=(Remain = 0);
 finally
  FBlocks.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.AllocClusters(var ACluster,ACount:Int64;AMft:Boolean):Boolean;
{Allocate count clusters on the disk and mark them in the bitmap blocks}
{Note: Caller must hold the records lock}
var
 VCN:Int64;
 Start:Int64;
 Remain:Int64;
 Cluster:Int64;
 Count:LongWord;
 Length:LongWord;
 Instance:LongWord;

 BlockNo:LongWord;
 Block:TNTFSDiskBlock;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FBlocks.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FBitmap = nil then Exit;
  if FBitmap.Origin = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AllocClusters - Cluster = ' + IntToStr(ACluster) + ' Count = ' + IntToStr(ACount) + ' MFT = ' + BoolToStr(AMft));
  {$ENDIF}

  {Check Count}
  if ACount = 0 then Exit;

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get Origin}
  Origin:=FBitmap.Origin.Origin;
  if Origin = nil then Exit;

  {Get Attribute}
  Attribute:=Origin.GetAttribute(ntfsAttrTypeData,ntfsBlankName,ntfsInstanceFirst);
  if Attribute = nil then Exit;

  {Get Start}
  Start:=GetNextFreeCluster(AMft);
  if Start = ntfsUnknownCluster then
   begin
    {Shrink Mft Zone}
    if not ShrinkMftZoneReservation then Exit;
    Start:=GetNextFreeCluster(AMft);
   end;
  if Start = ntfsUnknownCluster then Exit;

  {Get Params}
  Cluster:=Start;
  Remain:=ACount;
  while Remain > 0 do
   begin
    {Get Block}
    BlockNo:=(Cluster shr FBlockShiftCount);
    Block:=TNTFSDiskBlock(GetBlockEx(BlockNo,True));
    if Block = nil then Exit;

    {Get Count}
    Count:=Min64(Remain,Block.BlockCount - (Cluster - Block.BlockCluster));
    Length:=Count;

    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AllocClusters - BlockNo = ' + IntToHex(BlockNo,8) + ' Cluster = ' + IntToStr(Cluster) + ' Count = ' + IntToStr(Count));
    {$ENDIF}

    {Alloc Block}
    if not AllocBlock(Block,Cluster,Count) then Exit;

    {Write Run} {Allow Update}
    VCN:=BlockNo;
    Instance:=ntfsInstanceFirst;
    if WriteRun(Origin,Attribute,Block.BlockBuffer^,VCN,FClustersPerBlock,Instance,False,True) <> FClustersPerBlock then Exit;

    {Update Params}
    Inc(Cluster,Count);
    Dec(Remain,Count);
    if FFreeClusterCount <> ntfsUnknownCluster then Dec(FFreeClusterCount,Count);

    {Check Count}
    if Count < Length then Break;                                    {Break to allow success} {Allocated less than requested}
    if Cluster < (Block.BlockCluster + Block.BlockCount) then Break; {Break to allow success} {Next Cluster is within the same Block}

    {Check Remain}
    if Remain = 0 then Break;

    {Get Cluster}
    Cluster:=GetNextFreeCluster(AMft);
    if Cluster = ntfsUnknownCluster then
     begin
      {Shrink Mft Zone}
      if ShrinkMftZoneReservation then Cluster:=GetNextFreeCluster(AMft);
     end;
    if Cluster <> (Block.BlockCluster + Block.BlockCount) then Break; {Break to allow success} {Next Cluster is not contiguous with previous}
   end;

  {Get Results}
  ACluster:=Start;
  ACount:=(ACount - Remain);

  Result:=True;
 finally
  FBlocks.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.ReleaseClusters(const ACluster,ACount:Int64):Boolean;
{Release count clusters on the disk and free them in the bitmap blocks}
{Note: Caller must hold the records lock}
var
 VCN:Int64;
 Remain:Int64;
 Cluster:Int64;
 Count:LongWord;
 Instance:LongWord;

 BlockNo:LongWord;
 Block:TNTFSDiskBlock;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FBlocks.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FBitmap = nil then Exit;
  if FBitmap.Origin = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.ReleaseClusters - Cluster = ' + IntToStr(ACluster) + ' Count = ' + IntToStr(ACount));
  {$ENDIF}

  {Check Count}
  if ACount = 0 then Exit;

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Cluster}
  if ACluster = ntfsUnknownCluster then Exit;

  {Get Origin}
  Origin:=FBitmap.Origin.Origin;
  if Origin = nil then Exit;

  {Get Attribute}
  Attribute:=Origin.GetAttribute(ntfsAttrTypeData,ntfsBlankName,ntfsInstanceFirst);
  if Attribute = nil then Exit;

  {Get Params}
  Cluster:=ACluster;
  Remain:=ACount;
  while Remain > 0 do
   begin
    {Get Block}
    BlockNo:=(Cluster shr FBlockShiftCount);
    Block:=TNTFSDiskBlock(GetBlockEx(BlockNo,True));
    if Block = nil then Exit;

    {Get Count}
    Count:=Min64(Remain,Block.BlockCount - (Cluster - Block.BlockCluster));

    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.ReleaseClusters - BlockNo = ' + IntToHex(BlockNo,8) + ' Cluster = ' + IntToStr(Cluster) + ' Count = ' + IntToStr(Count));
    {$ENDIF}

    {Release Block}
    if not ReleaseBlock(Block,Cluster,Count) then Exit;

    {Write Run} {Allow Update}
    VCN:=BlockNo;
    Instance:=ntfsInstanceFirst;
    if WriteRun(Origin,Attribute,Block.BlockBuffer^,VCN,FClustersPerBlock,Instance,False,True) <> FClustersPerBlock then Exit;

    {Update Params}
    Inc(Cluster,Count);
    Dec(Remain,Count);
    if FFreeClusterCount <> ntfsUnknownCluster then Inc(FFreeClusterCount,Count);
   end;

  Result:=True;
 finally
  FBlocks.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.GetNextFreeCluster(AMft:Boolean):Int64;
{Get the next free cluster on the disk using the bitmap blocks}
{Caller must handle shrinking the Mft Zone on failure}
//To Do //Add support for passing a starting cluster for the search (last cluster of existing data stream)
var
 Next:Int64;
 Start:Int64;
 Origin:Int64;
 Cluster:Int64;
 Wrapped:Boolean;
 BlockNo:LongWord;
 Block:TNTFSDiskBlock;
begin
 {}
 Result:=ntfsUnknownCluster;

 if not FBlocks.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FEntriesPerBlock = 0 then Exit;
  if FTotalClusterCount = 0 then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetNextFreeCluster - MFT = ' + BoolToStr(AMft));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Load Blocks}
  {if not LoadBlocks then Exit;} {Will be loaded on demand}

  {Get Params}
  if AMft then
   begin
    Cluster:=FMftStartCluster;
    Wrapped:=False;
    if FLastMftCluster <> ntfsUnknownCluster then Inc(FLastMftCluster); {Increment on each Get Next as NTFS may reject the last returned value}
    if FLastMftCluster >= FTotalClusterCount then FLastMftCluster:=0;   {Check for Wraparound of Last Free}
    if FLastMftCluster <> ntfsUnknownCluster then Cluster:=FLastMftCluster;
   end
  else
   begin
    Cluster:=FMftZoneCluster; {0;} {Always start looking after the MFT until full}
    Wrapped:=False;
    if FLastFreeCluster <> ntfsUnknownCluster then Inc(FLastFreeCluster); {Increment on each Get Next as NTFS may reject the last returned value}
    if FLastFreeCluster >= FTotalClusterCount then FLastFreeCluster:=0;   {Check for Wraparound of Last Free}
    if FLastFreeCluster <> ntfsUnknownCluster then Cluster:=FLastFreeCluster;
   end;
  //Remove
  //Cluster:=FMftZoneCluster; {0;} {Always start looking after the MFT until full}
  //Wrapped:=False;
  //if (FLastFreeCluster <> ntfsUnknownCluster) and not(AMft) then Inc(FLastFreeCluster); {Increment on each Get Next (unless MFT) as NTFS may reject the last returned value}
  //if FLastFreeCluster >= FTotalClusterCount then FLastFreeCluster:=0;   {Check for Wraparound of Last Free}
  //if FLastFreeCluster <> ntfsUnknownCluster then Cluster:=FLastFreeCluster;

  {Check MFT}
  if (AMft) and (FMftZoneReservation > 0) then
   begin
    if FMftZoneStart = ntfsUnknownCluster then
     begin
      {Check if starting Cluster is outside the Mft Zone} {Note: >= to avoid allocating last cluster of MFT zone first}
      if (Cluster < FMftStartCluster) or (Cluster >= FMftZoneCluster) then Cluster:=FMftStartCluster;
     end
    else
     begin
      {Check if starting Cluster is outside the Mft Zone} {Note: >= to avoid allocating last cluster of MFT zone first}
      if (Cluster < FMftZoneStart) or (Cluster >= FMftZoneCluster) then Cluster:=FMftZoneStart;
     end;
    //Remove
    //{Check if starting Cluster is outside the Mft Zone} {Note: >= to avoid allocating last cluster of MFT zone first}
    //if (Cluster < FMftStartCluster) or (Cluster >= FMftZoneCluster) then Cluster:=FMftStartCluster;
   end
  else
   begin
    if FMftZoneReservation > 0 then {MftZoneCluster will be 0 when MftZoneReservation is 0}
     begin
      if FMftZoneStart = ntfsUnknownCluster then
       begin
        {Check if starting Cluster is within the Mft Zone}
        if (Cluster >= FMftStartCluster) and (Cluster <= FMftZoneCluster) then Cluster:=FMftZoneCluster;
       end
      else
       begin
        {Check if starting Cluster is within the Mft Zone}
        if (Cluster >= FMftZoneStart) and (Cluster <= FMftZoneCluster) then Cluster:=FMftZoneCluster;
       end;
      //Remove
      //{Check if starting Cluster is within the Mft Zone}
      //if (Cluster >= FMftStartCluster) and (Cluster <= FMftZoneCluster) then Cluster:=FMftZoneCluster;
     end;
   end;

  Origin:=Cluster;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetNextFreeCluster - Origin = ' + IntToHex(Origin,16));
  {$ENDIF}

  {Check each Block}
  while Cluster < FTotalClusterCount do
   begin
    {Get Block}
    BlockNo:=(Cluster shr FBlockShiftCount);
    Block:=TNTFSDiskBlock(GetBlockEx(BlockNo,True));
    if Block = nil then Exit;

    {Get Start}
    Start:=Block.BlockCluster;
    if Origin = Cluster then Start:=Origin;

    {Get Next Free}
    Next:=GetBlockNextFree(Block,Start);
    if Next <> ntfsUnknownCluster then
     begin
      if AMft then
       begin
        FLastMftCluster:=Next;
        Result:=FLastMftCluster;
       end
      else
       begin
        FLastFreeCluster:=Next;
        Result:=FLastFreeCluster;
       end;
      //Remove
      //FLastFreeCluster:=Next;
      //Result:=FLastFreeCluster;

      {$IFDEF NTFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetNextFreeCluster - Result = ' + IntToHex(Result,16));
      {$ENDIF}

      Exit;
     end;

    {Check for Origin}
    if (Origin > 0) and (Wrapped) and (Start >= Origin) then Exit;

    {Move next Block}
    if Start > Block.BlockCluster then Inc(Cluster,Block.BlockCount - (Start - Block.BlockCluster)) else Inc(Cluster,Block.BlockCount);

    {Check MFT}
    if (AMft) and (FMftZoneReservation > 0) then
     begin
      if FMftZoneStart = ntfsUnknownCluster then
       begin
        {Check if Cluster is outside the Mft Zone} {Allocate a new Zone if it is}

        //To Do

       end
      else
       begin
        {Check if Cluster is outside the Mft Zone} {Allocate a new Zone if it is}

        //To Do

       end;
     end
    else
     begin
      if FMftZoneReservation > 0 then
       begin
        if FMftZoneStart = ntfsUnknownCluster then
         begin
          {Check if Cluster is within the Mft Zone (Skip the Mft Zone if it is)}
          if (Cluster >= FMftStartCluster) and (Cluster <= FMftZoneCluster) then Cluster:=FMftZoneCluster;
         end
        else
         begin
          {Check if Cluster is within the Mft Zone (Skip the Mft Zone if it is)}
          if (Cluster >= FMftZoneStart) and (Cluster <= FMftZoneCluster) then Cluster:=FMftZoneCluster;
         end;
        //Remove
        //{Check if Cluster is within the Mft Zone (Skip the Mft Zone if it is)}
        //if (Cluster >= FMftStartCluster) and (Cluster <= FMftZoneCluster) then Cluster:=FMftZoneCluster;
       end;
     end;
    //Remove
    //{Check MFT}
    //if (AMft = False) and (FMftZoneReservation > 0) then
    // begin
    //  {Check if Cluster is within the Mft Zone (Skip the Mft Zone if it is)}
    //  if (Cluster >= FMftStartCluster) and (Cluster <= FMftZoneCluster) then Cluster:=FMftZoneCluster;
    // end;

    {Check for Wrap}
    if (Origin > 0) and (Cluster >= FTotalClusterCount) then Cluster:=0;
    if (Origin > 0) and (Cluster = 0) then Wrapped:=True;
   end;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetNextFreeCluster - Result = ' + IntToHex(Result,16));
  {$ENDIF}
 finally
  FBlocks.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.GetFreeClusterCount:Int64;
{Get the count of free clusters on the disk using the bitmap blocks}
var
 Count:Int64;
 Cluster:Int64;
 BlockNo:LongWord;
 Block:TNTFSDiskBlock;
begin
 {}
 Result:=ntfsUnknownCluster;

 if FDriver = nil then Exit;
 if FEntriesPerBlock = 0 then Exit;
 if FTotalClusterCount = 0 then Exit;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetFreeClusterCount');
 {$ENDIF}

 {Check ReadOnly}
 if FReadOnly then Exit;

 {Load Blocks}
 {if not LoadBlocks then Exit;} {Will be loaded on demand}

 {Check Free Count}
 if FFreeClusterCount = ntfsUnknownCluster then
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
      Block:=TNTFSDiskBlock(GetBlockEx(BlockNo,True));
      if Block = nil then Exit;

      {Get Free Count}
      Count:=GetBlockFreeCount(Block);
      if Count <> ntfsUnknownCluster then Inc(FFreeClusterCount,Count);

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

function TNTFSFileSystem.AllocRun(AAttribute:TNTFSDiskAttribute;const ACount:Int64;AMft,ASparse:Boolean):Boolean;
{Allocate count clusters to the end of the runs of the supplied attribute}
{Count is the number of clusters to allocate}
{Note: The passed attribute must contain the last VCN}
{Note: Updates LastVCN value of Attribute}
{Note: Caller must hold the records and attributes lock}
var
 Count:Int64;       {Clusters to be added to current run}
 Remain:Int64;      {Clusters remaining to be added}
 Length:Int64;      {Length of allocated clusters}
 Cluster:Int64;     {First cluster of allocated}
 Run:TNTFSDiskRun;  {Current run to be added}
begin
 {}
 Result:=False;

 if not FRecords.RunsWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if AAttribute = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AllocRun - Type = ' + IntToHex(AAttribute.AttributeType,8) + ' Id = ' + IntToHex(AAttribute.AttributeId,4) + ' Count = ' + IntToStr(ACount) + ' MFT = ' + BoolToStr(AMft));
  {$ENDIF}

  {Check Count}
  if ACount = 0 then Exit;

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Resident}
  if AAttribute.NonResident = ntfsAttributeNonResident then
   begin
    {Get Position}
    Remain:=ACount;
    while Remain > 0 do
     begin
      {Get Length}
      Length:=Remain;

      {Check Sparse}
      if ASparse then
       begin
        {Sparse Run}
        {Get Count}
        Count:=Remain;
        if Count > Length then Count:=Length;

        {Add Run}
        Run:=AAttribute.NewRun(ntfsUnknownCluster,Count);
        if Run = nil then Exit;

        {Coalesce Run}
        if not AAttribute.CoalesceRun(nil) then Exit;

        {Update Position}
        Dec(Remain,Count);
       end
      else
       begin
        {Normal Run}
        {Alloc Clusters}
        Cluster:=ntfsUnknownCluster;
        if not AllocClusters(Cluster,Length,AMft) then Exit;

        {Get Count}
        Count:=Remain;
        if Count > Length then Count:=Length;

        {Add Run}
        Run:=AAttribute.NewRun(Cluster,Count);
        if Run = nil then Exit;

        {Coalesce Run}
        if not AAttribute.CoalesceRun(nil) then Exit;

        {Update Position}
        Dec(Remain,Count);
       end;
     end;

    Result:=True;
   end;
 finally
  FRecords.RunsWriterUnlock;
 end;
end;


{=============================================================================}

function TNTFSFileSystem.ReleaseRun(AAttribute:TNTFSDiskAttribute;const ACount:Int64):Boolean;
{Release count clusters from the end of the runs of the supplied attribute}
{Count is the number of clusters to release and must be within the supplied attribute}
{Note: The passed attribute must contain the last VCN}
{Note: Updates LastVCN value of Attribute}
{Note: Caller must hold the records and attributes lock}
var
 Count:Int64;       {Clusters to be released from current run}
 Remain:Int64;      {Clusters remaining to be released}
 StartVCN:Int64;    {Starting VCN of the current run}
 Run:TNTFSDiskRun;  {Current run to be released}
begin
 {}
 Result:=False;

 if not FRecords.RunsWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if AAttribute = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.ReleaseRun - Type = ' + IntToHex(AAttribute.AttributeType,8) + ' Id = ' + IntToHex(AAttribute.AttributeId,4) + ' Count = ' + IntToStr(ACount));
  {$ENDIF}

  {Check Count}
  if ACount = 0 then Exit;

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Resident}
  if AAttribute.NonResident = ntfsAttributeNonResident then
   begin
    {Get Position}
    Remain:=ACount;

    {Get Run}
    Run:=AAttribute.GetRun(AAttribute.LastVCN,StartVCN);
    while Run <> nil do
     begin
      {Get Count}
      Count:=Remain;
      if Count > Run.Length then Count:=Run.Length;

      {Check Last}
      if Run.IsLast then Exit;

      {Check Sparse}
      if Run.IsSparse then
       begin
        {Sparse Run}
        {Check Count}
        if Count < Run.Length then
         begin
          {Split Run}
          if not AAttribute.SplitRun(Run,(Run.Length - Count)) then Exit;

          {Do not update position}
         end
        else
         begin
          {Remove Run}
          if not AAttribute.RemoveRun(Run) then Exit;

          {Update Position}
          Dec(Remain,Count);
          if Remain = 0 then Break;
         end;
       end
      else
       begin
        {Normal Run}
        {Check Count}
        if Count < Run.Length then
         begin
          {Split Run}
          if not AAttribute.SplitRun(Run,(Run.Length - Count)) then Exit;

          {Do not update position}
         end
        else
         begin
          {Release Clusters}
          if not ReleaseClusters(Run.Start,Count) then Exit;

          {Remove Run}
          if not AAttribute.RemoveRun(Run) then Exit;

          {Update Position}
          Dec(Remain,Count);
          if Remain = 0 then Break;
         end;
       end;

      {Get Run}
      Run:=AAttribute.GetRun(AAttribute.LastVCN,StartVCN);
     end;

    Result:=True;
   end;
 finally
  FRecords.RunsWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.GetRunUnit(AAttribute:TNTFSDiskAttribute;const AVCN:Int64;var AUnit,ALength:Int64):Boolean;
{Convert the virtual cluster to a compression unit using the details of the supplied attribute}
{Length is the length from the supplied VCN to the end of the unit containing the VCN}
{Note: The passed attribute must be the first instance}
{Note: Caller must hold the runs lock}
var
 StartVCN:Int64;
begin
 {}
 Result:=False;

 if FDriver = nil then Exit;
 if AAttribute = nil then Exit;

 {Check Compressed}
 if (AAttribute.IsCompressed) and (AAttribute.CompressionUnit <> 0) then
  begin
   {Get Unit}
   AUnit:=(AVCN shr AAttribute.CompressionUnit);

   {Get Start}
   StartVCN:=(AUnit shl AAttribute.CompressionUnit);

   {Get Length}
   ALength:=(1 shl AAttribute.CompressionUnit) - (AVCN - StartVCN);

   Result:=True;
  end;
end;

{=============================================================================}

function TNTFSFileSystem.GetRunCount(AAttribute:TNTFSDiskAttribute;const AVCN:Int64;var ACount:Int64):Boolean;
{Get the count of clusters from the supplied VCN to the end of the supplied attributes runs}
{The returned count may not respresent contiguous clusters from VCN to VCN plus count}
{Note: The passed attribute must contain the requested VCN}
{Note: Caller must hold the runs lock}
var
 StartVCN:Int64;
 Run:TNTFSDiskRun;
begin
 {}
 Result:=False;

 if FDriver = nil then Exit;
 if AAttribute = nil then Exit;

 //To Do //Lock //RemoveAttribute - No Lock (Add ?) / SizeRun - Ok

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetRunCount - Type = ' + IntToHex(AAttribute.AttributeType,8) + ' Attribute = ' + AAttribute.AttributeName + ' VCN = ' + IntToStr(AVCN));
 {$ENDIF}

 {Get Run}
 Run:=AAttribute.GetRun(AVCN,StartVCN);
 if Run = nil then Exit;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetRunCount - Start = ' + IntToStr(Run.Start) + ' Length = ' + IntToStr(Run.Length));
 {$ENDIF}

 if Run.IsLast then Exit;

 {Get Count}
 ACount:=(Run.Length - (AVCN - StartVCN));

 {Get Run}
 Run:=TNTFSDiskRun(Run.Next);
 while Run <> nil do
  begin
   {Get Count}
   Inc(ACount,Run.Length);

   Run:=TNTFSDiskRun(Run.Next);
  end;

 Result:=True;
end;

{=============================================================================}

function TNTFSFileSystem.GetRunLength(AAttribute:TNTFSDiskAttribute;const AVCN:Int64;var ALength:Int64):Boolean;
{Get the length of the run containing the supplied VCN from the VCN to the end of the run}
{The returned length will respresent contiguous clusters from VCN to VCN plus length}
{Note: The passed attribute must contain the requested VCN}
{Note: Caller must hold the runs lock}
var
 Cluster:Int64;
begin
 {}
 Result:=GetRunCluster(AAttribute,AVCN,Cluster,ALength);
end;

{=============================================================================}

function TNTFSFileSystem.GetRunCluster(AAttribute:TNTFSDiskAttribute;const AVCN:Int64;var ACluster,ALength:Int64):Boolean;
{Convert the virtual cluster to an absolute cluster using the runs of the supplied attribute}
{If the VCN is in a sparse run then the return is true but with cluster unknown}
{The returned Length allows ReadRun/WriteRun to perform multi cluster reads and writes}
{Length is the length from the supplied VCN to the end of the run containing the VCN}
{Note: The passed attribute must contain the requested VCN}
{Note: Caller must hold the runs lock}
var
 StartVCN:Int64;
 Run:TNTFSDiskRun;
begin
 {}
 Result:=False;

 if FDriver = nil then Exit;
 if AAttribute = nil then Exit;

 {Get Run}
 Run:=AAttribute.GetRun(AVCN,StartVCN);
 if Run = nil then Exit;

 {Get Cluster}
 if Run.IsLast then
  begin
   {Last Run}
   Exit;
  end
 else if Run.IsSparse then
  begin
   {Sparse Run}
   ALength:=(Run.Length - (AVCN - StartVCN));
   ACluster:=ntfsUnknownCluster;

   Result:=True;
  end
 else
  begin
   {Normal Run}
   ALength:=(Run.Length - (AVCN - StartVCN));
   ACluster:=(Run.Start + (AVCN - StartVCN));

   Result:=True;
  end;
end;

{=============================================================================}

function TNTFSFileSystem.GetUnitVCN(AAttribute:TNTFSDiskAttribute;const AUnit:Int64;var AVCN,ALength:Int64):Boolean;
{Get the Start VCN and Length of the supplied Unit using the details of the supplied Attribute}
{Note: The passed attribute must be the first instance}
{Note: Caller must hold the runs lock}
begin
 {}
 Result:=False;

 if FDriver = nil then Exit;
 if AAttribute = nil then Exit;

 {Check Compressed}
 if (AAttribute.IsCompressed) and (AAttribute.CompressionUnit <> 0) then
  begin
   {Get Start}
   AVCN:=(AUnit shl AAttribute.CompressionUnit);

   {Get Length}
   ALength:=(1 shl AAttribute.CompressionUnit);

   Result:=True;
  end;
end;

{=============================================================================}

function TNTFSFileSystem.GetUnitCompressed(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute;const AUnit:Int64;var AVCN,ALength:Int64):Boolean;
{Determine if the runs for the supplied unit are actually compressed by checking normal and sparse run sequence and length}
{A compressed unit must have one or more normal runs followed by one or more sparse runs which total to the unit size (or greater)}
{Note: The unit VCN and Length are returned even if the unit is not compressed}
{Note: The passed attribute must be the first instance}
{Note: Caller must hold the runs lock}
var
 VCN:Int64;
 Length:Int64;
 Remain:Int64;
 RunVCN:Int64;
 StartVCN:Int64;
 SparseVCN:Int64;

 Run:TNTFSDiskRun;
 Instance:LongWord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if FDriver = nil then Exit;
 if ARecord = nil then Exit;
 if AAttribute = nil then Exit;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetUnitCompressed - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' Type = ' + IntToHex(AAttribute.AttributeType,8) + ' Attribute = ' + AAttribute.AttributeName + ' Unit = ' + IntToStr(AUnit));
 {$ENDIF}

 {Check Compressed}
 if (AAttribute.IsCompressed) and (AAttribute.CompressionUnit <> 0) then
  begin
   Instance:=ntfsInstanceFirst;

   {Get Start}
   AVCN:=(AUnit shl AAttribute.CompressionUnit);
   VCN:=AVCN;
   StartVCN:=ntfsUnknownCluster;
   SparseVCN:=ntfsUnknownCluster;

   {Get Length}
   ALength:=(1 shl AAttribute.CompressionUnit);
   Remain:=ALength;

   {Get Attribute}
   Attribute:=ARecord.GetAttributeByUnit(AAttribute,AUnit,Instance);
   if Attribute = nil then Exit;

   {Get Run}
   Run:=Attribute.GetRun(VCN,RunVCN);
   if Run = nil then Exit;
   if Run.IsLast then Exit;

   {Check Sparse}
   if Run.IsSparse then Exit; {Starts with a Sparse Run, Not Compressed}

   {Normal Run}
   StartVCN:=VCN;

   {Update Position}
   Length:=(Run.Length - (VCN - RunVCN));
   Inc(VCN,Length);
   Dec(Remain,Min64(Length,Remain));

   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetUnitCompressed - VCN = ' + IntToStr(VCN) + ' Remain = ' + IntToStr(Remain));
   {$ENDIF}

   {Check Remain}
   while Remain > 0 do
    begin
     {Get Attribute}
     Attribute:=ARecord.GetAttributeByVCN(AAttribute,VCN,Instance);
     if Attribute = nil then Exit;

     {Get Run}
     Run:=Attribute.GetRun(VCN,RunVCN);
     if Run = nil then Exit;
     if Run.IsLast then Exit;

     {Check Sparse}
     if Run.IsSparse then
      begin
       {Sparse Run}
       if SparseVCN = ntfsUnknownCluster then SparseVCN:=VCN;

       {Update Position}
       Inc(VCN,Run.Length);
       Dec(Remain,Min64(Run.Length,Remain));
      end
     else
      begin
       {Normal Run}
       if SparseVCN <> ntfsUnknownCluster then Exit; {Already encountered a Sparse Run, Not Compressed}

       {Update Position}
       Inc(VCN,Run.Length);
       Dec(Remain,Min64(Run.Length,Remain));
      end;

     {$IFDEF NTFS_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetUnitCompressed - VCN = ' + IntToStr(VCN) + ' Remain = ' + IntToStr(Remain));
     {$ENDIF}
    end;

   {Check Start and Sparse}
   if (StartVCN = ntfsUnknownCluster) or (SparseVCN = ntfsUnknownCluster) then Exit; {Does not contain both Normal and Sparse, Not Compressed}

   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetUnitCompressed - StartVCN = ' + IntToStr(StartVCN) + ' SparseVCN = ' + IntToStr(SparseVCN));
   {$ENDIF}

   Result:=True;
  end;
end;

{=============================================================================}

function TNTFSFileSystem.AllocFileRecord(var ARecordNumber:Int64;AMft:Boolean):Boolean;
{Allocate a free file record and mark it in the mft bitmap}
{If Mft is True then allocate an Mft extension record}
var
 Next:Int64;
 Size:Int64;
 Count:LongWord;
 Remain:LongWord;
 Instance:LongWord;

 Origin:TNTFSDiskRecord;
 Current:TNTFSDiskRecord;
 Data:TNTFSDiskAttribute;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FRecords.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FMaster = nil then Exit;
  if FFileRecordSize = 0 then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AllocFileRecord - RecordNumber = ' + IntToHex(ARecordNumber,16) + ' MFT = ' + BoolToStr(AMft));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get Origin}
  Origin:=FMaster.Origin;
  if Origin = nil then Exit;

  {Check Total}
  if FTotalFileRecordCount = 0 then
   begin
    {Get Data}
    Data:=Origin.GetAttribute(ntfsAttrTypeData,ntfsBlankName,ntfsInstanceFirst);
    if Data = nil then Exit;

    {Get Total}
    Size:=0;
    if Data.NonResident = ntfsAttributeResident then Size:=Data.DataSize;
    if Data.NonResident = ntfsAttributeNonResident then Size:=Data.StreamSize;
    FTotalFileRecordCount:=(Size div FFileRecordSize);

    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AllocFileRecord - TotalFileRecordCount = ' + IntToStr(FTotalFileRecordCount));
    {$ENDIF}
   end;
  if FTotalFileRecordCount = 0 then Exit;

  {Get Bitmap}
  Attribute:=Origin.GetAttribute(ntfsAttrTypeBitmap,ntfsBlankName,ntfsInstanceFirst);
  if Attribute = nil then Exit;

  {Load Bitmap}
  if TNTFSBitmapAttribute(Attribute).Bitmap = nil then
   begin
    if Attribute.NonResident = ntfsAttributeResident then TNTFSBitmapAttribute(Attribute).BitmapSize:=Attribute.DataSize;
    if Attribute.NonResident = ntfsAttributeNonResident then TNTFSBitmapAttribute(Attribute).BitmapSize:=Attribute.StreamSize;

    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AllocFileRecord - BitmapSize = ' + IntToStr(TNTFSBitmapAttribute(Attribute).BitmapSize));
    {$ENDIF}

    {Read Bitmap}
    Instance:=ntfsInstanceFirst;
    if ReadAttribute(Origin,Attribute,TNTFSBitmapAttribute(Attribute).Bitmap^,0,TNTFSBitmapAttribute(Attribute).BitmapSize,Instance,True) <> Integer(TNTFSBitmapAttribute(Attribute).BitmapSize) then Exit;
   end;
  if TNTFSBitmapAttribute(Attribute).BitmapSize = 0 then Exit;

  {Get Next}
  Next:=GetNextFreeFileRecord(AMft);
  if (Next <> ntfsUnknownRecordNumber) and ((GetReservedFileRecordCount > 1) or ((GetFreeFileRecordCount - GetReservedFileRecordCount) > 1)) then {Make sure there is always one FileRecord available to expand MFT}
   begin
    {Get Size}
    Size:=(TNTFSBitmapAttribute(Attribute).BitmapSize shl 3); {Multiply by 8}
    Size:=Min64(Size,FTotalFileRecordCount);
    Count:=1;

    {Alloc Bitmap}
    if not AllocBitmap(TNTFSBitmapAttribute(Attribute).Bitmap,Size,Next,Count) then Exit;

    {Write Bitmap} {Allow Update}
    Instance:=ntfsInstanceFirst;
    if WriteAttribute(Origin,Attribute,TNTFSBitmapAttribute(Attribute).Bitmap^,0,TNTFSBitmapAttribute(Attribute).BitmapSize,Instance,True) <> Integer(TNTFSBitmapAttribute(Attribute).BitmapSize) then Exit;

    {Get Record}
    Current:=GetRecordEx(nil,Next,False,True);
    if Current = nil then
     begin
      {Get Free Record}
      Current:=GetRecordEx(nil,Next,True,True);
      if Current = nil then Exit;

      {Update Record} {Do not mark as used}
      if Current.RecordNumber < FMftMirrorCount then Current.Mirrored:=True;
      if (Current.RecordNumber >= ntfsFileTypeReserved1) and (Current.RecordNumber <= ntfsFileTypeReserved4) then Current.Reserved:=True;
      if (Current.RecordNumber >= ntfsFileTypeExpansion1) and (Current.RecordNumber <= ntfsFileTypeExpansion8) then Current.Expansion:=True;
      Current.IsFolder:=False;
      Current.IsUnknown1:=False;
      Current.IsIndexView:=False;
      Current.Overflow:=False;
      Current.Extension:=False;
      Current.RecordFlags:=ntfsFileRecordFlagNone;
      Current.HardLinkCount:=0;
      Current.SequenceNumber:=1;
      {Current.BaseReference:=0;} {Automatic}
      Current.NextAttributeId:=0;
      Current.RecordAllocated:=FFileRecordSize;
      Current.UpdateSequenceNumber:=0;
      Current.LogFileSequenceNumber:=0;
      Current.UpdateSequenceOffset:=Current.CalculatedSequenceOffset(FVolumeVersion);
      Current.UpdateSequenceLength:=Current.CalculatedSequenceLength(ntfsUpdateSequenceSize); {Previously FSectorSize however always 512}
      Current.AttributeOffset:=Current.CalculatedOffset(FVolumeVersion);
      Current.RecordSize:=Current.CalculatedSize(FVolumeVersion);

      {Set Record}
      if not SetRecord(Current) then Exit;
     end
    else
     begin
      {Check Free}
      if Current.IsUsed then Exit;

      {Update Record} {Do not save / Do not mark as used}
      Current.IsFolder:=False;
      Current.IsUnknown1:=False;
      Current.IsIndexView:=False;
      Current.Overflow:=False;
      Current.Extension:=False;
      Current.LogFileSequenceNumber:=0;
      Current.HardLinkCount:=0;
      Current.RecordFlags:=ntfsFileRecordFlagNone;
      Current.RecordSize:=Current.CalculatedSize(FVolumeVersion);
      {Current.BaseReference:=0;} {Automatic}
      Current.NextAttributeId:=0;
      Current.UpdateSequenceNumber:=0;
      if Current.SequenceNumber = 0 then Current.SequenceNumber:=1;
     end;

    {Update Free}
    if FFreeFileRecordCount <> ntfsUnknownRecordNumber then Dec(FFreeFileRecordCount);

    {Update Reserved}
    if (FReservedFileRecordCount <> ntfsUnknownRecordNumber) and (Current.Expansion) then Dec(FReservedFileRecordCount);
    ARecordNumber:=Next;

    Result:=True;
   end
  else
   begin
    {Get Data}
    Data:=Origin.GetAttribute(ntfsAttrTypeData,ntfsBlankName,ntfsInstanceFirst);
    if Data = nil then Exit;

    {Get Count}
    Count:=1; {FFilesPerCluster;} {Only allocate 1 record, SizeAttribute will expand the MFT allocation in 16k blocks}
    {if Count = 0 then Count:=1;}

    {Get Data Size}
    Size:=((FTotalFileRecordCount + Count) * FFileRecordSize);

    {Size Data}
    if not SizeAttribute(Origin,Data,Size) then Exit;

    {Get Bitmap Size}
    Size:=(TNTFSBitmapAttribute(Attribute).BitmapSize shl 3); {Multiply by 8}
    if Size < (FTotalFileRecordCount + Count) then
     begin
      {Size Bitmap}
      TNTFSBitmapAttribute(Attribute).BitmapSize:=TNTFSBitmapAttribute(Attribute).BitmapSize + 8;
      if not SizeAttribute(Origin,Attribute,TNTFSBitmapAttribute(Attribute).BitmapSize) then Exit;

      {Write Bitmap} {No Update}
      Instance:=ntfsInstanceFirst;
      if WriteAttribute(Origin,Attribute,TNTFSBitmapAttribute(Attribute).Bitmap^,0,TNTFSBitmapAttribute(Attribute).BitmapSize,Instance,False) <> Integer(TNTFSBitmapAttribute(Attribute).BitmapSize) then Exit;
     end;

    {Get Remain}
    Remain:=Count;
    while Remain > 0 do
     begin
      {Get Record Number}
      Next:=FTotalFileRecordCount;

      {Create Record}
      Current:=FRecords.NewRecord(nil,Next,FVolumeVersion);
      if Current = nil then Exit;

      {Update Record} {Do not mark as used}
      if Current.RecordNumber < FMftMirrorCount then Current.Mirrored:=True;
      if (Current.RecordNumber >= ntfsFileTypeReserved1) and (Current.RecordNumber <= ntfsFileTypeReserved4) then Current.Reserved:=True;
      if (Current.RecordNumber >= ntfsFileTypeExpansion1) and (Current.RecordNumber <= ntfsFileTypeExpansion8) then Current.Expansion:=True;
      Current.IsFolder:=False;
      Current.IsUnknown1:=False;
      Current.IsIndexView:=False;
      Current.Overflow:=False;
      Current.Extension:=False;
      Current.RecordFlags:=ntfsFileRecordFlagNone;
      Current.HardLinkCount:=0;
      Current.SequenceNumber:=0;
      {Current.BaseReference:=0;} {Automatic}
      Current.NextAttributeId:=0;
      Current.RecordAllocated:=FFileRecordSize;
      Current.UpdateSequenceNumber:=0;
      Current.LogFileSequenceNumber:=0;
      Current.UpdateSequenceOffset:=Current.CalculatedSequenceOffset(FVolumeVersion);
      Current.UpdateSequenceLength:=Current.CalculatedSequenceLength(ntfsUpdateSequenceSize); {Previously FSectorSize however always 512}
      Current.AttributeOffset:=Current.CalculatedOffset(FVolumeVersion);
      Current.RecordSize:=Current.CalculatedSize(FVolumeVersion);

      {Insert Record}
      if not FRecords.InsertRecord(Current) then Exit;

      {Set Record}
      if not SetRecord(Current) then Exit;

      {Update Total}
      Inc(FTotalFileRecordCount);

      {Update Free}
      if FFreeFileRecordCount <> ntfsUnknownRecordNumber then Inc(FFreeFileRecordCount);

      {Update Reserved}
      if (FReservedFileRecordCount <> ntfsUnknownRecordNumber) and (Current.Expansion) then Inc(FReservedFileRecordCount);

      {$IFDEF NTFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AllocFileRecord - Count = ' + IntToStr(Count) + ' Remain = ' + IntToStr(Remain) + ' Next = ' + IntToHex(Next,16));
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AllocFileRecord - TotalFileRecordCount = ' + IntToStr(FTotalFileRecordCount) + ' FreeFileRecordCount = ' + IntToStr(FFreeFileRecordCount) + ' ReservedFileRecordCount = ' + IntToStr(FReservedFileRecordCount));
      {$ENDIF}

      {Update Position}
      Dec(Remain);
     end;

    {Set Records}
    if not SetRecords(Origin) then Exit;

    {Update Entry}
    if not Data.UpdateEntry(FMft) then Exit;

    {Alloc FileRecord}
    Result:=AllocFileRecord(ARecordNumber,AMft);
   end;
 finally
  FRecords.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.ReleaseFileRecord(const ARecordNumber:Int64):Boolean;
{Return a file record to free and mark it in the mft bitmap}
var
 Size:Int64;
 Instance:LongWord;
 Origin:TNTFSDiskRecord;
 Current:TNTFSDiskRecord;
 Data:TNTFSDiskAttribute;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FRecords.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FMaster = nil then Exit;
  if FFileRecordSize = 0 then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.ReleaseFileRecord - RecordNumber = ' + IntToHex(ARecordNumber,16));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Record}
  if ARecordNumber = ntfsUnknownRecordNumber then Exit;

  {Get Origin}
  Origin:=FMaster.Origin;
  if Origin = nil then Exit;

  {Check Total}
  if FTotalFileRecordCount = 0 then
   begin
    {Get Data}
    Data:=Origin.GetAttribute(ntfsAttrTypeData,ntfsBlankName,ntfsInstanceFirst);
    if Data = nil then Exit;

    {Get Total}
    Size:=0;
    if Data.NonResident = ntfsAttributeResident then Size:=Data.DataSize;
    if Data.NonResident = ntfsAttributeNonResident then Size:=Data.StreamSize;
    FTotalFileRecordCount:=(Size div FFileRecordSize);

    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.ReleaseFileRecord - TotalFileRecordCount = ' + IntToStr(FTotalFileRecordCount));
    {$ENDIF}
   end;
  if FTotalFileRecordCount = 0 then Exit;

  {Get Bitmap}
  Attribute:=Origin.GetAttribute(ntfsAttrTypeBitmap,ntfsBlankName,ntfsInstanceFirst);
  if Attribute = nil then Exit;

  {Load Bitmap}
  if TNTFSBitmapAttribute(Attribute).Bitmap = nil then
   begin
    if Attribute.NonResident = ntfsAttributeResident then TNTFSBitmapAttribute(Attribute).BitmapSize:=Attribute.DataSize;
    if Attribute.NonResident = ntfsAttributeNonResident then TNTFSBitmapAttribute(Attribute).BitmapSize:=Attribute.StreamSize;
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.ReleaseFileRecord - BitmapSize = ' + IntToStr(TNTFSBitmapAttribute(Attribute).BitmapSize));
    {$ENDIF}

    {Read Bitmap}
    Instance:=ntfsInstanceFirst;
    if ReadAttribute(Origin,Attribute,TNTFSBitmapAttribute(Attribute).Bitmap^,0,TNTFSBitmapAttribute(Attribute).BitmapSize,Instance,True) <> Integer(TNTFSBitmapAttribute(Attribute).BitmapSize) then Exit;
   end;
  if TNTFSBitmapAttribute(Attribute).BitmapSize = 0 then Exit;

  {Get Record}
  Current:=GetRecordEx(nil,ARecordNumber,False,True);
  if Current = nil then Exit;

  {Check Free}
  if not Current.IsUsed then Exit;

  {Update Record}
  Current.IsUsed:=False;
  Current.IsFolder:=False;
  Current.IsUnknown1:=False;
  Current.IsIndexView:=False;
  Current.SequenceNumber:=Current.SequenceNumber + 1;
  if Current.SequenceNumber = 0 then Current.SequenceNumber:=1;

  {Set Record}
  if not SetRecord(Current) then Exit;

  {Get Size}
  Size:=(TNTFSBitmapAttribute(Attribute).BitmapSize shl 3); {Multiply by 8}
  Size:=Min64(Size,FTotalFileRecordCount);

  {Release Bitmap}
  if not ReleaseBitmap(TNTFSBitmapAttribute(Attribute).Bitmap,Size,ARecordNumber,1) then Exit;

  {Write Bitmap} {Allow Update}
  Instance:=ntfsInstanceFirst;
  if WriteAttribute(Origin,Attribute,TNTFSBitmapAttribute(Attribute).Bitmap^,0,TNTFSBitmapAttribute(Attribute).BitmapSize,Instance,True) <> Integer(TNTFSBitmapAttribute(Attribute).BitmapSize) then Exit;

  {Update Free}
  if FFreeFileRecordCount <> ntfsUnknownRecordNumber then Inc(FFreeFileRecordCount);

  {Update Reserved}
  if (FReservedFileRecordCount <> ntfsUnknownRecordNumber) and (Current.Expansion) then Inc(FReservedFileRecordCount);

  Result:=True;
 finally
  FRecords.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.GetFileRecordVCN(const ARecordNumber:Int64;var AVCN:Int64;var AOffset:LongWord):Boolean;
{Get the virtual cluster number for the supplied file record number}
{The offset indicates the starting point of the record in the cluster}
begin
 {}
 Result:=False;

 if FDriver = nil then Exit;
 if FFileRecordSize = 0 then Exit;

 {Get VCN}
 if FFilesPerCluster = 0 then
  begin
   if FClustersPerFile = 0 then Exit;
   AVCN:=(ARecordNumber shl FFileRecordShiftCount);

   {Get Offset}
   AOffset:=0;
  end
 else
  begin
   AVCN:=(ARecordNumber shr FFileRecordShiftCount);

   {Get Offset}
   AOffset:=(ARecordNumber and FFileRecordOffsetMask) * FFileRecordSize; //To Do //Make this a shift ?
  end;

 Result:=True;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetFileRecordVCN - RecordNumber = ' + IntToHex(ARecordNumber,16) + ' VCN = ' + IntToStr(AVCN) + ' Offset = ' + IntToStr(AOffset));
 {$ENDIF}
end;

{=============================================================================}

function TNTFSFileSystem.GetNextFreeFileRecord(AMft:Boolean):Int64;
{Get the next free file record number from the mft bitmap}
{If Mft is True then get the next free Mft extension record}
{Return is a Record Number (Not a VCN or FileReference)}
{Note: Return is a 64 bit value but NTFS is currently limited to 2^32 file records per volume}
var
 Next:Int64;
 Start:Int64;
 Size:LongWord;
 Instance:LongWord;
 Origin:TNTFSDiskRecord;
 Data:TNTFSDiskAttribute;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=ntfsUnknownRecordNumber;

 if not FRecords.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FMaster = nil then Exit;
  if FFileRecordSize = 0 then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetNextFreeFileRecord - MFT = ' + BoolToStr(AMft));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get Start}
  Start:=0;
  if FLastFreeFileRecord <> ntfsUnknownRecordNumber then Start:=FLastFreeFileRecord;

  {Get Origin}
  Origin:=FMaster.Origin;
  if Origin = nil then Exit;

  {Check Total}
  if FTotalFileRecordCount = 0 then
   begin
    {Get Data}
    Data:=Origin.GetAttribute(ntfsAttrTypeData,ntfsBlankName,ntfsInstanceFirst);
    if Data = nil then Exit;

    {Get Total}
    Size:=0;
    if Data.NonResident = ntfsAttributeResident then Size:=Data.DataSize;
    if Data.NonResident = ntfsAttributeNonResident then Size:=Data.StreamSize;
    FTotalFileRecordCount:=(Size div FFileRecordSize);

    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetNextFreeFileRecord - TotalFileRecordCount = ' + IntToStr(FTotalFileRecordCount));
    {$ENDIF}
   end;
  if FTotalFileRecordCount = 0 then Exit;

  {Get Bitmap}
  Attribute:=Origin.GetAttribute(ntfsAttrTypeBitmap,ntfsBlankName,ntfsInstanceFirst);
  if Attribute = nil then Exit;

  {Load Bitmap}
  if TNTFSBitmapAttribute(Attribute).Bitmap = nil then
   begin
    if Attribute.NonResident = ntfsAttributeResident then TNTFSBitmapAttribute(Attribute).BitmapSize:=Attribute.DataSize;
    if Attribute.NonResident = ntfsAttributeNonResident then TNTFSBitmapAttribute(Attribute).BitmapSize:=Attribute.StreamSize;

    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetNextFreeFileRecord - BitmapSize = ' + IntToStr(TNTFSBitmapAttribute(Attribute).BitmapSize));
    {$ENDIF}

    {Read Bitmap}
    Instance:=ntfsInstanceFirst;
    if ReadAttribute(Origin,Attribute,TNTFSBitmapAttribute(Attribute).Bitmap^,0,TNTFSBitmapAttribute(Attribute).BitmapSize,Instance,True) <> Integer(TNTFSBitmapAttribute(Attribute).BitmapSize) then Exit;
   end;
  if TNTFSBitmapAttribute(Attribute).BitmapSize = 0 then Exit;

  {Get Start}
  if AMft then
   begin
    Start:=0;
   end
  else
   begin
    if (Start >= ntfsFileTypeMft) and (Start <= ntfsFileTypeExpansion8) then Start:=ntfsFileTypeExpansion8 + 1;
   end;

  {Get Size}
  Size:=(TNTFSBitmapAttribute(Attribute).BitmapSize shl 3); {Multiply by 8}
  Size:=Min(Size,FTotalFileRecordCount);

  {Get Next}
  Next:=GetBitmapNextFree(TNTFSBitmapAttribute(Attribute).Bitmap,Size,Start);
  if Next <> ntfsBitmapUnknown then
   begin
    FLastFreeFileRecord:=Next;
    Result:=FLastFreeFileRecord;

    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetNextFreeFileRecord - Result = ' + IntToHex(Result,16));
    {$ENDIF}

    Exit;
   end;

  {Check for Wrap}
  if not(AMft) then {Note: This cannot happen for MFT because MFT searches always start at 0}
   begin
    if Start <> (ntfsFileTypeExpansion8 + 1) then
     begin
      FLastFreeFileRecord:=0;
      Result:=GetNextFreeFileRecord(AMft);
     end;
   end;
 finally
  FRecords.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.GetFreeFileRecordCount:Int64;
{Get the number of free file records from the mft bitmap}
{Note: Return is a 64 bit value but NTFS is currently limited to 2^32 file records per volume}
var
 Count:Int64;
 Size:LongWord;
 Instance:LongWord;
 Origin:TNTFSDiskRecord;
 Data:TNTFSDiskAttribute;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=ntfsUnknownRecordNumber;

 if FDriver = nil then Exit;
 if FMaster = nil then Exit;
 if FFileRecordSize = 0 then Exit;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetFreeFileRecordCount');
 {$ENDIF}

 {Check ReadOnly}
 if FReadOnly then Exit;

 {Check Free Count}
 if FFreeFileRecordCount = ntfsUnknownRecordNumber then
  begin
   if not FRecords.WriterLock then Exit;
   try
    {Get Params}
    Count:=0;
    FFreeFileRecordCount:=0;

    {Get Origin}
    Origin:=FMaster.Origin;
    if Origin = nil then Exit;

    {Check Total}
    if FTotalFileRecordCount = 0 then
     begin
      {Get Data}
      Data:=Origin.GetAttribute(ntfsAttrTypeData,ntfsBlankName,ntfsInstanceFirst);
      if Data = nil then Exit;

      {Get Total}
      Size:=0;
      if Data.NonResident = ntfsAttributeResident then Size:=Data.DataSize;
      if Data.NonResident = ntfsAttributeNonResident then Size:=Data.StreamSize;
      FTotalFileRecordCount:=(Size div FFileRecordSize);

      {$IFDEF NTFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetFreeFileRecordCount - TotalFileRecordCount = ' + IntToStr(FTotalFileRecordCount));
      {$ENDIF}
     end;
    if FTotalFileRecordCount = 0 then Exit;

    {Get Bitmap}
    Attribute:=Origin.GetAttribute(ntfsAttrTypeBitmap,ntfsBlankName,ntfsInstanceFirst);
    if Attribute = nil then Exit;

    {Load Bitmap}
    if TNTFSBitmapAttribute(Attribute).Bitmap = nil then
     begin
      if Attribute.NonResident = ntfsAttributeResident then TNTFSBitmapAttribute(Attribute).BitmapSize:=Attribute.DataSize;
      if Attribute.NonResident = ntfsAttributeNonResident then TNTFSBitmapAttribute(Attribute).BitmapSize:=Attribute.StreamSize;

      {$IFDEF NTFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetFreeFileRecordCount - BitmapSize = ' + IntToStr(TNTFSBitmapAttribute(Attribute).BitmapSize));
      {$ENDIF}

      {Read Bitmap}
      Instance:=ntfsInstanceFirst;
      if ReadAttribute(Origin,Attribute,TNTFSBitmapAttribute(Attribute).Bitmap^,0,TNTFSBitmapAttribute(Attribute).BitmapSize,Instance,True) <> Integer(TNTFSBitmapAttribute(Attribute).BitmapSize) then Exit;
     end;
    if TNTFSBitmapAttribute(Attribute).BitmapSize = 0 then Exit;

    {Get Size}
    Size:=(TNTFSBitmapAttribute(Attribute).BitmapSize shl 3); {Multiply by 8}
    Size:=Min(Size,FTotalFileRecordCount);

    {Get Free}
    Count:=GetBitmapFreeCount(TNTFSBitmapAttribute(Attribute).Bitmap,Size);
    if Count <> ntfsBitmapUnknown then FFreeFileRecordCount:=Count;
   finally
    FRecords.WriterUnlock;
   end;
  end;

 Result:=FFreeFileRecordCount;
end;

{=============================================================================}

function TNTFSFileSystem.GetReservedFileRecordCount:Int64;
var
 Next:Int64;
 Start:Int64;
 Count:Int64;
 Size:LongWord;
 Instance:LongWord;
 Origin:TNTFSDiskRecord;
 Data:TNTFSDiskAttribute;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=ntfsUnknownRecordNumber;

 if FDriver = nil then Exit;
 if FMaster = nil then Exit;
 if FFileRecordSize = 0 then Exit;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetReservedFileRecordCount');
 {$ENDIF}

 {Check ReadOnly}
 if FReadOnly then Exit;

 {Check Reserved Count}
 if FReservedFileRecordCount = ntfsUnknownRecordNumber then
  begin
   if not FRecords.WriterLock then Exit;
   try
    {Get Params}
    Count:=0;
    FReservedFileRecordCount:=0;

    {Get Origin}
    Origin:=FMaster.Origin;
    if Origin = nil then Exit;

    {Check Total}
    if FTotalFileRecordCount = 0 then
     begin
      {Get Data}
      Data:=Origin.GetAttribute(ntfsAttrTypeData,ntfsBlankName,ntfsInstanceFirst);
      if Data = nil then Exit;

      {Get Total}
      Size:=0;
      if Data.NonResident = ntfsAttributeResident then Size:=Data.DataSize;
      if Data.NonResident = ntfsAttributeNonResident then Size:=Data.StreamSize;
      FTotalFileRecordCount:=(Size div FFileRecordSize);

      {$IFDEF NTFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetReservedFileRecordCount - TotalFileRecordCount = ' + IntToStr(FTotalFileRecordCount));
      {$ENDIF}
     end;
    if FTotalFileRecordCount = 0 then Exit;

    {Get Bitmap}
    Attribute:=Origin.GetAttribute(ntfsAttrTypeBitmap,ntfsBlankName,ntfsInstanceFirst);
    if Attribute = nil then Exit;

    {Load Bitmap}
    if TNTFSBitmapAttribute(Attribute).Bitmap = nil then
     begin
      if Attribute.NonResident = ntfsAttributeResident then TNTFSBitmapAttribute(Attribute).BitmapSize:=Attribute.DataSize;
      if Attribute.NonResident = ntfsAttributeNonResident then TNTFSBitmapAttribute(Attribute).BitmapSize:=Attribute.StreamSize;

      {$IFDEF NTFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetReservedFileRecordCount - BitmapSize = ' + IntToStr(TNTFSBitmapAttribute(Attribute).BitmapSize));
      {$ENDIF}

      {Read Bitmap}
      Instance:=ntfsInstanceFirst;
      if ReadAttribute(Origin,Attribute,TNTFSBitmapAttribute(Attribute).Bitmap^,0,TNTFSBitmapAttribute(Attribute).BitmapSize,Instance,True) <> Integer(TNTFSBitmapAttribute(Attribute).BitmapSize) then Exit;
     end;
    if TNTFSBitmapAttribute(Attribute).BitmapSize = 0 then Exit;

    {Get Size}
    Size:=(TNTFSBitmapAttribute(Attribute).BitmapSize shl 3); {Multiply by 8}
    Size:=Min(Size,FTotalFileRecordCount);

    {Get Reserved}
    Start:=ntfsFileTypeExpansion1;
    while Start <= ntfsFileTypeExpansion8 do
     begin
      Next:=GetBitmapNextFree(TNTFSBitmapAttribute(Attribute).Bitmap,Size,Start);
      if Next = Start then Inc(Count);
      Inc(Start);
     end;
    FReservedFileRecordCount:=Count;
   finally
    FRecords.WriterUnlock;
   end;
  end;

 Result:=FReservedFileRecordCount;
end;

{=============================================================================}

function TNTFSFileSystem.AllocIndexRecord(ARecord:TNTFSDiskRecord;AAllocation,ABitmap:TNTFSDiskAttribute;AIndex:TNTFSDiskIndex;var ARecordNumber:Int64):Boolean;
{Allocate a free index record and mark it in the index bitmap}
{Note: The passed attributes must be the first instance}
{Note: Caller must hold the records lock}
var
 Next:Int64;
 Size:LongWord;
 Count:LongWord;
 Remain:LongWord;
 Instance:LongWord;
begin
 {}
 Result:=False;

 if not FRecords.NodesWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if AAllocation = nil then Exit;
  if ABitmap = nil then Exit;
  if AIndex = nil then Exit;
  if AIndex.IndexRecordSize = 0 then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AllocIndexRecord - RecordNumber = ' + IntToHex(ARecordNumber,16));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AllocIndexRecord - IndexRecordSize = ' + IntToHex(AIndex.IndexRecordSize,8));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AllocIndexRecord - IndexCounterOffset = ' + IntToHex(AIndex.IndexCounterOffset,8));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AllocIndexRecord - ClustersPerIndex = ' + IntToHex(AIndex.ClustersPerIndex,8));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AllocIndexRecord - IndexsPerCluster = ' + IntToHex(AIndex.IndexsPerCluster,8));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AllocIndexRecord - IndexCounterShift = ' + IntToHex(AIndex.IndexCounterShift,4));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AllocIndexRecord - IndexRecordShiftCount = ' + IntToHex(AIndex.IndexRecordShiftCount,4));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AllocIndexRecord - IndexRecordOffsetMask = ' + IntToHex(AIndex.IndexRecordOffsetMask,16));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Total}
  if AIndex.TotalIndexRecordCount = 0 then
   begin
    {Get Total}
    Size:=0;
    if AAllocation.NonResident = ntfsAttributeResident then Size:=AAllocation.DataSize;
    if AAllocation.NonResident = ntfsAttributeNonResident then Size:=AAllocation.StreamSize;
    AIndex.TotalIndexRecordCount:=(Size div AIndex.IndexRecordSize);

    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AllocIndexRecord - TotalIndexRecordCount = ' + IntToStr(AIndex.TotalIndexRecordCount));
    {$ENDIF}
   end;
  if AIndex.TotalIndexRecordCount = 0 then Exit;

  {Load Bitmap}
  if TNTFSBitmapAttribute(ABitmap).Bitmap = nil then
   begin
    if ABitmap.NonResident = ntfsAttributeResident then TNTFSBitmapAttribute(ABitmap).BitmapSize:=ABitmap.DataSize;
    if ABitmap.NonResident = ntfsAttributeNonResident then TNTFSBitmapAttribute(ABitmap).BitmapSize:=ABitmap.StreamSize;

    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AllocIndexRecord - BitmapSize = ' + IntToStr(TNTFSBitmapAttribute(ABitmap).BitmapSize));
    {$ENDIF}

    {Read Bitmap}
    Instance:=ntfsInstanceFirst;
    if ReadAttribute(ARecord,ABitmap,TNTFSBitmapAttribute(ABitmap).Bitmap^,0,TNTFSBitmapAttribute(ABitmap).BitmapSize,Instance,True) <> Integer(TNTFSBitmapAttribute(ABitmap).BitmapSize) then Exit;
   end;
  if TNTFSBitmapAttribute(ABitmap).BitmapSize = 0 then Exit;

  {Get Next}
  Next:=GetNextFreeIndexRecord(ARecord,AAllocation,ABitmap,AIndex);
  if Next <> ntfsUnknownRecordNumber then
   begin
    {Get Size}
    Size:=(TNTFSBitmapAttribute(ABitmap).BitmapSize shl 3); {Multiply by 8}
    Size:=Min(Size,AIndex.TotalIndexRecordCount);
    Count:=1;

    {Alloc Bitmap}
    if not AllocBitmap(TNTFSBitmapAttribute(ABitmap).Bitmap,Size,Next,Count) then Exit;

    {Write Bitmap} {No Update}
    Instance:=ntfsInstanceFirst;
    if WriteAttribute(ARecord,ABitmap,TNTFSBitmapAttribute(ABitmap).Bitmap^,0,TNTFSBitmapAttribute(ABitmap).BitmapSize,Instance,False) <> Integer(TNTFSBitmapAttribute(ABitmap).BitmapSize) then Exit;

    //To Do //Handling of AIndex.IndexCounterShift / AIndex.IndexRecordShiftCount etc

    //To Do //Update Node ? //Anything ? //Nothing !!

    {Update Free}
    if AIndex.FreeIndexRecordCount <> ntfsUnknownRecordNumber then AIndex.FreeIndexRecordCount:=AIndex.FreeIndexRecordCount - 1;
    ARecordNumber:=(Next shl AIndex.IndexCounterShift);

    Result:=True;
   end
  else
   begin
    {Get Count}
    Count:=1; {AIndex.IndexsPerCluster;} {Only allocate 1 record}
    {if Count = 0 then Count:=1;}

    {Get Allocation Size}
    Size:=((AIndex.TotalIndexRecordCount + Count) * AIndex.IndexRecordSize);

    {Size Allocation}
    if not SizeAttribute(ARecord,AAllocation,Size) then Exit;

    {Get Bitmap Size}
    Size:=(TNTFSBitmapAttribute(ABitmap).BitmapSize shl 3); {Multiply by 8}
    if Size < (AIndex.TotalIndexRecordCount + Count) then
     begin
      {Size Bitmap}
      TNTFSBitmapAttribute(ABitmap).BitmapSize:=TNTFSBitmapAttribute(ABitmap).BitmapSize + 8;
      if not SizeAttribute(ARecord,ABitmap,TNTFSBitmapAttribute(ABitmap).BitmapSize) then Exit;

      {Write Bitmap} {No Update}
      Instance:=ntfsInstanceFirst;
      if WriteAttribute(ARecord,ABitmap,TNTFSBitmapAttribute(ABitmap).Bitmap^,0,TNTFSBitmapAttribute(ABitmap).BitmapSize,Instance,False) <> Integer(TNTFSBitmapAttribute(ABitmap).BitmapSize) then Exit;
     end;

    {Get Remain}
    Remain:=Count;
    while Remain > 0 do
     begin
      {Update Total}
      AIndex.TotalIndexRecordCount:=AIndex.TotalIndexRecordCount + 1;

      {Update Free}
      if AIndex.FreeIndexRecordCount <> ntfsUnknownRecordNumber then AIndex.FreeIndexRecordCount:=AIndex.FreeIndexRecordCount + 1;

      {Update Position}
      Dec(Remain);
     end;

    {Alloc IndexRecord}
    Result:=AllocIndexRecord(ARecord,AAllocation,ABitmap,AIndex,ARecordNumber);
   end;
 finally
  FRecords.NodesWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.ReleaseIndexRecord(ARecord:TNTFSDiskRecord;AAllocation,ABitmap:TNTFSDiskAttribute;AIndex:TNTFSDiskIndex;const ARecordNumber:Int64):Boolean;
{Return an index record to free and mark it in the index bitmap}
{Note: The passed attributes must be the first instance}
{Note: Caller must hold the records lock}
var
 Size:LongWord;
 Instance:LongWord;
 Node:TNTFSDiskNode;
begin
 {}
 Result:=False;

 if not FRecords.NodesWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if AAllocation = nil then Exit;
  if ABitmap = nil then Exit;
  if AIndex = nil then Exit;
  if AIndex.IndexRecordSize = 0 then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.ReleaseIndexRecord - RecordNumber = ' + IntToHex(ARecordNumber,16));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.ReleaseIndexRecord - IndexRecordSize = ' + IntToHex(AIndex.IndexRecordSize,8));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.ReleaseIndexRecord - IndexCounterOffset = ' + IntToHex(AIndex.IndexCounterOffset,8));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.ReleaseIndexRecord - ClustersPerIndex = ' + IntToHex(AIndex.ClustersPerIndex,8));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.ReleaseIndexRecord - IndexsPerCluster = ' + IntToHex(AIndex.IndexsPerCluster,8));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.ReleaseIndexRecord - IndexCounterShift = ' + IntToHex(AIndex.IndexCounterShift,4));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.ReleaseIndexRecord - IndexRecordShiftCount = ' + IntToHex(AIndex.IndexRecordShiftCount,4));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.ReleaseIndexRecord - IndexRecordOffsetMask = ' + IntToHex(AIndex.IndexRecordOffsetMask,16));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Record}
  if ARecordNumber = ntfsUnknownRecordNumber then Exit;

  {Check Total}
  if AIndex.TotalIndexRecordCount = 0 then
   begin
    {Get Total}
    Size:=0;
    if AAllocation.NonResident = ntfsAttributeResident then Size:=AAllocation.DataSize;
    if AAllocation.NonResident = ntfsAttributeNonResident then Size:=AAllocation.StreamSize;
    AIndex.TotalIndexRecordCount:=(Size div AIndex.IndexRecordSize);

    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.ReleaseIndexRecord - TotalIndexRecordCount = ' + IntToStr(AIndex.TotalIndexRecordCount));
    {$ENDIF}
   end;
  if AIndex.TotalIndexRecordCount = 0 then Exit;

  {Load Bitmap}
  if TNTFSBitmapAttribute(ABitmap).Bitmap = nil then
   begin
    if ABitmap.NonResident = ntfsAttributeResident then TNTFSBitmapAttribute(ABitmap).BitmapSize:=ABitmap.DataSize;
    if ABitmap.NonResident = ntfsAttributeNonResident then TNTFSBitmapAttribute(ABitmap).BitmapSize:=ABitmap.StreamSize;

    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.ReleaseIndexRecord - BitmapSize = ' + IntToStr(TNTFSBitmapAttribute(ABitmap).BitmapSize));
    {$ENDIF}

    {Read Bitmap}
    Instance:=ntfsInstanceFirst;
    if ReadAttribute(ARecord,ABitmap,TNTFSBitmapAttribute(ABitmap).Bitmap^,0,TNTFSBitmapAttribute(ABitmap).BitmapSize,Instance,True) <> Integer(TNTFSBitmapAttribute(ABitmap).BitmapSize) then Exit;
   end;
  if TNTFSBitmapAttribute(ABitmap).BitmapSize = 0 then Exit;

  {Get Node}
  Node:=AIndex.GetNode(ARecordNumber);
  if Node = nil then Exit;

  //To Do //Handling of AIndex.IndexCounterShift / AIndex.IndexRecordShiftCount etc

  //To Do //Update Node ? //Release Run ? //Size Attribute ? - Check with Windows - Nothing ? //To be confirmed

  {Get Size}
  Size:=(TNTFSBitmapAttribute(ABitmap).BitmapSize shl 3); {Multiply by 8}
  Size:=Min(Size,AIndex.TotalIndexRecordCount);

  {Release Bitmap}
  if not ReleaseBitmap(TNTFSBitmapAttribute(ABitmap).Bitmap,Size,(ARecordNumber shr AIndex.IndexCounterShift),1) then Exit;

  {Write Bitmap} {No Update}
  Instance:=ntfsInstanceFirst;
  if WriteAttribute(ARecord,ABitmap,TNTFSBitmapAttribute(ABitmap).Bitmap^,0,TNTFSBitmapAttribute(ABitmap).BitmapSize,Instance,False) <> Integer(TNTFSBitmapAttribute(ABitmap).BitmapSize) then Exit;

  {Update Free}
  if AIndex.FreeIndexRecordCount <> ntfsUnknownRecordNumber then AIndex.FreeIndexRecordCount:=AIndex.FreeIndexRecordCount + 1;

  Result:=True;
 finally
  FRecords.NodesWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.GetIndexRecordVCN(AIndex:TNTFSDiskIndex;const ARecordNumber:Int64;var AVCN:Int64;var AOffset:LongWord):Boolean;
{Get the virtual cluster number for the supplied index record number}
{The offset indicates the starting point of the record in the cluster}
{Note: Caller must hold the records lock}
var
 Count:Int64;
begin
 {}
 Result:=False;

 if not FRecords.NodesWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if AIndex = nil then Exit;
  if AIndex.IndexRecordSize = 0 then Exit;

  {Get VCN}
  if AIndex.IndexsPerCluster = 0 then
   begin
    if AIndex.ClustersPerIndex = 0 then Exit;
    Count:=(ARecordNumber shr AIndex.IndexCounterShift);
    AVCN:=(Count shl AIndex.IndexRecordShiftCount);

    {Get Offset}
    AOffset:=0;
   end
  else
   begin
    Count:=(ARecordNumber shr AIndex.IndexCounterShift);
    AVCN:=(Count shr AIndex.IndexRecordShiftCount);

    {Get Offset}
    AOffset:=(Count and AIndex.IndexRecordOffsetMask) * AIndex.IndexRecordSize; //To Do //Make this a Shift ?
   end;

  Result:=True;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetIndexRecordVCN - RecordNumber = ' + IntToHex(ARecordNumber,16) + ' VCN = ' + IntToStr(AVCN) + ' Offset = ' + IntToStr(AOffset));
  {$ENDIF}
 finally
  FRecords.NodesWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.GetNextFreeIndexRecord(ARecord:TNTFSDiskRecord;AAllocation,ABitmap:TNTFSDiskAttribute;AIndex:TNTFSDiskIndex):Int64;
{Get the next free index record number from the index bitmap}
{Return is a Record Number (Not a VCN)}
{Note: The passed attributes must be the first instance}
{Note: Return is a 64 bit value but it is not currently possible for an index to contain more than
       2^32 index nodes due to the limitation on the number of file records on an NTFS volume}
{Note: Caller must hold the records lock}
var
 Next:Int64;
 Start:Int64;
 Size:LongWord;
 Instance:LongWord;
begin
 {}
 Result:=ntfsUnknownRecordNumber;

 if not FRecords.NodesWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if AAllocation = nil then Exit;
  if ABitmap = nil then Exit;
  if AIndex = nil then Exit;
  if AIndex.IndexRecordSize = 0 then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetNextFreeIndexRecord');
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get Start}
  Start:=0;
  if AIndex.LastFreeIndexRecord <> ntfsUnknownRecordNumber then Start:=AIndex.LastFreeIndexRecord;

  {Check Total}
  if AIndex.TotalIndexRecordCount = 0 then
   begin
    {Get Total}
    Size:=0;
    if AAllocation.NonResident = ntfsAttributeResident then Size:=AAllocation.DataSize;
    if AAllocation.NonResident = ntfsAttributeNonResident then Size:=AAllocation.StreamSize;
    AIndex.TotalIndexRecordCount:=(Size div AIndex.IndexRecordSize);

    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetNextFreeIndexRecord - TotalIndexRecordCount = ' + IntToStr(AIndex.TotalIndexRecordCount));
    {$ENDIF}
   end;
  if AIndex.TotalIndexRecordCount = 0 then Exit;

  {Load Bitmap}
  if TNTFSBitmapAttribute(ABitmap).Bitmap = nil then
   begin
    if ABitmap.NonResident = ntfsAttributeResident then TNTFSBitmapAttribute(ABitmap).BitmapSize:=ABitmap.DataSize;
    if ABitmap.NonResident = ntfsAttributeNonResident then TNTFSBitmapAttribute(ABitmap).BitmapSize:=ABitmap.StreamSize;

    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetNextFreeIndexRecord - BitmapSize = ' + IntToStr(TNTFSBitmapAttribute(ABitmap).BitmapSize));
    {$ENDIF}

    {Read Bitmap}
    Instance:=ntfsInstanceFirst;
    if ReadAttribute(ARecord,ABitmap,TNTFSBitmapAttribute(ABitmap).Bitmap^,0,TNTFSBitmapAttribute(ABitmap).BitmapSize,Instance,True) <> Integer(TNTFSBitmapAttribute(ABitmap).BitmapSize) then Exit;
   end;
  if TNTFSBitmapAttribute(ABitmap).BitmapSize = 0 then Exit;

  {Get Size}
  Size:=(TNTFSBitmapAttribute(ABitmap).BitmapSize shl 3); {Multiply by 8}
  Size:=Min(Size,AIndex.TotalIndexRecordCount);

  {Get Next}
  Next:=GetBitmapNextFree(TNTFSBitmapAttribute(ABitmap).Bitmap,Size,Start);
  if Next <> ntfsBitmapUnknown then
   begin
    AIndex.LastFreeIndexRecord:=Next;
    Result:=AIndex.LastFreeIndexRecord;
    Exit;
   end;

  {Check for Wrap}
  if Start <> 0 then
   begin
    AIndex.LastFreeIndexRecord:=0;
    Result:=GetNextFreeIndexRecord(ARecord,AAllocation,ABitmap,AIndex);
   end;
 finally
  FRecords.NodesWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.GetFreeIndexRecordCount(ARecord:TNTFSDiskRecord;AAllocation,ABitmap:TNTFSDiskAttribute;AIndex:TNTFSDiskIndex):Int64;
{Get the number of free index records from the index bitmap}
{Note: The passed attributes must be the first instance}
{Note: Return is a 64 bit value but it is not currently possible for a index to contain more than
       2^32 index nodes due to the limitation on the number of file records on an NTFS volume}
{Note: Caller must hold the records lock}
var
 Count:Int64;
 Size:LongWord;
 Instance:LongWord;
begin
 {}
 Result:=ntfsUnknownRecordNumber;

 if FDriver = nil then Exit;
 if ARecord = nil then Exit;
 if AAllocation = nil then Exit;
 if ABitmap = nil then Exit;
 if AIndex = nil then Exit;
 if AIndex.IndexRecordSize = 0 then Exit;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetFreeIndexRecordCount');
 {$ENDIF}

 {Check ReadOnly}
 if FReadOnly then Exit;

 {Check Free Count}
 if AIndex.FreeIndexRecordCount = ntfsUnknownRecordNumber then
  begin
   if not FRecords.NodesWriterLock then Exit;
   try
    {Get Params}
    Count:=0;
    AIndex.FreeIndexRecordCount:=0;

    {Check Total}
    if AIndex.TotalIndexRecordCount = 0 then
     begin
      {Get Total}
      Size:=0;
      if AAllocation.NonResident = ntfsAttributeResident then Size:=AAllocation.DataSize;
      if AAllocation.NonResident = ntfsAttributeNonResident then Size:=AAllocation.StreamSize;
      AIndex.TotalIndexRecordCount:=(Size div AIndex.IndexRecordSize);

      {$IFDEF NTFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetFreeIndexRecordCount - TotalIndexRecordCount = ' + IntToStr(AIndex.TotalIndexRecordCount));
      {$ENDIF}
     end;
    if AIndex.TotalIndexRecordCount = 0 then Exit;

    {Load Bitmap}
    if TNTFSBitmapAttribute(ABitmap).Bitmap = nil then
     begin
      if ABitmap.NonResident = ntfsAttributeResident then TNTFSBitmapAttribute(ABitmap).BitmapSize:=ABitmap.DataSize;
      if ABitmap.NonResident = ntfsAttributeNonResident then TNTFSBitmapAttribute(ABitmap).BitmapSize:=ABitmap.StreamSize;

      {$IFDEF NTFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetFreeIndexRecordCount - BitmapSize = ' + IntToStr(TNTFSBitmapAttribute(ABitmap).BitmapSize));
      {$ENDIF}

      {Read Bitmap}
      Instance:=ntfsInstanceFirst;
      if ReadAttribute(ARecord,ABitmap,TNTFSBitmapAttribute(ABitmap).Bitmap^,0,TNTFSBitmapAttribute(ABitmap).BitmapSize,Instance,True) <> Integer(TNTFSBitmapAttribute(ABitmap).BitmapSize) then Exit;
     end;
    if TNTFSBitmapAttribute(ABitmap).BitmapSize = 0 then Exit;

    {Get Size}
    Size:=(TNTFSBitmapAttribute(ABitmap).BitmapSize shl 3); {Multiply by 8}
    Size:=Min(Size,AIndex.TotalIndexRecordCount);

    {Get Free}
    Count:=GetBitmapFreeCount(TNTFSBitmapAttribute(ABitmap).Bitmap,Size);
    if Count <> ntfsBitmapUnknown then AIndex.FreeIndexRecordCount:=Count;
   finally
    FRecords.NodesWriterUnlock;
   end;
  end;

 Result:=AIndex.FreeIndexRecordCount;
end;

{=============================================================================}

function TNTFSFileSystem.AllocBlock(ABlock:TNTFSDiskBlock;const AStart:Int64;var ACount:LongWord):Boolean;
{Start is the cluster number in the block to start from}
{Count is the number of clusters to be allocated in the block}
{Count also returns the actual number of allocated clusters}
{Start must be greater than or equal to block cluster}
{Note: Caller must hold the blocks lock}
var
 Start:LongWord;
begin
 {}
 Result:=False;

 if ABlock = nil then Exit;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AllocBlock - BlockNo = ' + IntToHex(ABlock.BlockNo,8) + ' Start = ' + IntToStr(AStart) + ' Count = ' + IntToStr(ACount));
 {$ENDIF}

 {Check ReadOnly}
 if FReadOnly then Exit;

 {Check Start}
 if AStart < ABlock.BlockCluster then Exit;
 if AStart >= (ABlock.BlockCluster + ABlock.BlockCount) then Exit;

 {Check Count}
 if ACount = 0 then Exit;
 if (AStart + ACount) > (ABlock.BlockCluster + ABlock.BlockCount) then Exit;

 {Get Start}
 Start:=(AStart - ABlock.BlockCluster);

 {Alloc Bitmap}
 Result:=AllocBitmap(ABlock.BlockBuffer,ABlock.BlockCount,Start,ACount);
 {if Result then AStart:=(ABlock.BlockCluster + Start);} {Not required will be the same as passed value}
end;

{=============================================================================}

function TNTFSFileSystem.ReleaseBlock(ABlock:TNTFSDiskBlock;const AStart:Int64;ACount:LongWord):Boolean;
{Start is the cluster number in the block to start from}
{Count is the number of clusters to be released in the block}
{Start must be greater than or equal to block cluster}
{Note: Caller must hold the blocks lock}
var
 Start:LongWord;
begin
 {}
 Result:=False;

 if ABlock = nil then Exit;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.ReleaseBlock - BlockNo = ' + IntToHex(ABlock.BlockNo,8) + ' Start = ' + IntToStr(AStart) + ' Count = ' + IntToStr(ACount));
 {$ENDIF}

 {Check ReadOnly}
 if FReadOnly then Exit;

 {Check Start}
 if AStart < ABlock.BlockCluster then Exit;
 if AStart >= (ABlock.BlockCluster + ABlock.BlockCount) then Exit;

 {Check Count}
 if ACount = 0 then Exit;
 if (AStart + ACount) > (ABlock.BlockCluster + ABlock.BlockCount) then Exit;

 {Get Start}
 Start:=(AStart - ABlock.BlockCluster);

 {Release Bitmap}
 Result:=ReleaseBitmap(ABlock.BlockBuffer,ABlock.BlockCount,Start,ACount);
end;

{=============================================================================}

function TNTFSFileSystem.GetBlockNextFree(ABlock:TNTFSDiskBlock;const AStart:Int64):Int64;
{Start is the cluster number in the block to start from}
{Start must be greater than or equal to block cluster}
{Note: Caller must hold the blocks lock}
var
 Next:LongWord;
 Start:LongWord;
begin
 {}
 Result:=ntfsUnknownCluster;

 if ABlock = nil then Exit;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetBlockNextFree - BlockNo = ' + IntToHex(ABlock.BlockNo,8) + ' Start = ' + IntToStr(AStart));
 {$ENDIF}

 {Check ReadOnly}
 if FReadOnly then Exit;

 {Check Start}
 if AStart < ABlock.BlockCluster then Exit;
 if AStart >= (ABlock.BlockCluster + ABlock.BlockCount) then Exit;

 {Get Start}
 Start:=(AStart - ABlock.BlockCluster);

 {Get Next Free}
 Next:=GetBitmapNextFree(ABlock.BlockBuffer,ABlock.BlockCount,Start);
 if Next <> ntfsBitmapUnknown then Result:=(ABlock.BlockCluster + Next);
end;

{=============================================================================}

function TNTFSFileSystem.GetBlockFreeCount(ABlock:TNTFSDiskBlock):Int64;
{Note: Caller must hold the blocks lock}
var
 Count:LongWord;
begin
 {}
 Result:=ntfsUnknownCluster;

 if ABlock = nil then Exit;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetBlockFreeCount - BlockNo = ' + IntToHex(ABlock.BlockNo,8));
 {$ENDIF}

 {Check ReadOnly}
 if FReadOnly then Exit;

 {Get Free Count}
 Count:=GetBitmapFreeCount(ABlock.BlockBuffer,ABlock.BlockCount);
 if Count <> ntfsBitmapUnknown then Result:=Count;
end;

{=============================================================================}

function TNTFSFileSystem.AllocBitmap(ABuffer:Pointer;ASize:LongWord;AStart:LongWord;var ACount:LongWord):Boolean;
{Allocate up to Count bits in the Bitmap and mark them as Used}
{Size is the total number of bits in the bitmap}
{Start is the bit number in the bitmap to start from}
{Count is the number of bits to be allocated in the bitmap}
{Count also returns the actual number of allocated bits}
{Note: Bitmaps are a multiple of 64 bits in size but free and used
       blocks are marked using an array of bytes in little endian order}
{Note: Caller must hold the records, attributes or blocks lock (As appropriate)}
var
 Size:LongWord;   {Number of 64 bit blocks in bitmap}
 Start:LongWord;  {Starting block in the bitmap data}
 Block:LongWord;  {Current block in the bitmap data}
 Offset:LongWord; {Current offset into the bitmap data}
 Bit:LongWord;    {Starting bit to allocate in current block (0 if first bit)}
 Bits:LongWord;   {Number of bits to allocate in current block (64 if all bits)}
 Remain:LongWord; {Number of Bits remaining to be allocated}
 Current:LongWord;{Current bit to allocate in current block}
begin
 {}
 Result:=False;

 if ABuffer = nil then Exit;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AllocBitmap - Size = ' + IntToStr(ASize) + ' Start = ' + IntToStr(AStart) + ' Count = ' + IntToStr(ACount));
 {$ENDIF}

 {Check ReadOnly}
 if FReadOnly then Exit;

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
 ACount:=0;
 while Block < Size do
  begin
   {Get Bits}
   Bits:=Min(Remain,ntfsBitmapMaskBits);
   if Bit > 0 then Bits:=Min(Remain,(ntfsBitmapMaskBits - Bit));

   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AllocBitmap - Block = ' + IntToStr(Block) + ' Offset = ' + IntToStr(Offset) + ' Remain = ' + IntToStr(Remain) + ' Bit = ' + IntToStr(Bit) + ' Bits = ' + IntToStr(Bits));
   {$ENDIF}

   {Check Bits}
   if (Bit = 0) and (Bits = ntfsBitmapMaskBits) and (Int64(Pointer(PtrUInt(ABuffer) + Offset)^) = ntfsBitmapMaskNone) then
    begin
     {All Free}
     {Allocate All}
     Int64(Pointer(PtrUInt(ABuffer) + Offset)^):=ntfsBitmapMaskAll;
     Inc(ACount,ntfsBitmapMaskBits);
     Dec(Remain,ntfsBitmapMaskBits);
    end
   else if (Bit = 0) and (Bits = ntfsBitmapMaskBits) and (Int64(Pointer(PtrUInt(ABuffer) + Offset)^) = ntfsBitmapMaskAll) then
    begin
     {All Used}
     if Block = Start then Exit; {Start Bit was not Free}
     Remain:=0;
     Break; {Break to allow success}
    end
   else
    begin
     {Used and Free}
     {Allocate Some}
     for Current:=Bit to (Bit + (Bits - 1)) do
      begin
       if (Int64(Pointer(PtrUInt(ABuffer) + Offset)^) and ntfsBitmapMasks[Current]) = ntfsBitmapMaskNone then
        begin
         {$IFDEF NTFS_DEBUG}
         if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AllocBitmap - Block = ' + IntToStr(Block) + ' Current = ' + IntToStr(Current) + ' Bits = ' + IntToHex(Int64(Pointer(PtrUInt(ABuffer) + Offset)^),16));
         {$ENDIF}

         Int64(Pointer(PtrUInt(ABuffer) + Offset)^):=Int64(Pointer(PtrUInt(ABuffer) + Offset)^) or ntfsBitmapMasks[Current];
         Inc(ACount);
         Dec(Remain);
         if Remain = 0 then Break;
        end
       else
        begin
         if (Block = Start) and (Current = Bit) then Exit; {Start Bit was not Free}
         Remain:=0;
         Break; {Break to allow success}
        end;
      end;
    end;

   {Update Params}
   Bit:=0;
   Inc(Block);
   Inc(Offset,8);
   if Remain = 0 then Break;
  end;

 Result:=(ACount > 0);
end;

{=============================================================================}

function TNTFSFileSystem.ReleaseBitmap(ABuffer:Pointer;ASize:LongWord;AStart:LongWord;ACount:LongWord):Boolean;
{Deallocate Count bits from Start in the Bitmap and mark them as Free}
{Size is the total number of bits in the bitmap}
{Start is the bit number in the bitmap to start from}
{Count is the number of bits to be deallocated in the bitmap}
{Note: Bitmaps are a multiple of 64 bits in size but free and used
       blocks are marked using an array of bytes in little endian order}
{Note: Caller must hold the records, attributes or blocks lock (As appropriate)}
var
 Size:LongWord;   {Number of 64 bit blocks in bitmap}
 Start:LongWord;  {Starting block in the bitmap data}
 Block:LongWord;  {Current block in the bitmap data}
 Offset:LongWord; {Current offset into the bitmap data}
 Bit:LongWord;    {Starting bit to deallocate in current block (0 if first bit)}
 Bits:LongWord;   {Number of bits to deallocate in current block (64 if all bits)}
 Remain:LongWord; {Number of Bits remaining to be deallocated}
 Current:LongWord;{Current bit to deallocate in current block}
begin
 {}
 Result:=False;

 if ABuffer = nil then Exit;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.ReleaseBitmap - Size = ' + IntToStr(ASize) + ' Start = ' + IntToStr(AStart) + ' Count = ' + IntToStr(ACount));
 {$ENDIF}

 {Check ReadOnly}
 if FReadOnly then Exit;

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
   Bits:=Min(Remain,ntfsBitmapMaskBits);
   if Bit > 0 then Bits:=Min(Remain,(ntfsBitmapMaskBits - Bit));

   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.ReleaseBitmap - Block = ' + IntToStr(Block) + ' Offset = ' + IntToStr(Offset) + ' Remain = ' + IntToStr(Remain) + ' Bit = ' + IntToStr(Bit) + ' Bits = ' + IntToStr(Bits));
   {$ENDIF}

   {Deallocate Bits}
   if (Bit = 0) and (Bits = ntfsBitmapMaskBits) then
    begin
     {Deallocate All}
     Int64(Pointer(PtrUInt(ABuffer) + Offset)^):=ntfsBitmapMaskNone;
    end
   else
    begin
     {Deallocate Some}
     for Current:=Bit to (Bit + (Bits - 1)) do
      begin
       Int64(Pointer(PtrUInt(ABuffer) + Offset)^):=Int64(Pointer(PtrUInt(ABuffer) + Offset)^) and not(ntfsBitmapMasks[Current]);
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

function TNTFSFileSystem.GetBitmapNextFree(ABuffer:Pointer;ASize,AStart:LongWord):LongWord;
{Size is the total number of bits in the bitmap}
{Start is the bit number in the bitmap to start from}
{Note: Bitmaps are a multiple of 64 bits in size but free and used
       blocks are marked using an array of bytes in little endian order}
{Note: Caller must hold the records, attributes or blocks lock (As appropriate)}
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
 {}
 Result:=ntfsBitmapUnknown;

 if ABuffer = nil then Exit;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetBitmapNextFree - Size = ' + IntToStr(ASize) + ' Start = ' + IntToStr(AStart));
 {$ENDIF}

 {Check ReadOnly}
 if FReadOnly then Exit;

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
   Bits:=Min(Remain,ntfsBitmapMaskBits);
   if Bit > 0 then Bits:=Min(Remain,(ntfsBitmapMaskBits - Bit));

   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetBitmapNextFree - Block = ' + IntToStr(Block) + ' Offset = ' + IntToStr(Offset) + ' Remain = ' + IntToStr(Remain) + ' Bit = ' + IntToStr(Bit) + ' Bits = ' + IntToStr(Bits));
   {$ENDIF}

   {Check Bits}
   if (Bit = 0) and (Int64(Pointer(PtrUInt(ABuffer) + Offset)^) = ntfsBitmapMaskNone) then
    begin
     {All Free}
     Result:=(Block shl 6); {Multiply by 64}
     Exit;
    end
   else if (Bit = 0) and (Int64(Pointer(PtrUInt(ABuffer) + Offset)^) = ntfsBitmapMaskAll) then
    begin
     {All Used}
     {Nothing}
    end
   else
    begin
     {Used and Free}
     for Current:=Bit to (Bit + (Bits - 1)) do
      begin
       if (Int64(Pointer(PtrUInt(ABuffer) + Offset)^) and ntfsBitmapMasks[Current]) = ntfsBitmapMaskNone then
        begin
         {$IFDEF NTFS_DEBUG}
         if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetBitmapNextFree - Block = ' + IntToStr(Block) + ' Current = ' + IntToStr(Current) + ' Bits = ' + IntToHex(Int64(Pointer(PtrUInt(ABuffer) + Offset)^),16));
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

function TNTFSFileSystem.GetBitmapFreeCount(ABuffer:Pointer;ASize:LongWord):LongWord;
{Size is the total number of bits in the bitmap}
{Note: Bitmaps are a multiple of 64 bits in size but free and used
       blocks are marked using an array of bytes in little endian order}
{Note: Caller must hold the records, attributes or blocks lock (As appropriate)}
var
 Size:LongWord;   {Number of 64 bit blocks in bitmap}
 Value:Int64;
 Block:LongWord;  {Current block in the bitmap data}
 Offset:LongWord; {Current offset into the bitmap data}
 Bits:LongWord;   {Number of Bits to check in current block}
 Remain:LongWord; {Number of Bits remaining to be checked}
begin
 {}
 Result:=ntfsBitmapUnknown;

 if ABuffer = nil then Exit;

 Result:=0;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetBitmapFreeCount - Size = ' + IntToStr(ASize));
 {$ENDIF}

 {Check ReadOnly}
 if FReadOnly then Exit;

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
   Bits:=Min(Remain,ntfsBitmapMaskBits);

   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetBitmapFreeCount - Bits = ' + IntToStr(Bits) + ' Value = ' + IntToHex(Int64(Pointer(PtrUInt(ABuffer) + Offset)^),16) + ' Mask = ' + IntToHex(ntfsBitmapOverlays[Bits - 1],16));
   {$ENDIF}

   if Int64(Pointer(PtrUInt(ABuffer) + Offset)^) = ntfsBitmapMaskNone then
    begin
     {All Free}
     Inc(Result,Bits);
    end
   else if Int64(Pointer(PtrUInt(ABuffer) + Offset)^) = ntfsBitmapMaskAll then
    begin
     {All Used}
     {Nothing}
    end
   else
    begin
     {Used and Free}
     Value:=(not(Int64(Pointer(PtrUInt(ABuffer) + Offset)^))) and (ntfsBitmapOverlays[Bits - 1]);
     while Value <> 0 do
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

function TNTFSFileSystem.ReadFixup(ABuffer:Pointer;AOffset:LongWord;ASequenceNumber,ASequenceOffset,ASequenceLength:Word;AFree:Boolean):Boolean;
var
 Count:Word;
 Offset:LongWord;
 Update:PNTFSUpdateSequenceRecord;
begin
 {}
 Result:=False;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.ReadFixup - Offset = ' + IntToStr(AOffset) + ' SequenceNumber = ' + IntToHex(ASequenceNumber,4) + ' SequenceOffset = ' + IntToHex(ASequenceOffset,4) + ' SequenceLength = ' + IntToHex(ASequenceLength,4));
 {$ENDIF}

 if FDriver = nil then Exit;
 if ABuffer = nil then Exit;
 if ASequenceLength < 2 then Exit; {SizeOf(Word);}  {Must be at least one sector}
 if (ASequenceOffset = 0) and not(AFree) then Exit; {Can be zero on a free record}

 {Get Record}
 Update:=PNTFSUpdateSequenceRecord(PtrUInt(ABuffer) + AOffset + ASequenceOffset);

 {Check Record}
 if Update.UpdateSequenceNumber <> ASequenceNumber then Exit;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.ReadFixup - Update.UpdateSequenceNumber = ' + IntToStr(Update.UpdateSequenceNumber));
 {$ENDIF}

 {Read Fixup}
 Offset:=AOffset + ntfsUpdateSequenceSize - 2; {SizeOf(Word);} {Previously FSectorSize however always 512}
 for Count:=1 to ASequenceLength - 1 do
  begin
   if Word(Pointer(PtrUInt(ABuffer) + Offset)^) <> Update.UpdateSequenceNumber then Exit;
   Word(Pointer(PtrUInt(ABuffer) + Offset)^):=Update.UpdateSequenceArray[Count - 1];
   Inc(Offset,ntfsUpdateSequenceSize); {Previously FSectorSize however always 512}
  end;

 Result:=True;
end;

{=============================================================================}

function TNTFSFileSystem.WriteFixup(ABuffer:Pointer;AOffset:LongWord;ASequenceNumber,ASequenceOffset,ASequenceLength:Word):Boolean;
var
 Count:Word;
 Offset:LongWord;
 Update:PNTFSUpdateSequenceRecord;
begin
 {}
 Result:=False;

 if FDriver = nil then Exit;
 if ABuffer = nil then Exit;
 if ASequenceOffset = 0 then Exit;
 if ASequenceLength < 2 then Exit; {Must be at least one sector}

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.WriteFixup - SequenceNumber = ' + IntToHex(ASequenceNumber,4) + ' SequenceOffset = ' + IntToHex(ASequenceOffset,4) + ' SequenceLength = ' + IntToHex(ASequenceLength,4));
 {$ENDIF}

 {Check ReadOnly}
 if FReadOnly then Exit;

 {Get Record}
 Update:=PNTFSUpdateSequenceRecord(PtrUInt(ABuffer) + AOffset + ASequenceOffset);

 {Set Record}
 Update.UpdateSequenceNumber:=ASequenceNumber;

 {Write Fixup}
 Offset:=AOffset + ntfsUpdateSequenceSize - 2; {SizeOf(Word);} {Previously FSectorSize however always 512}
 for Count:=1 to ASequenceLength - 1 do
  begin
   Update.UpdateSequenceArray[Count - 1]:=Word(Pointer(PtrUInt(ABuffer) + Offset)^);
   Word(Pointer(PtrUInt(ABuffer) + Offset)^):=Update.UpdateSequenceNumber;
   Inc(Offset,ntfsUpdateSequenceSize); {Previously FSectorSize however always 512}
  end;

 Result:=True;
end;

{=============================================================================}

function TNTFSFileSystem.AddObjId(const AObjectId:TGUID;const AFileReference:Int64;ABirthVolumeId,ABirthObjectId,ADomainId:PGUID):Boolean;
{Insert the Key for ObjectId in the $O index of $ObjId}
var
 Key:TNTFSDiskKey;
 Index:TNTFSDiskIndex;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
 ObjIdData:TNTFSObjIdData;
begin
 {}
 Result:=False;

 if not FRecords.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FObjId = nil then Exit;
  if FObjId.Origin = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AddObjId - ObjectId = ' + GUIDToString(AObjectId) + ' Reference = ' + IntToHex(AFileReference,16));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get Origin}
  Origin:=FObjId.Origin.Origin;
  if Origin = nil then Exit;

  {Get Attribute}
  Attribute:=Origin.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameObjectId,ntfsInstanceFirst);
  if Attribute = nil then Exit;

  {Get Index}
  Index:=Attribute.Index;
  if Index = nil then Exit;
  if Index.IndexType <> ntfsAttrTypeNone then Exit;
  if Index.CollateRule <> ntfsCollateTypeGUID then Exit;

  {Get Key}
  Key:=Index.FindKey(@AObjectId,ntfsObjIdKeySize);
  if Key <> nil then Exit;

  {Add Key}
  FillChar(ObjIdData,SizeOf(TNTFSObjIdData),0);
  ObjIdData.FileReference:=AFileReference;
  if ABirthVolumeId <> nil then ObjIdData.BirthVolumeId:=ABirthVolumeId^;
  if ABirthObjectId <> nil then ObjIdData.BirthObjectId:=ABirthObjectId^;
  if ADomainId <> nil then ObjIdData.DomainId:=ADomainId^;
  Key:=Index.NewKey(@AObjectId,@ObjIdData,ntfsObjIdKeySize,ntfsObjIdSize);
  if Key = nil then Exit;
  try
   {Update Key}
   Key.EntrySize:=Key.CalculatedSize(FVolumeVersion);

   {Insert Key}
   if not Index.InsertKey(Key) then Exit;
   try
    {Set Index}
    if not SetAttribute(Origin,Attribute) then Exit;

    Result:=True;
   finally
    if not Result then Index.RemoveKey(Key);
    if not Result then Key:=nil;
   end;
  finally
   if not(Result) and (Key <> nil) then Index.DestroyKey(Key); {Dont double free the key}
  end;
 finally
  FRecords.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.RemoveObjId(const AObjectId:TGUID):Boolean;
{Remove the Key for ObjectId in the $O index of $ObjId}
var
 Key:TNTFSDiskKey;
 Index:TNTFSDiskIndex;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FRecords.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FObjId = nil then Exit;
  if FObjId.Origin = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.RemoveObjId - ObjectId = ' + GUIDToString(AObjectId));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get Origin}
  Origin:=FObjId.Origin.Origin;
  if Origin = nil then Exit;

  {Get Attribute}
  Attribute:=Origin.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameObjectId,ntfsInstanceFirst);
  if Attribute = nil then Exit;

  {Get Index}
  Index:=Attribute.Index;
  if Index = nil then Exit;
  if Index.IndexType <> ntfsAttrTypeNone then Exit;
  if Index.CollateRule <> ntfsCollateTypeGUID then Exit;

  {Get Key}
  Key:=Index.FindKey(@AObjectId,ntfsObjIdKeySize);
  if Key = nil then Exit;
  if TNTFSDataKey(Key).Data = nil then Exit;

  {Remove Key}
  if not Index.RemoveKey(Key) then Exit;

  {Set Index}
  if not SetAttribute(Origin,Attribute) then Exit;

  Result:=True;
 finally
  FRecords.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.AddQuota(AOwner:LongWord;AQuota:PNTFSQuotaData):Boolean;
{Insert the Key for Owner in the $Q index of $Quota}
var
 SidSize:LongWord;

 Key:TNTFSDiskKey;
 Index:TNTFSDiskIndex;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FRecords.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if AQuota = nil then Exit;
  if FQuota = nil then Exit;
  if FQuota.Origin = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AddQuota - Owner = ' + IntToHex(AOwner,8));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get Origin}
  Origin:=FQuota.Origin.Origin;
  if Origin = nil then Exit;

  {Get Attribute}
  Attribute:=Origin.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameQuota,ntfsInstanceFirst);
  if Attribute = nil then Exit;

  {Get Index}
  Index:=Attribute.Index;
  if Index = nil then Exit;
  if Index.IndexType <> ntfsAttrTypeNone then Exit;
  if Index.CollateRule <> ntfsCollateTypeLongWord then Exit;

  {Get Key}
  Key:=Index.FindKey(@AOwner,ntfsQuotaKeySize);
  if Key <> nil then Exit;

  {Add Key}
  SidSize:=0;
  if Security.IsValidSid(@AQuota.SID) then SidSize:=Security.GetLengthSid(@AQuota.SID);
  Key:=Index.NewKey(@AOwner,AQuota,ntfsQuotaKeySize,ntfsQuotaSize + SidSize);
  if Key = nil then Exit;
  try
   {Update Key}
   Key.EntrySize:=Key.CalculatedSize(FVolumeVersion);

   {Insert Key}
   if not Index.InsertKey(Key) then Exit;
   try
    {Set Index}
    if not SetAttribute(Origin,Attribute) then Exit;

    Result:=True;
   finally
    if not Result then Index.RemoveKey(Key);
    if not Result then Key:=nil;
   end;
  finally
   if not(Result) and (Key <> nil) then Index.DestroyKey(Key); {Dont double free the key}
  end;
 finally
  FRecords.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.RemoveQuota(AOwner:LongWord):Boolean;
{Remove the Key for Owner in the $Q index of $Quota}
var
 Key:TNTFSDiskKey;
 Index:TNTFSDiskIndex;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FRecords.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FQuota = nil then Exit;
  if FQuota.Origin = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.RemoveQuota - Owner = ' + IntToHex(AOwner,8));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get Origin}
  Origin:=FQuota.Origin.Origin;
  if Origin = nil then Exit;

  {Get Attribute}
  Attribute:=Origin.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameQuota,ntfsInstanceFirst);
  if Attribute = nil then Exit;

  {Get Index}
  Index:=Attribute.Index;
  if Index = nil then Exit;
  if Index.IndexType <> ntfsAttrTypeNone then Exit;
  if Index.CollateRule <> ntfsCollateTypeLongWord then Exit;

  {Get Key}
  Key:=Index.FindKey(@AOwner,ntfsQuotaKeySize);
  if Key = nil then Exit;
  if TNTFSDataKey(Key).Data = nil then Exit;

  {Remove Key}
  if not Index.RemoveKey(Key) then Exit;

  {Set Index}
  if not SetAttribute(Origin,Attribute) then Exit;

  Result:=True;
 finally
  FRecords.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.SetQuota(AOwner:LongWord;AQuota:PNTFSQuotaData):Boolean;
{Update the Key for Owner in the $Q index of $Quota}
var
 Key:TNTFSDiskKey;
 Index:TNTFSDiskIndex;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FRecords.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if AQuota = nil then Exit;
  if FQuota = nil then Exit;
  if FQuota.Origin = nil then Exit;

  //To Do //May also need IncrementQuota and DecrementQuota functions
                //Which call GetQuota and then SetQuota
                //Driven from SizeEntry/Attribute/Run

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SetQuota - Owner = ' + IntToHex(AOwner,8));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get Origin}
  Origin:=FQuota.Origin.Origin;
  if Origin = nil then Exit;

  {Get Attribute}
  Attribute:=Origin.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameQuota,ntfsInstanceFirst);
  if Attribute = nil then Exit;

  {Get Index}
  Index:=Attribute.Index;
  if Index = nil then Exit;
  if Index.IndexType <> ntfsAttrTypeNone then Exit;
  if Index.CollateRule <> ntfsCollateTypeLongWord then Exit;

  {Get Key}
  Key:=Index.FindKey(@AOwner,ntfsQuotaKeySize);
  if Key = nil then Exit;
  if TNTFSDataKey(Key).Data = nil then Exit;

  {Update Key}
  //To Do //DataSize ?
  TNTFSDataKey(Key).Data:=AQuota;
  Key.Changed:=True;
  Key.EntrySize:=Key.CalculatedSize(FVolumeVersion);

  {Set Index}
  if not SetAttribute(Origin,Attribute) then Exit;

  Result:=True;
 finally
  FRecords.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.AddOwner(ASID:PSID):LongWord;
{Insert the Key for SID in the $O index of $Quota}
{Return is an OwnerId}
var
 Key:TNTFSDiskKey;
 Index:TNTFSDiskIndex;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
 OwnerId:LongWord;
begin
 {}
 Result:=0;

 if not FRecords.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ASID = nil then Exit;
  if FQuota = nil then Exit;
  if FQuota.Origin = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AddOwner - SID = ' + SIDToString(ASID));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get Origin}
  Origin:=FQuota.Origin.Origin;
  if Origin = nil then Exit;

  {Get Attribute}
  Attribute:=Origin.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameOwnerId,ntfsInstanceFirst);
  if Attribute = nil then Exit;

  {Get Index}
  Index:=Attribute.Index;
  if Index = nil then Exit;
  if Index.IndexType <> ntfsAttrTypeNone then Exit;
  if Index.CollateRule <> ntfsCollateTypeSID then Exit;

  {Get Key}
  Key:=Index.FindKey(ASID,Security.GetLengthSid(ASID));
  if Key <> nil then Exit;

  {Get Owner Id}
  OwnerId:=GetNextOwnerId;
  if OwnerId = 0 then Exit;

  {Add Key}
  Key:=Index.NewKey(ASID,@OwnerId,Security.GetLengthSid(ASID),SizeOf(LongWord));
  if Key = nil then Exit;
  try
   {Update Key}
   Key.EntrySize:=Key.CalculatedSize(FVolumeVersion);

   {Insert Key}
   if not Index.InsertKey(Key) then Exit;
   try
    {Set Index}
    if not SetAttribute(Origin,Attribute) then Exit;

    Result:=OwnerId;
   finally
    if (Result = 0) then Index.RemoveKey(Key);
    if (Result = 0) then Key:=nil;
   end;
  finally
   if (Result = 0) and (Key <> nil) then Index.DestroyKey(Key); {Dont double free the key}
  end;
 finally
  FRecords.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.RemoveOwner(ASID:PSID):Boolean;
{Remove the Key for SID in the $O index of $Quota}
var
 Key:TNTFSDiskKey;
 Index:TNTFSDiskIndex;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FRecords.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ASID = nil then Exit;
  if FQuota = nil then Exit;
  if FQuota.Origin = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.RemoveOwner - SID = ' + SIDToString(ASID));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get Origin}
  Origin:=FQuota.Origin.Origin;
  if Origin = nil then Exit;

  {Get Attribute}
  Attribute:=Origin.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameOwnerId,ntfsInstanceFirst);
  if Attribute = nil then Exit;

  {Get Index}
  Index:=Attribute.Index;
  if Index = nil then Exit;
  if Index.IndexType <> ntfsAttrTypeNone then Exit;
  if Index.CollateRule <> ntfsCollateTypeSID then Exit;

  {Get Key}
  Key:=Index.FindKey(ASID,Security.GetLengthSid(ASID));
  if Key = nil then Exit;
  if TNTFSDataKey(Key).Data = nil then Exit;

  {Remove Key}
  if not Index.RemoveKey(Key) then Exit;

  {Set Index}
  if not SetAttribute(Origin,Attribute) then Exit;

  Result:=True;
 finally
  FRecords.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.AddReparse(AReparseTag:LongWord;const AFileReference:Int64):Boolean;
{Insert the Key for Tag and Reference in the $R index of $Reparse}
var
 Key:TNTFSDiskKey;
 Index:TNTFSDiskIndex;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
 ReparseKey:TNTFSReparseKeyData;
begin
 {}
 Result:=False;

 if not FRecords.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FReparse = nil then Exit;
  if FReparse.Origin = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AddReparse - Tag = ' + IntToHex(AReparseTag,8) + ' Reference = ' + IntToHex(AFileReference,16));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get Origin}
  Origin:=FReparse.Origin.Origin;
  if Origin = nil then Exit;

  {Get Attribute}
  Attribute:=Origin.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameReparse,ntfsInstanceFirst);
  if Attribute = nil then Exit;

  {Get Index}
  Index:=Attribute.Index;
  if Index = nil then Exit;
  if Index.IndexType <> ntfsAttrTypeNone then Exit;
  if Index.CollateRule <> ntfsCollateTypeGUID then Exit;

  {Get Key}
  ReparseKey.ReparseTag:=AReparseTag;
  ReparseKey.FileReference:=AFileReference;

  {ReparseKey.Padding:=0;}
  Key:=Index.FindKey(@ReparseKey,ntfsReparseKeySize);
  if Key <> nil then Exit;

  {Add Key}
  Key:=Index.NewKey(@ReparseKey,nil,ntfsReparseKeySize,0);
  if Key = nil then Exit;
  try
   {Update Key}
   Key.EntrySize:=Key.CalculatedSize(FVolumeVersion);

   {Insert Key}
   if not Index.InsertKey(Key) then Exit;
   try
    {Set Index}
    if not SetAttribute(Origin,Attribute) then Exit;

    Result:=True;
   finally
    if not Result then Index.RemoveKey(Key);
    if not Result then Key:=nil;
   end;
  finally
   if not(Result) and (Key <> nil) then Index.DestroyKey(Key); {Dont double free the key}
  end;
 finally
  FRecords.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.RemoveReparse(AReparseTag:LongWord;const AFileReference:Int64):Boolean;
{Remove the Key for Tag and Reference in the $R index of $Reparse}
var
 Key:TNTFSDiskKey;
 Index:TNTFSDiskIndex;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
 ReparseKey:TNTFSReparseKeyData;
begin
 {}
 Result:=False;

 if not FRecords.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FReparse = nil then Exit;
  if FReparse.Origin = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.RemoveReparse - Tag = ' + IntToHex(AReparseTag,8) + ' Reference = ' + IntToHex(AFileReference,16));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get Origin}
  Origin:=FReparse.Origin.Origin;
  if Origin = nil then Exit;

  {Get Attribute}
  Attribute:=Origin.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameReparse,ntfsInstanceFirst);
  if Attribute = nil then Exit;

  {Get Index}
  Index:=Attribute.Index;
  if Index = nil then Exit;
  if Index.IndexType <> ntfsAttrTypeNone then Exit;
  if Index.CollateRule <> ntfsCollateTypeGUID then Exit;

  {Get Key}
  ReparseKey.ReparseTag:=AReparseTag;
  ReparseKey.FileReference:=AFileReference;
  {ReparseKey.Padding:=0;}
  Key:=Index.FindKey(@ReparseKey,ntfsReparseKeySize);
  if Key = nil then Exit;
  if Key.Key = nil then Exit;

  {Remove Key}
  if not Index.RemoveKey(Key) then Exit;

  {Set Index}
  if not SetAttribute(Origin,Attribute) then Exit;

  Result:=True;
 finally
  FRecords.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.AddSecurity(ASecurity:TNTFSSecurity):LongWord;
{Insert the Key for Security in the $SDH index of $Secure}
{Insert the Key for SecurityId in the $SII index of $Secure}
{Insert Security and SecurityId in the $SDS stream of $Secure}
{Return is a SecurityId}
var
 Key:TNTFSDiskKey;
 Index:TNTFSDiskIndex;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
 SecurityItem:TNTFSSecurityItem;

 Size:LongWord;
 Offset:LongWord;
 Buffer:Pointer;
 Instance:LongWord;

 StreamSize:Int64;
 SecurityId:LongWord;
 SecurityOffset:Int64;
 Descriptor:PNTFSSecurityData;
 SecurityIdData:TNTFSSecurityIdData;
 SecurityHashData:TNTFSSecurityHashData;
 SecurityHashKey:TNTFSSecurityHashKeyData;
begin
 {}
 Result:=ntfsSecurityIdUnknown;

 if not FRecords.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FSecure = nil then Exit;
  if ASecurity = nil then Exit;
  if FSecuritys = nil then Exit;
  if FSecure.Origin = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AddSecurity - SecurityHash = ' + IntToHex(ASecurity.SecurityHash,8));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get Security Id}
  SecurityId:=GetNextSecurityId;
  if SecurityId = ntfsSecurityIdUnknown then Exit;

  {Get Security Offset}
  {SecurityOffset:=GetNextSecurityOffset;} {Replaced}
  SecurityOffset:=GetNextSecurityOffsetEx(ASecurity.SecuritySize,StreamSize);
  if SecurityOffset = ntfsSecurityOffsetUnknown then Exit;
  if StreamSize = 0 then Exit;

  {Get Origin}
  Origin:=FSecure.Origin.Origin;
  if Origin = nil then Exit;

  {Get Descriptor}
  Descriptor:=ASecurity.SecurityDescriptor;
  if Descriptor = nil then Exit;
  try
   {Get Attribute (Secure:SDS)}
   Attribute:=Origin.GetAttribute(ntfsAttrTypeData,ntfsStreamNameSecurity,ntfsInstanceFirst);
   if Attribute = nil then Exit;

   {Get Security Item}
   SecurityItem:=FSecuritys.GetSecurityItem(SecurityId);
   if SecurityItem <> nil then Exit;

   {Add Security Item}
   SecurityItem:=FSecuritys.NewSecurityItem(SecurityId,SecurityOffset,ASecurity);
   if SecurityItem = nil then Exit;
   try
    {Size Attribute} {Round the current size to the next offset and add the unrounded size of the new item}
    {if not SizeAttribute(Origin,Attribute,NTFSRoundQuadWordTo16Bytes(Attribute.StreamSize) + SecurityItem.SecuritySize) then Exit;} {Replaced}
    if not SizeAttribute(Origin,Attribute,StreamSize) then Exit;

    {Create Buffer}
    Buffer:=AllocMem(Attribute.StreamSize);
    try
     {Write Buffer}
     Offset:=0;
     Size:=Attribute.StreamSize;
     if not FSecuritys.WriteSecurityItems(Buffer,Offset,Size,FVolumeVersion) then Exit;

     {Write Attribute}
     Instance:=ntfsInstanceFirst;
     if WriteAttribute(Origin,Attribute,Buffer^,0,Attribute.StreamSize,Instance,False) <> Attribute.StreamSize then Exit;
    finally
     FreeMem(Buffer);
    end;

    {Get Attribute (Secure:SII)}
    Attribute:=Origin.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameSecurityId,ntfsInstanceFirst);
    if Attribute = nil then Exit;

    {Get Index}
    Index:=Attribute.Index;
    if Index = nil then Exit;
    if Index.IndexType <> ntfsAttrTypeNone then Exit;
    if Index.CollateRule <> ntfsCollateTypeLongWord then Exit;

    {Get Key}
    Key:=Index.FindKey(@SecurityId,ntfsSecurityIdKeySize);
    if Key <> nil then Exit;

    {Add Key}
    FillChar(SecurityIdData,SizeOf(TNTFSSecurityIdData),0);
    SecurityIdData.SecurityHash:=SecurityItem.SecurityHash;
    SecurityIdData.SecurityId:=SecurityItem.SecurityId;
    SecurityIdData.SecurityOffset:=SecurityItem.SecurityOffset;
    SecurityIdData.SecuritySize:=SecurityItem.SecuritySize;

    Key:=Index.NewKey(@SecurityId,@SecurityIdData,ntfsSecurityIdKeySize,ntfsSecurityIdSize);
    if Key = nil then Exit;
    try
     {Update Key}
     Key.EntrySize:=Key.CalculatedSize(FVolumeVersion);

     {Insert Key}
     if not Index.InsertKey(Key) then Exit;
     try
      {Set Index}
      if not SetAttribute(Origin,Attribute) then Exit;

      Result:=SecurityId;
     finally
      if (Result = ntfsSecurityIdUnknown) then Index.RemoveKey(Key);
      if (Result = ntfsSecurityIdUnknown) then Key:=nil;
     end;
    finally
     if (Result = ntfsSecurityIdUnknown) and (Key <> nil) then Index.DestroyKey(Key); {Dont double free the key}
    end;

    {Reset Result}
    Result:=ntfsSecurityIdUnknown;

    {Get Attribute (Secure:SDH)}
    Attribute:=Origin.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameSecurityHash,ntfsInstanceFirst);
    if Attribute = nil then Exit;

    {Get Index}
    Index:=Attribute.Index;
    if Index = nil then Exit;
    if Index.IndexType <> ntfsAttrTypeNone then Exit;
    if Index.CollateRule <> ntfsCollateTypeSecurityHash then Exit;

    {Get Key}
    Key:=Index.FindKey(Descriptor,ASecurity.SecuritySize);
    if Key <> nil then Exit;

    {Add Key}
    FillChar(SecurityHashKey,SizeOf(TNTFSSecurityHashKeyData),0);
    SecurityHashKey.SecurityHash:=SecurityItem.SecurityHash;
    SecurityHashKey.SecurityId:=SecurityItem.SecurityId;
    FillChar(SecurityHashData,SizeOf(TNTFSSecurityHashData),0);
    SecurityHashData.SecurityHash:=SecurityItem.SecurityHash;
    SecurityHashData.SecurityId:=SecurityItem.SecurityId;
    SecurityHashData.SecurityOffset:=SecurityItem.SecurityOffset;
    SecurityHashData.SecuritySize:=SecurityItem.SecuritySize;
    {SecurityHashData.Padding:=ntfsSecurityHashPadding;}

    {Key:=Index.NewKey(@SecurityHashKey,@SecurityHashData,ntfsSecurityHashKeySize,ntfsSecurityHashSize);}
    Key:=Index.NewKeyEx(@SecurityHashKey,@SecurityHashData,@SecurityHashPadding,ntfsSecurityHashKeySize,ntfsSecurityHashSize,ntfsSecurityHashPaddingSize);
    if Key = nil then Exit;
    try
     {Update Key}
     Key.EntrySize:=Key.CalculatedSize(FVolumeVersion);

     {Insert Key}
     if not Index.InsertKey(Key) then Exit;
     try
      {Set Index}
      if not SetAttribute(Origin,Attribute) then Exit;

      Result:=SecurityId;

      {$IFDEF NTFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AddSecurity - Id = ' + IntToHex(SecurityItem.SecurityId,8));
      if FILESYS_LOG_ENABLED then FileSysLogDebug('                            Size = ' + IntToHex(SecurityItem.SecuritySize,8));
      if FILESYS_LOG_ENABLED then FileSysLogDebug('                            Hash = ' + IntToHex(SecurityItem.SecurityHash,8));
      if FILESYS_LOG_ENABLED then FileSysLogDebug('                          Offset = ' + IntToHex(SecurityItem.SecurityOffset,16));
      {$ENDIF}
     finally
      if (Result = ntfsSecurityIdUnknown) then Index.RemoveKey(Key);
      if (Result = ntfsSecurityIdUnknown) then Key:=nil;
     end;
    finally
     if (Result = ntfsSecurityIdUnknown) and (Key <> nil) then Index.DestroyKey(Key); {Dont double free the key}
    end;

    {if not SetRecords(Origin) then Exit;} {Set Records handled by Set Index}
   finally
    if (Result = ntfsSecurityIdUnknown) then FSecuritys.RemoveSecurityItem(SecurityItem,False); {Do not free the security}
   end;
  finally
   {Release Descriptor}
   ASecurity.ReleaseDescriptor(Descriptor,False,False);
  end;
 finally
  FRecords.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.RemoveSecurity(ASecurity:TNTFSSecurity):Boolean;
{Remove the Key for Security in the $SDH index of $Secure}
{Remove the Key for SecurityId in the $SII index of $Secure}
{Remove Security and SecurityId in the $SDS stream of $Secure (Leave a blank in its place)}
begin
 {}
 Result:=False;

 if not FRecords.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FSecure = nil then Exit;
  if ASecurity = nil then Exit;
  if FSecuritys = nil then Exit;
  if FSecure.Origin = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.RemoveSecurity - SecurityHash = ' + IntToHex(ASecurity.SecurityHash,8));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  //To Do //FSecuritys.DeleteSecurityItem
 finally
  FRecords.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.GetObjId(const AObjectId:TGUID):Int64;
{Find ObjectId from $O index in $ObjId}
{Return is a FileReference}
var
 Key:TNTFSDiskKey;
 Index:TNTFSDiskIndex;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=ntfsFileReferenceNone;

 if not FRecords.ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FObjId = nil then Exit;
  if FObjId.Origin = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetObjId - ObjectId = ' + GUIDToString(AObjectId));
  {$ENDIF}

  {Get Origin}
  Origin:=FObjId.Origin.Origin;
  if Origin = nil then Exit;

  {Get Attribute}
  Attribute:=Origin.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameObjectId,ntfsInstanceFirst);
  if Attribute = nil then Exit;

  {Get Index}
  Index:=Attribute.Index;
  if Index = nil then Exit;
  if Index.IndexType <> ntfsAttrTypeNone then Exit;
  if Index.CollateRule <> ntfsCollateTypeGUID then Exit;

  {Get Key}
  Key:=Index.FindKey(@AObjectId,ntfsObjIdKeySize);
  if Key = nil then Exit;
  if TNTFSDataKey(Key).Data = nil then Exit;

  {Get Reference}
  Result:=PNTFSObjIdData(TNTFSDataKey(Key).Data).FileReference;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetObjId - FileReference = ' + IntToHex(Result,16));
  {$ENDIF}
 finally
  FRecords.ReaderUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.GetQuota(AOwner:LongWord):PNTFSQuotaData;
{Find Owner from $Q index in $Quota}
var
 Key:TNTFSDiskKey;
 Index:TNTFSDiskIndex;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=nil;

 if not FRecords.ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FQuota = nil then Exit;
  if FQuota.Origin = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetQuota - Owner = ' + IntToHex(AOwner,8));
  {$ENDIF}

  {Get Origin}
  Origin:=FQuota.Origin.Origin;
  if Origin = nil then Exit;

  {Get Attribute}
  Attribute:=Origin.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameQuota,ntfsInstanceFirst);
  if Attribute = nil then Exit;

  {Get Index}
  Index:=Attribute.Index;
  if Index = nil then Exit;
  if Index.IndexType <> ntfsAttrTypeNone then Exit;
  if Index.CollateRule <> ntfsCollateTypeLongWord then Exit;

  {Get Key}
  Key:=Index.FindKey(@AOwner,ntfsQuotaKeySize);
  if Key = nil then Exit;
  if TNTFSDataKey(Key).Data = nil then Exit;

  {Get Quota}
  Result:=PNTFSQuotaData(TNTFSDataKey(Key).Data);

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetQuota - SID = ' + SIDToString(@Result.SID[0]));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                     BytesUsed = ' + IntToStr(Result.BytesUsed));
  {$ENDIF}
 finally
  FRecords.ReaderUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.GetOwner(ASID:PSID):LongWord;
{Find SID from $O index in $Quota}
{Return is an OwnerId}
var
 Key:TNTFSDiskKey;
 Index:TNTFSDiskIndex;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=0;

 if not FRecords.ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FQuota = nil then Exit;
  if FQuota.Origin = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetOwner - SID = ' + SIDToString(ASID));
  {$ENDIF}

  {Get Origin}
  Origin:=FQuota.Origin.Origin;
  if Origin = nil then Exit;

  {Get Attribute}
  Attribute:=Origin.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameOwnerId,ntfsInstanceFirst);
  if Attribute = nil then Exit;

  {Get Index}
  Index:=Attribute.Index;
  if Index = nil then Exit;
  if Index.IndexType <> ntfsAttrTypeNone then Exit;
  if Index.CollateRule <> ntfsCollateTypeSID then Exit;

  {Get Key}
  Key:=Index.FindKey(ASID,Security.GetLengthSid(ASID));
  if Key = nil then Exit;
  if TNTFSDataKey(Key).Data = nil then Exit;

  {Get Owner}
  Result:=PNTFSOwnerData(TNTFSDataKey(Key).Data).OwnerId;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetOwner - Owner = ' + IntToHex(Result,8));
  {$ENDIF}
 finally
  FRecords.ReaderUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.GetReparseByReference(AReparseTag:LongWord;const AFileReference:Int64):Int64;
{Find Tag and Reference from $R index in $Reparse}
{Return is a FileReference}
var
 Key:TNTFSDiskKey;
 Index:TNTFSDiskIndex;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
 ReparseKey:TNTFSReparseKeyData;
begin
 {}
 Result:=ntfsFileReferenceNone;

 if not FRecords.ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FReparse = nil then Exit;
  if FReparse.Origin = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetReparseByReference - Tag = ' + IntToHex(AReparseTag,8) + ' Reference = ' + IntToHex(AFileReference,16));
  {$ENDIF}

  {Get Origin}
  Origin:=FReparse.Origin.Origin;
  if Origin = nil then Exit;

  {Get Attribute}
  Attribute:=Origin.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameReparse,ntfsInstanceFirst);
  if Attribute = nil then Exit;

  {Get Index}
  Index:=Attribute.Index;
  if Index = nil then Exit;
  if Index.IndexType <> ntfsAttrTypeNone then Exit;
  if Index.CollateRule <> ntfsCollateTypeGUID then Exit;

  {Get Key}
  ReparseKey.ReparseTag:=AReparseTag;
  ReparseKey.FileReference:=AFileReference;
  {ReparseKey.Padding:=0;}

  Key:=Index.FindKey(@ReparseKey,ntfsReparseKeySize);
  if Key = nil then Exit;
  if Key.Key = nil then Exit;

  {Get Reparse}
  Result:=PNTFSReparseKeyData(Key.Key).FileReference;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetReparseByReference - FileReference = ' + IntToHex(Result,16));
  {$ENDIF}
 finally
  FRecords.ReaderUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.GetSecurityId(ASecurity:TNTFSSecurity;AWrite:Boolean):LongWord;
{Find Security from $SDH index in $Secure}
{Return is a SecurityId}
var
 Key:TNTFSDiskKey;
 Index:TNTFSDiskIndex;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
 Descriptor:PNTFSSecurityData;
begin
 {}
 Result:=ntfsSecurityIdUnknown;

 if AWrite then
  begin
   if not FRecords.WriterLock then Exit;
  end
 else
  begin
   if not FRecords.ReaderLock then Exit;
  end;
 try
  if FDriver = nil then Exit;
  if FSecure = nil then Exit;
  if ASecurity = nil then Exit;
  if FSecure.Origin = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetSecurityId - SecurityHash = ' + IntToHex(ASecurity.SecurityHash,8));
  {$ENDIF}

  {Get Origin}
  Origin:=FSecure.Origin.Origin;
  if Origin = nil then Exit;

  {Get Attribute}
  Attribute:=Origin.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameSecurityHash,ntfsInstanceFirst);
  if Attribute = nil then Exit;

  {Get Index}
  Index:=Attribute.Index;
  if Index = nil then Exit;
  if Index.IndexType <> ntfsAttrTypeNone then Exit;
  if Index.CollateRule <> ntfsCollateTypeSecurityHash then Exit;

  {Get Descriptor}
  Descriptor:=ASecurity.SecurityDescriptor;
  if Descriptor = nil then Exit;
  try
   {Get Key}
   Key:=Index.FindKey(Descriptor,ASecurity.SecuritySize);
   if Key = nil then Exit;
   if TNTFSDataKey(Key).Data = nil then Exit;

   {Get SecurityId}
   Result:=PNTFSSecurityHashData(TNTFSDataKey(Key).Data).SecurityId;

   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetSecurityId - SecurityId = ' + IntToHex(Result,8));
   {$ENDIF}
  finally
   {Release Descriptor}
   ASecurity.ReleaseDescriptor(Descriptor,False,False);
  end;
 finally
  if AWrite then
   begin
    FRecords.WriterUnlock;
   end
  else
   begin
    FRecords.ReaderUnlock;
   end;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.GetSecurityById(ASecurityId:LongWord):TNTFSSecurity;
{Find SecurityId from $SDS stream in $Secure}
{Find SecurityId from $SII index in $Secure} {Not Used}
{Note: Caller must never free the returned object}
var
 SecurityItem:TNTFSSecurityItem;
begin
 {}
 Result:=nil;

 if FDriver = nil then Exit;
 if FSecuritys = nil then Exit;

 if not FSecuritys.ReaderLock then Exit;
 try
  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetSecurityById - SecurityId = ' + IntToHex(ASecurityId,8));
  {$ENDIF}

  SecurityItem:=TNTFSSecurityItem(FSecuritys.First);
  while SecurityItem <> nil do
   begin
    if SecurityItem.SecurityId = ASecurityId then
     begin
      Result:=SecurityItem.Security;
      Break;
     end;
    SecurityItem:=TNTFSSecurityItem(SecurityItem.Next);
   end;

  {$IFDEF NTFS_DEBUG}
  if (FILESYS_LOG_ENABLED) and (Result <> nil) then FileSysLogDebug('TNTFSFileSystem.GetSecurityById - SecurityHash = ' + IntToHex(Result.SecurityHash,8));
  {$ENDIF}
 finally
  FSecuritys.ReaderUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.GetDescriptorId(ADescriptor:Pointer;AWrite:Boolean):LongWord;
{Find Descriptor from $SDH index in $Secure}
{Return is a SecurityId}
var
 Size:LongWord;
 Key:TNTFSDiskKey;
 Index:TNTFSDiskIndex;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=ntfsSecurityIdUnknown;

 if AWrite then
  begin
   if not FRecords.WriterLock then Exit;
  end
 else
  begin
   if not FRecords.ReaderLock then Exit;
  end;
 try
  if FDriver = nil then Exit;
  if FSecure = nil then Exit;
  if ADescriptor = nil then Exit;
  if FSecure.Origin = nil then Exit;

  {Get Size}
  Size:=Security.GetSecurityDescriptorLength(ADescriptor);
  if Size < ntfsSecuritySize then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetDescriptorId - SecurityHash = ' + IntToHex(NTFSGenerateSecurityHash(ADescriptor,Size),8));
  {$ENDIF}

  {Get Origin}
  Origin:=FSecure.Origin.Origin;
  if Origin = nil then Exit;

  {Get Attribute}
  Attribute:=Origin.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameSecurityHash,ntfsInstanceFirst);
  if Attribute = nil then Exit;

  {Get Index}
  Index:=Attribute.Index;
  if Index = nil then Exit;
  if Index.IndexType <> ntfsAttrTypeNone then Exit;
  if Index.CollateRule <> ntfsCollateTypeSecurityHash then Exit;

  {Get Key}
  Key:=Index.FindKey(ADescriptor,Size);
  if Key = nil then Exit;
  if TNTFSDataKey(Key).Data = nil then Exit;

  {Get SecurityId}
  Result:=PNTFSSecurityHashData(TNTFSDataKey(Key).Data).SecurityId;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetDescriptorId - SecurityId = ' + IntToHex(Result,8));
  {$ENDIF}
 finally
  if AWrite then
   begin
    FRecords.WriterUnlock;
   end
  else
   begin
    FRecords.ReaderUnlock;
   end;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.GetNextOwnerId:LongWord;
{Return is an OwnerId}
var
 Key:TNTFSDiskKey;
 Index:TNTFSDiskIndex;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=0;

 if not FRecords.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FQuota = nil then Exit;
  if FQuota.Origin = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetNextOwnerId');
  {$ENDIF}

  {Get Origin}
  Origin:=FQuota.Origin.Origin;
  if Origin = nil then Exit;

  {Get Attribute}
  Attribute:=Origin.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameOwnerId,ntfsInstanceFirst);
  if Attribute = nil then Exit;

  {Get Index}
  Index:=Attribute.Index;
  if Index = nil then Exit;
  if Index.IndexType <> ntfsAttrTypeNone then Exit;
  if Index.CollateRule <> ntfsCollateTypeSID then Exit;

  {Get Last}
  Key:=TNTFSDiskKey(Index.Last);
  if Key = nil then
   begin
    {Return First}
    Result:=ntfsOwnerIdFirst;
   end
  else
   begin
    if TNTFSDataKey(Key).Data = nil then Exit;

    {Get Owner Id}
    Result:=PNTFSOwnerData(TNTFSDataKey(Key).Data).OwnerId;

    {Update Owner Id}
    Inc(Result);
   end;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetNextOwnerId - Result = ' + IntToHex(Result,8));
  {$ENDIF}
 finally
  FRecords.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.GetNextSecurityId:LongWord;
{Return is a SecurityId}
var
 Key:TNTFSDiskKey;
 Index:TNTFSDiskIndex;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=ntfsSecurityIdUnknown;

 if not FRecords.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FSecure = nil then Exit;
  if FSecure.Origin = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetNextSecurityId');
  {$ENDIF}

  {Get Origin}
  Origin:=FSecure.Origin.Origin;
  if Origin = nil then Exit;

  {Get Attribute}
  Attribute:=Origin.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameSecurityId,ntfsInstanceFirst);
  if Attribute = nil then Exit;

  {Get Index}
  Index:=Attribute.Index;
  if Index = nil then Exit;
  if Index.IndexType <> ntfsAttrTypeNone then Exit;
  if Index.CollateRule <> ntfsCollateTypeLongWord then Exit;

  {Get Last}
  Key:=TNTFSDiskKey(Index.Last);
  if Key = nil then
   begin
    {Return First}
    Result:=ntfsSecurityIdFirst;
   end
  else
   begin
    if Key.Key = nil then Exit;

    {Get Security Id}
    Result:=PNTFSSecurityIdKeyData(Key.Key).SecurityId;

    {Update Security Id}
    Inc(Result);
   end;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetNextSecurityId - Result = ' + IntToHex(Result,8));
  {$ENDIF}
 finally
  FRecords.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.GetNextSecurityOffset:Int64;
{Return is an Offset}
{Layout of $SDS file}
{ First 256K is the first block of records}
{ Second 256K is the mirror records of first block}
{ Third 256K is the second block of records}
{ Forth 256K is the mirror records of second block etc}
{This function has been replaced by GetNextSecurityOffsetEx below}
var
 Key:TNTFSDiskKey;
 Index:TNTFSDiskIndex;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=ntfsSecurityOffsetUnknown;

 if not FRecords.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FSecure = nil then Exit;
  if FSecure.Origin = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetNextSecurityOffset');
  {$ENDIF}

  {Get Origin}
  Origin:=FSecure.Origin.Origin;
  if Origin = nil then Exit;

  {Get Attribute (Secure:SII)}
  Attribute:=Origin.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameSecurityId,ntfsInstanceFirst);
  if Attribute = nil then Exit;

  {Get Index}
  Index:=Attribute.Index;
  if Index = nil then Exit;
  if Index.IndexType <> ntfsAttrTypeNone then Exit;
  if Index.CollateRule <> ntfsCollateTypeLongWord then Exit;

  {Get Last}
  Key:=TNTFSDiskKey(Index.Last);
  if Key = nil then
   begin
    {Return First Offset}
    Result:=ntfsSecurityOffsetFirst;
   end
  else
   begin
    if TNTFSDataKey(Key).Data = nil then Exit;

    {Get Security Offset}
    Result:=PNTFSSecurityIdData(TNTFSDataKey(Key).Data).SecurityOffset + PNTFSSecurityIdData(TNTFSDataKey(Key).Data).SecuritySize;

    {Update Security Offset}
    Result:=NTFSRoundQuadWordTo16Bytes(Result);
   end;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetNextSecurityOffset - Result = ' + IntToHex(Result,16));
  {$ENDIF}
 finally
  FRecords.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.GetNextSecurityOffsetEx(ASecuritySize:LongWord;var AStreamSize:Int64):Int64;
{Return is an Offset}
{Layout of $SDS file}
{ First 256K is the first block of records}
{ Second 256K is the mirror records of first block}
{ Third 256K is the second block of records}
{ Forth 256K is the mirror reocrds of second block etc}
{Note: This needs to be enhanced to reuse existing Security Items that are blank - Return and Existing parameter}
var
 Size:LongWord;
 Offset:LongWord;
 Section:LongWord;
 Current:LongWord;

 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
 SecurityItem:TNTFSSecurityItem;
begin
 {}
 Result:=ntfsSecurityOffsetUnknown;

 AStreamSize:=0;

 if not FRecords.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FSecure = nil then Exit;
  if FSecuritys = nil then Exit;
  if FSecure.Origin = nil then Exit;

  if not FSecuritys.ReaderLock then Exit;
  try
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetNextSecurityOffsetEx - SecuritySize = ' + IntToStr(ASecuritySize));
   {$ENDIF}

   {Check Size}
   if ASecuritySize < ntfsSecuritySize then Exit;
   ASecuritySize:=ASecuritySize + (ntfsSecurityItemSize - ntfsSecuritySize);

   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('                                          AdjustedSize = ' + IntToStr(ASecuritySize));
   {$ENDIF}

   {Get Origin}
   Origin:=FSecure.Origin.Origin;
   if Origin = nil then Exit;

   {Get Attribute (Secure:SDS)}
   Attribute:=Origin.GetAttribute(ntfsAttrTypeData,ntfsStreamNameSecurity,ntfsInstanceFirst);
   if Attribute = nil then Exit;

   {Get Last Security Item}
   SecurityItem:=TNTFSSecurityItem(FSecuritys.Last);
   if SecurityItem = nil then
    begin
     {Return First Offset}
     Result:=ntfsSecurityOffsetFirst;
     AStreamSize:=ntfsSecurityMirrorOffset + ASecuritySize;
    end
   else
    begin
     {Check Size}
     if SecurityItem.SecuritySize = 0 then Exit;

     {Get Security Offset}
     Current:=SecurityItem.SecurityOffset + SecurityItem.SecuritySize; {Assumes Security Offset will never exceed 4GB}

     {Update Security Offset}
     Current:=NTFSRoundLongWordTo16Bytes(Current);

     {$IFDEF NTFS_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('                                               Current = ' + IntToHex(Current,8));
     {$ENDIF}

     {Check Offset}
     Offset:=(Current and ntfsSecurityOffsetMask);   {Get the offset within this Section}
     Section:=(Current and ntfsSecuritySectionMask); {Get the starting offset of this Section}
     if (Offset = 0) and ((Section and ntfsSecurityMirrorTest) = ntfsSecurityMirrorTest) then
      begin
       {Update Offset}
       Size:=(ntfsSecuritySectionOffset - Offset);  {Get the bytes remaining before the start of the next Section}

       {$IFDEF NTFS_DEBUG}
       if FILESYS_LOG_ENABLED then FileSysLogDebug('                                       Size (Boundary) = ' + IntToHex(Size,8));
       {$ENDIF}

       {Return Next Offset} {New Section}
       Result:=Current + Size;

       {Return Stream Size} {Round the current size to the next offset and add the unrounded size of the new item}
       AStreamSize:=Section + ntfsSecurityMirrorOffset + ASecuritySize; //To Do //Confirm this is correct
      end
     else
      begin
       {Update Offset}
       Size:=(ntfsSecuritySectionOffset - Offset); {Get the bytes remaining before the end of this Section}

       {$IFDEF NTFS_DEBUG}
       if FILESYS_LOG_ENABLED then FileSysLogDebug('                                          Size (Below) = ' + IntToHex(Size,8));
       {$ENDIF}

       {Check Size}
       if Size >= ASecuritySize then
        begin
         {Return Next Offset} {Current Section}
         Result:=Current;

         {Return Stream Size} {Round the current size to the next offset and add the unrounded size of the new item}
         AStreamSize:=Current + ntfsSecurityMirrorOffset + ASecuritySize;
        end
       else
        begin
         {Return Next Offset} {New Section}
         Result:=Current + Size + ntfsSecuritySectionOffset;

         {Return Stream Size} {Round the current size to the next offset and add the unrounded size of the new item}
         AStreamSize:=Section + ntfsSecuritySectionOffset + ntfsSecurityMirrorOffset + ASecuritySize;
        end;
      end;
    end;

   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetNextSecurityOffsetEx - Result = ' + IntToHex(Result,16) + ' StreamSize = ' + IntToHex(AStreamSize,16));
   {$ENDIF}
  finally
   FSecuritys.ReaderUnlock;
  end;
 finally
  FRecords.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.GetNextMoveAttribute(ARecord:TNTFSDiskRecord):TNTFSDiskAttribute;
{Get the next movable attribute according to status}
{Note: Caller must hold the records lock}
var
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=nil;

 if not FRecords.AttributesWriterLock then Exit;
 try
  if ARecord = nil then Exit;
  if ARecord.Attributes = nil then Exit;

  {Get Attribute}
  Attribute:=TNTFSDiskAttribute(ARecord.Attributes.First);
  while Attribute <> nil do
   begin
    {Check Unmovable}
    if not(Attribute.IsUnmovable) then
     begin
      {Check MFT}
      if (ARecord = FMaster) then
       begin
        {Check Data and Start VCN (First instance of MFT Data cannot be moved)}
        if (Attribute.AttributeType <> ntfsAttrTypeData) or (Attribute.StartVCN > 0) then
         begin
          Result:=Attribute;
          Exit;
         end;
       end
      else
       begin
        Result:=Attribute;
        Exit;
       end;
     end;

    {Get Attribute}
    Attribute:=TNTFSDiskAttribute(Attribute.Next);
   end;
 finally
  FRecords.AttributesWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.GetNextSplitAttribute(ARecord:TNTFSDiskRecord):TNTFSDiskAttribute;
{Get the next splitable attribute according to status and run count}
{Note: Caller must hold the records lock}
var
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=nil;

 if not FRecords.AttributesWriterLock then Exit;
 try
  if ARecord = nil then Exit;
  if ARecord.Attributes = nil then Exit;

  {Get Attribute}
  Attribute:=TNTFSDiskAttribute(ARecord.Attributes.First);
  while Attribute <> nil do
   begin
    {Check Non Resident}
    if Attribute.NonResident = ntfsAttributeNonResident then
     begin
      {Check Single}
      if not(Attribute.IsSingle) then
       begin
        if Attribute.RunCount > 2 then
         begin
          Result:=Attribute;
          Exit;
         end;
       end;
     end;

    {Get Attribute}
    Attribute:=TNTFSDiskAttribute(Attribute.Next);
   end;
 finally
  FRecords.AttributesWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.GetNextConvertAttribute(ARecord:TNTFSDiskRecord):TNTFSDiskAttribute;
{Get the next convertable attribute according to attribute definitions}
{Note: Caller must hold the records lock}
var
 AttrDef:TNTFSAttrDef;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=nil;

 if not FRecords.AttributesWriterLock then Exit;
 try
  if ARecord = nil then Exit;
  if FAttrDefs = nil then Exit;
  if ARecord.Attributes = nil then Exit;

  {Get Attribute}
  Attribute:=TNTFSDiskAttribute(ARecord.Attributes.First);
  while Attribute <> nil do
   begin
    {Check Resident}
    if Attribute.NonResident = ntfsAttributeResident then
     begin
      {Check DataSize}
      if Attribute.DataSize > (ntfsNonResidentHeaderSize - ntfsResidentHeaderSize) then
       begin
        {Get AttrDef}
        AttrDef:=FAttrDefs.GetAttrDef(Attribute.AttributeType,ntfsAnyName);
        if AttrDef <> nil then
         begin
          {Check Resident}
          if not(AttrDef.IsResident) then
           begin
            {Check Secure}
            if (FSecure <> nil) and (ARecord = FSecure.Origin) then
             begin
              {Check List (Secure List cannot be converted)}
              if Attribute.AttributeType <> ntfsAttrTypeAttributeList then
               begin
                Result:=Attribute;
                Exit;
               end;
             end
            else
             begin
              Result:=Attribute;
              Exit;
             end;
           end;
         end;
       end;
     end;

    {Get Attribute}
    Attribute:=TNTFSDiskAttribute(Attribute.Next);
   end;
 finally
  FRecords.AttributesWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.CreateObjIds:Boolean;
{Create the default $ObjId:$O index entries}
var
 ObjIdData:TNTFSObjIdData;

 Origin:TNTFSDiskRecord;
 Current:TNTFSDiskRecord;

 Key:TNTFSDiskKey;
 Index:TNTFSDiskIndex;
 Root:TNTFSDiskAttribute;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FRecords.WriterLock then Exit;
 try
  if FDriver = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CreateObjIds');
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get ObjId}
  Current:=GetRecordEx(nil,ntfsObjIdRecordNumber,False,True);
  if Current = nil then Exit;

  {Get Origin}
  Origin:=Current.Origin;
  if Origin = nil then Exit;

  {Get Index Root}
  Root:=Origin.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameObjectId,ntfsInstanceFirst);
  if Root = nil then Exit;

  {Get Volume}
  Current:=GetRecordEx(nil,ntfsFileTypeVolume,False,True);
  if Current = nil then Exit;

  {Get Attribute}
  Attribute:=Current.GetAttribute(ntfsAttrTypeObjectId,ntfsBlankName,ntfsInstanceFirst);
  if Attribute = nil then Exit;

  {Get Index}
  Index:=Root.Index;
  if Index = nil then Exit;
  if Index.IndexType <> ntfsAttrTypeNone then Exit;
  if Index.CollateRule <> ntfsCollateTypeGUID then Exit;

  {Get Key}
  Key:=Index.FindKey(@TNTFSObjectIdAttribute(Attribute).ObjectId,ntfsObjIdKeySize);
  if Key <> nil then Exit;

  {Add Key}
  FillChar(ObjIdData,SizeOf(TNTFSObjIdData),0);
  ObjIdData.FileReference:=ntfsVolumeFileReference;
  Key:=Index.NewKey(@TNTFSObjectIdAttribute(Attribute).ObjectId,@ObjIdData,ntfsObjIdKeySize,ntfsObjIdSize);
  if Key = nil then Exit;
  try
   {Update Key}
   Key.EntrySize:=Key.CalculatedSize(FVolumeVersion);

   {Insert Key}
   if not Index.InsertKey(Key) then Exit;
   try
    {Set Index}
    if not SetAttribute(Origin,Root) then Exit;

    Result:=True;
   finally
    if not Result then Index.RemoveKey(Key);
    if not Result then Key:=nil;
   end;
  finally
   if not(Result) and (Key <> nil) then Index.DestroyKey(Key); {Dont double free the key}
  end;
 finally
  FRecords.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.CreateOwners:Boolean;
{Create the default $Quota:$O index entries}
var
 Sid:PSID;
 OwnerId:LongWord;

 Origin:TNTFSDiskRecord;
 Current:TNTFSDiskRecord;

 Key:TNTFSDiskKey;
 Index:TNTFSDiskIndex;
 Root:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FRecords.WriterLock then Exit;
 try
  if FDriver = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CreateOwners');
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get Quota}
  Current:=GetRecordEx(nil,ntfsQuotaRecordNumber,False,True);
  if Current = nil then Exit;

  {Get Origin}
  Origin:=Current.Origin;
  if Origin = nil then Exit;

  {Get Index Root}
  Root:=Origin.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameOwnerId,ntfsInstanceFirst);
  if Root = nil then Exit;

  {Get Index}
  Index:=Root.Index;
  if Index = nil then Exit;
  if Index.IndexType <> ntfsAttrTypeNone then Exit;
  if Index.CollateRule <> ntfsCollateTypeSID then Exit;

  {Get Key}
  Sid:=nil;
  if not NTFSCreateDefaultSid(ntfsDefaultSid100,Pointer(Sid),FVolumeVersion) then Exit;
  try
   Key:=Index.FindKey(Sid,Security.GetLengthSid(Sid));
   if Key <> nil then Exit;

   {Add Key}
   OwnerId:=ntfsDefaultOwnerId100;
   Key:=Index.NewKey(Sid,@OwnerId,Security.GetLengthSid(Sid),ntfsOwnerSize);
   if Key = nil then Exit;
   try
    {Update Key}
    Key.EntrySize:=Key.CalculatedSize(FVolumeVersion);

    {Insert Key}
    if not Index.InsertKey(Key) then Exit;
    try
     {Set Index}
     if not SetAttribute(Origin,Root) then Exit;

     Result:=True;
    finally
     if not Result then Index.RemoveKey(Key);
     if not Result then Key:=nil;
    end;
   finally
    if not(Result) and (Key <> nil) then Index.DestroyKey(Key); {Dont double free the key}
   end;
  finally
   NTFSDestroyDefaultSid(Sid,FVolumeVersion);
  end;
 finally
  FRecords.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.CreateQuotas:Boolean;
{Create the default $Quota:$Q index entries}
var
 Sid:PSID;
 SidSize:LongWord;
 OwnerId:LongWord;
 QuotaData:PNTFSQuotaData;

 Origin:TNTFSDiskRecord;
 Current:TNTFSDiskRecord;

 Key:TNTFSDiskKey;
 Index:TNTFSDiskIndex;
 Root:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FRecords.WriterLock then Exit;
 try
  if FDriver = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CreateQuotas');
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get Quota}
  Current:=GetRecordEx(nil,ntfsQuotaRecordNumber,False,True);
  if Current = nil then Exit;

  {Get Origin}
  Origin:=Current.Origin;
  if Origin = nil then Exit;

  {Get Index Root}
  Root:=Origin.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameQuota,ntfsInstanceFirst);
  if Root = nil then Exit;

  {Get Index}
  Index:=Root.Index;
  if Index = nil then Exit;
  if Index.IndexType <> ntfsAttrTypeNone then Exit;
  if Index.CollateRule <> ntfsCollateTypeLongWord then Exit;

  {Get Key (1)}
  OwnerId:=ntfsDefaultOwnerId1;
  Key:=Index.FindKey(@OwnerId,ntfsQuotaKeySize);
  if Key <> nil then Exit;

  {Add Key (1)}
  OwnerId:=ntfsDefaultOwnerId1;
  QuotaData:=AllocMem(ntfsQuotaSize);
  try
   QuotaData.Version:=2;
   QuotaData.Flags:=ntfsQuotaFlagDefaultLimits;
   QuotaData.BytesUsed:=0;
   QuotaData.ChangeTime:=Ultibo.DateTimeToFileTime(Now); {Converted to UTC}
   QuotaData.WarningLimit:=-1;
   QuotaData.HardLimit:=-1;
   QuotaData.ExceedTime.dwLowDateTime:=0;
   QuotaData.ExceedTime.dwHighDateTime:=0;

   Key:=Index.NewKey(@OwnerId,QuotaData,ntfsQuotaKeySize,ntfsQuotaSize);
   if Key = nil then Exit;
   try
    {Update Key}
    Key.EntrySize:=Key.CalculatedSize(FVolumeVersion);

    {Insert Key}
    if not Index.InsertKey(Key) then Exit;
    try
     {Set Index}
     if not SetAttribute(Origin,Root) then Exit;

     Result:=True;
    finally
     if not Result then Index.RemoveKey(Key);
     if not Result then Key:=nil;
    end;
   finally
    if not(Result) and (Key <> nil) then Index.DestroyKey(Key); {Dont double free the key}
   end;
  finally
   FreeMem(QuotaData);
  end;

  {Reset Result}
  Result:=False;

  {Get Key (100)}
  OwnerId:=ntfsDefaultOwnerId100;
  Key:=Index.FindKey(@OwnerId,ntfsQuotaKeySize);
  if Key <> nil then Exit;

  {Add Key (100)}
  Sid:=nil;
  if not NTFSCreateDefaultSid(ntfsDefaultSid100,Pointer(Sid),FVolumeVersion) then Exit;
  try
   OwnerId:=ntfsDefaultOwnerId100;
   SidSize:=Security.GetLengthSid(Sid);

   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CreateQuotas SidSize = ' + IntToStr(SidSize));
   {$ENDIF}

   QuotaData:=AllocMem(ntfsQuotaSize + SidSize);
   try
    QuotaData.Version:=2;
    QuotaData.Flags:=ntfsQuotaFlagDefaultLimits;
    QuotaData.BytesUsed:=0;
    QuotaData.ChangeTime:=Ultibo.DateTimeToFileTime(Now); {Converted to UTC}
    QuotaData.WarningLimit:=-1;
    QuotaData.HardLimit:=-1;
    QuotaData.ExceedTime.dwLowDateTime:=0;
    QuotaData.ExceedTime.dwHighDateTime:=0;
    if not Security.CopySid(SidSize,@QuotaData.SID,Sid) then Exit;

    Key:=Index.NewKey(@OwnerId,QuotaData,ntfsQuotaKeySize,ntfsQuotaSize + SidSize);
    if Key = nil then Exit;
    try
     {Update Key}
     Key.EntrySize:=Key.CalculatedSize(FVolumeVersion);

     {Insert Key}
     if not Index.InsertKey(Key) then Exit;
     try
      {Set Index}
      if not SetAttribute(Origin,Root) then Exit;

      Result:=True;
     finally
      if not Result then Index.RemoveKey(Key);
      if not Result then Key:=nil;
     end;
    finally
     if not(Result) and (Key <> nil) then Index.DestroyKey(Key); {Dont double free the key}
    end;
   finally
    FreeMem(QuotaData);
   end;
 finally
  NTFSDestroyDefaultSid(Sid,FVolumeVersion);
 end;
 finally
  FRecords.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.CreateReparses:Boolean;
{Create the default $Reparse:$R index entries}
var
 Origin:TNTFSDiskRecord;
 Current:TNTFSDiskRecord;

 Index:TNTFSDiskIndex;
 Root:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FRecords.WriterLock then Exit;
 try
  if FDriver = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CreateReparses');
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get Quota}
  Current:=GetRecordEx(nil,ntfsReparseRecordNumber,False,True);
  if Current = nil then Exit;

  {Get Origin}
  Origin:=Current.Origin;
  if Origin = nil then Exit;

  {Get Index Root}
  Root:=Origin.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameReparse,ntfsInstanceFirst);
  if Root = nil then Exit;

  {Get Index}
  Index:=Root.Index;
  if Index = nil then Exit;
  if Index.IndexType <> ntfsAttrTypeNone then Exit;
  if Index.CollateRule <> ntfsCollateTypeGUID then Exit;

  {Nothing}

  Result:=True;
 finally
  FRecords.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.CreateFileNames:Boolean;
{Create the default $I30 index entries (Root and $Extend)}
var
 Origin:TNTFSDiskRecord;
 Current:TNTFSDiskRecord;

 Root:TNTFSDiskAttribute;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FRecords.WriterLock then Exit;
 try
  if FDriver = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CreateFileNames');
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get Root}
  Current:=GetRecordEx(nil,ntfsFileTypeRoot,False,True);
  if Current = nil then Exit;

  {Get Origin (Root)}
  Origin:=Current.Origin;
  if Origin = nil then Exit;

  {Add Files in Root}
  {MFT}
  Current:=GetRecordEx(nil,ntfsFileTypeMft,False,True);
  if Current = nil then Exit;
  Attribute:=Current.GetAttribute(ntfsAttrTypeFileName,ntfsBlankName,ntfsInstanceFirst);
  if Attribute = nil then Exit;
  if not AddKey(Origin,Attribute) then Exit;

  {MFTMirr}
  Current:=GetRecordEx(nil,ntfsFileTypeMftMirr,False,True);
  if Current = nil then Exit;
  Attribute:=Current.GetAttribute(ntfsAttrTypeFileName,ntfsBlankName,ntfsInstanceFirst);
  if Attribute = nil then Exit;
  if not AddKey(Origin,Attribute) then Exit;

  {LogFile}
  Current:=GetRecordEx(nil,ntfsFileTypeLogFile,False,True);
  if Current = nil then Exit;
  Attribute:=Current.GetAttribute(ntfsAttrTypeFileName,ntfsBlankName,ntfsInstanceFirst);
  if Attribute = nil then Exit;
  if not AddKey(Origin,Attribute) then Exit;

  {Volume}
  Current:=GetRecordEx(nil,ntfsFileTypeVolume,False,True);
  if Current = nil then Exit;
  Attribute:=Current.GetAttribute(ntfsAttrTypeFileName,ntfsBlankName,ntfsInstanceFirst);
  if Attribute = nil then Exit;
  if not AddKey(Origin,Attribute) then Exit;

  {AttrDef}
  Current:=GetRecordEx(nil,ntfsFileTypeAttrDef,False,True);
  if Current = nil then Exit;
  Attribute:=Current.GetAttribute(ntfsAttrTypeFileName,ntfsBlankName,ntfsInstanceFirst);
  if Attribute = nil then Exit;
  if not AddKey(Origin,Attribute) then Exit;

  {Root}
  Current:=GetRecordEx(nil,ntfsFileTypeRoot,False,True);
  if Current = nil then Exit;
  Attribute:=Current.GetAttribute(ntfsAttrTypeFileName,ntfsBlankName,ntfsInstanceFirst);
  if Attribute = nil then Exit;
  if not AddKey(Origin,Attribute) then Exit;

  {Bitmap}
  Current:=GetRecordEx(nil,ntfsFileTypeBitmap,False,True);
  if Current = nil then Exit;
  Attribute:=Current.GetAttribute(ntfsAttrTypeFileName,ntfsBlankName,ntfsInstanceFirst);
  if Attribute = nil then Exit;
  if not AddKey(Origin,Attribute) then Exit;

  {Boot}
  Current:=GetRecordEx(nil,ntfsFileTypeBoot,False,True);
  if Current = nil then Exit;
  Attribute:=Current.GetAttribute(ntfsAttrTypeFileName,ntfsBlankName,ntfsInstanceFirst);
  if Attribute = nil then Exit;
  if not AddKey(Origin,Attribute) then Exit;

  {BadClus}
  Current:=GetRecordEx(nil,ntfsFileTypeBadClus,False,True);
  if Current = nil then Exit;
  Attribute:=Current.GetAttribute(ntfsAttrTypeFileName,ntfsBlankName,ntfsInstanceFirst);
  if Attribute = nil then Exit;
  if not AddKey(Origin,Attribute) then Exit;

  {Secure}
  Current:=GetRecordEx(nil,ntfsFileTypeSecure,False,True); {Also ntfs12FileTypeQuota}
  if Current = nil then Exit;
  Attribute:=Current.GetAttribute(ntfsAttrTypeFileName,ntfsBlankName,ntfsInstanceFirst);
  if Attribute = nil then Exit;
  if not AddKey(Origin,Attribute) then Exit;

  {UpCase}
  Current:=GetRecordEx(nil,ntfsFileTypeUpCase,False,True);
  if Current = nil then Exit;
  Attribute:=Current.GetAttribute(ntfsAttrTypeFileName,ntfsBlankName,ntfsInstanceFirst);
  if Attribute = nil then Exit;
  if not AddKey(Origin,Attribute) then Exit;

  {Check Version}
  if FNTFSType <> ntNTFS12 then
   begin
    {Extend}
    Current:=GetRecordEx(nil,ntfsFileTypeExtend,False,True);
    if Current = nil then Exit;
    Attribute:=Current.GetAttribute(ntfsAttrTypeFileName,ntfsBlankName,ntfsInstanceFirst);
    if Attribute = nil then Exit;
    if not AddKey(Origin,Attribute) then Exit;

    {Get Index Root}
    Root:=Origin.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameFileName,ntfsInstanceFirst);
    {Set Index}
    if not SetAttribute(Origin,Root) then Exit;

    {Get Origin (Extend)}
    Origin:=Current.Origin;
    if Origin = nil then Exit;

    {Add Files in Extend}
    {ObjId}
    Current:=GetRecordEx(nil,ntfsObjIdRecordNumber,False,True);
    if Current = nil then Exit;
    Attribute:=Current.GetAttribute(ntfsAttrTypeFileName,ntfsBlankName,ntfsInstanceFirst);
    if Attribute = nil then Exit;
    if not AddKey(Origin,Attribute) then Exit;

    {Quota}
    Current:=GetRecordEx(nil,ntfsQuotaRecordNumber,False,True);
    if Current = nil then Exit;
    Attribute:=Current.GetAttribute(ntfsAttrTypeFileName,ntfsBlankName,ntfsInstanceFirst);
    if Attribute = nil then Exit;
    if not AddKey(Origin,Attribute) then Exit;

    {Reparse}
    Current:=GetRecordEx(nil,ntfsReparseRecordNumber,False,True);
    if Current = nil then Exit;
    Attribute:=Current.GetAttribute(ntfsAttrTypeFileName,ntfsBlankName,ntfsInstanceFirst);
    if Attribute = nil then Exit;
    if not AddKey(Origin,Attribute) then Exit;

    {Get Index Root}
    Root:=Origin.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameFileName,ntfsInstanceFirst);

    {Set Index}
    if not SetAttribute(Origin,Root) then Exit;

    Result:=True;
   end
  else
   begin
    {Get Index Root}
    Root:=Origin.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameFileName,ntfsInstanceFirst);

    {Set Index}
    if not SetAttribute(Origin,Root) then Exit;

    Result:=True;
   end;
 finally
  FRecords.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.CreateBoots:Boolean;
{Create the $Boot file data}
var
 Buffer:Pointer;
 Instance:LongWord;
 NumberOfHeads:Word;
 SectorsPerTrack:Word;
 BootSector:PNtfsBootSector;

 Current:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FRecords.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FVolume = nil then Exit;
  if FClusterSize = 0 then Exit;
  if FVolume.Device = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CreateBoots');
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get Boot}
  Current:=GetRecordEx(nil,ntfsFileTypeBoot,False,True);
  if Current = nil then Exit;

  {Get Attribute}
  Attribute:=Current.GetAttribute(ntfsAttrTypeData,ntfsBlankName,ntfsInstanceFirst);
  if Attribute = nil then Exit;

  {Check Size}
  if Attribute.StreamSize <> CalculateBootSize then Exit;

  {Create Boot File}
  Buffer:=AllocMem(Attribute.StreamSize);
  try
   {Get Heads and Sectors}
   NumberOfHeads:=FVolume.Device.LogicalHeads;
   SectorsPerTrack:=FVolume.Device.LogicalSectors;

   {Create Boot Sector}
   BootSector:=PNtfsBootSector(Buffer);
   BootSector.BootJump:=ntfsBootJump;
   FillChar(BootSector.OEMName[0],8,ntfsEntryPadding);
   System.Move(ntfsOemName[1],BootSector.OEMName[0],Min(Length(ntfsOemName),8));
   BootSector.BPB.BytesPerSector:=FSectorSize;
   BootSector.BPB.SectorsPerCluster:=FSectorsPerCluster;
   BootSector.BPB.ReservedSectors:=0;
   BootSector.BPB.Reserved2:=0;
   BootSector.BPB.MediaId:=$F8;
   BootSector.BPB.Reserved3:=0;
   BootSector.BPB.SectorsPerTrack:=SectorsPerTrack;
   BootSector.BPB.NumberOfHeads:=NumberOfHeads;
   BootSector.BPB.HiddenSectors:=FStartSector;
   BootSector.BPB.Reserved4:=0;
   BootSector.BPB.Reserved5:=$00800080;
   BootSector.BPB.TotalSectors:=FSectorCount;
   BootSector.BPB.MFTCluster:=FMftStartCluster;
   BootSector.BPB.MFTMirror:=FMFtMirrorCluster;
   BootSector.BPB.ClustersPerFile:=CalculateBpbClustersPerFile;
   BootSector.BPB.ClustersPerIndex:=CalculateBpbClustersPerIndex;
   Int64Rec(BootSector.BPB.VolumeSerial).Lo:=DateTimeToFileDate(Now);
   Int64Rec(BootSector.BPB.VolumeSerial).Hi:=Int64Rec(BootSector.BPB.VolumeSerial).Lo;
   BootSector.BPB.Checksum:=0;
   BootSector.BootCode:=ntfsBootCode;
   BootSector.Signature:=BOOT_SECTOR_SIGNATURE;

   {Write Attribute}
   Instance:=ntfsInstanceFirst;
   if WriteAttribute(Current,Attribute,Buffer^,0,Attribute.StreamSize,Instance,False) <> Attribute.StreamSize then Exit;

   {Write Backup Boot}
   if FBootBackup <> FBootSector then
    begin
     {Write Sectors}
     if not WriteSectors(FBootBackup,1,Buffer^) then Exit;
    end;

   Result:=True;
  finally
   FreeMem(Buffer);
  end;
 finally
  FRecords.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.CreateBitmaps:Boolean;
{Create the $Bitmap blocks and mark the used clusters}
var
 Count:Int64;
 Cluster:Int64;
begin
 {}
 Result:=False;

 if not FRecords.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FClusterSize = 0 then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CreateBitmaps');
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Create Blocks}
  if not CreateBlocks then Exit;

  {Mark Clusters in Bitmap}
  {MFT:Data}
  Count:=NTFSRoundQuadWordToClusterSize(CalculateMftSize,FClusterShiftCount,FClusterSize);
  Count:=(Count shr FClusterShiftCount);
  Cluster:=FMftStartCluster;
  if not MarkClusters(Cluster,Count) then Exit;

  {MFT:Bitmap}
  Count:=NTFSRoundQuadWordToClusterSize(8,FClusterShiftCount,FClusterSize);
  Count:=(Count shr FClusterShiftCount);
  Cluster:=FMftStartCluster - Count;
  if not MarkClusters(Cluster,Count) then Exit;

  {MFTMirr:Data}
  Count:=NTFSRoundQuadWordToClusterSize(CalculateMftMirrSize,FClusterShiftCount,FClusterSize);
  Count:=(Count shr FClusterShiftCount);
  Cluster:=FMftMirrorCluster;
  if not MarkClusters(Cluster,Count) then Exit;

  {LogFile:Data}
  Count:=NTFSRoundQuadWordToClusterSize(CalculateLogFileSize,FClusterShiftCount,FClusterSize);
  Count:=(Count shr FClusterShiftCount);
  Cluster:=CalculateLogFileStartCluster;
  if Cluster = ntfsUnknownCluster then Exit;
  if not MarkClusters(Cluster,Count) then Exit;

  {AttrDef:Data}
  Count:=NTFSRoundQuadWordToClusterSize(CalculateAttrDefSize,FClusterShiftCount,FClusterSize);
  Count:=(Count shr FClusterShiftCount);
  Cluster:=CalculateAttrDefStartCluster;
  if Cluster = ntfsUnknownCluster then Exit;
  if not MarkClusters(Cluster,Count) then Exit;

  {Root:SecurityDescriptor}
  Count:=NTFSRoundQuadWordToClusterSize(CalculateRootSecurityDescriptorSize,FClusterShiftCount,FClusterSize);
  Count:=(Count shr FClusterShiftCount);
  Cluster:=CalculateRootSecurityDescriptorStartCluster;
  if Cluster = ntfsUnknownCluster then Exit;
  if not MarkClusters(Cluster,Count) then Exit;

  {Root:Allocation}
  Count:=NTFSRoundQuadWordToClusterSize(CalculateRootAllocationSize,FClusterShiftCount,FClusterSize);
  Count:=(Count shr FClusterShiftCount);
  Cluster:=CalculateRootAllocationStartCluster;
  if Cluster = ntfsUnknownCluster then Exit;
  if not MarkClusters(Cluster,Count) then Exit;

  {Bitmap:Data}
  Count:=NTFSRoundQuadWordToClusterSize(CalculateBitmapSize,FClusterShiftCount,FClusterSize);
  Count:=(Count shr FClusterShiftCount);
  Cluster:=CalculateBitmapStartCluster;
  if Cluster = ntfsUnknownCluster then Exit;
  if not MarkClusters(Cluster,Count) then Exit;

  {Boot:Data}
  Count:=NTFSRoundQuadWordToClusterSize(CalculateBootSize,FClusterShiftCount,FClusterSize);
  Count:=(Count shr FClusterShiftCount);
  Cluster:=ntfsStartCluster;
  if not MarkClusters(Cluster,Count) then Exit;

  {Secure:Sds}
  Count:=NTFSRoundQuadWordToClusterSize(CalculateSecureSdsSize,FClusterShiftCount,FClusterSize);
  Count:=(Count shr FClusterShiftCount);
  Cluster:=CalculateSecureSdsStartCluster;
  if Cluster = ntfsUnknownCluster then Exit;
  if not MarkClusters(Cluster,Count) then Exit;

  {UpCase:Data}
  Count:=NTFSRoundQuadWordToClusterSize(CalculateUpCaseSize,FClusterShiftCount,FClusterSize);
  Count:=(Count shr FClusterShiftCount);
  Cluster:=CalculateUpCaseStartCluster;
  if Cluster = ntfsUnknownCluster then Exit;
  if not MarkClusters(Cluster,Count) then Exit;

  Result:=True;
 finally
  FRecords.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.CreateLogFiles:Boolean;
{Create the default $LogFile headers}
var
 Remain:Int64;
 Offset:PtrUInt;
 Count:LongWord;
 Buffer:Pointer;

 Instance:LongWord;

 Current:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FRecords.WriterLock then Exit;
 try
  if FDriver = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CreateLogFiles');
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get LogFile}
  Current:=GetRecordEx(nil,ntfsFileTypeLogFile,False,True);
  if Current = nil then Exit;

  {Get Attribute}
  Attribute:=Current.GetAttribute(ntfsAttrTypeData,ntfsBlankName,ntfsInstanceFirst);
  if Attribute = nil then Exit;

  {Check Size}
  if Attribute.StreamSize <> CalculateLogFileSize then Exit;

  {Create LogFile File}
  Remain:=Attribute.StreamSize;
  Offset:=0;
  Count:=SIZE_256K;
  Buffer:=AllocMem(Count);
  Instance:=ntfsInstanceFirst;
  try
   while Remain > 0 do
    begin
     {Check Remain}
     if Remain < Count then Count:=Remain;

     {Write Attribute}
     if WriteAttribute(Current,Attribute,Buffer^,Offset,Count,Instance,False) <> Count then Exit;

     {Update Remain}
     Inc(Offset,Count);
     Dec(Remain,Count);
    end;

   //To Do //Create 2 empty Restart Records  (see Struct)
           //       2 Restart Areas
           //       2 Log Clients
           //       Fill LogRecord pages with ?
           //Do Fixup on Restart Records and Log Record

   Result:=True;
  finally
   FreeMem(Buffer);
  end;
 finally
  FRecords.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.CreateUpCases:Boolean;
{Create the $UpCase file data}
var
 Buffer:Pointer;
 Size:LongWord;
 Offset:LongWord;
 Instance:LongWord;

 Current:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FRecords.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FUpCases = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CreateUpCases');
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get UpCase}
  Current:=GetRecordEx(nil,ntfsFileTypeUpCase,False,True);
  if Current = nil then Exit;

  {Get Attribute}
  Attribute:=Current.GetAttribute(ntfsAttrTypeData,ntfsBlankName,ntfsInstanceFirst);
  if Attribute = nil then Exit;

  {Check Size}
  if Attribute.StreamSize <> CalculateUpCaseSize then Exit;

  {Create UpCase File}
  Buffer:=AllocMem(Attribute.StreamSize);
  try
   {Load Defaults}
   if not FUpCases.Init(FVolumeVersion) then Exit;

   {Write Buffer}
   Offset:=0;
   Size:=Attribute.StreamSize;
   if not FUpCases.WriteUpCase(Buffer,Offset,Size,FVolumeVersion) then Exit;

   {Write Attribute}
   Instance:=ntfsInstanceFirst;
   if WriteAttribute(Current,Attribute,Buffer^,0,Attribute.StreamSize,Instance,False) <> Attribute.StreamSize then Exit;

   Result:=True;
  finally
   FreeMem(Buffer);
  end;
 finally
  FRecords.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.CreateAttrDefs:Boolean;
{Create the $AttrDef file data}
var
 Buffer:Pointer;
 Size:LongWord;
 Offset:LongWord;
 Instance:LongWord;

 Current:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FRecords.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FAttrDefs = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CreateAttrDefs');
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get AttrDef}
  Current:=GetRecordEx(nil,ntfsFileTypeAttrDef,False,True);
  if Current = nil then Exit;

  {Get Attribute}
  Attribute:=Current.GetAttribute(ntfsAttrTypeData,ntfsBlankName,ntfsInstanceFirst);
  if Attribute = nil then Exit;

  {Check Size}
  if Attribute.StreamSize <> CalculateAttrDefSize then Exit;

  {Create AttrDef File}
  Buffer:=AllocMem(Attribute.StreamSize);
  try
   {Load Defaults}
   if not FAttrDefs.Init(FVolumeVersion) then Exit;

   {Write Buffer}
   Offset:=0;
   Size:=Attribute.StreamSize;
   if not FAttrDefs.WriteAttrDefs(Buffer,Offset,Size,FVolumeVersion) then Exit;

   {Write Attribute}
   Instance:=ntfsInstanceFirst;
   if WriteAttribute(Current,Attribute,Buffer^,0,Attribute.StreamSize,Instance,False) <> Attribute.StreamSize then Exit;

   Result:=True;
  finally
   FreeMem(Buffer);
  end;
 finally
  FRecords.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.CreateSecuritys:Boolean;
{Create the default $Secure:$SDS/$SDH/$SII index entries}
var
 Buffer:Pointer;
 Size:LongWord;
 Offset:LongWord;
 Instance:LongWord;
 Descriptor:Pointer;
 SecurityId:LongWord;
 SecurityIdData:TNTFSSecurityIdData;
 SecurityHashData:TNTFSSecurityHashData;
 SecurityHashKey:TNTFSSecurityHashKeyData;

 Origin:TNTFSDiskRecord;
 Current:TNTFSDiskRecord;

 Key:TNTFSDiskKey;
 Index:TNTFSDiskIndex;
 Root:TNTFSDiskAttribute;
 Attribute:TNTFSDiskAttribute;
 SecurityItem:TNTFSSecurityItem;
begin
 {}
 Result:=False;

 if not FRecords.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FSecuritys = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CreateSecuritys');
  {$ENDIF}

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CreateSecuritys - Creating $SDS');
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get Secure}
  Current:=GetRecordEx(nil,ntfsFileTypeSecure,False,True);
  if Current = nil then Exit;

  {Get Attribute}
  Attribute:=Current.GetAttribute(ntfsAttrTypeData,ntfsStreamNameSecurity,ntfsInstanceFirst);
  if Attribute = nil then Exit;

  {Check Size}
  if Attribute.StreamSize <> CalculateSecureSdsSize then Exit;

  {Create SecureSds File}
  Buffer:=AllocMem(Attribute.StreamSize);
  try
   {Load Defaults}
   if not FSecuritys.Init(FVolumeVersion) then Exit;

   {Write Buffer}
   Offset:=0;
   Size:=Attribute.StreamSize;
   if not FSecuritys.WriteSecurityItems(Buffer,Offset,Size,FVolumeVersion) then Exit;

   {Write Attribute}
   Instance:=ntfsInstanceFirst;
   if WriteAttribute(Current,Attribute,Buffer^,0,Attribute.StreamSize,Instance,False) <> Attribute.StreamSize then Exit;
  finally
   FreeMem(Buffer);
  end;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CreateSecuritys - Creating $SDH');
  {$ENDIF}

  {Get Origin}
  Origin:=Current.Origin;
  if Origin = nil then Exit;

  {Get Index Root ($SDH)}
  Root:=Origin.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameSecurityHash,ntfsInstanceFirst);
  if Root = nil then Exit;

  {Get Index}
  Index:=Root.Index;
  if Index = nil then Exit;
  if Index.IndexType <> ntfsAttrTypeNone then Exit;
  if Index.CollateRule <> ntfsCollateTypeSecurityHash then Exit;

  {Get Key (100)}
  Descriptor:=nil;
  if not NTFSCreateDefaultDescriptor(ntfsDefaultDescriptor100,Descriptor,FVolumeVersion) then Exit;
  try
   Key:=Index.FindKey(Descriptor,Security.GetSecurityDescriptorLength(Descriptor));
   if Key <> nil then Exit;

   {Add Key}
   SecurityItem:=FSecuritys.GetSecurityItem(ntfsDefaultSecurityId100);
   if SecurityItem = nil then Exit;
   FillChar(SecurityHashKey,SizeOf(TNTFSSecurityHashKeyData),0);
   SecurityHashKey.SecurityHash:=SecurityItem.SecurityHash;
   SecurityHashKey.SecurityId:=SecurityItem.SecurityId;
   FillChar(SecurityHashData,SizeOf(TNTFSSecurityHashData),0);
   SecurityHashData.SecurityHash:=SecurityItem.SecurityHash;
   SecurityHashData.SecurityId:=SecurityItem.SecurityId;
   SecurityHashData.SecurityOffset:=SecurityItem.SecurityOffset;
   SecurityHashData.SecuritySize:=SecurityItem.SecuritySize;
   {SecurityHashData.Padding:=ntfsSecurityHashPadding;}

   {Key:=Index.NewKey(@SecurityHashKey,@SecurityHashData,ntfsSecurityHashKeySize,ntfsSecurityHashSize);}
   Key:=Index.NewKeyEx(@SecurityHashKey,@SecurityHashData,@SecurityHashPadding,ntfsSecurityHashKeySize,ntfsSecurityHashSize,ntfsSecurityHashPaddingSize);
   if Key = nil then Exit;
   try
    {Update Key}
    Key.EntrySize:=Key.CalculatedSize(FVolumeVersion);

    {Insert Key}
    if not Index.InsertKey(Key) then Exit;
    try
     {Set Index}
     if not SetAttribute(Origin,Root) then Exit;

     Result:=True;
    finally
     if not Result then Index.RemoveKey(Key);
     if not Result then Key:=nil;
    end;
   finally
    if not(Result) and (Key <> nil) then Index.DestroyKey(Key); {Dont double free the key}
   end;
  finally
   NTFSDestroyDefaultDescriptor(Descriptor,FVolumeVersion);
  end;

  {Reset Result}
  Result:=False;

  {Get Key (101)}
  Descriptor:=nil;
  if not NTFSCreateDefaultDescriptor(ntfsDefaultDescriptor101,Descriptor,FVolumeVersion) then Exit;
  try
   Key:=Index.FindKey(Descriptor,Security.GetSecurityDescriptorLength(Descriptor));
   if Key <> nil then Exit;

   {Add Key}
   SecurityItem:=FSecuritys.GetSecurityItem(ntfsDefaultSecurityId101);
   if SecurityItem = nil then Exit;
   FillChar(SecurityHashKey,SizeOf(TNTFSSecurityHashKeyData),0);
   SecurityHashKey.SecurityHash:=SecurityItem.SecurityHash;
   SecurityHashKey.SecurityId:=SecurityItem.SecurityId;
   FillChar(SecurityHashData,SizeOf(TNTFSSecurityHashData),0);
   SecurityHashData.SecurityHash:=SecurityItem.SecurityHash;
   SecurityHashData.SecurityId:=SecurityItem.SecurityId;
   SecurityHashData.SecurityOffset:=SecurityItem.SecurityOffset;
   SecurityHashData.SecuritySize:=SecurityItem.SecuritySize;
   {SecurityHashData.Padding:=ntfsSecurityHashPadding;}

   {Key:=Index.NewKey(@SecurityHashKey,@SecurityHashData,ntfsSecurityHashKeySize,ntfsSecurityHashSize);}
   Key:=Index.NewKeyEx(@SecurityHashKey,@SecurityHashData,@SecurityHashPadding,ntfsSecurityHashKeySize,ntfsSecurityHashSize,ntfsSecurityHashPaddingSize);
   if Key = nil then Exit;
   try
    {Update Key}
    Key.EntrySize:=Key.CalculatedSize(FVolumeVersion);

    {Insert Key}
    if not Index.InsertKey(Key) then Exit;
    try
     {Set Index}
     if not SetAttribute(Origin,Root) then Exit;

     Result:=True;
    finally
     if not Result then Index.RemoveKey(Key);
     if not Result then Key:=nil;
    end;
   finally
    if not(Result) and (Key <> nil) then Index.DestroyKey(Key); {Dont double free the key}
   end;
  finally
   NTFSDestroyDefaultDescriptor(Descriptor,FVolumeVersion);
  end;

  (*{Reset Result}
  Result:=False;

  {Get Key (102)}
  Descriptor:=nil;
  if not NTFSCreateDefaultDescriptor(ntfsDefaultDescriptor102,Descriptor,FVolumeVersion) then Exit;
  try
   Key:=Index.FindKey(Descriptor,Security.GetSecurityDescriptorLength(Descriptor));
   if Key <> nil then Exit;

   {Add Key}
   SecurityItem:=FSecuritys.GetSecurityItem(ntfsDefaultSecurityId102);
   if SecurityItem = nil then Exit;
   FillChar(SecurityHashKey,SizeOf(TNTFSSecurityHashKeyData),0);
   SecurityHashKey.SecurityHash:=SecurityItem.SecurityHash;
   SecurityHashKey.SecurityId:=SecurityItem.SecurityId;
   FillChar(SecurityHashData,SizeOf(TNTFSSecurityHashData),0);
   SecurityHashData.SecurityHash:=SecurityItem.SecurityHash;
   SecurityHashData.SecurityId:=SecurityItem.SecurityId;
   SecurityHashData.SecurityOffset:=SecurityItem.SecurityOffset;
   SecurityHashData.SecuritySize:=SecurityItem.SecuritySize;
   {SecurityHashData.Padding:=ntfsSecurityHashPadding;}

   {Key:=Index.NewKey(@SecurityHashKey,@SecurityHashData,ntfsSecurityHashKeySize,ntfsSecurityHashSize);}
   Key:=Index.NewKeyEx(@SecurityHashKey,@SecurityHashData,@SecurityHashPadding,ntfsSecurityHashKeySize,ntfsSecurityHashSize,ntfsSecurityHashPaddingSize);
   if Key = nil then Exit;
   try
    {Update Key}
    Key.EntrySize:=Key.CalculatedSize(FVolumeVersion);

    {Insert Key}
    if not Index.InsertKey(Key) then Exit;
    try
     {Set Index}
     if not SetAttribute(Origin,Root) then Exit;

     Result:=True;
    finally
     if not Result then Index.RemoveKey(Key);
     if not Result then Key:=nil;
    end;
   finally
    if not(Result) and (Key <> nil) then Index.DestroyKey(Key); {Dont double free the key}
   end;
  finally
   NTFSDestroyDefaultDescriptor(Descriptor,FVolumeVersion);
  end;

  {Reset Result}
  Result:=False;

  {Get Key (103)}
  Descriptor:=nil;
  if not NTFSCreateDefaultDescriptor(ntfsDefaultDescriptor103,Descriptor,FVolumeVersion) then Exit;
  try
   Key:=Index.FindKey(Descriptor,Security.GetSecurityDescriptorLength(Descriptor));
   if Key <> nil then Exit;

   {Add Key}
   SecurityItem:=FSecuritys.GetSecurityItem(ntfsDefaultSecurityId103);
   if SecurityItem = nil then Exit;
   FillChar(SecurityHashKey,SizeOf(TNTFSSecurityHashKeyData),0);
   SecurityHashKey.SecurityHash:=SecurityItem.SecurityHash;
   SecurityHashKey.SecurityId:=SecurityItem.SecurityId;
   FillChar(SecurityHashData,SizeOf(TNTFSSecurityHashData),0);
   SecurityHashData.SecurityHash:=SecurityItem.SecurityHash;
   SecurityHashData.SecurityId:=SecurityItem.SecurityId;
   SecurityHashData.SecurityOffset:=SecurityItem.SecurityOffset;
   SecurityHashData.SecuritySize:=SecurityItem.SecuritySize;
   {SecurityHashData.Padding:=ntfsSecurityHashPadding;}

   {Key:=Index.NewKey(@SecurityHashKey,@SecurityHashData,ntfsSecurityHashKeySize,ntfsSecurityHashSize);}
   Key:=Index.NewKeyEx(@SecurityHashKey,@SecurityHashData,@SecurityHashPadding,ntfsSecurityHashKeySize,ntfsSecurityHashSize,ntfsSecurityHashPaddingSize);
   if Key = nil then Exit;
   try
    {Update Key}
    Key.EntrySize:=Key.CalculatedSize(FVolumeVersion);

    {Insert Key}
    if not Index.InsertKey(Key) then Exit;
    try
     {Set Index}
     if not SetAttribute(Origin,Root) then Exit;

     Result:=True;
    finally
     if not Result then Index.RemoveKey(Key);
     if not Result then Key:=nil;
    end;
   finally
    if not(Result) and (Key <> nil) then Index.DestroyKey(Key); {Dont double free the key}
   end;
  finally
   NTFSDestroyDefaultDescriptor(Descriptor,FVolumeVersion);
  end;*)

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CreateSecuritys - Creating $SII');
  {$ENDIF}

  {Reset Result}
  Result:=False;

  {Get Index Root ($SII)}
  Root:=Origin.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameSecurityId,ntfsInstanceFirst);
  if Root = nil then Exit;

  {Get Index}
  Index:=Root.Index;
  if Index = nil then Exit;
  if Index.IndexType <> ntfsAttrTypeNone then Exit;
  if Index.CollateRule <> ntfsCollateTypeLongWord then Exit;

  {Get Key (100)}
  SecurityId:=ntfsDefaultSecurityId100;
  Key:=Index.FindKey(@SecurityId,ntfsSecurityIdKeySize);
  if Key <> nil then Exit;

  {Add Key}
  SecurityItem:=FSecuritys.GetSecurityItem(ntfsDefaultSecurityId100);
  if SecurityItem = nil then Exit;
  SecurityId:=ntfsDefaultSecurityId100;
  FillChar(SecurityIdData,SizeOf(TNTFSSecurityIdData),0);
  SecurityIdData.SecurityHash:=SecurityItem.SecurityHash;
  SecurityIdData.SecurityId:=SecurityItem.SecurityId;
  SecurityIdData.SecurityOffset:=SecurityItem.SecurityOffset;
  SecurityIdData.SecuritySize:=SecurityItem.SecuritySize;

  Key:=Index.NewKey(@SecurityId,@SecurityIdData,ntfsSecurityIdKeySize,ntfsSecurityIdSize);
  if Key = nil then Exit;
  try
   {Update Key}
   Key.EntrySize:=Key.CalculatedSize(FVolumeVersion);
   {Insert Key}
   if not Index.InsertKey(Key) then Exit;
   try
    {Set Index}
    if not SetAttribute(Origin,Root) then Exit;

    Result:=True;
   finally
    if not Result then Index.RemoveKey(Key);
    if not Result then Key:=nil;
   end;
  finally
   if not(Result) and (Key <> nil) then Index.DestroyKey(Key); {Dont double free the key}
  end;

  {Reset Result}
  Result:=False;

  {Get Key (101)}
  SecurityId:=ntfsDefaultSecurityId101;
  Key:=Index.FindKey(@SecurityId,ntfsSecurityIdKeySize);
  if Key <> nil then Exit;

  {Add Key}
  SecurityItem:=FSecuritys.GetSecurityItem(ntfsDefaultSecurityId101);
  if SecurityItem = nil then Exit;
  SecurityId:=ntfsDefaultSecurityId101;
  FillChar(SecurityIdData,SizeOf(TNTFSSecurityIdData),0);
  SecurityIdData.SecurityHash:=SecurityItem.SecurityHash;
  SecurityIdData.SecurityId:=SecurityItem.SecurityId;
  SecurityIdData.SecurityOffset:=SecurityItem.SecurityOffset;
  SecurityIdData.SecuritySize:=SecurityItem.SecuritySize;

  Key:=Index.NewKey(@SecurityId,@SecurityIdData,ntfsSecurityIdKeySize,ntfsSecurityIdSize);
  if Key = nil then Exit;
  try
   {Update Key}
   Key.EntrySize:=Key.CalculatedSize(FVolumeVersion);

   {Insert Key}
   if not Index.InsertKey(Key) then Exit;
   try
    {Set Index}
    if not SetAttribute(Origin,Root) then Exit;

    Result:=True;
   finally
    if not Result then Index.RemoveKey(Key);
    if not Result then Key:=nil;
   end;
  finally
   if not(Result) and (Key <> nil) then Index.DestroyKey(Key); {Dont double free the key}
  end;

  (*{Reset Result}
  Result:=False;

  {Get Key (102)}
  SecurityId:=ntfsDefaultSecurityId102;
  Key:=Index.FindKey(@SecurityId,ntfsSecurityIdKeySize);
  if Key <> nil then Exit;

  {Add Key}
  SecurityItem:=FSecuritys.GetSecurityItem(ntfsDefaultSecurityId102);
  if SecurityItem = nil then Exit;
  SecurityId:=ntfsDefaultSecurityId102;
  FillChar(SecurityIdData,SizeOf(TNTFSSecurityIdData),0);
  SecurityIdData.SecurityHash:=SecurityItem.SecurityHash;
  SecurityIdData.SecurityId:=SecurityItem.SecurityId;
  SecurityIdData.SecurityOffset:=SecurityItem.SecurityOffset;
  SecurityIdData.SecuritySize:=SecurityItem.SecuritySize;

  Key:=Index.NewKey(@SecurityId,@SecurityIdData,ntfsSecurityIdKeySize,ntfsSecurityIdSize);
  if Key = nil then Exit;
  try
   {Update Key}
   Key.EntrySize:=Key.CalculatedSize(FVolumeVersion);

   {Insert Key}
   if not Index.InsertKey(Key) then Exit;
   try
    {Set Index}
    if not SetAttribute(Origin,Root) then Exit;

    Result:=True;
   finally
    if not Result then Index.RemoveKey(Key);
    if not Result then Key:=nil;
   end;
  finally
   if not(Result) and (Key <> nil) then Index.DestroyKey(Key); {Dont double free the key}
  end;

  {Reset Result}
  Result:=False;

  {Get Key (103)}
  SecurityId:=ntfsDefaultSecurityId103;
  Key:=Index.FindKey(@SecurityId,ntfsSecurityIdKeySize);
  if Key <> nil then Exit;

  {Add Key}
  SecurityItem:=FSecuritys.GetSecurityItem(ntfsDefaultSecurityId103);
  if SecurityItem = nil then Exit;
  SecurityId:=ntfsDefaultSecurityId103;
  FillChar(SecurityIdData,SizeOf(TNTFSSecurityIdData),0);
  SecurityIdData.SecurityHash:=SecurityItem.SecurityHash;
  SecurityIdData.SecurityId:=SecurityItem.SecurityId;
  SecurityIdData.SecurityOffset:=SecurityItem.SecurityOffset;
  SecurityIdData.SecuritySize:=SecurityItem.SecuritySize;

  Key:=Index.NewKey(@SecurityId,@SecurityIdData,ntfsSecurityIdKeySize,ntfsSecurityIdSize);
  if Key = nil then Exit;
  try
   {Update Key}
   Key.EntrySize:=Key.CalculatedSize(FVolumeVersion);

   {Insert Key}
   if not Index.InsertKey(Key) then Exit;
   try
    {Set Index}
    if not SetAttribute(Origin,Root) then Exit;

    Result:=True;
   finally
    if not Result then Index.RemoveKey(Key);
    if not Result then Key:=nil;
   end;
  finally
   if not(Result) and (Key <> nil) then Index.DestroyKey(Key); {Dont double free the key}
  end;*)
 finally
  FRecords.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.LoadUpCases:Boolean;
var
 Buffer:Pointer;
 Size:LongWord;
 Value:LongWord;
 Offset:LongWord;
begin
 {}
 Result:=False;

 if not FRecords.ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FUpCases = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadUpCases');
  {$ENDIF}

  if FUpCase = nil then
   begin
    {Load Defaults}
    Result:=FUpCases.Init(FVolumeVersion);

    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadUpCases - $UpCase Not Found');
    {$ENDIF}
   end
  else
   begin
    {Check $UpCase File}
    if FUpCase.Size <> ntfsFileSizeUpCase then
     begin
      {Load Defaults}
      Result:=FUpCases.Init(FVolumeVersion);

      {$IFDEF NTFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadUpCases - $UpCase Size not equal to Default');
     {$ENDIF}
     end
    else
     begin
      {Load $UpCase File}
      Buffer:=GetMem(ntfsFileSizeUpCase);
      if Buffer = nil then Exit;
      try
       Value:=0;
       if ReadEntry(FRoot,FUpCase,Buffer^,0,ntfsFileSizeUpCase,Offset,Value) = ntfsFileSizeUpCase then
        begin
         Offset:=0;
         Size:=ntfsFileSizeUpCase;
         Result:=FUpCases.ReadUpCase(Buffer,Offset,Size,FVolumeVersion);

         {$IFDEF NTFS_DEBUG}
         if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadUpCases - Size = ' + IntToStr(ntfsFileSizeUpCase));
         {$ENDIF}
        end;
      finally
       FreeMem(Buffer);
      end;
     end;
   end;
 finally
  FRecords.ReaderUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.LoadAttrDefs:Boolean;
var
 Buffer:Pointer;
 Size:LongWord;
 Value:LongWord;
 Offset:LongWord;
begin
 {}
 Result:=False;

 if not FRecords.ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FAttrDefs = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadAttrDefs');
  {$ENDIF}

  if FAttrDef = nil then
   begin
    {Load Defaults}
    Result:=FAttrDefs.Init(FVolumeVersion);

    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadAttrDefs - $AttrDef Not Found');
    {$ENDIF}
   end
  else
   begin
    {Check $AttrDef File}
    Size:=ntfsFileSize30AttrDef;
    if FNTFSType = ntNTFS12 then Size:=ntfsFileSize12AttrDef;
    if FAttrDef.Size < Size then
     begin
      {Load Defaults}
      Result:=FAttrDefs.Init(FVolumeVersion);

      {$IFDEF NTFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadAttrDefs - $AttrDef Size less than Default');
     {$ENDIF}
     end
    else
     begin
      {Load $AttrDef}
      Buffer:=GetMem(FAttrDef.Size);
      if Buffer = nil then Exit;
      try
       Value:=0;
       if ReadEntry(FRoot,FAttrDef,Buffer^,0,FAttrDef.Size,Offset,Value) = FAttrDef.Size then
        begin
         Offset:=0;
         Size:=FAttrDef.Size;
         Result:=FAttrDefs.ReadAttrDefs(Buffer,Offset,Size,FVolumeVersion);

         {$IFDEF NTFS_DEBUG}
         if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadAttrDefs - Size = ' + IntToStr(FAttrDef.Size));
         {$ENDIF}
        end;
      finally
       FreeMem(Buffer);
      end;
     end;
   end;
 finally
  FRecords.ReaderUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.LoadSecuritys:Boolean;
var
 Buffer:Pointer;
 Size:LongWord;
 Value:LongWord;
 Offset:LongWord;
 Entry:TNTFSDiskEntry;
begin
 {}
 Result:=False;

 if not FRecords.ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FSecuritys = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadSecuritys');
  {$ENDIF}

  if FSecure = nil then
   begin
    {Load Defaults}
    Result:=FSecuritys.Init(FVolumeVersion);

    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadSecuritys - $Secure Not Found');
    {$ENDIF}
   end
  else
   begin
    {Convert Lock}
    if not FRecords.ReaderConvert then Exit;

    {Get $SDS Stream}
    Entry:=TNTFSDiskEntry(GetEntryEx(FSecure,NTFSAttributeNameToStreamName(ntfsAttrTypeData,ntfsStreamNameSecurity),faStream,False,False,True));

    {Convert Lock}
    if not FRecords.WriterConvert then Exit;

    {Check $SDS Stream}
    if Entry = nil then
     begin
      {Load Defaults}
      Result:=FSecuritys.Init(FVolumeVersion);

      {$IFDEF NTFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadSecuritys - $SDS Not Found');
      {$ENDIF}
     end
    else
     begin
      {Check $SDS Stream}
      if Entry.Size = 0 then
       begin
        {Load Defaults}
        Result:=FSecuritys.Init(FVolumeVersion);

        {$IFDEF NTFS_DEBUG}
        if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadSecuritys - $SDS Size = 0');
       {$ENDIF}
       end
      else
       begin
        {Load $Secure:$SDS}
        Buffer:=GetMem(Entry.Size);
        if Buffer = nil then Exit;
        try
         Value:=0;
         if ReadEntry(FSecure,Entry,Buffer^,0,Entry.Size,Offset,Value) = Entry.Size then
          begin
           Offset:=0;
           Size:=Entry.Size;
           Result:=FSecuritys.ReadSecurityItems(Buffer,Offset,Size,FVolumeVersion);

           {$IFDEF NTFS_DEBUG}
           if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadSecuritys - Size = ' + IntToStr(Entry.Size));
           {$ENDIF}
          end;
        finally
         FreeMem(Buffer);
        end;
       end;
     end;
   end;
 finally
  FRecords.ReaderUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.GetMftZoneStart:Int64;
{Calculate the start cluster of the Mft reserved zone}
{Only called when allocating a new Mft reserved zone}
begin
 {}
 Result:=0;

 if FDriver = nil then Exit;
 if FMftStartCluster = 0 then Exit;
 if FSectorsPerCluster = 0 then Exit;
 if FMftZoneReservation = 0 then Exit;
 if FMftStartCluster = ntfsUnknownCluster then Exit;

 //To Do //Find a block with at least ??? free and set Mft zone to that block ?
               //Or, if last cluster of Mft is not in zone then start the Mft zone at that block ?
               //    would need an initial parameter to do this only on startup
end;

{=============================================================================}

function TNTFSFileSystem.GetMftZoneCluster:Int64;
{Calculate the end cluster of Mft reserved zone}
{Only called during volume mount and initialization}
{Also called when resizing the zone reservation}
begin
 {}
 Result:=0;

 if FDriver = nil then Exit;
 if FMftStartCluster = 0 then Exit;
 if FSectorsPerCluster = 0 then Exit;
 if FMftZoneReservation = 0 then Exit;
 if FMftStartCluster = ntfsUnknownCluster then Exit;

 if FFixedZone then
  begin
   if FSectorSize = 0 then Exit;

   Result:=FMftStartCluster + Trunc((((FMftZoneReservation * 1048576) div FSectorSize) div FSectorsPerCluster));
  end
 else
  begin
   if FSectorCount = 0 then Exit;

   Result:=FMftStartCluster + Trunc((FSectorCount div FSectorsPerCluster) * (FMftZoneReservation / 1000)); {Divide by 1000 to allow for 12.5/37.5 etc}
  end;
end;

{=============================================================================}

function TNTFSFileSystem.GetMftMirrorCount:LongWord;
{Calculate the number of Mft entries in the MftMirr}
{Only called during volume mount and initialization}
begin
 {}
 Result:=0;

 if FDriver = nil then Exit;

 Result:=Max(ntfsDefaultMirrorCount,FFilesPerCluster); {Default to 4}
end;

{=============================================================================}

function TNTFSFileSystem.GetMftZoneReservation:Word;
{Calculate the starting size of the Mft Zone Reservation}
{Only called during volume mount and initialization}
begin
 {}
 if FFixedZone then
  begin
   {If Volume is less than 1000MB then switch to Percentage}
   if (ntfsMftFixedZone1000MBSize div FSectorSize) >= FSectorCount then FFixedZone:=False;
  end;

 if FFixedZone then
  begin
   Result:=ntfsMftFixedZone0MB;

   if FDriver = nil then Exit;
   if FSectorSize = 0 then Exit;

   Result:=ntfsMftFixedZone800MB; {Default to 800MB}

   {If Zone is greater than half of the volume then shrink to next size down}
   if (ntfsMftFixedZone800MBSize div FSectorSize) >= (FSectorCount shr 1) then Result:=ntfsMftFixedZone600MB else Exit;
   if (ntfsMftFixedZone600MBSize div FSectorSize) >= (FSectorCount shr 1) then Result:=ntfsMftFixedZone400MB else Exit;
   if (ntfsMftFixedZone400MBSize div FSectorSize) >= (FSectorCount shr 1) then Result:=ntfsMftFixedZone200MB else Exit;
   if (ntfsMftFixedZone200MBSize div FSectorSize) >= (FSectorCount shr 1) then Result:=ntfsMftFixedZone0MB else Exit;
  end
 else
  begin
   Result:=ntfsMftZone0percent;

   if FDriver = nil then Exit;

   Result:=ntfsMftZone25percent; {Default to 25%}

   {If Volume is less than 1000MB then switch to 12.5%}
   if (ntfsMftFixedZone1000MBSize div FSectorSize) >= FSectorCount then Result:=ntfsMftZone12percent;
  end;
end;

{=============================================================================}

function TNTFSFileSystem.ShrinkMftZoneReservation:Boolean;
{Calculate the new size of the Mft Zone Reservation}
{Only called when resizing the zone reservation}
begin
 {}
 Result:=False;

 if FDriver = nil then Exit;
 if FMftZoneReservation = 0 then Exit; {Fail if already at zero}

 if FFixedZone then
  begin
   case FMftZoneReservation of
    ntfsMftFixedZone800MB:begin
      FMftZoneReservation:=ntfsMftFixedZone600MB;
      FMftZoneCluster:=GetMftZoneCluster;

      Result:=True;
     end;
    ntfsMftFixedZone600MB:begin
      FMftZoneReservation:=ntfsMftFixedZone400MB;
      FMftZoneCluster:=GetMftZoneCluster;

      Result:=True;
     end;
    ntfsMftFixedZone400MB:begin
      FMftZoneReservation:=ntfsMftFixedZone200MB;
      FMftZoneCluster:=GetMftZoneCluster;

      Result:=True;
     end;
    ntfsMftFixedZone200MB:begin
      FMftZoneReservation:=ntfsMftFixedZone0MB;
      FMftZoneCluster:=GetMftZoneCluster;

      Result:=True;
     end;
   end;
  end
 else
  begin
   case FMftZoneReservation of
    ntfsMftZone50percent:begin
      FMftZoneReservation:=ntfsMftZone37percent;
      FMftZoneCluster:=GetMftZoneCluster;

      Result:=True;
     end;
    ntfsMftZone37percent:begin
      FMftZoneReservation:=ntfsMftZone25percent;
      FMftZoneCluster:=GetMftZoneCluster;

      Result:=True;
     end;
    ntfsMftZone25percent:begin
      FMftZoneReservation:=ntfsMftZone12percent;
      FMftZoneCluster:=GetMftZoneCluster;

      Result:=True;
     end;
    ntfsMftZone12percent:begin
      FMftZoneReservation:=ntfsMftZone0percent;
      FMftZoneCluster:=GetMftZoneCluster;

      Result:=True;
     end;
   end;
  end;
end;

{=============================================================================}

function TNTFSFileSystem.GetFileRecordSize(AClustersPerFile:LongInt):LongWord;
{Calculate the size of an file record}
{Only called during volume mount and initialization}
begin
 {}
 Result:=0;

 if FDriver = nil then Exit;
 if AClustersPerFile = 0 then Exit;

 if ShortInt(AClustersPerFile) > 0 then
  begin
   Result:=(LongWord(AClustersPerFile) * (FSectorSize * FSectorsPerCluster));
  end
 else
  begin
   Result:=(1 shl (-1 * ShortInt(AClustersPerFile)));
  end;
end;

{=============================================================================}

function TNTFSFileSystem.GetIndexRecordSize(AClustersPerIndex:LongInt):LongWord;
{Calculate the size of an index record}
{Only called during volume mount and initialization}
begin
 {}
 Result:=0;

 if FDriver = nil then Exit;
 if AClustersPerIndex = 0 then Exit;

 if ShortInt(AClustersPerIndex) > 0 then
  begin
   Result:=(LongWord(AClustersPerIndex) * (FSectorSize * FSectorsPerCluster));
  end
 else
  begin
   Result:=(1 shl (-1 * ShortInt(AClustersPerIndex)));
  end;
end;

{=============================================================================}

function TNTFSFileSystem.GetClustersPerFile(AClustersPerFile:LongInt):LongWord;
{Calculate the number of clusters per file record}
{Only called during volume mount and initialization}
begin
 {}
 Result:=0;

 if FDriver = nil then Exit;
 if AClustersPerFile = 0 then Exit;

 if ShortInt(AClustersPerFile) > 0 then
  begin
   Result:=AClustersPerFile;
  end;
end;

{=============================================================================}

function TNTFSFileSystem.GetClustersPerIndex(AClustersPerIndex:LongInt):LongWord;
{Calculate the number of clusters per index record}
{Only called during volume mount and initialization}
begin
 {}
 Result:=0;

 if FDriver = nil then Exit;
 if AClustersPerIndex = 0 then Exit;

 if ShortInt(AClustersPerIndex) > 0 then
  begin
   Result:=AClustersPerIndex;
  end;
end;

{=============================================================================}

function TNTFSFileSystem.GetFilesPerCluster(AClustersPerFile:LongInt):LongWord;
{Calculate the number of file records per cluster}
{Only called during volume mount and initialization}
begin
 {}
 Result:=0;

 if FDriver = nil then Exit;
 if AClustersPerFile = 0 then Exit;

 if GetClustersPerFile(AClustersPerFile) > 1 then Exit;
 Result:=(FSectorSize * FSectorsPerCluster) div GetFileRecordSize(AClustersPerFile);
end;

{=============================================================================}

function TNTFSFileSystem.GetIndexsPerCluster(AClustersPerIndex:LongInt):LongWord;
{Calculate the number of index records per cluster}
{Only called during volume mount and initialization}
begin
 {}
 Result:=0;

 if FDriver = nil then Exit;
 if AClustersPerIndex = 0 then Exit;

 if GetClustersPerIndex(AClustersPerIndex) > 1 then Exit;
 Result:=(FSectorSize * FSectorsPerCluster) div GetIndexRecordSize(AClustersPerIndex);
end;

{=============================================================================}

function TNTFSFileSystem.GetBlockShiftCount(AClusterSize:LongWord):Word;
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

function TNTFSFileSystem.GetSectorShiftCount(ASectorsPerCluster:LongWord):Word;
{Calculate the sector shift count for sector to cluster conversion}
{Only called during volume mount and initialization}
begin
 {}
 Result:=0;

 if FDriver = nil then Exit;
 if ASectorsPerCluster = 0 then Exit;

 {Get the Shift Count}
 while (1 shl Result) < ASectorsPerCluster do
  begin
   Inc(Result);
  end;
end;

{=============================================================================}

function TNTFSFileSystem.GetClusterShiftCount(AClusterSize:LongWord):Word;
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

function TNTFSFileSystem.GetFileRecordShiftCount(AClusterSize,AFileRecordSize:LongWord):Word;
{Calculate the record shift count for record number to cluster conversion}
{Only called during volume mount and initialization}
begin
 {}
 Result:=0;

 if FDriver = nil then Exit;
 if AClusterSize = 0 then Exit;
 if AFileRecordSize = 0 then Exit;

 {Check the Sizes}
 if AFileRecordSize < AClusterSize then
  begin
   {Get the Shift Count}
   while (AFileRecordSize shl Result) < AClusterSize do
    begin
     Inc(Result);
    end;
  end
 else if AFileRecordSize > AClusterSize   then
  begin
   {Get the Shift Count}
   while (AFileRecordSize shr Result) > AClusterSize do
    begin
     Inc(Result);
    end;
  end;
end;

{=============================================================================}

function TNTFSFileSystem.GetFileRecordOffsetMask(AFilesPerCluster:LongWord):Int64;
{Calculate the mask for record number offset calculation}
{Only called during volume mount and initialization}
begin
 {}
 Result:=0;

 if FDriver = nil then Exit;
 if AFilesPerCluster = 0 then Exit;

 {Get the Mask}
 Result:=(AFilesPerCluster - 1);
end;

{=============================================================================}

function TNTFSFileSystem.GetIndexCounterShift(AIndexCounterOffset:LongWord):Word;
{Calculate the shift count for record number to counter conversion}
{Called during volume mount and initialization and index initialization}
begin
 {}
 Result:=0;

 if FDriver = nil then Exit;
 if AIndexCounterOffset = 0 then Exit;

 while (1 shl Result) < AIndexCounterOffset do
  begin
   Inc(Result);
  end;
end;

{=============================================================================}

function TNTFSFileSystem.GetIndexCounterOffset(AClustersPerIndex:LongWord):LongWord;
{Calculate the offset for record number increments}
{Called during volume mount and initialization}
begin
 {}
 Result:=0;

 if FDriver = nil then Exit;

 Result:=8;

 if AClustersPerIndex = 0 then Exit;

 {Get the Offset}
 Result:=AClustersPerIndex;
end;

{=============================================================================}

function TNTFSFileSystem.GetIndexRecordShiftCount(AClusterSize,AIndexRecordSize:LongWord):Word;
{Calculate the record shift count for record number to cluster conversion}
{Called during volume mount and initialization and index initialization}
begin
 {}
 Result:=0;

 if FDriver = nil then Exit;
 if AClusterSize = 0 then Exit;
 if AIndexRecordSize = 0 then Exit;

 {Check the Sizes}
 if AIndexRecordSize < AClusterSize then
  begin
   {Get the Shift Count}
   while (AIndexRecordSize shl Result) < AClusterSize do
    begin
     Inc(Result);
    end;
  end
 else if AIndexRecordSize > AClusterSize   then
  begin
   {Get the Shift Count}
   while (AIndexRecordSize shr Result) > AClusterSize do
    begin
     Inc(Result);
    end;
  end;
end;

{=============================================================================}

function TNTFSFileSystem.GetIndexRecordOffsetMask(AIndexsPerCluster:LongWord):Int64;
{Calculate the mask for record number offset calculation}
{Called during volume mount and initialization and index initialization}
begin
 {}
 Result:=0;

 if FDriver = nil then Exit;
 if AIndexsPerCluster = 0 then Exit;

 {Get the Mask}
 Result:=(AIndexsPerCluster - 1);
end;

{=============================================================================}

function TNTFSFileSystem.GetEntriesPerBlock(AClusterSize:LongWord):LongWord;
{Calculate the number of cluster entries per block of bitmap entries}
{Only called during volume mount and initialization}
begin
 {}
 Result:=(AClusterSize * 8); {Cluster sized blocks}
end;

{=============================================================================}

function TNTFSFileSystem.GetClustersPerBlock(AClusterSize:LongWord):LongWord;
{Calculate the number of clusters per block of bitmap entries}
{Only called during volume mount and initialization}
begin
 {}
 Result:=1; {Cluster sized blocks}
end;

{=============================================================================}

function TNTFSFileSystem.GetTotalBlockCount(const ATotalClusterCount:Int64):LongWord;
{Calculate the total number of bitmap entry blocks}
{Only called during volume mount and initialization}
var
 ClusterCount:Int64;
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

function TNTFSFileSystem.CalculateStartCluster(const ATotalClusterCount:Int64):Int64;
{Calculate the MTF start cluster}
{Only called during volume format and initialization}
begin
 {}
 Result:=ntfsUnknownCluster; {previously 0}

 if ATotalClusterCount = 0 then Exit;

 if ATotalClusterCount >= ntfsMftCutoverCount then
  begin
   {Get Mft Start}
   Result:=ntfsMftStartCluster;
  end
 else
  begin
   {Get Mft Start}
   Result:=(ATotalClusterCount div 3);
  end;
end;

{=============================================================================}

function TNTFSFileSystem.CalculateMirrorCluster(const ATotalClusterCount:Int64):Int64;
{Calculate the MTFMirr start cluster}
{Only called during volume format and initialization}
var
 Count:Int64;
begin
 {}
 Result:=ntfsUnknownCluster; {previously 0}

 if ATotalClusterCount = 0 then Exit;

 if FAltLayout then
  begin
   {Alternate Layout, under Vista/2008/7 Mirror Cluster is always immediately after $Boot (normally cluster 2)}
   {Get Boot Count}
   Count:=1;
   while (Count * FClusterSize) < ntfsDefaultBootRecordSize do
    begin
     Inc(Count);
    end;

   {Get Mirror Start}
   Result:=ntfsStartCluster + Count;
  end
 else
  begin
   {Get Mirror Start}
   Result:=(ATotalClusterCount div 2);
  end;
end;

{=============================================================================}

function TNTFSFileSystem.CalculateClustersPerFile(AFileRecordSize,AClusterSize:LongWord):LongWord;
{Calculate the number of clusters per file record}
{Only called during volume format and initialization}
begin
 {}
 Result:=0;

 if AClusterSize = 0 then Exit;
 if AFileRecordSize = 0 then Exit;

 while AClusterSize <= AFileRecordSize do
  begin
   Inc(Result);
   Dec(AFileRecordSize,AClusterSize);
  end;
end;

{=============================================================================}

function TNTFSFileSystem.CalculateClustersPerIndex(AIndexRecordSize,AClusterSize:LongWord):LongWord;
{Calculate the number of clusters per index record}
{Only called during volume format and initialization}
begin
 {}
 Result:=0;

 if AClusterSize = 0 then Exit;
 if AIndexRecordSize = 0 then Exit;

 while AClusterSize <= AIndexRecordSize do
  begin
   Inc(Result);
   Dec(AIndexRecordSize,AClusterSize);
  end;
end;

{=============================================================================}

function TNTFSFileSystem.CalculateFilesPerCluster(AFileRecordSize,AClusterSize:LongWord):LongWord;
{Calculate the number of file records per cluster}
{Only called during volume format and initialization}
begin
 {}
 Result:=0;

 if AClusterSize = 0 then Exit;
 if AFileRecordSize = 0 then Exit;

 while AFileRecordSize <= AClusterSize do
  begin
   Inc(Result);
   Dec(AClusterSize,AFileRecordSize);
  end;
end;

{=============================================================================}

function TNTFSFileSystem.CalculateIndexsPerCluster(AIndexRecordSize,AClusterSize:LongWord):LongWord;
{Calculate the number of index records per cluster}
{Only called during volume format and initialization}
begin
 {}
 Result:=0;

 if AClusterSize = 0 then Exit;
 if AIndexRecordSize = 0 then Exit;

 while AIndexRecordSize <= AClusterSize do
  begin
   Inc(Result);
   Dec(AClusterSize,AIndexRecordSize);
  end;
end;

{=============================================================================}

function TNTFSFileSystem.CalculateBpbClustersPerFile:LongInt;
{Determine the value of ClustersPerFile in Boot Sector BPB}
var
 Count:Integer;
begin
 {}
 Result:=0;

 if FFileRecordSize = 0 then Exit;

 {Check Clusters Per File}
 if FClustersPerFile > 0 then
  begin
   Result:=FClustersPerFile;
  end
 else
  begin
   {Check Files Per Cluster}
   if FFilesPerCluster > 0 then
    begin
     {Get Shift}
     Count:=0;
     while (FFileRecordSize shr Count) > 1 do
      begin
       Inc(Count);
      end;

     {Get Value}
     Result:=((0 - Count) and $000000FF);
    end;
  end;
end;

{=============================================================================}

function TNTFSFileSystem.CalculateBpbClustersPerIndex:LongInt;
{Determine the value of ClustersPerIndex in Boot Sector BPB}
var
 Count:Integer;
begin
 {}
 Result:=0;

 if FIndexRecordSize = 0 then Exit;

 {Check Clusters Per Index}
 if FClustersPerIndex > 0 then
  begin
   Result:=FClustersPerIndex;
  end
 else
  begin
   {Check Indexs Per Cluster}
   if FIndexsPerCluster > 0 then
    begin
     {Get Shift}
     Count:=0;
     while (FIndexRecordSize shr Count) > 1 do
      begin
       Inc(Count);
      end;

     {Get Value}
     Result:=((0 - Count) and $000000FF);
    end;
  end;
end;

{=============================================================================}

function TNTFSFileSystem.CalculateMftSize:Int64;
{Determine the size of $MFT}
begin
 {}
 Result:=0;

 if FFileRecordSize = 0 then Exit;

 {Get Mft Size}
 Result:=ntfsDefaultRecordCount * FFileRecordSize;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CalculateMftSize - Size = ' + IntToStr(Result));
 {$ENDIF}
end;

{=============================================================================}

function TNTFSFileSystem.CalculateMftMirrSize:Int64;
{Determine the size of $MFTMirr}
begin
 {}
 Result:=0;

 if FFileRecordSize = 0 then Exit;
 if FMftMirrorCount = 0 then Exit;

 {Get MftMirr Size}
 Result:=FMftMirrorCount * FFileRecordSize;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CalculateMftMirrSize - Size = ' + IntToStr(Result));
 {$ENDIF}
end;

{=============================================================================}

function TNTFSFileSystem.CalculateLogFileSize:Int64;
{Determine the size of $LogFile}
var
 Count:Integer;
 DiskSize:Int64;
begin
 {}
 Result:=0;

 if FTotalClusterCount = 0 then Exit;

 {Get Disk Size}
 DiskSize:=(FTotalClusterCount shl FClusterShiftCount);

 {Get Log Size}
 for Count:=ntfsMaxLogSize downto 0 do
  begin
   if DiskSize >= ntfsLogSizes[Count].DiskSize then
    begin
     Result:=ntfsLogSizes[Count].LogSize;

     {$IFDEF NTFS_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CalculateLogFileSize - Size = ' + IntToStr(Result));
     {$ENDIF}

     Exit;
    end;
  end;
end;

{=============================================================================}

function TNTFSFileSystem.CalculateAttrDefSize:Int64;
{Determine the size of $AttrDef}
begin
 {}
 Result:=0;

 case FNTFSType of
  ntNTFS12:Result:=ntfsFileSize12AttrDef;
  ntNTFS30:Result:=ntfsFileSize30AttrDef;
  ntNTFS31:Result:=ntfsFileSize31AttrDef;
 end;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CalculateAttrDefSize - Size = ' + IntToStr(Result));
 {$ENDIF}
end;

{=============================================================================}

function TNTFSFileSystem.CalculateBitmapSize:Int64;
{Determine the size of $Bitmap}
begin
 {}
 Result:=0;

 if FTotalClusterCount = 0 then Exit;

 {Get AttrDef Size}
 Result:=(FTotalClusterCount shr 3); {Divide by 8}

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CalculateBitmapSize - Size = ' + IntToStr(Result));
 {$ENDIF}
end;

{=============================================================================}

function TNTFSFileSystem.CalculateBootSize:Int64;
{Determine the size of $Boot}
begin
 {}
 Result:=ntfsDefaultBootRecordSize;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CalculateBootSize - Size = ' + IntToStr(Result));
 {$ENDIF}
end;

{=============================================================================}

function TNTFSFileSystem.CalculateBadClusBadSize:Int64;
{Determine the size of $BadClus:$Bad}
begin
 {}
 Result:=0;

 if FSectorSize = 0 then Exit;
 if FSectorCount = 0 then Exit;

 {Get BadClus Size}
 Result:=FSectorCount * FSectorSize;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CalculateBadClusBadSize - Size = ' + IntToStr(Result));
 {$ENDIF}
end;

{=============================================================================}

function TNTFSFileSystem.CalculateSecureSdsSize:Int64;
{Determine the size of $Secure:$SDS}
begin
 {}
 Result:=ntfsDefaultSecureSdsSize;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CalculateSecureSdsSize - Size = ' + IntToStr(Result));
 {$ENDIF}
end;

{=============================================================================}

function TNTFSFileSystem.CalculateUpCaseSize:Int64;
{Determine the size of $UpCase}
begin
 {}
 Result:=ntfsFileSizeUpCase;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CalculateUpCaseSize - Size = ' + IntToStr(Result));
 {$ENDIF}
end;

{=============================================================================}

function TNTFSFileSystem.CalculateRootAllocationSize:Int64;
{Determine the size of Root:$Allocation}
begin
 {}
 Result:=0;

 if FIndexRecordSize = 0 then Exit;

 //Result:=FIndexRecordSize; //To Do //Temporary - RootAllocation does not appear to be large enough ? - SetIndex adds 2 nodes !
                                           //AddKey does PushNode (on Root) then does SplitNode on next Add (Bitmap)
 Result:=FIndexRecordSize * 2;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CalculateRootAllocationSize - Size = ' + IntToStr(Result));
 {$ENDIF}
end;

{=============================================================================}

function TNTFSFileSystem.CalculateRootSecurityDescriptorSize:Int64;
{Determine the size of Root:$SecurityDescriptor}
begin
 {}
 Result:=ntfsDefaultDescriptors[ntfsDefaultDescriptorRoot].Size;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CalculateRootSecurityDescriptorSize - Size = ' + IntToStr(Result));
 {$ENDIF}
end;

{=============================================================================}

function TNTFSFileSystem.CalculateBitmapStartCluster:Int64;
{Determine the starting cluster of the $Bitmap}
{Follows Root:$SecurityDescriptor}
var
 Count:Int64;
 Cluster:Int64;
begin
 {}
 Result:=ntfsUnknownCluster; {previously 0}

 if FClusterSize = 0 then Exit;

 {Get RootSecurityDescriptor Start}
 Cluster:=CalculateRootSecurityDescriptorStartCluster;
 if Cluster = ntfsUnknownCluster then Exit;

 {Get RootSecurityDescriptor Size}
 Count:=NTFSRoundQuadWordToClusterSize(CalculateRootSecurityDescriptorSize,FClusterShiftCount,FClusterSize);
 if Count = 0 then Exit;

 {Get RootSecurityDescriptor Count}
 Count:=(Count shr FClusterShiftCount);
 Result:=Cluster + Count;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CalculateBitmapStartCluster - Cluster = ' + IntToStr(Result));
 {$ENDIF}
end;

{=============================================================================}

function TNTFSFileSystem.CalculateUpCaseStartCluster:Int64;
{Determine the starting cluster of the $UpCase}
{Follows $Bitmap}
var
 Count:Int64;
 Cluster:Int64;
begin
 {}
 Result:=ntfsUnknownCluster; {previously 0}

 if FClusterSize = 0 then Exit;

 {Get Bitmap Start}
 Cluster:=CalculateBitmapStartCluster;
 if Cluster = ntfsUnknownCluster then Exit;

 {Get Bitmap Size}
 Count:=NTFSRoundQuadWordToClusterSize(CalculateBitmapSize,FClusterShiftCount,FClusterSize);
 if Count = 0 then Exit;

 {Get Bitmap Count}
 Count:=(Count shr FClusterShiftCount);
 Result:=Cluster + Count;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CalculateUpCaseStartCluster - Cluster = ' + IntToStr(Result));
 {$ENDIF}
end;

{=============================================================================}

function TNTFSFileSystem.CalculateAttrDefStartCluster:Int64;
{Determine the starting cluster of the $AttrDef}
{Follows $Secure:$SDS}
var
 Count:Int64;
 Cluster:Int64;
begin
 {}
 Result:=ntfsUnknownCluster; {previously 0}

 if FClusterSize = 0 then Exit;

 {Get SecureSds Start}
 Cluster:=CalculateSecureSdsStartCluster;
 if Cluster = ntfsUnknownCluster then Exit;

 {Get SecureSds Size}
 Count:=NTFSRoundQuadWordToClusterSize(CalculateSecureSdsSize,FClusterShiftCount,FClusterSize);
 if Count = 0 then Exit;

 {Get SecureSds Count}
 Count:=(Count shr FClusterShiftCount);
 Result:=Cluster + Count;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CalculateAttrDefStartCluster - Cluster = ' + IntToStr(Result));
 {$ENDIF}
end;

{=============================================================================}

function TNTFSFileSystem.CalculateLogFileStartCluster:Int64;
{Determine the starting cluster of the $LogFile}
{Preceeds $Mft:$Bitmap}
var
 Count:Int64;
 Cluster:Int64;
begin
 {}
 Result:=ntfsUnknownCluster; {previously 0}

 if FClusterSize = 0 then Exit;
 if FMftStartCluster = 0 then Exit;
 if FMftStartCluster = ntfsUnknownCluster then Exit;

 {Get Mft Start}
 Cluster:=FMftStartCluster;

 {Get LogFile Size}
 Count:=NTFSRoundQuadWordToClusterSize(CalculateLogFileSize,FClusterShiftCount,FClusterSize);
 if Count = 0 then Exit;

 {Get LogFile Count}
 Count:=(Count shr FClusterShiftCount);
 Result:=Cluster - (Count + 4); {Allow 4 extra clusters}

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CalculateLogFileStartCluster - Cluster = ' + IntToStr(Result) + ' Count = ' + IntToStr(Count) + ' MFTStartCluster = ' + IntToStr(FMftStartCluster));
 {$ENDIF}
end;

{=============================================================================}

function TNTFSFileSystem.CalculateSecureSdsStartCluster:Int64;
{Determine the starting cluster of the $Secure:$SDS}
{Follows $UpCase (previously Follows MftZoneCluster)}
var
 Count:Int64;
 Cluster:Int64;
begin
 {}
 Result:=ntfsUnknownCluster; {previously 0}

 if FClusterSize = 0 then Exit;
 {if FMftZoneCluster = 0 then Exit;}

 {Get UpCase Start}                         {previously Get MftZone Start}
 Cluster:=CalculateUpCaseStartCluster;      {previously Cluster:=FMftZoneCluster;}
 if Cluster = ntfsUnknownCluster then Exit; {previously Result:=Cluster + 1;} {Allow 1 extra cluster}

 {Get UpCase Size}
 Count:=NTFSRoundQuadWordToClusterSize(CalculateUpCaseSize,FClusterShiftCount,FClusterSize);
 if Count = 0 then Exit;

 {Get UpCase Count}
 Count:=(Count shr FClusterShiftCount);
 Result:=Cluster + Count;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CalculateSecureSdsStartCluster - Cluster = ' + IntToStr(Result));
 {$ENDIF}
end;

{=============================================================================}

function TNTFSFileSystem.CalculateRootAllocationStartCluster:Int64;
{Determine the starting cluster of the Root:$Allocation}
{Follows $MftMirr}
var
 Cluster:Int64;
begin
 {}
 Result:=ntfsUnknownCluster; {previously 0}

 if FClusterSize = 0 then Exit;
 if FMftMirrorCluster = 0 then Exit;
 if FMftMirrorCluster = ntfsUnknownCluster then Exit;

 {Get MftMirr Start}
 Cluster:=FMftMirrorCluster;
 Result:=Cluster + 20; {Allow 20 extra clusters}

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CalculateRootAllocationStartCluster - Cluster = ' + IntToStr(Result));
 {$ENDIF}
end;

{=============================================================================}

function TNTFSFileSystem.CalculateRootSecurityDescriptorStartCluster:Int64;
{Determine the starting cluster of the Root:$SecurityDescriptor}
{Follows Root:$Allocation}
var
 Count:Int64;
 Cluster:Int64;
begin
 {}
 Result:=ntfsUnknownCluster; {previously 0}

 if FClusterSize = 0 then Exit;

 {Get RootAllocation Start}
 Cluster:=CalculateRootAllocationStartCluster;
 if Cluster = ntfsUnknownCluster then Exit;

 {Get RootAllocation Size}
 Count:=NTFSRoundQuadWordToClusterSize(CalculateRootAllocationSize,FClusterShiftCount,FClusterSize);
 if Count = 0 then Exit;

 {Get RootAllocation Count}
 Count:=(Count shr FClusterShiftCount);
 Result:=Cluster + Count;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CalculateRootSecurityDescriptorStartCluster - Cluster = ' + IntToStr(Result));
 {$ENDIF}
end;

{=============================================================================}

function TNTFSFileSystem.CheckLog:Boolean;
{Check if the $LogFile shows the volume is clean}
{Returns True if volume is clean}
var
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;

 Value:LongWord;
 Offset:LongWord;

 FirstLogClient:PNTFSLogClient;
 SecondLogClient:PNTFSLogClient;
 FirstRestartArea:PNTFSRestartArea;
 SecondRestartArea:PNTFSRestartArea;
 FirstRestartRecord:PNTFSRestartRecord;
 SecondRestartRecord:PNTFSRestartRecord;
 UpdateSeqeunceRecord:PNTFSUpdateSequenceRecord;
begin
 {}
 Result:=False;

 if not FEntries.ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FLogFile = nil then Exit;
  if FLogFile.Origin = nil then Exit;

  {Get Origin}
  if not FRecords.ReaderLock then Exit;
  try
   Origin:=FLogFile.Origin.Origin;
   if Origin = nil then Exit;

   {Get Attribute}
   Attribute:=Origin.GetAttribute(ntfsAttrTypeData,ntfsBlankName,ntfsInstanceFirst);
   if Attribute = nil then Exit;
  finally
   FRecords.ReaderUnlock;
  end;

  {Get First Restart Record}
  FirstRestartRecord:=AllocMem(ntfsRestartPageSize);
  try
   {Get Second Restart Record}
   SecondRestartRecord:=AllocMem(ntfsRestartPageSize);
   try
    Value:=0;
    Offset:=0;

    {Read First Restart Record}
    if ReadEntry(FRoot,FLogFile,FirstRestartRecord^,0,ntfsRestartPageSize,Offset,Value) = ntfsRestartPageSize then
     begin
      {Read Second Restart Record}
      if ReadEntry(FRoot,FLogFile,SecondRestartRecord^,ntfsRestartPageSize,ntfsRestartPageSize,Offset,Value) = ntfsRestartPageSize then
       begin
        {Check Magic}
        if (FirstRestartRecord.MagicNumber <> ntfsRestartSignature) and (FirstRestartRecord.MagicNumber <> ntfsCheckedSignature) then Exit;
        if (SecondRestartRecord.MagicNumber <> ntfsRestartSignature) and (SecondRestartRecord.MagicNumber <> ntfsCheckedSignature) then Exit;

        {Check Update Sequence Record}
        if FirstRestartRecord.UpdateSequenceOffset <> SecondRestartRecord.UpdateSequenceOffset then Exit;
        if FirstRestartRecord.UpdateSequenceLength <> SecondRestartRecord.UpdateSequenceLength then Exit;
        if FirstRestartRecord.UpdateSequenceOffset >= ntfsRestartPageSize then Exit;
        if SecondRestartRecord.UpdateSequenceOffset >= ntfsRestartPageSize then Exit;

        {Get Update Sequence Record}
        UpdateSeqeunceRecord:=PNTFSUpdateSequenceRecord(PtrUInt(FirstRestartRecord) + FirstRestartRecord.UpdateSequenceOffset);
        {Decode Fixup}
        if not ReadFixup(FirstRestartRecord,0,UpdateSeqeunceRecord.UpdateSequenceNumber,FirstRestartRecord.UpdateSequenceOffset,FirstRestartRecord.UpdateSequenceLength,False) then Exit;

        {Get Update Sequence Record}
        UpdateSeqeunceRecord:=PNTFSUpdateSequenceRecord(PtrUInt(SecondRestartRecord) + SecondRestartRecord.UpdateSequenceOffset);

        {Decode Fixup}
        if not ReadFixup(SecondRestartRecord,0,UpdateSeqeunceRecord.UpdateSequenceNumber,SecondRestartRecord.UpdateSequenceOffset,SecondRestartRecord.UpdateSequenceLength,False) then Exit;

        {Check Restart Record}
        if FirstRestartRecord.LastSequenceNumber <> SecondRestartRecord.LastSequenceNumber then Exit;
        if FirstRestartRecord.SystemPageSize <> SecondRestartRecord.SystemPageSize then Exit;
        if FirstRestartRecord.LogPageSize <> SecondRestartRecord.LogPageSize then Exit;
        if FirstRestartRecord.RestartAreaOffset <> SecondRestartRecord.RestartAreaOffset then Exit;
        if FirstRestartRecord.MinorVersion <> SecondRestartRecord.MinorVersion then Exit;
        if FirstRestartRecord.MajorVersion <> SecondRestartRecord.MajorVersion then Exit;
        if FirstRestartRecord.RestartAreaOffset >= ntfsRestartPageSize then Exit;
        if SecondRestartRecord.RestartAreaOffset >= ntfsRestartPageSize then Exit;

        {Get First Restart Area}
        FirstRestartArea:=PNTFSRestartArea(PtrUInt(FirstRestartRecord) + FirstRestartRecord.RestartAreaOffset);

        {Get Second Restart Area}
        SecondRestartArea:=PNTFSRestartArea(PtrUInt(SecondRestartRecord) + SecondRestartRecord.RestartAreaOffset);

        {Check Restart Area}
        if FirstRestartArea.CurrentSequenceNumber <> SecondRestartArea.CurrentSequenceNumber then Exit;
        if FirstRestartArea.LogClientCount <> SecondRestartArea.LogClientCount then Exit;
        if FirstRestartArea.FirstFreeClient <> SecondRestartArea.FirstFreeClient then Exit;
        if FirstRestartArea.FirstUsedClient <> SecondRestartArea.FirstUsedClient then Exit;
        if FirstRestartArea.Flags <> SecondRestartArea.Flags then Exit;
        if FirstRestartArea.SequenceNumberBits <> SecondRestartArea.SequenceNumberBits then Exit;
        if FirstRestartArea.RestartAreaLength <> SecondRestartArea.RestartAreaLength then Exit;
        if FirstRestartArea.ClientArrayOffset <> SecondRestartArea.ClientArrayOffset then Exit;
        if FirstRestartArea.FileSize <> SecondRestartArea.FileSize then Exit;
        if FirstRestartArea.LastSequenceDataLength <> SecondRestartArea.LastSequenceDataLength then Exit;
        if FirstRestartArea.LogRecordHeaderLength <> SecondRestartArea.LogRecordHeaderLength then Exit;
        if FirstRestartArea.LogPageDataOffset <> SecondRestartArea.LogPageDataOffset then Exit;
        if FirstRestartArea.LogFileOpenCount <> SecondRestartArea.LogFileOpenCount then Exit;

        {Get First Log Client}
        FirstLogClient:=PNTFSLogClient(PtrUInt(FirstRestartArea) + FirstRestartArea.ClientArrayOffset);

        {Get Second Log Client}
        SecondLogClient:=PNTFSLogClient(PtrUInt(SecondRestartArea) + SecondRestartArea.ClientArrayOffset);

        {Check Log Client}
        if FirstLogClient.OldestSequenceNumber <> SecondLogClient.OldestSequenceNumber then Exit;
        if FirstLogClient.CurrentSequenceNumber <> SecondLogClient.CurrentSequenceNumber then Exit;
        if FirstLogClient.PrevClient <> SecondLogClient.PrevClient then Exit;
        if FirstLogClient.NextClient <> SecondLogClient.NextClient then Exit;
        if FirstLogClient.SequenceNumber <> SecondLogClient.SequenceNumber then Exit;
        if FirstLogClient.ClientNameLength <> SecondLogClient.ClientNameLength then Exit;

        {Check Restart Flags}
        if (FirstRestartArea.Flags and ntfsLogFileRestartFlagClean) = ntfsLogFileRestartFlagClean then
         begin
          {Log is Clean}
          Result:=True;
         end
        else
         begin
          {Check Restart Clients}
          if (FirstRestartArea.FirstUsedClient = ntfsLogFileClientUnknown) and (FirstRestartArea.FirstFreeClient <> ntfsLogFileClientUnknown) and (FirstRestartArea.FirstFreeClient < FirstRestartArea.LogClientCount) then
           begin
            {Log is Clean}
            Result:=True;
           end;
         end;
       end;
     end;
   finally
    FreeMem(SecondRestartRecord);
   end;
  finally
   FreeMem(FirstRestartRecord);
  end;
 finally
  FEntries.ReaderUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.ClearLog(AForce:Boolean):Boolean;
{Reset the $LogFile to show the volume is clean}
var
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;

 Value:LongWord;
 Offset:LongWord;

 LogClient:PNTFSLogClient;
 RestartArea:PNTFSRestartArea;
 RestartRecord:PNTFSRestartRecord;
 UpdateSeqeunceRecord:PNTFSUpdateSequenceRecord;
begin
 {}
 Result:=False;

 if not FEntries.ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FLogFile = nil then Exit;
  if FLogFile.Origin = nil then Exit;

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get Origin}
  if not FRecords.ReaderLock then Exit;
  try
   Origin:=FLogFile.Origin.Origin;
   if Origin = nil then Exit;

   {Get Attribute}
   Attribute:=Origin.GetAttribute(ntfsAttrTypeData,ntfsBlankName,ntfsInstanceFirst);
   if Attribute = nil then Exit;
  finally
   FRecords.ReaderUnlock;
  end;

  {Get Restart Record}
  RestartRecord:=AllocMem(ntfsRestartPageSize);
  try
   Value:=0;
   Offset:=0;

   {Check Force}
   if AForce then
    begin
     {Create Log File}
     Result:=CreateLogFiles;
    end
   else
    begin
     {Clear Log File}
     {Read Restart Record}
     if ReadEntry(FRoot,FLogFile,RestartRecord^,0,ntfsRestartPageSize,Offset,Value) = ntfsRestartPageSize then
      begin
       {Check Magic}
       if (RestartRecord.MagicNumber <> ntfsRestartSignature) and (RestartRecord.MagicNumber <> ntfsCheckedSignature) then Exit;

       {Check Update Sequence Record}
       if RestartRecord.UpdateSequenceOffset >= ntfsRestartPageSize then Exit;

       {Get Update Sequence Record}
       UpdateSeqeunceRecord:=PNTFSUpdateSequenceRecord(PtrUInt(RestartRecord) + RestartRecord.UpdateSequenceOffset);
       {Decode Fixup}
       if not ReadFixup(RestartRecord,0,UpdateSeqeunceRecord.UpdateSequenceNumber,RestartRecord.UpdateSequenceOffset,RestartRecord.UpdateSequenceLength,False) then Exit;

       {Check Restart Record}
       if RestartRecord.RestartAreaOffset >= ntfsRestartPageSize then Exit;

       {Get Restart Area}
       RestartArea:=PNTFSRestartArea(PtrUInt(RestartRecord) + RestartRecord.RestartAreaOffset);

       {Get Log Client}
       LogClient:=PNTFSLogClient(PtrUInt(RestartArea) + RestartArea.ClientArrayOffset);

       {Reset Restart Record}
        {Nothing}

       {Reset Restart Area}
       RestartArea.FirstFreeClient:=ntfsLogFileClientFirst;
       RestartArea.FirstUsedClient:=ntfsLogFileClientUnknown;
       RestartArea.Flags:=RestartArea.Flags and not(ntfsLogFileRestartFlagClean); {Turn off the Clean bit}
       RestartArea.LastSequenceDataLength:=0;

       {Reset Log Client}
       LogClient.PrevClient:=ntfsLogFileClientUnknown;
       LogClient.NextClient:=ntfsLogFileClientUnknown;
       LogClient.SequenceNumber:=0;

       {Encode Fixup}
       if not WriteFixup(RestartRecord,0,UpdateSeqeunceRecord.UpdateSequenceNumber,RestartRecord.UpdateSequenceOffset,RestartRecord.UpdateSequenceLength) then Exit;

       {Write Restart Record}
       if WriteEntry(FRoot,FLogFile,RestartRecord^,0,ntfsRestartPageSize,Offset,Value) <> ntfsRestartPageSize then Exit;

       {Write Restart Record (Second)}
       if WriteEntry(FRoot,FLogFile,RestartRecord^,ntfsRestartPageSize,ntfsRestartPageSize,Offset,Value) <> ntfsRestartPageSize then Exit;

       Result:=True;
      end;
    end;
  finally
   FreeMem(RestartRecord);
  end;
 finally
  FEntries.ReaderUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.MarkDirty:Boolean;
{Mark the volume dirty in the volume flags}
begin
 {}
 Result:=False;

 if FNTFSType <> ntNONE then
  begin
   SetDirty(True);
   SetVolumeFlags(FVolumeFlags);
  end;
end;

{=============================================================================}

function TNTFSFileSystem.MarkClean:Boolean;
{Mark the volume clean in the volume flags}
begin
 {}
 Result:=False;

 if FNTFSType <> ntNONE then
  begin
   SetDirty(False);
   SetVolumeFlags(FVolumeFlags);
  end;
end;

{=============================================================================}

function TNTFSFileSystem.CheckClean:Boolean;
{Check the volume flags to see if the volume is clean}
{Returns True if volume is clean}
begin
 {}
 Result:=False;

 if FNTFSType <> ntNONE then
  begin
   Result:=GetDirty;
  end;
end;

{=============================================================================}

function TNTFSFileSystem.MarkMount:Boolean;
{Mark the Volume as Mounted (Dirty) / Check Dirty State}
begin
 {}
 Result:=False;

 if FNTFSType <> ntNONE then
  begin
   FMarkClean:=False;
   FMarkDirty:=False;
   FMarkError:=False;
   FLogDirty:=not(CheckLog);  {Get the current state of the LogFile}
   FMountDirty:=GetDirty;     {Get the current state of the Volume}

   Result:=MarkDirty;
  end;
end;

{=============================================================================}

function TNTFSFileSystem.MarkDismount:Boolean;
{Mark the Volume as Dismounted (Clean) / Check Mark Dirty, Mark Error and Mount Dirty}
begin
 {}
 Result:=False;

 if FNTFSType <> ntNONE then
  begin
   if (FMarkDirty or FMarkError or FMountDirty) and not(FMarkClean) then {if FMarkDirty or FMountDirty then}
    begin
     Result:=MarkDirty;

     FMarkClean:=False;
     FMarkDirty:=False;
     FMarkError:=False;
     FMountDirty:=False;
    end
   else
    begin
     Result:=MarkClean;

     FMarkClean:=False;
     FMarkDirty:=False;
     FMarkError:=False;
     FMountDirty:=False;
    end;

   if FLogDirty and FResetLog then
    begin
     if ClearLog(False) then Result:=True else Result:=ClearLog(True);

     FLogDirty:=False;
    end;
  end;
end;

{=============================================================================}

function TNTFSFileSystem.CheckVolumeName(const AName:String):Boolean;
{Note: This could be expanded to check if attribute will fit within a file record}
begin
 {}
 Result:=False;

 if Length(AName) > ntfsMaxVolumeName then Exit;

 Result:=True;
end;

{=============================================================================}

function TNTFSFileSystem.CheckAttributeName(const AName:String):Boolean;
{Note: This could be expanded to check if attribute will fit within a file record}
begin
 {}
 Result:=False;

 if Length(AName) > ntfsMaxAttributeName then Exit;

 Result:=True;
end;

{=============================================================================}

function TNTFSFileSystem.CompareSecurityDescriptor(ASecurityId:LongWord;ADescriptor:Pointer;ASize:Word):Boolean;
var
 Size:LongWord;
 Descriptor:Pointer;
 Security:TNTFSSecurity;
begin
 {}
 Result:=False;

 if FDriver = nil then Exit;
 if ADescriptor = nil then Exit;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CompareSecurityDescriptor - SecurityId = ' + IntToHex(ASecurityId,8) + ' Size = ' + IntToStr(ASize));
 {$ENDIF}

 {Get Security}
 Security:=GetSecurityById(ASecurityId);
 if Security = nil then Exit;

 {Get Size}
 Size:=Security.SecuritySize;

 {Get Descriptor}
 Descriptor:=Security.SecurityDescriptor;
 if Descriptor = nil then Exit;
 try
  {Compare Size}
  if Size <> ASize then Exit;

  {Compare Descriptor}
  if not CompareMem(Descriptor,ADescriptor,ASize) then Exit;

  Result:=True;
 finally
  {Release Descriptor}
  Security.ReleaseDescriptor(Descriptor,False,False);
 end;
end;

{=============================================================================}

function TNTFSFileSystem.NTFSTypeToNTFSVersion(ANTFSType:TNTFSType):Word;
begin
 {}
 Result:=ntfsNTFS31; {Default to NTFS 3.1}

 case ANTFSType of
  ntNTFS12:Result:=ntfsNTFS12;
  ntNTFS30:Result:=ntfsNTFS30;
  ntNTFS31:Result:=ntfsNTFS31;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.NTFSTypeToFileSysType(ANTFSType:TNTFSType):TFileSysType;
begin
 {}
 Result:=fsUNKNOWN; {Default to Unknown}

 case ANTFSType of
  ntNTFS12:Result:=fsNTFS;
  ntNTFS30:Result:=fsNTFS5;
  ntNTFS31:Result:=fsNTFS51;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.FileSysTypeToNTFSType(AFileSysType:TFileSysType):TNTFSType;
begin
 {}
 Result:=ntNONE; {Default to None}

 case AFileSysType of
  fsNTFS:Result:=ntNTFS12;
  fsNTFS5:Result:=ntNTFS30;
  fsNTFS51:Result:=ntNTFS31;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.LoadMaxFile:Integer;
begin
 {}
 Result:=ntfsMaxFile;
end;

{=============================================================================}

function TNTFSFileSystem.LoadMaxPath:Integer;
begin
 {}
 Result:=ntfsMaxPath;
end;

{=============================================================================}

function TNTFSFileSystem.LoadAttributes:LongWord;
begin
 {}
 Result:=inherited LoadAttributes;

 {Result:=(Result or vaCasePreserved);} {Now inbuilt}
 {Result:=(Result or vaUnicode);} {Now inbuilt}
 {Result:=(Result or vaPersistentAcls);} {Now inbuilt}
 {Result:=(Result or vaFileCompression);} {Now inbuilt}
 {Result:=(Result or vaVolumeQuotas);} {Now inbuilt}
 {Result:=(Result or vaSparseFiles);} {Now inbuilt}
 {Result:=(Result or vaObjectIds);} {Now inbuilt}
 {Result:=(Result or vaEncryption);} {Now inbuilt}
 {{Named Streams and Reparse Points handled by default}
end;

{=============================================================================}

function TNTFSFileSystem.LoadMountPointTag:LongWord;
begin
 {}
 Result:=ntfsReparseTagMountPoint;
end;

{=============================================================================}

function TNTFSFileSystem.LoadSymbolicLinkTag:LongWord;
begin
 {}
 Result:=ntfsReparseTagSymbolicLink;
end;

{=============================================================================}

function TNTFSFileSystem.LoadSystemName:String;
{Load System Name from $Boot file}
begin
 {}
 Result:=ntfsBlankName;

 if FDriver = nil then Exit;

 if not SectorLock then Exit;
 try
  if FSectorBuffer = nil then Exit;

  {Check Type}
  case FNTFSType of
   ntNTFS12,ntNTFS30,ntNTFS31:begin
     {Get BootSector}
     if not ReadSectors(FBootSector,1,FSectorBuffer^) then Exit;
     if PNtfsBootSector(FSectorBuffer).Signature <> BOOT_SECTOR_SIGNATURE then Exit;

     SetString(Result,PNtfsBootSector(FSectorBuffer).OEMName,8);

     Result:=Trim(Result);
    end;
  end;
 finally
  SectorUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.LoadVolumeName:String;
{Load Volume Name from $VOLUME_NAME attribute}
var
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=ntfsBlankName;

 if not FRecords.ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FVolInfo = nil then Exit;
  if FVolInfo.Origin = nil then Exit;

  {Get Origin}
  Origin:=FVolInfo.Origin.Origin;
  if Origin = nil then Exit;

  {Get Attribute}
  Attribute:=Origin.GetAttribute(ntfsAttrTypeVolumeName,ntfsBlankName,ntfsInstanceFirst);
  if Attribute = nil then Exit;

  {Get Volume Name}
  Result:=TNTFSVolumeNameAttribute(Attribute).VolumeName;
 finally
  FRecords.ReaderUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.LoadVolumeGUID:String;
{Load Volume GUID from $OBJECT_ID attribute}
var
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=ntfsBlankName;

 if not FRecords.ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FVolInfo = nil then Exit;
  if FVolInfo.Origin = nil then Exit;

  {Check Version}
  if FVolumeVersion < ntfsNTFS30 then Exit;

  {Get Origin}
  Origin:=FVolInfo.Origin.Origin;
  if Origin = nil then Exit;

  {Get Attribute}
  Attribute:=Origin.GetAttribute(ntfsAttrTypeObjectId,ntfsBlankName,ntfsInstanceFirst);
  if Attribute = nil then Exit;

  {Get Volume GUID}
  Result:=GUIDToString(TNTFSObjectIdAttribute(Attribute).ObjectId);
 finally
  FRecords.ReaderUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.LoadVolumeSerial:LongWord;
{Load Volume Serial from $Boot file}
begin
 {}
 Result:=0;

 if FDriver = nil then Exit;

 if not SectorLock then Exit;
 try
  if FSectorBuffer = nil then Exit;

  {Check Type}
  case FNTFSType of
   ntNTFS12,ntNTFS30,ntNTFS31:begin
     {Get BootSector}
     if not ReadSectors(FBootSector,1,FSectorBuffer^) then Exit;
     if PNtfsBootSector(FSectorBuffer).Signature <> BOOT_SECTOR_SIGNATURE then Exit;

     Result:=PNtfsBootSector(FSectorBuffer).BPB.VolumeSerial;
    end;
  end;
 finally
  SectorUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.LoadFileSysType:TFileSysType;
begin
 {}
 Result:=NTFSTypeToFileSysType(FNTFSType);
end;

{=============================================================================}

function TNTFSFileSystem.SetVolumeName(const AName:String):Boolean;
{Set Volume Name in $VOLUME_NAME attribute}
var
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FRecords.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FVolInfo = nil then Exit;
  if FVolInfo.Origin = nil then Exit;

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get Origin}
  Origin:=FVolInfo.Origin.Origin;
  if Origin = nil then Exit;

  {Get Attribute}
  Attribute:=Origin.GetAttribute(ntfsAttrTypeVolumeName,ntfsBlankName,ntfsInstanceFirst);
  if Attribute = nil then Exit;

  {Check Volume Name}
  if not CheckVolumeName(AName) then Exit;

  {Set Volume Name}
  TNTFSVolumeNameAttribute(Attribute).VolumeName:=AName;
  FVolumeName:=AName;
  UniqueString(FVolumeName);

  {Size Attribute}
  if not SizeAttribute(Origin,Attribute,Attribute.CalculatedStreamSize(FVolumeVersion)) then Exit;

  {Set Records}
  Result:=SetRecords(Origin);
 finally
  FRecords.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.SetVolumeSerial(ASerial:LongWord):Boolean;
{Set Volume Serial in $Boot file}
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
  case FNTFSType of
   ntNTFS12,ntNTFS30,ntNTFS31:begin
     {Get/Set BootSector}
     if not ReadSectors(FBootSector,1,FSectorBuffer^) then Exit;
     if PNtfsBootSector(FSectorBuffer).Signature <> BOOT_SECTOR_SIGNATURE then Exit;
     PNtfsBootSector(FSectorBuffer).BPB.VolumeSerial:=ASerial;
     if not WriteSectors(FBootSector,1,FSectorBuffer^) then Exit;

     {Get/Set BootBackup}
     if FBootBackup <> FBootSector then
      begin
       if not ReadSectors(FBootBackup,1,FSectorBuffer^) then Exit;
       if PNtfsBootSector(FSectorBuffer).Signature <> BOOT_SECTOR_SIGNATURE then Exit;
       PNtfsBootSector(FSectorBuffer).BPB.VolumeSerial:=ASerial;
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

{=============================================================================}

function TNTFSFileSystem.ReadEntry(AParent,AEntry:TDiskEntry;var ABuffer;const AStart:Int64;ACount:LongWord;var AOffset,AValue:LongWord):Integer;
{Read count bytes from the supplied entry beginning at the supplied start}
{Note: The caller must ensure the entry is large enough or the read will fail}
{Note: The offset parameter is not used by NTFS, value stores the attribute instance}
var
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=0;

 if not FEntries.ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if AParent = nil then Exit;
  if AEntry = nil then Exit;
  if AEntry = FRoot then Exit; {Cannot read root}

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.ReadEntry - Entry = ' + AEntry.Name + ' Start = ' + IntToStr(AStart) + ' Count = ' + IntToStr(ACount));
  {$ENDIF}

  {Check Relative}
  if (AEntry.Attributes and (faDot or faDotDot)) <> faNone then Exit;

  {Get Origin}
  if not FRecords.ReaderLock then Exit;
  try
   if TNTFSDiskEntry(AEntry).Origin = nil then Exit;
   Origin:=TNTFSDiskEntry(AEntry).Origin.Origin;
   if Origin = nil then Exit;

   {Check Stream}
   if (AEntry.Attributes and faMatchMask) = faStream then
    begin
     {Get Attribute}
     Attribute:=TNTFSDiskEntry(AEntry).Attribute; {Will always be the first instance due to LoadEntries}
     if Attribute = nil then Exit;
    end
   else
    begin
     {Check Entry}
     if (AEntry.Attributes and faMatchMask) <> faFile then Exit;

     {Get Attribute}
     Attribute:=Origin.GetAttribute(ntfsAttrTypeData,ntfsBlankName,ntfsInstanceFirst); {Must pass first instance}
     if Attribute = nil then Exit;
    end;

   {Read Attribute}
   Result:=ReadAttribute(Origin,Attribute,ABuffer,AStart,ACount,AValue,False);
  finally
   FRecords.ReaderUnlock;
  end;
 finally
  FEntries.ReaderUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.WriteEntry(AParent,AEntry:TDiskEntry;const ABuffer;const AStart:Int64;ACount:LongWord;var AOffset,AValue:LongWord):Integer;
{Write count bytes to the supplied entry beginning at the supplied start}
{Note: The caller must ensure the entry is large enough or the write will fail}
{Note: The offset parameter is not used by NTFS, value stores the attribute instance}
var
 Entry:TNTFSDiskEntry;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=0;

 if not FEntries.ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if AParent = nil then Exit;
  if AEntry = nil then Exit;
  if AEntry = FRoot then Exit; {Cannot write root}

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.WriteEntry - Entry = ' + AEntry.Name + ' Start = ' + IntToStr(AStart) + ' Count = ' + IntToStr(ACount));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Relative}
  if (AEntry.Attributes and (faDot or faDotDot)) <> faNone then Exit;

  {Get Origin}
  if not FRecords.ReaderLock then Exit;
  try
   if TNTFSDiskEntry(AEntry).Origin = nil then Exit;
   Origin:=TNTFSDiskEntry(AEntry).Origin.Origin;
   if Origin = nil then Exit;

   {Check Stream}
   if (AEntry.Attributes and faMatchMask) = faStream then
    begin
     {Get Attribute}
     Attribute:=TNTFSDiskEntry(AEntry).Attribute; {Will always be the first instance due to LoadEntries}
     if Attribute = nil then Exit;

     {Convert Lock}
     if not FRecords.ReaderConvert then Exit;

     {Write Attribute}
     Result:=WriteAttribute(Origin,Attribute,ABuffer,AStart,ACount,AValue,True);

     {Check Result}
     if (LongWord(Result) = ACount) and (Attribute.IsCompressed) then
      begin
       {Update Streams} {Streams belonging to other Links}
       if Origin.Streams <> nil then
        begin
         Entry:=Origin.Streams.FirstEntry;
         while Entry <> nil do
          begin
           if (Entry <> AEntry) and (Entry.Attribute = Attribute) then Attribute.UpdateEntry(Entry);

           Entry:=Entry.NextEntry;
          end;
        end;

       {Update Entry}
       Attribute.UpdateEntry(TNTFSDiskEntry(AEntry));
      end;

     {Convert Lock}
     if not FRecords.WriterConvert then Exit;
    end
   else
    begin
     {Check Entry}
     if (AEntry.Attributes and faMatchMask) <> faFile then Exit;

     {Get Attribute}
     Attribute:=Origin.GetAttribute(ntfsAttrTypeData,ntfsBlankName,ntfsInstanceFirst); {Must pass first instance}
     if Attribute = nil then Exit;

     {Convert Lock}
     if not FRecords.ReaderConvert then Exit;

     {Write Attribute}
     Result:=WriteAttribute(Origin,Attribute,ABuffer,AStart,ACount,AValue,True);

     {Check Result}
     if (LongWord(Result) = ACount) and (Attribute.IsCompressed) then
      begin
       {Update Links} {Links belonging to other Entries}
       if Origin.Links <> nil then
        begin
         Entry:=Origin.Links.FirstEntry;
         while Entry <> nil do
          begin
           if Entry <> AEntry then Attribute.UpdateEntry(Entry);

           Entry:=Entry.NextEntry;
          end;
        end;

       {Update Entry}
       Attribute.UpdateEntry(TNTFSDiskEntry(AEntry));
      end;

     {Convert Lock}
     if not FRecords.WriterConvert then Exit;
    end;
  finally
   FRecords.ReaderUnlock;
  end;
 finally
  FEntries.ReaderUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.ReadAttribute(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute;var ABuffer;const AStart:Int64;ACount:LongWord;var AInstance:LongWord;AWrite:Boolean):Integer;
{Read count bytes from the supplied attribute beginning at the supplied start}
{Handles partial reads at start and end of buffer and byte to vcn translations}
{Note: The attribute can be of any type (Named or Unnamed) but will most commonly be data, allocation or bitmap
       For resident attributes it must be a type that loads the Data member when reading the record}
{Note: The caller must ensure the attribute is large enough or the read will fail}
{Note: The passed attribute must be the first instance, instance contains the instance number}
{Note: Caller must hold the records lock}
var
 VCN:Int64;             {Virtual cluster number to Read from Run}
 Length:LongWord;       {Number of whole clusters to Read from Run}

 Start:LongWord;        {Starting offset for Read from Cluster}
 Count:LongWord;        {Count of bytes to Read from Cluster}
 Remain:LongWord;       {Remaining bytes to Write to Buffer}
 Offset:LongWord;       {Offset for Write to Buffer}
begin
 {}
 Result:=0;

 if AWrite then
  begin
   if not FRecords.AttributesWriterLock then Exit;
  end
 else
  begin
   if not FRecords.AttributesReaderLock then Exit;
  end;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if AAttribute = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.ReadAttribute - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' Type = ' + IntToHex(AAttribute.AttributeType,8) + ' Attribute = ' + AAttribute.AttributeName + ' Start = ' + IntToStr(AStart) + ' Count = ' + IntToStr(ACount));
  {$ENDIF}

  {Check Count} {Will succeed as Zero}
  if ACount = 0 then Exit;

  {Check Resident}
  if AAttribute.NonResident = ntfsAttributeResident then
   begin
    {Check Data}
    if AAttribute.Data = nil then Exit;

    {Check Start}
    if AStart >= AAttribute.DataSize then Exit;

    {Check Count}
    if (AStart + ACount) > AAttribute.DataSize then Exit;

    {Read Data}
    System.Move(Pointer(PtrUInt(AAttribute.Data) + PtrUInt(AStart))^,ABuffer,ACount);
    AInstance:=ntfsInstanceFirst;

    Result:=ACount;
   end
  else
   begin
    {Get VCN}
    VCN:=(AStart shr FClusterShiftCount);

    {Get Position}
    Offset:=0;
    Remain:=ACount;
    Start:=(AStart - (VCN shl FClusterShiftCount));
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

         {Read Run}
         if ReadRun(ARecord,AAttribute,FReadBuffer^,VCN,1,AInstance,False,AWrite) <> 1 then Break;

         {Read Data}
         System.Move(Pointer(PtrUInt(FReadBuffer) + Start)^,Pointer(PtrUInt(@ABuffer) + Offset)^,Count);

         {Update VCN}
         Inc(VCN);

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
          {Read Run}
          if ReadRun(ARecord,AAttribute,Pointer(PtrUInt(@ABuffer) + Offset)^,VCN,Length,AInstance,False,AWrite) <> Length then Break;

          {Update VCN}
          Inc(VCN,Length);

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

           {Read Run}
           if ReadRun(ARecord,AAttribute,FReadBuffer^,VCN,1,AInstance,False,AWrite) <> 1 then Break;

           {Read Data}
           System.Move(FReadBuffer^,Pointer(PtrUInt(@ABuffer) + Offset)^,Count);

           {Update VCN}
           {Inc(VCN);} {Must be last Read}

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
   end;
 finally
  if AWrite then
   begin
    FRecords.AttributesWriterUnlock;
   end
  else
   begin
    FRecords.AttributesReaderUnlock;
   end;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.WriteAttribute(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute;const ABuffer;const AStart:Int64;ACount:LongWord;var AInstance:LongWord;AUpdate:Boolean):Integer;
{Write count bytes to the supplied attribute beginning at the supplied start}
{Handles partial writes at start and end of buffer and byte to vcn translations}
{Note: The attribute can be of any type (Named or Unnamed) but will most commonly be data, allocation or bitmap
       For resident attributes it must be a type that loads the Data member when reading the record (Data, Bitmap, Allocation, Unknown etc)}
{Note: The caller must ensure the attribute is large enough or the write will fail}
{Note: The passed attribute must be the first instance, instance contains the instance number}
{Note: Update records on disk if requested by the caller}
{Note: Caller must hold the records lock}
var
 VCN:Int64;               {Virtual cluster number to Write to Run}
 Length:LongWord;         {Number of whole clusters to Write to Run}

 Start:LongWord;          {Starting offset for Write to Cluster}
 Count:LongWord;          {Count of bytes to Write to Cluster}
 Remain:LongWord;         {Remaining bytes to Read from Buffer}
 Offset:LongWord;         {Offset for Read from Buffer}

 Current:TNTFSDiskRecord; {The parent record of the current attribute}
begin
 {}
 Result:=0;

 if not FRecords.AttributesWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if AAttribute = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.WriteAttribute - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' Type = ' + IntToHex(AAttribute.AttributeType,8) + ' Attribute = ' + AAttribute.AttributeName + ' Start = ' + IntToStr(AStart) + ' Count = ' + IntToStr(ACount) + ' Update = ' + BoolToStr(AUpdate));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Count} {Will succeed as Zero}
  if ACount = 0 then Exit;

  {Check Resident}
  if AAttribute.NonResident = ntfsAttributeResident then
   begin
    {Get Record}
    Current:=AAttribute.Parent;
    if Current = nil then Exit;

    {Check Data}
    if AAttribute.Data = nil then Exit;

    {Check Start}
    if AStart >= AAttribute.DataSize then Exit;

    {Check Count}
    if (AStart + ACount) > AAttribute.DataSize then Exit;

    {Write Data}
    System.Move(ABuffer,Pointer(PtrUInt(AAttribute.Data) + PtrUInt(AStart))^,ACount);

    {Set Record}
    if AUpdate then if not SetRecord(Current) then Exit;
    AInstance:=ntfsInstanceFirst;

    Result:=ACount;
   end
  else
   begin
    {Get VCN}
    VCN:=(AStart shr FClusterShiftCount);

    {Get Position}
    Offset:=0;
    Remain:=ACount;
    Start:=(AStart - (VCN shl FClusterShiftCount));
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

         {Read Run}
         if ReadRun(ARecord,AAttribute,FWriteBuffer^,VCN,1,AInstance,False,True) <> 1 then Break;

         {Write Data}
         System.Move(Pointer(PtrUInt(@ABuffer) + Offset)^,Pointer(PtrUInt(FWriteBuffer) + Start)^,Count);

         {Write Run}
         if WriteRun(ARecord,AAttribute,FWriteBuffer^,VCN,1,AInstance,False,AUpdate) <> 1 then Break;

         {Update VCN}
         Inc(VCN);

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
          {Write Run}
          if WriteRun(ARecord,AAttribute,Pointer(PtrUInt(@ABuffer) + Offset)^,VCN,Length,AInstance,False,AUpdate) <> Length then Break;

          {Update VCN}
          Inc(VCN,Length);

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

           {Read Run}
           if ReadRun(ARecord,AAttribute,FWriteBuffer^,VCN,1,AInstance,False,True) <> 1 then Break;

           {Write Data}
           System.Move(Pointer(PtrUInt(@ABuffer) + Offset)^,FWriteBuffer^,Count);

           {Write Run}
           if WriteRun(ARecord,AAttribute,FWriteBuffer^,VCN,1,AInstance,False,AUpdate) <> 1 then Break;

           {Update VCN}
           {Inc(VCN);} {Must be last Write}

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
   end;
 finally
  FRecords.AttributesWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.ReadRun(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute;var ABuffer;const AVCN:Int64;ACount:LongWord;var AInstance:LongWord;ARaw,AWrite:Boolean):LongWord;
{Read count clusters from the supplied attribute starting at the supplied vcn}
{Handles multiple cluster reads, sparse, compressed and multiple instance runs}
{Note: The caller must ensure the run is large enough or the read will fail}
{Note: The passed attribute must be the first instance, instance contains last used instance number}
{Note: The raw parameter determines if the read should account for Compressed, Encrypted or just read the data}
{Note: Caller must hold the records and attributes lock}
var
 VCN:Int64;                     {VCN of current cluster to Read from Run}
 Length:Int64;                  {Number of clusters available in current Run}
 Cluster:Int64;                 {Absolute Cluster of current Run to Read}

 Count:LongWord;                {Count of clusters to Read from Run}
 Remain:LongWord;               {Remaining clusters to Write to Buffer}
 Offset:LongWord;               {Byte Offset for Write to Buffer}

 UnitNo:Int64;                  {Compression Unit for current VCN}
 UnitVCN:Int64;                 {Starting VCN of Compression Unit}
 UnitCount:Int64;               {Count of Clusters available in Unit}
 UnitLength:Int64;              {Length of Compression Unit in Clusters}
 UnitRemain:Int64;              {Bytes remaining in Compression Unit}
 UnitOffset:LongWord;           {Byte Offset into Decompressed Data}
 CompressBuffer:Pointer;        {Buffer for Raw read from Run}
 DecompressBuffer:Pointer;      {Buffer for Decompressed Data}

 Attribute:TNTFSDiskAttribute;  {Attribute instance containing Run to Read}
begin
 {}
 Result:=0;

 if AWrite then
  begin
   if not FRecords.RunsWriterLock then Exit;
  end
 else
  begin
   if not FRecords.RunsReaderLock then Exit;
  end;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if AAttribute = nil then Exit;

  //To Do //Handling of InitializedSize

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.ReadRun - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' Type = ' + IntToHex(AAttribute.AttributeType,8) + ' Attribute = ' + AAttribute.AttributeName + ' VCN = ' + IntToStr(AVCN) + ' Count = ' + IntToStr(ACount) + ' Raw = ' + BoolToStr(ARaw));
  {$ENDIF}

  {Check Resident}
  if AAttribute.NonResident = ntfsAttributeNonResident then
   begin
    {Check Compressed}
    if (ARaw = False) and (AAttribute.IsCompressed) and (AAttribute.CompressionUnit <> 0) then
     begin
      {Compressed Run}
      {Get Buffer}
      CompressBuffer:=AllocCompressionBuffer(AAttribute.CompressionUnit,False);
      if CompressBuffer = nil then Exit;
      try
       {Get Buffer}
       DecompressBuffer:=AllocDecompressionBuffer(AAttribute.CompressionUnit,False);
       if DecompressBuffer = nil then Exit;
       try
        {Get Position}
        VCN:=AVCN;
        Offset:=0;
        Remain:=ACount;

        {Read Clusters}
        while Remain > 0 do
         begin
          {Get Unit}
          if not GetRunUnit(AAttribute,VCN,UnitNo,UnitCount) then Break;

          {Get Count}
          Count:=Remain;
          if Count > UnitCount then Count:=UnitCount; {Min does not support Int64}

          {$IFDEF NTFS_DEBUG}
          if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.ReadRun - VCN = ' + IntToStr(VCN) + ' UnitNo = ' + IntToStr(UnitNo) + ' UnitCount = ' + IntToStr(UnitCount));
          {$ENDIF}

          {Check Unit}
          if GetUnitCompressed(ARecord,AAttribute,UnitNo,UnitVCN,UnitLength) then
           begin
            {Compressed Unit}
            {$IFDEF NTFS_DEBUG}
            if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.ReadRun - UnitVCN = ' + IntToStr(UnitVCN) + ' UnitLength = ' + IntToStr(UnitLength) + ' Count = ' + IntToStr(Count));
            {$ENDIF}

            {Read Run} {Read entire unit to zero fill unused buffer space}
            if ReadRun(ARecord,AAttribute,CompressBuffer^,UnitVCN,UnitLength,AInstance,True,AWrite) <> UnitLength then Break;

            {Get Remain}
            UnitRemain:=Min64(Count shl FClusterShiftCount,AAttribute.StreamSize - (VCN shl FClusterShiftCount));

            {$IFDEF NTFS_DEBUG}
            if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.ReadRun - UnitRemain = ' + IntToStr(UnitRemain));
            {$ENDIF}

            {Decompress Run} {Decompress only the required portion of buffer}
            if not NTFSDecompressUnit(CompressBuffer,DecompressBuffer,FClusterSize,AAttribute.CompressionUnit,(VCN - UnitVCN),Count,UnitRemain) then Break;

            {Read Data}
            UnitOffset:=((VCN - UnitVCN) shl FClusterShiftCount);
            System.Move(Pointer(PtrUInt(DecompressBuffer) + UnitOffset)^,Pointer(PtrUInt(@ABuffer) + Offset)^,(Count shl FClusterShiftCount));
           end
          else
           begin
            {Uncompressed Unit}
            {Read Run}
            if ReadRun(ARecord,AAttribute,Pointer(PtrUInt(@ABuffer) + Offset)^,VCN,Count,AInstance,True,AWrite) <> Count then Break;
           end;

          {Update Position}
          Inc(VCN,Count);
          Inc(Offset,(Count shl FClusterShiftCount));
          Dec(Remain,Count);
         end;

        Result:=(ACount - Remain);
       finally
        {Release Buffer}
        ReleaseDecompressionBuffer(AAttribute.CompressionUnit,DecompressBuffer);
       end;
      finally
       {Release Buffer}
       ReleaseCompressionBuffer(AAttribute.CompressionUnit,CompressBuffer);
      end;
     end
    else
     begin
      {Uncompressed Run (or Raw Read)}
      {Get Position}
      VCN:=AVCN;
      Offset:=0;
      Remain:=ACount;

      {Read Clusters}
      while Remain > 0 do
       begin
        {Get Attribute}
        Attribute:=ARecord.GetAttributeByVCN(AAttribute,VCN,AInstance);
        if Attribute = nil then Break;

        {Get Cluster}
        if not GetRunCluster(Attribute,VCN,Cluster,Length) then Break;

        {Get Count}
        Count:=Remain;
        if Count > Length then Count:=Length; {Min does not support Int64}

        {$IFDEF NTFS_DEBUG}
        if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.ReadRun - VCN = ' + IntToStr(VCN) + ' Instance = ' + IntToStr(AInstance) + ' Cluster = ' + IntToStr(Cluster) + ' Length = ' + IntToStr(Length) + ' Count = ' + IntToStr(Count));
        {$ENDIF}

        {Check Cluster}
        if Cluster = ntfsUnknownCluster then
         begin
          {Sparse Run}
          {Fill Buffer}
          FillChar(Pointer(PtrUInt(@ABuffer) + Offset)^,(Count shl FClusterShiftCount),0);
         end
        else
         begin
          {Normal Run}
          {Read Clusters}
          if not ReadClusters(Cluster,Count,Pointer(PtrUInt(@ABuffer) + Offset)^) then Break;
         end;

        {Update Position}
        Inc(VCN,Count);
        Inc(Offset,(Count shl FClusterShiftCount));
        Dec(Remain,Count);
       end;

      Result:=(ACount - Remain);
     end;
   end;
 finally
  if AWrite then
   begin
    FRecords.RunsWriterUnlock;
   end
  else
   begin
    FRecords.RunsReaderUnlock;
   end;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.WriteRun(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute;const ABuffer;const AVCN:Int64;ACount:LongWord;var AInstance:LongWord;ARaw,AUpdate:Boolean):LongWord;
{Write count clusters to the supplied attribute starting at the supplied vcn}
{Handles multiple cluster writes, sparse, compressed and multiple instance runs}
{Note: The caller must ensure the run is large enough or the write will fail}
{Note: The passed attribute must be the first instance, instance contains last used instance number}
{Note: The raw parameter determines if the write should account for Compressed, Encrypted or just write the data}
{Note: Update records on disk if requested by the caller}
{Note: Caller must hold the records and attributes lock}
var
 VCN:Int64;                     {VCN of current cluster to Write to Run}
 Length:Int64;                  {Number of clusters available in current Run}
 Cluster:Int64;                 {Absolute Cluster of current Run to Write}

 Count:LongWord;                {Count of clusters to Write to Run}
 Remain:LongWord;               {Remaining clusters to Read from Buffer}
 Offset:LongWord;               {Offset for Read from Buffer}

 UnitNo:Int64;                  {Compression Unit for current VCN}
 UnitVCN:Int64;                 {Starting VCN of Compression Unit}
 UnitCount:Int64;               {Count of Clusters available in Unit}
 UnitLength:Int64;              {Length of Compression Unit in Clusters}
 UnitRemain:Int64;              {Bytes remaining in Compression Unit}
 UnitOffset:LongWord;           {Byte Offset into Decompressed Data}
 CompressBuffer:Pointer;        {Buffer for Compressed Data}
 DecompressBuffer:Pointer;      {Buffer for Raw write to Run}

 UnitSize:LongWord;             {Size in bytes of Compressed Data}
 UnitClusters:Int64;            {Size in clusters of Compressed Data}

 Attribute:TNTFSDiskAttribute;  {Attribute instance containing Run to Write}
begin
 {}
 Result:=0;

 if not FRecords.RunsWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if AAttribute = nil then Exit;

  //To Do //Handling of InitializedSize

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.WriteRun - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' Type = ' + IntToHex(AAttribute.AttributeType,8) + ' Attribute = ' + AAttribute.AttributeName + ' VCN = ' + IntToStr(AVCN) + ' Count = ' + IntToStr(ACount) + ' Raw = ' + BoolToStr(ARaw) + ' Update = ' + BoolToStr(AUpdate));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Resident}
  if AAttribute.NonResident = ntfsAttributeNonResident then
   begin
    {Check Compressed}
    if (ARaw = False) and (AAttribute.IsCompressed) and (AAttribute.CompressionUnit <> 0) then
     begin
      {Compressed Run}
      {Get Buffer}
      CompressBuffer:=AllocCompressionBuffer(AAttribute.CompressionUnit,False);
      if CompressBuffer = nil then Exit;
      try
       {Get Buffer}
       DecompressBuffer:=AllocDecompressionBuffer(AAttribute.CompressionUnit,False);
       if DecompressBuffer = nil then Exit;
       try
        {Get Position}
        VCN:=AVCN;
        Offset:=0;
        Remain:=ACount;

        {Write Clusters}
        while Remain > 0 do
         begin
          {Get Unit}
          if not GetRunUnit(AAttribute,VCN,UnitNo,UnitCount) then Break;

          {Get Count}
          Count:=Remain;
          if Count > UnitCount then Count:=UnitCount; {Min does not support Int64}

          {$IFDEF NTFS_DEBUG}
          if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.WriteRun - VCN = ' + IntToStr(VCN) + ' UnitNo = ' + IntToStr(UnitNo) + ' UnitCount = ' + IntToStr(UnitCount));
          {$ENDIF}

          {Check Unit}
          if GetUnitCompressed(ARecord,AAttribute,UnitNo,UnitVCN,UnitLength) then
           begin
            {Compressed Unit}
            {$IFDEF NTFS_DEBUG}
            if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.WriteRun - UnitVCN = ' + IntToStr(UnitVCN) + ' UnitLength = ' + IntToStr(UnitLength) + ' Count = ' + IntToStr(Count));
            {$ENDIF}

            {Get Remain}
            UnitRemain:=Min64(UnitCount shl FClusterShiftCount,AAttribute.StreamSize - (VCN shl FClusterShiftCount));

            {$IFDEF NTFS_DEBUG}
            if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.WriteRun - UnitRemain = ' + IntToStr(UnitRemain));
            {$ENDIF}

            {Check Partial}
            if (VCN > UnitVCN) or (Count < UnitCount) then
             begin
              {Read Run} {Read entire unit to zero fill unused buffer space}
              if ReadRun(ARecord,AAttribute,CompressBuffer^,UnitVCN,UnitLength,AInstance,True,True) <> UnitLength then Break;

              {Decompress Run} {Decompress from required VCN to end of buffer}
              if not NTFSDecompressUnit(CompressBuffer,DecompressBuffer,FClusterSize,AAttribute.CompressionUnit,(VCN - UnitVCN),UnitCount,UnitRemain) then Break;
             end
            else
             begin
              {Zero Buffer}
              FillChar(DecompressBuffer^,(UnitLength shl FClusterShiftCount),0);
             end;

            {Write Data}
            UnitOffset:=((VCN - UnitVCN) shl FClusterShiftCount);
            System.Move(Pointer(PtrUInt(@ABuffer) + Offset)^,Pointer(PtrUInt(DecompressBuffer) + UnitOffset)^,(Count shl FClusterShiftCount));

            {Compress Run} {Compress from required VCN to end of buffer}
            if NTFSCompressUnit(DecompressBuffer,CompressBuffer,FClusterSize,AAttribute.CompressionUnit,(VCN - UnitVCN),UnitCount,UnitRemain) then
             begin
              {Compressable Unit} {Update Run as Compressed and Write Data}
              {Get Size}
              UnitSize:=NTFSGetUnitUsed(CompressBuffer,FClusterSize,AAttribute.CompressionUnit);
              UnitClusters:=(UnitSize shr FClusterShiftCount);
              if (UnitClusters shl FClusterShiftCount) < UnitSize then Inc(UnitClusters);

              {$IFDEF NTFS_DEBUG}
              if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.WriteRun - UnitSize = ' + IntToStr(UnitSize) + ' UnitClusters = ' + IntToStr(UnitClusters));
              {$ENDIF}

              {Convert Run} {To Sparse}
              if not ConvertRun(ARecord,AAttribute,(UnitVCN + UnitClusters),(UnitLength - UnitClusters),True) then Break;

              {Write Run} {Only write used clusters so that WriteRun does not convert sparse clusters}
              if WriteRun(ARecord,AAttribute,CompressBuffer^,UnitVCN,UnitClusters,AInstance,True,False) <> UnitClusters then Break;

              {Update Attribute}
              AAttribute.StreamUsed:=(ARecord.CalculatedStreamUsed(FVolumeVersion,AAttribute) shl FClusterShiftCount);
             end
            else
             begin
              {Uncompressable Unit} {Decompress Run and Write Data}

              {Decompress Run}
              if not DecompressRun(ARecord,AAttribute,VCN,Count) then Break;

              {Write Run}
              if WriteRun(ARecord,AAttribute,Pointer(PtrUInt(@ABuffer) + Offset)^,VCN,Count,AInstance,True,False) <> Count then Break;
             end;
           end
          else
           begin
            {Uncompressed Unit} {Write Data and Compress Run}
            {Write Run}
            if WriteRun(ARecord,AAttribute,Pointer(PtrUInt(@ABuffer) + Offset)^,VCN,Count,AInstance,True,False) <> Count then Break;

            {Compress Run}
            if not CompressRun(ARecord,AAttribute,VCN,Count) then Break;
           end;

          {Update Position}
          Inc(VCN,Count);
          Inc(Offset,(Count shl FClusterShiftCount));
          Dec(Remain,Count);

          {Set Records}
          if AUpdate then if not SetRecords(ARecord) then Break;
         end;

        Result:=(ACount - Remain);
       finally
        {Release Buffer}
        ReleaseDecompressionBuffer(AAttribute.CompressionUnit,DecompressBuffer);
       end;
      finally
       {Release Buffer}
       ReleaseCompressionBuffer(AAttribute.CompressionUnit,CompressBuffer);
      end;
     end
    else
     begin
      {Uncompressed Run (or Raw Write)}
      {Get Position}
      VCN:=AVCN;
      Offset:=0;
      Remain:=ACount;

      {Write Clusters}
      while Remain > 0 do
       begin
        {Get Attribute}
        Attribute:=ARecord.GetAttributeByVCN(AAttribute,VCN,AInstance);
        if Attribute = nil then Break;

        {Get Cluster}
        if not GetRunCluster(Attribute,VCN,Cluster,Length) then Break;

        {Get Count}
        Count:=Remain;
        if Count > Length then Count:=Length; {Min does not support Int64}

        {$IFDEF NTFS_DEBUG}
        if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.WriteRun - VCN = ' + IntToStr(VCN) + ' Instance = ' + IntToStr(AInstance) + ' Cluster = ' + IntToStr(Cluster) + ' Length = ' + IntToStr(Length) + ' Count = ' + IntToStr(Count));
        {$ENDIF}

        {Check Cluster}
        if Cluster = ntfsUnknownCluster then
         begin
          {Sparse Run}
          {Convert Run} {To Normal}
          if not ConvertRun(ARecord,AAttribute,VCN,Count,False) then Break;

          {Set Records}
          if AUpdate then if not SetRecords(ARecord) then Break;

          {Do not update position}
         end
        else
         begin
          {Normal Run}
          {Write Clusters}
          if not WriteClusters(Cluster,Count,Pointer(PtrUInt(@ABuffer) + Offset)^) then Break;

          {Update Position}
          Inc(VCN,Count);
          Inc(Offset,(Count shl FClusterShiftCount));
          Dec(Remain,Count);
         end;
       end;

      Result:=(ACount - Remain);
     end;
   end;
 finally
  FRecords.RunsWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.ReadFileRecord(const ARecordNumber:Int64;var ABuffer;const AVCN:Int64;ACount:LongWord):Boolean;
{Read the specified file record from the MFT data run}
{Allows for startup case where MFT is not yet loaded}
var
 Instance:LongWord;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.ReadFileRecord - RecordNumber = ' + IntToHex(ARecordNumber,16) + ' VCN = ' + IntToStr(AVCN) + ' Count = ' + IntToStr(ACount));
 {$ENDIF}

 if FDriver = nil then Exit;

 {Check Master}
 if FMaster = nil then
  begin
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.ReadFileRecord - reading from clusters');
   {$ENDIF}

   {Startup}
   if ARecordNumber > ntfsFileTypeExpansion8 then Exit; {Allows for extensions of Mft}

   {Read Clusters}
   Result:=ReadClusters(FMftStartCluster + AVCN,ACount,ABuffer);
  end
 else
  begin
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.ReadFileRecord - reading from MFT');
   {$ENDIF}

   {Normal}
   if not FRecords.WriterLock then Exit;
   try
    {Get Origin}
    Origin:=FMaster.Origin;
    if Origin = nil then Exit;

    {Get Attribute}
    Attribute:=Origin.GetAttribute(ntfsAttrTypeData,ntfsBlankName,ntfsInstanceFirst);
    if Attribute = nil then Exit;

    {Read Run}
    Instance:=ntfsInstanceFirst;
    if ReadRun(Origin,Attribute,ABuffer,AVCN,ACount,Instance,False,True) <> ACount then Exit;

    Result:=True;
   finally
    FRecords.WriterUnlock;
   end;
  end;
end;

{=============================================================================}

function TNTFSFileSystem.WriteFileRecord(const ARecordNumber:Int64;const ABuffer;const AVCN:Int64;ACount:LongWord;AMirror:Boolean):Boolean;
{Write the specified file record to the MFT/MFTMirr data run}
{Allows for startup case where MFT/MFTMirr is not yet loaded}
var
 Instance:LongWord;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if FDriver = nil then Exit;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.WriteFileRecord - RecordNumber = ' + IntToHex(ARecordNumber,16) + ' VCN = ' + IntToStr(AVCN) + ' Count = ' + IntToStr(ACount) + ' Mirror = ' + BoolToStr(AMirror));
 {$ENDIF}

 {Check ReadOnly}
 if FReadOnly then Exit;

 {Check Mirror}
 if AMirror then
  begin
   {Check Mirror}
   if FMirror = nil then
    begin
     {Startup}
     if ARecordNumber >= FMftMirrorCount then Exit; {Allows for variable mirror count}

     {Write Clusters}
     Result:=WriteClusters(FMftMirrorCluster + AVCN,ACount,ABuffer);
    end
   else
    begin
     {Normal}
     if not FRecords.WriterLock then Exit;
     try
      {Get Origin}
      Origin:=FMirror.Origin;
      if Origin = nil then Exit;

      {Get Attribute}
      Attribute:=Origin.GetAttribute(ntfsAttrTypeData,ntfsBlankName,ntfsInstanceFirst);
      if Attribute = nil then Exit;

      {Write Run}
      Instance:=ntfsInstanceFirst;
      if WriteRun(Origin,Attribute,ABuffer,AVCN,ACount,Instance,False,False) <> ACount then Exit;

      Result:=True;
     finally
      FRecords.WriterUnlock;
     end;
    end;
  end
 else
  begin
   {Check Master}
   if FMaster = nil then
    begin
     {Startup}
     if ARecordNumber > ntfsFileTypeExpansion8 then Exit; {Allows for extensions of Mft}

     {Write Clusters}
     Result:=WriteClusters(FMftStartCluster + AVCN,ACount,ABuffer);
    end
   else
    begin
     {Normal}
     if not FRecords.WriterLock then Exit;
     try
      {Get Origin}
      Origin:=FMaster.Origin;
      if Origin = nil then Exit;

      {Get Attribute}
      Attribute:=Origin.GetAttribute(ntfsAttrTypeData,ntfsBlankName,ntfsInstanceFirst);
      if Attribute = nil then Exit;

      {Write Run}
      Instance:=ntfsInstanceFirst;
      if WriteRun(Origin,Attribute,ABuffer,AVCN,ACount,Instance,False,False) <> ACount then Exit;

      Result:=True;
     finally
      FRecords.WriterUnlock;
     end;
    end;
  end;
end;

{=============================================================================}

function TNTFSFileSystem.CreateTables:Boolean;
begin
 {}
 Result:=False;

 if not FTables.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FNTFSType = ntNONE then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CreateTables');
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Create each Table}
  if not CreateTable(ntfsTableTypeMft) then Exit;
  if not CreateTable(ntfsTableTypeMftMirr) then Exit;

  Result:=True;
 finally
  FTables.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.CreateBlocks:Boolean;
var
 BlockNo:LongWord;
begin
 {}
 Result:=False;

 if not FBlocks.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FNTFSType = ntNONE then Exit;
  if FTotalBlockCount = 0 then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CreateBlocks');
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Create each Block}
  BlockNo:=0;
  while BlockNo < FTotalBlockCount do
   begin
    if not CreateBlock(BlockNo) then Exit;

    Inc(BlockNo);
   end;

  Result:=True;
 finally
  FBlocks.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.CreateMetafiles:Boolean;
var
 Size:LongWord;
 Offset:LongWord;
 Buffer:Pointer;

 Count:LongWord;
 Instance:LongWord;

 Security:TNTFSSecurity;
 Current:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if not FRecords.WriterLock then Exit;
  try
   if FDriver = nil then Exit;
   if FUpCases = nil then Exit;
   if FNTFSType = ntNONE then Exit;

   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CreateMetafiles');
   {$ENDIF}

   {Check ReadOnly}
   if FReadOnly then Exit;

   {Initialize UpCases} {Record by Indexes}
   if not FUpCases.Init(FVolumeVersion) then Exit;

   {Create each Metafile}
   if not CreateMetafile(ntfsFileTypeMft) then Exit;
   if not CreateMetafile(ntfsFileTypeMftMirr) then Exit;
   if not CreateMetafile(ntfsFileTypeLogFile) then Exit;
   if not CreateMetafile(ntfsFileTypeVolume) then Exit;
   if not CreateMetafile(ntfsFileTypeAttrDef) then Exit;
   if not CreateMetafile(ntfsFileTypeRoot) then Exit;
   if not CreateMetafile(ntfsFileTypeBitmap) then Exit;
   if not CreateMetafile(ntfsFileTypeBoot) then Exit;
   if not CreateMetafile(ntfsFileTypeBadClus) then Exit;
   if not CreateMetafile(ntfsFileTypeSecure) then Exit;                                 {Also ntfs12FileTypeQuota}
   if not CreateMetafile(ntfsFileTypeUpCase) then Exit;
   if (not CreateMetafile(ntfsFileTypeExtend)) and (FNTFSType <> ntNTFS12) then Exit;   {Not available in NTFS 1.2}
   {Metafiles within $Extend}
   if (not CreateMetafile(ntfsFileTypeObjId)) and (FNTFSType <> ntNTFS12) then Exit;    {Not available in NTFS 1.2}
   if (not CreateMetafile(ntfsFileTypeQuota)) and (FNTFSType <> ntNTFS12) then Exit;    {Not available in NTFS 1.2}
   if (not CreateMetafile(ntfsFileTypeReparse)) and (FNTFSType <> ntNTFS12) then Exit;  {Not available in NTFS 1.2}
   {CreateMetafile(ntfsFileTypeUsnJrnl);}                                               {UsnJrnl is an optional file}

   {Reserved Records}
   if not CreateReserved then Exit;
   if not CreateExpansion then Exit;

   {Save each Metafile}
   {MFT}
   Current:=GetRecordEx(nil,ntfsFileTypeMft,False,True);
   if Current = nil then Exit;
   if not SetRecord(Current) then Exit;

   {MFT:Bitmap}
   Attribute:=Current.GetAttribute(ntfsAttrTypeBitmap,ntfsBlankName,ntfsInstanceFirst);
   if Attribute = nil then Exit;
   Instance:=ntfsInstanceFirst;
   if WriteAttribute(Current,Attribute,TNTFSBitmapAttribute(Attribute).Bitmap^,0,TNTFSBitmapAttribute(Attribute).BitmapSize,Instance,False) <> Integer(TNTFSBitmapAttribute(Attribute).BitmapSize) then Exit;

   {MFTMirr}
   Current:=GetRecordEx(nil,ntfsFileTypeMftMirr,False,True);
   if Current = nil then Exit;
   if not SetRecord(Current) then Exit;

   {LogFile}
   Current:=GetRecordEx(nil,ntfsFileTypeLogFile,False,True);
   if Current = nil then Exit;
   if not SetRecord(Current) then Exit;

   {Volume}
   Current:=GetRecordEx(nil,ntfsFileTypeVolume,False,True);
   if Current = nil then Exit;
   if not SetRecord(Current) then Exit;

   {AttrDef}
   Current:=GetRecordEx(nil,ntfsFileTypeAttrDef,False,True);
   if Current = nil then Exit;
   if not SetRecord(Current) then Exit;

   {Root}
   Current:=GetRecordEx(nil,ntfsFileTypeRoot,False,True);
   if Current = nil then Exit;
   if not SetRecord(Current) then Exit;

   {Root:SecurityDescriptor}
   Attribute:=Current.GetAttribute(ntfsAttrTypeSecurityDescriptor,ntfsBlankName,ntfsInstanceFirst);
   if Attribute = nil then Exit;
   Instance:=ntfsInstanceFirst;
   Buffer:=AllocMem(Attribute.StreamSize);
   try
    {Get Security}
    Security:=TNTFSSecurityDescriptorAttribute(Attribute).Security;
    if Security = nil then Exit;

    {Write Buffer}
    Offset:=0;
    Size:=Attribute.StreamSize;
    if not Security.WriteSecurity(Buffer,Offset,Size,FVolumeVersion) then Exit;

    {Write Attribute}
    if WriteAttribute(Current,Attribute,Buffer^,0,Attribute.StreamSize,Instance,False) <> Attribute.StreamSize then Exit;
   finally
    FreeMem(Buffer);
   end;

   {Bitmap}
   Current:=GetRecordEx(nil,ntfsFileTypeBitmap,False,True);
   if Current = nil then Exit;
   if not SetRecord(Current) then Exit;

   {Boot}
   Current:=GetRecordEx(nil,ntfsFileTypeBoot,False,True);
   if Current = nil then Exit;
   if not SetRecord(Current) then Exit;

   {BadClus}
   Current:=GetRecordEx(nil,ntfsFileTypeBadClus,False,True);
   if Current = nil then Exit;
   if not SetRecord(Current) then Exit;

   {Secure}
   Current:=GetRecordEx(nil,ntfsFileTypeSecure,False,True); {Also ntfs12FileTypeQuota}
   if Current = nil then Exit;
   if not SetRecord(Current) then Exit;

   {UpCase}
   Current:=GetRecordEx(nil,ntfsFileTypeUpCase,False,True);
   if Current = nil then Exit;
   if not SetRecord(Current) then Exit;

   {Check Version}
   if FNTFSType <> ntNTFS12 then
    begin
     {Extend}
     Current:=GetRecordEx(nil,ntfsFileTypeExtend,False,True);
     if Current = nil then Exit;
     if not SetRecord(Current) then Exit;

     {ObjId}
     Current:=GetRecordEx(nil,ntfsObjIdRecordNumber,False,True);
     if Current = nil then Exit;
     if not SetRecord(Current) then Exit;

     {Quota}
     Current:=GetRecordEx(nil,ntfsQuotaRecordNumber,False,True);
     if Current = nil then Exit;
     if not SetRecord(Current) then Exit;

     {Reparse}
     Current:=GetRecordEx(nil,ntfsReparseRecordNumber,False,True);
     if Current = nil then Exit;
     if not SetRecord(Current) then Exit;
    end;

   {Reserved}
   for Count:=ntfsFileTypeReserved1 to ntfsFileTypeReserved4 do
    begin
     Current:=GetRecordEx(nil,Count,False,True);
     if Current = nil then Exit;
     if not SetRecord(Current) then Exit;
    end;

   {Expansion}
   for Count:=ntfsFileTypeExpansion1 to ntfsFileTypeExpansion8 do
    begin
     Current:=GetRecordEx(nil,Count,False,True);
     if Current = nil then Exit;
     if not SetRecord(Current) then Exit;
    end;

   {Create Boots}
   if not CreateBoots then Exit;

   {Create UpCases}
   if not CreateUpCases then Exit;

   {Create AttrDefs}
   if not CreateAttrDefs then Exit;

   {Create Bitmaps}
   if not CreateBitmaps then Exit;

   {Create FileNames}
   if not CreateFileNames then Exit;

   {Check Version}
   if FNTFSType <> ntNTFS12 then
    begin
     {Create ObjIds}
     if not CreateObjIds then Exit;

     {Create Owners}
     if not CreateOwners then Exit;

     {Create Quotas}
     if not CreateQuotas then Exit;

     {Create Reparses}
     if not CreateReparses then Exit;
    end;

   {Create LogFiles}
   if not CreateLogFiles then Exit;

   {Create Securitys}
   if not CreateSecuritys then Exit;

   {Load Metafiles}
   if not LoadMetafiles then Exit;

   {Create Tables}
   if not CreateTables then Exit;

   Result:=True;
  finally
   FRecords.WriterUnlock;
  end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.CreateTable(ATableNo:LongWord):Boolean;
var
 Table:TNTFSDiskTable;
begin
 {}
 Result:=False;

 if not FTables.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FNTFSType = ntNONE then Exit;
  if ATableNo > ntfsTableTypeMftMirr then Exit;

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Load Table}
  Table:=TNTFSDiskTable.Create(FTableLocal);
  Table.TableNo:=ATableNo;
  Table.StartCluster:=FMftStartCluster;
  Table.StartSector:=(FMftStartCluster shl FSectorShiftCount);
  Table.Entry:=FMft;
  if ATableNo = ntfsTableTypeMftMirr then
   begin
    Table.StartCluster:=FMftMirrorCluster;
    Table.StartSector:=(FMftMirrorCluster shl FSectorShiftCount);
    Table.Entry:=FMftMirr;
   end;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CreateTable - Table = ' + IntToHex(Table.TableNo,8) + ' Sector = ' + IntToStr(Table.StartSector) + ' Cluster = ' + IntToStr(Table.StartCluster));
  {$ENDIF}

  Result:=FTables.Add(Table);
 finally
  FTables.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.CreateBlock(ABlockNo:LongWord):Boolean;
var
 VCN:Int64;
 Count:Int64;
 Cluster:Int64;
 Buffer:Pointer;
 Remain:LongWord;
 Instance:LongWord;

 Block:TNTFSDiskBlock;
 Origin:TNTFSDiskRecord;
 Current:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FBlocks.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FNTFSType = ntNONE then Exit;
  if ABlockNo >= FTotalBlockCount then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CreateBlock BlockNo = ' + IntToHex(ABlockNo,8));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get Bitmap}
  Current:=GetRecordEx(nil,ntfsFileTypeBitmap,False,True);
  if Current = nil then Exit;

  {Get Origin}
  Origin:=Current.Origin;
  if Origin = nil then Exit;

  {Get Attribute}
  Attribute:=Origin.GetAttribute(ntfsAttrTypeData,ntfsBlankName,ntfsInstanceFirst);
  if Attribute = nil then Exit;

  {Get Cluster}
  VCN:=ABlockNo;
  Cluster:=(VCN shl FBlockShiftCount);
  Count:=(FTotalClusterCount - Cluster);
  if Count > FEntriesPerBlock then Count:=FEntriesPerBlock; {Min does not support Int64}

  {Allocate Buffer}
  Block:=nil;
  Buffer:=AllocMem(FClustersPerBlock shl FClusterShiftCount);
  if Buffer = nil then Exit;
  try
   {Create Block}
   Block:=TNTFSDiskBlock.Create(FBlockLocal);
   try
    Block.BlockNo:=ABlockNo;
    Block.BlockCount:=Count;
    Block.BlockBuffer:=Buffer;
    Block.BlockCluster:=Cluster;

    {Check Count}
    if Count < FEntriesPerBlock then
     begin
      {Alloc Bitmap} {Unavailable entries in last block}
      Remain:=FEntriesPerBlock - Count;
      if not AllocBitmap(Buffer,FEntriesPerBlock,Count,Remain) then Exit;
     end;

    {Write Run}
    Instance:=ntfsInstanceFirst;
    if WriteRun(Origin,Attribute,Buffer^,VCN,FClustersPerBlock,Instance,False,False) = FClustersPerBlock then
     begin
      {$IFDEF NTFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CreateBlock - BlockNo = ' + IntToHex(Block.BlockNo,8) + ' BlockCluster = ' + IntToStr(Block.BlockCluster) + ' BlockCount = ' + IntToStr(Block.BlockCount));
      {$ENDIF}

      Result:=FBlocks.Add(Block);
     end;
   finally
    if not Result then Block.Free;
   end;
  finally
   if not(Result) and (Block = nil) then FreeMem(Buffer);  {Dont double free the buffer} {Note: This proabably cannot happen}
  end;
 finally
  FBlocks.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.CreateMetafile(AFileNo:LongWord):Boolean;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if not FRecords.WriterLock then Exit;
  try
   if FDriver = nil then Exit;
   if FNTFSType = ntNONE then Exit;

   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CreateMetafile FileNo = ' + IntToHex(AFileNo,8));
   {$ENDIF}

   {Check ReadOnly}
   if FReadOnly then Exit;

   {Create Metafile}
   case AFileNo of
    ntfsFileTypeMft:Result:=CreateMft;
    ntfsFileTypeMftMirr:Result:=CreateMftMirr;
    ntfsFileTypeLogFile:Result:=CreateLogFile;
    ntfsFileTypeVolume:Result:=CreateVolume;
    ntfsFileTypeAttrDef:Result:=CreateAttrDef;
    ntfsFileTypeRoot:Result:=CreateRoot;
    ntfsFileTypeBitmap:Result:=CreateBitmap;
    ntfsFileTypeBoot:Result:=CreateBoot;
    ntfsFileTypeBadClus:Result:=CreateBadClus;
    ntfsFileTypeSecure:if FNTFSType = ntNTFS12 then Result:=CreateQuota else Result:=CreateSecure; {Also ntfs12FileTypeQuota}
    ntfsFileTypeUpCase:Result:=CreateUpCase;
    ntfsFileTypeExtend:if FNTFSType = ntNTFS12 then Result:=False else Result:=CreateExtend;
    {Metafiles within $Extend}
    ntfsFileTypeObjId:if FNTFSType = ntNTFS12 then Result:=False else Result:=CreateObjId;
    ntfsFileTypeQuota:if FNTFSType = ntNTFS12 then Result:=False else Result:=CreateQuota;
    ntfsFileTypeReparse:if FNTFSType = ntNTFS12 then Result:=False else Result:=CreateReparse;
    ntfsFileTypeUsnJrnl:if FNTFSType = ntNTFS12 then Result:=False else Result:=CreateUsnJrnl;
   end;
  finally
   FRecords.WriterUnlock;
  end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.CreateMft:Boolean;
{Create the $MFT metafile}
var
 Count:LongWord;

 Run:TNTFSDiskRun;
 Current:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if not FRecords.WriterLock then Exit;
  try
   if FDriver = nil then Exit;
   if FNTFSType = ntNONE then Exit;

   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CreateMft');
   {$ENDIF}

   {Check ReadOnly}
   if FReadOnly then Exit;

   {Check Record}
   if FMaster <> nil then Exit;

   {Create Record}
   Current:=FRecords.NewRecord(nil,ntfsFileTypeMft,FVolumeVersion);
   if Current = nil then Exit;

   {Update Record}
   Current.Metafile:=True;
   if Current.RecordNumber < FMftMirrorCount then Current.Mirrored:=True;
   Current.RecordFlags:=ntfsFileRecordFlagInUse;
   Current.HardLinkCount:=1;
   Current.SequenceNumber:=ntfsFileTypeMft + 1; {Plus one for MFT}
   Current.RecordAllocated:=FFileRecordSize;
   Current.UpdateSequenceOffset:=Current.CalculatedSequenceOffset(FVolumeVersion);
   Current.UpdateSequenceLength:=Current.CalculatedSequenceLength(ntfsUpdateSequenceSize); {Previously FSectorSize however always 512}
   Current.AttributeOffset:=Current.CalculatedOffset(FVolumeVersion);

   {Add StandardInformation}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeStandardInformation,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   TNTFSStandardInformationAttribute(Attribute).Attributes:=faHidden or faSysFile;
   TNTFSStandardInformationAttribute(Attribute).SecurityId:=ntfsDefaultSecurityId100;
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add FileName}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeFileName,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   TNTFSFileNameAttribute(Attribute).ParentReference:=ntfsRootFileReference;
   TNTFSFileNameAttribute(Attribute).NameSpace:=ntfsNameSpaceBoth;
   TNTFSFileNameAttribute(Attribute).FileName:=ntfsFileNameMft;
   Attribute.Indexed:=ntfsAttributeIndexed;
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add Data}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeData,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   Attribute.NonResident:=ntfsAttributeNonResident;
   Attribute.DataSize:=0;
   Attribute.StartVCN:=0;
   Attribute.LastVCN:=ntfsUnknownCluster;
   Attribute.StreamSize:=CalculateMftSize;
   Attribute.InitializedSize:=Attribute.StreamSize;
   Attribute.StreamAllocated:=NTFSRoundQuadWordToClusterSize(Attribute.StreamSize,FClusterShiftCount,FClusterSize);
   Run:=Attribute.NewRun(FMftStartCluster,(Attribute.StreamAllocated shr FClusterShiftCount));
   if Run = nil then Exit;
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add Bitmap}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeBitmap,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   TNTFSBitmapAttribute(Attribute).BitmapSize:=8;
   Attribute.NonResident:=ntfsAttributeNonResident;
   Attribute.DataSize:=0;
   Attribute.StartVCN:=0;
   Attribute.LastVCN:=ntfsUnknownCluster;
   Attribute.StreamSize:=8;
   Attribute.InitializedSize:=Attribute.StreamSize;
   Attribute.StreamAllocated:=NTFSRoundQuadWordToClusterSize(Attribute.StreamSize,FClusterShiftCount,FClusterSize);
   Run:=Attribute.NewRun(FMftStartCluster - (Attribute.StreamAllocated shr FClusterShiftCount),(Attribute.StreamAllocated shr FClusterShiftCount));
   if Run = nil then Exit;
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Update Record}
   Current.RecordSize:=Current.CalculatedSize(FVolumeVersion);

   {Insert Record}
   if not FRecords.InsertRecord(Current) then Exit;

   {Set Master}
   FMaster:=Current;

   {Alloc Bitmap}
   Count:=1;
   if not AllocBitmap(TNTFSBitmapAttribute(Attribute).Bitmap,ntfsDefaultRecordCount,ntfsFileTypeMft,Count) then Exit;

   {Dont save until others are loaded}
   Result:=True;
  finally
   FRecords.WriterUnlock;
  end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.CreateMftMirr:Boolean;
{Create the $MFTMirr metafile}
var
 Count:LongWord;

 Run:TNTFSDiskRun;
 Current:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if not FRecords.WriterLock then Exit;
  try
   if FDriver = nil then Exit;
   if FMaster = nil then Exit;
   if FNTFSType = ntNONE then Exit;

   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CreateMftMirr');
   {$ENDIF}

   {Check ReadOnly}
   if FReadOnly then Exit;

   {Check Record}
   if FMirror <> nil then Exit;

   {Create Record}
   Current:=FRecords.NewRecord(nil,ntfsFileTypeMftMirr,FVolumeVersion);
   if Current = nil then Exit;

   {Update Record}
   Current.Metafile:=True;
   if Current.RecordNumber < FMftMirrorCount then Current.Mirrored:=True;
   Current.RecordFlags:=ntfsFileRecordFlagInUse;
   Current.HardLinkCount:=1;
   Current.SequenceNumber:=ntfsFileTypeMftMirr; {Same as record no for all but MFT}
   Current.RecordAllocated:=FFileRecordSize;
   Current.UpdateSequenceOffset:=Current.CalculatedSequenceOffset(FVolumeVersion);
   Current.UpdateSequenceLength:=Current.CalculatedSequenceLength(ntfsUpdateSequenceSize); {Previously FSectorSize however always 512}
   Current.AttributeOffset:=Current.CalculatedOffset(FVolumeVersion);

   {Add StandardInformation}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeStandardInformation,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   TNTFSStandardInformationAttribute(Attribute).Attributes:=faHidden or faSysFile;
   TNTFSStandardInformationAttribute(Attribute).SecurityId:=ntfsDefaultSecurityId100;
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add FileName}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeFileName,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   TNTFSFileNameAttribute(Attribute).ParentReference:=ntfsRootFileReference;
   TNTFSFileNameAttribute(Attribute).NameSpace:=ntfsNameSpaceBoth;
   TNTFSFileNameAttribute(Attribute).FileName:=ntfsFileNameMftMirr;
   Attribute.Indexed:=ntfsAttributeIndexed;
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add Data}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeData,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   Attribute.NonResident:=ntfsAttributeNonResident;
   Attribute.DataSize:=0;
   Attribute.StartVCN:=0;
   Attribute.LastVCN:=ntfsUnknownCluster;
   Attribute.StreamSize:=CalculateMftMirrSize;
   Attribute.InitializedSize:=Attribute.StreamSize;
   Attribute.StreamAllocated:=NTFSRoundQuadWordToClusterSize(Attribute.StreamSize,FClusterShiftCount,FClusterSize);
   Run:=Attribute.NewRun(FMftMirrorCluster,(Attribute.StreamAllocated shr FClusterShiftCount));
   if Run = nil then Exit;
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Update Record}
   Current.RecordSize:=Current.CalculatedSize(FVolumeVersion);

   {Insert Record}
   if not FRecords.InsertRecord(Current) then Exit;

   {Set Mirror}
   FMirror:=Current;

   {Get Bitmap}
   Attribute:=FMaster.GetAttribute(ntfsAttrTypeBitmap,ntfsBlankName,ntfsInstanceFirst);
   if Attribute = nil then Exit;

   {Alloc Bitmap}
   Count:=1;
   if not AllocBitmap(TNTFSBitmapAttribute(Attribute).Bitmap,ntfsDefaultRecordCount,ntfsFileTypeMftMirr,Count) then Exit;

   {Dont save until others are loaded}
   Result:=True;
  finally
   FRecords.WriterUnlock;
  end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.CreateLogFile:Boolean;
{Create the $LogFile metafile}
var
 Count:LongWord;
 Cluster:Int64;

 Run:TNTFSDiskRun;
 Current:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if not FRecords.WriterLock then Exit;
  try
   if FDriver = nil then Exit;
   if FMaster = nil then Exit;
   if FNTFSType = ntNONE then Exit;

   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CreateLogFile');
   {$ENDIF}

   {Check ReadOnly}
   if FReadOnly then Exit;

   {Create Record}
   Current:=FRecords.NewRecord(nil,ntfsFileTypeLogFile,FVolumeVersion);
   if Current = nil then Exit;

   {Update Record}
   Current.Metafile:=True;
   if Current.RecordNumber < FMftMirrorCount then Current.Mirrored:=True;
   Current.RecordFlags:=ntfsFileRecordFlagInUse;
   Current.HardLinkCount:=1;
   Current.SequenceNumber:=ntfsFileTypeLogFile; {Same as record no for all but MFT}
   Current.RecordAllocated:=FFileRecordSize;
   Current.UpdateSequenceOffset:=Current.CalculatedSequenceOffset(FVolumeVersion);
   Current.UpdateSequenceLength:=Current.CalculatedSequenceLength(ntfsUpdateSequenceSize); {Previously FSectorSize however always 512}
   Current.AttributeOffset:=Current.CalculatedOffset(FVolumeVersion);

   {Add StandardInformation}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeStandardInformation,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   TNTFSStandardInformationAttribute(Attribute).Attributes:=faHidden or faSysFile;
   TNTFSStandardInformationAttribute(Attribute).SecurityId:=ntfsDefaultSecurityId100;
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add FileName}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeFileName,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   TNTFSFileNameAttribute(Attribute).ParentReference:=ntfsRootFileReference;
   TNTFSFileNameAttribute(Attribute).NameSpace:=ntfsNameSpaceBoth;
   TNTFSFileNameAttribute(Attribute).FileName:=ntfsFileNameLogFile;
   Attribute.Indexed:=ntfsAttributeIndexed;
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add Data}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeData,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   Attribute.NonResident:=ntfsAttributeNonResident;
   Attribute.DataSize:=0;
   Attribute.StartVCN:=0;
   Attribute.LastVCN:=ntfsUnknownCluster;
   Attribute.StreamSize:=CalculateLogFileSize;
   Attribute.InitializedSize:=Attribute.StreamSize;
   Attribute.StreamAllocated:=NTFSRoundQuadWordToClusterSize(Attribute.StreamSize,FClusterShiftCount,FClusterSize);
   Cluster:=CalculateLogFileStartCluster;
   if Cluster = ntfsUnknownCluster then Exit;
   Run:=Attribute.NewRun(Cluster,(Attribute.StreamAllocated shr FClusterShiftCount));
   if Run = nil then Exit;
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Update Record}
   Current.RecordSize:=Current.CalculatedSize(FVolumeVersion);

   {Insert Record}
   if not FRecords.InsertRecord(Current) then Exit;

   {Get Bitmap}
   Attribute:=FMaster.GetAttribute(ntfsAttrTypeBitmap,ntfsBlankName,ntfsInstanceFirst);
   if Attribute = nil then Exit;

   {Alloc Bitmap}
   Count:=1;
   if not AllocBitmap(TNTFSBitmapAttribute(Attribute).Bitmap,ntfsDefaultRecordCount,ntfsFileTypeLogFile,Count) then Exit;

   {Dont save until others are loaded}
   Result:=True;
  finally
   FRecords.WriterUnlock;
  end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.CreateVolume:Boolean;
{Create the $Volume metafile}
var
 Count:LongWord;
 Descriptor:Pointer;

 Security:TNTFSSecurity;
 Current:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if not FRecords.WriterLock then Exit;
  try
   if FDriver = nil then Exit;
   if FMaster = nil then Exit;
   if FNTFSType = ntNONE then Exit;

   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CreateVolume');
   {$ENDIF}

   {Check ReadOnly}
   if FReadOnly then Exit;

   {Create Record}
   Current:=FRecords.NewRecord(nil,ntfsFileTypeVolume,FVolumeVersion);
   if Current = nil then Exit;

   {Update Record}
   Current.Metafile:=True;
   if Current.RecordNumber < FMftMirrorCount then Current.Mirrored:=True;
   Current.RecordFlags:=ntfsFileRecordFlagInUse;
   Current.HardLinkCount:=1;
   Current.SequenceNumber:=ntfsFileTypeVolume; {Same as record no for all but MFT}
   Current.RecordAllocated:=FFileRecordSize;
   Current.UpdateSequenceOffset:=Current.CalculatedSequenceOffset(FVolumeVersion);
   Current.UpdateSequenceLength:=Current.CalculatedSequenceLength(ntfsUpdateSequenceSize); {Previously FSectorSize however always 512}
   Current.AttributeOffset:=Current.CalculatedOffset(FVolumeVersion);

   {Add StandardInformation}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeStandardInformation,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   TNTFSStandardInformationAttribute(Attribute).Attributes:=faHidden or faSysFile;
   Attribute.DataSize:=Attribute.CalculatedStreamSize(ntfsNTFS12); {Always version 1.2}
   Attribute.AttributeSize:=Attribute.CalculatedSize(ntfsNTFS12);  {Always version 1.2}

   {Add FileName}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeFileName,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   TNTFSFileNameAttribute(Attribute).ParentReference:=ntfsRootFileReference;
   TNTFSFileNameAttribute(Attribute).NameSpace:=ntfsNameSpaceBoth;
   TNTFSFileNameAttribute(Attribute).FileName:=ntfsFileNameVolume;
   Attribute.Indexed:=ntfsAttributeIndexed;
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add ObjectId}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeObjectId,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   TNTFSObjectIdAttribute(Attribute).ObjectId:=CreateGUID;
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add Security}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeSecurityDescriptor,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   Descriptor:=nil;
   if not NTFSCreateDefaultDescriptor(ntfsDefaultDescriptorVolume,Descriptor,FVolumeVersion) then Exit;
   try
    Security:=TNTFSSecurity.CreateFromDescriptor(FSecurityLocal,Descriptor);
    if Security = nil then Exit;
    if not TNTFSSecurityDescriptorAttribute(Attribute).NewSecurity(Security) then Exit;
   finally
    NTFSDestroyDefaultDescriptor(Descriptor,FVolumeVersion);
   end;
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add VolumeName}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeVolumeName,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add VolumeInformation}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeVolumeInformation,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   case FVolumeVersion of
    ntfsNTFS12:begin
      TNTFSVolumeInformationAttribute(Attribute).MinorVersion:=2;
      TNTFSVolumeInformationAttribute(Attribute).MajorVersion:=1;
     end;
    ntfsNTFS30:begin
      TNTFSVolumeInformationAttribute(Attribute).MinorVersion:=0;
      TNTFSVolumeInformationAttribute(Attribute).MajorVersion:=3;
     end;
    ntfsNTFS31:begin
      TNTFSVolumeInformationAttribute(Attribute).MinorVersion:=1;
      TNTFSVolumeInformationAttribute(Attribute).MajorVersion:=3;
     end;
   end;
   TNTFSVolumeInformationAttribute(Attribute).VolumeFlags:=ntfsVolumeFlagNone;
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add Data}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeData,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   Attribute.DataSize:=0;
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Update Record}
   Current.RecordSize:=Current.CalculatedSize(FVolumeVersion);

   {Insert Record}
   if not FRecords.InsertRecord(Current) then Exit;

   {Get Bitmap}
   Attribute:=FMaster.GetAttribute(ntfsAttrTypeBitmap,ntfsBlankName,ntfsInstanceFirst);
   if Attribute = nil then Exit;

   {Alloc Bitmap}
   Count:=1;
   if not AllocBitmap(TNTFSBitmapAttribute(Attribute).Bitmap,ntfsDefaultRecordCount,ntfsFileTypeVolume,Count) then Exit;

   {Dont save until others are loaded}
   Result:=True;
  finally
   FRecords.WriterUnlock;
  end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.CreateAttrDef:Boolean;
{Create the $AttrDef metafile}
var
 Count:LongWord;
 Cluster:Int64;
 Descriptor:Pointer;

 Run:TNTFSDiskRun;
 Security:TNTFSSecurity;
 Current:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if not FRecords.WriterLock then Exit;
  try
   if FDriver = nil then Exit;
   if FMaster = nil then Exit;
   if FNTFSType = ntNONE then Exit;

   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CreateAttrDef');
   {$ENDIF}

   {Check ReadOnly}
   if FReadOnly then Exit;

   {Create Record}
   Current:=FRecords.NewRecord(nil,ntfsFileTypeAttrDef,FVolumeVersion);
   if Current = nil then Exit;

   {Update Record}
   Current.Metafile:=True;
   if Current.RecordNumber < FMftMirrorCount then Current.Mirrored:=True;
   Current.RecordFlags:=ntfsFileRecordFlagInUse;
   Current.HardLinkCount:=1;
   Current.SequenceNumber:=ntfsFileTypeAttrDef; {Same as record no for all but MFT}
   Current.RecordAllocated:=FFileRecordSize;
   Current.UpdateSequenceOffset:=Current.CalculatedSequenceOffset(FVolumeVersion);
   Current.UpdateSequenceLength:=Current.CalculatedSequenceLength(ntfsUpdateSequenceSize); {Previously FSectorSize however always 512}
   Current.AttributeOffset:=Current.CalculatedOffset(FVolumeVersion);

   {Add StandardInformation}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeStandardInformation,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   TNTFSStandardInformationAttribute(Attribute).Attributes:=faHidden or faSysFile;
   Attribute.DataSize:=Attribute.CalculatedStreamSize(ntfsNTFS12); {Always version 1.2}
   Attribute.AttributeSize:=Attribute.CalculatedSize(ntfsNTFS12);  {Always version 1.2}

   {Add FileName}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeFileName,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   TNTFSFileNameAttribute(Attribute).ParentReference:=ntfsRootFileReference;
   TNTFSFileNameAttribute(Attribute).NameSpace:=ntfsNameSpaceBoth;
   TNTFSFileNameAttribute(Attribute).FileName:=ntfsFileNameAttrDef;
   Attribute.Indexed:=ntfsAttributeIndexed;
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add Security}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeSecurityDescriptor,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   Descriptor:=nil;
   if not NTFSCreateDefaultDescriptor(ntfsDefaultDescriptorAttrDef,Descriptor,FVolumeVersion) then Exit;
   try
    Security:=TNTFSSecurity.CreateFromDescriptor(FSecurityLocal,Descriptor);
    if Security = nil then Exit;
    if not TNTFSSecurityDescriptorAttribute(Attribute).NewSecurity(Security) then Exit;
   finally
    NTFSDestroyDefaultDescriptor(Descriptor,FVolumeVersion);
   end;
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add Data}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeData,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   Attribute.NonResident:=ntfsAttributeNonResident;
   Attribute.DataSize:=0;
   Attribute.StartVCN:=0;
   Attribute.LastVCN:=ntfsUnknownCluster;
   Attribute.StreamSize:=CalculateAttrDefSize;
   Attribute.InitializedSize:=Attribute.StreamSize;
   Attribute.StreamAllocated:=NTFSRoundQuadWordToClusterSize(Attribute.StreamSize,FClusterShiftCount,FClusterSize);
   Cluster:=CalculateAttrDefStartCluster;
   if Cluster = ntfsUnknownCluster then Exit;
   Run:=Attribute.NewRun(Cluster,(Attribute.StreamAllocated shr FClusterShiftCount));
   if Run = nil then Exit;
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Update Record}
   Current.RecordSize:=Current.CalculatedSize(FVolumeVersion);

   {Insert Record}
   if not FRecords.InsertRecord(Current) then Exit;

   {Get Bitmap}
   Attribute:=FMaster.GetAttribute(ntfsAttrTypeBitmap,ntfsBlankName,ntfsInstanceFirst);
   if Attribute = nil then Exit;

   {Alloc Bitmap}
   Count:=1;
   if not AllocBitmap(TNTFSBitmapAttribute(Attribute).Bitmap,ntfsDefaultRecordCount,ntfsFileTypeAttrDef,Count) then Exit;

   {Dont save until others are loaded}
   Result:=True;
  finally
   FRecords.WriterUnlock;
  end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.CreateRoot:Boolean;
{Create the root metafile}
var
 Count:LongWord;
 Cluster:Int64;
 Descriptor:Pointer;

 Run:TNTFSDiskRun;
 Index:TNTFSDiskIndex;
 Security:TNTFSSecurity;
 Current:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if not FRecords.WriterLock then Exit;
  try
   if FDriver = nil then Exit;
   if FMaster = nil then Exit;
   if FNTFSType = ntNONE then Exit;

   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CreateRoot');
   {$ENDIF}

   {Check ReadOnly}
   if FReadOnly then Exit;

   {Create Record}
   Current:=FRecords.NewRecord(nil,ntfsFileTypeRoot,FVolumeVersion);
   if Current = nil then Exit;

   {Update Record}
   Current.Metafile:=True;
   if Current.RecordNumber < FMftMirrorCount then Current.Mirrored:=True;
   Current.RecordFlags:=ntfsFileRecordFlagInUse or ntfsFileRecordFlagDirectory;
   Current.HardLinkCount:=1;
   Current.SequenceNumber:=ntfsFileTypeRoot; {Same as record no for all but MFT}
   Current.RecordAllocated:=FFileRecordSize;
   Current.UpdateSequenceOffset:=Current.CalculatedSequenceOffset(FVolumeVersion);
   Current.UpdateSequenceLength:=Current.CalculatedSequenceLength(ntfsUpdateSequenceSize); {Previously FSectorSize however always 512}
   Current.AttributeOffset:=Current.CalculatedOffset(FVolumeVersion);

   {Add StandardInformation}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeStandardInformation,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   TNTFSStandardInformationAttribute(Attribute).Attributes:=faHidden or faSysFile or faArchive;
   Attribute.DataSize:=Attribute.CalculatedStreamSize(ntfsNTFS12); {Always version 1.2}
   Attribute.AttributeSize:=Attribute.CalculatedSize(ntfsNTFS12);  {Always version 1.2}

   {Add FileName}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeFileName,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   TNTFSFileNameAttribute(Attribute).ParentReference:=ntfsRootFileReference;
   TNTFSFileNameAttribute(Attribute).NameSpace:=ntfsNameSpaceBoth;
   TNTFSFileNameAttribute(Attribute).FileName:=ntfsFileNameRoot;
   Attribute.Indexed:=ntfsAttributeIndexed;
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add Security}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeSecurityDescriptor,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   Descriptor:=nil;
   if not NTFSCreateDefaultDescriptor(ntfsDefaultDescriptorRoot,Descriptor,FVolumeVersion) then Exit;
   try
    Security:=TNTFSSecurity.CreateFromDescriptor(FSecurityLocal,Descriptor);
    if Security = nil then Exit;
    if not TNTFSSecurityDescriptorAttribute(Attribute).NewSecurity(Security) then Exit;
   finally
    NTFSDestroyDefaultDescriptor(Descriptor,FVolumeVersion);
   end;
   Attribute.NonResident:=ntfsAttributeNonResident;
   Attribute.DataSize:=0;
   Attribute.StartVCN:=0;
   Attribute.LastVCN:=ntfsUnknownCluster;
   Attribute.StreamSize:=Attribute.CalculatedStreamSize(FVolumeVersion); {Dont use CalculateRootSecurityDescriptorSize}
   Attribute.InitializedSize:=Attribute.StreamSize;
   Attribute.StreamAllocated:=NTFSRoundQuadWordToClusterSize(Attribute.StreamSize,FClusterShiftCount,FClusterSize);
   Cluster:=CalculateRootSecurityDescriptorStartCluster;
   if Cluster = ntfsUnknownCluster then Exit;
   Run:=Attribute.NewRun(Cluster,(Attribute.StreamAllocated shr FClusterShiftCount));
   if Run = nil then Exit;
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add Index ($I30)}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeIndexRoot,ntfsIndexNameFileName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   if not Attribute.NewIndex(FVolumeVersion,FSectorSize,ntfsAttrTypeFileName,ntfsCollateTypeFileName,FIndexRecordSize,FIndexCounterOffset) then Exit;
   Index:=Attribute.Index;
   if Index = nil then Exit;

   {Update Index}
   Index.ClustersPerIndex:=FClustersPerIndex;
   Index.IndexsPerCluster:=FIndexsPerCluster;
   Index.IndexCounterShift:=FIndexCounterShift;
   Index.IndexRecordShiftCount:=FIndexRecordShiftCount;
   Index.IndexRecordOffsetMask:=FIndexRecordOffsetMask;
   Index.Loaded:=True;

   {Update Index}
   Index.UpCase:=FUpCases;
   Index.CompareSecurityDescriptor:=CompareSecurityDescriptor;

   {Update Attribute}
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add Allocation}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeIndexAllocation,ntfsIndexNameFileName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   Attribute.NonResident:=ntfsAttributeNonResident;
   Attribute.DataSize:=0;
   Attribute.StartVCN:=0;
   Attribute.LastVCN:=ntfsUnknownCluster;
   Attribute.StreamSize:=CalculateRootAllocationSize;
   Attribute.InitializedSize:=Attribute.StreamSize;
   Attribute.StreamAllocated:=NTFSRoundQuadWordToClusterSize(Attribute.StreamSize,FClusterShiftCount,FClusterSize);
   Cluster:=CalculateRootAllocationStartCluster;
   if Cluster = ntfsUnknownCluster then Exit;
   Run:=Attribute.NewRun(Cluster,(Attribute.StreamAllocated shr FClusterShiftCount));
   if Run = nil then Exit;
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add Bitmap}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeBitmap,ntfsIndexNameFileName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   TNTFSBitmapAttribute(Attribute).BitmapSize:=8;
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Update Record}
   Current.RecordSize:=Current.CalculatedSize(FVolumeVersion);

   {Insert Record}
   if not FRecords.InsertRecord(Current) then Exit;

   {Get Bitmap}
   Attribute:=FMaster.GetAttribute(ntfsAttrTypeBitmap,ntfsBlankName,ntfsInstanceFirst);
   if Attribute = nil then Exit;

   {Alloc Bitmap}
   Count:=1;
   if not AllocBitmap(TNTFSBitmapAttribute(Attribute).Bitmap,ntfsDefaultRecordCount,ntfsFileTypeRoot,Count) then Exit;

   {Dont save until others are loaded}
   Result:=True;
  finally
   FRecords.WriterUnlock;
  end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.CreateBitmap:Boolean;
{Create the $Bitmap metafile}
var
 Count:LongWord;
 Cluster:Int64;

 Run:TNTFSDiskRun;
 Current:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if not FRecords.WriterLock then Exit;
  try
   if FDriver = nil then Exit;
   if FMaster = nil then Exit;
   if FNTFSType = ntNONE then Exit;

   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CreateBitmap');
   {$ENDIF}

   {Check ReadOnly}
   if FReadOnly then Exit;

   {Create Record}
   Current:=FRecords.NewRecord(nil,ntfsFileTypeBitmap,FVolumeVersion);
   if Current = nil then Exit;

   {Update Record}
   Current.Metafile:=True;
   if Current.RecordNumber < FMftMirrorCount then Current.Mirrored:=True;
   Current.RecordFlags:=ntfsFileRecordFlagInUse;
   Current.HardLinkCount:=1;
   Current.SequenceNumber:=ntfsFileTypeBitmap; {Same as record no for all but MFT}
   Current.RecordAllocated:=FFileRecordSize;
   Current.UpdateSequenceOffset:=Current.CalculatedSequenceOffset(FVolumeVersion);
   Current.UpdateSequenceLength:=Current.CalculatedSequenceLength(ntfsUpdateSequenceSize); {Previously FSectorSize however always 512}
   Current.AttributeOffset:=Current.CalculatedOffset(FVolumeVersion);

   {Add StandardInformation}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeStandardInformation,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   TNTFSStandardInformationAttribute(Attribute).Attributes:=faHidden or faSysFile;
   TNTFSStandardInformationAttribute(Attribute).SecurityId:=ntfsDefaultSecurityId100;
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add FileName}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeFileName,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   TNTFSFileNameAttribute(Attribute).ParentReference:=ntfsRootFileReference;
   TNTFSFileNameAttribute(Attribute).NameSpace:=ntfsNameSpaceBoth;
   TNTFSFileNameAttribute(Attribute).FileName:=ntfsFileNameBitmap;
   Attribute.Indexed:=ntfsAttributeIndexed;
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add Data}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeData,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   Attribute.NonResident:=ntfsAttributeNonResident;
   Attribute.DataSize:=0;
   Attribute.StartVCN:=0;
   Attribute.LastVCN:=ntfsUnknownCluster;
   Attribute.StreamSize:=CalculateBitmapSize;
   if (Attribute.StreamSize shl 3) < FTotalClusterCount then Attribute.StreamSize:=Attribute.StreamSize + 1;
   {Attribute.StreamSize:=Attribute.StreamSize + 1;} {1 extra cluster for Backup Boot} {Not Required}
   Attribute.InitializedSize:=Attribute.StreamSize;
   Attribute.StreamAllocated:=NTFSRoundQuadWordToClusterSize(Attribute.StreamSize,FClusterShiftCount,FClusterSize);
   Cluster:=CalculateBitmapStartCluster;
   if Cluster = ntfsUnknownCluster then Exit;
   Run:=Attribute.NewRun(Cluster,(Attribute.StreamAllocated shr FClusterShiftCount));
   if Run = nil then Exit;
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Update Record}
   Current.RecordSize:=Current.CalculatedSize(FVolumeVersion);

   {Insert Record}
   if not FRecords.InsertRecord(Current) then Exit;

   {Get Bitmap}
   Attribute:=FMaster.GetAttribute(ntfsAttrTypeBitmap,ntfsBlankName,ntfsInstanceFirst);
   if Attribute = nil then Exit;

   {Alloc Bitmap}
   Count:=1;
   if not AllocBitmap(TNTFSBitmapAttribute(Attribute).Bitmap,ntfsDefaultRecordCount,ntfsFileTypeBitmap,Count) then Exit;

   {Dont save until others are loaded}
   Result:=True;
  finally
   FRecords.WriterUnlock;
  end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.CreateBoot:Boolean;
{Create the $Boot metafile}
var
 Count:LongWord;
 Descriptor:Pointer;

 Run:TNTFSDiskRun;
 Security:TNTFSSecurity;
 Current:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if not FRecords.WriterLock then Exit;
  try
   if FDriver = nil then Exit;
   if FMaster = nil then Exit;
   if FNTFSType = ntNONE then Exit;

   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CreateBoot');
   {$ENDIF}

   {Check ReadOnly}
   if FReadOnly then Exit;

   {Create Record}
   Current:=FRecords.NewRecord(nil,ntfsFileTypeBoot,FVolumeVersion);
   if Current = nil then Exit;

   {Update Record}
   Current.Metafile:=True;
   if Current.RecordNumber < FMftMirrorCount then Current.Mirrored:=True;
   Current.RecordFlags:=ntfsFileRecordFlagInUse;
   Current.HardLinkCount:=1;
   Current.SequenceNumber:=ntfsFileTypeBoot; {Same as record no for all but MFT}
   Current.RecordAllocated:=FFileRecordSize;
   Current.UpdateSequenceOffset:=Current.CalculatedSequenceOffset(FVolumeVersion);
   Current.UpdateSequenceLength:=Current.CalculatedSequenceLength(ntfsUpdateSequenceSize); {Previously FSectorSize however always 512}
   Current.AttributeOffset:=Current.CalculatedOffset(FVolumeVersion);

   {Add StandardInformation}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeStandardInformation,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   TNTFSStandardInformationAttribute(Attribute).Attributes:=faHidden or faSysFile;
   Attribute.DataSize:=Attribute.CalculatedStreamSize(ntfsNTFS12); {Always version 1.2}
   Attribute.AttributeSize:=Attribute.CalculatedSize(ntfsNTFS12);  {Always version 1.2}

   {Add FileName}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeFileName,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   TNTFSFileNameAttribute(Attribute).ParentReference:=ntfsRootFileReference;
   TNTFSFileNameAttribute(Attribute).NameSpace:=ntfsNameSpaceBoth;
   TNTFSFileNameAttribute(Attribute).FileName:=ntfsFileNameBoot;
   Attribute.Indexed:=ntfsAttributeIndexed;
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add Security}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeSecurityDescriptor,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   Descriptor:=nil;
   if not NTFSCreateDefaultDescriptor(ntfsDefaultDescriptorBoot,Descriptor,FVolumeVersion) then Exit;
   try
    Security:=TNTFSSecurity.CreateFromDescriptor(FSecurityLocal,Descriptor);
    if Security = nil then Exit;
    if not TNTFSSecurityDescriptorAttribute(Attribute).NewSecurity(Security) then Exit;
   finally
    NTFSDestroyDefaultDescriptor(Descriptor,FVolumeVersion);
   end;
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add Data}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeData,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   Attribute.NonResident:=ntfsAttributeNonResident;
   Attribute.DataSize:=0;
   Attribute.StartVCN:=0;
   Attribute.LastVCN:=ntfsUnknownCluster;
   Attribute.StreamSize:=CalculateBootSize;
   Attribute.InitializedSize:=Attribute.StreamSize;
   Attribute.StreamAllocated:=NTFSRoundQuadWordToClusterSize(Attribute.StreamSize,FClusterShiftCount,FClusterSize);
   Run:=Attribute.NewRun(ntfsStartCluster,(Attribute.StreamAllocated shr FClusterShiftCount));
   if Run = nil then Exit;
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Update Record}
   Current.RecordSize:=Current.CalculatedSize(FVolumeVersion);

   {Insert Record}
   if not FRecords.InsertRecord(Current) then Exit;

   {Get Bitmap}
   Attribute:=FMaster.GetAttribute(ntfsAttrTypeBitmap,ntfsBlankName,ntfsInstanceFirst);
   if Attribute = nil then Exit;

   {Alloc Bitmap}
   Count:=1;
   if not AllocBitmap(TNTFSBitmapAttribute(Attribute).Bitmap,ntfsDefaultRecordCount,ntfsFileTypeBoot,Count) then Exit;

   {Dont save until others are loaded}
   Result:=True;
  finally
   FRecords.WriterUnlock;
  end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.CreateBadClus:Boolean;
{Create the $BadClus metafile}
var
 Count:LongWord;

 Run:TNTFSDiskRun;
 Current:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if not FRecords.WriterLock then Exit;
  try
   if FDriver = nil then Exit;
   if FMaster = nil then Exit;
   if FNTFSType = ntNONE then Exit;

   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CreateBadClus');
   {$ENDIF}

   {Check ReadOnly}
   if FReadOnly then Exit;

   {Create Record}
   Current:=FRecords.NewRecord(nil,ntfsFileTypeBadClus,FVolumeVersion);
   if Current = nil then Exit;

   {Update Record}
   Current.Metafile:=True;
   if Current.RecordNumber < FMftMirrorCount then Current.Mirrored:=True;
   Current.RecordFlags:=ntfsFileRecordFlagInUse;
   Current.HardLinkCount:=1;
   Current.SequenceNumber:=ntfsFileTypeBadClus; {Same as record no for all but MFT}
   Current.RecordAllocated:=FFileRecordSize;
   Current.UpdateSequenceOffset:=Current.CalculatedSequenceOffset(FVolumeVersion);
   Current.UpdateSequenceLength:=Current.CalculatedSequenceLength(ntfsUpdateSequenceSize); {Previously FSectorSize however always 512}
   Current.AttributeOffset:=Current.CalculatedOffset(FVolumeVersion);

   {Add StandardInformation}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeStandardInformation,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   TNTFSStandardInformationAttribute(Attribute).Attributes:=faHidden or faSysFile;
   TNTFSStandardInformationAttribute(Attribute).SecurityId:=ntfsDefaultSecurityId100;
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add FileName}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeFileName,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   TNTFSFileNameAttribute(Attribute).ParentReference:=ntfsRootFileReference;
   TNTFSFileNameAttribute(Attribute).NameSpace:=ntfsNameSpaceBoth;
   TNTFSFileNameAttribute(Attribute).FileName:=ntfsFileNameBadClus;
   Attribute.Indexed:=ntfsAttributeIndexed;
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add Data}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeData,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   Attribute.DataSize:=0;
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add Data ($Bad)}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeData,ntfsStreamNameBadClus,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   Attribute.NonResident:=ntfsAttributeNonResident;
   Attribute.DataSize:=0;
   Attribute.StartVCN:=0;
   Attribute.LastVCN:=ntfsUnknownCluster;
   Attribute.StreamSize:=CalculateBadClusBadSize;
   Attribute.InitializedSize:=0; {Sparse Run}
   Attribute.StreamAllocated:=NTFSRoundQuadWordToClusterSize(Attribute.StreamSize,FClusterShiftCount,FClusterSize);
   Run:=Attribute.NewRun(ntfsUnknownCluster,(Attribute.StreamAllocated shr FClusterShiftCount)); {Sparse Run}
   if Run = nil then Exit;
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Update Record}
   Current.RecordSize:=Current.CalculatedSize(FVolumeVersion);

   {Insert Record}
   if not FRecords.InsertRecord(Current) then Exit;

   {Get Bitmap}
   Attribute:=FMaster.GetAttribute(ntfsAttrTypeBitmap,ntfsBlankName,ntfsInstanceFirst);
   if Attribute = nil then Exit;

   {Alloc Bitmap}
   Count:=1;
   if not AllocBitmap(TNTFSBitmapAttribute(Attribute).Bitmap,ntfsDefaultRecordCount,ntfsFileTypeBadClus,Count) then Exit;

   {Dont save until others are loaded}
   Result:=True;
  finally
   FRecords.WriterUnlock;
  end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.CreateSecure:Boolean;
{Create the $Secure metafile}
var
 Count:LongWord;
 Cluster:Int64;

 Run:TNTFSDiskRun;
 Index:TNTFSDiskIndex;
 Current:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if not FRecords.WriterLock then Exit;
  try
   if FDriver = nil then Exit;
   if FMaster = nil then Exit;
   if FNTFSType = ntNONE then Exit;
   if FNTFSType = ntNTFS12 then Exit;

   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CreateSecure');
   {$ENDIF}

   {Check ReadOnly}
   if FReadOnly then Exit;

   {Create Record}
   Current:=FRecords.NewRecord(nil,ntfsFileTypeSecure,FVolumeVersion);
   if Current = nil then Exit;

   {Update Record}
   Current.Metafile:=True;
   if Current.RecordNumber < FMftMirrorCount then Current.Mirrored:=True;
   Current.RecordFlags:=ntfsFileRecordFlagInUse or ntfsFileRecordFlagIndexView;
   Current.HardLinkCount:=1;
   Current.SequenceNumber:=ntfsFileTypeSecure; {Same as record no for all but MFT}
   Current.RecordAllocated:=FFileRecordSize;
   Current.UpdateSequenceOffset:=Current.CalculatedSequenceOffset(FVolumeVersion);
   Current.UpdateSequenceLength:=Current.CalculatedSequenceLength(ntfsUpdateSequenceSize); {Previously FSectorSize however always 512}
   Current.AttributeOffset:=Current.CalculatedOffset(FVolumeVersion);

   {Add StandardInformation}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeStandardInformation,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   TNTFSStandardInformationAttribute(Attribute).Attributes:=faHidden or faSysFile or faMftIndexView;
   TNTFSStandardInformationAttribute(Attribute).SecurityId:=ntfsDefaultSecurityId101;
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add FileName}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeFileName,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   TNTFSFileNameAttribute(Attribute).ParentReference:=ntfsRootFileReference;
   TNTFSFileNameAttribute(Attribute).NameSpace:=ntfsNameSpaceBoth;
   TNTFSFileNameAttribute(Attribute).FileName:=ntfsFileNameSecure;
   Attribute.Indexed:=ntfsAttributeIndexed;
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add Data ($SDS)}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeData,ntfsStreamNameSecurity,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   Attribute.NonResident:=ntfsAttributeNonResident;
   Attribute.DataSize:=0;
   Attribute.StartVCN:=0;
   Attribute.LastVCN:=ntfsUnknownCluster;
   Attribute.StreamSize:=CalculateSecureSdsSize;
   Attribute.InitializedSize:=Attribute.StreamSize;
   Attribute.StreamAllocated:=NTFSRoundQuadWordToClusterSize(Attribute.StreamSize,FClusterShiftCount,FClusterSize);
   Cluster:=CalculateSecureSdsStartCluster;
   if Cluster = ntfsUnknownCluster then Exit;
   Run:=Attribute.NewRun(Cluster,(Attribute.StreamAllocated shr FClusterShiftCount));
   if Run = nil then Exit;
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add Index ($SDH)}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeIndexRoot,ntfsIndexNameSecurityHash,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   if not Attribute.NewIndex(FVolumeVersion,FSectorSize,ntfsAttrTypeNone,ntfsCollateTypeSecurityHash,FIndexRecordSize,FIndexCounterOffset) then Exit;
   Index:=Attribute.Index;
   if Index = nil then Exit;

   {Update Index}
   Index.ClustersPerIndex:=FClustersPerIndex;
   Index.IndexsPerCluster:=FIndexsPerCluster;
   Index.IndexCounterShift:=FIndexCounterShift;
   Index.IndexRecordShiftCount:=FIndexRecordShiftCount;
   Index.IndexRecordOffsetMask:=FIndexRecordOffsetMask;
   Index.Loaded:=True;

   {Update Index}
   Index.UpCase:=FUpCases;
   Index.CompareSecurityDescriptor:=CompareSecurityDescriptor;

   {Update Attribute}
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add Index ($SII)}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeIndexRoot,ntfsIndexNameSecurityId,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   if not Attribute.NewIndex(FVolumeVersion,FSectorSize,ntfsAttrTypeNone,ntfsCollateTypeLongWord,FIndexRecordSize,FIndexCounterOffset) then Exit;
   Index:=Attribute.Index;
   if Index = nil then Exit;

   {Update Index}
   Index.ClustersPerIndex:=FClustersPerIndex;
   Index.IndexsPerCluster:=FIndexsPerCluster;
   Index.IndexCounterShift:=FIndexCounterShift;
   Index.IndexRecordShiftCount:=FIndexRecordShiftCount;
   Index.IndexRecordOffsetMask:=FIndexRecordOffsetMask;
   Index.Loaded:=True;

   {Update Index}
   Index.UpCase:=FUpCases;
   Index.CompareSecurityDescriptor:=CompareSecurityDescriptor;

   {Update Attribute}
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Update Record}
   Current.RecordSize:=Current.CalculatedSize(FVolumeVersion);

   {Insert Record}
   if not FRecords.InsertRecord(Current) then Exit;

   {Get Bitmap}
   Attribute:=FMaster.GetAttribute(ntfsAttrTypeBitmap,ntfsBlankName,ntfsInstanceFirst);
   if Attribute = nil then Exit;

   {Alloc Bitmap}
   Count:=1;
   if not AllocBitmap(TNTFSBitmapAttribute(Attribute).Bitmap,ntfsDefaultRecordCount,ntfsFileTypeSecure,Count) then Exit;

   {Dont save until others are loaded}
   Result:=True;
  finally
   FRecords.WriterUnlock;
  end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.CreateUpCase:Boolean;
{Create the $UpCase metafile}
var
 Count:LongWord;
 Cluster:Int64;

 Run:TNTFSDiskRun;
 Current:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if not FRecords.WriterLock then Exit;
  try
   if FDriver = nil then Exit;
   if FMaster = nil then Exit;
   if FNTFSType = ntNONE then Exit;

   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CreateUpCase');
   {$ENDIF}

   {Check ReadOnly}
   if FReadOnly then Exit;

   {Create Record}
   Current:=FRecords.NewRecord(nil,ntfsFileTypeUpCase,FVolumeVersion);
   if Current = nil then Exit;

   {Update Record}
   Current.Metafile:=True;
   if Current.RecordNumber < FMftMirrorCount then Current.Mirrored:=True;
   Current.RecordFlags:=ntfsFileRecordFlagInUse;
   Current.HardLinkCount:=1;
   Current.SequenceNumber:=ntfsFileTypeUpCase; {Same as record no for all but MFT}
   Current.RecordAllocated:=FFileRecordSize;
   Current.UpdateSequenceOffset:=Current.CalculatedSequenceOffset(FVolumeVersion);
   Current.UpdateSequenceLength:=Current.CalculatedSequenceLength(ntfsUpdateSequenceSize); {Previously FSectorSize however always 512}
   Current.AttributeOffset:=Current.CalculatedOffset(FVolumeVersion);

   {Add StandardInformation}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeStandardInformation,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   TNTFSStandardInformationAttribute(Attribute).Attributes:=faHidden or faSysFile;
   TNTFSStandardInformationAttribute(Attribute).SecurityId:=ntfsDefaultSecurityId100;
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add FileName}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeFileName,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   TNTFSFileNameAttribute(Attribute).ParentReference:=ntfsRootFileReference;
   TNTFSFileNameAttribute(Attribute).NameSpace:=ntfsNameSpaceBoth;
   TNTFSFileNameAttribute(Attribute).FileName:=ntfsFileNameUpCase;
   Attribute.Indexed:=ntfsAttributeIndexed;
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add Data}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeData,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   Attribute.NonResident:=ntfsAttributeNonResident;
   Attribute.DataSize:=0;
   Attribute.StartVCN:=0;
   Attribute.LastVCN:=ntfsUnknownCluster;
   Attribute.StreamSize:=CalculateUpCaseSize;
   Attribute.InitializedSize:=Attribute.StreamSize;
   Attribute.StreamAllocated:=NTFSRoundQuadWordToClusterSize(Attribute.StreamSize,FClusterShiftCount,FClusterSize);
   Cluster:=CalculateUpCaseStartCluster;
   if Cluster = ntfsUnknownCluster then Exit;
   Run:=Attribute.NewRun(Cluster,(Attribute.StreamAllocated shr FClusterShiftCount));
   if Run = nil then Exit;
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Update Record}
   Current.RecordSize:=Current.CalculatedSize(FVolumeVersion);

   {Insert Record}
   if not FRecords.InsertRecord(Current) then Exit;

   {Get Bitmap}
   Attribute:=FMaster.GetAttribute(ntfsAttrTypeBitmap,ntfsBlankName,ntfsInstanceFirst);
   if Attribute = nil then Exit;

   {Alloc Bitmap}
   Count:=1;
   if not AllocBitmap(TNTFSBitmapAttribute(Attribute).Bitmap,ntfsDefaultRecordCount,ntfsFileTypeUpCase,Count) then Exit;

   {Dont save until others are loaded}
   Result:=True;
  finally
   FRecords.WriterUnlock;
  end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.CreateExtend:Boolean;
{Create the $Extend metafile}
var
 Count:LongWord;

 Index:TNTFSDiskIndex;
 Current:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if not FRecords.WriterLock then Exit;
  try
   if FDriver = nil then Exit;
   if FMaster = nil then Exit;
   if FNTFSType = ntNONE then Exit;
   if FNTFSType = ntNTFS12 then Exit;

   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CreateExtend');
   {$ENDIF}

   {Check ReadOnly}
   if FReadOnly then Exit;

   {Create Record}
   Current:=FRecords.NewRecord(nil,ntfsFileTypeExtend,FVolumeVersion);
   if Current = nil then Exit;

   {Update Record}
   Current.Metafile:=True;
   if Current.RecordNumber < FMftMirrorCount then Current.Mirrored:=True;
   Current.RecordFlags:=ntfsFileRecordFlagInUse or ntfsFileRecordFlagDirectory;
   Current.HardLinkCount:=1;
   Current.SequenceNumber:=ntfsFileTypeExtend; {Same as record no for all but MFT}
   Current.RecordAllocated:=FFileRecordSize;
   Current.UpdateSequenceOffset:=Current.CalculatedSequenceOffset(FVolumeVersion);
   Current.UpdateSequenceLength:=Current.CalculatedSequenceLength(ntfsUpdateSequenceSize); {Previously FSectorSize however always 512}
   Current.AttributeOffset:=Current.CalculatedOffset(FVolumeVersion);

   {Add StandardInformation}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeStandardInformation,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   TNTFSStandardInformationAttribute(Attribute).Attributes:=faHidden or faSysFile;
   TNTFSStandardInformationAttribute(Attribute).SecurityId:=ntfsDefaultSecurityId101;
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add FileName}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeFileName,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   TNTFSFileNameAttribute(Attribute).ParentReference:=ntfsRootFileReference;
   TNTFSFileNameAttribute(Attribute).NameSpace:=ntfsNameSpaceBoth;
   TNTFSFileNameAttribute(Attribute).FileName:=ntfsFileNameExtend;
   Attribute.Indexed:=ntfsAttributeIndexed;
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add Index ($I30)}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeIndexRoot,ntfsIndexNameFileName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   if not Attribute.NewIndex(FVolumeVersion,FSectorSize,ntfsAttrTypeFileName,ntfsCollateTypeFileName,FIndexRecordSize,FIndexCounterOffset) then Exit;
   Index:=Attribute.Index;
   if Index = nil then Exit;

   {Update Index}
   Index.ClustersPerIndex:=FClustersPerIndex;
   Index.IndexsPerCluster:=FIndexsPerCluster;
   Index.IndexCounterShift:=FIndexCounterShift;
   Index.IndexRecordShiftCount:=FIndexRecordShiftCount;
   Index.IndexRecordOffsetMask:=FIndexRecordOffsetMask;
   Index.Loaded:=True;

   {Update Index}
   Index.UpCase:=FUpCases;
   Index.CompareSecurityDescriptor:=CompareSecurityDescriptor;

   {Update Attribute}
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Update Record}
   Current.RecordSize:=Current.CalculatedSize(FVolumeVersion);

   {Insert Record}
   if not FRecords.InsertRecord(Current) then Exit;

   {Get Bitmap}
   Attribute:=FMaster.GetAttribute(ntfsAttrTypeBitmap,ntfsBlankName,ntfsInstanceFirst);
   if Attribute = nil then Exit;

   {Alloc Bitmap}
   Count:=1;
   if not AllocBitmap(TNTFSBitmapAttribute(Attribute).Bitmap,ntfsDefaultRecordCount,ntfsFileTypeExtend,Count) then Exit;

   {Dont save until others are loaded}
   Result:=True;
  finally
   FRecords.WriterUnlock;
  end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.CreateObjId:Boolean;
{Create the $ObjId metafile}
var
 Count:LongWord;

 Index:TNTFSDiskIndex;
 Current:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if not FRecords.WriterLock then Exit;
  try
   if FDriver = nil then Exit;
   if FMaster = nil then Exit;
   if FNTFSType = ntNONE then Exit;
   if FNTFSType = ntNTFS12 then Exit;

   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CreateObjId');
   {$ENDIF}

   {Check ReadOnly}
   if FReadOnly then Exit;

   {Create Record}
   Current:=FRecords.NewRecord(nil,ntfsObjIdRecordNumber,FVolumeVersion);
   if Current = nil then Exit;

   {Update Record}
   Current.Metafile:=True;
   if Current.RecordNumber < FMftMirrorCount then Current.Mirrored:=True;
   Current.RecordFlags:=ntfsFileRecordFlagInUse or ntfsFileRecordFlagUnknown1 or ntfsFileRecordFlagIndexView;
   Current.HardLinkCount:=1;
   Current.SequenceNumber:=1;
   Current.RecordAllocated:=FFileRecordSize;
   Current.UpdateSequenceOffset:=Current.CalculatedSequenceOffset(FVolumeVersion);
   Current.UpdateSequenceLength:=Current.CalculatedSequenceLength(ntfsUpdateSequenceSize); {Previously FSectorSize however always 512}
   Current.AttributeOffset:=Current.CalculatedOffset(FVolumeVersion);

   {Add StandardInformation}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeStandardInformation,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   TNTFSStandardInformationAttribute(Attribute).Attributes:=faHidden or faSysFile or faArchive or faMftIndexView;
   TNTFSStandardInformationAttribute(Attribute).SecurityId:=ntfsDefaultSecurityId101;
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add FileName}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeFileName,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   TNTFSFileNameAttribute(Attribute).ParentReference:=ntfsExtendFileReference;
   TNTFSFileNameAttribute(Attribute).NameSpace:=ntfsNameSpaceBoth;
   TNTFSFileNameAttribute(Attribute).FileName:=ntfsFileNameObjId;
   Attribute.Indexed:=ntfsAttributeIndexed;
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add Index ($O)}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeIndexRoot,ntfsIndexNameObjectId,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   if not Attribute.NewIndex(FVolumeVersion,FSectorSize,ntfsAttrTypeNone,ntfsCollateTypeGUID,FIndexRecordSize,FIndexCounterOffset) then Exit;
   Index:=Attribute.Index;
   if Index = nil then Exit;

   {Update Index}
   Index.ClustersPerIndex:=FClustersPerIndex;
   Index.IndexsPerCluster:=FIndexsPerCluster;
   Index.IndexCounterShift:=FIndexCounterShift;
   Index.IndexRecordShiftCount:=FIndexRecordShiftCount;
   Index.IndexRecordOffsetMask:=FIndexRecordOffsetMask;
   Index.Loaded:=True;

   {Update Index}
   Index.UpCase:=FUpCases;
   Index.CompareSecurityDescriptor:=CompareSecurityDescriptor;

   {Update Attribute}
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Update Record}
   Current.RecordSize:=Current.CalculatedSize(FVolumeVersion);

   {Insert Record}
   if not FRecords.InsertRecord(Current) then Exit;

   {Get Bitmap}
   Attribute:=FMaster.GetAttribute(ntfsAttrTypeBitmap,ntfsBlankName,ntfsInstanceFirst);
   if Attribute = nil then Exit;

   {Alloc Bitmap}
   Count:=1;
   if not AllocBitmap(TNTFSBitmapAttribute(Attribute).Bitmap,ntfsDefaultRecordCount,ntfsObjIdRecordNumber,Count) then Exit;

   {Dont save until others are loaded}
   Result:=True;
  finally
   FRecords.WriterUnlock;
  end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.CreateQuota:Boolean;
{Create the $Quota metafile}
var
 Count:LongWord;

 Index:TNTFSDiskIndex;
 Current:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if not FRecords.WriterLock then Exit;
  try
   if FDriver = nil then Exit;
   if FMaster = nil then Exit;
   if FNTFSType = ntNONE then Exit;

   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CreateQuota');
   {$ENDIF}

   {Check ReadOnly}
   if FReadOnly then Exit;

   {Create Record}
   if FNTFSType = ntNTFS12 then
    begin
     Current:=FRecords.NewRecord(nil,ntfs12FileTypeQuota,FVolumeVersion);
    end
   else
    begin
     Current:=FRecords.NewRecord(nil,ntfsQuotaRecordNumber,FVolumeVersion);
    end;
   if Current = nil then Exit;

   {Update Record}
   Current.Metafile:=True;
   if Current.RecordNumber < FMftMirrorCount then Current.Mirrored:=True;
   Current.RecordFlags:=ntfsFileRecordFlagInUse or ntfsFileRecordFlagUnknown1 or ntfsFileRecordFlagIndexView;
   Current.HardLinkCount:=1;
   if FNTFSType = ntNTFS12 then Current.SequenceNumber:=ntfs12FileTypeQuota else Current.SequenceNumber:=1;
   Current.RecordAllocated:=FFileRecordSize;
   Current.UpdateSequenceOffset:=Current.CalculatedSequenceOffset(FVolumeVersion);
   Current.UpdateSequenceLength:=Current.CalculatedSequenceLength(ntfsUpdateSequenceSize); {Previously FSectorSize however always 512}
   Current.AttributeOffset:=Current.CalculatedOffset(FVolumeVersion);

   {Add StandardInformation}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeStandardInformation,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   TNTFSStandardInformationAttribute(Attribute).Attributes:=faHidden or faSysFile or faArchive or faMftIndexView;
   TNTFSStandardInformationAttribute(Attribute).SecurityId:=ntfsDefaultSecurityId101;
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add FileName}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeFileName,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   TNTFSFileNameAttribute(Attribute).ParentReference:=ntfsExtendFileReference;
   TNTFSFileNameAttribute(Attribute).NameSpace:=ntfsNameSpaceBoth;
   TNTFSFileNameAttribute(Attribute).FileName:=ntfsFileNameQuota;
   Attribute.Indexed:=ntfsAttributeIndexed;
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add Index ($O)}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeIndexRoot,ntfsIndexNameOwnerId,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   if not Attribute.NewIndex(FVolumeVersion,FSectorSize,ntfsAttrTypeNone,ntfsCollateTypeSID,FIndexRecordSize,FIndexCounterOffset) then Exit;
   Index:=Attribute.Index;
   if Index = nil then Exit;

   {Update Index}
   Index.ClustersPerIndex:=FClustersPerIndex;
   Index.IndexsPerCluster:=FIndexsPerCluster;
   Index.IndexCounterShift:=FIndexCounterShift;
   Index.IndexRecordShiftCount:=FIndexRecordShiftCount;
   Index.IndexRecordOffsetMask:=FIndexRecordOffsetMask;
   Index.Loaded:=True;

   {Update Index}
   Index.UpCase:=FUpCases;
   Index.CompareSecurityDescriptor:=CompareSecurityDescriptor;

   {Update Attribute}
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add Index ($Q)}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeIndexRoot,ntfsIndexNameQuota,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   if not Attribute.NewIndex(FVolumeVersion,FSectorSize,ntfsAttrTypeNone,ntfsCollateTypeLongWord,FIndexRecordSize,FIndexCounterOffset) then Exit;
   Index:=Attribute.Index;
   if Index = nil then Exit;

   {Update Index}
   Index.ClustersPerIndex:=FClustersPerIndex;
   Index.IndexsPerCluster:=FIndexsPerCluster;
   Index.IndexCounterShift:=FIndexCounterShift;
   Index.IndexRecordShiftCount:=FIndexRecordShiftCount;
   Index.IndexRecordOffsetMask:=FIndexRecordOffsetMask;
   Index.Loaded:=True;

   {Update Index}
   Index.UpCase:=FUpCases;
   Index.CompareSecurityDescriptor:=CompareSecurityDescriptor;

   {Update Attribute}
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Update Record}
   Current.RecordSize:=Current.CalculatedSize(FVolumeVersion);

   {Insert Record}
   if not FRecords.InsertRecord(Current) then Exit;

   {Get Bitmap}
   Attribute:=FMaster.GetAttribute(ntfsAttrTypeBitmap,ntfsBlankName,ntfsInstanceFirst);
   if Attribute = nil then Exit;

   {Alloc Bitmap}
   Count:=1;
   if FNTFSType = ntNTFS12 then
    begin
     if not AllocBitmap(TNTFSBitmapAttribute(Attribute).Bitmap,ntfsDefaultRecordCount,ntfs12FileTypeQuota,Count) then Exit;
    end
   else
    begin
     if not AllocBitmap(TNTFSBitmapAttribute(Attribute).Bitmap,ntfsDefaultRecordCount,ntfsQuotaRecordNumber,Count) then Exit;
    end;

   {Dont save until others are loaded}
   Result:=True;
  finally
   FRecords.WriterUnlock;
  end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.CreateReparse:Boolean;
{Create the $Reparse metafile}
var
 Count:LongWord;

 Index:TNTFSDiskIndex;
 Current:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if not FRecords.WriterLock then Exit;
  try
   if FDriver = nil then Exit;
   if FMaster = nil then Exit;
   if FNTFSType = ntNONE then Exit;
   if FNTFSType = ntNTFS12 then Exit;

   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CreateReparse');
   {$ENDIF}

   {Check ReadOnly}
   if FReadOnly then Exit;

   {Create Record}
   Current:=FRecords.NewRecord(nil,ntfsReparseRecordNumber,FVolumeVersion);
   if Current = nil then Exit;

   {Update Record}
   Current.Metafile:=True;
   if Current.RecordNumber < FMftMirrorCount then Current.Mirrored:=True;
   Current.RecordFlags:=ntfsFileRecordFlagInUse or ntfsFileRecordFlagUnknown1 or ntfsFileRecordFlagIndexView;
   Current.HardLinkCount:=1;
   Current.SequenceNumber:=1;
   Current.RecordAllocated:=FFileRecordSize;
   Current.UpdateSequenceOffset:=Current.CalculatedSequenceOffset(FVolumeVersion);
   Current.UpdateSequenceLength:=Current.CalculatedSequenceLength(ntfsUpdateSequenceSize); {Previously FSectorSize however always 512}
   Current.AttributeOffset:=Current.CalculatedOffset(FVolumeVersion);

   {Add StandardInformation}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeStandardInformation,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   TNTFSStandardInformationAttribute(Attribute).Attributes:=faHidden or faSysFile or faArchive or faMftIndexView;
   TNTFSStandardInformationAttribute(Attribute).SecurityId:=ntfsDefaultSecurityId101;
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add FileName}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeFileName,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   TNTFSFileNameAttribute(Attribute).ParentReference:=ntfsExtendFileReference;
   TNTFSFileNameAttribute(Attribute).NameSpace:=ntfsNameSpaceBoth;
   TNTFSFileNameAttribute(Attribute).FileName:=ntfsFileNameReparse;
   Attribute.Indexed:=ntfsAttributeIndexed;
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add Index ($R)}
   Attribute:=Current.NewAttribute(nil,ntfsAttrTypeIndexRoot,ntfsIndexNameReparse,FVolumeVersion);
   if Attribute = nil then Exit;

   {Update Attribute}
   if not Attribute.NewIndex(FVolumeVersion,FSectorSize,ntfsAttrTypeNone,ntfsCollateTypeGUID,FIndexRecordSize,FIndexCounterOffset) then Exit;
   Index:=Attribute.Index;
   if Index = nil then Exit;

   {Update Index}
   Index.ClustersPerIndex:=FClustersPerIndex;
   Index.IndexsPerCluster:=FIndexsPerCluster;
   Index.IndexCounterShift:=FIndexCounterShift;
   Index.IndexRecordShiftCount:=FIndexRecordShiftCount;
   Index.IndexRecordOffsetMask:=FIndexRecordOffsetMask;
   Index.Loaded:=True;

   {Update Index}
   Index.UpCase:=FUpCases;
   Index.CompareSecurityDescriptor:=CompareSecurityDescriptor;

   {Update Attribute}
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Update Record}
   Current.RecordSize:=Current.CalculatedSize(FVolumeVersion);

   {Insert Record}
   if not FRecords.InsertRecord(Current) then Exit;

   {Get Bitmap}
   Attribute:=FMaster.GetAttribute(ntfsAttrTypeBitmap,ntfsBlankName,ntfsInstanceFirst);
   if Attribute = nil then Exit;

   {Alloc Bitmap}
   Count:=1;
   if not AllocBitmap(TNTFSBitmapAttribute(Attribute).Bitmap,ntfsDefaultRecordCount,ntfsReparseRecordNumber,Count) then Exit;

   {Dont save until others are loaded}
   Result:=True;
  finally
   FRecords.WriterUnlock;
  end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.CreateUsnJrnl:Boolean;
{Create the $UsnJrnl metafile (Optional)}
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if not FRecords.WriterLock then Exit;
  try
   if FDriver = nil then Exit;
   if FMaster = nil then Exit;
   if FNTFSType = ntNONE then Exit;
   if FNTFSType = ntNTFS12 then Exit;

   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CreateUsnJrnl');
   {$ENDIF}

   {Check ReadOnly}
   if FReadOnly then Exit;

   Result:=True;
  finally
   FRecords.WriterUnlock;
  end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.CreateReserved:Boolean;
{Create the reserved MFT entries}
var
 Count:LongWord;
 Counter:LongWord;
 Descriptor:Pointer;

 Security:TNTFSSecurity;
 Current:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if not FRecords.WriterLock then Exit;
  try
   if FDriver = nil then Exit;
   if FMaster = nil then Exit;
   if FNTFSType = ntNONE then Exit;

   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CreateReserved');
   {$ENDIF}

   {Check ReadOnly}
   if FReadOnly then Exit;

   for Counter:=ntfsFileTypeReserved1 to ntfsFileTypeReserved4 do
    begin
     {Create Record}
     Current:=FRecords.NewRecord(nil,Counter,FVolumeVersion);
     if Current = nil then Exit;

     {Update Record}
     Current.Reserved:=True;
     if Current.RecordNumber < FMftMirrorCount then Current.Mirrored:=True;
     Current.RecordFlags:=ntfsFileRecordFlagInUse;
     Current.HardLinkCount:=0;
     Current.SequenceNumber:=Counter; {Same as record no for all but MFT}
     Current.RecordAllocated:=FFileRecordSize;
     Current.UpdateSequenceOffset:=Current.CalculatedSequenceOffset(FVolumeVersion);
     Current.UpdateSequenceLength:=Current.CalculatedSequenceLength(ntfsUpdateSequenceSize); {Previously FSectorSize however always 512}
     Current.AttributeOffset:=Current.CalculatedOffset(FVolumeVersion);

     {Add StandardInformation}
     Attribute:=Current.NewAttribute(nil,ntfsAttrTypeStandardInformation,ntfsBlankName,FVolumeVersion);
     if Attribute = nil then Exit;

     {Update Attribute}
     TNTFSStandardInformationAttribute(Attribute).Attributes:=faHidden or faSysFile;
     Attribute.DataSize:=Attribute.CalculatedStreamSize(ntfsNTFS12); {Always version 1.2}
     Attribute.AttributeSize:=Attribute.CalculatedSize(ntfsNTFS12);  {Always version 1.2}

     {Add Security}
     Attribute:=Current.NewAttribute(nil,ntfsAttrTypeSecurityDescriptor,ntfsBlankName,FVolumeVersion);
     if Attribute = nil then Exit;

     {Update Attribute}
     Descriptor:=nil;
     if not NTFSCreateDefaultDescriptor(ntfsDefaultDescriptorReserved,Descriptor,FVolumeVersion) then Exit;
     try
      Security:=TNTFSSecurity.CreateFromDescriptor(FSecurityLocal,Descriptor);
      if Security = nil then Exit;
      if not TNTFSSecurityDescriptorAttribute(Attribute).NewSecurity(Security) then Exit;
     finally
      NTFSDestroyDefaultDescriptor(Descriptor,FVolumeVersion);
     end;
     Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
     Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

     {Add Data}
     Attribute:=Current.NewAttribute(nil,ntfsAttrTypeData,ntfsBlankName,FVolumeVersion);
     if Attribute = nil then Exit;

     {Update Attribute}
     Attribute.DataSize:=0;
     Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

     {Update Record}
     Current.RecordSize:=Current.CalculatedSize(FVolumeVersion);

     {Insert Record}
     if not FRecords.InsertRecord(Current) then Exit;

     {Get Bitmap}
     Attribute:=FMaster.GetAttribute(ntfsAttrTypeBitmap,ntfsBlankName,ntfsInstanceFirst);
     if Attribute = nil then Exit;

     {Alloc Bitmap}
     Count:=1;
     if not AllocBitmap(TNTFSBitmapAttribute(Attribute).Bitmap,ntfsDefaultRecordCount,Counter,Count) then Exit;

     {Dont save until others are loaded}
    end;

   Result:=True;
  finally
   FRecords.WriterUnlock;
  end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.CreateExpansion:Boolean;
{Create the expansion MFT entries}
var
 Counter:LongWord;

 Current:TNTFSDiskRecord;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if not FRecords.WriterLock then Exit;
  try
   if FDriver = nil then Exit;
   if FMaster = nil then Exit;
   if FNTFSType = ntNONE then Exit;

   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CreateExpansion');
   {$ENDIF}

   {Check ReadOnly}
   if FReadOnly then Exit;

   for Counter:=ntfsFileTypeExpansion1 to ntfsFileTypeExpansion8 do
    begin
     {Create Record}
     Current:=FRecords.NewRecord(nil,Counter,FVolumeVersion);
     if Current = nil then Exit;

     {Update Record}
     Current.Expansion:=True;
     if Current.RecordNumber < FMftMirrorCount then Current.Mirrored:=True;
     Current.RecordFlags:=ntfsFileRecordFlagNone;
     Current.HardLinkCount:=0;
     Current.SequenceNumber:=0;
     Current.RecordAllocated:=FFileRecordSize;
     Current.UpdateSequenceOffset:=Current.CalculatedSequenceOffset(FVolumeVersion);
     Current.UpdateSequenceLength:=Current.CalculatedSequenceLength(ntfsUpdateSequenceSize); {Previously FSectorSize however always 512}
     Current.AttributeOffset:=Current.CalculatedOffset(FVolumeVersion);

     {Update Record}
     Current.RecordSize:=Current.CalculatedSize(FVolumeVersion);

     {Insert Record}
     if not FRecords.InsertRecord(Current) then Exit;

     {Dont allocate in bitmap}

     {Dont save until others are loaded}
    end;

   Result:=True;
  finally
   FRecords.WriterUnlock;
  end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.LoadTables:Boolean;
begin
 {}
 Result:=False;

 if not FTables.WriterLock then Exit;
 try
  if FDriver = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadTables');
  {$ENDIF}

  {Get each Table}
  if GetTableEx(ntfsTableTypeMft,True) = nil then Exit;
  if GetTableEx(ntfsTableTypeMftMirr,True) = nil then Exit;

  Result:=True;
 finally
  FTables.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.LoadBlocks:Boolean;
var
 BlockNo:LongWord;
begin
 {}
 Result:=False;

 if not FBlocks.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FTotalBlockCount = 0 then Exit;

  //To Do //Critical //FBlocksLoaded //See CDFS

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadBlocks');
  {$ENDIF}

  {Get each Block}
  BlockNo:=0;
  while BlockNo < FTotalBlockCount do
   begin
    if GetBlockEx(BlockNo,True) = nil then Exit;

    Inc(BlockNo);
   end;

  Result:=True;
 finally
  FBlocks.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.LoadMetafiles:Boolean;
{Load each of the metafile entries}
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if not FRecords.WriterLock then Exit;
  try
   if FDriver = nil then Exit;
   if FNTFSType = ntNONE then Exit;

   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadMetafiles');
   {$ENDIF}

   {Get each Metafile}
   if GetMetafileEx(ntfsFileTypeRoot,True) = nil then Exit;                                   {Root must be loaded first}
   if GetMetafileEx(ntfsFileTypeMft,True) = nil then Exit;
   if GetMetafileEx(ntfsFileTypeMftMirr,True) = nil then Exit;
   if GetMetafileEx(ntfsFileTypeLogFile,True) = nil then Exit;
   if GetMetafileEx(ntfsFileTypeVolume,True) = nil then Exit;
   if GetMetafileEx(ntfsFileTypeAttrDef,True) = nil then Exit;
   if GetMetafileEx(ntfsFileTypeBitmap,True) = nil then Exit;
   if GetMetafileEx(ntfsFileTypeBoot,True) = nil then Exit;
   if GetMetafileEx(ntfsFileTypeBadClus,True) = nil then Exit;
   if GetMetafileEx(ntfsFileTypeSecure,True) = nil then Exit;                                 {Also ntfs12FileTypeQuota}
   if GetMetafileEx(ntfsFileTypeUpCase,True) = nil then Exit;
   if (GetMetafileEx(ntfsFileTypeExtend,True) = nil) and (FNTFSType <> ntNTFS12) then Exit;   {Not available in NTFS 1.2}
   {Metafiles within $Extend}
   if (GetMetafileEx(ntfsFileTypeObjId,True) = nil) and (FNTFSType <> ntNTFS12) then Exit;    {Not available in NTFS 1.2}
   if (GetMetafileEx(ntfsFileTypeQuota,True) = nil) and (FNTFSType <> ntNTFS12) then Exit;    {Not available in NTFS 1.2}
   if (GetMetafileEx(ntfsFileTypeReparse,True) = nil) and (FNTFSType <> ntNTFS12) then Exit;  {Not available in NTFS 1.2}
   GetMetafileEx(ntfsFileTypeUsnJrnl,True);                                                   {UsnJrnl is an optional file}

   Result:=True;
  finally
   FRecords.WriterUnlock;
  end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}
//Remove
function TNTFSFileSystem.LoadEntriesNew(AParent:TDiskEntry):Boolean;
{Load each of the entries within the parent entry}
var
 Instance:Integer;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
 Alternate:TNTFSDiskAttribute;

 Key:TNTFSAttributeKey;
 Index:TNTFSAttributeIndex;

 Entry:TNTFSDiskEntry;
 Current:TNTFSDiskRecord;
begin
 {}
 Result:=False;

 if FDriver = nil then Exit;
 if AParent = nil then Exit;

 //To Do //Remove ?

 {Check Loaded}
 if not AParent.EntriesLoaded then
  begin
   if not FEntries.WriterLock then Exit;
   try
    {Check Loaded (After Lock)}
    if not AParent.EntriesLoaded then
     begin
      {$IFDEF NTFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadEntries - Parent = ' + AParent.Name);
      {$ENDIF}

      {Get Origin}
      if not FRecords.WriterLock then Exit;
      try
       if TNTFSDiskEntry(AParent).Origin = nil then Exit;
       Origin:=TNTFSDiskEntry(AParent).Origin.Origin;
       if Origin = nil then Exit;

       {Check Directory}
       if (AParent.Attributes and faDirectory) = faDirectory then
        begin
         {Get Attribute}
         Attribute:=Origin.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameFileName,ntfsInstanceFirst);
         if Attribute <> nil then
          begin
           {Load Index}
           if not LoadIndex(Origin,Attribute) then Exit;

           {Load Records}
           //if not LoadRecords(Origin,Attribute) then Exit; //To Do //Testing8

           {Get Index}
           Index:=TNTFSAttributeIndex(Attribute.Index);
           if Index = nil then Exit;

           {Get Key}
           Key:=TNTFSAttributeKey(Index.First);

           {Load Dot}
           Entry:=TNTFSDiskEntry(AParent).CreateDot;
           if Entry = nil then Exit;
           if not FEntries.Add(Entry,AParent) then Exit;

           {Load DotDot}
           if AParent <> FRoot then
            begin
             Entry:=TNTFSDiskEntry(AParent).CreateDotDot;
             if Entry = nil then Exit;
             if not FEntries.Add(Entry,AParent) then Exit;
            end;

           {Load Links}
           while Key <> nil do
            begin
             {$IFDEF NTFS_DEBUG}
             if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadEntries - Key = ' + PtrToHex(Key));
             if FILESYS_LOG_ENABLED then if Key.Node <> nil then FileSysLogDebug('TNTFSFileSystem.LoadEntries - Node = ' + IntToHex(Key.Node.RecordNumber,16));
             {$ENDIF}

             if Key.Attribute = nil then  //To Do //Testing8
              begin
               if Key.Key = nil then Exit;

               {Get Record}
               Current:=GetRecordEx(nil,Key.RecordNumber,False,True);
               if Current = nil then
                begin
                 Key.Invalid:=True;
                end
               else
                begin
                 {Check Reference}
                 if Key.FileReference <> Current.FileReference then
                  begin
                   Key.Invalid:=True;
                  end
                 else
                  begin
                   {Get Attribute}
                   Key.Attribute:=Current.GetFileNameByKey(Key);

                   {Check Attribute}
                   if Key.Attribute = nil then
                    begin
                     Key.Invalid:=True;
                    end
                   else
                    begin
                     {Get Entry}
                     if Current.GetLink(Key.Attribute) = nil then {This resolves the root self reference and avoids double loading of DOS and Win32 entries}
                      begin
                       case TNTFSFileNameAttribute(Key.Attribute).NameSpace of
                        ntfsNameSpacePosix:begin {HardLinks use Posix namespace}
                          {Get Attribute}
                          Attribute:=Key.Attribute;

                          {Load Entry}
                          if not LoadEntry(TNTFSDiskEntry(AParent),Current,Attribute,nil) then Exit;
                         end;
                        ntfsNameSpaceWin32:begin
                          {Get Attribute}
                          Attribute:=Key.Attribute;

                          {Get Alternate}
                          Instance:=1;
                          Alternate:=Current.GetFileNameByNameSpace(ntfsAnyName,TNTFSFileNameAttribute(Attribute).ParentReference,ntfsNameSpaceDos,Instance);
                          while Alternate <> nil do
                           begin
                            {Get Entry}
                            if Current.GetLink(Alternate) = nil then Break;

                            {Get Alternate}
                            Inc(Instance);
                            Alternate:=Current.GetFileNameByNameSpace(ntfsAnyName,TNTFSFileNameAttribute(Attribute).ParentReference,ntfsNameSpaceDos,Instance);
                           end;

                          {Load Entry}
                          if not LoadEntry(TNTFSDiskEntry(AParent),Current,Attribute,Alternate) then Exit;
                         end;
                        ntfsNameSpaceDos:begin
                          {Get Alternate}
                          Alternate:=Key.Attribute;

                          {Get Attribute}
                          Instance:=1;
                          Attribute:=Current.GetFileNameByNameSpace(ntfsAnyName,TNTFSFileNameAttribute(Alternate).ParentReference,ntfsNameSpaceWin32,Instance);
                          while Attribute <> nil do
                           begin
                            {Get Entry}
                            if Current.GetLink(Attribute) = nil then Break;

                            {Get Attribute}
                            Inc(Instance);
                            Attribute:=Current.GetFileNameByNameSpace(ntfsAnyName,TNTFSFileNameAttribute(Alternate).ParentReference,ntfsNameSpaceWin32,Instance);
                           end;
                          if Attribute = nil then
                           begin
                            {Check Attribute}
                            Alternate:=nil;
                            Attribute:=Key.Attribute;

                            {Load Entry}
                            if not LoadEntry(TNTFSDiskEntry(AParent),Current,Attribute,Alternate) then Exit;
                           end;
                         end;
                        ntfsNameSpaceBoth:begin
                          {Get Attribute}
                          Attribute:=Key.Attribute;

                          {Load Entry}
                          if not LoadEntry(TNTFSDiskEntry(AParent),Current,Attribute,nil) then Exit;
                         end;
                       end;
                      end;
                    end;
                  end;
                end;
              end;  //To Do //Testing8

             (*if Key.Attribute <> nil then
              begin
               {Get Record}
               Current:=GetRecordEx(nil,Key.RecordNumber,False,True);
               if Current = nil then Exit;

               {Get Entry}
               if Current.GetLink(Key.Attribute) = nil then {This resolves the root self reference and avoids double loading of DOS and Win32 entries}
                begin
                 case TNTFSFileNameAttribute(Key.Attribute).NameSpace of
                  ntfsNameSpacePosix:begin {HardLinks use Posix namespace}
                    {Get Attribute}
                    Attribute:=Key.Attribute;

                    {Load Entry}
                    if not LoadEntry(TNTFSDiskEntry(AParent),Current,Attribute,nil) then Exit;
                   end;
                  ntfsNameSpaceWin32:begin
                    {Get Attribute}
                    Attribute:=Key.Attribute;

                    {Get Alternate}
                    Instance:=1;
                    Alternate:=Current.GetFileNameByNameSpace(ntfsAnyName,TNTFSFileNameAttribute(Attribute).ParentReference,ntfsNameSpaceDos,Instance);
                    while Alternate <> nil do
                     begin
                      {Get Entry}
                      if Current.GetLink(Alternate) = nil then Break;

                      {Get Alternate}
                      Inc(Instance);
                      Alternate:=Current.GetFileNameByNameSpace(ntfsAnyName,TNTFSFileNameAttribute(Attribute).ParentReference,ntfsNameSpaceDos,Instance);
                     end;

                    {Load Entry}
                    if not LoadEntry(TNTFSDiskEntry(AParent),Current,Attribute,Alternate) then Exit;
                   end;
                  ntfsNameSpaceDos:begin
                    {Get Alternate}
                    Alternate:=Key.Attribute;

                    {Get Attribute}
                    Instance:=1;
                    Attribute:=Current.GetFileNameByNameSpace(ntfsAnyName,TNTFSFileNameAttribute(Alternate).ParentReference,ntfsNameSpaceWin32,Instance);
                    while Attribute <> nil do
                     begin
                      {Get Entry}
                      if Current.GetLink(Attribute) = nil then Break;

                      {Get Attribute}
                      Inc(Instance);
                      Attribute:=Current.GetFileNameByNameSpace(ntfsAnyName,TNTFSFileNameAttribute(Alternate).ParentReference,ntfsNameSpaceWin32,Instance);
                     end;
                    if Attribute = nil then //To Do //Testing
                     begin                  //To Do //Testing
                      {Check Attribute}
                      if Attribute = nil then Alternate:=nil;
                      if Attribute = nil then Attribute:=Key.Attribute;

                      {Load Entry}
                      if not LoadEntry(TNTFSDiskEntry(AParent),Current,Attribute,Alternate) then Exit;
                     end;                   //To Do //Testing
                   end;
                  ntfsNameSpaceBoth:begin
                    {Get Attribute}
                    Attribute:=Key.Attribute;

                    {Load Entry}
                    if not LoadEntry(TNTFSDiskEntry(AParent),Current,Attribute,nil) then Exit;
                   end;
                 end;
                end;
              end;*)

             {Get Key}
             Key:=TNTFSAttributeKey(Key.Next);
            end;
          end;
        end;

       {Check File or Directory}
       if (AParent.Attributes and (faFile or faDirectory)) <> faNone then
        begin
         {Load Streams} {Data streams only}
         Instance:=1;
         Attribute:=Origin.GetAttribute(ntfsAttrTypeData,ntfsAnyName,Instance);
         while Attribute <> nil do
          begin
           {Check Name} {Dont load default or multiple instances}
           if (Attribute.AttributeName <> ntfsBlankName) and (Attribute.StartVCN = 0) then
            begin
             {Load Entry} {Do not call GetStream as multiple Links may refer to the same streams}
             if not LoadEntry(TNTFSDiskEntry(AParent),Origin,Attribute,nil) then Exit;
            end;

           {Get Attribute}
           Inc(Instance);
           Attribute:=Origin.GetAttribute(ntfsAttrTypeData,ntfsAnyName,Instance);
          end;
        end;

       AParent.EntriesLoaded:=True;
      finally
       FRecords.WriterUnlock;
      end;
     end;
   finally
    FEntries.WriterUnlock;
   end;
  end;

 Result:=True;
end;
//Remove
{=============================================================================}

function TNTFSFileSystem.LoadEntries(AParent:TDiskEntry):Boolean;
{Load each of the entries within the parent entry}
var
 Instance:Integer;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
 Alternate:TNTFSDiskAttribute;

 Key:TNTFSAttributeKey;
 Index:TNTFSAttributeIndex;

 Entry:TNTFSDiskEntry;
 Current:TNTFSDiskRecord;
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
      {$IFDEF NTFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadEntries - Parent = ' + AParent.Name);
      {$ENDIF}

      {Get Origin}
      if not FRecords.WriterLock then Exit;
      try
       if TNTFSDiskEntry(AParent).Origin = nil then Exit;
       Origin:=TNTFSDiskEntry(AParent).Origin.Origin;
       if Origin = nil then Exit;

       {Check Directory}
       if (AParent.Attributes and faDirectory) = faDirectory then
        begin
         {Get Attribute}
         Attribute:=Origin.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameFileName,ntfsInstanceFirst);
         if Attribute <> nil then
          begin
           {Load Index}
           if not LoadIndex(Origin,Attribute) then Exit;

           {Load Records}
           if not LoadRecords(Origin,Attribute) then Exit;

           {Get Index}
           Index:=TNTFSAttributeIndex(Attribute.Index);
           if Index = nil then Exit;

           {Get Key}
           Key:=TNTFSAttributeKey(Index.First);

           {Load Dot}
           Entry:=TNTFSDiskEntry(AParent).CreateDot;
           if Entry = nil then Exit;
           if not FEntries.Add(Entry,AParent) then Exit;

           {Load DotDot}
           if AParent <> FRoot then
            begin
             Entry:=TNTFSDiskEntry(AParent).CreateDotDot;
             if Entry = nil then Exit;
             if not FEntries.Add(Entry,AParent) then Exit;
            end;

           {Load Links}
           while Key <> nil do
            begin
             {$IFDEF NTFS_DEBUG}
             if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadEntries - Key = ' + PtrToHex(Key));
             if FILESYS_LOG_ENABLED then if Key.Node <> nil then FileSysLogDebug('TNTFSFileSystem.LoadEntries - Node = ' + IntToHex(Key.Node.RecordNumber,16));
             {$ENDIF}

             if Key.Attribute <> nil then
              begin
               {Get Record}
               Current:=GetRecordEx(nil,Key.RecordNumber,False,True);
               if Current = nil then Exit;

               {Get Entry}
               if Current.GetLink(Key.Attribute) = nil then {This resolves the root self reference and avoids double loading of DOS and Win32 entries}
                begin
                case TNTFSFileNameAttribute(Key.Attribute).NameSpace of
                  ntfsNameSpacePosix:begin {HardLinks use Posix namespace}
                    {Get Attribute}
                    Attribute:=Key.Attribute;

                    {Load Entry}
                    if not LoadEntry(TNTFSDiskEntry(AParent),Current,Attribute,nil) then Exit;
                   end;
                  ntfsNameSpaceWin32:begin
                    {Get Attribute}
                    Attribute:=Key.Attribute;

                    {Get Alternate}
                    Instance:=1;
                    Alternate:=Current.GetFileNameByNameSpace(ntfsAnyName,TNTFSFileNameAttribute(Attribute).ParentReference,ntfsNameSpaceDos,Instance);
                    while Alternate <> nil do
                     begin
                      {Get Entry}
                      if Current.GetLink(Alternate) = nil then Break;

                      {Get Alternate}
                      Inc(Instance);
                      Alternate:=Current.GetFileNameByNameSpace(ntfsAnyName,TNTFSFileNameAttribute(Attribute).ParentReference,ntfsNameSpaceDos,Instance);
                     end;

                    {Load Entry}
                    if not LoadEntry(TNTFSDiskEntry(AParent),Current,Attribute,Alternate) then Exit;
                   end;
                  ntfsNameSpaceDos:begin
                    {Get Alternate}
                    Alternate:=Key.Attribute;

                    {Get Attribute}
                    Instance:=1;
                    Attribute:=Current.GetFileNameByNameSpace(ntfsAnyName,TNTFSFileNameAttribute(Alternate).ParentReference,ntfsNameSpaceWin32,Instance);
                    while Attribute <> nil do
                     begin
                      {Get Entry}
                      if Current.GetLink(Attribute) = nil then Break;

                      {Get Attribute}
                      Inc(Instance);
                      Attribute:=Current.GetFileNameByNameSpace(ntfsAnyName,TNTFSFileNameAttribute(Alternate).ParentReference,ntfsNameSpaceWin32,Instance);
                     end;

                    if Attribute = nil then //To Do //Testing
                     begin                  //To Do //Testing
                      {Check Attribute}
                      if Attribute = nil then Alternate:=nil;
                      if Attribute = nil then Attribute:=Key.Attribute;

                      {Load Entry}
                      if not LoadEntry(TNTFSDiskEntry(AParent),Current,Attribute,Alternate) then Exit;
                     end;                   //To Do //Testing
                   end;
                  ntfsNameSpaceBoth:begin
                    {Get Attribute}
                    Attribute:=Key.Attribute;

                    {Load Entry}
                    if not LoadEntry(TNTFSDiskEntry(AParent),Current,Attribute,nil) then Exit;
                   end;
                 end;
                end;
              end;

             {Get Key}
             Key:=TNTFSAttributeKey(Key.Next);
            end;
          end;
        end;

       {Check File or Directory}
       if (AParent.Attributes and (faFile or faDirectory)) <> faNone then
        begin
         {Load Streams} {Data streams only}
         Instance:=1;
         Attribute:=Origin.GetAttribute(ntfsAttrTypeData,ntfsAnyName,Instance);
         while Attribute <> nil do
          begin
           {Check Name} {Dont load default or multiple instances}
           if (Attribute.AttributeName <> ntfsBlankName) and (Attribute.StartVCN = 0) then
            begin
             {Load Entry} {Do not call GetStream as multiple Links may refer to the same streams}
             if not LoadEntry(TNTFSDiskEntry(AParent),Origin,Attribute,nil) then Exit;
            end;

           {Get Attribute}
           Inc(Instance);
           Attribute:=Origin.GetAttribute(ntfsAttrTypeData,ntfsAnyName,Instance);
          end;
        end;

       AParent.EntriesLoaded:=True;
      finally
       FRecords.WriterUnlock;
      end;
     end;
   finally
    FEntries.WriterUnlock;
   end;
  end;

 Result:=True;
end;

{=============================================================================}

function TNTFSFileSystem.LoadRecords(ARecord:TNTFSDiskRecord;ARoot:TNTFSDiskAttribute):Boolean;
{Load each of the records referenced by a file name index}
var
 Key:TNTFSAttributeKey;
 Index:TNTFSAttributeIndex;
 Current:TNTFSDiskRecord;
begin
 {}
 Result:=False;

 if not FRecords.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if ARoot = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadRecords - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' Attribute = ' + ARoot.AttributeName);
  {$ENDIF}

  {Check Type}
  if ARoot.AttributeType = ntfsAttrTypeIndexRoot then
   begin
    {Get Index}
    Index:=TNTFSAttributeIndex(ARoot.Index);
    if Index = nil then Exit;

    {Check Type}
    if Index.IndexType = ntfsAttrTypeFileName then
     begin
      {Get Key}
      Key:=TNTFSAttributeKey(Index.First);
      while Key <> nil do
       begin
        if Key.Attribute = nil then
         begin
          if Key.Key = nil then Exit;

          {Get Record}
          Current:=GetRecordEx(nil,Key.RecordNumber,False,True);
          if Current = nil then
           begin
            Key.Invalid:=True;
           end
          else
           begin
            {Check Reference}
            if Key.FileReference <> Current.FileReference then
             begin
              Key.Invalid:=True;
             end
            else
             begin
              {Get Attribute}
              Key.Attribute:=Current.GetFileNameByKey(Key);

              {Check Attribute}
              if Key.Attribute = nil then Key.Invalid:=True;
             end;
           end;
         end;

        {Get Key}
        Key:=TNTFSAttributeKey(Key.Next);
       end;

      Result:=True;
     end;
   end;
 finally
  FRecords.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.LoadLists(ARecord:TNTFSDiskRecord):Boolean;
{Load any attribute lists within a file record}
{Note: Caller must hold the records lock}
var
 Instance:Integer;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FRecords.AttributesWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadLists - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16));
  {$ENDIF}

  {Check Extension}
  if not ARecord.Extension then
   begin
    {Get List}
    Instance:=1;
    Attribute:=ARecord.GetAttribute(ntfsAttrTypeAttributeList,ntfsAnyName,Instance);
    while Attribute <> nil do
     begin
      {Load List}
      if not LoadList(ARecord,Attribute) then Exit;

      {Get List}
      Inc(Instance);
      Attribute:=ARecord.GetAttribute(ntfsAttrTypeAttributeList,ntfsAnyName,Instance);
     end;
   end;

  Result:=True;
 finally
  FRecords.AttributesWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.LoadIndexes(ARecord:TNTFSDiskRecord):Boolean;
{Load any indexes within a file record}
{Note: Caller must hold the records lock}
var
 Instance:Integer;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FRecords.IndexWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadIndexes - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16));
  {$ENDIF}

  {Check Extension}
  if not ARecord.Extension then
   begin
    {Get Index}
    Instance:=1;
    Attribute:=ARecord.GetAttribute(ntfsAttrTypeIndexRoot,ntfsAnyName,Instance);
    while Attribute <> nil do
     begin
      {Load Index}
      if not LoadIndex(ARecord,Attribute) then Exit;

      {Get Index}
      Inc(Instance);
      Attribute:=ARecord.GetAttribute(ntfsAttrTypeIndexRoot,ntfsAnyName,Instance);
     end;
   end;

  Result:=True;
 finally
  FRecords.IndexWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.LoadTable(ATableNo:LongWord):Boolean;
var
 Table:TNTFSDiskTable;
begin
 {}
 Result:=False;

 if not FTables.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ATableNo > ntfsTableTypeMftMirr then Exit;

  {Load Table}
  Table:=TNTFSDiskTable.Create(FTableLocal);
  Table.TableNo:=ATableNo;
  Table.StartCluster:=FMftStartCluster;
  Table.StartSector:=(FMftStartCluster shl FSectorShiftCount);
  Table.Entry:=FMft;
  if ATableNo = ntfsTableTypeMftMirr then
   begin
    Table.StartCluster:=FMftMirrorCluster;
    Table.StartSector:=(FMftMirrorCluster shl FSectorShiftCount);
    Table.Entry:=FMftMirr;
   end;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadTable - Table = ' + IntToHex(Table.TableNo,8) + ' Sector = ' + IntToStr(Table.StartSector) + ' Cluster = ' + IntToStr(Table.StartCluster));
  {$ENDIF}

  Result:=FTables.Add(Table);
 finally
  FTables.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.LoadBlock(ABlockNo:LongWord):Boolean;
{Note: Caller must hold the records lock}
var
 VCN:Int64;
 Count:Int64;
 Cluster:Int64;
 Buffer:Pointer;
 Instance:LongWord;

 Block:TNTFSDiskBlock;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FBlocks.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FBitmap = nil then Exit;
  if FBitmap.Origin = nil then Exit;
  if ABlockNo >= FTotalBlockCount then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadBlock - BlockNo = ' + IntToHex(ABlockNo,8));
  {$ENDIF}

  {Get Origin}
  Origin:=FBitmap.Origin.Origin;
  if Origin = nil then Exit;

  {Get Attribute}
  Attribute:=Origin.GetAttribute(ntfsAttrTypeData,ntfsBlankName,ntfsInstanceFirst);
  if Attribute = nil then Exit;

  {Get Cluster}
  VCN:=ABlockNo;
  Cluster:=(VCN shl FBlockShiftCount);
  Count:=(FTotalClusterCount - Cluster);
  if Count > FEntriesPerBlock then Count:=FEntriesPerBlock; {Min does not support Int64}

  {Allocate Buffer}
  Buffer:=GetMem((FClustersPerBlock shl FClusterShiftCount));
  if Buffer = nil then Exit;
  //To Do //Free Block on failure

  {Read Run}
  Instance:=ntfsInstanceFirst;
  if ReadRun(Origin,Attribute,Buffer^,VCN,FClustersPerBlock,Instance,False,True) = FClustersPerBlock then
   begin
    {Load Block}
    Block:=TNTFSDiskBlock.Create(FBlockLocal);
    Block.BlockNo:=ABlockNo;
    Block.BlockCount:=Count;
    Block.BlockBuffer:=Buffer;
    Block.BlockCluster:=Cluster;

    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadBlock - BlockNo = ' + IntToHex(Block.BlockNo,8) + ' BlockCluster = ' + IntToStr(Block.BlockCluster) + ' BlockCount = ' + IntToStr(Block.BlockCount));
    {$ENDIF}

    Result:=FBlocks.Add(Block);
   end;
 finally
  FBlocks.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.LoadMetafile(AFileNo:LongWord):Boolean;
{Root is loaded first which loads all other Metafiles via LoadEntries}
{For all other Metafiles the entry is located using GetRecord or GetEntry}
{Any additional setup for each Metafile is then performed}
var
 RecordNumber:Int64;
 Table:TNTFSDiskTable;
 ARecord:TNTFSDiskRecord;  //To Do //Current/Origin ?
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if not FRecords.WriterLock then Exit;
  try
   if FDriver = nil then Exit;
   if FNTFSType = ntNONE then Exit;

   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadMetafile - File = ' + IntToHex(AFileNo,8));
   {$ENDIF}

   {Load Metafile}
   case AFileNo of
    ntfsFileTypeMft:begin
      if FRoot = nil then Exit;
      if FMft <> nil then Exit;

      {Get Record}
      RecordNumber:=AFileNo;
      ARecord:=GetRecordEx(nil,RecordNumber,False,True);
      if ARecord = nil then Exit;
      ARecord.Metafile:=True;

      {Get Entry}
      FMft:=TNTFSDiskEntry(GetEntryEx(FRoot,ntfsFileNameMft,faFile,False,False,True));
      if FMft = nil then Exit;
      FMft.Attributes:=(FMft.Attributes or faFlagMetafile);

      {Get Table}
      Table:=TNTFSDiskTable(GetTable(ntfsTableTypeMft));
      if Table = nil then Exit;
      Table.Entry:=FMft;

      Result:=True;
     end;
    ntfsFileTypeMftMirr:begin
      if FRoot = nil then Exit;
      if FMft = nil then Exit;
      if FMftMirr <> nil then Exit;

      {Get Record}
      RecordNumber:=AFileNo;
      ARecord:=GetRecordEx(nil,RecordNumber,False,True);
      if ARecord = nil then Exit;
      ARecord.Metafile:=True;

      {Get Entry}
      FMftMirr:=TNTFSDiskEntry(GetEntryEx(FRoot,ntfsFileNameMftMirr,faFile,False,False,True));
      if FMftMirr = nil then Exit;
      FMftMirr.Attributes:=(FMftMirr.Attributes or faFlagMetafile);

      {Get Table}
      Table:=TNTFSDiskTable(GetTable(ntfsTableTypeMftMirr));
      if Table = nil then Exit;
      Table.Entry:=FMftMirr;

      Result:=True;
     end;
    ntfsFileTypeLogFile:begin
      if FRoot = nil then Exit;
      if FMft = nil then Exit;
      if FLogFile <> nil then Exit;

      {Get Record}
      RecordNumber:=AFileNo;
      ARecord:=GetRecordEx(nil,RecordNumber,False,True);
      if ARecord = nil then Exit;
      ARecord.Metafile:=True;

      {Get Entry}
      FLogFile:=TNTFSDiskEntry(GetEntryEx(FRoot,ntfsFileNameLogFile,faFile,False,False,True));
      if FLogFile = nil then Exit;
      FLogFile.Attributes:=(FLogFile.Attributes or faFlagMetafile);

      Result:=True;
     end;
    ntfsFileTypeVolume:begin
      if FRoot = nil then Exit;
      if FMft = nil then Exit;
      if FVolInfo <> nil then Exit;

      {Get Record}
      RecordNumber:=AFileNo;
      ARecord:=GetRecordEx(nil,RecordNumber,False,True);
      if ARecord = nil then Exit;
      ARecord.Metafile:=True;

      {Get Entry}
      FVolInfo:=TNTFSDiskEntry(GetEntryEx(FRoot,ntfsFileNameVolume,faFile,False,False,True));
      if FVolInfo = nil then Exit;
      FVolInfo.Attributes:=(FVolInfo.Attributes or faFlagMetafile);

      Result:=True;
     end;
    ntfsFileTypeAttrDef:begin
      if FRoot = nil then Exit;
      if FMft = nil then Exit;
      if FAttrDef <> nil then Exit;

      {Get Record}
      RecordNumber:=AFileNo;
      ARecord:=GetRecordEx(nil,RecordNumber,False,True);
      if ARecord = nil then Exit;
      ARecord.Metafile:=True;

      {Get Entry}
      FAttrDef:=TNTFSDiskEntry(GetEntryEx(FRoot,ntfsFileNameAttrDef,faFile,False,False,True));
      if FAttrDef = nil then Exit;
      FAttrDef.Attributes:=(FAttrDef.Attributes or faFlagMetafile);

      Result:=True;
     end;
    ntfsFileTypeRoot:begin
      {Root must be loaded first}
      if FRoot <> nil then Exit;

      {Get Mft} {Allows for extensions of Root}
      RecordNumber:=ntfsFileTypeMft;
      ARecord:=GetRecordEx(nil,RecordNumber,False,True);
      if ARecord = nil then Exit;

      {Get Record}
      RecordNumber:=AFileNo;
      ARecord:=GetRecordEx(nil,RecordNumber,False,True);
      if ARecord = nil then Exit;
      ARecord.Metafile:=True;

      {Get Attribute}
      Attribute:=ARecord.GetAttribute(ntfsAttrTypeFileName,ntfsAnyName,ntfsInstanceFirst);
      if Attribute = nil then Exit;

      {Load Entry}
      if not LoadEntry(nil,ARecord,Attribute,nil) then Exit;
      FRoot.Attributes:=(FRoot.Attributes or faFlagMetafile);

      {Set Defaults}
      FRoot.Name:=FRootName;
      FRoot.AltName:=FRootPath;

      {Load Entries}
      if not LoadEntries(FRoot) then Exit;

      Result:=True;
     end;
    ntfsFileTypeBitmap:begin
      if FRoot = nil then Exit;
      if FMft = nil then Exit;
      if FBitmap <> nil then Exit;

      {Get Record}
      RecordNumber:=AFileNo;
      ARecord:=GetRecordEx(nil,RecordNumber,False,True);
      if ARecord = nil then Exit;
      ARecord.Metafile:=True;

      {Get Entry}
      FBitmap:=TNTFSDiskEntry(GetEntryEx(FRoot,ntfsFileNameBitmap,faFile,False,False,True));
      if FBitmap = nil then Exit;
      FBitmap.Attributes:=(FBitmap.Attributes or faFlagMetafile);

      Result:=True;
     end;
    ntfsFileTypeBoot:begin
      if FRoot = nil then Exit;
      if FMft = nil then Exit;
      if FBoot <> nil then Exit;

      {Get Record}
      RecordNumber:=AFileNo;
      ARecord:=GetRecordEx(nil,RecordNumber,False,True);
      if ARecord = nil then Exit;
      ARecord.Metafile:=True;

      {Get Entry}
      FBoot:=TNTFSDiskEntry(GetEntryEx(FRoot,ntfsFileNameBoot,faFile,False,False,True));
      if FBoot = nil then Exit;
      FBoot.Attributes:=(FBoot.Attributes or faFlagMetafile);

      Result:=True;
     end;
    ntfsFileTypeBadClus:begin
      if FRoot = nil then Exit;
      if FMft = nil then Exit;
      if FBadClus <> nil then Exit;

      {Get Record}
      RecordNumber:=AFileNo;
      ARecord:=GetRecordEx(nil,RecordNumber,False,True);
      if ARecord = nil then Exit;
      ARecord.Metafile:=True;

      {Get Entry}
      FBadClus:=TNTFSDiskEntry(GetEntryEx(FRoot,ntfsFileNameBadClus,faFile,False,False,True));
      if FBadClus = nil then Exit;
      FBadClus.Attributes:=(FBadClus.Attributes or faFlagMetafile);

      Result:=True;
     end;
    ntfsFileTypeSecure:begin    {Also ntfs12FileTypeQuota}
      if FRoot = nil then Exit;
      if FMft = nil then Exit;
      if FNTFSType = ntNTFS12 then
       begin
        if FQuota <> nil then Exit;

        {Get Record}
        RecordNumber:=AFileNo;
        ARecord:=GetRecordEx(nil,RecordNumber,False,True);
        if ARecord = nil then Exit;
        ARecord.Metafile:=True;

        {Get Entry}
        FQuota:=TNTFSDiskEntry(GetEntryEx(FRoot,ntfsFileNameQuota,faFile,False,False,True));
        if FQuota = nil then Exit;
        FQuota.Attributes:=(FQuota.Attributes or faFlagMetafile);

        Result:=True;
       end
      else
       begin
        if FSecure <> nil then Exit;

        {Get Record}
        RecordNumber:=AFileNo;
        ARecord:=GetRecordEx(nil,RecordNumber,False,True);
        if ARecord = nil then Exit;
        ARecord.Metafile:=True;

        {Get Entry}
        FSecure:=TNTFSDiskEntry(GetEntryEx(FRoot,ntfsFileNameSecure,faFile,False,False,True));
        if FSecure = nil then Exit;
        FSecure.Attributes:=(FSecure.Attributes or faFlagMetafile);

        Result:=True;
       end;
     end;
    ntfsFileTypeUpCase:begin
      if FRoot = nil then Exit;
      if FMft = nil then Exit;
      if FUpCase <> nil then Exit;

      {Get Record}
      RecordNumber:=AFileNo;
      ARecord:=GetRecordEx(nil,RecordNumber,False,True);
      if ARecord = nil then Exit;
      ARecord.Metafile:=True;

      {Get Entry}
      FUpCase:=TNTFSDiskEntry(GetEntryEx(FRoot,ntfsFileNameUpCase,faFile,False,False,True));
      if FUpCase = nil then Exit;
      FUpCase.Attributes:=(FUpCase.Attributes or faFlagMetafile);

      Result:=True;
     end;
    ntfsFileTypeExtend:begin
      if FRoot = nil then Exit;
      if FMft = nil then Exit;
      if FExtend <> nil then Exit;

      {Get Record}
      RecordNumber:=AFileNo;
      ARecord:=GetRecordEx(nil,RecordNumber,False,True);
      if ARecord = nil then Exit;
      ARecord.Metafile:=True;

      {Get Entry}
      FExtend:=TNTFSDiskEntry(GetEntryEx(FRoot,ntfsFileNameExtend,faDirectory,False,False,True));
      if FExtend = nil then Exit;
      FExtend.Attributes:=(FExtend.Attributes or faFlagMetafile);

      {Load Entries}
      if not LoadEntries(FExtend) then Exit;

      Result:=True;
     end;
    {Metafiles within $Extend}
    ntfsFileTypeObjId:begin
      if FMft = nil then Exit;
      if FExtend = nil then Exit;
      if FObjId <> nil then Exit;

      {Get Entry}
      FObjId:=TNTFSDiskEntry(GetEntryEx(FExtend,ntfsFileNameObjId,faFile,False,False,True));
      if FObjId = nil then Exit;
      FObjId.Attributes:=(FObjId.Attributes or faFlagMetafile);

      {Get Record}
      ARecord:=FObjId.Origin;
      if ARecord = nil then Exit;
      ARecord.Metafile:=True;

      Result:=True;
     end;
    ntfsFileTypeQuota:begin
      if FMft = nil then Exit;
      if FExtend = nil then Exit;
      if FQuota <> nil then Exit;

      {Get Entry}
      FQuota:=TNTFSDiskEntry(GetEntryEx(FExtend,ntfsFileNameQuota,faFile,False,False,True));
      if FQuota = nil then Exit;
      FQuota.Attributes:=(FQuota.Attributes or faFlagMetafile);

      {Get Record}
      ARecord:=FQuota.Origin;
      if ARecord = nil then Exit;
      ARecord.Metafile:=True;

      Result:=True;
     end;
    ntfsFileTypeReparse:begin
      if FMft = nil then Exit;
      if FExtend = nil then Exit;
      if FReparse <> nil then Exit;

      {Get Entry}
      FReparse:=TNTFSDiskEntry(GetEntryEx(FExtend,ntfsFileNameReparse,faFile,False,False,True));
      if FReparse = nil then Exit;
      FReparse.Attributes:=(FReparse.Attributes or faFlagMetafile);

      {Get Record}
      ARecord:=FReparse.Origin;
      if ARecord = nil then Exit;
      ARecord.Metafile:=True;

      Result:=True;
     end;
    ntfsFileTypeUsnJrnl:begin
      if FMft = nil then Exit;
      if FExtend = nil then Exit;
      if FUsnJrnl <> nil then Exit;

      {Get Entry}
      FUsnJrnl:=TNTFSDiskEntry(GetEntryEx(FExtend,ntfsFileNameUsnJrnl,faFile,False,False,True));
      if FUsnJrnl = nil then Exit;
      FUsnJrnl.Attributes:=(FUsnJrnl.Attributes or faFlagMetafile);

      {Get Record}
      ARecord:=FUsnJrnl.Origin;
      if ARecord = nil then Exit;
      ARecord.Metafile:=True;

      Result:=True;
     end;
   end;
  finally
   FRecords.WriterUnlock;
  end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.LoadEntry(AParent:TNTFSDiskEntry;ARecord:TNTFSDiskRecord;AAttribute,AAlternate:TNTFSDiskAttribute):Boolean;
{Note: Should only be called by LoadEntries or LoadMetafile}
var
 Entry:TNTFSDiskEntry;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if not FRecords.WriterLock then Exit;
  try
   if FDriver = nil then Exit;
   if ARecord = nil then Exit;
   if AAttribute = nil then Exit;

   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadEntry - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16));
   {$ENDIF}

   {Check Type}
   case AAttribute.AttributeType of
    ntfsAttrTypeData:begin
      {$IFDEF NTFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadEntry - ntfsAttrTypeData');
      {$ENDIF}

      if AParent = nil then Exit;
      if AAlternate <> nil then Exit;

      {Create Stream}
      Entry:=ARecord.CreateStream(AAttribute,False);
      if Entry = nil then Exit;
      try
       {Update Entry}
       Entry.LocalLock:=FEntryLocal;
       if not Entry.UpdateEntry then Exit;

       {Add Entry}
       Result:=FEntries.Add(Entry,AParent);

       {$IFDEF NTFS_DEBUG}
       if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadEntry - Entry = ' + Entry.Name + ' Alternate = ' + Entry.AltName);
       {$ENDIF}
      finally
       if not Result then ARecord.DestroyStream(Entry);
      end;
     end;
    ntfsAttrTypeFileName:begin
      {$IFDEF NTFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadEntry - ntfsAttrTypeFileName');
      {$ENDIF}

      {Create Link} {Parent may be nil}
      Entry:=ARecord.CreateLink(AAttribute,AAlternate,False);
      if Entry = nil then Exit;
      try
       {Update Entry}
       Entry.LocalLock:=FEntryLocal;
       if not Entry.UpdateEntry then Exit;

       {Add Entry}
       Result:=FEntries.Add(Entry,AParent);

       {Check Root}
       if Result then if AParent = nil then FRoot:=Entry;
       if FRoot = Entry then FRoot.AddReference; {Prevent Deletion}

       {$IFDEF NTFS_DEBUG}
       if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadEntry - Entry = ' + Entry.Name + ' Alternate = ' + Entry.AltName);
       {$ENDIF}
      finally
       if not Result then ARecord.DestroyLink(Entry);
      end;
     end;
   end;
  finally
   FRecords.WriterUnlock;
  end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.LoadRecord(ABase:TNTFSDiskRecord;const ARecordNumber:Int64;AFree:Boolean):Boolean;
{If Free is specified then allow loading of a record which is blank apart from the USN array}
{Caller should try loading without the Free parameter first (only used by AllocFileRecord)}
var
 VCN:Int64;
 Size:LongWord;
 Start:LongWord;
 Offset:LongWord;
 ARecord:TNTFSDiskRecord;  //To Do //Current/Origin ?
begin
 {}
 Result:=False;

 if not FRecords.WriterLock then Exit;
 try
  if FDriver = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadRecord - RecordNumber = ' + IntToHex(ARecordNumber,16) + ' Free = ' + BoolToStr(AFree));
  {$ENDIF}

  {Get VCN}
  if not GetFileRecordVCN(ARecordNumber,VCN,Start) then Exit;

  if not FileLock then Exit;
  try
   if FFileBuffer = nil then Exit;

   {Read Run}
   Size:=Max(FClustersPerFile,1);
   if not ReadFileRecord(ARecordNumber,FFileBuffer^,VCN,Size) then Exit;

   {Create Record}
   ARecord:=FRecords.CreateRecord(ABase,ARecordNumber,FVolumeVersion);
   if ARecord = nil then Exit;
   try
    {Read Record}
    Offset:=Start;
    Size:=FFileRecordSize;
    if not ARecord.ReadRecord(FFileBuffer,Offset,Size,FVolumeVersion,AFree) then Exit;

    {Check Free}
    if AFree then
     begin
      ARecord.RecordAllocated:=FFileRecordSize;
      ARecord.UpdateSequenceLength:=ARecord.CalculatedSequenceLength(ntfsUpdateSequenceSize); {Previously FSectorSize however always 512}

      {$IFDEF NTFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadRecord (Free) - UpdateSequenceNumber = ' + IntToStr(ARecord.UpdateSequenceNumber) + ' UpdateSequenceLength = ' + IntToStr(ARecord.UpdateSequenceLength));
      {$ENDIF}
     end;

    {Read Fixup}
    if not ReadFixup(FFileBuffer,Start,ARecord.UpdateSequenceNumber,ARecord.UpdateSequenceOffset,ARecord.UpdateSequenceLength,AFree) then Exit;

    {Read Attributes}
    if not ARecord.ReadAttributes(FFileBuffer,Offset,Size,FVolumeVersion) then Exit;

    {Check Mirrored, Reserved, Expansion and Extension}
    if ABase <> nil then ARecord.Extension:=True;
    if ARecord.RecordNumber < FMftMirrorCount then ARecord.Mirrored:=True;
    if (ARecord.RecordNumber >= ntfsFileTypeReserved1) and (ARecord.RecordNumber <= ntfsFileTypeReserved4) then ARecord.Reserved:=True;
    if (ARecord.RecordNumber >= ntfsFileTypeExpansion1) and (ARecord.RecordNumber <= ntfsFileTypeExpansion8) then ARecord.Expansion:=True;

    {Add Record}
    if not FRecords.InsertRecord(ARecord) then Exit;
    try
     {Load Lists}
     if not LoadLists(ARecord) then Exit;

     {Load Indexes}
     if not LoadIndexes(ARecord) then Exit;

     {Check Master}
     if ARecordNumber = ntfsFileTypeMft then FMaster:=ARecord;

     {Check Mirror}
     if ARecordNumber = ntfsFileTypeMftMirr then FMirror:=ARecord;

     {$IFDEF NTFS_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadRecord - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' FileReference = ' + IntToHex(ARecord.FileReference,16));
     {$ENDIF}

     Result:=True;
    finally
     if not Result then FRecords.DeleteRecord(ARecord); {Does not free record}
    end;
   finally
    if not Result then FRecords.DestroyRecord(ARecord);
   end;
  finally
   FileUnlock;
  end;
 finally
  FRecords.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.LoadList(ARecord:TNTFSDiskRecord;AList:TNTFSDiskAttribute):Boolean;
var
 Size:LongWord;
 Buffer:Pointer;
 Offset:LongWord;
 Instance:LongWord;

 Item:TNTFSDiskItem;
 Items:TNTFSDiskItems;
 Current:TNTFSDiskRecord;
begin
 {}
 Result:=False;

 if not FRecords.AttributesWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if AList = nil  then Exit;

  {Get Items}
  Items:=AList.Items;
  if Items = nil then Exit;

  {Check Loaded}
  if not Items.Loaded then
   begin
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadList - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16));
    {$ENDIF}

    {Setup Record}
    ARecord.Overflow:=True;

    {Check Resident}
    if AList.NonResident = ntfsAttributeNonResident then
     begin
      {Check Size}
      if AList.StreamSize > 0 then
       begin
        Buffer:=GetMem(AList.StreamSize);
        if Buffer = nil then Exit;
        try
         {Read Attribute}
         Instance:=ntfsInstanceFirst;
         if ReadAttribute(ARecord,AList,Buffer^,0,AList.StreamSize,Instance,True) <> AList.StreamSize then Exit;

         {Get Offset}
         Offset:=0;
         Size:=AList.StreamSize;

         {Read Items}
         if not AList.ReadItems(Buffer,Offset,Size,FVolumeVersion) then Exit;
        finally
         FreeMem(Buffer);
        end;
       end;
     end;

    {Load Items}
    Item:=TNTFSDiskItem(AList.Items.First);
    while Item <> nil do
     begin
      if Item.Attribute = nil then
       begin
        Current:=GetRecordEx(ARecord,Item.RecordNumber,False,True);
        if Current = nil then
         begin
          Item.Invalid:=True;
         end
        else
         begin
          {Check Reference}
          if Item.FileReference <> Current.FileReference then
           begin
            Item.Invalid:=True;
           end
          else
           begin
            {Set Base} {If not base record (Also done by LoadRecord)}
            if Current <> ARecord then Current.Extension:=True;
            if Current <> ARecord then Current.Base:=ARecord;

            {Get Attribute}
            Item.Attribute:=Current.GetAttributeByItem(Item);

            {Check Attribute}
            if Item.Attribute = nil then Item.Invalid:=True;
           end;
         end;
       end;

      Item:=TNTFSDiskItem(Item.Next);
     end;

    Items.Loaded:=True;
   end;

  Result:=True;
 finally
  FRecords.AttributesWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.LoadIndex(ARecord:TNTFSDiskRecord;ARoot:TNTFSDiskAttribute):Boolean;
{Note: Caller must hold the records lock}
var
 Key:TNTFSDiskKey;
 Node:TNTFSDiskNode;
 Index:TNTFSDiskIndex;
 Allocation:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FRecords.IndexWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if ARoot = nil  then Exit;

  {Get Index}
  Index:=ARoot.Index;
  if Index = nil then Exit;

  {Check Loaded}
  if not Index.Loaded then
   begin
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadIndex - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' Attribute = ' + ARoot.AttributeName);
    {$ENDIF}

    {Setup Index}
    if (Index.IndexRecordSize = FIndexRecordSize) and (Index.IndexCounterOffset = FIndexCounterOffset) then
     begin
      Index.SectorSize:=FSectorSize;
      Index.ClustersPerIndex:=FClustersPerIndex;
      Index.IndexsPerCluster:=FIndexsPerCluster;
      Index.IndexCounterShift:=FIndexCounterShift;
      Index.IndexRecordShiftCount:=FIndexRecordShiftCount;
      Index.IndexRecordOffsetMask:=FIndexRecordOffsetMask;
     end
    else
     begin
      if FClusterSize = 0 then Exit;
      if Index.IndexRecordSize = 0 then Exit;
      Index.SectorSize:=FSectorSize;
      Index.ClustersPerIndex:=Index.IndexRecordSize div FClusterSize;
      Index.IndexsPerCluster:=FClusterSize div Index.IndexRecordSize;
      Index.IndexCounterShift:=GetIndexCounterShift(Index.IndexCounterOffset);
      Index.IndexRecordShiftCount:=GetIndexRecordShiftCount(FClusterSize,Index.IndexRecordSize);
      Index.IndexRecordOffsetMask:=GetIndexRecordOffsetMask(Index.IndexsPerCluster);

      {$IFDEF NTFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadIndex - IndexRecordSize = ' + IntToStr(Index.IndexRecordSize));
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadIndex - ClustersPerIndex = ' + IntToStr(Index.ClustersPerIndex));
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadIndex - IndexsPerCluster = ' + IntToStr(Index.IndexsPerCluster));
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadIndex - IndexCounterShift = ' + IntToStr(Index.IndexCounterShift));
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadIndex - IndexCounterOffset = ' + IntToStr(Index.IndexCounterOffset));
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadIndex - IndexRecordShiftCount = ' + IntToStr(Index.IndexRecordShiftCount));
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadIndex - IndexRecordOffsetMask = ' + IntToStr(Index.IndexRecordOffsetMask));
      {$ENDIF}
     end;

    {Update Index}
    Index.UpCase:=FUpCases;
    Index.CompareSecurityDescriptor:=CompareSecurityDescriptor;

    {Get Key}
    Key:=TNTFSDiskKey(Index.Root);
    if Key = nil then Exit;

    {Get Node}
    Node:=Key.Node;
    if Node = nil then Exit;

    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadIndex - KeyCount = ' + IntToStr(Node.KeyCount));
    {$ENDIF}

    {Check Node}
    if Node.HasSubNodes then
     begin
      {Get Allocation}
      Allocation:=ARecord.GetAttribute(ntfsAttrTypeIndexAllocation,ARoot.AttributeName,ntfsInstanceFirst);
      if Allocation = nil then Exit;

      {Load Index}
      while Key <> nil do
       begin
        {Check Key}
        if Key.HasSubNode then
         begin
          {Load Node}
          if not LoadNode(ARecord,Allocation,Index,Key) then Exit;
         end;

        {Get Key}
        Key:=TNTFSDiskKey(Key.Right);
       end;
     end;

    Index.Loaded:=True;
   end;

  Result:=True;
 finally
  FRecords.IndexWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.LoadNode(ARecord:TNTFSDiskRecord;AAllocation:TNTFSDiskAttribute;AIndex:TNTFSDiskIndex;AParent:TNTFSDiskKey):Boolean;
{Note: Caller must hold the records and index lock}
var
 VCN:Int64;
 Size:LongWord;
 Start:LongWord;
 Offset:LongWord;
 Instance:LongWord;

 Key:TNTFSDiskKey;
 Node:TNTFSDiskNode;
begin
 {}
 Result:=False;

 if not FRecords.NodesWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if AAllocation = nil then Exit;
  if AIndex = nil then Exit;
  if AParent = nil then Exit;
  if FIndexBuffer = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadNode - SubNodeNumber = ' + IntToHex(AParent.SubNodeNumber,16));
  {$ENDIF}

  {Get VCN}
  if not GetIndexRecordVCN(AIndex,AParent.SubNodeNumber,VCN,Start) then Exit;

  {Read Run}
  Instance:=ntfsInstanceFirst;
  Size:=Max(AIndex.ClustersPerIndex,1);
  if ReadRun(ARecord,AAllocation,FIndexBuffer^,VCN,Size,Instance,False,True) <> Size then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadNode - IndexRecordSize = ' + IntToStr(AIndex.IndexRecordSize) + ' ClustersPerIndex = ' + IntToStr(AIndex.ClustersPerIndex) + ' Read = ' + IntToStr(Size));
  {$ENDIF}

  {Create Node}
  Node:=AIndex.CreateNode(False);
  if Node = nil then Exit;
  try
   {Read Node}
   Offset:=Start;
   Size:=AIndex.IndexRecordSize;
   if not Node.ReadRecord(FIndexBuffer,Offset,Size,FVolumeVersion) then Exit;

   {Read Fixup}
   if not ReadFixup(FIndexBuffer,Start,Node.UpdateSequenceNumber,Node.UpdateSequenceOffset,Node.UpdateSequenceLength,False) then Exit;

   {Read Keys}
   if not AIndex.ReadKeys(AParent,Node,FIndexBuffer,Offset,Size,FVolumeVersion) then Exit;

   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.LoadNode - KeyCount = ' + IntToStr(Node.KeyCount));
   {$ENDIF}

   {Check Node}
   if Node.HasSubNodes then
    begin
     {Get Key}
     Key:=Node.Start;
     while Key <> nil do
      begin
       {Check Key}
       if Key.HasSubNode then
        begin
         {Load Node}
         if not LoadNode(ARecord,AAllocation,AIndex,Key) then Exit;
        end;

       {Get Key}
       Key:=TNTFSDiskKey(Key.Right);
      end;
    end;

   Result:=True;
  finally
   if not Result then AIndex.DestroyNode(Node);
  end;
 finally
  FRecords.NodesWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.UnloadEntries(AParent:TDiskEntry):Boolean;
{Unload each of the entries within the parent entry}
begin
 {}
 Result:=False;

 if FDriver = nil then Exit;
 if AParent = nil then Exit;

 {Check Loaded}
 if AParent.EntriesLoaded then
  begin
   if not FEntries.WriterLock then Exit;
   try
    {Check Loaded (After Lock)}
    if AParent.EntriesLoaded then
     begin
      {$IFDEF NTFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.UnloadEntries - Parent = ' + AParent.Name);
      {$ENDIF}

      {Get Origin}
      if not FRecords.WriterLock then Exit;
      try

       //To Do

       AParent.EntriesLoaded:=False;
      finally
       FRecords.WriterUnlock;
      end;
     end;
   finally
    FEntries.WriterUnlock;
   end;
  end;

 Result:=True;
end;

{=============================================================================}

function TNTFSFileSystem.AddEntry(AParent:TDiskEntry;const AName:String;AAttributes:LongWord;AReference:Boolean):TDiskEntry;
{Reference: Add a reference on the returned entry if True}
var
 Name:String;
 AltName:String;
 NameSpace:Byte;
 SecurityId:LongWord;

 Link:TNTFSDiskEntry;
 Entry:TNTFSDiskEntry;
 Stream:TNTFSDiskEntry;

 Descriptor:Pointer;
 Security:TNTFSSecurity;

 Origin:TNTFSDiskRecord;
 Current:TNTFSDiskRecord;

 Root:TNTFSDiskAttribute;
 Attribute:TNTFSDiskAttribute;
 Alternate:TNTFSDiskAttribute;
begin
 {}
 Result:=nil;

 if not FEntries.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if AParent = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AddEntry - Parent = ' + AParent.Name + ' Name = ' + AName);
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Relative}
  if (AParent.Attributes and (faDot or faDotDot)) <> faNone then Exit;

  {Check Reparse}
  if (AParent.Attributes and faReparse) <> faNone then Exit;

  {Check Stream}
  if (AAttributes and faMatchMask) = faStream then
   begin
    {Check Parent}
    if (AParent.Attributes and faMatchMask) = faStream then Exit;

    {Check Existing}
    Result:=GetEntryEx(AParent,AName,faStream,AReference,False,True);
    if Result <> nil then Exit;

    {Get Origin}
    if not FRecords.WriterLock then Exit;
    try
     if TNTFSDiskEntry(AParent).Origin = nil then Exit;
     Origin:=TNTFSDiskEntry(AParent).Origin.Origin;
     if Origin = nil then Exit;

     {Get Name}
     Name:=NTFSStreamNameToAttributeName(ntfsAttrTypeData,AName);
     if Length(Name) = 0 then Exit;

     {Check Name}
     if not CheckAttributeName(Name) then Exit;

     {Add Attribute}
     Attribute:=AddAttribute(Origin,ntfsAttrTypeData,Name);
     if Attribute = nil then Exit;
     //To Do //try finally /remove attribute on failure

     {Add Stream}
     Stream:=Origin.NewStream(Attribute);
     if Stream = nil then Exit;
     try
      {Update Entry}
      Stream.LocalLock:=FEntryLocal;
      if not Stream.UpdateEntry then Exit;

      {Add Entry}
      if not FEntries.Add(Stream,AParent) then Exit;
      try
       {Add Streams} {Streams belonging to other Links}
       if Origin.Links <> nil then
        begin
         Link:=Origin.Links.FirstEntry;
         while Link <> nil do
          begin
           if Link <> AParent then
            begin
             {Add Stream}
             Entry:=Origin.NewStream(Attribute);
             if Entry = nil then Exit;

             {Update Entry}
             Entry.LocalLock:=FEntryLocal;
             if not Entry.UpdateEntry then Exit;

             {Add Entry}
             if not FEntries.Add(Entry,Link) then Exit;
            end;

           Link:=Link.NextEntry;
          end;
        end;

       {Set Records}
       if not SetRecords(Origin) then Exit;

       Result:=Stream;

       {Add Reference}
       if AReference then Result.AddReference;
      finally
       if Result = nil then FEntries.Remove(Stream);
      end;
     finally
      if Result = nil then Origin.DeleteStream(Stream);
      if Result = nil then Stream.Free;
     end;
     //To Do //Remove Streams belonging to other Links on failure
     //To Do //Remove Attribute on failure
    finally
     FRecords.WriterUnlock;
    end;
   end
  else
   begin
    {Check Parent}
    if (AParent.Attributes and faMatchMask) <> faDirectory then Exit;

    {Check Attribtues (Include Folder/File)}
    if (AAttributes and (faDirectory or faFile)) <> faNone then
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

    {Check Short}
    if IsShort(AName) then
     begin
      {Get AltName}
      AltName:=ntfsBlankName;

      {Get NameSpace}
      NameSpace:=ntfsNameSpaceBoth;
     end
    else
     begin
      {Check Supported}
      if not FLongNames then Exit;
      {Check Short Names}
      if FNoShortNames then
       begin
        {Get AltName}
        AltName:=ntfsBlankName;

        {Get NameSpace}
        NameSpace:=ntfsNameSpacePosix; {Always Posix namespace if no short name}
       end
      else
       begin
        {Get AltName}
        AltName:=GenerateName(AParent,nil,AName);
        if Length(AltName) = 0 then Exit;

        {Get NameSpace}
        NameSpace:=ntfsNameSpaceWin32;
       end;
     end;

    {Get Parent}
    if not FRecords.WriterLock then Exit;
    try
     if TNTFSDiskEntry(AParent).Origin = nil then Exit;
     Current:=TNTFSDiskEntry(AParent).Origin.Origin;
     if Current = nil then Exit;

     {Add Record}
     Origin:=AddRecord(nil,(AAttributes and faMatchMask) = faDirectory);
     if Origin = nil then Exit;
     try
      {Add Attribute}
      Attribute:=AddAttribute(Origin,ntfsAttrTypeFileName,ntfsBlankName);
      if Attribute = nil then Exit;

      {Update Attribute}
      TNTFSFileNameAttribute(Attribute).ParentReference:=Current.FileReference;
      TNTFSFileNameAttribute(Attribute).NameSpace:=NameSpace;
      TNTFSFileNameAttribute(Attribute).FileName:=AName;
      Attribute.Indexed:=ntfsAttributeIndexed;
      Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
      Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

      {Size Record}
      if not SizeRecord(Origin,Origin.CalculatedSize(FVolumeVersion)) then Exit;

      {Check Alt Name}
      Alternate:=nil;
      if Length(AltName) <> 0 then
        begin
        {Add Alternate}
        Alternate:=AddAttribute(Origin,ntfsAttrTypeFileName,ntfsBlankName);
        if Alternate = nil then Exit;

        {Update Alternate}
        TNTFSFileNameAttribute(Alternate).ParentReference:=Current.FileReference;
        TNTFSFileNameAttribute(Alternate).NameSpace:=ntfsNameSpaceDos;
        TNTFSFileNameAttribute(Alternate).FileName:=AltName;
        Alternate.Indexed:=ntfsAttributeIndexed;
        Alternate.DataSize:=Alternate.CalculatedStreamSize(FVolumeVersion);
        Alternate.AttributeSize:=Alternate.CalculatedSize(FVolumeVersion);

        {Size Record}
        if not SizeRecord(Origin,Origin.CalculatedSize(FVolumeVersion)) then Exit;
       end;

      {Add Link}
      Link:=Origin.NewLink(Attribute,Alternate);
      if Link = nil then Exit;
      try
       {Update Entry}
       Link.LocalLock:=FEntryLocal;
       if not Link.UpdateEntry then Exit;

       {Update Attribute}
       if not Attribute.UpdateAttribute(Link) then Exit;

       {Update Alternate}
       if Alternate <> nil then if not Alternate.UpdateAttribute(Link) then Exit;

       {Add Key}
       if not AddKey(Current,Attribute) then Exit;

       {Add Key}
       if Alternate <> nil then if not AddKey(Current,Alternate) then Exit;

       {Get Root}
       Root:=Current.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameFileName,ntfsInstanceFirst);
       if Root = nil then Exit;

       {Set Index}
       if not SetAttribute(Current,Root) then Exit;

       {Add Entry}
       if not FEntries.Add(Link,AParent) then Exit;
       try
        {Check Security}
        if not FNullSecurity then
         begin
          if FDefaultSecurity then
           begin
            {Default Security}
            {Check Version}
            case FVolumeVersion of
             ntfsNTFS12:begin
               {Get Attribute}
               Attribute:=Origin.GetAttribute(ntfsAttrTypeSecurityDescriptor,ntfsBlankName,ntfsInstanceFirst);
               if Attribute = nil then
                begin
                 {Add Attribute}
                 Attribute:=Origin.NewAttribute(nil,ntfsAttrTypeSecurityDescriptor,ntfsBlankName,FVolumeVersion);
                 if Attribute = nil then Exit;

                 {Get Descriptor}
                 Descriptor:=nil;
                 if (AAttributes and faMatchMask) = faDirectory then
                  begin
                   if not NTFSCreateDefaultDescriptor(ntfsDefaultDescriptorFolder,Descriptor,FVolumeVersion) then Exit;
                  end
                 else
                  begin
                   if not NTFSCreateDefaultDescriptor(ntfsDefaultDescriptorFile,Descriptor,FVolumeVersion) then Exit;
                  end;
                 try
                  {Add Security}
                  Security:=TNTFSSecurity.CreateFromDescriptor(FSecurityLocal,Descriptor);
                  if Security = nil then Exit;
                  if not TNTFSSecurityDescriptorAttribute(Attribute).NewSecurity(Security) then Exit;

                  {Update Attribute}
                  Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
                  Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

                  {Add Item}
                  if not AddItem(Origin,Attribute) then Exit;

                  {Size Record}
                  if not SizeRecord(Origin,Origin.CalculatedSize(FVolumeVersion)) then Exit;
                 finally
                  NTFSDestroyDefaultDescriptor(Descriptor,FVolumeVersion);
                 end;
                end;
              end;
             ntfsNTFS30,ntfsNTFS31:begin
               {Get Attribute}
               Attribute:=Origin.GetAttribute(ntfsAttrTypeStandardInformation,ntfsBlankName,ntfsInstanceFirst);
               if Attribute <> nil then
                begin
                 {Get Descriptor}
                 Descriptor:=nil;
                 if (AAttributes and faMatchMask) = faDirectory then
                  begin
                   if not NTFSCreateDefaultDescriptor(ntfsDefaultDescriptorFolder,Descriptor,FVolumeVersion) then Exit;
                  end
                 else
                  begin
                   if not NTFSCreateDefaultDescriptor(ntfsDefaultDescriptorFile,Descriptor,FVolumeVersion) then Exit;
                  end;
                 try
                  {Get Security Id}
                  SecurityId:=GetDescriptorId(Descriptor,True);
                  if SecurityId <> ntfsSecurityIdUnknown then
                   begin
                    {Set Security}
                    TNTFSStandardInformationAttribute(Attribute).SecurityId:=SecurityId;
                   end
                  else
                   begin
                    {Get Security}
                    Security:=TNTFSSecurity.CreateFromDescriptor(FSecurityLocal,Descriptor);
                    if Security = nil then Exit;
                    try
                     {Add Security}
                     SecurityId:=AddSecurity(Security);
                     if SecurityId = ntfsSecurityIdUnknown then Exit;

                     {Set Security}
                     TNTFSStandardInformationAttribute(Attribute).SecurityId:=SecurityId;
                    finally
                     if SecurityId = ntfsSecurityIdUnknown then Security.Free;
                    end;
                   end;
                 finally
                  NTFSDestroyDefaultDescriptor(Descriptor,FVolumeVersion);
                 end;
                end;
              end;
            end;
           end
          else
           begin
            {Standard Security}
            {Check Version}
            case FVolumeVersion of
             ntfsNTFS12:begin
               {Get Attribute}
               Attribute:=Origin.GetAttribute(ntfsAttrTypeSecurityDescriptor,ntfsBlankName,ntfsInstanceFirst);
               if Attribute = nil then
                begin
                 {Get Security}
                 Security:=TNTFSSecurity(GetSecurityEx(AParent,True,SecurityId,True));
                 if Security = nil then Exit;
                 try
                  {Add Attribute}
                  Attribute:=Origin.NewAttribute(nil,ntfsAttrTypeSecurityDescriptor,ntfsBlankName,FVolumeVersion);
                  if Attribute = nil then Exit;

                  {Add Security}
                  if not TNTFSSecurityDescriptorAttribute(Attribute).NewSecurity(Security) then Exit;

                  {Update Attribute}
                  Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
                  Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

                  {Add Item}
                  if not AddItem(Origin,Attribute) then Exit;

                  {Size Record}
                  if not SizeRecord(Origin,Origin.CalculatedSize(FVolumeVersion)) then Exit;
                 finally
                  if Attribute = nil then Security.Free;
                 end;
                end;
              end;
             ntfsNTFS30,ntfsNTFS31:begin
               {Get Attribute}
               Attribute:=Origin.GetAttribute(ntfsAttrTypeStandardInformation,ntfsBlankName,ntfsInstanceFirst);
               if Attribute <> nil then
                begin
                 {Get Security}
                 Security:=TNTFSSecurity(GetSecurityEx(AParent,True,SecurityId,True));
                 if Security = nil then Exit;
                 try
                  {Check Security Id}
                  if SecurityId <> ntfsSecurityIdUnknown then
                   begin
                    {Set Security}
                    TNTFSStandardInformationAttribute(Attribute).SecurityId:=SecurityId;
                   end
                  else
                   begin
                    {Add Security}
                    SecurityId:=AddSecurity(Security);
                    if SecurityId = ntfsSecurityIdUnknown then Exit;

                    {Set Security}
                    TNTFSStandardInformationAttribute(Attribute).SecurityId:=SecurityId;
                   end;
                 finally
                  if SecurityId = ntfsSecurityIdUnknown then Security.Free;
                 end;
                end;
              end;
            end;
           end;
         end;

        {Set Records}
        if not SetRecords(Origin) then Exit;

        Result:=Link;

        {Add Reference}
        if AReference then Result.AddReference;
       finally
        if Result = nil then FEntries.Remove(Link);
       end;
      finally
       if Result = nil then Origin.DeleteLink(Link);
       if Result = nil then Link.Free;
      end;
     finally
      if Result = nil then RemoveRecord(Origin);
     end;
    finally
     FRecords.WriterUnlock;
    end;
   end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.AddEntryEx(AParent:TDiskEntry;const AName,AAltName:String;AAttributes:LongWord;AReference:Boolean):TDiskEntry;
{Reference: Add a reference on the returned entry if True}
{If AltName already exists or is invalid then a generated one will be provided, will not fail due to AltName}
{If AltName is not supplied then none is created regardless of the setting of NoShortNames}
{Caller must either supply a short name or call AddEntry instead (Differs from FAT etc)}
var
 Name:String;
 AltName:String;
 NameSpace:Byte;
 SecurityId:LongWord;

 Link:TNTFSDiskEntry;
 Entry:TNTFSDiskEntry;
 Stream:TNTFSDiskEntry;

 Descriptor:Pointer;
 Security:TNTFSSecurity;

 Origin:TNTFSDiskRecord;
 Current:TNTFSDiskRecord;

 Root:TNTFSDiskAttribute;
 Attribute:TNTFSDiskAttribute;
 Alternate:TNTFSDiskAttribute;
begin
 {}
 Result:=nil;

 if not FEntries.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if AParent = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AddEntryEx - Parent = ' + AParent.Name + ' Name = ' + AName + ' AltName = ' + AAltName);
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Relative}
  if (AParent.Attributes and (faDot or faDotDot)) <> faNone then Exit;

  {Check Reparse}
  if (AParent.Attributes and faReparse) <> faNone then Exit;

  {Check Stream}
  if (AAttributes and faMatchMask) = faStream then
   begin
    {Check Parent}
    if (AParent.Attributes and faMatchMask) = faStream then Exit;

    {Check Existing}
    Result:=GetEntryEx(AParent,AName,faStream,AReference,False,True);
    if Result <> nil then Exit;

    {Get Origin}
    if not FRecords.WriterLock then Exit;
    try
     if TNTFSDiskEntry(AParent).Origin = nil then Exit;
     Origin:=TNTFSDiskEntry(AParent).Origin.Origin;
     if Origin = nil then Exit;

     {Get Name}
     Name:=NTFSStreamNameToAttributeName(ntfsAttrTypeData,AName);
     if Length(Name) = 0 then Exit;

     {Check Name}
     if not CheckAttributeName(Name) then Exit;

     {Add Attribute}
     Attribute:=AddAttribute(Origin,ntfsAttrTypeData,Name);
     if Attribute = nil then Exit;
     //To Do //try finally /remove attribute on failure

     {Add Stream}
     Stream:=Origin.NewStream(Attribute);
     if Stream = nil then Exit;
     try
      {Update Entry}
      Stream.LocalLock:=FEntryLocal;
      if not Stream.UpdateEntry then Exit;

      {Add Entry}
      if not FEntries.Add(Stream,AParent) then Exit;
      try
       {Add Streams} {Streams belonging to other Links}
       if Origin.Links <> nil then
        begin
         Link:=Origin.Links.FirstEntry;
         while Link <> nil do
          begin
           if Link <> AParent then
            begin
             {Add Stream}
             Entry:=Origin.NewStream(Attribute);
             if Entry = nil then Exit;

             {Update Entry}
             Entry.LocalLock:=FEntryLocal;
             if not Entry.UpdateEntry then Exit;

             {Add Entry}
             if not FEntries.Add(Entry,Link) then Exit;
            end;

           Link:=Link.NextEntry;
          end;
        end;

       {Set Records}
       if not SetRecords(Origin) then Exit;

       Result:=Stream;

       {Add Reference}
       if AReference then Result.AddReference;
      finally
       if Result = nil then FEntries.Remove(Stream);
      end;
     finally
      if Result = nil then Origin.DeleteStream(Stream);
      if Result = nil then Stream.Free;
     end;
     //To Do //Remove Streams belonging to other Links on failure
     //To Do //Remove Attribute on failure
    finally
     FRecords.WriterUnlock;
    end;
   end
  else
   begin
    {Check Parent}
    if (AParent.Attributes and faMatchMask) <> faDirectory then Exit;

    {Check Attribtues (Include Folder/File)}
    if (AAttributes and (faDirectory or faFile)) <> faNone then
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

    {Check Short}
    if IsShort(AName) then
     begin
      {Get AltName}
      AltName:=ntfsBlankName;

      {Get NameSpace}
      NameSpace:=ntfsNameSpaceBoth;
     end
    else
     begin
      {Check Supported}
      if not FLongNames then Exit;

      {Check AltName}
      if Length(AAltName) <> 0 then
       begin
        {Get AltName}
        AltName:=Uppercase(AAltName);
        if not(IsShort(AltName)) or (GetEntryEx(AParent,AltName,faDirectory or faFile,False,False,True) <> nil) then AltName:=GenerateName(AParent,nil,AName);
        if Length(AltName) = 0 then Exit;

        {Get NameSpace}
        NameSpace:=ntfsNameSpaceWin32;
       end
      else
       begin
        {Get AltName}
        AltName:=ntfsBlankName; {Do not generate if not supplied}
        {AltName:=GenerateName(AParent,nil,AName);}
        {if Length(AltName) = 0 then Exit;}

        {Get NameSpace}
        NameSpace:=ntfsNameSpacePosix; {Always Posix namespace if no short name}
       end;
     end;

    {Get Parent}
    if not FRecords.WriterLock then Exit;
    try
     if TNTFSDiskEntry(AParent).Origin = nil then Exit;
     Current:=TNTFSDiskEntry(AParent).Origin.Origin;
     if Current = nil then Exit;

     {Add Record}
     Origin:=AddRecord(nil,(AAttributes and faMatchMask) = faDirectory);
     if Origin = nil then Exit;
     try
      {Add Attribute}
      Attribute:=AddAttribute(Origin,ntfsAttrTypeFileName,ntfsBlankName);
      if Attribute = nil then Exit;

      {Update Attribute}
      TNTFSFileNameAttribute(Attribute).ParentReference:=Current.FileReference;
      TNTFSFileNameAttribute(Attribute).NameSpace:=NameSpace;
      TNTFSFileNameAttribute(Attribute).FileName:=AName;
      Attribute.Indexed:=ntfsAttributeIndexed;
      Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
      Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

      {Size Record}
      if not SizeRecord(Origin,Origin.CalculatedSize(FVolumeVersion)) then Exit;

      {Check Alt Name}
      Alternate:=nil;
      if Length(AltName) <> 0 then
       begin
        {Add Alternate}
        Alternate:=AddAttribute(Origin,ntfsAttrTypeFileName,ntfsBlankName);
        if Alternate = nil then Exit;

        {Update Alternate}
        TNTFSFileNameAttribute(Alternate).ParentReference:=Current.FileReference;
        TNTFSFileNameAttribute(Alternate).NameSpace:=ntfsNameSpaceDos;
        TNTFSFileNameAttribute(Alternate).FileName:=AltName;
        Alternate.Indexed:=ntfsAttributeIndexed;
        Alternate.DataSize:=Alternate.CalculatedStreamSize(FVolumeVersion);
        Alternate.AttributeSize:=Alternate.CalculatedSize(FVolumeVersion);

        {Size Record}
        if not SizeRecord(Origin,Origin.CalculatedSize(FVolumeVersion)) then Exit;
       end;

      {Add Link}
      Link:=Origin.NewLink(Attribute,Alternate);
      if Link = nil then Exit;
      try
       {Update Entry}
       Link.LocalLock:=FEntryLocal;
       if not Link.UpdateEntry then Exit;

       {Update Attribute}
       if not Attribute.UpdateAttribute(Link) then Exit;

       {Update Alternate}
       if Alternate <> nil then if not Alternate.UpdateAttribute(Link) then Exit;

       {Add Key}
       if not AddKey(Current,Attribute) then Exit;

       {Add Key}
       if Alternate <> nil then if not AddKey(Current,Alternate) then Exit;

       {Get Root}
       Root:=Current.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameFileName,ntfsInstanceFirst);
       if Root = nil then Exit;

       {Set Index}
       if not SetAttribute(Current,Root) then Exit;

       {Add Entry}
       if not FEntries.Add(Link,AParent) then Exit;
       try
        {Check Security}
        if not FNullSecurity then
         begin
          if FDefaultSecurity then
           begin
            {Default Security}
            {Check Version}
            case FVolumeVersion of
             ntfsNTFS12:begin
               {Get Attribute}
               Attribute:=Origin.GetAttribute(ntfsAttrTypeSecurityDescriptor,ntfsBlankName,ntfsInstanceFirst);
               if Attribute = nil then
                begin
                 {Add Attribute}
                 Attribute:=Origin.NewAttribute(nil,ntfsAttrTypeSecurityDescriptor,ntfsBlankName,FVolumeVersion);
                 if Attribute = nil then Exit;

                 {Get Descriptor}
                 Descriptor:=nil;
                 if (AAttributes and faMatchMask) = faDirectory then
                  begin
                   if not NTFSCreateDefaultDescriptor(ntfsDefaultDescriptorFolder,Descriptor,FVolumeVersion) then Exit;
                  end
                 else
                  begin
                   if not NTFSCreateDefaultDescriptor(ntfsDefaultDescriptorFile,Descriptor,FVolumeVersion) then Exit;
                  end;
                 try
                  {Add Security}
                  Security:=TNTFSSecurity.CreateFromDescriptor(FSecurityLocal,Descriptor);
                  if Security = nil then Exit;
                  if not TNTFSSecurityDescriptorAttribute(Attribute).NewSecurity(Security) then Exit;

                  {Update Attribute}
                  Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
                  Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

                  {Add Item}
                  if not AddItem(Origin,Attribute) then Exit;

                  {Size Record}
                  if not SizeRecord(Origin,Origin.CalculatedSize(FVolumeVersion)) then Exit;
                 finally
                  NTFSDestroyDefaultDescriptor(Descriptor,FVolumeVersion);
                 end;
                end;
              end;
             ntfsNTFS30,ntfsNTFS31:begin
               {Get Attribute}
               Attribute:=Origin.GetAttribute(ntfsAttrTypeStandardInformation,ntfsBlankName,ntfsInstanceFirst);
               if Attribute <> nil then
                begin
                 {Get Descriptor}
                 Descriptor:=nil;
                 if (AAttributes and faMatchMask) = faDirectory then
                  begin
                   if not NTFSCreateDefaultDescriptor(ntfsDefaultDescriptorFolder,Descriptor,FVolumeVersion) then Exit;
                  end
                 else
                  begin
                   if not NTFSCreateDefaultDescriptor(ntfsDefaultDescriptorFile,Descriptor,FVolumeVersion) then Exit;
                  end;
                 try
                  {Get Security Id}
                  SecurityId:=GetDescriptorId(Descriptor,True);
                  if SecurityId <> ntfsSecurityIdUnknown then
                   begin
                    {Set Security}
                    TNTFSStandardInformationAttribute(Attribute).SecurityId:=SecurityId;
                   end
                  else
                   begin
                    {Get Security}
                    Security:=TNTFSSecurity.CreateFromDescriptor(FSecurityLocal,Descriptor);
                    if Security = nil then Exit;
                    try
                     {Add Security}
                     SecurityId:=AddSecurity(Security);
                     if SecurityId = ntfsSecurityIdUnknown then Exit;

                     {Set Security}
                     TNTFSStandardInformationAttribute(Attribute).SecurityId:=SecurityId;
                    finally
                     if SecurityId = ntfsSecurityIdUnknown then Security.Free;
                    end;
                   end;
                 finally
                  NTFSDestroyDefaultDescriptor(Descriptor,FVolumeVersion);
                 end;
                end;
              end;
            end;
           end
          else
           begin
            {Standard Security}
            {Check Version}
            case FVolumeVersion of
             ntfsNTFS12:begin
               {Get Attribute}
               Attribute:=Origin.GetAttribute(ntfsAttrTypeSecurityDescriptor,ntfsBlankName,ntfsInstanceFirst);
               if Attribute = nil then
                begin
                 {Get Security}
                 Security:=TNTFSSecurity(GetSecurityEx(AParent,True,SecurityId,True));
                 if Security = nil then Exit;
                 try
                  {Add Attribute}
                  Attribute:=Origin.NewAttribute(nil,ntfsAttrTypeSecurityDescriptor,ntfsBlankName,FVolumeVersion);
                  if Attribute = nil then Exit;

                  {Add Security}
                  if not TNTFSSecurityDescriptorAttribute(Attribute).NewSecurity(Security) then Exit;

                  {Update Attribute}
                  Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
                  Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

                  {Add Item}
                  if not AddItem(Origin,Attribute) then Exit;

                  {Size Record}
                  if not SizeRecord(Origin,Origin.CalculatedSize(FVolumeVersion)) then Exit;
                 finally
                  if Attribute = nil then Security.Free;
                 end;
                end;
              end;
             ntfsNTFS30,ntfsNTFS31:begin
               {Get Attribute}
               Attribute:=Origin.GetAttribute(ntfsAttrTypeStandardInformation,ntfsBlankName,ntfsInstanceFirst);
               if Attribute <> nil then
                begin
                 {Get Security}
                 Security:=TNTFSSecurity(GetSecurityEx(AParent,True,SecurityId,True));
                 if Security = nil then Exit;
                 try
                  {Check Security Id}
                  if SecurityId <> ntfsSecurityIdUnknown then
                   begin
                    {Set Security}
                    TNTFSStandardInformationAttribute(Attribute).SecurityId:=SecurityId;
                   end
                  else
                   begin
                    {Add Security}
                    SecurityId:=AddSecurity(Security);
                    if SecurityId = ntfsSecurityIdUnknown then Exit;

                    {Set Security}
                    TNTFSStandardInformationAttribute(Attribute).SecurityId:=SecurityId;
                   end;
                 finally
                  if SecurityId = ntfsSecurityIdUnknown then Security.Free;
                 end;
                end;
              end;
            end;
           end;
         end;

        {Set Records}
        if not SetRecords(Origin) then Exit;

        Result:=Link;

        {Add Reference}
        if AReference then Result.AddReference;
       finally
        if Result = nil then FEntries.Remove(Link);
       end;
      finally
       if Result = nil then Origin.DeleteLink(Link);
       if Result = nil then Link.Free;
      end;
     finally
      if Result = nil then RemoveRecord(Origin);
     end;
    finally
     FRecords.WriterUnlock;
    end;
   end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.RemoveEntry(AParent,AEntry:TDiskEntry):Boolean;
var
 Success:Boolean;
 Entry:TNTFSDiskEntry;
 Previous:TNTFSDiskEntry;

 Origin:TNTFSDiskRecord;
 Current:TNTFSDiskRecord;

 Root:TNTFSDiskAttribute;
 Attribute:TNTFSDiskAttribute;
 Alternate:TNTFSDiskAttribute;
 Additional:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if AParent = nil then Exit;
  if AEntry = nil then Exit;
  if AEntry = FRoot then Exit; {Cannot remove root}

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.RemoveEntry - Entry = ' + AEntry.Name);
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Parent}
  if AEntry.Parent <> AParent then Exit;

  {Check Relative}
  if (AEntry.Attributes and (faDot or faDotDot)) <> faNone then Exit;

  {Get Origin}
  if not FRecords.WriterLock then Exit;
  try
   if TNTFSDiskEntry(AEntry).Origin = nil then Exit;
   Origin:=TNTFSDiskEntry(AEntry).Origin.Origin;
   if Origin = nil then Exit;

   {Check Metafile}
   if Origin.Metafile then Exit; {Cannot remove metafile}

   {Check Stream}
   if (AEntry.Attributes and faMatchMask) = faStream then
    begin
     {Get Attribute}
     Attribute:=TNTFSDiskEntry(AEntry).Attribute; {Will always be the first instance due to LoadEntries}
     if Attribute = nil then Exit;

     {Remove Streams} {Streams belonging to other Links}
     if Origin.Streams <> nil then
      begin
       Entry:=Origin.Streams.FirstEntry;
       while Entry <> nil do
        begin
         {Save Entry}
         Previous:=Entry;
         Entry:=Entry.NextEntry;

         {Check Stream}
         if (Previous <> AEntry) and (Previous.Attribute = Attribute) then
          begin
           {Delete Stream}
           if not Origin.DeleteStream(Previous) then Exit;

           {Remove Entry}
           if not FEntries.Remove(Previous) then Exit;

           Previous.Origin:=nil;

           {Schedule Entry}
           if not FDriver.ScheduleEntry(Previous,FILESYS_ENTRY_DELETE_TIMEOUT) then Exit;
          end;
        end;
      end;

     {Delete Stream}
     if not Origin.DeleteStream(TNTFSDiskEntry(AEntry)) then Exit;

     {Get Attribute} {Last Instance} //To Do //If an attribute list exists then instances are sorted by StartVCN //What if there are multiple instances and no list, can that happen ?
     Additional:=Origin.GetAttribute(Attribute.AttributeType,Attribute.AttributeName,ntfsInstanceLast);
     while Additional <> nil do
      begin
       if Additional.StartVCN = 0 then
        begin
         {Remove Attribute}
         if not RemoveAttribute(Origin,Additional) then Exit;

         Break; {Break to allow success}
        end
       else
        begin
         {Remove Attribute}
         if not RemoveAttribute(Origin,Additional) then Exit;
        end;

       {Get Attribute} {Last Instance}
       Additional:=Origin.GetAttribute(Attribute.AttributeType,Attribute.AttributeName,ntfsInstanceLast);
      end;

     {Remove Entry}
     if not FEntries.Remove(AEntry) then Exit;

     TNTFSDiskEntry(AEntry).Origin:=nil;

     {Schedule Entry}
     if not FDriver.ScheduleEntry(AEntry,FILESYS_ENTRY_DELETE_TIMEOUT) then Exit;

     {Set Records}
     Result:=SetRecords(Origin);

     //To Do //Also need to update the QuotaCharge in the StandardInformation attribute and the Quota index ?
    end
   else
    begin
     {Get Attribute}
     Attribute:=TNTFSDiskEntry(AEntry).Attribute;
     if Attribute = nil then Exit;

     {Get Reference}
     Current:=GetReferenceEx(nil,TNTFSFileNameAttribute(Attribute).ParentReference,True);
     if Current = nil then Exit;

     {Remove Key}
     if not RemoveKey(Current,Attribute) then Exit;

     {Remove Attribute} {Is single instance so no need to call get attribute}
     if not RemoveAttribute(Origin,Attribute) then Exit;

     {Get Alternate}
     Alternate:=TNTFSDiskEntry(AEntry).Alternate;
     if Alternate <> nil then
      begin
       {$IFDEF NTFS_DEBUG}
       if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.RemoveEntry - AltName = ' + AEntry.AltName);
       {$ENDIF}

       {Check Reference}
       if TNTFSFileNameAttribute(Attribute).ParentReference <> TNTFSFileNameAttribute(Alternate).ParentReference then
        begin
         {Get Root}
         Root:=Current.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameFileName,ntfsInstanceFirst);
         if Root = nil then Exit;

         {Set Index}
         if not SetAttribute(Current,Root) then Exit;
        end;

       {Get Reference}
       Current:=GetReferenceEx(nil,TNTFSFileNameAttribute(Alternate).ParentReference,True);
       if Current = nil then Exit;

       {Remove Key}
       if not RemoveKey(Current,Alternate) then Exit;

       {Remove Attribute} {Is single instance so no need to call get attribute}
       if not RemoveAttribute(Origin,Alternate) then Exit;
      end;

     {Get Root}
     Root:=Current.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameFileName,ntfsInstanceFirst);
     if Root = nil then Exit;

     {Set Index}
     if not SetAttribute(Current,Root) then Exit;

     {Delete Link}
     if not Origin.DeleteLink(TNTFSDiskEntry(AEntry)) then Exit;

     {Check Links}
     if Origin.LinkCount = 0 then {Note that Windows can actually miscalculate the Hardlink count when renaming a file with Hardlinks}
      begin
       {Check ObjectId}
       Attribute:=Origin.GetAttribute(ntfsAttrTypeObjectId,ntfsAnyName,ntfsInstanceFirst);
       if Attribute <> nil then
        begin
         {Remove ObjectId}
         {if not RemoveObjId(TNTFSObjectIdAttribute(Attribute).ObjectId) then Exit;} {Dont fail delete if ObjectId not found}
         Success:=RemoveObjId(TNTFSObjectIdAttribute(Attribute).ObjectId);  //To Do //Testing5
         if not(Success) and FILESYS_LOG_ENABLED then FileSysLogError('Failed to remove ObjectId from index for ' + AEntry.Name); //To Do //Need a NTFSLenient parameter to determine handling

         {$IFDEF NTFS_DEBUG}
         if FILESYS_LOG_ENABLED then if not Success then FileSysLogDebug('TNTFSFileSystem.RemoveEntry - Failed Remove ObjectId from Index for ' + AEntry.Name);
         {$ENDIF}
        end;

       {Check Reparse}
       Attribute:=Origin.GetAttribute(ntfsAttrTypeReparsePoint,ntfsAnyName,ntfsInstanceFirst);
       if Attribute <> nil then
        begin
         {Remove Reparse}
         {if not RemoveReparse(TNTFSReparsePointAttribute(Attribute).ReparseTag,Origin.FileReference) then Exit;} {Dont fail delete if Reparse not found}
         Success:=RemoveReparse(TNTFSReparsePointAttribute(Attribute).ReparseTag,Origin.FileReference);  //To Do //Testing5
         if not(Success) and FILESYS_LOG_ENABLED then FileSysLogError('Failed to remove Reparse from index for ' + AEntry.Name); //To Do //Need a NTFSLenient parameter to determine handling

         {$IFDEF NTFS_DEBUG}
         if FILESYS_LOG_ENABLED then if not Success then FileSysLogDebug('TNTFSFileSystem.RemoveEntry - Failed Remove Reparse from Index for ' + AEntry.Name);
         {$ENDIF}
        end;

       //To Do //Need to check for OwnerId/SecurityId/QuotaCharge in Header and update Indexes if not 0 ?

       {Check Directory}
       if (AEntry.Attributes and faMatchMask) = faDirectory then
        begin
         //To Do //Change this to use AEntry.GetDot and GetDotDot methods
         Entry:=TNTFSDiskEntry(AEntry.FirstChild);
         while Entry <> nil do
          begin
           {Save Entry}
           Previous:=Entry;
           Entry:=TNTFSDiskEntry(Entry.Next);

           {Remove Dot and DotDot}
           if (Previous.Attributes and (faDot or faDotDot)) <> faNone then
            begin
             {Remove Entry}
             if not FEntries.Remove(Previous) then Exit;

             Previous.Origin:=nil;

             {Schedule Entry}
             if not FDriver.ScheduleEntry(Previous,FILESYS_ENTRY_DELETE_TIMEOUT) then Exit;
            end;
          end;
        end;

       {Remove Streams}
       if Origin.Streams <> nil then
        begin
         Entry:=Origin.Streams.FirstEntry;
         while Entry <> nil do
          begin
           {Save Entry}
           Previous:=Entry;
           Entry:=Entry.NextEntry;

           {Delete Stream}
           if not Origin.DeleteStream(Previous) then Exit;

           {Remove Entry}
           if not FEntries.Remove(Previous) then Exit;

           Previous.Origin:=nil;

           {Schedule Entry}
           if not FDriver.ScheduleEntry(Previous,FILESYS_ENTRY_DELETE_TIMEOUT) then Exit;
          end;
        end;

       {Remove Record}
       if not RemoveRecord(Origin) then Exit;

       {Remove Entry}
       if not FEntries.Remove(AEntry) then Exit;

       TNTFSDiskEntry(AEntry).Origin:=nil;

       {Schedule Entry}
       if not FDriver.ScheduleEntry(AEntry,FILESYS_ENTRY_DELETE_TIMEOUT) then Exit;

       Result:=True;

       //To Do //Also need to update the QuotaCharge in the StandardInformation attribute and the Quota index ?
      end
     else
      begin
       {Remove Entry}
       if not FEntries.Remove(AEntry) then Exit;

       TNTFSDiskEntry(AEntry).Origin:=nil;

       {Schedule Entry}
       if not FDriver.ScheduleEntry(AEntry,FILESYS_ENTRY_DELETE_TIMEOUT) then Exit;

       {Set Records}
       Result:=SetRecords(Origin);

       //To Do //Also need to update the QuotaCharge in the StandardInformation attribute and the Quota index ?
      end;
    end;
  finally
   FRecords.WriterUnlock;
  end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.RenameEntry(AParent,AEntry:TDiskEntry;const AName:String):Boolean;
var
 Name:String;
 AltName:String;
 OldName:String;
 OldAltName:String;
 NameSpace:Byte;
 Entry:TNTFSDiskEntry;
 Origin:TNTFSDiskRecord;
 Current:TNTFSDiskRecord;

 Root:TNTFSDiskAttribute;
 Attribute:TNTFSDiskAttribute;
 Alternate:TNTFSDiskattribute;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if AParent = nil then Exit;
  if AEntry = nil then Exit;
  if AEntry = FRoot then Exit; {Cannot rename root}

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.RenameEntry - Entry = ' + AEntry.Name + ' Name = ' + AName);
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Relative}
  if (AEntry.Attributes and (faDot or faDotDot)) <> faNone then Exit;

  {Check Parent}
  if AEntry.Parent <> AParent then Exit;

  {Get Origin}
  if not FRecords.WriterLock then Exit;
  try
   if TNTFSDiskEntry(AEntry).Origin = nil then Exit;
   Origin:=TNTFSDiskEntry(AEntry).Origin.Origin;
   if Origin = nil then Exit;

   {Check Metafile}
   if Origin.Metafile then Exit; {Cannot rename metafile}

   {Check Stream}
   if (AEntry.Attributes and faMatchMask) = faStream then
    begin
     {Check Existing}
     if GetEntryEx(AParent,AName,faStream,False,False,True) <> nil then Exit;

     {Get Attribute}
     Attribute:=TNTFSDiskEntry(AEntry).Attribute; {Will always be the first instance due to LoadEntries}
     if Attribute = nil then Exit;

     {Get Name}
     Name:=NTFSStreamNameToAttributeName(ntfsAttrTypeData,AName);
     if Length(Name) = 0 then Exit;

     {Check Name}
     if not CheckAttributeName(Name) then Exit;

     {Rename Attribute}
     if not RenameAttribute(Origin,Attribute,Name) then Exit;

     {Set Records}
     if not SetRecords(Origin) then Exit;

     {Update Streams} {Streams belonging to other Links}
     if Origin.Streams <> nil then
      begin
       Entry:=Origin.Streams.FirstEntry;
       while Entry <> nil do
        begin
         if (Entry <> AEntry) and (Entry.Attribute = Attribute) then if not Attribute.UpdateEntry(Entry) then Exit;

         Entry:=Entry.NextEntry;
        end;
      end;

     {Update Entry}
     Result:=Attribute.UpdateEntry(TNTFSDiskEntry(AEntry));
    end
   else
    begin
     {Check Parent}
     if (AParent.Attributes and faMatchMask) <> faDirectory then Exit;

     {Check Attribtues (Include Folder/File)}
     if (AEntry.Attributes and (faDirectory or faFile)) <> faNone then
      begin
       {Check Existing}
       if GetEntryEx(AParent,AName,faDirectory or faFile,False,False,True) <> nil then Exit;
      end;

     {Check Short}
     if IsShort(AName) then
      begin
       {Get AltName}
       AltName:=ntfsBlankName;

       {Get NameSpace}
       NameSpace:=ntfsNameSpaceBoth;
      end
     else
      begin
       {Check Supported}
       if not FLongNames then Exit;

       {Check Short Names}
       if FNoShortNames then
        begin
         {Get AltName}
         AltName:=ntfsBlankName;

         {Get NameSpace}
         NameSpace:=ntfsNameSpacePosix; {Always Posix namespace if no short name}
        end
       else
        begin
         {Get AltName}
         AltName:=GenerateName(AParent,nil,AName);
         if Length(AltName) = 0 then Exit;

         {Get NameSpace}
         NameSpace:=ntfsNameSpaceWin32;
        end;
      end;

     {Get Parent}
     if TNTFSDiskEntry(AParent).Origin = nil then Exit;
     Current:=TNTFSDiskEntry(AParent).Origin.Origin;
     if Current = nil then Exit;

     {Get Attribute}
     Attribute:=TNTFSDiskEntry(AEntry).Attribute;
     if Attribute = nil then Exit;

     {Check Reference}
     if Current.FileReference <> TNTFSFileNameAttribute(Attribute).ParentReference then Exit;

     {Check NameSpace}
     if TNTFSFileNameAttribute(Attribute).NameSpace = ntfsNameSpacePosix then
      begin
       {Save Name}
       OldName:=TNTFSFileNameAttribute(Attribute).FileName;

       {Update Attribute}
       TNTFSFileNameAttribute(Attribute).FileName:=AName;
       Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
       Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

       {Size Record}
       if not SizeRecord(Origin,Origin.CalculatedSize(FVolumeVersion)) then Exit;

       {Update Entry}
       if not Attribute.UpdateEntry(TNTFSDiskEntry(AEntry)) then Exit;

       {Update Attribute}
       if not Attribute.UpdateAttribute(TNTFSDiskEntry(AEntry)) then Exit;

       {Rename Key}
       if not RenameKey(Current,Attribute,OldName) then Exit;
      end
     else
      begin
       {Save Name}
       OldName:=TNTFSFileNameAttribute(Attribute).FileName;

       {Update Attribute}
       TNTFSFileNameAttribute(Attribute).NameSpace:=NameSpace;
       TNTFSFileNameAttribute(Attribute).FileName:=AName;
       Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
       Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

       {Size Record}
       if not SizeRecord(Origin,Origin.CalculatedSize(FVolumeVersion)) then Exit;

       {Update Entry}
       if not Attribute.UpdateEntry(TNTFSDiskEntry(AEntry)) then Exit;

       {Update Attribute}
       if not Attribute.UpdateAttribute(TNTFSDiskEntry(AEntry)) then Exit;

       {Rename Key}
       if not RenameKey(Current,Attribute,OldName) then Exit;

       {Get Alternate}
       Alternate:=TNTFSDiskEntry(AEntry).Alternate;
       if Alternate <> nil then
        begin
         {Check Reference}
         if Current.FileReference <> TNTFSFileNameAttribute(Alternate).ParentReference then Exit;

         {Save Alt Name}
         OldAltName:=TNTFSFileNameAttribute(Alternate).FileName;

         {Check Alt Name}
         if Length(AltName) <> 0 then
          begin
           {Update Alternate}
           TNTFSFileNameAttribute(Alternate).FileName:=AltName;
           Alternate.DataSize:=Alternate.CalculatedStreamSize(FVolumeVersion);
           Alternate.AttributeSize:=Alternate.CalculatedSize(FVolumeVersion);

           {Size Record}
           if not SizeRecord(Origin,Origin.CalculatedSize(FVolumeVersion)) then Exit;

           {Update Entry}
           if not Alternate.UpdateEntry(TNTFSDiskEntry(AEntry)) then Exit;

           {Update Alternate}
           if not Alternate.UpdateAttribute(TNTFSDiskEntry(AEntry)) then Exit;

           {Rename Key}
           if not RenameKey(Current,Alternate,OldAltName) then Exit;
          end
         else
          begin
           {Remove Key}
           if not RemoveKey(Current,Alternate) then Exit;

           {Remove Alternate} {Is single instance so no need to call get attribute}
           if not RemoveAttribute(Origin,Alternate) then Exit;

           {Update Link}
           if not Origin.RenameLink(TNTFSDiskEntry(AEntry),Attribute,nil) then Exit;

           {Update Entry}
           if not Attribute.UpdateEntry(TNTFSDiskEntry(AEntry)) then Exit; {Added 22/11/2011}
          end;
        end
       else
        begin
         {Check Alt Name}
         if Length(AltName) <> 0 then
          begin
           {Add Alternate}
           Alternate:=AddAttribute(Origin,ntfsAttrTypeFileName,ntfsBlankName);
           if Alternate = nil then Exit;

           {Update Alternate}
           TNTFSFileNameAttribute(Alternate).ParentReference:=Current.FileReference;
           TNTFSFileNameAttribute(Alternate).NameSpace:=ntfsNameSpaceDos;
           TNTFSFileNameAttribute(Alternate).FileName:=AltName;
           Alternate.Indexed:=ntfsAttributeIndexed;
           Alternate.DataSize:=Alternate.CalculatedStreamSize(FVolumeVersion);
           Alternate.AttributeSize:=Alternate.CalculatedSize(FVolumeVersion);

           {Size Record}
           if not SizeRecord(Origin,Origin.CalculatedSize(FVolumeVersion)) then Exit;

           {Update Link}
           if not Origin.RenameLink(TNTFSDiskEntry(AEntry),Attribute,Alternate) then Exit;

           {Update Entry}
           if not Alternate.UpdateEntry(TNTFSDiskEntry(AEntry)) then Exit;

           {Update Alternate}
           if not Alternate.UpdateAttribute(TNTFSDiskEntry(AEntry)) then Exit;

           {Add Key}
           if not AddKey(Current,Alternate) then Exit;
          end;
        end;
      end;

     {Get Root}
     Root:=Current.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameFileName,ntfsInstanceFirst);
     if Root = nil then Exit;

     {Set Index}
     if not SetAttribute(Current,Root) then Exit;

     {Set Records}
     if not SetRecords(Origin) then Exit;

     Result:=True;
    end;
  finally
   FRecords.WriterUnlock;
  end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.RenameEntryEx(AParent,AEntry:TDiskEntry;const AAltName:String):Boolean;
var
 AltName:String;
 OldAltName:String;
 NameSpace:Byte;
 Entry:TNTFSDiskEntry;
 Origin:TNTFSDiskRecord;
 Current:TNTFSDiskRecord;

 Root:TNTFSDiskAttribute;
 Attribute:TNTFSDiskAttribute;
 Alternate:TNTFSDiskattribute;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if AParent = nil then Exit;
  if AEntry = nil then Exit;
  if AEntry = FRoot then Exit; {Cannot rename root}

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.RenameEntryEx - Entry = ' + AEntry.Name + ' AltName = ' + AAltName);
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Relative}
  if (AEntry.Attributes and (faDot or faDotDot)) <> faNone then Exit;

  {Check Parent}
  if AEntry.Parent <> AParent then Exit;

  {Get Origin}
  if not FRecords.WriterLock then Exit;
  try
   if TNTFSDiskEntry(AEntry).Origin = nil then Exit;
   Origin:=TNTFSDiskEntry(AEntry).Origin.Origin;
   if Origin = nil then Exit;

   {Check Metafile}
   if Origin.Metafile then Exit; {Cannot rename metafile}

   {Check Stream}
   if (AEntry.Attributes and faMatchMask) = faStream then Exit;

   {Check Parent}
   if (AParent.Attributes and faMatchMask) <> faDirectory then Exit;

   {Check AltName}
   if Length(AAltName) <> 0 then
    begin
     {Get AltName}
     AltName:=Uppercase(AAltName);
     if not(IsShort(AltName)) or (GetEntryEx(AParent,AltName,faDirectory or faFile,False,False,True) <> nil) then Exit;

     {Get NameSpace}
     NameSpace:=ntfsNameSpaceWin32;

     {Get Parent}
     if TNTFSDiskEntry(AParent).Origin = nil then Exit;
     Current:=TNTFSDiskEntry(AParent).Origin.Origin;
     if Current = nil then Exit;

     {Get Attribute}
     Attribute:=TNTFSDiskEntry(AEntry).Attribute;
     if Attribute = nil then Exit;

     {Check Reference}
     if Current.FileReference <> TNTFSFileNameAttribute(Attribute).ParentReference then Exit;

     {Check NameSpace}
     if (TNTFSFileNameAttribute(Attribute).NameSpace = ntfsNameSpaceWin32) or (TNTFSFileNameAttribute(Attribute).NameSpace = ntfsNameSpaceBoth) then
      begin
       {Update Attribute}
       TNTFSFileNameAttribute(Attribute).NameSpace:=NameSpace;

       {Update Entry}
       if not Attribute.UpdateEntry(TNTFSDiskEntry(AEntry)) then Exit;

       {Update Attribute}
       if not Attribute.UpdateAttribute(TNTFSDiskEntry(AEntry)) then Exit;

       {Set Key}
       if not SetKey(Current,Attribute) then Exit;

       {Get Alternate}
       Alternate:=TNTFSDiskEntry(AEntry).Alternate;
       if Alternate <> nil then
        begin
         {Check Reference}
         if Current.FileReference <> TNTFSFileNameAttribute(Alternate).ParentReference then Exit;

         {Save Alt Name}
         OldAltName:=TNTFSFileNameAttribute(Alternate).FileName;

         {Update Alternate}
         TNTFSFileNameAttribute(Alternate).FileName:=AltName;
         Alternate.DataSize:=Alternate.CalculatedStreamSize(FVolumeVersion);
         Alternate.AttributeSize:=Alternate.CalculatedSize(FVolumeVersion);

         {Size Record}
         if not SizeRecord(Origin,Origin.CalculatedSize(FVolumeVersion)) then Exit;

         {Update Entry}
         if not Alternate.UpdateEntry(TNTFSDiskEntry(AEntry)) then Exit;

         {Update Alternate}
         if not Alternate.UpdateAttribute(TNTFSDiskEntry(AEntry)) then Exit;

         {Rename Key}
         if not RenameKey(Current,Alternate,OldAltName) then Exit;
        end
       else
        begin
         {Add Alternate}
         Alternate:=AddAttribute(Origin,ntfsAttrTypeFileName,ntfsBlankName);
         if Alternate = nil then Exit;

         {Update Alternate}
         TNTFSFileNameAttribute(Alternate).ParentReference:=Current.FileReference;
         TNTFSFileNameAttribute(Alternate).NameSpace:=ntfsNameSpaceDos;
         TNTFSFileNameAttribute(Alternate).FileName:=AltName;
         Alternate.Indexed:=ntfsAttributeIndexed;
         Alternate.DataSize:=Alternate.CalculatedStreamSize(FVolumeVersion);
         Alternate.AttributeSize:=Alternate.CalculatedSize(FVolumeVersion);

         {Size Record}
         if not SizeRecord(Origin,Origin.CalculatedSize(FVolumeVersion)) then Exit;

         {Update Link}
         if not Origin.RenameLink(TNTFSDiskEntry(AEntry),Attribute,Alternate) then Exit;

         {Update Entry}
         if not Alternate.UpdateEntry(TNTFSDiskEntry(AEntry)) then Exit;

         {Update Alternate}
         if not Alternate.UpdateAttribute(TNTFSDiskEntry(AEntry)) then Exit;

         {Add Key}
         if not AddKey(Current,Alternate) then Exit;
        end;

       {Get Root}
       Root:=Current.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameFileName,ntfsInstanceFirst);
       if Root = nil then Exit;

       {Set Index}
       if not SetAttribute(Current,Root) then Exit;

       {Set Records}
       if not SetRecords(Origin) then Exit;

       Result:=True;
      end;
    end
   else
    begin
     {Get AltName}
     AltName:=ntfsBlankName; {Delete short name if not supplied}

     {Get NameSpace}
     NameSpace:=ntfsNameSpacePosix; {Always Posix namespace if no short name}

     {Get Parent}
     if TNTFSDiskEntry(AParent).Origin = nil then Exit;
     Current:=TNTFSDiskEntry(AParent).Origin.Origin;
     if Current = nil then Exit;

     {Get Attribute}
     Attribute:=TNTFSDiskEntry(AEntry).Attribute;
     if Attribute = nil then Exit;

     {Check Reference}
     if Current.FileReference <> TNTFSFileNameAttribute(Attribute).ParentReference then Exit;

     {Check NameSpace}
     if TNTFSFileNameAttribute(Attribute).NameSpace = ntfsNameSpaceWin32 then
      begin
       {Update Attribute}
       TNTFSFileNameAttribute(Attribute).NameSpace:=NameSpace;

       {Update Entry}
       if not Attribute.UpdateEntry(TNTFSDiskEntry(AEntry)) then Exit;

       {Update Attribute}
       if not Attribute.UpdateAttribute(TNTFSDiskEntry(AEntry)) then Exit;

       {Set Key}
       if not SetKey(Current,Attribute) then Exit;

       {Get Alternate}
       Alternate:=TNTFSDiskEntry(AEntry).Alternate;
       if Alternate <> nil then
        begin
         {Check Reference}
         if Current.FileReference <> TNTFSFileNameAttribute(Alternate).ParentReference then Exit;

         {Remove Key}
         if not RemoveKey(Current,Alternate) then Exit;

         {Remove Alternate} {Is single instance so no need to call get attribute}
         if not RemoveAttribute(Origin,Alternate) then Exit;

         {Update Link}
         if not Origin.RenameLink(TNTFSDiskEntry(AEntry),Attribute,nil) then Exit;

         {Update Entry}
         if not Attribute.UpdateEntry(TNTFSDiskEntry(AEntry)) then Exit; {Added 22/11/2011}

         {Get Root}
         Root:=Current.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameFileName,ntfsInstanceFirst);
         if Root = nil then Exit;

         {Set Index}
         if not SetAttribute(Current,Root) then Exit;

         {Set Records}
         if not SetRecords(Origin) then Exit;

         Result:=True;
        end;
      end;
    end;
  finally
   FRecords.WriterUnlock;
  end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.MoveEntry(ASource,ADest,AEntry:TDiskEntry):Boolean;
var
 Entry:TNTFSDiskEntry;
 Origin:TNTFSDiskRecord;
 Target:TNTFSDiskRecord;
 Current:TNTFSDiskRecord;

 Root:TNTFSDiskAttribute;
 Attribute:TNTFSDiskAttribute;
 Alternate:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ASource = nil then Exit;
  if ADest = nil then Exit;
  if AEntry = nil then Exit;
  if AEntry = FRoot then Exit; {Cannot move root}
  if ASource = ADest then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.MoveEntry - Entry = ' + AEntry.Name + ' Source = ' + ASource.Name + ' Dest = ' + ADest.Name);
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Relative}
  if (AEntry.Attributes and (faDot or faDotDot)) <> faNone then Exit;

  {Check Stream}
  if (AEntry.Attributes and faMatchMask) = faStream then Exit;

  {Check Dest}
  if (ADest.Attributes and faMatchMask) <> faDirectory then Exit;

  {Check Source}
  if (ASource.Attributes and faMatchMask) <> faDirectory then Exit;

  {Check Parent}
  if AEntry.Parent <> ASource then Exit;

  {Check Attribtues (Include Folder/File)}
  if (AEntry.Attributes and (faDirectory or faFile)) <> faNone then
   begin
    {Check Existing}
    if GetEntryEx(ADest,AEntry.Name,faDirectory or faFile,False,False,True) <> nil then Exit;
   end;

  {Get Origin}
  if not FRecords.WriterLock then Exit;
  try
   if TNTFSDiskEntry(AEntry).Origin = nil then Exit;
   Origin:=TNTFSDiskEntry(AEntry).Origin.Origin;
   if Origin = nil then Exit;

   {Check Metafile}
   if Origin.Metafile then Exit; {Cannot move metafile}

   {Get Current}
   if TNTFSDiskEntry(ASource).Origin = nil then Exit;
   Current:=TNTFSDiskEntry(ASource).Origin.Origin;
   if Current = nil then Exit;

   {Get Target}
   if TNTFSDiskEntry(ADest).Origin = nil then Exit;
   Target:=TNTFSDiskEntry(ADest).Origin.Origin;
   if Target = nil then Exit;

   {Get Attribute}
   Attribute:=TNTFSDiskEntry(AEntry).Attribute;
   if Attribute = nil then Exit;

   {Check Reference}
   if Current.FileReference <> TNTFSFileNameAttribute(Attribute).ParentReference then Exit;

   {Update Attribute}
   TNTFSFileNameAttribute(Attribute).ParentReference:=Target.FileReference;

   {Get Alternate}
   Alternate:=TNTFSDiskEntry(AEntry).Alternate;
   if Alternate <> nil then
    begin
     {No need to check if Attribute and Alternate are the same Parent (They have to be dont they)}
     {Check Reference}
     if Current.FileReference <> TNTFSFileNameAttribute(Alternate).ParentReference then Exit;

     {Update Alternate}
     TNTFSFileNameAttribute(Alternate).ParentReference:=Target.FileReference;
    end;

   {Move Key}
   if not MoveKey(Current,Target,Attribute) then Exit;

   {Move Key}
   if Alternate <> nil then if not MoveKey(Current,Target,Alternate) then Exit;

   {Get Current Root}
   Root:=Current.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameFileName,ntfsInstanceFirst);
   if Root = nil then Exit;

   {Set Current Index}
   if not SetAttribute(Current,Root) then Exit;

   {Get Target Root}
   Root:=Target.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameFileName,ntfsInstanceFirst);
   if Root = nil then Exit;

   {Set Target Index}
   if not SetAttribute(Target,Root) then Exit;

   {Check for Folder}
   if (AEntry.Attributes and faMatchMask) = faDirectory then
    begin
     {Get DotDot}
     Entry:=TNTFSDiskEntry(AEntry).GetDotDot;
     if Entry = nil then Exit;

     {Update DotDot}
     Entry.UpdateDotDot(TNTFSDiskEntry(ADest));
    end;

   {Move Entry}
   FEntries.Move(AEntry,ADest);

   {Set Records}
   if not SetRecords(Origin) then Exit;

   Result:=True;
  finally
   FRecords.WriterUnlock;
  end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.AddHardlink(AEntry,AParent:TDiskEntry;const AName:String;AReference:Boolean):TDiskEntry;
{Entry is the target and must be a file, Parent is the location of the Hardlink and must be a folder}
{Reference: Add a reference on the returned entry if True}
var
 Link:TNTFSDiskEntry;

 Origin:TNTFSDiskRecord;
 Current:TNTFSDiskRecord;

 Root:TNTFSDiskAttribute;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=nil;

 if not FEntries.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if AEntry = nil then Exit;
  if AParent = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AddHardlink - Entry = ' + AEntry.Name + ' Parent = ' + AParent.Name + ' Name = ' + AName);
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Version}
  if FVolumeVersion < ntfsNTFS30 then Exit;

  {Check Relative}
  if (AParent.Attributes and (faDot or faDotDot)) <> faNone then Exit;

  {Check Parent}
  if (AParent.Attributes and faMatchMask) <> faDirectory then Exit;

  {Check Target}
  if (AEntry.Attributes and faMatchMask) <> faFile then Exit;

  {Check Existing}
  if GetEntryEx(AParent,AName,faDirectory or faFile,False,False,True) <> nil then Exit;

  {Get Parent}
  if not FRecords.WriterLock then Exit;
  try
   if TNTFSDiskEntry(AParent).Origin = nil then Exit;
   Current:=TNTFSDiskEntry(AParent).Origin.Origin;
   if Current = nil then Exit;

   {Get Origin}
   if TNTFSDiskEntry(AEntry).Origin = nil then Exit;
   Origin:=TNTFSDiskEntry(AEntry).Origin.Origin;
   if Origin = nil then Exit;

   {Add Attribute}
   Attribute:=AddAttribute(Origin,ntfsAttrTypeFileName,ntfsBlankName);
   if Attribute = nil then Exit;
   try
    {Update Attribute}
    TNTFSFileNameAttribute(Attribute).ParentReference:=Current.FileReference;
    TNTFSFileNameAttribute(Attribute).NameSpace:=ntfsNameSpacePosix;
    TNTFSFileNameAttribute(Attribute).FileName:=AName;
    Attribute.Indexed:=ntfsAttributeIndexed;
    Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
    Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

    {Size Record}
    if not SizeRecord(Origin,Origin.CalculatedSize(FVolumeVersion)) then Exit;

    {Add Link}
    Link:=Origin.NewLink(Attribute,nil);
    if Link = nil then Exit;
    try
     {Update Entry}
     Link.LocalLock:=FEntryLocal;
     if not Link.UpdateEntry then Exit;

     {Update Attribute}
     if not Attribute.UpdateAttribute(Link) then Exit;

     {Add Key}
     if not AddKey(Current,Attribute) then Exit;

     {Get Root}
     Root:=Current.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameFileName,ntfsInstanceFirst);
     if Root = nil then Exit;

     {Set Index}
     if not SetAttribute(Current,Root) then Exit;

     {Add Entry}
     if not FEntries.Add(Link,AParent) then Exit;
     try
      //To Do //Add Streams belonging to target link - Or will LoadEntries do this ?

      {Set Records}
      if not SetRecords(Origin) then Exit;

      Result:=Link;

      {Add Reference}
      if AReference then Result.AddReference;
     finally
      if Result = nil then FEntries.Remove(Link);
     end;
    finally
     if Result = nil then Origin.DeleteLink(Link);
     if Result = nil then Link.Free;
    end;
   finally
    if Result = nil then RemoveAttribute(Origin,Attribute);
   end;
  finally
   FRecords.WriterUnlock;
  end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.AddMountPoint(AEntry:TDiskEntry;const ATarget:String):Boolean;
{Entry is the source and must be a folder and must be empty}
var
 PrintName:String;
 SubstituteName:String;

 Reparse:TNTFSReparse;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if AEntry = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AddMountPoint - Entry = ' + AEntry.Name + ' Target = ' + ATarget);
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Version}
  if FVolumeVersion < ntfsNTFS30 then Exit;

  {Check Relative}
  if (AEntry.Attributes and (faDot or faDotDot)) <> faNone then Exit;

  {Check Entry}
  if (AEntry.Attributes and faMatchMask) <> faDirectory then Exit;

  {Get Origin}
  if not FRecords.WriterLock then Exit;
  try
   if TNTFSDiskEntry(AEntry).Origin = nil then Exit;
   Origin:=TNTFSDiskEntry(AEntry).Origin.Origin;
   if Origin = nil then Exit;

   {Get Attribute}
   Attribute:=Origin.GetAttribute(ntfsAttrTypeReparsePoint,ntfsBlankName,ntfsInstanceFirst);
   if Attribute <> nil then Exit;

   {Add Attribute}
   Attribute:=Origin.NewAttribute(nil,ntfsAttrTypeReparsePoint,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;
   try
    {Add Reparse}
    TNTFSReparsePointAttribute(Attribute).NewReparse(ntfsReparseTagMountPoint);
    Reparse:=TNTFSReparsePointAttribute(Attribute).Reparse;
    if Reparse = nil then Exit;

    {Update Reparse}
    PrintName:=ntfsBlankName;
    SubstituteName:=AddTrailingChar(ATarget,GetPathChar);
    if Copy(SubstituteName,1,Length(ntfsReparsePointPrefix)) <> ntfsReparsePointPrefix then SubstituteName:=ntfsReparsePointPrefix + SubstituteName;
    TNTFSReparseMountPoint(Reparse).PrintName:=PrintName;
    TNTFSReparseMountPoint(Reparse).SubstituteName:=SubstituteName;
    TNTFSReparseMountPoint(Reparse).DataSize:=TNTFSReparseMountPoint(Reparse).CalculatedSize(FVolumeVersion);

    {Update Attribute}
    TNTFSReparsePointAttribute(Attribute).ReparseSize:=Reparse.DataSize;
    Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
    Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

    {Add Item}
    if not AddItem(Origin,Attribute) then Exit;
    try
     {Size Record}
     if not SizeRecord(Origin,Origin.CalculatedSize(FVolumeVersion)) then Exit;

     {Add Key}
     if not AddReparse(ntfsReparseTagMountPoint,Origin.FileReference) then Exit;

     {Update Entry}
     AEntry.Attributes:=(AEntry.Attributes or faReparse);
     AEntry.ReparseTag:=ntfsReparseTagMountPoint;

     {Set Entry}
     if not SetEntry(TDiskEntry(AEntry.Parent),AEntry) then Exit;

     Result:=True;
    finally
     if not Result then RemoveItem(Origin,Attribute);
    end;
   finally
    if not Result then RemoveAttribute(Origin,Attribute);
   end;
  finally
   FRecords.WriterUnlock;
  end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.RemoveMountPoint(AEntry:TDiskEntry):Boolean;
var
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if AEntry = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.RemoveMountPoint - Entry = ' + AEntry.Name);
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Version}
  if FVolumeVersion < ntfsNTFS30 then Exit;

  {Check Relative}
  if (AEntry.Attributes and (faDot or faDotDot)) <> faNone then Exit;

  {Check Entry}
  if (AEntry.Attributes and faMatchMask) <> faDirectory then Exit;

  {Get Origin}
  if not FRecords.WriterLock then Exit;
  try
   if TNTFSDiskEntry(AEntry).Origin = nil then Exit;
   Origin:=TNTFSDiskEntry(AEntry).Origin.Origin;
   if Origin = nil then Exit;

   {Get Attribute}
   Attribute:=Origin.GetAttribute(ntfsAttrTypeReparsePoint,ntfsAnyName,ntfsInstanceFirst);
   if Attribute = nil then Exit;

   {Remove Key}
   if not RemoveReparse(TNTFSReparsePointAttribute(Attribute).ReparseTag,Origin.FileReference) then Exit;

   {Remove Attribute}
   if not RemoveAttribute(Origin,Attribute) then Exit;

   {Update Entry}
   AEntry.Attributes:=(AEntry.Attributes and not(faReparse));
   AEntry.ReparseTag:=ntfsReparseTagNone;

   {Set Entry}
   if not SetEntry(TDiskEntry(AEntry.Parent),AEntry) then Exit;

   Result:=True;
  finally
   FRecords.WriterUnlock;
  end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.AddJunctionPoint(AEntry:TDiskEntry;const ATarget:String):Boolean;
{Entry is the source and must be a folder and must be empty}
var
 PrintName:String;
 SubstituteName:String;

 Reparse:TNTFSReparse;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if AEntry = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AddJunctionPoint - Entry = ' + AEntry.Name + ' Target = ' + ATarget);
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Version}
  if FVolumeVersion < ntfsNTFS30 then Exit;

  {Check Relative}
  if (AEntry.Attributes and (faDot or faDotDot)) <> faNone then Exit;

  {Check Entry}
  if (AEntry.Attributes and faMatchMask) <> faDirectory then Exit;

  {Get Origin}
  if not FRecords.WriterLock then Exit;
  try
   if TNTFSDiskEntry(AEntry).Origin = nil then Exit;
   Origin:=TNTFSDiskEntry(AEntry).Origin.Origin;
   if Origin = nil then Exit;

   {Get Attribute}
   Attribute:=Origin.GetAttribute(ntfsAttrTypeReparsePoint,ntfsBlankName,ntfsInstanceFirst);
   if Attribute <> nil then Exit;

   {Add Attribute}
   Attribute:=Origin.NewAttribute(nil,ntfsAttrTypeReparsePoint,ntfsBlankName,FVolumeVersion);
   if Attribute = nil then Exit;
   try
    {Add Reparse}
    TNTFSReparsePointAttribute(Attribute).NewReparse(ntfsReparseTagMountPoint);
    Reparse:=TNTFSReparsePointAttribute(Attribute).Reparse;
    if Reparse = nil then Exit;

    {Update Reparse}
    PrintName:=StripTrailingChar(ATarget,GetPathChar);
    if Copy(PrintName,1,Length(ntfsReparsePointPrefix)) = ntfsReparsePointPrefix then Delete(PrintName,1,Length(ntfsReparsePointPrefix));
    SubstituteName:=StripTrailingChar(ATarget,GetPathChar);
    if Copy(SubstituteName,1,Length(ntfsReparsePointPrefix)) <> ntfsReparsePointPrefix then SubstituteName:=ntfsReparsePointPrefix + SubstituteName;
    TNTFSReparseMountPoint(Reparse).PrintName:=PrintName;
    TNTFSReparseMountPoint(Reparse).SubstituteName:=SubstituteName;
    TNTFSReparseMountPoint(Reparse).DataSize:=TNTFSReparseMountPoint(Reparse).CalculatedSize(FVolumeVersion);

    {Update Attribute}
    TNTFSReparsePointAttribute(Attribute).ReparseSize:=Reparse.DataSize;
    Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
    Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

    {Add Item}
    if not AddItem(Origin,Attribute) then Exit;
    try
     {Size Record}
     if not SizeRecord(Origin,Origin.CalculatedSize(FVolumeVersion)) then Exit;

     {Add Key}
     if not AddReparse(ntfsReparseTagMountPoint,Origin.FileReference) then Exit;

     {Update Entry}
     AEntry.Attributes:=(AEntry.Attributes or faReparse);
     AEntry.ReparseTag:=ntfsReparseTagMountPoint;

     {Set Entry}
     if not SetEntry(TDiskEntry(AEntry.Parent),AEntry) then Exit;

     Result:=True;
    finally
     if not Result then RemoveItem(Origin,Attribute);
    end;
   finally
    if not Result then RemoveAttribute(Origin,Attribute);
   end;
  finally
   FRecords.WriterUnlock;
  end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.RemoveJunctionPoint(AEntry:TDiskEntry):Boolean;
var
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if AEntry = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.RemoveJunctionPoint - Entry = ' + AEntry.Name);
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Version}
  if FVolumeVersion < ntfsNTFS30 then Exit;

  {Check Relative}
  if (AEntry.Attributes and (faDot or faDotDot)) <> faNone then Exit;

  {Check Entry}
  if (AEntry.Attributes and faMatchMask) <> faDirectory then Exit;

  {Get Origin}
  if not FRecords.WriterLock then Exit;
  try
   if TNTFSDiskEntry(AEntry).Origin = nil then Exit;
   Origin:=TNTFSDiskEntry(AEntry).Origin.Origin;
   if Origin = nil then Exit;

   {Get Attribute}
   Attribute:=Origin.GetAttribute(ntfsAttrTypeReparsePoint,ntfsAnyName,ntfsInstanceFirst);
   if Attribute = nil then Exit;

   {Remove Key}
   if not RemoveReparse(TNTFSReparsePointAttribute(Attribute).ReparseTag,Origin.FileReference) then Exit;

   {Remove Attribute}
   if not RemoveAttribute(Origin,Attribute) then Exit;

   {Update Entry}
   AEntry.Attributes:=(AEntry.Attributes and not(faReparse));
   AEntry.ReparseTag:=ntfsReparseTagNone;

   {Set Entry}
   if not SetEntry(TDiskEntry(AEntry.Parent),AEntry) then Exit;

   Result:=True;
  finally
   FRecords.WriterUnlock;
  end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.AddSymbolicLink(AParent:TDiskEntry;const AName,ATarget:String;AFolder,AReference:Boolean):TDiskEntry;
{Name is the source and must not exist, Parent is the location of the SymbolicLink and must be a folder}
{Reference: Add a reference on the returned entry if True}
var
 PrintName:String;
 SubstituteName:String;
 Attributes:LongWord;

 Reparse:TNTFSReparse;
 Entry:TNTFSDiskEntry;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=nil;

 if not FEntries.WriterLock then Exit;
 try
  if AParent = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AddSymbolicLink - Parent = ' + AParent.Name + ' Name = ' + AName + ' Target = ' + ATarget + ' Folder = ' + BoolToStr(AFolder));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Version}
  if FVolumeVersion < ntfsNTFS30 then Exit;

  {Check Relative}
  if (AParent.Attributes and (faDot or faDotDot)) <> faNone then Exit;

  {Check Parent}
  if (AParent.Attributes and faMatchMask) <> faDirectory then Exit;

  {Get Attributes}
  Attributes:=faNone;
  if AFolder then Attributes:=faDirectory;

  {Add Entry}
  Entry:=TNTFSDiskEntry(AddEntry(AParent,AName,Attributes,False));
  if Entry = nil then Exit;
  try
   {Get Origin}
   if not FRecords.WriterLock then Exit;
   try
    if TNTFSDiskEntry(Entry).Origin = nil then Exit;
    Origin:=TNTFSDiskEntry(Entry).Origin.Origin;
    if Origin = nil then Exit;

    {Get Attribute}
    Attribute:=Origin.GetAttribute(ntfsAttrTypeReparsePoint,ntfsBlankName,ntfsInstanceFirst);
    if Attribute <> nil then Exit;

    {Add Attribute}
    Attribute:=Origin.NewAttribute(nil,ntfsAttrTypeReparsePoint,ntfsBlankName,FVolumeVersion);
    if Attribute = nil then Exit;
    try
     {Add Reparse}
     TNTFSReparsePointAttribute(Attribute).NewReparse(ntfsReparseTagSymbolicLink);
     Reparse:=TNTFSReparsePointAttribute(Attribute).Reparse;
     if Reparse = nil then Exit;

     {Update Reparse}
     PrintName:=ATarget;
     if Copy(PrintName,1,Length(ntfsReparsePointPrefix)) = ntfsReparsePointPrefix then Delete(PrintName,1,Length(ntfsReparsePointPrefix));
     SubstituteName:=ATarget;
     if Copy(SubstituteName,1,Length(ntfsReparsePointPrefix)) <> ntfsReparsePointPrefix then SubstituteName:=ntfsReparsePointPrefix + SubstituteName;
     TNTFSReparseSymLink(Reparse).PrintName:=PrintName;
     TNTFSReparseSymLink(Reparse).SubstituteName:=SubstituteName;
     TNTFSReparseSymLink(Reparse).DataSize:=TNTFSReparseSymLink(Reparse).CalculatedSize(FVolumeVersion);

     {Update Attribute}
     TNTFSReparsePointAttribute(Attribute).ReparseSize:=Reparse.DataSize;
     Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
     Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

     {Add Item}
     if not AddItem(Origin,Attribute) then Exit;
     try
      {Size Record}
      if not SizeRecord(Origin,Origin.CalculatedSize(FVolumeVersion)) then Exit;

      {Add Key}
      if not AddReparse(ntfsReparseTagSymbolicLink,Origin.FileReference) then Exit;

      {Update Entry}
      Entry.Attributes:=(Entry.Attributes or faReparse);
      Entry.ReparseTag:=ntfsReparseTagSymbolicLink;

      {Set Entry}
      if not SetEntry(AParent,Entry) then Exit;

      Result:=Entry;

      {Add Reference}
      if AReference then Result.AddReference;
     finally
      if Result = nil then RemoveItem(Origin,Attribute);
     end;
    finally
     if Result = nil then RemoveAttribute(Origin,Attribute);
    end;
   finally
    FRecords.WriterUnlock;
   end;
  finally
   if Result = nil then RemoveEntry(AParent,Entry);
  end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.AddSymbolicLinkEx(AParent:TDiskEntry;const AName,AAltName,ATarget:String;AFolder,AReference:Boolean):TDiskEntry;
{Name is the source and must not exist, Parent is the location of the SymbolicLink and must be a folder}
{Reference: Add a reference on the returned entry if True}
var
 PrintName:String;
 SubstituteName:String;
 Attributes:LongWord;

 Reparse:TNTFSReparse;
 Entry:TNTFSDiskEntry;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=nil;

 if not FEntries.WriterLock then Exit;
 try
  if AParent = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AddSymbolicLinkEx - Parent = ' + AParent.Name + ' Name = ' + AName + ' AltName = ' + AAltName + ' Target = ' + ATarget + ' Folder = ' + BoolToStr(AFolder));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Version}
  if FVolumeVersion < ntfsNTFS30 then Exit;

  {Check Relative}
  if (AParent.Attributes and (faDot or faDotDot)) <> faNone then Exit;

  {Check Parent}
  if (AParent.Attributes and faMatchMask) <> faDirectory then Exit;

  {Get Attributes}
  Attributes:=faNone;
  if AFolder then Attributes:=faDirectory;

  {Add Entry}
  Entry:=TNTFSDiskEntry(AddEntryEx(AParent,AName,AAltName,Attributes,False));
  if Entry = nil then Exit;
  try
   {Get Origin}
   if not FRecords.WriterLock then Exit;
   try
    if TNTFSDiskEntry(Entry).Origin = nil then Exit;
    Origin:=TNTFSDiskEntry(Entry).Origin.Origin;
    if Origin = nil then Exit;

    {Get Attribute}
    Attribute:=Origin.GetAttribute(ntfsAttrTypeReparsePoint,ntfsBlankName,ntfsInstanceFirst);
    if Attribute <> nil then Exit;

    {Add Attribute}
    Attribute:=Origin.NewAttribute(nil,ntfsAttrTypeReparsePoint,ntfsBlankName,FVolumeVersion);
    if Attribute = nil then Exit;
    try
     {Add Reparse}
     TNTFSReparsePointAttribute(Attribute).NewReparse(ntfsReparseTagSymbolicLink);
     Reparse:=TNTFSReparsePointAttribute(Attribute).Reparse;
     if Reparse = nil then Exit;

     {Update Reparse}
     PrintName:=ATarget;
     if Copy(PrintName,1,Length(ntfsReparsePointPrefix)) = ntfsReparsePointPrefix then Delete(PrintName,1,Length(ntfsReparsePointPrefix));
     SubstituteName:=ATarget;
     if Copy(SubstituteName,1,Length(ntfsReparsePointPrefix)) <> ntfsReparsePointPrefix then SubstituteName:=ntfsReparsePointPrefix + SubstituteName;
     TNTFSReparseSymLink(Reparse).PrintName:=PrintName;
     TNTFSReparseSymLink(Reparse).SubstituteName:=SubstituteName;
     TNTFSReparseSymLink(Reparse).DataSize:=TNTFSReparseSymLink(Reparse).CalculatedSize(FVolumeVersion);

     {Update Attribute}
     TNTFSReparsePointAttribute(Attribute).ReparseSize:=Reparse.DataSize;
     Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
     Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

     {Add Item}
     if not AddItem(Origin,Attribute) then Exit;
     try
      {Size Record}
      if not SizeRecord(Origin,Origin.CalculatedSize(FVolumeVersion)) then Exit;

      {Add Key}
      if not AddReparse(ntfsReparseTagSymbolicLink,Origin.FileReference) then Exit;

      {Update Entry}
      Entry.Attributes:=(Entry.Attributes or faReparse);
      Entry.ReparseTag:=ntfsReparseTagSymbolicLink;

      {Set Entry}
      if not SetEntry(AParent,Entry) then Exit;

      Result:=Entry;

      {Add Reference}
      if AReference then Result.AddReference;
     finally
      if Result = nil then RemoveItem(Origin,Attribute);
     end;
    finally
     if Result = nil then RemoveAttribute(Origin,Attribute);
    end;
   finally
    FRecords.WriterUnlock;
   end;
  finally
   if Result = nil then RemoveEntry(AParent,Entry);
  end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.AddRecord(ABase:TNTFSDiskRecord;AFolder:Boolean):TNTFSDiskRecord;
var
 RecordNumber:Int64;

 Origin:TNTFSDiskRecord;
 Current:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=nil;

 if not FRecords.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  {Base may be nil}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Base}
  if ABase = nil then
   begin
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AddRecord - Folder = ' + BoolToStr(AFolder));
    {$ENDIF}

    {Add a Base Record}
    {Allocate Record}
    RecordNumber:=ntfsUnknownRecordNumber;
    if not AllocFileRecord(RecordNumber,False) then Exit;

    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AddRecord - RecordNumber = ' + IntToHex(RecordNumber,16));
    {$ENDIF}

    {Get Record}
    Current:=GetRecordEx(nil,RecordNumber,False,True);
    if Current = nil then Exit;
    try
     {Update Record}
     Current.IsUsed:=True;
     Current.IsFolder:=AFolder;

     {Add Standard Information}
     Attribute:=Current.NewAttribute(nil,ntfsAttrTypeStandardInformation,ntfsBlankName,FVolumeVersion);
     if Attribute = nil then Exit;

     {Update Attribute}
     Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
     Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

     {Check Folder}
     if AFolder then
      begin
       {Size Record}
       if not SizeRecord(Current,Current.CalculatedSize(FVolumeVersion)) then Exit;

       {Add Index}
       if not AddIndex(Current,ntfsAttrTypeFileName,ntfsCollateTypeFileName,ntfsIndexNameFileName) then Exit;
      end
     else
      begin
       {Add Data}
       Attribute:=Current.NewAttribute(nil,ntfsAttrTypeData,ntfsBlankName,FVolumeVersion);
       if Attribute = nil then Exit;

       {Update Attribute}
       Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
       Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

       {Size Record}
       if not SizeRecord(Current,Current.CalculatedSize(FVolumeVersion)) then Exit;
      end;

     Result:=Current;
    finally
     if Result = nil then Current.IsUsed:=False;
    end;
   end
  else
   begin
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AddRecord - Base = ' + IntToHex(ABase.RecordNumber,16) + ' Folder = ' + BoolToStr(AFolder));
    {$ENDIF}

    {Add an Extension Record}
    {Get Origin}
    Origin:=ABase.Origin;
    if Origin = nil then Exit;

    {Allocate Record}
    RecordNumber:=ntfsUnknownRecordNumber;
    if not AllocFileRecord(RecordNumber,(Origin = FMaster)) then Exit;

    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AddRecord - RecordNumber = ' + IntToHex(RecordNumber,16));
    {$ENDIF}

    {Get Record}
    Current:=GetRecordEx(nil,RecordNumber,False,True);
    if Current = nil then Exit;
    try
     {Update Record}
     Current.Base:=Origin;
     Current.IsUsed:=True;
     Current.IsFolder:=Origin.IsFolder;
     {Current.IsUnknown1:=Origin.IsUnknown1;}   {Windows does not mark these flags on extensions}
     {Current.IsIndexView:=Origin.IsIndexView;} {Windows does not mark these flags on extensions}
     Current.Extension:=True;

     {Add Extension}
     Origin.AddRecord(Current);

     {Add Attribute List}
     if not AddList(Origin) then Exit;

     {Size Record}
     if not SizeRecord(Current,Current.CalculatedSize(FVolumeVersion)) then Exit;

     Result:=Current;
    finally
     if Result = nil then
      begin
       Current.IsUsed:=False;
       Current.Base:=nil;
       Origin.RemoveRecord(Current);
      end;
    end;
   end;
 finally
  FRecords.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.RemoveRecord(ARecord:TNTFSDiskRecord):Boolean;
{Note: For multi record files the passed record must be the one to be removed}
{Note: If the passed record is the base record then any extension records will also be removed}
var
 Instance:Integer;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
 Additional:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FRecords.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.RemoveRecord - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Removing}
  if ARecord.Removing then
   begin
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.RemoveRecord - (Remove in Progress)');
    {$ENDIF}

    Result:=True;
   end
  else
   begin
    {Set Removing}
    ARecord.Removing:=True;
    try
     {Get Origin}
     Origin:=ARecord.Origin;
     if Origin = nil then Exit;

     {Check Base}
     if Origin = ARecord then
      begin
       {Remove a Base Record (and all Extensions)}
       {Get Attribute}
       Instance:=1;
       Attribute:=Origin.GetAttribute(ntfsAttrTypeAny,ntfsAnyName,Instance);
       while Attribute <> nil do
        begin
         if (Attribute.AttributeType <> ntfsAttrTypeAttributeList) and (Attribute.AttributeType <> ntfsAttrTypeEnd) then
          begin
           {Get Attribute} {Last Instance} //To Do //If an attribute list exists then instances are sorted by StartVCN //What if there are multiple instances and no list, can that happen ?
           Additional:=Origin.GetAttribute(Attribute.AttributeType,Attribute.AttributeName,ntfsInstanceLast);
           while Additional <> nil do
            begin
             if Additional.StartVCN = 0 then
              begin
               {Remove Attribute}
               if not RemoveAttribute(Origin,Additional) then Exit;
               Break; {Break to allow success}
              end
             else
              begin
               {Remove Attribute}
               if not RemoveAttribute(Origin,Additional) then Exit;
              end;

             {Get Attribute} {Last Instance}
             Additional:=Origin.GetAttribute(Attribute.AttributeType,Attribute.AttributeName,ntfsInstanceLast);
            end;
          end
         else
          begin
           Inc(Instance);
          end;

         {Get Attribute}
         Attribute:=Origin.GetAttribute(ntfsAttrTypeAny,ntfsAnyName,Instance);
        end;

       {Check List}
       if Origin.Overflow then
        begin
         {Get List}
         Attribute:=Origin.GetAttribute(ntfsAttrTypeAttributeList,ntfsBlankName,ntfsInstanceFirst);
         if Attribute = nil then Exit;

         {Remove List}
         if not RemoveList(Origin,Attribute) then Exit;
        end;

       {Check Attributes}
       if Origin.AttributeCount > 1 then Exit;

       {Release Record}
       if not ReleaseFileRecord(Origin.RecordNumber) then Exit;

       Result:=True;
      end
     else
      begin
       {Remove an Extension Record}
       {Check Attributes}
       if ARecord.AttributeCount > 1 then Exit;

       {Remove Extension}
       Origin.RemoveRecord(ARecord);

       {Update Record}
       ARecord.Base:=nil;
       ARecord.Extension:=False;

       {Release Record}
       if not ReleaseFileRecord(ARecord.RecordNumber) then Exit;

       {Check List}
       if (Origin.RecordCount = 1) and not(Origin.Removing) then
        begin
         {Get List}
         Attribute:=Origin.GetAttribute(ntfsAttrTypeAttributeList,ntfsBlankName,ntfsInstanceFirst);
         if Attribute = nil then Exit;

         {Remove List}
         if not RemoveList(Origin,Attribute) then Exit;
        end;

       Result:=True;
      end;
    finally
     {Clear Removing}
     ARecord.Removing:=False;
    end;
   end;
 finally
  FRecords.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.AddAttribute(ARecord:TNTFSDiskRecord;AType:LongWord;const AName:String):TNTFSDiskAttribute;
{Add a new Attribute to the passed Record and update the Attribute List if required}
{Note: Only to be called by AddEntry, SizeEntry etc, lower level functions must call NewAttribute directly}
{Note: Caller must hold the records lock}
var
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=nil;

 if not FRecords.AttributesWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AddAttribute - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' Type = ' + IntToHex(AType,8) + ' Name = ' + AName);
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get Origin}
  Origin:=ARecord.Origin;
  if Origin = nil then Exit;

  {Check Attribute}
  Attribute:=Origin.GetAttribute(AType,AName,ntfsInstanceFirst);
  if (Attribute <> nil) and (Attribute.IsSingle) then Exit;

  {Add Attribute}
  Attribute:=Origin.NewAttribute(nil,AType,AName,FVolumeVersion);
  if Attribute = nil then Exit;
  try
   {Update Attribute}
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add Item}
   if not AddItem(Origin,Attribute) then Exit;
   try
    {Size Record}
    if not SizeRecord(Origin,Origin.CalculatedSize(FVolumeVersion)) then Exit;

    Result:=Attribute;
   finally
    if Result = nil then RemoveItem(Origin,Attribute);
   end;
  finally
   if Result = nil then Origin.RemoveAttribute(Attribute);
  end;
 finally
  FRecords.AttributesWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.RemoveAttribute(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute):Boolean;
{Remove the Attribute from the passed Record, update the Attribute List and free the Run if required}
{Note: For multi instance attribute the passed attribute must be the instance to be removed}
{Note: Only to be called by RemoveEntry, RemoveList, RemoveIndex etc, lower level functions must call Record.RemoveAttribute or RemoveRun directly}
{Note: Caller must hold the records lock}
var
 Count:Int64;
 Origin:TNTFSDiskRecord;
 Current:TNTFSDiskRecord;
begin
 {}
 Result:=False;

 if not FRecords.AttributesWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if AAttribute = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.RemoveAttribute - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' Type = ' + IntToHex(AAttribute.AttributeType,8) + ' Attribute = ' + AAttribute.AttributeName);
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Resident}
  if AAttribute.NonResident = ntfsAttributeResident then
   begin
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.RemoveAttribute - Resident');
    {$ENDIF}

    {Get Origin}
    Origin:=ARecord.Origin;
    if Origin = nil then Exit;

    {Get Record}
    Current:=AAttribute.Parent;
    if Current = nil then Exit;

    {Remove Item}
    if not RemoveItem(Origin,AAttribute) then Exit;

    {Remove Attribute}
    if not Current.RemoveAttribute(AAttribute) then Exit;

    {Size Record}
    if not SizeRecord(Current,Current.CalculatedSize(FVolumeVersion)) then Exit;

    {Check Record} {Remove if only End Attribute remaining}
    if Current.AttributeCount = 1 then if not RemoveRecord(Current) then Exit;

    Result:=True;
   end
  else
   begin
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.RemoveAttribute - NonResident');
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.RemoveAttribute - StartVCN = ' + IntToStr(AAttribute.StartVCN) + ' LastVCN = ' + IntToStr(AAttribute.LastVCN));
    {$ENDIF}

    {Remove Run} {Current Attribute Only}
    {Result:=RemoveRun(ARecord,AAttribute,AAttribute.StartVCN,AAttribute.LastVCN + 1,True);}

    //To Do //Testing4  //Seems OK
    {Get Count} {Current Attribute Only}
    Count:=0;
    if AAttribute.LastVCN <> ntfsUnknownCluster then if not GetRunCount(AAttribute,AAttribute.StartVCN,Count) then Exit;

    {Remove Run} {Current Attribute Only}
    Result:=RemoveRun(ARecord,AAttribute,AAttribute.StartVCN,Count,True); {Modified 25/7/2011}
    //To Do //Testing4  //Seems OK
   end;
 finally
  FRecords.AttributesWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.RenameAttribute(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute;const AName:String):Boolean;
{Rename the Attribute and update the Attribute List if required}
{Note: The passed attribute must be the first instance}
{Note: Caller must hold the records lock}
var
 Instance:Integer;
 Origin:TNTFSDiskRecord;
 Current:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FRecords.AttributesWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if AAttribute = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.RenameAttribute - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' Type = ' + IntToHex(AAttribute.AttributeType,8) + ' Attribute = ' + AAttribute.AttributeName + ' Name = ' + AName);
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get Origin}
  Origin:=ARecord.Origin;
  if Origin = nil then Exit;

  {Check Attribute}
  Attribute:=Origin.GetAttribute(AAttribute.AttributeType,AName,ntfsInstanceFirst);
  if Attribute <> nil then Exit;

  {Get Attribute}
  Instance:=1;
  Attribute:=Origin.GetAttribute(AAttribute.AttributeType,AAttribute.AttributeName,Instance);
  while Attribute <> nil do
   begin
    {Get Record}
    Current:=Attribute.Parent;
    if Current = nil then Exit;

    {Rename Attribute}
    if not Current.RenameAttribute(Attribute,AName) then Exit;

    {Update Attribute}
    Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

    {Rename Item}
    if not RenameItem(Origin,Attribute) then Exit;

    {Size Record}
    if not SizeRecord(Current,Current.CalculatedSize(FVolumeVersion)) then Exit;

    {Get Attribute}
    Inc(Instance);
    Attribute:=Origin.GetAttribute(AAttribute.AttributeType,AAttribute.AttributeName,Instance);
   end;

  Result:=True;
 finally
  FRecords.AttributesWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.MoveAttribute(ASource,ADest:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute):Boolean;
{Move the Attribute from the Source to the Destination Record and update the Attribute List if required}
{Note: For multi instance attribute the passed attribute must be the instance to be moved}
{Note: Caller must hold the records lock}
var
 Origin:TNTFSDiskRecord;
 Current:TNTFSDiskRecord;
begin
 {}
 Result:=False;

 if not FRecords.AttributesWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ASource = nil then Exit;
  if ADest = nil then Exit;
  if ASource = ADest then Exit;
  if AAttribute = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.MoveAttribute - SourceRecord = ' + IntToHex(ASource.RecordNumber,16) + ' DestRecord = ' + IntToHex(ADest.RecordNumber,16) + ' Type = ' + IntToHex(AAttribute.AttributeType,8) + ' Attribute = ' + AAttribute.AttributeName);
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get Origin}
  Origin:=ASource.Origin;
  if Origin = nil then Exit;

  {Check Origin}
  if Origin <> ADest.Origin then Exit;

  {Get Record (Source)}
  Current:=AAttribute.Parent;
  if Current = nil then Exit;

  {Move Attribute}
  if not Current.MoveAttribute(ADest,AAttribute) then Exit;

  {Update Attribute}
  AAttribute.AttributeSize:=AAttribute.CalculatedSize(FVolumeVersion);

  {Move Item}
  if not MoveItem(Current,ADest,AAttribute) then Exit;

  {Size Record (Source)}
  if not SizeRecord(Current,Current.CalculatedSize(FVolumeVersion)) then Exit;

  {Get Record (Dest)}
  Current:=AAttribute.Parent;
  if Current = nil then Exit;

  {Size Record (Dest)}
  if not SizeRecord(Current,Current.CalculatedSize(FVolumeVersion)) then Exit;

  Result:=True;
 finally
  FRecords.AttributesWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.AddRun(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute;const ACount:Int64;AMft,ASparse:Boolean):Boolean;
{Add count clusters to the runs of the supplied attribute}
{Count is the number of clusters to be added}
{Handles adding of multiple attribute instances if needed}
{Note: The passed attribute must be the first instance}
{Note: Updates AttributeSize value of Attribute}
{Note: Caller must hold the records and attributes lock}
var
 Size:LongWord;

 Instance:LongWord;
 Current:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FRecords.RunsWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if AAttribute = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AddRun - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' Type = ' + IntToHex(AAttribute.AttributeType,8) + ' Attribute = ' + AAttribute.AttributeName + ' Count = ' + IntToStr(ACount) + ' MFT = ' + BoolToStr(AMft) + ' Sparse = ' + BoolToStr(ASparse));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Resident}
  if AAttribute.NonResident = ntfsAttributeNonResident then
   begin
    {Check Count}
    if ACount = 0 then Exit;

    {Check Size}
    if AAttribute.StreamAllocated = 0 then
     begin
      {Get Attribute}
      Attribute:=ARecord.GetAttribute(AAttribute.AttributeType,AAttribute.AttributeName,ntfsInstanceFirst);
     end
    else
     begin
      {Get Attribute}
      Instance:=ntfsInstanceFirst;
      Attribute:=ARecord.GetAttributeByVCN(AAttribute,(AAttribute.StreamAllocated shr FClusterShiftCount) - 1,Instance);
     end;
    if Attribute = nil then Exit;

    {Alloc Run}
    if not AllocRun(Attribute,ACount,AMft,ASparse) then Exit;

    {Get Record}
    Current:=Attribute.Parent;
    if Current = nil then Exit;

    {Update Attribute}
    Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

    {Get Size}
    Size:=Current.AttributeSize(FVolumeVersion,Attribute.AttributeType);

    {Check Attribute}
    if (Attribute.AttributeSize < Size) or (Attribute.RunCount < 3) then
     begin
      {Size Record}
      if not SizeRecord(Current,Current.CalculatedSize(FVolumeVersion)) then Exit;

      Result:=True;
     end
    else
     begin
      {Check Single}
      if Attribute.IsSingle then ReleaseRun(Attribute,ACount);
      if Attribute.IsSingle then Exit;

      {Size Record}
      if not SizeRecord(Current,Current.CalculatedSize(FVolumeVersion)) then Exit;

      Result:=True;
     end;
   end;
 finally
  FRecords.RunsWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.RemoveRun(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute;const AVCN,ACount:Int64;ADelete:Boolean):Boolean;
{Remove count clusters from the runs of the supplied attribute}
{VCN is the starting cluster for the remove}
{Count is the number of clusters to be remove}
{Delete indicates whether the attribute should be removed}
{Handles removing of multiple attribute instances if needed}
{Note: For multi instance attribute the passed attribute must be the instance to be removed}
{Note: Updates AttributeSize value of Attribute}
{Note: Caller must hold the records and attributes lock}
var
 Current:TNTFSDiskRecord;
begin
 {}
 Result:=False;

 if not FRecords.RunsWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if AAttribute = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.RemoveRun - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' Type = ' + IntToHex(AAttribute.AttributeType,8) + ' Attribute = ' + AAttribute.AttributeName + ' VCN = ' + IntToStr(AVCN) + ' Count = ' + IntToStr(ACount) + ' Delete = ' + BoolToStr(ADelete));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Resident}
  if AAttribute.NonResident = ntfsAttributeNonResident then
   begin
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.RemoveRun - NonResident');
    {$ENDIF}

    {Check Count}
    if ACount = 0 then {Allow Zero}
     begin
      {Check Delete}
      if ADelete then
       begin
        {Get Record}
        Current:=AAttribute.Parent;
        if Current = nil then Exit;

        {Remove Item}
        if not RemoveItem(ARecord,AAttribute) then Exit;

        {Remove Attribute}
        if not Current.RemoveAttribute(AAttribute) then Exit;

        {Size Record}
        if not SizeRecord(Current,Current.CalculatedSize(FVolumeVersion)) then Exit;

        {Check Record} {Remove if only End Attribute remaining}
        if Current.AttributeCount = 1 then if not RemoveRecord(Current) then Exit;
       end;

      Result:=True;
     end
    else
     begin
      {$IFDEF NTFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.RemoveRun - AAttribute.StartVCN = ' + IntToStr(AAttribute.StartVCN));
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.RemoveRun - AAttribute.LastVCN = ' + IntToStr(AAttribute.LastVCN));
      {$ENDIF}

      {Check Start}
      if AVCN < AAttribute.StartVCN then Exit;
      if AVCN > AAttribute.LastVCN then Exit;

      {Check Count}
      if (AVCN + ACount) < (AAttribute.LastVCN + 1) then Exit;
      if (AVCN + ACount) > (AAttribute.LastVCN + 1) then Exit;

      {Check Start}
      if AVCN > AAttribute.StartVCN then
       begin
        {Release Run}
        if not ReleaseRun(AAttribute,ACount) then Exit;

        {Get Record}
        Current:=AAttribute.Parent;
        if Current = nil then Exit;

        {Update Attribute}
        AAttribute.AttributeSize:=AAttribute.CalculatedSize(FVolumeVersion);

        {Size Record}
        if not SizeRecord(Current,Current.CalculatedSize(FVolumeVersion)) then Exit;

        Result:=True;
       end
      else
       begin
        {Check Delete}
        if ADelete then
         begin
          {Release Run}
          if not ReleaseRun(AAttribute,ACount) then Exit;

          {Get Record}
          Current:=AAttribute.Parent;
          if Current = nil then Exit;

          {Remove Item}
          if not RemoveItem(ARecord,AAttribute) then Exit;

          {Remove Attribute}
          if not Current.RemoveAttribute(AAttribute) then Exit;

          {Size Record}
          if not SizeRecord(Current,Current.CalculatedSize(FVolumeVersion)) then Exit;

          {Check Record} {Remove if only End Attribute remaining}
          if Current.AttributeCount = 1 then if not RemoveRecord(Current) then Exit;
         end
        else
         begin
          {Release Run}
          if not ReleaseRun(AAttribute,ACount) then Exit;

          {Get Record}
          Current:=AAttribute.Parent;
          if Current = nil then Exit;

          {Update Attribute}
          AAttribute.AttributeSize:=AAttribute.CalculatedSize(FVolumeVersion);

          {Size Record}
          if not SizeRecord(Current,Current.CalculatedSize(FVolumeVersion)) then Exit;
         end;

        Result:=True;
       end;
     end;
   end;
 finally
  FRecords.RunsWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.MoveRun(ARecord:TNTFSDiskRecord;ASource,ADest:TNTFSDiskAttribute;const ACount:Int64):Boolean; {Not override}
{Move count or more clusters from the runs of the source attribute to the dest attribute}
{Count is the number of clusters to be moved}
{Runs will be removed from the end and added to the start}
{Source must contain more than one (plus Last) run}
{Note: Caller must hold the records and attributes lock}
var
 Count:Int64;
 Remain:Int64;
 StartVCN:Int64;

 Run:TNTFSDiskRun;
 Current:TNTFSDiskRecord;
begin
 {}
 Result:=False;

 if not FRecords.RunsWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if ASource = nil then Exit;
  if ADest = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.MoveRun - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' Type = ' + IntToHex(ASource.AttributeType,8) + ' Source = ' + ASource.AttributeName + ' Dest = ' + ADest.AttributeName + ' Count = ' + IntToStr(ACount));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Count}
  if ACount = 0 then Exit;

  {Check Source}
  if ASource.RunCount < 3 then Exit;

  {Check Resident}
  if ASource.NonResident = ntfsAttributeNonResident then
   begin
    {Check Resident}
    if ADest.NonResident = ntfsAttributeNonResident then
     begin
      {Get Position}
      Remain:=ACount;
      while (Remain > 0) and (ASource.RunCount > 2) do
       begin
        {Get Run}
        Run:=ASource.GetRun(ASource.LastVCN,StartVCN);
        if Run = nil then Exit;

        {Get Count}
        Count:=Remain;
        if Count > Run.Length then Count:=Run.Length;

        {Check Length}
        if Count < Run.Length then
         begin
          if not ASource.SplitRun(Run,Run.Length - Count) then Exit;

          {Do not update position}
         end
        else
         begin
          {Move Run}
          if not ASource.MoveRun(ADest,Run) then Exit;

          {Update Position}
          Dec(Remain,Count);
         end;
       end;

      {Update Source}
      ASource.AttributeSize:=ASource.CalculatedSize(FVolumeVersion);

      {Update Dest}
      ADest.AttributeSize:=ADest.CalculatedSize(FVolumeVersion);

      {Get Record (Source)}
      Current:=ASource.Parent;
      if Current = nil then Exit;

      {Size Record (Source)}
      if not SizeRecord(Current,Current.CalculatedSize(FVolumeVersion)) then Exit;

      {Get Record (Dest)}
      Current:=ADest.Parent;
      if Current = nil then Exit;

      {Size Record (Dest)}
      if not SizeRecord(Current,Current.CalculatedSize(FVolumeVersion)) then Exit;

      {Set Item (Dest)}
      if not SetItem(Current,ADest) then Exit;

      Result:=True;
     end;
   end;
 finally
  FRecords.RunsWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.AddList(ARecord:TNTFSDiskRecord):Boolean;
{Add an Attribute List to the base record of the passed record}
{Should only ever be called by AddRecord}
{Note: Caller must hold the records lock}
var
 Instance:Integer;
 Item:TNTFSDiskItem;
 Items:TNTFSDiskItems;
 Origin:TNTFSDiskRecord;
 List:TNTFSDiskAttribute;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FRecords.AttributesWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get Origin}
  Origin:=ARecord.Origin;
  if Origin = nil then Exit;

  {Check List}
  if not(Origin.Overflow) then
   begin
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AddList - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16));
    {$ENDIF}

    {Add List}
    List:=Origin.NewAttribute(nil,ntfsAttrTypeAttributeList,ntfsBlankName,FVolumeVersion);
    if List = nil then Exit;

    {Add Items}
    Instance:=1;
    Attribute:=Origin.GetAttribute(ntfsAttrTypeAny,ntfsAnyName,Instance);
    while Attribute <> nil do
     begin
      if not(Attribute.IsUnlisted) then
       begin
        {Add Item}
        Item:=List.NewItem(Attribute);
        if Item = nil then Exit;

        {Update Item}
        Item.ItemSize:=Item.CalculatedSize(FVolumeVersion);
       end;

      {Get Attribute}
      Inc(Instance);
      Attribute:=Origin.GetAttribute(ntfsAttrTypeAny,ntfsAnyName,Instance);
     end;

    {Get Items}
    Items:=List.Items;
    if Items = nil then Exit;

    {Update Items}
    Items.Loaded:=True;

    {Update Record}
    Origin.Overflow:=True;

    {Size Attribute}
    if not SizeAttribute(Origin,List,List.CalculatedStreamSize(FVolumeVersion)) then Exit;

    {Size attribute will call size record}

    {Set Attribute}
    if not SetAttribute(Origin,List) then Exit;
   end;

  Result:=True;
 finally
  FRecords.AttributesWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.RemoveList(ARecord:TNTFSDiskRecord;AList:TNTFSDiskAttribute):Boolean;
{Remove the Attribute List from the base record of the passed record}
{Should only ever be called by RemoveRecord (Can be called by RemoveItem but actually cannot occur)}
{Note: Caller must hold the records lock}
var
 Item:TNTFSDiskItem;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FRecords.AttributesWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if AList = nil then Exit;

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get Origin}
  Origin:=ARecord.Origin;
  if Origin = nil then Exit;

  {Check List}
  if Origin.Overflow then
   begin
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.RemoveList - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16));
    {$ENDIF}

    {Remove Items}
    Item:=AList.GetItem(ntfsAttrTypeAny,ntfsAnyName,ntfsInstanceFirst);
    while Item <> nil do
     begin
      {Remove Item}
      if not AList.RemoveItem(Item) then Exit;

      {Get Item}
      Item:=AList.GetItem(ntfsAttrTypeAny,ntfsAnyName,ntfsInstanceFirst);
     end;

    {Remove Attribute} {Is an unlisted attribute so will not try to RemoveItem} {Is single instance so no need to call get attribute}
    if not RemoveAttribute(Origin,AList) then Exit;

    {Remove attribute will call size record}

    {Update Record}
    Origin.Overflow:=False;
   end;

  Result:=True;
 finally
  FRecords.AttributesWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.AddIndex(ARecord:TNTFSDiskRecord;AType,ARule:LongWord;const AName:String):Boolean;
{Add an Index Root to the passed record}
{Note: Caller must hold the records lock}
var
 Index:TNTFSDiskIndex;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FRecords.IndexWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AddIndex - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' Type = ' + IntToHex(AType,8) + ' Rule = ' + IntToHex(ARule,8) + ' Name = ' + AName);
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get Origin}
  Origin:=ARecord.Origin;
  if Origin = nil then Exit;

  {Check Attribute}
  Attribute:=Origin.GetAttribute(ntfsAttrTypeIndexRoot,AName,ntfsInstanceFirst);
  if Attribute <> nil then Exit;

  {Add Attribute}
  Attribute:=Origin.NewAttribute(nil,ntfsAttrTypeIndexRoot,AName,FVolumeVersion);
  if Attribute = nil then Exit;
  try
   {Add Index}
   if not Attribute.NewIndex(FVolumeVersion,FSectorSize,AType,ARule,FIndexRecordSize,FIndexCounterOffset) then Exit;
   Index:=Attribute.Index;
   if Index = nil then Exit;

   {Update Index}
   Index.ClustersPerIndex:=FClustersPerIndex;
   Index.IndexsPerCluster:=FIndexsPerCluster;
   Index.IndexCounterShift:=FIndexCounterShift;
   Index.IndexRecordShiftCount:=FIndexRecordShiftCount;
   Index.IndexRecordOffsetMask:=FIndexRecordOffsetMask;
   Index.Loaded:=True;

   {Update Index}
   Index.UpCase:=FUpCases;
   Index.CompareSecurityDescriptor:=CompareSecurityDescriptor;

   {Update Attribute}
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

   {Add Item}
   if not AddItem(Origin,Attribute) then Exit;
   try
    {Size Record}
    if not SizeRecord(Origin,Origin.CalculatedSize(FVolumeVersion)) then Exit;

    Result:=True;
   finally
    if not Result then RemoveItem(Origin,Attribute);
   end;
  finally
   if not Result then Origin.RemoveAttribute(Attribute);
  end;
 finally
  FRecords.IndexWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.RemoveIndex(ARecord:TNTFSDiskRecord;ARoot:TNTFSDiskAttribute):Boolean;
{Remove the Index Root, Allocation and Bitmap from the passed record}
{Note: Caller must hold the records lock}
var
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FRecords.IndexWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if ARoot = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.RemoveIndex - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' Type = ' + IntToHex(ARoot.AttributeType,8) + ' Root = ' + ARoot.AttributeName);
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get Origin}
  Origin:=ARecord.Origin;
  if Origin = nil then Exit;

  {Get Bitmap} {Last Instance} //To Do //If an attribute list exists then instances are sorted by StartVCN //What if there are multiple instances and no list, can that happen ?
  Attribute:=Origin.GetAttribute(ntfsAttrTypeBitmap,ARoot.AttributeName,ntfsInstanceLast);
  while Attribute <> nil do
   begin
    {Remove Attribute}
    if not RemoveAttribute(Origin,Attribute) then Exit;

    {Get Attribute} {Last Instance}
    Attribute:=Origin.GetAttribute(ntfsAttrTypeBitmap,ARoot.AttributeName,ntfsInstanceLast);
   end;

  {Get Allocation} {Last Instance} //To Do //If an attribute list exists then instances are sorted by StartVCN //What if there are multiple instances and no list, can that happen ?
  Attribute:=Origin.GetAttribute(ntfsAttrTypeIndexAllocation,ARoot.AttributeName,ntfsInstanceLast);
  while Attribute <> nil do
   begin
    {Remove Attribute}
    if not RemoveAttribute(Origin,Attribute) then Exit;

    {Get Attribute} {Last Instance}
    Attribute:=Origin.GetAttribute(ntfsAttrTypeIndexAllocation,ARoot.AttributeName,ntfsInstanceLast);
   end;

  {Remove Root} {Is single instance so no need to call get attribute}
  if not RemoveAttribute(Origin,ARoot) then Exit;

  {Remove attribute will call size record}
  Result:=True;
 finally
  FRecords.IndexWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.AddNode(ARecord:TNTFSDiskRecord;AAllocation,ABitmap:TNTFSDiskAttribute;AIndex:TNTFSDiskIndex;ANode:TNTFSDiskNode):Boolean;
{Note: The caller must ensure the passed node is not the root node (Root node is always resident)}
{Note: Caller must hold the records and index lock}
var
 Key:TNTFSDiskKey;

 RecordNumber:Int64;
begin
 {}
 Result:=False;

 if not FRecords.NodesWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if AAllocation = nil then Exit;
  if ABitmap = nil then Exit;
  if AIndex = nil then Exit;
  if ANode = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AddNode - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' NodeNumber = ' + IntToHex(ANode.RecordNumber,16));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Allocate Record}
  RecordNumber:=ntfsUnknownRecordNumber;
  if not AllocIndexRecord(ARecord,AAllocation,ABitmap,AIndex,RecordNumber) then Exit;

  {Update Node}
  ANode.RecordNumber:=RecordNumber;
  Result:=True;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AddNode - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' NodeNumber = ' + IntToHex(ANode.RecordNumber,16));
  {$ENDIF}
 finally
  FRecords.NodesWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.RemoveNode(ARecord:TNTFSDiskRecord;AAllocation,ABitmap:TNTFSDiskAttribute;AIndex:TNTFSDiskIndex;ANode:TNTFSDiskNode):Boolean;
{Note: The caller must ensure the passed node is not the root node (Root node is always resident)}
{Note: Caller must hold the records and index lock}
var
 Key:TNTFSDiskKey;
begin
 {}
 Result:=False;

 if not FRecords.NodesWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if AAllocation = nil then Exit;
  if ABitmap = nil then Exit;
  if AIndex = nil then Exit;
  if ANode = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.RemoveNode - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' NodeNumber = ' + IntToHex(ANode.RecordNumber,16));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Release Record}
  if not ReleaseIndexRecord(ARecord,AAllocation,ABitmap,AIndex,ANode.RecordNumber) then Exit;

  {Update Node}
  ANode.RecordNumber:=ntfsUnknownRecordNumber;

  Result:=True;

  //To Do
  //When we release an index record should we
  // a) check if it is the last record and if so shrink the run
  // b) convert the clusters occupied by that record to sparse ?
  //     would Windows accept this ?
  //     need to check if IndexRecordSize >= ClusterSize
  // c) check if all index records have been released and if so release the run down to just one record ?

  // a or c is probably safest ? (Windows does not appear to ever release the run)

  // c seems to be the most likely choice

  //Where should this be done ? Here or ReleaseIndexRecord ? Probably ReleaseIndexRecord where we expand the run
 finally
  FRecords.NodesWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.AddItem(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute):Boolean;
{Add an item referencing the supplied attribute to the list}
{Note: Caller must hold the records lock}
var
 Item:TNTFSDiskItem;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FRecords.AttributesWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if AAttribute = nil then Exit;

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get Origin}
  Origin:=ARecord.Origin;
  if Origin = nil then Exit;

  {Check List}
  if (Origin.Overflow) and not(AAttribute.IsUnlisted) then
   begin
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AddItem - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' Type = ' + IntToHex(AAttribute.AttributeType,8) + ' Attribute = ' + AAttribute.AttributeName);
    {$ENDIF}

    {Check Removing}
    if Origin.Removing then
     begin
      {$IFDEF NTFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AddItem - (Remove in Progress)');
      {$ENDIF}
     end
    else
     begin
      {Get List}
      Attribute:=Origin.GetAttribute(ntfsAttrTypeAttributeList,ntfsAnyName,ntfsInstanceFirst);
      if Attribute = nil then Exit;

      {Get Item}
      Item:=Attribute.GetItemByAttribute(AAttribute);
      if Item <> nil then Exit;

      {Add Item}
      Item:=Attribute.NewItem(AAttribute);
      if Item = nil then Exit;

      {Update Item}
      Item.ItemSize:=Item.CalculatedSize(FVolumeVersion);

      {Size Attribute}
      if not SizeAttribute(Origin,Attribute,Attribute.CalculatedStreamSize(FVolumeVersion)) then Exit;

      {Set Attribute}
      if not SetAttribute(Origin,Attribute) then Exit;
     end;
   end;

  Result:=True;
 finally
  FRecords.AttributesWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.RemoveItem(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute):Boolean;
{Remove the item referencing the supplied attribute from the list}
{Note: Caller must hold the records lock}
var
 Item:TNTFSDiskItem;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FRecords.AttributesWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if AAttribute = nil then Exit;

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get Origin}
  Origin:=ARecord.Origin;
  if Origin = nil then Exit;

  {Check List}
  if (Origin.Overflow) and not(AAttribute.IsUnlisted) then
   begin
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.RemoveItem - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' Type = ' + IntToHex(AAttribute.AttributeType,8) + ' Attribute = ' + AAttribute.AttributeName);
    {$ENDIF}

    {Get List}
    Attribute:=Origin.GetAttribute(ntfsAttrTypeAttributeList,ntfsAnyName,ntfsInstanceFirst);
    if Attribute = nil then Exit;

    {Get Item}
    Item:=Attribute.GetItemByAttribute(AAttribute);
    if Item = nil then Exit;

    {Remove Item}
    if not Attribute.RemoveItem(Item) then Exit;

    {Check Removing} {Different to other Item methods as we must still remove and free the item}
    if Origin.Removing then
     begin
      {$IFDEF NTFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.RemoveItem - (Remove in Progress)');
      {$ENDIF}
     end
    else
     begin
      {Size Attribute}
      if not SizeAttribute(Origin,Attribute,Attribute.CalculatedStreamSize(FVolumeVersion)) then Exit;

      {Set Attribute}
      if not SetAttribute(Origin,Attribute) then Exit;

      {Check List} {List does not have an End Attribute item} {This should never happen due to Standard Information}
      if Attribute.ItemCount = 0 then if not RemoveList(Origin,Attribute) then Exit;
     end;
   end;

  Result:=True;
 finally
  FRecords.AttributesWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.RenameItem(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute):Boolean;
{Rename the item referencing the supplied attribute in the list}
{Note: Caller must hold the records lock}
var
 Item:TNTFSDiskItem;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FRecords.AttributesWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if AAttribute = nil then Exit;

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get Origin}
  Origin:=ARecord.Origin;
  if Origin = nil then Exit;

  {Check List}
  if (Origin.Overflow) and not(AAttribute.IsUnlisted) then
   begin
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.RenameItem - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' Type = ' + IntToHex(AAttribute.AttributeType,8) + ' Attribute = ' + AAttribute.AttributeName);
    {$ENDIF}

    {Check Removing}
    if Origin.Removing then
     begin
      {$IFDEF NTFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.RenameItem - (Remove in Progress)');
      {$ENDIF}
     end
    else
     begin
      {Get List}
      Attribute:=Origin.GetAttribute(ntfsAttrTypeAttributeList,ntfsAnyName,ntfsInstanceFirst);
      if Attribute = nil then Exit;

      {Get Item}
      Item:=Attribute.GetItemByAttribute(AAttribute);
      if Item = nil then Exit;

      {Rename Item}
      if not Attribute.RenameItem(Item,AAttribute) then Exit;

      {Update Item}
      Item.ItemSize:=Item.CalculatedSize(FVolumeVersion);

      {Size Attribute}
      if not SizeAttribute(Origin,Attribute,Attribute.CalculatedStreamSize(FVolumeVersion)) then Exit;

      {Set Attribute}
      if not SetAttribute(Origin,Attribute) then Exit;
     end;
   end;

  Result:=True;
 finally
  FRecords.AttributesWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.MoveItem(ASource,ADest:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute):Boolean; {Not override}
{Move the item referencing the supplied attribute in the list}
{Note: Caller must hold the records lock}
var
 Item:TNTFSDiskItem;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FRecords.AttributesWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ASource = nil then Exit;
  if ADest = nil then Exit;
  if AAttribute = nil then Exit;

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get Origin}
  Origin:=ASource.Origin;
  if Origin = nil then Exit;

  {Check Origin}
  if Origin <> ADest.Origin then Exit;

  {Check List}
  if (Origin.Overflow) and not(AAttribute.IsUnlisted) then
   begin
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.MoveItem - SourceRecord = ' + IntToHex(ASource.RecordNumber,16) + ' DestRecord = ' + IntToHex(ADest.RecordNumber,16) + ' Type = ' + IntToHex(AAttribute.AttributeType,8) + ' Attribute = ' + AAttribute.AttributeName);
    {$ENDIF}

    {Check Removing}
    if Origin.Removing then
     begin
      {$IFDEF NTFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.MoveItem - (Remove in Progress)');
      {$ENDIF}
     end
    else
     begin
      {Get List}
      Attribute:=Origin.GetAttribute(ntfsAttrTypeAttributeList,ntfsAnyName,ntfsInstanceFirst);
      if Attribute = nil then Exit;

      {Get Item}
      Item:=Attribute.GetItemByAttribute(AAttribute);
      if Item = nil then Exit;

      {Move Item}
      if not Attribute.MoveItem(Item,AAttribute) then Exit;

      {Update Item}
      Item.ItemSize:=Item.CalculatedSize(FVolumeVersion);

      {Size Attribute}
      if not SizeAttribute(Origin,Attribute,Attribute.CalculatedStreamSize(FVolumeVersion)) then Exit;

      {Set Attribute}
      if not SetAttribute(Origin,Attribute) then Exit;
     end;
   end;

  Result:=True;
 finally
  FRecords.AttributesWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.AddKey(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute):Boolean;
{Insert the Key for the FileName attribute in the $I30 index of the supplied record}
{Note: Caller must hold the records lock}
var
 Origin:TNTFSDiskRecord;

 Key:TNTFSAttributeKey;
 Root:TNTFSDiskAttribute;
 Index:TNTFSAttributeIndex;
begin
 {}
 Result:=False;

 if not FRecords.AttributesWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if AAttribute = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AddKey - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' Attribute = ' + AAttribute.AttributeName);
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Type}
  if AAttribute.AttributeType = ntfsAttrTypeFileName then
   begin
    {Get Origin}
    Origin:=ARecord.Origin;
    if Origin = nil then Exit;

    {Get Attribute (Index Root)}
    Root:=Origin.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameFileName,ntfsInstanceFirst);
    if Root = nil then Exit;

    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AddKey - Root = ' + Root.AttributeName + ' Type = ' + IntToHex(Root.AttributeType,8) + ' Id = ' + IntToHex(Root.AttributeId,4));
    {$ENDIF}

    {Get Index}
    Index:=TNTFSAttributeIndex(Root.Index);
    if Index = nil then Exit;

    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AddKey - IndexType = ' + IntToStr(Index.IndexType) + ' CollateRule = ' + IntToStr(Index.CollateRule) + ' Order = ' + IntToStr(Index.Order) + ' Median = ' + IntToStr(Index.Median));
    {$ENDIF}

    {Get Key}
    Key:=TNTFSAttributeKey(Index.GetKeyByAttribute(AAttribute));
    if Key <> nil then Exit;

    {Add Key}
    Key:=TNTFSAttributeKey(Index.NewKeyByAttribute(AAttribute));
    if Key = nil then Exit;
    try
     {Update Key}
     Key.EntrySize:=Key.CalculatedSize(FVolumeVersion);

     {Insert Key}
     if not Index.InsertKey(Key) then Exit;

     {$IFDEF NTFS_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.AddKey - Key = ' + IntToHex(Key.FileReference,16));
     {$ENDIF}

     Result:=True;
    finally
     if not Result then Index.DestroyKey(Key);
    end;
   end;
 finally
  FRecords.AttributesWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.RemoveKey(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute):Boolean;
{Remove the Key for the FileName attribute in the $I30 index of the supplied record}
{Note: Caller must hold the records lock}
var
 Origin:TNTFSDiskRecord;

 Key:TNTFSAttributeKey;
 Root:TNTFSDiskAttribute;
 Index:TNTFSAttributeIndex;
begin
 {}
 Result:=False;

 if not FRecords.AttributesWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if AAttribute = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.RemoveKey - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' Attribute = ' + AAttribute.AttributeName);
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Type}
  if AAttribute.AttributeType = ntfsAttrTypeFileName then
   begin
    {Get Origin}
    Origin:=ARecord.Origin;
    if Origin = nil then Exit;

    {Get Attribute (Index Root)}
    Root:=Origin.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameFileName,ntfsInstanceFirst);
    if Root = nil then Exit;

    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.RemoveKey - Root = ' + Root.AttributeName + ' Type = ' + IntToHex(Root.AttributeType,8) + ' Id = ' + IntToHex(Root.AttributeId,4));
    {$ENDIF}

    {Get Index}
    Index:=TNTFSAttributeIndex(Root.Index);
    if Index = nil then Exit;

    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.RemoveKey - IndexType = ' + IntToStr(Index.IndexType) + ' CollateRule = ' + IntToStr(Index.CollateRule) + ' Order = ' + IntToStr(Index.Order) + ' Median = ' + IntToStr(Index.Median));
    {$ENDIF}

    {Get Key}
    Key:=TNTFSAttributeKey(Index.GetKeyByAttribute(AAttribute));
    if Key = nil then Exit;

    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.RemoveKey - Key = ' + IntToHex(Key.FileReference,16));
    {$ENDIF}

    {Remove Key}
    if not Index.RemoveKey(Key) then Exit;

    Result:=True;
   end;
 finally
  FRecords.AttributesWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.RenameKey(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute;const AName:String):Boolean;
{Remove and reinsert the Key for the FileName attribute in the $I30 index of the supplied record}
{Note: Passed name is the old name of the attribute}
{Note: Caller must hold the records lock}
var
 Origin:TNTFSDiskRecord;

 Key:TNTFSAttributeKey;
 Root:TNTFSDiskAttribute;
 Index:TNTFSAttributeIndex;
begin
 {}
 Result:=False;

 if not FRecords.AttributesWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if AAttribute = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.RenameKey - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' Attribute = ' + AAttribute.AttributeName + ' Name = ' + AName);
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Type}
  if AAttribute.AttributeType = ntfsAttrTypeFileName then
   begin
    {Get Origin}
    Origin:=ARecord.Origin;
    if Origin = nil then Exit;

    {Get Attribute (Index Root)}
    Root:=Origin.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameFileName,ntfsInstanceFirst);
    if Root = nil then Exit;

    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.RenameKey - Root = ' + Root.AttributeName + ' Type = ' + IntToHex(Root.AttributeType,8) + ' Id = ' + IntToHex(Root.AttributeId,4));
    {$ENDIF}

    {Get Index}
    Index:=TNTFSAttributeIndex(Root.Index);
    if Index = nil then Exit;

    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.RenameKey - IndexType = ' + IntToStr(Index.IndexType) + ' CollateRule = ' + IntToStr(Index.CollateRule) + ' Order = ' + IntToStr(Index.Order) + ' Median = ' + IntToStr(Index.Median));
    {$ENDIF}

    {Get Key} {Old Name}
    Key:=TNTFSAttributeKey(Index.GetKeyByFileName(AName));
    if Key = nil then Exit;

    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.RenameKey - Key = ' + IntToHex(Key.FileReference,16));
    {$ENDIF}

    {Remove Key}
    if not Index.RemoveKey(Key) then Exit;

    {Get Key} {New Name}
    Key:=TNTFSAttributeKey(Index.GetKeyByAttribute(AAttribute));
    if Key <> nil then Exit;

    {Add Key}
    Key:=TNTFSAttributeKey(Index.NewKeyByAttribute(AAttribute));
    if Key = nil then Exit;
    try
     {Update Key}
     Key.EntrySize:=Key.CalculatedSize(FVolumeVersion);

     {Insert Key}
     if not Index.InsertKey(Key) then Exit;

     {$IFDEF NTFS_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.RenameKey - Key = ' + IntToHex(Key.FileReference,16));
     {$ENDIF}

     Result:=True;
    finally
     if not Result then Index.DestroyKey(Key);
    end;
   end;
 finally
  FRecords.AttributesWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.MoveKey(ASource,ADest:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute):Boolean;
{Remove the Key for the FileName attribute in the $I30 index of the source record and reinsert in the dest record}
{Note: Caller must hold the records lock}
begin
 {}
 Result:=False;

 if not FRecords.AttributesWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ADest = nil then Exit;
  if ASource = nil then Exit;
  if AAttribute = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.MoveKey - SourceRecord = ' + IntToHex(ASource.RecordNumber,16) + ' DestRecord = ' + IntToHex(ADest.RecordNumber,16)  + ' Attribute = ' + AAttribute.AttributeName);
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Type}
  if AAttribute.AttributeType = ntfsAttrTypeFileName then
   begin
    {Remove Source}
    if not RemoveKey(ASource,AAttribute) then Exit;

    {Add Dest}
    if not AddKey(ADest,AAttribute) then Exit;

    Result:=True;
   end;
 finally
  FRecords.AttributesWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.SetRecords(ARecord:TNTFSDiskRecord):Boolean;
var
 Instance:Integer;
 Current:TNTFSDiskRecord;
begin
 {}
 Result:=False;

 if not FRecords.WriterLock then Exit;
 try
  if ARecord = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SetRecords - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Base}
  if ARecord.Base <> nil then
   begin
    Result:=SetRecords(ARecord.Base);
   end
  else
   begin
    {Get Record}
    Instance:=1;
    Current:=ARecord.GetRecord(Instance);
    while Current <> nil do
     begin
      {Set Record}
      if not SetRecord(Current) then Exit;

      {Get Record}
      Inc(Instance);
      Current:=ARecord.GetRecord(Instance);
     end;

    Result:=True;
   end;
 finally
  FRecords.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.SetBlock(ABlock:TDiskBlock):Boolean;
var
 Instance:LongWord;
 Block:TNTFSDiskBlock;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FBlocks.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ABlock = nil then Exit;
  if FBitmap = nil then Exit;
  if FBitmap.Origin = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SetBlock - BlockNo = ' + IntToHex(ABlock.BlockNo,8));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get Block}
  Block:=TNTFSDiskBlock(ABlock);
  if Block.BlockBuffer = nil then Exit;

  {Get Origin}
  Origin:=FBitmap.Origin.Origin;
  if Origin = nil then Exit;

  {Get Attribute}
  Attribute:=Origin.GetAttribute(ntfsAttrTypeData,ntfsBlankName,ntfsInstanceFirst);
  if Attribute = nil then Exit;

  {Write Run} {Allow Update}
  Instance:=ntfsInstanceFirst;
  if WriteRun(Origin,Attribute,Block.BlockBuffer^,Block.BlockNo,FClustersPerBlock,Instance,False,True) <> FClustersPerBlock then Exit;

  Result:=True;
 finally
  FBlocks.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.SetEntry(AParent,AEntry:TDiskEntry):Boolean;
var
 Instance:Integer;
 Entry:TNTFSDiskEntry;
 Origin:TNTFSDiskRecord;
 Current:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if AParent = nil then Exit;
  if AEntry = nil then Exit;
  if AEntry = FRoot then Exit; {Root is a file in NTFS but cannot be set}

  //To Do //Check that Attributes like faReparse/faSparse are not set or cleared this way
          //AddSymbolicLink/MountPoint/JunctionPoint etc are calling this to update attributes etc
          //If we filter faReparse then this will not work

          //How about, if AEntry has faReparse but StandardInformation does not, check for ReparseAttribute ?
          //and if not found then mask the faReparse

          //Also, FAT and CDFS need to filter out the attributes that they dont support

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SetEntry - Entry = ' + AEntry.Name);
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Relative}
  if (AEntry.Attributes and (faDot or faDotDot)) <> faNone then Exit;

  {Get Origin}
  if not FRecords.WriterLock then Exit;
  try
   if TNTFSDiskEntry(AEntry).Origin = nil then Exit;
   Origin:=TNTFSDiskEntry(AEntry).Origin.Origin;
   if Origin = nil then Exit;

   {Check Metafile}
   {if Origin.Metafile then Exit;} {Allow setting on Metafile}

   {Check Stream}
   if (AEntry.Attributes and faMatchMask) = faStream then
    begin
     Result:=True;
    end
   else
    begin
     {Check File}
     if (AEntry.Attributes and faMatchMask) = faFile then
      begin
       {Get Attribute (Standard Information)}
       Attribute:=Origin.GetAttribute(ntfsAttrTypeStandardInformation,ntfsBlankName,ntfsInstanceFirst);
       if Attribute = nil then Exit;

       {$IFDEF NTFS_DEBUG}
       if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SetEntry - Attribute = ' + Attribute.AttributeName + ' Type = ' + IntToHex(Attribute.AttributeType,8) + ' Id = ' + IntToHex(Attribute.AttributeId,4));
       {$ENDIF}

       {Check Encrypted}
       if (AEntry.Attributes and faEncrypted) = faEncrypted then
        begin
         {Check Compressed}
         if (AEntry.Attributes and faCompressed) = faCompressed then Exit;

         {Check Attribute}
         if (TNTFSStandardInformationAttribute(Attribute).Attributes and faEncrypted) = faNone then
          begin
           {Encrypt Entry}
           //if not EncryptEntry(AParent,AEntry) then Exit;             //To Do //Temporary
           AEntry.Attributes:=(AEntry.Attributes and not(faEncrypted)); //To Do //Temporary
          end;
        end
       else
        begin
         {Check Attribute}
         if (TNTFSStandardInformationAttribute(Attribute).Attributes and faEncrypted) = faEncrypted then
          begin
           {Decrypt Entry}
           //if not DecryptEntry(AParent,AEntry) then Exit;             //To Do //Temporary
           AEntry.Attributes:=(AEntry.Attributes or faEncrypted);       //To Do //Temporary
          end;
        end;

       {Check Compressed}
       if (AEntry.Attributes and faCompressed) = faCompressed then
        begin
         {Check Encrypted}
         if (AEntry.Attributes and faEncrypted) = faEncrypted then Exit;

         {Check Attribute}
         if (TNTFSStandardInformationAttribute(Attribute).Attributes and faCompressed) = faNone then
          begin
           {Compress Entry}
           if not CompressEntry(AParent,AEntry) then Exit;
          end;
        end
       else
        begin
         {Check Attribute}
         if (TNTFSStandardInformationAttribute(Attribute).Attributes and faCompressed) = faCompressed then
          begin
           {Decompress Entry}
           if not DecompressEntry(AParent,AEntry) then Exit;
          end;
        end;
      end
     else
      begin
       {Get Attribute (Standard Information)}
       Attribute:=Origin.GetAttribute(ntfsAttrTypeStandardInformation,ntfsBlankName,ntfsInstanceFirst);
       if Attribute = nil then Exit;

       {Check Encrypted}
       if (AEntry.Attributes and faEncrypted) = faEncrypted then
        begin
         {Check Compressed}
         if (AEntry.Attributes and faCompressed) = faCompressed then Exit;
        end;

       {Check Compressed}
       if (AEntry.Attributes and faCompressed) = faCompressed then
        begin
         {Check Encrypted}
         if (AEntry.Attributes and faEncrypted) = faEncrypted then Exit;
        end;

       {$IFDEF NTFS_DEBUG}
       if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SetEntry - Attribute = ' + Attribute.AttributeName + ' Type = ' + IntToHex(Attribute.AttributeType,8) + ' Id = ' + IntToHex(Attribute.AttributeId,4));
       {$ENDIF}
      end;

     {Check Sparse}
     if (AEntry.Attributes and faSparse) = faSparse then
      begin
       {Check Attribute}
       if (TNTFSStandardInformationAttribute(Attribute).Attributes and faSparse) = faNone then
        begin
         {Remove Sparse}
         AEntry.Attributes:=(AEntry.Attributes and not(faSparse));
        end;
      end
     else
      begin
       {Check Attribute}
       if (TNTFSStandardInformationAttribute(Attribute).Attributes and faSparse) = faSparse then
        begin
         {Add Sparse}
         AEntry.Attributes:=(AEntry.Attributes or faSparse);
        end;
      end;

     {Check Reparse}
     if (AEntry.Attributes and faReparse) = faReparse then
      begin
       {Check Attribute}
       if (TNTFSStandardInformationAttribute(Attribute).Attributes and faReparse) = faNone then
        begin
         {Get Reparse}
         if Origin.GetAttribute(ntfsAttrTypeReparsePoint,ntfsAnyName,ntfsInstanceFirst) = nil then
          begin
           {Remove Reparse}
           AEntry.Attributes:=(AEntry.Attributes and not(faReparse));
          end;
        end;
      end
     else
      begin
       {Check Attribute}
       if (TNTFSStandardInformationAttribute(Attribute).Attributes and faReparse) = faReparse then
        begin
         {Get Reparse}
         if Origin.GetAttribute(ntfsAttrTypeReparsePoint,ntfsAnyName,ntfsInstanceFirst) <> nil then
          begin
           {Add Reparse}
           AEntry.Attributes:=(AEntry.Attributes or faReparse);
          end;
        end;
      end;

     {Update Attribute}
     if not Attribute.UpdateAttribute(TNTFSDiskEntry(AEntry)) then Exit;

     {Update Links} {Links belonging to other Entries}
     if Origin.Links <> nil then
      begin
       Entry:=Origin.Links.FirstEntry;
       while Entry <> nil do
        begin
         if Entry <> AEntry then if not Attribute.UpdateEntry(Entry) then Exit;
         Entry:=Entry.NextEntry;
        end;
      end;

     {Get Attribute (File Name)}
     Instance:=1;
     Attribute:=Origin.GetAttribute(ntfsAttrTypeFileName,ntfsAnyName,Instance);
     while Attribute <> nil do
      begin
       {$IFDEF NTFS_DEBUG}
       if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SetEntry - Attribute = ' + Attribute.AttributeName + ' Type = ' + IntToHex(Attribute.AttributeType,8) + ' Id = ' + IntToHex(Attribute.AttributeId,4));
       {$ENDIF}

       {Get Link}
       Entry:=Origin.GetLink(Attribute);
       if Entry <> nil then
        begin
         {$IFDEF NTFS_DEBUG}
         if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SetEntry - Link = ' + Entry.Name);
         {$ENDIF}

         {Update Attribute}
         if not Attribute.UpdateAttribute(Entry) then Exit;

         {Get Reference}
         Current:=GetReferenceEx(nil,TNTFSFileNameAttribute(Attribute).ParentReference,True);
         if Current = nil then Exit;

         {Set Key}
         if not SetKey(Current,Attribute) then Exit;
        end;

       {Get Attribute (FileName)}
       Inc(Instance);
       Attribute:=Origin.GetAttribute(ntfsAttrTypeFileName,ntfsAnyName,Instance);
      end;

     {Set Records}
     Result:=SetRecords(Origin);
    end;
  finally
   FRecords.WriterUnlock;
  end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.SetSecurity(AParent,AEntry:TDiskEntry;ASecurity:TDiskSecurity):Boolean;
{Note: If SetSecurity returns true then it must either retain or free the security object
       On failure the caller must free the security object}
var
 SecurityId:LongWord;

 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if AParent = nil then Exit;
  if AEntry = nil then Exit;
  if ASecurity = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SetSecurity - Entry = ' + AEntry.Name);
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Relative}
  if (AEntry.Attributes and (faDot or faDotDot)) <> faNone then Exit;

  {Get Origin}
  if not FRecords.WriterLock then Exit;
  try
   if TNTFSDiskEntry(AEntry).Origin = nil then Exit;
   Origin:=TNTFSDiskEntry(AEntry).Origin.Origin;
   if Origin = nil then Exit;

   {Check Version}
   case FVolumeVersion of
    ntfsNTFS12:begin
      {Get Attribute}
      Attribute:=Origin.GetAttribute(ntfsAttrTypeSecurityDescriptor,ntfsBlankName,ntfsInstanceFirst);
      if Attribute = nil then
       begin
        {Add Attribute}
        Attribute:=Origin.NewAttribute(nil,ntfsAttrTypeSecurityDescriptor,ntfsBlankName,FVolumeVersion);
        if Attribute = nil then Exit;

        {Add Security}
        if not TNTFSSecurityDescriptorAttribute(Attribute).NewSecurity(TNTFSSecurity(ASecurity)) then Exit;

        {Update Attribute}
        Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
        Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

        {Add Item}
        if not AddItem(Origin,Attribute) then Exit;

        {Size Record}
        if not SizeRecord(Origin,Origin.CalculatedSize(FVolumeVersion)) then Exit;
       end
      else
       begin
        {Update Security}

        //To Do //SizeAttribute ? //WriteAttribute ?

        Exit; //To Do

        //Cleanup on failure

       end;

      {Set Records}
      if not SetRecords(Origin) then Exit;

      {Do not Free Security}
      Result:=True;
     end;
    ntfsNTFS30,ntfsNTFS31:begin
      {Get Attribute}
      Attribute:=Origin.GetAttribute(ntfsAttrTypeSecurityDescriptor,ntfsBlankName,ntfsInstanceFirst);
      if Attribute <> nil then
       begin
        {Update Security (Do not convert to SecurityId}

        //To Do //SizeAttribute ? //WriteAttribute ?

        //To Do //SetRecords

        Exit; //To Do

        //Cleanup on failure

       end
      else
       begin
        {Get Attribute}
        Attribute:=Origin.GetAttribute(ntfsAttrTypeStandardInformation,ntfsBlankName,ntfsInstanceFirst);
        if Attribute = nil then Exit;

        {Get Security Id}
        SecurityId:=GetSecurityId(TNTFSSecurity(ASecurity),True);
        if SecurityId <> ntfsSecurityIdUnknown then
         begin
          {Set Security}
          TNTFSStandardInformationAttribute(Attribute).SecurityId:=SecurityId;

          {Set Records}
          if not SetRecords(Origin) then Exit;

          {Free Security}
          ASecurity.Free;

          Result:=True;
         end
        else
         begin
          {Add Security}
          SecurityId:=AddSecurity(TNTFSSecurity(ASecurity));
          if SecurityId = ntfsSecurityIdUnknown then Exit;

          {Set Security}
          TNTFSStandardInformationAttribute(Attribute).SecurityId:=SecurityId;

          {Set Records}
          if not SetRecords(Origin) then Exit;

          {Do not Free Security (even if AddSecurity fails)}
          Result:=True;
         end;
       end;
     end;
   end;
  finally
   FRecords.WriterUnlock;
  end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.SetRecord(ARecord:TNTFSDiskRecord):Boolean;
var
 VCN:Int64;
 Size:LongWord;
 Start:LongWord;
 Offset:LongWord;
begin
 {}
 Result:=False;

 if not FRecords.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SetRecord - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' BaseReference = ' + IntToHex(ARecord.BaseReference,16));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get VCN}
  if not GetFileRecordVCN(ARecord.RecordNumber,VCN,Start) then Exit;

  if not FileLock then Exit;
  try
   if FFileBuffer = nil then Exit;

   {Check Count}
   if FFilesPerCluster > 1 then
    begin
     {Read Run}
     Size:=Max(FClustersPerFile,1);
     if not ReadFileRecord(ARecord.RecordNumber,FFileBuffer^,VCN,Size) then Exit;
    end;

   {Get Offset}
   Offset:=Start;
   Size:=FFileRecordSize;

   {Zero Buffer}
   ZeroMemory(Pointer(PtrUInt(FFileBuffer) + Offset),Size);

   {Increment Sequence}
   ARecord.UpdateSequenceNumber:=ARecord.UpdateSequenceNumber + 1;

   {Write Record}
   if not ARecord.WriteRecord(FFileBuffer,Offset,Size,FVolumeVersion) then Exit;

   {Write Attributes}
   if not ARecord.WriteAttributes(FFileBuffer,Offset,Size,FVolumeVersion) then Exit;

   {Write Fixup}
   if not WriteFixup(FFileBuffer,Start,ARecord.UpdateSequenceNumber,ARecord.UpdateSequenceOffset,ARecord.UpdateSequenceLength) then Exit;

   {Write Run}
   Size:=Max(FClustersPerFile,1);
   if not WriteFileRecord(ARecord.RecordNumber,FFileBuffer^,VCN,Size,False) then Exit;

   {Check Mirrored}
   if ARecord.Mirrored then
    begin
     {Write Run}
     Size:=Max(FClustersPerFile,1);
     if not WriteFileRecord(ARecord.RecordNumber,FFileBuffer^,VCN,Size,True) then Exit;
    end;

   Result:=True;
  finally
   FileUnlock;
  end;
 finally
  FRecords.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.SetAttribute(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute):Boolean;
{Generic SetAttribute method which simply calls specific methods for attribute type}
{Can be expanded if needed to cover additional attribute types in future}
{Note: Caller must hold the records lock}
begin
 {}
 Result:=False;

 if not FRecords.AttributesWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if AAttribute = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SetAttribute - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' Type = ' + IntToHex(AAttribute.AttributeType,8) + ' Attribute = ' + AAttribute.AttributeName);
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Resident}
  if AAttribute.NonResident = ntfsAttributeResident then
   begin
    {Resident}
    case AAttribute.AttributeType of
     ntfsAttrTypeIndexRoot:begin
       {Index Root}
       Result:=SetIndex(ARecord,AAttribute);
      end;
     else
      begin
       {All Other Attributes}
       Result:=True;
      end;
    end;
   end
  else
   begin
    {Non Resident}
    case AAttribute.AttributeType of
     ntfsAttrTypeAttributeList:begin
       {Attribute List}
       Result:=SetList(ARecord,AAttribute);
      end;
     else
      begin
       {All Other Attributes}
       Result:=True;
      end;
    end;
   end;
 finally
  FRecords.AttributesWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.SetList(ARecord:TNTFSDiskRecord;AList:TNTFSDiskAttribute):Boolean;
{Only called by SetAttribute}
{Note: Caller must hold the records lock}
var
 Size:LongWord;
 Buffer:Pointer;
 Offset:LongWord;
 Instance:LongWord;
begin
 {}
 Result:=False;

 if not FRecords.AttributesWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if AList = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SetList - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' Type = ' + IntToHex(AList.AttributeType,8) + ' List = ' + AList.AttributeName);
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Updating}
  if AList.Updating then
   begin
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SetList - (Update in Progress)');
    {$ENDIF}

    Result:=True;
   end
  else
   begin
    {Set Updating}
    AList.Updating:=True;
    try
     {Check Resident}
     if AList.NonResident = ntfsAttributeNonResident then
      begin
       {Non Resident}
       {Check Size}
       if AList.StreamSize > 0 then
        begin
         Buffer:=GetMem(AList.StreamSize);
         if Buffer = nil then Exit;
         try
          {Get Offset}
          Offset:=0;
          Size:=AList.StreamSize;

          {Write Items}
          if not AList.WriteItems(Buffer,Offset,Size,FVolumeVersion) then Exit;

          {Write Attribute}
          Instance:=ntfsInstanceFirst;
          if WriteAttribute(ARecord,AList,Buffer^,0,AList.StreamSize,Instance,False) <> AList.StreamSize then Exit;

          Result:=True;
         finally
          FreeMem(Buffer);
         end;
        end;
      end;

     Result:=True; {If Resident then just succeed}
    finally
     {Clear Updating}
     AList.Updating:=False;
    end;
   end;
 finally
  FRecords.AttributesWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.SetIndex(ARecord:TNTFSDiskRecord;ARoot:TNTFSDiskAttribute):Boolean;
{Only called by SetAttribute}
{Note: Caller must hold the records lock}
var
 Instance:LongWord;

 Key:TNTFSDiskKey;
 Node:TNTFSDiskNode;
 Next:TNTFSDiskNode;
 Nodes:TNTFSDiskNodes;
 Index:TNTFSDiskIndex;
 Current:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
 Allocation:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FRecords.IndexWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if ARoot = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SetIndex - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' Type = ' + IntToHex(ARoot.AttributeType,8) + ' Root = ' + ARoot.AttributeName);
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Updating}
  if ARoot.Updating then
   begin
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SetIndex - (Update in Progress)');
    {$ENDIF}

    Result:=True;
   end
  else
   begin
    {Set Updating}
    ARoot.Updating:=True;
    try
     {Get Index}
     Index:=ARoot.Index;
     if Index = nil then Exit;

     {Get Nodes}
     Nodes:=Index.Nodes;
     if Nodes = nil then Exit;

     {Check Modified}
     if Nodes.Modified then
      begin
       {Get Attribute (Bitmap}
       Attribute:=ARecord.GetAttribute(ntfsAttrTypeBitmap,ARoot.AttributeName,ntfsInstanceFirst);
       {if Attribute = nil then Exit;} {Bitmap may be nil}

       {$IFDEF NTFS_DEBUG}
       if Attribute <> nil then if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SetIndex - Bitmap = ' + Attribute.AttributeName + ' Type = ' + IntToHex(Attribute.AttributeType,8) + ' Id = ' + IntToHex(Attribute.AttributeId,4));
       {$ENDIF}

       {Get Attribute (Index Allocation}
       Allocation:=ARecord.GetAttribute(ntfsAttrTypeIndexAllocation,ARoot.AttributeName,ntfsInstanceFirst);
       {if Allocation = nil then Exit;} {Allocation may be nil}

       {$IFDEF NTFS_DEBUG}
       if Allocation <> nil then if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SetIndex - Allocation = ' + Allocation.AttributeName + ' Type = ' + IntToHex(Allocation.AttributeType,8) + ' Id = ' + IntToHex(Allocation.AttributeId,4));
       {$ENDIF}

       {Deleted Nodes}
       Node:=TNTFSDiskNode(Nodes.First);
       while Node <> nil do
        begin
         {Get Next}
         Next:=TNTFSDiskNode(Node.Next);

         {Check Node}
         if (Node.Deleted) and (Node.RecordNumber <> ntfsUnknownRecordNumber) then
          begin
           {Deleted Node with Allocated Record}
           if not SetNode(ARecord,Allocation,Index,Node,nil,True) then Exit;

           {Remove Node}
           if not RemoveNode(ARecord,Allocation,Attribute,Index,Node) then Exit;

           {Remove Node}
           if not Index.RemoveNode(Node) then Exit;
          end
         else
          begin
           if Node.IsRoot and (Node.RecordNumber <> ntfsUnknownRecordNumber) then
            begin
             {Root Node with Allocated Record (Has been promoted to Root)}
             if not SetNode(ARecord,Allocation,Index,Node,nil,True) then Exit;

             {Remove Node}
             if not RemoveNode(ARecord,Allocation,Attribute,Index,Node) then Exit;
            end;
          end;

         Node:=Next;
        end;

       {Added Nodes}
       Node:=TNTFSDiskNode(Nodes.First);
       while Node <> nil do
        begin
         {Check Node}
         if not(Node.IsRoot) and (Node.Added) and (Node.RecordNumber = ntfsUnknownRecordNumber) then
          begin
           {Non Root Added Node with no Allocated Record}
           {Check Bitmap}
           if Attribute = nil then
            begin
             {Add Bitmap}
             Attribute:=AddAttribute(ARecord,ntfsAttrTypeBitmap,ARoot.AttributeName);
             if Attribute = nil then Exit;

             {Size Bitmap}
             TNTFSBitmapAttribute(Attribute).BitmapSize:=8;
             if not SizeAttribute(ARecord,Attribute,TNTFSBitmapAttribute(Attribute).BitmapSize) then Exit;

             {Write Bitmap} {No Update}
             Instance:=ntfsInstanceFirst;
             if WriteAttribute(ARecord,Attribute,TNTFSBitmapAttribute(Attribute).Bitmap^,0,TNTFSBitmapAttribute(Attribute).BitmapSize,Instance,False) <> Integer(TNTFSBitmapAttribute(Attribute).BitmapSize) then Exit;

             {Size attribute will call size record}

             {Set Records}
             if not SetRecords(ARecord) then Exit;
            end;

           {Check Allocation}
           if Allocation = nil then
            begin
             {Add Allocation}
             Allocation:=AddAttribute(ARecord,ntfsAttrTypeIndexAllocation,ARoot.AttributeName);
             if Allocation = nil then Exit;

             {Size Allocation}
             if not SizeAttribute(ARecord,Allocation,Index.IndexRecordSize) then Exit;

             {Size attribute will call size record}

             {Set Records}
             if not SetRecords(ARecord) then Exit;
            end;

           {Add Node}
           if not AddNode(ARecord,Allocation,Attribute,Index,Node) then Exit;
          end
         else
          begin
           if not(Node.IsRoot) and (Node.RecordNumber = ntfsUnknownRecordNumber) then
            begin
             {Non Root Node with no Allocated Record (Has been demoted from Root)}
             {Check Bitmap}
             if Attribute = nil then
              begin
               {Add Bitmap}
               Attribute:=AddAttribute(ARecord,ntfsAttrTypeBitmap,ARoot.AttributeName);
               if Attribute = nil then Exit;

               {Size Bitmap}
               TNTFSBitmapAttribute(Attribute).BitmapSize:=8;
               if not SizeAttribute(ARecord,Attribute,TNTFSBitmapAttribute(Attribute).BitmapSize) then Exit;

               {Write Bitmap} {No Update}
               Instance:=ntfsInstanceFirst;
               if WriteAttribute(ARecord,Attribute,TNTFSBitmapAttribute(Attribute).Bitmap^,0,TNTFSBitmapAttribute(Attribute).BitmapSize,Instance,False) <> Integer(TNTFSBitmapAttribute(Attribute).BitmapSize) then Exit;

               {Size attribute will call size record}

               {Set Records}
               if not SetRecords(ARecord) then Exit;
              end;

             {Check Allocation}
             if Allocation = nil then
              begin
               {Add Allocation}
               Allocation:=AddAttribute(ARecord,ntfsAttrTypeIndexAllocation,ARoot.AttributeName);
               if Allocation = nil then Exit;

               {Size Allocation}
               if not SizeAttribute(ARecord,Allocation,Index.IndexRecordSize) then Exit;

               {Size attribute will call size record}

               {Set Records}
               if not SetRecords(ARecord) then Exit;
              end;

             {Add Node}
             if not AddNode(ARecord,Allocation,Attribute,Index,Node) then Exit;
            end;
          end;

         Node:=TNTFSDiskNode(Node.Next);
        end;

       {Update Nodes}
       if not Index.UpdateNodes(FVolumeVersion) then Exit;

       {Changed Nodes}
       Node:=TNTFSDiskNode(Nodes.First);
       while Node <> nil do
        begin
         {Check Node}
         if Node.Changed then
          begin
           {Get Key}
           Key:=Node.Start;

           {Check Parent}
           if Key.Parent = nil then
            begin
             {Update Attribute}
             ARoot.DataSize:=ARoot.CalculatedStreamSize(FVolumeVersion);
             ARoot.AttributeSize:=ARoot.CalculatedSize(FVolumeVersion);

             {Get Record}
             Current:=ARoot.Parent;
             if Current = nil then Exit;

             {Size Record}
             if not SizeRecord(Current,Current.CalculatedSize(FVolumeVersion)) then Exit;

             {Set Records}
             if not SetRecords(ARecord) then Exit;
            end
           else
            begin
             {Set Node}
             if not SetNode(ARecord,Allocation,Index,Node,TNTFSDiskKey(Key.Parent),False) then Exit;
            end;
          end;

         Node:=TNTFSDiskNode(Node.Next);
        end;

       Nodes.Modified:=False;

       {Set Records}
       if not SetRecords(ARecord) then Exit;
      end;

     Result:=True;
    finally
     {Clear Updating}
     ARoot.Updating:=False;
    end;
   end;
 finally
  FRecords.IndexWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.SetNode(ARecord:TNTFSDiskRecord;AAllocation:TNTFSDiskAttribute;AIndex:TNTFSDiskIndex;ANode:TNTFSDiskNode;AParent:TNTFSDiskKey;AEmpty:Boolean):Boolean;
{Note: The caller must ensure the passed node is not the root node (Root node is always resident)}
{Note: Caller must hold the records and index lock}
var
 VCN:Int64;
 Size:LongWord;
 Start:LongWord;
 Offset:LongWord;
 Instance:LongWord;

 UpdateSequenceOffset:Word;
 UpdateSequenceLength:Word;
begin
 {}
 Result:=False;

 if not FRecords.NodesWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if AAllocation = nil then Exit;
  if AIndex = nil then Exit;
  if ANode = nil then Exit;
  if FIndexBuffer = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SetNode - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' NodeNumber = ' + IntToHex(ANode.RecordNumber,16));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Node}
  if ANode.RecordNumber = ntfsUnknownRecordNumber then Exit;

  {Get VCN}
  if not GetIndexRecordVCN(AIndex,ANode.RecordNumber,VCN,Start) then Exit;

  {Check Count}
  if AIndex.IndexsPerCluster > 1 then
   begin
    {Read Run}
    Instance:=ntfsInstanceFirst;
    Size:=Max(AIndex.ClustersPerIndex,1);
    if ReadRun(ARecord,AAllocation,FIndexBuffer^,VCN,Size,Instance,False,True) <> Size then Exit;
   end;

  {Get Offset}
  Offset:=Start;
  Size:=AIndex.IndexRecordSize;

  {Zero Buffer}
  ZeroMemory(Pointer(PtrUInt(FIndexBuffer) + Offset),Size);

  {Increment Sequence}
  ANode.UpdateSequenceNumber:=ANode.UpdateSequenceNumber + 1;

  {Check Empty}
  if AEmpty then
   begin
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SetNode - Empty = ' + BoolToStr(AEmpty));
    {$ENDIF}

    {Write Empty}
    if not ANode.WriteEmpty(FIndexBuffer,Offset,Size,FVolumeVersion,UpdateSequenceOffset,UpdateSequenceLength) then Exit;

    {Write Fixup}
    if not WriteFixup(FIndexBuffer,Start,ANode.UpdateSequenceNumber,UpdateSequenceOffset,UpdateSequenceLength) then Exit;
   end
  else
   begin
    if AParent = nil then Exit;

    {Write Node}
    if not ANode.WriteRecord(FIndexBuffer,Offset,Size,FVolumeVersion) then Exit;

    {Write Keys}
    if not AIndex.WriteKeys(AParent,ANode,FIndexBuffer,Offset,Size,FVolumeVersion) then Exit;

    {Write Fixup}
    if not WriteFixup(FIndexBuffer,Start,ANode.UpdateSequenceNumber,ANode.UpdateSequenceOffset,ANode.UpdateSequenceLength) then Exit;
   end;

  {Write Run}
  Instance:=ntfsInstanceFirst;
  Size:=Max(AIndex.ClustersPerIndex,1);
  if WriteRun(ARecord,AAllocation,FIndexBuffer^,VCN,Size,Instance,False,False) <> Size then Exit;

  Result:=True;
 finally
  FRecords.NodesWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.SetItem(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute):Boolean;
{Update the item referencing the supplied attribute in the list}
{To be called by MoveRun when the StartVCN of an attribute changes}
{Note: Caller must hold the records lock}
var
 Item:TNTFSDiskItem;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FRecords.AttributesWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if AAttribute = nil then Exit;

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Get Origin}
  Origin:=ARecord.Origin;
  if Origin = nil then Exit;

  {Check List}
  if (Origin.Overflow) and not(AAttribute.IsUnlisted) then
   begin
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SetItem - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' Type = ' + IntToHex(AAttribute.AttributeType,8) + ' Attribute = ' + AAttribute.AttributeName);
    {$ENDIF}

    {Check Removing}
    if Origin.Removing then
     begin
      {$IFDEF NTFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SetItem - (Remove in Progress)');
      {$ENDIF}
     end
    else
     begin
      {Get List}
      Attribute:=Origin.GetAttribute(ntfsAttrTypeAttributeList,ntfsAnyName,ntfsInstanceFirst);
      if Attribute = nil then Exit;

      {Get Item}
      Item:=Attribute.GetItemByAttribute(AAttribute);
      if Item = nil then Exit;

      {Update Item} {No need to remove and insert as the ordering (by StartVCN) will remain the same}
      if not Item.UpdateItem then Exit;

      {Size Attribute}
      if not SizeAttribute(ARecord,Attribute,Attribute.CalculatedStreamSize(FVolumeVersion)) then Exit;

      {Set Attribute}
      if not SetAttribute(ARecord,Attribute) then Exit;
     end;
   end;

  Result:=True;
 finally
  FRecords.AttributesWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.SetKey(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute):Boolean;
{Update the Key for the FileName attribute in the $I30 index of the supplied record}
{Note: Caller must hold the records lock}
var
 Origin:TNTFSDiskRecord;

 Key:TNTFSAttributeKey;
 Root:TNTFSDiskAttribute;
 Index:TNTFSAttributeIndex;
 Allocation:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FRecords.AttributesWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if AAttribute =nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SetKey - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' Attribute = ' + AAttribute.AttributeName);
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Type}
  if AAttribute.AttributeType = ntfsAttrTypeFileName then
   begin
    {Get Origin}
    Origin:=ARecord.Origin;
    if Origin = nil then Exit;

    {Get Attribute (Index Root)}
    Root:=Origin.GetAttribute(ntfsAttrTypeIndexRoot,ntfsIndexNameFileName,ntfsInstanceFirst);
    if Root = nil then Exit;

    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SetKey - Root = ' + Root.AttributeName + ' Type = ' + IntToHex(Root.AttributeType,8) + ' Id = ' + IntToHex(Root.AttributeId,4));
    {$ENDIF}

    {Get Attribute (Index Allocation}
    Allocation:=Origin.GetAttribute(ntfsAttrTypeIndexAllocation,ntfsIndexNameFileName,ntfsInstanceFirst);
    {if Allocation = nil then Exit;} {Allocation may be nil}

    {$IFDEF NTFS_DEBUG}
    if Allocation <> nil then if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SetKey - Allocation = ' + Allocation.AttributeName + ' Type = ' + IntToHex(Allocation.AttributeType,8) + ' Id = ' + IntToHex(Allocation.AttributeId,4));
    {$ENDIF}

    {Get Index}
    Index:=TNTFSAttributeIndex(Root.Index);
    if Index = nil then Exit;

    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SetKey - IndexType = ' + IntToStr(Index.IndexType) + ' CollateRule = ' + IntToStr(Index.CollateRule) + ' Order = ' + IntToStr(Index.Order) + ' Median = ' + IntToStr(Index.Median));
    {$ENDIF}

    {Get Key}
    Key:=TNTFSAttributeKey(Index.GetKeyByAttribute(AAttribute));
    if Key = nil then Exit;

    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SetKey - Key = ' + IntToHex(Key.FileReference,16));
    {$ENDIF}

    {Update Key}
    if not Key.UpdateKey then Exit;

    {Check Parent}
    if Key.Parent = nil then
     begin
      {Set Records}
      if not SetRecords(Origin) then Exit;
     end
    else
     begin
      {Set Node}
      if not SetNode(Origin,Allocation,Index,Key.Node,TNTFSDiskKey(Key.Parent),False) then Exit;
     end;

    Result:=True;
   end;
 finally
  FRecords.AttributesWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.SizeEntry(AParent,AEntry:TDiskEntry;const ASize:Int64):Boolean;
var
 Entry:TNTFSDiskEntry;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if AParent = nil then Exit;
  if AEntry = nil then Exit;
  if AEntry = FRoot then Exit; {Cannot size root}

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SizeEntry - Entry = ' + AEntry.Name + ' Size = ' + IntToStr(ASize));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Relative}
  if (AEntry.Attributes and (faDot or faDotDot)) <> faNone then Exit;

  {Get Origin}
  if not FRecords.WriterLock then Exit;
  try
   if TNTFSDiskEntry(AEntry).Origin = nil then Exit;
   Origin:=TNTFSDiskEntry(AEntry).Origin.Origin;
   if Origin = nil then Exit;

   {Check Metafile}
   {if Origin.Metafile then Exit;} {Allow sizing on Metafile}

   {Check Stream}
   if (AEntry.Attributes and faMatchMask) = faStream then
    begin
     {Get Attribute}
     Attribute:=TNTFSDiskEntry(AEntry).Attribute; {Will always be the first instance due to LoadEntries}
     if Attribute = nil then Exit;

     {Size Attribute}
     if not SizeAttribute(Origin,Attribute,ASize) then Exit;

     {Set Records}
     if not SetRecords(Origin) then Exit;

     {Update Streams} {Streams belonging to other Links}
     if Origin.Streams <> nil then
      begin
       Entry:=Origin.Streams.FirstEntry;
       while Entry <> nil do
        begin
         if (Entry <> AEntry) and (Entry.Attribute = Attribute) then if not Attribute.UpdateEntry(Entry) then Exit;

         Entry:=Entry.NextEntry;
        end;
      end;

     {Update Entry}
     Result:=Attribute.UpdateEntry(TNTFSDiskEntry(AEntry));
    end
   else
    begin
     {Check Entry}
     if (AEntry.Attributes and faMatchMask) <> faFile then Exit;

     {Get Attribute}
     Attribute:=Origin.GetAttribute(ntfsAttrTypeData,ntfsBlankName,ntfsInstanceFirst); {Must pass first instance}
     if Attribute = nil then
      begin
       {Add Attribute}
       Attribute:=AddAttribute(Origin,ntfsAttrTypeData,ntfsBlankName);
       if Attribute = nil then Exit;
      end;

     {Size Attribute}
     if not SizeAttribute(Origin,Attribute,ASize) then Exit;

     {Set Records}
     if not SetRecords(Origin) then Exit;

     {Update Links} {Links belonging to other Entries}
     if Origin.Links <> nil then
      begin
       Entry:=Origin.Links.FirstEntry;
       while Entry <> nil do
        begin
         if Entry <> AEntry then if not Attribute.UpdateEntry(Entry) then Exit;

         Entry:=Entry.NextEntry;
        end;
      end;

     {Update Entry}
     Result:=Attribute.UpdateEntry(TNTFSDiskEntry(AEntry));
    end;
  finally
   FRecords.WriterUnlock;
  end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.SizeRecord(ARecord:TNTFSDiskRecord;ASize:LongWord):Boolean;
{Update the record size to the supplied size}
{Handles adding and allocation of extension records if needed}
{Handles moving attributes to non resident and extension records if needed}
{Handles spliting attributes which no longer fit within one record}
var
 Size:LongWord;
 StartVCN:Int64;

 Run:TNTFSDiskRun;
 Instance:LongWord;
 Origin:TNTFSDiskRecord;
 Current:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
 Additional:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FRecords.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SizeRecord - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' Size = ' + IntToStr(ASize));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Size}
  if ASize = 0 then Exit;

  {Check Resizing}
  if ARecord.Resizing then
   begin
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SizeRecord - (Resize in Progress)');
    {$ENDIF}

    Result:=True;
   end
  else
   begin
    {Set Resizing}
    ARecord.Resizing:=True;
    try
     {$IFDEF NTFS_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SizeRecord - Record Size = ' + IntToStr(ARecord.RecordSize));
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SizeRecord - Record Allocated = ' + IntToStr(ARecord.RecordAllocated));
     {$ENDIF}

     {Check Size}
     if ASize < ARecord.RecordSize then
      begin
       {$IFDEF NTFS_DEBUG}
       if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SizeRecord - (Smaller)');
       {$ENDIF}

       {Update Record}
       ARecord.RecordSize:=ASize;
      end
     else if ASize > ARecord.RecordSize then
      begin
       {$IFDEF NTFS_DEBUG}
       if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SizeRecord - (Larger)');
       {$ENDIF}

       {Check Allocated}
       if ASize <= ARecord.RecordAllocated then
        begin
         {Update Record}
         ARecord.RecordSize:=ASize;
        end
       else
        begin
         {Get Origin}
         Origin:=ARecord.Origin;
         if Origin = nil then Exit;

         {Get Size}
         Size:=ASize;
         while Size > ARecord.RecordAllocated do
          begin
           {Get Attribute}
           Attribute:=GetNextConvertAttribute(ARecord);
           if Attribute <> nil then
            begin
             {$IFDEF NTFS_DEBUG}
             if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SizeRecord - Convertable Attribute Type = ' + IntToHex(Attribute.AttributeType,8) + ' Name = ' + Attribute.AttributeName + ' Size = ' + IntToStr(Attribute.AttributeSize));
             {$ENDIF}

             {Convert Attribute}
             if not ConvertAttribute(ARecord,Attribute) then Exit;
            end
           else
            begin
             {$IFDEF NTFS_DEBUG}
             if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SizeRecord - No Convertable Attributes');
             {$ENDIF}

             {Get Attribute}
             Attribute:=GetNextMoveAttribute(ARecord);
             if Attribute <> nil then
              begin
               {$IFDEF NTFS_DEBUG}
               if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SizeRecord - Movable Attribute Type = ' + IntToHex(Attribute.AttributeType,8) + ' Name = ' + Attribute.AttributeName + ' Size = ' + IntToStr(Attribute.AttributeSize));
               {$ENDIF}

               {Get Record}
               Current:=Origin.GetRecordByFree(Attribute.AttributeSize,True);
               if Current <> nil then
                begin
                 {Move Attribute}
                 if not MoveAttribute(ARecord,Current,Attribute) then Exit;
                end
               else
                begin
                 {$IFDEF NTFS_DEBUG}
                 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SizeRecord - No Record found with ' + IntToStr(Attribute.AttributeSize) + ' bytes free');
                 {$ENDIF}

                 {Check Split}
                 if (Attribute.AttributeSize < ARecord.AttributeSize(FVolumeVersion,Attribute.AttributeType)) or (Attribute.RunCount < 3) then
                  begin
                   {Add Record}
                   Current:=AddRecord(Origin,Origin.IsFolder);
                   if Current = nil then Exit;

                   {Move Attribute}
                   if not MoveAttribute(ARecord,Current,Attribute) then Exit;
                  end
                 else
                  begin
                   {Check Single}
                   if Attribute.IsSingle then Exit;

                   {Get Attribute}
                   Instance:=ntfsInstanceFirst;
                   Additional:=Origin.GetAttributeByVCN(Attribute,Attribute.LastVCN + 1,Instance);
                   if Additional = nil then
                    begin
                     {Add Attribute}
                     Additional:=ARecord.NewAttribute(Attribute,Attribute.AttributeType,Attribute.AttributeName,FVolumeVersion);
                     if Additional = nil then Exit;

                     {Add Item}
                     if not AddItem(ARecord,Additional) then Exit;
                    end;
                   {$IFDEF NTFS_DEBUG}
                   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SizeRecord Attribute StartVCN = ' + IntToHex(Attribute.StartVCN,16) + ' LastVCN = ' + IntToHex(Attribute.LastVCN,16));
                   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SizeRecord Additional StartVCN = ' + IntToHex(Additional.StartVCN,16) + ' LastVCN = ' + IntToHex(Additional.LastVCN,16));
                   {$ENDIF}

                   {Move Runs}
                   while (Attribute.AttributeSize >= ARecord.AttributeSize(FVolumeVersion,Attribute.AttributeType)) and (Attribute.RunCount > 2) do
                    begin
                     {Get Run}
                     Run:=Attribute.GetRun(Attribute.LastVCN,StartVCN);
                     if Run = nil then Exit;

                     {Move Run}
                     if not MoveRun(ARecord,Attribute,Additional,Run.Length) then Exit;
                    end;

                   {$IFDEF NTFS_DEBUG}
                   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SizeRecord Attribute StartVCN = ' + IntToHex(Attribute.StartVCN,16) + ' LastVCN = ' + IntToHex(Attribute.LastVCN,16));
                   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SizeRecord Additional StartVCN = ' + IntToHex(Additional.StartVCN,16) + ' LastVCN = ' + IntToHex(Additional.LastVCN,16));
                   {$ENDIF}

                   {Next Iteration will Move Attribute (either to existing or new record)}
                  end;
                end;
              end
             else
              begin
               {$IFDEF NTFS_DEBUG}
               if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SizeRecord - No Movable Attributes');
               {$ENDIF}

               {Get Attribute}
               Attribute:=GetNextSplitAttribute(ARecord);
               if Attribute = nil then Exit;

               {$IFDEF NTFS_DEBUG}
               if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SizeRecord - Splitable Attribute Type = ' + IntToHex(Attribute.AttributeType,8) + ' Name = ' + Attribute.AttributeName + ' Size = ' + IntToStr(Attribute.AttributeSize));
               {$ENDIF}

               {Get Attribute}
               Instance:=ntfsInstanceFirst;
               Additional:=Origin.GetAttributeByVCN(Attribute,Attribute.LastVCN + 1,Instance);
               if Additional = nil then
                begin
                 {Add Attribute}
                 Additional:=ARecord.NewAttribute(Attribute,Attribute.AttributeType,Attribute.AttributeName,FVolumeVersion);
                 if Additional = nil then Exit;

                 {Add Item}
                 if not AddItem(ARecord,Additional) then Exit;
                end;

               {$IFDEF NTFS_DEBUG}
               if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SizeRecord Attribute StartVCN = ' + IntToHex(Attribute.StartVCN,16) + ' LastVCN = ' + IntToHex(Attribute.LastVCN,16));
               if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SizeRecord Additional StartVCN = ' + IntToHex(Additional.StartVCN,16) + ' LastVCN = ' + IntToHex(Additional.LastVCN,16));
               {$ENDIF}

               {Move Runs}
               while Attribute.RunCount > 2 do
                begin
                 {Get Run}
                 Run:=Attribute.GetRun(Attribute.LastVCN,StartVCN);
                 if Run = nil then Exit;

                 {Move Run}
                 if not MoveRun(ARecord,Attribute,Additional,Run.Length) then Exit;
                end;

               {$IFDEF NTFS_DEBUG}
               if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SizeRecord Attribute StartVCN = ' + IntToHex(Attribute.StartVCN,16) + ' LastVCN = ' + IntToHex(Attribute.LastVCN,16));
               if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SizeRecord Additional StartVCN = ' + IntToHex(Additional.StartVCN,16) + ' LastVCN = ' + IntToHex(Additional.LastVCN,16));
               {$ENDIF}

               {Next Iteration will Move Attribute (either to existing or new record) (if applicable)}
              end;
            end;

           {Get Size}
           Size:=ARecord.CalculatedSize(FVolumeVersion);

           {$IFDEF NTFS_DEBUG}
           if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SizeRecord Size = ' + IntToStr(Size));
           {$ENDIF}
          end;

         {Update Record}
         ARecord.RecordSize:=Size;
        end;
      end;

     Result:=True; {Note: If Size is same then just succeed}
    finally
     {Clear Resizing}
     ARecord.Resizing:=False;
    end;
   end;
 finally
  FRecords.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.SizeAttribute(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute;const ASize:Int64):Boolean;
{Note: The attribute can be of any type (Named or Unnamed) but will most commonly be data, allocation or bitmap}
{Note: The passed attribute must be the first instance}
{Note: Updates DataSize and AttributeSize values of Attribute}
{Note: Caller must hold the records lock}
var
 Size:LongWord;
 AttrDef:TNTFSAttrDef;
 Current:TNTFSDiskRecord;
begin
 {}
 Result:=False;

 if not FRecords.AttributesWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if AAttribute = nil then Exit;

  //To Do //Remember to check if FMaster and if can be expanded (Reserved MFT)

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SizeAttribute - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' Type = ' + IntToHex(AAttribute.AttributeType,8) + ' Attribute = ' + AAttribute.AttributeName + ' Size = ' + IntToStr(ASize));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Resident}
  if AAttribute.NonResident = ntfsAttributeResident then
   begin
    {Check Size}
    if ASize = 0 then
     begin
      {Zero}
      if AAttribute.DataSize > 0 then
       begin
        {$IFDEF NTFS_DEBUG}
        if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SizeAttribute (Zero)');
        {$ENDIF}

        {Get Record}
        Current:=AAttribute.Parent;
        if Current = nil then Exit;

        {Update Attribute}
        AAttribute.DataSize:=ASize;
        AAttribute.AttributeSize:=AAttribute.CalculatedSize(FVolumeVersion);

        {Size Record}
        if not SizeRecord(Current,Current.CalculatedSize(FVolumeVersion)) then Exit;
       end;
     end
    else if ASize < AAttribute.DataSize then
     begin
      {Smaller}
      {$IFDEF NTFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SizeAttribute (Smaller)');
      {$ENDIF}

      {Get Record}
      Current:=AAttribute.Parent;
      if Current = nil then Exit;

      {Update Attribute}
      AAttribute.DataSize:=ASize;
      AAttribute.AttributeSize:=AAttribute.CalculatedSize(FVolumeVersion);

      {Size Record}
      if not SizeRecord(Current,Current.CalculatedSize(FVolumeVersion)) then Exit;
     end
    else if ASize > AAttribute.DataSize then
     begin
      {Larger}
      {$IFDEF NTFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SizeAttribute (Larger)');
      {$ENDIF}

      {Get Record}
      Current:=AAttribute.Parent;
      if Current = nil then Exit;

      {Check Free}
      if NTFSRoundLongWordTo8Bytes(ASize - AAttribute.DataSize) < Current.RecordFree then
       begin
        {Space Available}
        {Update Attribute}
        AAttribute.DataSize:=ASize;
        AAttribute.AttributeSize:=AAttribute.CalculatedSize(FVolumeVersion);

        {Size Record}
        if not SizeRecord(Current,Current.CalculatedSize(FVolumeVersion)) then Exit;
       end
      else
       begin
        {No Space Available}
        if FAttrDefs = nil then Exit;

        {Get AttrDef}
        AttrDef:=FAttrDefs.GetAttrDef(AAttribute.AttributeType,ntfsAnyName);
        if AttrDef = nil then Exit;

        {Check Resident}
        if not(AttrDef.IsResident) then
         begin
          {Can Convert}
          {Convert Attribute}
          if not ConvertAttribute(ARecord,AAttribute) then Exit;

          {Size Attribute}
          if not SizeAttribute(ARecord,AAttribute,ASize) then Exit;
         end
        else
         begin
          {Cannot Convert}
          {Get Size}
          Size:=Current.AttributeSize(FVolumeVersion,AAttribute.AttributeType);

          {Check Attribute}
          if NTFSRoundLongWordTo8Bytes(AAttribute.AttributeSize + (ASize - AAttribute.DataSize)) < Size then
           begin
            {Can Fit}

            {Update Attribute}
            AAttribute.DataSize:=ASize;
            AAttribute.AttributeSize:=AAttribute.CalculatedSize(FVolumeVersion);

            {Size Record}
            if not SizeRecord(Current,Current.CalculatedSize(FVolumeVersion)) then Exit;
           end
          else
           begin
            {Cannot Fit} {Resize Fails if not Movable} {Does not include attributes where first instance cannot be moved}
            if AAttribute.IsUnmovable then Exit;

            {Can Move}
            {Update Attribute}
            AAttribute.DataSize:=ASize;
            AAttribute.AttributeSize:=AAttribute.CalculatedSize(FVolumeVersion);

            {Size Record}
            if not SizeRecord(Current,Current.CalculatedSize(FVolumeVersion)) then Exit;
           end;
         end;
       end;
     end;

    Result:=True; {Note: If Size is same then just succeed}
   end
  else
   begin
    {Size Run}
    Result:=SizeRun(ARecord,AAttribute,ASize);
   end;
 finally
  FRecords.AttributesWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.SizeRun(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute;const ASize:Int64):Boolean;
{Resize the run to Size bytes and update the attribute and record accordingly}
{Note: The passed attribute must be the first instance}
{Note: Updates StreamSize, StreamUsed, StreamAllocated, InitializedSize and AttributeSize values of Attribute}
{Note: Caller must hold the records and attributes lock}
var
 Size:Int64;                    {New size of the run after rounding to compression unit}
 Count:Int64;                   {Count of clusters in the current run}
 Start:Int64;                   {Starting cluster to be removed}
 Remain:Int64;                  {Remaining clusters to be added or removed}
 Required:Int64;                {Number of clusters required to accomodate the new size}
 Allocated:Int64;               {Number of clusters currently allocated to the run}
 Instance:LongWord;

 Current:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;  {Attribute instance containing Run to Size}
begin
 {}
 Result:=False;

 if not FRecords.RunsWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if AAttribute = nil then Exit;

  //To Do //Remember to check if FMaster and if can be expanded (Reserved MFT)
  //To Do //Handling of InitializedSize
  //To Do //Handling of SparseFlag

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SizeRun - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' Type = ' + IntToHex(AAttribute.AttributeType,8) + ' Attribute = ' + AAttribute.AttributeName + ' Size = ' + IntToStr(ASize));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Resident}
  if AAttribute.NonResident = ntfsAttributeNonResident then
   begin
    {Check Size}
    if ASize = 0 then
     begin
      {Zero}
      if AAttribute.StreamSize > 0 then
       begin
        {Get Allocated}
        Allocated:=(AAttribute.StreamAllocated shr FClusterShiftCount);

        {$IFDEF NTFS_DEBUG}
        if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SizeRun (Zero) - Allocated = ' + IntToHex(Allocated,16));
        {$ENDIF}

        {Get Attribute} {Last Instance} //To Do //If an attribute list exists then instances are sorted by StartVCN //What if there are multiple instances and no list, can that happen ?
        Attribute:=ARecord.GetAttribute(AAttribute.AttributeType,AAttribute.AttributeName,ntfsInstanceLast);
        while Attribute <> nil do
         begin
          {Get Count}
          if not GetRunCount(Attribute,Attribute.StartVCN,Count) then Exit;

          {Check Start}
          if Attribute.StartVCN = 0 then
           begin
            {Remove Run}
            if not RemoveRun(ARecord,Attribute,Attribute.StartVCN,Count,False) then Exit;

            {Get Record}
            Current:=AAttribute.Parent;
            if Current = nil then Exit;

            {Update Attribute}
            Attribute.StreamSize:=0;
            Attribute.StreamUsed:=0;
            Attribute.StreamAllocated:=0;
            Attribute.InitializedSize:=0;
            Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

            {Size Record}
            if not SizeRecord(Current,Current.CalculatedSize(FVolumeVersion)) then Exit;

            Break; {Break to allow success}
           end
          else
           begin
            {Remove Run}
            if not RemoveRun(ARecord,Attribute,Attribute.StartVCN,Count,True) then Exit;
           end;

          {Get Attribute} {Last Instance}
          Attribute:=ARecord.GetAttribute(AAttribute.AttributeType,AAttribute.AttributeName,ntfsInstanceLast);
         end;
       end;
     end
    else if ASize < AAttribute.StreamSize then
     begin
      {Smaller}
      {Check Compressed}
      if (AAttribute.IsCompressed) and (AAttribute.CompressionUnit <> 0) then
       begin
        {Compressed}
        {Get Size}
        Size:=NTFSRoundQuadWordToUnitSize(ASize,(FClusterShiftCount + AAttribute.CompressionUnit),(FClusterSize shl AAttribute.CompressionUnit));

        {Get Required}
        Required:=(Size shr FClusterShiftCount);
        if (Required shl FClusterShiftCount) < Size then Inc(Required);

        {Get Allocated}
        Allocated:=(AAttribute.StreamAllocated shr FClusterShiftCount);

        {$IFDEF NTFS_DEBUG}
        if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SizeRun (Smaller) - Allocated = ' + IntToHex(Allocated,16) + ' Required = ' + IntToHex(Required,16));
        {$ENDIF}

        {Check Difference} {Greater than should never happen because Size is smaller}
        if Required >= Allocated then
         begin
          {Decompress Run} {LastVCN, 1 cluster - Decompress will decompress the unit containing LastVCN (Last Unit)}
          if not DecompressRun(ARecord,AAttribute,(Allocated - 1),1) then Exit;

          {Get Record}
          Current:=AAttribute.Parent;
          if Current = nil then Exit;

          {Update Attribute}
          AAttribute.StreamSize:=ASize;
          AAttribute.StreamUsed:=(ARecord.CalculatedStreamUsed(FVolumeVersion,AAttribute) shl FClusterShiftCount);
          AAttribute.InitializedSize:=ASize; //To Do //How to handle ?
          AAttribute.AttributeSize:=AAttribute.CalculatedSize(FVolumeVersion);

          {Size Record}
          if not SizeRecord(Current,Current.CalculatedSize(FVolumeVersion)) then Exit;
         end
        else
         begin
          {Get Remain}
          Remain:=(Allocated - Required);

          {Get Attribute} {Last Instance} //To Do //If an attribute list exists then instances are sorted by StartVCN //What if there are multiple instances and no list, can that happen ?
          Attribute:=ARecord.GetAttribute(AAttribute.AttributeType,AAttribute.AttributeName,ntfsInstanceLast);
          while Attribute <> nil do
           begin
            {Get Count}
            if not GetRunCount(Attribute,Attribute.StartVCN,Count) then Exit;

            {Get Start}
            Start:=Attribute.StartVCN;
            if Count > Remain then Start:=(Attribute.StartVCN + (Count - Remain));
            if Count > Remain then Count:=Remain;

            {Remove Run}
            if not RemoveRun(ARecord,Attribute,Start,Count,(Attribute.StartVCN <> 0)) then Exit;  //To Do //This may not be correct //May delete the Attribute when if should not

            {Update Postition}
            Dec(Remain,Count);
            if Remain = 0 then Break; {Break to allow success}

            {Get Attribute} {Last Instance}
            Attribute:=ARecord.GetAttribute(AAttribute.AttributeType,AAttribute.AttributeName,ntfsInstanceLast);
           end;

          {Decompress Run} {LastVCN, 1 cluster - Decompress will decompress the unit containing LastVCN (Last Unit)}
          if not DecompressRun(ARecord,AAttribute,(Required - 1),1) then Exit;

          {Get Record}
          Current:=AAttribute.Parent;
          if Current = nil then Exit;

          {Update Attribute}
          AAttribute.StreamSize:=ASize;
          AAttribute.StreamUsed:=(ARecord.CalculatedStreamUsed(FVolumeVersion,AAttribute) shl FClusterShiftCount);
          AAttribute.InitializedSize:=ASize; //To Do //How to handle ?
          AAttribute.StreamAllocated:=(Required shl FClusterShiftCount);
          AAttribute.AttributeSize:=AAttribute.CalculatedSize(FVolumeVersion);

          {Size Record}
          if not SizeRecord(Current,Current.CalculatedSize(FVolumeVersion)) then Exit;
         end;
       end
      else
       begin
        {Uncompressed}
        {Get Required}
        Required:=(ASize shr FClusterShiftCount);
        if (Required shl FClusterShiftCount) < ASize then Inc(Required);

        {Get Allocated}
        Allocated:=(AAttribute.StreamAllocated shr FClusterShiftCount);

        {$IFDEF NTFS_DEBUG}
        if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SizeRun (Smaller) - Allocated = ' + IntToHex(Allocated,16) + ' Required = ' + IntToHex(Required,16));
        {$ENDIF}

        {Check Difference} {Greater than should never happen because Size is smaller}
        if Required >= Allocated then
         begin
          {Get Record}
          Current:=AAttribute.Parent;
          if Current = nil then Exit;

          {Update Attribute}
          AAttribute.StreamSize:=ASize;
          AAttribute.InitializedSize:=ASize; //To Do //How to handle
          AAttribute.AttributeSize:=AAttribute.CalculatedSize(FVolumeVersion);

          {Size Record}
          if not SizeRecord(Current,Current.CalculatedSize(FVolumeVersion)) then Exit;
         end
        else
         begin
          {Get Remain}
          Remain:=(Allocated - Required);

          {Get Attribute} {Last Instance} //To Do //If an attribute list exists then instances are sorted by StartVCN //What if there are multiple instances and no list, can that happen ?
          Attribute:=ARecord.GetAttribute(AAttribute.AttributeType,AAttribute.AttributeName,ntfsInstanceLast);
          while Attribute <> nil do
           begin
            {Get Count}
            if not GetRunCount(Attribute,Attribute.StartVCN,Count) then Exit;

            {Get Start}
            Start:=Attribute.StartVCN;
            if Count > Remain then Start:=(Attribute.StartVCN + (Count - Remain));
            if Count > Remain then Count:=Remain;

            {Remove Run}
            if not RemoveRun(ARecord,Attribute,Start,Count,(Attribute.StartVCN <> 0)) then Exit; //To Do //This may not be correct //May delete the Attribute when if should not

            {Update Postition}
            Dec(Remain,Count);
            if Remain = 0 then Break; {Break to allow success}

            {Get Attribute} {Last Instance}
            Attribute:=ARecord.GetAttribute(AAttribute.AttributeType,AAttribute.AttributeName,ntfsInstanceLast);
           end;

          {Get Record}
          Current:=AAttribute.Parent;
          if Current = nil then Exit;

          {Update Attribute}
          AAttribute.StreamSize:=ASize;
          AAttribute.InitializedSize:=ASize; //To Do //How to handle ?
          AAttribute.StreamAllocated:=(Required shl FClusterShiftCount);
          AAttribute.AttributeSize:=AAttribute.CalculatedSize(FVolumeVersion);

          {Size Record}
          if not SizeRecord(Current,Current.CalculatedSize(FVolumeVersion)) then Exit;
         end;
       end;
     end
    else if ASize > AAttribute.StreamSize then
     begin
      {Larger}
      {Check Compressed}
      if (AAttribute.IsCompressed) and (AAttribute.CompressionUnit <> 0) then
       begin
        {Compressed}
        {Get Size}
        Size:=NTFSRoundQuadWordToUnitSize(ASize,(FClusterShiftCount + AAttribute.CompressionUnit),(FClusterSize shl AAttribute.CompressionUnit));

        {Get Required}
        Required:=(Size shr FClusterShiftCount);
        if (Required shl FClusterShiftCount) < Size then Inc(Required);

        {Get Allocated}
        Allocated:=(AAttribute.StreamAllocated shr FClusterShiftCount);

        {$IFDEF NTFS_DEBUG}
        if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SizeRun (Larger) - Allocated = ' + IntToHex(Allocated,16) + ' Required = ' + IntToHex(Required,16));
        {$ENDIF}

        {Check Difference} {Less than may happen}
        if Required <= Allocated then
         begin
          {Decompress Run} {LastVCN, 1 cluster - Decompress will decompress the unit containing LastVCN (Last Unit)}
          if not DecompressRun(ARecord,AAttribute,(Allocated - 1),1) then Exit;

          {Get Record}
          Current:=AAttribute.Parent;
          if Current = nil then Exit;

          {Update Attribute}
          AAttribute.StreamSize:=ASize;
          AAttribute.StreamUsed:=(ARecord.CalculatedStreamUsed(FVolumeVersion,AAttribute) shl FClusterShiftCount);
          AAttribute.InitializedSize:=ASize; //To Do //How to handle ?
          AAttribute.AttributeSize:=AAttribute.CalculatedSize(FVolumeVersion);

          {Size Record}
          if not SizeRecord(Current,Current.CalculatedSize(FVolumeVersion)) then Exit;
         end
        else
         begin
          {Add Run} //To Do //May request MFT even if Bitmap attribute, check (ARecord = FMaster) only ? - No ?
          if not AddRun(ARecord,AAttribute,(Required - Allocated),(ARecord = FMaster) and (AAttribute.AttributeType = ntfsAttrTypeData),False) then Exit;

          {Get Record}
          Current:=AAttribute.Parent;
          if Current = nil then Exit;

          {Update Attribute}
          AAttribute.StreamSize:=ASize;
          AAttribute.StreamUsed:=(ARecord.CalculatedStreamUsed(FVolumeVersion,AAttribute) shl FClusterShiftCount);
          AAttribute.InitializedSize:=ASize; //To Do //How to handle ?
          AAttribute.StreamAllocated:=(Required shl FClusterShiftCount);
          AAttribute.AttributeSize:=AAttribute.CalculatedSize(FVolumeVersion);

          {Size Record}
          if not SizeRecord(Current,Current.CalculatedSize(FVolumeVersion)) then Exit;
         end;
       end
      else
       begin
        {Uncompressed}
        {Check Mft} {Data Attribute only}
        if (ARecord = FMaster) and (AAttribute.AttributeType = ntfsAttrTypeData) then
         begin
          {Get Size} {Increase MFT allocation in 16k blocks}
          Size:=NTFSRoundQuadWordToUnitSize(ASize,14,16384);

          {Get Required}
          Required:=(Size shr FClusterShiftCount);
          if (Required shl FClusterShiftCount) < Size then Inc(Required);
         end
        else
         begin
          {Get Required}
          Required:=(ASize shr FClusterShiftCount);
          if (Required shl FClusterShiftCount) < ASize then Inc(Required);
         end;

        {Get Allocated}
        Allocated:=(AAttribute.StreamAllocated shr FClusterShiftCount);

        {$IFDEF NTFS_DEBUG}
        if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SizeRun (Larger) - Allocated = ' + IntToHex(Allocated,16) + ' Required = ' + IntToHex(Required,16));
        {$ENDIF}

        {Check Difference} {Less than may happen}
        if Required <= Allocated then
         begin
          {Get Record}
          Current:=AAttribute.Parent;
          if Current = nil then Exit;

          {Update Attribute}
          AAttribute.StreamSize:=ASize;
          AAttribute.InitializedSize:=ASize; //To Do //How to handle
          AAttribute.AttributeSize:=AAttribute.CalculatedSize(FVolumeVersion);

          {Size Record}
          if not SizeRecord(Current,Current.CalculatedSize(FVolumeVersion)) then Exit;
         end
        else
         begin
          {Add Run} //To Do //May request MFT even if Bitmap attribute, check (ARecord = FMaster) only ? - No ?
          if not AddRun(ARecord,AAttribute,(Required - Allocated),(ARecord = FMaster) and (AAttribute.AttributeType = ntfsAttrTypeData),False) then Exit;

          {Get Record}
          Current:=AAttribute.Parent;
          if Current = nil then Exit;

          {Update Attribute}
          AAttribute.StreamSize:=ASize;
          AAttribute.InitializedSize:=ASize; //To Do //How to handle ?
          AAttribute.StreamAllocated:=(Required shl FClusterShiftCount);
          AAttribute.AttributeSize:=AAttribute.CalculatedSize(FVolumeVersion);

          {Size Record}
          if not SizeRecord(Current,Current.CalculatedSize(FVolumeVersion)) then Exit;
         end;
       end;
     end;

    Result:=True; {Note: If Size is same then just succeed}
   end;
 finally
  FRecords.RunsWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.EncryptEntry(AParent,AEntry:TDiskEntry):Boolean;
var
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if AParent = nil then Exit;
  if AEntry = nil then Exit;
  if AEntry = FRoot then Exit; {Cannot encrypt root}

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.EncryptEntry - Entry = ' + AEntry.Name);
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Relative}
  if (AEntry.Attributes and (faDot or faDotDot)) <> faNone then Exit;

  {Get Origin}
  if not FRecords.WriterLock then Exit;
  try
   if TNTFSDiskEntry(AEntry).Origin = nil then Exit;
   Origin:=TNTFSDiskEntry(AEntry).Origin.Origin;
   if Origin = nil then Exit;

   {Check Metafile}
   if Origin.Metafile then Exit; {Cannot encrypt metafile}

   {Check Entry} {Exclude Stream and Directory}
   if (AEntry.Attributes and faMatchMask) <> faFile then Exit;

   //To Do //Does Windows encrypt the streams or just the file ?
  finally
   FRecords.WriterUnlock;
  end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.DecryptEntry(AParent,AEntry:TDiskEntry):Boolean;
var
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if AParent = nil then Exit;
  if AEntry = nil then Exit;
  if AEntry = FRoot then Exit; {Cannot decrypt root}

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.EncryptEntry - Entry = ' + AEntry.Name);
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Relative}
  if (AEntry.Attributes and (faDot or faDotDot)) <> faNone then Exit;

  {Get Origin}
  if not FRecords.WriterLock then Exit;
  try
   if TNTFSDiskEntry(AEntry).Origin = nil then Exit;
   Origin:=TNTFSDiskEntry(AEntry).Origin.Origin;
   if Origin = nil then Exit;

   {Check Metafile}
   if Origin.Metafile then Exit; {Cannot decrypt metafile}

   {Check Entry} {Exclude Stream and Directory}
   if (AEntry.Attributes and faMatchMask) <> faFile then Exit;

   //To Do //Does Windows encrypt the streams or just the file ?
  finally
   FRecords.WriterUnlock;
  end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.CompressEntry(AParent,AEntry:TDiskEntry):Boolean;
var
 Instance:Integer;
 Entry:TNTFSDiskEntry;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if AParent = nil then Exit;
  if AEntry = nil then Exit;
  if AEntry = FRoot then Exit; {Cannot compress root}

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CompressEntry - Entry = ' + AEntry.Name);
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Relative}
  if (AEntry.Attributes and (faDot or faDotDot)) <> faNone then Exit;

  {Get Origin}
  if not FRecords.WriterLock then Exit;
  try
   if TNTFSDiskEntry(AEntry).Origin = nil then Exit;
   Origin:=TNTFSDiskEntry(AEntry).Origin.Origin;
   if Origin = nil then Exit;

   {Check Metafile}
   if Origin.Metafile then Exit; {Cannot compress metafile}

   {Check Entry} {Exclude Stream and Directory}
   if (AEntry.Attributes and faMatchMask) <> faFile then Exit;

   {Get Attribute}
   Instance:=1;
   Attribute:=Origin.GetAttribute(ntfsAttrTypeData,ntfsBlankName,Instance); {ntfsAnyName (Windows only compresses the unnamed stream)}
   while Attribute <> nil do
    begin
     {Exclude Multiple Instances}
     if Attribute.StartVCN = 0 then
      begin
       {Compress Attribute}
       if not CompressAttribute(Origin,Attribute) then Exit;
      end;

     {Get Attribute}
     Inc(Instance);
     Attribute:=Origin.GetAttribute(ntfsAttrTypeData,ntfsBlankName,Instance); {ntfsAnyName (Windows only compresses the unnamed stream)}
    end;

   {Set Records}
   if not SetRecords(Origin) then Exit;

   {Get Attribute}
   Attribute:=Origin.GetAttribute(ntfsAttrTypeData,ntfsBlankName,ntfsInstanceFirst);
   if Attribute = nil then Exit;

   {Update Links} {Links belonging to other Entries}
   if Origin.Links <> nil then
    begin
     Entry:=Origin.Links.FirstEntry;
     while Entry <> nil do
      begin
       if Entry <> AEntry then if not Attribute.UpdateEntry(Entry) then Exit;

       Entry:=Entry.NextEntry;
      end;
    end;

   {Update Entry}
   Result:=Attribute.UpdateEntry(TNTFSDiskEntry(AEntry));
  finally
   FRecords.WriterUnlock;
  end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.DecompressEntry(AParent,AEntry:TDiskEntry):Boolean;
var
 Instance:Integer;
 Entry:TNTFSDiskEntry;
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FEntries.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if AParent = nil then Exit;
  if AEntry = nil then Exit;
  if AEntry = FRoot then Exit; {Cannot decompress root}

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.DecompressEntry - Entry = ' + AEntry.Name);
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Relative}
  if (AEntry.Attributes and (faDot or faDotDot)) <> faNone then Exit;

  {Get Origin}
  if not FRecords.WriterLock then Exit;
  try
   if TNTFSDiskEntry(AEntry).Origin = nil then Exit;
   Origin:=TNTFSDiskEntry(AEntry).Origin.Origin;
   if Origin = nil then Exit;

   {Check Metafile}
   if Origin.Metafile then Exit; {Cannot decompress metafile}

   {Check Entry} {Exclude Stream and Directory}
   if (AEntry.Attributes and faMatchMask) <> faFile then Exit;

   {Get Attribute}
   Instance:=1;
   Attribute:=Origin.GetAttribute(ntfsAttrTypeData,ntfsBlankName,Instance); {ntfsAnyName (Windows only compresses the unnamed stream)}
   while Attribute <> nil do
    begin
     {Exclude Multiple Instances}
     if Attribute.StartVCN = 0 then
      begin
       {Decompress Attribute}
       if not DecompressAttribute(Origin,Attribute) then Exit;
      end;

     {Get Attribute}
     Inc(Instance);
     Attribute:=Origin.GetAttribute(ntfsAttrTypeData,ntfsBlankName,Instance); {ntfsAnyName (Windows only compresses the unnamed stream)}
    end;

   {Set Records}
   if not SetRecords(Origin) then Exit;

   {Get Attribute}
   Attribute:=Origin.GetAttribute(ntfsAttrTypeData,ntfsBlankName,ntfsInstanceFirst);
   if Attribute = nil then Exit;

   {Update Links} {Links belonging to other Entries}
   if Origin.Links <> nil then
    begin
     Entry:=Origin.Links.FirstEntry;
     while Entry <> nil do
      begin
       if Entry <> AEntry then if not Attribute.UpdateEntry(Entry) then Exit;

       Entry:=Entry.NextEntry;
      end;
    end;

   {Update Entry}
   Result:=Attribute.UpdateEntry(TNTFSDiskEntry(AEntry));
  finally
   FRecords.WriterUnlock;
  end;
 finally
  FEntries.WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.ConvertAttribute(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute):Boolean; {Not override}
{Convert the data stream of the supplied attribute to non resident and update the record}
{Caller must check AttrDef to determine if conversion is allowed}
{Note: The passed attribute must be the first instance as multiple instance attributes are non resident}
{Note: Caller must hold the records lock}
var
 Size:LongWord;
 Buffer:Pointer;
 Allocated:Int64;
 Instance:LongWord;
 DataSize:LongWord;
 Current:TNTFSDiskRecord;
begin
 {}
 Result:=False;

 if not FRecords.AttributesWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if AAttribute = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.ConvertAttribute - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' Type = ' + IntToHex(AAttribute.AttributeType,8) + ' Attribute = ' + AAttribute.AttributeName);
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Resident}
  if AAttribute.NonResident = ntfsAttributeResident then
   begin
    {Check Compressed} {Flag Only (May be resident)}
    if AAttribute.IsCompressed then
     begin
      {Compressed} {Allocate Units}
      {Get Size}
      Size:=NTFSRoundLongWordToUnitSize(AAttribute.DataSize,(FClusterShiftCount + ntfsCompressionUnitSize),(FClusterSize shl ntfsCompressionUnitSize));
      DataSize:=AAttribute.DataSize;

      {Get Allocated}
      Allocated:=(Size shr FClusterShiftCount);
      if (Allocated shl FClusterShiftCount) < Size then Inc(Allocated);

      {$IFDEF NTFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.ConvertAttribute - DataSize = '+ IntToStr(DataSize) +  ' Size = ' + IntToStr(Size) + ' Allocated = ' + IntToStr(Allocated));
      {$ENDIF}

      {Check Size}
      if DataSize = 0 then
       begin
        {Update Attribute}
        AAttribute.NonResident:=ntfsAttributeNonResident;
        AAttribute.DataSize:=0;
        AAttribute.StartVCN:=0;
        AAttribute.LastVCN:=ntfsUnknownCluster;
        AAttribute.StreamSize:=0;
        AAttribute.StreamUsed:=0;
        AAttribute.InitializedSize:=0;
        AAttribute.StreamAllocated:=0;
        AAttribute.CompressionUnit:=ntfsCompressionUnitSize;

        {Get Record}
        Current:=AAttribute.Parent;
        if Current = nil then Exit;

        {Update Attribute}
        AAttribute.AttributeSize:=AAttribute.CalculatedSize(FVolumeVersion);

        {Size Record}
        if not SizeRecord(Current,Current.CalculatedSize(FVolumeVersion)) then Exit;

        Result:=True;
       end
      else
       begin
        {Allocate Buffer}
        Buffer:=GetMem(DataSize);
        if Buffer = nil then Exit;
        try
         {Read Attribute} {Non managed attributes}
         Instance:=ntfsInstanceFirst;
         if not AAttribute.IsManaged then if ReadAttribute(ARecord,AAttribute,Buffer^,0,DataSize,Instance,True) <> Integer(DataSize) then Exit;

         {Update Attribute}
         AAttribute.NonResident:=ntfsAttributeNonResident;
         AAttribute.DataSize:=0;
         AAttribute.StartVCN:=0;
         AAttribute.LastVCN:=ntfsUnknownCluster;
         AAttribute.StreamSize:=0;
         AAttribute.StreamUsed:=0;
         AAttribute.InitializedSize:=0;
         AAttribute.StreamAllocated:=0;
         AAttribute.CompressionUnit:=ntfsCompressionUnitSize;

         {Add Run} {Normal}
         if not AddRun(ARecord,AAttribute,Allocated,(ARecord = FMaster) and (AAttribute.AttributeType = ntfsAttrTypeData),False) then Exit;

         {Update Attribute}
         AAttribute.StreamSize:=DataSize;
         AAttribute.StreamUsed:=(Allocated shl FClusterShiftCount);
         AAttribute.InitializedSize:=DataSize;
         AAttribute.StreamAllocated:=(Allocated shl FClusterShiftCount);

         {Get Record}
         Current:=AAttribute.Parent;
         if Current = nil then Exit;

         {Update Attribute}
         AAttribute.AttributeSize:=AAttribute.CalculatedSize(FVolumeVersion);

         {Size Record}
         if not SizeRecord(Current,Current.CalculatedSize(FVolumeVersion)) then Exit;

         {Set Attribute} {Managed attributes}
         if AAttribute.IsManaged then if not SetAttribute(ARecord,AAttribute) then Exit;

         {Write Attribute} {Non managed attributes}
         if not AAttribute.IsManaged then if WriteAttribute(ARecord,AAttribute,Buffer^,0,DataSize,Instance,False) <> Integer(DataSize) then Exit;

         Result:=True;
        finally
        FreeMem(Buffer);
        end;
       end;
     end
    else
     begin
      {Uncompressed} {Allocate Clusters}
      {Get Size}
      Size:=NTFSRoundLongWordToClusterSize(AAttribute.DataSize,FClusterShiftCount,FClusterSize);
      DataSize:=AAttribute.DataSize;

      {Get Allocated}
      Allocated:=(Size shr FClusterShiftCount);
      if (Allocated shl FClusterShiftCount) < Size then Inc(Allocated);

      {$IFDEF NTFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.ConvertAttribute - DataSize = '+ IntToStr(DataSize) +  ' Size = ' + IntToStr(Size) + ' Allocated = ' + IntToStr(Allocated));
      {$ENDIF}

      {Check Size}
      if DataSize = 0 then
       begin
        {Update Attribute}
        AAttribute.NonResident:=ntfsAttributeNonResident;
        AAttribute.DataSize:=0;
        AAttribute.StartVCN:=0;
        AAttribute.LastVCN:=ntfsUnknownCluster;
        AAttribute.StreamSize:=0;
        AAttribute.InitializedSize:=0;
        AAttribute.StreamAllocated:=0;

        {Get Record}
        Current:=AAttribute.Parent;
        if Current = nil then Exit;

        {Update Attribute}
        AAttribute.AttributeSize:=AAttribute.CalculatedSize(FVolumeVersion);

        {Size Record}
        if not SizeRecord(Current,Current.CalculatedSize(FVolumeVersion)) then Exit;

        Result:=True;
       end
      else
       begin
        {Allocate Buffer}
        Buffer:=GetMem(DataSize);
        if Buffer = nil then Exit;
        try
         {Read Attribute} {Non managed attributes}
         Instance:=ntfsInstanceFirst;
         if not AAttribute.IsManaged then if ReadAttribute(ARecord,AAttribute,Buffer^,0,DataSize,Instance,True) <> Integer(DataSize) then Exit;

         {Update Attribute}
         AAttribute.NonResident:=ntfsAttributeNonResident;
         AAttribute.DataSize:=0;
         AAttribute.StartVCN:=0;
         AAttribute.LastVCN:=ntfsUnknownCluster;
         AAttribute.StreamSize:=0;
         AAttribute.InitializedSize:=0;
         AAttribute.StreamAllocated:=0;

         {Add Run} {Normal}
         if not AddRun(ARecord,AAttribute,Allocated,(ARecord = FMaster) and (AAttribute.AttributeType = ntfsAttrTypeData),False) then Exit;

         {Update Attribute}
         AAttribute.StreamSize:=DataSize;
         AAttribute.InitializedSize:=DataSize;
         AAttribute.StreamAllocated:=(Allocated shl FClusterShiftCount);

         {Get Record}
         Current:=AAttribute.Parent;
         if Current = nil then Exit;

         {Update Attribute}
         AAttribute.AttributeSize:=AAttribute.CalculatedSize(FVolumeVersion);

         {Size Record}
         if not SizeRecord(Current,Current.CalculatedSize(FVolumeVersion)) then Exit;

         {Set Attribute} {Managed attributes}
         if AAttribute.IsManaged then if not SetAttribute(ARecord,AAttribute) then Exit;

         {Write Attribute} {Non managed attributes}
         if not AAttribute.IsManaged then if WriteAttribute(ARecord,AAttribute,Buffer^,0,DataSize,Instance,False) <> Integer(DataSize) then Exit;

         Result:=True;
        finally
        FreeMem(Buffer);
        end;
       end;
     end;
   end;
 finally
  FRecords.AttributesWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.EncryptAttribute(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute):Boolean;
{Note: Caller must hold the records lock}
begin
 {}
 Result:=False;

 if not FRecords.AttributesWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if FAttrDefs = nil then Exit;
  if AAttribute = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.EncryptAttribute - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' Type = ' + IntToHex(AAttribute.AttributeType,8) + ' Attribute = ' + AAttribute.AttributeName);
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  //To Do
 finally
  FRecords.AttributesWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.DecryptAttribute(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute):Boolean;
{Note: Caller must hold the records lock}
begin
 {}
 Result:=False;

 if not FRecords.AttributesWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if FAttrDefs = nil then Exit;
  if AAttribute = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.DecryptAttribute - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' Type = ' + IntToHex(AAttribute.AttributeType,8) + ' Attribute = ' + AAttribute.AttributeName);
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  //To Do
 finally
  FRecords.AttributesWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.CompressAttribute(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute):Boolean;
{Compress the data stream of the supplied attribute and update the record}
{Handles checking of AttrDef to determine if compression is allowed}
{Note: The passed attribute must be the first instance}
{Note: Caller must hold the records lock}
var
 Size:Int64;
 Required:Int64;
 Allocated:Int64;
 Instance:Integer;
 AttrDef:TNTFSAttrDef;
 Current:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FRecords.AttributesWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if FAttrDefs = nil then Exit;
  if AAttribute = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CompressAttribute - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' Type = ' + IntToHex(AAttribute.AttributeType,8) + ' Attribute = ' + AAttribute.AttributeName);
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check AttrDef}
  AttrDef:=FAttrDefs.GetAttrDef(AAttribute.AttributeType,ntfsAnyName);
  if AttrDef = nil then Exit;
  if AttrDef.IsResident then Exit;
  if AttrDef.IsUncompressed then Exit;

  {Check Compressed} {Flag Only (May be resident)}
  if not AAttribute.IsCompressed then
   begin
    {Check Resident}
    if AAttribute.NonResident = ntfsAttributeResident then
     begin
      {Update Attribute}
      AAttribute.IsCompressed:=True;

      {Do not update size}
      Result:=True;
     end
    else
     begin
      {Get Size}
      Size:=NTFSRoundQuadWordToUnitSize(AAttribute.StreamSize,(FClusterShiftCount + ntfsCompressionUnitSize),(FClusterSize shl ntfsCompressionUnitSize));

      {Get Required}
      Required:=(Size shr FClusterShiftCount);
      if (Required shl FClusterShiftCount) < Size then Inc(Required);

      {Get Allocated}
      Allocated:=(AAttribute.StreamAllocated shr FClusterShiftCount);

      {$IFDEF NTFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CompressAttribute - Size = '+ IntToStr(Size) +  ' Required = ' + IntToStr(Required) + ' Allocated = ' + IntToStr(Allocated));
      {$ENDIF}

      {Add Run} {Normal}
      if (AAttribute.StreamSize > 0) and (Required > Allocated) then if not AddRun(ARecord,AAttribute,Required - Allocated,False,False) then Exit;

      {Get Attribute}
      Instance:=1;
      Attribute:=ARecord.GetAttribute(AAttribute.AttributeType,AAttribute.AttributeName,Instance);
      while Attribute <> nil do
       begin
        if Attribute.StartVCN = 0 then
         begin
          {Update Attribute}
          Attribute.IsCompressed:=True;
          Attribute.CompressionUnit:=ntfsCompressionUnitSize;
         end
        else
         begin
          {Update Attribute}
          Attribute.IsCompressed:=True;
         end;

        {Get Attribute}
        Inc(Instance);
        Attribute:=ARecord.GetAttribute(AAttribute.AttributeType,AAttribute.AttributeName,Instance);
       end;

      {Update Attribute}
      if AAttribute.StreamSize > 0 then AAttribute.StreamUsed:=(Required shl FClusterShiftCount);
      if AAttribute.StreamSize > 0 then AAttribute.StreamAllocated:=(Required shl FClusterShiftCount);

      {Get Record}
      Current:=AAttribute.Parent;
      if Current = nil then Exit;

      {Update Attribute}
      AAttribute.AttributeSize:=AAttribute.CalculatedSize(FVolumeVersion);

      {Size Record}
      if not SizeRecord(Current,Current.CalculatedSize(FVolumeVersion)) then Exit;

      {Compress Run} {Entire Run}
      if AAttribute.StreamSize > 0 then if not CompressRun(ARecord,AAttribute,AAttribute.StartVCN,(AAttribute.StreamAllocated shr FClusterShiftCount)) then Exit;

      Result:=True;
     end;
   end;
 finally
  FRecords.AttributesWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.DecompressAttribute(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute):Boolean;
{Decompress the data stream of the supplied attribute and update the record}
{Note: The passed attribute must be the first instance}
{Note: Caller must hold the records lock}
var
 Size:Int64;
 Required:Int64;
 Allocated:Int64;
 Instance:Integer;
 Current:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;

 if not FRecords.AttributesWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if AAttribute = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.DecompressAttribute - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' Type = ' + IntToHex(AAttribute.AttributeType,8) + ' Attribute = ' + AAttribute.AttributeName);
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Compressed} {Flag Only (May be resident)}
  if AAttribute.IsCompressed then
   begin
    {Check Resident}
    if AAttribute.NonResident = ntfsAttributeResident then
     begin
      {Update Attribute}
      AAttribute.IsCompressed:=False;

      {Do not update size}
      Result:=True;
     end
    else
     begin
      {Decompress Run} {Entire Run}
      if AAttribute.StreamSize > 0 then if not DecompressRun(ARecord,AAttribute,AAttribute.StartVCN,(AAttribute.StreamAllocated shr FClusterShiftCount)) then Exit;

      {Get Size}
      Size:=NTFSRoundQuadWordToClusterSize(AAttribute.StreamSize,FClusterShiftCount,FClusterSize);

      {Get Required}
      Required:=(Size shr FClusterShiftCount);
      if (Required shl FClusterShiftCount) < Size then Inc(Required);

      {Get Allocated}
      Allocated:=(AAttribute.StreamAllocated shr FClusterShiftCount);

      {$IFDEF NTFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.DecompressAttribute - Size = '+ IntToStr(Size) +  ' Required = ' + IntToStr(Required) + ' Allocated = ' + IntToStr(Allocated));
      {$ENDIF}

      {Get Attribute}
      Attribute:=ARecord.GetAttribute(AAttribute.AttributeType,AAttribute.AttributeName,ntfsInstanceLast);
      if Attribute = nil then Exit;

      {Remove Run}
      if (AAttribute.StreamSize > 0) and (Required < Allocated) then if not RemoveRun(ARecord,Attribute,Required,Allocated - Required,(Attribute.StartVCN <> 0)) then Exit;   //To Do //This may not be correct //May delete the Attribute when if should not

      {Get Attribute}
      Instance:=1;
      Attribute:=ARecord.GetAttribute(AAttribute.AttributeType,AAttribute.AttributeName,Instance);
      while Attribute <> nil do
       begin
        if Attribute.StartVCN = 0 then
         begin
          {Update Attribute}
          Attribute.IsCompressed:=False;
          Attribute.CompressionUnit:=0;
         end
        else
         begin
          {Update Attribute}
          Attribute.IsCompressed:=False;
         end;

        {Get Attribute}
        Inc(Instance);
        Attribute:=ARecord.GetAttribute(AAttribute.AttributeType,AAttribute.AttributeName,Instance);
       end;

      {Update Attribute}
      if AAttribute.StreamSize > 0 then AAttribute.StreamUsed:=0;
      if AAttribute.StreamSize > 0 then AAttribute.StreamAllocated:=(Required shl FClusterShiftCount);

      {Get Record}
      Current:=AAttribute.Parent;
      if Current = nil then Exit;

      {Update Attribute}
      AAttribute.AttributeSize:=AAttribute.CalculatedSize(FVolumeVersion);

      {Size Record}
      if not SizeRecord(Current,Current.CalculatedSize(FVolumeVersion)) then Exit;

      Result:=True;
     end;
   end;
 finally
  FRecords.AttributesWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.ConvertRun(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute;const AVCN,ACount:Int64;ASparse:Boolean):Boolean;
{Convert count clusters of the run of the supplied attribute from sparse to normal or normal to sparse}
{VCN is the starting point for the conversion}
{Count is the number of clusters to convert}
{Sparse indicates whether to convert to normal or sparse}
{Note: The passed attribute must be the first instance}
{Note: Caller must hold the records and attributes lock}
var
 VCN:Int64;         {Current VCN to convert}
 Count:Int64;       {Clusters to be converted in current run}
 Remain:Int64;      {Clusters remaining to be converted}
 Length:Int64;      {Number of clusters allocated}
 Cluster:Int64;     {Starting cluster number allocated}
 StartVCN:Int64;    {Starting VCN of the current run}

 Run:TNTFSDiskRun;              {Current run to be converted}
 Instance:LongWord;
 Current:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;  {Attribute instance containing the run to convert}
begin
 {}
 Result:=False;

 if not FRecords.RunsWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if AAttribute = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.ConvertRun - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' Type = ' + IntToHex(AAttribute.AttributeType,8) + ' Attribute = ' + AAttribute.AttributeName + ' VCN = ' + IntToStr(AVCN) + ' Count = ' + IntToStr(ACount) + ' Sparse = ' + BoolToStr(ASparse));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Count}
  if ACount = 0 then Exit;

  {Check Resident}
  if AAttribute.NonResident = ntfsAttributeNonResident then
   begin
    {Get Position}
    VCN:=AVCN;
    Remain:=ACount;
    while Remain > 0 do
     begin
      {Get Attribute}
      Instance:=ntfsInstanceFirst;
      Attribute:=ARecord.GetAttributeByVCN(AAttribute,VCN,Instance);
      if Attribute = nil then Exit;

      {Get Run}
      Run:=Attribute.GetRun(VCN,StartVCN);
      if Run = nil then Exit;
      if Run.IsLast then Exit;

      {Get Count}
      Count:=Remain;
      if Count > Run.Length then Count:=Run.Length;

      {$IFDEF NTFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.ConvertRun - VCN = ' + IntToStr(VCN) + ' Count = ' + IntToStr(Count) + ' Remain = ' + IntToStr(Remain));
      {$ENDIF}

      {Check Sparse}
      if (ASparse <> Run.IsSparse) then
       begin
        {Check Start}
        if VCN > StartVCN then
         begin
          {Split Run}
          if not Attribute.SplitRun(Run,(VCN - StartVCN)) then Exit;

          {Do not update position}
         end
        else
         begin
          if Run.IsSparse then
           begin
            {Convert to Normal}
            {Get Length}
            Length:=Count;

            {Alloc Clusters}
            Cluster:=ntfsUnknownCluster;
            if not AllocClusters(Cluster,Length,(ARecord = FMaster) and (AAttribute.AttributeType = ntfsAttrTypeData)) then Exit;

            {Zero Clusters}
            if not FillClusters(Cluster,Length,0) then Exit;

            {Get Count}
            Count:=Remain;
            if Count > Length then Count:=Length;

            {Check Count} {Split Run}
            if Count < Run.Length then if not Attribute.SplitRun(Run,Count) then Exit;

            {Update Run}
            Run.Start:=Cluster;
            Attribute.UpdateRun(Run);

            {Coalesce Run}
            //if not Attribute.CoalesceRun(Run) then Exit; //To Do //Testing
            if not Attribute.CoalesceRun(nil) then Exit;
           end
          else
           begin
            {Convert to Sparse}
            {Get Cluster}
            if not GetRunCluster(Attribute,VCN,Cluster,Length) then Exit;

            {Get Count}
            Count:=Remain;
            if Count > Length then Count:=Length;

            {Check Count} {Split Run}
            if Count < Run.Length then if not Attribute.SplitRun(Run,Count) then Exit;

            {Release Clusters}
            if not ReleaseClusters(Cluster,Count) then Exit;

            {Update Run}
            Run.Start:=ntfsUnknownCluster;
            Attribute.UpdateRun(Run);

            {Coalesce Run}
            //if not Attribute.CoalesceRun(Run) then Exit; //To Do //Testing
            if not Attribute.CoalesceRun(nil) then Exit;
           end;

          {Get Record}
          Current:=Attribute.Parent;
          if Current = nil then Exit;

          {Update Attribute}
          Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);

          {Size Record}
          if not SizeRecord(Current,Current.CalculatedSize(FVolumeVersion)) then Exit;

          {Update Position}
          Inc(VCN,Count);
          Dec(Remain,Count);
         end;
       end
      else
       begin
        {Update Position}
        Inc(VCN,Count);
        Dec(Remain,Count);
       end;
     end;

    Result:=True;
   end;
 finally
  FRecords.RunsWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.CompressRun(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute;const AVCN,ACount:Int64):Boolean;
{Compress count clusters of the run of the supplied attribute and instances}
{VCN is the starting point for the compression}
{Count is the number of clusters to compress}
{Will actually compress compression unit size blocks starting from the nearest unit}
{Note: The passed attribute must be the first instance}
{Note: Caller must hold the records and attributes lock}
var
 VCN:Int64;
 Count:Int64;
 Remain:Int64;

 UnitNo:Int64;                  {Compression Unit for current VCN}
 UnitVCN:Int64;                 {Starting VCN of Compression Unit}
 UnitCount:Int64;               {Count of Clusters available in Unit}
 UnitLength:Int64;              {Length of Compression Unit in Clusters}
 UnitRemain:Int64;              {Bytes remaining in Compression Unit}
 CompressBuffer:Pointer;        {Buffer for Compressed Data}
 DecompressBuffer:Pointer;      {Buffer for Raw read from Run}

 Instance:LongWord;
 UnitSize:LongWord;             {Size in bytes of Compressed Data}
 UnitClusters:Int64;            {Size in clusters of Compressed Data}
begin
 {}
 Result:=False;

 if not FRecords.RunsWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if AAttribute = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CompressRun - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' Type = ' + IntToHex(AAttribute.AttributeType,8) + ' Attribute = ' + AAttribute.AttributeName + ' VCN = ' + IntToStr(AVCN) + ' Count = ' + IntToStr(ACount));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Count}
  if ACount = 0 then Exit;

  {Check Resident}
  if AAttribute.NonResident = ntfsAttributeNonResident then
   begin
    {Check Compressed} {Do not check for uncompressed (CompressionUnit = 0)}
    if (AAttribute.IsCompressed) and (AAttribute.CompressionUnit <> 0) then
     begin
      {Get Buffer}
      CompressBuffer:=AllocCompressionBuffer(AAttribute.CompressionUnit,False);
      if CompressBuffer = nil then Exit;
      try
       {Get Buffer}
       DecompressBuffer:=AllocDecompressionBuffer(AAttribute.CompressionUnit,False);
       if DecompressBuffer = nil then Exit;
       try
        {Get Position}
        VCN:=AVCN;
        Remain:=ACount;
        Instance:=ntfsInstanceFirst;
        while Remain > 0 do
         begin
          {Get Unit}
          if not GetRunUnit(AAttribute,VCN,UnitNo,UnitCount) then Exit;

          {Get Count}
          Count:=Remain;
          if Count > UnitCount then Count:=UnitCount; {Min does not support Int64}

          {$IFDEF NTFS_DEBUG}
          if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CompressRun - VCN = ' + IntToStr(VCN) + ' UnitNo = ' + IntToStr(UnitNo) + ' UnitCount = ' + IntToStr(UnitCount));
          {$ENDIF}

          {Check Unit}
          if not GetUnitCompressed(ARecord,AAttribute,UnitNo,UnitVCN,UnitLength) then
           begin
            {$IFDEF NTFS_DEBUG}
            if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CompressRun - UnitVCN = ' + IntToStr(UnitVCN) + ' UnitLength = ' + IntToStr(UnitLength) + ' Count = ' + IntToStr(Count));
            {$ENDIF}

            {Read Run}
            if ReadRun(ARecord,AAttribute,DecompressBuffer^,UnitVCN,UnitLength,Instance,True,True) <> UnitLength then Exit;

            {Get Remain}
            UnitRemain:=Min64(UnitLength shl FClusterShiftCount,AAttribute.StreamSize - (UnitVCN shl FClusterShiftCount));

            {$IFDEF NTFS_DEBUG}
            if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CompressRun - UnitRemain = ' + IntToStr(UnitRemain));
            {$ENDIF}

            {Compress Run} {Compress entire Unit}
            if NTFSCompressUnit(DecompressBuffer,CompressBuffer,FClusterSize,AAttribute.CompressionUnit,0,UnitLength,UnitRemain) then
             begin
              {Get Size}
              UnitSize:=NTFSGetUnitUsed(CompressBuffer,FClusterSize,AAttribute.CompressionUnit);
              UnitClusters:=(UnitSize shr FClusterShiftCount);
              if (UnitClusters shl FClusterShiftCount) < UnitSize then Inc(UnitClusters);

              {$IFDEF NTFS_DEBUG}
              if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.CompressRun - UnitSize = ' + IntToStr(UnitSize) + ' UnitClusters = ' + IntToStr(UnitClusters));
              {$ENDIF}

              {Convert Run} {To Sparse}
              if not ConvertRun(ARecord,AAttribute,(UnitVCN + UnitClusters),(UnitLength - UnitClusters),True) then Exit;

              {Write Run} {Only write used clusters so that WriteRun does not convert sparse clusters}
              if WriteRun(ARecord,AAttribute,CompressBuffer^,UnitVCN,UnitClusters,Instance,True,False) <> UnitClusters then Exit;

              {Update Attribute}
              AAttribute.StreamUsed:=(ARecord.CalculatedStreamUsed(FVolumeVersion,AAttribute) shl FClusterShiftCount);
             end;
           end;

          {Update Position}
          Inc(VCN,Count);
          Dec(Remain,Count);
         end;

        Result:=True;
       finally
        {Release Buffer}
        ReleaseDecompressionBuffer(AAttribute.CompressionUnit,DecompressBuffer);
       end;
      finally
       {Release Buffer}
       ReleaseCompressionBuffer(AAttribute.CompressionUnit,CompressBuffer);
      end;
     end;
   end;
 finally
  FRecords.RunsWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.DecompressRun(ARecord:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute;const AVCN,ACount:Int64):Boolean;
{Decompress count clusters of the run of the supplied attribute and instances}
{VCN is the starting point for the decompression}
{Count is the number of clusters to decompress}
{Will actually decompress compression unit size blocks starting from the nearest unit}
{Note: The passed attribute must be the first instance}
{Note: Caller must hold the records and attributes lock}
var
 VCN:Int64;
 Count:Int64;
 Remain:Int64;

 UnitNo:Int64;                  {Compression Unit for current VCN}
 UnitVCN:Int64;                 {Starting VCN of Compression Unit}
 UnitCount:Int64;               {Count of Clusters available in Unit}
 UnitLength:Int64;              {Length of Compression Unit in Clusters}
 UnitRemain:Int64;              {Bytes remaining in Compression Unit}
 CompressBuffer:Pointer;        {Buffer for Compressed Data}
 DecompressBuffer:Pointer;      {Buffer for Raw write to Run}

 Instance:LongWord;
 UnitSize:LongWord;             {Size in bytes of Compressed Data}
 UnitClusters:Int64;            {Size in clusters of Compressed Data}
begin
 {}
 Result:=False;

 if not FRecords.RunsWriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ARecord = nil then Exit;
  if AAttribute = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.DecompressRun - RecordNumber = ' + IntToHex(ARecord.RecordNumber,16) + ' Type = ' + IntToHex(AAttribute.AttributeType,8) + ' Attribute = ' + AAttribute.AttributeName + ' VCN = ' + IntToStr(AVCN) + ' Count = ' + IntToStr(ACount));
  {$ENDIF}

  {Check ReadOnly}
  if FReadOnly then Exit;

  {Check Count}
  if ACount = 0 then Exit;

  {Check Resident}
  if AAttribute.NonResident = ntfsAttributeNonResident then
   begin
    {Check Compressed}
    if (AAttribute.IsCompressed) and (AAttribute.CompressionUnit <> 0) then
     begin
      {Get Buffer}
      CompressBuffer:=AllocCompressionBuffer(AAttribute.CompressionUnit,False);
      if CompressBuffer = nil then Exit;
      try
       {Get Buffer}
       DecompressBuffer:=AllocDecompressionBuffer(AAttribute.CompressionUnit,False);
       if DecompressBuffer = nil then Exit;
       try
        {Get Position}
        VCN:=AVCN;
        Remain:=ACount;
        Instance:=ntfsInstanceFirst;
        while Remain > 0 do
         begin
          {Get Unit}
          if not GetRunUnit(AAttribute,VCN,UnitNo,UnitCount) then Exit;

          {Get Count}
          Count:=Remain;
          if Count > UnitCount then Count:=UnitCount; {Min does not support Int64}

          {$IFDEF NTFS_DEBUG}
          if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.DecompressRun - VCN = ' + IntToStr(VCN) + ' UnitNo = ' + IntToStr(UnitNo) + ' UnitCount = ' + IntToStr(UnitCount));
          {$ENDIF}

          {Check Unit}
          if GetUnitCompressed(ARecord,AAttribute,UnitNo,UnitVCN,UnitLength) then
           begin
            {$IFDEF NTFS_DEBUG}
            if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.DecompressRun - UnitVCN = ' + IntToStr(UnitVCN) + ' UnitLength = ' + IntToStr(UnitLength) + ' Count = ' + IntToStr(Count));
            {$ENDIF}

            {Read Run}
            if ReadRun(ARecord,AAttribute,CompressBuffer^,UnitVCN,UnitLength,Instance,True,True) <> UnitLength then Exit;

            {Get Size}
            UnitSize:=NTFSGetUnitUsed(CompressBuffer,FClusterSize,AAttribute.CompressionUnit);
            UnitClusters:=(UnitSize shr FClusterShiftCount);
            if (UnitClusters shl FClusterShiftCount) < UnitSize then Inc(UnitClusters);

            {$IFDEF NTFS_DEBUG}
            if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.DecompressRun - UnitSize = ' + IntToStr(UnitSize) + ' UnitClusters = ' + IntToStr(UnitClusters));
            {$ENDIF}

            {Get Remain}
            UnitRemain:=Min64(UnitLength shl FClusterShiftCount,AAttribute.StreamSize - (UnitVCN shl FClusterShiftCount));

            {$IFDEF NTFS_DEBUG}
            if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.DecompressRun - UnitRemain = ' + IntToStr(UnitRemain));
            {$ENDIF}

            {Decompress Run} {Decompress entire Unit}
            if not NTFSDecompressUnit(CompressBuffer,DecompressBuffer,FClusterSize,AAttribute.CompressionUnit,0,UnitLength,UnitRemain) then Exit;

            {Convert Run} {To Normal}
            if not ConvertRun(ARecord,AAttribute,(UnitVCN + UnitClusters),(UnitLength - UnitClusters),False) then Exit;

            {Write Run}
            if WriteRun(ARecord,AAttribute,DecompressBuffer^,UnitVCN,UnitLength,Instance,True,False) <> UnitLength then Exit;

            {Update Attribute}
            AAttribute.StreamUsed:=(ARecord.CalculatedStreamUsed(FVolumeVersion,AAttribute) shl FClusterShiftCount);
           end;

          {Update Position}
          Inc(VCN,Count);
          Dec(Remain,Count);
         end;

        Result:=True;
       finally
        {Release Buffer}
        ReleaseDecompressionBuffer(AAttribute.CompressionUnit,DecompressBuffer);
       end;
      finally
       {Release Buffer}
       ReleaseCompressionBuffer(AAttribute.CompressionUnit,CompressBuffer);
      end;
     end;
   end;
 finally
  FRecords.RunsWriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.GetBlock(ABlockNo:LongWord):TDiskBlock;
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

function TNTFSFileSystem.GetBlockEx(ABlockNo:LongWord;AWrite:Boolean):TDiskBlock;
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

function TNTFSFileSystem.GetReparse(AEntry:TDiskEntry):TDiskReparse;
var
 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=nil;

 if not FEntries.ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if AEntry = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetReparse - Entry = ' + AEntry.Name);
  {$ENDIF}

  {Check Version}
  if FVolumeVersion < ntfsNTFS30 then Exit;

  {Check Relative}
  if (AEntry.Attributes and (faDot or faDotDot)) <> faNone then Exit;

  {Get Origin}
  if not FRecords.ReaderLock then Exit;
  try
   if TNTFSDiskEntry(AEntry).Origin = nil then Exit;
   Origin:=TNTFSDiskEntry(AEntry).Origin.Origin;
   if Origin = nil then Exit;

   {Get Attribute}
   Attribute:=Origin.GetAttribute(ntfsAttrTypeReparsePoint,ntfsBlankName,ntfsInstanceFirst);
   if Attribute = nil then Exit;

   {Get Reparse}
   Result:=TNTFSReparsePointAttribute(Attribute).Reparse;
  finally
   FRecords.ReaderUnlock;
  end;
 finally
  FEntries.ReaderUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.GetSecurity(AEntry:TDiskEntry):TDiskSecurity;
{Note: Caller must never free the returned object}
var
 Size:LongWord;
 Buffer:Pointer;
 Offset:LongWord;
 Instance:LongWord;
 Security:TNTFSSecurity;

 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=nil;

 if not FEntries.ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if AEntry = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetSecurity - Entry = ' + AEntry.Name);
  {$ENDIF}

  {Check Relative}
  if (AEntry.Attributes and (faDot or faDotDot)) <> faNone then Exit;

  {Get Origin}
  if not FRecords.ReaderLock then Exit;
  try
   if TNTFSDiskEntry(AEntry).Origin = nil then Exit;
   Origin:=TNTFSDiskEntry(AEntry).Origin.Origin;
   if Origin = nil then Exit;

   {Get Attribute}
   Attribute:=Origin.GetAttribute(ntfsAttrTypeStandardInformation,ntfsBlankName,ntfsInstanceFirst);
   if Attribute = nil then Exit;

   {Check Security}
   if TNTFSStandardInformationAttribute(Attribute).SecurityId <> 0 then
    begin
     {Get Security}
     Result:=GetSecurityById(TNTFSStandardInformationAttribute(Attribute).SecurityId);
    end
   else
    begin
     {Get Attribute}
     Attribute:=Origin.GetAttribute(ntfsAttrTypeSecurityDescriptor,ntfsBlankName,ntfsInstanceFirst);
     if Attribute = nil then Exit;

     {Get Security}
     Security:=TNTFSSecurityDescriptorAttribute(Attribute).Security;
     if Security = nil then
      begin
       {Convert Lock}
       if not FRecords.ReaderConvert then Exit;
       try
        {Get Security (After Lock)}
        Security:=TNTFSSecurityDescriptorAttribute(Attribute).Security;
        if Security = nil then
         begin
          {Check Resident}
          if Attribute.NonResident = ntfsAttributeNonResident then
           begin
            {Check Size}
            if Attribute.StreamSize > 0 then
             begin
              Buffer:=GetMem(Attribute.StreamSize);
              if Buffer = nil then Exit;
              try
               {Read Attribute}
               Instance:=ntfsInstanceFirst;
               if ReadAttribute(Origin,Attribute,Buffer^,0,Attribute.StreamSize,Instance,False) <> Attribute.StreamSize then Exit;

               {Get Offset}
               Offset:=0;
               Size:=Attribute.StreamSize;

               {Read Security}
               if not TNTFSSecurityDescriptorAttribute(Attribute).ReadSecurity(Buffer,Offset,Size,FVolumeVersion) then Exit;

               {Get Security}
               Security:=TNTFSSecurityDescriptorAttribute(Attribute).Security;
              finally
               FreeMem(Buffer);
              end;
             end;
           end;
         end;
       finally
        {Convert Lock}
        FRecords.WriterConvert;
       end;
      end;

     Result:=Security;
    end;
  finally
   FRecords.ReaderUnlock;
  end;
 finally
  FEntries.ReaderUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.GetSecurityEx(AEntry:TDiskEntry;AInherited:Boolean;var ASecurityId:LongWord;AWrite:Boolean):TDiskSecurity;
{Note: Caller may free the returned object if Inherited is True and SecurityId is ntfsSecurityIdUnknown or Version is ntfsNTFS12}
{Note: Caller must never free the returned object if it is not Inherited}
var
 Size:LongWord;
 Buffer:Pointer;
 Offset:LongWord;
 Instance:LongWord;
 Descriptor:Pointer;
 Security:TNTFSSecurity;

 Origin:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=nil;

 ASecurityId:=ntfsSecurityIdUnknown;

 if AWrite then
  begin
   if not FEntries.WriterLock then Exit;
  end
 else
  begin
   if not FEntries.ReaderLock then Exit;
  end;
 try
  if FDriver = nil then Exit;
  if AEntry = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetSecurityEx - Entry = ' + AEntry.Name + ' Inherited = ' + BoolToStr(AInherited));
  {$ENDIF}

  {Check Relative}
  if (AEntry.Attributes and (faDot or faDotDot)) <> faNone then Exit;

  {Get Origin}
  if AWrite then
   begin
    if not FRecords.WriterLock then Exit;
   end
  else
   begin
    if not FRecords.ReaderLock then Exit;
   end;
  try
   if TNTFSDiskEntry(AEntry).Origin = nil then Exit;
   Origin:=TNTFSDiskEntry(AEntry).Origin.Origin;
   if Origin = nil then Exit;

   {Get Attribute}
   Attribute:=Origin.GetAttribute(ntfsAttrTypeStandardInformation,ntfsBlankName,ntfsInstanceFirst);
   if Attribute = nil then Exit;

   {Check Security}
   if TNTFSStandardInformationAttribute(Attribute).SecurityId <> 0 then
    begin
     {Must be Version 3.0 or 3.1}
     {Check Inherited}
     if not AInherited then
      begin
       {Get Security Id}
       ASecurityId:=TNTFSStandardInformationAttribute(Attribute).SecurityId;

       {Get Security}
       Result:=GetSecurityById(TNTFSStandardInformationAttribute(Attribute).SecurityId);
      end
     else
      begin
       {Get Security}
       Security:=GetSecurityById(TNTFSStandardInformationAttribute(Attribute).SecurityId);
       if Security = nil then Exit;

       {Get Descriptor}
       Descriptor:=Security.InheritedDescriptor;
       if Descriptor = nil then Exit;
       try
        {Get Security Id}
        ASecurityId:=GetDescriptorId(Descriptor,AWrite);
        if ASecurityId <> ntfsSecurityIdUnknown then
         begin
          {Get Security}
          Result:=GetSecurityById(ASecurityId);
         end
        else
         begin
          {Create Security}
          Result:=TNTFSSecurity.CreateFromDescriptor(FSecurityLocal,Descriptor);
         end;
       finally
        Security.ReleaseDescriptor(Descriptor,True,False);
       end;
      end;
    end
   else
    begin
     {May be Version 1.2 or a Metafile in 3.0/3.1}
     {Get Attribute}
     Attribute:=Origin.GetAttribute(ntfsAttrTypeSecurityDescriptor,ntfsBlankName,ntfsInstanceFirst);
     if Attribute = nil then Exit;

     {Get Security}
     Security:=TNTFSSecurityDescriptorAttribute(Attribute).Security;
     if Security = nil then
      begin
       {Convert Lock}
       if not(AWrite) then if not FRecords.ReaderConvert then Exit;
       try
        {Get Security (After Lock)}
        Security:=TNTFSSecurityDescriptorAttribute(Attribute).Security;
        if Security = nil then
         begin
          {Check Resident}
          if Attribute.NonResident = ntfsAttributeNonResident then
           begin
            {Check Size}
            if Attribute.StreamSize > 0 then
             begin
              Buffer:=GetMem(Attribute.StreamSize);
              if Buffer = nil then Exit;
              try
               {Read Attribute}
               Instance:=ntfsInstanceFirst;
               if ReadAttribute(Origin,Attribute,Buffer^,0,Attribute.StreamSize,Instance,AWrite) <> Attribute.StreamSize then Exit;

               {Get Offset}
               Offset:=0;
               Size:=Attribute.StreamSize;

               {Read Security}
               if not TNTFSSecurityDescriptorAttribute(Attribute).ReadSecurity(Buffer,Offset,Size,FVolumeVersion) then Exit;

               {Get Security}
               Security:=TNTFSSecurityDescriptorAttribute(Attribute).Security;
              finally
               FreeMem(Buffer);
              end;
             end;
           end;
         end;
       finally
        {Convert Lock}
        if not(AWrite) then FRecords.WriterConvert;
       end;
      end;

     {Check Inherited}
     if not AInherited then
      begin
       case FVolumeVersion of
        ntfsNTFS12:begin
          {Get Security}
          Result:=Security;
         end;
        ntfsNTFS30,ntfsNTFS31:begin
          {Get Security Id}
          ASecurityId:=GetSecurityId(Security,AWrite);

          {Get Security}
          Result:=Security;
         end;
       end;
      end
     else
      begin
       if Security = nil then Exit;

       {Get Descriptor}
       Descriptor:=Security.InheritedDescriptor;
       if Descriptor = nil then Exit;
       try
        case FVolumeVersion of
         ntfsNTFS12:begin
           {Create Security}
           Result:=TNTFSSecurity.CreateFromDescriptor(FSecurityLocal,Descriptor);
          end;
         ntfsNTFS30,ntfsNTFS31:begin
           {Get Security Id}
           ASecurityId:=GetDescriptorId(Descriptor,AWrite);
           if ASecurityId <> ntfsSecurityIdUnknown then
            begin
             {Get Security}
             Result:=GetSecurityById(ASecurityId);
            end
           else
            begin
             {Create Security}
             Result:=TNTFSSecurity.CreateFromDescriptor(FSecurityLocal,Descriptor);
            end;
          end;
        end;
       finally
        Security.ReleaseDescriptor(Descriptor,True,False);
       end;
      end;
    end;
  finally
   if AWrite then
    begin
     FRecords.WriterUnlock;
    end
   else
    begin
     FRecords.ReaderUnlock;
    end;
  end;
 finally
  if AWrite then
   begin
    FEntries.WriterUnlock;
   end
  else
   begin
    FEntries.ReaderUnlock;
   end;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.GetMetafile(AFileNo:LongWord):TDiskEntry;
begin
 {}
 Result:=nil;

 if not FEntries.ReaderLock then Exit;
 try
  if FDriver = nil then Exit;

  {Get Metafile}
  case AFileNo of
   ntfsFileTypeMft:Result:=FMft;
   ntfsFileTypeMftMirr:Result:=FMftMirr;
   ntfsFileTypeLogFile:Result:=FLogFile;
   ntfsFileTypeVolume:Result:=FVolInfo;
   ntfsFileTypeAttrDef:Result:=FAttrDef;
   ntfsFileTypeRoot:Result:=FRoot;
   ntfsFileTypeBitmap:Result:=FBitmap;
   ntfsFileTypeBoot:Result:=FBoot;
   ntfsFileTypeBadClus:Result:=FBadClus;
   ntfsFileTypeSecure:if FNTFSType = ntNTFS12 then Result:=FQuota else Result:=FSecure; {Also ntfs12FileTypeQuota}
   ntfsFileTypeUpCase:Result:=FUpCase;
   ntfsFileTypeExtend:if FNTFSType = ntNTFS12 then Result:=nil else Result:=FExtend;
   {Metafiles within $Extend}
   ntfsFileTypeObjId:if FNTFSType = ntNTFS12 then Result:=nil else Result:=FObjId;
   ntfsFileTypeQuota:if FNTFSType = ntNTFS12 then Result:=nil else Result:=FQuota;
   ntfsFileTypeReparse:if FNTFSType = ntNTFS12 then Result:=nil else Result:=FReparse;
   ntfsFileTypeUsnJrnl:if FNTFSType = ntNTFS12 then Result:=nil else Result:=FUsnJrnl;
  end;
  if Result <> nil then Exit;
 finally
  FEntries.ReaderUnlock;
 end;

 {Load Metafile}
 if LoadMetafile(AFileNo) then
  begin
   {Metafile Loaded call GetMetafile}
   Result:=GetMetafile(AFileNo);
  end;
end;

{=============================================================================}

function TNTFSFileSystem.GetMetafileEx(AFileNo:LongWord;AWrite:Boolean):TDiskEntry;
begin
 {}
 Result:=nil;

 if AWrite then
  begin
   if not FEntries.WriterLock then Exit;
  end
 else
  begin
   if not FEntries.ReaderLock then Exit;
  end;
 try
  if FDriver = nil then Exit;

  {Get Metafile}
  case AFileNo of
   ntfsFileTypeMft:Result:=FMft;
   ntfsFileTypeMftMirr:Result:=FMftMirr;
   ntfsFileTypeLogFile:Result:=FLogFile;
   ntfsFileTypeVolume:Result:=FVolInfo;
   ntfsFileTypeAttrDef:Result:=FAttrDef;
   ntfsFileTypeRoot:Result:=FRoot;
   ntfsFileTypeBitmap:Result:=FBitmap;
   ntfsFileTypeBoot:Result:=FBoot;
   ntfsFileTypeBadClus:Result:=FBadClus;
   ntfsFileTypeSecure:if FNTFSType = ntNTFS12 then Result:=FQuota else Result:=FSecure; {Also ntfs12FileTypeQuota}
   ntfsFileTypeUpCase:Result:=FUpCase;
   ntfsFileTypeExtend:if FNTFSType = ntNTFS12 then Result:=nil else Result:=FExtend;
   {Metafiles within $Extend}
   ntfsFileTypeObjId:if FNTFSType = ntNTFS12 then Result:=nil else Result:=FObjId;
   ntfsFileTypeQuota:if FNTFSType = ntNTFS12 then Result:=nil else Result:=FQuota;
   ntfsFileTypeReparse:if FNTFSType = ntNTFS12 then Result:=nil else Result:=FReparse;
   ntfsFileTypeUsnJrnl:if FNTFSType = ntNTFS12 then Result:=nil else Result:=FUsnJrnl;
  end;
  if Result <> nil then Exit;
 finally
  if AWrite then
   begin
    FEntries.WriterUnlock;
   end
  else
   begin
    FEntries.ReaderUnlock;
   end;
 end;

 {Load Metafile}
 if LoadMetafile(AFileNo) then
  begin
   {Metafile Loaded call GetMetafileEx}
   Result:=GetMetafileEx(AFileNo,AWrite);
  end;
end;

{=============================================================================}

function TNTFSFileSystem.GetRecord(ABase:TNTFSDiskRecord;const ARecordNumber:Int64;AFree:Boolean):TNTFSDiskRecord;
begin
 {}
 Result:=nil;

 if not FRecords.ReaderLock then Exit;
 try
  if FDriver = nil then Exit;

  {Get Record}
  Result:=FRecords.FindRecord(ARecordNumber);
  if Result <> nil then Exit;
 finally
  FRecords.ReaderUnlock;
 end;

 {Load Record}
 if LoadRecord(ABase,ARecordNumber,AFree) then
  begin
   {Record Loaded call GetRecord}
   Result:=GetRecord(ABase,ARecordNumber,AFree);
  end;
end;

{=============================================================================}

function TNTFSFileSystem.GetRecordEx(ABase:TNTFSDiskRecord;const ARecordNumber:Int64;AFree,AWrite:Boolean):TNTFSDiskRecord;
begin
 {}
 Result:=nil;

 if AWrite then
  begin
   if not FRecords.WriterLock then Exit;
  end
 else
  begin
   if not FRecords.ReaderLock then Exit;
  end;
 try
  if FDriver = nil then Exit;

  {Get Record}
  Result:=FRecords.FindRecord(ARecordNumber);
  if Result <> nil then Exit;
 finally
  if AWrite then
   begin
    FRecords.WriterUnlock;
   end
  else
   begin
    FRecords.ReaderUnlock;
   end;
 end;

 {Load Record}
 if LoadRecord(ABase,ARecordNumber,AFree) then
  begin
   {Record Loaded call GetRecordEx}
   Result:=GetRecordEx(ABase,ARecordNumber,AFree,AWrite);
  end;
end;

{=============================================================================}

function TNTFSFileSystem.GetReference(ABase:TNTFSDiskRecord;const AFileReference:Int64):TNTFSDiskRecord;
var
 RecordNumber:Int64;
 ARecord:TNTFSDiskRecord;  //To Do //Current/Origin ?
begin
 {}
 Result:=nil;

 if not FRecords.ReaderLock then Exit;
 try
  if FDriver = nil then Exit;

  {Get Number}
  if AFileReference = ntfsFileReferenceNone then Exit;
  RecordNumber:=(AFileReference and ntfsRecordNumberMask);

  {Get Record}
  ARecord:=FRecords.FindRecord(RecordNumber);
  if ARecord <> nil then
   begin
    if ARecord.FileReference <> AFileReference then Exit;
    Result:=ARecord;
    Exit;
   end;
 finally
  FRecords.ReaderUnlock;
 end;

 {Load Record}
 if LoadRecord(ABase,RecordNumber,False) then
  begin
   {Record Loaded call GetRecord}
   ARecord:=GetRecord(ABase,RecordNumber,False);
   if ARecord <> nil then
    begin
     if ARecord.FileReference <> AFileReference then Exit;

     Result:=ARecord;
     Exit;
    end;
  end;
end;

{=============================================================================}

function TNTFSFileSystem.GetReferenceEx(ABase:TNTFSDiskRecord;const AFileReference:Int64;AWrite:Boolean):TNTFSDiskRecord; {Not override}
var
 RecordNumber:Int64;
 ARecord:TNTFSDiskRecord;  //To Do //Current/Origin ?
begin
 {}
 Result:=nil;

 if AWrite then
  begin
   if not FRecords.WriterLock then Exit;
  end
 else
  begin
   if not FRecords.ReaderLock then Exit;
  end;
 try
  if FDriver = nil then Exit;

  {Get Number}
  if AFileReference = ntfsFileReferenceNone then Exit;
  RecordNumber:=(AFileReference and ntfsRecordNumberMask);

  {Get Record}
  ARecord:=FRecords.FindRecord(RecordNumber);
  if ARecord <> nil then
   begin
    if ARecord.FileReference <> AFileReference then Exit;
    Result:=ARecord;
    Exit;
   end;
 finally
  if AWrite then
   begin
    FRecords.WriterUnlock;
   end
  else
   begin
    FRecords.ReaderUnlock;
   end;
 end;

 {Load Record}
 if LoadRecord(ABase,RecordNumber,False) then
  begin
   {Record Loaded call GetRecordEx}
   ARecord:=GetRecordEx(ABase,RecordNumber,False,AWrite);
   if ARecord <> nil then
    begin
     if ARecord.FileReference <> AFileReference then Exit;

     Result:=ARecord;
     Exit;
    end;
  end;
end;

{=============================================================================}

function TNTFSFileSystem.IsShort(const AName:String):Boolean;
{Modified from SoftUtils.IsEightDotThree as NTFS does not check case}
var
 Count:Integer;
 DotPos:Integer;
 DotCount:Integer;
 NameLength:Integer;
begin
 {}
 Result:=False;

 if FDriver = nil then Exit;

 {Check for zero length or series of spaces}
 if Length(Trim(AName)) = 0 then Exit;

 {Check for length greater than allowed}
 if Length(AName) > FMaxAltFile then Exit;

 {Check for leading dot}
 if AName[1] = '.' then Exit;

 {Check for leading space}
 if AName[1] = ' ' then Exit;

 {Check for case match}
 {if Uppercase(AName) <> AName then Exit;} {Do not check case for NTFS}

 {Check for Invalid Chars, Substituted Chars, Spaces, Multiple Dots}
 DotPos:=0;
 DotCount:=0;
 NameLength:=Length(AName);
 for Count:=1 to NameLength do
  begin
   {Check for Invalid Chars}
   if AName[Count] in INVALID_FILENAME_CHARS then Exit;

   {Check for Substituted Chars}
   if AName[Count] in SHORT_FILENAME_SUBST_CHARS then Exit;

   {Check for Spaces}
   if AName[Count] = ' ' then Exit;

   {Check for Multiple Dots}
   if AName[Count] = '.' then Inc(DotCount);
   if DotCount > 1 then Exit;

   {Check for Dot Position}
   if AName[Count] = '.' then DotPos:=Count;
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

{=============================================================================}

function TNTFSFileSystem.CheckName(const AName:String):Boolean;
//To Do //CheckName should have Attributes to allow varying behaviour based on type (eg Label)
begin
 {}
 Result:=False;

 if FDriver = nil then Exit;

 {Check for Length}
 if Length(AName) = 0 then Exit;
 if Length(AName) > FMaxFile then Exit;

 Result:=True;
end;

{=============================================================================}

function TNTFSFileSystem.CompareName(const AName,AMatch:String;AWildcard:Boolean):Boolean;
begin
 {}
 Result:=False;
 if FDriver = nil then Exit;

 //To Do //Not used yet - See TFileSystem.MatchEntry/CompareName
 //Result:=True; //The base method will probably do for this
end;

{=============================================================================}

function TNTFSFileSystem.CheckAltName(const AAltName:String):Boolean;
//To Do //CheckName should have Attributes to allow varying behaviour based on type (eg Label)
begin
 {}
 Result:=False;

 if FDriver = nil then Exit;

 {Check for Length}
 if Length(AAltName) = 0 then Exit;
 if Length(AAltName) > FMaxAltFile then Exit;

 Result:=True;
end;

{=============================================================================}

function TNTFSFileSystem.CompareAltName(const AAltName,AMatch:String;AWildcard:Boolean):Boolean;
begin
 {}
 Result:=False;

 if FDriver = nil then Exit;

 //To Do //Not used yet - See TFileSystem.MatchEntry/CompareName
 //Result:=True; //The base method will probably do for this
end;

{=============================================================================}

function TNTFSFileSystem.GenerateName(AParent,AEntry:TDiskEntry;const AName:String):String;
var
 Hash:LongWord;
 Shift:LongWord;
 Step:Integer;
 Count:Integer;
 AltName:String;
 Current:TDiskEntry;
begin
 {}
 Result:=ntfsBlankName;

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
   if Count < ntfsMaxNameAlias then
    begin
     Inc(Count);
     AltName:=GenerateShortNameEx(AName,Count,Hash,False);

     Current:=GetEntryEx(AParent,AltName,faDirectory or faFile,False,False,True);
    end
   else
    begin
     if (Step >= ntfsMaxNameAlias) and (Shift < ntfsMaxHashShift) then
      begin
       Step:=0;
       Inc(Shift);
       Hash:=(Hash shr 1);
      end;
     if Step = ntfsMaxHashAlias then Exit;
     Inc(Step);
     AltName:=GenerateShortNameEx(AName,Step,Hash,True);

     Current:=GetEntryEx(AParent,AltName,faDirectory or faFile,False,False,True);
    end;
  end;

 Result:=AltName;
end;

{=============================================================================}

function TNTFSFileSystem.GetSecurityFromDescriptor(ADescriptor:Pointer):TDiskSecurity;
begin
 {}
 Result:=nil;

 if FDriver = nil then Exit;
 if ADescriptor = nil then Exit;

 Result:=TNTFSSecurity.CreateFromDescriptor(FSecurityLocal,ADescriptor);
end;

{=============================================================================}

function TNTFSFileSystem.FileSystemInit:Boolean;
begin
 {}
 Result:=False;

 if not WriterLock then Exit;
 try
  if FDriver = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.FileSystemInit');
  {$ENDIF}

  {Clear Current}
  FNTFSType:=ntNONE;
  FVolumeVersion:=ntfsNTFS12;
  FVolumeFlags:=ntfsVolumeFlagNone;

  FRoot:=nil;

  SetCurrent(nil);

  FChunks.ClearList;
  FTables.ClearList;
  FBlocks.ClearList;
  FEntries.ClearList;

  FSectorsPerCluster:=0;

  FMftStartCluster:=ntfsUnknownCluster;
  FMftMirrorCluster:=ntfsUnknownCluster;

  FMftZoneStart:=ntfsUnknownCluster;
  FMftZoneCluster:=ntfsUnknownCluster;
  FMftMirrorCount:=0;
  FMftZoneReservation:=0;

  FFileRecordSize:=0;
  FIndexRecordSize:=0;

  FClustersPerFile:=0;
  FClustersPerIndex:=0;

  FFilesPerCluster:=0;
  FIndexsPerCluster:=0;

  FEntriesPerBlock:=0;
  FClustersPerBlock:=0;
  FTotalBlockCount:=0;

  FBlockShiftCount:=0;
  FSectorShiftCount:=0;
  FClusterShiftCount:=0;

  FFileRecordShiftCount:=0;
  FFileRecordOffsetMask:=0;

  FIndexCounterShift:=0;
  FIndexCounterOffset:=0;

  FIndexRecordShiftCount:=0;
  FIndexRecordOffsetMask:=0;

  FTotalClusterCount:=0;

  FLastMftCluster:=ntfsUnknownCluster;
  FLastFreeCluster:=ntfsUnknownCluster;
  FFreeClusterCount:=ntfsUnknownCluster;

  FTotalFileRecordCount:=0;

  FLastFreeFileRecord:=ntfsUnknownRecordNumber;
  FFreeFileRecordCount:=ntfsUnknownRecordNumber;
  FReservedFileRecordCount:=ntfsUnknownRecordNumber;

  FClusterSize:=0;

  FMft:=nil;
  FMftMirr:=nil;
  FLogFile:=nil;
  FVolInfo:=nil;
  FAttrDef:=nil;
  FBitmap:=nil;
  FBoot:=nil;
  FBadClus:=nil;
  FSecure:=nil;
  FUpCase:=nil;
  FExtend:=nil;
  FObjId:=nil;
  FQuota:=nil;
  FReparse:=nil;
  FUsnJrnl:=nil;

  FMaster:=nil;
  FMirror:=nil;

  {FUpCases} {Nothing}
  FAttrDefs.ClearList;
  FSecuritys.ClearList;
  FRecords.EmptyBtree;

  if FFileBuffer <> nil then FreeMem(FFileBuffer);
  FFileBuffer:=nil;

  if FIndexBuffer <> nil then FreeMem(FIndexBuffer);
  FIndexBuffer:=nil;

  if FReadBuffer <> nil then FreeMem(FReadBuffer);
  FReadBuffer:=nil;

  if FWriteBuffer <> nil then FreeMem(FWriteBuffer);
  FWriteBuffer:=nil;

  if FClusterBuffer <> nil then FreeMem(FClusterBuffer);
  FClusterBuffer:=nil;

  if FCompressionBuffer <> nil then FreeMem(FCompressionBuffer);
  FCompressionBuffer:=nil;

  if FDecompressionBuffer <> nil then FreeMem(FDecompressionBuffer);
  FDecompressionBuffer:=nil;

  {$IFDEF NTFS_DEBUG}
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

  {$IFDEF NTFS_DEBUG}
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

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                Volume Checked');
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.FileSystemInit Completed');
  {$ENDIF}

  Result:=True;
 finally
  WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.MountFileSystem:Boolean;
begin
 {}
 Result:=False;

 if not WriterLock then Exit;
 try
  if FDriver = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.MountFileSystem');
  {$ENDIF}

  {Check Count}
  if FSectorSize = 0 then Exit;
  if FSectorCount = 0 then Exit;

  {Get Boot Sector}
  if not ReadSectors(ntfsBootSector,1,FSectorBuffer^) then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                Boot Sector Read');
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                 PNtfsBootSector(FSectorBuffer).Signature=' + IntToHex(PNtfsBootSector(FSectorBuffer).Signature,4));
  {$ENDIF}

  {Check Boot Sector}
  if PNtfsBootSector(FSectorBuffer).Signature <> BOOT_SECTOR_SIGNATURE then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                Boot Sector Checked');
  {$ENDIF}

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                Check for NTFS');
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                 PNtfsBootSector(FSectorBuffer).BPB.BytesPerSector=' + IntToStr(PNtfsBootSector(FSectorBuffer).BPB.BytesPerSector));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                 PNtfsBootSector(FSectorBuffer).BPB.SectorsPerCluster=' + IntToStr(PNtfsBootSector(FSectorBuffer).BPB.SectorsPerCluster));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                 PNtfsBootSector(FSectorBuffer).BPB.TotalSectors=' + IntToStr(PNtfsBootSector(FSectorBuffer).BPB.TotalSectors));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                 PNtfsBootSector(FSectorBuffer).BPB.MFTCluster=' + IntToStr(PNtfsBootSector(FSectorBuffer).BPB.MFTCluster));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                 PNtfsBootSector(FSectorBuffer).BPB.MFTMirror=' + IntToStr(PNtfsBootSector(FSectorBuffer).BPB.MFTMirror));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                 PNtfsBootSector(FSectorBuffer).BPB.ClustersPerFile=' + IntToStr(PNtfsBootSector(FSectorBuffer).BPB.ClustersPerFile));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                 PNtfsBootSector(FSectorBuffer).BPB.ClustersPerIndex=' + IntToStr(PNtfsBootSector(FSectorBuffer).BPB.ClustersPerIndex));
  {$ENDIF}

  {Check for NTFS}
  if PNtfsBootSector(FSectorBuffer).BPB.BytesPerSector = 0 then Exit;
  if (PNtfsBootSector(FSectorBuffer).BPB.BytesPerSector mod MIN_SECTOR_SIZE) <> 0 then Exit;
  if (PNtfsBootSector(FSectorBuffer).BPB.SectorsPerCluster = 0) then Exit;
  if (PNtfsBootSector(FSectorBuffer).BPB.SectorsPerCluster <> 1) and ((PNtfsBootSector(FSectorBuffer).BPB.SectorsPerCluster mod 2) <> 0) then Exit;
  if PNtfsBootSector(FSectorBuffer).BPB.TotalSectors = 0 then Exit;
  {if PNtfsBootSector(FSectorBuffer).BPB.TotalSectors > FSectorCount then Exit;}
  if (PNtfsBootSector(FSectorBuffer).BPB.TotalSectors > FSectorCount) and ((PNtfsBootSector(FSectorBuffer).BPB.TotalSectors and ntfsBlockCountMask8) > FSectorCount) and ((PNtfsBootSector(FSectorBuffer).BPB.TotalSectors and ntfsBlockCountMask64) > FSectorCount) then Exit; {Allow for NTFS rounding to block multiple}
  if PNtfsBootSector(FSectorBuffer).BPB.MFTCluster < 1 then Exit;
  if PNtfsBootSector(FSectorBuffer).BPB.MFTMirror < 1 then Exit;
  if PNtfsBootSector(FSectorBuffer).BPB.ClustersPerFile = 0 then Exit;
  if PNtfsBootSector(FSectorBuffer).BPB.ClustersPerIndex = 0 then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                NTFS Detected');
  {$ENDIF}

  {Calculate Parameters}
  FSectorSize:=PNtfsBootSector(FSectorBuffer).BPB.BytesPerSector;
  FSectorCount:=PNtfsBootSector(FSectorBuffer).BPB.TotalSectors;

  FBootSector:=ntfsBootSector;
  FBootBackup:=FSectorCount; {Max Sector - 1}

  FSectorsPerCluster:=PNtfsBootSector(FSectorBuffer).BPB.SectorsPerCluster;

  FMftStartCluster:=PNtfsBootSector(FSectorBuffer).BPB.MFTCluster;
  FMftMirrorCluster:=PNtfsBootSector(FSectorBuffer).BPB.MFTMirror;

  FFileRecordSize:=GetFileRecordSize(PNtfsBootSector(FSectorBuffer).BPB.ClustersPerFile);
  FIndexRecordSize:=GetIndexRecordSize(PNtfsBootSector(FSectorBuffer).BPB.ClustersPerIndex);

  FClustersPerFile:=GetClustersPerFile(PNtfsBootSector(FSectorBuffer).BPB.ClustersPerFile);
  FClustersPerIndex:=GetClustersPerIndex(PNtfsBootSector(FSectorBuffer).BPB.ClustersPerIndex);

  FFilesPerCluster:=GetFilesPerCluster(PNtfsBootSector(FSectorBuffer).BPB.ClustersPerFile);
  FIndexsPerCluster:=GetIndexsPerCluster(PNtfsBootSector(FSectorBuffer).BPB.ClustersPerIndex);

  FMftZoneReservation:=GetMftZoneReservation;
  FMftZoneStart:=ntfsUnknownCluster;
  FMftZoneCluster:=GetMftZoneCluster;
  FMftMirrorCount:=GetMftMirrorCount;

  FBlockShiftCount:=GetBlockShiftCount(FSectorSize * FSectorsPerCluster);
  FSectorShiftCount:=GetSectorShiftCount(FSectorsPerCluster);
  FClusterShiftCount:=GetClusterShiftCount(FSectorSize * FSectorsPerCluster);

  FFileRecordShiftCount:=GetFileRecordShiftCount(FSectorSize * FSectorsPerCluster,FFileRecordSize);
  FFileRecordOffsetMask:=GetFileRecordOffsetMask(FFilesPerCluster);

  FIndexCounterOffset:=GetIndexCounterOffset(FClustersPerIndex);
  FIndexCounterShift:=GetIndexCounterShift(FIndexCounterOffset);

  FIndexRecordShiftCount:=GetIndexRecordShiftCount(FSectorSize * FSectorsPerCluster,FIndexRecordSize);
  FIndexRecordOffsetMask:=GetIndexRecordOffsetMask(FIndexsPerCluster);

  FEntriesPerBlock:=GetEntriesPerBlock(FSectorSize * FSectorsPerCluster);
  FClustersPerBlock:=GetClustersPerBlock(FSectorSize * FSectorsPerCluster);
  FTotalBlockCount:=GetTotalBlockCount(FSectorCount div FSectorsPerCluster);

  FTotalClusterCount:=(FSectorCount div FSectorsPerCluster);

  FClusterSize:=(FSectorSize * FSectorsPerCluster);

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                Parameters Calculated');
  {$ENDIF}

  {Create Buffers}
  FFileBuffer:=GetMem(Max(FFileRecordSize,FClusterSize));
  if FFileBuffer = nil then Exit;
  FIndexBuffer:=GetMem(Max(FIndexRecordSize,FClusterSize));
  if FIndexBuffer = nil then Exit;
  FReadBuffer:=GetMem(FClusterSize);
  if FReadBuffer = nil then Exit;
  FWriteBuffer:=GetMem(FClusterSize);
  if FWriteBuffer = nil then Exit;
  FClusterBuffer:=GetMem(FClusterSize);
  if FClusterBuffer = nil then Exit;
  FCompressionBuffer:=GetMem((FClusterSize shl 4) + FClusterSize); {Multiply by 16} {One extra cluster for overrun}
  if FCompressionBuffer = nil then Exit;
  FDecompressionBuffer:=GetMem((FClusterSize shl 4) + FClusterSize); {Multiply by 16} {One extra cluster for overrun}
  if FDecompressionBuffer = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                Buffers Created');
  {$ENDIF}

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then
   begin
    FileSysLogDebug('TNTFSFileSystem.MountFileSystem');
    FileSysLogDebug(' SectorSize = ' + IntToStr(FSectorSize));
    FileSysLogDebug(' StartSector = ' + IntToStr(FStartSector));
    FileSysLogDebug(' SectorCount = ' + IntToStr(FSectorCount));
    FileSysLogDebug('');
    FileSysLogDebug(' BootSector = ' + IntToStr(FBootSector));
    FileSysLogDebug(' BootBackup = ' + IntToStr(FBootBackup));
    FileSysLogDebug('');
    FileSysLogDebug(' SectorsPerCluster = ' + IntToStr(FSectorsPerCluster));
    FileSysLogDebug('');
    FileSysLogDebug(' MftStartCluster = ' + IntToStr(FMftStartCluster));
    FileSysLogDebug(' MftMirrorCluster = ' + IntToStr(FMftMirrorCluster));
    FileSysLogDebug('');
    FileSysLogDebug(' FileRecordSize = ' + IntToStr(FFileRecordSize));
    FileSysLogDebug(' IndexRecordSize = ' + IntToStr(FIndexRecordSize));
    FileSysLogDebug('');
    FileSysLogDebug(' ClustersPerFile = ' + IntToStr(FClustersPerFile));
    FileSysLogDebug(' ClustersPerIndex = ' + IntToStr(FClustersPerIndex));
    FileSysLogDebug('');
    FileSysLogDebug(' FilesPerCluster = ' + IntToStr(FFilesPerCluster));
    FileSysLogDebug(' IndexsPerCluster = ' + IntToStr(FIndexsPerCluster));
    FileSysLogDebug('');
    FileSysLogDebug(' MftZoneReservation = ' + IntToStr(FMftZoneReservation));
    FileSysLogDebug(' MftZoneStart = ' + IntToStr(FMftZoneStart));
    FileSysLogDebug(' MftZoneCluster = ' + IntToStr(FMftZoneCluster));
    FileSysLogDebug(' MftMirrorCount = ' + IntToStr(FMftMirrorCount));
    FileSysLogDebug('');
    FileSysLogDebug(' BlockShiftCount = ' + IntToStr(FBlockShiftCount));
    FileSysLogDebug(' SectorShiftCount = ' + IntToStr(FSectorShiftCount));
    FileSysLogDebug(' ClusterShiftCount = ' + IntToStr(FClusterShiftCount));
    FileSysLogDebug('');
    FileSysLogDebug(' FileRecordShiftCount = ' + IntToStr(FFileRecordShiftCount));
    FileSysLogDebug(' FileRecordOffsetMask = ' + IntToStr(FFileRecordOffsetMask));
    FileSysLogDebug('');
    FileSysLogDebug(' IndexCounterShift = ' + IntToStr(FIndexCounterShift));
    FileSysLogDebug(' IndexCounterOffset = ' + IntToStr(FIndexCounterOffset));
    FileSysLogDebug('');
    FileSysLogDebug(' IndexRecordShiftCount = ' + IntToStr(FIndexRecordShiftCount));
    FileSysLogDebug(' IndexRecordOffsetMask = ' + IntToStr(FIndexRecordOffsetMask));
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

  {Determine Type}
  FNTFSType:=GetVolumeType;
  FVolumeVersion:=GetVolumeVersion;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                Type Determined');
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                 NTFSType = ' + NTFSTypeToString(FNTFSType));
  {$ENDIF}

  {Load Metafiles}
  if not LoadMetafiles then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                Metafiles Loaded');
  {$ENDIF}

  {Load Tables}
  LoadTables;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                Tables Loaded');
  {$ENDIF}

  {Load Block}
  LoadBlock(0);

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                Block(0) Loaded');
  {$ENDIF}

  {Load UpCases}
  LoadUpCases;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                UpCases Loaded');
  {$ENDIF}

  {Load AttrDefs}
  LoadAttrDefs;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                AttrDefs Loaded');
  {$ENDIF}

  {Load Securitys}
  LoadSecuritys;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                Securitys Loaded');
  {$ENDIF}

  {Set Current}
  SetCurrent(FRoot);

  {Setup Values}
  FSystemName:=LoadSystemName;
  FVolumeName:=LoadVolumeName;
  FVolumeGUID:=LoadVolumeGUID;
  FVolumeSerial:=LoadVolumeSerial;
  FFileSysType:=LoadFileSysType;
  FVolumeFlags:=LoadVolumeFlags;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.MountFileSystem Completed');
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                SystemName = ' + FSystemName);
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                VolumeName = ' + FVolumeName);
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                VolumeSerial = ' + IntToHex(FVolumeSerial,8));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                FileSysType = ' + FileSysTypeToString(FFileSysType));
  {$ENDIF}

  {Set Dirty}
  MarkMount;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                Mount Marked (Dirty)');
  {$ENDIF}

  Result:=True;
 finally
  WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.DismountFileSystem:Boolean;
begin
 {}
 Result:=False;

 if not WriterLock then Exit;
 try
  if FDriver = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.DismountFileSystem');
  {$ENDIF}

  {Set Clean}
  MarkDismount;

  {Clear Current}
  FNTFSType:=ntNONE;
  FVolumeVersion:=ntfsNTFS12;
  FVolumeFlags:=ntfsVolumeFlagNone;

  FRoot:=nil;

  SetCurrent(nil);

  FChunks.ClearList;
  FTables.ClearList;
  FBlocks.ClearList;
  FEntries.ClearList;

  FSectorsPerCluster:=0;

  FMftStartCluster:=ntfsUnknownCluster;
  FMftMirrorCluster:=ntfsUnknownCluster;

  FMftZoneStart:=ntfsUnknownCluster;
  FMftZoneCluster:=ntfsUnknownCluster;
  FMftMirrorCount:=0;
  FMftZoneReservation:=0;

  FFileRecordSize:=0;
  FIndexRecordSize:=0;

  FClustersPerFile:=0;
  FClustersPerIndex:=0;

  FFilesPerCluster:=0;
  FIndexsPerCluster:=0;

  FEntriesPerBlock:=0;
  FClustersPerBlock:=0;
  FTotalBlockCount:=0;

  FBlockShiftCount:=0;
  FSectorShiftCount:=0;
  FClusterShiftCount:=0;

  FFileRecordShiftCount:=0;
  FFileRecordOffsetMask:=0;

  FIndexCounterShift:=0;
  FIndexCounterOffset:=0;

  FIndexRecordShiftCount:=0;
  FIndexRecordOffsetMask:=0;

  FTotalClusterCount:=0;

  FLastMftCluster:=ntfsUnknownCluster;
  FLastFreeCluster:=ntfsUnknownCluster;
  FFreeClusterCount:=ntfsUnknownCluster;

  FTotalFileRecordCount:=0;

  FLastFreeFileRecord:=ntfsUnknownRecordNumber;
  FFreeFileRecordCount:=ntfsUnknownRecordNumber;
  FReservedFileRecordCount:=ntfsUnknownRecordNumber;

  FClusterSize:=0;

  FMft:=nil;
  FMftMirr:=nil;
  FLogFile:=nil;
  FVolInfo:=nil;
  FAttrDef:=nil;
  FBitmap:=nil;
  FBoot:=nil;
  FBadClus:=nil;
  FSecure:=nil;
  FUpCase:=nil;
  FExtend:=nil;
  FObjId:=nil;
  FQuota:=nil;
  FReparse:=nil;
  FUsnJrnl:=nil;

  FMaster:=nil;
  FMirror:=nil;

  {FUpCases} {Nothing}
  FAttrDefs.ClearList;
  FSecuritys.ClearList;
  FRecords.EmptyBtree;

  if FFileBuffer <> nil then FreeMem(FFileBuffer);
  FFileBuffer:=nil;

  if FIndexBuffer <> nil then FreeMem(FIndexBuffer);
  FIndexBuffer:=nil;

  if FReadBuffer <> nil then FreeMem(FReadBuffer);
  FReadBuffer:=nil;

  if FWriteBuffer <> nil then FreeMem(FWriteBuffer);
  FWriteBuffer:=nil;

  if FClusterBuffer <> nil then FreeMem(FClusterBuffer);
  FClusterBuffer:=nil;

  if FCompressionBuffer <> nil then FreeMem(FCompressionBuffer);
  FCompressionBuffer:=nil;

  if FDecompressionBuffer <> nil then FreeMem(FDecompressionBuffer);
  FDecompressionBuffer:=nil;

  Result:=True;
 finally
  WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.InitializeFileSystem(ASectorsPerCluster:LongWord;AFileSysType:TFileSysType):Boolean;
begin
 {}
 Result:=False;

 if not WriterLock then Exit;
 try
  if FDriver = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.InitializeFileSystem SectorsPerCluster = ' + IntToStr(ASectorsPerCluster));
  {$ENDIF}

  {Check Count}
  if FSectorSize = 0 then Exit;
  if FSectorCount = 0 then Exit;
  if ASectorsPerCluster = 0 then Exit;

  {Check Volume}
  if FVolume = nil then Exit;
  if FVolume.Device = nil then Exit;
  FReadOnly:=not(FVolume.Device.Writeable);

  {Calculate Parameters}
  if FVolume.Partition <> nil then
   begin
    FSectorCount:=FVolume.Partition.SectorCount - 1; {Leave one sector for the Boot Backup}
   end
  else
   begin
    FSectorCount:=FVolume.Device.SectorCount - 1; {Leave one sector for the Boot Backup}
   end;

  FBootSector:=ntfsBootSector;
  FBootBackup:=FSectorCount; {Max Sector - 1}

  FSectorsPerCluster:=ASectorsPerCluster;
  FClusterSize:=(FSectorSize * FSectorsPerCluster);
  FTotalClusterCount:=(FSectorCount div FSectorsPerCluster);

  FMftStartCluster:=CalculateStartCluster(FTotalClusterCount);
  if FMftStartCluster = ntfsUnknownCluster then Exit;
  FMftMirrorCluster:=CalculateMirrorCluster(FTotalClusterCount);
  if FMftMirrorCluster = ntfsUnknownCluster then Exit;

  FFileRecordSize:=ntfsDefaultFileRecordSize;
  FIndexRecordSize:=ntfsDefaultIndexRecordSize;

  FClustersPerFile:=CalculateClustersPerFile(FFileRecordSize,FClusterSize);
  FClustersPerIndex:=CalculateClustersPerIndex(FIndexRecordSize,FClusterSize);

  FFilesPerCluster:=CalculateFilesPerCluster(FFileRecordSize,FClusterSize);
  FIndexsPerCluster:=CalculateIndexsPerCluster(FIndexRecordSize,FClusterSize);

  FMftZoneReservation:=GetMftZoneReservation;
  FMftZoneStart:=ntfsUnknownCluster;
  FMftZoneCluster:=GetMftZoneCluster;
  FMftMirrorCount:=GetMftMirrorCount;

  FBlockShiftCount:=GetBlockShiftCount(FSectorSize * FSectorsPerCluster);
  FSectorShiftCount:=GetSectorShiftCount(FSectorsPerCluster);
  FClusterShiftCount:=GetClusterShiftCount(FSectorSize * FSectorsPerCluster);

  FFileRecordShiftCount:=GetFileRecordShiftCount(FSectorSize * FSectorsPerCluster,FFileRecordSize);
  FFileRecordOffsetMask:=GetFileRecordOffsetMask(FFilesPerCluster);

  FIndexCounterOffset:=GetIndexCounterOffset(FClustersPerIndex);
  FIndexCounterShift:=GetIndexCounterShift(FIndexCounterOffset);

  FIndexRecordShiftCount:=GetIndexRecordShiftCount(FSectorSize * FSectorsPerCluster,FIndexRecordSize);
  FIndexRecordOffsetMask:=GetIndexRecordOffsetMask(FIndexsPerCluster);

  FEntriesPerBlock:=GetEntriesPerBlock(FSectorSize * FSectorsPerCluster);
  FClustersPerBlock:=GetClustersPerBlock(FSectorSize * FSectorsPerCluster);
  FTotalBlockCount:=GetTotalBlockCount(FSectorCount div FSectorsPerCluster);

  {Create Buffers}
  FFileBuffer:=GetMem(Max(FFileRecordSize,FClusterSize));
  if FFileBuffer = nil then Exit;
  FIndexBuffer:=GetMem(Max(FIndexRecordSize,FClusterSize));
  if FIndexBuffer = nil then Exit;
  FReadBuffer:=GetMem(FClusterSize);
  if FReadBuffer = nil then Exit;
  FWriteBuffer:=GetMem(FClusterSize);
  if FWriteBuffer = nil then Exit;
  FClusterBuffer:=GetMem(FClusterSize);
  if FClusterBuffer = nil then Exit;
  FCompressionBuffer:=GetMem((FClusterSize shl 4) + FClusterSize); {Multiply by 16} {One extra cluster for overrun}
  if FCompressionBuffer = nil then Exit;
  FDecompressionBuffer:=GetMem((FClusterSize shl 4) + FClusterSize); {Multiply by 16} {One extra cluster for overrun}
  if FDecompressionBuffer = nil then Exit;

  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then
   begin
    FileSysLogDebug('TNTFSFileSystem.InitializeFileSystem');
    FileSysLogDebug(' SectorSize = ' + IntToStr(FSectorSize));
    FileSysLogDebug(' StartSector = ' + IntToStr(FStartSector));
    FileSysLogDebug(' SectorCount = ' + IntToStr(FSectorCount));
    FileSysLogDebug('');
    FileSysLogDebug(' BootSector = ' + IntToStr(FBootSector));
    FileSysLogDebug(' BootBackup = ' + IntToStr(FBootBackup));
    FileSysLogDebug('');
    FileSysLogDebug(' SectorsPerCluster = ' + IntToStr(FSectorsPerCluster));
    FileSysLogDebug('');
    FileSysLogDebug(' MftStartCluster = ' + IntToStr(FMftStartCluster));
    FileSysLogDebug(' MftMirrorCluster = ' + IntToStr(FMftMirrorCluster));
    FileSysLogDebug('');
    FileSysLogDebug(' FileRecordSize = ' + IntToStr(FFileRecordSize));
    FileSysLogDebug(' IndexRecordSize = ' + IntToStr(FIndexRecordSize));
    FileSysLogDebug('');
    FileSysLogDebug(' ClustersPerFile = ' + IntToStr(FClustersPerFile));
    FileSysLogDebug(' ClustersPerIndex = ' + IntToStr(FClustersPerIndex));
    FileSysLogDebug('');
    FileSysLogDebug(' FilesPerCluster = ' + IntToStr(FFilesPerCluster));
    FileSysLogDebug(' IndexsPerCluster = ' + IntToStr(FIndexsPerCluster));
    FileSysLogDebug('');
    FileSysLogDebug(' MftZoneReservation = ' + IntToStr(FMftZoneReservation));
    FileSysLogDebug(' MftZoneStart = ' + IntToStr(FMftZoneStart));
    FileSysLogDebug(' MftZoneCluster = ' + IntToStr(FMftZoneCluster));
    FileSysLogDebug(' MftMirrorCount = ' + IntToStr(FMftMirrorCount));
    FileSysLogDebug('');
    FileSysLogDebug(' BlockShiftCount = ' + IntToStr(FBlockShiftCount));
    FileSysLogDebug(' SectorShiftCount = ' + IntToStr(FSectorShiftCount));
    FileSysLogDebug(' ClusterShiftCount = ' + IntToStr(FClusterShiftCount));
    FileSysLogDebug('');
    FileSysLogDebug(' FileRecordShiftCount = ' + IntToStr(FFileRecordShiftCount));
    FileSysLogDebug(' FileRecordOffsetMask = ' + IntToStr(FFileRecordOffsetMask));
    FileSysLogDebug('');
    FileSysLogDebug(' IndexCounterShift = ' + IntToStr(FIndexCounterShift));
    FileSysLogDebug(' IndexCounterOffset = ' + IntToStr(FIndexCounterOffset));
    FileSysLogDebug('');
    FileSysLogDebug(' IndexRecordShiftCount = ' + IntToStr(FIndexRecordShiftCount));
    FileSysLogDebug(' IndexRecordOffsetMask = ' + IntToStr(FIndexRecordOffsetMask));
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

  {Determine Type}
  FNTFSType:=FileSysTypeToNTFSType(AFileSysType);
  if FNTFSType = ntNONE then Exit;
  FVolumeVersion:=NTFSTypeToNTFSVersion(FNTFSType);

  {Create Metafiles}
  if not CreateMetafiles then Exit;

  {Set Current}
  SetCurrent(FRoot);

  {Setup Values}
  FSystemName:=LoadSystemName;
  FVolumeName:=LoadVolumeName;
  FVolumeGUID:=LoadVolumeGUID;
  FVolumeSerial:=LoadVolumeSerial;
  FFileSysType:=LoadFileSysType;
  FVolumeFlags:=LoadVolumeFlags;

  {Set Dirty}
  MarkMount;

  Result:=True;
 finally
  WriterUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.GetDriveLabel:String;
begin
 {}
 Result:=ntfsBlankName;

 if not ReaderLock then Exit;
 try
  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetDriveLabel');
  {$ENDIF}

  Result:=LoadVolumeName;
 finally
  ReaderUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.SetDriveLabel(const ALabel:String):Boolean;
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SetDriveLabel');
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                Label = ' + ALabel);
  {$ENDIF}

  Result:=SetVolumeName(ALabel);
 finally
  ReaderUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.SetDriveSerial(ASerial:LongWord):Boolean;
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.SetDriveSerial');
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                Serial = ' + IntToHex(ASerial,8));
  {$ENDIF}

  Result:=SetVolumeSerial(ASerial);
 finally
  ReaderUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.GetDriveFreeSpaceEx:Int64;
begin
 {}
 Result:=0;

 if not ReaderLock then Exit;
 try
  if not FRecords.ReaderLock then Exit; {Required for LoadBlock via GetFreeClusterCount}
  try
   if FDriver = nil then Exit;

   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetDriveFreeSpaceEx');
   {$ENDIF}

   {Check Free Cluster Count}
   if GetFreeClusterCount = ntfsUnknownCluster then Exit;

   {Return Free Space}
   Result:=FFreeClusterCount;
   Result:=(Result * FSectorsPerCluster);
   Result:=(Result * FSectorSize);
   {Modified to ensure Int64 mutliply}
   {Result:=((FFreeClusterCount * FSectorsPerCluster) * FSectorSize);}
  finally
   FRecords.ReaderUnlock;
  end;
 finally
  ReaderUnlock;
 end;
end;

{=============================================================================}

function TNTFSFileSystem.GetDriveTotalSpaceEx:Int64;
begin
 {}
 Result:=0;

 if not ReaderLock then Exit;
 try
  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetDriveTotalSpaceEx');
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

function TNTFSFileSystem.GetDriveInformation(var AClusterSize:LongWord;var ATotalClusterCount,AFreeClusterCount:Int64):Boolean;
{Get Drive Information from internal NTFS data}
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  if not FRecords.ReaderLock then Exit; {Required for LoadBlock via GetFreeClusterCount}
  try
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileSystem.GetDriveInformation');
   {$ENDIF}

   if FDriver = nil then Exit;

   {Check Free Cluster Count}
   if GetFreeClusterCount = ntfsUnknownCluster then Exit;

   {Return Drive Information}
   AClusterSize:=FClusterSize;
   ATotalClusterCount:=FTotalClusterCount;
   AFreeClusterCount:=FFreeClusterCount;

   Result:=True;
  finally
   FRecords.ReaderUnlock;
  end;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure NTFSInit;
var
 Recognizer:TNTFSRecognizer;
begin
 {}
 {Check Initialized}
 if NTFSInitialized then Exit;

 {Check Driver}
 if FileSysDriver = nil then Exit;

 {Check NTFS Enabled}
 if FILESYS_NTFS_ENABLED then
  begin
   {Check Default Stack Size}
   if THREAD_STACK_DEFAULT_SIZE < SIZE_1M then
    begin
     THREAD_STACK_DEFAULT_SIZE:=SIZE_1M;
    end;

   {Create NTFS Recognizer}
   Recognizer:=TNTFSRecognizer.Create(FileSysDriver);
   Recognizer.AllowDrive:=FILESYS_DRIVES_ENABLED;
   Recognizer.AllowDefault:=NTFS_DEFAULT;
   Recognizer.ResetLog:=NTFS_RESET_LOG;
   Recognizer.FixedZone:=NTFS_FIXED_ZONE;
   Recognizer.AltLayout:=NTFS_ALT_LAYOUT;
   Recognizer.Lenient:=NTFS_LENIENT;
   Recognizer.Defensive:=NTFS_DEFENSIVE;
   Recognizer.Aggressive:=NTFS_AGGRESSIVE;
   Recognizer.NoShortNames:=NTFS_NO_SHORT_NAMES;
   Recognizer.NullSecurity:=NTFS_NULL_SECURITY;
   Recognizer.DefaultSecurity:=NTFS_DEFAULT_SECURITY;
  end;

 NTFSInitialized:=True;
end;

{==============================================================================}

procedure NTFSQuit;
var
 NextRecognizer:TRecognizer;
 CurrentRecognizer:TRecognizer;
 NextFileSystem:TFileSystem;
 CurrentFileSystem:TFileSystem;
begin
 {}
 {Check Initialized}
 if not NTFSInitialized then Exit;

 {Check Driver}
 if FileSysDriver = nil then Exit;

 {Terminate FileSystems}
 NextFileSystem:=FileSysDriver.GetFileSystemByNext(nil,True,False,FILESYS_LOCK_READ);
 while NextFileSystem <> nil do
  begin
   CurrentFileSystem:=NextFileSystem;
   NextFileSystem:=FileSysDriver.GetFileSystemByNext(CurrentFileSystem,True,False,FILESYS_LOCK_READ);

   if CurrentFileSystem is TNTFSFileSystem then
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

   if CurrentRecognizer is TNTFSRecognizer then
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

 NTFSInitialized:=False;
end;

{==============================================================================}
{==============================================================================}
{NTFS Functions}

{==============================================================================}
{==============================================================================}
{NTFS Helper Functions}

{==============================================================================}
{==============================================================================}

initialization
 NTFSInit;

{==============================================================================}

finalization
 NTFSQuit;

{==============================================================================}
{==============================================================================}

end.
