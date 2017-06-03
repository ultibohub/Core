{
Ultibo Virtual Disk interface unit.

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

 
Virtual Disk
============


}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit VirtualDisk;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Storage,FileSystem,SysUtils,Classes,Unicode,Ultibo,UltiboUtils,UltiboClasses;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {Virtual Disk specific constants} 
 VIRTUAL_DEVICE_DESCRIPTION = 'Virtual Disk Device'; 
 VIRTUAL_CONTROLLER_DESCRIPTION = 'Virtual Disk Controller';

 {Virtual Contants}
  {Image/Extent Flags}  //To Do //Convert these //VIRTUAL_DEVICE_DESCRIPTION etc
 virtualFlagNone    = $00000000;
 virtualFlagFixed   = $00000001;
 virtualFlagDynamic = $00000002;
 virtualFlagBase    = $00000004;
 virtualFlagDelta   = $00000008;
 virtualFlagDevice  = $00000010;
 virtualFlagSplit   = $00000020;

 {VMware Constants}
  {General}
 vmwareFileExtension = '.vmdk';

 vmwareFileMask = '*.vmdk';

 vmwareNoParentCID = $FFFFFFFF;  {not($00000000)}

 vmwareSparseMagicNumber = $564d444b;  {'VMDK'}

 vmwareEsxSparseMagicNumber    = $44574f43;  {'COWD'}
 vmwareEsxSparseMaxParentLength = 1024;      {COWDISK_MAX_PARENT_FILELEN}
 vmwareEsxSparseMaxNameLength = 60;          {COWDISK_MAX_NAME_LEN}
 vmwareEsxSparseMaxDescriptionLength = 512;  {COWDISK_MAX_DESC_LEN}

 vmwareSectorSize = 512;

  {Disk Types}
 vmwareMaxDiskType = 0;
 vmwareDiskTypeNone = 0;
 //To Do //Not part of image, internal to this unit

 vmwareDiskTypes:array[0..vmwareMaxDiskType] of String = (
  'None');

  {Descriptor Tags}
 vmwareTagDisk = 'Disk DescriptorFile';
 vmwareTagVersion = 'version';
 vmwareTagCID = 'CID';
 vmwareTagParentCID = 'parentCID';
 vmwareTagCreateType = 'createType';
 vmwareTagExtent = 'Extent Description';
 vmwareTagDatabase = 'The Disk Data Base';
 vmwareTagDDB = 'DDB';
 vmwareTagAdapter = 'ddb.adapterType';
 vmwareTagSectors = 'ddb.geometry.sectors';
 vmwareTagHeads = 'ddb.geometry.heads';
 vmwareTagCylinders = 'ddb.geometry.cylinders';
 vmwareTagParentHint = 'parentFileNameHint';

  {Create Types}
 vmwareMaxCreateType = 11;
 vmwareCreateType_monolithicSparse = 0;
 vmwareCreateType_vmfsSparse = 1;
 vmwareCreateType_monolithicFlat = 2;
 vmwareCreateType_vmfs = 3;
 vmwareCreateType_twoGbMaxExtentSparse = 4;
 vmwareCreateType_twoGbMaxExtentFlat = 5;
 vmwareCreateType_fullDevice = 6;
 vmwareCreateType_vmfsRaw = 7;
 vmwareCreateType_partitionedDevice = 8;
 vmwareCreateType_vmfsRawDeviceMap = 9;
 vmwareCreateType_vmfsPassthroughRawDeviceMap = 10;
 vmwareCreateType_streamOptimized = 11;

 vmwareCreateTypes:array[0..vmwareMaxCreateType] of String = (
  'monolithicSparse',
  'vmfsSparse',
  'monolithicFlat',
  'vmfs',
  'twoGbMaxExtentSparse',
  'twoGbMaxExtentFlat',
  'fullDevice',
  'vmfsRaw',
  'partitionedDevice',
  'vmfsRawDeviceMap',
  'vmfsPassthroughRawDeviceMap',
  'streamOptimized');

  {Access Types}
 vmwareMaxAccessType = 2;
 vmwareAccessType_RW = 0;
 vmwareAccessType_RDONLY = 1;
 vmwareAccessType_NOACCESS = 2;

 vmwareAccessTypes:array[0..vmwareMaxAccessType] of String = (
  'RW',
  'RDONLY',
  'NOACCESS');

  {Extent Types}
 vmwareMaxExtentType = 6;
 vmwareExtentType_FLAT = 0;
 vmwareExtentType_SPARSE = 1;
 vmwareExtentType_ZERO = 2;
 vmwareExtentType_VMFS = 3;
 vmwareExtentType_VMFSSPARSE = 4;
 vmwareExtentType_VMFSRDM = 5;
 vmwareExtentType_VMFSRAW = 6;

 vmwareExtentTypes:array[0..vmwareMaxExtentType] of String = (
  'FLAT',
  'SPARSE',
  'ZERO',
  'VMFS',
  'VMFSSPARSE',
  'VMFSRDM',
  'VMFSRAW');

  {Disk Flags}
 vmwareFlagValidTest       = $00000001; {bit 0: valid new line detection test}
 vmwareFlagRedundantGrain  = $00000002; {bit 1: redundant grain table will be used}
 vmwareFlagGrainCompressed = $00010000; {bit 16: the grains are compressed. The type of compression is described by compressAlgorithm}
 vmwareFlagLBAMarkers      = $00020000; {bit 17: there are markers in the virtual disk to identify every block of metadata or data and the markers for the virtual machine data contain a LBA}

  {Compression Types}
 vmwareCompressionNone    = 0;
 vmwareCompressionDeflate = 1; {The deflate algorithm is described in RFC 1951}

 {VPC Constants}
  {General}
 vpcFileExtension = '.vhd';
 vpcUndoExtension = '.vud';

 vpcFileMask = '*.vhd';
 vpcUndoMask = '*.vud';
 vpcSplitMask = '*.v??'; {.v01, v02 etc up to 64}

 vpcFooterCookie = 'conectix';
 vpcDynamicCookie = 'cxsparse';

 vpcMaxDiskExtents = 64;          {Maximum number of 4GB split files in a disk image}
 vpcExtentMaxSize  = 4294967296;  {Maximum size of s split file (4GB)}

 vpcUnixTimeOffset = 946684800;   {Offset from 1/1/1970 to 1/1/2000}

 vpcFooterVersion = $00010000;
 vpcDynamicVersion = $00010000;

 vpcSectorSize = 512;

 vpcUnallocatedBlock = $FFFFFFFF;
 vpcFixedDiskTableOffset = $FFFFFFFFFFFFFFFF;
 vpcDynamicDiskDataOffset = $FFFFFFFFFFFFFFFF;

 vpcMaxBlockGroup    = $FF;
 vpcBlockGroupMask   = $FF;
 vpcBlockGroupOffset = $100;

  {Disk Types}
 vpcMaxDiskType = 6;
 vpcDiskTypeNone = 0;
 vpcDiskTypeReserved1 = 1;
 vpcDiskTypeFixed = 2;
 vpcDiskTypeDynamic = 3;
 vpcDiskTypeDifferencing = 4;
 vpcDiskTypeReserved2 = 5;
 vpcDiskTypeReserved3 = 6;

 vpcDiskTypes:array[0..vpcMaxDiskType] of String = (
  'None',
  'Reserved',
  'Fixed',
  'Dynamic',
  'Differencing',
  'Reserved',
  'Reserved');

  {Features}
 vpcFeatureNone      = $00000000;
 vpcFeatureTemporary = $00000001;
 vpcFeatureReserved  = $00000002;  {This bit must always be set to 1}

  {Creator Host OS}
 vpcCreatorWindows   = $5769326B; {Wi2k}
 vpcCreatorMacintosh = $4D616320; {Mac }

  {Platform Codes}
 vpcPlatformNone   = $00000000;
 vpcPlatformWi2r   = $57693272; {Wi2r deprecated}
 vpcPlatformWi2k   = $5769326B; {Wi2k deprecated}
 vpcPlatformW2ru   = $57327275; {W2ru Unicode pathname (UTF-16) on Windows relative to the differencing disk pathname}
 vpcPlatformW2ku   = $57326B75; {W2ku Absolute Unicode (UTF-16) pathname on Windows}
 vpcPlatformMac    = $4D616320; {Mac  Mac OS alias stored as a blob}
 vpcPlatformMacX   = $4D616358; {MacX A file URL with UTF-8 encoding conforming to RFC 2396}

  {Bitmap Masks}
 vpcBitmapMaskBits = 32;        {LongWord Bitmap Masks}
 vpcBitmapMaskNone = $00000000; {LongWord} {Used for fast counting of free blocks}
 vpcBitmapMaskAll  = $FFFFFFFF; {LongWord} {Used for fast counting of used blocks}
 {vpcBitmapMasks:array[0..31] of LongWord = (
  $00000001,$00000002,$00000004,$00000008,$00000010,$00000020,$00000040,$00000080,
  $00000100,$00000200,$00000400,$00000800,$00001000,$00002000,$00004000,$00008000,
  $00010000,$00020000,$00040000,$00080000,$00100000,$00200000,$00400000,$00800000,
  $01000000,$02000000,$04000000,$08000000,$10000000,$20000000,$40000000,$80000000); }

 vpcBitmapMasks:array[0..31] of LongWord = ( {Ordered to allow for big endian bitmap}
  $00000080,$00000040,$00000020,$00000010,$00000008,$00000004,$00000002,$00000001,
  $00008000,$00004000,$00002000,$00001000,$00000800,$00000400,$00000200,$00000100,
  $00800000,$00400000,$00200000,$00100000,$00080000,$00040000,$00020000,$00010000,
  $80000000,$40000000,$20000000,$10000000,$08000000,$04000000,$02000000,$01000000);
  
 {VirtualBox Constants}
  {General}
 vboxFileExtension = '.vdi';

 vboxDiskBanner    = '<<< Sun xVM VirtualBox Disk Image >>>'; {Plus a LF character in some images (#10)}
 vboxDiskVersion   = $00010001;
 vboxDiskSignature = $BEDA107F;

 vboxFileMask = '*.vdi';
 vboxPathMask = '*.*';
 vboxDeltaPath = '..\Machines\';
 vboxParentPath = '..\..\..\VDI\'; //To Do //Does not have to be this path
 vboxNewParentPath = '..\..\..\HardDisks\'; //To Do //Does not have to be this path
 vboxSnapshotPath = '\Snapshots\';

 vboxUnallocatedBlock = $FFFFFFFF;

  {Disk Types}
 vboxMaxDiskType = 4;
 vboxDiskTypeNone = 0;
 vboxDiskTypeDynamic = 1;
 vboxDiskTypeStatic = 2;
 vboxDiskTypeUnknown = 3;
 vboxDiskTypeDifferencing = 4;

 vboxDiskTypes:array[0..vboxMaxDiskType] of String = (
  'None',
  'Dynamic',
  'Static',
  'Unknown',
  'Differencing');

  {Disk Flags}
 vboxDiskFlagsNone = $00000000;
 //To Do

{==============================================================================}
type
 {Virtual Disk specific types}
 PEsxSparseExtentHeader = ^TEsxSparseExtentHeader;
 TEsxSparseExtentHeader = packed record {Always 2048 bytes} {Only used for Delta disks, not Dynamic}
  magicNumber:LongWord;       {0x44574f43 / 'COWD'}
  version:LongWord;           {The value of this entry should be 1}
  flags:LongWord;             {set to 3}
  numSectors:LongWord;        {total number of sectors on the base disk}
  grainSize:LongWord;         {one sector by default. Can vary from one sector to 1MB}
  gdOffset:LongWord;          {starts at the fourth sector, because the COWDisk_Header structure takes four sectors}
  numGDEntries:LongWord;      {is CEILING(numSectors, gtCoverage)}
  freeSector:LongWord;        {the next free data sector. This needs to be less than the length of the delta link. It is initially set to gdOffset + numGDSectors}
  case Integer of
   0:{root} (cylinders:LongWord;
             heads:LongWord;
             sectors:LongWord;
     {cont}  generation:LongWord;
             name:array[0..vmwareEsxSparseMaxNameLength - 1] of Char;
             description:array[0..vmwareEsxSparseMaxDescriptionLength - 1] of Char;
             savedGeneration:LongWord; {used to detect the unclean shutdown of the delta link. It is initially set to 0}
             reserved:array[0..7] of Char;
             uncleanShutdown:LongWord; {used to trigger the metadata consistency check in case there is an abnormal termination of the program}
             padding:array[0..395] of Char);
   1:{child}(parentFileName:array[0..vmwareEsxSparseMaxParentLength - 1] of Char;
             parentGeneration:LongWord;
     {cont}  generationx:LongWord;
             namex:array[0..vmwareEsxSparseMaxNameLength - 1] of Char;
             descriptionx:array[0..vmwareEsxSparseMaxDescriptionLength - 1] of Char;
             savedGenerationx:LongWord; {used to detect the unclean shutdown of the delta link. It is initially set to 0}
             reservedx:array[0..7] of Char;
             uncleanShutdownx:LongWord; {used to trigger the metadata consistency check in case there is an abnormal termination of the program}
             paddingx:array[0..395] of Char);
 end; {remaining fields are not used. They are present to maintain compatibility with legacy virtual disk formats}

 PVmwareSparseExtentHeader = ^TVmwareSparseExtentHeader;
 TVmwareSparseExtentHeader = packed record {Always 512 bytes} {Used for both Dynamic and Delta disks}
  magicNumber:LongWord;           { 0x564d444b / 'VMDK'}
  version:LongWord;               {The value of this entry should be 1}
  flags:LongWord;                 {See above}
  capacity:Int64;                 {the capacity of this extent in sectors (should be a multiple of the grain size)}
  grainSize:Int64;                {size of a grain in sectors (must be a power of 2 and must be greater than 8 (4KB))}
  descriptorOffset:Int64;         {offset of the embedded descriptor in the extent (expressed in sectors)}
  descriptorSize:Int64;           {valid only if descriptorOffset is non-zero (expressed in sectors)}
  numGTEsPerGT:LongWord;          {the number of entries in a grain table (for VMware virtual disks is 512)}
  rgdOffset:Int64;                {redundant level 0 of metadata (expressed in sectors)}
  gdOffset:Int64;                 {level 0 of metadata (expressed in sectors)}
  overHead:Int64;                 {number of sectors occupied by the metadata}
  uncleanShutdown:ByteBool;       {set to FALSE when VMware software closes an extent}
                                  {Four entries are used to detect when an extent file has been corrupted by transferring it using FTP in text mode}
                                  {The entries should be initialized with the following values}
  singleEndLineChar:Char;         {'\n'}
  nonEndLineChar:Char;            {' '}
  doubleEndLineChar1:Char;        {'\r'}
  doubleEndLineChar2:Char;        {'\n'}
  compressAlgorithm:Word;         {See above}
  pad:array[0..432] of Byte;
 end;

 PVpcDiskGeometry = ^TVpcDiskGeometry;
 TVpcDiskGeometry = packed record {Always 4 bytes}
  Cylinders:Word;    {Cylinders}
  Heads:Byte;        {Heads}
  Sectors:Byte;      {Sectors per track}
 end;

 PVpcParentLocator = ^TVpcParentLocator;
 TVpcParentLocator = packed record {Always 24 bytes}
  PlatformCode:LongWord;          {describes which platform-specific format is used for the file locator (see above)}
  PlatformDataSpace:LongWord;     {the number of 512-byte sectors needed to store the parent hard disk locator}
  PlatformDataLength:LongWord;    {length of the parent hard disk locator in bytes}
  Reserved:LongWord;              {Reserved, must be zero}
  PlatformDataOffset:Int64;       {offset in bytes where the platform specific file locator data is stored}
 end;

 PVpcHardDiskFooter = ^TVpcHardDiskFooter;
 TVpcHardDiskFooter = packed record {Always 512 bytes} {At end of file on Fixed disks, at start and end on Dynamic and Differencing disks}
  Cookie:array[0..7] of Char;             {conectix}
  Features:LongWord;
  Version:LongWord;                       {0x00010000}
  DataOffset:Int64;                       {Offset to Dynamic Disk Header, Must be 0xFFFFFFFF for Fixed Disks}
  TimeStamp:LongWord;                     {seconds since January 1, 2000 12:00:00 AM in UTC/GMT (Unix Time minus 30 years)}
  CreatorApplication:array[0..3] of Char; {Microsoft Virtual PC = "vpc " / Microsoft Virtual Server = "vs  "}
  CreatorVersion:LongWord;                {Virtual Server 2004 = 0x00010000 / Virtual PC 2004 = 0x00050000}
  CreatorHostOS:LongWord;                 {Windows 0x5769326B (Wi2k) / Macintosh 0x4D616320 (Mac )}
  OriginalSize:Int64;                     {Size in Bytes of the VHD at Creation}
  CurrentSize:Int64;                      {Current Size in Bytes of the VHD}
  DiskGeometry:TVpcDiskGeometry;          {See above}
  DiskType:LongWord;                      {See above}
  Checksum:LongWord;                      {1s compliment checksum of the footer}
  UniqueId:TGUID;
  SavedState:ByteBool;                    {Set to 1 if disk is in a saved state}
  Reserved:array[0..426] of Byte;         {Reserved, must be zero}
 end;

 PVpcDynamicDiskHeader = ^TVpcDynamicDiskHeader;
 TVpcDynamicDiskHeader = packed record {Always 1024 bytes} {Used for both Dynamic and Differencing disks}
  Cookie:array[0..7] of Char;                  {cxsparse}
  DataOffset:Int64;                            {currently unused by existing formats and should be set to 0xFFFFFFFF}
  TableOffset:Int64;                           {absolute byte offset of the Block Allocation Table (BAT)}
  HeaderVersion:LongWord;                      {must be initialized to 0x00010000}
  MaxTableEntries:LongWord;                    {maximum entries present in the BAT. equal to the number of blocks in the disk (disk size divided by the blocksize)}
  BlockSize:LongWord;                          {The default value is 0x00200000 (indicating a block size of 2 MB)}
  Checksum:LongWord;                           {1s compliment checksum of the header}
  ParentUniqueId:TGUID;                        {only used for differencing hard disks}
  ParentTimeStamp:LongWord;                    {number of seconds since January 1, 2000 12:00:00 AM in UTC/GMT}
  Reserved1:LongWord;                          {Must be zero}
  ParentUnicodeName:array[0..255] of WideChar; {parent hard disk filename (Unicode /UTF16 string)}
  ParentLocator1:TVpcParentLocator;            {These entries store an absolute byte offset in the file where the parent locator for a differencing hard disk is stored}
  ParentLocator2:TVpcParentLocator;            {These fields are used only for differencing disks and should be set to zero for dynamic disks}
  ParentLocator3:TVpcParentLocator;
  ParentLocator4:TVpcParentLocator;
  ParentLocator5:TVpcParentLocator;
  ParentLocator6:TVpcParentLocator;
  ParentLocator7:TVpcParentLocator;
  ParentLocator8:TVpcParentLocator;
  Reserved2:array[0..255] of Byte;              {Reserved, must be zero}
 end;

 PVboxDiskHeader = ^TVboxDiskHeader;
 TVboxDiskHeader = packed record     {Always 512 bytes} {At start of file for both Fixed and Dynamic disks}
  Banner:array[0..63] of Char;       {<<< Sun xVM VirtualBox Disk Image >>> }
  Signature:LongWord;                {7F 10 DA BE}
  Version:LongWord;                  {1.1}
  HeaderSize:LongWord;               {0x190}
  DiskType:LongWord;                 {see above}
  DiskFlags:LongWord;                {see above}
  Description:array[0..255] of Char; {Unused}
  BlocksOffset:LongWord;
  DataOffset:LongWord;
  Cylinders:LongWord;
  Heads:LongWord;
  Sectors:LongWord;
  SectorSize:LongWord;
  Reserved1:LongWord;                {Must be zero}
  DiskSize:Int64;                    {Bytes}
  BlockSize:LongWord;
  BlockExtra:LongWord;
  TotalBlocks:LongWord;
  AllocatedBlocks:LongWord;
  UUID:TGUID;
  SnapshotUUID:TGUID;
  LinkUUID:TGUID;
  ParentUUID:TGUID;
  Unknown1:LongWord;
  Unknown2:LongWord;
  Reserved2:LongWord;                {Must be 0x3F}
  Reserved3:LongWord;                {Must be 0x200}
  Unused:array[0..9] of LongWord;
 end;
 
{==============================================================================}
type
 {Virtual Disk specific classes}
 TVirtualDiskController = class(TDiskController)
   constructor Create(ADriver:TFileSysDriver);
  private
   {Private Variables}

   {Private Methods}
   
  public
   {Public Variables}

   {Public Methods}
   function ControllerInit:Boolean; override;

   function LocateDevices:Boolean; override;

   function Read(ADevice:TDiskDevice;ASector:LongWord;ACount:Word;var ABuffer):Boolean; override;
   function Write(ADevice:TDiskDevice;ASector:LongWord;ACount:Word;const ABuffer):Boolean; override;

   function Reset(ADevice:TDiskDevice):Boolean; override;

   function LockMedia(ADevice:TDiskDevice):Boolean; override;
   function UnlockMedia(ADevice:TDiskDevice):Boolean; override;
   function EjectMedia(ADevice:TDiskDevice):Boolean; override;

   function MediaReady(ADevice:TDiskDevice):Boolean; override;
   function MediaChanged(ADevice:TDiskDevice):Boolean; override;
   function MediaLocked(ADevice:TDiskDevice):Boolean; override;

   {Device Methods}
   function Information(ADevice:TDiskDevice):String; override;

   function LBA(ADevice:TDiskDevice):Boolean; override;

   function MediaType(ADevice:TDiskDevice):TMediaType; override;
   function FloppyType(ADevice:TDiskDevice):TFloppyType; override;
   function Ready(ADevice:TDiskDevice):Boolean; override;
   function Locked(ADevice:TDiskDevice):Boolean; override;
   function Lockable(ADevice:TDiskDevice):Boolean; override;
   function Readable(ADevice:TDiskDevice):Boolean; override;
   function Writeable(ADevice:TDiskDevice):Boolean; override;
   function Removable(ADevice:TDiskDevice):Boolean; override;
   function ChangeLine(ADevice:TDiskDevice):Boolean; override;

   function PhysicalCylinders(ADevice:TDiskDevice):LongWord; override;
   function PhysicalHeads(ADevice:TDiskDevice):LongWord; override;
   function PhysicalSectors(ADevice:TDiskDevice):LongWord; override;

   function LogicalCylinders(ADevice:TDiskDevice):LongWord; override;
   function LogicalHeads(ADevice:TDiskDevice):LongWord; override;
   function LogicalSectors(ADevice:TDiskDevice):LongWord; override;

   function SectorSize(ADevice:TDiskDevice):Word; override;
   function SectorCount(ADevice:TDiskDevice):Int64; override;
 end;

 TVirtualDiskDevice = class(TDiskDevice)
  private
   {Private Variables}

   {Private Methods}


  public
   {Public Variables}


   {Public Methods}
   function DeviceInit:Boolean; override;

   function LocatePartitions:Boolean; override;
   function LocateVolumes:Boolean; override;

   function CreatePartition(AParent:TDiskPartition;APartitionId:Byte;ACount:LongWord;AActive:Boolean):Boolean; override;
   function DeletePartition(APartition:TDiskPartition):Boolean; override;
   function ModifyPartition(APartition:TDiskPartition;APartitionId:Byte):Boolean; override;
   function ActivatePartition(APartition:TDiskPartition;AActive:Boolean):Boolean; override;
   function ShrinkPartition(APartition:TDiskPartition;const AStart,ASize:Int64):Boolean; override;
   function ExpandPartition(APartition:TDiskPartition;const AStart,ASize:Int64):Boolean; override;
 end;

 TVirtualDiskPartition = class(TDiskPartition)
  private
   {Private Variables}

   {Private Methods}

  public
   {Public Methods}
   function PartitionInit:Boolean; override;

   function LocatePartitions:Boolean; override;
   function LocateVolumes:Boolean; override;
 end;

 TVirtualDiskExtent = class;
 TVirtualDiskTable = class;
 TVirtualDiskBlock = class;
 TVirtualDiskImage = class(TDiskImage)
   constructor Create(ADriver:TFileSysDriver;AController:TDiskController;const AName:String;AImageNo:Integer);
   destructor Destroy; override;
  private
   {Private Variables}

   {Private Methods}
  protected
   {Protected Variables}
   FFlags:LongWord;

   FExtents:TFileSysList;            {List of Virtual Disk Extents}
   
   FBase:TVirtualDiskExtent;         {The first extent of the image}
   FCurrent:TVirtualDiskExtent;      {The current delta extent if applicable}

   {Protected Methods}
   function GetCylinders:LongWord; override;
   function GetHeads:LongWord; override;
   function GetSectors:LongWord; override;

   {Extent Methods}
   function ReadExtent(AExtent:TVirtualDiskExtent;ASector:LongWord;ACount:Word;var ABuffer):Word; virtual;
   function WriteExtent(AExtent:TVirtualDiskExtent;ASector:LongWord;ACount:Word;const ABuffer):Word; virtual;

   function LoadExtents:Boolean; virtual;
   function CloseExtents:Boolean; virtual;

   function CheckExtent(ADelta,AParent:TVirtualDiskExtent;const AFilename:String):Boolean; virtual;
   function LoadExtent(ADelta,AParent:TVirtualDiskExtent;const AFilename:String):TVirtualDiskExtent; virtual;

   function AddExtent(AParent:TVirtualDiskExtent;const AFilename:String):TVirtualDiskExtent; virtual;
   function RemoveExtent(AExtent:TVirtualDiskExtent):Boolean; virtual;

   function SetExtent(AExtent:TVirtualDiskExtent):Boolean; virtual;
   function GetExtent(const ASector:Int64;AWrite,ALock:Boolean):TVirtualDiskExtent; virtual;

   function FindExtent(const AFilename:String;ALock:Boolean):TVirtualDiskExtent; virtual;

   {Table Methods}
   function LoadTables:Boolean; virtual;

   function LoadTable(AExtent:TVirtualDiskExtent;ATableNo:LongWord):TVirtualDiskTable; virtual;

   function AddTable(AExtent:TVirtualDiskExtent):TVirtualDiskTable; virtual;
   function RemoveTable(ATable:TVirtualDiskTable):Boolean; virtual;

   function SetTable(ATable:TVirtualDiskTable):Boolean; virtual;
   function GetTable(AExtent:TVirtualDiskExtent;const ASector:Int64;AWrite:Boolean):TVirtualDiskTable; virtual;

   {Block Methods}
   function LoadBlocks(ATable:TVirtualDiskTable):Boolean; virtual;

   function LoadBlock(ATable:TVirtualDiskTable;ABlockNo:LongWord):TVirtualDiskBlock; virtual;

   function AddBlock(ATable:TVirtualDiskTable):TVirtualDiskBlock; virtual;
   function RemoveBlock(ABlock:TVirtualDiskBlock):Boolean; virtual;

   function SetBlock(ABlock:TVirtualDiskBlock):Boolean; virtual;
   function GetBlock(ATable:TVirtualDiskTable;const ASector:Int64;AWrite:Boolean):TVirtualDiskBlock; virtual;

   {Locate Methods}
   function LocateDelta(AExtent:TVirtualDiskExtent):String; virtual;
   function LocateParent(AExtent:TVirtualDiskExtent):String; virtual;
   function LocateSibling(AExtent:TVirtualDiskExtent):String; virtual;
  public
   {Public Properties}
   property Flags:LongWord read FFlags write FFlags;

   property Extents:TFileSysList read FExtents;

   property Base:TVirtualDiskExtent read FBase;
   property Current:TVirtualDiskExtent read FCurrent;

   {Public Variables}

   {Public Methods}
   function IsSplit:Boolean; virtual;
 end;

 TVirtualDiskMemoryImage = class(TVirtualDiskImage)
   constructor Create(ADriver:TFileSysDriver;AController:TDiskController;const AName:String;AImageNo:Integer);
   destructor Destroy; override;
  private
   {Private Variables}
   FData:Pointer;

   {Private Methods}
  protected
   {Protected Variables}

   {Protected Methods}
   function GetReady:Boolean; override;

   function GetSectorSize:Word; override;
   function GetSectorCount:Int64; override;

   function GetPartitionId:Byte; override;

   procedure SetName(const AName:String); override;
  public
   {Public Variables}

   {Public Methods}
   function Read(ASector:LongWord;ACount:Word;var ABuffer):Boolean; override;
   function Write(ASector:LongWord;ACount:Word;const ABuffer):Boolean; override;

   function ConvertImage(AImageType:TImageType):Boolean; override;

   {Image Methods}
   function CreateImage(AMediaType:TMediaType;AFloppyType:TFloppyType;AAttributes:LongWord;ASectorSize:Word;const ASectorCount:Int64;ACylinders,AHeads,ASectors:LongWord;APartitionId:Byte):Integer; override;
   function CloseImage:Boolean; override;
 end;

 TVirtualDiskFileImage = class(TVirtualDiskImage)
   constructor Create(ADriver:TFileSysDriver;AController:TDiskController;const AName:String;AImageNo:Integer);
   destructor Destroy; override;
  private
   {Private Variables}
   FHandle:THandle;

   {Private Methods}
  protected
   {Protected Variables}

   {Protected Methods}
   function GetReady:Boolean; override;

   function GetSectorSize:Word; override;
   function GetSectorCount:Int64; override;

   function GetPartitionId:Byte; override;
  public
   {Public Variables}

   {Public Methods}
   function Read(ASector:LongWord;ACount:Word;var ABuffer):Boolean; override;
   function Write(ASector:LongWord;ACount:Word;const ABuffer):Boolean; override;

   {Image Methods}
   function CreateImage(AMediaType:TMediaType;AFloppyType:TFloppyType;AAttributes:LongWord;ASectorSize:Word;const ASectorCount:Int64;ACylinders,AHeads,ASectors:LongWord;APartitionId:Byte):Integer; override;
   function OpenImage(AMediaType:TMediaType;AFloppyType:TFloppyType;AAttributes:LongWord;ASectorSize:Word;const ASectorCount:Int64;ACylinders,AHeads,ASectors:LongWord;APartitionId:Byte):Integer; override;
   function CloseImage:Boolean; override;
   function ResizeImage(const ASectorCount:Int64):Boolean; override;
 end;

 TVirtualDiskDeviceImage = class(TVirtualDiskImage)
   constructor Create(ADriver:TFileSysDriver;AController:TDiskController;const AName:String;AImageNo:Integer);
   destructor Destroy; override;
  private
   {Private Variables}
   FHandle:THandle;

   {Private Methods}
  protected
   {Protected Variables}

   {Protected Methods}
   function GetReady:Boolean; override;

   function GetCylinders:LongWord; override;
   function GetHeads:LongWord; override;
   function GetSectors:LongWord; override;

   function GetSectorSize:Word; override;
   function GetSectorCount:Int64; override;

   function GetPartitionId:Byte; override;
  public
   {Public Variables}

   {Public Methods}
   function Read(ASector:LongWord;ACount:Word;var ABuffer):Boolean; override;
   function Write(ASector:LongWord;ACount:Word;const ABuffer):Boolean; override;

   function ConvertImage(AImageType:TImageType):Boolean; override;
   function ShrinkImage(const ASize:Int64):Boolean; override;
   function ExpandImage(const ASize:Int64):Boolean; override;

   {Image Methods}
   function OpenImage(AMediaType:TMediaType;AFloppyType:TFloppyType;AAttributes:LongWord;ASectorSize:Word;const ASectorCount:Int64;ACylinders,AHeads,ASectors:LongWord;APartitionId:Byte):Integer; override;
   function CloseImage:Boolean; override;
 end;

 TVirtualDiskIsoImage = class(TVirtualDiskImage)
   constructor Create(ADriver:TFileSysDriver;AController:TDiskController;const AName:String;AImageNo:Integer);
   destructor Destroy; override;
  private
   {Private Variables}
   FHandle:THandle;

   {Private Methods}
  protected
   {Protected Variables}

   {Protected Methods}
   function GetReady:Boolean; override;

   function GetSectorSize:Word; override;
   function GetSectorCount:Int64; override;
  public
   {Public Variables}

   {Public Methods}
   function Read(ASector:LongWord;ACount:Word;var ABuffer):Boolean; override;
   function Write(ASector:LongWord;ACount:Word;const ABuffer):Boolean; override;

   function ConvertImage(AImageType:TImageType):Boolean; override;

   {Image Methods}
   function CreateImage(AMediaType:TMediaType;AFloppyType:TFloppyType;AAttributes:LongWord;ASectorSize:Word;const ASectorCount:Int64;ACylinders,AHeads,ASectors:LongWord;APartitionId:Byte):Integer; override;
   function OpenImage(AMediaType:TMediaType;AFloppyType:TFloppyType;AAttributes:LongWord;ASectorSize:Word;const ASectorCount:Int64;ACylinders,AHeads,ASectors:LongWord;APartitionId:Byte):Integer; override;
   function CloseImage:Boolean; override;
   function ResizeImage(const ASectorCount:Int64):Boolean; override;
 end;

 TVirtualDiskBochsImage = class(TVirtualDiskImage)
   constructor Create(ADriver:TFileSysDriver;AController:TDiskController;const AName:String;AImageNo:Integer);
   destructor Destroy; override;
  private
   {Private Variables}

   {Private Methods}
  protected
   {Protected Variables}

   {Protected Methods}
   function GetReady:Boolean; override;

   function GetCylinders:LongWord; override;
   function GetHeads:LongWord; override;
   function GetSectors:LongWord; override;

   function GetSectorSize:Word; override;
   function GetSectorCount:Int64; override;
  public
   {Public Variables}

   {Public Methods}
   function Read(ASector:LongWord;ACount:Word;var ABuffer):Boolean; override;
   function Write(ASector:LongWord;ACount:Word;const ABuffer):Boolean; override;
   function Allocated(ASector:LongWord;ACount:Word):Word; override;

   {Image Methods}
   function CreateImage(AMediaType:TMediaType;AFloppyType:TFloppyType;AAttributes:LongWord;ASectorSize:Word;const ASectorCount:Int64;ACylinders,AHeads,ASectors:LongWord;APartitionId:Byte):Integer; override;
   function OpenImage(AMediaType:TMediaType;AFloppyType:TFloppyType;AAttributes:LongWord;ASectorSize:Word;const ASectorCount:Int64;ACylinders,AHeads,ASectors:LongWord;APartitionId:Byte):Integer; override;
   function CloseImage:Boolean; override;
   function ResizeImage(const ASectorCount:Int64):Boolean; override;
 end;

 TVirtualDiskVmwareExtent = class;
 TVirtualDiskVmwareImage = class(TVirtualDiskImage)
   constructor Create(ADriver:TFileSysDriver;AController:TDiskController;const AName:String;AImageNo:Integer);
   destructor Destroy; override;
  private
   {Private Variables}

   {Private Methods}
   function GetAttributes:LongWord;
  protected
   {Protected Variables}
   FDescriptor:TVirtualDiskExtent;

   {Protected Methods}
   function GetReady:Boolean; override;

   function GetCylinders:LongWord; override;
   function GetHeads:LongWord; override;
   function GetSectors:LongWord; override;

   function GetSectorSize:Word; override;
   function GetSectorCount:Int64; override;
  public
   {Public Properties}
   property Descriptor:TVirtualDiskExtent read FDescriptor;

   {Public Variables}

   {Public Methods}
   function ImageInit:Boolean; override;

   function Read(ASector:LongWord;ACount:Word;var ABuffer):Boolean; override;
   function Write(ASector:LongWord;ACount:Word;const ABuffer):Boolean; override;
   function Allocated(ASector:LongWord;ACount:Word):Word; override;

   {Descriptor Methods}
   function LoadDescriptor(const AFilename:String):Boolean;

   function AddDescriptor(const AFilename:String):TVirtualDiskVmwareExtent;
   function RemoveDescriptor(AExtent:TVirtualDiskVmwareExtent):Boolean;

   function SetDescriptor(AExtent:TVirtualDiskVmwareExtent):Boolean;
   function GetDescriptor:TVirtualDiskVmwareExtent;

   {Image Methods}
   function CreateImage(AMediaType:TMediaType;AFloppyType:TFloppyType;AAttributes:LongWord;ASectorSize:Word;const ASectorCount:Int64;ACylinders,AHeads,ASectors:LongWord;APartitionId:Byte):Integer; override;
   function OpenImage(AMediaType:TMediaType;AFloppyType:TFloppyType;AAttributes:LongWord;ASectorSize:Word;const ASectorCount:Int64;ACylinders,AHeads,ASectors:LongWord;APartitionId:Byte):Integer; override;
   function CloseImage:Boolean; override;
   function ResizeImage(const ASectorCount:Int64):Boolean; override;

   function CreateSnapshot:Boolean; override;
   function DeleteSnapshot:Boolean; override;
   function MergeSnapshot:Boolean; override;
 end;

 TVirtualDiskVpcImage = class(TVirtualDiskImage)
   constructor Create(ADriver:TFileSysDriver;AController:TDiskController;const AName:String;AImageNo:Integer);
   destructor Destroy; override;
  private
   {Private Variables}

   {Private Methods}
   function GetAttributes:LongWord;

   function ChecksumFooter(AFooter:PVpcHardDiskFooter):LongWord;
   function ChecksumSparse(ASparse:PVpcDynamicDiskHeader):LongWord;

   function RoundToSector(AValue:LongWord;ASectorSize:Word):LongWord;
  protected
   {Protected Variables}

   {Protected Methods}
   function GetReady:Boolean; override;

   function GetCylinders:LongWord; override;
   function GetHeads:LongWord; override;
   function GetSectors:LongWord; override;

   function GetSectorSize:Word; override;
   function GetSectorCount:Int64; override;

   {Extent Methods}
   function ReadExtent(AExtent:TVirtualDiskExtent;ASector:LongWord;ACount:Word;var ABuffer):Word; override;
   function WriteExtent(AExtent:TVirtualDiskExtent;ASector:LongWord;ACount:Word;const ABuffer):Word; override;

   function LoadExtents:Boolean; override;

   //function CheckSplit //To Do
   //function LoadSplit //To Do

   function CheckExtent(ADelta,AParent:TVirtualDiskExtent;const AFilename:String):Boolean; override;
   function LoadExtent(ADelta,AParent:TVirtualDiskExtent;const AFilename:String):TVirtualDiskExtent; override;

   function AddExtent(AParent:TVirtualDiskExtent;const AFilename:String):TVirtualDiskExtent; override;

   function SetExtent(AExtent:TVirtualDiskExtent):Boolean; override;
   function GetExtent(const ASector:Int64;AWrite,ALock:Boolean):TVirtualDiskExtent; override;

   {Table Methods}
   function LoadTables:Boolean; override;

   function LoadTable(AExtent:TVirtualDiskExtent;ATableNo:LongWord):TVirtualDiskTable; override;

   function AddTable(AExtent:TVirtualDiskExtent):TVirtualDiskTable; override;

   function SetTable(ATable:TVirtualDiskTable):Boolean; override;

   {Block Methods}
   function LoadBlocks(ATable:TVirtualDiskTable):Boolean; override;

   function LoadBlock(ATable:TVirtualDiskTable;ABlockNo:LongWord):TVirtualDiskBlock; override;

   function AddBlock(ATable:TVirtualDiskTable):TVirtualDiskBlock; override;

   function SetBlock(ABlock:TVirtualDiskBlock):Boolean; override;
   function GetBlock(ATable:TVirtualDiskTable;const ASector:Int64;AWrite:Boolean):TVirtualDiskBlock; override;

   function TestBlock(ABlock:TVirtualDiskBlock;const ASector:Int64;ACount:LongWord;AUsed:Boolean):LongWord;
   function MarkBlock(ABlock:TVirtualDiskBlock;const ASector:Int64;ACount:LongWord;AUsed:Boolean):Boolean;

   {Bitmap Methods}
   function TestBitmap(ABuffer:Pointer;ASize,AStart,ACount:LongWord;AUsed:Boolean):LongWord;
   function MarkBitmap(ABuffer:Pointer;ASize,AStart,ACount:LongWord;AUsed:Boolean):Boolean;

   {Locate Methods}
   function LocateDelta(AExtent:TVirtualDiskExtent):String; override;
   function LocateParent(AExtent:TVirtualDiskExtent):String; override;
   function LocateSibling(AExtent:TVirtualDiskExtent):String; override;
  public
   {Public Properties}

   {Public Variables}

   {Public Methods}
   function ImageInit:Boolean; override;

   function Read(ASector:LongWord;ACount:Word;var ABuffer):Boolean; override;
   function Write(ASector:LongWord;ACount:Word;const ABuffer):Boolean; override;
   function Allocated(ASector:LongWord;ACount:Word):Word; override;

   {Image Methods}
   function CreateImage(AMediaType:TMediaType;AFloppyType:TFloppyType;AAttributes:LongWord;ASectorSize:Word;const ASectorCount:Int64;ACylinders,AHeads,ASectors:LongWord;APartitionId:Byte):Integer; override;
   function OpenImage(AMediaType:TMediaType;AFloppyType:TFloppyType;AAttributes:LongWord;ASectorSize:Word;const ASectorCount:Int64;ACylinders,AHeads,ASectors:LongWord;APartitionId:Byte):Integer; override;
   function CloseImage:Boolean; override;
   function ResizeImage(const ASectorCount:Int64):Boolean; override;

   function CreateSnapshot:Boolean; override;
   function DeleteSnapshot:Boolean; override;
   function MergeSnapshot:Boolean; override;
 end;

 TVirtualDiskVboxImage = class(TVirtualDiskImage)
   constructor Create(ADriver:TFileSysDriver;AController:TDiskController;const AName:String;AImageNo:Integer);
   destructor Destroy; override;
  private
   {Private Variables}

   {Private Methods}
   function GetAttributes:LongWord;
  protected
   {Protected Variables}

   {Protected Methods}
   function GetReady:Boolean; override;

   function GetCylinders:LongWord; override;
   function GetHeads:LongWord; override;
   function GetSectors:LongWord; override;

   function GetSectorSize:Word; override;
   function GetSectorCount:Int64; override;

   {Extent Methods}
   function ReadExtent(AExtent:TVirtualDiskExtent;ASector:LongWord;ACount:Word;var ABuffer):Word; override;
   function WriteExtent(AExtent:TVirtualDiskExtent;ASector:LongWord;ACount:Word;const ABuffer):Word; override;

   function LoadExtents:Boolean; override;

   function CheckExtent(ADelta,AParent:TVirtualDiskExtent;const AFilename:String):Boolean; override;
   function LoadExtent(ADelta,AParent:TVirtualDiskExtent;const AFilename:String):TVirtualDiskExtent; override;

   function AddExtent(AParent:TVirtualDiskExtent;const AFilename:String):TVirtualDiskExtent; override;

   function SetExtent(AExtent:TVirtualDiskExtent):Boolean; override;
   function GetExtent(const ASector:Int64;AWrite,ALock:Boolean):TVirtualDiskExtent; override;

   {Table Methods}
   function LoadTables:Boolean; override;

   function LoadTable(AExtent:TVirtualDiskExtent;ATableNo:LongWord):TVirtualDiskTable; override;

   function AddTable(AExtent:TVirtualDiskExtent):TVirtualDiskTable; override;

   function SetTable(ATable:TVirtualDiskTable):Boolean; override;

   {Locate Methods}
   function LocateDelta(AExtent:TVirtualDiskExtent):String; override;
   function LocateParent(AExtent:TVirtualDiskExtent):String; override;
  public
   {Public Properties}

   {Public Variables}

   {Public Methods}
   function ImageInit:Boolean; override;

   function Read(ASector:LongWord;ACount:Word;var ABuffer):Boolean; override;
   function Write(ASector:LongWord;ACount:Word;const ABuffer):Boolean; override;
   function Allocated(ASector:LongWord;ACount:Word):Word; override;

   {Image Methods}
   function CreateImage(AMediaType:TMediaType;AFloppyType:TFloppyType;AAttributes:LongWord;ASectorSize:Word;const ASectorCount:Int64;ACylinders,AHeads,ASectors:LongWord;APartitionId:Byte):Integer; override;
   function OpenImage(AMediaType:TMediaType;AFloppyType:TFloppyType;AAttributes:LongWord;ASectorSize:Word;const ASectorCount:Int64;ACylinders,AHeads,ASectors:LongWord;APartitionId:Byte):Integer; override;
   function CloseImage:Boolean; override;
   function ResizeImage(const ASectorCount:Int64):Boolean; override;

   function CreateSnapshot:Boolean; override;
   function DeleteSnapshot:Boolean; override;
   function MergeSnapshot:Boolean; override;
 end;

 TVirtualRecognizer = class(TRecognizer)
   constructor Create(ADriver:TFileSysDriver;AController:TDiskController);
   destructor Destroy; override;
  private
   {Private Variables}

   {Private Methods}
  protected
   {Parent Objects}
   FController:TDiskController;
   
   {Protected Variables}

   {Protected Methods}
   function GetName:String; override;
  public
   {Public Variables}

   {Public Methods}

   {Image Methods}
   function RecognizeImage(AImage:TDiskImage):Boolean; override;
   function MountImage(AImage:TDiskImage):Boolean; override;
   function InsertImage(AImage:TDiskImage):Boolean; override;
 end;

 TVirtualDiskResizer = class(TDiskResizer)
   constructor Create(ADriver:TFileSysDriver;ARecognizer:TRecognizer);
   destructor Destroy; override;
  private
   {Private Variables}

   {Private Methods}
  public
   {Public Variables}

   {Public Methods}
   function AcceptImage(AImage:TDiskImage;const ASize:Int64):Boolean; override;

   function ShrinkImage(AImage:TDiskImage;const ASize:Int64):Boolean; override;
   function ExpandImage(AImage:TDiskImage;const ASize:Int64):Boolean; override;
 end;

 TVirtualDiskCopier = class(TDiskCopier)
   constructor Create(ADriver:TFileSysDriver;ARecognizer:TRecognizer);
   destructor Destroy; override;
  private
   {Private Variables}

   {Private Methods}
  public
   {Public Variables}

   {Public Methods}
   function AcceptImage(AImage,ADest:TDiskImage):Boolean; override;

   function CopyImage(AImage,ADest:TDiskImage):Boolean; override;
 end;

 TVirtualDiskImager = class(TDiskImager)
   constructor Create(ADriver:TFileSysDriver;ARecognizer:TRecognizer;AController:TDiskController);
   destructor Destroy; override;
  private
   {Private Variables}

   {Private Methods}
   function CreateImageByType(const AName:String;AImageType:TImageType):TDiskImage;
  protected
   {Parent Objects}
   FController:TDiskController;
   {Protected Variables}

   {Protected Methods}
  public
   {Public Variables}

   {Public Methods}
   function AcceptImage(AImage:TDiskImage;const AName:String;AImageType:TImageType;AMediaType:TMediaType;AFloppyType:TFloppyType;AAttributes:LongWord;ASectorSize:Word;const ASectorCount:Int64):Boolean; override;

   function CreateImage(AImage:TDiskImage;const AName:String;AImageType:TImageType;AMediaType:TMediaType;AFloppyType:TFloppyType;AAttributes:LongWord;ASectorSize:Word;const ASectorCount:Int64;ACylinders,AHeads,ASectors:LongWord;APartitionId:Byte):Integer; override;
   function OpenImage(AImage:TDiskImage;const AName:String;AImageType:TImageType;AMediaType:TMediaType;AFloppyType:TFloppyType;AAttributes:LongWord;ASectorSize:Word;const ASectorCount:Int64;ACylinders,AHeads,ASectors:LongWord;APartitionId:Byte):Integer; override;
   function CloseImage(AImage:TDiskImage):Boolean; override;

   function AcceptSnapshot(AImage:TDiskImage):Boolean; override;

   function CreateSnapshot(AImage:TDiskImage):Boolean; override;
   function DeleteSnapshot(AImage:TDiskImage):Boolean; override;
   function MergeSnapshot(AImage:TDiskImage):Boolean; override;
 end;

 TVirtualDiskExtent = class(TListObject) {Represents an Extent (usually a file) forming part of a virtual disk}
   constructor Create(AImage:TVirtualDiskImage;ADelta,AParent:TVirtualDiskExtent);
   destructor Destroy; override;
  private
   {Private Variables}
   FLock:TMutexHandle;

   {Private Methods}
   
  protected
   {Parent Objects}
   FImage:TVirtualDiskImage;
   FDelta:TVirtualDiskExtent;
   FParent:TVirtualDiskExtent;

   {Protected Variables}
   FFlags:LongWord;

   FFilename:String;          {Filename of the file backing this Extent}
   FHandle:THandle;           {DiskDriver Handle to the file of this Extent}

   FDataOffset:Int64;         {Absolute byte offset of Data in file (Where Applicable)}
   FBlockSize:LongWord;       {Size of each data block in file}

   FStartSector:Int64;        {First Sector contained by this Extent}
   FSectorCount:Int64;        {Number of Sectors contained by this Extent}

   FBlockShiftCount:LongWord; {Sector to Block Shift Count}

   {Protected Methods}
   function GetFilename:String;
   procedure SetFilename(const AFilename:String);
   
   function GetBlockShiftCount:LongWord; virtual;
  public
   {Public Properties}
   property Image:TVirtualDiskImage read FImage;
   property Delta:TVirtualDiskExtent read FDelta write FDelta;
   property Parent:TVirtualDiskExtent read FParent write FParent;

   property Flags:LongWord read FFlags write FFlags;

   property Filename:String read GetFilename write SetFilename;
   property Handle:THandle read FHandle write FHandle;

   property DataOffset:Int64 read FDataOffset write FDataOffset;
   property BlockSize:LongWord read FBlockSize write FBlockSize;

   property StartSector:Int64 read FStartSector write FStartSector;
   property SectorCount:Int64 read FSectorCount write FSectorCount;

   property BlockShiftCount:LongWord read FBlockShiftCount write FBlockShiftCount;

   {Public Variables}

   {Public Methods}
   function AcquireLock:Boolean;
   function ReleaseLock:Boolean;

   function IsFixed:Boolean; virtual;
   function IsDynamic:Boolean; virtual;

   function IsBase:Boolean; virtual;
   function IsDelta:Boolean; virtual;

   function IsDevice:Boolean; virtual;

   function HasDelta:Boolean; virtual;
 end;

 TVirtualDiskVmwareTable = class;
 TVirtualDiskVmwareExtent = class(TVirtualDiskExtent)
  private
   {Private Variables}

   {Private Methods}
  protected
   {Parent Objects}

   {Protected Variables}

   {Protected Methods}
  public
   {Public Properties}

   {Public Variables}

   {Public Methods}
   function IsDescriptor:Boolean;
   function HasDescriptor:Boolean;
 end;

 TVirtualDiskVpcTable = class;
 TVirtualDiskVpcExtent = class(TVirtualDiskExtent)
   constructor Create(AImage:TVirtualDiskImage;ADelta,AParent:TVirtualDiskExtent);
   destructor Destroy; override;
  private
   {Private Variables}
   FHeader:PVpcHardDiskFooter;
   FFooter:PVpcHardDiskFooter;
   FSparse:PVpcDynamicDiskHeader;

   FHeaderOffset:Int64;   {Absolute byte offset of Header in file}
   FHeaderSize:LongWord;  {Size of Header in image}

   FFooterOffset:Int64;   {Absolute byte offset of Footer in file}
   FFooterSize:LongWord;  {Size of Footer in image}

   FSparseOffset:Int64;   {Absolute byte offset of Dynamic Header in file}
   FSparseSize:LongWord;  {Size of Dynamic Header in image}

   {Private Methods}
  protected
   {Parent Objects}

   {Protected Variables}
   FTable:TVirtualDiskVpcTable; {VirtualPC images contain only one Table per Extent}
   FGroups:TFileSysList;        {Block bitmap Groups}
   FGroupLocal:TMutexHandle;    {Local Lock shared by all Block bitmap Groups}

   {Protected Methods}
  public
   {Public Properties}
   property Header:PVpcHardDiskFooter read FHeader;
   property Footer:PVpcHardDiskFooter read FFooter;
   property Sparse:PVpcDynamicDiskHeader read FSparse;

   property Table:TVirtualDiskVpcTable read FTable;
   property Groups:TFileSysList read FGroups;
   
   property HeaderOffset:Int64 read FHeaderOffset write FHeaderOffset;
   property HeaderSize:LongWord read FHeaderSize write FHeaderSize;

   property FooterOffset:Int64 read FFooterOffset write FFooterOffset;
   property FooterSize:LongWord read FFooterSize write FFooterSize;

   property SparseOffset:Int64 read FSparseOffset write FSparseOffset;
   property SparseSize:LongWord read FSparseSize write FSparseSize;

   {Public Variables}

   {Public Methods}
   function HasFooter:Boolean;
 end;

 TVirtualDiskVboxTable = class;
 TVirtualDiskVboxExtent = class(TVirtualDiskExtent)
   constructor Create(AImage:TVirtualDiskImage;ADelta,AParent:TVirtualDiskExtent);
   destructor Destroy; override;
  private
   {Private Variables}
   FHeader:PVboxDiskHeader; {Header from Disk Extent}

   FHeaderOffset:Int64;     {Absolute byte offset of Header in file}
   FHeaderSize:LongWord;    {Size of Header in file}

   {Private Methods}
  protected
   {Parent Objects}

   {Protected Variables}
   FTable:TVirtualDiskVboxTable; {VirtualBox images contain only one Table per Extent}

   {Protected Methods}
  public
   {Public Properties}
   property Header:PVboxDiskHeader read FHeader;
   property Table:TVirtualDiskVboxTable read FTable;

   property HeaderOffset:Int64 read FHeaderOffset write FHeaderOffset;
   property HeaderSize:LongWord read FHeaderSize write FHeaderSize;

   {Public Variables}

   {Public Methods}
 end;

 TVirtualDiskTable = class(TListObject) {Represents a Table of blocks within an Extent of a virtual disk}
   constructor Create(AImage:TVirtualDiskImage;AExtent:TVirtualDiskExtent); //To Do //Pass ALock:TMutexHandle to allow sharing
   destructor Destroy; override;
  private
   {Private Variables}

   {Private Methods}
  protected
   {Parent Objects}
   FImage:TVirtualDiskImage;
   FExtent:TVirtualDiskExtent;

   {Protected Variables}
   FTableOffset:Int64;      {Absolute byte offset of Table in file}
   FTableSize:LongWord;     {Size of Table data in file}

   FStartSector:Int64;      {First Sector represented by this Table}
   FSectorCount:Int64;      {Number of Sectors represented by this Table}

   {Protected Methods}
  public
   {Public Properties}
   property Image:TVirtualDiskImage read FImage;
   property Extent:TVirtualDiskExtent read FExtent;

   property TableOffset:Int64 read FTableOffset write FTableOffset;
   property TableSize:LongWord read FTableSize write FTableSize;

   property StartSector:Int64 read FStartSector write FStartSector;
   property SectorCount:Int64 read FSectorCount write FSectorCount;

   {Public Variables}

   {Public Methods}
 end;

 TVirtualDiskVmwareTable = class(TVirtualDiskTable)
  private
   {Private Variables}

   {Private Methods}
  protected
   {Parent Objects}

   {Protected Variables}

   {Protected Methods}
  public
   {Public Properties}

   {Public Variables}

   {Public Methods}
 end;

 TVirtualDiskVpcTable = class(TVirtualDiskTable)
   constructor Create(AImage:TVirtualDiskImage;AExtent:TVirtualDiskExtent);
   destructor Destroy; override;
  private
   {Private Variables}
   FData:Pointer;

   {Private Methods}
  protected
   {Parent Objects}

   {Protected Variables}

   {Protected Methods}
  public
   {Public Properties}
   property Data:Pointer read FData write FData;

   {Public Variables}

   {Public Methods}
 end;

 TVirtualDiskVboxTable = class(TVirtualDiskTable)
   constructor Create(AImage:TVirtualDiskImage;AExtent:TVirtualDiskExtent);
   destructor Destroy; override;
  private
   {Private Variables}
   FData:Pointer;

   {Private Methods}
  protected
   {Parent Objects}

   {Protected Variables}

   {Protected Methods}
  public
   {Public Properties}
   property Data:Pointer read FData write FData;

   {Public Variables}

   {Public Methods}
 end;

 TVirtualDiskBlock = class(TListObject) {Represents a Block bitmap showing data allocation within a virtual disk}
   constructor Create(AImage:TVirtualDiskImage;ATable:TVirtualDiskTable); //To Do //Pass ALock:TMutexHandle to allow sharing
   destructor Destroy; override;
  private
   {Private Variables}

   {Private Methods}
  protected
   {Parent Objects}
   FImage:TVirtualDiskImage;
   FTable:TVirtualDiskTable;

   {Protected Variables}
   FDataOffset:Int64;       {Absolute byte offset of Data in file (Where Applicable)}
   FBlockOffset:Int64;      {Absolute byte offset of Block in file}
   FBlockSize:LongWord;     {Size of Block bitmap in file}

   FStartSector:Int64;      {First Sector represented by this Block}
   FSectorCount:Int64;      {Number of Sectors represented by this Block}

   {Protected Methods}
  public
   {Public Properties}
   property Image:TVirtualDiskImage read FImage;
   property Table:TVirtualDiskTable read FTable;

   property DataOffset:Int64 read FDataOffset write FDataOffset;
   property BlockOffset:Int64 read FBlockOffset write FBlockOffset;
   property BlockSize:LongWord read FBlockSize write FBlockSize;

   property StartSector:Int64 read FStartSector write FStartSector;
   property SectorCount:Int64 read FSectorCount write FSectorCount;

   {Public Variables}

   {Public Methods}
 end;

 TVirtualDiskVmwareBlock = class(TVirtualDiskBlock)
  private
   {Private Variables}

   {Private Methods}
  protected
   {Parent Objects}

   {Protected Variables}

   {Protected Methods}
  public
   {Public Properties}

   {Public Variables}

   {Public Methods}

 end;

 TVirtualDiskVpcGroup = class(TListObject) {Represents a Group of bitmap Blocks}
   constructor Create(AImage:TVirtualDiskImage;ATable:TVirtualDiskTable); //To Do //Pass ALock:TMutexHandle to allow sharing
   destructor Destroy; override;
  private
   {Private Variables}
   FGroupNo:LongWord;

   {Private Methods}
  protected
   {Parent Objects}

   {Protected Variables}
   FBlocks:TFileSysList;     {Block bitmap Blocks}
   FBlockLocal:TMutexHandle; {Local Lock shared by all Block bitmap Blocks}

   {Protected Methods}
  public
   {Public Properties}
   property GroupNo:LongWord read FGroupNo write FGroupNo;

   property Blocks:TFileSysList read FBlocks;

   {Public Variables}

   {Public Methods}

 end;

 TVirtualDiskVpcBlock = class(TVirtualDiskBlock)
   constructor Create(AImage:TVirtualDiskImage;ATable:TVirtualDiskTable);
   destructor Destroy; override;
  private
   {Private Variables}
   FData:Pointer;

   FBlockNo:LongWord;

   {Private Methods}
  protected
   {Parent Objects}

   {Protected Variables}

   {Protected Methods}
  public
   {Public Properties}
   property Data:Pointer read FData write FData;

   property BlockNo:LongWord read FBlockNo write FBlockNo;

   {Public Variables}

   {Public Methods}

 end;

 {Note: VirtualBox images do not contain a bitmap, allocation granularity is per block}
 
{==============================================================================}
{var}
 {Virtual Disk specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure VirtualInit;
procedure VirtualQuit;

{==============================================================================}
{Virtual Disk Functions}

{==============================================================================}
{Virtual Disk Helper Functions}
function VirtualDataToPointer(const AData;ASize:Integer;ASwap:Boolean):Pointer;
function VirtualPointerToData(APointer:Pointer;var AData;ASize:Integer;ASwap:Boolean):Boolean;

function VirtualDataToString(const AData;ASize:Integer;AUnicode:Boolean):String;
function VirtualStringToData(const AString:String;var AData;ASize:Integer;AUnicode:Boolean):Boolean;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Virtual Disk specific variables}
 VirtualInitialized:Boolean;

{==============================================================================}
{==============================================================================}
{TVirtualDiskController}
constructor TVirtualDiskController.Create(ADriver:TFileSysDriver);
begin
 {}
 inherited Create(ADriver);
 FDescription:=VIRTUAL_CONTROLLER_DESCRIPTION;
end;

{==============================================================================}

function TVirtualDiskController.ControllerInit:Boolean;
begin
 {}
 Result:=False;
 
 if not WriterLock then Exit;
 try
  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskController.ControllerInit');
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                        Name = ' + Name);
  {$ENDIF}

  if FDriver = nil then Exit;
 
  {Nothing}
  Result:=True;
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskController.LocateDevices:Boolean;
var
 Image:TDiskImage;
 Device:TDiskDevice;
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskController.LocateDevices');
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                        Name = ' + Name);
  {$ENDIF}

  if FDriver = nil then Exit;

  {Locate Image Devices}
  Image:=FDriver.GetImageByNext(nil,True,False,FILESYS_LOCK_WRITE);
  while Image <> nil do
   begin
    if Image.Controller = Self then
     begin
      Device:=FDriver.GetDeviceByImage(Image,False,FILESYS_LOCK_NONE); {Do not lock}
      if Device = nil then
       begin
        Device:=TVirtualDiskDevice.Create(FDriver,Self,Image,nil,FDriver.GetNextDeviceNo(Image.MediaType),Image.Name);
        Device.DeviceInit;
       end;
     end;
    
    Image:=FDriver.GetImageByNext(Image,True,True,FILESYS_LOCK_WRITE);
   end;
   
  Result:=True;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskController.Read(ADevice:TDiskDevice;ASector:LongWord;ACount:Word;var ABuffer):Boolean;
{Note: Caller must hold the device lock}
var
 Image:TDiskImage;
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if ADevice = nil then Exit;
  if ACount = 0 then Exit;

  {Check Readable}
  if not ADevice.Readable then Exit;
  
  {Get Image}
  Image:=FDriver.GetImageByDevice(ADevice,True,FILESYS_LOCK_AUTO);
  if Image = nil then Exit;
  
  {Read Image}
  Result:=Image.Read(ASector,ACount,ABuffer);
  
  {Unlock Image}
  if not(Image.WriterOwner) then Image.ReaderUnlock else Image.WriterUnlock;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskController.Write(ADevice:TDiskDevice;ASector:LongWord;ACount:Word;const ABuffer):Boolean;
{Note: Caller must hold the device lock}
var
 Image:TDiskImage;
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if ADevice = nil then Exit;
  if ACount = 0 then Exit;

  {Check Writeable}
  if not ADevice.Writeable then Exit;

  {Get Image}
  Image:=FDriver.GetImageByDevice(ADevice,True,FILESYS_LOCK_AUTO);
  if Image = nil then Exit;
  
  {Write Image}
  Result:=Image.Write(ASector,ACount,ABuffer);
  
  {Unlock Image}
  if not(Image.WriterOwner) then Image.ReaderUnlock else Image.WriterUnlock;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskController.Reset(ADevice:TDiskDevice):Boolean;
{Note: Caller must hold the device lock}
var
 Image:TDiskImage;
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if ADevice = nil then Exit;

  Image:=FDriver.GetImageByDevice(ADevice,True,FILESYS_LOCK_AUTO);
  if Image = nil then Exit;
 
  {Nothing}
  Result:=True;
  
  {Unlock Image}
  if not(Image.WriterOwner) then Image.ReaderUnlock else Image.WriterUnlock;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskController.LockMedia(ADevice:TDiskDevice):Boolean;
{Note: Caller must hold the device lock}
var
 Image:TDiskImage;
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if ADevice = nil then Exit;

  Image:=FDriver.GetImageByDevice(ADevice,True,FILESYS_LOCK_AUTO);
  if Image = nil then Exit;
 
  Result:=Image.LockMedia;
  
  {Unlock Image}
  if not(Image.WriterOwner) then Image.ReaderUnlock else Image.WriterUnlock;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskController.UnlockMedia(ADevice:TDiskDevice):Boolean;
{Note: Caller must hold the device lock}
var
 Image:TDiskImage;
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if ADevice = nil then Exit;

  Image:=FDriver.GetImageByDevice(ADevice,True,FILESYS_LOCK_AUTO);
  if Image = nil then Exit;
 
  Result:=Image.UnlockMedia;
  
  {Unlock Image}
  if not(Image.WriterOwner) then Image.ReaderUnlock else Image.WriterUnlock;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskController.EjectMedia(ADevice:TDiskDevice):Boolean;
{Note: Caller must hold the device lock}
var
 Image:TDiskImage;
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if ADevice = nil then Exit;

  Image:=FDriver.GetImageByDevice(ADevice,True,FILESYS_LOCK_AUTO);
  if Image = nil then Exit;
 
  Result:=Image.EjectMedia;
  
  {Unlock Image}
  if not(Image.WriterOwner) then Image.ReaderUnlock else Image.WriterUnlock;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskController.MediaReady(ADevice:TDiskDevice):Boolean;
{Note: Caller must hold the device lock}
var
 Image:TDiskImage;
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if ADevice = nil then Exit;

  Image:=FDriver.GetImageByDevice(ADevice,True,FILESYS_LOCK_AUTO);
  if Image = nil then Exit;
 
  Result:=Image.MediaReady;
  
  {Unlock Image}
  if not(Image.WriterOwner) then Image.ReaderUnlock else Image.WriterUnlock;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskController.MediaChanged(ADevice:TDiskDevice):Boolean;
{Note: Caller must hold the device lock}
var
 Image:TDiskImage;
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if ADevice = nil then Exit;

  Image:=FDriver.GetImageByDevice(ADevice,True,FILESYS_LOCK_AUTO);
  if Image = nil then Exit;
 
  Result:=Image.MediaChanged;
  
  {Unlock Image}
  if not(Image.WriterOwner) then Image.ReaderUnlock else Image.WriterUnlock;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskController.MediaLocked(ADevice:TDiskDevice):Boolean;
{Note: Caller must hold the device lock}
var
 Image:TDiskImage;
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if ADevice = nil then Exit;

  Image:=FDriver.GetImageByDevice(ADevice,True,FILESYS_LOCK_AUTO);
  if Image = nil then Exit;
 
  Result:=Image.MediaLocked;
  
  {Unlock Image}
  if not(Image.WriterOwner) then Image.ReaderUnlock else Image.WriterUnlock;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskController.Information(ADevice:TDiskDevice):String;
{Note: Caller must hold the device lock}
var
 Image:TDiskImage;
begin
 {}
 Result:='';
 
 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if ADevice = nil then Exit;

  Image:=FDriver.GetImageByDevice(ADevice,True,FILESYS_LOCK_AUTO);
  if Image = nil then Exit;
 
  Result:=VIRTUAL_DEVICE_DESCRIPTION + ' (' + ImageTypeToString(Image.ImageType) + ')';
  
  {Unlock Image}
  if not(Image.WriterOwner) then Image.ReaderUnlock else Image.WriterUnlock;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskController.LBA(ADevice:TDiskDevice):Boolean;
{Note: Caller must hold the device lock}
var
 Image:TDiskImage;
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if ADevice = nil then Exit;

  Image:=FDriver.GetImageByDevice(ADevice,True,FILESYS_LOCK_AUTO);
  if Image = nil then Exit;
 
  {Nothing (Always support LBA)}
  Result:=True;
  
  {Unlock Image}
  if not(Image.WriterOwner) then Image.ReaderUnlock else Image.WriterUnlock;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskController.MediaType(ADevice:TDiskDevice):TMediaType;
{Note: Caller must hold the device lock}
var
 Image:TDiskImage;
begin
 {}
 Result:=mtUNKNOWN;
 
 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if ADevice = nil then Exit;

  Image:=FDriver.GetImageByDevice(ADevice,True,FILESYS_LOCK_AUTO);
  if Image = nil then Exit;
 
  Result:=Image.MediaType;
  
  {Unlock Image}
  if not(Image.WriterOwner) then Image.ReaderUnlock else Image.WriterUnlock;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskController.FloppyType(ADevice:TDiskDevice):TFloppyType;
{Note: Caller must hold the device lock}
var
 Image:TDiskImage;
begin
 {}
 Result:=ftUNKNOWN;
 
 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if ADevice = nil then Exit;

  Image:=FDriver.GetImageByDevice(ADevice,True,FILESYS_LOCK_AUTO);
  if Image = nil then Exit;
 
  Result:=Image.FloppyType;
  
  {Unlock Image}
  if not(Image.WriterOwner) then Image.ReaderUnlock else Image.WriterUnlock;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskController.Ready(ADevice:TDiskDevice):Boolean;
{Note: Caller must hold the device lock}
var
 Image:TDiskImage;
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if ADevice = nil then Exit;

  Image:=FDriver.GetImageByDevice(ADevice,True,FILESYS_LOCK_AUTO);
  if Image = nil then Exit;
 
  Result:=Image.Ready;
  
  {Unlock Image}
  if not(Image.WriterOwner) then Image.ReaderUnlock else Image.WriterUnlock;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskController.Locked(ADevice:TDiskDevice):Boolean;
{Note: Caller must hold the device lock}
var
 Image:TDiskImage;
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if ADevice = nil then Exit;

  Image:=FDriver.GetImageByDevice(ADevice,True,FILESYS_LOCK_AUTO);
  if Image = nil then Exit;
 
  Result:=Image.Locked;
  
  {Unlock Image}
  if not(Image.WriterOwner) then Image.ReaderUnlock else Image.WriterUnlock;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskController.Lockable(ADevice:TDiskDevice):Boolean;
{Note: Caller must hold the device lock}
begin
 {Check for Removable}
 Result:=Removable(ADevice);
end;

{==============================================================================}

function TVirtualDiskController.Readable(ADevice:TDiskDevice):Boolean;
{Note: Caller must hold the device lock}
var
 Image:TDiskImage;
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if ADevice = nil then Exit;

  Image:=FDriver.GetImageByDevice(ADevice,True,FILESYS_LOCK_AUTO);
  if Image = nil then Exit;
 
  Result:=Image.Readable;
  
  {Unlock Image}
  if not(Image.WriterOwner) then Image.ReaderUnlock else Image.WriterUnlock;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskController.Writeable(ADevice:TDiskDevice):Boolean;
{Note: Caller must hold the device lock}
var
 Image:TDiskImage;
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if ADevice = nil then Exit;

  Image:=FDriver.GetImageByDevice(ADevice,True,FILESYS_LOCK_AUTO);
  if Image = nil then Exit;
 
  Result:=Image.Writeable;
  
  {Unlock Image}
  if not(Image.WriterOwner) then Image.ReaderUnlock else Image.WriterUnlock;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskController.Removable(ADevice:TDiskDevice):Boolean;
{Note: Caller must hold the device lock}
var
 Image:TDiskImage;
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if ADevice = nil then Exit;

  Image:=FDriver.GetImageByDevice(ADevice,True,FILESYS_LOCK_AUTO);
  if Image = nil then Exit;
 
  case Image.MediaType of
   mtFLOPPY,mtREMOVABLE,mtCDROM,mtDVD,mtOTHER:begin
     Result:=True;
    end;
  end;
  
  {Unlock Image}
  if not(Image.WriterOwner) then Image.ReaderUnlock else Image.WriterUnlock;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskController.ChangeLine(ADevice:TDiskDevice):Boolean;
{Note: Caller must hold the device lock}
var
 Image:TDiskImage;
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if ADevice = nil then Exit;

  Image:=FDriver.GetImageByDevice(ADevice,True,FILESYS_LOCK_AUTO);
  if Image = nil then Exit;
  
  case Image.MediaType of
   mtFLOPPY,mtREMOVABLE,mtCDROM,mtDVD,mtOTHER:begin
     Result:=True;
    end;
  end;
  
  {Unlock Image}
  if not(Image.WriterOwner) then Image.ReaderUnlock else Image.WriterUnlock;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskController.PhysicalCylinders(ADevice:TDiskDevice):LongWord;
{Note: Caller must hold the device lock}
var
 Image:TDiskImage;
begin
 {}
 Result:=0;
 
 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if ADevice = nil then Exit;

  Image:=FDriver.GetImageByDevice(ADevice,True,FILESYS_LOCK_AUTO);
  if Image = nil then Exit;
 
  Result:=Image.Cylinders;
  
  {Unlock Image}
  if not(Image.WriterOwner) then Image.ReaderUnlock else Image.WriterUnlock;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskController.PhysicalHeads(ADevice:TDiskDevice):LongWord;
{Note: Caller must hold the device lock}
var
 Image:TDiskImage;
begin
 {}
 Result:=0;
 
 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if ADevice = nil then Exit;

  Image:=FDriver.GetImageByDevice(ADevice,True,FILESYS_LOCK_AUTO);
  if Image = nil then Exit;
 
  Result:=Image.Heads;
  
  {Unlock Image}
  if not(Image.WriterOwner) then Image.ReaderUnlock else Image.WriterUnlock;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskController.PhysicalSectors(ADevice:TDiskDevice):LongWord;
{Note: Caller must hold the device lock}
var
 Image:TDiskImage;
begin
 {}
 Result:=0;
 
 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if ADevice = nil then Exit;

  Image:=FDriver.GetImageByDevice(ADevice,True,FILESYS_LOCK_AUTO);
  if Image = nil then Exit;
 
  Result:=Image.Sectors;
  
  {Unlock Image}
  if not(Image.WriterOwner) then Image.ReaderUnlock else Image.WriterUnlock;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskController.LogicalCylinders(ADevice:TDiskDevice):LongWord;
{Note: Caller must hold the device lock}
var
 Image:TDiskImage;
begin
 {}
 Result:=0;
 
 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if ADevice = nil then Exit;

  Image:=FDriver.GetImageByDevice(ADevice,True,FILESYS_LOCK_AUTO);
  if Image = nil then Exit;
 
  Result:=((Image.Cylinders shr Image.LogicalShiftCount) and $FFFFFFFE); {Round to even multiple}
  
  {Unlock Image}
  if not(Image.WriterOwner) then Image.ReaderUnlock else Image.WriterUnlock;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskController.LogicalHeads(ADevice:TDiskDevice):LongWord;
{Note: Caller must hold the device lock}
var
 Image:TDiskImage;
begin
 {}
 Result:=0;
 
 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if ADevice = nil then Exit;

  Image:=FDriver.GetImageByDevice(ADevice,True,FILESYS_LOCK_AUTO);
  if Image = nil then Exit;
 
  Result:=(Image.Heads shl Image.LogicalShiftCount);
  
  {Unlock Image}
  if not(Image.WriterOwner) then Image.ReaderUnlock else Image.WriterUnlock;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskController.LogicalSectors(ADevice:TDiskDevice):LongWord;
{Note: Caller must hold the device lock}
var
 Image:TDiskImage;
begin
 {}
 Result:=0;
 
 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if ADevice = nil then Exit;

  Image:=FDriver.GetImageByDevice(ADevice,True,FILESYS_LOCK_AUTO);
  if Image = nil then Exit;
 
  Result:=Image.Sectors;
  
  {Unlock Image}
  if not(Image.WriterOwner) then Image.ReaderUnlock else Image.WriterUnlock;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskController.SectorSize(ADevice:TDiskDevice):Word;
{Note: Caller must hold the device lock}
var
 Image:TDiskImage;
begin
 {}
 Result:=0;
 
 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if ADevice = nil then Exit;

  Image:=FDriver.GetImageByDevice(ADevice,True,FILESYS_LOCK_AUTO);
  if Image = nil then Exit;
 
  Result:=Image.SectorSize;
  
  {Unlock Image}
  if not(Image.WriterOwner) then Image.ReaderUnlock else Image.WriterUnlock;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskController.SectorCount(ADevice:TDiskDevice):Int64;
{Note: Caller must hold the device lock}
var
 Image:TDiskImage;
begin
 {}
 Result:=0;
 
 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if ADevice = nil then Exit;

  Image:=FDriver.GetImageByDevice(ADevice,True,FILESYS_LOCK_AUTO);
  if Image = nil then Exit;
 
  Result:=Image.SectorCount;
  
  {Unlock Image}
  if not(Image.WriterOwner) then Image.ReaderUnlock else Image.WriterUnlock;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{TVirtualDiskDevice}
function TVirtualDiskDevice.DeviceInit:Boolean;
begin
 {}
 Result:=False;

 if not WriterLock then Exit;
 try
  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskDevice.DeviceInit');
  if FILESYS_LOG_ENABLED then FileSysLogDebug('             Name = ' + Name);
  {$ENDIF}

  if FDriver = nil then Exit;
  if FController = nil then Exit;
  if FDeviceNo = -1 then Exit;
  if FImage = nil then Exit;

  if (FImage.Attributes and iaDisk) = iaDisk then
   begin
    Result:=inherited DeviceInit;
   end
  else if (FImage.Attributes and iaPartition) = iaPartition then
   begin
    Result:=inherited DeviceInit;
   end
  else if (FImage.Attributes and iaVolume) = iaVolume then
   begin
    Result:=inherited DeviceInit;
   end
  else if (FImage.Attributes and iaDrive) = iaDrive then
   begin
    Result:=inherited DeviceInit;
   end;
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskDevice.LocatePartitions:Boolean;
var
 Partition:TDiskPartition;
begin
 {}
 Result:=False;

 if not WriterLock then Exit;
 try
  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskDevice.LocatePartitions');
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                    Name = ' + Name);
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                    DeviceNo = ' + IntToHex(DeviceNo,2));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                    Identifier = ' + Identifier);
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                    Information = ' + Information);
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                    MediaType = ' + MediaTypeToString(MediaType));
  {$ENDIF}

  if FDriver = nil then Exit;
  if FController = nil then Exit;
  if FDeviceNo = -1 then Exit;
  if FImage = nil then Exit;
  if FSectorCount = 0 then Exit;

  if (FImage.Attributes and iaDisk) = iaDisk then
   begin
    Result:=inherited LocatePartitions;
   end
  else if (FImage.Attributes and iaPartition) = iaPartition then
   begin
    case FMediaType of
     mtFIXED:begin {Not mtREMOVABLE}
       {Create a Partition}
       Partition:=FDriver.GetPartitionByEntryNo(Self,nil,MIN_PARTITION,False,FILESYS_LOCK_NONE); {Do not lock}
       if Partition = nil then
        begin
         {Partition does not already Exist}
         Partition:=TVirtualDiskPartition.Create(FDriver,Self,nil,FDriver.GetNextPartitionNo(Self,False));
         Partition.EntryNo:=MIN_PARTITION;
         Partition.PartitionInit;
         
         {Get the Recognizer for this Partition}
         FDriver.GetRecognizerByPartition(Partition,False,FILESYS_LOCK_NONE); {Do not lock}
         
         Result:=True;
        end;
      end;
     mtFLOPPY,mtREMOVABLE,mtCDROM,mtDVD,mtOTHER:begin
       Result:=True;
      end;
    end;
   end
  else if (FImage.Attributes and iaVolume) = iaVolume then
   begin
    case FMediaType of
     mtFIXED:begin {Not mtREMOVABLE}
       {Create a Partition}
       Partition:=FDriver.GetPartitionByEntryNo(Self,nil,MIN_PARTITION,False,FILESYS_LOCK_NONE); {Do not lock}
       if Partition = nil then
        begin
         {Partition does not already Exist}
         Partition:=TVirtualDiskPartition.Create(FDriver,Self,nil,FDriver.GetNextPartitionNo(Self,False));
         Partition.EntryNo:=MIN_PARTITION;
         Partition.PartitionInit;
         
         {Get the Recognizer for this Partition}
         FDriver.GetRecognizerByPartition(Partition,False,FILESYS_LOCK_NONE); {Do not lock}
         
         Result:=True;
        end;
      end;
     mtFLOPPY,mtREMOVABLE,mtCDROM,mtDVD,mtOTHER:begin
       Result:=True;
      end;
    end;
   end
  else if (FImage.Attributes and iaDrive) = iaDrive then
   begin
    case FMediaType of
     mtFIXED:begin {Not mtREMOVABLE}
       {Create a Partition}
       Partition:=FDriver.GetPartitionByEntryNo(Self,nil,MIN_PARTITION,False,FILESYS_LOCK_NONE); {Do not lock}
       if Partition = nil then
        begin
         {Partition does not already Exist}
         Partition:=TVirtualDiskPartition.Create(FDriver,Self,nil,FDriver.GetNextPartitionNo(Self,False));
         Partition.EntryNo:=MIN_PARTITION;
         Partition.PartitionInit;
         
         {Get the Recognizer for this Partition}
         FDriver.GetRecognizerByPartition(Partition,False,FILESYS_LOCK_NONE); {Do not lock}
         
         Result:=True;
        end;
      end;
     mtFLOPPY,mtREMOVABLE,mtCDROM,mtDVD,mtOTHER:begin
       Result:=True;
      end;
    end;
   end;
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskDevice.LocateVolumes:Boolean;
var
 Volume:TDiskVolume;
begin
 {}
 Result:=False;

 if not WriterLock then Exit;
 try
  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskDevice.LocateVolumes');
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                    Name = ' + Name);
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                    DeviceNo = ' + IntToHex(DeviceNo,2));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                    Identifier = ' + Identifier);
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                    Information = ' + Information);
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                    MediaType = ' + MediaTypeToString(MediaType));
  {$ENDIF}

  if FDriver = nil then Exit;
  if FController = nil then Exit;
  if FDeviceNo = -1 then Exit;
  if FImage = nil then Exit;
  {if FSectorCount = 0 then Exit;} {Zero is allowed}

  if (FImage.Attributes and iaDisk) = iaDisk then
   begin
    Result:=inherited LocateVolumes;
   end
  else if (FImage.Attributes and iaPartition) = iaPartition then
   begin
    Result:=True;
   end
  else if (FImage.Attributes and iaVolume) = iaVolume then
   begin
    case FMediaType of
     mtFLOPPY,mtREMOVABLE,mtCDROM,mtDVD,mtOTHER:begin
       {Create a Volume}
       Volume:=FDriver.GetVolumeByDevice(Self,False,FILESYS_LOCK_NONE); {Do not lock}
       if Volume = nil then
        begin
         Volume:=TDiskVolume.Create(FDriver,Self,nil,FDriver.GetNextVolumeNo);
         Volume.VolumeInit;
        end;
        
       Result:=True;
      end;
     mtFIXED:begin {Not mtREMOVABLE}
       Result:=True;
      end;
    end;
   end
  else if (FImage.Attributes and iaDrive) = iaDrive then
   begin
    case FMediaType of
     mtFLOPPY,mtREMOVABLE,mtCDROM,mtDVD,mtOTHER:begin
       {Create a Volume}
       Volume:=FDriver.GetVolumeByDevice(Self,False,FILESYS_LOCK_NONE); {Do not lock}
       if Volume = nil then
        begin
         Volume:=TDiskVolume.Create(FDriver,Self,nil,FDriver.GetNextVolumeNo);
         Volume.VolumeInit;
        end;
        
       Result:=True;
      end;
     mtFIXED:begin {Not mtREMOVABLE}
       Result:=True;
      end;
    end;
   end;
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskDevice.CreatePartition(AParent:TDiskPartition;APartitionId:Byte;ACount:LongWord;AActive:Boolean):Boolean;
begin
 {}
 Result:=False;
 
 if not WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FController = nil then Exit;
  if FDeviceNo = -1 then Exit;
  if FImage = nil then Exit;

  if (FImage.Attributes and iaDisk) = iaDisk then
   begin
    Result:=inherited CreatePartition(AParent,APartitionId,ACount,AActive);
   end;
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskDevice.DeletePartition(APartition:TDiskPartition):Boolean;
{Note: Caller must hold the partition writer lock}
begin
 {}
 Result:=False;
 
 if not WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FController = nil then Exit;
  if FDeviceNo = -1 then Exit;
  if FImage = nil then Exit;
  if APartition = nil then Exit;

  if (FImage.Attributes and iaDisk) = iaDisk then
   begin
    Result:=inherited DeletePartition(APartition);
   end;
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskDevice.ModifyPartition(APartition:TDiskPartition;APartitionId:Byte):Boolean;
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FController = nil then Exit;
  if FDeviceNo = -1 then Exit;
  if FImage = nil then Exit;
  if APartition = nil then Exit;

  if (FImage.Attributes and iaDisk) = iaDisk then
   begin
    Result:=inherited ModifyPartition(APartition,APartitionId);
   end;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskDevice.ActivatePartition(APartition:TDiskPartition;AActive:Boolean):Boolean;
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FController = nil then Exit;
  if FDeviceNo = -1 then Exit;
  if FImage = nil then Exit;
  if APartition = nil then Exit;

  if (FImage.Attributes and iaDisk) = iaDisk then
   begin
    Result:=inherited ActivatePartition(APartition,AActive);
   end;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskDevice.ShrinkPartition(APartition:TDiskPartition;const AStart,ASize:Int64):Boolean;
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FController = nil then Exit;
  if FDeviceNo = -1 then Exit;
  if FImage = nil then Exit;
  if APartition = nil then Exit;

  if (FImage.Attributes and iaDisk) = iaDisk then
   begin
    Result:=inherited ShrinkPartition(APartition,AStart,ASize);
   end;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskDevice.ExpandPartition(APartition:TDiskPartition;const AStart,ASize:Int64):Boolean;
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FController = nil then Exit;
  if FDeviceNo = -1 then Exit;
  if FImage = nil then Exit;
  if APartition = nil then Exit;

  if (FImage.Attributes and iaDisk) = iaDisk then
   begin
    Result:=inherited ExpandPartition(APartition,AStart,ASize);
   end;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{TVirtualDiskPartition}
function TVirtualDiskPartition.PartitionInit:Boolean;
begin
 {}
 Result:=False;

 if not WriterLock then Exit;
 try
  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskPartition.PartitionInit');
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                       Name = ' + Name);
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                       EntryNo = ' + IntToStr(EntryNo));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                       PartitionId = ' + IntToStr(PartitionId));
  {$ENDIF}

  if FDriver = nil then Exit;
  if FDevice = nil then Exit;
  if FEntryNo < MIN_PARTITION then Exit;
  if FEntryNo > MAX_PARTITION then Exit;
  if FDevice.Controller = nil then Exit;
  if FDevice.Image = nil then Exit;

  if (FDevice.Image.Attributes and iaDisk) = iaDisk then
   begin
    Result:=inherited PartitionInit;
   end
  else if (FDevice.Image.Attributes and iaPartition) = iaPartition then
   begin
    FPartitionId:=FDevice.Image.PartitionId;
    FBeginHead:=0;
    FBeginSector:=0;
    FBeginCylinder:=0;
    FEndHead:=0;
    FEndSector:=0;
    FEndCylinder:=0;
    FSectorOffset:=0;

    FStartSector:=0;
    FSectorCount:=FDevice.Image.SectorCount;
    
    Result:=True;
   end
  else if (FDevice.Image.Attributes and iaVolume) = iaVolume then
   begin
    FPartitionId:=FDevice.Image.PartitionId;
    FBeginHead:=0;
    FBeginSector:=0;
    FBeginCylinder:=0;
    FEndHead:=0;
    FEndSector:=0;
    FEndCylinder:=0;
    FSectorOffset:=0;

    FStartSector:=0;
    FSectorCount:=FDevice.Image.SectorCount;
    
    Result:=True;
   end
  else if (FDevice.Image.Attributes and iaDrive) = iaDrive then
   begin
    FPartitionId:=FDevice.Image.PartitionId;
    FBeginHead:=0;
    FBeginSector:=0;
    FBeginCylinder:=0;
    FEndHead:=0;
    FEndSector:=0;
    FEndCylinder:=0;
    FSectorOffset:=0;

    FStartSector:=0;
    FSectorCount:=FDevice.Image.SectorCount;
    
    Result:=True;
   end;
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskPartition.LocatePartitions:Boolean;
begin
 {}
 Result:=False;

 if not WriterLock then Exit;
 try
  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskPartition.LocatePartitions');
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                       Name = ' + Name);
  {$ENDIF}

  if FDriver = nil then Exit;
  if FDevice = nil then Exit;
  if FEntryNo < MIN_PARTITION then Exit;
  if FEntryNo > MAX_PARTITION then Exit;
  if FDevice.Controller = nil then Exit;
  if FDevice.Image = nil then Exit;

  if (FDevice.Image.Attributes and iaDisk) = iaDisk then
   begin
    Result:=inherited LocatePartitions;
   end
  else if (FDevice.Image.Attributes and iaPartition) = iaPartition then
   begin
    Result:=True;
   end
  else if (FDevice.Image.Attributes and iaVolume) = iaVolume then
   begin
    Result:=True;
   end
  else if (FDevice.Image.Attributes and iaDrive) = iaDrive then
   begin
    Result:=True;
   end;
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskPartition.LocateVolumes:Boolean;
begin
 {}
 Result:=False;

 if not WriterLock then Exit;
 try
  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskPartition.LocateVolumes');
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                       Name = ' + Name);
  {$ENDIF}

  if FDriver = nil then Exit;
  if FDevice = nil then Exit;
  if FEntryNo < MIN_PARTITION then Exit;
  if FEntryNo > MAX_PARTITION then Exit;
  if FDevice.Controller = nil then Exit;
  if FDevice.Image = nil then Exit;

  if (FDevice.Image.Attributes and iaDisk) = iaDisk then
   begin
    Result:=inherited LocateVolumes;
   end
  else if (FDevice.Image.Attributes and iaPartition) = iaPartition then
   begin
    Result:=inherited LocateVolumes;
   end
  else if (FDevice.Image.Attributes and iaVolume) = iaVolume then
   begin
    Result:=inherited LocateVolumes;
   end
  else if (FDevice.Image.Attributes and iaDrive) = iaDrive then
   begin
    Result:=inherited LocateVolumes;
   end;
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{TVirtualDiskImage}
constructor TVirtualDiskImage.Create(ADriver:TFileSysDriver;AController:TDiskController;const AName:String;AImageNo:Integer);
begin
 {}
 inherited Create(ADriver,AController,AName,AImageNo);
 FFlags:=virtualFlagNone;

 FExtents:=TFileSysList.Create;

 FBase:=nil;
 FCurrent:=nil;
end;

{==============================================================================}

destructor TVirtualDiskImage.Destroy;
begin
 {}
 WriterLock;
 try
  CloseExtents;
  FExtents.Free;
 
  FBase:=nil;
  FCurrent:=nil;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  inherited Destroy;
 end; 
end;

{==============================================================================}

function TVirtualDiskImage.GetCylinders:LongWord;
var
 WorkInt:Int64;
begin
 {}
 Result:=0;
 if not Ready then Exit;

 Result:=FCylinders;
 if FSectors = 0 then Exit;
 if FSectorCount = 0 then Exit;

 {Check Media Type}
 case FMediaType of
  mtFLOPPY,mtFIXED,mtREMOVABLE,mtCDROM,mtDVD,mtOTHER:begin
    {Check Floppy Type}
    if (FMediaType = mtFLOPPY) and (FFloppyType <> ftATAPI) then
     begin
      case FFloppyType of
       ft360K:begin
         {Setup Cylinders}
         Result:=40;
        end;
       ft12M:begin
         {Setup Cylinders}
         Result:=80;
        end;
       ft720K:begin
         {Setup Cylinders}
         Result:=80;
        end;
       ft144M:begin
         {Setup Cylinders}
         Result:=80;
        end;
       ft288M:begin
         {Setup Cylinders}
         Result:=80;
        end;
      end;
     end
    else
     begin
      {Setup Cylinders}
      WorkInt:=FSectorCount div FSectors;
      if (WorkInt mod 255 = 0) then
       begin
        WorkInt:=WorkInt div 255; {Assume 255 Heads}
        Result:=WorkInt;
       end
      else if (WorkInt mod 64 = 0) then
       begin
        WorkInt:=WorkInt div 64; {Assume 64 Heads}
        Result:=WorkInt;
       end
      else if (WorkInt mod 16 = 0) then
       begin
        WorkInt:=WorkInt div 16; {Assume 16 Heads}
        Result:=WorkInt;
       end
      else if (WorkInt mod 15 = 0) then
       begin
        WorkInt:=WorkInt div 15; {Assume 15 Heads}
        Result:=WorkInt;
       end
      else if (WorkInt mod 4 = 0) then
       begin
        WorkInt:=WorkInt div 4; {Assume 4 Heads}
        Result:=WorkInt;
       end
      else if (WorkInt mod 2 = 0) then
       begin
        WorkInt:=WorkInt div 2; {Assume 2 Heads}
        Result:=WorkInt;
       end;
     end;
   end;
 end;
end;

{==============================================================================}

function TVirtualDiskImage.GetHeads:LongWord;
var
 WorkInt:Int64;
begin
 {}
 Result:=0;
 if not Ready then Exit;

 Result:=FHeads;
 if FSectors = 0 then Exit;
 if FSectorCount = 0 then Exit;

 {Check Media Type}
 case FMediaType of
  mtFLOPPY,mtFIXED,mtREMOVABLE,mtCDROM,mtDVD,mtOTHER:begin
    {Check Floppy Type}
    if (FMediaType = mtFLOPPY) and (FFloppyType <> ftATAPI) then
     begin
      case FFloppyType of
       ft360K:begin
         {Setup Heads}
         Result:=2;
        end;
       ft12M:begin
         {Setup Heads}
         Result:=2;
        end;
       ft720K:begin
         {Setup Heads}
         Result:=2;
        end;
       ft144M:begin
         {Setup Heads}
         Result:=2;
        end;
       ft288M:begin
         {Setup Heads}
         Result:=2;
        end;
      end;
     end
    else
     begin
      {Setup Heads}
      WorkInt:=FSectorCount div FSectors;
      if (WorkInt mod 255 = 0) then
       begin
        Result:=255; {Assume 255 Heads}
       end
      else if (WorkInt mod 64 = 0) then
       begin
        Result:=64;  {Assume 64 Heads}
       end
      else if (WorkInt mod 16 = 0) then
       begin
        Result:=16;  {Assume 16 Heads}
       end
      else if (WorkInt mod 15 = 0) then
       begin
        Result:=15;  {Assume 15 Heads}
       end
      else if (WorkInt mod 4 = 0) then
       begin
        Result:=4;   {Assume 4 Heads}
       end
      else if (WorkInt mod 2 = 0) then
       begin
        Result:=2;   {Assume 2 Heads}
       end;
     end;
   end;
 end;
end;

{==============================================================================}

function TVirtualDiskImage.GetSectors:LongWord;
var
 WorkInt:Int64;
begin
 {}
 Result:=0;
 if not Ready then Exit;

 Result:=FSectors;
 if FSectorCount = 0 then Exit;

 {Check Media Type}
 case FMediaType of
  mtFLOPPY,mtFIXED,mtREMOVABLE,mtCDROM,mtDVD,mtOTHER:begin
    {Check Floppy Type}
    if (FMediaType = mtFLOPPY) and (FFloppyType <> ftATAPI) then
     begin
      case FFloppyType of
       ft360K:begin
         {Setup Sectors}
         Result:=9;
        end;
       ft12M:begin
         {Setup Sectors}
         Result:=15;
        end;
       ft720K:begin
         {Setup Sectors}
         Result:=9;
        end;
       ft144M:begin
         {Setup Sectors}
         Result:=18;
        end;
       ft288M:begin
         {Setup Sectors}
         Result:=36;
        end;
      end;
     end
    else
     begin
      {Setup Sectors}
      WorkInt:=FSectorCount;
      if (WorkInt mod 63 = 0) then
       begin
        Result:=63; {Assume 63 Sectors}
       end
      else if (WorkInt mod 32 = 0) then
       begin
        Result:=32; {Assume 32 Sectors}
       end
      else if (WorkInt mod 17 = 0) then
       begin
        Result:=17; {Assume 17 Sectors}
       end;
     end;
   end;
 end;
end;

{==============================================================================}

function TVirtualDiskImage.ReadExtent(AExtent:TVirtualDiskExtent;ASector:LongWord;ACount:Word;var ABuffer):Word;
begin
 {Virtual Base Method - No Function}
 Result:=0;
end;

{==============================================================================}

function TVirtualDiskImage.WriteExtent(AExtent:TVirtualDiskExtent;ASector:LongWord;ACount:Word;const ABuffer):Word;
begin
 {Virtual Base Method - No Function}
 Result:=0;
end;

{==============================================================================}

function TVirtualDiskImage.LoadExtents:Boolean;
begin
 {Virtual Base Method - No Function}
 Result:=False;
end;

{==============================================================================}

function TVirtualDiskImage.CloseExtents:Boolean;
var
 Extent:TVirtualDiskExtent;
begin
 {Base Implementation}
 Result:=False;

 if not FExtents.WriterLock then Exit;
 try
  if FDriver = nil then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskImage.CloseExtents');
  {$ENDIF}

  {Close Extents}
  Extent:=TVirtualDiskExtent(FExtents.First);
  while Extent <> nil do
   begin
    if Extent.Handle <> INVALID_HANDLE_VALUE then FDriver.FileClose(Extent.Handle);
    Extent.Handle:=INVALID_HANDLE_VALUE;
    if Extent = FBase then FBase:=nil;
    if Extent = FCurrent then FCurrent:=nil;
    
    Extent:=TVirtualDiskExtent(Extent.Next);
   end;

  {Free Extents}
  FExtents.ClearList;

  Result:=True;
 finally  
  FExtents.WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskImage.CheckExtent(ADelta,AParent:TVirtualDiskExtent;const AFilename:String):Boolean;
begin
 {Virtual Base Method - No Function}
 Result:=False;
end;

{==============================================================================}

function TVirtualDiskImage.LoadExtent(ADelta,AParent:TVirtualDiskExtent;const AFilename:String):TVirtualDiskExtent;
begin
 {Virtual Base Method - No Function}
 Result:=nil;
end;

{==============================================================================}

function TVirtualDiskImage.AddExtent(AParent:TVirtualDiskExtent;const AFilename:String):TVirtualDiskExtent;
begin
 {Virtual Base Method - No Function}
 Result:=nil;
end;

{==============================================================================}

function TVirtualDiskImage.RemoveExtent(AExtent:TVirtualDiskExtent):Boolean;
begin
 {Virtual Base Method - No Function}
 Result:=False;
end;

{==============================================================================}

function TVirtualDiskImage.SetExtent(AExtent:TVirtualDiskExtent):Boolean;
begin
 {Virtual Base Method - No Function}
 Result:=False;
end;

{==============================================================================}

function TVirtualDiskImage.GetExtent(const ASector:Int64;AWrite,ALock:Boolean):TVirtualDiskExtent;
begin
 {Virtual Base Method - No Function}
 Result:=nil;
end;

{==============================================================================}

function TVirtualDiskImage.FindExtent(const AFilename:String;ALock:Boolean):TVirtualDiskExtent;
var
 Extent:TVirtualDiskExtent;
begin
 {Base Implementation}
 Result:=nil;
 
 if not FExtents.ReaderLock then Exit;
 try
  {Check Extents}
  Extent:=TVirtualDiskExtent(FExtents.First);
  while Extent <> nil do
   begin
    if Uppercase(Extent.Filename) = Uppercase(AFilename) then
     begin
      if ALock then Extent.AcquireLock;
      
      Result:=Extent;
      Exit;
     end;
     
    Extent:=TVirtualDiskExtent(Extent.Next);
   end;
 finally  
  FExtents.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskImage.LoadTables:Boolean;
begin
 {Virtual Base Method - No Function}
 Result:=False;
end;

{==============================================================================}

function TVirtualDiskImage.LoadTable(AExtent:TVirtualDiskExtent;ATableNo:LongWord):TVirtualDiskTable;
begin
 {Virtual Base Method - No Function}
 Result:=nil;
end;

{==============================================================================}

function TVirtualDiskImage.AddTable(AExtent:TVirtualDiskExtent):TVirtualDiskTable;
begin
 {Virtual Base Method - No Function}
 Result:=nil;
end;

{==============================================================================}

function TVirtualDiskImage.RemoveTable(ATable:TVirtualDiskTable):Boolean;
begin
 {Virtual Base Method - No Function}
 Result:=False;
end;

{==============================================================================}

function TVirtualDiskImage.SetTable(ATable:TVirtualDiskTable):Boolean;
begin
 {Virtual Base Method - No Function}
 Result:=False;
end;

{==============================================================================}

function TVirtualDiskImage.GetTable(AExtent:TVirtualDiskExtent;const ASector:Int64;AWrite:Boolean):TVirtualDiskTable;
begin
 {Virtual Base Method - No Function}
 Result:=nil;
end;

{==============================================================================}

function TVirtualDiskImage.LoadBlocks(ATable:TVirtualDiskTable):Boolean;
begin
 {Virtual Base Method - No Function}
 Result:=False;
end;

{==============================================================================}

function TVirtualDiskImage.LoadBlock(ATable:TVirtualDiskTable;ABlockNo:LongWord):TVirtualDiskBlock;
begin
 {Virtual Base Method - No Function}
 Result:=nil;
end;

{==============================================================================}

function TVirtualDiskImage.AddBlock(ATable:TVirtualDiskTable):TVirtualDiskBlock;
begin
 {Virtual Base Method - No Function}
 Result:=nil;
end;

{==============================================================================}

function TVirtualDiskImage.RemoveBlock(ABlock:TVirtualDiskBlock):Boolean;
begin
 {Virtual Base Method - No Function}
 Result:=False;
end;

{==============================================================================}

function TVirtualDiskImage.SetBlock(ABlock:TVirtualDiskBlock):Boolean;
begin
 {Virtual Base Method - No Function}
 Result:=False;
end;

{==============================================================================}

function TVirtualDiskImage.GetBlock(ATable:TVirtualDiskTable;const ASector:Int64;AWrite:Boolean):TVirtualDiskBlock;
begin
 {Virtual Base Method - No Function}
 Result:=nil;
end;

{==============================================================================}

function TVirtualDiskImage.LocateDelta(AExtent:TVirtualDiskExtent):String;
begin
 {Virtual Base Method - No Function}
 Result:='';
end;

{==============================================================================}

function TVirtualDiskImage.LocateParent(AExtent:TVirtualDiskExtent):String;
begin
 {Virtual Base Method - No Function}
 Result:='';
end;

{==============================================================================}

function TVirtualDiskImage.LocateSibling(AExtent:TVirtualDiskExtent):String;
begin
 {Virtual Base Method - No Function}
 Result:='';
end;

{==============================================================================}

function TVirtualDiskImage.IsSplit:Boolean;
begin
 {Base Implementation}
 Result:=(FFlags and virtualFlagSplit) = virtualFlagSplit;
end;

{==============================================================================}
{==============================================================================}
{TVirtualDiskMemoryImage}
constructor TVirtualDiskMemoryImage.Create(ADriver:TFileSysDriver;AController:TDiskController;const AName:String;AImageNo:Integer);
begin
 {}
 inherited Create(ADriver,AController,AName,AImageNo);
 FName:=AName + IntToStr(AImageNo); {Create a Unique Name}
 FAttributes:=iaDisk or iaReadable or iaWriteable;

 FImageType:=itMEMORY;
 FMediaType:=mtFIXED;
 FFloppyType:=ftUNKNOWN;

 FCylinders:=0;
 FHeads:=0;
 FSectors:=0;
 FLogicalShiftCount:=0;

 FSectorSize:=MIN_SECTOR_SIZE;
 FSectorCount:=0;
 FSectorShiftCount:=0;

 FData:=nil;
end;

{==============================================================================}

destructor TVirtualDiskMemoryImage.Destroy;
begin
 {}
 WriterLock;
 try
  if FData <> nil then FreeMem(FData);
  FData:=nil;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  inherited Destroy;
 end; 
end;

{==============================================================================}

function TVirtualDiskMemoryImage.GetReady:Boolean;
begin
 {}
 Result:=(FData <> nil);
end;

{==============================================================================}

function TVirtualDiskMemoryImage.GetSectorSize:Word;
begin
 {}
 Result:=0;
 if not Ready then Exit;

 Result:=FSectorSize;
 if FSectorSize > 0 then Exit;
 Result:=MIN_SECTOR_SIZE;
end;

{==============================================================================}

function TVirtualDiskMemoryImage.GetSectorCount:Int64;
begin
 {}
 Result:=0;
 if not Ready then Exit;

 Result:=FSectorCount;
end;

{==============================================================================}

function TVirtualDiskMemoryImage.GetPartitionId:Byte;
begin
 {}
 Result:=pidUnused;
 if not Ready then Exit;

 Result:=FPartitionId;
end;

{==============================================================================}

procedure TVirtualDiskMemoryImage.SetName(const AName:String);
begin
 {}
 if GetReady then Exit;
 FName:=AName + IntToStr(FImageNo); {Create a Unique Name}
end;

{==============================================================================}

function TVirtualDiskMemoryImage.Read(ASector:LongWord;ACount:Word;var ABuffer):Boolean;
{Note: Caller must hold the image lock}
var
 Start:Int64;
 Count:Integer;
begin
 {}
 Result:=False;

 {$IFDEF VIRTUAL_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskMemoryImage.Read');
 if FILESYS_LOG_ENABLED then FileSysLogDebug('                         Sector = ' + IntToStr(ASector));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('                         Count = ' + IntToStr(ACount));
 {$ENDIF}
 
 if FDriver = nil then Exit;
 if FController = nil then Exit;
 
 {Check Open}
 if FData = nil then Exit;
 
 {Check Read}
 if ASector >= FSectorCount then Exit;
 if (ASector + ACount) > FSectorCount then Exit;
 
 {Setup Read}
 Start:=ASector; {Allow for Int64 Result}
 Count:=ACount;
 Start:=(Start shl FSectorShiftCount);
 Count:=(Count shl FSectorShiftCount);
 
 {Perform Read}
 System.Move(Pointer(PtrUInt(FData) + PtrUInt(Start))^,ABuffer,Count);
 
 Result:=True;
end;

{==============================================================================}

function TVirtualDiskMemoryImage.Write(ASector:LongWord;ACount:Word;const ABuffer):Boolean;
{Note: Caller must hold the image lock}
var
 Start:Int64;
 Count:Integer;
begin
 {}
 Result:=False;

 {$IFDEF VIRTUAL_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskMemoryImage.Write');
 if FILESYS_LOG_ENABLED then FileSysLogDebug('                         Sector = ' + IntToStr(ASector));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('                         Count = ' + IntToStr(ACount));
 {$ENDIF}
 
 if FDriver = nil then Exit;
 if FController = nil then Exit;
 
 {Check Open}
 if FData = nil then Exit;
 
 {Check Write}
 if ASector >= FSectorCount then Exit;
 if (ASector + ACount) > FSectorCount then Exit;
 
 {Setup Write}
 Start:=ASector; {Allow for Int64 Result}
 Count:=ACount;
 Start:=(Start shl FSectorShiftCount);
 Count:=(Count shl FSectorShiftCount);
 
 {Perform Write}
 System.Move(ABuffer,Pointer(PtrUInt(FData) + PtrUInt(Start))^,Count);
 
 Result:=True;
end;

{==============================================================================}

function TVirtualDiskMemoryImage.ConvertImage(AImageType:TImageType):Boolean;
begin
 {Not Supported}
 Result:=False;
end;

{==============================================================================}

function TVirtualDiskMemoryImage.CreateImage(AMediaType:TMediaType;AFloppyType:TFloppyType;AAttributes:LongWord;ASectorSize:Word;const ASectorCount:Int64;ACylinders,AHeads,ASectors:LongWord;APartitionId:Byte):Integer;
{Note: Create only on Memory Image, Cannot Open}
var
 Size:Int64;
begin
 {}
 Result:=0;
 
 if not WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FController = nil then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskMemoryImage.CreateImage - Name = ' + FName);
  {$ENDIF}

  {Check Open}
  if FData <> nil then Exit;

  {Setup Parameters}
  if (AMediaType <> mtUNKNOWN) and (AMediaType <> mtINVALID) then FMediaType:=AMediaType;
  if (AFloppyType <> ftUNKNOWN) and (AFloppyType <> ftINVALID) then FFloppyType:=AFloppyType;
  if AAttributes <> iaNone then FAttributes:=AAttributes;
  if ASectorSize > 0 then FSectorSize:=ASectorSize;
  if ASectorCount > 0 then FSectorCount:=ASectorCount;
  if ACylinders > 0 then FCylinders:=ACylinders;
  if AHeads > 0 then FHeads:=AHeads;
  if ASectors > 0 then FSectors:=ASectors;
  if APartitionId <> pidUnused then FPartitionId:=APartitionId;

  {Check Parameters}
  if FSectorSize = 0 then Exit;
  if FSectorCount = 0 then Exit;
  FSectorShiftCount:=GetSectorShiftCount;

  {Allocate Memory}
  Size:=(FSectorCount shl FSectorShiftCount);
  FData:=GetMem(Size);
  if FData = nil then Exit;

  Result:=FImageNo;
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskMemoryImage.CloseImage:Boolean;
begin
 {}
 Result:=False;
 
 if not WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FController = nil then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskMemoryImage.CloseImage - Name = ' + FName);
  {$ENDIF}

  {Check Open}
  if FData = nil then Exit;

  {Free Memory}
  FreeMem(FData);
  FData:=nil;

  Result:=True;
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{TVirtualDiskFileImage}
constructor TVirtualDiskFileImage.Create(ADriver:TFileSysDriver;AController:TDiskController;const AName:String;AImageNo:Integer);
begin
 {}
 inherited Create(ADriver,AController,AName,AImageNo);
 FAttributes:=iaDisk or iaReadable or iaWriteable;

 FImageType:=itFILE;
 FMediaType:=mtFIXED;
 FFloppyType:=ftUNKNOWN;

 FCylinders:=0;
 FHeads:=0;
 FSectors:=0;
 FLogicalShiftCount:=0;

 FSectorSize:=MIN_SECTOR_SIZE;
 FSectorCount:=0;
 FSectorShiftCount:=0;

 FHandle:=INVALID_HANDLE_VALUE;
end;

{==============================================================================}

destructor TVirtualDiskFileImage.Destroy;
begin
 {}
 WriterLock;
 try
  if FHandle <> INVALID_HANDLE_VALUE then FDriver.FileClose(FHandle);
  FHandle:=INVALID_HANDLE_VALUE;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  inherited Destroy;
 end; 
end;

{==============================================================================}

function TVirtualDiskFileImage.GetReady:Boolean;
begin
 {}
 Result:=(FHandle <> INVALID_HANDLE_VALUE);
end;

{==============================================================================}

function TVirtualDiskFileImage.GetSectorSize:Word;
begin
 {}
 Result:=0;
 if not Ready then Exit;

 Result:=FSectorSize;
 if FSectorSize > 0 then Exit;
 Result:=MIN_SECTOR_SIZE;
end;

{==============================================================================}

function TVirtualDiskFileImage.GetSectorCount:Int64;
var
 Size:Int64;
begin
 {}
 Result:=0;
 if not Ready then Exit;

 Result:=FSectorCount;
 if FSectorCount > 0 then Exit;
 Size:=FDriver.FileSizeEx(FHandle);
 if FSectorSize = 0 then Exit;
 Result:=(Size div FSectorSize);
end;

{==============================================================================}

function TVirtualDiskFileImage.GetPartitionId:Byte;
begin
 {}
 Result:=pidUnused;
 if not Ready then Exit;

 Result:=FPartitionId;
end;

{==============================================================================}

function TVirtualDiskFileImage.Read(ASector:LongWord;ACount:Word;var ABuffer):Boolean;
{Note: Caller must hold the image lock}
var
 Start:Int64;
 Count:Integer;
begin
 {}
 Result:=False;
 
 if FDriver = nil then Exit;
 if FController = nil then Exit;

 {Check Open}
 if FHandle = INVALID_HANDLE_VALUE then Exit;

 {Check Read}
 if ASector >= FSectorCount then Exit;
 if (ASector + ACount) > FSectorCount then Exit;

 {Setup Read}
 Start:=ASector; {Allow for Int64 Result}
 Count:=ACount;
 Start:=(Start shl FSectorShiftCount);
 Count:=(Count shl FSectorShiftCount);

 {Perform Read}
 FDriver.FileSeekEx(FHandle,Start,soFromBeginning);
 Result:=(FDriver.FileRead(FHandle,ABuffer,Count) = Count);
end;

{==============================================================================}

function TVirtualDiskFileImage.Write(ASector:LongWord;ACount:Word;const ABuffer):Boolean;
{Note: Caller must hold the image lock}
var
 Start:Int64;
 Count:Integer;
begin
 {}
 Result:=False;
 
 if FDriver = nil then Exit;
 if FController = nil then Exit;

 {Check Open}
 if FHandle = INVALID_HANDLE_VALUE then Exit;

 {Check Write}
 if ASector >= FSectorCount then Exit;
 if (ASector + ACount) > FSectorCount then Exit;

 {Setup Write}
 Start:=ASector; {Allow for Int64 Result}
 Count:=ACount;
 Start:=(Start shl FSectorShiftCount);
 Count:=(Count shl FSectorShiftCount);

 {Perform Write}
 FDriver.FileSeekEx(FHandle,Start,soFromBeginning);
 Result:=(FDriver.FileWrite(FHandle,ABuffer,Count) = Count);
end;

{==============================================================================}

function TVirtualDiskFileImage.CreateImage(AMediaType:TMediaType;AFloppyType:TFloppyType;AAttributes:LongWord;ASectorSize:Word;const ASectorCount:Int64;ACylinders,AHeads,ASectors:LongWord;APartitionId:Byte):Integer;
var
 Size:Int64;
begin
 {}
 Result:=0;
 
 if not WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FController = nil then Exit;
  if Length(FName) = 0 then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskFileImage.CreateImage - Name = ' + FName);
  {$ENDIF}

  {Check Open}
  if FHandle <> INVALID_HANDLE_VALUE then Exit;

  {Setup Parameters}
  if (AMediaType <> mtUNKNOWN) and (AMediaType <> mtINVALID) then FMediaType:=AMediaType;
  if (AFloppyType <> ftUNKNOWN) and (AFloppyType <> ftINVALID) then FFloppyType:=AFloppyType;
  if AAttributes <> iaNone then FAttributes:=AAttributes;
  if ASectorSize > 0 then FSectorSize:=ASectorSize;
  if ASectorCount > 0 then FSectorCount:=ASectorCount;
  if ACylinders > 0 then FCylinders:=ACylinders;
  if AHeads > 0 then FHeads:=AHeads;
  if ASectors > 0 then FSectors:=ASectors;
  if APartitionId <> pidUnused then FPartitionId:=APartitionId;

  {Check Parameters}
  if FSectorSize = 0 then Exit;
  if FSectorCount = 0 then Exit;
  FSectorShiftCount:=GetSectorShiftCount;

  {Create File}
  if not Writeable then Exit;
  Size:=(FSectorCount shl FSectorShiftCount);
  FHandle:=FDriver.FileCreate(FName);
  if FHandle = INVALID_HANDLE_VALUE then Exit;
  FDriver.FileSeek(FHandle,Size,soFromBeginning);
  FDriver.FileTruncate(FHandle);

  Result:=FImageNo;
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskFileImage.OpenImage(AMediaType:TMediaType;AFloppyType:TFloppyType;AAttributes:LongWord;ASectorSize:Word;const ASectorCount:Int64;ACylinders,AHeads,ASectors:LongWord;APartitionId:Byte):Integer;
var
 Mode:Integer;
begin
 {}
 Result:=0;
 
 if not WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FController = nil then Exit;
  if Length(FName) = 0 then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskFileImage.OpenImage - Name = ' + FName);
  {$ENDIF}

  {Check Open}
  if FHandle <> INVALID_HANDLE_VALUE then Exit;

  {Setup Parameters}
  if (AMediaType <> mtUNKNOWN) and (AMediaType <> mtINVALID) then FMediaType:=AMediaType;
  if (AFloppyType <> ftUNKNOWN) and (AFloppyType <> ftINVALID) then FFloppyType:=AFloppyType;
  if AAttributes <> iaNone then FAttributes:=AAttributes;
  if ASectorSize > 0 then FSectorSize:=ASectorSize;
  if ASectorCount > 0 then FSectorCount:=ASectorCount;
  if ACylinders > 0 then FCylinders:=ACylinders;
  if AHeads > 0 then FHeads:=AHeads;
  if ASectors > 0 then FSectors:=ASectors;
  if APartitionId <> pidUnused then FPartitionId:=APartitionId;

  {Open File}
  if not FDriver.FileExists(FName) then Exit;
  Mode:=fmOpenRead or fmShareDenyNone;
  if Writeable then Mode:=fmOpenReadWrite or fmShareDenyWrite;
  FHandle:=FDriver.FileOpen(FName,Mode);
  if FHandle = INVALID_HANDLE_VALUE then Exit;

  Result:=FImageNo;
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskFileImage.CloseImage:Boolean;
begin
 {}
 Result:=False;
 
 if not WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FController = nil then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskFileImage.CloseImage - Name = ' + FName);
  {$ENDIF}

  {Check Open}
  if FHandle = INVALID_HANDLE_VALUE then Exit;

  {Close File}
  FDriver.FileClose(FHandle);
  FHandle:=INVALID_HANDLE_VALUE;

  Result:=True;
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskFileImage.ResizeImage(const ASectorCount:Int64):Boolean;
begin
 {}
 Result:=False;
 
 if not WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FController = nil then Exit;
  
  //To Do //
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{TVirtualDiskDeviceImage}
constructor TVirtualDiskDeviceImage.Create(ADriver:TFileSysDriver;AController:TDiskController;const AName:String;AImageNo:Integer);
begin
 {}
 inherited Create(ADriver,AController,AName,AImageNo);
 FAttributes:=iaDisk or iaReadable or iaWriteable;

 FImageType:=itDEVICE;
 FMediaType:=mtFIXED;
 FFloppyType:=ftUNKNOWN;

 FCylinders:=0;
 FHeads:=0;
 FSectors:=0;
 FLogicalShiftCount:=0;

 FSectorSize:=MIN_SECTOR_SIZE;
 FSectorCount:=0;
 FSectorShiftCount:=0;

 FHandle:=INVALID_HANDLE_VALUE;
end;

{==============================================================================}

destructor TVirtualDiskDeviceImage.Destroy;
begin
 {}
 WriterLock;
 try
  if FHandle <> INVALID_HANDLE_VALUE then
   begin
    if (FAttributes and iaDisk) = iaDisk then FDriver.CloseDevice(FHandle);
    if (FAttributes and iaPartition) = iaPartition then FDriver.ClosePartition(FHandle);
    if (FAttributes and iaVolume) = iaVolume then FDriver.CloseVolume(FHandle);
    if (FAttributes and iaDrive) = iaDrive then FDriver.CloseDrive(FHandle);
   end;
  FHandle:=INVALID_HANDLE_VALUE;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  inherited Destroy;
 end; 
end;

{==============================================================================}

function TVirtualDiskDeviceImage.GetReady:Boolean;
begin
 {}
 Result:=(FHandle <> INVALID_HANDLE_VALUE);
end;

{==============================================================================}

function TVirtualDiskDeviceImage.GetCylinders:LongWord;
var
 Device:TDiskDevice;
 Partition:TDiskPartition;
 Volume:TDiskVolume;
 Drive:TDiskDrive;
begin
 {}
 Result:=0;
 if not Ready then Exit;

 if (FAttributes and iaDisk) = iaDisk then
  begin
   Device:=FDriver.GetDeviceByName(FName,True,FILESYS_LOCK_READ);
   if Device = nil then Exit;
   
   Result:=Device.PhysicalCylinders;
   
   {Unlock Device}
   Device.ReaderUnlock;
  end
 else if (FAttributes and iaPartition) = iaPartition then
  begin
   Partition:=FDriver.GetPartitionByPath(FName,True,FILESYS_LOCK_READ);
   if Partition = nil then Exit;
   try
    if Partition.Device = nil then Exit;
   
    Result:=Partition.Device.PhysicalCylinders;
   finally
    {Unlock Partition}
    Partition.ReaderUnlock;
   end; 
  end
 else if (FAttributes and iaVolume) = iaVolume then
  begin
   Volume:=FDriver.GetVolumeByName(FName,True,FILESYS_LOCK_READ);
   if Volume = nil then Exit;
   try
    if Volume.Device = nil then Exit;
    
    Result:=Volume.Device.PhysicalCylinders;
   finally 
    {Unlock Volume}
    Volume.ReaderUnlock;
   end; 
  end
 else if (FAttributes and iaDrive) = iaDrive then
  begin
   Drive:=FDriver.GetDriveByName(FName,True,FILESYS_LOCK_READ);
   if Drive = nil then Exit;
   try
    if Drive.Device = nil then Exit;
    
    Result:=Drive.Device.PhysicalCylinders;
   finally 
    {Unlock Drive}
    Drive.ReaderUnlock;
   end; 
  end
 else
  begin
   Result:=FCylinders;
  end;
end;

{==============================================================================}

function TVirtualDiskDeviceImage.GetHeads:LongWord;
var
 Device:TDiskDevice;
 Partition:TDiskPartition;
 Volume:TDiskVolume;
 Drive:TDiskDrive;
begin
 {}
 Result:=0;
 if not Ready then Exit;

 if (FAttributes and iaDisk) = iaDisk then
  begin
   Device:=FDriver.GetDeviceByName(FName,True,FILESYS_LOCK_READ);
   if Device = nil then Exit;
   
   Result:=Device.PhysicalHeads;
   
   {Unlock Device}
   Device.ReaderUnlock;
  end
 else if (FAttributes and iaPartition) = iaPartition then
  begin
   Partition:=FDriver.GetPartitionByPath(FName,True,FILESYS_LOCK_READ);
   if Partition = nil then Exit;
   try
    if Partition.Device = nil then Exit;
   
    Result:=Partition.Device.PhysicalHeads;
   finally 
    {Unlock Partition}
    Partition.ReaderUnlock;
   end;
  end
 else if (FAttributes and iaVolume) = iaVolume then
  begin
   Volume:=FDriver.GetVolumeByName(FName,True,FILESYS_LOCK_READ);
   if Volume = nil then Exit;
   try
    if Volume.Device = nil then Exit;
    
    Result:=Volume.Device.PhysicalHeads;
   finally 
    {Unlock Volume}
    Volume.ReaderUnlock;
   end; 
  end
 else if (FAttributes and iaDrive) = iaDrive then
  begin
   Drive:=FDriver.GetDriveByName(FName,True,FILESYS_LOCK_READ);
   if Drive = nil then Exit;
   try
    if Drive.Device = nil then Exit;
    
    Result:=Drive.Device.PhysicalHeads;
   finally 
    {Unlock Drive}
    Drive.ReaderUnlock;
   end; 
  end
 else
  begin
   Result:=FHeads;
  end;
end;

{==============================================================================}

function TVirtualDiskDeviceImage.GetSectors:LongWord;
var
 Device:TDiskDevice;
 Partition:TDiskPartition;
 Volume:TDiskVolume;
 Drive:TDiskDrive;
begin
 {}
 Result:=0;
 if not Ready then Exit;

 if (FAttributes and iaDisk) = iaDisk then
  begin
   Device:=FDriver.GetDeviceByName(FName,True,FILESYS_LOCK_READ);
   if Device = nil then Exit;
   
   Result:=Device.PhysicalSectors;
   
   {Unlock Device}
   Device.ReaderUnlock;
  end
 else if (FAttributes and iaPartition) = iaPartition then
  begin
   Partition:=FDriver.GetPartitionByPath(FName,True,FILESYS_LOCK_READ);
   if Partition = nil then Exit;
   try
    if Partition.Device = nil then Exit;
   
    Result:=Partition.Device.PhysicalSectors;
   finally 
    {Unlock Partition}
    Partition.ReaderUnlock;
   end;
  end
 else if (FAttributes and iaVolume) = iaVolume then
  begin
   Volume:=FDriver.GetVolumeByName(FName,True,FILESYS_LOCK_READ);
   if Volume = nil then Exit;
   try
    if Volume.Device = nil then Exit;
    
    Result:=Volume.Device.PhysicalSectors;
   finally 
    {Unlock Volume}
    Volume.ReaderUnlock;
   end; 
  end
 else if (FAttributes and iaDrive) = iaDrive then
  begin
   Drive:=FDriver.GetDriveByName(FName,True,FILESYS_LOCK_READ);
   if Drive = nil then Exit;
   try
    if Drive.Device = nil then Exit;
    
    Result:=Drive.Device.PhysicalSectors;
   finally 
    {Unlock Drive}
    Drive.ReaderUnlock;
   end; 
  end
 else
  begin
   Result:=FSectors;
  end;
end;

{==============================================================================}

function TVirtualDiskDeviceImage.GetSectorSize:Word;
var
 Device:TDiskDevice;
 Partition:TDiskPartition;
 Volume:TDiskVolume;
 Drive:TDiskDrive;
begin
 {}
 Result:=0;
 if not Ready then Exit;

 if (FAttributes and iaDisk) = iaDisk then
  begin
   Device:=FDriver.GetDeviceByName(FName,True,FILESYS_LOCK_READ);
   if Device = nil then Exit;
   
   Result:=Device.SectorSize;
   
   {Unlock Device}
   Device.ReaderUnlock;
  end
 else if (FAttributes and iaPartition) = iaPartition then
  begin
   Partition:=FDriver.GetPartitionByPath(FName,True,FILESYS_LOCK_READ);
   if Partition = nil then Exit;
   try
    if Partition.Device = nil then Exit;
   
    Result:=Partition.Device.SectorSize;
   finally
    {Unlock Partition}
    Partition.ReaderUnlock;
   end;   
  end
 else if (FAttributes and iaVolume) = iaVolume then
  begin
   Volume:=FDriver.GetVolumeByName(FName,True,FILESYS_LOCK_READ);
   if Volume = nil then Exit;
   
   Result:=Volume.SectorSize;

   {Unlock Volume}
   Volume.ReaderUnlock;
  end
 else if (FAttributes and iaDrive) = iaDrive then
  begin
   Drive:=FDriver.GetDriveByName(FName,True,FILESYS_LOCK_READ);
   if Drive = nil then Exit;
   
   Result:=Drive.SectorSize;
   
   {Unlock Drive}
   Drive.ReaderUnlock;
  end
 else
  begin
   Result:=FSectorSize;
  end;
end;

{==============================================================================}

function TVirtualDiskDeviceImage.GetSectorCount:Int64;
var
 Device:TDiskDevice;
 Partition:TDiskPartition;
 Volume:TDiskVolume;
 Drive:TDiskDrive;
begin
 {}
 Result:=0;
 if not Ready then Exit;

 if (FAttributes and iaDisk) = iaDisk then
  begin
   Device:=FDriver.GetDeviceByName(FName,True,FILESYS_LOCK_READ);
   if Device = nil then Exit;
   
   Result:=Device.SectorCount;
   
   {Unlock Device}
   Device.ReaderUnlock;
  end
 else if (FAttributes and iaPartition) = iaPartition then
  begin
   Partition:=FDriver.GetPartitionByPath(FName,True,FILESYS_LOCK_READ);
   if Partition = nil then Exit;
   
   Result:=Partition.SectorCount;
   
   {Unlock Partition}
   Partition.ReaderUnlock;
  end
 else if (FAttributes and iaVolume) = iaVolume then
  begin
   Volume:=FDriver.GetVolumeByName(FName,True,FILESYS_LOCK_READ);
   if Volume = nil then Exit;
   
   Result:=Volume.SectorCount;
   
   {Unlock Volume}
   Volume.ReaderUnlock;
  end
 else if (FAttributes and iaDrive) = iaDrive then
  begin
   Drive:=FDriver.GetDriveByName(FName,True,FILESYS_LOCK_READ);
   if Drive = nil then Exit;
   
   Result:=Drive.SectorCount;

   {Unlock Drive}
   Drive.ReaderUnlock;
  end
 else
  begin
   Result:=FSectorCount;
  end;
end;

{==============================================================================}

function TVirtualDiskDeviceImage.GetPartitionId:Byte;
var
 Partition:TDiskPartition;
 Volume:TDiskVolume;
 Drive:TDiskDrive;
begin
 {}
 Result:=pidUnused;
 if not Ready then Exit;

 if (FAttributes and iaDisk) = iaDisk then
  begin
   {Nothing}
  end
 else if (FAttributes and iaPartition) = iaPartition then
  begin
   Partition:=FDriver.GetPartitionByPath(FName,True,FILESYS_LOCK_READ);
   if Partition = nil then Exit;
   
   Result:=Partition.PartitionId;
   
   {Unlock Partition}
   Partition.ReaderUnlock;
  end
 else if (FAttributes and iaVolume) = iaVolume then
  begin
   Volume:=FDriver.GetVolumeByName(FName,True,FILESYS_LOCK_READ);
   if Volume = nil then Exit;
   try
    if Volume.Partition = nil then Exit;
    
    Result:=Volume.Partition.PartitionId;
   finally 
    {Unlock Volume}
    Volume.ReaderUnlock;
   end; 
  end
 else if (FAttributes and iaDrive) = iaDrive then
  begin
   Drive:=FDriver.GetDriveByName(FName,True,FILESYS_LOCK_READ);
   if Drive = nil then Exit;
   try
    if Drive.Partition = nil then Exit;
    
    Result:=Drive.Partition.PartitionId;
   finally 
    {Unlock Drive}
    Drive.ReaderUnlock;
   end; 
  end
 else
  begin
   Result:=FPartitionId;
  end;
end;

{==============================================================================}

function TVirtualDiskDeviceImage.Read(ASector:LongWord;ACount:Word;var ABuffer):Boolean;
{Note: Caller must hold the image lock}
var
 Start:Int64;
 Count:Integer;
begin
 {}
 Result:=False;

 if FDriver = nil then Exit;
 if FController = nil then Exit;

 {Check Open}
 if FHandle = INVALID_HANDLE_VALUE then Exit;

 {Check Read}
 if ASector >= FSectorCount then Exit;
 if (ASector + ACount) > FSectorCount then Exit;

 {Setup Read}
 Start:=ASector; {Allow for Int64 Result}
 Count:=ACount;
 Start:=(Start shl FSectorShiftCount);
 Count:=(Count shl FSectorShiftCount);

 {Perform Read}
 if (FAttributes and iaDisk) = iaDisk then
  begin
   FDriver.SeekDevice(FHandle,Start,soFromBeginning);
   Result:=(FDriver.ReadDevice(FHandle,ABuffer,Count) = Count);
  end
 else if (FAttributes and iaPartition) = iaPartition then
  begin
   FDriver.SeekPartition(FHandle,Start,soFromBeginning);
   Result:=(FDriver.ReadPartition(FHandle,ABuffer,Count) = Count);
  end
 else if (FAttributes and iaVolume) = iaVolume then
  begin
   FDriver.SeekVolume(FHandle,Start,soFromBeginning);
   Result:=(FDriver.ReadVolume(FHandle,ABuffer,Count) = Count);
  end
 else if (FAttributes and iaDrive) = iaDrive then
  begin
   FDriver.SeekDrive(FHandle,Start,soFromBeginning);
   Result:=(FDriver.ReadDrive(FHandle,ABuffer,Count) = Count);
  end;
end;

{==============================================================================}

function TVirtualDiskDeviceImage.Write(ASector:LongWord;ACount:Word;const ABuffer):Boolean;
{Note: Caller must hold the image lock}
var
 Start:Int64;
 Count:Integer;
begin
 {}
 Result:=False;

 if FDriver = nil then Exit;
 if FController = nil then Exit;

 {Check Open}
 if FHandle = INVALID_HANDLE_VALUE then Exit;

 {Check Write}
 if ASector >= FSectorCount then Exit;
 if (ASector + ACount) > FSectorCount then Exit;

 {Setup Write}
 Start:=ASector; {Allow for Int64 Result}
 Count:=ACount;
 Start:=(Start shl FSectorShiftCount);
 Count:=(Count shl FSectorShiftCount);

 {Perform Write}
 if (FAttributes and iaDisk) = iaDisk then
  begin
   FDriver.SeekDevice(FHandle,Start,soFromBeginning);
   Result:=(FDriver.WriteDevice(FHandle,ABuffer,Count) = Count);
  end
 else if (FAttributes and iaPartition) = iaPartition then
  begin
   FDriver.SeekPartition(FHandle,Start,soFromBeginning);
   Result:=(FDriver.WritePartition(FHandle,ABuffer,Count) = Count);
  end
 else if (FAttributes and iaVolume) = iaVolume then
  begin
   FDriver.SeekVolume(FHandle,Start,soFromBeginning);
   Result:=(FDriver.WriteVolume(FHandle,ABuffer,Count) = Count);
  end
 else if (FAttributes and iaDrive) = iaDrive then
  begin
   FDriver.SeekDrive(FHandle,Start,soFromBeginning);
   Result:=(FDriver.WriteDrive(FHandle,ABuffer,Count) = Count);
  end;
end;

{==============================================================================}

function TVirtualDiskDeviceImage.ConvertImage(AImageType:TImageType):Boolean;
begin
 {Not Supported}
 Result:=False;
end;

{==============================================================================}

function TVirtualDiskDeviceImage.ShrinkImage(const ASize:Int64):Boolean;
begin
 {Not Supported}
 Result:=False;
end;

{==============================================================================}

function TVirtualDiskDeviceImage.ExpandImage(const ASize:Int64):Boolean;
begin
 {Not Supported}
 Result:=False;
end;

{==============================================================================}

function TVirtualDiskDeviceImage.OpenImage(AMediaType:TMediaType;AFloppyType:TFloppyType;AAttributes:LongWord;ASectorSize:Word;const ASectorCount:Int64;ACylinders,AHeads,ASectors:LongWord;APartitionId:Byte):Integer;
{Note: Open only on Device Image, Cannot Create}
var
 Mode:Integer;
begin
 {}
 Result:=0;
 
 if not WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FController = nil then Exit;
  if Length(FName) = 0 then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskDeviceImage.OpenImage - Name = ' + FName);
  {$ENDIF}

  {Check Open}
  if FHandle <> INVALID_HANDLE_VALUE then Exit;

  {Setup Parameters}
  if (AMediaType <> mtUNKNOWN) and (AMediaType <> mtINVALID) then FMediaType:=AMediaType;
  if (AFloppyType <> ftUNKNOWN) and (AFloppyType <> ftINVALID) then FFloppyType:=AFloppyType;
  if AAttributes <> iaNone then FAttributes:=AAttributes;
  if ASectorSize > 0 then FSectorSize:=ASectorSize;
  if ASectorCount > 0 then FSectorCount:=ASectorCount;
  {if ACylinders > 0 then FCylinders:=ACylinders;}                 {Ignore Cylinders}
  {if AHeads > 0 then FHeads:=AHeads;}                             {Ignore Heads}
  {if ASectors > 0 then FSectors:=ASectors;}                       {Ignore Sectors}
  {if APartitionId <> pidUnused then FPartitionId:=APartitionId;}  {Ignore PartitionId}

  {Open Device}
  Mode:=fmOpenRead or fmShareDenyNone;
  if Writeable then Mode:=fmOpenReadWrite or fmShareDenyWrite;

  if (FAttributes and iaDisk) = iaDisk then
   begin
    if FDriver.GetDeviceByName(FName,False,FILESYS_LOCK_NONE) = nil then Exit; {Do not lock}
    
    FHandle:=FDriver.OpenDevice(FName,Mode);
    if FHandle = INVALID_HANDLE_VALUE then Exit;

    Result:=FImageNo;
   end
  else if (FAttributes and iaPartition) = iaPartition then
   begin
    if FDriver.GetPartitionByPath(FName,False,FILESYS_LOCK_NONE) = nil then Exit; {Do not lock}
    
    FHandle:=FDriver.OpenPartition(FName,Mode);
    if FHandle = INVALID_HANDLE_VALUE then Exit;

    Result:=FImageNo;
   end
  else if (FAttributes and iaVolume) = iaVolume then
   begin
    if FDriver.GetVolumeByName(FName,False,FILESYS_LOCK_NONE) = nil then Exit; {Do not lock}
    
    FHandle:=FDriver.OpenVolume(FName,Mode);
    if FHandle = INVALID_HANDLE_VALUE then Exit;

    Result:=FImageNo;
   end
  else if (FAttributes and iaDrive) = iaDrive then
   begin
    if FDriver.GetDriveByName(FName,False,FILESYS_LOCK_NONE) = nil then Exit; {Do not lock}
    
    FHandle:=FDriver.OpenDrive(FName,Mode);
    if FHandle = INVALID_HANDLE_VALUE then Exit;

    Result:=FImageNo;
   end;
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskDeviceImage.CloseImage:Boolean;
begin
 {}
 Result:=False;
 
 if not WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FController = nil then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskDeviceImage.CloseImage - Name = ' + FName);
  {$ENDIF}

  {Check Open}
  if FHandle = INVALID_HANDLE_VALUE then Exit;

  {Close Device}
  if (FAttributes and iaDisk) = iaDisk then FDriver.CloseDevice(FHandle);
  if (FAttributes and iaPartition) = iaPartition then FDriver.ClosePartition(FHandle);
  if (FAttributes and iaVolume) = iaVolume then FDriver.CloseVolume(FHandle);
  if (FAttributes and iaDrive) = iaDrive then FDriver.CloseDrive(FHandle);
  FHandle:=INVALID_HANDLE_VALUE;

  Result:=True;
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{TVirtualDiskIsoImage}
constructor TVirtualDiskIsoImage.Create(ADriver:TFileSysDriver;AController:TDiskController;const AName:String;AImageNo:Integer);
begin
 {}
 inherited Create(ADriver,AController,AName,AImageNo);
 FAttributes:=iaDisk or iaCDROM or iaReadable or iaWriteable;

 FImageType:=itISO;
 FMediaType:=mtCDROM;
 FFloppyType:=ftUNKNOWN;

 FCylinders:=0;
 FHeads:=0;
 FSectors:=0;
 FLogicalShiftCount:=0;

 FSectorSize:=ISO_SECTOR_SIZE;
 FSectorCount:=0;
 FSectorShiftCount:=0;

 FHandle:=INVALID_HANDLE_VALUE;
end;

{==============================================================================}

destructor TVirtualDiskIsoImage.Destroy;
begin
 {}
 WriterLock;
 try
  if FHandle <> INVALID_HANDLE_VALUE then FDriver.FileClose(FHandle);
  FHandle:=INVALID_HANDLE_VALUE;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  inherited Destroy;
 end; 
end;

{==============================================================================}

function TVirtualDiskIsoImage.GetReady:Boolean;
begin
 {}
 Result:=(FHandle <> INVALID_HANDLE_VALUE);
end;

{==============================================================================}

function TVirtualDiskIsoImage.GetSectorSize:Word;
begin
 {}
 Result:=0;
 if not Ready then Exit;

 Result:=FSectorSize;
 if FSectorSize > 0 then Exit;
 Result:=ISO_SECTOR_SIZE;
end;

{==============================================================================}

function TVirtualDiskIsoImage.GetSectorCount:Int64;
var
 Size:Int64;
begin
 {}
 Result:=0;
 if not Ready then Exit;

 Result:=FSectorCount;
 if FSectorCount > 0 then Exit;
 Size:=FDriver.FileSizeEx(FHandle);
 if FSectorSize = 0 then Exit;
 Result:=(Size div FSectorSize);
end;

{==============================================================================}

function TVirtualDiskIsoImage.Read(ASector:LongWord;ACount:Word;var ABuffer):Boolean;
{Note: Caller must hold the image lock}
var
 Start:Int64;
 Count:Integer;
begin
 {}
 Result:=False;

 if FDriver = nil then Exit;
 if FController = nil then Exit;

 {Check Open}
 if FHandle = INVALID_HANDLE_VALUE then Exit;

 {Check Read}
 if ASector >= FSectorCount then Exit;
 if (ASector + ACount) > FSectorCount then Exit;

 {Setup Read}
 Start:=ASector; {Allow for Int64 Result}
 Count:=ACount;
 Start:=(Start shl FSectorShiftCount);
 Count:=(Count shl FSectorShiftCount);

 {Perform Read}
 FDriver.FileSeekEx(FHandle,Start,soFromBeginning);
 Result:=(FDriver.FileRead(FHandle,ABuffer,Count) = Count);
end;

{==============================================================================}

function TVirtualDiskIsoImage.Write(ASector:LongWord;ACount:Word;const ABuffer):Boolean;
{Note: Caller must hold the image lock}
var
 Start:Int64;
 Count:Integer;
begin
 {}
 Result:=False;

 if FDriver = nil then Exit;
 if FController = nil then Exit;

 {Check Open}
 if FHandle = INVALID_HANDLE_VALUE then Exit;

 {Check Write}
 if ASector >= FSectorCount then Exit;
 if (ASector + ACount) > FSectorCount then Exit;

 {Setup Write}
 Start:=ASector; {Allow for Int64 Result}
 Count:=ACount;
 Start:=(Start shl FSectorShiftCount);
 Count:=(Count shl FSectorShiftCount);

 {Perform Write}
 FDriver.FileSeekEx(FHandle,Start,soFromBeginning);
 Result:=(FDriver.FileWrite(FHandle,ABuffer,Count) = Count);
end;

{==============================================================================}

function TVirtualDiskIsoImage.ConvertImage(AImageType:TImageType):Boolean;
begin
 {Not Supported}
 Result:=False;
end;

{==============================================================================}

function TVirtualDiskIsoImage.CreateImage(AMediaType:TMediaType;AFloppyType:TFloppyType;AAttributes:LongWord;ASectorSize:Word;const ASectorCount:Int64;ACylinders,AHeads,ASectors:LongWord;APartitionId:Byte):Integer;
var
 Size:Int64;
begin
 {}
 Result:=0;
 
 if not WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FController = nil then Exit;
  if Length(FName) = 0 then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskIsoImage.CreateImage - Name = ' + FName);
  {$ENDIF}

  {Check Open}
  if FHandle <> INVALID_HANDLE_VALUE then Exit;

  {Setup Parameters}
  if (AMediaType <> mtUNKNOWN) and (AMediaType <> mtINVALID) then FMediaType:=AMediaType;
  if (AFloppyType <> ftUNKNOWN) and (AFloppyType <> ftINVALID) then FFloppyType:=AFloppyType;
  if AAttributes <> iaNone then FAttributes:=AAttributes;
  if ASectorSize > 0 then FSectorSize:=ASectorSize;
  if ASectorCount > 0 then FSectorCount:=ASectorCount;
  if ACylinders > 0 then FCylinders:=ACylinders;
  if AHeads > 0 then FHeads:=AHeads;
  if ASectors > 0 then FSectors:=ASectors;
  {if APartitionId <> pidUnused then FPartitionId:=APartitionId;} {Ignore PartitionId}

  {Check Parameters}
  if FSectorSize = 0 then Exit;
  if FSectorCount = 0 then Exit;
  FSectorShiftCount:=GetSectorShiftCount;

  {Create File}
  if not Writeable then Exit;
  Size:=(FSectorCount shl FSectorShiftCount);
  FHandle:=FDriver.FileCreate(FName);
  if FHandle = INVALID_HANDLE_VALUE then Exit;
  FDriver.FileSeek(FHandle,Size,soFromBeginning);
  FDriver.FileTruncate(FHandle);

  Result:=FImageNo;
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskIsoImage.OpenImage(AMediaType:TMediaType;AFloppyType:TFloppyType;AAttributes:LongWord;ASectorSize:Word;const ASectorCount:Int64;ACylinders,AHeads,ASectors:LongWord;APartitionId:Byte):Integer;
var
 Mode:Integer;
begin
 {}
 Result:=0;
 
 if not WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FController = nil then Exit;
  if Length(FName) = 0 then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskIsoImage.OpenImage - Name = ' + FName);
  {$ENDIF}

  {Check Open}
  if FHandle <> INVALID_HANDLE_VALUE then Exit;

  {Setup Parameters}
  if (AMediaType <> mtUNKNOWN) and (AMediaType <> mtINVALID) then FMediaType:=AMediaType;
  if (AFloppyType <> ftUNKNOWN) and (AFloppyType <> ftINVALID) then FFloppyType:=AFloppyType;
  if AAttributes <> iaNone then FAttributes:=AAttributes;
  if ASectorSize > 0 then FSectorSize:=ASectorSize;
  if ASectorCount > 0 then FSectorCount:=ASectorCount;
  if ACylinders > 0 then FCylinders:=ACylinders;
  if AHeads > 0 then FHeads:=AHeads;
  if ASectors > 0 then FSectors:=ASectors;
  {if APartitionId <> pidUnused then FPartitionId:=APartitionId;} {Ignore PartitionId}

  {Open File}
  if not FDriver.FileExists(FName) then Exit;
  Mode:=fmOpenRead or fmShareDenyNone;
  if Writeable then Mode:=fmOpenReadWrite or fmShareDenyWrite;
  FHandle:=FDriver.FileOpen(FName,Mode);
  if FHandle = INVALID_HANDLE_VALUE then Exit;

  Result:=FImageNo;
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskIsoImage.CloseImage:Boolean;
begin
 {}
 Result:=False;
 
 if not WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FController = nil then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskIsoImage.CloseImage - Name = ' + FName);
  {$ENDIF}

  {Check Open}
  if FHandle = INVALID_HANDLE_VALUE then Exit;

  {Close File}
  FDriver.FileClose(FHandle);
  FHandle:=INVALID_HANDLE_VALUE;

  Result:=True;
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskIsoImage.ResizeImage(const ASectorCount:Int64):Boolean;
begin
 {}
 Result:=False;

 if not WriterLock then Exit;
 try
  //To Do
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{TVirtualDiskBochsImage}
constructor TVirtualDiskBochsImage.Create(ADriver:TFileSysDriver;AController:TDiskController;const AName:String;AImageNo:Integer);
begin
 {}
 inherited Create(ADriver,AController,AName,AImageNo);
 FAttributes:=iaDisk or iaFlat or iaReadable or iaWriteable;

 FImageType:=itBOCHS;
 FMediaType:=mtFIXED;
 FFloppyType:=ftUNKNOWN;

 FCylinders:=0;
 FHeads:=0;
 FSectors:=0;
 FLogicalShiftCount:=0;

 FSectorSize:=MIN_SECTOR_SIZE;
 FSectorCount:=0;
 FSectorShiftCount:=0;
end;

{==============================================================================}

destructor TVirtualDiskBochsImage.Destroy;
begin
 {}
 WriterLock;
 try
  {Nothing}
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  inherited Destroy;
 end; 
end;

{==============================================================================}

function TVirtualDiskBochsImage.GetReady:Boolean;
begin
 {}
 Result:=False;
 //To Do //
end;

{==============================================================================}

function TVirtualDiskBochsImage.GetCylinders:LongWord;
begin
 {}
 Result:=FCylinders;
 //To Do
end;

{==============================================================================}

function TVirtualDiskBochsImage.GetHeads:LongWord;
begin
 {}
 Result:=FHeads;
 //To Do
end;

{==============================================================================}

function TVirtualDiskBochsImage.GetSectors:LongWord;
begin
 {}
 Result:=FSectors;
 //To Do
end;

{==============================================================================}

function TVirtualDiskBochsImage.GetSectorSize:Word;
begin
 {}
 Result:=FSectorSize;
 //To Do
end;

{==============================================================================}

function TVirtualDiskBochsImage.GetSectorCount:Int64;
begin
 {}
 Result:=FSectorCount;
 //To Do
end;

{==============================================================================}

function TVirtualDiskBochsImage.Read(ASector:LongWord;ACount:Word;var ABuffer):Boolean;
{Note: Caller must hold the image lock}
begin
 {}
 Result:=False;
 
 //To Do //
end;

{==============================================================================}

function TVirtualDiskBochsImage.Write(ASector:LongWord;ACount:Word;const ABuffer):Boolean;
{Note: Caller must hold the image lock}
begin
 {}
 Result:=False;
 
 //To Do //
end;

{==============================================================================}

function TVirtualDiskBochsImage.Allocated(ASector:LongWord;ACount:Word):Word;
begin
 {}
 Result:=ACount;
 
 //To Do //
end;

{==============================================================================}

function TVirtualDiskBochsImage.CreateImage(AMediaType:TMediaType;AFloppyType:TFloppyType;AAttributes:LongWord;ASectorSize:Word;const ASectorCount:Int64;ACylinders,AHeads,ASectors:LongWord;APartitionId:Byte):Integer;
begin
 {}
 Result:=0;
 
 if not WriterLock then Exit;
 try
  //To Do //
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskBochsImage.OpenImage(AMediaType:TMediaType;AFloppyType:TFloppyType;AAttributes:LongWord;ASectorSize:Word;const ASectorCount:Int64;ACylinders,AHeads,ASectors:LongWord;APartitionId:Byte):Integer;
begin
 {}
 Result:=0;
 
 if not WriterLock then Exit;
 try
  //To Do //
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskBochsImage.CloseImage:Boolean;
begin
 {}
 Result:=False;
 
 if not WriterLock then Exit;
 try
  //To Do //
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskBochsImage.ResizeImage(const ASectorCount:Int64):Boolean;
begin
 {}
 Result:=False;
 
 if not WriterLock then Exit;
 try
  //To Do //
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{TVirtualDiskVmwareImage}
constructor TVirtualDiskVmwareImage.Create(ADriver:TFileSysDriver;AController:TDiskController;const AName:String;AImageNo:Integer);
begin
 {}
 inherited Create(ADriver,AController,AName,AImageNo);
 FAttributes:=iaDisk or iaFixed or iaReadable or iaWriteable;

 FImageType:=itVMWARE;
 FMediaType:=mtFIXED;
 FFloppyType:=ftUNKNOWN;

 FCylinders:=0;
 FHeads:=0;
 FSectors:=0;
 FLogicalShiftCount:=0;

 FSectorSize:=vmwareSectorSize;
 FSectorCount:=0;
 FSectorShiftCount:=0;
end;

{==============================================================================}

destructor TVirtualDiskVmwareImage.Destroy;
begin
 {}
 WriterLock;
 try
  {Nothing}
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  inherited Destroy;
 end; 
end;

{==============================================================================}

function TVirtualDiskVmwareImage.GetAttributes:LongWord;
begin
 {}
 Result:=iaDisk or iaFixed or iaReadable or iaWriteable;
 //To Do //
end;

{==============================================================================}

function TVirtualDiskVmwareImage.GetReady:Boolean;
begin
 {}
 Result:=False;
 //To Do //
end;

{==============================================================================}

function TVirtualDiskVmwareImage.GetCylinders:LongWord;
begin
 {}
 Result:=FCylinders;
 //To Do
end;

{==============================================================================}

function TVirtualDiskVmwareImage.GetHeads:LongWord;
begin
 {}
 Result:=FHeads;
 //To Do
end;

{==============================================================================}

function TVirtualDiskVmwareImage.GetSectors:LongWord;
begin
 {}
 Result:=FSectors;
 //To Do
end;

{==============================================================================}

function TVirtualDiskVmwareImage.GetSectorSize:Word;
begin
 {}
 Result:=FSectorSize;
 //To Do
end;

{==============================================================================}

function TVirtualDiskVmwareImage.GetSectorCount:Int64;
begin
 {}
 Result:=FSectorCount;
 //To Do
end;

{==============================================================================}

function TVirtualDiskVmwareImage.ImageInit:Boolean;
begin
 {}
 Result:=False;
 
 if not WriterLock then Exit;
 try
  //To Do //
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVmwareImage.Read(ASector:LongWord;ACount:Word;var ABuffer):Boolean;
{Note: Caller must hold the image lock}
begin
 {}
 Result:=False;
 
 //To Do //
end;

{==============================================================================}

function TVirtualDiskVmwareImage.Write(ASector:LongWord;ACount:Word;const ABuffer):Boolean;
{Note: Caller must hold the image lock}
begin
 {}
 Result:=False;
 
 //To Do //
end;

{==============================================================================}

function TVirtualDiskVmwareImage.Allocated(ASector:LongWord;ACount:Word):Word;
begin
 {}
 Result:=ACount;
 //To Do //
end;

{==============================================================================}

function TVirtualDiskVmwareImage.LoadDescriptor(const AFilename:String):Boolean;
begin
 {}
 Result:=False;
 //To Do
end;

{==============================================================================}

function TVirtualDiskVmwareImage.AddDescriptor(const AFilename:String):TVirtualDiskVmwareExtent;
begin
 {}
 Result:=nil;
 //To Do
end;

{==============================================================================}

function TVirtualDiskVmwareImage.RemoveDescriptor(AExtent:TVirtualDiskVmwareExtent):Boolean;
begin
 {}
 Result:=False;
 //To Do
end;

{==============================================================================}

function TVirtualDiskVmwareImage.SetDescriptor(AExtent:TVirtualDiskVmwareExtent):Boolean;
begin
 {}
 Result:=False;
 //To Do
end;

{==============================================================================}

function TVirtualDiskVmwareImage.GetDescriptor:TVirtualDiskVmwareExtent;
begin
 {}
 Result:=nil;
 //To Do
end;

{==============================================================================}

function TVirtualDiskVmwareImage.CreateImage(AMediaType:TMediaType;AFloppyType:TFloppyType;AAttributes:LongWord;ASectorSize:Word;const ASectorCount:Int64;ACylinders,AHeads,ASectors:LongWord;APartitionId:Byte):Integer;
begin
 {}
 Result:=0;
 
 if not WriterLock then Exit;
 try
  //To Do //
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVmwareImage.OpenImage(AMediaType:TMediaType;AFloppyType:TFloppyType;AAttributes:LongWord;ASectorSize:Word;const ASectorCount:Int64;ACylinders,AHeads,ASectors:LongWord;APartitionId:Byte):Integer;
begin
 {}
 Result:=0;
 
 if not WriterLock then Exit;
 try
  //To Do //
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVmwareImage.CloseImage:Boolean;
begin
 {}
 Result:=False;
 
 if not WriterLock then Exit;
 try
  //To Do //
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVmwareImage.ResizeImage(const ASectorCount:Int64):Boolean;
begin
 {}
 Result:=False;
 
 if not WriterLock then Exit;
 try
  //To Do //
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVmwareImage.CreateSnapshot:Boolean;
begin
 {}
 Result:=False;
 
 if not WriterLock then Exit;
 try
  //To Do //
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVmwareImage.DeleteSnapshot:Boolean;
begin
 {}
 Result:=False;
 
 if not WriterLock then Exit;
 try
  //To Do //
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVmwareImage.MergeSnapshot:Boolean;
begin
 {}
 Result:=False;
 
 if not WriterLock then Exit;
 try
  //To Do //
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{TVirtualDiskVpcImage}
constructor TVirtualDiskVpcImage.Create(ADriver:TFileSysDriver;AController:TDiskController;const AName:String;AImageNo:Integer);
begin
 {}
 inherited Create(ADriver,AController,AName,AImageNo);
 FAttributes:=iaDisk or iaFixed or iaReadable or iaWriteable;

 FImageType:=itVPC;
 FMediaType:=mtFIXED;
 FFloppyType:=ftUNKNOWN;

 FCylinders:=0;
 FHeads:=0;
 FSectors:=0;
 FLogicalShiftCount:=0;

 FSectorSize:=vpcSectorSize;
 FSectorCount:=0;
 FSectorShiftCount:=GetSectorShiftCount;  {Sector is always 512 bytes in VPC images}
end;

{==============================================================================}

destructor TVirtualDiskVpcImage.Destroy;
begin
 {}
 WriterLock;
 try
  {Nothing}
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  inherited Destroy;
 end; 
end;

{==============================================================================}

function TVirtualDiskVpcImage.GetAttributes:LongWord;
begin
 {}
 Result:=iaDisk or iaFixed or iaReadable or iaWriteable;
 
 if FBase = nil then Exit;
 
 Result:=iaDisk;
 if IsSplit then Result:=(Result or iaSplit) else Result:=(Result and not(iaSplit));
 if (FAttributes and iaReadable) = iaReadable then Result:=(Result or iaReadable);
 if (FAttributes and iaWriteable) = iaWriteable then Result:=(Result or iaWriteable);
 if FBase.IsFixed then Result:=(Result or iaFixed) else Result:=(Result and not(iaFixed));
 if FBase.IsDynamic then Result:=(Result or iaDynamic) else Result:=(Result and not(iaDynamic));
 if FCurrent = nil then Exit;
 if FCurrent.IsDelta then Result:=(Result or iaUndoable) else Result:=(Result and not(iaUndoable));
end;

{==============================================================================}

function TVirtualDiskVpcImage.ChecksumFooter(AFooter:PVpcHardDiskFooter):LongWord;
var
 Offset:LongWord;
 Original:LongWord;
 Checksum:LongWord;
begin
 {}
 Result:=0;
 Original:=AFooter.Checksum;
 AFooter.Checksum:=0;
 try
  Offset:=0;
  Checksum:=0;
  
  while Offset < SizeOf(TVpcHardDiskFooter) do
   begin
    Inc(Checksum,Byte(Pointer(LongWord(AFooter) + Offset)^));
    Inc(Offset,SizeOf(Byte));
   end;
   
  Result:=not(Checksum);
 finally
  AFooter.Checksum:=Original;
 end;
end;

{==============================================================================}

function TVirtualDiskVpcImage.ChecksumSparse(ASparse:PVpcDynamicDiskHeader):LongWord;
var
 Offset:LongWord;
 Original:LongWord;
 Checksum:LongWord;
begin
 {}
 Result:=0;
 Original:=ASparse.Checksum;
 ASparse.Checksum:=0;
 try
  Offset:=0;
  Checksum:=0;
  
  while Offset < SizeOf(TVpcDynamicDiskHeader) do
   begin
    Inc(Checksum,Byte(Pointer(LongWord(ASparse) + Offset)^));
    Inc(Offset,SizeOf(Byte));
   end;
   
  Result:=not(Checksum);
 finally
  ASparse.Checksum:=Original;
 end;
end;

{==============================================================================}

function TVirtualDiskVpcImage.RoundToSector(AValue:LongWord;ASectorSize:Word):LongWord;
var
 Count:LongWord;
begin
 {}
 Result:=AValue;
 if ASectorSize = 0 then Exit;
 
 Count:=(AValue div ASectorSize);
 if (Count * ASectorSize) < AValue then Inc(Count); //To Do //This needs to be shl/shr based
 Result:=(Count * ASectorSize);
end;

{==============================================================================}

function TVirtualDiskVpcImage.GetReady:Boolean;
begin
 {}
 Result:=False;
 if FBase = nil then Exit;
 
 Result:=True;
end;

{==============================================================================}

function TVirtualDiskVpcImage.GetCylinders:LongWord;
begin
 {}
 Result:=0;
 if not Ready then Exit;
 
 Result:=WordBEtoN(TVirtualDiskVpcExtent(FBase).Footer.DiskGeometry.Cylinders);
 if Result = 0 then Result:=inherited GetCylinders;
end;

{==============================================================================}

function TVirtualDiskVpcImage.GetHeads:LongWord;
begin
 {}
 Result:=0;
 if not Ready then Exit;
 
 Result:=TVirtualDiskVpcExtent(FBase).Footer.DiskGeometry.Heads;
 if Result = 0 then Result:=inherited GetHeads;
end;

{==============================================================================}

function TVirtualDiskVpcImage.GetSectors:LongWord;
begin
 {}
 Result:=0;
 if not Ready then Exit;
 
 Result:=TVirtualDiskVpcExtent(FBase).Footer.DiskGeometry.Sectors;
 if Result = 0 then Result:=inherited GetSectors;
end;

{==============================================================================}

function TVirtualDiskVpcImage.GetSectorSize:Word;
begin
 {}
 Result:=vpcSectorSize; {Always 512 bytes in VPC images}
end;

{==============================================================================}

function TVirtualDiskVpcImage.GetSectorCount:Int64;
begin
 {}
 Result:=0;
 if not Ready then Exit;
 {Result:=Int64BEtoN(TVirtualDiskVpcExtent(FBase).Footer.CurrentSize) div vpcSectorSize;} {Dont use Shift Count, may not be initialized at this point}
 Result:=Int64BEtoN(TVirtualDiskVpcExtent(FBase).Footer.CurrentSize) shr FSectorShiftCount; {Shift Count is initialized on create due to fixed sector size}
end;

{==============================================================================}

function TVirtualDiskVpcImage.ReadExtent(AExtent:TVirtualDiskExtent;ASector:LongWord;ACount:Word;var ABuffer):Word;
{Note: Caller must hold the extent lock}
var
 Offset:Int64;         {Absolute Offset of Read from file}
 Count:LongWord;       {Count of Bytes to Read from file}

 TableValue:Int64;     {The Block offset value read from the Table}
 TableOffset:LongWord; {The offset of the Blocks entry in the Table}

 BlockNo:LongWord;     {Block Number in the Table (0 to N - 1 blocks)}
 BlockStart:LongWord;  {Starting Sector number of Block}
 BlockCount:LongWord;  {Number of Sectors in Block}
 BitmapCount:LongWord; {Number of Free or Used bits in the Block bitmap}

 Table:TVirtualDiskVpcTable;
 Block:TVirtualDiskVpcBlock;
begin
 {}
 Result:=0;
 
 if not FExtents.ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if AExtent = nil then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.ReadExtent - Name = ' + AExtent.Filename);
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                                  Sector = ' + IntToStr(ASector) + ' Count = ' + IntToStr(ACount));
  {$ENDIF}
  
  {Check Read}
  if ASector < AExtent.StartSector then Exit;
  if ASector >= (AExtent.StartSector + AExtent.SectorCount) then Exit;
  {if (ASector + ACount) > (AExtent.StartSector + AExtent.SectorCount) then Exit;} {Allow Read greater than SectorCount, return actual Count}

  {Check Fixed}
  if AExtent.IsFixed then
   begin
    {Get Offset}
    Offset:=((ASector - AExtent.StartSector) shl FSectorShiftCount);
    Count:=Min(ACount,(AExtent.SectorCount - (ASector - AExtent.StartSector))) shl FSectorShiftCount;
    if AExtent.HasDelta then Count:=Min(ACount,1) shl FSectorShiftCount; {Only Read 1 sector if Delta exists}
    
    {$IFDEF VIRTUAL_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   Offset = ' + IntToHex(Offset,16));
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   Count = ' + IntToHex(Count,8));
    {$ENDIF}
    
    {Read Extent}
    FDriver.FileSeekEx(AExtent.Handle,Offset,soFromBeginning);
    if FDriver.FileRead(AExtent.Handle,ABuffer,Count) <> Integer(Count) then Exit;
    
    Result:=(Count shr FSectorShiftCount);
   end
  else
   begin
    {Get Table}
    Table:=TVirtualDiskVpcExtent(AExtent).Table;
    if Table = nil then Exit;
    if Table.Data = nil then Exit;
    
    {Get Block}
    BlockNo:=(ASector shr AExtent.BlockShiftCount);                                {Get the Block No}
    TableOffset:=(BlockNo shl 2);                                                  {Get the Table Offset (Multiply BlockNo by 4)}
    TableValue:=LongWordBEtoN(LongWord(Pointer(LongWord(Table.Data) + TableOffset)^));  {Get the Table Value}
    
    {$IFDEF VIRTUAL_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   BlockNo = ' + IntToHex(BlockNo,8));
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   TableOffset = ' + IntToHex(TableOffset,8));
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   TableValue = ' + IntToHex(TableValue,8));
    {$ENDIF}
    
    {Check Block}
    if TableValue = vpcUnallocatedBlock then
     begin
      {Check Delta}
      if AExtent.IsDelta then Exit;
      
      {Get Block}
      BlockStart:=(BlockNo shl AExtent.BlockShiftCount);                       {Get the Block Start Sector (Multiply BlockNo by BlockShift)}
      BlockCount:=(AExtent.BlockSize shr FSectorShiftCount);                   {Get the Block Sector Count (Divide BlockSize by SectorSize)}
      
      {Get Count}
      Count:=Min(ACount,(BlockCount - (ASector - BlockStart))) shl FSectorShiftCount;
      if AExtent.HasDelta then Count:=Min(ACount,1) shl FSectorShiftCount; {Only Read 1 sector if Delta exists}
      
      {$IFDEF VIRTUAL_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   BlockStart = ' + IntToHex(BlockStart,8));
      if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   BlockCount = ' + IntToHex(BlockCount,8));
      if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   Count = ' + IntToHex(Count,8));
      {$ENDIF}
      
      {Read Extent (Zero)}
      ZeroMemory(@ABuffer,Count);
      
      Result:=(Count shr FSectorShiftCount);
     end
    else
     begin
      {Get Block}
      Block:=TVirtualDiskVpcBlock(GetBlock(Table,ASector,False));                {Get the Block}
      if Block = nil then Exit;
      
      {Test Block (Used)}
      BitmapCount:=TestBlock(Block,ASector,ACount,True);
      if BitmapCount = 0 then
       begin
        {Start Bit is Free}
        {Check Delta}
        if AExtent.IsDelta then Exit;
        
        {Test Block (Free)}
        BitmapCount:=TestBlock(Block,ASector,ACount,False);
        if BitmapCount = 0 then Exit;
        
        {Get Count}
        Count:=Min(ACount,BitmapCount) shl FSectorShiftCount;
        if AExtent.HasDelta then Count:=Min(ACount,1) shl FSectorShiftCount; {Only Read 1 sector if Delta exists}
        
        {$IFDEF VIRTUAL_DEBUG}
        if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   Count = ' + IntToHex(Count,8));
        {$ENDIF}
        
        {Read Extent (Zero)}
        ZeroMemory(@ABuffer,Count);
        
        Result:=(Count shr FSectorShiftCount);
       end
      else
       begin
        {Start Bit is Used}
        {Get Offset}
        Offset:=Block.DataOffset + ((ASector - Block.StartSector) shl FSectorShiftCount);
        Count:=Min(ACount,BitmapCount) shl FSectorShiftCount;
        if AExtent.HasDelta then Count:=Min(ACount,1) shl FSectorShiftCount; {Only Read 1 sector if Delta exists}
        
        {$IFDEF VIRTUAL_DEBUG}
        if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   Offset = ' + IntToHex(Offset,16));
        if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   Count = ' + IntToHex(Count,8));
        {$ENDIF}
        
        {Read Extent}
        FDriver.FileSeekEx(AExtent.Handle,Offset,soFromBeginning);
        if FDriver.FileRead(AExtent.Handle,ABuffer,Count) <> Integer(Count) then Exit;
        
        Result:=(Count shr FSectorShiftCount);
       end;
     end;
   end;
 finally  
  FExtents.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVpcImage.WriteExtent(AExtent:TVirtualDiskExtent;ASector:LongWord;ACount:Word;const ABuffer):Word;
{Note: Caller must hold the extent lock}
var
 Offset:Int64;         {Absolute Offset of Write to file}
 Count:LongWord;       {Count of Bytes to Write to file}
 Buffer:Pointer;       {Buffer for Dynamic and Differencing allocation}

 TableValue:Int64;     {The Block offset value read from the Table}
 TableOffset:LongWord; {The offset of the Blocks entry in the Table}

 BlockNo:LongWord;     {Block Number in the Table (0 to N - 1 blocks)}
 BlockSize:LongWord;   {Number of bytes in a Block (including Bitmap)}

 Table:TVirtualDiskVpcTable;
 Block:TVirtualDiskVpcBlock;
begin
 {}
 Result:=0;
 
 if not FExtents.ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if AExtent = nil then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.WriteExtent - Name = ' + AExtent.Filename);
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   Sector = ' + IntToStr(ASector) + ' Count = ' + IntToStr(ACount));
  {$ENDIF}
  
  {Check Write}
  if not Writeable then Exit;
  if ASector < AExtent.StartSector then Exit;
  if ASector >= (AExtent.StartSector + AExtent.SectorCount) then Exit;
  {if (ASector + ACount) > (AExtent.StartSector + AExtent.SectorCount) then Exit;} {Allow Write greater than SectorCount, return actual Count}

  {Check Fixed}
  if AExtent.IsFixed then
   begin
    {Get Offset}
    Offset:=((ASector - AExtent.StartSector) shl FSectorShiftCount);
    Count:=Min(ACount,(AExtent.SectorCount - (ASector - AExtent.StartSector))) shl FSectorShiftCount;
    
    {$IFDEF VIRTUAL_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   Offset = ' + IntToHex(Offset,16));
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   Count = ' + IntToHex(Count,8));
    {$ENDIF}
    
    {Write Extent}
    FDriver.FileSeekEx(AExtent.Handle,Offset,soFromBeginning);
    if FDriver.FileWrite(AExtent.Handle,ABuffer,Count) <> Integer(Count) then Exit;
    
    Result:=(Count shr FSectorShiftCount);
   end
  else
   begin
    {Get Table}
    Table:=TVirtualDiskVpcExtent(AExtent).Table;
    if Table = nil then Exit;
    if Table.Data = nil then Exit;
    
    {Get Block}
    BlockNo:=(ASector shr AExtent.BlockShiftCount);                                {Get the Block No}
    TableOffset:=(BlockNo shl 2);                                                  {Get the Table Offset (Multiply BlockNo by 4)}
    TableValue:=LongWordBEtoN(LongWord(Pointer(LongWord(Table.Data) + TableOffset)^));  {Get the Table Value}
    
    {$IFDEF VIRTUAL_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   BlockNo = ' + IntToHex(BlockNo,8));
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   TableOffset = ' + IntToHex(TableOffset,8));
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   TableValue = ' + IntToHex(TableValue,8));
    {$ENDIF}
    
    {Check Block}
    if TableValue = vpcUnallocatedBlock then
     begin
      {Get Block}
      Block:=TVirtualDiskVpcBlock(GetBlock(Table,ASector,True));                {Get the Block}
      if Block = nil then Exit;
      
      {Get Size}
      BlockSize:=AExtent.BlockSize + RoundToSector(Block.BlockSize,vpcSectorSize); {Block Size plus Bitmap Size}
      
      {Get Buffer}
      Buffer:=GetMem(BlockSize);
      if Buffer = nil then Exit;
      try
       {Allocate Block}
       TableValue:=(TVirtualDiskVpcExtent(AExtent).FooterOffset) shr FSectorShiftCount;
       
       {Extend File}
       ZeroMemory(Buffer,BlockSize);
       FDriver.FileSeekEx(AExtent.Handle,TVirtualDiskVpcExtent(AExtent).FooterOffset,soFromBeginning);
       if FDriver.FileWrite(AExtent.Handle,Buffer^,BlockSize) <> Integer(BlockSize) then Exit;
       
       {Update Block}
       Block.BlockOffset:=TVirtualDiskVpcExtent(AExtent).FooterOffset;
       Block.DataOffset:=Block.BlockOffset + RoundToSector(Block.BlockSize,vpcSectorSize);
       
       {Update Table}
       LongWord(Pointer(LongWord(Table.Data) + TableOffset)^):=LongWordNtoBE(TableValue);
       if not SetTable(Table) then Exit;
       
       {Update Extent}
       TVirtualDiskVpcExtent(AExtent).FooterOffset:=TVirtualDiskVpcExtent(AExtent).FooterOffset + BlockSize;
       if not SetExtent(AExtent) then Exit;
       
       {Get Offset}
       Offset:=Block.DataOffset + ((ASector - Block.StartSector) shl FSectorShiftCount);
       Count:=Min(ACount,(Block.SectorCount - (ASector - Block.StartSector))) shl FSectorShiftCount;
       
       {$IFDEF VIRTUAL_DEBUG}
       if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   Offset = ' + IntToHex(Offset,16));
       if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   Count = ' + IntToHex(Count,8));
       {$ENDIF}
       
       {Update Block}
       if not MarkBlock(Block,ASector,(Count shr FSectorShiftCount),True) then Exit;
       if not SetBlock(Block) then Exit;
       
       {Write Extent}
       FDriver.FileSeekEx(AExtent.Handle,Offset,soFromBeginning);
       if FDriver.FileWrite(AExtent.Handle,ABuffer,Count) <> Integer(Count) then Exit;
       
       Result:=(Count shr FSectorShiftCount);
      finally
       FreeMem(Buffer);
      end;
     end
    else
     begin
      {Get Block}
      Block:=TVirtualDiskVpcBlock(GetBlock(Table,ASector,True));                {Get the Block}
      if Block = nil then Exit;
      
      {Get Offset}
      Offset:=Block.DataOffset + ((ASector - Block.StartSector) shl FSectorShiftCount);
      Count:=Min(ACount,(Block.SectorCount - (ASector - Block.StartSector))) shl FSectorShiftCount;
      
      {$IFDEF VIRTUAL_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   Offset = ' + IntToHex(Offset,16));
      if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   Count = ' + IntToHex(Count,8));
      {$ENDIF}
      
      {Update Block}
      if not MarkBlock(Block,ASector,(Count shr FSectorShiftCount),True) then Exit;
      if not SetBlock(Block) then Exit;
      
      {Write Extent}
      FDriver.FileSeekEx(AExtent.Handle,Offset,soFromBeginning);
      if FDriver.FileWrite(AExtent.Handle,ABuffer,Count) <> Integer(Count) then Exit;
      
      Result:=(Count shr FSectorShiftCount);
     end;
   end;
 finally  
  FExtents.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVpcImage.LoadExtents:Boolean;
var
 WorkBuffer:String;
 Delta:TVirtualDiskVpcExtent;
 Extent:TVirtualDiskVpcExtent;
 Parent:TVirtualDiskVpcExtent;
begin
 {}
 Result:=False;
 
 if not FExtents.WriterLock then Exit;
 try
  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.LoadExtents');
  {$ENDIF}

  {Load Extent}
  Extent:=TVirtualDiskVpcExtent(LoadExtent(nil,nil,FName));
  if Extent = nil then Exit;
  try
   if Extent.IsBase then {Base Extent}
    begin
     {Set Base}
     FBase:=Extent;
     
     {Load Delta Extents}
     WorkBuffer:=LocateDelta(Extent);
     while Length(WorkBuffer) <> 0 do
      begin
       {Save Parent}
       Parent:=Extent;
       
       {Load Extent}
       Extent:=TVirtualDiskVpcExtent(LoadExtent(nil,Parent,WorkBuffer));
       if Extent = nil then Exit;
       
       WorkBuffer:=LocateDelta(Extent);
      end;
     
     {Set Current}
     FCurrent:=Extent; {Last Delta loaded must be Current (Will be same as Base if no Deltas loaded)}
    end
   else if Extent.IsDelta then {Delta Extent}
    begin
     {Save Delta}
     Delta:=Extent;
     
     {Locate Delta Extents}
     WorkBuffer:=LocateDelta(Extent);
     while Length(WorkBuffer) <> 0 do
      begin
       {Save Parent}
       Parent:=Extent;
       
       {Load Extent}
       Extent:=TVirtualDiskVpcExtent(LoadExtent(nil,Parent,WorkBuffer));
       if Extent = nil then Exit;
       
       WorkBuffer:=LocateDelta(Extent);
      end;
     
     {Set Current}
     FCurrent:=Extent; {Last Delta loaded must be Current}
     
     {Locate Parent Extents}
     WorkBuffer:=LocateParent(Delta);
     if Length(WorkBuffer) = 0 then Exit;
     while Length(WorkBuffer) <> 0 do
      begin
       {Load Extent}
       Extent:=TVirtualDiskVpcExtent(LoadExtent(Delta,nil,WorkBuffer));
       if Extent = nil then Exit;
       
       {Save Delta}
       Delta:=Extent;
       if Extent.IsBase then Break;
       
       WorkBuffer:=LocateParent(Delta);
      end;
     
     {Set Base}
     if Extent.IsBase then FBase:=Extent; {Last Parent loaded must be Base}
    end;
    
   {Check Loaded}
   if FBase = nil then Exit;
   if FCurrent = nil then Exit;
   
   Result:=True;

   {$IFDEF VIRTUAL_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.LoadExtents - Base    = ' + FBase.Filename);
   if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   Current = ' + FCurrent.Filename);
   if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   Count   = ' + IntToStr(FExtents.Count));
   {$ENDIF}
  finally
   if not Result then CloseExtents;
  end;
 finally  
  FExtents.WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVpcImage.CheckExtent(ADelta,AParent:TVirtualDiskExtent;const AFilename:String):Boolean;
{Check that the passed file is a valid VirtualPC extent}
{If delta is provided, check that passed file is the parent of the delta}
{If parent is provided, check that passed file is the delta of the parent}
{Note: Caller must hold the delta and parent locks}
var
 Handle:THandle;
 Header:TVpcHardDiskFooter;
 Footer:TVpcHardDiskFooter;
 Sparse:TVpcDynamicDiskHeader;
begin
 {}
 Result:=False;
 
 if FDriver = nil then Exit;
 if Length(AFilename) = 0 then Exit;

 {$IFDEF VIRTUAL_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.CheckExtent - Name = ' + AFilename);
 {$ENDIF}

 {Open File}
 if not FDriver.FileExists(AFilename) then Exit;
 Handle:=FDriver.FileOpen(AFilename,fmOpenRead or fmShareDenyNone);
 if Handle = INVALID_HANDLE_VALUE then Exit;
 try
  {Read Footer}
  if FDriver.FileSizeEx(Handle) < SizeOf(TVpcHardDiskFooter) then Exit;
  FDriver.FileSeekEx(Handle,0 - SizeOf(TVpcHardDiskFooter),soFromEnd);
  if FDriver.FileRead(Handle,Footer,SizeOf(TVpcHardDiskFooter)) <> SizeOf(TVpcHardDiskFooter) then Exit;
  
  {Check Cookie}
  if Footer.Cookie = vpcFooterCookie then
   begin
    {$IFDEF VIRTUAL_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.CheckExtent - Original Checksum = ' + IntToHex(LongWordBEtoN(Footer.Checksum),8));
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                                 Calculated Checksum = ' + IntToHex(ChecksumFooter(@Footer),8));
    {$ENDIF}
    
    {Check Footer}
    if LongWordBEtoN(Footer.Version) <> vpcFooterVersion then Exit;
    if LongWordBEtoN(Footer.Checksum) <> ChecksumFooter(@Footer) then Exit;
    
    {Check Delta}
    if ADelta <> nil then
     begin
      {$IFDEF VIRTUAL_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.CheckExtent - Delta = ' + GUIDToString(TVirtualDiskVpcExtent(ADelta).Footer.UniqueId));
      {$ENDIF}
      
      if not CompareGUID(Footer.UniqueId,TVirtualDiskVpcExtent(ADelta).Sparse.ParentUniqueId) then Exit;
     end;
    
    {Check Parent}
    if AParent <> nil then
     begin
      {$IFDEF VIRTUAL_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.CheckExtent - Parent = ' + GUIDToString(TVirtualDiskVpcExtent(AParent).Footer.UniqueId));
      {$ENDIF}
      
      if LongWordBEtoN(Footer.DiskType) <> vpcDiskTypeDifferencing then Exit;
      if Int64BEtoN(Footer.DataOffset) = vpcFixedDiskTableOffset then Exit;
      
      {Read Sparse}
      FDriver.FileSeekEx(Handle,Int64BEtoN(Footer.DataOffset),soFromBeginning);
      if FDriver.FileRead(Handle,Sparse,SizeOf(TVpcDynamicDiskHeader)) <> SizeOf(TVpcDynamicDiskHeader) then Exit;
      
      {Check Cookie}
      if Sparse.Cookie <> vpcDynamicCookie then Exit;
      
      {$IFDEF VIRTUAL_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.CheckExtent - Original Checksum = ' + IntToHex(LongWordBEtoN(Sparse.Checksum),8));
      if FILESYS_LOG_ENABLED then FileSysLogDebug('                                 Calculated Checksum = ' + IntToHex(ChecksumSparse(@Sparse),8));
      {$ENDIF}
      
      {Check Sparse}
      if LongWordBEtoN(Sparse.HeaderVersion) <> vpcDynamicVersion then Exit;
      if LongWordBEtoN(Sparse.Checksum) <> ChecksumSparse(@Sparse) then Exit;
      if not CompareGUID(Sparse.ParentUniqueId,TVirtualDiskVpcExtent(AParent).Footer.UniqueId) then Exit;
     end;
     
    Result:=True;
   end
  else
   begin
    {Read Header}
    FDriver.FileSeekEx(Handle,0,soFromBeginning);
    if FDriver.FileRead(Handle,Header,SizeOf(TVpcHardDiskFooter)) <> SizeOf(TVpcHardDiskFooter) then Exit;
    
    {Check Cookie}
    if Header.Cookie = vpcFooterCookie then
     begin
      {$IFDEF VIRTUAL_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.CheckExtent - Original Checksum = ' + IntToHex(LongWordBEtoN(Header.Checksum),8));
      if FILESYS_LOG_ENABLED then FileSysLogDebug('                                 Calculated Checksum = ' + IntToHex(ChecksumFooter(@Header),8));
      {$ENDIF}
      
      {Check Header}
      if LongWordBEtoN(Header.Version) <> vpcFooterVersion then Exit;
      if LongWordBEtoN(Header.Checksum) <> ChecksumFooter(@Header) then Exit;
      
      {Check Delta}
      if ADelta <> nil then
       begin
        {$IFDEF VIRTUAL_DEBUG}
        if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.CheckExtent - Delta = ' + GUIDToString(TVirtualDiskVpcExtent(ADelta).Footer.UniqueId));
        {$ENDIF}
        
        if not CompareGUID(Footer.UniqueId,TVirtualDiskVpcExtent(ADelta).Sparse.ParentUniqueId) then Exit;
       end;
       
      {Check Parent}
      if AParent <> nil then
       begin
        {$IFDEF VIRTUAL_DEBUG}
        if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.CheckExtent - Parent = ' + GUIDToString(TVirtualDiskVpcExtent(AParent).Footer.UniqueId));
        {$ENDIF}
        
        if LongWordBEtoN(Header.DiskType) <> vpcDiskTypeDifferencing then Exit;
        if Int64BEtoN(Header.DataOffset) = vpcFixedDiskTableOffset then Exit;
        
        {Read Sparse}
        FDriver.FileSeekEx(Handle,Int64BEtoN(Header.DataOffset),soFromBeginning);
        if FDriver.FileRead(Handle,Sparse,SizeOf(TVpcDynamicDiskHeader)) <> SizeOf(TVpcDynamicDiskHeader) then Exit;
        
        {Check Cookie}
        if Sparse.Cookie <> vpcDynamicCookie then Exit;
        
        {$IFDEF VIRTUAL_DEBUG}
        if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.CheckExtent - Original Checksum = ' + IntToHex(LongWordBEtoN(Sparse.Checksum),8));
        if FILESYS_LOG_ENABLED then FileSysLogDebug('                                 Calculated Checksum = ' + IntToHex(ChecksumSparse(@Sparse),8));
        {$ENDIF}
        
        {Check Sparse}
        if LongWordBEtoN(Sparse.HeaderVersion) <> vpcDynamicVersion then Exit;
        if LongWordBEtoN(Sparse.Checksum) <> ChecksumSparse(@Sparse) then Exit;
        if not CompareGUID(Sparse.ParentUniqueId,TVirtualDiskVpcExtent(AParent).Header.UniqueId) then Exit;
       end;
       
      Result:=True;
     end
    else
     begin
      //To Do //Look for Split files //Create Split file example to confirm behaviour
                    //Instead we could just fail the check and call CheckSplit
                    //which would set the Split Flag in the Image, LoadExtent(s) could then act on that Flag
     end;
   end;
 finally
  {Close File}
  FDriver.FileClose(Handle);
 end;
end;

{==============================================================================}

function TVirtualDiskVpcImage.LoadExtent(ADelta,AParent:TVirtualDiskExtent;const AFilename:String):TVirtualDiskExtent;
var
 Mode:Integer;
 FileSize:Int64;
 Extent:TVirtualDiskExtent;
begin
 {}
 Result:=nil;
 
 if not FExtents.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if Length(AFilename) = 0 then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.LoadExtent - Name = ' + AFilename);
  if ADelta <> nil then if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   Delta = ' + GUIDToString(TVirtualDiskVpcExtent(ADelta).Footer.UniqueId));
  if AParent <> nil then if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   Parent = ' + GUIDToString(TVirtualDiskVpcExtent(AParent).Footer.UniqueId));
  {$ENDIF}

  {Create Extent}
  Extent:=TVirtualDiskVpcExtent.Create(Self,ADelta,AParent);
  try
   {Open File}
   if not FDriver.FileExists(AFilename) then Exit;
   Extent.Filename:=AFilename;
   Mode:=fmOpenRead or fmShareDenyNone;
   if Writeable then Mode:=fmOpenReadWrite or fmShareDenyWrite;
   Extent.Handle:=FDriver.FileOpen(Extent.Filename,Mode);
   if Extent.Handle = INVALID_HANDLE_VALUE then Exit;
   try
    {Get Size}
    FileSize:=FDriver.FileSizeEx(Extent.Handle);
    if FileSize < SizeOf(TVpcHardDiskFooter) then Exit;
    
    {Read Footer}
    FDriver.FileSeekEx(Extent.Handle,FileSize - SizeOf(TVpcHardDiskFooter),soFromBeginning);
    if FDriver.FileRead(Extent.Handle,TVirtualDiskVpcExtent(Extent).Footer^,SizeOf(TVpcHardDiskFooter)) <> SizeOf(TVpcHardDiskFooter) then Exit;
    
    {Check Footer}
    if LongWordBEtoN(TVirtualDiskVpcExtent(Extent).Footer.Version) <> vpcFooterVersion then Exit;
    if LongWordBEtoN(TVirtualDiskVpcExtent(Extent).Footer.Checksum) <> ChecksumFooter(TVirtualDiskVpcExtent(Extent).Footer) then Exit;
    if (Int64BEtoN(TVirtualDiskVpcExtent(Extent).Footer.CurrentSize) mod vpcSectorSize) <> 0 then Exit;
    
    {Update Flags}
    Extent.Flags:=virtualFlagNone;
    if LongWordBEtoN(TVirtualDiskVpcExtent(Extent).Footer.DiskType) = vpcDiskTypeFixed then Extent.Flags:=Extent.Flags or virtualFlagBase;
    if LongWordBEtoN(TVirtualDiskVpcExtent(Extent).Footer.DiskType) = vpcDiskTypeDynamic then Extent.Flags:=Extent.Flags or virtualFlagBase;
    if LongWordBEtoN(TVirtualDiskVpcExtent(Extent).Footer.DiskType) = vpcDiskTypeFixed then Extent.Flags:=Extent.Flags or virtualFlagFixed;
    if LongWordBEtoN(TVirtualDiskVpcExtent(Extent).Footer.DiskType) = vpcDiskTypeDynamic then Extent.Flags:=Extent.Flags or virtualFlagDynamic;
    if LongWordBEtoN(TVirtualDiskVpcExtent(Extent).Footer.DiskType) = vpcDiskTypeDifferencing then Extent.Flags:=Extent.Flags or virtualFlagDelta;
    
    {Check Fixed}
    if Extent.IsFixed then
     begin
      {Fixed}
      {Zero Header}
      ZeroMemory(TVirtualDiskVpcExtent(Extent).Header,SizeOf(TVpcHardDiskFooter));
      
      {Zero Sparse}
      ZeroMemory(TVirtualDiskVpcExtent(Extent).Sparse,SizeOf(TVpcDynamicDiskHeader));
      
      {Update Extent}
      Extent.DataOffset:=0;  {Fixed Disk data starts at the beginning of the file}
      Extent.BlockSize:=0;   {No tables or blocks in a Fixed Disk}
      Extent.StartSector:=0;
      Extent.SectorCount:=(Int64BEtoN(TVirtualDiskVpcExtent(Extent).Footer.CurrentSize) div vpcSectorSize);
      Extent.BlockShiftCount:=0;
      TVirtualDiskVpcExtent(Extent).HeaderOffset:=0;
      TVirtualDiskVpcExtent(Extent).HeaderSize:=0;
      TVirtualDiskVpcExtent(Extent).FooterOffset:=FileSize - SizeOf(TVpcHardDiskFooter);
      TVirtualDiskVpcExtent(Extent).FooterSize:=SizeOf(TVpcHardDiskFooter);
      TVirtualDiskVpcExtent(Extent).SparseOffset:=0;
      TVirtualDiskVpcExtent(Extent).SparseSize:=0;
     end
    else
     begin
      {Dynamic/Delta}
      {Read Header}
      FDriver.FileSeekEx(Extent.Handle,0,soFromBeginning);
      if FDriver.FileRead(Extent.Handle,TVirtualDiskVpcExtent(Extent).Header^,SizeOf(TVpcHardDiskFooter)) <> SizeOf(TVpcHardDiskFooter) then Exit;
      
      {Check Header}
      if LongWordBEtoN(TVirtualDiskVpcExtent(Extent).Header.Version) <> vpcFooterVersion then Exit;
      if LongWordBEtoN(TVirtualDiskVpcExtent(Extent).Header.Checksum) <> ChecksumFooter(TVirtualDiskVpcExtent(Extent).Header) then Exit;
      if (Int64BEtoN(TVirtualDiskVpcExtent(Extent).Header.CurrentSize) mod vpcSectorSize) <> 0 then Exit;
      if Int64BEtoN(TVirtualDiskVpcExtent(Extent).Header.DataOffset) = vpcFixedDiskTableOffset then Exit;
      
      {Read Sparse}
      FDriver.FileSeekEx(Extent.Handle,Int64BEtoN(TVirtualDiskVpcExtent(Extent).Header.DataOffset),soFromBeginning);
      if FDriver.FileRead(Extent.Handle,TVirtualDiskVpcExtent(Extent).Sparse^,SizeOf(TVpcDynamicDiskHeader)) <> SizeOf(TVpcDynamicDiskHeader) then Exit;
      
      {Check Sparse}
      if LongWordBEtoN(TVirtualDiskVpcExtent(Extent).Sparse.HeaderVersion) <> vpcDynamicVersion then Exit;
      if LongWordBEtoN(TVirtualDiskVpcExtent(Extent).Sparse.Checksum) <> ChecksumSparse(TVirtualDiskVpcExtent(Extent).Sparse) then Exit;
      if (LongWordBEtoN(TVirtualDiskVpcExtent(Extent).Sparse.BlockSize) mod vpcSectorSize) <> 0 then Exit;
      if Int64BEtoN(TVirtualDiskVpcExtent(Extent).Sparse.DataOffset) <> vpcDynamicDiskDataOffset then Exit;
      
      {Update Extent}
      Extent.DataOffset:=RoundToSector(LongWordBEtoN(TVirtualDiskVpcExtent(Extent).Sparse.MaxTableEntries) shl 2,vpcSectorSize) + SizeOf(TVpcHardDiskFooter) + SizeOf(TVpcDynamicDiskHeader);
      Extent.BlockSize:=LongWordBEtoN(TVirtualDiskVpcExtent(Extent).Sparse.BlockSize);
      Extent.StartSector:=0;
      Extent.SectorCount:=(Int64BEtoN(TVirtualDiskVpcExtent(Extent).Footer.CurrentSize) div vpcSectorSize);
      Extent.BlockShiftCount:=Extent.GetBlockShiftCount;
      TVirtualDiskVpcExtent(Extent).HeaderOffset:=0;
      TVirtualDiskVpcExtent(Extent).HeaderSize:=SizeOf(TVpcHardDiskFooter);
      TVirtualDiskVpcExtent(Extent).FooterOffset:=FileSize - SizeOf(TVpcHardDiskFooter);
      TVirtualDiskVpcExtent(Extent).FooterSize:=SizeOf(TVpcHardDiskFooter);
      TVirtualDiskVpcExtent(Extent).SparseOffset:=Int64BetoN(TVirtualDiskVpcExtent(Extent).Header.DataOffset);
      TVirtualDiskVpcExtent(Extent).SparseSize:=SizeOf(TVpcDynamicDiskHeader);
     end;
    
    {Check Extent}
    if Extent.SectorCount = 0 then Exit;
    
    {Update Delta}
    if ADelta <> nil then ADelta.Parent:=Extent;
    
    {Update Parent}
    if AParent <> nil then AParent.Delta:=Extent;
    
    {$IFDEF VIRTUAL_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.LoadExtent - DataOffset = ' + IntToStr(Extent.DataOffset) + ' (May be invalid)');
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                                  BlockSize = ' + IntToStr(Extent.BlockSize));
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                                  StartSector = ' + IntToStr(Extent.StartSector));
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                                  SectorCount = ' + IntToStr(Extent.SectorCount));
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                                  BlockShiftCount = ' + IntToStr(Extent.BlockShiftCount));
    {Note: Do not use the Extent.DataOffset value in VPC images, use Block.DataOffset}
    {$ENDIF}
    
    {Add Extent}
    FExtents.Add(Extent);
    
    Result:=Extent;
   finally
    if Result = nil then FDriver.FileClose(Extent.Handle);
   end;
  finally
   if Result = nil then Extent.Free;
  end;
 finally  
  FExtents.WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVpcImage.AddExtent(AParent:TVirtualDiskExtent;const AFilename:String):TVirtualDiskExtent;
{Note: Caller must hold the parent lock}
begin
 {}
 Result:=nil;
 
 if not FExtents.WriterLock then Exit;
 try
  if FDriver = nil then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.AddExtent - Name = ' + AFilename);
  {$ENDIF}

  //To Do //Create a new Extent //Required for Create Image

 finally  
  FExtents.WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVpcImage.SetExtent(AExtent:TVirtualDiskExtent):Boolean;
{Note: Caller must hold the extent lock}
begin
 {}
 Result:=False;
 
 if not FExtents.ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if AExtent = nil then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.SetExtent - Name = ' + AExtent.Filename);
  {$ENDIF}

  {Check Extent}
  if AExtent.Handle = INVALID_HANDLE_VALUE then Exit;

  {Check Fixed}
  if AExtent.IsFixed then
   begin
    {Get Checksum}
    TVirtualDiskVpcExtent(AExtent).Footer.Checksum:=LongWordNtoBE(ChecksumFooter(TVirtualDiskVpcExtent(AExtent).Footer));
    
    {Write Footer}
    FDriver.FileSeekEx(AExtent.Handle,TVirtualDiskVpcExtent(AExtent).FooterOffset,soFromBeginning);
    if FDriver.FileWrite(AExtent.Handle,TVirtualDiskVpcExtent(AExtent).Footer^,TVirtualDiskVpcExtent(AExtent).FooterSize) <> Integer(TVirtualDiskVpcExtent(AExtent).FooterSize) then Exit;

    Result:=True;
   end
  else
   begin
    {Get Checksum}
    TVirtualDiskVpcExtent(AExtent).Header.Checksum:=LongWordNtoBE(ChecksumFooter(TVirtualDiskVpcExtent(AExtent).Header));
    
    {Write Header}
    FDriver.FileSeekEx(AExtent.Handle,TVirtualDiskVpcExtent(AExtent).HeaderOffset,soFromBeginning);
    if FDriver.FileWrite(AExtent.Handle,TVirtualDiskVpcExtent(AExtent).Header^,TVirtualDiskVpcExtent(AExtent).HeaderSize) <> Integer(TVirtualDiskVpcExtent(AExtent).HeaderSize) then Exit;

    {Get Checksum}
    TVirtualDiskVpcExtent(AExtent).Footer.Checksum:=LongWordNtoBE(ChecksumFooter(TVirtualDiskVpcExtent(AExtent).Footer));
    
    {Write Footer}
    FDriver.FileSeekEx(AExtent.Handle,TVirtualDiskVpcExtent(AExtent).FooterOffset,soFromBeginning);
    if FDriver.FileWrite(AExtent.Handle,TVirtualDiskVpcExtent(AExtent).Footer^,TVirtualDiskVpcExtent(AExtent).FooterSize) <> Integer(TVirtualDiskVpcExtent(AExtent).FooterSize) then Exit;

    {Get Checksum}
    TVirtualDiskVpcExtent(AExtent).Sparse.Checksum:=LongWordNtoBE(ChecksumSparse(TVirtualDiskVpcExtent(AExtent).Sparse));
    
    {Write Sparse}
    FDriver.FileSeekEx(AExtent.Handle,TVirtualDiskVpcExtent(AExtent).SparseOffset,soFromBeginning);
    if FDriver.FileWrite(AExtent.Handle,TVirtualDiskVpcExtent(AExtent).Sparse^,TVirtualDiskVpcExtent(AExtent).SparseSize) <> Integer(TVirtualDiskVpcExtent(AExtent).SparseSize) then Exit;

    Result:=True;
   end;
 finally  
  FExtents.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVpcImage.GetExtent(const ASector:Int64;AWrite,ALock:Boolean):TVirtualDiskExtent;
var
 BlockNo:LongWord;
 TableOffset:LongWord;

 Block:TVirtualDiskBlock;
 Extent:TVirtualDiskExtent;
begin
 {}
 Result:=nil;
 
 if not FExtents.ReaderLock then Exit;
 try
  if ASector >= FSectorCount then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.GetExtent - Sector = ' + IntToStr(ASector) + ' Write = ' + BoolToStr(AWrite));
  {$ENDIF}

  {Check Write}
  if AWrite then
   begin
    Extent:=FCurrent;
    if Extent <> nil then
     begin
      if (ASector >= Extent.StartSector) and (ASector < (Extent.StartSector + Extent.SectorCount)) then
       begin
        if ALock then Extent.AcquireLock;
        
        Result:=Extent;
       end;
     end;
   end
  else
   begin
    {Get Extent}
    Extent:=FCurrent;
    while Extent <> nil do
     begin
      if (ASector >= Extent.StartSector) and (ASector < (Extent.StartSector + Extent.SectorCount)) then
       begin
        {Check Fixed}
        if Extent.IsFixed then
         begin
          if ALock then Extent.AcquireLock;
          
          Result:=Extent;
          Exit;
         end
        else if Extent.IsDynamic then
         begin
          if TVirtualDiskVpcExtent(Extent).Table = nil then Exit;
          if TVirtualDiskVpcExtent(Extent).Table.Data = nil then Exit;
          
          if ALock then Extent.AcquireLock;
          
          Result:=Extent;
          Exit;
         end
        else if Extent.IsDelta then
         begin
          if TVirtualDiskVpcExtent(Extent).Table = nil then Exit;
          if TVirtualDiskVpcExtent(Extent).Table.Data = nil then Exit;
          
          {Get Block}
          BlockNo:=(ASector shr Extent.BlockShiftCount);
          TableOffset:=(BlockNo shl 2);
          
          {Check Block (Block is Allocated)}
          if LongWordBEtoN(LongWord(Pointer(LongWord(TVirtualDiskVpcExtent(Extent).Table.Data) + TableOffset)^)) <> vpcUnallocatedBlock then
           begin
            Block:=GetBlock(TVirtualDiskVpcExtent(Extent).Table,ASector,AWrite);
            if Block = nil then Exit;
            
            {Test Block (One or More Sectors Allocated)}
            if TestBlock(Block,ASector,1,True) > 0 then
             begin
              if ALock then Extent.AcquireLock;
              
              Result:=Extent;
              Exit;
             end;
           end;
         end;
       end;
       
      Extent:=Extent.Parent;
     end;
   end;
 finally  
  FExtents.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVpcImage.LoadTables:Boolean;
var
 Extent:TVirtualDiskExtent;
begin
 {}
 Result:=False;
 
 if not FExtents.WriterLock then Exit;
 try
  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.LoadTables');
  {$ENDIF}

  {Load Tables}
  Extent:=TVirtualDiskExtent(FExtents.First);
  while Extent <> nil do
   begin
    if LoadTable(Extent,0) = nil then Exit;
    
    Extent:=TVirtualDiskExtent(Extent.Next);
   end;

  Result:=True;
 finally  
  FExtents.WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVpcImage.LoadTable(AExtent:TVirtualDiskExtent;ATableNo:LongWord):TVirtualDiskTable;
{Note: Caller must hold the extent tables writer lock}
var
 Data:Pointer;
 Table:TVirtualDiskVpcTable;
begin
 {}
 Result:=nil;
 
 if not FExtents.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if AExtent = nil then Exit;
  if ATableNo <> 0 then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.LoadTable - Extent = ' + AExtent.Filename + ' TableNo = ' + IntToStr(ATableNo));
  {$ENDIF}

  {Get Table}
  Table:=TVirtualDiskVpcExtent(AExtent).Table;
  
  {Check Fixed}
  if AExtent.IsFixed then
   begin
    Table.Data:=nil;
    
    Result:=Table;
   end
  else
   begin
    {$IFDEF VIRTUAL_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                                 TableOffset = ' + IntToStr(Int64BEtoN(TVirtualDiskVpcExtent(AExtent).Sparse.TableOffset)));
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                                 MaxTableEntries = ' + IntToStr(LongWordBEtoN(TVirtualDiskVpcExtent(AExtent).Sparse.MaxTableEntries)));
    {$ENDIF}
    
    {Update Table}
    Table.TableOffset:=Int64BEtoN(TVirtualDiskVpcExtent(AExtent).Sparse.TableOffset);
    Table.TableSize:=RoundToSector(LongWordBEtoN(TVirtualDiskVpcExtent(AExtent).Sparse.MaxTableEntries) shl 2,vpcSectorSize);
    Table.StartSector:=TVirtualDiskVpcExtent(AExtent).StartSector;
    Table.SectorCount:=TVirtualDiskVpcExtent(AExtent).SectorCount;
    
    {Check Table}
    if Table.TableSize = 0 then Exit;
    
    {Alloc Table}
    Data:=GetMem(Table.TableSize);
    if Data = nil then Exit;
    try
     {Load Table}
     FDriver.FileSeekEx(AExtent.Handle,Table.TableOffset,soFromBeginning);
     if FDriver.FileRead(AExtent.Handle,Data^,Table.TableSize) <> Integer(Table.TableSize) then Exit;
     
     {$IFDEF VIRTUAL_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.LoadTable - TableSize = ' + IntToStr(Table.TableSize));
     {$ENDIF}
     
     Table.Data:=Data;
     
     {Load Blocks}
     if not LoadBlocks(Table) then Exit;
     
     Result:=Table;
    finally
     if Result = nil then Table.Data:=nil;
     if Result = nil then FreeMem(Data);
    end;
   end;
 finally  
  FExtents.WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVpcImage.AddTable(AExtent:TVirtualDiskExtent):TVirtualDiskTable;
{Note: Caller must hold the extent lock}
begin
 {}
 Result:=nil;
 
 if not FExtents.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if AExtent = nil then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.AddTable - Extent = ' + AExtent.Filename);
  {$ENDIF}

  //To Do //Create a new Table //Required for Create Image

 finally  
  FExtents.WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVpcImage.SetTable(ATable:TVirtualDiskTable):Boolean;
{Note: Caller must hold the table extent lock}
begin
 {}
 Result:=False;
 
 if not FExtents.ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if ATable = nil then Exit;
  if ATable.Extent = nil then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.SetTable - TableNo = 0');
  {$ENDIF}

  {Check Fixed}
  if ATable.Extent.IsFixed then
   begin
    Result:=True;
   end
  else
   begin
    {Check Table}
    if TVirtualDiskVpcTable(ATable).Data = nil then Exit;

    {Write Table}
    FDriver.FileSeekEx(ATable.Extent.Handle,ATable.TableOffset,soFromBeginning);
    if FDriver.FileWrite(ATable.Extent.Handle,TVirtualDiskVpcTable(ATable).Data^,ATable.TableSize) <> Integer(ATable.TableSize) then Exit;

    Result:=True;
   end;
 finally  
  FExtents.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVpcImage.LoadBlocks(ATable:TVirtualDiskTable):Boolean;
{Note: Caller must hold the extent tables writer lock}
var
 GroupNo:LongWord;
 BlockNo:LongWord;
 Group:TVirtualDiskVpcGroup;
 Block:TVirtualDiskVpcBlock;
begin
 {}
 Result:=False;
 
 if not FExtents.WriterLock then Exit;
 try
  if ATable = nil then Exit;
  if ATable.Extent = nil then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.LoadBlocks - TableNo = 0');
  {$ENDIF}

  {Check Fixed}
  if ATable.Extent.IsFixed then
   begin
    Result:=True;
   end
  else
   begin
    {Check Loaded}
    if TVirtualDiskVpcExtent(ATable.Extent).Groups.Count > 0 then Exit;
    
    {Load Groups}
    GroupNo:=0;
    while GroupNo <= vpcMaxBlockGroup do {Groups 0 to 255}
     begin
      Group:=TVirtualDiskVpcGroup.Create(Self,ATable);
      Group.GroupNo:=GroupNo;
      TVirtualDiskVpcExtent(ATable.Extent).Groups.Add(Group);
      
      {Load Blocks}
      BlockNo:=GroupNo;
      while BlockNo < LongWordBEtoN(TVirtualDiskVpcExtent(ATable.Extent).Sparse.MaxTableEntries) do
       begin
        Block:=TVirtualDiskVpcBlock(LoadBlock(ATable,BlockNo));
        if Block = nil then Exit;
        
        Group.Blocks.Add(Block);
        
        Inc(BlockNo,vpcBlockGroupOffset); {Increment by 256}
       end;
       
      Inc(GroupNo);
     end;
     
    Result:=True;
   end;
 finally  
  FExtents.WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVpcImage.LoadBlock(ATable:TVirtualDiskTable;ABlockNo:LongWord):TVirtualDiskBlock;
{Note: Caller must hold the extent tables writer lock}
var
 Data:Pointer;
 TableOffset:LongWord;
 Block:TVirtualDiskVpcBlock;
begin
 {}
 Result:=nil;
 
 if not FExtents.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ATable = nil then Exit;
  if ATable.Extent = nil then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.LoadBlock - BlockNo = ' + IntToStr(ABlockNo));
  {$ENDIF}

  {Check Fixed}
  if ATable.Extent.IsFixed then
   begin
    Result:=nil;
   end
  else
   begin
    Block:=TVirtualDiskVpcBlock.Create(Self,ATable);
    try
     {Check Table}
     if TVirtualDiskVpcTable(ATable).Data = nil then Exit;
     
     {Get Offset}
     TableOffset:=(ABlockNo shl 2);
     Block.BlockNo:=ABlockNo;
     
     {Check Block (Block is Allocated)}
     if LongWordBEtoN(LongWord(Pointer(LongWord(TVirtualDiskVpcTable(ATable).Data) + TableOffset)^)) <> vpcUnallocatedBlock then
      begin
       {Update Block}
       Block.BlockOffset:=LongWordBEtoN(LongWord(Pointer(LongWord(TVirtualDiskVpcTable(ATable).Data) + TableOffset)^)) shl FSectorShiftCount;
       Block.BlockSize:=(LongWordBEtoN(TVirtualDiskVpcExtent(ATable.Extent).Sparse.BlockSize) shr FSectorShiftCount) shr 3; {BlockSize div SectorSize div 8 (Bits per Byte = 8)}
       Block.DataOffset:=Block.BlockOffset + RoundToSector(Block.BlockSize,vpcSectorSize);
      end
     else
      begin
       {Update Block}
       Block.BlockOffset:=0;
       Block.BlockSize:=(LongWordBEtoN(TVirtualDiskVpcExtent(ATable.Extent).Sparse.BlockSize) shr FSectorShiftCount) shr 3; {BlockSize div SectorSize div 8 (Bits per Byte = 8)}
       Block.DataOffset:=0;
      end;
     
     Block.StartSector:=ABlockNo shl ATable.Extent.BlockShiftCount;
     Block.SectorCount:=LongWordBEtoN(TVirtualDiskVpcExtent(ATable.Extent).Sparse.BlockSize) shr FSectorShiftCount;
     if (ATable.Extent.SectorCount - Block.StartSector) < Block.SectorCount then Block.SectorCount:=(ATable.Extent.SectorCount - Block.StartSector);
     
     {Check Block}
     if Block.BlockSize = 0 then Exit;
     
     {Alloc Block}
     Data:=GetMem(Block.BlockSize);
     if Data = nil then Exit;
     try
      {Load Block}
      if Block.BlockOffset = 0 then
       begin
        ZeroMemory(Data,Block.BlockSize);
       end
      else
       begin
        FDriver.FileSeekEx(ATable.Extent.Handle,Block.BlockOffset,soFromBeginning);
        if FDriver.FileRead(ATable.Extent.Handle,Data^,Block.BlockSize) <> Integer(Block.BlockSize) then Exit;
       end;
      
      {$IFDEF VIRTUAL_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.LoadBlock - BlockSize = ' + IntToStr(Block.BlockSize));
      if FILESYS_LOG_ENABLED then FileSysLogDebug('                                 BlockOffset = ' + IntToStr(Block.BlockOffset));
      if FILESYS_LOG_ENABLED then FileSysLogDebug('                                 DataOffset = ' + IntToStr(Block.DataOffset));
      if FILESYS_LOG_ENABLED then FileSysLogDebug('                                 StartSector = ' + IntToStr(Block.StartSector));
      if FILESYS_LOG_ENABLED then FileSysLogDebug('                                 SectorCount = ' + IntToStr(Block.SectorCount));
      {$ENDIF}
      
      Block.Data:=Data;
      
      Result:=Block;
     finally
      if Result = nil then Block.Data:=nil;
      if Result = nil then FreeMem(Data);
     end;
    finally
     if Result = nil then Block.Free;
    end;
   end;
 finally  
  FExtents.WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVpcImage.AddBlock(ATable:TVirtualDiskTable):TVirtualDiskBlock;
{Note: Caller must hold the table extent lock}
begin
 {}
 Result:=nil;
 
 if not FExtents.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if ATable = nil then Exit;
  if ATable.Extent = nil then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.AddBlock - TableNo = 0');
  {$ENDIF}

  //To Do //Create a new Table //Required for Create Image

 finally  
  FExtents.WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVpcImage.SetBlock(ABlock:TVirtualDiskBlock):Boolean;
{Note: Caller must hold the block table extent lock}
begin
 {}
 Result:=False;
 
 if not FExtents.ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if ABlock = nil then Exit;
  if ABlock.Table = nil then Exit;
  if ABlock.Table.Extent = nil then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.SetBlock - BlockNo = ' + IntToStr(TVirtualDiskVpcBlock(ABlock).BlockNo));
  {$ENDIF}

  {Check Fixed}
  if ABlock.Table.Extent.IsFixed then
   begin
    Result:=True;
   end
  else
   begin
    {Check Block}
    if TVirtualDiskVpcBlock(ABlock).Data = nil then Exit;

    {Write Block}
    if ABlock.BlockOffset <> 0 then
     begin
      FDriver.FileSeekEx(ABlock.Table.Extent.Handle,ABlock.BlockOffset,soFromBeginning);
      if FDriver.FileWrite(ABlock.Table.Extent.Handle,TVirtualDiskVpcBlock(ABlock).Data^,ABlock.BlockSize) <> Integer(ABlock.BlockSize) then Exit;
     end;

    Result:=True;
   end;
 finally  
  FExtents.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVpcImage.GetBlock(ATable:TVirtualDiskTable;const ASector:Int64;AWrite:Boolean):TVirtualDiskBlock;
{The Write parameter is ignored for Block lookups}
{Note: Caller must hold the table extent lock}
var
 GroupNo:LongWord;
 BlockNo:LongWord;

 Group:TVirtualDiskVpcGroup;
 Block:TVirtualDiskVpcBlock;
begin
 {}
 Result:=nil;
 
 if not FExtents.ReaderLock then Exit;
 try
  if ATable = nil then Exit;
  if ATable.Extent = nil then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.GetBlock - Sector = ' + IntToStr(ASector) + ' Write = ' + BoolToStr(AWrite));
  {$ENDIF}

  {Get Block}
  BlockNo:=(ASector shr ATable.Extent.BlockShiftCount);
  
  {Get Group}
  GroupNo:=BlockNo and vpcBlockGroupMask;
  
  {Find Group}
  Group:=TVirtualDiskVpcGroup(TVirtualDiskVpcExtent(ATable.Extent).Groups.First);
  while Group <> nil do
   begin
    if Group.GroupNo = GroupNo then
     begin
      Block:=TVirtualDiskVpcBlock(Group.Blocks.First);
      while Block <> nil do
       begin
        if Block.BlockNo = BlockNo then
         begin
          Result:=Block;
          Exit;
         end;
        
        Block:=TVirtualDiskVpcBlock(Block.Next);
       end;
      Exit;
     end;
     
    Group:=TVirtualDiskVpcGroup(Group.Next);
   end;
 finally  
  FExtents.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVpcImage.TestBlock(ABlock:TVirtualDiskBlock;const ASector:Int64;ACount:LongWord;AUsed:Boolean):LongWord;
{Test Count sectors from Sector in the Block bitmap for Free or Used}
{Sector is the sector number in the block to start from}
{Sector must be greater than or equal to block start sector}

{Note: Caller must hold the block table extent lock}
var
 Start:LongWord;
begin
 {}
 Result:=0;
 
 if not FExtents.ReaderLock then Exit;
 try
  if ABlock = nil then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.TestBlock - BlockNo = ' + IntToStr(TVirtualDiskVpcBlock(ABlock).BlockNo) + ' Sector = ' + IntToStr(ASector) + ' Count = ' + IntToStr(ACount) + ' Used = ' + BoolToStr(AUsed));
  {$ENDIF}
  
  {Check Sector}
  if ASector < ABlock.StartSector then Exit;
  if ASector >= (ABlock.StartSector + ABlock.SectorCount) then Exit;
  
  {Get Start} {Block Sector Count must not exceed 32 bit value}
  Start:=(ASector - ABlock.StartSector);
  
  {Test Bitmap}
  Result:=TestBitmap(TVirtualDiskVpcBlock(ABlock).Data,ABlock.SectorCount,Start,ACount,AUsed);
 finally  
  FExtents.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVpcImage.MarkBlock(ABlock:TVirtualDiskBlock;const ASector:Int64;ACount:LongWord;AUsed:Boolean):Boolean;
{Mark Count sectors from Sector in the Block bitmap as Free or Used}
{Sector is the sector number in the block to start from}
{Sector must be greater than or equal to block start sector}

{Note: Caller must hold the block table extent lock}
var
 Start:LongWord;
begin
 {}
 Result:=False;
 
 if not FExtents.ReaderLock then Exit;
 try
  if ABlock = nil then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.MarkBlock - BlockNo = ' + IntToStr(TVirtualDiskVpcBlock(ABlock).BlockNo) + ' Sector = ' + IntToStr(ASector) + ' Count = ' + IntToStr(ACount) + ' Used = ' + BoolToStr(AUsed));
  {$ENDIF}
  
  {Check Sector}
  if ASector < ABlock.StartSector then Exit;
  if ASector >= (ABlock.StartSector + ABlock.SectorCount) then Exit;
  
  {Check Count}
  if ACount = 0 then Exit;
  if ACount > ABlock.SectorCount then Exit;
  if (ASector + ACount) > (ABlock.StartSector + ABlock.SectorCount) then Exit;
  
  {Get Start} {Block Sector Count must not exceed 32 bit value}
  Start:=(ASector - ABlock.StartSector);
  
  {Mark Bitmap}
  Result:=MarkBitmap(TVirtualDiskVpcBlock(ABlock).Data,ABlock.SectorCount,Start,ACount,AUsed);
 finally  
  FExtents.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVpcImage.TestBitmap(ABuffer:Pointer;ASize,AStart,ACount:LongWord;AUsed:Boolean):LongWord;
{Test Count bits from Start in the Block bitmap for Free or Used}
{Size is the total number of bits in the bitmap}
{Start is the bit number in the bitmap to start from}
{Count is the number of bits in the bitmap to be tested}
{Note: Should only be called by TestBlock}
{Note: Bitmaps use 32 bit blocks in big endian order}
var
 Size:LongWord;   {Number of 32 bit blocks in bitmap}
 Start:LongWord;  {Starting block in the bitmap data}
 Block:LongWord;  {Current block in the bitmap data}
 Offset:LongWord; {Current offset into the bitmap data}

 Bit:LongWord;    {Starting bit to test in current block (0 if first bit)}
 Bits:LongWord;   {Number of bits to test in current block (32 if all bits)}
 Remain:LongWord; {Number of bits remaining to be tested}
 Current:LongWord;{Current bit to test in current block}
begin
 {}
 Result:=0;
 
 if ABuffer = nil then Exit;

 {$IFDEF VIRTUAL_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.TestBitmap - Size = ' + IntToStr(ASize) + ' Start = ' + IntToStr(AStart) + ' Count = ' + IntToStr(ACount) + ' Used = ' + BoolToStr(AUsed));
 {$ENDIF}

 {Get Size}
 Size:=(ASize shr 5); {Divide by 32}
 if (Size shl 5) < ASize then Inc(Size);
 
 {Get Start}
 Start:=(AStart shr 5); {Divide by 32}
 
 {Get Params}
 Block:=Start;
 Offset:=(Block shl 2); {Multiply by 4}
 Bit:=AStart - ((AStart shr 5) shl 5); {Divide by 32 / Multiply by 32} {If less than 32 then will subtract 0 from start}
 Remain:=(ASize - AStart);
 while Block < Size do
  begin
   {Get Bits}
   Bits:=Min(Remain,vpcBitmapMaskBits);
   if Bit > 0 then Bits:=Min(Remain,(vpcBitmapMaskBits - Bit));
   
   {$IFDEF VIRTUAL_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.TestBitmap - Block = ' + IntToStr(Block) + ' Offset = ' + IntToStr(Offset) + ' Remain = ' + IntToStr(Remain) + ' Bit = ' + IntToStr(Bit) + ' Bits = ' + IntToStr(Bits));
   {$ENDIF}
   
   {Test Bits}
   if (Bit = 0) and (LongWord(Pointer(LongWord(ABuffer) + Offset)^) = vpcBitmapMaskNone) then
    begin
     {All Free}
     if AUsed then Exit;
     Inc(Result,32);
    end
   else if (Bit = 0) and (LongWord(Pointer(LongWord(ABuffer) + Offset)^) = vpcBitmapMaskAll) then
    begin
     {All Used}
     if not AUsed then Exit;
     Inc(Result,32);
    end
   else
    begin
     {Used and Free}
     for Current:=Bit to (Bit + (Bits - 1)) do
      begin
       if AUsed then
        begin
         {Test Used}
         if (LongWord(Pointer(LongWord(ABuffer) + Offset)^) and vpcBitmapMasks[Current]) = vpcBitmapMaskNone then Exit;
         Inc(Result);
        end
       else
        begin
         {Test Free}
         if (LongWord(Pointer(LongWord(ABuffer) + Offset)^) and vpcBitmapMasks[Current]) = vpcBitmapMasks[Current] then Exit;
         Inc(Result);
        end;
      end;
    end;
    
   {Update Params}
   Bit:=0;
   Inc(Block);
   Inc(Offset,4);
   Dec(Remain,Bits);
   if Remain = 0 then Break;
   if Result >= ACount then Exit;
  end;
end;

{==============================================================================}

function TVirtualDiskVpcImage.MarkBitmap(ABuffer:Pointer;ASize,AStart,ACount:LongWord;AUsed:Boolean):Boolean;
{Mark Count bits from Start in the Block bitmap as Free or Used}
{Size is the total number of bits in the bitmap}
{Start is the bit number in the bitmap to start from}
{Count is the number of bits in the bitmap to be marked}
{Note: Should only be called by MarkBlock}
{Note: Bitmaps use 32 bit blocks in big endian order}
var
 Size:LongWord;   {Number of 32 bit blocks in bitmap}
 Start:LongWord;  {Starting block in the bitmap data}
 Block:LongWord;  {Current block in the bitmap data}
 Offset:LongWord; {Current offset into the bitmap data}

 Bit:LongWord;    {Starting bit to test in current block (0 if first bit)}
 Bits:LongWord;   {Number of bits to test in current block (32 if all bits)}
 Remain:LongWord; {Number of bits remaining to be tested}
 Current:LongWord;{Current bit to test in current block}
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;

 {$IFDEF VIRTUAL_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.MarkBitmap - Size = ' + IntToStr(ASize) + ' Start = ' + IntToStr(AStart) + ' Count = ' + IntToStr(ACount) + ' Used = ' + BoolToStr(AUsed));
 {$ENDIF}

 {Get Size}
 Size:=(ASize shr 5); {Divide by 32}
 if (Size shl 5) < ASize then Inc(Size);
 
 {Get Start}
 Start:=(AStart shr 5); {Divide by 32}
 
 {Get Params}
 Block:=Start;
 Offset:=(Block shl 2); {Multiply by 4}
 Bit:=AStart - ((AStart shr 5) shl 5); {Divide by 32 / Multiply by 32} {If less than 32 then will subtract 0 from start}
 Remain:=ACount;
 while Block < Size do
  begin
   {Get Bits}
   Bits:=Min(Remain,vpcBitmapMaskBits);
   if Bit > 0 then Bits:=Min(Remain,(vpcBitmapMaskBits - Bit));
   
   {$IFDEF VIRTUAL_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.MarkBitmap - Block = ' + IntToStr(Block) + ' Offset = ' + IntToStr(Offset) + ' Remain = ' + IntToStr(Remain) + ' Bit = ' + IntToStr(Bit) + ' Bits = ' + IntToStr(Bits));
   {$ENDIF}
   
   {Mark Bits}
   if (Bit = 0) and (Bits = vpcBitmapMaskBits) then
    begin
     {Mark All}
     if AUsed then
      begin
       {Mark Used}
       LongWord(Pointer(LongWord(ABuffer) + Offset)^):=vpcBitmapMaskAll;
      end
     else
      begin
       {Mark Free}
       LongWord(Pointer(LongWord(ABuffer) + Offset)^):=vpcBitmapMaskNone;
      end;
    end
   else
    begin
     {Mark Some}
     for Current:=Bit to (Bit + (Bits - 1)) do
      begin
       if AUsed then
        begin
         {Mark Used}
         LongWord(Pointer(LongWord(ABuffer) + Offset)^):=LongWord(Pointer(LongWord(ABuffer) + Offset)^) or vpcBitmapMasks[Current];
        end
       else
        begin
         {Mark Free}
         LongWord(Pointer(LongWord(ABuffer) + Offset)^):=LongWord(Pointer(LongWord(ABuffer) + Offset)^) and not(vpcBitmapMasks[Current]);
        end;
      end;
    end;
    
   {Update Params}
   Bit:=0;
   Inc(Block);
   Inc(Offset,4);
   Dec(Remain,Bits);
   if Remain = 0 then Break;
  end;
  
 Result:=True;
end;

{==============================================================================}

function TVirtualDiskVpcImage.LocateDelta(AExtent:TVirtualDiskExtent):String;
{Note: Caller must hold the extent lock}
var
 Path:String;
 WorkBuffer:String;
 ResultCode:Integer;
 SearchRec:TFileSearchRec;
begin
 {}
 Result:='';
 
 if FDriver = nil then Exit;
 if AExtent = nil then Exit;

 {$IFDEF VIRTUAL_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.LocateDelta - Parent = ' + GUIDToString(TVirtualDiskVpcExtent(AExtent).Footer.UniqueId));
 {$ENDIF}

 {Locate Delta in current path (VUD)}
 Path:=FDriver.AddSlash(FDriver.GetPathName(AExtent.Filename),False,True);
 
 {Search current path}
 ResultCode:=FDriver.FindFirstEx(Path + vpcUndoMask,SearchRec);
 while ResultCode = 0 do
  begin
   if (SearchRec.FindData.dwFileAttributes and faDirectory) = faNone then
    begin
     {Check Extent}
     WorkBuffer:=FDriver.GetLongName(Path + SearchRec.FindData.cFileName);
     
     {$IFDEF VIRTUAL_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.LocateDelta - Checking = ' + WorkBuffer);
     {$ENDIF}
     
     if FindExtent(WorkBuffer,False) = nil then
      begin
       if CheckExtent(nil,AExtent,WorkBuffer) then Result:=WorkBuffer;
      end;
    end;
    
   if Length(Result) > 0 then Break;
   
   ResultCode:=FDriver.FindNextEx(SearchRec);
  end;
  
 FDriver.FindCloseEx(SearchRec);
end;

{==============================================================================}

function TVirtualDiskVpcImage.LocateParent(AExtent:TVirtualDiskExtent):String;
{Note: Caller must hold the extent lock}
var
 Path:String;
 WorkBuffer:String;
 ResultCode:Integer;
 SearchRec:TFileSearchRec;
begin
 {}
 Result:='';
 
 if FDriver = nil then Exit;
 if AExtent = nil then Exit;
 
 {$IFDEF VIRTUAL_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.LocateParent - Delta = ' + GUIDToString(TVirtualDiskVpcExtent(AExtent).Footer.UniqueId));
 {$ENDIF}
 
 if AExtent.IsDelta then
  begin
   {$IFDEF VIRTUAL_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.LocateParent - Parent = ' + GUIDToString(TVirtualDiskVpcExtent(AExtent).Sparse.ParentUniqueId));
   {$ENDIF}
   
   {Locate Parent in current path (VHD)}
   Path:=FDriver.AddSlash(FDriver.GetPathName(AExtent.Filename),False,True);
   ResultCode:=FDriver.FindFirstEx(Path + vpcFileMask,SearchRec);
   while ResultCode = 0 do
    begin
     if (SearchRec.FindData.dwFileAttributes and faDirectory) = faNone then
      begin
       WorkBuffer:=FDriver.GetLongName(Path + SearchRec.FindData.cFileName);
       
       {$IFDEF VIRTUAL_DEBUG}
       if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.LocateParent - Checking = ' + WorkBuffer);
       {$ENDIF}
       
       if FindExtent(WorkBuffer,False) = nil then
        begin
         if CheckExtent(AExtent,nil,WorkBuffer) then Result:=WorkBuffer;
        end;
      end;
      
     if Length(Result) > 0 then Break;
     ResultCode:=FDriver.FindNextEx(SearchRec);
    end;
    
   FDriver.FindCloseEx(SearchRec);
   if Length(Result) > 0 then Exit;
   
   {Locate Parent in current path (VUD)}
   Path:=FDriver.AddSlash(FDriver.GetPathName(AExtent.Filename),False,True);
   ResultCode:=FDriver.FindFirstEx(Path + vpcUndoMask,SearchRec);
   while ResultCode = 0 do
    begin
     if (SearchRec.FindData.dwFileAttributes and faDirectory) = faNone then
      begin
       WorkBuffer:=FDriver.GetLongName(Path + SearchRec.FindData.cFileName);
       
       {$IFDEF VIRTUAL_DEBUG}
       if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.LocateParent - Checking = ' + WorkBuffer);
       {$ENDIF}
       
       if FindExtent(WorkBuffer,False) = nil then
        begin
         if CheckExtent(AExtent,nil,WorkBuffer) then Result:=WorkBuffer;
        end;
      end;
      
     if Length(Result) > 0 then Break;
     ResultCode:=FDriver.FindNextEx(SearchRec);
    end;
    
   FDriver.FindCloseEx(SearchRec);
  end;
end;

{==============================================================================}

function TVirtualDiskVpcImage.LocateSibling(AExtent:TVirtualDiskExtent):String;
{Note: Caller must hold the extent lock}
begin
 {}
 Result:='';
 
 if FDriver = nil then Exit;
 if AExtent = nil then Exit;

 {$IFDEF VIRTUAL_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.LocateSibling - Extent = ' + GUIDToString(TVirtualDiskVpcExtent(AExtent).Footer.UniqueId));
 {$ENDIF}

 //To Do //
end;

{==============================================================================}

function TVirtualDiskVpcImage.ImageInit:Boolean;
begin
 {}
 Result:=False;
 
 if not WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FImageNo = 0 then Exit;
 
  if not Ready then
   begin
    FLocked:=False;
    FChanged:=False;
    FAttributes:=GetAttributes;
 
    {FImageType:=itUNKNOWN;} {Do not Reset}
    {FMediaType:=mtUNKNOWN;} {Do not Reset}
    FFloppyType:=ftUNKNOWN;
 
    FSectorSize:=0;
    FSectorCount:=0;
    FSectorShiftCount:=0;
 
    FCylinders:=0;
    FHeads:=0;
    FSectors:=0;
    FLogicalShiftCount:=0;
 
    FPartitionId:=pidUnused;
    FFileSysType:=fsUNKNOWN;
    
    Result:=True;
   end
  else
   begin
    {FLocked:=False;}        {Do not Reset}
    {FChanged:=False;}       {Do not Reset}
    FAttributes:=GetAttributes;
 
    {FImageType:=itUNKNOWN;} {Do not Reset}
    {FMediaType:=mtUNKNOWN;} {Do not Reset}
    {FFloppyType:=ftUNKNOWN;}{Do not Reset}
 
    FSectorSize:=GetSectorSize;
    FSectorCount:=GetSectorCount;
    FSectorShiftCount:=GetSectorShiftCount;
 
    FSectors:=GetSectors;    {Must be SHC not CHS} {Not for VirtualBox but retain for compatibility}
    FHeads:=GetHeads;
    FCylinders:=GetCylinders;
    FLogicalShiftCount:=GetLogicalShiftCount;
 
    FPartitionId:=pidUnused;
    FFileSysType:=fsUNKNOWN;
    
    Result:=True;
   end;
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVpcImage.Read(ASector:LongWord;ACount:Word;var ABuffer):Boolean;
{Note: Caller must hold the image lock}
var
 Start:LongWord;     {Next Sector to Read from Extent}
 Length:Word;        {Number of sectors Read from Extent}

 Remain:Word;        {Remaining sectors to Write to Buffer}
 Offset:LongWord;    {Offset for Write to Buffer}

 Extent:TVirtualDiskExtent;
begin
 {}
 Result:=False;

 if FDriver = nil then Exit;

 {Check Open}
 if FBase = nil then Exit;

 {Check Read}
 if ASector >= FSectorCount then Exit;
 if (ASector + ACount) > FSectorCount then Exit;
 
 {Get Start}
 Offset:=0;
 Start:=ASector;
 Remain:=ACount;
 while Remain > 0 do
  begin
   {Get Extent}
   Extent:=GetExtent(Start,False,True);
   if Extent = nil then Exit;

   {Read Extent}
   Length:=ReadExtent(Extent,Start,Remain,Pointer(LongWord(@ABuffer) + Offset)^);
   Extent.ReleaseLock;
   if Length = 0 then Exit;
    
   Inc(Start,Length);
   Dec(Remain,Length);
   Inc(Offset,(Length shl FSectorShiftCount));
  end;
  
 Result:=(Remain = 0);
end;

{==============================================================================}

function TVirtualDiskVpcImage.Write(ASector:LongWord;ACount:Word;const ABuffer):Boolean;
{Note: Caller must hold the image lock}
var
 Start:LongWord;     {Next Sector to Write to Extent}
 Length:Word;        {Number of sectors Written to Extent}

 Remain:Word;        {Remaining sectors to Read from Buffer}
 Offset:LongWord;    {Offset for Read from Buffer}

 Extent:TVirtualDiskExtent;
begin
 {}
 Result:=False;

 if FDriver = nil then Exit;

 {Check Open}
 if FBase = nil then Exit;

 {Check Write}
 if not Writeable then Exit;
 if ASector >= FSectorCount then Exit;
 if (ASector + ACount) > FSectorCount then Exit;
 
 {Get Start}
 Offset:=0;
 Start:=ASector;
 Remain:=ACount;
 while Remain > 0 do
  begin
   {Get Extent}
   Extent:=GetExtent(Start,True,True);
   if Extent = nil then Exit;
   
   {Write Extent}
   Length:=WriteExtent(Extent,Start,Remain,Pointer(LongWord(@ABuffer) + Offset)^);
   Extent.ReleaseLock;
   if Length = 0 then Exit;
   
   Inc(Start,Length);
   Dec(Remain,Length);
   Inc(Offset,(Length shl FSectorShiftCount));
  end;
  
 Result:=(Remain = 0);
end;

{==============================================================================}

function TVirtualDiskVpcImage.Allocated(ASector:LongWord;ACount:Word):Word;
begin
 {}
 Result:=ACount;

 //To Do //Use TestExtent ?  //Required for Copy Image / Resize Image etc
end;

{==============================================================================}

function TVirtualDiskVpcImage.CreateImage(AMediaType:TMediaType;AFloppyType:TFloppyType;AAttributes:LongWord;ASectorSize:Word;const ASectorCount:Int64;ACylinders,AHeads,ASectors:LongWord;APartitionId:Byte):Integer;
begin
 {}
 Result:=0;

 if not WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FController = nil then Exit;
  if Length(FName) = 0 then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.CreateImage - Name = ' + FName);
  {$ENDIF}

  {Check Open}
  if FBase <> nil then Exit;

  {Setup Parameters}

  {Add Extent}
  //To Do //

  {Add Table}
  //To Do //
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVpcImage.OpenImage(AMediaType:TMediaType;AFloppyType:TFloppyType;AAttributes:LongWord;ASectorSize:Word;const ASectorCount:Int64;ACylinders,AHeads,ASectors:LongWord;APartitionId:Byte):Integer;
begin
 {}
 Result:=0;
 
 if not WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FController = nil then Exit;
  if Length(FName) = 0 then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.OpenImage - Name = ' + FName);
  {$ENDIF}

  {Check Open}
  if FBase <> nil then Exit;

  {Setup Parameters}
  {if (AMediaType <> mtUNKNOWN) and (AMediaType <> mtINVALID) then FMediaType:=AMediaType;}    {Ignore MediaType}
  {if (AFloppyType <> ftUNKNOWN) and (AFloppyType <> ftINVALID) then FFloppyType:=AFloppyType;}{Ignore FloppyType}
  {if AAttributes <> iaNone then FAttributes:=AAttributes;}                                    {Ignore Attributes}
  {if ASectorSize > 0 then FSectorSize:=ASectorSize;}                                          {Ignore SectorSize}
  {if ASectorCount > 0 then FSectorCount:=ASectorCount;}                                       {Ignore SectorCount}
  {if ACylinders > 0 then FCylinders:=ACylinders;}                                             {Ignore Cylinders}
  {if AHeads > 0 then FHeads:=AHeads;}                                                         {Ignore Heads}
  {if ASectors > 0 then FSectors:=ASectors;}                                                   {Ignore Sectors}
  {if APartitionId <> pidUnused then FPartitionId:=APartitionId;}                              {Ignore PartitionId}
  if (AAttributes and iaReadable) <> iaNone then FAttributes:=(FAttributes or iaReadable) else FAttributes:=(FAttributes and not(iaReadable));
  if (AAttributes and iaWriteable) <> iaNone then FAttributes:=(FAttributes or iaWriteable) else FAttributes:=(FAttributes and not(iaWriteable));

  {Check Extent}
  if not CheckExtent(nil,nil,FName) then Exit;

  {Load Extents}
  if not LoadExtents then Exit;

  {Load Tables}
  if not LoadTables then Exit;

  Result:=FImageNo;
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVpcImage.CloseImage:Boolean;
begin
 {}
 Result:=False;
 
 if not WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FController = nil then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVpcImage.CloseImage - Name = ' + FName);
  {$ENDIF}

  {Check Open}
  if FBase = nil then Exit;

  {Close Extents}
  if not CloseExtents then Exit;

  Result:=True;
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVpcImage.ResizeImage(const ASectorCount:Int64):Boolean;
begin
 {}
 Result:=False;
 
 if not WriterLock then Exit;
 try
  //To Do
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVpcImage.CreateSnapshot:Boolean;
begin
 {}
 Result:=False;
 
 if not WriterLock then Exit;
 try
  //To Do
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVpcImage.DeleteSnapshot:Boolean;
begin
 {}
 Result:=False;
 
 if not WriterLock then Exit;
 try
  //To Do
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVpcImage.MergeSnapshot:Boolean;
begin
 {}
 Result:=False;
 
 if not WriterLock then Exit;
 try
  //To Do
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{TVirtualDiskVboxImage}
constructor TVirtualDiskVboxImage.Create(ADriver:TFileSysDriver;AController:TDiskController;const AName:String;AImageNo:Integer);
begin
 {}
 inherited Create(ADriver,AController,AName,AImageNo);
 FAttributes:=iaDisk or iaFixed or iaReadable or iaWriteable;

 FImageType:=itVBOX;
 FMediaType:=mtFIXED;
 FFloppyType:=ftUNKNOWN;

 FCylinders:=0;
 FHeads:=0;
 FSectors:=0;
 FLogicalShiftCount:=0;

 FSectorSize:=MIN_SECTOR_SIZE;
 FSectorCount:=0;
 FSectorShiftCount:=0;
end;

{==============================================================================}

destructor TVirtualDiskVboxImage.Destroy;
begin
 {}
 WriterLock;
 try
  {Nothing}
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  inherited Destroy;
 end; 
end;

{==============================================================================}

function TVirtualDiskVboxImage.GetAttributes:LongWord;
begin
 {}
 Result:=iaDisk or iaFixed or iaReadable or iaWriteable;
 
 if FBase = nil then Exit;
 
 Result:=iaDisk;

 {if IsSplit then Result:=(Result or iaSplit) else Result:=(Result and not(iaSplit));} {Not Applicable}
 if (FAttributes and iaReadable) = iaReadable then Result:=(Result or iaReadable);
 if (FAttributes and iaWriteable) = iaWriteable then Result:=(Result or iaWriteable);
 if FBase.IsFixed then Result:=(Result or iaFixed) else Result:=(Result and not(iaFixed));
 if FBase.IsDynamic then Result:=(Result or iaDynamic) else Result:=(Result and not(iaDynamic));
 if FCurrent = nil then Exit;
 if FCurrent.IsDelta then Result:=(Result or iaUndoable) else Result:=(Result and not(iaUndoable));
end;

{==============================================================================}

function TVirtualDiskVboxImage.GetReady:Boolean;
begin
 {}
 Result:=False;
 if FBase = nil then Exit;
 Result:=True;
end;

{==============================================================================}

function TVirtualDiskVboxImage.GetCylinders:LongWord;
begin
 {}
 Result:=0;
 if not Ready then Exit;
 Result:=TVirtualDiskVboxExtent(FBase).Header.Cylinders;
 if Result = 0 then Result:=inherited GetCylinders;
end;

{==============================================================================}

function TVirtualDiskVboxImage.GetHeads:LongWord;
begin
 {}
 Result:=0;
 if not Ready then Exit;
 Result:=TVirtualDiskVboxExtent(FBase).Header.Heads;
 if Result = 0 then Result:=inherited GetHeads;
end;

{==============================================================================}

function TVirtualDiskVboxImage.GetSectors:LongWord;
begin
 {}
 Result:=0;
 if not Ready then Exit;
 Result:=TVirtualDiskVboxExtent(FBase).Header.Sectors;
 if Result = 0 then Result:=inherited GetSectors;
end;

{==============================================================================}

function TVirtualDiskVboxImage.GetSectorSize:Word;
begin
 {}
 Result:=0;
 if not Ready then Exit;
 Result:=TVirtualDiskVboxExtent(FBase).Header.SectorSize;
end;

{==============================================================================}

function TVirtualDiskVboxImage.GetSectorCount:Int64;
begin
 {}
 Result:=0;
 if not Ready then Exit;
 if TVirtualDiskVboxExtent(FBase).Header.SectorSize = 0 then Exit;
 Result:=TVirtualDiskVboxExtent(FBase).Header.DiskSize div TVirtualDiskVboxExtent(FBase).Header.SectorSize; {Dont use Shift Count, not initialized at this point}
end;

{==============================================================================}

function TVirtualDiskVboxImage.ReadExtent(AExtent:TVirtualDiskExtent;ASector:LongWord;ACount:Word;var ABuffer):Word;
{Note: Caller must hold the extent lock}
var
 Offset:Int64;         {Absolute Offset of Read from file}
 Count:LongWord;       {Count of Bytes to Read from file}

 TableValue:Int64;     {The Block offset value read from the Table}
 TableOffset:LongWord; {The offset of the Blocks entry in the Table}

 BlockNo:LongWord;     {Block Number in the Table (0 to N - 1 blocks)}
 BlockStart:LongWord;  {Starting Sector number of Block}
 BlockCount:LongWord;  {Number of Sectors in Block}

 Table:TVirtualDiskVboxTable;
begin
 {}
 Result:=0;
 
 if not FExtents.ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if AExtent = nil then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVboxImage.ReadExtent - Name = ' + AExtent.Filename);
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   Sector = ' + IntToStr(ASector) + ' Count = ' + IntToStr(ACount));
  {$ENDIF}
  
  {Check Read}
  if ASector < AExtent.StartSector then Exit;
  if ASector >= (AExtent.StartSector + AExtent.SectorCount) then Exit;
  {if (ASector + ACount) > (AExtent.StartSector + AExtent.SectorCount) then Exit;} {Allow Read greater than SectorCount, return actual Count}
  
  {Get Table}
  Table:=TVirtualDiskVboxExtent(AExtent).Table;
  if Table = nil then Exit;
  if Table.Data = nil then Exit;
  
  {Get Block}
  BlockNo:=(ASector shr AExtent.BlockShiftCount);                  {Get the Block No}
  TableOffset:=(BlockNo shl 2);                                    {Get the Table Offset (Multiply BlockNo by 4)}
  TableValue:=LongWord(Pointer(LongWord(Table.Data) + TableOffset)^);  {Get the Table Value}
  BlockStart:=(BlockNo shl AExtent.BlockShiftCount);               {Get the Block Start Sector (Multiply BlockNo by BlockShift)}
  BlockCount:=(AExtent.BlockSize shr FSectorShiftCount);           {Get the Block Sector Count (Divide BlockSize by SectorSize)}
  
  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   BlockNo = ' + IntToHex(BlockNo,8));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   TableOffset = ' + IntToHex(TableOffset,8));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   TableValue = ' + IntToHex(TableValue,8));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   BlockStart = ' + IntToHex(BlockStart,8));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   BlockCount = ' + IntToHex(BlockCount,8));
  {$ENDIF}
  
  {Check Block}
  if TableValue = vboxUnallocatedBlock then
   begin
    {Get Count}
    Count:=Min(ACount,(BlockCount - (ASector - BlockStart))) shl FSectorShiftCount;
    
    {$IFDEF VIRTUAL_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   Count = ' + IntToHex(Count,8));
    {$ENDIF}
    
    {Read Extent (Zero)}
    ZeroMemory(@ABuffer,Count);
    
    Result:=(Count shr FSectorShiftCount);
   end
  else
   begin
    {Get Offset}
    Offset:=(TableValue shl (AExtent.BlockShiftCount + FSectorShiftCount)) + AExtent.DataOffset + ((ASector - BlockStart) shl FSectorShiftCount);
    Count:=Min(ACount,(BlockCount - (ASector - BlockStart))) shl FSectorShiftCount;
    
    {$IFDEF VIRTUAL_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   Offset = ' + IntToHex(Offset,16));
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   Count = ' + IntToHex(Count,8));
    {$ENDIF}
    
    {Read Extent}
    FDriver.FileSeekEx(AExtent.Handle,Offset,soFromBeginning);
    if FDriver.FileRead(AExtent.Handle,ABuffer,Count) <> Integer(Count) then Exit;
    
    Result:=(Count shr FSectorShiftCount);
   end;
 finally  
  FExtents.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVboxImage.WriteExtent(AExtent:TVirtualDiskExtent;ASector:LongWord;ACount:Word;const ABuffer):Word;
{Note: Caller must hold the extent lock}
var
 Offset:Int64;         {Absolute Offset of Write to file}
 Count:LongWord;       {Count of Bytes to Write to file}
 Buffer:Pointer;       {Buffer for Dynamic and Differencing allocation}

 TableValue:Int64;     {The Block offset value read from the Table}
 TableOffset:LongWord; {The offset of the Blocks entry in the Table}

 BlockNo:LongWord;     {Block Number in the Table (0 to N - 1 blocks)}
 BlockStart:LongWord;  {Starting Sector number of Block}
 BlockCount:LongWord;  {Number of Sectors in Block}

 Table:TVirtualDiskVboxTable;
begin
 {}
 Result:=0;
 
 if not FExtents.ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if AExtent = nil then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVboxImage.WriteExtent - Name = ' + AExtent.Filename);
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                                    Sector = ' + IntToStr(ASector) + ' Count = ' + IntToStr(ACount));
  {$ENDIF}
  
  {Check Write}
  if not Writeable then Exit;
  if ASector < AExtent.StartSector then Exit;
  if ASector >= (AExtent.StartSector + AExtent.SectorCount) then Exit;
  {if (ASector + ACount) > (AExtent.StartSector + AExtent.SectorCount) then Exit;} {Allow Write greater than SectorCount, return actual Count}
  
  {Get Table}
  Table:=TVirtualDiskVboxExtent(AExtent).Table;
  if Table = nil then Exit;
  if Table.Data = nil then Exit;
  
  {Get Block}
  BlockNo:=(ASector shr AExtent.BlockShiftCount);                  {Get the Block No}
  TableOffset:=(BlockNo shl 2);                                    {Get the Table Offset (Multiply BlockNo by 4)}
  TableValue:=LongWord(Pointer(LongWord(Table.Data) + TableOffset)^);  {Get the Table Value}
  BlockStart:=(BlockNo shl AExtent.BlockShiftCount);               {Get the Block Start Sector (Multiply BlockNo by BlockShift)}
  BlockCount:=(AExtent.BlockSize shr FSectorShiftCount);           {Get the Block Sector Count (Divide BlockSize by SectorSize)}
  
  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   BlockNo = ' + IntToHex(BlockNo,8));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   TableOffset = ' + IntToHex(TableOffset,8));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   TableValue = ' + IntToHex(TableValue,8));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   BlockStart = ' + IntToHex(BlockStart,8));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   BlockCount = ' + IntToHex(BlockCount,8));
  {$ENDIF}
  
  {Check Block}
  if TableValue = vboxUnallocatedBlock then
   begin
    {Check Extent}
    if (AExtent.IsDynamic) or (AExtent.IsDelta) then
     begin
      {Get Buffer}
      Buffer:=GetMem(AExtent.BlockSize);
      if Buffer = nil then Exit;
      try
       {Allocate Block}
       TableValue:=(FDriver.FileSizeEx(AExtent.Handle) - AExtent.DataOffset) shr (AExtent.BlockShiftCount + FSectorShiftCount);
       
       {$IFDEF VIRTUAL_DEBUG}
       if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   TableValue = ' + IntToHex(TableValue,8));
       {$ENDIF}
       
       {Extend File}
       if AExtent.IsDelta then
        begin
         {Copy on Write}
         if not Read(BlockStart,BlockCount,Buffer^) then Exit;
         FDriver.FileSeekEx(AExtent.Handle,0,soFromEnd);
         if FDriver.FileWrite(AExtent.Handle,Buffer^,AExtent.BlockSize) <> Integer(AExtent.BlockSize) then Exit;
        end
       else
        begin
         {Zero on Allocate}
         ZeroMemory(Buffer,AExtent.BlockSize);
         FDriver.FileSeekEx(AExtent.Handle,0,soFromEnd);
         if FDriver.FileWrite(AExtent.Handle,Buffer^,AExtent.BlockSize) <> Integer(AExtent.BlockSize) then Exit;
        end;
        
       {Update Table}
       LongWord(Pointer(LongWord(Table.Data) + TableOffset)^):=TableValue;
       if not SetTable(Table) then Exit;
       
       {Update Extent}
       Inc(TVirtualDiskVboxExtent(AExtent).Header.AllocatedBlocks);
       if not SetExtent(AExtent) then Exit;
       
       {Get Offset}
       Offset:=(TableValue shl (AExtent.BlockShiftCount + FSectorShiftCount)) + AExtent.DataOffset + ((ASector - BlockStart) shl FSectorShiftCount);
       Count:=Min(ACount,(BlockCount - (ASector - BlockStart))) shl FSectorShiftCount;
       
       {$IFDEF VIRTUAL_DEBUG}
       if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   Offset = ' + IntToHex(Offset,16));
       if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   Count = ' + IntToHex(Count,8));
       {$ENDIF}
       
       {Write Extent}
       FDriver.FileSeekEx(AExtent.Handle,Offset,soFromBeginning);
       if FDriver.FileWrite(AExtent.Handle,ABuffer,Count) <> Integer(Count) then Exit;
       
       Result:=(Count shr FSectorShiftCount);
      finally
       FreeMem(Buffer);
      end;
     end;
   end
  else
   begin
    {Get Offset}
    Offset:=(TableValue shl (AExtent.BlockShiftCount + FSectorShiftCount)) + AExtent.DataOffset + ((ASector - BlockStart) shl FSectorShiftCount);
    Count:=Min(ACount,(BlockCount - (ASector - BlockStart))) shl FSectorShiftCount;
    
    {$IFDEF VIRTUAL_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   Offset = ' + IntToHex(Offset,16));
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   Count = ' + IntToHex(Count,8));
    {$ENDIF}
    
    {Write Extent}
    FDriver.FileSeekEx(AExtent.Handle,Offset,soFromBeginning);
    if FDriver.FileWrite(AExtent.Handle,ABuffer,Count) <> Integer(Count) then Exit;
    
    Result:=(Count shr FSectorShiftCount);
   end;
 finally  
  FExtents.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVboxImage.LoadExtents:Boolean;
var
 WorkBuffer:String;
 Delta:TVirtualDiskVboxExtent;
 Extent:TVirtualDiskVboxExtent;
 Parent:TVirtualDiskVboxExtent;
begin
 {}
 Result:=False;
 
 if not FExtents.WriterLock then Exit;
 try
  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVboxImage.LoadExtents');
  {$ENDIF}

  {Load Extent}
  Extent:=TVirtualDiskVboxExtent(LoadExtent(nil,nil,FName));
  if Extent = nil then Exit;
  try
   if Extent.IsBase then {Base Extent}
    begin
     {Set Base}
     FBase:=Extent;
     
     {Load Delta Extents}
     WorkBuffer:=LocateDelta(Extent);
     while Length(WorkBuffer) <> 0 do
      begin
       {Save Parent}
       Parent:=Extent;
       
       {Load Extent}
       Extent:=TVirtualDiskVboxExtent(LoadExtent(nil,Parent,WorkBuffer));
       if Extent = nil then Exit;
       
       WorkBuffer:=LocateDelta(Extent);
      end;
      
     {Set Current}
     FCurrent:=Extent; {Last Delta loaded must be Current (Will be same as Base if no Deltas loaded)}
    end
   else if Extent.IsDelta then {Delta Extent}
    begin
     {Save Delta}
     Delta:=Extent;
     
     {Locate Delta Extents}
     WorkBuffer:=LocateDelta(Extent);
     while Length(WorkBuffer) <> 0 do
      begin
       {Save Parent}
       Parent:=Extent;
       
       {Load Extent}
       Extent:=TVirtualDiskVboxExtent(LoadExtent(nil,Parent,WorkBuffer));
       if Extent = nil then Exit;
       
       WorkBuffer:=LocateDelta(Extent);
      end;
      
     {Set Current}
     FCurrent:=Extent; {Last Delta loaded must be Current}
     
     {Locate Parent Extents}
     WorkBuffer:=LocateParent(Delta);
     if Length(WorkBuffer) = 0 then Exit;
     while Length(WorkBuffer) <> 0 do
      begin
       {Load Extent}
       Extent:=TVirtualDiskVboxExtent(LoadExtent(Delta,nil,WorkBuffer));
       if Extent = nil then Exit;
       
       {Save Delta}
       Delta:=Extent;
       if Extent.IsBase then Break;
       
       WorkBuffer:=LocateParent(Delta);
      end;
      
     {Set Base}
     if Extent.IsBase then FBase:=Extent; {Last Parent loaded must be Base}
    end;
    
   {Check Loaded}
   if FBase = nil then Exit;
   if FCurrent = nil then Exit;
   
   Result:=True;

   {$IFDEF VIRTUAL_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVboxImage.LoadExtents - Base    = ' + FBase.Filename);
   if FILESYS_LOG_ENABLED then FileSysLogDebug('                                    Current = ' + FCurrent.Filename);
   if FILESYS_LOG_ENABLED then FileSysLogDebug('                                    Count   = ' + IntToStr(FExtents.Count));
   {$ENDIF}
  finally
   if not Result then CloseExtents;
  end;
 finally  
  FExtents.WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVboxImage.CheckExtent(ADelta,AParent:TVirtualDiskExtent;const AFilename:String):Boolean;
{Check that the passed file is a valid VirtualBox extent}
{If delta is provided, check that passed file is the parent of the delta}
{If parent is provided, check that passed file is the delta of the parent}
{Note: Caller must hold the delta and parent locks}
var
 Handle:THandle;
 Header:TVboxDiskHeader;
begin
 {}
 Result:=False;
 
 if FDriver = nil then Exit;
 if Length(AFilename) = 0 then Exit;

 {$IFDEF VIRTUAL_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVboxImage.CheckExtent - Name = ' + AFilename);
 {$ENDIF}

 {Open File}
 if not FDriver.FileExists(AFilename) then Exit;
 Handle:=FDriver.FileOpen(AFilename,fmOpenRead or fmShareDenyNone);
 if Handle = INVALID_HANDLE_VALUE then Exit;
 try
  {Read Header}
  FDriver.FileSeekEx(Handle,0,soFromBeginning);
  if FDriver.FileRead(Handle,Header,SizeOf(TVboxDiskHeader)) <> SizeOf(TVboxDiskHeader) then Exit;
  
  {Check Header}
  if Header.Signature <> vboxDiskSignature then Exit;
  if Header.Version <> vboxDiskVersion then Exit;
  if Header.SectorSize < MIN_SECTOR_SIZE then Exit;
  if (Header.BlockSize mod Header.SectorSize) <> 0 then Exit;
  
  {Check Delta}
  if ADelta <> nil then
   begin
    {$IFDEF VIRTUAL_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVboxImage.CheckExtent - Delta = ' + GUIDToString(TVirtualDiskVboxExtent(ADelta).Header.UUID));
    {$ENDIF}
    
    if not CompareGUID(Header.UUID,TVirtualDiskVboxExtent(ADelta).Header.LinkUUID) then Exit;
   end;
   
  {Check Parent}
  if AParent <> nil then
   begin
    {$IFDEF VIRTUAL_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVboxImage.CheckExtent - Parent = ' + GUIDToString(TVirtualDiskVboxExtent(AParent).Header.UUID));
    {$ENDIF}
    
    if (Header.DiskType <> vboxDiskTypeDifferencing) and (Header.DiskType <> vboxDiskTypeDynamic) then Exit; {Sometimes VirtualBox marks Differencing disks as Dynamic}
    if not CompareGUID(Header.LinkUUID,TVirtualDiskVboxExtent(AParent).Header.UUID) then Exit;
   end;
   
  Result:=True;
 finally
  {Close File}
  FDriver.FileClose(Handle);
 end;
end;

{==============================================================================}

function TVirtualDiskVboxImage.LoadExtent(ADelta,AParent:TVirtualDiskExtent;const AFilename:String):TVirtualDiskExtent;
var
 Mode:Integer;
 Extent:TVirtualDiskExtent;
begin
 {}
 Result:=nil;
 
 if not FExtents.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if Length(AFilename) = 0 then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVboxImage.LoadExtent - Name = ' + AFilename);
  if ADelta <> nil then if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   Delta = ' + GUIDToString(TVirtualDiskVboxExtent(ADelta).Header.UUID));
  if AParent <> nil then if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   Parent = ' + GUIDToString(TVirtualDiskVboxExtent(AParent).Header.UUID));
  {$ENDIF}

  {Create Extent}
  Extent:=TVirtualDiskVboxExtent.Create(Self,ADelta,AParent);
  try
   {Open File}
   if not FDriver.FileExists(AFilename) then Exit;
   Extent.Filename:=AFilename;
   Mode:=fmOpenRead or fmShareDenyNone;
   if Writeable then Mode:=fmOpenReadWrite or fmShareDenyWrite;
   Extent.Handle:=FDriver.FileOpen(Extent.Filename,Mode);
   if Extent.Handle = INVALID_HANDLE_VALUE then Exit;
   try
    {Read Header}
    FDriver.FileSeekEx(Extent.Handle,0,soFromBeginning);
    if FDriver.FileRead(Extent.Handle,TVirtualDiskVboxExtent(Extent).Header^,SizeOf(TVboxDiskHeader)) <> SizeOf(TVboxDiskHeader) then Exit;
    
    {Check Header}
    if TVirtualDiskVboxExtent(Extent).Header.Signature <> vboxDiskSignature then Exit;
    if TVirtualDiskVboxExtent(Extent).Header.Version <> vboxDiskVersion then Exit;
    if TVirtualDiskVboxExtent(Extent).Header.SectorSize < MIN_SECTOR_SIZE then Exit;
    if (TVirtualDiskVboxExtent(Extent).Header.BlockSize mod TVirtualDiskVboxExtent(Extent).Header.SectorSize) <> 0 then Exit;
    
    {Update Image}
    FSectorSize:=TVirtualDiskVboxExtent(Extent).Header.SectorSize;
    
    {Update Flags}
    Extent.Flags:=virtualFlagNone;
    if NullGUID(TVirtualDiskVboxExtent(Extent).Header.LinkUUID) then Extent.Flags:=Extent.Flags or virtualFlagBase;
    if not NullGUID(TVirtualDiskVboxExtent(Extent).Header.LinkUUID) then Extent.Flags:=Extent.Flags or virtualFlagDelta;
    if TVirtualDiskVboxExtent(Extent).Header.DiskType = vboxDiskTypeStatic then Extent.Flags:=Extent.Flags or virtualFlagFixed;
    if TVirtualDiskVboxExtent(Extent).Header.DiskType = vboxDiskTypeDynamic then Extent.Flags:=Extent.Flags or virtualFlagDynamic;
    if TVirtualDiskVboxExtent(Extent).Header.DiskType = vboxDiskTypeDifferencing then Extent.Flags:=Extent.Flags or virtualFlagDelta;
    
    {Update Extent}
    Extent.DataOffset:=TVirtualDiskVboxExtent(Extent).Header.DataOffset;
    Extent.BlockSize:=TVirtualDiskVboxExtent(Extent).Header.BlockSize;
    Extent.StartSector:=0;
    Extent.SectorCount:=TVirtualDiskVboxExtent(Extent).Header.DiskSize div TVirtualDiskVboxExtent(Extent).Header.SectorSize;
    Extent.BlockShiftCount:=Extent.GetBlockShiftCount;
    TVirtualDiskVboxExtent(Extent).HeaderOffset:=0;
    TVirtualDiskVboxExtent(Extent).HeaderSize:=SizeOf(TVboxDiskHeader);
    
    {Check Extent}
    if Extent.SectorCount = 0 then Exit;
    
    {Update Delta}
    if ADelta <> nil then ADelta.Parent:=Extent;
    
    {Update Parent}
    if AParent <> nil then AParent.Delta:=Extent;
    
    {$IFDEF VIRTUAL_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVboxImage.LoadExtent - DataOffset = ' + IntToStr(Extent.DataOffset));
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   BlockSize = ' + IntToStr(Extent.BlockSize));
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   StartSector = ' + IntToStr(Extent.StartSector));
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   SectorCount = ' + IntToStr(Extent.SectorCount));
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   BlockShiftCount = ' + IntToStr(Extent.BlockShiftCount));
    {$ENDIF}
    
    {Add Extent}
    FExtents.Add(Extent);
    
    Result:=Extent;
   finally
    if Result = nil then FDriver.FileClose(Extent.Handle);
   end;
  finally
   if Result = nil then Extent.Free;
  end;
 finally  
  FExtents.WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVboxImage.AddExtent(AParent:TVirtualDiskExtent;const AFilename:String):TVirtualDiskExtent;
{Note: Caller must hold the parent lock}
begin
 {}
 Result:=nil;
 
 if not FExtents.WriterLock then Exit;
 try
  if FDriver = nil then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVboxImage.AddExtent - Name = ' + AFilename);
  {$ENDIF}

  //To Do //Create a new Extent //Required for Create Image

 finally  
  FExtents.WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVboxImage.SetExtent(AExtent:TVirtualDiskExtent):Boolean;
{Note: Caller must hold the extent lock}
begin
 {}
 Result:=False;
 
 if not FExtents.ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if AExtent = nil then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVboxImage.SetExtent - Name = ' + AExtent.Filename);
  {$ENDIF}

  {Check Extent}
  if AExtent.Handle = INVALID_HANDLE_VALUE then Exit;

  {Write Extent}
  FDriver.FileSeekEx(AExtent.Handle,TVirtualDiskVboxExtent(AExtent).HeaderOffset,soFromBeginning);
  if FDriver.FileWrite(AExtent.Handle,TVirtualDiskVboxExtent(AExtent).Header^,TVirtualDiskVboxExtent(AExtent).HeaderSize) <> Integer(TVirtualDiskVboxExtent(AExtent).HeaderSize) then Exit;

  Result:=True;
 finally  
  FExtents.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVboxImage.GetExtent(const ASector:Int64;AWrite,ALock:Boolean):TVirtualDiskExtent;
var
 BlockNo:LongWord;
 TableOffset:LongWord;
 Extent:TVirtualDiskExtent;
begin
 {}
 Result:=nil;
 
 if not FExtents.ReaderLock then Exit;
 try
  if ASector >= FSectorCount then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVboxImage.GetExtent - Sector = ' + IntToStr(ASector) + ' Write = ' + BoolToStr(AWrite));
  {$ENDIF}

  {Check Write}
  if AWrite then
   begin
    Extent:=FCurrent;
    if Extent <> nil then
     begin
      if (ASector >= Extent.StartSector) and (ASector < (Extent.StartSector + Extent.SectorCount)) then
       begin
        if ALock then Extent.AcquireLock;
        
        Result:=Extent;
       end;
     end;
   end
  else
   begin
    {Get Extent}
    Extent:=FCurrent;
    while Extent <> nil do
     begin
      if (ASector >= Extent.StartSector) and (ASector < (Extent.StartSector + Extent.SectorCount)) then
       begin
        if TVirtualDiskVboxExtent(Extent).Table = nil then Exit;
        if TVirtualDiskVboxExtent(Extent).Table.Data = nil then Exit;
        
        {Get Block}
        BlockNo:=(ASector shr Extent.BlockShiftCount);
        TableOffset:=(BlockNo shl 2);
        
        {Check Block (Block is Allocated or Extent is Base)}
        if (LongWord(Pointer(LongWord(TVirtualDiskVboxExtent(Extent).Table.Data) + TableOffset)^) <> vboxUnallocatedBlock) or (Extent.IsBase) then
         begin
          if ALock then Extent.AcquireLock;
          
          Result:=Extent;
          Exit;
         end;
       end;
       
      Extent:=Extent.Parent;
     end;
   end;
 finally  
  FExtents.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVboxImage.LoadTables:Boolean;
var
 Extent:TVirtualDiskExtent;
begin
 {}
 Result:=False;
 
 if not FExtents.WriterLock then Exit;
 try
  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVboxImage.LoadTables');
  {$ENDIF}

  {Load Tables}
  Extent:=TVirtualDiskExtent(FExtents.First);
  while Extent <> nil do
   begin
    if LoadTable(Extent,0) = nil then Exit;
    
    Extent:=TVirtualDiskExtent(Extent.Next);
   end;

  Result:=True;
 finally  
  FExtents.WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVboxImage.LoadTable(AExtent:TVirtualDiskExtent;ATableNo:LongWord):TVirtualDiskTable;
{Note: Caller must hold the extent tables writer lock}
var
 Data:Pointer;
 Table:TVirtualDiskVboxTable;
begin
 {}
 Result:=nil;
 
 if not FExtents.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if AExtent = nil then Exit;
  if ATableNo <> 0 then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVboxImage.LoadTable - Extent = ' + AExtent.Filename + ' TableNo = ' + IntToStr(ATableNo));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                                  TotalBlocks = ' + IntToStr(TVirtualDiskVboxExtent(AExtent).Header.TotalBlocks));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                                  BlocksOffset = ' + IntToStr(TVirtualDiskVboxExtent(AExtent).Header.BlocksOffset));
  if FILESYS_LOG_ENABLED then FileSysLogDebug('                                  AllocatedBlocks = ' + IntToStr(TVirtualDiskVboxExtent(AExtent).Header.AllocatedBlocks));
  {$ENDIF}

  {Get Table}
  Table:=TVirtualDiskVboxExtent(AExtent).Table;
  
  {Update Table}
  Table.TableOffset:=TVirtualDiskVboxExtent(AExtent).Header.BlocksOffset;
  Table.TableSize:=TVirtualDiskVboxExtent(AExtent).Header.TotalBlocks shl 2;
  Table.StartSector:=TVirtualDiskVboxExtent(AExtent).StartSector;
  Table.SectorCount:=TVirtualDiskVboxExtent(AExtent).SectorCount;
  
  {Check Table}
  if Table.TableSize = 0 then Exit;
  
  {Alloc Table}
  Data:=GetMem(Table.TableSize);
  if Data = nil then Exit;
  try
   {Load Table}
   FDriver.FileSeekEx(AExtent.Handle,Table.TableOffset,soFromBeginning);
   if FDriver.FileRead(AExtent.Handle,Data^,Table.TableSize) <> Integer(Table.TableSize) then Exit;
   
   {$IFDEF VIRTUAL_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVboxImage.LoadTable - TableSize = ' + IntToStr(Table.TableSize));
   {$ENDIF}
   
   Table.Data:=Data;
   
   Result:=Table;
  finally
   if Result = nil then FreeMem(Data);
  end;
 finally  
  FExtents.WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVboxImage.AddTable(AExtent:TVirtualDiskExtent):TVirtualDiskTable;
{Note: Caller must hold the extent lock}
begin
 {}
 Result:=nil;
 
 if not FExtents.WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if AExtent = nil then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVboxImage.AddTable - Extent = ' + AExtent.Filename);
  {$ENDIF}

  //To Do //Create a new Table //Required for Create Image

 finally  
  FExtents.WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVboxImage.SetTable(ATable:TVirtualDiskTable):Boolean;
{Note: Caller must hold the table extent lock}
begin
 {}
 Result:=False;
 
 if not FExtents.ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if ATable = nil then Exit;
  if ATable.Extent = nil then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVboxImage.SetTable - TableNo = 0');
  {$ENDIF}

  {Check Table}
  if TVirtualDiskVboxTable(ATable).Data = nil then Exit;

  {Write Table}
  FDriver.FileSeekEx(ATable.Extent.Handle,ATable.TableOffset,soFromBeginning);
  if FDriver.FileWrite(ATable.Extent.Handle,TVirtualDiskVboxTable(ATable).Data^,ATable.TableSize) <> Integer(ATable.TableSize) then Exit;

  Result:=True;
 finally  
  FExtents.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVboxImage.LocateDelta(AExtent:TVirtualDiskExtent):String;
{Note: Caller must hold the extent lock}
var
 Path:String;
 WorkBuffer:String;
 ResultCode:Integer;
 SearchRec:TFileSearchRec;
 FolderRec:TFileSearchRec;
begin
 {}
 Result:='';
 
 if FDriver = nil then Exit;
 if AExtent = nil then Exit;

 {$IFDEF VIRTUAL_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVboxImage.LocateDelta - Parent = ' + GUIDToString(TVirtualDiskVboxExtent(AExtent).Header.UUID));
 {$ENDIF}

 if AExtent.IsBase then
  begin
   {Locate Delta in Snapshots path}
   Path:=FDriver.AddSlash(FDriver.GetPathName(AExtent.Filename),False,True) + vboxDeltaPath;
   
   {Search Machines path}
   ResultCode:=FDriver.FindFirstEx(Path + vboxPathMask,FolderRec);
   while ResultCode = 0 do
    begin
     if (FolderRec.FindData.cFileName[0] <> '.') and (FolderRec.FindData.cFileName <> '..') then
      begin
       if (FolderRec.FindData.dwFileAttributes and faDirectory) = faDirectory then
        begin
         {Search Snapshots path}
         ResultCode:=FDriver.FindFirstEx(Path + FolderRec.FindData.cFileName + vboxSnapshotPath + vboxFileMask,SearchRec);
         while ResultCode = 0 do
          begin
           if (SearchRec.FindData.dwFileAttributes and faDirectory) = faNone then
            begin
             {Check Extent}
             WorkBuffer:=FDriver.GetLongName(Path + FolderRec.FindData.cFileName + vboxSnapshotPath + SearchRec.FindData.cFileName);
             
             {$IFDEF VIRTUAL_DEBUG}
             if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVboxImage.LocateDelta - Checking = ' + WorkBuffer);
             {$ENDIF}
             
             if FindExtent(WorkBuffer,False) = nil then
              begin
               if CheckExtent(nil,AExtent,WorkBuffer) then Result:=WorkBuffer;
              end;
            end;
            
           if Length(Result) > 0 then Break;
           ResultCode:=FDriver.FindNextEx(SearchRec);
          end;
          
         FDriver.FindCloseEx(SearchRec);
        end;
      end;
      
     if Length(Result) > 0 then Break;
     ResultCode:=FDriver.FindNextEx(FolderRec);
    end;
    
   FDriver.FindCloseEx(FolderRec);
  end
 else if AExtent.IsDelta then
  begin
   {Locate Delta in current path}
   Path:=FDriver.AddSlash(FDriver.GetPathName(AExtent.Filename),False,True);
   
   {Search current path}
   ResultCode:=FDriver.FindFirstEx(Path + vboxFileMask,SearchRec);
   while ResultCode = 0 do
    begin
     if (SearchRec.FindData.dwFileAttributes and faDirectory) = faNone then
      begin
       {Check Extent}
       WorkBuffer:=FDriver.GetLongName(Path + SearchRec.FindData.cFileName);
       
       {$IFDEF VIRTUAL_DEBUG}
       if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVboxImage.LocateDelta - Checking = ' + WorkBuffer);
       {$ENDIF}
       
       if FindExtent(WorkBuffer,False) = nil then
        begin
         if CheckExtent(nil,AExtent,WorkBuffer) then Result:=WorkBuffer;
        end;
      end;
      
     if Length(Result) > 0 then Break;
     ResultCode:=FDriver.FindNextEx(SearchRec);
    end;
    
   FDriver.FindCloseEx(SearchRec);
  end;
end;

{==============================================================================}

function TVirtualDiskVboxImage.LocateParent(AExtent:TVirtualDiskExtent):String;
{Note: Caller must hold the extent lock}
var
 Path:String;
 WorkBuffer:String;
 ResultCode:Integer;
 SearchRec:TFileSearchRec;
begin
 {}
 Result:='';
 
 if FDriver = nil then Exit;
 if AExtent = nil then Exit;

 {$IFDEF VIRTUAL_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVboxImage.LocateParent - Delta = ' + GUIDToString(TVirtualDiskVboxExtent(AExtent).Header.UUID));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVboxImage.LocateParent - Parent = ' + GUIDToString(TVirtualDiskVboxExtent(AExtent).Header.LinkUUID));
 {$ENDIF}

 if AExtent.IsDelta then
  begin
   {Locate Parent in current path}
   Path:=FDriver.AddSlash(FDriver.GetPathName(AExtent.Filename),False,True);
   ResultCode:=FDriver.FindFirstEx(Path + vboxFileMask,SearchRec);
   while ResultCode = 0 do
    begin
     if (SearchRec.FindData.dwFileAttributes and faDirectory) = faNone then
      begin
       WorkBuffer:=FDriver.GetLongName(Path + SearchRec.FindData.cFileName);
       
       {$IFDEF VIRTUAL_DEBUG}
       if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVboxImage.LocateParent - Checking = ' + WorkBuffer);
       {$ENDIF}
       
       if FindExtent(WorkBuffer,False) = nil then
        begin
         if CheckExtent(AExtent,nil,WorkBuffer) then Result:=WorkBuffer;
        end;
      end;
      
     if Length(Result) > 0 then Break;
     ResultCode:=FDriver.FindNextEx(SearchRec);
    end;
    
   FDriver.FindCloseEx(SearchRec);
   if Length(Result) > 0 then Exit;
   
   {Locate Parent in VDI path}
   Path:=FDriver.AddSlash(FDriver.GetPathName(AExtent.Filename),False,True) + vboxParentPath;
   ResultCode:=FDriver.FindFirstEx(Path + vboxFileMask,SearchRec);
   while ResultCode = 0 do
    begin
     if (SearchRec.FindData.dwFileAttributes and faDirectory) = faNone then
      begin
       WorkBuffer:=FDriver.GetLongName(Path + SearchRec.FindData.cFileName);
       
       {$IFDEF VIRTUAL_DEBUG}
       if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVboxImage.LocateParent - Checking = ' + WorkBuffer);
       {$ENDIF}
       
       if FindExtent(WorkBuffer,False) = nil then
        begin
         if CheckExtent(AExtent,nil,WorkBuffer) then Result:=WorkBuffer;
        end;
      end;
      
     if Length(Result) > 0 then Break;
     ResultCode:=FDriver.FindNextEx(SearchRec);
    end;
    
   FDriver.FindCloseEx(SearchRec);
   if Length(Result) > 0 then Exit;
   
   {Locate Parent in HardDisks path}
   Path:=FDriver.AddSlash(FDriver.GetPathName(AExtent.Filename),False,True) + vboxNewParentPath;
   ResultCode:=FDriver.FindFirstEx(Path + vboxFileMask,SearchRec);
   while ResultCode = 0 do
    begin
     if (SearchRec.FindData.dwFileAttributes and faDirectory) = faNone then
      begin
       WorkBuffer:=FDriver.GetLongName(Path + SearchRec.FindData.cFileName);
       
       {$IFDEF VIRTUAL_DEBUG}
       if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVboxImage.LocateParent - Checking = ' + WorkBuffer);
       {$ENDIF}
       
       if FindExtent(WorkBuffer,False) = nil then
        begin
         if CheckExtent(AExtent,nil,WorkBuffer) then Result:=WorkBuffer;
        end;
      end;
      
     if Length(Result) > 0 then Break;
     ResultCode:=FDriver.FindNextEx(SearchRec);
    end;
    
   FDriver.FindCloseEx(SearchRec);
   if Length(Result) > 0 then Exit;
  end;
end;

{==============================================================================}

function TVirtualDiskVboxImage.ImageInit:Boolean;
begin
 {}
 Result:=False;
 
 if not WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FImageNo = 0 then Exit;
 
  if not Ready then
   begin
    FLocked:=False;
    FChanged:=False;
    FAttributes:=GetAttributes;
 
    {FImageType:=itUNKNOWN;} {Do not Reset}
    {FMediaType:=mtUNKNOWN;} {Do not Reset}
    FFloppyType:=ftUNKNOWN;
 
    FSectorSize:=0;
    FSectorCount:=0;
    FSectorShiftCount:=0;
 
    FCylinders:=0;
    FHeads:=0;
    FSectors:=0;
    FLogicalShiftCount:=0;
 
    FPartitionId:=pidUnused;
    FFileSysType:=fsUNKNOWN;
    
    Result:=True;
   end
  else
   begin
    {FLocked:=False;}        {Do not Reset}
    {FChanged:=False;}       {Do not Reset}
    FAttributes:=GetAttributes;
 
    {FImageType:=itUNKNOWN;} {Do not Reset}
    {FMediaType:=mtUNKNOWN;} {Do not Reset}
    {FFloppyType:=ftUNKNOWN;}{Do not Reset}
 
    FSectorSize:=GetSectorSize;
    FSectorCount:=GetSectorCount;
    FSectorShiftCount:=GetSectorShiftCount;
 
    FSectors:=GetSectors;    {Must be SHC not CHS} {Not for VirtualBox but retain for compatibility}
    FHeads:=GetHeads;
    FCylinders:=GetCylinders;
    FLogicalShiftCount:=GetLogicalShiftCount;
 
    FPartitionId:=pidUnused;
    FFileSysType:=fsUNKNOWN;
    
    Result:=True;
   end;
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVboxImage.Read(ASector:LongWord;ACount:Word;var ABuffer):Boolean;
{Note: Caller must hold the image lock}
var
 Start:LongWord;     {Next Sector to Read from Extent}
 Length:Word;        {Number of sectors Read from Extent}

 Remain:Word;        {Remaining sectors to Write to Buffer}
 Offset:LongWord;    {Offset for Write to Buffer}

 Extent:TVirtualDiskExtent;
begin
 {}
 Result:=False;

 if FDriver = nil then Exit;

 {Check Open}
 if FBase = nil then Exit;

 {Check Read}
 if ASector >= FSectorCount then Exit;
 if (ASector + ACount) > FSectorCount then Exit;
 
 {Get Start}
 Offset:=0;
 Start:=ASector;
 Remain:=ACount;
 while Remain > 0 do
  begin
   {Get Extent}
   Extent:=GetExtent(Start,False,True);
   if Extent = nil then Exit;
   
   {Read Extent}
   Length:=ReadExtent(Extent,Start,Remain,Pointer(LongWord(@ABuffer) + Offset)^);
   Extent.ReleaseLock;
   if Length = 0 then Exit;
   
   Inc(Start,Length);
   Dec(Remain,Length);
   Inc(Offset,(Length shl FSectorShiftCount));
  end;
  
 Result:=(Remain = 0);
end;

{==============================================================================}

function TVirtualDiskVboxImage.Write(ASector:LongWord;ACount:Word;const ABuffer):Boolean;
{Note: Caller must hold the image lock}
var
 Start:LongWord;     {Next Sector to Write to Extent}
 Length:Word;        {Number of sectors Written to Extent}

 Remain:Word;        {Remaining sectors to Read from Buffer}
 Offset:LongWord;    {Offset for Read from Buffer}

 Extent:TVirtualDiskExtent;
begin
 {}
 Result:=False;

 if FDriver = nil then Exit;

 {Check Open}
 if FBase = nil then Exit;

 {Check Write}
 if not Writeable then Exit;
 if ASector >= FSectorCount then Exit;
 if (ASector + ACount) > FSectorCount then Exit;
 
 {Get Start}
 Offset:=0;
 Start:=ASector;
 Remain:=ACount;
 while Remain > 0 do
  begin
   {Get Extent}
   Extent:=GetExtent(Start,True,True);
   if Extent = nil then Exit;
   
   {Write Extent}
   Length:=WriteExtent(Extent,Start,Remain,Pointer(LongWord(@ABuffer) + Offset)^);
   Extent.ReleaseLock;
   if Length = 0 then Exit;
   
   Inc(Start,Length);
   Dec(Remain,Length);
   Inc(Offset,(Length shl FSectorShiftCount));
  end;
  
 Result:=(Remain = 0);
end;

{==============================================================================}

function TVirtualDiskVboxImage.Allocated(ASector:LongWord;ACount:Word):Word;
begin
 {}
 Result:=ACount;

 //To Do //Use TestExtent ?  //Required for Copy Image / Resize Image etc
end;

{==============================================================================}

function TVirtualDiskVboxImage.CreateImage(AMediaType:TMediaType;AFloppyType:TFloppyType;AAttributes:LongWord;ASectorSize:Word;const ASectorCount:Int64;ACylinders,AHeads,ASectors:LongWord;APartitionId:Byte):Integer;
begin
 {}
 Result:=0;
 
 if not WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FController = nil then Exit;
  if Length(FName) = 0 then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVboxImage.CreateImage - Name = ' + FName);
  {$ENDIF}

  {Check Open}
  if FBase <> nil then Exit;

  {Setup Parameters}

  {Add Extent}
  //To Do //

  {Add Table}
  //To Do //
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVboxImage.OpenImage(AMediaType:TMediaType;AFloppyType:TFloppyType;AAttributes:LongWord;ASectorSize:Word;const ASectorCount:Int64;ACylinders,AHeads,ASectors:LongWord;APartitionId:Byte):Integer;
begin
 {}
 Result:=0;
 
 if not WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FController = nil then Exit;
  if Length(FName) = 0 then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVboxImage.OpenImage - Name = ' + FName);
  {$ENDIF}

  {Check Open}
  if FBase <> nil then Exit;

  {Setup Parameters}
  {if (AMediaType <> mtUNKNOWN) and (AMediaType <> mtINVALID) then FMediaType:=AMediaType;}    {Ignore MediaType}
  {if (AFloppyType <> ftUNKNOWN) and (AFloppyType <> ftINVALID) then FFloppyType:=AFloppyType;}{Ignore FloppyType}
  {if AAttributes <> iaNone then FAttributes:=AAttributes;}                                    {Ignore Attributes}
  {if ASectorSize > 0 then FSectorSize:=ASectorSize;}                                          {Ignore SectorSize}
  {if ASectorCount > 0 then FSectorCount:=ASectorCount;}                                       {Ignore SectorCount}
  {if ACylinders > 0 then FCylinders:=ACylinders;}                                             {Ignore Cylinders}
  {if AHeads > 0 then FHeads:=AHeads;}                                                         {Ignore Heads}
  {if ASectors > 0 then FSectors:=ASectors;}                                                   {Ignore Sectors}
  {if APartitionId <> pidUnused then FPartitionId:=APartitionId;}                              {Ignore PartitionId}
  if (AAttributes and iaReadable) <> iaNone then FAttributes:=(FAttributes or iaReadable) else FAttributes:=(FAttributes and not(iaReadable));
  if (AAttributes and iaWriteable) <> iaNone then FAttributes:=(FAttributes or iaWriteable) else FAttributes:=(FAttributes and not(iaWriteable));

  {Check Extent}
  if not CheckExtent(nil,nil,FName) then Exit;

  {Load Extents}
  if not LoadExtents then Exit;

  {Load Tables}
  if not LoadTables then Exit;

  Result:=FImageNo;
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVboxImage.CloseImage:Boolean;
begin
 {}
 Result:=False;
 
 if not WriterLock then Exit;
 try
  if FDriver = nil then Exit;
  if FController = nil then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualDiskVboxImage.CloseImage - Name = ' + FName);
  {$ENDIF}

  {Check Open}
  if FBase = nil then Exit;

  {Close Extents}
  if not CloseExtents then Exit;

  Result:=True;
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVboxImage.ResizeImage(const ASectorCount:Int64):Boolean;
begin
 {}
 Result:=False;
 
 if not WriterLock then Exit;
 try
  //To Do
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVboxImage.CreateSnapshot:Boolean;
begin
 {}
 Result:=False;
 
 if not WriterLock then Exit;
 try
  //To Do
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVboxImage.DeleteSnapshot:Boolean;
begin
 {}
 Result:=False;
 
 if not WriterLock then Exit;
 try
  //To Do
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskVboxImage.MergeSnapshot:Boolean;
begin
 {}
 Result:=False;
 
 if not WriterLock then Exit;
 try
  //To Do
 finally  
  WriterUnlock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{TVirtualRecognizer}
constructor TVirtualRecognizer.Create(ADriver:TFileSysDriver;AController:TDiskController);
begin
 {}
 inherited Create(ADriver);
 FResizer:=TVirtualDiskResizer.Create(ADriver,Self);
 FCopier:=TVirtualDiskCopier.Create(ADriver,Self);
 FImager:=TVirtualDiskImager.Create(ADriver,Self,AController);
 
 FController:=AController;
end;

{==============================================================================}

destructor TVirtualRecognizer.Destroy;
begin
 {}
 WriterLock;
 try
  FController:=nil;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  inherited Destroy;
 end; 
end;

{==============================================================================}

function TVirtualRecognizer.GetName:String;
begin
 {}
 Result:='Virtual';
end;

{==============================================================================}

function TVirtualRecognizer.RecognizeImage(AImage:TDiskImage):Boolean;
{Note: Caller must hold the image lock}
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if AImage = nil then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualRecognizer.RecognizeImage - Image = ' + AImage.Name);
  {$ENDIF}
  
  {Check Image Type}
  case AImage.ImageType of
   itMEMORY,itFILE,itDEVICE,itISO,itBOCHS,itVMWARE,itVPC,itVBOX:begin
     {$IFDEF VIRTUAL_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualRecognizer.RecognizeImage - Image Recognized');
     {$ENDIF}
     
     Result:=True;
    end;
  end;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualRecognizer.MountImage(AImage:TDiskImage):Boolean;
{Note: Caller must hold the image lock}
var
 Device:TDiskDevice;
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FController = nil then Exit;
  if AImage = nil then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualRecognizer.MountImage - Image = ' + AImage.Name);
  {$ENDIF}
  
  {Check Ready}
  if not AImage.Ready then Exit;

  {Check Recognized}
  if not RecognizeImage(AImage) then Exit;

  {Check Device}
  if AImage.Device = nil then
   begin
    {Create Device}
    Device:=TVirtualDiskDevice.Create(FDriver,FController,AImage,nil,FDriver.GetNextDeviceNo(AImage.MediaType),AImage.Name);
    
    {Init Device}
    Device.DeviceInit;
    
    {Locate Partitions}
    Device.LocatePartitions;
    
    {Locate Volumes and Drives}
    FDriver.LocateVolumes;
    FDriver.LocateDrives;
   end;
     
  Result:=True;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualRecognizer.InsertImage(AImage:TDiskImage):Boolean;
{Note: Caller must hold the image lock}
var
 Device:TDiskDevice;
 Volume:TDiskVolume;
 Drive:TDiskDrive;
 Recognizer:TRecognizer;
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FController = nil then Exit;
  if AImage = nil then Exit;

  {$IFDEF VIRTUAL_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TVirtualRecognizer.InsertImage - Image = ' + AImage.Name);
  {$ENDIF}
  {Check Ready}
  if not AImage.Ready then Exit;

  {Check Recognized}
  if not RecognizeImage(AImage) then Exit;

  {Get the Device}
  Device:=FDriver.GetDeviceByImage(AImage,True,FILESYS_LOCK_WRITE);
  if Device = nil then Exit;
  try
   if not Device.Removable then Exit;
   Device.DeviceInit;

   {Get the Volume}
   Volume:=FDriver.GetVolumeByDevice(Device,True,FILESYS_LOCK_WRITE);
   if Volume = nil then Exit;
   try
    Volume.VolumeInit;
  
    {Get the Drive}
    Drive:=FDriver.GetDriveByDevice(Device,True,FILESYS_LOCK_WRITE);
    if Drive <> nil then
     begin
      Drive.DriveInit;
    
      {Unlock Drive}
      Drive.WriterUnlock;
     end; 

    {Get the Recognizer}
    Recognizer:=FDriver.GetRecognizerByVolume(Volume,True,FILESYS_LOCK_READ);
    if Recognizer <> nil then
     begin
      {Mount Volume}
      Recognizer.MountVolume(Volume,Drive);
      
      {Unlock Recognizer}
      Recognizer.ReaderUnlock;
     end;

    Result:=True;
   finally
    {Unlock Volume}
    Volume.WriterUnlock;
   end;
  finally
   {Unlock Device}
   Device.WriterUnlock;
  end;  
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{TVirtualDiskResizer}
constructor TVirtualDiskResizer.Create(ADriver:TFileSysDriver;ARecognizer:TRecognizer);
begin
 {}
 inherited Create(ADriver,ARecognizer);
end;

{==============================================================================}

destructor TVirtualDiskResizer.Destroy;
begin
 {}
 WriterLock;
 try
  {Nothing}
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  inherited Destroy;
 end; 
end;

{==============================================================================}

function TVirtualDiskResizer.AcceptImage(AImage:TDiskImage;const ASize:Int64):Boolean;
{Note: Caller must hold the image lock}
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FRecognizer = nil then Exit;

  {Check Image}
  if AImage = nil then Exit;
  
  {Check Type}
  case AImage.ImageType of
   itMEMORY,itFILE,itDEVICE,itISO,itBOCHS,itVMWARE,itVPC,itVBOX:begin
     {Shrink / Expand Image}
     {Cannot Resize if not Writeable}
     if not AImage.Writeable then Exit;
     
     {Cannot Resize a Device}
     if AImage.ImageType = itDEVICE then Exit;
     
     {Shrink / Expand Image}
     if ASize = 0 then Exit;
     
     Result:=True;
    end;
  end;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskResizer.ShrinkImage(AImage:TDiskImage;const ASize:Int64):Boolean;
{Note: Caller must hold the image lock}
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FRecognizer = nil then Exit;

  {Check Size}
  if ASize = 0 then Exit;

  {Check Accepted}
  if not AcceptImage(AImage,ASize) then Exit;

  //To Do //
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskResizer.ExpandImage(AImage:TDiskImage;const ASize:Int64):Boolean;
{Note: Caller must hold the image lock}
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FRecognizer = nil then Exit;

  {Check Size}
  if ASize = 0 then Exit;

  {Check Accepted}
  if not AcceptImage(AImage,ASize) then Exit;

  //To Do //
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{TVirtualDiskCopier}
constructor TVirtualDiskCopier.Create(ADriver:TFileSysDriver;ARecognizer:TRecognizer);
begin
 {}
 inherited Create(ADriver,ARecognizer);
end;

{==============================================================================}

destructor TVirtualDiskCopier.Destroy;
begin
 {}
 WriterLock;
 try
  {Nothing}
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  inherited Destroy;
 end; 
end;

{==============================================================================}

function TVirtualDiskCopier.AcceptImage(AImage,ADest:TDiskImage):Boolean;
{Note: Caller must hold the image lock}
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FRecognizer = nil then Exit;

  {Check Image}
  if AImage = nil then Exit;
  
  {Check Type}
  case AImage.ImageType of
   itMEMORY,itFILE,itDEVICE,itISO,itBOCHS,itVMWARE,itVPC,itVBOX:begin
     {Copy Image}
     if ADest = nil then Exit;
     
     {Cannot Copy if not Writeable}
     if not ADest.Writeable then Exit;
     
     {Cannot Copy if not same Size}
     if AImage.SectorSize <> ADest.SectorSize then Exit;
     if AImage.SectorCount <> ADest.SectorCount then Exit;
     
     Result:=True;
    end;
  end;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskCopier.CopyImage(AImage,ADest:TDiskImage):Boolean;
{Note: Caller must hold the image and dest locks}
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FRecognizer = nil then Exit;

  {Check Destination}
  if ADest = nil then Exit;

  {Check Accepted}
  if not AcceptImage(AImage,ADest) then Exit;

  //To Do //Read from Source/Write to Dest for SectorCount sectors
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{TVirtualDiskImager}
constructor TVirtualDiskImager.Create(ADriver:TFileSysDriver;ARecognizer:TRecognizer;AController:TDiskController);
begin
 {}
 inherited Create(ADriver,ARecognizer);
 FController:=AController;
end;

{==============================================================================}

destructor TVirtualDiskImager.Destroy;
begin
 {}
 WriterLock;
 try
  FController:=nil;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  inherited Destroy;
 end; 
end;

{==============================================================================}

function TVirtualDiskImager.CreateImageByType(const AName:String;AImageType:TImageType):TDiskImage;
begin
 {}
 Result:=nil;

 if FDriver = nil then Exit;
 if FRecognizer = nil then Exit;
 if FController = nil then Exit;

 {Check Image Type}
 case AImageType of
  itMEMORY:Result:=TVirtualDiskMemoryImage.Create(FDriver,FController,AName,FDriver.GetNextImageNo);
  itFILE:Result:=TVirtualDiskFileImage.Create(FDriver,FController,AName,FDriver.GetNextImageNo);
  itDEVICE:Result:=TVirtualDiskDeviceImage.Create(FDriver,FController,AName,FDriver.GetNextImageNo);
  itISO:Result:=TVirtualDiskIsoImage.Create(FDriver,FController,AName,FDriver.GetNextImageNo);
  itBOCHS:Result:=TVirtualDiskBochsImage.Create(FDriver,FController,AName,FDriver.GetNextImageNo);
  itVMWARE:Result:=TVirtualDiskVmwareImage.Create(FDriver,FController,AName,FDriver.GetNextImageNo);
  itVPC:Result:=TVirtualDiskVpcImage.Create(FDriver,FController,AName,FDriver.GetNextImageNo);
  itVBOX:Result:=TVirtualDiskVboxImage.Create(FDriver,FController,AName,FDriver.GetNextImageNo);
 end;
end;

{==============================================================================}

function TVirtualDiskImager.AcceptImage(AImage:TDiskImage;const AName:String;AImageType:TImageType;AMediaType:TMediaType;AFloppyType:TFloppyType;AAttributes:LongWord;ASectorSize:Word;const ASectorCount:Int64):Boolean;
{Note: Caller must hold the image lock}
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FRecognizer = nil then Exit;
  if FController = nil then Exit;

  {Check Name}
  if Length(AName) > 0 then
   begin
    {Check Image}
    if AImage <> nil then
     begin
      if AImage.ImageType <> AImageType then Exit;
      if AImage.MediaType <> AMediaType then Exit; //To Do //What about change from CD to DVD ? //case AImage.MediaType of instead
     end;
     
    {Check Type}
    case AImageType of
     itMEMORY,itFILE,itDEVICE,itISO,itBOCHS,itVMWARE,itVPC,itVBOX:begin
       {Create / Open Image}
       if AImageType = itDEVICE then
        begin
         {Open Image (Cannot Create Device)}
         if (AAttributes and iaDisk) = iaDisk then
          begin
           if FDriver.GetDeviceByName(AName,False,FILESYS_LOCK_NONE) = nil then Exit; {Do not lock}
           
           Result:=True;
          end
         else if (AAttributes and iaPartition) = iaPartition then
          begin
           if FDriver.GetPartitionByPath(AName,False,FILESYS_LOCK_NONE) = nil then Exit; {Do not lock}
           
           Result:=True;
          end
         else if (AAttributes and iaVolume) = iaVolume then
          begin
           if FDriver.GetVolumeByName(AName,False,FILESYS_LOCK_NONE) = nil then Exit; {Do not lock}
           
           Result:=True;
          end
         else if (AAttributes and iaDrive) = iaDrive then
          begin
           if FDriver.GetDriveByName(AName,False,FILESYS_LOCK_NONE) = nil then Exit; {Do not lock}
           
           Result:=True;
          end;
        end
       else if AImageType = itMEMORY then
        begin
         {Create Image (Cannot Open Memory}
         if ASectorCount = 0 then Exit;
         
         Result:=True;
        end
       else
        begin
         if FDriver.FileExists(AName) then
          begin
           {Create / Open Image}
           Result:=True;
          end
         else
          begin
           {Create Image}
           if ASectorCount = 0 then Exit;
           
           Result:=True;
          end;
        end;
      end;
    end;
   end
  else
   begin
    {Check Image}
    if AImage = nil then Exit;
    
    {Check Type}
    case AImage.ImageType of
     itMEMORY,itFILE,itDEVICE,itISO,itBOCHS,itVMWARE,itVPC,itVBOX:begin
       {Close Image}
       Result:=True;
      end;
    end;
   end;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskImager.CreateImage(AImage:TDiskImage;const AName:String;AImageType:TImageType;AMediaType:TMediaType;AFloppyType:TFloppyType;AAttributes:LongWord;ASectorSize:Word;const ASectorCount:Int64;ACylinders,AHeads,ASectors:LongWord;APartitionId:Byte):Integer;
{Note: Caller must hold the image lock}
var
 Image:TDiskImage;
begin
 {}
 Result:=0;
 
 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FRecognizer = nil then Exit;
  if FController = nil then Exit;
  {if AImage = nil then Exit;} {Passed Image may be nil}

  {Check Accepted}
  if not AcceptImage(AImage,AName,AImageType,AMediaType,AFloppyType,AAttributes,ASectorSize,ASectorCount) then Exit;

  {Create Image}
  Image:=AImage;
  if Image = nil then Image:=CreateImageByType(AName,AImageType) else Image.Name:=AName;
  if Image = nil then Exit;
  try
   {Create Image}
   if Image.CreateImage(AMediaType,AFloppyType,AAttributes,ASectorSize,ASectorCount,ACylinders,AHeads,ASectors,APartitionId) = 0 then Exit;
   if not Image.ImageInit then Exit;
   
   Result:=Image.ImageNo;
  finally
   if (Result = 0) and (AImage = nil) then Image.Free;
  end;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskImager.OpenImage(AImage:TDiskImage;const AName:String;AImageType:TImageType;AMediaType:TMediaType;AFloppyType:TFloppyType;AAttributes:LongWord;ASectorSize:Word;const ASectorCount:Int64;ACylinders,AHeads,ASectors:LongWord;APartitionId:Byte):Integer;
{Note: Caller must hold the image lock}
var
 Image:TDiskImage;
begin
 {}
 Result:=0;
 
 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FRecognizer = nil then Exit;
  if FController = nil then Exit;
  {if AImage = nil then Exit;} {Passed Image may be nil}

  {Check Accepted}
  if not AcceptImage(AImage,AName,AImageType,AMediaType,AFloppyType,AAttributes,ASectorSize,ASectorCount) then Exit;

  {Create Image}
  Image:=AImage;
  if Image = nil then Image:=CreateImageByType(AName,AImageType) else Image.Name:=AName;
  if Image = nil then Exit;
  try
   {Open Image}
   if Image.OpenImage(AMediaType,AFloppyType,AAttributes,ASectorSize,ASectorCount,ACylinders,AHeads,ASectors,APartitionId) = 0 then Exit;
   if not Image.ImageInit then Exit;
   
   Result:=Image.ImageNo;
  finally
   if (Result = 0) and (AImage = nil) then Image.Free;
  end;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskImager.CloseImage(AImage:TDiskImage):Boolean;
{Note: Caller must hold the image lock}
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FRecognizer = nil then Exit;
  if FController = nil then Exit;
  if AImage = nil then Exit;

  {Check Accepted}
  if not AcceptImage(AImage,'',itUNKNOWN,mtUNKNOWN,ftUNKNOWN,iaNone,0,0) then Exit;

  {Close Image}
  if not AImage.CloseImage then Exit;
  if not AImage.ImageInit then Exit;

  Result:=True;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskImager.AcceptSnapshot(AImage:TDiskImage):Boolean;
{Note: Caller must hold the image lock}
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FRecognizer = nil then Exit;
  if FController = nil then Exit;
  if AImage = nil then Exit;

  {Check Type}
  case AImage.ImageType of
   itVMWARE,itVPC,itVBOX:begin
     {Create / Delete / Merge Snapshot}
     Result:=True;
    end;
  end;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskImager.CreateSnapshot(AImage:TDiskImage):Boolean;
{Note: Caller must hold the image lock}
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FRecognizer = nil then Exit;
  if FController = nil then Exit;
  if AImage = nil then Exit;

  {Check Accepted}
  if not AcceptSnapshot(AImage) then Exit;

  {Create Snapshot}
  if not AImage.CreateSnapshot then Exit;

  Result:=True;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskImager.DeleteSnapshot(AImage:TDiskImage):Boolean;
{Note: Caller must hold the image lock}
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FRecognizer = nil then Exit;
  if FController = nil then Exit;
  if AImage = nil then Exit;

  {Check Accepted}
  if not AcceptSnapshot(AImage) then Exit;

  {Create Snapshot}
  if not AImage.DeleteSnapshot then Exit;

  Result:=True;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TVirtualDiskImager.MergeSnapshot(AImage:TDiskImage):Boolean;
{Note: Caller must hold the image lock}
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FRecognizer = nil then Exit;
  if FController = nil then Exit;
  if AImage = nil then Exit;

  {Check Accepted}
  if not AcceptSnapshot(AImage) then Exit;

  {Create Snapshot}
  if not AImage.MergeSnapshot then Exit;

  Result:=True;
 finally  
  ReaderUnlock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{TVirtualDiskExtent}
constructor TVirtualDiskExtent.Create(AImage:TVirtualDiskImage;ADelta,AParent:TVirtualDiskExtent);
begin
 {}
 inherited Create;
 FLock:=MutexCreateEx(False,MUTEX_DEFAULT_SPINCOUNT,MUTEX_FLAG_RECURSIVE);

 FImage:=AImage;
 FDelta:=ADelta;
 FParent:=AParent;

 FFlags:=virtualFlagNone;

 FHandle:=INVALID_HANDLE_VALUE;
 FFilename:='';

 FStartSector:=0;
 FSectorCount:=0;
end;

{==============================================================================}

destructor TVirtualDiskExtent.Destroy;
begin
 {}
 AcquireLock;
 try
  FImage:=nil;
  FDelta:=nil;
  FParent:=nil;
  
  inherited Destroy;
 finally 
  ReleaseLock; {Cannot destroy Mutex while holding lock} 
  MutexDestroy(FLock);
 end;
end;

{==============================================================================}

function TVirtualDiskExtent.GetFilename:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;
 
 Result:=FFilename;
 UniqueString(Result);
 
 ReleaseLock;
end;

{==============================================================================}

procedure TVirtualDiskExtent.SetFilename(const AFilename:String);
begin
 {}
 if not AcquireLock then Exit;
 
 FFilename:=AFilename;
 UniqueString(FFilename);
 
 ReleaseLock;
end;

{==============================================================================}

function TVirtualDiskExtent.GetBlockShiftCount:LongWord;
begin
 {Base Implementation}
 Result:=0;
 
 if FImage = nil then Exit;
 if FBlockSize = 0 then Exit;
 if FImage.SectorSize = 0 then Exit;
 
 while (FBlockSize shr Result) > FImage.SectorSize do
  begin
   Inc(Result);
  end;
 if (FBlockSize shr Result) < FImage.SectorSize then Result:=0;
end;

{==============================================================================}

function TVirtualDiskExtent.AcquireLock:Boolean;
begin
 {}
 Result:=(MutexLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TVirtualDiskExtent.ReleaseLock:Boolean;
begin
 {}
 Result:=(MutexUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TVirtualDiskExtent.IsFixed:Boolean;
begin
 {Base Implementation}
 Result:=(FFlags and virtualFlagFixed) = virtualFlagFixed;
end;

{==============================================================================}

function TVirtualDiskExtent.IsDynamic:Boolean;
begin
 {Base Implementation}
 Result:=(FFlags and virtualFlagDynamic) = virtualFlagDynamic;
end;

{==============================================================================}

function TVirtualDiskExtent.IsBase:Boolean;
begin
 {Base Implementation}
 Result:=(FFlags and virtualFlagBase) = virtualFlagBase;
end;

{==============================================================================}

function TVirtualDiskExtent.IsDelta:Boolean;
begin
 {Base Implementation}
 Result:=(FFlags and virtualFlagDelta) = virtualFlagDelta;
end;

{==============================================================================}

function TVirtualDiskExtent.IsDevice:Boolean;
begin
 {Base Implementation}
 Result:=(FFlags and virtualFlagDevice) = virtualFlagDevice;
end;

{==============================================================================}

function TVirtualDiskExtent.HasDelta:Boolean;
begin
 {Base Implementation}
 Result:=(FDelta <> nil);
end;

{==============================================================================}
{==============================================================================}
{TVirtualDiskVmwareExtent}
function TVirtualDiskVmwareExtent.IsDescriptor:Boolean;
begin
 {}
 Result:=False;
 //To Do
end;

{==============================================================================}

function TVirtualDiskVmwareExtent.HasDescriptor:Boolean;
begin
 {}
 Result:=False;
 //To Do
end;

{==============================================================================}
{==============================================================================}
{TVirtualDiskVpcExtent}
constructor TVirtualDiskVpcExtent.Create(AImage:TVirtualDiskImage;ADelta,AParent:TVirtualDiskExtent);
begin
 {}
 inherited Create(AImage,ADelta,AParent);
 FHeader:=GetMem(SizeOf(TVpcHardDiskFooter));
 FFooter:=GetMem(SizeOf(TVpcHardDiskFooter));
 FSparse:=GetMem(SizeOf(TVpcDynamicDiskHeader));
 FTable:=TVirtualDiskVpcTable.Create(AImage,Self);
 FGroups:=TFileSysList.Create;
 FGroupLocal:=MutexCreate;
end;

{==============================================================================}

destructor TVirtualDiskVpcExtent.Destroy;
begin
 {}
 AcquireLock;
 try
  if FGroups <> nil then FGroups.Free;
  FGroups:=nil;
  MutexDestroy(FGroupLocal);
  if FTable <> nil then FTable.Free;
  FTable:=nil;
  FreeMem(FHeader);
  FreeMem(FFooter);
  FreeMem(FSparse);
 finally 
  ReleaseLock; {Cannot destroy Mutex while holding lock} 
  inherited Destroy;
 end; 
end;

{==============================================================================}

function TVirtualDiskVpcExtent.HasFooter:Boolean;
begin
 {}
 Result:=False;
 //To Do //Used by Split Images to indicate which Extent holds the Footer
 Result:=IsDynamic or IsDelta;
end;

{==============================================================================}
{==============================================================================}
{TVirtualDiskVboxExtent}
constructor TVirtualDiskVboxExtent.Create(AImage:TVirtualDiskImage;ADelta,AParent:TVirtualDiskExtent);
begin
 {}
 inherited Create(AImage,ADelta,AParent);
 FHeader:=GetMem(SizeOf(TVboxDiskHeader));
 FTable:=TVirtualDiskVboxTable.Create(AImage,Self);
end;

{==============================================================================}

destructor TVirtualDiskVboxExtent.Destroy;
begin
 {}
 AcquireLock;
 try
  if FTable <> nil then FTable.Free;
  FTable:=nil;
  FreeMem(FHeader);
 finally 
  ReleaseLock; {Cannot destroy Mutex while holding lock} 
  inherited Destroy;
 end; 
end;

{==============================================================================}
{==============================================================================}
{TVirtualDiskTable}
constructor TVirtualDiskTable.Create(AImage:TVirtualDiskImage;AExtent:TVirtualDiskExtent);
begin
 {}
 inherited Create;
 FImage:=AImage;
 FExtent:=AExtent;

 FTableOffset:=0;
 FTableSize:=0;

 FStartSector:=0;
 FSectorCount:=0;
end;

{==============================================================================}

destructor TVirtualDiskTable.Destroy;
begin
 {}
 FImage:=nil;
 FExtent:=nil;
 inherited Destroy;
end;

{==============================================================================}
{==============================================================================}
{TVirtualDiskVmwareTable}

{==============================================================================}
{==============================================================================}
{TVirtualDiskVpcTable}
constructor TVirtualDiskVpcTable.Create(AImage:TVirtualDiskImage;AExtent:TVirtualDiskExtent);
begin
 {}
 inherited Create(AImage,AExtent);
 FData:=nil;
end;

{==============================================================================}

destructor TVirtualDiskVpcTable.Destroy;
begin
 {}
 if FData <> nil then FreeMem(FData);
 FData:=nil;
 inherited Destroy;
end;

{==============================================================================}
{==============================================================================}
{TVirtualDiskVboxTable}
constructor TVirtualDiskVboxTable.Create(AImage:TVirtualDiskImage;AExtent:TVirtualDiskExtent);
begin
 {}
 inherited Create(AImage,AExtent);
 FData:=nil;
end;

{==============================================================================}

destructor TVirtualDiskVboxTable.Destroy;
begin
 {}
 if FData <> nil then FreeMem(FData);
 FData:=nil;
 inherited Destroy;
end;

{==============================================================================}
{==============================================================================}
{TVirtualDiskBlock}
constructor TVirtualDiskBlock.Create(AImage:TVirtualDiskImage;ATable:TVirtualDiskTable);
begin
 {}
 inherited Create;
 FImage:=AImage;
 FTable:=ATable;

 FBlockOffset:=0;
 FBlockSize:=0;

 FStartSector:=0;
 FSectorCount:=0;
end;

{==============================================================================}

destructor TVirtualDiskBlock.Destroy;
begin
 {}
 FImage:=nil;
 FTable:=nil;
 inherited Destroy;
end;

{==============================================================================}
{==============================================================================}
{TVirtualDiskVmwareBlock}

{==============================================================================}
{==============================================================================}
{TVirtualDiskVpcGroup}
constructor TVirtualDiskVpcGroup.Create(AImage:TVirtualDiskImage;ATable:TVirtualDiskTable);
begin
 {}
 inherited Create;
 FBlocks:=TFileSysList.Create;
 FBlockLocal:=MutexCreate;
end;

{==============================================================================}

destructor TVirtualDiskVpcGroup.Destroy;
begin
 {}
 if FBlocks <> nil then FBlocks.Free;
 FBlocks:=nil;
 MutexDestroy(FBlockLocal);
 inherited Destroy;
end;

{==============================================================================}
{==============================================================================}
{TVirtualDiskVpcBlock}
constructor TVirtualDiskVpcBlock.Create(AImage:TVirtualDiskImage;ATable:TVirtualDiskTable);
begin
 {}
 inherited Create(AImage,ATable);
 FData:=nil;
end;

{==============================================================================}

destructor TVirtualDiskVpcBlock.Destroy;
begin
 {}
 if FData <> nil then FreeMem(FData);
 FData:=nil;
 inherited Destroy;
end;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure VirtualInit;
var
 Recognizer:TVirtualRecognizer;
 Controller:TVirtualDiskController;
begin
 {}
 {Check Initialized}
 if VirtualInitialized then Exit;
 
 {Check Driver}
 if FileSysDriver = nil then Exit;
 
 {Check Virtual Enabled}
 if FILESYS_VIRTUAL_ENABLED then
  begin
   {Create Virtual Disk Controller}
   Controller:=TVirtualDiskController.Create(FileSysDriver);
   Controller.ControllerInit;
   
   {Create Virtual Recognizer}
   Recognizer:=TVirtualRecognizer.Create(FileSysDriver,Controller);
   Recognizer.AllowDrive:=FILESYS_DRIVES_ENABLED;
  end;
 
 VirtualInitialized:=True;
end;

{==============================================================================}

procedure VirtualQuit;
var
 Next:TDiskController;
 Current:TDiskController;
begin
 {}
 {Check Initialized}
 if not VirtualInitialized then Exit;
 
 {Check Driver}
 if FileSysDriver = nil then Exit;
 
 {Terminate Disk Controllers}
 Next:=FileSysDriver.GetControllerByNext(nil,True,False,FILESYS_LOCK_READ);
 while Next <> nil do
  begin
   Current:=Next;
   Next:=FileSysDriver.GetControllerByNext(Current,True,False,FILESYS_LOCK_READ); 

   if Current is TVirtualDiskController then
    begin
     {Convert Controller}
     Current.ReaderConvert;
     
     {FileSysDriver.RemoveController(Current);} {Done by Destroy}
     Current.Free;
    end
   else
    begin
     {Unlock Controller}
     Current.ReaderUnlock;
    end;    
  end;
 
 VirtualInitialized:=False;
end;

{==============================================================================}
{==============================================================================}
{Virtual Disk Functions}

{==============================================================================}
{==============================================================================}
{Virtual Disk Helper Functions}
function VirtualDataToPointer(const AData;ASize:Integer;ASwap:Boolean):Pointer;
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
       PWord(LongWord(Result) + LongWord(Count))^:=SwapEndian(PWord(LongWord(Result) + LongWord(Count))^);
       Inc(Count,2); {SizeOf(Word)}
      end;
    end;
  end;
end;

{==============================================================================}

function VirtualPointerToData(APointer:Pointer;var AData;ASize:Integer;ASwap:Boolean):Boolean;
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
       PWord(LongWord(@AData) + LongWord(Count))^:=SwapEndian(PWord(LongWord(@AData) + LongWord(Count))^);
       Inc(Count,2); {SizeOf(Word)}
      end;
    end;
  end;
  
 Result:=True;
end;

{=============================================================================}

function VirtualDataToString(const AData;ASize:Integer;AUnicode:Boolean):String;
{Converts the supplied data to a string}
{Accounts for unicode and byte ordering}
var
 Count:Integer;
 Length:Integer;
 Buffer:Pointer;
 Terminator:PChar;
begin
 {}
 Result:='';
 
 {Check Size}
 if ASize > 0 then
  begin
   {Check Unicode}
   if AUnicode then
    begin
     {Get Buffer}
     Buffer:=VirtualDataToPointer(AData,ASize,True);
     if Buffer = nil then Exit;
     try
      {Get Length}
      Length:=(ASize shr 1);
      
      {Check Count}
      Count:=Unicode.WideCharToMultiByte(CP_ACP,0,PWideChar(Buffer),Length,nil,0,nil,nil);
      
      {$IFDEF VIRTUAL_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('VirtualDataToString - Size = ' + IntToStr(ASize) + ' Length = ' + IntToStr(Length) + ' Count = ' + IntToStr(Count));
      {$ENDIF}
      
      if Count <= Length then
       begin
        SetString(Result,nil,Count); {Count does not include null terminator}
        Unicode.WideCharToMultiByte(CP_ACP,0,PWideChar(Buffer),Length,PChar(Result),Count,nil,nil);
        {if Byte(Result[Count]) = 0 then SetLength(Result,Count - 1);} {Some CDs contain illegal null terminators}
        Terminator:=StrScan(PChar(Result),#0);
        if PtrUInt(Terminator) < (PtrUInt(Result) + LongWord(Count)) then
         begin
          SetLength(Result,PtrUInt(Terminator) - PtrUInt(Result));
         end;
       end;
     finally
      FreeMem(Buffer);
     end;
    end
   else
    begin
     {$IFDEF VIRTUAL_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('VirtualDataToString - Size = ' + IntToStr(ASize));
     {$ENDIF}
     
     SetString(Result,nil,ASize); {Size does not include null terminator}
     Unicode.OemToCharBuff(PChar(@AData),PChar(Result),ASize);
     {if Byte(Result[ASize]) = 0 then SetLength(Result,ASize - 1);} {Some CDs contain illegal null terminators}
     Terminator:=StrScan(PChar(Result),#0);
     if PtrUInt(Terminator) < (PtrUInt(Result) + LongWord(ASize)) then
      begin
       SetLength(Result,PtrUInt(Terminator) - PtrUInt(Result));
      end;
    end;
  end;
end;

{=============================================================================}

function VirtualStringToData(const AString:String;var AData;ASize:Integer;AUnicode:Boolean):Boolean;
{Converts the supplied string to data}
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
     FillChar(AData,ASize,0);
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
        
        {$IFDEF VIRTUAL_DEBUG}
        if FILESYS_LOG_ENABLED then FileSysLogDebug('VirtualStringToData - Size = ' + IntToStr(ASize) + ' Length = ' + IntToStr(Size) + ' Count = ' + IntToStr(Count));
        {$ENDIF}
        
        if Count > Size then Exit;
        if Unicode.MultiByteToWideChar(CP_ACP,0,PChar(AString),Length(AString),PWideChar(Buffer),Size) = 0 then Exit;
        if not VirtualPointerToData(Buffer,AData,ASize,True) then Exit;
       finally
        FreeMem(Buffer);
       end;
      end;
      
     Result:=True;
    end
   else
    begin
     {$IFDEF VIRTUAL_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('VirtualStringToData - Size = ' + IntToStr(ASize));
     {$ENDIF}
     
     FillChar(AData,ASize,0);
     if Length(AString) > 0 then
      begin
       Unicode.CharToOemBuff(PChar(AString),PChar(@AData),ASize);
      end;
      
     Result:=True;
    end;
  end;
end;

{==============================================================================}
{==============================================================================}

initialization
 VirtualInit;

{==============================================================================}
 
finalization
 VirtualQuit;

{==============================================================================}
{==============================================================================}

end.
