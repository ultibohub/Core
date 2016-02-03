{
Ultibo NTFS classes unit.

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

        All Names are currently String but must be modified to be WideString //To Do

        In all cases, Size is the size of a component in bytes
                      Length is the size of a component in characters (or other eg words)

        In all cases, Create means create a new object (possibly added to list/tree) but do not update (only used when loading)
                      New means create a new object (possibly added to list/tree) and update values
                      
        In all cases, Delete means delete from list/tree but do not free object
                      Remove means delete from list/tree and free the object
                      Destroy means delete from list/tree and free the object (only used when loading)

 Attribute Lists:     By nature of what it does, the Attribute List attribute must be resident in the
                      base record or else there would be no way to find the extension records which
                      are referenced only by the attribute list.

                      Because attributes must be stored in type order in the MFT then this also means
                      that the Standard Information attribute must be in the base record as it has a
                      lower type number ($10) than Attribute List ($20).

                      Since the Attribute List does not reference itself it would appear that once the
                      data runs for a non resident Attribute List reach the size of an MFT record (minus
                      headers and the Standard Information attribute) then the MFT record cannot grow
                      any further. Need to try and confirm this by testing although it would seem
                      reasonable as multiple Attribute Lists would make sorting very difficult.

 Multiple Instances:  It appears from testing that all attributes that are non resident can have multiple
                      instances except Attribute List as noted above. This includes Data, Allocation and
                      Bitmap.

                      Therefore all reading/writing to attribute values should go via the ReadRun/WriteRun
                      functions which handle Sparse, Compressed and multiple instance behaviour. This should
                      include MFT and Index as well.

 Security Descriptor: (Disk Format)
                      The ordering of items within the SecurityDescriptor data appears to always be:

                       Header
                       Sacl
                       Dacl
                       Owner
                       Group

                      Note: This is only true for Self Relative descriptors as used by NTFS etc

                      (API Format)
                      Even though the SecurityDescriptor structure header in Windows lists the Sacl before
                      the Dacl, in practice the Dacl is always before the Sacl in the actual layout of the
                      data. That is, the offset for the Sacl (if present) is always higher than the Dacl.

                      The ordering of items within the SecurityDescriptor data appears to always be:

                       Header
                       Owner
                       Group
                       Dacl
                       Sacl

                      Note: This is only true for Self Relative descriptors as used by NTFS etc
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit NTFSClass;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Threads,FileSystem,SysUtils,Classes,Unicode,Security,Ultibo,UltiboUtils,UltiboClasses,
     NTFSConst,NTFSTypes,NTFSUtils;
     
//To Do //Look for:

//Testing
   
//WideString
  //Change to UnicodeString for FPC
   
//Critical   
  
//Protected

//Lock

//) = Uppercase(  //Use WorkBuffer
  
{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}
     
{==============================================================================}
type
 {NTFS specific classes}
 {Inherited Classes}
  {Table}
 TNTFSDiskEntry = class;
 TNTFSDiskTable = class(TDiskTable)  {Respresents the $Mft and $MftMirr}
   constructor Create(ALocalLock:TMutexHandle);
   destructor Destroy; override;
  private
   {Private Variables}

   {Table Variables}
   FStartSector:Int64;              {Absolute starting sector of this table}
   FStartCluster:Int64;             {Absolute starting cluster of this table}

   {Object Variables}
   FEntry:TNTFSDiskEntry;

   {Private Methods}
  public
   {Public Properties}

   {Table Properties}
   property StartSector:Int64 read FStartSector write FStartSector;
   property StartCluster:Int64 read FStartCluster write FStartCluster;

   {Object Properties}
   property Entry:TNTFSDiskEntry read FEntry write FEntry;

   {Public Methods}
 end;

  {Block}
 TNTFSDiskBlock = class(TDiskBlock)  {Represents a block of $Bitmap entries}
   constructor Create(ALocalLock:TMutexHandle);
   destructor Destroy; override;
  private
   {Private Variables}

   {Block Variables}
   FBlockCount:LongWord;            {Number of cluster entries in this block} {Bits in the bitmap}
   FBlockBuffer:Pointer;
   FBlockCluster:Int64;             {First cluster represented by this block}
                                    {BlockNo represents the VCN of the block}
   {Private Methods}
  public
   {Public Properties}

   {Block Properties}
   property BlockCount:LongWord read FBlockCount write FBlockCount;
   property BlockBuffer:Pointer read FBlockBuffer write FBlockBuffer;
   property BlockCluster:Int64 read FBlockCluster write FBlockCluster;

   {Public Methods}
 end;

  {Entry}
 TNTFSDiskRecord = class;
 TNTFSDiskAttribute = class;
 TNTFSDiskEntry = class(TDiskEntry)  {Represents a file entry}
   constructor Create(ALocalLock:TMutexHandle;AOrigin:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute);
   destructor Destroy; override;
  private
   {Private Variables}
   FPrevEntry:TNTFSDiskEntry;
   FNextEntry:TNTFSDiskEntry;

   {Entry Variables}
   FUsed:Int64;                                {Used size of the file}
   FAllocated:Int64;                           {Allocated size of file}
   FChangeTime:TFileTime;                      {MFT record change time}

   {Object Variables}
   FOrigin:TNTFSDiskRecord;                    {Entry origin record}
   FAttribute:TNTFSDiskAttribute;              {Entry naming attribute}
   FAlternate:TNTFSDiskAttribute;              {Alternate name attribute}

   {Private Methods}
   procedure SetOrigin(AOrigin:TNTFSDiskRecord);
  public
   {Public Properties}
   property LocalLock:TMutexHandle read FLocalLock write FLocalLock;
   
   property PrevEntry:TNTFSDiskEntry read FPrevEntry write FPrevEntry;
   property NextEntry:TNTFSDiskEntry read FNextEntry write FNextEntry;

   {Entry Properties}
   property Used:Int64 read FUsed write FUsed;
   property Allocated:Int64 read FAllocated write FAllocated;
   property ChangeTime:TFileTime read FChangeTime write FChangeTime;

   {Object Properties}
   property Origin:TNTFSDiskRecord read FOrigin write SetOrigin;
   property Attribute:TNTFSDiskAttribute read FAttribute write FAttribute;
   property Alternate:TNTFSDiskAttribute read FAlternate write FAlternate;

   {Dot Methods}
   function GetDot:TNTFSDiskEntry;
   function GetDotDot:TNTFSDiskEntry;

   function CreateDot:TNTFSDiskEntry;
   function CreateDotDot:TNTFSDiskEntry;

   function UpdateDot:Boolean;
   function UpdateDotDot(AEntry:TNTFSDiskEntry):Boolean;

   {Public Methods}
   function RecordNumber:Int64;
   function FileReference:Int64;

   function UpdateEntry:Boolean;
   function UpdateRecord:Boolean;

   function FindFirstName(AHandle:TFindHandle;AReference:Boolean):TDiskEntry; override;
   function FindPrevName(AHandle:TFindHandle;AReference:Boolean):TDiskEntry; override;
   function FindNextName(AHandle:TFindHandle;AReference:Boolean):TDiskEntry; override;
   function FindLastName(AHandle:TFindHandle;AReference:Boolean):TDiskEntry; override;
 end;

  {Acl}
 TNTFSDiskAcl = class(TDiskAcl)      {Data of attribute $SECURITY_DESCRIPTOR} //To Do //No - See TNTFSSecurity - This is the more abstract level - Attached to Entry
   constructor Create(ALocalLock:TMutexHandle);
   destructor Destroy; override;
  private
   {Private Variables}

   {Acl Variables}
   //To Do //

   {Object Variables}
   //To Do //

   {Private Methods}
   //To Do //
  public
   {Public Properties}
   //To Do //

   {Acl Properties}
   //To Do //

   {Object Properties}
   //To Do //

   {Public Methods}
   //To Do //
 end;

  {Ace}
 TNTFSDiskAce = class(TDiskAce)      {Data of attribute $SECURITY_DESCRIPTOR} //To Do //No - See TNTFSSecurity - This is the more abstract level - Attached to Entry
   constructor Create(ALocalLock:TMutexHandle);
   destructor Destroy; override;
  private
   {Private Variables}

   {Ace Variables}
   //To Do //

   {Object Variables}
   //To Do //

   {Private Methods}
   //To Do //
  public
   {Public Properties}
   //To Do //

   {Ace Properties}
   //To Do //

   {Object Properties}
   //To Do //

   {Public Methods}
   //To Do //
 end;

 {Non Inherited Classes}
  {Entry}
 TNTFSEntryList = class(TObject)
   constructor Create;
   destructor Destroy; override;
  private
   {Private Variables}
   FEntryCount:Integer;
   FFirstEntry:TNTFSDiskEntry;
   FLastEntry:TNTFSDiskEntry;

   {Private Methods}
   function Link(AEntry:TNTFSDiskEntry):Boolean;
   function Unlink(AEntry:TNTFSDiskEntry):Boolean;
  public
   {Public Properties}
   property EntryCount:Integer read FEntryCount;
   property FirstEntry:TNTFSDiskEntry read FFirstEntry;
   property LastEntry:TNTFSDiskEntry read FLastEntry;

   {Public Methods}
   function Add(AEntry:TNTFSDiskEntry):Boolean;
   function Remove(AEntry:TNTFSDiskEntry):Boolean;

   procedure ClearList; virtual;
 end;

  {Record}
 TNTFSRecordList = class(TObject)
   constructor Create;
   destructor Destroy; override;
  private
   {Private Variables}
   FRecordCount:Integer;
   FFirstRecord:TNTFSDiskRecord;
   FLastRecord:TNTFSDiskRecord;

   {Private Methods}
   function Link(ARecord:TNTFSDiskRecord):Boolean;
   function Unlink(ARecord:TNTFSDiskRecord):Boolean;
  public
   {Public Properties}
   property RecordCount:Integer read FRecordCount;
   property FirstRecord:TNTFSDiskRecord read FFirstRecord;
   property LastRecord:TNTFSDiskRecord read FLastRecord;

   {Public Methods}
   function Add(ARecord:TNTFSDiskRecord):Boolean;
   function Remove(ARecord:TNTFSDiskRecord):Boolean;

   procedure ClearList; virtual;
 end;

 TNTFSRecordIndex = class(TLinkedObjBtree) {Index of FILE records}
   constructor Create;
   destructor Destroy; override;
  private
   {Private Variables}
   FLock:TSynchronizerHandle; 
   FRecordLocal:TMutexHandle;
   
   FRunsLock:TSynchronizerHandle; 
   FItemsLock:TSynchronizerHandle; 
   FNodesLock:TSynchronizerHandle; 
   FIndexLock:TSynchronizerHandle; 
   FExtendedsLock:TSynchronizerHandle; 
   FAttributesLock:TSynchronizerHandle; 
   
   FRunLocal:TMutexHandle;
   FItemLocal:TMutexHandle;
   FKeyLocal:TMutexHandle;
   FNodeLocal:TMutexHandle;
   FExtendedLocal:TMutexHandle;
   FAttributeLocal:TMutexHandle;
   
   {Private Methods}
   function Find(const ARecordNumber:Int64;ACurrent:TNTFSDiskRecord):TNTFSDiskRecord;
  protected
   {Protected Variables}

   {Protected Methods}
   function CreateBlank:TBtreeObject; override;

   function Compare(AEntry1,AEntry2:TBtreeObject):Integer; override;
  public
   {Public Properties}

   {Record Methods}
   function CreateRecord(ABase:TNTFSDiskRecord;const ARecordNumber:Int64;AVersion:Word):TNTFSDiskRecord;
   function DestroyRecord(ARecord:TNTFSDiskRecord):Boolean;
   function NewRecord(ABase:TNTFSDiskRecord;const ARecordNumber:Int64;AVersion:Word):TNTFSDiskRecord;
   function InsertRecord(ARecord:TNTFSDiskRecord):Boolean;
   function DeleteRecord(ARecord:TNTFSDiskRecord):Boolean;
   function RemoveRecord(ARecord:TNTFSDiskRecord):Boolean;

   {Public Methods}
   function FindRecord(const ARecordNumber:Int64):TNTFSDiskRecord;
   
   function ReaderLock:Boolean;
   function ReaderUnlock:Boolean;
   function ReaderConvert:Boolean;
   function WriterLock:Boolean;
   function WriterUnlock:Boolean;
   function WriterConvert:Boolean;
   
   function RunsReaderLock:Boolean;
   function RunsReaderUnlock:Boolean;
   function RunsWriterLock:Boolean;
   function RunsWriterUnlock:Boolean;

   function ItemsReaderLock:Boolean;
   function ItemsReaderUnlock:Boolean;
   function ItemsWriterLock:Boolean;
   function ItemsWriterUnlock:Boolean;

   function NodesReaderLock:Boolean;
   function NodesReaderUnlock:Boolean;
   function NodesWriterLock:Boolean;
   function NodesWriterUnlock:Boolean;
   
   function IndexReaderLock:Boolean;
   function IndexReaderUnlock:Boolean;
   function IndexWriterLock:Boolean;
   function IndexWriterUnlock:Boolean;

   function ExtendedsReaderLock:Boolean;
   function ExtendedsReaderUnlock:Boolean;
   function ExtendedsWriterLock:Boolean;
   function ExtendedsWriterUnlock:Boolean;
   
   function AttributesReaderLock:Boolean;
   function AttributesReaderUnlock:Boolean;
   function AttributesWriterLock:Boolean;
   function AttributesWriterUnlock:Boolean;
 end;

 TNTFSDiskItem = class;
 TNTFSAttributeKey = class;
 TNTFSDiskAttributes = class;
 TNTFSDiskRecord = class(TBtreeObject)  {Represents a FILE record entry}
   constructor Create(ALocalLock:TMutexHandle;ABase:TNTFSDiskRecord);
   destructor Destroy; override;
  private
   {Private Variables}
   FLocalLock:TMutexHandle;
   
   FRunsLock:TSynchronizerHandle; 
   FItemsLock:TSynchronizerHandle; 
   FNodesLock:TSynchronizerHandle; 
   FIndexLock:TSynchronizerHandle; 
   FExtendedsLock:TSynchronizerHandle; 
   FAttributesLock:TSynchronizerHandle; 
   
   FRunLocal:TMutexHandle;
   FItemLocal:TMutexHandle;
   FKeyLocal:TMutexHandle;
   FNodeLocal:TMutexHandle;
   FExtendedLocal:TMutexHandle;
   FAttributeLocal:TMutexHandle;
   
   FStatus:LongWord;

   FPrevRecord:TNTFSDiskRecord;
   FNextRecord:TNTFSDiskRecord;

   {Record Variables} {TNTFSFileRecord}
   FRecordFlags:Word;                          {Flags} {See Consts}
   FHardLinkCount:Word;                        {Hard link count}
   FSequenceNumber:Word;                       {Sequence number}
   FNextAttributeId:Word;                      {Next Attribute Id}
   FRecordNumber:Int64;                        {Number of this MFT Record}
   FRecordSize:LongWord;                       {Actual size of the FILE record}
   FRecordAllocated:LongWord;                  {Allocated size of the FILE record}

   FAttributeOffset:Word;                      {Offset to the first Attribute}
   FUpdateSequenceOffset:Word;                 {Offset to the Update Sequence Record}
   FUpdateSequenceLength:Word;                 {Size in words of the Update Sequence Record}

   FUpdateSequenceNumber:Word;                 {Update Sequence Number}
   FLogFileSequenceNumber:Int64;               {LogFile Sequence Number (LSN)}

   {Object Variables}
   FBase:TNTFSDiskRecord;                      {Base file record}
   FLinks:TNTFSEntryList;                      {List of record links}
   FStreams:TNTFSEntryList;                    {List of record streams}
   FRecords:TNTFSRecordList;                   {List of child records}
   FAttributes:TNTFSDiskAttributes;            {List of record attributes}

   {Private Methods}
   function AcquireLock:Boolean;
   function ReleaseLock:Boolean;
   
   function GetResizing:Boolean;
   procedure SetResizing(AValue:Boolean);
   function GetRemoving:Boolean;
   procedure SetRemoving(AValue:Boolean);

   function GetMirrored:Boolean;
   procedure SetMirrored(AValue:Boolean);
   function GetMetafile:Boolean;
   procedure SetMetafile(AValue:Boolean);
   function GetReserved:Boolean;
   procedure SetReserved(AValue:Boolean);
   function GetExpansion:Boolean;
   procedure SetExpansion(AValue:Boolean);

   function GetOverflow:Boolean;
   procedure SetOverflow(AValue:Boolean);
   function GetExtension:Boolean;
   procedure SetExtension(AValue:Boolean);

   function GetIsUsed:Boolean;
   procedure SetIsUsed(AValue:Boolean);
   function GetIsFolder:Boolean;
   procedure SetIsFolder(AValue:Boolean);
   function GetIsUnknown1:Boolean;
   procedure SetIsUnknown1(AValue:Boolean);
   function GetIsIndexView:Boolean;
   procedure SetIsIndexView(AValue:Boolean);

   function GetOrigin:TNTFSDiskRecord;

   function CreateLinks(ANew:Boolean):TNTFSEntryList;
   function CreateStreams(ANew:Boolean):TNTFSEntryList;
   function CreateRecords(ANew:Boolean):TNTFSRecordList;
   function CreateAttributes(AVersion:Word;ANew:Boolean):TNTFSDiskAttributes;
  public
   {Public Properties}
   property Resizing:Boolean read GetResizing write SetResizing;    {Record is being Resized}
   property Removing:Boolean read GetRemoving write SetRemoving;    {Record is being Removed}

   property Mirrored:Boolean read GetMirrored write SetMirrored;    {Record is in $MftMirr file}
   property Metafile:Boolean read GetMetafile write SetMetafile;    {Record is an NTFS metafile}
   property Reserved:Boolean read GetReserved write SetReserved;    {Record is an Mft reserved record}
   property Expansion:Boolean read GetExpansion write SetExpansion; {Record is an Mft expansion record}

   property Overflow:Boolean read GetOverflow write SetOverflow;    {Record has an attribute list attribute}
   property Extension:Boolean read GetExtension write SetExtension; {Record is an extension of the base record}

   property PrevRecord:TNTFSDiskRecord read FPrevRecord write FPrevRecord;
   property NextRecord:TNTFSDiskRecord read FNextRecord write FNextRecord;

   property IsUsed:Boolean read GetIsUsed write SetIsUsed;                 {Record is in use}
   property IsFolder:Boolean read GetIsFolder write SetIsFolder;           {Record is a folder}
   property IsUnknown1:Boolean read GetIsUnknown1 write SetIsUnknown1;     {Record is an ?????} //To Do //IndexOnly ?
   property IsIndexView:Boolean read GetIsIndexView write SetIsIndexView;  {Record is an index view}

   {Record Properties}
   property RecordFlags:Word read FRecordFlags write FRecordFlags;
   property HardLinkCount:Word read FHardLinkCount write FHardLinkCount;
   property SequenceNumber:Word read FSequenceNumber write FSequenceNumber;
   property NextAttributeId:Word read FNextAttributeId write FNextAttributeId;
   property RecordNumber:Int64 read FRecordNumber write FRecordNumber;
   property RecordSize:LongWord read FRecordSize write FRecordSize;
   property RecordAllocated:LongWord read FRecordAllocated write FRecordAllocated;

   property AttributeOffset:Word read FAttributeOffset write FAttributeOffset;
   property UpdateSequenceOffset:Word read FUpdateSequenceOffset write FUpdateSequenceOffset;
   property UpdateSequenceLength:Word read FUpdateSequenceLength write FUpdateSequenceLength;

   property UpdateSequenceNumber:Word read FUpdateSequenceNumber write FUpdateSequenceNumber;
   property LogFileSequenceNumber:Int64 read FLogFileSequenceNumber write FLogFileSequenceNumber;

   {Object Properties}
   property Base:TNTFSDiskRecord read FBase write FBase;
   property Links:TNTFSEntryList read FLinks;
   property Streams:TNTFSEntryList read FStreams;
   property Records:TNTFSRecordList read FRecords;
   property Attributes:TNTFSDiskAttributes read FAttributes;

   property Origin:TNTFSDiskRecord read GetOrigin;

   {Link Methods}
   function CreateLink(AAttribute,AAlternate:TNTFSDiskAttribute;ANew:Boolean):TNTFSDiskEntry;
   function DestroyLink(ALink:TNTFSDiskEntry):Boolean;
   function NewLink(AAttribute,AAlternate:TNTFSDiskAttribute):TNTFSDiskEntry;
   function GetLink(AAttribute:TNTFSDiskAttribute):TNTFSDiskEntry;
   function DeleteLink(ALink:TNTFSDiskEntry):Boolean;
   function RenameLink(ALink:TNTFSDiskEntry;AAttribute,AAlternate:TNTFSDiskAttribute):Boolean;

   {Stream Methods}
   function CreateStream(AAttribute:TNTFSDiskAttribute;ANew:Boolean):TNTFSDiskEntry;
   function DestroyStream(AStream:TNTFSDiskEntry):Boolean;
   function NewStream(AAttribute:TNTFSDiskAttribute):TNTFSDiskEntry;
   function GetStream(AAttribute:TNTFSDiskAttribute):TNTFSDiskEntry;
   function DeleteStream(AStream:TNTFSDiskEntry):Boolean;

   {FileName Methods}
   function GetFileNameByKey(AKey:TNTFSAttributeKey):TNTFSDiskAttribute;
   function GetFileNameByName(const AName:String;AInstance:Integer):TNTFSDiskAttribute; {Change to WideString}
   function GetFileNameByParent(const AName:String;const AParent:Int64;AInstance:Integer):TNTFSDiskAttribute; {Change to WideString}
   function GetFileNameByNameSpace(const AName:String;const AParent:Int64;ANameSpace:Byte;AInstance:Integer):TNTFSDiskAttribute; {Change to WideString}

   {Record Methods}
   function GetRecord(AInstance:Integer):TNTFSDiskRecord;
   function GetRecordByFree(AFree:LongWord;AExclude:Boolean):TNTFSDiskRecord;
   function GetRecordByReference(const AFileReference:Int64):TNTFSDiskRecord;

   {Record Methods}
   function AddRecord(ARecord:TNTFSDiskRecord):Boolean;
   function RemoveRecord(ARecord:TNTFSDiskRecord):Boolean;

   {Attribute Methods}
   function GetAttributeByItem(AItem:TNTFSDiskItem):TNTFSDiskAttribute;
   function GetAttributeByStatus(AInclude,AExclude:Word;AInstance:Integer):TNTFSDiskAttribute;
   function GetAttributeByVCN(AAttribute:TNTFSDiskAttribute;const AVCN:Int64;var AInstance:LongWord):TNTFSDiskAttribute;
   function GetAttributeByUnit(AAttribute:TNTFSDiskAttribute;const AUnit:Int64;var AInstance:LongWord):TNTFSDiskAttribute;

   {Attribute Methods}
   function CreateAttribute(AType:LongWord;AVersion:Word;ANew:Boolean):TNTFSDiskAttribute;
   function DestroyAttribute(AAttribute:TNTFSDiskAttribute):Boolean;
   function NewAttribute(APrevious:TNTFSDiskAttribute;AType:LongWord;const AName:String;AVersion:Word):TNTFSDiskAttribute; {Change to WideString}
   function GetAttribute(AType:LongWord;const AName:String;AInstance:Integer):TNTFSDiskAttribute; {Change to WideString}
   function RemoveAttribute(AAttribute:TNTFSDiskAttribute):Boolean;
   function MoveAttribute(ADest:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute):Boolean;
   function RenameAttribute(AAttribute:TNTFSDiskAttribute;const AName:String):Boolean; {Change to WideString}

   {Public Methods}
   function FileReference:Int64;
   function BaseReference:Int64;                                 {File reference to the base FILE record}

   function LinkCount:LongWord;
   function StreamCount:LongWord;
   function RecordCount:LongWord;
   function AttributeCount:LongWord;

   function RecordFree:LongWord;
   function AttributeSize(AVersion:Word;AType:LongWord):LongWord;

   function CalculatedSize(AVersion:Word):LongWord;
   function CalculatedOffset(AVersion:Word):Word;
   function CalculatedSequenceOffset(AVersion:Word):Word;
   function CalculatedSequenceLength(ASectorSize:Word):Word;
   function CalculatedStreamUsed(AVersion:Word;AAttribute:TNTFSDiskAttribute):Int64;

   function ReadAttributes(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
   function WriteAttributes(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;

   function ReadRecord(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word;AFree:Boolean):Boolean;
   function WriteRecord(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
 end;

 TNTFSCompareSecurityDescriptor = function(ASecurityId:LongWord;ADescriptor:Pointer;ASize:Word):Boolean of Object;

  {Index}
 TNTFSUpCase = class;
 TNTFSDiskKey = class;
 TNTFSDiskNode = class;
 TNTFSDiskNodes = class;
 TNTFSDiskIndex = class(TLinkedObjBtree) {Index of INDX records}
   constructor Create(AKeyLocal:TMutexHandle;ALock:TSynchronizerHandle;AVolumeVersion,ASectorSize:Word;AIndexType,ACollateRule:LongWord;AAttribute:TNTFSDiskAttribute);
   destructor Destroy; override;
  private
   {Private Variables}
   FLock:TSynchronizerHandle;
   FKeyLocal:TMutexHandle;
   
   FStatus:LongWord;

   FSectorSize:Word;
   FVolumeVersion:Word;

   {Index Variables} {TNTFSIndexRoot}
   FIndexType:LongWord;            {Attribute Type} {Or None if Data Index}
   FCollateRule:LongWord;          {Collation Rule}
   FIndexRecordSize:LongWord;      {Size of Index Allocation Entry (bytes)}
   FIndexCounterOffset:LongWord;   {Index Record Number increment}

   {Index Variables} {Not Stored}
   FClustersPerIndex:LongWord;     {Clusters per INDX Record or 0 if less than cluster size}
   FIndexsPerCluster:LongWord;     {INDX Records per Cluster or 0 if more than cluster size}

   FIndexCounterShift:Word;        {Shift count for Record -> Counter}
   FIndexRecordShiftCount:Word;    {Shift count for Record -> VCN}
   FIndexRecordOffsetMask:Int64;   {Mask for Record offset calculation}

   FTotalIndexRecordCount:Int64;   {Total number of index records in index}

   FLastFreeIndexRecord:Int64;     {Or ntfsUnknownRecordNumber if not known}
   FFreeIndexRecordCount:Int64;    {Or ntfsUnknownRecordNumber if not known}

   FCompareSecurityDescriptor:TNTFSCompareSecurityDescriptor;

   {Object Variables}
   FUpCase:TNTFSUpCase;            {Uppercase Conversion Table}

   FNodes:TNTFSDiskNodes;          {List of index nodes}
   FAttribute:TNTFSDiskAttribute;  {Attribute owning this index}

   {Private Methods}
   function GetNodesLock:TSynchronizerHandle;
   
   function GetKeyLocal:TMutexHandle;
   function GetNodeLocal:TMutexHandle;
   
   function GetLoaded:Boolean;
   procedure SetLoaded(AValue:Boolean);
   function GetChanged:Boolean;

   function GetUpCase:PNTFSUpCaseData;
   function GetRootNode:TNTFSDiskNode;

   function CreateNodes(ANew:Boolean):TNTFSDiskNodes;

   function CompareKey(AEntry1,AEntry2:Pointer;ASize1,ASize2:Word):Integer;

   function CompareBinary(AEntry1,AEntry2:Pointer;ASize1,ASize2:Word):Integer;
   function CompareFileName(AEntry1,AEntry2:PWideChar;ASize1,ASize2:Word):Integer;
   function CompareUnicode(AEntry1,AEntry2:PWideChar;ASize1,ASize2:Word):Integer;
   function CompareLongWord(AEntry1,AEntry2:LongWord;ASize1,ASize2:Word):Integer;
   function CompareSID(AEntry1,AEntry2:PSID;ASize1,ASize2:Word):Integer;
   function CompareSecurityHash(AEntry1,AEntry2:PNTFSSecurityHashKeyData;ASize1,ASize2:Word):Integer;
   function CompareGUID(AEntry1,AEntry2:PGUID;ASize1,ASize2:Word):Integer;
  protected
   {Protected Variables}

   {Protected Methods}
   function GetEnd(AEntry:TBtreeObject):TBtreeObject; override;
   function GetStart(AEntry:TBtreeObject):TBtreeObject; override;
   function GetBlank(AEntry:TBtreeObject):TBtreeObject; override;
   function GetMedian(AEntry:TBtreeObject):TBtreeObject; override;

   function GetDropTest(AEntry:TBtreeObject;var ALeft:Boolean):TBtreeObject; //override; //To Do //Remove ?
   function GetDrop(AEntry:TBtreeObject;var ALeft:Boolean):TBtreeObject; override;
   function GetMerge(AEntry:TBtreeObject):TBtreeObject; override;
   function GetBorrow(AEntry:TBtreeObject):TBtreeObject; override;
   function GetTarget(ADrop:TBtreeObject;ALeft:Boolean):TBtreeObject; override;

   {Protected Methods}
   function PushNode(AEntry:TBtreeObject):Boolean; override;
   function SplitNode(AEntry:TBtreeObject):Boolean; override;
   function DropNode(AEntry,ADrop,ATarget:TBtreeObject;ALeft:Boolean):Boolean; override;
   function DropNodeOld(AEntry,ADrop:TBtreeObject;ALeft:Boolean):Boolean; override; //To Do //Remove ?
   function MergeNode(AEntry,AMerge:TBtreeObject):Boolean; override;
   function BorrowEntry(AEntry,ABorrow:TBtreeObject):Boolean; override;

   function SwapEntry(AEntry,ASwap:TBtreeObject;ALeft:Boolean):Boolean; override;
   function SetParentEntry(AEntry,AParent:TBtreeObject):Boolean; override;

   function DeleteBlank(ABlank:TBtreeObject):Boolean; override;

   function AttachBlank(ABlank:TBtreeObject):Boolean; override;
   function DetachBlank(ABlank:TBtreeObject):Boolean; override;

   function AttachEntry(AEntry:TBtreeObject):Boolean; override;
   function DetachEntry(AEntry:TBtreeObject):Boolean; override;

   function RequirePush(AEntry:TBtreeObject):Boolean; override;
   function RequireSplit(AEntry:TBtreeObject):Boolean; override;
   function RequireDrop(AEntry:TBtreeObject):Boolean; override;
   function RequireMerge(AEntry:TBtreeObject):Boolean; override;
   function RequireBorrow(AEntry:TBtreeObject):Boolean; override;
  public
   {Public Properties}
   property Loaded:Boolean read GetLoaded write SetLoaded;
   property Changed:Boolean read GetChanged;

   property SectorSize:Word read FSectorSize write FSectorSize;
   property VolumeVersion:Word read FVolumeVersion;

   {Index Properties}
   property IndexType:LongWord read FIndexType write FIndexType;
   property CollateRule:LongWord read FCollateRule write FCollateRule;
   property IndexRecordSize:LongWord read FIndexRecordSize write FIndexRecordSize;
   property IndexCounterOffset:LongWord read FIndexCounterOffset write FIndexCounterOffset;

   {Index Properties}
   property ClustersPerIndex:LongWord read FClustersPerIndex write FClustersPerIndex;
   property IndexsPerCluster:LongWord read FIndexsPerCluster write FIndexsPerCluster;

   property IndexCounterShift:Word read FIndexCounterShift write FIndexCounterShift;
   property IndexRecordShiftCount:Word read FIndexRecordShiftCount write FIndexRecordShiftCount;
   property IndexRecordOffsetMask:Int64 read FIndexRecordOffsetMask write FIndexRecordOffsetMask;

   property TotalIndexRecordCount:Int64 read FTotalIndexRecordCount write FTotalIndexRecordCount;

   property LastFreeIndexRecord:Int64 read FLastFreeIndexRecord write FLastFreeIndexRecord;
   property FreeIndexRecordCount:Int64 read FFreeIndexRecordCount write FFreeIndexRecordCount;

   property CompareSecurityDescriptor:TNTFSCompareSecurityDescriptor read FCompareSecurityDescriptor write FCompareSecurityDescriptor;

   {Object Properties}
   property UpCase:TNTFSUpCase read FUpCase write FUpCase;

   property Nodes:TNTFSDiskNodes read FNodes;
   property Attribute:TNTFSDiskAttribute read FAttribute;

   property RootNode:TNTFSDiskNode read GetRootNode;

   function NodeCount:Integer;

   {Key Methods}
   function CreateKey(ANode:TNTFSDiskNode;ABlank:Boolean):TNTFSDiskKey; virtual;
   function DestroyKey(AKey:TNTFSDiskKey):Boolean;
   function NewKey(AKey,AData:Pointer;AKeySize,ADataSize:Word):TNTFSDiskKey; virtual;
   function NewKeyEx(AKey,AData,APadding:Pointer;AKeySize,ADataSize,APaddingSize:Word):TNTFSDiskKey; virtual;
   function AddKey(AParent,AKey:TNTFSDiskKey):Boolean;
   function InsertKey(AKey:TNTFSDiskKey):Boolean;
   function RemoveKey(AKey:TNTFSDiskKey):Boolean;

   {Node Methods}
   function CreateNode(ANew:Boolean):TNTFSDiskNode;
   function DestroyNode(ANode:TNTFSDiskNode):Boolean;
   function NewNode(const ARecordNumber:Int64):TNTFSDiskNode;
   function GetNode(const ARecordNumber:Int64):TNTFSDiskNode;
   function DeleteNode(ANode:TNTFSDiskNode):Boolean;
   function RemoveNode(ANode:TNTFSDiskNode):Boolean;

   {Public Methods}
   function UpdateNodes(AVersion:Word):Boolean; virtual;

   function FindKey(AValue:Pointer;ASize:Word):TNTFSDiskKey; virtual;

   function ReadRoot(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
   function WriteRoot(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;

   function ReadKeys(AParent:TNTFSDiskKey;ANode:TNTFSDiskNode;ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
   function WriteKeys(AParent:TNTFSDiskKey;ANode:TNTFSDiskNode;ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
   
   function ReaderLock:Boolean;
   function ReaderUnlock:Boolean;
   function WriterLock:Boolean;
   function WriterUnlock:Boolean;
 end;

 TNTFSDataKey = class;
 TNTFSDataIndex = class(TNTFSDiskIndex)
  private
   {Private Variables}

   {Index Variables}

   {Private Methods}
   function CompareSecurityDescriptor(AHash1,AHash2,ASecurityId:LongWord;ADescriptor:Pointer;ASize:Word):Integer;

   function Find(AValue:Pointer;ASize:Word;ACurrent:TNTFSDataKey):TNTFSDataKey;
   function FindEx(AValue:Pointer;ASize:Word;AHash:LongWord;ACurrent:TNTFSDataKey):TNTFSDataKey;
  protected
   {Protected Variables}

   {Protected Methods}
   function CreateBlank:TBtreeObject; override;

   function Compare(AEntry1,AEntry2:TBtreeObject):Integer; override;
  public
   {Public Properties}

   {Key Methods}
   function CreateKey(ANode:TNTFSDiskNode;ABlank:Boolean):TNTFSDiskKey; override;
   function NewKey(AKey,AData:Pointer;AKeySize,ADataSize:Word):TNTFSDiskKey; override;

   {Key Methods}
   //function GetKeyBySID(ASID:PSID):TNTFSDiskKey;        //To Do //Do we need these ? - Seems not ! - May be useful later
   //function GetKeyByOwner(AOwner:LongWord):TNTFSDiskKey;
   //function GetKeyByObjectId(const AObjectId:TGUID):TNTFSDiskKey;
   //function GetKeyBySecurity(ASecurity:TNTFSSecurity):TNTFSDiskKey;
   //function GetKeyByReparse(AReparseTag:LongWord;const AFileReference:Int64):TNTFSDiskKey;

   {Public Methods}
   function FindKey(AValue:Pointer;ASize:Word):TNTFSDiskKey; override;
 end;

 TNTFSPaddedKey = class;
 TNTFSPaddedIndex = class(TNTFSDataIndex) {A DataIndex that allows a block of Padding to be specified}
  private
   {Private Variables}

   {Index Variables}

   {Private Methods}
  protected
   {Protected Variables}

   {Protected Methods}
   function CreateBlank:TBtreeObject; override;
  public
   {Public Properties}

   {Key Methods}
   function CreateKey(ANode:TNTFSDiskNode;ABlank:Boolean):TNTFSDiskKey; override;
   function NewKey(AKey,AData:Pointer;AKeySize,ADataSize:Word):TNTFSDiskKey; override;
   function NewKeyEx(AKey,AData,APadding:Pointer;AKeySize,ADataSize,APaddingSize:Word):TNTFSDiskKey; override;

   {Public Methods}
 end;

 {TNTFSAttributeKey = class;} {Declared above}
 TNTFSAttributeIndex = class(TNTFSDiskIndex)
  private
   {Private Variables}

   {Index Variables}

   {Private Methods}
   function Find(AValue:Pointer;ASize:Word;ACurrent:TNTFSAttributeKey):TNTFSAttributeKey;
  protected
   {Protected Variables}

   {Protected Methods}
   function CreateBlank:TBtreeObject; override;

   function Compare(AEntry1,AEntry2:TBtreeObject):Integer; override;
  public
   {Public Properties}

   {Index Properties}

   {Key Methods}
   function CreateKey(ANode:TNTFSDiskNode;ABlank:Boolean):TNTFSDiskKey; override;
   function NewKey(AKey,AData:Pointer;AKeySize,ADataSize:Word):TNTFSDiskKey; override;

   {Key Methods}
   function GetKeyByFileName(const AFileName:String):TNTFSDiskKey;

   {Attribute Methods}
   function NewKeyByAttribute(AAttribute:TNTFSDiskAttribute):TNTFSDiskKey;
   function GetKeyByAttribute(AAttribute:TNTFSDiskAttribute):TNTFSDiskKey;

   {Public Methods}
   function FindKey(AValue:Pointer;ASize:Word):TNTFSDiskKey; override;
 end;

  {Node}
 TNTFSDiskNodes = class(TFileSysListEx)
   constructor Create(ANodeLocal:TMutexHandle;ALock:TSynchronizerHandle);
   destructor Destroy; override;
  private
   {Private Variables}
   FNodeLocal:TMutexHandle;
   
   {Private Methods}
   function GetModified:Boolean;
   procedure SetModified(AValue:Boolean);
  public
   {Public Properties}
   property Modified:Boolean read GetModified write SetModified;

   {Public Methods}
   function TotalSize:LongWord;

   function NodeCount:LongWord;
 end;

 TNTFSDiskNode = class(TListObject)  {Represents an INDX record header}
   constructor Create(ALocalLock:TMutexHandle;AIndex:TNTFSDiskIndex);
   destructor Destroy; override;
  private
   {Private Variables}
   FLocalLock:TMutexHandle;
   
   FStatus:LongWord;

   {Index Variables} {TNTFSIndexRecord / TNTFSIndexHeader}
   FIndexFlags:Word;                         {Flags} {See Consts}
   FIndexSize:LongWord;                      {Total size of the Index Entries}
   FIndexAllocated:LongWord;                 {Allocated size of the Index Entries}
   FEntryOffset:LongWord;                    {Offset to first Index Entry}

   FRecordNumber:Int64;                      {Number of this INDX record in the Index Allocation} //To Do //This appears to actually be the VCN in the data run //Recheck all this //What happens if cluster is > IndexRecordSize ?
   FUpdateSequenceOffset:Word;               {Offset to the Update Sequence Record}
   FUpdateSequenceLength:Word;               {Size in words of the Update Sequence Record}

   FUpdateSequenceNumber:Word;               {Update Sequence Number}
   FLogFileSequenceNumber:Int64;             {LogFile sequence number}

   {Object Variables}
   FIndex:TNTFSDiskIndex;                    {Index owning this node}
   FBlank:TNTFSDiskKey;                      {Blank key of this node} {Blank keys do not move}

   {Private Methods}
   function AcquireLock:Boolean;
   function ReleaseLock:Boolean;

   function GetAdded:Boolean;
   procedure SetAdded(AValue:Boolean);
   function GetDeleted:Boolean;
   procedure SetDeleted(AValue:Boolean);
   function GetChanged:Boolean;
   procedure SetChanged(AValue:Boolean);

   function GetModified:Boolean;
   procedure SetModified(AValue:Boolean);

   function GetIsRoot:Boolean;

   function GetHasSubNodes:Boolean;
   procedure SetHasSubNodes(AValue:Boolean);

   function GetStart:TNTFSDiskKey;
  public
   {Public Properties}
   property Added:Boolean read GetAdded write SetAdded;          {Index record to be allocated}
   property Deleted:Boolean read GetDeleted write SetDeleted;    {Index record to be deallocated}
   property Changed:Boolean read GetChanged write SetChanged;    {Index record to be updated}

   property Modified:Boolean read GetModified write SetModified; {Index record has been added, deleted or changed}

   property IsRoot:Boolean read GetIsRoot;

   property HasSubNodes:Boolean read GetHasSubNodes write SetHasSubNodes;

   {Index Properties}
   property IndexFlags:Word read FIndexFlags write FIndexFlags;
   property IndexSize:LongWord read FIndexSize write FIndexSize;
   property IndexAllocated:LongWord read FIndexAllocated write FIndexAllocated;
   property EntryOffset:LongWord read FEntryOffset write FEntryOffset;

   property RecordNumber:Int64 read FRecordNumber write FRecordNumber;
   property UpdateSequenceOffset:Word read FUpdateSequenceOffset write FUpdateSequenceOffset;
   property UpdateSequenceLength:Word read FUpdateSequenceLength write FUpdateSequenceLength;

   property UpdateSequenceNumber:Word read FUpdateSequenceNumber write FUpdateSequenceNumber;
   property LogFileSequenceNumber:Int64 read FLogFileSequenceNumber write FLogFileSequenceNumber;

   {Object Properties}
   property Index:TNTFSDiskIndex read FIndex;
   property Blank:TNTFSDiskKey read FBlank write FBlank;
   property Start:TNTFSDiskKey read GetStart;

   function KeyCount:Integer;

   function IndexFree:LongWord;

   {Public Methods}
   function UpdateKeys(AVersion:Word):Boolean; virtual;

   function CalculatedSize(AVersion:Word):LongWord;
   function CalculatedOffset(AVersion:Word):LongWord;
   function CalculatedAllocated(AVersion:Word):LongWord;
   function CalculatedSequenceOffset(AVersion:Word):Word;
   function CalculatedSequenceLength(ASectorSize:Word):Word;

   function WriteEmpty(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word;var AUpdateSequenceOffset,AUpdateSequenceLength:Word):Boolean;

   function ReadRecord(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
   function WriteRecord(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;

   function ReadHeader(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
   function WriteHeader(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
 end;

  {Key}
 TNTFSDiskKey = class(TBtreeObject)  {Represents an INDX record entry}
   constructor Create(ALocalLock:TMutexHandle);
   destructor Destroy; override;
  private
   {Private Variables}
   FLocalLock:TMutexHandle;
   
   FStatus:LongWord;
   FKey:Pointer;

   {Index Variables} {TNTFSDataIndexEntry / TNTFSAttributeIndexEntry}
   FEntryFlags:Word;                         {Flags} {See Consts}
   FEntrySize:Word;                          {Length of the index entry}
   FKeySize:Word;                            {Length of the key entry}
   FSubNodeNumber:Int64;                     {Number of the sub-node in the index allocation attribute}

   {Object Variables}
   FNode:TNTFSDiskNode;                      {Index node of this key}

   {Private Methods}
   function AcquireLock:Boolean;
   function ReleaseLock:Boolean;

   function GetChanged:Boolean;
   procedure SetChanged(AValue:Boolean);

   function GetHasSubNode:Boolean;
   procedure SetHasSubNode(AValue:Boolean);
   function GetIsLastNode:Boolean;
   procedure SetIsLastNode(AValue:Boolean);

   procedure SetKeySize(ASize:Word); virtual;
   procedure SetSubNodeNumber(const ANodeNumber:Int64);

   procedure SetNode(ANode:TNTFSDiskNode);
  public
   {Public Properties}
   property Changed:Boolean read GetChanged write SetChanged;    {Index record to be updated}

   property HasSubNode:Boolean read GetHasSubNode write SetHasSubNode;
   property IsLastNode:Boolean read GetIsLastNode write SetIsLastNode;

   property Key:Pointer read FKey;

   {Index Properties}
   property EntryFlags:Word read FEntryFlags write FEntryFlags;
   property EntrySize:Word read FEntrySize write FEntrySize;
   property KeySize:Word read FKeySize write SetKeySize;
   property SubNodeNumber:Int64 read FSubNodeNumber write SetSubNodeNumber;

   {Object Properties}
   property Node:TNTFSDiskNode read FNode write SetNode;

   {Public Methods}
   function CalculatedSize(AVersion:Word):Word; virtual;

   function ReadKey(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean; virtual;
   function WriteKey(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean; virtual;
 end;

 TNTFSDataKey = class(TNTFSDiskKey)
   constructor Create(ALocalLock:TMutexHandle);
   destructor Destroy; override;
  private
   {Private Variables}
   FData:Pointer;

   {Index Variables} {TNTFSDataIndexEntry}
   FDataSize:Word;                           {Size of the data}  {Only valid when the last entry flag is not set}

   {Private Methods}
   procedure SetKeySize(ASize:Word); override;
   procedure SetData(AData:Pointer);
   procedure SetDataSize(ASize:Word);
  public
   {Public Properties}
   property Data:Pointer read FData write SetData;

   {Index Properties}
   property DataSize:Word read FDataSize write SetDataSize;

   {Public Methods}
   function DataOffset:Word;                 {Offset to the data}{Only valid when the last entry flag is not set}
   function CalculatedSize(AVersion:Word):Word; override;

   function ReadKey(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean; override;
   function WriteKey(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean; override;
 end;

 TNTFSPaddedKey = class(TNTFSDataKey)
   constructor Create(ALocalLock:TMutexHandle);
   destructor Destroy; override;
  private
   {Private Variables}
   FPadding:Pointer;
   FPaddingSize:Word;

   {Index Variables} {TNTFSDataIndexEntry}

   {Private Methods}
   procedure SetPadding(APadding:Pointer);
   procedure SetPaddingSize(ASize:Word);
  public
   {Public Properties}
   property Padding:Pointer read FPadding write SetPadding;
   property PaddingSize:Word read FPaddingSize write SetPaddingSize;

   {Index Properties}

   {Public Methods}
   function ReadKey(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean; override;
   function WriteKey(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean; override;
 end;

 TNTFSAttributeKey = class(TNTFSDiskKey)
   constructor Create(ALocalLock:TMutexHandle);
   destructor Destroy; override;
  private
   {Private Variables}

   {Index Variables} {TNTFSAttributeIndexEntry}
   FFileReference:Int64;                     {File reference} {Only valid when the last entry flag is not set}

   {Object Variables}
   FAttribute:TNTFSDiskAttribute;            {Indexed attribute of this key}

   {Private Methods}
   function GetInvalid:Boolean;
   procedure SetInvalid(AValue:Boolean);

   procedure SetKeySize(ASize:Word); override;
   procedure SetAttribute(AAttribute:TNTFSDiskAttribute);
  public
   {Public Properties}
   property Invalid:Boolean read GetInvalid write SetInvalid;    {Key points to invalid file record or attribute}

   {Index Properties}
   property FileReference:Int64 read FFileReference write FFileReference;

   {Object Properties}
   property Attribute:TNTFSDiskAttribute read FAttribute write SetAttribute;

   {Public Methods}
   function UpdateKey:Boolean;

   function RecordNumber:Int64;

   function CalculatedSize(AVersion:Word):Word; override;

   function ReadKey(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean; override;
   function WriteKey(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean; override;
 end;

  {Run}
 TNTFSDiskRun = class;
 TNTFSDiskRuns = class(TFileSysListEx) 
   constructor Create(ARunLocal:TMutexHandle;ALock:TSynchronizerHandle);
   destructor Destroy; override;
  private
   {Private Variables}
   FRunLocal:TMutexHandle;
   
   FRecent:TNTFSDiskRun;
   FRecentVCN:Int64;
   
   {Private Methods}
   function GetStart:TNTFSDiskRun;
   function GetFinal:TNTFSDiskRun;
  public
   {Public Properties}
   property Recent:TNTFSDiskRun read FRecent write FRecent;
   property RecentVCN:Int64 read FRecentVCN write FRecentVCN;

   {Run Methods}
   function SparseCount:Int64;               {Count of sparse runs in list}
   function ClusterCount:Int64;              {Count of normal runs in list}
   function AllocatedCount:Int64;            {Count of allocated runs in list}

   {Public Methods}
   function TotalSize:LongWord;              {Total size of the encoded data runs}

   function RunCount:LongWord;
 end;

 TNTFSDiskRun = class(TListObject)
   constructor Create(ALocalLock:TMutexHandle;AAttribute:TNTFSDiskAttribute); 
   destructor Destroy; override;
  private
   {Private Variables}
   FLocalLock:TMutexHandle;
   
   {Run Variables}
   FStart:Int64;                             {Run start logical cluster}
   FOffset:Int64;                            {Run start cluster offset}
   FLength:Int64;                            {Run length in clusters}

   {Object Variables}
   FAttribute:TNTFSDiskAttribute;            {Attribute owning this run}

   {Private Methods}
   function AcquireLock:Boolean;
   function ReleaseLock:Boolean;

   function GetIsLast:Boolean;
   function GetIsSparse:Boolean;

   function GetOffsetSize:Byte;
   function GetLengthSize:Byte;

   procedure SetStart(const AStart:Int64);
   procedure SetOffset(const AOffset:Int64);

   function GetSuccessor:TNTFSDiskRun;
   function GetPredecessor:TNTFSDiskRun;
  public
   {Public Properties}
   property IsLast:Boolean read GetIsLast;
   property IsSparse:Boolean read GetIsSparse;

   {Run Properties}
   property Start:Int64 read FStart write SetStart;
   property Offset:Int64 read FOffset write SetOffset;
   property Length:Int64 read FLength write FLength;

   {Object Properties}
   property Attribute:TNTFSDiskAttribute read FAttribute;

   {Public Methods}
   function RunSize:LongWord;                {Size of the encoded data run}

   function UpdateRun:Boolean;

   function ReadRun(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean;
   function WriteRun(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean;
 end;

  {Item}
 TNTFSDiskItems = class(TFileSysListEx)
   constructor Create(AItemLocal:TMutexHandle;ALock:TSynchronizerHandle);
   destructor Destroy; override;
  private
   {Private Variables}
   FItemLocal:TMutexHandle;
   
   FStatus:LongWord;

   {Private Methods}
   function GetLoaded:Boolean;
   procedure SetLoaded(AValue:Boolean);

   function GetPrevious(AItem:TNTFSDiskItem):TNTFSDiskItem;
  public
   {Public Properties}
   property Loaded:Boolean read GetLoaded write SetLoaded;

   {Public Methods}
   function TotalSize:Int64;

   function ItemCount:LongWord;
 end;

 TNTFSDiskItem = class(TListObject) {Data of attribute $ATTRIBUTE_LIST}
   constructor Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskAttribute);
   destructor Destroy; override;
  private
   {Private Variables}
   FLocalLock:TMutexHandle;
   
   FStatus:LongWord;

   {Item Variables}
   FItemSize:Word;                           {Record length}
   FStartVCN:Int64;                          {Starting VCN if non resident}
   FFileReference:Int64;                     {File Reference of the attribute}
   FAttributeType:LongWord;                  {Attribute Type}
   FAttributeId:Word;                        {Attribute Id}
   FAttributeName:String;                    {Name in Unicode (if NameLength > 0)} {Change to WideString}

   {Object Variables}
   FParent:TNTFSDiskAttribute;               {Attribute list owning this item}
   FAttribute:TNTFSDiskAttribute;            {Attribute pointed to by this item}

   FAttributeHash:LongWord;

   {Private Methods}
   function AcquireLock:Boolean;
   function ReleaseLock:Boolean;

   function GetInvalid:Boolean;
   procedure SetInvalid(AValue:Boolean);

   function GetAttributeName:String; {Change to WideString}
   procedure SetAttributeName(const AAttributeName:String); {Change to WideString}
   procedure SetAttribute(AAttribute:TNTFSDiskAttribute);

   function Compare(AItem:TNTFSDiskItem):Integer; virtual;
  public
   {Public Properties}
   property Invalid:Boolean read GetInvalid write SetInvalid;    {Item points to invalid file record or attribute}

   {Item Properties}
   property ItemSize:Word read FItemSize write FItemSize;
   property StartVCN:Int64 read FStartVCN write FStartVCN;
   property FileReference:Int64 read FFileReference write FFileReference;
   property AttributeType:LongWord read FAttributeType write FAttributeType;
   property AttributeId:Word read FAttributeId write FAttributeId;
   property AttributeName:String read GetAttributeName write SetAttributeName; {Change to WideString}

   {Object Properties}
   property Parent:TNTFSDiskAttribute read FParent;
   property Attribute:TNTFSDiskAttribute read FAttribute write SetAttribute;

   property AttributeHash:LongWord read FAttributeHash;

   {Public Methods}
   function RecordNumber:Int64;

   function AttributeNameSize:Word;
   function AttributeNameLength:Byte;        {Name length}
   function AttributeNameOffset:Byte;        {Offset to Name}

   function UpdateItem:Boolean;

   function CalculatedSize(AVersion:Word):Word;

   function ReadItem(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
   function WriteItem(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
 end;

  {Attribute}
 TNTFSDiskAttributes = class(TFileSysListEx)
   constructor Create(AAttributeLocal:TMutexHandle;ALock:TSynchronizerHandle);
   destructor Destroy; override;
  private
   {Private Variables}
   FAttributeLocal:TMutexHandle;
   
   {Private Methods}
   function GetPrevious(AAttribute:TNTFSDiskAttribute):TNTFSDiskAttribute;
  public
   {Public Properties}

   {Public Methods}
   function TotalSize:LongWord;

   function AttributeCount:LongWord;
 end;

 TNTFSDiskAttribute = class(TListObject)
   constructor Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
   destructor Destroy; override;
  private
   {Private Variables}
   FLocalLock:TMutexHandle;

   {Private Methods}
   
  protected
   {Protected Variables}
   FStatus:LongWord;
   FData:Pointer;                            {Only applicable if attribute is resident}

   {Attribute Variables}    {TNTFSAttributeHeader}
   FAttributeType:LongWord;                  {Attribute Type (e.g. 0x80, 0xA0)}
   FAttributeSize:LongWord;                  {Length of the Attribute (including header)}
   FAttributeFlags:Word;                     {Attribute Flags}
   FAttributeId:Word;                        {Attribute Id}
   FAttributeName:String;                    {Attribute Name} {Change to WideString}
   FNonResident:Byte;                        {Non-resident flag}

   {Resident Variables}     {TNTFSResidentAttributeHeader}
   FIndexed:Byte;                            {Indexed flag}
   FDataSize:LongWord;                       {Length of the Attribute Data}

   {Non Resident Variables} {TNTFSNonResidentAttributeHeader}
   FStartVCN:Int64;                          {Starting VCN}
   FLastVCN:Int64;                           {Last VCN}
   FStreamSize:Int64;                        {Real size of the attribute}
   FStreamUsed:Int64;                        {The actual Allocated size of the attribute (Only present when compressed and only in the first instance)}
   FStreamAllocated:Int64;                   {Allocated size of the attribute}
   FInitializedSize:Int64;                   {Initialized data size of the stream (Portion which has been Written)}
   FCompressionUnit:Word;                    {Compression Unit Size}

   {Object Variables}
   FRuns:TNTFSDiskRuns;                      {List of runs of this attribute}
   FItems:TNTFSDiskItems;                    {List of items of this attribute}
   FIndex:TNTFSDiskIndex;                    {Index of this attribute if applicable}
   FParent:TNTFSDiskRecord;                  {Record owning this attribute}

   FAttributeHash:LongWord;

   {Protected Methods}
   function AcquireLock:Boolean;
   function ReleaseLock:Boolean;
   
   function GetRunsLock:TSynchronizerHandle;
   function GetItemsLock:TSynchronizerHandle;
   function GetIndexLock:TSynchronizerHandle;
   function GetExtendedsLock:TSynchronizerHandle;
   
   function GetRunLocal:TMutexHandle;
   function GetItemLocal:TMutexHandle;
   function GetKeyLocal:TMutexHandle;
   function GetExtendedLocal:TMutexHandle;
   
   function GetUpdating:Boolean;
   procedure SetUpdating(AValue:Boolean);

   function GetIsFixed:Boolean;
   function GetIsSingle:Boolean;
   function GetIsUnlisted:Boolean;
   function GetIsUnmovable:Boolean;
   function GetIsManaged:Boolean;

   function GetIsSparse:Boolean;
   procedure SetIsSparse(AValue:Boolean);
   function GetIsEncrypted:Boolean;
   procedure SetIsEncrypted(AValue:Boolean);
   function GetIsCompressed:Boolean;
   procedure SetIsCompressed(AValue:Boolean);

   function GetAttributeName:String; {Change to WideString}
   procedure SetAttributeName(const AAttributeName:String); {Change to WideString}

   function GetDataSize:LongWord; virtual;
   procedure SetDataSize(AValue:LongWord); virtual;

   function Compare(AAttribute:TNTFSDiskAttribute):Integer; virtual;

   function CreateRuns(ANew:Boolean):TNTFSDiskRuns;
   function CreateItems(ANew:Boolean):TNTFSDiskItems;

   function GetRecord(AInstance:Integer):TNTFSDiskRecord;
   function GetAttribute(AType:LongWord;const AName:String;AInstance:Integer):TNTFSDiskAttribute; {Change to WideString}
  public
   {Public Properties}
   property Updating:Boolean read GetUpdating write SetUpdating;

   property IsFixed:Boolean read GetIsFixed;
   property IsSingle:Boolean read GetIsSingle;
   property IsUnlisted:Boolean read GetIsUnlisted;
   property IsUnmovable:Boolean read GetIsUnmovable;
   property IsManaged:Boolean read GetIsManaged;

   property IsSparse:Boolean read GetIsSparse write SetIsSparse;
   property IsEncrypted:Boolean read GetIsEncrypted write SetIsEncrypted;
   property IsCompressed:Boolean read GetIsCompressed write SetIsCompressed;

   property Data:Pointer read FData;

   {Attribute Properties}
   property AttributeType:LongWord read FAttributeType;
   property AttributeSize:LongWord read FAttributeSize write FAttributeSize;
   property AttributeFlags:Word read FAttributeFlags write FAttributeFlags;
   property AttributeId:Word read FAttributeId write FAttributeId;
   property AttributeName:String read GetAttributeName write SetAttributeName; {Change to WideString}
   property NonResident:Byte read FNonResident write FNonResident;

   {Resident Properties}
   property Indexed:Byte read FIndexed write FIndexed;
   property DataSize:LongWord read GetDataSize write SetDataSize;

   {Non Resident Properties}
   property StartVCN:Int64 read FStartVCN write FStartVCN;
   property LastVCN:Int64 read FLastVCN write FLastVCN;
   property StreamSize:Int64 read FStreamSize write FStreamSize;
   property StreamUsed:Int64 read FStreamUsed write FStreamUsed;
   property StreamAllocated:Int64 read FStreamAllocated write FStreamAllocated;
   property InitializedSize:Int64 read FInitializedSize write FInitializedSize;
   property CompressionUnit:Word read FCompressionUnit write FCompressionUnit;

   {Object Properties}
   property Runs:TNTFSDiskRuns read FRuns;
   property Items:TNTFSDiskItems read FItems;
   property Index:TNTFSDiskIndex read FIndex;
   property Parent:TNTFSDiskRecord read FParent write FParent;

   property AttributeHash:LongWord read FAttributeHash;

   {Run Methods}
   function GetRunCount(const AVCN:Int64;var AStartVCN,ACount:Int64):Boolean;
   function GetRunLength(const AVCN:Int64;var AStartVCN,ALength:Int64):Boolean;
   function GetRunByUnit(const AUnit:Int64;var AStartVCN:Int64):TNTFSDiskRun;
   function GetRunByCluster(const ACluster:Int64;var AStartVCN:Int64):TNTFSDiskRun;

   {Run Methods}
   function CreateRun(ANew:Boolean):TNTFSDiskRun;
   function DestroyRun(ARun:TNTFSDiskRun):Boolean;
   function NewRun(const AStart,ALength:Int64):TNTFSDiskRun;
   function InsertRun(APrev:TNTFSDiskRun;const AStart,ALength:Int64):TNTFSDiskRun;
   function GetRun(const AVCN:Int64;var AStartVCN:Int64):TNTFSDiskRun;
   function GetRunOld(const AVCN:Int64;var AStartVCN:Int64):TNTFSDiskRun; //To Do //Remove ?
   function MergeRun(ARun:TNTFSDiskRun):Boolean;
   function SplitRun(ARun:TNTFSDiskRun;const ALength:Int64):Boolean;
   function RemoveRun(ARun:TNTFSDiskRun):Boolean;
   function MoveRun(ADest:TNTFSDiskAttribute;ARun:TNTFSDiskRun):Boolean;

   {Item Methods}
   function GetItemByAttribute(AAttribute:TNTFSDiskAttribute):TNTFSDiskItem;

   {Item Methods}
   function CreateItem(ANew:Boolean):TNTFSDiskItem;
   function DestroyItem(AItem:TNTFSDiskItem):Boolean;
   function NewItem(AAttribute:TNTFSDiskAttribute):TNTFSDiskItem;
   function GetItem(AType:LongWord;const AName:String;AInstance:Integer):TNTFSDiskItem;
   function RemoveItem(AItem:TNTFSDiskItem):Boolean;
   function MoveItem(AItem:TNTFSDiskItem;AAttribute:TNTFSDiskAttribute):Boolean;
   function RenameItem(AItem:TNTFSDiskItem;AAttribute:TNTFSDiskAttribute):Boolean;

   {Index Methods}
   function CreateIndex(AVersion,ASector:Word):Boolean; virtual;
   function NewIndex(AVersion,ASector:Word;AType,ARule,ASize,AOffset:LongWord):Boolean; virtual;

   {Public Methods}
   function RecordNumber:Int64;
   function FileReference:Int64;
   function BaseReference:Int64;

   function RunOffset:Word;                  {Offset to the Data Run}
   function DataOffset:Word;                 {Offset to the Attribute Data}
   function AttributeNameSize:Word;
   function AttributeNameLength:Byte;        {Name length}
   function AttributeNameOffset:Word;        {Offset to the Name}

   function RunCount:LongWord;
   function ItemCount:LongWord;

   function CalculatedSize(AVersion:Word):LongWord; virtual;
   function CalculatedDataSize(AVersion:Word):LongWord; virtual;
   function CalculatedStreamSize(AVersion:Word):Int64; virtual;
   function CalculatedStreamUsed(AVersion:Word):Int64; virtual;

   function CoalesceRun(ARun:TNTFSDiskRun):Boolean;

   function UpdateRun(ARun:TNTFSDiskRun):Boolean;
   function UpdateKey(AKey:TNTFSDiskKey):Boolean; virtual;
   function UpdateItem(AItem:TNTFSDiskItem):Boolean; virtual;
   function UpdateEntry(AEntry:TNTFSDiskEntry):Boolean; virtual;
   function UpdateAttribute(AEntry:TNTFSDiskEntry):Boolean; virtual;

   function ReadRuns(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean;
   function WriteRuns(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean;

   function ReadItems(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
   function WriteItems(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;

   function ReadData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean; virtual;
   function WriteData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean; virtual;

   function ReadAttribute(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean; virtual;
   function WriteAttribute(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean; virtual;
 end;

 TNTFSStandardInformationAttribute = class(TNTFSDiskAttribute)
   constructor Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
   destructor Destroy; override;
  private
   {Private Variables}

   {Attribute Variables}    {TNTFSStandardInformation}
   FCreateTime:TFileTime;                    {File Creation}
   FWriteTime:TFileTime;                     {File Altered}
   FChangeTime:TFileTime;                    {MFT Changed}
   FAccessTime:TFileTime;                    {File Read}
   FAttributes:LongWord;                     {DOS File Permissions}
   FMaxVersions:LongWord;                    {Maximum Number of Versions}
   FVersionNo:LongWord;                      {Version Number}
   FClassId:LongWord;                        {Class Id}
   FOwnerId:LongWord;                        {Owner Id}
   FSecurityId:LongWord;                     {Security Id}
   FQuotaCharge:Int64;                       {Quota Charged}
   FUpdateSequenceNumber:Int64;              {Update Sequence Number}

   {Private Methods}
   
  public
   {Public Properties}

   {Attribute Properties}
   property CreateTime:TFileTime read FCreateTime write FCreateTime;
   property WriteTime:TFileTime read FWriteTime write FWriteTime;
   property ChangeTime:TFileTime read FChangeTime write FChangeTime;
   property AccessTime:TFileTime read FAccessTime write FAccessTime;
   property Attributes:LongWord read FAttributes write FAttributes;
   property MaxVersions:LongWord read FMaxVersions write FMaxVersions;
   property VersionNo:LongWord read FVersionNo write FVersionNo;
   property ClassId:LongWord read FClassId write FClassId;
   property OwnerId:LongWord read FOwnerId write FOwnerId;
   property SecurityId:LongWord read FSecurityId write FSecurityId;
   property QuotaCharge:Int64 read FQuotaCharge write FQuotaCharge;
   property UpdateSequenceNumber:Int64 read FUpdateSequenceNumber write FUpdateSequenceNumber;

   {Public Methods}
   function CalculatedStreamSize(AVersion:Word):Int64; override;

   function UpdateEntry(AEntry:TNTFSDiskEntry):Boolean; override;
   function UpdateAttribute(AEntry:TNTFSDiskEntry):Boolean; override;

   function ReadData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean; override;
   function WriteData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean; override;
 end;

 TNTFSAttributeListAttribute = class(TNTFSDiskAttribute)
   constructor Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
   destructor Destroy; override;
  private
   {Private Variables}

   {Attribute Variables}    {TNTFSAttributeList}

   {Private Methods}
   
  public
   {Public Properties}

   {Attribute Properties}

   {Public Methods}
   function CalculatedStreamSize(AVersion:Word):Int64; override;

   function ReadData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean; override;
   function WriteData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean; override;
 end;

 TNTFSFileNameAttribute = class(TNTFSDiskAttribute)
   constructor Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
   destructor Destroy; override;
  private
   {Private Variables}

   {Attribute Variables}    {TNTFSFileName}
   FParentReference:Int64;                   {File reference to the parent directory.}
   FCreateTime:TFileTime;                    {C Time - File Creation}                     {Note: These fields are only updated when   }
   FWriteTime:TFileTime;                     {A Time - File Altered}                      {      the filename is changed. See Standard}
   FChangeTime:TFileTime;                    {M Time - MFT Changed}                       {      Information instead                  }
   FAccessTime:TFileTime;                    {R Time - File Read}                         {                                           }
   FFileAllocated:Int64;                     {Allocated size of the file}                 {As for CreateTime/WriteTime/ChangeTime etc }
   FFileSize:Int64;                          {Real size of the file}                      {As for CreateTime/WriteTime/ChangeTime etc }
   FFileFlags:LongWord;                      {Flags, e.g. Directory, compressed, hidden}  {As for CreateTime/WriteTime/ChangeTime etc }
   FReparseTag:LongWord;                     {Used by EAs and Reparse}
   FNameSpace:Byte;                          {Filename namespace}
   FFileName:String;                         {File name} {Change to WideString}

   FFileHash:LongWord;

   {Private Methods}
   function GetFileName:String; {Change to WideString}
   procedure SetFileName(const AFileName:String); {Change to WideString}
  public
   {Public Properties}

   {Attribute Properties}
   property ParentReference:Int64 read FParentReference write FParentReference;
   property CreateTime:TFileTime read FCreateTime write FCreateTime;
   property WriteTime:TFileTime read FWriteTime write FWriteTime;
   property ChangeTime:TFileTime read FChangeTime write FChangeTime;
   property AccessTime:TFileTime read FAccessTime write FAccessTime;
   property FileAllocated:Int64 read FFileAllocated write FFileAllocated;
   property FileSize:Int64 read FFileSize write FFileSize;
   property FileFlags:LongWord read FFileFlags write FFileFlags;
   property ReparseTag:LongWord read FReparseTag write FReparseTag;
   property NameSpace:Byte read FNameSpace write FNameSpace;
   property FileName:String read GetFileName write SetFileName; {Change to WideString}

   property FileHash:LongWord read FFileHash;

   {Public Methods}
   function FileNameSize:Word;
   function FileNameLength:Byte;             {Filename length in characters}

   function ParentRecord:Int64;

   function CalculatedStreamSize(AVersion:Word):Int64; override;

   function UpdateEntry(AEntry:TNTFSDiskEntry):Boolean; override;
   function UpdateAttribute(AEntry:TNTFSDiskEntry):Boolean; override;

   function ReadData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean; override;
   function WriteData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean; override;
 end;

 TNTFSObjectIdAttribute = class(TNTFSDiskAttribute)
   constructor Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
   destructor Destroy; override;
  private
   {Private Variables}

   {Attribute Variables}    {TNTFSObjectId}
   FObjectId:TGUID;                          {GUID Object Id Unique Id assigned to file}
   FBirthVolumeId:TGUID;                     {GUID Birth Volume Id Volume where file was created}
   FBirthObjectId:TGUID;                     {GUID Birth Object Id Original Object Id of file}
   FDomainId:TGUID;                          {GUID Domain Id Domain in which object was created}

   {Private Methods}
   
  public
   {Public Properties}

   {Attribute Properties}
   property ObjectId:TGUID read FObjectId write FObjectId;
   property BirthVolumeId:TGUID read FBirthVolumeId write FBirthVolumeId;
   property BirthObjectId:TGUID read FBirthObjectId write FBirthObjectId;
   property DomainId:TGUID read FDomainId write FDomainId;

   {Public Methods}
   function CalculatedStreamSize(AVersion:Word):Int64; override;

   function ReadData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean; override;
   function WriteData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean; override;
 end;

 TNTFSVolumeVersionAttribute = class(TNTFSDiskAttribute)
   constructor Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
   destructor Destroy; override;
  private
   {Private Variables}

   {Attribute Variables}    {TNTFSVolumeVersion}

   {Private Methods}
   
  protected
   {Protected Variables}

   {Protected Methods}
   procedure SetDataSize(AValue:LongWord); override;
  public
   {Public Properties}

   {Attribute Properties}

   {Public Methods}
 end;

 TNTFSSecurity = class;
 TNTFSSecurityDescriptorAttribute = class(TNTFSDiskAttribute)
   {Note: This is the same structure that is used for all security in Windows (File/Kernel/Registry etc)}
   {      See TSecurityDescriptor and associated values in Security unit for more information            }
   constructor Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
   destructor Destroy; override;
  private
   {Private Variables}

   {Attribute Variables}    {TNTFSSecurityDescriptor}

   {Object Variables}
   FSecurity:TNTFSSecurity;

   {Private Methods}
   //To Do //SetSecurity ? for use when Updating ?
  public
   {Public Properties}

   {Attribute Properties}

   {Object Properties}
   property Security:TNTFSSecurity read FSecurity;

   {Security Methods}
   function CreateSecurity:Boolean;
   function NewSecurity(ASecurity:TNTFSSecurity):Boolean;

   {Public Methods}
   function CalculatedStreamSize(AVersion:Word):Int64; override;

   function ReadSecurity(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
   function WriteSecurity(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;

   function ReadData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean; override;
   function WriteData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean; override;
 end;

 TNTFSVolumeNameAttribute = class(TNTFSDiskAttribute)
   constructor Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
   destructor Destroy; override;
  private
   {Private Variables}

   {Attribute Variables}    {TNTFSVolumeName}
   FVolumeName:String;                       {Volume name} {Change to WideString}

   {Private Methods}
   function GetVolumeName:String; {Change to WideString}
   procedure SetVolumeName(const AVolumeName:String); {Change to WideString}
  public
   {Public Properties}

   {Attribute Properties}
   property VolumeName:String read GetVolumeName write SetVolumeName; {Change to WideString}

   {Public Methods}
   function CalculatedStreamSize(AVersion:Word):Int64; override;

   function ReadData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean; override;
   function WriteData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean; override;
 end;

 TNTFSVolumeInformationAttribute = class(TNTFSDiskAttribute)
   constructor Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
   destructor Destroy; override;
  private
   {Private Variables}

   {Attribute Variables}    {TNTFSVolumeInformation}
   FMajorVersion:Byte;                       {Major version number}
   FMinorVersion:Byte;                       {Minor version number}
   FVolumeFlags:Word;                        {Flags}

   {Private Methods}
   
  public
   {Public Properties}

   {Attribute Properties}
   property MajorVersion:Byte read FMajorVersion write FMajorVersion;
   property MinorVersion:Byte read FMinorVersion write FMinorVersion;
   property VolumeFlags:Word read FVolumeFlags write FVolumeFlags;

   {Public Methods}
   function CalculatedStreamSize(AVersion:Word):Int64; override;

   function ReadData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean; override;
   function WriteData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean; override;
 end;

 TNTFSDataAttribute = class(TNTFSDiskAttribute)
   constructor Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
   destructor Destroy; override;
  private
   {Private Variables}

   {Attribute Variables}    {TNTFSData}

   {Private Methods}
   
  protected
   {Protected Variables}

   {Protected Methods}
   procedure SetDataSize(AValue:LongWord); override;
  public
   {Public Properties}

   {Attribute Properties}

   {Public Methods}
   function UpdateEntry(AEntry:TNTFSDiskEntry):Boolean; override;
   function UpdateAttribute(AEntry:TNTFSDiskEntry):Boolean; override;
 end;

 TNTFSIndexRootAttribute = class(TNTFSDiskAttribute)
   constructor Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
   destructor Destroy; override;
  private
   {Private Variables}

   {Attribute Variables}    {TNTFSIndexRoot}
   FIndexType:LongWord;                      {Attribute Type}
   FCollateRule:LongWord;                    {Collation Rule}
   FIndexRecordSize:LongWord;                {Size of Index Allocation Entry (bytes)}
   FIndexCounterOffset:LongWord;             {Index Record Number increment}

   {Private Methods}
   
  public
   {Public Properties}

   {Attribute Properties}
   property IndexType:LongWord read FIndexType write FIndexType;
   property CollateRule:LongWord read FCollateRule write FCollateRule;
   property IndexRecordSize:LongWord read FIndexRecordSize write FIndexRecordSize;
   property IndexCounterOffset:LongWord read FIndexCounterOffset write FIndexCounterOffset;

   {Index Methods}
   function CreateIndex(AVersion,ASector:Word):Boolean; override;
   function NewIndex(AVersion,ASector:Word;AType,ARule,ASize,AOffset:LongWord):Boolean; override;

   {Public Methods}
   function CalculatedStreamSize(AVersion:Word):Int64; override;

   function ReadData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean; override;
   function WriteData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean; override;
 end;

 TNTFSIndexAllocationAttribute = class(TNTFSDiskAttribute)
   constructor Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
   destructor Destroy; override;
  private
   {Private Variables}

   {Attribute Variables}    {TNTFSIndexAllocation}

   {Private Methods}
   
  protected
   {Protected Variables}

   {Protected Methods}
   procedure SetDataSize(AValue:LongWord); override;
  public
   {Public Properties}

   {Attribute Properties}

   {Public Methods}
 end;

 TNTFSBitmapAttribute = class(TNTFSDiskAttribute)
   constructor Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
   destructor Destroy; override;
  private
   {Private Variables}
   FBitmap:Pointer;
   FBitmapSize:LongWord;

   {Attribute Variables}    {TNTFSBitmap}

   {Private Methods}
   
  protected
   {Protected Variables}

   {Protected Methods}
   procedure SetDataSize(AValue:LongWord); override;
   procedure SetBitmapSize(AValue:LongWord);
  public
   {Public Properties}
   property Bitmap:Pointer read FBitmap;
   property BitmapSize:LongWord read FBitmapSize write SetBitmapSize;

   {Attribute Properties}

   {Public Methods}
   function CalculatedStreamSize(AVersion:Word):Int64; override;
 end;

 TNTFSReparse = class;
 TNTFSReparsePointAttribute = class(TNTFSDiskAttribute)
   constructor Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
   destructor Destroy; override;
  private
   {Private Variables}

   {Attribute Variables}    {TNTFSReparsePoint}
   FReparseTag:LongWord;                     {Reparse Type (and Flags)}
   FReparseSize:Word;                        {Reparse Data Length}
   FReparseGUID:TGUID;                       {Reparse GUID}

   {Object Variables}
   FReparse:TNTFSReparse;

   {Private Methods}
   function GetIsAlias:Boolean;
   procedure SetIsAlias(AValue:Boolean);
   function GetIsHighLatency:Boolean;
   procedure SetIsHighLatency(AValue:Boolean);
   function GetIsMicrosoft:Boolean;
   procedure SetIsMicrosoft(AValue:Boolean);
  public
   {Public Properties}
   property IsAlias:Boolean read GetIsAlias write SetIsAlias;
   property IsHighLatency:Boolean read GetIsHighLatency write SetIsHighLatency;
   property IsMicrosoft:Boolean read GetIsMicrosoft write SetIsMicrosoft;

   {Attribute Properties}
   property ReparseTag:LongWord read FReparseTag write FReparseTag;
   property ReparseSize:Word read FReparseSize write FReparseSize;
   property ReparseGUID:TGUID read FReparseGUID write FReparseGUID;

   {Object Properties}
   property Reparse:TNTFSReparse read FReparse;

   {Reparse Methods}
   function CreateReparse:Boolean;
   function NewReparse(AReparseTag:LongWord):Boolean;

   {Public Methods}
   function CalculatedStreamSize(AVersion:Word):Int64; override;

   function ReadReparse(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
   function WriteReparse(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;

   function ReadData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean; override;
   function WriteData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean; override;
 end;

 TNTFSSymbolicLinkAttribute = class(TNTFSDiskAttribute)
   constructor Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
   destructor Destroy; override;
  private
   {Private Variables}

   {Attribute Variables}    {TNTFSSymbolicLink}

   {Private Methods}
   
  protected
   {Protected Variables}

   {Protected Methods}
   procedure SetDataSize(AValue:LongWord); override;
  public
   {Public Properties}

   {Attribute Properties}

   {Public Methods}
 end;

 TNTFSExtendedAttrInformationAttribute = class(TNTFSDiskAttribute)
   constructor Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
   destructor Destroy; override;
  private
   {Private Variables}

   {Attribute Variables}    {TNTFSExtendedAttrInformation}
   FPackedSize:Word;                         {Size of the packed Extended Attributes}
   FFlagCount:Word;                          {Number of Extended Attributes with NEED_EA}
   FUnpackedSize:LongWord;                   {Size of the unpacked Extended Attributes}

   {Private Methods}
   
  public
   {Public Properties}

   {Attribute Properties}
   property PackedSize:Word read FPackedSize write FPackedSize;
   property FlagCount:Word read FFlagCount write FFlagCount;
   property UnpackedSize:LongWord read FUnpackedSize write FUnpackedSize;

   {Public Methods}
   function CalculatedStreamSize(AVersion:Word):Int64; override;

   function ReadData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean; override;
   function WriteData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean; override;
 end;

 TNTFSExtended = class;
 TNTFSExtendeds = class;
 TNTFSExtendedAttrAttribute = class(TNTFSDiskAttribute)
   constructor Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
   destructor Destroy; override;
  private
   {Private Variables}

   {Attribute Variables}    {TNTFSExtendedAttr}

   {Object Variables}
   FExtendeds:TNTFSExtendeds;              {List of attributes of this attribute}

   {Private Methods}
   
  public
   {Public Properties}

   {Attribute Properties}

   {Object Properties}
   property Extendeds:TNTFSExtendeds read FExtendeds;

   {Extended Methods}
   function CreateExtended:TNTFSExtended;
   function DestroyExtended(AExtended:TNTFSExtended):Boolean;
   function NewExtended(const AName:String):TNTFSExtended;
   function GetExtended(const AName:String):TNTFSExtended;
   function RemoveExtended(AExtended:TNTFSExtended):Boolean;

   {Public Methods}
   function ExtendedCount:LongWord;

   function CalculatedStreamSize(AVersion:Word):Int64; override;

   function ReadExtendeds(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
   function WriteExtendeds(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;

   function ReadData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean; override;
   function WriteData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean; override;
 end;

 TNTFSPropertySetAttribute = class(TNTFSDiskAttribute)
   constructor Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
   destructor Destroy; override;
  private
   {Private Variables}

   {Attribute Variables}    {TNTFSPropertySet}

   {Private Methods}
   
  protected
   {Protected Variables}

   {Protected Methods}
   procedure SetDataSize(AValue:LongWord); override;
  public
   {Public Properties}

   {Attribute Properties}

   {Public Methods}
 end;

 TNTFSLoggedUtilityStreamAttribute = class(TNTFSDiskAttribute)
   constructor Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
   destructor Destroy; override;
  private
   {Private Variables}

   {Attribute Variables}    {TNTFSLoggedUtilityStream}

   {Private Methods}
   
  protected
   {Protected Variables}

   {Protected Methods}
   procedure SetDataSize(AValue:LongWord); override;
  public
   {Public Properties}

   {Attribute Properties}

   {Public Methods}
 end;

 TNTFSEndAttribute = class(TNTFSDiskAttribute)
   constructor Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
   destructor Destroy; override;
  private
   {Private Variables}

   {Attribute Variables}    {TNTFSEnd}

   {Private Methods}
   
  public
   {Public Properties}

   {Attribute Properties}

   {Public Methods}
   function CalculatedSize(AVersion:Word):LongWord; override;
   function CalculatedDataSize(AVersion:Word):LongWord; override;
   function CalculatedStreamSize(AVersion:Word):Int64; override;

   function ReadAttribute(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean; override;
   function WriteAttribute(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean; override;
 end;

 TNTFSUnknownAttribute = class(TNTFSDiskAttribute)
   constructor Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
   destructor Destroy; override;
  private
   {Private Variables}

   {Attribute Variables}    {TNTFSUnknown}

   {Private Methods}
   
  protected
   {Protected Variables}

   {Protected Methods}
   procedure SetDataSize(AValue:LongWord); override;
  public
   {Public Properties}

   {Attribute Properties}

   {Public Methods}
 end;

  {Data}
 TNTFSObjId = class(TObject)       {Data of index $O in file $ObjId} {Not Used}
  private
   {Private Variables}
  public
   {Public Properties}
 end;

 TNTFSQuota = class(TObject)       {Data of index $Q in file $Quota} {Not Used}
  private
   {Private Variables}
  public
   {Public Properties}
 end;

 TNTFSOwner = class(TObject)       {Data of index $O in file $Quota} {Not Used}
  private
   {Private Variables}
  public
   {Public Properties}
 end;

 TNTFSUpCase = class(TObject)      {Data of file $UpCase}
   constructor Create;
   destructor Destroy; override;
  private
   {Private Variables}
   FLock:TSynchronizerHandle; 
   
   FData:Pointer;

   {Private Methods}
   function ReaderLock:Boolean;
   function ReaderUnlock:Boolean;
   function WriterLock:Boolean;
   function WriterUnlock:Boolean;
  public
   {Public Properties}
   property Data:Pointer read FData;

   {Public Methods}
   function Init(AVersion:Word):Boolean;

   function Convert(ASource,ADest:Pointer;ASize:LongWord):Boolean;

   function ReadUpCase(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
   function WriteUpCase(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
 end;

  {AttrDef}
 TNTFSAttrDef = class;
 TNTFSAttrDefs = class(TFileSysList)
   constructor Create;
   destructor Destroy; override;
  private
   {Private Variables}
   FAttrDefLocal:TMutexHandle;
   
   {Private Methods}
   function GetPrevious(AAttrDef:TNTFSAttrDef):TNTFSAttrDef;
  public
   {Public Properties}

   {AttrDef Methods}
   function CreateAttrDef(AType:LongWord;AVersion:Word;ANew:Boolean):TNTFSAttrDef;
   function DestroyAttrDef(AAttrDef:TNTFSAttrDef):Boolean;
   function NewAttrDef(AType:LongWord;const AName:String;AVersion:Word):TNTFSAttrDef;
   function GetAttrDef(AType:LongWord;const AName:String):TNTFSAttrDef;
   function GetAttrDefByIndex(AIndex:Integer;AVersion:Word):TNTFSAttrDef;
   function GetAttrDefByIndexEx(AIndex:Integer;AVersion:Word;AWrite:Boolean):TNTFSAttrDef;
   function GetAttrDefByAttribute(AAttribute:TNTFSDiskAttribute):TNTFSAttrDef;
   function RemoveAttrDef(AAttrDef:TNTFSAttrDef):Boolean;

   function CheckSize(AAttribute:TNTFSDiskAttribute;const ASize:Int64):Boolean;
   function CheckIndexed(AAttribute:TNTFSDiskAttribute):Boolean;
   function CheckResident(AAttribute:TNTFSDiskAttribute):Boolean;
   function CheckUncompressed(AAttribute:TNTFSDiskAttribute):Boolean;

   {Public Methods}
   function TotalSize:Int64;

   function AttrDefCount:LongWord;

   function Init(AVersion:Word):Boolean;

   function ReadAttrDefs(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
   function WriteAttrDefs(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
 end;

 TNTFSAttrDef = class(TListObject) {Data of file $AttrDef}
   constructor Create(ALocalLock:TMutexHandle);
   destructor Destroy; override;
  private
   {Private Variables}
   FLocalLock:TMutexHandle;
   
   FAttributeName:String;                {Label in Unicode} {Change to WideString}
   FAttributeType:LongWord;              {Type}
   FDisplayRule:LongWord;                {Display rule}
   FCollateRule:LongWord;                {Collation rule}
   FAttrDefFlags:LongWord;               {Flags} {See Consts}
   FMinimumSize:Int64;                   {Minimum size}
   FMaximumSize:Int64;                   {Maximum size}

   FAttributeHash:LongWord;

   {Private Methods}
   function AcquireLock:Boolean;
   function ReleaseLock:Boolean;

   function GetAttributeName:String; {Change to WideString}
   procedure SetAttributeName(const AAttributeName:String); {Change to WideString}

   function GetIsIndexed:Boolean;
   function GetIsResident:Boolean;
   function GetIsUncompressed:Boolean;

   function Compare(AAttrDef:TNTFSAttrDef):Integer; virtual;
  public
   {Public Properties}
   property AttributeName:String read GetAttributeName write SetAttributeName;
   property AttributeType:LongWord read FAttributeType write FAttributeType;
   property DisplayRule:LongWord read FDisplayRule write FDisplayRule;
   property CollateRule:LongWord read FCollateRule write FCollateRule;
   property AttrDefFlags:LongWord read FAttrDefFlags write FAttrDefFlags;
   property MinimumSize:Int64 read FMinimumSize write FMinimumSize;
   property MaximumSize:Int64 read FMaximumSize write FMaximumSize;

   property IsIndexed:Boolean read GetIsIndexed;
   property IsResident:Boolean read GetIsResident;
   property IsUncompressed:Boolean read GetIsUncompressed;

   {Public Methods}
   function Init(AIndex:Integer;AVersion:Word):Boolean;

   property AttributeHash:LongWord read FAttributeHash;

   function ReadAttrDef(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
   function WriteAttrDef(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
 end;

 TNTFSReparse = class(TDiskReparse)     {Data of attribute $REPARSE_POINT}
   constructor Create(ALocalLock:TMutexHandle;AAttribute:TNTFSDiskAttribute);
   destructor Destroy; override;
  private //To Do //Protected
   {Private Variables}
   FData:Pointer;

   FDataSize:Word;

   {Object Variables}
   FAttribute:TNTFSDiskAttribute;  {Attribute owning this index}

   {Private Methods}
   procedure SetData(AData:Pointer); virtual;

   function GetDataSize:Word; virtual;
   procedure SetDataSize(ASize:Word); virtual;
  public
   {Public Properties}
   property Data:Pointer read FData write SetData;

   property DataSize:Word read GetDataSize write SetDataSize;

   {Object Properties}
   property Attribute:TNTFSDiskAttribute read FAttribute;

   {Public Methods}
   function CalculatedSize(AVersion:Word):Word; virtual;

   function ReadReparse(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean; virtual;
   function WriteReparse(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean; virtual;
 end;

 TNTFSReparseSymLink = class(TNTFSReparse)     {Data of attribute $REPARSE_POINT (Tag 0xA000000C)}
   constructor Create(ALocalLock:TMutexHandle;AAttribute:TNTFSDiskAttribute);
   destructor Destroy; override;
  private //To Do //Protected
   {Private Variables}

   {Private Methods}
   procedure SetData(AData:Pointer); override;

   procedure SetDataSize(ASize:Word); override;
  protected
   {Protected Variables}
   FPrintName:String;                   {Path Buffer}
   FSubstituteName:String;              {Path Buffer}

   FPrintHash:LongWord;
   FSubstituteHash:LongWord;

   {Protected Methods}
   function GetTarget:String; override;

   function GetPrintName:String;
   procedure SetPrintName(const APrintName:String);
   function GetSubstituteName:String;
   procedure SetSubstituteName(const ASubstituteName:String);
  public
   {Public Properties}
   property PrintName:String read GetPrintName write SetPrintName;
   property SubstituteName:String read GetSubstituteName write SetSubstituteName;

   property PrintHash:LongWord read FPrintHash;
   property SubstituteHash:LongWord read FSubstituteHash;

   {Public Methods}
   function PrintNameSize:Word; virtual;
   function PrintNameOffset:Word; virtual;       {Print Name Offset}
   function PrintNameLength:Word; virtual;       {Print Name Length}
   function SubstituteNameSize:Word; virtual;
   function SubstituteNameOffset:Word; virtual;  {Substitute Name Offset}
   function SubstituteNameLength:Word; virtual;  {Substitute Name Length}

   function CalculatedSize(AVersion:Word):Word; override;

   function ReadReparse(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean; override;
   function WriteReparse(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean; override;
 end;

 TNTFSReparseMountPoint = class(TNTFSReparseSymLink) {Data of attribute $REPARSE_POINT (Tag 0xA0000003)}
   constructor Create(ALocalLock:TMutexHandle;AAttribute:TNTFSDiskAttribute);
   destructor Destroy; override;
  private
   {Private Variables}

   {Private Methods}
  protected
   {Protected Variables}

   {Protected Methods}
  public
   {Public Properties}

   {Public Methods}
   function PrintNameSize:Word; override;
   function PrintNameOffset:Word; override;       {Print Name Offset}
   function PrintNameLength:Word; override;       {Print Name Length}
   function SubstituteNameSize:Word; override;
   function SubstituteNameOffset:Word; override;  {Substitute Name Offset}
   function SubstituteNameLength:Word; override;  {Substitute Name Length}

   function CalculatedSize(AVersion:Word):Word; override;

   function ReadReparse(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean; override;
   function WriteReparse(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean; override;
 end;

 TNTFSExtendeds = class(TFileSysListEx) 
   constructor Create(AExtendedLocal:TMutexHandle;ALock:TSynchronizerHandle);
   destructor Destroy; override;
  private
   {Private Variables}
   FExtendedLocal:TMutexHandle;
   
   {Private Methods}

  public
   {Public Properties}

   {Public Methods}
   function TotalSize:Int64;

   function ExtendedCount:LongWord;
 end;

 TNTFSExtended = class(TListObject) {Data of attribute $EA}
   constructor Create(ALocalLock:TMutexHandle);
   destructor Destroy; override;
  private
   {Private Variables}
   FLocalLock:TMutexHandle;
   
   FExtendedOffset:LongWord;                 {Offset to next Extended Attribute} {Offset to next EA is the size of this EA}
   FExtendedFlags:Byte;                      {Flags}
   FExtendedName:String;                     {Name} {Note: No offset so always in the same location} {Not Unicode}
   FExtendedData:Pointer;                    {Value}
   FExtendedDataSize:Word;                   {Value Length}

   FExtendedHash:LongWord;

   {Private Methods}
   function AcquireLock:Boolean;
   function ReleaseLock:Boolean;

   function GetExtendedName:String;
   procedure SetExtendedName(const AExtendedName:String);
   procedure SetExtendedDataSize(ASize:Word);
  public
   {Public Properties}
   property ExtendedOffset:LongWord read FExtendedOffset write FExtendedOffset;
   property ExtendedFlags:Byte read FExtendedFlags write FExtendedFlags;
   property ExtendedName:String read GetExtendedName write SetExtendedName;
   property ExtendedData:Pointer read FExtendedData;
   property ExtendedDataSize:Word read FExtendedDataSize write SetExtendedDataSize;

   property ExtendedHash:LongWord read FExtendedHash;

   {Public Methods}
   function ExtendedSize:LongWord;

   function ExtendedNameSize:Word;
   function ExtendedNameLength:Byte;       {Name Length}

   function ReadExtended(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
   function WriteExtended(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
 end;

 TNTFSSecurityItem = class;
 TNTFSSecurityItems = class(TFileSysList) 
   constructor Create;
   destructor Destroy; override;
  private
   {Private Variables}
   FSecurityLocal:TMutexHandle;

   {Private Methods}
   function GetPrevious(ASecurityItem:TNTFSSecurityItem):TNTFSSecurityItem;
  public
   {Public Properties}

   {SecurityItem Methods}
   function CreateSecurityItem(ANew:Boolean):TNTFSSecurityItem;
   function DestroySecurityItem(ASecurityItem:TNTFSSecurityItem):Boolean;
   function NewSecurityItem(ASecurityId:LongWord;const ASecurityOffset:Int64;ASecurity:TNTFSSecurity):TNTFSSecurityItem;
   function GetSecurityItem(ASecurityId:LongWord):TNTFSSecurityItem;
   function GetSecurityItemEx(ASecurityId:LongWord;AWrite:Boolean):TNTFSSecurityItem;
   function UpdateSecurityItem(ASecurityItem:TNTFSSecurityItem;ASecurityId:LongWord;ASecurity:TNTFSSecurity):Boolean;
   function DeleteSecurityItem(ASecurityItem:TNTFSSecurityItem):Boolean;
   function RemoveSecurityItem(ASecurityItem:TNTFSSecurityItem;AFree:Boolean):Boolean;

   {Public Methods}
   function TotalSize:Int64;

   function SecurityItemCount:LongWord;

   function Init(AVersion:Word):Boolean;

   function ReadSecurityItems(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
   function WriteSecurityItems(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
 end;

 TNTFSSecurityItem = class(TListObject) {Data of stream $SDS in file $Secure}
   constructor Create(ALocalLock:TMutexHandle);
   destructor Destroy; override;
  private
   {Private Variables}
   FLocalLock:TMutexHandle;
   
   FSecurityHash:LongWord;              {Hash of Security Descriptor}
   FSecurityId:LongWord;                {Security Id}
   FSecurityOffset:Int64;               {Offset of this entry in $SDS}
   FSecuritySize:LongWord;              {Size of this entry in $SDS}

   {Object Variables}
   FSecurity:TNTFSSecurity;             {Self-relative Security Descriptor}

   {Private Methods}
   function AcquireLock:Boolean;
   function ReleaseLock:Boolean;

   function Compare(ASecurityItem:TNTFSSecurityItem):Integer; virtual;
   function CompareOld(ASecurityItem:TNTFSSecurityItem):Integer; virtual; //To Do //Remove ?
  public
   {Public Properties}
   property SecurityHash:LongWord read FSecurityHash write FSecurityHash;
   property SecurityId:LongWord read FSecurityId write FSecurityId;
   property SecurityOffset:Int64 read FSecurityOffset write FSecurityOffset;
   property SecuritySize:LongWord read FSecuritySize write FSecuritySize;

   {Object Properties}
   property Security:TNTFSSecurity read FSecurity;

   {Security Methods}
   function CreateSecurity:Boolean;
   function NewSecurity(ASecurity:TNTFSSecurity):Boolean;
   function UpdateSecurity(ASecurity:TNTFSSecurity):Boolean;
   function DeleteSecurity:Boolean;
   function RemoveSecurity(AFree:Boolean):Boolean;
   
   function MirrorOffset:Int64;

   function Init(ASecurityId:LongWord;AVersion:Word):Boolean;

   {Public Methods}
   function ReadSecurityItem(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
   function WriteSecurityItem(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
 end;

 TNTFSSecurity = class(TDiskSecurity) {Data of attribute $SECURITY_DESCRIPTOR and stream $SDS in file $Secure}
   {Note: This is the same structure that is used for all security in Windows (File/Kernel/Registry etc)}
   {      See TSecurityDescriptor and associated values in Security unit for more information            }
   constructor Create(ALocalLock:TMutexHandle);
   constructor CreateFromSecurity(ALocalLock:TMutexHandle;ASecurity:TDiskSecurity); override;
   constructor CreateFromDescriptor(ALocalLock:TMutexHandle;ADescriptor:Pointer); override;
   destructor Destroy; override;
  private
   {Private Variables}
   FVolumeVersion:Word;  //To Do //Passed to CreateSecurity/NewSecurity ? //For use in Descriptor handling ?

   FRevision:Byte;                           {Revision}
   FControl:Word;                            {Control Flags}
   FSacl:PACL;                               {SACL}
   FDacl:PACL;                               {DACL}
   FOwner:PSID;                              {Owner SID}
   FGroup:PSID;                              {Group SID}

   {Private Methods}
   procedure SetSacl(ASacl:PACL);
   procedure SetDacl(ADacl:PACL);
   procedure SetOwner(AOwner:PSID);
   procedure SetGroup(AGroup:PSID);
   procedure SetSaclSize(ASize:Word);
   procedure SetDaclSize(ASize:Word);
   procedure SetOwnerSize(ASize:Word);
   procedure SetGroupSize(ASize:Word);
  public
   {Public Properties}
   property VolumeVersion:Word read FVolumeVersion;

   property Revision:Byte read FRevision write FRevision;
   property Control:Word read FControl write FControl;
   property Sacl:PACL read FSacl write SetSacl;
   property Dacl:PACL read FDacl write SetDacl;
   property Owner:PSID read FOwner write SetOwner;
   property Group:PSID read FGroup write SetGroup;

   {Public Methods}
   function SaclSize:Word;
   function DaclSize:Word;
   function OwnerSize:Word;
   function GroupSize:Word;
   function SaclOffset:LongWord;             {Offset to SACL}
   function DaclOffset:LongWord;             {Offset to DACL}
   function OwnerOffset:LongWord;            {Offset to Owner SID}
   function GroupOffset:LongWord;            {Offset to Group SID}

   function SaclOffsetEx(ALocal:Boolean):LongWord;  {Offset to SACL}
   function DaclOffsetEx(ALocal:Boolean):LongWord;  {Offset to DACL}
   function OwnerOffsetEx(ALocal:Boolean):LongWord; {Offset to Owner SID}
   function GroupOffsetEx(ALocal:Boolean):LongWord; {Offset to Group SID}

   function SecuritySize:LongWord; override;
   function SecurityHash:LongWord;

   function SecurityDescriptor:Pointer; override;
   function SecurityDescriptorEx(ALocal:Boolean):Pointer; override;
   function InheritedDescriptor:Pointer; override;
   function MergedDescriptor(AChild:Pointer):Pointer; override;
   function ReleaseDescriptor(ADescriptor:Pointer;AInherited,AMerged:Boolean):Boolean; override;

   function CopyToSecurity(ASecurity:TDiskSecurity):Boolean; override;

   function CopyToDescriptor(ADescriptor:Pointer;ASize:LongWord):Boolean; override;
   function CopyToDescriptorEx(ADescriptor:Pointer;ASize:LongWord;ALocal:Boolean):Boolean; override;
   function CopyFromDescriptor(ADescriptor:Pointer;ASize:LongWord):Boolean; override;

   function ReadSecurity(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
   function WriteSecurity(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
   function WriteSecurityEx(ABuffer:Pointer;var AOffset,ASize:LongWord;ALocal:Boolean;AVersion:Word):Boolean;
 end;

 TNTFSSecurityId = class(TObject)            {Data of index $SII in file $Secure} {Not Used}
  private
   {Private Variables}
  public
   {Public Properties}
 end;

 TNTFSSecurityHash = class(TObject)          {Data of index $SDH in file $Secure} {Not Used}
  private
   {Private Variables}
  public
   {Public Properties}
 end;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{TNTFSDiskTable}
constructor TNTFSDiskTable.Create(ALocalLock:TMutexHandle);
begin
 {}
 inherited Create(ALocalLock);
 FStartSector:=ntfsUnknownSector;
 FStartCluster:=ntfsUnknownCluster;

 FEntry:=nil;
end;

{==============================================================================}

destructor TNTFSDiskTable.Destroy;
begin
 {}
 FEntry:=nil;
 inherited Destroy;
end;

{==============================================================================}
{==============================================================================}
{TNTFSDiskBlock}
constructor TNTFSDiskBlock.Create(ALocalLock:TMutexHandle);
begin
 {}
 inherited Create(ALocalLock);
 FBlockCount:=0;
 FBlockBuffer:=nil;
 FBlockCluster:=ntfsUnknownCluster;
end;

{==============================================================================}

destructor TNTFSDiskBlock.Destroy;
begin
 {}
 if FBlockBuffer <> nil then FreeMem(FBlockBuffer);
 FBlockBuffer:=nil;
 inherited Destroy;
end;

{==============================================================================}
{==============================================================================}
{TNTFSDiskEntry}
constructor TNTFSDiskEntry.Create(ALocalLock:TMutexHandle;AOrigin:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute);
begin
 {}
 inherited Create(ALocalLock);
 FPrevEntry:=nil;
 FNextEntry:=nil;

 FOrigin:=AOrigin;
 FAttribute:=AAttribute;
 FAlternate:=nil;
end;

{==============================================================================}

destructor TNTFSDiskEntry.Destroy;
begin
 {}
 FOrigin:=nil;
 FAttribute:=nil;
 FAlternate:=nil;
 inherited Destroy;
end;

{==============================================================================}

procedure TNTFSDiskEntry.SetOrigin(AOrigin:TNTFSDiskRecord);
begin
 {}
 if not AcquireLock then Exit;

 FOrigin:=AOrigin;
 
 ReleaseLock;
end;

{==============================================================================}

function TNTFSDiskEntry.GetDot:TNTFSDiskEntry;
{Called on the Parent to find the Dot entry}
var
 Entry:TNTFSDiskEntry;
begin
 {}
 Result:=nil;
 
 {Check Relative}
 if (Attributes and (faDot or faDotDot)) <> faNone then Exit;
 
 {Check Directory}
 if (Attributes and faDirectory) = faDirectory then
  begin
   {Get self relative entry}
   Entry:=TNTFSDiskEntry(FirstChild);
   while Entry <> nil do
    begin
     if (Entry.Attributes and faDot) = faDot then
      begin
       Result:=Entry;
       Exit;
      end;
      
     Entry:=TNTFSDiskEntry(Entry.Next);
    end;
  end;
end;

{==============================================================================}

function TNTFSDiskEntry.GetDotDot:TNTFSDiskEntry;
{Called on the Parent to find the DotDot entry}
var
 Entry:TNTFSDiskEntry;
begin
 {}
 Result:=nil;
 
 {Check Relative}
 if (Attributes and (faDot or faDotDot)) <> faNone then Exit;
 
 {Check Directory}
 if (Attributes and faDirectory) = faDirectory then
  begin
   {Get parent relative entry}
   Entry:=TNTFSDiskEntry(FirstChild);
   while Entry <> nil do
    begin
     if (Entry.Attributes and faDotDot) = faDotDot then
      begin
       Result:=Entry;
       Exit;
      end;
      
     Entry:=TNTFSDiskEntry(Entry.Next);
    end;
  end;
end;

{==============================================================================}

function TNTFSDiskEntry.CreateDot:TNTFSDiskEntry;
{Called on the Parent to create the Dot entry}
begin
 {}
 Result:=nil;
 
 {Check Relative}
 if (Attributes and (faDot or faDotDot)) <> faNone then Exit;
 
 {Check Directory}
 if (Attributes and faDirectory) = faDirectory then
  begin
   {Create self relative entry}
   Result:=TNTFSDiskEntry.Create(FLocalLock,FOrigin,FAttribute);
   Result.Name:=ntfsDotName;
   Result.AltName:=ntfsBlankName;
   Result.Size:=Size;
   Result.Used:=Used;
   Result.Allocated:=Allocated;
   Result.Attributes:=(Attributes or faDot);
   Result.WriteTime:=WriteTime;
   Result.CreateTime:=CreateTime;
   Result.AccessTime:=AccessTime;
   Result.ChangeTime:=ChangeTime;
   Result.ReparseTag:=ReparseTag;
   Result.EntriesLoaded:=True;
  end;
end;

{==============================================================================}

function TNTFSDiskEntry.CreateDotDot:TNTFSDiskEntry;
{Called on the Parent to create the DotDot entry}
begin
 {}
 Result:=nil;
 
 {Check Parent}
 if Parent = nil then Exit;
 
 {Check Relative}
 if (Attributes and (faDot or faDotDot)) <> faNone then Exit;
 
 {Check Directory}
 if (Attributes and faDirectory) = faDirectory then
  begin
   {Create parent relative entry}
   Result:=TNTFSDiskEntry.Create(FLocalLock,TNTFSDiskEntry(Parent).Origin,TNTFSDiskEntry(Parent).Attribute);
   Result.Name:=ntfsDotDotName;
   Result.AltName:=ntfsBlankName;
   Result.Size:=TNTFSDiskEntry(Parent).Size;
   Result.Used:=TNTFSDiskEntry(Parent).Used;
   Result.Allocated:=TNTFSDiskEntry(Parent).Allocated;
   Result.Attributes:=(TNTFSDiskEntry(Parent).Attributes or faDotDot);
   Result.WriteTime:=TNTFSDiskEntry(Parent).WriteTime;
   Result.CreateTime:=TNTFSDiskEntry(Parent).CreateTime;
   Result.AccessTime:=TNTFSDiskEntry(Parent).AccessTime;
   Result.ChangeTime:=TNTFSDiskEntry(Parent).ChangeTime;
   Result.ReparseTag:=TNTFSDiskEntry(Parent).ReparseTag;
   Result.EntriesLoaded:=True;
  end;
end;

{==============================================================================}

function TNTFSDiskEntry.UpdateDot:Boolean;
{Called on the Dot entry to update from the Parent}
begin
 {}
 Result:=False;
 
 {Check Parent}
 if Parent = nil then Exit;
 
 {Check Dot}
 if (Attributes and faDot) = faDot then
  begin
   {Parents self relative entry update}
   Name:=ntfsDotName;
   AltName:=ntfsBlankName;
   Size:=TNTFSDiskEntry(Parent).Size;
   Used:=TNTFSDiskEntry(Parent).Used;
   Allocated:=TNTFSDiskEntry(Parent).Allocated;
   Attributes:=(TNTFSDiskEntry(Parent).Attributes or faDot);
   WriteTime:=TNTFSDiskEntry(Parent).WriteTime;
   CreateTime:=TNTFSDiskEntry(Parent).CreateTime;
   AccessTime:=TNTFSDiskEntry(Parent).AccessTime;
   ChangeTime:=TNTFSDiskEntry(Parent).ChangeTime;
   ReparseTag:=TNTFSDiskEntry(Parent).ReparseTag;
   EntriesLoaded:=True;
   Result:=True;
  end;
end;

{==============================================================================}

function TNTFSDiskEntry.UpdateDotDot(AEntry:TNTFSDiskEntry):Boolean;
{Called on the DotDot entry to update from the supplied Entry}
begin
 {}
 Result:=False;
 
 {Check Entry}
 if AEntry = nil then Exit;
 
 {Check DotDot}
 if (Attributes and faDotDot) = faDotDot then
  begin
   {Parents parent relative entry update}
   FOrigin:=AEntry.Origin;
   FAttribute:=AEntry.Attribute;
   Name:=ntfsDotDotName;
   AltName:=ntfsBlankName;
   Size:=TNTFSDiskEntry(AEntry).Size;
   Used:=TNTFSDiskEntry(AEntry).Used;
   Allocated:=TNTFSDiskEntry(AEntry).Allocated;
   Attributes:=(TNTFSDiskEntry(AEntry).Attributes or faDotDot);
   WriteTime:=TNTFSDiskEntry(AEntry).WriteTime;
   CreateTime:=TNTFSDiskEntry(AEntry).CreateTime;
   AccessTime:=TNTFSDiskEntry(AEntry).AccessTime;
   ChangeTime:=TNTFSDiskEntry(AEntry).ChangeTime;
   ReparseTag:=TNTFSDiskEntry(AEntry).ReparseTag;
   EntriesLoaded:=True;
   Result:=True;
  end;
end;
{==============================================================================}

function TNTFSDiskEntry.RecordNumber:Int64;
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;
 try
  if FOrigin = nil then Exit;
 
  Result:=FOrigin.RecordNumber;
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TNTFSDiskEntry.FileReference:Int64;
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;
 try
  if FOrigin = nil then Exit;
 
  Result:=FOrigin.FileReference;
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TNTFSDiskEntry.UpdateEntry:Boolean;
var
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  if FOrigin = nil then Exit;
  
  {Get Attribute} {StandardInformation}
  Attribute:=FOrigin.GetAttribute(ntfsAttrTypeStandardInformation,ntfsBlankName,ntfsInstanceFirst);
  if Attribute = nil then Exit;
  if not Attribute.UpdateEntry(Self) then Exit;
  
  {Check Entry}
  if (Attributes and faStream) = faNone then
   begin
    {Get Attribute} {Data} {Optional}
    Attribute:=FOrigin.GetAttribute(ntfsAttrTypeData,ntfsBlankName,ntfsInstanceFirst);
    if Attribute <> nil then if not Attribute.UpdateEntry(Self) then Exit;
    
    {Check Attribute} {FileName}
    if FAttribute = nil then Exit;
    if not FAttribute.UpdateEntry(Self) then Exit;
    
    {Check Alternate} {FileName} {Optional}
    if FAlternate <> nil then if not FAlternate.UpdateEntry(Self) then Exit;
    
    Result:=True;
   end
  else
   begin
    {Check Attribute} {Data}
    if FAttribute = nil then Exit;
    if not FAttribute.UpdateEntry(Self) then Exit;
    
    {Check Alternate} {None}
    if FAlternate <> nil then Exit;
    
    Result:=True;
   end;
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TNTFSDiskEntry.UpdateRecord:Boolean;
{Note: This cannot be used as the Data attribute must be updated by Size Attribute}
var
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  if FOrigin = nil then Exit;
  
  {Get Attribute} {StandardInformation}
  Attribute:=FOrigin.GetAttribute(ntfsAttrTypeStandardInformation,ntfsBlankName,ntfsInstanceFirst);
  if Attribute = nil then Exit;
  if not Attribute.UpdateAttribute(Self) then Exit;
  
  {Check Entry}
  if (Attributes and faStream) = faNone then
   begin
    {Get Attribute} {Data} {Optional}
    Attribute:=FOrigin.GetAttribute(ntfsAttrTypeData,ntfsBlankName,ntfsInstanceFirst);
    if Attribute <> nil then if not Attribute.UpdateAttribute(Self) then Exit;
    
    {Check Attribute} {FileName}
    if FAttribute = nil then Exit;
    if not FAttribute.UpdateAttribute(Self) then Exit;
    
    {Check Alternate} {FileName} {Optional}
    if FAlternate <> nil then if not FAlternate.UpdateAttribute(Self) then Exit;
    
    Result:=True;
   end
  else
   begin
    {Check Attribute} {Data}
    if FAttribute = nil then Exit;
    if not FAttribute.UpdateAttribute(Self) then Exit;
    
    {Check Alternate} {None}
    if FAlternate <> nil then Exit;
    
    Result:=True;
   end;
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TNTFSDiskEntry.FindFirstName(AHandle:TFindHandle;AReference:Boolean):TDiskEntry;
{Note: Caller must hold the handle writer lock}
var
 Base:TNTFSDiskRecord;
begin
 {}
 Result:=nil;
 
 if not AcquireLock then Exit;
 try
  if FOrigin = nil then Exit;
  if AHandle = nil then Exit;
  if FOrigin.Links = nil then Exit;
  if AHandle.ParentEntry <> Self then Exit;

  {Get Base}
  Base:=FOrigin.Origin;
  if Base = nil then Exit;
  if Base.Links = nil then Exit;
 
  {Get First Name}
  Result:=Base.Links.FirstEntry;

  {Add Reference}
  if AReference and (Result <> nil) then Result.AddReference;
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TNTFSDiskEntry.FindPrevName(AHandle:TFindHandle;AReference:Boolean):TDiskEntry;
{Note: Caller must hold the handle writer lock}
var
 Current:TNTFSDiskEntry;
begin
 {}
 Result:=nil;
 
 if not AcquireLock then Exit;
 try
  if AHandle = nil then Exit;
  if AHandle.CurrentEntry = nil then Exit;
  if AHandle.ParentEntry <> Self then Exit;

  {Get Current}
  Current:=TNTFSDiskEntry(AHandle.CurrentEntry);
  if Current = nil then Exit;
  
  {Get Prev Name}
  Result:=Current.PrevEntry;

  {Add Reference}
  if AReference and (Result <> nil) then Result.AddReference;
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TNTFSDiskEntry.FindNextName(AHandle:TFindHandle;AReference:Boolean):TDiskEntry;
{Note: Caller must hold the handle writer lock}
var
 Current:TNTFSDiskEntry;
begin
 {}
 Result:=nil;
 
 if not AcquireLock then Exit;
 try
  if AHandle = nil then Exit;
  if AHandle.CurrentEntry = nil then Exit;
  if AHandle.ParentEntry <> Self then Exit;

  {Get Current}
  Current:=TNTFSDiskEntry(AHandle.CurrentEntry);
  if Current = nil then Exit;
  
  {Get Next Name}
  Result:=Current.NextEntry;

  {Add Reference}
  if AReference and (Result <> nil) then Result.AddReference;
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TNTFSDiskEntry.FindLastName(AHandle:TFindHandle;AReference:Boolean):TDiskEntry;
{Note: Caller must hold the handle writer lock}
var
 Base:TNTFSDiskRecord;
begin
 {}
 Result:=nil;
 
 if not AcquireLock then Exit;
 try
  if FOrigin = nil then Exit;
  if AHandle = nil then Exit;
  if FOrigin.Links = nil then Exit;
  if AHandle.ParentEntry <> Self then Exit;

  {Get Base}
  Base:=FOrigin.Origin;
  if Base = nil then Exit;
  if Base.Links = nil then Exit;
  
  {Get Last Name}
  Result:=Base.Links.LastEntry;
  
  {Add Reference}
  if AReference and (Result <> nil) then Result.AddReference;
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{TNTFSDiskAcl}
constructor TNTFSDiskAcl.Create(ALocalLock:TMutexHandle);
begin
 {}
 inherited Create(ALocalLock);
 //To Do //
end;

{==============================================================================}

destructor TNTFSDiskAcl.Destroy;
begin
 {}
 //To Do //
 inherited Destroy;
end;

{==============================================================================}
{==============================================================================}
{TNTFSDiskAce}
constructor TNTFSDiskAce.Create(ALocalLock:TMutexHandle);
begin
 {}
 inherited Create(ALocalLock);
 //To Do //
end;

{==============================================================================}

destructor TNTFSDiskAce.Destroy;
begin
 {}
 //To Do //
 inherited Destroy;
end;

{==============================================================================}
{==============================================================================}
{TNTFSEntryList}
constructor TNTFSEntryList.Create;
begin
 {}
 inherited Create;
 FFirstEntry:=nil;
 FLastEntry:=nil;
 
 FEntryCount:=0;
end;

{==============================================================================}

destructor TNTFSEntryList.Destroy;
begin
 {}
 ClearList;
 inherited Destroy;
end;

{==============================================================================}

function TNTFSEntryList.Link(AEntry:TNTFSDiskEntry):Boolean;
{Link Entry to PrevEntry,NextEntry Siblings}
var
 PrevEntry:TNTFSDiskEntry;
begin
 {}
 Result:=False;
 
 if AEntry = nil then Exit;
 
 PrevEntry:=FLastEntry;
 if PrevEntry = nil then
  begin
   {Is First Object}
   AEntry.PrevEntry:=nil;
   AEntry.NextEntry:=nil;
   FFirstEntry:=AEntry;
   FLastEntry:=AEntry;
   
   Result:=True;
  end
 else
  begin
   {Not First Object}
   PrevEntry.NextEntry:=AEntry;
   AEntry.PrevEntry:=PrevEntry;
   AEntry.NextEntry:=nil;
   FLastEntry:=AEntry;
   
   Result:=True;
  end;
end;

{==============================================================================}

function TNTFSEntryList.Unlink(AEntry:TNTFSDiskEntry):Boolean;
{Unlink Entry from PrevEntry,NextEntry Siblings}
var
 PrevEntry,NextEntry:TNTFSDiskEntry;
begin
 {}
 Result:=False;
 
 if AEntry = nil then Exit;
 
 if AEntry.PrevEntry <> nil then
  begin
   {Not First Object}
   PrevEntry:=AEntry.PrevEntry;
   if AEntry.NextEntry <> nil then
    begin
     {Not Last Object}
     NextEntry:=AEntry.NextEntry;
     PrevEntry.NextEntry:=NextEntry;
     NextEntry.PrevEntry:=PrevEntry;
    end
   else
    begin
     {Is Last Object}
     PrevEntry.NextEntry:=nil;
     FLastEntry:=PrevEntry;
    end;
  end
 else
  begin
   {Is First Object}
   if AEntry.NextEntry <> nil then
    begin
     {Not Last Object}
     NextEntry:=AEntry.NextEntry;
     NextEntry.PrevEntry:=nil;
     FFirstEntry:=NextEntry;
    end
   else
    begin
     {Is Last Object}
     FFirstEntry:=nil;
     FLastEntry:=nil;
    end;
  end;
  
 AEntry.PrevEntry:=nil;
 AEntry.NextEntry:=nil;
 
 Result:=True;
end;

{==============================================================================}

function TNTFSEntryList.Add(AEntry:TNTFSDiskEntry):Boolean;
{Add Entry to List and Link with Siblings}
begin
 {}
 Result:=False;
 
 if AEntry = nil then Exit;
 
 if Link(AEntry) then
  begin
   Inc(FEntryCount);
   
   Result:=True;
  end;
end;

{==============================================================================}

function TNTFSEntryList.Remove(AEntry:TNTFSDiskEntry):Boolean;
{Unlink Entry from Siblings and Remove from List}
begin
 {}
 Result:=False;
 
 if AEntry = nil then Exit;
 
 if Unlink(AEntry) then
  begin
   Dec(FEntryCount);
   
   Result:=True;
  end;
end;

{==============================================================================}

procedure TNTFSEntryList.ClearList;
begin
 {}
 {Do Not Clear Entries}
 {Reset Defaults}
 FFirstEntry:=nil;
 FLastEntry:=nil;
 FEntryCount:=0;
end;

{==============================================================================}
{==============================================================================}
{TNTFSRecordList}
constructor TNTFSRecordList.Create;
begin
 {}
 inherited Create;
 FFirstRecord:=nil;
 FLastRecord:=nil;
 
 FRecordCount:=0;
end;

{==============================================================================}

destructor TNTFSRecordList.Destroy;
begin
 {}
 ClearList;
 inherited Destroy;
end;

{==============================================================================}

function TNTFSRecordList.Link(ARecord:TNTFSDiskRecord):Boolean;
{Link Record to PrevRecord,NextRecord Siblings}
var
 PrevRecord:TNTFSDiskRecord;
begin
 {}
 Result:=False;
 
 if ARecord = nil then Exit;
 
 PrevRecord:=FLastRecord;
 if PrevRecord = nil then
  begin
   {Is First Object}
   ARecord.PrevRecord:=nil;
   ARecord.NextRecord:=nil;
   FFirstRecord:=ARecord;
   FLastRecord:=ARecord;
   
   Result:=True;
  end
 else
  begin
   {Not First Object}
   PrevRecord.NextRecord:=ARecord;
   ARecord.PrevRecord:=PrevRecord;
   ARecord.NextRecord:=nil;
   FLastRecord:=ARecord;
   
   Result:=True;
  end;
end;

{==============================================================================}

function TNTFSRecordList.Unlink(ARecord:TNTFSDiskRecord):Boolean;
{Unlink Record from PrevRecord,NextRecord Siblings}
var
 PrevRecord,NextRecord:TNTFSDiskRecord;
begin
 {}
 Result:=False;
 
 if ARecord = nil then Exit;
 
 if ARecord.PrevRecord <> nil then
  begin
   {Not First Object}
   PrevRecord:=ARecord.PrevRecord;
   if ARecord.NextRecord <> nil then
    begin
     {Not Last Object}
     NextRecord:=ARecord.NextRecord;
     PrevRecord.NextRecord:=NextRecord;
     NextRecord.PrevRecord:=PrevRecord;
    end
   else
    begin
     {Is Last Object}
     PrevRecord.NextRecord:=nil;
     FLastRecord:=PrevRecord;
    end;
  end
 else
  begin
   {Is First Object}
   if ARecord.NextRecord <> nil then
    begin
     {Not Last Object}
     NextRecord:=ARecord.NextRecord;
     NextRecord.PrevRecord:=nil;
     FFirstRecord:=NextRecord;
    end
   else
    begin
     {Is Last Object}
     FFirstRecord:=nil;
     FLastRecord:=nil;
    end;
  end;
  
 ARecord.PrevRecord:=nil;
 ARecord.NextRecord:=nil;
 
 Result:=True;
end;

{==============================================================================}

function TNTFSRecordList.Add(ARecord:TNTFSDiskRecord):Boolean;
{Add Record to List and Link with Siblings}
begin
 {}
 Result:=False;
 
 if ARecord = nil then Exit;
 
 if Link(ARecord) then
  begin
   Inc(FRecordCount);
   
   Result:=True;
  end;
end;

{==============================================================================}

function TNTFSRecordList.Remove(ARecord:TNTFSDiskRecord):Boolean;
{Unlink Record from Siblings and Remove from List}
begin
 {}
 Result:=False;
 
 if ARecord = nil then Exit;
 
 if Unlink(ARecord) then
  begin
   Dec(FRecordCount);
   
   Result:=True;
  end;
end;

{==============================================================================}

procedure TNTFSRecordList.ClearList;
begin
 {}
 {Do Not Clear Entries}
 {Reset Defaults}
 FFirstRecord:=nil;
 FLastRecord:=nil;
 FRecordCount:=0;
end;

{==============================================================================}
{==============================================================================}
{TNTFSRecordIndex}
constructor TNTFSRecordIndex.Create;
begin
 {}
 inherited Create;
 FLock:=SynchronizerCreate;
 FRecordLocal:=MutexCreate;
 
 FRunsLock:=SynchronizerCreate;
 FItemsLock:=SynchronizerCreate;
 FNodesLock:=SynchronizerCreate;
 FIndexLock:=SynchronizerCreate;
 FExtendedsLock:=SynchronizerCreate;
 FAttributesLock:=SynchronizerCreate;
 
 FRunLocal:=MutexCreate;
 FItemLocal:=MutexCreate;
 FKeyLocal:=MutexCreate;
 FNodeLocal:=MutexCreate;
 FExtendedLocal:=MutexCreate;
 FAttributeLocal:=MutexCreate;
 
 Order:=12; //36; {May need to be adjusted}  //To Do //Testing8
end;

{==============================================================================}

destructor TNTFSRecordIndex.Destroy;
begin
 {}
 WriterLock;
 try
  MutexDestroy(FAttributeLocal);
  MutexDestroy(FExtendedLocal);
  MutexDestroy(FNodeLocal);
  MutexDestroy(FKeyLocal);
  MutexDestroy(FItemLocal);
  MutexDestroy(FRunLocal);
 
  SynchronizerDestroy(FAttributesLock);
  SynchronizerDestroy(FExtendedsLock);
  SynchronizerDestroy(FIndexLock);
  SynchronizerDestroy(FNodesLock);
  SynchronizerDestroy(FItemsLock);
  SynchronizerDestroy(FRunsLock);
 
  MutexDestroy(FRecordLocal);
  inherited Destroy;
 finally
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  SynchronizerDestroy(FLock);
 end;
end;

{==============================================================================}

function TNTFSRecordIndex.Find(const ARecordNumber:Int64;ACurrent:TNTFSDiskRecord):TNTFSDiskRecord;
{Note: Caller must hold the records lock}
begin
 {}
 Result:=nil;
 
 if ACurrent = nil then Exit;
 
 if ACurrent.Blank then
  begin
   Result:=Find(ARecordNumber,TNTFSDiskRecord(ACurrent.Child));
  end
 else
  begin
   if ARecordNumber = ACurrent.RecordNumber then
    begin
     Result:=ACurrent;
    end
   else if ARecordNumber < ACurrent.RecordNumber then
    begin
     Result:=Find(ARecordNumber,TNTFSDiskRecord(ACurrent.Child));
    end
   else if ARecordNumber > ACurrent.RecordNumber then
    begin
     Result:=Find(ARecordNumber,TNTFSDiskRecord(ACurrent.Right));
    end;
  end;
end;

{==============================================================================}

function TNTFSRecordIndex.CreateBlank:TBtreeObject;
begin
 {}
 Result:=TNTFSDiskRecord.Create(FRecordLocal,nil);
 Result.Blank:=True;
 
 {List Locks}
 TNTFSDiskRecord(Result).FRunsLock:=FRunsLock;
 TNTFSDiskRecord(Result).FItemsLock:=FItemsLock;
 TNTFSDiskRecord(Result).FNodesLock:=FNodesLock;
 TNTFSDiskRecord(Result).FIndexLock:=FIndexLock;
 TNTFSDiskRecord(Result).FExtendedsLock:=FExtendedsLock;
 TNTFSDiskRecord(Result).FAttributesLock:=FAttributesLock;
 
 {Local Locks}
 TNTFSDiskRecord(Result).FRunLocal:=FRunLocal;
 TNTFSDiskRecord(Result).FItemLocal:=FItemLocal;
 TNTFSDiskRecord(Result).FKeyLocal:=FKeyLocal;
 TNTFSDiskRecord(Result).FNodeLocal:=FNodeLocal;
 TNTFSDiskRecord(Result).FExtendedLocal:=FExtendedLocal;
 TNTFSDiskRecord(Result).FAttributeLocal:=FAttributeLocal;
end;

{==============================================================================}

function TNTFSRecordIndex.Compare(AEntry1,AEntry2:TBtreeObject):Integer;
begin
 {}
 Result:=ntfsCompareEqual; {Equal to fail Insert/Merge/Borrow}
 
 if AEntry1 = nil then Exit;
 if AEntry2 = nil then Exit;
 
 Result:=ntfsCompareLess;
 
 if AEntry2.Blank then Exit;
 
 {AEntry1.Blank is an error}
 if TNTFSDiskRecord(AEntry1).RecordNumber = TNTFSDiskRecord(AEntry2).RecordNumber then
  begin
   Result:=ntfsCompareEqual;
  end
 else if TNTFSDiskRecord(AEntry1).RecordNumber < TNTFSDiskRecord(AEntry2).RecordNumber then
  begin
   Result:=ntfsCompareLess;
  end
 else if TNTFSDiskRecord(AEntry1).RecordNumber > TNTFSDiskRecord(AEntry2).RecordNumber then
  begin
   Result:=ntfsCompareGreater;
  end;
end;

{==============================================================================}

function TNTFSRecordIndex.CreateRecord(ABase:TNTFSDiskRecord;const ARecordNumber:Int64;AVersion:Word):TNTFSDiskRecord;
{Create a record, setup properties, do not insert in index}
{Note: Caller must hold the records lock}
begin
 {Create Record}
 Result:=TNTFSDiskRecord.Create(FRecordLocal,ABase);
 Result.RecordNumber:=ARecordNumber;

 {List Locks}
 Result.FRunsLock:=FRunsLock;
 Result.FItemsLock:=FItemsLock;
 Result.FNodesLock:=FNodesLock;
 Result.FIndexLock:=FIndexLock;
 Result.FExtendedsLock:=FExtendedsLock;
 Result.FAttributesLock:=FAttributesLock;
 
 {Local Locks}
 Result.FRunLocal:=FRunLocal;
 Result.FItemLocal:=FItemLocal;
 Result.FKeyLocal:=FKeyLocal;
 Result.FNodeLocal:=FNodeLocal;
 Result.FExtendedLocal:=FExtendedLocal;
 Result.FAttributeLocal:=FAttributeLocal;
 
 {Add to List}
 if Result.Base = nil then Result.CreateRecords(True);     {Add new record to its own record list}
 if Result.Base <> nil then Result.Base.AddRecord(Result); {Add new record to parents record list}
end;

{==============================================================================}

function TNTFSRecordIndex.DestroyRecord(ARecord:TNTFSDiskRecord):Boolean;
{Free the record, do not remove from index}
{Note: Caller must hold the records lock}
begin
 {}
 Result:=False;
 
 if ARecord = nil then Exit;
 
 {Remove from List}
 if ARecord.Base <> nil then ARecord.Base.RemoveRecord(ARecord);
 
 {Free Record}
 ARecord.Free;
 
 Result:=True;
end;

{==============================================================================}

function TNTFSRecordIndex.NewRecord(ABase:TNTFSDiskRecord;const ARecordNumber:Int64;AVersion:Word):TNTFSDiskRecord;
{Create a record, setup properties, do not insert in index}
{Note: Caller must hold the records lock}
begin
 {Create Record}
 Result:=CreateRecord(ABase,ARecordNumber,AVersion);
 if Result = nil then Exit;
 
 {Setup Record}
 Result.CreateAttributes(AVersion,True);  {Add the end attribute to the new record}
end;

{==============================================================================}

function TNTFSRecordIndex.InsertRecord(ARecord:TNTFSDiskRecord):Boolean;
{Insert the record in the index (Blank not allowed)}
{Note: Caller must hold the records lock}
begin
 {}
 Result:=False;
 
 if ARecord = nil then Exit;
 if ARecord.Blank then Exit;
 
 {Insert Record}
 Result:=Insert(ARecord);
end;

{==============================================================================}

function TNTFSRecordIndex.DeleteRecord(ARecord:TNTFSDiskRecord):Boolean;
{Remove the record from the index, do not free (Blank not allowed)}
{Note: Caller must hold the records lock}
begin
 {}
 Result:=False;
 
 if ARecord = nil then Exit;
 if ARecord.Blank then Exit;
 
 {Remove Record}
 Result:=Remove(ARecord);
end;

{==============================================================================}

function TNTFSRecordIndex.RemoveRecord(ARecord:TNTFSDiskRecord):Boolean;
{Remove the record from the index and free (Blank not allowed)}
{Note: Caller must hold the records lock}
begin
 {}
 Result:=False;
 
 if ARecord = nil then Exit;
 if ARecord.Blank then Exit;
 
 {Remove from List}
 if ARecord.Base <> nil then ARecord.Base.RemoveRecord(ARecord);
 
 {Remove Record}
 if not Remove(ARecord) then Exit;
 
 {Free Record}
 ARecord.Free;
 
 Result:=True;
end;

{==============================================================================}

function TNTFSRecordIndex.FindRecord(const ARecordNumber:Int64):TNTFSDiskRecord;
{Note: Caller must hold the records lock}
begin
 {}
 Result:=Find(ARecordNumber,TNTFSDiskRecord(FRoot));
end;

{==============================================================================}

function TNTFSRecordIndex.ReaderLock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSRecordIndex.ReaderUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSRecordIndex.ReaderConvert:Boolean; 
{Convert a Reader lock to a Writer lock}
begin
 {}
 Result:=(SynchronizerReaderConvert(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSRecordIndex.WriterLock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSRecordIndex.WriterUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSRecordIndex.WriterConvert:Boolean;
{Convert a Writer lock to a Reader lock}
begin
 {}
 Result:=(SynchronizerWriterConvert(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSRecordIndex.RunsReaderLock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderLock(FRunsLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSRecordIndex.RunsReaderUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderUnlock(FRunsLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSRecordIndex.RunsWriterLock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterLock(FRunsLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSRecordIndex.RunsWriterUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterUnlock(FRunsLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSRecordIndex.ItemsReaderLock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderLock(FItemsLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSRecordIndex.ItemsReaderUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderUnlock(FItemsLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSRecordIndex.ItemsWriterLock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterLock(FItemsLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSRecordIndex.ItemsWriterUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterUnlock(FItemsLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSRecordIndex.NodesReaderLock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderLock(FNodesLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSRecordIndex.NodesReaderUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderUnlock(FNodesLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSRecordIndex.NodesWriterLock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterLock(FNodesLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSRecordIndex.NodesWriterUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterUnlock(FNodesLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSRecordIndex.IndexReaderLock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderLock(FIndexLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSRecordIndex.IndexReaderUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderUnlock(FIndexLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSRecordIndex.IndexWriterLock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterLock(FIndexLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSRecordIndex.IndexWriterUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterUnlock(FIndexLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSRecordIndex.ExtendedsReaderLock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderLock(FExtendedsLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSRecordIndex.ExtendedsReaderUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderUnlock(FExtendedsLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSRecordIndex.ExtendedsWriterLock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterLock(FExtendedsLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSRecordIndex.ExtendedsWriterUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterUnlock(FExtendedsLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSRecordIndex.AttributesReaderLock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderLock(FAttributesLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSRecordIndex.AttributesReaderUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderUnlock(FAttributesLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSRecordIndex.AttributesWriterLock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterLock(FAttributesLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSRecordIndex.AttributesWriterUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterUnlock(FAttributesLock) = ERROR_SUCCESS);
end;

{==============================================================================}
{==============================================================================}
{TNTFSDiskRecord}
constructor TNTFSDiskRecord.Create(ALocalLock:TMutexHandle;ABase:TNTFSDiskRecord);
begin
 {}
 inherited Create;
 FLocalLock:=ALocalLock;
 
 FRunsLock:=INVALID_HANDLE_VALUE;
 FItemsLock:=INVALID_HANDLE_VALUE;
 FNodesLock:=INVALID_HANDLE_VALUE;
 FIndexLock:=INVALID_HANDLE_VALUE;
 FExtendedsLock:=INVALID_HANDLE_VALUE;
 FAttributesLock:=INVALID_HANDLE_VALUE;
   
 FRunLocal:=INVALID_HANDLE_VALUE;
 FItemLocal:=INVALID_HANDLE_VALUE;
 FKeyLocal:=INVALID_HANDLE_VALUE;
 FNodeLocal:=INVALID_HANDLE_VALUE;
 FExtendedLocal:=INVALID_HANDLE_VALUE;
 FAttributeLocal:=INVALID_HANDLE_VALUE;
 
 FStatus:=ntfsStatusNone;

 FPrevRecord:=nil;
 FNextRecord:=nil;

 FRecordFlags:=ntfsFileRecordFlagNone;
 FHardLinkCount:=0;
 FSequenceNumber:=0;
 FNextAttributeId:=0;
 FRecordNumber:=ntfsUnknownRecordNumber;
 FRecordSize:=0;
 FRecordAllocated:=0;

 FAttributeOffset:=0;
 FUpdateSequenceOffset:=0;
 FUpdateSequenceLength:=0;

 FUpdateSequenceNumber:=0;
 FLogFileSequenceNumber:=0;

 FBase:=ABase;
 FLinks:=nil;
 FStreams:=nil;
 FRecords:=nil;
 FAttributes:=nil;
end;

{==============================================================================}

destructor TNTFSDiskRecord.Destroy;
begin
 {}
 FBase:=nil;
 if FLinks <> nil then FLinks.Free;
 if FStreams <> nil then FStreams.Free;
 if FRecords <> nil then FRecords.Free;
 if FAttributes <> nil then FAttributes.Free;

 FRunLocal:=INVALID_HANDLE_VALUE;
 FItemLocal:=INVALID_HANDLE_VALUE;
 FKeyLocal:=INVALID_HANDLE_VALUE;
 FNodeLocal:=INVALID_HANDLE_VALUE;
 FExtendedLocal:=INVALID_HANDLE_VALUE;
 FAttributeLocal:=INVALID_HANDLE_VALUE;
 
 FRunsLock:=INVALID_HANDLE_VALUE;
 FItemsLock:=INVALID_HANDLE_VALUE;
 FNodesLock:=INVALID_HANDLE_VALUE;
 FIndexLock:=INVALID_HANDLE_VALUE;
 FExtendedsLock:=INVALID_HANDLE_VALUE;
 FAttributesLock:=INVALID_HANDLE_VALUE;
 
 FLocalLock:=INVALID_HANDLE_VALUE;
 inherited Destroy;
end;

{==============================================================================}

function TNTFSDiskRecord.AcquireLock:Boolean;
begin
 {}
 Result:=(MutexLock(FLocalLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSDiskRecord.ReleaseLock:Boolean;
begin
 {}
 Result:=(MutexUnlock(FLocalLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSDiskRecord.GetResizing:Boolean;
begin
 {}
 Result:=((FStatus and ntfsStatusResizing) = ntfsStatusResizing);
end;

{==============================================================================}

procedure TNTFSDiskRecord.SetResizing(AValue:Boolean);
begin
 {}
 if AValue then
  begin
   FStatus:=(FStatus or ntfsStatusResizing);
  end
 else
  begin
   FStatus:=(FStatus and not ntfsStatusResizing);
  end;
end;

{==============================================================================}

function TNTFSDiskRecord.GetRemoving:Boolean;
begin
 {}
 Result:=((FStatus and ntfsStatusRemoving) = ntfsStatusRemoving);
end;

{==============================================================================}

procedure TNTFSDiskRecord.SetRemoving(AValue:Boolean);
begin
 {}
 if AValue then
  begin
   FStatus:=(FStatus or ntfsStatusRemoving);
  end
 else
  begin
   FStatus:=(FStatus and not ntfsStatusRemoving);
  end;
end;

{==============================================================================}

function TNTFSDiskRecord.GetMirrored:Boolean;
begin
 {}
 Result:=((FStatus and ntfsStatusMirrored) = ntfsStatusMirrored);
end;

{==============================================================================}

procedure TNTFSDiskRecord.SetMirrored(AValue:Boolean);
begin
 {}
 if AValue then
  begin
   FStatus:=(FStatus or ntfsStatusMirrored);
  end
 else
  begin
   FStatus:=(FStatus and not ntfsStatusMirrored);
  end;
end;

{==============================================================================}

function TNTFSDiskRecord.GetMetafile:Boolean;
begin
 {}
 Result:=((FStatus and ntfsStatusMetafile) = ntfsStatusMetafile);
end;

{==============================================================================}

procedure TNTFSDiskRecord.SetMetafile(AValue:Boolean);
begin
 {}
 if AValue then
  begin
   FStatus:=(FStatus or ntfsStatusMetafile);
  end
 else
  begin
   FStatus:=(FStatus and not ntfsStatusMetafile);
  end;
end;

{==============================================================================}

function TNTFSDiskRecord.GetReserved:Boolean;
begin
 {}
 Result:=((FStatus and ntfsStatusReserved) = ntfsStatusReserved);
end;

{==============================================================================}

procedure TNTFSDiskRecord.SetReserved(AValue:Boolean);
begin
 {}
 if AValue then
  begin
   FStatus:=(FStatus or ntfsStatusReserved);
  end
 else
  begin
   FStatus:=(FStatus and not ntfsStatusReserved);
  end;
end;

{==============================================================================}

function TNTFSDiskRecord.GetExpansion:Boolean;
begin
 {}
 Result:=((FStatus and ntfsStatusExpansion) = ntfsStatusExpansion);
end;

{==============================================================================}

procedure TNTFSDiskRecord.SetExpansion(AValue:Boolean);
begin
 {}
 if AValue then
  begin
   FStatus:=(FStatus or ntfsStatusExpansion);
  end
 else
  begin
   FStatus:=(FStatus and not ntfsStatusExpansion);
  end;
end;

{==============================================================================}

function TNTFSDiskRecord.GetOverflow:Boolean;
begin
 {}
 Result:=((FStatus and ntfsStatusOverflow) = ntfsStatusOverflow);
end;

{==============================================================================}

procedure TNTFSDiskRecord.SetOverflow(AValue:Boolean);
begin
 {}
 if AValue then
  begin
   FStatus:=(FStatus or ntfsStatusOverflow);
  end
 else
  begin
   FStatus:=(FStatus and not ntfsStatusOverflow);
  end;
end;

{==============================================================================}

function TNTFSDiskRecord.GetExtension:Boolean;
begin
 {}
 Result:=((FStatus and ntfsStatusExtension) = ntfsStatusExtension);
end;

{==============================================================================}

procedure TNTFSDiskRecord.SetExtension(AValue:Boolean);
begin
 {}
 if AValue then
  begin
   FStatus:=(FStatus or ntfsStatusExtension);
  end
 else
  begin
   FStatus:=(FStatus and not ntfsStatusExtension);
  end;
end;

{==============================================================================}

function TNTFSDiskRecord.GetIsUsed:Boolean;
begin
 {}
 Result:=((FRecordFlags and ntfsFileRecordFlagInUse) = ntfsFileRecordFlagInUse);
end;

{==============================================================================}

procedure TNTFSDiskRecord.SetIsUsed(AValue:Boolean);
begin
 {}
 if AValue then
  begin
   FRecordFlags:=(FRecordFlags or ntfsFileRecordFlagInUse);
  end
 else
  begin
   FRecordFlags:=(FRecordFlags and not ntfsFileRecordFlagInUse);
  end;
end;

{==============================================================================}

function TNTFSDiskRecord.GetIsFolder:Boolean;
begin
 {}
 Result:=((FRecordFlags and ntfsFileRecordFlagDirectory) = ntfsFileRecordFlagDirectory);
end;

{==============================================================================}

procedure TNTFSDiskRecord.SetIsFolder(AValue:Boolean);
begin
 {}
 if AValue then
  begin
   FRecordFlags:=(FRecordFlags or ntfsFileRecordFlagDirectory);
  end
 else
  begin
   FRecordFlags:=(FRecordFlags and not ntfsFileRecordFlagDirectory);
  end;
end;

{==============================================================================}

function TNTFSDiskRecord.GetIsUnknown1:Boolean;
begin
 {}
 Result:=((FRecordFlags and ntfsFileRecordFlagUnknown1) = ntfsFileRecordFlagUnknown1);
end;

{==============================================================================}

procedure TNTFSDiskRecord.SetIsUnknown1(AValue:Boolean);
begin
 {}
 if AValue then
  begin
   FRecordFlags:=(FRecordFlags or ntfsFileRecordFlagUnknown1);
  end
 else
  begin
   FRecordFlags:=(FRecordFlags and not ntfsFileRecordFlagUnknown1);
  end;
end;

{==============================================================================}

function TNTFSDiskRecord.GetIsIndexView:Boolean;
begin
 {}
 Result:=((FRecordFlags and ntfsFileRecordFlagIndexView) = ntfsFileRecordFlagIndexView);
end;

{==============================================================================}

procedure TNTFSDiskRecord.SetIsIndexView(AValue:Boolean);
begin
 {}
 if AValue then
  begin
   FRecordFlags:=(FRecordFlags or ntfsFileRecordFlagIndexView);
  end
 else
  begin
   FRecordFlags:=(FRecordFlags and not ntfsFileRecordFlagIndexView);
  end;
end;

{==============================================================================}

function TNTFSDiskRecord.GetOrigin:TNTFSDiskRecord;
{Note: As attribute list must be in origin, Base (if not nil) will always be the origin record}
begin
 {}
 Result:=Self;
 
 {Check Base}
 if FBase <> nil then
  begin
   Result:=FBase;
   
   {Check Base}
   while Result.FBase <> nil do
    begin
     Result:=Result.FBase;
    end;
  end;
end;

{==============================================================================}

function TNTFSDiskRecord.CreateLinks(ANew:Boolean):TNTFSEntryList;
begin
 {}
 {Check Links}
 if FLinks = nil then
  begin
   {Create Links}
   FLinks:=TNTFSEntryList.Create;
   {Check New}
   if ANew then
    begin
     {Nothing}
    end;
  end;
  
 Result:=FLinks;
end;

{==============================================================================}

function TNTFSDiskRecord.CreateStreams(ANew:Boolean):TNTFSEntryList;
begin
 {}
 {Check Streams}
 if FStreams = nil then
  begin
   {Create Streams}
   FStreams:=TNTFSEntryList.Create;
   {Check New}
   if ANew then
    begin
     {Nothing}
    end;
  end;

 Result:=FStreams;
end;

{==============================================================================}

function TNTFSDiskRecord.CreateRecords(ANew:Boolean):TNTFSRecordList;
begin
 {}
 {Check Records}
 if FRecords = nil then
  begin
   {Create Records}
   FRecords:=TNTFSRecordList.Create;
   {Check New}
   if ANew then
    begin
     {Add Self if Origin}
     if FBase = nil then FRecords.Add(Self);
    end;
  end;

 Result:=FRecords;
end;

{==============================================================================}

function TNTFSDiskRecord.CreateAttributes(AVersion:Word;ANew:Boolean):TNTFSDiskAttributes;
var
 Attribute:TNTFSDiskAttribute;
begin
 {}
 {Check Attributes}
 if FAttributes = nil then
  begin
   {Create Attributes}
   FAttributes:=TNTFSDiskAttributes.Create(FAttributeLocal,FAttributesLock);
   
   {Check New}
   if ANew then
    begin
     Result:=nil;
     
     {Add End Attribute}
     Attribute:=CreateAttribute(ntfsAttrTypeEnd,AVersion,False);
     if Attribute = nil then Exit;
    end;
  end;

 Result:=FAttributes;
end;

{==============================================================================}

function TNTFSDiskRecord.CreateLink(AAttribute,AAlternate:TNTFSDiskAttribute;ANew:Boolean):TNTFSDiskEntry;
{Create a link, add to list do not update Record}
{Note: Request is propogated to base record to contain all links within one record}
var
 Link:TNTFSDiskEntry;
begin
 {}
 Result:=nil;
 
 if AAttribute = nil then Exit;
 
 {Check Base}
 if FBase <> nil then
  begin
   Result:=FBase.CreateLink(AAttribute,AAlternate,ANew);
  end
 else
  begin
   {Check Attribute}
   if AAttribute.AttributeType = ntfsAttrTypeFileName then
    begin
     if FLinks = nil then CreateLinks(ANew);
     
     {Create Link}
     Link:=TNTFSDiskEntry.Create(INVALID_HANDLE_VALUE,Self,AAttribute);
     Link.Alternate:=AAlternate;
     
     {Setup Link}
     if IsFolder then Link.Attributes:=(Link.Attributes or faDirectory);
     if not IsFolder then Link.Attributes:=(Link.Attributes or faFile);
     
     {Add Link}
     FLinks.Add(Link);
     
     Result:=Link;
    end;
  end;
end;

{==============================================================================}

function TNTFSDiskRecord.DestroyLink(ALink:TNTFSDiskEntry):Boolean;
{Remove the link from list and free but do not update record}
{Note: Request is propogated to base record to contain all links within one record}
begin
 {}
 Result:=False;
 
 if ALink = nil then Exit;
 
 {Check Base}
 if FBase <> nil then
  begin
   Result:=FBase.DestroyLink(ALink);
  end
 else
  begin
   if FLinks = nil then Exit;
   
   {Remove Link}
   FLinks.Remove(ALink);
   
   {Free Link}
   ALink.Free;
   
   Result:=True;
  end;
end;

{==============================================================================}

function TNTFSDiskRecord.NewLink(AAttribute,AAlternate:TNTFSDiskAttribute):TNTFSDiskEntry;
{Create a link, add to list and update Record}
{Note: Request is propogated to base record to contain all links within one record}
begin
 {}
 {Check Base}
 if FBase <> nil then
  begin
   Result:=FBase.NewLink(AAttribute,AAlternate);
  end
 else
  begin
   Result:=CreateLink(AAttribute,AAlternate,True);
   if Result = nil then Exit;

   {Update Record}
   Inc(FHardLinkCount);
   if AAlternate <> nil then Inc(FHardLinkCount);
  end;
end;

{==============================================================================}

function TNTFSDiskRecord.GetLink(AAttribute:TNTFSDiskAttribute):TNTFSDiskEntry;
{Note: Request is propogated to base record to contain all links within one record}
var
 Link:TNTFSDiskEntry;
begin
 {}
 Result:=nil;
 
 {Check Base}
 if FBase <> nil then
  begin
   Result:=FBase.GetLink(AAttribute);
  end
 else
  begin
   if FLinks = nil then Exit;
   if AAttribute = nil then Exit;
   
   {Check Links}
   Link:=TNTFSDiskEntry(FLinks.FirstEntry);
   while Link <> nil do
    begin
     if (Link.Attribute = AAttribute) or (Link.Alternate = AAttribute) then
      begin
       Result:=Link;
       Exit;
      end;
     
     Link:=TNTFSDiskEntry(Link.NextEntry);
    end;
  end;
end;

{==============================================================================}

function TNTFSDiskRecord.DeleteLink(ALink:TNTFSDiskEntry):Boolean;
{Remove the link from list and update record but do not free}
{Note: Request is propogated to base record to contain all links within one record}
begin
 {}
 Result:=False;
 
 if ALink = nil then Exit;
 
 {Check Base}
 if FBase <> nil then
  begin
   Result:=FBase.DeleteLink(ALink);
  end
 else
  begin
   if FLinks = nil then Exit;

   {Update Record}
   Dec(FHardLinkCount);
   if ALink.Alternate <> nil then Dec(FHardLinkCount);
   
   {Remove Link}
   Result:=FLinks.Remove(ALink);
  end;
end;

{==============================================================================}

function TNTFSDiskRecord.RenameLink(ALink:TNTFSDiskEntry;AAttribute,AAlternate:TNTFSDiskAttribute):Boolean;
{Update the link with new attribute and alternate and update Record}
{Note: Request is propogated to base record to contain all links within one record}
begin
 {}
 Result:=False;
 
 if ALink = nil then Exit;
 if AAttribute = nil then Exit;
 
 {Check Base}
 if FBase <> nil then
  begin
   Result:=FBase.RenameLink(ALink,AAttribute,AAlternate);
  end
 else
  begin
   {Check Attribute}
   if AAttribute.AttributeType = ntfsAttrTypeFileName then
    begin
     {Update Record}
     if AAlternate <> nil then
      begin
       if ALink.Alternate = nil then Inc(FHardLinkCount);
      end
     else
      begin
       if ALink.Alternate <> nil then Dec(FHardLinkCount);
      end;
     
     {Update Link}
     ALink.Alternate:=AAlternate;
     
     Result:=True;
    end;
  end;
end;

{==============================================================================}

function TNTFSDiskRecord.CreateStream(AAttribute:TNTFSDiskAttribute;ANew:Boolean):TNTFSDiskEntry;
{Create a stream, add to list do not update Record}
{Note: Request is propogated to base record to contain all streams within one record}
var
 Stream:TNTFSDiskEntry;
begin
 {}
 Result:=nil;
 
 if AAttribute = nil then Exit;
 
 {Check Base}
 if FBase <> nil then
  begin
   Result:=FBase.CreateStream(AAttribute,ANew);
  end
 else
  begin
   {Check Attribute}
   if AAttribute.AttributeType = ntfsAttrTypeData then
    begin
     if FStreams = nil then CreateStreams(ANew);
     
     {Create Stream}
     Stream:=TNTFSDiskEntry.Create(INVALID_HANDLE_VALUE,Self,AAttribute);
     
     {Setup Stream}
     Stream.Attributes:=(Stream.Attributes or faStream);
     
     {Add Stream}
     FStreams.Add(Stream);
     
     Result:=Stream;
    end;
  end;
end;

{==============================================================================}

function TNTFSDiskRecord.DestroyStream(AStream:TNTFSDiskEntry):Boolean;
{Remove the stream from list and free but do not update record}
{Note: Request is propogated to base record to contain all streams within one record}
begin
 {}
 Result:=False;
 
 if AStream = nil then Exit;
 
 {Check Base}
 if FBase <> nil then
  begin
   Result:=FBase.DestroyStream(AStream);
  end
 else
  begin
   if FStreams = nil then Exit;
   
   {Remove Stream}
   FStreams.Remove(AStream);
   
   {Free Stream}
   AStream.Free;
   
   Result:=True;
  end;
end;

{==============================================================================}

function TNTFSDiskRecord.NewStream(AAttribute:TNTFSDiskAttribute):TNTFSDiskEntry;
{Create a stream, add to list and update Record}
{Note: Request is propogated to base record to contain all streams within one record}
begin
 {}
 {Check Base}
 if FBase <> nil then
  begin
   Result:=FBase.NewStream(AAttribute);
  end
 else
  begin
   Result:=CreateStream(AAttribute,True);
   if Result = nil then Exit;
   
   {Nothing}
  end;
end;

{==============================================================================}

function TNTFSDiskRecord.GetStream(AAttribute:TNTFSDiskAttribute):TNTFSDiskEntry;
{Note: Request is propogated to base record to contain all streams within one record}
var
 Stream:TNTFSDiskEntry;
begin
 {}
 Result:=nil;
 {Check Base}
 if FBase <> nil then
  begin
   Result:=FBase.GetStream(AAttribute);
  end
 else
  begin
   if FStreams = nil then Exit;
   if AAttribute = nil then Exit;
   
   {Check Streams}
   Stream:=TNTFSDiskEntry(FStreams.FirstEntry);
   while Stream <> nil do
    begin
     if Stream.Attribute = AAttribute then
      begin
       Result:=Stream;
       Exit;
      end;
     
     Stream:=TNTFSDiskEntry(Stream.NextEntry);
    end;
  end;
end;

{==============================================================================}

function TNTFSDiskRecord.DeleteStream(AStream:TNTFSDiskEntry):Boolean;
{Remove the stream from list and update record but do not free}
{Note: Request is propogated to base record to contain all streams within one record}
begin
 {}
 Result:=False;
 
 {Check Base}
 if FBase <> nil then
  begin
   Result:=FBase.DeleteStream(AStream);
  end
 else
  begin
   if AStream = nil then Exit;
   if FStreams = nil then Exit;
   
   {Remove Stream}
   Result:=FStreams.Remove(AStream);
  end;
end;

{==============================================================================}

function TNTFSDiskRecord.GetFileNameByKey(AKey:TNTFSAttributeKey):TNTFSDiskAttribute;
{Get the attribute referenced by the supplied key (Checks base and extension records)}
var
 Name:String;
 Hash:LongWord;
 Instance:Integer;
 FileName:PNTFSFileName;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=nil;
 
 if AKey = nil then Exit;
 if AKey.Key = nil then Exit;
 if AKey.KeySize < ntfsFileNameSize then Exit;

 {Get Data}
 FileName:=PNTFSFileName(AKey.Key);
 
 {Get Name}
 Name:=NTFSWideBufferToString(@FileName.FileName[0],0,FileName.FileNameLength);
 
 {Calculate Hash}
 Hash:=GenerateNameHash(Name,NAME_HASH_SIZE);
 
 {Get Attribute}
 Instance:=1;
 Attribute:=GetAttribute(ntfsAttrTypeFileName,ntfsAnyName,Instance);
 while Attribute <> nil do
  begin
   {Check FileName}
   if TNTFSFileNameAttribute(Attribute).NameSpace = FileName.NameSpace then
    begin
     if TNTFSFileNameAttribute(Attribute).ParentReference = FileName.ParentReference then
      begin
       if TNTFSFileNameAttribute(Attribute).FileHash = Hash then
        begin
         if Uppercase(TNTFSFileNameAttribute(Attribute).FileName) = Uppercase(Name) then
          begin
           Result:=Attribute;
           Exit;
          end;
        end;
      end;
    end;
   
   {Get Attribute}
   Inc(Instance);
   Attribute:=GetAttribute(ntfsAttrTypeFileName,ntfsAnyName,Instance);
  end;
end;

{==============================================================================}

function TNTFSDiskRecord.GetFileNameByName(const AName:String;AInstance:Integer):TNTFSDiskAttribute;
{Get the attribute referenced by the supplied name (Checks base and extension records)}
var
 Hash:LongWord;
 Count:Integer;
 Instance:Integer;
 Wildcard:Boolean;
 Previous:TNTFSDiskAttribute;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=nil;
 
 Count:=0;
 Previous:=nil;
 
 {Calculate Hash}
 Hash:=0;                                                           //To Do //Testing4
 Wildcard:=(Length(AName) = ntfsAnyNameLength) and (AName = ntfsAnyName); {Modified 14/2/2011}
 if not Wildcard then Hash:=GenerateNameHash(AName,NAME_HASH_SIZE);  //To Do //Testing4
 
 {Get Attribute}
 Instance:=1;
 Attribute:=GetAttribute(ntfsAttrTypeFileName,ntfsAnyName,Instance);
 while Attribute <> nil do
  begin
   if Wildcard or (TNTFSFileNameAttribute(Attribute).FileHash = Hash) then
    begin
     if Wildcard or (Uppercase(TNTFSFileNameAttribute(Attribute).FileName) = Uppercase(AName)) then
      begin
       Inc(Count);
       if AInstance = ntfsInstanceLast then
        begin
         Previous:=Attribute;
        end
       else
        begin
         if (AInstance = ntfsInstanceFirst) or (Count = AInstance) then
          begin
           Result:=Attribute;
           Exit;
          end;
        end;
      end;
    end;
    
   {Get Attribute}
   Inc(Instance);
   Attribute:=GetAttribute(ntfsAttrTypeFileName,ntfsAnyName,Instance);
  end;
  
 {Get Last}
 if AInstance = ntfsInstanceLast then Result:=Previous;
end;

{==============================================================================}

function TNTFSDiskRecord.GetFileNameByParent(const AName:String;const AParent:Int64;AInstance:Integer):TNTFSDiskAttribute;
{Get the attribute referenced by the supplied name and parent (Checks base and extension records)}
var
 Hash:LongWord;
 Count:Integer;
 Instance:Integer;
 Wildcard:Boolean;
 Previous:TNTFSDiskAttribute;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=nil;
 
 Count:=0;
 Previous:=nil;
 
 {Calculate Hash}
 Hash:=0;                                                           //To Do //Testing4
 Wildcard:=(Length(AName) = ntfsAnyNameLength) and (AName = ntfsAnyName); {Modified 14/2/2011}
 if not Wildcard then Hash:=GenerateNameHash(AName,NAME_HASH_SIZE);  //To Do //Testing4
 
 {Get Attribute}
 Instance:=1;
 Attribute:=GetAttribute(ntfsAttrTypeFileName,ntfsAnyName,Instance);
 while Attribute <> nil do
  begin
   if TNTFSFileNameAttribute(Attribute).ParentReference = AParent then
    begin
     if Wildcard or (TNTFSFileNameAttribute(Attribute).FileHash = Hash) then
      begin
       if Wildcard or (Uppercase(TNTFSFileNameAttribute(Attribute).FileName) = Uppercase(AName)) then
        begin
         Inc(Count);
         if AInstance = ntfsInstanceLast then
          begin
           Previous:=Attribute;
          end
         else
          begin
           if (AInstance = ntfsInstanceFirst) or (Count = AInstance) then
            begin
             Result:=Attribute;
             Exit;
            end;
          end;
        end;
      end;
    end;
    
   {Get Attribute}
   Inc(Instance);
   Attribute:=GetAttribute(ntfsAttrTypeFileName,ntfsAnyName,Instance);
  end;
  
 {Get Last}
 if AInstance = ntfsInstanceLast then Result:=Previous;
end;

{==============================================================================}

function TNTFSDiskRecord.GetFileNameByNameSpace(const AName:String;const AParent:Int64;ANameSpace:Byte;AInstance:Integer):TNTFSDiskAttribute;
{Get the attribute referenced by the supplied name, parent and namespace (Checks base and extension records)}
var
 Hash:LongWord;
 Count:Integer;
 Instance:Integer;
 Wildcard:Boolean;
 Previous:TNTFSDiskAttribute;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=nil;
 
 Count:=0;
 Previous:=nil;
 
 {Calculate Hash}
 Hash:=0;                                                          //To Do //Testing4
 Wildcard:=(Length(AName) = ntfsAnyNameLength) and (AName = ntfsAnyName); {Modified 14/2/2011}
 if not Wildcard then Hash:=GenerateNameHash(AName,NAME_HASH_SIZE); //To Do //Testing4
 
 {Get Attribute}
 Instance:=1;
 Attribute:=GetAttribute(ntfsAttrTypeFileName,ntfsAnyName,Instance);
 while Attribute <> nil do
  begin
   if TNTFSFileNameAttribute(Attribute).NameSpace = ANameSpace then
    begin
     if TNTFSFileNameAttribute(Attribute).ParentReference = AParent then
      begin
       if Wildcard or (TNTFSFileNameAttribute(Attribute).FileHash = Hash) then
        begin
         if Wildcard or (Uppercase(TNTFSFileNameAttribute(Attribute).FileName) = Uppercase(AName)) then
          begin
           Inc(Count);
           if AInstance = ntfsInstanceLast then
            begin
             Previous:=Attribute;
            end
           else
            begin
             if (AInstance = ntfsInstanceFirst) or (Count = AInstance) then
              begin
               Result:=Attribute;
               Exit;
              end;
            end;
          end;
        end;
      end;
    end;
    
   {Get Attribute}
   Inc(Instance);
   Attribute:=GetAttribute(ntfsAttrTypeFileName,ntfsAnyName,Instance);
  end;
  
 {Get Last}
 if AInstance = ntfsInstanceLast then Result:=Previous;
end;

{==============================================================================}

function TNTFSDiskRecord.GetRecord(AInstance:Integer):TNTFSDiskRecord;
{Note: Request is propogated to base record to allow search of all records}
var
 Count:Integer;
 Current:TNTFSDiskRecord;
 Previous:TNTFSDiskRecord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=nil;
 
 Count:=0;
 Previous:=nil;
 
 {Check Base}
 if FBase <> nil then
  begin
   Result:=FBase.GetRecord(AInstance);
  end
 else
  begin
   {Check Records}
   if FRecords <> nil then
    begin
     {Get Record}
     Current:=FRecords.FirstRecord;
     while Current <> nil do
      begin
       Inc(Count);
       if AInstance = ntfsInstanceLast then
        begin
         Previous:=Current;
        end
       else
        begin
         if (AInstance = ntfsInstanceFirst) or (Count = AInstance) then  {Instance 0 equals first}
          begin
           Result:=Current;
           Exit;
          end;
        end;
       
       {Get Record}
       Current:=Current.NextRecord;
      end;
     
     {Get Last}
     if AInstance = ntfsInstanceLast then Result:=Previous;
    end
   else
    begin
     {Check List}
     if Overflow then
      begin
       {Get List}
       Attribute:=GetAttribute(ntfsAttrTypeAttributeList,ntfsBlankName,ntfsInstanceFirst);
       if Attribute = nil then Exit;
       
       {Get Attribute}
       Result:=Attribute.GetRecord(AInstance);
      end
     else
      begin
       {Get Self}
       Inc(Count);
       if (AInstance = ntfsInstanceLast) or (AInstance = ntfsInstanceFirst) or (Count = AInstance) then  {Instance 0 equals first}
        begin
         Result:=Self;
        end;
      end;
    end;
  end;
end;

{==============================================================================}

function TNTFSDiskRecord.GetRecordByFree(AFree:LongWord;AExclude:Boolean):TNTFSDiskRecord;
var
 Instance:Integer;
 Current:TNTFSDiskRecord;
begin
 {}
 Result:=nil;
 
 {Get Record}
 Instance:=1;
 Current:=GetRecord(Instance);
 while Current <> nil do
  begin
   if AExclude then
    begin
     if not(Current.Resizing) and not(Current.Removing) then
      begin
       {Check Free}
       if (Current.RecordAllocated - Current.RecordSize) >= AFree then
        begin
         Result:=Current;
         Exit;
        end;
      end;
    end
   else
    begin
     {Check Free}
     if (Current.RecordAllocated - Current.RecordSize) >= AFree then
      begin
       Result:=Current;
       Exit;
      end;
    end;
    
   {Get Record}
   Inc(Instance);
   Current:=GetRecord(Instance);
  end;
end;

{==============================================================================}

function TNTFSDiskRecord.GetRecordByReference(const AFileReference:Int64):TNTFSDiskRecord;
var
 Instance:Integer;
 Current:TNTFSDiskRecord;
begin
 {}
 Result:=nil;
 
 {Get Record}
 Instance:=1;
 Current:=GetRecord(Instance);
 while Current <> nil do
  begin
   {Check Reference}
   if Current.FileReference = AFileReference then
    begin
     Result:=Current;
     Exit;
    end;
    
   {Get Record}
   Inc(Instance);
   Current:=GetRecord(Instance);
  end;
end;

{==============================================================================}

function TNTFSDiskRecord.AddRecord(ARecord:TNTFSDiskRecord):Boolean;
begin
 {}
 Result:=False;
 
 if ARecord = nil then Exit;
 if FRecords = nil then CreateRecords(True);
 
 {Add Record}
 FRecords.Add(ARecord);
 
 Result:=True;
end;

{==============================================================================}

function TNTFSDiskRecord.RemoveRecord(ARecord:TNTFSDiskRecord):Boolean;
begin
 {}
 Result:=False;
 
 if ARecord = nil then Exit;
 if FRecords = nil then Exit;
 
 {Remove Record}
 FRecords.Remove(ARecord);
 
 Result:=True;
end;

{==============================================================================}

function TNTFSDiskRecord.GetAttributeByItem(AItem:TNTFSDiskItem):TNTFSDiskAttribute;
{Get the attribute referenced by the supplied item (Checks current record only as the match is only unique per record)}
var
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=nil;
 
 if AItem = nil then Exit;
 if FAttributes = nil then Exit;

 Attribute:=TNTFSDiskAttribute(FAttributes.First);
 while Attribute <> nil do
  begin
   if Attribute.AttributeType = AItem.AttributeType then
    begin
     if Attribute.AttributeId =  AItem.AttributeId then
      begin
       if Attribute.AttributeHash = AItem.AttributeHash then
        begin
         if Uppercase(Attribute.AttributeName) = Uppercase(AItem.AttributeName) then
          begin
           Result:=Attribute;
           Exit;
          end;
        end;
      end;
    end;
   
   Attribute:=TNTFSDiskAttribute(Attribute.Next);
  end;
end;

{==============================================================================}

function TNTFSDiskRecord.GetAttributeByStatus(AInclude,AExclude:Word;AInstance:Integer):TNTFSDiskAttribute;
{Get the next attribute whose status matches the supplied masks}
//var
 //Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=nil;
 
 {Get Attribute}
 //Attribute:=GetAttribute(ntfsAttrTypeAny //To Do //Not Needed ?
end;

{==============================================================================}

function TNTFSDiskRecord.GetAttributeByVCN(AAttribute:TNTFSDiskAttribute;const AVCN:Int64;var AInstance:LongWord):TNTFSDiskAttribute;
{Get the attribute containing the supplied vcn, passed attribute supplies the name and type}
{Instance can be passed and will form the starting point, otherwise start at one}
{Passed attribute can be any instance of a multiple instance attribute}
var
 Start:LongWord;
 Instance:LongWord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=nil;
 
 if AAttribute = nil then Exit;

 {Check Attribute}
 if (AAttribute.StartVCN <= AVCN) and (AAttribute.LastVCN >= AVCN) then
  begin
   Result:=AAttribute; {Instance is returned unchanged}
  end
 else
  begin
   {Get Start}
   Start:=AInstance;
   Instance:=AInstance;
   if AInstance = ntfsInstanceFirst then Instance:=1;
   {Get Attribute}
   Attribute:=GetAttribute(AAttribute.AttributeType,AAttribute.AttributeName,Instance);
   while Attribute <> nil do
    begin
     {Check Attribute}
     if (Attribute.StartVCN <= AVCN) and (Attribute.LastVCN >= AVCN) then
      begin
       AInstance:=Instance;
       Result:=Attribute;
       Exit;
      end;
      
     {Get Attribute}
     Inc(Instance);
     Attribute:=GetAttribute(AAttribute.AttributeType,AAttribute.AttributeName,Instance);
    end;
   
   {Check Start}
   if Start > 1 then
    begin
     Instance:=1;
     {Get Attribute}
     Attribute:=GetAttribute(AAttribute.AttributeType,AAttribute.AttributeName,Instance);
     while Attribute <> nil do
      begin
       {Check Attribute}
       if (Attribute.StartVCN <= AVCN) and (Attribute.LastVCN >= AVCN) then
        begin
         AInstance:=Instance;
         Result:=Attribute;
         Exit;
        end;
        
       {Get Attribute}
       if Instance = Start then Break;
       Inc(Instance);
       Attribute:=GetAttribute(AAttribute.AttributeType,AAttribute.AttributeName,Instance);
      end;
    end;
  end;
end;

{==============================================================================}

function TNTFSDiskRecord.GetAttributeByUnit(AAttribute:TNTFSDiskAttribute;const AUnit:Int64;var AInstance:LongWord):TNTFSDiskAttribute;
{Get the attribute containing the supplied compression unit, passed attribute supplies the name, type and unit size}
{Instance can be passed and will form the starting point, otherwise start at one}
{Passed attribute can be any instance of a multiple instance attribute}
var
 VCN:Int64;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=nil;
 
 if AAttribute = nil then Exit;

 {Check Resident}
 if AAttribute.NonResident = ntfsAttributeNonResident then
  begin
   {Get Attribute}
   Attribute:=GetAttribute(AAttribute.AttributeType,AAttribute.AttributeName,ntfsInstanceFirst);
   if Attribute = nil then Exit;
   
   {Check Compressed}
   if (Attribute.IsCompressed) and (Attribute.CompressionUnit <> 0) then
    begin
     {Get Unit VCN}
     VCN:=(AUnit shl Attribute.CompressionUnit);
     
     {Get Attribute by VCN}
     Result:=GetAttributeByVCN(AAttribute,VCN,AInstance);
    end;
  end;
end;

{==============================================================================}

function TNTFSDiskRecord.CreateAttribute(AType:LongWord;AVersion:Word;ANew:Boolean):TNTFSDiskAttribute;
{Create an attribute, add to end of list do not update record}
{Note: List of Attributes is per Record (Do not propogate request to base)}
var
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=nil;
 
 if FAttributes = nil then CreateAttributes(AVersion,ANew);
 
 {Check Type}
 Attribute:=nil;
 case AType of
  ntfsAttrTypeNone:begin
    {Nothing}
   end;
  ntfsAttrTypeStandardInformation:begin
    Attribute:=TNTFSStandardInformationAttribute.Create(FAttributeLocal,Self);
   end;
  ntfsAttrTypeAttributeList:begin
    Attribute:=TNTFSAttributeListAttribute.Create(FAttributeLocal,Self);
   end;
  ntfsAttrTypeFileName:begin
    Attribute:=TNTFSFileNameAttribute.Create(FAttributeLocal,Self);
   end;
  ntfsAttrTypeObjectId{,ntfsAttrTypeVolumeVersion}:begin
    case AVersion of
     ntfsNTFS12:Attribute:=TNTFSVolumeVersionAttribute.Create(FAttributeLocal,Self);
     ntfsNTFS30,ntfsNTFS31:Attribute:=TNTFSObjectIdAttribute.Create(FAttributeLocal,Self);
    end;
   end;
  ntfsAttrTypeSecurityDescriptor:begin
    Attribute:=TNTFSSecurityDescriptorAttribute.Create(FAttributeLocal,Self);
   end;
  ntfsAttrTypeVolumeName:begin
    Attribute:=TNTFSVolumeNameAttribute.Create(FAttributeLocal,Self);
   end;
  ntfsAttrTypeVolumeInformation:begin
    Attribute:=TNTFSVolumeInformationAttribute.Create(FAttributeLocal,Self);
   end;
  ntfsAttrTypeData:begin
    Attribute:=TNTFSDataAttribute.Create(FAttributeLocal,Self);
   end;
  ntfsAttrTypeIndexRoot:begin
    Attribute:=TNTFSIndexRootAttribute.Create(FAttributeLocal,Self);
   end;
  ntfsAttrTypeIndexAllocation:begin
    Attribute:=TNTFSIndexAllocationAttribute.Create(FAttributeLocal,Self);
   end;
  ntfsAttrTypeBitmap:begin
    Attribute:=TNTFSBitmapAttribute.Create(FAttributeLocal,Self);
   end;
  ntfsAttrTypeReparsePoint{,ntfsAttrTypeSymbolicLink}:begin
    case AVersion of
     ntfsNTFS12:Attribute:=TNTFSSymbolicLinkAttribute.Create(FAttributeLocal,Self);
     ntfsNTFS30,ntfsNTFS31:Attribute:=TNTFSReparsePointAttribute.Create(FAttributeLocal,Self);
    end;
   end;
  ntfsAttrTypeExtendedAttrInformation:begin
    Attribute:=TNTFSExtendedAttrInformationAttribute.Create(FAttributeLocal,Self);
   end;
  ntfsAttrTypeExtendedAttr:begin
    Attribute:=TNTFSExtendedAttrAttribute.Create(FAttributeLocal,Self);
   end;
  ntfsAttrTypePropertySet:begin
    case AVersion of
     ntfsNTFS12,ntfsNTFS31:Attribute:=TNTFSUnknownAttribute.Create(FAttributeLocal,Self);
     ntfsNTFS30:Attribute:=TNTFSPropertySetAttribute.Create(FAttributeLocal,Self);
    end;
   end;
  ntfsAttrTypeLoggedUtilityStream:begin
    Attribute:=TNTFSLoggedUtilityStreamAttribute.Create(FAttributeLocal,Self);
   end;
  ntfsAttrTypeEnd:begin
    Attribute:=TNTFSEndAttribute.Create(FAttributeLocal,Self);
   end;
  else
   begin
    Attribute:=TNTFSUnknownAttribute.Create(FAttributeLocal,Self);
    Attribute.FAttributeType:=AType;
   end;
 end;
 if Attribute = nil then Exit;

 {Add Attribute (Only if not new)}
 if not ANew then FAttributes.Add(Attribute);

 Result:=Attribute;
end;

{==============================================================================}

function TNTFSDiskRecord.DestroyAttribute(AAttribute:TNTFSDiskAttribute):Boolean;
{Remove the atttribute from the list and free, do not update record}
{Note: List of Attributes is per Record (Propogate request to parent if not self)}
begin
 {}
 Result:=False;
 
 if AAttribute = nil then Exit;
 if FAttributes = nil then Exit;

 {Check Parent}
 if AAttribute.Parent <> Self then
  begin
   Result:=AAttribute.Parent.DestroyAttribute(AAttribute);
  end
 else
  begin
   {Remove Attribute}
   FAttributes.Remove(AAttribute);
   
   {Free Attribute}
   AAttribute.Free;
   
   Result:=True;
  end;
end;

{==============================================================================}

function TNTFSDiskRecord.NewAttribute(APrevious:TNTFSDiskAttribute;AType:LongWord;const AName:String;AVersion:Word):TNTFSDiskAttribute;
{Create an attribute, insert in sorted list and update record}
{Note: List of Attributes is per Record (Do not propogate request to base)}
var
 Previous:TNTFSDiskAttribute;
begin
 {Check Records}
 if (FBase = nil) and (FRecords = nil) then CreateRecords(True);    {Add record to its own record list}
 
 {Create Attribute}
 Result:=CreateAttribute(AType,AVersion,True);
 if Result = nil then Exit;
 
 {Set Values}
 Result.AttributeId:=NextAttributeId;
 NextAttributeId:=NextAttributeId + 1;
 Result.AttributeName:=AName;
 if APrevious <> nil then
  begin
   Result.AttributeFlags:=APrevious.AttributeFlags;
   Result.NonResident:=APrevious.NonResident;
   Result.Indexed:=APrevious.Indexed;
   Result.StartVCN:=APrevious.LastVCN + 1;
  end;
  
 {Check Type}
 case AType of
  ntfsAttrTypeNone:begin
    {Nothing}
   end;
  ntfsAttrTypeStandardInformation:begin
    {Standard Information}
    TNTFSStandardInformationAttribute(Result).CreateTime:=Ultibo.DateTimeToFileTime(Now); {Converted to UTC}
    TNTFSStandardInformationAttribute(Result).WriteTime:=TNTFSStandardInformationAttribute(Result).CreateTime;
    TNTFSStandardInformationAttribute(Result).ChangeTime:=TNTFSStandardInformationAttribute(Result).CreateTime;
    TNTFSStandardInformationAttribute(Result).AccessTime:=TNTFSStandardInformationAttribute(Result).CreateTime;
   end;
  ntfsAttrTypeAttributeList:begin
    {Attribute List}
    {Nothing}
   end;
  ntfsAttrTypeFileName:begin
    {FileName}
    TNTFSFileNameAttribute(Result).CreateTime:=Ultibo.DateTimeToFileTime(Now); {Converted to UTC}
    TNTFSFileNameAttribute(Result).WriteTime:=TNTFSFileNameAttribute(Result).CreateTime;
    TNTFSFileNameAttribute(Result).ChangeTime:=TNTFSFileNameAttribute(Result).CreateTime;
    TNTFSFileNameAttribute(Result).AccessTime:=TNTFSFileNameAttribute(Result).CreateTime;
   end;
  ntfsAttrTypeObjectId{,ntfsAttrTypeVolumeVersion}:begin
    case AVersion of
     ntfsNTFS12:begin
       {Volume Version}
       {Nothing}
      end;
     ntfsNTFS30,ntfsNTFS31:begin
       {Object Id}
       TNTFSObjectIdAttribute(Result).ObjectId:=Ultibo.CreateGUID;
      end;
    end;
   end;
  ntfsAttrTypeSecurityDescriptor:begin
    {Security Descriptor}
    {Nothing}
   end;
  ntfsAttrTypeVolumeName:begin
    {Volume Name}
    {Nothing}
   end;
  ntfsAttrTypeVolumeInformation:begin
    {Volume Information}
    case AVersion of
     ntfsNTFS12:begin
       TNTFSVolumeInformationAttribute(Result).MajorVersion:=1;
       TNTFSVolumeInformationAttribute(Result).MinorVersion:=2;
      end;
     ntfsNTFS30:begin
       TNTFSVolumeInformationAttribute(Result).MajorVersion:=3;
       TNTFSVolumeInformationAttribute(Result).MinorVersion:=0;
      end;
     ntfsNTFS31:begin
       TNTFSVolumeInformationAttribute(Result).MajorVersion:=3;
       TNTFSVolumeInformationAttribute(Result).MinorVersion:=1;
      end;
    end;
   end;
  ntfsAttrTypeData:begin
    {Data}
    {Nothing}
   end;
  ntfsAttrTypeIndexRoot:begin
    {Index Root}
    {Nothing}
   end;
  ntfsAttrTypeIndexAllocation:begin
    {Index Allocation}
    {Nothing}
   end;
  ntfsAttrTypeBitmap:begin
    {Bitmap}
    {Nothing}
   end;
  ntfsAttrTypeReparsePoint{,ntfsAttrTypeSymbolicLink}:begin
    case AVersion of
     ntfsNTFS12:begin
       {Symbolic Link}
       {Nothing}
      end;
     ntfsNTFS30,ntfsNTFS31:begin
       {Reparse Point}
       {Nothing}
      end;
    end;
   end;
  ntfsAttrTypeExtendedAttrInformation:begin
    {Extended Attr Information}
    {Nothing}
   end;
  ntfsAttrTypeExtendedAttr:begin
    {Extended Attr}
    {Nothing}
   end;
  ntfsAttrTypePropertySet:begin
    case AVersion of
     ntfsNTFS12,ntfsNTFS31:begin
       {Nothing}
      end;
     ntfsNTFS30:begin
       {Property Set}
       {Nothing}
      end;
    end;
   end;
  ntfsAttrTypeLoggedUtilityStream:begin
    {Logged Utility Stream}
    {Nothing}
   end;
  ntfsAttrTypeEnd:begin
    {Nothing}
   end;
 end;
 
 {Get Previous}
 Previous:=FAttributes.GetPrevious(Result);
 
 {Insert Attribute}
 FAttributes.Insert(Previous,Result);
end;

{==============================================================================}

function TNTFSDiskRecord.GetAttribute(AType:LongWord;const AName:String;AInstance:Integer):TNTFSDiskAttribute;
{Note: Request is propogated to base record to allow search of all attributes}
var
 Hash:LongWord;
 Count:Integer;
 Wildcard:Boolean;
 Previous:TNTFSDiskAttribute;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=nil;
 
 //Count:=0;
 //Previous:=nil;
 
 //{Calculate Hash}
 //Hash:=0;                                                          //To Do //Testing4
 //Wildcard:=(Length(AName) = ntfsAnyNameLength) and (AName = ntfsAnyName); {Modified 14/2/2011}
 //if not Wildcard then Hash:=GenerateNameHash(AName,NAME_HASH_SIZE); //To Do //Testing4
 
 {Check Base}
 if FBase <> nil then
  begin
   Result:=FBase.GetAttribute(AType,AName,AInstance);
  end
 else
  begin
   {Check List}
   if Overflow and (AType <> ntfsAttrTypeAttributeList) then
    begin
     {Get List}
     Attribute:=GetAttribute(ntfsAttrTypeAttributeList,ntfsBlankName,ntfsInstanceFirst);
     if Attribute = nil then Exit;
     
     {Get Attribute}
     Result:=Attribute.GetAttribute(AType,AName,AInstance);
    end
   else
    begin
     //To Do //Testing
     Count:=0;
     Previous:=nil;
     
     {Calculate Hash}
     Hash:=0;                                                           //To Do //Testing4
     Wildcard:=(Length(AName) = ntfsAnyNameLength) and (AName = ntfsAnyName); {Modified 14/2/2011}
     if not Wildcard then Hash:=GenerateNameHash(AName,NAME_HASH_SIZE);  //To Do //Testing4
     //To Do //Testing

     {Check Attributes}
     if FAttributes <> nil then
      begin
       Attribute:=TNTFSDiskAttribute(FAttributes.First);
       while Attribute <> nil do
        begin
         {Check Type}
         if (AType = ntfsAttrTypeAny) or (Attribute.AttributeType = AType) then
         {if Attribute.AttributeType = AType then}
          begin
           if Wildcard or (Attribute.AttributeHash = Hash) then
            begin
             if Wildcard or (Uppercase(Attribute.AttributeName) = Uppercase(AName)) then //To Do //WorkBuffer ?
              begin
               Inc(Count);
               if AInstance = ntfsInstanceLast then
                begin
                 Previous:=Attribute;
                end
               else
                begin
                 if (AInstance = ntfsInstanceFirst) or (Count = AInstance) then  {Instance 0 equals first}
                  begin
                   Result:=Attribute;
                   Exit;
                  end;
                end;
              end;
            end;
          end;
          
         Attribute:=TNTFSDiskAttribute(Attribute.Next);
        end;
      end;
      
     {Get Last}
     if AInstance = ntfsInstanceLast then Result:=Previous;
    end;
  end;
end;

{==============================================================================}

function TNTFSDiskRecord.RemoveAttribute(AAttribute:TNTFSDiskAttribute):Boolean;
{Remove the attribute from the list, free and update record}
{Note: List of Attributes is per Record (Propogate request to parent if not self)}
begin
 {}
 Result:=False;
 
 if AAttribute = nil then Exit;
 if FAttributes = nil then Exit;

 {Check Parent}
 if AAttribute.Parent <> Self then
  begin
   Result:=AAttribute.Parent.RemoveAttribute(AAttribute);
  end
 else
  begin
   {Update Record}
    {Nothing}
   
   {Remove Attribute}
   FAttributes.Remove(AAttribute);
   
   {Free Attribute}
   AAttribute.Free;
   
   Result:=True;
  end;
end;

{==============================================================================}

function TNTFSDiskRecord.MoveAttribute(ADest:TNTFSDiskRecord;AAttribute:TNTFSDiskAttribute):Boolean;
{Remove the attribute from the list and insert in the list of the dest record}
{Note: List of Attributes is per Record (Propogate request to parent if not self)}
var
 Previous:TNTFSDiskAttribute;
begin
 {}
 Result:=False;
 
 if ADest = nil then Exit;
 if AAttribute = nil then Exit;
 if FAttributes = nil then Exit;
 if ADest.Attributes = nil then Exit; {Attibutes must have been created already (CreateAttribute/NewAttribute)}

 {Check Parent}
 if AAttribute.Parent <> Self then
  begin
   Result:=AAttribute.Parent.MoveAttribute(ADest,AAttribute);
  end
 else
  begin
   {Update Record}
    {Nothing}
   
   {Remove Attribute}
   FAttributes.Remove(AAttribute);
   
   {Update Attribute}
   AAttribute.Parent:=ADest;
   AAttribute.AttributeId:=ADest.NextAttributeId;
   ADest.NextAttributeId:=ADest.NextAttributeId + 1;
   
   {Get Previous}
   Previous:=ADest.Attributes.GetPrevious(AAttribute);
   
   {Insert Attribute}
   ADest.Attributes.Insert(Previous,AAttribute);
   
   Result:=True;
  end;
end;

{==============================================================================}

function TNTFSDiskRecord.RenameAttribute(AAttribute:TNTFSDiskAttribute;const AName:String):Boolean;
{Remove the attribute from the list and reinsert in the list with the new name}
{Note: List of Attributes is per Record (Propogate request to parent if not self)}
var
 Previous:TNTFSDiskAttribute;
begin
 {}
 Result:=False;
 
 if AAttribute = nil then Exit;
 if FAttributes = nil then Exit;

 {Check Parent}
 if AAttribute.Parent <> Self then
  begin
   Result:=AAttribute.Parent.RenameAttribute(AAttribute,AName);
  end
 else
  begin
   {Update Record}
    {Nothing}
   
   {Remove Attribute}
   FAttributes.Remove(AAttribute);
   
   {Update Attribute}
   AAttribute.AttributeName:=AName;
   
   {Get Previous}
   Previous:=FAttributes.GetPrevious(AAttribute);
   
   {Insert Attribute}
   FAttributes.Insert(Previous,AAttribute);
   
   Result:=True;
  end;
end;

{==============================================================================}

function TNTFSDiskRecord.FileReference:Int64;
{Build a File Reference from the RecordNumber and SequenceNumber}
begin
 {}
 TNTFSFileReference(Result).RecordNumber:=TNTFSFileReference(FRecordNumber).RecordNumber;
 TNTFSFileReference(Result).RecordSegment:=TNTFSFileReference(FRecordNumber).RecordSegment;
 TNTFSFileReference(Result).SequenceNumber:=FSequenceNumber;
end;

{==============================================================================}

function TNTFSDiskRecord.BaseReference:Int64;
{Returns zero when no base record is assigned (ie current record is the base)}
begin
 {}
 Result:=0;
 
 if FBase = nil then Exit;
 
 Result:=FBase.FileReference;
end;

{==============================================================================}

function TNTFSDiskRecord.LinkCount:LongWord;
begin
 {}
 Result:=0;
 
 if FLinks = nil then Exit;
 
 Result:=FLinks.EntryCount;
end;

{==============================================================================}

function TNTFSDiskRecord.StreamCount:LongWord;
begin
 {}
 Result:=0;
 
 if FStreams = nil then Exit;
 
 Result:=FStreams.EntryCount;
end;

{==============================================================================}

function TNTFSDiskRecord.RecordCount:LongWord;
begin
 {}
 Result:=0;
 
 if FRecords = nil then Exit;
 
 Result:=FRecords.RecordCount;
end;

{==============================================================================}

function TNTFSDiskRecord.AttributeCount:LongWord;
begin
 {}
 Result:=0;
 
 if FAttributes = nil then Exit;
 
 Result:=FAttributes.Count;
end;

{==============================================================================}

function TNTFSDiskRecord.RecordFree:LongWord;
{Determine the free space remaining in the record}
begin
 {}
 Result:=FRecordAllocated - FRecordSize;
end;

{==============================================================================}

function TNTFSDiskRecord.AttributeSize(AVersion:Word;AType:LongWord):LongWord;
{Determine the maximum size of this type of attribute in this record}
var
 Attribute:TNTFSDiskAttribute;
begin
 {Subtract Header}
 Result:=FRecordAllocated - FAttributeOffset;
 
 {Subtract StandardInformation}
 if (FBase = nil) and (AType <> ntfsAttrTypeStandardInformation) then
  begin
   Attribute:=GetAttribute(ntfsAttrTypeStandardInformation,ntfsBlankName,ntfsInstanceFirst);
   if Attribute <> nil then Dec(Result,Attribute.AttributeSize); {Use AttributeSize not CalculatedSize}
  end;
 
 {Subtract List}
 if (FBase = nil) and (Overflow) and (AType <> ntfsAttrTypeAttributeList) then
  begin
   Attribute:=GetAttribute(ntfsAttrTypeAttributeList,ntfsBlankName,ntfsInstanceFirst);
   if Attribute <> nil then Dec(Result,Attribute.AttributeSize); {Use AttributeSize not CalculatedSize}
  end;
 
 {Subtract End}
 if (AType <> ntfsAttrTypeEnd) then
  begin
   Attribute:=GetAttribute(ntfsAttrTypeEnd,ntfsBlankName,ntfsInstanceFirst);
   if Attribute <> nil then Dec(Result,Attribute.CalculatedSize(AVersion)); {Use CalculatedSize not AttributeSize for End}
  end;
end;

{==============================================================================}

function TNTFSDiskRecord.CalculatedSize(AVersion:Word):LongWord;
{Calculated Size includes any rounding required for alignment}
var
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=FAttributeOffset;
 
 if FAttributes = nil then Exit;
 
 Attribute:=TNTFSDiskAttribute(FAttributes.First);
 while Attribute <> nil do
  begin
   Inc(Result,Attribute.AttributeSize); {Use AttributeSize not CalculatedSize}
   
   Attribute:=TNTFSDiskAttribute(Attribute.Next);
  end;
 
 Result:=NTFSRoundLongWordTo8Bytes(Result);
end;

{==============================================================================}

function TNTFSDiskRecord.CalculatedOffset(AVersion:Word):Word;
{Determine the attribute offset for this record based on version}
begin
 {}
 Result:=0;
 
 case AVersion of
  ntfsNTFS12,ntfsNTFS30:Result:=NTFSRoundWordTo8Bytes(ntfsFileRecord12Size + (FUpdateSequenceLength shl 1)); {Multiply by SizeOf(Word)}
  ntfsNTFS31:Result:=NTFSRoundWordTo8Bytes(ntfsFileRecord31Size + (FUpdateSequenceLength shl 1)); {Multiply by SizeOf(Word)}
 end;
end;

{==============================================================================}

function TNTFSDiskRecord.CalculatedSequenceOffset(AVersion:Word):Word;
{Determine the update sequence offset for this record based on version}
begin
 {}
 Result:=0;
 
 case AVersion of
  ntfsNTFS12,ntfsNTFS30:Result:=ntfsFileRecord12Size;
  ntfsNTFS31:Result:=ntfsFileRecord31Size;
 end;
end;

{==============================================================================}

function TNTFSDiskRecord.CalculatedSequenceLength(ASectorSize:Word):Word;
{Determine the update sequence length for this record based on sector size}
{Sequence Length is the number of Words in the update sequence array}
var
 Size:LongWord;
begin
 {}
 Result:=0;
 
 if ASectorSize = 0 then Exit;

 Result:=1;
 Size:=0;
 while Size < FRecordAllocated do
  begin
   Inc(Result,1);
   Inc(Size,ASectorSize);
  end;
end;

{==============================================================================}

function TNTFSDiskRecord.CalculatedStreamUsed(AVersion:Word;AAttribute:TNTFSDiskAttribute):Int64;
{Calculated Stream Used is the actual allocated size of the data stream in a compressed attribute.
 This will be the same as the Data Size value for Resident but will be the total allocated runs
 (Non Sparse) for Non Resident}
{Note: Size Attribute will use this value to update Stream Used}
var
 Instance:LongWord;
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=0;
 
 if AAttribute = nil then Exit;

 {Get Attribute}
 Instance:=1;
 Attribute:=GetAttribute(AAttribute.AttributeType,AAttribute.AttributeName,Instance);
 while Attribute <> nil do
  begin
   Result:=Result + Attribute.CalculatedStreamUsed(AVersion);
   
   {Get Attribute}
   Inc(Instance);
   Attribute:=GetAttribute(AAttribute.AttributeType,AAttribute.AttributeName,Instance);
  end;
end;

{==============================================================================}

function TNTFSDiskRecord.ReadAttributes(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Read the record attributes from the supplied buffer at the supplied offset}
var
 Attribute:TNTFSDiskAttribute;
 AttributeHeader:PNTFSAttributeHeader;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskRecord.ReadAttributes - RecordNumber = ' + IntToHex(FRecordNumber,16));
 {$ENDIF}
 
 {Check Used}
 if (FRecordFlags and ntfsFileRecordFlagInUse) = ntfsFileRecordFlagInUse then
  begin
   {Check Size}
   while ASize >= ntfsAttributeTypeSize do
    begin
     {Get Header}
     AttributeHeader:=PNTFSAttributeHeader(LongWord(ABuffer) + AOffset);
     
     {Check None}
     if AttributeHeader.AttributeType = ntfsAttrTypeNone then Break;
     
     {Create Attribute}
     Attribute:=CreateAttribute(AttributeHeader.AttributeType,AVersion,False);
     if Attribute = nil then Exit;
     
     {Read Attribute}
     if not Attribute.ReadAttribute(ABuffer,AOffset,ASize,AVersion) then Exit;
     
     {Check Last}
     if AttributeHeader.AttributeType = ntfsAttrTypeEnd then Break;
    end;
  end
 else
  begin
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskRecord.ReadAttributes - Reading Free Record');
   {$ENDIF}
   
   {Check Size}
   if ASize >= ntfsEndSize then {SizeOf(LongWord)}
    begin
     {Create Attribute}
     Attribute:=CreateAttribute(ntfsAttrTypeEnd,AVersion,False);
     if Attribute = nil then Exit;
     
     {Read Attribute}
     {if not Attribute.ReadAttribute(ABuffer,AOffset,ASize,AVersion) then Exit;} {Read will overwrite the AttributeType value}
     
     {Update Offset}
     Dec(ASize,Attribute.AttributeSize);
     Inc(AOffset,Attribute.AttributeSize);
    end;
  end;
  
 Result:=True;
end;

{==============================================================================}

function TNTFSDiskRecord.WriteAttributes(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Write the record attributes to the supplied buffer at the supplied offset}
var
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskRecord.WriteAttributes - RecordNumber = ' + IntToHex(FRecordNumber,16));
 {$ENDIF}
 
 {Get Attribute}
 if FAttributes = nil then Exit;

 {Write Attributes}
 Attribute:=TNTFSDiskAttribute(FAttributes.First);
 while Attribute <> nil do
  begin
   {Check Size}
   if ASize < ntfsAttributeTypeSize then Exit;
  
   {Write Attribute}
   if not Attribute.WriteAttribute(ABuffer,AOffset,ASize,AVersion) then Exit;
  
   {Get Attribute}
   Attribute:=TNTFSDiskAttribute(Attribute.Next);
  end;
 
 Result:=True;
end;

{==============================================================================}

function TNTFSDiskRecord.ReadRecord(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word;AFree:Boolean):Boolean;
{Read the entry record from the supplied buffer at the supplied offset}

{A Free (but available) record will consist of just the USN in place of}
{the Magic Number and Fixup values at the end of each sector in the record}
var
 FileRecord12:PNTFS12FileRecord;
 FileRecord31:PNTFS31FileRecord;
 UpdateSequenceRecord:PNTFSUpdateSequenceRecord;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {Get Record}
 FileRecord12:=PNTFS12FileRecord(LongWord(ABuffer) + AOffset);
 
 {Check Magic}
 if FileRecord12.MagicNumber <> ntfsFileSignature then
  begin
   {Check Free}
   if not AFree then Exit;
   
   {Check Size}
   if ASize >= ntfsFileRecord12Size then
    begin
     {Clear Status}
     FStatus:=ntfsStatusNone;
     
     {Get Update}
     UpdateSequenceRecord:=PNTFSUpdateSequenceRecord(LongWord(ABuffer) + AOffset); { + FileRecord12.UpdateSequenceOffset}
     
     {Read Update}
     FUpdateSequenceNumber:=UpdateSequenceRecord.UpdateSequenceNumber;
     
     {Update Offset}
     Dec(ASize,ntfsFileRecord12Size);
     Inc(AOffset,ntfsFileRecord12Size);
     
     Result:=True;
    end;
  end
 else
  begin
   {Check Size}
   if ASize >= ntfsFileRecord12Size then
    begin
     {Clear Status}
     FStatus:=ntfsStatusNone;
    
     {Read Record}
     FRecordFlags:=FileRecord12.RecordFlags;
     FHardLinkCount:=FileRecord12.HardLinkCount;
     FSequenceNumber:=FileRecord12.SequenceNumber;
     FNextAttributeId:=FileRecord12.NextAttributeId;
     FRecordSize:=FileRecord12.RecordSize;
     FRecordAllocated:=FileRecord12.RecordAllocated;
     FAttributeOffset:=FileRecord12.AttributeOffset;
     FUpdateSequenceOffset:=FileRecord12.UpdateSequenceOffset;
     FUpdateSequenceLength:=FileRecord12.UpdateSequenceLength;
     FLogFileSequenceNumber:=FileRecord12.LogFileSequenceNumber;
    
     {Check Size}
     if FRecordSize < ntfsFileRecord12Size then Exit;
    
     {Check Version}
     case AVersion of
      ntfsNTFS31:begin
        {Get Record}
        FileRecord31:=PNTFS31FileRecord(LongWord(ABuffer) + AOffset);
       
        {Check Size}
        if ASize >= ntfsFileRecord31Size then
        begin
          if FileRecord31.UpdateSequenceOffset >= ntfsFileRecord31Size then {Account for Version 1.2 records on Version 3.1 filesystem}
           begin
            {Read Record}
            TNTFSFileReference(FRecordNumber).RecordNumber:=FileRecord31.RecordNumber;
            TNTFSFileReference(FRecordNumber).RecordSegment:=FileRecord31.RecordSegment;
            TNTFSFileReference(FRecordNumber).SequenceNumber:=0;
           end;
         end;
       end;
     end;
    
     {Get Update}
     UpdateSequenceRecord:=PNTFSUpdateSequenceRecord(LongWord(ABuffer) + AOffset + FileRecord12.UpdateSequenceOffset);
    
     {Read Update}
     FUpdateSequenceNumber:=UpdateSequenceRecord.UpdateSequenceNumber;
    
     {Update Offset}
     Dec(ASize,FileRecord12.AttributeOffset);
     Inc(AOffset,FileRecord12.AttributeOffset);
    
     Result:=True;
    end;
  end;
end;

{==============================================================================}

function TNTFSDiskRecord.WriteRecord(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Write the file record to the supplied buffer at the supplied offset}
var
 FileRecord12:PNTFS12FileRecord;
 FileRecord31:PNTFS31FileRecord;
 UpdateSequenceRecord:PNTFSUpdateSequenceRecord;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskRecord.WriteRecord - RecordNumber = ' + IntToHex(FRecordNumber,16));
 {$ENDIF}
 
 {Get Record}
 FileRecord12:=PNTFS12FileRecord(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if ASize >= ntfsFileRecord12Size then
  begin
   {Write Record}
   FileRecord12.MagicNumber:=ntfsFileSignature;
   FileRecord12.RecordFlags:=FRecordFlags;
   FileRecord12.HardLinkCount:=FHardLinkCount;
   FileRecord12.SequenceNumber:=FSequenceNumber;
   FileRecord12.NextAttributeId:=FNextAttributeId;
   FileRecord12.RecordSize:=FRecordSize;
   FileRecord12.RecordAllocated:=FRecordAllocated;
   FileRecord12.AttributeOffset:=FAttributeOffset;
   FileRecord12.UpdateSequenceOffset:=FUpdateSequenceOffset;
   FileRecord12.UpdateSequenceLength:=FUpdateSequenceLength;
   FileRecord12.LogFileSequenceNumber:=FLogFileSequenceNumber;
   FileRecord12.BaseReference:=BaseReference;
  
   {Check Version}
   case AVersion of
    ntfsNTFS31:begin
      {Get Record}
      FileRecord31:=PNTFS31FileRecord(LongWord(ABuffer) + AOffset);
     
      {Check Size}
      if ASize >= ntfsFileRecord31Size then
       begin
        if FUpdateSequenceOffset >= ntfsFileRecord31Size then {Account for Version 1.2 records on Version 3.1 filesystem}
         begin
          FileRecord31.RecordNumber:=TNTFSFileReference(FRecordNumber).RecordNumber;
          FileRecord31.RecordSegment:=TNTFSFileReference(FRecordNumber).RecordSegment;
         end;
       end;
     end;
   end;
  
   {Get Update}
   UpdateSequenceRecord:=PNTFSUpdateSequenceRecord(LongWord(ABuffer) + AOffset + FileRecord12.UpdateSequenceOffset);
  
   {Write Update}
   UpdateSequenceRecord.UpdateSequenceNumber:=FUpdateSequenceNumber;
  
   {Update Offset}
   Dec(ASize,FileRecord12.AttributeOffset);
   Inc(AOffset,FileRecord12.AttributeOffset);
  
   Result:=True;
  end;
end;

{==============================================================================}
{==============================================================================}
{TNTFSDiskIndex}
constructor TNTFSDiskIndex.Create(AKeyLocal:TMutexHandle;ALock:TSynchronizerHandle;AVolumeVersion,ASectorSize:Word;AIndexType,ACollateRule:LongWord;AAttribute:TNTFSDiskAttribute);
begin
 {}
 inherited Create;
 FLock:=ALock;
 FKeyLocal:=AKeyLocal;
 
 FSwapLeft:=True;
 
 FStatus:=ntfsStatusNone;

 FSectorSize:=ASectorSize;
 FVolumeVersion:=AVolumeVersion;

 FIndexType:=AIndexType;
 FCollateRule:=ACollateRule; {ntfsCollateTypeBinary;}
 FIndexRecordSize:=0;
 FIndexCounterOffset:=0;

 FClustersPerIndex:=0;
 FIndexsPerCluster:=0;

 FIndexCounterShift:=0;
 FIndexRecordShiftCount:=0;
 FIndexRecordOffsetMask:=0;

 FTotalIndexRecordCount:=0;
 FLastFreeIndexRecord:=ntfsUnknownRecordNumber;
 FFreeIndexRecordCount:=ntfsUnknownRecordNumber;

 FCompareSecurityDescriptor:=nil;
 
 FUpCase:=nil;

 FNodes:=nil;
 FAttribute:=AAttribute;

 case AIndexType of
  ntfsAttrTypeNone:begin
    case ACollateRule of
     ntfsCollateTypeBinary,ntfsCollateTypeFileName,ntfsCollateTypeUnicode,ntfsCollateTypeLongWord,
     ntfsCollateTypeSID,ntfsCollateTypeSecurityHash,ntfsCollateTypeGUID:begin
       Order:=100; {May need to be adjusted}
      end;
    end;
   end;
  ntfsAttrTypeFileName:begin
    Order:=38; {May need to be adjusted}
   end;
 end;
end;

{==============================================================================}

destructor TNTFSDiskIndex.Destroy;
begin
 {}
 WriterLock;
 try
  if FNodes <> nil then FNodes.Free;
  FAttribute:=nil;
  
  FKeyLocal:=INVALID_HANDLE_VALUE;
  inherited Destroy;
 finally
  WriterUnlock;
  FLock:=INVALID_HANDLE_VALUE;
 end;
end;

{==============================================================================}

function TNTFSDiskIndex.GetNodesLock:TSynchronizerHandle;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;
 
 if FAttribute = nil then Exit;
 if FAttribute.Parent = nil then Exit;
 
 Result:=FAttribute.Parent.FNodesLock;
end;

{==============================================================================}

function TNTFSDiskIndex.GetKeyLocal:TMutexHandle;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;
 
 if FAttribute = nil then Exit;
 if FAttribute.Parent = nil then Exit;
 
 Result:=FAttribute.Parent.FKeyLocal;
end;

{==============================================================================}

function TNTFSDiskIndex.GetNodeLocal:TMutexHandle;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;
 
 if FAttribute = nil then Exit;
 if FAttribute.Parent = nil then Exit;
 
 Result:=FAttribute.Parent.FNodeLocal;
end;

{==============================================================================}

function TNTFSDiskIndex.GetLoaded:Boolean;
begin
 {}
 Result:=((FStatus and ntfsStatusLoaded) = ntfsStatusLoaded);
end;

{==============================================================================}

procedure TNTFSDiskIndex.SetLoaded(AValue:Boolean);
begin
 {}
 if AValue then
  begin
   FStatus:=(FStatus or ntfsStatusLoaded);
  end
 else
  begin
   FStatus:=(FStatus and not ntfsStatusLoaded);
  end;
end;

{==============================================================================}

function TNTFSDiskIndex.GetChanged:Boolean;
var
 Node:TNTFSDiskNode;
begin
 {Check Index}
 Result:=False;
 
 if FNodes = nil then Exit;
 
 {Check Nodes}
 Node:=TNTFSDiskNode(FNodes.First);
 while Node <> nil do
  begin
   Result:=Node.Changed;
   if Result then Exit;
   
   Node:=TNTFSDiskNode(Node.Next);
  end;
end;

{==============================================================================}

function TNTFSDiskIndex.GetUpCase:PNTFSUpCaseData;
begin
 {}
 Result:=nil;
 
 if FUpCase = nil then
  begin
   Result:=NTFSGetUpCase;
  end
 else
  begin
   Result:=PNTFSUpCaseData(FUpCase.Data);
  end;
end;

{==============================================================================}

function TNTFSDiskIndex.GetRootNode:TNTFSDiskNode;
var
 Key:TNTFSDiskKey;
begin
 {}
 Result:=nil;
 
 Key:=TNTFSDiskKey(Root);
 if Key = nil then Exit;
 
 Result:=Key.Node;
end;

{==============================================================================}

function TNTFSDiskIndex.CreateNodes(ANew:Boolean):TNTFSDiskNodes;
begin
 {Check Nodes}
 if FNodes = nil then
  begin
   {Create Nodes}
   FNodes:=TNTFSDiskNodes.Create(GetNodeLocal,GetNodesLock);
   
   {Check New}
   if ANew then
    begin
     {Nothing}
    end;
  end;
  
 Result:=FNodes;
end;

{==============================================================================}

function TNTFSDiskIndex.CompareKey(AEntry1,AEntry2:Pointer;ASize1,ASize2:Word):Integer;
begin
 {}
 {$IFDEF NTFS_DEBUG}
 //To Do //Testing2 //if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.CompareKey - Size1 = ' + IntToStr(ASize1) + ' Size2 = ' + IntToStr(ASize2));
 {$ENDIF}
 Result:=ntfsCompareGreater; {Default to Greater to continue Find}
 
 if AEntry1 = nil then Exit;
 if AEntry2 = nil then Exit;
 
 case FCollateRule of
  ntfsCollateTypeBinary:begin
    Result:=CompareBinary(AEntry1,AEntry2,ASize1,ASize2);
   end;
  ntfsCollateTypeFileName:begin
    Result:=CompareFileName(PWideChar(AEntry1),PWideChar(AEntry2),ASize1,ASize2);
   end;
  ntfsCollateTypeUnicode:begin
    Result:=CompareUnicode(PWideChar(AEntry1),PWideChar(AEntry2),ASize1,ASize2);
   end;
  ntfsCollateTypeLongWord:begin
    Result:=CompareLongWord(LongWord(AEntry1^),LongWord(AEntry2^),ASize1,ASize2);
   end;
  ntfsCollateTypeSID:begin
    Result:=CompareSID(PSID(AEntry1),PSID(AEntry2),ASize1,ASize2);
   end;
  ntfsCollateTypeSecurityHash:begin
    Result:=CompareSecurityHash(PNTFSSecurityHashKeyData(AEntry1),PNTFSSecurityHashKeyData(AEntry2),ASize1,ASize2);
   end;
  ntfsCollateTypeGUID:begin
    Result:=CompareGUID(PGUID(AEntry1),PGUID(AEntry2),ASize1,ASize2);
   end;
 end;
end;

{==============================================================================}

function TNTFSDiskIndex.CompareBinary(AEntry1,AEntry2:Pointer;ASize1,ASize2:Word):Integer;
var
 Size:Word;
 Offset:LongWord;
begin
 {}
 Result:=ntfsCompareEqual;
 
 if (ASize1 = 0) and (ASize2 = 0) then Exit;
 
 {Check Bytes}
 Offset:=0;
 Size:=Min(ASize1,ASize2);
 while Offset < Size do
  begin
   {Check Less or Greater}
   if Byte(Pointer(LongWord(AEntry1) + Offset)^) < Byte(Pointer(LongWord(AEntry2) + Offset)^) then
    begin
     Result:=ntfsCompareLess;
     Exit;
    end
   else if Byte(Pointer(LongWord(AEntry1) + Offset)^) > Byte(Pointer(LongWord(AEntry2) + Offset)^) then
    begin
     Result:=ntfsCompareGreater;
     Exit;
    end;
   
   Inc(Offset);
  end;
  
 {Check Sizes} {All Bytes are equal}
 if ASize1 = ASize2 then
  begin
   Result:=ntfsCompareEqual;
  end
 else if ASize1 < ASize2 then
  begin
   Result:=ntfsCompareLess;
  end
 else if ASize1 > ASize2 then
  begin
   Result:=ntfsCompareGreater;
  end;
end;

{==============================================================================}

function TNTFSDiskIndex.CompareFileName(AEntry1,AEntry2:PWideChar;ASize1,ASize2:Word):Integer;
{Note: Current implementation is a simple ordinal value compare}
{      May need to use CompareString/CompareStringEx in future}
var
 Size:Word;
 Count:Word;
 Offset:LongWord;
 UpCaseData:PNTFSUpCaseData;
begin
 {}
 Result:=ntfsCompareEqual;
 
 if (ASize1 = 0) and (ASize2 = 0) then Exit;
 
 UpCaseData:=GetUpCase;
 if UpCaseData = nil then Exit;
 
 {Check Words}
 Offset:=0;
 Count:=0;
 Size:=Min(ASize1,ASize2); {Size is in Words (WideChars)}
 while Count < Size do
  begin
   {Check Less or Greater}
   if UpCaseData.Data[Word(Pointer(LongWord(AEntry1) + Offset)^)] < UpCaseData.Data[Word(Pointer(LongWord(AEntry2) + Offset)^)] then
    begin
     Result:=ntfsCompareLess;
     Exit;
    end
   else if UpCaseData.Data[Word(Pointer(LongWord(AEntry1) + Offset)^)] > UpCaseData.Data[Word(Pointer(LongWord(AEntry2) + Offset)^)] then
    begin
     Result:=ntfsCompareGreater;
     Exit;
    end;
    
   Inc(Count);
   Inc(Offset,2);
  end;
  
 {Check Sizes} {All Bytes are equal}
 if ASize1 = ASize2 then
  begin
   Result:=ntfsCompareEqual;
  end
 else if ASize1 < ASize2 then
  begin
   Result:=ntfsCompareLess;
  end
 else if ASize1 > ASize2 then
  begin
   Result:=ntfsCompareGreater;
  end;
end;

{==============================================================================}

function TNTFSDiskIndex.CompareUnicode(AEntry1,AEntry2:PWideChar;ASize1,ASize2:Word):Integer;
{Note: Current implementation is a simple ordinal value compare}
{      May need to use CompareString/CompareStringEx in future}
{      Documentation says Uppercase characters come first, no special accomodation is made for this}
{      The above is assumed at this point to be a case sensitive compare which is as implemented}
var
 Size:Word;
 Count:Word;
 Offset:LongWord;
begin
 {}
 Result:=ntfsCompareEqual;
 
 if (ASize1 = 0) and (ASize2 = 0) then Exit;
 
 {Check Words}
 Offset:=0;
 Count:=0;
 Size:=Min(ASize1,ASize2); {Size is in Words (WideChars)}
 while Count < Size do
  begin
   {Check Less or Greater}
   if Word(Pointer(LongWord(AEntry1) + Offset)^) < Word(Pointer(LongWord(AEntry2) + Offset)^) then
    begin
     Result:=ntfsCompareLess;
     Exit;
    end
   else if Word(Pointer(LongWord(AEntry1) + Offset)^) > Word(Pointer(LongWord(AEntry2) + Offset)^) then
    begin
     Result:=ntfsCompareGreater;
     Exit;
    end;
   
   Inc(Count);
   Inc(Offset,2);
  end;
  
 {Check Sizes} {All Bytes are equal}
 if ASize1 = ASize2 then
  begin
   Result:=ntfsCompareEqual;
  end
 else if ASize1 < ASize2 then
  begin
   Result:=ntfsCompareLess;
  end
 else if ASize1 > ASize2 then
  begin
   Result:=ntfsCompareGreater;
  end;
end;

{==============================================================================}

function TNTFSDiskIndex.CompareLongWord(AEntry1,AEntry2:LongWord;ASize1,ASize2:Word):Integer;
begin
 {}
 Result:=ntfsCompareGreater; {Default to Greater to continue Find}
 
 if ASize1 <> SizeOf(LongWord) then Exit;
 if ASize2 <> SizeOf(LongWord) then Exit;
 
 {Check Values}
 if AEntry1 = AEntry2 then
  begin
   Result:=ntfsCompareEqual;
  end
 else if AEntry1 < AEntry2 then
  begin
   Result:=ntfsCompareLess;
  end
 else if AEntry1 > AEntry2 then
  begin
   Result:=ntfsCompareGreater;
  end;
end;

{==============================================================================}

function TNTFSDiskIndex.CompareSID(AEntry1,AEntry2:PSID;ASize1,ASize2:Word):Integer;
var
 Size:Integer;
 Count:Integer;
 Offset:LongWord;
begin
 {}
 Result:=ntfsCompareGreater; {Default to Greater to continue Find}
 
 if ASize1 < (SizeOf(SID) - SizeOf(DWORD)) then Exit;
 if ASize2 < (SizeOf(SID) - SizeOf(DWORD)) then Exit;
 
 {Check Revision}
 if AEntry1.Revision < AEntry2.Revision then
  begin
   Result:=ntfsCompareLess;
   Exit;
  end
 else if AEntry1.Revision > AEntry2.Revision then
  begin
   Result:=ntfsCompareGreater;
   Exit;
  end;
  
 {Check Sub Authority Count}
 if AEntry1.SubAuthorityCount < AEntry2.SubAuthorityCount then
  begin
   Result:=ntfsCompareLess;
   Exit;
  end
 else if AEntry1.SubAuthorityCount > AEntry2.SubAuthorityCount then
  begin
   Result:=ntfsCompareGreater;
   Exit;
  end;
  
 {Check Identifier Authority}
 for Count:=0 to 5 do
  begin
   if AEntry1.IdentifierAuthority.Value[Count] < AEntry2.IdentifierAuthority.Value[Count] then
    begin
     Result:=ntfsCompareLess;
     Exit;
    end
   else if AEntry1.IdentifierAuthority.Value[Count] > AEntry2.IdentifierAuthority.Value[Count] then
    begin
     Result:=ntfsCompareGreater;
     Exit;
    end;
  end;
  
 {Check Sub Authorities}
 Offset:=0;
 Count:=0;
 Size:=Min(AEntry1.SubAuthorityCount,AEntry2.SubAuthorityCount);
 while Count < Size do
  begin
   if LongWord(Pointer(LongWord(@AEntry1.SubAuthority[0]) + Offset)^) < LongWord(Pointer(LongWord(@AEntry2.SubAuthority[0]) + Offset)^) then
    begin
     Result:=ntfsCompareLess;
     Exit;
    end
   else if LongWord(Pointer(LongWord(@AEntry1.SubAuthority[0]) + Offset)^) > LongWord(Pointer(LongWord(@AEntry2.SubAuthority[0]) + Offset)^) then
    begin
     Result:=ntfsCompareGreater;
     Exit;
    end;
    
   Inc(Count);
   Inc(Offset,SizeOf(DWORD));
  end;
  
 Result:=ntfsCompareEqual; {Default to Equal (Sub Authority Counts were previously checked)}
end;

{==============================================================================}

function TNTFSDiskIndex.CompareSecurityHash(AEntry1,AEntry2:PNTFSSecurityHashKeyData;ASize1,ASize2:Word):Integer;
begin
 {}
 Result:=ntfsCompareGreater; {Default to Greater to continue Find}
 
 if ASize1 < SizeOf(TNTFSSecurityHashKeyData) then Exit;
 if ASize2 < SizeOf(TNTFSSecurityHashKeyData) then Exit;
 
 {Check Security Hash}
 if AEntry1.SecurityHash < AEntry2.SecurityHash then
  begin
   Result:=ntfsCompareLess;
   Exit;
  end
 else if AEntry1.SecurityHash > AEntry2.SecurityHash then
  begin
   Result:=ntfsCompareGreater;
   Exit;
  end;
 
 {Check Security Id}
 if AEntry1.SecurityId < AEntry2.SecurityId then
  begin
   Result:=ntfsCompareLess;
   Exit;
  end
 else if AEntry1.SecurityId > AEntry2.SecurityId then
  begin
   Result:=ntfsCompareGreater;
   Exit;
  end;
 
 Result:=ntfsCompareEqual; {Default to Equal (Sizes are assumed the same)}
end;

{==============================================================================}

function TNTFSDiskIndex.CompareGUID(AEntry1,AEntry2:PGUID;ASize1,ASize2:Word):Integer;
var
 Count:Integer;
 Maximum:Integer;
begin
 {}
 Result:=ntfsCompareGreater; {Default to Greater to continue Find}
 
 if ASize1 < SizeOf(TNTFSReparseKeyData) then Exit; {Reparse Key is only 12 bytes not 16 bytes as per GUID}
 if ASize2 < SizeOf(TNTFSReparseKeyData) then Exit;
 
 {Check D1}
 if AEntry1.D1 < AEntry2.D1 then
  begin
   Result:=ntfsCompareLess;
   Exit;
  end
 else if AEntry1.D1 > AEntry2.D1 then
  begin
   Result:=ntfsCompareGreater;
   Exit;
  end;
 
 {Check D2}
 if AEntry1.D2 < AEntry2.D2 then
  begin
   Result:=ntfsCompareLess;
   Exit;
  end
 else if AEntry1.D2 > AEntry2.D2 then
  begin
   Result:=ntfsCompareGreater;
   Exit;
  end;
 
 {Check D3}
 if AEntry1.D3 < AEntry2.D3 then
  begin
   Result:=ntfsCompareLess;
   Exit;
  end
 else if AEntry1.D3 > AEntry2.D3 then
  begin
   Result:=ntfsCompareGreater;
   Exit;
  end;
 
 {Check D4}
 Maximum:=7;
 if ASize1 < SizeOf(TGUID) then Maximum:=3;
 if ASize2 < SizeOf(TGUID) then Maximum:=3;
 for Count:=0 to Maximum do
  begin
   if AEntry1.D4[Count] < AEntry2.D4[Count] then
    begin
     Result:=ntfsCompareLess;
     Exit;
    end
   else if AEntry1.D4[Count] > AEntry2.D4[Count] then
    begin
     Result:=ntfsCompareGreater;
     Exit;
    end;
  end;
  
 Result:=ntfsCompareEqual; {Default to Equal (Sizes are assumed the same)}
end;

{==============================================================================}

function TNTFSDiskIndex.GetEnd(AEntry:TBtreeObject):TBtreeObject;
begin
 {}
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetEnd');
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetEnd - Entry = ' + IntToHex(LongWord(AEntry),8));
 if AEntry <> nil then
  begin
   if TNTFSDiskKey(AEntry).Node <> nil then
    begin
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetEnd - Entry.Node = ' + IntToHex(TNTFSDiskKey(AEntry).Node.RecordNumber,16));
    end;
  end;
 {$ENDIF}
 
 Result:=inherited GetEnd(AEntry);
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetEnd - Result = ' + IntToHex(LongWord(Result),8));
 {$ENDIF}
end;

{==============================================================================}

function TNTFSDiskIndex.GetStart(AEntry:TBtreeObject):TBtreeObject;
begin
 {}
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetStart');
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetStart - Entry = ' + IntToHex(LongWord(AEntry),8));
 if AEntry <> nil then
  begin
   if TNTFSDiskKey(AEntry).Node <> nil then
    begin
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetStart - Entry.Node = ' + IntToHex(TNTFSDiskKey(AEntry).Node.RecordNumber,16));
    end;
  end;
 {$ENDIF}
 
 Result:=inherited GetStart(AEntry);
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetStart - Result = ' + IntToHex(LongWord(Result),8));
 {$ENDIF}
end;

{==============================================================================}

function TNTFSDiskIndex.GetBlank(AEntry:TBtreeObject):TBtreeObject;
begin
 {}
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetBlank');
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetBlank - Entry = ' + IntToHex(LongWord(AEntry),8));
 if AEntry <> nil then
  begin
   if TNTFSDiskKey(AEntry).Node <> nil then
    begin
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetBlank - Entry.Node = ' + IntToHex(TNTFSDiskKey(AEntry).Node.RecordNumber,16));
    end;
  end;
 {$ENDIF}
 
 Result:=inherited GetBlank(AEntry);
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetBlank - Result = ' + IntToHex(LongWord(Result),8));
 {$ENDIF}
end;

{==============================================================================}

function TNTFSDiskIndex.GetMedian(AEntry:TBtreeObject):TBtreeObject;
{Get the median entry in the node of the supplied entry}
{Note: Does not include the blank key}
{Note: The minimum key count is 3 including the blank}
var
 Count:LongWord;
 Total:LongWord;
 Offset:LongWord;
 Node:TNTFSDiskNode;
 Sibling:TNTFSDiskKey;
begin
 {}
 Result:=nil;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetMedian');
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetMedian - Entry = ' + IntToHex(LongWord(AEntry),8));
 if AEntry <> nil then
  begin
   if TNTFSDiskKey(AEntry).Node <> nil then
    begin
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetMedian - Entry.Node = ' + IntToHex(TNTFSDiskKey(AEntry).Node.RecordNumber,16));
    end;
  end;
 {$ENDIF}
 
 if AEntry = nil then Exit;
 
 {Get Node}
 Node:=TNTFSDiskKey(AEntry).Node;
 if Node = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetMedian - Entry.Node = ' + IntToHex(Node.RecordNumber,16));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetMedian - Entry.Node.KeyCount = ' + IntToStr(Node.KeyCount));
 {$ENDIF}
 
 {Get Total}
 Total:=Node.KeyCount;    {Node.KeyCount - 1} {Must be at least 3 keys including blank}
 if Total < 3 then Exit;  {Total < 2}
 
 {Get Offset}
 Offset:=(Total shr 1); {Divide by 2}
 if (Total mod 2) > 0 then Inc(Offset);
 
 {Get Median}
 Count:=0;
 Sibling:=TNTFSDiskKey(GetStart(AEntry));
 while Sibling <> nil do
  begin
   if not Sibling.Blank then Inc(Count);
   if Count = Offset then Break;
   Sibling:=TNTFSDiskKey(Sibling.Right);
  end;
 
 Result:=Sibling; {Sibling will only be not nil if Median was reached}
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetMedian - Total = ' + IntToStr(Total) + ' Offset = ' + IntToStr(Offset));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetMedian - Result = ' + IntToHex(LongWord(Result),8));
 {$ENDIF}
end;

{==============================================================================}
 //To Do //Remove ?
function TNTFSDiskIndex.GetDropTest(AEntry:TBtreeObject;var ALeft:Boolean):TBtreeObject;
{Get the neighbour with appropriate number of keys to drop}
{Always drop with the Righthand neighbour if available}

{Note: If both lefthand and righthand nodes are nil then GetDrop will return the entry itself}
{      to indicate success, the node should then be dropped without demoting the parent}

//To Do //Testing11

{Note: No account is made of whether the lefthand or righthand nodes have room for the demoted}
{      parent, the caller must check for require push or split on the neighbour after drop}
var
 Lefthand:TBtreeObject;
 Righthand:TBtreeObject;
begin
 {}
 Result:=nil;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop');
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop - Entry = ' + IntToHex(LongWord(AEntry),8));
 {$ENDIF}
 
 if AEntry = nil then Exit;
 
 {Get Left}
 ALeft:=False;
 
 {Get Lefthand/Righthand}
 Lefthand:=GetLefthand(AEntry);
 Righthand:=GetRighthand(AEntry);
 
 {Check Lefthand/Righthand}
 Result:=Lefthand; //Righthand; //To Do //Testing11
 if Result = nil then Result:=Righthand; //Lefthand; //To Do //Testing11
 
 {Return Entry}
 if Result = nil then Result:=AEntry;
 
 {Check Left}
 if Result = Lefthand then ALeft:=True;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop - Entry.Blank = ' + BoolToStr(AEntry.Blank));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop - Entry.Left = ' + IntToHex(LongWord(AEntry.Left),8));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop - Entry.Right = ' + IntToHex(LongWord(AEntry.Right),8));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop - Entry.Child = ' + IntToHex(LongWord(AEntry.Child),8));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop - Entry.Parent = ' + IntToHex(LongWord(AEntry.Parent),8));
 if TNTFSDiskKey(AEntry).Node <> nil then
  begin
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop - Entry.Node = ' + IntToHex(TNTFSDiskKey(AEntry).Node.RecordNumber,16));
  end;
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop');
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop - Lefthand = ' + IntToHex(LongWord(Lefthand),8));
 if Lefthand <> nil then
  begin
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop - Lefthand.Left = ' + IntToHex(LongWord(Lefthand.Left),8));
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop - Lefthand.Right = ' + IntToHex(LongWord(Lefthand.Right),8));
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop - Lefthand.Child = ' + IntToHex(LongWord(Lefthand.Child),8));
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop - Lefthand.Parent = ' + IntToHex(LongWord(Lefthand.Parent),8));
   if TNTFSDiskKey(Lefthand).Node <> nil then
    begin
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop - Lefthand.Node = ' + IntToHex(TNTFSDiskKey(Lefthand).Node.RecordNumber,16));
    end;
  end;
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop');
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop - Righthand = ' + IntToHex(LongWord(Righthand),8));
 if Righthand <> nil then
  begin
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop - Righthand.Left = ' + IntToHex(LongWord(Righthand.Left),8));
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop - Righthand.Right = ' + IntToHex(LongWord(Righthand.Right),8));
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop - Righthand.Child = ' + IntToHex(LongWord(Righthand.Child),8));
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop - Righthand.Parent = ' + IntToHex(LongWord(Righthand.Parent),8));
   if TNTFSDiskKey(Righthand).Node <> nil then
    begin
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop - Righthand.Node = ' + IntToHex(TNTFSDiskKey(Righthand).Node.RecordNumber,16));
    end;
  end;
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop');
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop - Result = ' + IntToHex(LongWord(Result),8) + ' Left = ' + BoolToStr(ALeft));
 {$ENDIF}
end;

{==============================================================================}

function TNTFSDiskIndex.GetDrop(AEntry:TBtreeObject;var ALeft:Boolean):TBtreeObject;
{Get the neighbour with appropriate number of keys to drop}
{Always drop with the Righthand neighbour if available}

{Note: If both lefthand and righthand nodes are nil then GetDrop will return the entry itself}
{      to indicate success, the node should then be dropped without demoting the parent}

{Note: No account is made of whether the lefthand or righthand nodes have room for the demoted}
{      parent, the caller must check for require push or split on the neighbour after drop}
var
 Lefthand:TBtreeObject;
 Righthand:TBtreeObject;
begin
 {}
 Result:=nil;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop');
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop - Entry = ' + IntToHex(LongWord(AEntry),8));
 {$ENDIF}
 
 if AEntry = nil then Exit;
 
 {Get Left}
 ALeft:=False;
 
 {Get Lefthand/Righthand}
 Lefthand:=GetLefthand(AEntry);
 Righthand:=GetRighthand(AEntry);
 
 {Check Lefthand/Righthand}
 Result:=Righthand;
 if Result = nil then Result:=Lefthand;
 
 {Return Entry}
 if Result = nil then Result:=AEntry;
 
 {Check Left}
 if Result = Lefthand then ALeft:=True;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop - Entry.Blank = ' + BoolToStr(AEntry.Blank));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop - Entry.Left = ' + IntToHex(LongWord(AEntry.Left),8));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop - Entry.Right = ' + IntToHex(LongWord(AEntry.Right),8));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop - Entry.Child = ' + IntToHex(LongWord(AEntry.Child),8));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop - Entry.Parent = ' + IntToHex(LongWord(AEntry.Parent),8));
 if TNTFSDiskKey(AEntry).Node <> nil then
  begin
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop - Entry.Node = ' + IntToHex(TNTFSDiskKey(AEntry).Node.RecordNumber,16));
  end;
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop');
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop - Lefthand = ' + IntToHex(LongWord(Lefthand),8));
 if Lefthand <> nil then
  begin
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop - Lefthand.Left = ' + IntToHex(LongWord(Lefthand.Left),8));
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop - Lefthand.Right = ' + IntToHex(LongWord(Lefthand.Right),8));
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop - Lefthand.Child = ' + IntToHex(LongWord(Lefthand.Child),8));
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop - Lefthand.Parent = ' + IntToHex(LongWord(Lefthand.Parent),8));
   if TNTFSDiskKey(Lefthand).Node <> nil then
    begin
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop - Lefthand.Node = ' + IntToHex(TNTFSDiskKey(Lefthand).Node.RecordNumber,16));
    end;
  end;
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop');
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop - Righthand = ' + IntToHex(LongWord(Righthand),8));
 if Righthand <> nil then
  begin
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop - Righthand.Left = ' + IntToHex(LongWord(Righthand.Left),8));
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop - Righthand.Right = ' + IntToHex(LongWord(Righthand.Right),8));
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop - Righthand.Child = ' + IntToHex(LongWord(Righthand.Child),8));
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop - Righthand.Parent = ' + IntToHex(LongWord(Righthand.Parent),8));
   if TNTFSDiskKey(Righthand).Node <> nil then
    begin
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop - Righthand.Node = ' + IntToHex(TNTFSDiskKey(Righthand).Node.RecordNumber,16));
    end;
  end;
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop');
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetDrop - Result = ' + IntToHex(LongWord(Result),8) + ' Left = ' + BoolToStr(ALeft));
 {$ENDIF}
end;

{==============================================================================}

function TNTFSDiskIndex.GetMerge(AEntry:TBtreeObject):TBtreeObject;
{Get the neighbour with appropriate number of keys to merge}
{Always merge with the Righthand neighbour if available}
begin
 {}
 Result:=nil;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetMerge');
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetMerge - Entry = ' + IntToHex(LongWord(AEntry),8));
 if AEntry <> nil then
  begin
   if TNTFSDiskKey(AEntry).Node <> nil then
    begin
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetMerge - Entry.Node = ' + IntToHex(TNTFSDiskKey(AEntry).Node.RecordNumber,16));
    end;
  end;
 {$ENDIF}
 
 if AEntry = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetMerge - Result = ' + IntToHex(LongWord(Result),8));
 {$ENDIF}
end;

{==============================================================================}

function TNTFSDiskIndex.GetBorrow(AEntry:TBtreeObject):TBtreeObject;
{Get the neighbour with sufficient keys to borrow one}
{Always borrow from the Righthand neighbour if available}
begin
 {}
 Result:=nil;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetBorrow');
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetBorrow - Entry = ' + IntToHex(LongWord(AEntry),8));
 if AEntry <> nil then
  begin
   if TNTFSDiskKey(AEntry).Node <> nil then
    begin
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetBorrow - Entry.Node = ' + IntToHex(TNTFSDiskKey(AEntry).Node.RecordNumber,16));
    end;
  end;
 {$ENDIF}
 
 if AEntry = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetBorrow - Result = ' + IntToHex(LongWord(Result),8));
 {$ENDIF}
end;

{==============================================================================}

function TNTFSDiskIndex.GetTarget(ADrop:TBtreeObject;ALeft:Boolean):TBtreeObject;
{Get the actual target within the neighbour that is appropriate to drop}

{Note: If drop has no child then GetTarget will return the neighbour itself}

{Note: No account is made of whether the leftmost or rightmost nodes have room for the demoted}
{      parent, the caller must check for require push or split on the target after drop}
begin
 {}
 Result:=ADrop;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetTarget');
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetTarget - Drop = ' + IntToHex(LongWord(ADrop),8) + ' Left = ' + BoolToStr(ALeft));
 {$ENDIF}
 
 if ADrop = nil then Exit;
 if ADrop.Child = nil then Exit;
 
 {Check Left}
 if not(ALeft) then
  begin
   {Drop with Right}
   {Get Leftmost of Drop node}
   Result:=GetLeftmost(ADrop);
   if Result = nil then Exit;
  end
 else
  begin
   {Drop with Left}
   Result:=GetRightmost(ADrop);
   if Result = nil then Exit;
   Result:=GetBlank(Result); {Rightmost returns the key left of blank}
   if Result = nil then Exit;
  end;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetTarget - Target.Blank = ' + BoolToStr(Result.Blank));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetTarget - Target.Left = ' + IntToHex(LongWord(Result.Left),8));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetTarget - Target.Right = ' + IntToHex(LongWord(Result.Right),8));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetTarget - Target.Child = ' + IntToHex(LongWord(Result.Child),8));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetTarget - Target.Parent = ' + IntToHex(LongWord(Result.Parent),8));
 if TNTFSDiskKey(Result).Node <> nil then
  begin
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.GetTarget - Target.Node = ' + IntToHex(TNTFSDiskKey(Result).Node.RecordNumber,16));
  end;
 {$ENDIF}
end;


{==============================================================================}

function TNTFSDiskIndex.PushNode(AEntry:TBtreeObject):Boolean;
{Called before a node is pushed following insert of an entry}
var
 Node:TNTFSDiskNode;
begin
 {}
 Result:=False;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.PushNode');
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.PushNode - Entry = ' + IntToHex(LongWord(AEntry),8));
 {$ENDIF}
 
 if AEntry = nil then Exit;
 
 
 {Get Node}
 Node:=TNTFSDiskKey(AEntry).Node;
 if Node = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.PushNode - Entry.Node = ' + IntToHex(Node.RecordNumber,16));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.PushNode - Entry.Node.KeyCount = ' + IntToStr(Node.KeyCount));
 {$ENDIF}
 
 {Update Node}
 Node.Changed:=True;
 
 Result:=True;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.PushNode - Result = ' + BoolToStr(Result));
 {$ENDIF}
end;

{==============================================================================}

function TNTFSDiskIndex.SplitNode(AEntry:TBtreeObject):Boolean;
{Called before a node is split following insert of an entry}
var
 Node:TNTFSDiskNode;
begin
 {}
 Result:=False;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.SplitNode');
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.SplitNode - Entry = ' + IntToHex(LongWord(AEntry),8));
 {$ENDIF}
 
 if AEntry = nil then Exit;
 
 {Get Node}
 Node:=TNTFSDiskKey(AEntry).Node;
 if Node = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.SplitNode - Entry.Node = ' + IntToHex(Node.RecordNumber,16));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.SplitNode - Entry.Node.KeyCount = ' + IntToStr(Node.KeyCount));
 {$ENDIF}
 
 {Update Node}
 Node.Changed:=True;
 
 Result:=True;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.SplitNode - Result = ' + BoolToStr(Result));
 {$ENDIF}
end;

{==============================================================================}

function TNTFSDiskIndex.DropNode(AEntry,ADrop,ATarget:TBtreeObject;ALeft:Boolean):Boolean;
{Called before a node is dropped following removal of an entry}
var
 Node:TNTFSDiskNode;
begin
 {}
 Result:=False;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.DropNode');
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.DropNode - Entry = ' + IntToHex(LongWord(AEntry),8) + ' Drop = ' + IntToHex(LongWord(ADrop),8) + ' Target = ' + IntToHex(LongWord(ATarget),8) + ' Left = ' + BoolToStr(ALeft));
 {$ENDIF}
 
 if AEntry = nil then Exit; {Drop may be nil} {Target may be nil}
 
 {Get Node}
 Node:=TNTFSDiskKey(AEntry).Node;
 if Node = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.DropNode - Entry.Node = ' + IntToHex(Node.RecordNumber,16));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.DropNode - Entry.Node.KeyCount = ' + IntToStr(Node.KeyCount));
 {$ENDIF}
 
 {Update Node}
 Node.Changed:=True;
 
 {Check Drop Node}
 if ADrop <> nil then
  begin
   {Get Drop Node}
   Node:=TNTFSDiskKey(ADrop).Node;
   if Node = nil then Exit;
   
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.DropNode - Drop.Node = ' + IntToHex(Node.RecordNumber,16));
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.DropNode - Drop.Node.KeyCount = ' + IntToStr(Node.KeyCount));
   {$ENDIF}
   
   {Update Drop Node}
   Node.Changed:=True;
  end;
  
 {Check Target Node}
 if ATarget <> nil then
  begin
   {Get Target Node}
   Node:=TNTFSDiskKey(ATarget).Node;
   if Node = nil then Exit;
   
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.DropNode - Target.Node = ' + IntToHex(Node.RecordNumber,16));
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.DropNode - Target.Node.KeyCount = ' + IntToStr(Node.KeyCount));
   {$ENDIF}
   
   {Update Target Node}
   Node.Changed:=True;
  end;
  
 Result:=True;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.DropNode - Result = ' + BoolToStr(Result));
 {$ENDIF}
end;

{==============================================================================}
 //To Do //Remove ?
function TNTFSDiskIndex.DropNodeOld(AEntry,ADrop:TBtreeObject;ALeft:Boolean):Boolean;
{Note: Changed to account for Target}

{Called before a node is dropped following removal of an entry}
var
 Node:TNTFSDiskNode;
begin
 {}
 Result:=False;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.DropNode');
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.DropNode - Entry = ' + IntToHex(LongWord(AEntry),8) + ' Drop = ' + IntToHex(LongWord(ADrop),8) + ' Left = ' + BoolToStr(ALeft));
 {$ENDIF}
 
 if AEntry = nil then Exit; {Drop may be nil}
 
 {Get Node}
 Node:=TNTFSDiskKey(AEntry).Node;
 if Node = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.DropNode - Entry.Node = ' + IntToHex(Node.RecordNumber,16));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.DropNode - Entry.Node.KeyCount = ' + IntToStr(Node.KeyCount));
 
 {$ENDIF}
 {Update Node}
 Node.Changed:=True;
 
 {Check Drop Node}
 if ADrop <> nil then
  begin
   {Get Drop Node}
   Node:=TNTFSDiskKey(ADrop).Node;
   if Node = nil then Exit;
   
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.DropNode - Drop.Node = ' + IntToHex(Node.RecordNumber,16));
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.DropNode - Drop.Node.KeyCount = ' + IntToStr(Node.KeyCount));
   {$ENDIF}
   
   {Update Drop Node}
   Node.Changed:=True;
  end;
  
 Result:=True;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.DropNode - Result = ' + BoolToStr(Result));
 {$ENDIF}
end;

{==============================================================================}

function TNTFSDiskIndex.MergeNode(AEntry,AMerge:TBtreeObject):Boolean;
{Called before a node is merged following removal of an entry}
var
 Node:TNTFSDiskNode;
begin
 {}
 Result:=False;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.MergeNode');
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.MergeNode - Entry = ' + IntToHex(LongWord(AEntry),8) + ' Merge = ' + IntToHex(LongWord(AMerge),8));
 {$ENDIF}
 
 if AEntry = nil then Exit;
 if AMerge = nil then Exit;
 
 {Get Node}
 Node:=TNTFSDiskKey(AEntry).Node;
 if Node = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.MergeNode - Entry.Node = ' + IntToHex(Node.RecordNumber,16));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.MergeNode - Entry.Node.KeyCount = ' + IntToStr(Node.KeyCount));
 {$ENDIF}
 
 {Update Node}
 Node.Changed:=True;
 
 {Get Merge Node}
 Node:=TNTFSDiskKey(AMerge).Node;
 if Node = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.MergeNode - Merge.Node = ' + IntToHex(Node.RecordNumber,16));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.MergeNode - Merge.Node.KeyCount = ' + IntToStr(Node.KeyCount));
 {$ENDIF}
 
 {Update Merge Node}
 Node.Changed:=True;
 
 Result:=True;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.MergeNode - Result = ' + BoolToStr(Result));
 {$ENDIF}
end;

{==============================================================================}

function TNTFSDiskIndex.BorrowEntry(AEntry,ABorrow:TBtreeObject):Boolean;
{Called before an entry is borrowed following removal of an entry}
begin
 {}
 Result:=False;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.BorrowEntry');
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.BorrowEntry - Entry = ' + IntToHex(LongWord(AEntry),8) + ' Borrow = ' + IntToHex(LongWord(ABorrow),8));
 {$ENDIF}
 
 if AEntry = nil then Exit;
 if ABorrow = nil then Exit;
 
 {Update Entry}
 TNTFSDiskKey(AEntry).Changed:=True;
 
 {Update Borrow}
 TNTFSDiskKey(ABorrow).Changed:=True;
 
 Result:=True;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.BorrowEntry - Result = ' + BoolToStr(Result));
 {$ENDIF}
end;

{==============================================================================}

function TNTFSDiskIndex.SwapEntry(AEntry,ASwap:TBtreeObject;ALeft:Boolean):Boolean;
{Called before an entry is swapped during a remove or borrow}
var
 Node:TNTFSDiskNode;
begin
 {}
 Result:=False;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.SwapEntry');
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.SwapEntry - Entry = ' + IntToHex(LongWord(AEntry),8) + ' Swap = ' + IntToHex(LongWord(ASwap),8) + ' Left = ' + BoolToStr(ALeft));
 {$ENDIF}
 
 if AEntry = nil then Exit;
 if ASwap = nil then Exit;

 {Update Entry}
 TNTFSDiskKey(AEntry).Changed:=True;
 
 {Get Node}
 Node:=TNTFSDiskKey(AEntry).Node;
 if Node = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.SwapEntry - Entry.Node = ' + IntToHex(Node.RecordNumber,16));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.SwapEntry - Entry.Node.KeyCount = ' + IntToStr(Node.KeyCount));
 {$ENDIF}
 
 {Update Node}
 Node.Changed:=True;

 {Update Swap}
 TNTFSDiskKey(ASwap).Changed:=True;
 
 {Get Swap Node}
 Node:=TNTFSDiskKey(ASwap).Node;
 if Node = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.SwapEntry - Swap.Node = ' + IntToHex(Node.RecordNumber,16));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.SwapEntry - Swap.Node.KeyCount = ' + IntToStr(Node.KeyCount));
 {$ENDIF}
 
 {Update Swap Node}
 Node.Changed:=True;

 //To Do //Testing2
 {Update Entry}
 TNTFSDiskKey(AEntry).Node:=nil;
 
 {Update Swap}
 TNTFSDiskKey(ASwap).Node:=nil;
 //To Do //Testing2

 //To Do //Testing
 //{Swap Node links}
 //{Get Node (From Entry)}
 //Node:=TNTFSDiskKey(AEntry).Node;
 //{Swap Node (From Swap to Entry}
 //TNTFSDiskKey(AEntry).Node:=TNTFSDiskKey(ASwap).Node;
 //{Swap Node (From Entry to Swap}
 //TNTFSDiskKey(ASwap).Node:=Node;
 //To Do //Testing

 Result:=True;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.SwapEntry - Entry = ' + IntToHex(LongWord(AEntry),8));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.SwapEntry - Entry.Blank = ' + BoolToStr(AEntry.Blank));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.SwapEntry - Entry.Left = ' + IntToHex(LongWord(AEntry.Left),8));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.SwapEntry - Entry.Right = ' + IntToHex(LongWord(AEntry.Right),8));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.SwapEntry - Entry.Child = ' + IntToHex(LongWord(AEntry.Child),8));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.SwapEntry - Entry.Parent = ' + IntToHex(LongWord(AEntry.Parent),8));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.SwapEntry');
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.SwapEntry - Swap = ' + IntToHex(LongWord(ASwap),8));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.SwapEntry - Swap.Blank = ' + BoolToStr(ASwap.Blank));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.SwapEntry - Swap.Left = ' + IntToHex(LongWord(ASwap.Left),8));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.SwapEntry - Swap.Right = ' + IntToHex(LongWord(ASwap.Right),8));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.SwapEntry - Swap.Child = ' + IntToHex(LongWord(ASwap.Child),8));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.SwapEntry - Swap.Parent = ' + IntToHex(LongWord(ASwap.Parent),8));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.SwapEntry');
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.SwapEntry - Result = ' + BoolToStr(Result));
 {$ENDIF}
end;

{==============================================================================}

function TNTFSDiskIndex.SetParentEntry(AEntry,AParent:TBtreeObject):Boolean;
{Called after an entry is reparented during a push, split, drop, merge, borrow or swap}
var
 Node:TNTFSDiskNode;
 Blank:TNTFSDiskKey;
 Parent:TNTFSDiskKey;
begin
 {}
 Result:=False;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.SetParentEntry');
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.SetParentEntry - Entry = ' + IntToHex(LongWord(AEntry),8) + ' Parent = ' + IntToHex(LongWord(AParent),8));
 {$ENDIF}
 
 if AEntry = nil then Exit;
 
 {Parent may be nil}
 
 {Update Entry}
 TNTFSDiskKey(AEntry).Changed:=True;
 
 {Check Node}
 if TNTFSDiskKey(AEntry).Node = nil then {Node may have been set to nil by Detach during some operations}
  begin
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.SetParentEntry - Entry Node is nil');
   {$ENDIF}
   
   {Get Blank}
   Blank:=TNTFSDiskKey(GetBlank(AEntry));
   if Blank = nil then Exit;
   
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.SetParentEntry - Blank = ' + IntToHex(LongWord(Blank),8));
   {$ENDIF}
   
   if Blank.Node = nil then Exit;
   
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.SetParentEntry - Blank.Node = ' + IntToHex(Blank.Node.RecordNumber,16));
   {$ENDIF}
   
   {Update Entry}
   TNTFSDiskKey(AEntry).Node:=Blank.Node;
  end;
  
 {Check Parent}
 if AParent <> nil then
  begin
   {Update Parent}
   TNTFSDiskKey(AParent).Changed:=True;
   TNTFSDiskKey(AParent).HasSubNode:=True;
   TNTFSDiskKey(AParent).EntrySize:=TNTFSDiskKey(AParent).CalculatedSize(FVolumeVersion);

   //To Do //Testing2
   {Check Node}
   if TNTFSDiskKey(AParent).Node = nil then {Node may have been set to nil by Detach during some operations}
    begin
     {$IFDEF NTFS_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.SetParentEntry - Parent Node is nil');
     {$ENDIF}
     
     {Get Blank}
     Blank:=TNTFSDiskKey(GetBlank(AParent));
     if Blank = nil then Exit;
     
     {$IFDEF NTFS_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.SetParentEntry - Blank = ' + IntToHex(LongWord(Blank),8));
     {$ENDIF}
     
     if AParent <> Blank then {During Split entry becomes a parent before Blank is created}
      begin
       if Blank.Node = nil then Exit;
       
       {$IFDEF NTFS_DEBUG}
       if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.SetParentEntry - Blank.Node = ' + IntToHex(Blank.Node.RecordNumber,16));
       {$ENDIF}
       
       {Update Parent}
       TNTFSDiskKey(AParent).Node:=Blank.Node;
      end;
    end;
   //To Do //Testing2

   {Get Parent}
   Parent:=TNTFSDiskKey(AParent.Parent);
   if Parent = nil then
    begin
     {Root Node}
     {$IFDEF NTFS_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.SetParentEntry -  Parent is the Root Node');
     {$ENDIF}
     
     {Get Node}
     Node:=TNTFSDiskKey(AParent).Node;
     if Node <> nil then {Node may be nil while some operations are in progress}
      begin
       {$IFDEF NTFS_DEBUG}
       if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.SetParentEntry - Parent.Node = ' + IntToHex(Node.RecordNumber,16));
       {$ENDIF}
       
       {Update Node}
       Node.IndexSize:=Node.CalculatedSize(FVolumeVersion);
       Node.IndexAllocated:=Node.CalculatedAllocated(FVolumeVersion);
      end;
      
     {Update Attribute}
     if Attribute = nil then Exit;
     
     Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
     Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);
    end
   else
    begin
     {Sub Node}
     {$IFDEF NTFS_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.SetParentEntry - Parent is a Sub Node');
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.SetParentEntry - Parent.Parent = ' + IntToHex(LongWord(Parent),8));
     {$ENDIF}
     
     {Get Node}
     Node:=TNTFSDiskKey(AParent).Node;
     if Node <> nil then {Node may be nil while some operations are in progress}
      begin
       {$IFDEF NTFS_DEBUG}
       if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.SetParentEntry - Parent.Node = ' + IntToHex(Node.RecordNumber,16));
       {$ENDIF}
       
       {Update Node}
       Node.IndexSize:=Node.CalculatedSize(FVolumeVersion);
      end;
      
     {Update Parent}
     Parent.Changed:=True;
     Parent.HasSubNode:=True;
     Parent.EntrySize:=Parent.CalculatedSize(FVolumeVersion);
     
     {Get Parent Node}
     Node:=Parent.Node;
     if Node <> nil then {Node may be nil while some operations are in progress}
      begin
       {$IFDEF NTFS_DEBUG}
       if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.SetParentEntry - Parent.Parent.Node = ' + IntToHex(Node.RecordNumber,16));
       {$ENDIF}
       
       {Update Parent Node}
       Node.Changed:=True;
       Node.HasSubNodes:=True;
       Node.IndexSize:=Node.CalculatedSize(FVolumeVersion);
      end;
    end;
  end;
  
 Result:=True;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.SetParentEntry - Result = ' + BoolToStr(Result));
 {$ENDIF}
end;

{==============================================================================}

function TNTFSDiskIndex.DeleteBlank(ABlank:TBtreeObject):Boolean;
{Delete a blank key when a node is removed (Merge/Drop)}
var
 Node:TNTFSDiskNode;
begin
 {}
 Result:=False;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.DeleteBlank');
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.DeleteBlank - Blank = ' + IntToHex(LongWord(ABlank),8));
 {$ENDIF}
 
 {Check Blank}
 if ABlank = nil then Exit;
 if not ABlank.Blank then Exit;
 
 {Get Node}
 Node:=TNTFSDiskKey(ABlank).Node;
 if Node = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.DeleteBlank - Node = ' + IntToHex(Node.RecordNumber,16));
 {$ENDIF}
 
 {Delete Node}
 if not DeleteNode(Node) then Exit;
 
 {Delete Blank}
 {ABlank.Free;} {Blank will be freed by Node on destruction}
 
 Result:=True;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.DeleteBlank - Result = ' + BoolToStr(Result));
 {$ENDIF}
end;

{==============================================================================}

function TNTFSDiskIndex.AttachBlank(ABlank:TBtreeObject):Boolean;
{Occurs immediately after a new Blank is Created and Attached}
var
 Node:TNTFSDiskNode;
 Parent:TNTFSDiskKey;
begin
 {}
 Result:=False;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.AttachBlank');
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.AttachBlank - Blank = ' + IntToHex(LongWord(ABlank),8));
 {$ENDIF}
 
 {Check Blank}
 if ABlank = nil then Exit;
 if not ABlank.Blank then Exit; {Cannot Attach Non Blank}
 
 {Get Parent}
 Parent:=TNTFSDiskKey(ABlank.Parent);
 if Parent = nil then
  begin
   {Root Node}
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.AttachBlank - Blank is the Root Node');
   {$ENDIF}
   
   {Get Node}
   Node:=TNTFSDiskKey(ABlank).Node;
   if Node = nil then Exit;
   
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.AttachBlank - Blank.Node = ' + IntToHex(Node.RecordNumber,16));
   {$ENDIF}
   
   {Update Node} {The order of these is highly important}
   Node.UpdateSequenceOffset:=Node.CalculatedSequenceOffset(FVolumeVersion);
   Node.UpdateSequenceLength:=Node.CalculatedSequenceLength(ntfsUpdateSequenceSize); {Previously FSectorSize however always 512} //To Do //Testing
   Node.EntryOffset:=Node.CalculatedOffset(FVolumeVersion);
   Node.IndexSize:=Node.CalculatedSize(FVolumeVersion);
   Node.IndexAllocated:=Node.CalculatedAllocated(FVolumeVersion);
   
   {Update Attribute}
   if Attribute = nil then Exit;
   
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);
  end
 else
  begin
   {Sub Node}
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.AttachBlank - Blank is a Sub Node');
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.AttachBlank - Blank.Parent = ' + IntToHex(LongWord(Parent),8));
   {$ENDIF}
   
   {Get Node}
   Node:=TNTFSDiskKey(ABlank).Node;
   if Node = nil then Exit;
   
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.AttachBlank - Parent.Node = ' + IntToHex(Node.RecordNumber,16));
   {$ENDIF}
   
   {Update Node} {The order of these is highly important}
   Node.IndexAllocated:=Node.CalculatedAllocated(FVolumeVersion);
   Node.UpdateSequenceOffset:=Node.CalculatedSequenceOffset(FVolumeVersion);
   Node.UpdateSequenceLength:=Node.CalculatedSequenceLength(ntfsUpdateSequenceSize); {Previously FSectorSize however always 512} //To Do //Testing
   Node.EntryOffset:=Node.CalculatedOffset(FVolumeVersion);
   Node.IndexSize:=Node.CalculatedSize(FVolumeVersion);
   
   {Update Parent}
   Parent.Changed:=True;
   Parent.HasSubNode:=True;
   Parent.EntrySize:=Parent.CalculatedSize(FVolumeVersion);
   
   {Get Parent Node}
   Node:=Parent.Node;
   if Node <> nil then {Node may be nil while some operations are in progress}
    begin
     {Update Parent Node}
     Node.Changed:=True;
     Node.HasSubNodes:=True;
     Node.IndexSize:=Node.CalculatedSize(FVolumeVersion);
    end;
  end;
  
 Result:=True;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.AttachBlank - Result = ' + BoolToStr(Result));
 {$ENDIF}
end;

{==============================================================================}

function TNTFSDiskIndex.DetachBlank(ABlank:TBtreeObject):Boolean;
{Occurs immediately before a Blank is Detached and Deleted}
var
 Node:TNTFSDiskNode;
 Parent:TNTFSDiskKey;
begin
 {}
 Result:=False;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.DetachBlank');
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDataIndex.DetachBlank - Blank = ' + IntToHex(LongWord(ABlank),8));
 {$ENDIF}
 
 {Check Blank}
 if ABlank = nil then Exit;
 if not ABlank.Blank then Exit; {Cannot Detach Non Blank}
 
 {Get Parent}
 Parent:=TNTFSDiskKey(ABlank.Parent);
 if Parent <> nil then
  begin
   {Sub Node}
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.DetachBlank - Blank is a Sub Node');
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.DetachBlank - Blank.Parent = ' + IntToHex(LongWord(Parent),8));
   {$ENDIF}
   
   {Update Parent}
   Parent.Changed:=True;
   
   {Get Parent Node}
   Node:=Parent.Node;
   if Node <> nil then {Node may be nil while some operations are in progress}
    begin
     {$IFDEF NTFS_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.DetachBlank - Parent.Node = ' + IntToHex(Node.RecordNumber,16));
     {$ENDIF}
     
     {Update Parent Node}
     Node.Changed:=True;
    end;
  end;
  
 Result:=True;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDataIndex.DetachBlank - Result = ' + BoolToStr(Result));
 {$ENDIF}
end;

{==============================================================================}

function TNTFSDiskIndex.AttachEntry(AEntry:TBtreeObject):Boolean;
{Occurs immediately after an Entry is Attached}
var
 Node:TNTFSDiskNode;
 Blank:TNTFSDiskKey;
begin
 {}
 Result:=False;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.AttachEntry');
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.AttachEntry - Entry = ' + IntToHex(LongWord(AEntry),8));
 {$ENDIF}
 
 {Check Entry}
 if AEntry = nil then Exit;
 if AEntry.Blank then Exit; {Cannot Attach Blank}
 
 {Get Blank}
 Blank:=TNTFSDiskKey(GetBlank(AEntry));
 if Blank = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.AttachEntry - Blank = ' + IntToHex(LongWord(Blank),8));
 {$ENDIF}
 
 {Get Node}
 Node:=Blank.Node;
 if Node = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.AttachEntry - Blank.Node = ' + IntToHex(Node.RecordNumber,16));
 {$ENDIF}
 
 {Update Entry}
 TNTFSDiskKey(AEntry).Changed:=True;
 TNTFSDiskKey(AEntry).Node:=Node;
 
 {Update Node}
 Node.Changed:=True;
 
 Result:=True;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.AttachEntry - Result = ' + BoolToStr(Result));
 {$ENDIF}
end;

{==============================================================================}

function TNTFSDiskIndex.DetachEntry(AEntry:TBtreeObject):Boolean;
{Occurs immediately before an Entry is Detached}
var
 Node:TNTFSDiskNode;
begin
 {}
 Result:=False;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.DetachEntry');
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.DetachEntry - Entry = ' + IntToHex(LongWord(AEntry),8));
 {$ENDIF}
 
 {Check Entry}
 if AEntry = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.DetachEntry - Entry.Blank = ' + BoolToStr(AEntry.Blank));
 {$ENDIF}
 
 if AEntry.Blank then Exit; {Cannot Detach Blank}
 
 {Get Node}
 Node:=TNTFSDiskKey(AEntry).Node;
 if Node = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.DetachEntry - Entry.Node = ' + IntToHex(Node.RecordNumber,16));
 {$ENDIF}
 
 {Update Entry}
 TNTFSDiskKey(AEntry).Changed:=True;
 TNTFSDiskKey(AEntry).Node:=nil;
 
 {Update Node}
 Node.Changed:=True;
 
 Result:=True;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.DetachEntry - Result = ' + BoolToStr(Result));
 {$ENDIF}
end;

{==============================================================================}

function TNTFSDiskIndex.RequirePush(AEntry:TBtreeObject):Boolean;
{Called after an entry is inserted to determine if a push is required}
{Entry is the key that was inserted}
{Or a key in a parent node of the node where the entry was inserted}

{Push requires a minimum of 2 keys including the blank}
{Push can only occur on the Root Node}
var
 Node:TNTFSDiskNode;
 Parent:TNTFSDiskKey;

 Current:TNTFSDiskRecord;
begin
 {}
 Result:=False;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.RequirePush');
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.RequirePush - Entry = ' + IntToHex(LongWord(AEntry),8));
 {$ENDIF}
 
 if AEntry = nil then Exit;
 
 {Get Parent}
 Parent:=TNTFSDiskKey(AEntry.Parent);
 if Parent = nil then
  begin
   {Root Node}
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.RequirePush - Entry is the Root Node');
   {$ENDIF}
   
   {Get Node}
   Node:=TNTFSDiskKey(AEntry).Node;
   if Node = nil then Exit;
   
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.RequirePush - Node = ' + IntToHex(Node.RecordNumber,16));
   {$ENDIF}
   
   {Update Node}
   Node.IndexSize:=Node.CalculatedSize(FVolumeVersion);
   Node.IndexAllocated:=Node.CalculatedAllocated(FVolumeVersion);
   
   {Update Attribute}
   if Attribute = nil then Exit;
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);
   
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.RequirePush - KeyCount = ' + IntToStr(Node.KeyCount));
   {$ENDIF}
   
   {Check Count}
   if Node.KeyCount < 2 then Exit; {Can push with only 1 key plus blank}
   
   {Get Record}
   Current:=Attribute.Parent;
   if Current = nil then Exit;
   
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.RequirePush - CalculatedSize = ' + IntToStr(Current.CalculatedSize(FVolumeVersion)) + ' RecordAllocated = ' + IntToStr(Current.RecordAllocated));
   {$ENDIF}
   
   {Check Size}
   if Current.CalculatedSize(FVolumeVersion) <= Current.RecordAllocated then Exit;
   Result:=True;
   
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.RequirePush - Result = ' + BoolToStr(Result));
   {$ENDIF}
  end;
end;

{==============================================================================}

function TNTFSDiskIndex.RequireSplit(AEntry:TBtreeObject):Boolean;
{Called after an entry is inserted to determine if a split is required}
{Entry is the key that was inserted}
{Or a key in a parent node of the node where the entry was inserted}

{Split requires a minimum of 3 keys including the blank}
var
 Node:TNTFSDiskNode;
 Parent:TNTFSDiskKey;

 Current:TNTFSDiskRecord;
begin
 {}
 Result:=False;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.RequireSplit');
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.RequireSplit - Entry = ' + IntToHex(LongWord(AEntry),8));
 {$ENDIF}
 
 if AEntry = nil then Exit;
 
 {Get Parent}
 Parent:=TNTFSDiskKey(AEntry.Parent);
 if Parent = nil then
  begin
   {Root Node}
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.RequireSplit - Entry is the Root Node');
   {$ENDIF}
   
   {Get Node}
   Node:=TNTFSDiskKey(AEntry).Node;
   if Node = nil then Exit;
   
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.RequireSplit - Node = ' + IntToHex(Node.RecordNumber,16));
   {$ENDIF}
   
   {Update Node}
   Node.IndexSize:=Node.CalculatedSize(FVolumeVersion);
   Node.IndexAllocated:=Node.CalculatedAllocated(FVolumeVersion);
   
   {Update Attribute}
   if Attribute = nil then Exit;
   Attribute.DataSize:=Attribute.CalculatedStreamSize(FVolumeVersion);
   Attribute.AttributeSize:=Attribute.CalculatedSize(FVolumeVersion);
   
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.RequireSplit - KeyCount = ' + IntToStr(Node.KeyCount));
   {$ENDIF}
   
   {Check Count}
   if Node.KeyCount < 3 then Exit;
   
   {Get Record}
   Current:=Attribute.Parent;
   if Current = nil then Exit;
   
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.RequireSplit - CalculatedSize = ' + IntToStr(Current.CalculatedSize(FVolumeVersion)) + ' RecordAllocated = ' + IntToStr(Current.RecordAllocated));
   {$ENDIF}
   
   {Check Size}
   if Current.CalculatedSize(FVolumeVersion) <= Current.RecordAllocated then Exit;
   
   Result:=True;
   
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.RequireSplit - Result = ' + BoolToStr(Result));
   {$ENDIF}
  end
 else
  begin
   {Sub Node}
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.RequireSplit - Entry is a Sub Node');
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.RequireSplit - Parent = ' + IntToHex(LongWord(Parent),8));
   {$ENDIF}
   
   {Get Node}
   Node:=TNTFSDiskKey(AEntry).Node;
   if Node = nil then Exit;
   
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.RequireSplit - Node = ' + IntToHex(Node.RecordNumber,16));
   {$ENDIF}
   
   {Update Node}
   Node.IndexSize:=Node.CalculatedSize(FVolumeVersion);
   
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.RequireSplit - KeyCount = ' + IntToStr(Node.KeyCount));
   {$ENDIF}
   
   {Check Count}
   if Node.KeyCount < 3 then Exit;
   
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.RequireSplit - IndexSize = ' + IntToStr(Node.IndexSize) + ' IndexAllocated = ' + IntToStr(Node.IndexAllocated));
   {$ENDIF}
   
   {Check Size}
   if Node.IndexSize <= Node.IndexAllocated then Exit;
   
   Result:=True;
   
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.RequireSplit - Result = ' + BoolToStr(Result));
   {$ENDIF}
  end;
end;

{==============================================================================}

function TNTFSDiskIndex.RequireDrop(AEntry:TBtreeObject):Boolean;
{Called after an entry is removed to determine if a drop is required}
{Entry is the start key of the node where the entry was removed}
{Or a key in a parent node of the node where the entry was removed}

{Drop requires a maximum of 1 key including the blank}
{Drop can only occur on a Sub Node}
var
 Node:TNTFSDiskNode;
 Parent:TNTFSDiskKey;
begin
 {}
 Result:=False;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.RequireDrop');
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.RequireDrop - Entry = ' + IntToHex(LongWord(AEntry),8));
 {$ENDIF}
 
 if AEntry = nil then Exit;
 
 {Get Parent}
 Parent:=TNTFSDiskKey(AEntry.Parent);
 if Parent <> nil then
  begin
   {Sub Node}
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.RequireDrop - Entry is a SubNode');
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.RequireDrop - Parent = ' + IntToHex(LongWord(Parent),8));
   {$ENDIF}
   
   {Get Node}
   Node:=TNTFSDiskKey(AEntry).Node;
   if Node = nil then Exit;
   
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.RequireDrop - Node = ' + IntToHex(Node.RecordNumber,16));
   {$ENDIF}
   
   {Update Node}
   Node.IndexSize:=Node.CalculatedSize(FVolumeVersion);
   
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.RequireDrop - KeyCount = ' + IntToStr(Node.KeyCount));
   {$ENDIF}
   
   {Check Count}
   if Node.KeyCount > 1 then Exit;
   
   Result:=True;
   
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.RequireDrop - Result = ' + BoolToStr(Result));
   {$ENDIF}
  end;
end;

{==============================================================================}

function TNTFSDiskIndex.RequireMerge(AEntry:TBtreeObject):Boolean;
{Called after an entry is removed to determine if a merge is required}
{Entry is the start key of the node where the entry was removed}
{Or a key in a parent node of the node where the entry was removed}
{Merge is never used in NTFS indexes}
begin
 {}
 Result:=False;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.RequireMerge');
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.RequireMerge - Entry = ' + IntToHex(LongWord(AEntry),8));
 {$ENDIF}
 
 if AEntry = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.RequireMerge - Result = ' + BoolToStr(Result));
 {$ENDIF}
end;

{==============================================================================}

function TNTFSDiskIndex.RequireBorrow(AEntry:TBtreeObject):Boolean;
{Called after an entry is removed to determine if a borrow is required}
{Entry is the start key of the node where the entry was removed}
{Or a key in a parent node of the node where the entry was removed}
{Borrow is never used in NTFS indexes}
begin
 {}
 Result:=False;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.RequireBorrow');
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.RequireBorrow - Entry = ' + IntToHex(LongWord(AEntry),8));
 {$ENDIF}
 
 if AEntry = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.RequireBorrow - Result = ' + BoolToStr(Result));
 {$ENDIF}
end;

{==============================================================================}

function TNTFSDiskIndex.NodeCount:Integer;
begin
 {}
 Result:=0;
 
 if FNodes = nil then Exit;
 
 Result:=FNodes.Count;
end;

{==============================================================================}

function TNTFSDiskIndex.CreateKey(ANode:TNTFSDiskNode;ABlank:Boolean):TNTFSDiskKey;
{Create a key in the supplied node do not insert in index or mark as added}
begin
 {Virtual Base}
 Result:=nil;
end;

{==============================================================================}

function TNTFSDiskIndex.DestroyKey(AKey:TNTFSDiskKey):Boolean;
{Free the key, do not remove from the index}
begin
 {}
 Result:=False;
 
 if AKey = nil then Exit;
 
 AKey.Free;
 
 Result:=True;
end;

{==============================================================================}

function TNTFSDiskIndex.NewKey(AKey,AData:Pointer;AKeySize,ADataSize:Word):TNTFSDiskKey;
{Create a key with the supplied data do not insert in index or mark as added}
begin
 {Virtual Base}
 Result:=CreateKey(nil,False);
 if Result = nil then Exit;
end;

{==============================================================================}

function TNTFSDiskIndex.NewKeyEx(AKey,AData,APadding:Pointer;AKeySize,ADataSize,APaddingSize:Word):TNTFSDiskKey;
{Create a key with the supplied data do not insert in index or mark as added}
begin
 {Virtual Base}
 Result:=CreateKey(nil,False);
 if Result = nil then Exit;
end;

{==============================================================================}

function TNTFSDiskIndex.AddKey(AParent,AKey:TNTFSDiskKey):Boolean;
{Add the key to the index (Blank allowed)}
begin
 {}
 Result:=False;
 
 if AKey = nil then Exit;
 {if AParent = nil then Exit;} {Parent may be nil}
 
 Result:=Add(AParent,AKey);
end;

{==============================================================================}

function TNTFSDiskIndex.InsertKey(AKey:TNTFSDiskKey):Boolean;
{Insert the key in the index (Blank not allowed)}
begin
 {}
 Result:=False;
 
 if AKey = nil then Exit;
 if AKey.Blank then Exit;
 
 Result:=Insert(AKey);
end;

{==============================================================================}

function TNTFSDiskIndex.RemoveKey(AKey:TNTFSDiskKey):Boolean;
{Remove the key from the index and free (Blank not allowed)}
begin
 {}
 Result:=False;
 
 if AKey = nil then Exit;
 if AKey.Blank then Exit;
 
 if not Remove(AKey) then Exit;
 
 AKey.Free;
 
 Result:=True;
end;

{==============================================================================}

function TNTFSDiskIndex.CreateNode(ANew:Boolean):TNTFSDiskNode;
{Create a node, add to list do not mark as added}
var
 Node:TNTFSDiskNode;
begin
 {}
 Result:=nil;
 
 if FNodes = nil then CreateNodes(ANew);
 
 {Create Node}
 Node:=TNTFSDiskNode.Create(GetNodeLocal,Self);
 
 {Add Node}
 FNodes.Add(Node);
 
 Result:=Node;
end;

{==============================================================================}

function TNTFSDiskIndex.DestroyNode(ANode:TNTFSDiskNode):Boolean;
{Remove the node from the list and free}
begin
 {}
 Result:=False;
 
 if ANode = nil then Exit;
 if FNodes = nil then Exit;
 
 {Remove Node}
 FNodes.Remove(ANode);
 
 {Free Node}
 ANode.Free;
 
 Result:=True;
end;

{==============================================================================}

function TNTFSDiskIndex.NewNode(const ARecordNumber:Int64):TNTFSDiskNode;
{Create a node, add to list and mark as added}
begin
 {}
 Result:=CreateNode(True);
 if Result = nil then Exit;
 
 {Setup Node}
 Result.Added:=True;
 Result.RecordNumber:=ARecordNumber;
end;

{==============================================================================}

function TNTFSDiskIndex.GetNode(const ARecordNumber:Int64):TNTFSDiskNode;
var
 Node:TNTFSDiskNode;
begin
 {}
 Result:=nil;
 
 if FNodes = nil then Exit;
 
 {Check Nodes}
 Node:=TNTFSDiskNode(FNodes.First);
 while Node <> nil do
  begin
   {Do not check Deleted, called by Release Index Record}
   if Node.RecordNumber = ARecordNumber then
    begin
     Result:=Node;
     Exit;
    end;
    
   Node:=TNTFSDiskNode(Node.Next);
  end;
end;

{==============================================================================}

function TNTFSDiskIndex.DeleteNode(ANode:TNTFSDiskNode):Boolean;
{Mark the node as deleted but do not free or remove}
begin
 {}
 Result:=False;
 
 if ANode = nil then Exit;
 if FNodes = nil then Exit;
 
 {Delete Node}
 ANode.Deleted:=True;
 
 Result:=True;
end;

{==============================================================================}

function TNTFSDiskIndex.RemoveNode(ANode:TNTFSDiskNode):Boolean;
{Remove the node from the list and free}
begin
 {}
 Result:=False;
 
 if ANode = nil then Exit;
 if FNodes = nil then Exit;
 
 {Remove Node}
 FNodes.Remove(ANode);
 
 {Free Node}
 ANode.Free;
 
 Result:=True;
end;

{==============================================================================}

function TNTFSDiskIndex.UpdateNodes(AVersion:Word):Boolean;
var
 Node:TNTFSDiskNode;
begin
 {}
 Result:=False;
 
 if FNodes = nil then Exit;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.UpdateNodes');
 {$ENDIF}
 
 {First pass for Node and Flags}
 Node:=TNTFSDiskNode(FNodes.First);
 while Node <> nil do
  begin
   {Update Keys}
   if not Node.UpdateKeys(AVersion) then Exit;
   
   Node:=TNTFSDiskNode(Node.Next);
  end;
  
 {Second pass for SubNodeNumber}
 Node:=TNTFSDiskNode(FNodes.First);
 while Node <> nil do
  begin
   {Update Keys}
   if not Node.UpdateKeys(AVersion) then Exit;
   
   {Check Root}
   if Node.IsRoot then
    begin
     {Root Node}
     {Update Node} {The order of these is highly important}
     Node.UpdateSequenceOffset:=Node.CalculatedSequenceOffset(FVolumeVersion);
     Node.UpdateSequenceLength:=Node.CalculatedSequenceLength(ntfsUpdateSequenceSize); {Previously FSectorSize however always 512} //To Do //Testing
     Node.EntryOffset:=Node.CalculatedOffset(FVolumeVersion);
     Node.IndexSize:=Node.CalculatedSize(FVolumeVersion);
     Node.IndexAllocated:=Node.CalculatedAllocated(FVolumeVersion);
     
     {$IFDEF NTFS_DEBUG}
     if Node.IndexSize > Node.IndexAllocated then
      begin
       if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - ERROR Node Size greater than Allocated');
       Exit;
      end;
     {$ENDIF}
    end
   else
    begin
     {Sub Node}
     {Update Node} {The order of these is highly important}
     Node.IndexAllocated:=Node.CalculatedAllocated(FVolumeVersion);
     Node.UpdateSequenceOffset:=Node.CalculatedSequenceOffset(FVolumeVersion);
     Node.UpdateSequenceLength:=Node.CalculatedSequenceLength(ntfsUpdateSequenceSize); {Previously FSectorSize however always 512} //To Do //Testing
     Node.EntryOffset:=Node.CalculatedOffset(FVolumeVersion);
     Node.IndexSize:=Node.CalculatedSize(FVolumeVersion);
     
     {$IFDEF NTFS_DEBUG}
     if Node.IndexSize > Node.IndexAllocated then
      begin
       if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - ERROR Node Size greater than Allocated');
       Exit;
      end;
     if Node.KeyCount < 2 then
      begin
       if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - ERROR Non Root Node with less than 2 Keys');
       Exit;
      end;
     {$ENDIF}
    end;
    
   Node:=TNTFSDiskNode(Node.Next);
  end;
  
 Result:=True;
end;

{==============================================================================}

function TNTFSDiskIndex.FindKey(AValue:Pointer;ASize:Word):TNTFSDiskKey;
begin
 {Virtual Base}
 Result:=nil;
end;

{==============================================================================}

function TNTFSDiskIndex.ReadRoot(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Read the resident index header and entries from the supplied buffer at the supplied offset}
var
 Node:TNTFSDiskNode;
begin
 {}
 Result:=False;
 
 if Loaded then Exit;
 if ABuffer = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.ReadRoot - Offset = ' + IntToStr(AOffset) + ' Size = ' + IntToStr(ASize));
 {$ENDIF}
 
 {Check Size}
 if ASize >= ntfsIndexHeaderSize then
  begin
   {Create Node}
   Node:=CreateNode(False);
   if Node = nil then Exit;
   
   {Read Header}
   if not Node.ReadHeader(ABuffer,AOffset,ASize,AVersion) then Exit;
   
   {Read Keys}
   if not ReadKeys(nil,Node,ABuffer,AOffset,ASize,AVersion) then Exit;
   
   Result:=True;
  end;
end;

{==============================================================================}

function TNTFSDiskIndex.WriteRoot(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Write the resident index header and entries to the supplied buffer at the supplied offset}
var
 Node:TNTFSDiskNode;
begin
 {}
 Result:=False;
 
 if not Loaded then Exit;
 if ABuffer = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.WriteRoot - Offset = ' + IntToStr(AOffset) + ' Size = ' + IntToStr(ASize));
 {$ENDIF}
 
 {Check Size}
 if ASize >= ntfsIndexHeaderSize then
  begin
   {Get Node}
   Node:=RootNode;
   if Node = nil then Exit;
   
   {Write Header}
   if not Node.WriteHeader(ABuffer,AOffset,ASize,AVersion) then Exit;
   
   {Write Keys}
   if not WriteKeys(nil,Node,ABuffer,AOffset,ASize,AVersion) then Exit;
   
   Result:=True;
  end;
end;

{==============================================================================}

function TNTFSDiskIndex.ReadKeys(AParent:TNTFSDiskKey;ANode:TNTFSDiskNode;ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Read the index entries from the supplied buffer at the supplied offset}
var
 Key:TNTFSDiskKey;
 IndexEntry:PNTFSIndexEntry;
begin
 {}
 Result:=False;
 
 if Loaded then Exit;
 if ABuffer = nil then Exit;
 if ANode = nil then Exit;
 {if AParent = nil then Exit;} {Parent may be nil}
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.ReadKeys - Offset = ' + IntToStr(AOffset) + ' Size = ' + IntToStr(ASize) + ' RecordNumber = ' + IntToHex(ANode.FRecordNumber,16));
 {$ENDIF}
 
 {Check Size}
 while ASize >= ntfsIndexEntrySize do
  begin
   {Get Entry}
   IndexEntry:=PNTFSIndexEntry(LongWord(ABuffer) + AOffset);
   
   {Create Key}
   Key:=CreateKey(ANode,((IndexEntry.EntryFlags and ntfsIndexEntryFlagLastNode) = ntfsIndexEntryFlagLastNode));
   if Key = nil then Exit;
   
   {Read Key}
   if not Key.ReadKey(ABuffer,AOffset,ASize,AVersion) then Exit;
   
   {Add Key}
   if not AddKey(AParent,Key) then Exit;
   
   {Check Last}
   if ((IndexEntry.EntryFlags and ntfsIndexEntryFlagLastNode) = ntfsIndexEntryFlagLastNode) then Break;
  end;
  
 Result:=True;
end;

{==============================================================================}

function TNTFSDiskIndex.WriteKeys(AParent:TNTFSDiskKey;ANode:TNTFSDiskNode;ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Write the index entries to the supplied buffer at the supplied offset}
var
 Key:TNTFSDiskKey;
begin
 {}
 Result:=False;
 
 if not Loaded then Exit;
 if ABuffer = nil then Exit;
 if ANode = nil then Exit;
 {if AParent = nil then Exit;} {Parent may be nil}
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskIndex.WriteKeys - Offset = ' + IntToStr(AOffset) + ' Size = ' + IntToStr(ASize) + ' RecordNumber = ' + IntToHex(ANode.FRecordNumber,16));
 {$ENDIF}
 
 {Get Key}
 Key:=ANode.Start;
 
 {Write Keys}
 while Key <> nil do
  begin
   {Check Size}
   if ASize < ntfsIndexEntrySize then Exit;
   
   {Write Key}
   if not Key.WriteKey(ABuffer,AOffset,ASize,AVersion) then Exit;
   
   {Get Key}
   Key:=TNTFSDiskKey(Key.Right);
  end;
  
 Result:=True;
end;

{==============================================================================}

function TNTFSDiskIndex.ReaderLock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSDiskIndex.ReaderUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSDiskIndex.WriterLock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSDiskIndex.WriterUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}
{==============================================================================}
{TNTFSDataIndex}
function TNTFSDataIndex.CompareSecurityDescriptor(AHash1,AHash2,ASecurityId:LongWord;ADescriptor:Pointer;ASize:Word):Integer;
begin
 {}
 Result:=ntfsCompareGreater; {Default to Greater to continue Find}
 
 {Check Security Hash}
 if AHash1 < AHash2 then
  begin
   Result:=ntfsCompareLess;
   Exit;
  end
 else if AHash1 > AHash2 then
  begin
   Result:=ntfsCompareGreater;
   Exit;
  end;
  
 {Check Security Descriptor}
 if Assigned(FCompareSecurityDescriptor) then
  begin
   if FCompareSecurityDescriptor(ASecurityId,ADescriptor,ASize) then
    begin
     Result:=ntfsCompareEqual;
    end;
  end;
end;

{==============================================================================}

function TNTFSDataIndex.Find(AValue:Pointer;ASize:Word;ACurrent:TNTFSDataKey):TNTFSDataKey;
var
 Value:Integer;
begin
 {}
 Result:=nil;
 
 if ACurrent = nil then Exit;
 
 if ACurrent.Blank then
  begin
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDataIndex.Find - Blank');
   {$ENDIF}
   
   Result:=Find(AValue,ASize,TNTFSDataKey(ACurrent.Child));
  end
 else
  begin
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDataIndex.Find - Current = ' + IntToHex(LongWord(ACurrent),8));
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDataIndex.Find - Current.Child = ' + IntToHex(LongWord(ACurrent.Child),8));
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDataIndex.Find - Current.Parent = ' + IntToHex(LongWord(ACurrent.Parent),8));
   if ACurrent.Node <> nil then
    begin
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDataIndex.Find - Current.Node = ' + IntToHex(ACurrent.Node.RecordNumber,16));
    end;
   {$ENDIF}
   
   Value:=CompareKey(AValue,ACurrent.Key,ASize,ACurrent.KeySize);
   if Value = ntfsCompareEqual then
    begin
     {$IFDEF NTFS_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDataIndex.Find - Equal');
     {$ENDIF}
     
     Result:=ACurrent;
    end
   else if Value = ntfsCompareLess then
    begin
     {$IFDEF NTFS_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDataIndex.Find - Less');
     {$ENDIF}
     
     Result:=Find(AValue,ASize,TNTFSDataKey(ACurrent.Child));
    end
   else if Value = ntfsCompareGreater then
    begin
     {$IFDEF NTFS_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDataIndex.Find - Greater');
     {$ENDIF}
     
     Result:=Find(AValue,ASize,TNTFSDataKey(ACurrent.Right));
    end;
  end;
end;

{==============================================================================}

function TNTFSDataIndex.FindEx(AValue:Pointer;ASize:Word;AHash:LongWord;ACurrent:TNTFSDataKey):TNTFSDataKey;
var
 Value:Integer;
begin
 {}
 Result:=nil;
 
 if ACurrent = nil then Exit;
 
 if ACurrent.Blank then
  begin
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDataIndex.FindEx - Blank');
   {$ENDIF}
   
   Result:=FindEx(AValue,ASize,AHash,TNTFSDataKey(ACurrent.Child));
  end
 else
  begin
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDataIndex.FindEx - Current = ' + IntToHex(LongWord(ACurrent),8));
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDataIndex.FindEx - Current.Child = ' + IntToHex(LongWord(ACurrent.Child),8));
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDataIndex.FindEx - Current.Parent = ' + IntToHex(LongWord(ACurrent.Parent),8));
   if ACurrent.Node <> nil then
    begin
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDataIndex.FindEx - Current.Node = ' + IntToHex(ACurrent.Node.RecordNumber,16));
    end;
   {$ENDIF}
   
   if ACurrent.Key <> nil then
    begin
     case FCollateRule of
      ntfsCollateTypeSecurityHash:begin
        Value:=CompareSecurityDescriptor(AHash,PNTFSSecurityHashKeyData(ACurrent.Key).SecurityHash,PNTFSSecurityHashKeyData(ACurrent.Key).SecurityId,AValue,ASize);
        if Value = ntfsCompareEqual then
         begin
          {$IFDEF NTFS_DEBUG}
          if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDataIndex.FindEx - Equal');
          {$ENDIF}
          
          Result:=ACurrent;
         end
        else if Value = ntfsCompareLess then
         begin
          {$IFDEF NTFS_DEBUG}
          if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDataIndex.FindEx - Less');
          {$ENDIF}
          
          Result:=FindEx(AValue,ASize,AHash,TNTFSDataKey(ACurrent.Child));
         end
        else if Value = ntfsCompareGreater then
         begin
          {$IFDEF NTFS_DEBUG}
          if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDataIndex.FindEx - Greater');
          {$ENDIF}
          
          Result:=FindEx(AValue,ASize,AHash,TNTFSDataKey(ACurrent.Right));
         end;
       end;
     end;
    end;
  end;
end;

{==============================================================================}

function TNTFSDataIndex.CreateBlank:TBtreeObject;
{Create a blank key when a node is added (Split/Empty)}
var
 Node:TNTFSDiskNode;
begin
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDataIndex.CreateBlank');
 {$ENDIF}
 
 {Create Blank}
 Result:=TNTFSDataKey.Create(GetKeyLocal);
 Result.Blank:=True;
 
 {Create Node}
 Node:=NewNode(ntfsUnknownRecordNumber);
 if Node = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDataIndex.CreateBlank - Node = ' + IntToHex(Node.RecordNumber,16));
 {$ENDIF}
 
 {Update Node}
 Node.Blank:=TNTFSDataKey(Result);
 
 {Update Key}
 TNTFSDataKey(Result).Node:=Node;
 TNTFSDataKey(Result).IsLastNode:=True;
 TNTFSDataKey(Result).EntrySize:=TNTFSDataKey(Result).CalculatedSize(FVolumeVersion);
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDataIndex.CreateBlank - Result = ' + IntToHex(LongWord(Result),8));
 {$ENDIF}
end;

{==============================================================================}

function TNTFSDataIndex.Compare(AEntry1,AEntry2:TBtreeObject):Integer;
begin
 {}
 Result:=ntfsCompareEqual; {Equal to fail Insert/Merge/Borrow}
 
 if AEntry1 = nil then Exit;
 if AEntry2 = nil then Exit;
 
 Result:=ntfsCompareLess;
 
 if AEntry2.Blank then Exit;
 {AEntry1.Blank is an error}
 
 Result:=ntfsCompareEqual; {Equal to fail Insert/Merge/Borrow}
 if (TNTFSDiskKey(AEntry1).Key <> nil) and (TNTFSDiskKey(AEntry2).Key <> nil) then
  begin
   Result:=CompareKey(TNTFSDiskKey(AEntry1).Key,TNTFSDiskKey(AEntry2).Key,TNTFSDiskKey(AEntry1).KeySize,TNTFSDiskKey(AEntry2).KeySize);
  end;
end;

{==============================================================================}

function TNTFSDataIndex.CreateKey(ANode:TNTFSDiskNode;ABlank:Boolean):TNTFSDiskKey;
{Create a key in the supplied node do not insert in index or mark as added}
begin
 {}
 Result:=nil;
 
 {Check Node}
 if ABlank and (ANode = nil) then Exit;
 
 {Create Key}
 Result:=TNTFSDataKey.Create(GetKeyLocal);
 
 {Setup Key}
 Result.Blank:=ABlank;
 Result.Node:=ANode;
 
 {Check Blank}
 if ABlank then
  begin
   {Set Blank}
   ANode.Blank:=Result;
  end;
end;

{==============================================================================}

function TNTFSDataIndex.NewKey(AKey,AData:Pointer;AKeySize,ADataSize:Word):TNTFSDiskKey;
{Create a key with the supplied data do not insert in index or mark as added}
begin
 {}
 Result:=nil;
 
 if AKey = nil then Exit;
 
 Result:=CreateKey(nil,False);
 if Result = nil then Exit;
 
 {Setup Key}
 TNTFSDataKey(Result).KeySize:=AKeySize;
 TNTFSDataKey(Result).DataSize:=ADataSize;
 if (TNTFSDataKey(Result).Key <> nil) and (AKey <> nil) then System.Move(AKey^,TNTFSDataKey(Result).Key^,AKeySize);
 if (TNTFSDataKey(Result).Data <> nil) and (AData <> nil) then System.Move(AData^,TNTFSDataKey(Result).Data^,ADataSize);
end;

{==============================================================================}

function TNTFSDataIndex.FindKey(AValue:Pointer;ASize:Word):TNTFSDiskKey;
var
 Hash:LongWord;
begin
 {}
 Result:=nil;
 
 if AValue = nil then Exit;
 
 case FCollateRule of
  ntfsCollateTypeBinary,ntfsCollateTypeFileName,ntfsCollateTypeUnicode,
  ntfsCollateTypeLongWord,ntfsCollateTypeGUID:begin
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDataIndex.FindKey - Size = ' + IntToStr(ASize));
    {$ENDIF}
    
    Result:=Find(AValue,ASize,TNTFSDataKey(FRoot));
   end;
  ntfsCollateTypeSID:begin
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDataIndex.FindKey - Size = ' + IntToStr(ASize));
    {$ENDIF}
    
    if not Security.IsValidSid(AValue) then Exit;
    
    Result:=Find(AValue,ASize,TNTFSDataKey(FRoot));
   end;
  ntfsCollateTypeSecurityHash:begin
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDataIndex.FindKey - Size = ' + IntToStr(ASize));
    {$ENDIF}
    
    if not Security.IsValidSecurityDescriptor(AValue) then Exit;
    Hash:=NTFSGenerateSecurityHash(AValue,ASize);
    
    Result:=FindEx(AValue,ASize,Hash,TNTFSDataKey(FRoot));
   end;
 end;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDataIndex.FindKey - Result = ' + IntToHex(LongWord(Result),8));
 {$ENDIF}
end;

{==============================================================================}
{==============================================================================}
{TNTFSPaddedIndex}
function TNTFSPaddedIndex.CreateBlank:TBtreeObject;
{Create a blank key when a node is added (Split/Empty)}
var
 Node:TNTFSDiskNode;
begin
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSPaddedIndex.CreateBlank');
 {$ENDIF}
 
 {Create Blank}
 Result:=TNTFSPaddedKey.Create(GetKeyLocal);
 Result.Blank:=True;
 
 {Create Node}
 Node:=NewNode(ntfsUnknownRecordNumber);
 if Node = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSPaddedIndex.CreateBlank - Node = ' + IntToHex(Node.RecordNumber,16));
 {$ENDIF}
 
 {Update Node}
 Node.Blank:=TNTFSPaddedKey(Result);
 
 {Update Key}
 TNTFSPaddedKey(Result).Node:=Node;
 TNTFSPaddedKey(Result).IsLastNode:=True;
 TNTFSPaddedKey(Result).EntrySize:=TNTFSPaddedKey(Result).CalculatedSize(FVolumeVersion);
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSPaddedIndex.CreateBlank - Result = ' + IntToHex(LongWord(Result),8));
 {$ENDIF}
end;

{==============================================================================}

function TNTFSPaddedIndex.CreateKey(ANode:TNTFSDiskNode;ABlank:Boolean):TNTFSDiskKey;
{Create a key in the supplied node do not insert in index or mark as added}
begin
 {}
 Result:=nil;
 
 {Check Node}
 if ABlank and (ANode = nil) then Exit;
 
 {Create Key}
 Result:=TNTFSPaddedKey.Create(GetKeyLocal);
 
 {Setup Key}
 Result.Blank:=ABlank;
 Result.Node:=ANode;
 
 {Check Blank}
 if ABlank then
  begin
   {Set Blank}
   ANode.Blank:=Result;
  end;
end;

{==============================================================================}

function TNTFSPaddedIndex.NewKey(AKey,AData:Pointer;AKeySize,ADataSize:Word):TNTFSDiskKey;
{Create a key with the supplied data do not insert in index or mark as added}
begin
 {}
 Result:=NewKeyEx(AKey,AData,nil,AKeySize,ADataSize,0);
end;

{==============================================================================}

function TNTFSPaddedIndex.NewKeyEx(AKey,AData,APadding:Pointer;AKeySize,ADataSize,APaddingSize:Word):TNTFSDiskKey;
{Create a key with the supplied data do not insert in index or mark as added}
begin
 {}
 Result:=nil;
 
 if AKey = nil then Exit;
 
 Result:=CreateKey(nil,False);
 if Result = nil then Exit;
 
 {Setup Key}
 TNTFSPaddedKey(Result).KeySize:=AKeySize;
 TNTFSPaddedKey(Result).DataSize:=ADataSize;
 TNTFSPaddedKey(Result).PaddingSize:=APaddingSize;
 if (TNTFSPaddedKey(Result).Key <> nil) and (AKey <> nil) then System.Move(AKey^,TNTFSPaddedKey(Result).Key^,AKeySize);
 if (TNTFSPaddedKey(Result).Data <> nil) and (AData <> nil) then System.Move(AData^,TNTFSPaddedKey(Result).Data^,ADataSize);
 if (TNTFSPaddedKey(Result).Padding <> nil) and (APadding <> nil) then System.Move(APadding^,TNTFSPaddedKey(Result).Padding^,APaddingSize);
end;

{==============================================================================}
{==============================================================================}
{TNTFSAttributeIndex}
function TNTFSAttributeIndex.Find(AValue:Pointer;ASize:Word;ACurrent:TNTFSAttributeKey):TNTFSAttributeKey;
var
 Value:Integer;
begin
 {}
 Result:=nil;
 
 if ACurrent = nil then Exit;
 if ACurrent.Blank then
  begin
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSAttributeIndex.Find - Blank');
   {$ENDIF}
   
   Result:=Find(AValue,ASize,TNTFSAttributeKey(ACurrent.Child));
  end
 else
  begin
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSAttributeIndex.Find - Current = ' + IntToHex(LongWord(ACurrent),8));
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSAttributeIndex.Find - Current.Child = ' + IntToHex(LongWord(ACurrent.Child),8));
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSAttributeIndex.Find - Current.Parent = ' + IntToHex(LongWord(ACurrent.Parent),8));
   if ACurrent.Node <> nil then
    begin
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSAttributeIndex.Find - Current.Node = ' + IntToHex(ACurrent.Node.RecordNumber,16));
    end;
   {$ENDIF}
   
   if ACurrent.Key <> nil then
    begin
     case FIndexType of
      ntfsAttrTypeFileName:begin
        Value:=CompareKey(AValue,@PNTFSFileName(ACurrent.Key).FileName[0],ASize,PNTFSFileName(ACurrent.Key).FileNameLength);
        if Value = ntfsCompareEqual then
         begin
          {$IFDEF NTFS_DEBUG}
          if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSAttributeIndex.Find - Equal');
          if ACurrent.Attribute <> nil then if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSAttributeIndex.Find - ' + TNTFSFileNameAttribute(ACurrent.Attribute).FileName);
          {$ENDIF}
          
          Result:=ACurrent;
         end
        else if Value = ntfsCompareLess then
         begin
          {$IFDEF NTFS_DEBUG}
          if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSAttributeIndex.Find - Less');
          if ACurrent.Attribute <> nil then if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSAttributeIndex.Find - ' + TNTFSFileNameAttribute(ACurrent.Attribute).FileName);
          {$ENDIF}
          
          Result:=Find(AValue,ASize,TNTFSAttributeKey(ACurrent.Child));
         end
        else if Value = ntfsCompareGreater then
         begin
          {$IFDEF NTFS_DEBUG}
          if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSAttributeIndex.Find - Greater');
          if ACurrent.Attribute <> nil then if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSAttributeIndex.Find - ' + TNTFSFileNameAttribute(ACurrent.Attribute).FileName);
          {$ENDIF}
          
          Result:=Find(AValue,ASize,TNTFSAttributeKey(ACurrent.Right));
         end;
       end;
     end;
    end;
  end;
end;

{==============================================================================}

function TNTFSAttributeIndex.CreateBlank:TBtreeObject;
{Create a blank key when a node is added (Split/Empty)}
var
 Node:TNTFSDiskNode;
begin
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSAttributeIndex.CreateBlank');
 {$ENDIF}
 
 {Create Blank}
 Result:=TNTFSAttributeKey.Create(GetKeyLocal);
 Result.Blank:=True;
 
 {Create Node}
 Node:=NewNode(ntfsUnknownRecordNumber);
 if Node = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSAttributeIndex.CreateBlank - Node = ' + IntToHex(Node.RecordNumber,16));
 {$ENDIF}
 
 {Update Node}
 Node.Blank:=TNTFSAttributeKey(Result);
 
 {Update Key}
 TNTFSAttributeKey(Result).Node:=Node;
 TNTFSAttributeKey(Result).IsLastNode:=True;
 TNTFSAttributeKey(Result).EntrySize:=TNTFSAttributeKey(Result).CalculatedSize(FVolumeVersion);
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSAttributeIndex.CreateBlank - Result = ' + IntToHex(LongWord(Result),8));
 {$ENDIF}
end;

{==============================================================================}

function TNTFSAttributeIndex.Compare(AEntry1,AEntry2:TBtreeObject):Integer;
begin
 {}
 Result:=ntfsCompareEqual; {Equal to fail Insert/Merge/Borrow}
 
 if AEntry1 = nil then Exit;
 if AEntry2 = nil then Exit;
 
 Result:=ntfsCompareLess;
 
 if AEntry2.Blank then Exit;
 {AEntry1.Blank is an error}
 
 Result:=ntfsCompareEqual; {Equal to fail Insert/Merge/Borrow}
 if (TNTFSDiskKey(AEntry1).Key <> nil) and (TNTFSDiskKey(AEntry2).Key <> nil) then
  begin
   case FIndexType of
    ntfsAttrTypeFileName:begin
      Result:=CompareKey(@PNTFSFileName(TNTFSDiskKey(AEntry1).Key).FileName[0],@PNTFSFileName(TNTFSDiskKey(AEntry2).Key).FileName[0],PNTFSFileName(TNTFSDiskKey(AEntry1).Key).FileNameLength,PNTFSFileName(TNTFSDiskKey(AEntry2).Key).FileNameLength);
     end;
   end;
  end;
end;

{==============================================================================}

function TNTFSAttributeIndex.CreateKey(ANode:TNTFSDiskNode;ABlank:Boolean):TNTFSDiskKey;
{Create a key in the supplied node do not insert in index or mark as added}
begin
 {}
 Result:=nil;
 
 {Check Node}
 if ABlank and (ANode = nil) then Exit;
 
 {Create Key}
 Result:=TNTFSAttributeKey.Create(GetKeyLocal);
 
 {Setup Key}
 Result.Blank:=ABlank;
 Result.Node:=ANode;
 
 {Check Blank}
 if ABlank then
  begin
   {Set Blank}
   ANode.Blank:=Result;
  end;
end;

{==============================================================================}

function TNTFSAttributeIndex.NewKey(AKey,AData:Pointer;AKeySize,ADataSize:Word):TNTFSDiskKey;
{Create a key with the supplied data do not insert in index or mark as added}
begin
 {}
 Result:=nil;
 
 if AKey = nil then Exit;
 if AData = nil then Exit;
 
 Result:=CreateKey(nil,False);
 if Result = nil then Exit;
 
 {Setup Key}
 if AKeySize = 0 then
  begin
   TNTFSAttributeKey(Result).FileReference:=Int64(AData^);
   TNTFSAttributeKey(Result).Attribute:=TNTFSDiskAttribute(AKey^);
   TNTFSAttributeKey(Result).UpdateKey;
  end
 else
  begin
   TNTFSAttributeKey(Result).FileReference:=Int64(AData^);
   TNTFSAttributeKey(Result).KeySize:=AKeySize;
   if (TNTFSAttributeKey(Result).Key <> nil) and (AKey <> nil) then System.Move(AKey^,TNTFSAttributeKey(Result).Key^,AKeySize);
  end;
end;

{==============================================================================}

function TNTFSAttributeIndex.GetKeyByFileName(const AFileName:String):TNTFSDiskKey;
var
 Size:LongWord;
 Length:LongWord;
 Buffer:Pointer;
begin
 {}
 Result:=nil;
 
 {Check Index}
 if FIndexType <> ntfsAttrTypeFileName then Exit;
 if FCollateRule <> ntfsCollateTypeFileName then Exit;
 
 {Get Size and Length}
 Size:=System.Length(AFileName) shl 1; {Multiply by SizeOf(WideChar) / SizeOf(Word)}
 Length:=System.Length(AFileName);
 
 {Get Buffer}
 Buffer:=GetMem(Size);
 if Buffer = nil then Exit;
 try
  {Get FileName}
  if not NTFSStringToWideBuffer(AFileName,Buffer,0,Length) then Exit;
  
  {Get Key}
  Result:=FindKey(Buffer,Length);
 finally
  FreeMem(Buffer);
 end;
end;

{==============================================================================}

function TNTFSAttributeIndex.NewKeyByAttribute(AAttribute:TNTFSDiskAttribute):TNTFSDiskKey;
var
 Reference:Int64;
begin
 {}
 Result:=nil;
 
 if AAttribute = nil then Exit;
 
 {Check Attribute}
 if AAttribute.AttributeType = FIndexType then
  begin
   {Get Reference}
   Reference:=AAttribute.BaseReference;
   
   {Add Key}
   Result:=NewKey(@AAttribute,@Reference,0,0);
  end;
end;

{==============================================================================}

function TNTFSAttributeIndex.GetKeyByAttribute(AAttribute:TNTFSDiskAttribute):TNTFSDiskKey;
var
 Size:LongWord;
 Length:LongWord;
 Buffer:Pointer;
begin
 {}
 Result:=nil;
 
 if AAttribute = nil then Exit;
 
 {Check Attribute}
 if AAttribute.AttributeType = ntfsAttrTypeFileName then
  begin
   {Check Index}
   if FIndexType <> ntfsAttrTypeFileName then Exit;
   if FCollateRule <> ntfsCollateTypeFileName then Exit;
   
   {Get Size and Length}
   Size:=TNTFSFileNameAttribute(AAttribute).FileNameSize;
   Length:=TNTFSFileNameAttribute(AAttribute).FileNameLength;
   
   {Get Buffer}
   Buffer:=GetMem(Size);
   if Buffer = nil then Exit;
   try
    {Get FileName}
    if not NTFSStringToWideBuffer(TNTFSFileNameAttribute(AAttribute).FileName,Buffer,0,Length) then Exit;
    
    {Get Key}
    Result:=FindKey(Buffer,Length);
   finally
    FreeMem(Buffer);
   end;
  end;
end;

{==============================================================================}

function TNTFSAttributeIndex.FindKey(AValue:Pointer;ASize:Word):TNTFSDiskKey;
begin
 {}
 Result:=nil;
 
 if AValue = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSAttributeIndex.FindKey - Size = ' + IntToStr(ASize));
 {$ENDIF}
 
 Result:=Find(AValue,ASize,TNTFSAttributeKey(FRoot));
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSAttributeIndex.FindKey - Result = ' + IntToHex(LongWord(Result),8));
 {$ENDIF}
end;

{==============================================================================}
{==============================================================================}
{TNTFSDiskNodes}
constructor TNTFSDiskNodes.Create(ANodeLocal:TMutexHandle;ALock:TSynchronizerHandle);
begin
 {}
 inherited Create(ALock);
 FNodeLocal:=ANodeLocal;
end;

{==============================================================================}

destructor TNTFSDiskNodes.Destroy; 
begin
 {}
 WriterLock;
 try
  FNodeLocal:=INVALID_HANDLE_VALUE;
 finally
  WriterUnlock;
  inherited Destroy;
 end;
end;

{==============================================================================}

function TNTFSDiskNodes.GetModified:Boolean;
var
 Node:TNTFSDiskNode;
begin
 {}
 Result:=False;
 
 Node:=TNTFSDiskNode(First);
 while Node <> nil do
  begin
   Result:=Node.Modified;
   if Result then Exit;
   
   Node:=TNTFSDiskNode(Node.Next);
  end;
end;

{==============================================================================}

procedure TNTFSDiskNodes.SetModified(AValue:Boolean);
var
 Node:TNTFSDiskNode;
begin
 {}
 Node:=TNTFSDiskNode(First);
 while Node <> nil do
  begin
   Node.Modified:=AValue;
   
   Node:=TNTFSDiskNode(Node.Next);
  end;
end;

{==============================================================================}

function TNTFSDiskNodes.TotalSize:LongWord;
var
 Node:TNTFSDiskNode;
begin
 {}
 Result:=0;
 
 Node:=TNTFSDiskNode(First);
 while Node <> nil do
  begin
   Inc(Result,Node.IndexSize); {Use IndexSize not CalculatedSize}
   
   Node:=TNTFSDiskNode(Node.Next);
  end;
end;

{==============================================================================}

function TNTFSDiskNodes.NodeCount:LongWord;
begin
 {}
 Result:=Count;
end;

{==============================================================================}
{==============================================================================}
{TNTFSDiskNode}
constructor TNTFSDiskNode.Create(ALocalLock:TMutexHandle;AIndex:TNTFSDiskIndex);
begin
 {}
 inherited Create;
 FLocalLock:=ALocalLock;
 
 FStatus:=ntfsStatusNone;

 FIndexFlags:=ntfsIndexHeaderFlagNone;
 FIndexSize:=0;
 FIndexAllocated:=0;
 FEntryOffset:=0;

 FRecordNumber:=ntfsUnknownRecordNumber;
 FUpdateSequenceOffset:=0;
 FUpdateSequenceLength:=0;

 FUpdateSequenceNumber:=0;
 FLogFileSequenceNumber:=0;

 FIndex:=AIndex;
 FBlank:=nil;
end;

{==============================================================================}

destructor TNTFSDiskNode.Destroy;
begin
 {}
 FIndex:=nil;
 if (Deleted) and (FBlank <> nil) then FBlank.Free;
 FBlank:=nil;
 
 FLocalLock:=INVALID_HANDLE_VALUE;
 inherited Destroy;
end;

{==============================================================================}

function TNTFSDiskNode.AcquireLock:Boolean;
begin
 {}
 Result:=(MutexLock(FLocalLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSDiskNode.ReleaseLock:Boolean;
begin
 {}
 Result:=(MutexUnlock(FLocalLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSDiskNode.GetAdded:Boolean;
begin
 {}
 Result:=((FStatus and ntfsStatusAdded) = ntfsStatusAdded);
end;

{==============================================================================}

procedure TNTFSDiskNode.SetAdded(AValue:Boolean);
begin
 {}
 if AValue then
  begin
   FStatus:=(FStatus or (ntfsStatusAdded or ntfsStatusChanged));
  end
 else
  begin
   FStatus:=(FStatus and not ntfsStatusAdded);
  end;
end;

{==============================================================================}

function TNTFSDiskNode.GetDeleted:Boolean;
begin
 {}
 Result:=((FStatus and ntfsStatusDeleted) = ntfsStatusDeleted);
end;

{==============================================================================}

procedure TNTFSDiskNode.SetDeleted(AValue:Boolean);
begin
 {}
 if AValue then
  begin
   FStatus:=(FStatus or (ntfsStatusDeleted or ntfsStatusChanged));
  end
 else
  begin
   FStatus:=(FStatus and not ntfsStatusDeleted);
  end;
end;

{==============================================================================}

function TNTFSDiskNode.GetChanged:Boolean;
var
 Key:TNTFSDiskKey;
begin
 {Check Node}
 Result:=((FStatus and ntfsStatusChanged) = ntfsStatusChanged);
 if Result then Exit;
 
 {Check Keys}
 Key:=TNTFSDiskKey(FBlank);
 while Key <> nil do
  begin
   Result:=Key.Changed;
   if Result then Exit;
   
   Key:=TNTFSDiskKey(Key.Left);
  end;
end;

{==============================================================================}

procedure TNTFSDiskNode.SetChanged(AValue:Boolean);
var
 Key:TNTFSDiskKey;
begin
 {}
 if AValue then
  begin
   FStatus:=(FStatus or ntfsStatusChanged);
  end
 else
  begin
   {Set Node}
   FStatus:=(FStatus and not ntfsStatusChanged);
   
   {Set Keys}
   Key:=TNTFSDiskKey(FBlank);
   while Key <> nil do
    begin
     Key.Changed:=False;
     Key:=TNTFSDiskKey(Key.Left);
    end;
  end;
end;

{==============================================================================}

function TNTFSDiskNode.GetModified:Boolean;
begin
 {}
 Result:=False;
 
 if Added or Deleted or Changed then
  begin
   Result:=True;
  end;
end;

{==============================================================================}

procedure TNTFSDiskNode.SetModified(AValue:Boolean);
begin
 {}
 if AValue then
  begin
   Changed:=True;
  end
 else
  begin
   Added:=False;
   Deleted:=False;
   Changed:=False;
  end;
end;

{==============================================================================}

function TNTFSDiskNode.GetIsRoot:Boolean;
begin
 {}
 Result:=False;
 
 if FBlank = nil then Exit;
 
 Result:=(FBlank.Parent = nil);
end;

{==============================================================================}

function TNTFSDiskNode.GetHasSubNodes:Boolean;
begin
 {}
 Result:=((FIndexFlags and ntfsIndexHeaderFlagSubNodes) = ntfsIndexHeaderFlagSubNodes);
end;

{==============================================================================}

procedure TNTFSDiskNode.SetHasSubNodes(AValue:Boolean);
begin
 {}
 if AValue then
  begin
   if (FIndexFlags and ntfsIndexHeaderFlagSubNodes) = 0 then Changed:=True;
   FIndexFlags:=(FIndexFlags or ntfsIndexHeaderFlagSubNodes);
  end
 else
  begin
   if (FIndexFlags and ntfsIndexHeaderFlagSubNodes) <> 0 then Changed:=True;
   FIndexFlags:=(FIndexFlags and not ntfsIndexHeaderFlagSubNodes);
  end;
end;

{==============================================================================}

function TNTFSDiskNode.GetStart:TNTFSDiskKey;
{Includes the blank key which may be the only key}
begin
 {}
 Result:=nil;
 
 {Check Blank}
 if FBlank <> nil then
  begin
   Result:=FBlank;
   
   {Check Left}
   while Result.Left <> nil do
    begin
     Result:=TNTFSDiskKey(Result.Left);
    end;
  end;
end;

{==============================================================================}

function TNTFSDiskNode.KeyCount:Integer;
{Includes the blank key which may be the only key}
var
 Sibling:TNTFSDiskKey;
begin
 {}
 Result:=0;
 
 Sibling:=GetStart;
 while Sibling <> nil do
  begin
   Inc(Result);
   
   Sibling:=TNTFSDiskKey(Sibling.Right);
  end;
end;

{==============================================================================}

function TNTFSDiskNode.IndexFree:LongWord;
{Determine the free space remaining in the node}
begin
 {}
 Result:=FIndexAllocated - FIndexSize;
end;

{==============================================================================}

function TNTFSDiskNode.UpdateKeys(AVersion:Word):Boolean;
{Update Flags and Sub Node Number on all keys}
var
 SubNodes:Boolean;
 Key:TNTFSDiskKey;
 Node:TNTFSDiskNode;
begin
 {}
 Result:=False;
 
 if FBlank = nil then Exit;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - RecordNumber = ' + IntToHex(FRecordNumber,16)  + ' KeyCount = ' + IntToStr(KeyCount) + ' Changed = ' + BoolToStr(Changed) +  ' Added = ' + BoolToStr(Added) +  ' Deleted = ' + BoolToStr(Deleted));
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - Blank.Parent = ' + IntToHex(LongWord(TNTFSDiskKey(FBlank).Parent),8));
 {$ENDIF}
 
 {Set Sub Nodes}
 SubNodes:=False;
 
 {Get Blank}
 Key:=TNTFSDiskKey(FBlank);
 
 {Set Last}
 Key.IsLastNode:=True;
 {$IFDEF NTFS_DEBUG}
  if Key.Right <> nil then
   begin
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - ERROR Blank key is not last');
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - Key = ' + IntToHex(LongWord(Key),8));
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - Node = ' + IntToHex(FRecordNumber,16));
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - Blank Right = ' + IntToHex(LongWord(Key.Right),8));
    Exit;
   end;
 {$ENDIF}
 while Key <> nil do
  begin
   {Set Node}
   Key.Node:=Self;
   
   {Set Size}
   Key.EntrySize:=Key.CalculatedSize(AVersion);
   {$IFDEF NTFS_DEBUG}
    if TNTFSDiskKey(FBlank).Child <> nil then
     begin
      if Key.Child = nil then
       begin
        if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - ERROR Key has no child when Blank does');
        if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - Key = ' + IntToHex(LongWord(Key),8));
        if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - Node = ' + IntToHex(FRecordNumber,16));
        Exit;
       end;
     end
    else
     begin
      if Key.Child <> nil then
       begin
        if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - ERROR Key has a child when Blank does not');
        if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - Key = ' + IntToHex(LongWord(Key),8));
        if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - Node = ' + IntToHex(FRecordNumber,16));
        if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - Key.Child = ' + IntToHex(LongWord(Key.Child),8));
        Exit;
       end;
     end;
    if Key.Parent <> TNTFSDiskKey(FBlank).Parent then
     begin
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - ERROR Key not same parent as Blank');
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - Key = ' + IntToHex(LongWord(Key),8));
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - Node = ' + IntToHex(FRecordNumber,16));
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - Key.Parent = ' + IntToHex(LongWord(Key.Parent),8));
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - Blank.Parent = ' + IntToHex(LongWord(TNTFSDiskKey(FBlank).Parent),8));
      Exit;
     end;
    if Key.Left = nil then
     begin
      if Key.Parent <> nil then
       begin
        if Key.Parent.Child <> Key then
         begin
          if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - ERROR Parent not linked to Child');
          if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - Key = ' + IntToHex(LongWord(Key),8));
          if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - Node = ' + IntToHex(FRecordNumber,16));
          if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - Key.Parent = ' + IntToHex(LongWord(Key.Parent),8));
          if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - Key.Parent.Child = ' + IntToHex(LongWord(Key.Parent.Child),8));
          Exit;
         end;
       end;
     end;
    if FIndex <> nil then
     begin
      if not Key.Blank then
       begin
        if Key.Left <> nil then
         begin
          if FIndex.CollateRule = ntfsCollateTypeFileName then
           begin
            if FIndex.CompareKey(@PNTFSFileName(Key.Key).FileName[0],@PNTFSFileName(TNTFSDiskKey(Key.Left).Key).FileName[0],PNTFSFileName(Key.Key).FileNameLength,PNTFSFileName(TNTFSDiskKey(Key.Left).Key).FileNameLength) <> ntfsCompareGreater then
             begin
              if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - ERROR Key not greater than Left');
              if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - Key = ' + IntToHex(LongWord(Key),8));
              if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - Node = ' + IntToHex(FRecordNumber,16));
              if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - Key.Left = ' + IntToHex(LongWord(Key.Left),8));
              Exit;
             end;
           end;
         end;
       end;
     end;
   {$ENDIF}
   
   {Check Child}
   if Key.Child = nil then
    begin
     {Set Sub Node}
     Key.HasSubNode:=False;
     
     {Set Sub Node Number}
     Key.SubNodeNumber:=ntfsUnknownRecordNumber;
    end
   else
    begin
     {Set Sub Nodes}
     SubNodes:=True;
     
     {Set Sub Node}
     Key.HasSubNode:=True;
     
     {Get Node}
     Node:=TNTFSDiskKey(Key.Child).Node;
     
     {Set Sub Node Number}
     if Node <> nil then Key.SubNodeNumber:=Node.RecordNumber; {Node may be nil until the second pass}
      {$IFDEF NTFS_DEBUG}
      if Key.Child.Parent <> Key then
       begin
        if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - ERROR Child not linked to Parent');
        if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - Key = ' + IntToHex(LongWord(Key),8));
        if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - Node = ' + IntToHex(FRecordNumber,16));
        if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - Key.Child = ' + IntToHex(LongWord(Key.Child),8));
        if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - Key.Child.Parent = ' + IntToHex(LongWord(Key.Child.Parent),8));
        Exit;
       end;
      if Key.Child.Left <> nil then
       begin
        if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - ERROR Child not Start key');
        if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - Key = ' + IntToHex(LongWord(Key),8));
        if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - Node = ' + IntToHex(FRecordNumber,16));
        if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - Key.Child = ' + IntToHex(LongWord(Key.Child),8));
        if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - Key.Child.Left = ' + IntToHex(LongWord(Key.Child.Left),8));
        Exit;
       end;
      if FIndex <> nil then
       begin
        if not Key.Blank then
         begin
          if FIndex.CollateRule = ntfsCollateTypeFileName then
           begin
            if FIndex.CompareKey(@PNTFSFileName(Key.Key).FileName[0],@PNTFSFileName(TNTFSDiskKey(Key.Child).Key).FileName[0],PNTFSFileName(Key.Key).FileNameLength,PNTFSFileName(TNTFSDiskKey(Key.Child).Key).FileNameLength) <> ntfsCompareGreater then
             begin
              if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - ERROR Key not greater than Child');
              if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - Key = ' + IntToHex(LongWord(Key),8));
              if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - Node = ' + IntToHex(FRecordNumber,16));
              if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - Key.Child = ' + IntToHex(LongWord(Key.Child),8));
              if TNTFSDiskKey(Key.Child).Node <> nil then
               begin
                if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.UpdateKeys - Child Node = ' + IntToHex(TNTFSDiskKey(Key.Child).Node.RecordNumber,16));
               end;
              Exit;
             end;
           end;
         end;
       end;
      {$ENDIF}
    end;
    
   Key:=TNTFSDiskKey(Key.Left);
  end;
  
 {Set Sub Nodes}
 HasSubNodes:=SubNodes;
 
 Result:=True;
end;

{==============================================================================}

function TNTFSDiskNode.CalculatedSize(AVersion:Word):LongWord;
{Calculated Size includes any rounding required for alignment}
var
 Key:TNTFSDiskKey;
begin
 {}
 Result:=FEntryOffset;
 
 Key:=TNTFSDiskKey(FBlank);
 while Key <> nil do
  begin
   Inc(Result,Key.EntrySize); {Use EntrySize not CalculatedSize}
   
   Key:=TNTFSDiskKey(Key.Left);
  end;
  
 Result:=NTFSRoundLongWordTo8Bytes(Result);
end;

{==============================================================================}

function TNTFSDiskNode.CalculatedOffset(AVersion:Word):LongWord;
{Determine the entry offset for this node based on version}
{Will be different for root node versus other nodes}
begin
 {}
 Result:=0;
 
 if FBlank = nil then Exit;

 if FBlank.Parent = nil then
  begin
   {Root Node}
   Result:=ntfsIndexHeaderSize;
  end
 else
  begin
   {Sub Node}
   Result:=NTFSRoundLongWordTo8Bytes(ntfsIndexHeaderSize + (FUpdateSequenceLength shl 1)); {Multiply by SizeOf(Word)}
  end;
end;

{==============================================================================}

function TNTFSDiskNode.CalculatedAllocated(AVersion:Word):LongWord;
{Determine the index allocated for this node based on version}
{Will be different for root node versus other nodes}
begin
 {}
 Result:=0;
 
 if FIndex = nil then Exit;
 if FBlank = nil then Exit;

 if FBlank.Parent = nil then
  begin
   {Root Node}
   Result:=CalculatedSize(AVersion);
  end
 else
  begin
   {Sub Node}
   Result:=FIndex.IndexRecordSize - ntfsIndexRecordSize;
  end;
end;

{==============================================================================}

function TNTFSDiskNode.CalculatedSequenceOffset(AVersion:Word):Word;
{Determine the update sequence offset for this node based on version}
{Root node will have a sequence offset of zero}
begin
 {}
 Result:=0;
 
 if FBlank = nil then Exit;

 if FBlank.Parent <> nil then
  begin
   Result:=ntfsIndexRecordSize + ntfsIndexHeaderSize;
  end;
end;

{==============================================================================}

function TNTFSDiskNode.CalculatedSequenceLength(ASectorSize:Word):Word;
{Determine the update sequence length for this node based on sector size}
{Sequence Length is the number of Words in the update sequence array}
{Root node will have a sequence length of zero}
var
 Size:LongWord;
begin
 {}
 Result:=0;
 
 if FBlank = nil then Exit;
 if ASectorSize = 0 then Exit;

 if FBlank.Parent <> nil then
  begin
   Result:=1;
   Size:=0;
   while Size < FIndexAllocated do
    begin
     Inc(Result,1);
     Inc(Size,ASectorSize);
    end;
  end;
end;

{==============================================================================}

function TNTFSDiskNode.WriteEmpty(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word;var AUpdateSequenceOffset,AUpdateSequenceLength:Word):Boolean;
{Write an empty index record, index header and index entry to the supplied buffer at the supplied offset}
var
 Size:LongWord;

 AEntrySize:Word;
 AEntryOffset:LongWord;
 AIndexSize:LongWord;
 AIndexAllocated:LongWord;

 IndexEntry:PNTFSIndexEntry;
 IndexHeader:PNTFSIndexHeader;
 IndexRecord:PNTFSIndexRecord;
 UpdateSequenceRecord:PNTFSUpdateSequenceRecord;
begin
 {}
 Result:=False;
 
 if FIndex = nil then Exit;
 if ABuffer = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.WriteEmpty - Offset = ' + IntToStr(AOffset) + ' Size = ' + IntToStr(ASize));
 {$ENDIF}
 
 {Get Values}
  {Entry Size}
 AEntrySize:=ntfsIndexEntrySize;
  {Index Allocated}
 AIndexAllocated:=FIndex.IndexRecordSize - ntfsIndexRecordSize;
  {Sequence Offset}
 AUpdateSequenceOffset:=ntfsIndexRecordSize + ntfsIndexHeaderSize;
  {Sequence Length}
 AUpdateSequenceLength:=1;
 Size:=0;
 while Size < AIndexAllocated do
  begin
   Inc(AUpdateSequenceLength);
   Inc(Size,ntfsUpdateSequenceSize); {Previously FIndex.SectorSize however always 512} //To Do //Testing
  end;
  {Entry Offset}
 AEntryOffset:=NTFSRoundLongWordTo8Bytes(ntfsIndexHeaderSize + (AUpdateSequenceLength shl 1)); {Multiply by SizeOf(Word)}
  {Index Size}
 AIndexSize:=NTFSRoundLongWordTo8Bytes(AEntryOffset + AEntrySize);
 
 {Get Record}
 IndexRecord:=PNTFSIndexRecord(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if ASize >=ntfsIndexRecordSize then
  begin
   {Write Record}
   IndexRecord.MagicNumber:=ntfsIndexSignature;
   IndexRecord.RecordNumber:=FRecordNumber;
   IndexRecord.UpdateSequenceOffset:=AUpdateSequenceOffset;
   IndexRecord.UpdateSequenceLength:=AUpdateSequenceLength;
   IndexRecord.LogFileSequenceNumber:=FLogFileSequenceNumber;
   
   {Get Update}
   UpdateSequenceRecord:=PNTFSUpdateSequenceRecord(LongWord(ABuffer) + AOffset + IndexRecord.UpdateSequenceOffset);
   
   {Write Update}
   UpdateSequenceRecord.UpdateSequenceNumber:=FUpdateSequenceNumber;
   
   {Update Offset}
   Dec(ASize,ntfsIndexRecordSize);
   Inc(AOffset,ntfsIndexRecordSize);
   
   {Get Header}
   IndexHeader:=PNTFSIndexHeader(LongWord(ABuffer) + AOffset);
   
   {Check Size}
   if ASize >= ntfsIndexHeaderSize then
    begin
     {Write Header}
     IndexHeader.IndexFlags:=ntfsIndexHeaderFlagNone;
     IndexHeader.IndexSize:=AIndexSize;
     IndexHeader.IndexAllocated:=AIndexAllocated;
     IndexHeader.EntryOffset:=AEntryOffset;
     IndexHeader.Reserved1:=0;
     
     {Update Offset}
     Dec(ASize,IndexHeader.EntryOffset);
     Inc(AOffset,IndexHeader.EntryOffset);
     
     {Get Entry}
     IndexEntry:=PNTFSIndexEntry(LongWord(ABuffer) + AOffset);
     
     {Check Size}
     if ASize >= ntfsIndexEntrySize then
      begin
       {Write Entry}
       IndexEntry.Reserved1:=0;
       IndexEntry.EntryFlags:=ntfsIndexEntryFlagLastNode;
       IndexEntry.EntrySize:=AEntrySize;
       IndexEntry.KeySize:=0;
       IndexEntry.Reserved2:=0;
       
       {Update Offset}
       Dec(ASize,IndexEntry.EntrySize);
       Inc(AOffset,IndexEntry.EntrySize);
       
       Result:=True;
      end;
    end;
  end;
end;

{==============================================================================}

function TNTFSDiskNode.ReadRecord(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Read the index record and index header from the supplied buffer at the supplied offset}
var
 Size:LongWord;
 Offset:LongWord;
 IndexRecord:PNTFSIndexRecord;
 UpdateSequenceRecord:PNTFSUpdateSequenceRecord;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.ReadRecord - Offset = ' + IntToStr(AOffset) + ' Size = ' + IntToStr(ASize));
 {$ENDIF}
 
 {Get Record}
 IndexRecord:=PNTFSIndexRecord(LongWord(ABuffer) + AOffset);
 
 {Check Magic}
 if IndexRecord.MagicNumber <> ntfsIndexSignature then Exit;
 
 {Check Size}
 if ASize >=ntfsIndexRecordSize then
  begin
   {Clear Status}
   FStatus:=ntfsStatusNone;
   
   {Read Record}
   FRecordNumber:=IndexRecord.RecordNumber;
   FUpdateSequenceOffset:=IndexRecord.UpdateSequenceOffset;
   FUpdateSequenceLength:=IndexRecord.UpdateSequenceLength;
   FLogFileSequenceNumber:=IndexRecord.LogFileSequenceNumber;
   
   {Get Update}
   UpdateSequenceRecord:=PNTFSUpdateSequenceRecord(LongWord(ABuffer) + AOffset + IndexRecord.UpdateSequenceOffset);
   
   {Read Update}
   FUpdateSequenceNumber:=UpdateSequenceRecord.UpdateSequenceNumber;
   
   {Set Offset}
   Size:=ASize - ntfsIndexRecordSize;
   Offset:=AOffset + ntfsIndexRecordSize;
   
   {Read Header}
   if not ReadHeader(ABuffer,Offset,Size,AVersion) then Exit;
   
   {Update Offset}
   Dec(ASize,ntfsIndexRecordSize + FEntryOffset);
   Inc(AOffset,ntfsIndexRecordSize + FEntryOffset);
   
   Result:=True;
  end;
end;

{==============================================================================}

function TNTFSDiskNode.WriteRecord(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Write the index record and index header to the supplied buffer at the supplied offset}
var
 Size:LongWord;
 Offset:LongWord;
 IndexRecord:PNTFSIndexRecord;
 UpdateSequenceRecord:PNTFSUpdateSequenceRecord;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.WriteRecord - Offset = ' + IntToStr(AOffset) + ' Size = ' + IntToStr(ASize));
 {$ENDIF}
 
 {Get Record}
 IndexRecord:=PNTFSIndexRecord(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if ASize >=ntfsIndexRecordSize then
  begin
   {Write Record}
   IndexRecord.MagicNumber:=ntfsIndexSignature;
   IndexRecord.RecordNumber:=FRecordNumber;
   IndexRecord.UpdateSequenceOffset:=FUpdateSequenceOffset;
   IndexRecord.UpdateSequenceLength:=FUpdateSequenceLength;
   IndexRecord.LogFileSequenceNumber:=FLogFileSequenceNumber;
   
   {Get Update}
   UpdateSequenceRecord:=PNTFSUpdateSequenceRecord(LongWord(ABuffer) + AOffset + IndexRecord.UpdateSequenceOffset);
   
   {Write Update}
   UpdateSequenceRecord.UpdateSequenceNumber:=FUpdateSequenceNumber;
   
   {Set Offset}
   Size:=ASize - ntfsIndexRecordSize;
   Offset:=AOffset + ntfsIndexRecordSize;
   
   {Write Header}
   if not WriteHeader(ABuffer,Offset,Size,AVersion) then Exit;
   
   {Update Offset}
   Dec(ASize,ntfsIndexRecordSize + FEntryOffset);
   Inc(AOffset,ntfsIndexRecordSize + FEntryOffset);
   
   Result:=True;
  end;
end;

{==============================================================================}

function TNTFSDiskNode.ReadHeader(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Read the index header from the supplied buffer at the supplied offset}
var
 IndexHeader:PNTFSIndexHeader;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.ReadHeader - Offset = ' + IntToStr(AOffset) + ' Size = ' + IntToStr(ASize));
 {$ENDIF}
 
 {Get Header}
 IndexHeader:=PNTFSIndexHeader(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if ASize >= ntfsIndexHeaderSize then
  begin
   {Clear Status}
   FStatus:=ntfsStatusNone;
   
   {Read Header}
   FIndexFlags:=IndexHeader.IndexFlags;
   FIndexSize:=IndexHeader.IndexSize;
   FIndexAllocated:=IndexHeader.IndexAllocated;
   FEntryOffset:=IndexHeader.EntryOffset;
   
   {Check Size}
   if FIndexSize < ntfsIndexHeaderSize then Exit;
   
   {Update Offset}
   Dec(ASize,IndexHeader.EntryOffset);
   Inc(AOffset,IndexHeader.EntryOffset);
   
   Result:=True;
  end;
end;

{==============================================================================}

function TNTFSDiskNode.WriteHeader(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Write the index header to the supplied buffer at the supplied offset}
var
 IndexHeader:PNTFSIndexHeader;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskNode.WriteHeader - Offset = ' + IntToStr(AOffset) + ' Size = ' + IntToStr(ASize));
 {$ENDIF}
 
 {Get Header}
 IndexHeader:=PNTFSIndexHeader(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if ASize >= ntfsIndexHeaderSize then
  begin
   {Write Header}
   IndexHeader.IndexFlags:=FIndexFlags;
   IndexHeader.IndexSize:=FIndexSize;
   IndexHeader.IndexAllocated:=FIndexAllocated;
   IndexHeader.EntryOffset:=FEntryOffset;
   IndexHeader.Reserved1:=0;
   
   {Update Offset}
   Dec(ASize,IndexHeader.EntryOffset);
   Inc(AOffset,IndexHeader.EntryOffset);
   
   Result:=True;
  end;
end;

{==============================================================================}
{==============================================================================}
{TNTFSDiskKey}
constructor TNTFSDiskKey.Create(ALocalLock:TMutexHandle);
begin
 {}
 inherited Create;
 FLocalLock:=ALocalLock;
 
 FStatus:=ntfsStatusNone;
 FKey:=nil;

 FEntryFlags:=ntfsIndexEntryFlagNone;
 FEntrySize:=0;
 FKeySize:=0;
 FSubNodeNumber:=ntfsUnknownRecordNumber;

 FNode:=nil;
end;

{==============================================================================}

destructor TNTFSDiskKey.Destroy;
begin
 {}
 if FKey <> nil then FreeMem(FKey);
 FNode:=nil;
 
 FLocalLock:=INVALID_HANDLE_VALUE;
 inherited Destroy;
end;

{==============================================================================}

function TNTFSDiskKey.AcquireLock:Boolean;
begin
 {}
 Result:=(MutexLock(FLocalLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSDiskKey.ReleaseLock:Boolean;
begin
 {}
 Result:=(MutexUnlock(FLocalLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSDiskKey.GetChanged:Boolean;
begin
 {}
 Result:=((FStatus and ntfsStatusChanged) = ntfsStatusChanged);
end;

{==============================================================================}

procedure TNTFSDiskKey.SetChanged(AValue:Boolean);
begin
 {}
 if AValue then
  begin
   FStatus:=(FStatus or ntfsStatusChanged);
  end
 else
  begin
   FStatus:=(FStatus and not ntfsStatusChanged);
  end;
end;

{==============================================================================}

function TNTFSDiskKey.GetHasSubNode:Boolean;
begin
 {}
 Result:=((FEntryFlags and ntfsIndexEntryFlagSubNode) = ntfsIndexEntryFlagSubNode);
end;

{==============================================================================}

procedure TNTFSDiskKey.SetHasSubNode(AValue:Boolean);
begin
 {}
 if AValue then
  begin
   if (FEntryFlags and ntfsIndexEntryFlagSubNode) = 0 then Changed:=True;
   FEntryFlags:=(FEntryFlags or ntfsIndexEntryFlagSubNode);
  end
 else
  begin
   if (FEntryFlags and ntfsIndexEntryFlagSubNode) <> 0 then Changed:=True;
   FEntryFlags:=(FEntryFlags and not ntfsIndexEntryFlagSubNode);
  end;
end;

{==============================================================================}

function TNTFSDiskKey.GetIsLastNode:Boolean;
begin
 {}
 Result:=((FEntryFlags and ntfsIndexEntryFlagLastNode) = ntfsIndexEntryFlagLastNode);
end;

{==============================================================================}

procedure TNTFSDiskKey.SetIsLastNode(AValue:Boolean);
begin
 {}
 if AValue then
  begin
   if (FEntryFlags and ntfsIndexEntryFlagLastNode) = 0 then Changed:=True;
   FEntryFlags:=(FEntryFlags or ntfsIndexEntryFlagLastNode);
  end
 else
  begin
   if (FEntryFlags and ntfsIndexEntryFlagLastNode) <> 0 then Changed:=True;
   FEntryFlags:=(FEntryFlags and not ntfsIndexEntryFlagLastNode);
  end;
end;

{==============================================================================}

procedure TNTFSDiskKey.SetKeySize(ASize:Word);
begin
 {Virtual Base}
 FKeySize:=ASize;
end;

{==============================================================================}

procedure TNTFSDiskKey.SetSubNodeNumber(const ANodeNumber:Int64);
begin
 {}
 if FSubNodeNumber <> ANodeNumber then Changed:=True;
 FSubNodeNumber:=ANodeNumber;
end;

{==============================================================================}

procedure TNTFSDiskKey.SetNode(ANode:TNTFSDiskNode);
begin
 {}
 if FNode <> ANode then Changed:=True;
 FNode:=ANode;
end;

{==============================================================================}

function TNTFSDiskKey.CalculatedSize(AVersion:Word):Word;
{Calculated Size includes any rounding required for alignment}
begin
 {Virtual Base}
 Result:=0;
end;

{==============================================================================}

function TNTFSDiskKey.ReadKey(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Read the index entry from the supplied buffer at the supplied offset}
var
 IndexEntry:PNTFSIndexEntry;
 SubNodeRecord:PNTFSSubNodeRecord;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskKey.ReadKey - Offset = ' + IntToStr(AOffset) + ' Size = ' + IntToStr(ASize));
 {$ENDIF}
 
 {Get Entry}
 IndexEntry:=PNTFSIndexEntry(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if ASize >= ntfsIndexEntrySize then
  begin
   {Clear Status}
   FStatus:=ntfsStatusNone;
   
   {Read Entry}
   FEntryFlags:=IndexEntry.EntryFlags;
   FEntrySize:=IndexEntry.EntrySize;
   FKeySize:=IndexEntry.KeySize;
   
   {Check Size}
   if FEntrySize < ntfsIndexEntrySize then Exit;
   
   {Check Sub Node}
   if ((IndexEntry.EntryFlags and ntfsIndexEntryFlagSubNode) = ntfsIndexEntryFlagSubNode) then
    begin
     {Get Sub Node}
     SubNodeRecord:=PNTFSSubNodeRecord(LongWord(ABuffer) + AOffset + LongWord(IndexEntry.EntrySize - 8));
     
     {Read Sub Node}
     FSubNodeNumber:=SubNodeRecord.SubNodeNumber;
    end;
    
   {Update Offset}
   {Dec(ASize,IndexEntry.EntrySize);}
   {Inc(AOffset,IndexEntry.EntrySize);} {Update must be done by child class}
   
   Result:=True;
  end;
end;

{==============================================================================}

function TNTFSDiskKey.WriteKey(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Write the index entry to the supplied buffer at the supplied offset}
var
 IndexEntry:PNTFSIndexEntry;
 SubNodeRecord:PNTFSSubNodeRecord;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskKey.WriteKey - Offset = ' + IntToStr(AOffset) + ' Size = ' + IntToStr(ASize));
 {$ENDIF}
 
 {Get Entry}
 IndexEntry:=PNTFSIndexEntry(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if ASize >= ntfsIndexEntrySize then
  begin
   {Write Entry}
   IndexEntry.EntryFlags:=FEntryFlags;
   IndexEntry.EntrySize:=FEntrySize;
   IndexEntry.KeySize:=FKeySize;
   IndexEntry.Reserved2:=0;
   
   {Check Sub Node}
   if ((FEntryFlags and ntfsIndexEntryFlagSubNode) = ntfsIndexEntryFlagSubNode) then
    begin
     {Get Sub Node}
     SubNodeRecord:=PNTFSSubNodeRecord(LongWord(ABuffer) + AOffset + LongWord(FEntrySize - 8));
     {Write Sub Node}
     SubNodeRecord.SubNodeNumber:=FSubNodeNumber;
    end;
   
   {Update Offset}
   {Dec(ASize,IndexEntry.EntrySize);}
   {Inc(AOffset,IndexEntry.EntrySize);} {Update must be done by child class}
   
   Result:=True;
  end;
end;

{==============================================================================}
{==============================================================================}
{TNTFSDataKey}
constructor TNTFSDataKey.Create(ALocalLock:TMutexHandle);
begin
 {}
 inherited Create(ALocalLock);
 FData:=nil;
 FDataSize:=0;
end;

{==============================================================================}

destructor TNTFSDataKey.Destroy;
begin
 {}
 if FData <> nil then FreeMem(FData);
 inherited Destroy;
end;

{==============================================================================}

procedure TNTFSDataKey.SetKeySize(ASize:Word);
begin
 {}
 if ASize = 0 then
  begin
   if FKey <> nil then FreeMem(FKey);
   FKeySize:=0;
   FKey:=nil;
  end
 else
  begin
   if FKey <> nil then FreeMem(FKey);
   FKeySize:=ASize;
   FKey:=AllocMem(FKeySize);
  end;
end;

{==============================================================================}

procedure TNTFSDataKey.SetData(AData:Pointer);
begin
 {}
 if AData = nil then Exit;
 if FData = nil then Exit;
 if FDataSize = 0 then Exit;
 System.Move(AData^,FData^,FDataSize);
end;

{==============================================================================}

procedure TNTFSDataKey.SetDataSize(ASize:Word);
begin
 {}
 if ASize = 0 then
  begin
   if FData <> nil then FreeMem(FData);
   FDataSize:=0;
   FData:=nil;
  end
 else
  begin
   if FData <> nil then FreeMem(FData);
   FDataSize:=ASize;
   FData:=AllocMem(FDataSize);
  end;
end;

{==============================================================================}

function TNTFSDataKey.DataOffset:Word;
begin
 {}
 Result:=0;
 if FKeySize = 0 then Exit;
 {if FDataSize = 0 then Exit;} {DataOffset should be calculated even when DataSize is zero (see Reparse Index)}
 if IsLastNode then Exit;      {DataOffset is not present when LastNode flag is set}
 Result:=ntfsIndexEntrySize + FKeySize;
end;

{==============================================================================}

function TNTFSDataKey.CalculatedSize(AVersion:Word):Word;
{Calculated Size includes any rounding required for alignment}
begin
 {}
 Result:=NTFSRoundWordTo8Bytes(ntfsIndexEntrySize + FKeySize + FDataSize);
 if Child <> nil then Inc(Result,8); {SizeOf(Int64)}
end;

{==============================================================================}

function TNTFSDataKey.ReadKey(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Read the index entry from the supplied buffer at the supplied offset}
var
 DataIndexEntry:PNTFSDataIndexEntry;
begin
 {}
 Result:=inherited ReadKey(ABuffer,AOffset,ASize,AVersion);
 if not Result then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDataKey.ReadKey - Offset = ' + IntToStr(AOffset) + ' Size = ' + IntToStr(ASize));
 {$ENDIF}
 
 {Get Entry}
 DataIndexEntry:=PNTFSDataIndexEntry(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if ASize >= ntfsIndexEntrySize then
  begin
   {Read Entry}
   FDataSize:=DataIndexEntry.DataSize;
   
   {Set Sizes}
   SetKeySize(FKeySize);
   SetDataSize(FDataSize);
   
   {Read Data}
   if FKey <> nil then System.Move(DataIndexEntry.Key[0],FKey^,FKeySize);
   if FData <> nil then System.Move(Pointer(LongWord(ABuffer) + AOffset + DataIndexEntry.DataOffset)^,FData^,FDataSize);
   
   {Update Offset}
   Dec(ASize,DataIndexEntry.EntrySize);
   Inc(AOffset,DataIndexEntry.EntrySize);
   
   Result:=True;
  end;
end;

{==============================================================================}

function TNTFSDataKey.WriteKey(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Write the index entry to the supplied buffer at the supplied offset}
var
 DataIndexEntry:PNTFSDataIndexEntry;
begin
 {}
 Result:=inherited WriteKey(ABuffer,AOffset,ASize,AVersion);
 if not Result then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDataKey.WriteKey - Offset = ' + IntToStr(AOffset) + ' Size = ' + IntToStr(ASize));
 {$ENDIF}
 
 {Get Entry}
 DataIndexEntry:=PNTFSDataIndexEntry(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if ASize >= ntfsIndexEntrySize then
  begin
   {Write Entry}
   DataIndexEntry.DataSize:=FDataSize;
   DataIndexEntry.DataOffset:=DataOffset;
   DataIndexEntry.Reserved1:=0;
   
   {Write Data}
   if FKey <> nil then System.Move(FKey^,DataIndexEntry.Key[0],FKeySize);
   if FData <> nil then System.Move(FData^,Pointer(LongWord(ABuffer) + AOffset + DataIndexEntry.DataOffset)^,FDataSize);
   
   {Update Offset}
   Dec(ASize,DataIndexEntry.EntrySize);
   Inc(AOffset,DataIndexEntry.EntrySize);
   
   Result:=True;
  end;
end;

{==============================================================================}
{==============================================================================}
{TNTFSPaddedKey}
constructor TNTFSPaddedKey.Create(ALocalLock:TMutexHandle);
begin
 {}
 inherited Create(ALocalLock);
 FPadding:=nil;
 FPaddingSize:=0;
end;

{==============================================================================}

destructor TNTFSPaddedKey.Destroy;
begin
 {}
 if FPadding <> nil then FreeMem(FPadding);
 inherited Destroy;
end;

{==============================================================================}

procedure TNTFSPaddedKey.SetPadding(APadding:Pointer);
begin
 {}
 if APadding = nil then Exit;
 if FPadding = nil then Exit;
 if FPaddingSize = 0 then Exit;
 System.Move(APadding^,FPadding^,FPaddingSize);
end;

{==============================================================================}

procedure TNTFSPaddedKey.SetPaddingSize(ASize:Word);
begin
 {}
 if ASize = 0 then
  begin
   if FPadding <> nil then FreeMem(FPadding);
   FPaddingSize:=0;
   FPadding:=nil;
  end
 else
  begin
   if FPadding <> nil then FreeMem(FPadding);
   FPaddingSize:=ASize;
   FPadding:=AllocMem(FPaddingSize);
  end;
end;

{==============================================================================}

function TNTFSPaddedKey.ReadKey(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Read the index entry from the supplied buffer at the supplied offset}
var
 SubNodeSize:Word;
begin
 {}
 Result:=inherited ReadKey(ABuffer,AOffset,ASize,AVersion);
 if not Result then Exit;
 
 {Check Last}
 if not IsLastNode then
  begin
   {Check Child}
   SubNodeSize:=0;
   if Child <> nil then Inc(SubNodeSize,8); {SizeOf(Int64)}
   
   {Inherited method has already read the Key, just read the Padding}
   if FEntrySize > (SubNodeSize + DataOffset + FDataSize) then
    begin
     {Set Sizes}
     SetPaddingSize(FEntrySize - (SubNodeSize + DataOffset + FDataSize));
     
     {Read Data}
     if FPadding <> nil then System.Move(Pointer(PtrUInt(ABuffer) + PtrUInt(AOffset - (SubNodeSize + FPaddingSize)))^,FPadding^,FPaddingSize);
    end;
    
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSPaddedKey.ReadKey - PaddingSize = ' + IntToStr(FPaddingSize));
   {$ENDIF}
   
   Result:=True;
  end;
end;

{==============================================================================}

function TNTFSPaddedKey.WriteKey(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Write the index entry to the supplied buffer at the supplied offset}
var
 SubNodeSize:Word;
begin
 {}
 Result:=inherited WriteKey(ABuffer,AOffset,ASize,AVersion);
 if not Result then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSPaddedKey.WriteKey - PaddingSize = ' + IntToStr(FPaddingSize));
 {$ENDIF}
 
 {Check Last}
 if not IsLastNode then
  begin
   {Check Child}
   SubNodeSize:=0;
   if Child <> nil then Inc(SubNodeSize,8); {SizeOf(Int64)}
   
   {Inherited method has already written the Key, just write the Padding}
   if (FEntrySize - (SubNodeSize + DataOffset + FDataSize)) = FPaddingSize then
    begin
     {Write Data}
     if FPadding <> nil then System.Move(FPadding^,Pointer(PtrUInt(ABuffer) + PtrUInt(AOffset - (SubNodeSize + FPaddingSize)))^,FPaddingSize);
    end;
    
   Result:=True;
  end;
end;

{==============================================================================}
{==============================================================================}
{TNTFSAttributeKey}
constructor TNTFSAttributeKey.Create(ALocalLock:TMutexHandle);
begin
 {}
 inherited Create(ALocalLock);
 FFileReference:=0;
 FAttribute:=nil;
end;

{==============================================================================}

destructor TNTFSAttributeKey.Destroy;
begin
 {}
 FAttribute:=nil;
 inherited Destroy;
end;

{==============================================================================}

function TNTFSAttributeKey.GetInvalid:Boolean;
begin
 {}
 Result:=((FStatus and ntfsStatusInvalid) = ntfsStatusInvalid);
end;

{==============================================================================}

procedure TNTFSAttributeKey.SetInvalid(AValue:Boolean);
begin
 {}
 if AValue then
  begin
   FStatus:=(FStatus or ntfsStatusInvalid);
  end
 else
  begin
   FStatus:=(FStatus and not ntfsStatusInvalid);
  end;
end;

{==============================================================================}

procedure TNTFSAttributeKey.SetKeySize(ASize:Word);
begin
 {}
 if ASize = 0 then
  begin
   if FKey <> nil then FreeMem(FKey);
   FKeySize:=0;
   FKey:=nil;
  end
 else
  begin
   if FKey <> nil then FreeMem(FKey);
   FKeySize:=ASize;
   FKey:=AllocMem(FKeySize);
  end;
end;

{==============================================================================}

procedure TNTFSAttributeKey.SetAttribute(AAttribute:TNTFSDiskAttribute);
begin
 {}
 FAttribute:=AAttribute;
 if FAttribute <> nil then
  begin
   if FKey = nil then UpdateKey;
  end;
end;

{==============================================================================}

function TNTFSAttributeKey.UpdateKey:Boolean;
var
 Offset:LongWord;
begin
 {}
 Result:=False;
 if FAttribute = nil then Exit;
 SetKeySize(FAttribute.DataSize);
 if FKey = nil then Exit;
 Offset:=0;
 Result:=FAttribute.WriteData(FKey,Offset,ntfsNTFS31); //To Do //AVersion ? //From where ?
end;

{==============================================================================}

function TNTFSAttributeKey.RecordNumber:Int64;
begin
 {}
 TNTFSFileReference(Result).RecordNumber:=TNTFSFileReference(FFileReference).RecordNumber;
 TNTFSFileReference(Result).RecordSegment:=TNTFSFileReference(FFileReference).RecordSegment;
 TNTFSFileReference(Result).SequenceNumber:=0;
end;

{==============================================================================}

function TNTFSAttributeKey.CalculatedSize(AVersion:Word):Word;
{Calculated Size includes any rounding required for alignment}
begin
 {}
 Result:=NTFSRoundWordTo8Bytes(ntfsIndexEntrySize + FKeySize);
 if Child <> nil then Inc(Result,8); {SizeOf(Int64)}
end;

{==============================================================================}

function TNTFSAttributeKey.ReadKey(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Read the index entry from the supplied buffer at the supplied offset}
var
 AttributeIndexEntry:PNTFSAttributeIndexEntry;
begin
 {}
 Result:=inherited ReadKey(ABuffer,AOffset,ASize,AVersion);
 if not Result then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSAttributeKey.ReadKey - Offset = ' + IntToStr(AOffset) + ' Size = ' + IntToStr(ASize));
 {$ENDIF}
 
 {Get Entry}
 AttributeIndexEntry:=PNTFSAttributeIndexEntry(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if ASize >= ntfsIndexEntrySize then
  begin
   {Read Entry}
   FFileReference:=AttributeIndexEntry.FileReference;
   
   {Set Size}
   SetKeySize(FKeySize);
   
   {Read Data}
   if FKey <> nil then System.Move(AttributeIndexEntry.Key[0],FKey^,FKeySize);
   
   {Update Offset}
   Dec(ASize,AttributeIndexEntry.EntrySize);
   Inc(AOffset,AttributeIndexEntry.EntrySize);
   
   Result:=True;
  end;
end;

{==============================================================================}

function TNTFSAttributeKey.WriteKey(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Write the index entry to the supplied buffer at the supplied offset}
var
 AttributeIndexEntry:PNTFSAttributeIndexEntry;
begin
 {}
 Result:=inherited WriteKey(ABuffer,AOffset,ASize,AVersion);
 if not Result then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSAttributeKey.WriteKey - Offset = ' + IntToStr(AOffset) + ' Size = ' + IntToStr(ASize));
 {$ENDIF}
 
 {Get Entry}
 AttributeIndexEntry:=PNTFSAttributeIndexEntry(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if ASize >= ntfsIndexEntrySize then
  begin
   {Write Entry}
   AttributeIndexEntry.FileReference:=FFileReference;
   
   {Write Data}
   if FKey <> nil then System.Move(FKey^,AttributeIndexEntry.Key[0],FKeySize);
   
   {Update Offset}
   Dec(ASize,AttributeIndexEntry.EntrySize);
   Inc(AOffset,AttributeIndexEntry.EntrySize);
   
   Result:=True;
  end;
end;

{==============================================================================}
{==============================================================================}
{TNTFSDiskRuns}
constructor TNTFSDiskRuns.Create(ARunLocal:TMutexHandle;ALock:TSynchronizerHandle);
begin
 {}
 inherited Create(ALock);
 FRunLocal:=ARunLocal;
 
 FRecent:=nil;
 FRecentVCN:=ntfsUnknownCluster;
end;

{==============================================================================}

destructor TNTFSDiskRuns.Destroy; 
begin
 {}
 WriterLock;
 try
  FRunLocal:=INVALID_HANDLE_VALUE;
 finally
  WriterUnlock;
  inherited Destroy;
 end;
end;

{==============================================================================}

function TNTFSDiskRuns.GetStart:TNTFSDiskRun;
{Get the first actual run in the list (not Last)}
var
 Run:TNTFSDiskRun;
begin
 {}
 Result:=nil;

 Run:=TNTFSDiskRun(First);
 while Run <> nil do
  begin
   if not Run.IsLast then
    begin
     Result:=Run;
     Exit;
    end;
    
   Run:=TNTFSDiskRun(Run.Next);
  end;
end;

{==============================================================================}

function TNTFSDiskRuns.GetFinal:TNTFSDiskRun;
{Get the last actual run in the list (not Last)}
var
 Run:TNTFSDiskRun;
begin
 {}
 Result:=nil;

 Run:=TNTFSDiskRun(Last);
 while Run <> nil do
  begin
   if not Run.IsLast then
    begin
     Result:=Run;
     Exit;
    end;
    
   Run:=TNTFSDiskRun(Run.Prev);
  end;
end;

{==============================================================================}

function TNTFSDiskRuns.SparseCount:Int64;
{Get the count of sparse runs in the list}
var
 Run:TNTFSDiskRun;
begin
 {}
 Result:=0;

 Run:=TNTFSDiskRun(First);
 while Run <> nil do
  begin
   if not Run.IsLast then
    begin
     if Run.IsSparse then
      begin
       Inc(Result,Run.Length);
      end;
    end;
    
   Run:=TNTFSDiskRun(Run.Next);
  end;
end;

{==============================================================================}

function TNTFSDiskRuns.ClusterCount:Int64;
{Get the count of normal runs in the list}
var
 Run:TNTFSDiskRun;
begin
 {}
 Result:=0;

 Run:=TNTFSDiskRun(First);
 while Run <> nil do
  begin
   if not Run.IsLast then
    begin
     if not Run.IsSparse then
      begin
       Inc(Result,Run.Length);
      end;
    end;
    
   Run:=TNTFSDiskRun(Run.Next);
  end;
end;

{==============================================================================}

function TNTFSDiskRuns.AllocatedCount:Int64;
{Get the count of allocated runs in the list}
var
 Run:TNTFSDiskRun;
begin
 {}
 Result:=0;

 Run:=TNTFSDiskRun(First);
 while Run <> nil do
  begin
   if not Run.IsLast then
    begin
     Inc(Result,Run.Length);
    end;
    
   Run:=TNTFSDiskRun(Run.Next);
  end;
end;

{==============================================================================}

function TNTFSDiskRuns.TotalSize:LongWord;
var
 Run:TNTFSDiskRun;
begin
 {}
 Result:=0;
 
 Run:=TNTFSDiskRun(First);
 while Run <> nil do
  begin
   Inc(Result,Run.RunSize);
   
   Run:=TNTFSDiskRun(Run.Next);
  end;
end;

{==============================================================================}

function TNTFSDiskRuns.RunCount:LongWord;
begin
 {}
 Result:=Count;
end;

{==============================================================================}
{==============================================================================}
{TNTFSDiskRun}
constructor TNTFSDiskRun.Create(ALocalLock:TMutexHandle;AAttribute:TNTFSDiskAttribute);
begin
 {}
 inherited Create;
 FLocalLock:=ALocalLock;
 
 FStart:=ntfsUnknownCluster;
 FOffset:=ntfsStartCluster;
 FLength:=0;

 FAttribute:=AAttribute;
end;

{==============================================================================}

destructor TNTFSDiskRun.Destroy;
begin
 {}
 FAttribute:=nil;
 
 FLocalLock:=INVALID_HANDLE_VALUE;
 inherited Destroy;
end;

{==============================================================================}

function TNTFSDiskRun.AcquireLock:Boolean;
begin
 {}
 Result:=(MutexLock(FLocalLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSDiskRun.ReleaseLock:Boolean;
begin
 {}
 Result:=(MutexUnlock(FLocalLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSDiskRun.GetIsLast:Boolean;
begin
 {}
 Result:=((FStart = ntfsUnknownCluster) and (FLength = 0));
end;

{==============================================================================}

function TNTFSDiskRun.GetIsSparse:Boolean;
begin
 {}
 Result:=((FStart = ntfsUnknownCluster) and (FLength > 0));
end;

{==============================================================================}

function TNTFSDiskRun.GetOffsetSize:Byte;
var
 Count:Integer;
begin
 {}
 if FStart = ntfsStartCluster then
  begin
   {Boot}
   Result:=1;
  end
 else if FStart = ntfsUnknownCluster then
  begin
   {Last or Sparse}
   Result:=0;
  end
 else
  begin
   {Other}
   Result:=8; {Default to 8}
   for Count:=0 to 7 do  {1 to 8 for negative}
    begin
     if (FOffset and ntfsRunEncodeTests[Count]) = FOffset then
      begin
       {Positive Offset}
       Result:=Count;
       Exit;
      end
     else if (FOffset and ntfsRunNegativeTests[Count + 1]) = ntfsRunNegativeTests[Count + 1] then
      begin
       {Negative Offset}
       Result:=Count + 1;
       Exit;
      end;
    end;
  end;
end;

{==============================================================================}

function TNTFSDiskRun.GetLengthSize:Byte;
var
 Count:Integer;
begin
 {}
 //To Do //Last or Sparse ?
 Result:=8; {Default to 8}
 for Count:=0 to 7 do
  begin
   if (FLength and ntfsRunEncodeTests[Count]) = FLength then
    begin
     Result:=Count;
     Exit;
    end;
  end;
end;

{==============================================================================}

procedure TNTFSDiskRun.SetStart(const AStart:Int64);
var
 Run:TNTFSDiskRun;
begin
 {}
 FStart:=AStart;
 if FStart = ntfsUnknownCluster then
  begin
   {Last or Sparse}
   FOffset:=ntfsStartCluster;
  end
 else
  begin
   {Boot or Other}
   Run:=GetPredecessor;
   if Run = nil then
    begin
     FOffset:=FStart;
    end
   else
    begin
     FOffset:=FStart - Run.Start;
    end;
  end;
end;

{==============================================================================}

procedure TNTFSDiskRun.SetOffset(const AOffset:Int64);
{Note: This should only be called by ReadRun, all others should call SetStart}
{Note: Calling this with Offset 0 to specify a Sparse run will produce incorrect results}
var
 Run:TNTFSDiskRun;
begin
 {}
 FOffset:=AOffset;
 if (FOffset = ntfsStartCluster) and (FLength = 0) then
  begin
   {Last (Not Sparse)}
   FStart:=ntfsUnknownCluster;
  end
 else
  begin
   {Boot or Other}
   Run:=GetPredecessor;
   if Run = nil then
    begin
     FStart:=FOffset;
    end
   else
    begin
     FStart:=Run.Start + FOffset;
    end;
  end;
end;

{==============================================================================}

function TNTFSDiskRun.GetSuccessor:TNTFSDiskRun;
{Get the Next non Sparse and non Last run in the list}
var
 Run:TNTFSDiskRun;
begin
 {}
 Result:=nil;
 
 {Get Next}
 Run:=TNTFSDiskRun(Next);
 while Run <> nil do
  begin
   {Check Last}
   if not Run.IsLast then
    begin
     {Check Sparse}
     if not Run.IsSparse then
      begin
       Result:=Run;
       Exit;
      end;
    end;
    
   {Get Next}
   Run:=TNTFSDiskRun(Run.Next);
  end;
end;

{==============================================================================}

function TNTFSDiskRun.GetPredecessor:TNTFSDiskRun;
{Get the Previous non Sparse and non Last run in the list}
var
 Run:TNTFSDiskRun;
begin
 {}
 Result:=nil;
 
 {Get Next}
 Run:=TNTFSDiskRun(Prev);
 while Run <> nil do
  begin
   {Check Last}
   if not Run.IsLast then
    begin
     {Check Sparse}
     if not Run.IsSparse then
      begin
       Result:=Run;
       Exit;
      end;
    end;
    
   {Get Next}
   Run:=TNTFSDiskRun(Run.Prev);
  end;
end;

{==============================================================================}

function TNTFSDiskRun.RunSize:LongWord;
begin
 {}
 Result:=1 + GetOffsetSize + GetLengthSize; {SizeOf(Byte)}
end;

{==============================================================================}

function TNTFSDiskRun.UpdateRun:Boolean;
begin
 {}
 SetStart(FStart);
 Result:=True;
end;

{==============================================================================}

function TNTFSDiskRun.ReadRun(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean;
{Read the run data from the supplied buffer at the supplied offset}
var
 OffsetSize:Byte;
 LengthSize:Byte;
 RunData:PNTFSRunData;
 RunOffset:PNTFSRunOffset;
 RunLength:PNTFSRunLength;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {Get Run}
 RunData:=PNTFSRunData(LongWord(ABuffer) + AOffset);
 
 {Read Run}
 OffsetSize:=(Byte(RunData^) and ntfsRunOffsetMask) shr 4;
 LengthSize:=(Byte(RunData^) and ntfsRunLengthMask);
 
 {Update Offset}
 Inc(AOffset);
 
 {Get Length}
 RunLength:=PNTFSRunLength(LongWord(ABuffer) + AOffset);
 
 {Read Length}
 case LengthSize of
  1:begin
    FLength:=(RunLength.Length1 and ntfsRunDecode1Masks[LengthSize]);
   end;
  2:begin
    FLength:=(RunLength.Length2 and ntfsRunDecode2Masks[LengthSize]);
   end;
  3,4:begin
    FLength:=(RunLength.Length4 and ntfsRunDecode4Masks[LengthSize]);
   end;
  5,6,7,8:begin
    FLength:=(RunLength.Length8 and ntfsRunDecode8Masks[LengthSize]);
   end;
 end;
 
 {Update Offset}
 Inc(AOffset,LengthSize);
 
 {Get Offset}
 RunOffset:=PNTFSRunOffset(LongWord(ABuffer) + AOffset);
 
 {Read Offset}
 case OffsetSize of
  1:begin
    FOffset:=(RunOffset.Offset1 and ShortInt(ntfsRunDecode1Masks[OffsetSize]));
    
    {Check Negative}
    if FOffset >= ntfsRunDecode1Tests[OffsetSize] then FOffset:=(FOffset or ShortInt(not(ntfsRunDecode1Masks[OffsetSize])));
   end;
  2:begin
    FOffset:=(RunOffset.Offset2 and SmallInt(ntfsRunDecode2Masks[OffsetSize]));
    
    {Check Negative}
    if FOffset >= ntfsRunDecode2Tests[OffsetSize] then FOffset:=(FOffset or SmallInt(not(ntfsRunDecode2Masks[OffsetSize])));
   end;
  3,4:begin
    FOffset:=(RunOffset.Offset4 and LongInt(ntfsRunDecode4Masks[OffsetSize]));
    
    {Check Negative}
    if FOffset >= ntfsRunDecode4Tests[OffsetSize] then FOffset:=(FOffset or LongInt(not(ntfsRunDecode4Masks[OffsetSize])));
   end;
  5,6,7,8:begin
    FOffset:=(RunOffset.Offset8 and Int64(ntfsRunDecode8Masks[OffsetSize]));
    
    {Check Negative}
    if FOffset >= ntfsRunDecode8Tests[OffsetSize] then FOffset:=(FOffset or Int64(not(ntfsRunDecode8Masks[OffsetSize])));
   end;
 end;
 
 {Set Offset (If not Sparse or Last)}
 if OffsetSize > 0 then SetOffset(FOffset);
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then
  begin
   if IsSparse then
    begin
     FileSysLogDebug('TNTFSDiskRun.ReadRun - Length = ' + IntToHex(FLength,16) + ' Offset = ' + IntToHex(FOffset,16) + ' Start = ' + IntToHex(FStart,16) + ' (Sparse)');
    end
   else if IsLast then
    begin
     FileSysLogDebug('TNTFSDiskRun.ReadRun - Length = ' + IntToHex(FLength,16) + ' Offset = ' + IntToHex(FOffset,16) + ' Start = ' + IntToHex(FStart,16) + ' (Last)');
    end
   else
    begin
     FileSysLogDebug('TNTFSDiskRun.ReadRun - Length = ' + IntToHex(FLength,16) + ' Offset = ' + IntToHex(FOffset,16) + ' Start = ' + IntToHex(FStart,16));
    end;
  end;
 {$ENDIF}
 
 {Update Offset}
 Inc(AOffset,OffsetSize);
 
 Result:=True;
end;

{==============================================================================}

function TNTFSDiskRun.WriteRun(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean;
{Write the run data to the supplied buffer at the supplied offset}
var
 OffsetSize:Byte;
 LengthSize:Byte;
 RunData:PNTFSRunData;
 RunOffset:PNTFSRunOffset;
 RunLength:PNTFSRunLength;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskRun.WriteRun - Length = ' + IntToHex(FLength,16) + ' Offset = ' + IntToHex(FOffset,16) + ' Start = ' + IntToHex(FStart,16));
 {$ENDIF}
 
 {Get Run}
 RunData:=PNTFSRunData(LongWord(ABuffer) + AOffset);
 
 {Write Run}
 OffsetSize:=GetOffsetSize;
 LengthSize:=GetLengthSize;
 Byte(RunData^):=((OffsetSize shl 4) or LengthSize);
 
 {Update Offset}
 Inc(AOffset);
 
 {Get Length}
 RunLength:=PNTFSRunLength(LongWord(ABuffer) + AOffset);
 
 {Write Length}
 case LengthSize of
  1:begin
    RunLength.Length1:=(FLength and ntfsRunEncodeMasks[LengthSize]);
   end;
  2:begin
    RunLength.Length2:=(FLength and ntfsRunEncodeMasks[LengthSize]);
   end;
  3,4:begin
    RunLength.Length4:=(FLength and ntfsRunEncodeMasks[LengthSize]);
   end;
  5,6,7,8:begin
    RunLength.Length8:=(FLength and ntfsRunEncodeMasks[LengthSize]);
   end;
 end;
 
 {Update Offset}
 Inc(AOffset,LengthSize);
 
 {Get Offset}
 RunOffset:=PNTFSRunOffset(LongWord(ABuffer) + AOffset);
 
 {Write Offset}
 case OffsetSize of
  1:begin
    RunOffset.Offset1:=(FOffset and ntfsRunEncodeMasks[OffsetSize]);
   end;
  2:begin
    RunOffset.Offset2:=(FOffset and ntfsRunEncodeMasks[OffsetSize]);
   end;
  3,4:begin
    RunOffset.Offset4:=(FOffset and ntfsRunEncodeMasks[OffsetSize]);
   end;
  5,6,7,8:begin
    RunOffset.Offset8:=(FOffset and ntfsRunEncodeMasks[OffsetSize]);
   end;
 end;
 
 {Update Offset}
 Inc(AOffset,OffsetSize);
 
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TNTFSDiskItems}
constructor TNTFSDiskItems.Create(AItemLocal:TMutexHandle;ALock:TSynchronizerHandle);
begin
 {}
 inherited Create(ALock);
 FItemLocal:=AItemLocal;
 
 FStatus:=ntfsStatusNone;
end;

{==============================================================================}

destructor TNTFSDiskItems.Destroy; 
begin
 {}
 WriterLock;
 try
  FItemLocal:=INVALID_HANDLE_VALUE;
 finally
  WriterUnlock;
  inherited Destroy;
 end;
end;

{==============================================================================}

function TNTFSDiskItems.GetLoaded:Boolean;
begin
 {}
 Result:=((FStatus and ntfsStatusLoaded) = ntfsStatusLoaded);
end;

{==============================================================================}

procedure TNTFSDiskItems.SetLoaded(AValue:Boolean);
begin
 {}
 if AValue then
  begin
   FStatus:=(FStatus or ntfsStatusLoaded);
  end
 else
  begin
   FStatus:=(FStatus and not ntfsStatusLoaded);
  end;
end;

{==============================================================================}

function TNTFSDiskItems.GetPrevious(AItem:TNTFSDiskItem):TNTFSDiskItem;
{Note: Caller must hold the lock}
var
 Current:TNTFSDiskItem;
begin
 {}
 Result:=nil;
 
 if AItem = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskItems.GetPrevious - Name = ' + AItem.AttributeName + ' Type = ' + IntToHex(AItem.AttributeType,8));
 {$ENDIF}
 
 {Check Items}
 Current:=TNTFSDiskItem(First);
 while Current <> nil do
  begin
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskItems.GetPrevious - Comparing with Name = ' + Current.AttributeName + ' Type = ' + IntToHex(Current.AttributeType,8));
   {$ENDIF}
   
   if AItem.Compare(Current) = ntfsCompareGreater then Exit;
   
   Result:=Current;
   Current:=TNTFSDiskItem(Current.Next);
  end;
end;

{==============================================================================}

function TNTFSDiskItems.TotalSize:Int64;
var
 Item:TNTFSDiskItem;
begin
 {}
 Result:=0;
 
 Item:=TNTFSDiskItem(First);
 while Item <> nil do
  begin
   Inc(Result,Item.ItemSize);
   
   Item:=TNTFSDiskItem(Item.Next);
  end;
end;

{==============================================================================}

function TNTFSDiskItems.ItemCount:LongWord;
begin
 {}
 Result:=Count;
end;

{==============================================================================}
{==============================================================================}
{TNTFSDiskItem}
constructor TNTFSDiskItem.Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskAttribute);
begin
 {}
 inherited Create;
 FLocalLock:=ALocalLock;
 
 FStatus:=ntfsStatusNone;

 FItemSize:=0;
 FStartVCN:=0;
 FFileReference:=0;
 FAttributeType:=0;
 FAttributeId:=0;
 FAttributeName:=ntfsBlankName;

 FParent:=AParent;
 FAttribute:=nil;

 FAttributeHash:=0;
end;

{==============================================================================}

destructor TNTFSDiskItem.Destroy;
begin
 {}
 FParent:=nil;
 FAttribute:=nil;
 
 FLocalLock:=INVALID_HANDLE_VALUE;
 inherited Destroy;
end;

{==============================================================================}

function TNTFSDiskItem.AcquireLock:Boolean;
begin
 {}
 Result:=(MutexLock(FLocalLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSDiskItem.ReleaseLock:Boolean;
begin
 {}
 Result:=(MutexUnlock(FLocalLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSDiskItem.GetInvalid:Boolean;
begin
 {}
 Result:=((FStatus and ntfsStatusInvalid) = ntfsStatusInvalid);
end;

{==============================================================================}

procedure TNTFSDiskItem.SetInvalid(AValue:Boolean);
begin
 {}
 if AValue then
  begin
   FStatus:=(FStatus or ntfsStatusInvalid);
  end
 else
  begin
   FStatus:=(FStatus and not ntfsStatusInvalid);
  end;
end;

{==============================================================================}

function TNTFSDiskItem.GetAttributeName:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;
 
 Result:=FAttributeName;
 UniqueString(Result);
 
 ReleaseLock;
end;

{==============================================================================}

procedure TNTFSDiskItem.SetAttributeName(const AAttributeName:String);
begin
 {}
 if not AcquireLock then Exit;
 
 FAttributeName:=AAttributeName;
 UniqueString(FAttributeName);
 FAttributeHash:=GenerateNameHash(FAttributeName,NAME_HASH_SIZE);
 {Do not update ItemSize}
 
 ReleaseLock;
end;

{==============================================================================}

procedure TNTFSDiskItem.SetAttribute(AAttribute:TNTFSDiskAttribute);
begin
 {}
 if AAttribute = nil then Exit;
 if FAttribute = AAttribute then Exit;
 
 FAttribute:=AAttribute;
 FStartVCN:=FAttribute.StartVCN;
 FFileReference:=FAttribute.FileReference;
 FAttributeType:=FAttribute.AttributeType;
 FAttributeId:=FAttribute.AttributeId;
 AttributeName:=FAttribute.AttributeName;
 
 {Do not update ItemSize}
end;

{==============================================================================}

function TNTFSDiskItem.Compare(AItem:TNTFSDiskItem):Integer;
{Compare item by type, name and vcn for sort order}
{The passed item is the first value in a standard compare}
{Id is not used for sort order as it is unique per file record}
{Name is compared using a case insensitive compare method}
begin
 {}
 Result:=ntfsCompareGreater; {Default to greater}
 
 if AItem = nil then Exit;
 
 {Check Attribute Type}
 if AItem.AttributeType < FAttributeType then
  begin
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskItem.Compare - Type ' + IntToHex(AItem.AttributeType,8) + ' < ' + IntToHex(FAttributeType,8));
   {$ENDIF}
   
   Result:=ntfsCompareLess;
  end
 else if AItem.AttributeType > FAttributeType then
  begin
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskItem.Compare - Type ' + IntToHex(AItem.AttributeType,8) + ' > ' + IntToHex(FAttributeType,8));
   {$ENDIF}
   
   Result:=ntfsCompareGreater;
  end
 else
  begin
   {Check Attribute Name}
   Result:=CompareText(AItem.AttributeName,AttributeName); //To Do //Windows Locale ?
   if Result < ntfsCompareEqual then
    begin
     {$IFDEF NTFS_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskItem.Compare - Name ' + AItem.AttributeName + ' < ' + AttributeName);
     {$ENDIF}
     
     Result:=ntfsCompareLess;
    end
   else if Result > ntfsCompareEqual then
    begin
     {$IFDEF NTFS_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskItem.Compare - Name ' + AItem.AttributeName + ' > ' + AttributeName);
     {$ENDIF}
     
     Result:=ntfsCompareGreater;
    end
   else
    begin
     {Check Start VCN}
     if AItem.StartVCN < FStartVCN then
      begin
       {$IFDEF NTFS_DEBUG}
       if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskItem.Compare - StartVCN ' + IntToHex(AItem.StartVCN,16) + ' < ' + IntToHex(FStartVCN,16));
       {$ENDIF}
       
       Result:=ntfsCompareLess;
      end
     else if AItem.StartVCN > FStartVCN then
      begin
       {$IFDEF NTFS_DEBUG}
       if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskItem.Compare - StartVCN ' + IntToHex(AItem.StartVCN,16) + ' > ' + IntToHex(FStartVCN,16));
       {$ENDIF}
       
       Result:=ntfsCompareGreater;
      end;
    end;
  end;
end;

{==============================================================================}

function TNTFSDiskItem.RecordNumber:Int64;
begin
 {}
 TNTFSFileReference(Result).RecordNumber:=TNTFSFileReference(FFileReference).RecordNumber;
 TNTFSFileReference(Result).RecordSegment:=TNTFSFileReference(FFileReference).RecordSegment;
 TNTFSFileReference(Result).SequenceNumber:=0;
end;

{==============================================================================}

function TNTFSDiskItem.AttributeNameSize:Word;
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;
 
 Result:=Length(FAttributeName) shl 1; {Multiply by SizeOf(WideChar) / SizeOf(Word)}
 
 ReleaseLock;
end;

{==============================================================================}

function TNTFSDiskItem.AttributeNameLength:Byte;
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;
 
 Result:=Length(FAttributeName);
 
 ReleaseLock;
end;

{==============================================================================}

function TNTFSDiskItem.AttributeNameOffset:Byte;
begin
 {}
 Result:=ntfsItemSize;
end;

{==============================================================================}

function TNTFSDiskItem.UpdateItem:Boolean;
begin
 {}
 Result:=False;
 
 if FAttribute = nil then Exit;
 
 FStartVCN:=FAttribute.StartVCN;
 FFileReference:=FAttribute.FileReference;
 FAttributeType:=FAttribute.AttributeType;
 FAttributeId:=FAttribute.AttributeId;
 AttributeName:=FAttribute.AttributeName;

 {Do not update ItemSize}
 
 Result:=True;
end;

{==============================================================================}

function TNTFSDiskItem.CalculatedSize(AVersion:Word):Word;
{Calculated Size includes any rounding required for alignment}
begin
 {}
 Result:=NTFSRoundWordTo8Bytes(ntfsItemSize + AttributeNameSize);
end;

{==============================================================================}

function TNTFSDiskItem.ReadItem(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Read the item data from the supplied buffer at the supplied offset}
var
 ItemData:PNTFSItemData;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {Get Item}
 ItemData:=PNTFSItemData(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if ASize >= ntfsItemSize then
  begin
   {Read Item}
   FItemSize:=ItemData.ItemSize;
   FStartVCN:=ItemData.StartVCN;
   FFileReference:=ItemData.FileReference;
   FAttributeType:=ItemData.AttributeType;
   FAttributeId:=ItemData.AttributeId;
   
   {Read Name}
   AttributeName:=NTFSWideBufferToString(ItemData,ItemData.AttributeNameOffset,ItemData.AttributeNameLength);
   
   {Update Offset}
   Dec(ASize,ItemData.ItemSize);
   Inc(AOffset,ItemData.ItemSize);
   
   Result:=True;
  end;
end;

{==============================================================================}

function TNTFSDiskItem.WriteItem(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Write the item data to the supplied buffer at the supplied offset}
var
 ItemData:PNTFSItemData;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {Get Item}
 ItemData:=PNTFSItemData(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if ASize >= ntfsItemSize then
  begin
   {Write Item}
   ItemData.ItemSize:=FItemSize;
   ItemData.StartVCN:=FStartVCN;
   ItemData.FileReference:=FFileReference;
   ItemData.AttributeType:=FAttributeType;
   ItemData.AttributeId:=FAttributeId;
   
   {Write Name}
   ItemData.AttributeNameLength:=AttributeNameLength;
   ItemData.AttributeNameOffset:=AttributeNameOffset;
   if not NTFSStringToWideBuffer(AttributeName,ItemData,ItemData.AttributeNameOffset,ItemData.AttributeNameLength) then Exit;
   
   {Update Offset}
   Dec(ASize,ItemData.ItemSize);
   Inc(AOffset,ItemData.ItemSize);
   
   Result:=True;
  end;
end;

{==============================================================================}
{==============================================================================}
{TNTFSDiskAttributes}
constructor TNTFSDiskAttributes.Create(AAttributeLocal:TMutexHandle;ALock:TSynchronizerHandle);
begin
 {}
 inherited Create(ALock);
 FAttributeLocal:=AAttributeLocal;
end;

{==============================================================================}

destructor TNTFSDiskAttributes.Destroy; 
begin
 {}
 WriterLock;
 try
  FAttributeLocal:=INVALID_HANDLE_VALUE;
 finally
  WriterUnlock;
  inherited Destroy;
 end;
end;

{==============================================================================}

function TNTFSDiskAttributes.GetPrevious(AAttribute:TNTFSDiskAttribute):TNTFSDiskAttribute;
{Note: Caller must hold the lock}
var
 Current:TNTFSDiskAttribute;
begin
 {}
 Result:=nil;
 
 if AAttribute = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskAttributes.GetPrevious - Name = ' + AAttribute.AttributeName + ' Type = ' + IntToHex(AAttribute.AttributeType,8));
 {$ENDIF}
 
 {Check Attributes}
 Current:=TNTFSDiskAttribute(First);
 while Current <> nil do
  begin
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskAttributes.GetPrevious - Comparing with Name = ' + Current.AttributeName + ' Type = ' + IntToHex(Current.AttributeType,8));
   {$ENDIF}
   
   if AAttribute.Compare(Current) = ntfsCompareGreater then Exit;
   
   Result:=Current;
   Current:=TNTFSDiskAttribute(Current.Next);
  end;
end;

{==============================================================================}

function TNTFSDiskAttributes.TotalSize:LongWord;
{Note: Caller must hold the lock}
var
 Attribute:TNTFSDiskAttribute;
begin
 {}
 Result:=0;
 
 Attribute:=TNTFSDiskAttribute(First);
 while Attribute <> nil do
  begin
   Inc(Result,Attribute.AttributeSize);
   
   Attribute:=TNTFSDiskAttribute(Attribute.Next);
  end;
end;

{==============================================================================}

function TNTFSDiskAttributes.AttributeCount:LongWord;
{Note: Caller must hold the lock}
begin
 {}
 Result:=Count;
end;

{==============================================================================}
{==============================================================================}
{TNTFSDiskAttribute}
constructor TNTFSDiskAttribute.Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
begin
 {}
 inherited Create;
 FLocalLock:=ALocalLock;
 
 FStatus:=ntfsStatusNone;
 FData:=nil;

 FAttributeType:=ntfsAttrTypeNone;
 FAttributeSize:=0;
 FAttributeFlags:=ntfsAttributeFlagNone;
 FAttributeId:=0;
 FAttributeName:=ntfsBlankName;
 FNonResident:=ntfsAttributeResident;

 FIndexed:=ntfsAttributeNonIndexed;
 FDataSize:=0;

 FStartVCN:=0;
 FLastVCN:=ntfsUnknownCluster;
 FStreamSize:=0;
 FStreamUsed:=0;
 FStreamAllocated:=0;
 FInitializedSize:=0;
 FCompressionUnit:=0;

 FRuns:=nil;
 FItems:=nil;
 FIndex:=nil;
 FParent:=AParent;

 FAttributeHash:=0;
end;

{==============================================================================}

destructor TNTFSDiskAttribute.Destroy;
begin
 {}
 if FData <> nil then FreeMem(FData);
 if FRuns <> nil then FRuns.Free;
 if FItems <> nil then FItems.Free;
 if FIndex <> nil then FIndex.Free;
 FParent:=nil;
 
 FLocalLock:=INVALID_HANDLE_VALUE;
 inherited Destroy;
end;

{==============================================================================}

function TNTFSDiskAttribute.AcquireLock:Boolean;
begin
 {}
 Result:=(MutexLock(FLocalLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSDiskAttribute.ReleaseLock:Boolean;
begin
 {}
 Result:=(MutexUnlock(FLocalLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSDiskAttribute.GetRunsLock:TSynchronizerHandle;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;
 
 if FParent = nil then Exit;
 
 Result:=FParent.FRunsLock;
end;

{==============================================================================}

function TNTFSDiskAttribute.GetItemsLock:TSynchronizerHandle;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;
 
 if FParent = nil then Exit;
 
 Result:=FParent.FItemsLock;
end;

{==============================================================================}

function TNTFSDiskAttribute.GetIndexLock:TSynchronizerHandle;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;
 
 if FParent = nil then Exit;
 
 Result:=FParent.FIndexLock;
end;

{==============================================================================}

function TNTFSDiskAttribute.GetExtendedsLock:TSynchronizerHandle;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;
 
 if FParent = nil then Exit;
 
 Result:=FParent.FExtendedsLock;
end;

{==============================================================================}

function TNTFSDiskAttribute.GetRunLocal:TMutexHandle;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;
 
 if FParent = nil then Exit;
 
 Result:=FParent.FRunLocal;
end;

{==============================================================================}

function TNTFSDiskAttribute.GetItemLocal:TMutexHandle;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;
 
 if FParent = nil then Exit;
 
 Result:=FParent.FItemLocal;
end;

{==============================================================================}

function TNTFSDiskAttribute.GetKeyLocal:TMutexHandle;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;
 
 if FParent = nil then Exit;
 
 Result:=FParent.FKeyLocal;
end;

{==============================================================================}

function TNTFSDiskAttribute.GetExtendedLocal:TMutexHandle;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;
 
 if FParent = nil then Exit;
 
 Result:=FParent.FExtendedLocal;
end;

{==============================================================================}

function TNTFSDiskAttribute.GetUpdating:Boolean;
begin
 {}
 Result:=((FStatus and ntfsStatusUpdating) = ntfsStatusUpdating);
end;

{==============================================================================}

procedure TNTFSDiskAttribute.SetUpdating(AValue:Boolean);
begin
 {}
 if AValue then
  begin
   FStatus:=(FStatus or ntfsStatusUpdating);
  end
 else
  begin
   FStatus:=(FStatus and not ntfsStatusUpdating);
  end;
end;

{==============================================================================}

function TNTFSDiskAttribute.GetIsFixed:Boolean;
begin
 {}
 Result:=((FStatus and ntfsStatusFixed) = ntfsStatusFixed);
end;

{==============================================================================}

function TNTFSDiskAttribute.GetIsSingle:Boolean;
begin
 {}
 Result:=((FStatus and ntfsStatusSingle) = ntfsStatusSingle);
end;

{==============================================================================}

function TNTFSDiskAttribute.GetIsUnlisted:Boolean;
begin
 {}
 Result:=((FStatus and ntfsStatusUnlisted) = ntfsStatusUnlisted);
end;

{==============================================================================}

function TNTFSDiskAttribute.GetIsUnmovable:Boolean;
begin
 {}
 Result:=((FStatus and ntfsStatusUnmovable) = ntfsStatusUnmovable);
end;

{==============================================================================}

function TNTFSDiskAttribute.GetIsManaged:Boolean;
begin
 {}
 Result:=((FStatus and ntfsStatusManaged) = ntfsStatusManaged);
end;

{==============================================================================}

function TNTFSDiskAttribute.GetIsSparse:Boolean;
begin
 {}
 Result:=((FAttributeFlags and ntfsAttributeFlagSparse) = ntfsAttributeFlagSparse);
end;

{==============================================================================}

procedure TNTFSDiskAttribute.SetIsSparse(AValue:Boolean);
begin
 {}
 if AValue then
  begin
   FAttributeFlags:=(FAttributeFlags or ntfsAttributeFlagSparse);
  end
 else
  begin
   FAttributeFlags:=(FAttributeFlags and not ntfsAttributeFlagSparse);
  end;
end;

{==============================================================================}

function TNTFSDiskAttribute.GetIsEncrypted:Boolean;
begin
 {}
 Result:=((FAttributeFlags and ntfsAttributeFlagEncrypted) = ntfsAttributeFlagEncrypted);
end;

{==============================================================================}

procedure TNTFSDiskAttribute.SetIsEncrypted(AValue:Boolean);
begin
 {}
 if AValue then
  begin
   FAttributeFlags:=(FAttributeFlags or ntfsAttributeFlagEncrypted);
  end
 else
  begin
   FAttributeFlags:=(FAttributeFlags and not ntfsAttributeFlagEncrypted);
  end;
end;

{==============================================================================}

function TNTFSDiskAttribute.GetIsCompressed:Boolean;
begin
 {}
 Result:=((FAttributeFlags and ntfsAttributeFlagCompressed) = ntfsAttributeFlagCompressed);
end;

{==============================================================================}

procedure TNTFSDiskAttribute.SetIsCompressed(AValue:Boolean);
begin
 {}
 if AValue then
  begin
   FAttributeFlags:=(FAttributeFlags or ntfsAttributeFlagCompressed);
  end
 else
  begin
   FAttributeFlags:=(FAttributeFlags and not ntfsAttributeFlagCompressed);
  end;
end;

{==============================================================================}

function TNTFSDiskAttribute.GetAttributeName:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;
 
 Result:=FAttributeName;
 UniqueString(Result);
 
 ReleaseLock;
end;

{==============================================================================}

procedure TNTFSDiskAttribute.SetAttributeName(const AAttributeName:String);
begin
 {}
 if not AcquireLock then Exit;
 
 FAttributeName:=AAttributeName;
 UniqueString(FAttributeName);
 FAttributeHash:=GenerateNameHash(FAttributeName,NAME_HASH_SIZE);
 {Do not update AttributeSize}
 
 ReleaseLock;
end;

{==============================================================================}

function TNTFSDiskAttribute.GetDataSize:LongWord;
begin
 {Virtual Base}
 Result:=FDataSize;
end;

{==============================================================================}

procedure TNTFSDiskAttribute.SetDataSize(AValue:LongWord);
begin
 {Virtual Base}
 FDataSize:=AValue;
end;

{==============================================================================}

function TNTFSDiskAttribute.Compare(AAttribute:TNTFSDiskAttribute):Integer;
{Compare attribute by type, name and vcn for sort order}
{The passed attribute is the first value in a standard compare}
{Id is not used for sort order as it is unique per file record}
{Name is compared using a case insensitive compare method}
begin
 {}
 Result:=ntfsCompareGreater; {Default to greater}
 
 if AAttribute = nil then Exit;
 
 {Check Attribute Type}
 if AAttribute.AttributeType < FAttributeType then
  begin
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskAttribute.Compare - Type ' + IntToHex(AAttribute.AttributeType,8) + ' < ' + IntToHex(FAttributeType,8));
   {$ENDIF}
   
   Result:=ntfsCompareLess;
  end
 else if AAttribute.AttributeType > FAttributeType then
  begin
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskAttribute.Compare - Type ' + IntToHex(AAttribute.AttributeType,8) + ' > ' + IntToHex(FAttributeType,8));
   {$ENDIF}
   
   Result:=ntfsCompareGreater;
  end
 else
  begin
   {Check Attribute Name}
   Result:=CompareText(AAttribute.AttributeName,AttributeName); //To Do //Windows Locale ?
   if Result < ntfsCompareEqual then
    begin
     {$IFDEF NTFS_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskAttribute.Compare - Name ' + AAttribute.AttributeName + ' < ' + AttributeName);
     {$ENDIF}
     
     Result:=ntfsCompareLess;
    end
   else if Result > ntfsCompareEqual then
    begin
     {$IFDEF NTFS_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskAttribute.Compare - Name ' + AAttribute.AttributeName + ' > ' + AttributeName);
     {$ENDIF}
     
     Result:=ntfsCompareGreater;
    end
   else
    begin
     {Check Start VCN}
     if AAttribute.StartVCN < FStartVCN then
      begin
       {$IFDEF NTFS_DEBUG}
       if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskAttribute.Compare - StartVCN ' + IntToHex(AAttribute.StartVCN,16) + ' < ' + IntToHex(FStartVCN,16));
       {$ENDIF}
       
       Result:=ntfsCompareLess;
      end
     else if AAttribute.StartVCN > FStartVCN then
      begin
       {$IFDEF NTFS_DEBUG}
       if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskAttribute.Compare - StartVCN ' + IntToHex(AAttribute.StartVCN,16) + ' > ' + IntToHex(FStartVCN,16));
       {$ENDIF}
       
       Result:=ntfsCompareGreater;
      end;
    end;
  end;
end;

{==============================================================================}

function TNTFSDiskAttribute.CreateRuns(ANew:Boolean):TNTFSDiskRuns;
var
 Run:TNTFSDiskRun;
begin
 {Check Runs}
 if FRuns = nil then
  begin
   {Create Runs}
   FRuns:=TNTFSDiskRuns.Create(GetRunLocal,GetRunsLock);
   
   {Check New}
   if ANew then
    begin
     Result:=nil;
     
     {Add Last}
     Run:=CreateRun(False);
     if Run = nil then Exit;
     Run.Start:=ntfsUnknownCluster;
     Run.Length:=0;
    end;
  end;
  
 Result:=FRuns;
end;

{==============================================================================}

function TNTFSDiskAttribute.CreateItems(ANew:Boolean):TNTFSDiskItems;
begin
 {Check Items}
 if FItems = nil then
  begin
   {Create Items}
   FItems:=TNTFSDiskItems.Create(GetItemLocal,GetItemsLock);
   
   {Check New}
   if ANew then
    begin
     {Nothing}
    end;
  end;
  
 Result:=FItems;
end;

{==============================================================================}

function TNTFSDiskAttribute.GetRecord(AInstance:Integer):TNTFSDiskRecord;
{Used by TNTFSDiskRecord.GetRecord methods to enumerate records within a list}
var
 Count:Integer;
 Item:TNTFSDiskItem;
 Previous:TNTFSDiskRecord;
begin
 {}
 Result:=nil;
 
 if FItems = nil then Exit;
 
 Count:=0;
 Previous:=nil;
 
 {Check Items}
 Item:=TNTFSDiskItem(FItems.First);
 while Item <> nil do
  begin
   {Check Attribute}
   if Item.Attribute <> nil then
    begin
     {Check Record}
     if Item.Attribute.Parent <> nil then
      begin
       Inc(Count);
       if AInstance = ntfsInstanceLast then
        begin
         Previous:=Item.Attribute.Parent;
        end
       else
        begin
         if (AInstance = ntfsInstanceFirst) or (Count = AInstance) then  {Instance 0 equals first}
          begin
           Result:=Item.Attribute.Parent;
           Exit;
          end;
        end;
      end;
    end;
    
   Item:=TNTFSDiskItem(Item.Next);
  end;
  
 {Get Last}
 if AInstance = ntfsInstanceLast then Result:=Previous;
end;

{==============================================================================}

function TNTFSDiskAttribute.GetAttribute(AType:LongWord;const AName:String;AInstance:Integer):TNTFSDiskAttribute;
{Used by TNTFSDiskRecord.GetAttribute methods to enumerate attributes within a list}
var
 Hash:LongWord;
 Count:Integer;
 Wildcard:Boolean;
 Item:TNTFSDiskItem;
 Previous:TNTFSDiskAttribute;
begin
 {}
 Result:=nil;
 
 if FItems = nil then Exit;
 
 Count:=0;
 Previous:=nil;
 
 {Calculate Hash}
 Hash:=0;                                                          //To Do //Testing4
 Wildcard:=(Length(AName) = ntfsAnyNameLength) and (AName = ntfsAnyName); {Modified 14/2/2011}
 if not Wildcard then Hash:=GenerateNameHash(AName,NAME_HASH_SIZE); //To Do //Testing4
 
 {Check Items}
 Item:=TNTFSDiskItem(FItems.First);
 while Item <> nil do
  begin
   {Check Attribute}
   if Item.Attribute <> nil then
    begin
     {Check Type}
     if (AType = ntfsAttrTypeAny) or (Item.Attribute.AttributeType = AType) then
     {if Item.Attribute.AttributeType = AType then}
      begin
       if Wildcard or (Item.Attribute.AttributeHash = Hash) then
        begin
         if Wildcard or (Uppercase(Item.Attribute.AttributeName) = Uppercase(AName)) then
          begin
           Inc(Count);
           if AInstance = ntfsInstanceLast then
            begin
             Previous:=Item.Attribute;
            end
           else
            begin
             if (AInstance = ntfsInstanceFirst) or (Count = AInstance) then  {Instance 0 equals first}
              begin
               Result:=Item.Attribute;
               Exit;
              end;
            end;
          end;
        end;
      end;
    end;
    
   Item:=TNTFSDiskItem(Item.Next);
  end;
 
 {Get Last}
 if AInstance = ntfsInstanceLast then Result:=Previous;
end;

{==============================================================================}

function TNTFSDiskAttribute.GetRunCount(const AVCN:Int64;var AStartVCN,ACount:Int64):Boolean;
{Get the count of clusters from the supplied VCN to the end of the attributes runs}
{Note: StartVCN will be the start of the run containing the VCN}
var
 Run:TNTFSDiskRun;
begin
 {}
 Result:=False;
 
 if FRuns = nil then Exit;

 {Get Run}
 Run:=GetRun(AVCN,AStartVCN);
 if Run = nil then Exit;
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

{==============================================================================}

function TNTFSDiskAttribute.GetRunLength(const AVCN:Int64;var AStartVCN,ALength:Int64):Boolean;
{Get the count of clusters from the supplied VCN to the end of run containing the VCN}
{Note: StartVCN will be the start of the run containing the VCN}
var
 Run:TNTFSDiskRun;
begin
 {}
 Result:=False;
 
 if FRuns = nil then Exit;

 {Get Run}
 Run:=GetRun(AVCN,AStartVCN);
 if Run = nil then Exit;
 if Run.IsLast then Exit;
 
 {Get Length}
 ALength:=(Run.Length - (AVCN - StartVCN));
 
 Result:=True;
end;

{==============================================================================}

function TNTFSDiskAttribute.GetRunByUnit(const AUnit:Int64;var AStartVCN:Int64):TNTFSDiskRun;
{Get the run containing the supplied compression unit}
{Note: StartVCN will be the start of the run, not neccessarily the start of the unit}
{Note: This will only work on the first instance}
var
 VCN:Int64;
begin
 {}
 Result:=nil;
 
 if FRuns = nil then Exit;

 {Check Resident}
 if FNonResident = ntfsAttributeNonResident then
  begin
   {Check Compressed}
   if (IsCompressed) and (FCompressionUnit <> 0) then
    begin
     {Get Unit VCN}
     VCN:=(AUnit shl FCompressionUnit);
     
     {Get Run by VCN}
     Result:=GetRun(VCN,AStartVCN);
    end;
  end;
end;

{==============================================================================}

function TNTFSDiskAttribute.GetRunByCluster(const ACluster:Int64;var AStartVCN:Int64):TNTFSDiskRun;
{Get the run containing the supplied cluster}
{Note: Not Required for FileSystem, may be used by Defragger}
var
 CurrentVCN:Int64;
 Run:TNTFSDiskRun;
begin
 {}
 Result:=nil;
 
 if FRuns = nil then Exit;

 CurrentVCN:=FStartVCN; {Allow for multiple instances}
 
 {Check Runs}
 Run:=TNTFSDiskRun(FRuns.First);
 while Run <> nil do
  begin
   if not(Run.IsSparse) and not(Run.IsLast) then
    begin
     if (ACluster >= Run.Start) and (ACluster < (Run.Start + Run.Length)) then
      begin
       AStartVCN:=CurrentVCN;
       Result:=Run;
       Exit;
      end;
    end;
    
   Inc(CurrentVCN,Run.Length);
   
   Run:=TNTFSDiskRun(Run.Next);
  end;
end;

{==============================================================================}

function TNTFSDiskAttribute.CreateRun(ANew:Boolean):TNTFSDiskRun;
{Create a run, add to list do not update attribute}
var
 Run:TNTFSDiskRun;
begin
 {}
 Result:=nil;
 
 if FRuns = nil then CreateRuns(ANew);
 
 {Create Run}
 Run:=TNTFSDiskRun.Create(GetRunLocal,Self);
 
 {Update Recent (Only if not new)}
 if not ANew then FRuns.RecentVCN:=ntfsUnknownCluster;  //To Do //Testing6
 if not ANew then FRuns.Recent:=nil;                    //To Do //Testing6
 
 {Add Run (Only if not new)}
 if not ANew then FRuns.Add(Run);
 
 Result:=Run;
end;

{==============================================================================}

function TNTFSDiskAttribute.DestroyRun(ARun:TNTFSDiskRun):Boolean;
{Remove the run from the list and free, do not update attribute}
begin
 {}
 Result:=False;
 
 if ARun = nil then Exit;
 if FRuns = nil then Exit;
 
 {Update Recent}
 FRuns.RecentVCN:=ntfsUnknownCluster;  //To Do //Testing6
 FRuns.Recent:=nil;                    //To Do //Testing6
 
 {Remove Run}
 FRuns.Remove(ARun);
 
 {Free Run}
 ARun.Free;
 
 Result:=True;
end;

{==============================================================================}

function TNTFSDiskAttribute.NewRun(const AStart,ALength:Int64):TNTFSDiskRun;
{Create a run, insert in list and update attribute}
begin
 {Create Run}
 Result:=CreateRun(True);
 if Result = nil then Exit;
 
 {Update Recent}
 FRuns.RecentVCN:=ntfsUnknownCluster;  //To Do //Testing6
 FRuns.Recent:=nil;                    //To Do //Testing6
 
 {Insert Run}
 FRuns.Insert(FRuns.GetFinal,Result);
 
 {Set Values}
 Result.Start:=AStart;
 Result.Length:=ALength;
 
 {Update Attribute}
 if FLastVCN = ntfsUnknownCluster then FLastVCN:=FStartVCN + (Result.Length - 1) else Inc(FLastVCN,Result.Length);
end;

{==============================================================================}

function TNTFSDiskAttribute.InsertRun(APrev:TNTFSDiskRun;const AStart,ALength:Int64):TNTFSDiskRun;
{Create a run, insert in list and update attribute}
begin
 {}
 Result:=nil;
 
 if APrev = nil then Exit;
 
 {Create Run}
 Result:=CreateRun(True);
 if Result = nil then Exit;
 
 {Update Recent}
 FRuns.RecentVCN:=ntfsUnknownCluster;  //To Do //Testing6
 FRuns.Recent:=nil;                    //To Do //Testing6
 
 {Insert Run}
 FRuns.Insert(APrev,Result);
 
 {Set Values}
 Result.Start:=AStart;
 Result.Length:=ALength;
 
 {Update Attribute}
 if FLastVCN = ntfsUnknownCluster then FLastVCN:=FStartVCN + (Result.Length - 1) else Inc(FLastVCN,Result.Length);
 
 {Update Runs}
 UpdateRun(Result.GetSuccessor);
end;

{==============================================================================}

function TNTFSDiskAttribute.GetRun(const AVCN:Int64;var AStartVCN:Int64):TNTFSDiskRun;
{Get the run containing the supplied VCN}
var
 CurrentVCN:Int64;
 Run:TNTFSDiskRun;
 Start:TNTFSDiskRun;
begin
 {}
 Result:=nil;
 
 if FRuns = nil then Exit;

 {Check Recent} //To Do //Testing6
 if (FRuns.Recent <> nil) and (AVCN >= FRuns.RecentVCN) then  //To Do //Testing8
  begin
   CurrentVCN:=FRuns.RecentVCN; {Allow for multiple instances}
   
   {Check Runs}
   Run:=FRuns.Recent;           {Start at Previous not Next}
   Start:=FRuns.Recent;         //To Do //Only read once, see TDiskEntry.Recent handling in FileSystem
   while Run <> nil do
    begin
     if (AVCN >= CurrentVCN) and (AVCN < (CurrentVCN + Run.Length)) then
      begin
       FRuns.RecentVCN:=CurrentVCN;
       AStartVCN:=CurrentVCN;
       FRuns.Recent:=Run;
       Result:=Run;
       Exit;
      end;
      
     Inc(CurrentVCN,Run.Length);
     
     Run:=TNTFSDiskRun(Run.Next);
     
     if (Start <> nil) and (Run = nil) then CurrentVCN:=FStartVCN; {Allow for multiple instances}
     if (Start <> nil) and (Run = nil) then Run:=TNTFSDiskRun(FRuns.First); {Start again from First}
     if (Start <> nil) and (Run = Start) then Break; {Break if returned to Start}
    end;
  end
 else
  begin
   CurrentVCN:=FStartVCN; {Allow for multiple instances}
   
   {Check Runs}
   Run:=TNTFSDiskRun(FRuns.First);
   while Run <> nil do
    begin
     if (AVCN >= CurrentVCN) and (AVCN < (CurrentVCN + Run.Length)) then
      begin
       FRuns.RecentVCN:=CurrentVCN;
       AStartVCN:=CurrentVCN;
       FRuns.Recent:=Run;
       Result:=Run;
       Exit;
      end;
      
     Inc(CurrentVCN,Run.Length);
     
     Run:=TNTFSDiskRun(Run.Next);
    end;
  end;          //To Do //Testing6
end;

{==============================================================================}
 //To Do //Remove ?
function TNTFSDiskAttribute.GetRunOld(const AVCN:Int64;var AStartVCN:Int64):TNTFSDiskRun;
{Get the run containing the supplied VCN}
var
 CurrentVCN:Int64;
 Run:TNTFSDiskRun;
begin
 {}
 Result:=nil;
 
 if FRuns = nil then Exit;

 CurrentVCN:=FStartVCN; {Allow for multiple instances}
 
 {Check Runs}
 Run:=TNTFSDiskRun(FRuns.First);
 while Run <> nil do
  begin
   if (AVCN >= CurrentVCN) and (AVCN < (CurrentVCN + Run.Length)) then
    begin
     AStartVCN:=CurrentVCN;
     Result:=Run;
     Exit;
    end;
    
   Inc(CurrentVCN,Run.Length);
   
   Run:=TNTFSDiskRun(Run.Next);
  end;
end;

{==============================================================================}

function TNTFSDiskAttribute.MergeRun(ARun:TNTFSDiskRun):Boolean;
{Merge the run with the next run in the list and delete the merged run}
{If supplied run is normal then next run must be contiguous or merge will fail}
{If supplied run is sparse then next run must also be sparse or merge will fail}
var
 Run:TNTFSDiskRun;
begin
 {}
 Result:=False;
 
 if ARun = nil then Exit;
 if FRuns = nil then Exit;

 {Get Next} {Not Successor}
 Run:=TNTFSDiskRun(ARun.Next);
 if Run = nil then Exit;
 if Run.IsLast then Exit;
 
 {Check Last}
 if ARun.IsLast then Exit;
 
 {Check Sparse}
 if ARun.IsSparse then
  begin
   {Sparse Run}
   {Check Sparse}
   if not Run.IsSparse then Exit;
   
   {Update Run}
   ARun.Length:=ARun.Length + Run.Length;
   
   {Update Recent}
   FRuns.RecentVCN:=ntfsUnknownCluster;  //To Do //Testing6
   FRuns.Recent:=nil;                    //To Do //Testing6
   
   {Remove Run}
   FRuns.Remove(Run);
   
   {Free Run}
   Run.Free;
   
   Result:=True;
  end
 else
  begin
   {Normal Run}
   {Check Sparse}
   if Run.IsSparse then Exit;
   
   {Check Start}
   if Run.Start <> (ARun.Start + ARun.Length) then Exit;
   
   {Update Run}
   ARun.Length:=ARun.Length + Run.Length;
   
   {Update Recent}
   FRuns.RecentVCN:=ntfsUnknownCluster;  //To Do //Testing6
   FRuns.Recent:=nil;                    //To Do //Testing6
   
   {Remove Run}
   FRuns.Remove(Run);
   
   {Free Run}
   Run.Free;
   
   {Update Runs}
   UpdateRun(ARun.GetSuccessor);
   
   Result:=True;
  end;
end;

{==============================================================================}

function TNTFSDiskAttribute.SplitRun(ARun:TNTFSDiskRun;const ALength:Int64):Boolean;
{Split the run at the supplied length and insert a new run in the list}
{New run will be inserted as the next run following the supplied run}
{Supplied run can be either normal or sparse}
var
 Start:Int64;
 Length:Int64;
 Run:TNTFSDiskRun;
begin
 {}
 Result:=False;
 
 if ARun = nil then Exit;
 if FRuns = nil then Exit;

 {Check Last}
 if ARun.IsLast then Exit;
 
 {Check Sparse}
 if ARun.IsSparse then
  begin
   {Check Length}
   if ALength >= ARun.Length then Exit;
   
   {Get Length}
   Length:=(ARun.Length - ALength);
   
   {Update Run}
   ARun.Length:=ALength;
   
   {Update Attribute}
   if (FLastVCN - Length) < FStartVCN then FLastVCN:=ntfsUnknownCluster else Dec(FLastVCN,Length);
   
   {Create Run}
   Run:=InsertRun(ARun,ntfsUnknownCluster,Length);
   if Run = nil then Exit;
   
   Result:=True;
  end
 else
  begin
   {Check Length}
   if ALength >= ARun.Length then Exit;
   
   {Get Length}
   Length:=(ARun.Length - ALength);
   
   {Get Start}
   Start:=(ARun.Start + ALength);
   
   {Update Run}
   ARun.Length:=ALength;
   
   {Update Attribute}
   if (FLastVCN - Length) < FStartVCN then FLastVCN:=ntfsUnknownCluster else Dec(FLastVCN,Length);
   
   {Create Run}
   Run:=InsertRun(ARun,Start,Length);
   if Run = nil then Exit;
   
   Result:=True;
  end;
end;

{==============================================================================}

function TNTFSDiskAttribute.RemoveRun(ARun:TNTFSDiskRun):Boolean;
{Remove the run from the list, free and update attribute}
{Run must be the last actual run (not Last)}
begin
 {}
 Result:=False;
 
 if ARun = nil then Exit;
 if FRuns = nil then Exit;

 {Check Last}
 if ARun.IsLast then Exit;
 
 {Update Attribute}
 if (FLastVCN - ARun.Length) < FStartVCN then FLastVCN:=ntfsUnknownCluster else Dec(FLastVCN,ARun.Length);
 
 {Update Recent}
 FRuns.RecentVCN:=ntfsUnknownCluster;  //To Do //Testing6
 FRuns.Recent:=nil;                    //To Do //Testing6
 
 {Remove Run}
 FRuns.Remove(ARun);
 
 {Free Run}
 ARun.Free;
 
 Result:=True;
end;

{==============================================================================}

function TNTFSDiskAttribute.MoveRun(ADest:TNTFSDiskAttribute;ARun:TNTFSDiskRun):Boolean;
{Remove the run from the list and insert in the list of passed attribute}
{Run must be the last actual run (not Last)}
{Run will be inserted first in the destination}
begin
 {}
 Result:=False;
 
 if ARun = nil then Exit;
 if ADest = nil then Exit;
 if FRuns = nil then Exit;
 if ADest.FRuns = nil then ADest.CreateRuns(True);

 {Check Last}
 if ARun.IsLast then Exit;
 
 {Update Attribute}
 if (FLastVCN - ARun.Length) < FStartVCN then FLastVCN:=ntfsUnknownCluster else Dec(FLastVCN,ARun.Length);
 
 {Update Recent}
 FRuns.RecentVCN:=ntfsUnknownCluster;  //To Do //Testing6
 FRuns.Recent:=nil;                    //To Do //Testing6
 
 {Remove Run}
 FRuns.Remove(ARun);
 
 {Update Recent}
 ADest.FRuns.RecentVCN:=ntfsUnknownCluster;  //To Do //Testing6
 ADest.FRuns.Recent:=nil;                    //To Do //Testing6
 
 {Insert Run}
 ADest.FRuns.Insert(nil,ARun);
 
 {Update Attribute}
 ADest.FStartVCN:=FLastVCN + 1;
 if ADest.FLastVCN = ntfsUnknownCluster then ADest.FLastVCN:=ADest.FStartVCN + (ARun.Length - 1); {Do not increment unless first}
 
 {Update Runs}
 ADest.UpdateRun(ARun);
 
 Result:=True;
end;

{==============================================================================}

function TNTFSDiskAttribute.GetItemByAttribute(AAttribute:TNTFSDiskAttribute):TNTFSDiskItem;
{Get the item which references the supplied attribute}
var
 Item:TNTFSDiskItem;
begin
 {}
 Result:=nil;
 
 if FItems = nil then Exit;
 if AAttribute = nil then Exit;
 
 {Check Items}
 Item:=TNTFSDiskItem(FItems.First);
 while Item <> nil do
  begin
   if Item.Attribute = AAttribute then
    begin
     Result:=Item;
     Exit;
    end;
    
   Item:=TNTFSDiskItem(Item.Next);
  end;
end;

{==============================================================================}

function TNTFSDiskAttribute.CreateItem(ANew:Boolean):TNTFSDiskItem;
{Create an item, add to end of list do not update attribute}
var
 Item:TNTFSDiskItem;
begin
 {}
 Result:=nil;
 
 if FItems = nil then CreateItems(ANew);
 
 {Create Item}
 Item:=TNTFSDiskItem.Create(GetItemLocal,Self);
 
 {Add Item  (Only if not new)}
 if not ANew then FItems.Add(Item);
 
 Result:=Item;
end;

{==============================================================================}

function TNTFSDiskAttribute.DestroyItem(AItem:TNTFSDiskItem):Boolean;
{Remove the item from the list and free, do not update attribute}
begin
 {}
 Result:=False;
 
 if AItem = nil then Exit;
 if FItems = nil then Exit;
 
 {Remove Item}
 FItems.Remove(AItem);
 
 {Free Item}
 AItem.Free;
end;

{==============================================================================}

function TNTFSDiskAttribute.NewItem(AAttribute:TNTFSDiskAttribute):TNTFSDiskItem;
{Create an item, insert in sorted list and update attribute}
var
 Previous:TNTFSDiskItem;
begin
 {Create Item}
 Result:=CreateItem(True);
 if Result = nil then Exit;
 
 {Set Values}
 Result.Attribute:=AAttribute;
 
 {Get Previous}
 Previous:=FItems.GetPrevious(Result);
 
 {Insert Item}
 FItems.Insert(Previous,Result);
end;

{==============================================================================}

function TNTFSDiskAttribute.GetItem(AType:LongWord;const AName:String;AInstance:Integer):TNTFSDiskItem;
{Get an item by type, name and instance}
var
 Hash:LongWord;
 Count:Integer;
 Wildcard:Boolean;
 Item:TNTFSDiskItem;
 Previous:TNTFSDiskItem;
begin
 {}
 Result:=nil;
 
 if FItems = nil then Exit;
 
 Count:=0;
 Previous:=nil;
 
 {Calculate Hash}
 Hash:=0;                                                          //To Do //Testing4
 Wildcard:=(Length(AName) = ntfsAnyNameLength) and (AName = ntfsAnyName); {Modified 14/2/2011}
 if not Wildcard then Hash:=GenerateNameHash(AName,NAME_HASH_SIZE); //To Do //Testing4
 
 {Check Items}
 Item:=TNTFSDiskItem(FItems.First);
 while Item <> nil do
  begin
   {Check Type}
   if (AType = ntfsAttrTypeAny) or (Item.AttributeType = AType) then
    begin
     if Wildcard or (Item.AttributeHash = Hash) then
      begin
       if Wildcard or (Uppercase(Item.AttributeName) = Uppercase(AName)) then
        begin
         Inc(Count);
         if AInstance = ntfsInstanceLast then
          begin
           Previous:=Item;
          end
         else
          begin
           if (AInstance = ntfsInstanceFirst) or (Count = AInstance) then
            begin
             Result:=Item;
             Exit;
            end;
          end;
        end;
      end;
    end;
    
   Item:=TNTFSDiskItem(Item.Next);
  end;
  
 {Get Last}
 if AInstance = ntfsInstanceLast then Result:=Previous;
end;

{==============================================================================}

function TNTFSDiskAttribute.RemoveItem(AItem:TNTFSDiskItem):Boolean;
{Remove the item from the list, free and update attribute}
begin
 {}
 Result:=False;
 
 if AItem = nil then Exit;
 if FItems = nil then Exit;
 
 {Update Attribute}
  {Nothing}
 
 {Remove Item}
 FItems.Remove(AItem);
 
 {Free Item}
 AItem.Free;
 
 Result:=True;
end;

{==============================================================================}

function TNTFSDiskAttribute.MoveItem(AItem:TNTFSDiskItem;AAttribute:TNTFSDiskAttribute):Boolean;
{Remove the item from the list and reinsert with the new parent reference}
var
 Previous:TNTFSDiskItem;
begin
 {}
 Result:=False;
 
 if AItem = nil then Exit;
 if FItems = nil then Exit;
 if AAttribute = nil then Exit;

 {Update Attribute}
  {Nothing}
 
 {Remove Item}
 FItems.Remove(AItem);
 
 {Update Item}
 AItem.UpdateItem;
 
 {Get Previous}
 Previous:=FItems.GetPrevious(AItem);
 
 {Insert Item}
 FItems.Insert(Previous,AItem);
 
 Result:=True;
end;

{==============================================================================}

function TNTFSDiskAttribute.RenameItem(AItem:TNTFSDiskItem;AAttribute:TNTFSDiskAttribute):Boolean;
{Remove the item from the list and reinsert with the new name}
begin
 {}
 Result:=MoveItem(AItem,AAttribute);
end;

{==============================================================================}

function TNTFSDiskAttribute.CreateIndex(AVersion,ASector:Word):Boolean;
{Create an index, setup properties do not update attribute}
begin
 {Virtual Base}
 Result:=False;
end;

{==============================================================================}

function TNTFSDiskAttribute.NewIndex(AVersion,ASector:Word;AType,ARule,ASize,AOffset:LongWord):Boolean;
{Create an index, setup properties and update attribute}
begin
 {Virtual Base}
 Result:=False;
end;

{==============================================================================}

function TNTFSDiskAttribute.RecordNumber:Int64;
begin
 {}
 Result:=0;
 
 if FParent = nil then Exit;
 
 Result:=FParent.RecordNumber;
end;

{==============================================================================}

function TNTFSDiskAttribute.FileReference:Int64;
begin
 {}
 Result:=0;
 
 if FParent = nil then Exit;
 
 Result:=FParent.FileReference;
end;

{==============================================================================}

function TNTFSDiskAttribute.BaseReference:Int64;
{Returns parent reference when no base record is assigned (ie parent record is the base)}
begin
 {}
 Result:=0;
 
 if FParent = nil then Exit;
 
 if FParent.Base = nil then
  begin
   Result:=FParent.FileReference;
  end
 else
  begin
   Result:=FParent.Base.FileReference;
  end;
end;

{==============================================================================}

function TNTFSDiskAttribute.RunOffset:Word;
begin
 {}
 Result:=0;
 
 {Check Resident}
 if FNonResident = ntfsAttributeNonResident then
  begin
   {Check Compressed}
   if ((FAttributeFlags and ntfsAttributeFlagCompressed) = ntfsAttributeFlagCompressed) and (FCompressionUnit <> 0) then
    begin
     Result:=ntfsCompressedHeaderSize + NTFSRoundWordTo8Bytes(AttributeNameSize);
    end
   else
    begin
     Result:=ntfsNonResidentHeaderSize + NTFSRoundWordTo8Bytes(AttributeNameSize);
    end;
  end;
end;

{==============================================================================}

function TNTFSDiskAttribute.DataOffset:Word;
{Data Offset is supplied even if no data exists}
begin
 {}
 Result:=0;
 
 {Check Resident}
 if FNonResident = ntfsAttributeResident then
  begin
   Result:=ntfsResidentHeaderSize + NTFSRoundWordTo8Bytes(AttributeNameSize);
  end;
end;

{==============================================================================}

function TNTFSDiskAttribute.AttributeNameSize:Word;
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;
 
 Result:=Length(FAttributeName) shl 1; {Multiply by SizeOf(WideChar) / SizeOf(Word)}
 
 ReleaseLock;
end;

{==============================================================================}

function TNTFSDiskAttribute.AttributeNameLength:Byte;
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;
 
 Result:=Length(FAttributeName);
 
 ReleaseLock;
end;

{==============================================================================}

function TNTFSDiskAttribute.AttributeNameOffset:Word;
{Name Offset is supplied even if no name exists}
begin
 {}
 Result:=0;
 
 {Check Resident}
 if FNonResident = ntfsAttributeResident then
  begin
   Result:=ntfsResidentHeaderSize;
  end
 else
  begin
   {Check Compressed}
   if ((FAttributeFlags and ntfsAttributeFlagCompressed) = ntfsAttributeFlagCompressed) and (FCompressionUnit <> 0) then
    begin
     Result:=ntfsCompressedHeaderSize;
    end
   else
    begin
     Result:=ntfsNonResidentHeaderSize;
    end;
  end;
end;

{==============================================================================}

function TNTFSDiskAttribute.RunCount:LongWord;
begin
 {}
 Result:=0;
 
 if FRuns = nil then Exit;
 
 Result:=FRuns.Count;
end;

{==============================================================================}

function TNTFSDiskAttribute.ItemCount:LongWord;
begin
 {}
 Result:=0;
 
 if FItems = nil then Exit;
 
 Result:=FItems.Count;
end;

{==============================================================================}

function TNTFSDiskAttribute.CalculatedSize(AVersion:Word):LongWord;
{Calculated Size includes any rounding required for alignment}
begin
 {}
 Result:=0;
 
 {Check Resident}
 if FNonResident = ntfsAttributeResident then
  begin
   {Resident}
   Inc(Result,ntfsResidentHeaderSize + NTFSRoundWordTo8Bytes(AttributeNameSize) + DataSize); {Must round name size as DataOffset is 8 byte aligned}
   
   {Round Result}
   Result:=NTFSRoundLongWordTo8Bytes(Result);
  end
 else
  begin
   {Check Compressed}
   if ((FAttributeFlags and ntfsAttributeFlagCompressed) = ntfsAttributeFlagCompressed) and (FCompressionUnit <> 0) then
    begin
     {Compressed}
     Inc(Result,ntfsCompressedHeaderSize + NTFSRoundWordTo8Bytes(AttributeNameSize)); {Must round name size as RunOffset is 8 byte aligned}
     if FRuns <> nil then Inc(Result,FRuns.TotalSize);
     
     {Round Result}
     Result:=NTFSRoundLongWordTo8Bytes(Result);
    end
   else
    begin
     {Non Resident}
     Inc(Result,ntfsNonResidentHeaderSize + NTFSRoundWordTo8Bytes(AttributeNameSize)); {Must round name size as RunOffset is 8 byte aligned}
     if FRuns <> nil then Inc(Result,FRuns.TotalSize);
     
     {Round Result}
     Result:=NTFSRoundLongWordTo8Bytes(Result);
    end;
  end;
end;

{==============================================================================}

function TNTFSDiskAttribute.CalculatedDataSize(AVersion:Word):LongWord;
{Calculated Data Size is the actual size of the data following the attribute
 header. This will be the Data Size for Resident and the Run Size for
 Non Resident but may be overridden by descendants}
{Note: Size Attribute will use this value to update the Attribute and Record Sizes}
begin
 {Virtual Base}
 Result:=0;
 
 {Check Resident}
 if FNonResident = ntfsAttributeResident then
  begin
   {Resident}
   Result:=CalculatedStreamSize(AVersion); {Allows descendants to override only CalculatedStreamSize}
  end
 else
  begin
   {Non Resident}
   if FRuns <> nil then Result:=FRuns.TotalSize;
  end;
end;

{==============================================================================}

function TNTFSDiskAttribute.CalculatedStreamSize(AVersion:Word):Int64;
{Calculated Stream Size is the actual size of the data stream of the attribute.
 This will be the Data Size value for Resident, and the Stream Size value for
 Non Resident but may be overridden by descendants}
{Note: This is the value that should be passed to Size Attribute}
begin
 {Virtual Base}
 Result:=0;
 
 {Check Resident}
 if FNonResident = ntfsAttributeResident then
  begin
   {Resident}
   Result:=FDataSize;
  end
 else
  begin
   {Non Resident}
   Result:=FStreamSize;
  end;
end;

{==============================================================================}

function TNTFSDiskAttribute.CalculatedStreamUsed(AVersion:Word):Int64;
{Calculated Stream Used is the actual allocated size of the data stream in a compressed attribute.
 This will be the same as the Data Size value for Resident but will be the total allocated runs
 (Non Sparse) for Non Resident}
{Note: Size Attribute will use this value to update Stream Used}
begin
 {Virtual Base}
 Result:=0;
 
 {Check Resident}
 if FNonResident = ntfsAttributeNonResident then
  begin
   {Check Compressed}
   if ((FAttributeFlags and ntfsAttributeFlagCompressed) = ntfsAttributeFlagCompressed) and (FCompressionUnit <> 0) then
    begin
     if FRuns <> nil then Inc(Result,FRuns.ClusterCount);
    end;
  end;
end;

{==============================================================================}

function TNTFSDiskAttribute.CoalesceRun(ARun:TNTFSDiskRun):Boolean;
{Coalesce all runs in the list starting at the supplied run or first if not supplied}
var
 Run:TNTFSDiskRun;
 Current:TNTFSDiskRun;
begin
 {}
 Result:=False;
 
 {Run may be nil}
 if FRuns = nil then Exit;

 {Get Start}
 Run:=ARun;
 if Run = nil then Run:=TNTFSDiskRun(FRuns.First); {Not Start}
 
 {Coalesce Runs}
 while Run <> nil do
  begin
   {Save Current}
   Current:=Run;
   if not Current.IsLast then
    begin
     {Get Next} {Not Successor}
     Run:=TNTFSDiskRun(Current.Next);
     if Run <> nil then
      begin
       if (Current.IsSparse) and (Run.IsSparse) then
        begin
         {Sparse Runs}
         if not MergeRun(Current) then Exit;
        end
       else if not(Current.IsSparse) and not(Run.IsSparse) then
        begin
         {Normal Runs}
         if Run.Start = (Current.Start + Current.Length) then
          begin
           if not MergeRun(Current) then Exit;
          end;
        end;
      end;
    end;
    
   {Get Run} {Not Successor}
   Run:=TNTFSDiskRun(Current.Next);
  end;
  
 Result:=True;
end;

{==============================================================================}

function TNTFSDiskAttribute.UpdateRun(ARun:TNTFSDiskRun):Boolean;
{Update all runs in the list starting at the supplied run or first if not supplied}
var
 Run:TNTFSDiskRun;
begin
 {}
 Result:=False;
 
 {Run may be nil}
 if FRuns = nil then Exit;

 {Get Start}
 Run:=ARun;
 if Run = nil then Run:=FRuns.GetStart;
 
 {Update Runs}
 while Run <> nil do
  begin
   {Update Run}
   Run.UpdateRun;
   
   {Get Run}
   Run:=Run.GetSuccessor;
  end;
  
 Result:=True;
end;

{==============================================================================}

function TNTFSDiskAttribute.UpdateKey(AKey:TNTFSDiskKey):Boolean;
begin
 {Virtual Base}
 Result:=False;
 
 if AKey = nil then Exit;
 
 Result:=TNTFSAttributeKey(AKey).UpdateKey;
end;

{==============================================================================}

function TNTFSDiskAttribute.UpdateItem(AItem:TNTFSDiskItem):Boolean;
begin
 {Virtual Base}
 Result:=False;
 
 if AItem = nil then Exit;
 
 Result:=AItem.UpdateItem;
end;

{==============================================================================}

function TNTFSDiskAttribute.UpdateEntry(AEntry:TNTFSDiskEntry):Boolean;
begin
 {Virtual Base}
 Result:=False;
end;

{==============================================================================}

function TNTFSDiskAttribute.UpdateAttribute(AEntry:TNTFSDiskEntry):Boolean;
begin
 {Virtual Base}
 Result:=False;
end;

{==============================================================================}

function TNTFSDiskAttribute.ReadRuns(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean;
{Read the attribute data runs from the supplied buffer at the supplied offset}
var
 Run:TNTFSDiskRun;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {Read Runs}
 while True do
  begin
   {Create Run}
   Run:=CreateRun(False);
   if Run = nil then Exit;
   
   {Read Run}
   if not Run.ReadRun(ABuffer,AOffset,AVersion) then Exit;
   
   {Check Last}
   if Run.IsLast then Break;
  end;
  
 Result:=True;
end;

{==============================================================================}

function TNTFSDiskAttribute.WriteRuns(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean;
{Write the attribute data runs to the supplied buffer at the supplied offset}
var
 Run:TNTFSDiskRun;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {Get Run}
 if Runs = nil then Exit;
 Run:=TNTFSDiskRun(Runs.First);
 
 {Write Runs}
 while Run <> nil do
  begin
   {Write Run}
   if not Run.WriteRun(ABuffer,AOffset,AVersion) then Exit;
   
   {Get Run}
   Run:=TNTFSDiskRun(Run.Next);
  end;
  
 Result:=True;
end;

{==============================================================================}

function TNTFSDiskAttribute.ReadItems(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Read the attribute data items from the supplied buffer at the supplied offset}
var
 Item:TNTFSDiskItem;
 ItemData:PNTFSItemData;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {Check Size}
 while ASize >= ntfsItemSize do
  begin
   {Get Data}
   ItemData:=PNTFSItemData(LongWord(ABuffer) + AOffset);
   
   {Check None}
   if ItemData.AttributeType = ntfsAttrTypeNone then Break;
   
   {Check Last}
   if ItemData.AttributeType = ntfsAttrTypeEnd then Break;
   
   {Create Item}
   Item:=CreateItem(False);
   if Item = nil then Exit;
   
   {Read Item}
   if not Item.ReadItem(ABuffer,AOffset,ASize,AVersion) then Exit;
  end;
 
 Result:=True;
end;

{==============================================================================}

function TNTFSDiskAttribute.WriteItems(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Write the attribute data items to the supplied buffer at the supplied offset}
var
 Item:TNTFSDiskItem;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {Get Item}
 if Items = nil then Exit;
 Item:=TNTFSDiskItem(Items.First);
 
 {Write Items}
 while Item <> nil do
  begin
   {Check Size}
   if ASize < ntfsItemSize then Exit;
   
   {Write Item}
   if not Item.WriteItem(ABuffer,AOffset,ASize,AVersion) then Exit;
   
   {Get Item}
   Item:=TNTFSDiskItem(Item.Next);
  end;
  
 Result:=True;
end;

{==============================================================================}

function TNTFSDiskAttribute.ReadData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean;
{Read the resident attribute data from the supplied buffer at the supplied offset}
var
 NTFSUnknown:PNTFSUnknown;
begin
 {Virtual Base}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {Check Resident}
 if FNonResident = ntfsAttributeNonResident then Exit;
 
 {Get Data}
 NTFSUnknown:=PNTFSUnknown(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if FDataSize > ntfsUnknownSize then {Not Equal}
  begin
   {Set Size}
   SetDataSize(FDataSize);
   {Read Data}
   System.Move(NTFSUnknown.Data[0],FData^,FDataSize);
  end;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskAttribute.ReadData - Offset = ' + IntToStr(AOffset));
 {$ENDIF}
 
 {Update Offset}
 Inc(AOffset,FDataSize);
 
 Result:=True;
end;

{==============================================================================}

function TNTFSDiskAttribute.WriteData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean;
{Write the resident attribute data to the supplied buffer at the supplied offset}
var
 NTFSUnknown:PNTFSUnknown;
begin
 {Virtual Base}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskAttribute.WriteData - Offset = ' + IntToStr(AOffset));
 {$ENDIF}
 
 {Check Resident}
 if FNonResident = ntfsAttributeNonResident then Exit;
 
 {Get Data}
 NTFSUnknown:=PNTFSUnknown(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if FDataSize > ntfsUnknownSize then {Not Equal}
  begin
   {Write Data}
   System.Move(FData^,NTFSUnknown.Data[0],FDataSize);
  end;
  
 {Update Offset}
 Inc(AOffset,FDataSize);
 
 Result:=True;
end;

{==============================================================================}

function TNTFSDiskAttribute.ReadAttribute(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Read the attribute header and resident attribute data or data runs from the supplied buffer at the supplied offset}
var
 Offset:LongWord;
 AttributeHeader:PNTFSAttributeHeader;
 ResidentHeader:PNTFSResidentAttributeHeader;
 NonResidentHeader:PNTFSNonResidentAttributeHeader;
 CompressedHeader:PNTFSCompressedAttributeHeader;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {Get Header}
 AttributeHeader:=PNTFSAttributeHeader(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if ASize >= ntfsAttributeHeaderSize then
  begin
   {Preserve Status}
   if (IsFixed or IsSingle or IsUnlisted or IsUnmovable or IsManaged) then FStatus:=(FStatus and (ntfsStatusFixed or ntfsStatusSingle or ntfsStatusUnlisted or ntfsStatusUnmovable or ntfsStatusManaged)) else FStatus:=ntfsStatusNone;
   
   {Read Header}
   FAttributeType:=AttributeHeader.AttributeType;
   FAttributeSize:=AttributeHeader.AttributeSize;
   FAttributeFlags:=AttributeHeader.AttributeFlags;
   FAttributeId:=AttributeHeader.AttributeId;
   FNonResident:=AttributeHeader.NonResident;
   
   {Check Size}
   if FAttributeSize < ntfsAttributeHeaderSize then Exit;
   
   {Check Resident}
   if FNonResident = ntfsAttributeResident then
    begin
     {Get Header}
     ResidentHeader:=PNTFSResidentAttributeHeader(LongWord(ABuffer) + AOffset);
     
     {Check Size}
     if ASize >= ntfsResidentHeaderSize then
      begin
       {Read Header}
       FIndexed:=ResidentHeader.Indexed;
       FDataSize:=ResidentHeader.DataSize;
       
       {Read Name}
       AttributeName:=NTFSWideBufferToString(ResidentHeader,ResidentHeader.AttributeNameOffset,ResidentHeader.AttributeNameLength);
       
       {Set Offset}
       Offset:=AOffset + ResidentHeader.DataOffset;
       
       {Read Data}
       if not ReadData(ABuffer,Offset,AVersion) then Exit;
      end;
    end
   else
    begin
     {Get Header}
     NonResidentHeader:=PNTFSNonResidentAttributeHeader(LongWord(ABuffer) + AOffset);
     
     {Check Size}
     if ASize >= ntfsNonResidentHeaderSize then
      begin
       {Read Header}
       FStartVCN:=NonResidentHeader.StartVCN;
       FLastVCN:=NonResidentHeader.LastVCN;
       FStreamSize:=NonResidentHeader.StreamSize;
       FStreamAllocated:=NonResidentHeader.StreamAllocated;
       FInitializedSize:=NonResidentHeader.InitializedSize;
       FCompressionUnit:=NonResidentHeader.CompressionUnit;
       
       {Read Name}
       AttributeName:=NTFSWideBufferToString(NonResidentHeader,NonResidentHeader.AttributeNameOffset,NonResidentHeader.AttributeNameLength);
       
       {Check Compressed}
       if ((FAttributeFlags and ntfsAttributeFlagCompressed) = ntfsAttributeFlagCompressed) and (FCompressionUnit <> 0) then
        begin
         {Get Header}
         CompressedHeader:=PNTFSCompressedAttributeHeader(LongWord(ABuffer) + AOffset);
         
         {Check Size}
         if ASize >= ntfsCompressedHeaderSize then
          begin
           FStreamUsed:=CompressedHeader.StreamUsed;
          end;
        end;
        
       {Set Offset}
       Offset:=AOffset + NonResidentHeader.RunOffset;
       
       {Read Runs}
       if not ReadRuns(ABuffer,Offset,AVersion) then Exit;
      end;
    end;
    
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskAttribute.ReadAttribute - Type = ' + IntToHex(FAttributeType,8) + ' Name = ' + AttributeName);
   {$ENDIF}
   
   {Update Offset}
   Dec(ASize,AttributeHeader.AttributeSize);
   Inc(AOffset,AttributeHeader.AttributeSize);
   
   Result:=True;
  end;
end;

{==============================================================================}

function TNTFSDiskAttribute.WriteAttribute(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Write the attribute header and resident attribute data or data runs to the supplied buffer at the supplied offset}
var
 Offset:LongWord;
 AttributeHeader:PNTFSAttributeHeader;
 ResidentHeader:PNTFSResidentAttributeHeader;
 NonResidentHeader:PNTFSNonResidentAttributeHeader;
 CompressedHeader:PNTFSCompressedAttributeHeader;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskAttribute.WriteAttribute - Type = ' + IntToHex(FAttributeType,8) + ' Name = ' + AttributeName);
 {$ENDIF}
 
 {Get Header}
 AttributeHeader:=PNTFSAttributeHeader(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if ASize >= ntfsAttributeHeaderSize then
  begin
   {Write Header}
   AttributeHeader.AttributeType:=FAttributeType;
   AttributeHeader.AttributeSize:=FAttributeSize;
   AttributeHeader.AttributeFlags:=FAttributeFlags;
   AttributeHeader.AttributeId:=FAttributeId;
   AttributeHeader.NonResident:=FNonResident;
   
   {Check Resident}
   if FNonResident = ntfsAttributeResident then
    begin
     {Get Header}
     ResidentHeader:=PNTFSResidentAttributeHeader(LongWord(ABuffer) + AOffset);
     
     {Check Size}
     if ASize >= ntfsResidentHeaderSize then
      begin
       {Write Header}
       ResidentHeader.Indexed:=FIndexed;
       ResidentHeader.Reserved1:=0;
       ResidentHeader.DataSize:=DataSize; {Must be DataSize not FDataSize to pickup any overridden methods}
       ResidentHeader.DataOffset:=DataOffset;
       
       {Write Name}
       ResidentHeader.AttributeNameOffset:=AttributeNameOffset;
       ResidentHeader.AttributeNameLength:=AttributeNameLength;
       if not NTFSStringToWideBuffer(AttributeName,ResidentHeader,ResidentHeader.AttributeNameOffset,ResidentHeader.AttributeNameLength) then Exit;
       
       {Set Offset}
       Offset:=AOffset + ResidentHeader.DataOffset;
       
       {Write Data}
       if not WriteData(ABuffer,Offset,AVersion) then Exit;
      end;
    end
   else
    begin
     {Get Header}
     NonResidentHeader:=PNTFSNonResidentAttributeHeader(LongWord(ABuffer) + AOffset);
     
     {Check Size}
     if ASize >= ntfsNonResidentHeaderSize then
      begin
       {Write Header}
       NonResidentHeader.RunOffset:=RunOffset;
       NonResidentHeader.Reserved1:=0;
       NonResidentHeader.StartVCN:=FStartVCN;
       NonResidentHeader.LastVCN:=FLastVCN;
       NonResidentHeader.StreamSize:=FStreamSize;
       NonResidentHeader.StreamAllocated:=FStreamAllocated;
       NonResidentHeader.InitializedSize:=FInitializedSize;
       NonResidentHeader.CompressionUnit:=FCompressionUnit;
       
       {Write Name}
       NonResidentHeader.AttributeNameOffset:=AttributeNameOffset;
       NonResidentHeader.AttributeNameLength:=AttributeNameLength;
       if not NTFSStringToWideBuffer(AttributeName,NonResidentHeader,NonResidentHeader.AttributeNameOffset,NonResidentHeader.AttributeNameLength) then Exit;
       
       {Check Compressed}
       if ((FAttributeFlags and ntfsAttributeFlagCompressed) = ntfsAttributeFlagCompressed) and (FCompressionUnit <> 0) then
        begin
         {Get Header}
         CompressedHeader:=PNTFSCompressedAttributeHeader(LongWord(ABuffer) + AOffset);
         
         {Check Size}
         if ASize >= ntfsCompressedHeaderSize then
          begin
           CompressedHeader.StreamUsed:=FStreamUsed;
          end;
        end;
        
       {Set Offset}
       Offset:=AOffset + NonResidentHeader.RunOffset;
       
       {Write Runs}
       if not WriteRuns(ABuffer,Offset,AVersion) then Exit;
      end;
    end;
    
   {Update Offset}
   Dec(ASize,AttributeHeader.AttributeSize);
   Inc(AOffset,AttributeHeader.AttributeSize);
   
   Result:=True;
  end;
end;

{==============================================================================}
{==============================================================================}
{TNTFSStandardInformationAttribute}
constructor TNTFSStandardInformationAttribute.Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
begin
 {}
 inherited Create(ALocalLock,AParent);
 FStatus:=ntfsStatusFixed or ntfsStatusSingle or ntfsStatusUnmovable or ntfsStatusManaged; {Fixed Size, Single Instance, Unmovable, Managed Attribute}
 FAttributeType:=ntfsAttrTypeStandardInformation;

 FCreateTime:=ntfsNullFileTime;
 FWriteTime:=ntfsNullFileTime;
 FChangeTime:=ntfsNullFileTime;
 FAccessTime:=ntfsNullFileTime;
 FAttributes:=0;
 FMaxVersions:=0;
 FVersionNo:=0;
 FClassId:=0;
 FOwnerId:=0;
 FSecurityId:=0;
 FQuotaCharge:=0;
 FUpdateSequenceNumber:=0;
end;

{==============================================================================}

destructor TNTFSStandardInformationAttribute.Destroy;
begin
 {}
 inherited Destroy;
end;

{==============================================================================}

function TNTFSStandardInformationAttribute.CalculatedStreamSize(AVersion:Word):Int64;
{See notes in virtual base method}
begin
 {}
 Result:=0;
 
 case AVersion of
  ntfsNTFS12:begin
    Result:=ntfsStandardInformation12Size;
   end;
  ntfsNTFS30,ntfsNTFS31:begin
    Result:=ntfsStandardInformation30Size;
   end;
 end;
end;

{==============================================================================}

function TNTFSStandardInformationAttribute.UpdateEntry(AEntry:TNTFSDiskEntry):Boolean;
begin
 {}
 Result:=False;
 
 {Check Entry}
 if AEntry <> nil then
  begin
   {Update Entry}
   AEntry.WriteTime:=FWriteTime;
   AEntry.CreateTime:=FCreateTime;
   AEntry.AccessTime:=FAccessTime;
   AEntry.ChangeTime:=FChangeTime;
   {AEntry.Attributes:=(AEntry.Attributes or (FAttributes and faFindMask));}
   AEntry.Attributes:=((AEntry.Attributes and (faFlagMask or faMatchMask)) or (FAttributes and faFindMask)); {Preserve Internal Attributes} {Preserve VolumeId and Directory}
   //To Do //Show faNormal
   
   Result:=True;
  end;
end;

{==============================================================================}

function TNTFSStandardInformationAttribute.UpdateAttribute(AEntry:TNTFSDiskEntry):Boolean;
begin
 {}
 Result:=False;
 
 {Check Entry}
 if AEntry <> nil then
  begin
   {Update Entry}
   AEntry.ChangeTime:=Ultibo.DateTimeToFileTime(Now); {Converted to UTC}
   
   {Update Attribute}
   FWriteTime:=AEntry.WriteTime;
   FCreateTime:=AEntry.CreateTime;
   FAccessTime:=AEntry.AccessTime;
   FChangeTime:=AEntry.ChangeTime;
   {FAttributes:=(AEntry.Attributes and faFindMask);} {Hide Internal Attributes}
   FAttributes:=(AEntry.Attributes and (faFindMask and not(faVolumeID or faDirectory))); {Hide Internal Attributes} {Hide VolumeId and Directory}
   if FParent <> nil then if FParent.IsIndexView then FAttributes:=(FAttributes or faMftIndexView);
   //To Do //Hide faNormal
   
   Result:=True;
  end;
end;

{==============================================================================}

function TNTFSStandardInformationAttribute.ReadData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean;
{Read the resident attribute data from the supplied buffer at the supplied offset}
var
 NTFSStandardInformation12:PNTFS12StandardInformation;
 NTFSStandardInformation30:PNTFS30StandardInformation;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {Check Resident}
 if FNonResident = ntfsAttributeNonResident then Exit;
 
 {Get Data}
 NTFSStandardInformation12:=PNTFS12StandardInformation(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if FDataSize >= ntfsStandardInformation12Size then
  begin
   {Read Data}
   FCreateTime:=NTFSStandardInformation12.CreateTime;
   FWriteTime:=NTFSStandardInformation12.WriteTime;
   FChangeTime:=NTFSStandardInformation12.ChangeTime;
   FAccessTime:=NTFSStandardInformation12.AccessTime;
   FAttributes:=NTFSStandardInformation12.Attributes;
   FMaxVersions:=NTFSStandardInformation12.MaxVersions;
   FVersionNo:=NTFSStandardInformation12.VersionNo;
   FClassId:=NTFSStandardInformation12.ClassId;
   
   {Check Version}
   case AVersion of
    ntfsNTFS30,ntfsNTFS31:begin
      {Get Data}
      NTFSStandardInformation30:=PNTFS30StandardInformation(LongWord(ABuffer) + AOffset);
      
      {Check Size}
      if FDataSize >= ntfsStandardInformation30Size then
       begin
        {Read Data}
        FOwnerId:=NTFSStandardInformation30.OwnerId;
        FSecurityId:=NTFSStandardInformation30.SecurityId;
        FQuotaCharge:=NTFSStandardInformation30.QuotaCharge;
        FUpdateSequenceNumber:=NTFSStandardInformation30.UpdateSequenceNumber;
       end;
     end;
   end;
  end;
  
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSStandardInformationAttribute.ReadData - Offset = ' + IntToStr(AOffset));
 {$ENDIF}
 
 {Update Offset}
 Inc(AOffset,FDataSize);
 
 Result:=True;
end;

{==============================================================================}

function TNTFSStandardInformationAttribute.WriteData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean;
{Write the resident attribute data to the supplied buffer at the supplied offset}
var
 NTFSStandardInformation12:PNTFS12StandardInformation;
 NTFSStandardInformation30:PNTFS30StandardInformation;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSStandardInformationAttribute.WriteData - Offset = ' + IntToStr(AOffset));
 {$ENDIF}
 
 {Check Resident}
 if FNonResident = ntfsAttributeNonResident then Exit;
 
 {Get Data}
 NTFSStandardInformation12:=PNTFS12StandardInformation(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if FDataSize >= ntfsStandardInformation12Size then
  begin
   {Write Data}
   NTFSStandardInformation12.CreateTime:=FCreateTime;
   NTFSStandardInformation12.WriteTime:=FWriteTime;
   NTFSStandardInformation12.ChangeTime:=FChangeTime;
   NTFSStandardInformation12.AccessTime:=FAccessTime;
   NTFSStandardInformation12.Attributes:=FAttributes;
   NTFSStandardInformation12.MaxVersions:=FMaxVersions;
   NTFSStandardInformation12.VersionNo:=FVersionNo;
   NTFSStandardInformation12.ClassId:=FClassId;
   
   {Check Version}
   case AVersion of
    ntfsNTFS30,ntfsNTFS31:begin
      {Get Data}
      NTFSStandardInformation30:=PNTFS30StandardInformation(LongWord(ABuffer) + AOffset);
      
      {Check Size}
      if FDataSize >= ntfsStandardInformation30Size then
       begin
        {Write Data}
        NTFSStandardInformation30.OwnerId:=FOwnerId;
        NTFSStandardInformation30.SecurityId:=FSecurityId;
        NTFSStandardInformation30.QuotaCharge:=FQuotaCharge;
        NTFSStandardInformation30.UpdateSequenceNumber:=FUpdateSequenceNumber;
       end;
     end;
   end;
  end;
  
 {Update Offset}
 Inc(AOffset,FDataSize);
 
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TNTFSAttributeListAttribute}
constructor TNTFSAttributeListAttribute.Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
begin
 {}
 inherited Create(ALocalLock,AParent);
 FStatus:=ntfsStatusSingle or ntfsStatusUnlisted or ntfsStatusUnmovable or ntfsStatusManaged; {Single Instance, Unlisted Attribute, Unmovable, Managed Attribute}
 FAttributeType:=ntfsAttrTypeAttributeList;

 {FItems:=TNTFSDiskItems.Create;} {This conflicts with CreateItems()}
 FItems:=CreateItems(False); {Required by LoadList() as AttributeList can be Non Resident}
end;

{==============================================================================}

destructor TNTFSAttributeListAttribute.Destroy;
begin
 {}
 inherited Destroy;
end;

{==============================================================================}

function TNTFSAttributeListAttribute.CalculatedStreamSize(AVersion:Word):Int64;
{See notes in virtual base method}
begin
 {}
 Result:=0;
 
 if FItems <> nil then Result:=FItems.TotalSize;
end;

{==============================================================================}

function TNTFSAttributeListAttribute.ReadData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean;
{Read the resident attribute data from the supplied buffer at the supplied offset}
var
 Size:LongWord;
 Offset:LongWord;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {Check Resident}
 if FNonResident = ntfsAttributeNonResident then Exit;
 
 {Check Size}
 if FDataSize > ntfsAttributeListSize then {Not Equal}
  begin
   {Set Offset}
   Size:=FDataSize;
   Offset:=AOffset;
   
   {Read Items}
   if not ReadItems(ABuffer,Offset,Size,AVersion) then Exit;
  end;
  
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSAttributeListAttribute.ReadData - Offset = ' + IntToStr(AOffset));
 {$ENDIF}
 
 {Update Offset}
 Inc(AOffset,FDataSize);
 
 Result:=True;
end;

{==============================================================================}

function TNTFSAttributeListAttribute.WriteData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean;
{Write the resident attribute data to the supplied buffer at the supplied offset}
var
 Size:LongWord;
 Offset:LongWord;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSAttributeListAttribute.WriteData - Offset = ' + IntToStr(AOffset));
 {$ENDIF}
 
 {Check Resident}
 if FNonResident = ntfsAttributeNonResident then Exit;
 
 {Check Size}
 if FDataSize > ntfsAttributeListSize then {Not Equal}
  begin
   {Set Offset}
   Size:=FDataSize;
   Offset:=AOffset;
   
   {Write Items}
   if not WriteItems(ABuffer,Offset,Size,AVersion) then Exit;
  end;
  
 {Update Offset}
 Inc(AOffset,FDataSize);
 
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TNTFSFileNameAttribute}
constructor TNTFSFileNameAttribute.Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
begin
 {}
 inherited Create(ALocalLock,AParent);
 FStatus:=ntfsStatusManaged; {Managed Attribute}
 FAttributeType:=ntfsAttrTypeFileName;

 FParentReference:=0;
 FCreateTime:=ntfsNullFileTime;
 FWriteTime:=ntfsNullFileTime;
 FChangeTime:=ntfsNullFileTime;
 FAccessTime:=ntfsNullFileTime;
 FFileAllocated:=0;
 FFileSize:=0;
 FFileFlags:=faNone;
 FReparseTag:=0;
 FNameSpace:=ntfsNameSpacePosix;
 FFileName:=ntfsBlankName;

 FFileHash:=0;
end;

{==============================================================================}

destructor TNTFSFileNameAttribute.Destroy;
begin
 {}
 inherited Destroy;
end;

{==============================================================================}

function TNTFSFileNameAttribute.GetFileName:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;
 
 Result:=FFileName;
 UniqueString(Result);
 
 ReleaseLock;
end;

{==============================================================================}

procedure TNTFSFileNameAttribute.SetFileName(const AFileName:String);
begin
 {}
 if not AcquireLock then Exit;
 
 FFileName:=AFileName;
 UniqueString(FFileName);
 FFileHash:=GenerateNameHash(FFileName,NAME_HASH_SIZE);
 
 ReleaseLock;
end;

{==============================================================================}

function TNTFSFileNameAttribute.FileNameSize:Word;
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;
 
 Result:=Length(FFileName) shl 1; {Multiply by SizeOf(WideChar) / SizeOf(Word)}
 
 ReleaseLock;
end;

{==============================================================================}

function TNTFSFileNameAttribute.FileNameLength:Byte;
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;
 
 Result:=Length(FFileName);
 
 ReleaseLock;
end;

{==============================================================================}

function TNTFSFileNameAttribute.ParentRecord:Int64;
begin
 {}
 TNTFSFileReference(Result).RecordNumber:=TNTFSFileReference(FParentReference).RecordNumber;
 TNTFSFileReference(Result).RecordSegment:=TNTFSFileReference(FParentReference).RecordSegment;
 TNTFSFileReference(Result).SequenceNumber:=0;
end;

{==============================================================================}

function TNTFSFileNameAttribute.CalculatedStreamSize(AVersion:Word):Int64;
{See notes in virtual base method}
begin
 {}
 Result:=ntfsFileNameSize + FileNameSize;
end;

{==============================================================================}

function TNTFSFileNameAttribute.UpdateEntry(AEntry:TNTFSDiskEntry):Boolean;
begin
 {}
 Result:=False;
 
 {Check Entry}
 if AEntry <> nil then
  begin
   if (AEntry.Attributes and faStream) = faStream then Exit;
   
   {Update Entry}
   case FNameSpace of
    ntfsNameSpacePosix:begin {HardLinks use Posix namespace}
      AEntry.Name:=FileName;
      AEntry.AltName:=ntfsBlankName;
     end;
    ntfsNameSpaceWin32:begin
      AEntry.Name:=FileName;
      if AEntry.Alternate = nil then AEntry.AltName:=ntfsBlankName; {Added 22/11/2011 to support rename}
     end;
    ntfsNameSpaceDos:begin
      AEntry.AltName:=FileName;
     end;
    ntfsNameSpaceBoth:begin
      AEntry.Name:=FileName;
      AEntry.AltName:=ntfsBlankName;
     end;
   end;
   {AEntry.WriteTime:=FWriteTime;} {WriteTime is obtained from the Standard Information attribute}
   {AEntry.CreateTime:=FCreateTime;} {CreateTime is obtained from the Standard Information attribute}
   {AEntry.AccessTime:=FAccessTime;} {AccessTime is obtained from the Standard Information attribute}
   {AEntry.ChangeTime:=FChangeTime;} {ChangeTime is obtained from the Standard Information attribute}
   {AEntry.Attributes:=((AEntry.Attributes and (faFlagMask or faMatchMask)) or (FFileFlags and faFindMask));} {Preserve Internal Attributes} {Preserve VolumeId and Directory} {Attributes is obtained from the Standard Information attribute}
   {AEntry.Size:=FFileSize;} {Size is obtained from the Data attribute}
   {AEntry.Used:=0;} {Used is obtained from the Data attribute}
   {AEntry.Allocated:=FFileAllocated;} {Allocated is obtained from the Data attribute}
   if FReparseTag <> 0 then AEntry.ReparseTag:=FReparseTag;
   
   Result:=True;
  end;
end;

{==============================================================================}

function TNTFSFileNameAttribute.UpdateAttribute(AEntry:TNTFSDiskEntry):Boolean;
begin
 {}
 Result:=False;
 
 {Check Entry}
 if AEntry <> nil then
  begin
   if (AEntry.Attributes and faStream) = faStream then Exit;
   
   {Update Attribute}
   case FNameSpace of
    ntfsNameSpacePosix:begin {HardLinks use Posix namespace}
      FileName:=AEntry.Name;
     end;
    ntfsNameSpaceWin32:begin
      FileName:=AEntry.Name;
     end;
    ntfsNameSpaceDos:begin
      FileName:=AEntry.AltName;
     end;
    ntfsNameSpaceBoth:begin
      FileName:=AEntry.Name;
     end;
   end;
   
   FWriteTime:=AEntry.WriteTime; {WriteTime is obtained from the Standard Information attribute (But is updated to File Name attribute)}
   FCreateTime:=AEntry.CreateTime; {CreateTime is obtained from the Standard Information attribute (But is updated to File Name attribute)}
   FAccessTime:=AEntry.AccessTime; {AccessTime is obtained from the Standard Information attribute (But is updated to File Name attribute)}
   FChangeTime:=AEntry.ChangeTime; {ChangeTime is obtained from the Standard Information attribute (But is updated to File Name attribute)}
   {FFileFlags:=(AEntry.Attributes and faFindMask);} {Hide Internal Attributes} {Attributes is obtained from the Standard Information attribute (But is updated to File Name attribute)}
   FFileFlags:=(AEntry.Attributes and (faFindMask and not(faVolumeID or faDirectory))); {Hide Internal Attributes} {Hide VolumeId and Directory} {Attributes is obtained from the Standard Information attribute (But is updated to File Name attribute)}
   FFileSize:=AEntry.Size; {Size is obtained from the Data attribute (But is updated to File Name attribute)}
   FFileAllocated:=AEntry.Used; {Used is obtained from the Data attribute (But is updated to File Name attribute)}
   if FFileAllocated = 0 then FFileAllocated:=AEntry.Allocated; {Allocated is obtained from the Data attribute (But is updated to File Name attribute)}
   if FParent <> nil then if FParent.IsFolder then FFileFlags:=(FFileFlags or faMftDirectory);
   if FParent <> nil then if FParent.IsIndexView then FFileFlags:=(FFileFlags or faMftIndexView);
   FReparseTag:=AEntry.ReparseTag;
   
   Result:=True;
  end;
end;

{==============================================================================}

function TNTFSFileNameAttribute.ReadData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean;
{Read the resident attribute data from the supplied buffer at the supplied offset}
var
 NTFSFileName:PNTFSFileName;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {Check Resident}
 if FNonResident = ntfsAttributeNonResident then Exit;
 
 {Get Data}
 NTFSFileName:=PNTFSFileName(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if FDataSize >= ntfsFileNameSize then
  begin
   {Read Data}
   FParentReference:=NTFSFileName.ParentReference;
   FCreateTime:=NTFSFileName.CreateTime;
   FWriteTime:=NTFSFileName.WriteTime;
   FChangeTime:=NTFSFileName.ChangeTime;
   FAccessTime:=NTFSFileName.AccessTime;
   FFileAllocated:=NTFSFileName.FileAllocated;
   FFileSize:=NTFSFileName.FileSize;
   FFileFlags:=NTFSFileName.FileFlags;
   FReparseTag:=NTFSFileName.ReparseTag;
   FNameSpace:=NTFSFileName.NameSpace;
   
   {Read Name}
   FileName:=NTFSWideBufferToString(@NTFSFileName.FileName[0],0,NTFSFileName.FileNameLength);
  end;
  
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileNameAttribute.ReadData - Offset = ' + IntToStr(AOffset) + ' FileName = ' + FileName + ' NameSpace = ' + IntToStr(FNameSpace));
 {$ENDIF}
 
 {Update Offset}
 Inc(AOffset,FDataSize);
 
 Result:=True;
end;

{==============================================================================}

function TNTFSFileNameAttribute.WriteData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean;
{Write the resident attribute data to the supplied buffer at the supplied offset}
var
 NTFSFileName:PNTFSFileName;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSFileNameAttribute.WriteData - Offset = ' + IntToStr(AOffset) + ' FileName = ' + FileName + ' NameSpace = ' + IntToStr(FNameSpace));
 {$ENDIF}
 
 {Check Resident}
 if FNonResident = ntfsAttributeNonResident then Exit;
 
 {Get Data}
 NTFSFileName:=PNTFSFileName(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if FDataSize >= ntfsFileNameSize then
  begin
   {Write Data}
   NTFSFileName.ParentReference:=FParentReference;
   NTFSFileName.CreateTime:=FCreateTime;
   NTFSFileName.WriteTime:=FWriteTime;
   NTFSFileName.ChangeTime:=FChangeTime;
   NTFSFileName.AccessTime:=FAccessTime;
   NTFSFileName.FileAllocated:=FFileAllocated;
   NTFSFileName.FileSize:=FFileSize;
   NTFSFileName.FileFlags:=FFileFlags;
   NTFSFileName.ReparseTag:=FReparseTag;
   NTFSFileName.NameSpace:=FNameSpace;
   
   {Write Name}
   NTFSFileName.FileNameLength:=FileNameLength;
   if not NTFSStringToWideBuffer(FileName,@NTFSFileName.FileName[0],0,NTFSFileName.FileNameLength) then Exit;
  end;
  
 {Update Offset}
 Inc(AOffset,FDataSize);
 
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TNTFSObjectIdAttribute}
constructor TNTFSObjectIdAttribute.Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
begin
 {}
 inherited Create(ALocalLock,AParent);
 FStatus:=ntfsStatusFixed or ntfsStatusManaged; {Fixed Size, Managed Attribute}
 FAttributeType:=ntfsAttrTypeObjectId;

 FObjectId:=ntfsNullGUID;
 FBirthVolumeId:=ntfsNullGUID;
 FBirthObjectId:=ntfsNullGUID;
 FDomainId:=ntfsNullGUID;
end;

{==============================================================================}

destructor TNTFSObjectIdAttribute.Destroy;
begin
 {}
 inherited Destroy;
end;

{==============================================================================}

function TNTFSObjectIdAttribute.CalculatedStreamSize(AVersion:Word):Int64;
{See notes in virtual base method}
begin
 {}
 Result:=ntfsObjectIdSize;
 
 if not NullGUID(FBirthVolumeId) then Inc(Result,ntfsObjectIdSize);
 if not NullGUID(FBirthObjectId) then Inc(Result,ntfsObjectIdSize);
 if not NullGUID(FDomainId) then Inc(Result,ntfsObjectIdSize);
end;

{==============================================================================}

function TNTFSObjectIdAttribute.ReadData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean;
{Read the resident attribute data from the supplied buffer at the supplied offset}
var
 NTFSObjectId:PNTFSObjectId;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {Check Resident}
 if FNonResident = ntfsAttributeNonResident then Exit;
 
 {Get Data}
 NTFSObjectId:=PNTFSObjectId(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if FDataSize >= ntfsObjectIdSize then
  begin
   {Read Data}
   FObjectId:=NTFSObjectId.ObjectId;
   if FDataSize >= ntfsObjectIdSize2 then FBirthVolumeId:=NTFSObjectId.BirthVolumeId;
   if FDataSize >= ntfsObjectIdSize3 then FBirthObjectId:=NTFSObjectId.BirthObjectId;
   if FDataSize >= ntfsObjectIdSize4 then FDomainId:=NTFSObjectId.DomainId;
   //To Do //Can be up to 256 bytes, need to Read and store any remainder //In Data ? Elsewhere ?
  end;
  
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSObjectIdAttribute.ReadData - Offset = ' + IntToStr(AOffset) + ' ObjectId = ' + GUIDToString(FObjectId));
 {$ENDIF}
 
 {Update Offset}
 Inc(AOffset,FDataSize);
 
 Result:=True;
end;

{==============================================================================}

function TNTFSObjectIdAttribute.WriteData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean;
{Write the resident attribute data to the supplied buffer at the supplied offset}
var
 NTFSObjectId:PNTFSObjectId;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSObjectIdAttribute.WriteData - Offset = ' + IntToStr(AOffset) + ' ObjectId = ' + GUIDToString(FObjectId));
 {$ENDIF}
 
 {Check Resident}
 if FNonResident = ntfsAttributeNonResident then Exit;
 
 {Get Data}
 NTFSObjectId:=PNTFSObjectId(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if FDataSize >= ntfsObjectIdSize then
  begin
   {Write Data}
   NTFSObjectId.ObjectId:=FObjectId;
   if FDataSize >= ntfsObjectIdSize2 then NTFSObjectId.BirthVolumeId:=FBirthVolumeId;
   if FDataSize >= ntfsObjectIdSize3 then NTFSObjectId.BirthObjectId:=FBirthObjectId;
   if FDataSize >= ntfsObjectIdSize4 then NTFSObjectId.DomainId:=FDomainId;
   //To Do //Can be up to 256 bytes, need to Store and Write any remainder
  end;
  
 {Update Offset}
 Inc(AOffset,FDataSize);
 
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TNTFSVolumeVersionAttribute}
constructor TNTFSVolumeVersionAttribute.Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
begin
 {}
 inherited Create(ALocalLock,AParent);
 {FStatus:=ntfsStatusFixed;} {Fixed Size}
 FAttributeType:=ntfsAttrTypeVolumeVersion;
end;

{==============================================================================}

destructor TNTFSVolumeVersionAttribute.Destroy;
begin
 {}
 inherited Destroy;
end;

{==============================================================================}

procedure TNTFSVolumeVersionAttribute.SetDataSize(AValue:LongWord);
var
 Size:LongWord;
 Buffer:Pointer;
begin
 {}
 //To Do //Lock 
 
 if AValue = 0 then
  begin
   if FData <> nil then FreeMem(FData);
   FDataSize:=0;
   FData:=nil;
  end
 else
  begin
   Buffer:=FData;
   Size:=FDataSize;
   FDataSize:=AValue;
   FData:=AllocMem(FDataSize);
   
   if Buffer <> nil then
    begin
     if FData <> nil then System.Move(Buffer^,FData^,Min(Size,FDataSize));
     
     FreeMem(Buffer);
    end;
  end;
end;

{==============================================================================}
{==============================================================================}
{TNTFSSecurityDescriptorAttribute}
constructor TNTFSSecurityDescriptorAttribute.Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
begin
 {}
 inherited Create(ALocalLock,AParent);
 FStatus:=ntfsStatusManaged; {Managed Attribute}
 FAttributeType:=ntfsAttrTypeSecurityDescriptor;

 FSecurity:=nil;
end;

{==============================================================================}

destructor TNTFSSecurityDescriptorAttribute.Destroy;
begin
 {}
 if FSecurity <> nil then FSecurity.Free;
 inherited Destroy;
end;

{==============================================================================}

function TNTFSSecurityDescriptorAttribute.CreateSecurity:Boolean;
{Create a security, setup properties do not update Attribute}
begin
 {}
 Result:=False;
 
 if FSecurity <> nil then Exit;
 
 {Create Security}
 FSecurity:=TNTFSSecurity.Create(INVALID_HANDLE_VALUE); //To Do //Critical
 
 Result:=True;
end;

{==============================================================================}

function TNTFSSecurityDescriptorAttribute.NewSecurity(ASecurity:TNTFSSecurity):Boolean;
{Create a security, setup properties and update Attribute}
begin
 {}
 if ASecurity = nil then
  begin
   Result:=CreateSecurity;
   if not Result then Exit;
   
   {Setup Security}
   FSecurity.Control:=SE_SELF_RELATIVE;
  end
 else
  begin
   FSecurity:=ASecurity;
   
   Result:=True;
  end;
end;

{==============================================================================}

function TNTFSSecurityDescriptorAttribute.CalculatedStreamSize(AVersion:Word):Int64;
{See notes in virtual base method}
begin
 {}
 Result:=0;
 
 if FSecurity <> nil then Result:=FSecurity.SecuritySize;
end;

{==============================================================================}

function TNTFSSecurityDescriptorAttribute.ReadSecurity(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {Check Size}
 if ASize >= ntfsSecuritySize then
  begin
   {Create Security}
   if not CreateSecurity then Exit;
   
   {Read Security}
   if not FSecurity.ReadSecurity(ABuffer,AOffset,ASize,AVersion) then Exit;
  end;
  
 Result:=True;
end;

{==============================================================================}

function TNTFSSecurityDescriptorAttribute.WriteSecurity(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {Check Size}
 if ASize >= ntfsSecuritySize then
  begin
   {Check Security}
   if FSecurity = nil then Exit;
   
   {Write Security}
   if not FSecurity.WriteSecurity(ABuffer,AOffset,ASize,AVersion) then Exit;
  end;
  
 Result:=True;
end;

{==============================================================================}

function TNTFSSecurityDescriptorAttribute.ReadData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean;
{Read the resident attribute data from the supplied buffer at the supplied offset}
var
 Size:LongWord;
 Offset:LongWord;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {Check Resident}
 if FNonResident = ntfsAttributeNonResident then Exit;
 
 {Check Size}
 if FDataSize > ntfsSecurityDescriptorSize then {Not Equal}
  begin
   {Set Offset}
   Size:=FDataSize;
   Offset:=AOffset;
   
   {Read Security}
   if not ReadSecurity(ABuffer,Offset,Size,AVersion) then Exit;
  end;
  
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSSecurityDescriptorAttribute.ReadData - Offset = ' + IntToStr(AOffset));
 {$ENDIF}
 
 {Update Offset}
 Inc(AOffset,FDataSize);
 
 Result:=True;
end;

{==============================================================================}

function TNTFSSecurityDescriptorAttribute.WriteData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean;
{Write the resident attribute data to the supplied buffer at the supplied offset}
var
 Size:LongWord;
 Offset:LongWord;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSSecurityDescriptorAttribute.WriteData - Offset = ' + IntToStr(AOffset));
 {$ENDIF}
 
 {Check Resident}
 if FNonResident = ntfsAttributeNonResident then Exit;
 
 {Check Size}
 if FDataSize > ntfsSecurityDescriptorSize then {Not Equal}
  begin
   {Set Offset}
   Size:=FDataSize;
   Offset:=AOffset;
   
   {Write Security}
   if not WriteSecurity(ABuffer,Offset,Size,AVersion) then Exit;
  end;
  
 {Update Offset}
 Inc(AOffset,FDataSize);
 
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TNTFSVolumeNameAttribute}
constructor TNTFSVolumeNameAttribute.Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
begin
 {}
 inherited Create(ALocalLock,AParent);
 FStatus:=ntfsStatusSingle or ntfsStatusManaged; {Single Instance, Managed Attribute}
 FAttributeType:=ntfsAttrTypeVolumeName;

 FVolumeName:=ntfsBlankName;
end;

{==============================================================================}

destructor TNTFSVolumeNameAttribute.Destroy;
begin
 {}
 inherited Destroy;
end;

{==============================================================================}

function TNTFSVolumeNameAttribute.GetVolumeName:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;
 
 Result:=FVolumeName;
 UniqueString(Result);
 
 ReleaseLock;
end;

{==============================================================================}

procedure TNTFSVolumeNameAttribute.SetVolumeName(const AVolumeName:String);
begin
 {}
 if not AcquireLock then Exit;
 
 FVolumeName:=AVolumeName;
 UniqueString(FVolumeName);
 {Do not update AttributeSize or DataSize}
 
 ReleaseLock;
end;

{==============================================================================}

function TNTFSVolumeNameAttribute.CalculatedStreamSize(AVersion:Word):Int64;
{See notes in virtual base method}
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;
 
 Result:=Length(FVolumeName) shl 1; {Multiply by SizeOf(WideChar) / SizeOf(Word)}
 
 ReleaseLock;
end;

{==============================================================================}

function TNTFSVolumeNameAttribute.ReadData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean;
{Read the resident attribute data from the supplied buffer at the supplied offset}
var
 NTFSVolumeName:PNTFSVolumeName;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {Check Resident}
 if FNonResident = ntfsAttributeNonResident then Exit;
 
 {Get Data}
 NTFSVolumeName:=PNTFSVolumeName(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if FDataSize > ntfsVolumeNameSize then {Not Equal}
  begin
   {Read Name}
   VolumeName:=NTFSWideBufferToString(@NTFSVolumeName.VolumeName[0],0,(FDataSize shr 1)); {Divide by SizeOf(WideChar) / SizeOf(Word)}
  end;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSVolumeNameAttribute.ReadData - Offset = ' + IntToStr(AOffset) + ' VolumeName = ' + VolumeName);
 {$ENDIF}
 
 {Update Offset}
 Inc(AOffset,FDataSize);
 
 Result:=True;
end;

{==============================================================================}

function TNTFSVolumeNameAttribute.WriteData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean;
{Write the resident attribute data to the supplied buffer at the supplied offset}
var
 NTFSVolumeName:PNTFSVolumeName;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSVolumeNameAttribute.WriteData - Offset = ' + IntToStr(AOffset) + ' VolumeName = ' + VolumeName);
 {$ENDIF}
 
 {Check Resident}
 if FNonResident = ntfsAttributeNonResident then Exit;
 
 {Get Data}
 NTFSVolumeName:=PNTFSVolumeName(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if FDataSize > ntfsVolumeNameSize then {Not Equal}
  begin
   {Write Name}
   {DataSize is updated by SizeAttribute in FileSystem}
   if not NTFSStringToWideBuffer(VolumeName,@NTFSVolumeName.VolumeName[0],0,(FDataSize shr 1)) then Exit; {Divide by SizeOf(WideChar) / SizeOf(Word)}
  end;
 
 {Update Offset}
 Inc(AOffset,FDataSize);
 
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TNTFSVolumeInformationAttribute}
constructor TNTFSVolumeInformationAttribute.Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
begin
 {}
 inherited Create(ALocalLock,AParent);
 FStatus:=ntfsStatusFixed or ntfsStatusSingle or ntfsStatusManaged; {Fixed Size, Single Instance, Managed Attribute}
 FAttributeType:=ntfsAttrTypeVolumeInformation;

 FMajorVersion:=0;
 FMinorVersion:=0;
 FVolumeFlags:=ntfsVolumeFlagNone;
end;

{==============================================================================}

destructor TNTFSVolumeInformationAttribute.Destroy;
begin
 {}
 inherited Destroy;
end;

{==============================================================================}

function TNTFSVolumeInformationAttribute.CalculatedStreamSize(AVersion:Word):Int64;
{See notes in virtual base method}
begin
 {}
 Result:=ntfsVolumeInformationSize;
end;

{==============================================================================}

function TNTFSVolumeInformationAttribute.ReadData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean;
{Read the resident attribute data from the supplied buffer at the supplied offset}
var
 NTFSVolumeInformation:PNTFSVolumeInformation;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {Check Resident}
 if FNonResident = ntfsAttributeNonResident then Exit;
 
 {Get Data}
 NTFSVolumeInformation:=PNTFSVolumeInformation(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if FDataSize >= ntfsVolumeInformationSize then
  begin
   {Read Data}
   FMajorVersion:=NTFSVolumeInformation.MajorVersion;
   FMinorVersion:=NTFSVolumeInformation.MinorVersion;
   FVolumeFlags:=NTFSVolumeInformation.VolumeFlags;
  end;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSVolumeInformationAttribute.ReadData - Offset = ' + IntToStr(AOffset));
 {$ENDIF}
 
 {Update Offset}
 Inc(AOffset,FDataSize);
 
 Result:=True;
end;

{==============================================================================}

function TNTFSVolumeInformationAttribute.WriteData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean;
{Write the resident attribute data to the supplied buffer at the supplied offset}
var
 NTFSVolumeInformation:PNTFSVolumeInformation;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSVolumeInformationAttribute.WriteData - Offset = ' + IntToStr(AOffset) + ' Major = ' + IntToStr(FMajorVersion) + ' Minor = ' + IntToStr(FMinorVersion) + ' Flags = ' + IntToHex(FVolumeFlags,4));
 {$ENDIF}
 
 {Check Resident}
 if FNonResident = ntfsAttributeNonResident then Exit;
 
 {Get Data}
 NTFSVolumeInformation:=PNTFSVolumeInformation(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if FDataSize >= ntfsVolumeInformationSize then
  begin
   {Write Data}
   NTFSVolumeInformation.MajorVersion:=FMajorVersion;
   NTFSVolumeInformation.MinorVersion:=FMinorVersion;
   NTFSVolumeInformation.VolumeFlags:=FVolumeFlags;
  end;
 
 {Update Offset}
 Inc(AOffset,FDataSize);
 
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TNTFSDataAttribute}
constructor TNTFSDataAttribute.Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
begin
 {}
 inherited Create(ALocalLock,AParent);
 FAttributeType:=ntfsAttrTypeData;
end;

{==============================================================================}

destructor TNTFSDataAttribute.Destroy;
begin
 {}
 inherited Destroy;
end;

{==============================================================================}

procedure TNTFSDataAttribute.SetDataSize(AValue:LongWord);
var
 Size:LongWord;
 Buffer:Pointer;
begin
 {}
 //To Do //Lock 
 
 if AValue = 0 then
  begin
   if FData <> nil then FreeMem(FData);
   FDataSize:=0;
   FData:=nil;
  end
 else
  begin
   Buffer:=FData;
   Size:=FDataSize;
   FDataSize:=AValue;
   FData:=AllocMem(FDataSize);
   
   if Buffer <> nil then
    begin
     if FData <> nil then System.Move(Buffer^,FData^,Min(Size,FDataSize));
     
     FreeMem(Buffer);
    end;
  end;
end;

{==============================================================================}

function TNTFSDataAttribute.UpdateEntry(AEntry:TNTFSDiskEntry):Boolean;
begin
 {}
 Result:=False;
 
 {Check Entry}
 if AEntry <> nil then
  begin
   if (AEntry.Attributes and faStream) = faNone then
    begin
     {Update Entry}
     AEntry.Size:=FDataSize;
     AEntry.Used:=0;
     AEntry.Allocated:=NTFSRoundLongWordTo8Bytes(FDataSize);
     if FNonResident = ntfsAttributeNonResident then AEntry.Size:=FStreamSize;
     if FNonResident = ntfsAttributeNonResident then AEntry.Used:=FStreamUsed;
     if FNonResident = ntfsAttributeNonResident then AEntry.Allocated:=FStreamAllocated;
     
     Result:=True;
    end
   else
    begin
     {Update Entry}
     AEntry.Name:=NTFSAttributeNameToStreamName(FAttributeType,AttributeName); {Format name as :<Name>:<Type>}
     AEntry.AltName:=AttributeName; {AEntry.AltName:=ntfsBlankName;} {Store Attribute name as AltName for lookups}
     AEntry.Size:=FDataSize;
     AEntry.Used:=0;
     AEntry.Allocated:=NTFSRoundLongWordTo8Bytes(FDataSize);
     if FNonResident = ntfsAttributeNonResident then AEntry.Size:=FStreamSize;
     if FNonResident = ntfsAttributeNonResident then AEntry.Used:=FStreamUsed;
     if FNonResident = ntfsAttributeNonResident then AEntry.Allocated:=FStreamAllocated;
     
     Result:=True;
    end;
  end;
end;

{==============================================================================}

function TNTFSDataAttribute.UpdateAttribute(AEntry:TNTFSDiskEntry):Boolean;
{Note: This cannot be used as the DataSize or StreamSize must be updated by Size Attribute}
begin
 {}
 Result:=False;
 
 {Check Entry}
 if AEntry <> nil then
  begin
   if (AEntry.Attributes and faStream) = faNone then
    begin
     {Update Attribute}
     if FNonResident = ntfsAttributeResident then FDataSize:=AEntry.Size else FStreamSize:=AEntry.Size;
     if FNonResident = ntfsAttributeNonResident then FStreamUsed:=AEntry.Used;
     if FNonResident = ntfsAttributeNonResident then FStreamAllocated:=AEntry.Allocated;
     
     Result:=True;
    end
   else
    begin
     {Update Attribute}
     AttributeName:=NTFSStreamNameToAttributeName(FAttributeType,AEntry.Name); {Extract name from :<Name>:<Type>}
     if FNonResident = ntfsAttributeResident then FDataSize:=AEntry.Size else FStreamSize:=AEntry.Size;
     if FNonResident = ntfsAttributeNonResident then FStreamUsed:=AEntry.Used;
     if FNonResident = ntfsAttributeNonResident then FStreamAllocated:=AEntry.Allocated;
     
     Result:=True;
    end;
  end;
end;

{==============================================================================}
{==============================================================================}
{TNTFSIndexRootAttribute}
constructor TNTFSIndexRootAttribute.Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
begin
 {}
 inherited Create(ALocalLock,AParent);
 FStatus:=ntfsStatusManaged; {Managed Attribute}
 FAttributeType:=ntfsAttrTypeIndexRoot;

 FIndexType:=ntfsAttrTypeNone;
 FCollateRule:=ntfsCollateTypeBinary;
 FIndexRecordSize:=0;
 FIndexCounterOffset:=0;
end;

{==============================================================================}

destructor TNTFSIndexRootAttribute.Destroy;
begin
 {}
 inherited Destroy;
end;

{==============================================================================}

function TNTFSIndexRootAttribute.CreateIndex(AVersion,ASector:Word):Boolean;
{Create an index setup properties do not update Attribute}
begin
 {}
 Result:=False;
 
 if FIndex <> nil then Exit;
 
 {Check Type}
 if FIndexType = ntfsAttrTypeNone then
  begin
   if FCollateRule <> ntfsCollateTypeSecurityHash then
    begin
     {Create Index (Data)}
     FIndex:=TNTFSDataIndex.Create(GetKeyLocal,GetIndexLock,AVersion,ASector,FIndexType,FCollateRule,Self);
     FIndex.FIndexType:=FIndexType;
     FIndex.FCollateRule:=FCollateRule;
     FIndex.FIndexRecordSize:=FIndexRecordSize;
     FIndex.FIndexCounterOffset:=FIndexCounterOffset;
     
     Result:=True;
    end
   else
    begin
     {Create Index (Padded)}
     FIndex:=TNTFSPaddedIndex.Create(GetKeyLocal,GetIndexLock,AVersion,ASector,FIndexType,FCollateRule,Self);
     FIndex.FIndexType:=FIndexType;
     FIndex.FCollateRule:=FCollateRule;
     FIndex.FIndexRecordSize:=FIndexRecordSize;
     FIndex.FIndexCounterOffset:=FIndexCounterOffset;
     
     Result:=True;
    end;
  end
 else
  begin
   {Create Index (Attribute)}
   FIndex:=TNTFSAttributeIndex.Create(GetKeyLocal,GetIndexLock,AVersion,ASector,FIndexType,FCollateRule,Self);
   FIndex.FIndexType:=FIndexType;
   FIndex.FCollateRule:=FCollateRule;
   FIndex.FIndexRecordSize:=FIndexRecordSize;
   FIndex.FIndexCounterOffset:=FIndexCounterOffset;
   
   Result:=True;
  end
end;

{==============================================================================}

function TNTFSIndexRootAttribute.NewIndex(AVersion,ASector:Word;AType,ARule,ASize,AOffset:LongWord):Boolean;
{Setup parameters and create an index}
begin
 {}
 Result:=False;
 
 {Setup Parameters}
 FIndexType:=AType;
 FCollateRule:=ARule;
 FIndexRecordSize:=ASize;
 FIndexCounterOffset:=AOffset;
 
 {Create Index}
 Result:=CreateIndex(AVersion,ASector);
 if not Result then Exit;
 
 {Setup Index}
 FIndex.EmptyBtree;
end;

{==============================================================================}

function TNTFSIndexRootAttribute.CalculatedStreamSize(AVersion:Word):Int64;
{See notes in virtual base method}
var
 Node:TNTFSDiskNode;
begin
 {}
 Result:=ntfsIndexRootSize;
 
 if FIndex <> nil then
  begin
   Node:=FIndex.RootNode;
   if Node <> nil then Inc(Result,Node.IndexSize);
  end;
end;

{==============================================================================}

function TNTFSIndexRootAttribute.ReadData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean;
{Read the resident attribute data from the supplied buffer at the supplied offset}
var
 Size:LongWord;
 Offset:LongWord;
 NTFSIndexRoot:PNTFSIndexRoot;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {Check Resident}
 if FNonResident = ntfsAttributeNonResident then Exit;
 
 {Get Data}
 NTFSIndexRoot:=PNTFSIndexRoot(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if FDataSize >= ntfsIndexRootSize then
  begin
   {Read Data}
   FIndexType:=NTFSIndexRoot.IndexType;
   FCollateRule:=NTFSIndexRoot.CollateRule;
   FIndexRecordSize:=NTFSIndexRoot.IndexRecordSize;
   FIndexCounterOffset:=NTFSIndexRoot.IndexCounterOffset;
   
   {Set Offset}
   Size:=FDataSize - ntfsIndexRootSize;
   Offset:=AOffset + ntfsIndexRootSize;
   
   {Create Index}
   if not CreateIndex(AVersion,0) then Exit; {Created here as Index Root is always Resident}
   
   {Read Root}
   if not FIndex.ReadRoot(ABuffer,Offset,Size,AVersion) then Exit;
  end;
  
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSIndexRootAttribute.ReadData - Offset = ' + IntToStr(AOffset));
 {$ENDIF}
 
 {Update Offset}
 Inc(AOffset,FDataSize);
 
 Result:=True;
end;

{==============================================================================}

function TNTFSIndexRootAttribute.WriteData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean;
{Write the resident attribute data to the supplied buffer at the supplied offset}
var
 Size:LongWord;
 Offset:LongWord;
 NTFSIndexRoot:PNTFSIndexRoot;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSIndexRootAttribute.WriteData - Offset = ' + IntToStr(AOffset));
 {$ENDIF}
 
 {Check Resident}
 if FNonResident = ntfsAttributeNonResident then Exit;
 
 {Get Data}
 NTFSIndexRoot:=PNTFSIndexRoot(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if FDataSize >= ntfsIndexRootSize then
  begin
   {Write Data}
   NTFSIndexRoot.IndexType:=FIndexType;
   NTFSIndexRoot.CollateRule:=FCollateRule;
   NTFSIndexRoot.IndexRecordSize:=FIndexRecordSize;
   NTFSIndexRoot.IndexCounterOffset:=FIndexCounterOffset;
   
   {Set Offset}
   Size:=FDataSize - ntfsIndexRootSize;
   Offset:=AOffset + ntfsIndexRootSize;
   
   {Check Index}
   if FIndex = nil then Exit;
   
   {Write Root}
   if not FIndex.WriteRoot(ABuffer,Offset,Size,AVersion) then Exit;
  end;
  
 {Update Offset}
 Inc(AOffset,FDataSize);
 
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TNTFSIndexAllocationAttribute}
constructor TNTFSIndexAllocationAttribute.Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
begin
 {}
 inherited Create(ALocalLock,AParent);
 FAttributeType:=ntfsAttrTypeIndexAllocation;
end;

{==============================================================================}

destructor TNTFSIndexAllocationAttribute.Destroy;
begin
 {}
 inherited Destroy;
end;

{==============================================================================}

procedure TNTFSIndexAllocationAttribute.SetDataSize(AValue:LongWord);
var
 Size:LongWord;
 Buffer:Pointer;
begin
 {}
 //To Do //Lock 
 
 if AValue = 0 then
  begin
   if FData <> nil then FreeMem(FData);
   FDataSize:=0;
   FData:=nil;
  end
 else
  begin
   Buffer:=FData;
   Size:=FDataSize;
   FDataSize:=AValue;
   FData:=AllocMem(FDataSize);
   
   if Buffer <> nil then
    begin
     if FData <> nil then System.Move(Buffer^,FData^,Min(Size,FDataSize));
     
     FreeMem(Buffer);
    end;
  end;
end;

{==============================================================================}
{==============================================================================}
{TNTFSBitmapAttribute}
constructor TNTFSBitmapAttribute.Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
begin
 {}
 inherited Create(ALocalLock,AParent);
 FAttributeType:=ntfsAttrTypeBitmap;

 FBitmap:=nil;
 FBitmapSize:=0;
end;

{==============================================================================}

destructor TNTFSBitmapAttribute.Destroy;
begin
 {}
 if FBitmap <> nil then FreeMem(FBitmap);
 inherited Destroy;
end;

{==============================================================================}

procedure TNTFSBitmapAttribute.SetDataSize(AValue:LongWord);
var
 Size:LongWord;
 Buffer:Pointer;
begin
 {}
 //To Do //Lock 
 
 if AValue = 0 then
  begin
   if FData <> nil then FreeMem(FData);
   FDataSize:=0;
   FData:=nil;
  end
 else
  begin
   Buffer:=FData;
   Size:=FDataSize;
   FDataSize:=AValue;
   FData:=AllocMem(FDataSize);
   
   if Buffer <> nil then
    begin
     if FData <> nil then System.Move(Buffer^,FData^,Min(Size,FDataSize));
     
     FreeMem(Buffer);
    end;
  end;
end;

{==============================================================================}

procedure TNTFSBitmapAttribute.SetBitmapSize(AValue:LongWord);
var
 Size:LongWord;
 Buffer:Pointer;
begin
 {}
 if AValue = 0 then
  begin
   if FBitmap <> nil then FreeMem(FBitmap);
   FBitmapSize:=0;
   FBitmap:=nil;
  end
 else
  begin
   Buffer:=FBitmap;
   Size:=FBitmapSize;
   FBitmapSize:=AValue;
   FBitmap:=AllocMem(FBitmapSize);
   
   if Buffer <> nil then
    begin
     if FBitmap <> nil then System.Move(Buffer^,FBitmap^,Min(Size,FBitmapSize));
     
     FreeMem(Buffer);
    end;
  end;
end;

{==============================================================================}

function TNTFSBitmapAttribute.CalculatedStreamSize(AVersion:Word):Int64;
{See notes in virtual base method}
begin
 {}
 Result:=0;
 
 if FBitmap <> nil then Result:=FBitmapSize else Result:=FDataSize;
end;

{==============================================================================}
{==============================================================================}
{TNTFSReparsePointAttribute}
constructor TNTFSReparsePointAttribute.Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
begin
 {}
 inherited Create(ALocalLock,AParent);
 FStatus:=ntfsStatusManaged; {Managed Attribute}
 FAttributeType:=ntfsAttrTypeReparsePoint;

 FReparseTag:=ntfsReparseTagNone;
 FReparseSize:=0;
 FReparseGUID:=ntfsNullGUID;

 FReparse:=nil;
end;

{==============================================================================}

destructor TNTFSReparsePointAttribute.Destroy;
begin
 {}
 if FReparse <> nil then FReparse.Free;
 inherited Destroy;
end;

{==============================================================================}

function TNTFSReparsePointAttribute.GetIsAlias:Boolean;
begin
 {}
 Result:=((FReparseTag and ntfsReparseTagFlagIsAlias) = ntfsReparseTagFlagIsAlias);
end;

{==============================================================================}

procedure TNTFSReparsePointAttribute.SetIsAlias(AValue:Boolean);
begin
 {}
 if AValue then
  begin
   FReparseTag:=(FReparseTag or ntfsReparseTagFlagIsAlias);
  end
 else
  begin
   FReparseTag:=(FReparseTag and not ntfsReparseTagFlagIsAlias);
  end;
end;

{==============================================================================}

function TNTFSReparsePointAttribute.GetIsHighLatency:Boolean;
begin
 {}
 Result:=((FReparseTag and ntfsReparseTagFlagIsHighLatency) = ntfsReparseTagFlagIsHighLatency);
end;

{==============================================================================}

procedure TNTFSReparsePointAttribute.SetIsHighLatency(AValue:Boolean);
begin
 {}
 if AValue then
  begin
   FReparseTag:=(FReparseTag or ntfsReparseTagFlagIsHighLatency);
  end
 else
  begin
   FReparseTag:=(FReparseTag and not ntfsReparseTagFlagIsHighLatency);
  end;
end;

{==============================================================================}

function TNTFSReparsePointAttribute.GetIsMicrosoft:Boolean;
begin
 {}
 Result:=((FReparseTag and ntfsReparseTagFlagIsMicrosoft) = ntfsReparseTagFlagIsMicrosoft);
end;

{==============================================================================}

procedure TNTFSReparsePointAttribute.SetIsMicrosoft(AValue:Boolean);
begin
 {}
 if AValue then
  begin
   FReparseTag:=(FReparseTag or ntfsReparseTagFlagIsMicrosoft);
  end
 else
  begin
   FReparseTag:=(FReparseTag and not ntfsReparseTagFlagIsMicrosoft);
  end;
end;

{==============================================================================}

function TNTFSReparsePointAttribute.CreateReparse:Boolean;
{Create a reparse, setup properties do not update Attribute}
begin
 {}
 Result:=False;
 
 if FReparse <> nil then Exit;
 
 {Check Type}
 case FReparseTag of
  ntfsReparseTagSymbolicLink:begin
    {Create Reparse Sym Link}
    FReparse:=TNTFSReparseSymLink.Create(INVALID_HANDLE_VALUE,Self); //To Do //Critical
    
    Result:=True;
   end;
  ntfsReparseTagMountPoint:begin
    {Create Reparse Mount Point}
    FReparse:=TNTFSReparseMountPoint.Create(INVALID_HANDLE_VALUE,Self); //To Do //Critical
    
    Result:=True;
   end;
  else
   begin
    {Create Reparse}
    FReparse:=TNTFSReparse.Create(INVALID_HANDLE_VALUE,Self); //To Do //Critical
    
    Result:=True;
   end;
 end;
end;

{==============================================================================}

function TNTFSReparsePointAttribute.NewReparse(AReparseTag:LongWord):Boolean;
{Create a reparse, setup properties and update Attribute}
begin
 {}
 Result:=False;
 
 if FReparse <> nil then Exit;
 
 FReparseTag:=AReparseTag;
 
 Result:=CreateReparse;
 if not Result then Exit;
 
 {Setup Reparse}
 {Nothing}
end;

{==============================================================================}

function TNTFSReparsePointAttribute.CalculatedStreamSize(AVersion:Word):Int64;
{See notes in virtual base method}
begin
 {}
 Result:=0;
 
 if IsMicrosoft then
  begin
   Result:=ntfsReparsePointMicrosoftSize + FReparseSize;
  end
 else
  begin
   Result:=ntfsReparsePointOtherSize + FReparseSize;
  end;
end;

{==============================================================================}

function TNTFSReparsePointAttribute.ReadReparse(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Read the attribute reparse data from the supplied buffer at the supplied offset}
var
 ReparsePoint:PNTFSReparsePoint;
 ReparsePointOther:PNTFSReparsePointOther;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {Get Data}
 ReparsePoint:=PNTFSReparsePoint(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if ASize >= ntfsReparsePointSize then
  begin
   {Read Data}
   FReparseTag:=ReparsePoint.ReparseTag;
   FReparseSize:=ReparsePoint.ReparseSize;
   
   {Check Tag}
   if (FReparseTag and ntfsReparseTagFlagIsMicrosoft) = ntfsReparseTagFlagIsMicrosoft then
    begin
     {Update Offset}
     Dec(ASize,ntfsReparsePointSize);
     Inc(AOffset,ntfsReparsePointSize);
     
     {Check Size}
     {if ASize >= ntfsReparseSize then}
     if ASize >= FReparseSize then
      begin
       {Create Reparse}
       if not CreateReparse then Exit;
       
       {Read Reparse}
       if not FReparse.ReadReparse(ABuffer,AOffset,ASize,AVersion) then Exit;
      end;
    end
   else
    begin
     {Get Data}
     ReparsePointOther:=PNTFSReparsePointOther(LongWord(ABuffer) + AOffset);
     
     {Check Size}
     if ASize >= ntfsReparsePointOtherSize then
      begin
       {Read Data}
       FReparseGUID:=ReparsePointOther.ReparseGUID;
       
       {Update Offset}
       Dec(ASize,ntfsReparsePointOtherSize);
       Inc(AOffset,ntfsReparsePointOtherSize);
       
       {Check Size}
       {if ASize >= ntfsReparseSize then}
       if ASize >= FReparseSize then
        begin
         {Create Reparse}
         if not CreateReparse then Exit;
         
         {Read Reparse}
         if not FReparse.ReadReparse(ABuffer,AOffset,ASize,AVersion) then Exit;
        end;
      end;
    end;
  end;
  
 Result:=True;
end;

{==============================================================================}

function TNTFSReparsePointAttribute.WriteReparse(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Write the attribute reparse data to the supplied buffer at the supplied offset}
var
 ReparsePoint:PNTFSReparsePoint;
 ReparsePointOther:PNTFSReparsePointOther;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {Get Data}
 ReparsePoint:=PNTFSReparsePoint(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if ASize >= ntfsReparsePointSize then
  begin
   {Write Data}
   ReparsePoint.ReparseTag:=ReparseTag;
   ReparsePoint.ReparseSize:=ReparseSize;
   
   {Check Tag}
   if (ReparseTag and ntfsReparseTagFlagIsMicrosoft) = ntfsReparseTagFlagIsMicrosoft then
    begin
     {Update Offset}
     Dec(ASize,ntfsReparsePointSize);
     Inc(AOffset,ntfsReparsePointSize);
     
     {Check Size}
     {if ASize >= ntfsReparseSize then}
     if ASize >= ReparseSize then
      begin
       {Check Reparse}
       if FReparse = nil then Exit;
       
       {Write Reparse}
       if not FReparse.WriteReparse(ABuffer,AOffset,ASize,AVersion) then Exit;
      end;
    end
   else
    begin
     {Get Data}
     ReparsePointOther:=PNTFSReparsePointOther(LongWord(ABuffer) + AOffset);
     
     {Check Size}
     if ASize >= ntfsReparsePointOtherSize then
      begin
       {Write Data}
       ReparsePointOther.ReparseGUID:=ReparseGUID;
       
       {Update Offset}
       Dec(ASize,ntfsReparsePointOtherSize);
       Inc(AOffset,ntfsReparsePointOtherSize);
       
       {Check Size}
       {if ASize >= ntfsReparseSize then}
       if ASize >= ReparseSize then
        begin
         {Check Reparse}
         if FReparse = nil then Exit;
         
         {Write Reparse}
         if not FReparse.WriteReparse(ABuffer,AOffset,ASize,AVersion) then Exit;
        end;
      end;
    end;
  end;
  
 Result:=True;
end;

{==============================================================================}

function TNTFSReparsePointAttribute.ReadData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean;
{Read the resident attribute data from the supplied buffer at the supplied offset}
var
 Size:LongWord;
 Offset:LongWord;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {Check Resident}
 if FNonResident = ntfsAttributeNonResident then Exit;
 
 {Check Size}
 if FDataSize >= ntfsReparsePointSize then
  begin
   {Set Offset}
   Size:=FDataSize;
   Offset:=AOffset;
   
   {Read Reparse}
   if not ReadReparse(ABuffer,Offset,Size,AVersion) then Exit;
  end;
  
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSReparsePointAttribute.ReadData - Offset = ' + IntToStr(AOffset));
 {$ENDIF}
 
 {Update Offset}
 Inc(AOffset,FDataSize);
 
 Result:=True;
end;

{==============================================================================}

function TNTFSReparsePointAttribute.WriteData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean;
{Write the resident attribute data to the supplied buffer at the supplied offset}
var
 Size:LongWord;
 Offset:LongWord;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSReparsePointAttribute.WriteData - Offset = ' + IntToStr(AOffset));
 {$ENDIF}
 
 {Check Resident}
 if FNonResident = ntfsAttributeNonResident then Exit;
 
 {Check Size}
 if FDataSize >= ntfsReparsePointSize then
  begin
   {Set Offset}
   Size:=FDataSize;
   Offset:=AOffset;
   
   {Write Reparse}
   if not WriteReparse(ABuffer,Offset,Size,AVersion) then Exit;
  end;
  
 {Update Offset}
 Inc(AOffset,FDataSize);
 
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TNTFSSymbolicLinkAttribute}
constructor TNTFSSymbolicLinkAttribute.Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
begin
 {}
 inherited Create(ALocalLock,AParent);
 FAttributeType:=ntfsAttrTypeSymbolicLink;
end;

{==============================================================================}

destructor TNTFSSymbolicLinkAttribute.Destroy;
begin
 {}
 inherited Destroy;
end;

{==============================================================================}

procedure TNTFSSymbolicLinkAttribute.SetDataSize(AValue:LongWord);
var
 Size:LongWord;
 Buffer:Pointer;
begin
 {}
 //To Do //Lock 
 
 if AValue = 0 then
  begin
   if FData <> nil then FreeMem(FData);
   FDataSize:=0;
   FData:=nil;
  end
 else
  begin
   Buffer:=FData;
   Size:=FDataSize;
   FDataSize:=AValue;
   FData:=AllocMem(FDataSize);
   
   if Buffer <> nil then
    begin
     if FData <> nil then System.Move(Buffer^,FData^,Min(Size,FDataSize));
     
     FreeMem(Buffer);
    end;
  end;
end;

{==============================================================================}
{==============================================================================}
{TNTFSExtendedAttrInformationAttribute}
constructor TNTFSExtendedAttrInformationAttribute.Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
begin
 {}
 inherited Create(ALocalLock,AParent);
 FStatus:=ntfsStatusFixed or ntfsStatusManaged; {Fixed Size, Managed Attribute}
 FAttributeType:=ntfsAttrTypeExtendedAttrInformation;

 FPackedSize:=0;
 FFlagCount:=0;
 FUnpackedSize:=0;
end;

{==============================================================================}

destructor TNTFSExtendedAttrInformationAttribute.Destroy;
begin
 {}
 inherited Destroy;
end;

{==============================================================================}

function TNTFSExtendedAttrInformationAttribute.CalculatedStreamSize(AVersion:Word):Int64;
{See notes in virtual base method}
begin
 {}
 Result:=ntfsExtendedAttrInformationSize;
end;

{==============================================================================}

function TNTFSExtendedAttrInformationAttribute.ReadData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean;
{Read the resident attribute data from the supplied buffer at the supplied offset}
var
 NTFSExtendedAttrInformation:PNTFSExtendedAttrInformation;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {Check Resident}
 if FNonResident = ntfsAttributeNonResident then Exit;
 
 {Get Data}
 NTFSExtendedAttrInformation:=PNTFSExtendedAttrInformation(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if FDataSize >= ntfsExtendedAttrInformationSize then
  begin
   {Read Data}
   FPackedSize:=NTFSExtendedAttrInformation.PackedSize;
   FFlagCount:=NTFSExtendedAttrInformation.FlagCount;
   FUnpackedSize:=NTFSExtendedAttrInformation.UnpackedSize;
  end;
  
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSExtendedAttrInformationAttribute.ReadData - Offset = ' + IntToStr(AOffset));
 {$ENDIF}
 
 {Update Offset}
 Inc(AOffset,FDataSize);
 
 Result:=True;
end;

{==============================================================================}

function TNTFSExtendedAttrInformationAttribute.WriteData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean;
{Write the resident attribute data to the supplied buffer at the supplied offset}
var
 NTFSExtendedAttrInformation:PNTFSExtendedAttrInformation;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSExtendedAttrInformationAttribute.WriteData - Offset = ' + IntToStr(AOffset));
 {$ENDIF}
 
 {Check Resident}
 if FNonResident = ntfsAttributeNonResident then Exit;
 
 {Get Data}
 NTFSExtendedAttrInformation:=PNTFSExtendedAttrInformation(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if FDataSize >= ntfsExtendedAttrInformationSize then
  begin
   {Write Data}
   NTFSExtendedAttrInformation.PackedSize:=PackedSize;
   NTFSExtendedAttrInformation.FlagCount:=FlagCount;
   NTFSExtendedAttrInformation.UnpackedSize:=UnpackedSize;
  end;
 
 {Update Offset}
 Inc(AOffset,FDataSize);
 
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TNTFSExtendedAttrAttribute}
constructor TNTFSExtendedAttrAttribute.Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
begin
 {}
 inherited Create(ALocalLock,AParent);
 FStatus:=ntfsStatusManaged; {Managed Attribute}
 FAttributeType:=ntfsAttrTypeExtendedAttr;

 FExtendeds:=nil;
end;

{==============================================================================}

destructor TNTFSExtendedAttrAttribute.Destroy;
begin
 {}
 if FExtendeds <> nil then FExtendeds.Free;
 inherited Destroy;
end;

{==============================================================================}

function TNTFSExtendedAttrAttribute.CreateExtended:TNTFSExtended;
{Create an extended, add to list do not update Attribute}
var
 Extended:TNTFSExtended;
begin
 {}
 Result:=nil;
 
 if FExtendeds = nil then FExtendeds:=TNTFSExtendeds.Create(GetExtendedLocal,GetExtendedsLock);
 
 {Create Extended}
 Extended:=TNTFSExtended.Create(GetExtendedLocal);
 
 {Add Extended}
 FExtendeds.Add(Extended);
 
 Result:=Extended;
end;

{==============================================================================}

function TNTFSExtendedAttrAttribute.DestroyExtended(AExtended:TNTFSExtended):Boolean;
{Remove the extended from the list and free, do not update Attribute}
begin
 {}
 Result:=False;
 
 if AExtended = nil then Exit;
 if FExtendeds = nil then Exit;
 
 {Remove Extended}
 FExtendeds.Remove(AExtended);
 
 {Free Extended}
 AExtended.Free;
 
 Result:=True;
end;

{==============================================================================}

function TNTFSExtendedAttrAttribute.NewExtended(const AName:String):TNTFSExtended;
{Create an extended, add to list and update Attribute}
begin
 {}
 Result:=CreateExtended;
 if Result = nil then Exit;
 
 {Setup Extended}
 Result.ExtendedName:=AName;
end;

{==============================================================================}

function TNTFSExtendedAttrAttribute.GetExtended(const AName:String):TNTFSExtended;
{Get an extended by name}
var
 Hash:LongWord;
 Wildcard:Boolean;
 Extended:TNTFSExtended;
begin
 {}
 Result:=nil;
 
 if FExtendeds = nil then Exit;
 
 {Calculate Hash}
 Hash:=0;                                                          //To Do //Testing4
 Wildcard:=(Length(AName) = ntfsAnyNameLength) and (AName = ntfsAnyName); {Modified 14/2/2011}
 if not Wildcard then Hash:=GenerateNameHash(AName,NAME_HASH_SIZE); //To Do //Testing4
 
 {Check Extendeds}
 Extended:=TNTFSExtended(FExtendeds.First);
 while Extended <> nil do
  begin
   if Wildcard or (Extended.ExtendedHash = Hash) then
    begin
     if Wildcard or (Uppercase(Extended.ExtendedName) = Uppercase(AName)) then
      begin
       Result:=Extended;
       Exit;
      end;
    end;
    
   Extended:=TNTFSExtended(Extended.Next);
  end;
end;

{==============================================================================}

function TNTFSExtendedAttrAttribute.RemoveExtended(AExtended:TNTFSExtended):Boolean;
{Remove the extended from the list, free and update attribute}
begin
 {}
 Result:=False;
 
 if AExtended = nil then Exit;
 if FExtendeds = nil then Exit;
 
 {Remove Extended}
 FExtendeds.Remove(AExtended);
 
 {Free Extended}
 AExtended.Free;
 
 Result:=True;
end;

{==============================================================================}

function TNTFSExtendedAttrAttribute.ExtendedCount:LongWord;
begin
 {}
 Result:=0;
 
 if FExtendeds = nil then Exit;
 
 Result:=FExtendeds.Count;
end;

{==============================================================================}

function TNTFSExtendedAttrAttribute.CalculatedStreamSize(AVersion:Word):Int64;
{See notes in virtual base method}
begin
 {}
 Result:=0;
 
 if FExtendeds <> nil then Result:=FExtendeds.TotalSize;
end;

{==============================================================================}

function TNTFSExtendedAttrAttribute.ReadExtendeds(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Read the attribute extended attributes from the supplied buffer at the supplied offset}
var
 Extended:TNTFSExtended;
 ExtendedData:PNTFSExtendedData;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {Check Size}
 while ASize >= ntfsExtendedSize do
  begin
   {Get Data}
   ExtendedData:=PNTFSExtendedData(LongWord(ABuffer) + AOffset);
   
   {Create Extended}
   Extended:=CreateExtended;
   if Extended = nil then Exit;
   
   {Read Extended}
   if not Extended.ReadExtended(ABuffer,AOffset,ASize,AVersion) then Exit;
   
   {Check Last}
   if ExtendedData.ExtendedOffset = 0 then Break;
  end;
  
 Result:=True;
end;

{==============================================================================}

function TNTFSExtendedAttrAttribute.WriteExtendeds(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Write the attribute extended attributes to the supplied buffer at the supplied offset}
var
 Extended:TNTFSExtended;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {Get Extended}
 if Extendeds = nil then Exit;
 Extended:=TNTFSExtended(Extendeds.First);
 
 {Write Extendeds}
 while Extended <> nil do
  begin
   {Check Size}
   if ASize < ntfsExtendedSize then Exit;
   
   {Write Extended}
   if not Extended.WriteExtended(ABuffer,AOffset,ASize,AVersion) then Exit;
   
   {Get Extended}
   Extended:=TNTFSExtended(Extended.Next);
  end;
  
 Result:=True;
end;

{==============================================================================}

function TNTFSExtendedAttrAttribute.ReadData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean;
{Read the resident attribute data from the supplied buffer at the supplied offset}
var
 Size:LongWord;
 Offset:LongWord;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {Check Resident}
 if FNonResident = ntfsAttributeNonResident then Exit;
 
 {Check Size}
 if FDataSize > ntfsExtendedAttrSize then {Not Equal}
  begin
   {Set Offset}
   Size:=FDataSize;
   Offset:=AOffset;
   
   {Read Extendeds}
   if not ReadExtendeds(ABuffer,Offset,Size,AVersion) then Exit;
  end;
  
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSExtendedAttrAttribute.ReadData - Offset = ' + IntToStr(AOffset));
 {$ENDIF}
 
 {Update Offset}
 Inc(AOffset,FDataSize);
 
 Result:=True;
end;

{==============================================================================}

function TNTFSExtendedAttrAttribute.WriteData(ABuffer:Pointer;var AOffset:LongWord;AVersion:Word):Boolean;
{Write the resident attribute data to the supplied buffer at the supplied offset}
var
 Size:LongWord;
 Offset:LongWord;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSExtendedAttrAttribute.WriteData - Offset = ' + IntToStr(AOffset));
 {$ENDIF}
 
 {Check Resident}
 if FNonResident = ntfsAttributeNonResident then Exit;
 
 {Check Size}
 if FDataSize > ntfsExtendedAttrSize then {Not Equal}
  begin
   {Set Offset}
   Size:=FDataSize;
   Offset:=AOffset;
   
   {Write Extendeds}
   if not WriteExtendeds(ABuffer,Offset,Size,AVersion) then Exit;
  end;
  
 {Update Offset}
 Inc(AOffset,FDataSize);
 
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TNTFSPropertySetAttribute}
constructor TNTFSPropertySetAttribute.Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
begin
 {}
 inherited Create(ALocalLock,AParent);
 FAttributeType:=ntfsAttrTypePropertySet;
end;

{==============================================================================}

destructor TNTFSPropertySetAttribute.Destroy;
begin
 {}
 inherited Destroy;
end;

{==============================================================================}

procedure TNTFSPropertySetAttribute.SetDataSize(AValue:LongWord);
var
 Size:LongWord;
 Buffer:Pointer;
begin
 {}
 //To Do //Lock 
 
 if AValue = 0 then
  begin
   if FData <> nil then FreeMem(FData);
   FDataSize:=0;
   FData:=nil;
  end
 else
  begin
   Buffer:=FData;
   Size:=FDataSize;
   FDataSize:=AValue;
   FData:=AllocMem(FDataSize);
   
   if Buffer <> nil then
    begin
     if FData <> nil then System.Move(Buffer^,FData^,Min(Size,FDataSize));
     
     FreeMem(Buffer);
    end;
  end;
end;

{==============================================================================}
{==============================================================================}
{TNTFSLoggedUtilityStreamAttribute}
constructor TNTFSLoggedUtilityStreamAttribute.Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
begin
 {}
 inherited Create(ALocalLock,AParent);
 FAttributeType:=ntfsAttrTypeLoggedUtilityStream;
end;

{==============================================================================}

destructor TNTFSLoggedUtilityStreamAttribute.Destroy;
begin
 {}
 inherited Destroy;
end;

{==============================================================================}

procedure TNTFSLoggedUtilityStreamAttribute.SetDataSize(AValue:LongWord);
var
 Size:LongWord;
 Buffer:Pointer;
begin
 {}
 //To Do //Lock 
 
 if AValue = 0 then
  begin
   if FData <> nil then FreeMem(FData);
   FDataSize:=0;
   FData:=nil;
  end
 else
  begin
   Buffer:=FData;
   Size:=FDataSize;
   FDataSize:=AValue;
   FData:=AllocMem(FDataSize);
   
   if Buffer <> nil then
    begin
     if FData <> nil then System.Move(Buffer^,FData^,Min(Size,FDataSize));
     
     FreeMem(Buffer);
    end;
  end;
end;

{==============================================================================}
{==============================================================================}
{TNTFSEndAttribute}
constructor TNTFSEndAttribute.Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
begin
 {}
 inherited Create(ALocalLock,AParent);
 FStatus:=ntfsStatusFixed or ntfsStatusSingle or ntfsStatusUnlisted or ntfsStatusUnmovable or ntfsStatusManaged; {Fixed Size, Single Instance, Unlisted, Unmovable, Managed Attribute}
 FAttributeType:=ntfsAttrTypeEnd;
 FAttributeSize:=ntfsEndSize; {SizeOf(LongWord)}
end;

{==============================================================================}

destructor TNTFSEndAttribute.Destroy;
begin
 {}
 inherited Destroy;
end;

{==============================================================================}

function TNTFSEndAttribute.CalculatedSize(AVersion:Word):LongWord;
{See notes in virtual base method}
begin
 {}
 Result:=NTFSRoundLongWordTo8Bytes(ntfsEndSize); {ntfsEndSize;}
end;

{==============================================================================}

function TNTFSEndAttribute.CalculatedDataSize(AVersion:Word):LongWord;
{See notes in virtual base method}
begin
 {}
 Result:=0; {No data in an End Attribute}
end;

{==============================================================================}

function TNTFSEndAttribute.CalculatedStreamSize(AVersion:Word):Int64;
{See notes in virtual base method}
begin
 {}
 Result:=0; {No data in an End Attribute}
end;

{==============================================================================}

function TNTFSEndAttribute.ReadAttribute(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Read the attribute header and resident attribute data from the supplied buffer at the supplied offset}
var
 AttributeHeader:PNTFSAttributeHeader;
begin
 {}
 Result:=False; {No Inherited}
 
 if ABuffer = nil then Exit;
 
 {Get Attribute}
 AttributeHeader:=PNTFSAttributeHeader(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if ASize >= FAttributeSize then
  begin
   {Read Header}
   FAttributeType:=AttributeHeader.AttributeType;
   
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSEndAttribute.ReadAttribute - Offset = ' + IntToStr(AOffset));
   {$ENDIF}
   
   {Update Offset}
   Dec(ASize,FAttributeSize);
   Inc(AOffset,FAttributeSize);
   
   Result:=True;
  end;
end;

{==============================================================================}

function TNTFSEndAttribute.WriteAttribute(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Write the attribute header and resident attribute data to the supplied buffer at the supplied offset}
var
 AttributeHeader:PNTFSAttributeHeader;
begin
 {}
 Result:=False; {No Inherited}
 
 if ABuffer = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSDiskAttribute.WriteAttribute - Type = ' + IntToHex(FAttributeType,8) + ' Name = ' + AttributeName);
 {$ENDIF}
 
 {Get Attribute}
 AttributeHeader:=PNTFSAttributeHeader(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if ASize >= FAttributeSize then
  begin
   {Write Header}
   AttributeHeader.AttributeType:=FAttributeType;
   
   {Update Offset}
   Dec(ASize,FAttributeSize);
   Inc(AOffset,FAttributeSize);
   
   Result:=True;
  end;
end;

{==============================================================================}
{==============================================================================}
{TNTFSUnknownAttribute}
constructor TNTFSUnknownAttribute.Create(ALocalLock:TMutexHandle;AParent:TNTFSDiskRecord);
begin
 {}
 inherited Create(ALocalLock,AParent);
 {FAttributeType:=ntfsAttrTypeNone;} {Set by CreateAttribute}
end;

{==============================================================================}

destructor TNTFSUnknownAttribute.Destroy;
begin
 {}
 inherited Destroy;
end;

{==============================================================================}

procedure TNTFSUnknownAttribute.SetDataSize(AValue:LongWord);
var
 Size:LongWord;
 Buffer:Pointer;
begin
 {}
 //To Do //Lock 
 
 if AValue = 0 then
  begin
   if FData <> nil then FreeMem(FData);
   FDataSize:=0;
   FData:=nil;
  end
 else
  begin
   Buffer:=FData;
   Size:=FDataSize;
   FDataSize:=AValue;
   FData:=AllocMem(FDataSize);
   
   if Buffer <> nil then
    begin
     if FData <> nil then System.Move(Buffer^,FData^,Min(Size,FDataSize));
     
     FreeMem(Buffer);
    end;
  end;
end;

{==============================================================================}
{==============================================================================}
{TNTFSUpCase}
constructor TNTFSUpCase.Create;
begin
 {}
 inherited Create;
 FLock:=SynchronizerCreate;
 
 FData:=GetMem(ntfsFileSizeUpCase)
end;

{==============================================================================}

destructor TNTFSUpCase.Destroy;
begin
 {}
 WriterLock;
 try
  FreeMem(FData);
  inherited Destroy;
 finally
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  SynchronizerDestroy(FLock);
 end;
end;

{==============================================================================}

function TNTFSUpCase.ReaderLock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSUpCase.ReaderUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSUpCase.WriterLock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSUpCase.WriterUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSUpCase.Init(AVersion:Word):Boolean;
var
 Count:LongWord;
 Offset:LongWord;
begin
 {}
 Result:=False;
 
 if not WriterLock then Exit;
 try
  {Create default table}
  Count:=0;
  Offset:=0;
  while Offset < ntfsFileSizeUpCase do
   begin
    PWord(LongWord(FData) + Offset)^:=Count;
    Inc(Count);
    Inc(Offset,2);
   end;
  
  {Initialize default values}
  for Count:=0 to ntfsMaxUpcaseConvert do
   begin
    Offset:=(ntfsUpcaseConverts[Count].Count shl 1);
    PWord(LongWord(FData) + Offset)^:=ntfsUpcaseConverts[Count].Value;
   end;
  
  Result:=True;
 finally
  WriterUnlock;
 end;
end;

{==============================================================================}

function TNTFSUpCase.Convert(ASource,ADest:Pointer;ASize:LongWord):Boolean;
var
 Offset:Word;
 Count:LongWord;
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  if ASize = 0 then Exit;
  if ADest = nil then Exit;
  if ASource = nil then Exit;
  
  Count:=0;
  while Count < ASize do
   begin
    Offset:=PWord(LongWord(ASource) + Count)^;
    PWord(LongWord(ADest) + Count)^:=PWord(LongWord(FData) + Offset)^;
    Inc(Count,2);
   end;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TNTFSUpCase.ReadUpCase(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Read the upcase file data from the supplied buffer at the supplied offset}
var
 UpCaseData:PNTFSUpCaseData;
begin
 {}
 Result:=False;
 
 if not WriterLock then Exit;
 try
  if ABuffer = nil then Exit;
  
  {Get Data}
  UpCaseData:=PNTFSUpCaseData(LongWord(ABuffer) + AOffset);
  
  {Check Size}
  if ASize >= ntfsFileSizeUpCase then
   begin
    {Read Data}
    System.Move(UpCaseData.Data[0],FData^,ntfsFileSizeUpCase);
    
    {Update Offset}
    Dec(ASize,ntfsFileSizeUpCase);
    Inc(AOffset,ntfsFileSizeUpCase);
    
    Result:=True;
   end;
 finally
  WriterUnlock;
 end;
end;

{==============================================================================}

function TNTFSUpCase.WriteUpCase(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Write the upcase file data to the supplied buffer at the supplied offset}
var
 UpCaseData:PNTFSUpCaseData;
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  if ABuffer = nil then Exit;
  
  {Get Data}
  UpCaseData:=PNTFSUpCaseData(LongWord(ABuffer) + AOffset);
  
  {Check Size}
  if ASize >= ntfsFileSizeUpCase then
   begin
    {Write Data}
    System.Move(FData^,UpCaseData.Data[0],ntfsFileSizeUpCase);
    
    {Update Offset}
    Dec(ASize,ntfsFileSizeUpCase);
    Inc(AOffset,ntfsFileSizeUpCase);
    
    Result:=True;
   end;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}
{==============================================================================}
{TNTFSAttrDefs}
constructor TNTFSAttrDefs.Create;
begin
 {}
 inherited Create;
 FAttrDefLocal:=MutexCreate;
end;

{==============================================================================}

destructor TNTFSAttrDefs.Destroy; 
begin
 {}
 WriterLock;
 try
  MutexDestroy(FAttrDefLocal);
 finally
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  inherited Destroy;
 end;
end;

{==============================================================================}

function TNTFSAttrDefs.GetPrevious(AAttrDef:TNTFSAttrDef):TNTFSAttrDef;
{Note: Caller must hold the lock}
var
 Current:TNTFSAttrDef;
begin
 {}
 Result:=nil;
 
 if AAttrDef = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSAttrDefs.GetPrevious - Name = ' + AAttrDef.AttributeName + ' Type = ' + IntToHex(AAttrDef.AttributeType,8));
 {$ENDIF}
 
 {Check Items}
 Current:=TNTFSAttrDef(First);
 while Current <> nil do
  begin
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSAttrDefs.GetPrevious - Comparing with Name = ' + Current.AttributeName + ' Type = ' + IntToHex(Current.AttributeType,8));
   {$ENDIF}
   
   if AAttrDef.Compare(Current) = ntfsCompareGreater then Exit;
   
   Result:=Current;
   Current:=TNTFSAttrDef(Current.Next);
  end;
end;

{==============================================================================}

function TNTFSAttrDefs.CreateAttrDef(AType:LongWord;AVersion:Word;ANew:Boolean):TNTFSAttrDef;
{Create an attribute def, add to end of list}
var
 AttrDef:TNTFSAttrDef;
begin
 {}
 Result:=nil;
 
 if not WriterLock then Exit;
 try
  {Create AttrDef}
  AttrDef:=TNTFSAttrDef.Create(FAttrDefLocal);
  
  {Add AttrDef  (Only if not new)}
  if not ANew then Add(AttrDef);
  
  Result:=AttrDef;
 finally
  WriterUnlock;
 end;
end;

{==============================================================================}

function TNTFSAttrDefs.DestroyAttrDef(AAttrDef:TNTFSAttrDef):Boolean;
{Remove the attribute def from the list and free}
begin
 {}
 Result:=False;
 
 if not WriterLock then Exit;
 try
  if AAttrDef = nil then Exit;
  
  {Remove AttrDef}
  Remove(AAttrDef);
  
  {Free AttrDef}
  AAttrDef.Free;
  
  Result:=True;
 finally
  WriterUnlock;
 end;
end;

{==============================================================================}

function TNTFSAttrDefs.NewAttrDef(AType:LongWord;const AName:String;AVersion:Word):TNTFSAttrDef;
{Create an attribute def, insert in sorted list}
var
 Previous:TNTFSAttrDef;
begin
 {}
 Result:=CreateAttrDef(AType,AVersion,True);
 if Result = nil then Exit;
  
 if not WriterLock then Exit;
 try
  {Set Values}
  Result.AttributeName:=AName;
  Result.AttributeType:=AType;
  
  {Get Previous}
  Previous:=GetPrevious(Result);
  
  {Insert AttrDef}
  Insert(Previous,Result);
 finally
  WriterUnlock;
 end;
end;

{==============================================================================}

function TNTFSAttrDefs.GetAttrDef(AType:LongWord;const AName:String):TNTFSAttrDef;
{Get an attribute def by type and name}
var
 Hash:LongWord;
 Wildcard:Boolean;
 AttrDef:TNTFSAttrDef;
begin
 {}
 Result:=nil;
 
 if not ReaderLock then Exit;
 try
  {Calculate Hash}
  Hash:=0;                                                          //To Do //Testing4
  Wildcard:=(Length(AName) = ntfsAnyNameLength) and (AName = ntfsAnyName); {Modified 14/2/2011}
  if not Wildcard then Hash:=GenerateNameHash(AName,NAME_HASH_SIZE); //To Do //Testing4
  
  {Check AttrDefs}
  AttrDef:=TNTFSAttrDef(First);
  while AttrDef <> nil do
   begin
    {Check Type}
    if (AType = ntfsAttrTypeAny) or (AttrDef.AttributeType = AType) then
    {if AttrDef.AttributeType = AType then}
     begin
      if Wildcard or (AttrDef.AttributeHash = Hash) then
       begin
        if Wildcard or (Uppercase(AttrDef.AttributeName) = Uppercase(AName)) then
         begin
          Result:=AttrDef;
          Exit;
         end;
       end;
     end;
     
    AttrDef:=TNTFSAttrDef(AttrDef.Next);
   end;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TNTFSAttrDefs.GetAttrDefByIndex(AIndex:Integer;AVersion:Word):TNTFSAttrDef;
var
 AttrDef:TNTFSAttrDef;
begin
 {}
 Result:=nil;
 
 if not ReaderLock then Exit;
 try
  if AIndex < 0 then Exit;
  
  {Check Version}
  case AVersion of
   ntfsNTFS12:begin
     if AIndex > ntfs12MaxAttrDefine then Exit;
     
     {Check Attr Defs}
     AttrDef:=TNTFSAttrDef(First);
     while AttrDef <> nil do
      begin
       if AttrDef.AttributeType = ntfs12AttrDefines[AIndex].AttributeType then
        begin
         Result:=AttrDef;
         Exit;
        end;
        
       AttrDef:=TNTFSAttrDef(AttrDef.Next);
      end;
    end;
   ntfsNTFS30:begin
     if AIndex > ntfs30MaxAttrDefine then Exit;
     
     {Check Attr Defs}
     AttrDef:=TNTFSAttrDef(First);
     while AttrDef <> nil do
      begin
       if AttrDef.AttributeType = ntfs30AttrDefines[AIndex].AttributeType then
        begin
         Result:=AttrDef;
         Exit;
        end;
        
       AttrDef:=TNTFSAttrDef(AttrDef.Next);
      end;
    end;
   ntfsNTFS31:begin
     if AIndex > ntfs31MaxAttrDefine then Exit;
     
     {Check Attr Defs}
     AttrDef:=TNTFSAttrDef(First);
     while AttrDef <> nil do
      begin
       if AttrDef.AttributeType = ntfs31AttrDefines[AIndex].AttributeType then
        begin
         Result:=AttrDef;
         Exit;
        end;
        
       AttrDef:=TNTFSAttrDef(AttrDef.Next);
      end;
    end;
  end;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TNTFSAttrDefs.GetAttrDefByIndexEx(AIndex:Integer;AVersion:Word;AWrite:Boolean):TNTFSAttrDef;
var
 AttrDef:TNTFSAttrDef;
begin
 {}
 Result:=nil;
 
 if AWrite then
  begin
   if not WriterLock then Exit;
  end
 else
  begin 
   if not ReaderLock then Exit;
  end; 
 try
  if AIndex < 0 then Exit;
  
  {Check Version}
  case AVersion of
   ntfsNTFS12:begin
     if AIndex > ntfs12MaxAttrDefine then Exit;
     
     {Check Attr Defs}
     AttrDef:=TNTFSAttrDef(First);
     while AttrDef <> nil do
      begin
       if AttrDef.AttributeType = ntfs12AttrDefines[AIndex].AttributeType then
        begin
         Result:=AttrDef;
         Exit;
        end;
        
       AttrDef:=TNTFSAttrDef(AttrDef.Next);
      end;
    end;
   ntfsNTFS30:begin
     if AIndex > ntfs30MaxAttrDefine then Exit;
     
     {Check Attr Defs}
     AttrDef:=TNTFSAttrDef(First);
     while AttrDef <> nil do
      begin
       if AttrDef.AttributeType = ntfs30AttrDefines[AIndex].AttributeType then
        begin
         Result:=AttrDef;
         Exit;
        end;
        
       AttrDef:=TNTFSAttrDef(AttrDef.Next);
      end;
    end;
   ntfsNTFS31:begin
     if AIndex > ntfs31MaxAttrDefine then Exit;
     
     {Check Attr Defs}
     AttrDef:=TNTFSAttrDef(First);
     while AttrDef <> nil do
      begin
       if AttrDef.AttributeType = ntfs31AttrDefines[AIndex].AttributeType then
        begin
         Result:=AttrDef;
         Exit;
        end;
        
       AttrDef:=TNTFSAttrDef(AttrDef.Next);
      end;
    end;
  end;
 finally
  if AWrite then
   begin
    WriterUnlock;
   end
  else
   begin  
    ReaderUnlock;
   end; 
 end;
end;

{==============================================================================}

function TNTFSAttrDefs.GetAttrDefByAttribute(AAttribute:TNTFSDiskAttribute):TNTFSAttrDef;
var
 AttrDef:TNTFSAttrDef;
begin
 {}
 Result:=nil;
 
 if not ReaderLock then Exit;
 try
  if AAttribute = nil then Exit;
  
  {Check Attr Defs}
  AttrDef:=TNTFSAttrDef(First);
  while AttrDef <> nil do
   begin
    if AttrDef.AttributeType = AAttribute.AttributeType then
     begin
      Result:=AttrDef;
      Exit;
     end;
     
    AttrDef:=TNTFSAttrDef(AttrDef.Next);
   end;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TNTFSAttrDefs.RemoveAttrDef(AAttrDef:TNTFSAttrDef):Boolean;
{Remove the attribute def from the list and free}
begin
 {}
 Result:=False;
 
 if not WriterLock then Exit;
 try
  if AAttrDef = nil then Exit;
  
  {Remove AttrDef}
  Remove(AAttrDef);
  
  {Free AttrDef}
  AAttrDef.Free;
  
  Result:=True;
 finally
  WriterUnlock;
 end;
end;

{==============================================================================}

function TNTFSAttrDefs.CheckSize(AAttribute:TNTFSDiskAttribute;const ASize:Int64):Boolean;
{Check if the Attribute can be made the passed size (Smaller or Larger)}
var
 AttrDef:TNTFSAttrDef;
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  if AAttribute = nil then Exit;
  
  {Get Attr Def}
  AttrDef:=GetAttrDefByAttribute(AAttribute);
  if AttrDef = nil then Exit;
  
  {Check Size}
  if AttrDef.MinimumSize > ASize then Exit;
  if AttrDef.MaximumSize < ASize then Exit;
  
  Result:=True;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TNTFSAttrDefs.CheckIndexed(AAttribute:TNTFSDiskAttribute):Boolean;
{Check if the Attribute must be indexed}
var
 AttrDef:TNTFSAttrDef;
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  if AAttribute = nil then Exit;
  
  {Get Attr Def}
  AttrDef:=GetAttrDefByAttribute(AAttribute);
  if AttrDef = nil then Exit;
  
  {Check Flags}
  if (AttrDef.AttrDefFlags and ntfsAttrDefFlagIndexed) = ntfsAttrDefFlagNone then Exit;
  
  Result:=True;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TNTFSAttrDefs.CheckResident(AAttribute:TNTFSDiskAttribute):Boolean;
{Check if the Attribute must be resident}
var
 AttrDef:TNTFSAttrDef;
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  if AAttribute = nil then Exit;
  
  {Get Attr Def}
  AttrDef:=GetAttrDefByAttribute(AAttribute);
  if AttrDef = nil then Exit;
  
  {Check Flags}
  if (AttrDef.AttrDefFlags and ntfsAttrDefFlagResident) = ntfsAttrDefFlagNone then Exit;
  
  Result:=True;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TNTFSAttrDefs.CheckUncompressed(AAttribute:TNTFSDiskAttribute):Boolean;
{Check if the Attribute must be uncompresed}
var
 AttrDef:TNTFSAttrDef;
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  if AAttribute = nil then Exit;
  
  {Get Attr Def}
  AttrDef:=GetAttrDefByAttribute(AAttribute);
  if AttrDef = nil then Exit;
  
  {Check Flags}
  if (AttrDef.AttrDefFlags and ntfsAttrDefFlagUncompressed) = ntfsAttrDefFlagNone then Exit;
  
  Result:=True;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TNTFSAttrDefs.TotalSize:Int64;
var
 AttrDef:TNTFSAttrDef;
begin
 {}
 Result:=0;
 
 if not ReaderLock then Exit;
 try
  AttrDef:=TNTFSAttrDef(First);
  while AttrDef <> nil do
   begin
    Inc(Result,ntfsAttrDefSize);
    
    AttrDef:=TNTFSAttrDef(AttrDef.Next);
   end;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TNTFSAttrDefs.AttrDefCount:LongWord;
begin
 {}
 Result:=0;
 
 if not ReaderLock then Exit;
 try
  Result:=Count;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TNTFSAttrDefs.Init(AVersion:Word):Boolean;
var
 Count:Integer;
 Upper:Integer;
 AttrDef:TNTFSAttrDef;
begin
 {}
 Result:=False;

 if not WriterLock then Exit;
 try
  {Check Version}
  Upper:=ntfs31MaxAttrDefine;
  if AVersion = ntfsNTFS30 then Upper:=ntfs30MaxAttrDefine;
  if AVersion = ntfsNTFS12 then Upper:=ntfs12MaxAttrDefine;
  
  {Create AttrDefs}
  for Count:=0 to Upper do
   begin
    {Check AttrDef}
    AttrDef:=GetAttrDefByIndexEx(Count,AVersion,True);
    if AttrDef <> nil then Exit;
    
    AttrDef:=CreateAttrDef(ntfsAttrTypeNone,AVersion,False);
    if not AttrDef.Init(Count,AVersion) then
     begin
      DestroyAttrDef(AttrDef);
      Exit;
     {$IFDEF NTFS_DEBUG}
      end
     else
      begin
       if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSAttrDefs.Init - Type = ' + IntToHex(AttrDef.AttributeType,4) + ' Name = ' + AttrDef.AttributeName);
     {$ENDIF}
     end;
   end;
   
  Result:=True;
 finally
  WriterUnlock;
 end;
end;

{==============================================================================}

function TNTFSAttrDefs.ReadAttrDefs(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Read the attribute def items from the supplied buffer at the supplied offset}
var
 AttrDef:TNTFSAttrDef;
 AttrDefData:PNTFSAttrDefData;
begin
 {}
 Result:=False;
 
 if not WriterLock then Exit;
 try
  if ABuffer = nil then Exit;
  
  {Check Size}
  while ASize >= ntfsAttrDefSize do
   begin
    {Get Data}
    AttrDefData:=PNTFSAttrDefData(LongWord(ABuffer) + AOffset);
    
    {Check Last}
    if AttrDefData.AttributeType = ntfsAttrTypeEnd then Break;
    if AttrDefData.AttributeType = ntfsAttrTypeNone then Break;
    
    {Create AttrDef}
    AttrDef:=CreateAttrDef(AttrDefData.AttributeType,AVersion,False);
    
    {Read AttrDef}
    if not AttrDef.ReadAttrDef(ABuffer,AOffset,ASize,AVersion) then
     begin
      DestroyAttrDef(AttrDef);
      Exit;
     {$IFDEF NTFS_DEBUG}
      end
     else
      begin
       if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSAttrDefs.ReadAttrDefs - Type = ' + IntToHex(AttrDef.AttributeType,8) + ' Name = ' + AttrDef.AttributeName);
       if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSAttrDefs.ReadAttrDefs - Flags = ' + IntToHex(AttrDef.AttrDefFlags,8) + ' Min = ' + IntToStr(AttrDef.MinimumSize) + ' Max = ' + IntToStr(AttrDef.MaximumSize));
     {$ENDIF}
     end;
   end;
   
  Result:=True;
 finally
  WriterUnlock;
 end;
end;

{==============================================================================}

function TNTFSAttrDefs.WriteAttrDefs(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Write the attribute def items to the supplied buffer at the supplied offset}
var
 AttrDef:TNTFSAttrDef;
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  if ABuffer = nil then Exit;
  
  {Get AttrDef}
  AttrDef:=TNTFSAttrDef(First);
  
  {Write AttrDefs}
  while AttrDef <> nil do
   begin
    {Check Size}
    if ASize < ntfsAttrDefSize then Exit;
    
    {Write AttrDef}
    if not AttrDef.WriteAttrDef(ABuffer,AOffset,ASize,AVersion) then Exit;
    
    {Get AttrDef}
    AttrDef:=TNTFSAttrDef(AttrDef.Next);
   end;
   
  Result:=True;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}
{==============================================================================}
{TNTFSAttrDef}
constructor TNTFSAttrDef.Create(ALocalLock:TMutexHandle);
begin
 {}
 inherited Create;
 FLocalLock:=ALocalLock;
 
 FAttributeName:=ntfsBlankName;
 FAttributeType:=ntfsAttrTypeNone;
 FDisplayRule:=ntfsDisplayTypeNone;
 FCollateRule:=ntfsCollateTypeBinary;
 FAttrDefFlags:=ntfsAttrDefFlagNone;
 FMinimumSize:=ntfsAttrDefNoMinimum;
 FMaximumSize:=ntfsAttrDefNoMaximum;

 FAttributeHash:=0;
end;

{==============================================================================}

destructor TNTFSAttrDef.Destroy;
begin
 {}
 FLocalLock:=INVALID_HANDLE_VALUE;
 inherited Destroy;
end;

{==============================================================================}

function TNTFSAttrDef.AcquireLock:Boolean;
begin
 {}
 Result:=(MutexLock(FLocalLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSAttrDef.ReleaseLock:Boolean;
begin
 {}
 Result:=(MutexUnlock(FLocalLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSAttrDef.GetAttributeName:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;

 Result:=FAttributeName;
 UniqueString(Result);

 ReleaseLock;
end;

{==============================================================================}

procedure TNTFSAttrDef.SetAttributeName(const AAttributeName:String);
begin
 {}
 if not AcquireLock then Exit;

 FAttributeName:=AAttributeName;
 UniqueString(FAttributeName);
 FAttributeHash:=GenerateNameHash(FAttributeName,NAME_HASH_SIZE);

 ReleaseLock;
end;

{==============================================================================}

function TNTFSAttrDef.GetIsIndexed:Boolean;
begin
 {}
 Result:=((FAttrDefFlags and ntfsAttrDefFlagIndexed) = ntfsAttrDefFlagIndexed);
end;

{==============================================================================}

function TNTFSAttrDef.GetIsResident:Boolean;
begin
 {}
 Result:=((FAttrDefFlags and ntfsAttrDefFlagResident) = ntfsAttrDefFlagResident);
end;

{==============================================================================}

function TNTFSAttrDef.GetIsUncompressed:Boolean;
begin
 {}
 Result:=((FAttrDefFlags and ntfsAttrDefFlagUncompressed) = ntfsAttrDefFlagUncompressed);
end;

{==============================================================================}

function TNTFSAttrDef.Compare(AAttrDef:TNTFSAttrDef):Integer;
{Compare attrdef by type and name for sort order}
{The passed attrdef is the first value in a standard compare}
{Name is compared using a case insensitive compare method}
begin
 {}
 Result:=ntfsCompareGreater;
 
 if AAttrDef = nil then Exit;
 
 {Check Attribute Type}
 if AAttrDef.AttributeType < FAttributeType then
  begin
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSAttrDef.Compare - Type ' + IntToHex(AAttrDef.AttributeType,8) + ' < ' + IntToHex(FAttributeType,8));
   {$ENDIF}
   
   Result:=ntfsCompareLess;
  end
 else if AAttrDef.AttributeType > FAttributeType then
  begin
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSAttrDef.Compare - Type ' + IntToHex(AAttrDef.AttributeType,8) + ' > ' + IntToHex(FAttributeType,8));
   {$ENDIF}
   
   Result:=ntfsCompareGreater;
  end
 else
  begin
   {Check Attribute Name}
   Result:=CompareText(AAttrDef.AttributeName,AttributeName);  //To Do //Windows Locale ?
   if Result < ntfsCompareEqual then
    begin
     {$IFDEF NTFS_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSAttrDef.Compare - Name ' + AAttrDef.AttributeName + ' < ' + AttributeName);
     {$ENDIF}
     
     Result:=ntfsCompareLess;
    end
   else if Result > ntfsCompareEqual then
    begin
     {$IFDEF NTFS_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSAttrDef.Compare - Name ' + AAttrDef.AttributeName + ' > ' + AttributeName);
     {$ENDIF}
     
     Result:=ntfsCompareGreater;
    end;
  end;
end;

{==============================================================================}

function TNTFSAttrDef.Init(AIndex:Integer;AVersion:Word):Boolean;
begin
 {}
 Result:=False;

 if AIndex < 0 then Exit;
  
 {Check Version}
 case AVersion of
  ntfsNTFS12:begin
    if AIndex > ntfs12MaxAttrDefine then Exit;
    
    {Load Defaults}
    AttributeName:=ntfs12AttrDefines[AIndex].AttributeName;
    FAttributeType:=ntfs12AttrDefines[AIndex].AttributeType;
    FDisplayRule:=ntfsDisplayTypeNone;
    FCollateRule:=ntfsCollateTypeBinary;
    FAttrDefFlags:=ntfs12AttrDefines[AIndex].AttrDefFlags;
    FMinimumSize:=ntfs12AttrDefines[AIndex].MinimumSize;
    FMaximumSize:=ntfs12AttrDefines[AIndex].MaximumSize;
    
    Result:=True;
   end;
  ntfsNTFS30:begin
    if AIndex > ntfs30MaxAttrDefine then Exit;
    
    {Load Defaults}
    AttributeName:=ntfs30AttrDefines[AIndex].AttributeName;
    FAttributeType:=ntfs30AttrDefines[AIndex].AttributeType;
    FDisplayRule:=ntfsDisplayTypeNone;
    FCollateRule:=ntfsCollateTypeBinary;
    FAttrDefFlags:=ntfs30AttrDefines[AIndex].AttrDefFlags;
    FMinimumSize:=ntfs30AttrDefines[AIndex].MinimumSize;
    FMaximumSize:=ntfs30AttrDefines[AIndex].MaximumSize;
    
    Result:=True;
   end;
  ntfsNTFS31:begin
    if AIndex > ntfs31MaxAttrDefine then Exit;
    
    {Load Defaults}
    AttributeName:=ntfs31AttrDefines[AIndex].AttributeName;
    FAttributeType:=ntfs31AttrDefines[AIndex].AttributeType;
    FDisplayRule:=ntfsDisplayTypeNone;
    FCollateRule:=ntfsCollateTypeBinary;
    FAttrDefFlags:=ntfs31AttrDefines[AIndex].AttrDefFlags;
    FMinimumSize:=ntfs31AttrDefines[AIndex].MinimumSize;
    FMaximumSize:=ntfs31AttrDefines[AIndex].MaximumSize;
    
    Result:=True;
   end;
 end;
end;

{==============================================================================}

function TNTFSAttrDef.ReadAttrDef(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Read the attrdef data from the supplied buffer at the supplied offset}
var
 AttrDefData:PNTFSAttrDefData;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {Get Data}
 AttrDefData:=PNTFSAttrDefData(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if ASize >= ntfsAttrDefSize then
  begin
   {Read Data}
   FAttributeType:=AttrDefData.AttributeType;
   FDisplayRule:=AttrDefData.DisplayRule;
   FCollateRule:=AttrDefData.CollateRule;
   FAttrDefFlags:=AttrDefData.AttrDefFlags;
   FMinimumSize:=AttrDefData.MinimumSize;
   FMaximumSize:=AttrDefData.MaximumSize;
   
   {Read Name}
   AttributeName:=NTFSWideBufferToString(@AttrDefData.AttributeName[0],0,64);
   
   {Update Offset}
   Dec(ASize,ntfsAttrDefSize);
   Inc(AOffset,ntfsAttrDefSize);
   
   Result:=True;
  end;
end;

{==============================================================================}

function TNTFSAttrDef.WriteAttrDef(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Write the attrdef data to the supplied buffer at the supplied offset}
var
 AttrDefData:PNTFSAttrDefData;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {Get Data}
 AttrDefData:=PNTFSAttrDefData(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if ASize >= ntfsAttrDefSize then
  begin
   {Write Data}
   AttrDefData.AttributeType:=FAttributeType;
   AttrDefData.DisplayRule:=FDisplayRule;
   AttrDefData.CollateRule:=FCollateRule;
   AttrDefData.AttrDefFlags:=FAttrDefFlags;
   AttrDefData.MinimumSize:=FMinimumSize;
   AttrDefData.MaximumSize:=FMaximumSize;
   
   {Write Name}
   if not NTFSStringToWideBuffer(AttributeName,@AttrDefData.AttributeName[0],0,64) then Exit;
   
   {Update Offset}
   Dec(ASize,ntfsAttrDefSize);
   Inc(AOffset,ntfsAttrDefSize);
   
   Result:=True;
  end;
end;

{==============================================================================}
{==============================================================================}
{TNTFSReparse}
constructor TNTFSReparse.Create(ALocalLock:TMutexHandle;AAttribute:TNTFSDiskAttribute);
begin
 {}
 inherited Create(ALocalLock);
 FData:=nil;

 FDataSize:=0;

 FAttribute:=AAttribute;
end;

{==============================================================================}

destructor TNTFSReparse.Destroy;
begin
 {}
 if FData <> nil then FreeMem(FData);
 FAttribute:=nil;
 inherited Destroy;
end;

{==============================================================================}

procedure TNTFSReparse.SetData(AData:Pointer);
begin
 {Virtual Base}
 if not AcquireLock then Exit;
 try
  if AData = nil then Exit;
  if FData = nil then Exit;
  if FDataSize = 0 then Exit;
 
  System.Move(AData^,FData^,FDataSize);
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TNTFSReparse.GetDataSize:Word;
begin
 {Virtual Base}
 Result:=FDataSize;
end;

{==============================================================================}

procedure TNTFSReparse.SetDataSize(ASize:Word);
begin
 {Virtual Base}
 if not AcquireLock then Exit;

 if ASize = 0 then
  begin
   if FData <> nil then FreeMem(FData);
   FDataSize:=0;
   FData:=nil;
  end
 else
  begin
   if FData <> nil then FreeMem(FData);
   FDataSize:=ASize;
   FData:=AllocMem(FDataSize);
  end;

 ReleaseLock;
end;

{==============================================================================}

function TNTFSReparse.CalculatedSize(AVersion:Word):Word;
begin
 {Virtual Base}
 Result:=FDataSize;
end;

{==============================================================================}

function TNTFSReparse.ReadReparse(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Read the reparse point data from the supplied buffer at the supplied offset}
var
 ReparseData:PNTFSReparseData;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 if FAttribute = nil then Exit;
 
 {Get Data}
 ReparseData:=PNTFSReparseData(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if ASize >= TNTFSReparsePointAttribute(FAttribute).ReparseSize then
  begin
   {Set Size}
   SetDataSize(TNTFSReparsePointAttribute(FAttribute).ReparseSize);
   
   if not AcquireLock then Exit;

   {Read Data}
   System.Move(ReparseData.Data[0],FData^,FDataSize);
   
   ReleaseLock;
   
   {Update Offset}
   Dec(ASize,TNTFSReparsePointAttribute(FAttribute).ReparseSize);
   Inc(AOffset,TNTFSReparsePointAttribute(FAttribute).ReparseSize);
   
   Result:=True;
  end;
end;

{==============================================================================}

function TNTFSReparse.WriteReparse(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Write the reparse point data to the supplied buffer at the supplied offset}
var
 ReparseData:PNTFSReparseData;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 if FAttribute = nil then Exit;
 
 {Get Data}
 ReparseData:=PNTFSReparseData(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if ASize >= TNTFSReparsePointAttribute(FAttribute).ReparseSize then
  begin
   if not AcquireLock then Exit;
   
   {Write Data}
   System.Move(FData^,ReparseData.Data[0],FDataSize);
   
   ReleaseLock;

   {Update Offset}
   Dec(ASize,TNTFSReparsePointAttribute(FAttribute).ReparseSize);
   Inc(AOffset,TNTFSReparsePointAttribute(FAttribute).ReparseSize);
   
   Result:=True;
  end;
end;

{==============================================================================}
{==============================================================================}
{TNTFSReparseSymLink}
constructor TNTFSReparseSymLink.Create(ALocalLock:TMutexHandle;AAttribute:TNTFSDiskAttribute);
begin
 {}
 inherited Create(ALocalLock,AAttribute);
 FPrintName:=ntfsBlankName;
 FSubstituteName:=ntfsBlankName;

 FPrintHash:=0;
 FSubstituteHash:=0;
end;

{==============================================================================}

destructor TNTFSReparseSymLink.Destroy;
begin
 {}
 inherited Destroy;
end;

{==============================================================================}

procedure TNTFSReparseSymLink.SetData(AData:Pointer);
begin
 {Nothing}
end;

{==============================================================================}

procedure TNTFSReparseSymLink.SetDataSize(ASize:Word);
begin
 {}
 FDataSize:=ASize;
end;

{==============================================================================}

function TNTFSReparseSymLink.GetTarget:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;
 
 Result:=FSubstituteName;
 UniqueString(Result);
 
 ReleaseLock;

 if Copy(Result,1,Length(ntfsReparsePointPrefix)) = ntfsReparsePointPrefix then Delete(Result,1,Length(ntfsReparsePointPrefix));
end;

{==============================================================================}

function TNTFSReparseSymLink.GetPrintName:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;
 
 Result:=FPrintName;
 UniqueString(Result);
 
 ReleaseLock;
end;

{==============================================================================}

procedure TNTFSReparseSymLink.SetPrintName(const APrintName:String);
begin
 {}
 if not AcquireLock then Exit;
 
 FPrintName:=APrintName;
 UniqueString(FPrintName);
 FPrintHash:=GenerateNameHash(FPrintName,NAME_HASH_SIZE);
 
 ReleaseLock;
end;

{==============================================================================}

function TNTFSReparseSymLink.GetSubstituteName:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;
 
 Result:=FSubstituteName;
 UniqueString(Result);
 
 ReleaseLock;
end;

{==============================================================================}

procedure TNTFSReparseSymLink.SetSubstituteName(const ASubstituteName:String);
begin
 {}
 if not AcquireLock then Exit;
 
 FSubstituteName:=ASubstituteName;
 UniqueString(FSubstituteName);
 FSubstituteHash:=GenerateNameHash(FSubstituteName,NAME_HASH_SIZE);
 
 ReleaseLock;
end;

{==============================================================================}

function TNTFSReparseSymLink.PrintNameSize:Word;
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;

 Result:=Length(FPrintName) shl 1; {Multiply by SizeOf(WideChar) / SizeOf(Word)}

 ReleaseLock;
end;

{==============================================================================}

function TNTFSReparseSymLink.PrintNameOffset:Word;
begin
 {}
 Result:=0; {Print Name comes first}
end;

{==============================================================================}

function TNTFSReparseSymLink.PrintNameLength:Word;
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;

 Result:=Length(FPrintName);

 ReleaseLock;
end;

{==============================================================================}

function TNTFSReparseSymLink.SubstituteNameSize:Word;
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;

 Result:=Length(FSubstituteName) shl 1; {Multiply by SizeOf(WideChar) / SizeOf(Word)}

 ReleaseLock;
end;

{==============================================================================}

function TNTFSReparseSymLink.SubstituteNameOffset:Word;
begin
 {}
 Result:=PrintNameSize; {Substitute Name comes after Print Name (no null terminator)}
end;

{==============================================================================}

function TNTFSReparseSymLink.SubstituteNameLength:Word;
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;

 Result:=Length(FSubstituteName);

 ReleaseLock;
end;

{==============================================================================}

function TNTFSReparseSymLink.CalculatedSize(AVersion:Word):Word;
begin
 {}
 Result:=ntfsReparseSize + PrintNameSize + SubstituteNameSize; {Not Rounded}
end;

{==============================================================================}

function TNTFSReparseSymLink.ReadReparse(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Read the reparse point data from the supplied buffer at the supplied offset}
var
 ReparseData:PNTFSReparseSymLinkData;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {Get Data}
 ReparseData:=PNTFSReparseSymLinkData(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if ASize >= ntfsReparseSize then
  begin
   {Read Names}
   PrintName:=NTFSWideBufferToString(ReparseData,ntfsReparseSize + ReparseData.PrintNameOffset,ReparseData.PrintNameLength shr 1); {Divide by SizeOf(WideChar) / SizeOf(Word)}
   SubstituteName:=NTFSWideBufferToString(ReparseData,ntfsReparseSize + ReparseData.SubstituteNameOffset,ReparseData.SubstituteNameLength shr 1); {Divide by SizeOf(WideChar) / SizeOf(Word)}
   
   {Update Offset}
   Dec(ASize,ntfsReparseSize + PrintNameSize + SubstituteNameSize);
   Inc(AOffset,ntfsReparseSize + PrintNameSize + SubstituteNameSize);
   
   Result:=True;
   
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSReparseSymLink.ReadReparse - PrintName = ' + PrintName + ' SubstituteName = ' + SubstituteName);
   {$ENDIF}
  end;
end;

{==============================================================================}

function TNTFSReparseSymLink.WriteReparse(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Write the reparse point data to the supplied buffer at the supplied offset}
var
 ReparseData:PNTFSReparseSymLinkData;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {Get Data}
 ReparseData:=PNTFSReparseSymLinkData(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if ASize >= ntfsReparseSize then
  begin
   {Write Names}
   ReparseData.PrintNameOffset:=PrintNameOffset;
   ReparseData.PrintNameLength:=PrintNameSize; {PrintNameLength} {Bytes not Chars}
   if not NTFSStringToWideBuffer(PrintName,ReparseData,ntfsReparseSize + ReparseData.PrintNameOffset,ReparseData.PrintNameLength shr 1) then Exit; {Divide by SizeOf(WideChar) / SizeOf(Word)}
   ReparseData.SubstituteNameOffset:=SubstituteNameOffset;
   ReparseData.SubstituteNameLength:=SubstituteNameSize; {SubstituteNameLength} {Bytes not Chars}
   if not NTFSStringToWideBuffer(SubstituteName,ReparseData,ntfsReparseSize + ReparseData.SubstituteNameOffset,ReparseData.SubstituteNameLength shr 1) then Exit; {Divide by SizeOf(WideChar) / SizeOf(Word)}
   
   {Update Offset}
   Dec(ASize,ntfsReparseSize + PrintNameSize + SubstituteNameSize);
   Inc(AOffset,ntfsReparseSize + PrintNameSize + SubstituteNameSize);
   
   Result:=True;
   
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSReparseSymLink.WriteReparse - PrintName = ' + PrintName + ' SubstituteName = ' + SubstituteName);
   {$ENDIF}
  end;
end;

{==============================================================================}
{==============================================================================}
{TNTFSReparseMountPoint}
constructor TNTFSReparseMountPoint.Create(ALocalLock:TMutexHandle;AAttribute:TNTFSDiskAttribute);
begin
 {}
 inherited Create(ALocalLock,AAttribute);
end;

{==============================================================================}

destructor TNTFSReparseMountPoint.Destroy;
begin
 {}
 inherited Destroy;
end;

{==============================================================================}

function TNTFSReparseMountPoint.PrintNameSize:Word;
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;

 Result:=Length(FPrintName) shl 1; {Multiply by SizeOf(WideChar) / SizeOf(Word)}
 
 ReleaseLock;
end;

{==============================================================================}

function TNTFSReparseMountPoint.PrintNameOffset:Word;
begin
 {}
 Result:=SubstituteNameSize + SizeOf(Word); {Print Name comes after Substitute Name and null terminator}
end;

{==============================================================================}

function TNTFSReparseMountPoint.PrintNameLength:Word;
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;

 Result:=Length(FPrintName);
 
 ReleaseLock;
end;

{==============================================================================}

function TNTFSReparseMountPoint.SubstituteNameSize:Word;
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;

 Result:=Length(FSubstituteName) shl 1; {Multiply by SizeOf(WideChar) / SizeOf(Word)}
 
 ReleaseLock;
end;

{==============================================================================}

function TNTFSReparseMountPoint.SubstituteNameOffset:Word;
begin
 {}
 Result:=0; {Subsitute Name comes first}
end;

{==============================================================================}

function TNTFSReparseMountPoint.SubstituteNameLength:Word;
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;

 Result:=Length(FSubstituteName);
 
 ReleaseLock;
end;

{==============================================================================}

function TNTFSReparseMountPoint.CalculatedSize(AVersion:Word):Word;
begin
 {}
 Result:=ntfsReparseSize + PrintNameSize + SubstituteNameSize; {ntfsReparseSize includes the 2 null chars} {Not Rounded}
end;

{==============================================================================}

function TNTFSReparseMountPoint.ReadReparse(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Read the reparse point data from the supplied buffer at the supplied offset}
var
 ReparseData:PNTFSReparseMountPointData;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {Get Data}
 ReparseData:=PNTFSReparseMountPointData(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if ASize >= ntfsReparseSize then
  begin
   {Read Names} {ntfsReparseSize includes the 2 null chars}
   PrintName:=NTFSWideBufferToString(ReparseData,(ntfsReparseSize - 4) + ReparseData.PrintNameOffset,ReparseData.PrintNameLength shr 1); {Divide by SizeOf(WideChar) / SizeOf(Word)}
   SubstituteName:=NTFSWideBufferToString(ReparseData,(ntfsReparseSize - 4) + ReparseData.SubstituteNameOffset,ReparseData.SubstituteNameLength shr 1); {Divide by SizeOf(WideChar) / SizeOf(Word)}

   {Update Offset}
   Dec(ASize,ntfsReparseSize + PrintNameSize + SubstituteNameSize);  {ntfsReparseSize includes the 2 null chars}
   Inc(AOffset,ntfsReparseSize + PrintNameSize + SubstituteNameSize);  {ntfsReparseSize includes the 2 null chars}
   
   Result:=True;
   
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSReparseMountPoint.ReadReparse - PrintName = ' + PrintName + ' SubstituteName = ' + SubstituteName);
   {$ENDIF}
  end;
end;

{==============================================================================}

function TNTFSReparseMountPoint.WriteReparse(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Write the reparse point data to the supplied buffer at the supplied offset}
var
 ReparseData:PNTFSReparseMountPointData;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {Get Data}
 ReparseData:=PNTFSReparseMountPointData(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if ASize >= ntfsReparseSize then
  begin
   {Write Names} {ntfsReparseSize includes the 2 null chars}
   ReparseData.PrintNameOffset:=PrintNameOffset;
   ReparseData.PrintNameLength:=PrintNameSize; {PrintNameLength} {Bytes not Chars}
   if not NTFSStringToWideBuffer(PrintName,ReparseData,(ntfsReparseSize - 4) + ReparseData.PrintNameOffset,ReparseData.PrintNameLength shr 1) then Exit; {Divide by SizeOf(WideChar) / SizeOf(Word)}
   ReparseData.SubstituteNameOffset:=SubstituteNameOffset;
   ReparseData.SubstituteNameLength:=SubstituteNameSize; {SubstituteNameLength} {Bytes not Chars}
   if not NTFSStringToWideBuffer(SubstituteName,ReparseData,(ntfsReparseSize - 4) + ReparseData.SubstituteNameOffset,ReparseData.SubstituteNameLength shr 1) then Exit; {Divide by SizeOf(WideChar) / SizeOf(Word)}
   
   {Update Offset}
   Dec(ASize,ntfsReparseSize + PrintNameSize + SubstituteNameSize);  {ntfsReparseSize includes the 2 null chars}
   Inc(AOffset,ntfsReparseSize + PrintNameSize + SubstituteNameSize);  {ntfsReparseSize includes the 2 null chars}
   
   Result:=True;
   
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSReparseMountPoint.WriteReparse - PrintName = ' + PrintName + ' SubstituteName = ' + SubstituteName);
   {$ENDIF}
  end;
end;

{==============================================================================}
{==============================================================================}
{TNTFSExtendeds}
constructor TNTFSExtendeds.Create(AExtendedLocal:TMutexHandle;ALock:TSynchronizerHandle);
begin
 {}
 inherited Create(ALock);
 FExtendedLocal:=AExtendedLocal;
end;

{==============================================================================}

destructor TNTFSExtendeds.Destroy; 
begin
 {}
 WriterLock;
 try
  FExtendedLocal:=INVALID_HANDLE_VALUE;
 finally
  WriterUnlock;
  inherited Destroy;
 end;
end;

{==============================================================================}

function TNTFSExtendeds.TotalSize:Int64;
var
 Extended:TNTFSExtended;
begin
 {}
 Result:=0;
 
 Extended:=TNTFSExtended(First);
 while Extended <> nil do
  begin
   Inc(Result,Extended.ExtendedSize);
   
   Extended:=TNTFSExtended(Extended.Next);
  end;
end;

{==============================================================================}

function TNTFSExtendeds.ExtendedCount:LongWord;
begin
 {}
 Result:=Count;
end;

{==============================================================================}
{==============================================================================}
{TNTFSExtended}
constructor TNTFSExtended.Create(ALocalLock:TMutexHandle);
begin
 {}
 inherited Create;
 FLocalLock:=ALocalLock;
 
 FExtendedOffset:=0;
 FExtendedFlags:=ntfsExtendedAttributeFlagNone;
 FExtendedName:=ntfsBlankName;
 FExtendedData:=nil;
 FExtendedDataSize:=0;

 FExtendedHash:=0;
end;

{==============================================================================}

destructor TNTFSExtended.Destroy;
begin
 {}
 if FExtendedData <> nil then FreeMem(FExtendedData);
 
 FLocalLock:=INVALID_HANDLE_VALUE;
 inherited Destroy;
end;

{==============================================================================}

function TNTFSExtended.AcquireLock:Boolean;
begin
 {}
 Result:=(MutexLock(FLocalLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSExtended.ReleaseLock:Boolean;
begin
 {}
 Result:=(MutexUnlock(FLocalLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSExtended.GetExtendedName:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;
 
 Result:=FExtendedName;
 UniqueString(Result);
 
 ReleaseLock;
end;

{==============================================================================}

procedure TNTFSExtended.SetExtendedName(const AExtendedName:String);
begin
 {}
 if not AcquireLock then Exit;
 
 FExtendedName:=AExtendedName;
 UniqueString(FExtendedName);
 FExtendedHash:=GenerateNameHash(FExtendedName,NAME_HASH_SIZE);
 
 ReleaseLock;
end;

{==============================================================================}

procedure TNTFSExtended.SetExtendedDataSize(ASize:Word);
begin
 {}
 //To Do //Lock 
 
 if ASize = 0 then
  begin
   if FExtendedData <> nil then FreeMem(FExtendedData);
   FExtendedDataSize:=0;
   FExtendedData:=nil;
  end
 else
  begin
   if FExtendedData <> nil then FreeMem(FExtendedData);
   FExtendedDataSize:=ASize;
   FExtendedData:=AllocMem(FExtendedDataSize);
  end;
  
 //To Do //Lock 
end;

{==============================================================================}

function TNTFSExtended.ExtendedSize:LongWord;
begin
 {}
 Result:=FExtendedOffset;
end;

{==============================================================================}

function TNTFSExtended.ExtendedNameSize:Word;
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;
 
 Result:=Length(FExtendedName); {SizeOf(Char)}
 
 ReleaseLock;
end;

{==============================================================================}

function TNTFSExtended.ExtendedNameLength:Byte;
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;
 
 Result:=Length(FExtendedName);
 
 ReleaseLock;
end;

{==============================================================================}

function TNTFSExtended.ReadExtended(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Read the extended attribute from the supplied buffer at the supplied offset}
var
 ExtendedData:PNTFSExtendedData;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {Get Data}
 ExtendedData:=PNTFSExtendedData(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if ASize >= ntfsExtendedSize then
  begin
   {Read Data}
   FExtendedOffset:=ExtendedData.ExtendedOffset;
   FExtendedFlags:=ExtendedData.ExtendedFlags;
   FExtendedDataSize:=ExtendedData.ExtendedDataSize;
   
   {Read Name}
   ExtendedName:=NTFSBufferToString(@ExtendedData.ExtendedName[0],0,ExtendedData.ExtendedNameLength);
   
   {Set Size}
   SetExtendedDataSize(FExtendedDataSize);
   
   {Read Data}
   if FExtendedDataSize > 0 then
    begin
     System.Move(Pointer(LongWord(ExtendedData) + AOffset + ExtendedData.ExtendedNameLength)^,FExtendedData^,FExtendedDataSize);
    end;
   
   {Update Offset}
   Dec(ASize,ExtendedData.ExtendedOffset);
   Inc(AOffset,ExtendedData.ExtendedOffset);
   
   Result:=True;
  end;
end;

{==============================================================================}

function TNTFSExtended.WriteExtended(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Write the extended attribute to the supplied buffer at the supplied offset}
var
 ExtendedData:PNTFSExtendedData;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {Get Data}
 ExtendedData:=PNTFSExtendedData(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if ASize >= ntfsExtendedSize then
  begin
   {Write Data}
   ExtendedData.ExtendedOffset:=FExtendedOffset;
   ExtendedData.ExtendedFlags:=FExtendedFlags;
   ExtendedData.ExtendedDataSize:=FExtendedDataSize;
   ExtendedData.ExtendedNameLength:=ExtendedNameLength;
   
   {Write Name}
   if not NTFSStringToBuffer(ExtendedName,@ExtendedData.ExtendedName[0],0,ExtendedData.ExtendedNameLength) then Exit;
   
   {Write Data}
   if FExtendedDataSize > 0 then
    begin
     System.Move(FExtendedData^,Pointer(LongWord(ExtendedData) + AOffset + ExtendedData.ExtendedNameLength)^,FExtendedDataSize);
    end;
   
   {Update Offset}
   Dec(ASize,ExtendedData.ExtendedOffset);
   Inc(AOffset,ExtendedData.ExtendedOffset);
   
   Result:=True;
  end;
end;

{==============================================================================}
{==============================================================================}
{TNTFSSecurityItems}
constructor TNTFSSecurityItems.Create;
begin
 {}
 inherited Create;
 FSecurityLocal:=MutexCreate;
end;

{==============================================================================}

destructor TNTFSSecurityItems.Destroy; 
begin
 {}
 WriterLock;
 try
  MutexDestroy(FSecurityLocal);
 finally
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  inherited Destroy;
 end;
end;

{==============================================================================}

function TNTFSSecurityItems.GetPrevious(ASecurityItem:TNTFSSecurityItem):TNTFSSecurityItem;
{Note: Caller must hold the lock}
var
 Current:TNTFSSecurityItem;
begin
 {}
 Result:=nil;
 
 if ASecurityItem = nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSSecurityItems.GetPrevious - Hash = ' + IntToHex(ASecurityItem.SecurityHash,8) + ' Id = ' + IntToHex(ASecurityItem.SecurityId,8) + ' Offset = ' + IntToHex(ASecurityItem.SecurityOffset,16));
 {$ENDIF}
 
 {Check Items}
 Current:=TNTFSSecurityItem(First);
 while Current <> nil do
  begin
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSSecurityItems.GetPrevious - Comparing with Hash = ' + IntToHex(Current.SecurityHash,8) + ' Id = ' + IntToHex(Current.SecurityId,8) + ' Offset = ' + IntToHex(Current.SecurityOffset,16));
   {$ENDIF}
   
   if ASecurityItem.Compare(Current) = ntfsCompareGreater then Exit;
   
   Result:=Current;
   Current:=TNTFSSecurityItem(Current.Next);
  end;
end;

{==============================================================================}

function TNTFSSecurityItems.CreateSecurityItem(ANew:Boolean):TNTFSSecurityItem;
{Create a security item, add to end of list}
var
 SecurityItem:TNTFSSecurityItem;
begin
 {}
 Result:=nil;
 
 if not WriterLock then Exit;
 try
  {Create SecurityItem}
  SecurityItem:=TNTFSSecurityItem.Create(FSecurityLocal);
  
  {Add SecurityItem  (Only if not new)}
  if not ANew then Add(SecurityItem);
  
  Result:=SecurityItem;
 finally
  WriterUnlock;
 end;
end;

{==============================================================================}

function TNTFSSecurityItems.DestroySecurityItem(ASecurityItem:TNTFSSecurityItem):Boolean;
{Remove the security item from the list and free}
begin
 {}
 Result:=False;
 
 if not WriterLock then Exit;
 try
  if ASecurityItem = nil then Exit;
  
  {Remove SecurityItem}
  Remove(ASecurityItem);
  
  {Free SecurityItem}
  ASecurityItem.Free;
  
  Result:=True;
 finally
  WriterUnlock;
 end;
end;

{==============================================================================}

function TNTFSSecurityItems.NewSecurityItem(ASecurityId:LongWord;const ASecurityOffset:Int64;ASecurity:TNTFSSecurity):TNTFSSecurityItem;
{Create a security item, insert in sorted list}
var
 Previous:TNTFSSecurityItem;
begin
 {}
 Result:=nil;
 
 if not WriterLock then Exit;
 try
  if ASecurity = nil then Exit;
  if ASecurityId = 0 then Exit;
  
  Result:=CreateSecurityItem(True);
  if Result = nil then Exit;
  
  {Set Values}
  Result.SecurityId:=ASecurityId;
  Result.SecurityOffset:=ASecurityOffset;
  if Result.NewSecurity(ASecurity) then
   begin
    Result.SecurityHash:=ASecurity.SecurityHash;
    Result.SecuritySize:=(ntfsSecurityItemSize - ntfsSecuritySize) + ASecurity.SecuritySize;
    
    {Get Previous}
    Previous:=GetPrevious(Result);
    
    {Insert SecurityItem}
    Insert(Previous,Result);
   end
  else
   begin
    Result.Free;
    Result:=nil;
   end;
 finally
  WriterUnlock;
 end;
end;

{==============================================================================}

function TNTFSSecurityItems.GetSecurityItem(ASecurityId:LongWord):TNTFSSecurityItem;
{Get a security item by Id}
var
 SecurityItem:TNTFSSecurityItem;
begin
 {}
 Result:=nil;
 
 if not ReaderLock then Exit;
 try
  {Check SecurityItems}
  SecurityItem:=TNTFSSecurityItem(First);
  while SecurityItem <> nil do
   begin
    if SecurityItem.SecurityId = ASecurityId then
     begin
      Result:=SecurityItem;
      Exit;
     end;
     
    SecurityItem:=TNTFSSecurityItem(SecurityItem.Next);
   end;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TNTFSSecurityItems.GetSecurityItemEx(ASecurityId:LongWord;AWrite:Boolean):TNTFSSecurityItem;
{Get a security item by Id}
var
 SecurityItem:TNTFSSecurityItem;
begin
 {}
 Result:=nil;
 
 if AWrite then
  begin
   if not WriterLock then Exit;
  end
 else
  begin 
   if not ReaderLock then Exit;
  end; 
 try
  {Check SecurityItems}
  SecurityItem:=TNTFSSecurityItem(First);
  while SecurityItem <> nil do
   begin
    if SecurityItem.SecurityId = ASecurityId then
     begin
      Result:=SecurityItem;
      Exit;
     end;
     
    SecurityItem:=TNTFSSecurityItem(SecurityItem.Next);
   end;
 finally
  if AWrite then
   begin
    WriterUnlock;
   end
  else
   begin  
    ReaderUnlock;
   end; 
 end;
end;

{==============================================================================}

function TNTFSSecurityItems.UpdateSecurityItem(ASecurityItem:TNTFSSecurityItem;ASecurityId:LongWord;ASecurity:TNTFSSecurity):Boolean;
{Convert a blank to a security item}
begin
 {}
 Result:=False;
 
 //To Do //Call SecurityItem.UpdateSecurity, Set Id to supplied, Offset to Existing, Size to supplied + item
end;

{==============================================================================}

function TNTFSSecurityItems.DeleteSecurityItem(ASecurityItem:TNTFSSecurityItem):Boolean;
{Convert the security item to a blank, do not remove from list and do not free}
begin
 {}
 Result:=False;
 
 if ASecurityItem = nil then Exit;
 
 //To Do //Call SecurityItem.DeleteSecurity, Set Id to 0, Offset to Existing, Size to Existing
end;

{==============================================================================}

function TNTFSSecurityItems.RemoveSecurityItem(ASecurityItem:TNTFSSecurityItem;AFree:Boolean):Boolean;
{Remove the security item from the list and free if requested}
begin
 {}
 Result:=False;
 
 if not WriterLock then Exit;
 try
  if ASecurityItem = nil then Exit;
  
  {Remove Security}
  ASecurityItem.RemoveSecurity(AFree);
  
  {Remove SecurityItem}
  Remove(ASecurityItem);
  
  {Free SecurityItem}
  ASecurityItem.Free;
  
  Result:=True;
 finally
  WriterUnlock;
 end;
end;

{==============================================================================}

function TNTFSSecurityItems.TotalSize:Int64;
var
 SecurityItem:TNTFSSecurityItem;
begin
 {}
 Result:=0;
 
 if not ReaderLock then Exit;
 try
  SecurityItem:=TNTFSSecurityItem(First);
  while SecurityItem <> nil do
   begin
    Inc(Result,SecurityItem.SecuritySize); //To Do //NTFSRoundLongWordTo16Bytes ? //What about Mirror entries ?
    
    SecurityItem:=TNTFSSecurityItem(SecurityItem.Next);                                 //Dont round Last Mirror Item ?
   end;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TNTFSSecurityItems.SecurityItemCount:LongWord;
begin
 {}
 Result:=0;

 if not ReaderLock then Exit;
 try
  Result:=Count;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TNTFSSecurityItems.Init(AVersion:Word):Boolean;
var
 Offset:Int64;
 Count:Integer;
 Lower:Integer;
 Upper:Integer;

 SecurityItem:TNTFSSecurityItem;
begin
 {}
 Result:=False;
 
 if not WriterLock then Exit;
 try
  {Check Version}
  Lower:=ntfsDefaultSecurityId100;
  Upper:=ntfsDefaultSecurityId101; {ntfsDefaultSecurityId103}
  Offset:=0;
  
  {Create Security Items}
  for Count:=Lower to Upper do
   begin
    {Check Security Item}
    SecurityItem:=GetSecurityItemEx(Count,True);
    if SecurityItem <> nil then Exit;
    
    SecurityItem:=CreateSecurityItem(False);
    if not SecurityItem.Init(Count,AVersion) then
     begin
      DestroySecurityItem(SecurityItem);
      Exit;
     end
    else
     begin
      {Update Offset}
      SecurityItem.SecurityOffset:=Offset;
      Inc(Offset,NTFSRoundLongWordTo16Bytes(SecurityItem.SecuritySize));
      
      {$IFDEF NTFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSSecurityItems.Init - Id = ' + IntToHex(SecurityItem.SecurityId,8) + ' Hash = ' + IntToHex(SecurityItem.SecurityHash,8) + ' Offset = ' + IntToHex(SecurityItem.SecurityOffset,16) + ' Size = ' + IntToHex(SecurityItem.SecuritySize,8));
      {$ENDIF}
     end;
   end;
   
  Result:=True;
 finally
  WriterUnlock;
 end;
end;

{==============================================================================}

function TNTFSSecurityItems.ReadSecurityItems(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Read the security items from the supplied buffer at the supplied offset}
var
 Size:LongWord;
 Offset:LongWord;
 Section:LongWord;
 SecurityItem:TNTFSSecurityItem;
 SecurityItemData:PNTFSSecurityItemData;
begin
 {}
 Result:=False;
 
 if not WriterLock then Exit;
 try
  if ABuffer = nil then Exit;
  
  {Check Size}
  while ASize >= ntfsSecurityItemSize do
   begin
    {Get Data}
    SecurityItemData:=PNTFSSecurityItemData(LongWord(ABuffer) + AOffset);
    
    {Check Last}
    if SecurityItemData.SecuritySize = 0 then Break;
    {if SecurityItemData.SecurityOffset <> AOffset then Break;} {Offset can be zero}
    if (SecurityItemData.SecurityOffset <> 0) and (SecurityItemData.SecurityOffset <> AOffset) then Break;
    {if (SecurityItemData.SecurityId <> 0) and (SecurityItemData.SecurityOffset <> AOffset) then Break;} {Is this a better test}
    
    {Create SecurityItem}
    SecurityItem:=CreateSecurityItem(False);
    if SecurityItem = nil then Exit;
    
    {Read SecurityItem}
    if not SecurityItem.ReadSecurityItem(ABuffer,AOffset,ASize,AVersion) then
     begin
      DestroySecurityItem(SecurityItem);
      Exit;
     {$IFDEF NTFS_DEBUG}
      end
     else
      begin
       if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSSecurityItems.ReadSecurityItems - SecurityId = ' + IntToHex(SecurityItem.SecurityId,8));
       if FILESYS_LOG_ENABLED then FileSysLogDebug('                                     SecuritySize = ' + IntToHex(SecurityItem.SecuritySize,8));
       if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   SecurityOffset = ' + IntToHex(SecurityItem.SecurityOffset,16));
       if FILESYS_LOG_ENABLED then FileSysLogDebug('                                     SecurityHash = ' + IntToHex(SecurityItem.SecurityHash,8));
       if SecurityItem.SecurityId <> 0 then
        begin
         if FILESYS_LOG_ENABLED then FileSysLogDebug('                                  Calculated Hash = ' + IntToHex(SecurityItem.Security.SecurityHash,8));
        end;
     {$ENDIF}
     end;
 
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                                          AOffset = ' + IntToHex(AOffset,8));
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                                            ASize = ' + IntToHex(ASize,8));
    {$ENDIF}
    
    {Check Size}
    if ASize >= ntfsSecurityItemSize then
     begin
      {Check Offset}
      Offset:=(AOffset and ntfsSecurityOffsetMask);   {Get the offset within this Section}
      Section:=(AOffset and ntfsSecuritySectionMask); {Get the starting offset of this Section}
      
      {$IFDEF NTFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('                                           Offset = ' + IntToHex(Offset,8));
      if FILESYS_LOG_ENABLED then FileSysLogDebug('                                          Section = ' + IntToHex(Section,8));
      {$ENDIF}
      
      if (Offset = 0) and ((Section and ntfsSecurityMirrorTest) = ntfsSecurityMirrorTest) then
       begin
        {Update Offset}
        Size:=(ntfsSecuritySectionOffset - Offset);  {Get the bytes remaining before the start of the next Section}
        
        {$IFDEF NTFS_DEBUG}
        if FILESYS_LOG_ENABLED then FileSysLogDebug('                                  Size (Boundary) = ' + IntToHex(Size,8));
        {$ENDIF}
        
        if Size > ASize then Break;
        Dec(ASize,Size);
        Inc(AOffset,Size);
        
        {$IFDEF NTFS_DEBUG}
        if FILESYS_LOG_ENABLED then FileSysLogDebug('                                AOffset (Updated) = ' + IntToHex(AOffset,8));
        if FILESYS_LOG_ENABLED then FileSysLogDebug('                                  ASize (Updated) = ' + IntToHex(ASize,8));
        {$ENDIF}
       end
      else
       begin
        {Check Data}
        SecurityItemData:=PNTFSSecurityItemData(LongWord(ABuffer) + AOffset);
        
        {Check Last}
        if SecurityItemData.SecuritySize = 0 then
         begin
          {Update Offset}
          Size:=(ntfsSecuritySectionOffset - Offset); {Get the bytes remaining before the end of this Section}
          
          {$IFDEF NTFS_DEBUG}
          if FILESYS_LOG_ENABLED then FileSysLogDebug('                                     Size (Below) = ' + IntToHex(Size,8));
          {$ENDIF}
          
          if (Size + ntfsSecuritySectionOffset) > ASize then Break;
          Dec(ASize,Size + ntfsSecuritySectionOffset);
          Inc(AOffset,Size + ntfsSecuritySectionOffset);
          
          {$IFDEF NTFS_DEBUG}
          if FILESYS_LOG_ENABLED then FileSysLogDebug('                                AOffset (Updated) = ' + IntToHex(AOffset,8));
          if FILESYS_LOG_ENABLED then FileSysLogDebug('                                  ASize (Updated) = ' + IntToHex(ASize,8));
          {$ENDIF}
         end;
       end;
     end;
   end;
   
  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSSecurityItems.ReadSecurityItems - Success');
  {$ENDIF}
  
  Result:=True;
 finally
  WriterUnlock;
 end;
end;

{==============================================================================}

function TNTFSSecurityItems.WriteSecurityItems(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Write the security items to the supplied buffer at the supplied offset}
var
 Size:LongWord;
 Offset:LongWord;
 Section:LongWord;
 SecurityItem:TNTFSSecurityItem;
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  if ABuffer = nil then Exit;
  
  {Get SecurityItem}
  SecurityItem:=TNTFSSecurityItem(First);
  
  {Write SecurityItems}
  while SecurityItem <> nil do
   begin
    {Check Size}
    if ASize < ntfsSecurityItemSize then Exit;
    if SecurityItem.SecuritySize = 0 then Exit;
    {if SecurityItem.SecurityOffset <> AOffset then Exit;} {Offset can be zero}
    if (SecurityItem.SecurityOffset <> 0) and (SecurityItem.SecurityOffset <> AOffset) then Exit;
    {if (SecurityItem.SecurityId <> 0) and (SecurityItem.SecurityOffset <> AOffset) then Exit;} {Is this a better test}
    
    {Write SecurityItem}
    if not SecurityItem.WriteSecurityItem(ABuffer,AOffset,ASize,AVersion) then Exit;
    
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSSecurityItems.WriteSecurityItems - SecurityId = ' + IntToHex(SecurityItem.SecurityId,8));
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                                      SecuritySize = ' + IntToHex(SecurityItem.SecuritySize,8));
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                                    SecurityOffset = ' + IntToHex(SecurityItem.SecurityOffset,16));
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                                      SecurityHash = ' + IntToHex(SecurityItem.SecurityHash,8));
    {$ENDIF}
    
    {Get SecurityItem}
    SecurityItem:=TNTFSSecurityItem(SecurityItem.Next);
    if SecurityItem = nil then Break;
 
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                                           AOffset = ' + IntToHex(AOffset,8));
    if FILESYS_LOG_ENABLED then FileSysLogDebug('                                             ASize = ' + IntToHex(ASize,8));
    {$ENDIF}
    
    {Check SecurityItem}
    if SecurityItem.SecurityOffset <> AOffset then
     begin
      {Check Offset}
      Offset:=(AOffset and ntfsSecurityOffsetMask);   {Get the offset within this Section}
      Section:=(AOffset and ntfsSecuritySectionMask); {Get the starting offset of this Section}
      
      {$IFDEF NTFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('                                            Offset = ' + IntToHex(Offset,8));
      if FILESYS_LOG_ENABLED then FileSysLogDebug('                                           Section = ' + IntToHex(Section,8));
      {$ENDIF}
      
      if (Offset = 0) and ((Section and ntfsSecurityMirrorTest) = ntfsSecurityMirrorTest) then
       begin
        {Update Offset}
        Size:=(ntfsSecuritySectionOffset - Offset);  {Get the bytes remaining before the start of the next Section}
        
        {$IFDEF NTFS_DEBUG}
        if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   Size (Boundary) = ' + IntToHex(Size,8));
        {$ENDIF}
        
        if Size > ASize then Break;
        Dec(ASize,Size);
        Inc(AOffset,Size);
        
        {$IFDEF NTFS_DEBUG}
        if FILESYS_LOG_ENABLED then FileSysLogDebug('                                 AOffset (Updated) = ' + IntToHex(AOffset,8));
        if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   ASize (Updated) = ' + IntToHex(ASize,8));
        {$ENDIF}
       end
      else
       begin
        {Update Offset}
        Size:=(ntfsSecuritySectionOffset - Offset); {Get the bytes remaining before the end of this Section}
        
        {$IFDEF NTFS_DEBUG}
        if FILESYS_LOG_ENABLED then FileSysLogDebug('                                      Size (Below) = ' + IntToHex(Size,8));
        {$ENDIF}
        
        if (Size + ntfsSecuritySectionOffset) > ASize then Break;
        Dec(ASize,Size + ntfsSecuritySectionOffset);
        Inc(AOffset,Size + ntfsSecuritySectionOffset);
        
        {$IFDEF NTFS_DEBUG}
        if FILESYS_LOG_ENABLED then FileSysLogDebug('                                 AOffset (Updated) = ' + IntToHex(AOffset,8));
        if FILESYS_LOG_ENABLED then FileSysLogDebug('                                   ASize (Updated) = ' + IntToHex(ASize,8));
        {$ENDIF}
       end;
     end;
   end;
   
  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSSecurityItems.WriteSecurityItems - Success');
  {$ENDIF}
  
  Result:=True;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}
{==============================================================================}
{TNTFSSecurityItem}
constructor TNTFSSecurityItem.Create(ALocalLock:TMutexHandle);
begin
 {}
 inherited Create;
 FLocalLock:=ALocalLock;
 
 FSecurity:=nil;
end;

{==============================================================================}

destructor TNTFSSecurityItem.Destroy;
begin
 {}
 if FSecurity <> nil then FSecurity.Free;
 FSecurity:=nil;
 
 FLocalLock:=INVALID_HANDLE_VALUE;
 inherited Destroy;
end;

{==============================================================================}

function TNTFSSecurityItem.AcquireLock:Boolean;
begin
 {}
 Result:=(MutexLock(FLocalLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSSecurityItem.ReleaseLock:Boolean;
begin
 {}
 Result:=(MutexUnlock(FLocalLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNTFSSecurityItem.Compare(ASecurityItem:TNTFSSecurityItem):Integer;
{Replaces function below, Security Items are sorted by Offset not by Id}
begin
 {}
 Result:=ntfsCompareGreater;
 
 if ASecurityItem = nil then Exit;
 
 {Check Security Offset}
 if ASecurityItem.SecurityOffset = FSecurityOffset then
  begin
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSSecurityItem.Compare - Offset ' + IntToHex(ASecurityItem.SecurityOffset,16) + ' = ' + IntToHex(FSecurityOffset,16));
   {$ENDIF}
   
   Result:=ntfsCompareEqual;
  end
 else if ASecurityItem.SecurityOffset < FSecurityOffset then
  begin
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSSecurityItem.Compare - Offset ' + IntToHex(ASecurityItem.SecurityOffset,16) + ' < ' + IntToHex(FSecurityOffset,16));
   {$ENDIF}
   
   Result:=ntfsCompareLess;
  end
 else if ASecurityItem.SecurityOffset > FSecurityOffset then
  begin
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSSecurityItem.Compare - Offset ' + IntToHex(ASecurityItem.SecurityOffset,16) + ' > ' + IntToHex(FSecurityOffset,16));
   {$ENDIF}
   
   Result:=ntfsCompareGreater;
  end
end;

{==============================================================================}
 //To Do //Remove ?
function TNTFSSecurityItem.CompareOld(ASecurityItem:TNTFSSecurityItem):Integer;
begin
 {}
 Result:=ntfsCompareGreater;
 
 if ASecurityItem = nil then Exit;
 
 {Check Security Id}
 if ASecurityItem.SecurityId = FSecurityId then
  begin
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSSecurityItem.Compare - Id ' + IntToHex(ASecurityItem.SecurityId,8) + ' = ' + IntToHex(FSecurityId,8));
   {$ENDIF}
   
   Result:=ntfsCompareEqual;
  end
 else if ASecurityItem.SecurityId < FSecurityId then
  begin
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSSecurityItem.Compare - Id ' + IntToHex(ASecurityItem.SecurityId,8) + ' < ' + IntToHex(FSecurityId,8));
   {$ENDIF}
   
   Result:=ntfsCompareLess;
  end
 else if ASecurityItem.SecurityId > FSecurityId then
  begin
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('TNTFSSecurityItem.Compare - Id ' + IntToHex(ASecurityItem.SecurityId,8) + ' > ' + IntToHex(FSecurityId,8));
   {$ENDIF}
   
   Result:=ntfsCompareGreater;
  end
end;

{==============================================================================}

function TNTFSSecurityItem.CreateSecurity:Boolean;
{Create a security, setup properties do not update Item}
begin
 {}
 Result:=False;
 
 if FSecurity <> nil then Exit;
 
 {Create Security}
 FSecurity:=TNTFSSecurity.Create(INVALID_HANDLE_VALUE); //To Do //Critical
 
 Result:=True;
end;

{==============================================================================}

function TNTFSSecurityItem.NewSecurity(ASecurity:TNTFSSecurity):Boolean;
{Create a security, setup properties and update Item}
begin
 {}
 if ASecurity = nil then
  begin
   Result:=CreateSecurity;
   if not Result then Exit;
   
   {Setup Security}
   FSecurity.Control:=SE_SELF_RELATIVE;
  end
 else
  begin
   FSecurity:=ASecurity;
   
   Result:=True;
  end;
end;

{==============================================================================}

function TNTFSSecurityItem.UpdateSecurity(ASecurity:TNTFSSecurity):Boolean;
begin
 {}
 Result:=False;
 
 //To Do //Called by UpdateSecurityItem to convert a Blank to an Item
end;

{==============================================================================}

function TNTFSSecurityItem.DeleteSecurity:Boolean;
begin
 {}
 Result:=False;
 
 //To Do //Called by DeleteSecurityItem to convert an Item to a Blank
               //Delete and Free the Security descriptor
end;

{==============================================================================}

function TNTFSSecurityItem.RemoveSecurity(AFree:Boolean):Boolean;
{Called by RemoveSecurityItem to remove and free the security if requested}
begin
 {}
 Result:=False;
 
 if FSecurity = nil then
  begin
   Result:=True;
  end
 else
  begin
   if AFree then FSecurity.Free;
   FSecurity:=nil;
   
   Result:=True;
  end;
end;

{==============================================================================}

function TNTFSSecurityItem.MirrorOffset:Int64;
begin
 {}
 Result:=FSecurityOffset + ntfsSecurityMirrorOffset;
end;

{==============================================================================}

function TNTFSSecurityItem.Init(ASecurityId:LongWord;AVersion:Word):Boolean;
var
 Descriptor:Pointer;
begin
 {}
 Result:=False;

 {Create Descriptor}
 Descriptor:=nil;
 case ASecurityId of
  ntfsDefaultSecurityId100:begin
    if not NTFSCreateDefaultDescriptor(ntfsDefaultDescriptor100,Descriptor,AVersion) then Exit;
   end;
  ntfsDefaultSecurityId101:begin
    if not NTFSCreateDefaultDescriptor(ntfsDefaultDescriptor101,Descriptor,AVersion) then Exit;
   end;
  ntfsDefaultSecurityId102:begin
    if not NTFSCreateDefaultDescriptor(ntfsDefaultDescriptor102,Descriptor,AVersion) then Exit;
   end;
  ntfsDefaultSecurityId103:begin
    if not NTFSCreateDefaultDescriptor(ntfsDefaultDescriptor103,Descriptor,AVersion) then Exit;
   end;
 end;
 if Descriptor = nil then Exit;
 try
  {Create Security}
  if not CreateSecurity then Exit;
  if FSecurity = nil then Exit;
  
  {Setup Security}
  if not FSecurity.CopyFromDescriptor(Descriptor,{Security.}GetSecurityDescriptorLength(Descriptor)) then Exit; {Security references the internal property}
  
  {Update Item}
  FSecurityHash:=FSecurity.SecurityHash;
  FSecurityId:=ASecurityId;
  FSecurityOffset:=0;
  FSecuritySize:=(ntfsSecurityItemSize - ntfsSecuritySize) + FSecurity.SecuritySize;
  
  Result:=True;
 finally
  NTFSDestroyDefaultDescriptor(Descriptor,AVersion);
 end;
end;

{==============================================================================}

function TNTFSSecurityItem.ReadSecurityItem(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Read the securityitem data from the supplied buffer at the supplied offset}
var
 Size:LongWord;
 Offset:LongWord;
 SecurityItemData:PNTFSSecurityItemData;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {Check Size}
 if ASize >= ntfsSecurityItemSize then
  begin
   {Get Data}
   SecurityItemData:=PNTFSSecurityItemData(LongWord(ABuffer) + AOffset);
   
   {Check Last}
   if SecurityItemData.SecuritySize = 0 then Exit;
   
   {Read Data}
   FSecurityHash:=SecurityItemData.SecurityHash;
   FSecurityId:=SecurityItemData.SecurityId;
   FSecurityOffset:=SecurityItemData.SecurityOffset;
   FSecuritySize:=SecurityItemData.SecuritySize;
   if FSecurityId = 0 then
    begin
     FSecurityOffset:=AOffset;
    end
   else
    begin
     {Set Offset}
     Size:=FSecuritySize - (ntfsSecurityItemSize - ntfsSecuritySize);
     Offset:=AOffset + (ntfsSecurityItemSize - ntfsSecuritySize);
     
     {Create Security}
     if not CreateSecurity then Exit;
     
     {Read Security}
     if not FSecurity.ReadSecurity(ABuffer,Offset,Size,AVersion) then Exit;
    end;

   {Update Offset}
   Dec(ASize,NTFSRoundLongWordTo16Bytes(FSecuritySize));
   Inc(AOffset,NTFSRoundLongWordTo16Bytes(FSecuritySize));
   
   Result:=True;
  end;
end;

{==============================================================================}

function TNTFSSecurityItem.WriteSecurityItem(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Write the securityitem data to the supplied buffer at the supplied offset}
var
 Size:LongWord;
 Offset:LongWord;
 SecurityItemData:PNTFSSecurityItemData;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {Check Size}
 if ASize >= ntfsSecurityItemSize then
  begin
   {Get Data}
   SecurityItemData:=PNTFSSecurityItemData(LongWord(ABuffer) + AOffset);
   
   {Check Size}
   if FSecuritySize = 0 then Exit;
   
   {Write Data}
   SecurityItemData.SecurityHash:=FSecurityHash;
   SecurityItemData.SecurityId:=FSecurityId;
   SecurityItemData.SecurityOffset:=FSecurityOffset;
   SecurityItemData.SecuritySize:=FSecuritySize;
   if FSecurityId = 0 then
    begin
     SecurityItemData.SecurityOffset:=0;
    end
   else
    begin
     {Set Offset}
     Size:=FSecuritySize - (ntfsSecurityItemSize - ntfsSecuritySize);
     Offset:=AOffset + (ntfsSecurityItemSize - ntfsSecuritySize);
     
     {Check Security}
     if FSecurity = nil then Exit;
     
     {Write Security}
     if not FSecurity.WriteSecurity(ABuffer,Offset,Size,AVersion) then Exit;
    end;

   {Check Size (Mirror)}
   if ASize >= (ntfsSecurityItemSize + ntfsSecurityMirrorOffset) then
    begin
     {Get Data (Mirror)}
     SecurityItemData:=PNTFSSecurityItemData(LongWord(ABuffer) + AOffset + ntfsSecurityMirrorOffset);
     
     {Write Data (Mirror)}
     SecurityItemData.SecurityHash:=FSecurityHash;
     SecurityItemData.SecurityId:=FSecurityId;
     SecurityItemData.SecurityOffset:=FSecurityOffset;
     SecurityItemData.SecuritySize:=FSecuritySize;
     if FSecurityId = 0 then
      begin
       SecurityItemData.SecurityOffset:=0;
      end
     else
      begin
       {Set Offset (Mirror)}
       Size:=FSecuritySize - (ntfsSecurityItemSize - ntfsSecuritySize);
       Offset:=AOffset  + ntfsSecurityMirrorOffset + (ntfsSecurityItemSize - ntfsSecuritySize);
       
       {Check Security}
       if FSecurity = nil then Exit;
       
       {Write Security (Mirror)}
       if not FSecurity.WriteSecurity(ABuffer,Offset,Size,AVersion) then Exit;
      end;
    end;

   {Update Offset}
   Dec(ASize,NTFSRoundLongWordTo16Bytes(FSecuritySize));
   Inc(AOffset,NTFSRoundLongWordTo16Bytes(FSecuritySize));
   
   Result:=True;
  end;
end;

{==============================================================================}
{==============================================================================}
{TNTFSSecurity}
constructor TNTFSSecurity.Create(ALocalLock:TMutexHandle);
begin
 {}
 inherited Create(ALocalLock);
 FRevision:=0;
 FControl:=0;
 FSacl:=nil;
 FDacl:=nil;
 FOwner:=nil;
 FGroup:=nil;
end;

{==============================================================================}

constructor TNTFSSecurity.CreateFromSecurity(ALocalLock:TMutexHandle;ASecurity:TDiskSecurity);
begin
 {}
 Create(ALocalLock);
 
 if ASecurity = nil then Exit;
 if not(ASecurity is TNTFSSecurity) then Exit;
 
 Revision:=TNTFSSecurity(ASecurity).Revision;
 Control:=TNTFSSecurity(ASecurity).Control;
 Sacl:=TNTFSSecurity(ASecurity).Sacl;
 Dacl:=TNTFSSecurity(ASecurity).Dacl;
 Owner:=TNTFSSecurity(ASecurity).Owner;
 Group:=TNTFSSecurity(ASecurity).Group;
end;

{==============================================================================}

constructor TNTFSSecurity.CreateFromDescriptor(ALocalLock:TMutexHandle;ADescriptor:Pointer);
var
 Size:LongWord;
 Offset:LongWord;
begin
 {}
 Create(ALocalLock);
 
 if ADescriptor = nil then Exit;

 Size:=Security.GetSecurityDescriptorLength(ADescriptor);
 if Size < ntfsSecuritySize then Exit;
 
 Offset:=0;
 ReadSecurity(ADescriptor,Offset,Size,ntfsNTFS31); //To Do //AVersion ? //From where ?
end;

{==============================================================================}

destructor TNTFSSecurity.Destroy;
begin
 {}
 if FSacl <> nil then FreeMem(FSacl);
 if FDacl <> nil then FreeMem(FDacl);
 if FOwner <> nil then FreeMem(FOwner);
 if FGroup <> nil then FreeMem(FGroup);
 inherited Destroy;
end;

{==============================================================================}

procedure TNTFSSecurity.SetSacl(ASacl:PACL);
begin
 {}
 if ASacl = nil then
  begin
   if FSacl <> nil then FreeMem(FSacl);
   FSacl:=nil;
  end
 else
  begin
   SetSaclSize(ASacl.AclSize);
   if ASacl.AclSize > 0 then System.Move(ASacl^,FSacl^,ASacl.AclSize);
  end;
end;

{==============================================================================}

procedure TNTFSSecurity.SetDacl(ADacl:PACL);
begin
 {}
 if ADacl = nil then
  begin
   if FDacl <> nil then FreeMem(FDacl);
   FDacl:=nil;
  end
 else
  begin
   SetDaclSize(ADacl.AclSize);
   if ADacl.AclSize > 0 then System.Move(ADacl^,FDacl^,ADacl.AclSize);
  end;
end;

{==============================================================================}

procedure TNTFSSecurity.SetOwner(AOwner:PSID);
var
 SidSize:LongWord;
begin
 {}
 if AOwner = nil then
  begin
   if FOwner <> nil then FreeMem(FOwner);
   FOwner:=nil;
  end
 else
  begin
   SidSize:=Security.GetLengthSid(AOwner);
   SetOwnerSize(SidSize);
   if SidSize > 0 then System.Move(AOwner^,FOwner^,SidSize);
  end;
end;

{==============================================================================}

procedure TNTFSSecurity.SetGroup(AGroup:PSID);
var
 SidSize:LongWord;
begin
 {}
 if AGroup = nil then
  begin
   if FGroup <> nil then FreeMem(FGroup);
   FGroup:=nil;
  end
 else
  begin
   SidSize:=Security.GetLengthSid(AGroup);
   SetGroupSize(SidSize);
   if SidSize > 0 then System.Move(AGroup^,FGroup^,SidSize);
  end;
end;

{==============================================================================}

procedure TNTFSSecurity.SetSaclSize(ASize:Word);
begin
 {}
 if ASize = 0 then
  begin
   if FSacl <> nil then FreeMem(FSacl);
   FSacl:=nil;
  end
 else
  begin
   if FSacl <> nil then FreeMem(FSacl);
   FSacl:=GetMem(ASize);
  end;
end;

{==============================================================================}

procedure TNTFSSecurity.SetDaclSize(ASize:Word);
begin
 {}
 if ASize = 0 then
  begin
   if FDacl <> nil then FreeMem(FDacl);
   FDacl:=nil;
  end
 else
  begin
   if FDacl <> nil then FreeMem(FDacl);
   FDacl:=GetMem(ASize);
  end;
end;

{==============================================================================}

procedure TNTFSSecurity.SetOwnerSize(ASize:Word);
begin
 {}
 if ASize = 0 then
  begin
   if FOwner <> nil then FreeMem(FOwner);
   FOwner:=nil;
  end
 else
  begin
   if FOwner <> nil then FreeMem(FOwner);
   FOwner:=GetMem(ASize);
  end;
end;

{==============================================================================}

procedure TNTFSSecurity.SetGroupSize(ASize:Word);
begin
 {}
 if ASize = 0 then
  begin
   if FGroup <> nil then FreeMem(FGroup);
   FGroup:=nil;
  end
 else
  begin
   if FGroup <> nil then FreeMem(FGroup);
   FGroup:=GetMem(ASize);
  end;
end;

{==============================================================================}

function TNTFSSecurity.SaclSize:Word;
begin
 {}
 Result:=0;
 
 if FSacl = nil then Exit;
 
 Result:=FSacl.AclSize;
end;

{==============================================================================}

function TNTFSSecurity.DaclSize:Word;
begin
 {}
 Result:=0;
 
 if FDacl = nil then Exit;
 
 Result:=FDacl.AclSize;
end;

{==============================================================================}

function TNTFSSecurity.OwnerSize:Word;
begin
 {}
 Result:=0;
 
 if FOwner = nil then Exit;
 
 Result:=Security.GetLengthSid(FOwner);
end;

{==============================================================================}

function TNTFSSecurity.GroupSize:Word;
begin
 {}
 Result:=0;
 
 if FGroup = nil then Exit;
 
 Result:=Security.GetLengthSid(FGroup);
end;

{==============================================================================}

function TNTFSSecurity.SaclOffset:LongWord;
{Note: Order is always Sacl, Dacl, Owner, Group (Disk Format)}
begin
 {}
 Result:=0;
 
 if FSacl = nil then Exit;
 
 Result:=ntfsSecuritySize;
end;

{==============================================================================}

function TNTFSSecurity.DaclOffset:LongWord;
{Note: Order is always Sacl, Dacl, Owner, Group (Disk Format)}
begin
 {}
 Result:=0;
 
 if FDacl = nil then Exit;
 
 Result:=ntfsSecuritySize;
 
 if FSacl <> nil then Inc(Result,SaclSize);
end;

{==============================================================================}

function TNTFSSecurity.OwnerOffset:LongWord;
{Note: Order is always Sacl, Dacl, Owner, Group (Disk Format)}
begin
 {}
 Result:=0;
 
 if FOwner = nil then Exit;
 
 Result:=ntfsSecuritySize;
 
 if FSacl <> nil then Inc(Result,SaclSize);
 if FDacl <> nil then Inc(Result,DaclSize);
end;

{==============================================================================}

function TNTFSSecurity.GroupOffset:LongWord;
{Note: Order is always Sacl, Dacl, Owner, Group (Disk Format)}
begin
 {}
 Result:=0;
 
 if FGroup = nil then Exit;
 
 Result:=ntfsSecuritySize;
 
 if FSacl <> nil then Inc(Result,SaclSize);
 if FDacl <> nil then Inc(Result,DaclSize);
 if FOwner <> nil then Inc(Result,OwnerSize);
end;

{==============================================================================}

function TNTFSSecurity.SaclOffsetEx(ALocal:Boolean):LongWord;  {Offset to SACL}
{Note: Order is always Owner, Group, Dacl, Sacl (API Format}
{Local indicates the Descriptor should be in Disk Format (not API Format)}
begin
 {}
 Result:=0;
 
 if FSacl = nil then Exit;

 if ALocal then
  begin
   Result:=SaclOffset;
  end
 else
  begin
   Result:=ntfsSecuritySize;
   
   if FOwner <> nil then Inc(Result,OwnerSize);
   if FGroup <> nil then Inc(Result,GroupSize);
   if FDacl <> nil then Inc(Result,DaclSize);
  end;
end;

{==============================================================================}

function TNTFSSecurity.DaclOffsetEx(ALocal:Boolean):LongWord;  {Offset to DACL}
{Note: Order is always Owner, Group, Dacl, Sacl (API Format}
{Local indicates the Descriptor should be in Disk Format (not API Format)}
begin
 {}
 Result:=0;
 
 if FDacl = nil then Exit;

 if ALocal then
  begin
   Result:=DaclOffset;
  end
 else
  begin
   Result:=ntfsSecuritySize;
   
   if FOwner <> nil then Inc(Result,OwnerSize);
   if FGroup <> nil then Inc(Result,GroupSize);
  end;
end;

{==============================================================================}

function TNTFSSecurity.OwnerOffsetEx(ALocal:Boolean):LongWord; {Offset to Owner SID}
{Note: Order is always Owner, Group, Dacl, Sacl (API Format}
{Local indicates the Descriptor should be in Disk Format (not API Format)}
begin
 {}
 Result:=0;
 
 if FOwner = nil then Exit;

 if ALocal then
  begin
   Result:=OwnerOffset;
  end
 else
  begin
   Result:=ntfsSecuritySize;
  end;
end;

{==============================================================================}

function TNTFSSecurity.GroupOffsetEx(ALocal:Boolean):LongWord; {Offset to Group SID}
{Note: Order is always Owner, Group, Dacl, Sacl (API Format}
{Local indicates the Descriptor should be in Disk Format (not API Format)}
begin
 {}
 Result:=0;
 
 if FGroup = nil then Exit;

 if ALocal then
  begin
   Result:=GroupOffset;
  end
 else
  begin
   Result:=ntfsSecuritySize;
   
   if FOwner <> nil then Inc(Result,OwnerSize);
  end;
end;

{==============================================================================}

function TNTFSSecurity.SecuritySize:LongWord;
begin
 {}
 Result:=ntfsSecuritySize;
 Inc(Result,SaclSize);
 Inc(Result,DaclSize);
 Inc(Result,OwnerSize);
 Inc(Result,GroupSize);
end;

{==============================================================================}

function TNTFSSecurity.SecurityHash:LongWord;
var
 Size:LongWord;
 Descriptor:Pointer;
begin
 {}
 Result:=0;
 
 Descriptor:=SecurityDescriptor;
 if Descriptor = nil then Exit;
 try
  Size:=Security.GetSecurityDescriptorLength(Descriptor);
  if Size < ntfsSecuritySize then Exit;
  
  Result:=NTFSGenerateSecurityHash(Descriptor,Size);
 finally
  FreeMem(Descriptor);
 end;
end;

{==============================================================================}

function TNTFSSecurity.SecurityDescriptor:Pointer;
var
 Size:LongWord;
 Offset:LongWord;
begin
 {}
 Result:=nil;
 
 Size:=SecuritySize;
 if Size < ntfsSecuritySize then Exit;
 
 Offset:=0;
 Result:=AllocMem(Size);
 if Result = nil then Exit;
 
 if not WriteSecurity(Result,Offset,Size,ntfsNTFS31) then //To Do //AVersion ? //From where ?
  begin
   FreeMem(Result);
   Result:=nil;
  end;
end;

{==============================================================================}

function TNTFSSecurity.SecurityDescriptorEx(ALocal:Boolean):Pointer;
{Local indicates the Descriptor should be in Disk Format (not API Format)}
var
 Size:LongWord;
 Offset:LongWord;
begin
 {}
 Result:=nil;
 
 if ALocal then
  begin
   Result:=SecurityDescriptor;
  end
 else
  begin
   Size:=SecuritySize;
   if Size < ntfsSecuritySize then Exit;
   
   Offset:=0;
   Result:=AllocMem(Size);
   if Result = nil then Exit;
   
   if not WriteSecurityEx(Result,Offset,Size,ALocal,ntfsNTFS31) then //To Do //AVersion ? //From where ?
    begin
     FreeMem(Result);
     Result:=nil;
    end;
  end;
end;

{==============================================================================}

function TNTFSSecurity.InheritedDescriptor:Pointer;
var
 Descriptor:Pointer;
begin
 {}
 Result:=nil;
 
 {Get Descriptor}
 Descriptor:=SecurityDescriptor;
 if Descriptor = nil then Exit;
 try
  {Create Inherited}
  if not NTFSCreateInheritedDescriptor(Descriptor,Result,ntfsNTFS31) then //To Do //AVersion ? //From where ?
   begin
    Result:=nil;
   end;
 finally
  FreeMem(Descriptor);
 end;
end;

{==============================================================================}

function TNTFSSecurity.MergedDescriptor(AChild:Pointer):Pointer;
var
 Descriptor:Pointer;
begin
 {}
 Result:=nil;
 
 {Get Descriptor}
 Descriptor:=SecurityDescriptor;
 if Descriptor = nil then Exit;
 try
  {Create Merged}
  if not NTFSCreateMergedDescriptor(Descriptor,AChild,Result,ntfsNTFS31) then //To Do //AVersion ? //From where ?
   begin
    Result:=nil;
   end;
 finally
  FreeMem(Descriptor);
 end;
end;

{==============================================================================}

function TNTFSSecurity.ReleaseDescriptor(ADescriptor:Pointer;AInherited,AMerged:Boolean):Boolean;
begin
 {}
 Result:=False;
 
 if ADescriptor = nil then Exit;

 if AInherited then
  begin
   Result:=NTFSDestroyInheritedDescriptor(ADescriptor,ntfsNTFS31); //To Do //AVersion ? //From where ?
  end
 else if AMerged then
  begin
   Result:=NTFSDestroyMergedDescriptor(ADescriptor,ntfsNTFS31);   //To Do //AVersion ? //From where ?
  end
 else
  begin
   FreeMem(ADescriptor);
   
   Result:=True;
  end;
end;

{==============================================================================}

function TNTFSSecurity.CopyToSecurity(ASecurity:TDiskSecurity):Boolean;
begin
 {}
 Result:=False;
 
 if ASecurity = nil then Exit;
 if not(ASecurity is TNTFSSecurity) then Exit;

 TNTFSSecurity(ASecurity).Revision:=Revision;
 TNTFSSecurity(ASecurity).Control:=Control;
 TNTFSSecurity(ASecurity).Sacl:=Sacl;
 TNTFSSecurity(ASecurity).Dacl:=Dacl;
 TNTFSSecurity(ASecurity).Owner:=Owner;
 TNTFSSecurity(ASecurity).Group:=Group;
 
 Result:=True;
end;

{==============================================================================}

function TNTFSSecurity.CopyToDescriptor(ADescriptor:Pointer;ASize:LongWord):Boolean;
var
 Size:LongWord;
 Offset:LongWord;
begin
 {}
 Result:=False;
 
 if ADescriptor = nil then Exit;

 Size:=SecuritySize;
 if ASize < Size then Exit;
 if Size < ntfsSecuritySize then Exit;
 
 Offset:=0;
 if WriteSecurity(ADescriptor,Offset,Size,ntfsNTFS31) then //To Do //AVersion ? //From where ?
  begin
   Result:=True;
  end;
end;

{==============================================================================}

function TNTFSSecurity.CopyToDescriptorEx(ADescriptor:Pointer;ASize:LongWord;ALocal:Boolean):Boolean;
{Local indicates the Descriptor should be in Disk Format (not API Format)}
var
 Size:LongWord;
 Offset:LongWord;
begin
 {}
 Result:=False;
 
 if ADescriptor = nil then Exit;

 if ALocal then
  begin
   Result:=CopyToDescriptor(ADescriptor,ASize);
  end
 else
  begin
   Size:=SecuritySize;
   if ASize < Size then Exit;
   if Size < ntfsSecuritySize then Exit;
   
   Offset:=0;
   if WriteSecurityEx(ADescriptor,Offset,Size,ALocal,ntfsNTFS31) then //To Do //AVersion ? //From where ?
    begin
     Result:=True;
    end;
  end;
end;

{==============================================================================}

function TNTFSSecurity.CopyFromDescriptor(ADescriptor:Pointer;ASize:LongWord):Boolean;
var
 Size:LongWord;
 Offset:LongWord;
begin
 {}
 Result:=False;
 
 if ADescriptor = nil then Exit;

 Size:=ASize;
 if Size < ntfsSecuritySize then Exit;
 
 Offset:=0;
 if ReadSecurity(ADescriptor,Offset,Size,ntfsNTFS31) then //To Do //AVersion ? //From where ?
  begin
   Result:=True;
  end;
end;

{==============================================================================}

function TNTFSSecurity.ReadSecurity(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Read the security descriptor from the supplied buffer at the supplied offset}
var
 Acl:PACL;
 Sid:PSID;
 SecurityData:PNTFSSecurityData;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {Get Data}
 SecurityData:=PNTFSSecurityData(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if ASize >= ntfsSecuritySize then
  begin
   {Read Data}
   FRevision:=SecurityData.Revision;
   FControl:=SecurityData.Control;
   
   {Check Offset}
   if SecurityData.SaclOffset > 0 then
    begin
     {Get Sacl}
     Acl:=PACL(LongWord(ABuffer) + AOffset + SecurityData.SaclOffset);
     
     {Read Sacl}
     SetSacl(Acl);
    end;
   
   {Check Offset}
   if SecurityData.DaclOffset > 0 then
    begin
     {Get Dacl}
     Acl:=PACL(LongWord(ABuffer) + AOffset + SecurityData.DaclOffset);
     
     {Read Dacl}
     SetDacl(Acl);
    end;
   
   {Check Offset}
   if SecurityData.OwnerOffset > 0 then
    begin
     {Get Owner}
     Sid:=PSID(LongWord(ABuffer) + AOffset + SecurityData.OwnerOffset);
     
     {Read Owner}
     SetOwner(Sid);
    end;
   
   {Check Offset}
   if SecurityData.GroupOffset > 0 then
    begin
     {Get Group}
     Sid:=PSID(LongWord(ABuffer) + AOffset + SecurityData.GroupOffset);
     
     {Read Group}
     SetGroup(Sid);
    end;
   
   {Update Offset}
   Dec(ASize,SecuritySize);
   Inc(AOffset,SecuritySize);
   
   Result:=True;
  end;
end;

{==============================================================================}

function TNTFSSecurity.WriteSecurity(ABuffer:Pointer;var AOffset,ASize:LongWord;AVersion:Word):Boolean;
{Write the security descriptor to the supplied buffer at the supplied offset}
var
 Acl:PACL;
 Sid:PSID;
 SecurityData:PNTFSSecurityData;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 {Get Data}
 SecurityData:=PNTFSSecurityData(LongWord(ABuffer) + AOffset);
 
 {Check Size}
 if ASize >= ntfsSecuritySize then
  begin
   {Write Data}
   SecurityData.Revision:=FRevision;
   SecurityData.Reserved1:=0;
   SecurityData.Control:=FControl;
   SecurityData.SaclOffset:=SaclOffset;
   SecurityData.DaclOffset:=DaclOffset;
   SecurityData.OwnerOffset:=OwnerOffset;
   SecurityData.GroupOffset:=GroupOffset;
   
   {Check Object}
   if FSacl <> nil then
    begin
     {Write Sacl}
     Acl:=PACL(LongWord(ABuffer) + AOffset + SecurityData.SaclOffset);
     System.Move(FSacl^,Acl^,SaclSize);
    end;
   
   {Check Object}
   if FDacl <> nil then
    begin
     {Write Dacl}
     Acl:=PACL(LongWord(ABuffer) + AOffset + SecurityData.DaclOffset);
     System.Move(FDacl^,Acl^,DaclSize);
    end;
   
   {Check Object}
   if FOwner <> nil then
    begin
     {Write Owner}
     Sid:=PSID(LongWord(ABuffer) + AOffset + SecurityData.OwnerOffset);
     System.Move(FOwner^,Sid^,OwnerSize);
    end;
   
   {Check Object}
   if FGroup <> nil then
    begin
     {Write Group}
     Sid:=PSID(LongWord(ABuffer) + AOffset + SecurityData.GroupOffset);
     System.Move(FGroup^,Sid^,GroupSize);
    end;
   
   {Update Offset}
   Dec(ASize,SecuritySize);
   Inc(AOffset,SecuritySize);
   
   Result:=True;
  end;
end;

{==============================================================================}

function TNTFSSecurity.WriteSecurityEx(ABuffer:Pointer;var AOffset,ASize:LongWord;ALocal:Boolean;AVersion:Word):Boolean;
{Write the security descriptor to the supplied buffer at the supplied offset}
{Local indicates the Descriptor should be in Disk Format (not API Format)}
var
 Acl:PACL;
 Sid:PSID;
 SecurityData:PNTFSSecurityData;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;

 if ALocal then
  begin
   Result:=WriteSecurity(ABuffer,AOffset,ASize,AVersion);
  end
 else
  begin
   {Get Data}
   SecurityData:=PNTFSSecurityData(LongWord(ABuffer) + AOffset);
   
   {Check Size}
   if ASize >= ntfsSecuritySize then
    begin
     {Write Data}
     SecurityData.Revision:=FRevision;
     SecurityData.Reserved1:=0;
     SecurityData.Control:=FControl;
     SecurityData.SaclOffset:=SaclOffsetEx(ALocal);
     SecurityData.DaclOffset:=DaclOffsetEx(ALocal);
     SecurityData.OwnerOffset:=OwnerOffsetEx(ALocal);
     SecurityData.GroupOffset:=GroupOffsetEx(ALocal);
     
     {Check Object}
     if FSacl <> nil then
      begin
       {Write Sacl}
       Acl:=PACL(LongWord(ABuffer) + AOffset + SecurityData.SaclOffset);
       System.Move(FSacl^,Acl^,SaclSize);
      end;
     
     {Check Object}
     if FDacl <> nil then
      begin
       {Write Dacl}
       Acl:=PACL(LongWord(ABuffer) + AOffset + SecurityData.DaclOffset);
       System.Move(FDacl^,Acl^,DaclSize);
      end;
     
     {Check Object}
     if FOwner <> nil then
      begin
       {Write Owner}
       Sid:=PSID(LongWord(ABuffer) + AOffset + SecurityData.OwnerOffset);
       System.Move(FOwner^,Sid^,OwnerSize);
      end;
     
     {Check Object}
     if FGroup <> nil then
      begin
       {Write Group}
       Sid:=PSID(LongWord(ABuffer) + AOffset + SecurityData.GroupOffset);
       System.Move(FGroup^,Sid^,GroupSize);
      end;
     
     {Update Offset}
     Dec(ASize,SecuritySize);
     Inc(AOffset,SecuritySize);
     
     Result:=True;
    end;
  end;
end;

{==============================================================================}
{==============================================================================}

end.      