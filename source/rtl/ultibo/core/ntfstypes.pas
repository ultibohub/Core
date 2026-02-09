{
Ultibo NTFS types unit.

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

        Byte counts shown against structures are minimum sizes only
        and most structures are variable in length with size and offset values

        The details in TNTFSFileName (Time/Size/Flags) are only ever updated when
        the file is created or renamed. The up to date details can be found in the
        following attributes:

         Time/Attributes - TNTFSStandardInformation
         Size            - TNTFSData (DataSize - Resident, StreamSize - NonResident)

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

{$IFNDEF FPC_DOTTEDUNITS}
unit NTFSTypes;
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
  Core.UltiboClasses,
  Core.NTFSConst;
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
  UltiboClasses,
  NTFSConst;
{$ENDIF FPC_DOTTEDUNITS}

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
type
 {NTFS specific types}
 TNTFSType = (ntNONE,ntNTFS12,ntNTFS30,ntNTFS31);

 {Record Number Types}
 PNTFSRecordNumber = ^TNTFSRecordNumber;
 TNTFSRecordNumber = packed record   {48 bits}  {Read/Write as Word,LongWord}
  RecordSegment:Word;
  RecordNumber:LongWord;
 end;

 PNTFSFileReference = ^TNTFSFileReference;
 TNTFSFileReference = packed record  {64 bits}  {Read/Write as Int64} {Note endian ordering}
  RecordNumber:LongWord;
  RecordSegment:Word;
  SequenceNumber:Word;
 end;

 {Record Types}
 PNTFSSubNodeRecord = ^TNTFSSubNodeRecord;  {8 bytes}
 TNTFSSubNodeRecord = packed record
  SubNodeNumber:Int64;
 end;

 PNTFSUpdateSequenceRecord = ^TNTFSUpdateSequenceRecord;  {2 bytes}
 TNTFSUpdateSequenceRecord = packed record
  UpdateSequenceNumber:Word;               {Update Sequence Number}
  UpdateSequenceArray:array[0..0] of Word; {Update Sequence Array}
  {Padding}                                {Padding (align to 8 bytes)}
 end;

 PNTFS12FileRecord = ^TNTFS12FileRecord;  {42 bytes}
 TNTFS12FileRecord = packed record {Includes NTFS 3.0}
  MagicNumber:LongWord;                    {Magic number 'FILE'}
  UpdateSequenceOffset:Word;               {Offset to the Update Sequence Record}
  UpdateSequenceLength:Word;               {Size in words of the Update Sequence Record}
  LogFileSequenceNumber:Int64;             {LogFile Sequence Number (LSN)}
  SequenceNumber:Word;                     {Sequence number}
  HardLinkCount:Word;                      {Hard link count}
  AttributeOffset:Word;                    {Offset to the first Attribute}
  RecordFlags:Word;                        {Flags}
  RecordSize:LongWord;                     {Actual size of the FILE record}
  RecordAllocated:LongWord;                {Allocated size of the FILE record}
  BaseReference:Int64;                     {File reference to the base FILE record}
  NextAttributeId:Word;                    {Next Attribute Id}
  {UpdateSequenceRecord}
 end;

 PNTFS31FileRecord = ^TNTFS31FileRecord;  {48 bytes}
 TNTFS31FileRecord = packed record
  MagicNumber:LongWord;                    {Magic number 'FILE'}
  UpdateSequenceOffset:Word;               {Offset to the Update Sequence Record}
  UpdateSequenceLength:Word;               {Size in words of the Update Sequence Record}
  LogFileSequenceNumber:Int64;             {LogFile Sequence Number (LSN)}
  SequenceNumber:Word;                     {Sequence number}
  HardLinkCount:Word;                      {Hard link count}
  AttributeOffset:Word;                    {Offset to the first Attribute}
  RecordFlags:Word;                        {Flags}
  RecordSize:LongWord;                     {Actual size of the FILE record}
  RecordAllocated:LongWord;                {Allocated size of the FILE record}
  BaseReference:Int64;                     {File reference to the base FILE record}
  NextAttributeId:Word;                    {Next Attribute Id}
  RecordSegment:Word;                      {Align to 4 byte boundary} {Segment of this MFT Record}
  RecordNumber:LongWord;                   {Number of this MFT Record}
  {UpdateSequenceRecord}
 end;

 PNTFSRestartRecord = ^TNTFSRestartRecord;  {30 bytes} {http://www.disy.cse.unsw.edu.au/lxr/source/fs/ntfs/logfile.h?v=linux-2.6.32}
 TNTFSRestartRecord = packed record
  MagicNumber:LongWord;                    {Magic number 'RSTR'}
  UpdateSequenceOffset:Word;               {Offset to the Update Sequence Record}
  UpdateSequenceLength:Word;               {Size in words of the Update Sequence Record}
  LastSequenceNumber:Int64;                {Last Log File Sequence Number (LSN) only used by Chkdsk when magic is CHKD otherwise 0}
  SystemPageSize:LongWord;                 {Must be >= 512 and a power of 2 (Normally 4096)}
  LogPageSize:LongWord;                    {Must be >= 512 and a power of 2 (Normally 4096 if the SystemPageSize is 4096 to 8192 otherwise SystemPageSize)}
  RestartAreaOffset:Word;                  {Absolute offset to the start of the restart area. Must be 8 byte aligned (After the Update Sequence Array)}
  MinorVersion:Word;
  MajorVersion:Word;                       {NTFS1.2 is version ?.? / NTFS3.0 is version ?.? / NTFS3.1 is Version 1.1}
  {UpdateSequenceRecord}
 end;

 PNTFSRestartArea = ^TNTFSRestartArea;  {48 Bytes} {http://www.disy.cse.unsw.edu.au/lxr/source/fs/ntfs/logfile.h?v=linux-2.6.32}
 TNTFSRestartArea = packed record
  CurrentSequenceNumber:Int64;             {The Current (Last Written) Log File Sequence Number (LSN) on create this will be 0}
  LogClientCount:Word;                     {Must be 1}
  FirstFreeClient:Word;                    {The index of the first free log record (When clean normally 0 on Win2k or below and 0xFFFF on WinXP or above)}
  FirstUsedClient:Word;                    {The index of the first used log record (When clean normally 0xFFFF on Win2k or below and 0 on WinXP or above)}
  Flags:Word;                              {On Win2k or below always 0, on WinXP or above $0002 if clean}
  SequenceNumberBits:LongWord;             {The number of bits to use for the sequence number (67 - the number of bits to store the logfile size in bytes}
  RestartAreaLength:Word;                  {Length of the restart area and log clients}
  ClientArrayOffset:Word;                  {Offset from the start of the restart area to the first log client}
  FileSize:Int64;                          {Usable byte size of the log file (rounded down to a multiple of LogPageSize, must be large enough to hold 2 restart records and 48 log records)}
  LastSequenceDataLength:LongWord;         {Length of data of last LSN not including log record header (On create this will be 0)}
  LogRecordHeaderLength:Word;              {Byte size of the log record header. Must be a multiple of 8.}
  LogPageDataOffset:Word;                  {Offset to the start of data in a log record. Must be a multiple of 8. (After the Update Sequence Array)}
  LogFileOpenCount:LongWord;               {On create this will be a random value}
  Reserved:LongWord;                       {Alignment to 8 bytes}
 end;

 PNTFSLogClient = ^TNTFSLogClient;  {160 bytes} {http://www.disy.cse.unsw.edu.au/lxr/source/fs/ntfs/logfile.h?v=linux-2.6.32}
 TNTFSLogClient = packed record
  OldestSequenceNumber:Int64;              {Oldest LSN of this log client (On create this will be 0)}
  CurrentSequenceNumber:Int64;             {Current LSN within the log client (On create this will be 0)}
  PrevClient:Word;                         {Offset of the previous log client or 0xFFFF if first (Always 0xFFFF)}
  NextClient:Word;                         {Offset of the next log client or 0xFFFF if last (Always 0xFFFF}
  SequenceNumber:Word;                     {On Win2K or below 0 when dirty and 1 when clean, on WinXP or above always 0}
  Reserved:array[0..5] of Byte;            {Reserved/Alignment}
  ClientNameLength:LongWord;               {Client name length in bytes. Should always be 8}
  ClientName:array[0..63] of WideChar;     {Should always be NTFS in unicode}
 end;

 PNTFSLogRecord = ^TNTFSLogRecord;  {? Bytes}
 TNTFSLogRecord = packed record
  MagicNumber:LongWord;                    {Magic number 'RCRD'}
  UpdateSequenceOffset:Word;               {Offset to the Update Sequence Record}
  UpdateSequenceLength:Word;               {Size in words of the Update Sequence Record}
  LastLSN:Int64;                           {???????} //file_offset:LongWord;
  Flags:LongWord;                          {???????}
  PageCount:Word;                          {???????}
  PagePosition:Word;                       {???????}
  NextRecordOffset:Int64;                  {???????}
  LastEndLSN:Int64;                        {???????}
  {UpdateSequenceRecord}
 end;

 {Index Types}
 PNTFSIndexRecord = ^TNTFSIndexRecord;  {24 bytes}
 TNTFSIndexRecord = packed record
  MagicNumber:LongWord;                    {Magic number 'INDX'}
  UpdateSequenceOffset:Word;               {Offset to the Update Sequence Record}
  UpdateSequenceLength:Word;               {Size in words of the Update Sequence Record}
  LogFileSequenceNumber:Int64;             {LogFile sequence number}
  RecordNumber:Int64;                      {Number of this INDX record in the Index Allocation}
  {IndexHeader}
  {UpdateSequenceRecord}
  {IndexEntry(s)}
 end;

 PNTFSIndexHeader = ^TNTFSIndexHeader;  {16 bytes}
 TNTFSIndexHeader = packed record
  EntryOffset:LongWord;          {Offset to first Index Entry}
  IndexSize:LongWord;            {Total size of the Index Entries}
  IndexAllocated:LongWord;       {Allocated size of the Index Entries}
  IndexFlags:Word;               {Flags} {See Consts}
  Reserved1:Word;                {Padding (Align to 8 bytes)}
 end;

 PNTFSIndexEntry = ^TNTFSIndexEntry;  {16 bytes (Not including Key of SubNodeNumber)}
 TNTFSIndexEntry = packed record
  Reserved1:Int64;             {Padding (Align to 8 bytes)}
  EntrySize:Word;              {Length of the index entry}
  KeySize:Word;                {Length of the key entry}
  EntryFlags:Word;             {Flags} {See Consts}
  Reserved2:Word;              {Padding (Align to 8 bytes)}
  Key:array[0..0] of Byte;     {Key} {Only present when the last entry flag is not set} {Note: No Offset so always in the same location}
  {SubNodeNumber:Int64}        {Number of the sub-node in the index allocation attribute} {Only present when the sub-node flag is set}
 end;

 PNTFSDataIndexEntry = ^TNTFSDataIndexEntry;  {16 bytes (Not including Key, Data or SubNodeNumber)}
 TNTFSDataIndexEntry = packed record
  DataOffset:Word;             {Offset to the data}{Only valid when the last entry flag is not set}
  DataSize:Word;               {Size of the data}  {Only valid when the last entry flag is not set}
  Reserved1:LongWord;          {Padding (Align to 8 bytes)}
  EntrySize:Word;              {Length of the index entry}
  KeySize:Word;                {Length of the key entry}
  EntryFlags:Word;             {Flags} {See Consts}
  Reserved2:Word;              {Padding (Align to 8 bytes)}
  Key:array[0..0] of Byte;     {Key} {Only present when the last entry flag is not set} {Note: No Offset so always in the same location}
  {Data:array[0..0] of Byte}   {Data}{Only present when the last entry flag is not set}
  {SubNodeNumber:Int64}        {Number of the sub-node in the index allocation attribute} {Only present when the sub-node flag is set}
 end;                          {Note: Key data depends on what is indexed (See ObjId/Quota/Owner/SecurityId/SecurityHash/Reparse structures below)}

 PNTFSAttributeIndexEntry = ^TNTFSAttributeIndexEntry;  {16 bytes (Not including Key of SubNodeNumber)}
 TNTFSAttributeIndexEntry = packed record
  FileReference:Int64;         {File reference} {Only valid when the last entry flag is not set}
  EntrySize:Word;              {Length of the index entry}
  KeySize:Word;                {Length of the key entry}
  EntryFlags:Word;             {Flags} {See Consts}
  Reserved1:Word;              {Padding (Align to 8 bytes)}
  Key:array[0..0] of Byte;     {Key} {Only present when the last entry flag is not set} {Note: No Offset so always in the same location}
  {SubNodeNumber:Int64}        {Number of the sub-node in the index allocation attribute} {Only present when the sub-node flag is set}
 end;                          {Note: Key data depends on what is indexed but for an $I30 index it will be a FileName Attribute}

 {Header Types}
 PNTFSAttributeHeader = ^TNTFSAttributeHeader;  {16 bytes}
 TNTFSAttributeHeader = packed record
  AttributeType:LongWord;                 {Attribute Type (e.g. 0x80, 0xA0)}
  AttributeSize:LongWord;                 {Length of the Attribute (including header)}
  NonResident:Byte;                       {Non-resident flag}
  AttributeNameLength:Byte;               {Name length}
  AttributeNameOffset:Word;               {Offset to the Name}
  AttributeFlags:Word;                    {Flags}
  AttributeId:Word;                       {Attribute Id}
 end;

 PNTFSResidentAttributeHeader = ^TNTFSResidentAttributeHeader;  {24 bytes}
 TNTFSResidentAttributeHeader = packed record
  AttributeType:LongWord;                 {Attribute Type (e.g. 0x80, 0xA0)}
  AttributeSize:LongWord;                 {Length of the Attribute (including header)}
  NonResident:Byte;                       {Non-resident flag (Always 0)}
  AttributeNameLength:Byte;               {Name length}
  AttributeNameOffset:Word;               {Offset to the Name}
  AttributeFlags:Word;                    {Flags}
  AttributeId:Word;                       {Attribute Id}
  DataSize:LongWord;                      {Length of the Attribute Data}
  DataOffset:Word;                        {Offset to the Attribute Data}
  Indexed:Byte;                           {Indexed flag}
  Reserved1:Byte;                         {Padding}
  {AttributeName:array[0..0] of WideChar;}{Unicode Attribute Name (if NameLength > 0)}
  {Data:array[0..0] of Byte}              {Attribute Data}
 end;

 PNTFSNonResidentAttributeHeader = ^TNTFSNonResidentAttributeHeader;  {64 bytes}
 TNTFSNonResidentAttributeHeader = packed record
  AttributeType:LongWord;                 {Attribute Type (e.g. 0x80, 0xA0)}
  AttributeSize:LongWord;                 {Length of the Attribute (including header)}
  NonResident:Byte;                       {Non-resident flag (Always 1)}
  AttributeNameLength:Byte;               {Name length}
  AttributeNameOffset:Word;               {Offset to the Name}
  AttributeFlags:Word;                    {Flags}
  AttributeId:Word;                       {Attribute Id}
  StartVCN:Int64;                         {Starting VCN}
  LastVCN:Int64;                          {Last VCN}
  RunOffset:Word;                         {Offset to the Data Run}
  CompressionUnit:Word;                   {Compression Unit Size}
  Reserved1:LongWord;                     {Padding}
  StreamAllocated:Int64;                  {Allocated size of the attribute}
  StreamSize:Int64;                       {Real size of the attribute}
  InitializedSize:Int64;                  {Initialized data size of the stream (Portion which has been Written)}
  {AttributeName:array[0..0] of WideChar;}{Unicode Attribute Name (if NameLength > 0)}
  {Run:array[0..0] of Byte}               {Attribute Data Runs}
 end;

 PNTFSCompressedAttributeHeader = ^TNTFSCompressedAttributeHeader;  {64 bytes}
 TNTFSCompressedAttributeHeader = packed record
  AttributeType:LongWord;                 {Attribute Type (e.g. 0x80, 0xA0)}
  AttributeSize:LongWord;                 {Length of the Attribute (including header)}
  NonResident:Byte;                       {Non-resident flag (Always 1)}
  AttributeNameLength:Byte;               {Name length}
  AttributeNameOffset:Word;               {Offset to the Name}
  AttributeFlags:Word;                    {Flags}
  AttributeId:Word;                       {Attribute Id}
  StartVCN:Int64;                         {Starting VCN}
  LastVCN:Int64;                          {Last VCN}
  RunOffset:Word;                         {Offset to the Data Run}
  CompressionUnit:Word;                   {Compression Unit Size}
  Reserved1:LongWord;                     {Padding}
  StreamAllocated:Int64;                  {Allocated size of the attribute}
  StreamSize:Int64;                       {Real size of the attribute}
  InitializedSize:Int64;                  {Initialized data size of the stream (Portion which has been Written)}
  StreamUsed:Int64;                       {The actual Allocated size of the attribute (Only present when compressed and only in the first instance)}
  {AttributeName:array[0..0] of WideChar;}{Unicode Attribute Name (if NameLength > 0)}
  {Run:array[0..0] of Byte}               {Attribute Data Runs}
 end;

 {Attribute Types}
 PNTFS12StandardInformation = ^TNTFS12StandardInformation;  {48 bytes}
 TNTFS12StandardInformation = packed record {0x10}
  {Standard Attribute Header}
  CreateTime:TFileTime;  {File Creation}
  WriteTime:TFileTime;   {File Altered}
  ChangeTime:TFileTime;  {MFT Changed}
  AccessTime:TFileTime;  {File Read}
  Attributes:LongWord;   {DOS File Permissions}
  MaxVersions:LongWord;  {Maximum Number of Versions}
  VersionNo:LongWord;    {Version Number}
  ClassId:LongWord;      {Class Id}
 end;

 PNTFS30StandardInformation = ^TNTFS30StandardInformation;  {72 bytes}
 TNTFS30StandardInformation = packed record {0x10} {Includes NTFS 3.1}
  {Standard Attribute Header}
  CreateTime:TFileTime;       {File Creation}
  WriteTime:TFileTime;        {File Altered}
  ChangeTime:TFileTime;       {MFT Changed}
  AccessTime:TFileTime;       {File Read}
  Attributes:LongWord;        {DOS File Permissions}
  MaxVersions:LongWord;       {Maximum Number of Versions}
  VersionNo:LongWord;         {Version Number}
  ClassId:LongWord;           {Class Id}
  OwnerId:LongWord;           {Owner Id}
  SecurityId:LongWord;        {Security Id}
  QuotaCharge:Int64;          {Quota Charged}
  UpdateSequenceNumber:Int64; {Update Sequence Number}
 end;

 PNTFSAttributeList = ^TNTFSAttributeList;  {0 bytes}
 TNTFSAttributeList = packed record {0x20}
  {Standard Attribute Header}
  Item:array[0..0] of Byte;   {Attribute List Items}
 end;

 PNTFSFileName = ^TNTFSFileName;  {66 bytes}
 TNTFSFileName = packed record {0x30}
  {Standard Attribute Header}
  ParentReference:Int64;             {File reference to the parent directory.}
  CreateTime:TFileTime;              {C Time - File Creation}                     {Note: These fields are only updated when   }
  WriteTime:TFileTime;               {A Time - File Altered}                      {      the filename is changed. See Standard}
  ChangeTime:TFileTime;              {M Time - MFT Changed}                       {      Information instead                  }
  AccessTime:TFileTime;              {R Time - File Read}
  FileAllocated:Int64;               {Allocated size of the file}
  FileSize:Int64;                    {Real size of the file}
  FileFlags:LongWord;                {Flags, e.g. Directory, Compressed, Hidden}
  ReparseTag:LongWord;               {Used by EAs and Reparse}
  FileNameLength:Byte;               {Filename length in characters}
  NameSpace:Byte;                    {Filename namespace}
  FileName:array[0..0] of WideChar;  {File name in Unicode (not null terminated)} {Note: NameLength but no NameOffset so Name}
 end;

 PNTFSObjectId = ^TNTFSObjectId;  {64 bytes}
 TNTFSObjectId = packed record {0x40}
  {Standard Attribute Header}
  ObjectId:TGUID;           {GUID Object Id Unique Id assigned to file}
  BirthVolumeId:TGUID;      {GUID Birth Volume Id Volume where file was created}
  BirthObjectId:TGUID;      {GUID Birth Object Id Original Object Id of file}
  DomainId:TGUID;           {GUID Domain Id Domain in which object was created}
 end;

 PNTFSVolumeVersion = ^TNTFSVolumeVersion;  {0 bytes}
 TNTFSVolumeVersion = packed record {0x40}
  {Standard Attribute Header}
  Data:array[0..0] of Byte; {Unknown data} {Structure to be determined}
 end;

 PNTFSSecurityDescriptor = ^TNTFSSecurityDescriptor;  {0 bytes}
 TNTFSSecurityDescriptor = packed record {0x50}
  {Standard Attribute Header}
  Security:array[0..0] of Byte; {Security Descriptor}
 end;

 PNTFSVolumeName = ^TNTFSVolumeName;  {0 bytes}
 TNTFSVolumeName = packed record {0x60}
  {Standard Attribute Header}
  VolumeName:array[0..0] of WideChar; {Unicode name} {Note: The DataOffset and DataSize in the  }
 end;

 PNTFSVolumeInformation = ^TNTFSVolumeInformation;  {16 bytes}
 TNTFSVolumeInformation = packed record {0x70}
  {Standard Attribute Header}
  Reserved1:Int64;      {Always zero ?}
  MajorVersion:Byte;    {Major version number}
  MinorVersion:Byte;    {Minor version number}
  VolumeFlags:Word;     {Flags}
  {Reserved2:LongWord;} {Always zero ? (Padding only, not part of data)}
 end;

 PNTFSData = ^TNTFSData;  {0 bytes}
 TNTFSData = packed record {0x80}
  {Standard Attribute Header}
  Data:array[0..0] of Byte; {Any data}              {Note: The DataOffset and DataSize in the  }
 end;                                               {      header give the pointer to the Data }

 PNTFSIndexRoot = ^TNTFSIndexRoot;  {16 bytes}
 TNTFSIndexRoot = packed record {0x90}
  {Standard Attribute Header}
  IndexType:LongWord;             {Attribute Type}
  CollateRule:LongWord;           {Collation Rule}
  IndexRecordSize:LongWord;       {Size of Index Allocation Entry (bytes)}
  IndexCounterOffset:LongWord;    {Index Record Number increment}
  {IndexHeader}
  {IndexEntry(s)}
 end;

 PNTFSIndexAllocation = ^TNTFSIndexAllocation;  {0 bytes}
 TNTFSIndexAllocation = packed record {0xA0}
  {Standard Attribute Header}
  Run:array[0..0] of Byte;    {Data run}          {Note: The RunOffset and AttributeSize in the }
 end;                                             {      header give the pointer to the Run     }

 PNTFSBitmap = ^TNTFSBitmap;  {0 bytes}
 TNTFSBitmap = packed record {0xB0}
  {Standard Attribute Header}
  Bitmap:array[0..0] of Int64; {Bit field}          {Note: The DataOffset and DataSize in the    } {Bit ordering works as an array of bytes}
 end;                                               {      header give the pointer to the Bitmap }

 PNTFSReparsePoint = ^TNTFSReparsePoint;  {8 bytes}
 TNTFSReparsePoint = packed record {0xC0}
  {Standard Attribute Header}
  ReparseTag:LongWord;              {Reparse Type (and Flags)}
  ReparseSize:Word;                 {Reparse Data Length}
  Reserved1:Word;                   {Padding (align to 8 bytes)}
  ReparseData:array[0..0] of Byte;  {Reparse Data}
 end;

 PNTFSReparsePointMicrosoft = ^TNTFSReparsePointMicrosoft;  {8 bytes}
 TNTFSReparsePointMicrosoft = TNTFSReparsePoint; {0xC0}

 PNTFSReparsePointOther = ^TNTFSReparsePointOther;  {24 bytes}
 TNTFSReparsePointOther = packed record {0xC0}
  {Standard Attribute Header}
  ReparseTag:LongWord;              {Reparse Type (and Flags)}
  ReparseSize:Word;                 {Reparse Data Length}
  Reserved1:Word;                   {Padding (align to 8 bytes)}
  ReparseGUID:TGUID;                {Reparse GUID}
  ReparseData:array[0..0] of Byte;  {Reparse Data}
 end;

 PNTFSSymbolicLink = ^TNTFSSymbolicLink;  {0 bytes}
 TNTFSSymbolicLink = packed record {0xC0}
  {Standard Attribute Header}
  Data:array[0..0] of Byte; {Unknown data} {Structure to be determined}
 end;

 PNTFSExtendedAttrInformation = ^TNTFSExtendedAttrInformation;  {8 bytes}
 TNTFSExtendedAttrInformation = packed record {0xD0}
  {Standard Attribute Header}
  PackedSize:Word;            {Size of the packed Extended Attributes}
  FlagCount:Word;             {Number of Extended Attributes which have NEED_EA flag}
  UnpackedSize:LongWord;      {Size of the unpacked Extended Attributes}
 end;

 PNTFSExtendedAttr = ^TNTFSExtendedAttr;  {0 bytes}
 TNTFSExtendedAttr = packed record {0xE0}
  {Standard Attribute Header}
  Extended:array[0..0] of Byte; {Extended Attributes}
 end;

 PNTFSPropertySet = ^TNTFSPropertySet;  {0 bytes}
 TNTFSPropertySet = packed record {0xF0}
  {Standard Attribute Header}
  Data:array[0..0] of Byte; {Unknown data} {Structure to be determined}
 end;

 PNTFSLoggedUtilityStream = ^TNTFSLoggedUtilityStream;  {0 bytes}
 TNTFSLoggedUtilityStream = packed record {0x100}
  {Standard Attribute Header}
  Data:array[0..0] of Byte; {Any data}         {Note: The DataOffset and DataSize in the  }
 end;                                          {      header give the pointer to the Data }

 PNTFSUnknown = ^TNTFSUnknown;  {0 bytes}
 TNTFSUnknown = packed record {0x100}
  {Standard Attribute Header}
  Data:array[0..0] of Byte; {Any data}         {Note: The DataOffset and DataSize in the  }
 end;                                          {      header give the pointer to the Data }

 PNTFSEnd = ^TNTFSEnd;  {0 bytes}
 TNTFSEnd = packed record {0xFFFFFFFF}
  {Standard Attribute Header}
 end;

 {Data Types}
 PNTFSRunData = ^TNTFSRunData;  {0 bytes}
 TNTFSRunData = packed record
  Run:array[0..0] of Byte;  {Data Run}         {Note: The RunOffset and AttributeSize in the }
 end;                                          {      header give the pointer to the Run     }

 PNTFSItemData = ^TNTFSItemData;  {26 bytes}         {Data of attribute $ATTRIBUTE_LIST}
 TNTFSItemData = packed record
  AttributeType:LongWord;                 {Type}
  ItemSize:Word;                          {Record length}
  AttributeNameLength:Byte;               {Name length}
  AttributeNameOffset:Byte;               {Offset to Name}
  StartVCN:Int64;                         {Starting VCN}
  FileReference:Int64;                    {File Reference of the attribute}
  AttributeId:Word;                       {Attribute Id}
  {AttributeName:array[0..0] of WideChar;}{Name in Unicode (if NameLength > 0)}
 end;

 PNTFSObjIdData = ^TNTFSObjIdData; {56 bytes}        {Data of index $O in file $ObjId} {Key is ObjectId}
 TNTFSObjIdData = packed record
  FileReference:Int64;    {MFT Reference}
  BirthVolumeId:TGUID;    {GUID Birth Volume Id} {This is the ObjectId from the file $Volume}
  BirthObjectId:TGUID;    {GUID Birth Object Id}
  DomainId:TGUID;         {GUID Domain Id}
 end;

 PNTFSQuotaData = ^TNTFSQuotaData; {48 bytes (Not including SID)}  {Data of index $Q in file $Quota} {Key is OwnerId}
 TNTFSQuotaData = packed record
  Version:LongWord;             {Version (0x02)}
  Flags:LongWord;               {Flags}
  BytesUsed:Int64;              {Bytes Used}
  ChangeTime:TFileTime;         {Change Time}
  WarningLimit:Int64;           {Warning Limit}
  HardLimit:Int64;              {Hard Limit}
  ExceedTime:TFileTime;         {Exceeded Time}
  SID:array[0..0] of Byte;      {SID}
  {Padding:array[0..0] of Byte} {Padding (align to 8 bytes)} {Handled by Calculation of Index Entry Size}
 end;

 PNTFSOwnerData = ^TNTFSOwnerData; {4 bytes}         {Data of index $O in file $Quota} {Key is SID}
 TNTFSOwnerData = packed record
  OwnerId:LongWord;             {Owner Id}
  {Padding:array[0..0] of Byte} {Padding (align to 8 bytes)} {Handled by Calculation of Index Entry Size}
 end;

 PNTFSUpCaseData = ^TNTFSUpCaseData;  {131072 bytes} {Data of file $UpCase}
 TNTFSUpCaseData = packed record
  Data:array[0..65535] of Word;           {Conversion data}
 end;

 PNTFSAttrDefData = ^TNTFSAttrDefData;  {160 bytes}  {Data of file $AttrDef}
 TNTFSAttrDefData = packed record
  AttributeName:array[0..63] of WideChar;  {Label in Unicode}
  AttributeType:LongWord;                  {Type}
  DisplayRule:LongWord;                    {Display rule}
  CollateRule:LongWord;                    {Collation rule}
  AttrDefFlags:LongWord;                   {Flags} {See Consts}
  MinimumSize:Int64;                       {Minimum size}
  MaximumSize:Int64;                       {Maximum size}
 end;

 PNTFSReparseData = ^TNTFSReparseData;  {0 bytes (Not including Data)}    {Data of attribute $REPARSE_POINT}
 TNTFSReparseData = packed record
  Data:array[0..0] of Byte;            {Any data}
 end;

 PNTFSReparseSymLinkData = ^TNTFSReparseSymLinkData;  {12 bytes (Not including Names)}    {Data of attribute $REPARSE_POINT}
 TNTFSReparseSymLinkData = packed record
  SubstituteNameOffset:Word;           {Substitute Name Offset}
  SubstituteNameLength:Word;           {Substitute Name Length} {Length is in bytes not characters}
  PrintNameOffset:Word;                {Print Name Offset}
  PrintNameLength:Word;                {Print Name Length}      {Length is in bytes not characters}
  Reserved1:LongWord;                  {Always Zero}
  {PrintName:array[0..0] of Byte}      {Path Buffer}            {Not Null terminated}
  {SubstituteName:array[0..0] of Byte} {Path Buffer}            {Not Null terminated}
 end;

 PNTFSReparseMountPointData = ^TNTFSReparseMountPointData;  {12 bytes (Minimum 8 bytes plus 2 unicode nulls) (Not including Names)} {Data of attribute $REPARSE_POINT}
 TNTFSReparseMountPointData = packed record
  SubstituteNameOffset:Word;           {Substitute Name Offset} {Offset includes the null terminator}
  SubstituteNameLength:Word;           {Substitute Name Length} {Length is in bytes not characters and does not include the null terminator}
  PrintNameOffset:Word;                {Print Name Offset}      {Offset includes the null terminator}
  PrintNameLength:Word;                {Print Name Length}      {Length is in bytes not characters and does not include the null terminator}
  {SubstituteName:array[0..0] of Byte} {Path Buffer}            {Null terminated}
  {PrintName:array[0..0] of Byte}      {Path Buffer}            {Null terminated}
 end;


 PNTFSExtendedData = ^TNTFSExtendedData;  {8 bytes (Not including Name}  {Data of attribute $EA}
 TNTFSExtendedData = packed record
  ExtendedOffset:LongWord;             {Offset to next Extended Attribute} {Offset to next EA is the size of this EA}
  ExtendedFlags:Byte;                  {Flags}
  ExtendedNameLength:Byte;             {Name Length (N)}
  ExtendedDataSize:Word;               {Value Length (V)}
  ExtendedName:array[0..0] of Char;    {Name} {Note: No offset so always in the same location}
  {ExtendedData:array[0..0] of Byte}   {Value}
 end;

 PNTFSSecurityData = ^TNTFSSecurityData;  {20 bytes (Minimum)} {Data of attribute $SECURITY_DESCRIPTOR and stream $SDS in file $Secure}
 TNTFSSecurityData = packed record
  Revision:Byte;            {Revision}
  Reserved1:Byte;           {Padding}
  Control:Word;             {Control Flags}
  OwnerOffset:LongWord;     {Offset to Owner SID}
  GroupOffset:LongWord;     {Offset to Group SID}
  SaclOffset:LongWord;      {Offset to SACL}
  DaclOffset:LongWord;      {Offset to DACL}
 end;

 PNTFSSidIdentifierAuthorityData = ^TNTFSSidIdentifierAuthorityData; {6 Bytes}
 TNTFSSidIdentifierAuthorityData = packed record
  Value:array[0..5] of Byte;
 end;

 PNTFSSidData = ^TNTFSSidData; {8 Bytes (Minimum) (Not including SubAuthority)}
 TNTFSSidData = packed record
  Revision:Byte;
  SubAuthorityCount:Byte;
  IdentifierAuthority:array[0..5] of Byte;
  SubAuthority:array[0..0] of LongWord; {Not included in size}
 end;

 PNTFSAclData = ^TNTFSAclData; {8 Bytes}
 TNTFSAclData = packed record
  AclRevision:Byte;
  Reserved1:Byte;           {Must be Zero}
  AclSize:Word;
  AceCount:Word;
  Reserved2:Word;           {Must be Zero}
 end;

 PNTFSAceHeaderData = ^TNTFSAceHeaderData; {4 Bytes}
 TNTFSAceHeaderData = packed record
  AceType:Byte;
  AceFlags:Byte;
  AceSize:Word;
 end;

 PNTFSAceData = ^TNTFSAceData; {8 Bytes (Minimum) (Not including Sid)}
 TNTFSAceData = packed record
  Header:TNTFSAceHeaderData;
  Mask:LongWord;
  Sid:array[0..0] of Byte; {Not included in size}
 end;

 PNTFSObjectAceData = ^TNTFSObjectAceData; {44 Bytes (Minimum)  (Not including Sid)}
 TNTFSObjectAceData = packed record
  Header:TNTFSAceHeaderData;
  Mask:LongWord;
  Flags:LongWord;
  ObjectType:TGUID;
  InheritedObjectType:TGUID;
  Sid:array[0..0] of Byte; {Not included in size}
 end;

 PNTFSSecurityItemData = ^TNTFSSecurityItemData; {40 Bytes (Minimum) (Including Security)}  {Data of stream $SDS in file $Secure}
 TNTFSSecurityItemData = packed record
  SecurityHash:LongWord;        {Hash of Security Descriptor}
  SecurityId:LongWord;          {Security Id}
  SecurityOffset:Int64;         {Offset of this entry in $SDS}
  SecuritySize:LongWord;        {Size of this entry in $SDS}
  Security:TNTFSSecurityData;   {Self-relative Security Descriptor}
  {Padding:array[0..0] of Byte} {Padding (align to 16 bytes)} {Handled by Calculation of Stream Size}
 end;

 PNTFSSecurityIdData = ^TNTFSSecurityIdData; {20 Bytes}      {Data of index $SII in file $Secure} {Key is SecurityId}
 TNTFSSecurityIdData = packed record
  SecurityHash:LongWord;        {Hash of Security Descriptor}
  SecurityId:LongWord;          {Security Id}
  SecurityOffset:Int64;         {Offset of this entry in $SDS}
  SecuritySize:LongWord;        {Size of this entry in $SDS}
 end;

 PNTFSSecurityHashData = ^TNTFSSecurityHashData; {20 Bytes}  {Data of index $SDH in file $Secure}
 TNTFSSecurityHashData = packed record
  SecurityHash:LongWord;        {Hash of Security Descriptor}
  SecurityId:LongWord;          {Security Id}
  SecurityOffset:Int64;         {Offset of this entry in $SDS}
  SecuritySize:LongWord;        {Size of this entry in $SDS}
  {Padding:LongWord;}           {Padding (align to 8 bytes)}{Always 4 bytes and always appears to be the Unicode string "II"}
 end;

 {Key Types}
 PNTFSObjIdKeyData = ^TNTFSObjIdKeyData; {16 Bytes}              {Key of index $O in file $ObjId}
 TNTFSObjIdKeyData = packed record
  ObjectId:TGUID;               {GUID Object Id}
  {Data:array[0..0] of Byte}    {Data (see above)}
 end;

 PNTFSQuotaKeyData = ^TNTFSQuotaKeyData; {4 Bytes}               {Key of index $Q in file $Quota}
 TNTFSQuotaKeyData = packed record
  OwnerId:LongWord;             {Owner Id}
  {Data:array[0..0] of Byte}    {Data (see above)}
  {Padding:array[0..0] of Byte} {Padding (align to 8 bytes)} {Handled by Calculation of Index Entry Size}
 end;

 PNTFSOwnerKeyData = ^TNTFSOwnerKeyData; {0 Bytes (Not including SID)}  {Key of index $O in file $Quota}
 TNTFSOwnerKeyData = packed record
  SID:array[0..0] of Byte;      {SID}
  {Data:array[0..0] of Byte}    {Data (see above)}
  {Padding:array[0..0] of Byte} {Padding (align to 8 bytes)} {Handled by Calculation of Index Entry Size}
 end;

 PNTFSSecurityIdKeyData = ^TNTFSSecurityIdKeyData; {4 Bytes}     {Key of index $SII in file $Secure}
 TNTFSSecurityIdKeyData = packed record
  SecurityId:LongWord;          {Security Id}
  {Data:array[0..0] of Byte}    {Data (see above)}
 end;

 PNTFSSecurityHashKeyData = ^TNTFSSecurityHashKeyData; {8 Bytes} {Key of index $SDH in file $Secure}
 TNTFSSecurityHashKeyData = packed record
  SecurityHash:LongWord;        {Hash of Security Descriptor}
  SecurityId:LongWord;          {Security Id}
  {Data:array[0..0] of Byte}     {Data (see above)}
  {Padding:array[0..0] of Byte} {Padding (align to 8 bytes)} {Handled by Calculation of Index Entry Size}
 end;

 PNTFSReparseKeyData = ^TNTFSReparseKeyData; {12 Bytes} {Key of index $R in file $Reparse}
 TNTFSReparseKeyData = packed record                    {This index contains no data}
  ReparseTag:LongWord;      {Reparse Tag (and Flags)}
  FileReference:Int64;      {MFT Reference of Reparse Point}
  {Padding:LongWord;}       {Padding (align to 8 bytes)} {Handled by Calculation of Index Entry Size}
 end;

 {Run Types}
 PNTFSRunOffset = ^TNTFSRunOffset; {8 Bytes}
 TNTFSRunOffset = packed record
  case Integer of
   1:(Offset1:ShortInt);
   2:(Offset2:SmallInt);
   4:(Offset4:LongInt);
   8:(Offset8:Int64);
 end;

 PNTFSRunLength = ^TNTFSRunLength; {8 Bytes}
 TNTFSRunLength = packed record
  case Integer of
   1:(Length1:Byte);
   2:(Length2:Word);
   4:(Length4:LongWord);
   8:(Length8:Int64);
 end;

 {Compression Types}
  {Nothing}

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}

end.
