{
Ultibo SCSI interface unit.

Copyright (C) 2022 - SoftOz Pty Ltd.

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


SCSI Devices
============

The SCSI interface unit is device non specific in that it implements only the SCSI protocol.
Actual command transfer is performed by a device specific driver that implements the exact
semantics of the device. eg USB Mass Storage devices use the SCSI protocol, the SCSI interface
implements the protocol behaviour but the actual command transfer is handled by the USB storage driver.

//To Do //No, More work required on the exact behaviour of this overall.
//For now the USB Storage driver implements a Storage device interface and utilizes the SCSI unit for
//protocol definitions and structures etc

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit SCSI;

interface

uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Devices,
  Storage,
  SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {SCSI specific constants}
 SCSI_NAME_PREFIX = 'SCSI';     {Name prefix for SCSI Devices}
 SCSI_HOST_PREFIX = 'SCSIHost'; {Name prefix for Host Devices}

 {SCSI Device Types}
 SCSI_TYPE_NONE     = 0;
 SCSI_TYPE_HDD      = 1;
 //To Do

 {SCSI Device States}
 SCSI_STATE_EJECTED  = 0;
 SCSI_STATE_INSERTED = 1;

 {SCSI Device Flags}
 SCSI_FLAG_NONE       = $00000000;

 {SCSI Device Control Codes}
 //To Do //Stats/Info/Reset/Disable/Eject etc

 {SCSI Protocol Messages}
 SCSI_MESSAGE_COMPLETE    = $00;
 SCSI_MESSAGE_EXTENDED    = $01;
 SCSI_MESSAGE_SAVE_DP     = $02;
 SCSI_MESSAGE_RESTORE_DP  = $03;
 SCSI_MESSAGE_DISCONNECT  = $04;
 SCSI_MESSAGE_ID_ERROR    = $05;
 SCSI_MESSAGE_ABORT       = $06;
 SCSI_MESSAGE_REJECT      = $07;
 SCSI_MESSAGE_NOOP        = $08;
 SCSI_MESSAGE_PARITY      = $09;
 SCSI_MESSAGE_LCOMPLETE   = $0a;
 SCSI_MESSAGE_FCOMPLETE   = $0b;
 SCSI_MESSAGE_RESET       = $0c;
 SCSI_MESSAGE_ABORT_TAG   = $0d;
 SCSI_MESSAGE_CLEAR_QUEUE = $0e;
 SCSI_MESSAGE_INIT_REC    = $0f;
 SCSI_MESSAGE_REL_REC     = $10;
 SCSI_MESSAGE_TERMINATE   = $11;
 SCSI_MESSAGE_SIMPLE_TAG  = $20;
 SCSI_MESSAGE_HEAD_TAG    = $21;
 SCSI_MESSAGE_ORDERED_TAG = $22;
 SCSI_MESSAGE_IGN_RESIDUE = $23;
 SCSI_MESSAGE_IDENTIFY    = $80;

 SCSI_MESSAGE_X_MODIFY_DP = $00;
 SCSI_MESSAGE_X_SYNC_REQ  = $01;
 SCSI_MESSAGE_X_WIDE_REQ  = $03;
 SCSI_MESSAGE_X_PPR_REQ   = $04;

 {SCSI Protocol Status}
 SCSI_STATUS_GOOD         = $00;
 SCSI_STATUS_CHECK_COND   = $02;
 SCSI_STATUS_COND_MET     = $04;
 SCSI_STATUS_BUSY         = $08;
 SCSI_STATUS_INT          = $10;
 SCSI_STATUS_INT_COND_MET = $14;
 SCSI_STATUS_CONFLICT     = $18;
 SCSI_STATUS_TERMINATED   = $20;
 SCSI_STATUS_QUEUE_FULL   = $28;
 SCSI_STATUS_ILLEGAL      = $ff;
 SCSI_STATUS_SENSE        = $80;

 {SCSI Protocol Sense Keys}
 SCSI_SENSE_NO_SENSE         = $00;
 SCSI_SENSE_RECOVERED_ERROR  = $01;
 SCSI_SENSE_NOT_READY        = $02;
 SCSI_SENSE_MEDIUM_ERROR     = $03;
 SCSI_SENSE_HARDWARE_ERROR   = $04;
 SCSI_SENSE_ILLEGAL_REQUEST  = $05;
 SCSI_SENSE_UNIT_ATTENTION   = $06;
 SCSI_SENSE_DATA_PROTECT     = $07;
 SCSI_SENSE_BLANK_CHECK      = $08;
 SCSI_SENSE_VENDOR_SPECIFIC  = $09;
 SCSI_SENSE_COPY_ABORTED     = $0A;
 SCSI_SENSE_ABORTED_COMMAND  = $0B;
 SCSI_SENSE_VOLUME_OVERFLOW  = $0D;
 SCSI_SENSE_MISCOMPARE       = $0E;

 {SCSI Protocol Additional Sense Codes}
 SCSI_ASC_NO_SENSE                      = $00; {NO SENSE}
 SCSI_ASC_RECOVERED_RETRIES             = $17; {RECOVERED DATA WITH RETRIES}
 SCSI_ASC_RECOVERED_ECC                 = $18; {RECOVERED DATA WITH ECC}
 SCSI_ASC_NOT_READY_LUN                 = $04; {LOGICAL UNIT NOT READY}
 SCSI_ASC_NOT_READY_NO_REF_POS          = $06; {NO REFERENCE POSITION FOUND}
 SCSI_ASC_NOT_READY_LUN_COMM            = $08; {LOGICAL UNIT COMMUNICATION}
 SCSI_ASC_NOT_READY_MEDIUM_NOT_PRESENT  = $3A; {MEDIUM NOT PRESENT}
 //SCSI_ASC_
 //SCSI_ASC_
 //SCSI_ASC_
 //SCSI_ASC_
 //SCSI_ASC_
 //SCSI_ASC_
 //SCSI_ASC_
 //To Do //See Table 51 in Mass Storage UFI Specification 1.0 (or other sources)
 //Possibly make this a matrix or a function that returns a single error code decoded from Sense/ASC/ASCQ

 {SCSI Protocol Additional Sense Code Qualifiers}
 SCSI_ASCQ_NO_SENSE                      = $00; {NO SENSE}
 SCSI_ASCQ_RECOVERED_RETRIES             = $01; {RECOVERED DATA WITH RETRIES}
 SCSI_ASCQ_RECOVERED_ECC                 = $00; {RECOVERED DATA WITH ECC}
 SCSI_ASCQ_NOT_READY_LUN_BECOME_READY    = $01; {LOGICAL DRIVE NOT READY - BECOMING READY}
 SCSI_ASCQ_NOT_READY_LUN_INIT_REQUIRED   = $02; {LOGICAL DRIVE NOT READY - INITIALIZATION REQUIRED}
 SCSI_ASCQ_NOT_READY_LUN_FORMAT_PROGRESS = $04; {LOGICAL UNIT NOT READY - FORMAT IN PROGRESS}
 SCSI_ASCQ_NOT_READY_LUN_DEVICE_BUSY     = $FF; {LOGICAL DRIVE NOT READY - DEVICE IS BUSY}
 SCSI_ASCQ_NOT_READY_LUN_COMM_FAILURE    = $00; {LOGICAL UNIT COMMUNICATION FAILURE}
 SCSI_ASCQ_NOT_READY_LUN_COMM_TIMEOUT    = $01; {LOGICAL UNIT COMMUNICATION TIME-OUT}
 SCSI_ASCQ_NOT_READY_LUN_COMM_OVERRUN    = $80; {LOGICAL UNIT COMMUNICATION OVERRUN}
 //SCSI_ASCQ_
 //SCSI_ASCQ_
 //SCSI_ASCQ_
 //SCSI_ASCQ_
 //SCSI_ASCQ_
 //To Do //See Table 51 in Mass Storage UFI Specification 1.0 (or other sources)
 //Possibly make this a matrix or a function that returns a single error code decoded from Sense/ASC/ASCQ

 {SCSI Protocol Commands}
 SCSI_COMMAND_CHANGE_DEF = $40; {Change Definition (Optional)}
 SCSI_COMMAND_COMPARE    = $39; {Compare (Optional)}
 SCSI_COMMAND_COPY       = $18; {Copy (Optional)}
 SCSI_COMMAND_COP_VERIFY = $3A; {Copy and Verify (Optional)}
 SCSI_COMMAND_INQUIRY    = $12; {Inquiry (MANDATORY)}
 SCSI_COMMAND_LOG_SELECT = $4C; {Log Select (Optional)}
 SCSI_COMMAND_LOG_SENSE  = $4D; {Log Sense (Optional)}
 SCSI_COMMAND_MODE_SEL6  = $15; {Mode Select 6-byte (Device Specific)}
 SCSI_COMMAND_MODE_SEL10 = $55; {Mode Select 10-byte (Device Specific)}
 SCSI_COMMAND_MODE_SEN6  = $1A; {Mode Sense 6-byte (Device Specific)}
 SCSI_COMMAND_MODE_SEN10 = $5A; {Mode Sense 10-byte (Device Specific)}
 SCSI_COMMAND_READ_BUFF  = $3C; {Read Buffer (Optional)}
 SCSI_COMMAND_REQ_SENSE  = $03; {Request Sense (MANDATORY)}
 SCSI_COMMAND_SEND_DIAG  = $1D; {Send Diagnostic (Optional)}
 SCSI_COMMAND_TST_U_RDY  = $00; {Test Unit Ready (MANDATORY)}
 SCSI_COMMAND_WRITE_BUFF = $3B; {Write Buffer (Optional)}
 {Commands Unique to Direct Access Devices}
 {SCSI_COMMAND_COMPARE   = $39;} {Compare (Optional)}
 SCSI_COMMAND_FORMAT     = $04; {Format Unit (MANDATORY)}
 SCSI_COMMAND_LCK_UN_CAC = $36; {Lock Unlock Cache (Optional)}
 SCSI_COMMAND_PREFETCH   = $34; {Prefetch (Optional)}
 SCSI_COMMAND_MED_REMOVL = $1E; {Prevent/Allow medium Removal (Optional)}
 SCSI_COMMAND_READ6      = $08; {Read 6-byte (MANDATORY)}
 SCSI_COMMAND_READ10     = $28; {Read 10-byte (MANDATORY)}
 SCSI_COMMAND_READ12     = $A8; {Read 12-byte (Optional)}
 SCSI_COMMAND_READ16     = $88; {Read 16-byte (Optional)}
 SCSI_COMMAND_RD_CAPAC   = $25; {Read Capacity (MANDATORY)}
 SCSI_COMMAND_RD_CAPAC10 = SCSI_COMMAND_RD_CAPAC; {Read Capacity (10)}
 SCSI_COMMAND_RD_DEFECT  = $37; {Read Defect Data (Optional)}
 SCSI_COMMAND_READ_LONG  = $3E; {Read Long (Optional)}
 SCSI_COMMAND_REASS_BLK  = $07; {Reassign Blocks (Optional)}
 SCSI_COMMAND_RCV_DIAG   = $1C; {Receive Diagnostic Results (Optional)}
 SCSI_COMMAND_RELEASE    = $17; {Release Unit (MANDATORY)}
 SCSI_COMMAND_REZERO     = $01; {Rezero Unit (Optional)}
 SCSI_COMMAND_SRCH_DAT_E = $31; {Search Data Equal (Optional)}
 SCSI_COMMAND_SRCH_DAT_H = $30; {Search Data High (Optional)}
 SCSI_COMMAND_SRCH_DAT_L = $32; {Search Data Low (Optional)}
 SCSI_COMMAND_SEEK6      = $0B; {Seek 6-Byte (Optional)}
 SCSI_COMMAND_SEEK10     = $2B; {Seek 10-Byte (Optional)}
 {SCSI_COMMAND_SEND_DIAG = $1D;} {Send Diagnostics (MANDATORY)}
 SCSI_COMMAND_SET_LIMIT  = $33; {Set Limits (Optional)}
 SCSI_COMMAND_START_STP  = $1B; {Start/Stop Unit (Optional)}
 SCSI_COMMAND_SYNC_CACHE = $35; {Synchronize Cache (Optional)}
 SCSI_COMMAND_VERIFY     = $2F; {Verify (Optional)}
 SCSI_COMMAND_WRITE6     = $0A; {Write 6-Byte (MANDATORY)}
 SCSI_COMMAND_WRITE10    = $2A; {Write 10-Byte (MANDATORY)}
 SCSI_COMMAND_WRITE12    = $AA; {Write 12-Byte (Optional)}
 SCSI_COMMAND_WRITE16    = $8A; {Write 16-Byte (Optional)}
 SCSI_COMMAND_WRT_VERIFY = $2E; {Write and Verify (Optional)}
 SCSI_COMMAND_WRITE_LONG = $3F; {Write Long (Optional)}
 SCSI_COMMAND_WRITE_SAME = $41; {Write Same (Optional)}
 SCSI_COMMAND_RD_FMT_CAP = $23; {Read Format Capacities}
 SCSI_COMMAND_SVC_ACT_IN = $9E; {Service Action In}

 {SCSI Command Data}
 SCSI_COMMAND_MAX_SIZE = 16;

 {SCSI Service Actions}
 SCSI_SAI_READ_CAPACITY_16 = $10; {Read Capacity (16)}
 SCSI_SAI_GET_LBA_STATUS   = $12; {Get LBA Status}

 {SCSI Inquiry Data}
 SCSI_INQUIRY_STANDARD = $00;

 {SCSI Standard Inquiry Data}
 SCSI_STANDARD_INQUIRY_SIZE = 36;

 {SCSI Inquiry Peripheral Device Types}
 SCSI_DEVICE_TYPE_DISK       = $00; {SBC-3 - Direct access block device (e.g., magnetic disk)}
 SCSI_DEVICE_TYPE_TAPE       = $01; {SSC-3 - Sequential-access device (e.g., magnetic tape)}
 SCSI_DEVICE_TYPE_PRINTER    = $02; {SSC - Printer device}
 SCSI_DEVICE_TYPE_PROCESSOR  = $03; {SPC-2 - Processor device}
 SCSI_DEVICE_TYPE_WRITE_ONCE = $04; {SBC - Write-once device (e.g., some optical disks)}
 SCSI_DEVICE_TYPE_CD_DVD     = $05; {MMC-5 - CD/DVD device}
 SCSI_DEVICE_TYPE_SCANNER    = $06; {Scanner device (obsolete)}
 SCSI_DEVICE_TYPE_OPTICAL    = $07; {SBC - Optical memory device (e.g., some optical disks)}
 SCSI_DEVICE_TYPE_CHANGER    = $08; {SMC-3 - Medium changer device (e.g., jukeboxes)}
 SCSI_DEVICE_TYPE_COMMS      = $09; {Communications device (obsolete)}
 SCSI_DEVICE_TYPE_ARRAY      = $0C; {SCC-2 - Storage array controller device (e.g., RAID)}
 SCSI_DEVICE_TYPE_ENCLOSURE  = $0D; {SES - Enclosure services device}
 SCSI_DEVICE_TYPE_RBC        = $0E; {RBC - Simplified direct-access device (e.g., magnetic disk)}
 SCSI_DEVICE_TYPE_CARD       = $0F; {OCRW - Optical card reader/writer device}
 SCSI_DEVICE_TYPE_BRIDGE     = $10; {BCC - Bridge Controller Commands}
 SCSI_DEVICE_TYPE_OBJECT     = $11; {OSD - Object-based Storage Device}
 SCSI_DEVICE_TYPE_AUTOMATION = $12; {ADC-2 - Automation/Drive Interface}
 SCSI_DEVICE_TYPE_WELL_KNOWN = $1E; {Well known logical unit}
 SCSI_DEVICE_TYPE_UNKNOWN    = $1F; {Unknown or no device type}

 {SCSI Inquiry Removable Media Bit}
 SCSI_REMOVABLE_MEDIA_BIT    = $80;

 {SCSI Request Sense Data}
 SCSI_REQUEST_SENSE_SIZE = 18;

 {SCSI Read Capacity Data}
 SCSI_READ_CAPACITY_SIZE    = 8;
 SCSI_READ_CAPACITY_16_SIZE = 32;

 {SCSI Read 10 Data}
 SCSI_READ_10_MAX_BLOCKS = $FFFF;

 {SCSI Write 10 Data}
 SCSI_WRITE_10_MAX_BLOCKS = $FFFF;

 {SCSI logging}
 SCSI_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {SCSI debugging messages}
 SCSI_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {SCSI informational messages, such as a device being attached or detached}
 SCSI_LOG_LEVEL_WARN      = LOG_LEVEL_WARN;   {SCSI warning messages}
 SCSI_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {SCSI error messages}
 SCSI_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No SCSI messages}

var
 SCSI_DEFAULT_LOG_LEVEL:LongWord = SCSI_LOG_LEVEL_DEBUG; {Minimum level for SCSI messages.  Only messages with level greater than or equal to this will be printed}

var
 {SCSI logging}
 SCSI_LOG_ENABLED:Boolean;

{==============================================================================}
type
 {SCSI specific types}
 {SCSI Host}
 PSCSIHost = ^TSCSIHost;

 {SCSI Host Enumeration Callback}
 TSCSIHostEnumerate = function(Host:PSCSIHost;Data:Pointer):LongWord;

 {SCSI Host Methods}
 //To Do

 TSCSIHost = record
  {Device Properties}
  Device:TDevice;                      {The Device entry for this Host}
  {Host Properties}
  HostId:LongWord;                     {Unique Id of this Host in the Host table}
  HostState:LongWord;                  {Host state (eg ?????)}
  //To Do
  {Statistics Properties}
  //To Do //Errors etc
  {Driver Properties}
  Lock:TMutexHandle;                   {Host lock}
  //To Do
  {Internal Properties}
  Prev:PSCSIHost;                      {Previous entry in Host table}
  Next:PSCSIHost;                      {Next entry in Host table}
 end;

 {SCSI Device}
 PSCSIDevice = ^TSCSIDevice;

 {SCSI Enumeration Callback}
 TSCSIEnumerate = function(SCSI:PSCSIDevice;Data:Pointer):LongWord;
 {SCSI Notification Callback}
 TSCSINotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;

 {SCSI Device Methods}
 //To Do //Inquiry/RequestSense/Read10/Write10 etc ?

 TSCSIDevice = record
  {Device Properties}
  Device:TDevice;                      {The Device entry for this SCSI}
  {SCSI Properties}
  SCSIId:LongWord;                     {Unique Id of this SCSI in the SCSI table}
  SCSIState:LongWord;                  {SCSI state (eg SCSI_STATE_INSERTED)}
  //To Do
  {Statistics Properties}
  //To Do //Errors etc
  {Driver Properties}
  Lock:TMutexHandle;                   {Device lock}
  //To Do //Size etc
  Vendor:PChar;                        {SCSI Vendor}
  Product:PChar;                       {SCSI Product}
  Revision:PChar;                      {Firmware Revision}
  {Internal Properties}
  Prev:PSCSIDevice;                    {Previous entry in SCSI table}
  Next:PSCSIDevice;                    {Next entry in SCSI table}
 end;

 {SCSI Command Block}
 PSCSICommandBlock = ^TSCSICommandBlock;
 TSCSICommandBlock = record
  Command:array[0..SCSI_COMMAND_MAX_SIZE - 1] of Byte;     {Command}
  SenseData:array[0..63] of Byte;   {Request Sense}
  Status:Byte;                      {SCSI Status}
  TargetID:Byte;                    {Target ID}
  TargetLUN:Byte;                   {Target LUN}
  CommandLength:Byte;               {Command Length}
  DataLength:LongWord;              {Total Data Length}
  Data:Pointer;                     {Pointer to Data}
  MessageOut:array[0..11] of Byte;  {Message out buffer}
  MessageIn:array[0..11] of Byte;   {Message in buffer}
  SenseCommandLength:Byte;          {Sense Command Length}
  SenseDataLength:LongWord;         {Sense Data Length}
  SenseCommand:array[0..5] of Byte; {Sense Command}
  ControllerStatus:LongWord;        {Controller Status}
  TransferredBytes:LongWord;        {Transferred Bytes}
 end;
 //To Do //Many of these may not be required. Handled instead in the Device descriptors ?

 {SCSI Inquiry Data}
 PSCSIStandardInquiryData = ^TSCSIStandardInquiryData;
 TSCSIStandardInquiryData = packed record
  DeviceType:Byte;                  {Peripheral Device Type (Bits 4..0)}
  RemovableMediaBit:Byte;           {Removable Media Bit (Bit 7) ($80)}
  Version:Byte;                     {ISO/ECMA/ANSI Version (ISO Bits 7..6)(ECMA Bits 5..3)(ANSI Bits 2..0)}
  ResponseFormat:Byte;              {Response Data Format (Bits 3..0)}
  AdditionalLength:Byte;            {Additional Length}
  Reserved1:Byte;
  Reserved2:Word;
  Vendor:array[0..7] of Char;       {Vendor Information}
  Product:array[0..15] of Char;     {Product Identification}
  Revision:array[0..3] of Char;     {Product Revision Level}
 end;

 {SCSI Request Sense Data}
 PSCSIRequestSenseData = ^TSCSIRequestSenseData;
 TSCSIRequestSenseData = packed record
  ErrorCode:Byte;        {Error Code (Bits 6..0) (Bit 7 is Valid bit)}
  Reserved1:Byte;
  SenseKey:Byte;         {Sense Key (Bits 3..0)}
  Information:LongWord;  {Information}
  AdditionalLength:Byte; {Additional Sense Length}
  Reserved2:LongWord;
  ASC:Byte;              {Additional Sense Code}
  ASCQ:Byte;             {Additional Sense Code Qualifier}
  Reserved3:LongWord;
 end;

 {SCSI Read Capacity Data}
 PSCSIReadCapacityData = ^TSCSIReadCapacityData;
 TSCSIReadCapacityData = packed record
  LastBlock:LongWord;    {Last Logical Block Address (Big Endian)}
  BlockSize:LongWord;    {Block Length In Bytes (Big Endian)}
 end;

 {SCSI Read Capacity 16 Data}
 PSCSIReadCapacity16Data = ^TSCSIReadCapacity16Data;
 TSCSIReadCapacity16Data = packed record
  LastBlock:Int64;       {Last Logical Block Address (Big Endian)}
  BlockSize:LongWord;    {Block Length In Bytes (Big Endian)}
  Reserved1:Byte;        {P_TYPE (Bits 3..1) PROT_EN (Bit 0)}
  Reserved2:Byte;        {P_I_EXPONENT (Bitss 7..4) LOGICAL BLOCKS PER PHYSICAL BLOCK EXPONENT (Bits 3..0}
  Reserved3:Byte;        {TPE (Bit 7) TPRZ (Bit 6) LOWEST ALIGNED LOGICAL BLOCK ADDRESS (Bits 5..0)(Big Endian)}
  Reserved4:Byte;        {LOWEST ALIGNED LOGICAL BLOCK ADDRESS (Bits 7..0)(Big Endian)}
  Reserved5:array[0..15] of Byte;
 end;

{==============================================================================}
{var}
 {SCSI specific variables}

{==============================================================================}
{Initialization Functions}
procedure SCSIInit;

{==============================================================================}
{SCSI Functions}
function SCSIDeviceInquiry(SCSI:PSCSIDevice;var DeviceType,DeviceFlags:LongWord;var Vendor,Product,Revision:PChar):LongWord;
function SCSIDeviceRequestSense(SCSI:PSCSIDevice;var SenseKey,ASC,ASCQ:Byte):LongWord;
function SCSIDeviceReadCapacity(SCSI:PSCSIDevice;var BlockSize,BlockShift:LongWord;var BlockCount:Int64):LongWord;
function SCSIDeviceTestUnitReady(SCSI:PSCSIDevice;var DeviceFlags:LongWord):LongWord;

function SCSIDeviceRead10(SCSI:PSCSIDevice;Start:LongWord;Count:Word;Buffer:Pointer):LongWord;
function SCSIDeviceWrite10(SCSI:PSCSIDevice;Start:LongWord;Count:Word;Buffer:Pointer):LongWord;

function SCSIDeviceRead16(SCSI:PSCSIDevice;const Start,Count:Int64;Buffer:Pointer):LongWord;
function SCSIDeviceWrite16(SCSI:PSCSIDevice;const Start,Count:Int64;Buffer:Pointer):LongWord;

function SCSIDeviceTransport(SCSI:PSCSIDevice;Command:PSCSICommandBlock):LongWord;

function SCSIDeviceCreate:PSCSIDevice;
function SCSIDeviceCreateEx(Size:LongWord):PSCSIDevice;
function SCSIDeviceDestroy(SCSI:PSCSIDevice):LongWord;

function SCSIDeviceRegister(SCSI:PSCSIDevice):LongWord;
function SCSIDeviceDeregister(SCSI:PSCSIDevice):LongWord;

function SCSIDeviceFind(SCSIId:LongWord):PSCSIDevice;
function SCSIDeviceFindByName(const Name:String):PSCSIDevice; inline;
function SCSIDeviceFindByDescription(const Description:String):PSCSIDevice; inline;
function SCSIDeviceEnumerate(Callback:TSCSIEnumerate;Data:Pointer):LongWord;

function SCSIDeviceNotification(SCSI:PSCSIDevice;Callback:TSCSINotification;Data:Pointer;Notification,Flags:LongWord):LongWord;

function SCSIHostCreate:PSCSIHost;
function SCSIHostCreateEx(Size:LongWord):PSCSIHost;
function SCSIHostDestroy(Host:PSCSIHost):LongWord;

function SCSIHostRegister(Host:PSCSIHost):LongWord;
function SCSIHostDeregister(Host:PSCSIHost):LongWord;

function SCSIHostFind(HostId:LongWord):PSCSIHost;
function SCSIHostEnumerate(Callback:TSCSIHostEnumerate;Data:Pointer):LongWord;

{==============================================================================}
{SCSI Helper Functions}
function SCSIGetCount:LongWord;

function SCSIDeviceCheck(SCSI:PSCSIDevice):PSCSIDevice;

procedure SCSILog(Level:LongWord;SCSI:PSCSIDevice;const AText:String);
procedure SCSILogInfo(SCSI:PSCSIDevice;const AText:String); inline;
procedure SCSILogWarn(SCSI:PSCSIDevice;const AText:String); inline;
procedure SCSILogError(SCSI:PSCSIDevice;const AText:String); inline;
procedure SCSILogDebug(SCSI:PSCSIDevice;const AText:String); inline;

function SCSIDeviceTypeToStorageType(DeviceType:Byte;Removable,Floppy:Boolean):LongWord;

function SCSIHostGetCount:LongWord;

function SCSIHostCheck(Host:PSCSIHost):PSCSIHost;

{==============================================================================}
{SCSI Storage Functions}
function SCSIStorageDeviceRead(Storage:PStorageDevice;const Start,Count:Int64;Buffer:Pointer):LongWord;
function SCSIStorageDeviceWrite(Storage:PStorageDevice;const Start,Count:Int64;Buffer:Pointer):LongWord;

function SCSIStorageDeviceControl(Storage:PStorageDevice;Request:Integer;Argument1:PtrUInt;var Argument2:PtrUInt):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {SCSI specific variables}
 SCSIInitialized:Boolean;

 SCSITable:PSCSIDevice;
 SCSITableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 SCSITableCount:LongWord;

 SCSIHostTable:PSCSIHost;
 SCSIHostTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 SCSIHostTableCount:LongWord;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure SCSIInit;
begin
 {}
 {Check Initialized}
 if SCSIInitialized then Exit;

 {Initialize Logging}
 SCSI_LOG_ENABLED:=(SCSI_DEFAULT_LOG_LEVEL <> SCSI_LOG_LEVEL_NONE);

 {Initialize SCSI Table}
 SCSITable:=nil;
 SCSITableLock:=CriticalSectionCreate;
 SCSITableCount:=0;
 if SCSITableLock = INVALID_HANDLE_VALUE then
  begin
   if SCSI_LOG_ENABLED then SCSILogError(nil,'Failed to create SCSI table lock');
  end;

 {Initialize SCSI Host Table}
 SCSIHostTable:=nil;
 SCSIHostTableLock:=CriticalSectionCreate;
 SCSIHostTableCount:=0;
 if SCSIHostTableLock = INVALID_HANDLE_VALUE then
  begin
   if SCSI_LOG_ENABLED then SCSILogError(nil,'Failed to create SCSI Host table lock');
  end;

 SCSIInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{SCSI Functions}
function SCSIDeviceInquiry(SCSI:PSCSIDevice;var DeviceType,DeviceFlags:LongWord;var Vendor,Product,Revision:PChar):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check SCSI}
 if SCSI = nil then Exit;

 //To Do
end;

{==============================================================================}

function SCSIDeviceRequestSense(SCSI:PSCSIDevice;var SenseKey,ASC,ASCQ:Byte):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check SCSI}
 if SCSI = nil then Exit;

 //To Do
end;

{==============================================================================}

function SCSIDeviceReadCapacity(SCSI:PSCSIDevice;var BlockSize,BlockShift:LongWord;var BlockCount:Int64):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check SCSI}
 if SCSI = nil then Exit;

 //To Do
end;

{==============================================================================}

function SCSIDeviceTestUnitReady(SCSI:PSCSIDevice;var DeviceFlags:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check SCSI}
 if SCSI = nil then Exit;

 //To Do
end;

{==============================================================================}

function SCSIDeviceRead10(SCSI:PSCSIDevice;Start:LongWord;Count:Word;Buffer:Pointer):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check SCSI}
 if SCSI = nil then Exit;

 //To Do
end;

{==============================================================================}

function SCSIDeviceWrite10(SCSI:PSCSIDevice;Start:LongWord;Count:Word;Buffer:Pointer):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check SCSI}
 if SCSI = nil then Exit;

 //To Do
end;

{==============================================================================}

function SCSIDeviceRead16(SCSI:PSCSIDevice;const Start,Count:Int64;Buffer:Pointer):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check SCSI}
 if SCSI = nil then Exit;

 //To Do
end;

{==============================================================================}

function SCSIDeviceWrite16(SCSI:PSCSIDevice;const Start,Count:Int64;Buffer:Pointer):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check SCSI}
 if SCSI = nil then Exit;

 //To Do
end;

{==============================================================================}

function SCSIDeviceTransport(SCSI:PSCSIDevice;Command:PSCSICommandBlock):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check SCSI}
 if SCSI = nil then Exit;

 {Check Command}
 if Command = nil then Exit;

 //To Do
end;

{==============================================================================}

function SCSIDeviceCreate:PSCSIDevice;
{Create a new SCSI entry}
{Return: Pointer to new SCSI entry or nil if SCSI could not be created}
begin
 {}
 Result:=SCSIDeviceCreateEx(SizeOf(TSCSIDevice));
end;

{==============================================================================}

function SCSIDeviceCreateEx(Size:LongWord):PSCSIDevice;
{Create a new SCSI entry}
{Size: Size in bytes to allocate for new SCSI (Including the SCSI entry)}
{Return: Pointer to new SCSI entry or nil if SCSI could not be created}
begin
 {}
 Result:=nil;

 {Check Size}
 if Size < SizeOf(TSCSIDevice) then Exit;

 {Create SCSI}
 Result:=PSCSIDevice(DeviceCreateEx(Size));
 if Result = nil then Exit;

 {Update Device}
 Result.Device.DeviceBus:=DEVICE_BUS_NONE;
 Result.Device.DeviceType:=SCSI_TYPE_NONE;
 Result.Device.DeviceFlags:=SCSI_FLAG_NONE;
 Result.Device.DeviceData:=nil;

 {Update SCSI}
 Result.SCSIId:=DEVICE_ID_ANY;
 Result.SCSIState:=SCSI_STATE_EJECTED;
 //To Do
 Result.Lock:=INVALID_HANDLE_VALUE;
 //To Do
 Result.Vendor:=nil;
 Result.Product:=nil;
 Result.Revision:=nil;

 {Create Lock}
 Result.Lock:=MutexCreate;
 if Result.Lock = INVALID_HANDLE_VALUE then
  begin
   if SCSI_LOG_ENABLED then SCSILogError(nil,'Failed to create lock for SCSI');
   SCSIDeviceDestroy(Result);
   Result:=nil;
   Exit;
  end;
end;

{==============================================================================}

function SCSIDeviceDestroy(SCSI:PSCSIDevice):LongWord;
{Destroy an existing SCSI entry}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check SCSI}
 if SCSI = nil then Exit;
 if SCSI.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check SCSI}
 Result:=ERROR_IN_USE;
 if SCSIDeviceCheck(SCSI) = SCSI then Exit;

 {Check State}
 if SCSI.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;

 {Destroy Lock}
 if SCSI.Lock <> INVALID_HANDLE_VALUE then
  begin
   MutexDestroy(SCSI.Lock);
  end;

 {Update SCSI}
 if SCSI.Vendor <> nil then FreeMem(SCSI.Vendor);
 if SCSI.Product <> nil then FreeMem(SCSI.Product);
 if SCSI.Revision <> nil then FreeMem(SCSI.Revision);

 {Destroy SCSI}
 Result:=DeviceDestroy(@SCSI.Device);
end;

{==============================================================================}

function SCSIDeviceRegister(SCSI:PSCSIDevice):LongWord;
{Register a new SCSI in the SCSI table}
var
 SCSIId:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check SCSI}
 if SCSI = nil then Exit;
 if SCSI.SCSIId <> DEVICE_ID_ANY then Exit;
 if SCSI.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check SCSI}
 Result:=ERROR_ALREADY_EXISTS;
 if SCSIDeviceCheck(SCSI) = SCSI then Exit;

 {Check State}
 if SCSI.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;

 {Insert SCSI}
 if CriticalSectionLock(SCSITableLock) = ERROR_SUCCESS then
  begin
   try
    {Update SCSI}
    SCSIId:=0;
    while SCSIDeviceFind(SCSIId) <> nil do
     begin
      Inc(SCSIId);
     end;
    SCSI.SCSIId:=SCSIId;

    {Update Device}
    SCSI.Device.DeviceName:=SCSI_NAME_PREFIX + IntToStr(SCSI.SCSIId);
    SCSI.Device.DeviceClass:=DEVICE_CLASS_SCSI;

    {Register Device}
    Result:=DeviceRegister(@SCSI.Device);
    if Result <> ERROR_SUCCESS then
     begin
      SCSI.SCSIId:=DEVICE_ID_ANY;
      Exit;
     end;

    {Link SCSI}
    if SCSITable = nil then
     begin
      SCSITable:=SCSI;
     end
    else
     begin
      SCSI.Next:=SCSITable;
      SCSITable.Prev:=SCSI;
      SCSITable:=SCSI;
     end;

    {Increment Count}
    Inc(SCSITableCount);

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(SCSITableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SCSIDeviceDeregister(SCSI:PSCSIDevice):LongWord;
{Deregister a SCSI from the SCSI table}
var
 Prev:PSCSIDevice;
 Next:PSCSIDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check SCSI}
 if SCSI = nil then Exit;
 if SCSI.SCSIId = DEVICE_ID_ANY then Exit;
 if SCSI.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check SCSI}
 Result:=ERROR_NOT_FOUND;
 if SCSIDeviceCheck(SCSI) <> SCSI then Exit;

 {Check State}
 if SCSI.Device.DeviceState <> DEVICE_STATE_REGISTERED then Exit;

 {Remove SCSI}
 if CriticalSectionLock(SCSITableLock) = ERROR_SUCCESS then
  begin
   try
    {Deregister Device}
    Result:=DeviceDeregister(@SCSI.Device);
    if Result <> ERROR_SUCCESS then Exit;

    {Unlink SCSI}
    Prev:=SCSI.Prev;
    Next:=SCSI.Next;
    if Prev = nil then
     begin
      SCSITable:=Next;
      if Next <> nil then
       begin
        Next.Prev:=nil;
       end;
     end
    else
     begin
      Prev.Next:=Next;
      if Next <> nil then
       begin
        Next.Prev:=Prev;
       end;
     end;

    {Decrement Count}
    Dec(SCSITableCount);

    {Update SCSI}
    SCSI.SCSIId:=DEVICE_ID_ANY;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(SCSITableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SCSIDeviceFind(SCSIId:LongWord):PSCSIDevice;
var
 SCSI:PSCSIDevice;
begin
 {}
 Result:=nil;

 {Check Id}
 if SCSIId = DEVICE_ID_ANY then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(SCSITableLock) = ERROR_SUCCESS then
  begin
   try
    {Get SCSI}
    SCSI:=SCSITable;
    while SCSI <> nil do
     begin
      {Check State}
      if SCSI.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Index}
        if SCSI.SCSIId = SCSIId then
         begin
          Result:=SCSI;
          Exit;
         end;
       end;

      {Get Next}
      SCSI:=SCSI.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(SCSITableLock);
   end;
  end;
end;

{==============================================================================}

function SCSIDeviceFindByName(const Name:String):PSCSIDevice; inline;
begin
 {}
 Result:=PSCSIDevice(DeviceFindByName(Name));
end;

{==============================================================================}

function SCSIDeviceFindByDescription(const Description:String):PSCSIDevice; inline;
begin
 {}
 Result:=PSCSIDevice(DeviceFindByDescription(Description));
end;

{==============================================================================}

function SCSIDeviceEnumerate(Callback:TSCSIEnumerate;Data:Pointer):LongWord;
var
 SCSI:PSCSIDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Callback}
 if not Assigned(Callback) then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(SCSITableLock) = ERROR_SUCCESS then
  begin
   try
    {Get SCSI}
    SCSI:=SCSITable;
    while SCSI <> nil do
     begin
      {Check State}
      if SCSI.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        if Callback(SCSI,Data) <> ERROR_SUCCESS then Exit;
       end;

      {Get Next}
      SCSI:=SCSI.Next;
     end;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(SCSITableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SCSIDeviceNotification(SCSI:PSCSIDevice;Callback:TSCSINotification;Data:Pointer;Notification,Flags:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check SCSI}
 if SCSI = nil then
  begin
   Result:=DeviceNotification(nil,DEVICE_CLASS_SCSI,Callback,Data,Notification,Flags);
  end
 else
  begin
   {Check SCSI}
   if SCSI.Device.Signature <> DEVICE_SIGNATURE then Exit;

   Result:=DeviceNotification(@SCSI.Device,DEVICE_CLASS_SCSI,Callback,Data,Notification,Flags);
  end;
end;

{==============================================================================}

function SCSIHostCreate:PSCSIHost;
{Create a new Host entry}
{Return: Pointer to new Host entry or nil if host could not be created}
begin
 {}
 Result:=SCSIHostCreateEx(SizeOf(TSCSIHost));
end;

{==============================================================================}

function SCSIHostCreateEx(Size:LongWord):PSCSIHost;
{Create a new Host entry}
{Size: Size in bytes to allocate for new host (Including the host entry)}
{Return: Pointer to new Host entry or nil if host could not be created}
begin
 {}
 Result:=nil;

 {Check Size}
 if Size < SizeOf(TSCSIHost) then Exit;

 {Create Host}
 Result:=PSCSIHost(DeviceCreateEx(Size));
 if Result = nil then Exit;

 {Update Device}
 Result.Device.DeviceBus:=DEVICE_BUS_NONE;
 //Result.Device.DeviceType:=SCSIHOST_TYPE_NONE;   //To Do
 //Result.Device.DeviceFlags:=SCSIHOST_FLAG_NONE;   //To Do
 Result.Device.DeviceData:=nil;

 {Update Host}
 Result.HostId:=DEVICE_ID_ANY;
 //Result.HostState:=SCSIHOST_STATE_; //To Do
 //To Do
 Result.Lock:=INVALID_HANDLE_VALUE;

 {Create Lock}
 Result.Lock:=MutexCreate;
 if Result.Lock = INVALID_HANDLE_VALUE then
  begin
   if SCSI_LOG_ENABLED then SCSILogError(nil,'Failed to create lock for SCSI Host');
   SCSIHostDestroy(Result);
   Result:=nil;
   Exit;
  end;
end;

{==============================================================================}

function SCSIHostDestroy(Host:PSCSIHost):LongWord;
{Destroy an existing Host entry}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Host}
 if Host = nil then Exit;
 if Host.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Host}
 Result:=ERROR_IN_USE;
 if SCSIHostCheck(Host) = Host then Exit;

 {Check State}
 if Host.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;

 {Destroy Lock}
 if Host.Lock <> INVALID_HANDLE_VALUE then
  begin
   MutexDestroy(Host.Lock);
  end;

 {Destroy Host}
 Result:=DeviceDestroy(@Host.Device);
end;

{==============================================================================}

function SCSIHostRegister(Host:PSCSIHost):LongWord;
{Register a new Host in the Host table}
var
 HostId:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Host}
 if Host = nil then Exit;
 if Host.HostId <> DEVICE_ID_ANY then Exit;
 if Host.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Host}
 Result:=ERROR_ALREADY_EXISTS;
 if SCSIHostCheck(Host) = Host then Exit;

 {Check State}
 if Host.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;

 {Insert Host}
 if CriticalSectionLock(SCSIHostTableLock) = ERROR_SUCCESS then
  begin
   try
    {Update Host}
    HostId:=0;
    while SCSIHostFind(HostId) <> nil do
     begin
      Inc(HostId);
     end;
    Host.HostId:=HostId;

    {Update Device}
    Host.Device.DeviceName:=SCSI_HOST_PREFIX + IntToStr(Host.HostId);
    Host.Device.DeviceClass:=DEVICE_CLASS_SCSIHOST;

    {Register Device}
    Result:=DeviceRegister(@Host.Device);
    if Result <> ERROR_SUCCESS then
     begin
      Host.HostId:=DEVICE_ID_ANY;
      Exit;
     end;

    {Link Host}
    if SCSIHostTable = nil then
     begin
      SCSIHostTable:=Host;
     end
    else
     begin
      Host.Next:=SCSIHostTable;
      SCSIHostTable.Prev:=Host;
      SCSIHostTable:=Host;
     end;

    {Increment Count}
    Inc(SCSIHostTableCount);

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(SCSIHostTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SCSIHostDeregister(Host:PSCSIHost):LongWord;
{Deregister a Host from the Host table}
var
 Prev:PSCSIHost;
 Next:PSCSIHost;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Host}
 if Host = nil then Exit;
 if Host.HostId = DEVICE_ID_ANY then Exit;
 if Host.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Host}
 Result:=ERROR_NOT_FOUND;
 if SCSIHostCheck(Host) <> Host then Exit;

 {Check State}
 if Host.Device.DeviceState <> DEVICE_STATE_REGISTERED then Exit;

 {Remove Host}
 if CriticalSectionLock(SCSIHostTableLock) = ERROR_SUCCESS then
  begin
   try
    {Deregister Device}
    Result:=DeviceDeregister(@Host.Device);
    if Result <> ERROR_SUCCESS then Exit;

    {Unlink Host}
    Prev:=Host.Prev;
    Next:=Host.Next;
    if Prev = nil then
     begin
      SCSIHostTable:=Next;
      if Next <> nil then
       begin
        Next.Prev:=nil;
       end;
     end
    else
     begin
      Prev.Next:=Next;
      if Next <> nil then
       begin
        Next.Prev:=Prev;
       end;
     end;

    {Decrement Count}
    Dec(SCSIHostTableCount);

    {Update Host}
    Host.HostId:=DEVICE_ID_ANY;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(SCSIHostTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SCSIHostFind(HostId:LongWord):PSCSIHost;
var
 Host:PSCSIHost;
begin
 {}
 Result:=nil;

 {Check Id}
 if HostId = DEVICE_ID_ANY then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(SCSIHostTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Host}
    Host:=SCSIHostTable;
    while Host <> nil do
     begin
      {Check State}
      if Host.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Index}
        if Host.HostId = HostId then
         begin
          Result:=Host;
          Exit;
         end;
       end;

      {Get Next}
      Host:=Host.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(SCSIHostTableLock);
   end;
  end;
end;

{==============================================================================}

function SCSIHostEnumerate(Callback:TSCSIHostEnumerate;Data:Pointer):LongWord;
var
 Host:PSCSIHost;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Callback}
 if not Assigned(Callback) then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(SCSIHostTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Host}
    Host:=SCSIHostTable;
    while Host <> nil do
     begin
      {Check State}
      if Host.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        if Callback(Host,Data) <> ERROR_SUCCESS then Exit;
       end;

      {Get Next}
      Host:=Host.Next;
     end;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(SCSIHostTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}
{==============================================================================}
{SCSI Helper Functions}
function SCSIGetCount:LongWord;
{Get the current SCSI count}
begin
 {}
 Result:=SCSITableCount;
end;

{==============================================================================}

function SCSIDeviceCheck(SCSI:PSCSIDevice):PSCSIDevice;
{Check if the supplied SCSI is in the SCSI table}
var
 Current:PSCSIDevice;
begin
 {}
 Result:=nil;

 {Check SCSI}
 if SCSI = nil then Exit;
 if SCSI.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(SCSITableLock) = ERROR_SUCCESS then
  begin
   try
    {Get SCSI}
    Current:=SCSITable;
    while Current <> nil do
     begin
      {Check SCSI}
      if Current = SCSI then
       begin
        Result:=SCSI;
        Exit;
       end;

      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(SCSITableLock);
   end;
  end;
end;

{==============================================================================}

procedure SCSILog(Level:LongWord;SCSI:PSCSIDevice;const AText:String);
var
 WorkBuffer:String;
begin
 {}
 {Check Level}
 if Level < SCSI_DEFAULT_LOG_LEVEL then Exit;

 WorkBuffer:='';
 {Check Level}
 if Level = SCSI_LOG_LEVEL_DEBUG then
  begin
   WorkBuffer:=WorkBuffer + '[DEBUG] ';
  end
 else if Level = SCSI_LOG_LEVEL_WARN then
  begin
   WorkBuffer:=WorkBuffer + '[WARN] ';
  end
 else if Level = SCSI_LOG_LEVEL_ERROR then
  begin
   WorkBuffer:=WorkBuffer + '[ERROR] ';
  end;

 {Add Prefix}
 WorkBuffer:=WorkBuffer + 'SCSI: ';

 {Check SCSI}
 if SCSI <> nil then
  begin
   WorkBuffer:=WorkBuffer + SCSI_NAME_PREFIX + IntToStr(SCSI.SCSIId) + ': ';
  end;

 {Output Logging}
 LoggingOutputEx(LOGGING_FACILITY_SCSI,LogLevelToLoggingSeverity(Level),'SCSI',WorkBuffer + AText);
end;

{==============================================================================}

procedure SCSILogInfo(SCSI:PSCSIDevice;const AText:String); inline;
begin
 {}
 SCSILog(SCSI_LOG_LEVEL_INFO,SCSI,AText);
end;

{==============================================================================}

procedure SCSILogWarn(SCSI:PSCSIDevice;const AText:String); inline;
begin
 {}
 SCSILog(SCSI_LOG_LEVEL_WARN,SCSI,AText);
end;

{==============================================================================}

procedure SCSILogError(SCSI:PSCSIDevice;const AText:String); inline;
begin
 {}
 SCSILog(SCSI_LOG_LEVEL_ERROR,SCSI,AText);
end;

{==============================================================================}

procedure SCSILogDebug(SCSI:PSCSIDevice;const AText:String); inline;
begin
 {}
 SCSILog(SCSI_LOG_LEVEL_DEBUG,SCSI,AText);
end;

{==============================================================================}

function SCSIDeviceTypeToStorageType(DeviceType:Byte;Removable,Floppy:Boolean):LongWord;
begin
 {}
 Result:=STORAGE_TYPE_NONE;

 case DeviceType of
  SCSI_DEVICE_TYPE_DISK:begin
    Result:=STORAGE_TYPE_HDD;
    if Removable then Result:=STORAGE_TYPE_REMOVABLE;
    if Removable and Floppy then Result:=STORAGE_TYPE_FDD;
   end;
  SCSI_DEVICE_TYPE_TAPE:Result:=STORAGE_TYPE_TAPE;
  SCSI_DEVICE_TYPE_PRINTER:Result:=STORAGE_TYPE_NONE;
  SCSI_DEVICE_TYPE_PROCESSOR:Result:=STORAGE_TYPE_NONE;
  SCSI_DEVICE_TYPE_WRITE_ONCE:Result:=STORAGE_TYPE_OPTICAL;
  SCSI_DEVICE_TYPE_CD_DVD:Result:=STORAGE_TYPE_CDROM;
  SCSI_DEVICE_TYPE_SCANNER:Result:=STORAGE_TYPE_NONE;
  SCSI_DEVICE_TYPE_OPTICAL:Result:=STORAGE_TYPE_OPTICAL;
  SCSI_DEVICE_TYPE_CHANGER:Result:=STORAGE_TYPE_NONE;
  SCSI_DEVICE_TYPE_COMMS:Result:=STORAGE_TYPE_NONE;
  SCSI_DEVICE_TYPE_ARRAY:Result:=STORAGE_TYPE_NONE;
  SCSI_DEVICE_TYPE_ENCLOSURE:Result:=STORAGE_TYPE_NONE;
  SCSI_DEVICE_TYPE_RBC:begin
    Result:=STORAGE_TYPE_HDD;
    if Removable then Result:=STORAGE_TYPE_REMOVABLE;
    if Removable and Floppy then Result:=STORAGE_TYPE_FDD;
   end;
  SCSI_DEVICE_TYPE_CARD:Result:=STORAGE_TYPE_NONE;
  SCSI_DEVICE_TYPE_BRIDGE:Result:=STORAGE_TYPE_NONE;
  SCSI_DEVICE_TYPE_OBJECT:Result:=STORAGE_TYPE_NONE;
  SCSI_DEVICE_TYPE_AUTOMATION:Result:=STORAGE_TYPE_NONE;
  SCSI_DEVICE_TYPE_WELL_KNOWN:Result:=STORAGE_TYPE_NONE;
  SCSI_DEVICE_TYPE_UNKNOWN:Result:=STORAGE_TYPE_NONE;
 end;
end;

{==============================================================================}

function SCSIHostGetCount:LongWord;
{Get the current Host count}
begin
 {}
 Result:=SCSIHostTableCount;
end;

{==============================================================================}

function SCSIHostCheck(Host:PSCSIHost):PSCSIHost;
{Check if the supplied Host is in the host table}
var
 Current:PSCSIHost;
begin
 {}
 Result:=nil;

 {Check Host}
 if Host = nil then Exit;
 if Host.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(SCSIHostTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Host}
    Current:=SCSIHostTable;
    while Current <> nil do
     begin
      {Check Host}
      if Current = Host then
       begin
        Result:=Host;
        Exit;
       end;

      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(SCSIHostTableLock);
   end;
  end;
end;

{==============================================================================}
{==============================================================================}
{SCSI Storage Functions}
function SCSIStorageDeviceRead(Storage:PStorageDevice;const Start,Count:Int64;Buffer:Pointer):LongWord;
var
 BlockCount:Word;
 ReadOffset:PtrUInt;
 ReadRemain:Int64;
 SCSI:PSCSIDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Storage}
 if Storage = nil then Exit;

 {Check Count}
 Result:=ERROR_SUCCESS;
 if Count = 0 then Exit;

 {Check Buffer}
 Result:=ERROR_INVALID_PARAMETER;
 if Buffer = nil then Exit;

 {Get SCSI}
 SCSI:=PSCSIDevice(Storage.Device.DeviceData);
 if SCSI = nil then Exit;

 //To Do //See USBStorageDeviceRead
end;

{==============================================================================}

function SCSIStorageDeviceWrite(Storage:PStorageDevice;const Start,Count:Int64;Buffer:Pointer):LongWord;
var
 BlockCount:Word;
 WriteOffset:PtrUInt;
 WriteRemain:Int64;
 SCSI:PSCSIDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Storage}
 if Storage = nil then Exit;

 {Check Count}
 Result:=ERROR_SUCCESS;
 if Count = 0 then Exit;

 {Check Buffer}
 Result:=ERROR_INVALID_PARAMETER;
 if Buffer = nil then Exit;

 {Get SCSI}
 SCSI:=PSCSIDevice(Storage.Device.DeviceData);
 if SCSI = nil then Exit;

 //To Do //See USBStorageDeviceWrite
end;

{==============================================================================}

function SCSIStorageDeviceControl(Storage:PStorageDevice;Request:Integer;Argument1:PtrUInt;var Argument2:PtrUInt):LongWord;
var
 SCSI:PSCSIDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Storage}
 if Storage = nil then Exit;

 {Get SCSI}
 SCSI:=PSCSIDevice(Storage.Device.DeviceData);
 if SCSI = nil then Exit;

 //To Do //See USBStorageDeviceControl
end;

{==============================================================================}
{==============================================================================}

initialization
 SCSIInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.

