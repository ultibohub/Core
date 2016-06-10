{
Ultibo Global Constant Definitions.

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


Global Constants
================


}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit GlobalConst; 

interface

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
{Global constants}
const
 {Version constants}
 ULTIBO_RELEASE_DATE             = '10/6/2016';
 ULTIBO_RELEASE_NAME             = 'Cucumber';
 ULTIBO_RELEASE_VERSION          = '1.2.021';
 ULTIBO_RELEASE_VERSION_MAJOR    = 1;
 ULTIBO_RELEASE_VERSION_MINOR    = 2;
 ULTIBO_RELEASE_VERSION_REVISION = 021;
 
{==============================================================================}
const
 {Universal error constants}
 ERROR_SUCCESS             = 0;    {Success}
 NO_ERROR                  = 0;    {Success}
 
 ERROR_INVALID_FUNCTION    = 1;    {Invalid function}
 ERROR_FILE_NOT_FOUND      = 2;    {The file cannot be found}
 ERROR_PATH_NOT_FOUND      = 3;    {The path cannot be found}
 ERROR_TOO_MANY_OPEN_FILES = 4;    {Too many open files}
 ERROR_ACCESS_DENIED       = 5;    {Access is denied}
 ERROR_INVALID_HANDLE      = 6;    {Invalid handle}
 ERROR_NOT_ENOUGH_MEMORY   = DWORD(8);  {Not enough storage is available to process this command}
 
 ERROR_INVALID_ACCESS      = 12;   {Invalid access}
 ERROR_INVALID_DATA        = 13;   {The data is invalid}
 ERROR_OUTOFMEMORY         = 14;   {Not enough memory is available}
 ERROR_INVALID_DRIVE       = 15;   {Cannot find the drive specified}
 ERROR_CURRENT_DIRECTORY   = 16;   {Current directory cannot be removed}
 ERROR_NOT_SAME_DEVICE     = 17;   {Cannot move the file to a different disk drive}
 ERROR_NO_MORE_FILES       = 18;   {There are no more files}
 ERROR_WRITE_PROTECT       = 19;   {Media is write protected}
 ERROR_BAD_UNIT            = 20;   {Cannot find the device specified}
 ERROR_NOT_READY           = 21;   {The device is not ready}
 ERROR_BAD_COMMAND         = 22;   {The device does not recognise the command}
 
 ERROR_WRITE_FAULT         = 29;   {The device cannot be written to}
 ERROR_READ_FAULT          = 30;   {The device cannot be read from}
 ERROR_GEN_FAILURE         = 31;   {The device has failed}
 
 ERROR_NOT_SUPPORTED       = 50;   {The request is not supported}
 
 ERROR_DEV_NOT_EXIST       = 55;   {The device does not exist}
 
 ERROR_BAD_DEV_TYPE        = 66;   {Invalid device type}
 
 ERROR_ALREADY_ASSIGNED    = 85;   {The device name is already in use}
 ERROR_INVALID_PASSWORD    = 86;   {Invalid pasword}
 ERROR_INVALID_PARAMETER   = 87;   {Invalid parameter}
  
 ERROR_SEM_IS_SET          = 102;  {The semaphore is in use and cannot be closed}
 ERROR_OPEN_FAILED         = 110;  {The file or device could not be opened}
 ERROR_CALL_NOT_IMPLEMENTED = 120; {The function is not currently implemented}
 ERROR_INSUFFICIENT_BUFFER = 122;  {The buffer passed is too small for the requested data}
 ERROR_WAIT_NO_CHILDREN    = 128;  {There are no child processes to wait for}
 
 ERROR_NOT_LOCKED          = 158;  {The entry is not locked}
 
 ERROR_LOCK_FAILED         = 167;  {The lock operation failed}
 
 ERROR_ALREADY_EXISTS      = 183;  {The file or object already exists}
 
 ERROR_ENVVAR_NOT_FOUND    = 203;  {The environment variable could not be found}
 
 ERROR_LOCKED              = 212;  {The entry is already locked}

 ERROR_MORE_DATA           = 234;  {More data is available than the provided buffer}

 ERROR_WAIT_TIMEOUT        = 258;  {The operation timed out}
 ERROR_NO_MORE_ITEMS       = 259;  {No more items available}
 
 ERROR_NOT_OWNER           = 288;  {The current thread is not the owner}
 
 ERROR_OPERATION_ABORTED   = DWORD(995); {The I/O operation has been aborted because of either a thread exit or an application request}
 ERROR_IO_INCOMPLETE       = DWORD(996); {Overlapped I/O event is not in a signaled state}
 ERROR_IO_PENDING          = DWORD(997); {Overlapped I/O operation is in progress}
 
 ERROR_CAN_NOT_COMPLETE    = 1003; {Cannot complete the function}
 
 ERROR_NOT_FOUND           = 1168; {The entry or device was not found}
 
 ERROR_INVALID_ACL            = DWORD(1336);  {The access control list (ACL) structure is invalid}
 ERROR_INVALID_SID            = DWORD(1337);  {The security ID structure is invalid}
 ERROR_INVALID_SECURITY_DESCR = DWORD(1338);  {The security descriptor structure is invalid}
 
 ERROR_TIMEOUT             = 1460; {The operation returned because the timeout expired}
 
 ERROR_FUNCTION_FAILED     = 1627; {The function call failed}
 
 {Errors below here have no compatibility equivalent}
 ERROR_NOT_VALID           = 1000001;    {The entry or device is not valid}
 ERROR_NOT_ASSIGNED        = 1000002;    {The device is not assigned}
 ERROR_IN_USE              = 1000003;    {The device is in use}
 ERROR_OPERATION_FAILED    = 1000004;    {The operation failed}
 ERROR_NOT_OPEN            = 1000005;    {The file or device is not open}
 ERROR_ALREADY_OPEN        = 1000006;    {The file or device is already open}
 ERROR_WAIT_ABANDONED      = 1000007;    {The operation was abandoned}
 ERROR_IN_PROGRESS         = 1000008;    {An operation is already in progress}
 ERROR_RUNTIME_ERROR       = 1000009;    {A run time occurred}
 ERROR_EXCEPTION           = 1000010;    {An exception occurred}
 ERROR_NOT_PROCESSED       = 1000011;    {The entry has not been processed}
 ERROR_NOT_COMPLETED       = 1000012;    {The entry or operation has not completed}
 ERROR_NOT_COMPATIBLE      = 1000013;    {The entry is not compatible for the operation}
 ERROR_CANCELLED           = 1000014;    {The entry or operation has been cancelled}
 
 ERROR_UNKNOWN             = $FFFFFFFF;
 
{==============================================================================}
const
 {Universal value constants}
 INVALID_HANDLE_VALUE     = THandle(-1); {DWORD(-1);}
 INVALID_FILE_SIZE        = DWORD($FFFFFFFF);
 INVALID_SET_FILE_POINTER = DWORD(-1);
 INVALID_FILE_ATTRIBUTES  = DWORD(-1);
 
 {File position constants}
 FILE_BEGIN   = 0;
 FILE_CURRENT = 1;
 FILE_END     = 2;
 
 {File open/create constants}
 CREATE_NEW        = 1;
 CREATE_ALWAYS     = 2;
 OPEN_EXISTING     = 3;
 OPEN_ALWAYS       = 4;
 TRUNCATE_EXISTING = 5;
 
 {File creation flag constants}
 FILE_FLAG_WRITE_THROUGH       = DWORD($80000000);
 FILE_FLAG_OVERLAPPED          = $40000000;
 FILE_FLAG_NO_BUFFERING        = $20000000;
 FILE_FLAG_RANDOM_ACCESS       = $10000000;
 FILE_FLAG_SEQUENTIAL_SCAN     = $08000000;
 FILE_FLAG_DELETE_ON_CLOSE     = $04000000;
 FILE_FLAG_BACKUP_SEMANTICS    = $02000000;
 FILE_FLAG_POSIX_SEMANTICS     = $01000000;
 FILE_FLAG_OPEN_REPARSE_POINT  = $00200000;
 FILE_FLAG_OPEN_NO_RECALL      = $00100000;
 FILE_FLAG_FIRST_PIPE_INSTANCE = $00080000;
 
 {File attribute constants}
 FILE_ATTRIBUTE_READONLY            = $00000001;
 FILE_ATTRIBUTE_HIDDEN              = $00000002;
 FILE_ATTRIBUTE_SYSTEM              = $00000004;
 FILE_ATTRIBUTE_DIRECTORY           = $00000010;
 FILE_ATTRIBUTE_ARCHIVE             = $00000020;
 FILE_ATTRIBUTE_DEVICE              = $00000040;
 FILE_ATTRIBUTE_NORMAL              = $00000080;
 FILE_ATTRIBUTE_TEMPORARY           = $00000100;
 FILE_ATTRIBUTE_SPARSE_FILE         = $00000200;
 FILE_ATTRIBUTE_REPARSE_POINT       = $00000400;
 FILE_ATTRIBUTE_COMPRESSED          = $00000800;
 FILE_ATTRIBUTE_OFFLINE             = $00001000;
 FILE_ATTRIBUTE_NOT_CONTENT_INDEXED = $00002000;
 FILE_ATTRIBUTE_ENCRYPTED           = $00004000;
 
 {Filesystem attribute constants}
 FS_CASE_SENSITIVE_SEARCH           = $00000001;
 FS_CASE_PRESERVED_NAMES            = $00000002;
 FS_UNICODE_ON_DISK                 = $00000004;
 FS_PERSISTENT_ACLS                 = $00000008;
 FS_FILE_COMPRESSION                = $00000010;
 FS_VOLUME_QUOTAS                   = $00000020;
 FS_SUPPORTS_SPARSE_FILES           = $00000040;
 FS_SUPPORTS_REPARSE_POINTS         = $00000080;
 FS_SUPPORTS_REMOTE_STORAGE         = $00000100;
 FS_VOLUME_IS_COMPRESSED            = $00008000;
 FS_SUPPORTS_OBJECT_IDS             = $00010000;
 FS_SUPPORTS_ENCRYPTION             = $00020000;
 FS_NAMED_STREAMS                   = $00040000;
 FS_READ_ONLY_VOLUME                = $00080000;
 
 {Timeout constants}
 INFINITE = LongWord(-1);
 
 {TLS constants}
 TLS_OUT_OF_INDEXES = DWORD($FFFFFFFF);
 
 {Wait constants}
 WAIT_OBJECT_0      = ERROR_SUCCESS + 0;
 WAIT_ABANDONED     = $00000080 + 0; {STATUS_ABANDONED_WAIT_0}
 WAIT_ABANDONED_0   = $00000080 + 0; {STATUS_ABANDONED_WAIT_0}
 WAIT_TIMEOUT       = ERROR_WAIT_TIMEOUT;
 WAIT_FAILED        = DWORD($FFFFFFFF);

 WAIT_IO_COMPLETION = $000000C0; {STATUS_USER_APC}
 
 MAXIMUM_WAIT_OBJECTS  = 64;      {Maximum number of wait objects}
 
 {Thread state constants}
 STILL_ACTIVE     = ERROR_NO_MORE_ITEMS;
 
 {TimeZone constants}
 TIME_ZONE_ID_INVALID = DWORD($FFFFFFFF);
 
{==============================================================================}
const
 {Universal key constants}
 KEY_NONE = 0;
 //To Do //KEY_ etc

{==============================================================================}
const
 {Universal color constants}
 {Basic 32 bit RGB colors (8 bit Transparency, 8 bit Red, 8 bit Green, 8 bit Blue)}
 COLOR_NONE      = $00000000;
 COLOR_BLACK     = $FF000000;
 COLOR_RED       = $FFFF0000;
 COLOR_ORANGE    = $FFFF8500; 
 COLOR_LEAFGREEN = $FF009900;
 COLOR_GREEN     = $FF00FF00;
 COLOR_YELLOW    = $FFFFFF00;
 COLOR_DARKGREEN = $FF254117;
 COLOR_DARKGRAY  = $FF595959; 
 COLOR_BROWN     = $FF6F4E37;
 COLOR_INDIGO    = $FF2E0854;
 COLOR_RASPBERRY = $FFE30B5C;
 COLOR_GRAY      = $FF808080;
 COLOR_PURPLE    = $FF4B0082;
 COLOR_PINK      = $FFFF0090;
 COLOR_DARKBLUE  = $FF0000A0;
 COLOR_SILVER    = $FFC0C0C0;
 COLOR_BLUEIVY   = $FF3090C7;
 COLOR_MIDGRAY   = $FFE0E0E0;
 COLOR_LIGHTGRAY = $FFF7F7F7;
 COLOR_BLUE      = $FF0000FF;
 COLOR_MAGENTA   = $FFFF00FF;
 COLOR_CYAN      = $FF00FFFF;
 COLOR_WHITE     = $FFFFFFFF;
 
{==============================================================================}
const
 {Universal size constants (From /include/linux/sizes.h)}
 SIZE_0     = $00000000;
 SIZE_1     = $00000001;
 SIZE_2     = $00000002;
 SIZE_4     = $00000004;
 SIZE_8     = $00000008;
 SIZE_16    = $00000010;
 SIZE_32    = $00000020;
 SIZE_64    = $00000040;
 SIZE_128   = $00000080;
 SIZE_256   = $00000100;
 SIZE_512   = $00000200;

 SIZE_1K    = $00000400;
 SIZE_2K    = $00000800;
 SIZE_4K    = $00001000;
 SIZE_8K    = $00002000;
 SIZE_16K   = $00004000;
 SIZE_32K   = $00008000;
 SIZE_64K   = $00010000;
 SIZE_128K  = $00020000;
 SIZE_256K  = $00040000;
 SIZE_512K  = $00080000;

 SIZE_1M    = $00100000;
 SIZE_2M    = $00200000;
 SIZE_4M    = $00400000;
 SIZE_8M    = $00800000;
 SIZE_16M   = $01000000;
 SIZE_32M   = $02000000;
 SIZE_64M   = $04000000;
 SIZE_128M  = $08000000;
 SIZE_256M  = $10000000;
 SIZE_512M  = $20000000;

 SIZE_1G    = $40000000;
 SIZE_2G    = $80000000;
 
{==============================================================================}
const
 {Universal time constants}
 MILLISECONDS_PER_SECOND = 1000;
 MICROSECONDS_PER_SECOND = 1000000;
 NANOSECONDS_PER_SECOND  = 1000000000;
 
const
 {Ultibo time constants (100 nanosecond ticks since 1/1/1601)}
 {Note: Nanoseconds is 10^9 so 100 nanosecond ticks is 10^7}
 TIME_TICKS_PER_MICROSECOND = 10;           {10^7 / 10^6}
 TIME_TICKS_PER_MILLISECOND = 10000;        {10^7 / 10^3}
 TIME_TICKS_PER_SECOND      = 10000000;     {10^7}
 TIME_TICKS_PER_MINUTE      = 600000000;    {60 * 10^7}
 TIME_TICKS_PER_HOUR        = 36000000000;  {60 * 60 * 10^7}
 TIME_TICKS_PER_DAY         = 864000000000; {24 * 60 * 60 * 10^7}

 TIME_TICKS_TO_1899 = 94353120000000000;    {Offset between 1/1/1601 (Ultibo) and 30/12/1899 (FreePascal)}
 TIME_TICKS_TO_1970 = 116444736000000000;   {Offset between 1/1/1601 (Ultibo) and 1/1/1970 (Unix/Linux)}
 TIME_TICKS_TO_1980 = 119600064000000000;   {Offset between 1/1/1601 (Ultibo) and 1/1/1980 (DOS)}

 TIME_TICKS_PER_10MILLISECONDS = 100000;    {10^7 / 10^2}
 
const
 {Unix/Linux time constants (Seconds since 1/1/1970)}
 UNIX_TIME_SECONDS_PER_DAY =  86400;    {60*60*24;}
 UNIX_TIME_DAYS_TO_1970    =  25569.0;  {Offset between 1899 (FreePascal) and 1970 (Unix/Linux)}
 
const
 {FreePascal time constants (TDateTime starts at 30/12/1899)}
 PASCAL_TIME_MILLISECONDS_PER_DAY   =  86400000; {60*60*24*1000;}
 PASCAL_TIME_SECONDS_PER_DAY        =  86400;    {60*60*24;}
 PASCAL_TIME_DOS_TIME_START         =  2162688;  {DOS date time value for start of DOS time (1/1/1980)}

 PASCAL_DAY_OFFSET = 1.0;                        {TDateTime value 1 day} 
 PASCAL_MINUTE_OFFSET = 0.000694444444444444;    {TDateTime value of 1 minute}

{==============================================================================}
const
 {System Call constants} 
 SYSTEM_CALL_UNDEFINED      = $00000000;
 SYSTEM_CALL_CONTEXT_SWITCH = $00000001;
 
{==============================================================================}
const
 {Machine Type constants} 
 MACHINE_TYPE_UNKNOWN = 0;
 MACHINE_TYPE_BCM2708 = 1;  {Broadcom BCM2708 (Raspberry Pi)}
 MACHINE_TYPE_BCM2709 = 2;  {Broadcom BCM2709 (Raspberry Pi 2)}
 MACHINE_TYPE_BCM2710 = 3;  {Broadcom BCM2710 (Raspberry Pi 3)}
 
{==============================================================================}
const
 {Board Type constants}
 BOARD_TYPE_UNKNOWN      = 0;
 BOARD_TYPE_RPIA         = 1;  {Raspberry Pi Model A}
 BOARD_TYPE_RPIB         = 2;  {Raspberry Pi Model B}
 BOARD_TYPE_RPI_COMPUTE  = 3;  {Raspberry Pi Compute Module}
 BOARD_TYPE_RPIA_PLUS    = 4;  {Raspberry Pi Model A+}
 BOARD_TYPE_RPIB_PLUS    = 5;  {Raspberry Pi Model B+}
 BOARD_TYPE_RPI2B        = 6;  {Raspberry Pi 2 Model B}
 BOARD_TYPE_RPI_ZERO     = 7;  {Raspberry Pi Model Zero}
 BOARD_TYPE_BPI          = 8;  {Banana Pi}
 BOARD_TYPE_BPRO         = 9;  {Banana Pro}
 BOARD_TYPE_BBB_REVC     = 10; {Beagle Bone Black (Revision C)}
 BOARD_TYPE_CUBOX_I      = 11; {Cubox i1/i2/i2ex/i4pro/i4x4}
 BOARD_TYPE_HUMMINGBOARD = 12; {Hummingboard}
 BOARD_TYPE_CREATOR_CI20 = 13; {MIPS Creator CI20}
 BOARD_TYPE_PCDUINO1     = 14; {pcDuino V1}
 BOARD_TYPE_PCDUINO2     = 15; {pcDuino V2}
 BOARD_TYPE_PCDUINO3     = 16; {pcDuino V3}
 BOARD_TYPE_ODROID_C1    = 17; {Odroid C1/C1+}
 BOARD_TYPE_ODROID_U2    = 18; {Odroid U2}
 BOARD_TYPE_ODROID_U3    = 19; {Odroid U3}
 BOARD_TYPE_ODROID_XU3   = 20; {Odroid XU3}
 BOARD_TYPE_ODROID_XU4   = 21; {Odroid XU4}
 BOARD_TYPE_PC_X86       = 22; {PC x86}
 BOARD_TYPE_PC_X86_64    = 23; {PC x86 64bit}
 BOARD_TYPE_RPI3B        = 24; {Raspberry Pi 3 Model B}
 
{==============================================================================}
const 
 {CPU Arch constants}
 CPU_ARCH_UNKNOWN   = 0;
 CPU_ARCH_ARM32     = 1; {ARM Arch 32 (ARMv6/ARMv7)(ARMv8 in 32bit mode)}
 CPU_ARCH_ARM64     = 2; {ARM Arch 64 (ARMv8}
 
const 
 {CPU Type constants}
 CPU_TYPE_UNKNOWN   = 0;
 CPU_TYPE_ARMV6     = 1; {ARMv6 (ARM1176 etc)}
 CPU_TYPE_ARMV7     = 2; {ARMv7 (Cortex A5/A7/A8/A9/A15/A17 etc)}
 CPU_TYPE_ARMV8     = 3; {ARMv8 (Cortex A53/A57/A72 etc)}
 
const
 {CPU Model constants}
 CPU_MODEL_UNKNOWN     = 0;
 CPU_MODEL_ARM1176JZFS = 1;  {ARM1176JZF-S}
 CPU_MODEL_CORTEX_A5   = 2;  {ARM Cortex-A5}
 CPU_MODEL_CORTEX_A7   = 3;  {ARM Cortex-A7}
 CPU_MODEL_CORTEX_A8   = 4;  {ARM Cortex-A8}
 CPU_MODEL_CORTEX_A9   = 5;  {ARM Cortex-A9}
 CPU_MODEL_CORTEX_A15  = 6;  {ARM Cortex-A15}
 CPU_MODEL_CORTEX_A17  = 7;  {ARM Cortex-A17}
 CPU_MODEL_CORTEX_A53  = 8;  {ARM Cortex-A53}
 CPU_MODEL_CORTEX_A57  = 9;  {ARM Cortex-A57}
 CPU_MODEL_CORTEX_A72  = 10; {ARM Cortex-A72}
 
const
 {CPU Description constants}
 CPU_DESCRIPTION_UNKNOWN      = 'Unknown';
 CPU_DESCRIPTION_ARM1176JZFS  = 'ARM1176JZF-S';
 CPU_DESCRIPTION_CORTEX_A5    = 'ARM Cortex-A5';
 CPU_DESCRIPTION_CORTEX_A5_MP = 'ARM Cortex-A5 MPCore';
 CPU_DESCRIPTION_CORTEX_A7    = 'ARM Cortex-A7 MPCore';
 CPU_DESCRIPTION_CORTEX_A8    = 'ARM Cortex-A8';
 CPU_DESCRIPTION_CORTEX_A9    = 'ARM Cortex-A9'; 
 CPU_DESCRIPTION_CORTEX_A9_MP = 'ARM Cortex-A9 MPCore'; 
 CPU_DESCRIPTION_CORTEX_A15   = 'ARM Cortex-A15 MPCore';
 CPU_DESCRIPTION_CORTEX_A17   = 'ARM Cortex-A17 MPCore'; 
 CPU_DESCRIPTION_CORTEX_A53   = 'ARM Cortex-A53 MPCore'; 
 CPU_DESCRIPTION_CORTEX_A57   = 'ARM Cortex-A57 MPCore'; 
 CPU_DESCRIPTION_CORTEX_A72   = 'ARM Cortex-A72 MPCore'; 
 
const 
 {CPU State constants}
 CPU_STATE_NONE                      = (0 shl 0);
 CPU_STATE_MMU_ENABLED               = (1 shl 0);
 CPU_STATE_DATA_CACHE_ENABLED        = (1 shl 1);
 CPU_STATE_INSTRUCTION_CACHE_ENABLED = (1 shl 2);
 CPU_STATE_BRANCH_PREDICTION_ENABLED = (1 shl 3);
 
const
 {CPU ID constants}
 CPU_ID_0   = 0;
 CPU_ID_1   = 1;
 CPU_ID_2   = 2;
 CPU_ID_3   = 3;
 CPU_ID_4   = 4;
 CPU_ID_5   = 5;
 CPU_ID_6   = 6;
 CPU_ID_7   = 7;
 CPU_ID_8   = 8;
 CPU_ID_9   = 9;
 CPU_ID_10  = 10;
 CPU_ID_11  = 11;
 CPU_ID_12  = 12;
 CPU_ID_13  = 13;
 CPU_ID_14  = 14;
 CPU_ID_15  = 15;
 CPU_ID_16  = 16;
 CPU_ID_17  = 17;
 CPU_ID_18  = 18;
 CPU_ID_19  = 19;
 CPU_ID_20  = 20;
 CPU_ID_21  = 21;
 CPU_ID_22  = 22;
 CPU_ID_23  = 23;
 CPU_ID_24  = 24;
 CPU_ID_25  = 25;
 CPU_ID_26  = 26;
 CPU_ID_27  = 27;
 CPU_ID_28  = 28;
 CPU_ID_29  = 29;
 CPU_ID_30  = 30;
 CPU_ID_31  = 31;
 
 CPU_ID_ALL =  $FFFFFFFF;
 
const
 {CPU Affinity constants}
 CPU_AFFINITY_0  = (1 shl CPU_ID_0);
 CPU_AFFINITY_1  = (1 shl CPU_ID_1);
 CPU_AFFINITY_2  = (1 shl CPU_ID_2);
 CPU_AFFINITY_3  = (1 shl CPU_ID_3);
 CPU_AFFINITY_4  = (1 shl CPU_ID_4);
 CPU_AFFINITY_5  = (1 shl CPU_ID_5);
 CPU_AFFINITY_6  = (1 shl CPU_ID_6);
 CPU_AFFINITY_7  = (1 shl CPU_ID_7);
 CPU_AFFINITY_8  = (1 shl CPU_ID_8);
 CPU_AFFINITY_9  = (1 shl CPU_ID_9);
 CPU_AFFINITY_10 = (1 shl CPU_ID_10);
 CPU_AFFINITY_11 = (1 shl CPU_ID_11);
 CPU_AFFINITY_12 = (1 shl CPU_ID_12);
 CPU_AFFINITY_13 = (1 shl CPU_ID_13);
 CPU_AFFINITY_14 = (1 shl CPU_ID_14);
 CPU_AFFINITY_15 = (1 shl CPU_ID_15);
 CPU_AFFINITY_16 = (1 shl CPU_ID_16);
 CPU_AFFINITY_17 = (1 shl CPU_ID_17);
 CPU_AFFINITY_18 = (1 shl CPU_ID_18);
 CPU_AFFINITY_19 = (1 shl CPU_ID_19);
 CPU_AFFINITY_20 = (1 shl CPU_ID_20);
 CPU_AFFINITY_21 = (1 shl CPU_ID_21);
 CPU_AFFINITY_22 = (1 shl CPU_ID_22);
 CPU_AFFINITY_23 = (1 shl CPU_ID_23);
 CPU_AFFINITY_24 = (1 shl CPU_ID_24);
 CPU_AFFINITY_25 = (1 shl CPU_ID_25);
 CPU_AFFINITY_26 = (1 shl CPU_ID_26);
 CPU_AFFINITY_27 = (1 shl CPU_ID_27);
 CPU_AFFINITY_28 = (1 shl CPU_ID_28);
 CPU_AFFINITY_29 = (1 shl CPU_ID_29);
 CPU_AFFINITY_30 = (1 shl CPU_ID_30);
 CPU_AFFINITY_31 = (1 shl CPU_ID_31);
 
 CPU_AFFINITY_NONE = $00000000;
 CPU_AFFINITY_ALL  = $FFFFFFFF;
 
{==============================================================================}
const
 {FPU Type constants}
 FPU_TYPE_UNKNOWN  = 0;
 FPU_TYPE_SOFT     = 1;
 FPU_TYPE_VFPV2    = 2;
 FPU_TYPE_VFPV3    = 3;
 FPU_TYPE_VFPV4    = 4;
 
const 
 {FPU State constants}
 FPU_STATE_NONE    = (0 shl 0);
 FPU_STATE_ENABLED = (1 shl 0);

{==============================================================================}
const
 {GPU Type constants}
 GPU_TYPE_UNKNOWN  = 0;
 GPU_TYPE_VC4      = 1; {Broadcom VideoCore IV} 
 GPU_TYPE_MALI400  = 2; {ARM Mali 400}
 GPU_TYPE_MALI450  = 3; {ARM Mali 450}
 GPU_TYPE_GC880    = 4; {Vivante GC880}
 GPU_TYPE_GC2000   = 5; {Vivante GC2000}
 
const 
 {GPU State constants}
 GPU_STATE_NONE    = (0 shl 0);
 GPU_STATE_ENABLED = (1 shl 0);
 
{==============================================================================}
const
 {Cache Type constants}
 CACHE_TYPE_NONE        = 0; {No Cache}
 CACHE_TYPE_DATA        = 1; {Data Cache Only}
 CACHE_TYPE_INSTRUCTION = 2; {Instruction Cache Only}
 CACHE_TYPE_SEPARATE    = 3; {Separate Data and Instruction Caches}
 CACHE_TYPE_UNIFIED     = 4; {Unified Data and Instruction Cache}
 
{==============================================================================}
const
 {DMA Direction constants}
 DMA_DIR_NONE       = 0; {No direction (No special handling by controller)}
 DMA_DIR_MEM_TO_MEM = 1;
 DMA_DIR_MEM_TO_DEV = 2;
 DMA_DIR_DEV_TO_MEM = 3;
 DMA_DIR_DEV_TO_DEV = 4;
 
 {DMA DREQ ID constants}
 DMA_DREQ_ID_NONE          =  0;  {No peripheral gating (memory to memory transfer)}
 DMA_DREQ_ID_UART_TX       =  1;
 DMA_DREQ_ID_UART_RX       =  2;
 DMA_DREQ_ID_SPI_TX        =  3;
 DMA_DREQ_ID_SPI_RX        =  4;
 DMA_DREQ_ID_SPI_SLAVE_TX  =  5;
 DMA_DREQ_ID_SPI_SLAVE_RX  =  6;
 DMA_DREQ_ID_PCM_TX        =  7;
 DMA_DREQ_ID_PCM_RX        =  8;
 DMA_DREQ_ID_PWM           =  9;
 DMA_DREQ_ID_MMC           =  10;
 DMA_DREQ_ID_SDHOST        =  11;
 
{==============================================================================}
const
 {GPIO Pin constants}
 GPIO_PIN_0   =  0;
 GPIO_PIN_1   =  1;
 GPIO_PIN_2   =  2;
 GPIO_PIN_3   =  3;
 GPIO_PIN_4   =  4;
 GPIO_PIN_5   =  5;
 GPIO_PIN_6   =  6;
 GPIO_PIN_7   =  7;
 GPIO_PIN_8   =  8;
 GPIO_PIN_9   =  9;
 GPIO_PIN_10  =  10;
 GPIO_PIN_11  =  11;
 GPIO_PIN_12  =  12;
 GPIO_PIN_13  =  13;
 GPIO_PIN_14  =  14;
 GPIO_PIN_15  =  15;
 GPIO_PIN_16  =  16;
 GPIO_PIN_17  =  17;
 GPIO_PIN_18  =  18;
 GPIO_PIN_19  =  19;
 GPIO_PIN_20  =  20;
 GPIO_PIN_21  =  21;
 GPIO_PIN_22  =  22;
 GPIO_PIN_23  =  23;
 GPIO_PIN_24  =  24;
 GPIO_PIN_25  =  25;
 GPIO_PIN_26  =  26;
 GPIO_PIN_27  =  27;
 GPIO_PIN_28  =  28;
 GPIO_PIN_29  =  29;
 GPIO_PIN_30  =  30;
 GPIO_PIN_31  =  31;
 GPIO_PIN_32  =  32;
 GPIO_PIN_33  =  33;
 GPIO_PIN_34  =  34;
 GPIO_PIN_35  =  35;
 GPIO_PIN_36  =  36;
 GPIO_PIN_37  =  37;
 GPIO_PIN_38  =  38;
 GPIO_PIN_39  =  39;
 GPIO_PIN_40  =  40;
 GPIO_PIN_41  =  41;
 GPIO_PIN_42  =  42;
 GPIO_PIN_43  =  43;
 GPIO_PIN_44  =  44;
 GPIO_PIN_45  =  45;
 GPIO_PIN_46  =  46;
 GPIO_PIN_47  =  47;
 GPIO_PIN_48  =  48;
 GPIO_PIN_49  =  49;
 GPIO_PIN_50  =  50;
 GPIO_PIN_51  =  51;
 GPIO_PIN_52  =  52;
 GPIO_PIN_53  =  53;
 GPIO_PIN_54  =  54;
 GPIO_PIN_55  =  55;
 GPIO_PIN_56  =  56;
 GPIO_PIN_57  =  57;
 GPIO_PIN_58  =  58;
 GPIO_PIN_59  =  59;
 GPIO_PIN_60  =  60;
 
 GPIO_PIN_MAX =  60;
 
 GPIO_PIN_UNKNOWN = LongWord(-1); 
 
const
 {GPIO Function constants}
 GPIO_FUNCTION_IN    =  0;
 GPIO_FUNCTION_OUT   =  1;
 GPIO_FUNCTION_ALT0  =  2;
 GPIO_FUNCTION_ALT1  =  3;
 GPIO_FUNCTION_ALT2  =  4;
 GPIO_FUNCTION_ALT3  =  5;
 GPIO_FUNCTION_ALT4  =  6;
 GPIO_FUNCTION_ALT5  =  7;
 
 GPIO_FUNCTION_UNKNOWN = LongWord(-1); {Returned by GPIOFunctionGet on error (eg device does not support reading the function state)}
 
const
 {GPIO Level constants}
 GPIO_LEVEL_LOW  =  0;
 GPIO_LEVEL_HIGH =  1;

 GPIO_LEVEL_UNKNOWN = LongWord(-1); {Returned by GPIOInputGet/Wait on error (eg device does not exist)}
 
const
 {GPIO Pull constants}
 GPIO_PULL_NONE  =  0;
 GPIO_PULL_UP    =  1;
 GPIO_PULL_DOWN  =  2;
 
 GPIO_PULL_UNKNOWN = LongWord(-1); {Returned by GPIOPullGet on error (eg device does not support reading the PullUp/Down state)}
 
const 
 {GPIO Trigger constants}
 GPIO_TRIGGER_NONE          = 0;
 GPIO_TRIGGER_LOW           = 1;
 GPIO_TRIGGER_HIGH          = 2;
 GPIO_TRIGGER_RISING        = 3;
 GPIO_TRIGGER_FALLING       = 4;
 GPIO_TRIGGER_ASYNC_RISING  = 5;
 GPIO_TRIGGER_ASYNC_FALLING = 6;
 GPIO_TRIGGER_EDGE          = 7;
 
 GPIO_TRIGGER_UNKNOWN       = LongWord(-1); {Passed to GPIO callback event when device does not support determining the trigger source}
 
{==============================================================================}
const
 {Virtual GPIO Pin constants}
 VIRTUAL_GPIO_PIN_0  =  0;
 VIRTUAL_GPIO_PIN_1  =  1;
 
const
 {Virtual GPIO Function constants}
 VIRTUAL_GPIO_FUNCTION_IN  =  0;
 VIRTUAL_GPIO_FUNCTION_OUT =  1;
 
{==============================================================================}
const
 {Serial Baud Rate constants}
 SERIAL_BAUD_RATE_DEFAULT = 0;
 
 SERIAL_BAUD_RATE_STANDARD = 115200;  {If SERIAL_BAUD_RATE_DEFAULT is passed to SerialOpen then this is the baud rate to use}
 SERIAL_BAUD_RATE_FALLBACK = 9600;    {The fallback baud rate if SERIAL_BAUD_RATE_STANDARD is not supported by the device}
 
 {Serial Data bit constants}
 SERIAL_DATA_8BIT = 8; 
 SERIAL_DATA_7BIT = 7;
 SERIAL_DATA_6BIT = 6;
 SERIAL_DATA_5BIT = 5;

 {Serial Stop bit constants}
 SERIAL_STOP_1BIT  = 1;
 SERIAL_STOP_2BIT  = 2;
 SERIAL_STOP_1BIT5 = 3;  {1.5 Stop bits}
 
 {Serial Parity constants}
 SERIAL_PARITY_NONE  = 0;
 SERIAL_PARITY_ODD   = 1;
 SERIAL_PARITY_EVEN  = 2;
 SERIAL_PARITY_MARK  = 3;
 SERIAL_PARITY_SPACE = 4;
  
 {Serial Flow Control constants}
 SERIAL_FLOW_NONE    = 0;
 SERIAL_FLOW_RTS_CTS = 1;
 SERIAL_FLOW_DSR_DTR = 2;
 
{==============================================================================}
const
 {Power ID constants}
 POWER_ID_MMC0   = 0;
 POWER_ID_MMC1   = 1;
 POWER_ID_MMC2   = 2;
 POWER_ID_MMC3   = 3;
 POWER_ID_UART0  = 4;
 POWER_ID_UART1  = 5;
 POWER_ID_UART2  = 6;
 POWER_ID_UART3  = 7;
 POWER_ID_USB0   = 8;
 POWER_ID_USB1   = 9;
 POWER_ID_USB2   = 10;
 POWER_ID_USB3   = 11;
 POWER_ID_I2C0   = 12;
 POWER_ID_I2C1   = 13;
 POWER_ID_I2C2   = 14;
 POWER_ID_I2C3   = 15;
 POWER_ID_SPI0   = 16;
 POWER_ID_SPI1   = 17;
 POWER_ID_SPI2   = 18;
 POWER_ID_SPI3   = 19;
 POWER_ID_CCP2TX = 20;
 
const
 {Power State constants}
 POWER_STATE_OFF   = 0;
 POWER_STATE_ON    = 1;
 
{==============================================================================}
const
 {Clock ID constants}
 CLOCK_ID_MMC0    = 0;
 CLOCK_ID_MMC1    = 1;
 CLOCK_ID_MMC2    = 2;
 CLOCK_ID_MMC3    = 3;
 CLOCK_ID_UART0   = 4;
 CLOCK_ID_UART1   = 5;
 CLOCK_ID_UART2   = 6;
 CLOCK_ID_UART3   = 7;
 CLOCK_ID_CPU     = 8;
 CLOCK_ID_CORE    = 9;
 CLOCK_ID_GPU     = 10;
 CLOCK_ID_V3D     = 11;
 CLOCK_ID_H264    = 12;
 CLOCK_ID_ISP     = 13;
 CLOCK_ID_SDRAM   = 14;
 CLOCK_ID_PIXEL   = 15;
 CLOCK_ID_PWM     = 16;
 
const
 {Clock State constants}
 CLOCK_STATE_OFF   = 0;
 CLOCK_STATE_ON    = 1;
 
{==============================================================================}
const
 {Turbo ID constants}
 TURBO_ID_SOC  = 0;
 
{==============================================================================}
const
 {Voltage ID constants}
 VOLTAGE_ID_CORE     = 0;
 VOLTAGE_ID_SDRAM_C  = 1;
 VOLTAGE_ID_SDRAM_P  = 2;
 VOLTAGE_ID_SDRAM_I  = 3;
 
{==============================================================================}
const
 {Temperature ID constants}
 TEMPERATURE_ID_SOC  = 0;

{==============================================================================}
const
 {Console Direction constants}
 CONSOLE_DIRECTION_UP    = 0;  {Scroll Console Up}
 CONSOLE_DIRECTION_DOWN  = 1;  {Scroll Console Down}
 CONSOLE_DIRECTION_LEFT  = 2;  {Scroll Console Left}
 CONSOLE_DIRECTION_RIGHT = 3;  {Scroll Console Right}
 
const
 {Console Position constants}
 CONSOLE_POSITION_FULL        = 0;  {Console Window will appear in the full console}
 CONSOLE_POSITION_TOP         = 1;  {Console Window will appear in the top half of the console}
 CONSOLE_POSITION_BOTTOM      = 2;  {Console Window will appear in the bottom half of the console}
 CONSOLE_POSITION_LEFT        = 3;  {Console Window will appear in the left half of the console}
 CONSOLE_POSITION_RIGHT       = 4;  {Console Window will appear in the right half of the console}
 CONSOLE_POSITION_TOPLEFT     = 5;  {Console Window will appear in the top left corner of the console}
 CONSOLE_POSITION_TOPRIGHT    = 6;  {Console Window will appear in the top right corner of the console}
 CONSOLE_POSITION_BOTTOMLEFT  = 7;  {Console Window will appear in the bottom left corner of the console}
 CONSOLE_POSITION_BOTTOMRIGHT = 8;  {Console Window will appear in the bottom right corner of the console}
 
{==============================================================================}
const
 {Framebuffer Depth constants}
 FRAMEBUFFER_DEPTH_8  = 8;
 FRAMEBUFFER_DEPTH_16 = 16;
 FRAMEBUFFER_DEPTH_24 = 24;
 FRAMEBUFFER_DEPTH_32 = 32;
 
const
 {Framebuffer Pixel Order constants}
 FRAMEBUFFER_ORDER_BGR = 0;
 FRAMEBUFFER_ORDER_RGB = 1;
 
const
 {Framebuffer Alpha Mode constants}
 FRAMEBUFFER_MODE_ENABLED  = 0;  {Alpha channel enabled (0 = Fully opaque)}
 FRAMEBUFFER_MODE_REVERSED = 1;  {Alpha channel reversed (0 = Fully transparent)}
 FRAMEBUFFER_MODE_IGNORED  = 2;  {Alpha channel ignored}
 
{==============================================================================}
const
 {Log Level constants}
 LOG_LEVEL_DEBUG     = 1;  {Debugging messages}
 LOG_LEVEL_INFO      = 2;  {Informational messages}
 LOG_LEVEL_ERROR     = 3;  {Error messages}
 LOG_LEVEL_NONE      = 4;  {No messages}
 
const
 {Logging Protocol constants}
 LOGGING_PROTOCOL_UDP = 0;
 LOGGING_PROTOCOL_TCP = 1;
 
const
 {Logging Facility constants}
 LOGGING_FACILITY_KERNEL     = 0;  {Core "kernel" log messages}
 LOGGING_FACILITY_PLATFORM   = 1;  {Platform log messages}
 LOGGING_FACILITY_THREADS    = 2;  {Thread log messages}
 LOGGING_FACILITY_DEVICES    = 3;  {Device log messages}
 LOGGING_FACILITY_NETWORK    = 4;  {Network log messages}
 LOGGING_FACILITY_STORAGE    = 5;  {Storage log messages}
 LOGGING_FACILITY_FILESYSTEM = 6;  {Filesystem log messages}
 LOGGING_FACILITY_KEYBOARD   = 7;  {Keyboard log messages}
 LOGGING_FACILITY_MOUSE      = 8;  {Mouse log messages}
 LOGGING_FACILITY_SCSI       = 9;  {SCSI log messages}
 LOGGING_FACILITY_DMA        = 10; {DMA log messages}
 LOGGING_FACILITY_GPIO       = 11; {GPIO log messages}
 LOGGING_FACILITY_MMC        = 12; {MMC/SD log messages}
 LOGGING_FACILITY_USB        = 13; {USB log messages}
 LOGGING_FACILITY_SERVICES   = 14; {Services log messages}
 LOGGING_FACILITY_HTTP       = 15; {HTTP log messages}
 LOGGING_FACILITY_IMAP       = 16; {IMAP4 log messages}
 LOGGING_FACILITY_POP        = 17; {POP3 log messages}
 LOGGING_FACILITY_SMTP       = 18; {SMTP log messages}
 LOGGING_FACILITY_TELNET     = 19; {Telnet log messages}
 LOGGING_FACILITY_SSH        = 20; {SSH log messages}
 LOGGING_FACILITY_SHELL      = 21; {Shell log messages}
 LOGGING_FACILITY_NTP        = 22; {NTP log messages}
 LOGGING_FACILITY_FTP        = 23; {FTP log messages}
 LOGGING_FACILITY_RTC        = 24; {RTC log messages}
 LOGGING_FACILITY_I2C        = 25; {I2C log messages}
 LOGGING_FACILITY_I2S        = 26; {I2S log messages}
 LOGGING_FACILITY_PWM        = 27; {PWM log messages}
 LOGGING_FACILITY_SERIAL     = 28; {Serial log messages}
 LOGGING_FACILITY_SPI        = 29; {SPI log messages}
 LOGGING_FACILITY_UART       = 30; {UART log messages}
 
 LOGGING_FACILITY_USER       = 1000; {User log messages}

 LOGGING_FACILITY_INVALID    = $FFFFFFFF;
 
const
 {Logging Severity constants}
 LOGGING_SEVERITY_ERROR = 0;    {Error log messages}
 LOGGING_SEVERITY_INFO  = 1;    {Informational log messages}
 LOGGING_SEVERITY_DEBUG = 2;    {Debugging log messages}
 
 LOGGING_SEVERITY_INVALID    = $FFFFFFFF;
 
{==============================================================================}
const
 {Handle Type constants}
 HANDLE_TYPE_SPIN         = 1;
 HANDLE_TYPE_MUTEX        = 2;
 HANDLE_TYPE_SECTION      = 3;
 HANDLE_TYPE_SEMAPHORE    = 4;
 HANDLE_TYPE_SYNCHRONIZER = 5;
 HANDLE_TYPE_LIST         = 6;
 HANDLE_TYPE_QUEUE        = 7;
 HANDLE_TYPE_THREAD       = 8;
 HANDLE_TYPE_MESSAGESLOT  = 9;
 HANDLE_TYPE_MAILSLOT     = 10;
 HANDLE_TYPE_BUFFER       = 11;
 HANDLE_TYPE_EVENT        = 12;

 HANDLE_TYPE_TIMER        = 13;
 HANDLE_TYPE_WORKER       = 14;
 HANDLE_TYPE_WINDOW       = 15;
 HANDLE_TYPE_FONT         = 16;
 HANDLE_TYPE_KEYMAP       = 17;

{==============================================================================}
const
 {Filesystem Cache Mode constants}
 FILESYS_CACHE_MODE_NONE      = 0;
 FILESYS_CACHE_MODE_READONLY  = 1;
 FILESYS_CACHE_MODE_READWRITE = 2;
 
{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}

end.
