{
Ultibo Broadcom VideoCoreIV VCHIQ driver unit.

Copyright (C) 2019 - SoftOz Pty Ltd.

Arch
====

 ARMv6 (ARM1176)
 ARMv7 (Cortex A7)
 ARMv8 (Cortex A53)

Boards
======

 Raspberry Pi - Model A/B/A+/B+/CM1
 Raspberry Pi - Model Zero/ZeroW
 Raspberry Pi 2 - Model B
 Raspberry Pi 3 - Model B/B+/A+
 Raspberry Pi CM3/CM3+
 Raspberry Pi 4 - Model B
 Raspberry Pi 400
 Raspberry Pi CM4
 
Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)
 
Credits
=======

 Information for this unit was obtained from:

  Linux - \drivers\staging\vc04_services\interface\vchiq_arm\* - Copyright (c) 2010-2012 Broadcom
          (Initial port from kernel rpi-4.9.y)
          
  Linux - \drivers\char\broadcom\vc_sm\* - Copyright 2011-2012 Broadcom Corporation

  Linux - \drivers\char\broadcom\vc_mem.c - Copyright 2010 - 2011 Broadcom Corporation
  
References
==========

 VideoCore APIs - http://elinux.org/Raspberry_Pi_VideoCore_APIs
 
 Raspberry Pi Userland - https://github.com/raspberrypi/userland

 VideoCore IV Documentation - https://docs.broadcom.com/docs/12358545
 
 Windows 10 IoT Userland - https://github.com/ms-iot/userland
 
 Windows 10 IoT VCHIQ - https://github.com/ms-iot/bsp/tree/master/drivers/misc/vchiq
 
VideoCore IV VCHIQ
==================
 
 VCHIQ Driver
 ------------
 
 VCHIQ is the VideoCore Host Interface driver which implements a messaging interface 
 between the host running on the ARM CPU and the VC4 GPU firmware.
 
 The format and content of the messages that are passed is not defined by this driver
 but can be found throughout the various libraries that make up the userland interface
 and ultimately expose standard interfaces such as OpenGL ES, OpenVG and OpenMAX IL.
 
 There is no specific documentation for the VCHIQ messaging protocol or behaviour and
 all of the information for this driver has been obtained directly from the equivalent 
 Linux driver. The protocol basically uses a series of memory based slots that form
 a circular buffer, both the master (GPU) and slave (CPU) side have their own set of
 slots which they write data into whenever a new message is to be sent. The other side
 then receives messages from those slots and signals when processing has completed so
 that the slot can be freed.
 
 All requests from higher level interfaces are received in the form of IO control requests
 which are then formed into VCHIQ messages and submitted. The driver uses multiple threads
 to perform the message passing process, the primary ones being a slot handler thread and
 a slot recycle thread.
 
 Because the VideoCoreIV firmware for the Raspberry Pi is under active development and
 future revisions may change the details of the communications between the ARM CPU and
 the VC4 GPU, this driver has been written to retain many of the structures used by the
 original Linux driver and allow easier updating in response to changes. Because of this
 the driver may look unusual in relation to other Pascal code and other Ultibo drivers
 however the functionality is as required in order to perform the task.
 
 Note: The meaning and purpose of some values in this driver are unknown, the values
 are ported directly from the Linux driver and no other source of documentation exists.
 
 Not all of the defined constants and types are required for this driver, some are made
 available as imports for the VC4 unit because they are identical to the definitions of 
 the interface used by applications to communicate with VCHI or VCHIQ services using
 the libvchiq_arm interface.
 
 Implementing VCHIQ in 64-bit
 ----------------------------
 
 Because the implementation of VCHIQ shares multiple structures between the ARM and the VC4
 it is essential that the sizes of those structures do not change when compiling this driver
 for 64-bit. Of particular interest are things that refer to a memory address (a pointer)
 or things that are otherwise adjusted in size when changing between 32 and 64-bit.
 
 One item of note in the Ultibo implementation is the THandle which is defined as LongInt
 in 32-bit but as Int64 in 64-bit. Some structures such as REMOTE_EVENT_T contain a
 TSemaphoreHandle which will need to be accounted for in the change. Potentially some
 form of table needs to be maintained in order to translate between a value and a 
 handle in order for the structure member to remain the correct size.
 
 At present this driver will not function correctly in 64-bit, however the userland
 library will also not function in 64-bit yet either. The same is true for VCHIQ and
 userland on the official Raspbian distribution.
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit VC4VCHIQ;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,HeapManager,Devices,CTypes,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

//To Do //Some functions need better parameter checking
//To Do //Normalize structure and member naming where relevant and field types (eg Char -> Byte or Boolean depending on purpose)
//To Do //Some instances of SemaphoreWait, SpinLock and MutexLock should have error checking

{==============================================================================}
{Local definitions}
{$DEFINE VCHIQ_ENABLE_STATS}
{$DEFINE VCHIQ_ENABLE_DEBUG} {Note: For all current firmware versions, debug must be enabled}

{==============================================================================}
const
 {VCHIQ specific constants}
 VCHIQ_DESCRIPTION = 'Broadcom VideoCore4 VCHIQ';  {Description of VCHIQ device}

 VCHIQ_NAME_PREFIX = 'VCHIQ';

 VCHIQ_SYNC_THREAD_NAME = 'VCHIQ Sync';
 VCHIQ_SYNC_THREAD_STACK_SIZE = SIZE_128K;              {Stack size of sync thread}
 VCHIQ_SYNC_THREAD_PRIORITY = THREAD_PRIORITY_CRITICAL; {Priority of sync thread} 
 
 VCHIQ_RECYCLE_THREAD_NAME = 'VCHIQ Recycle';
 VCHIQ_RECYCLE_THREAD_STACK_SIZE = SIZE_128K;             {Stack size of recycle thread}
 VCHIQ_RECYCLE_THREAD_PRIORITY = THREAD_PRIORITY_HIGHEST; {Priority of recycle thread} 
 
 VCHIQ_SLOT_THREAD_NAME = 'VCHIQ Slot Handler';
 VCHIQ_SLOT_THREAD_STACK_SIZE = SIZE_128K;             {Stack size of slot handler thread}
 VCHIQ_SLOT_THREAD_PRIORITY = THREAD_PRIORITY_HIGHEST; {Priority of slot handler thread} 
 
 VCHIQ_KEEPALIVE_THREAD_NAME = 'VCHIQ Keepalive';
 VCHIQ_KEEPALIVE_THREAD_STACK_SIZE = SIZE_128K;             {Stack size of keepalive thread}
 VCHIQ_KEEPALIVE_THREAD_PRIORITY = THREAD_PRIORITY_NORMAL;  {Priority of keepalive thread} 
 
 {VCHIQ Device Types}
 VCHIQ_TYPE_NONE      = 0;
 
 {VCHIQ Device Flags}
 VCHIQ_FLAG_NONE       = $00000000;
 
 {VCHIQ Doorbell}
 VCHIQ_DOORBELL_REGS_BASE = $B840;
 
 VCHIQ_DOORBELL_BELL0 = $00;
 VCHIQ_DOORBELL_BELL2 = $08;

 VCHIQ_DOORBELL0_IRQ  = 66;  {ARM IRQ 2}
 
const
 {VCHI Interface Constants (From vchi.h)}
 VCHI_SERVICE_HANDLE_INVALID = 0;
 //To Do //More 

 {VCHI Memory Constants (From vchi_mh.h)} 
 VCHI_MEM_HANDLE_INVALID = 0;  
 
 {VCHIQ ARM Constants (From vchiq_arm.h)}
 VCHIQ_MINOR = 0;

 {Per-instance constants (From vchiq_arm.c)}
 VCHIQ_MAX_COMPLETIONS = 128;
 {VCHIQ_MAX_SERVICES = 64;} {Not used by original, per instance conflicts with global value below}
 VCHIQ_MAX_ELEMENTS = 8;
 VCHIQ_MSG_QUEUE_SIZE = 128;

 VCHIQ_KEEPALIVE_VER = 1;
 VCHIQ_KEEPALIVE_VER_MIN = VCHIQ_KEEPALIVE_VER;

 VCHIQ_SUSPEND_TIMER_TIMEOUT_MS = 100;
 VCHIQ_SUSPEND_RETRY_TIMER_TIMEOUT_MS = 1000;

 VCHIQ_SUSPEND_NUM_OFFSET = 3; {Number of values before idle which are negative}
 
 VCHIQ_FORCE_SUSPEND_FAIL_MAX = 8;
 VCHIQ_FORCE_SUSPEND_TIMEOUT_MS = 200;
 
 {VCHIQ Configuration Constants (From vchiq_cfg.h)} 
 VCHIQ_MAGIC               = $56434849; {VCHIQ_MAKE_FOURCC('V', 'C', 'H', 'I')}

 VCHIQ_VERSION             = 8; {The version of VCHIQ - change with any non-trivial change}

 VCHIQ_VERSION_MIN         = 3; {The minimum compatible version - update to match VCHIQ_VERSION with any incompatible change}

 VCHIQ_VERSION_LIB_VERSION = 7; {The version that introduced the VCHIQ_IOC_LIB_VERSION ioctl}

 VCHIQ_VERSION_CLOSE_DELIVERED = 7; {The version that introduced the VCHIQ_IOC_CLOSE_DELIVERED ioctl}

 VCHIQ_VERSION_SYNCHRONOUS_MODE = 8; {The version that made it safe to use SYNCHRONOUS mode}

 VCHIQ_MAX_STATES         = 1; {Note: 1 in VCHIQ driver, 2 in Userland}
 VCHIQ_MAX_SERVICES       = 4096;
 VCHIQ_MAX_SERVICES_BITSET_SIZE = (VCHIQ_MAX_SERVICES + 31) shr 5; {VCHIQ_BITSET_SIZE(VCHIQ_MAX_SERVICES)}
 VCHIQ_MAX_SLOTS          = 128;
 VCHIQ_MAX_SLOTS_PER_SIDE = 64;

 VCHIQ_NUM_CURRENT_BULKS  = 32;
 VCHIQ_NUM_SERVICE_BULKS  = 4;
 
 {VCHIQ Interface Constants (From vchiq_if.h)} 
 VCHIQ_SERVICE_HANDLE_INVALID = 0;

 VCHIQ_SLOT_SIZE    = 4096;
 VCHIQ_HEADER_SIZE  = 8; {SizeOf(VCHIQ_HEADER_T) (Not including data member)}
 VCHIQ_MAX_MSG_SIZE = VCHIQ_SLOT_SIZE - VCHIQ_HEADER_SIZE;
 VCHIQ_CHANNEL_SIZE = VCHIQ_MAX_MSG_SIZE;  {For backwards compatibility}

 {VCHIQ Core Constants (From vchiq_core.h)} 
 VCHIQ_SLOT_MASK       = (VCHIQ_SLOT_SIZE - 1);
 VCHIQ_SLOT_QUEUE_MASK = (VCHIQ_MAX_SLOTS_PER_SIDE - 1);
 VCHIQ_SLOT_ZERO_SIZE  = 1208; {SizeOf(VCHIQ_SLOT_ZERO_T)} 
 VCHIQ_SLOT_ZERO_SLOTS = (VCHIQ_SLOT_ZERO_SIZE + VCHIQ_SLOT_SIZE - 1) div VCHIQ_SLOT_SIZE;
 
 VCHIQ_MSG_PADDING           =  0;  {-                                }
 VCHIQ_MSG_CONNECT           =  1;  {-                                }
 VCHIQ_MSG_OPEN              =  2;  {+ (srcport, -), fourcc, client_id}
 VCHIQ_MSG_OPENACK           =  3;  {+ (srcport, dstport)             }
 VCHIQ_MSG_CLOSE             =  4;  {+ (srcport, dstport)             }
 VCHIQ_MSG_DATA              =  5;  {+ (srcport, dstport)             }
 VCHIQ_MSG_BULK_RX           =  6;  {+ (srcport, dstport), data, size }
 VCHIQ_MSG_BULK_TX           =  7;  {+ (srcport, dstport), data, size }
 VCHIQ_MSG_BULK_RX_DONE      =  8;  {+ (srcport, dstport), actual     }
 VCHIQ_MSG_BULK_TX_DONE      =  9;  {+ (srcport, dstport), actual     }
 VCHIQ_MSG_PAUSE             = 10;  {-                                }
 VCHIQ_MSG_RESUME            = 11;  {-                                }
 VCHIQ_MSG_REMOTE_USE        = 12;  {-                                }
 VCHIQ_MSG_REMOTE_RELEASE    = 13;  {-                                }
 VCHIQ_MSG_REMOTE_USE_ACTIVE = 14;  {-                                }
 
 VCHIQ_PORT_MAX  = (VCHIQ_MAX_SERVICES - 1);
 VCHIQ_PORT_FREE = $1000;
 
 VCHIQ_MSGID_PADDING         = $00000000; {VCHIQ_MAKE_MSG(VCHIQ_MSG_PADDING, 0, 0)}
 VCHIQ_MSGID_CLAIMED         = $40000000;
 
 VCHIQ_FOURCC_INVALID        = $00000000;
 
 VCHIQ_BULK_ACTUAL_ABORTED   = -1;
 
 {VCHIQ Core Constants (From vchiq_core.c)} 
 QMFLAGS_IS_BLOCKING     = (1 shl 0);
 QMFLAGS_NO_MUTEX_LOCK   = (1 shl 1);
 QMFLAGS_NO_MUTEX_UNLOCK = (1 shl 2);
 
 {VCHIQ IO Control Constants (From vchiq_ioctl.h)}
 VCHIQ_IOC_MAGIC = $c4;
 
 VCHIQ_IOC_CONNECT             = $0000C400; {_IO(VCHIQ_IOC_MAGIC,   0)}
 VCHIQ_IOC_SHUTDOWN            = $0000C401; {_IO(VCHIQ_IOC_MAGIC,   1)}
 VCHIQ_IOC_CREATE_SERVICE      = $C01CC402; {_IOWR(VCHIQ_IOC_MAGIC, 2, VCHIQ_CREATE_SERVICE_T)}
 VCHIQ_IOC_REMOVE_SERVICE      = $0000C403; {_IO(VCHIQ_IOC_MAGIC,   3)}
 VCHIQ_IOC_QUEUE_MESSAGE       = $400CC404; {_IOW(VCHIQ_IOC_MAGIC,  4, VCHIQ_QUEUE_MESSAGE_T)}
 VCHIQ_IOC_QUEUE_BULK_TRANSMIT = $C014C405; {_IOWR(VCHIQ_IOC_MAGIC, 5, VCHIQ_QUEUE_BULK_TRANSFER_T)}
 VCHIQ_IOC_QUEUE_BULK_RECEIVE  = $C014C406; {_IOWR(VCHIQ_IOC_MAGIC, 6, VCHIQ_QUEUE_BULK_TRANSFER_T)}
 VCHIQ_IOC_AWAIT_COMPLETION    = $C014C407; {_IOWR(VCHIQ_IOC_MAGIC, 7, VCHIQ_AWAIT_COMPLETION_T)}
 VCHIQ_IOC_DEQUEUE_MESSAGE     = $C010C408; {_IOWR(VCHIQ_IOC_MAGIC, 8, VCHIQ_DEQUEUE_MESSAGE_T)}
 VCHIQ_IOC_GET_CLIENT_ID       = $0000C409; {_IO(VCHIQ_IOC_MAGIC,   9)}
 VCHIQ_IOC_GET_CONFIG          = $C008C40A; {_IOWR(VCHIQ_IOC_MAGIC, 10, VCHIQ_GET_CONFIG_T)}
 VCHIQ_IOC_CLOSE_SERVICE       = $0000C40B; {_IO(VCHIQ_IOC_MAGIC,   11)}
 VCHIQ_IOC_USE_SERVICE         = $0000C40C; {_IO(VCHIQ_IOC_MAGIC,   12)}
 VCHIQ_IOC_RELEASE_SERVICE     = $0000C40D; {_IO(VCHIQ_IOC_MAGIC,   13)}
 VCHIQ_IOC_SET_SERVICE_OPTION  = $400CC40E; {_IOW(VCHIQ_IOC_MAGIC,  14, VCHIQ_SET_SERVICE_OPTION_T)}
 VCHIQ_IOC_DUMP_PHYS_MEM       = $4008C40F; {_IOW(VCHIQ_IOC_MAGIC,  15, VCHIQ_DUMP_MEM_T)}
 VCHIQ_IOC_LIB_VERSION         = $0000C410; {_IO(VCHIQ_IOC_MAGIC,   16)}
 VCHIQ_IOC_CLOSE_DELIVERED     = $0000C411; {_IO(VCHIQ_IOC_MAGIC,   17)}
 VCHIQ_IOC_MAX                 = 17;
 
 {VCHIQ BCM2835 Constants (From vchiq_2835.h)}
 VCHIQ_PLATFORM_FRAGMENTS_OFFSET_IDX = 0;
 VCHIQ_PLATFORM_FRAGMENTS_COUNT_IDX  = 1;
 
 {VCHIQ BCM2835 Constants (From vchiq_2835_arm.c)}
 VCHIQ_TOTAL_SLOTS = (VCHIQ_SLOT_ZERO_SLOTS + 2 * 32);
 
 VCHIQ_MAX_FRAGMENTS = (VCHIQ_NUM_CURRENT_BULKS * 2);
 
 {VCHIQ Kernel Constants (From vchiq_kern_lib.c)}
 VCHIQ_INIT_RETRIES = 10;
 
 {VCHIQ Pagelist Constants (From vchiq_pagelist.h)}
 {VCHIQ_PAGE_SIZE = 4096;} {Not used}
 {VCHIQ_CACHE_LINE_SIZE = 32;} {Not used}
 VCHIQ_PAGELIST_WRITE = 0;
 VCHIQ_PAGELIST_READ = 1;
 VCHIQ_PAGELIST_READ_WITH_FRAGMENTS = 2;
 
{==============================================================================}
type
 {VCHIQ specific types}
 
 {VCHIQ Device}
 PVCHIQDevice = ^TVCHIQDevice;
 PVCHIQInstance = ^TVCHIQInstance;
 PVCHIQ_STATE_T = ^VCHIQ_STATE_T; {Declared early for TVCHIQDevice and VCHIQ_SERVICE_T}
 
 {VCHIQ Device Methods}
 TVCHIQDeviceStart = function(VCHIQ:PVCHIQDevice):LongWord; 
 TVCHIQDeviceStop = function(VCHIQ:PVCHIQDevice):LongWord; 
 
 TVCHIQDeviceOpen = function(VCHIQ:PVCHIQDevice):PVCHIQInstance; 
 TVCHIQDeviceClose = function(VCHIQ:PVCHIQDevice;Instance:PVCHIQInstance):LongWord;
 
 TVCHIQDeviceControl = function(VCHIQ:PVCHIQDevice;Instance:PVCHIQInstance;Code,Argument:LongWord;var Response:LongWord):LongWord;
 
 TVCHIQDevice = record
  {Device Properties}
  Device:TDevice;                                 {The Device entry for this device}
  {VCHIQ Properties}
  DeviceStart:TVCHIQDeviceStart;                  {A Device specific DeviceStart method implementing the VCHIQ device interface (Mandatory)}
  DeviceStop:TVCHIQDeviceStop;                    {A Device specific DeviceStop method implementing the VCHIQ device interface (Mandatory)}
  DeviceOpen:TVCHIQDeviceOpen;                    {A Device specific DeviceOpen method implementing the VCHIQ device interface (Mandatory)}
  DeviceClose:TVCHIQDeviceClose;                  {A Device specific DeviceClose method implementing the VCHIQ device interface (Mandatory)}
  DeviceControl:TVCHIQDeviceControl;              {A Device specific DeviceControl method implementing the VCHIQ device interface (Mandatory)}
  {Driver Properties}
  State:PVCHIQ_STATE_T;                           {State information for the VCHIQ driver}
  //To Do //Instances ?
  Registers:PtrUInt;
  Use36BitAddress:LongBool;
  CacheLineSize:LongWord;
  FragmentsSize:LongWord;
  FragmentsBase:Pointer;
  FreeFragments:Pointer;
  FragmentsMutex:TMutexHandle;
  FragmentsSemaphore:TSemaphoreHandle;
  {Statistics Properties}
  DoorbellInterruptCount:LongWord;
 end;
 
 {$PACKRECORDS C}
 {Internal Types}
 VCHIQ_FOURCC_CHARS = array[0..3] of Byte;

 {VCHI Interface Types (From vchi.h}
 //To Do //More
 
 {VCHI Memory Types (From vchi_mh.h)} 
 VCHI_MEM_HANDLE_T = int32_t;
 
 {VCHIQ ARM Types (From vchiq_arm.h)}
 VC_SUSPEND_STATUS_T = (
  VC_SUSPEND_FORCE_CANCELED = -3, {Force suspend canceled, too busy}
  VC_SUSPEND_REJECTED = -2,       {Videocore rejected suspend request}
  VC_SUSPEND_FAILED = -1,         {Videocore suspend failed}
  VC_SUSPEND_IDLE = 0,            {VC active, no suspend actions}
  VC_SUSPEND_REQUESTED,           {User has requested suspend}
  VC_SUSPEND_IN_PROGRESS,         {Slot handler has recvd suspend request}
  VC_SUSPEND_SUSPENDED            {Videocore suspend succeeded}
 );

 VC_RESUME_STATUS_T = (
  VC_RESUME_FAILED = -1, {Videocore resume failed}
  VC_RESUME_IDLE = 0,    {VC suspended, no resume actions}
  VC_RESUME_REQUESTED,   {User has requested resume}
  VC_RESUME_IN_PROGRESS, {Slot handler has received resume request}
  VC_RESUME_RESUMED      {Videocore resumed successfully (active)}
 );
 
 VC_USE_TYPE_T = (
  USE_TYPE_SERVICE,
  USE_TYPE_SERVICE_NO_RESUME,
  USE_TYPE_VCHIQ
 );
 
 {VCHIQ IO Interface Types (From vchiq_if.h)}
 VCHIQ_REASON_T = (
  VCHIQ_SERVICE_OPENED,          {service, -, -}
  VCHIQ_SERVICE_CLOSED,          {service, -, -}
  VCHIQ_MESSAGE_AVAILABLE,       {service, header, -}
  VCHIQ_BULK_TRANSMIT_DONE,      {service, -, bulk_userdata}
  VCHIQ_BULK_RECEIVE_DONE,       {service, -, bulk_userdata}
  VCHIQ_BULK_TRANSMIT_ABORTED,   {service, -, bulk_userdata}
  VCHIQ_BULK_RECEIVE_ABORTED     {service, -, bulk_userdata}
 );
 
 VCHIQ_STATUS_T = (
  VCHIQ_ERROR   = -1,
  VCHIQ_SUCCESS = 0,
  VCHIQ_RETRY   = 1
 );
 
 VCHIQ_BULK_MODE_T = (
  VCHIQ_BULK_MODE_CALLBACK,
  VCHIQ_BULK_MODE_BLOCKING,
  VCHIQ_BULK_MODE_NOCALLBACK,
  VCHIQ_BULK_MODE_WAITING       {Reserved for internal use}
 );
 
 VCHIQ_SERVICE_OPTION_T = (
  VCHIQ_SERVICE_OPTION_AUTOCLOSE,
  VCHIQ_SERVICE_OPTION_SLOT_QUOTA,
  VCHIQ_SERVICE_OPTION_MESSAGE_QUOTA,
  VCHIQ_SERVICE_OPTION_SYNCHRONOUS,
  VCHIQ_SERVICE_OPTION_TRACE
 );
 
 PPVCHIQ_HEADER_T = ^PVCHIQ_HEADER_T;
 PVCHIQ_HEADER_T = ^VCHIQ_HEADER_T;
 VCHIQ_HEADER_T = record {This structure is passed by or returned to callers via the IOCTL request, it MUST NOT be changed}
  msgid: cint;              {The message identifier (Opaque to applications)}
  size: cunsigned;          {Size of message data}     
  data:array[0..0] of Byte; {Message (Not part of header size)}
 end;
 
 PVCHIQ_ELEMENT_T = ^VCHIQ_ELEMENT_T;
 VCHIQ_ELEMENT_T = record {This structure is passed by or returned to callers via the IOCTL request, it MUST NOT be changed}
  data: Pointer;
  size: cint;
 end;
 
 PVCHIQ_ELEMENT_ARRAY_T = ^VCHIQ_ELEMENT_ARRAY_T;
 VCHIQ_ELEMENT_ARRAY_T = array[0..0] of VCHIQ_ELEMENT_T; {Not part of original driver}
 
 VCHIQ_SERVICE_HANDLE_T = cunsigned;
 PVCHIQ_SERVICE_HANDLE_T = ^VCHIQ_SERVICE_HANDLE_T;
 
 VCHIQ_CALLBACK_T = function(Reason: VCHIQ_REASON_T; Header: PVCHIQ_HEADER_T; Handle: VCHIQ_SERVICE_HANDLE_T; Data: Pointer): VCHIQ_STATUS_T;
 
 PVCHIQ_SERVICE_BASE_T = ^VCHIQ_SERVICE_BASE_T;
 VCHIQ_SERVICE_BASE_T = record
  fourcc: cint;
  callback: VCHIQ_CALLBACK_T;
  userdata: Pointer;
 end;
 
 PVCHIQ_SERVICE_PARAMS_T = ^VCHIQ_SERVICE_PARAMS_T;
 VCHIQ_SERVICE_PARAMS_T = record {This structure is passed by or returned to callers via the IOCTL request, it MUST NOT be changed}
  fourcc: cint;
  callback: VCHIQ_CALLBACK_T;
  userdata: Pointer;
  version: cshort;       {Increment for non-trivial changes}
  version_min: cshort;   {Update for incompatible changes}
 end;
 
 PVCHIQ_CONFIG_T = ^VCHIQ_CONFIG_T;
 VCHIQ_CONFIG_T = record {This structure is passed by or returned to callers via the IOCTL request, it MUST NOT be changed}
  max_msg_size: cint;
  bulk_threshold: cint;  {The message size aboce which it is better to use a bulk transfer (<= max_msg_size)}
  max_outstanding_bulks: cint;
  max_services: cint;
  version: cshort;       {The version of VCHIQ}
  version_min: cshort;   {The minimum compatible version of VCHIQ}
 end;
 
 PVCHIQ_INSTANCE_T = ^VCHIQ_INSTANCE_T;
 VCHIQ_INSTANCE_T = record {Note: The Linux driver version of this is different from the Userland library version, this is the Userland version from vchiq_lib.c}
                           {      The actual struture should be opaque to the caller, the Ultibo driver uses TVCHIQInstance for tracking instance data instead}
                           
  //To Do //See (vchiq_instance_struct): Linux\drivers\staging\vc04_services\interface\vchiq_arm\vchiq_kern_lib.c <- Not needed ? (This is a subset of vchiq_arm.c version)
          //And (vchiq_instance_struct): Linux\drivers\staging\\vc04_services\interface\vchiq_arm\vchiq_arm.c  <-- Use this one for TVCHIQInstance !
          //And (vchiq_instance_struct): Userland\interface\vchiq_arm\vchiq_lib.c <- Use this one for here !
 end;

 VCHIQ_REMOTE_USE_CALLBACK_T = procedure(cb_arg: Pointer);
 
 {VCHIQ Core Types (From vchiq_core.h)} 
 BITSET_T = uint32_t;
 PBITSET_T = puint32_t;

 VCHIQ_DEBUG_T = (
  VCHIQ_DEBUG_ENTRIES,
  {$IFDEF VCHIQ_ENABLE_DEBUG}
  VCHIQ_DEBUG_SLOT_HANDLER_COUNT,
  VCHIQ_DEBUG_SLOT_HANDLER_LINE,
  VCHIQ_DEBUG_PARSE_LINE,
  VCHIQ_DEBUG_PARSE_HEADER,
  VCHIQ_DEBUG_PARSE_MSGID,
  VCHIQ_DEBUG_AWAIT_COMPLETION_LINE,
  VCHIQ_DEBUG_DEQUEUE_MESSAGE_LINE,
  VCHIQ_DEBUG_SERVICE_CALLBACK_LINE,
  VCHIQ_DEBUG_MSG_QUEUE_FULL_COUNT,
  VCHIQ_DEBUG_COMPLETION_QUEUE_FULL_COUNT,
  {$ENDIF}
  VCHIQ_DEBUG_MAX
 );
 
 VCHIQ_CONNSTATE_T = (
  VCHIQ_CONNSTATE_DISCONNECTED,
  VCHIQ_CONNSTATE_CONNECTING,
  VCHIQ_CONNSTATE_CONNECTED,
  VCHIQ_CONNSTATE_PAUSING,
  VCHIQ_CONNSTATE_PAUSE_SENT,
  VCHIQ_CONNSTATE_PAUSED,
  VCHIQ_CONNSTATE_RESUMING,
  VCHIQ_CONNSTATE_PAUSE_TIMEOUT,
  VCHIQ_CONNSTATE_RESUME_TIMEOUT
 );
 
 VCHIQ_SRVSTATE_T = ( 
  VCHIQ_SRVSTATE_FREE,
  VCHIQ_SRVSTATE_HIDDEN,
  VCHIQ_SRVSTATE_LISTENING,
  VCHIQ_SRVSTATE_OPENING,
  VCHIQ_SRVSTATE_OPEN,
  VCHIQ_SRVSTATE_OPENSYNC,
  VCHIQ_SRVSTATE_CLOSESENT,
  VCHIQ_SRVSTATE_CLOSERECVD,
  VCHIQ_SRVSTATE_CLOSEWAIT,
  VCHIQ_SRVSTATE_CLOSED
 );   
 
 VCHIQ_POLL_T = (
  VCHIQ_POLL_TERMINATE,
  VCHIQ_POLL_REMOVE,
  VCHIQ_POLL_TXNOTIFY,
  VCHIQ_POLL_RXNOTIFY,
  VCHIQ_POLL_COUNT
 ); 
 
 VCHIQ_BULK_DIR_T = (
  VCHIQ_BULK_TRANSMIT,
  VCHIQ_BULK_RECEIVE
 );
 
 VCHIQ_USERDATA_TERM_T = procedure(userdata: Pointer);
 
 PVCHIQ_BULK_T = ^VCHIQ_BULK_T;
 VCHIQ_BULK_T = record
  mode: VCHIQ_BULK_MODE_T; {cshort}
  dir: VCHIQ_BULK_DIR_T; {cshort}
  userdata: Pointer;
  handle: VCHI_MEM_HANDLE_T;
  data: Pointer;
  size: cint;
  remote_data: Pointer;
  remote_size: cint;
  actual: cint;
 end; 

 PVCHIQ_BULK_QUEUE_T = ^VCHIQ_BULK_QUEUE_T;
 VCHIQ_BULK_QUEUE_T = record
  local_insert: cint;  {Where to insert the next local bulk}
  remote_insert: cint; {Where to insert the next remote bulk (master)}
  process: cint;       {Bulk to transfer next}
  remote_notify: cint; {Bulk to notify the remote client of next (mstr)}
  remove: cint;        {Bulk to notify the local client of, and remove, next}
  bulks: array[0..VCHIQ_NUM_SERVICE_BULKS - 1] of VCHIQ_BULK_T;
 end;
 
 PREMOTE_EVENT_T = ^REMOTE_EVENT_T;
 REMOTE_EVENT_T = record {This structure is shared with the GPU via slot memory, it MUST NOT be changed}
  armed: cint;
  fired: cint;
  event: TSemaphoreHandle;
 end;
 
 PVCHIQ_PLATFORM_STATE_T = ^VCHIQ_PLATFORM_STATE_T;
 VCHIQ_PLATFORM_STATE_T = record {Combined from VCHIQ_2835_ARM_STATE_T and VCHIQ_ARM_STATE_T}
  inited: Boolean; {cint}
  
  {Keepalive-related data}
  ka_thread: TThreadHandle;
  ka_evt: TCompletionHandle;
  ka_use_count: LongInt; {atomic_t}
  ka_use_ack_count: LongInt; {atomic_t}
  ka_release_count: LongInt; {atomic_t}
 
  vc_suspend_complete: TCompletionHandle;
  vc_resume_complete: TCompletionHandle;
 
  susp_res_lock: TSynchronizerHandle;
  vc_suspend_state: VC_SUSPEND_STATUS_T;
  vc_resume_state: VC_RESUME_STATUS_T;
 
  wake_address: cunsigned;
 
  suspend_timer: TTimerHandle;
  suspend_timer_timeout: cint;
  suspend_timer_running: Boolean; {cint}
 
  {Global use count for videocore. This is equal to the sum of the use counts for all services. When this hits zero the videocore suspend procedure will be initiated}
  videocore_use_count: cint;
 
  {Use count to track requests from videocore peer. This use count is not associated with a service, so needs to be tracked separately with the state}
  peer_use_count: cint;
 
  {Flag to indicate whether resume is blocked.  This happens when the ARM is suspending}
  resume_blocker: TCompletionHandle;
  resume_blocked: cint;
  blocked_blocker: TCompletionHandle;
  blocked_count: cint;
 
  autosuspend_override: cint;
 
  {Flag to indicate that the first vchiq connect has made it through. This means that both sides should be fully ready, and we should be able to suspend after this point}
  first_connect: Boolean; {cint}
 
  suspend_start_time: culonglong;
  sleep_start_time: culonglong;
  resume_start_time: culonglong;
  last_wake_time: culonglong;
 end;
          
 {PVCHIQ_STATE_T = ^VCHIQ_STATE_T;} {Declared above for TVCHIQDevice}
 
 PVCHIQ_SLOT_T = ^VCHIQ_SLOT_T;
 VCHIQ_SLOT_T = record
  data: array[0..VCHIQ_SLOT_SIZE - 1] of Byte;
 end;
 
 PVCHIQ_SLOT_ARRAY_T = ^VCHIQ_SLOT_ARRAY_T;
 VCHIQ_SLOT_ARRAY_T = array[0..0] of VCHIQ_SLOT_T; {Not part of original driver}
 
 PVCHIQ_SLOT_INFO_T = ^VCHIQ_SLOT_INFO_T;
 VCHIQ_SLOT_INFO_T = record {This structure is shared with the GPU via slot memory, it MUST NOT be changed}
  use_count: cshort;     {Use two counters rather than one to avoid the need for a mutex}
  release_count: cshort;
 end;
 
 PVCHIQ_SERVICE_STATS_T = ^VCHIQ_SERVICE_STATS_T;
 VCHIQ_SERVICE_STATS_T = record
  quota_stalls: cint;
  slot_stalls: cint;
  bulk_stalls: cint;
  error_count: cint;
  ctrl_tx_count: cint;
  ctrl_rx_count: cint;
  bulk_tx_count: cint;
  bulk_rx_count: cint;
  bulk_aborted_count: cint;
  ctrl_tx_bytes: uint64_t;
  ctrl_rx_bytes: uint64_t;
  bulk_tx_bytes: uint64_t;
  bulk_rx_bytes: uint64_t;
 end;
 
 PPVCHIQ_SERVICE_T = ^PVCHIQ_SERVICE_T;
 PVCHIQ_SERVICE_T = ^VCHIQ_SERVICE_T;
 VCHIQ_SERVICE_T = record
  base: VCHIQ_SERVICE_BASE_T;
  handle: VCHIQ_SERVICE_HANDLE_T ;
  ref_count: cunsigned;
  srvstate: VCHIQ_SRVSTATE_T;
  userdata_term: VCHIQ_USERDATA_TERM_T;
  localport: cunsigned;
  remoteport: cunsigned;
  public_fourcc: cint;
  client_id: cint;
  auto_close: Byte;
  sync: Byte;
  closing: Byte;
  trace: Byte;
  poll_flags: LongInt; {atomic_t}
  version: cshort;
  version_min: cshort;
  peer_version: cshort;
     
  state: PVCHIQ_STATE_T;
  instance: PVCHIQInstance; {PVCHIQ_INSTANCE_T}
     
  service_use_count: cint;
     
  bulk_tx: VCHIQ_BULK_QUEUE_T;
  bulk_rx: VCHIQ_BULK_QUEUE_T;
     
  remove_event: TSemaphoreHandle;
  bulk_remove_event: TSemaphoreHandle;
  bulk_mutex: TMutexHandle;
     
  stats: VCHIQ_SERVICE_STATS_T;
 end;

 PVCHIQ_SERVICE_QUOTA_T = ^VCHIQ_SERVICE_QUOTA_T;
 VCHIQ_SERVICE_QUOTA_T = record
  slot_quota: cushort;             {The quota information is outside VCHIQ_SERVICE_T so that it can be}
  slot_use_count: cushort;         {statically allocated, since for accounting reasons a service's slot}
  message_quota: cushort;          {usage is carried over between users of the same port number}        
  message_use_count: cushort;
  quota_event: TSemaphoreHandle;
  previous_tx_index: cint;
 end;

 PVCHIQ_SHARED_STATE_T = ^VCHIQ_SHARED_STATE_T;
 VCHIQ_SHARED_STATE_T = record {This structure is shared with the GPU via slot memory, it MUST NOT be changed}
  initialised: cint; {A non-zero value here indicates that the content is valid}
  
  slot_first: cint; {The first and last (inclusive) slots allocated to the owner}
  slot_last: cint;
  
  slot_sync: cint; {The slot allocated to synchronous messages from the owner}
  
  trigger: REMOTE_EVENT_T; {Signaling this event indicates that owner's slot handler thread should run}
  
  tx_pos: cint; {Indicates the byte position within the stream where the next message will be written. The least significant bits are an index into the slot. The next bits are the index of the slot in slot_queue}
  
  recycle: REMOTE_EVENT_T; {This event should be signalled when a slot is recycled}
  
  slot_queue_recycle: cint; {The slot_queue index where the next recycled slot will be written}
  
  sync_trigger: REMOTE_EVENT_T; {This event should be signalled when a synchronous message is sent}
  
  sync_release: REMOTE_EVENT_T; {This event should be signalled when a synchronous message has been released}
  
  slot_queue: array[0..VCHIQ_MAX_SLOTS_PER_SIDE - 1] of cint; {A circular buffer of slot indexes}
  
  debug: array[0..VCHIQ_DEBUG_MAX - 1] of cint; {Debugging state}
 end;
 
 PVCHIQ_SLOT_ZERO_T = ^VCHIQ_SLOT_ZERO_T;
 VCHIQ_SLOT_ZERO_T = record {This structure is shared with the GPU via slot memory, it MUST NOT be changed}
  magic: cint;
  version: cshort;
  version_min: cshort;
  slot_zero_size: cint;
  slot_size: cint;
  max_slots: cint;
  max_slots_per_side: cint;
  platform_data: array[0..1] of cint;
  master: VCHIQ_SHARED_STATE_T;
  slave: VCHIQ_SHARED_STATE_T;
  slots: array[0..VCHIQ_MAX_SLOTS - 1] of VCHIQ_SLOT_INFO_T;
 end;
 
 PVCHIQ_STATE_STATS_T = ^VCHIQ_STATE_STATS_T;
 VCHIQ_STATE_STATS_T = record
  slot_stalls: cint;
  data_stalls: cint;
  ctrl_tx_count: cint;
  ctrl_rx_count: cint;
  error_count: cint;
 end;
 
 {PVCHIQ_STATE_T = ^VCHIQ_STATE_T;} {Declared above for TVCHIQDevice and VCHIQ_SERVICE_T}
 VCHIQ_STATE_T = record
  id: cint;
  initialised: Boolean; {cint}
  conn_state: VCHIQ_CONNSTATE_T;
  is_master: Boolean; {cint}
  version_common: cshort;
     
  local: PVCHIQ_SHARED_STATE_T;
  remote: PVCHIQ_SHARED_STATE_T;
  slot_data: PVCHIQ_SLOT_ARRAY_T; {PVCHIQ_SLOT_T}
     
  default_slot_quota: cushort;
  default_message_quota: cushort;
  
  connect: TSemaphoreHandle; {Event indicating connect message received}
  
  mutex: TMutexHandle; {Mutex protecting services}
  instance: PVCHIQ_INSTANCE_T; //To Do //What to do with this ? //Change to a PVCHIQInstance ? //What uses it ?
  
  slot_handler_thread: TThreadHandle; {Processes incoming messages}
  
  recycle_thread: TThreadHandle; {Processes recycled slots}
  
  sync_thread: TThreadHandle; {Processes synchronous messages}
  
  trigger_event: TSemaphoreHandle; {Local implementation of the trigger remote event} //To Do //Not required, actual Semapahore is in local.trigger.event //May be needed for 64-bit handling
  
  recycle_event: TSemaphoreHandle; {Local implementation of the recycle remote event} //To Do //Not required, actual Semapahore is in local.recycle.event //May be needed for 64-bit handling
  
  sync_trigger_event: TSemaphoreHandle; {Local implementation of the sync trigger remote event} //To Do //Not required, actual Semapahore is in local.sync_trigger.event //May be needed for 64-bit handling
  
  sync_release_event: TSemaphoreHandle; {Local implementation of the sync release remote event} //To Do //Not required, actual Semapahore is in local.sync_release.event //May be needed for 64-bit handling
     
  tx_data: PByte;
  rx_data: PByte;
  rx_info: PVCHIQ_SLOT_INFO_T;
     
  slot_mutex: TMutexHandle;
     
  recycle_mutex: TMutexHandle;
     
  sync_mutex: TMutexHandle;
     
  bulk_transfer_mutex: TMutexHandle;
  
  rx_pos: cint; {Indicates the byte position within the stream from where the next message will be read. The least significant bits are an index into the slot. The next bits are the index of the slot in remote.slot_queue}
  
  local_tx_pos: cint; {A cached copy of local.tx_pos. Only write to local.tx_pos, and read from remote.tx_pos}
  
  slot_queue_available: cint; {The slot_queue index of the slot to become available next}
  
  poll_needed: cint; {A flag to indicate if any poll has been requested}
  
  previous_data_index: cint; {Ths index of the previous slot used for data messages}
  
  data_use_count: cushort; {The number of slots occupied by data messages}
  
  data_quota: cushort; {The maximum number of slots to be occupied by data messages}
  
  poll_services: array[0..VCHIQ_MAX_SERVICES_BITSET_SIZE - 1] of LongInt; {atomic_t} {An array of bit sets indicating which services must be polled}
  
  unused_service: cint; {The number of the first unused service}
  
  slot_available_event: TSemaphoreHandle; {Signalled when a free slot becomes available}
     
  slot_remove_event: TSemaphoreHandle; {Signalled when a free data slot becomes available}
  
  data_quota_event: TSemaphoreHandle;
  
  deferred_bulks: cint; {Incremented when there are bulk transfers which cannot be processed whilst paused and must be processed on resume}
     
  stats: VCHIQ_STATE_STATS_T;
     
  services: array[0..VCHIQ_MAX_SERVICES - 1] of PVCHIQ_SERVICE_T;
  service_quotas: array[0..VCHIQ_MAX_SERVICES - 1] of VCHIQ_SERVICE_QUOTA_T;
  slot_info: array[0..VCHIQ_MAX_SLOTS - 1] of VCHIQ_SLOT_INFO_T;
     
  platform_state: PVCHIQ_PLATFORM_STATE_T;
 end;

 PVCHIQ_BULK_WAITER_T = ^VCHIQ_BULK_WAITER_T;
 VCHIQ_BULK_WAITER_T = record
  bulk: PVCHIQ_BULK_T;
  event: TSemaphoreHandle;
  actual: cint;
 end;
 
 {VCHIQ IO Control Types (From vchiq_ioctl.h)}
 PVCHIQ_CREATE_SERVICE_T = ^VCHIQ_CREATE_SERVICE_T;
 VCHIQ_CREATE_SERVICE_T = record {This structure is passed by or returned to callers via the IOCTL request, it MUST NOT be changed}
  params: VCHIQ_SERVICE_PARAMS_T;
  is_open: cint;
  is_vchi: cint;
  handle: cunsigned;       {OUT}
 end;
 
 PVCHIQ_QUEUE_MESSAGE_T = ^VCHIQ_QUEUE_MESSAGE_T;
 VCHIQ_QUEUE_MESSAGE_T = record {This structure is passed by or returned to callers via the IOCTL request, it MUST NOT be changed}
  handle: cunsigned;
  count: cunsigned ;
  elements: PVCHIQ_ELEMENT_ARRAY_T; {PVCHIQ_ELEMENT_T}
 end;

 PVCHIQ_QUEUE_BULK_TRANSFER_T = ^VCHIQ_QUEUE_BULK_TRANSFER_T;
 VCHIQ_QUEUE_BULK_TRANSFER_T = record {This structure is passed by or returned to callers via the IOCTL request, it MUST NOT be changed}
  handle: cunsigned;
  data: Pointer; 
  size: cunsigned;
  userdata: Pointer;
  mode: VCHIQ_BULK_MODE_T ;
 end;
 
 PVCHIQ_COMPLETION_DATA_T = ^VCHIQ_COMPLETION_DATA_T;
 VCHIQ_COMPLETION_DATA_T = record {This structure is passed by or returned to callers via the IOCTL request, it MUST NOT be changed}
  reason: VCHIQ_REASON_T;
  header: PVCHIQ_HEADER_T;
  service_userdata: Pointer;
  bulk_userdata: Pointer;
 end;
 
 PVCHIQ_COMPLETION_DATA_ARRAY_T = ^VCHIQ_COMPLETION_DATA_ARRAY_T;
 VCHIQ_COMPLETION_DATA_ARRAY_T = array[0..0] of VCHIQ_COMPLETION_DATA_T; {Not part of original driver}
 
 PVCHIQ_AWAIT_COMPLETION_T = ^VCHIQ_AWAIT_COMPLETION_T;
 VCHIQ_AWAIT_COMPLETION_T = record {This structure is passed by or returned to callers via the IOCTL request, it MUST NOT be changed}
  count: cunsigned;
  buf: PVCHIQ_COMPLETION_DATA_ARRAY_T; {PVCHIQ_COMPLETION_DATA_T}
  msgbufsize: cunsigned;
  msgbufcount: cunsigned; {IN/OUT}
  msgbufs: PPointer;
 end;

 PVCHIQ_DEQUEUE_MESSAGE_T = ^VCHIQ_DEQUEUE_MESSAGE_T;
 VCHIQ_DEQUEUE_MESSAGE_T = record {This structure is passed by or returned to callers via the IOCTL request, it MUST NOT be changed}
  handle: cunsigned;
  blocking: cint;
  bufsize: cunsigned;
  buf: Pointer;
 end;

 PVCHIQ_GET_CONFIG_T = ^VCHIQ_GET_CONFIG_T;
 VCHIQ_GET_CONFIG_T = record {This structure is passed by or returned to callers via the IOCTL request, it MUST NOT be changed}
  config_size: cunsigned;
  pconfig: PVCHIQ_CONFIG_T;
 end;
 
 PVCHIQ_SET_SERVICE_OPTION_T = ^VCHIQ_SET_SERVICE_OPTION_T;
 VCHIQ_SET_SERVICE_OPTION_T = record {This structure is passed by or returned to callers via the IOCTL request, it MUST NOT be changed}
  handle: cunsigned;
  option: VCHIQ_SERVICE_OPTION_T;
  value: cint;
 end;

 PVCHIQ_DUMP_MEM_T = ^VCHIQ_DUMP_MEM_T;
 VCHIQ_DUMP_MEM_T = record {This structure is passed by or returned to callers via the IOCTL request, it MUST NOT be changed}
  virt_addr: Pointer;
  num_bytes: size_t;
 end;
 
 {VCHIQ Core Types (From vchiq_core.c)} 
 PVCHIQ_OPEN_PAYLOAD = ^VCHIQ_OPEN_PAYLOAD;
 VCHIQ_OPEN_PAYLOAD = record {This structure is shared with the GPU via slot memory, it MUST NOT be changed}
  fourcc: cint;
  client_id: cint;
  version: cshort;
  version_min: cshort;
 end;
 
 PVCHIQ_OPENACK_PAYLOAD = ^VCHIQ_OPENACK_PAYLOAD;
 VCHIQ_OPENACK_PAYLOAD = record {This structure is shared with the GPU via slot memory, it MUST NOT be changed}
  version: cshort;
 end;
 
 PVCHIQ_BULK_PAYLOAD = ^VCHIQ_BULK_PAYLOAD;
 VCHIQ_BULK_PAYLOAD = record
  data: cint; {Note: Not pointer due to size difference for 32/64 bit}
  size: cint; 
 end;
 
 {VCHIQ ARM Types (From vchiq_arm.c)}
 PVCHIQ_USER_SERVICE_T = ^VCHIQ_USER_SERVICE_T;
 VCHIQ_USER_SERVICE_T = record
  service: PVCHIQ_SERVICE_T;
  userdata: Pointer;
  instance: PVCHIQInstance; {PVCHIQ_INSTANCE_T}
  is_vchi: Boolean; {char}
  dequeue_pending: Boolean; {char}
  close_pending: Boolean; {char}
  message_available_pos: cint;
  msg_insert: cint;
  msg_remove: cint;
  insert_event: TSemaphoreHandle;
  remove_event: TSemaphoreHandle;
  close_event: TSemaphoreHandle;
  msg_queue:array[0..VCHIQ_MSG_QUEUE_SIZE - 1] of PVCHIQ_HEADER_T;
 end;
 
 PVCHIQ_BULK_WAITER_NODE_T = ^VCHIQ_BULK_WAITER_NODE_T;
 VCHIQ_BULK_WAITER_NODE_T = record
  bulk_waiter: VCHIQ_BULK_WAITER_T;
  pid: cint; {Thread ID (In the Linux driver this is the Process ID which is the Thread ID)}
  //list: list_head; //To Do //Continuing
 end;
 
 {VCHIQ Pagelist Constants (From vchiq_pagelist.h)}
 PVCHIQ_PAGELIST_T = ^VCHIQ_PAGELIST_T;
 VCHIQ_PAGELIST_T = record
  length: culong;
  pagetype: cushort;
  offset: cushort;
  addrs: array[0..0] of culong; {N.B. 12 LSBs hold the number of following pages at consecutive addresses}
 end;
 
 PVCHIQ_PAGELIST_PAGES = ^VCHIQ_PAGELIST_PAGES;
 VCHIQ_PAGELIST_PAGES = array[0..0] of PtrUInt; {Not part of original driver}
 
 {PVCHIQ_FRAGMENTS_T = ^VCHIQ_FRAGMENTS_T;
 VCHIQ_FRAGMENTS_T = record
  headbuf: array[0..VCHIQ_CACHE_LINE_SIZE - 1] of Byte;
  tailbuf: array[0..VCHIQ_CACHE_LINE_SIZE - 1] of Byte;
 end;} {Not used}
 
 {$PACKRECORDS DEFAULT}
 
 {VCHIQ Instance (Based on vchiq_instance_struct in vchiq_arm.c)}
 TVCHIQInstance = record
  VCHIQ:PVCHIQDevice;
  State:PVCHIQ_STATE_T;
  {Completions and Events}
  Completions:array[0..VCHIQ_MAX_COMPLETIONS] of VCHIQ_COMPLETION_DATA_T;
  CompletionInsert:Integer;
  CompletionRemove:Integer;
  InsertEvent:TSemaphoreHandle;
  RemoveEvent:TSemaphoreHandle;
  CompletionMutex:TMutexHandle;
  
  {Status}
  Connected:Boolean;
  Closing:Boolean;
  PID:Integer; {Process ID (In the Linux driver this is the Thread Group ID which is the Process ID)}
  Mark:Integer; //To Do //Not required ?, used by vchiq_dump_platform_instances in vchiq_arm.c
  UseCloseDelivered:Boolean;
  Trace:Boolean; //To Do //Not required ?, used by vchiq_debugfs.c 
  
  {Bulk Waiter}
  //bulk_waiter_list //To Do //Continuing
  BulkWaiterMutex:TMutexHandle;
 end;

{==============================================================================}
var
 {VCHIQ specific variables}
 VCHIQ_REASON_NAMES:array[Low(VCHIQ_REASON_T)..High(VCHIQ_REASON_T)] of String = (
  'SERVICE_OPENED',
  'SERVICE_CLOSED',
  'MESSAGE_AVAILABLE',
  'BULK_TRANSMIT_DONE',
  'BULK_RECEIVE_DONE',
  'BULK_TRANSMIT_ABORTED',
  'BULK_RECEIVE_ABORTED'
 );
  
 VCHIQ_CONNSTATE_NAMES:array[Low(VCHIQ_CONNSTATE_T)..High(VCHIQ_CONNSTATE_T)] of String = (
  'DISCONNECTED',
  'CONNECTING',
  'CONNECTED',
  'PAUSING',
  'PAUSE_SENT',
  'PAUSED',
  'RESUMING',
  'PAUSE_TIMEOUT',
  'RESUME_TIMEOUT'
 );
 
 VCHIQ_SRVSTATE_NAMES:array[Low(VCHIQ_SRVSTATE_T)..High(VCHIQ_SRVSTATE_T)] of String = (
  'FREE',
  'HIDDEN',
  'LISTENING',
  'OPENING',
  'OPEN',
  'OPENSYNC',
  'CLOSESENT',
  'CLOSERECVD',
  'CLOSEWAIT',
  'CLOSED'
 );   
 
{==============================================================================}
{Initialization Functions}
procedure VCHIQInit;
 
{==============================================================================}
{VCHIQ Mailbox Functions}
function VCHIQEnable(Address:LongWord):LongWord;
 
{==============================================================================}
{VCHIQ Device Functions}
function VCHIQDeviceStart(VCHIQ:PVCHIQDevice):LongWord;
function VCHIQDeviceStop(VCHIQ:PVCHIQDevice):LongWord;
 
function VCHIQDeviceOpen(VCHIQ:PVCHIQDevice):PVCHIQInstance;
function VCHIQDeviceClose(VCHIQ:PVCHIQDevice;Instance:PVCHIQInstance):LongWord;
 
function VCHIQDeviceControl(VCHIQ:PVCHIQDevice;Instance:PVCHIQInstance;Code,Argument:LongWord;var Response:LongWord):LongWord;

{==============================================================================}
{VCHIQ Core Functions}
{Initialization}
function VCHIQInitSlots(MemBase:Pointer;MemSize:LongWord):PVCHIQ_SLOT_ZERO_T;
function VCHIQInitState(VCHIQ:PVCHIQDevice;State:PVCHIQ_STATE_T;SlotZero:PVCHIQ_SLOT_ZERO_T;IsMaster:Boolean):VCHIQ_STATUS_T;

procedure VCHIQInitBulkQueue(Queue:PVCHIQ_BULK_QUEUE_T);

{Service}
function VCHIQCloseService(Handle:VCHIQ_SERVICE_HANDLE_T):VCHIQ_STATUS_T;
function VCHIQRemoveService(Handle:VCHIQ_SERVICE_HANDLE_T):VCHIQ_STATUS_T;

procedure VCHIQMarkServiceClosing(Service:PVCHIQ_SERVICE_T);
function VCHIQMakeServiceCallback(Service:PVCHIQ_SERVICE_T;Reason:VCHIQ_REASON_T;Header:PVCHIQ_HEADER_T;BulkUserdata:Pointer):VCHIQ_STATUS_T;

function VCHIQSetServiceOption(Handle:VCHIQ_SERVICE_HANDLE_T;Option:VCHIQ_SERVICE_OPTION_T;Value:Integer):VCHIQ_STATUS_T;

{Transfer}    
function VCHIQBulkTransfer(Handle:VCHIQ_SERVICE_HANDLE_T;MemHandle:VCHI_MEM_HANDLE_T;Offset:Pointer;Size:Integer;Userdata:Pointer;Mode:VCHIQ_BULK_MODE_T;Dir:VCHIQ_BULK_DIR_T):VCHIQ_STATUS_T;

{State}
function VCHIQPauseInternal(State:PVCHIQ_STATE_T):VCHIQ_STATUS_T;
function VCHIQResumeInternal(State:PVCHIQ_STATE_T):VCHIQ_STATUS_T;

function VCHIQSendRemoteUse(State:PVCHIQ_STATE_T):VCHIQ_STATUS_T;
function VCHIQSendRemoteRelease(State:PVCHIQ_STATE_T):VCHIQ_STATUS_T;
function VCHIQSendRemoteUseActive(State:PVCHIQ_STATE_T):VCHIQ_STATUS_T;

{Instance}
function VCHIQConnectInternal(State:PVCHIQ_STATE_T;Instance:PVCHIQInstance):VCHIQ_STATUS_T;
function VCHIQShutdownInternal(State:PVCHIQ_STATE_T;Instance:PVCHIQInstance):VCHIQ_STATUS_T;

{Config}
function VCHIQGetConfig(Instance:PVCHIQInstance;ConfigSize:Integer;Config:PVCHIQ_CONFIG_T):VCHIQ_STATUS_T;
function VCHIQGetPeerVersion(Handle:VCHIQ_SERVICE_HANDLE_T;PeerVersion:PSmallInt):VCHIQ_STATUS_T;

{Service Internal}
function VCHIQAddServiceInternal(State:PVCHIQ_STATE_T;Params:PVCHIQ_SERVICE_PARAMS_T;ServiceState:VCHIQ_SRVSTATE_T;Instance:PVCHIQInstance;TermProc:VCHIQ_USERDATA_TERM_T):PVCHIQ_SERVICE_T;

function VCHIQOpenServiceInternal(Service:PVCHIQ_SERVICE_T;ClientID:Integer):VCHIQ_STATUS_T;
function VCHIQCloseServiceInternal(Service:PVCHIQ_SERVICE_T;CloseReceived:Boolean):VCHIQ_STATUS_T;

procedure VCHIQMarkServiceClosingInternal(Service:PVCHIQ_SERVICE_T;SlotHandler:Boolean);
procedure VCHIQTerminateServiceInternal(Service:PVCHIQ_SERVICE_T);
procedure VCHIQFreeServiceInternal(Service:PVCHIQ_SERVICE_T);

function VCHIQGetClientID(Handle:VCHIQ_SERVICE_HANDLE_T):Integer;
function VCHIQGetServiceUserData(Handle:VCHIQ_SERVICE_HANDLE_T):Pointer;
function VCHIQGetServiceFourcc(Handle:VCHIQ_SERVICE_HANDLE_T):Integer;

procedure VCHIQLockService(Service:PVCHIQ_SERVICE_T);
procedure VCHIQUnlockService(Service:PVCHIQ_SERVICE_T);

function VCHIQHandleToService(Handle:VCHIQ_SERVICE_HANDLE_T):PVCHIQ_SERVICE_T;

function VCHIQFindServiceByHandle(Handle:VCHIQ_SERVICE_HANDLE_T):PVCHIQ_SERVICE_T;
function VCHIQFindServiceByPort(State:PVCHIQ_STATE_T;LocalPort:Integer):PVCHIQ_SERVICE_T;
function VCHIQFindServiceForInstance(Instance:PVCHIQInstance;Handle:VCHIQ_SERVICE_HANDLE_T):PVCHIQ_SERVICE_T;
function VCHIQFindClosedServiceForInstance(Instance:PVCHIQInstance;Handle:VCHIQ_SERVICE_HANDLE_T):PVCHIQ_SERVICE_T;
    
function VCHIQNextServiceByInstance(State:PVCHIQ_STATE_T;Instance:PVCHIQInstance;var Index:Integer):PVCHIQ_SERVICE_T;

{Other}    
procedure VCHIQRequestPoll(State:PVCHIQ_STATE_T;Service:PVCHIQ_SERVICE_T;PollType:VCHIQ_POLL_T);
function VCHIQReserveSpace(State:PVCHIQ_STATE_T;Space:Integer;IsBlocking:Boolean):PVCHIQ_HEADER_T;

procedure VCHIQDumpState(Context:Pointer;State:PVCHIQ_STATE_T);
procedure VCHIQDumpSharedState(Context:Pointer;State:PVCHIQ_STATE_T;Shared:PVCHIQ_SHARED_STATE_T;Text:PChar);
procedure VCHIQDumpServiceState(Context:Pointer;Service:PVCHIQ_SERVICE_T);

procedure VCHIQSetConnState(State:PVCHIQ_STATE_T;NewState:VCHIQ_CONNSTATE_T); inline;
procedure VCHIQSetServiceState(Service:PVCHIQ_SERVICE_T;NewState:VCHIQ_SRVSTATE_T); inline;
    
{Message}
function VCHIQQueueMessage(Handle:VCHIQ_SERVICE_HANDLE_T;Elements:PVCHIQ_ELEMENT_ARRAY_T;Count:LongWord):VCHIQ_STATUS_T;
procedure VCHIQReleaseMessage(Handle:VCHIQ_SERVICE_HANDLE_T;Header:PVCHIQ_HEADER_T);

{Message Internal}
function VCHIQQueueMessageInternal(State:PVCHIQ_STATE_T;Service:PVCHIQ_SERVICE_T;MsgId:Integer;Elements:PVCHIQ_ELEMENT_ARRAY_T;Count,Size,Flags:Integer):VCHIQ_STATUS_T;
function VCHIQQueueMessageInternalSync(State:PVCHIQ_STATE_T;Service:PVCHIQ_SERVICE_T;MsgId:Integer;Elements:PVCHIQ_ELEMENT_ARRAY_T;Count,Size:Integer;IsBlocking:Boolean):VCHIQ_STATUS_T;

procedure VCHIQReleaseMessageSync(State:PVCHIQ_STATE_T;Header:PVCHIQ_HEADER_T);

{Slot}
procedure VCHIQClaimSlot(Slot:PVCHIQ_SLOT_INFO_T); inline;
procedure VCHIQReleaseSlot(State:PVCHIQ_STATE_T;Slot:PVCHIQ_SLOT_INFO_T;Header:PVCHIQ_HEADER_T;Service:PVCHIQ_SERVICE_T);

{Remote Event}    
procedure VCHIQRemoteEventCreate(Event:PREMOTE_EVENT_T);
procedure VCHIQRemoteEventDestroy(Event:PREMOTE_EVENT_T);
function VCHIQRemoteEventWait(Event:PREMOTE_EVENT_T):Boolean;
procedure VCHIQRemoteEventSignalLocal(Event:PREMOTE_EVENT_T);
procedure VCHIQRemoteEventPoll(Event:PREMOTE_EVENT_T);
procedure VCHIQRemoteEventPollAll(State:PVCHIQ_STATE_T);

{Threads}
function VCHIQSyncExecute(VCHIQ:PVCHIQDevice):PtrInt;
function VCHIQRecycleExecute(VCHIQ:PVCHIQDevice):PtrInt;
function VCHIQSlotHandlerExecute(VCHIQ:PVCHIQDevice):PtrInt;

{==============================================================================}
{VCHIQ Platform Functions}
{Initialization}
function VCHIQPlatformInit(VCHIQ:PVCHIQDevice;State:PVCHIQ_STATE_T):LongWord;

function VCHIQPlatformInitState(State:PVCHIQ_STATE_T):VCHIQ_STATUS_T;

{Service}
function VCHIQCheckService(Service:PVCHIQ_SERVICE_T):VCHIQ_STATUS_T;

function VCHIQUseServiceNoResume(Handle:VCHIQ_SERVICE_HANDLE_T):VCHIQ_STATUS_T;
function VCHIQUseService(Handle:VCHIQ_SERVICE_HANDLE_T):VCHIQ_STATUS_T;
function VCHIQReleaseService(Handle:VCHIQ_SERVICE_HANDLE_T):VCHIQ_STATUS_T;

function VCHIQServiceCallback(Reason:VCHIQ_REASON_T;Header:PVCHIQ_HEADER_T;Handle:VCHIQ_SERVICE_HANDLE_T;BulkUserdata:Pointer):VCHIQ_STATUS_T;

{User Service}
procedure VCHIQUserServiceFree(Userdata:Pointer);

procedure VCHIQCloseDelivered(UserService:PVCHIQ_USER_SERVICE_T);

{Transfer}    
function VCHIQPrepareBulkData(Bulk:PVCHIQ_BULK_T;MemHandle:VCHI_MEM_HANDLE_T;Offset:Pointer;Size:Integer;Dir:VCHIQ_BULK_DIR_T):VCHIQ_STATUS_T;
procedure VCHIQCompleteBulk(Bulk:PVCHIQ_BULK_T);
procedure VCHIQTransferBulk(Bulk:PVCHIQ_BULK_T);

{State}
function VCHIQGetState:PVCHIQ_STATE_T;

function VCHIQUseInternal(State:PVCHIQ_STATE_T;Service:PVCHIQ_SERVICE_T;UseType:VC_USE_TYPE_T):VCHIQ_STATUS_T;
function VCHIQReleaseInternal(State:PVCHIQ_STATE_T;Service:PVCHIQ_SERVICE_T):VCHIQ_STATUS_T;

procedure VCHIQOnRemoteUse(State:PVCHIQ_STATE_T);
procedure VCHIQOnRemoteRelease(State:PVCHIQ_STATE_T);

{Service Internal}
function VCHIQUseServiceInternal(Service:PVCHIQ_SERVICE_T):VCHIQ_STATUS_T;
function VCHIQReleaseServiceInternal(Service:PVCHIQ_SERVICE_T):VCHIQ_STATUS_T;

{Instance}
function VCHIQInstanceGetUseCount(Instance:PVCHIQInstance):Integer;
function VCHIQInstanceGetPID(Instance:PVCHIQInstance):Integer;
function VCHIQInstanceGetTrace(Instance:PVCHIQInstance):Boolean;
procedure VCHIQInstanceSetTrace(Instance:PVCHIQInstance;Trace:Integer);

{Other}
procedure VCHIQDumpPhysicalMemory(VirtAddr:Pointer;NumBytes:LongWord);

procedure VCHIQOnRemoteUseActive(State:PVCHIQ_STATE_T);

procedure VCHIQPlatformConnStateChanged(State:PVCHIQ_STATE_T;OldState,NewState:VCHIQ_CONNSTATE_T);

{Remote Event}    
procedure VCHIQRemoteEventSignal(Event:PREMOTE_EVENT_T);

{Suspend/Resume}
procedure VCHIQCheckSuspend(State:PVCHIQ_STATE_T);
procedure VCHIQPlatformCheckSuspend(State:PVCHIQ_STATE_T);

function VCHIQPlatformSuspend(State:PVCHIQ_STATE_T):VCHIQ_STATUS_T;
function VCHIQPlatformResume(State:PVCHIQ_STATE_T):VCHIQ_STATUS_T;

procedure VCHIQPlatformPaused(State:PVCHIQ_STATE_T);
procedure VCHIQPlatformResumed(State:PVCHIQ_STATE_T);

function VCHIQVideocoreWanted(State:PVCHIQ_STATE_T):Boolean;
function VCHIQPlatformVideocoreWanted(State:PVCHIQ_STATE_T):Boolean;

function VCHIQPlatformUseSuspendTimer:Integer;
procedure VCHIQDumpPlatformUseState(State:PVCHIQ_STATE_T);
procedure VCHIQPlatformHandleTimeout(State:PVCHIQ_STATE_T);

procedure VCHIQSetSuspendState(PlatformState:PVCHIQ_PLATFORM_STATE_T;NewState:VC_SUSPEND_STATUS_T);
procedure VCHIQSetResumeState(PlatformState:PVCHIQ_PLATFORM_STATE_T;NewState:VC_RESUME_STATUS_T);

procedure VCHIQStartSuspendTimer(PlatformState:PVCHIQ_PLATFORM_STATE_T);
procedure VCHIQStopSuspendTimer(PlatformState:PVCHIQ_PLATFORM_STATE_T);

function VCHIQNeedResume(State:PVCHIQ_STATE_T):Boolean;
function VCHIQBlockResume(PlatformState:PVCHIQ_PLATFORM_STATE_T):VCHIQ_STATUS_T;

procedure VCHIQSuspendTimerCallback(State:PVCHIQ_STATE_T);

{Pagelist}
function VCHIQCreatePagelist(Data:Pointer;Count:PtrUInt;PageType:Word;var Pagelist:PVCHIQ_PAGELIST_T):LongWord;
procedure VCHIQFreePagelist(Pagelist:PVCHIQ_PAGELIST_T;Actual:Integer);

{Interrupts}
procedure VCHIQDoorbellInterruptHandler(VCHIQ:PVCHIQDevice);

{Threads}
function VCHIQKeepaliveExecute(State:PVCHIQ_STATE_T):PtrInt;
function VCHIQKeepaliveCallback(Reason:VCHIQ_REASON_T;Header:PVCHIQ_HEADER_T;Handle:VCHIQ_SERVICE_HANDLE_T;Data:Pointer):VCHIQ_STATUS_T;

//To Do //vchiq_arm.c
//vchiq_arm_force_suspend
//etc

{==============================================================================}
{VCHIQ Kernel Functions}
function VCHIQInitialise(var Instance:PVCHIQInstance):VCHIQ_STATUS_T;

function VCHIQConnect(Instance:PVCHIQInstance):VCHIQ_STATUS_T;
function VCHIQShutdown(Instance:PVCHIQInstance):VCHIQ_STATUS_T;

function VCHIQAddService(Instance:PVCHIQInstance;Params:PVCHIQ_SERVICE_PARAMS_T;var Handle:VCHIQ_SERVICE_HANDLE_T):VCHIQ_STATUS_T;
function VCHIQOpenService(Instance:PVCHIQInstance;Params:PVCHIQ_SERVICE_PARAMS_T;var Handle:VCHIQ_SERVICE_HANDLE_T):VCHIQ_STATUS_T;

function VCHIQQueueBulkTransmit(Handle:VCHIQ_SERVICE_HANDLE_T;Data:Pointer;Size:LongWord;Userdata:Pointer):VCHIQ_STATUS_T;
function VCHIQQueueBulkReceive(Handle:VCHIQ_SERVICE_HANDLE_T;Data:Pointer;Size:LongWord;Userdata:Pointer):VCHIQ_STATUS_T;

function VCHIQBulkTransmit(Handle:VCHIQ_SERVICE_HANDLE_T;Data:Pointer;Size:LongWord;Userdata:Pointer;Mode:VCHIQ_BULK_MODE_T):VCHIQ_STATUS_T;
function VCHIQBulkReceive(Handle:VCHIQ_SERVICE_HANDLE_T;Data:Pointer;Size:LongWord;Userdata:Pointer;Mode:VCHIQ_BULK_MODE_T):VCHIQ_STATUS_T;

function VCHIQBlockingBulkTransfer(Handle:VCHIQ_SERVICE_HANDLE_T;Data:Pointer;Size:LongWord;Dir:VCHIQ_BULK_DIR_T):VCHIQ_STATUS_T;

{==============================================================================}
{VCHIQ Shim Functions}
//To Do //vchiq_shim.c
//vchi_initialise
//etc

{==============================================================================}
{VCHIQ Util Functions}
//To Do //vchiq_util.c
//vchiu_queue_init
//etc

{==============================================================================}
{VCHIQ Connected Functions}
//To Do //vchiq_connected.c
//connected_init
//vchiq_add_connected_callback
//vchiq_call_connected_callbacks
//etc

{==============================================================================}
{VCHIQ Helper Functions}
function VCHIQ_MAKE_FOURCC(x0,x1,x2,x3:Char):LongWord; inline;
 
function VCHIQ_PORT_IS_VALID(Port:LongWord):Boolean; inline;

function VCHIQ_MAKE_MSG(MsgType,SrcPort,DstPort:LongWord):LongWord; inline; 

function VCHIQ_MSG_TYPE(MsgId:LongWord):LongWord; inline; 

function VCHIQ_MSG_SRCPORT(MsgId:LongWord):LongWord; inline; 

function VCHIQ_MSG_DSTPORT(MsgId:LongWord):LongWord; inline; 

procedure VCHIQ_FOURCC_AS_4CHARS(Fourcc:LongWord;var chars:VCHIQ_FOURCC_CHARS);

function VCHIQ_FOURCC_IS_LEGAL(Fourcc:LongWord):Boolean; inline;

function VCHIQ_BITSET_SIZE(b:LongWord):LongWord; inline;
function VCHIQ_BITSET_WORD(b:LongWord):LongWord; inline; 
function VCHIQ_BITSET_BIT(b:LongWord):LongWord; inline;
procedure VCHIQ_BITSET_ZERO(bs:PBITSET_T;Size:LongWord); inline;
function VCHIQ_BITSET_IS_SET(bs:PBITSET_T;b:LongWord):Boolean; inline;
procedure VCHIQ_BITSET_SET(bs:PBITSET_T;b:LongWord); inline;
procedure VCHIQ_BITSET_CLR(bs:PBITSET_T;b:LongWord); inline;
 
function VCHIQ_SLOT_INFO_FROM_INDEX(State:PVCHIQ_STATE_T;Index:Integer):PVCHIQ_SLOT_INFO_T;
function VCHIQ_SLOT_DATA_FROM_INDEX(State:PVCHIQ_STATE_T;Index:Integer):PByte;
function VCHIQ_SLOT_INDEX_FROM_DATA(State:PVCHIQ_STATE_T;Data:PByte):Integer;
function VCHIQ_SLOT_INDEX_FROM_INFO(State:PVCHIQ_STATE_T;Info:PVCHIQ_SLOT_INFO_T):Integer;
function VCHIQ_SLOT_QUEUE_INDEX_FROM_POS(Pos:LongWord):Integer;
 
function VCHIQ_BULK_INDEX(x:Integer):Integer; inline;
 
function VCHIQCalcStride(Size:LongWord):LongWord; inline;

function VCHIQFourccToString(Fourcc:LongWord):String;

function VCHIQMessageTypeToString(MsgType:LongWord):String;

function VCHIQPollTypeToString(PollType:VCHIQ_POLL_T):String;

function VCHIQPageTypeToString(PageType:Word):String;

function VCHIQStatusToString(Status:VCHIQ_STATUS_T):String;
function VCHIQBulkDirToString(BulkDir:VCHIQ_BULK_DIR_T):String;
function VCHIQBulkModeToString(BulkMode:VCHIQ_BULK_MODE_T):String;
function VCHIQServiceOptionToString(ServiceOption:VCHIQ_SERVICE_OPTION_T):String;

function VCHIQReasonToString(Reason:VCHIQ_REASON_T):String; inline;
function VCHIQConnStateToString(ConnState:VCHIQ_CONNSTATE_T):String; inline;
function VCHIQServiceStateToString(ServiceState:VCHIQ_SRVSTATE_T):String; inline;
 
function VCHIQSuspendStatusToString(Status:VC_SUSPEND_STATUS_T):String;
function VCHIQResumeStatusToString(Status:VC_RESUME_STATUS_T):String;

{==============================================================================}
{==============================================================================}

implementation
 
uses {$IFDEF CPUARMV6}BCM2835{$ELSE}BCM2837{$ENDIF};

{==============================================================================}
{==============================================================================}
const
 {VCHIQ mailbox constants}
 {$IFDEF CPUARMV6}
 VCHIQ_MBOX_REQUEST_CODE = BCM2835_MBOX_REQUEST_CODE;

 VCHIQ_MBOX_TAG_END = BCM2835_MBOX_TAG_END;
 VCHIQ_MBOX_TAG_VCHIQ_INIT = BCM2835_MBOX_TAG_VCHIQ_INIT;
 
 VCHIQ_MAILBOX_0 = BCM2835_MAILBOX_0;
 VCHIQ_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC = BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC;
 {$ELSE}
 VCHIQ_MBOX_REQUEST_CODE = BCM2837_MBOX_REQUEST_CODE;

 VCHIQ_MBOX_TAG_END = BCM2837_MBOX_TAG_END;
 VCHIQ_MBOX_TAG_VCHIQ_INIT = BCM2837_MBOX_TAG_VCHIQ_INIT;
 
 VCHIQ_MAILBOX_0 = BCM2837_MAILBOX_0;
 VCHIQ_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC = BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC;
 {$ENDIF}
{==============================================================================}
{==============================================================================}
type
 {VCHIQ mailbox types}
 {$IFDEF CPUARMV6}
 TVCHIQMailboxHeader = TBCM2835MailboxHeader;
 PVCHIQMailboxHeader = PBCM2835MailboxHeader;
 
 TVCHIQMailboxFooter = TBCM2835MailboxFooter;
 PVCHIQMailboxFooter = PBCM2835MailboxFooter;
 
 TVCHIQMailboxTagHeader = TBCM2835MailboxTagHeader;
 PVCHIQMailboxTagHeader = PBCM2835MailboxTagHeader;
 
 TVCHIQMailboxTagVCHIQInit = TBCM2835MailboxTagVCHIQInit;
 PVCHIQMailboxTagVCHIQInit = PBCM2835MailboxTagVCHIQInit;
 
 {$ELSE}
 TVCHIQMailboxHeader = TBCM2837MailboxHeader;
 PVCHIQMailboxHeader = PBCM2837MailboxHeader;
 
 TVCHIQMailboxFooter = TBCM2837MailboxFooter;
 PVCHIQMailboxFooter = PBCM2837MailboxFooter;
 
 TVCHIQMailboxTagHeader = TBCM2837MailboxTagHeader;
 PVCHIQMailboxTagHeader = PBCM2837MailboxTagHeader;
 
 TVCHIQMailboxTagVCHIQInit = TBCM2837MailboxTagVCHIQInit;
 PVCHIQMailboxTagVCHIQInit = PBCM2837MailboxTagVCHIQInit;
 {$ENDIF}
{==============================================================================}
{==============================================================================}
var
 {VCHIQ specific variables}
 VCHIQInitialized:Boolean;
 
 VCHIQId:Integer;
 
 VCHIQState:PVCHIQ_STATE_T;       {Global pointer to VCHIQ_STATE_T record, used to keep code compatible with original (Only one instance of VCHIQ driver allowed)}
 VCHIQDevice:PVCHIQDevice;        {Global pointer to existing VCHIQ device (Only one instance of VCHIQ driver allowed)}
 
 VCHIQStates:array[0..VCHIQ_MAX_STATES - 1] of PVCHIQ_STATE_T;
 
 //To Do //These should all move to TVCHIQDevice or to VCHIQ_STATE_T ?
 VCHIQHandleSequence:LongWord;
 VCHIQPauseBulksCount:LongInt; {atomic_t}
 
 VCHIQServiceLock:TSpinHandle = INVALID_HANDLE_VALUE; //To Do //Currently we are using SpinLockIRQ because vchiq_core.c says the SpinLock disables interrupts, need to confirm and determine if that is needed
 VCHIQQuotaLock:TSpinHandle = INVALID_HANDLE_VALUE;           //In Linux spin_lock() calls to __raw_spin_lock() which calls preempt_disable() but does not disable interrupts. preempt_disable() disables kernel preemption which we do by SpinLockPreempt()
 VCHIQBulkWaiterLock:TSpinHandle = INVALID_HANDLE_VALUE;      //but kernel preemption is not such a problem to us so we probably don't need to use IRQ or Preempt
 
 VCHIQMessageQueueLock:TSpinHandle = INVALID_HANDLE_VALUE;
 
{==============================================================================}
{==============================================================================}
{Forward Declarations}
function VCHIQNotifyBulks(Service:PVCHIQ_SERVICE_T;Queue:PVCHIQ_BULK_QUEUE_T;RetryPoll:Boolean):VCHIQ_STATUS_T; forward;
function VCHIQResolveBulks(Service:PVCHIQ_SERVICE_T;Queue:PVCHIQ_BULK_QUEUE_T):Integer; forward;
procedure VCHIQAbortOutstandingBulks(Service:PVCHIQ_SERVICE_T;Queue:PVCHIQ_BULK_QUEUE_T); forward;
 
function VCHIQARMVCSuspend(State:PVCHIQ_STATE_T):VCHIQ_STATUS_T; forward;
 
{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure VCHIQInit;
{Initialize the VCHIQ device}

{Note: Called only during system startup}
var
 Status:LongWord;
 VCHIQ:PVCHIQDevice;
begin
 {}
 {Check Initialized}
 if VCHIQInitialized then Exit;

 {Create Service Lock}
 VCHIQServiceLock:=SpinCreate;
 
 {Create Quota Lock}
 VCHIQQuotaLock:=SpinCreate;
 
 {Create Bulk Waiter Lock}
 VCHIQBulkWaiterLock:=SpinCreate;
 
 {Create Message Queue Lock}
 VCHIQMessageQueueLock:=SpinCreate;
 
 {Create VCHIQ}
 VCHIQ:=PVCHIQDevice(DeviceCreateEx(SizeOf(TVCHIQDevice)));
 if VCHIQ <> nil then
  begin
   {Update VCHIQ}
   {Device}
   VCHIQ.Device.DeviceName:=VCHIQ_NAME_PREFIX + IntToStr(0); {Only one VCHIQ device supported}
   VCHIQ.Device.DeviceClass:=DEVICE_CLASS_GENERIC;
   VCHIQ.Device.DeviceBus:=DEVICE_BUS_MMIO; 
   VCHIQ.Device.DeviceType:=VCHIQ_TYPE_NONE;
   VCHIQ.Device.DeviceFlags:=VCHIQ_FLAG_NONE;
   VCHIQ.Device.DeviceData:=nil;
   VCHIQ.Device.DeviceDescription:=VCHIQ_DESCRIPTION;
   {VCHIQ}
   VCHIQ.DeviceStart:=VCHIQDeviceStart;
   VCHIQ.DeviceStop:=VCHIQDeviceStop;
   VCHIQ.DeviceOpen:=VCHIQDeviceOpen;
   VCHIQ.DeviceClose:=VCHIQDeviceClose;
   VCHIQ.DeviceControl:=VCHIQDeviceControl;
 
   {Register VCHIQ}
   Status:=DeviceRegister(@VCHIQ.Device);
   if Status <> ERROR_SUCCESS then
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Failed to register new VCHIQ device: ' + ErrorToString(Status));
        
     {Destroy VCHIQ}
     DeviceDestroy(@VCHIQ.Device);
    end;
  end
 else 
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Failed to create new VCHIQ device');
  end;

 VCHIQInitialized:=True;
end;
  
{==============================================================================}
{==============================================================================}
{VCHIQ Mailbox Functions}
function VCHIQEnable(Address:LongWord):LongWord;
{Enable the VCHIQ (Master) using the Mailbox property tags channel}
{Address: The bus address of the VCHIQ slots allocated from coherent memory}
{Return: Zero on success or another value on failure}
var
 Size:LongWord;
 Status:LongWord;
 Response:LongWord;
 Header:PVCHIQMailboxHeader;
 Footer:PVCHIQMailboxFooter;
 Tag:PVCHIQMailboxTagVCHIQInit;
begin
 {}
 Result:=LongWord(-1);
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: VCHIQEnable (Address=' + IntToHex(Address,8) + ')');
 {$ENDIF}
 
 {Calculate Size}
 Size:=SizeOf(TVCHIQMailboxHeader) + SizeOf(TVCHIQMailboxTagVCHIQInit) + SizeOf(TVCHIQMailboxFooter);
 
 {Allocate Mailbox Buffer}
 {$IFDEF CPUARMV6}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 {$ELSE}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 {$ENDIF}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);
 
  {Setup Header}
  Header.Size:=Size;
  Header.Code:=VCHIQ_MBOX_REQUEST_CODE;
 
  {Setup Tag}
  Tag:=PVCHIQMailboxTagVCHIQInit(PtrUInt(Header) + PtrUInt(SizeOf(TVCHIQMailboxHeader)));
  Tag.Header.Tag:=VCHIQ_MBOX_TAG_VCHIQ_INIT;
  Tag.Header.Size:=SizeOf(TVCHIQMailboxTagVCHIQInit) - SizeOf(TVCHIQMailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.Address:=Address;
 
  {Setup Footer}
  Footer:=PVCHIQMailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TVCHIQMailboxTagVCHIQInit)));
  Footer.Tag:=VCHIQ_MBOX_TAG_END;
  
  {Call Mailbox}
  Status:=MailboxPropertyCall(VCHIQ_MAILBOX_0,VCHIQ_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
  if Status <> ERROR_SUCCESS then
   begin
    if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: VCHIQEnable - MailboxPropertyCall failed (Status=' + ErrorToString(Status) + ')');
    
    Exit;
   end; 

  {Get Result}
  Result:=Tag.Response.Status;
  
  {$IFDEF VC4VCHIQ_DEBUG}
  if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: VCHIQEnable (Result=' + IntToHex(Result,8) + ')');
  {$ENDIF}
 finally
  FreeMem(Header);
 end;
end;
  
{==============================================================================}
{==============================================================================}
{VCHIQ Device Functions}
function VCHIQDeviceStart(VCHIQ:PVCHIQDevice):LongWord; 
{Start the specificed VCHIQ device, allocate and initialize slots register shared data and enable to master VCHIQ}
{VCHIQ: The VCHIQ device to start}
{Return: ERROR_SUCCESS on completion or another error code on failure}

{Note: There can only be one VCHIQ device active at once, attempting to start a second will return on error}

{Incorporates functionality from vchiq_probe in vchiq_arm.c}
{                       and from vchiq_platform_init in vchiq_2835_arm.c}
var
 Count:Integer;
 SlotMemory:Pointer;
 SlotPhysical:PtrUInt;
 SlotMemSize:Integer;
 FragmentMemSize:Integer;
 SlotZero:PVCHIQ_SLOT_ZERO_T;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Device Start (VCHIQ=' + IntToHex(PtrUInt(VCHIQ),8) + ')');
 {$ENDIF}
 
 {Check VCHIQ}
 if VCHIQ = nil then Exit;
 if VCHIQ.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check State}
 Result:=ERROR_ALREADY_ASSIGNED;
 if VCHIQState <> nil then Exit;
 
 {Check Device}
 if VCHIQDevice <> nil then Exit;
 
 {Setup Properties}
 Result:=ERROR_OPERATION_FAILED;
 VCHIQ.Registers:=PERIPHERALS_BASE + VCHIQ_DOORBELL_REGS_BASE;
 VCHIQ.Use36BitAddress:=False;
 VCHIQ.CacheLineSize:=32;
 case BoardGetType of
  BOARD_TYPE_RPI2B,
  BOARD_TYPE_RPI3B,
  BOARD_TYPE_RPI_COMPUTE3,
  BOARD_TYPE_RPI3B_PLUS,
  BOARD_TYPE_RPI3A_PLUS,
  BOARD_TYPE_RPI_COMPUTE3_PLUS:begin
    if DEVICE_TREE_VALID then VCHIQ.CacheLineSize:=64;
   end;
  BOARD_TYPE_RPI4B,
  BOARD_TYPE_RPI400,
  BOARD_TYPE_RPI_COMPUTE4:begin
    VCHIQ.Use36BitAddress:=True;
    if DEVICE_TREE_VALID then VCHIQ.CacheLineSize:=64;
   end; 
 end;
 VCHIQ.FragmentsSize:=2 * VCHIQ.CacheLineSize;
 VCHIQ.FragmentsMutex:=INVALID_HANDLE_VALUE;
 VCHIQ.FragmentsSemaphore:=INVALID_HANDLE_VALUE;

 {Allocate space for the channels in DMA coherent memory}
 SlotMemSize:=Align(VCHIQ_TOTAL_SLOTS * VCHIQ_SLOT_SIZE,MEMORY_PAGE_SIZE);
 FragmentMemSize:=Align(VCHIQ.FragmentsSize * VCHIQ_MAX_FRAGMENTS,MEMORY_PAGE_SIZE);
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Device Start (SlotMemSize=' + IntToStr(SlotMemSize) + ' FragmentMemSize=' + IntToStr(FragmentMemSize) + ')');
 {$ENDIF}
 
 {$IFDEF CPUARMV6}
 SlotMemory:=AllocSharedAlignedMem(SlotMemSize + FragmentMemSize,MEMORY_PAGE_SIZE);
 {$ELSE}
 SlotMemory:=AllocNoCacheAlignedMem(SlotMemSize + FragmentMemSize,MEMORY_PAGE_SIZE);
 {$ENDIF}
 if SlotMemory = nil then Exit;
 
 SlotPhysical:=PhysicalToBusAddress(SlotMemory);
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Device Start (SlotMemory=' + IntToHex(PtrUInt(SlotMemory),8) + ' SlotPhysical=' + IntToHex(PtrUInt(SlotPhysical),8) + ')');
 {$ENDIF}
 
 {Check Alignment}
 if (PtrUInt(SlotMemory) and (MEMORY_PAGE_SIZE - 1)) <> 0 then
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Device Start: Slot memory not aligned to MEMORY_PAGE_SIZE');
  end;
  
 {Initialize Slots}
 SlotZero:=VCHIQInitSlots(SlotMemory,SlotMemSize);
 if SlotZero = nil then Exit;
 
 SlotZero.platform_data[VCHIQ_PLATFORM_FRAGMENTS_OFFSET_IDX]:=SlotPhysical + SlotMemSize;
 SlotZero.platform_data[VCHIQ_PLATFORM_FRAGMENTS_COUNT_IDX]:=VCHIQ_MAX_FRAGMENTS;
 
 {Initialize Fragments}
 VCHIQ.FragmentsBase:=SlotMemory + SlotMemSize;
 SlotMemSize:=SlotMemSize + FragmentMemSize;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Device Start (FragmentsBase=' + IntToHex(PtrUInt(VCHIQ.FragmentsBase),8) + ')');
 {$ENDIF}
 
 VCHIQ.FreeFragments:=VCHIQ.FragmentsBase;
 Count:=0;
 while Count < VCHIQ_MAX_FRAGMENTS - 1 do
  begin
   PPointer(VCHIQ.FragmentsBase + (Count * VCHIQ.FragmentsSize))^:=Pointer(VCHIQ.FragmentsBase + ((Count + 1) * VCHIQ.FragmentsSize));
 
   Inc(Count);
  end;
 PPointer(VCHIQ.FragmentsBase + (Count * VCHIQ.FragmentsSize))^:=nil;
 VCHIQ.FragmentsMutex:=MutexCreate;
 VCHIQ.FragmentsSemaphore:=SemaphoreCreate(VCHIQ_MAX_FRAGMENTS);
 
 {Allocate State} 
 VCHIQ.State:=GetMem(SizeOf(VCHIQ_STATE_T));
 if VCHIQ.State = nil then
  begin
   {Release Slot Memory}
   FreeMem(SlotMemory);
   Exit;
  end;
  
 {Initialize State}
 if VCHIQInitState(VCHIQ,VCHIQ.State,SlotZero,False) <> VCHIQ_SUCCESS then
  begin
   {Release Platform State}
   if VCHIQ.State.platform_state <> nil then FreeMem(VCHIQ.State.platform_state);
   VCHIQ.State.platform_state:=nil;
   
   {Release State}
   FreeMem(VCHIQ.State);
   VCHIQ.State:=nil;
   
   {Release Slot Memory}
   FreeMem(SlotMemory);
   Exit;
  end;
 
 {Request IRQ}
 RequestIRQ(IRQ_ROUTING,VCHIQ_DOORBELL0_IRQ,TInterruptHandler(VCHIQDoorbellInterruptHandler),VCHIQ);
 
 {Initialize VCHIQ (Master)}
 if VCHIQEnable(SlotPhysical) <> 0 then
  begin
   {Release IRQ}
   ReleaseIRQ(IRQ_ROUTING,VCHIQ_DOORBELL0_IRQ,TInterruptHandler(VCHIQDoorbellInterruptHandler),VCHIQ);
  
   {Release Platform State}
   if VCHIQ.State.platform_state <> nil then FreeMem(VCHIQ.State.platform_state);
   VCHIQ.State.platform_state:=nil;
 
   {Release State}
   FreeMem(VCHIQ.State);
   VCHIQ.State:=nil;
   
   {Release Slot Memory}
   FreeMem(SlotMemory);
   Exit;
  end;
 
 {Update Id}
 Inc(VCHIQId);
 
 {Set Defaults}
 VCHIQDevice:=VCHIQ;
 VCHIQState:=VCHIQ.State;
 
 //vchiq_call_connected_callbacks //To Do //Continuing //Implement these to support VCSM
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function VCHIQDeviceStop(VCHIQ:PVCHIQDevice):LongWord; 
{Stop the specified VCHIQ device, deregister the master VCHIQ and release slots and shared data}
{VCHIQ: The VCHIQ device to stop}
{Return: ERROR_SUCCESS on completion or another error code on failure}

{Note: The Linux driver does not incorporate any functionality for shutting down the VCHIQ and releasing slots and data}

{Incorporates functionality from vchiq_remove in vchiq_arm.c}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Device Stop (VCHIQ=' + IntToHex(PtrUInt(VCHIQ),8) + ')');
 {$ENDIF}
 
 {Check VCHIQ}
 if VCHIQ = nil then Exit;
 if VCHIQ.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check State}
 Result:=ERROR_NOT_ASSIGNED;
 if VCHIQState = nil then Exit;
 
 {Check Device}
 if VCHIQDevice = nil then Exit;
 
 {Release IRQ}
 ReleaseIRQ(IRQ_ROUTING,VCHIQ_DOORBELL0_IRQ,TInterruptHandler(VCHIQDoorbellInterruptHandler),VCHIQ);
 
 //To Do //Continuing //Linux driver does not seem to perform any shutdown process at all
 
 //VCHIQStates[VCHIQ.State.id]:=nil;
 //vchiq_remove (vchiq_arm.c)
 
 {Reset Defaults}
 VCHIQState:=nil;
 VCHIQDevice:=nil;
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function VCHIQDeviceOpen(VCHIQ:PVCHIQDevice):PVCHIQInstance;
{Open a new instance of the specified VCHIQ device}
{VCHIQ: The VCHIQ device to open}
{Return: Pointer to a newly opened VCHIQ instance}

{Incorporates functionality from vchiq_open in vchiq_arm.c}
var
 State:PVCHIQ_STATE_T;
begin
 {}
 Result:=nil;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Device Open (VCHIQ=' + IntToHex(PtrUInt(VCHIQ),8) + ')');
 {$ENDIF}
 
 {Check VCHIQ}
 if VCHIQ = nil then Exit;
 if VCHIQ.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Get State}
 State:=VCHIQGetState;
 if State = nil then Exit;
 
 {Allocate Instance}
 Result:=AllocMem(SizeOf(TVCHIQInstance));
 if Result = nil then Exit;
 
 {Initialize Instance}
 Result.VCHIQ:=VCHIQ;
 Result.State:=VCHIQ.State;
 Result.PID:=1; {Ultibo has no concept of process so we use a dummy value}
 Result.InsertEvent:=SemaphoreCreate(0);
 Result.RemoveEvent:=SemaphoreCreate(0);
 Result.CompletionMutex:=MutexCreate;
 Result.BulkWaiterMutex:=MutexCreate;
end;

{==============================================================================}

function VCHIQDeviceClose(VCHIQ:PVCHIQDevice;Instance:PVCHIQInstance):LongWord;
{Close an existing instance on the specified VCHIQ device}
{VCHIQ: The VCHIQ device to close the instance on}
{Instance: The instance to be closed}
{Return: ERROR_SUCCESS on completion or another error code on failure}

{Incorporates functionality from vchiq_release in vchiq_arm.c}
var
 Index:Integer;
 State:PVCHIQ_STATE_T;
 Header:PVCHIQ_HEADER_T;
 Service:PVCHIQ_SERVICE_T;
 UserService:PVCHIQ_USER_SERVICE_T;
 Completion:PVCHIQ_COMPLETION_DATA_T;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Device Close (VCHIQ=' + IntToHex(PtrUInt(VCHIQ),8) + ' Instance=' + IntToHex(PtrUInt(Instance),8) + ')');
 {$ENDIF}
 
 {Check VCHIQ}
 if VCHIQ = nil then Exit;
 if VCHIQ.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Instance}
 if Instance = nil then Exit;
 
 {Get State}
 State:=VCHIQGetState;
 if State = nil then Exit;
 
 {Ensure videocore is awake to allow termination}
 VCHIQUseInternal(Instance.state,nil,USE_TYPE_VCHIQ);
 
 {Lock Mutex}
 MutexLock(Instance.CompletionMutex);
 
 {Wake the completion thread and ask it to exit}
 Instance.Closing:=True;
 SemaphoreSignal(Instance.InsertEvent);
 
 {Unlock Mutex}
 MutexUnlock(Instance.CompletionMutex);
 
 {Wake the slot handler if the completion queue is full}
 SemaphoreSignal(Instance.RemoveEvent);
 
 {Mark all services for termination...}
 Index:=0;
 Service:=VCHIQNextServiceByInstance(State,Instance,Index);
 while Service <> nil do
  begin
   {Get User Service}
   UserService:=PVCHIQ_USER_SERVICE_T(Service.base.userdata);
   
   {Wake the slot handler if the msg queue is full}
   SemaphoreSignal(UserService.remove_event);
   
   {Terminate Service Internal}
   VCHIQTerminateServiceInternal(Service);
   
   {Unlock Service}
   VCHIQUnlockService(Service);
   
   {Get Next Service}
   Service:=VCHIQNextServiceByInstance(State,Instance,Index);
  end;
 
 {...and wait for them to die}
 Index:=0;
 Service:=VCHIQNextServiceByInstance(State,Instance,Index);
 while Service <> nil do
  begin
   {Get User Service}
   UserService:=PVCHIQ_USER_SERVICE_T(Service.base.userdata);
  
   {Wait for Remove Event}
   SemaphoreWait(Service.remove_event);
   
   {Check Service State}
   if Service.srvstate <> VCHIQ_SRVSTATE_FREE then
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Service.srvstate <> VCHIQ_SRVSTATE_FREE');
     ThreadHalt(0);
    end;
  
   {Lock Spinlock}
   SpinLock(VCHIQMessageQueueLock);
   
   {Check Message Queue}
   while UserService.msg_remove <> UserService.msg_insert do
    begin
     {Get Header}
     Header:=UserService.msg_queue[UserService.msg_remove and (VCHIQ_MSG_QUEUE_SIZE - 1)];
     
     {Update Message Remove}
     Inc(UserService.msg_remove);
     
     {Unlock Spinlock}
     SpinUnlock(VCHIQMessageQueueLock);
     
     {Check Header}
     if Header <> nil then
      begin
       {Release Message}
       VCHIQReleaseMessage(Service.handle,Header);
      end;
     
     {Lock Spinlock}
     SpinLock(VCHIQMessageQueueLock);
    end;
   
   {Unlock Spinlock}
   SpinUnlock(VCHIQMessageQueueLock);
   
   {Unlock Service}
   VCHIQUnlockService(Service);
   
   {Get Next Service}
   Service:=VCHIQNextServiceByInstance(State,Instance,Index);
  end;
 
 {Release any closed services}
 while Instance.CompletionRemove <> Instance.CompletionInsert do
  begin
   {Get Completion}
   Completion:=@Instance.Completions[Instance.CompletionRemove and (VCHIQ_MAX_COMPLETIONS - 1)];
   
   {Get Service}
   Service:=Completion.service_userdata;
   
   {Check Reason}
   if Completion.reason = VCHIQ_SERVICE_CLOSED then
    begin
     {Get User Service}
     UserService:=PVCHIQ_USER_SERVICE_T(Service.base.userdata);
     
     {Wake any blocked user-thread}
     if Instance.UseCloseDelivered then
      begin
       SemaphoreSignal(UserService.close_event);
      end;
     
     {Unlock Service}
     VCHIQUnlockService(Service);
    end;
    
   {Update Completion Remove}
   Inc(Instance.CompletionRemove);   
  end;
 
 {Release the PEER service count}
 VCHIQReleaseInternal(Instance.State,nil);
 
 //To Do //Continuing //Instance.bulk_waiter_list
                      //When freeing Waiter also free the bulk_waiter.event Semaphore
 if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: VCHIQDeviceClose'); //To Do //Temp
 
 {Destroy Semaphores and Mutexes}
 MutexDestroy(Instance.BulkWaiterMutex);
 MutexDestroy(Instance.CompletionMutex);
 SemaphoreDestroy(Instance.RemoveEvent);
 SemaphoreDestroy(Instance.InsertEvent);

 {Invalidate Semaphores and Mutexes}
 Instance.BulkWaiterMutex:=INVALID_HANDLE_VALUE;
 Instance.CompletionMutex:=INVALID_HANDLE_VALUE;
 Instance.RemoveEvent:=INVALID_HANDLE_VALUE;
 Instance.InsertEvent:=INVALID_HANDLE_VALUE;
 
 {Free Instance}
 FreeMem(Instance);
 
 {Return Success}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}
 
function VCHIQDeviceControl(VCHIQ:PVCHIQDevice;Instance:PVCHIQInstance;Code,Argument:LongWord;var Response:LongWord):LongWord;
{Send an IO control message to the specified VCHIQ device and instance}
{VCHIQ: The VCHIQ device to send the control message to}
{Instance: The open instance to use for the control message}
{Code: The IO control code to send (eg VCHIQ_IOC_CONNECT)}
{Argument: The argument to be used for the control message (Control code specific)}
{Response: The response returned from the control message (Control code specific)}
{Return: ERROR_SUCCESS on completion or another error code on failure}

{Incorporates functionality from vchiq_ioctl in vchiq_arm.c}
var
 Index:Integer;
 Version:LongWord;
 Status:VCHIQ_STATUS_T;
 Service:PVCHIQ_SERVICE_T;
 Handle:VCHIQ_SERVICE_HANDLE_T;
 
 DumpMem:PVCHIQ_DUMP_MEM_T;
 GetConfig:PVCHIQ_GET_CONFIG_T;
 AwaitCompletion:PVCHIQ_AWAIT_COMPLETION_T;
 CreateService:PVCHIQ_CREATE_SERVICE_T;
 SetServiceOption:PVCHIQ_SET_SERVICE_OPTION_T;
 QueueMessage:PVCHIQ_QUEUE_MESSAGE_T;
 DequeueMessage:PVCHIQ_DEQUEUE_MESSAGE_T;
 QueueBulkTransfer:PVCHIQ_QUEUE_BULK_TRANSFER_T;
 
 Userdata:Pointer;
 Header:PVCHIQ_HEADER_T;
 ServiceState:VCHIQ_SRVSTATE_T;
 BulkDirection:VCHIQ_BULK_DIR_T;
 UserService:PVCHIQ_USER_SERVICE_T;
 Completion:PVCHIQ_COMPLETION_DATA_T;
 BulkWaiterNode:PVCHIQ_BULK_WAITER_NODE_T;
 
 CompletionRemove:Integer;
 MessageLength:Integer;
 MessageBuffer:Pointer;
 MessageBufferCount:Integer;
 ModeWaiting:VCHIQ_BULK_MODE_T;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Device Control (VCHIQ=' + IntToHex(PtrUInt(VCHIQ),8) + ' Instance=' + IntToHex(PtrUInt(Instance),8) + ' Code=' + IntToHex(Code,8) + ' Argument=' + IntToHex(Argument,8) + ')');
 {$ENDIF}
 
 {Setup Default}
 Response:=0;
 
 {Check VCHIQ}
 if VCHIQ = nil then Exit;
 if VCHIQ.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Instance}
 if Instance = nil then Exit;
 
 {Set Defaults}
 Service:=nil;
 try
  {Check Code} 
  case Code of
   VCHIQ_IOC_SHUTDOWN:begin
     {$IFDEF VC4VCHIQ_DEBUG}
     if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Device Control (Code=VCHIQ_IOC_SHUTDOWN)');
     {$ENDIF}
     
     {Check Connected}
     if not Instance.Connected then
      begin
       {Return Success}
       Result:=ERROR_SUCCESS;
       Exit;
      end;
      
     {Remove all services}
     Index:=0;
     Service:=VCHIQNextServiceByInstance(Instance.State,Instance,Index);
     while Service <> nil do
      begin
       {Remove Service}
       Status:=VCHIQRemoveService(Service.handle);
       
       {Unlock Service}
       VCHIQUnlockService(Service);
       
       if Status <> VCHIQ_SUCCESS then Break;
       
       {Get Next Service}
       Service:=VCHIQNextServiceByInstance(Instance.State,Instance,Index);
      end;
      
     {Do Not Unlock Service}
     Service:=nil;
     
     if Status = VCHIQ_SUCCESS then
      begin
       {Wake the completion thread and ask it to exit}
       Instance.Closing:=True;
       SemaphoreSignal(Instance.InsertEvent);
      end;
       
     {Return Success}  
     Result:=ERROR_SUCCESS;
    end;
   VCHIQ_IOC_CONNECT:begin
     {$IFDEF VC4VCHIQ_DEBUG}
     if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Device Control (Code=VCHIQ_IOC_CONNECT)');
     {$ENDIF}
     
     {Check Connected}
     if Instance.Connected then Exit;
   
     {Lock Mutex}
     if MutexLock(Instance.State.mutex) <> ERROR_SUCCESS then
      begin
       Result:=ERROR_OPERATION_FAILED;
       Exit;
      end; 
     try 
      {Connect Internal}
      Status:=VCHIQConnectInternal(Instance.State,Instance);
      if Status <> VCHIQ_SUCCESS then
       begin
        if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Could not connect (Status=' + VCHIQStatusToString(Status) + ')');
        
        Result:=ERROR_OPERATION_FAILED;
        Exit;
       end;     
      
      {Update Connected}
      Instance.Connected:=True;
      
      {Return Success}  
      Result:=ERROR_SUCCESS;
     finally
      {Unlock Mutex}
      MutexUnlock(Instance.State.mutex);
     end;     
    end;
   VCHIQ_IOC_CREATE_SERVICE:begin
     {$IFDEF VC4VCHIQ_DEBUG}
     if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Device Control (Code=VCHIQ_IOC_CREATE_SERVICE)');
     {$ENDIF}
 
     {Get Request}
     CreateService:=PVCHIQ_CREATE_SERVICE_T(Argument);
     if CreateService = nil then
      begin
       Exit;
      end;
     
     {Allocate User Service}
     UserService:=AllocMem(SizeOf(VCHIQ_USER_SERVICE_T));
     if UserService = nil then
      begin
       Result:=ERROR_NOT_ENOUGH_MEMORY;
       Exit;
      end;
     
     {Check Open}
     if CreateService.is_open <> 0 then
      begin
       {Check Connected}
       if not Instance.Connected then
        begin
         {Free User Service}
         FreeMem(UserService);
         
         Result:=ERROR_NOT_OPEN;
         Exit;
        end;
        
       {Set State} 
       ServiceState:=VCHIQ_SRVSTATE_OPENING; 
      end
     else
      begin
       {Set State} 
       ServiceState:=VCHIQ_SRVSTATE_HIDDEN; 
       if Instance.Connected then
        begin
         ServiceState:=VCHIQ_SRVSTATE_LISTENING; 
        end;
      end; 
       
     {Setup Callback}
     Userdata:=CreateService.params.userdata;
     CreateService.params.callback:=VCHIQServiceCallback;
     CreateService.params.userdata:=UserService;
     
     {Add Service Internal}
     Service:=VCHIQAddServiceInternal(Instance.State,@CreateService.params,ServiceState,Instance,VCHIQUserServiceFree);
     if Service <> nil then
      begin
       {Update User Service}
       UserService.service:=Service;
       UserService.userdata:=Userdata;
       UserService.instance:=Instance;
       UserService.is_vchi:=(CreateService.is_vchi <> 0);
       UserService.dequeue_pending:=False;
       UserService.close_pending:=False;
       UserService.message_available_pos:=Instance.CompletionRemove - 1;
       UserService.msg_insert:=0;
       UserService.msg_remove:=0;
       UserService.insert_event:=SemaphoreCreate(0); {Destroyed by VCHIQUserServiceFree}
       UserService.remove_event:=SemaphoreCreate(0); {Destroyed by VCHIQUserServiceFree}
       UserService.close_event:=SemaphoreCreate(0); {Destroyed by VCHIQUserServiceFree}
       
       {Check Open}
       if CreateService.is_open <> 0 then
        begin
         {Open Service Internal}
         Status:=VCHIQOpenServiceInternal(Service,Instance.PID);
         if Status <> VCHIQ_SUCCESS then
          begin
           {Remove Service}
           VCHIQRemoveService(Service.handle);
           
           Result:=ERROR_OPERATION_FAILED;
           Exit;
          end;
        end;
       
       {Update Request}
       CreateService.handle:=Service.handle;
      
       {Do Not Unlock Service}
       Service:=nil;
       
       {Return Success}  
       Result:=ERROR_SUCCESS;
      end
     else
      begin
       {Free User Service}
       FreeMem(UserService);
       
       Result:=ERROR_ALREADY_EXISTS;
       Exit;
      end;
    end;
   VCHIQ_IOC_CLOSE_SERVICE:begin
     {$IFDEF VC4VCHIQ_DEBUG}
     if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Device Control (Code=VCHIQ_IOC_CLOSE_SERVICE Handle=' + IntToHex(Argument,8) + ')');
     {$ENDIF}
   
     {Get Handle}
     Handle:=VCHIQ_SERVICE_HANDLE_T(Argument);
   
     {Find Service}
     Service:=VCHIQFindServiceForInstance(Instance,Handle);
     if Service <> nil then
      begin
       {Get User Service}
       UserService:=PVCHIQ_USER_SERVICE_T(Service.base.userdata);
       
       {close_pending is false on first entry, and when the wait in VCHIQCloseService has been interrupted}
       if not UserService.close_pending then
        begin
         {Close Service}
         Status:=VCHIQCloseService(Service.handle);
         if Status <> VCHIQ_SUCCESS then
          begin
           Result:=ERROR_OPERATION_FAILED;
           Exit;
          end;
        end;
        
       {close_pending is true once the underlying service  has been closed until the client library calls the CLOSE_DELIVERED ioctl, signalling close_event}
       if (UserService.close_pending) and (SemaphoreWait(UserService.close_event) <> ERROR_SUCCESS) then
        begin
         Result:=ERROR_OPERATION_FAILED;
         Exit;
        end;
       
       {Return Success}  
       Result:=ERROR_SUCCESS;
      end;
    end;
   VCHIQ_IOC_REMOVE_SERVICE:begin
     {$IFDEF VC4VCHIQ_DEBUG}
     if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Device Control (Code=VCHIQ_IOC_REMOVE_SERVICE Handle=' + IntToHex(Argument,8) + ')');
     {$ENDIF}
   
     {Get Handle}
     Handle:=VCHIQ_SERVICE_HANDLE_T(Argument);
   
     {Find Service}
     Service:=VCHIQFindServiceForInstance(Instance,Handle);
     if Service <> nil then
      begin
       {Get User Service}
       UserService:=PVCHIQ_USER_SERVICE_T(Service.base.userdata);
   
       {close_pending is false on first entry, and when the wait in VCHIQCloseService has been interrupted}
       if not UserService.close_pending then
        begin
         {Remove Service}
         Status:=VCHIQRemoveService(Service.handle);
         if Status <> VCHIQ_SUCCESS then
          begin
           Result:=ERROR_OPERATION_FAILED;
           Exit;
          end;
        end;
   
       {close_pending is true once the underlying service has been closed until the client library calls the CLOSE_DELIVERED ioctl, signalling close_event}
       if (UserService.close_pending) and (SemaphoreWait(UserService.close_event) <> ERROR_SUCCESS) then
        begin
         Result:=ERROR_OPERATION_FAILED;
         Exit;
        end;
       
       {Return Success}  
       Result:=ERROR_SUCCESS;
      end;
    end;
   VCHIQ_IOC_QUEUE_MESSAGE:begin
     {$IFDEF VC4VCHIQ_DEBUG}
     if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Device Control (Code=VCHIQ_IOC_QUEUE_MESSAGE)');
     {$ENDIF}
   
     {Get Request}
     QueueMessage:=PVCHIQ_QUEUE_MESSAGE_T(Argument);
     if QueueMessage = nil then
      begin
       Exit;
      end;
   
     {Find Service}
     Service:=VCHIQFindServiceForInstance(Instance,QueueMessage.handle);
     if (Service <> nil) and (QueueMessage.count <= VCHIQ_MAX_ELEMENTS) then
      begin
       {Queue Message}
       Status:=VCHIQQueueMessage(QueueMessage.handle,QueueMessage.elements,QueueMessage.count);
       if Status <> VCHIQ_SUCCESS then
        begin
         Result:=ERROR_OPERATION_FAILED;
         Exit;
        end;

       {Return Success}  
       Result:=ERROR_SUCCESS;
      end;
    end;
   VCHIQ_IOC_QUEUE_BULK_TRANSMIT,VCHIQ_IOC_QUEUE_BULK_RECEIVE:begin
     if Code = VCHIQ_IOC_QUEUE_BULK_TRANSMIT then
      begin
       {$IFDEF VC4VCHIQ_DEBUG}
       if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Device Control (Code=VCHIQ_IOC_QUEUE_BULK_TRANSMIT)');
       {$ENDIF}
       
       {Get Direction}
       BulkDirection:=VCHIQ_BULK_TRANSMIT;
      end
     else
      begin
       {$IFDEF VC4VCHIQ_DEBUG}
       if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Device Control (Code=VCHIQ_IOC_QUEUE_BULK_RECEIVE)');
       {$ENDIF}
      
       {Get Direction}
       BulkDirection:=VCHIQ_BULK_RECEIVE;
      end;
   
     {Get Request}
     QueueBulkTransfer:=PVCHIQ_QUEUE_BULK_TRANSFER_T(Argument);
     if QueueBulkTransfer = nil then
      begin
       Exit;
      end;
   
     {Find Service}
     Service:=VCHIQFindServiceForInstance(Instance,QueueBulkTransfer.handle);
     if Service <> nil then
      begin
       {Set Defaults}
       BulkWaiterNode:=nil;
       
       {Check Mode}
       if QueueBulkTransfer.mode = VCHIQ_BULK_MODE_BLOCKING then
        begin
         {Create Waiter Node}
         BulkWaiterNode:=AllocMem(SizeOf(VCHIQ_BULK_WAITER_NODE_T));
         if BulkWaiterNode = nil then
          begin
           Result:=ERROR_NOT_ENOUGH_MEMORY;
           Exit;
          end;
         
         {Update Waiter Node}
         BulkWaiterNode.bulk_waiter.event:=INVALID_HANDLE_VALUE;
         
         {Save Bulk Waiter}
         QueueBulkTransfer.userdata:=@BulkWaiterNode.bulk_waiter; 
        end
       else if QueueBulkTransfer.mode = VCHIQ_BULK_MODE_WAITING then
        begin
         {Lock Mutex}
         MutexLock(Instance.BulkWaiterMutex);
         
         {Find Waiter Node}
         //To Do //Continuing //instance->bulk_waiter_list
         if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: VCHIQDeviceControl'); //To Do //Temp
         
         {Unlock Mutex}
         MutexUnlock(Instance.BulkWaiterMutex);
         
         {Check Waiter Node}
         if BulkWaiterNode = nil then
          begin
           if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Bulk waiter not found');
           
           Result:=ERROR_NOT_FOUND;
           Exit;
          end;
          
         {Save Bulk Waiter} 
         QueueBulkTransfer.userdata:=@BulkWaiterNode.bulk_waiter; 
        end;
   
       {Bulk Transfer}
       Status:=VCHIQBulkTransfer(QueueBulkTransfer.handle,VCHI_MEM_HANDLE_INVALID,QueueBulkTransfer.data,QueueBulkTransfer.size,QueueBulkTransfer.userdata,QueueBulkTransfer.mode,BulkDirection);
       
       {Check Waiter Node}
       if BulkWaiterNode <> nil then
        begin
         {Check Status and Bulk}
         if (Status <> VCHIQ_RETRY) or (BulkWaiterNode.bulk_waiter.bulk = nil) then
          begin
           {Check Bulk}
           if BulkWaiterNode.bulk_waiter.bulk <> nil then
            begin
             {Lock Spinlock}
             SpinLock(VCHIQBulkWaiterLock);
             
             {Cancel the signal when the transfer completes}
             BulkWaiterNode.bulk_waiter.bulk.userdata:=nil;
             
             {Unlock Spinlock}
             SpinUnlock(VCHIQBulkWaiterLock);         
            end;
            
           {Check Waiter Event} 
           if BulkWaiterNode.bulk_waiter.event <> INVALID_HANDLE_VALUE then
            begin
             {Destroy Semaphore}
             SemaphoreDestroy(BulkWaiterNode.bulk_waiter.event);
             
             {Invalidate Semaphore}
             BulkWaiterNode.bulk_waiter.event:=INVALID_HANDLE_VALUE;
            end;
           
           {Free Waiter Node}
           FreeMem(BulkWaiterNode);
          end
         else
          begin
           {Get Mode}
           ModeWaiting:=VCHIQ_BULK_MODE_WAITING;
           
           {Get PID}
           BulkWaiterNode.pid:=ThreadGetCurrent; {Thread ID (In the Linux driver this is the Process ID which is the Thread ID)}
           
           {Lock Mutex}
           MutexLock(Instance.BulkWaiterMutex);
           
           {Add Waiter Node}
           //To Do //Continuing //instance->bulk_waiter_list
           if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: VCHIQDeviceControl'); //To Do //Temp
           
           {Unlock Mutex}
           MutexUnlock(Instance.BulkWaiterMutex);
           
           {Return Mode Waiting}
           QueueBulkTransfer.mode:=ModeWaiting;
          end;
        end
       else
        begin
         {Check Status}
         if Status <> VCHIQ_SUCCESS then
          begin
           Result:=ERROR_OPERATION_FAILED;
           Exit;
          end;
        end;        
       
       {Return Success}  
       Result:=ERROR_SUCCESS;
      end;
    end;
   VCHIQ_IOC_AWAIT_COMPLETION:begin
     {$IFDEF VC4VCHIQ_DEBUG}
     if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Device Control (Code=VCHIQ_IOC_AWAIT_COMPLETION)');
     {$ENDIF}
   
     {Check Connected}
     if not Instance.Connected then
      begin
       Result:=ERROR_NOT_OPEN;
       Exit;
      end; 
   
     {Get Request}
     AwaitCompletion:=PVCHIQ_AWAIT_COMPLETION_T(Argument);
     if AwaitCompletion = nil then
      begin
       Exit;
      end;
     
     {Lock Mutex}
     if MutexLock(Instance.CompletionMutex) <> ERROR_SUCCESS then
      begin
       Result:=ERROR_OPERATION_FAILED;
       Exit;
      end;
     try
      while (Instance.CompletionRemove = Instance.CompletionInsert) and not(Instance.Closing) do
       begin
        {Unlock Mutex}
        MutexUnlock(Instance.CompletionMutex);
        
        {Wait for Insert}
        if SemaphoreWait(Instance.InsertEvent) <> ERROR_SUCCESS then
         begin
          Result:=ERROR_OPERATION_FAILED;
          Exit;
         end;
        
        {Lock Mutex}
        MutexLock(Instance.CompletionMutex);
       end;
      
      {Get Message Buffer Count}
      MessageBufferCount:=AwaitCompletion.msgbufcount;
       
      {Get Completion Remove}
      CompletionRemove:=Instance.CompletionRemove;

      {Default Success}
      Result:=ERROR_SUCCESS;
      Response:=0;
      try
       while Response < AwaitCompletion.count do
        begin
         {Check Remove}
         if CompletionRemove = Instance.CompletionInsert then Break;
          
         {Get Completion} 
         Completion:=@Instance.Completions[CompletionRemove and (VCHIQ_MAX_COMPLETIONS - 1)]; 
        
         {A read memory barrier is needed to prevent the prefetch of a stale completion record}
         DataMemoryBarrier;
        
         {Get Service}
         Service:=Completion.service_userdata;
         
         {Get User Service}
         UserService:=PVCHIQ_USER_SERVICE_T(Service.base.userdata);
         
         {Update Completion}
         Completion.service_userdata:=UserService.userdata;
         
         {Get Header}
         Header:=Completion.header;
         if Header <> nil then
          begin
           {Get Message Length}
           MessageLength:=Header.size + VCHIQ_HEADER_SIZE; {SizeOf(VCHIQ_HEADER_T)} {Do not include the data member}
           
           {This must be a VCHIQ-style service}
           if AwaitCompletion.msgbufsize < MessageLength then
            begin
             if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Device Control: AwaitCompletion.msgbufsize < MessageLength (msgbufsize=' + IntToStr(AwaitCompletion.msgbufsize) + ' MessageLength=' + IntToStr(MessageLength) + ')');
             
             Result:=ERROR_INSUFFICIENT_BUFFER;
             Exit;
            end;
           
           {Stall here for lack of a buffer for the message}
           if MessageBufferCount <= 0 then Break;
           
           {Update Buffer Count}
           Dec(MessageBufferCount);
           
           {Get Message Buffer}
           MessageBuffer:=AwaitCompletion.msgbufs[MessageBufferCount];
           
           {Copy the message}
           System.Move(Header^,MessageBuffer^,MessageLength);
           
           {Now it has been copied, the message can be released}
           VCHIQReleaseMessage(Service.handle,Header);

           {The completion must point to the message buffer}
           Completion.header:=MessageBuffer;
          end;
         
         {Check Reason}
         if (Completion.reason = VCHIQ_SERVICE_CLOSED) and not(Instance.UseCloseDelivered) then
          begin
           {Unlock Service}
           VCHIQUnlockService(Service);
          end;
         
         {Copy Completion}
         System.Move(Completion^,AwaitCompletion.buf[Response],SizeOf(VCHIQ_COMPLETION_DATA_T));
         
         {Ensure that the above copy has completed before advancing the remove pointer}
         DataMemoryBarrier;
         
         {Update Completion Remove}
         Inc(CompletionRemove);
         Instance.CompletionRemove:=CompletionRemove;
         
         {Update Response}
         Inc(Response);
        end;
       
       {Return Message Buffer Count}
       AwaitCompletion.msgbufcount:=MessageBufferCount;
       
       {Do Not Unlock Service}
       Service:=nil;
      finally
       if Response > 0 then
        begin
         {Signal Remove Event}
         SemaphoreSignalEx(Instance.RemoveEvent,Response,nil);
         
         {Return Success}
         Result:=ERROR_SUCCESS;
        end; 
      end; 
     finally 
      {Unlock Mutex}
      MutexUnlock(Instance.CompletionMutex);
     end; 
    end;
   VCHIQ_IOC_DEQUEUE_MESSAGE:begin
     {$IFDEF VC4VCHIQ_DEBUG}
     if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Device Control (Code=VCHIQ_IOC_DEQUEUE_MESSAGE)');
     {$ENDIF}
   
     {Get Request}
     DequeueMessage:=PVCHIQ_DEQUEUE_MESSAGE_T(Argument);
     if DequeueMessage = nil then
      begin
       Exit;
      end;
   
     {$IFDEF VC4VCHIQ_DEBUG}
     if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Device Control: Dequeue Message (handle=' + IntToHex(DequeueMessage.handle,8) + ' blocking=' + IntToStr(DequeueMessage.blocking) + ' bufsize=' + IntToStr(DequeueMessage.bufsize) + ' buf=' + IntToHex(PtrUInt(DequeueMessage.buf),8) + ')');
     {$ENDIF}
   
     {Find Service}
     Service:=VCHIQFindServiceForInstance(Instance,DequeueMessage.handle);
     if Service <> nil then
      begin
       {Get User Service}
       UserService:=PVCHIQ_USER_SERVICE_T(Service.base.userdata);
       
       {Check VCHI}
       if not UserService.is_vchi then Exit;
       
       {Lock Spinlock}
       if SpinLock(VCHIQMessageQueueLock) <> ERROR_SUCCESS then Exit;
       try
        {$IFDEF VC4VCHIQ_DEBUG}
        if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Device Control: User Service (msg_remove=' + IntToStr(UserService.msg_remove) + ' msg_insert=' + IntToStr(UserService.msg_insert) + ')');
        {$ENDIF}
       
        if UserService.msg_remove = UserService.msg_insert then
         begin
          {Check Blocking}
          if DequeueMessage.blocking = 0 then
           begin
            Result:=ERROR_IN_USE;
            Exit;
           end;
          
          {Wait for Message}
          UserService.dequeue_pending:=True;
          repeat
           {Unlock Spinlock}
           SpinUnlock(VCHIQMessageQueueLock);
           
           {$IFDEF VC4VCHIQ_DEBUG}
           if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Device Control: User Service wait for insert (msg_remove=' + IntToStr(UserService.msg_remove) + ' msg_insert=' + IntToStr(UserService.msg_insert) + ')');
           {$ENDIF}
           
           if SemaphoreWait(UserService.insert_event) <> ERROR_SUCCESS then
            begin
             Result:=ERROR_OPERATION_FAILED;
             Exit;
            end;
            
           {Lock Spinlock} 
           SpinLock(VCHIQMessageQueueLock);
          until UserService.msg_remove <> UserService.msg_insert;
         end;
        
        if UserService.msg_remove > UserService.msg_insert then
         begin
          if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: UserService.msg_remove > UserService.msg_insert');
          
          Result:=ERROR_OPERATION_FAILED;
          Exit;
         end;
        
        {Get Header} 
        Header:=UserService.msg_queue[UserService.msg_remove and (VCHIQ_MSG_QUEUE_SIZE - 1)];
        Inc(UserService.msg_remove);
       finally
        {Unlock Spinlock}
        SpinUnlock(VCHIQMessageQueueLock);
       end; 
        
       {Signal Remove}
       SemaphoreSignal(UserService.remove_event);
       
       {Check Header}
       if Header = nil then
        begin
         Result:=ERROR_NOT_OPEN;
         Exit;
        end
       else if Header.size <= DequeueMessage.bufsize then 
        begin
         {$IFDEF VC4VCHIQ_DEBUG}
         if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Device Control: Dequeue Message (Header=' + IntToHex(PtrUInt(Header),8) + ' Size=' + IntToStr(Header.size) + ')');
         {$ENDIF}
         
         {Check Buffer}
         if DequeueMessage.buf <> nil then
          begin
           {Copy Message}
           System.Move(Header.data[0],DequeueMessage.buf^,Header.size);
          end; 
         
         {Get Message Size} 
         Response:=Header.size;

         {Release Message}
         VCHIQReleaseMessage(Service.handle,Header);         
        end
       else
        begin
         if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: DequeueMessage.bufsize < Header.size');
         
         Result:=ERROR_INSUFFICIENT_BUFFER;
         Exit;
        end;        

       {Return Success}  
       Result:=ERROR_SUCCESS;
      end;
    end;
   VCHIQ_IOC_GET_CLIENT_ID:begin
     {$IFDEF VC4VCHIQ_DEBUG}
     if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Device Control (Code=VCHIQ_IOC_GET_CLIENT_ID Handle=' + IntToHex(Argument,8) + ')');
     {$ENDIF}
   
     {Get Handle}
     Handle:=VCHIQ_SERVICE_HANDLE_T(Argument);
   
     {Get Client ID}
     Response:=VCHIQGetClientID(Handle);    
     
     {Return Success}  
     Result:=ERROR_SUCCESS;
    end;
   VCHIQ_IOC_GET_CONFIG:begin
     {$IFDEF VC4VCHIQ_DEBUG}
     if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Device Control (Code=VCHIQ_IOC_GET_CONFIG)');
     {$ENDIF}
   
     {Get Request}
     GetConfig:=PVCHIQ_GET_CONFIG_T(Argument);
     if (GetConfig = nil) or (GetConfig.config_size <> SizeOf(VCHIQ_CONFIG_T)) then
      begin
       Exit;
      end;
      
     {Get Config}
     if VCHIQGetConfig(Instance,GetConfig.config_size,GetConfig.pconfig) <> VCHIQ_SUCCESS then
      begin
       Result:=ERROR_OPERATION_FAILED;
       Exit;
      end;
   
     {Return Success}  
     Result:=ERROR_SUCCESS;
    end;
   VCHIQ_IOC_USE_SERVICE:begin
     {$IFDEF VC4VCHIQ_DEBUG}
     if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Device Control (Code=VCHIQ_IOC_USE_SERVICE Handle=' + IntToHex(Argument,8) + ')');
     {$ENDIF}
     
     {Get Handle}
     Handle:=VCHIQ_SERVICE_HANDLE_T(Argument);
   
     {Find Service}
     Service:=VCHIQFindServiceForInstance(Instance,Handle);
     if Service <> nil then
      begin
       {Use Service Internal}
       Status:=VCHIQUseServiceInternal(Service);
       if Status <> VCHIQ_SUCCESS then
        begin
         if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Use service failed (Status=' + VCHIQStatusToString(Status) + ' Service=' + VCHIQFourccToString(Service.base.fourcc) + ')');
         
         Result:=ERROR_OPERATION_FAILED;
         Exit;
        end;
       
       {Return Success}  
       Result:=ERROR_SUCCESS;
      end;
    end;
   VCHIQ_IOC_RELEASE_SERVICE:begin
     {$IFDEF VC4VCHIQ_DEBUG}
     if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Device Control (Code=VCHIQ_IOC_RELEASE_SERVICE Handle=' + IntToHex(Argument,8) + ')');
     {$ENDIF}
     
     {Get Handle}
     Handle:=VCHIQ_SERVICE_HANDLE_T(Argument);
   
     {Find Service}
     Service:=VCHIQFindServiceForInstance(Instance,Handle);
     if Service <> nil then
      begin
       {Release Service Internal}
       Status:=VCHIQReleaseServiceInternal(Service);
       if Status <> VCHIQ_SUCCESS then
        begin
         if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Release service failed (Status=' + VCHIQStatusToString(Status) + ' Service=' + VCHIQFourccToString(Service.base.fourcc) + ')');

         Result:=ERROR_OPERATION_FAILED;
         Exit;
        end;
        
       {Return Success}  
       Result:=ERROR_SUCCESS;
      end;
    end;
   VCHIQ_IOC_SET_SERVICE_OPTION:begin
     {$IFDEF VC4VCHIQ_DEBUG}
     if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Device Control (Code=VCHIQ_IOC_SET_SERVICE_OPTION)');
     {$ENDIF}
   
     {Get Request}
     SetServiceOption:=PVCHIQ_SET_SERVICE_OPTION_T(Argument);
     if SetServiceOption = nil then
      begin
       Exit;
      end;
   
     {Find Service}
     Service:=VCHIQFindServiceForInstance(Instance,SetServiceOption.handle);
     if Service <> nil then
      begin
       {Set Service Option}
       Status:=VCHIQSetServiceOption(SetServiceOption.handle,SetServiceOption.option,SetServiceOption.value);
       if Status <> VCHIQ_SUCCESS then
        begin
         Result:=ERROR_OPERATION_FAILED;
         Exit;
        end;
      
       {Return Success}  
       Result:=ERROR_SUCCESS;
      end;
    end;
   VCHIQ_IOC_DUMP_PHYS_MEM:begin
     {$IFDEF VC4VCHIQ_DEBUG}
     if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Device Control (Code=VCHIQ_IOC_DUMP_PHYS_MEM)');
     {$ENDIF}
   
     {Get Request}
     DumpMem:=PVCHIQ_DUMP_MEM_T(Argument);
     if DumpMem = nil then
      begin
       Exit;
      end;
   
     {Dump Physical Memory}
     VCHIQDumpPhysicalMemory(DumpMem.virt_addr,DumpMem.num_bytes);
     
     {Return Success}  
     Result:=ERROR_SUCCESS;
    end;
   VCHIQ_IOC_LIB_VERSION:begin
     {$IFDEF VC4VCHIQ_DEBUG}
     if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Device Control (Code=VCHIQ_IOC_LIB_VERSION Version=' + IntToStr(Argument) + ')');
     {$ENDIF}
   
     {Get Version}
     Version:=Argument;
     if Version < VCHIQ_VERSION_MIN then
      begin
       Exit;
      end
     else if Version >= VCHIQ_VERSION_CLOSE_DELIVERED then
      begin
       Instance.UseCloseDelivered:=True;
      end;
      
     {Return Success}
     Result:=ERROR_SUCCESS;
    end;
   VCHIQ_IOC_CLOSE_DELIVERED:begin
     {$IFDEF VC4VCHIQ_DEBUG}
     if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Device Control (Code=VCHIQ_IOC_CLOSE_DELIVERED Handle=' + IntToHex(Argument,8) + ')');
     {$ENDIF}
   
     {Get Handle}
     Handle:=VCHIQ_SERVICE_HANDLE_T(Argument);
   
     {Find Service}
     Service:=VCHIQFindClosedServiceForInstance(Instance,Handle);
     if Service <> nil then
      begin
       {Get User Service}
       UserService:=PVCHIQ_USER_SERVICE_T(Service.base.userdata);
     
       {Close Delivered}     
       VCHIQCloseDelivered(UserService);
       
       {Return Success}  
       Result:=ERROR_SUCCESS;
      end;
    end;
  end;
 finally
  {Unlock Service}
  if Service <> nil then VCHIQUnlockService(Service);
 end; 
end;
 
{==============================================================================}
{==============================================================================}
{VCHIQ Core Functions}
function VCHIQInitSlots(MemBase:Pointer;MemSize:LongWord):PVCHIQ_SLOT_ZERO_T;
{From vchiq_init_slots in vchiq_core.c}
var
 MemAlign:Integer;
 NumSlots:Integer;
 FirstDataSlot:Integer;
 SlotZero:PVCHIQ_SLOT_ZERO_T;
begin
 {}
 Result:=nil;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Init Slots (Base=' + IntToHex(PtrUInt(MemBase),8) + ' Size=' + IntToStr(MemSize) + ')');
 {$ENDIF}
 
 {Get Alignment}
 MemAlign:=(VCHIQ_SLOT_SIZE - PtrUInt(MemBase)) and VCHIQ_SLOT_MASK;
 
 {Get Slot Zero}
 SlotZero:=PVCHIQ_SLOT_ZERO_T(MemBase + MemAlign);
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Init Slots (SlotZero=' + IntToHex(PtrUInt(SlotZero),8) + ' MemAlign=' + IntToStr(MemAlign) + ')');
 {$ENDIF}
 
 {Get Number of Slots}
 NumSlots:=(MemSize - MemAlign) div VCHIQ_SLOT_SIZE;
 
 {Get First Data Slot}
 FirstDataSlot:=VCHIQ_SLOT_ZERO_SLOTS;
 
 {Ensure there is enough memory to run an absolutely minimum system}
 NumSlots:=NumSlots - FirstDataSlot;
 if NumSlots < 4 then
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Init Slots: Insufficient memory (MemSize=' + IntToStr(MemSize) + ' NumSlots=' + IntToStr(NumSlots) + ')');
   Exit;
  end;
  
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Init Slots (NumSlots=' + IntToStr(NumSlots) + ' FirstDataSlot=' + IntToStr(FirstDataSlot) + ')');
 {$ENDIF}
  
 {Initialize Slots}
 FillChar(SlotZero^,SizeOf(VCHIQ_SLOT_ZERO_T),0);
 
 SlotZero.magic:=VCHIQ_MAGIC;
 SlotZero.version:=VCHIQ_VERSION;
 SlotZero.version_min:=VCHIQ_VERSION_MIN;
 SlotZero.slot_zero_size:=SizeOf(VCHIQ_SLOT_ZERO_T);
 SlotZero.slot_size:=VCHIQ_SLOT_SIZE;
 SlotZero.max_slots:=VCHIQ_MAX_SLOTS;
 SlotZero.max_slots_per_side:=VCHIQ_MAX_SLOTS_PER_SIDE;
 
 SlotZero.master.slot_sync:=FirstDataSlot;
 SlotZero.master.slot_first:=FirstDataSlot + 1;
 SlotZero.master.slot_last:=FirstDataSlot + (NumSlots div 2) - 1;
 SlotZero.slave.slot_sync:=FirstDataSlot + (NumSlots div 2);
 SlotZero.slave.slot_first:=FirstDataSlot + (NumSlots div 2) + 1;
 SlotZero.slave.slot_last:=FirstDataSlot + NumSlots - 1;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then
  begin
   DeviceLogDebug(nil,'VCHIQ: Init Slots');
   DeviceLogDebug(nil,'VCHIQ:  magic=' + IntToHex(SlotZero.magic,8));
   DeviceLogDebug(nil,'VCHIQ:  version=' + IntToStr(SlotZero.version));
   DeviceLogDebug(nil,'VCHIQ:  version_min=' + IntToStr(SlotZero.version_min));
   DeviceLogDebug(nil,'VCHIQ:  slot_zero_size=' + IntToStr(SlotZero.slot_zero_size));
   DeviceLogDebug(nil,'VCHIQ:  slot_size=' + IntToStr(SlotZero.slot_size));
   DeviceLogDebug(nil,'VCHIQ:  max_slots=' + IntToStr(SlotZero.max_slots));
   DeviceLogDebug(nil,'VCHIQ:  max_slots_per_side=' + IntToStr(SlotZero.max_slots_per_side));
   
   DeviceLogDebug(nil,'VCHIQ:  master.slot_sync=' + IntToStr(SlotZero.master.slot_sync));
   DeviceLogDebug(nil,'VCHIQ:  master.slot_first=' + IntToStr(SlotZero.master.slot_first));
   DeviceLogDebug(nil,'VCHIQ:  master.slot_last=' + IntToStr(SlotZero.master.slot_last));
   DeviceLogDebug(nil,'VCHIQ:  slave.slot_sync=' + IntToStr(SlotZero.slave.slot_sync));
   DeviceLogDebug(nil,'VCHIQ:  slave.slot_first=' + IntToStr(SlotZero.slave.slot_first));
   DeviceLogDebug(nil,'VCHIQ:  slave.slot_last=' + IntToStr(SlotZero.slave.slot_last));
  end; 
 {$ENDIF}
 
 {Return Slot Zero}
 Result:=SlotZero;
end;

{==============================================================================}

function VCHIQInitState(VCHIQ:PVCHIQDevice;State:PVCHIQ_STATE_T;SlotZero:PVCHIQ_SLOT_ZERO_T;IsMaster:Boolean):VCHIQ_STATUS_T;
{From vchiq_init_state in vchiq_core.c}
var
 Count:Integer;
 Status:VCHIQ_STATUS_T;
 Local:PVCHIQ_SHARED_STATE_T;
 Remote:PVCHIQ_SHARED_STATE_T;
 ServiceQuota:PVCHIQ_SERVICE_QUOTA_T;
begin
 {}
 Result:=VCHIQ_ERROR;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Init State (VCHIQ=' + IntToHex(PtrUInt(VCHIQ),8) + ' State=' + IntToHex(PtrUInt(State),8) + ' SlotZero=' + IntToHex(PtrUInt(SlotZero),8) + ' IsMaster=' + BoolToStr(IsMaster) + ')');
 {$ENDIF}
 
 {Check State}
 if State = nil then Exit;
 
 {Check Slot Zero}
 if SlotZero = nil then Exit;
 
 {Check Parameters}
 if SlotZero.magic <> VCHIQ_MAGIC then Exit;
 if SlotZero.version < VCHIQ_VERSION_MIN then Exit;
 if VCHIQ_VERSION < SlotZero.version_min then Exit;
 if SlotZero.slot_zero_size <> SizeOf(VCHIQ_SLOT_ZERO_T) then Exit;
 if SlotZero.slot_size <> VCHIQ_SLOT_SIZE then Exit;
 if SlotZero.max_slots <> VCHIQ_MAX_SLOTS then Exit;
 if SlotZero.max_slots_per_side <> VCHIQ_MAX_SLOTS_PER_SIDE then Exit;
 
 {Check Version}
 if VCHIQ_VERSION < SlotZero.version then
  begin
   SlotZero.version:=VCHIQ_VERSION;
  end;
  
 {Check Master} 
 if IsMaster then
  begin
   {Get Local/Remote}
   Local:=@SlotZero.master;
   Remote:=@SlotZero.slave;
  end
 else
  begin
   {Get Local/Remote}
   Local:=@SlotZero.slave;
   Remote:=@SlotZero.master;
  end;  

 {Check Initialized}
 if Local.initialised <> 0 then Exit;
 
 {Initialize State}
 FillChar(State^,SizeOf(VCHIQ_STATE_T),0);
 
 State.id:=VCHIQId;
 State.is_master:=IsMaster;

 {Initialize shared state pointers}
 State.local:=Local;
 State.remote:=Remote;
 State.slot_data:=PVCHIQ_SLOT_ARRAY_T(SlotZero);
 
 {Initialize events and mutexes}
 State.connect:=SemaphoreCreate(0);
 State.mutex:=MutexCreate;
 State.trigger_event:=SemaphoreCreateEx(0,SEMAPHORE_DEFAULT_MAXIMUM,SEMAPHORE_FLAG_IRQ); {Used within interrupt handler}
 State.recycle_event:=SemaphoreCreateEx(0,SEMAPHORE_DEFAULT_MAXIMUM,SEMAPHORE_FLAG_IRQ); {Used within interrupt handler}
 State.sync_trigger_event:=SemaphoreCreateEx(0,SEMAPHORE_DEFAULT_MAXIMUM,SEMAPHORE_FLAG_IRQ); {Used within interrupt handler}
 State.sync_release_event:=SemaphoreCreateEx(0,SEMAPHORE_DEFAULT_MAXIMUM,SEMAPHORE_FLAG_IRQ); {Used within interrupt handler}
 
 State.slot_mutex:=MutexCreate;
 State.recycle_mutex:=MutexCreate;
 State.sync_mutex:=MutexCreate;
 State.bulk_transfer_mutex:=MutexCreate;

 State.slot_available_event:=SemaphoreCreate(0);
 State.slot_remove_event:=SemaphoreCreate(0);
 State.data_quota_event:=SemaphoreCreate(0);

 State.slot_queue_available:=0;
 
 for Count:=0 to VCHIQ_MAX_SERVICES - 1 do
  begin
   ServiceQuota:=@State.service_quotas[Count];
   ServiceQuota.quota_event:=SemaphoreCreate(0);
  end;
  
 for Count:=Local.slot_first to Local.slot_last do
  begin
   Local.slot_queue[State.slot_queue_available]:=Count;
   Inc(State.slot_queue_available);
   SemaphoreSignal(State.slot_available_event);
  end;
 
 State.default_slot_quota:=State.slot_queue_available div 2;
 State.default_message_quota:=Min(Word(State.default_slot_quota * 256),Word(Not(0)));
 
 State.previous_data_index:=-1;
 State.data_use_count:=0;
 State.data_quota:=State.slot_queue_available - 1;
 
 Local.trigger.event:=State.trigger_event;
 VCHIQRemoteEventCreate(@Local.trigger);
 Local.tx_pos:=0;
 
 Local.recycle.event:=State.recycle_event;
 VCHIQRemoteEventCreate(@Local.recycle);
 Local.slot_queue_recycle:=State.slot_queue_available;

 Local.sync_trigger.event:=State.sync_trigger_event;
 VCHIQRemoteEventCreate(@Local.sync_trigger);

 Local.sync_release.event:=State.sync_release_event;
 VCHIQRemoteEventCreate(@Local.sync_release);
 
 {At start-of-day, the slot is empty and available}
 PVCHIQ_HEADER_T(VCHIQ_SLOT_DATA_FROM_INDEX(State,Local.slot_sync)).msgid:=VCHIQ_MSGID_PADDING;
 VCHIQRemoteEventSignalLocal(@Local.sync_release);
 
 Local.debug[Ord(VCHIQ_DEBUG_ENTRIES)]:=Ord(VCHIQ_DEBUG_MAX);
 
 {Platform Init State}
 Status:=VCHIQPlatformInitState(State);
 
 {Create the slot handler thread}
 State.slot_handler_thread:=BeginThread(TThreadStart(VCHIQSlotHandlerExecute),VCHIQ,State.slot_handler_thread,VCHIQ_SLOT_THREAD_STACK_SIZE);
 if State.slot_handler_thread = INVALID_HANDLE_VALUE then 
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Init State: Failed to create slot handler thread');
   Exit;
  end;
 ThreadSetPriority(State.slot_handler_thread,VCHIQ_SLOT_THREAD_PRIORITY);
 ThreadSetName(State.slot_handler_thread,VCHIQ_SLOT_THREAD_NAME);

 {Create the recycle thread}
 State.recycle_thread:=BeginThread(TThreadStart(VCHIQRecycleExecute),VCHIQ,State.recycle_thread,VCHIQ_RECYCLE_THREAD_STACK_SIZE);
 if State.recycle_thread = INVALID_HANDLE_VALUE then 
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Init State: Failed to create recycle thread');
   Exit;
  end;
 ThreadSetPriority(State.recycle_thread,VCHIQ_RECYCLE_THREAD_PRIORITY);
 ThreadSetName(State.recycle_thread,VCHIQ_RECYCLE_THREAD_NAME);
 
 {Create the sync thread}
 State.sync_thread:=BeginThread(TThreadStart(VCHIQSyncExecute),VCHIQ,State.sync_thread,VCHIQ_SYNC_THREAD_STACK_SIZE);
 if State.sync_thread = INVALID_HANDLE_VALUE then 
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Init State: Failed to create sync thread');
   Exit;
  end;
 ThreadSetPriority(State.sync_thread,VCHIQ_SYNC_THREAD_PRIORITY);
 ThreadSetName(State.sync_thread,VCHIQ_SYNC_THREAD_NAME);
 
 {Store State}
 if State.id >= VCHIQ_MAX_STATES then
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Init State: State.id >= VCHIQ_MAX_STATES');
   Exit;
  end;
 VCHIQStates[State.id]:=State;
 
 {Indicate readiness to the other side}
 Local.initialised:=1;
 
 {Return Status}
 Result:=Status;
end;

{==============================================================================}

procedure VCHIQInitBulkQueue(Queue:PVCHIQ_BULK_QUEUE_T);
{From init_bulk_queue in vchiq_core.c}
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Init Bulk Queue (Queue=' + IntToHex(PtrUInt(Queue),8) + ')');
 {$ENDIF}
 
 {Check Queue}
 if Queue = nil then Exit;
 
 Queue.local_insert:=0;
 Queue.remote_insert:=0;
 Queue.process:=0;
 Queue.remote_notify:=0;
 Queue.remove:=0;
end;

{==============================================================================}

function VCHIQCloseService(Handle:VCHIQ_SERVICE_HANDLE_T):VCHIQ_STATUS_T;
{From vchiq_close_service in vchiq_core.c}
var 
 Status:VCHIQ_STATUS_T;
 Service:PVCHIQ_SERVICE_T;
begin
 {}
 Result:=VCHIQ_ERROR;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Close Service (Handle=' + IntToHex(Handle,8) + ')');
 {$ENDIF}
 
 {Get Service}
 Service:=VCHIQFindServiceByHandle(Handle);
 if Service = nil then Exit;
 try
  {Check Service State}
  if (Service.srvstate = VCHIQ_SRVSTATE_FREE) or (Service.srvstate = VCHIQ_SRVSTATE_LISTENING) or (Service.srvstate = VCHIQ_SRVSTATE_HIDDEN) then
   begin
    Exit;
   end;
  
  {Mark Closing}
  VCHIQMarkServiceClosing(Service);
  
  {Check Thread}
  if ThreadGetCurrent = Service.state.slot_handler_thread then
   begin
    {Close Service Internal}
    Status:=VCHIQCloseServiceInternal(Service,False); {Not CloseReceived}
    if Status = VCHIQ_RETRY then
     begin
      if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Status = VCHIQ_RETRY from Close Service Internal');
      Exit;
     end;
   end
  else
   begin
    {Mark the service for termination by the slot handler}
    VCHIQRequestPoll(Service.state,Service,VCHIQ_POLL_TERMINATE);
    
    Status:=VCHIQ_SUCCESS;
   end;   
   
  while True do
   begin
    {Wait for Remove}
    if SemaphoreWait(Service.remove_event) <> ERROR_SUCCESS then
     begin
      Result:=VCHIQ_RETRY;
      Exit;
     end;
   
    {Check Service State}
    if (Service.srvstate = VCHIQ_SRVSTATE_FREE) or (Service.srvstate = VCHIQ_SRVSTATE_LISTENING) or (Service.srvstate = VCHIQ_SRVSTATE_OPEN) then
     begin
      Break;
     end; 
     
    {$IFDEF VC4VCHIQ_DEBUG}
    if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Close Service (Id=' + IntToStr(Service.state.id) + ' LocalPort=' + IntToStr(Service.localport) + ' ServiceState=' + VCHIQServiceStateToString(Service.srvstate) + ')');
    {$ENDIF}
   end;
  
  {Check Service State}
  if (Status = VCHIQ_SUCCESS) and (Service.srvstate <> VCHIQ_SRVSTATE_FREE) and (Service.srvstate <> VCHIQ_SRVSTATE_LISTENING) then
   begin
    Exit;
   end;
 
  {Return Success}
  Result:=VCHIQ_SUCCESS;
 finally
  {Unlock Service}
  VCHIQUnlockService(Service);
 end;
end;

{==============================================================================}

function VCHIQRemoveService(Handle:VCHIQ_SERVICE_HANDLE_T):VCHIQ_STATUS_T;
{From vchiq_remove_service in vchiq_core.c}
var 
 Status:VCHIQ_STATUS_T;
 Service:PVCHIQ_SERVICE_T;
begin
 {}
 Result:=VCHIQ_ERROR;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Remove Service (Handle=' + IntToHex(Handle,8) + ')');
 {$ENDIF}
 
 {Get Service}
 Service:=VCHIQFindServiceByHandle(Handle);
 if Service = nil then Exit;
 try
  {Check Service State}
  if Service.srvstate = VCHIQ_SRVSTATE_FREE then
   begin
    Exit;
   end;
  
  {Mark Closing}
  VCHIQMarkServiceClosing(Service);
  
  {Check Service State and Thread}
  if (Service.srvstate = VCHIQ_SRVSTATE_HIDDEN) or (ThreadGetCurrent = Service.state.slot_handler_thread) then
   begin
    {Make it look like a client, because it must be removed and not left in the LISTENING state}
    Service.public_fourcc:=VCHIQ_FOURCC_INVALID;
    
    {Close Service Internal}
    Status:=VCHIQCloseServiceInternal(Service,False); {Not CloseReceived}
    if Status = VCHIQ_RETRY then
     begin
      if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Status = VCHIQ_RETRY from Close Service Internal');
      Exit;
     end;
   end
  else
   begin
    {Mark the service for termination by the slot handler}
    VCHIQRequestPoll(Service.state,Service,VCHIQ_POLL_TERMINATE);
    
    Status:=VCHIQ_SUCCESS;
   end;   
  
  while True do
   begin
    {Wait for Remove}
    if SemaphoreWait(Service.remove_event) <> ERROR_SUCCESS then
     begin
      Result:=VCHIQ_RETRY;
      Exit;
     end;
   
    {Check Service State}
    if (Service.srvstate = VCHIQ_SRVSTATE_FREE) or (Service.srvstate = VCHIQ_SRVSTATE_OPEN) then
     begin
      Break;
     end; 
     
    {$IFDEF VC4VCHIQ_DEBUG}
    if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Remove Service (Id=' + IntToStr(Service.state.id) + ' LocalPort=' + IntToStr(Service.localport) + ' ServiceState=' + VCHIQServiceStateToString(Service.srvstate) + ')');
    {$ENDIF}
   end;
 
  {Check Service State}
  if (Status = VCHIQ_SUCCESS) and (Service.srvstate <> VCHIQ_SRVSTATE_FREE) then
   begin
    Exit;
   end;
 
  {Return Success}
  Result:=VCHIQ_SUCCESS;
 finally
  {Unlock Service}
  VCHIQUnlockService(Service);
 end;
end;

{==============================================================================}

procedure VCHIQMarkServiceClosing(Service:PVCHIQ_SERVICE_T);
{From mark_service_closing in vchiq_core.c}
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Mark Service Closing (Service=' + IntToHex(PtrUInt(Service),8) + ')');
 {$ENDIF}
 
 VCHIQMarkServiceClosingInternal(Service,False);
end;

{==============================================================================}

function VCHIQMakeServiceCallback(Service:PVCHIQ_SERVICE_T;Reason:VCHIQ_REASON_T;Header:PVCHIQ_HEADER_T;BulkUserdata:Pointer):VCHIQ_STATUS_T;
{From make_service_callback in vchiq_core.c}
var
 Status:VCHIQ_STATUS_T;
begin
 {}
 Result:=VCHIQ_ERROR;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Make Service Callback (Service=' + IntToHex(PtrUInt(Service),8) + ' Reason=' + VCHIQReasonToString(Reason) + ' Header=' + IntToHex(PtrUInt(Header),8) + ' BulkUserdata=' + IntToHex(PtrUInt(BulkUserdata),8) + ')');
 {$ENDIF}
 
 {Check Service}
 if Service = nil then Exit;
 
 {Call Callback}
 Status:=Service.base.callback(Reason,Header,Service.handle,BulkUserdata);
 if Status = VCHIQ_ERROR then
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Ignoring error from callback to service (Id=' + IntToStr(Service.state.id) + ' Handle=' + IntToHex(Service.handle,8) + ')');
  
   Status:=VCHIQ_SUCCESS;
  end;
 
 Result:=Status;
end;

{==============================================================================}

function VCHIQSetServiceOption(Handle:VCHIQ_SERVICE_HANDLE_T;Option:VCHIQ_SERVICE_OPTION_T;Value:Integer):VCHIQ_STATUS_T;
{From vchiq_set_service_option in vchiq_core.c}
var
 Service:PVCHIQ_SERVICE_T;
 ServiceQuota:PVCHIQ_SERVICE_QUOTA_T;
begin
 {}
 Result:=VCHIQ_ERROR;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Set Service Option (Handle=' + IntToHex(Handle,8) + ' Option=' + VCHIQServiceOptionToString(Option) + ' Value=' + IntToStr(Value) + ')');
 {$ENDIF}

 {Get Service}
 Service:=VCHIQFindServiceByHandle(Handle);
 if Service = nil then Exit;
 try
  {Check Option}
  case Option of
   VCHIQ_SERVICE_OPTION_AUTOCLOSE:begin
     {Set Auto Close}
     Service.auto_close:=Value;
     
     Result:=VCHIQ_SUCCESS;
    end;
   VCHIQ_SERVICE_OPTION_SLOT_QUOTA:begin
     {Get Service Quota}
     ServiceQuota:=@Service.state.service_quotas[Service.localport];
     
     {Check Value}
     if Value = 0 then
      begin
       Value:=Service.state.default_slot_quota;
      end;
     if (Value >= ServiceQuota.slot_use_count) and (Value < Word(not(0))) then
      begin
       {Set Slot Quota}
       ServiceQuota.slot_quota:=Value;
       
       {Check Quota}
       if (ServiceQuota.slot_quota >= ServiceQuota.slot_use_count) and (ServiceQuota.message_quota >= ServiceQuota.message_use_count) then
        begin
         {Signal the service that it may have dropped below its quota}
         SemaphoreSignal(ServiceQuota.quota_event);
        end;
     
       Result:=VCHIQ_SUCCESS;
      end;
    end;  
   VCHIQ_SERVICE_OPTION_MESSAGE_QUOTA:begin
     {Get Service Quota}
     ServiceQuota:=@Service.state.service_quotas[Service.localport];
     
     {Check Value}
     if Value = 0 then
      begin
       Value:=Service.state.default_message_quota;
      end;
     if (Value >= ServiceQuota.message_use_count) and (Value < Word(not(0))) then
      begin
       {Set Message Quota}
       ServiceQuota.message_quota:=Value;
 
       {Check Quota}
       if (ServiceQuota.message_quota >= ServiceQuota.message_use_count) and (ServiceQuota.slot_quota >= ServiceQuota.slot_use_count) then
        begin
         {Signal the service that it may have dropped below its quota}
         SemaphoreSignal(ServiceQuota.quota_event);
        end;
   
       Result:=VCHIQ_SUCCESS;
      end; 
    end;
   VCHIQ_SERVICE_OPTION_SYNCHRONOUS:begin
     {Check Service State}
     if (Service.srvstate = VCHIQ_SRVSTATE_HIDDEN) or (Service.srvstate = VCHIQ_SRVSTATE_LISTENING) then
      begin
       {Set Synchronous}
       Service.sync:=Value;
   
       Result:=VCHIQ_SUCCESS;
      end; 
    end;
   VCHIQ_SERVICE_OPTION_TRACE:begin
     {Set Trace}
     Service.trace:=Value;
   
     Result:=VCHIQ_SUCCESS;
    end;
  end;
 finally
  {Unlock Service}
  VCHIQUnlockService(Service);
 end; 
end;

{==============================================================================}

function VCHIQBulkTransfer(Handle:VCHIQ_SERVICE_HANDLE_T;MemHandle:VCHI_MEM_HANDLE_T;Offset:Pointer;Size:Integer;Userdata:Pointer;Mode:VCHIQ_BULK_MODE_T;Dir:VCHIQ_BULK_DIR_T):VCHIQ_STATUS_T;
{From vchiq_bulk_transfer in vchiq_core.c}

{This function may be called by kernel threads or user threads}
{When called in blocking mode, the userdata field points to a bulk_waiter structure}
var
 Unlock:Boolean;
 MsgType:Integer;
 Bulk:PVCHIQ_BULK_T;
 State:PVCHIQ_STATE_T;
 Status:VCHIQ_STATUS_T;
 Service:PVCHIQ_SERVICE_T;
 Queue:PVCHIQ_BULK_QUEUE_T;
 BulkWaiter:PVCHIQ_BULK_WAITER_T;
 
 Element:VCHIQ_ELEMENT_T;
 Payload:array[0..1] of Integer; //To Do //Continuing //Change this to a VCHIQ_BULK_PAYLOAD ?
begin
 {}
 Result:=VCHIQ_ERROR;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Bulk Transfer (Handle=' + IntToHex(Handle,8) + ' MemHandle=' + IntToHex(MemHandle,8) + ' Offset=' + IntToHex(PtrUInt(Offset),8) + ' Size=' + IntToStr(Size) + ' Userdata=' + IntToHex(PtrUInt(Userdata),8) + ' Mode=' + VCHIQBulkModeToString(Mode) + ' Dir=' + VCHIQBulkDirToString(Dir) + ')');
 {$ENDIF}
 
 {Get Service}
 Service:=VCHIQFindServiceByHandle(Handle);
 if Service = nil then Exit;
 try
  Unlock:=True;
  
  {Check Service}
  if VCHIQCheckService(Service) <> VCHIQ_SUCCESS then Exit;
  
  {Check Service State}
  if Service.srvstate <> VCHIQ_SRVSTATE_OPEN then Exit;

  {Check Mem Handle and Offset}
  if (MemHandle = VCHI_MEM_HANDLE_INVALID) and (Offset = nil) then Exit;
  
  {Check Mode}
  case Mode of
   VCHIQ_BULK_MODE_NOCALLBACK,
   VCHIQ_BULK_MODE_CALLBACK:begin
     BulkWaiter:=nil;
    end;
   VCHIQ_BULK_MODE_BLOCKING:begin
     BulkWaiter:=PVCHIQ_BULK_WAITER_T(Userdata);
     BulkWaiter.event:=SemaphoreCreate(0); {Destroyed when the waiter node is freed}
     BulkWaiter.actual:=0;
     BulkWaiter.bulk:=nil;
    end;
   VCHIQ_BULK_MODE_WAITING:begin
     BulkWaiter:=PVCHIQ_BULK_WAITER_T(Userdata);
     {Bulk:=BulkWaiter.bulk;} {Not Used}
     
     {Unlock Service}
     VCHIQUnlockService(Service);
     Unlock:=False;
     
     {BulkWaiter.bulk:=Bulk;} {Not Used}
     
     Status:=VCHIQ_SUCCESS;
     if SemaphoreWait(BulkWaiter.event) <> ERROR_SUCCESS then
      begin
       Status:=VCHIQ_RETRY;
      end
     else if BulkWaiter.actual = VCHIQ_BULK_ACTUAL_ABORTED then
      begin
       Status:=VCHIQ_ERROR;
      end;
     
     {Return Result}
     Result:=Status;
     Exit;
    end;
   else
    begin
     Exit;
    end;    
  end;
  
  {Get State}
  State:=Service.state;
  
  {Get Queue}
  Queue:=@Service.bulk_rx;
  if Dir = VCHIQ_BULK_TRANSMIT then Queue:=@Service.bulk_tx;

  {Get Message Type}
  MsgType:=VCHIQ_MSG_BULK_RX;
  if Dir = VCHIQ_BULK_TRANSMIT then MsgType:=VCHIQ_MSG_BULK_TX;
  
  {Lock Mutex}
  if MutexLock(Service.bulk_mutex) <> ERROR_SUCCESS then
   begin
    Result:=VCHIQ_RETRY;
    Exit;
   end;
   
   if Queue.local_insert = (Queue.remove + VCHIQ_NUM_SERVICE_BULKS) then
    begin
     {Update Statistics}
     Inc(Service.stats.bulk_stalls);
     
     repeat
      {Unlock Mutex}
      MutexUnlock(Service.bulk_mutex);
      
      {Wait Bulk Remove Event}
      if SemaphoreWait(Service.bulk_remove_event) <> ERROR_SUCCESS then
       begin
        Result:=VCHIQ_RETRY; 
        Exit;
       end;
      
      {Lock Mutex}
      if MutexLock(Service.bulk_mutex) <> ERROR_SUCCESS then
       begin
        Result:=VCHIQ_RETRY; 
        Exit;
       end;
      
     until (Queue.local_insert <> (Queue.remove + VCHIQ_NUM_SERVICE_BULKS));
    end;
  
  {Get Bulk}  
  Bulk:=@Queue.bulks[VCHIQ_BULK_INDEX(Queue.local_insert)];
  Bulk.mode:=Mode;
  Bulk.dir:=Dir;
  Bulk.userdata:=Userdata;
  Bulk.size:=Size;
  Bulk.actual:=VCHIQ_BULK_ACTUAL_ABORTED;
  
  {Prepare Bulk Data}
  if VCHIQPrepareBulkData(Bulk,MemHandle,Offset,Size,Dir) <> VCHIQ_SUCCESS then
   begin
    {Unlock Mutex}
    MutexUnlock(Service.bulk_mutex);
    Exit;
   end; 

  {Memory Barrier}  
  DataMemoryBarrier;
  
  {The slot mutex must be held when the service is being closed, so claim it here to ensure that isn't happening}
  if MutexLock(State.slot_mutex) <> ERROR_SUCCESS then
   begin
    {Complete Bulk}
    VCHIQCompleteBulk(Bulk);
    
    {Unlock Mutex}
    MutexUnlock(Service.bulk_mutex);
   
    Result:=VCHIQ_RETRY; 
    Exit;
   end;
  
  {Check Service State}
  if Service.srvstate <> VCHIQ_SRVSTATE_OPEN then
   begin
    {Unlock Mutex}
    MutexUnlock(State.slot_mutex);
   
    {Complete Bulk}
    VCHIQCompleteBulk(Bulk);
    
    {Unlock Mutex}
    MutexUnlock(Service.bulk_mutex);
   
    Result:=VCHIQ_RETRY; 
    Exit;
   end;
   
  {Check Master}
  if State.is_master then
   begin
    {Update Local Insert}
    Inc(Queue.local_insert);
    
    {Resolve Bulks}
    if VCHIQResolveBulks(Service,Queue) > 0 then
     begin
      if Dir = VCHIQ_BULK_TRANSMIT then
       begin
        {Request Poll}
        VCHIQRequestPoll(State,Service,VCHIQ_POLL_TXNOTIFY);
       end
      else
       begin
        {Request Poll}
        VCHIQRequestPoll(State,Service,VCHIQ_POLL_RXNOTIFY);
       end;       
     end;
   end
  else
   begin
    {Create Payload}
    Payload[0]:=PtrInt(Bulk.data);
    Payload[1]:=Bulk.size;
    
    {Create Element}
    Element.data:=@Payload;
    Element.size:=SizeOf(Payload);
    
    {Queue Message Internal}
    Status:=VCHIQQueueMessageInternal(State,nil,VCHIQ_MAKE_MSG(MsgType,Service.localport,Service.remoteport),@Element,1,SizeOf(Payload),QMFLAGS_IS_BLOCKING or QMFLAGS_NO_MUTEX_LOCK or QMFLAGS_NO_MUTEX_UNLOCK);
    if Status <> VCHIQ_SUCCESS then
     begin
      {Unlock Mutex}
      MutexUnlock(State.slot_mutex);
     
      {Complete Bulk}
      VCHIQCompleteBulk(Bulk);
      
      {Unlock Mutex}
      MutexUnlock(Service.bulk_mutex);
     
      Exit;
     end;
     
    {Update Local Insert}
    Inc(Queue.local_insert);
   end;   
  
  {Unlock Mutex}
  MutexUnlock(State.slot_mutex);

  {Unlock Mutex}
  MutexUnlock(Service.bulk_mutex);
  
  {Unlock Service}
  VCHIQUnlockService(Service);
  Unlock:=False;
  
  Status:=VCHIQ_SUCCESS;

  if BulkWaiter <> nil then
   begin
    BulkWaiter.bulk:=Bulk;
    
    if SemaphoreWait(BulkWaiter.event) <> ERROR_SUCCESS then
     begin
      Status:=VCHIQ_RETRY;
     end
    else if BulkWaiter.actual = VCHIQ_BULK_ACTUAL_ABORTED then
     begin
      Status:=VCHIQ_ERROR;
     end;
   end; 
  
  {Return Result}
  Result:=Status;
 finally
  {Unlock Service}
  if Unlock then VCHIQUnlockService(Service);
 end;
end;

{==============================================================================}

function VCHIQPauseInternal(State:PVCHIQ_STATE_T):VCHIQ_STATUS_T;
{From vchiq_pause_internal in vchiq_core.c}
begin
 {}
 Result:=VCHIQ_ERROR;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Pause Internal (State=' + IntToHex(PtrUInt(State),8) + ')');
 {$ENDIF}
 
 {Check State}
 if State = nil then Exit;
 
 {Check Conn State}
 case State.conn_state of
  VCHIQ_CONNSTATE_CONNECTED:begin
    {Request Pause}
    VCHIQSetConnState(State,VCHIQ_CONNSTATE_PAUSING);
    
    {Request Poll}
    VCHIQRequestPoll(State,nil,VCHIQ_POLL_TERMINATE); {Note: PollType not used if Service is nil}
   end;
  else
   begin
    if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Pause Internal in state ' + VCHIQConnStateToString(State.conn_state));

    {Update Statistics}
    Inc(State.stats.error_count);
    Exit;
   end;   
 end;
 
 Result:=VCHIQ_SUCCESS;
end;

{==============================================================================}

function VCHIQResumeInternal(State:PVCHIQ_STATE_T):VCHIQ_STATUS_T;
{From vchiq_resume_internal in vchiq_core.c}
begin
 {}
 Result:=VCHIQ_ERROR;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Resume Internal (State=' + IntToHex(PtrUInt(State),8) + ')');
 {$ENDIF}
 
 {Check State}
 if State = nil then Exit;
 
 {Check Conn State}
 if State.conn_state = VCHIQ_CONNSTATE_PAUSED then
  begin
   {Request Resume}
   VCHIQSetConnState(State,VCHIQ_CONNSTATE_RESUMING);
   
   {Request Poll}
   VCHIQRequestPoll(State,nil,VCHIQ_POLL_TERMINATE); {Note: PollType not used if Service is nil}
  end
 else
  begin
   {Update Statistics}
   Inc(State.stats.error_count);
   Exit;
  end;
 
 Result:=VCHIQ_SUCCESS;
end;

{==============================================================================}

function VCHIQSendRemoteUse(State:PVCHIQ_STATE_T):VCHIQ_STATUS_T;
{From vchiq_send_remote_use in vchiq_core.c}
var
 Status:VCHIQ_STATUS_T;
begin
 {}
 Result:=VCHIQ_ERROR;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Send Remote Use (State=' + IntToHex(PtrUInt(State),8) + ')');
 {$ENDIF}
 
 {Check State}
 if State = nil then Exit;
 
 {Check Connection State}
 Status:=VCHIQ_RETRY;
 if State.conn_state <> VCHIQ_CONNSTATE_DISCONNECTED then
  begin
   Status:=VCHIQQueueMessageInternal(State,nil,VCHIQ_MAKE_MSG(VCHIQ_MSG_REMOTE_USE,0,0),nil,0,0,0);
  end; 
 
 Result:=Status;
end;

{==============================================================================}

function VCHIQSendRemoteRelease(State:PVCHIQ_STATE_T):VCHIQ_STATUS_T;
{From vchiq_send_remote_release in vchiq_core.c}
var
 Status:VCHIQ_STATUS_T;
begin
 {}
 Result:=VCHIQ_ERROR;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Send Remote Release (State=' + IntToHex(PtrUInt(State),8) + ')');
 {$ENDIF}
 
 {Check State}
 if State = nil then Exit;
 
 {Check Connection State}
 Status:=VCHIQ_RETRY;
 if State.conn_state <> VCHIQ_CONNSTATE_DISCONNECTED then
  begin
   Status:=VCHIQQueueMessageInternal(State,nil,VCHIQ_MAKE_MSG(VCHIQ_MSG_REMOTE_RELEASE,0,0),nil,0,0,0);
  end; 
 
 Result:=Status;
end;

{==============================================================================}

function VCHIQSendRemoteUseActive(State:PVCHIQ_STATE_T):VCHIQ_STATUS_T;
{From vchiq_send_remote_use_active in vchiq_core.c}
var
 Status:VCHIQ_STATUS_T;
begin
 {}
 Result:=VCHIQ_ERROR;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Send Remote Use Active (State=' + IntToHex(PtrUInt(State),8) + ')');
 {$ENDIF}
 
 {Check State}
 if State = nil then Exit;
 
 {Check Connection State}
 Status:=VCHIQ_RETRY;
 if State.conn_state <> VCHIQ_CONNSTATE_DISCONNECTED then
  begin
   Status:=VCHIQQueueMessageInternal(State,nil,VCHIQ_MAKE_MSG(VCHIQ_MSG_REMOTE_USE_ACTIVE,0,0),nil,0,0,0);
  end; 
 
 Result:=Status;
end;

{==============================================================================}

function VCHIQConnectInternal(State:PVCHIQ_STATE_T;Instance:PVCHIQInstance):VCHIQ_STATUS_T;
{From vchiq_connect_internal in vchiq_core.c}
var
 Index:Integer;
 Service:PVCHIQ_SERVICE_T;
begin
 {}
 Result:=VCHIQ_ERROR;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Connect Internal (State=' + IntToHex(PtrUInt(State),8) + ' Instance=' + IntToHex(PtrUInt(Instance),8) + ')');
 {$ENDIF}
 
 {Check State}
 if State = nil then Exit;
 
 {Check Instance}
 if Instance = nil then Exit;
 
 {Find all services registered to this client and enable them}
 Index:=0;
 Service:=VCHIQNextServiceByInstance(State,Instance,Index);
 while Service <> nil do
  begin
   {Check Service State}
   if Service.srvstate = VCHIQ_SRVSTATE_HIDDEN then
    begin
     {Update Service State}
     VCHIQSetServiceState(Service,VCHIQ_SRVSTATE_LISTENING);
    end;
  
   {Unlock Service}
   VCHIQUnlockService(Service);
   
   {Next Service}
   Service:=VCHIQNextServiceByInstance(State,Instance,Index);
  end;
 
 {Check Connection State}
 if State.conn_state = VCHIQ_CONNSTATE_DISCONNECTED then
  begin
   {Queue Message}
   if VCHIQQueueMessageInternal(State,nil,VCHIQ_MAKE_MSG(VCHIQ_MSG_CONNECT,0,0),nil,0,0,QMFLAGS_IS_BLOCKING) = VCHIQ_RETRY then
    begin
     Result:=VCHIQ_RETRY;
     Exit;
    end;
   
   {Update Connection State}
   VCHIQSetConnState(State,VCHIQ_CONNSTATE_CONNECTING);
  end;
  
 {Check Connection State}
 if State.conn_state = VCHIQ_CONNSTATE_CONNECTING then
  begin
   {Wait Connect}
   if SemaphoreWait(State.connect) <> ERROR_SUCCESS then
    begin
     Result:=VCHIQ_RETRY;
     Exit;
    end;
   
   {Update Connection State} 
   VCHIQSetConnState(State,VCHIQ_CONNSTATE_CONNECTED);
   
   {Signal Connect}
   SemaphoreSignal(State.connect);
  end;
 
 {Return Success}
 Result:=VCHIQ_SUCCESS;
end;

{==============================================================================}

function VCHIQShutdownInternal(State:PVCHIQ_STATE_T;Instance:PVCHIQInstance):VCHIQ_STATUS_T;
{From vchiq_shutdown_internal in vchiq_core.c}
var
 Index:Integer;
 Service:PVCHIQ_SERVICE_T;
begin
 {}
 Result:=VCHIQ_ERROR;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Shutdown Internal (State=' + IntToHex(PtrUInt(State),8) + ' Instance=' + IntToHex(PtrUInt(Instance),8) + ')');
 {$ENDIF}
 
 {Check State}
 if State = nil then Exit;
 
 {Check Instance}
 if Instance = nil then Exit;
 
 {Find all services registered to this client and remove them}
 Index:=0;
 Service:=VCHIQNextServiceByInstance(State,Instance,Index);
 while Service <> nil do
  begin
   {Remove Service}
   VCHIQRemoveService(Service.handle);
  
   {Unlock Service}
   VCHIQUnlockService(Service);
   
   {Next Service}
   Service:=VCHIQNextServiceByInstance(State,Instance,Index);
  end;
 
 {Return Success}
 Result:=VCHIQ_SUCCESS;
end;

{==============================================================================}

function VCHIQGetConfig(Instance:PVCHIQInstance;ConfigSize:Integer;Config:PVCHIQ_CONFIG_T):VCHIQ_STATUS_T;
{From vchiq_get_config in vchiq_core.c}
begin
 {}
 Result:=VCHIQ_ERROR;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Get Config (Size=' + IntToStr(ConfigSize) + ' Config=' + IntToHex(PtrUInt(Config),8) + ')');
 {$ENDIF}
 
 {Check Config}
 if Config = nil then Exit;
 
 {Check Size}
 if ConfigSize <> SizeOf(VCHIQ_CONFIG_T) then Exit;

 {Copy Config} 
 Config.max_msg_size:=VCHIQ_MAX_MSG_SIZE;
 Config.bulk_threshold:=VCHIQ_MAX_MSG_SIZE;
 Config.max_outstanding_bulks:=VCHIQ_NUM_SERVICE_BULKS;
 Config.max_services:=VCHIQ_MAX_SERVICES;
 Config.version:=VCHIQ_VERSION;
 Config.version_min:=VCHIQ_VERSION_MIN;
 
 Result:=VCHIQ_SUCCESS;
end;
 
{==============================================================================}

function VCHIQGetPeerVersion(Handle:VCHIQ_SERVICE_HANDLE_T;PeerVersion:PSmallInt):VCHIQ_STATUS_T;
{From vchiq_get_peer_version in vchiq_core.c}
var
 Service:PVCHIQ_SERVICE_T;
begin
 {}
 Result:=VCHIQ_ERROR;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Get Peer Version (Handle=' + IntToHex(Handle,8) + ')');
 {$ENDIF}
 
 {Check Peer Version}
 if PeerVersion = nil then Exit;
 
 {Get Service}
 Service:=VCHIQFindServiceByHandle(Handle);
 if Service = nil then Exit;
 try
  {Check Service}
  if VCHIQCheckService(Service) <> VCHIQ_SUCCESS then Exit;
  
  {Get Peer Version}
  PeerVersion^:=Service.peer_version;
  
  Result:=VCHIQ_SUCCESS;
 finally
  {Unlock Service}
  VCHIQUnlockService(Service);
 end; 
end;
 
{==============================================================================}

function VCHIQAddServiceInternal(State:PVCHIQ_STATE_T;Params:PVCHIQ_SERVICE_PARAMS_T;ServiceState:VCHIQ_SRVSTATE_T;Instance:PVCHIQInstance;TermProc:VCHIQ_USERDATA_TERM_T):PVCHIQ_SERVICE_T;
{From vchiq_add_service_internal in vchiq_core.c}

{Called from application thread when a client or server service is created}
var
 Count:Integer;
 Current:PVCHIQ_SERVICE_T;
 Service:PVCHIQ_SERVICE_T;
 PService:PPVCHIQ_SERVICE_T;
 ServiceQuota:PVCHIQ_SERVICE_QUOTA_T;
begin
 {}
 Result:=nil;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Add Service Internal (State=' + IntToHex(PtrUInt(State),8) + ' Instance=' + IntToHex(PtrUInt(Instance),8) + ')');
 {$ENDIF}
 
 {Check State}
 if State = nil then Exit;
 
 {Check Params}
 if Params = nil then Exit;
 
 {Create Service}
 Service:=AllocMem(SizeOf(VCHIQ_SERVICE_T));
 if Service <> nil then 
  begin
   {Initialize Service}
   Service.base.fourcc:=Params.fourcc;
   Service.base.callback:=Params.callback;
   Service.base.userdata:=Params.userdata;
   Service.handle:=VCHIQ_SERVICE_HANDLE_INVALID;
   Service.ref_count:=1;
   Service.srvstate:=VCHIQ_SRVSTATE_FREE;
   Service.userdata_term:=TermProc;
   Service.localport:=VCHIQ_PORT_FREE;
   Service.remoteport:=VCHIQ_PORT_FREE;
   
   Service.public_fourcc:=Params.fourcc;
   if ServiceState = VCHIQ_SRVSTATE_OPENING then Service.public_fourcc:=VCHIQ_FOURCC_INVALID;
   Service.client_id:=0;
   Service.auto_close:=1;
   Service.sync:=0;
   Service.closing:=0;
   Service.trace:=0;
   Service.poll_flags:=0;
   Service.version:=Params.version;
   Service.version_min:=Params.version_min;
   Service.state:=State;
   Service.instance:=Instance;
   Service.service_use_count:=0;
   VCHIQInitBulkQueue(@Service.bulk_tx);
   VCHIQInitBulkQueue(@Service.bulk_rx);
   Service.remove_event:=SemaphoreCreate(0);
   Service.bulk_remove_event:=SemaphoreCreate(0);
   Service.bulk_mutex:=MutexCreate;
   FillChar(Service.stats,SizeOf(VCHIQ_SERVICE_STATS_T),0);
  end
 else
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Failed to allocate memory for new service');
   Exit;
  end;
 
 {Setup Defaults}
 PService:=nil;
 
 {Lock Mutex}
 MutexLock(State.mutex);
 
 {Prepare to use a previously unused service}
 if State.unused_service < VCHIQ_MAX_SERVICES then
  begin
   PService:=@State.services[State.unused_service];
  end;
  
 {Check Service State}
 if ServiceState = VCHIQ_SRVSTATE_OPENING then
  begin
   for Count:=0 to State.unused_service - 1 do
    begin
     Current:=State.services[Count];
     if Current = nil then 
      begin
       PService:=@State.services[Count];
       Break;
      end;
    end;
  end
 else
  begin
   for Count:=State.unused_service - 1 downto 0 do
    begin
     Current:=State.services[Count];
     if Current = nil then 
      begin
       PService:=@State.services[Count];
      end
     else if (Current.public_fourcc = Params.fourcc) and ((Current.instance <> Instance) or (@Current.base.callback <> @Params.callback)) then
      begin
       {There is another server using this fourcc which doesn't match}
       PService:=nil;
       Break;
      end; 
    end; 
  end;  
  
 {Check PService} 
 if PService <> nil then
  begin
   {Get Local Port}
   Service.localport:=(PService - PPVCHIQ_SERVICE_T(@State.services));
   
   {Check Handle Sequence}
   if VCHIQHandleSequence = 0 then 
    begin
     VCHIQHandleSequence:=VCHIQ_MAX_STATES * VCHIQ_MAX_SERVICES;
    end;
    
   {Get Handle}
   Service.handle:=VCHIQHandleSequence or (State.id * VCHIQ_MAX_SERVICES) or Service.localport;
   
   {Update Handle Sequence}
   VCHIQHandleSequence:=VCHIQHandleSequence + VCHIQ_MAX_STATES * VCHIQ_MAX_SERVICES;
   
   {Update Service}
   PService^:=Service;
   if PService = @State.services[State.unused_service] then
    begin
     {Update Unused Service}
     Inc(State.unused_service);
    end;
  end; 
 
 {Unlock Mutex}
 MutexUnlock(State.mutex);
 
 {Check PService} 
 if PService = nil then 
  begin
   {Free Service}
   FreeMem(Service);
   Exit;
  end;
  
 {Get Service Quota}
 ServiceQuota:=@State.service_quotas[Service.localport];
 ServiceQuota.slot_quota:=State.default_slot_quota;
 ServiceQuota.message_quota:=State.default_message_quota;
 if ServiceQuota.slot_use_count = 0 then
  begin
   ServiceQuota.previous_tx_index:=VCHIQ_SLOT_QUEUE_INDEX_FROM_POS(State.local_tx_pos) - 1;
  end;

 {Bring this service online}
 VCHIQSetServiceState(Service,ServiceState);
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Add Service Internal (ServiceState=' + VCHIQServiceStateToString(ServiceState) + ' Fourcc=' + VCHIQFourccToString(Params.fourcc) + ' LocalPort=' + IntToStr(Service.localport) + ')');
 {$ENDIF}
 
 {Don't unlock the service - leave it with a ref_count of 1}
 Result:=Service;
end;

{==============================================================================}

function VCHIQOpenServiceInternal(Service:PVCHIQ_SERVICE_T;ClientID:Integer):VCHIQ_STATUS_T;
{From vchiq_open_service_internal in vchiq_core.c}
var
 Status:VCHIQ_STATUS_T;
 Body:VCHIQ_ELEMENT_T;
 Payload:VCHIQ_OPEN_PAYLOAD;
begin
 {}
 Result:=VCHIQ_ERROR;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Open Service Internal (Service=' + IntToHex(PtrUInt(Service),8) + ' ClientID=' + IntToStr(ClientID) + ')');
 {$ENDIF}
 
 {Check Service}
 if Service = nil then Exit;
 
 {Create Payload}
 Payload.fourcc:=Service.base.fourcc;
 Payload.client_id:=ClientID;
 Payload.version:=Service.version;
 Payload.version_min:=Service.version_min;
 
 {Create Body}
 Body.data:=@Payload;
 Body.size:=SizeOf(VCHIQ_OPEN_PAYLOAD);
 
 {Set Client Id}
 Service.client_id:=ClientID;
 
 {Use Service Internal}
 VCHIQUseServiceInternal(Service);
 
 {Queue Message}
 Status:=VCHIQQueueMessageInternal(Service.state,nil,VCHIQ_MAKE_MSG(VCHIQ_MSG_OPEN,Service.localport,0),@Body,1,SizeOf(VCHIQ_OPEN_PAYLOAD),QMFLAGS_IS_BLOCKING);
 if Status = VCHIQ_SUCCESS then
  begin
   {Wait for the ACK/NAK}
   if SemaphoreWait(Service.remove_event) <> ERROR_SUCCESS then
    begin
     Status:=VCHIQ_RETRY;
     
     {Release Service Internal}
     VCHIQReleaseServiceInternal(Service);
    end
   else if (Service.srvstate <> VCHIQ_SRVSTATE_OPEN) and (Service.srvstate <> VCHIQ_SRVSTATE_OPENSYNC) then
    begin
     if Service.srvstate <> VCHIQ_SRVSTATE_CLOSEWAIT then
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Open Service Internal (State=' + IntToStr(Service.state.id) + ' ServiceState=' + VCHIQServiceStateToString(Service.srvstate) + ' ReferenceCount=' + IntToStr(Service.ref_count) + ')');
      end;
     Status:=VCHIQ_ERROR;
     
     {Update Statistics}
     Inc(Service.stats.error_count);
     
     {Release Service Internal}
     VCHIQReleaseServiceInternal(Service);
    end;
  end;
 
 {Return Status}
 Result:=Status;
end;

{==============================================================================}

procedure VCHIQReleaseServiceMessages(Service:PVCHIQ_SERVICE_T);
{From release_service_messages in vchiq_core.c}
var
 Data:PByte;
 Index:Integer;
 MsgId:Integer;
 Port:Integer;
 SlotLast:Integer;
 DataPos:Integer;
 DataEnd:Integer;
 State:PVCHIQ_STATE_T;
 Header:PVCHIQ_HEADER_T;
 SLotInfo:PVCHIQ_SLOT_INFO_T;
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Release Service Messages (Service=' + IntToHex(PtrUInt(Service),8) + ')');
 {$ENDIF}
 
 {Check Service}
 if Service = nil then Exit;
 
 {Get State}
 State:=Service.state;
 
 {Get Slot Last}
 SlotLast:=State.remote.slot_last;
 
 {Release any claimed messages aimed at this service}
 if Service.sync <> 0 then
  begin
   {Get Header}
   Header:=PVCHIQ_HEADER_T(VCHIQ_SLOT_DATA_FROM_INDEX(State,State.remote.slot_sync));
   
   {Check Port}
   if VCHIQ_MSG_DSTPORT(Header.msgid) = Service.localport then
    begin
     {Release Message Sync}
     VCHIQReleaseMessageSync(State,Header);
    end;
   
   Exit;
  end;
 
 {Check Slots}
 for Index:=State.remote.slot_first to SlotLast do
  begin
   {Get Slot Info}
   SlotInfo:=VCHIQ_SLOT_INFO_FROM_INDEX(State,Index);
   
   {Check Counts}
   if SlotInfo.release_count <> SlotInfo.use_count then
    begin
     {Get Data}
     Data:=VCHIQ_SLOT_DATA_FROM_INDEX(State,Index);
     
     {Get End}
     DataEnd:=VCHIQ_SLOT_SIZE;
     if Data = State.rx_data then
      begin
       {This buffer is still being read from (Stop at the current read position)}
       DataEnd:=State.rx_pos and VCHIQ_SLOT_MASK;
      end;
      
     {Get Position}
     DataPos:=0;
     
     while DataPos < DataEnd do
      begin
       {Get Header}
       Header:=PVCHIQ_HEADER_T(Data + DataPos);
       
       {Get Message}
       MsgId:=Header.msgid;
       Port:=VCHIQ_MSG_DSTPORT(MsgId);
       
       {Check Port and Message Id}
       if (Port = Service.localport) and ((MsgId and VCHIQ_MSGID_CLAIMED) <> 0) then
        begin
         {$IFDEF VC4VCHIQ_DEBUG}
         if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Release Service Messages (Header=' + IntToHex(PtrUInt(Header),8) + ')');
         {$ENDIF}
        
         {Release Slot}
         VCHIQReleaseSlot(State,SlotInfo,Header,nil);
        end;
        
       {Update Position}
       DataPos:=DataPos + VCHIQCalcStride(Header.size);
       
       {Check Position}
       if DataPos > VCHIQ_SLOT_SIZE then
        begin
         if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Invalid slot position (Position=' + IntToStr(DataPos) + ' Header=' + IntToHex(PtrUInt(Header),8) + ' MsgId=' + IntToHex(MsgId,8) + ' Size=' + IntToStr(Header.size) + ')');
        end;
      end;
    end;
  end;
end;

{==============================================================================}

function VCHIQDoAbortBulks(Service:PVCHIQ_SERVICE_T):Boolean;
{From do_abort_bulks in vchiq_core.c}
var
 Status:VCHIQ_STATUS_T;
begin
 {}
 Result:=False;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Do Abort Bulks (Service=' + IntToHex(PtrUInt(Service),8) + ')');
 {$ENDIF}
 
 {Check Service}
 if Service = nil then Exit;
 
 {Abort any outstanding bulk transfers}
 if MutexLock(Service.bulk_mutex) <> ERROR_SUCCESS then Exit;
 
 {Abort Outstanding Bulks}
 VCHIQAbortOutstandingBulks(Service,@Service.bulk_tx);
 VCHIQAbortOutstandingBulks(Service,@Service.bulk_rx);
    
 {Unlock Mutex}
 MutexUnlock(Service.bulk_mutex);  
 
 {Notify Bulks}
 Status:=VCHIQNotifyBulks(Service,@Service.bulk_tx,False); {Not RetryPoll}
 if Status = VCHIQ_SUCCESS then
  begin
   Status:=VCHIQNotifyBulks(Service,@Service.bulk_rx,False); {Not RetryPoll}
  end;

 {Return Result}
 Result:=(Status = VCHIQ_SUCCESS);
end;

{==============================================================================}

function VCHIQCloseServiceComplete(Service:PVCHIQ_SERVICE_T;FailState:VCHIQ_SRVSTATE_T):VCHIQ_STATUS_T;
{From close_service_complete in vchiq_core.c}
var 
 Count:Integer;
 IsServer:Boolean;
 UseCount:Integer;
 Status:VCHIQ_STATUS_T;
 NewState:VCHIQ_SRVSTATE_T;
begin
 {}
 Result:=VCHIQ_ERROR;

 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Close Service Complete (Service=' + IntToHex(PtrUInt(Service),8) + ' FailState=' + VCHIQServiceStateToString(FailState) + ')');
 {$ENDIF}
 
 {Check Service}
 if Service = nil then Exit;
 
 {Check Server}
 IsServer:=(Service.public_fourcc <> VCHIQ_FOURCC_INVALID);
 
 {Check Service State}
 case Service.srvstate of
  VCHIQ_SRVSTATE_OPEN,
  VCHIQ_SRVSTATE_CLOSESENT,
  VCHIQ_SRVSTATE_CLOSERECVD:begin
    {Check Server}
    if IsServer then
     begin
      {Check Auto Close}
      if Service.auto_close <> 0 then
       begin
        Service.client_id:=0;
        Service.remoteport:=VCHIQ_PORT_FREE;
        NewState:=VCHIQ_SRVSTATE_LISTENING;
       end
      else
       begin
        NewState:=VCHIQ_SRVSTATE_CLOSEWAIT;
       end;       
     end
    else
     begin
      NewState:=VCHIQ_SRVSTATE_CLOSED;
     end;

    {Set Service State}
    VCHIQSetServiceState(Service,NewState);
   end;
  VCHIQ_SRVSTATE_LISTENING:begin
    {Nothing}
   end;  
  else
   begin
    if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Service in unexpected state (Handle=' + IntToHex(Service.handle,8) + ' State=' + VCHIQServiceStateToString(Service.srvstate) + ')');
    Exit;
   end;
 end;
 
 {Make Service Callback}
 Status:=VCHIQMakeServiceCallback(Service,VCHIQ_SERVICE_CLOSED,nil,nil);
 if Status <> VCHIQ_RETRY then
  begin
   {Get Use Count}
   UseCount:=Service.service_use_count;
   
   {Complete the close process}
   for Count:=0 to UseCount - 1 do
    begin
     {Cater for cases where close is forced and the client may not close all it's handles}
     VCHIQReleaseServiceInternal(Service);
    end;
    
   Service.client_id:=0;
   Service.remoteport:=VCHIQ_PORT_FREE;
   
   {Check Service State}
   if Service.srvstate = VCHIQ_SRVSTATE_CLOSED then
    begin
     {Free Service Internal}
     VCHIQFreeServiceInternal(Service);
    end
   else if Service.srvstate = VCHIQ_SRVSTATE_CLOSEWAIT then 
    begin
     {Check Server}
     if IsServer then Service.closing:=0;
     
     {Signal Remove Event}
     SemaphoreSignal(Service.remove_event);
    end;
  end
 else
  begin
   {Set Service State}
   VCHIQSetServiceState(Service,FailState);
  end;  
  
 {Return Status} 
 Result:=Status;
end;

{==============================================================================}

function VCHIQCloseServiceInternal(Service:PVCHIQ_SERVICE_T;CloseReceived:Boolean):VCHIQ_STATUS_T;
{From vchiq_close_service_internal in vchiq_core.c}

{Called by the slot handler}
var
 IsServer:Boolean;
 State:PVCHIQ_STATE_T;
 Status:VCHIQ_STATUS_T;
begin
 {}
 Result:=VCHIQ_ERROR;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Close Service Internal (Service=' + IntToHex(PtrUInt(Service),8) + ' CloseReceived=' + BoolToStr(CloseReceived) + ')');
 {$ENDIF}
 
 {Check Service}
 if Service = nil then Exit;
 
 {Get State}
 State:=Service.state;
 
 {Check Server}
 IsServer:=(Service.public_fourcc <> VCHIQ_FOURCC_INVALID);
 
 {Setup Defaults}
 Status:=VCHIQ_SUCCESS;
 try
  {Check Service State}
  case Service.srvstate of
   VCHIQ_SRVSTATE_CLOSED,
   VCHIQ_SRVSTATE_HIDDEN,
   VCHIQ_SRVSTATE_LISTENING,
   VCHIQ_SRVSTATE_CLOSEWAIT:begin
     {Check Close Received}
     if CloseReceived then
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Close Service Internal (ServiceState=' + VCHIQServiceStateToString(Service.srvstate) + ')');
      end
     else if IsServer then 
      begin
       {Check Service State}
       if Service.srvstate = VCHIQ_SRVSTATE_LISTENING then
        begin
         Status:=VCHIQ_ERROR;
        end
       else
        begin 
         {Reset ClientId and RemotePort}       
         Service.client_id:=0;
         Service.remoteport:=VCHIQ_PORT_FREE;
         
         {Check Service State}
         if Service.srvstate = VCHIQ_SRVSTATE_CLOSEWAIT then 
          begin
           {Set Service State}
           VCHIQSetServiceState(Service,VCHIQ_SRVSTATE_LISTENING);
          end;
        end; 
        
       {Signal Remove Event}
       SemaphoreSignal(Service.remove_event);
      end
     else
      begin
       {Free Service Internal}
       VCHIQFreeServiceInternal(Service);
      end;     
    end;
   VCHIQ_SRVSTATE_OPENING:begin
     {Check Close Received}
     if CloseReceived then
      begin
       {The open was rejected (Tell the user)}
       VCHIQSetServiceState(Service,VCHIQ_SRVSTATE_CLOSEWAIT);
       
       {Signal Remove Event}
       SemaphoreSignal(Service.remove_event);
      end
     else
      begin
       {Shutdown mid-open (Let the other side know)}
       Status:=VCHIQQueueMessageInternal(State,Service,VCHIQ_MAKE_MSG(VCHIQ_MSG_CLOSE,Service.localport,VCHIQ_MSG_DSTPORT(Service.remoteport)),nil,0,0,0);
      end;     
    end;
   VCHIQ_SRVSTATE_OPENSYNC,VCHIQ_SRVSTATE_OPEN:begin
     {Check Service State}
     if Service.srvstate = VCHIQ_SRVSTATE_OPENSYNC then
      begin
       {Lock Mutex}
       MutexLock(State.sync_mutex);
      end;
     
     {Check Master and Close Received}
     if State.is_master or CloseReceived then
      begin
       {Do Abort Bulks}
       if not VCHIQDoAbortBulks(Service) then
        begin
         Status:=VCHIQ_RETRY;
        end;
      end;
      
     {Releaese Service Messages} 
     VCHIQReleaseServiceMessages(Service); 
     
     if Status = VCHIQ_SUCCESS then
      begin
       {Queue Message Internal}
       Status:=VCHIQQueueMessageInternal(State,Service,VCHIQ_MAKE_MSG(VCHIQ_MSG_CLOSE,Service.localport,VCHIQ_MSG_DSTPORT(Service.remoteport)),nil,0,0,QMFLAGS_NO_MUTEX_UNLOCK);
      end;
           
     if Status = VCHIQ_SUCCESS then
      begin
       {Check Close Received}
       if not CloseReceived then
        begin
         {Change the state while the mutex is  still held}
         VCHIQSetServiceState(Service,VCHIQ_SRVSTATE_CLOSESENT);
         
         {Unlock Mutex}
         MutexUnlock(State.slot_mutex);
         
         {Check Sync}
         if Service.sync <> 0 then
          begin
           {Unlock Mutex}
           MutexUnlock(State.sync_mutex);
          end;
          
         Exit;
        end;
      end
     else if Service.srvstate = VCHIQ_SRVSTATE_OPENSYNC then
      begin
       {Unlock Mutex}
       MutexUnlock(State.sync_mutex);
       Exit;
      end
     else
      begin
       Exit;
      end;      
     
     {Change the state while the mutex is still held}
     VCHIQSetServiceState(Service,VCHIQ_SRVSTATE_CLOSERECVD);
     
     {Unlock Mutex}
     MutexUnlock(State.slot_mutex);
     
     {Check Sync}
     if Service.sync <> 0 then
      begin
       {Unlock Mutex}
       MutexUnlock(State.sync_mutex);
      end;
     
     {Close Service Complete}
     Status:=VCHIQCloseServiceComplete(Service,VCHIQ_SRVSTATE_CLOSERECVD);
    end;
   VCHIQ_SRVSTATE_CLOSESENT:begin
     {Check Close Received}
     if not CloseReceived then
      begin
       {This happens when a process is killed mid-close}
       Exit;
      end;     
      
     {Check Master} 
     if not State.is_master then
      begin
       {Do Abort Bulks}
       if not VCHIQDoAbortBulks(Service) then
        begin
         Status:=VCHIQ_RETRY;
         Exit;
        end;
      end;
     
     if Status = VCHIQ_SUCCESS then
      begin
       {Close Service Complete}    
       Status:=VCHIQCloseServiceComplete(Service,VCHIQ_SRVSTATE_CLOSERECVD);
      end; 
    end;
   VCHIQ_SRVSTATE_CLOSERECVD:begin
     {Check Close Received and Server}
     if not CloseReceived and IsServer then
      begin
       {Force into LISTENING mode}
       VCHIQSetServiceState(Service,VCHIQ_SRVSTATE_LISTENING);
      end;
     
     {Close Service Complete}    
     Status:=VCHIQCloseServiceComplete(Service,VCHIQ_SRVSTATE_CLOSERECVD);
    end;
   else
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Close Service Internal (CloseReceived=' + BoolToStr(CloseReceived) + ' ServiceState=' + VCHIQServiceStateToString(Service.srvstate) + ')');
    end;   
  end;
 finally
  {Return Status}
  Result:=Status;
 end; 
end;

{==============================================================================}

procedure VCHIQMarkServiceClosingInternal(Service:PVCHIQ_SERVICE_T;SlotHandler:Boolean);
{From mark_service_closing_internal in vchiq_core.c}
var
 State:PVCHIQ_STATE_T;
 ServiceQuota:PVCHIQ_SERVICE_QUOTA_T;
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Mark Service Closing Internal (Service=' + IntToHex(PtrUInt(Service),8) + ' SlotHandler=' + BoolToStr(SlotHandler) + ')');
 {$ENDIF}
 
 {Check Service}
 if Service = nil then Exit;
 
 {Get State}
 State:=Service.state;
 
 Service.closing:=1;
 
 {Synchronise with other threads}
 MutexLock(State.recycle_mutex);
 MutexUnlock(State.recycle_mutex);
 
 if not(SlotHandler) or (State.conn_state <> VCHIQ_CONNSTATE_PAUSE_SENT) then
  begin
   {If we're pausing then the slot_mutex is held until resume by the slot handler.  Therefore don't try to 
    acquire this mutex if we're the slot handler and in the pause sent state. We don't need to in this case anyway.}
   MutexLock(State.slot_mutex);
   MutexUnlock(State.slot_mutex);
  end;
 
 {Get Service Quota}
 ServiceQuota:=@State.service_quotas[Service.localport];
 
 {Unblock any sending thread}
 SemaphoreSignal(ServiceQuota.quota_event);
end;

{==============================================================================}

procedure VCHIQTerminateServiceInternal(Service:PVCHIQ_SERVICE_T);
{From vchiq_terminate_service_internal in vchiq_core.c}

{Called from the application process upon process death}
var
 State:PVCHIQ_STATE_T;
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Terminate Service Internal (Service=' + IntToHex(PtrUInt(Service),8) + ')');
 {$ENDIF}

 {Check Service}
 if Service = nil then Exit;
 
 {Get State}
 State:=Service.state;
 
 {Mark Closing}
 VCHIQMarkServiceClosing(Service);
 
 {Mark the service for removal by the slot handler}
 VCHIQRequestPoll(State,Service,VCHIQ_POLL_REMOVE);
end; 

{==============================================================================}

procedure VCHIQFreeServiceInternal(Service:PVCHIQ_SERVICE_T);
{From vchiq_free_service_internal in vchiq_core.c}

{Called from the slot handler}
var
 State:PVCHIQ_STATE_T;
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Free Service Internal (Service=' + IntToHex(PtrUInt(Service),8) + ')');
 {$ENDIF}
 
 {Check Service}
 if Service = nil then Exit;
 
 {Get State}
 State:=Service.state;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Free Service Internal (Id=' + IntToStr(State.id) + ' LocalPort=' + IntToStr(Service.localport) + ')');
 {$ENDIF}
 
 {Check Service State}
 case Service.srvstate of
  VCHIQ_SRVSTATE_OPENING,
  VCHIQ_SRVSTATE_CLOSED,
  VCHIQ_SRVSTATE_HIDDEN,
  VCHIQ_SRVSTATE_LISTENING,
  VCHIQ_SRVSTATE_CLOSEWAIT:begin
    {Nothing}
   end;
  else
   begin
    if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Free Service Internal (Id=' + IntToStr(State.id) + ' LocalPort=' + IntToStr(Service.localport) + ' ServiceState=' + VCHIQServiceStateToString(Service.srvstate) + ')');
    Exit;
   end;   
 end;
 
 {Set Service State}
 VCHIQSetServiceState(Service,VCHIQ_SRVSTATE_FREE);
 
 {Signal Remove}
 SemaphoreSignal(Service.remove_event);
 
 {Release the initial lock}
 VCHIQUnlockService(Service);
end;

{==============================================================================}

function VCHIQGetClientID(Handle:VCHIQ_SERVICE_HANDLE_T):Integer;
{From vchiq_get_client_id in vchiq_core.c}
var
 Service:PVCHIQ_SERVICE_T;
begin
 {}
 Result:=0;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Get Client ID (Handle=' + IntToHex(Handle,8) + ')');
 {$ENDIF}

 {Get Service} 
 Service:=VCHIQFindServiceByHandle(Handle);
 if Service <> nil then
  begin
    Result:=Service.client_id;
    
    {Unlock Service}
    VCHIQUnlockService(Service);
  end;
end;

{==============================================================================}

function VCHIQGetServiceUserData(Handle:VCHIQ_SERVICE_HANDLE_T):Pointer;
{From vchiq_get_service_userdata in vchiq_core.c}
var
 Service:PVCHIQ_SERVICE_T;
begin
 {}
 Result:=nil;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Get Service UserData (Handle=' + IntToHex(Handle,8) + ')');
 {$ENDIF}
 
 Service:=VCHIQHandleToService(Handle);
 if Service <> nil then
  begin
   Result:=Service.base.userdata;
  end;
end;

{==============================================================================}

function VCHIQGetServiceFourcc(Handle:VCHIQ_SERVICE_HANDLE_T):Integer;
{From vchiq_get_service_fourcc in vchiq_core.c}
var
 Service:PVCHIQ_SERVICE_T;
begin
 {}
 Result:=0;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Get Service Fourcc (Handle=' + IntToHex(Handle,8) + ')');
 {$ENDIF}
 
 Service:=VCHIQHandleToService(Handle);
 if Service <> nil then
  begin
   Result:=Service.base.fourcc;
  end;
end;

{==============================================================================}

procedure VCHIQLockService(Service:PVCHIQ_SERVICE_T);
{From lock_service in vchiq_core.c}
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Lock Service (Service=' + IntToHex(PtrUInt(Service),8) + ')');
 {$ENDIF}

 {Lock Spinlock} 
 SpinLock(VCHIQServiceLock);
  
 {Check Service}
 if (Service <> nil) and (Service.ref_count > 0) then
  begin
   {Update Reference Count}
   Inc(Service.ref_count);
  end
 else
  begin
   {Unlock Spinlock}
   SpinUnlock(VCHIQServiceLock);
   
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Lock Service: ref_count = 0');
   Exit;
  end;  
  
 {Unlock Spinlock}
 SpinUnlock(VCHIQServiceLock);
end;

{==============================================================================}

procedure VCHIQUnlockService(Service:PVCHIQ_SERVICE_T);
{From unlock_service in vchiq_core.c}
var
 State:PVCHIQ_STATE_T;
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Unlock Service (Service=' + IntToHex(PtrUInt(Service),8) + ')');
 {$ENDIF}
 
 {Lock Spinlock}
 if SpinLock(VCHIQServiceLock) <> ERROR_SUCCESS then Exit;

 {Check Service}
 if (Service <> nil) and (Service.ref_count > 0) then
  begin
   {Update Reference Count}
   Dec(Service.ref_count);
   
   {$IFDEF VC4VCHIQ_DEBUG}
   if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Unlock Service (ref_count=' + IntToStr(Service.ref_count) + ')'); 
   {$ENDIF}
   
   {Check Reference Count}
   if Service.ref_count = 0 then
    begin
     {Check Service State}
     if Service.srvstate <> VCHIQ_SRVSTATE_FREE then
      begin
       {Unlock Spinlock}
       SpinUnlock(VCHIQServiceLock);
       
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Unlock Service: srvstate <> VCHIQ_SRVSTATE_FREE');
       Exit;
      end;
      
     {Get State} 
     State:=Service.state;
     
     {Free Service Port}
     State.services[Service.localport]:=nil;
    end
   else
    begin
     Service:=nil;
    end;    
  end
 else
  begin
   {Unlock Spinlock}
   SpinUnlock(VCHIQServiceLock);
   
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Unlock Service: ref_count = 0');
   Exit;
  end;  

 {Unlock Spinlock}
 SpinUnlock(VCHIQServiceLock);
 
 {Check Service}
 if Service <> nil then
  begin
   {Check termination function}
   if Assigned(Service.userdata_term) then
    begin
     Service.userdata_term(Service.base.userdata);
    end;
   
   {Destroy Semaphores and Mutexes}
   SemaphoreDestroy(Service.remove_event);
   SemaphoreDestroy(Service.bulk_remove_event);
   MutexDestroy(Service.bulk_mutex);
   
   {Invalidate Semaphores and Mutexes}
   Service.remove_event:=INVALID_HANDLE_VALUE;
   Service.bulk_remove_event:=INVALID_HANDLE_VALUE;
   Service.bulk_mutex:=INVALID_HANDLE_VALUE;
   
   {Free Service}
   FreeMem(Service);
  end;
end;

{==============================================================================}

function VCHIQHandleToService(Handle:VCHIQ_SERVICE_HANDLE_T):PVCHIQ_SERVICE_T;
{From handle_to_service in vchiq_core.h}
var
 State:PVCHIQ_STATE_T;
begin
 {}
 Result:=nil;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Handle To Service (Handle=' + IntToHex(Handle,8) + ')');
 {$ENDIF}
 
 {Get State}
 State:=VCHIQStates[(Handle div VCHIQ_MAX_SERVICES) and (VCHIQ_MAX_STATES - 1)];
 if State = nil then Exit;

 {Get Service}
 Result:=State.services[Handle and (VCHIQ_MAX_SERVICES - 1)];
end;

{==============================================================================}

function VCHIQFindServiceByHandle(Handle:VCHIQ_SERVICE_HANDLE_T):PVCHIQ_SERVICE_T;
{From find_service_by_handle in vchiq_core.c}
var
 Service:PVCHIQ_SERVICE_T;
begin
 {}
 Result:=nil;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Find Service By Handle (Handle=' + IntToHex(Handle,8) + ')');
 {$ENDIF}
 
 {Lock Spinlock}
 if SpinLock(VCHIQServiceLock) <> ERROR_SUCCESS then Exit;
 
 {Get Service}
 Service:=VCHIQHandleToService(Handle);
 if (Service <> nil) and (Service.srvstate <> VCHIQ_SRVSTATE_FREE) and (Service.handle = Handle) then
  begin
   {Check Reference Count}
   if Service.ref_count = 0 then
    begin
     {Unlock Spinlock}
     SpinUnlock(VCHIQServiceLock);
    
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Find Service: ref_count = 0');
     Exit;
    end;
    
   {Update Reference Count}
   Inc(Service.ref_count);
  end
 else
  begin
   Service:=nil;
  end;  
 
 {Unlock Spinlock}
 SpinUnlock(VCHIQServiceLock);
 
 {Check Service}
 if Service = nil then
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Find Service: Invalid service handle ' + IntToHex(Handle,8));
  end;
  
 {Return Service}
 Result:=Service;
end;

{==============================================================================}

function VCHIQFindServiceByPort(State:PVCHIQ_STATE_T;LocalPort:Integer):PVCHIQ_SERVICE_T;
{From find_service_by_port in vchiq_core.c}
var
 Service:PVCHIQ_SERVICE_T;
begin
 {}
 Result:=nil;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Find Service By Port (State=' + IntToHex(PtrUInt(State),8) + ' LocalPort=' + IntToStr(LocalPort) + ')');
 {$ENDIF}
 
 {Check State}
 if State = nil then Exit;
 
 {Set Defaults}
 Service:=nil;
 
 {Check Port}
 if LocalPort <= VCHIQ_PORT_MAX then
  begin
   {Lock Spinlock}
   if SpinLock(VCHIQServiceLock) <> ERROR_SUCCESS then Exit;
 
   {Get Service}
   Service:=state.services[LocalPort];
   if (Service <> nil) and (Service.srvstate <> VCHIQ_SRVSTATE_FREE) then
    begin
     {Check Reference Count}
     if Service.ref_count = 0 then
      begin
       {Unlock Spinlock}
       SpinUnlock(VCHIQServiceLock);
      
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Find Service: ref_count = 0');
       Exit;
      end;
    
     {Update Reference Count}
     Inc(Service.ref_count);
    end
   else
    begin
     Service:=nil;
    end;  
 
   {Unlock Spinlock}
   SpinUnlock(VCHIQServiceLock);
  end;
 
 {Check Service}
 if Service = nil then
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Find Service: Invalid port ' + IntToStr(LocalPort));
  end;
  
 {Return Service}
 Result:=Service;
end;

{==============================================================================}

function VCHIQFindServiceForInstance(Instance:PVCHIQInstance;Handle:VCHIQ_SERVICE_HANDLE_T):PVCHIQ_SERVICE_T;
{From find_service_for_instance in vchiq_core.c}
var
 Service:PVCHIQ_SERVICE_T;
begin
 {}
 Result:=nil;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Find Service For Instance (Instance=' + IntToHex(PtrUInt(Instance),8) + ' Handle=' + IntToHex(Handle,8) + ')');
 {$ENDIF}
 
 {Check Instance}
 if Instance = nil then Exit;
 
 {Lock Spinlock}
 if SpinLock(VCHIQServiceLock) <> ERROR_SUCCESS then Exit;
 
 {Get Service}
 Service:=VCHIQHandleToService(Handle);
 if (Service <> nil) and (Service.srvstate <> VCHIQ_SRVSTATE_FREE) and (Service.handle = Handle) and (Service.instance = Instance) then
  begin
   {Check Reference Count}
   if Service.ref_count = 0 then
    begin
     {Unlock Spinlock}
     SpinUnlock(VCHIQServiceLock);
    
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Find Service: ref_count = 0');
     Exit;
    end;
    
   {Update Reference Count}
   Inc(Service.ref_count);
  end
 else
  begin
   Service:=nil;
  end;  
 
 {Unlock Spinlock}
 SpinUnlock(VCHIQServiceLock);
 
 {Check Service}
 if Service = nil then
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Find Service: Invalid service handle ' + IntToHex(Handle,8));
  end;
  
 {Return Service}
 Result:=Service;
end;

{==============================================================================}

function VCHIQFindClosedServiceForInstance(Instance:PVCHIQInstance;Handle:VCHIQ_SERVICE_HANDLE_T):PVCHIQ_SERVICE_T;
{From find_closed_service_for_instance in vchiq_core.c}
var
 Service:PVCHIQ_SERVICE_T;
begin
 {}
 Result:=nil;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Find Closed Service For Instance (Instance=' + IntToHex(PtrUInt(Instance),8) + ' Handle=' + IntToHex(Handle,8) + ')');
 {$ENDIF}
 
 {Check Instance}
 if Instance = nil then Exit;
 
 {Lock Spinlock}
 if SpinLock(VCHIQServiceLock) <> ERROR_SUCCESS then Exit;
 
 {Get Service}
 Service:=VCHIQHandleToService(Handle);
 if (Service <> nil) and ((Service.srvstate = VCHIQ_SRVSTATE_FREE) or (Service.srvstate = VCHIQ_SRVSTATE_CLOSED)) and (Service.handle = Handle) and (Service.instance = Instance) then
  begin
   {Check Reference Count}
   if Service.ref_count = 0 then
    begin
     {Unlock Spinlock}
     SpinUnlock(VCHIQServiceLock);
    
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Find Closed Service: ref_count = 0');
     Exit;
    end;
    
   {Update Reference Count}
   Inc(Service.ref_count);
  end
 else
  begin
   Service:=nil;
  end;  
 
 {Unlock Spinlock}
 SpinUnlock(VCHIQServiceLock);
 
 {Check Service}
 if Service = nil then
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Find Closed Service: Invalid service handle ' + IntToHex(Handle,8));
  end;
  
 {Return Service}
 Result:=Service;
end;

{==============================================================================}

function VCHIQNextServiceByInstance(State:PVCHIQ_STATE_T;Instance:PVCHIQInstance;var Index:Integer):PVCHIQ_SERVICE_T;
{From next_service_by_instance in vchiq_core.c}
var
 Service:PVCHIQ_SERVICE_T;
 Current:PVCHIQ_SERVICE_T;
begin
 {}
 Result:=nil;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Next Service By Instance (State=' + IntToHex(PtrUInt(State),8) + ' Instance=' + IntToHex(PtrUInt(Instance),8) + ' Index=' + IntToStr(Index) + ')');
 {$ENDIF}
 
 {Check State}
 if State = nil then Exit;
 
 {Check Instance}
 if Instance = nil then Exit;
 
 {Set Defaults}
 Service:=nil;
 
 {Lock Spinlock}
 if SpinLock(VCHIQServiceLock) <> ERROR_SUCCESS then Exit;
 
 while Index < State.unused_service do
  begin
   {Get Service}
   Current:=State.services[Index];
   
   {Update Index}
   Inc(Index);
   
   {Check Service}
   if (Current <> nil) and (Current.srvstate <> VCHIQ_SRVSTATE_FREE) and (Current.instance = Instance) then
    begin
     Service:=Current;
     
     {Check Reference Count}
     if Service.ref_count = 0 then
      begin
       {Unlock Spinlock}
       SpinUnlock(VCHIQServiceLock);
      
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Next Service: ref_count = 0');
       Exit;
      end;
     
     {Update Reference Count}
     Inc(Service.ref_count);
     
     Break;
    end;
  end;
 
 {Unlock Spinlock}
 SpinUnlock(VCHIQServiceLock);
 
 {Return Service}
 Result:=Service;
end;

{==============================================================================}

procedure VCHIQRequestPoll(State:PVCHIQ_STATE_T;Service:PVCHIQ_SERVICE_T;PollType:VCHIQ_POLL_T);
{From request_poll in vchiq_core.c}
var
 Value:LongWord;
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Request Poll (State=' + IntToHex(PtrUInt(State),8) + ' Service=' + IntToHex(PtrUInt(Service),8) + ' PollType=' + VCHIQPollTypeToString(PollType) + ')');
 {$ENDIF}
 
 {Check State}
 if State = nil then Exit;
 
 {Check Service}
 if Service <> nil then
  begin
   {Update Poll Flags}
   repeat 
    Value:=Service.poll_flags;
   until InterlockedCompareExchange(Service.poll_flags,Value or (1 shl Ord(PollType)),Value) = Value;
  
   {Update Poll Services}
   repeat
    Value:=State.poll_services[Service.localport shr 5];
   until InterlockedCompareExchange(State.poll_services[Service.localport shr 5],Value or (1 shl (Service.localport and $1f)),Value) = Value;
  end;
 
 {Set Poll Needed}
 State.poll_needed:=1;
 
 {Memory Barrier}
 DataMemoryBarrier;
 
 {... and ensure the slot handler runs}
 VCHIQRemoteEventSignalLocal(@State.local.trigger);
end;

{==============================================================================}

function VCHIQReserveSpace(State:PVCHIQ_STATE_T;Space:Integer;IsBlocking:Boolean):PVCHIQ_HEADER_T;
{From reserve_space in vchiq_core.c}

{Called from VCHIQQueueMessageInternal, by the slot handler and application threads, with slot_mutex held}
var
 TxPos:Integer;
 SlotSpace:Integer;
 SlotIndex:Integer;
 Header:PVCHIQ_HEADER_T;
 Local:PVCHIQ_SHARED_STATE_T;
begin
 {}
 Result:=nil;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Reserve Space (State=' + IntToHex(PtrUInt(State),8) + ' Space=' + IntToStr(Space) + ' IsBlocking=' + BoolToStr(IsBlocking) + ')');
 {$ENDIF}
 
 {Check State}
 if State = nil then Exit;
 
 {Get Local}
 Local:=State.local;
 
 {Get TxPos and SlotSpace}
 TxPos:=State.local_tx_pos;
 SlotSpace:=VCHIQ_SLOT_SIZE - (TxPos and VCHIQ_SLOT_MASK);
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Reserve Space (TxPos=' + IntToStr(TxPos) + ' SlotSpace=' + IntToStr(SlotSpace) + ')');
 {$ENDIF}
 
 if Space > SlotSpace then
  begin
   //To Do //Continuing //Log Warning on tx_data = nil
   
   {Fill the remaining space with padding}
   Header:=PVCHIQ_HEADER_T(State.tx_data + (TxPos and VCHIQ_SLOT_MASK));
   Header.msgid:=VCHIQ_MSGID_PADDING;
   Header.size:=SlotSpace - VCHIQ_HEADER_SIZE; {SizeOf(VCHIQ_HEADER_T)} {Do not include the data member}

   TxPos:=TxPos + SlotSpace;
  end;
  
 {If necessary, get the next slot}
 if (TxPos and VCHIQ_SLOT_MASK) = 0 then
  begin
   {If there is no free slot...}
   if SemaphoreWaitEx(State.slot_available_event,0) <> ERROR_SUCCESS then
    begin
     {Update Statistics}
     Inc(State.stats.slot_stalls);
     
     {But first, flush through the last slot}
     State.local_tx_pos:=TxPos;
     Local.tx_pos:=TxPos;
     VCHIQRemoteEventSignal(@State.remote.trigger);
     
     if not(IsBlocking) or (SemaphoreWait(State.slot_available_event) <> ERROR_SUCCESS) then
      begin
       {No space available}
       Exit;
      end;
    end;    
   
   //To Do //Continuing //Log Error on TxPos = (State.slot_queue_available * VCHIQ_SLOT_SIZE)

   SlotIndex:=Local.slot_queue[VCHIQ_SLOT_QUEUE_INDEX_FROM_POS(TxPos) and VCHIQ_SLOT_QUEUE_MASK];
   State.tx_data:=VCHIQ_SLOT_DATA_FROM_INDEX(State,SlotIndex);
   
   {$IFDEF VC4VCHIQ_DEBUG}
   if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Reserve Space (SlotIndex=' + IntToStr(SlotIndex) + ' TxData=' + IntToHex(PtrUInt(State.tx_data),8) + ')');
   {$ENDIF}
  end;
 
 {Update Local TxPos}
 State.local_tx_pos:=TxPos + Space; 
 
 {Return Header}
 Result:=PVCHIQ_HEADER_T(State.tx_data + (TxPos and VCHIQ_SLOT_MASK));
end;

{==============================================================================}

procedure VCHIQDumpState(Context:Pointer;State:PVCHIQ_STATE_T);
{From vchiq_dump_state in vchiq_core.c}
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Dump State (Context=' + IntToHex(PtrUInt(Context),8) + 'State=' + IntToHex(PtrUInt(State),8) + ')');
 {$ENDIF}
 
 //TO DO //Continuing
 if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: VCHIQDumpState'); //To Do //Temp
 
end;

{==============================================================================}

procedure VCHIQDumpSharedState(Context:Pointer;State:PVCHIQ_STATE_T;Shared:PVCHIQ_SHARED_STATE_T;Text:PChar);
{From vchiq_dump_shared_state in vchiq_core.c}
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Dump Shared State (Context=' + IntToHex(PtrUInt(Context),8) + 'Shared=' + IntToHex(PtrUInt(Shared),8) + ')');
 {$ENDIF}
 
 //TO DO //Continuing 
 if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: VCHIQDumpSharedState'); //To Do //Temp
 
end;

{==============================================================================}

procedure VCHIQDumpServiceState(Context:Pointer;Service:PVCHIQ_SERVICE_T);
{From vchiq_dump_service_state in vchiq_core.c}
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Dump Service State (Context=' + IntToHex(PtrUInt(Context),8) + 'Service=' + IntToHex(PtrUInt(Service),8) + ')');
 {$ENDIF}
 
 //TO DO //Continuing
 if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: VCHIQDumpServiceState'); //To Do //Temp
 
end;

{==============================================================================}

procedure VCHIQSetConnState(State:PVCHIQ_STATE_T;NewState:VCHIQ_CONNSTATE_T); inline;
{From vchiq_set_conn_state in vchiq_core.c}
var
 OldState:VCHIQ_CONNSTATE_T;
begin
 {}
 {Check State}
 if State = nil then Exit;
 
 OldState:=State.conn_state;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Set Conn State (Id=' + IntToStr(State.id) + ' OldState=' + VCHIQConnStateToString(OldState) + ' NewState=' + VCHIQConnStateToString(NewState) + ')');
 {$ENDIF}
 
 State.conn_state:=NewState;
 
 VCHIQPlatformConnStateChanged(State,OldState,NewState);
end;

{==============================================================================}

procedure VCHIQSetServiceState(Service:PVCHIQ_SERVICE_T;NewState:VCHIQ_SRVSTATE_T); inline;
{From vchiq_set_service_state in vchiq_core.c}
begin
 {}
 {Check Service}
 if Service = nil then Exit;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Set Service State (Id=' + IntToStr(Service.state.id) + ' LocalPort=' + IntToStr(Service.localport) + ' ServiceState=' + VCHIQServiceStateToString(Service.srvstate) + ' NewState=' + VCHIQServiceStateToString(NewState) + ')');
 {$ENDIF}
 
 Service.srvstate:=NewState;
end;

{==============================================================================}

function VCHIQQueueMessage(Handle:VCHIQ_SERVICE_HANDLE_T;Elements:PVCHIQ_ELEMENT_ARRAY_T;Count:LongWord):VCHIQ_STATUS_T;
{From vchiq_queue_message in vchiq_core.c}
var
 Size:Integer;
 Index:Integer;
 Service:PVCHIQ_SERVICE_T;
begin
 {}
 Result:=VCHIQ_ERROR;

 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Queue Message (Handle=' + IntToHex(Handle,8) + ' Elements=' + IntToHex(PtrUInt(Elements),8) + ' Count=' + IntToStr(Count) + ')');
 {$ENDIF}

 {Check Elements}
 if Elements = nil then Exit;
 
 {Get Service}
 Service:=VCHIQFindServiceByHandle(Handle);
 if Service = nil then Exit;
 try
  {Check Service}
  if VCHIQCheckService(Service) <> VCHIQ_SUCCESS then Exit;
  
  {Get Size}
  Size:=0;
  for Index:=0 to Count - 1 do
   begin
    if Elements[Index].size <> 0 then
     begin
      if Elements[Index].data = nil then
       begin
        {Update Statistics}
        Inc(Service.stats.error_count);
        Exit;
       end;
      
      {Update Size}
      Size:=Size + Elements[Index].size;
     end;
   end;
   
  {Check Size}
  if Size > VCHIQ_MAX_MSG_SIZE then
   begin
    {Update Statistics}
    Inc(Service.stats.error_count);
    Exit;
   end;
   
  {Check Service State}
  case Service.srvstate of
   VCHIQ_SRVSTATE_OPEN:begin
     {Queue Message Internal}
     Result:=VCHIQQueueMessageInternal(Service.state,Service,VCHIQ_MAKE_MSG(VCHIQ_MSG_DATA,Service.localport,Service.remoteport),Elements,Count,Size,QMFLAGS_IS_BLOCKING);
    end;
   VCHIQ_SRVSTATE_OPENSYNC:begin
     {Queue Message Internal Sync}
     Result:=VCHIQQueueMessageInternalSync(Service.state,Service,VCHIQ_MAKE_MSG(VCHIQ_MSG_DATA,Service.localport,Service.remoteport),Elements,Count,Size,True);
    end;
  end;
 finally
  {Unlock Service}
  VCHIQUnlockService(Service);
 end; 
end;

{==============================================================================}

procedure VCHIQReleaseMessage(Handle:VCHIQ_SERVICE_HANDLE_T;Header:PVCHIQ_HEADER_T);
{From vchiq_release_message in vchiq_core.c}
var
 MsgId:Integer;
 SlotIndex:Integer;
 State:PVCHIQ_STATE_T;
 Slot:PVCHIQ_SLOT_INFO_T;
 Service:PVCHIQ_SERVICE_T;
 Remote:PVCHIQ_SHARED_STATE_T;
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Release Message (Handle=' + IntToHex(Handle,8) + ' Header=' + IntToHex(PtrUInt(Header),8) + ')');
 {$ENDIF}

 {Check Header}
 if Header = nil then Exit;
 
 {Get Service}
 Service:=VCHIQFindServiceByHandle(Handle);
 if Service = nil then Exit;
 try
  {Get State}
  State:=Service.state;
  
  {Get Remote}
  Remote:=State.remote;
  
  {Get Slot Index}
  SlotIndex:=VCHIQ_SLOT_INDEX_FROM_DATA(State,PByte(Header));
  if (SlotIndex >= Remote.slot_first) and (SlotIndex <= Remote.slot_last) then
   begin
    {Get Message Id}
    MsgId:=Header.msgid;
    if (MsgId and VCHIQ_MSGID_CLAIMED) <> 0 then
     begin
      {Get Slot}
      Slot:=VCHIQ_SLOT_INFO_FROM_INDEX(State,SlotIndex);
      
      {Release Slot}
      VCHIQReleaseSlot(State,Slot,Header,Service);
     end;
   end
  else if SlotIndex = Remote.slot_sync then
   begin
    {Release Message Sync}
    VCHIQReleaseMessageSync(State,Header);
   end;
 finally
  {Unlock Service}
  VCHIQUnlockService(Service);
 end; 
end;

{==============================================================================}

function VCHIQQueueMessageInternal(State:PVCHIQ_STATE_T;Service:PVCHIQ_SERVICE_T;MsgId:Integer;Elements:PVCHIQ_ELEMENT_ARRAY_T;Count,Size,Flags:Integer):VCHIQ_STATUS_T;
{From queue_message in vchiq_core.c}

{Called by the slot handler and application threads}
var
 Index:Integer;
 Position:Integer;
 
 TxEndIndex:Integer;
 SlotUseCount:Integer;
 
 Stride:LongWord;
 MsgType:LongWord;
 Header:PVCHIQ_HEADER_T;
 Local:PVCHIQ_SHARED_STATE_T;
 ServiceQuota:PVCHIQ_SERVICE_QUOTA_T;
begin
 {}
 Result:=VCHIQ_ERROR;

 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Queue Message Internal (State=' + IntToHex(PtrUInt(State),8) + ' Service=' + IntToHex(PtrUInt(Service),8) + ' MsgId=' + IntToStr(MsgId) + ')');
 {$ENDIF}
 
 {Check State}
 if State = nil then Exit;
 
 {Set Defaults}
 ServiceQuota:=nil;
 
 {Get Message Type}
 MsgType:=VCHIQ_MSG_TYPE(MsgId);
 
 {Get Local}
 Local:=State.Local;
 
 {Calculate Stride}
 Stride:=VCHIQCalcStride(Size);
 if Stride > VCHIQ_SLOT_SIZE then
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Queue Message: Stride > VCHIQ_SLOT_SIZE');
  end;
 
 {Lock Mutex}
 if ((Flags and QMFLAGS_NO_MUTEX_LOCK) = 0) and (MutexLock(State.slot_mutex) <> ERROR_SUCCESS) then
  begin
   Result:=VCHIQ_RETRY;
   Exit;
  end;
  
 {Check Message Type}
 if MsgType = VCHIQ_MSG_DATA then
  begin
   {Check Service}
   if Service = nil then
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Queue Message: Service = nil');
     
     {Unlock Mutex}
     if (Flags and QMFLAGS_NO_MUTEX_LOCK) = 0 then
      begin
       MutexUnlock(State.slot_mutex);
      end;
     
     Exit;
    end; 
   
   {Check Flags}
   if (Flags and (QMFLAGS_NO_MUTEX_LOCK or QMFLAGS_NO_MUTEX_UNLOCK)) <> 0 then
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Queue Message: (Flags and (QMFLAGS_NO_MUTEX_LOCK or QMFLAGS_NO_MUTEX_UNLOCK)) <> 0');
    
     {Unlock Mutex}
     if (Flags and QMFLAGS_NO_MUTEX_LOCK) = 0 then
      begin
       MutexUnlock(State.slot_mutex);
      end;
     
     Exit;
    end;
    
   {Check Closing} 
   if Service.closing <> 0 then
    begin
     {The service has been closed}
     MutexUnlock(State.slot_mutex);
     Exit;
    end;
    
   {Get Service Quota} 
   ServiceQuota:=@State.service_quotas[Service.localport];
   
   {Lock Spinlock}
   SpinLock(VCHIQQuotaLock);
   
   {Ensure this service doesn't use more than its quota of messages or slots}
   TxEndIndex:=VCHIQ_SLOT_QUEUE_INDEX_FROM_POS(State.local_tx_pos + Stride - 1);
   
   {Ensure data messages don't use more than their quota of slots}
   while (TxEndIndex <> State.previous_data_index) and (State.data_use_count = State.data_quota) do
    begin
     {Update Statistics}
     Inc(State.stats.data_stalls);
     
     {Unlock Spinlock}
     SpinUnlock(VCHIQQuotaLock);
     
     {Unlock Mutex}
     MutexUnlock(State.slot_mutex);
    
     {Wait for Data Quota Event}
     if SemaphoreWait(State.data_quota_event) <> ERROR_SUCCESS then
      begin
       Result:=VCHIQ_RETRY;
       Exit;
      end;
      
     {Lock Mutex}
     MutexLock(State.slot_mutex);
     
     {Lock Spinlock}
     SpinLock(VCHIQQuotaLock);
     
     TxEndIndex:=VCHIQ_SLOT_QUEUE_INDEX_FROM_POS(State.local_tx_pos + Stride - 1);
     if (TxEndIndex = State.previous_data_index) or (State.data_use_count < State.data_quota) then
      begin
       {Pass the signal on to other waiters}
       SemaphoreSignal(State.data_quota_event);
       Break;
      end;
    end;
    
   while (ServiceQuota.message_use_count = ServiceQuota.message_quota) or ((TxEndIndex <> ServiceQuota.previous_tx_index) and (ServiceQuota.slot_use_count = ServiceQuota.slot_quota)) do
    begin
     {Unlock Spinlock}
     SpinUnlock(VCHIQQuotaLock);
    
     {$IFDEF VC4VCHIQ_DEBUG}
     if DEVICE_LOG_ENABLED then
      begin
       DeviceLogDebug(nil,'VCHIQ: Queue Message Internal: Quota Stall');
       DeviceLogDebug(nil,'VCHIQ:  Id=' + IntToStr(State.id));
       DeviceLogDebug(nil,'VCHIQ:  LocalPort=' + IntToStr(Service.localport));
       DeviceLogDebug(nil,'VCHIQ:  MessageType=' + VCHIQMessageTypeToString(MsgType));
       DeviceLogDebug(nil,'VCHIQ:  Size=' + IntToStr(Size));
       DeviceLogDebug(nil,'VCHIQ:  MessageUseCount=' + IntToStr(ServiceQuota.message_use_count));
       DeviceLogDebug(nil,'VCHIQ:  SlotUseCount=' + IntToStr(ServiceQuota.slot_use_count));
      end; 
     {$ENDIF}
     
     {Update Statistics}
     Inc(Service.stats.quota_stalls);
    
     {Unlock Mutex}
     MutexUnlock(State.slot_mutex);
    
     {Wait for Quota Event}
     if SemaphoreWait(ServiceQuota.quota_event) <> ERROR_SUCCESS then
      begin
       Result:=VCHIQ_RETRY;
       Exit;
      end;
      
     {Check Closing} 
     if Service.closing <> 0 then
      begin
       Exit;
      end;
      
     {Lock Mutex}
     if MutexLock(State.slot_mutex) <> ERROR_SUCCESS then
      begin
       Result:=VCHIQ_RETRY;
       Exit;
      end;
      
     {Check Service State}
     if Service.srvstate <> VCHIQ_SRVSTATE_OPEN then
      begin
       {The service has been closed}
       MutexUnlock(State.slot_mutex);
       Exit;
      end;
     
     {Lock Spinlock}
     SpinLock(VCHIQQuotaLock);
     
     TxEndIndex:=VCHIQ_SLOT_QUEUE_INDEX_FROM_POS(State.local_tx_pos + Stride - 1);
    end;
   
   {Unlock Spinlock}
   SpinUnlock(VCHIQQuotaLock);
  end;

 {Reserve Space}
 Header:=VCHIQReserveSpace(State,Stride,(Flags and QMFLAGS_IS_BLOCKING) <> 0);
 if Header = nil then
  begin
   {Update Statistics}
   if Service <> nil then Inc(Service.stats.slot_stalls);
   
   {In the event of a failure, return the mutex to the state it was in}
   if (Flags and QMFLAGS_NO_MUTEX_LOCK) = 0 then
    begin
     MutexUnlock(State.slot_mutex);
    end;
   
   Result:=VCHIQ_RETRY;
   Exit;
  end; 
 
 {Check Message Type}
 if MsgType = VCHIQ_MSG_DATA then
  begin
   {$IFDEF VC4VCHIQ_DEBUG}
   if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Queue Message Internal (Id=' + IntToStr(State.id) + ' MessageType=' + VCHIQMessageTypeToString(MsgType) + ' Header=' + IntToHex(PtrUInt(Header),8) + ' Size=' + IntToStr(Size) + ' SourcePort=' + IntToStr(VCHIQ_MSG_SRCPORT(MsgId)) + ' DestPort=' + IntToStr(VCHIQ_MSG_DSTPORT(MsgId)) + ')');
   {$ENDIF}
  
   {Check Service}
   if Service = nil then 
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Queue Message: Service = nil');
     ThreadHalt(0);
    end;
    
   {Check Flags}
   if (Flags and (QMFLAGS_NO_MUTEX_LOCK or QMFLAGS_NO_MUTEX_UNLOCK)) <> 0 then
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Queue Message: (Flags and (QMFLAGS_NO_MUTEX_LOCK or QMFLAGS_NO_MUTEX_UNLOCK)) <> 0');
     ThreadHalt(0);
    end;
    
   {Copy Elements} 
   Index:=0;
   Position:=0;
   while Index < Count do
    begin
     {Check Size}
     if Elements[Index].size <> 0 then
      begin
       {Copy Element}
       System.Move(Elements[Index].data^,Pointer(@Header.data + Position)^,Elements[Index].size);
      end;
     
     {Update Position}
     Inc(Position,Elements[Index].size);
     
     {Update Index}
     Inc(Index);
    end;

   //To Do //Continuing //Log Trace
    
   {Lock Spinlock}
   SpinLock(VCHIQQuotaLock);
   
   {Update Message Use Count}
   Inc(ServiceQuota.message_use_count);
   
   {Get Tx End Index}
   TxEndIndex:=VCHIQ_SLOT_QUEUE_INDEX_FROM_POS(State.local_tx_pos - 1);
   
   {If this transmission can't fit in the last slot used by any service, the data_use_count must be increased}
   if TxEndIndex <> State.previous_data_index then
    begin
     State.previous_data_index:=TxEndIndex;
     Inc(State.data_use_count);
    end;
   
   {If this isn't the same slot last used by this service, the service's slot_use_count must be increased}
   if TxEndIndex <> ServiceQuota.previous_tx_index then
    begin
     ServiceQuota.previous_tx_index:=TxEndIndex;
     Inc(ServiceQuota.slot_use_count);
     SlotUseCount:=ServiceQuota.slot_use_count;
    end
   else
    begin
     SlotUseCount:=0;
    end;    
   
   {Unlock Spinlock}
   SpinUnlock(VCHIQQuotaLock);
   
   {Check Slot Use Count}
   if SlotUseCount <> 0 then
    begin
     {$IFDEF VC4VCHIQ_DEBUG}
     if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Queue Message Internal (Id=' + IntToStr(State.id) + ' LocalPort=' + IntToStr(Service.localport) + ' MessageType=' + VCHIQMessageTypeToString(MsgType) + ' Size=' + IntToStr(Size) + ' SlotUseCount=' + IntToStr(SlotUseCount) + ')');
     {$ENDIF}
    end;
    
   {Update Statistics}
   Inc(Service.stats.ctrl_tx_count);
   Inc(Service.stats.ctrl_tx_bytes,Size);
  end
 else
  begin
   {$IFDEF VC4VCHIQ_DEBUG}
   if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Queue Message Internal (Id=' + IntToStr(State.id) + ' MessageType=' + VCHIQMessageTypeToString(MsgType) + ' Header=' + IntToHex(PtrUInt(Header),8) + ' Size=' + IntToStr(Size) + ' SourcePort=' + IntToStr(VCHIQ_MSG_SRCPORT(MsgId)) + ' DestPort=' + IntToStr(VCHIQ_MSG_DSTPORT(MsgId)) + ')');
   {$ENDIF}

   {Check Size}
   if Size <> 0 then
    begin
     if (Count <> 1) or (Size <> Elements[0].size) then
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Queue Message: (Count <> 1) or (Size <> Elements[0].size)');
      end;
      
     {Copy Data} 
     System.Move(Elements[0].data^,Header.data[0],Elements[0].size);
    end;
    
   {Update Statistics}
   Inc(State.stats.ctrl_tx_count);
  end;  
 
 {Update Header}
 Header.msgid:=MsgId;
 Header.size:=Size;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then
  begin
   DeviceLogDebug(nil,'VCHIQ: Queue Message Internal: Sent Message');
   DeviceLogDebug(nil,'VCHIQ:  MessageType=' + VCHIQMessageTypeToString(MsgType));
   DeviceLogDebug(nil,'VCHIQ:  MessageNo=' + IntToStr(MsgType));
   if Service <> nil then DeviceLogDebug(nil,'VCHIQ:  Fourcc=' + VCHIQFourccToString(Service.base.fourcc));
   DeviceLogDebug(nil,'VCHIQ:  SourcePort=' + IntToStr(VCHIQ_MSG_SRCPORT(MsgId)));
   DeviceLogDebug(nil,'VCHIQ:  DestPort=' + IntToStr(VCHIQ_MSG_DSTPORT(MsgId)));
   DeviceLogDebug(nil,'VCHIQ:  Size=' + IntToStr(Size));
  end; 
 {$ENDIF}
 
 {Make sure the new header is visible to the peer}
 DataMemoryBarrier;
 
 {Make the new tx_pos visible to the peer}
 Local.tx_pos:=State.local_tx_pos;
 
 {Memory Barrier}
 DataMemoryBarrier;
 
 {Check Close}
 if (Service <> nil) and (MsgType = VCHIQ_MSG_CLOSE) then
  begin
   VCHIQSetServiceState(Service,VCHIQ_SRVSTATE_CLOSESENT);
  end;
 
 {Unlock Mutex}
 if (Flags and QMFLAGS_NO_MUTEX_UNLOCK) = 0 then
  begin
   MutexUnlock(State.slot_mutex);
  end; 
 
 {Signal Remote Event}
 VCHIQRemoteEventSignal(@State.remote.trigger);
 
 {Return Success}
 Result:=VCHIQ_SUCCESS;
end;

{==============================================================================}

function VCHIQQueueMessageInternalSync(State:PVCHIQ_STATE_T;Service:PVCHIQ_SERVICE_T;MsgId:Integer;Elements:PVCHIQ_ELEMENT_ARRAY_T;Count,Size:Integer;IsBlocking:Boolean):VCHIQ_STATUS_T;
{From queue_message_sync in vchiq_core.c}

{Called by the slot handler and application threads}
var
 Index:Integer;
 Position:Integer;
 OldMsgId:Integer;
 {ServiceFourcc:Integer;} {Not Used}
 Header:PVCHIQ_HEADER_T;
 Local:PVCHIQ_SHARED_STATE_T;
begin
 {}
 Result:=VCHIQ_ERROR;

 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Queue Message Internal Sync (State=' + IntToHex(PtrUInt(State),8) + ' Service=' + IntToHex(PtrUInt(Service),8) + ' MsgId=' + IntToStr(MsgId) + ')');
 {$ENDIF}
 
 {Check State}
 if State = nil then Exit;

 {Check Service}
 if Service = nil then Exit;
 
 {Get Local}
 Local:=State.local;
 
 {Check Message Type and Lock Mutex}
 if (VCHIQ_MSG_TYPE(MsgId) <> VCHIQ_MSG_RESUME) and (MutexLock(State.sync_mutex) <> ERROR_SUCCESS) then
  begin
   Result:=VCHIQ_RETRY;
   Exit;
  end;
 
 {Wait Remote Event}
 VCHIQRemoteEventWait(@Local.sync_release);
 
 {Memory Barrier}
 DataMemoryBarrier;        
 
 {Get Header}
 Header:=PVCHIQ_HEADER_T(VCHIQ_SLOT_DATA_FROM_INDEX(State,Local.slot_sync));

 {Check Header}
 OldMsgId:=Header.msgid;
 if OldMsgId <> VCHIQ_MSGID_PADDING then
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Queue Message Sync: Message Id not PADDING (Id=' + IntToStr(State.id) + ' MsgId=' + IntToHex(PtrUInt(OldMsgId),8) + ')');
  end;

 {Check Service}
 if Service <> nil then
  begin
   {$IFDEF VC4VCHIQ_DEBUG}
   if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Queue Message Internal Sync (Id=' + IntToStr(State.id) + ' MessageType=' + VCHIQMessageTypeToString(VCHIQ_MSG_TYPE(MsgId)) + ' Header=' + IntToHex(PtrUInt(Header),8) + ' Size=' + IntToStr(Size) + ' SourcePort=' + IntToStr(VCHIQ_MSG_SRCPORT(MsgId)) + ' DestPort=' + IntToStr(VCHIQ_MSG_DSTPORT(MsgId)) + ')');
   {$ENDIF}
  
   {Copy Elements}
   Index:=0;
   Position:=0;
   while Index < Count do
    begin
     {Check Size}
     if Elements[Index].size <> 0 then
      begin
       {Copy Element}
       System.Move(Elements[Index].data^,Pointer(@Header.data + Position)^,Elements[Index].size);
      end;
     
     {Update Position}
     Inc(Position,Elements[Index].size);
     
     {Update Index}
     Inc(Index);
    end;
    
   //To Do //Continuing //Log Trace 
   
   {Update Statistics}
   Inc(Service.stats.ctrl_tx_count);
   Inc(Service.stats.ctrl_tx_bytes,Size);
  end
 else
  begin
   {$IFDEF VC4VCHIQ_DEBUG}
   if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Queue Message Internal Sync (Id=' + IntToStr(State.id) + ' MessageType=' + VCHIQMessageTypeToString(VCHIQ_MSG_TYPE(MsgId)) + ' Header=' + IntToHex(PtrUInt(Header),8) + ' Size=' + IntToStr(Size) + ' SourcePort=' + IntToStr(VCHIQ_MSG_SRCPORT(MsgId)) + ' DestPort=' + IntToStr(VCHIQ_MSG_DSTPORT(MsgId)) + ')');
   {$ENDIF}
   
   {Check Size}
   if Size <> 0 then
    begin
     if (Count <> 1) or (Size <> Elements[0].size) then
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Queue Message Sync: (Count <> 1) or (Size <> Elements[0].size)');
      end;
      
     {Copy Data} 
     System.Move(Elements[0].data^,Header.data[0],Elements[0].size);
    end;
    
   {Update Statistics}
   Inc(State.stats.ctrl_tx_count);
  end;  
 
 {Update Header}
 Header.size:=Size;
 Header.msgid:=MsgId;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then
  begin
   DeviceLogDebug(nil,'VCHIQ: Queue Message Internal Sync: Sent Sync Message');
   DeviceLogDebug(nil,'VCHIQ:  MessageType=' + VCHIQMessageTypeToString(VCHIQ_MSG_TYPE(MsgId)));
   DeviceLogDebug(nil,'VCHIQ:  MessageNo=' + IntToStr(VCHIQ_MSG_TYPE(MsgId)));
   if Service <> nil then DeviceLogDebug(nil,'VCHIQ:  Fourcc=' + VCHIQFourccToString(Service.base.fourcc));
   DeviceLogDebug(nil,'VCHIQ:  SourcePort=' + IntToStr(VCHIQ_MSG_SRCPORT(MsgId)));
   DeviceLogDebug(nil,'VCHIQ:  DestPort=' + IntToStr(VCHIQ_MSG_DSTPORT(MsgId)));
   DeviceLogDebug(nil,'VCHIQ:  Size=' + IntToStr(Size));
  end; 
 {$ENDIF}
 
 {Make sure the new header is visible to the peer}
 DataMemoryBarrier;
 
 {Signal Remote Event}
 VCHIQRemoteEventSignal(@State.remote.sync_trigger);

 {Check Message Type}
 if VCHIQ_MSG_TYPE(MsgId) <> VCHIQ_MSG_PAUSE then
  begin
   {Unlock Mutex}
   MutexUnlock(State.sync_mutex);
  end;
 
 {Return Success}
 Result:=VCHIQ_SUCCESS;
end;

{==============================================================================}

procedure VCHIQReleaseMessageSync(State:PVCHIQ_STATE_T;Header:PVCHIQ_HEADER_T);
{From release_message_sync in vchiq_core.c}
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Release Message Syn (State=' + IntToHex(PtrUInt(State),8) + ' Header=' + IntToHex(PtrUInt(Header),8) + ')');
 {$ENDIF}

 {Check State}
 if State = nil then Exit;

 {Check Header}
 if Header = nil then Exit;
 
 {Update Message Id}
 Header.msgid:=VCHIQ_MSGID_PADDING;
 
 {Memory Barrier}
 DataMemoryBarrier;
 
 {Signal Remote Event}
 VCHIQRemoteEventSignal(@State.remote.sync_release);
end;

{==============================================================================}

procedure VCHIQClaimSlot(Slot:PVCHIQ_SLOT_INFO_T); inline;
{From claim_slot in vchiq_core.c}
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Claim Slot (Slot=' + IntToHex(PtrUInt(Slot),8) + ')');
 {$ENDIF}
 
 {Check Slot}
 if Slot = nil then Exit;

 {Update Use Count}
 Inc(Slot.use_count);
end;

{==============================================================================}

procedure VCHIQReleaseSlot(State:PVCHIQ_STATE_T;Slot:PVCHIQ_SLOT_INFO_T;Header:PVCHIQ_HEADER_T;Service:PVCHIQ_SERVICE_T);
{From release_slot in vchiq_core.c}
var
 MsgId:Integer;
 ReleaseCount:Integer;
 SlotQueueRecycle:Integer;
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Release Slot (State=' + IntToHex(PtrUInt(State),8) + ' Slot=' + IntToHex(PtrUInt(Slot),8) + ' Header=' + IntToHex(PtrUInt(Header),8) + ' Service=' + IntToHex(PtrUInt(Service),8) + ')');
 {$ENDIF}
 
 {Check State}
 if State = nil then Exit;
 
 {Check Slot}
 if Slot = nil then Exit;

 {Lock Mutex}
 MutexLock(State.recycle_mutex);
 
 {Check Header}
 if Header <> nil then
  begin
   MsgId:=Header.msgid;
   if ((MsgId and VCHIQ_MSGID_CLAIMED) = 0) or ((Service <> nil) and (Service.closing <> 0)) then
    begin
     {Unlock Mutex}
     MutexUnlock(State.recycle_mutex);
     
     Exit;
    end;
   
   {Rewrite the message header to prevent a double release}
   Header.msgid:=MsgId and not(VCHIQ_MSGID_CLAIMED);
  end; 
 
 {Update Release Count}
 ReleaseCount:=Slot.release_count;
 Inc(ReleaseCount);
 Slot.release_count:=ReleaseCount;
 
 if ReleaseCount = Slot.use_count then
  begin
   {Add to the freed queue}
   {A read barrier is necessary here to prevent speculative fetches of Remote.slot_queue_recycle from overtaking the mutex}
   DataMemoryBarrier;
   
   {Update Slot Queue Recycle}
   SlotQueueRecycle:=State.remote.slot_queue_recycle;
   State.remote.slot_queue[SlotQueueRecycle and VCHIQ_SLOT_QUEUE_MASK]:=VCHIQ_SLOT_INDEX_FROM_INFO(State,Slot);
   State.remote.slot_queue_recycle:=SlotQueueRecycle + 1;
   
   {$IFDEF VC4VCHIQ_DEBUG}
   if DEVICE_LOG_ENABLED then
    begin
     DeviceLogDebug(nil,'VCHIQ: Release Slot');
     DeviceLogDebug(nil,'VCHIQ:  Id=' + IntToStr(State.id));
     DeviceLogDebug(nil,'VCHIQ:  SlotIndex=' + IntToStr(VCHIQ_SLOT_INDEX_FROM_INFO(State,Slot)));
     DeviceLogDebug(nil,'VCHIQ:  SlotQueueRecycle=' + IntToStr(State.remote.slot_queue_recycle));
    end; 
   {$ENDIF}
   
   {A write barrier is necessary, but VCHIQRemoteEventSignal contains one}
   VCHIQRemoteEventSignal(@State.remote.recycle);
  end;
 
 {Unlock Mutex}
 MutexUnlock(State.recycle_mutex);
end;

{==============================================================================}

procedure VCHIQRemoteEventCreate(Event:PREMOTE_EVENT_T);
{From remote_event_create in vchiq_core.c}
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Remote Event Create (Event=' + IntToHex(PtrUInt(Event),8) + ')');
 {$ENDIF}
 
 {Check Event}
 if Event = nil then Exit;
 
 {Set Armed}
 Event.armed:=0;
 
 {Don't clear the 'fired' flag because it may already have been set by the other side}
 Event.event:=SemaphoreCreateEx(0,SEMAPHORE_DEFAULT_MAXIMUM,SEMAPHORE_FLAG_IRQ); {Used within interrupt handler}
end;

{==============================================================================}

procedure VCHIQRemoteEventDestroy(Event:PREMOTE_EVENT_T);
{From remote_event_destroy in vchiq_core.c}
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Remote Event Destroy (Event=' + IntToHex(PtrUInt(Event),8) + ')');
 {$ENDIF}
 
 {Check Event}
 if Event = nil then Exit;
 
 {Destroy Semaphore}
 SemaphoreDestroy(Event.event);
 
 {Invalidate Semaphore}
 Event.event:=INVALID_HANDLE_VALUE;
end;

{==============================================================================}

function VCHIQRemoteEventWait(Event:PREMOTE_EVENT_T):Boolean;
{From remote_event_wait in vchiq_core.c}
begin
 {}
 Result:=False;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Remote Event Wait (Event=' + IntToHex(PtrUInt(Event),8) + ')');
 {$ENDIF}
 
 {Check Event}
 if Event = nil then Exit;
 
 {Check Fired}
 if Event.fired = 0 then
  begin
   {Set Armed}
   Event.armed:=1;
   
   {Synchronization Barrier}
   DataSynchronizationBarrier;
   
   {Check Fired}
   if Event.fired = 0 then
    begin
     {Wait Event}
     if SemaphoreWait(Event.event) <> ERROR_SUCCESS then
      begin
       {Reset Armed}
       Event.armed:=0; 
       Exit;
      end;      
    end;
    
   {Reset Armed}
   Event.armed:=0; 
   
   {Memory Barrier}
   DataMemoryBarrier;
  end;
 
 {Reset Fired} 
 Event.fired:=0;
 
 Result:=True; 
end;

{==============================================================================}

procedure VCHIQRemoteEventSignalLocal(Event:PREMOTE_EVENT_T);
{From remote_event_signal_local in vchiq_core.c}

{Called by the doorbell interrupt handler}
begin
 {}
 {$IF DEFINED(VC4VCHIQ_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Remote Event Signal Local (Event=' + IntToHex(PtrUInt(Event),8) + ')');
 {$ENDIF}
 
 {Check Event}
 if Event = nil then Exit;
 
 {Reset Armed}
 Event.armed:=0;
 
 {Signal Event}
 SemaphoreSignal(Event.event);
end;

{==============================================================================}

procedure VCHIQRemoteEventPoll(Event:PREMOTE_EVENT_T);
{From remote_event_poll in vchiq_core.c}

{Called by the doorbell interrupt handler}
begin
 {}
 {$IF DEFINED(VC4VCHIQ_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Remote Event Poll (Event=' + IntToHex(PtrUInt(Event),8) + ')');
 {$ENDIF}
 
 {Check Event}
 if Event = nil then Exit;
 
 {Check Fired and Armed}
 if (Event.fired <> 0) and (Event.armed <> 0) then 
  begin
   {Signal Event Local}
   VCHIQRemoteEventSignalLocal(Event);
  end;
end;

{==============================================================================}

procedure VCHIQRemoteEventPollAll(State:PVCHIQ_STATE_T);
{From remote_event_pollall in vchiq_core.c}

{Called by the doorbell interrupt handler}
begin
 {}
 {$IF DEFINED(VC4VCHIQ_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Remote Event Poll All (State=' + IntToHex(PtrUInt(State),8) + ')');
 {$ENDIF}
 
 {Check State}
 if State = nil then Exit;

 {Poll Events}
 VCHIQRemoteEventPoll(@State.local.sync_trigger);
 VCHIQRemoteEventPoll(@State.local.sync_release);
 VCHIQRemoteEventPoll(@State.local.trigger);
 VCHIQRemoteEventPoll(@State.local.recycle);
end;

{==============================================================================}

function VCHIQSyncExecute(VCHIQ:PVCHIQDevice):PtrInt;
{From sync_func in vchiq_core.c}

{Called by the sync thread}
var
 Size:Integer;
 MsgId:Integer;
 MsgType:Integer;
 LocalPort:LongWord;
 RemotePort:LongWord;
 Header:PVCHIQ_HEADER_T;
 Service:PVCHIQ_SERVICE_T;
 {ServiceFourcc:Integer;} {Not Used}
 Payload:PVCHIQ_OPENACK_PAYLOAD;
 
 State:PVCHIQ_STATE_T;
 Local:PVCHIQ_SHARED_STATE_T;
begin
 {}
 Result:=0;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Sync Execute (ThreadID=' + IntToHex(ThreadGetCurrent,8) + ')');
 {$ENDIF}
 
 {Check VCHIQ}
 if VCHIQ = nil then Exit;
 
 {Get State}
 State:=VCHIQ.State;
 
 {Get Local}
 Local:=State.local;
 
 {Get Header}
 Header:=PVCHIQ_HEADER_T(VCHIQ_SLOT_DATA_FROM_INDEX(State,State.remote.slot_sync));
 
 {Update Priority}
 Sleep(1);
 
 while True do
  begin
   {Wait for Sync Trigger Event}
   VCHIQRemoteEventWait(@Local.sync_trigger);
   
   {Memory Barrier}
   DataMemoryBarrier;
   
   {Get Message}
   MsgId:=Header.msgid;
   Size:=Header.size;
   MsgType:=VCHIQ_MSG_TYPE(MsgId);
   LocalPort:=VCHIQ_MSG_DSTPORT(MsgId);
   RemotePort:=VCHIQ_MSG_SRCPORT(MsgId);

   {Get Service}
   Service:=VCHIQFindServiceByPort(State,LocalPort);
   if Service = nil then
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Invalid/closed service (Id=' + IntToStr(State.id) + ' MessageType=' + VCHIQMessageTypeToString(MsgType) + ' LocalPort=' + IntToStr(LocalPort) + ' RemotePort=' + IntToStr(RemotePort) + ')');
     
     {Release Message Sync}
     VCHIQReleaseMessageSync(State,Header);
    end
   else
    begin
     //To Do //Continuing //Log Trace

     {Check Message Type}
     case MsgType of
      VCHIQ_MSG_OPENACK:begin
        {Check Size}
        if Size >= SizeOf(VCHIQ_OPENACK_PAYLOAD) then
         begin
          {Get Payload}
          Payload:=PVCHIQ_OPENACK_PAYLOAD(@Header.data);
          
          {Get Peer Version}
          Service.peer_version:=Payload.version;
         end;
        
        {$IFDEF VC4VCHIQ_DEBUG}
        if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: VCHIQ_MSG_OPENACK (Id=' + IntToStr(State.id) + ' Size=' + IntToStr(Size) + ' LocalPort=' + IntToStr(LocalPort) + ' RemotePort=' + IntToStr(RemotePort) + ' PeerVersion=' + IntToStr(Service.peer_version) + ')');
        {$ENDIF}
        
        {Check Service State}
        if Service.srvstate = VCHIQ_SRVSTATE_OPENING then
         begin
          {Update Service}
          Service.remoteport:=RemotePort;
          VCHIQSetServiceState(Service,VCHIQ_SRVSTATE_OPENSYNC);
          Service.sync:=1;
          
          {Signal Remove Event}
          SemaphoreSignal(Service.remove_event);
         end;
        
        {Release Message Sync}
        VCHIQReleaseMessageSync(State,Header);
       end;
      VCHIQ_MSG_DATA:begin
        {$IFDEF VC4VCHIQ_DEBUG}
        if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: VCHIQ_MSG_DATA (Id=' + IntToStr(State.id) + ' Size=' + IntToStr(Size) + ' LocalPort=' + IntToStr(LocalPort) + ' RemotePort=' + IntToStr(RemotePort) + ')');
        {$ENDIF}
        
        {Check Remote Port and Service State}
        if (Service.remoteport = RemotePort) and (Service.srvstate = VCHIQ_SRVSTATE_OPENSYNC) then
         begin
          {Make Service Callback}
          if VCHIQMakeServiceCallback(Service,VCHIQ_MESSAGE_AVAILABLE,Header,nil) = VCHIQ_RETRY then
           begin
            if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Synchronous callback to service returns VCHIQ_RETRY (LocalPort=' + IntToStr(LocalPort) + ')');
           end;
         end;
       end;      
      else
       begin
        if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Unexpected message id (Id=' + IntToStr(State.id) + ' MessageId=' + IntToStr(MsgId) + ' Size=' + IntToStr(Size) + ')');
        
        {Release Message Sync}
        VCHIQReleaseMessageSync(State,Header);
       end;
     end;
     
     {Unlock Service}
     VCHIQUnlockService(Service);
    end;    
  end; 
end;

{==============================================================================}

procedure VCHIQProcessFreeQueue(State:PVCHIQ_STATE_T);
{From process_free_queue in vchiq_core.c}

{Called by the recycle thread}
var
 Local:PVCHIQ_SHARED_STATE_T;
 SlotQueueAvailable:Integer;
 
 Data:PByte;
 MsgId:Integer;
 Port:Integer;
 Count:Integer;
 Position:LongWord;
 SlotIndex:Integer;
 DataFound:Boolean;
 Header:PVCHIQ_HEADER_T;
 ServiceQuota:PVCHIQ_SERVICE_QUOTA_T;
 ServiceFound:array[0..VCHIQ_MAX_SERVICES_BITSET_SIZE - 1] of BITSET_T;
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Process Free Queue (State=' + IntToHex(PtrUInt(State),8) + ')');
 {$ENDIF}

 {Check State}
 if State = nil then Exit;
 
 {Get Local}
 Local:=State.local;
 if Local = nil then Exit;
 
 {Find slots which have been freed by the other side, and return them to the available queue}
 SlotQueueAvailable:=State.slot_queue_available;
 
 {Use a memory barrier to ensure that any state that may have been modified by another thread is not masked by stale prefetched values}
 DataMemoryBarrier;
 
 while SlotQueueAvailable <> Local.slot_queue_recycle do
  begin
   {Get Slot Index}
   SlotIndex:=Local.slot_queue[SlotQueueAvailable and VCHIQ_SLOT_QUEUE_MASK];
   Inc(SlotQueueAvailable);
   
   {Get Data}
   Data:=VCHIQ_SLOT_DATA_FROM_INDEX(State,SlotIndex);
   DataFound:=False;
   
   {Memory Barrier}
   DataMemoryBarrier;
  
   {$IFDEF VC4VCHIQ_DEBUG}
   if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Process Free Queue (Id=' + IntToStr(State.id) + ' SlotIndex=' + IntToStr(SlotIndex) + ' Data=' + IntToHex(PtrUInt(Data),8) + ' SlotQueueRecycle=' + IntToStr(Local.slot_queue_recycle) + ' SlotQueueAvailable=' + IntToStr(SlotQueueAvailable) + ')');
   {$ENDIF}
  
   {Initialise the bitmask for services which have used this slot}
   VCHIQ_BITSET_ZERO(@ServiceFound,VCHIQ_MAX_SERVICES_BITSET_SIZE);

   Position:=0;
   while Position < VCHIQ_SLOT_SIZE do
    begin
     {Get Header}
     Header:=PVCHIQ_HEADER_T(Data + Position);
     
     {Get Message Id}
     MsgId:=Header.msgid;
     
     {Check Message Type}
     if VCHIQ_MSG_TYPE(MsgId) = VCHIQ_MSG_DATA then
      begin
       {Get Port}
       Port:=VCHIQ_MSG_SRCPORT(MsgId);
       
       {Get Service Quota}
       ServiceQuota:=@State.service_quotas[Port];
       
       {Lock Spinlock}
       SpinLock(VCHIQQuotaLock);
       
       {Get Message Use Count}
       Count:=ServiceQuota.message_use_count;
       if Count > 0 then
        begin
         ServiceQuota.message_use_count:=Count - 1;
        end;
       
       {Unlock Spinlock}
       SpinUnlock(VCHIQQuotaLock); 
      
       {Check Message Quota}
       if Count = ServiceQuota.message_quota then
        begin
         {Signal the service that it has dropped below its quota}      
         SemaphoreSignal(ServiceQuota.quota_event);
        end
       else if Count = 0 then
        begin
         if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Process Free Queue: Invalid message use count (Port=' + IntToStr(Port) + ' MessageUseCount=' + IntToStr(ServiceQuota.message_quota) + ' MsgId=' + IntToHex(MsgId,8) + ' Size=' + IntToStr(Header.size) + ')');
        end;
        
       if not VCHIQ_BITSET_IS_SET(@ServiceFound,Port) then
        begin
         {Set the found bit for this service}
         VCHIQ_BITSET_SET(@ServiceFound,Port);
         
         {Lock Spinlock}
         SpinLock(VCHIQQuotaLock);
         
         {Get Slot Use Count}
         Count:=ServiceQuota.slot_use_count;
         if Count > 0 then
          begin
           ServiceQuota.slot_use_count:=Count - 1;
          end;
        
         {Unlock Spinlock}
         SpinUnlock(VCHIQQuotaLock); 
        
         {Check Count}
         if Count > 0 then
          begin
           {Signal the service in case it has dropped below its quota}
           SemaphoreSignal(ServiceQuota.quota_event);

           {$IFDEF VC4VCHIQ_DEBUG}
           if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Process Free Queue (Id=' + IntToStr(State.id) + ' Port=' + IntToStr(Port) + ' Size=' + IntToStr(Header.size) + ' Count=' + IntToStr(Count - 1) + ')');
           {$ENDIF}
          end
         else
          begin
           if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Process Free Queue: Invalid message use count (Port=' + IntToStr(Port) + ' SlotUseCount=' + IntToStr(ServiceQuota.slot_use_count) + ' MsgId=' + IntToHex(MsgId,8) + ' Size=' + IntToStr(Header.size) + ')');
          end;          
        end;
      
       DataFound:=True;
      end;
     
     {Update Position}
     Position:=Position + VCHIQCalcStride(Header.size);
     if Position > VCHIQ_SLOT_SIZE then
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Process Free Queue: Invalid slot position (Position=' + IntToStr(Position) + ' MsgId=' + IntToHex(MsgId,8) + ' Size=' + IntToStr(Header.size) + ')');
      end;
    end;
  
   {Check Data Found}
   if DataFound then
    begin
     {Lock Spinlock}
     SpinLock(VCHIQQuotaLock);
     
     {Get Data Use Count}
     Count:=State.data_use_count;
     if Count > 0 then
      begin
       State.data_use_count:=Count - 1;
      end;
      
     {Unlock Spinlock}
     SpinUnlock(VCHIQQuotaLock); 
     
     {Check Data Quota}
     if Count = State.data_quota then
      begin
       {Signal Data Quota}
       SemaphoreSignal(State.data_quota_event);
      end;
    end;
  
   {Memory Barrier}
   DataMemoryBarrier;
   
   {Update Slot Queue Available}
   State.slot_queue_available:=SlotQueueAvailable;
   
   {Signal Slot Queue Event}
   SemaphoreSignal(State.slot_available_event)
  end;
end;

{==============================================================================}

function VCHIQRecycleExecute(VCHIQ:PVCHIQDevice):PtrInt;
{From recycle_func in vchiq_core.c}

{Called by the recycle thread}
var
 State:PVCHIQ_STATE_T;
 Local:PVCHIQ_SHARED_STATE_T;
begin
 {}
 Result:=0;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Recycle Execute (ThreadID=' + IntToHex(ThreadGetCurrent,8) + ')');
 {$ENDIF}
 
 {Check VCHIQ}
 if VCHIQ = nil then Exit;
 
 {Get State}
 State:=VCHIQ.State;
 
 {Get Local}
 Local:=State.local;
 
 {Update Priority}
 Sleep(1);
 
 while True do
  begin
   {Wait for Recycle Event}
   VCHIQRemoteEventWait(@Local.recycle);
   
   {Process Free Queue}
   VCHIQProcessFreeQueue(State);
  end; 
end;

{==============================================================================}

function VCHIQGetListeningService(State:PVCHIQ_STATE_T;Fourcc:Integer):PVCHIQ_SERVICE_T;
{From get_listening_service in vchiq_core.c}

{Called by the slot handler thread}
var
 Count:Integer;
 Service:PVCHIQ_SERVICE_T;
begin
 {}
 Result:=nil;

 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Get Listening Service (State=' + IntToHex(PtrUInt(State),8) + ' Fourcc=' + IntToHex(Fourcc,8) + ')');
 {$ENDIF}
 
 {Check State} 
 if State = nil then Exit;
 
 {Check Fourcc}
 if Fourcc = VCHIQ_FOURCC_INVALID then
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Fourcc = VCHIQ_FOURCC_INVALID'); 
  end;
 
 {Get Service}
 for Count:=0 to State.unused_service - 1 do
  begin
   Service:=State.services[Count];
   if (Service <> nil) and (Service.public_fourcc = Fourcc) and ((Service.srvstate = VCHIQ_SRVSTATE_LISTENING) or (Service.srvstate = VCHIQ_SRVSTATE_OPEN)) and (Service.remoteport = VCHIQ_PORT_FREE) then
    begin
     VCHIQLockService(Service);

     Result:=Service;
     Exit;
    end;
  end;
end;

{==============================================================================}

function VCHIQGetConnectedService(State:PVCHIQ_STATE_T;Port:longWord):PVCHIQ_SERVICE_T;
{From get_connected_service in vchiq_core.c}

{Called by the slot handler thread}
var
 Count:Integer;
 Service:PVCHIQ_SERVICE_T;
begin
 {}
 Result:=nil;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Get Connected Service (State=' + IntToHex(PtrUInt(State),8) + ' Port=' + IntToStr(Port) + ')');
 {$ENDIF}
 
 {Check State} 
 if State = nil then Exit;
 
 {Get Service}
 for Count:=0 to State.unused_service - 1 do
  begin
   Service:=State.services[Count];
   if (Service <> nil) and (Service.srvstate = VCHIQ_SRVSTATE_OPEN) and (Service.remoteport = Port) then
    begin
     VCHIQLockService(Service);

     Result:=Service;
     Exit;
    end;
  end;
end;

{==============================================================================}

procedure VCHIQPollServices(State:PVCHIQ_STATE_T);
{From poll_services in vchiq_core.c}

{Called by the slot handler thread}
var
 Group:Integer;
 Count:Integer;
 Flags:LongWord;
 Service:PVCHIQ_SERVICE_T;
 ServiceFlags:LongWord;
begin
 {}
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Poll Services (State=' + IntToHex(PtrUInt(State),8) + ')');
 {$ENDIF}
 
 {Check State} 
 if State = nil then Exit;

 for Group:=0 to VCHIQ_BITSET_SIZE(State.unused_service) - 1 do
  begin
   {Get Flags}
   Flags:=InterlockedExchange(State.poll_services[Group],0);
   
   Count:=0;
   while Flags <> 0 do
    begin
     {Check Flags}
     if (Flags and (1 shl Count)) <> 0 then
      begin
       {Get Service}
       Service:=VCHIQFindServiceByPort(State,(Group shl 5) + Count);
       
       {Update Flags}
       Flags:=Flags and not(1 shl Count);
       
       {Check Service}
       if Service <> nil then
        begin
         {Get Service Flags}
         ServiceFlags:=InterlockedExchange(Service.poll_flags,0);
         
         {Check Service Flags}
         if (ServiceFlags and (1 shl Ord(VCHIQ_POLL_REMOVE))) <> 0 then
          begin
           {$IFDEF VC4VCHIQ_DEBUG}
           if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Poll Services: Remove (Id=' + IntToStr(State.id) + ' LocalPort=' + IntToStr(Service.localport) + ' RemotePort=' + IntToStr(Service.remoteport) + ')');
           {$ENDIF}
           
           {Make it look like a client, because it must be removed and not left in the LISTENING state}
           Service.public_fourcc:=VCHIQ_FOURCC_INVALID;
           
           {Close Service Internal}
           if VCHIQCloseServiceInternal(Service,False) <> VCHIQ_SUCCESS then {Not CloseReceived}
            begin
             {Request Poll}
             VCHIQRequestPoll(State,Service,VCHIQ_POLL_REMOVE);
            end;
          end
         else if (ServiceFlags and (1 shl Ord(VCHIQ_POLL_TERMINATE))) <> 0 then
          begin
           {$IFDEF VC4VCHIQ_DEBUG}
           if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Poll Services: Terminate (Id=' + IntToStr(State.id) + ' LocalPort=' + IntToStr(Service.localport) + ' RemotePort=' + IntToStr(Service.remoteport) + ')');
           {$ENDIF}
           
           {Close Service Internal}
           if VCHIQCloseServiceInternal(Service,False) <> VCHIQ_SUCCESS then {Not CloseReceived}
            begin
             {Request Poll}
             VCHIQRequestPoll(State,Service,VCHIQ_POLL_TERMINATE);
            end;
          end;          
         
         {Check Service Flags}
         if (ServiceFlags and (1 shl Ord(VCHIQ_POLL_TXNOTIFY))) <> 0 then
          begin
           {Notify Bulks}
           VCHIQNotifyBulks(Service,@Service.bulk_tx,True); {RetryPoll}
          end;
         
         {Check Service Flags}
         if (ServiceFlags and (1 shl Ord(VCHIQ_POLL_RXNOTIFY))) <> 0 then
          begin
           {Notify Bulks}
           VCHIQNotifyBulks(Service,@Service.bulk_rx,True); {RetryPoll}
          end; 
         
         {Unlock Service}
         VCHIQUnlockService(Service);
        end;
      end;
    
     {Update Count}
     Inc(Count);
    end;
  end;
end;

{==============================================================================}

function VCHIQNotifyBulks(Service:PVCHIQ_SERVICE_T;Queue:PVCHIQ_BULK_QUEUE_T;RetryPoll:Boolean):VCHIQ_STATUS_T;
{From notify_bulks in vchiq_core.c}

{Called by the slot handler - don't hold the bulk mutex}
var
 MsgId:Integer;
 MsgType:Integer;
 Bulk:PVCHIQ_BULK_T;
 Status:VCHIQ_STATUS_T;
 Reason:VCHIQ_REASON_T;
 
 Element:VCHIQ_ELEMENT_T;
 BulkWaiter:PVCHIQ_BULK_WAITER_T;
begin
 {}
 Result:=VCHIQ_ERROR;

 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Notify Bulks (Service=' + IntToHex(PtrUInt(Service),8) + ' Queue=' + IntToHex(PtrUInt(Queue),8) + ' RetryPoll=' + BoolToStr(RetryPoll) + ')');
 {$ENDIF}
 
 {Check Service}
 if Service = nil then Exit;

 {Check Queue}
 if Queue = nil then Exit;
 
 {Setup Defaults}
 Status:=VCHIQ_SUCCESS;
 
 {Check Master}
 if Service.state.is_master then
  begin
   while Queue.remote_notify <> Queue.process do
    begin
     {Get Bulk}
     Bulk:=@Queue.bulks[VCHIQ_BULK_INDEX(Queue.remote_notify)];
     
     {Get Message Type}
     MsgType:=VCHIQ_MSG_BULK_TX_DONE;
     if Bulk.dir = VCHIQ_BULK_TRANSMIT then MsgType:=VCHIQ_MSG_BULK_RX_DONE;
     
     {Get Message Id}
     MsgId:=VCHIQ_MAKE_MSG(MsgType,Service.localport,Service.remoteport);
     
     {Create Element}
     Element.data:=@Bulk.actual;
     Element.size:=4;
     
     {Only reply to non-dummy bulk requests}
     if Bulk.remote_data <> nil then
      begin
       {Queue Message}
       Status:=VCHIQQueueMessageInternal(Service.state,nil,MsgId,@Element,1,4,0);
       if Status <> VCHIQ_SUCCESS then Break;
      end;
      
     {Update Remote Notify} 
     Inc(Queue.remote_notify);
    end;  
  end
 else
  begin
   {Update Remote Notify}
   Queue.remote_notify:=Queue.process;
  end;  
 
 if Status = VCHIQ_SUCCESS then
  begin
   while Queue.remove <> Queue.remote_notify do
    begin
     {Get Bulk}
     Bulk:=@Queue.bulks[VCHIQ_BULK_INDEX(Queue.remove)];
    
     {Only generate callbacks for non-dummy bulk requests, and non-terminated services}
     if (Bulk.data <> nil) and (Service.instance <> nil) then
      begin
       {Check Actual}
       if Bulk.actual <> VCHIQ_BULK_ACTUAL_ABORTED then
        begin
         {Check Direction}
         if Bulk.dir = VCHIQ_BULK_TRANSMIT then
          begin
           {Update Statistics}
           Inc(Service.stats.bulk_tx_count);
           Inc(Service.stats.bulk_tx_bytes,Bulk.actual);
          end
         else
          begin
           {Update Statistics}
           Inc(Service.stats.bulk_rx_count);
           Inc(Service.stats.bulk_rx_bytes,Bulk.actual);
          end;          
        end
       else
        begin
         {Update Statistics}
         Inc(Service.stats.bulk_aborted_count);
        end;
        
       {Check Mode}
       if Bulk.mode = VCHIQ_BULK_MODE_BLOCKING then
        begin
         {Lock Spinlock}
         SpinLock(VCHIQBulkWaiterLock);
         
         {Get Bulk Waiter}
         BulkWaiter:=PVCHIQ_BULK_WAITER_T(Bulk.userdata);
         if BulkWaiter <> nil then
          begin
           {Update Actual}
           BulkWaiter.actual:=Bulk.actual;
           
           {Signal Event}
           SemaphoreSignal(BulkWaiter.event);
          end;
          
         {Unlock Spinlock}
         SpinUnlock(VCHIQBulkWaiterLock);         
        end
       else if Bulk.mode = VCHIQ_BULK_MODE_CALLBACK then
        begin
         {Check Direction}
         if Bulk.dir = VCHIQ_BULK_TRANSMIT then
          begin
           {Get Reason}
           Reason:=VCHIQ_BULK_TRANSMIT_DONE;
           if Bulk.actual = VCHIQ_BULK_ACTUAL_ABORTED then Reason:=VCHIQ_BULK_TRANSMIT_ABORTED;
          end
         else
          begin
           {Get Reason}
           Reason:=VCHIQ_BULK_RECEIVE_DONE;
           if Bulk.actual = VCHIQ_BULK_ACTUAL_ABORTED then Reason:=VCHIQ_BULK_RECEIVE_ABORTED;
          end;          
         
         {Make Service Callback}
         Status:=VCHIQMakeServiceCallback(Service,Reason,nil,Bulk.userdata);
         if Status = VCHIQ_RETRY then Break;
        end;        
      end;
     
     {Update Remove}
     Inc(Queue.remove);
     
     {Signal Bulk Remove}
     SemaphoreSignal(Service.bulk_remove_event);
    end;
   
   if not RetryPoll then Status:=VCHIQ_SUCCESS;
  end; 
 
 if Status = VCHIQ_RETRY then
  begin
   {Check Queue}
   if Queue = @Service.bulk_tx then
    begin
     {Request Poll}
     VCHIQRequestPoll(Service.state,Service,VCHIQ_POLL_TXNOTIFY);
    end
   else
    begin
     {Request Poll}
     VCHIQRequestPoll(Service.state,Service,VCHIQ_POLL_RXNOTIFY);
    end;    
  end; 
 
 {Return Status}
 Result:=Status;
end;

{==============================================================================}

function VCHIQResolveBulks(Service:PVCHIQ_SERVICE_T;Queue:PVCHIQ_BULK_QUEUE_T):Integer;
{From resolve_bulks in vchiq_core.c}

{Called by the slot handler or application threads, holding the bulk mutex}
var
 Resolved:Integer;
 Bulk:PVCHIQ_BULK_T;
 State:PVCHIQ_STATE_T;
begin
 {}
 Result:=0;

 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Resolve Bulks (Service=' + IntToHex(PtrUInt(Service),8) + ' Queue=' + IntToHex(PtrUInt(Queue),8) + ')');
 {$ENDIF}
 
 {Check Service}
 if Service = nil then Exit;

 {Check Queue}
 if Queue = nil then Exit;
 
 {Get State}
 State:=Service.state;
 
 {Setup Defaults}
 Resolved:=0;
 
 while (Queue.process <> Queue.local_insert) and (Queue.process <> Queue.remote_insert) do
  begin
   {Get Bulk}
   Bulk:=@Queue.bulks[VCHIQ_BULK_INDEX(Queue.process)];

   //To Do //Continuing //Log Error
   
   {Lock Mutex}
   if MutexLock(State.bulk_transfer_mutex) <> ERROR_SUCCESS then Break;
   
   {Transfer Bulk}
   VCHIQTransferBulk(Bulk);
   
   {Unlock Mutex}
   MutexUnlock(State.bulk_transfer_mutex);
   
   //To Do //Continuing //Log Trace
   
   {Complete Bulk}
   VCHIQCompleteBulk(Bulk);
   
   {Update Process}
   Inc(Queue.process);
   
   {Update Resolved}
   Inc(Resolved);
  end;
 
 {Return Resolved}
 Result:=Resolved;
end;

{==============================================================================}

procedure VCHIQAbortOutstandingBulks(Service:PVCHIQ_SERVICE_T;Queue:PVCHIQ_BULK_QUEUE_T);
{From abort_outstanding_bulks in vchiq_core.c}

{Called with the bulk mutex held}
var
 IsTx:Boolean;
 Bulk:PVCHIQ_BULK_T;
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Abort Outstanding Bulks (Service=' + IntToHex(PtrUInt(Service),8) + ' Queue=' + IntToHex(PtrUInt(Queue),8) + ')');
 {$ENDIF}
 
 {Check Service}
 if Service = nil then Exit;

 {Check Queue}
 if Queue = nil then Exit;
 
 {Get Is Tx}
 IsTx:=(Queue = @Service.bulk_tx);
 
 {Check Local Insert}
 if (Queue.local_insert - Queue.process) < 0 then
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: (Queue.local_insert - Queue.process) < 0');
  end;
 
 {Check Remote Insert}
 if (Queue.remote_insert - Queue.process) < 0 then
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: (Queue.remote_insert - Queue.process) < 0');
  end;
 
 while (Queue.process <> Queue.local_insert) or (Queue.process <> Queue.remote_insert) do
  begin
   {Get Bulk}
   Bulk:=@Queue.bulks[VCHIQ_BULK_INDEX(Queue.process)];
 
   if Queue.process = Queue.remote_insert then
    begin
     {Fabricate a matching dummy bulk}
     Bulk.remote_data:=nil;
     Bulk.remote_size:=0;
     
     {Update Remote Insert}
     Inc(Queue.remote_insert);
    end;
    
   if Queue.process <> Queue.local_insert then
    begin
     {Complete Bulk}
     VCHIQCompleteBulk(Bulk);
     
     //To Do //Continuing //Log Trace
    end
   else
    begin
     {Fabricate a matching dummy bulk}
     Bulk.data:=nil;
     Bulk.size:=0;
     Bulk.actual:=VCHIQ_BULK_ACTUAL_ABORTED;
     if IsTx then
      begin
       Bulk.dir:=VCHIQ_BULK_TRANSMIT;
      end
     else
      begin
       Bulk.dir:=VCHIQ_BULK_RECEIVE;
       end;
       
     {Update Local Insert}
     Inc(Queue.local_insert);
    end;    
    
   {Update Process}
   Inc(Queue.process);
  end;
end;
 
{==============================================================================}

procedure VCHIQPauseBulks(State:PVCHIQ_STATE_T);
{From pause_bulks in vchiq_core.c}

{Called from the slot handler thread}
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Pause Bulks (State=' + IntToHex(PtrUInt(State),8) + ')');
 {$ENDIF}
 
 {Check State} 
 if State = nil then Exit;
 
 {Increment Pause Count}
 if InterlockedIncrement(VCHIQPauseBulksCount) <> 1 then
  begin
   InterlockedExchange(VCHIQPauseBulksCount,1);
   
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: VCHIQPauseBulksCount <> 1');
   Exit;
  end;
 
 {Block bulk transfers from all services}
 MutexLock(State.bulk_transfer_mutex);
end;

{==============================================================================}

procedure VCHIQResumeBulks(State:PVCHIQ_STATE_T);
{From resume_bulks in vchiq_core.c}

{Called from the slot handler thread}
var
 Count:Integer;
 ResolvedRx:Integer;
 ResolvedTx:Integer;
 Service:PVCHIQ_SERVICE_T;
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Resume Bulks (State=' + IntToHex(PtrUInt(State),8) + ')');
 {$ENDIF}
 
 {Check State} 
 if State = nil then Exit;
 
 {Decrement Pause Count}
 if InterlockedDecrement(VCHIQPauseBulksCount) <> 0 then
  begin
   InterlockedExchange(VCHIQPauseBulksCount,0);
   
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: VCHIQPauseBulksCount <> 0');
   Exit;
  end;
 
 {Allow bulk transfers from all services}
 MutexUnlock(State.bulk_transfer_mutex);
 
 {Check Deferred Bulks}
 if State.deferred_bulks = 0 then Exit;

 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Resume Bulks (DeferredBulks=' + IntToStr(State.deferred_bulks) + ')');
 {$ENDIF}
 
 {Deal with any bulks which had to be deferred due to being in paused state}
 for Count:=0 to State.unused_service - 1 do
  begin
   {Get Service}
   Service:=State.services[Count];
   
   {Check Service}
   if (Service <> nil) and (Service.srvstate = VCHIQ_SRVSTATE_OPEN) then
    begin
     {Lock Mutex}
     MutexLock(Service.bulk_mutex);
     
     {Resolve Rx and Tx}
     ResolvedRx:=VCHIQResolveBulks(Service,@Service.bulk_rx);
     ResolvedTx:=VCHIQResolveBulks(Service,@Service.bulk_tx);
     
     {Unlock Mutex}
     MutexUnlock(Service.bulk_mutex);
    
     {Notify Rx and Tx}
     if ResolvedRx > 0 then VCHIQNotifyBulks(Service,@Service.bulk_rx,True); {RetryPoll}
     if ResolvedTx > 0 then VCHIQNotifyBulks(Service,@Service.bulk_tx,True); {RetryPoll}
    end;
  end;
  
 {Reset Deferred Bulks}
 State.deferred_bulks:=0;
end;

{==============================================================================}

function VCHIQParseOpen(State:PVCHIQ_STATE_T;Header:PVCHIQ_HEADER_T):Boolean;
{From parse_open in vchiq_core.c}
var
 Size:Integer;
 MsgId:Integer;
 {MsgType:Integer;} {Not Used}
 LocalPort:LongWord;
 RemotePort:LongWord;
 Service:PVCHIQ_SERVICE_T;
 Payload:PVCHIQ_OPEN_PAYLOAD;
 
 Fourcc:Integer;
 Version:SmallInt;
 VersionMin:SmallInt;
 
 Body:VCHIQ_ELEMENT_T;
 AckPayload:VCHIQ_OPENACK_PAYLOAD;
begin
 {}
 Result:=False;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Parse Open (State=' + IntToHex(PtrUInt(State),8) + ')');
 {$ENDIF}
 
 {Check State} 
 if State = nil then Exit;

 {Check Header} 
 if Header = nil then Exit;
 
 {Get Message}
 MsgId:=Header.msgid;
 Size:=Header.size;
 {MsgType:=VCHIQ_MSG_TYPE(MsgId);} {Not Used}
 LocalPort:=VCHIQ_MSG_DSTPORT(MsgId);
 RemotePort:=VCHIQ_MSG_SRCPORT(MsgId);
 
 {Check Size}
 if Size >= SizeOf(VCHIQ_OPEN_PAYLOAD) then
  begin
   {Get Payload}
   Payload:=PVCHIQ_OPEN_PAYLOAD(@Header.data);
   
   {Get Fourcc}
   Fourcc:=Payload.fourcc;
   
   {$IFDEF VC4VCHIQ_DEBUG}
   if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Parse: OPEN (Id=' + IntToStr(State.id) + ' LocalPort=' + IntToStr(LocalPort) + ' Fourcc=' + VCHIQFourccToString(Fourcc) + ')');
   {$ENDIF}
   
   {Get Service}
   Service:=VCHIQGetListeningService(State,Fourcc);
   if Service <> nil then
    begin
     try
      {A matching service exists}
      Version:=Payload.version;
      VersionMin:=Payload.version_min;
      
      {Check Version}
      if (Service.version < VersionMin) or (Version < Service.version_min) then
       begin
        {Version mismatch}
        if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Service Version Mismatch (Id=' + IntToStr(State.id) + ' LocalPort=' + IntToStr(Service.localport) + ' Fourcc=' + VCHIQFourccToString(Fourcc) + ' Local Version / Min=' + IntToStr(Service.version) + ' / ' + IntToStr(Service.version_min) + ' Remote Version / Min=' + IntToStr(Version) + ' / ' + IntToStr(VersionMin) + ')');
        
        {No available service, or an invalid request - send a CLOSE}
        if VCHIQQueueMessageInternal(State,nil,VCHIQ_MAKE_MSG(VCHIQ_MSG_CLOSE,0,VCHIQ_MSG_SRCPORT(MsgId)),nil,0,0,0) = VCHIQ_RETRY then
         begin
          Exit;
         end;
        
        Result:=True;  
        Exit;
       end;
      
      {Update Peer Version}
      Service.peer_version:=Version;
      
      {Check Service State}
      if Service.srvstate = VCHIQ_SRVSTATE_LISTENING then
       begin
        {Create Payload}
        AckPayload.version:=Service.version;
        
        {Create Body}
        Body.data:=@AckPayload;
        Body.size:=SizeOf(VCHIQ_OPENACK_PAYLOAD);
        
        {Check Version}
        if State.version_common < VCHIQ_VERSION_SYNCHRONOUS_MODE then
         begin
          Service.sync:=0;
         end;
        
        {Acknowledge the OPEN}
        if (Service.sync <> 0) and (State.version_common >= VCHIQ_VERSION_SYNCHRONOUS_MODE) then
         begin
          {Queue Message Internal Sync}
          if VCHIQQueueMessageInternalSync(State,nil,VCHIQ_MAKE_MSG(VCHIQ_MSG_OPENACK,Service.localport,RemotePort),@Body,1,SizeOf(VCHIQ_OPENACK_PAYLOAD),False) = VCHIQ_RETRY then
           begin
            Exit;
           end;
         end
        else
         begin
         {Queue Message Internal}
         if VCHIQQueueMessageInternal(State,nil,VCHIQ_MAKE_MSG(VCHIQ_MSG_OPENACK,Service.localport,RemotePort),@Body,1,SizeOf(VCHIQ_OPENACK_PAYLOAD),0) = VCHIQ_RETRY then
          begin
           Exit;
          end;
         end;         
       
        {The service is now open}
        if Service.sync <> 0 then
         begin
          {Set Service State}
          VCHIQSetServiceState(Service,VCHIQ_SRVSTATE_OPENSYNC);
         end
        else
         begin
          {Set Service State}
          VCHIQSetServiceState(Service,VCHIQ_SRVSTATE_OPEN);
         end;
       end;
       
      {Update Service}
      Service.remoteport:=RemotePort;
      Service.client_id:=Payload.client_id;
      
      {Make Service Callback}
      if VCHIQMakeServiceCallback(Service,VCHIQ_SERVICE_OPENED,nil,nil) = VCHIQ_RETRY then
       begin
        {Bail out if not ready}
        Service.remoteport:=VCHIQ_PORT_FREE;
        
        Exit;
       end;
       
      {Success (The message has been dealt with)}
      Result:=True;  
     finally
      {Unlock Service}
      VCHIQUnlockService(Service);
     end;
    end
   else
    begin
     {No available service, or an invalid request - send a CLOSE}
     if VCHIQQueueMessageInternal(State,nil,VCHIQ_MAKE_MSG(VCHIQ_MSG_CLOSE,0,VCHIQ_MSG_SRCPORT(MsgId)),nil,0,0,0) = VCHIQ_RETRY then
      begin
       Exit;
      end;
     
     Result:=True;  
    end;    
  end
 else
  begin
   {No available service, or an invalid request - send a CLOSE}
   if VCHIQQueueMessageInternal(State,nil,VCHIQ_MAKE_MSG(VCHIQ_MSG_CLOSE,0,VCHIQ_MSG_SRCPORT(MsgId)),nil,0,0,0) = VCHIQ_RETRY then
    begin
     Exit;
    end;

   Result:=True;  
  end;  
end;

{==============================================================================}

procedure VCHIQParseRxSlots(State:PVCHIQ_STATE_T);
{From parse_rx_slots in vchiq_core.c}

{Called by the slot handler thread}
var
 TxPos:Integer;
 RxIndex:Integer;
 Size:Integer;
 MsgId:Integer;
 MsgType:Integer;
 LocalPort:LongWord;
 RemotePort:LongWord;
 Header:PVCHIQ_HEADER_T;
 Service:PVCHIQ_SERVICE_T;
 Remote:PVCHIQ_SHARED_STATE_T;
 Payload:PVCHIQ_OPENACK_PAYLOAD;
 
 SkipMessage:Boolean;
 
 Resolved:Integer;
 Bulk:PVCHIQ_BULK_T;
 Queue:PVCHIQ_BULK_QUEUE_T;
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Parse Rx Slots (State=' + IntToHex(PtrUInt(State),8) + ')');
 {$ENDIF}
 
 {Check State} 
 if State = nil then Exit;
 
 {Get Remote}
 Remote:=State.remote;
 
 {Get Tx Pos}
 TxPos:=Remote.tx_pos;
 
 {Set Defaults}
 Service:=nil;
 
 while State.rx_pos <> TxPos do
  begin
   {$IFDEF VC4VCHIQ_DEBUG}
   if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Parse Rx Slots (TxPos=' + IntToStr(TxPos) + ' RxPos=' + IntToStr(State.rx_pos) + ')');
   {$ENDIF}
   
   SkipMessage:=False;
   
   {Check Rx Data}
   if State.rx_data = nil then
    begin
     if (State.rx_pos and VCHIQ_SLOT_MASK) <> 0 then
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: (State.rx_pos and VCHIQ_SLOT_MASK) <> 0');
      end;
    
     {Get Rx Data and Info}
     RxIndex:=Remote.slot_queue[VCHIQ_SLOT_QUEUE_INDEX_FROM_POS(State.rx_pos) and VCHIQ_SLOT_QUEUE_MASK];
     State.rx_data:=VCHIQ_SLOT_DATA_FROM_INDEX(State,RxIndex);
     State.rx_info:=VCHIQ_SLOT_INFO_FROM_INDEX(State,RxIndex);
     
     {$IFDEF VC4VCHIQ_DEBUG}
     if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Parse Rx Slots (RxIndex=' + IntToStr(RxIndex) + ' RxData=' + IntToHex(PtrUInt(State.rx_data),8) + ' RxInfo=' + IntToHex(PtrUInt(State.rx_info),8) + ')');
     {$ENDIF}
     
     {Initialise use_count to one, and increment release_count at the end of the slot to avoid releasing the slot prematurely}
     State.rx_info.use_count:=1;
     State.rx_info.release_count:=0;
    end;
    
   {Get Header}
   Header:=PVCHIQ_HEADER_T(State.rx_data + (State.rx_pos and VCHIQ_SLOT_MASK));
    
   {$IFDEF VC4VCHIQ_DEBUG}
   if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Parse Rx Slots (Header=' + IntToHex(PtrUInt(Header),8) + ')');
   {$ENDIF}
    
   {Get Message}
   MsgId:=Header.msgid;
   Size:=Header.size;
   MsgType:=VCHIQ_MSG_TYPE(MsgId);
   LocalPort:=VCHIQ_MSG_DSTPORT(MsgId);
   RemotePort:=VCHIQ_MSG_SRCPORT(MsgId);
    
   {Update Statistics}
   if MsgType <> VCHIQ_MSG_DATA then Inc(State.stats.ctrl_rx_count);
    
   {Check Message Type}
   case MsgType of
    VCHIQ_MSG_OPENACK,
    VCHIQ_MSG_CLOSE,
    VCHIQ_MSG_DATA,
    VCHIQ_MSG_BULK_RX,
    VCHIQ_MSG_BULK_TX,
    VCHIQ_MSG_BULK_RX_DONE,
    VCHIQ_MSG_BULK_TX_DONE:begin
      {Get Service}
      Service:=VCHIQFindServiceByPort(State,LocalPort);
      
      {Check Service}
      if ((Service = nil) or ((Service.remoteport <> RemotePort) and (Service.remoteport <> VCHIQ_PORT_FREE))) and (LocalPort = 0) and (MsgType = VCHIQ_MSG_CLOSE) then
       begin
        {This could be a CLOSE from a client which hadn't yet received the OPENACK (Look for the connected service)}
        {Unlock Service}
        if Service <> nil then VCHIQUnlockService(Service);
        
        {Get Connected Service}
        Service:=VCHIQGetConnectedService(State,RemotePort);
        if Service <> nil then
         begin
          if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Found Connected Service (Id=' + IntToStr(State.id) + ' MessageType=' + VCHIQMessageTypeToString(MsgType) + ' LocalPort=' + IntToStr(LocalPort) + ' RemotePort=' + IntToStr(RemotePort) + ')');
         end;
       end;
      
      {Check Service}
      if Service = nil then
       begin
        if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Invalid / Closed Service (Id=' + IntToStr(State.id) + ' MessageType=' + VCHIQMessageTypeToString(MsgType) + ' LocalPort=' + IntToStr(LocalPort) + ' RemotePort=' + IntToStr(RemotePort) + ')');
        
        SkipMessage:=True;
       end; 
     end;
    end;
    
   if not SkipMessage then
    begin
     {$IFDEF VC4VCHIQ_DEBUG}
     if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Parse Rx Slots (MsgId=' + IntToHex(MsgId,8) + ' Size=' + IntToStr(Size) + ' MessageType=' + VCHIQMessageTypeToString(MsgType) + ' LocalPort=' + IntToStr(LocalPort) + ' RemotePort=' + IntToStr(RemotePort) + ')');
     {$ENDIF}
     
     //To Do //Continuing //Log Trace
   
     {Check Size}
     if (PtrUInt(Header) and VCHIQ_SLOT_MASK) + VCHIQCalcStride(Size) > VCHIQ_SLOT_SIZE then
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Size Too Big For Slot (MsgId=' + IntToHex(MsgId,8) + ' Size=' + IntToStr(Size) + ')');
      end;
   
     {Check Message Type}
     case MsgType of
      VCHIQ_MSG_OPEN:begin
        {Check Dest Port}
        if VCHIQ_MSG_DSTPORT(MsgId) <> 0 then
         begin
          if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: VCHIQ_MSG_DSTPORT(MsgId) <> 0');
         end;
         
        {Parse Open}
        if not VCHIQParseOpen(State,Header) then
         begin
          {Unlock Service}
          if Service <> nil then VCHIQUnlockService(Service);
          Exit;
         end;         
       end;
      VCHIQ_MSG_OPENACK:begin
        {Check Size}
        if Size >= SizeOf(VCHIQ_OPENACK_PAYLOAD) then
         begin
          {Get Payload}
          Payload:=PVCHIQ_OPENACK_PAYLOAD(@Header.data);
          
          {Get Peer Version}
          Service.peer_version:=Payload.version;
         end; 
         
        {$IFDEF VC4VCHIQ_DEBUG}
        if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Parse: OPENACK (Id=' + IntToStr(State.id) + ' Header=' + IntToHex(PtrUInt(Header),8) + ' Size=' + IntToStr(Size) + ' RemotePort=' + IntToStr(RemotePort) + ' LocalPort=' + IntToStr(LocalPort) + ' PeerVersion=' + IntToStr(Service.peer_version) + ')');
        {$ENDIF}
         
        {Check Service State}
        if Service.srvstate = VCHIQ_SRVSTATE_OPENING then
         begin
          {Update Service}
          Service.remoteport:=RemotePort;
          VCHIQSetServiceState(Service,VCHIQ_SRVSTATE_OPEN);
          
          {Signal Remove Event}
          SemaphoreSignal(Service.remove_event);
         end
        else
         begin
          if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: OPENACK received in state ' + VCHIQServiceStateToString(Service.srvstate));
         end;         
       end;  
      VCHIQ_MSG_CLOSE:begin
        {Check Size}
        if Size <> 0 then
         begin
          if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Size <> 0');
         end;
        
        {$IFDEF VC4VCHIQ_DEBUG}
        if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Parse: CLOSE (Id=' + IntToStr(State.id) + ' Header=' + IntToHex(PtrUInt(Header),8) + ' RemotePort=' + IntToStr(RemotePort) + ' LocalPort=' + IntToStr(LocalPort) + ')');
        {$ENDIF}
        
        {Mark Service Closing Internal}
        VCHIQMarkServiceClosingInternal(Service,True);
        
        {Close Service Internal}
        if VCHIQCloseServiceInternal(Service,True) = VCHIQ_RETRY then {CloseReceived}
         begin
          {Unlock Service}
          if Service <> nil then VCHIQUnlockService(Service);
          Exit;
         end;
        
        {$IFDEF VC4VCHIQ_DEBUG}
        if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Parse: Close Service (Fourcc=' + VCHIQFourccToString(Service.base.fourcc) + ' LocalPort=' + IntToStr(Service.localport) + ' RemotePort=' + IntToStr(Service.remoteport) + ')');
        {$ENDIF}
       end;
      VCHIQ_MSG_DATA:begin
        {$IFDEF VC4VCHIQ_DEBUG}
        if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Parse: DATA (Id=' + IntToStr(State.id) + ' Header=' + IntToHex(PtrUInt(Header),8) + ' Size=' + IntToStr(Size) + ' RemotePort=' + IntToStr(RemotePort) + ' LocalPort=' + IntToStr(LocalPort) + ')');
        {$ENDIF}
        
        {Check Remote Port and Service State}
        if (Service.remoteport = RemotePort) and (Service.srvstate = VCHIQ_SRVSTATE_OPEN) then
         begin
          {Update Header}
          Header.msgid:=MsgId or VCHIQ_MSGID_CLAIMED;
          
          {Claim Slot}
          VCHIQClaimSlot(State.rx_info);
          
          {Make Service Callback}
          if VCHIQMakeServiceCallback(Service,VCHIQ_MESSAGE_AVAILABLE,Header,nil) = VCHIQ_RETRY then
           begin
            {Unlock Service}
            if Service <> nil then VCHIQUnlockService(Service);
            Exit;
           end;
           
          {Update Statistics} 
          Inc(Service.stats.ctrl_rx_count);
          Inc(Service.stats.ctrl_rx_bytes,Size);
         end
        else
         begin
          {Update Statistics} 
          Inc(State.stats.error_count);
         end;
       end;
      VCHIQ_MSG_CONNECT:begin
        {$IFDEF VC4VCHIQ_DEBUG}
        if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Parse: CONNECT (Id=' + IntToStr(State.id) + ' Header=' + IntToHex(PtrUInt(Header),8) + ')');
        {$ENDIF}
        
        {Get Version Common}
        State.version_common:=PVCHIQ_SLOT_ZERO_T(State.slot_data).version;

        {$IFDEF VC4VCHIQ_DEBUG}
        if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Parse Rx Slots (VersionCommon=' + IntToStr(State.version_common) + ')');
        {$ENDIF}
        
        {Signal Connect}
        SemaphoreSignal(State.connect);
       end;
      VCHIQ_MSG_BULK_RX,
      VCHIQ_MSG_BULK_TX:begin
        {Check Master}
        if not State.is_master then
         begin
          if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: not State.is_master');
         end;
         
        {Get Queue}
        if MsgType = VCHIQ_MSG_BULK_RX then
         begin
          Queue:=@Service.bulk_tx;
         end
        else
         begin
          Queue:=@Service.bulk_rx;
         end;         
        
        {Check Remote Port and Service State}
        if (Service.remoteport = RemotePort) and (Service.srvstate = VCHIQ_SRVSTATE_OPEN) then
         begin
          {Lock Mutex}
          if MutexLock(Service.bulk_mutex) <> ERROR_SUCCESS then
           begin
            {Unlock Service}
            if Service <> nil then VCHIQUnlockService(Service);
            Exit;
           end;
           
          if Queue.remote_insert >= Queue.remove + VCHIQ_NUM_SERVICE_BULKS then
           begin
            if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Queue.remote_insert >= Queue.remove + VCHIQ_NUM_SERVICE_BULKS');
           end;
          
          {Get Bulk}
          Bulk:=@Queue.bulks[VCHIQ_BULK_INDEX(Queue.remote_insert)];
          Bulk.remote_data:=Pointer(PVCHIQ_BULK_PAYLOAD(@Header.data).data);
          Bulk.remote_size:=PVCHIQ_BULK_PAYLOAD(@Header.data).size;
          
          {Memory Barrier}
          DataMemoryBarrier;
          
          {$IFDEF VC4VCHIQ_DEBUG}
          if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Parse: BULK (Id=' + IntToStr(State.id) + ' Header=' + IntToHex(PtrUInt(Header),8) + ' RemotePort=' + IntToStr(RemotePort) + ' LocalPort=' + IntToStr(LocalPort) + ' RemoteSize=' + IntToStr(Bulk.remote_size) + ' RemoteData=' + IntToHex(PtrUInt(Bulk.remote_data),8) + ')');
          {$ENDIF}
           
          {Update Remote Insert}
          Inc(Queue.remote_insert);
          
          Resolved:=0;
          
          {Check Pause}
          if VCHIQPauseBulksCount <> 0 then
           begin
            {Update Deferred}
            Inc(State.deferred_bulks);
            
            {$IFDEF VC4VCHIQ_DEBUG}
            if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Deferring Bulk (DeferredBulks=' + IntToStr(State.deferred_bulks) + ')');
            {$ENDIF}
            
            {Check State}
            if State.conn_state <> VCHIQ_CONNSTATE_PAUSE_SENT then
             begin
              if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: State.conn_state <> VCHIQ_CONNSTATE_PAUSE_SENT');
             end;
           end
          else if State.conn_state = VCHIQ_CONNSTATE_CONNECTED then
           begin
            {Resolve Bulks}
            Resolved:=VCHIQResolveBulks(Service,Queue);
           end;
           
          {Unlock Mutex} 
          MutexUnlock(Service.bulk_mutex);
          
          {Notify Bulks}
          if Resolved <> 0 then VCHIQNotifyBulks(Service,Queue,True); {RetryPoll}
         end;
       end;
      VCHIQ_MSG_BULK_RX_DONE,
      VCHIQ_MSG_BULK_TX_DONE:begin
        {Check Master}
        if State.is_master then
         begin
          if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: State.is_master');
         end;
        
        {Check Remote Port and Service State}
        if (Service.remoteport = RemotePort) and (Service.srvstate <> VCHIQ_SRVSTATE_FREE) then
         begin
          {Get Queue}
          if MsgType = VCHIQ_MSG_BULK_RX_DONE then
           begin
            Queue:=@Service.bulk_rx;
           end
          else
           begin
            Queue:=@Service.bulk_tx;
           end;
         
          {Lock Mutex}
          if MutexLock(Service.bulk_mutex) <> ERROR_SUCCESS then
           begin
            {Unlock Service}
            if Service <> nil then VCHIQUnlockService(Service);
            Exit;
           end;
         
          if (Queue.remote_insert - Queue.local_insert) >= 0 then
           begin
            if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: (Queue.remote_insert - Queue.local_insert) >= 0');
            
            {Unlock Mutex}
            MutexUnlock(Service.bulk_mutex);
            
            Break;
           end;
         
          if Queue.process = Queue.local_insert then
           begin
            if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Queue.process = Queue.local_insert');
            ThreadHalt(0);
           end;
           
          if Queue.process <> Queue.remote_insert then
           begin
            if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Queue.process <> Queue.remote_insert');
            ThreadHalt(0);
           end;
           
          {Get Bulk}
          Bulk:=@Queue.bulks[VCHIQ_BULK_INDEX(Queue.remote_insert)];
          Bulk.actual:=PInteger(@Header.data)^;
          
          {Update Remote Insert}
          Inc(Queue.remote_insert);
          
          {$IFDEF VC4VCHIQ_DEBUG}
          if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Parse: BULK DONE (Id=' + IntToStr(State.id) + ' Header=' + IntToHex(PtrUInt(Header),8) + ' RemotePort=' + IntToStr(RemotePort) + ' LocalPort=' + IntToStr(LocalPort) + ' Actual=' + IntToStr(Bulk.actual) + ' Data=' + IntToHex(PtrUInt(Bulk.data),8) + ')');
          if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Parse: BULK DONE (Id=' + IntToStr(State.id) + ' LocalInsert=' + IntToStr(Queue.local_insert) + ' RemoteInsert=' + IntToStr(Queue.remote_insert) + ' Process=' + IntToStr(Queue.process) + ')');
          {$ENDIF}
         
          if Queue.process = Queue.local_insert then
           begin
            if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Queue.process = Queue.local_insert');
           end;
           
          {Complete Bulk}
          VCHIQCompleteBulk(Bulk);
          
          {Update Process}
          Inc(Queue.process);
          
          {Unlock Mutex}
          MutexUnlock(Service.bulk_mutex);
          
          {Notify Bulks}
          VCHIQNotifyBulks(Service,Queue,True); {RetryPoll}
         end;
       end;
      VCHIQ_MSG_PADDING:begin
        {$IFDEF VC4VCHIQ_DEBUG}
        if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Parse: PADDING (Id=' + IntToStr(State.id) + ' Header=' + IntToHex(PtrUInt(Header),8) + ' Size=' + IntToStr(Size) + ')');
        {$ENDIF}
       end;
      VCHIQ_MSG_PAUSE:begin
        {If initiated, signal the application thread}
        {$IFDEF VC4VCHIQ_DEBUG}
        if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Parse: PAUSE (Id=' + IntToStr(State.id) + ' Header=' + IntToHex(PtrUInt(Header),8) + ' Size=' + IntToStr(Size) + ')');
        {$ENDIF}
        
        {Check Connection State}
        if State.conn_state = VCHIQ_CONNSTATE_PAUSED then
         begin
          if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: PAUSE received in state PAUSED (Id=' + IntToStr(State.id) + ')');
          
          Break;
         end;
         
        {Check Connection State}
        if State.conn_state <> VCHIQ_CONNSTATE_PAUSE_SENT then
         begin
          {Send a PAUSE in response}
          if VCHIQQueueMessageInternal(State,nil,VCHIQ_MAKE_MSG(VCHIQ_MSG_PAUSE,0,0),nil,0,0,QMFLAGS_NO_MUTEX_UNLOCK) = VCHIQ_RETRY then
           begin
            {Unlock Service}
            if Service <> nil then VCHIQUnlockService(Service);
            Exit;
           end;
          
          {Pause Bulks}
          if State.is_master then VCHIQPauseBulks(State);
         end;
         
        {At this point slot_mutex is held}
        VCHIQSetConnState(State,VCHIQ_CONNSTATE_PAUSED);
        VCHIQPlatformPaused(State);
       end;
      VCHIQ_MSG_RESUME:begin
        {$IFDEF VC4VCHIQ_DEBUG}
        if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Parse: RESUME (Id=' + IntToStr(State.id) + ' Header=' + IntToHex(PtrUInt(Header),8) + ' Size=' + IntToStr(Size) + ')');
        {$ENDIF}
        
        {Release the slot mutex}
        MutexUnlock(State.slot_mutex);
        
        {Resume Bulks}
        if State.is_master then VCHIQResumeBulks(State);
        
        VCHIQSetConnState(State,VCHIQ_CONNSTATE_CONNECTED);
        VCHIQPlatformResumed(State);
       end;
      VCHIQ_MSG_REMOTE_USE:begin
        {On Remote Use}
        VCHIQOnRemoteUse(State);
       end;
      VCHIQ_MSG_REMOTE_RELEASE:begin
        {On Remote Release}
        VCHIQOnRemoteRelease(State);
       end;
      VCHIQ_MSG_REMOTE_USE_ACTIVE:begin
        {On Remote Use Active}
        VCHIQOnRemoteUseActive(State);
       end;
      else
       begin
        if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Invalid Message Id (Id=' + IntToStr(State.id) + ' MsgId=' + IntToHex(MsgId,8) + ' Size=' + IntToStr(Size) + ')');
       end;
     end;
    end;
   
   {Unlock Service}
   if Service <> nil then
    begin
     VCHIQUnlockService(Service);
     Service:=nil;
    end;
   
   {Update Rx Pos}
   State.rx_pos:=State.rx_pos + VCHIQCalcStride(Size);
   
   {Perform some housekeeping when the end of the slot is reached}
   if (State.rx_pos and VCHIQ_SLOT_MASK) = 0 then
    begin
     {Remove the extra reference count}
     VCHIQReleaseSlot(State,State.rx_info,nil,nil);
     State.rx_data:=nil;
    end;
  end;
end;

{==============================================================================}

function VCHIQSlotHandlerExecute(VCHIQ:PVCHIQDevice):PtrInt;
{From slot_handler_func in vchiq_core.c}

{Called by the slot handler thread}
var
 State:PVCHIQ_STATE_T;
 Local:PVCHIQ_SHARED_STATE_T;
begin
 {}
 Result:=0;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Slot Handler Execute (ThreadID=' + IntToHex(ThreadGetCurrent,8) + ')');
 {$ENDIF}
 
 {Check VCHIQ}
 if VCHIQ = nil then Exit;
 
 {Get State}
 State:=VCHIQ.State;
 
 {Get Local}
 Local:=State.local;
 
 {Update Priority}
 Sleep(1);
 
 while True do
  begin
   {Wait for Trigger Event}
   VCHIQRemoteEventWait(@Local.trigger);
   
   {Memory Barrier}
   DataMemoryBarrier;
   
   {Check Poll Needed}
   if State.poll_needed <> 0 then
    begin
     {Check if we need to suspend (May change our conn_state)}
     VCHIQPlatformCheckSuspend(State);
     
     {Reset Poll Needed}
     State.poll_needed:=0;
     
     {Handle service polling and other rare conditions here out of the mainline code}
     case State.conn_state of
      VCHIQ_CONNSTATE_CONNECTED:begin
        {Poll the services as requested}
        VCHIQPollServices(State);
       end;
      VCHIQ_CONNSTATE_PAUSING:begin
       {Check Master}
       if State.is_master then
        begin
         {Pause Bulks}
         VCHIQPauseBulks(State);
        end;
       
       {Queue Message Internal}
       if VCHIQQueueMessageInternal(State,nil,VCHIQ_MAKE_MSG(VCHIQ_MSG_PAUSE,0,0),nil,0,0,QMFLAGS_NO_MUTEX_UNLOCK) <> VCHIQ_RETRY then
        begin
         {Set Connection State}
         VCHIQSetConnState(State,VCHIQ_CONNSTATE_PAUSE_SENT);
        end
       else
        begin
         {Check Master}
         if State.is_master then
          begin
           {Resume Bulks}
           VCHIQResumeBulks(State);
          end;
          
         {Retry later}
         State.poll_needed:=1;
        end;        
       end;
      VCHIQ_CONNSTATE_PAUSED:begin
        {Platform Resume}
        VCHIQPlatformResume(State);
       end;
      VCHIQ_CONNSTATE_RESUMING:begin
        {Queue Message Internal}
        if VCHIQQueueMessageInternal(State,nil,VCHIQ_MAKE_MSG(VCHIQ_MSG_RESUME,0,0),nil,0,0,QMFLAGS_NO_MUTEX_LOCK) <> VCHIQ_RETRY then
         begin
          {Check Master}
          if State.is_master then
           begin
            {Resume Bulks}
            VCHIQResumeBulks(State);
           end;
           
          {Set Connection State}
          VCHIQSetConnState(State,VCHIQ_CONNSTATE_CONNECTED);
          
          {Platform Resumed}
          VCHIQPlatformResumed(State);
         end
        else 
         begin
          if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Failed to send VCHIQ_MSG_RESUME');
          
          {This should really be impossible, since the PAUSE should have flushed through outstanding messages}
          ThreadHalt(0);
         end;
       end;
      VCHIQ_CONNSTATE_PAUSE_TIMEOUT,VCHIQ_CONNSTATE_RESUME_TIMEOUT:begin
        {Platform Handle Timeout}
        VCHIQPlatformHandleTimeout(State);
       end;
     end;
    end; 
   
   {Parse Rx Slots}
   VCHIQParseRxSlots(State);
  end; 
end;

{==============================================================================}
{==============================================================================}
{VCHIQ Platform Functions}
function VCHIQPlatformInit(VCHIQ:PVCHIQDevice;State:PVCHIQ_STATE_T):LongWord;
{From vchiq_platform_init in vchiq_2835_arm.c}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Platform Init (VCHIQ=' + IntToHex(PtrUInt(VCHIQ),8) + ' State=' + IntToHex(PtrUInt(State),8) + ')');
 {$ENDIF}
 
 {Check VCHIQ}
 if VCHIQ = nil then Exit;
 if VCHIQ.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check State}
 if State = nil then Exit;
 
 //To Do //Not Required, built into VCHIQDeviceStart //Should we separate for clarity ?
 
end;

{==============================================================================}

function VCHIQPlatformInitState(State:PVCHIQ_STATE_T):VCHIQ_STATUS_T;
{From vchiq_platform_init_state in vchiq_2835_arm.c}
{ And vchiq_arm_init_state in vchiq_arm.c}
var
 PlatformState:PVCHIQ_PLATFORM_STATE_T;
begin
 {}
 Result:=VCHIQ_ERROR;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Platform Init State (State=' + IntToHex(PtrUInt(State),8) + ')');
 {$ENDIF}
 
 {Check State} 
 if State = nil then Exit;
 
 {Allocate Platform State}
 PlatformState:=AllocMem(SizeOf(VCHIQ_PLATFORM_STATE_T));
 if PlatformState = nil then Exit;
 
 {Initialize Platform State}
 PlatformState.susp_res_lock:=SynchronizerCreate;
 
 PlatformState.ka_evt:=CompletionCreate(COMPLETION_FLAG_COUNTED);
 PlatformState.ka_use_count:=0;
 PlatformState.ka_use_ack_count:=0;
 PlatformState.ka_release_count:=0;

 PlatformState.vc_suspend_complete:=CompletionCreate(COMPLETION_FLAG_COUNTED);

 PlatformState.vc_resume_complete:=CompletionCreate(COMPLETION_FLAG_COUNTED);
 {Initialise to 'done' state.  We only want to block on resume completion while VideoCore is suspended}
 VCHIQSetResumeState(PlatformState,VC_RESUME_RESUMED);
 
 PlatformState.resume_blocker:=CompletionCreate(COMPLETION_FLAG_COUNTED);
 {Initialise to 'done' state.  We only want to block on this completion while resume is blocked}
 CompletionCompleteAll(PlatformState.resume_blocker);
 
 PlatformState.blocked_blocker:=CompletionCreate(COMPLETION_FLAG_COUNTED);
 {Initialise to 'done' state.  We only want to block on this completion while things are waiting on the resume blocker}
 CompletionCompleteAll(PlatformState.blocked_blocker);
 
 PlatformState.suspend_timer_timeout:=VCHIQ_SUSPEND_TIMER_TIMEOUT_MS;
 PlatformState.suspend_timer_running:=False;
 PlatformState.suspend_timer:=TimerCreate(VCHIQ_SUSPEND_TIMER_TIMEOUT_MS,False,False,TTimerEvent(VCHIQSuspendTimerCallback),State);
 
 PlatformState.first_connect:=False;
 
 PlatformState.inited:=True;
 
 {Store Platform State}
 State.platform_state:=PlatformState;
 
 {Return Success}
 Result:=VCHIQ_SUCCESS;
end;

{==============================================================================}

function VCHIQCheckService(Service:PVCHIQ_SERVICE_T):VCHIQ_STATUS_T;
{From vchiq_check_service in vchiq_arm.c}
var
 PlatformState:PVCHIQ_PLATFORM_STATE_T;
begin
 {}
 Result:=VCHIQ_ERROR;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Check Service (Service=' + IntToHex(PtrUInt(Service),8) + ')');
 {$ENDIF}
 
 {Check Service} 
 if Service = nil then Exit;

 {Check State}
 if Service.state = nil then Exit;
 
 {Get Platform State}
 PlatformState:=Service.state.platform_state;
 if PlatformState = nil then Exit;
 
 {Lock Reader}
 if SynchronizerReaderLock(PlatformState.susp_res_lock) <> ERROR_SUCCESS then Exit;
 
 if Service.service_use_count <> 0 then
  begin
   Result:=VCHIQ_SUCCESS;
  end;
  
 {Unlock Reader}
 SynchronizerReaderUnlock(PlatformState.susp_res_lock);
 
 if Result = VCHIQ_ERROR then
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Check Service (Fourcc=' + VCHIQFourccToString(Service.base.fourcc) + ' ClientId=' + IntToStr(Service.client_id) + ' ServiceUseCont=' + IntToStr(Service.service_use_count) + ' VideocoreUseCount=' + IntToStr(PlatformState.videocore_use_count) + ')');
  end; 
end; 

{==============================================================================}

function VCHIQUseServiceNoResume(Handle:VCHIQ_SERVICE_HANDLE_T):VCHIQ_STATUS_T;
{From vchiq_use_service_no_resume in vchiq_arm.c}
var
 Service:PVCHIQ_SERVICE_T;
begin
 {}
 Result:=VCHIQ_ERROR;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Use Service No Resume (Handle=' + IntToHex(Handle,8) + ')');
 {$ENDIF}
 
 {Get Service}
 Service:=VCHIQFindServiceByHandle(Handle);
 if Service <> nil then
  begin
   {Use Internal}
   Result:=VCHIQUseInternal(Service.state,Service,USE_TYPE_SERVICE_NO_RESUME);
   
   {Unlock Service}
   VCHIQUnlockService(Service);
  end;
end; 

{==============================================================================}

function VCHIQUseService(Handle:VCHIQ_SERVICE_HANDLE_T):VCHIQ_STATUS_T;
{From vchiq_use_service in vchiq_arm.c}
var
 Service:PVCHIQ_SERVICE_T;
begin
 {}
 Result:=VCHIQ_ERROR;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Use Service (Handle=' + IntToHex(Handle,8) + ')');
 {$ENDIF}
 
 {Get Service}
 Service:=VCHIQFindServiceByHandle(Handle);
 if Service <> nil then
  begin
   {Use Internal}
   Result:=VCHIQUseInternal(Service.state,Service,USE_TYPE_SERVICE);
   
   {Unlock Service}
   VCHIQUnlockService(Service);
  end;
end; 

{==============================================================================}

function VCHIQReleaseService(Handle:VCHIQ_SERVICE_HANDLE_T):VCHIQ_STATUS_T;
{From vchiq_release_service in vchiq_arm.c}
var
 Service:PVCHIQ_SERVICE_T;
begin
 {}
 Result:=VCHIQ_ERROR;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Release Service (Handle=' + IntToHex(Handle,8) + ')');
 {$ENDIF}
 
 {Get Service}
 Service:=VCHIQFindServiceByHandle(Handle);
 if Service <> nil then
  begin
   {Release Internal}
   Result:=VCHIQReleaseInternal(Service.state,Service);
   
   {Unlock Service}
   VCHIQUnlockService(Service);
  end;
end; 

{==============================================================================}

function VCHIQAddCompletion(Instance:PVCHIQInstance;Reason:VCHIQ_REASON_T;Header:PVCHIQ_HEADER_T;UserService:PVCHIQ_USER_SERVICE_T;BulkUserdata:Pointer):VCHIQ_STATUS_T;
{From add_completion in vchiq_arm.c}
var
 Completion:PVCHIQ_COMPLETION_DATA_T;
 CompletionInsert:Integer;
begin
 {}
 Result:=VCHIQ_ERROR;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Add Completion (Instance=' + IntToHex(PtrUInt(Instance),8) + ' Reason=' + VCHIQReasonToString(Reason) + ' Header=' + IntToHex(PtrUInt(Header),8) + ' UserService=' + IntToHex(PtrUInt(UserService),8) + ')');
 {$ENDIF}
 
 {Check Instance}
 if Instance = nil then Exit;
 
 {Check User Service}
 if UserService = nil then Exit;
 
 CompletionInsert:=Instance.CompletionInsert;
 while (CompletionInsert - Instance.CompletionRemove) >= VCHIQ_MAX_COMPLETIONS do
  begin
   {Out of space - wait for the client}
   {$IFDEF VC4VCHIQ_DEBUG}
   if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Add Completion: Completion queue full');
   {$ENDIF}
   
   {Wait for Remove Event}
   if SemaphoreWait(Instance.RemoveEvent) <> ERROR_SUCCESS then
    begin
     {$IFDEF VC4VCHIQ_DEBUG}
     if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Add Completion: Wait failed');
     {$ENDIF}
    
     Result:=VCHIQ_RETRY;
     Exit;
    end;
    
   {Check Closing}
   if Instance.Closing then
    begin
     {$IFDEF VC4VCHIQ_DEBUG}
     if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Add Completion: Instance closing');
     {$ENDIF}
     
     Result:=VCHIQ_SUCCESS;
     Exit;
    end;    
  end;
 
 {Get Completion} 
 Completion:=@Instance.Completions[CompletionInsert and (VCHIQ_MAX_COMPLETIONS - 1)];
 
 {Update Completion}
 Completion.header:=Header;
 Completion.reason:=Reason;
 {N.B. service_userdata is updated while processing AWAIT_COMPLETION}
 Completion.service_userdata:=UserService.service;
 Completion.bulk_userdata:=BulkUserdata;
 
 {Check Reason}
 if Reason = VCHIQ_SERVICE_CLOSED then
  begin
   {Take an extra reference, to be held until this CLOSED notification is delivered}
   VCHIQLockService(UserService.service);
   if Instance.UseCloseDelivered then
    begin
     UserService.close_pending:=True;
    end;
  end;
  
 {A write barrier is needed here to ensure that the entire completion record is written out before the insert point} 
 DataMemoryBarrier;
 
 {Check Reason}
 if Reason = VCHIQ_MESSAGE_AVAILABLE then
  begin
   UserService.message_available_pos:=CompletionInsert;
  end;
  
 {Update Completion Insert}
 Inc(CompletionInsert);
 Instance.CompletionInsert:=CompletionInsert;
  
 {Signal Insert Event}
 SemaphoreSignal(Instance.InsertEvent);
 
 {Return Success}
 Result:=VCHIQ_SUCCESS;
end;

{==============================================================================}

function VCHIQServiceCallback(Reason:VCHIQ_REASON_T;Header:PVCHIQ_HEADER_T;Handle:VCHIQ_SERVICE_HANDLE_T;BulkUserdata:Pointer):VCHIQ_STATUS_T;
{From service_callback in vchiq_arm.c}
var
 Status:VCHIQ_STATUS_T;
 SkipCompletion:Boolean;
 Instance:PVCHIQInstance;
 Service:PVCHIQ_SERVICE_T;
 UserService:PVCHIQ_USER_SERVICE_T;
begin
 {}
 Result:=VCHIQ_ERROR;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Service Callback (Reason=' + VCHIQReasonToString(Reason) + ')');
 {$ENDIF}
 
 {How do we ensure the callback goes to the right client? The service userdata points to a VCHIQ_USER_SERVICE_T record
  containing the original callback and the user state structure, which contains a circular buffer for completion records}
  
 {Get Service}
 Service:=VCHIQHandleToService(Handle);
 if Service = nil then Exit;
 
 {Get User Service}
 UserService:=PVCHIQ_USER_SERVICE_T(Service.base.userdata);
 if UserService = nil then Exit;
 
 {Get Instance}
 Instance:=UserService.instance;
 if (Instance = nil) or (Instance.Closing) then
  begin
   Result:=VCHIQ_SUCCESS;
   Exit;
  end; 
 
 {Setup Defaults}
 SkipCompletion:=False;
 
 {Check Header and User Service}
 if (Header <> nil) and (UserService.is_vchi) then
  begin
   {Lock Spinlock}
   if SpinLock(VCHIQMessageQueueLock) <> ERROR_SUCCESS then Exit;
   
   {Check Message Queue}
   while UserService.msg_insert = (UserService.msg_remove + VCHIQ_MSG_QUEUE_SIZE) do
    begin
     {Unlock Spinlock}
     SpinUnlock(VCHIQMessageQueueLock);
    
     {$IFDEF VC4VCHIQ_DEBUG}
     if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Service Callback: Message queue full');
     {$ENDIF}
    
     {If there is no MESSAGE_AVAILABLE in the completion queue, add one}
     if (UserService.message_available_pos - Instance.CompletionRemove) < 0 then
      begin
       {$IFDEF VC4VCHIQ_DEBUG}
       if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Service Callback: Inserting extra MESSAGE_AVAILABLE');
       {$ENDIF}
      
       {Add Completion}
       Status:=VCHIQAddCompletion(Instance,Reason,nil,UserService,BulkUserdata);
       if Status <> VCHIQ_SUCCESS then
        begin
         Result:=Status;
         Exit;
        end;
      end;
      
     {Wait for Remove Event}
     if SemaphoreWait(UserService.remove_event) <> ERROR_SUCCESS then
      begin
       {$IFDEF VC4VCHIQ_DEBUG}
       if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Service Callback: Wait failed');
       {$ENDIF}
      
       Result:=VCHIQ_RETRY;
       Exit;
      end
     else if Instance.Closing then
      begin
       {$IFDEF VC4VCHIQ_DEBUG}
       if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Service Callback: Instance closing');
       {$ENDIF}
      
       Result:=VCHIQ_ERROR;
       Exit;
      end;
    
     {Lock Spinlock}
     SpinLock(VCHIQMessageQueueLock);
    end;
   
   {Update Message Queue}
   UserService.msg_queue[UserService.msg_insert and (VCHIQ_MSG_QUEUE_SIZE - 1)]:=Header;
   Inc(UserService.msg_insert);
   
   {If there is a thread waiting in DEQUEUE_MESSAGE, or if there is a MESSAGE_AVAILABLE in the completion queue then bypass the completion queue}
   if ((UserService.message_available_pos - Instance.CompletionRemove) >= 0) or (UserService.dequeue_pending) then
    begin
     UserService.dequeue_pending:=False;
     SkipCompletion:=True;
    end;
   
   {Unlock Spinlock}
   SpinUnlock(VCHIQMessageQueueLock);
   
   {Signal Insert Event}
   SemaphoreSignal(UserService.insert_event);
   
   {Reset Header}
   Header:=nil;
  end;
  
 {Check Skip} 
 if SkipCompletion then
  begin
   Result:=VCHIQ_SUCCESS;
   Exit;
  end;
  
 {Add Completion}
 Result:=VCHIQAddCompletion(Instance,Reason,Header,UserService,BulkUserdata);
end;

{==============================================================================}

procedure VCHIQUserServiceFree(Userdata:Pointer);
{From user_service_free in vchiq_arm.c}
var
 UserService:PVCHIQ_USER_SERVICE_T;
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: User Service Free (Userdata=' + IntToHex(PtrUInt(Userdata),8) + ')');
 {$ENDIF}
 
 {Check Userdata}
 if Userdata = nil then Exit;

 {Get User Service}
 UserService:=PVCHIQ_USER_SERVICE_T(Userdata);
 
 {Destroy Semaphores}
 SemaphoreDestroy(UserService.insert_event);
 SemaphoreDestroy(UserService.remove_event);
 SemaphoreDestroy(UserService.close_event);
 
 {Invalidate Semaphores}
 UserService.insert_event:=INVALID_HANDLE_VALUE;
 UserService.remove_event:=INVALID_HANDLE_VALUE;
 UserService.close_event:=INVALID_HANDLE_VALUE;
 
 {Free User Service}
 FreeMem(UserService);
end;

{==============================================================================}

procedure VCHIQCloseDelivered(UserService:PVCHIQ_USER_SERVICE_T);
{From close_delivered in vchiq_arm.c}
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Close Delivered (UserService=' + IntToHex(PtrUInt(UserService),8) + ')');
 {$ENDIF}

 {Check User Service}
 if UserService = nil then Exit;
 
 if UserService.close_pending then
  begin
   {Allow the underlying service to be culled}
   VCHIQUnlockService(UserService.service);
   
   {Wake the user-thread blocked in close_ or remove_service}
   SemaphoreSignal(UserService.close_event);
   
   UserService.close_pending:=False;
  end;
end;

{==============================================================================}

function VCHIQPrepareBulkData(Bulk:PVCHIQ_BULK_T;MemHandle:VCHI_MEM_HANDLE_T;Offset:Pointer;Size:Integer;Dir:VCHIQ_BULK_DIR_T):VCHIQ_STATUS_T;
{From vchiq_prepare_bulk_data in vchiq_2835_arm.c}
var
 PageType:Word;
 Pagelist:PVCHIQ_PAGELIST_T;
begin
 {}
 Result:=VCHIQ_ERROR;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Prepare Bulk Data (Bulk=' + IntToHex(PtrUInt(Bulk),8) + ' MemHandle=' + IntToHex(MemHandle,8) + ' Offset=' + IntToHex(PtrUInt(Offset),8) + ' Size=' + IntToStr(Size) + ' Dir=' + VCHIQBulkDirToString(Dir) + ')');
 {$ENDIF}

 {Check Bulk}
 if Bulk = nil then Exit;
 
 {Check MemHandle}
 if MemHandle <> VCHI_MEM_HANDLE_INVALID then
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: MemHandle <> VCHI_MEM_HANDLE_INVALID'); 
  end;
  
 {Get Page Type}
 PageType:=VCHIQ_PAGELIST_WRITE;
 if Dir = VCHIQ_BULK_RECEIVE then PageType:=VCHIQ_PAGELIST_READ;
 
 {Create Pagelist}
 if VCHIQCreatePagelist(Offset,Size,PageType,Pagelist) <> ERROR_SUCCESS then Exit;
 
 {Update Bulk}
 Bulk.handle:=MemHandle;
 Bulk.data:=Pointer(PhysicalToBusAddress(Pagelist));
 
 {Store the pagelist address in remote_data, which isn't used by the slave}
 Bulk.remote_data:=Pagelist;
 
 {Return Success}
 Result:=VCHIQ_SUCCESS;
end;

{==============================================================================}

procedure VCHIQCompleteBulk(Bulk:PVCHIQ_BULK_T);
{From vchiq_complete_bulk in vchiq_2835_arm.c}
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Complete Bulk (Bulk=' + IntToHex(PtrUInt(Bulk),8) + ')');
 {$ENDIF}

 {Check Bulk}
 if Bulk = nil then Exit;
 
 {Check Remote Data}
 if Bulk.remote_data = nil then Exit;
 
 {Check Actual Size}
 if Bulk.actual = 0 then Exit;
 
 {Free Pagelist}
 VCHIQFreePagelist(PVCHIQ_PAGELIST_T(Bulk.remote_data),Bulk.actual);
end;

{==============================================================================}

procedure VCHIQTransferBulk(Bulk:PVCHIQ_BULK_T);
{From vchiq_transfer_bulk in vchiq_2835_arm.c}
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Transfer Bulk (Bulk=' + IntToHex(PtrUInt(Bulk),8) + ')');
 {$ENDIF}
 
 {This should only be called on the master (VideoCore) side}
 ThreadHalt(0);
end;

{==============================================================================}

function VCHIQGetState:PVCHIQ_STATE_T;
{From vchiq_get_state in vchiq_arm.c}
begin
 {}
 Result:=nil;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Get State');
 {$ENDIF}

 if VCHIQState.remote = nil then
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: VCHIQState.remote = nil');
  end
 else if VCHIQState.remote.initialised <> 1 then
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: VCHIQState.remote.initialised <> 1');
  end;
  
 if (VCHIQState.remote <> nil) and (VCHIQState.remote.initialised = 1) then
  begin
   Result:=VCHIQState;
  end;
end;

{==============================================================================}

function VCHIQUseInternal(State:PVCHIQ_STATE_T;Service:PVCHIQ_SERVICE_T;UseType:VC_USE_TYPE_T):VCHIQ_STATUS_T;
{From vchiq_use_internal in vchiq_arm.c}
var
 Entity:String;
 LocalUseCount:Integer;
 EntityUseCount:PInteger;
 LocalEntityUseCount:Integer;
 
 AckCount:LongInt;
 Status:VCHIQ_STATUS_T;
 PlatformState:PVCHIQ_PLATFORM_STATE_T;
begin
 {}
 Result:=VCHIQ_ERROR;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Use Internal (State=' + IntToHex(PtrUInt(State),8) + ' Service=' + IntToHex(PtrUInt(Service),8) + ')');
 {$ENDIF}
 
 {Check State} 
 if State = nil then Exit;

 {Get Platform State}
 PlatformState:=State.platform_state;
 if PlatformState = nil then Exit;
 
 {Check Use Type}
 if UseType = USE_TYPE_VCHIQ then
  begin
   Entity:='VCHIQ:';
   EntityUseCount:=@PlatformState.peer_use_count;
  end
 else if Service <> nil then
  begin
   Entity:=VCHIQFourccToString(Service.base.fourcc) + ':' + IntToStr(Service.client_id);
   EntityUseCount:=@Service.service_use_count;
  end
 else 
  begin 
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Service = nil');
   
   Exit;
  end;
 
 {Lock Writer}
 SynchronizerWriterLock(PlatformState.susp_res_lock);
 
 while PlatformState.resume_blocked <> 0 do
  begin
   {If we call 'use' while force suspend is waiting for suspend, then we're about to block the thread which the force is waiting to complete, so
    we're bound to just time out. In this  case, set the suspend state such that the wait will be canceled, so we can complete as quickly as possible}
   if (PlatformState.resume_blocked <> 0) and (PlatformState.vc_suspend_state = VC_SUSPEND_IDLE) then
    begin
     {Set Suspend State}
     VCHIQSetSuspendState(PlatformState,VC_SUSPEND_FORCE_CANCELED);
     
     Break;
    end;
   
   {If suspend is already in progress then we need to block}
   if CompletionTryWait(PlatformState.resume_blocker) = ERROR_NOT_READY then
    begin
     {Indicate that there are threads waiting on the resume blocker. These need to be allowed to complete before
      a _second_ call to force suspend can complete, otherwise low priority threads might never actually continue}
     Inc(PlatformState.blocked_count);
     
     {Unlock Writer}
     SynchronizerWriterUnlock(PlatformState.susp_res_lock);
     
     {$IFDEF VC4VCHIQ_DEBUG}
     if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Resume Blocked Waiting (Entity=' + Entity + ')');
     {$ENDIF}

     if CompletionWait(PlatformState.resume_blocker) <> ERROR_SUCCESS then
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Wait for resume blocker failed');
      
       {Lock Writer}
       SynchronizerWriterLock(PlatformState.susp_res_lock);
       
       {Update Blocked Count}
       Dec(PlatformState.blocked_count);
       
       {Unlock Writer}
       SynchronizerWriterUnlock(PlatformState.susp_res_lock);
    
       Exit;
      end;
      
     {$IFDEF VC4VCHIQ_DEBUG}
     if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Resume Unblocked (Entity=' + Entity + ')');
     {$ENDIF}
      
     {Lock Writer}
     SynchronizerWriterLock(PlatformState.susp_res_lock);
      
     {Update Blocked Count}
     Dec(PlatformState.blocked_count);
     if PlatformState.blocked_count = 0 then
      begin
       CompletionCompleteAll(PlatformState.blocked_blocker);
      end;
    end;
  end;
  
 {Stop Suspend Timer}
 VCHIQStopSuspendTimer(PlatformState);
 
 {Update Use Count}
 Inc(PlatformState.videocore_use_count);
 LocalUseCount:=PlatformState.videocore_use_count;
 Inc(EntityUseCount^);
 LocalEntityUseCount:=EntityUseCount^;
 
 {If there's a pending request which hasn't yet been serviced then just clear it. If we're past VC_SUSPEND_REQUESTED
  state then vc_resume_complete will block until we either resume or fail to suspend}
 if PlatformState.vc_suspend_state <= VC_SUSPEND_REQUESTED then
  begin
   {Set Suspend State}
   VCHIQSetSuspendState(PlatformState,VC_SUSPEND_IDLE);
  end;
  
 {Check Use Type}
 if (UseType <> USE_TYPE_SERVICE_NO_RESUME) and VCHIQNeedResume(State) then
  begin
   {Set Resume State}
   VCHIQSetResumeState(PlatformState,VC_RESUME_REQUESTED);

   {$IFDEF VC4VCHIQ_DEBUG}
   if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Use Internal (Entity=' + Entity + ' EntityUseCount=' + IntToStr(LocalEntityUseCount) + ' UseCount=' + IntToStr(LocalUseCount) + ')');
   {$ENDIF}
   
   {Request Poll}
   VCHIQRequestPoll(State,nil,VCHIQ_POLL_TERMINATE); {Note: PollType not used if Service is nil}
  end
 else
  begin
   {$IFDEF VC4VCHIQ_DEBUG}
   if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Use Internal (Entity=' + Entity + ' EntityUseCount=' + IntToStr(EntityUseCount^) + ' UseCount=' + IntToStr(LocalUseCount) + ')');
   {$ENDIF}
  end;
 
 {Unlock Writer}
 SynchronizerWriterUnlock(PlatformState.susp_res_lock);
  
 {Completion is in a done state when we're not suspended, so this won't block for the non-suspended case}
 if CompletionTryWait(PlatformState.vc_resume_complete) = ERROR_NOT_READY then
  begin
   {$IFDEF VC4VCHIQ_DEBUG}
   if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Wait for Resume (Entity=' + Entity + ')');
   {$ENDIF}
  
   if CompletionWait(PlatformState.vc_resume_complete) <> ERROR_SUCCESS then
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Wait for resume complete failed');
     
     Exit;
    end;
    
   {$IFDEF VC4VCHIQ_DEBUG}
   if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Resumed (Entity=' + Entity + ')');
   {$ENDIF}
  end;
 
 {Get Ack Count}
 Status:=VCHIQ_SUCCESS;
 AckCount:=InterlockedExchange(PlatformState.ka_use_ack_count,0);
 while (AckCount <> 0) and (Status = VCHIQ_SUCCESS) do
  begin
   {Send the use notify to videocore}
   Status:=VCHIQSendRemoteUseActive(State);
   if Status = VCHIQ_SUCCESS then
    begin
     {Update Ack Count}
     Dec(AckCount);
    end
   else
    begin
     {Restore Ack Count}
     InterlockedAddExchange(PlatformState.ka_use_ack_count,AckCount);
    end;    
  end;
 
 {Return Success}
 Result:=VCHIQ_SUCCESS;
end;

{==============================================================================}

function VCHIQReleaseInternal(State:PVCHIQ_STATE_T;Service:PVCHIQ_SERVICE_T):VCHIQ_STATUS_T;
{From vchiq_release_internal in vchiq_arm.c}
var
 Entity:String;
 {LocalUseCount:Integer;} {Not Used}
 EntityUseCount:PInteger;
 {LocalEntityUseCount:Integer;} {Not Used}
 
 PlatformState:PVCHIQ_PLATFORM_STATE_T;
begin
 {}
 Result:=VCHIQ_ERROR;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Release Internal (State=' + IntToHex(PtrUInt(State),8) + ' Service=' + IntToHex(PtrUInt(Service),8) + ')');
 {$ENDIF}
 
 {Check State} 
 if State = nil then Exit;

 {Get Platform State}
 PlatformState:=State.platform_state;
 if PlatformState = nil then Exit;
 
 {Check Service} 
 if Service <> nil then
  begin
   Entity:=VCHIQFourccToString(Service.base.fourcc) + ':' + IntToStr(Service.client_id);
   EntityUseCount:=@Service.service_use_count;
  end
 else
  begin 
   Entity:='PEER:';
   EntityUseCount:=@PlatformState.peer_use_count;
  end;
 
 {Lock Writer}
 SynchronizerWriterLock(PlatformState.susp_res_lock);
 try
  {Check Use Count}
  if (PlatformState.videocore_use_count = 0) or (EntityUseCount^ = 0) then
   begin
    if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: (PlatformState.videocore_use_count = 0) or (EntityUseCount^ = 0)');
    
    Result:=VCHIQ_ERROR;
    Exit;
   end;
  
  {Update Use Count}
  Dec(PlatformState.videocore_use_count);
  {LocalUseCount:=PlatformState.videocore_use_count;} {Not Used}
  Dec(EntityUseCount^);
  {LocalEntityUseCount:=EntityUseCount^;} {Not Used}
  
  {Check VideoCore Wanted}
  if not VCHIQVideocoreWanted(State) then
   begin
    {Check Suspend Timer and Resume Blocked}
    if (VCHIQPlatformUseSuspendTimer <> 0) and (PlatformState.resume_blocked = 0) then
     begin
      {Only use the timer if we're not trying to force suspend (=> resume_blocked)}
      VCHIQStartSuspendTimer(PlatformState);
     end
    else
     begin
      {$IFDEF VC4VCHIQ_DEBUG}
      if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Release Internal - Suspending (Entity=' + Entity + ' EntityUseCount=' + IntToStr(EntityUseCount^) + ' VideocoreUseCount=' + IntToStr(PlatformState.videocore_use_count) + ')');
      {$ENDIF}

      {ARM VC Suspend}
      VCHIQARMVCSuspend(State);
     end;     
   end
  else
   begin
    {$IFDEF VC4VCHIQ_DEBUG}
    if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Release Internal (Entity=' + Entity + ' EntityUseCount=' + IntToStr(EntityUseCount^) + ' VideocoreUseCount=' + IntToStr(PlatformState.videocore_use_count) + ')');
    {$ENDIF}
   end;   

  {Return Success}
  Result:=VCHIQ_SUCCESS;
 finally 
  {Unlock Writer}
  SynchronizerWriterUnlock(PlatformState.susp_res_lock);
 end; 
end;

{==============================================================================}

procedure VCHIQOnRemoteUse(State:PVCHIQ_STATE_T);
{From vchiq_on_remote_use in vchiq_arm.c}
var
 PlatformState:PVCHIQ_PLATFORM_STATE_T;
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: On Remote Use (State=' + IntToHex(PtrUInt(State),8) + ')');
 {$ENDIF}

 {Check State} 
 if State = nil then Exit;
 
 {Get Platform State}
 PlatformState:=State.platform_state;
 
 {Update Use Count}
 InterlockedIncrement(PlatformState.ka_use_count);
 
 {Signal Completion}
 CompletionComplete(PlatformState.ka_evt);
end;
 
{==============================================================================}

procedure VCHIQOnRemoteRelease(State:PVCHIQ_STATE_T);
{From vchiq_on_remote_release in vchiq_arm.c}
var
 PlatformState:PVCHIQ_PLATFORM_STATE_T;
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: On Remote Release (State=' + IntToHex(PtrUInt(State),8) + ')');
 {$ENDIF}

 {Check State} 
 if State = nil then Exit;
 
 {Get Platform State}
 PlatformState:=State.platform_state;
 
 {Update Release Count}
 InterlockedIncrement(PlatformState.ka_release_count);
 
 {Signal Completion}
 CompletionComplete(PlatformState.ka_evt);
end;
 
{==============================================================================}

function VCHIQUseServiceInternal(Service:PVCHIQ_SERVICE_T):VCHIQ_STATUS_T;
{From vchiq_use_service_internal in vchiq_arm.c}
begin
 {}
 Result:=VCHIQ_ERROR;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Use Service Internal (Service=' + IntToHex(PtrUInt(Service),8) + ')');
 {$ENDIF}
 
 {Check Service} 
 if Service = nil then Exit;
 
 {Use Internal}
 Result:=VCHIQUseInternal(Service.state,Service,USE_TYPE_SERVICE);
end;

{==============================================================================}

function VCHIQReleaseServiceInternal(Service:PVCHIQ_SERVICE_T):VCHIQ_STATUS_T;
{From vchiq_release_service_internal in vchiq_arm.c}
begin
 {}
 Result:=VCHIQ_ERROR;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Release Service Internal (Service=' + IntToHex(PtrUInt(Service),8) + ')');
 {$ENDIF}
 
 {Check Service} 
 if Service = nil then Exit;
 
 {Release Internal}
 Result:=VCHIQReleaseInternal(Service.state,Service);
end;

{==============================================================================}

function VCHIQInstanceGetUseCount(Instance:PVCHIQInstance):Integer;
{From vchiq_instance_get_use_count in vchiq_arm.c}
var
 Index:Integer;
 UseCount:Integer;
 Service:PVCHIQ_SERVICE_T;
begin
 {}
 Result:=0;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Instance Get Use Count (Instance=' + IntToHex(PtrUInt(Instance),8) + ')');
 {$ENDIF}

 {Check Instance} 
 if Instance = nil then Exit;
 
 {Set Defaults}
 Index:=0;
 UseCount:=0;
 
 {Get Service}
 Service:=VCHIQNextServiceByInstance(Instance.State,Instance,Index);
 while Service <> nil do
  begin
   {Update Use Count}
   Inc(UseCount,Service.service_use_count);
   
   {Unlock Service}
   VCHIQUnlockService(Service);
   
   {Next Service}
   Service:=VCHIQNextServiceByInstance(Instance.State,Instance,Index);
  end;
 
 {Return Use Count}
 Result:=UseCount;
end;

{==============================================================================}

function VCHIQInstanceGetPID(Instance:PVCHIQInstance):Integer;
{From vchiq_instance_get_pid in vchiq_arm.c}
begin
 {}
 Result:=0;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Instance Get PID (Instance=' + IntToHex(PtrUInt(Instance),8) + ')');
 {$ENDIF}

 {Check Instance} 
 if Instance = nil then Exit;
 
 {Return PID}
 Result:=Instance.PID;
end;

{==============================================================================}

function VCHIQInstanceGetTrace(Instance:PVCHIQInstance):Boolean;
{From vchiq_instance_get_trace in vchiq_arm.c}
begin
 {}
 Result:=False;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Instance Get Trace (Instance=' + IntToHex(PtrUInt(Instance),8) + ')');
 {$ENDIF}

 {Check Instance} 
 if Instance = nil then Exit;
 
 {Return Trace}
 Result:=Instance.Trace;
end;

{==============================================================================}

procedure VCHIQInstanceSetTrace(Instance:PVCHIQInstance;Trace:Integer);
{From vchiq_instance_set_trace in vchiq_arm.c}
var
 Index:Integer;
 Service:PVCHIQ_SERVICE_T;
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Instance Set Trace (Instance=' + IntToHex(PtrUInt(Instance),8) + ')');
 {$ENDIF}

 {Check Instance} 
 if Instance = nil then Exit;
 
 {Set Defaults}
 Index:=0;
 
 {Get Service}
 Service:=VCHIQNextServiceByInstance(Instance.State,Instance,Index);
 while Service <> nil do
  begin
   {Set Trace}
   Service.trace:=Trace;
   
   {Unlock Service}
   VCHIQUnlockService(Service);
   
   {Next Service}
   Service:=VCHIQNextServiceByInstance(Instance.State,Instance,Index);
  end;

 {Set Trace}  
 Instance.Trace:=(Trace <> 0);
end;

{==============================================================================}

procedure VCHIQDumpPhysicalMemory(VirtAddr:Pointer;NumBytes:LongWord);
{From dump_phys_mem in vchiq_arm.c}
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Dump Physical Memory (VirtAddr=' + IntToHex(PtrUInt(VirtAddr),8) + ' NumBytes=' + IntToStr(NumBytes) + ')');
 {$ENDIF}
 
 //To Do //Continuing
 if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: VCHIQDumpPhysicalMemory'); //To Do //Temp
 
end;

{==============================================================================}

procedure VCHIQOnRemoteUseActive(State:PVCHIQ_STATE_T);
{From vchiq_on_remote_use_active in vchiq_arm.c}
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: On Remote Use Active (State=' + IntToHex(PtrUInt(State),8) + ')');
 {$ENDIF}
 
 {Check State} 
 if State = nil then Exit;
 
 {Nothing}
end;

{==============================================================================}

procedure VCHIQPlatformConnStateChanged(State:PVCHIQ_STATE_T;OldState,NewState:VCHIQ_CONNSTATE_T);
{From vchiq_platform_conn_state_changed in vchiq_arm.c}
var
 PlatformState:PVCHIQ_PLATFORM_STATE_T;
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Platform Conn State Changed (State=' + IntToHex(PtrUInt(State),8) + ' OldState=' + VCHIQConnStateToString(OldState) + ' NewState=' + VCHIQConnStateToString(NewState) + ')');
 {$ENDIF}
 
 {Check State} 
 if State = nil then Exit;
 
 {Check Connection State}
 if State.conn_state = VCHIQ_CONNSTATE_CONNECTED then
  begin
   {Get Platform State}
   PlatformState:=State.platform_state;
   if PlatformState = nil then Exit;
   
   {Lock Writer}
   if SynchronizerWriterLock(PlatformState.susp_res_lock) <> ERROR_SUCCESS then Exit;
   
   {Check First Connect}
   if not PlatformState.first_connect then
    begin
     {Set First Connect}
     PlatformState.first_connect:=True;
     
     {Unlock Writer}
     SynchronizerWriterUnlock(PlatformState.susp_res_lock);

     {Create Keepalive Thread}
     PlatformState.ka_thread:=BeginThread(TThreadStart(VCHIQKeepaliveExecute),State,PlatformState.ka_thread,VCHIQ_KEEPALIVE_THREAD_STACK_SIZE);
     if PlatformState.ka_thread = INVALID_HANDLE_VALUE then 
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Conn State: Failed to create keepalive thread');
       Exit;
      end;
     ThreadSetPriority(PlatformState.ka_thread,VCHIQ_KEEPALIVE_THREAD_PRIORITY);
     ThreadSetName(PlatformState.ka_thread,VCHIQ_KEEPALIVE_THREAD_NAME);
    end
   else
    begin
     {Unlock Writer}
     SynchronizerWriterUnlock(PlatformState.susp_res_lock);
    end;    
  end;
end;

{==============================================================================}

procedure VCHIQRemoteEventSignal(Event:PREMOTE_EVENT_T);
{From remote_event_signal in vchiq_2835_arm.c}
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Remote Event Signal (Event=' + IntToHex(PtrUInt(Event),8) + ')');
 {$ENDIF}
 
 {Check Event}
 if Event = nil then Exit;
 
 {Memory Barrier}
 DataMemoryBarrier;
 
 {Set Fired}
 Event.fired:=1;
 
 {Synchronization Barrier}
 DataSynchronizationBarrier;

 {Check Armed} 
 if Event.armed <> 0 then
  begin
   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}
   
   {Trigger VC Interrupt}
   PLongWord(PERIPHERALS_BASE + VCHIQ_DOORBELL_REGS_BASE + VCHIQ_DOORBELL_BELL2)^:=0;
  end;
end;

{==============================================================================}

procedure VCHIQCheckSuspend(State:PVCHIQ_STATE_T);
{From vchiq_check_suspend in vchiq_arm.c}
var
 PlatformState:PVCHIQ_PLATFORM_STATE_T;
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Check Suspend (State=' + IntToHex(PtrUInt(State),8) + ')');
 {$ENDIF}
 
 {Check State} 
 if State = nil then Exit;
 
 {Get Platform State}
 PlatformState:=State.platform_state;
 if PlatformState = nil then Exit;
 
 {Lock Writer}
 if SynchronizerWriterLock(PlatformState.susp_res_lock) <> ERROR_SUCCESS then Exit;
 
 {Check State}
 if (PlatformState.vc_suspend_state <> VC_SUSPEND_SUSPENDED) and PlatformState.first_connect and not(VCHIQVideocoreWanted(State)) then
  begin
   {ARM VC Suspend}
   VCHIQARMVCSuspend(State);
  end;
 
 {Unlock Writer}
 SynchronizerWriterUnlock(PlatformState.susp_res_lock);
end;

{==============================================================================}

procedure VCHIQPlatformCheckSuspend(State:PVCHIQ_STATE_T);
{From vchiq_platform_check_suspend in vchiq_arm.c}
var
 Suspend:Boolean;
 PlatformState:PVCHIQ_PLATFORM_STATE_T;
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Platform Check Suspend (State=' + IntToHex(PtrUInt(State),8) + ')');
 {$ENDIF}
 
 {Check State} 
 if State = nil then Exit;
 
 {Get Platform State}
 PlatformState:=State.platform_state;
 if PlatformState = nil then Exit;

 {Set Defaults}
 Suspend:=False;
 
 {Lock Writer}
 if SynchronizerWriterLock(PlatformState.susp_res_lock) <> ERROR_SUCCESS then Exit;
 
 if (PlatformState.vc_suspend_state = VC_SUSPEND_REQUESTED) and (PlatformState.vc_resume_state = VC_RESUME_RESUMED) then
  begin
   VCHIQSetSuspendState(PlatformState,VC_SUSPEND_IN_PROGRESS);
   Suspend:=True;
  end;
 
 {Unlock Writer}
 SynchronizerWriterUnlock(PlatformState.susp_res_lock);
 
 {Check Suspend}
 if Suspend then
  begin
   {Platform Suspend}
   VCHIQPlatformSuspend(State);
  end;
end;

{==============================================================================}

function VCHIQPlatformSuspend(State:PVCHIQ_STATE_T):VCHIQ_STATUS_T;
{From vchiq_platform_suspend in vchiq_2835_arm.c}
begin
 {}
 Result:=VCHIQ_ERROR;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Platform Suspend (State=' + IntToHex(PtrUInt(State),8) + ')');
 {$ENDIF}
end;

{==============================================================================}

function VCHIQPlatformResume(State:PVCHIQ_STATE_T):VCHIQ_STATUS_T;
{From vchiq_platform_resume in vchiq_2835_arm.c}
begin
 {}
 Result:=VCHIQ_SUCCESS;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Platform Resume (State=' + IntToHex(PtrUInt(State),8) + ')');
 {$ENDIF}
end;

{==============================================================================}

procedure VCHIQPlatformPaused(State:PVCHIQ_STATE_T);
{From vchiq_platform_paused in vchiq_2835_arm.c}
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Platform Paused (State=' + IntToHex(PtrUInt(State),8) + ')');
 {$ENDIF}
end;

{==============================================================================}

procedure VCHIQPlatformResumed(State:PVCHIQ_STATE_T);
{From vchiq_platform_resumed in vchiq_2835_arm.c}
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Platform Resumed (State=' + IntToHex(PtrUInt(State),8) + ')');
 {$ENDIF}
end;

{==============================================================================}

function VCHIQVideocoreWanted(State:PVCHIQ_STATE_T):Boolean;
{From vchiq_videocore_wanted in vchiq_arm.c}
var
 PlatformState:PVCHIQ_PLATFORM_STATE_T;
begin
 {}
 Result:=True;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Videocore Wanted (State=' + IntToHex(PtrUInt(State),8) + ')');
 {$ENDIF}
 
 {Check State} 
 if State = nil then Exit;
 
 {Get Platform State}
 PlatformState:=State.platform_state;
 
 {Check Platform State}
 if PlatformState = nil then
  begin
   {Autosuspend not supported (Always return wanted)}
   Result:=True;
  end
 else if PlatformState.blocked_count <> 0 then
  begin
   Result:=True;
  end
 else if PlatformState.videocore_use_count = 0 then
  begin
   {Usage count zero (Check for override unless forcing)}
   if PlatformState.resume_blocked <> 0 then
    begin
     Result:=False;
    end
   else 
    begin
     Result:=VCHIQPlatformVideocoreWanted(State);
    end;    
  end
 else
  begin
   {Non-zero usage count (Videocore still required)}
   Result:=True;
  end;  
end;

{==============================================================================}

function VCHIQPlatformVideocoreWanted(State:PVCHIQ_STATE_T):Boolean;
{From vchiq_platform_videocore_wanted in vchiq_2835_arm.c}
begin
 {}
 Result:=True; {Autosuspend not supported (Videocore always wanted)}
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Platform Videocore Wanted (State=' + IntToHex(PtrUInt(State),8) + ')');
 {$ENDIF}
end;

{==============================================================================}

function VCHIQPlatformUseSuspendTimer:Integer;
{From vchiq_platform_use_suspend_timer in vchiq_2835_arm.c}
begin
 {}
 Result:=0;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Platform Use Suspend Timer');
 {$ENDIF}
end;

{==============================================================================}

procedure VCHIQDumpPlatformUseState(State:PVCHIQ_STATE_T);
{From vchiq_dump_platform_use_state in vchiq_2835_arm.c}
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Dump Platform Use State (State=' + IntToHex(PtrUInt(State),8) + ')');
 {$ENDIF}
end;

{==============================================================================}

procedure VCHIQPlatformHandleTimeout(State:PVCHIQ_STATE_T);
{From vchiq_platform_handle_timeout in vchiq_2835_arm.c}
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Platform Handle Timeout (State=' + IntToHex(PtrUInt(State),8) + ')');
 {$ENDIF}
end;

{==============================================================================}

procedure VCHIQSetSuspendState(PlatformState:PVCHIQ_PLATFORM_STATE_T;NewState:VC_SUSPEND_STATUS_T);
{From set_suspend_state in vchiq_arm.c}
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Set Suspend State (PlatformState=' + IntToHex(PtrUInt(PlatformState),8) + ' NewState=' + VCHIQSuspendStatusToString(NewState) + ')');
 {$ENDIF}
 
 {Check Platform State} 
 if PlatformState = nil then Exit;
 
 {Set the state in all cases}
 PlatformState.vc_suspend_state:=NewState;
 
 {State specific additional actions}
 case NewState of
  VC_SUSPEND_FORCE_CANCELED:begin
    CompletionCompleteAll(PlatformState.vc_suspend_complete);
   end;
  VC_SUSPEND_REJECTED:begin
    CompletionCompleteAll(PlatformState.vc_suspend_complete);
   end;
  VC_SUSPEND_FAILED:begin
    CompletionCompleteAll(PlatformState.vc_suspend_complete);
    
    PlatformState.vc_resume_state:=VC_RESUME_RESUMED;
    
    CompletionCompleteAll(PlatformState.vc_resume_complete);
   end;
  VC_SUSPEND_IDLE:begin
    CompletionReset(PlatformState.vc_suspend_complete);
   end;
  VC_SUSPEND_REQUESTED:begin
    {Nothing}
   end;
  VC_SUSPEND_IN_PROGRESS:begin
    VCHIQSetResumeState(PlatformState,VC_RESUME_IDLE);
   end;
  VC_SUSPEND_SUSPENDED:begin
    CompletionCompleteAll(PlatformState.vc_suspend_complete);
   end;
 end;
end;

{==============================================================================}

procedure VCHIQSetResumeState(PlatformState:PVCHIQ_PLATFORM_STATE_T;NewState:VC_RESUME_STATUS_T);
{From set_resume_state in vchiq_arm.c}
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Set Resume State (PlatformState=' + IntToHex(PtrUInt(PlatformState),8) + ' NewState=' + VCHIQResumeStatusToString(NewState) + ')');
 {$ENDIF}
 
 {Check Platform State} 
 if PlatformState = nil then Exit;
 
 {Set the state in all cases}
 PlatformState.vc_resume_state:=NewState;
 
 {State specific additional actions}
 case NewState of
  VC_RESUME_FAILED:begin
    {Nothing}
   end;
  VC_RESUME_IDLE:begin
    CompletionReset(PlatformState.vc_resume_complete);
   end;
  VC_RESUME_REQUESTED:begin
    {Nothing}
   end;
  VC_RESUME_IN_PROGRESS:begin
    {Nothing}
   end;
  VC_RESUME_RESUMED:begin
    CompletionCompleteAll(PlatformState.vc_resume_complete);
    
    VCHIQSetSuspendState(PlatformState,VC_SUSPEND_IDLE);
   end;
 end;
end;

{==============================================================================}

procedure VCHIQStartSuspendTimer(PlatformState:PVCHIQ_PLATFORM_STATE_T);
{From start_suspend_timer in vchiq_arm.c}

{Should be called with the write lock held}
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Start Suspend Timer (PlatformState=' + IntToHex(PtrUInt(PlatformState),8) + ')');
 {$ENDIF}
 
 {Check PlatformState} 
 if PlatformState = nil then Exit;
 
 {Disable Timer}
 TimerDisable(PlatformState.suspend_timer);
 
 {Enable Timer}
 TimerEnable(PlatformState.suspend_timer);
 
 {Update Running}
 PlatformState.suspend_timer_running:=True;
end;

{==============================================================================}

procedure VCHIQStopSuspendTimer(PlatformState:PVCHIQ_PLATFORM_STATE_T);
{From stop_suspend_timer in vchiq_arm.c}

{Should be called with the write lock held}
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Stop Suspend Timer (PlatformState=' + IntToHex(PtrUInt(PlatformState),8) + ')');
 {$ENDIF}
 
 {Check PlatformState} 
 if PlatformState = nil then Exit;
 
 {Check Running}
 if PlatformState.suspend_timer_running then
  begin
   {Disable Timer}
   TimerDisable(PlatformState.suspend_timer);
  
   {Update Running}
   PlatformState.suspend_timer_running:=False;
  end;
end;

{==============================================================================}

function VCHIQNeedResume(State:PVCHIQ_STATE_T):Boolean;
{From need_resume in vchiq_arm.c}
var
 PlatformState:PVCHIQ_PLATFORM_STATE_T;
begin
 {}
 Result:=False;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Need Resume (State=' + IntToHex(PtrUInt(State),8) + ')');
 {$ENDIF}

 {Check State}
 if State = nil then Exit;
 
 {Get Platform State}
 PlatformState:=State.platform_state;
 if PlatformState = nil then Exit;
 
 {Get Result}
 Result:=(PlatformState.vc_suspend_state > VC_SUSPEND_IDLE) and (PlatformState.vc_resume_state < VC_RESUME_REQUESTED) and VCHIQVideocoreWanted(State);
end;

{==============================================================================}

function VCHIQBlockResume(PlatformState:PVCHIQ_PLATFORM_STATE_T):VCHIQ_STATUS_T;
{From block_resume in vchiq_arm.c}
var
 ResumeCount:Integer;
begin
 {}
 Result:=VCHIQ_ERROR;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Block Resume (PlatformState=' + IntToHex(PtrUInt(PlatformState),8) + ')');
 {$ENDIF}

 {Check Platform State}
 if PlatformState = nil then Exit;
 
 {Allow any threads which were blocked by the last force suspend to complete if they haven't already.  Only give this one shot; if  blocked_count is
  incremented after blocked_blocker is completed (which only happens when blocked_count hits 0) then those threads will have to wait until next time around}
 if PlatformState.blocked_count <> 0 then
  begin
   {Reset Completion}
   CompletionReset(PlatformState.blocked_blocker);
   
   {Unlock Writer}
   SynchronizerWriterUnlock(PlatformState.susp_res_lock);
   
   {$IFDEF VC4VCHIQ_DEBUG}
   if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Block Resume: Wait for previously blocked clients (BlockedCount=' + IntToStr(PlatformState.blocked_count) + ')');
   {$ENDIF}
   
   {Wait for Completion}
   if CompletionWait(PlatformState.blocked_blocker,VCHIQ_FORCE_SUSPEND_TIMEOUT_MS) <> ERROR_SUCCESS then
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Block Resume: Wait for previously blocked clients failed');
    
     {Lock Writer}
     SynchronizerWriterLock(PlatformState.susp_res_lock);
     
     Result:=VCHIQ_ERROR;
     Exit;
    end;
    
   {$IFDEF VC4VCHIQ_DEBUG}
   if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Block Resume: Previously blocked clients resumed');
   {$ENDIF}
   
   {Lock Writer}
   SynchronizerWriterLock(PlatformState.susp_res_lock);
  end;

 {We need to wait for resume to complete if it's in process}
 ResumeCount:=0;
 while (PlatformState.vc_resume_state <> VC_RESUME_RESUMED) and (PlatformState.vc_resume_state > VC_RESUME_IDLE) do
  begin
   {Check Resume Count}
   if ResumeCount > 1 then
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Block Resume: Waited too many times for resume');
     
     Result:=VCHIQ_ERROR;
     Exit;
    end;
   
   {Unlock Writer}
   SynchronizerWriterUnlock(PlatformState.susp_res_lock);
   
   {$IFDEF VC4VCHIQ_DEBUG}
   if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Block Resume: Wait for resume');
   {$ENDIF}
   
   {Wait for Completion}
   if CompletionWait(PlatformState.vc_resume_complete,VCHIQ_FORCE_SUSPEND_TIMEOUT_MS) <> ERROR_SUCCESS then
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Block Resume: Wait for resume failed (ResumeState=' + VCHIQResumeStatusToString(PlatformState.vc_resume_state) + ')');
    
     {Lock Writer}
     SynchronizerWriterLock(PlatformState.susp_res_lock);
     
     Result:=VCHIQ_ERROR;
     Exit;
    end;
   
   {$IFDEF VC4VCHIQ_DEBUG}
   if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Block Resume: Resumed');
   {$ENDIF}
   
   {Lock Writer}
   SynchronizerWriterLock(PlatformState.susp_res_lock);
   
   {Update Resume Count}
   Inc(ResumeCount);
  end;
  
 {Reset Completion}
 CompletionReset(PlatformState.resume_blocker);
 
 {Update Resume Blocked}
 PlatformState.resume_blocked:=1;
 
 {Return Success}
 Result:=VCHIQ_SUCCESS;
end;

{==============================================================================}

procedure VCHIQUnlockResume(PlatformState:PVCHIQ_PLATFORM_STATE_T);
{From unblock_resume in vchiq_arm.c}
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Unblock Resume (PlatformState=' + IntToHex(PtrUInt(PlatformState),8) + ')');
 {$ENDIF}
 
 {Check Platform State}
 if PlatformState = nil then Exit;
 
 CompletionCompleteAll(PlatformState.resume_blocker);
 PlatformState.resume_blocked:=0;
end;

{==============================================================================}

function VCHIQARMVCSuspend(State:PVCHIQ_STATE_T):VCHIQ_STATUS_T;
{From vchiq_arm_vcsuspend in vchiq_arm.c}

{Initiate suspend via slot handler. Should be called with the write lock held}
var
 PlatformState:PVCHIQ_PLATFORM_STATE_T;
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: ARM VC Suspend (State=' + IntToHex(PtrUInt(State),8) + ')');
 {$ENDIF}
 
 {Check State}
 if State = nil then Exit;
 
 {Get Platform State}
 PlatformState:=State.platform_state;
 if PlatformState = nil then Exit;
 
 {Check Suspend State}
 case PlatformState.vc_suspend_state of
  VC_SUSPEND_REQUESTED:begin
    if DEVICE_LOG_ENABLED then DeviceLogInfo(nil,'VCHIQ: Suspend already requested');
   end;
  VC_SUSPEND_IN_PROGRESS:begin
    if DEVICE_LOG_ENABLED then DeviceLogInfo(nil,'VCHIQ: Suspend already in progress');
   end;
  VC_SUSPEND_REJECTED,
  VC_SUSPEND_FAILED:begin
    {Ensure any idle state actions have been run}
    VCHIQSetSuspendState(PlatformState,VC_SUSPEND_IDLE);
  
    if DEVICE_LOG_ENABLED then DeviceLogInfo(nil,'VCHIQ: Suspending');
    
    {Set Suspend State}
    VCHIQSetSuspendState(PlatformState,VC_SUSPEND_REQUESTED);
    
    {Ask the slot handler thread to initiate suspend}
    VCHIQRequestPoll(State,nil,VCHIQ_POLL_TERMINATE); {Note: PollType not used if Service is nil}
   end;
  VC_SUSPEND_IDLE:begin
    if DEVICE_LOG_ENABLED then DeviceLogInfo(nil,'VCHIQ: Suspending');
    
    {Set Suspend State}
    VCHIQSetSuspendState(PlatformState,VC_SUSPEND_REQUESTED);
    
    {Ask the slot handler thread to initiate suspend}
    VCHIQRequestPoll(State,nil,VCHIQ_POLL_TERMINATE); {Note: PollType not used if Service is nil}
   end;
  else
   begin
    {We don't expect to be in other states, so log but continue anyway}
    if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Unexpected suspend state (State=' + VCHIQSuspendStatusToString(PlatformState.vc_suspend_state) + ')');

    {Ensure any idle state actions have been run}
    VCHIQSetSuspendState(PlatformState,VC_SUSPEND_IDLE);
  
    if DEVICE_LOG_ENABLED then DeviceLogInfo(nil,'VCHIQ: Suspending');
    
    {Set Suspend State}
    VCHIQSetSuspendState(PlatformState,VC_SUSPEND_REQUESTED);
    
    {Ask the slot handler thread to initiate suspend}
    VCHIQRequestPoll(State,nil,VCHIQ_POLL_TERMINATE); {Note: PollType not used if Service is nil}
   end;   
 end;
 
 {Return Success}
 Result:=VCHIQ_SUCCESS
end;

{==============================================================================}

procedure VCHIQSuspendTimerCallback(State:PVCHIQ_STATE_T);
{From suspend_timer_callback in vchiq_arm.c}
var
 PlatformState:PVCHIQ_PLATFORM_STATE_T;
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Suspend Timer Callback (State=' + IntToHex(PtrUInt(State),8) + ')');
 {$ENDIF}
 
 {Check State} 
 if State = nil then Exit;
 
 {Get Platform State}
 PlatformState:=State.platform_state;
 if PlatformState = nil then Exit;
 
 {Check Suspend}
 VCHIQCheckSuspend(State);
end;

{==============================================================================}

function VCHIQCreatePages(Data:Pointer;Count:PtrUInt;PageType:Word;NumPages:LongWord;Pages:PVCHIQ_PAGELIST_PAGES):LongWord;
var
 PageCount:LongWord;
 BaseAddress:PtrUInt;
 PageTableEntry:TPageTableEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Create Pages (Data=' + IntToHex(PtrUInt(Data),8) + ' Count=' + IntToStr(Count) + ' PageType=' + VCHIQPageTypeToString(PageType) + ' NumPages=' + IntToStr(NumPages) + ' Pages=' + IntToHex(PtrUInt(Pages),8) + ')');
 {$ENDIF}

 {Check Data}
 if Data = nil then Exit;
 
 {Check Pages}
 if Pages = nil then Exit;
 
 {Get Base}
 BaseAddress:=PtrUInt(Data) and not(MEMORY_PAGE_SIZE - 1);
 
 {Get Pages}
 for PageCount:=0 to NumPages - 1 do
  begin
   {Get Page}
   PageTableGetEntry(BaseAddress,PageTableEntry);
  
   {Check Page}
   if PageTableEntry.Size <> MEMORY_PAGE_SIZE then
    begin
     {Update Page}
     PageTableEntry.Size:=MEMORY_PAGE_SIZE;
     
     {Set Page}
     PageTableSetEntry(PageTableEntry);
     
     {Get Page}
     PageTableGetEntry(BaseAddress,PageTableEntry);
    end;
    
   {Get Physical Address}
   Pages[PageCount]:=PageTableEntry.PhysicalAddress;
   
   {Update Base}
   Inc(BaseAddress,MEMORY_PAGE_SIZE);
  end;
 
 {Return Success}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function VCHIQCreatePagelist(Data:Pointer;Count:PtrUInt;PageType:Word;var Pagelist:PVCHIQ_PAGELIST_T):LongWord;
{From create_pagelist in vchiq_2835_arm.c}
var
 Offset:LongWord;
 NumPages:LongWord;
 PageCount:LongWord;
 Fragments:Pointer;
 PageAddress:Pointer;
 BaseAddress:Pointer;
 NextAddress:Pointer;
 RunLength:Integer;
 Addresses:PLongWord; {Use LongWord to ensure size as VC4 is 32-bit}
 AddressIndex:Integer;
 Pages:PVCHIQ_PAGELIST_PAGES;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Set Defaults}
 Pagelist:=nil;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Create Pagelist (Data=' + IntToHex(PtrUInt(Data),8) + ' Count=' + IntToStr(Count) + ' PageType=' + VCHIQPageTypeToString(PageType) + ')');
 {$ENDIF}
 
 {Check Data}
 if Data = nil then Exit;
 
 {Check VCHIQ}
 if VCHIQDevice = nil then Exit;
 
 {Get Offset}
 Offset:=PtrUInt(Data) and (MEMORY_PAGE_SIZE - 1);
 
 {Get Num Pages}
 NumPages:=(Count + Offset + MEMORY_PAGE_SIZE - 1) div MEMORY_PAGE_SIZE;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Create Pagelist (Offset=' + IntToStr(Offset) + ' NumPages=' + IntToStr(NumPages) + ')');
 {$ENDIF}
 
 {Allocate enough storage to hold the page pointers and the page list}
 Pagelist:=GetMem(SizeOf(VCHIQ_PAGELIST_T) + (NumPages * SizeOf(LongWord)) + (NumPages * SizeOf(PtrUInt)));
 if Pagelist = nil then 
  begin
   Result:=ERROR_NOT_ENOUGH_MEMORY;
   Exit;
  end;
 
 {Get Pages}
 Pages:=PVCHIQ_PAGELIST_PAGES(PtrUInt(Pagelist) + SizeOf(VCHIQ_PAGELIST_T) + (NumPages * SizeOf(LongWord)));

 {Create Pages}
 if VCHIQCreatePages(Data,Count,PageType,NumPages,Pages) <> ERROR_SUCCESS then
  begin
   {Free Pagelist}
   FreeMem(Pagelist);
   Pagelist:=nil;
  
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
 
 {Update Pagelist}
 Pagelist.length:=Count;
 Pagelist.pagetype:=PageType;
 Pagelist.offset:=Offset;
 
 {Get Addresses}
 Addresses:=@Pagelist.addrs[0];
 
 {Check PageType}
 if PageType = VCHIQ_PAGELIST_WRITE then
  begin
   {Clean Cache Range (Source)}
   CleanDataCacheRange(PtrUInt(Data),Count);
  end
 else
  begin
   {Clean Cache Range (Dest)}
   CleanDataCacheRange(PtrUInt(Data),Count);
  end;  
 
 {Group the pages into runs of contiguous physical pages}
 if VCHIQDevice.Use36BitAddress then
  begin
   BaseAddress:=Pointer(Pages[0]);
   NextAddress:=BaseAddress + MEMORY_PAGE_SIZE;
   RunLength:=0;
   AddressIndex:=0;
   
   {$IFDEF VC4VCHIQ_DEBUG}
   if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Create Pagelist (BaseAddress=' + IntToHex(PtrUInt(BaseAddress),8) + ' NextAddress=' + IntToHex(PtrUInt(NextAddress),8) + ')');
   {$ENDIF}
   
   for PageCount:=1 to NumPages - 1 do
    begin
     {Get Physical Address}
     PageAddress:=Pointer(Pages[PageCount]);
   
     {Check Address and Length (Must be less than 1MB)}
     if (PageAddress = NextAddress) and (RunLength < ($FF)) then
      begin
       {Update Next and Length}
       Inc(NextAddress,MEMORY_PAGE_SIZE);
       Inc(RunLength);
      end
     else
      begin
       {Update Page}
       Addresses[AddressIndex]:=LongWord((PtrUInt(BaseAddress) shr 4) + RunLength);
       Inc(AddressIndex);
       
       {Get Next Page}
       BaseAddress:=PageAddress;
       NextAddress:=BaseAddress + MEMORY_PAGE_SIZE;
       RunLength:=0;
      end;
    end;
  
   {Update Last Page} 
   Addresses[AddressIndex]:=LongWord((PtrUInt(BaseAddress) shr 4) + RunLength);
   {Inc(AddressIndex);} {Not Used}
  end
 else
  begin 
   BaseAddress:=Pointer(PhysicalToBusAddress(Pointer(Pages[0])));
   NextAddress:=BaseAddress + MEMORY_PAGE_SIZE;
   RunLength:=0;
   AddressIndex:=0;
 
   {$IFDEF VC4VCHIQ_DEBUG}
   if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Create Pagelist (BaseAddress=' + IntToHex(PtrUInt(BaseAddress),8) + ' NextAddress=' + IntToHex(PtrUInt(NextAddress),8) + ')');
   {$ENDIF}
 
   for PageCount:=1 to NumPages - 1 do
    begin
     {Get Physical Address}
     PageAddress:=Pointer(PhysicalToBusAddress(Pointer(Pages[PageCount])));
   
     {Check Address and Length (Must be less than 16MB)}
     if (PageAddress = NextAddress) and (RunLength < (MEMORY_PAGE_SIZE - 1)) then
      begin
       {Update Next and Length}
       Inc(NextAddress,MEMORY_PAGE_SIZE);
       Inc(RunLength);
      end
     else
      begin
       {Update Page}
       Addresses[AddressIndex]:=LongWord(BaseAddress + RunLength);
       Inc(AddressIndex);
       
       {Get Next Page}
       BaseAddress:=PageAddress;
       NextAddress:=BaseAddress + MEMORY_PAGE_SIZE;
       RunLength:=0;
      end;
    end;
  
   {Update Last Page} 
   Addresses[AddressIndex]:=LongWord(BaseAddress + RunLength);
   {Inc(AddressIndex);} {Not Used}
  end;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Create Pagelist (AddressIndex=' + IntToStr(AddressIndex) + ' RunLength=' + IntToStr(RunLength) + ')');
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Create Pagelist (BaseAddress=' + IntToHex(PtrUInt(BaseAddress),8) + ' NextAddress=' + IntToHex(PtrUInt(NextAddress),8) + ')');
 {$ENDIF}
 
 {Partial cache lines (fragments) require special measures}
 if (PageType = VCHIQ_PAGELIST_READ) and (((Pagelist.offset and (VCHIQDevice.CacheLineSize - 1)) <> 0) or (((Pagelist.offset + Pagelist.length) and (VCHIQDevice.CacheLineSize - 1)) <> 0)) then
  begin
   if DEVICE_LOG_ENABLED then DeviceLogWarn(nil,'VCHIQ: VCHIQCreatePagelist - Fragments'); //To Do //Testing, should now be resolved
   
   {Wait Free Fragments}
   if SemaphoreWait(VCHIQDevice.FragmentsSemaphore) <> ERROR_SUCCESS then
    begin
     {Free Pagelist}
     FreeMem(Pagelist);
     Pagelist:=nil;
     
     Result:=ERROR_OPERATION_FAILED;
     Exit;
    end;
    
   {Check Free Fragments}
   if VCHIQDevice.FreeFragments = nil then
    begin
     if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Create Pagelist: VCHIQDevice.FreeFragments = nil');
    end;
    
   {Lock Mutex}
   MutexLock(VCHIQDevice.FragmentsMutex);   
   
   {Get Fragments}
   Fragments:=VCHIQDevice.FreeFragments;
   
   {Update Free Fragments}
   VCHIQDevice.FreeFragments:=PPointer(VCHIQDevice.FreeFragments)^;
   
   {Unlock Mutex}
   MutexUnlock(VCHIQDevice.FragmentsMutex);   
   
   {$IFDEF VC4VCHIQ_DEBUG}
   if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Create Pagelist (Fragments=' + IntToHex(PtrUInt(Fragments),8) + ' FreeFragments=' + IntToHex(PtrUInt(VCHIQDevice.FreeFragments),8) + ')');
   {$ENDIF}
   
   {Update Pagelist}
   Pagelist.pagetype:=VCHIQ_PAGELIST_READ_WITH_FRAGMENTS + ((Fragments - VCHIQDevice.FragmentsBase) div VCHIQDevice.FragmentsSize);
  end;
 
 {Clean Cache Range (Pagelist)} 
 CleanDataCacheRange(PtrUInt(Pagelist),SizeOf(VCHIQ_PAGELIST_T) + (NumPages * SizeOf(LongWord)));
 
 {Return Success}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

procedure VCHIQFreePagelist(Pagelist:PVCHIQ_PAGELIST_T;Actual:Integer);
{From free_pagelist in vchiq_2835_arm.c}
var
 Bytes:LongWord;
 Count:LongWord;
 Offset:LongWord;
 NumPages:LongWord;
 PageCount:LongWord;
 Fragments:Pointer;
 HeadBytes:Integer;
 TailBytes:Integer;
 Pages:PVCHIQ_PAGELIST_PAGES;
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Free Pagelist (Pagelist=' + IntToHex(PtrUInt(Pagelist),8) + ' Actual=' + IntToStr(Actual) + ')');
 {$ENDIF}
 
 {Check Pagelist}
 if Pagelist = nil then Exit;
 
 {Check VCHIQ}
 if VCHIQDevice = nil then Exit;
 
 {Get Num Pages}
 NumPages:=(Pagelist.length + Pagelist.offset + MEMORY_PAGE_SIZE - 1) div MEMORY_PAGE_SIZE;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Free Pagelist (NumPages=' + IntToStr(NumPages) + ')');
 {$ENDIF}
 
 {Get Pages}
 Pages:=PVCHIQ_PAGELIST_PAGES(PtrUInt(Pagelist) + SizeOf(VCHIQ_PAGELIST_T) + (NumPages * SizeOf(LongWord)));
 
 {Check PageType}
 if Pagelist.pagetype <> VCHIQ_PAGELIST_WRITE then
  begin
   {Get Count and Offset}
   Count:=Pagelist.length;
   Offset:=Pagelist.offset;
   
   {Get Head and Tail Bytes}
   HeadBytes:=0;
   TailBytes:=0;
   
   {Check for Fragments}
   if Pagelist.pagetype >= VCHIQ_PAGELIST_READ_WITH_FRAGMENTS then
    begin
     {Get Head Bytes}
     HeadBytes:=(VCHIQDevice.CacheLineSize - Pagelist.offset) and (VCHIQDevice.CacheLineSize - 1);
     
     {Get Tail Bytes}
     TailBytes:=(Pagelist.offset + Actual) and (VCHIQDevice.CacheLineSize - 1);
    end;
    
   for PageCount:=0 to NumPages - 1 do
    begin
     {Get Bytes}
     Bytes:=MEMORY_PAGE_SIZE - (Offset + HeadBytes);
     if Bytes > Count then Bytes:=(Count - (HeadBytes + TailBytes));
     
     {Invalidate Cache Range (Dest)}
     InvalidateDataCacheRange(Pages[PageCount] + Offset + HeadBytes,Bytes);
     
     {Update Count and Offset}
     Dec(Count,Bytes + HeadBytes);
     Offset:=0;
     
     {Update Head Bytes}
     HeadBytes:=0;
    end;
  end; 
 
 {Deal with any partial cache lines (fragments)}
 if Pagelist.pagetype >= VCHIQ_PAGELIST_READ_WITH_FRAGMENTS then
  begin
   if DEVICE_LOG_ENABLED then DeviceLogWarn(nil,'VCHIQ: VCHIQFreePagelist - Fragments'); //To Do //Testing, should now be resolved
   
   {Get Fragments}
   Fragments:=VCHIQDevice.FragmentsBase + ((Pagelist.pagetype - VCHIQ_PAGELIST_READ_WITH_FRAGMENTS) * VCHIQDevice.FragmentsSize);
  
   {Get Head Bytes}
   HeadBytes:=(VCHIQDevice.CacheLineSize - Pagelist.offset) and (VCHIQDevice.CacheLineSize - 1);
   
   {Get Tail Bytes}
   TailBytes:=(Pagelist.offset + Actual) and (VCHIQDevice.CacheLineSize - 1);
   
   {$IFDEF VC4VCHIQ_DEBUG}
   if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Free Pagelist (HeadBytes=' + IntToStr(HeadBytes) + ' TailBytes=' + IntToStr(TailBytes) + ')');
   {$ENDIF}
   
   {Copy Head Bytes}
   if (Actual >= 0) and (HeadBytes <> 0) then
    begin
     if HeadBytes > Actual then HeadBytes:=Actual;
     
     System.Move(Fragments^,Pointer(Pages[0] + Pagelist.offset)^,HeadBytes);
    end;
    
   {Copy Tail Bytes}
   if (Actual >= 0) and (HeadBytes < Actual) and (TailBytes <> 0) then
    begin
     System.Move(Pointer(Fragments + VCHIQDevice.CacheLineSize)^,Pointer(Pages[NumPages - 1] + PtrUInt((Pagelist.offset + Actual) and (MEMORY_PAGE_SIZE - 1) and not(VCHIQDevice.CacheLineSize - 1)))^,TailBytes);
    end;
  
   {Lock Mutex}
   MutexLock(VCHIQDevice.FragmentsMutex);   
   
   {Update Fragments}
   PPointer(Fragments)^:=VCHIQDevice.FreeFragments;
   
   {Set Free Fragments}
   VCHIQDevice.FreeFragments:=Fragments;
   
   {Unlock Mutex}
   MutexUnlock(VCHIQDevice.FragmentsMutex);   

   {$IFDEF VC4VCHIQ_DEBUG}
   if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Free Pagelist (Fragments=' + IntToHex(PtrUInt(Fragments),8) + ' FreeFragments=' + IntToHex(PtrUInt(VCHIQDevice.FreeFragments),8) + ')');
   {$ENDIF}
   
   {Wait Free Fragments}  
   SemaphoreSignal(VCHIQDevice.FragmentsSemaphore);
  end;
 
 {Free Pagelist}
 FreeMem(Pagelist);
end;

{==============================================================================}

procedure VCHIQDoorbellInterruptHandler(VCHIQ:PVCHIQDevice);
{From vchiq_doorbell_irq in vchiq_2835_arm.c}
var
 Status:LongWord;
begin
 {}
 {Check VCHIQ}
 if VCHIQ = nil then Exit;
 
 {Update Statistics}
 Inc(VCHIQ.DoorbellInterruptCount);
 
 {Read (and Clear) the Doorbell}
 Status:=PLongWord(PERIPHERALS_BASE + VCHIQ_DOORBELL_REGS_BASE + VCHIQ_DOORBELL_BELL0)^;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Was the doorbell rung?}
 if (Status and $04) <> 0 then
  begin
   VCHIQRemoteEventPollAll(VCHIQ.State);
  end;
end;

{==============================================================================}

function VCHIQKeepaliveExecute(State:PVCHIQ_STATE_T):PtrInt;
{From vchiq_keepalive_thread_func in vchiq_arm.c}
var
 UseCount:LongInt;
 ReleaseCount:LongInt;
 Status:VCHIQ_STATUS_T;
 Instance:PVCHIQInstance;
 Params:VCHIQ_SERVICE_PARAMS_T;
 PlatformState:PVCHIQ_PLATFORM_STATE_T;
 KeepaliveHandle:VCHIQ_SERVICE_HANDLE_T;
begin
 {}
 Result:=0;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Keepalive Execute (ThreadID=' + IntToHex(ThreadGetCurrent,8) + ')');
 {$ENDIF}
 
 {Check State}
 if State = nil then Exit;

 {Get Platform State}
 PlatformState:=State.platform_state;
 if PlatformState = nil then Exit;
 
 {Update Priority}
 Sleep(1);
 
 {Create Params}
 Params.fourcc:=VCHIQ_MAKE_FOURCC('K','E','E','P');
 Params.callback:=VCHIQKeepaliveCallback;
 Params.version:=VCHIQ_KEEPALIVE_VER;
 Params.version_min:=VCHIQ_KEEPALIVE_VER_MIN;
 
 {Initialize}
 Status:=VCHIQInitialise(Instance);
 if Status <> VCHIQ_SUCCESS then
  begin
   if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Keepalive: Initialize failed (Status=' + VCHIQStatusToString(Status) + ')');
   
   Exit;
  end;
 try 
  {Connect} 
  Status:=VCHIQConnect(Instance);
  if Status <> VCHIQ_SUCCESS then
   begin
    if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Keepalive: Connect failed (Status=' + VCHIQStatusToString(Status) + ')');
    
    Exit;
   end;
  
  {Add Service}
  Status:=VCHIQAddService(Instance,@Params,KeepaliveHandle);
  if Status <> VCHIQ_SUCCESS then
   begin
    if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Keepalive: Add Service failed (Status=' + VCHIQStatusToString(Status) + ')');
    
    Exit;
   end;
  
  while True do
   begin
    {Set Defaults}
    UseCount:=0;
    ReleaseCount:=0;
    
    if CompletionWait(PlatformState.ka_evt) = ERROR_SUCCESS then
     begin
      {Read and clear counters. Do release_count then use_count to prevent getting more releases than uses}
      ReleaseCount:=InterlockedExchange(PlatformState.ka_release_count,0);
      UseCount:=InterlockedExchange(PlatformState.ka_use_count,0);
      
      {Call use/release service the requisite number of times. Process use before release so use counts don't go negative}
      while UseCount <> 0 do
       begin
        {Increment Acknowledge Count}
        InterlockedIncrement(PlatformState.ka_use_ack_count);
       
        {Use Service}
        Status:=VCHIQUseService(KeepaliveHandle);
        if Status <> VCHIQ_SUCCESS then
         begin
          if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Keepalive: Use Service failed (Status=' + VCHIQStatusToString(Status) + ')');
         end;
         
        {Decrement Use Count}
        Dec(UseCount);
       end;
      
      while ReleaseCount <> 0 do
       begin
        {Release Service}
        Status:=VCHIQReleaseService(KeepaliveHandle);
        if Status <> VCHIQ_SUCCESS then
         begin
          if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Keepalive: Release Service failed (Status=' + VCHIQStatusToString(Status) + ')');
         end;
        
        {Decrement Release Count}
        Dec(ReleaseCount);
       end;
     end
    else
     begin  
      if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Keepalive: Wait failed');     
      
      Sleep(1000);
     end; 
   end; 
 finally
  {Shutdown}
  VCHIQShutdown(Instance);
 end; 
end;

{==============================================================================}

function VCHIQKeepaliveCallback(Reason:VCHIQ_REASON_T;Header:PVCHIQ_HEADER_T;Handle:VCHIQ_SERVICE_HANDLE_T;Data:Pointer):VCHIQ_STATUS_T;
{From vchiq_keepalive_vchiq_callback in vchiq_arm.c}
begin
 {}
 if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Keepalive Callback (Reason=' + VCHIQReasonToString(Reason) + ')');
 
 Result:=VCHIQ_SUCCESS;
end;

{==============================================================================}
{==============================================================================}
{VCHIQ Kernel Functions}
function VCHIQInitialise(var Instance:PVCHIQInstance):VCHIQ_STATUS_T;
{From vchiq_initialise in vchiq_kern_lib.c}

{Note: This differs from VCHIQDeviceOpen but is compatible with it}
var
 Count:Integer;
 State:PVCHIQ_STATE_T;
begin
 {}
 Result:=VCHIQ_ERROR;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Initialise (Instance=' + IntToHex(PtrUInt(Instance),8) + ')');
 {$ENDIF}
 
 {VideoCore may not be ready due to boot up timing. It may never be ready if kernel and firmware are mismatched, so don't block forever}
 Count:=0;
 while Count < VCHIQ_INIT_RETRIES do
  begin
   {Get State}
   State:=VCHIQGetState;
   if State <> nil then Break;
   
   MicrosecondDelay(500);
   Inc(Count);
  end;
 
 {Check Count}
 if Count = VCHIQ_INIT_RETRIES then
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Initialise: Videocore not intialized');
   Exit;
  end
 else if Count > 0 then
  begin
   if DEVICE_LOG_ENABLED then DeviceLogInfo(nil,'VCHIQ: Initialise: Videocore intialized after ' + IntToStr(Count) + ' retries');
  end;
  
 {Allocate Instance}
 Instance:=AllocMem(SizeOf(TVCHIQInstance));
 if Instance = nil then 
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: Initialise: Failed to allocate instance');
   Exit;
  end;
  
 {Initialize Instance}
 Instance.VCHIQ:=VCHIQDevice;
 Instance.State:=State;
 Instance.PID:=1; {Ultibo has no concept of process so we use a dummy value}
 Instance.InsertEvent:=SemaphoreCreate(0);
 Instance.RemoveEvent:=SemaphoreCreate(0);
 Instance.CompletionMutex:=MutexCreate;
 Instance.BulkWaiterMutex:=MutexCreate;

 {Return Success}
 Result:=VCHIQ_SUCCESS;
end;

{==============================================================================}

function VCHIQIsConnected(Instance:PVCHIQInstance):Boolean; inline;
{From vchiq_is_connected in vchiq_kern_lib.c}
begin
 {}
 Result:=(Instance <> nil) and (Instance.Connected);
end;

{==============================================================================}

function VCHIQConnect(Instance:PVCHIQInstance):VCHIQ_STATUS_T;
{From vchiq_connect in vchiq_kern_lib.c}
var
 State:PVCHIQ_STATE_T;
 Status:VCHIQ_STATUS_T;
begin
 {}
 Result:=VCHIQ_ERROR;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Connect (Instance=' + IntToHex(PtrUInt(Instance),8) + ')');
 {$ENDIF}
 
 {Check Instance}
 if Instance = nil then Exit;
 
 {Get State}
 State:=Instance.State;
 if State = nil then Exit;
 
 {Lock Mutex}
 if MutexLock(State.mutex) <> ERROR_SUCCESS then
  begin
   Result:=VCHIQ_RETRY;
   Exit;
  end;

 {Connect Internal}  
 Status:=VCHIQConnectInternal(State,Instance);
 if Status = VCHIQ_SUCCESS then
  begin
   Instance.Connected:=True;
  end;

 {Unlock Mutex}
 MutexUnlock(State.mutex);
  
 {Return Status}
 Result:=Status;
end;

{==============================================================================}

function VCHIQShutdown(Instance:PVCHIQInstance):VCHIQ_STATUS_T;
{From vchiq_shutdown in vchiq_kern_lib.c}

{Note: This differs from VCHIQDeviceClose}
var
 State:PVCHIQ_STATE_T;
 Status:VCHIQ_STATUS_T;
begin
 {}
 Result:=VCHIQ_ERROR;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Shutdown (Instance=' + IntToHex(PtrUInt(Instance),8) + ')');
 {$ENDIF}
 
 {Check Instance}
 if Instance = nil then Exit;
 
 {Get State}
 State:=Instance.State;
 if State = nil then Exit;

 {Lock Mutex}
 if MutexLock(State.mutex) <> ERROR_SUCCESS then
  begin
   Result:=VCHIQ_RETRY;
   Exit;
  end;
 
 {Remove all services}
 Status:=VCHIQShutdownInternal(State,Instance);
 
 {Unlock Mutex}
 MutexUnlock(State.mutex);
 
 {Check Status}
 if Status = VCHIQ_SUCCESS then
  begin
   //To Do //Continuing //Instance.bulk_waiter_list
                        //When freeing Waiter also free the bulk_waiter.event Semaphore
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: VCHIQShutdown'); //To Do //Temp
  
   {Destroy Semaphores and Mutexes}
   MutexDestroy(Instance.BulkWaiterMutex);
   MutexDestroy(Instance.CompletionMutex);
   SemaphoreDestroy(Instance.RemoveEvent);
   SemaphoreDestroy(Instance.InsertEvent);
   
   {Invalidate Semaphores and Mutexes}
   Instance.BulkWaiterMutex:=INVALID_HANDLE_VALUE;
   Instance.CompletionMutex:=INVALID_HANDLE_VALUE;
   Instance.RemoveEvent:=INVALID_HANDLE_VALUE;
   Instance.InsertEvent:=INVALID_HANDLE_VALUE;
   
   {Free Instance}
   FreeMem(Instance);
  end;
  
 {Return Status}
 Result:=Status;
end;

{==============================================================================}

function VCHIQAddService(Instance:PVCHIQInstance;Params:PVCHIQ_SERVICE_PARAMS_T;var Handle:VCHIQ_SERVICE_HANDLE_T):VCHIQ_STATUS_T;
{From vchiq_add_service in vchiq_kern_lib.c}
var
 State:PVCHIQ_STATE_T;
 Status:VCHIQ_STATUS_T;
 Service:PVCHIQ_SERVICE_T;
 ServiceState:VCHIQ_SRVSTATE_T;
begin
 {}
 Result:=VCHIQ_ERROR;
 
 {Set Defaults}
 Handle:=VCHIQ_SERVICE_HANDLE_INVALID;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Add Service (Instance=' + IntToHex(PtrUInt(Instance),8) + ')');
 {$ENDIF}
 
 {Check Instance}
 if Instance = nil then Exit;

 {Check Params}
 {if Params = nil then Exit;} {Checked by VCHIQAddServiceInternal}
 
 {Get State}
 State:=Instance.State;
 if State = nil then Exit;
 
 {Get Service State}
 ServiceState:=VCHIQ_SRVSTATE_HIDDEN;
 if VCHIQIsConnected(Instance) then ServiceState:=VCHIQ_SRVSTATE_LISTENING;
 
 {Add Service Internal}
 Service:=VCHIQAddServiceInternal(State,Params,ServiceState,Instance,nil);
 if Service <> nil then
  begin
   {Return Handle}
   Handle:=Service.handle;
   
   Status:=VCHIQ_SUCCESS;
  end
 else
  begin
   Status:=VCHIQ_ERROR;
  end;  
 
 {Return Status}
 Result:=Status;
end;

{==============================================================================}

function VCHIQOpenService(Instance:PVCHIQInstance;Params:PVCHIQ_SERVICE_PARAMS_T;var Handle:VCHIQ_SERVICE_HANDLE_T):VCHIQ_STATUS_T;
{From vchiq_open_service in vchiq_kern_lib.c}
var
 State:PVCHIQ_STATE_T;
 Status:VCHIQ_STATUS_T;
 Service:PVCHIQ_SERVICE_T;
begin
 {}
 Result:=VCHIQ_ERROR;
 
 {Set Defaults}
 Handle:=VCHIQ_SERVICE_HANDLE_INVALID;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Open Service (Instance=' + IntToHex(PtrUInt(Instance),8) + ')');
 {$ENDIF}
 
 {Check Instance}
 if Instance = nil then Exit;

 {Check Params}
 {if Params = nil then Exit;} {Checked by VCHIQAddServiceInternal}
 
 {Get State}
 State:=Instance.State;
 if State = nil then Exit;
 
 {Check Connected}
 if not VCHIQIsConnected(Instance) then Exit;
 
 {Add Service Internal}
 Service:=VCHIQAddServiceInternal(State,Params,VCHIQ_SRVSTATE_OPENING,Instance,nil);
 if Service <> nil then
  begin
   {Return Handle}
   Handle:=Service.handle;

   {Open Service Internal}
   Status:=VCHIQOpenServiceInternal(Service,ThreadGetCurrent); {Thread ID (In the Linux driver this is the Process ID which is the Thread ID)}
   if Status <> VCHIQ_SUCCESS then
    begin
     {Remove Service}
     VCHIQRemoveService(Service.handle);
     
     {Reset Handle}
     Handle:=VCHIQ_SERVICE_HANDLE_INVALID;
    end;
  end
 else
  begin
   Status:=VCHIQ_ERROR;
  end;  
 
 {Return Status}
 Result:=Status;
end;

{==============================================================================}

function VCHIQQueueBulkTransmit(Handle:VCHIQ_SERVICE_HANDLE_T;Data:Pointer;Size:LongWord;Userdata:Pointer):VCHIQ_STATUS_T;
{From vchiq_queue_bulk_transmit in vchiq_kern_lib.c}
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Queue Bulk Transmit (Handle=' + IntToHex(Handle,8) + ')');
 {$ENDIF}
 
 Result:=VCHIQBulkTransfer(Handle,VCHI_MEM_HANDLE_INVALID,Data,Size,Userdata,VCHIQ_BULK_MODE_CALLBACK,VCHIQ_BULK_TRANSMIT);
end;

{==============================================================================}

function VCHIQQueueBulkReceive(Handle:VCHIQ_SERVICE_HANDLE_T;Data:Pointer;Size:LongWord;Userdata:Pointer):VCHIQ_STATUS_T;
{From vchiq_queue_bulk_receive in vchiq_kern_lib.c}
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Queue Bulk Receive (Handle=' + IntToHex(Handle,8) + ')');
 {$ENDIF}
 
 Result:=VCHIQBulkTransfer(Handle,VCHI_MEM_HANDLE_INVALID,Data,Size,Userdata,VCHIQ_BULK_MODE_CALLBACK,VCHIQ_BULK_RECEIVE);
end;

{==============================================================================}

function VCHIQBulkTransmit(Handle:VCHIQ_SERVICE_HANDLE_T;Data:Pointer;Size:LongWord;Userdata:Pointer;Mode:VCHIQ_BULK_MODE_T):VCHIQ_STATUS_T;
{From vchiq_bulk_transmit in vchiq_kern_lib.c}
var
 Status:VCHIQ_STATUS_T;
begin
 {}
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Bulk Transmit (Handle=' + IntToHex(Handle,8) + ')');
 {$ENDIF}
 
 {Check Mode}
 case Mode of 
  VCHIQ_BULK_MODE_NOCALLBACK,VCHIQ_BULK_MODE_CALLBACK:begin
    Status:=VCHIQBulkTransfer(Handle,VCHI_MEM_HANDLE_INVALID,Data,Size,Userdata,Mode,VCHIQ_BULK_TRANSMIT);
   end;
  VCHIQ_BULK_MODE_BLOCKING:begin
    Status:=VCHIQBlockingBulkTransfer(Handle,Data,Size,VCHIQ_BULK_TRANSMIT);
   end;
  else 
   begin
    Status:=VCHIQ_ERROR;
   end;
 end;
 
 Result:=Status
end;

{==============================================================================}

function VCHIQBulkReceive(Handle:VCHIQ_SERVICE_HANDLE_T;Data:Pointer;Size:LongWord;Userdata:Pointer;Mode:VCHIQ_BULK_MODE_T):VCHIQ_STATUS_T;
{From vchiq_bulk_receive in vchiq_kern_lib.c}
var
 Status:VCHIQ_STATUS_T;
begin
 {}
 Result:=VCHIQ_ERROR;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Bulk Receive (Handle=' + IntToHex(Handle,8) + ')');
 {$ENDIF}
 
 {Check Mode}
 case Mode of 
  VCHIQ_BULK_MODE_NOCALLBACK,VCHIQ_BULK_MODE_CALLBACK:begin
    Status:=VCHIQBulkTransfer(Handle,VCHI_MEM_HANDLE_INVALID,Data,Size,Userdata,Mode,VCHIQ_BULK_RECEIVE);
   end;
  VCHIQ_BULK_MODE_BLOCKING:begin
    Status:=VCHIQBlockingBulkTransfer(Handle,Data,Size,VCHIQ_BULK_RECEIVE);
   end;
  else 
   begin
    Status:=VCHIQ_ERROR;
   end;
 end;  
 
 Result:=Status
end;

{==============================================================================}

function VCHIQBlockingBulkTransfer(Handle:VCHIQ_SERVICE_HANDLE_T;Data:Pointer;Size:LongWord;Dir:VCHIQ_BULK_DIR_T):VCHIQ_STATUS_T;
{From vchiq_blocking_bulk_transfer in vchiq_kern_lib.c}
var
 Status:VCHIQ_STATUS_T;
 Service:PVCHIQ_SERVICE_T;
 Instance:PVCHIQInstance;
begin
 {}
 Result:=VCHIQ_ERROR;
 
 {$IFDEF VC4VCHIQ_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VCHIQ: Blocking Bulk Transfer (Handle=' + IntToHex(Handle,8) + ' Data=' + IntToHex(PtrUInt(Data),8) + ' Size=' + IntToStr(Size) + ' Dir=' + VCHIQBulkDirToString(Dir) + ')');
 {$ENDIF}
 
 {Get Service}
 Service:=VCHIQFindServiceByHandle(Handle);
 if Service = nil then Exit;
 
 {Get Instance}
 Instance:=Service.Instance;

 //To Do //Continuing
 if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VCHIQ: VCHIQBlockingBulkTransfer'); //To Do //Temp
 
 {Unlock Service}
 VCHIQUnlockService(Service);
 
 //To Do //Continuing
end;

{==============================================================================}
{==============================================================================}
{VCHIQ Helper Functions}
function VCHIQ_MAKE_FOURCC(x0, x1, x2, x3: Char): LongWord; inline;
{From vchiq_if.h}
begin
 {}
 Result:=(Ord(x0) shl 24) or (Ord(x1) shl 16) or (Ord(x2) shl 8) or Ord(x3);
end;

{==============================================================================}

function VCHIQ_PORT_IS_VALID(Port:LongWord):Boolean; inline;
{From vchiq_core.h}
begin
 {}
 Result:=(Port < VCHIQ_PORT_FREE);
end; 

{==============================================================================}

function VCHIQ_MAKE_MSG(MsgType,SrcPort,DstPort:LongWord):LongWord; inline; 
{From vchiq_core.h}
begin
 {}
 Result:=(MsgType shl 24) or (SrcPort shl 12) or (DstPort shl 0);
end;

{==============================================================================}

function VCHIQ_MSG_TYPE(MsgId:LongWord):LongWord; inline; 
{From vchiq_core.h}
begin
 {}
 Result:=MsgId shr 24;
end;

{==============================================================================}

function VCHIQ_MSG_SRCPORT(MsgId:LongWord):LongWord; inline; 
{From vchiq_core.h}
begin
 {}
 Result:=(MsgId shr 12) and $fff;
end;

{==============================================================================}

function VCHIQ_MSG_DSTPORT(MsgId:LongWord):LongWord; inline; 
{From vchiq_core.h}
begin
 {}
 Result:=MsgId and $fff;
end;

{==============================================================================}

procedure VCHIQ_FOURCC_AS_4CHARS(Fourcc:LongWord;var chars:VCHIQ_FOURCC_CHARS);
{From vchiq_core.h}
begin
 {}
 chars[0]:=(fourcc shr 24) and $ff;
 chars[1]:=(fourcc shr 16) and $ff;
 chars[2]:=(fourcc shr  8) and $ff;
 chars[3]:=fourcc and $ff;
end;

{==============================================================================}

function VCHIQ_FOURCC_IS_LEGAL(Fourcc:LongWord):Boolean; inline;
{From vchiq_core.h}
begin
 {}
 Result:=(fourcc <> VCHIQ_FOURCC_INVALID);
end;

{==============================================================================}

function VCHIQ_BITSET_SIZE(b:LongWord):LongWord; inline;
{From vchiq_core.h}
begin
 {}
 Result:=(b + 31) shr 5;
end;

{==============================================================================}

function VCHIQ_BITSET_WORD(b:LongWord):LongWord; inline; 
{From vchiq_core.h}
begin
 {}
 Result:=b shr 5;
end;

{==============================================================================}

function VCHIQ_BITSET_BIT(b:LongWord):LongWord; inline;
{From vchiq_core.h}
begin
 {}
 Result:=1 shl (b and 31);
end;

{==============================================================================}

procedure VCHIQ_BITSET_ZERO(bs:PBITSET_T;Size:LongWord); inline;
{From vchiq_core.h}
begin
 {}
 FillChar(bs^,Size * SizeOf(BITSET_T),0);
end;

{==============================================================================}

function VCHIQ_BITSET_IS_SET(bs:PBITSET_T;b:LongWord):Boolean; inline;
{From vchiq_core.h}
begin
 {}
 Result:=(bs[VCHIQ_BITSET_WORD(b)] and VCHIQ_BITSET_BIT(b)) <> 0;
end;

{==============================================================================}

procedure VCHIQ_BITSET_SET(bs:PBITSET_T;b:LongWord); inline;
{From vchiq_core.h}
begin
 {}
 bs[VCHIQ_BITSET_WORD(b)]:=bs[VCHIQ_BITSET_WORD(b)] or VCHIQ_BITSET_BIT(b);
end;

{==============================================================================}

procedure VCHIQ_BITSET_CLR(bs:PBITSET_T;b:LongWord); inline;
{From vchiq_core.h}
begin
 {}
 bs[VCHIQ_BITSET_WORD(b)]:=bs[VCHIQ_BITSET_WORD(b)] and not(VCHIQ_BITSET_BIT(b));
end;

{==============================================================================}

function VCHIQ_SLOT_INFO_FROM_INDEX(State:PVCHIQ_STATE_T;Index:Integer):PVCHIQ_SLOT_INFO_T;
{From vchiq_core.c}
begin
 {}
 Result:=@State.slot_info[Index];
end;

{==============================================================================}

function VCHIQ_SLOT_DATA_FROM_INDEX(State:PVCHIQ_STATE_T;Index:Integer):PByte;
{From vchiq_core.c}
begin
 {}
 Result:=@State.slot_data[Index];
end;

{==============================================================================}

function VCHIQ_SLOT_INDEX_FROM_DATA(State:PVCHIQ_STATE_T;Data:PByte):Integer;
{From vchiq_core.c}
begin
 {}
 Result:=(PtrUInt(Data) - PtrUInt(State.slot_data)) div VCHIQ_SLOT_SIZE;
end;

{==============================================================================}

function VCHIQ_SLOT_INDEX_FROM_INFO(State:PVCHIQ_STATE_T;Info:PVCHIQ_SLOT_INFO_T):Integer;
{From vchiq_core.c}
begin
 {}
 Result:=(PtrUInt(Info) - PtrUInt(@State.slot_info)) div SizeOf(VCHIQ_SLOT_INFO_T);
end;

{==============================================================================}

function VCHIQ_SLOT_QUEUE_INDEX_FROM_POS(Pos:LongWord):Integer;
{From vchiq_core.c}
begin
 {}
 Result:=(Pos div VCHIQ_SLOT_SIZE);
end;

{==============================================================================}

function VCHIQ_BULK_INDEX(x:Integer):Integer; inline;
{From vchiq_core.c}
begin
 {}
 Result:=x and (VCHIQ_NUM_SERVICE_BULKS - 1);
end;

{==============================================================================}

function VCHIQCalcStride(Size:LongWord):LongWord; inline;
{From calc_stride in vchiq_core.c}
begin
 {}
 {Allow room for the header}
 Size:=Size + VCHIQ_HEADER_SIZE; {SizeOf(VCHIQ_HEADER_T)} {Do not include the data member}
 
 {Round up}
 Result:=(Size + VCHIQ_HEADER_SIZE {SizeOf(VCHIQ_HEADER_T)} - 1) and not(VCHIQ_HEADER_SIZE {SizeOf(VCHIQ_HEADER_T)} - 1); {Do not include the data member}
end;

{==============================================================================}

function VCHIQFourccToString(Fourcc:LongWord):String;
begin
 {}
 SetLength(Result,4);
 
 Result[1]:=Chr((Fourcc shr 24) and $ff);
 Result[2]:=Chr((Fourcc shr 16) and $ff);
 Result[3]:=Chr((Fourcc shr  8) and $ff);
 Result[4]:=Chr(Fourcc and $ff);
end;

{==============================================================================}

function VCHIQMessageTypeToString(MsgType:LongWord):String;
begin
 {}
 Result:='VCHIQ_MSG_UNKNOWN';
 
 case MsgType of
  VCHIQ_MSG_PADDING:Result:='VCHIQ_MSG_PADDING';
  VCHIQ_MSG_CONNECT:Result:='VCHIQ_MSG_CONNECT';
  VCHIQ_MSG_OPEN:Result:='VCHIQ_MSG_OPEN';
  VCHIQ_MSG_OPENACK:Result:='VCHIQ_MSG_OPENACK';
  VCHIQ_MSG_CLOSE:Result:='VCHIQ_MSG_CLOSE';
  VCHIQ_MSG_DATA:Result:='VCHIQ_MSG_DATA';
  VCHIQ_MSG_BULK_RX:Result:='VCHIQ_MSG_BULK_RX';
  VCHIQ_MSG_BULK_TX:Result:='VCHIQ_MSG_BULK_TX';
  VCHIQ_MSG_BULK_RX_DONE:Result:='VCHIQ_MSG_BULK_RX_DONE';
  VCHIQ_MSG_BULK_TX_DONE:Result:='VCHIQ_MSG_BULK_TX_DONE';
  VCHIQ_MSG_PAUSE:Result:='VCHIQ_MSG_PAUSE';
  VCHIQ_MSG_RESUME:Result:='VCHIQ_MSG_RESUME';
  VCHIQ_MSG_REMOTE_USE:Result:='VCHIQ_MSG_REMOTE_USE';
  VCHIQ_MSG_REMOTE_RELEASE:Result:='VCHIQ_MSG_REMOTE_RELEASE';
  VCHIQ_MSG_REMOTE_USE_ACTIVE:Result:='VCHIQ_MSG_REMOTE_USE_ACTIVE';
 end;
end;

{==============================================================================}

function VCHIQPollTypeToString(PollType:VCHIQ_POLL_T):String;
begin
 {}
 Result:='VCHIQ_POLL_UNKNOWN';
 
 case PollType of
  VCHIQ_POLL_TERMINATE:Result:='VCHIQ_POLL_TERMINATE';
  VCHIQ_POLL_REMOVE:Result:='VCHIQ_POLL_REMOVE';
  VCHIQ_POLL_TXNOTIFY:Result:='VCHIQ_POLL_TXNOTIFY';
  VCHIQ_POLL_RXNOTIFY:Result:='VCHIQ_POLL_RXNOTIFY';
 end;
end;

{==============================================================================}

function VCHIQPageTypeToString(PageType:Word):String; 
begin
 {}
 Result:='VCHIQ_PAGELIST_UNKNOWN';
 
 case PageType of
  VCHIQ_PAGELIST_WRITE:Result:='VCHIQ_PAGELIST_WRITE';
  VCHIQ_PAGELIST_READ:Result:='VCHIQ_PAGELIST_READ';
  VCHIQ_PAGELIST_READ_WITH_FRAGMENTS:Result:='VCHIQ_PAGELIST_READ_WITH_FRAGMENTS';
 end;
end;

{==============================================================================}

function VCHIQStatusToString(Status:VCHIQ_STATUS_T):String;
begin
 {}
 Result:='VCHIQ_UNKNOWN';
 
 case Status of
  VCHIQ_ERROR:Result:='VCHIQ_ERROR';
  VCHIQ_SUCCESS:Result:='VCHIQ_SUCCESS';
  VCHIQ_RETRY:Result:='VCHIQ_RETRY';
 end;
end;

{==============================================================================}
 
function VCHIQBulkDirToString(BulkDir:VCHIQ_BULK_DIR_T):String;
begin
 {}
 Result:='VCHIQ_BULK_DIR_UNKNOWN';
 
 case BulkDir of
  VCHIQ_BULK_TRANSMIT:Result:='VCHIQ_BULK_TRANSMIT';
  VCHIQ_BULK_RECEIVE:Result:='VCHIQ_BULK_RECEIVE';
 end;
end;

{==============================================================================}

function VCHIQBulkModeToString(BulkMode:VCHIQ_BULK_MODE_T):String;
begin
 {}
 Result:='VCHIQ_BULK_MODE_UNKNOWN';
 
 case BulkMode of
  VCHIQ_BULK_MODE_CALLBACK:Result:='VCHIQ_BULK_MODE_CALLBACK';
  VCHIQ_BULK_MODE_BLOCKING:Result:='VCHIQ_BULK_MODE_BLOCKING';
  VCHIQ_BULK_MODE_NOCALLBACK:Result:='VCHIQ_BULK_MODE_NOCALLBACK';
  VCHIQ_BULK_MODE_WAITING:Result:='VCHIQ_BULK_MODE_WAITING';
 end; 
end;

{==============================================================================}

function VCHIQServiceOptionToString(ServiceOption:VCHIQ_SERVICE_OPTION_T):String;
begin
 {}
 Result:='VCHIQ_SERVICE_OPTION_UNKNOWN';
 
 case ServiceOption of
  VCHIQ_SERVICE_OPTION_AUTOCLOSE:Result:='VCHIQ_SERVICE_OPTION_AUTOCLOSE';
  VCHIQ_SERVICE_OPTION_SLOT_QUOTA:Result:='VCHIQ_SERVICE_OPTION_SLOT_QUOTA';
  VCHIQ_SERVICE_OPTION_MESSAGE_QUOTA:Result:='VCHIQ_SERVICE_OPTION_MESSAGE_QUOTA';
  VCHIQ_SERVICE_OPTION_SYNCHRONOUS:Result:='VCHIQ_SERVICE_OPTION_SYNCHRONOUS';
  VCHIQ_SERVICE_OPTION_TRACE:Result:='VCHIQ_SERVICE_OPTION_TRACE';
 end; 
end;

{==============================================================================}

function VCHIQReasonToString(Reason:VCHIQ_REASON_T):String; inline;
{From vchiq_core.h}
begin
 {}
 Result:=VCHIQ_REASON_NAMES[Reason];
end;

{==============================================================================}

function VCHIQConnStateToString(ConnState:VCHIQ_CONNSTATE_T):String; inline;
{From get_conn_state_name in vchiq_core.h}
begin
 {}
 Result:=VCHIQ_CONNSTATE_NAMES[ConnState];
end;

{==============================================================================}

function VCHIQServiceStateToString(ServiceState:VCHIQ_SRVSTATE_T):String; inline;
{From vchiq_core.h}
begin
 {}
 Result:=VCHIQ_SRVSTATE_NAMES[ServiceState];
end;

{==============================================================================}

function VCHIQSuspendStatusToString(Status:VC_SUSPEND_STATUS_T):String;
begin
 {}
 Result:='VC_SUSPEND_UNKNOWN';
 
 case Status of
  VC_SUSPEND_FORCE_CANCELED:Result:='VC_SUSPEND_FORCE_CANCELED';
  VC_SUSPEND_REJECTED:Result:='VC_SUSPEND_REJECTED';
  VC_SUSPEND_FAILED:Result:='VC_SUSPEND_FAILED';
  VC_SUSPEND_IDLE:Result:='VC_SUSPEND_IDLE';
  VC_SUSPEND_REQUESTED:Result:='VC_SUSPEND_REQUESTED';
  VC_SUSPEND_IN_PROGRESS:Result:='VC_SUSPEND_IN_PROGRESS';
  VC_SUSPEND_SUSPENDED:Result:='VC_SUSPEND_SUSPENDED';
 end; 
end;

{==============================================================================}

function VCHIQResumeStatusToString(Status:VC_RESUME_STATUS_T):String;
begin
 {}
 Result:='VC_RESUME_UNKNOWN';
 
 case Status of
  VC_RESUME_FAILED:Result:='VC_RESUME_FAILED';
  VC_RESUME_IDLE:Result:='VC_RESUME_IDLE';
  VC_RESUME_REQUESTED:Result:='VC_RESUME_REQUESTED';
  VC_RESUME_IN_PROGRESS:Result:='VC_RESUME_IN_PROGRESS';
  VC_RESUME_RESUMED:Result:='VC_RESUME_RESUMED';
 end; 
end;

{==============================================================================}
{==============================================================================}

initialization
 VCHIQInit;

{==============================================================================}
 
{finalization}
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
 