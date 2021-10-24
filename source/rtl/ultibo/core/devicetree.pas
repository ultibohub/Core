{
Ultibo Flattened Device Tree (DTB) interface unit.

Copyright (C) 2021 - SoftOz Pty Ltd.

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

 Devicetree Specification Release v0.3

  https://github.com/devicetree-org/devicetree-specification/releases/download/v0.3/devicetree-specification-v0.3.pdf
  
 The Linux Kernel - Open Firmware and Device Tree
  
  https://www.kernel.org/doc/html/v5.11/devicetree/index.html
  
 Device Tree Usage
  
  https://elinux.org/Device_Tree_Usage
  
Device Tree
===========

 Using Device Tree Blob (DTB) files for passing configuration information to the Linux kernel is
 now widely accepted as the standard method of providing platform specific information without
 it needing to be hard coded into the kernel at build time.
 
 While this adoption of device tree by Linux it is becomming increasinly common for the only
 source of information about the runtime configuration of specific hardware to be found in
 the blob loaded and configured during startup by the firmware and passed to the kernel at
 the beginning of the boot process.
  
 While Ultibo targets a much smaller range of devices than Linux and does not need the generic
 kernel model it still requires detailed configuration information in order to be able to
 interface with the full range of available devices. 
 
 It can also benefit from the ability to adapt to the runtime environment by checking for known
 device tree parameters while registering and initializing hardware and devices.

 Use of device tree is considered optional and Ultibo will operate correctly with or without
 the information provided, however some devices may be forced to operate in a fall back or 
 default mode without the complete configuration.
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit DeviceTree;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {Device Tree specific constants}
 {DTB Signature}
 DTB_MAGIC = $D00DFEED;
 
 {DTB Versions}
 DTB_VERSION_17 = 17;
 DTB_VERSION_16 = 16;
 
 DTB_VERSION_CURRENT    = DTB_VERSION_17;
 DTB_VERSION_COMPATIBLE = DTB_VERSION_16;
 
 {DTB Tokens}
 DTB_BEGIN_NODE = $00000001; {The FDT_BEGIN_NODE token marks the beginning of a node’s representation, followed by the node’s unit name as extra data}
 DTB_END_NODE   = $00000002; {The FDT_END_NODE token marks the end of a node’s representation}
 DTB_PROP       = $00000003; {The FDT_PROP token marks the beginning of the representation of one property, followed by extra data describing the property}
 DTB_NOP        = $00000004; {The FDT_NOP token will be ignored by any program parsing the device tree}
 DTB_END        = $00000009; {The FDT_END token marks the end of the structure block}
 
 {DTB Offsets}
 DTB_NODE_OFFSET     = 4;  {Token}
 DTB_PROPERTY_OFFSET = 12; {Token + Value Length + Name Offset}
 
 {DTB Alignment}
 DTB_STRUCTURE_ALIGNMENT   = 4;
 DTB_RESERVATION_ALIGNMENT = 8;
 
 {DTB Nodes}
 {Standard Nodes}
 DTB_NODE_ROOT = '/';
 DTB_NODE_CPUS = 'cpus';
 DTB_NODE_MEMORY = 'memory';
 DTB_NODE_CHOSEN = 'chosen';
 DTB_NODE_ALIASES = 'aliases';
 {Other Nodes}
 DTB_NODE_SYMBOLS = '__symbols__';
 DTB_NODE_OVERRIDES = '__overrides__';
 
 {DTB Paths}
 DTB_PATH_SEPARATOR = '/';
 
 {DTB Types}
 DTB_TYPE_UNKNOWN       = 0; 
 DTB_TYPE_EMPTY         = 1; {Value is empty. Used for conveying true-false information}
 DTB_TYPE_U32           = 2; {A 32-bit integer in big-endian format}
 DTB_TYPE_U64           = 3; {Represents a 64-bit integer in big-endian format}
 DTB_TYPE_STRING        = 4; {Strings are printable and null-terminated}
 DTB_TYPE_ENCODED_ARRAY = 5; {Format is specific to the property. See the property definition}
 DTB_TYPE_PHANDLE       = 6; {A <u32> value. A phandle value is a way to reference another node in the devicetree}
 DTB_TYPE_STRINGLIST    = 7; {A list of <string> values concatenated together}
 
 {DTB Properties}
 {Standard Properties}
 DTB_PROPERTY_COMPATIBLE    = 'compatible'; {The compatible property value consists of one or more strings that define the specific programming model for the device (Type: <stringlist>)}
 DTB_PROPERTY_MODEL         = 'model'; {The model property value specifies the manufacturer’s model number of the device (Type: <string>)}
 DTB_PROPERTY_PHANDLE       = 'phandle'; {The phandle property specifies a numerical identifier for a node that is unique within the devicetree (Type: <u32>)}
 DTB_PROPERTY_STATUS        = 'status'; {The status property indicates the operational status of a device (Type: <string>)}
 DTB_PROPERTY_ADDRESS_CELLS = '#address-cells'; {Specifies the number of <u32> cells to represent the address in the reg property (Type: <u32>)}
 DTB_PROPERTY_SIZE_CELLS    = '#size-cells'; {Specifies the number of <u32> cells to represent the size in the reg property (Type: <u32>)}
 DTB_PROPERTY_REG           = 'reg'; {The reg property describes the address of the device’s resources within the address space defined by its parent bus (Type: <prop-encoded-array>)}
 DTB_PROPERTY_VIRTUAL_REG   = 'virtual-reg'; {The virtual-reg property specifies an effective address that maps to the first physical address specified in the reg property of the device node (Type: <u32>)}
 DTB_PROPERTY_RANGES        = 'ranges'; {The ranges property provides a means of defining a mapping or translation between address spaces (Type: <empty> or <prop-encoded-array>)}
 DTB_PROPERTY_DMA_RANGES    = 'dma-ranges'; {The dma-ranges property is used to describe the direct memory access (DMA) structure of a memory-mapped bus (Type: <empty> or <prop-encoded-array>)}
 DTB_PROPERTY_NAME          = 'name'; {The name property is a string specifying the name of the node (Type: <string>)}
 DTB_PROPERTY_DEVICE_TYPE   = 'device_type'; {The device_type property was used in IEEE 1275 to describe the device (Type: <string>)}
 
 {Additional Properties for Interrupt Generating Devices}
 DTB_PROPERTY_INTERRUPTS          = 'interrupts'; {The interrupts property defines the interrupt or interrupts that are generated by the device (Type: <prop-encoded-array>)}
 DTB_PROPERTY_INTERRUPT_PARENT    = 'interrupt-parent'; {The interrupt-parent property provides an explicit definition of an interrupt parent (Type: <phandle>)}
 DTB_PROPERTY_INTERRUPTS_EXTENDED = 'interrupts-extended'; {The interrupts-extended property lists the interrupt(s) generated by a device (Type: <phandle> <prop-encoded-array>)}
 
 {Additional Properties for Interrupt Controllers}
 DTB_PROPERTY_INTERRUPT_CELLS      = '#interrupt-cells'; {The #interrupt-cells property defines the number of cells required to encode an interrupt specifier for an interrupt domain (Type: <u32>)}
 DTB_PROPERTY_INTERRUPT_CONTROLLER = 'interrupt-controller'; {The presence of an interrupt-controller property defines a node as an interrupt controller node (Type: <empty>)}
 
 {Additional Interrupt Nexus Properties}
 DTB_PROPERTY_INTERRUPT_MAP      = 'interrupt-map'; {An interrupt-map is a property on a nexus node that bridges an interrupt domain to a parent domain (Type: <prop-encoded-array>)}
 DTB_PROPERTY_INTERRUPT_MAP_MASK = 'interrupt-map-mask'; {An interrupt-map-mask property is specified for a nexus node in the interrupt tree (Type: <prop-encoded-array>)}

 {Additional Memory Node Properties}
 DTB_PROPERTY_INITIAL_MAPPED_AREA  = 'initial-mapped-area'; {Specifies the address and size of the Initial Mapped Area (Type: <prop-encoded-array>)}
 
 {Additional Chosen Node Properties}
 DTB_PROPERTY_BOOTARGS     = 'bootargs'; {A string that specifies the boot arguments for the client program (Type: <string>>)}
 DTB_PROPERTY_STDOUT_PATH  = 'stdout-path'; {A string that specifies the full path to the node representing the device to be used for boot console output (Type: <string>>)}
 DTB_PROPERTY_STDIN_PATH   = 'stdin-path'; {A string that specifies the full path to the node representing the device to be used for boot console input (Type: <string>>)}
 DTB_PROPERTY_INITRD_START = 'initrd-start'; {The starting address of the initial ram disk (Type: <u32>)}
 DTB_PROPERTY_INITRD_END   = 'initrd-end'; {The ending address of the initial ram disk (Type: <u32>)}
 
 {Additional CPUs Node Properties}
 DTB_PROPERTY_CLOCK_FREQUENCY          = 'clock-frequency'; {Specifies the current clock speed of the CPU in Hertz (Type: <prop-encoded-array>)}
 DTB_PROPERTY_TIMEBASE_FREQUENCY       = 'timebase-frequency'; {Specifies the current frequency at which the timebase and decrementer registers are updated (Type: <propencoded-array>)}
 DTB_PROPERTY_ENABLE_METHOD            = 'enable-method'; {Describes the method by which a CPU in a disabled state is enabled (Type: <stringlist>)}
 DTB_PROPERTY_CPU_RELEASE_ADDR         = 'cpu-release-addr'; {The cpu-release-addr property is required for cpu nodes that have an enable-method property value of "spin-table" (Type: <u64>)}
 
 DTB_PROPERTY_POWER_ISA_VERSION        = 'power-isa-version'; {A string that specifies the numerical portion of the Power ISA version string (Type: <string>)}
 DTB_PROPERTY_CACHE_OP_BLOCK_SIZE      = 'cache-op-block-size'; {The block size in bytes upon which cache block instructions operate (Type: <u32>)}
 DTB_PROPERTY_RESERVATION_GRANULE_SIZE = 'reservation-granule-size'; {The reservation granule size supported by this processor in bytes (Type: <u32>)}
 DTB_PROPERTY_MMU_TYPE                 = 'mmu-type'; {Specifies the CPU’s MMU type (Type: <string>)}

 DTB_PROPERTY_TLB_SPLIT                = 'tlb-split'; {Specifies that the TLB has a split configuration (Type: <empty>)}
 DTB_PROPERTY_TLB_SIZE                 = 'tlb-size'; {Specifies the number of entries in the TLB (Type: <u32>)}
 DTB_PROPERTY_TLB_SETS                 = 'tlb-sets'; {Specifies the number of associativity sets inthe TLB. (Type: <u32>)}
 DTB_PROPERTY_D_TLB_SIZE               = 'd-tlb-size'; {Specifies the number of entries in the data TLB (Type: <u32>)}
 DTB_PROPERTY_D_TLB_SETS               = 'd-tlb-sets'; {Specifies the number of associativity sets in the data TLB. (Type: <u32>)}
 DTB_PROPERTY_I_TLB_SIZE               = 'i-tlb-size'; {Specifies the number of entries in the instruction TLB (Type: <u32>)}
 DTB_PROPERTY_I_TLB_SETS               = 'i-tlb-sets'; {Specifies the number of associativity sets in the instruction TLB (Type: <u32>)}
 
 DTB_PROPERTY_CACHE_UNIFIED            = 'cache-unified'; {Specifies the cache has a unified organization (Type: <empty>)}
 DTB_PROPERTY_CACHE_SIZE               = 'cache-size'; {Specifies the size in bytes of a unified cache (Type: <u32>)}
 DTB_PROPERTY_CACHE_SETS               = 'cache-sets'; {Specifies the number of associativity sets in a unified cache (Type: <u32>)}
 DTB_PROPERTY_CACHE_BLOCK_SIZE         = 'cache-block-size'; {Specifies the block size in bytes of a unified cache (Type: <u32>)}
 DTB_PROPERTY_CACHE_LINE_SIZE          = 'cache-line-size'; {Specifies the line size in bytes of a unified cache (Type: <u32>)}
 DTB_PROPERTY_I_CACHE_SIZE             = 'i-cache-size'; {Specifies the size in bytes of the instruction cache (Type: <u32>)}
 DTB_PROPERTY_I_CACHE_SETS             = 'i-cache-sets'; {Specifies the number of associativity sets in the instruction cache (Type: <u32>)}
 DTB_PROPERTY_I_CACHE_BLOCK_SIZE       = 'i-cache-block-size'; {Specifies the block size in bytes of the instruction cache (Type: <u32>)}
 DTB_PROPERTY_I_CACHE_LINE_SIZE        = 'i-cache-line-size'; {Specifies the line size in bytes of the instruction cache (Type: <u32>)}
 DTB_PROPERTY_D_CACHE_SIZE             = 'd-cache-size'; {Specifies the size in bytes of the data cache (Type: <u32>)}
 DTB_PROPERTY_D_CACHE_SETS             = 'd-cache-sets'; {Specifies the number of associativity sets in the data cache. (Type: <u32>)}
 DTB_PROPERTY_D_CACHE_BLOCK_SIZE       = 'd-cache-block-size'; {Specifies the block size in bytes of the data cache. (Type: <u32>)}
 DTB_PROPERTY_D_CACHE_LINE_SIZE        = 'd-cache-line-size'; {Specifies the line size in bytes of the data cache, (Type: <u32>)}
 DTB_PROPERTY_NEXT_LEVEL_CACHE         = 'next-level-cache'; {If present, indicates that another level of cache exists. (Type: <phandle>)}
 
 DTB_PROPERTY_CACHE_LEVEL              = 'cache-level'; {Specifies the level in the cache hierarchy (Type: <u32>)}
 
 {Other Additional Properties}
 DTB_PROPERTY_SERIAL_NUMBER            = 'serial-number'; {Serial Number (Type: <string>)}
 DTB_PROPERTY_ALLOC_RANGES             = 'alloc-ranges'; {Allocated Ranges (Type: <prop-encoded-array>)}
 DTB_PROPERTY_RESETS                   = 'resets'; {Resets (Type: <prop-encoded-array>)}
 DTB_PROPERTY_CLOCK_CELLS              = '#clock-cells'; {Number of clock cells (Type: <u32>)}
 DTB_PROPERTY_LINUX_INITRD_START       = 'linux,initrd-start'; {The starting address of the initial ram disk (Type: <u32>)}
 DTB_PROPERTY_LINUX_INITRD_END         = 'linux,initrd-end'; {The ending address of the initial ram disk (Type: <u32>)}
 
 {DTB Property Types}
 DTB_MAX_PROPERTY_TYPE = 60;
 
{==============================================================================}
type
 {Device Tree specific types}
 {DTB Header}
 PDTBHeader = ^TDTBHeader;
 TDTBHeader = packed record
  Magic:LongWord;               {The value 0xd00dfeed (big-endian)}
  TotalSize:LongWord;           {The total size in bytes of the devicetree data structure (big-endian)}
  StructureOffset:LongWord;     {The offset in bytes of the structure block from the beginning of the header (big-endian)}
  StringsOffset:LongWord;       {The offset in bytes of the strings block from the beginning of the header (big-endian)}
  ReservationOffset:LongWord;   {The offset in bytes of the memory reservation block from the beginning of the header (big-endian)}
  Version:LongWord;             {The version of the devicetree data structure (big-endian)}
  CompatibleVersion:LongWord;   {The lowest version of the devicetree data structure with which the version used is backwards compatible (big-endian)}
  BootCPUID:LongWord;           {The physical ID of the system’s boot CPU (big-endian)}
  StringsSize:LongWord;         {The length in bytes of the strings block section of the devicetree blob (big-endian)}
  StructureSize:LongWord;       {The length in bytes of the structure block section of the devicetree blob (big-endian)}
 end;

 {DTB Reservation}
 PDTBReservation = ^TDTBReservation;
 TDTBReservation = packed record
  Address:UInt64; {Physical address of a reserved memory region}
  Size:UInt64;    {Size in bytes of a reserved memory region}
 end;
  
 {DTB Node}
 PDTBNode = ^TDTBNode;
 TDTBNode = packed record
  Token:LongWord;           {Token (big-endian)}
  Name:array[0..0] of Char; {The name stored as a null-terminated string, shall include the unit address if any}
 end; 

 {DTB Property}
 PDTBProperty = ^TDTBProperty;
 TDTBProperty = packed record
  Token:LongWord;            {Token (big-endian)}
  ValueLength:LongWord;      {The length of the property value in bytes}
  NameOffset:LongWord;       {The offset into the strings block of the property name stored as a null-terminated string}
  Value:array[0..0] of Byte; {The property value as an array of bytes of value length}
 end;

 {DTB Property Char}
 PDTBPropertyChar = ^TDTBPropertyChar;
 TDTBPropertyChar = packed record
  Token:LongWord;            {Token (big-endian)}
  ValueLength:LongWord;      {The length of the property value in bytes}
  NameOffset:LongWord;       {The offset into the strings block of the property name stored as a null-terminated string}
  Value:array[0..0] of Char; {The property value as a string of value length}
 end;

 {DTB Property LongWord}
 PDTBPropertyLongWord = ^TDTBPropertyLongWord;
 TDTBPropertyLongWord = packed record
  Token:LongWord;                {Token (big-endian)}
  ValueLength:LongWord;          {The length of the property value in bytes}
  NameOffset:LongWord;           {The offset into the strings block of the property name stored as a null-terminated string}
  Value:array[0..0] of LongWord; {The property value as an array of longwords of value length (big-endian)}
 end;

 {DTB Property QuadWord}
 PDTBPropertyQuadWord = ^TDTBPropertyQuadWord;
 TDTBPropertyQuadWord = packed record
  Token:LongWord;              {Token (big-endian)}
  ValueLength:LongWord;        {The length of the property value in bytes}
  NameOffset:LongWord;         {The offset into the strings block of the property name stored as a null-terminated string}
  Value:array[0..0] of UInt64; {The property value as an array of quadwords of value length (big-endian)}
 end;
 
 {$IFDEF DEVICE_TREE_ENUMERATION}
 {DTB Property Types}
 TDTBPropertyType = record
  Name:String;
  Encoding:LongWord;
 end;
 {$ENDIF DEVICE_TREE_ENUMERATION}
 
{==============================================================================}
{$IFDEF DEVICE_TREE_ENUMERATION}
type
 {Device Tree Logging specific types}
 TDTBLogOutput = procedure(const AText:String;Data:Pointer);
 TDTBDecodeValue = function(Node,Handle:THandle;Value:Pointer;Size:LongWord;Data:Pointer):String;
{$ENDIF DEVICE_TREE_ENUMERATION}

{==============================================================================}
{$IFDEF DEVICE_TREE_ENUMERATION}
var
 {Device Tree specific variables}
 {DTB Property Types}
 DTB_PROPERTY_TYPES:array[0..DTB_MAX_PROPERTY_TYPE] of TDTBPropertyType = (
  {Standard Properties}
  (Name:DTB_PROPERTY_COMPATIBLE;               Encoding:DTB_TYPE_STRINGLIST),
  (Name:DTB_PROPERTY_MODEL;                    Encoding:DTB_TYPE_STRING),
  (Name:DTB_PROPERTY_PHANDLE;                  Encoding:DTB_TYPE_U32),
  (Name:DTB_PROPERTY_STATUS;                   Encoding:DTB_TYPE_STRING),
  (Name:DTB_PROPERTY_ADDRESS_CELLS;            Encoding:DTB_TYPE_U32),
  (Name:DTB_PROPERTY_SIZE_CELLS;               Encoding:DTB_TYPE_U32),
  (Name:DTB_PROPERTY_REG;                      Encoding:DTB_TYPE_ENCODED_ARRAY),
  (Name:DTB_PROPERTY_VIRTUAL_REG;              Encoding:DTB_TYPE_U32),
  (Name:DTB_PROPERTY_RANGES;                   Encoding:DTB_TYPE_ENCODED_ARRAY),
  (Name:DTB_PROPERTY_DMA_RANGES;               Encoding:DTB_TYPE_ENCODED_ARRAY),
  (Name:DTB_PROPERTY_NAME;                     Encoding:DTB_TYPE_STRING),
  (Name:DTB_PROPERTY_DEVICE_TYPE;              Encoding:DTB_TYPE_STRING),
  {Additional Properties for Interrupt Generating Devices}
  (Name:DTB_PROPERTY_INTERRUPTS;               Encoding:DTB_TYPE_ENCODED_ARRAY),
  (Name:DTB_PROPERTY_INTERRUPT_PARENT;         Encoding:DTB_TYPE_PHANDLE),
  (Name:DTB_PROPERTY_INTERRUPTS_EXTENDED;      Encoding:DTB_TYPE_ENCODED_ARRAY),
  {Additional Properties for Interrupt Controllers}
  (Name:DTB_PROPERTY_INTERRUPT_CELLS;          Encoding:DTB_TYPE_U32),
  (Name:DTB_PROPERTY_INTERRUPT_CONTROLLER;     Encoding:DTB_TYPE_EMPTY),
  {Additional Interrupt Nexus Properties}
  (Name:DTB_PROPERTY_INTERRUPT_MAP;            Encoding:DTB_TYPE_ENCODED_ARRAY),
  (Name:DTB_PROPERTY_INTERRUPT_MAP_MASK;       Encoding:DTB_TYPE_ENCODED_ARRAY),
  {Additional Memory Node Properties}
  (Name:DTB_PROPERTY_INITIAL_MAPPED_AREA;      Encoding:DTB_TYPE_ENCODED_ARRAY),
  {Additional Chosen Node Properties}
  (Name:DTB_PROPERTY_BOOTARGS;                 Encoding:DTB_TYPE_STRING),
  (Name:DTB_PROPERTY_STDOUT_PATH;              Encoding:DTB_TYPE_STRING),
  (Name:DTB_PROPERTY_STDIN_PATH;               Encoding:DTB_TYPE_STRING),
  (Name:DTB_PROPERTY_INITRD_START;             Encoding:DTB_TYPE_U32),
  (Name:DTB_PROPERTY_INITRD_END;               Encoding:DTB_TYPE_U32),
  {Additional CPUs Node Properties}
  (Name:DTB_PROPERTY_CLOCK_FREQUENCY;          Encoding:DTB_TYPE_ENCODED_ARRAY),
  (Name:DTB_PROPERTY_TIMEBASE_FREQUENCY;       Encoding:DTB_TYPE_ENCODED_ARRAY),
  (Name:DTB_PROPERTY_ENABLE_METHOD;            Encoding:DTB_TYPE_STRINGLIST),
  (Name:DTB_PROPERTY_CPU_RELEASE_ADDR;         Encoding:DTB_TYPE_U64),
  
  (Name:DTB_PROPERTY_POWER_ISA_VERSION;        Encoding:DTB_TYPE_STRING),
  (Name:DTB_PROPERTY_CACHE_OP_BLOCK_SIZE;      Encoding:DTB_TYPE_U32),
  (Name:DTB_PROPERTY_RESERVATION_GRANULE_SIZE; Encoding:DTB_TYPE_U32),
  (Name:DTB_PROPERTY_MMU_TYPE;                 Encoding:DTB_TYPE_STRING),
  
  (Name:DTB_PROPERTY_TLB_SPLIT;                Encoding:DTB_TYPE_EMPTY),
  (Name:DTB_PROPERTY_TLB_SIZE;                 Encoding:DTB_TYPE_U32),
  (Name:DTB_PROPERTY_TLB_SETS;                 Encoding:DTB_TYPE_U32),
  (Name:DTB_PROPERTY_D_TLB_SIZE;               Encoding:DTB_TYPE_U32),
  (Name:DTB_PROPERTY_D_TLB_SETS;               Encoding:DTB_TYPE_U32),
  (Name:DTB_PROPERTY_I_TLB_SIZE;               Encoding:DTB_TYPE_U32),
  (Name:DTB_PROPERTY_I_TLB_SETS;               Encoding:DTB_TYPE_U32),
  
  (Name:DTB_PROPERTY_CACHE_UNIFIED;            Encoding:DTB_TYPE_EMPTY),
  (Name:DTB_PROPERTY_CACHE_SIZE;               Encoding:DTB_TYPE_U32),
  (Name:DTB_PROPERTY_CACHE_SETS;               Encoding:DTB_TYPE_U32),
  (Name:DTB_PROPERTY_CACHE_BLOCK_SIZE;         Encoding:DTB_TYPE_U32),
  (Name:DTB_PROPERTY_CACHE_LINE_SIZE;          Encoding:DTB_TYPE_U32),
  (Name:DTB_PROPERTY_I_CACHE_SIZE;             Encoding:DTB_TYPE_U32),
  (Name:DTB_PROPERTY_I_CACHE_SETS;             Encoding:DTB_TYPE_U32),
  (Name:DTB_PROPERTY_I_CACHE_BLOCK_SIZE;       Encoding:DTB_TYPE_U32),
  (Name:DTB_PROPERTY_I_CACHE_LINE_SIZE;        Encoding:DTB_TYPE_U32),
  (Name:DTB_PROPERTY_D_CACHE_SIZE;             Encoding:DTB_TYPE_U32),
  (Name:DTB_PROPERTY_D_CACHE_SETS;             Encoding:DTB_TYPE_U32),
  (Name:DTB_PROPERTY_D_CACHE_BLOCK_SIZE;       Encoding:DTB_TYPE_U32),
  (Name:DTB_PROPERTY_D_CACHE_LINE_SIZE;        Encoding:DTB_TYPE_U32),
  (Name:DTB_PROPERTY_NEXT_LEVEL_CACHE;         Encoding:DTB_TYPE_PHANDLE),
  
  (Name:DTB_PROPERTY_CACHE_LEVEL;              Encoding:DTB_TYPE_U32),
  {Other Additional Properties}
  (Name:DTB_PROPERTY_SERIAL_NUMBER;            Encoding:DTB_TYPE_STRING),
  (Name:DTB_PROPERTY_ALLOC_RANGES;             Encoding:DTB_TYPE_ENCODED_ARRAY),
  (Name:DTB_PROPERTY_RESETS;                   Encoding:DTB_TYPE_ENCODED_ARRAY),
  (Name:DTB_PROPERTY_CLOCK_CELLS;              Encoding:DTB_TYPE_U32),
  (Name:DTB_PROPERTY_LINUX_INITRD_START;       Encoding:DTB_TYPE_U32),
  (Name:DTB_PROPERTY_LINUX_INITRD_END;         Encoding:DTB_TYPE_U32)
  
 );
{$ENDIF DEVICE_TREE_ENUMERATION}
 
{==============================================================================}
{Initialization Functions}
procedure DeviceTreeInit;
 
{==============================================================================}
{Device Tree Functions}
function DeviceTreeValidate(Address:PtrUInt;var Size:LongWord):Boolean;

function DeviceTreeNextNode(Parent,Previous:THandle):THandle;
function DeviceTreeNextProperty(Node,Previous:THandle):THandle;

function DeviceTreeGetNode(const Path:String;Parent:THandle):THandle;
function DeviceTreeGetProperty(Node:THandle;const Name:String):THandle;

function DeviceTreeGetNodeName(Handle:THandle):String;
procedure DeviceTreeSplitNodeName(Handle:THandle;var NodeName,UnitAddress:String);

function DeviceTreeGetNodeParent(Handle:THandle):THandle;
function DeviceTreeGetNodeRegCells(Handle:THandle;var Address,Size:LongWord):Boolean;
function DeviceTreeGetNodeRangeCells(Handle:THandle;var ParentAddress,NodeAddress,NodeSize:LongWord):Boolean;

function DeviceTreeGetPropertyName(Handle:THandle):String;
procedure DeviceTreeSplitPropertyName(Handle:THandle;var UniquePrefix,PropertyName:String);

function DeviceTreeGetPropertyValue(Handle:THandle):Pointer;
function DeviceTreeGetPropertyLength(Handle:THandle):LongWord;

function DeviceTreeGetPropertyString(Handle:THandle):String;
function DeviceTreeGetPropertyLongWord(Handle:THandle):LongWord;
function DeviceTreeGetPropertyQuadWord(Handle:THandle):UInt64;

{==============================================================================}
{RTL Device Tree Functions}
function SysDeviceTreeRead(const Path,Name:String;Buffer:Pointer;var Size:LongWord):LongWord;
function SysDeviceTreeRead32(const Path,Name:String;var Value:LongWord):LongWord;
function SysDeviceTreeRead64(const Path,Name:String;var Value:UInt64):LongWord;
function SysDeviceTreeReadString(const Path,Name:String;var Value:String):LongWord;

{==============================================================================}
{Device Tree Helper Functions}
function DeviceTreeGetBootArgs:PChar;
function DeviceTreeGetRamdisk(var Address:PtrUInt;var Size:UInt64):Boolean;
function DeviceTreeGetMemory(Index:LongWord;{$IFDEF CPU32}var Range:LongWord;{$ENDIF CPU32}var Address:PtrUInt;var Size:UInt64):Boolean;
function DeviceTreeGetReservation(Index:LongWord;var Address:PtrUInt;var Size:UInt64):Boolean;
 
{$IFDEF DEVICE_TREE_ENUMERATION} 
function DeviceTreeLogTree:LongWord;
function DeviceTreeLogTreeEx(Node:THandle;Output:TDTBLogOutput;Decode:TDTBDecodeValue;Data:Pointer):LongWord;
{$ENDIF DEVICE_TREE_ENUMERATION}
{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Device Tree specific variables}
 DeviceTreeInitialized:Boolean;
 
 DeviceTreeRootNode:THandle = INVALID_HANDLE_VALUE;
 DeviceTreeChosenNode:THandle = INVALID_HANDLE_VALUE;
 DeviceTreeMemoryNodes:array[0..3] of THandle = (INVALID_HANDLE_VALUE, INVALID_HANDLE_VALUE, INVALID_HANDLE_VALUE, INVALID_HANDLE_VALUE);

{==============================================================================}
{==============================================================================}
{Forward Declarations}
function DeviceTreeIsValid:Boolean; forward;
function DeviceTreeGetRootNode:THandle; forward;
function DeviceTreeGetChosenNode:THandle; forward;
function DeviceTreeGetMemoryNode(Index:LongWord):THandle; forward;

{$IFDEF DEVICE_TREE_ENUMERATION}
procedure DeviceTreeLogOutput(const AText:String;Data:Pointer); forward;

function DeviceTreeGetValue(Node,Handle:THandle;Decode:TDTBDecodeValue;Data:Pointer):String; forward;
function DeviceTreeDecodeValue(Node,Handle:THandle;Value:Pointer;Size:LongWord;Data:Pointer):String; forward;

procedure DeviceTreeEnumerateTree(Parent:THandle;Level:LongWord;Flat:Boolean;Output:TDTBLogOutput;Decode:TDTBDecodeValue;Data:Pointer); forward;
{$ENDIF DEVICE_TREE_ENUMERATION}

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure DeviceTreeInit;
{Initialize the Device Tree unit}

{Note: Called only during system startup}
begin
 {}
 {Check Initialized}
 if DeviceTreeInitialized then Exit;

 {Register Platform Device Tree Handlers}
 DeviceTreeReadHandler:=SysDeviceTreeRead;
 DeviceTreeRead32Handler:=SysDeviceTreeRead32;
 DeviceTreeRead64Handler:=SysDeviceTreeRead64;
 DeviceTreeReadStringHandler:=SysDeviceTreeReadString;

 {$IFNDEF DEVICE_TREE_ENABLE}
 {Validate the Device Tree address (If available)}
 {Note: Assumes that the platform architecture module will set DEVICE_TREE_BASE while checking for boot tags}
 DEVICE_TREE_VALID:=DeviceTreeValidate(DEVICE_TREE_BASE,DEVICE_TREE_SIZE);
 {$ENDIF}

 DeviceTreeInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Device Tree Functions}
function DeviceTreeValidate(Address:PtrUInt;var Size:LongWord):Boolean;
{Check the data at the supplied address to determine if it is a valid Device Tree Blob}
{Address: The address to be validated}
{Size: On return contains the total size of the Device Tree Blob if valid}
{Return: True if the address points to a valid Device Tree Blob, False if not}

{Note: Does not overwrite passed Size unless the DTB is valid}
var
 Version:LongWord;
 DTBHeader:PDTBHeader;
begin
 {}
 Result:=False;
  
 {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('DeviceTree Validate (Address=' + AddrToHex(Address) + ')');
 {$ENDIF}
 
 {Check Address}
 if Address = 0 then Exit;
 
 {Get Header}
 DTBHeader:=PDTBHeader(Address);
 
 {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
 if PLATFORM_LOG_ENABLED then
  begin
   PlatformLogDebug('DTB Header');
   PlatformLogDebug(' Magic = ' + IntToHex(LongWordBEtoN(DTBHeader.Magic),8));
   PlatformLogDebug(' Total Size = ' + IntToStr(LongWordBEtoN(DTBHeader.TotalSize)));
   PlatformLogDebug(' Structure Offset = ' + IntToStr(LongWordBEtoN(DTBHeader.StructureOffset)));
   PlatformLogDebug(' Strings Offset = ' + IntToStr(LongWordBEtoN(DTBHeader.StringsOffset)));
   PlatformLogDebug(' Reservation Offset = ' + IntToStr(LongWordBEtoN(DTBHeader.ReservationOffset)));
   PlatformLogDebug(' Version = ' + IntToStr(LongWordBEtoN(DTBHeader.Version)));
   PlatformLogDebug(' Compatible Version = ' + IntToStr(LongWordBEtoN(DTBHeader.CompatibleVersion)));
   PlatformLogDebug(' Boot CPUID = ' + IntToStr(LongWordBEtoN(DTBHeader.BootCPUID)));
   PlatformLogDebug(' Strings Size = ' + IntToStr(LongWordBEtoN(DTBHeader.StringsSize)));
   PlatformLogDebug(' Structure Size = ' + IntToStr(LongWordBEtoN(DTBHeader.StructureSize)));
  end; 
 {$ENDIF}
 
 {Check Signature}
 if LongWordBEtoN(DTBHeader.Magic) <> DTB_MAGIC then Exit;
 
 {Check Version}
 Version:=LongWordBEtoN(DTBHeader.CompatibleVersion);
 if (Version <> DTB_VERSION_CURRENT) and (Version <> DTB_VERSION_COMPATIBLE) then Exit;
 
 {Return Size}
 Size:=LongWordBEtoN(DTBHeader.TotalSize);
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function DeviceTreeNextNode(Parent,Previous:THandle):THandle;
{Find the next DTB node within the Device Tree}
{Parent: Handle of the parent node to search in, INVALID_HANDLE_VALUE to search the entire tree}
{Previous: Handle of the node to start from, INVALID_HANDLE_VALUE to start from the root node}
{Return: The handle of the next node in the tree or INVALID_HANDLE_VALUE if no node was found}
var
 Len:LongWord;
 Token:LongWord;
 Level:LongWord;
 Offset:PtrUInt;
 Maximum:PtrUInt;
 DTBNode:PDTBNode;
 DTBHeader:PDTBHeader;
 DTBProperty:PDTBProperty;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;
 
 {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('DeviceTree Next Node (Parent=' + HandleToHex(Parent) + ' Previous=' + HandleToHex(Previous) + ')');
 {$ENDIF}
 
 {Check Valid}
 if not DeviceTreeIsValid then Exit;
 
 {Check Previous}
 if Previous = 0 then Previous:=INVALID_HANDLE_VALUE;
 
 {Check Parent}
 if Parent = 0 then Parent:=INVALID_HANDLE_VALUE;
 
 {Get Header}
 DTBHeader:=PDTBHeader(DEVICE_TREE_BASE);
 
 {Get Start}
 if Previous = INVALID_HANDLE_VALUE then
  begin
   if Parent = INVALID_HANDLE_VALUE then
    begin
     {Use Header}
     Offset:=LongWordBEtoN(DTBHeader.StructureOffset);
    end
   else
    begin
     {Use Parent}
     Offset:=Parent;

     {Set Previous}
     Previous:=Parent;
    end;
  end
 else
  begin
   if Parent = INVALID_HANDLE_VALUE then
    begin
     {Use Previous}
     Offset:=Previous;
    end
   else
    begin
     {Use Parent}
     Offset:=Parent;
    end;
  end;  

 {Get End}
 Maximum:=LongWordBEtoN(DTBHeader.StructureOffset) + LongWordBEtoN(DTBHeader.StructureSize);
 
 {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('DeviceTree Next Node (Offset=' + IntToHex(Offset,8) + ' Maximum=' + IntToHex(Maximum,8) + ')');
 {$ENDIF}
 
 {Get Token}
 Token:=LongWordBEtoN(PLongWord(DEVICE_TREE_BASE + Offset)^);
 
 {Check Token}
 while Token = DTB_NOP do
  begin
   {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
   if PLATFORM_LOG_ENABLED then PlatformLogDebug('DTB_NOP at ' + IntToHex(Offset,8));
   {$ENDIF}

   {Next Token}
   Inc(Offset,SizeOf(LongWord));
   
   {Check Offset}
   if Offset >= Maximum then Exit;
  
   {Get Token}
   Token:=LongWordBEtoN(PLongWord(DEVICE_TREE_BASE + Offset)^);
  end;
 
 {Check Token}
 if Token <> DTB_BEGIN_NODE then Exit;
 
 {Set Level}
 Level:=0;
 
 {Find Node}
 while Offset < Maximum do
  begin
   {Get Token}
   Token:=LongWordBEtoN(PLongWord(DEVICE_TREE_BASE + Offset)^);
   
   {Check Token}
   if Token = DTB_NOP then
    begin
     {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
     if PLATFORM_LOG_ENABLED then PlatformLogDebug('DTB_NOP at ' + IntToHex(Offset,8));
     {$ENDIF}

     {Next Token}
     Inc(Offset,SizeOf(LongWord));
    end
   else if Token = DTB_BEGIN_NODE then
    begin
     {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
     if PLATFORM_LOG_ENABLED then PlatformLogDebug('DTB_BEGIN_NODE at ' + IntToHex(Offset,8));
     {$ENDIF}
     
     {Check Parent}
     if Parent <> INVALID_HANDLE_VALUE then
      begin
       {Increment Level}
       Inc(Level);
       
       {Check Previous and Level}
       if (Previous = INVALID_HANDLE_VALUE) and (Level = 2) then
        begin
         Result:=Offset;
         Break;
        end;
       
       {Check / Reset Previous}
       if Previous = Offset then Previous:=INVALID_HANDLE_VALUE;
      end
     else
      begin
       {Check Previous}
       if Previous = INVALID_HANDLE_VALUE then
        begin
         Result:=Offset;
         Break;
        end;
        
       {Reset Previous}
       Previous:=INVALID_HANDLE_VALUE;
      end;
     
     {Skip Node}
     DTBNode:=PDTBNode(DEVICE_TREE_BASE + Offset);
     Len:=0;
     while Ord(DTBNode.Name[Len]) <> 0 do
      begin
       Inc(Len);
      end;
     {Plus null Terminator} 
     Inc(Len);
     
     {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
     if PLATFORM_LOG_ENABLED then PlatformLogDebug('Name Len=' + IntToStr(Len));
     {$ENDIF}
     
     {Next Token}
     Inc(Offset,DTB_NODE_OFFSET + ((Len + 3) and not(3)));
     
     {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
     if PLATFORM_LOG_ENABLED then PlatformLogDebug('Next Offset=' + IntToHex(Offset,8));
     {$ENDIF}
    end
   else if Token = DTB_END_NODE then
    begin
     {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
     if PLATFORM_LOG_ENABLED then PlatformLogDebug('DTB_END_NODE at ' + IntToHex(Offset,8));
     {$ENDIF}
     
     {Check Parent}
     if Parent <> INVALID_HANDLE_VALUE then
      begin
       {Decrement Level}
       Dec(Level);
       
       {Check Level}
       if Level = 0 then Break;
      end;
     
     {Next Token}
     Inc(Offset,SizeOf(LongWord));
    end
   else if Token = DTB_PROP then
    begin
     {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
     if PLATFORM_LOG_ENABLED then PlatformLogDebug('DTB_PROP at ' + IntToHex(Offset,8));
     {$ENDIF}
     
     {Skip Property}
     DTBProperty:=PDTBProperty(DEVICE_TREE_BASE + Offset);
    
     {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
     if PLATFORM_LOG_ENABLED then PlatformLogDebug('ValueLength=' + IntToStr(LongWordBEtoN(DTBProperty.ValueLength)));
     if PLATFORM_LOG_ENABLED then PlatformLogDebug('NameOffset=' + IntToStr(LongWordBEtoN(DTBProperty.NameOffset)));
     {$ENDIF}
    
     {Next Token}
     Inc(Offset,DTB_PROPERTY_OFFSET + ((LongWordBEtoN(DTBProperty.ValueLength) + 3) and not(3)));
     
     {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
     if PLATFORM_LOG_ENABLED then PlatformLogDebug('Next Offset=' + IntToHex(Offset,8));
     {$ENDIF}
    end
   else if Token = DTB_END then
    begin   
     {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
     if PLATFORM_LOG_ENABLED then PlatformLogDebug('DTB_END at ' + IntToHex(Offset,8));
     {$ENDIF}
     
     {End of Tree}
     Break;
    end
   else
    begin
     {Invalid Token}
     Break;
    end;
  end;  
  
 {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('DeviceTree Next Node (Result=' + HandleToHex(Result) + ')');
 {$ENDIF}
end;

{==============================================================================}

function DeviceTreeNextProperty(Node,Previous:THandle):THandle;
{Find the next DTB property within the specified node of the Device Tree}
{Node: Handle of the node to search in}
{Previous: Handle of the property to start from, INVALID_HANDLE_VALUE to start from the first property}
{Return: The handle of the next property in the node or INVALID_HANDLE_VALUE if no property was found}
var
 Len:LongWord;
 Token:LongWord;
 Level:LongWord;
 Offset:PtrUInt;
 Maximum:PtrUInt;
 DTBNode:PDTBNode;
 DTBHeader:PDTBHeader;
 DTBProperty:PDTBProperty;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;
 
 {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('DeviceTree Next Property (Node=' + HandleToHex(Node) + ' Previous=' + HandleToHex(Previous) + ')');
 {$ENDIF}
 
 {Check Valid}
 if not DeviceTreeIsValid then Exit;
 
 {Check Node}
 if (Node = 0) or (Node = INVALID_HANDLE_VALUE) then Exit;
 
 {Check Previous}
 if Previous = 0 then Previous:=INVALID_HANDLE_VALUE;
 
 {Get Header}
 DTBHeader:=PDTBHeader(DEVICE_TREE_BASE);
 
 {Get Start}
 if Previous = INVALID_HANDLE_VALUE then
  begin
   {Use Node}
   Offset:=Node;
  end
 else
  begin
   {Use Previous}
   Offset:=Previous;
  end; 
 
 {Get End}
 Maximum:=LongWordBEtoN(DTBHeader.StructureOffset) + LongWordBEtoN(DTBHeader.StructureSize);
 
 {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('DeviceTree Next Property (Offset=' + IntToHex(Offset,8) + ' Maximum=' + IntToHex(Maximum,8) + ')');
 {$ENDIF}
 
 {Get Token}
 Token:=LongWordBEtoN(PLongWord(DEVICE_TREE_BASE + Offset)^);
 
 {Check Token}
 while Token = DTB_NOP do
  begin
   {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
   if PLATFORM_LOG_ENABLED then PlatformLogDebug('DTB_NOP at ' + IntToHex(Offset,8));
   {$ENDIF}

   {Next Token}
   Inc(Offset,SizeOf(LongWord));
   
   {Check Offset}
   if Offset >= Maximum then Exit;
  
   {Get Token}
   Token:=LongWordBEtoN(PLongWord(DEVICE_TREE_BASE + Offset)^);
  end;
 
 {Check Previous}
 if Previous = INVALID_HANDLE_VALUE then
  begin
   {Check Token}
   if Token <> DTB_BEGIN_NODE then Exit;
   
   {Set Level}
   Level:=0;
  end
 else
  begin
   {Check Token}
   if Token <> DTB_PROP then Exit;

   {Set Level}
   Level:=1;
  end;
  
 {Find Node}
 while Offset < Maximum do
  begin
   {Get Token}
   Token:=LongWordBEtoN(PLongWord(DEVICE_TREE_BASE + Offset)^);

   {Check Token}
   if Token = DTB_NOP then
    begin
     {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
     if PLATFORM_LOG_ENABLED then PlatformLogDebug('DTB_NOP at ' + IntToHex(Offset,8));
     {$ENDIF}

     {Next Token}
     Inc(Offset,SizeOf(LongWord));
    end
   else if Token = DTB_BEGIN_NODE then
    begin
     {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
     if PLATFORM_LOG_ENABLED then PlatformLogDebug('DTB_BEGIN_NODE at ' + IntToHex(Offset,8));
     {$ENDIF}

     {Increment Level}
     Inc(Level);
     
     {Check Level}
     if Level > 1 then Break;
     
     {Skip Node}
     DTBNode:=PDTBNode(DEVICE_TREE_BASE + Offset);
     Len:=0;
     while Ord(DTBNode.Name[Len]) <> 0 do
      begin
       Inc(Len);
      end;
     {Plus null Terminator} 
     Inc(Len);
     
     {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
     if PLATFORM_LOG_ENABLED then PlatformLogDebug('Name Len=' + IntToStr(Len));
     {$ENDIF}
     
     {Next Token}
     Inc(Offset,DTB_NODE_OFFSET + ((Len + 3) and not(3)));
     
     {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
     if PLATFORM_LOG_ENABLED then PlatformLogDebug('Next Offset=' + IntToHex(Offset,8));
     {$ENDIF}
    end
   else if Token = DTB_END_NODE then
    begin
     {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
     if PLATFORM_LOG_ENABLED then PlatformLogDebug('DTB_END_NODE at ' + IntToHex(Offset,8));
     {$ENDIF}
     
     {End of Node}
     Break;
    end
   else if Token = DTB_PROP then
    begin
     {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
     if PLATFORM_LOG_ENABLED then PlatformLogDebug('DTB_PROP at ' + IntToHex(Offset,8));
     {$ENDIF}
     
     {Check Previous}
     if Previous = INVALID_HANDLE_VALUE then
      begin
       Result:=Offset;
       Break;
      end;
     
     {Reset Previous}
     Previous:=INVALID_HANDLE_VALUE;
     
     {Skip Property}
     DTBProperty:=PDTBProperty(DEVICE_TREE_BASE + Offset);
    
     {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
     if PLATFORM_LOG_ENABLED then PlatformLogDebug('ValueLength=' + IntToStr(LongWordBEtoN(DTBProperty.ValueLength)));
     if PLATFORM_LOG_ENABLED then PlatformLogDebug('NameOffset=' + IntToStr(LongWordBEtoN(DTBProperty.NameOffset)));
     {$ENDIF}
    
     {Next Token}
     Inc(Offset,DTB_PROPERTY_OFFSET + ((LongWordBEtoN(DTBProperty.ValueLength) + 3) and not(3)));
     
     {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
     if PLATFORM_LOG_ENABLED then PlatformLogDebug('Next Offset=' + IntToHex(Offset,8));
     {$ENDIF}
    end
   else if Token = DTB_END then
    begin   
     {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
     if PLATFORM_LOG_ENABLED then PlatformLogDebug('DTB_END at ' + IntToHex(Offset,8));
     {$ENDIF}
     
     {End of Tree}
     Break;
    end
   else
    begin
     {Invalid Token}
     Break;
    end;
  end;  
 
 {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('DeviceTree Next Property (Result=' + HandleToHex(Result) + ')');
 {$ENDIF}
end;

{==============================================================================}

function DeviceTreeGetNode(const Path:String;Parent:THandle):THandle;
{Get the handle of the node matching the specified path, optionally within a specified parent}
{Path: The path of the node to find, relative to parent node or fully qualified if parent not specified (eg /chosen or /cpus/cpu0)}
{Parent: Handle of the parent node to search in, INVALID_HANDLE_VALUE to search the entire tree}
{Return: The handle of the node which matches the path or INVALID_HANDLE_VALUE if no node was found}
var
 Name:String;
 Remain:String;
 Index:LongWord;
 NextNode:THandle;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;
 
 {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('DeviceTree Get Node (Path=' + Path + ' Parent=' + HandleToHex(Parent) + ')');
 {$ENDIF}

 {Check Path}
 if Length(Path) = 0 then Exit;
 
 {Check Parent}
 if Parent = 0 then Parent:=INVALID_HANDLE_VALUE;
 
 {Check Parent}
 if Parent = INVALID_HANDLE_VALUE then
  begin
   {Check Path (Must be full qualified)}
   if Path[1] <> DTB_PATH_SEPARATOR then Exit;
   
   {Get Root}
   Parent:=DeviceTreeGetRootNode;
   if Parent = INVALID_HANDLE_VALUE then Exit;
  end;

 {Get Path}
 Remain:=Path;
 
 {Remove Leading Slash}
 if Remain[1] = DTB_PATH_SEPARATOR then Delete(Remain,1,1);

 {Get First Name}
 Index:=Pos(DTB_PATH_SEPARATOR,Remain);
 if Index > 0 then
  begin
   Name:=Copy(Remain,1,Index - 1);
   Delete(Remain,1,Index - 1);
  end
 else
  begin
   Name:=Remain;
   Remain:='';
  end;

 {Get First Node}
 NextNode:=DeviceTreeNextNode(Parent,INVALID_HANDLE_VALUE);
 while NextNode <> INVALID_HANDLE_VALUE do
  begin
   {Compare Node}
   if DeviceTreeGetNodeName(NextNode) = Name then
    begin
     if Length(Remain) > 0 then
      begin
       {Recurse to Next Level}
       Result:=DeviceTreeGetNode(Remain,NextNode);
      end
     else
      begin
       Result:=NextNode;
      end;
     
     Exit; 
    end;
    
   {Get Next Node}
   NextNode:=DeviceTreeNextNode(Parent,NextNode);
  end;
end;

{==============================================================================}

function DeviceTreeGetProperty(Node:THandle;const Name:String):THandle;
{Get the handle of the property matching the specified name}
{Node: Handle of the node to search in}
{Name: The name to the node to find (eg compatible)}
{Return: The handle of the property which matches the name or INVALID_HANDLE_VALUE if no property was found}
var
 NextProperty:THandle;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;
 
 {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('DeviceTree Get Property (Node=' + HandleToHex(Node) + ' Name=' + Name + ')');
 {$ENDIF}
 
 {Check Node}
 if (Node = 0) or (Node = INVALID_HANDLE_VALUE) then Exit;
 
 {Check Name}
 if Length(Name) = 0 then Exit;
 
 {Get First Property}
 NextProperty:=DeviceTreeNextProperty(Node,INVALID_HANDLE_VALUE);
 while NextProperty <> INVALID_HANDLE_VALUE do
  begin
   {Get Name}
   if DeviceTreeGetPropertyName(NextProperty) = Name then
    begin
     Result:=NextProperty;
     Exit;
    end;
    
   {Get Next Property}
   NextProperty:=DeviceTreeNextProperty(Node,NextProperty);
  end;
end;

{==============================================================================}

function DeviceTreeGetNodeName(Handle:THandle):String;
{Get the name of the specified node}
{Handle: The handle of the node to get the name of}
{Return: The name of the specified node or an empty string if the node was not valid}
var
 Name:PChar;
 DTBNode:PDTBNode;
begin
 {}
 Result:='';
 
 {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('DeviceTree Get Node Name (Handle=' + HandleToHex(Handle) + ')');
 {$ENDIF}
 
 {Check Valid}
 if not DeviceTreeIsValid then Exit;
 
 {Check Handle}
 if (Handle = 0) or (Handle = INVALID_HANDLE_VALUE) then Exit;
 
 {Get Node}
 DTBNode:=PDTBNode(DEVICE_TREE_BASE + PtrUInt(Handle));
 
 {Check Token}
 if LongWordBEtoN(DTBNode.Token) <> DTB_BEGIN_NODE then Exit;
 
 {Get Name}
 Name:=@DTBNode.Name[0];
 
 {Return Result}
 Result:=String(Name);
 
 {Check for Root}
 if Length(Result) = 0 then Result:=DTB_NODE_ROOT;
end;

{==============================================================================}

procedure DeviceTreeSplitNodeName(Handle:THandle;var NodeName,UnitAddress:String);
{Split the name of a node into node name and unit address}
{Handle: The handle of the node to split the name of}
{NodeName: The node name on return or an empty string if the node was not valid}
{UnitAddress: The unit address on return (If applicable)}
var
 Name:String;
 Index:LongWord;
begin
 {}
 {Set Defaults}
 NodeName:='';
 UnitAddress:='';
 
 {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('DeviceTree Split Node Name (Handle=' + HandleToHex(Handle) + ')');
 {$ENDIF}

 {Get Name}
 Name:=DeviceTreeGetNodeName(Handle);
 if Length(Name) > 0 then
  begin
   {Split Name}
   Index:=Pos('@',Name);
   if Index > 0 then
    begin
     NodeName:=Copy(Name,1,Index - 1);
     UnitAddress:=Copy(Name,Index + 1,Length(Name));
    end
   else
    begin
     NodeName:=Name;
    end;
  end; 
end;

{==============================================================================}

function DeviceTreeGetNodeParent(Handle:THandle):THandle;
{Get the parent node of the specified node}
{Handle: The handle of the node to get the parent of}
{Return: The handle of the parent node or INVALID_HANDLE_VALUE if the node was not valid}

 function FindParentNode(Current,Node:THandle):THandle;
 var
  Next:THandle;
 begin
  {}
  Result:=INVALID_HANDLE_VALUE;
  
  {Check Handles}
  if (Current = INVALID_HANDLE_VALUE) or (Node = INVALID_HANDLE_VALUE) then Exit;
  
  {Get First Node}
  Next:=DeviceTreeNextNode(Current,INVALID_HANDLE_VALUE);
  while Next <> INVALID_HANDLE_VALUE do
   begin
    {Check Node}
    if Next = Node then
     begin
      {Return Current}
      Result:=Current;
      Exit;
     end
    else 
     begin
      {Check Children}
      Result:=FindParentNode(Next,Node);
      if Result <> INVALID_HANDLE_VALUE then Exit;
     end;
    
    {Get Next Node}
    Next:=DeviceTreeNextNode(Current,Next);
   end;
 end;
 
var
 Root:THandle;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;
 
 {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('DeviceTree Get Node Parent (Handle=' + HandleToHex(Handle) + ')');
 {$ENDIF}
 
 {Check Handle}
 if (Handle = 0) or (Handle = INVALID_HANDLE_VALUE) then Exit;
 
 {Get Root Node}
 Root:=DeviceTreeGetRootNode;
 if Root = INVALID_HANDLE_VALUE then Exit;
 
 {Find the Parent Node}
 Result:=FindParentNode(Root,Handle);
end;

{==============================================================================}

function DeviceTreeGetNodeRegCells(Handle:THandle;var Address,Size:LongWord):Boolean;
{Get the #address-cells and #size-cells values that apply to reg properties of the specified node}
{Handle: The handle of the node to get the cell sizes for}
{Address: The #address-cells value on return (or the default value if not found)}
{Size: The #size-cells value on return (or the default value if not found)}
{Return: True if the cell sizes were found or False if the node was not valid}

{Note: The address and size values applicable to a given node will be those
       from a parent node, not those found in the node itself (if present)}
       
{Note: Used by early stage boot stage processing which must limit the use of strings
       and other memory allocations. This function uses a memory compare for names}
var
 Name:PChar;
 Parent:THandle;
 SizeFound:Boolean;
 AddressFound:Boolean;
 NextProperty:THandle;
 DTBHeader:PDTBHeader;
 DTBProperty:PDTBPropertyLongWord;
begin
 {}
 Result:=False;
 
 {Set Defaults (As per Device Tree Specification 0.2}
 Address:=2;
 Size:=1;
 
 {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('DeviceTree Get Node Reg Cells (Handle=' + HandleToHex(Handle) + ')');
 {$ENDIF}
 
 {Get Header}
 DTBHeader:=PDTBHeader(DEVICE_TREE_BASE);
 
 {Get Parent} 
 Parent:=DeviceTreeGetNodeParent(Handle);
 if Parent = INVALID_HANDLE_VALUE then Exit;

 {Get Start}
 AddressFound:=False;
 SizeFound:=False;

 Result:=True;

 {Get First Property}
 NextProperty:=DeviceTreeNextProperty(Parent,INVALID_HANDLE_VALUE);
 while NextProperty <> INVALID_HANDLE_VALUE do
  begin
   {Get Property}
   DTBProperty:=PDTBPropertyLongWord(DEVICE_TREE_BASE + PtrUInt(NextProperty));
     
   {Get Name}
   Name:=PChar(DEVICE_TREE_BASE + LongWordBEtoN(DTBHeader.StringsOffset) + LongWordBEtoN(DTBProperty.NameOffset));

   {Compare Name}
   if CompareMem(Name,@DTB_PROPERTY_ADDRESS_CELLS[1],Length(DTB_PROPERTY_ADDRESS_CELLS)) then
    begin
     {Get Address Cells}
     Address:=LongWordBEtoN(DTBProperty.Value[0]);
     AddressFound:=True;
    end
   else if CompareMem(Name,@DTB_PROPERTY_SIZE_CELLS[1],Length(DTB_PROPERTY_SIZE_CELLS)) then 
    begin
     {Get Size Cells}
     Size:=LongWordBEtoN(DTBProperty.Value[0]);
     SizeFound:=True;
    end;
     
   {Check Results} 
   if AddressFound and SizeFound then Exit;
  
   {Get Next Property}
   NextProperty:=DeviceTreeNextProperty(Parent,NextProperty);
  end;
end;

{==============================================================================}

function DeviceTreeGetNodeRangeCells(Handle:THandle;var ParentAddress,NodeAddress,NodeSize:LongWord):Boolean;
{Get the #address-cells and #size-cells values that apply to range properties of the specified node}
{Handle: The handle of the node to get the cell sizes for}
{ParentAddress: The #address-cells value from the parent on return (or the default value if not found)}
{NodeAddress: The #address-cells value from this node on return (or the default value if not found)}
{NodeSize: The #size-cells value from this node on return (or the default value if not found)}
{Return: True if the cell sizes were found or False if the node was not valid}

{Note: Range properties use the address value from the parent node and the address and size values
       from the node itself to determine the size of each range}
       
{Note: Used by early stage boot stage processing which must limit the use of strings
       and other memory allocations. This function uses a memory compare for names}
var
 Name:PChar;
 SizeFound:Boolean;
 AddressFound:Boolean;
 ParentSize:LongWord;
 NextProperty:THandle;
 DTBHeader:PDTBHeader;
 DTBProperty:PDTBPropertyLongWord;
begin
 {}
 Result:=False;
 
 {Set Defaults (As per Device Tree Specification 0.2}
 ParentAddress:=2;
 NodeAddress:=2;
 ParentSize:=1;
 NodeSize:=1;
 
 {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('DeviceTree Get Node Range Cells (Handle=' + HandleToHex(Handle) + ')');
 {$ENDIF}
 
 {Check Handle}
 if (Handle = 0) or (Handle = INVALID_HANDLE_VALUE) then Exit;
 
 {Get Header}
 DTBHeader:=PDTBHeader(DEVICE_TREE_BASE);
 
 {Get Start}
 AddressFound:=False;
 SizeFound:=False;
 
 {Get First Property}
 NextProperty:=DeviceTreeNextProperty(Handle,INVALID_HANDLE_VALUE);
 while NextProperty <> INVALID_HANDLE_VALUE do
  begin
   {Get Property}
   DTBProperty:=PDTBPropertyLongWord(DEVICE_TREE_BASE + PtrUInt(NextProperty));
     
   {Get Name}
   Name:=PChar(DEVICE_TREE_BASE + LongWordBEtoN(DTBHeader.StringsOffset) + LongWordBEtoN(DTBProperty.NameOffset));

   {Compare Name}
   if CompareMem(Name,@DTB_PROPERTY_ADDRESS_CELLS[1],Length(DTB_PROPERTY_ADDRESS_CELLS)) then
    begin
     {Get Node Address Cells}
     NodeAddress:=LongWordBEtoN(DTBProperty.Value[0]);
     AddressFound:=True;
    end
   else if CompareMem(Name,@DTB_PROPERTY_SIZE_CELLS[1],Length(DTB_PROPERTY_SIZE_CELLS)) then 
    begin
     {Get Node Size Cells}
     NodeSize:=LongWordBEtoN(DTBProperty.Value[0]);
     SizeFound:=True;
    end;
  
   {Check Results} 
   if AddressFound and SizeFound then Break;
  
   {Get Next Property}
   NextProperty:=DeviceTreeNextProperty(Handle,NextProperty);
  end;
  
 {Get Parent Address Cells} 
 Result:=DeviceTreeGetNodeRegCells(Handle,ParentAddress,ParentSize);
end;

{==============================================================================}

function DeviceTreeGetPropertyName(Handle:THandle):String;
{Get the name of the specified property}
{Handle: The handle of the property to get the name of}
{Return: The name of the specified property or an empty string if the property was not valid}
var
 Name:PChar;
 DTBHeader:PDTBHeader;
 DTBProperty:PDTBProperty;
begin
 {}
 Result:='';
 
 {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('DeviceTree Get Property Name (Handle=' + HandleToHex(Handle) + ')');
 {$ENDIF}
 
 {Check Valid}
 if not DeviceTreeIsValid then Exit;

 {Check Handle}
 if (Handle = 0) or (Handle = INVALID_HANDLE_VALUE) then Exit;
 
 {Get Header}
 DTBHeader:=PDTBHeader(DEVICE_TREE_BASE);
 
 {Get Property}
 DTBProperty:=PDTBProperty(DEVICE_TREE_BASE + PtrUInt(Handle));
 
 {Check Token}
 if LongWordBEtoN(DTBProperty.Token) <> DTB_PROP then Exit;
 
 {Get Name}
 Name:=PChar(DEVICE_TREE_BASE + LongWordBEtoN(DTBHeader.StringsOffset) + LongWordBEtoN(DTBProperty.NameOffset));
 
 {Return Result}
 Result:=String(Name);
end;

{==============================================================================}

procedure DeviceTreeSplitPropertyName(Handle:THandle;var UniquePrefix,PropertyName:String);
{Split the name of a property into property name and unique prefix}
{Handle: The handle of the property to split the name of}
{UniquePrefix: The unique prefix on return (If applicable)}
{PropertyName: The property name on return or an empty string if the property was not valid}
var
 Name:String;
 Index:LongWord;
begin
 {}
 {Set Defaults}
 UniquePrefix:='';
 PropertyName:='';
 
 {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('DeviceTree Split Property Name (Handle=' + HandleToHex(Handle) + ')');
 {$ENDIF}

 {Get Name}
 Name:=DeviceTreeGetPropertyName(Handle);
 if Length(Name) > 0 then
  begin
   {Split Name}
   Index:=Pos(',',Name);
   if Index > 0 then
    begin
     UniquePrefix:=Copy(Name,1,Index - 1);
     PropertyName:=Copy(Name,Index + 1,Length(Name));
    end
   else
    begin
     PropertyName:=Name;
    end;
  end; 
end;

{==============================================================================}

function DeviceTreeGetPropertyValue(Handle:THandle):Pointer;
{Get a pointer to the raw value of the specified property}
{Handle: The handle of the property to get the value of}
{Return: A pointer to the specified property value or nil if the property was not valid}

{Note: The returned value points to the memory block where device tree is stored and should not be modified or freed}
var
 DTBProperty:PDTBProperty;
begin
 {}
 Result:=nil;
 
 {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('DeviceTree Get Property Value (Handle=' + HandleToHex(Handle) + ')');
 {$ENDIF}
 
 {Check Valid}
 if not DeviceTreeIsValid then Exit;
 
 {Check Handle}
 if (Handle = 0) or (Handle = INVALID_HANDLE_VALUE) then Exit;
 
 {Get Property}
 DTBProperty:=PDTBProperty(DEVICE_TREE_BASE + PtrUInt(Handle));
 
 {Check Token}
 if LongWordBEtoN(DTBProperty.Token) <> DTB_PROP then Exit;
 
 {Return Result}
 Result:=@DTBProperty.Value[0];
end;

{==============================================================================}

function DeviceTreeGetPropertyLength(Handle:THandle):LongWord;
{Get the length of the raw value of the specified property}
{Handle: The handle of the property to get the value lenth of}
{Return: The length of the specified property value in bytes or -1 if the property was not valid}
var
 DTBProperty:PDTBProperty;
begin
 {}
 Result:=MAX_LONG;
 
 {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('DeviceTree Get Property Length (Handle=' + HandleToHex(Handle) + ')');
 {$ENDIF}
 
 {Check Valid}
 if not DeviceTreeIsValid then Exit;
 
 {Check Handle}
 if (Handle = 0) or (Handle = INVALID_HANDLE_VALUE) then Exit;
 
 {Get Property}
 DTBProperty:=PDTBProperty(DEVICE_TREE_BASE + PtrUInt(Handle));
 
 {Check Token}
 if LongWordBEtoN(DTBProperty.Token) <> DTB_PROP then Exit;
 
 {Return Result}
 Result:=LongWordBEtoN(DTBProperty.ValueLength);
end;

{==============================================================================}

function DeviceTreeGetPropertyString(Handle:THandle):String;
{Get the value of the specified property as a string}
{Handle: The handle of the property to get the value of}
{Return: A string representation of the value or an empty string if the property was not valid}
var
 Value:PChar;
 DTBProperty:PDTBPropertyChar;
begin
 {}
 Result:='';
 
 {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('DeviceTree Get Property String (Handle=' + HandleToHex(Handle) + ')');
 {$ENDIF}
 
 {Check Valid}
 if not DeviceTreeIsValid then Exit;
 
 {Check Handle}
 if (Handle = 0) or (Handle = INVALID_HANDLE_VALUE) then Exit;
 
 {Get Property}
 DTBProperty:=PDTBPropertyChar(DEVICE_TREE_BASE + PtrUInt(Handle));
 
 {Check Token}
 if LongWordBEtoN(DTBProperty.Token) <> DTB_PROP then Exit;

 {Check Null}
 if DTBProperty.Value[LongWordBEtoN(DTBProperty.ValueLength)] <> #0 then Exit;
 
 {Get Value}
 Value:=@DTBProperty.Value[0];
 
 {Return Result}
 Result:=String(Value);
end;

{==============================================================================}

function DeviceTreeGetPropertyLongWord(Handle:THandle):LongWord;
{Get the value of the specified property as a longword}
{Handle: The handle of the property to get the value of}
{Return: A longword representation of the value or 0 if the property was not valid}
var
 DTBProperty:PDTBPropertyLongWord;
begin
 {}
 Result:=0;
 
 {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('DeviceTree Get Property LongWord (Handle=' + HandleToHex(Handle) + ')');
 {$ENDIF}
 
 {Check Valid}
 if not DeviceTreeIsValid then Exit;
 
 {Check Handle}
 if (Handle = 0) or (Handle = INVALID_HANDLE_VALUE) then Exit;
 
 {Get Property}
 DTBProperty:=PDTBPropertyLongWord(DEVICE_TREE_BASE + PtrUInt(Handle));
 
 {Check Token}
 if LongWordBEtoN(DTBProperty.Token) <> DTB_PROP then Exit;
 
 {Check Size}
 if LongWordBEtoN(DTBProperty.ValueLength) < SizeOf(LongWord) then Exit;
 
 {Return Result}
 Result:=LongWordBEtoN(DTBProperty.Value[0]);
end;

{==============================================================================}

function DeviceTreeGetPropertyQuadWord(Handle:THandle):UInt64;
{Get the value of the specified property as a quadword}
{Handle: The handle of the property to get the value of}
{Return: A quadword representation of the value or 0 if the property was not valid}
var
 DTBProperty:PDTBPropertyQuadWord;
begin
 {}
 Result:=0;
 
 {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('DeviceTree Get Property QuadWord (Handle=' + HandleToHex(Handle) + ')');
 {$ENDIF}
 
 {Check Valid}
 if not DeviceTreeIsValid then Exit;
 
 {Check Handle}
 if (Handle = 0) or (Handle = INVALID_HANDLE_VALUE) then Exit;
 
 {Get Property}
 DTBProperty:=PDTBPropertyQuadWord(DEVICE_TREE_BASE + PtrUInt(Handle));
 
 {Check Token}
 if LongWordBEtoN(DTBProperty.Token) <> DTB_PROP then Exit;
 
 {Check Size}
 if LongWordBEtoN(DTBProperty.ValueLength) < SizeOf(UInt64) then Exit;
 
 {Return Result}
 Result:=Int64BEtoN(DTBProperty.Value[0]);
end;

{==============================================================================}
{==============================================================================}
{RTL Device Tree Functions}
function SysDeviceTreeRead(const Path,Name:String;Buffer:Pointer;var Size:LongWord):LongWord;
{Implementation of DeviceTreeRead API}

{Note: Not intended to be called directly by applications, use DeviceTreeRead instead}
var
 Len:LongWord;
 Value:Pointer;
 DTBNode:THandle;
 DTBProperty:THandle;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Get Node}
 DTBNode:=DeviceTreeGetNode(Path,INVALID_HANDLE_VALUE);
 if DTBNode = INVALID_HANDLE_VALUE then Exit;
 
 {Get Property}
 DTBProperty:=DeviceTreeGetProperty(DTBNode,Name);
 if DTBProperty = INVALID_HANDLE_VALUE then Exit;
 
 {Get Property Length}
 Len:=DeviceTreeGetPropertyLength(DTBProperty);
 if Len = MAX_LONG then Exit;
 if Len > Size then
  begin
   {Update Size}
   Size:=Len;
   
   Result:=ERROR_INSUFFICIENT_BUFFER;
   Exit;
  end
 else
  begin
   {Update Size}
   Size:=Len;
   
   {Check Length}
   if Len > 0 then
    begin
     {Get Property Value}
     Value:=DeviceTreeGetPropertyValue(DTBProperty);
     if Value = nil then Exit;
   
     {Copy Property Value}
     System.Move(Value^,Buffer^,Len);
    end; 
  end;
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function SysDeviceTreeRead32(const Path,Name:String;var Value:LongWord):LongWord;
{Implementation of DeviceTreeRead32 API}

{Note: Not intended to be called directly by applications, use DeviceTreeRead32 instead}
var
 DTBNode:THandle;
 DTBProperty:THandle;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Get Node}
 DTBNode:=DeviceTreeGetNode(Path,INVALID_HANDLE_VALUE);
 if DTBNode = INVALID_HANDLE_VALUE then Exit;
 
 {Get Property}
 DTBProperty:=DeviceTreeGetProperty(DTBNode,Name);
 if DTBProperty = INVALID_HANDLE_VALUE then Exit;

 {Get Property Value}
 Value:=DeviceTreeGetPropertyLongWord(DTBProperty);
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function SysDeviceTreeRead64(const Path,Name:String;var Value:UInt64):LongWord;
{Implementation of DeviceTreeRead64 API}

{Note: Not intended to be called directly by applications, use DeviceTreeRead64 instead}
var
 DTBNode:THandle;
 DTBProperty:THandle;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Get Node}
 DTBNode:=DeviceTreeGetNode(Path,INVALID_HANDLE_VALUE);
 if DTBNode = INVALID_HANDLE_VALUE then Exit;
 
 {Get Property}
 DTBProperty:=DeviceTreeGetProperty(DTBNode,Name);
 if DTBProperty = INVALID_HANDLE_VALUE then Exit;

 {Get Property Value}
 Value:=DeviceTreeGetPropertyQuadWord(DTBProperty);
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function SysDeviceTreeReadString(const Path,Name:String;var Value:String):LongWord;
{Implementation of DeviceTreeReadString API}

{Note: Not intended to be called directly by applications, use DeviceTreeReadString instead}
var
 DTBNode:THandle;
 DTBProperty:THandle;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Get Node}
 DTBNode:=DeviceTreeGetNode(Path,INVALID_HANDLE_VALUE);
 if DTBNode = INVALID_HANDLE_VALUE then Exit;
 
 {Get Property}
 DTBProperty:=DeviceTreeGetProperty(DTBNode,Name);
 if DTBProperty = INVALID_HANDLE_VALUE then Exit;

 {Get Property Value}
 Value:=DeviceTreeGetPropertyString(DTBProperty);
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}
{==============================================================================}
{Device Tree Helper Functions}
function DeviceTreeGetBootArgs:PChar;
{Return a character pointer to the location of the command line in the device tree blob}

{Note: Intended primarily for use by early boot stage processing which must limit the use of strings
       and other memory allocations. For normal use see DeviceTreeGetNode and DeviceTreeGetProperty}
var
 Name:PChar;
 Chosen:THandle;
 NextProperty:THandle;
 DTBHeader:PDTBHeader;
 DTBProperty:PDTBPropertyChar;
begin
 {}
 Result:=nil;
 
 {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('DeviceTree Get BootArgs');
 {$ENDIF}

 {Get Header}
 DTBHeader:=PDTBHeader(DEVICE_TREE_BASE);
 
 {Get Chosen Node}
 Chosen:=DeviceTreeGetChosenNode;
 if Chosen = INVALID_HANDLE_VALUE then Exit;
    
 {Get First Property}
 NextProperty:=DeviceTreeNextProperty(Chosen,INVALID_HANDLE_VALUE);
 while NextProperty <> INVALID_HANDLE_VALUE do
  begin
   {Get Property}
   DTBProperty:=PDTBPropertyChar(DEVICE_TREE_BASE + PtrUInt(NextProperty));
   
   {Get Name}
   Name:=PChar(DEVICE_TREE_BASE + LongWordBEtoN(DTBHeader.StringsOffset) + LongWordBEtoN(DTBProperty.NameOffset));
   
   {Compare Name}
   if CompareMem(Name,@DTB_PROPERTY_BOOTARGS[1],Length(DTB_PROPERTY_BOOTARGS)) then
    begin
     {Return Value}
     Result:=@DTBProperty.Value[0];
     Exit;
    end;
   
   {Get Next Property}
   NextProperty:=DeviceTreeNextProperty(Chosen,NextProperty);
  end;
end;

{==============================================================================}

function DeviceTreeGetRamdisk(var Address:PtrUInt;var Size:UInt64):Boolean;
{Return the address and size of the initial ram disk specified in the device tree blob}
{Address: Used to return the address value}
{Size: Used to return the size value}
{Return: True if an initial ram disk was specified in the device tree, False if not}

{Note: Intended primarily for use by early boot stage processing which must limit the use of strings
       and other memory allocations. For normal use see DeviceTreeGetNode and DeviceTreeGetProperty}
var
 Name:PChar;
 Chosen:THandle;
 NextProperty:THandle;
 DTBHeader:PDTBHeader;
 DTBProperty:PDTBPropertyLongWord;
begin
 {}
 Result:=False;
 
 {Set Defaults}
 Address:=0;
 Size:=0;
 
 {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('DeviceTree Get Ramdisk');
 {$ENDIF}
 
 {Get Header}
 DTBHeader:=PDTBHeader(DEVICE_TREE_BASE);
 
 {Get Chosen Node}
 Chosen:=DeviceTreeGetChosenNode;
 if Chosen = INVALID_HANDLE_VALUE then Exit;
 
 {Get First Property}
 NextProperty:=DeviceTreeNextProperty(Chosen,INVALID_HANDLE_VALUE);
 while NextProperty <> INVALID_HANDLE_VALUE do
  begin
   {Get Property}
   DTBProperty:=PDTBPropertyLongWord(DEVICE_TREE_BASE + PtrUInt(NextProperty));
   
   {Get Name}
   Name:=PChar(DEVICE_TREE_BASE + LongWordBEtoN(DTBHeader.StringsOffset) + LongWordBEtoN(DTBProperty.NameOffset));
   
   {Compare Name}
   if CompareMem(Name,@DTB_PROPERTY_INITRD_START[1],Length(DTB_PROPERTY_INITRD_START)) then
    begin
     Address:=LongWordBEtoN(DTBProperty.Value[0]);
    end
   else if CompareMem(Name,@DTB_PROPERTY_INITRD_END[1],Length(DTB_PROPERTY_INITRD_END)) then
    begin
     Size:=LongWordBEtoN(DTBProperty.Value[0]);
    end
   else if CompareMem(Name,@DTB_PROPERTY_LINUX_INITRD_START[1],Length(DTB_PROPERTY_LINUX_INITRD_START)) then 
    begin
     Address:=LongWordBEtoN(DTBProperty.Value[0]);
    end
   else if CompareMem(Name,@DTB_PROPERTY_LINUX_INITRD_END[1],Length(DTB_PROPERTY_LINUX_INITRD_END)) then
    begin
     Size:=LongWordBEtoN(DTBProperty.Value[0]);
    end;
    
   {Check Result} 
   if (Address <> 0) and (Size <> 0) then
    begin
     {Get Size from Start and End Address}
     if Size > Address then
      begin
       Size:=Size - Address;
      end; 
     
     Result:=True;
     Exit;
    end;
   
   {Get Next Property}
   NextProperty:=DeviceTreeNextProperty(Chosen,NextProperty);
  end;
end;

{==============================================================================}

function DeviceTreeGetMemory(Index:LongWord;{$IFDEF CPU32}var Range:LongWord;{$ENDIF CPU32}var Address:PtrUInt;var Size:UInt64):Boolean;
{Return the address and size of a memory block specified in the device tree blob}
{Index: the index of the memory block to return (The first block is 0)}
{Range: Used to return the page range value (If applicable)}
{Address: Used to return the address value}
{Size: Used to return the size value}
{Return: True if the memory block requested was found in the device tree, False if not}

{Note: Intended primarily for use by early boot stage processing which must limit the use of strings
       and other memory allocations. For normal use see DeviceTreeGetNode and DeviceTreeGetProperty}
       
 function GetValue(DTBProperty:PDTBPropertyLongWord;Offset,Cells:LongWord):UInt64;
 begin
  {}
  if Cells = 1 then
   begin
    Result:=LongWordBEtoN(DTBProperty.Value[Offset]);
   end
  else if Cells = 2 then
   begin
    Result:=LongWordBEtoN(DTBProperty.Value[Offset]);
    Result:=(Result shl 32) or LongWordBEtoN(DTBProperty.Value[Offset + 1]);
   end;
 end;
 
var
 Name:PChar;
 {$IFDEF CPU32}
 Value:UInt64;
 {$ENDIF CPU32}
 Count:LongWord;
 Total:LongWord;
 Offset:LongWord;
 Instance:LongWord;
 SizeCells:LongWord;
 AddressCells:LongWord;
 Memory:THandle;
 NextProperty:THandle;
 DTBHeader:PDTBHeader;
 DTBProperty:PDTBPropertyLongWord;
begin
 {}
 Result:=False;
 
 {Set Defaults}
 {$IFDEF CPU32}
 Range:=0;
 {$ENDIF CPU32}
 Address:=0;
 Size:=0;
 
 {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('DeviceTree Get Memory (Index=' + IntToStr(Index) + ')');
 {$ENDIF}
 
 {Get Header}
 DTBHeader:=PDTBHeader(DEVICE_TREE_BASE);
 
 {Get Start}
 Count:=0;
 Instance:=0;
 
 {Get Memory Node}
 Memory:=DeviceTreeGetMemoryNode(Instance);
 while Memory <> INVALID_HANDLE_VALUE do
  begin
   {Get First Property}
   NextProperty:=DeviceTreeNextProperty(Memory,INVALID_HANDLE_VALUE);
   while NextProperty <> INVALID_HANDLE_VALUE do
    begin
     {Get Property}
     DTBProperty:=PDTBPropertyLongWord(DEVICE_TREE_BASE + PtrUInt(NextProperty));
   
     {Get Name}
     Name:=PChar(DEVICE_TREE_BASE + LongWordBEtoN(DTBHeader.StringsOffset) + LongWordBEtoN(DTBProperty.NameOffset));
     
     {Compare Name}
     if CompareMem(Name,@DTB_PROPERTY_REG[1],Length(DTB_PROPERTY_REG)) then
      begin
       {Get Cell Sizes}
       if DeviceTreeGetNodeRegCells(Memory,AddressCells,SizeCells) then
        begin
         {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
         if PLATFORM_LOG_ENABLED then PlatformLogDebug('DeviceTree Get Memory (AddressCells=' + IntToStr(AddressCells) + ' SizeCells=' + IntToStr(SizeCells) + ')');
         {$ENDIF}
       
         {Check Values}
         Offset:=0;
         Total:=LongWordBEtoN(DTBProperty.ValueLength) div SizeOf(LongWord);
         while Offset < Total do
          begin
           {Check Count}
           if Count = Index then
            begin
             {$IFDEF CPU32}
             {Get Range and Address}
             Value:=GetValue(DTBProperty,Offset,AddressCells);
             Range:=Int64Rec(Value).Hi;
             Address:=Int64Rec(Value).Lo;
             {$ELSE CPU32}
             {Get Address}
             Address:=GetValue(DTBProperty,Offset,AddressCells);
             {$ENDIF CPU32}
             
             {Get Size}
             Size:=GetValue(DTBProperty,Offset + AddressCells,SizeCells);
             
             Result:=True;
             Exit;
            end;
           
           {Update Count and Offset}
           Inc(Count);
           Inc(Offset,AddressCells + SizeCells);
          end;
        end; 
       
       Break;
      end;
     
     {Get Next Property}
     NextProperty:=DeviceTreeNextProperty(Memory,NextProperty);
    end;
   
   Inc(Instance);
   
   {Get Memory Node}
   Memory:=DeviceTreeGetMemoryNode(Instance);
  end;
end;

{==============================================================================}

function DeviceTreeGetReservation(Index:LongWord;var Address:PtrUInt;var Size:UInt64):Boolean;
{Return the address and size of a memory reservation specified in the device tree blob}
{Index: the index of the memory reservation to return (The first reservation is 0)}
{Address: Used to return the address value}
{Size: Used to return the size value}
{Return: True if the memory reservation requested was found in the device tree, False if not}
var
 Count:LongWord;
 Offset:PtrUInt;
 DTBHeader:PDTBHeader;
 DTBReservation:PDTBReservation;
begin
 {}
 Result:=False;
 
 {Set Defaults}
 Address:=0;
 Size:=0;
 
 {$IF DEFINED(DEVICE_TREE_DEBUG) or DEFINED(PLATFORM_DEBUG)}
 if PLATFORM_LOG_ENABLED then PlatformLogDebug('DeviceTree Get Reservation (Index=' + IntToStr(Index) + ')');
 {$ENDIF}
 
 {Get Header}
 DTBHeader:=PDTBHeader(DEVICE_TREE_BASE);
 
 {Get Start}
 Count:=0;
 Offset:=LongWordBEtoN(DTBHeader.ReservationOffset);
 
 {Get First Reservation}
 DTBReservation:=PDTBReservation(DEVICE_TREE_BASE + Offset);
 while (Int64BEtoN(DTBReservation.Address) <> 0) and (Int64BEtoN(DTBReservation.Size) <> 0) do
  begin
   {Check Index}
   if Count = Index then
    begin
     Address:=Int64BEtoN(DTBReservation.Address);
     Size:=Int64BEtoN(DTBReservation.Size);
     
     Result:=True;
     Exit;
    end;
    
   Inc(Count);
   Inc(Offset,SizeOf(TDTBReservation));
   
   {Get Next Reservation}
   DTBReservation:=PDTBReservation(DEVICE_TREE_BASE + Offset);
  end;
end;

{==============================================================================}
{$IFDEF DEVICE_TREE_ENUMERATION}
function DeviceTreeLogTree:LongWord;
{Print information about all nodes and properties in the device tree}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=DeviceTreeLogTreeEx(INVALID_HANDLE_VALUE,DeviceTreeLogOutput,DeviceTreeDecodeValue,nil);
end;

{==============================================================================}

function DeviceTreeLogTreeEx(Node:THandle;Output:TDTBLogOutput;Decode:TDTBDecodeValue;Data:Pointer):LongWord;
{Print information about one or all nodes and properties in the device tree with custom value decode callback}
{Node: The node to print information about (INVALID_HANDLE_VALUE for all nodes)}
{Output: The log output callback to print information to (nil to use the default output)}
{Decode: The callback to decode a value into a string (nil to use the default decode}
{Data: A pointer to caller specific data which should be passed to the callbacks (Optional)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
  
 {Check Valid}
 if not DeviceTreeIsValid then Exit;

 {Check Node}
 if Node = 0 then Node:=INVALID_HANDLE_VALUE;

 {Check Output}
 if not Assigned(Output) then Output:=@DeviceTreeLogOutput;

 {Check Decode}
 if not Assigned(Decode) then Decode:=@DeviceTreeDecodeValue;

 {Enumerate Tree}
 DeviceTreeEnumerateTree(Node,0,False,Output,Decode,Data);
 
 Result:=ERROR_SUCCESS;
end;
{$ENDIF DEVICE_TREE_ENUMERATION}
{==============================================================================}
{==============================================================================}
{Device Tree Internal Functions}
function DeviceTreeIsValid:Boolean;
{Internal procedure to check if device tree is valid}
begin
 {}
 Result:=False;

 {Check Valid}
 if not DEVICE_TREE_VALID then Exit;
 
 {Check Signature}
 if LongWordBEtoN(PDTBHeader(DEVICE_TREE_BASE).Magic) <> DTB_MAGIC then Exit;
 
 Result:=True;
end;

{==============================================================================}

function DeviceTreeGetRootNode:THandle;
{Internal procedure to find the root node and cache it for later use}
begin
 {}
 Result:=DeviceTreeRootNode;
 if Result = INVALID_HANDLE_VALUE then
  begin
   {Get Root Node}
   DeviceTreeRootNode:=DeviceTreeNextNode(INVALID_HANDLE_VALUE,INVALID_HANDLE_VALUE);
   
   Result:=DeviceTreeRootNode;
  end;
end;

{==============================================================================}

function DeviceTreeGetChosenNode:THandle;
{Internal procedure to find the chosen node and cache it for later use}
var
 Root:THandle;
 NextNode:THandle;
 DTBNode:PDTBNode;
begin
 {}
 Result:=DeviceTreeChosenNode;
 if Result = INVALID_HANDLE_VALUE then
  begin
   {Get Root}
   Root:=DeviceTreeGetRootNode;
   if Root = INVALID_HANDLE_VALUE then Exit;
   
   {Get First Node}
   NextNode:=DeviceTreeNextNode(Root,INVALID_HANDLE_VALUE);
   while NextNode <> INVALID_HANDLE_VALUE do
    begin
     {Get Node}
     DTBNode:=PDTBNode(DEVICE_TREE_BASE + PtrUInt(NextNode));
     
     {Compare Name}
     if CompareMem(@DTBNode.Name[0],@DTB_NODE_CHOSEN[1],Length(DTB_NODE_CHOSEN)) then
      begin
       DeviceTreeChosenNode:=NextNode;
       
       Result:=DeviceTreeChosenNode;
       Break;
      end;
    
     {Get Next Node}
     NextNode:=DeviceTreeNextNode(Root,NextNode);
    end;
  end;
end;

{==============================================================================}

function DeviceTreeGetMemoryNode(Index:LongWord):THandle;
{Internal procedure to find the memory node specified by index and cache it for later use}

 function IsMemoryNode(Node:PDTBNode):Boolean;
 begin
  {}
  Result:=False;
  
  {Compare Name}
  if CompareMem(@Node.Name[0],@DTB_NODE_MEMORY[1],Length(DTB_NODE_MEMORY)) then
   begin
    {Check Next Character (null or @)}
    if (Node.Name[Length(DTB_NODE_MEMORY)] = #0) or (Node.Name[Length(DTB_NODE_MEMORY)] = #64) then
     begin
      Result:=True;
     end;
   end;
 end;
 
var
 Count:LongWord;
 Root:THandle;
 NextNode:THandle;
 DTBNode:PDTBNode;
begin
 {}
 if (Index <= 3) and (DeviceTreeMemoryNodes[Index] <> INVALID_HANDLE_VALUE) then
  begin
   Result:=DeviceTreeMemoryNodes[Index];
  end
 else 
  begin
   Result:=INVALID_HANDLE_VALUE;
   
   {Get Root}
   Root:=DeviceTreeGetRootNode;
   if Root = INVALID_HANDLE_VALUE then Exit;

   {Set Start}
   Count:=0;
   
   {Get First Node}
   NextNode:=DeviceTreeNextNode(Root,INVALID_HANDLE_VALUE);
   while NextNode <> INVALID_HANDLE_VALUE do
    begin
     {Get Node}
     DTBNode:=PDTBNode(DEVICE_TREE_BASE + PtrUInt(NextNode));
     
     {Check Name}
     if IsMemoryNode(DTBNode) then
      begin
       {Check Index}
       if Count = Index then
        begin
         if Index <= 3 then DeviceTreeMemoryNodes[Index]:=NextNode;

         Result:=NextNode;
         Break;
        end;
        
       Inc(Count); 
      end; 
     
     {Get Next Node}
     NextNode:=DeviceTreeNextNode(Root,NextNode);
    end;
  end;
end;

{==============================================================================}
{$IFDEF DEVICE_TREE_ENUMERATION}
procedure DeviceTreeLogOutput(const AText:String;Data:Pointer);
{Default log output procedure for DeviceTreeLogTree etc}
begin
 {}
 LoggingOutput(AText);
end;

{==============================================================================}

function DeviceTreeGetValue(Node,Handle:THandle;Decode:TDTBDecodeValue;Data:Pointer):String;
{Internal function to obtain the value of a property as a string, calling the decode callback if required}
var
 Value:Pointer;
 Size:LongWord;
begin
 {}
 Result:='';
 
 {Check Decode}
 if not Assigned(Decode) then Exit;
 
 {Check Node}
 if (Node = 0) or (Node = INVALID_HANDLE_VALUE) then Exit;

 {Check Handle}
 if (Handle = 0) or (Handle = INVALID_HANDLE_VALUE) then Exit;
 
 {Get Size}
 Size:=DeviceTreeGetPropertyLength(Handle);
 if Size = MAX_LONG then Exit;
 if Size > 0 then
  begin
   {Get Value}
   Value:=DeviceTreeGetPropertyValue(Handle);
   
   {Return Result}
   Result:=' = ' + Decode(Node,Handle,Value,Size,Data) + ';';
  end
 else
  begin
   {Return Result}
   Result:=';';
  end;  
end;

{==============================================================================}

function DeviceTreeDecodeValue(Node,Handle:THandle;Value:Pointer;Size:LongWord;Data:Pointer):String;
{Default decode value callback for DeviceTreeLogTree etc}

 function StripLeadingZeros(const Value:String):String;
 var
  Found:Boolean;
  Count:LongWord;
 begin
  {}
  Result:='';
  Found:=False;
  
  for Count:=1 to Length(Value) do
   begin
    if Found or (Value[Count] <>  '0') or (Count = Length(Value)) then
     begin
      Result:=Result + Value[Count];
      Found:=True;
     end; 
   end;
 end;
 
 function SplitQuadWordValue(Value:UInt64):String;
 begin
  {}
  Result:=StripLeadingZeros(IntToHex(Int64Rec(Value).Hi,8)) + ' 0x' + StripLeadingZeros(IntToHex(Int64Rec(Value).Lo,8));
 end;
 
 function DecodeStringList(Value:Pointer;Size:LongWord):String;
 var
  Count:LongWord;
  DTBProperty:PDTBPropertyChar;
 begin
  {}
  Result:='';
  
  if (Value = nil) or (Size = 0) then Exit;
  
  Result:='"';
  
  {Get Property}
  DTBProperty:=PDTBPropertyChar(PtrUInt(Value) - DTB_PROPERTY_OFFSET);
  
  {Get Chars}
  Count:=0;
  while Count < Size do
   begin
    if DTBProperty.Value[Count] <> #0 then
     begin
      Result:=Result + DTBProperty.Value[Count];
     end
    else
     begin
      if Count < (Size - 1) then Result:=Result + '", "';
     end; 
    
    Inc(Count);
   end;
  Result:=Result + '"';
 end;
 
 function DecodeByteString(Value:Pointer;Size:LongWord):String;
 var
  Count:LongWord;
  DTBProperty:PDTBProperty;
 begin
  {}
  Result:='';
  
  if (Value = nil) or (Size = 0) then Exit;
  
  Result:='[';
  
  {Get Property}
  DTBProperty:=PDTBProperty(PtrUInt(Value) - DTB_PROPERTY_OFFSET);
  
  {Get Bytes}
  Count:=0;
  while Count < Size do
   begin
    Result:=Result + IntToHex(DTBProperty.Value[Count],2);
    if Count < (Size - 1) then Result:=Result + ' ';
    
    Inc(Count);
   end;
  Result:=Result + ']';
 end;
 
 function DecodeEncodedArray(Value:Pointer;Size:LongWord):String;
 var
  Count:LongWord;
  Offset:LongWord;
  DTBProperty:PDTBPropertyLongWord;
 begin
  {}
  Result:='';
  
  if (Value = nil) or (Size = 0) then Exit;
  
  Result:='<';
  
  {Get Property}
  DTBProperty:=PDTBPropertyLongWord(PtrUInt(Value) - DTB_PROPERTY_OFFSET);
  
  {Get LongWords}
  Count:=0;
  Offset:=0;
  while Offset < Size do
   begin
    Result:=Result + '0x' + StripLeadingZeros(IntToHex(LongWordBEtoN(DTBProperty.Value[Count]),8));
    if Offset < (Size - SizeOf(LongWord)) then Result:=Result + ' ';
    
    Inc(Count);
    Inc(Offset,SizeOf(LongWord));
   end;
  Result:=Result + '>';
 end;
 
 function GuessPropertyType(Value:Pointer;Size:LongWord):LongWord;
 var
  Count:LongWord;
  HasPrintable:Boolean;
  DTBProperty:PDTBProperty;
 begin
  {}
  Result:=DTB_TYPE_UNKNOWN;
  
  if (Value = nil) or (Size = 0) then Exit;

  {Get Property}
  DTBProperty:=PDTBProperty(PtrUInt(Value) - DTB_PROPERTY_OFFSET);
  
  {Check for Null Terminator}
  if DTBProperty.Value[Size - 1] = 0 then
   begin
    {Check for Printable Characters}
    Count:=0;
    HasPrintable:=False;
    while Count < Size do
     begin
      {Check Zero or Printable}
      if (DTBProperty.Value[Count] <> 0) and ((DTBProperty.Value[Count] < 32) or (DTBProperty.Value[Count] > 127)) then Break;
      
      {Check Printable}
      HasPrintable:=HasPrintable or (DTBProperty.Value[Count] <> 0);
       
      Inc(Count);
     end;
   
    {Assume String List}
    if (Count = Size) and HasPrintable then
     begin
      Result:=DTB_TYPE_STRINGLIST;
      Exit;
     end;
   end;
  
  {Check for Multiple of LongWord}  
  if (Size mod SizeOf(LongWord)) = 0 then
   begin
    Result:=DTB_TYPE_ENCODED_ARRAY;
    Exit;
   end;
 end;
 
var
 Name:String;
 Count:LongWord;
 Encoding:LongWord;
begin
 {}
 Result:='';

 {Check Node}
 if (Node = 0) or (Node = INVALID_HANDLE_VALUE) then Exit;

 {Check Handle}
 if (Handle = 0) or (Handle = INVALID_HANDLE_VALUE) then Exit;
 
 {Set Default}
 Encoding:=DTB_TYPE_UNKNOWN;
 
 {Get Node Name}
 Name:=DeviceTreeGetNodeName(Node);
 if (Name = DTB_NODE_ALIASES) or (Name = DTB_NODE_SYMBOLS) then
  begin
   Encoding:=DTB_TYPE_STRING;
  end;
 
 {Check Encoding}
 if Encoding = DTB_TYPE_UNKNOWN then
  begin
   {Get Property Name}
   Name:=DeviceTreeGetPropertyName(Handle);
   
   {Check Property Types}
   for Count:=0 to DTB_MAX_PROPERTY_TYPE do
    begin
     if DTB_PROPERTY_TYPES[Count].Name = Name then
      begin
       Encoding:=DTB_PROPERTY_TYPES[Count].Encoding;
       Break;
      end;
    end;
  end;
  
 {Guess Property Type}
 if Encoding = DTB_TYPE_UNKNOWN then Encoding:=GuessPropertyType(Value,Size);
  
 {Check Encoding}
 case Encoding of
  DTB_TYPE_EMPTY:begin
    {Empty Value}
    Result:='';
   end;
  DTB_TYPE_U32,DTB_TYPE_PHANDLE:begin
    {LongWord Value}
    Result:='<0x' + StripLeadingZeros(IntToHex(DeviceTreeGetPropertyLongWord(Handle),8)) + '>';
   end;
  DTB_TYPE_U64:begin
    {QuadWord Value}
    Result:='<0x' + SplitQuadWordValue(DeviceTreeGetPropertyQuadWord(Handle)) + '>';
   end;
  DTB_TYPE_STRING:begin
    {String Value}
    Result:='"' + DeviceTreeGetPropertyString(Handle) + '"';
   end;
  DTB_TYPE_ENCODED_ARRAY:begin
    {Encoded Array Value}
    Result:=DecodeEncodedArray(Value,Size);
   end;
  DTB_TYPE_STRINGLIST:begin
    {String List Value}
    Result:=DecodeStringList(Value,Size);
   end;
 else
  begin
   {Unknown Value}
   Result:=DecodeByteString(Value,Size);
  end;
 end;
end;

{==============================================================================}

procedure DeviceTreeEnumerateTree(Parent:THandle;Level:LongWord;Flat:Boolean;Output:TDTBLogOutput;Decode:TDTBDecodeValue;Data:Pointer);
{Internal procedure to iterate through the device tree blob and output information about each node and property}
var
 Name:String;
 NextNode:THandle;
 NextProperty:THandle;
begin
 {}
 {Check Output}
 if not Assigned(Output) then Exit; 
 
 {Check Decode}
 if not Assigned(Decode) then Exit;
 
 {Get First Node}
 NextNode:=DeviceTreeNextNode(Parent,INVALID_HANDLE_VALUE);
 while NextNode <> INVALID_HANDLE_VALUE do
  begin
   Output('',Data);
   
   {Output Name}
   Name:=DeviceTreeGetNodeName(NextNode);
   Output(StringOfChar(' ',Level * 4) + Name + ' {',Data);
   
   {Get First Property}
   NextProperty:=DeviceTreeNextProperty(NextNode,INVALID_HANDLE_VALUE);
   while NextProperty <> INVALID_HANDLE_VALUE do
    begin
     {Output Name and Value}
     Name:=DeviceTreeGetPropertyName(NextProperty);
     Output(StringOfChar(' ',(Level + 1) * 4) + Name + DeviceTreeGetValue(NextNode,NextProperty,Decode,Data),Data);
     
     {Get Next Property}
     NextProperty:=DeviceTreeNextProperty(NextNode,NextProperty);
    end;
   
   {Check Flat Output}
   if Flat then
    begin
     {Get Next Node}
     NextNode:=DeviceTreeNextNode(Parent,NextNode);
     
     Output(StringOfChar(' ',Level * 4) + '};',Data);
    end
   else
    begin
     {Check Parent} 
     if Parent = INVALID_HANDLE_VALUE then
      begin
       {Recurse to next level}
       DeviceTreeEnumerateTree(NextNode,Level + 1,Flat,Output,Decode,Data);
     
       Output(StringOfChar(' ',Level * 4) + '};',Data);
     
       Break;
      end
     else
      begin
       {Recurse to next level}
       DeviceTreeEnumerateTree(NextNode,Level + 1,Flat,Output,Decode,Data);
     
       Output(StringOfChar(' ',Level * 4) + '};',Data);
     
       {Get Next Node}
       NextNode:=DeviceTreeNextNode(Parent,NextNode);
      end;
    end;  
  end;
end;
{$ENDIF DEVICE_TREE_ENUMERATION}
{==============================================================================}
{==============================================================================}

initialization
 DeviceTreeInit;

{==============================================================================}

{finalization}
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
