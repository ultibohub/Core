{
Ultibo Platform interface unit for ARMv7 LPAE.

Copyright (C) 2023 - SoftOz Pty Ltd.

Arch
====

 ARMv7 LPAE (Cortex A5/A7/A8/A9/A15/A17)
 ARMv8 LPAE (Cortex A53/A57/A72)

Boards
======

 Raspberry Pi 4 - Model B
 Raspberry Pi 400
 Raspberry Pi CM4

Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)

Credits
=======

 Information for this unit was obtained from:

  Linux - /arch/arm/mm/proc-v7-3level.S - Copyright 2001 Deep Blue Solutions Ltd

References
==========

 ARM Architecture Reference Manual (ARMv7-A and ARMv7-R edition)

 ARM Architecture Reference Manual ARMv8, for ARMv8-A architecture profile

Platform ARMv7 LPAE
===================

This unit extends the Platform ARMv7 unit to provide support for implementations using
the Large Physical Address Extensions (LPAE) to access greater than 4GB of memory in
32-bit environments.

The LPAE defines a new page table descriptor format along with new memory region attributes
and memory attribute indirection registers (MAIR0/1) as well as extending the TTBR0, TTBR1
and TTBCR registers with additional information.

The LPAE extensions allow addressing of up to 40 bits of physical address space (1TB) by
mapping it into the 32-bit virtual address space.

For 64-bit environments the Platform ARMv8 unit provides the equivalent functionality as
LPAE is not required and the processor can address up to 48 bits of address space directly.

All functionality not directly related to LPAE is provided from the Platform ARMv7 unit.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit PlatformARMv7L;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,PlatformARM,PlatformARMv7,HeapManager,Threads,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {ARMv7L specific constants common to all processor models}

 {Page Table Levels}
 ARMV7L_PAGE_TABLE_LEVELS = 3;

 {Page Table Shift}
 ARMV7L_PAGE_TABLES_SHIFT = 12;

 {Definitions of CP15 C2 (Memory Attribute Indirection Registers 0 and 1) bits in the system control processor registers}
 ARMV7L_CP15_C2_MAIR1_ATTR7_SHIFT = 24;
 ARMV7L_CP15_C2_MAIR1_ATTR7_MASK = ($FF shl ARMV7L_CP15_C2_MAIR1_ATTR7_SHIFT);
 ARMV7L_CP15_C2_MAIR1_ATTR6_SHIFT = 16;
 ARMV7L_CP15_C2_MAIR1_ATTR6_MASK = ($FF shl ARMV7L_CP15_C2_MAIR1_ATTR6_SHIFT);
 ARMV7L_CP15_C2_MAIR1_ATTR5_SHIFT = 8;
 ARMV7L_CP15_C2_MAIR1_ATTR5_MASK = ($FF shl ARMV7L_CP15_C2_MAIR1_ATTR5_SHIFT);
 ARMV7L_CP15_C2_MAIR1_ATTR4_SHIFT = 0;
 ARMV7L_CP15_C2_MAIR1_ATTR4_MASK = ($FF shl ARMV7L_CP15_C2_MAIR1_ATTR4_SHIFT);
 ARMV7L_CP15_C2_MAIR0_ATTR3_SHIFT = 24;
 ARMV7L_CP15_C2_MAIR0_ATTR3_MASK = ($FF shl ARMV7L_CP15_C2_MAIR0_ATTR3_SHIFT);
 ARMV7L_CP15_C2_MAIR0_ATTR2_SHIFT = 16;
 ARMV7L_CP15_C2_MAIR0_ATTR2_MASK = ($FF shl ARMV7L_CP15_C2_MAIR0_ATTR2_SHIFT);
 ARMV7L_CP15_C2_MAIR0_ATTR1_SHIFT = 8;
 ARMV7L_CP15_C2_MAIR0_ATTR1_MASK = ($FF shl ARMV7L_CP15_C2_MAIR0_ATTR1_SHIFT);
 ARMV7L_CP15_C2_MAIR0_ATTR0_SHIFT = 0;
 ARMV7L_CP15_C2_MAIR0_ATTR0_MASK = ($FF shl ARMV7L_CP15_C2_MAIR0_ATTR0_SHIFT);

 {Memory Attribute Indirection Registers (MAIR) values (See B4.1.104 of the ARM Architecture Reference Manual ARMv7-A and ARMv7-R edition)(These values are from Linux)}
 ARMV7L_CP15_C2_MAIR_STRONGLY_ORDERED      = $00; {Strongly-ordered memory}
 ARMV7L_CP15_C2_MAIR_DEVICE                = $04; {Device memory}
 ARMV7L_CP15_C2_MAIR_NORMAL_NONCACHED      = $44; {Normal memory, Inner Non-cacheable.}
 ARMV7L_CP15_C2_MAIR_NORMAL_WRITE_THROUGH  = $AA; {Normal memory, Inner Write-Through Cacheable}
 ARMV7L_CP15_C2_MAIR_NORMAL_WRITE_BACK     = $EE; {Normal memory, Inner Write-Back Cacheable}
 ARMV7L_CP15_C2_MAIR_NORMAL_WRITE_ALLOCATE = $FF; {Normal memory, Inner Write-Allocate Cacheable}

                             {NORMAL_WRITE_BACK, NORMAL_WRITE_THROUGH, NORMAL_NONCACHED, STRONGLY_ORDERED (0xEEAA4400)}
 ARMV7L_CP15_C2_MAIR0_MASK = ARMV7L_CP15_C2_MAIR_NORMAL_WRITE_BACK shl ARMV7L_CP15_C2_MAIR0_ATTR3_SHIFT
                          or ARMV7L_CP15_C2_MAIR_NORMAL_WRITE_THROUGH shl ARMV7L_CP15_C2_MAIR0_ATTR2_SHIFT
                          or ARMV7L_CP15_C2_MAIR_NORMAL_NONCACHED shl ARMV7L_CP15_C2_MAIR0_ATTR1_SHIFT
                          or ARMV7L_CP15_C2_MAIR_STRONGLY_ORDERED shl ARMV7L_CP15_C2_MAIR0_ATTR0_SHIFT;

                             {NORMAL_WRITE_ALLOCATE, UNUSED, UNUSED, DEVICE (0xFF000004)}
 ARMV7L_CP15_C2_MAIR1_MASK = ARMV7L_CP15_C2_MAIR_NORMAL_WRITE_ALLOCATE shl ARMV7L_CP15_C2_MAIR1_ATTR7_SHIFT
                          or ARMV7L_CP15_C2_MAIR_DEVICE shl ARMV7L_CP15_C2_MAIR1_ATTR4_SHIFT;

 {Definitions of CP15 C2 (Translation Table Base Control Register) bits in the system control processor registers}
 ARMV7L_CP15_C2_TTBCR_EAE                        = (1 shl 31); {Extended Address Enable. (0 Use the 32-bit translation system / 1 Use the 40-bit translation system)}
 ARMV7L_CP15_C2_TTBCR_SH1_MASK                   = (3 shl 28); {Shareability attribute for memory associated with translation table walks using TTBR1}
 ARMV7L_CP15_C2_TTBCR_SH1_NONSHAREABLE           = (0 shl 28); {Non-shareable}
 ARMV7L_CP15_C2_TTBCR_SH1_OUTER_SHAREABLE        = (2 shl 28); {Outer Shareable}
 ARMV7L_CP15_C2_TTBCR_SH1_INNER_SHAREABLE        = (3 shl 28); {Inner Shareable}
 ARMV7L_CP15_C2_TTBCR_ORGN1_MASK                 = (3 shl 26); {Outer cacheability attribute for memory associated with translation table walks using TTBR1}
 ARMV7L_CP15_C2_TTBCR_ORGN1_OUTER_NONCACHED      = (0 shl 26); {Normal memory, Outer Non-cacheable}
 ARMV7L_CP15_C2_TTBCR_ORGN1_OUTER_WRITE_ALLOCATE = (1 shl 26); {Normal memory, Outer Write-Back Write-Allocate Cacheable}
 ARMV7L_CP15_C2_TTBCR_ORGN1_OUTER_WRITE_THROUGH  = (2 shl 26); {Normal memory, Outer Write-Through Cacheable}
 ARMV7L_CP15_C2_TTBCR_ORGN1_OUTER_WRITE_BACK     = (3 shl 26); {Normal memory, Outer Write-Back no Write-Allocate Cacheable}
 ARMV7L_CP15_C2_TTBCR_IRGN1_MASK                 = (3 shl 24); {Inner cacheability attribute for memory associated with translation table walks using TTBR1}
 ARMV7L_CP15_C2_TTBCR_IRGN1_INNER_NONCACHED      = (0 shl 24); {Normal memory, Inner Non-cacheable}
 ARMV7L_CP15_C2_TTBCR_IRGN1_INNER_WRITE_ALLOCATE = (1 shl 24); {Normal memory, Inner Write-Back Write-Allocate Cacheable}
 ARMV7L_CP15_C2_TTBCR_IRGN1_INNER_WRITE_THROUGH  = (2 shl 24); {Normal memory, Inner Write-Through Cacheable}
 ARMV7L_CP15_C2_TTBCR_IRGN1_INNER_WRITE_BACK     = (3 shl 24); {Normal memory, Inner Write-Back no Write-Allocate Cacheable}
 ARMV7L_CP15_C2_TTBCR_EPD1                       = (1 shl 23); {Translation table walk disable for translations using TTBR1 (0 Perform translation table walks using TTBR1 / 1 A TLB miss on an address that is translated using TTBR1 generates a Translation fault)}
 ARMV7L_CP15_C2_TTBCR_A1                         = (1 shl 22); {Selects whether TTBR0 or TTBR1 defines the ASID (0 TTBR0.ASID defines the ASID / 1 TTBR1.ASID defines the ASID)}
 ARMV7L_CP15_C2_TTBCR_T1SZ_MASK                  = (7 shl 16); {The size offset of the memory region addressed by TTBR1}
 ARMV7L_CP15_C2_TTBCR_SH0_MASK                   = (3 shl 12); {Shareability attribute for memory associated with translation table walks using TTBR0}
 ARMV7L_CP15_C2_TTBCR_SH0_NONSHAREABLE           = (0 shl 12); {Non-shareable}
 ARMV7L_CP15_C2_TTBCR_SH0_OUTER_SHAREABLE        = (2 shl 12); {Outer Shareable}
 ARMV7L_CP15_C2_TTBCR_SH0_INNER_SHAREABLE        = (3 shl 12); {Inner Shareable}
 ARMV7L_CP15_C2_TTBCR_ORGN0_MASK                 = (3 shl 10); {Outer cacheability attribute for memory associated with translation table walks using TTBR0}
 ARMV7L_CP15_C2_TTBCR_ORGN0_OUTER_NONCACHED      = (0 shl 10); {Normal memory, Outer Non-cacheable}
 ARMV7L_CP15_C2_TTBCR_ORGN0_OUTER_WRITE_ALLOCATE = (1 shl 10); {Normal memory, Outer Write-Back Write-Allocate Cacheable}
 ARMV7L_CP15_C2_TTBCR_ORGN0_OUTER_WRITE_THROUGH  = (2 shl 10); {Normal memory, Outer Write-Through Cacheable}
 ARMV7L_CP15_C2_TTBCR_ORGN0_OUTER_WRITE_BACK     = (3 shl 10); {Normal memory, Outer Write-Back no Write-Allocate Cacheable}
 ARMV7L_CP15_C2_TTBCR_IRGN0_MASK                 = (3 shl 8); {Inner cacheability attribute for memory associated with translation table walks using TTBR0}
 ARMV7L_CP15_C2_TTBCR_IRGN0_INNER_NONCACHED      = (0 shl 8); {Normal memory, Inner Non-cacheable}
 ARMV7L_CP15_C2_TTBCR_IRGN0_INNER_WRITE_ALLOCATE = (1 shl 8); {Normal memory, Inner Write-Back Write-Allocate Cacheable}
 ARMV7L_CP15_C2_TTBCR_IRGN0_INNER_WRITE_THROUGH  = (2 shl 8); {Normal memory, Inner Write-Through Cacheable}
 ARMV7L_CP15_C2_TTBCR_IRGN0_INNER_WRITE_BACK     = (3 shl 8); {Normal memory, Inner Write-Back no Write-Allocate Cacheable}
 ARMV7L_CP15_C2_TTBCR_EPD0                       = (1 shl 7); {Translation table walk disable for translations using TTBR0 (See ARMV7L_CP15_C2_TTBCR_EPD1)}
 ARMV7L_CP15_C2_TTBCR_T0SZ_MASK                  = (7 shl 0); {The size offset of the memory region addressed by TTBR0}
 ARMV7L_CP15_C2_TTBCR_T0SZ_4GB                   = (0 shl 0);

                             {TTBR0 Inner Write Allocate, Outer Write Allocate, Inner Shareable}
 ARMV7L_CP15_C2_TTBCR_MASK = ARMV7L_CP15_C2_TTBCR_IRGN0_INNER_WRITE_ALLOCATE or ARMV7L_CP15_C2_TTBCR_ORGN0_OUTER_WRITE_ALLOCATE or ARMV7L_CP15_C2_TTBCR_SH0_INNER_SHAREABLE
                             {TTBR1 Inner Write Allocate, Outer Write Allocate, Inner Shareable}
                          or ARMV7L_CP15_C2_TTBCR_IRGN1_INNER_WRITE_ALLOCATE or ARMV7L_CP15_C2_TTBCR_ORGN1_OUTER_WRITE_ALLOCATE or ARMV7L_CP15_C2_TTBCR_SH1_INNER_SHAREABLE;

 {Definitions of CP15 C2 (Translation Table Base Registers 0 and 1) bits in the system control processor registers}
 ARMV7L_CP15_C2_TTBR_ASID_MASK    = ($F shl 48);       {An ASID for the translation table base address. The TTBCR.A1 field selects either TTBR0.ASID or TTBR1.ASID}
 ARMV7L_CP15_C2_TTBR_BASE_MASK    = $000000FFFFFFFFE0; {Translation table base address, bits[39:5] (Only correct if TTBCR.TxSZ is 0)}
 ARMV7L_CP15_C2_TTBR_BASE_MASK_HI = $000000FF;
 ARMV7L_CP15_C2_TTBR_BASE_MASK_LO = $FFFFFFE0;

 {Definitions of the Hardware Page Table Descriptors (See B3.6 of the ARM Architecture Reference Manual ARMv7-A and ARMv7-R edition)}
 {Level 1 Page Table contains up to 512 64bit (8 byte) entries for a total size of 4KB}
 {Level 2 Page Table contains up to 512 64bit (8 byte) entries for a total size of 4KB}
 {Level 3 Page Table contains 512 64bit (8 byte) entries for a total size of 4KB}
 ARMV7L_LARGESECTION_BASE_MASK = $C0000000; {1GB Large Sections}
 ARMV7L_SECTION_BASE_MASK      = $FFE00000; {2MB Sections}
 ARMV7L_PAGE_BASE_MASK         = $FFFFF000; {4KB Pages}
 ARMV7L_DESCRIPTOR_BASE_MASK   = $FFFFF000;

 {Descriptor Types (See B3.6.1 of the ARM Architecture Reference Manual ARMv7-A and ARMv7-R edition)}
 ARMV7L_DESCRIPTOR_TYPE_INVALID  = (0 shl 0); {00 - Invalid at all levels}
 ARMV7L_DESCRIPTOR_TYPE_BLOCK    = (1 shl 0); {01 - Block at level 1 and 2}
 ARMV7L_DESCRIPTOR_TYPE_TABLE    = (3 shl 0); {11 - Table at level 1 and 2}
 ARMV7L_DESCRIPTOR_TYPE_RESERVED = (1 shl 0); {01 - Reserved at level 3}
 ARMV7L_DESCRIPTOR_TYPE_PAGE     = (3 shl 0); {11 - Page at level 3}

 ARMV7L_DESCRIPTOR_TYPE_MASK     = (3 shl 0);

 {Descriptor Output (See B3.6.1 of the ARM Architecture Reference Manual ARMv7-A and ARMv7-R edition)}
 ARMV7L_DESCRIPTOR_L1_OUTPUT_MASK  = $000000FFC0000000; {Output address bits[39:30]}
 ARMV7L_DESCRIPTOR_L2_OUTPUT_MASK  = $000000FFFFE00000; {Output address bits[39:21]}
 ARMV7L_DESCRIPTOR_L3_OUTPUT_MASK  = $000000FFFFFFF000; {Output address bits[39:12]}
 ARMV7L_DESCRIPTOR_NEXT_TABLE_MASK = $000000FFFFFFF000; {Next-level table address bits[39:12] (The first-level descriptor returns the address of the second-level table, The second-level descriptor returns the address of the third-level table)}

 {Descriptor Attributes (B3.6.2 of the ARM Architecture Reference Manual ARMv7-A and ARMv7-R edition))}
 {Next-level attributes in Long-descriptor Table descriptors}
 ARMV7L_DESCRIPTOR_ATTRIBUTE_NSTABLE                        = (1 shl 63); {For memory accesses from Secure state, specifies the security level for subsequent levels of lookup (For memory accesses from Non-secure state, this bit is ignored)}
 ARMV7L_DESCRIPTOR_ATTRIBUTE_APTABLE_MASK                   = (3 shl 61); {Access permissions limit for subsequent levels of lookup}
 ARMV7L_DESCRIPTOR_ATTRIBUTE_APTABLE_NONE                   = (0 shl 61); {No effect on permissions in subsequent levels of lookup}
 ARMV7L_DESCRIPTOR_ATTRIBUTE_APTABLE_NOACCESS_PL0           = (1 shl 61); {Access at PL0 not permitted, regardless of permissions in subsequent levels of lookup}
 ARMV7L_DESCRIPTOR_ATTRIBUTE_APTABLE_NOWRITE_ALL            = (2 shl 61); {Write access not permitted, at any privilege level, regardless of permissions in subsequent levels of lookup}
 ARMV7L_DESCRIPTOR_ATTRIBUTE_APTABLE_NOWRITE_ALL_NOREAD_PL0 = (3 shl 61); {Regardless of permissions in subsequent levels of lookup write access not permitted, at any privilege level, read access not permitted at PL0}
 ARMV7L_DESCRIPTOR_ATTRIBUTE_XNTABLE                        = (1 shl 60); {XN limit for subsequent levels of lookup (when XNTable is set to 0 it has no effect)}
 ARMV7L_DESCRIPTOR_ATTRIBUTE_PXNTABLE                       = (1 shl 59); {PXN limit for subsequent levels of lookup (when PXNTable is set to 0 it has no effect)}

 {Attribute in Long-descriptor Block and Page descriptors}
 ARMV7L_DESCRIPTOR_ATTRIBUTE_XN                 = (1 shl 54); {The Execute-never bit. Determines whether the region is executable}
 ARMV7L_DESCRIPTOR_ATTRIBUTE_PXN                = (1 shl 53); {The Privileged execute-never bit. Determines whether the region is executable at PL1}
 ARMV7L_DESCRIPTOR_ATTRIBUTE_CONTIGUOUS         = (1 shl 52); {A hint bit indicating that 16 adjacent translation table entries point to contiguous memory regions}
 ARMV7L_DESCRIPTOR_ATTRIBUTE_NG                 = (1 shl 11); {The not global bit. Determines how the translation is marked in the TLB}
 ARMV7L_DESCRIPTOR_ATTRIBUTE_AF                 = (1 shl 10); {The Access flag}
 ARMV7L_DESCRIPTOR_ATTRIBUTE_SH_MASK            = (3 shl 8);  {Shareability field}
 ARMV7L_DESCRIPTOR_ATTRIBUTE_SH_NONSHAREABLE    = (0 shl 8);  {Non-shareable}
 ARMV7L_DESCRIPTOR_ATTRIBUTE_SH_OUTER_SHAREABLE = (2 shl 8);  {Outer Shareable}
 ARMV7L_DESCRIPTOR_ATTRIBUTE_SH_INNER_SHAREABLE = (3 shl 8);  {Inner Shareable}
 ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_MASK            = (3 shl 6);  {Access Permissions bits (Note: There is no No Access permission in the AP[2:1] model)}
 ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_READWRITE_PL1   = (0 shl 6);  {Read/write, at PL1}
 ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_READWRITE_ALL   = (1 shl 6);  {Read/write, at any privilege level}
 ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_READONLY_PL1    = (2 shl 6);  {Read-only, at PL1}
 ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_READONLY_ALL    = (3 shl 6);  {Read-only, at any privilege level}
 ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_NONE_ALL        = ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_READONLY_PL1; {No access, at any privilege level (No such permission exists, see note above)}
 ARMV7L_DESCRIPTOR_ATTRIBUTE_NS                 = (1 shl 5);  {Non-secure bit. For memory accesses from Secure state, specifies whether the output address is in Secure or Non-secure memory (For memory accesses from Non-secure state, this bit is ignored)}
 ARMV7L_DESCRIPTOR_ATTRIBUTE_ATTRINDX_MASK      = (7 shl 2);  {Memory attributes index field, for the indicated Memory Attribute Indirection Register (MAIR)}
 ARMV7L_DESCRIPTOR_ATTRIBUTE_ATTRINDX_ATTR0     = (0 shl 2);
 ARMV7L_DESCRIPTOR_ATTRIBUTE_ATTRINDX_ATTR1     = (1 shl 2);
 ARMV7L_DESCRIPTOR_ATTRIBUTE_ATTRINDX_ATTR2     = (2 shl 2);
 ARMV7L_DESCRIPTOR_ATTRIBUTE_ATTRINDX_ATTR3     = (3 shl 2);
 ARMV7L_DESCRIPTOR_ATTRIBUTE_ATTRINDX_ATTR4     = (4 shl 2);
 ARMV7L_DESCRIPTOR_ATTRIBUTE_ATTRINDX_ATTR5     = (5 shl 2);
 ARMV7L_DESCRIPTOR_ATTRIBUTE_ATTRINDX_ATTR6     = (6 shl 2);
 ARMV7L_DESCRIPTOR_ATTRIBUTE_ATTRINDX_ATTR7     = (7 shl 2);

 ARMV7L_DESCRIPTOR_ATTRIBUTE_MASK               = $FFF0000000000FFC; {Upper Attributes bits[63:52] Lower Attributes bits[11:9]}

 {Descriptor Cache Values (ATTRINDX <-> MAIR) (See B4.1.104 of the ARM Architecture Reference Manual ARMv7-A and ARMv7-R edition)(These values are from Linux)}
 ARMV7L_DESCRIPTOR_CACHE_STRONGLY_ORDERED      = ARMV7L_DESCRIPTOR_ATTRIBUTE_ATTRINDX_ATTR0; {Strongly Ordered}
 ARMV7L_DESCRIPTOR_CACHE_NORMAL_NONCACHED      = ARMV7L_DESCRIPTOR_ATTRIBUTE_ATTRINDX_ATTR1; {Normal Noncacheable (Shared if SH bits set)}
 ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_THROUGH  = ARMV7L_DESCRIPTOR_ATTRIBUTE_ATTRINDX_ATTR2; {Normal Write Through (Shared if SH bits set)}
 ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_BACK     = ARMV7L_DESCRIPTOR_ATTRIBUTE_ATTRINDX_ATTR3; {Normal Write Back (Shared if SH bits set)}
 ARMV7L_DESCRIPTOR_CACHE_DEVICE                = ARMV7L_DESCRIPTOR_ATTRIBUTE_ATTRINDX_ATTR4; {Device}
 ARMV7L_DESCRIPTOR_CACHE_UNUSED1               = ARMV7L_DESCRIPTOR_ATTRIBUTE_ATTRINDX_ATTR5; {Not currently used}
 ARMV7L_DESCRIPTOR_CACHE_UNUSED2               = ARMV7L_DESCRIPTOR_ATTRIBUTE_ATTRINDX_ATTR6; {Not currently used}
 ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_ALLOCATE = ARMV7L_DESCRIPTOR_ATTRIBUTE_ATTRINDX_ATTR7; {Normal Write Allocate (Shared if SH bits set)}

{==============================================================================}
{type}
 {ARMv7L specific types}

{==============================================================================}
{var}
 {ARMv7L specific variables}

{==============================================================================}
{Initialization Functions}
procedure ARMv7LInit;

{==============================================================================}
{ARMv7L Platform Functions}
procedure ARMv7LMMUInit;

procedure ARMv7LPageTableInit;

procedure ARMv7LPageTableGetEntry(Address:PtrUInt;var Entry:TPageTableEntry);
function ARMv7LPageTableSetEntry(const Entry:TPageTableEntry):LongWord;

{==============================================================================}
{ARMv7L Helper Functions}
procedure ARMv7LStartMMU;

function ARMv7LGetPageTableLevel1(Address:PtrUInt):UInt64;
function ARMv7LSetPageTableLevel1(Address,TableAddress:PtrUInt;Flags:UInt64):Boolean;

function ARMv7LGetPageTableLevel2(Address:PtrUInt):UInt64;
function ARMv7LSetPageTableLevel2(Address,TableAddress:PtrUInt;Flags:UInt64):Boolean;

function ARMv7LGetPageTablePage(Address:PtrUInt):UInt64;
function ARMv7LSetPageTablePage(Address:PtrUInt;PhysicalRange:LongWord;PhysicalAddress:PtrUInt;Flags:UInt64):Boolean;

function ARMv7LGetPageTableSection(Address:PtrUInt):UInt64;
function ARMv7LSetPageTableSection(Address:PtrUInt;PhysicalRange:LongWord;PhysicalAddress:PtrUInt;Flags:UInt64):Boolean;

function ARMv7LGetPageTableLargeSection(Address:PtrUInt):UInt64;
function ARMv7LSetPageTableLargeSection(Address:PtrUInt;PhysicalRange:LongWord;PhysicalAddress:PtrUInt;Flags:UInt64):Boolean;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {ARMv7L specific variables}
 ARMv7LInitialized:Boolean;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure ARMv7LInit;
begin
 {}
 if ARMv7LInitialized then Exit;

 {Call ARMv7 Initialization}
 ARMv7Init;

 {Setup PAGE_TABLE_LEVELS}
 PAGE_TABLE_LEVELS:=ARMV7L_PAGE_TABLE_LEVELS;

 {Setup PAGE_TABLES_SHIFT}
 PAGE_TABLES_SHIFT:=ARMV7L_PAGE_TABLES_SHIFT;

 {Register Platform MMUInit Handler}
 MMUInitHandler:=ARMv7LMMUInit;

 {Register Platform PageTable Handlers}
 PageTableGetEntryHandler:=ARMv7LPageTableGetEntry;
 PageTableSetEntryHandler:=ARMv7LPageTableSetEntry;

 ARMv7LInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{ARMv7L Platform Functions}
procedure ARMv7LMMUInit;
begin
 {}
 {Check the Page Directory}
 if PAGE_DIRECTORY_SIZE < SIZE_4K then Exit; {Level 1 Page Directory is 4KB}
 if Pointer(PAGE_DIRECTORY_BASE) = nil then Exit;

 {Check the Page Table}
 if PAGE_TABLE_SIZE < SIZE_16K then Exit; {Level 2 Page Table is 16KB}
 if Pointer(PAGE_TABLE_BASE) = nil then Exit;

 {Initialize the Page Table}
 ARMv7LPageTableInit;

 {Start the MMU}
 ARMv7LStartMMU;
end;

{==============================================================================}

procedure ARMv7LPageTableInit;
{Initialize the Hardware Page Tables before enabling the MMU}
var
 Count:Integer;
 Table:PtrUInt;
 Address:PtrUInt;
begin
 {}
 if Assigned(ARMv7PageTableInitHandler) then
  begin
   {Call the supplied handler}
   ARMv7PageTableInitHandler;
  end
 else
  begin
   {Perform the default initialization}
   {If no handler is supplied then simply initialize 1GB (512 2MB sections) as Normal, Cacheable, Shared, Executable with Read/Write access and direct mapping of Virtual to Physical}
   {And 3GB (1536 2MB sections) as Normal, Non Cacheable, Shared, Executable with Read/Write access and direct mapping of Virtual to Physical}
   {This will not normally be a useful setup but it will at least provide a Page Table that allows the memory management unit to be enabled}

   {Create the first level page table}
   {Setup 1GB tables covering the entire 4GB address space}
   Table:=(PAGE_TABLE_BASE and ARMV7L_DESCRIPTOR_BASE_MASK);
   Address:=$00000000;
   while Address < SIZE_4G do
    begin
     ARMv7LSetPageTableLargeSection(Address,0,Address,ARMV7L_DESCRIPTOR_CACHE_NORMAL_NONCACHED or ARMV7L_DESCRIPTOR_ATTRIBUTE_SH_INNER_SHAREABLE or ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_NONE_ALL);
     ARMv7LSetPageTableLevel1(Address,Table,0);
     Inc(Table,SIZE_4K);
     Inc(Address,SIZE_1G);
    end;

   {Set the 2MB sections in the first 1GB as ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_ALLOCATE (Shared)(Executable)(Read Write)}
   Address:=$00000000;
   for Count:=0 to 511 do
    begin
     ARMv7LSetPageTableSection(Address,0,Address,ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_ALLOCATE or ARMV7L_DESCRIPTOR_ATTRIBUTE_SH_INNER_SHAREABLE or ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_READWRITE_ALL);
     Inc(Address,SIZE_2M);
    end;

   {Set the 2MB sections in the remaining 3GB as ARMV7L_DESCRIPTOR_CACHE_NORMAL_NONCACHED (Shared)(Executable)(Read Write)}
   for Count:=512 to 2047 do
    begin
     ARMv7LSetPageTableSection(Address,0,Address,ARMV7L_DESCRIPTOR_CACHE_NORMAL_NONCACHED or ARMV7L_DESCRIPTOR_ATTRIBUTE_SH_INNER_SHAREABLE or ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_READWRITE_ALL);
     Inc(Address,SIZE_2M);
    end;

   {Set the 2MB section containing the PAGE_DIRECTORY_BASE to ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_ALLOCATE (Shared)(Executable)(Read Write)}
   Address:=(PAGE_DIRECTORY_BASE and ARMV7L_SECTION_BASE_MASK);
   ARMv7LSetPageTableSection(Address,0,Address,ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_ALLOCATE or ARMV7L_DESCRIPTOR_ATTRIBUTE_SH_INNER_SHAREABLE or ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_READWRITE_ALL);

   {Set the 2MB section containing the PAGE_TABLE_BASE to ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_ALLOCATE (Shared)(Executable)(Read Write)}
   Address:=(PAGE_TABLE_BASE and ARMV7L_SECTION_BASE_MASK);
   ARMv7LSetPageTableSection(Address,0,Address,ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_ALLOCATE or ARMV7L_DESCRIPTOR_ATTRIBUTE_SH_INNER_SHAREABLE or ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_READWRITE_ALL);

   {Set the 2MB sections containing the PERIPHERALS_BASE to ARMV7L_DESCRIPTOR_CACHE_DEVICE (Shared)(Non Executable)(Read Write)}
   if PERIPHERALS_SIZE > 0 then
    begin
     Address:=(PERIPHERALS_BASE and ARMV7L_SECTION_BASE_MASK);
     while Address < (PERIPHERALS_BASE + PERIPHERALS_SIZE) do
      begin
       ARMv7LSetPageTableSection(Address,0,Address,ARMV7L_DESCRIPTOR_CACHE_DEVICE or ARMV7L_DESCRIPTOR_ATTRIBUTE_SH_OUTER_SHAREABLE or ARMV7L_DESCRIPTOR_ATTRIBUTE_XN or ARMV7L_DESCRIPTOR_ATTRIBUTE_PXN or ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_READWRITE_ALL);
       Inc(Address,SIZE_2M);
      end;
    end;

   {Set the 2MB sections containing the LOCAL_PERIPHERALS_BASE to ARMV7L_DESCRIPTOR_CACHE_DEVICE (Shared)(Non Executable)(Read Write)}
   if LOCAL_PERIPHERALS_SIZE > 0 then
    begin
     Address:=(LOCAL_PERIPHERALS_BASE and ARMV7L_SECTION_BASE_MASK);
     while Address < (LOCAL_PERIPHERALS_BASE + LOCAL_PERIPHERALS_SIZE) do
      begin
       ARMv7LSetPageTableSection(Address,0,Address,ARMV7L_DESCRIPTOR_CACHE_DEVICE or ARMV7L_DESCRIPTOR_ATTRIBUTE_SH_OUTER_SHAREABLE or ARMV7L_DESCRIPTOR_ATTRIBUTE_XN or ARMV7L_DESCRIPTOR_ATTRIBUTE_PXN or ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_READWRITE_ALL);
       Inc(Address,SIZE_2M);
      end;
    end;
  end;
end;

{==============================================================================}

procedure ARMv7LPageTableGetEntry(Address:PtrUInt;var Entry:TPageTableEntry);
{Get and Decode the entry in the Page Table that corresponds to the supplied virtual address}

 function GetEntryFlags(TableEntry:UInt64):LongWord;
 begin
  {}
  Result:=PAGE_TABLE_FLAG_NONE;

  {Check Normal/Cacheable/WriteBack/WriteThrough}
  if (TableEntry and ARMV7L_DESCRIPTOR_ATTRIBUTE_ATTRINDX_MASK) = ARMV7L_DESCRIPTOR_CACHE_NORMAL_NONCACHED then
   begin
    Result:=Result or PAGE_TABLE_FLAG_NORMAL;
   end
  else if (TableEntry and ARMV7L_DESCRIPTOR_ATTRIBUTE_ATTRINDX_MASK) = ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_BACK then
   begin
    Result:=Result or PAGE_TABLE_FLAG_NORMAL or PAGE_TABLE_FLAG_CACHEABLE or PAGE_TABLE_FLAG_WRITEBACK;
   end
  else if (TableEntry and ARMV7L_DESCRIPTOR_ATTRIBUTE_ATTRINDX_MASK) = ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_THROUGH then
   begin
    Result:=Result or PAGE_TABLE_FLAG_NORMAL or PAGE_TABLE_FLAG_CACHEABLE or PAGE_TABLE_FLAG_WRITETHROUGH;
   end
  else if (TableEntry and ARMV7L_DESCRIPTOR_ATTRIBUTE_ATTRINDX_MASK) = ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_ALLOCATE then
   begin
    Result:=Result or PAGE_TABLE_FLAG_NORMAL or PAGE_TABLE_FLAG_CACHEABLE or PAGE_TABLE_FLAG_WRITEALLOCATE;
   end;

  {Check Device}
  if (TableEntry and ARMV7L_DESCRIPTOR_ATTRIBUTE_ATTRINDX_MASK) = ARMV7L_DESCRIPTOR_CACHE_DEVICE then
   begin
    Result:=Result or PAGE_TABLE_FLAG_DEVICE;
   end;

  {Check Ordered}
  if (TableEntry and ARMV7L_DESCRIPTOR_ATTRIBUTE_ATTRINDX_MASK) = ARMV7L_DESCRIPTOR_CACHE_STRONGLY_ORDERED then
   begin
    Result:=Result or PAGE_TABLE_FLAG_ORDERED;
   end;

  {Check Shared}
  if (TableEntry and ARMV7L_DESCRIPTOR_ATTRIBUTE_SH_MASK) = ARMV7L_DESCRIPTOR_ATTRIBUTE_SH_INNER_SHAREABLE then
   begin
    Result:=Result or PAGE_TABLE_FLAG_SHARED;
   end
  else if (TableEntry and ARMV7L_DESCRIPTOR_ATTRIBUTE_SH_MASK) = ARMV7L_DESCRIPTOR_ATTRIBUTE_SH_OUTER_SHAREABLE then
   begin
    Result:=Result or PAGE_TABLE_FLAG_SHARED;
   end;

  {Check NoAccess}
  if (TableEntry and ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_MASK) = ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_NONE_ALL then
   begin
    Result:=Result or PAGE_TABLE_FLAG_READONLY;
   end;

  {Check ReadOnly}
  if (TableEntry and ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_MASK) = ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_READONLY_ALL then
   begin
    Result:=Result or PAGE_TABLE_FLAG_READONLY;
   end;

  {Check ReadWrite}
  if (TableEntry and ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_MASK) = ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_READWRITE_ALL then
   begin
    Result:=Result or PAGE_TABLE_FLAG_READWRITE;
   end;

  {Check Executable}
  if (TableEntry and ARMV7L_DESCRIPTOR_ATTRIBUTE_XN) <> ARMV7L_DESCRIPTOR_ATTRIBUTE_XN then
   begin
    Result:=Result or PAGE_TABLE_FLAG_EXECUTABLE;
   end;
 end;

var
 TableEntry:UInt64;
begin
 {}
 FillChar(Entry,SizeOf(TPageTableEntry),0);

 {Check Address}
 {Zero may be valid}

 {Get Level 2 Table}
 TableEntry:=ARMv7LGetPageTableLevel2(Address);
 if TableEntry <> 0 then
  begin
   {Get Page}
   TableEntry:=ARMv7LGetPageTablePage(Address);
   if TableEntry <> 0 then
    begin
     {Get Virtual Address}
     Entry.VirtualAddress:=(Address and ARMV7L_PAGE_BASE_MASK);

     {Get Physical Range}
     Entry.PhysicalRange:=(TableEntry and ARMV7L_DESCRIPTOR_L3_OUTPUT_MASK) shr 32;

     {Get Physical Address and Size}
     Entry.PhysicalAddress:=(TableEntry and ARMV7L_DESCRIPTOR_L3_OUTPUT_MASK);
     Entry.Size:=SIZE_4K;

     {Get Flags}
     Entry.Flags:=GetEntryFlags(TableEntry);
    end;
  end
 else
  begin
   {Get Level 1 Table}
   TableEntry:=ARMv7LGetPageTableLevel1(Address);
   if TableEntry <> 0 then
    begin
     {Get Section}
     TableEntry:=ARMv7LGetPageTableSection(Address);
     if TableEntry <> 0 then
      begin
       {Get Virtual Address}
       Entry.VirtualAddress:=(Address and ARMV7L_SECTION_BASE_MASK);

       {Get Physical Range}
       Entry.PhysicalRange:=(TableEntry and ARMV7L_DESCRIPTOR_L2_OUTPUT_MASK) shr 32;

       {Get Physical Address and Size}
       Entry.PhysicalAddress:=(TableEntry and ARMV7L_DESCRIPTOR_L2_OUTPUT_MASK);
       Entry.Size:=SIZE_2M;

       {Get Flags}
       Entry.Flags:=GetEntryFlags(TableEntry);
      end;
    end
   else
    begin
     {Get Large Section}
     TableEntry:=ARMv7LGetPageTableLargeSection(Address);
     if TableEntry <> 0 then
      begin
       {Get Virtual Address}
       Entry.VirtualAddress:=(Address and ARMV7L_LARGESECTION_BASE_MASK);

       {Get Physical Range}
       Entry.PhysicalRange:=(TableEntry and ARMV7L_DESCRIPTOR_L1_OUTPUT_MASK) shr 32;

       {Get Physical Address and Size}
       Entry.PhysicalAddress:=(TableEntry and ARMV7L_DESCRIPTOR_L1_OUTPUT_MASK);
       Entry.Size:=SIZE_1G;

       {Get Flags}
       Entry.Flags:=GetEntryFlags(TableEntry);
      end;
    end;
  end;
end;

{==============================================================================}

function ARMv7LPageTableSetEntry(const Entry:TPageTableEntry):LongWord;
{Encode and Set an entry in the Page Table that corresponds to the supplied virtual address}

 function GetTableFlags(Flags:LongWord;var TableFlags:UInt64):Boolean;
 var
  ReadMask:LongWord;
  RemapMask:LongWord;
  FlagDevice:Boolean;
  FlagOrdered:Boolean;
 begin
  {}
  Result:=False;

  TableFlags:=0;

  {Get Masks}
  ReadMask:=PAGE_TABLE_FLAG_READONLY or PAGE_TABLE_FLAG_READWRITE;
  RemapMask:=PAGE_TABLE_FLAG_NORMAL or PAGE_TABLE_FLAG_DEVICE or PAGE_TABLE_FLAG_ORDERED or PAGE_TABLE_FLAG_CACHEABLE or PAGE_TABLE_FLAG_WRITEBACK or PAGE_TABLE_FLAG_WRITETHROUGH or PAGE_TABLE_FLAG_WRITEALLOCATE;
  FlagDevice:=False;
  FlagOrdered:=False;

  {Check Normal/Cacheable/WriteBack/WriteThrough/Device/Ordered}
  if (Flags and RemapMask) = PAGE_TABLE_FLAG_NORMAL then
   begin
    TableFlags:=TableFlags or ARMV7L_DESCRIPTOR_CACHE_NORMAL_NONCACHED;
   end
  else if (Flags and RemapMask) = (PAGE_TABLE_FLAG_NORMAL or PAGE_TABLE_FLAG_CACHEABLE or PAGE_TABLE_FLAG_WRITEBACK) then
   begin
    TableFlags:=TableFlags or ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_BACK;
   end
  else if (Flags and RemapMask) = (PAGE_TABLE_FLAG_NORMAL or PAGE_TABLE_FLAG_CACHEABLE or PAGE_TABLE_FLAG_WRITETHROUGH) then
   begin
    TableFlags:=TableFlags or ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_THROUGH;
   end
  else if (Flags and RemapMask) = (PAGE_TABLE_FLAG_NORMAL or PAGE_TABLE_FLAG_CACHEABLE or PAGE_TABLE_FLAG_WRITEALLOCATE) then
   begin
    TableFlags:=TableFlags or ARMV7L_DESCRIPTOR_CACHE_NORMAL_WRITE_ALLOCATE;
   end
  else if (Flags and RemapMask) = PAGE_TABLE_FLAG_DEVICE then
   begin
    TableFlags:=TableFlags or ARMV7L_DESCRIPTOR_CACHE_DEVICE;

    FlagDevice:=True;
   end
  else if (Flags and RemapMask) = PAGE_TABLE_FLAG_ORDERED then
   begin
    TableFlags:=TableFlags or ARMV7L_DESCRIPTOR_CACHE_STRONGLY_ORDERED;

    FlagOrdered:=True;
   end
  else
   begin
    {Not Supported}
    Exit;
   end;

  {Check Shared}
  if (Flags and PAGE_TABLE_FLAG_SHARED) = PAGE_TABLE_FLAG_SHARED then
   begin
    if FlagDevice or FlagOrdered then
     begin
      TableFlags:=TableFlags or ARMV7L_DESCRIPTOR_ATTRIBUTE_SH_OUTER_SHAREABLE;
     end
    else
     begin
      TableFlags:=TableFlags or ARMV7L_DESCRIPTOR_ATTRIBUTE_SH_INNER_SHAREABLE;
     end;
   end;

  {Check NoAccess / ReadOnly / ReadWrite}
  if (Flags and ReadMask) = PAGE_TABLE_FLAG_NONE then
   begin
    TableFlags:=TableFlags or ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_NONE_ALL;
   end
  else if (Flags and ReadMask) = PAGE_TABLE_FLAG_READONLY then
   begin
    TableFlags:=TableFlags or ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_READONLY_ALL;
   end
  else if (Flags and ReadMask) = PAGE_TABLE_FLAG_READWRITE then
   begin
    TableFlags:=TableFlags or ARMV7L_DESCRIPTOR_ATTRIBUTE_AP_READWRITE_ALL;
   end
  else
   begin
    {Not Supported}
    Exit;
   end;

  {Check Executable}
  if (Flags and PAGE_TABLE_FLAG_EXECUTABLE) <> PAGE_TABLE_FLAG_EXECUTABLE then
   begin
    TableFlags:=TableFlags or ARMV7L_DESCRIPTOR_ATTRIBUTE_XN or ARMV7L_DESCRIPTOR_ATTRIBUTE_PXN;
   end;

  Result:=True;
 end;

var
 TableBase:PtrUInt;
 TableAddress:PtrUInt;
 TableFlags:UInt64;
 TableEntry:UInt64;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Entry}
 if Entry.Flags = PAGE_TABLE_FLAG_NONE then Exit;

 {Acquire Lock}
 if PageTableLock.Lock <> INVALID_HANDLE_VALUE then PageTableLock.AcquireLock(PageTableLock.Lock);
 try
  {Check Size}
  case Entry.Size of
   SIZE_4K:begin
     {4KB Page}
     {Get Level 2 Table}
     TableEntry:=ARMv7LGetPageTableLevel2(Entry.VirtualAddress);
     if TableEntry = 0 then
      begin
       {Allocate Level 3 Table}
       if PAGE_TABLES_FREE = 0 then Exit;

       {Update Free/Used}
       Dec(PAGE_TABLES_FREE);
       Inc(PAGE_TABLES_USED);

       {Get Table Base}
       TableBase:=(Entry.VirtualAddress and ARMV7L_SECTION_BASE_MASK);

       {Get Table Address}
       TableAddress:=(PAGE_TABLES_NEXT and ARMV7L_DESCRIPTOR_BASE_MASK);

       {Update Next}
       Inc(PAGE_TABLES_NEXT,SIZE_4K);

       {Set Coarse}
       if not ARMv7LSetPageTableLevel2(TableBase,TableAddress,0) then
        begin
         {Reset Free/Used/Next}
         Inc(PAGE_TABLES_FREE);
         Dec(PAGE_TABLES_USED);
         Dec(PAGE_TABLES_NEXT,SIZE_4K);

         Exit;
        end;

       {Clean Data Cache Range (Level 3 Table)}
       ARMv7CleanDataCacheRange(TableAddress,SIZE_4K);

       {Clean Data Cache Range (Page Table)}
       ARMv7CleanDataCacheRange(PAGE_TABLE_BASE,PAGE_TABLE_SIZE);

       {Invalidate TLB}
       ARMv7InvalidateTLB;
      end
     else
      begin
       {Get Table Addres}
       TableAddress:=(TableEntry and ARMV7L_DESCRIPTOR_NEXT_TABLE_MASK);
      end;

     {Get Page}
     TableEntry:=ARMv7LGetPageTablePage(Entry.VirtualAddress);
     if TableEntry = 0 then Exit;

     {Get Flags}
     if not GetTableFlags(Entry.Flags,TableFlags) then Exit;

     {Update Page}
     if ARMv7LSetPageTablePage(Entry.VirtualAddress,Entry.PhysicalRange,Entry.PhysicalAddress,TableFlags) then
      begin
       {Clean Data Cache Range (Level 3 Table)}
       ARMv7CleanDataCacheRange(TableAddress,SIZE_4K);

       {Clean Data Cache Range (Page Table)}
       ARMv7CleanDataCacheRange(PAGE_TABLE_BASE,PAGE_TABLE_SIZE);

       {Invalidate TLB}
       ARMv7InvalidateTLB;

       {Return Result}
       Result:=ERROR_SUCCESS;
      end;
    end;
   SIZE_2M:begin
     {2MB Section}
     {Get Section}
     TableEntry:=ARMv7LGetPageTableSection(Entry.VirtualAddress);
     if TableEntry = 0 then Exit;

     {Get Flags}
     if not GetTableFlags(Entry.Flags,TableFlags) then Exit;

     {Update Section}
     if ARMv7LSetPageTableSection(Entry.VirtualAddress,Entry.PhysicalRange,Entry.PhysicalAddress,TableFlags) then
      begin
       {Clean Data Cache Range (Page Table)}
       ARMv7CleanDataCacheRange(PAGE_TABLE_BASE,PAGE_TABLE_SIZE);

       {Invalidate TLB}
       ARMv7InvalidateTLB;

       {Return Result}
       Result:=ERROR_SUCCESS;
      end;
    end;
   SIZE_1G:begin
     {1GB Large Section}
     {Get Large Section}
     TableEntry:=ARMv7LGetPageTableLargeSection(Entry.VirtualAddress);
     if TableEntry = 0 then Exit;

     {Get Flags}
     if not GetTableFlags(Entry.Flags,TableFlags) then Exit;

     {Update Large Section}
     if ARMv7LSetPageTableLargeSection(Entry.VirtualAddress,Entry.PhysicalRange,Entry.PhysicalAddress,TableFlags) then
      begin
       {Clean Data Cache Range (Page Directory)}
       ARMv7CleanDataCacheRange(PAGE_DIRECTORY_BASE,PAGE_DIRECTORY_SIZE);

       {Invalidate TLB}
       ARMv7InvalidateTLB;

       {Return Result}
       Result:=ERROR_SUCCESS;
      end;
    end;
  end;
 finally
  {Release Lock}
  if PageTableLock.Lock <> INVALID_HANDLE_VALUE then PageTableLock.ReleaseLock(PageTableLock.Lock);
 end;
end;

{==============================================================================}
{==============================================================================}
{ARMv7L Helper Functions}
procedure ARMv7LStartMMU; assembler; nostackframe;
asm
 //Preseve LR for return (None of the following uses R4)
 //Note: This function is only called by ARMv7LMMUInit and RPi4SecondaryHandler
 //      which also do not use R4 so it does not need to be preserved on the stack
 mov r4, lr

 //Disable the L1 Data and Instruction Cache before enabling the MMU by clearing the I and C bits in the C1 control register.
 //Also ensure the MMU is disabled by clearing the M bit in the C1 control register.
 mrc p15, #0, r12, cr1, cr0, #0;
 bic r12, r12, #ARMV7_CP15_C1_I_BIT
 bic r12, r12, #ARMV7_CP15_C1_C_BIT
 bic r12, r12, #ARMV7_CP15_C1_M_BIT
 mcr p15, #0, r12, cr1, cr0, #0;

 //Enable coherent processor requests before enabling the MMU by setting the SMP bit in the C1 auxiliary control register.
 //See page 4-59 of the Cortex-A7 MPCore Technical Reference Manual
 mrc p15, #0, r12, cr1, cr0, #1;
 orr r12, r12, #ARMV7_CP15_C1_AUX_SMP
 mcr p15, #0, r12, cr1, cr0, #1;

 //Perform an Instruction Synchronization Barrier (ISB) operation immediately after the change above.
 //See page A8-102  of the ARMv7 Architecture Reference Manual
 //ARMv7 "instruction synchronization barrier" instruction.
 isb

 //Invalidate the L1 Instruction Cache before enabling the MMU
 //See page B3-138 of the ARMv7 Architecture Reference Manual
 bl ARMv7InvalidateInstructionCache

 //Invalidate the L1 Data Cache before enabling the MMU
 //See page B3-138 of the ARMv7 Architecture Reference Manual
 bl ARMv7InvalidateL1DataCache

 //Flush the Branch Target Cache before enabling the MMU
 bl ARMv7FlushBranchTargetCache

 //Invalidate the Transaction Lookaside Buffers (TLB) before enabling the MMU
 //See page B3-138 of the ARMv7 Architecture Reference Manual
 bl ARMv7InvalidateTLB

 //Perform a data synchronization barrier operation
 //See page A8-92 of the ARMv7 Architecture Reference Manual
 //ARMv7 "data synchronization barrier" instruction.
 dsb

 //Set the Translation Table Base Control Register (TTBCR) in the C2 control register.
 //See B4.1.153 of the ARM Architecture Reference Manual ARMv7-A and ARMv7-R edition
 ldr r12, =ARMV7L_CP15_C2_TTBCR_MASK
 orr r12, r12, #ARMV7L_CP15_C2_TTBCR_EAE
 mcr p15, #0, r12, cr2, cr0, #2

 //Load the Memory Attribute Indirection Register 0 (MAIR0) in the C10 control register.
 //See B4.1.104 of the ARM Architecture Reference Manual ARMv7-A and ARMv7-R edition
 ldr r12, =ARMV7L_CP15_C2_MAIR0_MASK
 mcr p15, #0, r12, cr10, cr2, #0

 //Load the Memory Attribute Indirection Register 1 (MAIR1) in the C10 control register.
 //See B4.1.104 of the ARM Architecture Reference Manual ARMv7-A and ARMv7-R edition
 ldr r12, =ARMV7L_CP15_C2_MAIR1_MASK
 mcr p15, #0, r12, cr10, cr2, #1

 //Set the access for Domain 0 to Client in the C3 domain access control register.
 mov r12, #ARMV7_CP15_C3_DOMAIN0_CLIENT
 mcr p15, #0, r12, cr3, cr0, #0

 //Set the Translation Table Base Register 0 (TTBR0) in the C2 control register.
 //Bits 31 to 5 of the Page Directory base address are written to the register
 //The alignment of the Page Directory base address depends on the value of
 //T0SZ in the Translation Table Base Control Register (TTBCR).
 //See B4.1.154 of the ARM Architecture Reference Manual ARMv7-A and ARMv7-R edition
 ldr r0, .LPAGE_DIRECTORY_BASE
 ldr r0, [r0]
 mov r1, #0
 mcrr p15, #0, r0, r1, cr2

 //Set the Translation Table Base Register 1 (TTBR1) in the C2 control register.
 //Bits 31 to 5 of the Page Directory base address are written to the register
 //The alignment of the Page Directory base address depends on the value of
 //T1SZ in the Translation Table Base Control Register (TTBCR).
 //See B4.1.155 of the ARM Architecture Reference Manual ARMv7-A and ARMv7-R edition
 mcrr p15, #1, r0, r1, cr2

 //Perform a instruction synchronization barrier operation to ensure
 //all of the above is completed before enabling the MMU and Caches
 //See page A8-102 of the ARMv7 Architecture Reference Manual
 //ARMv7 "instruction synchronization barrier" instruction.
 isb

 //Enable the Memory Management Unit and L1 Data and Instruction Cache
 //by setting the I, C, M and XP bits in the C1 control register.
 mrc p15, #0, r12, cr1, cr0, #0;
 orr r12, r12, #ARMV7_CP15_C1_I_BIT
 orr r12, r12, #ARMV7_CP15_C1_C_BIT
 orr r12, r12, #ARMV7_CP15_C1_M_BIT
 //orr r12, r12, #ARMV7_CP15_C1_XP_BIT {Always enabled in ARMv7}
 mcr p15, #0, r12, cr1, cr0, #0;

 //Perform an Instruction Synchronization Barrier (ISB) operation immediately after the change above.
 //See page A8-102 of the ARMv7 Architecture Reference Manual
 //ARMv7 "instruction synchronization barrier" instruction.
 isb

 //Restore LR for return
 mov lr, r4

 //Return to caller
 bx lr

.LPAGE_DIRECTORY_BASE:
 .long PAGE_DIRECTORY_BASE
end;

{==============================================================================}

function ARMv7LGetPageTableLevel1(Address:PtrUInt):UInt64;
{Get the descriptor for a Level 1 Page Table (1GB)}
var
 TableBase:LongWord;
 TableOffset:LongWord;

 Level1Entry:UInt64;
begin
 {}
 Result:=0;

 {Check Page Directory}
 if PAGE_DIRECTORY_SIZE < SIZE_4K then Exit; {Level 1 Page Directory is 4KB}
 if Pointer(PAGE_DIRECTORY_BASE) = nil then Exit;

 {Get Table Base}
 TableBase:=(Address and ARMV7L_LARGESECTION_BASE_MASK);

 {Get Table Offset}
 TableOffset:=((TableBase shr 30) shl 3); {Divide Base by 1GB then multiply by 8 to get Offset into Page Directory}

 {Read Page Directory}
 Level1Entry:=PUInt64(PtrUInt(PAGE_DIRECTORY_BASE) + PtrUInt(TableOffset))^;

 {Check Level 1 Type}
 if (Level1Entry and ARMV7L_DESCRIPTOR_TYPE_MASK) = ARMV7L_DESCRIPTOR_TYPE_TABLE then
  begin
   {Return Result}
   Result:=Level1Entry;
  end;
end;

{==============================================================================}

function ARMv7LSetPageTableLevel1(Address,TableAddress:PtrUInt;Flags:UInt64):Boolean;
{Set the descriptor for a Level 1 Page Table (1GB)}
{Note: Caller must call ARMv7InvalidateTLB after changes if MMU is enabled}
var
 Count:Integer;

 TableBase:LongWord;
 TableFlags:UInt64;
 TableOffset:LongWord;

 Level2Base:LongWord;
 Level2Offset:LongWord;

 CurrentAddress:UInt64;
 CurrentFlags:UInt64;
 CurrentBase:LongWord;
 CurrentEntry:UInt64;
 CurrentOffset:LongWord;
begin
 {}
 Result:=False;

 {Check Page Directory}
 if PAGE_DIRECTORY_SIZE < SIZE_4K then Exit; {Level 1 Page Directory is 4KB}
 if Pointer(PAGE_DIRECTORY_BASE) = nil then Exit;

 {Get Table Base}
 TableBase:=(Address and ARMV7L_LARGESECTION_BASE_MASK);
 if TableBase <> Address then Exit; {Must begin on a 1GB boundary}

 {Get Level 2 Base}
 Level2Base:=(TableAddress and ARMV7L_DESCRIPTOR_BASE_MASK);
 if Level2Base <> TableAddress then Exit; {Must begin on a 4KB boundary}

 {Get Table Offset}
 TableOffset:=((TableBase shr 30) shl 3); {Divide Base by 1GB then multiply by 8 to get Offset into Page Directory}

 {Read Page Directory}
 CurrentEntry:=PUInt64(PtrUInt(PAGE_DIRECTORY_BASE) + PtrUInt(TableOffset))^;

 {Check Level 1 Type}
 if (CurrentEntry and ARMV7L_DESCRIPTOR_TYPE_MASK) = ARMV7L_DESCRIPTOR_TYPE_TABLE then
  begin
   {Current entry is a Level 1 Page Table}
   CurrentBase:=(CurrentEntry and ARMV7L_DESCRIPTOR_NEXT_TABLE_MASK);
   CurrentOffset:=0;

   {Compare existing base with new base}
   if CurrentBase <> Level2Base then
    begin
     {Copy existing table to new table}
     for Count:=0 to 511 do
      begin
       PUInt64(PtrUInt(Level2Base) + PtrUInt(CurrentOffset))^:=PUInt64(PtrUInt(CurrentBase) + PtrUInt(CurrentOffset))^;
       Inc(CurrentOffset,SizeOf(UInt64));
      end;
    end;
  end
 else if (CurrentEntry and ARMV7L_DESCRIPTOR_TYPE_MASK) = ARMV7L_DESCRIPTOR_TYPE_BLOCK then
  begin
   {Current entry is a Large Section}
   {Convert to Level 1 Page Table}
   CurrentAddress:=(CurrentEntry and ARMV7L_DESCRIPTOR_L1_OUTPUT_MASK);
   CurrentOffset:=0;
   CurrentFlags:=(CurrentEntry and ARMV7L_DESCRIPTOR_ATTRIBUTE_MASK);

   {Create 512 Section (2MB) entries}
   Level2Offset:=0;
   for Count:=0 to 511 do
    begin
     PUInt64(PtrUInt(Level2Base) + PtrUInt(Level2Offset))^:=(CurrentAddress + CurrentOffset) or CurrentFlags or ARMV7L_DESCRIPTOR_TYPE_BLOCK;
     Inc(Level2Offset,SizeOf(UInt64));
     Inc(CurrentOffset,SIZE_2M);
    end;
  end;

 {Check Flags}
 TableFlags:=Flags and not(ARMV7L_DESCRIPTOR_NEXT_TABLE_MASK);

 {Write Page Table}
 PUInt64(PtrUInt(PAGE_DIRECTORY_BASE) + PtrUInt(TableOffset))^:=Level2Base or TableFlags or ARMV7L_DESCRIPTOR_TYPE_TABLE;

 Result:=True;
end;

{==============================================================================}

function ARMv7LGetPageTableLevel2(Address:PtrUInt):UInt64;
{Get the descriptor for a Level 2 Page Table (2MB)}
var
 TableBase:LongWord;
 TableOffset:LongWord;

 Level2Entry:UInt64;
begin
 {}
 Result:=0;

 {Check Page Table}
 if PAGE_TABLE_SIZE < SIZE_16K then Exit; {Level 2 Page Table is 16KB}
 if Pointer(PAGE_TABLE_BASE) = nil then Exit;

 {Get Table Base}
 TableBase:=(Address and ARMV7L_SECTION_BASE_MASK);

 {Get Table Offset}
 TableOffset:=((TableBase shr 21) shl 3); {Divide Base by 2MB then multiply by 8 to get Offset into Page Table}

 {Read Page Table}
 Level2Entry:=PUInt64(PtrUInt(PAGE_TABLE_BASE) + PtrUInt(TableOffset))^;

 {Check Level 2 Type}
 if (Level2Entry and ARMV7L_DESCRIPTOR_TYPE_MASK) = ARMV7L_DESCRIPTOR_TYPE_TABLE then
  begin
   {Return Result}
   Result:=Level2Entry;
  end;
end;

{==============================================================================}

function ARMv7LSetPageTableLevel2(Address,TableAddress:PtrUInt;Flags:UInt64):Boolean;
{Set the descriptor for a Level 2 Page Table (2MB)}
{Note: Caller must call ARMv7InvalidateTLB after changes if MMU is enabled}
var
 Count:Integer;

 TableBase:LongWord;
 TableFlags:UInt64;
 TableOffset:LongWord;

 Level3Base:LongWord;
 Level3Offset:LongWord;

 CurrentAddress:UInt64;
 CurrentFlags:UInt64;
 CurrentBase:LongWord;
 CurrentEntry:UInt64;
 CurrentOffset:LongWord;
begin
 {}
 Result:=False;

 {Check Page Table}
 if PAGE_TABLE_SIZE < SIZE_16K then Exit; {Level 2 Page Table is 16KB}
 if Pointer(PAGE_TABLE_BASE) = nil then Exit;

 {Get Table Base}
 TableBase:=(Address and ARMV7L_SECTION_BASE_MASK);
 if TableBase <> Address then Exit; {Must begin on a 2MB boundary}

 {Get Level 3 Base}
 Level3Base:=(TableAddress and ARMV7L_DESCRIPTOR_BASE_MASK);
 if Level3Base <> TableAddress then Exit; {Must begin on a 4KB boundary}

 {Get Table Offset}
 TableOffset:=((TableBase shr 21) shl 3); {Divide Base by 2MB then multiply by 8 to get Offset into Page Table}

 {Read Page Table}
 CurrentEntry:=PUInt64(PtrUInt(PAGE_TABLE_BASE) + PtrUInt(TableOffset))^;

 {Check Level 2 Type}
 if (CurrentEntry and ARMV7L_DESCRIPTOR_TYPE_MASK) = ARMV7L_DESCRIPTOR_TYPE_TABLE then
  begin
   {Current entry is a Level 2 Page Table}
   CurrentBase:=(CurrentEntry and ARMV7L_DESCRIPTOR_NEXT_TABLE_MASK);
   CurrentOffset:=0;

   {Compare existing base with new base}
   if CurrentBase <> Level3Base then
    begin
     {Copy existing table to new table}
     for Count:=0 to 511 do
      begin
       PUInt64(PtrUInt(Level3Base) + PtrUInt(CurrentOffset))^:=PUInt64(PtrUInt(CurrentBase) + PtrUInt(CurrentOffset))^;
       Inc(CurrentOffset,SizeOf(UInt64));
      end;
    end;
  end
 else if (CurrentEntry and ARMV7L_DESCRIPTOR_TYPE_MASK) = ARMV7L_DESCRIPTOR_TYPE_BLOCK then
  begin
   {Current entry is a Section}
   {Convert to Level 2 Page Table}
   CurrentAddress:=(CurrentEntry and ARMV7L_DESCRIPTOR_L2_OUTPUT_MASK);
   CurrentOffset:=0;
   CurrentFlags:=(CurrentEntry and ARMV7L_DESCRIPTOR_ATTRIBUTE_MASK);

   {Create 512 Page (4KB) entries}
   Level3Offset:=0;
   for Count:=0 to 511 do
    begin
     PUInt64(PtrUInt(Level3Base) + PtrUInt(Level3Offset))^:=(CurrentAddress + CurrentOffset) or CurrentFlags or ARMV7L_DESCRIPTOR_TYPE_PAGE;
     Inc(Level3Offset,SizeOf(UInt64));
     Inc(CurrentOffset,SIZE_4K);
    end;
  end;

 {Check Flags}
 TableFlags:=Flags and not(ARMV7L_DESCRIPTOR_NEXT_TABLE_MASK);

 {Write Page Table}
 PUInt64(PtrUInt(PAGE_TABLE_BASE) + PtrUInt(TableOffset))^:=Level3Base or TableFlags or ARMV7L_DESCRIPTOR_TYPE_TABLE;

 Result:=True;
end;

{==============================================================================}

function ARMv7LGetPageTablePage(Address:PtrUInt):UInt64;
{Get the descriptor for a Page Table Page (4KB)}
var
 TableBase:LongWord;
 TableOffset:LongWord;

 Level3Base:LongWord;
 Level2Entry:UInt64;

 PageBase:LongWord;
 PageEntry:UInt64;
 PageOffset:LongWord;
begin
 {}
 {Check Page Table}
 if PAGE_TABLE_SIZE < SIZE_16K then Exit; {Level 2 Page Table is 16KB}
 if Pointer(PAGE_TABLE_BASE) = nil then Exit;

 {Get Table Base}
 TableBase:=(Address and ARMV7L_SECTION_BASE_MASK);

 {Get Table Offset}
 TableOffset:=((TableBase shr 21) shl 3); {Divide Base by 2MB then multiply by 8 to get Offset into Page Table}

 {Read Page Table}
 Level2Entry:=PUInt64(PtrUInt(PAGE_TABLE_BASE) + PtrUInt(TableOffset))^;

 {Check Level 2 Type}
 if (Level2Entry and ARMV7L_DESCRIPTOR_TYPE_MASK) = ARMV7L_DESCRIPTOR_TYPE_TABLE then
  begin
   {Get Level 3 Base}
   Level3Base:=(Level2Entry and ARMV7L_DESCRIPTOR_NEXT_TABLE_MASK);

   {Get Page Base}
   PageBase:=(Address and ARMV7L_PAGE_BASE_MASK);

   {Get Page Offset}
   PageOffset:=(((PageBase shr 12) and $000001FF) shl 3); {Divide Base by 4KB then multiply by 8 to get Offset into Level 3 Page Table}

   {Read Level 3 Page Table}
   PageEntry:=PUInt64(PtrUInt(Level3Base) + PtrUInt(PageOffset))^;

   {Check Level 3 Type}
   if (PageEntry and ARMV7L_DESCRIPTOR_TYPE_MASK) = ARMV7L_DESCRIPTOR_TYPE_PAGE then
    begin
     {Return Result}
     Result:=PageEntry;
    end;
  end;
end;

{==============================================================================}

function ARMv7LSetPageTablePage(Address:PtrUInt;PhysicalRange:LongWord;PhysicalAddress:PtrUInt;Flags:UInt64):Boolean;
{Set the descriptor for a Page Table Page (4KB)}
{Note: Caller must call ARMv7InvalidateTLB after changes if MMU is enabled}
var
 TableBase:LongWord;
 TableOffset:LongWord;

 Level3Base:LongWord;
 Level2Entry:UInt64;

 PageBase:LongWord;
 PageFlags:UInt64;
 PageOffset:LongWord;

 PhysicalBase:UInt64;
begin
 {}
 Result:=False;

 {Check Page Table}
 if PAGE_TABLE_SIZE < SIZE_16K then Exit; {Level 2 Page Table is 16KB}
 if Pointer(PAGE_TABLE_BASE) = nil then Exit;

 {Get Table Base}
 TableBase:=(Address and ARMV7L_SECTION_BASE_MASK);

 {Get Table Offset}
 TableOffset:=((TableBase shr 21) shl 3); {Divide Base by 2MB then multiply by 8 to get Offset into Page Table}

 {Read Page Table}
 Level2Entry:=PUInt64(PtrUInt(PAGE_TABLE_BASE) + PtrUInt(TableOffset))^;

 {Check Level 1 Type}
 if (Level2Entry and ARMV7L_DESCRIPTOR_TYPE_MASK) = ARMV7L_DESCRIPTOR_TYPE_TABLE then
  begin
   {Get Level 3 Base}
   Level3Base:=(Level2Entry and ARMV7L_DESCRIPTOR_NEXT_TABLE_MASK);

   {Get Page Base}
   PageBase:=(Address and ARMV7L_PAGE_BASE_MASK);
   if PageBase <> Address then Exit; {Must begin on a 4KB boundary}

   {Get Physical Base}
   PhysicalBase:=(PhysicalAddress and ARMV7L_PAGE_BASE_MASK);
   if PhysicalBase <> PhysicalAddress then Exit; {Must begin on a 4KB boundary}

   {Add Physical Range}
   Int64Rec(PhysicalBase).Hi:=PhysicalRange; {PhysicalBase:=PhysicalBase or (UInt64(PhysicalRange) shl 32);}

   {Get Page Offset}
   PageOffset:=(((PageBase shr 12) and $000001FF) shl 3); {Divide Base by 4KB then multiply by 8 to get Offset into Level 3 Page Table}

   {Check Flags}
   PageFlags:=Flags and not(ARMV7L_DESCRIPTOR_L3_OUTPUT_MASK);

   {Add Access Flag}
   PageFlags:=PageFlags or ARMV7L_DESCRIPTOR_ATTRIBUTE_AF;

   {Write Level 3 Page Table}
   PUInt64(PtrUInt(Level3Base) + PtrUInt(PageOffset))^:=PhysicalBase or PageFlags or ARMV7L_DESCRIPTOR_TYPE_PAGE;

   Result:=True;
  end;
end;

{==============================================================================}

function ARMv7LGetPageTableSection(Address:PtrUInt):UInt64;
{Get the descriptor for a Page Table Section (2MB)}
var
 SectionBase:LongWord;
 SectionEntry:UInt64;
 SectionOffset:LongWord;
begin
 {}
 Result:=0;

 {Check Page Table}
 if PAGE_TABLE_SIZE < SIZE_16K then Exit; {Level 2 Page Table is 16KB}
 if Pointer(PAGE_TABLE_BASE) = nil then Exit;

 {Get Section Base}
 SectionBase:=(Address and ARMV7L_SECTION_BASE_MASK);

 {Get Section Offset}
 SectionOffset:=((SectionBase shr 21) shl 3); {Divide Base by 2MB then multiply by 8 to get Offset into Page Table}

 {Read Page Table}
 SectionEntry:=PUInt64(PtrUInt(PAGE_TABLE_BASE) + PtrUInt(SectionOffset))^;

 {Check Level 2 Type}
 if (SectionEntry and ARMV7L_DESCRIPTOR_TYPE_MASK) = ARMV7L_DESCRIPTOR_TYPE_BLOCK then
  begin
   {Return Result}
   Result:=SectionEntry;
  end;
end;

{==============================================================================}

function ARMv7LSetPageTableSection(Address:PtrUInt;PhysicalRange:LongWord;PhysicalAddress:PtrUInt;Flags:UInt64):Boolean;
{Set the descriptor for a Page Table Section (2MB)}
{Note: Caller must call ARMv7InvalidateTLB after changes if MMU is enabled}
var
 SectionBase:LongWord;
 SectionFlags:UInt64;
 SectionOffset:LongWord;

 PhysicalBase:UInt64;
begin
 {}
 Result:=False;

 {Check Page Table}
 if PAGE_TABLE_SIZE < SIZE_16K then Exit; {Level 2 Page Table is 16KB}
 if Pointer(PAGE_TABLE_BASE) = nil then Exit;

 {Get Section Base}
 SectionBase:=(Address and ARMV7L_SECTION_BASE_MASK);
 if SectionBase <> Address then Exit; {Must begin on a 2MB boundary}

 {Get Physical Base}
 PhysicalBase:=(PhysicalAddress and ARMV7L_SECTION_BASE_MASK);
 if PhysicalBase <> PhysicalAddress then Exit; {Must begin on a 2MB boundary}

 {Add Physical Range}
 Int64Rec(PhysicalBase).Hi:=PhysicalRange; {PhysicalBase:=PhysicalBase or (UInt64(PhysicalRange) shl 32);}

 {Get Section Offset}
 SectionOffset:=((SectionBase shr 21) shl 3); {Divide Base by 2MB then multiply by 8 to get Offset into Page Table}

 {Check Flags}
 SectionFlags:=Flags and not(ARMV7L_DESCRIPTOR_L2_OUTPUT_MASK);

 {Add Access Flag}
 SectionFlags:=SectionFlags or ARMV7L_DESCRIPTOR_ATTRIBUTE_AF;

 {Write Page Table}
 PUInt64(PtrUInt(PAGE_TABLE_BASE) + PtrUInt(SectionOffset))^:=PhysicalBase or SectionFlags or ARMV7L_DESCRIPTOR_TYPE_BLOCK;

 Result:=True;
end;

{==============================================================================}

function ARMv7LGetPageTableLargeSection(Address:PtrUInt):UInt64;
{Get the descriptor for a Page Table Large Section (1GB)}
var
 SectionBase:LongWord;
 SectionEntry:UInt64;
 SectionOffset:LongWord;
begin
 {}
 Result:=0;

 {Check Page Directory}
 if PAGE_DIRECTORY_SIZE < SIZE_4K then Exit; {Level 1 Page Directory is 4KB}
 if Pointer(PAGE_DIRECTORY_BASE) = nil then Exit;

 {Get Section Base}
 SectionBase:=(Address and ARMV7L_LARGESECTION_BASE_MASK);

 {Get Section Offset}
 SectionOffset:=((SectionBase shr 30) shl 3); {Divide Base by 1GB then multiply by 8 to get Offset into Page Directory}

 {Read Page Directory}
 SectionEntry:=PUInt64(PtrUInt(PAGE_DIRECTORY_BASE) + PtrUInt(SectionOffset))^;

 {Check Level 1 Type}
 if (SectionEntry and ARMV7L_DESCRIPTOR_TYPE_MASK) = ARMV7L_DESCRIPTOR_TYPE_BLOCK then
  begin
   {Return Result}
   Result:=SectionEntry;
  end;
end;

{==============================================================================}

function ARMv7LSetPageTableLargeSection(Address:PtrUInt;PhysicalRange:LongWord;PhysicalAddress:PtrUInt;Flags:UInt64):Boolean;
{Set the descriptor for a Page Table Large Section (1GB)}
{Note: Caller must call ARMv7InvalidateTLB after changes if MMU is enabled}
var
 SectionBase:LongWord;
 SectionFlags:UInt64;
 SectionOffset:LongWord;

 PhysicalBase:UInt64;
begin
 {}
 Result:=False;

 {Check Page Directory}
 if PAGE_DIRECTORY_SIZE < SIZE_4K then Exit; {Level 1 Page Directory is 4KB}
 if Pointer(PAGE_DIRECTORY_BASE) = nil then Exit;

 {Get Section Base}
 SectionBase:=(Address and ARMV7L_LARGESECTION_BASE_MASK);
 if SectionBase <> Address then Exit; {Must begin on a 1GB boundary}

 {Get Physical Base}
 PhysicalBase:=(PhysicalAddress and ARMV7L_LARGESECTION_BASE_MASK);
 if PhysicalBase <> PhysicalAddress then Exit; {Must begin on a 1GB boundary}

 {Add Physical Range}
 Int64Rec(PhysicalBase).Hi:=PhysicalRange; {PhysicalBase:=PhysicalBase or (UInt64(PhysicalRange) shl 32);}

 {Get Section Offset}
 SectionOffset:=((SectionBase shr 30) shl 3); {Divide Base by 1GB then multiply by 8 to get Offset into Page Directory}

 {Check Flags}
 SectionFlags:=Flags and not(ARMV7L_DESCRIPTOR_L1_OUTPUT_MASK);

 {Add Access Flag}
 SectionFlags:=SectionFlags or ARMV7L_DESCRIPTOR_ATTRIBUTE_AF;

 {Write Page Directory}
 PUInt64(PtrUInt(PAGE_DIRECTORY_BASE) + PtrUInt(SectionOffset))^:=PhysicalBase or SectionFlags or ARMV7L_DESCRIPTOR_TYPE_BLOCK;

 Result:=True;
end;

{==============================================================================}
{==============================================================================}

end.
