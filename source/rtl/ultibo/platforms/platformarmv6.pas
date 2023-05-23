{
Ultibo Platform interface unit for ARMv6.

Copyright (C) 2023 - SoftOz Pty Ltd.

Arch
====

 ARMv6 (ARM1176)

Boards
======

 Raspberry Pi - Model A/B/A+/B+/CM1
 Raspberry Pi - Model Zero/ZeroW

Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)
 
Credits
=======

 Information for this unit was obtained from:

 NetBSD - /sys/arch/arm/arm/cpufunc_asm_armv6.S - Copyright 2002, 2005 ARM Limited
 
 Linux - /arch/arm/mm/proc-v6.S - Copyright 2001 Deep Blue Solutions Ltd
         /arch/arm/mm/cache-v6.S - Copyright (C) 2001 Deep Blue Solutions Ltd
 
References
==========

 ARM1176JZF-S Technical Reference Manual
 
 ARM Architecture Reference Manual
 
 ARM Synchronization Primitives (DHT0008A_arm_synchronization_primitives.pdf)
 
 ARM Technical Support Knowledge Articles
  In what situations might I need to insert memory barrier instructions? - http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.faqs/ka14041.html
  
Platform ARMv6
==============

The ARMv6 (ARM11) does not support WFI, WFE, DMB, DSB or ISB instructions, these must be done using MCR operations
on the system control processor registers.

The ARMv6 supports the LDREX/STREX instructions for syncronisation (Lock/Mutex/Semaphore etc) but only if the MMU is enabled.
 
}
              
{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit PlatformARMv6; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,PlatformARM,HeapManager,Threads,SysUtils;
         
{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {ARMv6 specific constants common to all processor models}

 {Page Table Shift}
 ARMV6_PAGE_TABLES_SHIFT = 10;
 
 {Definitions of CP15 C0 (Main ID Register) bits in the system control processor registers}
 ARMV6_CP15_C0_MAINID_IMPLEMENTOR_MASK  = ($FF shl 24);
 ARMV6_CP15_C0_MAINID_VARIANT_MASK      = ($F shl 20);
 ARMV6_CP15_C0_MAINID_ARCHITECTURE_MASK = ($F shl 16);
 ARMV6_CP15_C0_MAINID_PARTNUMBER_MASK   = ($FFF shl 4);
 ARMV6_CP15_C0_MAINID_REVISION_MASK     = ($F shl 0);
 
 ARMV6_CP15_C0_MAINID_IMPLEMENTOR_ARM     = ($41 shl 24);
 ARMV6_CP15_C0_MAINID_ARCHITECTURE_CPUID  = ($F shl 16);
 ARMV6_CP15_C0_MAINID_PARTNUMBER_1176JZSF = ($B76 shl 4);
 
 {Definitions of CP15 C0 (Cache Type Register) bits in the system control processor registers}
 ARMV6_CP15_C0_CTR_CTYPE_MASK  = ($F shl 25); {The Cache type bits provide information about the cache architecture (b1110 in the ARM1176JZF-S processor)}
 ARMV6_CP15_C0_CTR_S           = (1 shl 24);  {S = 1, indicates that the processor has separate instruction and data caches and not a unified cache}
 
 ARMV6_CP15_C0_CTR_DP          = (1 shl 23);  {The P, Page, bit indicates restrictions on page allocation for bits [13:12] of the VA For ARM1176JZF-S processors, the P bit is set if the cache size is greater than 16KB}
 ARMV6_CP15_C0_CTR_DSIZE_MASK  = ($F shl 18); {The Size field indicates the cache size in conjunction with the M bit}
 ARMV6_CP15_C0_CTR_DSIZE_128K  = (8 shl 18);  {128KB cache, not supported}
 ARMV6_CP15_C0_CTR_DSIZE_64K   = (7 shl 18);  {64KB cache}
 ARMV6_CP15_C0_CTR_DSIZE_32K   = (6 shl 18);  {32KB cache}
 ARMV6_CP15_C0_CTR_DSIZE_16K   = (5 shl 18);  {16KB cache}
 ARMV6_CP15_C0_CTR_DSIZE_8K    = (4 shl 18);  {8KB cache} 
 ARMV6_CP15_C0_CTR_DSIZE_4K    = (3 shl 18);  {4KB cache}
 ARMV6_CP15_C0_CTR_DSIZE_2K    = (2 shl 18);  {2KB cache, not supported}
 ARMV6_CP15_C0_CTR_DSIZE_1K    = (1 shl 18);  {1KB cache, not supported}
 ARMV6_CP15_C0_CTR_DSIZE_05K   = (0 shl 18);  {0.5KB cache, not supported}   
 ARMV6_CP15_C0_CTR_DASSOC_MASK = (7 shl 15);  {b010, indicates that the ARM1176JZF-S processor has 4-way associativity}
 ARMV6_CP15_C0_CTR_DM          = (1 shl 14);  {Indicates the cache size and cache associativity values in conjunction with the Size and Assoc fields (In the ARM1176JZF-S processor the M bit is set to 0)}
 ARMV6_CP15_C0_CTR_DLEN_MASK   = (3 shl 12);  {b10, indicates that ARM1176JZF-S processor has a cache line length of 8 words, that is 32 bytes}
 ARMV6_CP15_C0_CTR_DLEN_32     = (2 shl 12);
 
 ARMV6_CP15_C0_CTR_IP          = (1 shl 11);  {The P, Page, bit indicates restrictions on page allocation for bits [13:12] of the VA For ARM1176JZF-S processors, the P bit is set if the cache size is greater than 16KB}
 ARMV6_CP15_C0_CTR_ISIZE_MASK  = ($F shl 6);  {The Size field indicates the cache size in conjunction with the M bit}
 ARMV6_CP15_C0_CTR_ISIZE_128K  = (8 shl 6);   {128KB cache, not supported}
 ARMV6_CP15_C0_CTR_ISIZE_64K   = (7 shl 6);   {64KB cache}
 ARMV6_CP15_C0_CTR_ISIZE_32K   = (6 shl 6);   {32KB cache}
 ARMV6_CP15_C0_CTR_ISIZE_16K   = (5 shl 6);   {16KB cache}
 ARMV6_CP15_C0_CTR_ISIZE_8K    = (4 shl 6);   {8KB cache} 
 ARMV6_CP15_C0_CTR_ISIZE_4K    = (3 shl 6);   {4KB cache}
 ARMV6_CP15_C0_CTR_ISIZE_2K    = (2 shl 6);   {2KB cache, not supported}
 ARMV6_CP15_C0_CTR_ISIZE_1K    = (1 shl 6);   {1KB cache, not supported}
 ARMV6_CP15_C0_CTR_ISIZE_05K   = (0 shl 6);   {0.5KB cache, not supported}   
 ARMV6_CP15_C0_CTR_IASSOC_MASK = (7 shl 3);   {b010, indicates that the ARM1176JZF-S processor has 4-way associativity}
 ARMV6_CP15_C0_CTR_IM          = (1 shl 2);   {Indicates the cache size and cache associativity values in conjunction with the Size and Assoc fields (In the ARM1176JZF-S processor the M bit is set to 0)}
 ARMV6_CP15_C0_CTR_ILEN_MASK   = (3 shl 0);   {b10, indicates that ARM1176JZF-S processor has a cache line length of 8 words, that is 32 bytes} 
 ARMV6_CP15_C0_CTR_ILEN_32     = (2 shl 0);
 
 {Definitions of CP15 C1 (Control Register) bits in the system control processor registers}
 ARMV6_CP15_C1_FA_BIT = (1 shl 29); {Force AP functionality in the MMU is enabled when set to 1 (Default 0)}
 ARMV6_CP15_C1_TR_BIT = (1 shl 28); {TEX remap enabled when set to 1 (TEX[2:1] become page table bits for OS) (Default 0)}
 ARMV6_CP15_C1_EE_BIT = (1 shl 25); {CPSR E bit is set to 1 on an exception when set to 1 (Default 0)}
 ARMV6_CP15_C1_VE_BIT = (1 shl 24); {Interrupt vectors are defined by the VIC interface when set to 1 (Default 0)}
 ARMV6_CP15_C1_XP_BIT = (1 shl 23); {Subpage AP bits disabled when set to 1 (Default 0)}
 ARMV6_CP15_C1_U_BIT  = (1 shl 22); {Unaligned data access support enabled when set to 1 (Default 0). The processor permits unaligned loads and stores and support for mixed endian data is enabled}
 ARMV6_CP15_C1_FI_BIT = (1 shl 21); {Low interrupt latency configuration enabled when set to 1 (Default 0)}
 ARMV6_CP15_C1_L4_BIT = (1 shl 15); {Loads to PC do not set the T bit when set to 1 (ARMv4 behavior) (Default 0)}
 ARMV6_CP15_C1_RR_BIT = (1 shl 14); {Predictable cache replacement strategy by round-robin replacement when set to 1 (Default 0)}
 ARMV6_CP15_C1_V_BIT  = (1 shl 13); {High exception vectors selected when set to 1, address range = 0xFFFF0000-0xFFFF001C (Default 0)}
 ARMV6_CP15_C1_I_BIT  = (1 shl 12); {L1 Instruction Cache enabled when set to 1 (Default 0)}
 ARMV6_CP15_C1_Z_BIT  = (1 shl 11); {Branch prediction enabled when set to 1 (Default 0)}
 ARMV6_CP15_C1_B_BIT  = (1 shl 7);  {Big-endian word-invariant memory system when set to 1 (Default 0)}
 ARMV6_CP15_C1_C_BIT  = (1 shl 2);  {L1 Data cache enabled when set to 1 (Default 0)}
 ARMV6_CP15_C1_A_BIT  = (1 shl 1);  {Strict alignment fault checking enabled when set to 1 (Default 0)}
 ARMV6_CP15_C1_M_BIT  = (1 shl 0);  {MMU enabled when set to 1 (Default 0)}

 {Definitions of CP15 C1 (Auxiliary Control Register) bits in the system control processor registers}
 ARMV6_CP15_C1_AUX_CZ = (1  shl 6); {Controls the restriction of cache size to 16KB}

 {Definitions of CP15 C1 (Coprocessor Access Control Register) bits in the system control processor registers}
 ARMV6_CP15_C1_CP0_NONE = (0 shl 0); {Access denied (Default)}
 ARMV6_CP15_C1_CP0_SYS  = (1 shl 0); {Privileged mode access only}
 ARMV6_CP15_C1_CP0_USER = (3 shl 0); {Privileged and User mode access}
 
 ARMV6_CP15_C1_CP1_NONE = (0 shl 2); {Access denied (Default)}
 ARMV6_CP15_C1_CP1_SYS  = (1 shl 2); {Privileged mode access only}
 ARMV6_CP15_C1_CP1_USER = (3 shl 2); {Privileged and User mode access}

 ARMV6_CP15_C1_CP2_NONE = (0 shl 4); {Access denied (Default)}
 ARMV6_CP15_C1_CP2_SYS  = (1 shl 4); {Privileged mode access only}
 ARMV6_CP15_C1_CP2_USER = (3 shl 4); {Privileged and User mode access}

 ARMV6_CP15_C1_CP3_NONE = (0 shl 6); {Access denied (Default)}
 ARMV6_CP15_C1_CP3_SYS  = (1 shl 6); {Privileged mode access only}
 ARMV6_CP15_C1_CP3_USER = (3 shl 6); {Privileged and User mode access}

 ARMV6_CP15_C1_CP4_NONE = (0 shl 8); {Access denied (Default)}
 ARMV6_CP15_C1_CP4_SYS  = (1 shl 8); {Privileged mode access only}
 ARMV6_CP15_C1_CP4_USER = (3 shl 8); {Privileged and User mode access}

 ARMV6_CP15_C1_CP5_NONE = (0 shl 10); {Access denied (Default)}
 ARMV6_CP15_C1_CP5_SYS  = (1 shl 10); {Privileged mode access only}
 ARMV6_CP15_C1_CP5_USER = (3 shl 10); {Privileged and User mode access}

 ARMV6_CP15_C1_CP6_NONE = (0 shl 12); {Access denied (Default)}
 ARMV6_CP15_C1_CP6_SYS  = (1 shl 12); {Privileged mode access only}
 ARMV6_CP15_C1_CP6_USER = (3 shl 12); {Privileged and User mode access}

 ARMV6_CP15_C1_CP7_NONE = (0 shl 14); {Access denied (Default)}
 ARMV6_CP15_C1_CP7_SYS  = (1 shl 14); {Privileged mode access only}
 ARMV6_CP15_C1_CP7_USER = (3 shl 14); {Privileged and User mode access}
 
 ARMV6_CP15_C1_CP8_NONE = (0 shl 16); {Access denied (Default)}
 ARMV6_CP15_C1_CP8_SYS  = (1 shl 16); {Privileged mode access only}
 ARMV6_CP15_C1_CP8_USER = (3 shl 16); {Privileged and User mode access}

 ARMV6_CP15_C1_CP9_NONE = (0 shl 18); {Access denied (Default)}
 ARMV6_CP15_C1_CP9_SYS  = (1 shl 18); {Privileged mode access only}
 ARMV6_CP15_C1_CP9_USER = (3 shl 18); {Privileged and User mode access}

 ARMV6_CP15_C1_CP10_NONE = (0 shl 20); {Access denied (Default)}
 ARMV6_CP15_C1_CP10_SYS  = (1 shl 20); {Privileged mode access only}
 ARMV6_CP15_C1_CP10_USER = (3 shl 20); {Privileged and User mode access}

 ARMV6_CP15_C1_CP11_NONE = (0 shl 22); {Access denied (Default)}
 ARMV6_CP15_C1_CP11_SYS  = (1 shl 22); {Privileged mode access only}
 ARMV6_CP15_C1_CP11_USER = (3 shl 22); {Privileged and User mode access}

 ARMV6_CP15_C1_CP12_NONE = (0 shl 24); {Access denied (Default)}
 ARMV6_CP15_C1_CP12_SYS  = (1 shl 24); {Privileged mode access only}
 ARMV6_CP15_C1_CP12_USER = (3 shl 24); {Privileged and User mode access}

 ARMV6_CP15_C1_CP13_NONE = (0 shl 26); {Access denied (Default)}
 ARMV6_CP15_C1_CP13_SYS  = (1 shl 26); {Privileged mode access only}
 ARMV6_CP15_C1_CP13_USER = (3 shl 26); {Privileged and User mode access}
 {Coprocessors CP14 (Debug Control) and CP15 (System Control) are not affected by the Coprocessor Access Control Register}
 
 {Definitions of CP15 C1 (Secure Configuration Register) bits in the system control processor registers}
 //To Do

 {Definitions of CP15 C1 (Secure Debug Enable Register) bits in the system control processor registers}
 //To Do

 {Definitions of CP15 C1 (Non-Secure Access Control Register) bits in the system control processor registers}
 //To Do

 {Definitions of CP15 C2 bits in the system control processor registers}
 //To Do
 
 {Definitions of CP15 C2 (Translation Table Base Registers 0 and 1) bits in the system control processor registers}
 ARMV6_CP15_C2_TTBR_BASE_MASK                = $FFFFC000;
 ARMV6_CP15_C2_TTBR_RGN_OUTER_NONCACHED      = (0 shl 3); {Outer Noncacheable (Default)}
 ARMV6_CP15_C2_TTBR_RGN_OUTER_WRITE_ALLOCATE = (1 shl 3); {Outer Write-back, Write Allocate}
 ARMV6_CP15_C2_TTBR_RGN_OUTER_WRITE_THROUGH  = (2 shl 3); {Outer Write-through, No Allocate on Write}
 ARMV6_CP15_C2_TTBR_RGN_OUTER_WRITE_BACK     = (3 shl 3); {Outer Write-back, No Allocate on Write}
 ARMV6_CP15_C2_TTBR_P                        = (1 shl 2); {If the processor supports ECC, it indicates to the memory controller it is enabled or disabled. For ARM1176JZF-S processors this is 0}
 ARMV6_CP15_C2_TTBR_S                        = (1 shl 1); {Indicates the page table walk is to Shared memory if set to 1 (Default 0)}
 ARMV6_CP15_C2_TTBR_C_INNER_CACHED           = (1 shl 0); {Indicates the page table walk is Inner Cacheable if set to 1 (Default 0)}
 
 {Definitions of CP15 C3 (Domain Access Control Register) bits in the system control processor registers}
 ARMV6_CP15_C3_DOMAIN0_NONE     = (0 shl 0); {No access, Any access generates a domain fault (Default)}
 ARMV6_CP15_C3_DOMAIN0_CLIENT   = (1 shl 0); {Client, Accesses are checked against the access permission bits in the TLB entry}
 ARMV6_CP15_C3_DOMAIN0_MANAGER  = (3 shl 0); {Manager, Accesses are not checked against the access permission bits in the TLB entry, so a permission fault cannot be generated}

 ARMV6_CP15_C3_DOMAIN1_NONE     = (0 shl 2); {No access, Any access generates a domain fault (Default)}
 ARMV6_CP15_C3_DOMAIN1_CLIENT   = (1 shl 2); {Client, Accesses are checked against the access permission bits in the TLB entry}
 ARMV6_CP15_C3_DOMAIN1_MANAGER  = (3 shl 2); {Manager, Accesses are not checked against the access permission bits in the TLB entry, so a permission fault cannot be generated}

 ARMV6_CP15_C3_DOMAIN2_NONE     = (0 shl 4); {No access, Any access generates a domain fault (Default)}
 ARMV6_CP15_C3_DOMAIN2_CLIENT   = (1 shl 4); {Client, Accesses are checked against the access permission bits in the TLB entry}
 ARMV6_CP15_C3_DOMAIN2_MANAGER  = (3 shl 4); {Manager, Accesses are not checked against the access permission bits in the TLB entry, so a permission fault cannot be generated}

 ARMV6_CP15_C3_DOMAIN3_NONE     = (0 shl 6); {No access, Any access generates a domain fault (Default)}
 ARMV6_CP15_C3_DOMAIN3_CLIENT   = (1 shl 6); {Client, Accesses are checked against the access permission bits in the TLB entry}
 ARMV6_CP15_C3_DOMAIN3_MANAGER  = (3 shl 6); {Manager, Accesses are not checked against the access permission bits in the TLB entry, so a permission fault cannot be generated}

 ARMV6_CP15_C3_DOMAIN4_NONE     = (0 shl 8); {No access, Any access generates a domain fault (Default)}
 ARMV6_CP15_C3_DOMAIN4_CLIENT   = (1 shl 8); {Client, Accesses are checked against the access permission bits in the TLB entry}
 ARMV6_CP15_C3_DOMAIN4_MANAGER  = (3 shl 8); {Manager, Accesses are not checked against the access permission bits in the TLB entry, so a permission fault cannot be generated}

 ARMV6_CP15_C3_DOMAIN5_NONE     = (0 shl 10); {No access, Any access generates a domain fault (Default)}
 ARMV6_CP15_C3_DOMAIN5_CLIENT   = (1 shl 10); {Client, Accesses are checked against the access permission bits in the TLB entry}
 ARMV6_CP15_C3_DOMAIN5_MANAGER  = (3 shl 10); {Manager, Accesses are not checked against the access permission bits in the TLB entry, so a permission fault cannot be generated}

 ARMV6_CP15_C3_DOMAIN6_NONE     = (0 shl 12); {No access, Any access generates a domain fault (Default)}
 ARMV6_CP15_C3_DOMAIN6_CLIENT   = (1 shl 12); {Client, Accesses are checked against the access permission bits in the TLB entry}
 ARMV6_CP15_C3_DOMAIN6_MANAGER  = (3 shl 12); {Manager, Accesses are not checked against the access permission bits in the TLB entry, so a permission fault cannot be generated}

 ARMV6_CP15_C3_DOMAIN7_NONE     = (0 shl 14); {No access, Any access generates a domain fault (Default)}
 ARMV6_CP15_C3_DOMAIN7_CLIENT   = (1 shl 14); {Client, Accesses are checked against the access permission bits in the TLB entry}
 ARMV6_CP15_C3_DOMAIN7_MANAGER  = (3 shl 14); {Manager, Accesses are not checked against the access permission bits in the TLB entry, so a permission fault cannot be generated}

 ARMV6_CP15_C3_DOMAIN8_NONE     = (0 shl 16); {No access, Any access generates a domain fault (Default)}
 ARMV6_CP15_C3_DOMAIN8_CLIENT   = (1 shl 16); {Client, Accesses are checked against the access permission bits in the TLB entry}
 ARMV6_CP15_C3_DOMAIN8_MANAGER  = (3 shl 16); {Manager, Accesses are not checked against the access permission bits in the TLB entry, so a permission fault cannot be generated}

 ARMV6_CP15_C3_DOMAIN9_NONE     = (0 shl 18); {No access, Any access generates a domain fault (Default)}
 ARMV6_CP15_C3_DOMAIN9_CLIENT   = (1 shl 18); {Client, Accesses are checked against the access permission bits in the TLB entry}
 ARMV6_CP15_C3_DOMAIN9_MANAGER  = (3 shl 18); {Manager, Accesses are not checked against the access permission bits in the TLB entry, so a permission fault cannot be generated}

 ARMV6_CP15_C3_DOMAIN10_NONE    = (0 shl 20); {No access, Any access generates a domain fault (Default)}
 ARMV6_CP15_C3_DOMAIN10_CLIENT  = (1 shl 20); {Client, Accesses are checked against the access permission bits in the TLB entry}
 ARMV6_CP15_C3_DOMAIN10_MANAGER = (3 shl 20); {Manager, Accesses are not checked against the access permission bits in the TLB entry, so a permission fault cannot be generated}

 ARMV6_CP15_C3_DOMAIN11_NONE    = (0 shl 22); {No access, Any access generates a domain fault (Default)}
 ARMV6_CP15_C3_DOMAIN11_CLIENT  = (1 shl 22); {Client, Accesses are checked against the access permission bits in the TLB entry}
 ARMV6_CP15_C3_DOMAIN11_MANAGER = (3 shl 22); {Manager, Accesses are not checked against the access permission bits in the TLB entry, so a permission fault cannot be generated}

 ARMV6_CP15_C3_DOMAIN12_NONE    = (0 shl 24); {No access, Any access generates a domain fault (Default)}
 ARMV6_CP15_C3_DOMAIN12_CLIENT  = (1 shl 24); {Client, Accesses are checked against the access permission bits in the TLB entry}
 ARMV6_CP15_C3_DOMAIN12_MANAGER = (3 shl 24); {Manager, Accesses are not checked against the access permission bits in the TLB entry, so a permission fault cannot be generated}

 ARMV6_CP15_C3_DOMAIN13_NONE    = (0 shl 26); {No access, Any access generates a domain fault (Default)}
 ARMV6_CP15_C3_DOMAIN13_CLIENT  = (1 shl 26); {Client, Accesses are checked against the access permission bits in the TLB entry}
 ARMV6_CP15_C3_DOMAIN13_MANAGER = (3 shl 26); {Manager, Accesses are not checked against the access permission bits in the TLB entry, so a permission fault cannot be generated}

 ARMV6_CP15_C3_DOMAIN14_NONE    = (0 shl 28); {No access, Any access generates a domain fault (Default)}
 ARMV6_CP15_C3_DOMAIN14_CLIENT  = (1 shl 28); {Client, Accesses are checked against the access permission bits in the TLB entry}
 ARMV6_CP15_C3_DOMAIN14_MANAGER = (3 shl 28); {Manager, Accesses are not checked against the access permission bits in the TLB entry, so a permission fault cannot be generated}

 ARMV6_CP15_C3_DOMAIN15_NONE    = (0 shl 30); {No access, Any access generates a domain fault (Default)}
 ARMV6_CP15_C3_DOMAIN15_CLIENT  = (1 shl 30); {Client, Accesses are checked against the access permission bits in the TLB entry}
 ARMV6_CP15_C3_DOMAIN15_MANAGER = (3 shl 30); {Manager, Accesses are not checked against the access permission bits in the TLB entry, so a permission fault cannot be generated}

 {Definitions of CP15 C5 (Data Fault Status Register) bits in the system control processor registers}
 //To Do

 {Definitions of CP15 C5 (Instruction Fault Status Register) bits in the system control processor registers}
 //To Do
 
 {Definitions of CP15 C6 (Data Fault Address Register) bits in the system control processor registers}
 //To Do

 {Definitions of CP15 C6 (Instruction Fault Address Register) bits in the system control processor registers}
 //To Do
 
 {Definitions of CP15 C7 (Cache Operations Register) bits in the system control processor registers}
 {Nothing}
 
 {Definitions of CP15 C8 (TLB Operations Register) bits in the system control processor registers}
 {Nothing}

 {Definitions of CP15 C9 bits in the system control processor registers}
 //To Do

 {Definitions of CP15 C10 bits in the system control processor registers}
 //To Do
 
 {Definitions of CP15 C11 bits in the system control processor registers}
 //To Do

 {Definitions of CP15 C12 bits in the system control processor registers}
 //To Do

 {Definitions of CP15 C13 bits in the system control processor registers}
 //To Do
 
 {Definitions of CP15 C15 bits in the system control processor registers}
 //To Do
 
 {Definitions of bits in the Floating-point System ID register (FPSID)}
 //To Do
 
 {Definitions of bits in the Floating-point Status and Control (FPSCR)}
 //To Do

 {Definitions of bits in the Floating-point Exception register (FPEXC)}
 ARMV6_FPEXC_EN = (1 shl 30); {Floating-point system is enabled and operates normally if set to 1 (Default 0)}
 ARMV6_FPEXC_EX = (1 shl 31); {If EX is set to 0 then only FPSCR and FPEXC need to be preseved on a context switch (Default 0)}

 {Definitions of the Hardware Page Table Descriptors (See page 6-36 of the ARM1176JZF-S Technical Reference Manual)}
 {These formats assume that the XP bit in the C1 control register is set to one to enable the ARMv6 format Page Tables}
 {Level One Descriptor (L1D) Types (See page 6-39 of the ARM1176JZF-S Technical Reference Manual)}
 {Level One Page Table contains 4096 32bit (4 byte) entries for a total size of 16KB}
 ARMV6_L1D_TYPE_COARSE        = 1; {The entry points to a 1MB second-level page table. See page 6-40}
 ARMV6_L1D_TYPE_SECTION       = 2; {The entry points to a either a 1MB Section of memory or a 16MB Supersection of memory}
 ARMV6_L1D_TYPE_SUPERSECTION  = 2; {Bit[18] of the descriptor selects between a Section and a Supersection}
 
 {Level One Descriptor (L1D) Flags (See page 6-39 of the ARM1176JZF-S Technical Reference Manual)}
 ARMV6_L1D_FLAG_COARSE_NS     = (1 shl 3);  {NS (Non Secure) Attribute bit to enable the support of TrustZone}
 ARMV6_L1D_FLAG_SECTION_NS    = (1 shl 19); {NS (Non Secure) Attribute bit to enable the support of TrustZone}
 ARMV6_L1D_FLAG_SUPERSECTION  = (1 shl 18); {The descriptor is a 16MB Supersection instead of a 1MB Section (Section Only)}
 ARMV6_L1D_FLAG_NOT_GLOBAL    = (1 shl 17); {The Not-Global (nG) bit, determines if the translation is marked as global (0), or process-specific (1) (Section Only)}
 ARMV6_L1D_FLAG_SHARED        = (1 shl 16); {The Shared (S) bit, determines if the translation is for Non-Shared (0), or Shared (1) memory. This only applies to Normal memory regions. 
                                             Device memory can be Shared or Non-Shared as determined by the TEX bits and the C and B bits (Section Only)}
 ARMV6_L1D_FLAG_APX           = (1 shl 15); {The access permissions extension (APX) bit, provides an extra access permission bit (Section Only)}
 ARMV6_L1D_FLAG_P             = (1 shl 9);  {If the P bit is supported and set for the memory region, it indicates to the system memory controller that this memory region has ECC enabled. ARM1176JZF-S processors do not support the P bit}
 ARMV6_L1D_FLAG_XN            = (1 shl 4);  {The Execute-Never (XN) bit, determines if the region is Executable (0) or Not-executable(1) (Section Only)}
 ARMV6_L1D_FLAG_C             = (1 shl 3);  {Cacheable (C) bit (Section Only)}
 ARMV6_L1D_FLAG_B             = (1 shl 2);  {Bufferable (B) bit (Section Only)}
 
 {Level One Descriptor (L1D) Masks (See page 6-39 of the ARM1176JZF-S Technical Reference Manual)}
 ARMV6_L1D_COARSE_BASE_MASK       = $FFFFFC00;
 ARMV6_L1D_SECTION_BASE_MASK      = $FFF00000;  
 ARMV6_L1D_SUPERSECTION_BASE_MASK = $FF000000;  
 ARMV6_L1D_DOMAIN_MASK            = ($F shl 5); {Security Domain of the Descriptor}
 ARMV6_L1D_TEX_MASK               = (7 shl 12); {Type extension field bits (Section Only)}  
 ARMV6_L1D_AP_MASK                = (3 shl 10); {Access permission bits (Section Only)}
 
 {Level One Descriptor (L1D) TEX Values (See page 6-15 of the ARM1176JZF-S Technical Reference Manual) (Section Only)}
 ARMV6_L1D_TEX0 = (0 shl 12);
 ARMV6_L1D_TEX1 = (1 shl 12); 
 ARMV6_L1D_TEX2 = (2 shl 12); 
 ARMV6_L1D_TEX4 = (4 shl 12); {Only used for Cacheable memory values}
 ARMV6_L1D_TEX5 = (5 shl 12); {Only used for Cacheable memory values}
 ARMV6_L1D_TEX6 = (6 shl 12); {Only used for Cacheable memory values} 
 ARMV6_L1D_TEX7 = (7 shl 12); {Only used for Cacheable memory values}
 
 {Level One Descriptor (L1D) AP Values (See page 6-12 of the ARM1176JZF-S Technical Reference Manual) (Section Only)}
 ARMV6_L1D_AP0 = (0 shl 10);
 ARMV6_L1D_AP1 = (1 shl 10); 
 ARMV6_L1D_AP2 = (2 shl 10); 
 ARMV6_L1D_AP3 = (3 shl 10);

 {Level One Descriptor (L1D) Permission Values (See page 6-12 of the ARM1176JZF-S Technical Reference Manual)}
 {This is not the full set of permissions as Ultibo always runs in priviledged mode}
 {The XN bit can also be applied to control whether memory regions are executable or not}
 ARMV6_L1D_ACCESS_NONE      = ARMV6_L1D_AP0;                        {No Access for both Privileged and Unprivileged code}
 ARMV6_L1D_ACCESS_READONLY  = ARMV6_L1D_FLAG_APX or ARMV6_L1D_AP3;  {Read-Only for both Privileged and Unprivileged code}
 ARMV6_L1D_ACCESS_READWRITE = ARMV6_L1D_AP3;                        {Read-Write for both Privileged and Unprivileged code}
 
 {Level One Descriptor (L1D) Cache Values (See page 6-15 of the ARM1176JZF-S Technical Reference Manual)}
 ARMV6_L1D_CACHE_STRONGLY_ORDERED      = ARMV6_L1D_TEX0;                                         {Strongly Ordered. (Always Shared)}
 ARMV6_L1D_CACHE_SHARED_DEVICE         = ARMV6_L1D_TEX0 or ARMV6_L1D_FLAG_B;                     {Device. (Always Shared)}
 ARMV6_L1D_CACHE_NORMAL_WRITE_THROUGH  = ARMV6_L1D_TEX0 or ARMV6_L1D_FLAG_C;                     {Normal. Write Through (Shared if S bit set)}
 ARMV6_L1D_CACHE_NORMAL_WRITE_BACK     = ARMV6_L1D_TEX0 or ARMV6_L1D_FLAG_C or ARMV6_L1D_FLAG_B; {Normal. Write Back (Shared if S bit set)}
 ARMV6_L1D_CACHE_NORMAL_NONCACHED      = ARMV6_L1D_TEX1;                                         {Normal. Noncacheable (Shared if S bit set)}
 ARMV6_L1D_CACHE_NONSHARED_DEVICE      = ARMV6_L1D_TEX2;                                         {Device. (Not Shared}
 
 {Level One Descriptor (L1D) Cache Values (Cacheable Memory)(See page 6-15 of the ARM1176JZF-S Technical Reference Manual)}
 //To Do
 
 {Level One Descriptor (L1D) Cache Values (TEX Remap Enabled)(See page 6-15 of the ARM1176JZF-S Technical Reference Manual)(These values are from Linux)}
 //To Do
 
 {Level Two Descriptor (L2D) Types (See page 6-40 of the ARM1176JZF-S Technical Reference Manual)}
 {Level Two Page Table contains 256 32bit (4 byte) entries for a total size of 1KB}
 ARMV6_L2D_TYPE_LARGE         = 1; {The entry points to a 64KB Large page in memory}
 ARMV6_L2D_TYPE_SMALL         = 2; {The entry points to a 4KB Extended small page in memory. Bit[0] of the entry is the XN (Execute Never) bit for the entry}

 {Level Two Descriptor (L2D) Flags (See page 6-40 of the ARM1176JZF-S Technical Reference Manual)}
 ARMV6_L2D_FLAG_LARGE_XN      = (1 shl 15); {The Execute-Never (XN) bit, determines if the region is Executable (0) or Not-executable(1)}
 ARMV6_L2D_FLAG_SMALL_XN      = (1 shl 0);  {The Execute-Never (XN) bit, determines if the region is Executable (0) or Not-executable(1)}
 ARMV6_L2D_FLAG_NOT_GLOBAL    = (1 shl 11); {The Not-Global (nG) bit, determines if the translation is marked as global (0), or process-specific (1)}
 ARMV6_L2D_FLAG_SHARED        = (1 shl 10); {The Shared (S) bit, determines if the translation is for Non-Shared (0), or Shared (1) memory. This only applies to Normal memory regions. 
                                             Device memory can be Shared or Non-Shared as determined by the TEX bits and the C and B bits}
 ARMV6_L2D_FLAG_APX           = (1 shl 9);  {The access permissions extension (APX) bit, provides an extra access permission bit}
 ARMV6_L2D_FLAG_C             = (1 shl 3);  {Cacheable (C) bit}
 ARMV6_L2D_FLAG_B             = (1 shl 2);  {Bufferable (B) bit}
 
 {Level Two Descriptor (L2D) Masks (See page 6-40 of the ARM1176JZF-S Technical Reference Manual)}
 ARMV6_L2D_LARGE_BASE_MASK   = $FFFF0000;
 ARMV6_L2D_SMALL_BASE_MASK   = $FFFFF000;
 ARMV6_L2D_LARGE_TEX_MASK    = (7 shl 12); {Type extension field bits}          
 ARMV6_L2D_SMALL_TEX_MASK    = (7 shl 6);  {Type extension field bits}  
 ARMV6_L2D_AP_MASK           = (3 shl 4);  {Access permission bits}

 {Level Two Descriptor (L2D) Large TEX Values (See page 6-15 of the ARM1176JZF-S Technical Reference Manual)}
 ARMV6_L2D_LARGE_TEX0 = (0 shl 12);
 ARMV6_L2D_LARGE_TEX1 = (1 shl 12); 
 ARMV6_L2D_LARGE_TEX2 = (2 shl 12); 
 ARMV6_L2D_LARGE_TEX4 = (4 shl 12); 
 ARMV6_L2D_LARGE_TEX5 = (5 shl 12); {Only used for Cacheable memory values}
 ARMV6_L2D_LARGE_TEX6 = (6 shl 12); {Only used for Cacheable memory values} 
 ARMV6_L2D_LARGE_TEX7 = (7 shl 12); {Only used for Cacheable memory values}

 {Level Two Descriptor (L2D) Small TEX Values (See page 6-15 of the ARM1176JZF-S Technical Reference Manual)}
 ARMV6_L2D_SMALL_TEX0 = (0 shl 6);
 ARMV6_L2D_SMALL_TEX1 = (1 shl 6); 
 ARMV6_L2D_SMALL_TEX2 = (2 shl 6); 
 ARMV6_L2D_SMALL_TEX4 = (4 shl 6); 
 ARMV6_L2D_SMALL_TEX5 = (5 shl 6); {Only used for Cacheable memory values}
 ARMV6_L2D_SMALL_TEX6 = (6 shl 6); {Only used for Cacheable memory values} 
 ARMV6_L2D_SMALL_TEX7 = (7 shl 6); {Only used for Cacheable memory values}
 
 {Level Two Descriptor (L2D) AP Values (See page 6-12 of the ARM1176JZF-S Technical Reference Manual)}
 ARMV6_L2D_AP0 = (0 shl 4);
 ARMV6_L2D_AP1 = (1 shl 4); 
 ARMV6_L2D_AP2 = (2 shl 4); 
 ARMV6_L2D_AP3 = (3 shl 4);
 
 {Level Two Descriptor (L2D) Permission Values (See page 6-12 of the ARM1176JZF-S Technical Reference Manual)}
 {This is not the full set of permissions as Ultibo always runs in priviledged mode}
 {The XN bit can also be applied to control whether memory regions are executable or not}
 ARMV6_L2D_ACCESS_NONE      = ARMV6_L2D_AP0;                        {No Access for both Privileged and Unprivileged code}
 ARMV6_L2D_ACCESS_READONLY  = ARMV6_L2D_FLAG_APX or ARMV6_L2D_AP3;  {Read-Only for both Privileged and Unprivileged code}
 ARMV6_L2D_ACCESS_READWRITE = ARMV6_L2D_AP3;                        {Read-Write for both Privileged and Unprivileged code}
 
 {Level Two Descriptor (L2D) Large Cache Values (See page 6-15 of the ARM1176JZF-S Technical Reference Manual)}
 ARMV6_L2D_LARGE_CACHE_STRONGLY_ORDERED      = ARMV6_L2D_LARGE_TEX0;                                         {Strongly Ordered. (Always Shared)}
 ARMV6_L2D_LARGE_CACHE_SHARED_DEVICE         = ARMV6_L2D_LARGE_TEX0 or ARMV6_L2D_FLAG_B;                     {Device. (Always Shared)}
 ARMV6_L2D_LARGE_CACHE_NORMAL_WRITE_THROUGH  = ARMV6_L2D_LARGE_TEX0 or ARMV6_L2D_FLAG_C;                     {Normal. Write Through (Shared if S bit set)}
 ARMV6_L2D_LARGE_CACHE_NORMAL_WRITE_BACK     = ARMV6_L2D_LARGE_TEX0 or ARMV6_L2D_FLAG_C or ARMV6_L2D_FLAG_B; {Normal. Write Back (Shared if S bit set)}
 ARMV6_L2D_LARGE_CACHE_NORMAL_NONCACHED      = ARMV6_L2D_LARGE_TEX1;                                         {Normal. Noncacheable (Shared if S bit set)}
 ARMV6_L2D_LARGE_CACHE_NONSHARED_DEVICE      = ARMV6_L2D_LARGE_TEX2;                                         {Device. (Not Shared}
 
 {Level Two Descriptor (L2D) Large Cache Values (Cacheable Memory)(See page 6-15 of the ARM1176JZF-S Technical Reference Manual)}
 //To Do

 {Level Two Descriptor (L2D) Large Cache Values (TEX Remap Enabled)(See page 6-15 of the ARM1176JZF-S Technical Reference Manual)(These values are from Linux)}
 //To Do
 
 {Level Two Descriptor (L2D) Small Cache Values (See page 6-15 of the ARM1176JZF-S Technical Reference Manual)}
 ARMV6_L2D_SMALL_CACHE_STRONGLY_ORDERED      = ARMV6_L2D_SMALL_TEX0;                                         {Strongly Ordered. (Always Shared)}
 ARMV6_L2D_SMALL_CACHE_SHARED_DEVICE         = ARMV6_L2D_SMALL_TEX0 or ARMV6_L2D_FLAG_B;                     {Device. (Always Shared)}
 ARMV6_L2D_SMALL_CACHE_NORMAL_WRITE_THROUGH  = ARMV6_L2D_SMALL_TEX0 or ARMV6_L2D_FLAG_C;                     {Normal. Write Through (Shared if S bit set)}
 ARMV6_L2D_SMALL_CACHE_NORMAL_WRITE_BACK     = ARMV6_L2D_SMALL_TEX0 or ARMV6_L2D_FLAG_C or ARMV6_L2D_FLAG_B; {Normal. Write Back (Shared if S bit set)}
 ARMV6_L2D_SMALL_CACHE_NORMAL_NONCACHED      = ARMV6_L2D_SMALL_TEX1;                                         {Normal. Noncacheable (Shared if S bit set)}
 ARMV6_L2D_SMALL_CACHE_NONSHARED_DEVICE      = ARMV6_L2D_SMALL_TEX2;                                         {Device. (Not Shared}

 {Level Two Descriptor (L2D) Small Cache Values (Cacheable Memory)(See page 6-15 of the ARM1176JZF-S Technical Reference Manual)}
 //To Do

 {Level Two Descriptor (L2D) Small Cache Values (TEX Remap Enabled)(See page 6-15 of the ARM1176JZF-S Technical Reference Manual)(These values are from Linux)}
 //To Do
 
{==============================================================================}
const
 {ARMv6 specific constants}
 
 {Length of ARM context switch record in 32 bit words (includes fpexc, fpscr, d0-d15, r0-r12, lr, pc, cpsr)}
 ARMV6_CONTEXT_LENGTH = 50; 
 
{==============================================================================}
type
 {ARMv6 specific types}

 {Prototypes for Page Table Handlers}
 TARMv6PageTableInit = procedure;
 
 {Prototypes for IRQ Handlers}
 TARMv6DispatchIRQ = function(CPUID:LongWord;Thread:TThreadHandle):TThreadHandle;
 
 {Prototypes for FIQ Handlers}
 TARMv6DispatchFIQ = function(CPUID:LongWord;Thread:TThreadHandle):TThreadHandle;
 
 {Prototypes for SWI Handlers}
 TARMv6DispatchSWI = function(CPUID:LongWord;Thread:TThreadHandle;Request:PSystemCallRequest):TThreadHandle; 
 
{==============================================================================}
var
 {ARMv6 specific variables}
 ARMv6DummySTREX:LongWord;               {Variable to allow a dummy STREX operation to be performed after each context switch as required by ARM documentation}
 
var
 {Page Table Handlers}
 ARMv6PageTableInitHandler:TARMv6PageTableInit;
 
var
 {IRQ Handlers}
 ARMv6DispatchIRQHandler:TARMv6DispatchIRQ;
 
var
 {FIQ Handlers}
 ARMv6DispatchFIQHandler:TARMv6DispatchFIQ;
 
var
 {SWI Handlers}
 ARMv6DispatchSWIHandler:TARMv6DispatchSWI;
 
{==============================================================================}
{Initialization Functions}
procedure ARMv6Init;

{==============================================================================}
{ARMv6 Platform Functions}
procedure ARMv6CPUInit;
procedure ARMv6FPUInit;
procedure ARMv6MMUInit;

procedure ARMv6CacheInit;

procedure ARMv6PageTableInit;

procedure ARMv6SystemCall(Number:LongWord;Param1,Param2,Param3:PtrUInt);

function ARMv6CPUGetMode:LongWord;
function ARMv6CPUGetState:LongWord;

function ARMv6CPUGetMainID:LongWord;
function ARMv6CPUGetModel:LongWord;
function ARMv6CPUGetRevision:LongWord;
function ARMv6CPUGetDescription:String;

function ARMv6FPUGetState:LongWord;

function ARMv6L1CacheGetType:LongWord;
function ARMv6L1DataCacheGetSize:LongWord;
function ARMv6L1DataCacheGetLineSize:LongWord;  
function ARMv6L1InstructionCacheGetSize:LongWord;
function ARMv6L1InstructionCacheGetLineSize:LongWord;  

procedure ARMv6Halt;
procedure ARMv6Pause;

procedure ARMv6WaitForEvent; 
procedure ARMv6WaitForInterrupt; 

procedure ARMv6DataMemoryBarrier;
procedure ARMv6DataSynchronizationBarrier;
procedure ARMv6InstructionMemoryBarrier;

procedure ARMv6InvalidateTLB;
procedure ARMv6InvalidateDataTLB;
procedure ARMv6InvalidateInstructionTLB;

procedure ARMv6InvalidateCache;
procedure ARMv6CleanDataCache;
procedure ARMv6InvalidateDataCache;
procedure ARMv6CleanAndInvalidateDataCache;
procedure ARMv6InvalidateInstructionCache;

procedure ARMv6CleanDataCacheRange(Address:PtrUInt;Size:LongWord); 
procedure ARMv6InvalidateDataCacheRange(Address:PtrUInt;Size:LongWord);
procedure ARMv6CleanAndInvalidateDataCacheRange(Address:PtrUInt;Size:LongWord);
procedure ARMv6InvalidateInstructionCacheRange(Address:PtrUInt;Size:LongWord); 

procedure ARMv6FlushPrefetchBuffer;

procedure ARMv6FlushBranchTargetCache;

procedure ARMv6ContextSwitch(OldStack,NewStack:Pointer;NewThread:TThreadHandle); 
procedure ARMv6ContextSwitchIRQ(OldStack,NewStack:Pointer;NewThread:TThreadHandle); 
procedure ARMv6ContextSwitchFIQ(OldStack,NewStack:Pointer;NewThread:TThreadHandle); 
procedure ARMv6ContextSwitchSWI(OldStack,NewStack:Pointer;NewThread:TThreadHandle); 

function ARMv6InterlockedOr(var Target:LongInt;Value:LongInt):LongInt;
function ARMv6InterlockedXor(var Target:LongInt;Value:LongInt):LongInt;
function ARMv6InterlockedAnd(var Target:LongInt;Value:LongInt):LongInt;

function ARMv6InterlockedDecrement(var Target:LongInt):LongInt;
function ARMv6InterlockedIncrement(var Target:LongInt):LongInt; 
function ARMv6InterlockedExchange(var Target:LongInt;Source:LongInt):LongInt; 
function ARMv6InterlockedAddExchange(var Target:LongInt;Source:LongInt):LongInt; 
function ARMv6InterlockedCompareExchange(var Target:LongInt;Source,Compare:LongInt):LongInt;

procedure ARMv6PageTableGetEntry(Address:PtrUInt;var Entry:TPageTableEntry);
function ARMv6PageTableSetEntry(const Entry:TPageTableEntry):LongWord; 

function ARMv6VectorTableGetEntry(Number:LongWord):PtrUInt;
function ARMv6VectorTableSetEntry(Number:LongWord;Address:PtrUInt):LongWord;

function ARMv6FirstBitSet(Value:LongWord):LongWord;
function ARMv6CountLeadingZeros(Value:LongWord):LongWord;

{==============================================================================}
{ARMv6 Thread Functions}
procedure ARMv6PrimaryInit;

function ARMv6SpinLock(Spin:PSpinEntry):LongWord;
function ARMv6SpinUnlock(Spin:PSpinEntry):LongWord;

function ARMv6SpinLockIRQ(Spin:PSpinEntry):LongWord;
function ARMv6SpinUnlockIRQ(Spin:PSpinEntry):LongWord;

function ARMv6SpinLockFIQ(Spin:PSpinEntry):LongWord;
function ARMv6SpinUnlockFIQ(Spin:PSpinEntry):LongWord;

function ARMv6SpinLockIRQFIQ(Spin:PSpinEntry):LongWord;
function ARMv6SpinUnlockIRQFIQ(Spin:PSpinEntry):LongWord;

function ARMv6SpinCheckIRQ(Spin:PSpinEntry):Boolean;
function ARMv6SpinCheckFIQ(Spin:PSpinEntry):Boolean;
 
function ARMv6SpinExchangeIRQ(Spin1,Spin2:PSpinEntry):LongWord;
function ARMv6SpinExchangeFIQ(Spin1,Spin2:PSpinEntry):LongWord;

function ARMv6MutexLock(Mutex:PMutexEntry):LongWord;
function ARMv6MutexUnlock(Mutex:PMutexEntry):LongWord;
function ARMv6MutexTryLock(Mutex:PMutexEntry):LongWord;

function ARMv6ThreadGetCurrent:TThreadHandle; 
function ARMv6ThreadSetCurrent(Thread:TThreadHandle):LongWord;

function ARMv6ThreadSetupStack(StackBase:Pointer;StartProc:TThreadStart;ReturnProc:TThreadEnd;Parameter:Pointer):Pointer;

{==============================================================================}
{ARMv6 IRQ Functions}
function ARMv6DispatchIRQ(CPUID:LongWord;Thread:TThreadHandle):TThreadHandle; inline;

{==============================================================================}
{ARMv6 FIQ Functions}
function ARMv6DispatchFIQ(CPUID:LongWord;Thread:TThreadHandle):TThreadHandle; inline;

{==============================================================================}
{ARMv6 SWI Functions}
function ARMv6DispatchSWI(CPUID:LongWord;Thread:TThreadHandle;Request:PSystemCallRequest):TThreadHandle; inline;

{==============================================================================}
{ARMv6 Interrupt Functions}
procedure ARMv6ResetHandler;     
procedure ARMv6UndefinedInstructionHandler;     
procedure ARMv6SoftwareInterruptHandler;       
procedure ARMv6PrefetchAbortHandler;  
procedure ARMv6DataAbortHandler;     
procedure ARMv6ReservedHandler;  
procedure ARMv6IRQHandler;
procedure ARMv6FIQHandler;

{==============================================================================}
{ARMv6 Helper Functions}
function ARMv6GetFPEXC:LongWord;
function ARMv6GetFPSCR:LongWord;

procedure ARMv6StartMMU;

function ARMv6GetPageTableCoarse(Address:PtrUInt):LongWord;
function ARMv6SetPageTableCoarse(Address,CoarseAddress:PtrUInt;Flags:Word):Boolean;

function ARMv6GetPageTableLarge(Address:PtrUInt):LongWord;
function ARMv6SetPageTableLarge(Address,PhysicalAddress:PtrUInt;Flags:Word):Boolean;

function ARMv6GetPageTableSmall(Address:PtrUInt):LongWord;
function ARMv6SetPageTableSmall(Address,PhysicalAddress:PtrUInt;Flags:Word):Boolean;

function ARMv6GetPageTableSection(Address:PtrUInt):LongWord;
function ARMv6SetPageTableSection(Address,PhysicalAddress:PtrUInt;Flags:LongWord):Boolean;
function ARMv6SetPageTableSupersection(Address,PhysicalAddress:PtrUInt;Flags:LongWord):Boolean;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {ARMv6 specific variables}
 ARMv6Initialized:Boolean;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure ARMv6Init;
begin
 {}
 if ARMv6Initialized then Exit;
 
 {Setup PAGE_TABLES_SHIFT}
 PAGE_TABLES_SHIFT:=ARMV6_PAGE_TABLES_SHIFT;
 
 {Setup SPIN_SHARED_MEMORY} 
 SPIN_SHARED_MEMORY:=False;
 if not(HEAP_NORMAL_SHARED) and (CPUGetCount > 1) then SPIN_SHARED_MEMORY:=True;
 
 {Setup MUTEX_SHARED_MEMORY} 
 MUTEX_SHARED_MEMORY:=False;
 if not(HEAP_NORMAL_SHARED) and (CPUGetCount > 1) then MUTEX_SHARED_MEMORY:=True;
 
 {Setup HEAP_REQUEST_ALIGNMENT}
 HEAP_REQUEST_ALIGNMENT:=SIZE_1M;
 
 {Register Platform CPUInit Handler}
 CPUInitHandler:=ARMv6CPUInit;
 
 {Register Platform FPUInit Handler}
 FPUInitHandler:=ARMv6FPUInit;

 {Register Platform MMUInit Handler}
 MMUInitHandler:=ARMv6MMUInit;

 {Register Platform CacheInit Handler}
 CacheInitHandler:=ARMv6CacheInit;
 
 {Register Platform System Handlers}
 SystemCallHandler:=ARMv6SystemCall;
 
 {Register Platform CPU Handlers}
 CPUGetModeHandler:=ARMv6CPUGetMode;
 CPUGetStateHandler:=ARMv6CPUGetState;
 CPUGetModelHandler:=ARMv6CPUGetModel;
 CPUGetRevisionHandler:=ARMv6CPUGetRevision;
 CPUGetDescriptionHandler:=ARMv6CPUGetDescription;
 
 {Register Platform FPU Handlers}
 FPUGetStateHandler:=ARMv6FPUGetState;

 {Register Platform Cache Handlers}
 L1CacheGetTypeHandler:=ARMv6L1CacheGetType;
 L1DataCacheGetSizeHandler:=ARMv6L1DataCacheGetSize;
 L1DataCacheGetLineSizeHandler:=ARMv6L1DataCacheGetLineSize;
 L1InstructionCacheGetSizeHandler:=ARMv6L1InstructionCacheGetSize;
 L1InstructionCacheGetLineSizeHandler:=ARMv6L1InstructionCacheGetLineSize;
 
 {Register Platform Halt Handler}
 HaltHandler:=ARMv6Halt;
 
 {Register Platform Pause Handler}
 PauseHandler:=ARMv6Pause;
 
 {Register Platform WaitForEvent/Interrupt Handlers}
 WaitForEventHandler:=ARMv6WaitForEvent; 
 WaitForInterruptHandler:=ARMv6WaitForInterrupt;
 
 {Register Platform Barrier Handlers}
 ReadMemoryBarrierHandler:=ARMv6DataMemoryBarrier;
 WriteMemoryBarrierHandler:=ARMv6DataMemoryBarrier;
 DataMemoryBarrierHandler:=ARMv6DataMemoryBarrier;
 DataSynchronizationBarrierHandler:=ARMv6DataSynchronizationBarrier;
 InstructionMemoryBarrierHandler:=ARMv6InstructionMemoryBarrier;
 
 {Register Platform TLB Handlers}
 InvalidateTLBHandler:=ARMv6InvalidateTLB;
 InvalidateDataTLBHandler:=ARMv6InvalidateDataTLB;
 InvalidateInstructionTLBHandler:=ARMv6InvalidateInstructionTLB;
 
 {Register Platform Cache Handlers}
 InvalidateCacheHandler:=ARMv6InvalidateCache;
 CleanDataCacheHandler:=ARMv6CleanDataCache;
 InvalidateDataCacheHandler:=ARMv6InvalidateDataCache;
 CleanAndInvalidateDataCacheHandler:=ARMv6CleanAndInvalidateDataCache;
 InvalidateInstructionCacheHandler:=ARMv6InvalidateInstructionCache;
 
 CleanDataCacheRangeHandler:=ARMv6CleanDataCacheRange;
 InvalidateDataCacheRangeHandler:=ARMv6InvalidateDataCacheRange;
 CleanAndInvalidateDataCacheRangeHandler:=ARMv6CleanAndInvalidateDataCacheRange;
 InvalidateInstructionCacheRangeHandler:=ARMv6InvalidateInstructionCacheRange;
 
 {Register Platform PrefetchBuffer Handlers}
 FlushPrefetchBufferHandler:=ARMv6FlushPrefetchBuffer;
 
 {Register Platform BranchTargetCache Handlers}
 FlushBranchTargetCacheHandler:=ARMv6FlushBranchTargetCache;

 {Register Platform ContextSwitch Handlers}
 ContextSwitchHandler:=ARMv6ContextSwitch;
 ContextSwitchIRQHandler:=ARMv6ContextSwitchIRQ; 
 ContextSwitchFIQHandler:=ARMv6ContextSwitchFIQ; 
 ContextSwitchSWIHandler:=ARMv6ContextSwitchSWI; 
 
 {Register Platform And/Xor/Or/Increment/Decrement/Exchange Handlers}
 InterlockedOrHandler:=ARMv6InterlockedOr;
 InterlockedXorHandler:=ARMv6InterlockedXor;
 InterlockedAndHandler:=ARMv6InterlockedAnd;
 
 InterlockedDecrementHandler:=ARMv6InterlockedDecrement;
 InterlockedIncrementHandler:= ARMv6InterlockedIncrement;
 InterlockedExchangeHandler:=ARMv6InterlockedExchange;
 InterlockedAddExchangeHandler:=ARMv6InterlockedAddExchange;
 InterlockedCompareExchangeHandler:=ARMv6InterlockedCompareExchange;
 
 {Register Platform PageTable Handlers}
 PageTableGetEntryHandler:=ARMv6PageTableGetEntry;
 PageTableSetEntryHandler:=ARMv6PageTableSetEntry;

 {Register Platform VectorTable Handlers}
 VectorTableGetEntryHandler:=ARMv6VectorTableGetEntry;
 VectorTableSetEntryHandler:=ARMv6VectorTableSetEntry;
 
 {Register Threads PrimaryInit Handler}
 PrimaryInitHandler:=ARMv6PrimaryInit;
 
 {Register Threads SpinLock/Unlock Handlers}
 SpinLockHandler:=ARMv6SpinLock;
 SpinUnlockHandler:=ARMv6SpinUnlock;

 SpinLockIRQHandler:=ARMv6SpinLockIRQ;
 SpinUnlockIRQHandler:=ARMv6SpinUnlockIRQ;

 SpinLockFIQHandler:=ARMv6SpinLockFIQ;
 SpinUnlockFIQHandler:=ARMv6SpinUnlockFIQ;

 SpinLockIRQFIQHandler:=ARMv6SpinLockIRQFIQ;
 SpinUnlockIRQFIQHandler:=ARMv6SpinUnlockIRQFIQ;

 SpinCheckIRQHandler:=ARMv6SpinCheckIRQ;
 SpinCheckFIQHandler:=ARMv6SpinCheckFIQ;
 
 SpinExchangeIRQHandler:=ARMv6SpinExchangeIRQ;
 SpinExchangeFIQHandler:=ARMv6SpinExchangeFIQ;

 {Register Threads MutexLock/Unlock Handlers}
 MutexLockHandler:=ARMv6MutexLock;
 MutexUnlockHandler:=ARMv6MutexUnlock;
 MutexTryLockHandler:=ARMv6MutexTryLock;

 {Register Threads ThreadGet/SetCurrent Handler}
 ThreadGetCurrentHandler:=ARMv6ThreadGetCurrent; 
 ThreadSetCurrentHandler:=ARMv6ThreadSetCurrent;

 {Register Threads ThreadSetupStack Handler}
 ThreadSetupStackHandler:=ARMv6ThreadSetupStack;

 {Register Global Handlers}
 FirstBitSetHandler:=ARMv6FirstBitSet;
 CountLeadingZerosHandler:=ARMv6CountLeadingZeros;

 ARMv6Initialized:=True;
end;

{==============================================================================}
{==============================================================================}
{ARMv6 Platform Functions}
procedure ARMv6CPUInit; assembler; nostackframe;
asm
 //Load the c13 (Thread and process ID) register in system control coprocessor CP15 with INVALID_HANDLE_VALUE.
 mov r0, #INVALID_HANDLE_VALUE
 mcr p15, #0, r0, cr13, cr0, #4
 
 //Enable L1 Instruction Caching and Branch Prediction by setting the I and Z bits in the C1 control register.
 //Also set the U bit to enable unaligned data access which may have already been set by the startup handler.
 //See page 3-46 of the ARM1176JZF-S Technical Reference Manual.
 mrc p15, #0, r12, cr1, cr0, #0;
 orr r12, r12, #ARMV6_CP15_C1_I_BIT 
 orr r12, r12, #ARMV6_CP15_C1_Z_BIT
 orr r12, r12, #ARMV6_CP15_C1_U_BIT
 mcr p15, #0, r12, cr1, cr0, #0;
 
 //Perform an Instruction Memory Barrier (IMB) operation immediately after the change above.
 //The ARM1176JZF-S Technical Reference Manual states on page 5-10 (section 5.5) that a Flush Prefetch Buffer operation also acts as an IMB.
 //Perform a Flush Prefetch Buffer operation.
 //See page 3-79 of the ARM1176JZF-S Technical Reference Manual.
 mov r12, #0
 mcr p15, #0, r12, cr7, cr5, #4
end;

{==============================================================================}

procedure ARMv6FPUInit; assembler; nostackframe;
asm
 //Enable access to Coprocessor 10 and 11 in the C1 coprocessor access control register.
 //See page 3-51 of the ARM1176JZF-S Technical Reference Manual.
 mrc p15, #0, r12, cr1, cr0, #2
 orr r12, r12, #ARMV6_CP15_C1_CP10_SYS
 orr r12, r12, #ARMV6_CP15_C1_CP11_SYS 
 mcr p15, #0, r12, cr1, cr0, #2
 
 //Perform an Instruction Memory Barrier (IMB) operation immediately after the change above.
 //The ARM1176JZF-S Technical Reference Manual states on page 5-10 (section 5.5) that a Flush Prefetch Buffer operation also acts as an IMB.
 //Perform a Flush Prefetch Buffer operation.
 //See page 3-79 of the ARM1176JZF-S Technical Reference Manual.
 mov r12, #0
 mcr p15, #0, r12, cr7, cr5, #4

 //Enable the VFP unit by setting the EN bit in the FPEXC system register.
 //See page ? of the ARM1176JZF-S Technical Reference Manual.
 fmrx r12, fpexc
 orr r12, r12, #ARMV6_FPEXC_EN
 fmxr fpexc, r12
 
 //Enable FPU exceptions, but disable INEXACT, UNDERFLOW, DENORMAL
 //This is already done by the FPC RTL but is done here early to
 //allow FPU operations during the initialization process.
 //See: \source\rtl\arm\arm.inc
 fmrx r12, fpscr
 //Set "round to nearest" mode
 and  r12, r12, #0xff3fffff
 //Mask "exception happened" and overflow flags
 and  r12, r12, #0xffffff20
 //Mask exception flags
 and  r12, r12, #0xffff40ff
 //Disable flush-to-zero mode (IEEE math compliant)
 and  r12, r12, #0xfeffffff
 //Enable invalid operation, div-by-zero and overflow exceptions
 orr  r12, r12, #0x00000700
 fmxr fpscr, r12
end;

{==============================================================================}

procedure ARMv6MMUInit;
begin
 {}
 {Check the Page Table}
 if PAGE_TABLE_SIZE < SIZE_16K then Exit; {Level 1 Page Table is 16KB}
 if Pointer(PAGE_TABLE_BASE) = nil then Exit;
 
 {Initialize the Page Table}
 ARMv6PageTableInit;

 {Start the MMU}
 ARMv6StartMMU;
end;

{==============================================================================}

procedure ARMv6CacheInit; assembler; nostackframe;
asm
 //Enable L1 Data Caching by setting the C bit in the C1 control register.
 //See page 3-47 of the ARM1176JZF-S Technical Reference Manual.
 mrc p15, #0, r12, cr1, cr0, #0;
 orr r12, r12, #ARMV6_CP15_C1_C_BIT
 mcr p15, #0, r12, cr1, cr0, #0;
 
 //Perform an Instruction Memory Barrier (IMB) operation immediately after the change above.
 //The ARM1176JZF-S Technical Reference Manual states on page 5-10 (section 5.5) that a Flush Prefetch Buffer operation also acts as an IMB.
 //Perform a Flush Prefetch Buffer operation.
 //See page 3-79 of the ARM1176JZF-S Technical Reference Manual.
 mov r12, #0
 mcr p15, #0, r12, cr7, cr5, #4
end;

{==============================================================================}

procedure ARMv6PageTableInit; 
{Initialize the Hardware Page Tables before enabling the MMU
 See page 6-36 of the ARM1176JZF-S Technical Reference Manual}
var
 Count:Integer;
 Address:PtrUInt;
begin
 {}
 if Assigned(ARMv6PageTableInitHandler) then
  begin
   {Call the supplied handler}
   ARMv6PageTableInitHandler;
  end
 else
  begin
   {Perform the default initialization}
   {If no handler is supplied then simply initialize 1GB (1024 1MB sections) as Normal, Cacheable, Executable with Read/Write access and direct mapping of Virtual to Physical}
   {And 3GB (3072 1MB sections) as Normal, Non Cacheable, Executable with Read/Write access and direct mapping of Virtual to Physical}
   {This will not normally be a useful setup but it will at least provide a Page Table that allows the memory management unit to be enabled}
   
   {Set the 1MB sections in the first 1GB as ARMV6_L1D_CACHE_NORMAL_WRITE_BACK (Non Shared)(Executable)(Read Write)}
   Address:=$00000000;
   for Count:=0 to 1023 do
    begin
     ARMv6SetPageTableSection(Address,Address,ARMV6_L1D_CACHE_NORMAL_WRITE_BACK or ARMV6_L1D_ACCESS_READWRITE);
     Inc(Address,SIZE_1M);
    end;

   {Set the 1MB sections in the remaining 3GB as ARMV6_L1D_CACHE_NORMAL_NONCACHED (Non Shared)(Executable)(Read Write)}
   for Count:=1024 to 4095 do
    begin
     ARMv6SetPageTableSection(Address,Address,ARMV6_L1D_CACHE_NORMAL_NONCACHED or ARMV6_L1D_ACCESS_READWRITE);
     Inc(Address,SIZE_1M);
    end;
    
   {Set the 1MB section containing the PAGE_TABLE_BASE to ARMV6_L1D_CACHE_NORMAL_WRITE_BACK (Non Shared)(Executable)(Read Write)}
   Address:=(PAGE_TABLE_BASE and ARMV6_L1D_SECTION_BASE_MASK);
   ARMv6SetPageTableSection(Address,Address,ARMV6_L1D_CACHE_NORMAL_WRITE_BACK or ARMV6_L1D_ACCESS_READWRITE); 
   
   {Set the 1MB sections containing the PERIPHERALS_BASE to ARMV6_L1D_CACHE_SHARED_DEVICE (Non Shared)(Executable)(Read Write)} 
   if PERIPHERALS_SIZE > 0 then
    begin
     Address:=(PERIPHERALS_BASE and ARMV6_L1D_SECTION_BASE_MASK);
     while Address < (PERIPHERALS_BASE + PERIPHERALS_SIZE) do
      begin
       ARMv6SetPageTableSection(Address,Address,ARMV6_L1D_CACHE_SHARED_DEVICE or ARMV6_L1D_ACCESS_READWRITE);
       Inc(Address,SIZE_1M);
      end;
    end;

   {Set the 1MB sections containing the LOCAL_PERIPHERALS_BASE to ARMV6_L1D_CACHE_SHARED_DEVICE (Non Shared)(Executable)(Read Write)} 
   if LOCAL_PERIPHERALS_SIZE > 0 then
    begin
     Address:=(LOCAL_PERIPHERALS_BASE and ARMV6_L1D_SECTION_BASE_MASK);
     while Address < (LOCAL_PERIPHERALS_BASE + LOCAL_PERIPHERALS_SIZE) do
      begin
       ARMv6SetPageTableSection(Address,Address,ARMV6_L1D_CACHE_SHARED_DEVICE or ARMV6_L1D_ACCESS_READWRITE);
       Inc(Address,SIZE_1M);
      end;
    end;  
  end;  
end;

{==============================================================================}

procedure ARMv6SystemCall(Number:LongWord;Param1,Param2,Param3:PtrUInt); assembler; nostackframe;
asm
 //Perform a Software Interrupt call
 //Number will be passed in R0
 //Param1 will be passed in R1
 //Param2 will be passed in R2
 //Param3 will be passed in R3
 swi #0
end;

{==============================================================================}

function ARMv6CPUGetMode:LongWord; assembler; nostackframe;
asm
 //Get Current program status register
 mrs r0, cpsr
 //Mask off everything except the MODE bits
 and r0, r0, #ARM_MODE_BITS
end;

{==============================================================================}

function ARMv6CPUGetState:LongWord; assembler; nostackframe;
asm
 //To Do 
 
end;

{==============================================================================}

function ARMv6CPUGetMainID:LongWord; assembler; nostackframe;
asm
 //Read the MainID register from the system control coprocessor
 mrc p15, #0, r0, cr0, cr0, #0
end;

{==============================================================================}

function ARMv6CPUGetModel:LongWord;
var
 MainID:LongWord;
begin 
 {}
 Result:=CPU_MODEL_UNKNOWN;
 
 {Get MainID}
 MainID:=ARMv6CPUGetMainID;
 if MainID <> 0 then
  begin 
   {Check Primary Part Number}
   if (MainID and ARMV6_CP15_C0_MAINID_PARTNUMBER_MASK) = ARMV6_CP15_C0_MAINID_PARTNUMBER_1176JZSF then
    begin
     Result:=CPU_MODEL_ARM1176JZFS;
    end;
  end;
end;

{==============================================================================}

function ARMv6CPUGetRevision:LongWord;
var
 MainID:LongWord;
begin 
 {}
 Result:=0;
 
 {Get MainID}
 MainID:=ARMv6CPUGetMainID;
 if MainID <> 0 then
  begin 
   {Get Variant}
   Result:=(MainID and ARMV6_CP15_C0_MAINID_VARIANT_MASK) shr 16;
   
   {Get Revision}
   Result:=Result or (MainID and ARMV6_CP15_C0_MAINID_REVISION_MASK);
  end;
end;

{==============================================================================}

function ARMv6CPUGetDescription:String;
var
 MainID:LongWord;
begin 
 {}
 Result:='';
 
 {Get MainID}
 MainID:=ARMv6CPUGetMainID;
 if MainID <> 0 then
  begin 
   {Check Primary Part Number}
   if (MainID and ARMV6_CP15_C0_MAINID_PARTNUMBER_MASK) = ARMV6_CP15_C0_MAINID_PARTNUMBER_1176JZSF then
    begin
     Result:=CPU_DESCRIPTION_ARM1176JZFS;
    end;
  end;
end;

{==============================================================================}

function ARMv6FPUGetState:LongWord; assembler; nostackframe;
asm
 //To Do 
 
end;

{==============================================================================}

function ARMv6L1CacheGetType:LongWord; assembler; nostackframe;
asm
 //Default to separate caches
 mov r0, #CACHE_TYPE_SEPARATE
 
 //Read the CacheType register from the system control coprocessor
 mrc p15, #0, r1, cr0, cr0, #1

 //Check the S bit
 and r1, r1, #ARMV6_CP15_C0_CTR_S
 cmp r1, #ARMV6_CP15_C0_CTR_S
 bxeq lr
 
 //Unified cache
 mov r0, #CACHE_TYPE_UNIFIED
end;

{==============================================================================}

function ARMv6L1DataCacheGetSize:LongWord; assembler; nostackframe;
asm
 //Read the CacheType register from the system control coprocessor
 mrc p15, #0, r1, cr0, cr0, #1

 //Mask off the DSize bits
 and r1, r1, #ARMV6_CP15_C0_CTR_DSIZE_MASK
 
 //Check for 64K
 mov r0, #65536
 cmp r1, #ARMV6_CP15_C0_CTR_DSIZE_64K
 bxeq lr
 
 //Check for 32K
 mov r0, #32768
 cmp r1, #ARMV6_CP15_C0_CTR_DSIZE_32K
 bxeq lr

 //Check for 16K
 mov r0, #16384
 cmp r1, #ARMV6_CP15_C0_CTR_DSIZE_16K
 bxeq lr
 
 //Check for 8K
 mov r0, #8192
 cmp r1, #ARMV6_CP15_C0_CTR_DSIZE_8K
 bxeq lr

 //Check for 4K
 mov r0, #4096
 cmp r1, #ARMV6_CP15_C0_CTR_DSIZE_4K
 bxeq lr
 
 //Invalid size
 mov r0, #0
end;

{==============================================================================}

function ARMv6L1DataCacheGetLineSize:LongWord; assembler; nostackframe;
asm
 //Read the CacheType register from the system control coprocessor
 mrc p15, #0, r1, cr0, cr0, #1

 //Mask off the DLen bits
 and r1, r1, #ARMV6_CP15_C0_CTR_DLEN_MASK
 
 //Check for 32
 mov r0, #32
 cmp r1, #ARMV6_CP15_C0_CTR_DLEN_32
 bxeq lr
 
 //Invalid size
 mov r0, #0
end;

{==============================================================================}

function ARMv6L1InstructionCacheGetSize:LongWord; assembler; nostackframe;
asm
 //Read the CacheType register from the system control coprocessor
 mrc p15, #0, r1, cr0, cr0, #1

 //Mask off the ISize bits
 and r1, r1, #ARMV6_CP15_C0_CTR_ISIZE_MASK
 
 //Check for 64K
 mov r0, #65536
 cmp r1, #ARMV6_CP15_C0_CTR_ISIZE_64K
 bxeq lr
 
 //Check for 32K
 mov r0, #32768
 cmp r1, #ARMV6_CP15_C0_CTR_ISIZE_32K
 bxeq lr

 //Check for 16K
 mov r0, #16384
 cmp r1, #ARMV6_CP15_C0_CTR_ISIZE_16K
 bxeq lr
 
 //Check for 8K
 mov r0, #8192
 cmp r1, #ARMV6_CP15_C0_CTR_ISIZE_8K
 bxeq lr

 //Check for 4K
 mov r0, #4096
 cmp r1, #ARMV6_CP15_C0_CTR_ISIZE_4K
 bxeq lr
 
 //Invalid size
 mov r0, #0
end;

{==============================================================================}

function ARMv6L1InstructionCacheGetLineSize:LongWord; assembler; nostackframe;
asm
 //Read the CacheType register from the system control coprocessor
 mrc p15, #0, r1, cr0, cr0, #1

 //Mask off the ILen bits
 and r1, r1, #ARMV6_CP15_C0_CTR_ILEN_MASK
 
 //Check for 32
 mov r0, #32
 cmp r1, #ARMV6_CP15_C0_CTR_ILEN_32
 bxeq lr
 
 //Invalid size
 mov r0, #0
end;

{==============================================================================}

procedure ARMv6Halt; assembler; nostackframe; public name '_haltproc';
{The purpose of the Wait For Interrupt operation is to put the processor in to a low power state,
 see Standby mode on page 10-3 of the ARM1176JZF-S Revision: r0p7 Technical Reference Manual}
asm
 //Perform a data synchronization barrier operation using the c7 (Cache Operations) register of system control coprocessor CP15
 //See page 3-74 of the ARM1176JZF-S Technical Reference Manual
 mov r12, #0
 mcr p15, #0, r12, cr7, cr10, #4
 
.LLoop:
 //Disable IRQ and FIQ
 msr cpsr_c, #ARM_I_BIT | ARM_F_BIT

 //ARMv6 "wait for interrupt" instruction.
 mov r12, #0
 mcr p15, #0, r12, cr7, cr0, #4 
 b .LLoop
end;

{==============================================================================}

procedure ARMv6Pause; assembler; nostackframe;
{The purpose of the Wait For Interrupt operation is to put the processor in to a low power state,
 see Standby mode on page 10-3 of the ARM1176JZF-S Revision: r0p7 Technical Reference Manual}
asm
 //Perform a data synchronization barrier operation using the c7 (Cache Operations) register of system control coprocessor CP15
 //See page 3-74 of the ARM1176JZF-S Technical Reference Manual
 mov r12, #0
 mcr p15, #0, r12, cr7, cr10, #4

 //ARMv6 "wait for interrupt" instruction.
 mov r12, #0
 mcr p15, #0, r12, cr7, cr0, #4 
end;

{==============================================================================}

procedure ARMv6WaitForEvent; assembler; nostackframe; 
{Wait For Event not available in ARMv6, do a Wait For Interrupt instead}
asm
 //Perform a data synchronization barrier operation using the c7 (Cache Operations) register of system control coprocessor CP15
 //See page 3-74 of the ARM1176JZF-S Technical Reference Manual
 mov r12, #0
 mcr p15, #0, r12, cr7, cr10, #4

 //ARMv6 "wait for interrupt" instruction.
 mov r12, #0
 mcr p15, #0, r12, cr7, cr0, #4 
end;
 
{==============================================================================}

procedure ARMv6WaitForInterrupt; assembler; nostackframe; 
{The purpose of the Wait For Interrupt operation is to put the processor in to a low power state,
 see Standby mode on page 10-3 of the ARM1176JZF-S Revision: r0p7 Technical Reference Manual}
asm
 //Perform a data synchronization barrier operation using the c7 (Cache Operations) register of system control coprocessor CP15
 //See page 3-74 of the ARM1176JZF-S Technical Reference Manual
 mov r12, #0
 mcr p15, #0, r12, cr7, cr10, #4

 //ARMv6 "wait for interrupt" instruction.
 mov r12, #0
 mcr p15, #0, r12, cr7, cr0, #4 
end;

{==============================================================================}

procedure ARMv6DataMemoryBarrier; assembler; nostackframe; 
{Perform a data memory barrier operation using the c7 (Cache Operations) register of system control coprocessor CP15
 See page 3-74 of the ARM1176JZF-S Technical Reference Manual}
 
{Note that this is also available in the FPC RTL as ReadBarrier/WriteBarrier

 See: \source\rtl\arm\arm.inc
 
 Implementation is exactly the same for either.
} 
asm
 //ARMv6 "data memory barrier" instruction.
 mov r12, #0
 mcr p15, #0, r12, cr7, cr10, #5
end;

{==============================================================================}

procedure ARMv6DataSynchronizationBarrier; assembler; nostackframe; 
{Perform a data synchronization barrier operation using the c7 (Cache Operations) register of system control coprocessor CP15
 See page 3-74 of the ARM1176JZF-S Technical Reference Manual}
asm
 //ARMv6 "data synchronization barrier" instruction.
 mov r12, #0
 mcr p15, #0, r12, cr7, cr10, #4
end;

{==============================================================================}

procedure ARMv6InstructionMemoryBarrier; assembler; nostackframe; 
{The ARM1176JZF-S Technical Reference Manual states on page 5-10 (section 5.5) that a Flush Prefetch Buffer operation also acts as an IMB}
{Perform a Flush Prefetch Buffer operation. See page 3-79 of the ARM1176JZF-S Technical Reference Manual}
asm
 //ARMv6 "flush prefetch buffer" instruction.
 mov r12, #0
 mcr p15, #0, r12, cr7, cr5, #4
end;

{==============================================================================}

procedure ARMv6InvalidateTLB; assembler; nostackframe; 
{Perform an invalidate entire TLB (Unlocked/Unified) operation using the c8 (TLB Operations) register of system control coprocessor CP15
 See page 3-86 of the ARM1176JZF-S Technical Reference Manual}
asm
 mov r12, #0
 mcr p15, #0, r12, cr8, cr7, #0
 
 //Perform a data synchronization barrier operation using the c7 (Cache Operations) register of system control coprocessor CP15
 //See page 3-74 of the ARM1176JZF-S Technical Reference Manual
 mov r12, #0
 mcr p15, #0, r12, cr7, cr10, #4
end;

{==============================================================================}

procedure ARMv6InvalidateDataTLB; assembler; nostackframe; 
{Perform an invalidate data TLB (Unlocked/Data) operation using the c8 (TLB Operations) register of system control coprocessor CP15
 See page 3-86 of the ARM1176JZF-S Technical Reference Manual}
asm
 mov r12, #0
 mcr p15, #0, r12, cr8, cr6, #0
 
 //Perform a data synchronization barrier operation using the c7 (Cache Operations) register of system control coprocessor CP15
 //See page 3-74 of the ARM1176JZF-S Technical Reference Manual
 mov r12, #0
 mcr p15, #0, r12, cr7, cr10, #4
end;

{==============================================================================}

procedure ARMv6InvalidateInstructionTLB; assembler; nostackframe; 
{Perform an invalidate instruction TLB (Unlocked/Instruction) operation using the c8 (TLB Operations) register of system control coprocessor CP15
 See page 3-86 of the ARM1176JZF-S Technical Reference Manual}
asm
 mov r12, #0
 mcr p15, #0, r12, cr8, cr5, #0
 
 //Perform a data synchronization barrier operation using the c7 (Cache Operations) register of system control coprocessor CP15
 //See page 3-74 of the ARM1176JZF-S Technical Reference Manual
 mov r12, #0
 mcr p15, #0, r12, cr7, cr10, #4
end;

{==============================================================================}

procedure ARMv6InvalidateCache; assembler; nostackframe; 
{Perform an invalidate both caches operation using the c7 (Cache Operations) register of system control coprocessor CP15
 See page 3-74 of the ARM1176JZF-S Technical Reference Manual}
asm
 mov r12, #0
 mcr p15, #0, r12, cr7, cr7, #0
 
 //Perform a data synchronization barrier operation using the c7 (Cache Operations) register of system control coprocessor CP15
 //See page 3-74 of the ARM1176JZF-S Technical Reference Manual
 mov r12, #0
 mcr p15, #0, r12, cr7, cr10, #4
end;

{==============================================================================}

procedure ARMv6CleanDataCache; assembler; nostackframe; 
{Perform a clean entire data cache operation using the c7 (Cache Operations) register of system control coprocessor CP15
 See page 3-74 of the ARM1176JZF-S Technical Reference Manual}
asm
 mov r12, #0
 mcr p15, #0, r12, cr7, cr10, #0
 
 //Perform a data synchronization barrier operation using the c7 (Cache Operations) register of system control coprocessor CP15
 //See page 3-74 of the ARM1176JZF-S Technical Reference Manual
 mov r12, #0
 mcr p15, #0, r12, cr7, cr10, #4
end;

{==============================================================================}

procedure ARMv6InvalidateDataCache; assembler; nostackframe; 
{Perform an invalidate entire data cache operation using the c7 (Cache Operations) register of system control coprocessor CP15
 See page 3-74 of the ARM1176JZF-S Technical Reference Manual}
asm
 mov r12, #0
 mcr p15, #0, r12, cr7, cr6, #0
 
 //Perform a data synchronization barrier operation using the c7 (Cache Operations) register of system control coprocessor CP15
 //See page 3-74 of the ARM1176JZF-S Technical Reference Manual
 mov r12, #0
 mcr p15, #0, r12, cr7, cr10, #4
end;

{==============================================================================}

procedure ARMv6CleanAndInvalidateDataCache; assembler; nostackframe; 
{Perform a clean and invalidate entire data cache operation using the c7 (Cache Operations) register of system control coprocessor CP15
 See page 3-74 of the ARM1176JZF-S Technical Reference Manual}
asm
 mov r12, #0
 mcr p15, #0, r12, cr7, cr14, #0
 
 //Perform a data synchronization barrier operation using the c7 (Cache Operations) register of system control coprocessor CP15
 //See page 3-74 of the ARM1176JZF-S Technical Reference Manual
 mov r12, #0
 mcr p15, #0, r12, cr7, cr10, #4
end;

{==============================================================================}

procedure ARMv6InvalidateInstructionCache; assembler; nostackframe; 
{Perform an invalidate entire instruction cache operation using the c7 (Cache Operations) register of system control coprocessor CP15
 See page 3-74 of the ARM1176JZF-S Technical Reference Manual}
asm
 mov r12, #0
 mcr p15, #0, r12, cr7, cr5, #0
 
 //Perform a data synchronization barrier operation using the c7 (Cache Operations) register of system control coprocessor CP15
 //See page 3-74 of the ARM1176JZF-S Technical Reference Manual
 mov r12, #0
 mcr p15, #0, r12, cr7, cr10, #4
end;

{==============================================================================}

procedure ARMv6CleanDataCacheRangeInternal(Address:PtrUInt;Size:LongWord); assembler; nostackframe; 
{Perform a clean data cache range operation
 See page 3-71 / 3-76 of the ARM1176JZF-S Technical Reference Manual}
asm
 add  r1, r1, r0                  //Start Address in r0
 sub  r1, r1, #1                  //End Address in r1
 mcrr p15, #0, r1, r0, cr12       //Clean D-Cache range
 
 //Perform a data synchronization barrier operation using the c7 (Cache Operations) register of system control coprocessor CP15
 //See page 3-74 of the ARM1176JZF-S Technical Reference Manual
 mov r12, #0
 mcr p15, #0, r12, cr7, cr10, #4
end;

{==============================================================================}

procedure ARMv6CleanDataCacheRange(Address:PtrUInt;Size:LongWord);
{Perform a clean data cache range, limiting the size for each operation to
 4MB because some processors fail to correctly operate with larger ranges}
const
 BLOCK_SIZE = $00400000; {4MB block size}
var
 Offset:LongWord;
 Remain:LongWord;
begin
 {}
 Offset:=0;
 Remain:=Size;
 while Remain > BLOCK_SIZE do
  begin
   {Clean block}
   ARMv6CleanDataCacheRangeInternal(Address + Offset,BLOCK_SIZE);
   
   Inc(Offset,BLOCK_SIZE);
   Dec(Remain,BLOCK_SIZE);
  end;
 if Remain = 0 then Exit;
 
 {Clean last block}
 ARMv6CleanDataCacheRangeInternal(Address + Offset,Remain);
end;

{==============================================================================}

procedure ARMv6InvalidateDataCacheRangeInternal(Address:PtrUInt;Size:LongWord); assembler; nostackframe; 
{Perform an invalidate data cache range operation
 See page 3-71 / 3-76 of the ARM1176JZF-S Technical Reference Manual}
asm
 add  r1, r1, r0                  //Start Address in r0
 sub  r1, r1, #1                  //End Address in r1
 mcrr p15, #0, r1, r0, cr6        //Invaliate D-Cache range
    
 //Perform a data synchronization barrier operation using the c7 (Cache Operations) register of system control coprocessor CP15
 //See page 3-74 of the ARM1176JZF-S Technical Reference Manual
 mov r12, #0
 mcr p15, #0, r12, cr7, cr10, #4
end;

{==============================================================================}

procedure ARMv6InvalidateDataCacheRange(Address:PtrUInt;Size:LongWord);
{Perform an invalidate data cache range, limiting the size for each operation
 to 4MB because some processors fail to correctly operate with larger ranges}
const
 BLOCK_SIZE = $00400000; {4MB block size}
var
 Offset:LongWord;
 Remain:LongWord;
begin
 {}
 Offset:=0;
 Remain:=Size;
 while Remain > BLOCK_SIZE do
  begin
   {Clean block}
   ARMv6InvalidateDataCacheRangeInternal(Address + Offset,BLOCK_SIZE);
   
   Inc(Offset,BLOCK_SIZE);
   Dec(Remain,BLOCK_SIZE);
  end;
 if Remain = 0 then Exit;
 
 {Clean last block}
 ARMv6InvalidateDataCacheRangeInternal(Address + Offset,Remain);
end;

{==============================================================================}

procedure ARMv6CleanAndInvalidateDataCacheRangeInternal(Address:PtrUInt;Size:LongWord); assembler; nostackframe; 
{Perform a clean and invalidate data cache range operation
 See page 3-71 / 3-76 of the ARM1176JZF-S Technical Reference Manual}
asm
 add  r1, r1, r0                  //Start Address in r0
 sub  r1, r1, #1                  //End Address in r1
 mcrr p15, #0, r1, r0, cr14       //Clean and Invaliate D-Cache range
 
 //Perform a data synchronization barrier operation using the c7 (Cache Operations) register of system control coprocessor CP15
 //See page 3-74 of the ARM1176JZF-S Technical Reference Manual
 mov r12, #0
 mcr p15, #0, r12, cr7, cr10, #4
end;

{==============================================================================}

procedure ARMv6CleanAndInvalidateDataCacheRange(Address:PtrUInt;Size:LongWord);
{Perform a clean and invalidate data cache range, limiting the size for each operation
 to 4MB because some processors fail to correctly operate with larger ranges}
const
 BLOCK_SIZE = $00400000; {4MB block size}
var
 Offset:LongWord;
 Remain:LongWord;
begin
 {}
 Offset:=0;
 Remain:=Size;
 while Remain > BLOCK_SIZE do
  begin
   {Clean block}
   ARMv6CleanAndInvalidateDataCacheRangeInternal(Address + Offset,BLOCK_SIZE);
   
   Inc(Offset,BLOCK_SIZE);
   Dec(Remain,BLOCK_SIZE);
  end;
 if Remain = 0 then Exit;
 
 {Clean last block}
 ARMv6CleanAndInvalidateDataCacheRangeInternal(Address + Offset,Remain);
end;

{==============================================================================}

procedure ARMv6InvalidateInstructionCacheRangeInternal(Address:PtrUInt;Size:LongWord); assembler; nostackframe;  
{Perform an invalidate instruction cache range operation
 See page 3-71 / 3-76 of the ARM1176JZF-S Technical Reference Manual}
asm
 add  r1, r1, r0                  //Start Address in r0
 sub  r1, r1, #1                  //End Address in r1
 mcrr p15, #0, r1, r0, cr5        //Invalidate I-Cache range
    
 //Perform a data synchronization barrier operation using the c7 (Cache Operations) register of system control coprocessor CP15
 //See page 3-74 of the ARM1176JZF-S Technical Reference Manual
 mov r12, #0
 mcr p15, #0, r12, cr7, cr10, #4
end;

{==============================================================================}

procedure ARMv6InvalidateInstructionCacheRange(Address:PtrUInt;Size:LongWord);
{Perform an invalidate instruction cache range operation, limiting the size for each
 operation to 4MB because some processors fail to correctly operate with larger ranges}
const
 BLOCK_SIZE = $00400000; {4MB block size}
var
 Offset:LongWord;
 Remain:LongWord;
begin
 {}
 Offset:=0;
 Remain:=Size;
 while Remain > BLOCK_SIZE do
  begin
   {Clean block}
   ARMv6InvalidateInstructionCacheRangeInternal(Address + Offset,BLOCK_SIZE);
   
   Inc(Offset,BLOCK_SIZE);
   Dec(Remain,BLOCK_SIZE);
  end;
 if Remain = 0 then Exit;
 
 {Clean last block}
 ARMv6InvalidateInstructionCacheRangeInternal(Address + Offset,Remain);
end;

{==============================================================================}

procedure ARMv6FlushPrefetchBuffer; assembler; nostackframe; 
{Perform a Flush Prefetch Buffer operation. See page 3-79 of the ARM1176JZF-S Technical Reference Manual}
asm
 mov r12, #0
 mcr p15, #0, r12, cr7, cr5, #4
end;

{==============================================================================}

procedure ARMv6FlushBranchTargetCache; assembler; nostackframe;
{Perform a Flush Entire Branch Target Cache. See page 3-79 of the ARM1176JZF-S Technical Reference Manual}
asm
 mov r12, #0
 mcr p15, #0, r12, cr7, cr5, #6
 
 //Perform a data synchronization barrier operation using the c7 (Cache Operations) register of system control coprocessor CP15
 //See page 3-74 of the ARM1176JZF-S Technical Reference Manual
 mov r12, #0
 mcr p15, #0, r12, cr7, cr10, #4
end;

{==============================================================================}

procedure ARMv6ContextSwitch(OldStack,NewStack:Pointer;NewThread:TThreadHandle); assembler; nostackframe; 
{Perform a context switch from one thread to another as a result of a thread yielding, sleeping or waiting}
{OldStack: The address to save the stack pointer to for the current thread (Passed in r0)}
{NewStack: The address to restore the stack pointer from for the new thread (Passed in r1)}
{NewThread: The handle of the new thread to switch to (Passed in r2)}
{Notes: At the point of the actual context switch (str sp / ldr sp) the thread stacks will look like this:
        (See: ARMv6ThreadSetupStack for additional information)

        (Base "Highest Address" of Stack)
        .
        .
        .
        .
        cpsr  <- The current program status register value to load on return from the context switch
        lr/pc <- The address to return to from the context switch
        lr    <- The lr value prior to the context switch
        r12   <- 
        r11   <- 
        r10   <- 
        r9    <- 
        r8    <- 
        r7    <- 
        r6    <- The value of these registers prior to the context switch 
        r5    <-  
        r4    <-  
        r3    <-  
        r2    <- 
        r1    <- 
        r0    <-
        
        d15   <- 
        d14   <- 
        d13   <- 
        d12   <- 
        d11   <- 
        d10   <- 
        d9    <- 
        d8    <- The value of these floating point registers prior to the context switch
        d7    <-
        d6    <- 
        d5    <- 
        d4    <- 
        d3    <- 
        d2    <- 
        d1    <- 
        d0    <- 
        
        fpscr <- The floating point FPSCR register
        fpexc <- The floating point FPEXC register (Current StackPointer points to here)
        .
        .
        .
        .
        (Top "Lowest Address" of Stack)
             
        This form of context switch uses r12 to save the cpsr value (and RFE to restore it). Because this context switch is called from a
        routine which will have saved the value of r12 (which is caller save in the ARM ABI) then we do not need to save the original value
        of r12
        
        The context switch will be performed from SYS mode to SYS mode, the cpsr value will include the control bits (Mode and IRQ/FIQ state)
        but not the flags values. Again the ARM ABI does not require that the flags be saved by the callee and so the caller would have 
        accounted for any needed flags before calling. If the thread to be resumed was interrupted by an IRQ or FIQ then the cpsr will also
        contain the flags etc as they were at the point of interrupt. We do not need to account for the state bits in the cpsr since all
        operations are performed in ARM mode at present
        
        The main requirement of this routine is to ensure that the context record on the stack matches exactly that which is created on an
        interrupt and also that created by ThreadSetupStack for a new thread. If this is correct then the next context switch for any given 
        thread can be either by a call to reschedule or by an interrupt. Equally a new thread can be first run from a context switch that
        resulted from either a call to reschedule or an interrupt
        
        Note that this routine could use:
        
         pop (lr)
         pop (r12)
         msr cpsr_c, r12
         mov pc, lr
         
        to return but that would mess up the value of r12, lr and the cpsr flags etc if the thread being resumed was interrupted by an IRQ,
        FIQ or SWI. The use of RFE here allows for exactly the same behaviour no matter which way the context record is saved and restored
         
}
asm
 //Save the Current Program Status Register (CPSR)
 mrs r12, cpsr
 
 //Save register R12 (CPSR) onto the old stack
 push {r12}
 
 //Save the Link Register (LR) onto the old stack
 push {lr}
 
 //Save registers R0 to R12 and the Link Register (LR) onto the old stack
 push {r0-r12, lr}
 
 //Save the Floating point registers D0 to D15 onto the old stack
 fstmdbd sp!, {d0-d15}
 
 //Save the Floating point FPSCR onto the old stack
 fmrx r12, fpscr
 push {r12}

 //Save the Floating point FPEXC onto the old stack
 fmrx r12, fpexc
 push {r12}
  
 //Check if old and new stacks are the same address
 //If they are then simply skip the context switch
 cmp r0, r1
 beq .LSkip 
 
 //Switch thread stacks 
 str sp, [r0]
 ldr sp, [r1]

 //Load the new thread id into c13 (Thread and process ID) register of system control coprocessor CP15 
 mcr p15, #0, r2, cr13, cr0, #4
 
 //Perform a dummy STREX to clear the Local Monitor status to Open
 mov   r0, #0
 ldr   r1, .LARMv6DummySTREX
 strex r2, r0, [r1]

.LSkip:
 //Restore the Floating point FPEXC from the new stack
 pop {r12}
 fmxr fpexc, r12
 
 //Restore the Floating point FPSCR from the new stack
 pop {r12}
 fmxr fpscr, r12
  
 //Restore the Floating point registers D0 to D15 from the new stack
 fldmiad sp!, {d0-d15}

 //Restore registers R0 to R12 and the Link Register (LR) from the new stack
 pop {r0-r12, lr}
 
 //Return From Exception (RFEIA) loading PC and CPSR from the new stack
 //Which is somewhat equivalent to doing "pop {pc, cpsr}" if that was a real instruction 
 //See: A2.6.14 RFE – Return From Exception in the ARM Architecture Reference Manual (arm_arm)
 //Note: This instruction is not yet supported by the FPC compiler
 .long 0xf8bd0a00  //rfeia sp!   
 
.LARMv6DummySTREX:
 .long ARMv6DummySTREX
 
 //Note: Compiler adds "mov pc, lr" or "bx lr" to the end of this. Should not be an issue due to pop pc above
end;

{==============================================================================}

procedure ARMv6ContextSwitchIRQ(OldStack,NewStack:Pointer;NewThread:TThreadHandle); assembler; nostackframe;  
{Perform a context switch from one thread to another as a result of an interrupt request (IRQ)}
{OldStack: The address to save the stack pointer to for the current thread (Passed in r0)}
{NewStack: The address to restore the stack pointer from for the new thread (Passed in r1)}
{NewThread: The handle of the new thread to switch to (Passed in r2)}
{Notes: At the point of the actual context switch (str sp / ldr sp) the thread stacks will look like this:
        (See: ARMv6ThreadSetupStack for additional information)

        (Base "Highest Address" of Stack)
        .
        .
        .
        .
        cpsr  <- The current program status register value to load on return from the context switch
        lr/pc <- The address to return to from the context switch
        lr    <- The lr value prior to the context switch
        r12   <- 
        r11   <- 
        r10   <- 
        r9    <- 
        r8    <- 
        r7    <- 
        r6    <- The value of these registers prior to the context switch 
        r5    <-  
        r4    <-  
        r3    <-  
        r2    <- 
        r1    <- 
        r0    <-
        
        d15   <- 
        d14   <- 
        d13   <- 
        d12   <- 
        d11   <- 
        d10   <- 
        d9    <- 
        d8    <- The value of these floating point registers prior to the context switch
        d7    <- 
        d6    <- 
        d5    <-  
        d4    <- 
        d3    <- 
        d2    <- 
        d1    <- 
        d0    <- 
        
        fpscr <- The floating point FPSCR register
        fpexc <- The floating point FPEXC register (Current StackPointer points to here)
        .
        .
        .
        .
        (Top "Lowest Address" of Stack)
             
        This form of context switch relies on the IRQ handler to save the necessary registers including the lr, cpsr and other general
        registers from the point at which the thread was interrupted. The thread to be resumed may have been saved by a previous IRQ or
        by a call to the standard context switch from SchedulerReschule or it may be a new thread to be run for the first time. All of
        these result in the same context record on the stack and therefore can be resumed the same way
        
        The context switch will be performed by switching to SYS mode, exchanging the stack pointers and then returning to IRQ mode
}
asm
 //Switch to SYS mode and stack for the context switch
 cps #ARM_MODE_SYS
 
 //Save the Floating point registers D0 to D15 onto the old stack
 fstmdbd sp!, {d0-d15}
 
 //Save the Floating point FPSCR onto the old stack
 fmrx r12, fpscr
 push {r12}

 //Save the Floating point FPEXC onto the old stack
 fmrx r12, fpexc
 push {r12}
 
 //Check if old and new stacks are the same address
 //If they are then simply skip the context switch
 cmp r0, r1
 beq .LSkip 
 
 //Switch thread stacks 
 str sp, [r0]
 ldr sp, [r1]

 //Perform a dummy STREX to clear the Local Monitor status to Open
 mov   r0, #0
 ldr   r1, .LARMv6DummySTREX
 strex r2, r0, [r1]
 
.LSkip:
 //Restore the Floating point FPEXC from the new stack
 pop {r12}
 fmxr fpexc, r12
 
 //Restore the Floating point FPSCR from the new stack
 pop {r12}
 fmxr fpscr, r12
  
 //Restore the Floating point registers D0 to D15 from the new stack
 fldmiad sp!, {d0-d15}

 //Return to IRQ mode and stack after context switch
 cps #ARM_MODE_IRQ
 bx  lr 
 
.LARMv6DummySTREX:
 .long ARMv6DummySTREX

 //Note: Compiler adds "mov pc, lr" or "bx lr" to the end of this. Should not be an issue due to bx lr above
end;

{==============================================================================}

procedure ARMv6ContextSwitchFIQ(OldStack,NewStack:Pointer;NewThread:TThreadHandle); assembler; nostackframe;  
{Perform a context switch from one thread to another as a result of a fast interrupt request (FIQ)}
{OldStack: The address to save the stack pointer to for the current thread (Passed in r0)}
{NewStack: The address to restore the stack pointer from for the new thread (Passed in r1)}
{NewThread: The handle of the new thread to switch to (Passed in r2)}
{Notes: At the point of the actual context switch (str sp / ldr sp) the thread stacks will look like this:
        (See: ARMv6ThreadSetupStack for additional information)

        (Base "Highest Address" of Stack)
        .
        .
        .
        .
        cpsr  <- The current program status register value to load on return from the context switch
        lr/pc <- The address to return to from the context switch
        lr    <- The lr value prior to the context switch
        r12   <- 
        r11   <- 
        r10   <- 
        r9    <- 
        r8    <- 
        r7    <- 
        r6    <- The value of these registers prior to the context switch 
        r5    <-  
        r4    <-  
        r3    <-  
        r2    <- 
        r1    <- 
        r0    <- 
        
        d15   <- 
        d14   <- 
        d13   <- 
        d12   <- 
        d11   <- 
        d10   <- 
        d9    <- 
        d8    <- The value of these floating point registers prior to the context switch
        d7    <- 
        d6    <- 
        d5    <- 
        d4    <- 
        d3    <- 
        d2    <- 
        d1    <- 
        d0    <- 
        
        fpscr <- The floating point FPSCR register
        fpexc <- The floating point FPEXC register (Current StackPointer points to here)
        .
        .
        .
        .
        (Top "Lowest Address" of Stack)
             
        This form of context switch relies on the FIQ handler to save the necessary registers including the lr, cpsr and other general
        registers from the point at which the thread was interrupted. The thread to be resumed may have been saved by a previous FIQ or
        by a call to the standard context switch from SchedulerReschule or it may be a new thread to be run for the first time. All of
        these result in the same context record on the stack and therefore can be resumed the same way
        
        The context switch will be performed by switching to SYS mode, exchanging the stack pointers and then returning to FIQ mode
}
asm
 //Switch to SYS mode and stack for the context switch
 cps #ARM_MODE_SYS
 
 //Save the Floating point registers D0 to D15 onto the old stack
 fstmdbd sp!, {d0-d15}
 
 //Save the Floating point FPSCR onto the old stack
 fmrx r12, fpscr
 push {r12}

 //Save the Floating point FPEXC onto the old stack
 fmrx r12, fpexc
 push {r12}
 
 //Check if old and new stacks are the same address
 //If they are then simply skip the context switch
 cmp r0, r1
 beq .LSkip 
 
 //Switch thread stacks 
 str sp, [r0]
 ldr sp, [r1]

 //Perform a dummy STREX to clear the Local Monitor status to Open
 mov   r0, #0
 ldr   r1, .LARMv6DummySTREX
 strex r2, r0, [r1]
 
.LSkip:
 //Restore the Floating point FPEXC from the new stack
 pop {r12}
 fmxr fpexc, r12
 
 //Restore the Floating point FPSCR from the new stack
 pop {r12}
 fmxr fpscr, r12
  
 //Restore the Floating point registers D0 to D15 from the new stack
 fldmiad sp!, {d0-d15}

 //Return to FIQ mode and stack after context switch
 cps #ARM_MODE_FIQ
 bx  lr 
 
.LARMv6DummySTREX:
 .long ARMv6DummySTREX
 
 //Note: Compiler adds "mov pc, lr" or "bx lr" to the end of this. Should not be an issue due to bx lr above
end;

{==============================================================================}

procedure ARMv6ContextSwitchSWI(OldStack,NewStack:Pointer;NewThread:TThreadHandle); 
{Perform a context switch from one thread to another as a result of a software interrupt (SWI)}
{OldStack: The address to save the stack pointer to for the current thread (Passed in r0)}
{NewStack: The address to restore the stack pointer from for the new thread (Passed in r1)}
{NewThread: The handle of the new thread to switch to (Passed in r2)}
{Notes: At the point of the actual context switch (str sp / ldr sp) the thread stacks will look like this:
        (See: ARMv6ThreadSetupStack for additional information)

        (Base "Highest Address" of Stack)
        .
        .
        .
        .
        cpsr  <- The current program status register value to load on return from the context switch
        lr/pc <- The address to return to from the context switch
        lr    <- The lr value prior to the context switch
        r12   <- 
        r11   <- 
        r10   <- 
        r9    <- 
        r8    <- 
        r7    <- 
        r6    <- The value of these registers prior to the context switch 
        r5    <-  
        r4    <-  
        r3    <-  
        r2    <- 
        r1    <- 
        r0    <- 
        
        d15   <- 
        d14   <- 
        d13   <- 
        d12   <- 
        d11   <- 
        d10   <- 
        d9    <- 
        d8    <- The value of these floating point registers prior to the context switch
        d7    <-
        d6    <- 
        d5    <- 
        d4    <- 
        d3    <- 
        d2    <- 
        d1    <- 
        d0    <- 
        
        fpscr <- The floating point FPSCR register
        fpexc <- The floating point FPEXC register (Current StackPointer points to here)
        .
        .
        .
        .
        (Top "Lowest Address" of Stack)
             
        This form of context switch relies on the SWI handler to save the necessary registers including the lr, cpsr and other general
        registers from the point at which the thread was interrupted. The thread to be resumed may have been saved by a previous SWI or
        by a call to the standard context switch from SchedulerReschule or it may be a new thread to be run for the first time. All of
        these result in the same context record on the stack and therefore can be resumed the same way
        
        The context switch will be performed by switching to SYS mode, exchanging the stack pointers and then returning to SWI (SVC) mode
}
asm
 //Switch to SYS mode and stack for the context switch
 cps #ARM_MODE_SYS

 //Save the Floating point registers D0 to D15 onto the old stack
 fstmdbd sp!, {d0-d15}
 
 //Save the Floating point FPSCR onto the old stack
 fmrx r12, fpscr
 push {r12}

 //Save the Floating point FPEXC onto the old stack
 fmrx r12, fpexc
 push {r12}
 
 //Check if old and new stacks are the same address
 //If they are then simply skip the context switch
 cmp r0, r1
 beq .LSkip 
 
 //Switch thread stacks 
 str sp, [r0]
 ldr sp, [r1]

 //Perform a dummy STREX to clear the Local Monitor status to Open
 mov   r0, #0
 ldr   r1, .LARMv6DummySTREX
 strex r2, r0, [r1]
 
.LSkip:
 //Restore the Floating point FPEXC from the new stack
 pop {r12}
 fmxr fpexc, r12
 
 //Restore the Floating point FPSCR from the new stack
 pop {r12}
 fmxr fpscr, r12
  
 //Restore the Floating point registers D0 to D15 from the new stack
 fldmiad sp!, {d0-d15}

 //Return to SWI mode and stack after context switch
 cps #ARM_MODE_SVC
 bx  lr 
 
.LARMv6DummySTREX:
 .long ARMv6DummySTREX

 //Note: Compiler adds "mov pc, lr" or "bx lr" to the end of this. Should not be an issue due to bx lr above
end;

{==============================================================================}

function ARMv6InterlockedOr(var Target:LongInt;Value:LongInt):LongInt; assembler; nostackframe;
{Perform an atomic OR operation using LDREX/STREX. See page ???}
asm
.LLoop:
 ldrex r2, [r0]
 orr   r12, r1, r2
 strex r3, r12, [r0]
 cmp r3, #0
 bne .LLoop
 
 //If successful then execute a data memory barrier before accessing protected resource
 mov   r3, #0
 mcr   p15, #0, r3, cr7, cr10, #5
 
 //Return previous value
 mov  r0, r2
end;

{==============================================================================}

function ARMv6InterlockedXor(var Target:LongInt;Value:LongInt):LongInt; assembler; nostackframe;
{Perform an atomic XOR operation using LDREX/STREX. See page ???}
asm
.LLoop:
 ldrex r2, [r0]
 eor   r12, r1, r2
 strex r3, r12, [r0]
 cmp r3, #0
 bne .LLoop
 
 //If successful then execute a data memory barrier before accessing protected resource
 mov   r3, #0
 mcr   p15, #0, r3, cr7, cr10, #5
 
 //Return previous value
 mov  r0, r2
end;

{==============================================================================}

function ARMv6InterlockedAnd(var Target:LongInt;Value:LongInt):LongInt; assembler; nostackframe;
{Perform an atomic AND operation using LDREX/STREX. See page ???}
asm
.LLoop:
 ldrex r2, [r0]
 and   r12, r1, r2
 strex r3, r12, [r0]
 cmp r3, #0
 bne .LLoop
 
 //If successful then execute a data memory barrier before accessing protected resource
 mov   r3, #0
 mcr   p15, #0, r3, cr7, cr10, #5
 
 //Return previous value
 mov  r0, r2
end;

{==============================================================================}

function ARMv6InterlockedDecrement(var Target:LongInt):LongInt; assembler; nostackframe;
{Perform an atomic decrement operation using LDREX/STREX. See page 8-6 of the ARM1176JZF-S Technical Reference Manual}
asm
.LLoop:
 ldrex r1, [r0]
 sub   r1, r1, #1
 strex r2, r1, [r0]
 cmp r2, #0
 bne .LLoop
 
 //If successful then execute a data memory barrier before accessing protected resource
 mov   r3, #0
 mcr   p15, #0, r3, cr7, cr10, #5
 
 //Return previous value
 movs r0, r1
end;

{==============================================================================}

function ARMv6InterlockedIncrement(var Target:LongInt):LongInt; assembler; nostackframe;
{Perform an atomic increment operation using LDREX/STREX. See page 8-6 of the ARM1176JZF-S Technical Reference Manual}
asm
.LLoop:
 ldrex r1, [r0]
 add   r1, r1, #1
 strex r2, r1, [r0]
 cmp r2, #0
 bne .LLoop
 
 //If successful then execute a data memory barrier before accessing protected resource
 mov   r3, #0
 mcr   p15, #0, r3, cr7, cr10, #5
 
 //Return previous value
 mov r0, r1
end;

{==============================================================================}

function ARMv6InterlockedExchange(var Target:LongInt;Source:LongInt):LongInt; assembler; nostackframe;
{Perform an atomic exchange operation using LDREX/STREX. See page 8-6 of the ARM1176JZF-S Technical Reference Manual}
asm
.LLoop:
 ldrex r2, [r0]
 strex r3, r1, [r0]
 cmp r3, #0
 bne .LLoop
 
 //If successful then execute a data memory barrier before accessing protected resource
 mov   r3, #0
 mcr   p15, #0, r3, cr7, cr10, #5
 
 //Return previous value
 mov r0, r2
end;

{==============================================================================}

function ARMv6InterlockedAddExchange(var Target:LongInt;Source:LongInt):LongInt; assembler; nostackframe;
{Perform an atomic add and exchange operation using LDREX/STREX. See page 8-6 of the ARM1176JZF-S Technical Reference Manual}
asm
.LLoop:
 ldrex r2, [r0]
 add   r12, r1, r2
 strex r3, r12, [r0]
 cmp r3, #0
 bne .LLoop
 
 //If successful then execute a data memory barrier before accessing protected resource
 mov   r3, #0
 mcr   p15, #0, r3, cr7, cr10, #5
 
 //Return previous value
 mov  r0, r2
end;

{==============================================================================}

function ARMv6InterlockedCompareExchange(var Target:LongInt;Source,Compare:LongInt):LongInt; assembler; nostackframe;
{Perform an atomic compare and exchange operation using LDREX/STREX. See page 8-6 of the ARM1176JZF-S Technical Reference Manual}
asm
.LLoop:
 ldrex    r3, [r0]
 mov      r12, #0
 cmp      r3, r2
 strexeq  r12, r1, [r0]
 cmp      r12, #0
 bne      .LLoop
 
 //If successful then execute a data memory barrier before accessing protected resource
 mov   r3, #0
 mcr   p15, #0, r3, cr7, cr10, #5
 
 //Return previous value
 mov      r0, r3
end;

{==============================================================================}

procedure ARMv6PageTableGetEntry(Address:PtrUInt;var Entry:TPageTableEntry);
{Get and Decode the entry in the Page Table that corresponds to the supplied virtual address}
var
 TableEntry:LongWord;
begin
 {}
 FillChar(Entry,SizeOf(TPageTableEntry),0);
 
 {Check Address}
 {Zero may be valid}
 
 {Get Coarse}
 TableEntry:=ARMv6GetPageTableCoarse(Address);
 if TableEntry <> 0 then
  begin
   {Get Small}
   TableEntry:=ARMv6GetPageTableSmall(Address);
   if TableEntry <> 0 then
    begin
     {Get Virtual Address}
     Entry.VirtualAddress:=(Address and ARMV6_L2D_SMALL_BASE_MASK);

     {Get Physical Address and Size}
     Entry.PhysicalAddress:=(TableEntry and ARMV6_L2D_SMALL_BASE_MASK);
     Entry.Size:=SIZE_4K;
    
     {Get Flags} {ARMv6 uses the standard L2D values (Not Cacheable or TEX Remap values)}
     Entry.Flags:=PAGE_TABLE_FLAG_NONE;
    
     {Check Normal/Cacheable/WriteBack/WriteThrough}     
     if (TableEntry and (ARMV6_L2D_SMALL_TEX_MASK or ARMV6_L2D_FLAG_C or ARMV6_L2D_FLAG_B)) = ARMV6_L2D_SMALL_CACHE_NORMAL_NONCACHED then
      begin
       Entry.Flags:=Entry.Flags or PAGE_TABLE_FLAG_NORMAL;
      end
     else if (TableEntry and (ARMV6_L2D_SMALL_TEX_MASK or ARMV6_L2D_FLAG_C or ARMV6_L2D_FLAG_B)) = ARMV6_L2D_SMALL_CACHE_NORMAL_WRITE_BACK then 
      begin
       Entry.Flags:=Entry.Flags or PAGE_TABLE_FLAG_NORMAL or PAGE_TABLE_FLAG_CACHEABLE or PAGE_TABLE_FLAG_WRITEBACK;
      end
     else if (TableEntry and (ARMV6_L2D_SMALL_TEX_MASK or ARMV6_L2D_FLAG_C or ARMV6_L2D_FLAG_B)) = ARMV6_L2D_SMALL_CACHE_NORMAL_WRITE_THROUGH then 
      begin
       Entry.Flags:=Entry.Flags or PAGE_TABLE_FLAG_NORMAL or PAGE_TABLE_FLAG_CACHEABLE or PAGE_TABLE_FLAG_WRITETHROUGH;
      end;
     
     {Check Device}
     if (TableEntry and (ARMV6_L2D_SMALL_TEX_MASK or ARMV6_L2D_FLAG_C or ARMV6_L2D_FLAG_B)) = ARMV6_L2D_SMALL_CACHE_SHARED_DEVICE then
      begin
       Entry.Flags:=Entry.Flags or PAGE_TABLE_FLAG_DEVICE or PAGE_TABLE_FLAG_SHARED;
      end
     else if (TableEntry and (ARMV6_L2D_SMALL_TEX_MASK or ARMV6_L2D_FLAG_C or ARMV6_L2D_FLAG_B)) = ARMV6_L2D_SMALL_CACHE_NONSHARED_DEVICE then
      begin
       Entry.Flags:=Entry.Flags or PAGE_TABLE_FLAG_DEVICE;
      end;
     
     {Check Ordered}
     if (TableEntry and (ARMV6_L2D_SMALL_TEX_MASK or ARMV6_L2D_FLAG_C or ARMV6_L2D_FLAG_B)) = ARMV6_L2D_SMALL_CACHE_STRONGLY_ORDERED then
      begin
       Entry.Flags:=Entry.Flags or PAGE_TABLE_FLAG_ORDERED or PAGE_TABLE_FLAG_SHARED;
      end;
     
     {Check Shared}
     if (TableEntry and ARMV6_L2D_FLAG_SHARED) = ARMV6_L2D_FLAG_SHARED then
      begin
       Entry.Flags:=Entry.Flags or PAGE_TABLE_FLAG_SHARED;
      end;
     
     {Check ReadOnly}
     if (TableEntry and (ARMV6_L2D_AP_MASK or ARMV6_L2D_FLAG_APX)) = ARMV6_L2D_ACCESS_READONLY then
      begin
       Entry.Flags:=Entry.Flags or PAGE_TABLE_FLAG_READONLY;
      end;
     
     {Check ReadWrite}
     if (TableEntry and (ARMV6_L2D_AP_MASK or ARMV6_L2D_FLAG_APX)) = ARMV6_L2D_ACCESS_READWRITE then
      begin
       Entry.Flags:=Entry.Flags or PAGE_TABLE_FLAG_READWRITE;
      end;
     
     {Check Executable}
     if (TableEntry and ARMV6_L2D_FLAG_SMALL_XN) <> ARMV6_L2D_FLAG_SMALL_XN then
      begin
       Entry.Flags:=Entry.Flags or PAGE_TABLE_FLAG_EXECUTABLE;
      end;
    end
   else
    begin   
     {Get Large}
     TableEntry:=ARMv6GetPageTableLarge(Address);
     if TableEntry <> 0 then
      begin
       {Get Virtual Address}
       Entry.VirtualAddress:=(Address and ARMV6_L2D_LARGE_BASE_MASK);
       
       {Get Physical Address and Size}
       Entry.PhysicalAddress:=(TableEntry and ARMV6_L2D_LARGE_BASE_MASK);
       Entry.Size:=SIZE_64K;
       
       {Get Flags} {ARMv6 uses the standard L2D values (Not Cacheable or TEX Remap values)}
       Entry.Flags:=PAGE_TABLE_FLAG_NONE;
       
       {Check Normal/Cacheable/WriteBack/WriteThrough}     
       if (TableEntry and (ARMV6_L2D_LARGE_TEX_MASK or ARMV6_L2D_FLAG_C or ARMV6_L2D_FLAG_B)) = ARMV6_L2D_LARGE_CACHE_NORMAL_NONCACHED then
        begin
         Entry.Flags:=Entry.Flags or PAGE_TABLE_FLAG_NORMAL;
        end
       else if (TableEntry and (ARMV6_L2D_LARGE_TEX_MASK or ARMV6_L2D_FLAG_C or ARMV6_L2D_FLAG_B)) = ARMV6_L2D_LARGE_CACHE_NORMAL_WRITE_BACK then 
        begin
         Entry.Flags:=Entry.Flags or PAGE_TABLE_FLAG_NORMAL or PAGE_TABLE_FLAG_CACHEABLE or PAGE_TABLE_FLAG_WRITEBACK;
        end
       else if (TableEntry and (ARMV6_L2D_LARGE_TEX_MASK or ARMV6_L2D_FLAG_C or ARMV6_L2D_FLAG_B)) = ARMV6_L2D_LARGE_CACHE_NORMAL_WRITE_THROUGH then 
        begin
         Entry.Flags:=Entry.Flags or PAGE_TABLE_FLAG_NORMAL or PAGE_TABLE_FLAG_CACHEABLE or PAGE_TABLE_FLAG_WRITETHROUGH;
        end;
       
       {Check Device}
       if (TableEntry and (ARMV6_L2D_LARGE_TEX_MASK or ARMV6_L2D_FLAG_C or ARMV6_L2D_FLAG_B)) = ARMV6_L2D_LARGE_CACHE_SHARED_DEVICE then
        begin
         Entry.Flags:=Entry.Flags or PAGE_TABLE_FLAG_DEVICE or PAGE_TABLE_FLAG_SHARED;
        end
       else if (TableEntry and (ARMV6_L2D_LARGE_TEX_MASK or ARMV6_L2D_FLAG_C or ARMV6_L2D_FLAG_B)) = ARMV6_L2D_LARGE_CACHE_NONSHARED_DEVICE then
        begin
         Entry.Flags:=Entry.Flags or PAGE_TABLE_FLAG_DEVICE;
        end;
       
       {Check Ordered}
       if (TableEntry and (ARMV6_L2D_LARGE_TEX_MASK or ARMV6_L2D_FLAG_C or ARMV6_L2D_FLAG_B)) = ARMV6_L2D_LARGE_CACHE_STRONGLY_ORDERED then
        begin
         Entry.Flags:=Entry.Flags or PAGE_TABLE_FLAG_ORDERED or PAGE_TABLE_FLAG_SHARED;
        end;
       
       {Check Shared}
       if (TableEntry and ARMV6_L2D_FLAG_SHARED) = ARMV6_L2D_FLAG_SHARED then
        begin
         Entry.Flags:=Entry.Flags or PAGE_TABLE_FLAG_SHARED;
        end;
       
       {Check ReadOnly}
       if (TableEntry and (ARMV6_L2D_AP_MASK or ARMV6_L2D_FLAG_APX)) = ARMV6_L2D_ACCESS_READONLY then
        begin
         Entry.Flags:=Entry.Flags or PAGE_TABLE_FLAG_READONLY;
        end;
       
       {Check ReadWrite}
       if (TableEntry and (ARMV6_L2D_AP_MASK or ARMV6_L2D_FLAG_APX)) = ARMV6_L2D_ACCESS_READWRITE then
        begin
         Entry.Flags:=Entry.Flags or PAGE_TABLE_FLAG_READWRITE;
        end;
       
       {Check Executable}
       if (TableEntry and ARMV6_L2D_FLAG_LARGE_XN) <> ARMV6_L2D_FLAG_LARGE_XN then
        begin
         Entry.Flags:=Entry.Flags or PAGE_TABLE_FLAG_EXECUTABLE;
        end;
      end;
    end;  
  end
 else
  begin 
   {Get Section}
   TableEntry:=ARMv6GetPageTableSection(Address);
   if TableEntry <> 0 then
    begin
     {Check Supersection}
     if (TableEntry and ARMV6_L1D_FLAG_SUPERSECTION) = 0 then
      begin
       {Get Virtual Address}
       Entry.VirtualAddress:=(Address and ARMV6_L1D_SECTION_BASE_MASK);
      
       {Get Physical Address and Size}
       Entry.PhysicalAddress:=(TableEntry and ARMV6_L1D_SECTION_BASE_MASK);
       Entry.Size:=SIZE_1M;
      end
     else
      begin
       {Get Virtual Address}
       Entry.VirtualAddress:=(Address and ARMV6_L1D_SUPERSECTION_BASE_MASK);

       {Get Physical Address and Size}
       Entry.PhysicalAddress:=(TableEntry and ARMV6_L1D_SUPERSECTION_BASE_MASK);
       Entry.Size:=SIZE_16M;
      end;      
     
     {Get Flags} {ARMv6 uses the standard L1D values (Not Cacheable or TEX Remap values)}
     Entry.Flags:=PAGE_TABLE_FLAG_NONE;
     
     {Check Normal/Cacheable/WriteBack/WriteThrough}     
     if (TableEntry and (ARMV6_L1D_TEX_MASK or ARMV6_L1D_FLAG_C or ARMV6_L1D_FLAG_B)) = ARMV6_L1D_CACHE_NORMAL_NONCACHED then
      begin
       Entry.Flags:=Entry.Flags or PAGE_TABLE_FLAG_NORMAL;
      end
     else if (TableEntry and (ARMV6_L1D_TEX_MASK or ARMV6_L1D_FLAG_C or ARMV6_L1D_FLAG_B)) = ARMV6_L1D_CACHE_NORMAL_WRITE_BACK then 
      begin
       Entry.Flags:=Entry.Flags or PAGE_TABLE_FLAG_NORMAL or PAGE_TABLE_FLAG_CACHEABLE or PAGE_TABLE_FLAG_WRITEBACK;
      end
     else if (TableEntry and (ARMV6_L1D_TEX_MASK or ARMV6_L1D_FLAG_C or ARMV6_L1D_FLAG_B)) = ARMV6_L1D_CACHE_NORMAL_WRITE_THROUGH then 
      begin
       Entry.Flags:=Entry.Flags or PAGE_TABLE_FLAG_NORMAL or PAGE_TABLE_FLAG_CACHEABLE or PAGE_TABLE_FLAG_WRITETHROUGH;
      end;
     
     {Check Device}
     if (TableEntry and (ARMV6_L1D_TEX_MASK or ARMV6_L1D_FLAG_C or ARMV6_L1D_FLAG_B)) = ARMV6_L1D_CACHE_SHARED_DEVICE then
      begin
       Entry.Flags:=Entry.Flags or PAGE_TABLE_FLAG_DEVICE or PAGE_TABLE_FLAG_SHARED;
      end
     else if (TableEntry and (ARMV6_L1D_TEX_MASK or ARMV6_L1D_FLAG_C or ARMV6_L1D_FLAG_B)) = ARMV6_L1D_CACHE_NONSHARED_DEVICE then
      begin
       Entry.Flags:=Entry.Flags or PAGE_TABLE_FLAG_DEVICE;
      end;
     
     {Check Ordered}
     if (TableEntry and (ARMV6_L1D_TEX_MASK or ARMV6_L1D_FLAG_C or ARMV6_L1D_FLAG_B)) = ARMV6_L1D_CACHE_STRONGLY_ORDERED then
      begin
       Entry.Flags:=Entry.Flags or PAGE_TABLE_FLAG_ORDERED or PAGE_TABLE_FLAG_SHARED;
      end;
     
     {Check Shared}
     if (TableEntry and ARMV6_L1D_FLAG_SHARED) = ARMV6_L1D_FLAG_SHARED then
      begin
       Entry.Flags:=Entry.Flags or PAGE_TABLE_FLAG_SHARED;
      end;
     
     {Check ReadOnly}
     if (TableEntry and (ARMV6_L1D_AP_MASK or ARMV6_L1D_FLAG_APX)) = ARMV6_L1D_ACCESS_READONLY then
      begin
       Entry.Flags:=Entry.Flags or PAGE_TABLE_FLAG_READONLY;
      end;
     
     {Check ReadWrite}
     if (TableEntry and (ARMV6_L1D_AP_MASK or ARMV6_L1D_FLAG_APX)) = ARMV6_L1D_ACCESS_READWRITE then
      begin
       Entry.Flags:=Entry.Flags or PAGE_TABLE_FLAG_READWRITE;
      end;
     
     {Check Executable}
     if (TableEntry and ARMV6_L1D_FLAG_XN) <> ARMV6_L1D_FLAG_XN then
      begin
       Entry.Flags:=Entry.Flags or PAGE_TABLE_FLAG_EXECUTABLE;
      end;
    end;
  end; 
end;

{==============================================================================}

function ARMv6PageTableSetEntry(const Entry:TPageTableEntry):LongWord; 
{Encode and Set an entry in the Page Table that corresponds to the supplied virtual address}
var
 CoarseBase:PtrUInt;
 
 TableBase:PtrUInt;
 TableFlags:LongWord;
 TableEntry:LongWord;
 
 ReadMask:LongWord;
 CacheMask:LongWord;
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
     {4KB Small Page}
     {Get Coarse}
     TableEntry:=ARMv6GetPageTableCoarse(Entry.VirtualAddress);
     if TableEntry = 0 then
      begin
       {Allocate Coarse}
       if PAGE_TABLES_FREE = 0 then Exit;
       
       {Update Free/Used}
       Dec(PAGE_TABLES_FREE);
       Inc(PAGE_TABLES_USED);
       
       {Get Table Base}
       TableBase:=(Entry.VirtualAddress and ARMV6_L1D_SECTION_BASE_MASK);
       
       {Get Coarse Base}
       CoarseBase:=(PAGE_TABLES_NEXT and ARMV6_L1D_COARSE_BASE_MASK);
       
       {Update Next}
       Inc(PAGE_TABLES_NEXT,SIZE_1K);
       
       {Set Coarse}
       if not ARMv6SetPageTableCoarse(TableBase,CoarseBase,0) then
        begin
         {Reset Free/Used/Next}
         Inc(PAGE_TABLES_FREE);
         Dec(PAGE_TABLES_USED);
         Dec(PAGE_TABLES_NEXT,SIZE_1K);
         
         Exit;
        end;
       
       {Clean Data Cache Range (Coarse Page)}
       ARMv6CleanDataCacheRange(CoarseBase,SIZE_1K);
       
       {Clean Data Cache Range (Page Table)}
       ARMv6CleanDataCacheRange(PAGE_TABLE_BASE,PAGE_TABLE_SIZE);
       
       {Invalidate TLB}
       ARMv6InvalidateTLB;
      end
     else
      begin
       {Get Coarse Base}
       CoarseBase:=(TableEntry and ARMV6_L1D_COARSE_BASE_MASK);
      end;     
      
     {Get Small}
     TableEntry:=ARMv6GetPageTableSmall(Entry.VirtualAddress);
     if TableEntry = 0 then
      begin
       {Not Supported}
       Exit;
      end;
      
     {Get Masks}
     ReadMask:=PAGE_TABLE_FLAG_READONLY or PAGE_TABLE_FLAG_READWRITE;
     CacheMask:=PAGE_TABLE_FLAG_NORMAL or PAGE_TABLE_FLAG_DEVICE or PAGE_TABLE_FLAG_ORDERED or PAGE_TABLE_FLAG_CACHEABLE or PAGE_TABLE_FLAG_WRITEBACK or PAGE_TABLE_FLAG_WRITETHROUGH or PAGE_TABLE_FLAG_WRITEALLOCATE;
     
     {Get Flags}
     TableFlags:=0;
     
     {Check Normal/Cacheable/WriteBack/WriteThrough/Device/Ordered}
     if (Entry.Flags and CacheMask) = PAGE_TABLE_FLAG_NORMAL then
      begin
       TableFlags:=TableFlags or ARMV6_L2D_SMALL_CACHE_NORMAL_NONCACHED;
      end
     else if (Entry.Flags and CacheMask) = (PAGE_TABLE_FLAG_NORMAL or PAGE_TABLE_FLAG_CACHEABLE or PAGE_TABLE_FLAG_WRITEBACK) then 
      begin
       TableFlags:=TableFlags or ARMV6_L2D_SMALL_CACHE_NORMAL_WRITE_BACK;
      end
     else if (Entry.Flags and CacheMask) = (PAGE_TABLE_FLAG_NORMAL or PAGE_TABLE_FLAG_CACHEABLE or PAGE_TABLE_FLAG_WRITETHROUGH) then  
      begin
       TableFlags:=TableFlags or ARMV6_L2D_SMALL_CACHE_NORMAL_WRITE_THROUGH;
      end
     else if (Entry.Flags and (CacheMask or PAGE_TABLE_FLAG_SHARED)) = (PAGE_TABLE_FLAG_DEVICE or PAGE_TABLE_FLAG_SHARED) then
      begin
       TableFlags:=TableFlags or ARMV6_L2D_SMALL_CACHE_SHARED_DEVICE;
      end
     else if (Entry.Flags and CacheMask) = PAGE_TABLE_FLAG_DEVICE then
      begin
       TableFlags:=TableFlags or ARMV6_L2D_SMALL_CACHE_NONSHARED_DEVICE;
      end
     else if (Entry.Flags and CacheMask) = PAGE_TABLE_FLAG_ORDERED then
      begin
       TableFlags:=TableFlags or ARMV6_L2D_SMALL_CACHE_STRONGLY_ORDERED;
      end
     else 
      begin
       {Not Supported}
       Exit;
      end;      
     
     {Check Shared (Only for Normal memory)}
     if (Entry.Flags and (PAGE_TABLE_FLAG_SHARED or PAGE_TABLE_FLAG_NORMAL)) = (PAGE_TABLE_FLAG_SHARED or PAGE_TABLE_FLAG_NORMAL) then
      begin
       TableFlags:=TableFlags or ARMV6_L2D_FLAG_SHARED;
      end; 
     
     {Check NoAccess / ReadOnly / ReadWrite}
     if (Entry.Flags and ReadMask) = PAGE_TABLE_FLAG_NONE then
      begin
       {Nothing}
      end
     else if (Entry.Flags and ReadMask) = PAGE_TABLE_FLAG_READONLY then
      begin
       TableFlags:=TableFlags or ARMV6_L2D_ACCESS_READONLY;
      end
     else if (Entry.Flags and ReadMask) = PAGE_TABLE_FLAG_READWRITE then 
      begin
       TableFlags:=TableFlags or ARMV6_L2D_ACCESS_READWRITE;
      end
     else
      begin
       {Not Supported}
       Exit;
      end;
     
     {Check Executable}
     if (Entry.Flags and PAGE_TABLE_FLAG_EXECUTABLE) <> PAGE_TABLE_FLAG_EXECUTABLE then
      begin
       TableFlags:=TableFlags or ARMV6_L2D_FLAG_SMALL_XN;
      end;
     
     {Update Small}
     if ARMv6SetPageTableSmall(Entry.VirtualAddress,Entry.PhysicalAddress,TableFlags) then
      begin
       {Clean Data Cache Range (Coarse Page)}
       ARMv6CleanDataCacheRange(CoarseBase,SIZE_1K);
       
       {Clean Data Cache Range (Page Table)}
       ARMv6CleanDataCacheRange(PAGE_TABLE_BASE,PAGE_TABLE_SIZE);
       
       {Invalidate TLB}
       ARMv6InvalidateTLB;
       
       {Return Result}
       Result:=ERROR_SUCCESS;
      end; 
    end;
   SIZE_64K:begin
     {64KB Large Page}
     {Not Supported}
     Exit;
    end;
   SIZE_1M:begin
     {1MB Section}
     {Get Section}
     TableEntry:=ARMv6GetPageTableSection(Entry.VirtualAddress);
     if TableEntry = 0 then
      begin
       {Not Supported}
       Exit;
      end
     else
      begin
       {Check Supersection}
       if (TableEntry and ARMV6_L1D_FLAG_SUPERSECTION) <> 0 then
        begin
         {Not Supported}
         Exit;
        end;
      end;      
     
     {Get Masks}     
     ReadMask:=PAGE_TABLE_FLAG_READONLY or PAGE_TABLE_FLAG_READWRITE;
     CacheMask:=PAGE_TABLE_FLAG_NORMAL or PAGE_TABLE_FLAG_DEVICE or PAGE_TABLE_FLAG_ORDERED or PAGE_TABLE_FLAG_CACHEABLE or PAGE_TABLE_FLAG_WRITEBACK or PAGE_TABLE_FLAG_WRITETHROUGH or PAGE_TABLE_FLAG_WRITEALLOCATE;
     
     {Get Flags}
     TableFlags:=0;
     
     {Check Normal/Cacheable/WriteBack/WriteThrough/Device/Ordered}
     if (Entry.Flags and CacheMask) = PAGE_TABLE_FLAG_NORMAL then
      begin
       TableFlags:=TableFlags or ARMV6_L1D_CACHE_NORMAL_NONCACHED;
      end
     else if (Entry.Flags and CacheMask) = (PAGE_TABLE_FLAG_NORMAL or PAGE_TABLE_FLAG_CACHEABLE or PAGE_TABLE_FLAG_WRITEBACK) then 
      begin
       TableFlags:=TableFlags or ARMV6_L1D_CACHE_NORMAL_WRITE_BACK;
      end
     else if (Entry.Flags and CacheMask) = (PAGE_TABLE_FLAG_NORMAL or PAGE_TABLE_FLAG_CACHEABLE or PAGE_TABLE_FLAG_WRITETHROUGH) then  
      begin
       TableFlags:=TableFlags or ARMV6_L1D_CACHE_NORMAL_WRITE_THROUGH;
      end
     else if (Entry.Flags and (CacheMask or PAGE_TABLE_FLAG_SHARED)) = (PAGE_TABLE_FLAG_DEVICE or PAGE_TABLE_FLAG_SHARED) then
      begin
       TableFlags:=TableFlags or ARMV6_L1D_CACHE_SHARED_DEVICE;
      end
     else if (Entry.Flags and CacheMask) = PAGE_TABLE_FLAG_DEVICE then
      begin
       TableFlags:=TableFlags or ARMV6_L1D_CACHE_NONSHARED_DEVICE;
      end
     else if (Entry.Flags and CacheMask) = PAGE_TABLE_FLAG_ORDERED then
      begin
       TableFlags:=TableFlags or ARMV6_L1D_CACHE_STRONGLY_ORDERED;
      end
     else 
      begin
       {Not Supported}
       Exit;
      end;      
     
     {Check Shared (Only for Normal memory)}
     if (Entry.Flags and (PAGE_TABLE_FLAG_SHARED or PAGE_TABLE_FLAG_NORMAL)) = (PAGE_TABLE_FLAG_SHARED or PAGE_TABLE_FLAG_NORMAL) then
      begin
       TableFlags:=TableFlags or ARMV6_L1D_FLAG_SHARED;
      end; 
     
     {Check NoAccess / ReadOnly / ReadWrite}
     if (Entry.Flags and ReadMask) = PAGE_TABLE_FLAG_NONE then
      begin
       {Nothing}
      end
     else if (Entry.Flags and ReadMask) = PAGE_TABLE_FLAG_READONLY then
      begin
       TableFlags:=TableFlags or ARMV6_L1D_ACCESS_READONLY;
      end
     else if (Entry.Flags and ReadMask) = PAGE_TABLE_FLAG_READWRITE then 
      begin
       TableFlags:=TableFlags or ARMV6_L1D_ACCESS_READWRITE;
      end
     else
      begin
       {Not Supported}
       Exit;
      end;
     
     {Check Executable}
     if (Entry.Flags and PAGE_TABLE_FLAG_EXECUTABLE) <> PAGE_TABLE_FLAG_EXECUTABLE then
      begin
       TableFlags:=TableFlags or ARMV6_L1D_FLAG_XN;
      end;
     
     {Update Section}
     if ARMv6SetPageTableSection(Entry.VirtualAddress,Entry.PhysicalAddress,TableFlags) then
      begin
       {Clean Data Cache Range (Page Table)}
       ARMv6CleanDataCacheRange(PAGE_TABLE_BASE,PAGE_TABLE_SIZE);
       
       {Invalidate TLB}
       ARMv6InvalidateTLB;
    
       {Return Result}
       Result:=ERROR_SUCCESS;
      end; 
    end;
   SIZE_16M:begin
     {16MB Supersection}
     {Not Supported}
     Exit;
    end;
  end;  
 finally
  {Release Lock}
  if PageTableLock.Lock <> INVALID_HANDLE_VALUE then PageTableLock.ReleaseLock(PageTableLock.Lock);
 end;
end;

{==============================================================================}

function ARMv6VectorTableGetEntry(Number:LongWord):PtrUInt;
{Return the address of the specified vector table entry number}
var
 Offset:PtrUInt;
 Entry:TPageTableEntry;
begin
 {}
 Result:=0;
 
 {Check Number}
 if Number >= VECTOR_TABLE_COUNT then Exit;
 
 {Calculate Offset}
 Offset:=VECTOR_TABLE_BASE + (Number shl 2) + 32; {Vector entries use "ldr pc, [pc, #24]" for each entry}
 
 {Get Page Table Entry}
 PageTableGetEntry(Offset,Entry);
 
 {Check for Read Only or Read Write}
 if (Entry.Flags and (PAGE_TABLE_FLAG_READONLY or PAGE_TABLE_FLAG_READWRITE)) <> 0 then
  begin
   {Get Entry}
   Result:=PPtrUInt(Offset)^;
  end; 
end;

{==============================================================================}

function ARMv6VectorTableSetEntry(Number:LongWord;Address:PtrUInt):LongWord;
{Set the supplied address as the value of the specified vector table entry number}
var
 Offset:PtrUInt;
 Flags:LongWord;
 Entry:TPageTableEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Number}
 if Number >= VECTOR_TABLE_COUNT then Exit;

 {Check Address}
 {Zero may be valid}

 {Acquire Lock}
 if VectorTableLock.Lock <> INVALID_HANDLE_VALUE then VectorTableLock.AcquireLock(VectorTableLock.Lock);
 try 
  {Calculate Offset}
  Offset:=VECTOR_TABLE_BASE + (Number shl 2) + 32; {Vector entries use "ldr pc, [pc, #24]" for each entry}
 
  {Get Page Table Entry}
  PageTableGetEntry(Offset,Entry);
  
  {Check for Read Only}
  if (Entry.Flags and PAGE_TABLE_FLAG_READONLY) <> 0 then
   begin
    {Modify Flags (Change to Read Write)}
    Flags:=Entry.Flags;
    Entry.Flags:=Entry.Flags and not(PAGE_TABLE_FLAG_READONLY);
    Entry.Flags:=Entry.Flags or PAGE_TABLE_FLAG_READWRITE;
    
    if PageTableSetEntry(Entry) = ERROR_SUCCESS then
     begin
      {Set Entry}
      PPtrUInt(Offset)^:=Address;
      
      {Clean Data Cache Range}
      ARMv6CleanDataCacheRange(VECTOR_TABLE_BASE,VECTOR_TABLE_SIZE);
      
      {Data Synchronisation Barrier}
      ARMv6DataSynchronizationBarrier;
      
      {Invalidate Instruction Cache}
      ARMv6InvalidateInstructionCache;
      
      {Flush Branch Target Cache}
      ARMv6FlushBranchTargetCache;
      
      {Data Synchronisation Barrier}
      ARMv6DataSynchronizationBarrier;
      
      {Instruction Synchronisation Barrier}
      ARMv6InstructionMemoryBarrier;
      
      {Restore Flags (Back to Read Only)}
      Entry.Flags:=Flags;
      
      {Return Result}
      Result:=PageTableSetEntry(Entry);
     end; 
   end
  else
   begin 
    {Set Entry}
    PPtrUInt(Offset)^:=Address;
    
    {Clean Data Cache Range}
    ARMv6CleanDataCacheRange(VECTOR_TABLE_BASE,VECTOR_TABLE_SIZE);
    
    {Data Synchronisation Barrier}
    ARMv6DataSynchronizationBarrier;
    
    {Invalidate Instruction Cache}
    ARMv6InvalidateInstructionCache;
    
    {Flush Branch Target Cache}
    ARMv6FlushBranchTargetCache;
    
    {Data Synchronisation Barrier}
    ARMv6DataSynchronizationBarrier;
    
    {Instruction Synchronisation Barrier}
    ARMv6InstructionMemoryBarrier;
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   end; 
 finally
  {Release Lock}
  if VectorTableLock.Lock <> INVALID_HANDLE_VALUE then VectorTableLock.ReleaseLock(VectorTableLock.Lock);
 end;
end;

{==============================================================================}

function ARMv6FirstBitSet(Value:LongWord):LongWord; assembler; nostackframe; 
{Note: ARM arm states that CLZ is supported for ARMv5 and above}
asm
 //Count leading zeros
 mov r1, r0
 clz r0, r1
 
 //Subtract from 31
 mov r1, #31
 sub r0, r1, r0
end;

{==============================================================================}

function ARMv6CountLeadingZeros(Value:LongWord):LongWord; assembler; nostackframe; 
{Equivalent of the GCC Builtin function __builtin_clz}
{Note: ARM arm states that CLZ is supported for ARMv5 and above}
asm
 //Count leading zeros
 mov r1, r0 
 clz r0, r1
end;

{==============================================================================}
{==============================================================================}
{ARMv6 Thread Functions}
procedure ARMv6PrimaryInit; assembler; nostackframe;
asm
 //Get the current CPU (Assume CPU 0 always)
 mov r1, #0
 //Multiply by 4 to get the offset in the array
 lsl r1, #2
 
 //Change to IRQ mode
 cpsid if, #ARM_MODE_IRQ
 //Get the IRQ mode stack base
 ldr r0, .LIRQ_STACK_BASE
 ldr r0, [r0]
 ldr r0, [r0, r1]
 //Set the IRQ mode stack pointer
 mov sp, r0
 
 //Change to FIQ mode
 cpsid if, #ARM_MODE_FIQ
 //Get the FIQ mode stack base
 ldr r0, .LFIQ_STACK_BASE
 ldr r0, [r0]
 ldr r0, [r0, r1]
 //Set the FIQ mode stack pointer
 mov sp, r0

 //Change to SWI mode
 cpsid if, #ARM_MODE_SVC
 //Get the SWI mode stack base
 ldr r0, .LSWI_STACK_BASE
 ldr r0, [r0]
 ldr r0, [r0, r1]
 //Set the SWI mode stack pointer
 mov sp, r0

 //Change to ABORT mode
 cpsid if, #ARM_MODE_ABT
 //Get the ABORT mode stack base
 ldr r0, .LABORT_STACK_BASE
 ldr r0, [r0]
 ldr r0, [r0, r1]
 //Set the ABORT mode stack pointer
 mov sp, r0
 
 //Change to UNDEFINED mode
 cpsid if, #ARM_MODE_UND
 //Get the UNDEFINED mode stack base
 ldr r0, .LUNDEFINED_STACK_BASE
 ldr r0, [r0]
 ldr r0, [r0, r1]
 //Set the UNDEFINED mode stack pointer
 mov sp, r0

 //Return to SYS mode (Note: Interrupts remain disabled during initialization)
 cpsid if, #ARM_MODE_SYS
 bx lr
 
.LIRQ_STACK_BASE:
  .long IRQ_STACK_BASE  
.LFIQ_STACK_BASE:
  .long FIQ_STACK_BASE  
.LSWI_STACK_BASE:
  .long SWI_STACK_BASE  
.LABORT_STACK_BASE:
  .long ABORT_STACK_BASE  
.LUNDEFINED_STACK_BASE:
  .long UNDEFINED_STACK_BASE  
end;
 
{==============================================================================}

function ARMv6SpinLock(Spin:PSpinEntry):LongWord; assembler; nostackframe;
{Lock an existing Spin entry}
{Spin: Pointer to the Spin entry to lock (Passed in R0)}
{Return: ERROR_SUCCESS if completed or another error code on failure (Returned in R0)}
asm
 //Load the SPIN_STATE_LOCKED value
 mov   r1, #SPIN_STATE_LOCKED
 
.LAcquireLock: 
 //Load the pointer to the state (TSpinEntry.State)
 add   r12, r0, #4
 //Get the current state of the lock
 ldrex r2, [r12]
 //Compare the current state of the lock
 cmp   r2, r1
 //If already locked then wait for unlock
 beq   .LWaitLock
 //If not locked attempt to acquire the lock
 strex r3, r1, [r12] 
 //Check if the lock was successful
 cmp   r3, #1
 //If failed then try again
 beq   .LAcquireLock
 
 //If successful then execute a data memory barrier before accessing protected resource
 mov   r3, #0
 mcr   p15, #0, r3, cr7, cr10, #5
 
 //Get the current thread
 mrc p15, #0, r2, cr13, cr0, #4
 
 //Set the owner (TSpinEntry.Owner)
 str   r2, [r0, #12]
 
 //Return success
 mov   r0, #ERROR_SUCCESS
 bx    lr
 
.LWaitLock:
 //No point simply retrying over and over, perform a wait for interrupt on each loop
 mov   r3, #0
 mcr   p15, #0, r3, cr7, cr0, #4
 //Get the signature of the lock (TSpinEntry.Signature)
 ldr   r2, [r0, #0]
 //Compare the signature of the lock
 ldr   r3, =SPIN_SIGNATURE
 cmp   r2, r3
 //Fail if the signature is not valid (Lock may have been destroyed)
 bne   .LFailLock
 //Get the current state of the lock (TSpinEntry.State)
 ldr   r2, [r0, #4]
 //Compare the current state of the lock
 cmp   r2, r1
 //If still locked keep waiting (This is intended to prevent cache bouncing on multi CPU systems)
 beq   .LWaitLock
 //If not locked try to acquire lock again
 b     .LAcquireLock
 
.LFailLock:
 //Return failure
 mov   r0, #ERROR_INVALID_PARAMETER
 bx    lr
end;

{==============================================================================}

function ARMv6SpinUnlock(Spin:PSpinEntry):LongWord; assembler; nostackframe;
{Unlock an existing Spin entry}
{Spin: Pointer to the Spin entry to lock (Passed in R0)}
{Return: ERROR_SUCCESS if completed or another error code on failure (Returned in R0)}
asm
 //Get the state of the lock (TSpinEntry.State)
 ldr   r1, [r0, #4]
 cmp   r1, #SPIN_STATE_LOCKED
 //If not locked then return an error
 bne   .LErrorLocked
 
 //Get the owner of the lock (TSpinEntry.Owner)
 ldr   r1, [r0, #12]
 //Get the current thread
 mrc   p15, #0, r2, cr13, cr0, #4
 cmp   r1, r2
 //If not the current thread then return an error
 bne   .LErrorOwner
 
 //Load the SPIN_STATE_UNLOCKED value
 mov   r1, #SPIN_STATE_UNLOCKED
 
 //Release the owner of the lock (TSpinEntry.Owner);
 ldr   r2, =INVALID_HANDLE_VALUE
 str   r2, [r0, #12]
 
 //Execute a data memory barrier before releasing protected resource
 mov   r12, #0
 mcr   p15, #0, r12, cr7, cr10, #5
 
 //Release the lock (TSpinEntry.State)
 str   r1, [r0, #4]
 
 //Return success
 mov   r0, #ERROR_SUCCESS
 bx    lr
 
.LErrorOwner:
 //Return failure
 mov   r0, #ERROR_NOT_OWNER
 bx    lr
 
.LErrorLocked:
 //Return failure
 mov   r0, #ERROR_NOT_LOCKED
 bx    lr
end;

{==============================================================================}

function ARMv6SpinLockIRQ(Spin:PSpinEntry):LongWord; assembler; nostackframe;
{Lock an existing Spin entry, disable IRQ and save the previous IRQ state}
{Spin: Pointer to the Spin entry to lock (Passed in R0)}
{Return: ERROR_SUCCESS if completed or another error code on failure (Returned in R0)}
asm
 //Preserve R4 and R5
 push  {r4, r5}
 
 //Load the SPIN_STATE_LOCKED value
 mov   r1, #SPIN_STATE_LOCKED
 
.LAcquireLockIRQ: 
 //Get the current IRQ mask
 mrs   r4, cpsr
 //Disable IRQ
 cpsid i
 
.LAcquireLock: 
 //Load the pointer to the state (TSpinEntry.State)
 add   r12, r0, #4
 //Get the current state of the lock
 ldrex r2, [r12]
 //Compare the current state of the lock
 cmp   r2, r1
 //If already locked then wait for unlock
 beq   .LWaitLockIRQ
 //If not locked attempt to acquire the lock
 strex r3, r1, [r12] 
 //Check if the lock was successful
 cmp   r3, #1
 //If failed then try again
 beq   .LAcquireLock
 
 //If successful then execute a data memory barrier before accessing protected resource
 mov   r3, #0
 mcr   p15, #0, r3, cr7, cr10, #5
 
 //Save the current IRQ mask (TSpinEntry.Mask)
 str   r4, [r0, #8]
 
 //Get the current thread
 mrc p15, #0, r2, cr13, cr0, #4
 
 //Set the owner (TSpinEntry.Owner)
 str   r2, [r0, #12]
 
 //Return success
 mov   r0, #ERROR_SUCCESS
 //Restore R4 and R5
 pop   {r4, r5}
 bx    lr
 
.LWaitLockIRQ:
 //Restore the current IRQ mask
 msr   cpsr_c, r4
 
.LWaitLock:
 //No point simply retrying over and over, perform a wait for interrupt on each loop
 mov   r3, #0
 mcr   p15, #0, r3, cr7, cr0, #4
 //Get the signature of the lock (TSpinEntry.Signature)
 ldr   r2, [r0, #0]
 //Compare the signature of the lock
 ldr   r3, =SPIN_SIGNATURE
 cmp   r2, r3
 //Fail if the signature is not valid (Lock may have been destroyed)
 bne   .LFailLock
 //Get the current state of the lock (TSpinEntry.State)
 ldr   r2, [r0, #4]
 //Compare the current state of the lock
 cmp   r2, r1
 //If still locked keep waiting (This is intended to prevent cache bouncing on multi CPU systems)
 beq   .LWaitLock
 //If not locked try to acquire lock again
 b     .LAcquireLockIRQ
 
.LFailLock:
 //Return failure
 mov   r0, #ERROR_INVALID_PARAMETER
 //Restore R4 and R5
 pop   {r4, r5}
 bx    lr
end;

{==============================================================================}

function ARMv6SpinUnlockIRQ(Spin:PSpinEntry):LongWord; assembler; nostackframe;
{Unlock an existing Spin entry and restore the previous IRQ state}
{Spin: Pointer to the Spin entry to lock (Passed in R0)}
{Return: ERROR_SUCCESS if completed or another error code on failure (Returned in R0)}
asm
 //Get the state of the lock (TSpinEntry.State)
 ldr   r1, [r0, #4]
 cmp   r1, #SPIN_STATE_LOCKED
 //If not locked then return an error
 bne   .LErrorLocked
 
 //Get the owner of the lock (TSpinEntry.Owner)
 ldr   r1, [r0, #12]
 //Get the current thread
 mrc   p15, #0, r2, cr13, cr0, #4
 cmp   r1, r2
 //If not the current thread then return an error
 bne   .LErrorOwner
 
 //Load the SPIN_STATE_UNLOCKED value
 mov   r1, #SPIN_STATE_UNLOCKED
 
 //Release the owner of the lock (TSpinEntry.Owner);
 ldr   r2, =INVALID_HANDLE_VALUE
 str   r2, [r0, #12]
 
 //Get the previous IRQ mask (TSpinEntry.Mask)
 ldr   r3, [r0, #8]
 //Clear the previous IRQ mask (TSpinEntry.Mask)
 mov   r12, #0
 str   r12, [r0, #8]
 
 //Execute a data memory barrier before releasing protected resource
 mov   r12, #0
 mcr   p15, #0, r12, cr7, cr10, #5
 
 //Release the lock (TSpinEntry.State)
 str   r1, [r0, #4]
 
 //Restore the previous IRQ mask
 msr   cpsr_c, r3
 
 //Return success
 mov   r0, #ERROR_SUCCESS
 bx    lr
 
.LErrorOwner:
 //Return failure
 mov   r0, #ERROR_NOT_OWNER
 bx    lr
 
.LErrorLocked:
 //Return failure
 mov   r0, #ERROR_NOT_LOCKED
 bx    lr
end;

{==============================================================================}

function ARMv6SpinLockFIQ(Spin:PSpinEntry):LongWord; assembler; nostackframe;
{Lock an existing Spin entry, disable FIQ and save the previous FIQ state}
{Spin: Pointer to the Spin entry to lock (Passed in R0)}
{Return: ERROR_SUCCESS if completed or another error code on failure (Returned in R0)}
asm
 //Preserve R4 and R5
 push  {r4, r5}
 
 //Load the SPIN_STATE_LOCKED value
 mov   r1, #SPIN_STATE_LOCKED
 
.LAcquireLockFIQ: 
 //Get the current FIQ mask
 mrs   r4, cpsr
 //Disable FIQ
 cpsid f
 
.LAcquireLock: 
 //Load the pointer to the state (TSpinEntry.State)
 add   r12, r0, #4
 //Get the current state of the lock
 ldrex r2, [r12]
 //Compare the current state of the lock
 cmp   r2, r1
 //If already locked then wait for unlock
 beq   .LWaitLockFIQ
 //If not locked attempt to acquire the lock
 strex r3, r1, [r12] 
 //Check if the lock was successful
 cmp   r3, #1
 //If failed then try again
 beq   .LAcquireLock
 
 //If successful then execute a data memory barrier before accessing protected resource
 mov   r3, #0
 mcr   p15, #0, r3, cr7, cr10, #5
 
 //Save the current FIQ mask (TSpinEntry.Mask)
 str   r4, [r0, #8]
 
 //Get the current thread
 mrc p15, #0, r2, cr13, cr0, #4
 
 //Set the owner (TSpinEntry.Owner)
 str   r2, [r0, #12]
 
 //Return success
 mov   r0, #ERROR_SUCCESS
 //Restore R4 and R5
 pop   {r4, r5}
 bx    lr
 
.LWaitLockFIQ:
 //Restore the current FIQ mask
 msr   cpsr_c, r4

.LWaitLock:
 //No point simply retrying over and over, perform a wait for interrupt on each loop
 mov   r3, #0
 mcr   p15, #0, r3, cr7, cr0, #4
 //Get the signature of the lock (TSpinEntry.Signature)
 ldr   r2, [r0, #0]
 //Compare the signature of the lock
 ldr   r3, =SPIN_SIGNATURE
 cmp   r2, r3
 //Fail if the signature is not valid (Lock may have been destroyed)
 bne   .LFailLock
 //Get the current state of the lock (TSpinEntry.State)
 ldr   r2, [r0, #4]
 //Compare the current state of the lock
 cmp   r2, r1
 //If still locked keep waiting (This is intended to prevent cache bouncing on multi CPU systems)
 beq   .LWaitLock
 //If not locked try to acquire lock again
 b     .LAcquireLockFIQ
 
.LFailLock:
 //Return failure
 mov   r0, #ERROR_INVALID_PARAMETER
 //Restore R4 and R5
 pop   {r4, r5}
 bx    lr
end;

{==============================================================================}

function ARMv6SpinUnlockFIQ(Spin:PSpinEntry):LongWord; assembler; nostackframe;
{Unlock an existing Spin entry and restore the previous FIQ state}
{Spin: Pointer to the Spin entry to lock (Passed in R0)}
{Return: ERROR_SUCCESS if completed or another error code on failure (Returned in R0)}
asm
 //Get the state of the lock (TSpinEntry.State)
 ldr   r1, [r0, #4]
 cmp   r1, #SPIN_STATE_LOCKED
 //If not locked then return an error
 bne   .LErrorLocked
 
 //Get the owner of the lock (TSpinEntry.Owner)
 ldr   r1, [r0, #12]
 //Get the current thread
 mrc   p15, #0, r2, cr13, cr0, #4
 cmp   r1, r2
 //If not the current thread then return an error
 bne   .LErrorOwner
 
 //Load the SPIN_STATE_UNLOCKED value
 mov   r1, #SPIN_STATE_UNLOCKED
 
 //Release the owner of the lock (TSpinEntry.Owner);
 ldr   r2, =INVALID_HANDLE_VALUE
 str   r2, [r0, #12]
 
 //Get the previous FIQ mask (TSpinEntry.Mask)
 ldr   r3, [r0, #8]
 //Clear the previous FIQ mask (TSpinEntry.Mask)
 mov   r12, #0
 str   r12, [r0, #8]
 
 //Execute a data memory barrier before releasing protected resource
 mov   r12, #0
 mcr   p15, #0, r12, cr7, cr10, #5
 
 //Release the lock (TSpinEntry.State)
 str   r1, [r0, #4]
 
 //Restore the previous FIQ mask
 msr   cpsr_c, r3
 
 //Return success
 mov   r0, #ERROR_SUCCESS
 bx    lr
 
.LErrorOwner:
 //Return failure
 mov   r0, #ERROR_NOT_OWNER
 bx    lr
 
.LErrorLocked:
 //Return failure
 mov   r0, #ERROR_NOT_LOCKED
 bx    lr
end;

{==============================================================================}

function ARMv6SpinLockIRQFIQ(Spin:PSpinEntry):LongWord; assembler; nostackframe;
{Lock an existing Spin entry, disable IRQ and FIQ and save the previous IRQ/FIQ state}
{Spin: Pointer to the Spin entry to lock (Passed in R0)}
{Return: ERROR_SUCCESS if completed or another error code on failure (Returned in R0)}
asm
 //Preserve R4 and R5
 push  {r4, r5}
 
 //Load the SPIN_STATE_LOCKED value
 mov   r1, #SPIN_STATE_LOCKED
 
.LAcquireLockIRQFIQ: 
 //Get the current IRQ/FIQ mask
 mrs   r4, cpsr
 //Disable IRQ/FIQ
 cpsid if
 
.LAcquireLock: 
 //Load the pointer to the state (TSpinEntry.State)
 add   r12, r0, #4
 //Get the current state of the lock
 ldrex r2, [r12]
 //Compare the current state of the lock
 cmp   r2, r1
 //If already locked then wait for unlock
 beq   .LWaitLockIRQFIQ
 //If not locked attempt to acquire the lock
 strex r3, r1, [r12] 
 //Check if the lock was successful
 cmp   r3, #1
 //If failed then try again
 beq   .LAcquireLock
 
 //If successful then execute a data memory barrier before accessing protected resource
 mov   r3, #0
 mcr   p15, #0, r3, cr7, cr10, #5
 
 //Save the current IRQ/FIQ mask (TSpinEntry.Mask)
 str   r4, [r0, #8]
 
 //Get the current thread
 mrc p15, #0, r2, cr13, cr0, #4
 
 //Set the owner (TSpinEntry.Owner)
 str   r2, [r0, #12]
 
 //Return success
 mov   r0, #ERROR_SUCCESS
 //Restore R4 and R5
 pop   {r4, r5}
 bx    lr
 
.LWaitLockIRQFIQ:
 //Restore the current IRQ/FIQ mask
 msr   cpsr_c, r4

.LWaitLock:
 //No point simply retrying over and over, perform a wait for interrupt on each loop
 mov   r3, #0
 mcr   p15, #0, r3, cr7, cr0, #4
 //Get the signature of the lock (TSpinEntry.Signature)
 ldr   r2, [r0, #0]
 //Compare the signature of the lock
 ldr   r3, =SPIN_SIGNATURE
 cmp   r2, r3
 //Fail if the signature is not valid (Lock may have been destroyed)
 bne   .LFailLock
 //Get the current state of the lock (TSpinEntry.State)
 ldr   r2, [r0, #4]
 //Compare the current state of the lock
 cmp   r2, r1
 //If still locked keep waiting (This is intended to prevent cache bouncing on multi CPU systems)
 beq   .LWaitLock
 //If not locked try to acquire lock again
 b     .LAcquireLockIRQFIQ
 
.LFailLock:
 //Return failure
 mov   r0, #ERROR_INVALID_PARAMETER
 //Restore R4 and R5
 pop   {r4, r5}
 bx    lr
end;

{==============================================================================}

function ARMv6SpinUnlockIRQFIQ(Spin:PSpinEntry):LongWord; assembler; nostackframe;
{Unlock an existing Spin entry and restore the previous IRQ/FIQ state}
{Spin: Pointer to the Spin entry to lock (Passed in R0)}
{Return: ERROR_SUCCESS if completed or another error code on failure (Returned in R0)}
asm
 //Get the state of the lock (TSpinEntry.State)
 ldr   r1, [r0, #4]
 cmp   r1, #SPIN_STATE_LOCKED
 //If not locked then return an error
 bne   .LErrorLocked
 
 //Get the owner of the lock (TSpinEntry.Owner)
 ldr   r1, [r0, #12]
 //Get the current thread
 mrc   p15, #0, r2, cr13, cr0, #4
 cmp   r1, r2
 //If not the current thread then return an error
 bne   .LErrorOwner
 
 //Load the SPIN_STATE_UNLOCKED value
 mov   r1, #SPIN_STATE_UNLOCKED
 
 //Release the owner of the lock (TSpinEntry.Owner);
 ldr   r2, =INVALID_HANDLE_VALUE
 str   r2, [r0, #12]
 
 //Get the previous IRQ/FIQ mask (TSpinEntry.Mask)
 ldr   r3, [r0, #8]
 //Clear the previous IRQ/FIQ mask (TSpinEntry.Mask)
 mov   r12, #0
 str   r12, [r0, #8]
 
 //Execute a data memory barrier before releasing protected resource
 mov   r12, #0
 mcr   p15, #0, r12, cr7, cr10, #5
 
 //Release the lock (TSpinEntry.State)
 str   r1, [r0, #4]
 
 //Restore the previous IRQ/FIQ mask
 msr   cpsr_c, r3
 
 //Return success
 mov   r0, #ERROR_SUCCESS
 bx    lr
 
.LErrorOwner:
 //Return failure
 mov   r0, #ERROR_NOT_OWNER
 bx    lr
 
.LErrorLocked:
 //Return failure
 mov   r0, #ERROR_NOT_LOCKED
 bx    lr
end;

{==============================================================================}

function ARMv6SpinCheckIRQ(Spin:PSpinEntry):Boolean;
{Return: True if the mask would enable IRQ on restore, False if it would not}
begin
 {}
 Result:=True;
 
 {Check Spin}
 if Spin = nil then Exit;
 
 {Check Mask}
 if (Spin.Mask and ARM_I_BIT) <> 0 then
  begin
   Result:=False;
  end;
end;

{==============================================================================}

function ARMv6SpinCheckFIQ(Spin:PSpinEntry):Boolean;
{Return: True if the mask would enable FIQ on restore, False if it would not}
begin
 {}
 Result:=True;
 
 {Check Spin}
 if Spin = nil then Exit;
 
 {Check Mask}
 if (Spin.Mask and ARM_F_BIT) <> 0 then
  begin
   Result:=False;
  end;
end;

{==============================================================================}
 
function ARMv6SpinExchangeIRQ(Spin1,Spin2:PSpinEntry):LongWord;
var
 Mask:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Spin1}
 if Spin1 = nil then Exit;
 
 {Check Spin2}
 if Spin2 = nil then Exit;
 
 {Save Spin2 Mask}
 Mask:=Spin2.Mask;
 
 {Check Spin1 Mask}
 if (Spin1.Mask and ARM_I_BIT) = 0 then
  begin
   {Clear Spin2 IRQ Bit}
   Spin2.Mask:=Spin2.Mask and not(ARM_I_BIT);
  end
 else
  begin
   {Set Spin2 IRQ Bit}
   Spin2.Mask:=Spin2.Mask or ARM_I_BIT;
  end;  
 
 {Check Spin2 Mask (Saved)}
 if (Mask and ARM_I_BIT) = 0 then
  begin
   {Clear Spin1 IRQ Bit}
   Spin1.Mask:=Spin1.Mask and not(ARM_I_BIT);
  end
 else
  begin
   {Set Spin1 IRQ Bit}
   Spin1.Mask:=Spin1.Mask or ARM_I_BIT;
  end;  
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function ARMv6SpinExchangeFIQ(Spin1,Spin2:PSpinEntry):LongWord;
var
 Mask:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Spin1}
 if Spin1 = nil then Exit;
 
 {Check Spin2}
 if Spin2 = nil then Exit;
 
 {Save Spin2 Mask}
 Mask:=Spin2.Mask;
 
 {Check Spin1 Mask}
 if (Spin1.Mask and ARM_F_BIT) = 0 then
  begin
   {Clear Spin2 FIQ Bit}
   Spin2.Mask:=Spin2.Mask and not(ARM_F_BIT);
  end
 else
  begin
   {Set Spin2 FIQ Bit}
   Spin2.Mask:=Spin2.Mask or ARM_F_BIT;
  end;  
 
 {Check Spin2 Mask (Saved)}
 if (Mask and ARM_F_BIT) = 0 then
  begin
   {Clear Spin1 FIQ Bit}
   Spin1.Mask:=Spin1.Mask and not(ARM_F_BIT);
  end
 else
  begin
   {Set Spin1 FIQ Bit}
   Spin1.Mask:=Spin1.Mask or ARM_F_BIT;
  end;  
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function ARMv6MutexLock(Mutex:PMutexEntry):LongWord; assembler; nostackframe;
{Lock an existing Mutex entry}
{Mutex: Pointer to the Mutex entry to lock (Passed in R0)}
{Return: ERROR_SUCCESS if completed or another error code on failure (Returned in R0)}
asm
 //Get the flags of the lock (TMutexEntry.Flags)
 ldr   r1, [r0, #20]
 and   r1, r1, #MUTEX_FLAG_RECURSIVE
 cmp   r1, #0
 //If not recursive then acquire the lock
 beq   .LStartLock
 
 //Get the owner of the lock (TMutexEntry.Owner)
 ldr   r1, [r0, #8]
 //Get the current thread
 mrc   p15, #0, r2, cr13, cr0, #4
 cmp   r1, r2
 //If not the current thread then acquire the lock
 bne   .LStartLock
 
 //Get the count of the lock (TMutexEntry.Count)
 ldr   r1, [r0, #16]
 //Increment the count (TMutexEntry.Count)
 add   r1, r1, #1
 str   r1, [r0, #16]
 
 //Return success
 mov   r0, #ERROR_SUCCESS
 bx    lr
 
.LStartLock:
 //Load the MUTEX_STATE_LOCKED value
 mov   r1, #MUTEX_STATE_LOCKED
 
.LAcquireLock: 
 //Load the pointer to the state (TMutexEntry.State)
 add   r12, r0, #4
 //Get the current state of the lock
 ldrex r2, [r12]
 //Compare the current state of the lock
 cmp   r2, r1
 //If already locked then wait for unlock
 beq   .LWaitLock
 //If not locked attempt to acquire the lock
 strex r3, r1, [r12] 
 //Check if the lock was successful
 cmp   r3, #1
 //If failed then try again
 beq   .LAcquireLock
 
 //If successful then execute a data memory barrier before accessing protected resource
 mov   r3, #0
 mcr   p15, #0, r3, cr7, cr10, #5
 
 //Set the count (TMutexEntry.Count)
 mov   r2, #1
 str   r2, [r0, #16]
 
 //Get the current thread
 mrc p15, #0, r2, cr13, cr0, #4
 //Set the owner (TMutexEntry.Owner)
 str   r2, [r0, #8]
 
 //Return success
 mov   r0, #ERROR_SUCCESS
 bx    lr
 
.LWaitLock:
 //Preserve R0, R1 and LR (Include R2 to maintain stack alignment of 8 bytes)
 push  {r0, r1, r2, lr}
 //Get the yield function for the lock (TMutexEntry.Yield)
 //Because this is a procedure variable it already contains the address
 //of the function and does not need to be dereferenced via ldr r3, [r3]
 ldr   r3, [r0, #12]
 //Call the yield function
 blx   r3
 //Restore R0, R1 and LR (Include R2 to maintain stack alignment of 8 bytes)
 pop   {r0, r1, r2, lr}
 //Get the signature of the lock (TMutexEntry.Signature)
 ldr   r2, [r0, #0]
 //Compare the signature of the lock
 ldr   r3, =MUTEX_SIGNATURE
 cmp   r2, r3
 //Fail if the signature is not valid (Lock may have been destroyed)
 bne   .LFailLock
 //Get the current state of the lock (TMutexEntry.State)
 ldr   r2, [r0, #4]
 //Compare the current state of the lock
 cmp   r2, r1
 //If still locked keep waiting (This is intended to prevent cache bouncing on multi CPU systems)
 beq   .LWaitLock
 //If not locked try to acquire lock again
 b     .LAcquireLock
 
.LFailLock:
 //Return failure
 mov   r0, #ERROR_INVALID_PARAMETER
 bx    lr
end;

{==============================================================================}

function ARMv6MutexUnlock(Mutex:PMutexEntry):LongWord; assembler; nostackframe;
{Unlock an existing Mutex entry}
{Mutex: Pointer to the Mutex entry to lock (Passed in R0)}
{Return: ERROR_SUCCESS if completed or another error code on failure (Returned in R0)}
asm
 //Get the state of the lock (TMutexEntry.State)
 ldr   r1, [r0, #4]
 cmp   r1, #MUTEX_STATE_LOCKED
 //If not locked then return an error
 bne   .LErrorLocked
 
 //Get the owner of the lock (TMutexEntry.Owner)
 ldr   r1, [r0, #8]
 //Get the current thread
 mrc   p15, #0, r2, cr13, cr0, #4
 cmp   r1, r2
 //If not the current thread then return an error
 bne   .LErrorOwner

 //Get the flags of the lock (TMutexEntry.Flags)
 ldr   r1, [r0, #20]
 and   r1, r1, #MUTEX_FLAG_RECURSIVE
 cmp   r1, #0
 //If not recursive then release the lock
 beq   .LStartUnlock
 
 //Get the count of the lock (TMutexEntry.Count)
 ldr   r1, [r0, #16]
 cmp   r1, #0
 //If already zero then return an error
 beq   .LErrorInvalid
 
 //Decrement the count (TMutexEntry.Count)
 sub   r1, r1, #1
 //Check the count
 cmp   r1, #0
 //If zero then release the lock
 beq   .LStartUnlock

 //Save the count (TMutexEntry.Count)
 str   r1, [r0, #16]
 
 //Return success
 mov   r0, #ERROR_SUCCESS
 bx    lr
 
.LStartUnlock: 
 //Load the MUTEX_STATE_UNLOCKED value
 mov   r1, #MUTEX_STATE_UNLOCKED
 
 //Reset the count of the lock (TMutexEntry.Count)
 mov   r2, #0
 str   r2, [r0, #16]
 
 //Reset the owner of the lock (TMutexEntry.Owner);
 ldr   r2, =INVALID_HANDLE_VALUE
 str   r2, [r0, #8]
 
 //Execute a data memory barrier before releasing protected resource
 mov   r12, #0
 mcr   p15, #0, r12, cr7, cr10, #5
 
 //Release the lock (TMutexEntry.State)
 str   r1, [r0, #4]
 
 //Return success
 mov   r0, #ERROR_SUCCESS
 bx    lr
 
.LErrorOwner:
 //Return failure
 mov   r0, #ERROR_NOT_OWNER
 bx    lr
 
.LErrorLocked:
 //Return failure
 mov   r0, #ERROR_NOT_LOCKED
 bx    lr
 
.LErrorInvalid:
 //Return failure
 mov   r0, #ERROR_INVALID_FUNCTION
 bx    lr
end;

{==============================================================================}

function ARMv6MutexTryLock(Mutex:PMutexEntry):LongWord; assembler; nostackframe;
{Try to lock an existing Mutex entry}
{Mutex: Pointer to the Mutex entry to try to lock (Passed in R0)}
{Return: ERROR_SUCCESS if completed, ERROR_LOCKED if already locked or another error code on failure (Returned in R0)}
asm
 //Get the flags of the lock (TMutexEntry.Flags)
 ldr   r1, [r0, #20]
 and   r1, r1, #MUTEX_FLAG_RECURSIVE
 cmp   r1, #0
 //If not recursive then acquire the lock
 beq   .LStartLock
 
 //Get the owner of the lock (TMutexEntry.Owner)
 ldr   r1, [r0, #8]
 //Get the current thread
 mrc   p15, #0, r2, cr13, cr0, #4
 cmp   r1, r2
 //If not the current thread then acquire the lock
 bne   .LStartLock
 
 //Get the count of the lock (TMutexEntry.Count)
 ldr   r1, [r0, #16]
 //Increment the count (TMutexEntry.Count)
 add   r1, r1, #1
 str   r1, [r0, #16]
 
 //Return success
 mov   r0, #ERROR_SUCCESS
 bx    lr
 
.LStartLock:
 //Load the MUTEX_STATE_LOCKED value
 mov   r1, #MUTEX_STATE_LOCKED

.LAcquireLock: 
 //Load the pointer to the state (TMutexEntry.State)
 add   r12, r0, #4
 //Get the current state of the lock
 ldrex r2, [r12]
 //Compare the current state of the lock
 cmp   r2, r1
 //If already locked then exit
 beq   .LSkipLock
 //If not locked attempt to acquire the lock
 strex r3, r1, [r12] 
 //Check if the lock was successful
 cmp   r3, #1
 //If failed then try again
 beq   .LAcquireLock
 
 //If successful then execute a data memory barrier before accessing protected resource
 mov   r3, #0
 mcr   p15, #0, r3, cr7, cr10, #5
 
 //Set the count (TMutexEntry.Count)
 mov   r2, #1
 str   r2, [r0, #16]
 
 //Get the current thread
 mrc p15, #0, r2, cr13, cr0, #4
 //Set the owner (TMutexEntry.Owner)
 str   r2, [r0, #8]
 
 //Return success
 mov   r0, #ERROR_SUCCESS
 bx    lr
 
.LSkipLock:
 //Return already locked
 mov   r0, #ERROR_LOCKED
 bx    lr
end;

{==============================================================================}

function ARMv6ThreadGetCurrent:TThreadHandle; assembler; nostackframe;
{Get the current thread id from the c13 (Thread and process ID) register of system control coprocessor CP15
 See page 3-129 of the ARM1176JZF-S Technical Reference Manual}
asm
 //Get the contents of c13 (Thread and process ID) register in system control coprocessor CP15
 mrc p15, #0, r0, cr13, cr0, #4
end;

{==============================================================================}

function ARMv6ThreadSetCurrent(Thread:TThreadHandle):LongWord; assembler; nostackframe;
{Set the current thread id in the c13 (Thread and process ID) register of system control coprocessor CP15
 See page 3-129 of the ARM1176JZF-S Technical Reference Manual}
asm
 //Set the contents of c13 (Thread and process ID) register in system control coprocessor CP15
 mcr p15, #0, r0, cr13, cr0, #4
 //Return success
 mov r0, #ERROR_SUCCESS
end;

{==============================================================================}

function ARMv6ThreadSetupStack(StackBase:Pointer;StartProc:TThreadStart;ReturnProc:TThreadEnd;Parameter:Pointer):Pointer;
{Set up the context record and arguments on the stack for a new thread}
{StackBase: Pointer to the base (highest address) of the allocated stack (as returned by ThreadAllocateStack}
{StartProc: The procedure the thread will start executing when resumed}
{ReturnProc: The procedure the thread will return to on exit}
{Return: Pointer to the starting address of the stack, which will be the Stack Pointer on the first context switch}
{Notes: At the point of a context switch the thread stack will look like this:

        (Base "Highest Address" of Stack)
        .
        .
        .
        .
        cpsr  <- The current program status register value to load on return from the context switch
                  (On Interrupt: Includes the flags and control bits for the interrupted thread)
                  (On Yield: Includes the control bits only for the yielded thread)
                  (On Create: Includes the control bits only for the new thread)

        lr/pc <- The address to return to from the context switch
                  (On Interrupt: The location the thread was at before interrupt)
                  (On Yield: The location to return to in SchedulerReschedule)
                  (On Create: The location of StartProc for the new thread)
        
        lr    <- The lr value prior to the context switch
                  (On Interrupt: The value of lr before the thread was interrupted)
                  (On Yield: The location to return to in SchedulerReschedule)
                  (On Create: The location of ReturnProc for the new thread)
        
        r12   <- 
        r11   <- 
        r10   <- 
        r9    <- 
        r8    <- 
        r7    <- 
        r6    <- The value of these registers prior to the context switch 
        r5    <-  (On Interrupt: The values before the thread was interrupted)
        r4    <-  (On Yield: The values on return to SchedulerReschedule) 
        r3    <-  (On Create: The values on entry to StartProc as set by ThreadSetupStack)
        r2    <- 
        r1    <- 
        r0    <- 
        
        d15   <- 
        d14   <- 
        d13   <- 
        d12   <- 
        d11   <- 
        d10   <- 
        d9    <- 
        d8    <- The value of these floating point registers prior to the context switch
        d7    <-  (On Interrupt: The values before the thread was interrupted) 
        d6    <-  (On Yield: The values on return to SchedulerReschedule)  
        d5    <-  (On Create: The values on entry to StartProc as set by ThreadSetupStack) 
        d4    <- 
        d3    <- 
        d2    <- 
        d1    <- 
        d0    <- 
        
        fpscr <- The floating point FPSCR register
        fpexc <- The floating point FPEXC register (Current StackPointer points to here)
        .
        .
        .
        .
        (Top "Lowest Address" of Stack)
               
        On exit from a standard context switch as performed by SchedulerReschedule the first value (Highest Address) of lr is used by the
        RFE (Return From Exception) instruction to load the pc which also loads the saved cpsr value
        
        On exit from an IRQ or FIQ context switch as performed by SchedulerSwitch the first value (Highest Address) of lr is used by the
        interrupt handler to return from the interrupt

        A standard context switch uses r12 to save the cpsr value (and RFE to restore it). Because the standard context switch is called
        from a routine which will have saved the value of r12 (which is caller save in the ARM ABI) then we do not need to save the original
        value of r12
        
        An IRQ or FIQ context switch uses the SRS (Store Return State) and RFE (Return From Exception) instructions to save and restore the
        cpsr value from the spsr value of either IRQ or FIQ mode
}
var
 Count:LongWord;
 StackPointer:LongWord;
begin
 {}
 Result:=nil;
 
 {Check Stack Base}
 if StackBase = nil then Exit;
 
 {Check Start Proc}
 if not Assigned(StartProc) then Exit;
 
 {Check Return Proc}
 if not Assigned(ReturnProc) then Exit;
 
 {Get Stack Pointer}
 StackPointer:=PtrUInt(StackBase) - 4;
 
 {Ensure Stack Pointer is 8 byte Aligned}
 if (StackPointer and $00000004) <> 0 then
  begin
   StackPointer:=StackPointer - 4;
  end;
 
 {Subtract the context record from the Stack Pointer}
 StackPointer:=StackPointer - (ARMV6_CONTEXT_LENGTH * SizeOf(LongWord));

 {Floating point (fpexc)}
 PLongWord(StackPointer + ((ARMV6_CONTEXT_LENGTH - 50) * SizeOf(LongWord)))^:=ARMv6GetFPEXC;
 
 {Floating point (fpscr)}
 PLongWord(StackPointer + ((ARMV6_CONTEXT_LENGTH - 49) * SizeOf(LongWord)))^:=ARMv6GetFPSCR;
 
 {Registers d0 to d15}
 for Count:=(ARMV6_CONTEXT_LENGTH - 48) to (ARMV6_CONTEXT_LENGTH - 17) do 
  begin
   PLongWord(StackPointer + (Count * SizeOf(LongWord)))^:=0;
  end;
  
 {Parameter passed in r0}
 PLongWord(StackPointer + ((ARMV6_CONTEXT_LENGTH - 16) * SizeOf(LongWord)))^:=LongWord(Parameter);
 
 {Registers r1 to r12}
 for Count:=(ARMV6_CONTEXT_LENGTH - 15) to (ARMV6_CONTEXT_LENGTH - 4) do 
  begin
   PLongWord(StackPointer + (Count * SizeOf(LongWord)))^:=0;
  end;
 
 {Return address (lr)}
 PLongWord(StackPointer + ((ARMV6_CONTEXT_LENGTH - 3) * SizeOf(LongWord)))^:=LongWord(@ReturnProc);

 {Start address (lr/pc)}
 PLongWord(StackPointer + ((ARMV6_CONTEXT_LENGTH - 2) * SizeOf(LongWord)))^:=LongWord(@StartProc);

 {Control bits (cpsr) (SYS mode, IRQ enabled, FIQ enabled, Abort enabled)} 
 PLongWord(StackPointer + ((ARMV6_CONTEXT_LENGTH - 1) * SizeOf(LongWord)))^:=ARM_MODE_SYS;
 
 {Return top "Lowest Address" of stack}
 Result:=Pointer(StackPointer);
end;

{==============================================================================}
{==============================================================================}
{ARMv6 IRQ Functions}
function ARMv6DispatchIRQ(CPUID:LongWord;Thread:TThreadHandle):TThreadHandle; inline;
begin
 {}
 if Assigned(ARMv6DispatchIRQHandler) then
  begin
   Result:=ARMv6DispatchIRQHandler(CPUID,Thread);
  end
 else
  begin
   Result:=Thread;
  end; 
end;

{==============================================================================}
{==============================================================================}
{ARMv6 FIQ Functions}
function ARMv6DispatchFIQ(CPUID:LongWord;Thread:TThreadHandle):TThreadHandle; inline;
begin
 {}
 if Assigned(ARMv6DispatchFIQHandler) then
  begin
   Result:=ARMv6DispatchFIQHandler(CPUID,Thread);
  end
 else
  begin
   Result:=Thread;
  end;  
end;

{==============================================================================}
{==============================================================================}
{ARMv6 SWI Functions}
function ARMv6DispatchSWI(CPUID:LongWord;Thread:TThreadHandle;Request:PSystemCallRequest):TThreadHandle; inline;
begin
 {}
 if Assigned(ARMv6DispatchSWIHandler) then
  begin
   Result:=ARMv6DispatchSWIHandler(CPUID,Thread,Request);
  end
 else
  begin
   Result:=Thread;
  end;
end;

{==============================================================================}
{==============================================================================}
{ARMv6 Interrupt Functions}
procedure ARMv6ResetHandler; assembler; nostackframe;    
asm
 //For more information see: A2.6 Exceptions in the arm_arm.pdf
 bl ActivityLEDEnable
.LLoop:
 bl ActivityLEDOff
 bl ARMLongWait
 mov r4,#3
.LWait:
 bl ActivityLEDOn
 bl ARMWait
 bl ActivityLEDOff
 bl ARMWait
 sub r4,#1
 cmp r4,#0
 bne .LWait
 b .LLoop
end;

{==============================================================================}

procedure ARMv6UndefinedInstructionHandler; assembler; nostackframe;         
{Handle an undefined instruction exception}
{Notes: This routine is registered as the vector for undefined instruction exception in the vector table loaded during startup.}
asm
 //On entry, processor will be in Undefined mode, IRQ will be disabled and SP will point to the Undefined stack
 //See: ???

 //Adjust the Undefined mode link register (LR_und) for the return
 //See: ???
 sub lr, lr, #4

 //Save the Undefined mode link register (LR_und) in R1 for the exception handler
 mov r1, lr

 //Load the Undefined mode link register (LR_und) with the address of the exception handler 
 ldr lr, =HardwareException
 
 //Store Return State (SRSDB) on the SYS mode stack which will be the stack of the interrupted thread
 //This will store the Undefined mode link register (LR_und) and saved program status register (SPSR_und)
 //Which is somewhat equivalent to doing "push {lr, spsr}" if that was a real instruction
 //See: A2.6.14 SRS – Store Return State in the ARM Architecture Reference Manual (arm_arm)
 //Note: This instruction is not yet supported by the FPC compiler
 .long 0xf96d051f  //srsdb #ARM_MODE_SYS! 

 //Change Program State (CPSID) to SYS mode with IRQ still disabled
 //See: A7.1.24 CPS in the ARM Architecture Reference Manual (arm_arm)
 cpsid i, #ARM_MODE_SYS  

 //Load R0 with the exception type
 mov r0, #EXCEPTION_TYPE_UNDEFINED_INSTRUCTION
 
 //Load R2 with the exception frame
 mov r2, #0 //To Do
 
 //Return From Exception (RFEIA) loading PC and CPSR from the SYS mode stack
 //Which is somewhat equivalent to doing "pop {pc, cpsr}" if that was a real instruction
 //See: A2.6.14 RFE – Return From Exception in the ARM Architecture Reference Manual (arm_arm)
 //Note: This instruction is not yet supported by the FPC compiler
 .long 0xf8bd0a00  //rfeia sp!   
 
 //Note: Compiler adds "mov pc, lr" or "bx lr" to the end of this. Should not be an issue because of rfe above
end;

{==============================================================================}

procedure ARMv6SoftwareInterruptHandler; assembler; nostackframe;           
{Handle a software interrupt (SWI) from a system call (SVC)}
{Notes: This routine is registered as the vector for SWI requests in the vector table loaded during startup.

        When the processor executes an SVC it switches to SWI mode, stores the address of the next instruction
        in the SWI mode link register (lr_svc) and saves the current program status register into the SWI mode
        saved program status register (spsr_svc).
        
        The SWI handler first saves the SWI mode lr and spsr (which represent the location and state to return
        to) onto the SYS mode stack using the srsdb (Store Return State Decrement Before) instruction.
        
        The SWI handler then switches to SYS mode and saves all the neccessary registers for the return to the
        interrupted thread before switching back to SWI mode in order to process the software interrupt. Because
        we arrive here from an interrupt the thread that was executing has no opportunity to save registers and
        will be unaware on return that it was interrupted. For this reason we must save all of the general purpose
        registers (r0 to r12) as well as the SYS mode link register (lr). We do not save the stack pointer (r13)
        because we use it to store the other registers and will return it to the correct value before we return
        from the SWI handler. The program counter (r15) does not need to be saved as it now points to this code.
        
        The SystemCall function should pass the parameters of the call as follows:
        
          R0 - System Call Number (eg SYSTEM_CALL_CONTEXT_SWITCH)
          R1 - Parameter 1
          R2 - Parameter 2
          R3 - Parameter 3
          
        To process the software interrupt
        
        ??????
        
        To return from the software interrupt

        ??????
        
}
asm
 //On entry, processor will be in SWI mode, IRQ will be disabled and SP will point to the SWI stack
 //See: ???

 //Do NOT adjust the SWI mode link register (LR_svc) for the return
 //See: ???
 //sub lr, lr, #4
 
 //Store Return State (SRSDB) on the SYS mode stack which will be the stack of the interrupted thread
 //This will store the SWI mode link register (LR_svc) and saved program status register (SPSR_svc)
 //Which is somewhat equivalent to doing "push {lr, spsr}" if that was a real instruction
 //See: A2.6.14 SRS – Store Return State in the ARM Architecture Reference Manual (arm_arm)
 //Note: This instruction is not yet supported by the FPC compiler
 .long 0xf96d051f  //srsdb #ARM_MODE_SYS! 

 //Change Program State (CPSID) to SYS mode with IRQ still disabled
 //See: A7.1.24 CPS in the ARM Architecture Reference Manual (arm_arm)
 cpsid i, #ARM_MODE_SYS  

 //Save all of the general registers (R0 to R12) and the SYS mode link register (LR) 
 push {r0-r12, lr}
  
 //Change Program State (CPS) to SWI mode (IRQ will remain disabled)
 //The SWI mode stack will have been set by initialization routines
 //We use this stack to process the SWI not the SWI thread stack
 cps #ARM_MODE_SVC
  
 //Get the current CPU (Always assume CPU 0 for ARMv6)
 mov r4, #0
 //Multiply by 4 to get the offset into the array
 lsl r6, r4, #2
 
 //Get the SWI thread id
 ldr r5, .LSWI_THREAD_HANDLE
 ldr r5, [r5]
 ldr r5, [r5, r6]
 
 //Save the current thread id from c13 (Thread and process ID) register of system control coprocessor CP15
 mrc p15, #0, r6, cr13, cr0, #4
 
 //Load the SWI thread id into c13 (Thread and process ID) register of system control coprocessor CP15
 mcr p15, #0, r5, cr13, cr0, #4
 
 //Align the SWI mode stack pointer (SP) to an 8 byte boundary for external calls
 //See: Procedure Call Standard for the ARM Architecture
 and r7, sp, #4
 sub sp, sp, r7
 
 //Save value of R7 (Stack Alignment) on the SWI mode stack for return from external call
 //Also save R6 (Current Thread Id) to maintain the 8 byte alignment
 push {r6, r7}
 
 //Execute a data memory barrier 
 //ARMv6 "data memory barrier" instruction.
 mov r12, #0
 mcr p15, #0, r12, cr7, cr10, #5
 
 //Determine the System Call number passed in R0
 //Check for SYSTEM_CALL_CONTEXT_SWITCH
 cmp r0, #SYSTEM_CALL_CONTEXT_SWITCH
 bne .LSystemCallInvalid
 //Perform a System Call Context Switch (Caller will have disabled IRQ/FIQ as appropriate to prevent rescheduling)
 //Passing R0 = OldStack (Parameter 1 in R1) / R1 = NewStack (Parameter 2 in R2) / R2 = NewThread (Parameter 3 in R3)
 mov r0, r1
 mov r1, r2
 mov r2, r3
 bl ARMv6ContextSwitchSWI
 //Put the New Thread Id in R0 for restore (ARMv7ContextSwitchSWI does not modify R2)
 mov r0, r2
 //System Call completed
 b .LSystemCallCompleted
 
.LSystemCallInvalid:
 //Put the Current Thread Id in R0 for restore
 mov r0, r6
 
.LSystemCallCompleted:
 //Execute a data memory barrier
 //ARMv6 "data memory barrier" instruction.
 mov r12, #0
 mcr p15, #0, r12, cr7, cr10, #5
 
 //Restore value of R7 (Stack Alignment) from the SWI mode stack after return from external call
 //Also restore R6 (Current Thread Id) to maintain the 8 byte alignment
 pop {r6, r7}
  
 //Restore the SWI mode stack pointer alignment 
 add sp, sp, r7
 
 //Load the current thread id into c13 (Thread and process ID) register of system control coprocessor CP15
 //Must be saved in R0 by any of the system call operations above
 mcr p15, #0, r0, cr13, cr0, #4
 
 //Change Program State (CPS) to SYS mode  (IRQ will remain disabled)
 //If a context switch occurred then the SYS mode stack will have been swapped
 cps #ARM_MODE_SYS
 
 //Restore all of the general registers (R0 to R12) and the SYS mode link register (LR) 
 pop {r0-r12, lr}
 
 //Return From Exception (RFEIA) loading PC and CPSR from the SYS mode stack
 //Which is somewhat equivalent to doing "pop {pc, cpsr}" if that was a real instruction
 //See: A2.6.14 RFE – Return From Exception in the ARM Architecture Reference Manual (arm_arm)
 //Note: This instruction is not yet supported by the FPC compiler
 .long 0xf8bd0a00  //rfeia sp!   
 
.LSWI_THREAD_HANDLE:   
  .long SWI_THREAD_HANDLE

 //Note: Compiler adds "mov pc, lr" or "bx lr" to the end of this. Should not be an issue because of rfe above
end;

{==============================================================================}

procedure ARMv6PrefetchAbortHandler; assembler; nostackframe;      
{Handle a prefetch abort exception}
{Notes: This routine is registered as the vector for prefetch abort exception in the vector table loaded during startup.}
asm
 //On entry, processor will be in Abort mode, IRQ will be disabled and SP will point to the Abort stack
 //See: ???

 //Adjust the Abort mode link register (LR_abt) for the return
 //See: ???
 sub lr, lr, #4

 //Save the Abort mode link register (LR_abt) in R1 for the exception handler
 mov r1, lr

 //Load the Abort mode link register (LR_abt) with the address of the exception handler 
 ldr lr, =HardwareException
 
 //Store Return State (SRSDB) on the SYS mode stack which will be the stack of the interrupted thread
 //This will store the Abort mode link register (LR_abt) and saved program status register (SPSR_abt)
 //Which is somewhat equivalent to doing "push {lr, spsr}" if that was a real instruction
 //See: A2.6.14 SRS – Store Return State in the ARM Architecture Reference Manual (arm_arm)
 //Note: This instruction is not yet supported by the FPC compiler
 .long 0xf96d051f  //srsdb #ARM_MODE_SYS! 

 //Change Program State (CPSID) to SYS mode with IRQ still disabled
 //See: A7.1.24 CPS in the ARM Architecture Reference Manual (arm_arm)
 cpsid i, #ARM_MODE_SYS  

 //Load R0 with the exception type
 mov r0, #EXCEPTION_TYPE_PREFETCH_ABORT
 
 //Load R2 with the exception frame
 mov r2, #0 //To Do
 
 //Return From Exception (RFEIA) loading PC and CPSR from the SYS mode stack
 //Which is somewhat equivalent to doing "pop {pc, cpsr}" if that was a real instruction
 //See: A2.6.14 RFE – Return From Exception in the ARM Architecture Reference Manual (arm_arm)
 //Note: This instruction is not yet supported by the FPC compiler
 .long 0xf8bd0a00  //rfeia sp!   
 
 //Note: Compiler adds "mov pc, lr" or "bx lr" to the end of this. Should not be an issue because of rfe above
end;

{==============================================================================}

procedure ARMv6DataAbortHandler; assembler; nostackframe;         
{Handle a data abort exception}
{Notes: This routine is registered as the vector for data abort exception in the vector table loaded during startup.}
asm
 //On entry, processor will be in Abort mode, IRQ will be disabled and SP will point to the Abort stack
 //See: ???

 //Adjust the Abort mode link register (LR_abt) for the return
 //See: ???
 sub lr, lr, #8
 
 //Save the Abort mode link register (LR_abt) in R1 for the exception handler
 mov r1, lr

 //Load the Abort mode link register (LR_abt) with the address of the exception handler 
 ldr lr, =HardwareException
 
 //Store Return State (SRSDB) on the SYS mode stack which will be the stack of the interrupted thread
 //This will store the Abort mode link register (LR_abt) and saved program status register (SPSR_abt)
 //Which is somewhat equivalent to doing "push {lr, spsr}" if that was a real instruction
 //See: A2.6.14 SRS – Store Return State in the ARM Architecture Reference Manual (arm_arm)
 //Note: This instruction is not yet supported by the FPC compiler
 .long 0xf96d051f  //srsdb #ARM_MODE_SYS! 

 //Change Program State (CPSID) to SYS mode with IRQ still disabled
 //See: A7.1.24 CPS in the ARM Architecture Reference Manual (arm_arm)
 cpsid i, #ARM_MODE_SYS  

 //Load R0 with the exception type
 mov r0, #EXCEPTION_TYPE_DATA_ABORT
 
 //Load R2 with the exception frame
 mov r2, #0 //To Do
 
 //Return From Exception (RFEIA) loading PC and CPSR from the SYS mode stack
 //Which is somewhat equivalent to doing "pop {pc, cpsr}" if that was a real instruction
 //See: A2.6.14 RFE – Return From Exception in the ARM Architecture Reference Manual (arm_arm)
 //Note: This instruction is not yet supported by the FPC compiler
 .long 0xf8bd0a00  //rfeia sp! 
 
 //Note: Compiler adds "mov pc, lr" or "bx lr" to the end of this. Should not be an issue because of rfe above
end;

{==============================================================================}

procedure ARMv6ReservedHandler; assembler; nostackframe;      
asm
 //For more information see: A2.6 Exceptions in the arm_arm.pdf
 bl ActivityLEDEnable
.LLoop:
 bl ActivityLEDOff
 bl ARMLongWait
 mov r4,#8
.LWait:
 bl ActivityLEDOn
 bl ARMWait
 bl ActivityLEDOff
 bl ARMWait
 sub r4,#1
 cmp r4,#0
 bne .LWait
 b .LLoop
end;

{==============================================================================}

procedure ARMv6IRQHandler; assembler; nostackframe;
{Handle an interrupt request IRQ from an interrupt source}
{Notes: This routine is registered as the vector for IRQ requests in the vector table loaded during startup.
        
        At the end of each instruction the processor checks the IRQ line and if triggered it will lookup the
        vector in the vector table and jump to the routine listed.
        
        When the processor receives an IRQ it switches to IRQ mode, stores the address of the next instruction
        in the IRQ mode link register (lr_irq) and saves the current program status register into the IRQ mode
        saved program status register (spsr_irq).
        
        The IRQ handler first saves the IRQ mode lr and spsr (which represent the location and state to return
        to) onto the SYS mode stack using the srsdb (Store Return State Decrement Before) instruction.
        
        The IRQ handler then switches to SYS mode and saves all the neccessary registers for the return to the
        interrupted thread before switching back to IRQ mode in order to process the interrupt request. Because
        we arrive here from an interrupt the thread that was executing has no opportunity to save registers and
        will be unaware on return that it was interrupted. For this reason we must save all of the general purpose
        registers (r0 to r12) as well as the SYS mode link register (lr). We do not save the stack pointer (r13)
        because we use it to store the other registers and will return it to the correct value before we return
        from the IRQ handler. The program counter (r15) does not need to be saved as it now points to this code.
        
        To process the interrupt request the handler calls the DispatchIRQ function which will dispatch the
        interrupt to a registered handler for processing. The handler must clear the interrupt source before it
        returns or the interrupt will simply occur again immediately once reenabled.
        
        To return from the interrupt request the handler uses the rfeia (Return From Exception Increment After)
        instruction which will load the pc and cpsr from the SYS mode stack
        
}
asm
 //On entry, processor will be in IRQ mode, IRQ will be disabled and SP will point to the IRQ stack
 //See: A2.6.8 Interrupt request (IRQ) exception in the ARM Architecture Reference Manual (arm_arm)
 
 //Adjust the IRQ mode link register (LR_irq) for the return
 //See: A2.6.8 Interrupt request (IRQ) exception in the ARM Architecture Reference Manual (arm_arm)
 sub lr, lr, #4

 //Save r8 so we can us it to examine the spsr of the interrupted task to determine whether we are
 //interrupting a normal thread or if we are interrupting an IRQ or other exception
 push {r8}
 mrs  r8, spsr
 and  r8, r8, #ARM_MODE_BITS
 
 //Check for SYS mode
 cmp  r8, #ARM_MODE_SYS
 bne  .LOtherIRQ
 
.LThreadIRQ:  
 //Interrupted a normal thread
 //Restore r8 from above
 pop {r8}
 
 //Store Return State (SRSDB) on the SYS mode stack which will be the stack of the interrupted thread
 //This will store the IRQ mode link register (LR_irq) and saved program status register (SPSR_irq)
 //Which is somewhat equivalent to doing "push {lr, spsr}" if that was a real instruction
 //See: A2.6.14 SRS – Store Return State in the ARM Architecture Reference Manual (arm_arm)
 //Note: This instruction is not yet supported by the FPC compiler
 .long 0xf96d051f  //srsdb #ARM_MODE_SYS! 

 //Change Program State (CPSID) to SYS mode with IRQ still disabled
 //See: A7.1.24 CPS in the ARM Architecture Reference Manual (arm_arm)
 cpsid i, #ARM_MODE_SYS  

 //Save all of the general registers (R0 to R12) and the SYS mode link register (LR) 
 push {r0-r12, lr}
                       
 //Change Program State (CPS) to IRQ mode (IRQ will remain disabled)
 //The IRQ mode stack will have been set by initialization routines
 //We use this stack to process the IRQ not the IRQ thread stack
 cps #ARM_MODE_IRQ
 
 //Get the current CPU (Always assume CPU 0 for ARMv6)
 mov r0, #0
 //Multiply by 4 to get the offset into the array
 lsl r2, r0, #2
 
 //Get the IRQ thread id
 ldr r1, .LIRQ_THREAD_HANDLE
 ldr r1, [r1]
 ldr r1, [r1, r2]
 
 //Save the current thread id from c13 (Thread and process ID) register of system control coprocessor CP15  
 mrc p15, #0, r4, cr13, cr0, #4
 
 //Load the IRQ thread id into c13 (Thread and process ID) register of system control coprocessor CP15  
 mcr p15, #0, r1, cr13, cr0, #4

 //Align the IRQ mode stack pointer (SP) to an 8 byte boundary for external calls
 //See: Procedure Call Standard for the ARM Architecture
 and r3, sp, #4
 sub sp, sp, r3
 
 //Save value of R3 (Stack Alignment) on the IRQ mode stack for return from ARMv6DispatchIRQ
 //Also save R4 (Current Thread Id) to maintain the 8 byte alignment
 push {r3, r4}
  
 //Execute a data memory barrier 
 //ARMv6 "data memory barrier" instruction.
 mov r12, #0
 mcr p15, #0, r12, cr7, cr10, #5

 //Call DispatchIRQ passing CPU in R0 and Thread Id in R1 (Thread Id of the interrupted thread)
 //DispatchIRQ will return Thread Id in R0 which may be different if a context switch occurred
 mov r1, r4
 bl ARMv6DispatchIRQ
 
 //Execute a data memory barrier
 //ARMv6 "data memory barrier" instruction.
 mov r12, #0
 mcr p15, #0, r12, cr7, cr10, #5
 
 //Restore value of R3 (Stack Alignment) from the IRQ mode stack after return from ARMv6DispatchIRQ
 //Also restore R4 (Current Thread Id) to maintain the 8 byte alignment
 pop {r3, r4}
  
 //Restore the IRQ mode stack pointer alignment 
 add sp, sp, r3
 
 //Load the current thread id into c13 (Thread and process ID) register of system control coprocessor CP15  
 //Returned from ARMv6DispatchIRQ in R0 and may be different if a context switch occurred 
 mcr p15, #0, r0, cr13, cr0, #4

 //Change Program State (CPS) to SYS mode (IRQ will remain disabled)
 //If a context switch occurred then the SYS mode stack will have been swapped
 cps #ARM_MODE_SYS
 
 //Restore all of the general registers (R0 to R12) and the SYS mode link register (LR) 
 pop {r0-r12, lr}
 
 //Return From Exception (RFEIA) loading PC and CPSR from the SYS mode stack
 //Which is somewhat equivalent to doing "pop {pc, cpsr}" if that was a real instruction
 //See: A2.6.14 RFE – Return From Exception in the ARM Architecture Reference Manual (arm_arm)
 //Note: This instruction is not yet supported by the FPC compiler
 .long 0xf8bd0a00  //rfeia sp!   
 
.LOtherIRQ: 
 //Interrupted an IRQ or other exception 
 //Restore r8 from above
 pop {r8}
 
 //Store Return State (SRSDB) on the SVC mode stack which will be the stack of the interrupted thread
 //This will store the IRQ mode link register (LR_irq) and saved program status register (SPSR_irq)
 //Which is somewhat equivalent to doing "push {lr, spsr}" if that was a real instruction
 //See: A2.6.14 SRS – Store Return State in the ARM Architecture Reference Manual (arm_arm)
 //Note: This instruction is not yet supported by the FPC compiler
 .long 0xf96d0513  //srsdb #ARM_MODE_SVC! 
 
 //Change Program State (CPSID) to SVC mode with IRQ still disabled
 //See: A7.1.24 CPS in the ARM Architecture Reference Manual (arm_arm)
 cpsid i, #ARM_MODE_SVC  
 
 //Save all of the general registers (R0 to R12) and the SVC mode link register (LR) 
 push {r0-r12, lr}
 
 //Get the current CPU (Always assume CPU 0 for ARMv6)
 mov r0, #0
 //Multiply by 4 to get the offset into the array
 lsl r2, r0, #2
 
 //Get the IRQ thread id
 ldr r1, .LIRQ_THREAD_HANDLE
 ldr r1, [r1]
 ldr r1, [r1, r2]
 
 //Save the current thread id from c13 (Thread and process ID) register of system control coprocessor CP15  
 mrc p15, #0, r4, cr13, cr0, #4
 
 //Load the IRQ thread id into c13 (Thread and process ID) register of system control coprocessor CP15  
 mcr p15, #0, r1, cr13, cr0, #4
 
 //Align the SVC mode stack pointer (SP) to an 8 byte boundary for external calls
 //See: Procedure Call Standard for the ARM Architecture
 and r3, sp, #4
 sub sp, sp, r3
 
 //Save value of R3 (Stack Alignment) on the SVC mode stack for return from ARMv6DispatchIRQ
 //Also save R4 (Current Thread Id) to maintain the 8 byte alignment
 push {r3, r4}
  
 //Execute a data memory barrier 
 //ARMv6 "data memory barrier" instruction.
 mov r12, #0
 mcr p15, #0, r12, cr7, cr10, #5

 //Call DispatchIRQ passing CPU in R0 and INVALID_HANDLE_VALUE in R1 (No interrupted thread)
 //DispatchIRQ will return INVALID_HANDLE_VALUE in R0 and no context switch will occur
 ldr r1, =INVALID_HANDLE_VALUE
 bl ARMv6DispatchIRQ
 
 //Execute a data memory barrier
 //ARMv6 "data memory barrier" instruction.
 mov r12, #0
 mcr p15, #0, r12, cr7, cr10, #5
 
 //Restore value of R3 (Stack Alignment) from the SVC mode stack after return from ARMv6DispatchIRQ
 //Also restore R4 (Current Thread Id) to maintain the 8 byte alignment
 pop {r3, r4}
 
 //Restore the SVC mode stack pointer alignment 
 add sp, sp, r3
 
 //Load the current thread id from R4 into c13 (Thread and process ID) register of system control coprocessor CP15
 mcr p15, #0, r4, cr13, cr0, #4
 
 //Restore all of the general registers (R0 to R12) and the SVC mode link register (LR) 
 pop {r0-r12, lr}
 
 //Return From Exception (RFEIA) loading PC and CPSR from the SVC mode stack
 //Which is somewhat equivalent to doing "pop {pc, cpsr}" if that was a real instruction
 //See: A2.6.14 RFE – Return From Exception in the ARM Architecture Reference Manual (arm_arm)
 //Note: This instruction is not yet supported by the FPC compiler
 .long 0xf8bd0a00  //rfeia sp!   
 
.LIRQ_THREAD_HANDLE:   
  .long IRQ_THREAD_HANDLE

 //Note: Compiler adds "mov pc, lr" or "bx lr" to the end of this. Should not be an issue because of rfe above
end;

{==============================================================================}

procedure ARMv6FIQHandler; assembler; nostackframe;    
{Handle a fast interrupt request FIQ from an interrupt source}
{Notes: This routine is registered as the vector for FIQ requests in the vector table loaded during startup.
        
        At the end of each instruction the processor checks the FIQ line and if triggered it will lookup the
        vector in the vector table and jump to the routine listed.
        
        When the processor receives an FIQ it switches to FIQ mode, stores the address of the next instruction
        in the FIQ mode link register (lr_fiq) and saves the current program status register into the FIQ mode
        saved program status register (spsr_fiq).
        
        The FIQ handler first checks the spsr to determine if the task being interrupted is a normal thread or
        an exception or interrupt handler. 
        
        The FIQ handler then saves the FIQ mode lr and spsr (which represent the location and state to return
        to) onto eihter the SYS mode or SVC mode stack using the srsdb (Store Return State Decrement Before)
        instruction depending on the value of spsr.
        
        The FIQ handler switches to SYS or SVC mode and saves all the neccessary registers for the return to the
        interrupted task before switching back to FIQ mode in order to process the interrupt request. Because
        we arrive here from an interrupt the task that was executing has no opportunity to save registers and
        will be unaware on return that it was interrupted. For this reason we must save all of the general purpose
        registers (r0 to r12) as well as the SYS mode link register (lr). We do not save the stack pointer (r13)
        because we use it to store the other registers and will return it to the correct value before we return
        from the FIQ handler. The program counter (r15) does not need to be saved as it now points to this code.
        
        To process the fast interrupt request the handler calls the DispatchFIQ function which will dispatch the
        interrupt to a registered handler for processing. The handler must clear the interrupt source before it
        returns or the fast interrupt will simply occur again immediately once reenabled.
        
        To return from the fast interrupt request the handler uses the rfeia (Return From Exception Increment After)
        instruction which will load the pc and cpsr from the stack of the current mode (SYS or SVC)
        
}
asm
 //On entry, processor will be in FIQ mode, IRQ and FIQ will be disabled and SP will point to the FIQ stack
 //See: A2.6.9 Fast interrupt request (FIQ) exception in the ARM Architecture Reference Manual (arm_arm)
 
 //Adjust the FIQ mode link register (LR_fiq) for the return
 //See: A2.6.9 Fast interrupt request (FIQ) exception in the ARM Architecture Reference Manual (arm_arm)
 sub  lr, lr, #4
 
 //Because FIQ mode has a banked set of registers that includes r8 to r12 we can use one of these to examine
 //the spsr of the interrupted task to determine whether we are interrupting a normal thread or if we are
 //interrupting an IRQ or other exception
 mrs  r8, spsr
 and  r8, r8, #ARM_MODE_BITS
 
 //Check for SYS mode
 cmp  r8, #ARM_MODE_SYS
 bne  .LOtherFIQ
 
.LThreadFIQ:  
 //Interrupted a normal thread
 //Store Return State (SRSDB) on the SYS mode stack which will be the stack of the interrupted thread
 //This will store the FIQ mode link register (LR_fiq) and saved program status register (SPSR_fiq)
 //Which is somewhat equivalent to doing "push {lr, spsr}" if that was a real instruction
 //See: A2.6.14 SRS – Store Return State in the ARM Architecture Reference Manual (arm_arm)
 //Note: This instruction is not yet supported by the FPC compiler
 .long 0xf96d051f  //srsdb #ARM_MODE_SYS! 

 //Change Program State (CPSID) to SYS mode with IRQ and FIQ still disabled
 //See: A7.1.24 CPS in the ARM Architecture Reference Manual (arm_arm)
 cpsid if, #ARM_MODE_SYS  

 //Save all of the general registers (R0 to R12) and the SYS mode link register (LR) 
 push {r0-r12, lr}

 //Change Program State (CPS) to FIQ mode (IRQ and FIQ will remain disabled)
 //The FIQ mode stack will have been set by initialization routines
 //We use this stack to process the FIQ not the FIQ thread stack
 cps #ARM_MODE_FIQ

 //Get the current CPU (Always assume CPU 0 for ARMv6)
 mov r0, #0
 //Multiply by 4 to get the offset into the array
 lsl r2, r0, #2
 
 //Get the FIQ thread id
 ldr r1, .LFIQ_THREAD_HANDLE
 ldr r1, [r1]
 ldr r1, [r1, r2]
 
 //Save the current thread id from c13 (Thread and process ID) register of system control coprocessor CP15  
 mrc p15, #0, r4, cr13, cr0, #4
 
 //Load the FIQ thread id into c13 (Thread and process ID) register of system control coprocessor CP15  
 mcr p15, #0, r1, cr13, cr0, #4

 //Align the FIQ mode stack pointer (SP) to an 8 byte boundary for external calls
 //See: Procedure Call Standard for the ARM Architecture
 and r3, sp, #4
 sub sp, sp, r3
 
 //Save value of R3 (Stack Alignment) on the FIQ mode stack for return from ARMv6DispatchFIQ
 //Also save R4 (Current Thread Id) to maintain the 8 byte alignment
 push {r3, r4}
  
 //Execute a data memory barrier 
 //ARMv6 "data memory barrier" instruction.
 mov r12, #0
 mcr p15, #0, r12, cr7, cr10, #5

 //Call DispatchFIQ passing CPU in R0 and Thread Id in R1 (Thread Id of the interrupted thread)
 //DispatchFIQ will return Thread Id in R0 which may be different if a context switch occurred
 mov r1, r4
 bl ARMv6DispatchFIQ
 
 //Execute a data memory barrier
 //ARMv6 "data memory barrier" instruction.
 mov r12, #0
 mcr p15, #0, r12, cr7, cr10, #5
 
 //Restore value of R3 (Stack Alignment) from the FIQ mode stack after return from ARMv6DispatchFIQ
 //Also restore R4 (Current Thread Id) to maintain the 8 byte alignment
 pop {r3, r4}
  
 //Restore the FIQ mode stack pointer alignment 
 add sp, sp, r3
 
 //Load the current thread id into c13 (Thread and process ID) register of system control coprocessor CP15  
 //Returned from ARMv6DispatchFIQ in R0 and may be different if a context switch occurred 
 mcr p15, #0, r0, cr13, cr0, #4

 //Change Program State (CPS) to SYS mode (IRQ and FIQ will remain disabled)
 //If a context switch occurred then the SYS mode stack will have been swapped
 cps #ARM_MODE_SYS
 
 //Restore all of the general registers (R0 to R12) and the SYS mode link register (LR) 
 pop {r0-r12, lr}
 
 //Return From Exception (RFEIA) loading PC and CPSR from the SYS mode stack
 //Which is somewhat equivalent to doing "pop {pc, cpsr}" if that was a real instruction
 //See: A2.6.14 RFE – Return From Exception in the ARM Architecture Reference Manual (arm_arm)
 //Note: This instruction is not yet supported by the FPC compiler
 .long 0xf8bd0a00  //rfeia sp!   

.LOtherFIQ: 
 //Interrupted an IRQ or other exception
 //Store Return State (SRSDB) on the SVC mode stack which will be the stack we use for the handler
 //This will store the FIQ mode link register (LR_fiq) and saved program status register (SPSR_fiq)
 //Which is somewhat equivalent to doing "push {lr, spsr}" if that was a real instruction
 //See: A2.6.14 SRS – Store Return State in the ARM Architecture Reference Manual (arm_arm)
 //Note: This instruction is not yet supported by the FPC compiler
 .long 0xf96d0513  //srsdb #ARM_MODE_SVC! 
 
 //Change Program State (CPSID) to SVC mode with IRQ and FIQ still disabled
 //See: A7.1.24 CPS in the ARM Architecture Reference Manual (arm_arm)
 cpsid if, #ARM_MODE_SVC  

 //Save all of the general registers (R0 to R12) and the SVC mode link register (LR) 
 push {r0-r12, lr}
 
 //Get the current CPU (Always assume CPU 0 for ARMv6)
 mov r0, #0
 //Multiply by 4 to get the offset into the array
 lsl r2, r0, #2
 
 //Get the FIQ thread id
 ldr r1, .LFIQ_THREAD_HANDLE
 ldr r1, [r1]
 ldr r1, [r1, r2]
 
 //Save the current thread id from c13 (Thread and process ID) register of system control coprocessor CP15
 mrc p15, #0, r4, cr13, cr0, #4
 
 //Load the FIQ thread id into c13 (Thread and process ID) register of system control coprocessor CP15
 mcr p15, #0, r1, cr13, cr0, #4

 //Align the SVC mode stack pointer (SP) to an 8 byte boundary for external calls
 //See: Procedure Call Standard for the ARM Architecture
 and r3, sp, #4
 sub sp, sp, r3
 
 //Save value of R3 (Stack Alignment) on the SVC mode stack for return from ARMv6DispatchFIQ
 //Also save R4 (Current Thread Id) to maintain the 8 byte alignment
 push {r3, r4}
  
 //Execute a data memory barrier 
 //ARMv6 "data memory barrier" instruction.
 mov r12, #0
 mcr p15, #0, r12, cr7, cr10, #5

 //Call DispatchFIQ passing CPU in R0 and INVALID_HANDLE_VALUE in R1 (No interrupted thread)
 //DispatchFIQ will return INVALID_HANDLE_VALUE in R0 and no context switch will occur
 ldr r1, =INVALID_HANDLE_VALUE
 bl ARMv6DispatchFIQ
 
 //Execute a data memory barrier
 //ARMv6 "data memory barrier" instruction.
 mov r12, #0
 mcr p15, #0, r12, cr7, cr10, #5
 
 //Restore value of R3 (Stack Alignment) from the SVC mode stack after return from ARMv6DispatchFIQ
 //Also restore R4 (Current Thread Id) to maintain the 8 byte alignment
 pop {r3, r4}
  
 //Restore the SVC mode stack pointer alignment 
 add sp, sp, r3
 
 //Load the current thread id from R4 into c13 (Thread and process ID) register of system control coprocessor CP15
 mcr p15, #0, r4, cr13, cr0, #4
 
 //Restore all of the general registers (R0 to R12) and the SVC mode link register (LR) 
 pop {r0-r12, lr}
 
 //Return From Exception (RFEIA) loading PC and CPSR from the SVC mode stack
 //Which is somewhat equivalent to doing "pop {pc, cpsr}" if that was a real instruction
 //See: A2.6.14 RFE – Return From Exception in the ARM Architecture Reference Manual (arm_arm)
 //Note: This instruction is not yet supported by the FPC compiler
 .long 0xf8bd0a00  //rfeia sp!   
 
.LFIQ_THREAD_HANDLE:   
  .long FIQ_THREAD_HANDLE

 //Note: Compiler adds "mov pc, lr" or "bx lr" to the end of this. Should not be an issue because of rfe above
end;

{==============================================================================}
{==============================================================================}
{ARMv6 Helper Functions}
function ARMv6GetFPEXC:LongWord; assembler; nostackframe; 
asm
 //Get the FPEXC register from the VFP unit
 fmrx r0, fpexc
end;

{==============================================================================}

function ARMv6GetFPSCR:LongWord; assembler; nostackframe; 
asm
 //Get the FPSCR register from the VFP unit
 fmrx r0, fpscr
end;

{==============================================================================}

procedure ARMv6StartMMU; assembler; nostackframe; 
asm
 //Disable the L1 Data and Instruction Cache before enabling the MMU by clearing the I and C bits in the C1 control register.
 //Also ensure the MMU is disabled by clearing the M bit in the C1 control register.
 //See pages 3-46 and 6-9 of the ARM1176JZF-S Technical Reference Manual.
 mrc p15, #0, r12, cr1, cr0, #0;
 bic r12, r12, #ARMV6_CP15_C1_I_BIT
 bic r12, r12, #ARMV6_CP15_C1_C_BIT
 bic r12, r12, #ARMV6_CP15_C1_M_BIT
 mcr p15, #0, r12, cr1, cr0, #0;
  
 //Perform an Instruction Memory Barrier (IMB) operation immediately after the change above.
 //The ARM1176JZF-S Technical Reference Manual states on page 5-10 (section 5.5) that a Flush Prefetch Buffer operation also acts as an IMB.
 //Perform a Flush Prefetch Buffer operation.
 //See page 3-79 of the ARM1176JZF-S Technical Reference Manual.
 mov r12, #0
 mcr p15, #0, r12, cr7, cr5, #4
  
 //Invalidate the L1 Data and Instruction Caches before enabling the MMU
 //See page 3-74 and 6-9 of the ARM1176JZF-S Technical Reference Manual.
 //mov r12, #0
 //mcr p15, #0, r12, cr7, cr7, #0
 
 //Invalidate the Transaction Lookaside Buffers (TLB) before enabling the MMU
 //See page 3-86 and 6-9 of the ARM1176JZF-S Technical Reference Manual.
 mov r12, #0
 mcr p15, #0, r12, cr8, cr7, #0
 
 //Perform a data synchronization barrier operation using the c7 (Cache Operations) register of system control coprocessor CP15
 //See page 3-74 of the ARM1176JZF-S Technical Reference Manual
 mov r12, #0
 mcr p15, #0, r12, cr7, cr10, #4
 
 //Set the access for Domain 0 to Client in the C3 domain access control register.
 //See page 3-63 of the ARM1176JZF-S Technical Reference Manual.
 mov r12, #ARMV6_CP15_C3_DOMAIN0_CLIENT 
 mcr p15, #0, r12, cr3, cr0, #0
 
 //Set the Page Table base address in the C2 translation table base register 0
 //Only bits 31 to 14 are written to the register
 //The alignment of the Translation Table Base Register 0 depends on the value
 //of N in the C2 translation table base control register.
 //See page 3-58 of the ARM1176JZF-S Technical Reference Manual.
 ldr r12, .LPAGE_TABLE_BASE
 ldr r12, [r12]
 orr r12, r12, #ARMV6_CP15_C2_TTBR_C_INNER_CACHED     
 orr r12, r12, #ARMV6_CP15_C2_TTBR_RGN_OUTER_WRITE_BACK
 mcr p15, #0, r12, cr2, cr0, #0
  
 //Set the Page Table base address in the C2 translation table base register 1
 //Only bits 31 to 14 are written to the register 
 //Translation Table Base Register 1 must reside on a 16KB page boundary
 //See page 3-59 of the ARM1176JZF-S Technical Reference Manual.
 mcr p15, #0, r12, cr2, cr0, #1
  
 //Perform a Instruction Memory Barrier (IMB) operation to ensure 
 //all of the above is completed before enabling the MMU and Caches
 //The ARM1176JZF-S Technical Reference Manual states on page 5-10 (section 5.5) that a Flush Prefetch Buffer operation also acts as an IMB.
 //Perform a Flush Prefetch Buffer operation.
 //See page 3-79 of the ARM1176JZF-S Technical Reference Manual.
 mov r12, #0
 mcr p15, #0, r12, cr7, cr5, #4
  
 //Enable the Memory Management Unit and L1 Instruction Cache by setting the I, M and XP bits in the C1 control register.
 //See page 3-46 and 3-47 of the ARM1176JZF-S Technical Reference Manual.
 mrc p15, #0, r12, cr1, cr0, #0;
 orr r12, r12, #ARMV6_CP15_C1_I_BIT 
 orr r12, r12, #ARMV6_CP15_C1_C_BIT
 orr r12, r12, #ARMV6_CP15_C1_M_BIT
 orr r12, r12, #ARMV6_CP15_C1_XP_BIT
 mcr p15, #0, r12, cr1, cr0, #0;
 
 //Perform an Instruction Memory Barrier (IMB) operation immediately after the change above.
 //The ARM1176JZF-S Technical Reference Manual states on page 5-10 (section 5.5) that a Flush Prefetch Buffer operation also acts as an IMB.
 //Perform a Flush Prefetch Buffer operation.
 //See page 3-79 of the ARM1176JZF-S Technical Reference Manual.
 mov r12, #0
 mcr p15, #0, r12, cr7, cr5, #4
 
 //Return to caller
 bx lr
 
.LPAGE_TABLE_BASE:
 .long PAGE_TABLE_BASE
end;

{==============================================================================}

function ARMv6GetPageTableCoarse(Address:PtrUInt):LongWord;
{Get the descriptor for a Coarse Page Table entry (1MB)}
{See page 6-39 of the ARM1176JZF-S Technical Reference Manual}
var
 TableBase:LongWord;
 TableOffset:LongWord;
 
 CoarseEntry:LongWord;
begin
 {}
 Result:=0;
 
 {Check Page Table}
 if PAGE_TABLE_SIZE < SIZE_16K then Exit; {Level 1 Page Table is 16KB}
 if Pointer(PAGE_TABLE_BASE) = nil then Exit;
 
 {Get Table Base}
 TableBase:=(Address and ARMV6_L1D_SECTION_BASE_MASK);
 
 {Get Table Offset}
 TableOffset:=((TableBase shr 20) shl 2); {Divide Base by 1MB then multiply by 4 to get Offset into Page Table}
 
 {Read Page Table}
 CoarseEntry:=PLongWord(PtrUInt(PAGE_TABLE_BASE) + PtrUInt(TableOffset))^;
 
 {Check Level 1 Type}
 if (CoarseEntry and ARMV6_L1D_TYPE_COARSE) <> 0 then
  begin
   {Return Result}
   Result:=CoarseEntry;
  end; 
end;

{==============================================================================}

function ARMv6SetPageTableCoarse(Address,CoarseAddress:PtrUInt;Flags:Word):Boolean;
{Set the descriptor for a Coarse Page Table entry (1MB)}
{See page 6-39 of the ARM1176JZF-S Technical Reference Manual}
{Note: Caller must call ARMv6InvalidateTLB after changes if MMU is enabled}
var
 Count:Integer;

 TableBase:LongWord;
 TableFlags:LongWord;
 TableOffset:LongWord;

 CoarseBase:LongWord;
 CoarseOffset:LongWord;

 CurrentFlags:LongWord;
 CurrentBase:LongWord;
 CurrentEntry:LongWord;
 CurrentOffset:LongWord;
begin
 {}
 Result:=False;
 
 {Check Page Table}
 if PAGE_TABLE_SIZE < SIZE_16K then Exit; {Level 1 Page Table is 16KB}
 if Pointer(PAGE_TABLE_BASE) = nil then Exit;

 {Get Table Base}
 TableBase:=(Address and ARMV6_L1D_SECTION_BASE_MASK);
 if TableBase <> Address then Exit; {Must begin on a 1MB boundary}
 
 {Get Coarse Base}
 CoarseBase:=(CoarseAddress and ARMV6_L1D_COARSE_BASE_MASK);
 if CoarseBase <> CoarseAddress then Exit; {Must begin on a 1KB boundary}
 
 {Get Table Offset}
 TableOffset:=((TableBase shr 20) shl 2); {Divide Base by 1MB then multiply by 4 to get Offset into Page Table}
 
 {Read Page Table}
 CurrentEntry:=PLongWord(PtrUInt(PAGE_TABLE_BASE) + PtrUInt(TableOffset))^;
 
 {Check Level 1 Type}
 if (CurrentEntry and ARMV6_L1D_TYPE_COARSE) <> 0 then
  begin
   {Current entry is a Coarse Page Table}
   CurrentBase:=(CurrentEntry and ARMV6_L1D_COARSE_BASE_MASK);
   CurrentOffset:=0;
   
   {Compare existing base with new base}
   if CurrentBase <> CoarseBase then
    begin
     {Copy existing table to new table}
     for Count:=0 to 255 do
      begin
       PLongWord(PtrUInt(CoarseBase) + PtrUInt(CurrentOffset))^:=PLongWord(PtrUInt(CurrentBase) + PtrUInt(CurrentOffset))^;
       Inc(CurrentOffset,SizeOf(LongWord));
      end;
    end;
  end
 else if (CurrentEntry and ARMV6_L1D_TYPE_SECTION) <> 0 then
  begin
   {Current entry is a Section or Supersection}
   if (CurrentEntry and ARMV6_L1D_FLAG_SUPERSECTION) = 0 then 
    begin
     {Current entry is a Section}
     {Convert to Coarse Page Table}
     CurrentBase:=(CurrentEntry and ARMV6_L1D_SECTION_BASE_MASK);
     CurrentOffset:=0;
     {Convert Flags}
     CurrentFlags:=0;
     {Non Secure}
     if (CurrentEntry and ARMV6_L1D_FLAG_SECTION_NS) <> 0 then
      begin
       Flags:=Flags or ARMV6_L1D_FLAG_COARSE_NS; {Put NS flag on Coarse Page Table}
      end;
     {Not Global} 
     if (CurrentEntry and ARMV6_L1D_FLAG_NOT_GLOBAL) <> 0 then
      begin
       CurrentFlags:=CurrentFlags or ARMV6_L2D_FLAG_NOT_GLOBAL;
      end; 
     {Shared} 
     if (CurrentEntry and ARMV6_L1D_FLAG_SHARED) <> 0 then
      begin
       CurrentFlags:=CurrentFlags or ARMV6_L2D_FLAG_SHARED;
      end; 
     {APX} 
     if (CurrentEntry and ARMV6_L1D_FLAG_APX) <> 0 then
      begin
       CurrentFlags:=CurrentFlags or ARMV6_L2D_FLAG_APX;
      end; 
     {P} 
     if (CurrentEntry and ARMV6_L1D_FLAG_P) <> 0 then
      begin
       Flags:=Flags or ARMV6_L1D_FLAG_P; {Put P flag on Coarse Page Table}
      end; 
     {Execute Never} 
     if (CurrentEntry and ARMV6_L1D_FLAG_XN) <> 0 then
      begin
       CurrentFlags:=CurrentFlags or ARMV6_L2D_FLAG_SMALL_XN;
      end; 
     {Cacheable} 
     if (CurrentEntry and ARMV6_L1D_FLAG_C) <> 0 then
      begin
       CurrentFlags:=CurrentFlags or ARMV6_L2D_FLAG_C;
      end; 
     {Bufferable}
     if (CurrentEntry and ARMV6_L1D_FLAG_B) <> 0 then
      begin
       CurrentFlags:=CurrentFlags or ARMV6_L2D_FLAG_B;
      end; 
     {AP}
     case (CurrentEntry and ARMV6_L1D_AP_MASK) of
      ARMV6_L1D_AP0:CurrentFlags:=CurrentFlags or ARMV6_L2D_AP0;
      ARMV6_L1D_AP1:CurrentFlags:=CurrentFlags or ARMV6_L2D_AP1;
      ARMV6_L1D_AP2:CurrentFlags:=CurrentFlags or ARMV6_L2D_AP2;
      ARMV6_L1D_AP3:CurrentFlags:=CurrentFlags or ARMV6_L2D_AP3;
     end;
     {TEX}
     case (CurrentEntry and ARMV6_L1D_TEX_MASK) of
      ARMV6_L1D_TEX0:CurrentFlags:=CurrentFlags or ARMV6_L2D_SMALL_TEX0;
      ARMV6_L1D_TEX1:CurrentFlags:=CurrentFlags or ARMV6_L2D_SMALL_TEX1;
      ARMV6_L1D_TEX2:CurrentFlags:=CurrentFlags or ARMV6_L2D_SMALL_TEX2;
      ARMV6_L1D_TEX4:CurrentFlags:=CurrentFlags or ARMV6_L2D_SMALL_TEX4;
     end; 
     {Domain} 
     Flags:=Flags or (CurrentEntry and ARMV6_L1D_DOMAIN_MASK); {Add Domain to Coarse Page Table}
      
     {Create 256 Small Page (4KB) entries}
     CoarseOffset:=0;
     for Count:=0 to 255 do
      begin
       PLongWord(PtrUInt(CoarseBase) + PtrUInt(CoarseOffset))^:=(CurrentBase + CurrentOffset) or CurrentFlags or ARMV6_L2D_TYPE_SMALL;
       Inc(CoarseOffset,SizeOf(LongWord));
       Inc(CurrentOffset,SIZE_4K);
      end;
    end
   else
    begin
     {Current entry is a Supersection}
     {Cannot Convert}
     Exit;
    end;    
  end;
 
 {Check Flags}
 TableFlags:=Flags and not(ARMV6_L1D_COARSE_BASE_MASK);
 //To Do
 
 {Write Page Table}
 PLongWord(PtrUInt(PAGE_TABLE_BASE) + PtrUInt(TableOffset))^:=CoarseBase or TableFlags or ARMV6_L1D_TYPE_COARSE;
 
 Result:=True;
end;

{==============================================================================}

function ARMv6GetPageTableLarge(Address:PtrUInt):LongWord;
{Get the descriptor for a Large Page Table entry (64KB)}
{See page 6-40 of the ARM1176JZF-S Technical Reference Manual}
var
 TableBase:LongWord;
 TableOffset:LongWord;

 CoarseBase:LongWord;
 CoarseEntry:LongWord;

 LargeBase:LongWord;
 LargeEntry:LongWord;
 LargeOffset:LongWord;
begin
 {}
 Result:=0;
 
 {Check Page Table}
 if PAGE_TABLE_SIZE < SIZE_16K then Exit; {Level 1 Page Table is 16KB}
 if Pointer(PAGE_TABLE_BASE) = nil then Exit;
 
 {Get Table Base}
 TableBase:=(Address and ARMV6_L1D_SECTION_BASE_MASK);
 
 {Get Table Offset}
 TableOffset:=((TableBase shr 20) shl 2); {Divide Base by 1MB then multiply by 4 to get Offset into Page Table}
 
 {Read Page Table}
 CoarseEntry:=PLongWord(PtrUInt(PAGE_TABLE_BASE) + PtrUInt(TableOffset))^;

 {Check Level 1 Type}
 if (CoarseEntry and ARMV6_L1D_TYPE_COARSE) <> 0 then
  begin
   {Get Coarse Base}
   CoarseBase:=(CoarseEntry and ARMV6_L1D_COARSE_BASE_MASK);
   
   {Get Large Base}
   LargeBase:=(Address and ARMV6_L2D_LARGE_BASE_MASK);
   
   {Get Large Offset}
   LargeOffset:=(((LargeBase shr 16) and $000000FF) shl 2); {Divide Base by 64KB then multiply by 4 to get Offset into Coarse Page Table}
   
   {Read Coarse Page Table}
   LargeEntry:=PLongWord(PtrUInt(CoarseBase) + PtrUInt(LargeOffset))^;
   
   {Check Level 2 Type}
   if (LargeEntry and ARMV6_L2D_TYPE_LARGE) <> 0 then
    begin
     {Return Result}
     Result:=LargeEntry;
    end;
  end; 
end;

{==============================================================================}

function ARMv6SetPageTableLarge(Address,PhysicalAddress:PtrUInt;Flags:Word):Boolean;
{Set the descriptor for a Large Page Table entry (64KB)}
{Large Page Table descriptors must begin on a 16 longword (64 byte)
 boundary and be repeated for 16 consecutive longwords}
{See page 6-40 of the ARM1176JZF-S Technical Reference Manual}
{Note: Caller must call ARMv6InvalidateTLB after changes if MMU is enabled}
var
 Count:Integer;

 TableBase:LongWord;
 TableOffset:LongWord;

 CoarseBase:LongWord;
 CoarseEntry:LongWord;
 
 LargeBase:LongWord;
 LargeFlags:LongWord;
 LargeOffset:LongWord;
 
 PhysicalBase:LongWord;
begin
 {}
 Result:=False;
 
 {Check Page Table}
 if PAGE_TABLE_SIZE < SIZE_16K then Exit; {Level 1 Page Table is 16KB}
 if Pointer(PAGE_TABLE_BASE) = nil then Exit;
 
 {Get Table Base}
 TableBase:=(Address and ARMV6_L1D_SECTION_BASE_MASK);
 
 {Get Table Offset}
 TableOffset:=((TableBase shr 20) shl 2); {Divide Base by 1MB then multiply by 4 to get Offset into Page Table}
 
 {Read Page Table}
 CoarseEntry:=PLongWord(PtrUInt(PAGE_TABLE_BASE) + PtrUInt(TableOffset))^;
 
 {Check Level 1 Type}
 if (CoarseEntry and ARMV6_L1D_TYPE_COARSE) <> 0 then
  begin
   {Get Coarse Base}
   CoarseBase:=(CoarseEntry and ARMV6_L1D_COARSE_BASE_MASK);
 
   {Get Large Base}
   LargeBase:=(Address and ARMV6_L2D_LARGE_BASE_MASK);
   if LargeBase <> Address then Exit; {Must begin on a 64KB boundary}
   
   {Get Physical Base}
   PhysicalBase:=(PhysicalAddress and ARMV6_L2D_LARGE_BASE_MASK);
   if PhysicalBase <> PhysicalAddress then Exit; {Must begin on a 64KB boundary}
 
   {Get Large Offset}
   LargeOffset:=(((LargeBase shr 16) and $000000FF) shl 2); {Divide Base by 64KB then multiply by 4 to get Offset into Coarse Page Table}
   if ((CoarseBase + LargeOffset) and $3F) <> 0 then Exit;  {Must begin on a 64 byte boundary}
   
   {Check Flags}
   LargeFlags:=Flags and not(ARMV6_L2D_LARGE_BASE_MASK);
   //To Do
 
   {Write Coarse Page Table (16 consecutive entries)}
   for Count:=0 to 15 do
    begin
     PLongWord(PtrUInt(CoarseBase) + PtrUInt(LargeOffset))^:=PhysicalBase or LargeFlags or ARMV6_L2D_TYPE_LARGE;
     Inc(LargeOffset,SizeOf(LongWord));
    end; 
    
   Result:=True;
  end;   
end;

{==============================================================================}

function ARMv6GetPageTableSmall(Address:PtrUInt):LongWord;
{Get the descriptor for a Small Page Table entry (4KB)}
{See page 6-40 of the ARM1176JZF-S Technical Reference Manual}
var
 TableBase:LongWord;
 TableOffset:LongWord;

 CoarseBase:LongWord;
 CoarseEntry:LongWord;

 SmallBase:LongWord;
 SmallEntry:LongWord;
 SmallOffset:LongWord;
begin
 {}
 Result:=0;
 
 {Check Page Table}
 if PAGE_TABLE_SIZE < SIZE_16K then Exit; {Level 1 Page Table is 16KB}
 if Pointer(PAGE_TABLE_BASE) = nil then Exit;
 
 {Get Table Base}
 TableBase:=(Address and ARMV6_L1D_SECTION_BASE_MASK);
 
 {Get Table Offset}
 TableOffset:=((TableBase shr 20) shl 2); {Divide Base by 1MB then multiply by 4 to get Offset into Page Table}
 
 {Read Page Table}
 CoarseEntry:=PLongWord(PtrUInt(PAGE_TABLE_BASE) + PtrUInt(TableOffset))^;
 
 {Check Level 1 Type}
 if (CoarseEntry and ARMV6_L1D_TYPE_COARSE) <> 0 then
  begin
   {Get Coarse Base}
   CoarseBase:=(CoarseEntry and ARMV6_L1D_COARSE_BASE_MASK);
   
   {Get Small Base}
   SmallBase:=(Address and ARMV6_L2D_SMALL_BASE_MASK);
   
   {Get Small Offset}
   SmallOffset:=(((SmallBase shr 12) and $000000FF) shl 2); {Divide Base by 4KB then multiply by 4 to get Offset into Coarse Page Table}
   
   {Read Coarse Page Table}
   SmallEntry:=PLongWord(PtrUInt(CoarseBase) + PtrUInt(SmallOffset))^;
   
   {Check Level 2 Type}
   if (SmallEntry and ARMV6_L2D_TYPE_SMALL) <> 0 then
    begin
     {Return Result}
     Result:=SmallEntry;
    end;
  end; 
end;

{==============================================================================}

function ARMv6SetPageTableSmall(Address,PhysicalAddress:PtrUInt;Flags:Word):Boolean;
{Set the descriptor for a Small Page Table entry (4KB)}
{See page 6-40 of the ARM1176JZF-S Technical Reference Manual}
{Note: Caller must call ARMv6InvalidateTLB after changes if MMU is enabled}
var
 TableBase:LongWord;
 TableOffset:LongWord;

 CoarseBase:LongWord;
 CoarseEntry:LongWord;
 
 SmallBase:LongWord;
 SmallFlags:LongWord;
 SmallOffset:LongWord;
 
 PhysicalBase:LongWord;
begin
 {}
 Result:=False;
 
 {Check Page Table}
 if PAGE_TABLE_SIZE < SIZE_16K then Exit; {Level 1 Page Table is 16KB}
 if Pointer(PAGE_TABLE_BASE) = nil then Exit;
 
 {Get Table Base}
 TableBase:=(Address and ARMV6_L1D_SECTION_BASE_MASK);
 
 {Get Table Offset}
 TableOffset:=((TableBase shr 20) shl 2); {Divide Base by 1MB then multiply by 4 to get Offset into Page Table}
 
 {Read Page Table}
 CoarseEntry:=PLongWord(PtrUInt(PAGE_TABLE_BASE) + PtrUInt(TableOffset))^;
 
 {Check Level 1 Type}
 if (CoarseEntry and ARMV6_L1D_TYPE_COARSE) <> 0 then
  begin
   {Get Coarse Base}
   CoarseBase:=(CoarseEntry and ARMV6_L1D_COARSE_BASE_MASK);
 
   {Get Small Base}
   SmallBase:=(Address and ARMV6_L2D_SMALL_BASE_MASK);
   if SmallBase <> Address then Exit; {Must begin on a 4KB boundary}
   
   {Get Physical Base}
   PhysicalBase:=(PhysicalAddress and ARMV6_L2D_SMALL_BASE_MASK);
   if PhysicalBase <> PhysicalAddress then Exit; {Must begin on a 4KB boundary}
 
   {Get Small Offset}
   SmallOffset:=(((SmallBase shr 12) and $000000FF) shl 2); {Divide Base by 4KB then multiply by 4 to get Offset into Coarse Page Table}
 
   {Check Flags}
   SmallFlags:=Flags and not(ARMV6_L2D_SMALL_BASE_MASK);
   //To Do
   
   {Write Coarse Page Table}
   PLongWord(PtrUInt(CoarseBase) + PtrUInt(SmallOffset))^:=PhysicalBase or SmallFlags or ARMV6_L2D_TYPE_SMALL;
   
   Result:=True;
  end;   
end;

{==============================================================================}

function ARMv6GetPageTableSection(Address:PtrUInt):LongWord;
{Get the descriptor for a Page Table Section (1MB) or Supersection (16MB)}
{See page 6-39 of the ARM1176JZF-S Technical Reference Manual}
var
 SectionBase:LongWord;
 SectionEntry:LongWord;
 SectionOffset:LongWord;
begin
 {}
 Result:=0;
 
 {Check Page Table}
 if PAGE_TABLE_SIZE < SIZE_16K then Exit; {Level 1 Page Table is 16KB}
 if Pointer(PAGE_TABLE_BASE) = nil then Exit;
 
 {Get Section Base}
 SectionBase:=(Address and ARMV6_L1D_SECTION_BASE_MASK);
 
 {Get Section Offset}
 SectionOffset:=((SectionBase shr 20) shl 2); {Divide Base by 1MB then multiply by 4 to get Offset into Page Table}
 
 {Read Page Table}
 SectionEntry:=PLongWord(PtrUInt(PAGE_TABLE_BASE) + PtrUInt(SectionOffset))^;
 
 {Check Level 1 Type}
 if (SectionEntry and ARMV6_L1D_TYPE_SECTION) <> 0 then
  begin
   {Return Result}
   Result:=SectionEntry;
  end; 
end;

{==============================================================================}

function ARMv6SetPageTableSection(Address,PhysicalAddress:PtrUInt;Flags:LongWord):Boolean;
{Set the descriptor for a Page Table Section (1MB)}
{See page 6-39 of the ARM1176JZF-S Technical Reference Manual}
{Note: Caller must call ARMv6InvalidateTLB after changes if MMU is enabled}
var
 SectionBase:LongWord;
 SectionFlags:LongWord;
 SectionOffset:LongWord;

 PhysicalBase:LongWord;
begin
 {}
 Result:=False;
 
 {Check Page Table}
 if PAGE_TABLE_SIZE < SIZE_16K then Exit; {Level 1 Page Table is 16KB}
 if Pointer(PAGE_TABLE_BASE) = nil then Exit;
 
 {Get Section Base}
 SectionBase:=(Address and ARMV6_L1D_SECTION_BASE_MASK);
 if SectionBase <> Address then Exit; {Must begin on a 1MB boundary}
 
 {Get Physical Base}
 PhysicalBase:=(PhysicalAddress and ARMV6_L1D_SECTION_BASE_MASK);
 if PhysicalBase <> PhysicalAddress then Exit; {Must begin on a 1MB boundary}
 
 {Get Section Offset}
 SectionOffset:=((SectionBase shr 20) shl 2); {Divide Base by 1MB then multiply by 4 to get Offset into Page Table}
 
 {Check Flags}
 SectionFlags:=Flags and not(ARMV6_L1D_SECTION_BASE_MASK);
 SectionFlags:=SectionFlags and not(ARMV6_L1D_FLAG_SUPERSECTION);
 //To Do
 
 {Write Page Table}
 PLongWord(PtrUInt(PAGE_TABLE_BASE) + PtrUInt(SectionOffset))^:=PhysicalBase or SectionFlags or ARMV6_L1D_TYPE_SECTION;
 
 Result:=True;
end;

{==============================================================================}

function ARMv6SetPageTableSupersection(Address,PhysicalAddress:PtrUInt;Flags:LongWord):Boolean;
{Set the descriptor for a Page Table Supersection (16MB)}
{Supersection Page Table descriptors must begin on a 16 longword (64 byte)
 boundary and be repeated for 16 consecutive longwords}
{See page 6-39 of the ARM1176JZF-S Technical Reference Manual}
{Note: Caller must call ARMv6InvalidateTLB after changes if MMU is enabled}
var
 Count:Integer;
 
 SectionBase:LongWord;
 SectionFlags:LongWord;
 SectionOffset:LongWord;

 PhysicalBase:LongWord;
begin
 {}
 Result:=False;
 
 {Check Page Table}
 if PAGE_TABLE_SIZE < SIZE_16K then Exit; {Level 1 Page Table is 16KB}
 if Pointer(PAGE_TABLE_BASE) = nil then Exit;
 
 {Get Section Base}
 SectionBase:=(Address and ARMV6_L1D_SUPERSECTION_BASE_MASK); {ARMV6_L1D_SECTION_BASE_MASK}
 if SectionBase <> Address then Exit; {Must begin on a 16MB boundary}
 
 {Get Physical Base}
 PhysicalBase:=(PhysicalAddress and ARMV6_L1D_SUPERSECTION_BASE_MASK); {ARMV6_L1D_SECTION_BASE_MASK}
 if PhysicalBase <> PhysicalAddress then Exit; {Must begin on a 16MB boundary}
 
 {Get Section Offset}
 SectionOffset:=((SectionBase shr 20) shl 2); {Divide Base by 1MB then multiply by 4 to get Offset into Page Table}
 if ((PAGE_TABLE_BASE + SectionOffset) and $3F) <> 0 then Exit; {Must begin on a 64 byte boundary}
 
 {Check Flags}
 SectionFlags:=Flags and not(ARMV6_L1D_SECTION_BASE_MASK);
 SectionFlags:=SectionFlags or ARMV6_L1D_FLAG_SUPERSECTION;
 //To Do
 
 {Write Page Table (16 consecutive entries)}
 for Count:=0 to 15 do
  begin
   PLongWord(PtrUInt(PAGE_TABLE_BASE) + PtrUInt(SectionOffset))^:=PhysicalBase or SectionFlags or ARMV6_L1D_TYPE_SECTION;
   Inc(SectionOffset,SizeOf(LongWord));
  end; 
 
 Result:=True;
end;

{==============================================================================}
{==============================================================================}

end.
