{
Ultibo Platform interface unit for ARMv7.

Copyright (C) 2015 - SoftOz Pty Ltd.

Arch
====

 ARMv7 (Cortex A5/A7/A8/A9/A15/A17)

Boards
======
 
 Raspberry Pi 2 - Model B
 Raspberry Pi 3 - Model B
 BeagleBone Black
 Banana Pi
 Banana Pro
 Cubox-i2
 Cubox-i2Ex
 Cubox-i4Pro
 Hummingboard
 Odroid C1
 Odroid U3
 Odroid XU3
 QEMU VersatilePB
 
Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)
 
Credits
=======

 Information for this unit was obtained from:

  NetBSD - /src/sys/arch/arm/arm/cpufunc_asm_armv7.S - Copyright 2010 Per Odlund
 
  Linux - /arch/arm/mm/proc-v7.S - Copyright 2001 Deep Blue Solutions Ltd
          /arch/arm/mm/proc-v7-2level.S - Copyright 2001 Deep Blue Solutions Ltd
          /arch/arm/mm/cache-v7.S - Copyright (C) 2005 ARM Ltd
          
References
==========
 
 Cortex-A9 Technical Reference Manual (Revision: r4p1)
 
 Cortex-A9 MPCore Technical Reference Manual (Revision: r4p1)
 
 Cortex-A8 Technical Reference Manual (Revision: r3p2)
 
 Cortex-A7 MPCore Technical Reference Manual (Revision: r0p5)
 
 ARM v7 Architecture Reference Manual
 
 ARM Architecture Reference Manual (ARMv7-A and ARMv7-R edition)
 
 ARM Synchronization Primitives (DHT0008A_arm_synchronization_primitives.pdf)
 
 ARM Technical Support Knowledge Articles
  In what situations might I need to insert memory barrier instructions? - http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.faqs/ka14041.html
 
Platform ARMv7
==============

The ARMv7 does not support the SWP/SWPB instructions for syncronisation (Lock/Mutex/Semaphore etc) unless enabled.

On ARMv7 Unaligned memory access is always enabled.

On ARMv7 the Extended Page Table format is always enabled.

For usage of barriers (DMB/DSB/ISB) after cache maintenance operations see: ARM.Reference_Manual_1.pdf - Appendix G Barrier Litmus Tests

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit PlatformARMv7; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,PlatformARM,HeapManager,Threads,SysUtils;

//To Do //Look for:

//Critical

//TestingRpi2

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}
{--$DEFINE ARMV7_SPIN_WAIT_INTERRUPT} {Use Wait For Interrupt in Spinlocks instead of Send Event / Wait For Event}

{==============================================================================}
const
 {ARMv7 specific constants common to all processor models}
 
 {Page Table Shift}
 ARMV7_PAGE_TABLES_SHIFT = 10;
 
 {Definitions of CP15 C0 (Main ID Register) bits in the system control processor registers}
 ARMV7_CP15_C0_MAINID_IMPLEMENTOR_MASK  = ($FF shl 24);
 ARMV7_CP15_C0_MAINID_VARIANT_MASK      = ($F shl 20);
 ARMV7_CP15_C0_MAINID_ARCHITECTURE_MASK = ($F shl 16);
 ARMV7_CP15_C0_MAINID_PARTNUMBER_MASK   = ($FFF shl 4);
 ARMV7_CP15_C0_MAINID_REVISION_MASK     = ($F shl 0);
 
 ARMV7_CP15_C0_MAINID_IMPLEMENTOR_ARM       = ($41 shl 24);
 ARMV7_CP15_C0_MAINID_IMPLEMENTOR_DEC       = ($44 shl 24);
 ARMV7_CP15_C0_MAINID_IMPLEMENTOR_FREESCALE = ($4D shl 24);
 ARMV7_CP15_C0_MAINID_IMPLEMENTOR_QUALCOMM  = ($51 shl 24);
 ARMV7_CP15_C0_MAINID_IMPLEMENTOR_MARVELL   = ($56 shl 24);
 ARMV7_CP15_C0_MAINID_IMPLEMENTOR_INTEL     = ($69 shl 24);

 ARMV7_CP15_C0_MAINID_ARCHITECTURE_ARMV4    = ($1 shl 16);
 ARMV7_CP15_C0_MAINID_ARCHITECTURE_ARMV4T   = ($2 shl 16);
 ARMV7_CP15_C0_MAINID_ARCHITECTURE_ARMV5    = ($3 shl 16);
 ARMV7_CP15_C0_MAINID_ARCHITECTURE_ARMV5T   = ($4 shl 16);
 ARMV7_CP15_C0_MAINID_ARCHITECTURE_ARMV5TE  = ($5 shl 16);
 ARMV7_CP15_C0_MAINID_ARCHITECTURE_ARMV5TEJ = ($6 shl 16);
 ARMV7_CP15_C0_MAINID_ARCHITECTURE_ARMV6    = ($7 shl 16);
 ARMV7_CP15_C0_MAINID_ARCHITECTURE_CPUID    = ($F shl 16);
 
 ARMV7_CP15_C0_MAINID_PARTNUMBER_CORTEX_A5   = ($C05 shl 4);
 ARMV7_CP15_C0_MAINID_PARTNUMBER_CORTEX_A7   = ($C07 shl 4);
 ARMV7_CP15_C0_MAINID_PARTNUMBER_CORTEX_A8   = ($C08 shl 4);
 ARMV7_CP15_C0_MAINID_PARTNUMBER_CORTEX_A9   = ($C09 shl 4);
 ARMV7_CP15_C0_MAINID_PARTNUMBER_CORTEX_A15  = ($C0F shl 4);
 ARMV7_CP15_C0_MAINID_PARTNUMBER_CORTEX_A17  = ($C0E shl 4);
 {The following are ARMv8 part numbers, included here to allow ARMv7 code on ARMv8 in 32bit mode}
 ARMV7_CP15_C0_MAINID_PARTNUMBER_CORTEX_A53  = ($D03 shl 4);
 ARMV7_CP15_C0_MAINID_PARTNUMBER_CORTEX_A57  = ($D07 shl 4);
 ARMV7_CP15_C0_MAINID_PARTNUMBER_CORTEX_A72  = ($D08 shl 4);
 
 {Definitions of CP15 C0 (Multiprocessor Affinity Register) bits in the system control processor registers}
 ARMV7_CP15_C0_MPID_MPE               = (1 shl 31);
 ARMV7_CP15_C0_MPID_U_UNIPROCESSOR    = (1 shl 30);
 ARMV7_CP15_C0_MPID_U_MULTIPROCESSOR  = (0 shl 30);
 ARMV7_CP15_C0_MPID_CLUSTERID_MASK    = ($F shl 8);
 ARMV7_CP15_C0_MPID_CPUID_MASK        = (3 shl 0);
 
 {Definitions of CP15 C0 (Cache Size ID Register) bits in the system control processor registers}
 ARMV7_CP15_C0_CCSID_WT            = (1 shl 31);      {Indicates whether the cache level supports Write-Through}
 ARMV7_CP15_C0_CCSID_WB            = (1 shl 30);      {Indicates whether the cache level supports Write-Back} 
 ARMV7_CP15_C0_CCSID_RA            = (1 shl 29);      {Indicates whether the cache level supports Read-Allocation} 
 ARMV7_CP15_C0_CCSID_WA            = (1 shl 28);      {Indicates whether the cache level supports Write-Allocation}
 ARMV7_CP15_C0_CCSID_NUMSETS_MASK  = ($7FFF shl 13);  {(Number of sets in cache) - 1, therefore a value of 0 indicates 1 set in the cache. The number of sets does not have to be a power of 2}
 ARMV7_CP15_C0_CCSID_NUMWAYS_MASK  = ($3FF shl 3);    {(Associativity of cache) - 1, therefore a value of 0 indicates an associativity of 1. The associativity does not have to be a power of 2}
 ARMV7_CP15_C0_CCSID_LINESIZE_MASK = (7 shl 0);       {(Log2(Number of words in cache line)) -2. (eg For a line length of 8 words: Log2(8) = 3, LineSize entry = 1)}

 ARMV7_CP15_C0_CCSID_NUMSETS_SHIFT = 13;
 ARMV7_CP15_C0_CCSID_NUMWAYS_SHIFT = 3;
 
 {Definitions of CP15 C0 (Cache Level ID Register) bits in the system control processor registers}
 ARMV7_CP15_C0_CLID_LOUU_MASK          = (7 shl 27); {Level of Unification Uniprocessor for the cache hierarchy}
 ARMV7_CP15_C0_CLID_LOC_MASK           = (7 shl 24); {Level of Coherency for the cache hierarchy} 
 ARMV7_CP15_C0_CLID_LOUIS_MASK         = (7 shl 21); {Level of Unification Inner Shareable for the cache hierarchy} 
 ARMV7_CP15_C0_CLID_CTYPE7_MASK        = (7 shl 18); {Cache Type fields. Indicate the type of cache implemented at each level, from Level 1 up to a maximum of seven levels of cache hierarchy}
 ARMV7_CP15_C0_CLID_CTYPE7_NONE        = (0 shl 18); {No cache}
 ARMV7_CP15_C0_CLID_CTYPE7_INSTRUCTION = (1 shl 18); {Instruction cache only}
 ARMV7_CP15_C0_CLID_CTYPE7_DATA        = (2 shl 18); {Data cache only}
 ARMV7_CP15_C0_CLID_CTYPE7_SEPARATE    = (3 shl 18); {Separate instruction and data caches}
 ARMV7_CP15_C0_CLID_CTYPE7_UNIFIED     = (4 shl 18); {Unified cache}
  
 ARMV7_CP15_C0_CLID_CTYPE6_MASK        = (7 shl 15); {Cache Type fields. Indicate the type of cache implemented at each level, from Level 1 up to a maximum of seven levels of cache hierarchy}
 ARMV7_CP15_C0_CLID_CTYPE6_NONE        = (0 shl 15); {No cache}
 ARMV7_CP15_C0_CLID_CTYPE6_INSTRUCTION = (1 shl 15); {Instruction cache only}
 ARMV7_CP15_C0_CLID_CTYPE6_DATA        = (2 shl 15); {Data cache only}
 ARMV7_CP15_C0_CLID_CTYPE6_SEPARATE    = (3 shl 15); {Separate instruction and data caches}
 ARMV7_CP15_C0_CLID_CTYPE6_UNIFIED     = (4 shl 15); {Unified cache}
 
 ARMV7_CP15_C0_CLID_CTYPE5_MASK        = (7 shl 12); {Cache Type fields. Indicate the type of cache implemented at each level, from Level 1 up to a maximum of seven levels of cache hierarchy}
 ARMV7_CP15_C0_CLID_CTYPE5_NONE        = (0 shl 12); {No cache}
 ARMV7_CP15_C0_CLID_CTYPE5_INSTRUCTION = (1 shl 12); {Instruction cache only}
 ARMV7_CP15_C0_CLID_CTYPE5_DATA        = (2 shl 12); {Data cache only}
 ARMV7_CP15_C0_CLID_CTYPE5_SEPARATE    = (3 shl 12); {Separate instruction and data caches}
 ARMV7_CP15_C0_CLID_CTYPE5_UNIFIED     = (4 shl 12); {Unified cache}

 ARMV7_CP15_C0_CLID_CTYPE4_MASK        = (7 shl 9);  {Cache Type fields. Indicate the type of cache implemented at each level, from Level 1 up to a maximum of seven levels of cache hierarchy}
 ARMV7_CP15_C0_CLID_CTYPE4_NONE        = (0 shl 9);  {No cache}
 ARMV7_CP15_C0_CLID_CTYPE4_INSTRUCTION = (1 shl 9);  {Instruction cache only}
 ARMV7_CP15_C0_CLID_CTYPE4_DATA        = (2 shl 9);  {Data cache only}
 ARMV7_CP15_C0_CLID_CTYPE4_SEPARATE    = (3 shl 9);  {Separate instruction and data caches}
 ARMV7_CP15_C0_CLID_CTYPE4_UNIFIED     = (4 shl 9);  {Unified cache}

 ARMV7_CP15_C0_CLID_CTYPE3_MASK        = (7 shl 6);  {Cache Type fields. Indicate the type of cache implemented at each level, from Level 1 up to a maximum of seven levels of cache hierarchy}
 ARMV7_CP15_C0_CLID_CTYPE3_NONE        = (0 shl 6);  {No cache}
 ARMV7_CP15_C0_CLID_CTYPE3_INSTRUCTION = (1 shl 6);  {Instruction cache only}
 ARMV7_CP15_C0_CLID_CTYPE3_DATA        = (2 shl 6);  {Data cache only}
 ARMV7_CP15_C0_CLID_CTYPE3_SEPARATE    = (3 shl 6);  {Separate instruction and data caches}
 ARMV7_CP15_C0_CLID_CTYPE3_UNIFIED     = (4 shl 6);  {Unified cache}

 ARMV7_CP15_C0_CLID_CTYPE2_MASK        = (7 shl 3);  {Cache Type fields. Indicate the type of cache implemented at each level, from Level 1 up to a maximum of seven levels of cache hierarchy}
 ARMV7_CP15_C0_CLID_CTYPE2_NONE        = (0 shl 3);  {No cache}
 ARMV7_CP15_C0_CLID_CTYPE2_INSTRUCTION = (1 shl 3);  {Instruction cache only}
 ARMV7_CP15_C0_CLID_CTYPE2_DATA        = (2 shl 3);  {Data cache only}
 ARMV7_CP15_C0_CLID_CTYPE2_SEPARATE    = (3 shl 3);  {Separate instruction and data caches}
 ARMV7_CP15_C0_CLID_CTYPE2_UNIFIED     = (4 shl 3);  {Unified cache}

 ARMV7_CP15_C0_CLID_CTYPE1_MASK        = (7 shl 0);  {Cache Type fields. Indicate the type of cache implemented at each level, from Level 1 up to a maximum of seven levels of cache hierarchy}
 ARMV7_CP15_C0_CLID_CTYPE1_NONE        = (0 shl 0);  {No cache}
 ARMV7_CP15_C0_CLID_CTYPE1_INSTRUCTION = (1 shl 0);  {Instruction cache only}
 ARMV7_CP15_C0_CLID_CTYPE1_DATA        = (2 shl 0);  {Data cache only}
 ARMV7_CP15_C0_CLID_CTYPE1_SEPARATE    = (3 shl 0);  {Separate instruction and data caches}
 ARMV7_CP15_C0_CLID_CTYPE1_UNIFIED     = (4 shl 0);  {Unified cache}
 
 {Definitions of CP15 C0 (Cache Size Selection Register) bits in the system control processor registers}
 ARMV7_CP15_C0_CSSEL_LEVEL1      = (0 shl 1); {Cache level of required cache. Permitted values are from 0b000, indicating Level 1 cache, to 0b110 indicating Level 7 cache}
 ARMV7_CP15_C0_CSSEL_LEVEL2      = (1 shl 1); 
 ARMV7_CP15_C0_CSSEL_LEVEL3      = (2 shl 1);  
 ARMV7_CP15_C0_CSSEL_LEVEL4      = (3 shl 1);  
 ARMV7_CP15_C0_CSSEL_LEVEL5      = (4 shl 1); 
 ARMV7_CP15_C0_CSSEL_LEVEL6      = (5 shl 1);  
 ARMV7_CP15_C0_CSSEL_LEVEL7      = (6 shl 1);  
 ARMV7_CP15_C0_CSSEL_DATA        = (0 shl 0); {Instruction not Data bit (0 = Data or unified cache)}
 ARMV7_CP15_C0_CSSEL_INSTRUCTION = (1 shl 0); {Instruction not Data bit (1 = Instruction cache)}
 
 {Definitions of CP15 C1 (Control Register) bits in the system control processor registers}
 ARMV7_CP15_C1_TE_BIT   = (1 shl 30);  {Thumb Exception enable. This bit enabled exceptions to be taken in Thumb state when set to 1 (Default 0)}
 ARMV7_CP15_C1_AFE_BIT  = (1 shl 29);  {Access Flag Enable bit. This bit enables use of the AP[0] bit in the translation table descriptors as an access flag when set to 1 (Default 0)}
 ARMV7_CP15_C1_TRE_BIT  = (1 shl 28);  {TEX remap enabled when set to 1 (TEX[2:1] become page table bits for OS) (Default 0)}
 ARMV7_CP15_C1_NMFI_BIT = (1 shl 27);  {Non-maskable Fast Interrupts enabled when set to 1 (Default 0)}
 ARMV7_CP15_C1_EE_BIT   = (1 shl 25);  {CPSR E bit is set to 1 on an exception when set to 1 (Default 0)}
 ARMV7_CP15_C1_VE_BIT   = (1 shl 24);  {Interrupt vectors are defined by the VIC interface when set to 1 (Default 0)}
 ARMV7_CP15_C1_U_BIT    = (1 shl 22);  {Unaligned data access support enabled when set to 1 (Always 1 in ARMv7). The processor permits unaligned loads and stores and support for mixed endian data is enabled}
 ARMV7_CP15_C1_FI_BIT   = (1 shl 21);  {Low interrupt latency configuration enabled when set to 1 (Default 0)}
 ARMV7_CP15_C1_UWXN_BIT = (1 shl 20);  {Unprivileged write permission implies Execute Never (XN) when set to 1 (Default 0)(Cortext-A7 MPCore)}
 ARMV7_CP15_C1_WXN_BIT  = (1 shl 19);  {Write permission implies Execute Never (XN) when set to 1 (Default 0)(Cortext-A7 MPCore)} 
 ARMV7_CP15_C1_HA_BIT   = (1 shl 17);  {Hardware Access Flag Enable bit. If the implementation provides hardware management of the access flag this bit enables the access flag management (Default 0)}
 ARMV7_CP15_C1_RR_BIT   = (1 shl 14);  {Predictable cache replacement strategy by round-robin replacement when set to 1 (Default 0)}
 ARMV7_CP15_C1_V_BIT    = (1 shl 13);  {High exception vectors selected when set to 1, address range = 0xFFFF0000-0xFFFF001C (Default 0)}
 ARMV7_CP15_C1_I_BIT    = (1 shl 12);  {L1 Instruction Cache enabled when set to 1 (Default 0)}
 ARMV7_CP15_C1_Z_BIT    = (1 shl 11);  {Branch prediction enabled when set to 1 (Default 0)(Always Enabled on Cortext-A7 MPCore)}
 ARMV7_CP15_C1_SW_BIT   = (1 shl 10);  {SWP/SWPB Enable bit. This bit enables the use of SWP and SWPB instructions when set to 1 (Default 0)}
 ARMV7_CP15_C1_B_BIT    = (1 shl 7);   {Big-endian word-invariant memory system when set to 1 (Always 0 in ARMv7)}
 ARMV7_CP15_C1_C_BIT    = (1 shl 2);   {L1 Data cache enabled when set to 1 (Default 0)}
 ARMV7_CP15_C1_A_BIT    = (1 shl 1);   {Strict alignment fault checking enabled when set to 1 (Default 0)}
 ARMV7_CP15_C1_M_BIT    = (1 shl 0);   {MMU enabled when set to 1 (Default 0)}

 {Definitions of CP15 C1 (Auxiliary Control Register) bits in the system control processor registers}
 ARMV7_CP15_C1_AUX_DDI      = (1 shl 28);  {Disable dual issue when set to 1 (Default 0)}
 ARMV7_CP15_C1_AUX_DDVM     = (1 shl 15);  {Disable Distributed Virtual Memory (DVM) transactions when set to 1  (Default 0)}
 ARMV7_CP15_C1_AUX_L1PCTL_0 = (0 shl 13);  {L1 Data prefetch control, Prefetch disabled}
 ARMV7_CP15_C1_AUX_L1PCTL_1 = (1 shl 13);  {L1 Data prefetch control, 1 outstanding pre-fetch permitted}
 ARMV7_CP15_C1_AUX_L1PCTL_2 = (2 shl 13);  {L1 Data prefetch control, 2 outstanding pre-fetches permitted}
 ARMV7_CP15_C1_AUX_L1PCTL_3 = (3 shl 13);  {L1 Data prefetch control, 3 outstanding pre-fetches permitted, this is the reset value (Default)}
 ARMV7_CP15_C1_AUX_L1RADIS  = (1 shl 12);  {L1 Data Cache read-allocate mode disable when set to 1 (Default 0)}
 ARMV7_CP15_C1_AUX_L2RADIS  = (1 shl 11);  {L2 Data Cache read-allocate mode disable when set to 1 (Default 0)}
 ARMV7_CP15_C1_AUX_DODMBS   = (1 shl 10);  {Disable optimized data memory barrier behavior when set to 1 (Default 0)}
 ARMV7_CP15_C1_AUX_SMP      = (1 shl 6);   {Enables coherent requests to the processor when set to 1 (Default 0)} {You must ensure this bit is set to 1 before the caches and MMU are enabled, or any cache and TLB maintenance operations are performed}
 
 ARMV7_CP15_C1_AUX_FW       = (1 shl 0);   {Cache and TLB maintenance broadcast enabled when set to 1 (Default 0) (Cortex-A9 Only)}
 
 {Definitions of CP15 C1 (Coprocessor Access Control Register) bits in the system control processor registers}
 ARMV7_CP15_C1_COPRO_ASEDIS = (1 shl 31); {Disable Advanced SIMD Functionality when set to 1 (Default 0)}
 ARMV7_CP15_C1_COPRO_D32DIS = (1 shl 30); {Disable use of D16-D31 of the VFP register file when set to 1 (Default 0)}
 
 ARMV7_CP15_C1_CP0_NONE = (0 shl 0); {Access denied (Default)}
 ARMV7_CP15_C1_CP0_SYS  = (1 shl 0); {Privileged mode access only}
 ARMV7_CP15_C1_CP0_USER = (3 shl 0); {Privileged and User mode access}
 
 ARMV7_CP15_C1_CP1_NONE = (0 shl 2); {Access denied (Default)}
 ARMV7_CP15_C1_CP1_SYS  = (1 shl 2); {Privileged mode access only}
 ARMV7_CP15_C1_CP1_USER = (3 shl 2); {Privileged and User mode access}

 ARMV7_CP15_C1_CP2_NONE = (0 shl 4); {Access denied (Default)}
 ARMV7_CP15_C1_CP2_SYS  = (1 shl 4); {Privileged mode access only}
 ARMV7_CP15_C1_CP2_USER = (3 shl 4); {Privileged and User mode access}

 ARMV7_CP15_C1_CP3_NONE = (0 shl 6); {Access denied (Default)}
 ARMV7_CP15_C1_CP3_SYS  = (1 shl 6); {Privileged mode access only}
 ARMV7_CP15_C1_CP3_USER = (3 shl 6); {Privileged and User mode access}

 ARMV7_CP15_C1_CP4_NONE = (0 shl 8); {Access denied (Default)}
 ARMV7_CP15_C1_CP4_SYS  = (1 shl 8); {Privileged mode access only}
 ARMV7_CP15_C1_CP4_USER = (3 shl 8); {Privileged and User mode access}

 ARMV7_CP15_C1_CP5_NONE = (0 shl 10); {Access denied (Default)}
 ARMV7_CP15_C1_CP5_SYS  = (1 shl 10); {Privileged mode access only}
 ARMV7_CP15_C1_CP5_USER = (3 shl 10); {Privileged and User mode access}

 ARMV7_CP15_C1_CP6_NONE = (0 shl 12); {Access denied (Default)}
 ARMV7_CP15_C1_CP6_SYS  = (1 shl 12); {Privileged mode access only}
 ARMV7_CP15_C1_CP6_USER = (3 shl 12); {Privileged and User mode access}

 ARMV7_CP15_C1_CP7_NONE = (0 shl 14); {Access denied (Default)}
 ARMV7_CP15_C1_CP7_SYS  = (1 shl 14); {Privileged mode access only}
 ARMV7_CP15_C1_CP7_USER = (3 shl 14); {Privileged and User mode access}
 
 ARMV7_CP15_C1_CP8_NONE = (0 shl 16); {Access denied (Default)}
 ARMV7_CP15_C1_CP8_SYS  = (1 shl 16); {Privileged mode access only}
 ARMV7_CP15_C1_CP8_USER = (3 shl 16); {Privileged and User mode access}

 ARMV7_CP15_C1_CP9_NONE = (0 shl 18); {Access denied (Default)}
 ARMV7_CP15_C1_CP9_SYS  = (1 shl 18); {Privileged mode access only}
 ARMV7_CP15_C1_CP9_USER = (3 shl 18); {Privileged and User mode access}

 ARMV7_CP15_C1_CP10_NONE = (0 shl 20); {Access denied (Default)}
 ARMV7_CP15_C1_CP10_SYS  = (1 shl 20); {Privileged mode access only}
 ARMV7_CP15_C1_CP10_USER = (3 shl 20); {Privileged and User mode access}

 ARMV7_CP15_C1_CP11_NONE = (0 shl 22); {Access denied (Default)}
 ARMV7_CP15_C1_CP11_SYS  = (1 shl 22); {Privileged mode access only}
 ARMV7_CP15_C1_CP11_USER = (3 shl 22); {Privileged and User mode access}

 ARMV7_CP15_C1_CP12_NONE = (0 shl 24); {Access denied (Default)}
 ARMV7_CP15_C1_CP12_SYS  = (1 shl 24); {Privileged mode access only}
 ARMV7_CP15_C1_CP12_USER = (3 shl 24); {Privileged and User mode access}

 ARMV7_CP15_C1_CP13_NONE = (0 shl 26); {Access denied (Default)}
 ARMV7_CP15_C1_CP13_SYS  = (1 shl 26); {Privileged mode access only}
 ARMV7_CP15_C1_CP13_USER = (3 shl 26); {Privileged and User mode access}
 {Coprocessors CP14 (Debug Control) and CP15 (System Control) are not affected by the Coprocessor Access Control Register}

 {Definitions of CP15 C1 (Secure Configuration Register) bits in the system control processor registers}
 ARMV7_CP15_C1_SCR_SIF = (1 shl 9); {Secure Instruction Fetch bit} 
 ARMV7_CP15_C1_SCR_HCE = (1 shl 8); {Hyp Call enable. This bit enables the use of HVC instruction from Non-secure PL1 modes} 
 ARMV7_CP15_C1_SCR_SCD = (1 shl 7); {Secure Monitor Call disable. This bit causes the SMC instruction to be UNDEFINED in Non-secure state} 
 ARMV7_CP15_C1_SCR_NET = (1 shl 6); {Not Early Termination. This bit disables early termination of data operations} 
 ARMV7_CP15_C1_SCR_AW  = (1 shl 5); {A bit writable. This bit controls whether the A bit in the CPSR can be modified in Non-secure state} 
 ARMV7_CP15_C1_SCR_FW  = (1 shl 4); {F bit writable. This bit controls whether the F bit in the CPSR can be modified in Non-secure state} 
 ARMV7_CP15_C1_SCR_EA  = (1 shl 3); {External Abort handler. This bit controls which mode takes external aborts} 
 ARMV7_CP15_C1_SCR_FIQ = (1 shl 2); {FIQ handler. This bit controls which mode takes FIQ exceptions} 
 ARMV7_CP15_C1_SCR_IRQ = (1 shl 1); {IRQ handler. This bit controls which mode takes IRQ exceptions} 
 ARMV7_CP15_C1_SCR_NS  = (1 shl 0); {Non Secure bit. Except when the processor is in Monitor mode, this bit determines the security state of the processor} 

 {Definitions of CP15 C1 (Secure Debug Enable Register) bits in the system control processor registers}
 //To Do

 {Definitions of CP15 C1 (Non-Secure Access Control Register) bits in the system control processor registers}
 //To Do
 
 {Definitions of CP15 C2 (Memory Protection and Control Register) bits in the system control processor registers}
 //To Do

 {Definitions of CP15 C2 (Translation Table Support Registers) bits in the system control processor registers}
 //To Do

 {Definitions of CP15 C2 (Translation Table Base Registers 0 and 1) bits in the system control processor registers}
 ARMV7_CP15_C2_TTBR_BASE_MASK                 = $FFFFC000;
 ARMV7_CP15_C2_TTBR_NOS                       = (1 shl 5); {Not Outer Shareable bit  (0 Outer Shareable / 1 Inner Shareable)}
 ARMV7_CP15_C2_TTBR_RGN_OUTER_NONCACHED       = (0 shl 3); {Normal Outer Noncacheable (Default)}
 ARMV7_CP15_C2_TTBR_RGN_OUTER_WRITE_ALLOCATE  = (1 shl 3); {Normal Outer Write-back, Write Allocate}
 ARMV7_CP15_C2_TTBR_RGN_OUTER_WRITE_THROUGH   = (2 shl 3); {Normal Outer Write-through, No Allocate on Write}
 ARMV7_CP15_C2_TTBR_RGN_OUTER_WRITE_BACK      = (3 shl 3); {Normal Outer Write-back, No Allocate on Write}
 ARMV7_CP15_C2_TTBR_IMP                       = (1 shl 2); {The effect of this bit is IMPLEMENTATION DEFINED}
 ARMV7_CP15_C2_TTBR_S                         = (1 shl 1); {Shareable bit (0 Non Shareable / 1 Shareable)}
 ARMV7_CP15_C2_TTBR_C_INNER_CACHED            = (1 shl 0); {Cacheable bit (0 Inner Non Cacheable / 1 Inner Cacheable) (ARMv7-A base only)}
 ARMV7_CP15_C2_TTBR_IRGN_INNER_NONCACHED      = (0 shl 6) or (0 shl 0); {Normal Inner Noncacheable (Default)}
 ARMV7_CP15_C2_TTBR_IRGN_INNER_WRITE_ALLOCATE = (1 shl 6) or (0 shl 0); {Normal Inner Write-Back Write-Allocate Cacheable}
 ARMV7_CP15_C2_TTBR_IRGN_INNER_WRITE_THROUGH  = (0 shl 6) or (1 shl 0); {Normal Inner Write-Through Cacheable}
 ARMV7_CP15_C2_TTBR_IRGN_INNER_WRITE_BACK     = (1 shl 6) or (1 shl 0); {Normal Inner Write-Back no Write-Allocate Cacheable}

 {Definitions of CP15 C3 (Memory Protection and Control Register) bits in the system control processor registers}
 //To Do
 
 {Definitions of CP15 C3 (Domain Access Control Register) bits in the system control processor registers}
 ARMV7_CP15_C3_DOMAIN0_NONE     = (0 shl 0); {No access, Any access generates a domain fault (Default)}
 ARMV7_CP15_C3_DOMAIN0_CLIENT   = (1 shl 0); {Client, Accesses are checked against the access permission bits in the TLB entry}
 ARMV7_CP15_C3_DOMAIN0_MANAGER  = (3 shl 0); {Manager, Accesses are not checked against the access permission bits in the TLB entry, so a permission fault cannot be generated}

 ARMV7_CP15_C3_DOMAIN1_NONE     = (0 shl 2); {No access, Any access generates a domain fault (Default)}
 ARMV7_CP15_C3_DOMAIN1_CLIENT   = (1 shl 2); {Client, Accesses are checked against the access permission bits in the TLB entry}
 ARMV7_CP15_C3_DOMAIN1_MANAGER  = (3 shl 2); {Manager, Accesses are not checked against the access permission bits in the TLB entry, so a permission fault cannot be generated}

 ARMV7_CP15_C3_DOMAIN2_NONE     = (0 shl 4); {No access, Any access generates a domain fault (Default)}
 ARMV7_CP15_C3_DOMAIN2_CLIENT   = (1 shl 4); {Client, Accesses are checked against the access permission bits in the TLB entry}
 ARMV7_CP15_C3_DOMAIN2_MANAGER  = (3 shl 4); {Manager, Accesses are not checked against the access permission bits in the TLB entry, so a permission fault cannot be generated}

 ARMV7_CP15_C3_DOMAIN3_NONE     = (0 shl 6); {No access, Any access generates a domain fault (Default)}
 ARMV7_CP15_C3_DOMAIN3_CLIENT   = (1 shl 6); {Client, Accesses are checked against the access permission bits in the TLB entry}
 ARMV7_CP15_C3_DOMAIN3_MANAGER  = (3 shl 6); {Manager, Accesses are not checked against the access permission bits in the TLB entry, so a permission fault cannot be generated}

 ARMV7_CP15_C3_DOMAIN4_NONE     = (0 shl 8); {No access, Any access generates a domain fault (Default)}
 ARMV7_CP15_C3_DOMAIN4_CLIENT   = (1 shl 8); {Client, Accesses are checked against the access permission bits in the TLB entry}
 ARMV7_CP15_C3_DOMAIN4_MANAGER  = (3 shl 8); {Manager, Accesses are not checked against the access permission bits in the TLB entry, so a permission fault cannot be generated}

 ARMV7_CP15_C3_DOMAIN5_NONE     = (0 shl 10); {No access, Any access generates a domain fault (Default)}
 ARMV7_CP15_C3_DOMAIN5_CLIENT   = (1 shl 10); {Client, Accesses are checked against the access permission bits in the TLB entry}
 ARMV7_CP15_C3_DOMAIN5_MANAGER  = (3 shl 10); {Manager, Accesses are not checked against the access permission bits in the TLB entry, so a permission fault cannot be generated}

 ARMV7_CP15_C3_DOMAIN6_NONE     = (0 shl 12); {No access, Any access generates a domain fault (Default)}
 ARMV7_CP15_C3_DOMAIN6_CLIENT   = (1 shl 12); {Client, Accesses are checked against the access permission bits in the TLB entry}
 ARMV7_CP15_C3_DOMAIN6_MANAGER  = (3 shl 12); {Manager, Accesses are not checked against the access permission bits in the TLB entry, so a permission fault cannot be generated}

 ARMV7_CP15_C3_DOMAIN7_NONE     = (0 shl 14); {No access, Any access generates a domain fault (Default)}
 ARMV7_CP15_C3_DOMAIN7_CLIENT   = (1 shl 14); {Client, Accesses are checked against the access permission bits in the TLB entry}
 ARMV7_CP15_C3_DOMAIN7_MANAGER  = (3 shl 14); {Manager, Accesses are not checked against the access permission bits in the TLB entry, so a permission fault cannot be generated}

 ARMV7_CP15_C3_DOMAIN8_NONE     = (0 shl 16); {No access, Any access generates a domain fault (Default)}
 ARMV7_CP15_C3_DOMAIN8_CLIENT   = (1 shl 16); {Client, Accesses are checked against the access permission bits in the TLB entry}
 ARMV7_CP15_C3_DOMAIN8_MANAGER  = (3 shl 16); {Manager, Accesses are not checked against the access permission bits in the TLB entry, so a permission fault cannot be generated}

 ARMV7_CP15_C3_DOMAIN9_NONE     = (0 shl 18); {No access, Any access generates a domain fault (Default)}
 ARMV7_CP15_C3_DOMAIN9_CLIENT   = (1 shl 18); {Client, Accesses are checked against the access permission bits in the TLB entry}
 ARMV7_CP15_C3_DOMAIN9_MANAGER  = (3 shl 18); {Manager, Accesses are not checked against the access permission bits in the TLB entry, so a permission fault cannot be generated}

 ARMV7_CP15_C3_DOMAIN10_NONE    = (0 shl 20); {No access, Any access generates a domain fault (Default)}
 ARMV7_CP15_C3_DOMAIN10_CLIENT  = (1 shl 20); {Client, Accesses are checked against the access permission bits in the TLB entry}
 ARMV7_CP15_C3_DOMAIN10_MANAGER = (3 shl 20); {Manager, Accesses are not checked against the access permission bits in the TLB entry, so a permission fault cannot be generated}

 ARMV7_CP15_C3_DOMAIN11_NONE    = (0 shl 22); {No access, Any access generates a domain fault (Default)}
 ARMV7_CP15_C3_DOMAIN11_CLIENT  = (1 shl 22); {Client, Accesses are checked against the access permission bits in the TLB entry}
 ARMV7_CP15_C3_DOMAIN11_MANAGER = (3 shl 22); {Manager, Accesses are not checked against the access permission bits in the TLB entry, so a permission fault cannot be generated}

 ARMV7_CP15_C3_DOMAIN12_NONE    = (0 shl 24); {No access, Any access generates a domain fault (Default)}
 ARMV7_CP15_C3_DOMAIN12_CLIENT  = (1 shl 24); {Client, Accesses are checked against the access permission bits in the TLB entry}
 ARMV7_CP15_C3_DOMAIN12_MANAGER = (3 shl 24); {Manager, Accesses are not checked against the access permission bits in the TLB entry, so a permission fault cannot be generated}

 ARMV7_CP15_C3_DOMAIN13_NONE    = (0 shl 26); {No access, Any access generates a domain fault (Default)}
 ARMV7_CP15_C3_DOMAIN13_CLIENT  = (1 shl 26); {Client, Accesses are checked against the access permission bits in the TLB entry}
 ARMV7_CP15_C3_DOMAIN13_MANAGER = (3 shl 26); {Manager, Accesses are not checked against the access permission bits in the TLB entry, so a permission fault cannot be generated}

 ARMV7_CP15_C3_DOMAIN14_NONE    = (0 shl 28); {No access, Any access generates a domain fault (Default)}
 ARMV7_CP15_C3_DOMAIN14_CLIENT  = (1 shl 28); {Client, Accesses are checked against the access permission bits in the TLB entry}
 ARMV7_CP15_C3_DOMAIN14_MANAGER = (3 shl 28); {Manager, Accesses are not checked against the access permission bits in the TLB entry, so a permission fault cannot be generated}

 ARMV7_CP15_C3_DOMAIN15_NONE    = (0 shl 30); {No access, Any access generates a domain fault (Default)}
 ARMV7_CP15_C3_DOMAIN15_CLIENT  = (1 shl 30); {Client, Accesses are checked against the access permission bits in the TLB entry}
 ARMV7_CP15_C3_DOMAIN15_MANAGER = (3 shl 30); {Manager, Accesses are not checked against the access permission bits in the TLB entry, so a permission fault cannot be generated}
 
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

 {Definitions of CP15 C10 (Primary Region Remap Register) bits in the system control processor registers}
 ARMV7_CP15_C10_PRRR_NOS7  = (1 shl 31); {Outer Shareable property mapping for memory attributes 7, if the region is mapped as Normal Shareable (0 Outer Shareable / 1 Inner Shareable)}
 ARMV7_CP15_C10_PRRR_NOS6  = (1 shl 30); {Outer Shareable property mapping for memory attributes 6, if the region is mapped as Normal Shareable (0 Outer Shareable / 1 Inner Shareable)}
 ARMV7_CP15_C10_PRRR_NOS5  = (1 shl 29); {Outer Shareable property mapping for memory attributes 5, if the region is mapped as Normal Shareable (0 Outer Shareable / 1 Inner Shareable)}
 ARMV7_CP15_C10_PRRR_NOS4  = (1 shl 28); {Outer Shareable property mapping for memory attributes 4, if the region is mapped as Normal Shareable (0 Outer Shareable / 1 Inner Shareable)}
 ARMV7_CP15_C10_PRRR_NOS3  = (1 shl 27); {Outer Shareable property mapping for memory attributes 3, if the region is mapped as Normal Shareable (0 Outer Shareable / 1 Inner Shareable)}
 ARMV7_CP15_C10_PRRR_NOS2  = (1 shl 26); {Outer Shareable property mapping for memory attributes 2, if the region is mapped as Normal Shareable (0 Outer Shareable / 1 Inner Shareable)}
 ARMV7_CP15_C10_PRRR_NOS1  = (1 shl 25); {Outer Shareable property mapping for memory attributes 1, if the region is mapped as Normal Shareable (0 Outer Shareable / 1 Inner Shareable)}
 ARMV7_CP15_C10_PRRR_NOS0  = (1 shl 24); {Outer Shareable property mapping for memory attributes 0, if the region is mapped as Normal Shareable (0 Outer Shareable / 1 Inner Shareable)}
 ARMV7_CP15_C10_PRRR_NS1   = (1 shl 19); {Mapping of S = 1 attribute for Normal memory (0 Not Sharable / 1 Shareable)}
 ARMV7_CP15_C10_PRRR_NS0   = (1 shl 18); {Mapping of S = 0 attribute for Normal memory (0 Not Sharable / 1 Shareable)}
 ARMV7_CP15_C10_PRRR_DS1   = (1 shl 17); {Mapping of S = 1 attribute for Device memory (This field has no significance in the Cortex-A7)}
 ARMV7_CP15_C10_PRRR_DS0   = (1 shl 16); {Mapping of S = 0 attribute for Device memory (This field has no significance in the Cortex-A7)}
 ARMV7_CP15_C10_PRRR_TR7_STRONGLY_ORDERED = (0 shl 14); {Primary TEX mapping for memory attributes 7 (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_PRRR_TR7_DEVICE           = (1 shl 14); {Primary TEX mapping for memory attributes 7 (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_PRRR_TR7_NORMAL           = (2 shl 14); {Primary TEX mapping for memory attributes 7 (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_PRRR_TR6_STRONGLY_ORDERED = (0 shl 12); {Primary TEX mapping for memory attributes 6 (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_PRRR_TR6_DEVICE           = (1 shl 12); {Primary TEX mapping for memory attributes 6 (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_PRRR_TR6_NORMAL           = (2 shl 12); {Primary TEX mapping for memory attributes 6 (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_PRRR_TR5_STRONGLY_ORDERED = (0 shl 10); {Primary TEX mapping for memory attributes 5 (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_PRRR_TR5_DEVICE           = (1 shl 10); {Primary TEX mapping for memory attributes 5 (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_PRRR_TR5_NORMAL           = (2 shl 10); {Primary TEX mapping for memory attributes 5 (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_PRRR_TR4_STRONGLY_ORDERED = (0 shl 8);  {Primary TEX mapping for memory attributes 4 (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_PRRR_TR4_DEVICE           = (1 shl 8);  {Primary TEX mapping for memory attributes 4 (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_PRRR_TR4_NORMAL           = (2 shl 8);  {Primary TEX mapping for memory attributes 4 (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_PRRR_TR3_STRONGLY_ORDERED = (0 shl 6);  {Primary TEX mapping for memory attributes 3 (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_PRRR_TR3_DEVICE           = (1 shl 6);  {Primary TEX mapping for memory attributes 3 (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_PRRR_TR3_NORMAL           = (2 shl 6);  {Primary TEX mapping for memory attributes 3 (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_PRRR_TR2_STRONGLY_ORDERED = (0 shl 4);  {Primary TEX mapping for memory attributes 2 (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_PRRR_TR2_DEVICE           = (1 shl 4);  {Primary TEX mapping for memory attributes 2 (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_PRRR_TR2_NORMAL           = (2 shl 4);  {Primary TEX mapping for memory attributes 2 (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_PRRR_TR1_STRONGLY_ORDERED = (0 shl 2);  {Primary TEX mapping for memory attributes 1 (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_PRRR_TR1_DEVICE           = (1 shl 2);  {Primary TEX mapping for memory attributes 1 (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_PRRR_TR1_NORMAL           = (2 shl 2);  {Primary TEX mapping for memory attributes 1 (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_PRRR_TR0_STRONGLY_ORDERED = (0 shl 0);  {Primary TEX mapping for memory attributes 0 (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_PRRR_TR0_DEVICE           = (1 shl 0);  {Primary TEX mapping for memory attributes 0 (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_PRRR_TR0_NORMAL           = (2 shl 0);  {Primary TEX mapping for memory attributes 0 (The value of the TEX[0], C and B bits)}
 
                            {TR0 to TR7 Inner Shareable}
 ARMV7_CP15_C10_PRRR_MASK = ARMV7_CP15_C10_PRRR_NOS7 or ARMV7_CP15_C10_PRRR_NOS6 or ARMV7_CP15_C10_PRRR_NOS5 or ARMV7_CP15_C10_PRRR_NOS4
                         or ARMV7_CP15_C10_PRRR_NOS3 or ARMV7_CP15_C10_PRRR_NOS2 or ARMV7_CP15_C10_PRRR_NOS1 or ARMV7_CP15_C10_PRRR_NOS0
                            {S bit controls Shareable for Normal and Device memory}
                         or ARMV7_CP15_C10_PRRR_NS1 or ARMV7_CP15_C10_PRRR_DS1
                            {TR0 is Strongly Ordered}
                         or ARMV7_CP15_C10_PRRR_TR0_STRONGLY_ORDERED
                            {TR1/2/3 are Normal}
                         or ARMV7_CP15_C10_PRRR_TR1_NORMAL or ARMV7_CP15_C10_PRRR_TR2_NORMAL or ARMV7_CP15_C10_PRRR_TR3_NORMAL
                            {TR4 is Device}
                         or ARMV7_CP15_C10_PRRR_TR4_DEVICE
                            {TR7 is Normal}
                         or ARMV7_CP15_C10_PRRR_TR7_NORMAL;
 
 {Definitions of CP15 C10 (Normal Memory Remap Register) bits in the system control processor registers}
 ARMV7_CP15_C10_NMRR_OR7_NONCACHED        = (0 shl 30); {Outer Cacheable property mapping for memory attributes 7, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_OR7_WRITE_ALLOCATE   = (1 shl 30); {Outer Cacheable property mapping for memory attributes 7, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_OR7_WRITE_THROUGH    = (2 shl 30); {Outer Cacheable property mapping for memory attributes 7, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_OR7_WRITE_BACK       = (3 shl 30); {Outer Cacheable property mapping for memory attributes 7, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_OR6_NONCACHED        = (0 shl 28); {Outer Cacheable property mapping for memory attributes 6, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_OR6_WRITE_ALLOCATE   = (1 shl 28); {Outer Cacheable property mapping for memory attributes 6, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_OR6_WRITE_THROUGH    = (2 shl 28); {Outer Cacheable property mapping for memory attributes 6, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_OR6_WRITE_BACK       = (3 shl 28); {Outer Cacheable property mapping for memory attributes 6, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_OR5_NONCACHED        = (0 shl 26); {Outer Cacheable property mapping for memory attributes 5, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_OR5_WRITE_ALLOCATE   = (1 shl 26); {Outer Cacheable property mapping for memory attributes 5, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_OR5_WRITE_THROUGH    = (2 shl 26); {Outer Cacheable property mapping for memory attributes 5, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_OR5_WRITE_BACK       = (3 shl 26); {Outer Cacheable property mapping for memory attributes 5, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_OR4_NONCACHED        = (0 shl 24); {Outer Cacheable property mapping for memory attributes 4, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_OR4_WRITE_ALLOCATE   = (1 shl 24); {Outer Cacheable property mapping for memory attributes 4, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_OR4_WRITE_THROUGH    = (2 shl 24); {Outer Cacheable property mapping for memory attributes 4, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_OR4_WRITE_BACK       = (3 shl 24); {Outer Cacheable property mapping for memory attributes 4, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_OR3_NONCACHED        = (0 shl 22); {Outer Cacheable property mapping for memory attributes 3, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_OR3_WRITE_ALLOCATE   = (1 shl 22); {Outer Cacheable property mapping for memory attributes 3, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_OR3_WRITE_THROUGH    = (2 shl 22); {Outer Cacheable property mapping for memory attributes 3, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_OR3_WRITE_BACK       = (3 shl 22); {Outer Cacheable property mapping for memory attributes 3, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_OR2_NONCACHED        = (0 shl 20); {Outer Cacheable property mapping for memory attributes 2, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_OR2_WRITE_ALLOCATE   = (1 shl 20); {Outer Cacheable property mapping for memory attributes 2, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_OR2_WRITE_THROUGH    = (2 shl 20); {Outer Cacheable property mapping for memory attributes 2, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_OR2_WRITE_BACK       = (3 shl 20); {Outer Cacheable property mapping for memory attributes 2, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_OR1_NONCACHED        = (0 shl 18); {Outer Cacheable property mapping for memory attributes 1, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_OR1_WRITE_ALLOCATE   = (1 shl 18); {Outer Cacheable property mapping for memory attributes 1, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_OR1_WRITE_THROUGH    = (2 shl 18); {Outer Cacheable property mapping for memory attributes 1, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_OR1_WRITE_BACK       = (3 shl 18); {Outer Cacheable property mapping for memory attributes 1, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_OR0_NONCACHED        = (0 shl 16); {Outer Cacheable property mapping for memory attributes 0, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_OR0_WRITE_ALLOCATE   = (1 shl 16); {Outer Cacheable property mapping for memory attributes 0, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_OR0_WRITE_THROUGH    = (2 shl 16); {Outer Cacheable property mapping for memory attributes 0, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_OR0_WRITE_BACK       = (3 shl 16); {Outer Cacheable property mapping for memory attributes 0, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_IR7_NONCACHED        = (0 shl 14); {Inner Cacheable property mapping for memory attributes 7, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_IR7_WRITE_ALLOCATE   = (1 shl 14); {Inner Cacheable property mapping for memory attributes 7, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_IR7_WRITE_THROUGH    = (2 shl 14); {Inner Cacheable property mapping for memory attributes 7, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_IR7_WRITE_BACK       = (3 shl 14); {Inner Cacheable property mapping for memory attributes 7, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_IR6_NONCACHED        = (0 shl 12); {Inner Cacheable property mapping for memory attributes 6, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_IR6_WRITE_ALLOCATE   = (1 shl 12); {Inner Cacheable property mapping for memory attributes 6, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_IR6_WRITE_THROUGH    = (2 shl 12); {Inner Cacheable property mapping for memory attributes 6, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_IR6_WRITE_BACK       = (3 shl 12); {Inner Cacheable property mapping for memory attributes 6, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_IR5_NONCACHED        = (0 shl 10); {Inner Cacheable property mapping for memory attributes 5, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_IR5_WRITE_ALLOCATE   = (1 shl 10); {Inner Cacheable property mapping for memory attributes 5, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_IR5_WRITE_THROUGH    = (2 shl 10); {Inner Cacheable property mapping for memory attributes 5, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_IR5_WRITE_BACK       = (3 shl 10); {Inner Cacheable property mapping for memory attributes 5, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_IR4_NONCACHED        = (0 shl 8);  {Inner Cacheable property mapping for memory attributes 4, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_IR4_WRITE_ALLOCATE   = (1 shl 8);  {Inner Cacheable property mapping for memory attributes 4, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_IR4_WRITE_THROUGH    = (2 shl 8);  {Inner Cacheable property mapping for memory attributes 4, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_IR4_WRITE_BACK       = (3 shl 8);  {Inner Cacheable property mapping for memory attributes 4, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_IR3_NONCACHED        = (0 shl 6);  {Inner Cacheable property mapping for memory attributes 3, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_IR3_WRITE_ALLOCATE   = (1 shl 6);  {Inner Cacheable property mapping for memory attributes 3, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_IR3_WRITE_THROUGH    = (2 shl 6);  {Inner Cacheable property mapping for memory attributes 3, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_IR3_WRITE_BACK       = (3 shl 6);  {Inner Cacheable property mapping for memory attributes 3, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_IR2_NONCACHED        = (0 shl 4);  {Inner Cacheable property mapping for memory attributes 2, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_IR2_WRITE_ALLOCATE   = (1 shl 4);  {Inner Cacheable property mapping for memory attributes 2, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_IR2_WRITE_THROUGH    = (2 shl 4);  {Inner Cacheable property mapping for memory attributes 2, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_IR2_WRITE_BACK       = (3 shl 4);  {Inner Cacheable property mapping for memory attributes 2, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_IR1_NONCACHED        = (0 shl 2);  {Inner Cacheable property mapping for memory attributes 1, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_IR1_WRITE_ALLOCATE   = (1 shl 2);  {Inner Cacheable property mapping for memory attributes 1, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_IR1_WRITE_THROUGH    = (2 shl 2);  {Inner Cacheable property mapping for memory attributes 1, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_IR1_WRITE_BACK       = (3 shl 2);  {Inner Cacheable property mapping for memory attributes 1, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_IR0_NONCACHED        = (0 shl 0);  {Inner Cacheable property mapping for memory attributes 0, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_IR0_WRITE_ALLOCATE   = (1 shl 0);  {Inner Cacheable property mapping for memory attributes 0, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_IR0_WRITE_THROUGH    = (2 shl 0);  {Inner Cacheable property mapping for memory attributes 0, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 ARMV7_CP15_C10_NMRR_IR0_WRITE_BACK       = (3 shl 0);  {Inner Cacheable property mapping for memory attributes 0, if the region is mapped as Normal memory by the PRRR (The value of the TEX[0], C and B bits)}
 
                            {IR1 and OR1 are Non Cached}
 ARMV7_CP15_C10_NMRR_MASK = ARMV7_CP15_C10_NMRR_IR1_NONCACHED or ARMV7_CP15_C10_NMRR_OR1_NONCACHED
                            {IR2 and OR2 are Write Through}
                         or ARMV7_CP15_C10_NMRR_IR2_WRITE_THROUGH or ARMV7_CP15_C10_NMRR_OR2_WRITE_THROUGH
                            {IR3 and OR3 are Write Back}
                         or ARMV7_CP15_C10_NMRR_IR3_WRITE_BACK or ARMV7_CP15_C10_NMRR_OR3_WRITE_BACK
                            {IR7 and OR7 are Write Allocate}
                         or ARMV7_CP15_C10_NMRR_IR7_WRITE_ALLOCATE or ARMV7_CP15_C10_NMRR_OR7_WRITE_ALLOCATE;
 
 {Definitions of CP15 C11 bits in the system control processor registers}
 //To Do

 {Definitions of CP15 C12 bits in the system control processor registers}
 //To Do

 {Definitions of CP15 C13 bits in the system control processor registers}
 //To Do
 
 {Definitions of CP15 C14 (Generic Timer) bits in the system control processor registers}
 ARMV7_CP15_C14_CNT_CTL_ISTATUS = (1 shl 2); {The status of the timer (Read Only)(When set the timer condition is asserted)}
 ARMV7_CP15_C14_CNT_CTL_IMASK   = (1 shl 1); {Timer output signal mask bit (When set the timer output signal is masked)}
 ARMV7_CP15_C14_CNT_CTL_ENABLE  = (1 shl 0); {Enables the timer (When set the timer output signal is enabled)}
 
 {CP15 C14 Generic Timers}
 ARMV7_CP15_C14_CNTP = 0; {Physical Timer (Secure or Non Secure depending on the NS bit of the SCR)}
 ARMV7_CP15_C14_CNTV = 1; {Virtual Timer}
 ARMV7_CP15_C14_CNTH = 2; {Hypervisor Timer (Only available from HYP mode}
 
 {Definitions of CP15 C15 bits in the system control processor registers}
 //To Do
 
 {Definitions of bits in the Floating-point System ID register (FPSID)}
 //To Do
 
 {Definitions of bits in the Floating-point Status and Control (FPSCR)}
 //To Do
 
 {Definitions of bits in the Floating-point Exception register (FPEXC)}
 ARMV7_FPEXC_EN = (1 shl 30); {Floating-point system is enabled and operates normally if set to 1 (Default 0)}
 ARMV7_FPEXC_EX = (1 shl 31); {If EX is set to 0 then only FPSCR and FPEXC need to be preseved on a context switch (Default 0)}
 
 {Definitions of the Hardware Page Table Descriptors (See page B3-7 of the ARMv7 Architecture Reference Manual)}
 {Level One Descriptor (L1D) Types (See page B3-8 of the ARMv7 Architecture Reference Manual)}
 {Level One Page Table contains 4096 32bit (4 byte) entries for a total size of 16KB}
 ARMV7_L1D_TYPE_COARSE        = 1; {The entry points to a 1MB second-level page table. See page 6-40}
 ARMV7_L1D_TYPE_SECTION       = 2; {The entry points to a either a 1MB Section of memory or a 16MB Supersection of memory}
 ARMV7_L1D_TYPE_SUPERSECTION  = 2; {Bit[18] of the descriptor selects between a Section and a Supersection}
 
 {Level One Descriptor (L1D) Flags (See page B3-9 of the ARMv7 Architecture Reference Manual)}
 ARMV7_L1D_FLAG_COARSE_NS     = (1 shl 3);  {NS (Non Secure) Attribute bit to enable the support of TrustZone}
 ARMV7_L1D_FLAG_SECTION_NS    = (1 shl 19); {NS (Non Secure) Attribute bit to enable the support of TrustZone}
 ARMV7_L1D_FLAG_SUPERSECTION  = (1 shl 18); {The descriptor is a 16MB Supersection instead of a 1MB Section (Section Only)}
 ARMV7_L1D_FLAG_NOT_GLOBAL    = (1 shl 17); {The Not-Global (nG) bit, determines if the translation is marked as global (0), or process-specific (1) (Section Only)}
 ARMV7_L1D_FLAG_SHARED        = (1 shl 16); {The Shared (S) bit, determines if the translation is for Non-Shared (0), or Shared (1) memory. This only applies to Normal memory regions. 
                                             Device memory can be Shared or Non-Shared as determined by the TEX bits and the C and B bits (Section Only)}
 ARMV7_L1D_FLAG_AP2           = (1 shl 15); {The access permissions extension (AP2) bit, provides an extra access permission bit (Section Only)}
 ARMV7_L1D_FLAG_IMP           = (1 shl 9);  {The meaning of this bit is IMPLEMENTATION DEFINED}
 ARMV7_L1D_FLAG_XN            = (1 shl 4);  {The Execute-Never (XN) bit, determines if the region is Executable (0) or Not-executable(1) (Section Only)}
 ARMV7_L1D_FLAG_C             = (1 shl 3);  {Cacheable (C) bit (Section Only)}
 ARMV7_L1D_FLAG_B             = (1 shl 2);  {Bufferable (B) bit (Section Only)}
 
 {Level One Descriptor (L1D) Masks (See page B3-8 of the ARMv7 Architecture Reference Manual)}
 ARMV7_L1D_COARSE_BASE_MASK       = $FFFFFC00;
 ARMV7_L1D_SECTION_BASE_MASK      = $FFF00000;  
 ARMV7_L1D_SUPERSECTION_BASE_MASK = $FF000000;  
 ARMV7_L1D_DOMAIN_MASK            = ($F shl 5); {Security Domain of the Descriptor}
 ARMV7_L1D_TEX_MASK               = (7 shl 12); {Type extension field bits (Section Only)}  
 ARMV7_L1D_AP_MASK                = (3 shl 10); {Access permission bits (Section Only)}
 
 {Level One Descriptor (L1D) TEX Values (See page B3-32 of the ARMv7 Architecture Reference Manual) (Section Only)}
 ARMV7_L1D_TEX0 = (0 shl 12);
 ARMV7_L1D_TEX1 = (1 shl 12); 
 ARMV7_L1D_TEX2 = (2 shl 12);
 ARMV7_L1D_TEX4 = (4 shl 12); {Only used for Cacheable memory values}
 ARMV7_L1D_TEX5 = (5 shl 12); {Only used for Cacheable memory values}
 ARMV7_L1D_TEX6 = (6 shl 12); {Only used for Cacheable memory values} 
 ARMV7_L1D_TEX7 = (7 shl 12); {Only used for Cacheable memory values}
 
 {Level One Descriptor (L1D) AP Values (See page B3-28 of the ARMv7 Architecture Reference Manual) (Section Only)}
 ARMV7_L1D_AP0 = (0 shl 10);
 ARMV7_L1D_AP1 = (1 shl 10); 
 ARMV7_L1D_AP2 = (2 shl 10); 
 ARMV7_L1D_AP3 = (3 shl 10);

 {Level One Descriptor (L1D) Permission Values (See page B3-28 of the ARMv7 Architecture Reference Manual)}
 {This is not the full set of permissions as Ultibo always runs in priviledged mode}
 {The XN bit can also be applied to control whether memory regions are executable or not}
 ARMV7_L1D_ACCESS_NONE      = ARMV7_L1D_AP0;                        {No Access for both Privileged and Unprivileged code}
 ARMV7_L1D_ACCESS_READONLY  = ARMV7_L1D_FLAG_AP2 or ARMV7_L1D_AP3;  {Read-Only for both Privileged and Unprivileged code}
 ARMV7_L1D_ACCESS_READWRITE = ARMV7_L1D_AP3;                        {Read-Write for both Privileged and Unprivileged code}
 
 {Level One Descriptor (L1D) Cache Values (See page B3-32 of the ARMv7 Architecture Reference Manual)}
 ARMV7_L1D_CACHE_STRONGLY_ORDERED      = ARMV7_L1D_TEX0;                                         {Strongly Ordered. (Always Shared)}
 ARMV7_L1D_CACHE_SHARED_DEVICE         = ARMV7_L1D_TEX0 or ARMV7_L1D_FLAG_B;                     {Device. (Always Shared)}
 ARMV7_L1D_CACHE_NORMAL_WRITE_THROUGH  = ARMV7_L1D_TEX0 or ARMV7_L1D_FLAG_C;                     {Normal. Write Through (Shared if S bit set)}
 ARMV7_L1D_CACHE_NORMAL_WRITE_BACK     = ARMV7_L1D_TEX0 or ARMV7_L1D_FLAG_C or ARMV7_L1D_FLAG_B; {Normal. Write Back (Shared if S bit set)}
 ARMV7_L1D_CACHE_NORMAL_NONCACHED      = ARMV7_L1D_TEX1;                                         {Normal. Noncacheable (Shared if S bit set)}
 ARMV7_L1D_CACHE_NORMAL_WRITE_ALLOCATE = ARMV7_L1D_TEX1 or ARMV7_L1D_FLAG_C or ARMV7_L1D_FLAG_B; {Normal. Write Allocate (Shared if S bit set)}
 ARMV7_L1D_CACHE_NONSHARED_DEVICE      = ARMV7_L1D_TEX2;                                         {Device. (Not Shared}
 
 {Level One Descriptor (L1D) Cache Values (Cacheable Memory)(See page B3-34 of the ARMv7 Architecture Reference Manual)}
 ARMV7_L1D_CACHE_CACHEABLE_OUTER_NONCACHED      = ARMV7_L1D_TEX4; {Outer Normal Noncacheable (Shared if S bit set)}
 ARMV7_L1D_CACHE_CACHEABLE_OUTER_WRITE_ALLOCATE = ARMV7_L1D_TEX5; {Outer Normal Write Allocate (Shared if S bit set)}
 ARMV7_L1D_CACHE_CACHEABLE_OUTER_WRITE_THROUGH  = ARMV7_L1D_TEX6; {Outer Normal Write Through (Shared if S bit set)}
 ARMV7_L1D_CACHE_CACHEABLE_OUTER_WRITE_BACK     = ARMV7_L1D_TEX7; {Outer Normal Write Back (Shared if S bit set)}

 ARMV7_L1D_CACHE_CACHEABLE_INNER_NONCACHED      = ARMV7_L1D_TEX4;                                         {Inner Normal Noncacheable (Shared if S bit set)}
 ARMV7_L1D_CACHE_CACHEABLE_INNER_WRITE_ALLOCATE = ARMV7_L1D_TEX4 or ARMV7_L1D_FLAG_B;                     {Inner Normal Write Allocate (Shared if S bit set)}
 ARMV7_L1D_CACHE_CACHEABLE_INNER_WRITE_THROUGH  = ARMV7_L1D_TEX4 or ARMV7_L1D_FLAG_C;                     {Inner Normal Write Through (Shared if S bit set)}
 ARMV7_L1D_CACHE_CACHEABLE_INNER_WRITE_BACK     = ARMV7_L1D_TEX4 or ARMV7_L1D_FLAG_C or ARMV7_L1D_FLAG_B; {Inner Normal Write Back (Shared if S bit set)}
 
 {Level One Descriptor (L1D) Cache Values (TEX Remap Enabled)(See page B3-34 of the ARMv7 Architecture Reference Manual)(These values are from Linux)}
 ARMV7_L1D_CACHE_REMAP_STRONGLY_ORDERED      = ARMV7_L1D_TEX0;                                         {TR0 - Strongly Ordered}
 ARMV7_L1D_CACHE_REMAP_NORMAL_NONCACHED      = ARMV7_L1D_TEX0 or ARMV7_L1D_FLAG_B;                     {TR1 - Normal Noncacheable (Inner Shared if S bit set)}
 ARMV7_L1D_CACHE_REMAP_NORMAL_WRITE_THROUGH  = ARMV7_L1D_TEX0 or ARMV7_L1D_FLAG_C;                     {TR2 - Normal Write Through (Inner Shared if S bit set)}
 ARMV7_L1D_CACHE_REMAP_NORMAL_WRITE_BACK     = ARMV7_L1D_TEX0 or ARMV7_L1D_FLAG_C or ARMV7_L1D_FLAG_B; {TR3 - Normal Write Back (Inner Shared if S bit set)}
 ARMV7_L1D_CACHE_REMAP_DEVICE                = ARMV7_L1D_TEX1;                                         {TR4 - Device}
 ARMV7_L1D_CACHE_REMAP_UNUSED                = ARMV7_L1D_TEX1 or ARMV7_L1D_FLAG_B;                     {TR5 - Not currently used}
 ARMV7_L1D_CACHE_REMAP_RESERVED              = ARMV7_L1D_TEX1 or ARMV7_L1D_FLAG_C;                     {TR6 - Implementation Defined}
 ARMV7_L1D_CACHE_REMAP_NORMAL_WRITE_ALLOCATE = ARMV7_L1D_TEX1 or ARMV7_L1D_FLAG_C or ARMV7_L1D_FLAG_B; {TR7 - Normal Write Allocate (Inner Shared if S bit set)}
 
 {Level Two Descriptor (L2D) Types (See page B3-10 of the ARMv7 Architecture Reference Manual)}
 {Level Two Page Table contains 256 32bit (4 byte) entries for a total size of 1KB}
 ARMV7_L2D_TYPE_LARGE         = 1; {The entry points to a 64KB Large page in memory}
 ARMV7_L2D_TYPE_SMALL         = 2; {The entry points to a 4KB Extended small page in memory. Bit[0] of the entry is the XN (Execute Never) bit for the entry}
 
 {Level Two Descriptor (L2D) Flags (See page B3-10 of the ARMv7 Architecture Reference Manual)}
 ARMV7_L2D_FLAG_LARGE_XN      = (1 shl 15); {The Execute-Never (XN) bit, determines if the region is Executable (0) or Not-executable(1)}
 ARMV7_L2D_FLAG_SMALL_XN      = (1 shl 0);  {The Execute-Never (XN) bit, determines if the region is Executable (0) or Not-executable(1)}
 ARMV7_L2D_FLAG_NOT_GLOBAL    = (1 shl 11); {The Not-Global (nG) bit, determines if the translation is marked as global (0), or process-specific (1)}
 ARMV7_L2D_FLAG_SHARED        = (1 shl 10); {The Shared (S) bit, determines if the translation is for Non-Shared (0), or Shared (1) memory. This only applies to Normal memory regions. 
                                             Device memory can be Shared or Non-Shared as determined by the TEX bits and the C and B bits}
 ARMV7_L2D_FLAG_AP2           = (1 shl 9);  {The access permissions extension (APX) bit, provides an extra access permission bit}
 ARMV7_L2D_FLAG_C             = (1 shl 3);  {Cacheable (C) bit}
 ARMV7_L2D_FLAG_B             = (1 shl 2);  {Bufferable (B) bit}
 
 {Level Two Descriptor (L2D) Masks (See page B3-10 of the ARMv7 Architecture Reference Manual)}
 ARMV7_L2D_LARGE_BASE_MASK   = $FFFF0000;
 ARMV7_L2D_SMALL_BASE_MASK   = $FFFFF000;
 ARMV7_L2D_LARGE_TEX_MASK    = (7 shl 12); {Type extension field bits}          
 ARMV7_L2D_SMALL_TEX_MASK    = (7 shl 6);  {Type extension field bits}  
 ARMV7_L2D_AP_MASK           = (3 shl 4);  {Access permission bits}

 {Level Two Descriptor (L2D) Large TEX Values (See page B3-32 of the ARMv7 Architecture Reference Manual)}
 ARMV7_L2D_LARGE_TEX0 = (0 shl 12);
 ARMV7_L2D_LARGE_TEX1 = (1 shl 12); 
 ARMV7_L2D_LARGE_TEX2 = (2 shl 12); 
 ARMV7_L2D_LARGE_TEX4 = (4 shl 12); 
 ARMV7_L2D_LARGE_TEX5 = (5 shl 12); {Only used for Cacheable memory values}
 ARMV7_L2D_LARGE_TEX6 = (6 shl 12); {Only used for Cacheable memory values} 
 ARMV7_L2D_LARGE_TEX7 = (7 shl 12); {Only used for Cacheable memory values}

 {Level Two Descriptor (L2D) Small TEX Values (See page B3-32 of the ARMv7 Architecture Reference Manual)}
 ARMV7_L2D_SMALL_TEX0 = (0 shl 6);
 ARMV7_L2D_SMALL_TEX1 = (1 shl 6); 
 ARMV7_L2D_SMALL_TEX2 = (2 shl 6); 
 ARMV7_L2D_SMALL_TEX4 = (4 shl 6); 
 ARMV7_L2D_SMALL_TEX5 = (5 shl 6); {Only used for Cacheable memory values}
 ARMV7_L2D_SMALL_TEX6 = (6 shl 6); {Only used for Cacheable memory values} 
 ARMV7_L2D_SMALL_TEX7 = (7 shl 6); {Only used for Cacheable memory values}
 
 {Level Two Descriptor (L2D) AP Values (See page B3-28 of the ARMv7 Architecture Reference Manual)}
 ARMV7_L2D_AP0 = (0 shl 4);
 ARMV7_L2D_AP1 = (1 shl 4); 
 ARMV7_L2D_AP2 = (2 shl 4); 
 ARMV7_L2D_AP3 = (3 shl 4);
 
 {Level Two Descriptor (L2D) Permission Values (See page B3-28 of the ARMv7 Architecture Reference Manual)}
 {This is not the full set of permissions as Ultibo always runs in priviledged mode}
 {The XN bit can also be applied to control whether memory regions are executable or not}
 ARMV7_L2D_ACCESS_NONE      = ARMV7_L2D_AP0;                        {No Access for both Privileged and Unprivileged code}
 ARMV7_L2D_ACCESS_READONLY  = ARMV7_L2D_FLAG_AP2 or ARMV7_L2D_AP3;  {Read-Only for both Privileged and Unprivileged code}
 ARMV7_L2D_ACCESS_READWRITE = ARMV7_L2D_AP3;                        {Read-Write for both Privileged and Unprivileged code}
 
 {Level Two Descriptor (L2D) Large Cache Values (See page B3-32 of the ARMv7 Architecture Reference Manual)}
 ARMV7_L2D_LARGE_CACHE_STRONGLY_ORDERED      = ARMV7_L2D_LARGE_TEX0;                                         {Strongly Ordered. (Always Shared)}
 ARMV7_L2D_LARGE_CACHE_SHARED_DEVICE         = ARMV7_L2D_LARGE_TEX0 or ARMV7_L2D_FLAG_B;                     {Device. (Always Shared)}
 ARMV7_L2D_LARGE_CACHE_NORMAL_WRITE_THROUGH  = ARMV7_L2D_LARGE_TEX0 or ARMV7_L2D_FLAG_C;                     {Normal. Write Through (Shared if S bit set)}
 ARMV7_L2D_LARGE_CACHE_NORMAL_WRITE_BACK     = ARMV7_L2D_LARGE_TEX0 or ARMV7_L2D_FLAG_C or ARMV7_L2D_FLAG_B; {Normal. Write Back (Shared if S bit set)}
 ARMV7_L2D_LARGE_CACHE_NORMAL_NONCACHED      = ARMV7_L2D_LARGE_TEX1;                                         {Normal. Noncacheable (Shared if S bit set)}
 ARMV7_L2D_LARGE_CACHE_NORMAL_WRITE_ALLOCATE = ARMV7_L2D_LARGE_TEX1 or ARMV7_L2D_FLAG_C or ARMV7_L2D_FLAG_B; {Normal. Write Allocate (Shared if S bit set)}
 ARMV7_L2D_LARGE_CACHE_NONSHARED_DEVICE      = ARMV7_L2D_LARGE_TEX2;                                         {Device. (Not Shared}
 
 {Level Two Descriptor (L2D) Large Cache Values (Cacheable Memory)(See page B3-32 of the ARMv7 Architecture Reference Manual)}
 ARMV7_L2D_LARGE_CACHE_CACHEABLE_OUTER_NONCACHED      = ARMV7_L2D_LARGE_TEX4; {Outer Normal Noncacheable (Shared if S bit set)}
 ARMV7_L2D_LARGE_CACHE_CACHEABLE_OUTER_WRITE_ALLOCATE = ARMV7_L2D_LARGE_TEX5; {Outer Normal Write Allocate (Shared if S bit set)}
 ARMV7_L2D_LARGE_CACHE_CACHEABLE_OUTER_WRITE_THROUGH  = ARMV7_L2D_LARGE_TEX6; {Outer Normal Write Through (Shared if S bit set)}
 ARMV7_L2D_LARGE_CACHE_CACHEABLE_OUTER_WRITE_BACK     = ARMV7_L2D_LARGE_TEX7; {Outer Normal Write Back (Shared if S bit set)}

 ARMV7_L2D_LARGE_CACHE_CACHEABLE_INNER_NONCACHED      = ARMV7_L2D_LARGE_TEX4;                                         {Inner Normal Noncacheable (Shared if S bit set)}
 ARMV7_L2D_LARGE_CACHE_CACHEABLE_INNER_WRITE_ALLOCATE = ARMV7_L2D_LARGE_TEX4 or ARMV7_L2D_FLAG_B;                     {Inner Normal Write Allocate (Shared if S bit set)}
 ARMV7_L2D_LARGE_CACHE_CACHEABLE_INNER_WRITE_THROUGH  = ARMV7_L2D_LARGE_TEX4 or ARMV7_L2D_FLAG_C;                     {Inner Normal Write Through (Shared if S bit set)}
 ARMV7_L2D_LARGE_CACHE_CACHEABLE_INNER_WRITE_BACK     = ARMV7_L2D_LARGE_TEX4 or ARMV7_L2D_FLAG_C or ARMV7_L2D_FLAG_B; {Inner Normal Write Back (Shared if S bit set)}

 {Level Two Descriptor (L2D) Large Cache Values (TEX Remap Enabled)(See page B3-32 of the ARMv7 Architecture Reference Manual)(These values are from Linux)}
 ARMV7_L2D_LARGE_CACHE_REMAP_STRONGLY_ORDERED      = ARMV7_L2D_LARGE_TEX0;                                         {TR0 - Strongly Ordered}
 ARMV7_L2D_LARGE_CACHE_REMAP_NORMAL_NONCACHED      = ARMV7_L2D_LARGE_TEX0 or ARMV7_L2D_FLAG_B;                     {TR1 - Normal Noncacheable (Inner Shared if S bit set)}
 ARMV7_L2D_LARGE_CACHE_REMAP_NORMAL_WRITE_THROUGH  = ARMV7_L2D_LARGE_TEX0 or ARMV7_L2D_FLAG_C;                     {TR2 - Normal Write Through (Inner Shared if S bit set)}
 ARMV7_L2D_LARGE_CACHE_REMAP_NORMAL_WRITE_BACK     = ARMV7_L2D_LARGE_TEX0 or ARMV7_L2D_FLAG_C or ARMV7_L2D_FLAG_B; {TR3 - Normal Write Back (Inner Shared if S bit set)}
 ARMV7_L2D_LARGE_CACHE_REMAP_DEVICE                = ARMV7_L2D_LARGE_TEX1;                                         {TR4 - Device}
 ARMV7_L2D_LARGE_CACHE_REMAP_UNUSED                = ARMV7_L2D_LARGE_TEX1 or ARMV7_L2D_FLAG_B;                     {TR5 - Not currently used}
 ARMV7_L2D_LARGE_CACHE_REMAP_RESERVED              = ARMV7_L2D_LARGE_TEX1 or ARMV7_L2D_FLAG_C;                     {TR6 - Implementation Defined}
 ARMV7_L2D_LARGE_CACHE_REMAP_NORMAL_WRITE_ALLOCATE = ARMV7_L2D_LARGE_TEX1 or ARMV7_L2D_FLAG_C or ARMV7_L2D_FLAG_B; {TR7 - Normal Write Allocate (Inner Shared if S bit set)}
 
 {Level Two Descriptor (L2D) Small Cache Values (See page B3-32 of the ARMv7 Architecture Reference Manual)}
 ARMV7_L2D_SMALL_CACHE_STRONGLY_ORDERED      = ARMV7_L2D_SMALL_TEX0;                                         {Strongly Ordered. (Always Shared)}
 ARMV7_L2D_SMALL_CACHE_SHARED_DEVICE         = ARMV7_L2D_SMALL_TEX0 or ARMV7_L2D_FLAG_B;                     {Device. (Always Shared)}
 ARMV7_L2D_SMALL_CACHE_NORMAL_WRITE_THROUGH  = ARMV7_L2D_SMALL_TEX0 or ARMV7_L2D_FLAG_C;                     {Normal. Write Through (Shared if S bit set)}
 ARMV7_L2D_SMALL_CACHE_NORMAL_WRITE_BACK     = ARMV7_L2D_SMALL_TEX0 or ARMV7_L2D_FLAG_C or ARMV7_L2D_FLAG_B; {Normal. Write Back (Shared if S bit set)}
 ARMV7_L2D_SMALL_CACHE_NORMAL_NONCACHED      = ARMV7_L2D_SMALL_TEX1;                                         {Normal. Noncacheable (Shared if S bit set)}
 ARMV7_L2D_SMALL_CACHE_NORMAL_WRITE_ALLOCATE = ARMV7_L2D_SMALL_TEX1 or ARMV7_L2D_FLAG_C or ARMV7_L2D_FLAG_B; {Normal. Write Allocate (Shared if S bit set)}
 ARMV7_L2D_SMALL_CACHE_NONSHARED_DEVICE      = ARMV7_L2D_SMALL_TEX2;                                         {Device. (Not Shared}
 
 {Level Two Descriptor (L2D) Small Cache Values (Cacheable Memory)(See page B3-32 of the ARMv7 Architecture Reference Manual)}
 ARMV7_L2D_SMALL_CACHE_CACHEABLE_OUTER_NONCACHED      = ARMV7_L2D_SMALL_TEX4; {Outer Normal Noncacheable (Shared if S bit set)}
 ARMV7_L2D_SMALL_CACHE_CACHEABLE_OUTER_WRITE_ALLOCATE = ARMV7_L2D_SMALL_TEX5; {Outer Normal Write Allocate (Shared if S bit set)}
 ARMV7_L2D_SMALL_CACHE_CACHEABLE_OUTER_WRITE_THROUGH  = ARMV7_L2D_SMALL_TEX6; {Outer Normal Write Through (Shared if S bit set)}
 ARMV7_L2D_SMALL_CACHE_CACHEABLE_OUTER_WRITE_BACK     = ARMV7_L2D_SMALL_TEX7; {Outer Normal Write Back (Shared if S bit set)}

 ARMV7_L2D_SMALL_CACHE_CACHEABLE_INNER_NONCACHED      = ARMV7_L2D_SMALL_TEX4;                                         {Inner Normal Noncacheable (Shared if S bit set)}
 ARMV7_L2D_SMALL_CACHE_CACHEABLE_INNER_WRITE_ALLOCATE = ARMV7_L2D_SMALL_TEX4 or ARMV7_L2D_FLAG_B;                     {Inner Normal Write Allocate (Shared if S bit set)}
 ARMV7_L2D_SMALL_CACHE_CACHEABLE_INNER_WRITE_THROUGH  = ARMV7_L2D_SMALL_TEX4 or ARMV7_L2D_FLAG_C;                     {Inner Normal Write Through (Shared if S bit set)}
 ARMV7_L2D_SMALL_CACHE_CACHEABLE_INNER_WRITE_BACK     = ARMV7_L2D_SMALL_TEX4 or ARMV7_L2D_FLAG_C or ARMV7_L2D_FLAG_B; {Inner Normal Write Back (Shared if S bit set)}

 {Level Two Descriptor (L2D) Small Cache Values (TEX Remap Enabled)(See page B3-32 of the ARMv7 Architecture Reference Manual)(These values are from Linux)}
 ARMV7_L2D_SMALL_CACHE_REMAP_STRONGLY_ORDERED      = ARMV7_L2D_SMALL_TEX0;                                         {TR0 - Strongly Ordered}
 ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_NONCACHED      = ARMV7_L2D_SMALL_TEX0 or ARMV7_L2D_FLAG_B;                     {TR1 - Normal Noncacheable (Inner Shared if S bit set)}
 ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_THROUGH  = ARMV7_L2D_SMALL_TEX0 or ARMV7_L2D_FLAG_C;                     {TR2 - Normal Write Through (Inner Shared if S bit set)}
 ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_BACK     = ARMV7_L2D_SMALL_TEX0 or ARMV7_L2D_FLAG_C or ARMV7_L2D_FLAG_B; {TR3 - Normal Write Back (Inner Shared if S bit set)}
 ARMV7_L2D_SMALL_CACHE_REMAP_DEVICE                = ARMV7_L2D_SMALL_TEX1;                                         {TR4 - Device}
 ARMV7_L2D_SMALL_CACHE_REMAP_UNUSED                = ARMV7_L2D_SMALL_TEX1 or ARMV7_L2D_FLAG_B;                     {TR5 - Not currently used}
 ARMV7_L2D_SMALL_CACHE_REMAP_RESERVED              = ARMV7_L2D_SMALL_TEX1 or ARMV7_L2D_FLAG_C;                     {TR6 - Implementation Defined}
 ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_ALLOCATE = ARMV7_L2D_SMALL_TEX1 or ARMV7_L2D_FLAG_C or ARMV7_L2D_FLAG_B; {TR7 - Normal Write Allocate (Inner Shared if S bit set)}
 
 
{==============================================================================}
const
 {ARMv7 specific constants}
 
 {Length of ARM context switch record in 32 bit words (includes fpexc, fpscr, d0-d15, r0-r12, lr, pc, cpsr)}
 ARMV7_CONTEXT_LENGTH = 50;  //To Do //Critical //To change for VFPV3 d16-d31
 
{==============================================================================}
type
 {ARMv7 specific types}
 
 {Prototypes for Page Table Handlers}
 TARMv7PageTableInit = procedure;

 {Prototypes for IRQ Handlers}
 TARMv7DispatchIRQ = function(CPUID:LongWord;Thread:TThreadHandle):TThreadHandle;
 
 {Prototypes for FIQ Handlers}
 TARMv7DispatchFIQ = function(CPUID:LongWord;Thread:TThreadHandle):TThreadHandle; 
 
 {Prototypes for SWI Handlers}
 TARMv7DispatchSWI = function(CPUID:LongWord;Thread:TThreadHandle;Request:PSystemCallRequest):TThreadHandle; 
 
{==============================================================================}
var
 {ARMv7 specific variables}
 ARMv7Initialized:Boolean;
 
var
 {Page Table Handlers}
 ARMv7PageTableInitHandler:TARMv7PageTableInit;
 
var
 {IRQ Handlers}
 ARMv7DispatchIRQHandler:TARMv7DispatchIRQ;
 
var
 {FIQ Handlers}
 ARMv7DispatchFIQHandler:TARMv7DispatchFIQ;
 
var
 {SWI Handlers}
 ARMv7DispatchSWIHandler:TARMv7DispatchSWI;
 
{==============================================================================}
{Initialization Functions}
procedure ARMv7Init;
  
{==============================================================================}
{ARMv7 Platform Functions}
procedure ARMv7CPUInit;
procedure ARMv7FPUInit;
procedure ARMv7MMUInit;

procedure ARMv7CacheInit;

procedure ARMv7TimerInit(Frequency:LongWord);

procedure ARMv7PageTableInit;

procedure ARMv7SystemCall(Number:LongWord;Param1,Param2,Param3:PtrUInt);

function ARMv7CPUGetMode:LongWord;
function ARMv7CPUGetState:LongWord;
function ARMv7CPUGetCurrent:LongWord;

function ARMv7CPUGetMainID:LongWord;
function ARMv7CPUGetMultiprocessorID:LongWord;
function ARMv7CPUGetModel:LongWord;
function ARMv7CPUGetRevision:LongWord;
function ARMv7CPUGetDescription:String;

function ARMv7FPUGetState:LongWord;

function ARMv7L1CacheGetType:LongWord; 
function ARMv7L1DataCacheGetSize:LongWord; 
function ARMv7L1DataCacheGetLineSize:LongWord;  
function ARMv7L1InstructionCacheGetSize:LongWord;
function ARMv7L1InstructionCacheGetLineSize:LongWord;  

function ARMv7L2CacheGetType:LongWord;
function ARMv7L2CacheGetSize:LongWord;
function ARMv7L2CacheGetLineSize:LongWord;  

procedure ARMv7Halt;
procedure ARMv7Pause;

procedure ARMv7SendEvent;
procedure ARMv7WaitForEvent; 
procedure ARMv7WaitForInterrupt; 

procedure ARMv7DataMemoryBarrier;
procedure ARMv7DataSynchronizationBarrier;
procedure ARMv7InstructionMemoryBarrier;

procedure ARMv7InvalidateTLB;
procedure ARMv7InvalidateDataTLB;
procedure ARMv7InvalidateInstructionTLB;

procedure ARMv7InvalidateCache;
procedure ARMv7CleanDataCache;
procedure ARMv7InvalidateDataCache;
procedure ARMv7InvalidateL1DataCache;
procedure ARMv7CleanAndInvalidateDataCache;
procedure ARMv7InvalidateInstructionCache;

procedure ARMv7CleanDataCacheRange(Address,Size:LongWord); 
procedure ARMv7InvalidateDataCacheRange(Address,Size:LongWord);
procedure ARMv7CleanAndInvalidateDataCacheRange(Address,Size:LongWord);
procedure ARMv7InvalidateInstructionCacheRange(Address,Size:LongWord); 

procedure ARMv7CleanDataCacheSetWay(SetWay:LongWord);
procedure ARMv7InvalidateDataCacheSetWay(SetWay:LongWord);
procedure ARMv7CleanAndInvalidateDataCacheSetWay(SetWay:LongWord);

procedure ARMv7FlushPrefetchBuffer;

procedure ARMv7FlushBranchTargetCache;

procedure ARMv7ContextSwitch(OldStack,NewStack:Pointer;NewThread:TThreadHandle); 
procedure ARMv7ContextSwitchIRQ(OldStack,NewStack:Pointer;NewThread:TThreadHandle); 
procedure ARMv7ContextSwitchFIQ(OldStack,NewStack:Pointer;NewThread:TThreadHandle); 
procedure ARMv7ContextSwitchSWI(OldStack,NewStack:Pointer;NewThread:TThreadHandle); 

function ARMv7InterlockedOr(var Target:LongInt;Value:LongInt):LongInt;
function ARMv7InterlockedXor(var Target:LongInt;Value:LongInt):LongInt;
function ARMv7InterlockedAnd(var Target:LongInt;Value:LongInt):LongInt;

function ARMv7InterlockedDecrement(var Target:LongInt):LongInt;
function ARMv7InterlockedIncrement(var Target:LongInt):LongInt; 
function ARMv7InterlockedExchange(var Target:LongInt;Source:LongInt):LongInt; 
function ARMv7InterlockedAddExchange(var Target:LongInt;Source:LongInt):LongInt; 
function ARMv7InterlockedCompareExchange(var Target:LongInt;Source,Compare:LongInt):LongInt;

function ARMv7PageTableGetEntry(Address:PtrUInt):TPageTableEntry;
function ARMv7PageTableSetEntry(const Entry:TPageTableEntry):LongWord; 

function ARMv7VectorTableGetEntry(Number:LongWord):PtrUInt;
function ARMv7VectorTableSetEntry(Number:LongWord;Address:PtrUInt):LongWord;

function ARMv7FirstBitSet(Value:LongWord):LongWord;
function ARMv7CountLeadingZeros(Value:LongWord):LongWord;

{==============================================================================}
{ARMv7 Thread Functions}
procedure ARMv7PrimaryInit;

function ARMv7SpinLock(Spin:PSpinEntry):LongWord;
function ARMv7SpinUnlock(Spin:PSpinEntry):LongWord;

function ARMv7SpinLockIRQ(Spin:PSpinEntry):LongWord;
function ARMv7SpinUnlockIRQ(Spin:PSpinEntry):LongWord;

function ARMv7SpinLockFIQ(Spin:PSpinEntry):LongWord;
function ARMv7SpinUnlockFIQ(Spin:PSpinEntry):LongWord;

function ARMv7SpinLockIRQFIQ(Spin:PSpinEntry):LongWord;
function ARMv7SpinUnlockIRQFIQ(Spin:PSpinEntry):LongWord;

function ARMv7SpinCheckIRQ(Spin:PSpinEntry):Boolean;
function ARMv7SpinCheckFIQ(Spin:PSpinEntry):Boolean;
 
function ARMv7SpinExchangeIRQ(Spin1,Spin2:PSpinEntry):LongWord;
function ARMv7SpinExchangeFIQ(Spin1,Spin2:PSpinEntry):LongWord;

function ARMv7MutexLock(Mutex:PMutexEntry):LongWord;
function ARMv7MutexUnlock(Mutex:PMutexEntry):LongWord;
function ARMv7MutexTryLock(Mutex:PMutexEntry):LongWord;

function ARMv7ThreadGetCurrent:TThreadHandle; 
function ARMv7ThreadSetCurrent(Thread:TThreadHandle):LongWord;

function ARMv7ThreadSetupStack(StackBase:Pointer;StartProc:TThreadStart;ReturnProc:TThreadEnd;Parameter:Pointer):Pointer;

{==============================================================================}
{ARMv7 IRQ Functions}
function ARMv7DispatchIRQ(CPUID:LongWord;Thread:TThreadHandle):TThreadHandle; inline;

{==============================================================================}
{ARMv7 FIQ Functions}
function ARMv7DispatchFIQ(CPUID:LongWord;Thread:TThreadHandle):TThreadHandle; inline;

{==============================================================================}
{ARMv7 SWI Functions}
function ARMv7DispatchSWI(CPUID:LongWord;Thread:TThreadHandle;Request:PSystemCallRequest):TThreadHandle; inline;

{==============================================================================}
{ARMv7 Interrupt Functions}
procedure ARMv7ResetHandler;     
procedure ARMv7UndefinedInstructionHandler;     
procedure ARMv7SoftwareInterruptHandler;       
procedure ARMv7PrefetchAbortHandler;  
procedure ARMv7DataAbortHandler;
procedure ARMv7ReservedHandler;  
procedure ARMv7IRQHandler;
procedure ARMv7FIQHandler;

{==============================================================================}
{ARMv7 Helper Functions}
function ARMv7GetFPEXC:LongWord;
function ARMv7GetFPSCR:LongWord;

procedure ARMv7StartMMU;

function ARMv7GetTimerState(Timer:LongWord):LongWord;
procedure ARMv7SetTimerState(Timer,State:LongWord);

function ARMv7GetTimerCount(Timer:LongWord):Int64;

function ARMv7GetTimerValue(Timer:LongWord):LongWord;
procedure ARMV7SetTimerValue(Timer,Value:LongWord);

function ARMv7GetTimerCompare(Timer:LongWord):Int64;
procedure ARMV7SetTimerCompare(Timer,High,Low:LongWord);

function ARMv7GetTimerFrequency:LongWord;

function ARMv7GetPageTableCoarse(Address:PtrUInt):LongWord;
function ARMv7SetPageTableCoarse(Address,CoarseAddress:PtrUInt;Flags:Word):Boolean;

function ARMv7GetPageTableLarge(Address:PtrUInt):LongWord;
function ARMv7SetPageTableLarge(Address,PhysicalAddress:PtrUInt;Flags:Word):Boolean;

function ARMv7GetPageTableSmall(Address:PtrUInt):LongWord;
function ARMv7SetPageTableSmall(Address,PhysicalAddress:PtrUInt;Flags:Word):Boolean;

function ARMv7GetPageTableSection(Address:PtrUInt):LongWord;
function ARMv7SetPageTableSection(Address,PhysicalAddress:PtrUInt;Flags:LongWord):Boolean;
function ARMv7SetPageTableSupersection(Address,PhysicalAddress:PtrUInt;Flags:LongWord):Boolean;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure ARMv7Init;
begin
 {}
 if ARMv7Initialized then Exit;
 
 {Setup PAGE_TABLES_SHIFT}
 PAGE_TABLES_SHIFT:=ARMV7_PAGE_TABLES_SHIFT;
 
 {Setup SPIN_SHARED_MEMORY} 
 SPIN_SHARED_MEMORY:=False;
 if not(HEAP_NORMAL_SHARED) and (CPUGetCount > 1) then SPIN_SHARED_MEMORY:=True;
 
 {Setup MUTEX_SHARED_MEMORY} 
 MUTEX_SHARED_MEMORY:=False;
 if not(HEAP_NORMAL_SHARED) and (CPUGetCount > 1) then MUTEX_SHARED_MEMORY:=True;
 
 {Setup HEAP_REQUEST_ALIGNMENT}
 HEAP_REQUEST_ALIGNMENT:=SIZE_1M;
 
 {Register Platform CPUInit Handler}
 CPUInitHandler:=ARMv7CPUInit;
 
 {Register Platform FPUInit Handler}
 FPUInitHandler:=ARMv7FPUInit;

 {Register Platform MMUInit Handler}
 MMUInitHandler:=ARMv7MMUInit;

 {Register Platform CacheInit Handler}
 CacheInitHandler:=ARMv7CacheInit;

 {Register Platform System Handlers}
 SystemCallHandler:=ARMv7SystemCall;
 
 {Register Platform CPU Handlers}
 CPUGetModeHandler:=ARMv7CPUGetMode;
 CPUGetStateHandler:=ARMv7CPUGetState;
 CPUGetCurrentHandler:=ARMv7CPUGetCurrent;
 CPUGetModelHandler:=ARMv7CPUGetModel;
 CPUGetRevisionHandler:=ARMv7CPUGetRevision;
 CPUGetDescriptionHandler:=ARMv7CPUGetDescription;
 
 {Register Platform FPU Handlers}
 FPUGetStateHandler:=ARMv7FPUGetState;
 
 {Register Platform Cache Handlers}
 L1CacheGetTypeHandler:=ARMv7L1CacheGetType;
 L1DataCacheGetSizeHandler:=ARMv7L1DataCacheGetSize;
 L1DataCacheGetLineSizeHandler:=ARMv7L1DataCacheGetLineSize;
 L1InstructionCacheGetSizeHandler:=ARMv7L1InstructionCacheGetSize;
 L1InstructionCacheGetLineSizeHandler:=ARMv7L1InstructionCacheGetLineSize;
 
 L2CacheGetTypeHandler:=ARMv7L2CacheGetType;
 L2CacheGetSizeHandler:=ARMv7L2CacheGetSize;
 L2CacheGetLineSizeHandler:=ARMv7L2CacheGetLineSize;
 
 {Register Platform Halt Handler}
 HaltHandler:=ARMv7Halt;
 
 {Register Platform Pause Handler}
 PauseHandler:=ARMv7Pause;

 {Register Platform SendEvent/WaitForEvent/Interrupt Handlers}
 SendEventHandler:=ARMv7SendEvent;
 WaitForEventHandler:=ARMv7WaitForEvent; 
 WaitForInterruptHandler:=ARMv7WaitForInterrupt;
 
 {Register Platform Barrier Handlers}
 ReadMemoryBarrierHandler:=ARMv7DataMemoryBarrier;
 WriteMemoryBarrierHandler:=ARMv7DataMemoryBarrier;
 DataMemoryBarrierHandler:=ARMv7DataMemoryBarrier;
 DataSynchronizationBarrierHandler:=ARMv7DataSynchronizationBarrier;
 InstructionMemoryBarrierHandler:=ARMv7InstructionMemoryBarrier;

 {Register Platform TLB Handlers}
 InvalidateTLBHandler:=ARMv7InvalidateTLB;
 InvalidateDataTLBHandler:=ARMv7InvalidateDataTLB;
 InvalidateInstructionTLBHandler:=ARMv7InvalidateInstructionTLB;
 
 {Register Platform Cache Handlers}
 InvalidateCacheHandler:=ARMv7InvalidateCache;
 CleanDataCacheHandler:=ARMv7CleanDataCache;
 InvalidateDataCacheHandler:=ARMv7InvalidateDataCache;
 CleanAndInvalidateDataCacheHandler:=ARMv7CleanAndInvalidateDataCache;
 InvalidateInstructionCacheHandler:=ARMv7InvalidateInstructionCache;
 
 CleanDataCacheRangeHandler:=ARMv7CleanDataCacheRange;
 InvalidateDataCacheRangeHandler:=ARMv7InvalidateDataCacheRange;
 CleanAndInvalidateDataCacheRangeHandler:=ARMv7CleanAndInvalidateDataCacheRange;
 InvalidateInstructionCacheRangeHandler:=ARMv7InvalidateInstructionCacheRange;
 
 {Register Platform PrefetchBuffer Handlers}
 FlushPrefetchBufferHandler:=ARMv7FlushPrefetchBuffer;

 {Register Platform BranchTargetCache Handlers}
 FlushBranchTargetCacheHandler:=ARMv7FlushBranchTargetCache;
 
 {Register Platform ContextSwitch Handlers}
 ContextSwitchHandler:=ARMv7ContextSwitch;
 ContextSwitchIRQHandler:=ARMv7ContextSwitchIRQ; 
 ContextSwitchFIQHandler:=ARMv7ContextSwitchFIQ; 
 ContextSwitchSWIHandler:=ARMv7ContextSwitchSWI; 
 
 {Register Platform And/Xor/Or/Increment/Decrement/Exchange Handlers}
 InterlockedOrHandler:=ARMv7InterlockedOr;
 InterlockedXorHandler:=ARMv7InterlockedXor;
 InterlockedAndHandler:=ARMv7InterlockedAnd;
 
 InterlockedDecrementHandler:=ARMv7InterlockedDecrement;
 InterlockedIncrementHandler:= ARMv7InterlockedIncrement;
 InterlockedExchangeHandler:=ARMv7InterlockedExchange;
 InterlockedAddExchangeHandler:=ARMv7InterlockedAddExchange;
 InterlockedCompareExchangeHandler:=ARMv7InterlockedCompareExchange;
 
 {Register Platform PageTable Handlers}
 PageTableGetEntryHandler:=ARMv7PageTableGetEntry;
 PageTableSetEntryHandler:=ARMv7PageTableSetEntry;
 
 {Register Platform VectorTable Handlers}
 VectorTableGetEntryHandler:=ARMv7VectorTableGetEntry;
 VectorTableSetEntryHandler:=ARMv7VectorTableSetEntry;
 
 {Register Platform FirstBitSet Handler}
 FirstBitSetHandler:=ARMv7FirstBitSet;
 
 {Register Platform CountLeadingZeros Handler}
 CountLeadingZerosHandler:=ARMv7CountLeadingZeros;
 
 {Register Threads PrimaryInit Handler}
 PrimaryInitHandler:=ARMv7PrimaryInit;
 
 {Register Threads SpinLock/Unlock Handlers}
 SpinLockHandler:=ARMv7SpinLock;
 SpinUnlockHandler:=ARMv7SpinUnlock;

 SpinLockIRQHandler:=ARMv7SpinLockIRQ;
 SpinUnlockIRQHandler:=ARMv7SpinUnlockIRQ;

 SpinLockFIQHandler:=ARMv7SpinLockFIQ;
 SpinUnlockFIQHandler:=ARMv7SpinUnlockFIQ;
  
 SpinLockIRQFIQHandler:=ARMv7SpinLockIRQFIQ;
 SpinUnlockIRQFIQHandler:=ARMv7SpinUnlockIRQFIQ;

 SpinCheckIRQHandler:=ARMv7SpinCheckIRQ;
 SpinCheckFIQHandler:=ARMv7SpinCheckFIQ;
 
 SpinExchangeIRQHandler:=ARMv7SpinExchangeIRQ;
 SpinExchangeFIQHandler:=ARMv7SpinExchangeFIQ;
  
 {Register Threads MutexLock/Unlock Handlers}
 MutexLockHandler:=ARMv7MutexLock; 
 MutexUnlockHandler:=ARMv7MutexUnlock;
 MutexTryLockHandler:=ARMv7MutexTryLock;
  
 {Register Threads ThreadGet/SetCurrent Handler}
 ThreadGetCurrentHandler:=ARMv7ThreadGetCurrent;
 ThreadSetCurrentHandler:=ARMv7ThreadSetCurrent;

 {Register Threads ThreadSetupStack Handler}
 ThreadSetupStackHandler:=ARMv7ThreadSetupStack;
 
 ARMv7Initialized:=True;
end;

{==============================================================================}
{==============================================================================}
{ARMv7 Platform Functions}
procedure ARMv7CPUInit; assembler; nostackframe;
asm
 //Load the c13 (Thread and process ID) register in system control coprocessor CP15 with INVALID_HANDLE_VALUE.
 //See page ???
 mov r0, #INVALID_HANDLE_VALUE
 mcr p15, #0, r0, cr13, cr0, #4
 
 //Enable L1 Instruction Caching and Branch Prediction by setting the I and Z bits in the C1 control register.
 //Also set the U bit to enable unaligned data access which may have already been set by the startup handler. 
 //See page ???
 mrc p15, #0, r12, cr1, cr0, #0;
 orr r12, r12, #ARMV7_CP15_C1_I_BIT
 orr r12, r12, #ARMV7_CP15_C1_Z_BIT
 //orr r12, r12, #ARMV7_CP15_C1_U_BIT (Always enabled in ARMv7)
 mcr p15, #0, r12, cr1, cr0, #0;
 
 //Enable coherent processor requests by setting the SMP bit in the C1 auxiliary control register.
 //See page 4-59 of the Cortex-A7 MPCore Technical Reference Manual.
 mrc p15, #0, r12, cr1, cr0, #1;
 orr r12, r12, #ARMV7_CP15_C1_AUX_SMP
 mcr p15, #0, r12, cr1, cr0, #1;
 
 //Perform an Instruction Synchronization Barrier (ISB) operation immediately after the change above.
 //See page A8-102  of the ARMv7 Architecture Reference Manual
 //ARMv7 "instruction synchronization barrier" instruction.
 isb
end;

{==============================================================================}

procedure ARMv7FPUInit; assembler; nostackframe;
asm
 //Enable access to Coprocessor 10 and 11 in the C1 coprocessor access control register.
 //See page ???
 mrc p15, #0, r12, cr1, cr0, #2
 orr r12, r12, #ARMV7_CP15_C1_CP10_SYS
 orr r12, r12, #ARMV7_CP15_C1_CP11_SYS 
 mcr p15, #0, r12, cr1, cr0, #2
 
 //Perform an Instruction Synchronization Barrier (ISB) operation immediately after the change above.
 //See page A8-102  of the ARMv7 Architecture Reference Manual
 //ARMv7 "instruction synchronization barrier" instruction.
 isb

 //Enable the VFP unit by setting the EN bit in the FPEXC system register.
 //See page ???
 fmrx r12, fpexc
 orr r12, r12, #ARMV7_FPEXC_EN
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

procedure ARMv7MMUInit;
begin
 {}
 {Check the Page Table}
 if PAGE_TABLE_SIZE < SIZE_16K then Exit; {Level 1 Page Table is 16KB}
 if Pointer(PAGE_TABLE_BASE) = nil then Exit;
 
 {Initialize the Page Table}
 ARMv7PageTableInit;
 
 {Start the MMU}
 ARMv7StartMMU;
end;

{==============================================================================}

procedure ARMv7CacheInit; assembler; nostackframe;
asm
 //Enable L1 Data Caching by setting the C bit in the C1 control register.
 //See page ???
 mrc p15, #0, r12, cr1, cr0, #0;
 orr r12, r12, #ARMV7_CP15_C1_C_BIT
 mcr p15, #0, r12, cr1, cr0, #0;
 
 //Perform an Instruction Synchronization Barrier (ISB) operation immediately after the change above.
 //See page A8-102  of the ARMv7 Architecture Reference Manual
 //ARMv7 "instruction synchronization barrier" instruction.
 isb
end;

{==============================================================================}

procedure ARMv7TimerInit(Frequency:LongWord); assembler; nostackframe;
asm
 //Set the Counter Frequency register of the Generic Timer in the C14 control register.
 //See page B4-1532 of the ARM Architecture Reference Manual
 mcr p15, #0, r0, cr14, cr0, #0
end;

{==============================================================================}

procedure ARMv7PageTableInit;
{Initialize the Hardware Page Tables before enabling the MMU
 See page ???}
var
 Count:Integer;
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
   {If no handler is supplied then simply initialize 1GB (1024 1MB sections) as Normal, Cacheable, Shared, Executable with Read/Write access and direct mapping of Virtual to Physical}
   {And 3GB (3072 1MB sections) as Normal, Non Cacheable, Shared, Executable with Read/Write access and direct mapping of Virtual to Physical}
   {This will not normally be a useful setup but it will at least provide a Page Table that allows the memory management unit to be enabled}

   {Set the 1MB sections in the first 1GB as ARMV7_L1D_CACHE_REMAP_NORMAL_WRITE_ALLOCATE (Shared)(Executable)(Read Write)}
   Address:=$00000000;
   for Count:=0 to 1023 do
    begin
     ARMv7SetPageTableSection(Address,Address,ARMV7_L1D_CACHE_REMAP_NORMAL_WRITE_ALLOCATE or ARMV7_L1D_FLAG_SHARED or ARMV7_L1D_ACCESS_READWRITE);
     Inc(Address,SIZE_1M);
    end;

   {Set the 1MB sections in the remaining 3GB as ARMV7_L1D_CACHE_REMAP_NORMAL_NONCACHED (Shared)(Executable)(Read Write)}
   for Count:=1024 to 4095 do
    begin
     ARMv7SetPageTableSection(Address,Address,ARMV7_L1D_CACHE_REMAP_NORMAL_NONCACHED or ARMV7_L1D_FLAG_SHARED or ARMV7_L1D_ACCESS_READWRITE);
     Inc(Address,SIZE_1M);
    end;
   
   {Set the 1MB section containing the PAGE_TABLE_BASE to ARMV7_L1D_CACHE_REMAP_NORMAL_WRITE_ALLOCATE (Shared)(Executable)(Read Write)}
   Address:=(PAGE_TABLE_BASE and ARMV7_L1D_SECTION_BASE_MASK);
   ARMv7SetPageTableSection(Address,Address,ARMV7_L1D_CACHE_REMAP_NORMAL_WRITE_ALLOCATE or ARMV7_L1D_FLAG_SHARED or ARMV7_L1D_ACCESS_READWRITE); 
   
   {Set the 1MB sections containing the PERIPHERALS_BASE to ARMV7_L1D_CACHE_REMAP_DEVICE (Shared)(Non Executable)(Read Write)} 
   if PERIPHERALS_SIZE > 0 then
    begin
     Address:=(PERIPHERALS_BASE and ARMV7_L1D_SECTION_BASE_MASK);
     while Address < (PERIPHERALS_BASE + PERIPHERALS_SIZE) do
      begin
       ARMv7SetPageTableSection(Address,Address,ARMV7_L1D_CACHE_REMAP_DEVICE or ARMV7_L1D_FLAG_SHARED or ARMV7_L1D_FLAG_XN or ARMV7_L1D_ACCESS_READWRITE);
       Inc(Address,SIZE_1M);
      end;
    end;  
   
   {Set the 1MB sections containing the LOCAL_PERIPHERALS_BASE to ARMV7_L1D_CACHE_REMAP_DEVICE (Shared)(Non Executable)(Read Write)}    
   if LOCAL_PERIPHERALS_SIZE > 0 then
    begin
     Address:=(LOCAL_PERIPHERALS_BASE and ARMV7_L1D_SECTION_BASE_MASK);
     while Address < (LOCAL_PERIPHERALS_BASE + LOCAL_PERIPHERALS_SIZE) do
      begin
       ARMv7SetPageTableSection(Address,Address,ARMV7_L1D_CACHE_REMAP_DEVICE or ARMV7_L1D_FLAG_SHARED or ARMV7_L1D_FLAG_XN or ARMV7_L1D_ACCESS_READWRITE);
       Inc(Address,SIZE_1M);
      end;
    end;  
  end;  
end;

{==============================================================================}

procedure ARMv7SystemCall(Number:LongWord;Param1,Param2,Param3:PtrUInt); assembler; nostackframe;
asm
 //Perform a Supervisor Call
 //Number will be passed in R0
 //Param1 will be passed in R1
 //Param2 will be passed in R2
 //Param3 will be passed in R3
 svc #0
end;

{==============================================================================}

function ARMv7CPUGetMode:LongWord; assembler; nostackframe;
asm
 //Get Current program status register
 mrs r0, cpsr
 
 //Mask off everything except the MODE bits
 and r0, r0, #ARM_MODE_BITS
end;

{==============================================================================}

function ARMv7CPUGetState:LongWord; assembler; nostackframe;
asm
 //To Do 
 
end;

{==============================================================================}

function ARMv7CPUGetCurrent:LongWord; assembler; nostackframe;
asm
 //Read the Multiprocessor Affinity (MPIDR) register from the system control coprocessor CP15
 mrc p15, #0, r0, cr0, cr0, #5
 
 //Mask off the CPUID value
 and r0, r0, #ARMV7_CP15_C0_MPID_CPUID_MASK
end;

{==============================================================================}

function ARMv7CPUGetMainID:LongWord; assembler; nostackframe;
asm
 //Read the MainID register from the system control coprocessor CP15
 mrc p15, #0, r0, cr0, cr0, #0
end;

{==============================================================================}

function ARMv7CPUGetMultiprocessorID:LongWord; assembler; nostackframe;
asm
 //Read the Multiprocessor Affinity (MPIDR) register from the system control coprocessor CP15
 mrc p15, #0, r0, cr0, cr0, #5
end;

{==============================================================================}

function ARMv7CPUGetModel:LongWord;
var
 MainID:LongWord;
begin 
 {}
 Result:=CPU_MODEL_UNKNOWN;
 
 {Get MainID}
 MainID:=ARMv7CPUGetMainID;
 if MainID <> 0 then
  begin 
   {Check Primary Part Number}
   if (MainID and ARMV7_CP15_C0_MAINID_PARTNUMBER_MASK) = ARMV7_CP15_C0_MAINID_PARTNUMBER_CORTEX_A5 then
    begin
     Result:=CPU_MODEL_CORTEX_A5;
    end
   else if (MainID and ARMV7_CP15_C0_MAINID_PARTNUMBER_MASK) = ARMV7_CP15_C0_MAINID_PARTNUMBER_CORTEX_A7 then 
    begin
     Result:=CPU_MODEL_CORTEX_A7;
    end
   else if (MainID and ARMV7_CP15_C0_MAINID_PARTNUMBER_MASK) = ARMV7_CP15_C0_MAINID_PARTNUMBER_CORTEX_A8 then 
    begin
     Result:=CPU_MODEL_CORTEX_A8;
    end
   else if (MainID and ARMV7_CP15_C0_MAINID_PARTNUMBER_MASK) = ARMV7_CP15_C0_MAINID_PARTNUMBER_CORTEX_A9 then 
    begin
     Result:=CPU_MODEL_CORTEX_A9;
    end
   else if (MainID and ARMV7_CP15_C0_MAINID_PARTNUMBER_MASK) = ARMV7_CP15_C0_MAINID_PARTNUMBER_CORTEX_A15 then 
    begin
     Result:=CPU_MODEL_CORTEX_A15;
    end
   else if (MainID and ARMV7_CP15_C0_MAINID_PARTNUMBER_MASK) = ARMV7_CP15_C0_MAINID_PARTNUMBER_CORTEX_A17 then 
    begin
     Result:=CPU_MODEL_CORTEX_A17;
    end
   {The following are ARMv8 part numbers, included here to allow ARMv7 code on ARMv8 in 32bit mode}
   else if (MainID and ARMV7_CP15_C0_MAINID_PARTNUMBER_MASK) = ARMV7_CP15_C0_MAINID_PARTNUMBER_CORTEX_A53 then 
    begin
     Result:=CPU_MODEL_CORTEX_A53;
    end
   else if (MainID and ARMV7_CP15_C0_MAINID_PARTNUMBER_MASK) = ARMV7_CP15_C0_MAINID_PARTNUMBER_CORTEX_A57 then 
    begin
     Result:=CPU_MODEL_CORTEX_A57;
    end
   else if (MainID and ARMV7_CP15_C0_MAINID_PARTNUMBER_MASK) = ARMV7_CP15_C0_MAINID_PARTNUMBER_CORTEX_A72 then 
    begin
     Result:=CPU_MODEL_CORTEX_A72;
    end;
  end;
end;

{==============================================================================}

function ARMv7CPUGetRevision:LongWord;
var
 MainID:LongWord;
begin 
 {}
 Result:=0;
 
 {Get MainID}
 MainID:=ARMv7CPUGetMainID;
 if MainID <> 0 then
  begin 
   {Get Variant}
   Result:=(MainID and ARMV7_CP15_C0_MAINID_VARIANT_MASK) shr 16;
   
   {Get Revision}
   Result:=Result or (MainID and ARMV7_CP15_C0_MAINID_REVISION_MASK);
  end;
end;

{==============================================================================}

function ARMv7CPUGetDescription:String;
var
 MainID:LongWord;
 MultiprocessorID:LongWord;
begin 
 {}
 Result:='';
 
 {Get MainID}
 MainID:=ARMv7CPUGetMainID;
 if MainID <> 0 then
  begin 
   {Check Primary Part Number}
   if (MainID and ARMV7_CP15_C0_MAINID_PARTNUMBER_MASK) = ARMV7_CP15_C0_MAINID_PARTNUMBER_CORTEX_A5 then
    begin
     {Get MultiprocessorID}
     MultiprocessorID:=ARMv7CPUGetMultiprocessorID;
     if ((MultiprocessorID and ARMV7_CP15_C0_MPID_MPE) = 1) and ((MultiprocessorID and ARMV7_CP15_C0_MPID_U_UNIPROCESSOR) = ARMV7_CP15_C0_MPID_U_MULTIPROCESSOR) then
      begin
       Result:=CPU_DESCRIPTION_CORTEX_A5_MP;
      end
     else
      begin
       Result:=CPU_DESCRIPTION_CORTEX_A5;
      end;
    end
   else if (MainID and ARMV7_CP15_C0_MAINID_PARTNUMBER_MASK) = ARMV7_CP15_C0_MAINID_PARTNUMBER_CORTEX_A7 then 
    begin
     Result:=CPU_DESCRIPTION_CORTEX_A7;
    end
   else if (MainID and ARMV7_CP15_C0_MAINID_PARTNUMBER_MASK) = ARMV7_CP15_C0_MAINID_PARTNUMBER_CORTEX_A8 then 
    begin
     Result:=CPU_DESCRIPTION_CORTEX_A8;
    end
   else if (MainID and ARMV7_CP15_C0_MAINID_PARTNUMBER_MASK) = ARMV7_CP15_C0_MAINID_PARTNUMBER_CORTEX_A9 then 
    begin
     {Get MultiprocessorID}
     MultiprocessorID:=ARMv7CPUGetMultiprocessorID;
     if ((MultiprocessorID and ARMV7_CP15_C0_MPID_MPE) = 1) and ((MultiprocessorID and ARMV7_CP15_C0_MPID_U_UNIPROCESSOR) = ARMV7_CP15_C0_MPID_U_MULTIPROCESSOR) then
      begin
       Result:=CPU_DESCRIPTION_CORTEX_A9_MP;
      end
     else
      begin
       Result:=CPU_DESCRIPTION_CORTEX_A9;
      end;
    end
   else if (MainID and ARMV7_CP15_C0_MAINID_PARTNUMBER_MASK) = ARMV7_CP15_C0_MAINID_PARTNUMBER_CORTEX_A15 then 
    begin
     Result:=CPU_DESCRIPTION_CORTEX_A15;
    end
   else if (MainID and ARMV7_CP15_C0_MAINID_PARTNUMBER_MASK) = ARMV7_CP15_C0_MAINID_PARTNUMBER_CORTEX_A17 then 
    begin
     Result:=CPU_DESCRIPTION_CORTEX_A17;
    end
   {The following are ARMv8 part numbers, included here to allow ARMv7 code on ARMv8 in 32bit mode} 
   else if (MainID and ARMV7_CP15_C0_MAINID_PARTNUMBER_MASK) = ARMV7_CP15_C0_MAINID_PARTNUMBER_CORTEX_A53 then 
    begin
     Result:=CPU_DESCRIPTION_CORTEX_A53;
    end
   else if (MainID and ARMV7_CP15_C0_MAINID_PARTNUMBER_MASK) = ARMV7_CP15_C0_MAINID_PARTNUMBER_CORTEX_A57 then 
    begin
     Result:=CPU_DESCRIPTION_CORTEX_A57;
    end
   else if (MainID and ARMV7_CP15_C0_MAINID_PARTNUMBER_MASK) = ARMV7_CP15_C0_MAINID_PARTNUMBER_CORTEX_A72 then 
    begin
     Result:=CPU_DESCRIPTION_CORTEX_A72;
    end;
  end;
end;

{==============================================================================}

function ARMv7FPUGetState:LongWord; assembler; nostackframe;
asm
 //To Do 
 
end;

{==============================================================================}

function ARMv7L1CacheGetType:LongWord; assembler; nostackframe; 
asm
 //Read the CacheLevel ID register from the system control coprocessor
 mrc p15, #1, r1, cr0, cr0, #1
 
 //Mask off the Ctype1 bits
 and r1, r1, #ARMV7_CP15_C0_CLID_CTYPE1_MASK
 
 //Check for Unified
 mov r0, #CACHE_TYPE_UNIFIED
 cmp r1, #ARMV7_CP15_C0_CLID_CTYPE1_UNIFIED
 bxeq lr
 
 //Check for Separate
 mov r0, #CACHE_TYPE_SEPARATE
 cmp r1, #ARMV7_CP15_C0_CLID_CTYPE1_SEPARATE
 bxeq lr

 //Check for Data
 mov r0, #CACHE_TYPE_DATA
 cmp r1, #ARMV7_CP15_C0_CLID_CTYPE1_DATA
 bxeq lr

 //Check for Instruction
 mov r0, #CACHE_TYPE_INSTRUCTION
 cmp r1, #ARMV7_CP15_C0_CLID_CTYPE1_INSTRUCTION
 bxeq lr
 
 //Return None
 mov r0, #CACHE_TYPE_NONE
end;

{==============================================================================}

function ARMv7L1DataCacheGetSize:LongWord; assembler; nostackframe; 
asm
 //Preserve LR
 mov r12, lr

 //Get the L1 Cache Type (Returned in R0)
 bl ARMv7L1CacheGetType
 
 //Restore LR
 mov lr, r12
 
 //Check for None 
 cmp r0, #CACHE_TYPE_NONE
 beq .LInvalid
 
 //Check for Instruction 
 cmp r0, #CACHE_TYPE_INSTRUCTION
 beq .LInvalid
 
 //Set the CacheSize Selection to Level 1 Data (or Unified)
 mov r0, #ARMV7_CP15_C0_CSSEL_LEVEL1 | ARMV7_CP15_C0_CSSEL_DATA
 mcr p15, #2, r0, cr0, cr0, #0
 
 //Get the CacheSize ID register
 mov r0, #0
 mrc p15, #1, r0, cr0, cr0, #0

 //Get the NumSets in R1 (Mask from R0, Shift Right, Add 1)
 ldr r12, =ARMV7_CP15_C0_CCSID_NUMSETS_MASK
 and r1, r0, r12
 lsr r1, r1, #ARMV7_CP15_C0_CCSID_NUMSETS_SHIFT
 add r1, r1, #1
 
 //Get the NumWays in R2 (Mask from R0, Shift Right, Add 1)
 ldr r12, =ARMV7_CP15_C0_CCSID_NUMWAYS_MASK
 and r2, r0, r12
 lsr r2, r2, #ARMV7_CP15_C0_CCSID_NUMWAYS_SHIFT
 add r2, r2, #1
 
 //Get the LineSize in R3 (Mask from R0, Add 2, Shift 1 Left by result, Multiply by 4)
 and r3, r0, #ARMV7_CP15_C0_CCSID_LINESIZE_MASK
 add r3, r3, #2
 mov r12, #1
 lsl r12, r12, r3
 mov r3, r12
 lsl r3, r3, #2
 
 //Multiply NumSets * NumWays * LineSize (Result in R0)
 mul r0, r1, r2
 mul r0, r0, r3
 
 //Return to caller
 bx lr
 
.LInvalid:
 //Invalid size
 mov r0, #0
end;

{==============================================================================}

function ARMv7L1DataCacheGetLineSize:LongWord; assembler; nostackframe;  
asm
 //Preserve LR
 mov r12, lr

 //Get the L1 Cache Type (Returned in R0)
 bl ARMv7L1CacheGetType
 
 //Restore LR
 mov lr, r12
 
 //Check for None 
 cmp r0, #CACHE_TYPE_NONE
 beq .LInvalid
 
 //Check for Instruction 
 cmp r0, #CACHE_TYPE_INSTRUCTION
 beq .LInvalid
 
 //Set the CacheSize Selection to Level 1 Data (or Unified)
 mov r0, #ARMV7_CP15_C0_CSSEL_LEVEL1 | ARMV7_CP15_C0_CSSEL_DATA
 mcr p15, #2, r0, cr0, cr0, #0
 
 //Get the CacheSize ID register
 mov r0, #0
 mrc p15, #1, r0, cr0, cr0, #0

 //Get the LineSize in R3 (Mask from R0, Add 2, Shift 1 Left by result, Multiply by 4)
 and r3, r0, #ARMV7_CP15_C0_CCSID_LINESIZE_MASK
 add r3, r3, #2
 mov r12, #1
 lsl r12, r12, r3
 mov r3, r12
 lsl r3, r3, #2
 
 //Return to caller
 mov r0, r3
 bx lr
 
.LInvalid:
 //Invalid size
 mov r0, #0
end;

{==============================================================================}

function ARMv7L1InstructionCacheGetSize:LongWord; assembler; nostackframe;
asm
 //Preserve LR
 mov r12, lr

 //Get L1 Cache Type (Returned in R0)
 bl ARMv7L1CacheGetType
 
 //Restore LR
 mov lr, r12
 
 //Check for None 
 cmp r0, #CACHE_TYPE_NONE
 beq .LInvalid
 
 //Check for Data 
 cmp r0, #CACHE_TYPE_DATA
 beq .LInvalid
 
 //Check for Instruction
 cmp r0, #CACHE_TYPE_INSTRUCTION
 movne r1, #ARMV7_CP15_C0_CSSEL_DATA
 moveq r1, #ARMV7_CP15_C0_CSSEL_INSTRUCTION

 //Set the CacheSize Selection to Level 1 (Instruction or Unified)
 mov r0, #ARMV7_CP15_C0_CSSEL_LEVEL1
 orr r0, r0, r1
 mcr p15, #2, r0, cr0, cr0, #0
 
 //Get the CacheSize ID register
 mov r0, #0
 mrc p15, #1, r0, cr0, cr0, #0

 //Get the NumSets in R1 (Mask from R0, Shift Right, Add 1)
 ldr r12, =ARMV7_CP15_C0_CCSID_NUMSETS_MASK
 and r1, r0, r12
 lsr r1, r1, #ARMV7_CP15_C0_CCSID_NUMSETS_SHIFT
 add r1, r1, #1
 
 //Get the NumWays in R2 (Mask from R0, Shift Right, Add 1)
 ldr r12, =ARMV7_CP15_C0_CCSID_NUMWAYS_MASK
 and r2, r0, r12
 lsr r2, r2, #ARMV7_CP15_C0_CCSID_NUMWAYS_SHIFT
 add r2, r2, #1
 
 //Get the LineSize in R3 (Mask from R0, Add 2, Shift 1 Left by result, Multiply by 4)
 and r3, r0, #ARMV7_CP15_C0_CCSID_LINESIZE_MASK
 add r3, r3, #2
 mov r12, #1
 lsl r12, r12, r3
 mov r3, r12
 lsl r3, r3, #2
 
 //Multiply NumSets * NumWays * LineSize (Result in R0)
 mul r0, r1, r2
 mul r0, r0, r3
 
 //Return to caller
 bx lr
 
.LInvalid:
 //Invalid size
 mov r0, #0
end;

{==============================================================================}

function ARMv7L1InstructionCacheGetLineSize:LongWord; assembler; nostackframe;  
asm
 //Preserve LR
 mov r12, lr

 //Get L1 Cache Type (Returned in R0)
 bl ARMv7L1CacheGetType
 
 //Restore LR
 mov lr, r12
 
 //Check for None 
 cmp r0, #CACHE_TYPE_NONE
 beq .LInvalid
 
 //Check for Data 
 cmp r0, #CACHE_TYPE_DATA
 beq .LInvalid
 
 //Check for Instruction
 cmp r0, #CACHE_TYPE_INSTRUCTION
 movne r1, #ARMV7_CP15_C0_CSSEL_DATA
 moveq r1, #ARMV7_CP15_C0_CSSEL_INSTRUCTION

 //Set the CacheSize Selection to Level 1 (Instruction or Unified)
 mov r0, #ARMV7_CP15_C0_CSSEL_LEVEL1
 orr r0, r0, r1
 mcr p15, #2, r0, cr0, cr0, #0
 
 //Get the CacheSize ID register
 mov r0, #0
 mrc p15, #1, r0, cr0, cr0, #0

 //Get the LineSize in R3 (Mask from R0, Add 2, Shift 1 Left by result, Multiply by 4)
 and r3, r0, #ARMV7_CP15_C0_CCSID_LINESIZE_MASK
 add r3, r3, #2
 mov r12, #1
 lsl r12, r12, r3
 mov r3, r12
 lsl r3, r3, #2
 
 //Return to caller
 mov r0, r3
 bx lr
 
.LInvalid:
 //Invalid size
 mov r0, #0
end;

{==============================================================================}

function ARMv7L2CacheGetType:LongWord; assembler; nostackframe;
asm
 //Read the CacheLevel ID register from the system control coprocessor
 mrc p15, #1, r1, cr0, cr0, #1

 //Mask off the Ctype2 bits
 and r1, r1, #ARMV7_CP15_C0_CLID_CTYPE2_MASK
 
 //Check for Unified
 mov r0, #CACHE_TYPE_UNIFIED
 cmp r1, #ARMV7_CP15_C0_CLID_CTYPE2_UNIFIED
 bxeq lr
 
 //Check for Separate
 mov r0, #CACHE_TYPE_SEPARATE
 cmp r1, #ARMV7_CP15_C0_CLID_CTYPE2_SEPARATE
 bxeq lr

 //Check for Data
 mov r0, #CACHE_TYPE_DATA
 cmp r1, #ARMV7_CP15_C0_CLID_CTYPE2_DATA
 bxeq lr

 //Check for Instruction
 mov r0, #CACHE_TYPE_INSTRUCTION
 cmp r1, #ARMV7_CP15_C0_CLID_CTYPE2_INSTRUCTION
 bxeq lr
 
 //Return None
 mov r0, #CACHE_TYPE_NONE
end;

{==============================================================================}

function ARMv7L2CacheGetSize:LongWord; assembler; nostackframe;
asm
 //Preserve LR
 mov r12, lr

 //Get L2 Cache Type (Returned in R0)
 bl ARMv7L2CacheGetType
 
 //Restore LR
 mov lr, r12
 
 //Check for None 
 cmp r0, #CACHE_TYPE_NONE
 beq .LInvalid

 //Check for Instruction
 cmp r0, #CACHE_TYPE_INSTRUCTION
 movne r1, #ARMV7_CP15_C0_CSSEL_DATA
 moveq r1, #ARMV7_CP15_C0_CSSEL_INSTRUCTION

 //Set the CacheSize Selection to Level 2 (Instruction or Unified)
 mov r0, #ARMV7_CP15_C0_CSSEL_LEVEL2
 orr r0, r0, r1
 mcr p15, #2, r0, cr0, cr0, #0
 
 //Get the CacheSize ID register
 mov r0, #0
 mrc p15, #1, r0, cr0, cr0, #0

 //Get the NumSets in R1 (Mask from R0, Shift Right, Add 1)
 ldr r12, =ARMV7_CP15_C0_CCSID_NUMSETS_MASK
 and r1, r0, r12
 lsr r1, r1, #ARMV7_CP15_C0_CCSID_NUMSETS_SHIFT
 add r1, r1, #1
 
 //Get the NumWays in R2 (Mask from R0, Shift Right, Add 1)
 ldr r12, =ARMV7_CP15_C0_CCSID_NUMWAYS_MASK
 and r2, r0, r12
 lsr r2, r2, #ARMV7_CP15_C0_CCSID_NUMWAYS_SHIFT
 add r2, r2, #1
 
 //Get the LineSize in R3 (Mask from R0, Add 2, Shift 1 Left by result, Multiply by 4)
 and r3, r0, #ARMV7_CP15_C0_CCSID_LINESIZE_MASK
 add r3, r3, #2
 mov r12, #1
 lsl r12, r12, r3
 mov r3, r12
 lsl r3, r3, #2
 
 //Multiply NumSets * NumWays * LineSize (Result in R0)
 mul r0, r1, r2
 mul r0, r0, r3
 
 //Return to caller
 bx lr
 
.LInvalid:
 //Invalid size
 mov r0, #0
end;

{==============================================================================}

function ARMv7L2CacheGetLineSize:LongWord; assembler; nostackframe;
asm
 //Preserve LR
 mov r12, lr

 //Get L2 Cache Type (Returned in R0)
 bl ARMv7L2CacheGetType
 
 //Restore LR
 mov lr, r12
 
 //Check for None 
 cmp r0, #CACHE_TYPE_NONE
 beq .LInvalid

 //Check for Instruction
 cmp r0, #CACHE_TYPE_INSTRUCTION
 movne r1, #ARMV7_CP15_C0_CSSEL_DATA
 moveq r1, #ARMV7_CP15_C0_CSSEL_INSTRUCTION

 //Set the CacheSize Selection to Level 2 (Instruction or Unified)
 mov r0, #ARMV7_CP15_C0_CSSEL_LEVEL2
 orr r0, r0, r1
 mcr p15, #2, r0, cr0, cr0, #0
 
 //Get the CacheSize ID register
 mov r0, #0
 mrc p15, #1, r0, cr0, cr0, #0
 
 //Get the LineSize in R3 (Mask from R0, Add 2, Shift 1 Left by result, Multiply by 4)
 and r3, r0, #ARMV7_CP15_C0_CCSID_LINESIZE_MASK
 add r3, r3, #2
 mov r12, #1
 lsl r12, r12, r3
 mov r3, r12
 lsl r3, r3, #2
 
 //Return to caller
 mov r0, r3
 bx lr
 
.LInvalid:
 //Invalid size
 mov r0, #0
end;

{==============================================================================}

procedure ARMv7Halt; assembler; nostackframe; public name'_haltproc';
{The purpose of the Wait For Interrupt operation is to put the processor in to a low power state,
 see Standby mode on page A8-810 of the ARMv7 Architecture Reference Manual}
asm
 //Perform a "data synchronization barrier'
 dsb
 
.LLoop:
 //Disable IRQ and FIQ
 msr cpsr_c, #ARM_I_BIT | ARM_F_BIT

 //ARMv7 "wait for interrupt" instruction.
 wfi
 b .LLoop
end;

{==============================================================================}

procedure ARMv7Pause; assembler; nostackframe;
{The purpose of the Wait For Interrupt operation is to put the processor in to a low power state,
 see Standby mode on page A8-810 of the ARMv7 Architecture Reference Manual}
asm
 //Perform a "data synchronization barrier'
 dsb

 //ARMv7 "wait for interrupt" instruction.
 wfi
end;

{==============================================================================}

procedure ARMv7SendEvent; assembler; nostackframe;
{See Page A8-316 of the ARMv7 Architecture Reference Manual}
asm
 //Perform a "data synchronization barrier'
 dsb
 
 //ARMv7 "send event" instruction.
 sev
end;

{==============================================================================}

procedure ARMv7WaitForEvent; assembler; nostackframe;
{See Page A8-808 of the ARMv7 Architecture Reference Manual}
asm
 //Perform a "data synchronization barrier'
 dsb

 //ARMv7 "wait for event" instruction.
 wfe
end;

{==============================================================================}

procedure ARMv7WaitForInterrupt; assembler; nostackframe;
{The purpose of the Wait For Interrupt operation is to put the processor in to a low power state,
 see Standby mode on page A8-810 of the ARMv7 Architecture Reference Manual}
asm
 //Perform a "data synchronization barrier'
 dsb

 //ARMv7 "wait for interrupt" instruction.
 wfi
end;

{==============================================================================}

procedure ARMv7DataMemoryBarrier; assembler; nostackframe;
{Perform a data memory barrier operation using the c7 (Cache Operations) register of system control coprocessor CP15
 See page A8-90 of the ARMv7 Architecture Reference Manual}
 
{Note that this is also available in the FPC RTL as ReadBarrier/WriteBarrier

 See: \source\rtl\arm\arm.inc
 
 Implementation is exactly the same for either.
} 
asm
 //ARMv7 "data memory barrier" instruction.
 dmb
end;

{==============================================================================}

procedure ARMv7DataSynchronizationBarrier; assembler; nostackframe;
{Perform a data synchronization barrier operation
 See page A8-92 of the ARMv7 Architecture Reference Manual}
asm
 //ARMv7 "data synchronization barrier" instruction.
 dsb
end;

{==============================================================================}

procedure ARMv7InstructionMemoryBarrier; assembler; nostackframe;
{Perform a instruction synchronization barrier operation
 See page A8-102 of the ARMv7 Architecture Reference Manual}
asm
 //ARMv7 "instruction synchronization barrier" instruction.
 isb
end;

{==============================================================================}

procedure ARMv7InvalidateTLB; assembler; nostackframe;
{Perform an invalidate entire TLB operation using the c8 (TLB Operations) register of system control coprocessor CP15
 See page B3-138 of the ARMv7 Architecture Reference Manual}
asm
 //Get the MPID register from the system control coprocessor CP15
 mrc p15, #0, r0, cr0, cr0, #5
 
 //Mask off the Multiprocessor Extensions bit (MPE)
 and r1, r0, #ARMV7_CP15_C0_MPID_MPE
 cmp r1, #0
 beq .LUniprocessor
 
 //Mask off the Uniprocessor bit (U)
 and r1, r0, #ARMV7_CP15_C0_MPID_U_UNIPROCESSOR
 cmp r1, #0
 bne .LUniprocessor
 
 //Invalidate entire TLB (Unified/Inner Shareable)
 mov r12, #0
 mcr p15, #0, r12, cr8, cr3, #0

 //Perform a data synchronization barrier
 dsb
 
 //Return to caller
 bx lr
 
.LUniprocessor: 
 //Invalidate entire TLB (Unlocked/Unified)
 mov r12, #0
 mcr p15, #0, r12, cr8, cr7, #0
 
 //Perform a data synchronization barrier
 dsb
end;

{==============================================================================}

procedure ARMv7InvalidateDataTLB; assembler; nostackframe;
{Perform an invalidate data TLB (Unlocked/Data) operation using the c8 (TLB Operations) register of system control coprocessor CP15
 See page B3-138 of the ARMv7 Architecture Reference Manual}
asm
 //Invalidate data TLB (Unlocked/Data)
 mov r12, #0
 mcr p15, #0, r12, cr8, cr6, #0
 
 //Perform a data synchronization barrier
 dsb
end;

{==============================================================================}

procedure ARMv7InvalidateInstructionTLB; assembler; nostackframe;
{Perform an invalidate instruction TLB (Unlocked/Instruction) operation using the c8 (TLB Operations) register of system control coprocessor CP15
 See page B3-138 of the ARMv7 Architecture Reference Manual}
asm
 //Invalidate instruction TLB (Unlocked/Instruction)
 mov r12, #0
 mcr p15, #0, r12, cr8, cr5, #0
 
 //Perform a data synchronization barrier
 dsb
end;

{==============================================================================}

procedure ARMv7InvalidateCache; assembler; nostackframe;
{Perform an invalidate both caches operation using the c7 (Cache Operations) register of system control coprocessor CP15
 See page B3-127 of the ARMv7 Architecture Reference Manual}
asm
 //Get the MPID register from the system control coprocessor CP15
 mrc p15, #0, r0, cr0, cr0, #5
 
 //Mask off the Multiprocessor Extensions bit (MPE)
 and r1, r0, #ARMV7_CP15_C0_MPID_MPE
 cmp r1, #0
 beq .LUniprocessor
 
 //Mask off the Uniprocessor bit (U)
 and r1, r0, #ARMV7_CP15_C0_MPID_U_UNIPROCESSOR
 cmp r1, #0
 bne .LUniprocessor
 
 //Invalidate all instruction caches to PoU (Inner Shareable)
 mov r12, #0
 mcr p15, #0, r12, cr7, cr1, #0

 //Perform a data synchronization barrier
 dsb

 //Branch to ARMv7InvalidateDataCache (Will return to caller via LR)
 b ARMv7InvalidateDataCache
 
.LUniprocessor: 
 //Invalidate all instruction caches to PoU
 mov r12, #0
 mcr p15, #0, r12, cr7, cr5, #0
  
 //Perform a data synchronization barrier
 dsb
 
 //Branch to ARMv7InvalidateDataCache (Will return to caller via LR)
 b ARMv7InvalidateDataCache
end;

{==============================================================================}

procedure ARMv7CleanDataCache; assembler; nostackframe;
{Perform a clean entire data cache operation
 See page B3-127 of the ARMv7 Architecture Reference Manual}
asm
 mrc	p15, #1, r0, cr0, cr0, #1        //read CLIDR
 tst	r0, #0x07000000
 beq	.LDone
 mov	r3, #0			                 //start with L1

.LStart:
 add	r2, r3, r3, lsr #1	             //r2 = level * 3 / 2
 mov	r1, r0, lsr r2		             //r1 = cache type
 tst	r1, #6			                 //is it unified or data?
 beq	.LNext	                         //nope, skip level

 mcr	p15, #2, r3, cr0, cr0, #0	     //select cache level
 isb
 mrc	p15, #1, r0, cr0, cr0, #0	     //read CCSIDR

 ubfx	ip, r0, #0, #3		             //get linesize from CCSIDR
 add	ip, ip, #4		                 //apply bias
 ubfx	r2, r0, #13, #15	             //get numsets - 1 from CCSIDR
 lsl	r2, r2, ip		                 //shift to set position
 orr	r3, r3, r2		                 //merge set into way/set/level
 mov	r1, #1
 lsl	r1, r1, ip		                 //r1 = set decr

 ubfx	ip, r0, #3, #10		             //get numways - 1 from [to be discarded] CCSIDR
 clz	r2, ip			                 //number of bits to MSB of way
 lsl	ip, ip, r2		                 //shift by that into way position
 mov	r0, #1			                
 lsl	r2, r0, r2		                 //r2 now contains the way decr
 mov	r0, r3 			                 //get sets/level (no way yet)
 orr	r3, r3, ip		                 //merge way into way/set/level
 bfc	r0, #0, #4		                 //clear low 4 bits (level) to get numset - 1
 sub	r2, r2, r0		                 //subtract from way decr

 //r3 = ways/sets/level, r2 = way decr, r1 = set decr, r0 and ip are free
.LLoop:    
 mcr	p15, #0, r3, cr7, cr10, #2	     //clean line
 cmp	r3, #15			                 //are we done with this level (way/set == 0)
 bls	.LNext	                         //yes, go to next level
 ubfx	r0, r3, #4, #18		             //extract set bits
 cmp	r0, #0			                 //compare
 subne	r3, r3, r1		                 //non-zero?, decrement set #
 subeq	r3, r3, r2		                 //zero?, decrement way # and restore set count
 b	.LLoop

.LNext:
 dsb
 mrc	p15, #1, r0, cr0, cr0, #1	     //read CLIDR
 ubfx	ip, r0, #24, #3		             //narrow to LoC
 add	r3, r3, #2		                 //go to next level
 cmp	r3, ip, lsl #1		             //compare
 blt	.LStart		                     //not done, next level (r0 == CLIDR)

.LDone:
 mov	r0, #0			                 //default back to cache level 0
 mcr	p15, #2, r0, cr0, cr0, #0	     //select cache level
 dsb
 isb
end;

{==============================================================================}

procedure ARMv7InvalidateDataCache; assembler; nostackframe;
{Perform an invalidate entire data cache operation
 See page B3-127 of the ARMv7 Architecture Reference Manual}
asm
 mrc	p15, #1, r0, cr0, cr0, #1        //read CLIDR
 tst	r0, #0x07000000
 beq	.LDone
 mov	r3, #0			                 //start with L1

.LStart:
 add	r2, r3, r3, lsr #1	             //r2 = level * 3 / 2
 mov	r1, r0, lsr r2		             //r1 = cache type
 tst	r1, #6			                 //is it unified or data?
 beq	.LNext	                         //nope, skip level

 mcr	p15, #2, r3, cr0, cr0, #0	     //select cache level
 isb
 mrc	p15, #1, r0, cr0, cr0, #0	     //read CCSIDR

 ubfx	ip, r0, #0, #3		             //get linesize from CCSIDR
 add	ip, ip, #4		                 //apply bias
 ubfx	r2, r0, #13, #15	             //get numsets - 1 from CCSIDR
 lsl	r2, r2, ip		                 //shift to set position
 orr	r3, r3, r2		                 //merge set into way/set/level
 mov	r1, #1
 lsl	r1, r1, ip		                 //r1 = set decr

 ubfx	ip, r0, #3, #10		             //get numways - 1 from [to be discarded] CCSIDR
 clz	r2, ip			                 //number of bits to MSB of way
 lsl	ip, ip, r2		                 //shift by that into way position
 mov	r0, #1			                
 lsl	r2, r0, r2		                 //r2 now contains the way decr
 mov	r0, r3 			                 //get sets/level (no way yet)
 orr	r3, r3, ip		                 //merge way into way/set/level
 bfc	r0, #0, #4		                 //clear low 4 bits (level) to get numset - 1
 sub	r2, r2, r0		                 //subtract from way decr

 //r3 = ways/sets/level, r2 = way decr, r1 = set decr, r0 and ip are free
.LLoop:    
 mcr	p15, #0, r3, cr7, cr6, #2	     //invalidate line
 cmp	r3, #15			                 //are we done with this level (way/set == 0)
 bls	.LNext	                         //yes, go to next level
 ubfx	r0, r3, #4, #18		             //extract set bits
 cmp	r0, #0			                 //compare
 subne	r3, r3, r1		                 //non-zero?, decrement set #
 subeq	r3, r3, r2		                 //zero?, decrement way # and restore set count
 b	.LLoop

.LNext:
 dsb
 mrc	p15, #1, r0, cr0, cr0, #1	     //read CLIDR
 ubfx	ip, r0, #24, #3		             //narrow to LoC
 add	r3, r3, #2		                 //go to next level
 cmp	r3, ip, lsl #1		             //compare
 blt	.LStart		                     //not done, next level (r0 == CLIDR)

.LDone:
 mov	r0, #0			                 //default back to cache level 0
 mcr	p15, #2, r0, cr0, cr0, #0	     //select cache level
 dsb
 isb
end;

{==============================================================================}

procedure ARMv7InvalidateL1DataCache; assembler; nostackframe;
{Perform an invalidate entire L1 data cache operation
 See page B3-127 of the ARMv7 Architecture Reference Manual}
asm
 mrc	p15, #1, r0, cr0, cr0, #1	     //read CLIDR
 and	r0, r0, #0x7		             //check L1
 bxeq	lr			                     //return if no L1 cache
 mov	r3, #0			                 //start with L1
 
 mcr	p15, #2, r3, cr0, cr0, #0	     //select cache level
 isb
 mrc	p15, #1, r0, cr0, cr0, #0	     //read CCSIDR
    
 ubfx	ip, r0, #0, #3		             //get linesize from CCSIDR
 add	ip, ip, #4		                 //apply bias
 ubfx	r2, r0, #13, #15	             //get numsets - 1 from CCSIDR
 lsl	r2, r2, ip		                 //shift to set position
 orr	r3, r3, r2		                 //merge set into way/set/level
 mov	r1, #1
 lsl	r1, r1, ip		                 //r1 = set decr
    
 ubfx	ip, r0, #3, #10		             //get numways - 1 from [to be discarded] CCSIDR
 clz	r2, ip			                 //number of bits to MSB of way
 lsl	ip, ip, r2		                 //shift by that into way position
 mov	r0, #1			
 lsl	r2, r0, r2		                 //r2 now contains the way decr
 mov	r0, r3 			                 //get sets/level (no way yet)
 orr	r3, r3, ip		                 //merge way into way/set/level
 bfc	r0, #0, #4		                 //clear low 4 bits (level) to get numset - 1
 sub	r2, r2, r0		                 //subtract from way decr
    
 //r3 = ways/sets/level, r2 = way decr, r1 = set decr, r0 and ip are free
.LLoop:	
 mcr	p15, #0, r3, cr7, cr6, #2	     //invalidate line
 cmp	r3, #15			                 //are we done with this level (way/set == 0)
 bls	.LDone		                     //yes, we've finished
 ubfx	r0, r3, #4, #18		             //extract set bits
 cmp	r0, #0			                 //compare
 subne	r3, r3, r1		                 //non-zero?, decrement set #
 subeq	r3, r3, r2		                 //zero?, decrement way # and restore set count
 b	.LLoop

.LDone:
 dsb
 mov	r0, #0			                 //default back to cache level 0
 mcr	p15, #2, r0, cr0, cr0, #0	     //select cache level
 dsb
 isb
end;

{==============================================================================}

procedure ARMv7CleanAndInvalidateDataCache; assembler; nostackframe;
{Perform a clean and invalidate entire data cache operation
 See page B3-127 of the ARMv7 Architecture Reference Manual}
asm
 mrc	p15, #1, r0, cr0, cr0, #1	     //read CLIDR
 tst	r0, #0x07000000
 bxeq	lr
 mov	r3, #0			                 //start with L1

.LStart: 
 add	r2, r3, r3, lsr #1	             //r2 = level * 3 / 2
 mov	r1, r0, lsr r2		             //r1 = cache type
 tst	r1, #6			                 //is it unified or data?
 beq	.LNext	                         //nope, skip level

 mcr	p15, #2, r3, cr0, cr0, #0	     //select cache level
 isb
 mrc	p15, #1, r0, cr0, cr0, #0	     //read CCSIDR

 ubfx	ip, r0, #0, #3		             //get linesize from CCSIDR
 add	ip, ip, #4		                 //apply bias
 ubfx	r2, r0, #13, #15	             //get numsets - 1 from CCSIDR
 lsl	r2, r2, ip		                 //shift to set position
 orr	r3, r3, r2		                 //merge set into way/set/level
 mov	r1, #1
 lsl	r1, r1, ip		                 //r1 = set decr

 ubfx	ip, r0, #3, #10		             //get numways - 1 from [to be discarded] CCSIDR
 clz	r2, ip			                 //number of bits to MSB of way
 lsl	ip, ip, r2		                 //shift by that into way position
 mov	r0, #1			
 lsl	r2, r0, r2		                 //r2 now contains the way decr
 mov	r0, r3 			                 //get sets/level (no way yet)
 orr	r3, r3, ip		                 //merge way into way/set/level
 bfc	r0, #0, #4		                 //clear low 4 bits (level) to get numset - 1
 sub	r2, r2, r0		                 //subtract from way decr

 //r3 = ways/sets/level, r2 = way decr, r1 = set decr, r0 and ip are free
.LLoop:    
 mcr	p15, #0, r3, cr7, cr14, #2	     //clean and invalidate line
 cmp	r3, #15			                 //are we done with this level (way/set == 0)
 bls	.LNext	                         //yes, go to next level
 ubfx	r0, r3, #4, #18		             //extract set bits
 cmp	r0, #0			                 //compare
 subne	r3, r3, r1		                 //non-zero?, decrement set #
 subeq	r3, r3, r2		                 //zero?, decrement way # and restore set count
 b  .LLoop

.LNext:
 dsb
 mrc	p15, #1, r0, cr0, cr0, #1	     //read CLIDR
 ubfx	ip, r0, #24, #3		             //narrow to LoC
 add	r3, r3, #2		                 //go to next level
 cmp	r3, ip, lsl #1		             //compare
 blt	.LStart		                     //not done, next level (r0 == CLIDR)

.LDone:
 mov	r0, #0			                 //default back to cache level 0
 mcr	p15, #2, r0, cr0, cr0, #0	     //select cache level
 dsb
 isb
end;

{==============================================================================}

procedure ARMv7InvalidateInstructionCache; assembler; nostackframe;
{Perform an invalidate entire instruction cache operation using the c7 (Cache Operations) register of system control coprocessor CP15
 See page B3-127 of the ARMv7 Architecture Reference Manual}
asm
 //Get the MPID register from the system control coprocessor CP15
 mrc p15, #0, r0, cr0, cr0, #5
 
 //Mask off the Multiprocessor Extensions bit (MPE)
 and r1, r0, #ARMV7_CP15_C0_MPID_MPE
 cmp r1, #0
 beq .LUniprocessor
 
 //Mask off the Uniprocessor bit (U)
 and r1, r0, #ARMV7_CP15_C0_MPID_U_UNIPROCESSOR
 cmp r1, #0
 bne .LUniprocessor
 
 //Invalidate all instruction caches to PoU (Inner Shareable)
 mov r12, #0
 mcr p15, #0, r12, cr7, cr1, #0

 //Perform a data synchronization barrier
 dsb
 
 //Return to caller
 bx lr
 
.LUniprocessor: 
 //Invalidate all instruction caches to PoU
 mov r12, #0
 mcr p15, #0, r12, cr7, cr5, #0
  
 //Perform a data synchronization barrier
 dsb
end;

{==============================================================================}

procedure ARMv7CleanDataCacheRange(Address,Size:LongWord); assembler; nostackframe;
{Perform a clean data cache by MVA to PoC operation
 See page B3-127 of the ARMv7 Architecture Reference Manual}
asm
 mov r12, #0
 mcr p15, #2, r12, cr0, cr0, #0	    //Set CSSELR to L1
 mrc p15, #1, r2, cr0, cr0, #0	    //Read CCSIDR
 and r2, r2, #7		                //get line size (log2(size)-4, 0=16)
 mov	r12, #16		            //make a bit mask
 lsl	r2, r12, r2		            //and shift into position
 sub	r12, r2, #1		            //make into a mask
 and	r3, r0, r12		            //get offset into cache line
 add	r1, r1, r3		            //add to length
 bic	r0, r0, r12		            //clear offset from start
.LLoop:
 mcr	p15, #0, r0, cr7, cr10, #1	//Clean the D-Cache line
 add	r0, r0, r2
 subs	r1, r1, r2
 bhi	.LLoop

 dsb				                //Data Synchronization Barrier
 bx	lr
end;

{==============================================================================}

procedure ARMv7InvalidateDataCacheRange(Address,Size:LongWord); assembler; nostackframe;
{Perform an invalidate data cache by MVA to PoC operation
 See page B3-127 of the ARMv7 Architecture Reference Manual}
asm
 mov r12, #0
 mcr p15, #2, r12, cr0, cr0, #0	    //Set CSSELR to L1
 mrc p15, #1, r2, cr0, cr0, #0	    //Read CCSIDR
 and r2, r2, #7		                //get line size (log2(size)-4, 0=16)
 mov	r12, #16		            //make a bit mask
 lsl	r2, r12, r2		            //and shift into position
 sub	r12, r2, #1		            //make into a mask
 and	r3, r0, r12		            //get offset into cache line
 add	r1, r1, r3		            //add to length
 bic	r0, r0, r12		            //clear offset from start
.LLoop:
 mcr	p15, #0, r0, cr7, cr6, #1	//Invalidate the D-Cache line
 add	r0, r0, r2
 subs	r1, r1, r2
 bhi	.LLoop

 dsb				                //Data Synchronization Barrier
 bx	lr
end;

{==============================================================================}

procedure ARMv7CleanAndInvalidateDataCacheRange(Address,Size:LongWord); assembler; nostackframe;
{Perform a clean and invalidate data cache by MVA to PoC operation
 See page B3-127 of the ARMv7 Architecture Reference Manual}
asm
 mov r12, #0
 mcr p15, #2, r12, cr0, cr0, #0	    //Set CSSELR to L1
 mrc p15, #1, r2, cr0, cr0, #0	    //Read CCSIDR
 and r2, r2, #7		                //get line size (log2(size)-4, 0=16)
 mov	r12, #16		            //make a bit mask
 lsl	r2, r12, r2		            //and shift into position
 sub	r12, r2, #1		            //make into a mask
 and	r3, r0, r12		            //get offset into cache line
 add	r1, r1, r3		            //add to length
 bic	r0, r0, r12		            //clear offset from start
.LLoop:
 mcr	p15, #0, r0, cr7, cr14, #1	//Clean and Invalidate the D-Cache line
 add	r0, r0, r2
 subs	r1, r1, r2
 bhi	.LLoop

 dsb				                //Data Synchronization Barrier
 bx	lr
end;

{==============================================================================}

procedure ARMv7InvalidateInstructionCacheRange(Address,Size:LongWord); assembler; nostackframe; 
{Perform an invalidate instruction caches by MVA to PoU operation
 See page B3-127 of the ARMv7 Architecture Reference Manual}
asm
 mov  r12, #0
 mcr  p15, #2, r12, c0, c0, #0      //Set CSSELR to L1
 mrc  p15, #1, r2, c0, c0, #0       //Read CCSIDR
 and  r2, r2, #7                    //get line size (log2(size)-4, 0=16)
 mov  r12, #16                      //make a bit mask
 lsl  r2, r12, r2                   //and shift into position
 sub  r12, r2, #1                   //make into a mask
 and  r3, r0, r12                   //get offset into cache line
 add  r1, r1, r3                    //add to length
 bic  r0, r0, r12                   //clear offset from start.
 dsb				                //Data Synchronization Barrier 
.LLoop:
 mcr  p15, #0, r0, c7, c5, #1       //Invalidate the I-Cache line
 //mcr  p15, #0, r0, c7, c14, #1    //Clean and Invalidate the D-Cache line (See above)
 add  r0, r0, r2
 subs r1, r1, r2
 bhi  .LLoop

 dsb				                //Data Synchronization Barrier
 isb				                //Instructions Synchronization Barrier
 bx lr
end;

{==============================================================================}

procedure ARMv7CleanDataCacheSetWay(SetWay:LongWord); assembler; nostackframe;
{Perform a clean data cache line by set/way operation
 See page B3-127 of the ARMv7 Architecture Reference Manual}
{Set/Way/Level will be passed in r0}
asm
 mcr p15, #0, r0, cr7, cr10, #2
end;

{==============================================================================}

procedure ARMv7InvalidateDataCacheSetWay(SetWay:LongWord); assembler; nostackframe;
{Perform an invalidate data cache line by set/way operation
 See page B3-127 of the ARMv7 Architecture Reference Manual}
{Set/Way/Level will be passed in r0}
asm
 mcr p15, #0, r0, cr7, cr6, #2  
end;

{==============================================================================}

procedure ARMv7CleanAndInvalidateDataCacheSetWay(SetWay:LongWord); assembler; nostackframe;
{Perform a clean and invalidate data cache line by set/way operation
 See page B3-127 of the ARMv7 Architecture Reference Manual}
{Set/Way/Level will be passed in r0} 
asm
 mcr p15, #0, r0, cr7, cr14, #2 
end;

{==============================================================================}

procedure ARMv7FlushPrefetchBuffer; assembler; nostackframe;
{Perform an Instruction Synchronization Barrier operation. See page A8-102 of the ARMv7 Architecture Reference Manual}
asm
 //ARMv7 "instruction synchronization barrier" instruction.
 isb
end;

{==============================================================================}

procedure ARMv7FlushBranchTargetCache; assembler; nostackframe; 
{Perform a Flush Entire Branch Target Cache operation. See page B3-127 of the ARMv7 Architecture Reference Manual}
asm
 //Get the MPID register from the system control coprocessor CP15
 mrc p15, #0, r0, cr0, cr0, #5
 
 //Mask off the Multiprocessor Extensions bit (MPE)
 and r1, r0, #ARMV7_CP15_C0_MPID_MPE
 cmp r1, #0
 beq .LUniprocessor
 
 //Mask off the Uniprocessor bit (U)
 and r1, r0, #ARMV7_CP15_C0_MPID_U_UNIPROCESSOR
 cmp r1, #0
 bne .LUniprocessor
 
 //Flush Entire Branch Target Cache (Inner Shareable)
 mov r12, #0
 mcr p15, #0, r12, cr7, cr1, #6
 
 //Perform a data synchronization barrier
 dsb
 
 //Return to caller
 bx lr
 
.LUniprocessor: 
 //Flush Entire Branch Target Cache
 mov r12, #0
 mcr p15, #0, r12, cr7, cr5, #6
 
 //Perform a data synchronization barrier
 dsb
end;

{==============================================================================}

procedure ARMv7ContextSwitch(OldStack,NewStack:Pointer;NewThread:TThreadHandle);  assembler; nostackframe;
{Perform a context switch from one thread to another as a result of a thread yielding, sleeping or waiting}
{OldStack: The address to save the stack pointer to for the current thread (Passed in r0)}
{NewStack: The address to restore the stack pointer from for the new thread (Passed in r1)}
{NewThread: The handle of the new thread to switch to (Passed in r2)}
{Notes: At the point of the actual context switch (str sp / ldr sp) the thread stacks will look like this:
        (See: ARMv7ThreadSetupStack for additional information)

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
 //To Do //Critical //Account for differences from ARMV7 (eg VFP3)

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
 
 //Perform a CLREX to clear the Local Monitor status to Open 
 clrex

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
 
 //Note: Compiler adds "mov pc, lr" or "bx lr" to the end of this. Should not be an issue due to pop pc above
end;

{==============================================================================}

procedure ARMv7ContextSwitchIRQ(OldStack,NewStack:Pointer;NewThread:TThreadHandle);  assembler; nostackframe;
{Perform a context switch from one thread to another as a result of an interrupt request (IRQ)}
{OldStack: The address to save the stack pointer to for the current thread (Passed in r0)}
{NewStack: The address to restore the stack pointer from for the new thread (Passed in r1)}
{NewThread: The handle of the new thread to switch to (Passed in r2)}
{Notes: At the point of the actual context switch (str sp / ldr sp) the thread stacks will look like this:
        (See: ARMv7ThreadSetupStack for additional information)

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
 //To Do //Critical //Account for differences from ARMV7 (eg VFP3) 
 
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

 //Perform a CLREX to clear the Local Monitor status to Open
 clrex
 
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

 //Note: Compiler adds "mov pc, lr" or "bx lr" to the end of this. Should not be an issue due to bx lr above
end;

{==============================================================================}

procedure ARMv7ContextSwitchFIQ(OldStack,NewStack:Pointer;NewThread:TThreadHandle); assembler; nostackframe;
{Perform a context switch from one thread to another as a result of a fast interrupt request (FIQ)}
{OldStack: The address to save the stack pointer to for the current thread (Passed in r0)}
{NewStack: The address to restore the stack pointer from for the new thread (Passed in r1)}
{NewThread: The handle of the new thread to switch to (Passed in r2)}
{Notes: At the point of the actual context switch (str sp / ldr sp) the thread stacks will look like this:
        (See: ARMv7ThreadSetupStack for additional information)

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
 //To Do //Critical //Account for differences from ARMV7 (eg VFP3)
 
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

 //Perform a CLREX to clear the Local Monitor status to Open
 clrex
 
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
 
 //Note: Compiler adds "mov pc, lr" or "bx lr" to the end of this. Should not be an issue due to bx lr above
end;

{==============================================================================}

procedure ARMv7ContextSwitchSWI(OldStack,NewStack:Pointer;NewThread:TThreadHandle); assembler; nostackframe; 
{Perform a context switch from one thread to another as a result of a software interrupt (SWI)}
{OldStack: The address to save the stack pointer to for the current thread (Passed in r0)}
{NewStack: The address to restore the stack pointer from for the new thread (Passed in r1)}
{NewThread: The handle of the new thread to switch to (Passed in r2)}
{Notes: At the point of the actual context switch (str sp / ldr sp) the thread stacks will look like this:
        (See: ARMv7ThreadSetupStack for additional information)

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
 //To Do //Critical //Account for differences from ARMV7 (eg VFP3)
 
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

 //Perform a CLREX to clear the Local Monitor status to Open
 clrex
 
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
 
 //Note: Compiler adds "mov pc, lr" or "bx lr" to the end of this. Should not be an issue due to bx lr above
end;

{==============================================================================}

function ARMv7InterlockedOr(var Target:LongInt;Value:LongInt):LongInt; assembler; nostackframe;
{Perform an atomic OR operation using LDREX/STREX. See page ???}
asm
.LLoop:
 ldrex r2, [r0]
 orr   r12, r1, r2
 strex r3, r12, [r0]
 cmp r3, #0
 bne .LLoop
 
 dmb 
 mov  r0, r2
end;

{==============================================================================}

function ARMv7InterlockedXor(var Target:LongInt;Value:LongInt):LongInt; assembler; nostackframe;
{Perform an atomic XOR operation using LDREX/STREX. See page ???}
asm
.LLoop:
 ldrex r2, [r0]
 eor   r12, r1, r2
 strex r3, r12, [r0]
 cmp r3, #0
 bne .LLoop
 
 dmb 
 mov  r0, r2
end;

{==============================================================================}

function ARMv7InterlockedAnd(var Target:LongInt;Value:LongInt):LongInt; assembler; nostackframe;
{Perform an atomic AND operation using LDREX/STREX. See page ???}
asm
.LLoop:
 ldrex r2, [r0]
 and   r12, r1, r2
 strex r3, r12, [r0]
 cmp r3, #0
 bne .LLoop
 
 dmb 
 mov  r0, r2
end;

{==============================================================================}

function ARMv7InterlockedDecrement(var Target:LongInt):LongInt; assembler; nostackframe;
{Perform an atomic decrement operation using LDREX/STREX. See page ???}
asm
.LLoop:
 ldrex r1, [r0]
 sub   r1, r1, #1
 strex r2, r1, [r0]
 cmp r2, #0
 bne .LLoop
 
 dmb
 movs r0, r1
end;

{==============================================================================}

function ARMv7InterlockedIncrement(var Target:LongInt):LongInt; assembler; nostackframe;
{Perform an atomic increment operation using LDREX/STREX. See page ???}
asm
.LLoop:
 ldrex r1, [r0]
 add   r1, r1, #1
 strex r2, r1, [r0]
 cmp r2, #0
 bne .LLoop
 
 dmb
 mov r0, r1
end;

{==============================================================================}

function ARMv7InterlockedExchange(var Target:LongInt;Source:LongInt):LongInt; assembler; nostackframe;
{Perform an atomic exchange operation using LDREX/STREX. See page ???}
asm
.LLoop:
 ldrex r2, [r0]
 strex r3, r1, [r0]
 cmp r3, #0
 bne .LLoop
 
 dmb
 mov r0, r2
end;

{==============================================================================}

function ARMv7InterlockedAddExchange(var Target:LongInt;Source:LongInt):LongInt; assembler; nostackframe;
{Perform an atomic add and exchange operation using LDREX/STREX. See page ???}
asm
.LLoop:
 ldrex r2, [r0]
 add   r12, r1, r2
 strex r3, r12, [r0]
 cmp r3, #0
 bne .LLoop
 
 dmb
 mov  r0, r2
end;

{==============================================================================}

function ARMv7InterlockedCompareExchange(var Target:LongInt;Source,Compare:LongInt):LongInt; assembler; nostackframe;
{Perform an atomic compare and exchange operation using LDREX/STREX. See page ???}
asm
.LLoop:
 ldrex    r3, [r0]
 mov      r12, #0
 cmp      r3, r2
 strexeq  r12, r1, [r0]
 cmp      r12, #0
 bne      .LLoop
 
 dmb
 mov      r0, r3
end;

{==============================================================================}

function ARMv7PageTableGetEntry(Address:PtrUInt):TPageTableEntry;
{Get and Decode the entry in the Page Table that corresponds to the supplied virtual address}
var
 TableEntry:LongWord;
begin
 {}
 FillChar(Result,SizeOf(TPageTableEntry),0);
 
 {Check Address}
 {Zero may be valid}
 
 {Get Coarse}
 TableEntry:=ARMv7GetPageTableCoarse(Address);
 if TableEntry <> 0 then
  begin
   {Get Small}
   TableEntry:=ARMv7GetPageTableSmall(Address);
   if TableEntry <> 0 then
    begin
     {Get Virtual Address}
     Result.VirtualAddress:=(Address and ARMV7_L2D_SMALL_BASE_MASK);

     {Get Physical Address and Size}
     Result.PhysicalAddress:=(TableEntry and ARMV7_L2D_SMALL_BASE_MASK);
     Result.Size:=SIZE_4K;
    
     {Get Flags} {ARMv7 uses the TEX Remap L2D values (Not Standard or Cacheable values)}
     Result.Flags:=PAGE_TABLE_FLAG_NONE;
    
     {Check Normal/Cacheable/WriteBack/WriteThrough}     
     if (TableEntry and (ARMV7_L2D_SMALL_TEX_MASK or ARMV7_L2D_FLAG_C or ARMV7_L2D_FLAG_B)) = ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_NONCACHED then
      begin
       Result.Flags:=Result.Flags or PAGE_TABLE_FLAG_NORMAL;
      end
     else if (TableEntry and (ARMV7_L2D_SMALL_TEX_MASK or ARMV7_L2D_FLAG_C or ARMV7_L2D_FLAG_B)) = ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_BACK then 
      begin
       Result.Flags:=Result.Flags or PAGE_TABLE_FLAG_NORMAL or PAGE_TABLE_FLAG_CACHEABLE or PAGE_TABLE_FLAG_WRITEBACK;
      end
     else if (TableEntry and (ARMV7_L2D_SMALL_TEX_MASK or ARMV7_L2D_FLAG_C or ARMV7_L2D_FLAG_B)) = ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_THROUGH then 
      begin
       Result.Flags:=Result.Flags or PAGE_TABLE_FLAG_NORMAL or PAGE_TABLE_FLAG_CACHEABLE or PAGE_TABLE_FLAG_WRITETHROUGH;
      end
     else if (TableEntry and (ARMV7_L2D_SMALL_TEX_MASK or ARMV7_L2D_FLAG_C or ARMV7_L2D_FLAG_B)) = ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_ALLOCATE then 
      begin
       Result.Flags:=Result.Flags or PAGE_TABLE_FLAG_NORMAL or PAGE_TABLE_FLAG_CACHEABLE or PAGE_TABLE_FLAG_WRITEALLOCATE;
      end;
     
     {Check Device}
     if (TableEntry and (ARMV7_L2D_SMALL_TEX_MASK or ARMV7_L2D_FLAG_C or ARMV7_L2D_FLAG_B)) = ARMV7_L2D_SMALL_CACHE_REMAP_DEVICE then
      begin
       Result.Flags:=Result.Flags or PAGE_TABLE_FLAG_DEVICE;
      end;
     
     {Check Ordered}
     if (TableEntry and (ARMV7_L2D_SMALL_TEX_MASK or ARMV7_L2D_FLAG_C or ARMV7_L2D_FLAG_B)) = ARMV7_L2D_SMALL_CACHE_REMAP_STRONGLY_ORDERED then
      begin
       Result.Flags:=Result.Flags or PAGE_TABLE_FLAG_ORDERED;
      end;
     
     {Check Shared}
     if (TableEntry and ARMV7_L2D_FLAG_SHARED) = ARMV7_L2D_FLAG_SHARED then
      begin
       Result.Flags:=Result.Flags or PAGE_TABLE_FLAG_SHARED;
      end;
     
     {Check ReadOnly}
     if (TableEntry and (ARMV7_L2D_AP_MASK or ARMV7_L2D_FLAG_AP2)) = ARMV7_L2D_ACCESS_READONLY then
      begin
       Result.Flags:=Result.Flags or PAGE_TABLE_FLAG_READONLY;
      end;
     
     {Check ReadWrite}
     if (TableEntry and (ARMV7_L2D_AP_MASK or ARMV7_L2D_FLAG_AP2)) = ARMV7_L2D_ACCESS_READWRITE then
      begin
       Result.Flags:=Result.Flags or PAGE_TABLE_FLAG_READWRITE;
      end;
     
     {Check Executable}
     if (TableEntry and ARMV7_L2D_FLAG_SMALL_XN) <> ARMV7_L2D_FLAG_SMALL_XN then
      begin
       Result.Flags:=Result.Flags or PAGE_TABLE_FLAG_EXECUTABLE;
      end;
    end
   else
    begin   
     {Get Large}
     TableEntry:=ARMv7GetPageTableLarge(Address);
     if TableEntry <> 0 then
      begin
       {Get Virtual Address}
       Result.VirtualAddress:=(Address and ARMV7_L2D_LARGE_BASE_MASK);
       
       {Get Physical Address and Size}
       Result.PhysicalAddress:=(TableEntry and ARMV7_L2D_LARGE_BASE_MASK);
       Result.Size:=SIZE_64K;
       
       {Get Flags} {ARMv7 uses the TEX Remap L2D values (Not Standard or Cacheable values)}
       Result.Flags:=PAGE_TABLE_FLAG_NONE;
       
       {Check Normal/Cacheable/WriteBack/WriteThrough}     
       if (TableEntry and (ARMV7_L2D_LARGE_TEX_MASK or ARMV7_L2D_FLAG_C or ARMV7_L2D_FLAG_B)) = ARMV7_L2D_LARGE_CACHE_REMAP_NORMAL_NONCACHED then
        begin
         Result.Flags:=Result.Flags or PAGE_TABLE_FLAG_NORMAL;
        end
       else if (TableEntry and (ARMV7_L2D_LARGE_TEX_MASK or ARMV7_L2D_FLAG_C or ARMV7_L2D_FLAG_B)) = ARMV7_L2D_LARGE_CACHE_REMAP_NORMAL_WRITE_BACK then 
        begin
         Result.Flags:=Result.Flags or PAGE_TABLE_FLAG_NORMAL or PAGE_TABLE_FLAG_CACHEABLE or PAGE_TABLE_FLAG_WRITEBACK;
        end
       else if (TableEntry and (ARMV7_L2D_LARGE_TEX_MASK or ARMV7_L2D_FLAG_C or ARMV7_L2D_FLAG_B)) = ARMV7_L2D_LARGE_CACHE_REMAP_NORMAL_WRITE_THROUGH then 
        begin
         Result.Flags:=Result.Flags or PAGE_TABLE_FLAG_NORMAL or PAGE_TABLE_FLAG_CACHEABLE or PAGE_TABLE_FLAG_WRITETHROUGH;
        end
       else if (TableEntry and (ARMV7_L2D_LARGE_TEX_MASK or ARMV7_L2D_FLAG_C or ARMV7_L2D_FLAG_B)) = ARMV7_L2D_LARGE_CACHE_REMAP_NORMAL_WRITE_ALLOCATE then 
        begin
         Result.Flags:=Result.Flags or PAGE_TABLE_FLAG_NORMAL or PAGE_TABLE_FLAG_CACHEABLE or PAGE_TABLE_FLAG_WRITEALLOCATE;
        end;
       
       {Check Device}
       if (TableEntry and (ARMV7_L2D_LARGE_TEX_MASK or ARMV7_L2D_FLAG_C or ARMV7_L2D_FLAG_B)) = ARMV7_L2D_LARGE_CACHE_REMAP_DEVICE then
        begin
         Result.Flags:=Result.Flags or PAGE_TABLE_FLAG_DEVICE;
        end;
       
       {Check Ordered}
       if (TableEntry and (ARMV7_L2D_LARGE_TEX_MASK or ARMV7_L2D_FLAG_C or ARMV7_L2D_FLAG_B)) = ARMV7_L2D_LARGE_CACHE_REMAP_STRONGLY_ORDERED then
        begin
         Result.Flags:=Result.Flags or PAGE_TABLE_FLAG_ORDERED;
        end;
       
       {Check Shared}
       if (TableEntry and ARMV7_L2D_FLAG_SHARED) = ARMV7_L2D_FLAG_SHARED then
        begin
         Result.Flags:=Result.Flags or PAGE_TABLE_FLAG_SHARED;
        end;
       
       {Check ReadOnly}
       if (TableEntry and (ARMV7_L2D_AP_MASK or ARMV7_L2D_FLAG_AP2)) = ARMV7_L2D_ACCESS_READONLY then
        begin
         Result.Flags:=Result.Flags or PAGE_TABLE_FLAG_READONLY;
        end;
       
       {Check ReadWrite}
       if (TableEntry and (ARMV7_L2D_AP_MASK or ARMV7_L2D_FLAG_AP2)) = ARMV7_L2D_ACCESS_READWRITE then
        begin
         Result.Flags:=Result.Flags or PAGE_TABLE_FLAG_READWRITE;
        end;
       
       {Check Executable}
       if (TableEntry and ARMV7_L2D_FLAG_LARGE_XN) <> ARMV7_L2D_FLAG_LARGE_XN then
        begin
         Result.Flags:=Result.Flags or PAGE_TABLE_FLAG_EXECUTABLE;
        end;
      end;
    end;  
  end
 else
  begin 
   {Get Section}
   TableEntry:=ARMv7GetPageTableSection(Address);
   if TableEntry <> 0 then
    begin
     {Check Supersection}
     if (TableEntry and ARMV7_L1D_FLAG_SUPERSECTION) = 0 then
      begin
       {Get Virtual Address}
       Result.VirtualAddress:=(Address and ARMV7_L1D_SECTION_BASE_MASK);
      
       {Get Physical Address and Size}
       Result.PhysicalAddress:=(TableEntry and ARMV7_L1D_SECTION_BASE_MASK);
       Result.Size:=SIZE_1M;
      end
     else
      begin
       {Get Virtual Address}
       Result.VirtualAddress:=(Address and ARMV7_L1D_SUPERSECTION_BASE_MASK);

       {Get Physical Address and Size}
       Result.PhysicalAddress:=(TableEntry and ARMV7_L1D_SUPERSECTION_BASE_MASK);
       Result.Size:=SIZE_16M;
      end;      
     
     {Get Flags} {ARMv7 uses the TEX Remap L1D values (Not Standard or Cacheable values)}
     Result.Flags:=PAGE_TABLE_FLAG_NONE;
     
     {Check Normal/Cacheable/WriteBack/WriteThrough}     
     if (TableEntry and (ARMV7_L1D_TEX_MASK or ARMV7_L1D_FLAG_C or ARMV7_L1D_FLAG_B)) = ARMV7_L1D_CACHE_REMAP_NORMAL_NONCACHED then
      begin
       Result.Flags:=Result.Flags or PAGE_TABLE_FLAG_NORMAL;
      end
     else if (TableEntry and (ARMV7_L1D_TEX_MASK or ARMV7_L1D_FLAG_C or ARMV7_L1D_FLAG_B)) = ARMV7_L1D_CACHE_REMAP_NORMAL_WRITE_BACK then 
      begin
       Result.Flags:=Result.Flags or PAGE_TABLE_FLAG_NORMAL or PAGE_TABLE_FLAG_CACHEABLE or PAGE_TABLE_FLAG_WRITEBACK;
      end
     else if (TableEntry and (ARMV7_L1D_TEX_MASK or ARMV7_L1D_FLAG_C or ARMV7_L1D_FLAG_B)) = ARMV7_L1D_CACHE_REMAP_NORMAL_WRITE_THROUGH then 
      begin
       Result.Flags:=Result.Flags or PAGE_TABLE_FLAG_NORMAL or PAGE_TABLE_FLAG_CACHEABLE or PAGE_TABLE_FLAG_WRITETHROUGH;
      end
     else if (TableEntry and (ARMV7_L1D_TEX_MASK or ARMV7_L1D_FLAG_C or ARMV7_L1D_FLAG_B)) = ARMV7_L1D_CACHE_REMAP_NORMAL_WRITE_ALLOCATE then 
      begin
       Result.Flags:=Result.Flags or PAGE_TABLE_FLAG_NORMAL or PAGE_TABLE_FLAG_CACHEABLE or PAGE_TABLE_FLAG_WRITEALLOCATE;
      end;
     
     {Check Device}
     if (TableEntry and (ARMV7_L1D_TEX_MASK or ARMV7_L1D_FLAG_C or ARMV7_L1D_FLAG_B)) = ARMV7_L1D_CACHE_REMAP_DEVICE then
      begin
       Result.Flags:=Result.Flags or PAGE_TABLE_FLAG_DEVICE;
      end;
     
     {Check Ordered}
     if (TableEntry and (ARMV7_L1D_TEX_MASK or ARMV7_L1D_FLAG_C or ARMV7_L1D_FLAG_B)) = ARMV7_L1D_CACHE_REMAP_STRONGLY_ORDERED then
      begin
       Result.Flags:=Result.Flags or PAGE_TABLE_FLAG_ORDERED;
      end;
     
     {Check Shared}
     if (TableEntry and ARMV7_L1D_FLAG_SHARED) = ARMV7_L1D_FLAG_SHARED then
      begin
       Result.Flags:=Result.Flags or PAGE_TABLE_FLAG_SHARED;
      end;
     
     {Check ReadOnly}
     if (TableEntry and (ARMV7_L1D_AP_MASK or ARMV7_L1D_FLAG_AP2)) = ARMV7_L1D_ACCESS_READONLY then
      begin
       Result.Flags:=Result.Flags or PAGE_TABLE_FLAG_READONLY;
      end;
     
     {Check ReadWrite}
     if (TableEntry and (ARMV7_L1D_AP_MASK or ARMV7_L1D_FLAG_AP2)) = ARMV7_L1D_ACCESS_READWRITE then
      begin
       Result.Flags:=Result.Flags or PAGE_TABLE_FLAG_READWRITE;
      end;
     
     {Check Executable}
     if (TableEntry and ARMV7_L1D_FLAG_XN) <> ARMV7_L1D_FLAG_XN then
      begin
       Result.Flags:=Result.Flags or PAGE_TABLE_FLAG_EXECUTABLE;
      end;
    end;
  end; 
end;

{==============================================================================}

function ARMv7PageTableSetEntry(const Entry:TPageTableEntry):LongWord;
{Encode and Set an entry in the Page Table that corresponds to the supplied virtual address}
var
 CoarseBase:PtrUInt;
 
 TableBase:PtrUInt;
 TableFlags:LongWord;
 TableEntry:LongWord;
 
 ReadMask:LongWord;
 RemapMask:LongWord;
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
     TableEntry:=ARMv7GetPageTableCoarse(Entry.VirtualAddress);
     if TableEntry = 0 then
      begin
       {Allocate Coarse}
       if PAGE_TABLES_FREE = 0 then Exit;
       
       {Update Free/Used}
       Dec(PAGE_TABLES_FREE);
       Inc(PAGE_TABLES_USED);
       
       {Get Table Base}
       TableBase:=(Entry.VirtualAddress and ARMV7_L1D_SECTION_BASE_MASK);
       
       {Get Coarse Base}
       CoarseBase:=(PAGE_TABLES_NEXT and ARMV7_L1D_COARSE_BASE_MASK);
       
       {Update Next}
       Inc(PAGE_TABLES_NEXT,SIZE_1K);
       
       {Set Coarse}
       if not ARMv7SetPageTableCoarse(TableBase,CoarseBase,0) then
        begin
         {Reset Free/Used/Next}
         Inc(PAGE_TABLES_FREE);
         Dec(PAGE_TABLES_USED);
         Dec(PAGE_TABLES_NEXT,SIZE_1K);
         
         Exit;
        end;
       
       {Clean Data Cache Range (Coarse Page)}
       ARMv7CleanDataCacheRange(CoarseBase,SIZE_1K);
       
       {Clean Data Cache Range (Page Table)}
       ARMv7CleanDataCacheRange(PAGE_TABLE_BASE,PAGE_TABLE_SIZE);
       
       {Invalidate TLB}
       ARMv7InvalidateTLB;
      end
     else
      begin
       {Get Coarse Base}
       CoarseBase:=(TableEntry and ARMV7_L1D_COARSE_BASE_MASK);
      end;     
      
     {Get Small}
     TableEntry:=ARMv7GetPageTableSmall(Entry.VirtualAddress);
     if TableEntry = 0 then
      begin
       {Not Supported}
       Exit;
      end;
      
     {Get Masks}
     ReadMask:=PAGE_TABLE_FLAG_READONLY or PAGE_TABLE_FLAG_READWRITE;
     RemapMask:=PAGE_TABLE_FLAG_NORMAL or PAGE_TABLE_FLAG_DEVICE or PAGE_TABLE_FLAG_ORDERED or PAGE_TABLE_FLAG_CACHEABLE or PAGE_TABLE_FLAG_WRITEBACK or PAGE_TABLE_FLAG_WRITETHROUGH or PAGE_TABLE_FLAG_WRITEALLOCATE;
     
     {Get Flags}
     TableFlags:=0;
     
     {Check Normal/Cacheable/WriteBack/WriteThrough/Device/Ordered}     
     if (Entry.Flags and RemapMask) = PAGE_TABLE_FLAG_NORMAL then
      begin
       TableFlags:=TableFlags or ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_NONCACHED;
      end
     else if (Entry.Flags and RemapMask) = (PAGE_TABLE_FLAG_NORMAL or PAGE_TABLE_FLAG_CACHEABLE or PAGE_TABLE_FLAG_WRITEBACK) then 
      begin
       TableFlags:=TableFlags or ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_BACK;
      end
     else if (Entry.Flags and RemapMask) = (PAGE_TABLE_FLAG_NORMAL or PAGE_TABLE_FLAG_CACHEABLE or PAGE_TABLE_FLAG_WRITETHROUGH) then  
      begin
       TableFlags:=TableFlags or ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_THROUGH;
      end
     else if (Entry.Flags and RemapMask) = (PAGE_TABLE_FLAG_NORMAL or PAGE_TABLE_FLAG_CACHEABLE or PAGE_TABLE_FLAG_WRITEALLOCATE) then
      begin
       TableFlags:=TableFlags or ARMV7_L2D_SMALL_CACHE_REMAP_NORMAL_WRITE_ALLOCATE;
      end
     else if (Entry.Flags and RemapMask) = PAGE_TABLE_FLAG_DEVICE then
      begin
       TableFlags:=TableFlags or ARMV7_L2D_SMALL_CACHE_REMAP_DEVICE;
      end
     else if (Entry.Flags and RemapMask) = PAGE_TABLE_FLAG_ORDERED then
      begin
       TableFlags:=TableFlags or ARMV7_L2D_SMALL_CACHE_REMAP_STRONGLY_ORDERED;
      end
     else 
      begin
       {Not Supported}
       Exit;
      end;      
     
     {Check Shared}
     if (Entry.Flags and PAGE_TABLE_FLAG_SHARED) = PAGE_TABLE_FLAG_SHARED then
      begin
       TableFlags:=TableFlags or ARMV7_L2D_FLAG_SHARED;
      end; 
     
     {Check NoAccess / ReadOnly / ReadWrite}
     if (Entry.Flags and ReadMask) = PAGE_TABLE_FLAG_NONE then
      begin
       {Nothing}
      end
     else if (Entry.Flags and ReadMask) = PAGE_TABLE_FLAG_READONLY then
      begin
       TableFlags:=TableFlags or ARMV7_L2D_ACCESS_READONLY;
      end
     else if (Entry.Flags and ReadMask) = PAGE_TABLE_FLAG_READWRITE then 
      begin
       TableFlags:=TableFlags or ARMV7_L2D_ACCESS_READWRITE;
      end
     else
      begin
       {Not Supported}
       Exit;
      end;
     
     {Check Executable}
     if (Entry.Flags and PAGE_TABLE_FLAG_EXECUTABLE) <> PAGE_TABLE_FLAG_EXECUTABLE then
      begin
       TableFlags:=TableFlags or ARMV7_L2D_FLAG_SMALL_XN;
      end;
     
     {Update Small}
     if ARMv7SetPageTableSmall(Entry.VirtualAddress,Entry.PhysicalAddress,TableFlags) then
      begin
       {Clean Data Cache Range (Coarse Page)}
       ARMv7CleanDataCacheRange(CoarseBase,SIZE_1K);
       
       {Clean Data Cache Range (Page Table)}
       ARMv7CleanDataCacheRange(PAGE_TABLE_BASE,PAGE_TABLE_SIZE);
       
       {Invalidate TLB}
       ARMv7InvalidateTLB;
       
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
     TableEntry:=ARMv7GetPageTableSection(Entry.VirtualAddress);
     if TableEntry = 0 then
      begin
       {Not Supported}
       Exit;
      end
     else
      begin
       {Check Supersection}
       if (TableEntry and ARMV7_L1D_FLAG_SUPERSECTION) <> 0 then
        begin
         {Not Supported}
         Exit;
        end;
      end;      
     
     {Get Masks}     
     ReadMask:=PAGE_TABLE_FLAG_READONLY or PAGE_TABLE_FLAG_READWRITE;
     RemapMask:=PAGE_TABLE_FLAG_NORMAL or PAGE_TABLE_FLAG_DEVICE or PAGE_TABLE_FLAG_ORDERED or PAGE_TABLE_FLAG_CACHEABLE or PAGE_TABLE_FLAG_WRITEBACK or PAGE_TABLE_FLAG_WRITETHROUGH or PAGE_TABLE_FLAG_WRITEALLOCATE;
     
     {Get Flags}
     TableFlags:=0;
     
     {Check Normal/Cacheable/WriteBack/WriteThrough/Device/Ordered}
     if (Entry.Flags and RemapMask) = PAGE_TABLE_FLAG_NORMAL then
      begin
       TableFlags:=TableFlags or ARMV7_L1D_CACHE_REMAP_NORMAL_NONCACHED;
      end
     else if (Entry.Flags and RemapMask) = (PAGE_TABLE_FLAG_NORMAL or PAGE_TABLE_FLAG_CACHEABLE or PAGE_TABLE_FLAG_WRITEBACK) then 
      begin
       TableFlags:=TableFlags or ARMV7_L1D_CACHE_REMAP_NORMAL_WRITE_BACK;
      end
     else if (Entry.Flags and RemapMask) = (PAGE_TABLE_FLAG_NORMAL or PAGE_TABLE_FLAG_CACHEABLE or PAGE_TABLE_FLAG_WRITETHROUGH) then  
      begin
       TableFlags:=TableFlags or ARMV7_L1D_CACHE_REMAP_NORMAL_WRITE_THROUGH;
      end
     else if (Entry.Flags and RemapMask) = (PAGE_TABLE_FLAG_NORMAL or PAGE_TABLE_FLAG_CACHEABLE or PAGE_TABLE_FLAG_WRITEALLOCATE) then
      begin
       TableFlags:=TableFlags or ARMV7_L1D_CACHE_REMAP_NORMAL_WRITE_ALLOCATE;
      end
     else if (Entry.Flags and RemapMask) = PAGE_TABLE_FLAG_DEVICE then
      begin
       TableFlags:=TableFlags or ARMV7_L1D_CACHE_REMAP_DEVICE;
      end
     else if (Entry.Flags and RemapMask) = PAGE_TABLE_FLAG_ORDERED then
      begin
       TableFlags:=TableFlags or ARMV7_L1D_CACHE_REMAP_STRONGLY_ORDERED;
      end
     else 
      begin
       {Not Supported}
       Exit;
      end;      
     
     {Check Shared}
     if (Entry.Flags and PAGE_TABLE_FLAG_SHARED) = PAGE_TABLE_FLAG_SHARED then
      begin
       TableFlags:=TableFlags or ARMV7_L1D_FLAG_SHARED;
      end; 
     
     {Check NoAccess / ReadOnly / ReadWrite}
     if (Entry.Flags and ReadMask) = PAGE_TABLE_FLAG_NONE then
      begin
       {Nothing}
      end
     else if (Entry.Flags and ReadMask) = PAGE_TABLE_FLAG_READONLY then
      begin
       TableFlags:=TableFlags or ARMV7_L1D_ACCESS_READONLY;
      end
     else if (Entry.Flags and ReadMask) = PAGE_TABLE_FLAG_READWRITE then 
      begin
       TableFlags:=TableFlags or ARMV7_L1D_ACCESS_READWRITE;
      end
     else
      begin
       {Not Supported}
       Exit;
      end;
     
     {Check Executable}
     if (Entry.Flags and PAGE_TABLE_FLAG_EXECUTABLE) <> PAGE_TABLE_FLAG_EXECUTABLE then
      begin
       TableFlags:=TableFlags or ARMV7_L1D_FLAG_XN;
      end;
     
     {Update Section}
     if ARMv7SetPageTableSection(Entry.VirtualAddress,Entry.PhysicalAddress,TableFlags) then
      begin
       {Clean Data Cache Range (Page Table)}
       ARMv7CleanDataCacheRange(PAGE_TABLE_BASE,PAGE_TABLE_SIZE);
       
       {Invalidate TLB}
       ARMv7InvalidateTLB;
    
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

function ARMv7VectorTableGetEntry(Number:LongWord):PtrUInt;
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
 Entry:=ARMv7PageTableGetEntry(Offset);
 
 {Check for Read Only or Read Write}
 if (Entry.Flags and (PAGE_TABLE_FLAG_READONLY or PAGE_TABLE_FLAG_READWRITE)) <> 0 then
  begin
   {Get Entry}
   Result:=PPtrUInt(Offset)^;
  end; 
end;

{==============================================================================}

function ARMv7VectorTableSetEntry(Number:LongWord;Address:PtrUInt):LongWord;
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
  Entry:=ARMv7PageTableGetEntry(Offset);
  
  {Check for Read Only}
  if (Entry.Flags and PAGE_TABLE_FLAG_READONLY) <> 0 then
   begin
    {Modify Flags (Change to Read Write)}
    Flags:=Entry.Flags;
    Entry.Flags:=Entry.Flags and not(PAGE_TABLE_FLAG_READONLY);
    Entry.Flags:=Entry.Flags or PAGE_TABLE_FLAG_READWRITE;
    
    if ARMv7PageTableSetEntry(Entry) = ERROR_SUCCESS then
     begin
      {Set Entry}
      PPtrUInt(Offset)^:=Address;
      
      {Clean Data Cache Range}
      ARMv7CleanDataCacheRange(VECTOR_TABLE_BASE,VECTOR_TABLE_SIZE);
      
      {Data Synchronisation Barrier}
      ARMv7DataSynchronizationBarrier;
      
      {Invalidate Instruction Cache}
      ARMv7InvalidateInstructionCache;
      
      {Flush Branch Target Cache}
      ARMv7FlushBranchTargetCache;
      
      {Data Synchronisation Barrier}
      ARMv7DataSynchronizationBarrier;
      
      {Instruction Synchronisation Barrier}
      ARMv7InstructionMemoryBarrier;
      
      {Restore Flags (Back to Read Only)}
      Entry.Flags:=Flags;
      
      {Return Result}
      Result:=ARMv7PageTableSetEntry(Entry);
     end; 
   end
  else
   begin 
    {Set Entry}
    PPtrUInt(Offset)^:=Address;
    
    {Clean Data Cache Range}
    ARMv7CleanDataCacheRange(VECTOR_TABLE_BASE,VECTOR_TABLE_SIZE);
    
    {Data Synchronisation Barrier}
    ARMv7DataSynchronizationBarrier;
    
    {Invalidate Instruction Cache}
    ARMv7InvalidateInstructionCache;
    
    {Flush Branch Target Cache}
    ARMv7FlushBranchTargetCache;
    
    {Data Synchronisation Barrier}
    ARMv7DataSynchronizationBarrier;
    
    {Instruction Synchronisation Barrier}
    ARMv7InstructionMemoryBarrier;
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   end; 
 finally
  {Release Lock}
  if VectorTableLock.Lock <> INVALID_HANDLE_VALUE then VectorTableLock.ReleaseLock(VectorTableLock.Lock);
 end;
end;

{==============================================================================}

function ARMv7FirstBitSet(Value:LongWord):LongWord; assembler; nostackframe; 
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

function ARMv7CountLeadingZeros(Value:LongWord):LongWord; assembler; nostackframe; 
{Equivalent of the GCC Builtin function __builtin_clz}
{Note: ARM arm states that CLZ is supported for ARMv5 and above}
asm
 //Count leading zeros
 mov r1, r0
 clz r0, r1
end;

{==============================================================================}
{==============================================================================}
{ARMv7 Thread Functions}
procedure ARMv7PrimaryInit; assembler; nostackframe;
asm
 //Get the current CPU
 //Read the Multiprocessor Affinity (MPIDR) register from the system control coprocessor CP15
 mrc p15, #0, r1, cr0, cr0, #5;
 //Mask off the CPUID value
 and r1, #ARMV7_CP15_C0_MPID_CPUID_MASK
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

function ARMv7SpinLock(Spin:PSpinEntry):LongWord; assembler; nostackframe;
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
 //ARMv7 "data memory barrier" instruction.
 dmb
 
 //Get the current thread
 mrc p15, #0, r2, cr13, cr0, #4
 
 //Set the owner (TSpinEntry.Owner)
 str   r2, [r0, #12]
 
 //Return success
 mov   r0, #ERROR_SUCCESS
 bx    lr
 
.LWaitLock:
 {$IFNDEF ARMV7_SPIN_WAIT_INTERRUPT}
 //No point simply retrying over and over, perform a wait for event on each loop
 wfe
 {$ELSE}
 //No point simply retrying over and over, perform a wait for interrupt on each loop
 wfi
 {$ENDIF} 
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

function ARMv7SpinUnlock(Spin:PSpinEntry):LongWord; assembler; nostackframe;
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
 //ARMv7 "data memory barrier" instruction.
 dmb
 
 //Release the lock (TSpinEntry.State)
 str   r1, [r0, #4]
 
 //Return success
 mov   r0, #ERROR_SUCCESS
 {$IFNDEF ARMV7_SPIN_WAIT_INTERRUPT}
 //Send an event to others that are waiting
 sev 
 {$ENDIF} 
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

function ARMv7SpinLockIRQ(Spin:PSpinEntry):LongWord; assembler; nostackframe;
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
 //ARMv7 "data memory barrier" instruction.
 dmb
 
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
 {$IFNDEF ARMV7_SPIN_WAIT_INTERRUPT}
 //No point simply retrying over and over, perform a wait for event on each loop
 wfe
 {$ELSE}
 //No point simply retrying over and over, perform a wait for interrupt on each loop
 wfi
 {$ENDIF} 
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

function ARMv7SpinUnlockIRQ(Spin:PSpinEntry):LongWord; assembler; nostackframe;
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
 //ARMv7 "data memory barrier" instruction.
 dmb
 
 //Release the lock (TSpinEntry.State)
 str   r1, [r0, #4]
 
 //Restore the previous IRQ mask
 msr   cpsr_c, r3
 
 //Return success
 mov   r0, #ERROR_SUCCESS
 {$IFNDEF ARMV7_SPIN_WAIT_INTERRUPT}
 //Send an event to others that are waiting
 sev 
 {$ENDIF} 
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

function ARMv7SpinLockFIQ(Spin:PSpinEntry):LongWord; assembler; nostackframe;
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
 //ARMv7 "data memory barrier" instruction.
 dmb
 
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
 {$IFNDEF ARMV7_SPIN_WAIT_INTERRUPT}
 //No point simply retrying over and over, perform a wait for event on each loop
 wfe
 {$ELSE}
 //No point simply retrying over and over, perform a wait for interrupt on each loop
 wfi
 {$ENDIF} 
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

function ARMv7SpinUnlockFIQ(Spin:PSpinEntry):LongWord; assembler; nostackframe;
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
 //ARMv7 "data memory barrier" instruction.
 dmb
 
 //Release the lock (TSpinEntry.State)
 str   r1, [r0, #4]
 
 //Restore the previous FIQ mask
 msr   cpsr_c, r3
 
 //Return success
 mov   r0, #ERROR_SUCCESS
 {$IFNDEF ARMV7_SPIN_WAIT_INTERRUPT}
 //Send an event to others that are waiting
 sev 
 {$ENDIF} 
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

function ARMv7SpinLockIRQFIQ(Spin:PSpinEntry):LongWord; assembler; nostackframe;
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
 //ARMv7 "data memory barrier" instruction.
 dmb
 
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
 {$IFNDEF ARMV7_SPIN_WAIT_INTERRUPT}
 //No point simply retrying over and over, perform a wait for event on each loop
 wfe
 {$ELSE}
 //No point simply retrying over and over, perform a wait for interrupt on each loop
 wfi
 {$ENDIF} 
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

function ARMv7SpinUnlockIRQFIQ(Spin:PSpinEntry):LongWord; assembler; nostackframe;
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
 //ARMv7 "data memory barrier" instruction.
 dmb

 //Release the lock (TSpinEntry.State)
 str   r1, [r0, #4]
 
 //Restore the previous IRQ/FIQ mask
 msr   cpsr_c, r3
 
 //Return success
 mov   r0, #ERROR_SUCCESS
 {$IFNDEF ARMV7_SPIN_WAIT_INTERRUPT}
 //Send an event to others that are waiting
 sev 
 {$ENDIF} 
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

function ARMv7SpinCheckIRQ(Spin:PSpinEntry):Boolean;
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

function ARMv7SpinCheckFIQ(Spin:PSpinEntry):Boolean;
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
 
function ARMv7SpinExchangeIRQ(Spin1,Spin2:PSpinEntry):LongWord;
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

function ARMv7SpinExchangeFIQ(Spin1,Spin2:PSpinEntry):LongWord;
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

function ARMv7MutexLock(Mutex:PMutexEntry):LongWord; assembler; nostackframe;
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
 //ARMv7 "data memory barrier" instruction.
 dmb
 
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

function ARMv7MutexUnlock(Mutex:PMutexEntry):LongWord; assembler; nostackframe;
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
 //ARMv7 "data memory barrier" instruction.
 dmb
 
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

function ARMv7MutexTryLock(Mutex:PMutexEntry):LongWord; assembler; nostackframe;
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
 //ARMv7 "data memory barrier" instruction.
 dmb
 
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

function ARMv7ThreadGetCurrent:TThreadHandle; assembler; nostackframe;
{Get the current thread id from the c13 (Thread and process ID) register of system control coprocessor CP15
 See page ???}
asm
 //Get the contents of c13 (Thread and process ID) register in system control coprocessor CP15
 mrc p15, #0, r0, cr13, cr0, #4
end;

{==============================================================================}

function ARMv7ThreadSetCurrent(Thread:TThreadHandle):LongWord; assembler; nostackframe;
{Set the current thread id in the c13 (Thread and process ID) register of system control coprocessor CP15
 See page ???}
asm
 //Set the contents of c13 (Thread and process ID) register in system control coprocessor CP15
 mcr p15, #0, r0, cr13, cr0, #4
 
 //Return success
 mov r0, #ERROR_SUCCESS
end;

{==============================================================================}

function ARMv7ThreadSetupStack(StackBase:Pointer;StartProc:TThreadStart;ReturnProc:TThreadEnd;Parameter:Pointer):Pointer;
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
 StackPointer:=StackPointer - (ARMV7_CONTEXT_LENGTH * SizeOf(LongWord));

 {Floating point (fpexc)}
 PLongWord(StackPointer + ((ARMV7_CONTEXT_LENGTH - 50) * SizeOf(LongWord)))^:=ARMv7GetFPEXC;
 
 {Floating point (fpscr)}
 PLongWord(StackPointer + ((ARMV7_CONTEXT_LENGTH - 49) * SizeOf(LongWord)))^:=ARMv7GetFPSCR;
 
 {Registers d0 to d15}
 for Count:=(ARMV7_CONTEXT_LENGTH - 48) to (ARMV7_CONTEXT_LENGTH - 17) do 
  begin
   PLongWord(StackPointer + (Count * SizeOf(LongWord)))^:=0;
  end;
  
 {Parameter passed in r0}
 PLongWord(StackPointer + ((ARMV7_CONTEXT_LENGTH - 16) * SizeOf(LongWord)))^:=LongWord(Parameter);
 
 {Registers r1 to r12}
 for Count:=(ARMV7_CONTEXT_LENGTH - 15) to (ARMV7_CONTEXT_LENGTH - 4) do 
  begin
   PLongWord(StackPointer + (Count * SizeOf(LongWord)))^:=0;
  end;
 
 {Return address (lr)}
 PLongWord(StackPointer + ((ARMV7_CONTEXT_LENGTH - 3) * SizeOf(LongWord)))^:=LongWord(@ReturnProc);

 {Start address (lr/pc)}
 PLongWord(StackPointer + ((ARMV7_CONTEXT_LENGTH - 2) * SizeOf(LongWord)))^:=LongWord(@StartProc);

 {Control bits (cpsr) (SYS mode, IRQ enabled, FIQ enabled, Abort enabled)} 
 PLongWord(StackPointer + ((ARMV7_CONTEXT_LENGTH - 1) * SizeOf(LongWord)))^:=ARM_MODE_SYS;
 
 {Return top "Lowest Address" of stack}
 Result:=Pointer(StackPointer);
end;

{==============================================================================}
{==============================================================================}
{ARMv7 IRQ Functions}
function ARMv7DispatchIRQ(CPUID:LongWord;Thread:TThreadHandle):TThreadHandle; inline;
begin
 {}
 if Assigned(ARMv7DispatchIRQHandler) then
  begin
   Result:=ARMv7DispatchIRQHandler(CPUID,Thread);
  end
 else
  begin
   Result:=Thread;
  end;
end;

{==============================================================================}
{==============================================================================}
{ARMv7 FIQ Functions}
function ARMv7DispatchFIQ(CPUID:LongWord;Thread:TThreadHandle):TThreadHandle; inline;
begin
 {}
 if Assigned(ARMv7DispatchFIQHandler) then
  begin
   Result:=ARMv7DispatchFIQHandler(CPUID,Thread);
  end
 else
  begin
   Result:=Thread;
  end;
end;

{==============================================================================}
{==============================================================================}
{ARMv7 SWI Functions}
function ARMv7DispatchSWI(CPUID:LongWord;Thread:TThreadHandle;Request:PSystemCallRequest):TThreadHandle; inline;
begin
 {}
 if Assigned(ARMv7DispatchSWIHandler) then
  begin
   Result:=ARMv7DispatchSWIHandler(CPUID,Thread,Request);
  end
 else
  begin
   Result:=Thread;
  end;
end;

{==============================================================================}
{==============================================================================}
{ARMv7 Interrupt Functions}
procedure ARMv7ResetHandler; assembler; nostackframe;    
asm
 //See: B1.6.10 Reset in the ARM v7 Architecture Reference Manual
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

procedure ARMv7UndefinedInstructionHandler; assembler; nostackframe;    
{Handle an undefined instruction exception}
{Notes: This routine is registered as the vector for undefined instruction exception in the vector table loaded during startup.}
asm
 //On entry, processor will be in Undefined mode, IRQ will be disabled and SP will point to the Undefined stack
 //See: B1.6.11 Undefined Instruction exception in the ARM v7 Architecture Reference Manual

 //Adjust the Undefined mode link register (LR_und) for the return
 //See: B1.6.11 Undefined Instruction exception in the ARM v7 Architecture Reference Manual
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

procedure ARMv7SoftwareInterruptHandler; assembler; nostackframe;          
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
 //See: B1.6.12 Supervisor Call (SVC) exception in the ARM v7 Architecture Reference Manual

 //Do NOT adjust the SWI mode link register (LR_svc) for the return
 //See: B1.6.12 Supervisor Call (SVC) exception in the ARM v7 Architecture Reference Manual
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
  
 //Get the current CPU
 //Read the Multiprocessor Affinity (MPIDR) register from the system control coprocessor CP15
 mrc p15, #0, r4, cr0, cr0, #5;
 //Mask off the CPUID value
 and r4, #ARMV7_CP15_C0_MPID_CPUID_MASK
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
 //ARMv7 "data memory barrier" instruction.
 dmb
 
 //Determine the System Call number passed in R0
 //Check for SYSTEM_CALL_CONTEXT_SWITCH
 cmp r0, #SYSTEM_CALL_CONTEXT_SWITCH
 bne .LSystemCallInvalid
 //Perform a System Call Context Switch (Caller will have disabled IRQ/FIQ as appropriate to prevent rescheduling)
 //Passing R0 = OldStack (Parameter 1 in R1) / R1 = NewStack (Parameter 2 in R2) / R2 = NewThread (Parameter 3 in R3)
 mov r0, r1
 mov r1, r2
 mov r2, r3
 bl ARMv7ContextSwitchSWI
 //Put the New Thread Id in R0 for restore (ARMv7ContextSwitchSWI does not modify R2)
 mov r0, r2
 //System Call completed
 b .LSystemCallCompleted
 
.LSystemCallInvalid:
 //Put the Current Thread Id in R0 for restore
 mov r0, r6
 
.LSystemCallCompleted:
 //Execute a data memory barrier
 //ARMv7 "data memory barrier" instruction.
 dmb
 
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

procedure ARMv7PrefetchAbortHandler; assembler; nostackframe;     
{Handle a prefetch abort exception}
{Notes: This routine is registered as the vector for prefetch abort exception in the vector table loaded during startup.}
asm
 //On entry, processor will be in Abort mode, IRQ will be disabled and SP will point to the Abort stack
 //See: B1.9.7 Prefetch Abort exception in the ARM v7 Architecture Reference Manual

 //Adjust the Abort mode link register (LR_abt) for the return
 //See: B1.9.7 Prefetch Abort exception in the ARM v7 Architecture Reference Manual
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

procedure ARMv7DataAbortHandler; assembler; nostackframe;
{Handle a data abort exception}
{Notes: This routine is registered as the vector for data abort exception in the vector table loaded during startup.}
asm
 //On entry, processor will be in Abort mode, IRQ will be disabled and SP will point to the Abort stack
 //See: B1.6.15 Data Abort exception in the ARM v7 Architecture Reference Manual

 //Adjust the Abort mode link register (LR_abt) for the return
 //See: B1.6.15 Data Abort exception in the ARM v7 Architecture Reference Manual
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

procedure ARMv7ReservedHandler; assembler; nostackframe;     
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

procedure ARMv7IRQHandler; assembler; nostackframe;    
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
 //interrupting a normal thread or if we are interrupting an IRQ, SWI or other exception
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
 
 //Get the current CPU
 //Read the Multiprocessor Affinity (MPIDR) register from the system control coprocessor CP15
 mrc p15, #0, r0, cr0, cr0, #5;
 //Mask off the CPUID value
 and r0, #ARMV7_CP15_C0_MPID_CPUID_MASK
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
 
 //Save value of R3 (Stack Alignment) on the IRQ mode stack for return from ARMv7DispatchIRQ
 //Also save R4 (Current Thread Id) to maintain the 8 byte alignment
 push {r3, r4}
  
 //Execute a data memory barrier 
 //ARMv7 "data memory barrier" instruction.
 dmb

 //Call DispatchIRQ passing CPU in R0 and Thread Id in R1 (Thread Id of the interrupted thread)
 //DispatchIRQ will return Thread Id in R0 which may be different if a context switch occurred
 mov r1, r4
 bl ARMv7DispatchIRQ
 
 //Execute a data memory barrier
 //ARMv7 "data memory barrier" instruction.
 dmb
 
 //Restore value of R3 (Stack Alignment) from the IRQ mode stack after return from ARMv7DispatchIRQ
 //Also restore R4 (Current Thread Id) to maintain the 8 byte alignment
 pop {r3, r4}
  
 //Restore the IRQ mode stack pointer alignment 
 add sp, sp, r3
 
 //Load the current thread id into c13 (Thread and process ID) register of system control coprocessor CP15  
 //Returned from ARMv7DispatchIRQ in R0 and may be different if a context switch occurred 
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
 //Interrupted an IRQ, SWI or other exception 
 //Restore r8 from above
 pop {r8}
 
 //Store Return State (SRSDB) on the SVC mode stack which which will be the stack we use for the handler
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
 
 //Get the current CPU
 //Read the Multiprocessor Affinity (MPIDR) register from the system control coprocessor CP15
 mrc p15, #0, r0, cr0, cr0, #5;
 //Mask off the CPUID value
 and r0, #ARMV7_CP15_C0_MPID_CPUID_MASK
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
 
 //Save value of R3 (Stack Alignment) on the SVC mode stack for return from ARMv7DispatchIRQ
 //Also save R4 (Current Thread Id) to maintain the 8 byte alignment
 push {r3, r4}
  
 //Execute a data memory barrier 
 //ARMv7 "data memory barrier" instruction.
 dmb

 //Call DispatchIRQ passing CPU in R0 and INVALID_HANDLE_VALUE in R1 (No interrupted thread)
 //DispatchIRQ will return INVALID_HANDLE_VALUE in R0 and no context switch will occur
 ldr r1, =INVALID_HANDLE_VALUE
 bl ARMv7DispatchIRQ
 
 //Execute a data memory barrier
 //ARMv7 "data memory barrier" instruction.
 dmb
 
 //Restore value of R3 (Stack Alignment) from the SVC mode stack after return from ARMv7DispatchIRQ
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

procedure ARMv7FIQHandler; assembler; nostackframe;    
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

 //Get the current CPU
 //Read the Multiprocessor Affinity (MPIDR) register from the system control coprocessor CP15
 mrc p15, #0, r0, cr0, cr0, #5;
 //Mask off the CPUID value
 and r0, #ARMV7_CP15_C0_MPID_CPUID_MASK
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
 
 //Save value of R3 (Stack Alignment) on the FIQ mode stack for return from ARMv7DispatchFIQ
 //Also save R4 (Current Thread Id) to maintain the 8 byte alignment
 push {r3, r4}
  
 //Execute a data memory barrier 
 //ARMv7 "data memory barrier" instruction.
 dmb

 //Call DispatchFIQ passing CPU in R0 and Thread Id in R1 (Thread Id of the interrupted thread)
 //DispatchFIQ will return Thread Id in R0 which may be different if a context switch occurred
 mov r1, r4
 bl ARMv7DispatchFIQ
 
 //Execute a data memory barrier
 //ARMv7 "data memory barrier" instruction.
 dmb
 
 //Restore value of R3 (Stack Alignment) from the FIQ mode stack after return from ARMv7DispatchFIQ
 //Also restore R4 (Current Thread Id) to maintain the 8 byte alignment
 pop {r3, r4}
  
 //Restore the FIQ mode stack pointer alignment 
 add sp, sp, r3
 
 //Load the current thread id into c13 (Thread and process ID) register of system control coprocessor CP15  
 //Returned from ARMv7DispatchFIQ in R0 and may be different if a context switch occurred 
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
 
 //Get the current CPU
 //Read the Multiprocessor Affinity (MPIDR) register from the system control coprocessor CP15
 mrc p15, #0, r0, cr0, cr0, #5;
 //Mask off the CPUID value
 and r0, #ARMV7_CP15_C0_MPID_CPUID_MASK
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
 
 //Save value of R3 (Stack Alignment) on the SVC mode stack for return from ARMv7DispatchFIQ
 //Also save R4 (Current Thread Id) to maintain the 8 byte alignment
 push {r3, r4}
  
 //Execute a data memory barrier 
 //ARMv7 "data memory barrier" instruction.
 dmb

 //Call DispatchFIQ passing CPU in R0 and INVALID_HANDLE_VALUE in R1 (No interrupted thread)
 //DispatchFIQ will return INVALID_HANDLE_VALUE in R0 and no context switch will occur
 ldr r1, =INVALID_HANDLE_VALUE
 bl ARMv7DispatchFIQ
 
 //Execute a data memory barrier
 //ARMv7 "data memory barrier" instruction.
 dmb
 
 //Restore value of R3 (Stack Alignment) from the SVC mode stack after return from ARMv7DispatchFIQ
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
{ARMv7 Helper Functions}
function ARMv7GetFPEXC:LongWord; assembler; nostackframe; 
asm
 //Get the FPEXC register from the VFP unit
 fmrx r0, fpexc
end;

{==============================================================================}

function ARMv7GetFPSCR:LongWord; assembler; nostackframe; 
asm
 //Get the FPSCR register from the VFP unit
 fmrx r0, fpscr
end;

{==============================================================================}

procedure ARMv7StartMMU; assembler; nostackframe; 
asm
 //Preseve LR for return (None of the following uses R4)
 mov r4, lr

 //Disable the L1 Data and Instruction Cache before enabling the MMU by clearing the I and C bits in the C1 control register.
 //Also ensure the MMU is disabled by clearing the M bit in the C1 control register.
 //See page ???
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
 //bl ARMv7InvalidateInstructionCache //TestingRpi2/TestingRpi1
 
 //Invalidate the L1 Data Cache before enabling the MMU
 //See page B3-138 of the ARMv7 Architecture Reference Manual
 //bl ARMv7InvalidateL1DataCache  //TestingRpi2/TestingRpi1
 
 //Invalidate the Transaction Lookaside Buffers (TLB) before enabling the MMU
 //See page B3-138 of the ARMv7 Architecture Reference Manual
 bl ARMv7InvalidateTLB
 
 //Perform a data synchronization barrier operation
 //See page A8-92 of the ARMv7 Architecture Reference Manual
 //ARMv7 "data synchronization barrier" instruction.
 dsb
 
 //Load the Primary Region Remap Register (PRRR) in the C10 control register.
 //See page ???
 ldr r12, =ARMV7_CP15_C10_PRRR_MASK
 mcr p15, #0, r12, cr10, cr2, #0
 
 //Load the Normal Memory Remap Register (NMRR) in the C10 control register.
 //See page ???
 ldr r12, =ARMV7_CP15_C10_NMRR_MASK
 mcr p15, #0, r12, cr10, cr2, #1
 
 //Set the access for Domain 0 to Client in the C3 domain access control register.
 //See page ???
 mov r12, #ARMV7_CP15_C3_DOMAIN0_CLIENT 
 mcr p15, #0, r12, cr3, cr0, #0
 
 //Set the Page Table base address in the C2 translation table base register 0
 //Only bits 31 to 14 are written to the register
 //The alignment of the Translation Table Base Register 0 depends on the value
 //of N in the C2 translation table base control register.
 //See page B3-113 of the ARMv7 Architecture Reference Manual
 ldr r12, .LPAGE_TABLE_BASE
 ldr r12, [r12]
 orr r12, r12, #ARMV7_CP15_C2_TTBR_NOS
 orr r12, r12, #ARMV7_CP15_C2_TTBR_RGN_OUTER_WRITE_ALLOCATE
 orr r12, r12, #ARMV7_CP15_C2_TTBR_S
 orr r12, r12, #ARMV7_CP15_C2_TTBR_IRGN_INNER_WRITE_ALLOCATE
 mcr p15, #0, r12, cr2, cr0, #0
  
 //Set the Page Table base address in the C2 translation table base register 1
 //Only bits 31 to 14 are written to the register 
 //Translation Table Base Register 1 must reside on a 16KB page boundary
 //See page B3-113 of the ARMv7 Architecture Reference Manual
 mcr p15, #0, r12, cr2, cr0, #1
  
 //Perform a instruction synchronization barrier operation to ensure 
 //all of the above is completed before enabling the MMU and Caches
 //See page A8-102 of the ARMv7 Architecture Reference Manual
 //ARMv7 "instruction synchronization barrier" instruction.
 isb 
 
 //Enable the Memory Management Unit and L1 Data and Instruction Cache by setting the TRE, I, C, M and XP bits in the C1 control register.
 //See page ???
 mrc p15, #0, r12, cr1, cr0, #0;
 orr r12, r12, #ARMV7_CP15_C1_TRE_BIT
 orr r12, r12, #ARMV7_CP15_C1_I_BIT
 orr r12, r12, #ARMV7_CP15_C1_C_BIT
 orr r12, r12, #ARMV7_CP15_C1_M_BIT
 //orr r12, r12, #ARMV7_CP15_C1_XP_BIT {Always enabled in ARMv7}
 mcr p15, #0, r12, cr1, cr0, #0;
 
 //Perform an Instruction Synchronization Barrier (ISB) operation immediately after the change above.
 //See page A8-102  of the ARMv7 Architecture Reference Manual
 //ARMv7 "instruction synchronization barrier" instruction.
 isb
 
 //Restore LR for return 
 mov lr, r4

 //Return to caller
 bx lr
 
.LPAGE_TABLE_BASE:
 .long PAGE_TABLE_BASE
end;

{==============================================================================}

function ARMv7GetTimerState(Timer:LongWord):LongWord; assembler; nostackframe; 
asm
 //Get the Physical, Virtual or Hypervisor Timer Control register of the Generic Timer in the C14 control register.
 //Check for the Physical Timer
 cmp r0, #ARMV7_CP15_C14_CNTP
 bne .LVirtual
 
 //Get the Physical Timer Control register of the Generic Timer in the C14 control register.
 //See page B4-1543 of the ARM Architecture Reference Manual
 mrc p15, #0, r0, cr14, cr2, #1
 
 //Return to caller
 bx lr
 
.LVirtual:
 //Check for the Virtual Timer
 cmp r0, #ARMV7_CP15_C14_CNTV
 bne .LHypervisor

 //Get the Virtual Timer Control register of the Generic Timer in the C14 control register.
 //See page B4-1544 of the ARM Architecture Reference Manual
 mrc p15, #0, r0, cr14, cr3, #1
 
 //Return to caller
 bx lr

.LHypervisor:
 //Check for the Hypervisor Timer
 cmp r0, #ARMV7_CP15_C14_CNTH
 bne .LInvalid

 //Get Current program status register
 mrs r0, cpsr
 //Mask off everything except the MODE bits
 and r0, r0, #ARM_MODE_BITS
 //Check for HYP mode
 cmp r0, #ARM_MODE_HYP
 bne .LInvalid
 
 //Get the Hypervisor Timer Control register of the Generic Timer in the C14 control register.
 //See page B4-1535 of the ARM Architecture Reference Manual
 mrc p15, #4, r0, cr14, cr2, #1
 
.LInvalid:
 //Invalid Timer
 mov r0, #0
 //Return to caller
 bx lr
end;

{==============================================================================}

procedure ARMv7SetTimerState(Timer,State:LongWord); assembler; nostackframe; 
asm
 //Set the Physical, Virtual or Hypervisor Timer Control register of the Generic Timer in the C14 control register.
 //Check for the Physical Timer
 cmp r0, #ARMV7_CP15_C14_CNTP
 bne .LVirtual

 //Set the Physical Timer Control register of the Generic Timer in the C14 control register.
 //See page B4-1543 of the ARM Architecture Reference Manual
 mcr p15, #0, r1, cr14, cr2, #1
 
 //Return to caller
 bx lr
 
.LVirtual:
 //Check for the Virtual Timer
 cmp r0, #ARMV7_CP15_C14_CNTV
 bne .LHypervisor
 
 //Set the Virtual Timer Control register of the Generic Timer in the C14 control register.
 //See page B4-1544 of the ARM Architecture Reference Manual
 mcr p15, #0, r1, cr14, cr3, #1
 
 //Return to caller
 bx lr

.LHypervisor:
 //Check for the Hypervisor Timer
 cmp r0, #ARMV7_CP15_C14_CNTH
 bne .LInvalid

 //Get Current program status register
 mrs r0, cpsr
 //Mask off everything except the MODE bits
 and r0, r0, #ARM_MODE_BITS
 //Check for HYP mode
 cmp r0, #ARM_MODE_HYP
 bne .LInvalid
 
 //Set the Hypervisor Timer Control register of the Generic Timer in the C14 control register.
 //See page B4-1535 of the ARM Architecture Reference Manual
 mcr p15, #4, r1, cr14, cr2, #1
 
.LInvalid:
 //Return to caller
 bx lr
end;

{==============================================================================}

function ARMv7GetTimerCount(Timer:LongWord):Int64; assembler; nostackframe; 
asm
 //Get the Physical, Virtual or Hypervisor Count register of the Generic Timer in the C14 control register.
 //Check for the Physical Timer
 cmp r0, #ARMV7_CP15_C14_CNTP
 bne .LVirtual

 //Get the Physical Count register of the Generic Timer in the C14 control register.
 //See page B4-1543 of the ARM Architecture Reference Manual
 mrrc p15, #0, r0, r1, cr14
 
 //Return to caller
 bx lr
 
.LVirtual:
 //Check for the Virtual Timer
 cmp r0, #ARMV7_CP15_C14_CNTV
 bne .LHypervisor
 
 //Get the Virtual Count register of the Generic Timer in the C14 control register.
 //See page B4-1546 of the ARM Architecture Reference Manual
 mrrc p15, #1, r0, r1, cr14
 
 //Return to caller
 bx lr

.LHypervisor:
 //Check for the Hypervisor Timer
 cmp r0, #ARMV7_CP15_C14_CNTH
 bne .LInvalid

 //Get Current program status register
 mrs r0, cpsr
 //Mask off everything except the MODE bits
 and r0, r0, #ARM_MODE_BITS
 //Check for HYP mode
 cmp r0, #ARM_MODE_HYP
 bne .LInvalid
 
 //Get the Physical (not Hypervisor) Count register of the Generic Timer in the C14 control register.
 //See page B4-1543 of the ARM Architecture Reference Manual
 mrrc p15, #0, r0, r1, cr14
 
.LInvalid:
 //Invalid Timer
 mov r0, #0
 mov r1, #0
 //Return to caller
 bx lr
end;

{==============================================================================}

function ARMv7GetTimerValue(Timer:LongWord):LongWord; assembler; nostackframe; 
asm
 //Get the Physical, Virtual or Hypervisor Timer Value register of the Generic Timer in the C14 control register.
 //Check for the Physical Timer
 cmp r0, #ARMV7_CP15_C14_CNTP
 bne .LVirtual

 //Get the Physical Timer Value register of the Generic Timer in the C14 control register.
 //See page B4-1542 of the ARM Architecture Reference Manual
 mrc p15, #0, r0, cr14, cr2, #0
 
 //Return to caller
 bx lr
 
.LVirtual:
 //Check for the Virtual Timer
 cmp r0, #ARMV7_CP15_C14_CNTV
 bne .LHypervisor
 
 //Get the Virtual Timer Value register of the Generic Timer in the C14 control register.
 //See page B4-1545 of the ARM Architecture Reference Manual
 mrc p15, #0, r0, cr14, cr3, #0
 
 //Return to caller
 bx lr

.LHypervisor:
 //Check for the Hypervisor Timer
 cmp r0, #ARMV7_CP15_C14_CNTH
 bne .LInvalid

 //Get Current program status register
 mrs r0, cpsr
 //Mask off everything except the MODE bits
 and r0, r0, #ARM_MODE_BITS
 //Check for HYP mode
 cmp r0, #ARM_MODE_HYP
 bne .LInvalid
 
 //Get the Hypervisor Timer Value register of the Generic Timer in the C14 control register.
 //See page B4-1536 of the ARM Architecture Reference Manual
 mrc p15, #4, r0, cr14, cr2, #0
 
.LInvalid:
 //Invalid Timer
 mov r0, #0
 //Return to caller
 bx lr
end;

{==============================================================================}

procedure ARMV7SetTimerValue(Timer,Value:LongWord); assembler; nostackframe; 
asm
 //Set the Physical, Virtual or Hypervisor Timer Value register of the Generic Timer in the C14 control register.
 //Check for the Physical Timer
 cmp r0, #ARMV7_CP15_C14_CNTP
 bne .LVirtual
 
 //Set the Physical Timer Value register of the Generic Timer in the C14 control register.
 //See page B4-1542 of the ARM Architecture Reference Manual
 mcr p15, #0, r1, cr14, cr2, #0
 
 //Return to caller
 bx lr
 
.LVirtual:
 //Check for the Virtual Timer
 cmp r0, #ARMV7_CP15_C14_CNTV
 bne .LHypervisor
 
 //Set the Virtual Timer Value register of the Generic Timer in the C14 control register.
 //See page B4-1545 of the ARM Architecture Reference Manual
 mcr p15, #0, r1, cr14, cr3, #0
 
 //Return to caller
 bx lr

.LHypervisor:
 //Check for the Hypervisor Timer
 cmp r0, #ARMV7_CP15_C14_CNTH
 bne .LInvalid

 //Get Current program status register
 mrs r0, cpsr
 //Mask off everything except the MODE bits
 and r0, r0, #ARM_MODE_BITS
 //Check for HYP mode
 cmp r0, #ARM_MODE_HYP
 bne .LInvalid
 
 //Set the Hypervisor Timer Value register of the Generic Timer in the C14 control register.
 //See page B4-1536 of the ARM Architecture Reference Manual
 mcr p15, #4, r1, cr14, cr2, #0
 
.LInvalid:
 //Return to caller
 bx lr
end;

{==============================================================================}

function ARMv7GetTimerCompare(Timer:LongWord):Int64; assembler; nostackframe; 
asm
 //Get the Physical, Virtual or Hypervisor Timer CompareValue register of the Generic Timer in the C14 control register.
 //Check for the Physical Timer
 cmp r0, #ARMV7_CP15_C14_CNTP
 bne .LVirtual

 //Get the Physical Timer CompareValue register of the Generic Timer in the C14 control register.
 //See page B4-1541 of the ARM Architecture Reference Manual
 mrrc p15, #2, r0, r1, cr14
 
 //Return to caller
 bx lr
 
.LVirtual:
 //Check for the Virtual Timer
 cmp r0, #ARMV7_CP15_C14_CNTV
 bne .LHypervisor
 
 //Get the Virtual Timer CompareValue register of the Generic Timer in the C14 control register.
 //See page B4-1544 of the ARM Architecture Reference Manual
 mrrc p15, #3, r0, r1, cr14
 
 //Return to caller
 bx lr

.LHypervisor:
 //Check for the Hypervisor Timer
 cmp r0, #ARMV7_CP15_C14_CNTH
 bne .LInvalid

 //Get Current program status register
 mrs r0, cpsr
 //Mask off everything except the MODE bits
 and r0, r0, #ARM_MODE_BITS
 //Check for HYP mode
 cmp r0, #ARM_MODE_HYP
 bne .LInvalid
 
 //Get the Hypervisor Timer CompareValue register of the Generic Timer in the C14 control register.
 //See page B4-1535 of the ARM Architecture Reference Manual
 mrrc p15, #6, r0, r1, cr14
 
.LInvalid:
 //Invalid Timer
 mov r0, #0
 mov r1, #0
 //Return to caller
 bx lr
end;

{==============================================================================}

procedure ARMV7SetTimerCompare(Timer,High,Low:LongWord); assembler; nostackframe; 
asm
 //Set the Physical, Virtual or Hypervisor Timer CompareValue register of the Generic Timer in the C14 control register.
 //Check for the Physical Timer
 cmp r0, #ARMV7_CP15_C14_CNTP
 bne .LVirtual

 //Set the Physical Timer CompareValue register of the Generic Timer in the C14 control register.
 //See page B4-1541 of the ARM Architecture Reference Manual
 mcrr p15, #2, r2, r1, cr14
 
 //Return to caller
 bx lr
 
.LVirtual:
 //Check for the Virtual Timer
 cmp r0, #ARMV7_CP15_C14_CNTV
 bne .LHypervisor
 
 //Set the Virtual Timer CompareValue register of the Generic Timer in the C14 control register.
 //See page B4-1544 of the ARM Architecture Reference Manual
 mcrr p15, #3, r2, r1, cr14
 
 //Return to caller
 bx lr

.LHypervisor:
 //Check for the Hypervisor Timer
 cmp r0, #ARMV7_CP15_C14_CNTH
 bne .LInvalid

 //Get Current program status register
 mrs r0, cpsr
 //Mask off everything except the MODE bits
 and r0, r0, #ARM_MODE_BITS
 //Check for HYP mode
 cmp r0, #ARM_MODE_HYP
 bne .LInvalid
 
 //Set the Hypervisor Timer CompareValue register of the Generic Timer in the C14 control register.
 //See page B4-1535 of the ARM Architecture Reference Manual
 mcrr p15, #6, r2, r1, cr14

.LInvalid:
 //Return to caller
 bx lr
end;

{==============================================================================}

function ARMv7GetTimerFrequency:LongWord; assembler; nostackframe; 
asm
 //Get the Counter Frequency register of the Generic Timer in the C14 control register.
 //See page B4-1532 of the ARM Architecture Reference Manual
 mrc p15, #0, r0, cr14, cr0, #0
end;

{==============================================================================}

function ARMv7GetPageTableCoarse(Address:PtrUInt):LongWord;
{Get the descriptor for a Coarse Page Table entry (1MB)}
{See page ???}
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
 TableBase:=(Address and ARMV7_L1D_SECTION_BASE_MASK);
 
 {Get Table Offset}
 TableOffset:=((TableBase shr 20) shl 2); {Divide Base by 1MB then multiply by 4 to get Offset into Page Table}
 
 {Read Page Table}
 CoarseEntry:=PLongWord(PtrUInt(PAGE_TABLE_BASE) + PtrUInt(TableOffset))^;
 
 {Check Level 1 Type}
 if (CoarseEntry and ARMV7_L1D_TYPE_COARSE) <> 0 then
  begin
   {Return Result}
   Result:=CoarseEntry;
  end; 
end;

{==============================================================================}

function ARMv7SetPageTableCoarse(Address,CoarseAddress:PtrUInt;Flags:Word):Boolean;
{Set the descriptor for a Coarse Page Table entry (1MB)}
{See page ???}
{Note: Caller must call ARMv7InvalidateTLB after changes if MMU is enabled}
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
 TableBase:=(Address and ARMV7_L1D_SECTION_BASE_MASK);
 if TableBase <> Address then Exit; {Must begin on a 1MB boundary}
 
 {Get Coarse Base}
 CoarseBase:=(CoarseAddress and ARMV7_L1D_COARSE_BASE_MASK);
 if CoarseBase <> CoarseAddress then Exit; {Must begin on a 1KB boundary}
 
 {Get Table Offset}
 TableOffset:=((TableBase shr 20) shl 2); {Divide Base by 1MB then multiply by 4 to get Offset into Page Table}
 
 {Read Page Table}
 CurrentEntry:=PLongWord(PtrUInt(PAGE_TABLE_BASE) + PtrUInt(TableOffset))^;
 
 {Check Level 1 Type}
 if (CurrentEntry and ARMV7_L1D_TYPE_COARSE) <> 0 then
  begin
   {Current entry is a Coarse Page Table}
   CurrentBase:=(CurrentEntry and ARMV7_L1D_COARSE_BASE_MASK);
   CurrentOffset:=0;
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
 else if (CurrentEntry and ARMV7_L1D_TYPE_SECTION) <> 0 then
  begin
   {Current entry is a Section or Supersection}
   if (CurrentEntry and ARMV7_L1D_FLAG_SUPERSECTION) = 0 then 
    begin
     {Current entry is a Section}
     {Convert to Coarse Page Table}
     CurrentBase:=(CurrentEntry and ARMV7_L1D_SECTION_BASE_MASK);
     CurrentOffset:=0;
     {Convert Flags}
     CurrentFlags:=0;
     {Non Secure}
     if (CurrentEntry and ARMV7_L1D_FLAG_SECTION_NS) <> 0 then
      begin
       Flags:=Flags or ARMV7_L1D_FLAG_COARSE_NS; {Put NS flag on Coarse Page Table}
      end;
     {Not Global} 
     if (CurrentEntry and ARMV7_L1D_FLAG_NOT_GLOBAL) <> 0 then
      begin
       CurrentFlags:=CurrentFlags or ARMV7_L2D_FLAG_NOT_GLOBAL;
      end; 
     {Shared} 
     if (CurrentEntry and ARMV7_L1D_FLAG_SHARED) <> 0 then
      begin
       CurrentFlags:=CurrentFlags or ARMV7_L2D_FLAG_SHARED;
      end; 
     {AP2} 
     if (CurrentEntry and ARMV7_L1D_FLAG_AP2) <> 0 then
      begin
       CurrentFlags:=CurrentFlags or ARMV7_L2D_FLAG_AP2;
      end; 
     {IMP} 
     if (CurrentEntry and ARMV7_L1D_FLAG_IMP) <> 0 then
      begin
       Flags:=Flags or ARMV7_L1D_FLAG_IMP; {Put IMP flag on Coarse Page Table}
      end; 
     {Execute Never} 
     if (CurrentEntry and ARMV7_L1D_FLAG_XN) <> 0 then
      begin
       CurrentFlags:=CurrentFlags or ARMV7_L2D_FLAG_SMALL_XN;
      end; 
     {Cacheable} 
     if (CurrentEntry and ARMV7_L1D_FLAG_C) <> 0 then
      begin
       CurrentFlags:=CurrentFlags or ARMV7_L2D_FLAG_C;
      end; 
     {Bufferable}
     if (CurrentEntry and ARMV7_L1D_FLAG_B) <> 0 then
      begin
       CurrentFlags:=CurrentFlags or ARMV7_L2D_FLAG_B;
      end; 
     {AP}
     case (CurrentEntry and ARMV7_L1D_AP_MASK) of
      ARMV7_L1D_AP0:CurrentFlags:=CurrentFlags or ARMV7_L2D_AP0;
      ARMV7_L1D_AP1:CurrentFlags:=CurrentFlags or ARMV7_L2D_AP1;
      ARMV7_L1D_AP2:CurrentFlags:=CurrentFlags or ARMV7_L2D_AP2;
      ARMV7_L1D_AP3:CurrentFlags:=CurrentFlags or ARMV7_L2D_AP3;
     end;
     {TEX}
     case (CurrentEntry and ARMV7_L1D_TEX_MASK) of
      ARMV7_L1D_TEX0:CurrentFlags:=CurrentFlags or ARMV7_L2D_SMALL_TEX0;
      ARMV7_L1D_TEX1:CurrentFlags:=CurrentFlags or ARMV7_L2D_SMALL_TEX1;
      ARMV7_L1D_TEX2:CurrentFlags:=CurrentFlags or ARMV7_L2D_SMALL_TEX2;
      ARMV7_L1D_TEX4:CurrentFlags:=CurrentFlags or ARMV7_L2D_SMALL_TEX4;
      ARMV7_L1D_TEX5:CurrentFlags:=CurrentFlags or ARMV7_L2D_SMALL_TEX5;
      ARMV7_L1D_TEX6:CurrentFlags:=CurrentFlags or ARMV7_L2D_SMALL_TEX6;
      ARMV7_L1D_TEX7:CurrentFlags:=CurrentFlags or ARMV7_L2D_SMALL_TEX7;
     end; 
     {Domain} 
     Flags:=Flags or (CurrentEntry and ARMV7_L1D_DOMAIN_MASK); {Add Domain to Coarse Page Table}
      
     {Create 256 Small Page (4KB) entries}
     CoarseOffset:=0;
     for Count:=0 to 255 do
      begin
       PLongWord(PtrUInt(CoarseBase) + PtrUInt(CoarseOffset))^:=(CurrentBase + CurrentOffset) or CurrentFlags or ARMV7_L2D_TYPE_SMALL;
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
 TableFlags:=Flags and not(ARMV7_L1D_COARSE_BASE_MASK);
 //To Do
 
 {Write Page Table}
 PLongWord(PtrUInt(PAGE_TABLE_BASE) + PtrUInt(TableOffset))^:=CoarseBase or TableFlags or ARMV7_L1D_TYPE_COARSE;
 
 Result:=True;
end;

{==============================================================================}

function ARMv7GetPageTableLarge(Address:PtrUInt):LongWord;
{Get the descriptor for a Large Page Table entry (64KB)}
{See page ???}
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
 TableBase:=(Address and ARMV7_L1D_SECTION_BASE_MASK);
 
 {Get Table Offset}
 TableOffset:=((TableBase shr 20) shl 2); {Divide Base by 1MB then multiply by 4 to get Offset into Page Table}
 
 {Read Page Table}
 CoarseEntry:=PLongWord(PtrUInt(PAGE_TABLE_BASE) + PtrUInt(TableOffset))^;

 {Check Level 1 Type}
 if (CoarseEntry and ARMV7_L1D_TYPE_COARSE) <> 0 then
  begin
   {Get Coarse Base}
   CoarseBase:=(CoarseEntry and ARMV7_L1D_COARSE_BASE_MASK);
   
   {Get Large Base}
   LargeBase:=(Address and ARMV7_L2D_LARGE_BASE_MASK);
   
   {Get Large Offset}
   LargeOffset:=(((LargeBase shr 16) and $000000FF) shl 2); {Divide Base by 64KB then multiply by 4 to get Offset into Coarse Page Table}
   
   {Read Coarse Page Table}
   LargeEntry:=PLongWord(PtrUInt(CoarseBase) + PtrUInt(LargeOffset))^;
   
   {Check Level 2 Type}
   if (LargeEntry and ARMV7_L2D_TYPE_LARGE) <> 0 then
    begin
     {Return Result}
     Result:=LargeEntry;
    end;
  end; 
end;

{==============================================================================}

function ARMv7SetPageTableLarge(Address,PhysicalAddress:PtrUInt;Flags:Word):Boolean;
{Set the descriptor for a Large Page Table entry (64KB)}
{Large Page Table descriptors must begin on a 16 longword (64 byte)
 boundary and be repeated for 16 consecutive longwords}
{See page ???}
{Note: Caller must call ARMv7InvalidateTLB after changes if MMU is enabled}
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
 TableBase:=(Address and ARMV7_L1D_SECTION_BASE_MASK);
 
 {Get Table Offset}
 TableOffset:=((TableBase shr 20) shl 2); {Divide Base by 1MB then multiply by 4 to get Offset into Page Table}
 
 {Read Page Table}
 CoarseEntry:=PLongWord(PtrUInt(PAGE_TABLE_BASE) + PtrUInt(TableOffset))^;
 
 {Check Level 1 Type}
 if (CoarseEntry and ARMV7_L1D_TYPE_COARSE) <> 0 then
  begin
   {Get Coarse Base}
   CoarseBase:=(CoarseEntry and ARMV7_L1D_COARSE_BASE_MASK);
 
   {Get Large Base}
   LargeBase:=(Address and ARMV7_L2D_LARGE_BASE_MASK);
   if LargeBase <> Address then Exit; {Must begin on a 64KB boundary}
   
   {Get Physical Base}
   PhysicalBase:=(PhysicalAddress and ARMV7_L2D_LARGE_BASE_MASK);
   if PhysicalBase <> PhysicalAddress then Exit; {Must begin on a 64KB boundary}
 
   {Get Large Offset}
   LargeOffset:=(((LargeBase shr 16) and $000000FF) shl 2); {Divide Base by 64KB then multiply by 4 to get Offset into Coarse Page Table}
   if ((CoarseBase + LargeOffset) and $3F) <> 0 then Exit;  {Must begin on a 64 byte boundary}
   
   {Check Flags}
   LargeFlags:=Flags and not(ARMV7_L2D_LARGE_BASE_MASK);
   //To Do
 
   {Write Coarse Page Table (16 consecutive entries)}
   for Count:=0 to 15 do
    begin
     PLongWord(PtrUInt(CoarseBase) + PtrUInt(LargeOffset))^:=PhysicalBase or LargeFlags or ARMV7_L2D_TYPE_LARGE;
     Inc(LargeOffset,SizeOf(LongWord));
    end; 
    
   Result:=True;
  end;   
end;

{==============================================================================}

function ARMv7GetPageTableSmall(Address:PtrUInt):LongWord;
{Get the descriptor for a Small Page Table entry (4KB)}
{See page ???}
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
 TableBase:=(Address and ARMV7_L1D_SECTION_BASE_MASK);
 
 {Get Table Offset}
 TableOffset:=((TableBase shr 20) shl 2); {Divide Base by 1MB then multiply by 4 to get Offset into Page Table}
 
 {Read Page Table}
 CoarseEntry:=PLongWord(PtrUInt(PAGE_TABLE_BASE) + PtrUInt(TableOffset))^;
 
 {Check Level 1 Type}
 if (CoarseEntry and ARMV7_L1D_TYPE_COARSE) <> 0 then
  begin
   {Get Coarse Base}
   CoarseBase:=(CoarseEntry and ARMV7_L1D_COARSE_BASE_MASK);
   
   {Get Small Base}
   SmallBase:=(Address and ARMV7_L2D_SMALL_BASE_MASK);
   
   {Get Small Offset}
   SmallOffset:=(((SmallBase shr 12) and $000000FF) shl 2); {Divide Base by 4KB then multiply by 4 to get Offset into Coarse Page Table}
   
   {Read Coarse Page Table}
   SmallEntry:=PLongWord(PtrUInt(CoarseBase) + PtrUInt(SmallOffset))^;
   
   {Check Level 2 Type}
   if (SmallEntry and ARMV7_L2D_TYPE_SMALL) <> 0 then
    begin
     {Return Result}
     Result:=SmallEntry;
    end;
  end; 
end;

{==============================================================================}

function ARMv7SetPageTableSmall(Address,PhysicalAddress:PtrUInt;Flags:Word):Boolean;
{Set the descriptor for a Small Page Table entry (4KB)}
{See page ???}
{Note: Caller must call ARMv7InvalidateTLB after changes if MMU is enabled}
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
 TableBase:=(Address and ARMV7_L1D_SECTION_BASE_MASK);
 
 {Get Table Offset}
 TableOffset:=((TableBase shr 20) shl 2); {Divide Base by 1MB then multiply by 4 to get Offset into Page Table}
 
 {Read Page Table}
 CoarseEntry:=PLongWord(PtrUInt(PAGE_TABLE_BASE) + PtrUInt(TableOffset))^;
 
 {Check Level 1 Type}
 if (CoarseEntry and ARMV7_L1D_TYPE_COARSE) <> 0 then
  begin
   {Get Coarse Base}
   CoarseBase:=(CoarseEntry and ARMV7_L1D_COARSE_BASE_MASK);
 
   {Get Small Base}
   SmallBase:=(Address and ARMV7_L2D_SMALL_BASE_MASK);
   if SmallBase <> Address then Exit; {Must begin on a 4KB boundary}
   
   {Get Physical Base}
   PhysicalBase:=(PhysicalAddress and ARMV7_L2D_SMALL_BASE_MASK);
   if PhysicalBase <> PhysicalAddress then Exit; {Must begin on a 4KB boundary}
 
   {Get Small Offset}
   SmallOffset:=(((SmallBase shr 12) and $000000FF) shl 2); {Divide Base by 4KB then multiply by 4 to get Offset into Coarse Page Table}
 
   {Check Flags}
   SmallFlags:=Flags and not(ARMV7_L2D_SMALL_BASE_MASK);
   //To Do
   
   {Write Coarse Page Table}
   PLongWord(PtrUInt(CoarseBase) + PtrUInt(SmallOffset))^:=PhysicalBase or SmallFlags or ARMV7_L2D_TYPE_SMALL;
   
   Result:=True;
  end;   
end;

{==============================================================================}

function ARMv7GetPageTableSection(Address:PtrUInt):LongWord;
{Get the descriptor for a Page Table Section (1MB) or Supersection (16MB)}
{See page ???}
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
 SectionBase:=(Address and ARMV7_L1D_SECTION_BASE_MASK);
 
 {Get Section Offset}
 SectionOffset:=((SectionBase shr 20) shl 2); {Divide Base by 1MB then multiply by 4 to get Offset into Page Table}
 
 {Read Page Table}
 SectionEntry:=PLongWord(PtrUInt(PAGE_TABLE_BASE) + PtrUInt(SectionOffset))^;
 
 {Check Level 1 Type}
 if (SectionEntry and ARMV7_L1D_TYPE_SECTION) <> 0 then
  begin
   {Return Result}
   Result:=SectionEntry;
  end; 
end;

{==============================================================================}

function ARMv7SetPageTableSection(Address,PhysicalAddress:PtrUInt;Flags:LongWord):Boolean;
{Set the descriptor for a Page Table Section (1MB)}
{See page ???}
{Note: Caller must call ARMv7InvalidateTLB after changes if MMU is enabled}
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
 SectionBase:=(Address and ARMV7_L1D_SECTION_BASE_MASK);
 if SectionBase <> Address then Exit; {Must begin on a 1MB boundary}
 
 {Get Physical Base}
 PhysicalBase:=(PhysicalAddress and ARMV7_L1D_SECTION_BASE_MASK);
 if PhysicalBase <> PhysicalAddress then Exit; {Must begin on a 1MB boundary}
 
 {Get Section Offset}
 SectionOffset:=((SectionBase shr 20) shl 2); {Divide Base by 1MB then multiply by 4 to get Offset into Page Table}
 
 {Check Flags}
 SectionFlags:=Flags and not(ARMV7_L1D_SECTION_BASE_MASK);
 SectionFlags:=SectionFlags and not(ARMV7_L1D_FLAG_SUPERSECTION);
 //To Do
 
 {Write Page Table}
 PLongWord(PtrUInt(PAGE_TABLE_BASE) + PtrUInt(SectionOffset))^:=PhysicalBase or SectionFlags or ARMV7_L1D_TYPE_SECTION;
 
 Result:=True;
end;

{==============================================================================}

function ARMv7SetPageTableSupersection(Address,PhysicalAddress:PtrUInt;Flags:LongWord):Boolean;
{Set the descriptor for a Page Table Supersection (16MB)}
{Supersection Page Table descriptors must begin on a 16 longword (64 byte)
 boundary and be repeated for 16 consecutive longwords}
{See page ???}
{Note: Caller must call ARMv7InvalidateTLB after changes if MMU is enabled}
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
 SectionBase:=(Address and ARMV7_L1D_SUPERSECTION_BASE_MASK); {ARMV7_L1D_SECTION_BASE_MASK}
 if SectionBase <> Address then Exit; {Must begin on a 16MB boundary}
 
 {Get Physical Base}
 PhysicalBase:=(PhysicalAddress and ARMV7_L1D_SUPERSECTION_BASE_MASK); {ARMV7_L1D_SECTION_BASE_MASK}
 if PhysicalBase <> PhysicalAddress then Exit; {Must begin on a 16MB boundary}
 
 {Get Section Offset}
 SectionOffset:=((SectionBase shr 20) shl 2); {Divide Base by 1MB then multiply by 4 to get Offset into Page Table}
 if ((PAGE_TABLE_BASE + SectionOffset) and $3F) <> 0 then Exit; {Must begin on a 64 byte boundary}
 
 {Check Flags}
 SectionFlags:=Flags and not(ARMV7_L1D_SECTION_BASE_MASK);
 SectionFlags:=SectionFlags or ARMV7_L1D_FLAG_SUPERSECTION;
 //To Do
 
 {Write Page Table (16 consecutive entries)}
 for Count:=0 to 15 do
  begin
   PLongWord(PtrUInt(PAGE_TABLE_BASE) + PtrUInt(SectionOffset))^:=PhysicalBase or SectionFlags or ARMV7_L1D_TYPE_SECTION;
   Inc(SectionOffset,SizeOf(LongWord));
  end; 
 
 Result:=True;
end;

{==============================================================================}
{==============================================================================}

end.
