{
Ultibo Initialization code for QEMU VersatilePB.

Copyright (C) 2020 - SoftOz Pty Ltd.

Arch
====

 ARMv7 (Cortex A8)
 ARMv8 (Cortex A53)

Boards
======

 QEMU - VersatilePB 

Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)
 
Credits
=======

 Information for this unit was obtained from:
 
  Linux - \arch\arm\mach-versatile\*
  
References
==========
 
 Cortex-A8 Technical Reference Manual
 
 http://wiki.qemu.org/download/qemu-doc.html#ARM-System-emulator
 
 http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.dui0224i/index.html
 
QEMU VersatilePB
================
 
 SoC: ARM926EJ-S (Emulated)
 
 CPU: Cortex A8 (ARMv7) (1 @ ???MHz)
  or  Cortex A53 (ARMv8) (1 @ ???MHz)
 
 Cache: L1 16KB / L2 0KB
    or  L1 ??KB / L2 ??KB
    
 FPU: VFPV3
  or  VFP
  
 GPU: (None)
 
 RAM: 256MB
 
 USB: PCI OHCI USB controller
 
 LAN: SMC 91c111 Ethernet adapter 
  
 SD/MMC: PL181 MultiMedia Card Interface with SD card
 
 WiFi: (None)
 
 Bluetooth: (None)
 
 Other: PL190 Vectored Interrupt Controller 
        Four PL011 UARTs
        PL110 LCD controller 
        PL050 KMI with PS/2 keyboard and mouse
        PCI host bridge
        PCI OHCI USB controller
        LSI53C895A PCI SCSI Host Bus Adapter with hard disk and CD-ROM devices
 
        GPIO / I2C / Watchdog / SP804 Timer
 
Boot QEMUVPB
============

 The QEMU system emulator (qemu-system-arm) will load this code at address 0x00010000 onwards and set the
 following registers before jumping to this code.

 R0 - Zero
 R1 - Machine Type (Versatile_PB = 0x0183)
 R2 - Address of the ARM Tags structure (Normally 0x0100)

 On entry to this code the processor will be in the following state:
 
 World - Secure
 Mode - Supervisor (ARM_MODE_SVC)
 MMU - Disabled
 FPU - Disabled
 L1 Data Cache - Disabled
 L1 Instruction Cache - Disabled
 Branch Predication - Disabled
 Unaligned Data Access - Disabled

 Ultibo switches the processor to System mode for all operations and remains in the Secure world.

 The initialization process enables the MMU, FPU, L1 Cache and other performance optimizations.
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString} 
{$inline on}   {Allow use of Inline procedures}

unit BootQEMUVPB;

interface

{==============================================================================}
{Global definitions} {Must be prior to uses}
{$INCLUDE GlobalDefines.inc}

uses GlobalConfig,GlobalConst,GlobalTypes,VersatilePB,Platform,PlatformQEMUVPB,{$IFDEF CPUARM}PlatformARM,PlatformARMv7,{$ENDIF CPUARM}{$IFDEF CPUAARCH64}PlatformAARCH64,PlatformARMv8,{$ENDIF CPUAARCH64}Threads{$IFDEF CONSOLE_EARLY_INIT},Devices,Framebuffer,Console{$ENDIF}{$IFDEF LOGGING_EARLY_INIT},Logging{$ENDIF}; 
 
{==============================================================================}
{Boot Functions}
procedure Startup;

procedure Vectors; 

procedure StartupHandler;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Boot Functions}
procedure Startup; assembler; nostackframe; [public, alias: '_START'];
{Entry point of Ultibo on QEMU VersatilePB, this will be the very first byte executed
 and will be loaded by QEMU at address 0x00010000}
{$IFDEF CPUARM} 
asm
 //Save the pointer to the ARM Tags that the bootloader should have passed
 //to us in R2.
 //Check if the value is 0 and assume 0x0100 in case of an old bootloader
 ldr r3, .LARMTagsAddress
 cmp r2, #0
 moveq r2, #0x0100
 str r2, [r3]

 //Save the ARM Machine Type that the bootloader should have passed to us
 //in R1. See: http://www.arm.linux.org.uk/developer/machines/
 ldr r3, .LARMMachineType
 str r1, [r3]
 
 //Save the ARM Boot Mode that the CPU was in at startup
 ldr r3, .LARMBootMode
 mrs r0, cpsr
 and r0, r0, #ARM_MODE_BITS
 str r0, [r3] 

 //Save the Vector Base Address that was current at startup
 ldr r3, .LARMBootVectors
 mrc p15, #0, r0, cr12, cr0, #0
 str r0, [r3] 
 
 //Continue execution at the StartupHandler
 b StartupHandler
  
.LARMBootMode:
  .long ARMBootMode
.LARMBootVectors:
  .long ARMBootVectors
.LARMTagsAddress:
  .long ARMTagsAddress  
.LARMMachineType:
  .long ARMMachineType
end;
{$ENDIF CPUARM}
{$IFDEF CPUAARCH64}
asm
 //TestingAARCH64
 mov x0, #0x101f
 lsl x0, x0, #16
 add x0, x0, #0x1000
 
 //VERSATILEPB_UART0_REGS_BASE
 //mov  x0, #0x1000 
 //movk x0, #0x101f, lsl #16

 mov w1, #72  //H
 str w1,[x0]

 mov w1, #69  //E
 str w1,[x0]

 mov w1, #76  //L
 str w1,[x0]

 mov w1, #76  //L
 str w1,[x0]

 mov w1, #79  //O
 str w1,[x0]
 
 mov w1, #32  //<Space>
 str w1,[x0]
 
 mov w1, #85  //U
 str w1,[x0]
 
 mov w1, #76  //L
 str w1,[x0]

 mov w1, #84  //T
 str w1,[x0]

 mov w1, #73  //I
 str w1,[x0]

 mov w1, #66  //B
 str w1,[x0]

 mov w1, #79  //O
 str w1,[x0]
 
 mov w1, #32  //<Space>
 str w1,[x0]

 mov w1, #54  //6
 str w1,[x0]

 mov w1, #52  //4
 str w1,[x0]
 
.Loop:  
 
 b .Loop
 
 //TestingAARCH64
 bl QEMUVPBBootOutput
end;
{$ENDIF CPUAARCH64}

{==============================================================================}

procedure Vectors; assembler; nostackframe; 
{ARM exception vector table which is copied to address 0 by the StartupHandler
 See A2.6 "Exceptions" of the ARM Architecture Reference Manual}
{$IFDEF CPUARM}
asm
 ldr pc, .Lreset_addr     //Reset Handler 
 ldr pc, .Lundef_addr	  //Undefined Instruction Handler 
 ldr pc, .Lswi_addr	      //Software Interrupt Handler 
 ldr pc, .Lprefetch_addr  //Prefetch Abort Handler 
 ldr pc, .Labort_addr	  //Data Abort Handler 
 ldr pc, .Lreserved_addr  //Reserved Handler
 ldr pc, .Lirq_addr	      //IRQ (Interrupt Request) Handler 
 ldr pc, .Lfiq_addr	      //FIQ (Fast Interrupt Request) Handler 

.Lreset_addr:
  .long ARMv7ResetHandler   
.Lundef_addr:     
  .long ARMv7UndefinedInstructionHandler      
.Lswi_addr:       
  .long ARMv7SoftwareInterruptHandler        
.Lprefetch_addr:  
  .long ARMv7PrefetchAbortHandler   
.Labort_addr:     
  .long ARMv7DataAbortHandler      
.Lreserved_addr:  
  .long ARMv7ReservedHandler  
.Lirq_addr:       
  .long ARMv7IRQHandler      
.Lfiq_addr:       
  .long ARMv7FIQHandler        
end;
{$ENDIF CPUARM}
{$IFDEF CPUAARCH64}
asm
 //To Do
end;
{$ENDIF CPUAARCH64}

{==============================================================================}

procedure StartupHandler; assembler; nostackframe; 
{Startup handler routine executed to start the Ultibo kernel}
{$IFDEF CPUARM}
asm
 //Invalidate all Caches before starting the boot process
 bl ARMv7InvalidateCache
 
 //Invalidate the TLB before starting the boot process
 bl ARMv7InvalidateTLB
 
 //Change to SYS mode and ensure all interrupts are disabled
 //so the ARM processor is in a known state.
 cpsid if, #ARM_MODE_SYS

 //Copy the ARM exception table from Vectors to the vector base address.
 mov r0, #QEMUVPB_VECTOR_TABLE_BASE
 ldr r1, .L_vectors
 ldmia r1!, {r2-r9}
 stmia r0!, {r2-r9}
 ldmia r1!, {r2-r9}
 stmia r0!, {r2-r9}

 //Set the Vector Base Address register in the System Control
 //register to the address of the vector table base above.
 mov r0, #QEMUVPB_VECTOR_TABLE_BASE
 mcr p15, #0, r0, cr12, cr0, #0
 
 //Enable Unaligned Memory Accesses (U Bit) in the System Control
 //Register to simplify memory access routines from Pascal code.
 //
 //This would normally occur in CPUInit but is done here to allow
 //calls to Pascal code during initialization. (Always enabled in ARMv7)
 //mrc p15, #0, r0, cr1, cr0, #0
 //orr r0, #ARMV7_CP15_C1_U_BIT
 //mcr p15, #0, r0, cr1, cr0, #0
 
 //Clear the entire .bss section of the kernel image.
 //Linker should have aligned bss start and end to a 4KB boundary
 ldr r0, .L_bss_start
 ldr r1, .L_bss_end
 mov r2, #0
 mov r3, #0
 mov r4, #0
 mov r5, #0
 b .Lbssloopa
.Lbssloopb:
 stmia r0!, {r2-r5}
.Lbssloopa:
 cmp r0, r1 	    //Check if we have reached bss_end yet.
 bcc .Lbssloopb	    //Repeat the loop if still more to go.

 //Ensure INITIAL_STACK_SIZE is a multiple of 4KB
 ldr r0, .LINITIAL_STACK_SIZE
 ldr r2, [r0]
 //Divide by 4KB, Multiply by 4KB
 mov r3, r2
 lsr r3, r3, #12
 lsl r3, r3, #12
 cmp r2, r3
 beq .LSkipRoundStack
 mov r2, r3
 add r2, r2, #4096
 str r2, [r0] 
.LSkipRoundStack:
  
 //Ensure INITIAL_HEAP_SIZE is a multiple of 4KB
 ldr r0, .LINITIAL_HEAP_SIZE
 ldr r2, [r0]
 //Divide by 4KB, Multiply by 4KB
 mov r3, r2
 lsr r3, r3, #12
 lsl r3, r3, #12
 cmp r2, r3
 beq .LSkipRoundHeap
 mov r2, r3
 add r2, r2, #4096
 str r2, [r0] 
.LSkipRoundHeap:
  
 //Determine how many second level page tables are required.
 //Each one represents 1MB and is 1KB in size (256 4 byte entries).
 //R1 will contain the address of the bss_end from above.
 //Add the initial stack size to bss_end
 ldr r0, .LINITIAL_STACK_SIZE
 ldr r0, [r0]
 add r3, r1, r0
 //Add the initial heap size to R3
 ldr r0, .LINITIAL_HEAP_SIZE
 ldr r0, [r0]
 add r3, r3, r0
 //Divide R3 by 1MB to get the count.
 lsr r2, r3, #20 
 //Add one for any leftover amount and one for safety.
 add r2, r2, #2
 //Store the second level page table used count
 ldr r0, .LPAGE_TABLES_USED
 str r2, [r0]
 
 //Get the second level page table free count
 ldr r0, .LPAGE_TABLES_FREE
 ldr r0, [r0]
 //Add the free count to the used count
 add r2, r2, r0
 //Store the second level page table count
 ldr r0, .LPAGE_TABLES_COUNT
 str r2, [r0]
 
 //Put the second level page tables directly after the kernel image.
 //R1 will contain the address of the bss_end from above.
 //Store the start address of the second level page tables.
 ldr r0, .LPAGE_TABLES_ADDRESS
 str r1, [r0]
 //Multiply count by 1KB to get the size.
 //R2 will contain the number of page tables from above.
 lsl r2, r2, #10
 //Round up the size to a multiple of 4KB
 //Divide by 4KB, Multiply by 4KB
 mov r3, r2
 lsr r3, r3, #12
 lsl r3, r3, #12
 cmp r2, r3
 beq .LSkipRoundTables
 mov r2, r3
 add r2, r2, #4096
.LSkipRoundTables:
 //Store the size of the second level page tables.
 ldr r0, .LPAGE_TABLES_LENGTH
 str r2, [r0]  
 add r1, r1, r2
  
 //Put the initial thread stack directly after the page tables.
 //R1 will contain the end address of the page tables.
 ldr r0, .LINITIAL_STACK_SIZE
 ldr r0, [r0]
 add sp, r1, r0
  
 //Store the pointer to the top of the initial thread stack.
 //SP will contain the address of the top of the initial stack.
 ldr r0, .LINITIAL_STACK_BASE
 str sp, [r0]
  
 //Store the start address of the initial memory heap.
 //SP will contain the address of the top of the initial stack
 //which will also be the starting address of the initial heap.
 ldr r0, .LINITIAL_HEAP_BASE
 str sp, [r0]

 //Move the initial stack away from the initial heap but remain 8 byte aligned.
 sub sp, sp, #8
 
 //Initialize the QEMUVPB Platform specific behaviour (Memory, Peripherals, Interrupts etc).
 bl QEMUVPBInit
  
 //Initialize the ARM Platform specific behaviour (IRQ, FIQ, Abort etc).
 bl ARMInit

 //Initialize the ARMv7 Platform specific behaviour (Halt, Pause, Locks, Barriers, ContextSwitch, ThreadStack etc).
 bl ARMv7Init

 //Initialize the Ultibo Platform (CPU, FPU, MMU, Heap, Clock, Interrupts, ATAGS, Power etc).
 bl PlatformInit
 
 {$IFDEF CONSOLE_EARLY_INIT}
 //Initialize the Ultibo Locking primitives
 bl LocksInit
  
 //Initialize the Ultibo Device manager
 bl DevicesInit
  
 //Initialize the Peripheral settings
 bl PeripheralInit
  
 //Initialize the Framebuffer device
 bl FramebufferInit
  
 //Initialize the Console device
 bl ConsoleInit
 
 //Start the Boot Console device
 bl BootConsoleStart
 {$ENDIF}
  
 //Initialize the Ultibo Threading which will create the IRQ, FIQ and Idle Threads
 //and branch to the platform independent Pascal startup code. 
 //This should never return unless an error occurs during startup.
 bl ThreadsInit 
 
 //If ThreadsInit returns then halt the CPU
 b ARMv7Halt
  
.L_vectors:
  .long Vectors
  
.LPAGE_TABLES_USED:
  .long PAGE_TABLES_USED  
.LPAGE_TABLES_FREE:
  .long PAGE_TABLES_FREE
.LPAGE_TABLES_COUNT:
  .long PAGE_TABLES_COUNT
.LPAGE_TABLES_ADDRESS:
  .long PAGE_TABLES_ADDRESS
.LPAGE_TABLES_LENGTH:
  .long PAGE_TABLES_LENGTH
  
.LINITIAL_STACK_SIZE:
  .long INITIAL_STACK_SIZE
.LINITIAL_STACK_BASE:
  .long INITIAL_STACK_BASE

.LINITIAL_HEAP_SIZE:
  .long INITIAL_HEAP_SIZE
.LINITIAL_HEAP_BASE:
  .long INITIAL_HEAP_BASE

.L_data:
  .long _data
.L_edata:
  .long _edata
.L_text_start:
  .long _text_start
.L_etext:
  .long _etext
.L_bss_start:
  .long _bss_start
.L_bss_end:
  .long _bss_end
end;
{$ENDIF CPUARM}
{$IFDEF CPUAARCH64}
asm
 //To Do
end;
{$ENDIF CPUAARCH64}

{==============================================================================}
{==============================================================================}
 
end.
