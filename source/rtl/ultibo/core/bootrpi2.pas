{
Ultibo Initialization code for Raspberry Pi 2.

Copyright (C) 2018 - SoftOz Pty Ltd.

Arch
====

 ARMv7 (Cortex A7)

Boards
======

 Raspberry Pi 2 - Model B
 Raspberry Pi 3 - Model B/B+/A+
 Raspberry Pi CM3
 
Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)
 
Credits
=======

 Information for this unit was obtained from:
 
  rsta2 (circle) - https://github.com/rsta2/circle
  
  dwelch67 (raspberrypi) - https://github.com/dwelch67/raspberrypi
  
  PeterLemon (RaspberryPi) - https://github.com/PeterLemon/RaspberryPi
  
  brianwiddas (pi-baremetal) - https://github.com/brianwiddas/pi-baremetal
  
  Linux - \arch\arm\include\asm\assembler.h  (Switching from HYP mode to SVC mode)
  
  OSDev - http://wiki.osdev.org/Raspberry_Pi_Bare_Bones
          http://wiki.osdev.org/ARM_RaspberryPi_Tutorial_C
 
References
==========

 BCM2835 ARM Peripherals

 QA7 Rev3.4
 
 Cortex-A7 MPCore Technical Reference Manual (Revision: r0p5)
 
 ARM v7 Architecture Reference Manual
 
 ARM Architecture Reference Manual (ARMv7-A and ARMv7-R edition)
 
 Linux Device Tree files in /arch/arm/boot/dts
 
  bcm2709.dtsi
  bcm2709-rpi-2-b.dts

 RPi Configuration
  
  http://elinux.org/RPi_Configuration
  
 RPi2 Boot Loader (Origin Unknown)
 
  http://pastebin.com/rgGgBuTN
  
  Referenced from here and appears to be "official": https://www.raspberrypi.org/forums/viewtopic.php?f=63&t=98367&start=375#p697474
  
 RPi2 HYP Mode Boot Loader (Unofficial)
 
  https://github.com/slp/rpi2-hyp-boot/blob/master/rpi2-hyp-boot.S
    
 Raspberry Pi Boot Stubs 
 
  https://github.com/raspberrypi/tools/tree/master/armstubs
    
Raspberry Pi 2
==============

 SoC: Broadcom BCM2836
 
 CPU: Cortex A7 (ARMv7) (4 @ 900MHz)

 Cache: L1 32KB (Per Core) / L2 512KB (Shared by all Cores)
 
 FPU: VFPV3
 
 GPU: Broadcom VideoCore IV (VC4)
 
 RAM: 1GB
 
 USB: Synopsys DesignWare Hi-Speed USB 2.0 On-The-Go Controller (DWCOTG)
 
 LAN: SMSC LAN9514 (SMSC95XX)
  
 SD/MMC: Arasan (BCM2709)
 
 WiFi: (None)
 
 Bluetooth: (None)

 Other: GPIO / SPI / I2C / I2S / PL011 (UART) / PWM / SMI / Watchdog (PM) / Random (RNG) / Timer ???
 
Boot RPi2
=========

 The boot loader on the Raspberry Pi 2 will load this code at address 0x00008000 onwards and set the
 following registers before jumping to this code.

 R0 - Zero
 R1 - Machine Type (Raspberry Pi 2 or BCM2709 = 0x0C42) 
 R2 - Address of the ARM Tags structure (Normally 0x0100)

 On entry to this code the processor will be in the following state:

 World - Non Secure (Firmware causes a switch to Non Secure before jumping to Startup code)
 Mode - Supervisor (ARM_MODE_SVC) Note: Firmware later than 2/10/2015 will boot in Hypervisor mode (ARM_MODE_HYP)
 MMU - Disabled
 FPU - Disabled
 L1 Data Cache - Enabled (Firmware enabled prior to Non Secure switch) 
 L1 Instruction Cache - Enabled (Firmware enabled prior to Non Secure switch) 
 Branch Predication - Disabled
 Unaligned Data Access - Enabled (Always enabled on ARMv7)
 SMP Coherence - Enabled (Firmware enabled prior to Non Secure switch) 
 
 If the processor is in Hypervisor mode (Firmware behaviour after 2/10/2015) then Ultibo switches
 it to Supervisor mode during initial boot.
 
 If the configuration option RPI2_SECURE_BOOT is set to 1 then Ultibo switches the processor back
 to Secure world during initial boot (see note below).
 
 Ultibo then switches the processor to System mode for all operations and remains in either the
 Secure or Non Secure World as per the option above.

 The initialization process enables the MMU, FPU, L1 Cache and other performance optimizations.
 
 Returning to Secure World:
 --------------------------
 
 The Raspberry Pi 2 firmware always switches to Non Secure world in order to:
 
  a) Reset CNTVOFF (Virtual Offset register) to zero
  
  b) Switch to Hypervisor mode (Firmware later than 2/10/2015)
  
 It is not normally possible (by design) to return from the Non Secure world to the Secure world
 except by invoking the Secure Monitor which then handles any requests in the Secure world.
 
 However due to the fact that this switch is performed by the boot loader code (loaded between 
 0x00000000 and 0x000000FF by the firmware) and that the code appears to set the secure, non secure
 and monitor vector base addresses to 0x00000000 and because all memory is writable from non secure
 at boot (the MMU is disabled) then the option exists to return to the Secure world as follows:
 
  - Install a new Secure Monitor Call vector at the appropriate point in the vector table
  - Clean the data cache by Modified Virtual Address at address 0x00000000
  - Perform a data synchronisation barrier
  - Invalidate the entire instruction cache
  - Perform a data synchronisation barrier
  - Perform an instruction synchronisation barrier
  - Perform a Secure Monitor Call (SMC)
  
  - From within the newly installed Secure Monitor Call read the Secure Configuration Register (SCR)
    mask off the Non Secure (NS) bit, rewrite the SCR and then return to Supervisor mode
 
 Secondary cores also switch back to the Secure world using the same technique however they do not
 need to install the new SMC vector and instead use the one installed by the primary core.
 
 Future changes to the firmware will hopefully not invalidate this technique, however there is also
 the option to use the config.txt parameter kernel_old=1 which would load Ultibo at 0x00000000 and
 allow us to setup the boot state independent of the firmware. This would require changes to both
 Ultibo (this boot module) and to the FPC compiler to change the linker start location.
 
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString} 
{$inline on}   {Allow use of Inline procedures}

unit BootRPi2;

interface

{==============================================================================}
{Global definitions} {Must be prior to uses}
{$INCLUDE GlobalDefines.inc}

uses GlobalConfig,GlobalConst,GlobalTypes,BCM2836,Platform,PlatformRPi2,PlatformARM,PlatformARMv7,Threads{$IFDEF CONSOLE_EARLY_INIT},Devices,Framebuffer,Console{$ENDIF}{$IFDEF LOGGING_EARLY_INIT},Logging{$ENDIF}; 

{==============================================================================}
{Boot Functions}
procedure Startup;

procedure Vectors; 
procedure SecureVectors;

procedure SecureMonitor;

procedure StartupSwitch;
procedure StartupSecure;
procedure StartupHandler;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Boot Functions}
procedure Startup; assembler; nostackframe; [public, alias: '_START'];
{Entry point of Ultibo on Raspberry Pi 2, this will be the very first byte executed
 and will be loaded by the GPU at address 0x00008000}
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

{==============================================================================}

procedure Vectors; assembler; nostackframe; 
{ARM exception vector table which is copied to the vector base address by the
 StartupHandler. See A2.6 "Exceptions" of the ARM Architecture Reference Manual}
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

{==============================================================================}

procedure SecureVectors; assembler; nostackframe; 
{ARM secure vector table which is copied to the sector vector base address by the
 StartupHandler. See A2.6 "Exceptions" of the ARM Architecture Reference Manual}
asm
 ldr pc, .Lreset_addr     //Reset Handler 
 ldr pc, .Lreset_addr	  //Undefined Instruction Handler 
 ldr pc, .Lsmc_addr	      //Secure Monitor Handler 
 ldr pc, .Lreset_addr     //Prefetch Abort Handler 
 ldr pc, .Lreset_addr	  //Data Abort Handler 
 ldr pc, .Lreset_addr     //Reserved Handler
 ldr pc, .Lreset_addr	  //IRQ (Interrupt Request) Handler 
 ldr pc, .Lreset_addr	  //FIQ (Fast Interrupt Request) Handler 

.Lreset_addr:
  .long ARMv7ResetHandler   
.Lsmc_addr:       
  .long SecureMonitor        
end;

{==============================================================================}

procedure SecureMonitor; assembler; nostackframe; 
{Secure monitor mode handler to switch to secure mode}
asm
 //Read the SCR (Secure Configuration Register)
 mrc p15, #0, r1, cr1, cr1, #0

 //Clear the NS bit 
 bic r1, r1, #ARMV7_CP15_C1_SCR_NS  
  
 //Write the SCR (with NS bit clear) 
 mcr p15, #0, r1, cr1, cr1, #0           
 
 //Return to secure SVC mode
 movs    pc, lr                          
end;

{==============================================================================}

procedure StartupSwitch; assembler; nostackframe; 
{Startup handler routine to switch from hypervisor mode}
asm
 //Get the CPSR
 mrs r0, cpsr            
 //Test for HYP mode 
 eor r0, r0, #ARM_MODE_HYP                           
 tst r0, #ARM_MODE_BITS
 //Clear the mode bits
 bic r0, r0, #ARM_MODE_BITS		             
 //Mask IRQ/FIQ bits and set SVC mode 
 orr r0, r0, #ARM_I_BIT | ARM_F_BIT | ARM_MODE_SVC
 
 //Return if not in HYP mode 
 bne .LNoSwitch                                      
 
 //Save value of CNTVOFF from boot
 mrrc p15, #4, r1, r2, cr14
 ldr r3, .LRPi2CNTVOFFLow
 str r1, [r3]
 ldr r3, .LRPi2CNTVOFFHigh
 str r2, [r3]
 
 //Reset CNTVOFF to 0 while in HYP mode
 mov r1, #0
 mcrr p15, #4, r1, r1, cr14
 
 //Mask the Abort bit
 orr r0, r0, #ARM_A_BIT  
 //Load the SPSR 
 msr spsr_cxsf, r0
 //Return to SVC mode
.long 0xE12EF30E  //msr ELR_hyp, lr  (Not supported by the FPC compiler)
.long 0xE160006E  //eret             (Not supported by the FPC compiler)

.LNoSwitch:
 //Set the CPSR (C fields)
 msr cpsr_c, r0                                      
 //Return to startup 
 bx lr
 
.LRPi2CNTVOFFLow:
  .long RPi2CNTVOFFLow
.LRPi2CNTVOFFHigh:
  .long RPi2CNTVOFFHigh
end;

{==============================================================================}

procedure StartupSecure; assembler; nostackframe; 
{Startup handler routine to switch to secure mode}
asm
 //Check the secure boot configuration
 mov r0, #RPI2_SECURE_BOOT
 cmp r0, #0
 beq .LNoSecure
  
 //Attempt to switch back to secure world by performing a
 //secure monitor call to the Secure Monitor handler.
 //Get the vector base address from the System Control register.
 mrc p15, #0, r0, cr12, cr0, #0
 
 //Get the address of the secure vectors
 ldr r1, .LSecureVectors
 
 //Copy the secure vector table
 ldmia r1!, {r2-r9}
 stmia r0!, {r2-r9}
 ldmia r1!, {r2-r9}
 stmia r0!, {r2-r9}

 //Clean Data Cache MVA
 mov r12, #0
 mcr p15, #0, r12, cr7, cr10, #1
 
 //Perform a data synchronisation barrier
 dsb
 
 //Invalidate Instruction Cache
 mov r12, #0
 mcr p15, #0, r12, cr7, cr5, #0

 //Flush Branch Target Cache 
 mov r12, #0
 mcr p15, #0, r12, cr7, cr5, #6

 //Perform a data synchronisation barrier
 dsb
 
 //Perform an instruction synchronisation barrier
 isb
 
 //Perform a secure monitor call (Not supported by the FPC compiler)
 .long 0xE1600070  //smc #0
  
.LNoSecure:
 //Return to startup
 bx lr
 
.LSecureVectors:
  .long SecureVectors
end;

{==============================================================================}

procedure StartupHandler; assembler; nostackframe; 
{Startup handler routine executed to start the Ultibo kernel}
asm
 //Call the HYP mode switch handler in case the CPU is in HYP mode
 bl StartupSwitch

 //Call the secure mode switch handler to return to secure mode
 bl StartupSecure
  
 //Invalidate all Caches before starting the boot process
 bl ARMv7InvalidateCache
 
 //Invalidate the TLB before starting the boot process
 bl ARMv7InvalidateTLB
 
 //Change to SYS mode and ensure all interrupts are disabled
 //so the ARM processor is in a known state.
 cpsid if, #ARM_MODE_SYS

 //Copy the ARM exception table from Vectors to the vector base address.
 mov r0, #RPI2_VECTOR_TABLE_BASE
 ldr r1, .L_vectors
 ldmia r1!, {r2-r9}
 stmia r0!, {r2-r9}
 ldmia r1!, {r2-r9}
 stmia r0!, {r2-r9}

 //Set the Vector Base Address register in the System Control
 //register to the address of the vector table base above.
 mov r0, #RPI2_VECTOR_TABLE_BASE
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
 
 //Initialize the RPi2 Platform specific behaviour (Memory, Peripherals, Interrupts etc).
 bl RPi2Init
  
 //Initialize the ARM Platform specific behaviour (IRQ, FIQ, Abort etc).
 bl ARMInit

 //Initialize the ARMv7 Platform specific behaviour (Halt, Pause, Locks, Barriers, ContextSwitch, ThreadStack etc).
 bl ARMv7Init

 //Initialize the Ultibo Platform (CPU, FPU, MMU, Heap, Clock, Interrupts, ATAGS, Power etc).
 bl PlatformInit
  
 {$IFDEF CONSOLE_EARLY_INIT}
 //Initialize the Ultibo Locking primitives
 bl LocksInit;
  
 //Initialize the Ultibo Device manager
 bl DevicesInit;
  
 //Initialize the Framebuffer device
 bl FramebufferInit;
  
 //Initialize the Console device
 bl ConsoleInit;
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

{==============================================================================}
{==============================================================================}
 
end.
