{
Ultibo Initialization code for Raspberry Pi 4.

Copyright (C) 2023 - SoftOz Pty Ltd.

Arch
====

 ARMv8 (Cortex A72)

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

  Circle - https://github.com/rsta2/circle

  Linux - https://github.com/raspberrypi/linux

  Linux - \arch\arm\include\asm\assembler.h  (Switching from HYP mode to SVC mode)

References
==========

 BCM2711 ARM Peripherals

 QA7 Rev3.4

 Cortex-A8 MPCore Technical Reference Manual (Revision: r0p4)

 ARM v8 Architecture Reference Manual

 ARM Architecture Reference Manual (ARMv8-A)

 Linux Device Tree files in /arch/arm/boot/dts

  bcm2711.dtsi
  bcm2711-rpi-4-b.dts

 Raspberry Pi Boot Stubs

  https://github.com/raspberrypi/tools/tree/master/armstubs

Raspberry Pi 4
==============

 SoC: Broadcom BCM2838

 CPU: Cortex A72 (ARMv8) (4 @ 1500MHz)

 Cache: L1 32KB (Per Core) / L2 1024KB (Shared by all Cores)

 FPU: VFPV4

 GPU: Broadcom VideoCore VI (VC6)

 RAM: 1/2/4/8GB

 USB: xHCI
      Synopsys DesignWare Hi-Speed USB 2.0 On-The-Go Controller (DWCOTG)

 LAN: Broadcom (BCM54213)

 SD/MMC: Arasan (BCM2711)
         Broadcom (Proprietry)

 WiFi: Broadcom (BCM43438)

 Bluetooth: Broadcom (BCM43438)

 Other: GPIO / SPI / I2C / I2S / PL011 (UART) / PWM / SMI / Watchdog (PM) / Random (RNG) / Timer

Boot RPi4
=========

 The boot loader on the Raspberry Pi 4 will load this code at address 0x00008000 onwards and set the
 following registers before jumping to this code.

 R0 - Zero
 R1 - Machine Type (Raspberry Pi 4 or BCM2711 = 0x0C42)
 R2 - Address of the ARM Tags structure (Normally 0x0100)

 On entry to this code the processor will be in the following state:

 World - Non Secure (Firmware causes a switch to Non Secure before jumping to Startup code)
 Mode - Hypervisor (ARM_MODE_HYP)
 MMU - Disabled
 FPU - Disabled
 Data Cache - Disabled
 Instruction Cache - Enabled (Firmware enabled during boot)
 Branch Predication - Disabled
 Unaligned Data Access - Enabled (Always enabled on ARMv8)
 SMP Coherence - Enabled (Firmware enabled during boot)

 The firmware also does initial configuration of the GIC as follows:

  Distributor interface enabled for Group 0 and Group 1 interrupts
  CPU interface enabled for Group 1 interrupts
  All interrupts routed to Group 1

 All secondary CPUs will be executing a loop reading their designated local peripherals mailbox0
 and waiting for a start address to begin execution.

 Ultibo switches from Hypervisor mode to Supervisor mode during initial boot.

 If the configuration option ARMSecureBoot is set to 1 then Ultibo switches the processor back
 to Secure world during initial boot (see note below).

 Ultibo then switches the processor to System mode for all operations and remains in either the
 Secure or Non Secure World as per the option above.

 The initialization process enables the MMU, FPU, L1 Cache and other performance optimizations.

 Note that this code is currently almost identical to the Raspberry Pi 2 and 3 boot code but has been
 separated to allow for supporting 64 bit mode on the ARMv8 in future. Ultibo currently runs the RPi4
 in 32 bit mode which is almost identical to ARMv7.

 ARM64 support on Raspberry Pi 4
 -------------------------------

 A 64 bit loader is included in the firmware and if the file kernel8.img exists in the boot partition
 the firmware will attempt to boot this image in 64 bit mode.

 The boot loader on the Raspberry Pi 4 will load this code at address 0x00080000 onwards and set the
 following registers before jumping to this code.

 X0 - Address of the ARM Tags or Device Tree structure
 X1 - Zero
 X2 - Zero
 X3 - Zero

 On entry to this code in 64 bit mode the processor will be in the following state:

 World - Non Secure (Firmware causes a switch to Non Secure before jumping to Startup code)
 Mode - EL2 (Hypervisor)
 MMU - Disabled
 FPU - Disabled
 Data Cache - Disabled
 Instruction Cache - Enabled (Firmware enabled during boot)
 Branch Predication - Disabled
 Unaligned Data Access - Enabled (Always enabled on ARMv8)
 SMP Coherence - Enabled (Firmware enabled during boot)

 The boot loader also does initial configuration of the GIC as follows:

  Distributor interface enabled for Group 0 and Group 1 interrupts
  CPU interface enabled for Group 1 interrupts
  All interrupts routed to Group 1

 All secondary CPUs will be executing a loop reading a memory address at 0xd8/e0/e8/f0 and
 waiting for a start address to begin execution.

 Returning to Secure World:
 --------------------------

 The Raspberry Pi 4 firmware always switches to Non Secure world in order to:

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

 Firmware changes from October 2020 disable the ability to perform the SMC instruction from non
 secure mode which means this technique will no longer work with the default firmware. To allow
 continued support for returning to the Secure world a custom ARM boot stub is now included with
 Ultibo that allows the SMC instruction to be performed.

 This custom stub contains a marker that allows the boot process to detect it and disable the
 attempt to switch back to the Secure world if the default boot stub is used instead.

 As the default boot stub continues to evolve in response to demands from the Linux community this
 technique will allow Ultibo to continue to provide the features we want without having to make
 frequent changes to combat items that are beyond our control.

 Thanks to the Circle project for introducing the custom boot stub technique to work around these
 changes.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

{$IFNDEF FPC_DOTTEDUNITS}
unit BootRPi4;
{$ENDIF FPC_DOTTEDUNITS}

interface

{==============================================================================}
{Global definitions} {Must be prior to uses}
{$INCLUDE ..\core\GlobalDefines.inc}

{$IFDEF FPC_DOTTEDUNITS}
uses
  Core.GlobalConfig,
  Core.GlobalConst,
  Core.GlobalTypes,
  Platforms.BCM2838,
  Core.Platform,
  Platforms.PlatformRPi4,
  {$IFDEF CPUARM}
  Platforms.PlatformARM,
  Platforms.PlatformARMv7,
  Platforms.PlatformARMv7l,
  {$ENDIF CPUARM}
  {$IFDEF CPUAARCH64}
  Platforms.PlatformAARCH64,
  Platforms.PlatformARMv8,
  {$ENDIF CPUAARCH64}
  Core.Threads,
  {$IFDEF CONSOLE_EARLY_INIT}
  Core.Devices,
  Core.Framebuffer,
  Core.Console,
  {$ENDIF}
  {$IFDEF LOGGING_EARLY_INIT}
  Core.Logging,
  {$ENDIF}
  System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  BCM2838,
  Platform,
  PlatformRPi4,
  {$IFDEF CPUARM}
  PlatformARM,
  PlatformARMv7,
  PlatformARMv7L,
  {$ENDIF CPUARM}
  {$IFDEF CPUAARCH64}
  PlatformAARCH64,
  PlatformARMv8,
  {$ENDIF CPUAARCH64}
  Threads,
  {$IFDEF CONSOLE_EARLY_INIT}
  Devices,
  Framebuffer,
  Console,
  {$ENDIF}
  {$IFDEF LOGGING_EARLY_INIT}
  Logging,
  {$ENDIF}
  SysUtils;
{$ENDIF FPC_DOTTEDUNITS}

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
procedure Startup; assembler; nostackframe; public name '_START';
{Entry point of Ultibo on Raspberry Pi 4, this will be the very first byte executed
 and will be loaded by the GPU at address 0x00008000 (or 0x00080000 in 64-bit mode)}
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

 //Check ARM stub for Secure Boot support
 mov r0, #RPI4_SECURE_BOOT_OFFSET
 ldr r0, [r0]

 //Check for "RPIX" marker
 ldr r3, =RPI4_SECURE_BOOT_MARKER
 cmp r0, r3
 beq StartupHandler

 //Check for "FIQS" alternate marker
 ldr r3, =RPI4_SECURE_BOOT_CIRCLE
 cmp r0, r3
 beq StartupHandler

 //Disable Secure Boot
 ldr r3, .LARMSecureBoot
 mov r0, #0
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
.LARMSecureBoot:
  .long ARMSecureBoot
end;
{$ENDIF CPUARM}
{$IFDEF CPUAARCH64}
asm
 //To Do
end;
{$ENDIF CPUAARCH64}

{==============================================================================}

procedure Vectors; assembler; nostackframe;
{ARM exception vector table which is copied to the vector base address by the
 StartupHandler. See A2.6 "Exceptions" of the ARM Architecture Reference Manual}
{$IFDEF CPUARM}
asm
 ldr pc, .Lreset_addr     //Reset Handler
 ldr pc, .Lundef_addr      //Undefined Instruction Handler
 ldr pc, .Lswi_addr          //Software Interrupt Handler
 ldr pc, .Lprefetch_addr  //Prefetch Abort Handler
 ldr pc, .Labort_addr      //Data Abort Handler
 ldr pc, .Lreserved_addr  //Reserved Handler
 ldr pc, .Lirq_addr          //IRQ (Interrupt Request) Handler
 ldr pc, .Lfiq_addr          //FIQ (Fast Interrupt Request) Handler

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

procedure SecureVectors; assembler; nostackframe;
{ARM secure vector table which is copied to the sector vector base address by the
 StartupHandler. See A2.6 "Exceptions" of the ARM Architecture Reference Manual}
{$IFDEF CPUARM}
asm
 ldr pc, .Lreset_addr     //Reset Handler
 ldr pc, .Lreset_addr      //Undefined Instruction Handler
 ldr pc, .Lsmc_addr          //Secure Monitor Handler
 ldr pc, .Lreset_addr     //Prefetch Abort Handler
 ldr pc, .Lreset_addr      //Data Abort Handler
 ldr pc, .Lreset_addr     //Reserved Handler
 ldr pc, .Lreset_addr      //IRQ (Interrupt Request) Handler
 ldr pc, .Lreset_addr      //FIQ (Fast Interrupt Request) Handler

.Lreset_addr:
  .long ARMv7ResetHandler
.Lsmc_addr:
  .long SecureMonitor
end;
{$ENDIF CPUARM}
{$IFDEF CPUAARCH64}
asm
 //To Do
end;
{$ENDIF CPUAARCH64}

{==============================================================================}

procedure SecureMonitor; assembler; nostackframe;
{Secure monitor mode handler to switch to secure mode}
{$IFDEF CPUARM}
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
{$ENDIF CPUARM}
{$IFDEF CPUAARCH64}
asm
 //To Do
end;
{$ENDIF CPUAARCH64}

{==============================================================================}

procedure StartupSwitch; assembler; nostackframe;
{Startup handler routine to switch from hypervisor mode}
{$IFDEF CPUARM}
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
 ldr r3, .LRPi4CNTVOFFLow
 str r1, [r3]
 ldr r3, .LRPi4CNTVOFFHigh
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

.LRPi4CNTVOFFLow:
  .long RPi4CNTVOFFLow
.LRPi4CNTVOFFHigh:
  .long RPi4CNTVOFFHigh
end;
{$ENDIF CPUARM}
{$IFDEF CPUAARCH64}
asm
 //To Do
end;
{$ENDIF CPUAARCH64}

{==============================================================================}

procedure StartupSecure; assembler; nostackframe;
{Startup handler routine to switch to secure mode}
{$IFDEF CPUARM}
asm
 //Check the secure boot configuration
 ldr r0, .LARMSecureBoot
 ldr r0, [r0]
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

.LARMSecureBoot:
  .long ARMSecureBoot
.LSecureVectors:
  .long SecureVectors
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
 //Call the HYP mode switch handler in case the CPU is in HYP mode
 bl StartupSwitch

 //Call the secure mode switch handler to return to secure mode
 bl StartupSecure

 //Invalidate all Caches before starting the boot process
 bl ARMv7InvalidateCache

 //Flush the Branch Target Cache before starting the boot process
 bl ARMv7FlushBranchTargetCache

 //Invalidate the TLB before starting the boot process
 bl ARMv7InvalidateTLB

 //Change to SYS mode and ensure all interrupts are disabled
 //so the ARM processor is in a known state.
 cpsid if, #ARM_MODE_SYS

 //Copy the ARM exception table from Vectors to the vector base address.
 mov r0, #RPI4_VECTOR_TABLE_BASE
 ldr r1, .L_vectors
 ldmia r1!, {r2-r9}
 stmia r0!, {r2-r9}
 ldmia r1!, {r2-r9}
 stmia r0!, {r2-r9}

 //Set the Vector Base Address register in the System Control
 //register to the address of the vector table base above.
 mov r0, #RPI4_VECTOR_TABLE_BASE
 mcr p15, #0, r0, cr12, cr0, #0

 //Enable Unaligned Memory Accesses (U Bit) in the System Control
 //Register to simplify memory access routines from Pascal code.
 //
 //This would normally occur in CPUInit but is done here to allow
 //calls to Pascal code during initialization. (Always enabled in ARMv8)
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
 cmp r0, r1         //Check if we have reached bss_end yet.
 bcc .Lbssloopb        //Repeat the loop if still more to go.

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

 //Determine how many third level page tables are required.
 //Each one represents 2MB and is 4KB in size (512 8 byte entries).
 //R1 will contain the address of the bss_end from above.
 //Add the initial stack size to bss_end
 ldr r0, .LINITIAL_STACK_SIZE
 ldr r0, [r0]
 add r3, r1, r0
 //Add the initial heap size to R3
 ldr r0, .LINITIAL_HEAP_SIZE
 ldr r0, [r0]
 add r3, r3, r0
 //Divide R3 by 2MB to get the count.
 lsr r2, r3, #21
 //Add one for any leftover amount and one for safety.
 add r2, r2, #2
 //Store the third level page table used count
 ldr r0, .LPAGE_TABLES_USED
 str r2, [r0]

 //Get the third level page table free count
 ldr r0, .LPAGE_TABLES_FREE
 ldr r0, [r0]
 //Add the free count to the used count
 add r2, r2, r0
 //Store the third level page table count
 ldr r0, .LPAGE_TABLES_COUNT
 str r2, [r0]

 //Put the third level page tables directly after the kernel image.
 //R1 will contain the address of the bss_end from above.
 //Store the start address of the third level page tables.
 ldr r0, .LPAGE_TABLES_ADDRESS
 str r1, [r0]
 //Multiply count by 4KB to get the size.
 //R2 will contain the number of page tables from above.
 lsl r2, r2, #12
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
 //Store the size of the third level page tables.
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

 //Initialize the RPi4 Platform specific behaviour (Memory, Peripherals, Interrupts etc).
 bl RPi4Init

 //Initialize the ARM Platform specific behaviour (IRQ, FIQ, Abort etc).
 bl ARMInit

 //Initialize the ARMv7L Platform specific behaviour (Halt, Pause, Locks, Barriers, ContextSwitch, ThreadStack etc).
 {$IFDEF RPI4_ENABLE_LPAE}
 bl ARMv7LInit
 {$ELSE RPI4_ENABLE_LPAE}
 bl ARMv7Init
 {$ENDIF RPI4_ENABLE_LPAE}

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
