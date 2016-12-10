{
Ultibo Definitions specific to the ARM Versatile Platform Baseboard.

Copyright (C) 2016 - SoftOz Pty Ltd.

Arch
====

 ARMv7 (Cortex-A8)

Boards
======

 QEMU - VersatilePB 
 
Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)
 
Credits
=======

 Information for this unit was obtained from:

  QEMU - \hw\arm\versatilepb.c - Copyright (c) 2005-2007 CodeSourcery
  
  Linux - \drivers\clocksource\timer-sp804.c - Copyright (C) 1999 - 2003 ARM Limited
  
  Linux - \drivers\irqchip\irq-vic.c - Copyright (C) 1999 - 2003 ARM Limited
  
  Linux - \arch\arm\mach-versatile\include\mach\platform.h - Copyright (c) ARM Limited 2003
  
References
==========

 QEMU System ARM - http://wiki.qemu.org/download/qemu-doc.html#ARM-System-emulator

 RealView Versatile PB - http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.dui0224i/index.html
 
 SP804 Dual Timer - http://infocenter.arm.com/help/topic/com.arm.doc.ddi0271d/DDI0271.pdf
 
 PL190 Vectored Interrupt Controller - http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0181e/index.html
 
ARM VersatilePB
===============

 From the QEMU source the memory map of the VersatilePB is shown as this:
 
  Memory map for Versatile/PB: 
  0x10000000 System registers
  0x10001000 PCI controller config registers
  0x10002000 Serial bus interface
  0x10003000 Secondary interrupt controller
  0x10004000 AACI (audio)
  0x10005000 MMCI0
  0x10006000 KMI0 (keyboard)
  0x10007000 KMI1 (mouse)
  0x10008000 Character LCD Interface
  0x10009000 UART3
  0x1000a000 Smart card 1
  0x1000b000 MMCI1
  0x10010000 Ethernet
  0x10020000 USB 
  0x10100000 SSMC 
  0x10110000 MPMC
  0x10120000 CLCD Controller
  0x10130000 DMA Controller
  0x10140000 Vectored interrupt controller
  0x101d0000 AHB Monitor Interface
  0x101e0000 System Controller 
  0x101e1000 Watchdog Interface
  0x101e2000 Timer 0/1 
  0x101e3000 Timer 2/3
  0x101e4000 GPIO port 0 
  0x101e5000 GPIO port 1 
  0x101e6000 GPIO port 2 
  0x101e7000 GPIO port 3
  0x101e8000 RTC
  0x101f0000 Smart card 0
  0x101f1000 UART0
  0x101f2000 UART1
  0x101f3000 UART2
  0x101f4000 SSPI
  0x34000000 NOR Flash

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit VersatilePB; 
                               
interface

uses GlobalConfig,GlobalConst,GlobalTypes;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}
 
{==============================================================================}
const
 {VersatilePB specific constants}
 VERSATILEPB_CPU_COUNT = 1;
 
 {Physical memory addresses of VersatilePB peripherals (See: \hw\arm\versatilepb.c and http://infocenter.arm.com/help/topic/com.arm.doc.dui0224i/Bbajihec.html)}
 {See also: \arch\arm\mach-versatile\include\mach\platform.h}
 VERSATILEPB_PERIPHERALS_BASE = $10000000;
 VERSATILEPB_PERIPHERALS_SIZE = $001FFFFF;

 {Peripherals}
 VERSATILEPB_SYS_REGS_BASE      = $10000000; {System Registers}
 VERSATILEPB_PCI_CORE_REGS_BASE = $10001000; {PCI core control}
 VERSATILEPB_I2C_REGS_BASE      = $10002000; {I2C control}
 VERSATILEPB_SIC_REGS_BASE      = $10003000; {Secondary interrupt controller}
 VERSATILEPB_AACI_REGS_BASE     = $10004000; {PL041 Audio}
 VERSATILEPB_MMCI0_REGS_BASE    = $10005000; {Pl180 MMC interface}
 VERSATILEPB_KMI0_REGS_BASE     = $10006000; {PL050 KMI interface}
 VERSATILEPB_KMI1_REGS_BASE     = $10007000; {PL050 KMI 2nd interface}
 VERSATILEPB_CHAR_LCD_REGS_BASE = $10008000; {Character LCD}
 VERSATILEPB_UART3_REGS_BASE    = $10009000; {PL011 UART 3}
 VERSATILEPB_SCI1_REGS_BASE     = $1000A000;
 VERSATILEPB_MMCI1_REGS_BASE    = $1000B000; {Pl180 MMC Interface}
 {0x1000C000 - 0x1000CFFF = reserved}
 VERSATILEPB_ETH_REGS_BASE      = $10010000; {LAN91C111 Ethernet}
 VERSATILEPB_USB_REGS_BASE      = $10020000; {OHCI USB}
 {0x10030000 - 0x100FFFFF = reserved}
 VERSATILEPB_SMC_REGS_BASE      = $10100000; {SMC}
 VERSATILEPB_MPMC_REGS_BASE     = $10110000; {MPMC}
 VERSATILEPB_CLCD_REGS_BASE     = $10120000; {PL110 CLCD}
 VERSATILEPB_DMAC_REGS_BASE     = $10130000; {PL080 DMA controller}
 VERSATILEPB_VIC_REGS_BASE      = $10140000; {PL190Vectored interrupt controller}
 
 VERSATILEPB_AHBM_REGS_BASE     = $101D0000; {AHB monitor}
 VERSATILEPB_SYSCTRL_REGS_BASE  = $101E0000; {System controller}
 VERSATILEPB_WATCHDOG_REGS_BASE = $101E1000; {Watchdog}
 VERSATILEPB_TIMER0_REGS_BASE   = $101e2000; {SP804 Timer 0}
 VERSATILEPB_TIMER1_REGS_BASE   = $101e2020; {SP804 Timer 1}
 VERSATILEPB_TIMER2_REGS_BASE   = $101e3000; {SP804 Timer 2}
 VERSATILEPB_TIMER3_REGS_BASE   = $101e3020; {SP804 Timer 3}
 VERSATILEPB_GPIO0_REGS_BASE    = $101E4000; {PL061 GPIO port 0}
 VERSATILEPB_GPIO1_REGS_BASE    = $101E5000; {PL061 GPIO port 1}
 VERSATILEPB_GPIO2_REGS_BASE    = $101E6000; {PL061 GPIO port 2}
 VERSATILEPB_GPIO3_REGS_BASE    = $101E7000; {PL061 GPIO port 3}
 VERSATILEPB_RTC_REGS_BASE      = $101E8000; {PL031 Real Time Clock}
 {0x101E9000 - reserved}
 VERSATILEPB_SCI_REGS_BASE      = $101F0000; {Smart card controller}
 VERSATILEPB_UART0_REGS_BASE    = $101f1000; {PL011 UART0}
 VERSATILEPB_UART1_REGS_BASE    = $101f2000; {PL011 UART1}
 VERSATILEPB_UART2_REGS_BASE    = $101f3000; {PL011 UART2}
 VERSATILEPB_SSP_REGS_BASE      = $101F4000; {Synchronous Serial Port}
 
const
 {IRQ lines of VersatilePB peripherals (See: \hw\arm\versatilepb.c and http://infocenter.arm.com/help/topic/com.arm.doc.dui0224i/Bbajihec.html)}
 {Interrupt Assignments (Primary)(See: \arch\arm\mach-versatile\include\mach\platform.h)}
 VERSATILEPB_IRQ_WDOG                    = 0;  {Watchdog timer}
 VERSATILEPB_IRQ_SOFT                    = 1;  {Software interrupt}
 VERSATILEPB_IRQ_COMMRX                  = 2;  {Debug Comm Rx interrupt}
 VERSATILEPB_IRQ_COMMTX                  = 3;  {Debug Comm Tx interrupt}
 VERSATILEPB_IRQ_TIMER0_1                = 4;  {Timer 0 and 1}
 VERSATILEPB_IRQ_TIMER2_3                = 5;  {Timer 2 and 3}
 VERSATILEPB_IRQ_GPIO0                   = 6;  {GPIO 0}
 VERSATILEPB_IRQ_GPIO1                   = 7;  {GPIO 1}
 VERSATILEPB_IRQ_GPIO2                   = 8;  {GPIO 2}
 VERSATILEPB_IRQ_GPIO3                   = 9;  {GPIO 3}
 VERSATILEPB_IRQ_RTC                     = 10; {Real Time Clock}
 VERSATILEPB_IRQ_SSP                     = 11; {Synchronous Serial Port}
 VERSATILEPB_IRQ_UART0                   = 12; {UART 0 on development chip}
 VERSATILEPB_IRQ_UART1                   = 13; {UART 1 on development chip}
 VERSATILEPB_IRQ_UART2                   = 14; {UART 2 on development chip}
 VERSATILEPB_IRQ_SCI                     = 15; {Smart Card Interface}
 VERSATILEPB_IRQ_CLCD                    = 16; {CLCD controller}
 VERSATILEPB_IRQ_DMA                     = 17; {DMA controller}
 VERSATILEPB_IRQ_PWRFAIL                 = 18; {Power failure}
 VERSATILEPB_IRQ_MBX                     = 19; {Graphics processor}
 VERSATILEPB_IRQ_GND                     = 20; {Reserved}
 {External interrupt signals from logic tiles or secondary controller}
 VERSATILEPB_IRQ_VICSOURCE21             = 21; {Disk on Chip}
 VERSATILEPB_IRQ_VICSOURCE22             = 22; {MCI0A}
 VERSATILEPB_IRQ_VICSOURCE23             = 23; {MCI1A}
 VERSATILEPB_IRQ_VICSOURCE24             = 24; {AACI}
 VERSATILEPB_IRQ_VICSOURCE25             = 25; {Ethernet}
 VERSATILEPB_IRQ_VICSOURCE26             = 26; {USB}
 VERSATILEPB_IRQ_VICSOURCE27             = 27; {PCI 0}
 VERSATILEPB_IRQ_VICSOURCE28             = 28; {PCI 1}
 VERSATILEPB_IRQ_VICSOURCE29             = 29; {PCI 2}
 VERSATILEPB_IRQ_VICSOURCE30             = 30; {PCI 3}
 VERSATILEPB_IRQ_VICSOURCE31             = 31; {SIC source}
 
 {Interrupt Assignments (Secondary)(See: \arch\arm\mach-versatile\include\mach\platform.h)}
 VERSATILEPB_IRQ_SIC_MMCI0B              = 1;  {Multimedia Card 0B}
 VERSATILEPB_IRQ_SIC_MMCI1B              = 2;  {Multimedia Card 1B}
 VERSATILEPB_IRQ_SIC_KMI0                = 3;  {Keyboard/Mouse port 0}
 VERSATILEPB_IRQ_SIC_KMI1                = 4;  {Keyboard/Mouse port 1}
 VERSATILEPB_IRQ_SIC_SCI3                = 5;  {Smart Card interface}
 VERSATILEPB_IRQ_SIC_UART3               = 6;  {UART 3 empty or data available}
 VERSATILEPB_IRQ_SIC_CHAR_LCD            = 7;  {Character LCD}
 VERSATILEPB_IRQ_SIC_TOUCH               = 8;  {Touchscreen}
 VERSATILEPB_IRQ_SIC_KEYPAD              = 9;  {Key pressed on display keypad}
 {10:20 - reserved}
 VERSATILEPB_IRQ_SIC_DoC                 = 21; {Disk on Chip memory controller}
 VERSATILEPB_IRQ_SIC_MMCI0A              = 22; {MMC 0A}
 VERSATILEPB_IRQ_SIC_MMCI1A              = 23; {MMC 1A}
 VERSATILEPB_IRQ_SIC_AACI                = 24; {Audio Codec}
 VERSATILEPB_IRQ_SIC_ETH                 = 25; {Ethernet controller}
 VERSATILEPB_IRQ_SIC_USB                 = 26; {USB controller}
 VERSATILEPB_IRQ_SIC_PCI0                = 27;
 VERSATILEPB_IRQ_SIC_PCI1                = 28;
 VERSATILEPB_IRQ_SIC_PCI2                = 29;
 VERSATILEPB_IRQ_SIC_PCI3                = 30;
 
 {Number of IRQs on the Vectored (Primary) Interrupt Controller (VIC)}
 VERSATILEPB_VIC_IRQ_COUNT = 32;

 {Number of IRQs on the Secondary Interrupt Controller (SIC)}
 VERSATILEPB_SIC_IRQ_COUNT = 32;
 
 {Total number of IRQs available}
 VERSATILEPB_IRQ_COUNT = VERSATILEPB_VIC_IRQ_COUNT + VERSATILEPB_SIC_IRQ_COUNT; {64}

 {Total number of FIQs available}
 VERSATILEPB_FIQ_COUNT = 1;
 
const
 {Timer frequencies}
 VERSATILEPB_TIMER_FREQUENCY = 1000000; {Default frequency of the VersatilePB Timer when reference is set to TIMCLK (1MHz)}
 
const
 {System register offsets (See: \arm\mach-versatile\include\mach\platform.h and http://infocenter.arm.com/help/topic/com.arm.doc.dui0224i/I1006122.html)}
 VERSATILEPB_SYS_ID               = VERSATILEPB_SYS_REGS_BASE + $00;
 VERSATILEPB_SYS_SW               = VERSATILEPB_SYS_REGS_BASE + $04;
 VERSATILEPB_SYS_LED              = VERSATILEPB_SYS_REGS_BASE + $08;
 VERSATILEPB_SYS_OSC0             = VERSATILEPB_SYS_REGS_BASE + $0C;
 VERSATILEPB_SYS_OSC1             = VERSATILEPB_SYS_REGS_BASE + $10;
 VERSATILEPB_SYS_OSC2             = VERSATILEPB_SYS_REGS_BASE + $14;
 VERSATILEPB_SYS_OSC3             = VERSATILEPB_SYS_REGS_BASE + $18;
 VERSATILEPB_SYS_OSC4             = VERSATILEPB_SYS_REGS_BASE + $1C;
 VERSATILEPB_SYS_OSCCLCD          = VERSATILEPB_SYS_REGS_BASE + $1c;
 VERSATILEPB_SYS_LOCK             = VERSATILEPB_SYS_REGS_BASE + $20;
 VERSATILEPB_SYS_100HZ            = VERSATILEPB_SYS_REGS_BASE + $24;
 VERSATILEPB_SYS_CFGDATA1         = VERSATILEPB_SYS_REGS_BASE + $28;
 VERSATILEPB_SYS_CFGDATA2         = VERSATILEPB_SYS_REGS_BASE + $2C;
 VERSATILEPB_SYS_FLAGS            = VERSATILEPB_SYS_REGS_BASE + $30;
 VERSATILEPB_SYS_FLAGSSET         = VERSATILEPB_SYS_REGS_BASE + $30;
 VERSATILEPB_SYS_FLAGSCLR         = VERSATILEPB_SYS_REGS_BASE + $34;
 VERSATILEPB_SYS_NVFLAGS          = VERSATILEPB_SYS_REGS_BASE + $38;
 VERSATILEPB_SYS_NVFLAGSSET       = VERSATILEPB_SYS_REGS_BASE + $38;
 VERSATILEPB_SYS_NVFLAGSCLR       = VERSATILEPB_SYS_REGS_BASE + $3C;
 VERSATILEPB_SYS_RESETCTL         = VERSATILEPB_SYS_REGS_BASE + $40;
 VERSATILEPB_SYS_PCICTL           = VERSATILEPB_SYS_REGS_BASE + $44;
 VERSATILEPB_SYS_MCI              = VERSATILEPB_SYS_REGS_BASE + $48;
 VERSATILEPB_SYS_FLASH            = VERSATILEPB_SYS_REGS_BASE + $4C;
 VERSATILEPB_SYS_CLCD             = VERSATILEPB_SYS_REGS_BASE + $50;
 VERSATILEPB_SYS_CLCDSER          = VERSATILEPB_SYS_REGS_BASE + $54;
 VERSATILEPB_SYS_BOOTCS           = VERSATILEPB_SYS_REGS_BASE + $58;
 VERSATILEPB_SYS_24MHZ            = VERSATILEPB_SYS_REGS_BASE + $5C;
 VERSATILEPB_SYS_MISC             = VERSATILEPB_SYS_REGS_BASE + $60;
 VERSATILEPB_SYS_TEST_OSC0        = VERSATILEPB_SYS_REGS_BASE + $80;
 VERSATILEPB_SYS_TEST_OSC1        = VERSATILEPB_SYS_REGS_BASE + $84;
 VERSATILEPB_SYS_TEST_OSC2        = VERSATILEPB_SYS_REGS_BASE + $88;
 VERSATILEPB_SYS_TEST_OSC3        = VERSATILEPB_SYS_REGS_BASE + $8C;
 VERSATILEPB_SYS_TEST_OSC4        = VERSATILEPB_SYS_REGS_BASE + $90;
 
 {System register bits (See: \arm\mach-versatile\include\mach\platform.h and http://infocenter.arm.com/help/topic/com.arm.doc.dui0224i/I1006122.html)}
 VERSATILEPB_SYS_CLCD_MODE888    = 0; {LCD Mode [1:0], controls mapping of video memory to RGB signals 00 = 8:8:8}
 VERSATILEPB_SYS_CLCD_MODE5551   = 1; {                                                                01 = 5:5:5:1}
 VERSATILEPB_SYS_CLCD_MODE565BGR = 2; {                                                                10 = 5:6:5, red LSB}
 VERSATILEPB_SYS_CLCD_MODE565RGB = 3; {                                                                11 = 5:6:5, blue LSB}
 
 VERSATILEPB_SYS_CLCD_MODEMASK   = 3;
 
const
 {System Control register bits (See: http://infocenter.arm.com/help/topic/com.arm.doc.dui0440b/CACIHEAD.html}
 VERSATILEPB_SYSCTRL_REFCLK = 0; {REFCLK is 32.768kHz}
 VERSATILEPB_SYSCTRL_TIMCLK = 1; {TIMCLK is 1MHz}

 VERSATILEPB_SYSCTRL_TIMER0_ENSEL = 15; {Timer 0 enable / Timer Reference Select. If 0, the timing reference is REFCLK. If 1, the timing reference is TIMCLK}
 VERSATILEPB_SYSCTRL_TIMER1_ENSEL = 17; {Timer 1 enable / Timer Reference Select. If 0, the timing reference is REFCLK. If 1, the timing reference is TIMCLK}
 VERSATILEPB_SYSCTRL_TIMER2_ENSEL = 19; {Timer 2 enable / Timer Reference Select. If 0, the timing reference is REFCLK. If 1, the timing reference is TIMCLK}
 VERSATILEPB_SYSCTRL_TIMER3_ENSEL = 21; {Timer 3 enable / Timer Reference Select. If 0, the timing reference is REFCLK. If 1, the timing reference is TIMCLK}
 
const
 {SP804 Timer register offsets (See: http://infocenter.arm.com/help/topic/com.arm.doc.ddi0271d/DDI0271.pdf)}
 SP804_TIMER_LOAD            = $00; {Timer Load register}
 SP804_TIMER_VALUE           = $04; {Timer Value register}
 SP804_TIMER_CONTROL         = $08; {Timer control register}
 SP804_TIMER_INT_CLEAR       = $0c; {Timer Interrupt clear register}
 SP804_TIMER_RAW_INT         = $10; {Timer Raw Interrupt register}
 SP804_TIMER_MASKED_INT      = $14; {Timer Masked Interrupt register}
 SP804_TIMER_BACKGROUND_LOAD = $18; {Timer Background Load register}
 
 {SP804 Timer register bits (See: http://infocenter.arm.com/help/topic/com.arm.doc.ddi0271d/DDI0271.pdf)}
 SP804_TIMER_CONTROL_ONESHOT       = (1 shl 0); {Selects one-shot or wrapping counter mode: (0 = wrapping mode (default) / 1 = one-shot mode)}
 SP804_TIMER_CONTROL_32BIT         = (1 shl 1); {Selects 16/32 bit counter operation: (0 = 16-bit counter (default) / 1 = 32-bit counter)}
 SP804_TIMER_CONTROL_PRESCALE1     = (0 shl 2); {Prescale bits: 00 = 0 stages of prescale, clock is divided by 1 (default)}
 SP804_TIMER_CONTROL_PRESCALE16    = (1 shl 2); {               01 = 4 stages of prescale, clock is divided by 16}
 SP804_TIMER_CONTROL_PRESCALE256   = (2 shl 2); {               10 = 8 stages of prescale, clock is divided by 256}
 SP804_TIMER_CONTROL_INT_ENABLED   = (1 shl 5); {Interrupt Enable bit: (0 = Timer module Interrupt disabled / 1 = Timer module Interrupt enabled (default))}
 SP804_TIMER_CONTROL_PERIODIC      = (1 shl 6); {Mode bit: (0 = Timer module is in free-running mode (default) / 1 = Timer module is in periodic mode)}
 SP804_TIMER_CONTROL_TIMER_ENABLED = (1 shl 7); {Enable bit: (0 = Timer module disabled (default) / 1 = Timer module enabled)}
 
 {PL190 Vectored Interrupt Controller register offsets (See: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0181e/index.html)}
 PL190_VIC_IRQSTATUS    = $00;  {IRQ Status Register}
 PL190_VIC_FIQSTATUS    = $04;  {FIQ Status Register}
 PL190_VIC_RAWINTR      = $08;  {Raw Interrupt Status Register}
 PL190_VIC_INTSELECT    = $0c;  {Interrupt Select Register (1 = FIQ, 0 = IRQ)}
 PL190_VIC_INTENABLE    = $10;  {Interrupt Enable Register}
 PL190_VIC_INTENCLEAR   = $14;  {Interrupt Enable Clear Register}
 PL190_VIC_SOFTINT      = $18;  {Software Interrupt Register}
 PL190_VIC_SOFTINTCLEAR = $1c;  {Software Interrupt Clear Register}
 PL190_VIC_PROTECTION   = $20;  {Protection Enable Register}
 PL190_VIC_VECTADDR     = $30;  {Vector Address Register (PL190 only)}
 PL190_VIC_DEFVECTADDR  = $34;  {Default Vector Address Register (PL190 only)}
 PL190_VIC_VECTADDR0    = $100; {Vector Address Registers (0 to 15) (0..31 PL192)}
 PL190_VIC_VECTCNTL0    = $200; {Vector Control Registers (0 to 15) (0..31 PL192)}
 
 {PL190 Vectored Interrupt Controller register bits (See: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0181e/index.html)}
 PL190_VIC_VECTCNTL_ENABLE    = (1 shl 5);   {Enables vector interrupt (This bit is cleared on reset)}
 PL190_VIC_VECTCNTL_INTSOURCE = ($1F shl 0); {Selects interrupt source 0 to 31}
 
const
 {VersatilePB Secondary Interrupt Consoller register offsets (See: http://infocenter.arm.com/help/topic/com.arm.doc.dui0224i/Cacdggia.html)}
 VERSATILEPB_SIC_STATUS     = $00; {Status of interrupt (after mask) (Read)}
 VERSATILEPB_SIC_RAWSTAT    = $04; {Status of interrupt (before mask) (Read)}
 VERSATILEPB_SIC_ENABLE     = $08; {Interrupt mask / Set bits HIGH to enable the corresponding interrupt signals (Read/Write)}
 VERSATILEPB_SIC_ENSET      = $08; 
 VERSATILEPB_SIC_ENCLR      = $0C; {Set bits HIGH to mask the corresponding interrupt signals (Write)}
 VERSATILEPB_SIC_SOFTINTSET = $10; {Set software interrupt (Read/Write)}
 VERSATILEPB_SIC_SOFTINTCLR = $14; {Clear software interrupt (Write)}
 VERSATILEPB_SIC_PICENABLE  = $20; {Pass-through mask (allows interrupt to pass directly to the primary interrupt controller) / Set bits HIGH to set the corresponding interrupt pass-through mask bits (Read/Write)}
 VERSATILEPB_SIC_PICENSET   = $20;   
 VERSATILEPB_SIC_PICENCLR   = $24; {Set bits HIGH to clear the corresponding interrupt pass-through mask bits (Write)}
 
 {VersatilePB Secondary Interrupt Controller register bits (See: http://infocenter.arm.com/help/topic/com.arm.doc.dui0224i/Cacdggia.html)}
 VERSATILEPB_SIC_PIC_MASK = $7FE00000; {Interrupts on secondary controller from 21 to 30 are routed directly to the VIC on the corresponding number on primary controller}
 
{==============================================================================}
type 
 {VersatilePB specific structures (See: http://infocenter.arm.com/help/topic/com.arm.doc.ddi0271d/DDI0271.pdf)}
 {Layout of the SP804 Timer registers}
 PSP804TimerRegisters = ^TSP804TimerRegisters;
 TSP804TimerRegisters = record
  Load:LongWord;           {Timer Load register}
  Value:LongWord;          {Timer Value register}
  Control:LongWord;        {Timer control register}
  IRQClear:LongWord;       {Timer IRQ clear register}
  RawIRQ:LongWord;         {Timer Raw IRQ register}
  MaskedIRQ:LongWord;      {Timer Masked IRQ register}
  BackgroundLoad:LongWord; {Timer Background Load register}
 end;
 
 {Layout of the PL190 Vectored Interrupt Controller registers (See: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0181e/index.html)}
 PPL190InterruptRegisters = ^TPL190InterruptRegisters;
 TPL190InterruptRegisters = record
  IRQSTATUS:LongWord;    {IRQ Status Register}
  FIQSTATUS:LongWord;    {FIQ Status Register}
  RAWINTR:LongWord;      {Raw Interrupt Status Register} 
  INTSELECT:LongWord;    {Interrupt Select Register}
  INTENABLE:LongWord;    {Interrupt Enable Register}
  INTENCLEAR:LongWord;   {Interrupt Enable Clear Register}
  SOFTINT:LongWord;      {Software Interrupt Register}
  SOFTINTCLEAR:LongWord; {Software Interrupt Clear Register}
  PROTECTION:LongWord;   {Protection Enable Register}
  RESERVED1:LongWord;
  RESERVED2:LongWord;
  RESERVED3:LongWord;
  VECTADDR:LongWord;     {Vector Address Register}
  DEFVECTADDR:LongWord;  {Default Vector Address Register}
 end;
 
 PPL190VectorAddressRegisters = ^TPL190VectorAddressRegisters;
 TPL190VectorAddressRegisters = record
  VECTADDR:array[0..15] of LongWord; {Vector Address Registers}
 end;

 PPL190VectorControlRegisters = ^TPL190VectorControlRegisters;
 TPL190VectorControlRegisters = record
  VECTCNTL:array[0..15] of LongWord; {Vector Control Registers}
 end;
 
 {Layout of the VersatilePB Secondary Interrupt Controller registers (See: http://infocenter.arm.com/help/topic/com.arm.doc.dui0224i/Cacdggia.html)}
 PVersatilePBInterruptRegisters = ^TVersatilePBInterruptRegisters;
 TVersatilePBInterruptRegisters = record
  SIC_STATUS:LongWord;     {Status of interrupt (after mask) (Read)}
  SIC_RAWSTAT:LongWord;    {Status of interrupt (before mask) (Read)}
  SIC_ENSET:LongWord;      {Interrupt mask / Set bits HIGH to enable the corresponding interrupt signals (Read/Write)}
  SIC_ENCLR:LongWord;      {Set bits HIGH to mask the corresponding interrupt signals (Write)}
  SIC_SOFTINTSET:LongWord; {Set software interrupt (Read/Write)}
  SIC_SOFTINTCLR:LongWord; {Clear software interrupt (Write)}
  RESERVED1:LongWord;
  RESERVED2:LongWord;
  SIC_PICENSET:LongWord;   {Pass-through mask (allows interrupt to pass directly to the primary interrupt controller) / Set bits HIGH to set the corresponding interrupt pass-through mask bits (Read/Write)}
  SIC_PICENCLR:LongWord;   {Set bits HIGH to clear the corresponding interrupt pass-through mask bits (Write)}
 end;
  
{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}

end.
 