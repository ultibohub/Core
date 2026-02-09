{
ARM Generic Interrupt Controller Driver.

Copyright (C) 2023 - SoftOz Pty Ltd.

Arch
====

 ARMv7 (All)
 ARMv8 (All)

Boards
======

 <All>

Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)

Credits
=======

 Information for this unit was obtained from:

  Linux - \drivers\irqchip\irq-gic.c - Copyright (C) 2002 ARM Limited

References
==========

 GICv2 - https://developer.arm.com/docs/ihi0048/b/arm-generic-interrupt-controller-architecture-version-20-architecture-specification

ARM Generic Interrupt Controller
================================

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

{$IFNDEF FPC_DOTTEDUNITS}
unit ARMGIC;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Core.GlobalConfig,
  Core.GlobalConst,
  Core.GlobalTypes,
  Core.Platform,
  {$IFDEF CPUARM}
  Platforms.PlatformARM,
  {$ENDIF CPUARM}
  {$IFDEF CPUAARCH64}
  Platforms.PlatformAARCH64,
  {$ENDIF CPUAARCH64}
  System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  {$IFDEF CPUARM}
  PlatformARM,
  {$ENDIF CPUARM}
  {$IFDEF CPUAARCH64}
  PlatformAARCH64,
  {$ENDIF CPUAARCH64}
  SysUtils;
{$ENDIF FPC_DOTTEDUNITS}

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {ARM GIC specific constants}
 ARM_GIC_MAX_DEVICES = 4;

 ARM_GIC_SGI_BASE = 0;    {Software Generated Interrupts from 0 to 15}
 ARM_GIC_PPI_BASE = 16;   {Private Peripheral Interrupts from 16 to 31}
 ARM_GIC_SPI_BASE = 32;   {Shared Peripheral Interrupts from 32 to 1019 (Maximum)}
 ARM_GIC_SPI_MAX  = 1019;

 ARM_GIC_SGI_COUNT = 16;  {16 Software Generated Interrupts}
 ARM_GIC_PPI_COUNT = 16;  {16 Private Peripheral Interrupts}
 ARM_GIC_SPI_COUNT = 988; {988 Shared Peripheral Interrupts (Maximum)}

 ARM_GIC_GROUP1_INTERRUPT = 1022; {Returned by GICC_IAR when a Group1 interrupt is the highest priority interrupt pending}
 ARM_GIC_SPURIOUS_INTERRUPT = 1023; {Return by GICC_IAR when there is no pending interrupt}

 {ARM GIC Distributor registers}
 ARM_GICD_CTLR        = $000; {Distributor Control Register}
 ARM_GICD_TYPER       = $004; {Interrupt Controller Type Register}
 ARM_GICD_IIDR        = $008; {Distributor Implementer Identification Register}
 ARM_GICD_IGROUPR0    = $080; {Interrupt Group Registers}
 ARM_GICD_ISENABLER0  = $100; {Interrupt Set-Enable Registers}
 ARM_GICD_ICENABLER0  = $180; {Interrupt Clear-Enable Registers}
 ARM_GICD_ISPENDR0    = $200; {Interrupt Set-Pending Registers}
 ARM_GICD_ICPENDR0    = $280; {Interrupt Clear-Pending Registers}
 ARM_GICD_ISACTIVER0  = $300; {GICv2 Interrupt Set-Active Registers}
 ARM_GICD_ICACTIVER0  = $380; {Interrupt Clear-Active Registers}
 ARM_GICD_IPRIORITYR0 = $400; {Interrupt Priority Registers}
 ARM_GICD_ITARGETSR0  = $800; {Interrupt Processor Targets Registers}
 ARM_GICD_ICFGR0      = $C00; {Interrupt Configuration Registers}
 ARM_GICD_NSACR0      = $E00; {Non-secure Access Control Registers (Optional)}
 ARM_GICD_SGIR        = $F00; {Software Generated Interrupt Register}
 ARM_GICD_CPENDSGIR0  = $F10; {SGI Clear-Pending Registers}
 ARM_GICD_SPENDSGIR0  = $F20; {SGI Set-Pending Registers}

 {ARM GIC Distributor Control Register}
 {Non secure copy (Also appropriate bits for Secure copy)}
 ARM_GICD_CTLR_DISABLE = (0 shl 0);
 ARM_GICD_CTLR_ENABLE  = (1 shl 0); {Global enable for forwarding pending interrupts from the Distributor to the CPU interfaces}
 {Secure copy}
 ARM_GICD_CTLRS_DISABLE       = (0 shl 0);
 ARM_GICD_CTLRS_ENABLE_GROUP1 = (1 shl 1); {Global enable for forwarding pending Group 1 interrupts from the Distributor to the CPU interfaces}
 ARM_GICD_CTLRS_ENABLE_GROUP0 = (1 shl 0); {Global enable for forwarding pending Group 0 interrupts from the Distributor to the CPU interfaces}

 {ARM GIC Interrupt Controller Type Register}
 ARM_GICD_TYPER_LSPI_MASK    = ($1F shl 11); {If the GIC implements the Security Extensions, the value of this field is the maximum number of implemented lockable SPIs, from 0 (0b00000) to 31 (0b11111)}
 ARM_GICD_TYPER_SECURITY_EXT = (1 shl 10);   {Indicates whether the GIC implements the Security Extensions}
 ARM_GICD_TYPER_CPUNUM_MASK  = (7 shl 5);    {Indicates the number of implemented CPU interfaces. The number of implemented CPU interfaces is one more than the value of this field}
 ARM_GICD_TYPER_IRQNUM_MASK  = ($1F shl 0);  {Indicates the maximum number of interrupts that the GIC supports. The maximum number of interrupts is 32(N+1)}

 ARM_GICD_TYPER_LSPI_SHIFT     = 11;
 ARM_GICD_TYPER_SECURITY_SHIFT = 10;
 ARM_GICD_TYPER_CPUNUM_SHIFT   = 5;
 ARM_GICD_TYPER_IRQNUM_SHIFT   = 0;

 {ARM GIC Distributor Implementer Identification Register}
 ARM_GICD_IIDR_PRODUCTID_MASK   = ($FF shl 24); {An IMPLEMENTATION DEFINED product identifier}
 ARM_GICD_IIDR_VARIANT_MASK     = ($F shl 16); {An IMPLEMENTATION DEFINED variant number}
 ARM_GICD_IIDR_REVISION_MASK    = ($F shl 12); {An IMPLEMENTATION DEFINED revision number}
 ARM_GICD_IIDR_IMPLEMENTOR_MASK = ($FFF shl 0); {Contains the JEP106 code of the company that implemented the GIC Distributor}

 ARM_GICD_IIDR_PRODUCTID_SHIFT   = 24;
 ARM_GICD_IIDR_VARIANT_SHIFT     = 16;
 ARM_GICD_IIDR_REVISION_SHIFT    = 12;
 ARM_GICD_IIDR_IMPLEMENTOR_SHIFT = 0;

 {ARM GIC Interrupt Priority Register}
 ARM_GICD_IPRIORITYR_MASK = $FF; {Each byte of the priority register holds a priority value}

 {ARM GIC Interrupt Processor Targets Register}
 ARM_GICD_ITARGETSR_MASK = $FF; {Each byte of the targets register holds a bit mask of the CPU targets}

 {ARM GIC Interrupt Configuration Register}
 ARM_GICD_ICFGR_MASK = $03;
 ARM_GICD_ICFGR_LEVEL_SENSITIVE = (0 shl 1); {Corresponding interrupt is level-sensitive}
 ARM_GICD_ICFGR_EDGE_TRIGGERED  = (1 shl 1); {Corresponding interrupt is edge-triggered}

 {ARM GIC Non-secure Access Control Register (Group 0 Interrupts)}
 ARM_GICD_NSACR_MASK = $03;
 ARM_GICD_NSACR_NONE   = $0; {No Non-secure access is permitted to fields associated with the corresponding interrupt}
 ARM_GICD_NSACR_SET    = $1; {Non-secure write access is permitted to fields associated with the corresponding interrupt in the GICD_ISPENDRn registers}
 ARM_GICD_NSACR_CLEAR  = $2; {Adds Non-secure write access permission to fields associated with the corresponding interrupt in the GICD_ICPENDRn registers}
 ARM_GICD_NSACR_TARGET = $3; {Adds Non-secure read and write access permission to fields associated with the corresponding interrupt in the GICD_ITARGETSRn registers}

 {ARM GIC Software Generated Interrupt Register}
 ARM_GICD_SGIR_CPUFILTER_MASK = ($03 shl 24); {Determines how the distributor must process the requested SGI}
 ARM_GICD_SGIR_CPUTARGET_MASK = ($FF shl 16); {When TargetList Filter = 0b00, defines the CPU interfaces to which the Distributor must forward the interrupt}
 ARM_GICD_SGIR_NSATT          = (1 shl 15);   {Specifies the required security value of the SGI}
 ARM_GICD_SGIR_INTID_MASK     = ($F shl 0);   {The Interrupt ID of the SGI to forward to the specified CPU interfaces}

 ARM_GICD_SGIR_CPUFILTER_SHIFT = 24;
 ARM_GICD_SGIR_CPUTARGET_SHIFT = 16;
 ARM_GICD_SGIR_NSATT_SHIFT     = 15;
 ARM_GICD_SGIR_INTID_SHIFT     = 0;

 ARM_GICD_SGIR_CPUFILTER_LIST = $0; {Forward the interrupt to the CPU interfaces specified in the CPUTargetList field}
 ARM_GICD_SGIR_CPUFILTER_ALL  = $1; {Forward the interrupt to all CPU interfaces except that of the processor that requested the interrupt}
 ARM_GICD_SGIR_CPUFILTER_SELF = $2; {Forward the interrupt only to the CPU interface of the processor that requested the interrupt}

 {ARM GIC SGI Clear-Pending Registers}
 ARM_GICD_CPENDSGIR_MASK = $FF;

 {ARM GIC SGI Set-Pending Registers}
 ARM_GICD_SPENDSGIR_MASK = $FF;

 {ARM GIC CPU registers}
 ARM_GICC_CTLR   = $0000; {CPU Interface Control Register}
 ARM_GICC_PMR    = $0004; {Interrupt Priority Mask Register}
 ARM_GICC_BPR    = $0008; {Binary Point Register}
 ARM_GICC_IAR    = $000C; {Interrupt Acknowledge Register}
 ARM_GICC_EOIR   = $0010; {End of Interrupt Register}
 ARM_GICC_RPR    = $0014; {Running Priority Register}
 ARM_GICC_HPPIR  = $0018; {Highest Priority Pending Interrupt Register}
 ARM_GICC_ABPR   = $001C; {Aliased Binary Point Register}
 ARM_GICC_AIAR   = $0020; {Aliased Interrupt Acknowledge Register}
 ARM_GICC_AEOIR  = $0024; {Aliased End of Interrupt Register}
 ARM_GICC_AHPPIR = $0028; {Aliased Highest Priority Pending Interrupt Register}
 ARM_GICC_APR0   = $00D0; {Active Priorities Registers}
 ARM_GICC_NSAPR0 = $00E0; {Non-secure Active Priorities Registers}
 ARM_GICC_IIDR   = $00FC; {CPU Interface Identification Register}
 ARM_GICC_DIR    = $1000; {Deactivate Interrupt Register}

 {ARM GIC CPU Interface Control Register}
 {Non secure copy (Also appropriate bits for Secure copy)}
 ARM_GICC_CTLR_DISABLE    = (0 shl 0);
 ARM_GICC_CTLR_EOI_MODE   = (1 shl 9); {}
 ARM_GICC_CTLR_IRQ_BYPASS = (1 shl 6); {}
 ARM_GICC_CTLR_FIQ_BYPASS = (1 shl 5); {}
 ARM_GICC_CTLR_ENABLE     = (1 shl 0); {Enable for the signaling of Group 1 interrupts by the CPU interface to the connected processor}
 {Secure copy}
 ARM_GICC_CTLRS_DISABLE           = (0 shl 0);
 ARM_GICC_CTLRS_EOI_MODE_NS       = (1 shl 10); {Alias of EOImodeNS from the Non-secure copy of this register}
 ARM_GICC_CTLRS_EOI_MODE          = (1 shl 9);  {Controls the behavior of accesses to GICC_EOIR and GICC_DIR register}
 ARM_GICC_CTLRS_IRQ_BYPASS_GROUP1 = (1 shl 8);  {Alias of IRQBypDisGrp1 from the Non-secure copy of this register}
 ARM_GICC_CTLRS_FIQ_BYPASS_GROUP1 = (1 shl 7);  {Alias of FIQBypDisGrp1 from the Non-secure copy of this register}
 ARM_GICC_CTLRS_IRQ_BYPASS_GROUP0 = (1 shl 6);  {When the signaling of IRQs by the CPU interface is disabled, this bit partly controls whether the bypass IRQ signal is signaled to the processor}
 ARM_GICC_CTLRS_FIQ_BYPASS_GROUP0 = (1 shl 5);  {When the signaling of FIQs by the CPU interface is disabled, this bit partly controls whether the bypass FIQ signal is signaled to the processor}
 ARM_GICC_CTLRS_CBPR              = (1 shl 4);  {Controls whether the GICC_BPR provides common control to Group 0 and Group 1 interrupts}
 ARM_GICC_CTLRS_FIQ_ENABLE        = (1 shl 3);  {Controls whether the CPU interface signals Group 0 interrupts to a target processor using the FIQ or the IRQ signal}
 ARM_GICC_CTLRS_ACKCTL            = (1 shl 2);  {Controls behaviour when the highest priority pending interrupt is a Group 1 interrupt}
 ARM_GICC_CTLRS_ENABLE_GROUP1     = (1 shl 1);  {Enable for the signaling of Group 1 interrupts by the CPU interface to the connected processor}
 ARM_GICC_CTLRS_ENABLE_GROUP0     = (1 shl 0);  {Enable for the signaling of Group 0 interrupts by the CPU interface to the connected processor}

 {ARM GIC Interrupt Priority Mask Register}
 ARM_GICC_PMR_PRIORITY = $F0; {The priority mask level for the CPU interface. If the priority of an interrupt is higher than the value indicated by this field, the interface signals the interrupt to the processor}

 {ARM GIC Binary Point Register}
 ARM_GICC_BPR_BINARYPOINT_MASK = (7 shl 0); {The value of this field controls how the 8-bit interrupt priority field is split into a group priority field, used to determine interrupt preemption, and a subpriority field}

 {ARM GIC Interrupt Acknowledge Register}
 ARM_GICC_IAR_CPUID_MASK       = $07;  {For SGIs in a multiprocessor implementation, this field identifies the processor that requested the interrupt}
 ARM_GICC_IAR_INTERRUPTID_MASK = $3FF; {A read of the GICC_IAR returns the interrupt ID of the highest priority pending interrupt for the CPU interface
                                        The read returns a spurious interrupt ID of 1023 if any of the following apply
                                        - forwarding of interrupts by the Distributor to the CPU interface is disabled
                                        - signaling of interrupts by the CPU interface to the connected processor is disabled
                                        - no pending interrupt on the CPU interface has sufficient priority for the interface to signal it to the processor}

 ARM_GICC_IAR_CPUID_SHIFT = 10;
 ARM_GICC_IAR_INTERRUPTID_SHIFT = 0;

 {ARM GIC End of Interrupt Register}
 ARM_GICC_EOIR_CPUID_MASK    = $07;  {On a multiprocessor implementation, if the write refers to an SGI, this field contains the CPUID value from the corresponding GICC_IAR access}
 ARM_GICC_EOIR_EOIINTID_MASK = $3FF; {The Interrupt ID value from the corresponding GICC_IAR access}

 ARM_GICC_EOIR_CPUID_SHIFT = 10;
 ARM_GICC_EOIR_EOIINTID_SHIFT = 0;

 {ARM GIC Running Priority Register}
 ARM_GICC_RPR_PRIORITY_MASK = $FF; {The current running priority on the CPU interface}

 {ARM GIC Highest Priority Pending Interrupt Register}
 ARM_GICC_HPPIR_CPUID_MASK    = $07;   {On a multiprocessor implementation, if the PENDINTID field returns the ID of an SGI, this field contains the CPUID value for that interrupt}
 ARM_GICC_HPPIR_PENDINTID_MASK = $3FF; {The interrupt ID of the highest priority pending interrupt}

 ARM_GICC_HPPIR_CPUID_SHIFT = 10;
 ARM_GICC_HPPIR_PENDINTID_SHIFT = 0;

 {ARM GIC Aliased Binary Point Register}
 ARM_GICC_ABPR_BINARYPOINT_MASK = ARM_GICC_BPR_BINARYPOINT_MASK; {A Binary Point Register for handling Group 1 interrupts}

 {ARM GIC Aliased Interrupt Acknowledge Register}
 ARM_GICC_AIAR_CPUID_MASK       = ARM_GICC_IAR_CPUID_MASK; {An Interrupt Acknowledge register for handling Group 1 interrupts}
 ARM_GICC_AIAR_INTERRUPTID_MASK = ARM_GICC_IAR_INTERRUPTID_MASK;

 ARM_GICC_AIAR_CPUID_SHIFT = ARM_GICC_IAR_CPUID_SHIFT;
 ARM_GICC_AIAR_INTERRUPTID_SHIFT = ARM_GICC_IAR_INTERRUPTID_SHIFT;

 {ARM GIC Aliased End of Interrupt Register}
 ARM_GICC_AEOIR_CPUID_MASK    = ARM_GICC_EOIR_CPUID_MASK; {An end of interrupt register for handling Group 1 interrupts}
 ARM_GICC_AEOIR_EOIINTID_MASK = ARM_GICC_EOIR_EOIINTID_MASK;

 ARM_GICC_AEOIR_CPUID_SHIFT = ARM_GICC_EOIR_CPUID_SHIFT;
 ARM_GICC_AEOIR_EOIINTID_SHIFT = ARM_GICC_EOIR_EOIINTID_SHIFT;

 {ARM GIC Aliased Highest Priority Pending Interrupt Register}
 ARM_GICC_AHPPIR_CPUID_MASK    = ARM_GICC_HPPIR_CPUID_MASK; {A Highest Priority Pending Interrupt register for the handling of Group 1 interrupts}
 ARM_GICC_AHPPIR_PENDINTID_MASK = ARM_GICC_HPPIR_PENDINTID_MASK;

 ARM_GICC_AHPPIR_CPUID_SHIFT = ARM_GICC_HPPIR_CPUID_SHIFT;
 ARM_GICC_AHPPIR_PENDINTID_SHIFT = ARM_GICC_HPPIR_PENDINTID_SHIFT;

 {ARM GIC CPU Interface Identification Register}
 ARM_GICC_IIDR_PRODUCTID_MASK    = ($FFF shl 20); {An IMPLEMENTATION DEFINED product identifier}
 ARM_GICC_IIDR_ARCHITECTURE_MASK = ($F shl 16);   {The value of this field depends on the GIC architecture version (0x1 for GICv1 / 0x2 for GICv2)}
 ARM_GICC_IIDR_REVISION_MASK     = ($F shl 12);   {An IMPLEMENTATION DEFINED revision number for the CPU interface}
 ARM_GICC_IIDR_IMPLEMENTOR_MASK  = ($FFF shl 0);  {Contains the JEP106 code of the company that implemented the GIC CPU interface}

 ARM_GICC_IIDR_PRODUCTID_SHIFT    = 20;
 ARM_GICC_IIDR_ARCHITECTURE_SHIFT = 16;
 ARM_GICC_IIDR_REVISION_SHIFT     = 12;
 ARM_GICC_IIDR_IMPLEMENTOR_SHIFT  = 0;

 {ARM GIC Deactivate Interrupt Register}
 ARM_GICC_DIR_CPUID_MASK       = $07;  {For an SGI in a multiprocessor implementation, this field identifies the processor that requested the interrupt}
 ARM_GICC_DIR_INTERRUPTID_MASK = $3FF; {The interrupt ID}

 ARM_GICC_DIR_CPUID_SHIFT = 10;
 ARM_GICC_DIR_INTERRUPTID_SHIFT = 0;

{==============================================================================}
type
 {ARM GIC specific types}
 TGICState = record {Describes the state of a block of interrupts (32 per record)}
  Enabled:LongWord;   {Bit mask of the enabled interrupts}
  Available:LongWord; {Bit mask of the available interrupts}
  Permanent:LongWord; {Bit mask of the permanently enabled interrupts}
 end;

 TGICStates = array[0..31] of TGICState;
 PGICStates = ^TGICStates;

 TGICEntries = array[0..1023] of PInterruptEntry;
 PGICEntries = ^TGICEntries;

 TGICArrays = array[0..7] of PGICEntries;
 PGICArrays = ^TGICArrays;

 PGICDevice = ^TGICDevice;
 TGICDevice = record
  {GIC Properties}
  DistAddress:PtrUInt;
  CPUAddress:PtrUInt;
  CPUCount:LongWord;
  IRQCount:LongWord;
  SecureMode:LongBool;
  FIQAvailable:LongBool;
  GICEnabled:LongBool;
  {Interrupt Properties}
  Interrupts:PGICEntries;        {Array of interrupt entries}
  LocalInterrupts:PGICArrays;    {Arrays of local interrupt entries (1 array per CPU)}
  SoftwareInterrupts:PGICArrays; {Arrays of software interrupt entries (1 array per CPU)}
  {State Properties}
  States:PGICStates;             {Array of interrupt states}
  LocalStates:PGICStates;        {Array of local interrupt states (1 element per CPU)}
  SoftwareStates:PGICStates;     {Array of software interrupt states (1 element per CPU)}
 end;

{==============================================================================}
{var}
 {ARM GIC specific variables}

{==============================================================================}
{Initialization Functions}

{==============================================================================}
{ARM GIC Functions}
function ARMGICCreate(DistAddress,CPUAddress:PtrUInt):PGICDevice;

function ARMGICStart(GIC:PGICDevice):LongWord;

function ARMGICGetEntry(GIC:PGICDevice;CPUID,Number,Flags:LongWord;var Entry:TInterruptEntry;Index:LongWord):LongWord;
function ARMGICRegisterEntry(GIC:PGICDevice;const Entry:TInterruptEntry):LongWord;
function ARMGICDeregisterEntry(GIC:PGICDevice;const Entry:TInterruptEntry):LongWord;

{==============================================================================}
{ARM GIC Interrupt Functions}
function ARMGICDispatchIRQ(CPUID:LongWord;Thread:TThreadHandle):TThreadHandle;
function ARMGICDispatchFIQ(CPUID:LongWord;Thread:TThreadHandle):TThreadHandle;

function ARMGICDispatchInterrupt(CPUID:LongWord;Thread:TThreadHandle):TThreadHandle;

{==============================================================================}
{ARM GIC Helper Functions}
function ARMGICIsValid(GIC:PGICDevice;Number:LongWord):Boolean;
function ARMGICIsLocal(GIC:PGICDevice;Number:LongWord):Boolean;
function ARMGICIsSoftware(GIC:PGICDevice;Number:LongWord):Boolean;
function ARMGICIsGlobal(GIC:PGICDevice;Number:LongWord):Boolean;

function ARMGICGetIRQCount(GIC:PGICDevice):LongWord;
function ARMGICGetFIQCount(GIC:PGICDevice):LongWord;

function ARMGICGetIRQStart(GIC:PGICDevice):LongWord;

function ARMGICGetIRQRouting(GIC:PGICDevice):LongWord;
function ARMGICGetFIQRouting(GIC:PGICDevice):LongWord;

function ARMGICGetIRQLocalCount(GIC:PGICDevice):LongWord;
function ARMGICGetFIQLocalCount(GIC:PGICDevice):LongWord;

function ARMGICGetIRQLocalStart(GIC:PGICDevice):LongWord;

function ARMGICGetIRQSoftwareCount(GIC:PGICDevice):LongWord;
function ARMGICGetFIQSoftwareCount(GIC:PGICDevice):LongWord;

function ARMGICGetIRQSoftwateStart(GIC:PGICDevice):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {ARM GIC specific variables}
 ARMGICDevices:array[0..ARM_GIC_MAX_DEVICES - 1] of PGICDevice;

{==============================================================================}
{==============================================================================}
{ARM GIC Forward Declarations}
function ARMGICHandleInterrupt(GIC:PGICDevice;Number,Source,CPUID:LongWord;Thread:TThreadHandle):TThreadHandle; forward;

function ARMGICCheckValid(GIC:PGICDevice;const Entry:TInterruptEntry):Boolean; forward;
function ARMGICCheckHandlers(GIC:PGICDevice;const Entry:TInterruptEntry):Boolean; forward;
function ARMGICCompareHandlers(GIC:PGICDevice;const Entry,Current:TInterruptEntry):Boolean; forward;

function ARMGICEnableIRQ(GIC:PGICDevice;const Entry:TInterruptEntry):Boolean; forward;
function ARMGICDisableIRQ(GIC:PGICDevice;const Entry:TInterruptEntry):Boolean; forward;

function ARMGICEnableFIQ(GIC:PGICDevice;const Entry:TInterruptEntry):Boolean; forward;
function ARMGICDisableFIQ(GIC:PGICDevice;const Entry:TInterruptEntry):Boolean; forward;

function ARMGICGetCurrentCount(GIC:PGICDevice;CPUID,Number:LongWord):LongWord; forward;
function ARMGICGetCurrentEntry(GIC:PGICDevice;CPUID,Number:LongWord;Index:LongWord):PInterruptEntry; forward;

function ARMGICAddCurrentEntry(GIC:PGICDevice;CPUID,Number:LongWord;Entry:PInterruptEntry):Boolean; forward;
function ARMGICDeleteCurrentEntry(GIC:PGICDevice;CPUID,Number:LongWord;Entry:PInterruptEntry):Boolean; forward;

function ARMGICFindMatchingEntry(GIC:PGICDevice;const Entry:TInterruptEntry):PInterruptEntry; forward;

{==============================================================================}
{==============================================================================}
{Initialization Functions}

{==============================================================================}
{==============================================================================}
{ARM GIC Functions}
function ARMGICCreate(DistAddress,CPUAddress:PtrUInt):PGICDevice;
var
 Count:LongWord;
 GICDevice:PGICDevice;
begin
 {}
 Result:=nil;

 {Check Address}
 if (DistAddress = 0) or (CPUAddress = 0) then Exit;

 {Create GIC}
 GICDevice:=AllocMem(SizeOf(TGICDevice));
 if GICDevice = nil then Exit;

 {Update GIC}
 GICDevice.DistAddress:=DistAddress;
 GICDevice.CPUAddress:=CPUAddress;
 GICDevice.SecureMode:=SECURE_BOOT;
 GICDevice.FIQAvailable:=SECURE_BOOT;

 {Get CPU and IRQ Counts}
 GICDevice.CPUCount:=((PLongWord(DistAddress + ARM_GICD_TYPER)^ and ARM_GICD_TYPER_CPUNUM_MASK) shr ARM_GICD_TYPER_CPUNUM_SHIFT) + 1;
 GICDevice.IRQCount:=((PLongWord(DistAddress + ARM_GICD_TYPER)^ and ARM_GICD_TYPER_IRQNUM_MASK) shr ARM_GICD_TYPER_IRQNUM_SHIFT + 1) * 32;

 {Allocate Interrupts array}
 GICDevice.Interrupts:=AllocMem(SizeOf(PInterruptEntry) * (GICDevice.IRQCount - (ARM_GIC_SGI_COUNT + ARM_GIC_PPI_COUNT)));

 {Allocate Local Interrupts arrays (1 array per CPU / 16 entries per array)}
 GICDevice.LocalInterrupts:=AllocMem(SizeOf(PGICEntries) * GICDevice.CPUCount);
 for Count:=0 to GICDevice.CPUCount - 1 do
  begin
   GICDevice.LocalInterrupts[Count]:=AllocMem(SizeOf(PInterruptEntry) * ARM_GIC_PPI_COUNT);
  end;

 {Allocate Software Interrupts arrays (1 array per CPU / 16 entries per array)}
 GICDevice.SoftwareInterrupts:=AllocMem(SizeOf(PGICEntries) * GICDevice.CPUCount);
 for Count:=0 to GICDevice.CPUCount - 1 do
  begin
   GICDevice.SoftwareInterrupts[Count]:=AllocMem(SizeOf(PInterruptEntry) * ARM_GIC_SGI_COUNT);
  end;

 {Allocate Interrupt States array (1 element per 32 interrupts)}
 GICDevice.States:=AllocMem(SizeOf(TGICState) * ((GICDevice.IRQCount - (ARM_GIC_SGI_COUNT + ARM_GIC_PPI_COUNT)) div 32));

 {Allocate Local Interrupt States array (1 element per CPU)}
 GICDevice.LocalStates:=AllocMem(SizeOf(TGICState) * GICDevice.CPUCount);

 {Allocate Software Interrupt States array (1 element per CPU)}
 GICDevice.SoftwareStates:=AllocMem(SizeOf(TGICState) * GICDevice.CPUCount);

 {Acquire Lock}
 if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.AcquireLock(InterruptLock.Lock);
 try
  {Add GIC}
  Count:=0;
  while Count < ARM_GIC_MAX_DEVICES do
   begin
    if ARMGICDevices[Count] = nil then
     begin
      ARMGICDevices[Count]:=GICDevice;
      Break;
     end;

    Inc(Count);

    {Fail if no free slots}
    if Count >= ARM_GIC_MAX_DEVICES then Exit;
   end;

  {Return GIC}
  Result:=GICDevice;
 finally
  {Release Lock}
  if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.ReleaseLock(InterruptLock.Lock);

  {Release GIC}
  if Result = nil then FreeMem(GICDevice);
 end;
end;

{==============================================================================}

function ARMGICStart(GIC:PGICDevice):LongWord;

 function GetPriorityValue:LongWord;
 begin
  {}
  Result:=INTERRUPT_PRIORITY_DEFAULT
       or INTERRUPT_PRIORITY_DEFAULT shl 8
       or INTERRUPT_PRIORITY_DEFAULT shl 16
       or INTERRUPT_PRIORITY_DEFAULT shl 24;
 end;

 function GetTargetsValue(CPUID:LongWord):LongWord;
 begin
  {}
  Result:=(1 shl CPUID)
       or (1 shl CPUID) shl 8
       or (1 shl CPUID) shl 16
       or (1 shl CPUID) shl 24;
 end;

var
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check GIC}
 if GIC = nil then Exit;

 {Acquire Lock}
 if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.AcquireLock(InterruptLock.Lock);
 try
  {Check Current CPU}
  if CPUGetCurrent = CPU_BOOT then
   begin
    {Check Enabled}
    if GIC.GICEnabled then Exit;

    //To Do //Identify the supported interrupts, see 3.1.2 Identifying the supported interrupts
            //Or should that be done in create ? - Possibly

    {Initialize Distributor}
    if GIC.SecureMode then
     begin
      {Secure mode initialization}
      {Disable interrupt forwarding}
      PLongWord(GIC.DistAddress + ARM_GICD_CTLR)^:=ARM_GICD_CTLRS_DISABLE;

      {Disable, Acknowledge and Deactivate all interrupts}
      for Count:=0 to (GIC.IRQCount div 32) - 1 do
       begin
        PLongWord(GIC.DistAddress + ARM_GICD_ICENABLER0 + (4 * Count))^:=$FFFFFFFF;
        PLongWord(GIC.DistAddress + ARM_GICD_ICPENDR0 + (4 * Count))^:=$FFFFFFFF;
        PLongWord(GIC.DistAddress + ARM_GICD_ICACTIVER0 + (4 * Count))^:=$FFFFFFFF;
       end;

      {Set all interrupts to boot CPU and default priority}
      for Count:=0 to (GIC.IRQCount div 4) - 1 do
       begin
        PLongWord(GIC.DistAddress + ARM_GICD_IPRIORITYR0 + (4 * Count))^:=GetPriorityValue;
        PLongWord(GIC.DistAddress + ARM_GICD_ITARGETSR0 + (4 * Count))^:=GetTargetsValue(CPUGetCurrent);
       end;

      {Set all interrupts to level triggerred}
      for Count:=0 to (GIC.IRQCount div 16) - 1 do
       begin
        PLongWord(GIC.DistAddress + ARM_GICD_ICFGR0 + (4 * Count))^:=0; {ARM_GICD_ICFGR_LEVEL_SENSITIVE}
       end;

      {Enable interrupt forwarding}
      PLongWord(GIC.DistAddress + ARM_GICD_CTLR)^:=ARM_GICD_CTLRS_ENABLE_GROUP0 or ARM_GICD_CTLRS_ENABLE_GROUP1;
     end
    else
     begin
      {Non secure mode initialization}
      {Disable interrupt forwarding}
      PLongWord(GIC.DistAddress + ARM_GICD_CTLR)^:=ARM_GICD_CTLR_DISABLE;

      {Disable, Acknowledge and Deactivate all interrupts}
      for Count:=0 to (GIC.IRQCount div 32) - 1 do
       begin
        PLongWord(GIC.DistAddress + ARM_GICD_ICENABLER0 + (4 * Count))^:=$FFFFFFFF;
        PLongWord(GIC.DistAddress + ARM_GICD_ICPENDR0 + (4 * Count))^:=$FFFFFFFF;
        PLongWord(GIC.DistAddress + ARM_GICD_ICACTIVER0 + (4 * Count))^:=$FFFFFFFF;
       end;

      {Set all interrupts to CPU0 and default priority}
      for Count:=0 to (GIC.IRQCount div 4) - 1 do
       begin
        PLongWord(GIC.DistAddress + ARM_GICD_IPRIORITYR0 + (4 * Count))^:=GetPriorityValue;
        PLongWord(GIC.DistAddress + ARM_GICD_ITARGETSR0 + (4 * Count))^:=GetTargetsValue(CPUGetCurrent);
       end;

      {Set all interrupts to level triggered}
      for Count:=0 to (GIC.IRQCount div 16) - 1 do
       begin
        PLongWord(GIC.DistAddress + ARM_GICD_ICFGR0 + (4 * Count))^:=0; {ARM_GICD_ICFGR_LEVEL_SENSITIVE}
       end;

      {Enable interrupt forwarding}
      PLongWord(GIC.DistAddress + ARM_GICD_CTLR)^:=ARM_GICD_CTLR_ENABLE;
     end;
   end
  else
   begin
    {Check Enabled}
    if not GIC.GICEnabled then Exit;

    {Disable, Acknowledge and Deactivate local interrupts (0-31)}
    {GICD_ICENABLER0 is banked for each connected processor}
    {GICD_ICPENDR0 is banked for each connected processor}
    {GICD_ICACTIVER0 is banked for each connected processor}
    PLongWord(GIC.DistAddress + ARM_GICD_ICENABLER0)^:=$FFFFFFFF;
    PLongWord(GIC.DistAddress + ARM_GICD_ICPENDR0)^:=$FFFFFFFF;
    PLongWord(GIC.DistAddress + ARM_GICD_ICACTIVER0)^:=$FFFFFFFF;

    {Set local interrupts to default priority (0-31)}
    {GICD_ITARGETSR0 to GICD_ITARGETSR7 are read-only, and each field returns a value that corresponds only to the processor reading the register}
    {GICD_IPRIORITYR0 to GICD_IPRIORITYR7 are banked for each connected processor}
    for Count:=0 to (32 div 4) - 1 do
     begin
      PLongWord(GIC.DistAddress + ARM_GICD_IPRIORITYR0 + (4 * Count))^:=GetPriorityValue;
     end;

    {Set local interrupts to level triggerred (0-31)}
    {GICD_ICFGR1 is banked for each connected processor}
    PLongWord(GIC.DistAddress + ARM_GICD_ICFGR0 + 4)^:=0; {ARM_GICD_ICFGR_LEVEL_SENSITIVE}
   end;

  {Initialize CPU Interface}
  if GIC.SecureMode then
   begin
    {Secure mode initialization}
    {Disable Interrupt Forwarding}
    PLongWord(GIC.CPUAddress + ARM_GICC_CTLR)^:=ARM_GICC_CTLRS_DISABLE;

    {Set the Priority Mask}
    PLongWord(GIC.CPUAddress + ARM_GICC_PMR)^:=ARM_GICC_PMR_PRIORITY;

    {Enable FIQ and Interrupt Forwarding}
    PLongWord(GIC.CPUAddress + ARM_GICC_CTLR)^:=ARM_GICC_CTLRS_ENABLE_GROUP0 or ARM_GICC_CTLRS_ENABLE_GROUP1 or ARM_GICC_CTLRS_FIQ_ENABLE;
   end
  else
   begin
    {Non secure mode initialization}
    {Disable Interrupt Forwarding}
    PLongWord(GIC.CPUAddress + ARM_GICC_CTLR)^:=ARM_GICC_CTLR_DISABLE;

    {Set the Priority Mask}
    PLongWord(GIC.CPUAddress + ARM_GICC_PMR)^:=ARM_GICC_PMR_PRIORITY;

    {Enable Interrupt Forwarding}
    PLongWord(GIC.CPUAddress + ARM_GICC_CTLR)^:=ARM_GICC_CTLR_ENABLE;
   end;

  {Check Current CPU}
  if CPUGetCurrent = CPU_BOOT then
   begin
    {Enabled GIC}
    GIC.GICEnabled:=True;
   end;

  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Release Lock}
  if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.ReleaseLock(InterruptLock.Lock);
 end;
end;

{==============================================================================}

function ARMGICGetEntry(GIC:PGICDevice;CPUID,Number,Flags:LongWord;var Entry:TInterruptEntry;Index:LongWord):LongWord;
{Note: The returned Entry is a copy of the registered value. Caller should free Entry if required}
{      For shared entries the Index parameter indicates which entry in the chain to return (0 equals first etc)}
var
 Current:PInterruptEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Setup Defaults}
 FillChar(Entry,SizeOf(TInterruptEntry),0);

 {Check GIC}
 if GIC = nil then Exit;

 {Acquire Lock}
 if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.AcquireLock(InterruptLock.Lock);
 try
  {Check Enabled}
  if not GIC.GICEnabled then Exit;

  {Check Flags}
  if (Flags and INTERRUPT_FLAG_IPI) <> 0 then
   begin
    {Software Entry}
    if not ARMGICIsSoftware(GIC,Number) then Exit;

    {Check CPU}
    if CPUID > GIC.CPUCount - 1 then Exit;
   end
  else if (Flags and INTERRUPT_FLAG_LOCAL) <> 0 then
   begin
    {Local Entry}
    if not ARMGICIsLocal(GIC,Number) then Exit;

    {Check CPU}
    if CPUID > GIC.CPUCount - 1 then Exit;
   end
  else
   begin
    {Global Entry}
    if not ARMGICIsGlobal(GIC,Number) then Exit;
   end;

  Result:=ERROR_NOT_FOUND;

  {Get Current}
  Current:=ARMGICGetCurrentEntry(GIC,CPUID,Number,Index);
  if Current <> nil then
   begin
    {Copy Entry}
    Entry.Number:=Current.Number;
    Entry.Flags:=Current.Flags;
    Entry.CPUMask:=Current.CPUMask;
    Entry.Priority:=Current.Priority;
    Entry.Handler:=Current.Handler;
    Entry.HandlerEx:=Current.HandlerEx;
    Entry.SharedHandler:=Current.SharedHandler;
    Entry.Parameter:=Current.Parameter;

    {Return Result}
    Result:=ERROR_SUCCESS;
   end;
 finally
  {Release Lock}
  if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.ReleaseLock(InterruptLock.Lock);
 end;
end;

{==============================================================================}

function ARMGICRegisterEntry(GIC:PGICDevice;const Entry:TInterruptEntry):LongWord;
{Note: Entry must be allocated from heap as a pointer to it will be retained while
       the interrupt remains registered. Entry must not be freed by the caller}
var
 Current:PInterruptEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check GIC}
 if GIC = nil then Exit;

 {Acquire Lock}
 if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.AcquireLock(InterruptLock.Lock);
 try
  {Check Enabled}
  if not GIC.GICEnabled then Exit;

  {Check Entry}
  if not ARMGICCheckValid(GIC,Entry) then Exit;

  Result:=ERROR_ALREADY_ASSIGNED;

  {Check Count}
  if ARMGICGetCurrentCount(GIC,Entry.CPUID,Entry.Number) = 0 then
   begin
    {Single Entry}
    Result:=ERROR_OPERATION_FAILED;

    {Check FIQ}
    if Entry.IsFIQ then
     begin
      {Enable FIQ}
      if not ARMGICEnableFIQ(GIC,Entry) then Exit;
     end
    else
     begin
      {Enable IRQ}
      if not ARMGICEnableIRQ(GIC,Entry) then Exit;
     end;

    {Add Entry}
    if not ARMGICAddCurrentEntry(GIC,Entry.CPUID,Entry.Number,@Entry) then Exit;
   end
  else
   begin
    {Shared Entry}
    Result:=ERROR_ALREADY_ASSIGNED;

    {Check Shared}
    if not Entry.IsShared then Exit;

    {Get Match}
    Current:=ARMGICFindMatchingEntry(GIC,Entry);
    if Current <> nil then Exit;

    {Get Current}
    Current:=ARMGICGetCurrentEntry(GIC,Entry.CPUID,Entry.Number,0);
    if Current = nil then Exit;

    {Check Shared}
    if not Current.IsShared then Exit;

    {Check FIQ}
    if Entry.IsFIQ <> Current.IsFIQ then Exit;

    Result:=ERROR_OPERATION_FAILED;

    {Add Entry}
    if not ARMGICAddCurrentEntry(GIC,Entry.CPUID,Entry.Number,@Entry) then Exit;
   end;

  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Release Lock}
  if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.ReleaseLock(InterruptLock.Lock);
 end;
end;

{==============================================================================}

function ARMGICDeregisterEntry(GIC:PGICDevice;const Entry:TInterruptEntry):LongWord;
{Note: The Entry can be a local temporary copy allocated either from the stack or on
       the heap, this routine will free the original Entry passed to Register once it
       is successfully deregistered. Caller should free Entry if required}
var
 Current:PInterruptEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check GIC}
 if GIC = nil then Exit;

 {Acquire Lock}
 if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.AcquireLock(InterruptLock.Lock);
 try
  {Check Enabled}
  if not GIC.GICEnabled then Exit;

  {Check Entry}
  if not ARMGICCheckValid(GIC,Entry) then Exit;

  Result:=ERROR_NOT_ASSIGNED;

  {Get Match}
  Current:=ARMGICFindMatchingEntry(GIC,Entry);
  if Current = nil then Exit;

  Result:=ERROR_OPERATION_FAILED;

  {Check Count}
  if ARMGICGetCurrentCount(GIC,Entry.CPUID,Entry.Number) = 1 then
   begin
    {Single Entry}
    {Check FIQ}
    if Entry.IsFIQ then
     begin
      {Disable FIQ}
      if not ARMGICDisableFIQ(GIC,Entry) then Exit;
     end
    else
     begin
      {Disable IRQ}
      if not ARMGICDisableIRQ(GIC,Entry) then Exit;
     end;
   end;

  {Delete Entry}
  if not ARMGICDeleteCurrentEntry(GIC,Entry.CPUID,Entry.Number,Current) then Exit;

  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Release Lock}
  if InterruptLock.Lock <> INVALID_HANDLE_VALUE then InterruptLock.ReleaseLock(InterruptLock.Lock);
 end;
end;

{==============================================================================}
{==============================================================================}
{ARM GIC Interrupt Functions}
function ARMGICDispatchIRQ(CPUID:LongWord;Thread:TThreadHandle):TThreadHandle;
{Called by ARMv7/8IRQHandler in PlatformARMv7/8}
begin
 {}
 {$IF DEFINED(IRQ_STATISTICS) or DEFINED(INTERRUPT_DEBUG)}
 Inc(DispatchInterruptCounter[CPUID]);
 {$ENDIF}

 Result:=ARMGICDispatchInterrupt(CPUID,Thread);
end;

{==============================================================================}

function ARMGICDispatchFIQ(CPUID:LongWord;Thread:TThreadHandle):TThreadHandle;
{Called by ARMv7/8FIQHandler in PlatformARMv7/8}
begin
 {}
 {$IF DEFINED(FIQ_STATISTICS) or DEFINED(INTERRUPT_DEBUG)}
 Inc(DispatchFastInterruptCounter[CPUID]);
 {$ENDIF}

 Result:=ARMGICDispatchInterrupt(CPUID,Thread);
end;

{==============================================================================}

function ARMGICDispatchInterrupt(CPUID:LongWord;Thread:TThreadHandle):TThreadHandle;
{Process any pending IRQ/FIQ requests}
{Called by ARMGICDispatchIRQ/ARMGICDispatchFIQ}
{Note: A DataMemoryBarrier is executed before and after calling this function}
var
 Count:Integer;
 Status:LongWord;
 Source:LongWord;
 Number:LongWord;
 GICDevice:PGICDevice;
begin
 {}
 Result:=Thread;

 {Check each ARM GIC Device}
 for Count:=0 to ARM_GIC_MAX_DEVICES - 1 do
  begin
   Source:=CPU_ID_ALL;

   GICDevice:=ARMGICDevices[Count];
   if GICDevice <> nil then
    begin
     {Read Interrupt Acknowledge}
     Status:=PLongWord(GICDevice.CPUAddress + ARM_GICC_IAR)^;

     {Get Interrupt Number}
     Number:=(Status and ARM_GICC_IAR_INTERRUPTID_MASK) shr ARM_GICC_IAR_INTERRUPTID_SHIFT;
     case Number of
      {Software (0 to 15}
      ARM_GIC_SGI_BASE..(ARM_GIC_SGI_BASE + ARM_GIC_SGI_COUNT - 1):begin
        {Get Source CPU}
        Source:=(Status and ARM_GICC_IAR_CPUID_MASK) shr ARM_GICC_IAR_CPUID_SHIFT;

        {Handle Interrupt}
        Result:=ARMGICHandleInterrupt(GICDevice,Number,Source,CPUID,Result); {Pass Result as Thread to allow for multiple calls}

        {Signal End Of Interrupt}
        PLongWord(GICDevice.CPUAddress + ARM_GICC_EOIR)^:=Status;

        Break;
       end;
      {Local (16 to 31)}
      ARM_GIC_PPI_BASE..(ARM_GIC_PPI_BASE + ARM_GIC_PPI_COUNT - 1):begin
        {Handle Interrupt}
        Result:=ARMGICHandleInterrupt(GICDevice,Number,Source,CPUID,Result); {Pass Result as Thread to allow for multiple calls}

        {Signal End Of Interrupt}
        PLongWord(GICDevice.CPUAddress + ARM_GICC_EOIR)^:=Status;

        Break;
       end;
      {Global (32..1019)}
      ARM_GIC_SPI_BASE..ARM_GIC_SPI_MAX:begin
        {Handle Interrupt}
        Result:=ARMGICHandleInterrupt(GICDevice,Number,Source,CPUID,Result); {Pass Result as Thread to allow for multiple calls}

        {Signal End Of Interrupt}
        PLongWord(GICDevice.CPUAddress + ARM_GICC_EOIR)^:=Status;

        Break;
       end;
      {Group 1}
      ARM_GIC_GROUP1_INTERRUPT:begin
        {When operating in Secure (Group0) mode the GIC will signal an ARM_GIC_GROUP1_INTERRUPT to
         indicate that a Group1 interrupt is pending and we should read ARM_GICC_AIAR to obtain it}
        {Read Interrupt Acknowledge Alias}
        Status:=PLongWord(GICDevice.CPUAddress + ARM_GICC_AIAR)^;

        {Get Interrupt Number}
        Number:=(Status and ARM_GICC_AIAR_INTERRUPTID_MASK) shr ARM_GICC_AIAR_INTERRUPTID_SHIFT;
        case Number of
         {Software (0 to 15}
         ARM_GIC_SGI_BASE..(ARM_GIC_SGI_BASE + ARM_GIC_SGI_COUNT - 1):begin
           {Get Source CPU}
           Source:=(Status and ARM_GICC_AIAR_CPUID_MASK) shr ARM_GICC_AIAR_CPUID_SHIFT;

           {Handle Interrupt}
           Result:=ARMGICHandleInterrupt(GICDevice,Number,Source,CPUID,Result); {Pass Result as Thread to allow for multiple calls}

           {Signal End Of Interrupt Alias}
           PLongWord(GICDevice.CPUAddress + ARM_GICC_AEOIR)^:=Status;

           Break;
          end;
         {Local (16 to 31)}
         ARM_GIC_PPI_BASE..(ARM_GIC_PPI_BASE + ARM_GIC_PPI_COUNT - 1):begin
           {Handle Interrupt}
           Result:=ARMGICHandleInterrupt(GICDevice,Number,Source,CPUID,Result); {Pass Result as Thread to allow for multiple calls}

           {Signal End Of Interrupt Alias}
           PLongWord(GICDevice.CPUAddress + ARM_GICC_AEOIR)^:=Status;

           Break;
          end;
         {Global (32..1019)}
         ARM_GIC_SPI_BASE..ARM_GIC_SPI_MAX:begin
           {Handle Interrupt}
           Result:=ARMGICHandleInterrupt(GICDevice,Number,Source,CPUID,Result); {Pass Result as Thread to allow for multiple calls}

           {Signal End Of Interrupt Alias}
           PLongWord(GICDevice.CPUAddress + ARM_GICC_AEOIR)^:=Status;

           Break;
          end;
        end;
       end;
     end;
    end;
  end;
end;

{==============================================================================}

function ARMGICHandleInterrupt(GIC:PGICDevice;Number,Source,CPUID:LongWord;Thread:TThreadHandle):TThreadHandle;
{Call the handler function for an IRQ/FIQ that was received, or halt if it doesn't exist}
var
 Status:LongWord;
 Entry:PInterruptEntry;
begin
 {}
 Result:=Thread;

 {Check GIC}
 if GIC = nil then Exit;

 {Get Entry}
 Entry:=ARMGICGetCurrentEntry(GIC,CPUID,Number,0);
 if Entry = nil then
  begin
   {Check Software}
   if ARMGICIsSoftware(GIC,Number) then Exit;

   {Halt}
   {$IF DEFINED(PLATFORM_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
   if PLATFORM_LOG_ENABLED then PlatformLogDebug('No entry registered for interrupt ' + IntToStr(Number) + ' on CPUID ' + IntToStr(CPUID));
   {$ENDIF}

   Halt;
  end;

 {Check Entry}
 if not Entry.IsIPI then
  begin
   {Global or Local}
   if Entry.IsShared then
    begin
     {Shared}
     if not Assigned(Entry.SharedHandler) then
      begin
       {Halt}
       {$IF DEFINED(PLATFORM_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
       if PLATFORM_LOG_ENABLED then PlatformLogDebug('No shared handler registered for interrupt ' + IntToStr(Number) + ' on CPUID ' + IntToStr(CPUID));
       {$ENDIF}

       Halt;
      end;

     {Call Handler}
     Status:=Entry.SharedHandler(Number,CPUID,Entry.Flags,Entry.Parameter);
     while Status <> INTERRUPT_RETURN_HANDLED do
      begin
       {Get Next}
       Entry:=Entry.Next;
       if Entry = nil then
        begin
         {Halt}
         {$IF DEFINED(PLATFORM_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
         if PLATFORM_LOG_ENABLED then PlatformLogDebug('Unhandled interrupt ' + IntToStr(Number) + ' on CPUID ' + IntToStr(CPUID));
         {$ENDIF}

         Halt;
        end;

       if not Assigned(Entry.SharedHandler) then
        begin
         {Halt}
         {$IF DEFINED(PLATFORM_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
         if PLATFORM_LOG_ENABLED then PlatformLogDebug('No shared handler registered for interrupt ' + IntToStr(Number) + ' on CPUID ' + IntToStr(CPUID));
         {$ENDIF}

         Halt;
        end;

       {Call Handler}
       Status:=Entry.SharedHandler(Number,CPUID,Entry.Flags,Entry.Parameter);
      end;
    end
   else
    begin
     {Single}
     if Assigned(Entry.Handler) then
      begin
       {Call Handler}
       Entry.Handler(Entry.Parameter);
      end
     else if Assigned(Entry.HandlerEx) then
      begin
       {Call Handler}
       Result:=Entry.HandlerEx(CPUID,Thread,Entry.Parameter);
      end
     else if Assigned(Entry.SharedHandler) then
      begin
       {Call Handler}
       Entry.SharedHandler(Number,CPUID,Entry.Flags,Entry.Parameter);
      end
     else
      begin
       {Halt}
       {$IF DEFINED(PLATFORM_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
       if PLATFORM_LOG_ENABLED then PlatformLogDebug('No handler registered for interrupt ' + IntToStr(Number) + ' on CPUID ' + IntToStr(CPUID));
       {$ENDIF}

       Halt;
      end;
    end;
  end
 else
  begin
   {Software}
   if not Assigned(Entry.SharedHandler) then
    begin
     {Halt}
     {$IF DEFINED(PLATFORM_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
     if PLATFORM_LOG_ENABLED then PlatformLogDebug('No IPI handler registered for interrupt ' + IntToStr(Number) + ' on CPUID ' + IntToStr(CPUID));
     {$ENDIF}

     Halt;
    end;

   {Call Handler}
   Status:=Entry.SharedHandler(Number,Source,Entry.Flags,Entry.Parameter);

   {Check Entry}
   if Entry.IsShared then
    begin
     {Shared}
     while Status <> INTERRUPT_RETURN_HANDLED do
      begin
       {Get Next}
       Entry:=Entry.Next;
       if Entry = nil then Exit;

       if not Assigned(Entry.SharedHandler) then
        begin
         {Halt}
         {$IF DEFINED(PLATFORM_DEBUG) and DEFINED(INTERRUPT_DEBUG)}
         if PLATFORM_LOG_ENABLED then PlatformLogDebug('No IPI handler registered for interrupt ' + IntToStr(Number) + ' on CPUID ' + IntToStr(CPUID));
         {$ENDIF}

         Halt;
        end;

       {Call Handler}
       Status:=Entry.SharedHandler(Number,Source,Entry.Flags,Entry.Parameter);
      end;
    end;
  end;
end;

{==============================================================================}
{==============================================================================}
{ARM GIC Helper Functions}
function ARMGICIsValid(GIC:PGICDevice;Number:LongWord):Boolean;
begin
 {}
 Result:=False;

 {Check GIC}
 if GIC = nil then Exit;

 {Check Enabled}
 if not GIC.GICEnabled then Exit;

 {Check Number}
 if Number > GIC.IRQCount - 1 then Exit;

 Result:=True;
end;

{==============================================================================}

function ARMGICIsLocal(GIC:PGICDevice;Number:LongWord):Boolean;
begin
 {}
 Result:=False;

 {Check GIC}
 if GIC = nil then Exit;

 {Check Enabled}
 if not GIC.GICEnabled then Exit;

 {Check Number}
 if (Number < ARM_GIC_PPI_BASE) or (Number >= ARM_GIC_SPI_BASE) then Exit;

 Result:=True;
end;

{==============================================================================}

function ARMGICIsSoftware(GIC:PGICDevice;Number:LongWord):Boolean;
begin
 {}
 Result:=False;

 {Check GIC}
 if GIC = nil then Exit;

 {Check Enabled}
 if not GIC.GICEnabled then Exit;

 {Check Number}
 if Number >= ARM_GIC_PPI_BASE then Exit;

 Result:=True;
end;

{==============================================================================}

function ARMGICIsGlobal(GIC:PGICDevice;Number:LongWord):Boolean;
begin
 {}
 Result:=False;

 {Check GIC}
 if GIC = nil then Exit;

 {Check Enabled}
 if not GIC.GICEnabled then Exit;

 {Check Number}
 if (Number < ARM_GIC_SPI_BASE) or (Number > GIC.IRQCount - 1) then Exit;

 Result:=True;
end;

{==============================================================================}

function ARMGICGetIRQCount(GIC:PGICDevice):LongWord;
begin
 {}
 Result:=0;

 {Check GIC}
 if GIC = nil then Exit;

 {Check Enabled}
 if not GIC.GICEnabled then Exit;

 {Get IRQ Count}
 Result:=GIC.IRQCount;
end;

{==============================================================================}

function ARMGICGetFIQCount(GIC:PGICDevice):LongWord;
begin
 {}
 Result:=0;

 {Check GIC}
 if GIC = nil then Exit;

 {Check Enabled}
 if not GIC.GICEnabled then Exit;

 {Check FIQ}
 if not GIC.FIQAvailable then Exit;

 {Get IRQ Count}
 Result:=GIC.IRQCount;
end;

{==============================================================================}

function ARMGICGetIRQStart(GIC:PGICDevice):LongWord;
begin
 {}
 Result:=0;

 {Check GIC}
 if GIC = nil then Exit;

 {Check Enabled}
 if not GIC.GICEnabled then Exit;

 {Get SPI Base}
 Result:=ARM_GIC_SPI_BASE;
end;

{==============================================================================}

function ARMGICGetIRQRouting(GIC:PGICDevice):LongWord;
begin
 {}
 Result:=0;

 {Check GIC}
 if GIC = nil then Exit;

 {Check Enabled}
 if not GIC.GICEnabled then Exit;

 Result:=CPU_ID_ALL;
end;

{==============================================================================}

function ARMGICGetFIQRouting(GIC:PGICDevice):LongWord;
begin
 {}
 Result:=0;

 {Check GIC}
 if GIC = nil then Exit;

 {Check Enabled}
 if not GIC.GICEnabled then Exit;

 Result:=CPU_ID_ALL;
end;

{==============================================================================}

function ARMGICGetIRQLocalCount(GIC:PGICDevice):LongWord;
begin
 {}
 Result:=0;

 {Check GIC}
 if GIC = nil then Exit;

 {Check Enabled}
 if not GIC.GICEnabled then Exit;

 {Get PPI Count}
 Result:=ARM_GIC_PPI_COUNT;
end;

{==============================================================================}

function ARMGICGetFIQLocalCount(GIC:PGICDevice):LongWord;
begin
 {}
 Result:=0;

 {Check GIC}
 if GIC = nil then Exit;

 {Check Enabled}
 if not GIC.GICEnabled then Exit;

 {Check FIQ}
 if not GIC.FIQAvailable then Exit;

 {Get PPI Count}
 Result:=ARM_GIC_PPI_COUNT;
end;

{==============================================================================}

function ARMGICGetIRQLocalStart(GIC:PGICDevice):LongWord;
begin
 {}
 Result:=0;

 {Check GIC}
 if GIC = nil then Exit;

 {Check Enabled}
 if not GIC.GICEnabled then Exit;

 {Get PPI Base}
 Result:=ARM_GIC_PPI_BASE;
end;

{==============================================================================}

function ARMGICGetIRQSoftwareCount(GIC:PGICDevice):LongWord;
begin
 {}
 Result:=0;

 {Check GIC}
 if GIC = nil then Exit;

 {Check Enabled}
 if not GIC.GICEnabled then Exit;

 {Get SGI Count}
 Result:=ARM_GIC_SGI_COUNT;
end;

{==============================================================================}

function ARMGICGetFIQSoftwareCount(GIC:PGICDevice):LongWord;
begin
 {}
 Result:=0;

 {Check GIC}
 if GIC = nil then Exit;

 {Check Enabled}
 if not GIC.GICEnabled then Exit;

 {Check FIQ}
 if not GIC.FIQAvailable then Exit;

 {Get SGI Count}
 Result:=ARM_GIC_SGI_COUNT;
end;

{==============================================================================}

function ARMGICGetIRQSoftwateStart(GIC:PGICDevice):LongWord;
begin
 {}
 Result:=0;

 {Check GIC}
 if GIC = nil then Exit;

 {Check Enabled}
 if not GIC.GICEnabled then Exit;

 {Get SGI Base}
 Result:=ARM_GIC_SGI_BASE;
end;

{==============================================================================}
{==============================================================================}
{ARM GIC Internal Functions}
function ARMGICCheckValid(GIC:PGICDevice;const Entry:TInterruptEntry):Boolean;
{Note: Caller must hold the interrupt lock}
var
 Count:LongWord;
begin
 {}
 Result:=False;

 {Check GIC}
 if GIC = nil then Exit;

 {Check Flags}
 if Entry.IsLocal then
  begin
   {Check Number (Local)}
   if not ARMGICIsLocal(GIC,Entry.Number) then Exit;

   {Check Mask Count}
   if CPUMaskCount(Entry.CPUMask) <> 1 then Exit;

   {Check Mask CPU (Only current CPU)}
   if CPUMaskToID(Entry.CPUMask) <> CPUGetCurrent then Exit;
  end
 else if Entry.IsIPI then
  begin
   {Check Number (Software)}
   if not ARMGICIsSoftware(GIC,Entry.Number) then Exit;

   {Check Mask Count}
   if CPUMaskCount(Entry.CPUMask) <> 1 then Exit;

   {Check Mask CPU (Only current CPU)}
   if CPUMaskToID(Entry.CPUMask) <> CPUGetCurrent then Exit;
  end
 else
  begin
   {Check Number (Global)}
   if not ARMGICIsGlobal(GIC,Entry.Number) then Exit;

   {Check Mask Count}
   if CPUMaskCount(Entry.CPUMask) = 0 then Exit;

   {Check Mask CPUs}
   for Count:=CPU_ID_0 to CPU_ID_MAX do
    begin
     if (Entry.CPUMask and (1 shl Count)) <> 0 then
      begin
       if Count > (GIC.CPUCount - 1) then Exit;
      end;
    end;
  end;

 {Check Handlers}
 if not ARMGICCheckHandlers(GIC,Entry) then Exit;

 {Check Priority}
 case Entry.Priority of
  INTERRUPT_PRIORITY_MAXIMUM,INTERRUPT_PRIORITY_FIQ,INTERRUPT_PRIORITY_DEFAULT,INTERRUPT_PRIORITY_MINIMUM:begin
    {Priority is valid}
   end;
  else
   Exit;
 end;

 {Check FIQ}
 if Entry.IsFIQ then
  begin
   if not GIC.FIQAvailable then Exit;
  end;

 {Check IPI}
 if Entry.IsIPI then
  begin
   if not Assigned(Entry.SharedHandler) then Exit;
  end;

 {Check Shared}
 if Entry.IsShared then
  begin
   if not Assigned(Entry.SharedHandler) then Exit;
  end;

 Result:=True;
end;

{==============================================================================}

function ARMGICCheckHandlers(GIC:PGICDevice;const Entry:TInterruptEntry):Boolean;
{Note: Caller must hold the interrupt lock}
begin
 {}
 Result:=False;

 {Check GIC}
 if GIC = nil then Exit;

 {Check Handlers}
 if Assigned(Entry.Handler) then
  begin
   {Check Other Handlers}
   if Assigned(Entry.HandlerEx) then Exit;
   if Assigned(Entry.SharedHandler) then Exit;

   Result:=True;
  end
 else if Assigned(Entry.HandlerEx) then
  begin
   {Check Other Handlers}
   if Assigned(Entry.Handler) then Exit;
   if Assigned(Entry.SharedHandler) then Exit;

   Result:=True;
  end
 else if Assigned(Entry.SharedHandler) then
  begin
   {Check Other Handlers}
   if Assigned(Entry.Handler) then Exit;
   if Assigned(Entry.HandlerEx) then Exit;

   Result:=True;
  end;
end;

{==============================================================================}

function ARMGICCompareHandlers(GIC:PGICDevice;const Entry,Current:TInterruptEntry):Boolean;
{Note: Caller must hold the interrupt lock}
begin
 {}
 Result:=False;

 {Check GIC}
 if GIC = nil then Exit;

 {Check Handlers}
 if Assigned(Entry.Handler) then
  begin
   {Check Current Handlers}
   if not Assigned(Current.Handler) then Exit;
   if @Entry.Handler <> @Current.Handler then Exit;

   Result:=True;
  end
 else if Assigned(Entry.HandlerEx) then
  begin
   {Check Current Handlers}
   if not Assigned(Current.HandlerEx) then Exit;
   if @Entry.HandlerEx <> @Current.HandlerEx then Exit;

   Result:=True;
  end
 else if Assigned(Entry.SharedHandler) then
  begin
   {Check Current Handlers}
   if not Assigned(Current.SharedHandler) then Exit;
   if @Entry.SharedHandler <> @Current.SharedHandler then Exit;

   Result:=True;
  end;
end;

{==============================================================================}

function ARMGICEnableIRQ(GIC:PGICDevice;const Entry:TInterruptEntry):Boolean;
{Note: Caller must hold the interrupt lock}
var
 EnableOffset:LongWord;
 EnableMask:LongWord;
 PriorityOffset:LongWord;
 PriorityMask:LongWord;
 PriorityShift:LongWord;
begin
 {}
 Result:=False;

 {Check GIC}
 if GIC = nil then Exit;

 {Get Offset, Shift and Mask values}
 {Enable values are also used for Group}
 {Priority values are also used for Target}
 EnableOffset:=(Entry.Number div 32) * 4;
 EnableMask:=1 shl (Entry.Number mod 32);
 PriorityOffset:=(Entry.Number div 4) * 4;
 PriorityShift:=(Entry.Number mod 4) * 8;
 PriorityMask:=$FF shl PriorityShift;

 {Set Interrupt to Group 1}
 PLongWord(GIC.DistAddress + ARM_GICD_IGROUPR0 + EnableOffset)^:=PLongWord(GIC.DistAddress + ARM_GICD_IGROUPR0 + EnableOffset)^ or EnableMask;

 {Set Priority}
 PLongWord(GIC.DistAddress + ARM_GICD_IPRIORITYR0 + PriorityOffset)^:=(PLongWord(GIC.DistAddress + ARM_GICD_IPRIORITYR0 + PriorityOffset)^ and not(PriorityMask)) or (Entry.Priority shl PriorityShift);

 {Set Target}
 PLongWord(GIC.DistAddress + ARM_GICD_ITARGETSR0 + PriorityOffset)^:=(PLongWord(GIC.DistAddress + ARM_GICD_ITARGETSR0 + PriorityOffset)^ and not(PriorityMask)) or (Entry.CPUMask shl PriorityShift);

 {Enable Interrupt}
 PLongWord(GIC.DistAddress + ARM_GICD_ISENABLER0 + EnableOffset)^:=EnableMask;

 Result:=True;
end;

{==============================================================================}

function ARMGICDisableIRQ(GIC:PGICDevice;const Entry:TInterruptEntry):Boolean;
{Note: Caller must hold the interrupt lock}
var
 Offset:LongWord;
 Mask:LongWord;
begin
 {}
 Result:=False;

 {Check GIC}
 if GIC = nil then Exit;

 {Get Offset and Mask values}
 Offset:=(Entry.Number div 32) * 4;
 Mask:=1 shl (Entry.Number mod 32);

 {Enable Interrupt}
 PLongWord(GIC.DistAddress + ARM_GICD_ICENABLER0 + Offset)^:=Mask;

 Result:=True;
end;

{==============================================================================}

function ARMGICEnableFIQ(GIC:PGICDevice;const Entry:TInterruptEntry):Boolean;
{Note: Caller must hold the interrupt lock}
var
 EnableOffset:LongWord;
 EnableMask:LongWord;
 PriorityOffset:LongWord;
 PriorityMask:LongWord;
 PriorityShift:LongWord;
begin
 {}
 Result:=False;

 {Check GIC}
 if GIC = nil then Exit;

 {Get Offset, Shift and Mask values}
 {Enable values are also used for Group}
 {Priority values are also used for Target}
 EnableOffset:=(Entry.Number div 32) * 4;
 EnableMask:=1 shl (Entry.Number mod 32);
 PriorityOffset:=(Entry.Number div 4) * 4;
 PriorityShift:=(Entry.Number mod 4) * 8;
 PriorityMask:=$FF shl PriorityShift;

 {Set Interrupt to Group 0}
 PLongWord(GIC.DistAddress + ARM_GICD_IGROUPR0 + EnableOffset)^:=PLongWord(GIC.DistAddress + ARM_GICD_IGROUPR0 + EnableOffset)^ and not(EnableMask);

 {Set Priority}
 PLongWord(GIC.DistAddress + ARM_GICD_IPRIORITYR0 + PriorityOffset)^:=(PLongWord(GIC.DistAddress + ARM_GICD_IPRIORITYR0 + PriorityOffset)^ and not(PriorityMask)) or (Entry.Priority shl PriorityShift);

 {Set Target}
 PLongWord(GIC.DistAddress + ARM_GICD_ITARGETSR0 + PriorityOffset)^:=(PLongWord(GIC.DistAddress + ARM_GICD_ITARGETSR0 + PriorityOffset)^ and not(PriorityMask)) or (Entry.CPUMask shl PriorityShift);

 {Enable Interrupt}
 PLongWord(GIC.DistAddress + ARM_GICD_ISENABLER0 + EnableOffset)^:=EnableMask;

 Result:=True;
end;

{==============================================================================}

function ARMGICDisableFIQ(GIC:PGICDevice;const Entry:TInterruptEntry):Boolean;
{Note: Caller must hold the interrupt lock}
var
 Offset:LongWord;
 Mask:LongWord;
begin
 {}
 Result:=False;

 {Check GIC}
 if GIC = nil then Exit;

 {Get Offset and Mask values}
 Offset:=(Entry.Number div 32) * 4;
 Mask:=1 shl (Entry.Number mod 32);

 {Enable Interrupt}
 PLongWord(GIC.DistAddress + ARM_GICD_ICENABLER0 + Offset)^:=Mask;

 Result:=True;
end;

{==============================================================================}

function ARMGICGetCurrentCount(GIC:PGICDevice;CPUID,Number:LongWord):LongWord;
{Note: Caller must hold the interrupt lock}
var
 Entry:PInterruptEntry;
begin
 {}
 Result:=0;

 {Check GIC}
 if GIC = nil then Exit;

 {Setup Defaults}
 Entry:=nil;

 {Check Number}
 if ARMGICIsGlobal(GIC,Number) then
  begin
   {Count Global}
   Entry:=GIC.Interrupts[Number - ARM_GIC_SPI_BASE];
  end
 else if ARMGICIsLocal(GIC,Number) then
  begin
   {Check CPU}
   if CPUID > GIC.CPUCount - 1 then Exit;

   {Count Local}
   Entry:=GIC.LocalInterrupts[CPUID][Number - ARM_GIC_PPI_BASE];
  end
 else if ARMGICIsSoftware(GIC,Number) then
  begin
   {Check CPU}
   if CPUID > GIC.CPUCount - 1 then Exit;

   {Count Software}
   Entry:=GIC.SoftwareInterrupts[CPUID][Number - ARM_GIC_SGI_BASE];
  end;

 {Count Entries}
 while Entry <> nil do
  begin
   Inc(Result);

   {Get Next}
   Entry:=Entry.Next;
  end;
end;

{==============================================================================}

function ARMGICGetCurrentEntry(GIC:PGICDevice;CPUID,Number:LongWord;Index:LongWord):PInterruptEntry;
{Note: Caller must hold the interrupt lock (or be within an interrupt handler)}
var
 Count:LongWord;
 Entry:PInterruptEntry;
begin
 {}
 Result:=nil;

 {Check GIC}
 if GIC = nil then Exit;

 {Setup Defaults}
 Entry:=nil;

 {Check Number}
 if ARMGICIsGlobal(GIC,Number) then
  begin
   {Get Global}
   Entry:=GIC.Interrupts[Number - ARM_GIC_SPI_BASE];
  end
 else if ARMGICIsLocal(GIC,Number) then
  begin
   {Check CPU}
   if CPUID > GIC.CPUCount - 1 then Exit;

   {Get Local}
   Entry:=GIC.LocalInterrupts[CPUID][Number - ARM_GIC_PPI_BASE];
  end
 else if ARMGICIsSoftware(GIC,Number) then
  begin
   {Check CPU}
   if CPUID > GIC.CPUCount - 1 then Exit;

   {Get Software}
   Entry:=GIC.SoftwareInterrupts[CPUID][Number - ARM_GIC_SGI_BASE];
  end;

 {Get Entry}
 Count:=0;
 while Entry <> nil do
  begin
   {Check Count}
   if Count = Index then
    begin
     Result:=Entry;
     Exit;
    end;

   Inc(Count);

   {Get Next}
   Entry:=Entry.Next;
  end;
end;

{==============================================================================}

function ARMGICAddCurrentEntry(GIC:PGICDevice;CPUID,Number:LongWord;Entry:PInterruptEntry):Boolean;
{Note: Caller must hold the interrupt lock}
var
 Current:PInterruptEntry;
begin
 {}
 Result:=False;

 {Check GIC}
 if GIC = nil then Exit;

 {Check Entry}
 if Entry = nil then Exit;

 {Check Number}
 if ARMGICIsGlobal(GIC,Number) then
  begin
   {Add Global}
   Current:=GIC.Interrupts[Number - ARM_GIC_SPI_BASE];
   if Current = nil then
    begin
     {Set Global}
     GIC.Interrupts[Number - ARM_GIC_SPI_BASE]:=Entry;

     Result:=True;
    end;
  end
 else if ARMGICIsLocal(GIC,Number) then
  begin
   {Check CPU}
   if CPUID > GIC.CPUCount - 1 then Exit;

   {Add Local}
   Current:=GIC.LocalInterrupts[CPUID][Number - ARM_GIC_PPI_BASE];
   if Current = nil then
    begin
      {Set Local}
      GIC.LocalInterrupts[CPUID][Number - ARM_GIC_PPI_BASE]:=Entry;

      Result:=True;
    end;
  end
 else if ARMGICIsSoftware(GIC,Number) then
  begin
   {Check CPU}
   if CPUID > GIC.CPUCount - 1 then Exit;

   {Add Software}
   Current:=GIC.SoftwareInterrupts[CPUID][Number - ARM_GIC_SGI_BASE];
   if Current = nil then
    begin
     {Set Software}
     GIC.SoftwareInterrupts[CPUID][Number - ARM_GIC_SGI_BASE]:=Entry;

     Result:=True;
    end;
  end;

 {Check Current}
 if Current <> nil then
  begin
   {Find last}
   while Current.Next <> nil do
    begin
     {Get Next}
     Current:=Current.Next;
    end;

   {Add to end of list}
   Current.Next:=Entry;
   Entry.Prev:=Current;
   Entry.Next:=nil;

   Result:=True;
  end;
end;

{==============================================================================}

function ARMGICDeleteCurrentEntry(GIC:PGICDevice;CPUID,Number:LongWord;Entry:PInterruptEntry):Boolean;
{Note: Caller must hold the interrupt lock}
var
 Current:PInterruptEntry;
begin
 {}
 Result:=False;

 {Check GIC}
 if GIC = nil then Exit;

 {Check Entry}
 if Entry = nil then Exit;

 {Check Number}
 if ARMGICIsGlobal(GIC,Number) then
  begin
   {Delete Global}
   Current:=GIC.Interrupts[Number - ARM_GIC_SPI_BASE];
   if Current = Entry then
    begin
     GIC.Interrupts[Number - ARM_GIC_SPI_BASE]:=nil;
    end;
  end
 else if ARMGICIsLocal(GIC,Number) then
  begin
   {Check CPU}
   if CPUID > GIC.CPUCount - 1 then Exit;

   {Delete Local}
   Current:=GIC.LocalInterrupts[CPUID][Number - ARM_GIC_PPI_BASE];
   if Current = Entry then
    begin
     GIC.LocalInterrupts[CPUID][Number - ARM_GIC_PPI_BASE]:=nil;
    end;
  end
 else if ARMGICIsSoftware(GIC,Number) then
  begin
   {Check CPU}
   if CPUID > GIC.CPUCount - 1 then Exit;

   {Delete Software}
   Current:=GIC.SoftwareInterrupts[CPUID][Number - ARM_GIC_SGI_BASE];
   if Current = Entry then
    begin
     GIC.SoftwareInterrupts[CPUID][Number - ARM_GIC_SGI_BASE]:=nil;
    end;
  end;

 {Check Current}
 if Current <> nil then
  begin
   {Find Entry}
   while Current <> nil do
    begin
     if Current = Entry then
      begin
       Break;
      end;

     {Get Next}
     Current:=Current.Next;
    end;

   {Check Current}
   if Current <> nil then
    begin
     {Remove from list}
     if Current.Prev <> nil then
      begin
       Current.Prev.Next:=Current.Next;
      end;
     if Current.Next <> nil then
      begin
       Current.Next.Prev:=Current.Prev;
      end;
     Current.Prev:=nil;
     Current.Next:=nil;

     {Free Entry}
     FreeMem(Current);

     Result:=True;
    end;
  end;
end;

{==============================================================================}

function ARMGICFindMatchingEntry(GIC:PGICDevice;const Entry:TInterruptEntry):PInterruptEntry;
{Note: Caller must hold the interrupt lock}
var
 Current:PInterruptEntry;
begin
 {}
 Result:=nil;

 {Check GIC}
 if GIC = nil then Exit;

 {Get Current}
 Current:=ARMGICGetCurrentEntry(GIC,Entry.CPUID,Entry.Number,0);

 {Find Match}
 while Current <> nil do
  begin
   if ARMGICCompareHandlers(GIC,Entry,Current^) then
    begin
     if Entry.Parameter = Current.Parameter then
      begin
       Result:=Current;
       Exit;
      end;
    end;

   {Get Next}
   Current:=Current.Next;
  end;
end;

{==============================================================================}
{==============================================================================}

{initialization}
 {Nothing}

{==============================================================================}

{finalization}
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
