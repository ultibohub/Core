{
Ultibo Global Define Defaults.

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


Global Defines
==============


}

{==============================================================================}
{==============================================================================}
{System init definitions}
{--$DEFINE UART_EARLY_INIT}       {Enable early initialization of UART output, useful for debugging (Default: Off)}
{--$DEFINE CONSOLE_EARLY_INIT}    {Enable early initialization of console output, useful for debugging (Default: Off)}
{--$DEFINE LOGGING_EARLY_INIT}    {Enable early initialization of logging output, useful for debugging (Default: Off)}

{$IFDEF LOGGING_EARLY_INIT}
 {--$DEFINE UART_EARLY_INIT}
 {$DEFINE CONSOLE_EARLY_INIT}
{$ENDIF}

{==============================================================================}
{==============================================================================}
{System inlining definitions}

{==============================================================================}
{==============================================================================}
{Subsystem inlining definitions}
{$DEFINE SPIN_INLINE}             {Inline parts of Spin lock handling for performance (Default: On)}
{$DEFINE MUTEX_INLINE}            {Inline parts of Mutex handling for performance (Default: On)}
{$DEFINE CRITICALSECTION_INLINE}  {Inline parts of Critical Section handling for performance (Default: On)}
{$DEFINE SEMAPHORE_INLINE}        {Inline parts of Semaphore handling for performance (Default: On)}
{$DEFINE SYNCHRONIZER_INLINE}     {Inline parts of Synchronizer handling for performance (Default: On)}
{$DEFINE CONDITION_INLINE}        {Inline parts of Condition handling for performance (Default: On)}
{$DEFINE COMPLETION_INLINE}       {Inline parts of Completion handling for performance (Default: On)}
{$DEFINE LIST_INLINE}             {Inline parts of List handling for performance (Default: On)}
{$DEFINE QUEUE_INLINE}            {Inline parts of Queue handling for performance (Default: On)}
{$DEFINE THREAD_INLINE}           {Inline parts of Thread handling for performance (Default: On)}
{$DEFINE MESSAGESLOT_INLINE}      {Inline parts of Messageslot handling for performance (Default: On)}
{$DEFINE MAILSLOT_INLINE}         {Inline parts of Mailslot handling for performance (Default: On)}
{$DEFINE BUFFER_INLINE}           {Inline parts of Buffer handling for performance (Default: On)}
{$DEFINE EVENT_INLINE}            {Inline parts of Event handling for performance (Default: On)}
{$DEFINE TIMER_INLINE}            {Inline parts of Timer handling for performance (Default: On)}
{$DEFINE WORKER_INLINE}           {Inline parts of Worker handling for performance (Default: On)}
{$DEFINE TASKER_INLINE}           {Inline parts of Tasker handling for performance (Default: On)}

{==============================================================================}
{==============================================================================}
{System function definitions}
{--$DEFINE CLOCK_TICK_MANUAL}         {Enable use of the manual clock tick algorithm (Default: Off)}
{--$DEFINE SCHEDULER_YIELD_ALTERNATE} {Enable use of the alternate scheduler yield algorithm (Default: Off)}

{==============================================================================}
{==============================================================================}
{System statistics definitions}
{$DEFINE IRQ_STATISTICS}        {Enable statistics collection for IRQ handling (Default: On)}
{$DEFINE FIQ_STATISTICS}        {Enable statistics collection for FIQ handling (Default: On)}
{--$DEFINE SWI_STATISTICS}        {Enable statistics collection for SWI handling (Default: Off)}
{--$DEFINE IPI_STATISTICS}        {Enable statistics collection for IPI handling (Default: Off)}

{--$DEFINE HEAP_STATISTICS}       {Enable statistics collection for heap manager internals (Default: Off)}
{--$DEFINE LOCK_STATISTICS}       {Enable statistics collection for lock primitive internals (Default: Off)}
{$DEFINE THREAD_STATISTICS}       {Enable statistics collection for thread times and counts (Default: On)}
{--$DEFINE DEVICE_STATISTICS}     {Enable statistics collection for device usage (Default: Off)}
{--$DEFINE PLATFORM_STATISTICS}   {Enable statistics collection for platform internals (Default: Off)}

{==============================================================================}
{==============================================================================}
{System debug definitions}
{--$DEFINE IRQ_DEBUG}             {Enable debug output/counters for IRQ handling (Default: Off)}
{--$DEFINE FIQ_DEBUG}             {Enable debug output/counters for FIQ handling (Default: Off)}
{--$DEFINE SWI_DEBUG}             {Enable debug output/counters for SWI handling (Default: Off)}

{$IF DEFINED(IRQ_DEBUG) or DEFINED(FIQ_DEBUG) or DEFINED(SWI_DEBUG)}
 {$DEFINE INTERRUPT_DEBUG}
{$ENDIF}

{--$DEFINE HEAP_DEBUG}            {Enable debug output/counters for heap manager (Default: Off)}
{--$DEFINE THREAD_DEBUG}          {Enable debug output/counters for thread internals (Default: Off)}
{--$DEFINE PLATFORM_DEBUG}        {Enable debug output/counters for platform internals (Default: Off)}
{--$DEFINE SYSCALLS_DEBUG}        {Enable debug output/counters for C library syscalls (Default: Off)}
{--$DEFINE DEVICE_TREE_DEBUG}     {Enable debug output/counters for device tree internals (Default: Off)}
{$DEFINE API_DEBUG}             {Enable debug output/counters for library API internals (Default: Off)}
{$DEFINE LOADER_DEBUG}          {Enable debug output/counters for dynamic loader internals (Default: Off)}
{--$DEFINE CONSOLE_DEBUG}         {Enable debug output/counters for console devices (Default: Off) (Do not enable when console logging is active)}
{--$DEFINE LOGGING_DEBUG}         {Enable debug output/counters for logging devices (Default: Off)}

{--$DEFINE DEVICE_DEBUG}          {Enable debug output/counters for device management (Default: Off)}
{--$DEFINE DMA_DEBUG}             {Enable debug output/counters for DMA devices/hosts (Default: Off)}
{--$DEFINE USB_DEBUG}             {Enable debug output/counters for USB devices/drivers/hosts (Default: Off) (Do not enable when network or file logging is active)}
{--$DEFINE PCI_DEBUG}             {Enable debug output/counters for PCI devices/drivers/hosts (Default: Off) (Do not enable when network or file logging is active)}
{--$DEFINE MMC_DEBUG}             {Enable debug output/counters for MMC/SD devices/hosts (Default: Off)}
{--$DEFINE SPI_DEBUG}             {Enable debug output/counters for SPI devices (Default: Off)}
{--$DEFINE I2C_DEBUG}             {Enable debug output/counters for I2C devices (Default: Off)}
{--$DEFINE PWM_DEBUG}             {Enable debug output/counters for PWM devices (Default: Off)}
{--$DEFINE PCM_DEBUG}             {Enable debug output/counters for PCM devices (Default: Off)}
{--$DEFINE GPIO_DEBUG}            {Enable debug output/counters for GPIO devices (Default: Off)}
{--$DEFINE UART_DEBUG}            {Enable debug output/counters for UART devices (Default: Off) (Do not enable when serial logging is active)}
{--$DEFINE BLUETOOTH_DEBUG}       {Enable debug output/counters for Bluetooth devices/drivers/hosts (Default: Off)}
{--$DEFINE FRAMEBUFFER_DEBUG}     {Enable debug output/counters for Framebuffer devices (Default: Off)}

{--$DEFINE STORAGE_DEBUG}         {Enable debug output/counters for Storage devices (Default: Off)}
{$IFDEF STORAGE_DEBUG}
 {$DEFINE ATA_DEBUG}
 {$DEFINE SCSI_DEBUG}
 {$DEFINE ATAPI_DEBUG}
{$ENDIF}

{--$DEFINE FILESYS_DEBUG}         {Enable debug output/counters for Filesystem services (Default: Off)}
{$IFDEF FILESYS_DEBUG} 
 {$DEFINE FAT_DEBUG}
 {$DEFINE CDFS_DEBUG}
 {$DEFINE NTFS_DEBUG}
 {$DEFINE EXTFS_DEBUG} 
 {$DEFINE HPFS_DEBUG} 
 {$DEFINE NSS_DEBUG} 
 {$DEFINE NWFS_DEBUG} 
 {$DEFINE VIRTUAL_DEBUG}
 {$DEFINE CACHE_DEBUG} 
{$ENDIF}

{--$DEFINE NETWORK_DEBUG}         {Enable debug output/counters for Network services (Default: Off) (Do not enable when network logging is active)}
{$IFDEF NETWORK_DEBUG}
 {$DEFINE TRANSPORT_DEBUG}
 {$DEFINE PROTOCOL_DEBUG}
 {$DEFINE SOCKET_DEBUG}
 {$DEFINE WINSOCK_DEBUG}
 {$DEFINE WINSOCK2_DEBUG}
 {$DEFINE LOOPBACK_DEBUG}
 {$DEFINE ARP_DEBUG}
 {$DEFINE IP_DEBUG}
 {$DEFINE IP6_DEBUG}
 {$DEFINE ICMP_DEBUG}
 {$DEFINE ICMP6_DEBUG}
 {$DEFINE IGMP_DEBUG}
 {$DEFINE UDP_DEBUG} 
 {$DEFINE TCP_DEBUG} 
 {$DEFINE RAW_DEBUG} 
 {$DEFINE DHCP_DEBUG} 
 {$DEFINE DNS_DEBUG} 
{$ENDIF}

{--$DEFINE KEYBOARD_DEBUG}        {Enable debug output/counters for Keyboard devices (Default: Off)}
{--$DEFINE MOUSE_DEBUG}           {Enable debug output/counters for Mouse devices (Default: Off)}
{--$DEFINE TOUCH_DEBUG}           {Enable debug output/counters for Touch devices (Default: Off)}

{$DEFINE WIFI_DEBUG}            {Enable debug output/counters for WiFi devices (Default: Off)}
{--$DEFINE VPN_DEBUG}             {Enable debug output/counters for VPN devices (Default: Off)}

{--$DEFINE SERIAL_DEBUG}          {Enable debug output/counters for Serial devices (Default: Off) (Do not enable when serial logging is active)}

{--$DEFINE CODEC_DEBUG}           {Enable debug output/counters for Codec devices (Default: Off)}
{--$DEFINE VIDEO_DEBUG}           {Enable debug output/counters for Video devices (Default: Off)}
{--$DEFINE AUDIO_DEBUG}           {Enable debug output/counters for Audio devices (Default: Off)}

{--$DEFINE RTC_DEBUG}           {Enable debug output/counters for RTC devices (Default: Off)}

{==============================================================================}
{==============================================================================}
{Exception debug definitions}
{--$DEFINE EXCEPTION_DEBUG}       {Enable debug output/counters for Exception handling (Default: Off)}
{--$DEFINE DATA_ABORT_DEBUG}
{--$DEFINE PREFETCH_ABORT_DEBUG}
{--$DEFINE UNDEFINED_INSTRUCTION_DEBUG}

{==============================================================================}
{==============================================================================}
{Subsystem debug definitions}
{--$DEFINE BOOT_DEBUG}            {Enable debug output/counters for startup sequence (Default: Off)}
{--$DEFINE LOCK_DEBUG}            {Enable debug output/counters for Lock handling (Default: Off)}
{--$DEFINE SPIN_DEBUG}            {Enable debug output/counters for Spin lock handling (Default: Off)}
{--$DEFINE MUTEX_DEBUG}           {Enable debug output/counters for Mutex handling (Default: Off)}
{--$DEFINE CLOCK_DEBUG}           {Enable debug output/counters for Clock handling (Default: Off)}
{--$DEFINE SCHEDULER_DEBUG}       {Enable debug output/counters for Scheduler internals (Default: Off)}
{--$DEFINE INTERRUPT_DEBUG}       {Enable debug output/counters for Interrupt handling (Default: Off)}

{==============================================================================}
{==============================================================================}
{Device debug definitions}
{USB/PCI/SoC}
{--$DEFINE AHCI_DEBUG}            {Enable debug output/counters for AHCI driver (Default: Off)}
{--$DEFINE EHCI_DEBUG}            {Enable debug output/counters for EHCI driver (Default: Off)}
{--$DEFINE XHCI_DEBUG}            {Enable debug output/counters for XHCI driver (Default: Off)}
{--$DEFINE DWCOTG_DEBUG}          {Enable debug output/counters for DWCOTG driver (Default: Off)}
{--$DEFINE BCM2708_DEBUG}         {Enable debug output/counters for BCM2708 drivers (Default: Off)}
{--$DEFINE BCM2709_DEBUG}         {Enable debug output/counters for BCM2709 drivers (Default: Off)}
{--$DEFINE BCM2710_DEBUG}         {Enable debug output/counters for BCM2710 drivers (Default: Off)}
{--$DEFINE BCM2711_DEBUG}         {Enable debug output/counters for BCM2711 drivers (Default: Off)}

{DMA}
{--$DEFINE PL08X_DEBUG}           {Enable debug output/counters for PL08X driver (Default: Off)}

{Network}
{--$DEFINE SMSC95XX_DEBUG}        {Enable debug output/counters for SMSC95XX driver (Default: Off)}
{--$DEFINE SMC91X_DEBUG}          {Enable debug output/counters for SMC91X driver (Default: Off)}
{--$DEFINE LAN78XX_DEBUG}         {Enable debug output/counters for SMSC95XX driver (Default: Off)}
{--$DEFINE AX88XXX_DEBUG}         {Enable debug output/counters for AX88XXX driver (Default: Off)}
{--$DEFINE GENET_DEBUG}           {Enable debug output/counters for GENET driver (Default: Off)}
{--$DEFINE CDCETHERNET_DEBUG}     {Enable debug output/counters for CDCEthernet driver (Default: Off)}

{WiFi}
{$DEFINE RTL8188EU_DEBUG}       {Enable debug output/counters for RTL8188EU driver (Default: Off)}
{$DEFINE RTL8192CU_DEBUG}       {Enable debug output/counters for RTL8192CU driver (Default: Off)}
{--$DEFINE RT2800USB_DEBUG}       {Enable debug output/counters for RT2800USB driver (Default: Off)}
     
{Serial/UART}     
{--$DEFINE PL2303_DEBUG}          {Enable debug output/counters for PL2303 driver (Default: Off)}
{--$DEFINE CDCACM_DEBUG}          {Enable debug output/counters for USB CDC ACM driver (Default: Off)}
{--$DEFINE FTDISERIAL_DEBUG}      {Enable debug output/counters for FTDI Serial driver (Default: Off)}
{--$DEFINE PL011_DEBUG}           {Enable debug output/counters for PL011 driver (Default: Off)}

{RTC}
{--$DEFINE DS1307_DEBUG}          {Enable debug output/counters for DS1307 driver (Default: Off)}
{--$DEFINE PL031_DEBUG}           {Enable debug output/counters for PL031 driver (Default: Off)}

{GPIO}
{--$DEFINE MCP230XX_DEBUG}        {Enable debug output/counters for MCP230XX driver (Default: Off)}
{--$DEFINE STMPE_DEBUG}           {Enable debug output/counters for STMPE driver (Default: Off)}
{--$DEFINE RPIGPIOEXP_DEBUG}      {Enable debug output/counters for RPiGPIO Expander driver (Default: Off)}
{--$DEFINE REMOTEGPIO_DEBUG}      {Enable debug output/counters for RemoteGPIO driver (Default: Off)}
{--$DEFINE PCF857X_DEBUG}         {Enable debug output/counters for PCF857X driver (Default: Off)}

{Framebuffer/Console}
{--$DEFINE HD44780_DEBUG}         {Enable debug output/counters for HD44780 driver (Default: Off)}
{--$DEFINE ILI9340_DEBUG}         {Enable debug output/counters for ILI9340 driver (Default: Off)}
{--$DEFINE HX8357D_DEBUG}         {Enable debug output/counters for HX8357D driver (Default: Off)}
{--$DEFINE PL110_DEBUG}           {Enable debug output/counters for PL110 driver (Default: Off)}
{--$DEFINE MEMORYFB_DEBUG}        {Enable debug output/counters for Memory Framebuffer driver (Default: Off)}
{--$DEFINE MIRRORFB_DEBUG}        {Enable debug output/counters for Mirror Framebuffer driver (Default: Off)}
{--$DEFINE REMOTEFB_DEBUG}        {Enable debug output/counters for Remote Framebuffer driver (Default: Off)}

{Framebuffer/Console}
{--$DEFINE AF16X2LCD_DEBUG}       {Enable debug output/counters for AF16X2LCD driver (Default: Off)}
{--$DEFINE PITFT28_DEBUG}         {Enable debug output/counters for PITFT28 driver (Default: Off)}
{--$DEFINE PITFT35_DEBUG}         {Enable debug output/counters for PITFT35 driver (Default: Off)}

{Keyboard/Mouse/Touch}
{--$DEFINE PL050_DEBUG}           {Enable debug output/counters for PL050 driver (Default: Off)}
{--$DEFINE RPIFT5406_DEBUG}       {Enable debug output/counters for RPIFT5406 driver (Default: Off)}

{MMC/SDHCI}
{--$DEFINE PL18X_DEBUG}           {Enable debug output/counters for PL18X driver (Default: Off)}
{--$DEFINE BCMSDHOST_DEBUG}       {Enable debug output/counters for PL18X driver (Default: Off)}

{GPU}
{--$DEFINE VC4_DEBUG}             {Enable debug output/counters for VC4 interface (Default: Off)}
{--$DEFINE VC4VCHIQ_DEBUG}        {Enable debug output/counters for VC4 VCHIQ driver (Default: Off)}
{--$DEFINE VC4VCSM_DEBUG}         {Enable debug output/counters for VC4 VCSM driver (Default: Off)}
{--$DEFINE VC4V3D_DEBUG}          {Enable debug output/counters for VC4 V3D interface (Default: Off)}

{Other}
{--$DEFINE RPIOTP_DEBUG}          {Enable debug output/counters for RPIOTP interface (Default: Off)}
{--$DEFINE RPIUSBBOOT_DEBUG}      {Enable debug output/counters for RPIUSBBOOT driver (Default: Off)}
{--$DEFINE RPISENSEHAT_DEBUG}     {Enable debug output/counters for RPISENSEHAT driver (Default: Off)}

{==============================================================================}
{==============================================================================}
{Service debug definitions}
{--$DEFINE SERVICE_DEBUG}         {Enable debug output/counters for services (NTP/SysLog/Telnet/SSH) (Default: Off)}
{--$DEFINE PING_DEBUG}            {Enable debug output/counters for Ping service (Default: Off)}
{--$DEFINE NTP_DEBUG}             {Enable debug output/counters for NTP service (Default: Off)}
{--$DEFINE TELNET_DEBUG}          {Enable debug output/counters for Telnet service (Default: Off)}
{--$DEFINE SYSLOG_DEBUG}          {Enable debug output/counters for SysLog service (Default: Off)}
{--$DEFINE SSH_DEBUG}             {Enable debug output/counters for SSH service (Default: Off)}

{--$DEFINE CIFS_DEBUG}            {Enable debug output/counters for CIFS provider (Default: Off)}
{--$DEFINE NFS_DEBUG}             {Enable debug output/counters for NFS provider (Default: Off)}
{--$DEFINE RPC_DEBUG}             {Enable debug output/counters for RPC provider (Default: Off)}

{--$DEFINE HTTP_DEBUG}            {Enable debug output/counters for HTTP client and server (Default: Off)}
{--$DEFINE SMTP_DEBUG}            {Enable debug output/counters for SMTP client and server (Default: Off)}
{--$DEFINE POP3_DEBUG}            {Enable debug output/counters for POP3 client and server (Default: Off)}
{--$DEFINE IMAP4_DEBUG}           {Enable debug output/counters for IMAP4 client and server (Default: Off)}
{--$DEFINE FTP_DEBUG}             {Enable debug output/counters for FTP client and server (Default: Off)}

{--$DEFINE SHELL_DEBUG}           {Enable debug output/counters for Shell modules (Default: Off)}

{==============================================================================}
{==============================================================================}
{Debug lock definitions}
{$IFDEF IRQ_DEBUG}
 {$DEFINE HEAP_LOCK_IRQ}
 {$DEFINE LOGGING_LOCK_IRQ}
{$ENDIF}

{$IFDEF FIQ_DEBUG}
 {$DEFINE HEAP_LOCK_IRQFIQ}
 {$DEFINE LOGGING_LOCK_IRQFIQ} 
{$ENDIF}

{$IFDEF SWI_DEBUG}
 {$DEFINE HEAP_LOCK_IRQFIQ} 
 {$DEFINE LOGGING_LOCK_IRQFIQ} 
{$ENDIF}

{$IFDEF HEAP_DEBUG}
 {$DEFINE HEAP_LOCK_IRQFIQ} 
 {$DEFINE LOGGING_LOCK_IRQFIQ} 
{$ENDIF}

{$IFDEF THREAD_DEBUG}
 {$DEFINE HEAP_LOCK_IRQFIQ} 
 {$DEFINE LOGGING_LOCK_IRQFIQ} 
{$ENDIF}

{$IFDEF PLATFORM_DEBUG}
 {$DEFINE HEAP_LOCK_IRQFIQ} 
 {$DEFINE LOGGING_LOCK_IRQFIQ} 
{$ENDIF}

{==============================================================================}
{==============================================================================}
{Syscalls definitions}
{--$DEFINE SYSCALLS_LARGE64_FILES}   {Enable support for 64bit file access in Syscalls unit (Default: Off)} 
                                     {Note: Not currently supported by Newlib, need to modify \newlib\configure.host to enable stdio64 support for arm-none-eabi or aarch64-none-elf}
{$DEFINE SYSCALLS_EXPORT_SOCKETS}    {Enable export of POSIX standard socket functions by the Syscalls unit (Default: On)}
                                     {Note: Must be disabled in order to enable export of the Winsock or Winsock2 API below}
{$DEFINE SYSCALLS_USE_LONG_TIME_T}   {Define time_t as a long (32-bit) instead of int64_t (64-bit) for compatibility with older GCC and NewLib (Default: On)}                                  

{==============================================================================}
{==============================================================================}
{API definitions}
{$DEFINE API_EXPORT_PLATFORM}        {Enable the library API unit to export symbols for the Platform unit (Default: On)}
{$DEFINE API_EXPORT_THREADS}         {Enable the library API unit to export symbols for the Threads unit (Default: On)}
{$DEFINE API_EXPORT_HEAP}            {Enable the library API unit to export symbols for the HeapManager unit (Default: On)}

{$DEFINE API_EXPORT_DEVICE_TREE}     {Enable the library API unit to export symbols for the DeviceTree unit (Default: On)}
{$DEFINE API_EXPORT_DEVICES}         {Enable the library API unit to export symbols for the Devices unit (Default: On)}
{$DEFINE API_EXPORT_CONSOLE}         {Enable the library API unit to export symbols for the Console unit (Default: On)}
{$DEFINE API_EXPORT_LOGGING}         {Enable the library API unit to export symbols for the Logging unit (Default: On)}
{$DEFINE API_EXPORT_DMA}             {Enable the library API unit to export symbols for the DMA unit (Default: On)}
{$DEFINE API_EXPORT_PCI}             {Enable the library API unit to export symbols for the PCI unit (Default: On)}
{$DEFINE API_EXPORT_USB}             {Enable the library API unit to export symbols for the USB unit (Default: On)}
{$DEFINE API_EXPORT_MMC}             {Enable the library API unit to export symbols for the MMC unit (Default: On)}
{$DEFINE API_EXPORT_SPI}             {Enable the library API unit to export symbols for the SPI unit (Default: On)}
{$DEFINE API_EXPORT_I2C}             {Enable the library API unit to export symbols for the I2C unit (Default: On)}
{$DEFINE API_EXPORT_PWM}             {Enable the library API unit to export symbols for the PWM unit (Default: On)}
{$DEFINE API_EXPORT_PCM}             {Enable the library API unit to export symbols for the PCM unit (Default: On)}
{$DEFINE API_EXPORT_GPIO}            {Enable the library API unit to export symbols for the GPIO unit (Default: On)}
{$DEFINE API_EXPORT_UART}            {Enable the library API unit to export symbols for the UART unit (Default: On)}
{$DEFINE API_EXPORT_SERIAL}          {Enable the library API unit to export symbols for the Serial unit (Default: On)}
{$DEFINE API_EXPORT_RTC}             {Enable the library API unit to export symbols for the RTC unit (Default: On)}
{$DEFINE API_EXPORT_FONT}            {Enable the library API unit to export symbols for the Font unit (Default: On)}
{$DEFINE API_EXPORT_FRAMEBUFFER}     {Enable the library API unit to export symbols for the Framebuffer unit (Default: On)}
{$DEFINE API_EXPORT_GRAPHICSCONSOLE} {Enable the library API unit to export symbols for the GraphicsConsole unit (Default: On)}
{$DEFINE API_EXPORT_KEYMAP}          {Enable the library API unit to export symbols for the Keymap unit (Default: On)}
{$DEFINE API_EXPORT_KEYBOARD}        {Enable the library API unit to export symbols for the Keyboard unit (Default: On)}
{$DEFINE API_EXPORT_MOUSE}           {Enable the library API unit to export symbols for the Mouse unit (Default: On)}
{$DEFINE API_EXPORT_TOUCH}           {Enable the library API unit to export symbols for the Touch unit (Default: On)}
{$DEFINE API_EXPORT_STORAGE}         {Enable the library API unit to export symbols for the Storage unit (Default: On)}
{$DEFINE API_EXPORT_NETWORK}         {Enable the library API unit to export symbols for the Network unit (Default: On)}
{$DEFINE API_EXPORT_TIMEZONE}        {Enable the library API unit to export symbols for the Timezone unit (Default: On)}
{$DEFINE API_EXPORT_LOCALE}          {Enable the library API unit to export symbols for the Locale unit (Default: On)}
{$DEFINE API_EXPORT_UNICODE}         {Enable the library API unit to export symbols for the Unicode unit (Default: On)}
{$DEFINE API_EXPORT_TFTFRAMEBUFFER}  {Enable the library API unit to export symbols for the TFTFramebuffer unit (Default: On)}

{$DEFINE API_EXPORT_FILESYSTEM}      {Enable the library API unit to export symbols for the FileSystem unit (Default: On)}
{--$DEFINE API_EXPORT_WINSOCK}       {Enable the library API unit to export symbols for the Winsock unit (Default: Off)}
{--$DEFINE API_EXPORT_WINSOCK2}      {Enable the library API unit to export symbols for the Winsock2 unit (Default: Off)}
{--$DEFINE API_EXPORT_IPHLPAPI}      {Enable the library API unit to export symbols for the Iphlpapi unit (Default: Off)}

{$DEFINE API_EXPORT_ULTIBO}          {Enable the library API unit to export symbols for the Ultibo unit (Default: On)}

{API dependencies}
{$IFDEF API_EXPORT_CONSOLE}
 {$DEFINE API_EXPORT_DEVICES}
{$ENDIF}

{$IFDEF API_EXPORT_THREADS}
 {$DEFINE API_EXPORT_PLATFORM}
{$ENDIF}

{==============================================================================}
{==============================================================================}
{Architecture definitions}
{--$DEFINE ARMV6_IRQ_SAVE_VFP}        {Enable save and restore of VFP registers during IRQ processing on ARMv6 (Default: Off)}
{--$DEFINE ARMV6_FIQ_SAVE_VFP}        {Enable save and restore of VFP registers during FIQ processing on ARMv6 (Default: Off)}
{--$DEFINE ARMV6_SWI_SAVE_VFP}        {Enable save and restore of VFP registers during SWI processing on ARMv6 (Default: Off)}

{--$DEFINE ARMV7_IRQ_SAVE_VFP}        {Enable save and restore of VFP registers during IRQ processing on ARMv7 (Default: Off)}
{--$DEFINE ARMV7_FIQ_SAVE_VFP}        {Enable save and restore of VFP registers during FIQ processing on ARMv7 (Default: Off)}
{--$DEFINE ARMV7_SWI_SAVE_VFP}        {Enable save and restore of VFP registers during SWI processing on ARMv7 (Default: Off)}
{--$DEFINE ARMV7_CONTEXT_SWITCH_NEON} {Enable save and restore of the full NEON register set (D0-D31) during context switch on ARMv7 (Default: Off)}

{--$DEFINE ARMV8_IRQ_SAVE_VFP}        {Enable save and restore of VFP registers during IRQ processing on ARMv8 (Default: Off)}
{--$DEFINE ARMV8_FIQ_SAVE_VFP}        {Enable save and restore of VFP registers during FIQ processing on ARMv8 (Default: Off)}
{--$DEFINE ARMV8_SWI_SAVE_VFP}        {Enable save and restore of VFP registers during SWI processing on ARMv8 (Default: Off)}
{--$DEFINE ARMV8_CONTEXT_SWITCH_NEON} {Enable save and restore of the full NEON register set (D0-D31) during context switch on ARMv8 (Default: Off)}

{==============================================================================}
{==============================================================================}
{Device and Platform definitions}
{$DEFINE DEVICE_TREE_ENABLE}        {Enable use of device tree for configuration during boot (Default: On)}
{$DEFINE DEVICE_TREE_ENUMERATION}   {Enable support for device tree enumeration (Output to text) (Default: On)}

{$DEFINE DEVICE_NOTIFIER_RETRY}     {Enable asynchronous retry of device notifications (Default: On)}

{--$DEFINE USB_LEGACY_DEVICE_INIT}  {Enable old style USB device initialization (Default: Off)}

{$DEFINE PL011_UART_RX_BUFFER}      {Enable receive buffering for PL011 UART (Default: On)}
{$DEFINE BCM2708_UART0_RX_BUFFER}   {Enable receive buffering for BCM2708 UART0 (Default: On)}
{$DEFINE BCM2709_UART0_RX_BUFFER}   {Enable receive buffering for BCM2709 UART0 (Default: On)}
{$DEFINE BCM2710_UART0_RX_BUFFER}   {Enable receive buffering for BCM2710 UART0 (Default: On)}
{$DEFINE BCM2711_UART0_RX_BUFFER}   {Enable receive buffering for BCM2711 UART0 (Default: On)}

{--$DEFINE RPI2_CLOCK_SYSTEM_TIMER} {Use the System Timer for the Clock instead of the Virtual Timer on the RPi2 (Default: Off)}
{--$DEFINE RPI3_CLOCK_SYSTEM_TIMER} {Use the System Timer for the Clock instead of the Virtual Timer on the RPi3 (Default: Off)}
{--$DEFINE RPI4_CLOCK_SYSTEM_TIMER} {Use the System Timer for the Clock instead of the Virtual Timer on the RPi4 (Default: Off)}

{--$DEFINE RPI2_MAX_CLOCK_RATE}     {Enable use of the maximum clock rate (19.2MHz) instead of the default rate (1MHz) on the RPi2 (Default: Off)}
{--$DEFINE RPI3_MAX_CLOCK_RATE}     {Enable use of the maximum clock rate (19.2MHz) instead of the default rate (1MHz) on the RPi3 (Default: Off)}
{$DEFINE RPI4_MAX_CLOCK_RATE}       {Enable use of the maximum clock rate (54MHz) instead of the default rate (1MHz) on the RPi4 (Default: On)}

{$DEFINE RPI4_ENABLE_LPAE}          {Enable use of LPAE (Large Physical Address Extensions) on the RPi4 to access memory above 4GB (Default: On)}

{==============================================================================}
{==============================================================================}
{BigInt definitions}
{--$DEFINE BIGINT_DEBUG}                         {Enable debug output/functions for BitInt unit (Default: Off)}
{$DEFINE BIGINT_SQUARE}                          {Enable use of squaring instead of multiplication (Default: On)}
{$DEFINE BIGINT_SLIDING_WINDOW}                  {Enable use of Sliding-Window Exponentiation (Default: On)}
{$DEFINE BIGINT_CRT}                             {Enable use of the Chinese Remainder Theorem (Default: On)}
{$DEFINE BIGINT_BARRETT}                         {Enable use of the Barrett reduction algorithm (Default: On)}
{--$DEFINE BIGINT_MONTGOMERY}                    {Enable use of the Montgomery reduction algorithm (Default: Off)}

{ASN1 definitions}
{--$DEFINE ASN1_DEBUG}                           {Enable debug output/functions for ASN1 unit (Default: Off)}

{Crypto definitions}
{--$DEFINE CRYPTO_DEBUG}                         {Enable debug output/functions for Crypto unit (Default: Off)}
{--$DEFINE CRYPTO_DES_PRECOMPUTED_PERMUTATIONS}  {Enable precomputed permutations (IP/FP) for DES cipher (Faster but bigger) (Default: Off)}
{--$DEFINE CRYPTO_AES_PRECOMPUTED_TABLES}        {Enable precomputed tables (TE1/TE2/TE3/TE4/TD1/TD2/TD3/TD4 etc) for AES cipher (Faster but bigger) (Default: Off)}

{X509 definitions}
{--$DEFINE X509_DEBUG}                           {Enable debug output/functions for X509 unit (Default: Off)}

{==============================================================================}
{==============================================================================}
