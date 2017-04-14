{
Ultibo Global Define Defaults.

Copyright (C) 2015 - SoftOz Pty Ltd.

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
{--$DEFINE CONSOLE_EARLY_INIT}    {Enable early initialization of console output, useful for debugging (Default: Off)}
{--$DEFINE LOGGING_EARLY_INIT}    {Enable early initialization of logging output, useful for debugging (Default: Off)}

{$IFDEF LOGGING_EARLY_INIT}
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

{$DEFINE LIST_INLINE}             {Inline parts of List handling for performance (Default: On)}
{$DEFINE QUEUE_INLINE}            {Inline parts of Queue handling for performance (Default: On)}

{==============================================================================}
{==============================================================================}
{System statistics definitions}
{--$DEFINE IRQ_STATISTICS}        {Enable statistics collection for IRQ handling (Default: Off)}
{--$DEFINE FIQ_STATISTICS}        {Enable statistics collection for FIQ handling (Default: Off)}
{--$DEFINE SWI_STATISTICS}        {Enable statistics collection for SWI handling (Default: Off)}

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
{$DEFINE LOADER_DEBUG}          {Enable debug output/counters for loader internals (Default: Off)}
{--$DEFINE CONSOLE_DEBUG}         {Enable debug output/counters for console devices (Default: Off) (Do not enable when console logging is active)}
{--$DEFINE LOGGING_DEBUG}         {Enable debug output/counters for logging devices (Default: Off)}

{--$DEFINE DEVICE_DEBUG}          {Enable debug output/counters for device management (Default: Off)}
{--$DEFINE DMA_DEBUG}             {Enable debug output/counters for DMA devices/hosts (Default: Off)}
{--$DEFINE USB_DEBUG}             {Enable debug output/counters for USB devices/drivers/hosts (Default: Off) (Do not enable when network or file logging is active)}
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
{--$DEFINE SMSC95XX_DEBUG}        {Enable debug output/counters for SMSC95XX driver (Default: Off)}
{--$DEFINE BCM2708_DEBUG}         {Enable debug output/counters for BCM2708 drivers (Default: Off)}
{--$DEFINE BCM2709_DEBUG}         {Enable debug output/counters for BCM2709 drivers (Default: Off)}
{--$DEFINE BCM2710_DEBUG}         {Enable debug output/counters for BCM2710 drivers (Default: Off)}

{DMA}
{--$DEFINE PL08X_DEBUG}           {Enable debug output/counters for PL08X driver (Default: Off)}

{Network}
{--$DEFINE SMC91X_DEBUG}          {Enable debug output/counters for SMC91X driver (Default: Off)}

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
{--$DEFINE RPIFT5406_DEBUG}       {Enable debug output/counters for RPIFT5406 driver (Default: Off)}

{Framebuffer/Console}
{--$DEFINE HD44780_DEBUG}         {Enable debug output/counters for HD44780 driver (Default: Off)}
{--$DEFINE ILI9340_DEBUG}         {Enable debug output/counters for ILI9340 driver (Default: Off)}
{--$DEFINE HX8357D_DEBUG}         {Enable debug output/counters for HX8357D driver (Default: Off)}
{--$DEFINE PL110_DEBUG}           {Enable debug output/counters for PL110 driver (Default: Off)}

{Framebuffer/Console}
{--$DEFINE AF16X2LCD_DEBUG}       {Enable debug output/counters for AF16X2LCD driver (Default: Off)}
{--$DEFINE PITFT28_DEBUG}         {Enable debug output/counters for PITFT28 driver (Default: Off)}
{--$DEFINE PITFT35_DEBUG}         {Enable debug output/counters for PITFT35 driver (Default: Off)}

{Keyboard/Mouse}
{--$DEFINE PL050_DEBUG}           {Enable debug output/counters for PL050 driver (Default: Off)}

{MMC/SDHCI}
{--$DEFINE PL18X_DEBUG}           {Enable debug output/counters for PL18X driver (Default: Off)}

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
{--$DEFINE SYSCALLS_LARGE64_FILES}    {Enable support for 64bit file access in Syscalls unit (Default: Off)} 
                                      {Note: Not currently supported by Newlib, need to modify \newlib\configure.host to enable stdio64 support for arm-none-eabi or aarch64-none-elf}
{==============================================================================}
{==============================================================================}
{Loader definitions}
{$DEFINE LOADER_EXPORT_SYSTEM}      {Enable the library loader to export symbols for System unit (Default: On)} 
{--$DEFINE LOADER_EXPORT_OBJPAS}    {Enable the library loader to export symbols for ObjPas unit (Default: Off)} 
{--$DEFINE LOADER_EXPORT_CLASSES}   {Enable the library loader to export symbols for Classes unit (Default: Off)} 
{--$DEFINE LOADER_EXPORT_SYSUTILS}  {Enable the library loader to export symbols for SysUtils unit (Default: Off)} 

{--$DEFINE LOADER_EXPORT_HEAP}      {Enable the library loader to export symbols for Heap unit (Default: Off)}
{--$DEFINE LOADER_EXPORT_THREADS}   {Enable the library loader to export symbols for Threads unit (Default: Off)} 
{--$DEFINE LOADER_EXPORT_PLATFORM}  {Enable the library loader to export symbols for Platform unit (Default: Off)} 

{--$DEFINE LOADER_EXPORT_DEVICE}    {Enable the library loader to export symbols for Device unit (Default: Off)} 
{--$DEFINE LOADER_EXPORT_CONSOLE}   {Enable the library loader to export symbols for Console unit (Default: Off)} 
{--$DEFINE LOADER_EXPORT_LOGGING}   {Enable the library loader to export symbols for Logging unit (Default: Off)} 
{--$DEFINE LOADER_EXPORT_DMA}       {Enable the library loader to export symbols for DMA unit (Default: Off)} 
{--$DEFINE LOADER_EXPORT_USB}       {Enable the library loader to export symbols for USB unit (Default: Off)} 
{--$DEFINE LOADER_EXPORT_MMC}       {Enable the library loader to export symbols for MMC unit (Default: Off)} 
{--$DEFINE LOADER_EXPORT_SPI}       {Enable the library loader to export symbols for SPI unit (Default: Off)} 
{--$DEFINE LOADER_EXPORT_I2C}       {Enable the library loader to export symbols for I2C unit (Default: Off)} 
{--$DEFINE LOADER_EXPORT_PWM}       {Enable the library loader to export symbols for PWM unit (Default: Off)} 
{--$DEFINE LOADER_EXPORT_PCM}       {Enable the library loader to export symbols for PCM unit (Default: Off)} 
{--$DEFINE LOADER_EXPORT_GPIO}      {Enable the library loader to export symbols for GPIO unit (Default: Off)} 
{--$DEFINE LOADER_EXPORT_UART}      {Enable the library loader to export symbols for UART unit (Default: Off)} 

{==============================================================================}
{==============================================================================}
{Crypto definitions}
{--$DEFINE CRYPTO_DES_PRECOMPUTED_PERMUTATIONS}  {Enable precomputed permutations (IP/FP) for DES cipher (Faster but bigger) (Default: Off)}
{--$DEFINE CRYPTO_AES_PRECOMPUTED_TABLES}        {Enable precomputed tables (TE1/TE2/TE3/TE4/TD1/TD2/TD3/TD4 etc) for AES cipher (Faster but bigger) (Default: Off)}

{==============================================================================}
{==============================================================================}
