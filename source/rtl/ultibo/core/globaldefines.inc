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
{--$DEFINE CONSOLE_DEBUG}         {Enable debug output/counters for console devices (Default: Off)}
{--$DEFINE LOGGING_DEBUG}         {Enable debug output/counters for logging devices (Default: Off)}

{--$DEFINE DEVICE_DEBUG}          {Enable debug output/counters for device management (Default: Off)}
{--$DEFINE DMA_DEBUG}             {Enable debug output/counters for DMA devices/hosts (Default: Off)}
{--$DEFINE USB_DEBUG}             {Enable debug output/counters for USB devices/drivers/hosts (Default: Off)}
{--$DEFINE MMC_DEBUG}             {Enable debug output/counters for MMC/SD devices/hosts (Default: Off)}
{--$DEFINE SPI_DEBUG}             {Enable debug output/counters for SPI devices (Default: Off)}
{--$DEFINE I2C_DEBUG}             {Enable debug output/counters for I2C devices (Default: Off)}
{--$DEFINE PWM_DEBUG}             {Enable debug output/counters for PWM devices (Default: Off)}
{--$DEFINE PCM_DEBUG}             {Enable debug output/counters for PCM devices (Default: Off)}
{--$DEFINE GPIO_DEBUG}            {Enable debug output/counters for GPIO devices (Default: Off)}
{--$DEFINE BLUETOOTH_DEBUG}       {Enable debug output/counters for Bluetooth devices/drivers/hosts (Default: Off)}

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
{$ENDIF}

{--$DEFINE NETWORK_DEBUG}         {Enable debug output/counters for Network services (Default: Off)}
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

{--$DEFINE WIFI_DEBUG}            {Enable debug output/counters for WiFi devices (Default: Off)}
{--$DEFINE VPN_DEBUG}             {Enable debug output/counters for VPN devices (Default: Off)}

{--$DEFINE SERIAL_DEBUG}          {Enable debug output/counters for Serial devices (Default: Off)}

{--$DEFINE VIDEO_DEBUG}           {Enable debug output/counters for Video devices (Default: Off)}
{--$DEFINE AUDIO_DEBUG}           {Enable debug output/counters for Audio devices (Default: Off)}

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

{==============================================================================}
{==============================================================================}
{Device debug definitions}
{--$DEFINE AHCI_DEBUG}            {Enable debug output/counters for AHCI driver (Default: Off)}
{--$DEFINE EHCI_DEBUG}            {Enable debug output/counters for EHCI driver (Default: Off)}
{--$DEFINE XHCI_DEBUG}            {Enable debug output/counters for XHCI driver (Default: Off)}
{--$DEFINE DWCOTG_DEBUG}          {Enable debug output/counters for DWCOTG driver (Default: Off)}
{--$DEFINE SMSC95XX_DEBUG}        {Enable debug output/counters for SMSC95XX driver (Default: Off)}
{--$DEFINE BCM2708_DEBUG}         {Enable debug output/counters for BCM2708 drivers (Default: Off)}
{--$DEFINE BCM2709_DEBUG}         {Enable debug output/counters for BCM2709 drivers (Default: Off)}

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
{Crypto definitions}
{--$DEFINE CRYPTO_DES_PRECOMPUTED_PERMUTATIONS}  {Enable precomputed permutations (IP/FP) for DES cipher (Faster but bigger) (Default: Off)}
{--$DEFINE CRYPTO_AES_PRECOMPUTED_TABLES}        {Enable precomputed tables (TE1/TE2/TE3/TE4/TD1/TD2/TD3/TD4 etc) for AES cipher (Faster but bigger) (Default: Off)}

{==============================================================================}
{==============================================================================}
