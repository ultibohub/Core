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
{$DEFINE CONSOLE_EARLY_INIT}
{$DEFINE LOGGING_EARLY_INIT}

{$IFDEF LOGGING_EARLY_INIT}
 {$DEFINE CONSOLE_EARLY_INIT}
{$ENDIF}

{==============================================================================}
{==============================================================================}
{System debug definitions}
{--$DEFINE IRQ_DEBUG}
{--$DEFINE FIQ_DEBUG}
{--$DEFINE SWI_DEBUG}

{$IF DEFINED(IRQ_DEBUG) or DEFINED(FIQ_DEBUG) or DEFINED(SWI_DEBUG)}
 {$DEFINE INTERRUPT_DEBUG}
{$ENDIF}

{--$DEFINE HEAP_DEBUG}
{--$DEFINE THREAD_DEBUG}
{--$DEFINE PLATFORM_DEBUG}
{--$DEFINE CONSOLE_DEBUG}
{--$DEFINE LOGGING_DEBUG}

{--$DEFINE DEVICE_DEBUG}
{--$DEFINE DMA_DEBUG}
{--$DEFINE USB_DEBUG}
{--$DEFINE MMC_DEBUG}
{--$DEFINE SPI_DEBUG}
{--$DEFINE I2C_DEBUG}
{--$DEFINE PWM_DEBUG}
{--$DEFINE PCM_DEBUG}
{--$DEFINE GPIO_DEBUG}
{--$DEFINE BLUETOOTH_DEBUG}

{--$DEFINE STORAGE_DEBUG}
{$IFDEF STORAGE_DEBUG}
 {$DEFINE ATA_DEBUG}
 {$DEFINE SCSI_DEBUG}
 {$DEFINE ATAPI_DEBUG}
{$ENDIF}

{--$DEFINE FILESYS_DEBUG}
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

{--$DEFINE NETWORK_DEBUG}
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

{--$DEFINE KEYBOARD_DEBUG}
{--$DEFINE MOUSE_DEBUG}

{--$DEFINE WIFI_DEBUG}
{--$DEFINE VPN_DEBUG}

{--$DEFINE SERIAL_DEBUG}

{--$DEFINE VIDEO_DEBUG}
{--$DEFINE AUDIO_DEBUG}

{==============================================================================}
{==============================================================================}
{Exception debug definitions}
{$DEFINE EXCEPTION_DEBUG}
{--$DEFINE DATA_ABORT_DEBUG}
{--$DEFINE PREFETCH_ABORT_DEBUG}
{--$DEFINE UNDEFINED_INSTRUCTION_DEBUG}

{==============================================================================}
{==============================================================================}
{Subsystem debug definitions}
{--$DEFINE BOOT_DEBUG}
{$DEFINE LOCK_DEBUG}
{--$DEFINE SPIN_DEBUG}
{--$DEFINE MUTEX_DEBUG}
{$DEFINE CLOCK_DEBUG}
{$DEFINE SCHEDULER_DEBUG}

{==============================================================================}
{==============================================================================}
{Device debug definitions}
{--$DEFINE AHCI_DEBUG}
{--$DEFINE EHCI_DEBUG}
{--$DEFINE XHCI_DEBUG}
{--$DEFINE DWCOTG_DEBUG}
{--$DEFINE SMSC95XX_DEBUG}
{--$DEFINE BCM2708_DEBUG}
{--$DEFINE BCM2709_DEBUG}

{==============================================================================}
{==============================================================================}
{Service debug definitions}
{--$DEFINE SERVICE_DEBUG}
{--$DEFINE PING_DEBUG}
{--$DEFINE NTP_DEBUG}
{--$DEFINE TELNET_DEBUG}
{--$DEFINE SYSLOG_DEBUG}
{--$DEFINE SSH_DEBUG}

{--$DEFINE CIFS_DEBUG}
{--$DEFINE NFS_DEBUG}

{--$DEFINE HTTP_DEBUG}
{--$DEFINE SMTP_DEBUG}
{--$DEFINE POP3_DEBUG}
{--$DEFINE IMAP4_DEBUG}
{--$DEFINE FTP_DEBUG}

{--$DEFINE SHELL_DEBUG}

{==============================================================================}
{==============================================================================}
{System statistics definitions}
{--$DEFINE IRQ_STATISTICS}
{--$DEFINE FIQ_STATISTICS}
{--$DEFINE SWI_STATISTICS}

{--$DEFINE HEAP_STATISTICS}
{--$DEFINE THREAD_STATISTICS}
{--$DEFINE PLATFORM_STATISTICS}

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
