## Ultibo core

This is the source for the Ultibo core project, a full featured embedded (no OS) development environment for Raspberry Pi.

Ultibo core is much more than just another OS example it is a complete platform for creating modern full featured embedded applications in C, C++ or FreePascal which run without any operating system and provides a large number of stable, ready to use features including:

* Support for most models of Raspberry Pi (A/B/A+/B+/2B/3B/3B+/4B/400/Zero/ZeroW/Zero2W) plus QEMU ARM emulation

* Pre-emptive threading

* Full range of locking and synchronization primitives

* Multicore support on Raspberry Pi 2, 3 and 4 with all cores sharing workload

* Thread priority, affinity and migration

* Complete IPv4 stack including TCP, UDP, ICMP and raw sockets as well as DNS and DHCP protocols

* USB support with drivers for Hub, Keyboard, Mouse, Storage, Network, HID, Touch, Gamepad and Joystick

* MMC/SD/SDIO device support including eMMC devices on Raspberry Pi Compute Modules

* Support for GPIO, I2C, SPI, PWM, UART and DMA devices

* Full support for FAT12/16/32, NTFS and CDFS filesystems

* Interrupt and fast interrupt handling

* Hardware exception handling

* Complete RTL with strings, code pages, Unicode, classes, objects and exceptions

* Full Winsock 1.1 and Winsock 2 implementation

* Standard C library support including BSD sockets and POSIX threads

* Standard C++ library included

* Prebuilt libraries for Freetype2, SQLite, Zlib, Libpng, LVGL, Libmad and more included

* Support for many common FPC packages

* Hardware accelerated OpenGL ES and OpenVG graphics and support for the official Pi camera

* Many included extras like HTTP, SMTP, NTP, SysLog, Telnet and Shell

Note that the Raspberry Pi Pico is not supported by Ultibo core as it is based on a microcontroller instead of a microprocessor and is not able to support the features required by Ultibo core.

### More information:

For all information on developing with Ultibo, building the source and getting started please see the [Ultibo.org website](https://ultibo.org)

An installer download for Windows and a Linux installer script are available from the [downloads page](https://ultibo.org/download/), installation on MacOS is supported using [fpcupdeluxe](https://github.com/LongDirtyAnimAlf/fpcupdeluxe/releases/latest)

The installer includes the Ultibo core source, examples and runtime and a full featured FreePascal development environment, for C/C++ development the [Ultibo API](https://github.com/ultibohub/API) provides the necessary headers plus examples for getting started.

Detailed documentation can be found in the [Ultibo wiki](https://ultibo.org/wiki)

Questions and other discussion should be posted in the [Ultibo forum](https://ultibo.org/forum/index.php)
