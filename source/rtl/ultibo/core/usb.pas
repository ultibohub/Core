{
Ultibo USB interface unit.

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

  Embedded XINU - Copyright, Douglas Comer and Dennis Brylow
                   http://xinu.mscs.mu.edu/USB
 
  USPI (rsta2) - https://github.com/rsta2/uspi
  
  Circle (rsta2) - https://github.com/rsta2/circle/tree/master/lib/usb
 
  U-Boot - \include\usbdescriptors.h
  
References
==========
 
 Universal Serial Bus Specification Revision 2.0 - http://www.usb.org/developers/docs/usb20_docs/
 
 USB in a NutShell - http://www.beyondlogic.org/usbnutshell/usb5.shtml
 
USB  
===

This unit implements the USB core software that does not depend on the specific
host controller hardware and is not specific to any single USB device or platform.

Features and limitations:

 - This driver is written to be compatible with USB 2.0.  USB 3.0 devices
   work correctly when operating in USB 2.0 compatible mode, actual support
   for USB 3.0 host controllers and native super speed modes will require
   some modification to this driver.

 - Not all USB transfer types and speeds are supported yet.  This depends on
   the Host Controller Driver, see USBRequestSubmit.

 - This driver does not attempt to do any intelligent power management,
   bandwidth allocation, or transfer scheduling.  Devices are always set to
   their first listed configuration regardless of power requirements.
   Requests are simply passed to the Host Controller Driver in the
   order submitted, the Host Controller Driver is responsible for doing
   any more intelligent scheduling if desired.

 - This driver does not support multiple configurations per USB device.  If
   a device happens to have multiple configurations, the first one will be
   assigned (see devices below for more information).

 - By design, it is possible to implement a host controller driver for
   different host controller hardware without changing any of this code, as
   long as the host controller driver provides the functions declared in
   TUSBHost (Start, Stop, Reset, Submit and Cancel).

 - By design, this driver has a hard-coded dependency on the USB hub driver
   because USB is useless without hubs.

To initialize this core USB driver, USBInit must be called by the system
startup code.  See that function for details.

If the flag USB_AUTOSTART is not set then USBStart must be called to start
the USB core.  See that function for details.

The other functions exported by this core USB driver are mostly intended to
be used by USB device drivers.

Debugging messages in this driver can be enabled by changing the value of
USB_DEFAULT_LOG_LEVEL and by enabling the USB_DEBUG define in GlobalDefines.
Be careful when enabling USB debugging as the USB core can generate a lot 
of messages due to constant polling of interrupt endpoints on devices such
as hubs, keyboards and mice.

USB Device
==========

USB devices are the generic implementation of anything that can be connected
to the USB bus. All USB devices require a driver to implement the specific
behaviour of the device but at the generic level each device will have:

 - One or more configurations available. The USB core will first offer the device
   to drivers without a configuration being selected, the driver may choose a 
   specific configuration if required otherwise the core will simply select the
   first available configuration.
   
 - One or more interfaces available. On devices with multiple interfaces, the USB
   core supports different drivers binding to different interfaces. This way
   combination devices are supported.
   
 - One or more endpoints available. All devices must have a control endpoint
   and most devices will have one or more bulk, interrupt or isochronous endpoints
   as well. The USB core will only communicate with the control endpoint to perform
   generic operations like reading the desriptors and assigning a device addresss.
   
   Drivers are expected to understand which endpoints they need to communicate with
   to provide the device specific functionality.

Devices are considered dynamic by the USB core and can be connected or disconnected 
at any time. The hub driver is responsible for attaching and detaching devices in
response to hub status change events and for binding or unbinding drivers as devices
are added or removed.
 
USB Driver
==========

USB drivers implement support for specific devices or specific device classes
and provide the interfaces to present those devices to other parts of the system.

For example the USB Mass Storage driver accepts devices of the mass storage class
and presents them to the file system as a disk device.

All drivers must implement the functions defined in TUSBDriver (Bind and Unbind)
and must register themselves with the USB core by calling USBDriverRegister.

When a new device is detected the hub driver will enumerate all drivers and call
their Bind functions until either one of the drivers accepts the device or all
drivers have rejected the device. The hub driver first attempts to bind each driver
to the device itself and then offers each driver the option to bind to a single
interface on the device. In this way devices that have multiple interfaces of 
different classes (such as a wireless mouse and keyboard dongle) can actually be
bound to multiple drivers which each service a specific interface.

When a new driver is registered the USB core will call the drivers Bind function
for each device already present to allow the driver an opportunity to bind to
any existing devices that are not serviced by other drivers.
 
When a device is disconnected the hub driver will first call the Unbind function 
of any driver that is servicing the device to allow the driver to clean up allocated
resources and cancel outstanding requests. Drivers must be careful not to block the
hub driver during this process, for example trying to send a USB control message to
the device at this time will likely be pointless as in many cases the device will have
already been disconnected from the system.

USB Host
========
 
USB hosts implement the hardware level interface that supports sending USB requests
to the hardware and receiving responses. All handling of interrupts, DMA, transaction
sequence, errors and resubmitting/retrying requests is done by the USB host driver.

All host drivers must implement the functions defined in TUSBHost (Start, Stop, Reset,
Submit and Cancel) and must register themselves with the USB core by calling USBHostRegister.

When the USB core is started each registered host driver will be called via the Start
function so it can initialize itself and allocate locks, buffers and other resources
required to interact with the hardware.

A host driver can also be registered after the USB core has been started and it will
be given the opportunity to start itself immediately. In this way USB hosts can potentially
be hot pluggable.

Each host driver must also implement a root hub which respresents the port or ports that
are directly connected to the controller hardware. In many cases this will not be a real
hub but will be simulated in the host driver so that the hub driver can interact with it
as though it was a standard hub device.
 
USB Hub
=======

Hubs are one of the fundamental devices in USB and are used to provide
connection points (ports) for additional devices. Note that even
if no "external" hub is plugged in, the USB still will have at
least one logical hub (the root hub) and usually additional "internal" hubs.
That is, a USB is a tree of devices where the root node and all non-leaf
nodes are hubs.  A port on a USB hub may correspond to a port you can
physically plug a device into or may correspond to an internal port.

USB hubs are commonly based around a design that has 4 ports per hub, so
a standard 7 port hub will most often appear as 2 hubs to this driver with
one hub connected to a port on the other.

This hub driver is an example of a USB device driver, but it is somewhat
special as it mandatory to include this driver if USB is supported at all.
This is because it would be impossible to access any USB devices if a hub
driver were not available.  This hub driver also uses some interfaces in the
core driver, such as USBDeviceAttach, that are not useful to any other
USB device driver.

The initial entry point of this USB hub driver is USBHubDriverBind, which is
called when the USB core has configured a newly attached USB device that may
be a hub.  USBHubDriverBind is responsible for checking if the device is a
hub, and if so, doing hub-specific setup, reading the hub descriptor, powering
on the ports, and submitting an asynchronous USB interrupt request to the hub's
status change endpoint.

Everything else this hub driver does happens asynchronously as a response to
a status change request being completed.  Every USB hub has exactly one IN
interrupt endpoint called the "status change endpoint".  The hub responds on
this endpoint whenever the status of the hub or one of the hub's ports has
changed, for example when a USB device has been connected or disconnected
from a port.

At the hardware level, when a hub has data to send on its status change
endpoint, an interrupt will come in from the USB host controller.  This
eventually will result in the status change transfer being completed and
USBHubStatusComplete being called.  Thus, the detection of status changes is
interrupt-driven and is not implemented by polling at the software level.
(At the hardware level, USB is still a polled bus, but the host controller
hardware handles that for us)  Upon detecting a status change on one or more
ports on a hub, the hub driver then must submit one or more control messages
to the hub to determine exactly what changed on the affected ports.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit USB;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,HeapManager,Devices,Unicode,SysUtils;

//To Do //For some USB 3.0 information see: \u-boot-HEAD-5745f8c\include\usb.h
              //eg usb_interface / ss_ep_comp_desc (Super Speed Endpoint Companion Descriptor)
        
//To Do //Look for:
        
//Critical
        
//To Do  //Move global variables in each module to local only ? - 
                                                  
                         
{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}
        
{==============================================================================}
const
 {USB Device, Driver and Host specific constants}
 USB_DEVICE_PREFIX = 'USB';    {Name prefix for USB Devices}
 USB_DRIVER_PREFIX = 'USB';    {Name prefix for USB Drivers}
 USB_HOST_PREFIX = 'USBHost';  {Name prefix for USB Hosts}
 
 {USB Device Types}
 USB_TYPE_NONE       = 0;
 
 USB_TYPE_MAX        = 0;
 
 {USB Device Type Names}
 USB_TYPE_NAMES:array[USB_TYPE_NONE..USB_TYPE_MAX] of String = (
  'USB_TYPE_NONE');
 
 {USB Device States}
 USB_STATE_DETACHED  = 0;
 USB_STATE_DETACHING = 1;
 USB_STATE_ATTACHING = 2;
 USB_STATE_ATTACHED  = 3;

 USB_STATE_MAX       = 3;
 
 {USB Device State Names}
 USB_STATE_NAMES:array[USB_STATE_DETACHED..USB_STATE_MAX] of String = (
  'USB_STATE_DETACHED',
  'USB_STATE_DETACHING',
  'USB_STATE_ATTACHING',
  'USB_STATE_ATTACHED');
 
 {USB Device Status}
 USB_STATUS_UNBOUND   = 0; 
 USB_STATUS_BOUND     = 1;
 
 USB_STATUS_MAX       = 1;
 
 {USB Device Status Names}
 USB_STATUS_NAMES:array[USB_STATUS_UNBOUND..USB_STATUS_MAX] of String = (
  'USB_STATUS_UNBOUND',
  'USB_STATUS_BOUND');
 
 {USB Device Flags}
 USB_FLAG_NONE       = $00000000;
 
 {USB Host Types}
 USBHOST_TYPE_NONE   = 0;
 USBHOST_TYPE_EHCI   = 1;
 USBHOST_TYPE_OHCI   = 2;
 USBHOST_TYPE_UHCI   = 3;
 USBHOST_TYPE_XHCI   = 4;
 USBHOST_TYPE_DWCOTG = 5;
 
 USBHOST_TYPE_MAX    = 5;
 
 {USB Host Type Names}
 USBHOST_TYPE_NAMES:array[USBHOST_TYPE_NONE..USBHOST_TYPE_MAX] of String = (
  'USBHOST_TYPE_NONE',
  'USBHOST_TYPE_EHCI',
  'USBHOST_TYPE_OHCI',
  'USBHOST_TYPE_UHCI',
  'USBHOST_TYPE_XHCI',
  'USBHOST_TYPE_DWCOTG');
 
 {USB Host States}
 USBHOST_STATE_DISABLED = 0;
 USBHOST_STATE_ENABLED  = 1;
 
 USBHOST_STATE_MAX      = 1;
 
 {USB Host State Names}
 USBHOST_STATE_NAMES:array[USBHOST_STATE_DISABLED..USBHOST_STATE_MAX] of String = (
  'USBHOST_STATE_DISABLED',
  'USBHOST_STATE_ENABLED');
 
 {USB Host Flags}
 USBHOST_FLAG_NONE      = $00000000;
 USBHOST_FLAG_SHARED    = $00000001;
 USBHOST_FLAG_NOCACHE   = $00000002;
 
 {USB Status Codes}
 USB_STATUS_SUCCESS                   = 0;  {Function successful}
 USB_STATUS_DEVICE_DETACHED           = 1;  {USB device was detached}
 USB_STATUS_DEVICE_UNSUPPORTED        = 2;  {USB device is unsupported by the driver}
 USB_STATUS_HARDWARE_ERROR            = 3;  {Hardware error of some form occurred}
 USB_STATUS_INVALID_DATA              = 4;  {Invalid data was received}
 USB_STATUS_INVALID_PARAMETER         = 5;  {An invalid parameter was passed to the function}
 USB_STATUS_NOT_PROCESSED             = 6;  {The USB request has been submitted but not yet processed}
 USB_STATUS_OUT_OF_MEMORY             = 7;  {Failed to allocate memory}
 USB_STATUS_TIMEOUT                   = 8;  {The operation timed out}
 USB_STATUS_UNSUPPORTED_REQUEST       = 9;  {The request is unsupported}
 USB_STATUS_HARDWARE_STALL            = 10; {The device reported an endpoint STALL}
 USB_STATUS_OPERATION_FAILED          = 11; {The operation was not able to be completed}
 USB_STATUS_NOT_BOUND                 = 12; {USB device is not bound to a driver}
 USB_STATUS_ALREADY_BOUND             = 13; {USB device is already bound to a driver}
 USB_STATUS_NOT_READY                 = 14; {USB device is not in a ready state}
 USB_STATUS_NOT_COMPLETED             = 15; {The USB request has been scheduled but not yet completed}
 USB_STATUS_CANCELLED                 = 16; {The USB request was cancelled}
 USB_STATUS_NOT_VALID                 = 17; {The USB request is not valid}
 
 {USB Request Flags}
 USB_REQUEST_FLAG_NONE       = $00000000;
 USB_REQUEST_FLAG_ALLOCATED  = $00000001; {Request data has been allocated by USBBufferAllocate (and can be freed by USBBufferRelease)}
 USB_REQUEST_FLAG_COMPATIBLE = $00000002; {Request data is compatible with DMA requirements of host configuration (Can be passed directly to DMA)}
 USB_REQUEST_FLAG_ALIGNED    = $00000004; {Request data is aligned according to host configuration}
 USB_REQUEST_FLAG_SIZED      = $00000008; {Request data is sized according to host configuration}
 USB_REQUEST_FLAG_SHARED     = $00000010; {Request data has been allocated from Shared memory}
 USB_REQUEST_FLAG_NOCACHE    = $00000020; {Request data has been allocated from Non Cached memory}
 
 {USB Control Phases}
 USB_CONTROL_PHASE_SETUP  = 0; {Setup phase of a Control request (Using SetupData)}
 USB_CONTROL_PHASE_DATA   = 1; {Data phase of a Control request (Using Data buffer)}
 USB_CONTROL_PHASE_STATUS = 2; {Status phase of a Control request (Using StatusData)}
 
 {USB Control Timeouts}
 USB_CONTROL_GET_TIMEOUT = 5000;
 USB_CONTROL_SET_TIMEOUT = 5000;
 
 {Default maximum packet size for unconfigured Endpoints}
 USB_DEFAULT_MAX_PACKET_SIZE = 8;
 USB_ALTERNATE_MAX_PACKET_SIZE = 64;
 
 {Maximum packet size of any USB Endpoint  (1024 is the maximum allowed by USB 2.0)}
 USB_MAX_PACKET_SIZE = 1024;
 
 {Number of USB frames per millisecond}
 USB_FRAMES_PER_MS  = 1;

 {Number of USB microframes per millisecond}
 USB_UFRAMES_PER_MS = 8;
 
 {Values for bmAttributes in type TUSBConfigurationDescriptor}
 USB_CONFIGURATION_ATTRIBUTE_RESERVED_HIGH  = $80;
 USB_CONFIGURATION_ATTRIBUTE_SELF_POWERED   = $40;
 USB_CONFIGURATION_ATTRIBUTE_REMOTE_WAKEUP  = $20;
 USB_CONFIGURATION_ATTRIBUTE_RESERVED_LOW   = $1f;
 
 {Values for wStatus in type TUSBDeviceStatus (Device Recipient) (See Figure 9-4 of Section 9.4 of the USB 2.0 specification)}
 USB_DEVICE_STATUS_SELF_POWERED  = (1 shl 0);
 USB_DEVICE_STATUS_REMOTE_WAKEUP = (1 shl 1);
 
 {Values for vStatus in type TUSBDeviceStatus (Endpoint Recipient) (See Figure 9-6 of Section 9.4 of the USB 2.0 specification)}
 USB_ENDPOINT_STATUS_HALT        = (1 shl 0);
 
 {USB Device Speeds}
 USB_SPEED_HIGH      = 0; {480 Mb/s}
 USB_SPEED_FULL      = 1; {12 Mb/s}
 USB_SPEED_LOW       = 2; {1.5 Mb/s}
 USB_SPEED_SUPER     = 3; {5 Gb/s}
 USB_SPEED_SUPERPLUS = 4; {10 Gb/s}
 
 {Values for bmAttributes (bits 1..0) in type TUSBEndpointDescriptor}
 {USB Transfer Types}
 USB_TRANSFER_TYPE_CONTROL     = 0;
 USB_TRANSFER_TYPE_ISOCHRONOUS = 1;
 USB_TRANSFER_TYPE_BULK        = 2;
 USB_TRANSFER_TYPE_INTERRUPT   = 3;
 USB_TRANSFER_TYPE_MASK        = $03;
 
 {USB Transfer Sizes}
 USB_TRANSFER_SIZE_8_BIT  = 0;
 USB_TRANSFER_SIZE_16_BIT = 1;
 USB_TRANSFER_SIZE_32_BIT = 2;
 USB_TRANSFER_SIZE_64_BIT = 3;
 
 {Values for bDescriptorType in types TUSBDescriptorHeader and TUSBDeviceDescriptor}
 {USB Descriptor Types (See Table 9-5 in Section 9.4 of the USB 2.0 specification)}
 USB_DESCRIPTOR_TYPE_DEVICE                    = 1;
 USB_DESCRIPTOR_TYPE_CONFIGURATION             = 2;
 USB_DESCRIPTOR_TYPE_STRING                    = 3;
 USB_DESCRIPTOR_TYPE_INTERFACE                 = 4;
 USB_DESCRIPTOR_TYPE_ENDPOINT                  = 5;
 USB_DESCRIPTOR_TYPE_DEVICE_QUALIFIER          = 6;
 USB_DESCRIPTOR_TYPE_OTHER_SPEED_CONFIGURATION = 7;
 USB_DESCRIPTOR_TYPE_INTERFACE_POWER           = 8;
 USB_DESCRIPTOR_TYPE_HUB                       = $29;
 
 {Values for bmRequestType (bit 7) in type TUSBControlSetupData (See Table 9-2 of Section 9.3 of the USB 2.0 specification)}
 {USB Transfer Directions (Relative to the host)}
 USB_DIRECTION_OUT = 0;  {Host to Device}
 USB_DIRECTION_IN  = 1;  {Device to Host}
 
 {USB Request Types (bits 6..5 of bmRequestType in type TUSBControlSetupData) (See Table 9-2 of Section 9.3 of the USB 2.0 specification)}
 USB_REQUEST_TYPE_STANDARD = 0;
 USB_REQUEST_TYPE_CLASS    = 1;
 USB_REQUEST_TYPE_VENDOR   = 2;
 USB_REQUEST_TYPE_RESERVED = 3;
 
 {USB Request Recipients (bits 4..0 of bmRequestType in type TUSBControlSetupData) (See Table 9-2 of Section 9.3 of the USB 2.0 specification)}
 USB_REQUEST_RECIPIENT_DEVICE    = 0;
 USB_REQUEST_RECIPIENT_INTERFACE = 1;
 USB_REQUEST_RECIPIENT_ENDPOINT  = 2;
 USB_REQUEST_RECIPIENT_OTHER     = 3;

 {Values of the bitfields within the bmRequestType member of TUSBControlSetupData (See Table 9-2 of Section 9.3 of the USB 2.0 specification)}
 USB_BMREQUESTTYPE_DIR_OUT             = (USB_DIRECTION_OUT shl 7);
 USB_BMREQUESTTYPE_DIR_IN              = (USB_DIRECTION_IN shl 7);
 USB_BMREQUESTTYPE_DIR_MASK            = ($01 shl 7);
 USB_BMREQUESTTYPE_TYPE_STANDARD       = (USB_REQUEST_TYPE_STANDARD shl 5);
 USB_BMREQUESTTYPE_TYPE_CLASS          = (USB_REQUEST_TYPE_CLASS shl 5);
 USB_BMREQUESTTYPE_TYPE_VENDOR         = (USB_REQUEST_TYPE_VENDOR shl 5);
 USB_BMREQUESTTYPE_TYPE_RESERVED       = (USB_REQUEST_TYPE_RESERVED shl 5);
 USB_BMREQUESTTYPE_TYPE_MASK           = ($03 shl 5);
 USB_BMREQUESTTYPE_RECIPIENT_DEVICE    = (USB_REQUEST_RECIPIENT_DEVICE shl 0);
 USB_BMREQUESTTYPE_RECIPIENT_INTERFACE = (USB_REQUEST_RECIPIENT_INTERFACE shl 0);
 USB_BMREQUESTTYPE_RECIPIENT_ENDPOINT  = (USB_REQUEST_RECIPIENT_ENDPOINT shl 0);
 USB_BMREQUESTTYPE_RECIPIENT_OTHER     = (USB_REQUEST_RECIPIENT_OTHER shl 0);
 USB_BMREQUESTTYPE_RECIPIENT_MASK      = ($1f shl 0);
 
 {Values for bRequest in type TUSBControlSetupData}
 {USB Device Requests (See Table 9-3 in Section 9.4 of the USB 2.0 specification)}
 USB_DEVICE_REQUEST_GET_STATUS        = 0;
 USB_DEVICE_REQUEST_CLEAR_FEATURE     = 1;
 USB_DEVICE_REQUEST_SET_FEATURE       = 3;
 USB_DEVICE_REQUEST_SET_ADDRESS       = 5;
 USB_DEVICE_REQUEST_GET_DESCRIPTOR    = 6;
 USB_DEVICE_REQUEST_SET_DESCRIPTOR    = 7;
 USB_DEVICE_REQUEST_GET_CONFIGURATION = 8;
 USB_DEVICE_REQUEST_SET_CONFIGURATION = 9;
 USB_DEVICE_REQUEST_GET_INTERFACE     = 10;
 USB_DEVICE_REQUEST_SET_INTERFACE     = 11;
 USB_DEVICE_REQUEST_SYNCH_FRAME       = 12;
 
 {Values for wValue in type TUSBControlSetupData}
 {USB Device Features (See Table 9-6 in Section 9.4 of the USB 2.0 specification)}
 USB_DEVICE_FEATURE_ENDPOINT_HALT     = 0; {Endpoint Only}
 USB_DEVICE_FEATURE_REMOTE_WAKEUP     = 1; {Device Only}
 USB_DEVICE_FEATURE_TEST_MODE         = 2; {Device Only}
 
 {Values for wIndex in type TUSBControlSetupData}
 {USB Test Modes (See Table 9-7 in Section 9.4 of the USB 2.0 specification)}
 USB_DEVICE_TEST_MODE_J            = $01;
 USB_DEVICE_TEST_MODE_K            = $02;
 USB_DEVICE_TEST_MODE_SE0_NAK      = $03;
 USB_DEVICE_TEST_MODE_PACKET       = $04;
 USB_DEVICE_TEST_MODE_FORCE_ENABLE = $05;
 
 {USB Packet ID values (See ????????)} //To Do //Not used ?, not part of spec ?
 USB_PACKETID_UNDEF_0           =  $f0;
 USB_PACKETID_OUT               =  $e1;
 USB_PACKETID_ACK               =  $d2;
 USB_PACKETID_DATA0             =  $c3;
 USB_PACKETID_UNDEF_4           =  $b4;
 USB_PACKETID_SOF               =  $a5;
 USB_PACKETID_UNDEF_6           =  $96;
 USB_PACKETID_UNDEF_7           =  $87;
 USB_PACKETID_UNDEF_8           =  $78;
 USB_PACKETID_IN                =  $69;
 USB_PACKETID_NAK               =  $5a;
 USB_PACKETID_DATA1             =  $4b;
 USB_PACKETID_PREAMBLE          =  $3c;
 USB_PACKETID_SETUP             =  $2d;
 USB_PACKETID_STALL             =  $1e;
 USB_PACKETID_UNDEF_F           =  $0f;
 
 {USB Class Codes (bDeviceClass / bInterfaceClass) (Note that only the hub class is defined in the USB 2.0 specification itself the other standard class codes are defined in additional specifications)}
 USB_CLASS_CODE_INTERFACE_SPECIFIC             = $00; {Use class code info from Interface Descriptors }
 USB_CLASS_CODE_AUDIO                          = $01; {Audio device}
 USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL = $02; {Communication device class}
 USB_CLASS_CODE_HID                            = $03; {HID device class}
 USB_CLASS_CODE_PHYSICAL                       = $05; {Physical device class}
 USB_CLASS_CODE_IMAGE                          = $06; {Still Imaging device}
 USB_CLASS_CODE_PRINTER                        = $07; {Printer device}
 USB_CLASS_CODE_MASS_STORAGE                   = $08; {Mass Storage device}
 USB_CLASS_CODE_HUB                            = $09; {Hub Device}
 USB_CLASS_CODE_CDC_DATA                       = $0a; {CDC data device}
 USB_CLASS_CODE_SMART_CARD                     = $0b; {Smart Card device}
 USB_CLASS_CODE_CONTENT_SECURITY               = $0d; {Content Security device}
 USB_CLASS_CODE_VIDEO                          = $0e; {Video device}
 USB_CLASS_CODE_PERSONAL_HEALTHCARE            = $0f; {Personal Healthcare device}
 USB_CLASS_CODE_AUDIO_VIDEO                    = $10; {Audio/Video Devices}
 USB_CLASS_CODE_BILLBOARD                      = $11; {Billboard Device}
 USB_CLASS_CODE_DIAGNOSTIC                     = $DC; {Diagnostic Device}
 USB_CLASS_CODE_WIRELESS_CONTROLLER            = $e0; {Wireless Controller}
 USB_CLASS_CODE_MISCELLANEOUS                  = $ef; {Miscellaneous}
 USB_CLASS_CODE_APPLICATION_SPECIFIC           = $fe; {Application Specific}
 USB_CLASS_CODE_VENDOR_SPECIFIC                = $ff; {Vendor Specific}
 
 {USB SubClass Codes (bInterfaceSubClass) (See: http://www.usb.org/developers/defined_class/)}
 {Communications Devices}
 USB_SUBCLASS_CDC_DLCM                         = $01; {Direct Line Control Model (USBPSTN1.2)}
 USB_SUBCLASS_CDC_ACM                          = $02; {Abstract Control Model (USBPSTN1.2)}
 USB_SUBCLASS_CDC_TCM                          = $03; {Telephone Control Model (USBPSTN1.2)}
 USB_SUBCLASS_CDC_MCCM                         = $04; {Multi-Channel Control Model (USBISDN1.2)}
 USB_SUBCLASS_CDC_CCM                          = $05; {CAPI Control Model (USBISDN1.2)}
 USB_SUBCLASS_CDC_ETHERNET                     = $06; {Ethernet Networking Control Model (USBECM1.2)}
 USB_SUBCLASS_CDC_WHCM                         = $08; {Wireless Handset Control Model (USBWMC1.1)}
 USB_SUBCLASS_CDC_DMM                          = $09; {Device Management Model (USBWMC1.1)}
 USB_SUBCLASS_CDC_MDLM                         = $0a; {Mobile Direct Line Model (USBWMC1.1)}
 USB_SUBCLASS_CDC_OBEX                         = $0b; {OBEX (USBWMC1.1)}
 USB_SUBCLASS_CDC_EEM                          = $0c; {Ethernet Emulation Model (USBEEM1.0)}
 USB_SUBCLASS_CDC_NCM                          = $0d; {Network Control Model (USBNCM1.0)}
 USB_SUBCLASS_CDC_MBIM                         = $0e; 
 {Still Image Devices}
 USB_SUBCLASS_IMAGE_DEFAULT                    = $01;
 {Mass Storage Devices}
 USB_SUBCLASS_MASS_STORAGE_DEFAULT             = $00; {SCSI command set not reported, De facto use}
 USB_SUBCLASS_MASS_STORAGE_RBC                 = $01; {Reduced Block Commands (RBC), INCITS 330:2000, available at http://www.t10.org}
 USB_SUBCLASS_MASS_STORAGE_MMC5                = $02; {Multi-Media Command Set 5 (MMC-5), T10/1675-D available at http://www.t10.org}
 USB_SUBCLASS_MASS_STORAGE_QIC157              = $03; {Obsolete was QIC-157}
 USB_SUBCLASS_MASS_STORAGE_UFI                 = $04; {UFI Specifies how to interface Floppy Disk Drives to USB}
 USB_SUBCLASS_MASS_STORAGE_SFF8070I            = $05; {Obsolete Was SFF-8070i}
 USB_SUBCLASS_MASS_STORAGE_SCSI                = $06; {SCSI transparent command set}
 USB_SUBCLASS_MASS_STORAGE_LSDFS               = $07; {LSD FS }
 USB_SUBCLASS_MASS_STORAGE_IEEE1667            = $08; {IEEE 1667 Standard Protocol for Authentication in Host Attachments of Transient Storage Devices (IEEE 1667) available at www.ieee1667.com}
 USB_SUBCLASS_MASS_STORAGE_VENDOR_SPECIFIC     = $ff; {Specific to device vendor, De facto use}
 {Content Security Devices}
 USB_SUBCLASS_CONTENT_SECURITY_DEFAULT         = $00;
 {Audio/Video Devices}
 USB_SUBCLASS_AUDIO_VIDEO_CONTROL              = $01; {Audio/Video Device – AVControl Interface}
 USB_SUBCLASS_AUDIO_VIDEO_DATA_VIDEO           = $02; {Audio/Video Device – AVData Video Streaming Interface}
 USB_SUBCLASS_AUDIO_VIDEO_DATA_AUDIO           = $03; {Audio/Video Device – AVData Audio Streaming Interface}
 {Billboard Devices}
 USB_SUBCLASS_BILLBOARD_DEFAULT                = $00;
 {Diagnostic Device}
 USB_SUBCLASS_DIAGNOSTIC_DEFAULT               = $01;
 {Wireless Controller}
 USB_SUBCLASS_WIRELESS_CONTROLLER_BLUETOOTH    = $01;
 USB_SUBCLASS_WIRELESS_CONTROLLER_USB          = $02;
 {Miscellaneous}
 USB_SUBCLASS_MISCELLANEOUS_SYNC               = $01;
 USB_SUBCLASS_MISCELLANEOUS_IAD_WAMP           = $02; {Interface Association Descriptor / Wire Adapter Multifunction Peripheral}
 USB_SUBCLASS_MISCELLANEOUS_CBAF               = $03; {Cable Based Association Framework}
 USB_SUBCLASS_MISCELLANEOUS_RNDIS              = $04;
 USB_SUBCLASS_MISCELLANEOUS_USB3VISION         = $05;
 {Application Specific}
 USB_SUBCLASS_APPLICATION_SPECIFIC_DFU         = $01;
 USB_SUBCLASS_APPLICATION_SPECIFIC_IRDA        = $02;
 USB_SUBCLASS_APPLICATION_SPECIFIC_TMC         = $02;
 
 {USB Protocol Codes (bInterfaceProtocol) (See: http://www.usb.org/developers/defined_class/)}
 {Communications Devices}
 USB_PROTOCOL_CDC_ACM_NONE                     = 0;   {Abstract Control Model - No class specific protocol required}
 USB_PROTOCOL_CDC_ACM_AT_V25TER                = 1;   {Abstract Control Model - AT Commands: V.250 etc}
 USB_PROTOCOL_CDC_ACM_AT_PCCA101               = 2;   {Abstract Control Model - AT Commands defined by PCCA-101}
 USB_PROTOCOL_CDC_ACM_AT_PCCA101_WAKE          = 3;   {Abstract Control Model - AT Commands defined by PCCA-101 & Annex O}
 USB_PROTOCOL_CDC_ACM_AT_GSM                   = 4;   {Abstract Control Model - AT Commands defined by GSM 07.07}
 USB_PROTOCOL_CDC_ACM_AT_3G                    = 5;   {Abstract Control Model - AT Commands defined by 3GPP 27.007}
 USB_PROTOCOL_CDC_ACM_AT_CDMA                  = 6;   {Abstract Control Model - AT Commands defined by TIA for CDMA}
 USB_PROTOCOL_CDC_ACM_VENDOR                   = $ff; {Abstract Control Model - Vendor-specific}
 USB_PROTOCOL_CDC_EEM                          = 7;   {Ethernet Emulation Model}
 USB_PROTOCOL_CDC_NCM_NTB                      = 1;   {Network Control Model - Network Transfer Block}
 USB_PROTOCOL_CDC_MBIM_NTB                     = 2;   {Network Transfer Block}
 {Still Image Devices}
 USB_PROTOCOL_IMAGE_DEFAULT                    = $01;
 {Mass Storage Devices}
 USB_PROTOCOL_MASS_STORAGE_CBI                 = $00; {CBI (with command completion interrupt) USB Mass Storage Class Control/Bulk/Interrupt Transport}
 USB_PROTOCOL_MASS_STORAGE_CB                  = $01; {CBI (with no command completion interrupt) USB Mass Storage Class Control/Bulk/Interrupt Transport}
 USB_PROTOCOL_MASS_STORAGE_BBB                 = $50; {BBB USB Mass Storage Class Bulk-Only Transport }
 USB_PROTOCOL_MASS_STORAGE_UAS                 = $62; {UAS }
 USB_PROTOCOL_MASS_STORAGE_VENDOR_SPECIFIC     = $ff; {Specific to device vendor, De facto use}
 {Hub Devices}
 USB_PROTOCOL_HUB_FULLSPEED                    = $00; {Full speed Hub}
 USB_PROTOCOL_HUB_HIGHSPEED_SINGLE_TT          = $01; {Hi-speed hub with single Transaction Translator}
 USB_PROTOCOL_HUB_HIGHSPEED_MULTI_TT           = $02; {Hi-speed hub with multiple Transaction Translators}
 {Content Security Devices}
 USB_PROTOCOL_CONTENT_SECURITY_DEFAULT         = $00;
 {Audio/Video Devices}
 USB_PROTOCOL_AUDIO_VIDEO_DEFAULT              = $00;
 {Billboard Devices}
 USB_PROTOCOL_BILLBOARD_DEFAULT                = $00;
 {Diagnostic Device}
 USB_PROTOCOL_DIAGNOSTIC_DEFAULT               = $01;
 {Wireless Controller}
 USB_PROTOCOL_WIRELESS_CONTROLLER_BLUETOOTH      = $01; {See: http://www.bluetooth.com/}
 USB_PROTOCOL_WIRELESS_CONTROLLER_UWB            = $02; {See: Wireless USB Specification in Chapter 8}
 USB_PROTOCOL_WIRELESS_CONTROLLER_NDIS           = $03; {See: http://www.microsoft.com/windowsmobile/mobileoperators/default.mspx}
 USB_PROTOCOL_WIRELESS_CONTROLLER_BLUETOOTH_AMP  = $04; {See: http://www.bluetooth.com/}
 USB_PROTOCOL_WIRELESS_CONTROLLER_USB_HOST        = $01; {Host Wire Adapter Control/Data interface.  Definition can be found in the Wireless USB Specification in Chapter 8}
 USB_PROTOCOL_WIRELESS_CONTROLLER_USB_DEVICE      = $02; {Device Wire Adapter Control/Data interface.  Definition can be found in the Wireless USB Specification in Chapter 8}
 USB_PROTOCOL_WIRELESS_CONTROLLER_USB_DEVOCE_ISOC = $03; {Device Wire Adapter Isochronous interface.  Definition can be found in the Wireless USB Specification in Chapter 8}
 {Miscellaneous}
 USB_PROTOCOL_MISCELLANEOUS_ACTIVESYNC            = $01; {Active Sync device}
 USB_PROTOCOL_MISCELLANEOUS_PALMSYNC              = $02; {Palm Sync}
 USB_PROTOCOL_MISCELLANEOUS_IAD                   = $01; {Interface Association Descriptor. The usage of this class code triple is defined in the Interface Association Descriptor ECN }
 USB_PROTOCOL_MISCELLANEOUS_WAMP                  = $02; {Wire Adapter Multifunction Peripheral programming interface. Definition can be found in the Wireless USB Specification in Chapter 8}
 USB_PROTOCOL_MISCELLANEOUS_CBAF                  = $01; {Cable Based Association Framework. This is defined in the Association Model addendum to the Wireless USB specification}
 USB_PROTOCOL_MISCELLANEOUS_RNDIS_ETHERNET        = $01; {RNDIS over Ethernet}
 USB_PROTOCOL_MISCELLANEOUS_RNDIS_WIFI            = $02; {RNDIS over WiFi}
 USB_PROTOCOL_MISCELLANEOUS_RNDIS_WIMAX           = $03; {RNDIS over WiMAX}
 USB_PROTOCOL_MISCELLANEOUS_RNDIS_WWAN            = $04; {RNDIS over WWAN}
 USB_PROTOCOL_MISCELLANEOUS_RNDIS_IPV4            = $05; {RNDIS for Raw IPv4}
 USB_PROTOCOL_MISCELLANEOUS_RNDIS_IPV6            = $06; {RNDIS for Raw IPv6}
 USB_PROTOCOL_MISCELLANEOUS_RNDIS_GPRS            = $07; {RNDIS for GPRS}
 USB_PROTOCOL_MISCELLANEOUS_USB3VISION_CONTROL    = $00; {USB3 Vision Control Interface }
 USB_PROTOCOL_MISCELLANEOUS_USB3VISION_EVENT      = $01; {USB3 Vision Event Interface}
 USB_PROTOCOL_MISCELLANEOUS_USB3VISION_STREAM     = $02; {USB3 Vision Streaming Interface}
 {Application Specific}
 USB_PROTOCOL_APPLICATION_SPECIFIC_DFU_DEFAULT    = $01; {Device Firmware Upgrade}
 USB_PROTOCOL_APPLICATION_SPECIFIC_IRDA_DEFAULT   = $00; {IRDA Bridge device}
 USB_PROTOCOL_APPLICATION_SPECIFIC_TMC_DEFAULT    = $00; {USB Test and Measurement Device}
 USB_PROTOCOL_APPLICATION_SPECIFIC_TMC_488        = $01; {USB Test and Measurement Device conforming to the USBTMC USB488 Subclass Specification}
 
 {USB Primary Language IDs (See Language Identifiers supplement to the USB 2.0 specification) (These are the first 10 bits of the 16-bit language identifier)}
 {Reserved $00}
 USB_LANG_ARABIC     = $01; {Arabic}
 USB_LANG_BULGARIAN  = $02; {Bulgarian}
 USB_LANG_CATALAN    = $03; {Catalan}
 USB_LANG_CHINESE    = $04; {Chinese}
 USB_LANG_CZECH      = $05; {Czech}
 USB_LANG_DANISH     = $06; {Danish}
 USB_LANG_GERMAN     = $07; {German}
 USB_LANG_GREEK      = $08; {Greek}
 USB_LANG_ENGLISH    = $09; {English}
 USB_LANG_SPANISH    = $0a; {Spanish}
 USB_LANG_FINNISH    = $0b; {Finnish}
 USB_LANG_FRENCH     = $0c; {French}
 USB_LANG_HEBREW     = $0d; {Hebrew}
 USB_LANG_HUNGARIAN  = $0e; {Hungarian}
 USB_LANG_ICELANDIC  = $0f; {Icelandic}
 USB_LANG_ITALIAN    = $10; {Italian}
 USB_LANG_JAPANESE   = $11; {Japanese}
 USB_LANG_KOREAN     = $12; {Korean}
 USB_LANG_DUTCH      = $13; {Dutch}
 USB_LANG_NORWEGIAN  = $14; {Norwegian}
 USB_LANG_POLISH     = $15; {Polish}
 USB_LANG_PORTUGUESE = $16; {Portuguese}
 USB_LANG_ROMANIAN   = $18; {Romanian}
 USB_LANG_RUSSIAN    = $19; {Russian}
 USB_LANG_CROATIAN   = $1a; {Croatian}
 USB_LANG_SERBIAN    = $1a; {Serbian}
 USB_LANG_SLOVAK     = $1b; {Slovak}
 USB_LANG_ALBANIAN   = $1c; {Albanian}
 USB_LANG_SWEDISH    = $1d; {Swedish}
 USB_LANG_THAI       = $1e; {Thai}
 USB_LANG_TURKISH    = $1f; {Turkish}
 USB_LANG_URDU       = $20; {Urdu}
 USB_LANG_INDONESIAN = $21; {Indonesian}
 USB_LANG_UKRANIAN   = $22; {Ukrainian}
 USB_LANG_BELARUSIAN = $23; {Belarusian}
 USB_LANG_SLOVENIAN  = $24; {Slovenian}
 USB_LANG_ESTONIAN   = $25; {Estonian}
 USB_LANG_LATVIAN    = $26; {Latvian}
 USB_LANG_LITHUANIAN = $27; {Lithuanian}
 USB_LANG_FARSI      = $29; {Farsi}
 USB_LANG_VIETNAMESE = $2a; {Vietnamese}
 USB_LANG_ARMENIAN   = $2b; {Armenian}
 USB_LANG_AZERI      = $2c; {Azeri}
 USB_LANG_BASQUE     = $2d; {Basque}
 USB_LANG_MACEDONIAN = $2f; {Macedonian}
 USB_LANG_AFRIKAANS  = $36; {Afrikaans}
 USB_LANG_GEORGIAN   = $37; {Georgian}
 USB_LANG_FAEROESE   = $38; {Faeroese}
 USB_LANG_HINDI      = $39; {Hindi}
 USB_LANG_MALAY      = $3e; {Malay}
 USB_LANG_KAZAK      = $3f; {Kazak}
 USB_LANG_SWAHILI    = $41; {Swahili}
 USB_LANG_UZBEK      = $43; {Uzbek}
 USB_LANG_TATAR      = $44; {Tatar}
 USB_LANG_BENGALI    = $45; {Bengali}
 USB_LANG_PUNJABI    = $46; {Punjabi}
 USB_LANG_GUJARATI   = $47; {Gujarati}
 USB_LANG_ORIYA      = $48; {Oriya}
 USB_LANG_TAMIL      = $49; {Tamil}
 USB_LANG_TELUGU     = $4a; {Telugu}
 USB_LANG_KANNADA    = $4b; {Kannada}
 USB_LANG_MALAYALAM  = $4c; {Malayalam}
 USB_LANG_ASSAMESE   = $4d; {Assamese}
 USB_LANG_MARATHI    = $4e; {Marathi}
 USB_LANG_SANSKRIT   = $4f; {Sanskrit}
 USB_LANG_KONKANI    = $57; {Konkani}
 USB_LANG_MANIPURI   = $58; {Manipuri}
 USB_LANG_SINDHI     = $59; {Sindhi}
 USB_LANG_KASHMIRI   = $60; {Kashmiri}
 USB_LANG_NEPALI     = $61; {Nepali}
 {Reserved $62-$fe}
 USB_LANG_HID        = $ff; {Reserved for USB HID Class use}
 {Reserved $100-$3ff}
 
 USB_PRIMARY_LANGUAGE_MASK = $3ff;
 
 {USB SubLanguage IDs (See Language Identifiers supplement to the USB 2.0 specification) (These are the upper 6 bits of the 16-bit language identifier)}
 {Reserved $00-$02}     
 USB_SUBLANG_ARABIC_SAUDI_ARABIA        = $01; {Arabic (Saudi Arabia)}
 USB_SUBLANG_ARABIC_IRAQ                = $02; {Arabic (Iraq)}  
 USB_SUBLANG_ARABIC_EGYPT               = $03; {Arabic (Egypt)}   
 USB_SUBLANG_ARABIC_LIBYA               = $04; {Arabic (Libya)}   
 USB_SUBLANG_ARABIC_ALGERIA             = $05; {Arabic (Algeria)}   
 USB_SUBLANG_ARABIC_MOROCCO             = $06; {Arabic (Morocco)}   
 USB_SUBLANG_ARABIC_TUNISIA             = $07; {Arabic (Tunisia)}   
 USB_SUBLANG_ARABIC_OMAN                = $08; {Arabic (Oman)}  
 USB_SUBLANG_ARABIC_YEMEN               = $09; {Arabic (Yemen)}   
 USB_SUBLANG_ARABIC_SYRIA               = $10; {Arabic (Syria)}   
 USB_SUBLANG_ARABIC_JORDAN              = $11; {Arabic (Jordan)}   
 USB_SUBLANG_ARABIC_LEBANON             = $12; {Arabic (Lebanon)}   
 USB_SUBLANG_ARABIC_KUWAIT              = $13; {Arabic (Kuwait)}  
 USB_SUBLANG_ARABIC_UAE                 = $14; {Arabic (U.A.E.)}   
 USB_SUBLANG_ARABIC_BAHRAIN             = $15; {Arabic (Bahrain)}   
 USB_SUBLANG_ARABIC_QATAR               = $16; {Arabic (Qatar)} 
 USB_SUBLANG_AZERI_CYRILLIC             = $01; {Azeri (Cyrillic)}   
 USB_SUBLANG_AZERI_LATIN                = $02; {Azeri (Latin)}   
 USB_SUBLANG_CHINESE_TRADITIONAL        = $01; {Chinese (Traditional)}   
 USB_SUBLANG_CHINESE_SIMPLIFIED         = $02; {Chinese (Simplified)} 
 USB_SUBLANG_CHINESE_HONGKONG           = $03; {Chinese (Hong Kong SAR, PRC)}
 USB_SUBLANG_CHINESE_SINGAPORE          = $04; {Chinese (Singapore)}   
 USB_SUBLANG_CHINESE_MACAU              = $05; {Chinese (Macau SAR)} 
 USB_SUBLANG_DUTCH                      = $01; {Dutch}    
 USB_SUBLANG_DUTCH_BELGIAN              = $02; {Dutch (Belgian)}   
 USB_SUBLANG_ENGLISH_US                 = $01; {English (US)}  
 USB_SUBLANG_ENGLISH_UK                 = $02; {English (UK)}  
 USB_SUBLANG_ENGLISH_AUS                = $03; {English (Australian)}   
 USB_SUBLANG_ENGLISH_CAN                = $04; {English (Canadian)}   
 USB_SUBLANG_ENGLISH_NZ                 = $05; {English (New Zealand)}  
 USB_SUBLANG_ENGLISH_EIRE               = $06; {English (Ireland)}   
 USB_SUBLANG_ENGLISH_SOUTH_AFRICA       = $07; {English (South Africa)}  
 USB_SUBLANG_ENGLISH_JAMAICA            = $08; {English (Jamaica)}   
 USB_SUBLANG_ENGLISH_CARIBBEAN          = $09; {English (Caribbean)}   
 USB_SUBLANG_ENGLISH_BELIZE             = $0a; {English (Belize)}   
 USB_SUBLANG_ENGLISH_TRINIDAD           = $0b; {English (Trinidad)}   
 USB_SUBLANG_ENGLISH_PHILIPPINES        = $0c; {English (Zimbabwe)}  
 USB_SUBLANG_ENGLISH_ZIMBABWE           = $0d; {English (Philippines)}   
 USB_SUBLANG_FRENCH                     = $01; {French}    
 USB_SUBLANG_FRENCH_BELGIAN             = $02; {French (Belgian)}   
 USB_SUBLANG_FRENCH_CANADIAN            = $03; {French (Canadian)}   
 USB_SUBLANG_FRENCH_SWISS               = $04; {French (Swiss)}   
 USB_SUBLANG_FRENCH_LUXEMBOURG          = $05; {French (Luxembourg)}   
 USB_SUBLANG_FRENCH_MONACO              = $06; {French (Monaco)}   
 USB_SUBLANG_GERMAN                     = $01; {German}    
 USB_SUBLANG_GERMAN_SWISS               = $02; {German (Swiss)}   
 USB_SUBLANG_GERMAN_AUSTRIAN            = $03; {German (Austrian)}   
 USB_SUBLANG_GERMAN_LUXEMBOURG          = $04; {German (Luxembourg)}   
 USB_SUBLANG_GERMAN_LIECHTENSTEIN       = $05; {German (Liechtenstein)}
 USB_SUBLANG_ITALIAN                    = $01; {Italian} 
 USB_SUBLANG_ITALIAN_SWISS              = $02; {Italian (Swiss)}
 USB_SUBLANG_KASHMIRI_INDIA             = $02; {Kashmiri (India)} 
 USB_SUBLANG_KOREAN                     = $01; {Korean} 
 USB_SUBLANG_LITHUANIAN                 = $01; {Lithuanian}  
 USB_SUBLANG_MALAY_MALAYSIA             = $01; {Malay (Malaysia)}
 USB_SUBLANG_MALAY_BRUNEI_DARUSSALAM    = $02; {Malay (Brunei Darassalam)}
 USB_SUBLANG_NEPALI_INDIA               = $02; {Nepali (India)} 
 USB_SUBLANG_NORWEGIAN_BOKMAL           = $01; {Norwegian (Bokmal)} 
 USB_SUBLANG_NORWEGIAN_NYNORSK          = $02; {Norwegian (Nynorsk)} 
 USB_SUBLANG_PORTUGUESE                 = $01; {Portuguese (Brazilian)} 
 USB_SUBLANG_PORTUGUESE_BRAZILIAN       = $02; {Portuguese}  
 USB_SUBLANG_SERBIAN_LATIN              = $02; {Serbian (Latin)}
 USB_SUBLANG_SERBIAN_CYRILLIC           = $03; {Serbian (Cyrillic)} 
 USB_SUBLANG_SPANISH                    = $01; {Spanish (Castilian)} 
 USB_SUBLANG_SPANISH_MEXICAN            = $02; {Spanish (Mexican)} 
 USB_SUBLANG_SPANISH_MODERN             = $03; {Spanish (Modern)} 
 USB_SUBLANG_SPANISH_GUATEMALA          = $04; {Spanish (Guatemala)} 
 USB_SUBLANG_SPANISH_COSTA_RICA         = $05; {Spanish (Costa Rica)} 
 USB_SUBLANG_SPANISH_PANAMA             = $06; {Spanish (Panama)} 
 USB_SUBLANG_SPANISH_DOMINICAN_REPUBLIC = $07; {Spanish (Dominican Republic)} 
 USB_SUBLANG_SPANISH_VENEZUELA          = $08; {Spanish (Venezuela)}
 USB_SUBLANG_SPANISH_COLOMBIA           = $09; {Spanish (Colombia)} 
 USB_SUBLANG_SPANISH_PERU               = $0a; {Spanish (Peru)} 
 USB_SUBLANG_SPANISH_ARGENTINA          = $0b; {Spanish (Argentina)}
 USB_SUBLANG_SPANISH_ECUADOR            = $0c; {Spanish (Ecuador)} 
 USB_SUBLANG_SPANISH_CHILE              = $0d; {Spanish (Chile)} 
 USB_SUBLANG_SPANISH_URUGUAY            = $0e; {Spanish (Uruguay)} 
 USB_SUBLANG_SPANISH_PARAGUAY           = $0f; {Spanish (Paraguay)} 
 USB_SUBLANG_SPANISH_BOLIVIA            = $10; {Spanish (Bolivia)} 
 USB_SUBLANG_SPANISH_EL_SALVADOR        = $11; {Spanish (El Salvador)}
 USB_SUBLANG_SPANISH_HONDURAS           = $12; {Spanish (Honduras)} 
 USB_SUBLANG_SPANISH_NICARAGUA          = $13; {Spanish (Nicaragua)}
 USB_SUBLANG_SPANISH_PUERTO_RICO        = $14; {Spanish (Puerto Rico)} 
 USB_SUBLANG_SWEDISH                    = $01; {Swedish}  
 USB_SUBLANG_SWEDISH_FINLAND            = $02; {Swedish (Finland)} 
 USB_SUBLANG_URDU_PAKISTAN              = $01; {Urdu (Pakistan)} 
 USB_SUBLANG_URDU_INDIA                 = $02; {Urdu (India)} 
 USB_SUBLANG_UZBEK_LATIN                = $01; {Uzbek (Latin)} 
 USB_SUBLANG_UZBEK_CYRILLIC             = $02; {Uzbek (Cyrillic)} 
 USB_SUBLANG_HID_USAGE_DATA_DESCRIPTOR  = $01; {HID (Usage Data Descriptor)}
 USB_SUBLANG_HID_VENDOR_DEFINED_1       = $3c; {HID (Vendor Defined 1)}
 USB_SUBLANG_HID_VENDOR_DEFINED_2       = $3d; {HID (Vendor Defined 2)}
 USB_SUBLANG_HID_VENDOR_DEFINED_3       = $3e; {HID (Vendor Defined 3)}
 USB_SUBLANG_HID_VENDOR_DEFINED_4       = $3f; {HID (Vendor Defined 4)}
 
 {USB Primary Language Identifiers (See Language Identifiers supplement to the USB 2.0 specification)}
 USB_LANGID_US_ENGLISH            = (USB_LANG_ENGLISH or (USB_SUBLANG_ENGLISH_US shl 10)); {English (US)}  
 USB_LANGID_UK_ENGLISH            = (USB_LANG_ENGLISH or (USB_SUBLANG_ENGLISH_UK shl 10)); {English (UK)}  
 USB_LANGID_AUS_ENGLISH           = (USB_LANG_ENGLISH or (USB_SUBLANG_ENGLISH_AUS shl 10)); {English (Australian)}   
 USB_LANGID_CAN_ENGLISH           = (USB_LANG_ENGLISH or (USB_SUBLANG_ENGLISH_CAN shl 10)); {English (Canadian)}   
 USB_LANGID_NZ_ENGLISH            = (USB_LANG_ENGLISH or (USB_SUBLANG_ENGLISH_NZ shl 10)); {English (New Zealand)}  
 USB_LANGID_EIRE_ENGLISH          = (USB_LANG_ENGLISH or (USB_SUBLANG_ENGLISH_EIRE shl 10)); {English (Ireland)}    
 USB_LANGID_SOUTH_AFRICA_ENGLISH  = (USB_LANG_ENGLISH or (USB_SUBLANG_ENGLISH_SOUTH_AFRICA shl 10)); {English (South Africa)}  
 USB_LANGID_JAMAICA_ENGLISH       = (USB_LANG_ENGLISH or (USB_SUBLANG_ENGLISH_JAMAICA shl 10)); {English (Jamaica)}   
 USB_LANGID_CARIBBEAN_ENGLISH     = (USB_LANG_ENGLISH or (USB_SUBLANG_ENGLISH_CARIBBEAN shl 10)); {English (Caribbean)}   
 USB_LANGID_BELIZE_ENGLISH        = (USB_LANG_ENGLISH or (USB_SUBLANG_ENGLISH_BELIZE shl 10)); {English (Belize)}   
 USB_LANGID_TRINIDAD_ENGLISH      = (USB_LANG_ENGLISH or (USB_SUBLANG_ENGLISH_TRINIDAD shl 10)); {English (Trinidad)}    
 USB_LANGID_PHILIPPINES_ENGLISH   = (USB_LANG_ENGLISH or (USB_SUBLANG_ENGLISH_PHILIPPINES shl 10)); {English (Philippines)}   
 USB_LANGID_ZIMBABWE_ENGLISH      = (USB_LANG_ENGLISH or (USB_SUBLANG_ENGLISH_ZIMBABWE shl 10)); {English (Zimbabwe)}   
 
 USB_LANGID_USAGE_HID             = (USB_LANG_HID or (USB_SUBLANG_HID_USAGE_DATA_DESCRIPTOR shl 10));
 USB_LANGID_VENDOR1_HID           = (USB_LANG_HID or (USB_SUBLANG_HID_VENDOR_DEFINED_1 shl 10)); 
 USB_LANGID_VENDOR2_HID           = (USB_LANG_HID or (USB_SUBLANG_HID_VENDOR_DEFINED_2 shl 10)); 
 USB_LANGID_VENDOR3_HID           = (USB_LANG_HID or (USB_SUBLANG_HID_VENDOR_DEFINED_3 shl 10)); 
 USB_LANGID_VENDOR4_HID           = (USB_LANG_HID or (USB_SUBLANG_HID_VENDOR_DEFINED_4 shl 10)); 
 
 {USB Vendor IDs} {Not a complete list}
 USB_VENDORID_REALTEK = $0BDA;  {Realtek}
 
 {USB tree output}
 USB_TREE_SPACES_PER_LEVEL = 6;
 USB_TREE_LINES_PER_PORT   = 2;
 
 {USB logging}
 USB_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {USB debugging messages}
 USB_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {USB informational messages, such as a device being attached or detached}
 USB_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {USB error messages}
 USB_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No USB messages}

var 
 USB_DEFAULT_LOG_LEVEL:LongWord = USB_LOG_LEVEL_DEBUG; {Minimum level for USB messages.  Only messages with level greater than or equal to this will be printed}
 
var 
 {USB logging}
 USB_LOG_ENABLED:Boolean; 
 
{==============================================================================}
const
 {USB Hub specific constants}
 USB_HUB_PREFIX = 'USBHub';  {Name prefix for USB Hubs}
 
 {USB Hub Types}
 USBHUB_TYPE_NONE      = 0;
 
 {USB Hub States}
 USBHUB_STATE_DETACHED  = 0;
 USBHUB_STATE_DETACHING = 1;
 USBHUB_STATE_ATTACHING = 2;
 USBHUB_STATE_ATTACHED  = 3;
 
 {USB Hub Flags}
 USBHUB_FLAG_NONE      = $00000000;
 
 USBHUB_THREAD_STACK_SIZE = SIZE_32K;                {Stack size of USB hub thread} 
 USBHUB_THREAD_PRIORITY  = THREAD_PRIORITY_HIGHEST;  {Priority of USB hub thread}
 USBHUB_THREAD_NAME = 'USB Hub';                     {Name of USB hub thread}

 USBHUB_DRIVER_NAME = 'USB Hub Driver';              {Name of USB hub driver}
 
 {USB_HUB_MAX_PORTS = 255;}     {USB 2.0 allows up to 255 ports per hub} {Not Required}
 
 USB_PORT_RESET_TIMEOUT  = 800; {Maximum milliseconds to wait for a port to reset (800 is the same value that Linux uses)}
 USB_PORT_RESET_DELAY    = 10;  {Milliseconds between each status check on the port while waiting for it to finish being reset (Linux uses several values, but 10 is the default case)}
 USB_PORT_RESET_RECOVERY = 30;  {Milliseconds to wait after port reset to allow the device attached to the port to recover before any data transfers. USB 2.0 spec says 10ms}
 
 {Values for wHubCharacteristics in type TUSBHubDescriptor}
 USB_HUB_CHARACTERISTIC_IS_COMPOUND_DEVICE = (1 shl 2);

 {USB Hub Features (See Table 11-17 in Section 11.24.2 of the USB 2.0 specification)}
 USB_C_HUB_LOCAL_POWER   = 0;
 USB_C_HUB_OVER_CURRENT  = 1;
  
 {USB Port Features (See Table 11-17 in Section 11.24.2 of the USB 2.0 specification)}
 USB_PORT_CONNECTION     = 0;
 USB_PORT_ENABLE         = 1;
 USB_PORT_SUSPEND        = 2;
 USB_PORT_OVER_CURRENT   = 3;
 USB_PORT_RESET          = 4;
 USB_PORT_POWER          = 8;
 USB_PORT_LOW_SPEED      = 9;
 USB_C_PORT_CONNECTION   = 16;
 USB_C_PORT_ENABLE       = 17;
 USB_C_PORT_SUSPEND      = 18;
 USB_C_PORT_OVER_CURRENT = 19;
 USB_C_PORT_RESET        = 20;
 USB_PORT_TEST           = 21;
 USB_PORT_INDICATOR      = 22;
 
 {USB Hub Class Requests (See Table 11-16 in Section 11.24.2 of the USB 2.0 specification)}
 USB_HUB_REQUEST_GET_STATUS       = 0;
 USB_HUB_REQUEST_CLEAR_FEATURE    = 1;
 USB_HUB_REQUEST_SET_FEATURE      = 3;
 USB_HUB_REQUEST_GET_DESCRIPTOR   = 6;
 USB_HUB_REQUEST_SET_DESCRIPTOR   = 7;
 USB_HUB_REQUEST_CLEAR_TT_BUFFER  = 8;
 USB_HUB_REQUEST_RESET_TT         = 9;
 USB_HUB_REQUEST_GET_TT_STATE     = 10;
 USB_HUB_REQUEST_STOP_TT          = 11;
 
 {Values for wPortStatus in type TUSBPortStatus (See Table 11-21 in Section 11.24.2.7.1 of the USB 2.0 specification)}
 USB_PORT_STATUS_CONNNECTED             = (1 shl 0);
 USB_PORT_STATUS_ENABLED                = (1 shl 1);
 USB_PORT_STATUS_SUSPENDED              = (1 shl 2);
 USB_PORT_STATUS_OVERCURRENT            = (1 shl 3);
 USB_PORT_STATUS_RESET                  = (1 shl 4);
 USB_PORT_STATUS_POWERED                = (1 shl 8);
 USB_PORT_STATUS_LOW_SPEED_ATTACHED     = (1 shl 9);
 USB_PORT_STATUS_HIGH_SPEED_ATTACHED    = (1 shl 10);
 USB_PORT_STATUS_TEST_MODE              = (1 shl 11);
 USB_PORT_STATUS_INDICATOR_CONTROL      = (1 shl 12);

 {Values for wPortChange in type TUSBPortStatus (See Table 11-20 in Section 11.24.2.6 of the USB 2.0 specification)}
 USB_PORT_CHANGE_CONNECTED      = (1 shl 0);
 USB_PORT_CHANGE_ENABLED        = (1 shl 1);
 USB_PORT_CHANGE_SUSPENDED      = (1 shl 2);
 USB_PORT_CHANGE_OVERCURRENT    = (1 shl 3);
 USB_PORT_CHANGE_RESET          = (1 shl 4);

 {Values for wHubStatus in type TUSBHubStatus (See Table 11-19 in Section 11.24.2.6 of the USB 2.0 specification)}
 USB_HUB_STATUS_LOCAL_POWER  = (1 shl 0);
 USB_HUB_STATUS_OVERCURRENT  = (1 shl 1);
 
 {Values for wHubChange in type TUSBHubStatus (See Table 11-20 in Section 11.24.2.6 of the USB 2.0 specification)}
 USB_HUB_CHANGE_LOCAL_POWER  = (1 shl 0);
 USB_HUB_CHANGE_OVERCURRENT  = (1 shl 1);

{==============================================================================}
type
 {USB Device, Driver and Host specific types}
 PUSBDeviceId = ^TUSBDeviceId;
 TUSBDeviceId = record
  idVendor:Word;
  idProduct:Word;
 end;

 {USB Control Request SETUP data (See Table 9-2 in Section 9.3 of the USB 2.0 specification)}
 PUSBControlSetupData = ^TUSBControlSetupData;
 TUSBControlSetupData = packed record
  bmRequestType:Byte;
  bRequest:Byte;
  wValue:Word;
  wIndex:Word;
  wLength:Word;
 end;

 {USB Device Descriptor Header (See Table 9-8 in 9.6.1 of the USB 2.0 specification)}
 PUSBDescriptorHeader = ^TUSBDescriptorHeader;
 TUSBDescriptorHeader = packed record
  bLength:Byte;
  bDescriptorType:Byte;
 end; 

 {USB Device Descriptor (See Table 9-8 in 9.6.1 of the USB 2.0 specification)}
 PUSBDeviceDescriptor = ^TUSBDeviceDescriptor;
 TUSBDeviceDescriptor = packed record
  bLength:Byte;
  bDescriptorType:Byte;
  bcdUSB:Word;
  bDeviceClass:Byte;
  bDeviceSubClass:Byte;
  bDeviceProtocol:Byte;
  bMaxPacketSize0:Byte;
  idVendor:Word;
  idProduct:Word;
  bcdDevice:Word;
  iManufacturer:Byte;
  iProduct:Byte;
  iSerialNumber:Byte;
  bNumConfigurations:Byte;
 end;

 {USB Configuration Descriptor (See Table 9-10 in Section 9.6.3 of the USB 2.0 specification)}
 PUSBConfigurationDescriptor = ^TUSBConfigurationDescriptor;
 TUSBConfigurationDescriptor = packed record
  bLength:Byte;
  bDescriptorType:Byte;
  wTotalLength:Word;
  bNumInterfaces:Byte;
  bConfigurationValue:Byte;
  iConfiguration:Byte;
  bmAttributes:Byte;
  bMaxPower:Byte;
 end; 

 {USB Interface Descriptor (See Table 9-12 in Section 9.6.6 of the USB 2.0 specification)}
 PUSBInterfaceDescriptor = ^TUSBInterfaceDescriptor;
 TUSBInterfaceDescriptor = packed record
  bLength:Byte;
  bDescriptorType:Byte;
  bInterfaceNumber:Byte;
  bAlternateSetting:Byte;
  bNumEndpoints:Byte;
  bInterfaceClass:Byte;
  bInterfaceSubClass:Byte;
  bInterfaceProtocol:Byte;
  iInterface:Byte;
 end;

 {USB Endpoint Descriptor (See Table 9-13 in Section 9.6.6 of the USB 2.0 specification)}
 PUSBEndpointDescriptor = ^TUSBEndpointDescriptor;
 TUSBEndpointDescriptor = packed record
  bLength:Byte;
  bDescriptorType:Byte;
  bEndpointAddress:Byte;
  bmAttributes:Byte;
  wMaxPacketSize:Word;
  bInterval:Byte;
 end;   

 {USB String Descriptor (See Table 9-16 in Section 9.7 of the USB 2.0 specification)}
 PUSBStringDescriptor = ^TUSBStringDescriptor;
 TUSBStringDescriptor = packed record
  bLength:Byte;
  bDescriptorType:Byte;
  bString:array[0..0] of Word; {UTF-16LE encoded string (specification says "UNICODE")}
 end; 
 
 PUSBStringDescriptorString = ^TUSBStringDescriptorString;
 TUSBStringDescriptorString = array[0..255] of Word; {Array type to allow typecasting of bString element in TUSBStringDescriptor}
 
 {Device status information returned by a USB_DEVICE_REQUEST_GET_STATUS control message (See Section 9.4.6 of the USB 2.0 specification)}
 PUSBDeviceStatus = ^TUSBDeviceStatus;
 TUSBDeviceStatus = packed record
  wStatus:Word;
 end; 
 
 {USB Device}
 PUSBHost = ^TUSBHost;                   {Forward declared to satisfy USBDevice}
 PUSBDriver = ^TUSBDriver;               {Forward declared to satisfy USBDevice}
 PUSBAlternate = ^TUSBAlternate;         {Forward declared to satisfy USBDevice}
 PUSBInterface = ^TUSBInterface;         {Forward declared to satisfy USBDevice}
 PUSBConfiguration = ^TUSBConfiguration; {Forward declared to satisfy USBDevice}
 PUSBDevice = ^TUSBDevice;
 
 {USB Device Bind Callback}
 TUSBDeviceBind = function(Device:PUSBDevice):LongWord;
 {USB Device Unbind Callback}
 TUSBDeviceUnbind = function(Device:PUSBDevice;Driver:PUSBDriver):LongWord;
 {USB Device Enumeration Callback}
 TUSBDeviceEnumerate = function(Device:PUSBDevice;Data:Pointer):LongWord;
 {USB Device Notification Callback}
 TUSBDeviceNotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;

 {USB Device Methods}
  {None}
 
 TUSBDevice = record 
  {Device Properties}
  Device:TDevice;                            {The Device entry for this USB device}
  {USB Properties}                           
  USBId:LongWord;                            {Unique Id of this USB in the USB device table}
  USBState:LongWord;                         {USB device state (eg USB_STATE_ATTACHED)}
  USBStatus:LongWord;                        {USB device status (eg USB_STATUS_BOUND)}
  Host:PUSBHost;                             {Host controller this USB device is connected to (Set by USB core)}
  Parent:PUSBDevice;                         {Hub this USB device is connected to, or nil if this is the Root Hub (Set by USB core)}
  Driver:PUSBDriver;                         {Driver this USB device is bound to, if any (Set by USB core)} 
  {Driver Properties}                        
  Lock:TMutexHandle;                         {USB device lock}
  Address:LongWord;                          {Address of this device (Set by USB core)}
  Speed:LongWord;                            {Speed at which this device is attached (Set by USB core)}
  Depth:LongWord;                            {Depth of this device (Root Hub is 0, next level hub is 1 etc) (Set by USB core)}
  PortNumber:LongWord;                       {1 based index of the USB port on the parent hub this device is plugged into (0 for the Root Hub) (Set by USB core)}
  ConfigurationValue:LongWord;               {The current configuration value of this USB device (Set by USB core)}
  Descriptor:PUSBDeviceDescriptor;           {Device descriptor of this device (Set by USB core)}
  Configuration:PUSBConfiguration;           {Currently selected configuration of this USB device (Set by USB core)}
  Configurations:array of PUSBConfiguration; {All available configurations on this device (Set by USB core)} 
  Product:array[0..127] of Char;             {Null terminated product string (ASCII encoded, English if available) of this device}
  Manufacturer:array[0..127] of Char;        {Null terminated manufacturer string (ASCII encoded, English if available) of this device}
  SerialNumber:array[0..127] of Char;        {Null terminated serial number string (ASCII encoded, English if available) of this device}
  DriverData:Pointer;                        {Private data for the driver of this USB device}
  LastError:LongWord;                        {Last error to occur on this device}
  PendingCount:LongWord;                     {Number of USB requests pending for this device (Set by USB core)}
  WaiterThread:TThreadId;                    {Thread waiting for pending requests to complete (for device detachment) (Set by USB core)}
  {Statistics Properties}                    
  RequestCount:LongWord;                     {Number of USB requests that have been submitted to this device} 
  RequestErrors:LongWord;                    {Number of USB requests that have failed on this device}
  {Internal Properties}                                                                               
  Prev:PUSBDevice;                           {Previous entry in USB device table}
  Next:PUSBDevice;                           {Next entry in USB device table}
 end;

 {USB Configuration}
 {PUSBConfiguration = ^TUSBConfiguration;} {Declared above for USBDevice}
 TUSBConfiguration = record
  {Driver Properties}                                
  Descriptor:PUSBConfigurationDescriptor;      {Configuration descriptor of this configuration (Set by USB core)}
  Interfaces:array of PUSBInterface;           {All available interfaces in this configuration (Set by USB core)}
  Description:array[0..127] of Char;           {Null terminated description string (ASCII encoded, English if available) of this configuration}
 end; 

 {USB Interface}
 {PUSBInterface = ^TUSBInterface;} {Declared above for USBConfiguration}
 TUSBInterface = record
  {USB Properties}                           
  Driver:PUSBDriver;                          {Driver this USB interface is bound to, if any (Set by USB core)}
  {Driver Properties}                                
  AlternateCount:LongWord;                    {The number of alternate settings available for this interface (Set by USB core)}
  AlternateSetting:LongWord;                  {The currently selected alternate setting for this interface (Set by USB core)}
  Descriptor:PUSBInterfaceDescriptor;         {Interface descriptor of this interface (Set by USB core)}
  Endpoints:array of PUSBEndpointDescriptor;  {All available endpoint descriptors on this interface (Set by USB core)}
  Alternates:array of PUSBAlternate;          {All available alternate settings for this interface (Set by USB core)}
  Description:array[0..127] of Char;          {Null terminated description string (ASCII encoded, English if available) of this interface}
  DriverData:Pointer;                         {Private data for the driver of this USB interface}
 end; 
 
 {USB Alternate}
 {PUSBAlternate = ^TUSBAlternate;} {Declared above for USBInterface}
 TUSBAlternate = record
  {Driver Properties}                                
  Descriptor:PUSBInterfaceDescriptor;         {Interface descriptor of this alternate setting (Set by USB core)}
  Endpoints:array of PUSBEndpointDescriptor;  {All available endpoint descriptors on this alternate setting (Set by USB core)}
  Description:array[0..127] of Char;          {Null terminated description string (ASCII encoded, English if available) of this alternate setting}
 end;
 
 {USB Driver}
 {PUSBDriver = ^TUSBDriver;} {Declared above for USBDevice}
 
 {USB Driver Enumeration Callback}
 TUSBDriverEnumerate = function(Driver:PUSBDriver;Data:Pointer):LongWord;
 
 {USB Driver Methods}
 TUSBDriverBind = function(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
 TUSBDriverUnbind = function(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
 
 TUSBDriver = record
  {Driver Properties}
  Driver:TDriver;                 {The Driver entry for this USB Driver}
  {USB Properties}
  DriverBind:TUSBDriverBind;      {A Driver specific DriverBind method implementing the standard USB driver interface}
  DriverUnbind:TUSBDriverUnbind;  {A Driver specific DriverUnbind method implementing the standard USB driver interface}
  {Interface Properties}
  Lock:TMutexHandle;              {Driver lock}
  {Internal Properties}                                                                        
  Prev:PUSBDriver;                {Previous entry in Driver table}
  Next:PUSBDriver;                {Next entry in Driver table}
 end;
 
 {USB Host}
 {PUSBHost = ^TUSBHost;} {Declared above for USBDevice}
 PUSBRequest = ^TUSBRequest; {Forward declared to satisfy USBHost}
 
 {USB Host Enumeration Callback}
 TUSBHostEnumerate = function(Host:PUSBHost;Data:Pointer):LongWord;
 {USB Host Notification Callback}
 TUSBHostNotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;
 
 {USB Host Methods}
 TUSBHostStart = function(Host:PUSBHost):LongWord;
 TUSBHostStop = function(Host:PUSBHost):LongWord;
 TUSBHostReset = function(Host:PUSBHost):LongWord;
 TUSBHostSubmit = function(Host:PUSBHost;Request:PUSBRequest):LongWord;
 TUSBHostCancel = function(Host:PUSBHost;Request:PUSBRequest):LongWord;
 
 TUSBHost = record
  {Device Properties}
  Device:TDevice;              {The Device entry for this USB Host}
  {USB Properties}
  HostId:LongWord;             {Unique Id of this Host in the Host table}
  HostState:LongWord;          {Host state (eg USBHOST_STATE_ENABLED)}
  HostStart:TUSBHostStart;     {A Host specific HostStart method implementing the standard USB host interface}
  HostStop:TUSBHostStop;       {A Host specific HostStop method implementing the standard USB host interface}
  HostReset:TUSBHostReset;     {A Host specific HostReset method implementing the standard USB host interface}
  HostSubmit:TUSBHostSubmit;   {A Host specific HostSubmit method implementing the standard USB host interface}
  HostCancel:TUSBHostCancel;   {A Host specific HostCancel method implementing the standard USB host interface}
  {Driver Properties}
  Lock:TMutexHandle;           {Host lock}
  RootHub:PUSBDevice;          {The Root hub for this Host (or nil if the Host has not yet been started)}
  Alignment:LongWord;          {Host data buffer alignment (for DMA requests etc)}
  Multiplier:LongWord;         {Host data buffer multiplier (for DMA requests etc)}
  {Statistics Properties}
  RequestCount:LongWord;       {Number of USB requests that have been submitted to this host} 
  RequestErrors:LongWord;      {Number of USB requests that have failed on this host}
  {Internal Properties}                                                                        
  Prev:PUSBHost;               {Previous entry in Host table}
  Next:PUSBHost;               {Next entry in Host table}
 end;
 
 {USB Request}
 {PUSBRequest = ^TUSBRequest;} {Declared above for USBHost}
 
 {USB Request Methods}
 TUSBRequestCompleted = procedure(Request:PUSBRequest); 
 
 TUSBRequest = record
  {Request Properties}
  Device:PUSBDevice;                         {USB Device to send this request to}
  Endpoint:PUSBEndpointDescriptor;           {Endpoint descriptor on the device to send this request to}
  Data:Pointer;                              {Data buffer for the request (IN or OUT)}
  Size:LongWord;                             {Size of data buffer (For IN requests, the maximum number of bytes of data to receive) (For OUT requests, the exact number of bytes of data to send)}
  Flags:LongWord;                            {Flags for the request (eg USB_REQUEST_FLAG_ALIGNED)}
  Callback:TUSBRequestCompleted;             {Callback function that will be called when this USB request has been successfully completed or has failed}
  DriverData:Pointer;                        {USB device driver private data for the completion callback (Optional)}
  {Control Properties}
  SetupData:PUSBControlSetupData;            {Data for the Setup phase of a USB control request (Must be provided for control requests, ignored otherwise)}
  StatusData:LongWord;                       {Data for the Status phase of a USB control request (For safety only as no data is normally transferred in the status phase)}
  {Result Properties}
  Status:LongWord;                           {Status of the transfer (USB_STATUS_SUCCESS if successful, or another error code if the transfer failed)}
  ActualSize:LongWord;                       {Actual size of the data transferred (Should be checked after a successful IN request)}
  {Driver Properties}                        {Private variables for use by Host drivers (Do not use from device drivers)}
  CurrentData:Pointer;                       
  CompleteSplit:LongBool;
  ShortAttempt:LongBool; 
  StartOfFrame:LongBool;
  ControlPhase:LongWord; 
  NextDataPID:LongWord; 
  AttemptedSize:LongWord;
  AttemptedPacketsRemaining:LongWord;
  AttemptedBytesRemaining:LongWord;
  CompleteSplitRetries:LongWord;
  ResubmitThread:TThreadHandle;
  ResubmitSemaphore:TSemaphoreHandle;
 end;
  
{==============================================================================}
type
 {USB Hub specific types}

 {USB Hub Descriptor (See Table 11-13 in Section 11.23 of the USB 2.0 specification)}
 PUSBHubDescriptor = ^TUSBHubDescriptor;
 TUSBHubDescriptor = packed record
  bDescLength:Byte;
  bDescriptorType:Byte;
  bNbrPorts:Byte;
  wHubCharacteristics:Word;
  bPwrOn2PwrGood:Byte;
  bHubContrCurrent:Byte;
  varData:array[0..0] of Byte; {Variable length field, 64 should be the maximum possible length (255 ports = 2 x 32 bytes of data)}
 end;  

 PUSBHubDescriptorData = ^TUSBHubDescriptorData;
 TUSBHubDescriptorData = array[0..63] of Byte; {Array type to allow typecasting of varData element in TUSBHubDescriptor}
 
 {USB Port Status (See Section 11.24.2.7 of the USB 2.0 specification)}
 PUSBPortStatus = ^TUSBPortStatus;
 TUSBPortStatus = packed record
  wPortStatus:Word; {See: USB_PORT_STATUS values above}
  wPortChange:Word; {See: USB_PORT_CHANGE values above}
 end; 
 
 {USB Hub Status (See Section 11.24.2.6 of the USB 2.0 specification)}
 PUSBHubStatus = ^TUSBHubStatus;
 TUSBHubStatus = packed record
  wHubStatus:Word; {See: USB_HUB_STATUS values above}
  wHubChange:Word; {See: USB_HUB_CHANGE values above}
 end;

 {USB Hub Status Change Data}
 PUSBHubData = ^TUSBHubData;
 TUSBHubData = record
  Data:array[0..7] of Byte;
 end;
 
 {USB Port}
 PUSBHub = ^TUSBHub; {Forward declared to satisfy USBPort}
 PUSBPort = ^TUSBPort;
 TUSBPort = record
  Hub:PUSBHub;            {Pointer to the USB hub this port is attached to}
  Number:Byte;            {Number of this port (1-based)}
  Child:PUSBDevice;       {Pointer to the USB device attached to this port, or nil if there is none}
  Status:TUSBPortStatus;  {Status of this port}
 end;

 {USB Hub}
 {PUSBHub = ^TUSBHub;} {Declared above for USBPort}
 
 {USB Hub Enumeration Callback}
 TUSBHubEnumerate = function(Hub:PUSBHub;Data:Pointer):LongWord;
 {USB Hub Notification Callback}
 TUSBHubNotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;

 {USB Hub Methods}
  {None}
 
 TUSBHub = record
  {Device Properties}
  Device:TDevice;                                   {The Device entry for this Hub}
  {Hub Properties}
  HubId:LongWord;                                   {Unique Id of this Hub in the Hub table}
  HubState:LongWord;                                {Hub state (eg USBHUB_STATE_ATTACHED)}
  {Driver Properties}
  Lock:TMutexHandle;                                {Hub lock}
  Descriptor:PUSBHubDescriptor;                     {Class specific Descriptor for this hub}
  Ports:array of TUSBPort;                          {Ports on this hub (Set by USBHubCreatePorts using the value in Descriptor.bNbrPorts)}
  {Statistics Properties}
  ReceiveCount:LongWord;
  ReceiveErrors:LongWord;
  {USB Properties}
  StatusData:PUSBHubData;                           {Hub status change data buffer}
  StatusRequest:PUSBRequest;                        {Hub status change request}
  StatusEndpoint:PUSBEndpointDescriptor;            {Hub Interrupt IN Endpoint}
  PendingCount:LongWord;                            {Number of USB requests pending for this hub}
  WaiterThread:TThreadId;                           {Thread waiting for pending requests to complete (for hub detachment)}
  {Internal Properties}                                                                        
  Prev:PUSBHub;                                     {Previous entry in Hub table}
  Next:PUSBHub;                                     {Next entry in Hub table}
 end;
 
{==============================================================================}
{var}
 {USB Device, Driver and Host specific variables}
 
{==============================================================================}
{var}
 {USB Hub specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure USBInit;
function USBStart:LongWord;
function USBStop:LongWord;

procedure USBAsyncStart(Host:PUSBHost);

{==============================================================================}
{USB Device, Driver and Host Functions}
//Device Methods
function USBDeviceGetAddress(Device:PUSBDevice):Byte;
function USBDeviceSetAddress(Device:PUSBDevice;Address:Byte):LongWord;

function USBDeviceGetDescriptor(Device:PUSBDevice;bRequest,bmRequestType:Byte;wValue,wIndex:Word;Data:Pointer;Length:Word):LongWord;

function USBDeviceGetDeviceDescriptor(Device:PUSBDevice;Data:Pointer;Length:Word):LongWord;

function USBDeviceCreateDeviceDescriptor(Device:PUSBDevice;Length:Word):LongWord;
function USBDeviceReadDeviceDescriptor(Device:PUSBDevice;Length:Word):LongWord; inline;
function USBDeviceReadDeviceDescriptorEx(Device:PUSBDevice;Length:Word;AllowShort:Boolean):LongWord;

function USBDeviceCreateConfigurations(Device:PUSBDevice):LongWord;
function USBDeviceReadConfigurations(Device:PUSBDevice):LongWord;

function USBDeviceCreateConfiguration(Device:PUSBDevice;Index:Byte;Size:Word):LongWord;
function USBDeviceReadConfiguration(Device:PUSBDevice;Index:Byte):LongWord;

function USBDeviceGetStringDescriptor(Device:PUSBDevice;Index:Byte;Data:Pointer;Length:Word):LongWord;

function USBDeviceReadStringDescriptor(Device:PUSBDevice;Index:Byte):String;
function USBDeviceReadStringDescriptorW(Device:PUSBDevice;Index:Byte):UnicodeString;

function USBDeviceGetConfigurationDescriptor(Device:PUSBDevice;Index:Byte;Data:Pointer;Length:Word):LongWord;

function USBDeviceGetConfiguration(Device:PUSBDevice;var ConfigurationValue:Byte):LongWord;
function USBDeviceSetConfiguration(Device:PUSBDevice;ConfigurationValue:Byte):LongWord;
function USBDeviceFindConfigurationByValue(Device:PUSBDevice;ConfigurationValue:Byte):PUSBConfiguration;

function USBDeviceGetInterface(Device:PUSBDevice;Index:Byte;var AlternateSetting:Byte):LongWord;
function USBDeviceSetInterface(Device:PUSBDevice;Index,AlternateSetting:Byte):LongWord;
function USBDeviceFindInterfaceByIndex(Device:PUSBDevice;Index:Byte):PUSBInterface;
function USBDeviceFindInterfaceByClass(Device:PUSBDevice;InterfaceClass,InterfaceSubClass,InterfaceProtocol:Byte):PUSBInterface;

function USBDeviceFindEndpointByIndex(Device:PUSBDevice;Interrface:PUSBInterface;Index:Byte):PUSBEndpointDescriptor;
function USBDeviceFindEndpointByType(Device:PUSBDevice;Interrface:PUSBInterface;Direction,TransferType:Byte):PUSBEndpointDescriptor;
function USBDeviceFindEndpointByTypeEx(Device:PUSBDevice;Interrface:PUSBInterface;Direction,TransferType:Byte;var Index:Byte):PUSBEndpointDescriptor;

function USBDeviceCountEndpointsByType(Device:PUSBDevice;Interrface:PUSBInterface;Direction,TransferType:Byte):Byte;

function USBDeviceFindAlternateByIndex(Device:PUSBDevice;Interrface:PUSBInterface;Index:Byte):PUSBAlternate;

function USBDeviceFindAlternateEndpointByIndex(Device:PUSBDevice;Interrface:PUSBInterface;Alternate:PUSBAlternate;Index:Byte):PUSBEndpointDescriptor;
function USBDeviceFindAlternateEndpointByType(Device:PUSBDevice;Interrface:PUSBInterface;Alternate:PUSBAlternate;Direction,TransferType:Byte):PUSBEndpointDescriptor;

function USBDeviceSetFeature(Device:PUSBDevice;Endpoint:PUSBEndpointDescriptor;Feature,Index:Word):LongWord;
function USBDeviceClearFeature(Device:PUSBDevice;Endpoint:PUSBEndpointDescriptor;Feature:Word):LongWord;

function USBDeviceSetState(Device:PUSBDevice;State:LongWord):LongWord;
function USBDeviceSetStatus(Device:PUSBDevice;Status:LongWord):LongWord;

function USBDeviceBind(Device:PUSBDevice):LongWord; 
function USBDeviceUnbind(Device:PUSBDevice;Driver:PUSBDriver):LongWord;

function USBDeviceAttach(Device:PUSBDevice):LongWord;
function USBDeviceDetach(Device:PUSBDevice):LongWord;

function USBDeviceAllocate(Host:PUSBHost;Parent:PUSBDevice):PUSBDevice;
function USBDeviceRelease(Device:PUSBDevice):LongWord;

function USBDeviceFind(USBId:LongWord):PUSBDevice;
function USBDeviceEnumerate(Callback:TUSBDeviceEnumerate;Data:Pointer):LongWord;

function USBDeviceNotification(Device:PUSBDevice;Callback:TUSBDeviceNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;

//Driver Methods
function USBDriverCreate:PUSBDriver;
function USBDriverCreateEx(Size:LongWord):PUSBDriver;
function USBDriverDestroy(Driver:PUSBDriver):LongWord;

function USBDriverRegister(Driver:PUSBDriver):LongWord;
function USBDriverDeregister(Driver:PUSBDriver):LongWord;

function USBDriverFind(DriverId:LongWord):PUSBDriver;
function USBDriverEnumerate(Callback:TUSBDriverEnumerate;Data:Pointer):LongWord;

//Host Methods
function USBHostSetState(Host:PUSBHost;State:LongWord):LongWord;

function USBHostCreate:PUSBHost;
function USBHostCreateEx(Size:LongWord):PUSBHost;
function USBHostDestroy(Host:PUSBHost):LongWord;

function USBHostRegister(Host:PUSBHost):LongWord;
function USBHostDeregister(Host:PUSBHost):LongWord;

function USBHostFind(HostId:LongWord):PUSBHost;
function USBHostEnumerate(Callback:TUSBHostEnumerate;Data:Pointer):LongWord;

function USBHostNotification(Host:PUSBHost;Callback:TUSBHostNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;

//Buffer Methods
function USBBufferAllocate(Device:PUSBDevice;Size:LongWord):Pointer; inline;
function USBBufferAllocateEx(Device:PUSBDevice;Size:LongWord;var Flags:LongWord):Pointer;
function USBBufferValidate(Device:PUSBDevice;Buffer:Pointer;Size:LongWord;var Flags:LongWord):LongWord;
function USBBufferRelease(Buffer:Pointer):LongWord;

//Request Methods
function USBRequestAllocate(Device:PUSBDevice;Endpoint:PUSBEndpointDescriptor;Callback:TUSBRequestCompleted;Size:LongWord;DriverData:Pointer):PUSBRequest; inline;
function USBRequestAllocateEx(Device:PUSBDevice;Endpoint:PUSBEndpointDescriptor;Callback:TUSBRequestCompleted;var Data:Pointer;Size:LongWord;DriverData:Pointer):PUSBRequest;
function USBRequestRelease(Request:PUSBRequest):LongWord;
function USBRequestInitialize(Request:PUSBRequest;Callback:TUSBRequestCompleted;Data:Pointer;Size:LongWord;DriverData:Pointer):LongWord;
function USBRequestInitializeOld(Request:PUSBRequest):LongWord; //To Do //Remove (Modify SMSC95XX first)

function USBRequestSubmit(Request:PUSBRequest):LongWord;
function USBRequestCancel(Request:PUSBRequest):LongWord;
procedure USBRequestComplete(Request:PUSBRequest);

//Control Methods
function USBControlRequest(Device:PUSBDevice;Endpoint:PUSBEndpointDescriptor;bRequest,bmRequestType:Byte;wValue,wIndex:Word;Data:Pointer;wLength:Word):LongWord; inline;
function USBControlRequestEx(Device:PUSBDevice;Endpoint:PUSBEndpointDescriptor;bRequest,bmRequestType:Byte;wValue,wIndex:Word;Data:Pointer;wLength:Word;Timeout:LongWord;AllowShort:Boolean):LongWord;
procedure USBControlRequestComplete(Request:PUSBRequest);

{==============================================================================}
{USB Hub Functions}
//Hub Methods
function USBHubCreatePorts(Hub:PUSBHub):LongWord;
function USBHubPowerOnPorts(Hub:PUSBHub):LongWord;

function USBHubCreateHubDescriptor(Hub:PUSBHub):LongWord;
function USBHubReadHubDescriptor(Hub:PUSBHub):LongWord;

function USBHubLock(Hub:PUSBHub):LongWord; 
function USBHubUnlock(Hub:PUSBHub):LongWord; 

function USBHubSetState(Hub:PUSBHub;State:LongWord):LongWord;

function USBHubAllocate(Device:PUSBDevice):PUSBHub;
function USBHubRelease(Hub:PUSBHub):LongWord;

function USBHubFind(HubId:LongWord):PUSBHub;
function USBHubEnumerate(Callback:TUSBHubEnumerate;Data:Pointer):LongWord;

function USBHubNotification(Hub:PUSBHub;Callback:TUSBHubNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;

procedure USBHubBindDevices(Device:PUSBDevice;Callback:TUSBDeviceBind);
procedure USBHubUnbindDevices(Device:PUSBDevice;Driver:PUSBDriver;Callback:TUSBDeviceUnbind);
procedure USBHubEnumerateDevices(Device:PUSBDevice;Callback:TUSBDeviceEnumerate;Data:Pointer);

//Hub Port Methods
function USBHubPortReset(Port:PUSBPort):LongWord;

function USBHubPortGetStatus(Port:PUSBPort):LongWord;

function USBHubPortSetFeature(Port:PUSBPort;Feature:Word):LongWord;
function USBHubPortClearFeature(Port:PUSBPort;Feature:Word):LongWord;
function USBHubPortChangeFeature(Port:PUSBPort;Feature:Word;Enable:Boolean):LongWord;

function USBHubPortAttachDevice(Port:PUSBPort):LongWord;
function USBHubPortDetachDevice(Port:PUSBPort):LongWord;

function USBHubPortStatusChanged(Port:PUSBPort):LongWord;

function USBHubExecute(Parameter:Pointer):PtrInt;
procedure USBHubStatusComplete(Request:PUSBRequest); 

function USBHubDriverBind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
function USBHubDriverUnbind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;

{==============================================================================}
{USB Device, Driver and Host Helper Functions}
function USBDeviceGetCount:LongWord; inline;

function USBDeviceCheck(Device:PUSBDevice):PUSBDevice;

function USBDriverGetCount:LongWord; inline;

function USBDriverCheck(Driver:PUSBDriver):PUSBDriver;

function USBHostGetCount:LongWord; inline;

function USBHostCheck(Host:PUSBHost):PUSBHost;

function USBIsHub(Device:PUSBDevice):Boolean;
function USBIsRootHub(Device:PUSBDevice):Boolean; 

function USBIsControlRequest(Request:PUSBRequest):Boolean;     
function USBIsBulkRequest(Request:PUSBRequest):Boolean;     
function USBIsInterruptRequest(Request:PUSBRequest):Boolean;  
function USBIsIsochronousRequest(Request:PUSBRequest):Boolean;     

function USBDeviceToString(Device:PUSBDevice):String;
function USBStatusToString(Status:LongWord):String;
function USBClassCodeToString(ClassCode:Integer):String;
function USBSubClassCodeToString(ClassCode,SubClassCode:Integer):String;
function USBProtocolCodeToString(ClassCode,ProtocolCode:Integer):String;
function USBSpeedToString(Speed:Integer):String;
function USBTransferTypeToString(TransferType:Integer):String;
function USBDirectionToString(Direction:Integer):String;
function USBBCDVersionToString(BCDVersion:Word):String;

function USBPortStatusConnectedToString(Status:Word):String;
//To Do //Critical //Merge all of these ? //Move to Hub ?
//function USBPortStatusEnabledToString(Status:Word):String;
//function USBPortStatusSuspendedToString(Status:Word):String;
//function USBPortStatusOvercurrentToString(Status:Word):String;
//function USBPortStatusResetToString(Status:Word):String;

function USBHubCharacteristicsToString(HubCharacteristics:Word):String;

function USBDeviceTypeToString(USBType:LongWord):String;
function USBDeviceStateToString(USBState:LongWord):String;
function USBDeviceStatusToString(USBStatus:LongWord):String;

function USBDeviceStateToNotification(State:LongWord):LongWord;
function USBDeviceStatusToNotification(Status:LongWord):LongWord;

function USBHostTypeToString(HostType:LongWord):String;
function USBHostStateToString(HostState:LongWord):String;

function USBHostStateToNotification(State:LongWord):LongWord;

procedure USBLog(Level:LongWord;Device:PUSBDevice;const AText:String);
procedure USBLogInfo(Device:PUSBDevice;const AText:String);
procedure USBLogError(Device:PUSBDevice;const AText:String);
procedure USBLogDebug(Device:PUSBDevice;const AText:String);

procedure USBLogDeviceConfiguration(Device:PUSBDevice);
procedure USBLogDeviceDescriptor(Device:PUSBDevice;Descriptor:PUSBDeviceDescriptor);
procedure USBLogConfigurationDescriptor(Device:PUSBDevice;Descriptor:PUSBConfigurationDescriptor);
procedure USBLogInterfaceDescriptor(Device:PUSBDevice;Descriptor:PUSBInterfaceDescriptor);
procedure USBLogEndpointDescriptor(Device:PUSBDevice;Descriptor:PUSBEndpointDescriptor);

function USBLogDevices:LongWord;

function USBLogDeviceCallback(Device:PUSBDevice;Data:Pointer):LongWord;
function USBLogTreeCallback(Device:PUSBDevice;Data:Pointer):LongWord;

{==============================================================================}
{USB Hub Helper Functions}
function USBHubGetCount:LongWord; inline;

function USBHubCheck(Hub:PUSBHub):PUSBHub;

function USBHubStateToNotification(State:LongWord):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {USB Device, Driver and Host specific variables}
 USBInitialized:Boolean;
 USBStarted:Boolean;

 USBDeviceTable:PUSBDevice;
 USBDeviceTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 USBDeviceTableCount:LongWord;
 
 USBDriverTable:PUSBDriver;
 USBDriverTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 USBDriverTableCount:LongWord;
 
 USBHostTable:PUSBHost;
 USBHostTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 USBHostTableCount:LongWord;
 
{==============================================================================}
{==============================================================================}
var
 {USB Hub specific variables}
 USBHubTable:PUSBHub;
 USBHubTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 USBHubTableCount:LongWord;

 USBHubThread:TThreadId = INVALID_HANDLE_VALUE; {Thread ID of the hub thread (USBHubExecute)}
 USBHubMessageslot:TMessageslotHandle = INVALID_HANDLE_VALUE;
 
 USBHubDriver:PUSBDriver;  {USB Hub Driver interface (Set by USBStart)} 

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure USBInit;
{Performs basic initialization of the USB core driver, after this devices, hosts
 and drivers can be registered however nothing will work until USBStart is called}
var
 Status:LongWord;
begin
 {}
 {Check Initialized}
 if USBInitialized then Exit;
 
 {Initialize Logging}
 USB_LOG_ENABLED:=(USB_DEFAULT_LOG_LEVEL <> USB_LOG_LEVEL_NONE); 
 
 {Initialize USB Device Table}
 USBDeviceTable:=nil;
 USBDeviceTableLock:=CriticalSectionCreate; 
 USBDeviceTableCount:=0;
 if USBDeviceTableLock = INVALID_HANDLE_VALUE then
  begin
   if USB_LOG_ENABLED then USBLogError(nil,'Failed to create USB device table lock');
  end;

 {Initialize USB Driver Table}
 USBDriverTable:=nil;
 USBDriverTableLock:=CriticalSectionCreate; 
 USBDriverTableCount:=0;
 if USBDriverTableLock = INVALID_HANDLE_VALUE then
  begin
   if USB_LOG_ENABLED then USBLogError(nil,'Failed to create USB driver table lock');
  end;
  
 {Initialize USB Host Table}
 USBHostTable:=nil;
 USBHostTableLock:=CriticalSectionCreate; 
 USBHostTableCount:=0;
 if USBHostTableLock = INVALID_HANDLE_VALUE then
  begin
   if USB_LOG_ENABLED then USBLogError(nil,'Failed to create USB host table lock');
  end;

 {Initialize USB Hub Table}
 USBHubTable:=nil;
 USBHubTableLock:=CriticalSectionCreate; 
 USBHubTableCount:=0;
 if USBHubTableLock = INVALID_HANDLE_VALUE then
  begin
   if USB_LOG_ENABLED then USBLogError(nil,'Failed to create USB hub table lock');
  end;

 {Create USB Hub Driver}
 USBHubDriver:=USBDriverCreate;
 if USBHubDriver <> nil then
  begin
   {Update USB Hub Driver}
   {Driver}
   USBHubDriver.Driver.DriverName:=USBHUB_DRIVER_NAME; 
   {USB}
   USBHubDriver.DriverBind:=USBHubDriverBind;
   USBHubDriver.DriverUnbind:=USBHubDriverUnbind;
   
   {Register USB Hub Driver}
   Status:=USBDriverRegister(USBHubDriver);
   if Status <> USB_STATUS_SUCCESS then
    begin
     if USB_LOG_ENABLED then USBLogError(nil,'Failed to register USB hub driver: ' + USBStatusToString(Status));
    end;
  end
 else
  begin
   if USB_LOG_ENABLED then USBLogError(nil,'Failed to create USB hub driver');
  end;
 
 USBInitialized:=True;
end;

{==============================================================================}

function USBStart:LongWord;
{Starts all registered USB hosts, starts the USB hub thread and begins the USB
 enumeration process. USB enumeration will continue after this function returns
 as devices are discovered by changes in hub status.}
var
 Host:PUSBHost;
 Status:LongWord;
 RootHub:PUSBDevice;
begin
 {}
 Result:=ERROR_SUCCESS;
 
 {Check Started}
 if USBStarted then Exit;
 
 Result:=ERROR_INVALID_PARAMETER;
 
 {Create Hub Messageslot}
 USBHubMessageslot:=MessageslotCreateEx(USB_HUB_MESSAGESLOT_MAXIMUM,MESSAGESLOT_FLAG_NONE);
 if USBHubMessageslot = INVALID_HANDLE_VALUE then
  begin
   Result:=ERROR_FUNCTION_FAILED;
   Exit;
  end;
 
 {Create Hub Thread}
 USBHubThread:=BeginThread(USBHubExecute,nil,USBHubThread,USBHUB_THREAD_STACK_SIZE);
 if USBHubThread = INVALID_HANDLE_VALUE then
  begin
   Result:=ERROR_FUNCTION_FAILED;
   Exit;
  end
 else
  begin
   ThreadSetPriority(USBHubThread,USBHUB_THREAD_PRIORITY);
   ThreadSetName(USBHubThread,USBHUB_THREAD_NAME);
  end;  
 
 {Acquire the Lock}
 if CriticalSectionLock(USBHostTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Host}
    Host:=USBHostTable;
    while Host <> nil do
     begin
      {Start Host}
      Status:=Host.HostStart(Host);
      if Status = USB_STATUS_SUCCESS then
       begin
        if USB_LOG_ENABLED then USBLogInfo(nil,'Successfully started USB host ' + DeviceGetName(@Host.Device));
        
        {Allocate Root Hub}
        RootHub:=USBDeviceAllocate(Host,nil);
        if RootHub <> nil then
         begin
          {$IFDEF USB_DEBUG}
          if USB_LOG_ENABLED then USBLogDebug(nil,'Attaching root hub to USB host ' + DeviceGetName(@Host.Device));         
          {$ENDIF}
          
          {Attach Root Hub}
          Status:=USBDeviceAttach(RootHub);
          if Status = USB_STATUS_SUCCESS then
           begin
            {Update Host}
            Host.RootHub:=RootHub;
            
            {Set State to Enabled}
            USBHostSetState(Host,USBHOST_STATE_ENABLED);
           end
          else
           begin          
            if USB_LOG_ENABLED then USBLogError(nil,'Failed to attach root hub to USB host ' + DeviceGetName(@Host.Device) + ' (Status=' + USBStatusToString(Status) + ')');
            
            {Release Device}
            USBDeviceRelease(RootHub);
            
            {Stop Host}
            Host.HostStop(Host);
           end;
         end
        else
         begin        
          if USB_LOG_ENABLED then USBLogError(nil,'Failed to allocate new USB device');
          
          {Stop Host}
          Host.HostStop(Host);
         end;
       end
      else
       begin
        if USB_LOG_ENABLED then USBLogError(nil,'Failed to start USB host ' + DeviceGetName(@Host.Device) + ' (Status=' + USBStatusToString(Status) + ')');
       end;
 
      {Get Next}
      Host:=Host.Next;
     end;
     
    {Set Started} 
    USBStarted:=True;
   finally
    {Release the Lock}
    CriticalSectionUnlock(USBHostTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
   Exit;
  end;  
 
 if USB_LOG_ENABLED then USBLogInfo(nil,'Successfully initialized USB subsystem'); 
 
 {Return Result} 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function USBStop:LongWord;
var
 Host:PUSBHost;
begin
 {}
 Result:=ERROR_SUCCESS;
 
 {Check Started}
 if not(USBStarted) then Exit;

 Result:=ERROR_INVALID_PARAMETER;

 //To Do //Critical
            
            //{Set State to Disabled}
            //USBHostSetState(Host,USBHOST_STATE_DISABLED);

 
 //To Do //Terminate USBHubThread
 
 //To DO //Destroy USBHubMessageslot
 
 //To Do

 {Set Started}
 USBStarted:=False;    
 
 {Return Result} 
 Result:=ERROR_SUCCESS;
end; 
 
{==============================================================================}
 
procedure USBAsyncStart(Host:PUSBHost);
var
 Status:LongWord;
 RootHub:PUSBDevice;
begin
 {}
 {Wait for Ready}
 while not(SysInitCompleted) do
  begin
   ThreadSleep(0);
  end;
  
 {Check Host}
 if Host = nil then
  begin
   {Start USB Subsystem}
   USBStart;
  end
 else
  begin
   {Check Host}
   if USBHostCheck(Host) <> Host then Exit;
   
   {Acquire the Lock}
   if CriticalSectionLock(USBHostTableLock) = ERROR_SUCCESS then
    begin
     try
      {Start Host}
      Status:=Host.HostStart(Host);
      if Status = USB_STATUS_SUCCESS then
       begin
        if USB_LOG_ENABLED then USBLogInfo(nil,'Successfully started USB host ' + DeviceGetName(@Host.Device));
        
        {Allocate Root Hub}
        RootHub:=USBDeviceAllocate(Host,nil);
        if RootHub <> nil then
         begin
          {$IFDEF USB_DEBUG}
          if USB_LOG_ENABLED then USBLogDebug(nil,'Attaching root hub to USB host ' + DeviceGetName(@Host.Device));         
          {$ENDIF}
          
          {Attach Root Hub}
          Status:=USBDeviceAttach(RootHub);
          if Status = USB_STATUS_SUCCESS then
           begin
            {Update Host}
            Host.RootHub:=RootHub;
            
            {Set State to Enabled}
            USBHostSetState(Host,USBHOST_STATE_ENABLED);
           end
          else
           begin          
            if USB_LOG_ENABLED then USBLogError(nil,'Failed to attach root hub to USB host ' + DeviceGetName(@Host.Device) + ' (Status=' + USBStatusToString(Status) + ')');
            
            {Release Device}
            USBDeviceRelease(RootHub);
            
            {Stop Host}
            Host.HostStop(Host);
           end;
         end
        else
         begin        
          if USB_LOG_ENABLED then USBLogError(nil,'Failed to allocate new USB device');
          
          {Stop Host}
          Host.HostStop(Host);
         end;
       end
      else
       begin
        if USB_LOG_ENABLED then USBLogError(nil,'Failed to start USB host ' + DeviceGetName(@Host.Device) + ' (Status=' + USBStatusToString(Status) + ')');
       end;
     finally
      {Release the Lock}
      CriticalSectionUnlock(USBHostTableLock);
     end;
    end;
  end;  
end;

{==============================================================================}
{==============================================================================}
{USB Device, Driver and Host Functions}
function USBDeviceGetAddress(Device:PUSBDevice):Byte;
{Get the bus address for the specified device}
{Device: The USB device to get the address for}
{Return: Device address or 0 on failure}
begin
 {}
 Result:=0;
 
 {Check Device}
 if Device = nil then Exit;
 if Device.USBId = DEVICE_ID_ANY then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Acquire the Lock}
 if MutexLock(Device.Lock) = ERROR_SUCCESS then
  begin
   try 
    {Get Address}
    Result:=Device.USBId + 1;
   finally
    {Release the Lock}
    MutexUnlock(Device.Lock);
   end;
  end;
end; 

{==============================================================================}

function USBDeviceSetAddress(Device:PUSBDevice;Address:Byte):LongWord;
{Set the bus address for the specified device}
{Device: The USB device to set the address for}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var
 Status:LongWord;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Set Address}
 Status:=USBControlRequest(Device,nil,USB_DEVICE_REQUEST_SET_ADDRESS,USB_BMREQUESTTYPE_DIR_OUT or USB_BMREQUESTTYPE_TYPE_STANDARD or USB_BMREQUESTTYPE_RECIPIENT_DEVICE,Address,0,nil,0);
 if Status <> USB_STATUS_SUCCESS then
  begin
   Result:=Status; 
   Exit;
  end;

 {Acquire the Lock}
 if MutexLock(Device.Lock) = ERROR_SUCCESS then
  begin
   try 
    {Set Address}
    Device.Address:=Address;

    {Return Result}
    Result:=USB_STATUS_SUCCESS;
   finally
    {Release the Lock}
    MutexUnlock(Device.Lock);
   end;
  end
 else
  begin
   Result:=USB_STATUS_OPERATION_FAILED;
  end;
end;

{==============================================================================}

function USBDeviceGetDescriptor(Device:PUSBDevice;bRequest,bmRequestType:Byte;wValue,wIndex:Word;Data:Pointer;Length:Word):LongWord;
{Read any descriptor from the specified device using USBControlRequest}
{Device: The USB device to read the descriptor from}
{bRequest: See USBControlRequest}
{bmRequestType: See USBControlRequest}
{wValue: See USBControlRequest}
{wIndex: See USBControlRequest}
{Data: See USBControlRequest}
{Length: See USBControlRequest}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var
 Size:Word;
 Status:LongWord;
 Header:TUSBDescriptorHeader;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Data}
 if Data = nil then Exit;
 
 {Check Length}
 if Length > SizeOf(TUSBDescriptorHeader) then
  begin
   {Get Descriptor Length}
   Status:=USBControlRequest(Device,nil,bRequest,bmRequestType,wValue,wIndex,@Header,SizeOf(TUSBDescriptorHeader));
   if Status <> USB_STATUS_SUCCESS then
    begin
     Result:=Status;
     Exit;
    end;    
  
   {Check Descriptor Length}
   if Header.bLength < SizeOf(TUSBDescriptorHeader) then
    begin
     if USB_LOG_ENABLED then USBLogError(Device,'Descriptor length too short');
     
     Result:=USB_STATUS_INVALID_DATA;
     Exit;
    end;
   
   {Determine Read Length}
   Size:=Header.bLength;
   if Length < Header.bLength then Size:=Length; //To Do //Add Min() function somewhere
  end
 else
  begin
   Size:=Length;
  end;  
  
 {Get Descriptor} 
 Result:=USBControlRequest(Device,nil,bRequest,bmRequestType,wValue,wIndex,Data,Size);
end;

{==============================================================================}

function USBDeviceGetDeviceDescriptor(Device:PUSBDevice;Data:Pointer;Length:Word):LongWord;
{Read all or part of the device descriptor from the specified device using USBControlRequest}
{Device: The USB device to read the device descriptor from}
{Data: See USBControlRequest}
{Length: See USBControlRequest}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Data}
 if Data = nil then Exit;
 
 {Get Descriptor}
 Result:=USBControlRequest(Device,nil,USB_DEVICE_REQUEST_GET_DESCRIPTOR,USB_BMREQUESTTYPE_DIR_IN or USB_BMREQUESTTYPE_TYPE_STANDARD or USB_BMREQUESTTYPE_RECIPIENT_DEVICE,(USB_DESCRIPTOR_TYPE_DEVICE shl 8),0,Data,Length);
end;

{==============================================================================}

function USBDeviceCreateDeviceDescriptor(Device:PUSBDevice;Length:Word):LongWord;
{Allocate a device descriptor for the specified device}
{Device: The USB device to create the device descriptor for}
{Length: The length of the descriptor to create}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Length}
 if Length < USB_ALTERNATE_MAX_PACKET_SIZE then Length:=USB_ALTERNATE_MAX_PACKET_SIZE;
 
 {Check Descriptor}
 Result:=USB_STATUS_OPERATION_FAILED;
 if Device.Descriptor <> nil then Exit;
 
 {Allocate Descriptor}
 Device.Descriptor:=USBBufferAllocate(Device,Length);
 if Device.Descriptor = nil then Exit;
    
 {Return Result} 
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function USBDeviceReadDeviceDescriptor(Device:PUSBDevice;Length:Word):LongWord; inline;
{Read all or part of the device descriptor from the specified device using USBControlRequest}
{Device: The USB device to read the device descriptor from}
{Length: The amount of the descriptor to read which may be less than the full size}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=USBDeviceReadDeviceDescriptorEx(Device,Length,False);
end;

{==============================================================================}

function USBDeviceReadDeviceDescriptorEx(Device:PUSBDevice;Length:Word;AllowShort:Boolean):LongWord;
{Read all or part of the device descriptor from the specified device using USBControlRequest}
{Device: The USB device to read the device descriptor from}
{Length: The amount of the descriptor to read which may be less than the full size}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Descriptor}
 if Device.Descriptor = nil then Exit;
 
 {Get Descriptor}
 Result:=USBControlRequestEx(Device,nil,USB_DEVICE_REQUEST_GET_DESCRIPTOR,USB_BMREQUESTTYPE_DIR_IN or USB_BMREQUESTTYPE_TYPE_STANDARD or USB_BMREQUESTTYPE_RECIPIENT_DEVICE,(USB_DESCRIPTOR_TYPE_DEVICE shl 8),0,Device.Descriptor,Length,INFINITE,AllowShort);
end;

{==============================================================================}

function USBDeviceCreateConfigurations(Device:PUSBDevice):LongWord;
{Allocate the available configurations for this device}
{Device: The USB device to create the configurations for}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Descriptor}
 if Device.Descriptor = nil then Exit;
 
 {Check Count}
 if Device.Descriptor.bNumConfigurations < 1 then Exit;
 
 {Check Configurations}
 if Length(Device.Configurations) > 0 then Exit;

 {Create Configurations}
 SetLength(Device.Configurations,Device.Descriptor.bNumConfigurations);
 
 {Return Result} 
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function USBDeviceReadConfigurations(Device:PUSBDevice):LongWord;
{Read and parse the available configurations for this device}
{Device: The USB device to read the configurations for}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var
 Count:LongWord;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Descriptor}
 if Device.Descriptor = nil then Exit;
 
 {Check Count}
 if Device.Descriptor.bNumConfigurations < 1 then Exit;
 
 {Check Configurations}
 if Length(Device.Configurations) = 0 then Exit;
 
 {Read Configurations}
 for Count:=0 to Device.Descriptor.bNumConfigurations - 1 do
  begin
   {Read Configuration}
   Result:=USBDeviceReadConfiguration(Device,Count);
   if Result <> USB_STATUS_SUCCESS then Exit;
 
   {Default Configuraton}
   if Count = 0 then
    begin
     Device.Configuration:=Device.Configurations[Count];
    end; 
  end;
 
 {Return Result} 
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function USBDeviceCreateConfiguration(Device:PUSBDevice;Index:Byte;Size:Word):LongWord;
{Allocate the specified configuration for this device}
{Device: The USB device to create the configuration for}
{Index: The index of the configuration to create}
{Size: The size of the configuration descriptor to create}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Descriptor}
 if Device.Descriptor = nil then Exit;
 
 {Check Count}
 if Device.Descriptor.bNumConfigurations < 1 then Exit;
 
 {Check Configurations}
 if Length(Device.Configurations) = 0 then Exit;

 {Check Index}
 if Index > (Device.Descriptor.bNumConfigurations - 1) then Exit;
 
 {Check Configuration}
 if Device.Configurations[Index] <> nil then Exit;
 
 Result:=USB_STATUS_OPERATION_FAILED;

 {Create Configuration}
 Device.Configurations[Index]:=AllocMem(SizeOf(TUSBConfiguration));
 if Device.Configurations[Index] = nil then Exit;
 
 {Create Descriptor}
 Device.Configurations[Index].Descriptor:=USBBufferAllocate(Device,Size);
 if Device.Configurations[Index].Descriptor = nil then Exit;
 
 {Return Result} 
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function USBDeviceReadConfiguration(Device:PUSBDevice;Index:Byte):LongWord;
{Read and parse the specified configuration for this device}
{Device: The USB device to read the configuration for}
{Index: The index of the configuration to read}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var
 Offset:PtrUInt;
 Status:LongWord;
 EndpointIndex:Integer;
 InterfaceIndex:Integer;
 AlternateSetting:LongWord;
 Descriptor:PUSBDescriptorHeader; 
 Configuration:TUSBConfigurationDescriptor;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Descriptor}
 if Device.Descriptor = nil then Exit;
 
 {Check Count}
 if Device.Descriptor.bNumConfigurations < 1 then Exit;
 
 {Check Configurations}
 if Length(Device.Configurations) = 0 then Exit;
 
 {Check Index}
 if Index > (Device.Descriptor.bNumConfigurations - 1) then Exit;
 
 {Get Descriptor Length}
 Status:=USBDeviceGetConfigurationDescriptor(Device,Index,@Configuration,SizeOf(TUSBConfigurationDescriptor));
 if Status <> USB_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;
 
 {Create Configuration}
 Status:=USBDeviceCreateConfiguration(Device,Index,Configuration.wTotalLength);
 if Status <> USB_STATUS_SUCCESS then 
  begin
   Result:=Status;
   Exit;
  end;

 {Get Descriptor} 
 Status:=USBDeviceGetConfigurationDescriptor(Device,Index,Device.Configurations[Index].Descriptor,Configuration.wTotalLength);
 if Status <> USB_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;

 {Acquire the Lock}
 if MutexLock(Device.Lock) = ERROR_SUCCESS then
  begin
   try 
    {Read and Parse Configuration}
    if Device.Configurations[Index].Descriptor.bNumInterfaces < 1 then
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'Invalid configuration descriptor (No interfaces available)');
      
      Result:=USB_STATUS_INVALID_DATA;
      Exit;
     end; 
    
    Result:=USB_STATUS_OPERATION_FAILED;
    
    {Check Interfaces}
    if Length(Device.Configurations[Index].Interfaces) > 0 then Exit;
    
    {Create Interfaces}
    SetLength(Device.Configurations[Index].Interfaces,Device.Configurations[Index].Descriptor.bNumInterfaces);
    
    {Get First Descriptor}
    Offset:=0;
    EndpointIndex:=-1;
    InterfaceIndex:=-1;
    AlternateSetting:=0;
    while (Offset + SizeOf(TUSBDescriptorHeader)) <= Configuration.wTotalLength do
     begin
      {Get Descriptor Header}
      Descriptor:=PUSBDescriptorHeader(PtrUInt(Device.Configurations[Index].Descriptor) + Offset);
    
      {Check Descriptor Length}
      if Descriptor.bLength < SizeOf(TUSBDescriptorHeader) then
       begin
        if USB_LOG_ENABLED then USBLogError(Device,'Invalid configuration descriptor (Descriptor length too small)');
     
        Result:=USB_STATUS_INVALID_DATA;
        Exit;
       end;
    
      {Check Descriptor Type}
      case Descriptor.bDescriptorType of
       USB_DESCRIPTOR_TYPE_INTERFACE:begin
         {Interface Descriptor}
         {Check Offset}
         if (Offset + SizeOf(TUSBInterfaceDescriptor)) > Configuration.wTotalLength then
          begin
           if USB_LOG_ENABLED then USBLogError(Device,'Invalid configuration descriptor (Interface exceeds total length)');
      
           Result:=USB_STATUS_INVALID_DATA;
           Exit;
          end;
      
         {Check Endpoint Count}
         if (InterfaceIndex >= 0) and (AlternateSetting = 0) and ((EndpointIndex + 1) <> Device.Configurations[Index].Interfaces[InterfaceIndex].Descriptor.bNumEndpoints) then
          begin
           if USB_LOG_ENABLED then USBLogError(Device,'Invalid configuration descriptor (Number of endpoints incorrect)');
           
           Result:=USB_STATUS_INVALID_DATA;
           Exit;
          end
         else //if (InterfaceIndex >= 0) and (AlternateSetting > 0)
          begin

           //To Do
           
          end;          
      
         {Check Alternate Setting}
         if PUSBInterfaceDescriptor(Descriptor).bAlternateSetting = 0 then
          begin
           {Increment Interface}
           Inc(InterfaceIndex);
           
           {Reset Endpoint}
           EndpointIndex:=-1;

           {Reset Alternate}
           AlternateSetting:=0;
           
           {Create Interface}
           Device.Configurations[Index].Interfaces[InterfaceIndex]:=AllocMem(SizeOf(TUSBInterface));
           if Device.Configurations[Index].Interfaces[InterfaceIndex] = nil then Exit;
          
           {Set Descriptor}
           Device.Configurations[Index].Interfaces[InterfaceIndex].Descriptor:=PUSBInterfaceDescriptor(Descriptor);
           
           {Check Endpoints}
           if Length(Device.Configurations[Index].Interfaces[InterfaceIndex].Endpoints) > 0 then Exit;
           
           {Create Endpoints}
           if Device.Configurations[Index].Interfaces[InterfaceIndex].Descriptor.bNumEndpoints > 0 then
            begin
             SetLength(Device.Configurations[Index].Interfaces[InterfaceIndex].Endpoints,Device.Configurations[Index].Interfaces[InterfaceIndex].Descriptor.bNumEndpoints);
            end; 
          end
         else
          begin
           {Check Interface}
           if InterfaceIndex < 0 then
            begin
             if USB_LOG_ENABLED then USBLogError(Device,'Invalid configuration descriptor (Alternate before Interface)');
          
             Result:=USB_STATUS_INVALID_DATA;
             Exit;
            end;

           {Reset Endpoint}
           EndpointIndex:=-1;

           {Update Alternate}
           AlternateSetting:=PUSBInterfaceDescriptor(Descriptor).bAlternateSetting;
           
           {Check Alternates}
           if AlternateSetting > Device.Configurations[Index].Interfaces[InterfaceIndex].AlternateCount then
            begin
             {Update Interface}
             Device.Configurations[Index].Interfaces[InterfaceIndex].AlternateCount:=AlternateSetting;
             
             {Create Alternates}
             SetLength(Device.Configurations[Index].Interfaces[InterfaceIndex].Alternates,AlternateSetting);
            end;
           
           {Create Alternate}
           Device.Configurations[Index].Interfaces[InterfaceIndex].Alternates[AlternateSetting - 1]:=AllocMem(SizeOf(TUSBAlternate));
           if Device.Configurations[Index].Interfaces[InterfaceIndex].Alternates[AlternateSetting - 1] = nil then Exit;
           
           {Set Descriptor}
           Device.Configurations[Index].Interfaces[InterfaceIndex].Alternates[AlternateSetting - 1].Descriptor:=PUSBInterfaceDescriptor(Descriptor);
           
           {Check Endpoints}
           if Length(Device.Configurations[Index].Interfaces[InterfaceIndex].Alternates[AlternateSetting - 1].Endpoints) > 0 then Exit;
           
           {Create Endpoints}
           if Device.Configurations[Index].Interfaces[InterfaceIndex].Alternates[AlternateSetting - 1].Descriptor.bNumEndpoints > 0 then
            begin
             SetLength(Device.Configurations[Index].Interfaces[InterfaceIndex].Alternates[AlternateSetting - 1].Endpoints,Device.Configurations[Index].Interfaces[InterfaceIndex].Alternates[AlternateSetting - 1].Descriptor.bNumEndpoints);
            end;
          end;          
        end;
       USB_DESCRIPTOR_TYPE_ENDPOINT:begin
         {Endpoint Descriptor}
         {Check Interface}
         if InterfaceIndex < 0 then
          begin
           if USB_LOG_ENABLED then USBLogError(Device,'Invalid configuration descriptor (Endpoint before Interface)');
        
           Result:=USB_STATUS_INVALID_DATA;
           Exit;
          end;
      
         {Check Offset}
         if (Offset + SizeOf(TUSBEndpointDescriptor)) > Configuration.wTotalLength then
          begin
           if USB_LOG_ENABLED then USBLogError(Device,'Invalid configuration descriptor (Endpoint exceeds total length)');
        
           Result:=USB_STATUS_INVALID_DATA;
           Exit;
          end;
      
         {Check Alternate Setting} 
         if AlternateSetting = 0 then
          begin
           {Increment Endpoint}
           Inc(EndpointIndex);
         
           {Set Descriptor}
           Device.Configurations[Index].Interfaces[InterfaceIndex].Endpoints[EndpointIndex]:=PUSBEndpointDescriptor(Descriptor);
          end
         else
          begin
           {Increment Endpoint}
           Inc(EndpointIndex);

           {Set Descriptor}
           Device.Configurations[Index].Interfaces[InterfaceIndex].Alternates[AlternateSetting - 1].Endpoints[EndpointIndex]:=PUSBEndpointDescriptor(Descriptor);
          end;          
        end;
      end;
 
      {Get Next Descriptor}
      Offset:=Offset + Descriptor.bLength;
     end;
 
    {Check Interface Count}
    if (InterfaceIndex + 1) <> Device.Configurations[Index].Descriptor.bNumInterfaces then
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'Invalid configuration descriptor (Number of interfaces incorrect)');
   
      Result:=USB_STATUS_INVALID_DATA;
      Exit;
     end;
    
    {Return Result}
    Result:=USB_STATUS_SUCCESS;
   finally
    {Release the Lock}
    MutexUnlock(Device.Lock);
   end;
  end
 else
  begin
   Result:=USB_STATUS_OPERATION_FAILED;
  end;
end;

{==============================================================================}

function USBDeviceGetStringDescriptor(Device:PUSBDevice;Index:Byte;Data:Pointer;Length:Word):LongWord;
{Read all or part of the specified string descriptor from the specified device using USBControlRequest}
{Device: The USB device to read the string descriptor from}
{Index: The index of the string descriptor to read}
{Data: See USBControlRequest}
{Length: See USBControlRequest}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Data}
 if Data = nil then Exit;
 
 {Get Descriptor}
 Result:=USBControlRequest(Device,nil,USB_DEVICE_REQUEST_GET_DESCRIPTOR,USB_BMREQUESTTYPE_DIR_IN or USB_BMREQUESTTYPE_TYPE_STANDARD or USB_BMREQUESTTYPE_RECIPIENT_DEVICE,(USB_DESCRIPTOR_TYPE_STRING shl 8) or Index,0,Data,Length);
end;
 
{==============================================================================}

function USBDeviceReadStringDescriptor(Device:PUSBDevice;Index:Byte):String;
begin
 {}
 Result:='';
 
 //Critical
 
end;

{==============================================================================}

function USBDeviceReadStringDescriptorW(Device:PUSBDevice;Index:Byte):UnicodeString;
begin
 {}
 Result:='';
 
 //Critical
 
end;
 
{==============================================================================}

function USBDeviceGetConfigurationDescriptor(Device:PUSBDevice;Index:Byte;Data:Pointer;Length:Word):LongWord;
{Read all or part of the specified configuration descriptor from the specified device using USBControlRequest}
{Device: The USB device to read the configuration descriptor from}
{Index: The index of the configuration descriptor to read}
{Data: See USBControlRequest}
{Length: See USBControlRequest}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Descriptor}
 if Device.Descriptor = nil then Exit;

 {Check Index}
 if Index > (Device.Descriptor.bNumConfigurations - 1) then Exit;

 {Check Data}
 if Data = nil then Exit;
 
 {Get Descriptor}
 Result:=USBControlRequest(Device,nil,USB_DEVICE_REQUEST_GET_DESCRIPTOR,USB_BMREQUESTTYPE_DIR_IN or USB_BMREQUESTTYPE_TYPE_STANDARD or USB_BMREQUESTTYPE_RECIPIENT_DEVICE,(USB_DESCRIPTOR_TYPE_CONFIGURATION shl 8) or Index,0,Data,Length);
end;

{==============================================================================}

function USBDeviceGetConfiguration(Device:PUSBDevice;var ConfigurationValue:Byte):LongWord;
{Get the current configuration for the specified device}
{Device: The USB device to get the current configuration for}
{ConfigurationValue: The current configuration (As per bConfigurationValue in the configuration descriptor)}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var
 Status:LongWord;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Get Configuration}
 Status:=USBControlRequest(Device,nil,USB_DEVICE_REQUEST_GET_CONFIGURATION,USB_BMREQUESTTYPE_DIR_IN or USB_BMREQUESTTYPE_TYPE_STANDARD or USB_BMREQUESTTYPE_RECIPIENT_DEVICE,0,0,@ConfigurationValue,SizeOf(Byte));
 if Status <> USB_STATUS_SUCCESS then
  begin
   Result:=Status; 
   Exit;
  end;
 
 {Return Result}
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function USBDeviceSetConfiguration(Device:PUSBDevice;ConfigurationValue:Byte):LongWord;
{Set the configuration for the specified device}
{Device: The USB device to set the configuration for}
{ConfigurationValue: The configuration to set (As per bConfigurationValue in the configuration descriptor)}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var
 Status:LongWord;
 Configuration:PUSBConfiguration;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Set Configuration}
 Status:=USBControlRequest(Device,nil,USB_DEVICE_REQUEST_SET_CONFIGURATION,USB_BMREQUESTTYPE_DIR_OUT or USB_BMREQUESTTYPE_TYPE_STANDARD or USB_BMREQUESTTYPE_RECIPIENT_DEVICE,ConfigurationValue,0,nil,0);
 if Status <> USB_STATUS_SUCCESS then
  begin
   Result:=Status; 
   Exit;
  end;

 {Find Configuration}
 Configuration:=USBDeviceFindConfigurationByValue(Device,ConfigurationValue);
 if Configuration = nil then Exit;
 
 {Acquire the Lock}
 if MutexLock(Device.Lock) = ERROR_SUCCESS then
  begin
   try 
    {Set Configuration}
    Device.Configuration:=Configuration;
    
    {Set Configuration Value}
    Device.ConfigurationValue:=ConfigurationValue;
    
    {Return Result}
    Result:=USB_STATUS_SUCCESS;
   finally
    {Release the Lock}
    MutexUnlock(Device.Lock);
   end;
  end
 else
  begin
   Result:=USB_STATUS_OPERATION_FAILED;
  end;
end;

{==============================================================================}

function USBDeviceFindConfigurationByValue(Device:PUSBDevice;ConfigurationValue:Byte):PUSBConfiguration;
{Find the configuration represented by configuration value for the specified device}
{Device: The USB device to find the configuration for}
{ConfigurationValue: The configuration value to find (As per bConfigurationValue in the configuration descriptor)}
{Return: USB Configuration if completed or nil on failure}
var
 Count:LongWord;
 Configuration:PUSBConfiguration;
begin
 {}
 Result:=nil;
 
 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Acquire the Lock}
 if MutexLock(Device.Lock) = ERROR_SUCCESS then
  begin
   try 
    {Find Configuration}
    for Count:=0 to Length(Device.Configurations) - 1 do
     begin
      {Check Configuration}
      Configuration:=Device.Configurations[Count];
      if Configuration <> nil then
       begin
        {Check Descriptor}
        if Configuration.Descriptor <> nil then
         begin
          {Check Configuration Value}
          if Configuration.Descriptor.bConfigurationValue = ConfigurationValue then
           begin
            {Return Result}
            Result:=Configuration;
            Exit;
           end;
         end;
       end;
     end;
   finally
    {Release the Lock}
    MutexUnlock(Device.Lock);
   end;
  end;
end;

{==============================================================================}

function USBDeviceGetInterface(Device:PUSBDevice;Index:Byte;var AlternateSetting:Byte):LongWord;
{Get the interface alternate setting for the specified device}
{Device: The USB device to get the interface alternate setting for}
{Index: The index of the interface to get (As per bInterfaceNumber in the interface descriptor)}
{AlternateSetting: The current alternate setting of the specified interface (As per bAlternateSetting in the interface descriptor)}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var
 Status:LongWord;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Get Interface}
 Status:=USBControlRequest(Device,nil,USB_DEVICE_REQUEST_GET_INTERFACE,USB_BMREQUESTTYPE_DIR_IN or USB_BMREQUESTTYPE_TYPE_STANDARD or USB_BMREQUESTTYPE_RECIPIENT_INTERFACE,0,Index,@AlternateSetting,SizeOf(Byte));
 if Status <> USB_STATUS_SUCCESS then
  begin
   Result:=Status; 
   Exit;
  end;
 
 {Return Result}
 Result:=USB_STATUS_SUCCESS;
end;
 
{==============================================================================}

function USBDeviceSetInterface(Device:PUSBDevice;Index,AlternateSetting:Byte):LongWord;
{Set the interface alternate setting for the specified device}
{Device: The USB device to set the interface alternate setting for}
{Index: The index of the interface to set (As per bInterfaceNumber in the interface descriptor)}
{AlternateSetting: The alternate setting to set on the specified interface (As per bAlternateSetting in the interface descriptor)}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var
 Status:LongWord;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Configuration}
 if Device.Configuration = nil then Exit;
 
 {Set Interface}
 Status:=USBControlRequest(Device,nil,USB_DEVICE_REQUEST_SET_INTERFACE,USB_BMREQUESTTYPE_DIR_OUT or USB_BMREQUESTTYPE_TYPE_STANDARD or USB_BMREQUESTTYPE_RECIPIENT_INTERFACE,AlternateSetting,Index,nil,0);
 if Status <> USB_STATUS_SUCCESS then
  begin
   Result:=Status; 
   Exit;
  end;
  
 {Acquire the Lock}
 if MutexLock(Device.Lock) = ERROR_SUCCESS then
  begin
   try 
    {Set Alternate Setting}
    Device.Configuration.Interfaces[Index].AlternateSetting:=AlternateSetting;
 
    {Return Result}
    Result:=USB_STATUS_SUCCESS;
   finally
    {Release the Lock}
    MutexUnlock(Device.Lock);
   end;
  end
 else
  begin
   Result:=USB_STATUS_OPERATION_FAILED;
  end;
end;

{==============================================================================}

function USBDeviceFindInterfaceByIndex(Device:PUSBDevice;Index:Byte):PUSBInterface;
{Find the interface with the specified index on the specified device}
{Device: The USB device to find the interface from}
{Index: The index of the interface to find  (As per bInterfaceNumber in the interface descriptor)}
{Return: The interface for the matching interface of nil if no interface matched}
var
 Count:LongWord;
 Interrface:PUSBInterface;
begin
 {}
 Result:=nil;
 
 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Configuration}
 if Device.Configuration = nil then Exit;
 
 {Acquire the Lock}
 if MutexLock(Device.Lock) = ERROR_SUCCESS then
  begin
   try 
    {Find Interface}
    for Count:=0 to Length(Device.Configuration.Interfaces) - 1 do
     begin
      {Check Interface}
      Interrface:=Device.Configuration.Interfaces[Count];
      if Interrface <> nil then
       begin
        {Check Interface Number}
        if Interrface.Descriptor.bInterfaceNumber = Index then
         begin
          {Return Result}
          Result:=Interrface;
          Exit;
         end;
       end;
     end;
   finally
    {Release the Lock}
    MutexUnlock(Device.Lock);
   end;
  end;
end;

{==============================================================================}

function USBDeviceFindInterfaceByClass(Device:PUSBDevice;InterfaceClass,InterfaceSubClass,InterfaceProtocol:Byte):PUSBInterface;
{Find an interface of the specified class, subclass and protocol on the specified device}
{Device: The USB device to find the interface from}
{InterfaceClass: The interface class to match}
{InterfaceSubClass: The interface subclass to match}
{InterfaceProtocol: The interface protocol to match}
{Return: The interface for the matching interface of nil if no interface matched}
var
 Count:LongWord;
 Interrface:PUSBInterface;
begin
 {}
 Result:=nil;
 
 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Configuration}
 if Device.Configuration = nil then Exit;
 
 {Acquire the Lock}
 if MutexLock(Device.Lock) = ERROR_SUCCESS then
  begin
   try 
    {Find Interface}
    for Count:=0 to Length(Device.Configuration.Interfaces) - 1 do
     begin
      {Check Interface}
      Interrface:=Device.Configuration.Interfaces[Count];
      if Interrface <> nil then
       begin
        {Check Interface Class}
        if Interrface.Descriptor.bInterfaceClass = InterfaceClass then
         begin
          {Check Interface SubClass}
          if Interrface.Descriptor.bInterfaceSubClass = InterfaceSubClass then
           begin
            {Check Interface Protocol}
            if Interrface.Descriptor.bInterfaceProtocol = InterfaceProtocol then
             begin
              Result:=Interrface;
              Exit;
             end;
           end;
         end;
       end;
     end;
   finally
    {Release the Lock}
    MutexUnlock(Device.Lock);
   end;
  end;
end;
       
{==============================================================================}

function USBDeviceFindEndpointByIndex(Device:PUSBDevice;Interrface:PUSBInterface;Index:Byte):PUSBEndpointDescriptor;
{Find the endpoint with the specified index on the specified interface of the specified device}
{Device: The USB device to find the endpoint from}
{Interrface: The interface to find the endpoint from}
{Index: The index of the endpoint to find (First endpoint is zero)}
{Return: The endpoint for the matching endpoint of nil if no endpoint matched}
begin
 {}
 Result:=nil;
 
 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Interface}
 if Interrface = nil then Exit;
 
 {Acquire the Lock}
 if MutexLock(Device.Lock) = ERROR_SUCCESS then
  begin
   try 
    {Check Index}
    if Index >= Length(Interrface.Endpoints) then Exit;
    
    {Get Endpoint}
    Result:=Interrface.Endpoints[Index];
   finally
    {Release the Lock}
    MutexUnlock(Device.Lock);
   end;
  end;
end;

{==============================================================================}

function USBDeviceFindEndpointByType(Device:PUSBDevice;Interrface:PUSBInterface;Direction,TransferType:Byte):PUSBEndpointDescriptor;
{Find an endpoint of the specified type and direction on the specified interface of the specified device}
{Device: The USB device to find the endpoint from}
{Interrface: The interface to find the endpoint from}
{Direction: The direction of the endpoint to find (eg USB_DIRECTION_OUT)}
{TransferType: The transfer type of the endpoint to find (eg USB_TRANSFER_TYPE_BULK)}
{Return: The endpoint for the matching endpoint of nil if no endpoint matched}
var
 Count:LongWord;
 Endpoint:PUSBEndpointDescriptor;
begin
 {}
 Result:=nil;
 
 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Interface}
 if Interrface = nil then Exit;
 
 {Acquire the Lock}
 if MutexLock(Device.Lock) = ERROR_SUCCESS then
  begin
   try 
    {Find Endpoint}
    for Count:=0 to Length(Interrface.Endpoints) - 1 do
     begin
      {Check Endpoint}
      Endpoint:=Interrface.Endpoints[Count];
      if Endpoint <> nil then
       begin
        {Check Direction}
        if (Endpoint.bEndpointAddress shr 7) = Direction then
         begin
          {Check Transfer Type}
          if (Endpoint.bmAttributes and USB_TRANSFER_TYPE_MASK) = TransferType then
           begin
            {Return Result}
            Result:=Endpoint;
            Exit;
           end;
         end;
       end;
     end;  
   finally
    {Release the Lock}
    MutexUnlock(Device.Lock);
   end;
  end;
end;

{==============================================================================}

function USBDeviceFindEndpointByTypeEx(Device:PUSBDevice;Interrface:PUSBInterface;Direction,TransferType:Byte;var Index:Byte):PUSBEndpointDescriptor;
{Find the next endpoint of the specified type and direction on the specified interface of the specified device}
{Device: The USB device to find the endpoint from}
{Interrface: The interface to find the endpoint from}
{Direction: The direction of the endpoint to find (eg USB_DIRECTION_OUT)}
{TransferType: The transfer type of the endpoint to find (eg USB_TRANSFER_TYPE_BULK)}
{Index: The index returned from the last call, pass 0 on the first call]
{Return: The endpoint for the matching endpoint of nil if no endpoint matched}
var
 Count:LongWord;
 Endpoint:PUSBEndpointDescriptor;
begin
 {}
 Result:=nil;
 
 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Interface}
 if Interrface = nil then Exit;
 
 {Acquire the Lock}
 if MutexLock(Device.Lock) = ERROR_SUCCESS then
  begin
   try 
    {Find Endpoint}
    for Count:=0 to Length(Interrface.Endpoints) - 1 do
     begin
      {Check Endpoint}
      Endpoint:=Interrface.Endpoints[Count];
      if Endpoint <> nil then
       begin
        {Check Direction}
        if (Endpoint.bEndpointAddress shr 7) = Direction then
         begin
          {Check Transfer Type}
          if (Endpoint.bmAttributes and USB_TRANSFER_TYPE_MASK) = TransferType then
           begin
            {Check Index}
            if (Count >= Index) then
             begin
              {Update Index}
              Index:=Count + 1;
              
              {Return Result}
              Result:=Endpoint;
              Exit;
             end; 
           end;
         end;
       end;
     end;  
   finally
    {Release the Lock}
    MutexUnlock(Device.Lock);
   end;
  end;
end;

{==============================================================================}

function USBDeviceCountEndpointsByType(Device:PUSBDevice;Interrface:PUSBInterface;Direction,TransferType:Byte):Byte;
{Count the number of endpoints of the specified type and direction on the specified interface of the specified device}
{Device: The USB device to find the endpoint from}
{Interrface: The interface to find the endpoint from}
{Direction: The direction of the endpoint to find (eg USB_DIRECTION_OUT)}
{TransferType: The transfer type of the endpoint to find (eg USB_TRANSFER_TYPE_BULK)}
{Return: The number of matching endpoints on the specified interface}
var
 Count:LongWord;
 Endpoint:PUSBEndpointDescriptor;
begin
 {}
 Result:=0;
 
 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Interface}
 if Interrface = nil then Exit;
 
 {Acquire the Lock}
 if MutexLock(Device.Lock) = ERROR_SUCCESS then
  begin
   try 
    {Find Endpoint}
    for Count:=0 to Length(Interrface.Endpoints) - 1 do
     begin
      {Check Endpoint}
      Endpoint:=Interrface.Endpoints[Count];
      if Endpoint <> nil then
       begin
        {Check Direction}
        if (Endpoint.bEndpointAddress shr 7) = Direction then
         begin
          {Check Transfer Type}
          if (Endpoint.bmAttributes and USB_TRANSFER_TYPE_MASK) = TransferType then
           begin
            {Update Result}
            Inc(Result);
           end;
         end;
       end;
     end;  
   finally
    {Release the Lock}
    MutexUnlock(Device.Lock);
   end;
  end;
end;

{==============================================================================}

function USBDeviceFindAlternateByIndex(Device:PUSBDevice;Interrface:PUSBInterface;Index:Byte):PUSBAlternate;
{Find the alternate setting with the specified index on the specified interface of the specified device}
{Device: The USB device to find the alternate setting from}
{Interrface: The interface to find the alternate setting from}
{Index: The index of the alternate setting to find (First alternate setting is zero)}
{Return: The alternate setting for the matching alternate setting of nil if no alternate setting matched}
begin
 {}
 Result:=nil;
 
 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Interface}
 if Interrface = nil then Exit;
 
 {Acquire the Lock}
 if MutexLock(Device.Lock) = ERROR_SUCCESS then
  begin
   try 
    {Check Index}
    if Index >= Length(Interrface.Alternates) then Exit;
    
    {Get Alternate}
    Result:=Interrface.Alternates[Index];
   finally
    {Release the Lock}
    MutexUnlock(Device.Lock);
   end;
  end;
end;

{==============================================================================}

function USBDeviceFindAlternateEndpointByIndex(Device:PUSBDevice;Interrface:PUSBInterface;Alternate:PUSBAlternate;Index:Byte):PUSBEndpointDescriptor;
{Find the endpoint with the specified index on the specified alternate setting interface of the specified device}
{Device: The USB device to find the endpoint from}
{Interrface: The interface to find the endpoint from}
{Alternate: The alternate setting to find the endpoint from}
{Index: The index of the endpoint to find (First endpoint is zero)}
{Return: The endpoint for the matching endpoint of nil if no endpoint matched}
begin
 {}
 Result:=nil;
 
 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Interface}
 if Interrface = nil then Exit;
 
 {Check Alternate}
 if Alternate = nil then Exit;
 
 {Acquire the Lock}
 if MutexLock(Device.Lock) = ERROR_SUCCESS then
  begin
   try 
    {Check Index}
    if Index >= Length(Alternate.Endpoints) then Exit;
    
    {Get Endpoint}
    Result:=Alternate.Endpoints[Index];
   finally
    {Release the Lock}
    MutexUnlock(Device.Lock);
   end;
  end;
end;

{==============================================================================}

function USBDeviceFindAlternateEndpointByType(Device:PUSBDevice;Interrface:PUSBInterface;Alternate:PUSBAlternate;Direction,TransferType:Byte):PUSBEndpointDescriptor;
{Find an endpoint of the specified type and direction on the specified alternate setting interface of the specified device}
{Device: The USB device to find the endpoint from}
{Interrface: The interface to find the endpoint from}
{Alternate: The alternate setting to find the endpoint from}
{Direction: The direction of the endpoint to find (eg USB_DIRECTION_OUT)}
{TransferType: The transfer type of the endpoint to find (eg USB_TRANSFER_TYPE_BULK)}
{Return: The endpoint for the matching endpoint of nil if no endpoint matched}
var
 Count:LongWord;
 Endpoint:PUSBEndpointDescriptor;
begin
 {}
 Result:=nil;
 
 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Interface}
 if Interrface = nil then Exit;
 
 {Check Alternate}
 if Alternate = nil then Exit;
 
 {Acquire the Lock}
 if MutexLock(Device.Lock) = ERROR_SUCCESS then
  begin
   try 
    {Find Endpoint}
    for Count:=0 to Length(Alternate.Endpoints) - 1 do
     begin
      {Check Endpoint}
      Endpoint:=Alternate.Endpoints[Count];
      if Endpoint <> nil then
       begin
        {Check Direction}
        if (Endpoint.bEndpointAddress shr 7) = Direction then
         begin
          {Check Transfer Type}
          if (Endpoint.bmAttributes and USB_TRANSFER_TYPE_MASK) = TransferType then
           begin
            {Return Result}
            Result:=Endpoint;
            Exit;
           end;
         end;
       end;
     end;  
   finally
    {Release the Lock}
    MutexUnlock(Device.Lock);
   end;
  end;
end;

{==============================================================================}

function USBDeviceSetFeature(Device:PUSBDevice;Endpoint:PUSBEndpointDescriptor;Feature,Index:Word):LongWord;
{Enable a feature on the specified endpoint on the specified device}
{Device: The USB device to enable the feature for}
{Endpoint: The endpoint to enable the feature on}
{Feature: The feature to enable}
{Index: ??????}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Endpoint}
 if Endpoint = nil then
  begin
   {Set Feature}
   Result:=USBControlRequest(Device,nil,USB_DEVICE_REQUEST_SET_FEATURE,USB_BMREQUESTTYPE_DIR_OUT or USB_BMREQUESTTYPE_TYPE_STANDARD or USB_BMREQUESTTYPE_RECIPIENT_DEVICE,Feature,(Index shl 8),nil,0);
  end
 else
  begin
   {Set Feature}
   Result:=USBControlRequest(Device,nil,USB_DEVICE_REQUEST_SET_FEATURE,USB_BMREQUESTTYPE_DIR_OUT or USB_BMREQUESTTYPE_TYPE_STANDARD or USB_BMREQUESTTYPE_RECIPIENT_ENDPOINT,Feature,(Index shl 8) or Endpoint.bEndpointAddress,nil,0);
  end;  
end;
 
{==============================================================================}

function USBDeviceClearFeature(Device:PUSBDevice;Endpoint:PUSBEndpointDescriptor;Feature:Word):LongWord;
{Disable a feature on the specified endpoint on the specified device}
{Device: The USB device to disable the feature for}
{Endpoint: The endpoint to disable the feature on}
{Feature: The feature to disable}
{Index: ??????}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Endpoint}
 if Endpoint = nil then
  begin
   {Clear Feature}
   Result:=USBControlRequest(Device,nil,USB_DEVICE_REQUEST_CLEAR_FEATURE,USB_BMREQUESTTYPE_DIR_OUT or USB_BMREQUESTTYPE_TYPE_STANDARD or USB_BMREQUESTTYPE_RECIPIENT_DEVICE,Feature,0,nil,0);
  end
 else
  begin
   {Clear Feature}
   Result:=USBControlRequest(Device,nil,USB_DEVICE_REQUEST_CLEAR_FEATURE,USB_BMREQUESTTYPE_DIR_OUT or USB_BMREQUESTTYPE_TYPE_STANDARD or USB_BMREQUESTTYPE_RECIPIENT_ENDPOINT,Feature,Endpoint.bEndpointAddress,nil,0);
  end;  
end;

{==============================================================================}

function USBDeviceSetState(Device:PUSBDevice;State:LongWord):LongWord;
{Set the state of the specified device and send a notification}
{Device: The USB device to set the state for}
{State: The new state to set and notify}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check State}
 if State > USB_STATE_ATTACHED then Exit;
 
 {Check State}
 if Device.USBState = State then
  begin
   {Return Result}
   Result:=USB_STATUS_SUCCESS;
  end
 else
  begin
   {Acquire the Lock}
   if MutexLock(Device.Lock) = ERROR_SUCCESS then
    begin
     try 
      {Set State}
      Device.USBState:=State;
  
      {Notify State}
      NotifierNotify(@Device.Device,USBDeviceStateToNotification(State));

      {Return Result}
      Result:=USB_STATUS_SUCCESS;
     finally
      {Release the Lock}
      MutexUnlock(Device.Lock);
     end;
    end
   else
    begin
     Result:=USB_STATUS_OPERATION_FAILED;
    end;
  end;  
end;

{==============================================================================}

function USBDeviceSetStatus(Device:PUSBDevice;Status:LongWord):LongWord;
{Set the status of the specified device and send a notification}
{Device: The USB device to set the status for}
{Status: The new status to set and notify}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Status}
 if Status > USB_STATUS_BOUND then Exit;
 
 {Check Status}
 if Device.USBStatus = Status then
  begin
   {Return Result}
   Result:=USB_STATUS_SUCCESS;
  end
 else
  begin
   {Acquire the Lock}
   if MutexLock(Device.Lock) = ERROR_SUCCESS then
    begin
     try 
      {Set Status}
      Device.USBStatus:=Status;
  
      {Notify Status}
      NotifierNotify(@Device.Device,USBDeviceStatusToNotification(Status));

      {Return Result}
      Result:=USB_STATUS_SUCCESS;
     finally
      {Release the Lock}
      MutexUnlock(Device.Lock);
     end;
    end
   else
    begin
     Result:=USB_STATUS_OPERATION_FAILED;
    end;
  end;  
end;

{==============================================================================}

function USBDeviceBind(Device:PUSBDevice):LongWord;
{Attempt to bind a device to one of the registers drivers}
{Device: The device to attempt to bind a driver to}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var
 Bound:Boolean;
 Count:LongWord;
 Status:LongWord;
 Driver:PUSBDriver;
 Interrface:PUSBInterface;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Device}
 if USBDeviceCheck(Device) <> Device then Exit;
 
 {Check Driver}
 if Device.Driver <> nil then
  begin
   {Already bound}
   Result:=USB_STATUS_SUCCESS;
   Exit;
  end; 

 {Set Default}
 Bound:=False;
 Status:=USB_STATUS_DEVICE_UNSUPPORTED;
 
 //Critical //Lock the Device table (Can't lock the Device because Bind will call Submit !)
 
 {Acquire the Lock}
 if CriticalSectionLock(USBDriverTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Driver}
    Driver:=USBDriverTable;
    while Driver <> nil do
     begin
      {$IFDEF USB_DEBUG}      
      if USB_LOG_ENABLED then USBLogDebug(Device,'Attempting to bind ' + DriverGetName(@Driver.Driver) + ' to device');
      {$ENDIF}

      {Attempt to Bind (Device)}
      Status:=Driver.DriverBind(Device,nil);
      if Status <> USB_STATUS_DEVICE_UNSUPPORTED then
       begin
        if Status = USB_STATUS_SUCCESS then
         begin
          if USB_LOG_ENABLED then USBLogInfo(nil,'Bound ' + DriverGetName(@Driver.Driver) + ' to ' + USBDeviceToString(Device));
          
          {Set Driver}
          Device.Driver:=Driver;
          
          {Set Bound}
          Bound:=True;
          
          {Set Status to Bound}
          USBDeviceSetStatus(Device,USB_STATUS_BOUND);
         end;
         
        Break; {Break to return Status}
       end;
      
      {Get Next}
      Driver:=Driver.Next;
     end;
    
    {Check Status}
    if Status = USB_STATUS_DEVICE_UNSUPPORTED then 
     begin
      {Check Configuration}
      if Device.ConfigurationValue = 0 then
       begin
        {$IFDEF USB_DEBUG}
        if USB_LOG_ENABLED then USBLogDebug(Device,'Assigning configuration ' + IntToStr(Device.Configuration.Descriptor.bConfigurationValue) + ' (' + IntToStr(Device.Configuration.Descriptor.bNumInterfaces) + ' interfaces available)');
        {$ENDIF}
        
        {Set Configuration}
        Status:=USBDeviceSetConfiguration(Device,Device.Configuration.Descriptor.bConfigurationValue);
        if Status <> USB_STATUS_SUCCESS then
         begin
          if USB_LOG_ENABLED then USBLogError(Device,'Failed to set device configuration: ' + USBStatusToString(Status));
          
          {Return Result}
          Result:=Status;
          Exit;
         end;
       end;
     
      {Get Driver}
      Driver:=USBDriverTable;
      while Driver <> nil do
       begin
        {Get Interface}
        for Count:=0 to Length(Device.Configuration.Interfaces) - 1 do
         begin
          Interrface:=Device.Configuration.Interfaces[Count];
          if Interrface <> nil then
           begin
            {Check Driver}
            if Interrface.Driver = nil then
             begin
              {$IFDEF USB_DEBUG}      
              if USB_LOG_ENABLED then USBLogDebug(Device,'Attempting to bind ' + DriverGetName(@Driver.Driver) + ' to device on interface ' + IntToStr(Count));
              {$ENDIF}
        
              {Attempt to Bind (Interface)}
              Status:=Driver.DriverBind(Device,Interrface);
              if Status <> USB_STATUS_DEVICE_UNSUPPORTED then
               begin
                {Check Status}
                if Status = USB_STATUS_SUCCESS then
                 begin
                  if USB_LOG_ENABLED then USBLogInfo(nil,'Bound ' + DriverGetName(@Driver.Driver) + ' to ' + USBDeviceToString(Device) + ' (Interface=' + IntToStr(Count) + ')');
  
                  {Set Driver}
                  Interrface.Driver:=Driver;
                  
                  {Set Bound}
                  Bound:=True;
                 end
                else
                 begin
                  {Return Error}
                  Result:=Status;
                  Exit;
                 end;
               end;
             end;
           end;
         end;
        
        {Get Next}
        Driver:=Driver.Next;
       end;
       
      {Check Bound}
      if Bound then
       begin
        {Set Status to Bound}
        USBDeviceSetStatus(Device,USB_STATUS_BOUND);
       end;
     end;  
   finally
    {Release the Lock}
    CriticalSectionUnlock(USBDriverTableLock);
   end;
  end;
 
 {Return Result}
 Result:=Status;
 if Bound then Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function USBDeviceUnbind(Device:PUSBDevice;Driver:PUSBDriver):LongWord;
{Unbind a device from a driver}
{Device: The device to unbind a driver from}
{Driver: The driver to unbind the device from (nil to unbind from current driver)}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var
 Count:LongWord;
 Status:LongWord;
 Interrface:PUSBInterface;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Device}
 if USBDeviceCheck(Device) <> Device then Exit;
 
 {Set Default}
 Status:=USB_STATUS_NOT_BOUND;
 
 //Critical //Lock the Device table (Can't lock the Device because Unbind may deadlock)
 //Critical //Lock the Driver table 
 
 {Check Driver}
 if Driver = nil then
  begin
   {Check Device Driver}
   if (Device.Driver <> nil) and Assigned(Device.Driver.DriverUnbind) then
    begin
     {$IFDEF USB_DEBUG}
     if USB_LOG_ENABLED then USBLogDebug(Device,'Unbinding ' + DriverGetName(@Device.Driver.Driver));
     {$ENDIF}
     
     {Unbind Driver (Device)}
     Device.Driver.DriverUnbind(Device,nil);
     
     {Set Status to Unbound}
     USBDeviceSetStatus(Device,USB_STATUS_UNBOUND);
     
     Status:=USB_STATUS_SUCCESS;
    end
   else
    begin
     {Check Interfaces}
     for Count:=0 to Length(Device.Configuration.Interfaces) - 1 do
      begin
       Interrface:=Device.Configuration.Interfaces[Count];
       if Interrface <> nil then
        begin
         if (Interrface.Driver <> nil) and Assigned(Interrface.Driver.DriverUnbind) then
          begin
           {$IFDEF USB_DEBUG}
           if USB_LOG_ENABLED then USBLogDebug(Device,'Unbinding ' + DriverGetName(@Device.Driver.Driver) + ' (Interface=' + IntToStr(Count) + ')');
           {$ENDIF}
           
           {Unbind Driver (Interface)}
           Interrface.Driver.DriverUnbind(Device,Interrface);
           
           Status:=USB_STATUS_SUCCESS;
          end;
        end;
      end;  

     {Check Status}
     if Status = USB_STATUS_SUCCESS then
      begin
       {Set Status to Unbound}
       USBDeviceSetStatus(Device,USB_STATUS_UNBOUND);
      end; 
    end;    
  end
 else
  begin
   {Check Device Driver}
   if (Device.Driver = Driver) and Assigned(Device.Driver.DriverUnbind) then
    begin
     {$IFDEF USB_DEBUG}
     if USB_LOG_ENABLED then USBLogDebug(Device,'Unbinding ' + DriverGetName(@Device.Driver.Driver));
     {$ENDIF}
     
     {Unbind Driver (Device)}
     Device.Driver.DriverUnbind(Device,nil);
     
     {Set Status to Unbound}
     USBDeviceSetStatus(Device,USB_STATUS_UNBOUND);
     
     Status:=USB_STATUS_SUCCESS;
    end
   else
    begin
     {Check Interfaces}
     for Count:=0 to Length(Device.Configuration.Interfaces) - 1 do
      begin
       Interrface:=Device.Configuration.Interfaces[Count];
       if Interrface <> nil then
        begin
         if (Interrface.Driver = Driver) and Assigned(Interrface.Driver.DriverUnbind) then
          begin
           {$IFDEF USB_DEBUG}
           if USB_LOG_ENABLED then USBLogDebug(Device,'Unbinding ' + DriverGetName(@Device.Driver.Driver) + ' (Interface=' + IntToStr(Count) + ')');
           {$ENDIF}
           
           {Unbind Driver (Interface)}
           Interrface.Driver.DriverUnbind(Device,Interrface);
           
           Status:=USB_STATUS_SUCCESS;
          end;
        end;
      end;  

     {Check Status}
     if Status = USB_STATUS_SUCCESS then
      begin
       {Set Status to Unbound}
       USBDeviceSetStatus(Device,USB_STATUS_UNBOUND);
      end; 
    end;    
  end;
 
 {Return Result}
 Result:=Status;
end;

{==============================================================================}

function USBDeviceAttach(Device:PUSBDevice):LongWord;
{Configure and initialize a newly attached USB device}
{Device: New USB device to configure and initialize}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var
 Address:Byte;
 Count:LongWord;
 Status:LongWord;       
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Device}
 if USBDeviceCheck(Device) <> Device then Exit;
 
 {Check State}
 if Device.USBState <> USB_STATE_DETACHED then Exit;
 
 {Set State to Attaching}
 Result:=USB_STATUS_OPERATION_FAILED;
 if USBDeviceSetState(Device,USB_STATE_ATTACHING) <> USB_STATUS_SUCCESS then Exit;

 {Create Device Descriptor}
 Status:=USBDeviceCreateDeviceDescriptor(Device,USB_ALTERNATE_MAX_PACKET_SIZE);
 if Status <> USB_STATUS_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Failed to create device descriptor: ' + USBStatusToString(Status));
   
   {Return Result}
   Result:=Status;
   Exit;
  end;  
 
 //To Do //Critical //See notes about this in usb_new_device in \u-boot-HEAD-5745f8c\common\usb.c
               //Apparently Windows and Linux both send a 64 byte initial GetDescriptor request to avoid some issues
               //Apparently also this will fail in XHCI (USB 3.0) until the Address has been set and other initialization completed
 
 {$IFDEF USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'Getting maximum packet size');
 {$ENDIF}
 
 {Setup Retry}
 Count:=3;
 while Count > 0 do
  begin
   {Read Device Descriptor (Starting with 8 byte maximum)}
   Device.Descriptor.bMaxPacketSize0:=USB_DEFAULT_MAX_PACKET_SIZE; 
   Status:=USBDeviceReadDeviceDescriptor(Device,Device.Descriptor.bMaxPacketSize0);
   if Status <> USB_STATUS_SUCCESS then
    begin
     {$IFDEF USB_DEBUG}
     if USB_LOG_ENABLED then USBLogDebug(Device,'Retrying with maximum packet size ' + IntToStr(USB_ALTERNATE_MAX_PACKET_SIZE) + ' bytes');
     {$ENDIF}
     
     {Read Device Descriptor (Retry with 64 byte maximum)}
     Device.Descriptor.bMaxPacketSize0:=USB_ALTERNATE_MAX_PACKET_SIZE;
     Status:=USBDeviceReadDeviceDescriptorEx(Device,Device.Descriptor.bMaxPacketSize0,True);
     if Status <> USB_STATUS_SUCCESS then
      begin
       if USB_LOG_ENABLED then USBLogError(Device,'Failed to read start of device descriptor: ' + USBStatusToString(Status));
   
       ThreadSleep(50);
       
       {Update Count}
       Dec(Count);
       if Count < 1 then
        begin
         {Return Result}
         Result:=Status;
         Exit;
        end; 
      end
     else
      begin
       {Continue}
       Break;
      end;    
    end
   else
    begin
     {Continue}
     Break;
    end;  
  end;
 
 {$IFDEF USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'Using bMaxPacketSize0=' + IntToStr(Device.Descriptor.bMaxPacketSize0));
 {$ENDIF}
 
 {Get Device Address}
 Address:=USBDeviceGetAddress(Device);
 if Address = 0 then
  begin
   {Return Result}
   Result:=USB_STATUS_INVALID_PARAMETER;
   Exit;
  end;
 
 {$IFDEF USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'Assigning address ' + IntToStr(Address) + ' to new device');
 {$ENDIF}

 {Set Device Address}
 Status:=USBDeviceSetAddress(Device,Address);
 if Status <> USB_STATUS_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Failed to assign device address:  ' + USBStatusToString(Status));
   
   {Return Result}
   Result:=Status;
   Exit;
  end;
  
 //To Do //Critical //May need to allow 10msec to settle before proceeding //See: usb_new_device in \u-boot-HEAD-5745f8c\common\usb.c
 
 {$IFDEF USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'Reading device descriptor');
 {$ENDIF}

 {Read Device Descriptor}
 Status:=USBDeviceReadDeviceDescriptor(Device,SizeOf(TUSBDeviceDescriptor));
 if Status <> USB_STATUS_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Failed to read device descriptor: ' + USBStatusToString(Status));
   
   {Return Result}
   Result:=Status;
   Exit;
  end;
 
 {Get Product}
 if Device.Descriptor.iProduct <> 0 then
  begin
   {$IFDEF USB_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Device,'Reading product string');
   {$ENDIF}
   
   //To Do //Critical  //usb_get_ascii_string etc
  end;
 
 {Get Manufacturer}
 if Device.Descriptor.iManufacturer <> 0 then
  begin
   {$IFDEF USB_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Device,'Reading manufacturer string');
   {$ENDIF}
   
   //To Do //Critical  //usb_get_ascii_string etc
  end;  
 
 {Get SerialNumber}
 if Device.Descriptor.iSerialNumber <> 0 then
  begin
   {$IFDEF USB_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Device,'Reading serial number string');
   {$ENDIF}
  
   //To Do //Critical //iSerialNumber as well ? //See Descriptor
  end; 

 {$IFDEF USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'Creating configurations');
 {$ENDIF}

 {Create Configurations}
 Status:=USBDeviceCreateConfigurations(Device);
 if Status <> USB_STATUS_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Failed to create device configurations: ' + USBStatusToString(Status));
   
   {Return Result}
   Result:=Status;
   Exit;
  end;
 
 {$IFDEF USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'Reading configurations');
 {$ENDIF}
 
 {Read Configurations}
 Status:=USBDeviceReadConfigurations(Device);
 if Status <> USB_STATUS_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Failed to read device configurations: ' + USBStatusToString(Status));
   
   {Return Result}
   Result:=Status;
   Exit;
  end;

 {Check Configuration Count}
 if Device.Descriptor.bNumConfigurations = 1 then
  begin
   {$IFDEF USB_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Device,'Assigning configuration ' + IntToStr(Device.Configuration.Descriptor.bConfigurationValue) + ' (' + IntToStr(Device.Configuration.Descriptor.bNumInterfaces) + ' interfaces available)');
   {$ENDIF}
   
   {Set Default Configuration}
   Status:=USBDeviceSetConfiguration(Device,Device.Configuration.Descriptor.bConfigurationValue);
   if Status <> USB_STATUS_SUCCESS then
    begin
     if USB_LOG_ENABLED then USBLogError(Device,'Failed to set device configuration: ' + USBStatusToString(Status));
     
     {Return Result}
     Result:=Status;
     Exit;
    end;
  end;
 
 {Report Attach}
 if USB_LOG_ENABLED then USBLogInfo(Device,'Attaching ' + USBDeviceToString(Device));
 
 {Set State to Attached}
 if USBDeviceSetState(Device,USB_STATE_ATTACHED) <> USB_STATUS_SUCCESS then Exit;
 
 {Bind a Driver to the Device}
 Status:=USBDeviceBind(Device);
 if Status = USB_STATUS_DEVICE_UNSUPPORTED then
  begin
   if USB_LOG_ENABLED then USBLogInfo(Device,'No driver found for device');
   
   {No Driver for Device}
   Status:=USB_STATUS_SUCCESS;
  end
 else if Status <> USB_STATUS_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Failed to bind driver to new USB device: ' + USBStatusToString(Status));
  end;  
 
 {Return Result}
 Result:=Status;
end;

{==============================================================================}

function USBDeviceDetach(Device:PUSBDevice):LongWord;
{Shutdown and detach a USB device}
{Device: The USB device to shutdown and detach}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var
 Message:TMessage;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Device}
 if USBDeviceCheck(Device) <> Device then Exit;

 {Check State}
 if Device.USBState <> USB_STATE_ATTACHED then Exit;

 {Set State to Detaching}
 Result:=USB_STATUS_OPERATION_FAILED;
 if USBDeviceSetState(Device,USB_STATE_DETACHING) <> USB_STATUS_SUCCESS then Exit;

 {Acquire the Lock}
 if MutexLock(Device.Lock) <> ERROR_SUCCESS then Exit;
 
 {Check Pending}
 if Device.PendingCount <> 0 then
  begin
   {$IFDEF USB_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Device,'Waiting for ' + IntToStr(Device.PendingCount) + ' pending requests to complete');
   {$ENDIF}
   
   {Wait for Pending}
   
   {Setup Waiter}
   Device.WaiterThread:=GetCurrentThreadId; 
   
   {Release the Lock}
   MutexUnlock(Device.Lock);
   
   {Wait for Message}
   ThreadReceiveMessage(Message); 
  end
 else
  begin
   {Release the Lock}
   MutexUnlock(Device.Lock);
  end;  
     
 {Set State to Detached}
 if USBDeviceSetState(Device,USB_STATE_DETACHED) <> USB_STATUS_SUCCESS then Exit;
   
 {Return Result}
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function USBDeviceAllocate(Host:PUSBHost;Parent:PUSBDevice):PUSBDevice;
{Create and Register a new Device entry in the Device table}
{Host: The Host this device is attached to}
{Parent: The Parent device (Hub) this device is attached to (nil if this device is a root hub)}
{Return: Pointer to new Device entry or nil if device could not be created}
var
 USBId:LongWord;
 Device:PUSBDevice;
begin
 {}
 Result:=nil;
 
 {Check Host}
 if Host = nil then Exit;
 
 {Create USB Device}
 Device:=PUSBDevice(DeviceCreateEx(SizeOf(TUSBDevice)));
 if Device = nil then Exit;
 
 {Update Device}
 Device.Device.DeviceBus:=DEVICE_BUS_USB;   
 Device.Device.DeviceType:=USB_TYPE_NONE;
 Device.Device.DeviceFlags:=USB_FLAG_NONE;
 Device.Device.DeviceData:=nil;
 
 {Update USB Device}
 Device.USBId:=DEVICE_ID_ANY;
 Device.USBState:=USB_STATE_DETACHED;
 Device.USBStatus:=USB_STATUS_UNBOUND;
 Device.Host:=Host;
 Device.Parent:=Parent;
 Device.Lock:=INVALID_HANDLE_VALUE;
 Device.Speed:=USB_SPEED_HIGH; {Default to high-speed unless overridden later}
 if Parent <> nil then Device.Depth:=Parent.Depth + 1;
 Device.Descriptor:=nil;
 Device.LastError:=USB_STATUS_SUCCESS;
 Device.WaiterThread:=INVALID_HANDLE_VALUE;
 
 {Create Lock}
 Device.Lock:=MutexCreate;
 if Device.Lock = INVALID_HANDLE_VALUE then
  begin
   if USB_LOG_ENABLED then USBLogError(nil,'Failed to create lock for USB device');
   
   DeviceDestroy(@Device.Device);
   Exit;
  end;
 
 {Insert USB Device}
 if CriticalSectionLock(USBDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Update USB Device}
    USBId:=0;
    while USBDeviceFind(USBId) <> nil do
     begin
      Inc(USBId);
     end;
    Device.USBId:=USBId;
 
    {Update Device}
    Device.Device.DeviceName:=USB_DEVICE_PREFIX + IntToStr(Device.USBId);
    Device.Device.DeviceClass:=DEVICE_CLASS_USB;
 
    {Register Device}
    if DeviceRegister(@Device.Device) <> ERROR_SUCCESS then
     begin
      Device.USBId:=DEVICE_ID_ANY;
      Exit;
     end; 
 
    {Link USB Device}
    if USBDeviceTable = nil then
     begin
      USBDeviceTable:=Device;
     end
    else
     begin
      Device.Next:=USBDeviceTable;
      USBDeviceTable.Prev:=Device;
      USBDeviceTable:=Device;
     end;
 
    {Increment Count}
    Inc(USBDeviceTableCount);
 
    {Return Result}
    Result:=Device;
   finally
    CriticalSectionUnlock(USBDeviceTableLock);
   end;
  end
end;

{==============================================================================}

function USBDeviceRelease(Device:PUSBDevice):LongWord;
{Deregister and Destroy a Device from the Device table}
{Device: The device to deregister and destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Index:LongWord;
 Count:LongWord;
 Prev:PUSBDevice;
 Next:PUSBDevice;
 Alternate:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;
 if Device.USBId = DEVICE_ID_ANY then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check USB Device}
 Result:=ERROR_NOT_FOUND;
 if USBDeviceCheck(Device) <> Device then Exit;
 
 {Check State}
 if Device.Device.DeviceState <> DEVICE_STATE_REGISTERED then Exit;
 
 {Acquire the Lock}
 Result:=MutexLock(Device.Lock);
 if Result <> ERROR_SUCCESS then Exit;
 
 {Remove USB Device}
 if CriticalSectionLock(USBDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Deregister Device}
    Result:=DeviceDeregister(@Device.Device);
    if Result <> ERROR_SUCCESS then Exit;
 
    {Unlink USB Device}
    Prev:=Device.Prev;
    Next:=Device.Next;
    if Prev = nil then
     begin
      USBDeviceTable:=Next;
      if Next <> nil then
       begin
        Next.Prev:=nil;
       end;       
     end
    else
     begin
      Prev.Next:=Next;
      if Next <> nil then
       begin
        Next.Prev:=Prev;
       end;       
     end;     
 
    {Decrement Count}
    Dec(USBDeviceTableCount);
 
    {Update USB Device}
    Device.USBId:=DEVICE_ID_ANY;
   
    {Free Configurations}
    if Length(Device.Configurations) > 0 then
     begin
      for Index:=0 to Length(Device.Configurations) - 1 do
       begin
        {Free Interfaces}
        if Length(Device.Configurations[Index].Interfaces) > 0 then
         begin
          for Count:=0 to Length(Device.Configurations[Index].Interfaces) - 1 do
           begin
            {Free Alternates}
            if Length(Device.Configurations[Index].Interfaces[Count].Alternates) > 0 then
             begin
              for Alternate:=0 to Length(Device.Configurations[Index].Interfaces[Count].Alternates) - 1 do
               begin
                {Free Endpoints}
                if Length(Device.Configurations[Index].Interfaces[Count].Alternates[Alternate].Endpoints) > 0 then
                 begin
                  SetLength(Device.Configurations[Index].Interfaces[Count].Alternates[Alternate].Endpoints,0);
                 end;
               
                {Free Alternate}
                FreeMem(Device.Configurations[Index].Interfaces[Count].Alternates[Alternate]);
               end;
               
              SetLength(Device.Configurations[Index].Interfaces[Count].Alternates,0);
             end; 

            {Free Endpoints}
            if Length(Device.Configurations[Index].Interfaces[Count].Endpoints) > 0 then
             begin
              SetLength(Device.Configurations[Index].Interfaces[Count].Endpoints,0);
             end;
            
            {Free Interface}
            FreeMem(Device.Configurations[Index].Interfaces[Count]);
           end; 
          
          SetLength(Device.Configurations[Index].Interfaces,0);
         end;
        
        {Free Configuration Descriptor}
        USBBufferRelease(Device.Configurations[Index].Descriptor);
        
        {Free Configuration}
        FreeMem(Device.Configurations[Index]);
       end;
       
      SetLength(Device.Configurations,0);
     end;
    
    {Free Descriptor}
    if Device.Descriptor <> nil then
     begin
      USBBufferRelease(Device.Descriptor);
     end; 
    
    {Release the Lock}
    MutexUnlock(Device.Lock);
    
    {Free the Lock}
    MutexDestroy(Device.Lock);
 
    {Destroy USB Device} 
    Result:=DeviceDestroy(@Device.Device);
   finally
    CriticalSectionUnlock(USBDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function USBDeviceFind(USBId:LongWord):PUSBDevice;
var
 Device:PUSBDevice;
begin
 {}
 Result:=nil;
 
 {Check Id}
 if USBId = DEVICE_ID_ANY then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(USBDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Device}
    Device:=USBDeviceTable;
    while Device <> nil do
     begin
      {Check State}
      if Device.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Id}
        if Device.USBId = USBId then
         begin
          Result:=Device;
          Exit;
         end;
       end;
       
      {Get Next}
      Device:=Device.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(USBDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

function USBDeviceEnumerate(Callback:TUSBDeviceEnumerate;Data:Pointer):LongWord;
var
 Device:PUSBDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Callback}
 if not Assigned(Callback) then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(USBDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Device}
    Device:=USBDeviceTable;
    while Device <> nil do
     begin
      {Check State}
      if Device.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        if Callback(Device,Data) <> ERROR_SUCCESS then Exit;
       end;
       
      {Get Next}
      Device:=Device.Next;
     end;
     
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(USBDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function USBDeviceNotification(Device:PUSBDevice;Callback:TUSBDeviceNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then
  begin
   Result:=DeviceNotification(nil,DEVICE_CLASS_USB,Callback,Data,Notification,Flags);
  end
 else
  begin 
   {Check Device}
   if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

   Result:=DeviceNotification(@Device.Device,DEVICE_CLASS_USB,Callback,Data,Notification,Flags);
  end; 
end;
 
{==============================================================================}

function USBDriverCreate:PUSBDriver;
{Create a new Driver entry}
{Return: Pointer to new Driver entry or nil if driver could not be created}
begin
 {}
 Result:=USBDriverCreateEx(SizeOf(TUSBDriver));
end;

{==============================================================================}

function USBDriverCreateEx(Size:LongWord):PUSBDriver;
{Create a new Driver entry}
{Size: Size in bytes to allocate for new driver (Including the driver entry)}
{Return: Pointer to new Driver entry or nil if driver could not be created}
begin
 {}
 Result:=nil;
 
 {Check Size}
 if Size < SizeOf(TUSBDriver) then Exit;
 
 {Create Driver}
 Result:=PUSBDriver(DriverCreateEx(Size));
 if Result = nil then Exit;
 
 {Update Driver}
 Result.DriverBind:=nil;
 Result.DriverUnbind:=nil;
 Result.Lock:=INVALID_HANDLE_VALUE;
 
 {Create Lock}
 Result.Lock:=MutexCreate;
 if Result.Lock = INVALID_HANDLE_VALUE then
  begin
   if USB_LOG_ENABLED then USBLogError(nil,'Failed to create lock for USB driver');
   USBDriverDestroy(Result);
   Result:=nil;
   Exit;
  end;
end;

{==============================================================================}

function USBDriverDestroy(Driver:PUSBDriver):LongWord;
{Destroy an existing Driver entry}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Driver}
 if Driver = nil then Exit;
 if Driver.Driver.Signature <> DRIVER_SIGNATURE then Exit;
 
 {Check Driver}
 Result:=ERROR_IN_USE;
 if USBDriverCheck(Driver) = Driver then Exit;

 {Check State}
 if Driver.Driver.DriverState <> DRIVER_STATE_UNREGISTERED then Exit;
 
 {Destroy Lock}
 if Driver.Lock <> INVALID_HANDLE_VALUE then
  begin
   MutexDestroy(Driver.Lock);
  end;
 
 {Destroy Driver} 
 Result:=DriverDestroy(@Driver.Driver);
end;

{==============================================================================}

function USBDriverRegister(Driver:PUSBDriver):LongWord;
{Register a new Driver in the Driver table}
var
 Host:PUSBHost;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Driver}
 if Driver = nil then Exit;
 if Driver.Driver.DriverId <> DRIVER_ID_ANY then Exit;
 if Driver.Driver.Signature <> DRIVER_SIGNATURE then Exit;
 
 {Check Bind}
 if not(Assigned(Driver.DriverBind)) then
  begin
   if USB_LOG_ENABLED then USBLogError(nil,'Cannot register driver, Bind function must be implemented');
   Exit;
  end;
 
 {Check Unbind}
 if not(Assigned(Driver.DriverUnbind)) then
  begin
   if USB_LOG_ENABLED then USBLogError(nil,'Cannot register driver, Unbind function must be implemented');
   Exit;
  end;
  
 {Check Driver}
 Result:=ERROR_ALREADY_EXISTS;
 if USBDriverCheck(Driver) = Driver then
  begin
   if USB_LOG_ENABLED then USBLogError(nil,'Cannot register driver, already registered');
   Exit;
  end; 
 
 {Check State}
 if Driver.Driver.DriverState <> DRIVER_STATE_UNREGISTERED then Exit;
 
 {Insert Driver}
 if CriticalSectionLock(USBDriverTableLock) = ERROR_SUCCESS then
  begin
   try
    {Update Driver}
    Driver.Driver.DriverClass:=DRIVER_CLASS_USB;
    
    {Register Driver}
    Result:=DriverRegister(@Driver.Driver);
    if Result <> ERROR_SUCCESS then Exit;
    
    {Link Driver}
    if USBDriverTable = nil then
     begin
      USBDriverTable:=Driver;
     end
    else
     begin
      Driver.Next:=USBDriverTable;
      USBDriverTable.Prev:=Driver;
      USBDriverTable:=Driver;
     end;
 
    {Increment Count}
    Inc(USBDriverTableCount);
    
    if USB_LOG_ENABLED then USBLogInfo(nil,'Registered ' + DriverGetName(@Driver.Driver) + ' (Id=' + IntToStr(Driver.Driver.DriverId) + ')');
    
    {Acquire the Lock}
    if CriticalSectionLock(USBHostTableLock) = ERROR_SUCCESS then
     begin
      try
       {Get Host}
       Host:=USBHostTable;
       while Host <> nil do
        begin
         {Check Root Hub}
         if Host.RootHub <> nil then
          begin
           {Bind Devices}
           USBHubBindDevices(Host.RootHub,USBDeviceBind);  
          end; 
    
         {Get Next}
         Host:=Host.Next;
        end;
        
       {Return Result}
       Result:=ERROR_SUCCESS;
      finally
       {Release the Lock}
       CriticalSectionUnlock(USBHostTableLock);
      end;
     end
    else
     begin
      Result:=ERROR_CAN_NOT_COMPLETE;
     end;     
   finally
    CriticalSectionUnlock(USBDriverTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function USBDriverDeregister(Driver:PUSBDriver):LongWord;
{Deregister a Driver from the Driver table}
var
 Host:PUSBHost;
 Prev:PUSBDriver;
 Next:PUSBDriver;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Driver}
 if Driver = nil then Exit;
 if Driver.Driver.DriverId = DRIVER_ID_ANY then Exit;
 if Driver.Driver.Signature <> DRIVER_SIGNATURE then Exit;
 
 {Check Driver}
 Result:=ERROR_NOT_FOUND;
 if USBDriverCheck(Driver) <> Driver then Exit;
 
 {Check State}
 if Driver.Driver.DriverState <> DRIVER_STATE_REGISTERED then Exit;
 
 {Remove Driver}
 if CriticalSectionLock(USBDriverTableLock) = ERROR_SUCCESS then
  begin
   try
    {Acquire the Lock}
    if CriticalSectionLock(USBHostTableLock) = ERROR_SUCCESS then
     begin
      try
       {Get Host}
       Host:=USBHostTable;
       while Host <> nil do
        begin
         {Check Root Hub}
         if Host.RootHub <> nil then
          begin
           {Unbind Devices}
           USBHubUnbindDevices(Host.RootHub,Driver,USBDeviceUnbind);  
          end; 
    
         {Get Next}
         Host:=Host.Next;
        end;
      finally
       {Release the Lock}
       CriticalSectionUnlock(USBHostTableLock);
      end;
     end
    else
     begin
      Result:=ERROR_CAN_NOT_COMPLETE;
      Exit;
     end;     
    
    {Deregister Driver}
    Result:=DriverDeregister(@Driver.Driver);
    if Result <> ERROR_SUCCESS then Exit;
    
    {Unlink Driver}
    Prev:=Driver.Prev;
    Next:=Driver.Next;
    if Prev = nil then
     begin
      USBDriverTable:=Next;
      if Next <> nil then
       begin
        Next.Prev:=nil;
       end;       
     end
    else
     begin
      Prev.Next:=Next;
      if Next <> nil then
       begin
        Next.Prev:=Prev;
       end;       
     end;     
 
    {Decrement Count}
    Dec(USBDriverTableCount);
 
    if USB_LOG_ENABLED then USBLogInfo(nil,'Deregistered ' + DriverGetName(@Driver.Driver));
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(USBDriverTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function USBDriverFind(DriverId:LongWord):PUSBDriver;
var
 Driver:PUSBDriver;
begin
 {}
 Result:=nil;
 
 {Check Id}
 if DriverId = DRIVER_ID_ANY then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(USBDriverTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Driver}
    Driver:=USBDriverTable;
    while Driver <> nil do
     begin
      {Check State}
      if Driver.Driver.DriverState = DRIVER_STATE_REGISTERED then
       begin
        {Check Id}
        if Driver.Driver.DriverId = DriverId then
         begin
          Result:=Driver;
          Exit;
         end;
       end;
       
      {Get Next}
      Driver:=Driver.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(USBDriverTableLock);
   end;
  end;
end;

{==============================================================================}

function USBDriverEnumerate(Callback:TUSBDriverEnumerate;Data:Pointer):LongWord;
var
 Driver:PUSBDriver;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Callback}
 if not Assigned(Callback) then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(USBDriverTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Driver}
    Driver:=USBDriverTable;
    while Driver <> nil do
     begin
      {Check State}
      if Driver.Driver.DriverState = DRIVER_STATE_REGISTERED then
       begin
        if Callback(Driver,Data) <> ERROR_SUCCESS then Exit;
       end;
       
      {Get Next}
      Driver:=Driver.Next;
     end;
     
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(USBDriverTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function USBHostSetState(Host:PUSBHost;State:LongWord):LongWord;
{Set the state of the specified host and send a notification}
{Host: The USB host to set the state for}
{State: The new state to set and notify}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Host}
 if Host = nil then Exit;
 if Host.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check State}
 if State > USBHOST_STATE_ENABLED then Exit;
 
 {Check State}
 if Host.HostState = State then
  begin
   {Return Result}
   Result:=USB_STATUS_SUCCESS;
  end
 else
  begin
   {Acquire the Lock}
   if MutexLock(Host.Lock) = ERROR_SUCCESS then
    begin
     try 
      {Set State}
      Host.HostState:=State;
  
      {Notify State}
      NotifierNotify(@Host.Device,USBHostStateToNotification(State));

      {Return Result}
      Result:=USB_STATUS_SUCCESS;
     finally
      {Release the Lock}
      MutexUnlock(Host.Lock);
     end;
    end
   else
    begin
     Result:=USB_STATUS_OPERATION_FAILED;
    end;
  end;  
end;

{==============================================================================}

function USBHostCreate:PUSBHost;
{Create a new Host entry}
{Return: Pointer to new Host entry or nil if host could not be created}
begin
 {}
 Result:=USBHostCreateEx(SizeOf(TUSBHost));
end;

{==============================================================================}

function USBHostCreateEx(Size:LongWord):PUSBHost;
{Create a new Host entry}
{Size: Size in bytes to allocate for new host (Including the host entry)}
{Return: Pointer to new Host entry or nil if host could not be created}
begin
 {}
 Result:=nil;
 
 {Check Size}
 if Size < SizeOf(TUSBHost) then Exit;
 
 {Create Host}
 Result:=PUSBHost(DeviceCreateEx(Size));
 if Result = nil then Exit;
 
 {Update Device}
 Result.Device.DeviceBus:=DEVICE_BUS_NONE;   
 Result.Device.DeviceType:=USBHOST_TYPE_NONE;
 Result.Device.DeviceFlags:=USBHOST_FLAG_NONE;
 Result.Device.DeviceData:=nil;

 {Update Host}
 Result.HostId:=DEVICE_ID_ANY;
 Result.HostState:=USBHOST_STATE_DISABLED;
 Result.HostStart:=nil;
 Result.HostStop:=nil;
 Result.HostReset:=nil;
 Result.HostSubmit:=nil;
 Result.HostCancel:=nil;
 Result.Lock:=INVALID_HANDLE_VALUE;
 Result.RootHub:=nil;
 Result.Alignment:=1;
 Result.Multiplier:=1;
 
 {Create Lock}
 Result.Lock:=MutexCreate;
 if Result.Lock = INVALID_HANDLE_VALUE then
  begin
   if USB_LOG_ENABLED then USBLogError(nil,'Failed to create lock for USB host');
   USBHostDestroy(Result);
   Result:=nil;
   Exit;
  end;
end;

{==============================================================================}

function USBHostDestroy(Host:PUSBHost):LongWord;
{Destroy an existing Host entry}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Host}
 if Host = nil then Exit;
 if Host.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Host}
 Result:=ERROR_IN_USE;
 if USBHostCheck(Host) = Host then Exit;

 {Check State}
 if Host.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;
  
 {Destroy Lock}
 if Host.Lock <> INVALID_HANDLE_VALUE then
  begin
   MutexDestroy(Host.Lock);
  end;
 
 {Destroy Host} 
 Result:=DeviceDestroy(@Host.Device);
end;

{==============================================================================}

function USBHostRegister(Host:PUSBHost):LongWord;
{Register a new Host in the Host table}
var
 HostId:LongWord;
 Status:LongWord;
 RootHub:PUSBDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Host}
 if Host = nil then Exit;
 if Host.HostId <> DEVICE_ID_ANY then Exit;
 if Host.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Start}
 if not(Assigned(Host.HostStart)) then
  begin
   if USB_LOG_ENABLED then USBLogError(nil,'Cannot register host, Start function must be implemented');
   Exit;
  end;

 {Check Stop}
 if not(Assigned(Host.HostStop)) then
  begin
   if USB_LOG_ENABLED then USBLogError(nil,'Cannot register host, Stop function must be implemented');
   Exit;
  end;

 {Check Submit}
 if not(Assigned(Host.HostSubmit)) then
  begin
   if USB_LOG_ENABLED then USBLogError(nil,'Cannot register host, Submit function must be implemented');
   Exit;
  end;
  
 {Check Cancel}
 if not(Assigned(Host.HostCancel)) then
  begin
   if USB_LOG_ENABLED then USBLogError(nil,'Cannot register host, Cancel function must be implemented');
   Exit;
  end;
 
 {Check Host}
 Result:=ERROR_ALREADY_EXISTS;
 if USBHostCheck(Host) = Host then
  begin
   if USB_LOG_ENABLED then USBLogError(nil,'Cannot register host, already registered');
   Exit;
  end; 
 
 {Check State}
 if Host.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;
 
 {Insert Host}
 if CriticalSectionLock(USBHostTableLock) = ERROR_SUCCESS then
  begin
   try
    {Update Host}
    HostId:=0;
    while USBHostFind(HostId) <> nil do
     begin
      Inc(HostId);
     end;
    Host.HostId:=HostId;
    
    {Update Device}
    Host.Device.DeviceName:=USB_HOST_PREFIX + IntToStr(Host.HostId);
    Host.Device.DeviceClass:=DEVICE_CLASS_USBHOST;
    
    {Register Device}
    Result:=DeviceRegister(@Host.Device);
    if Result <> ERROR_SUCCESS then
     begin
      Host.HostId:=DEVICE_ID_ANY;
      Exit;
     end; 
    
    {Link Host}
    if USBHostTable = nil then
     begin
      USBHostTable:=Host;
     end
    else
     begin
      Host.Next:=USBHostTable;
      USBHostTable.Prev:=Host;
      USBHostTable:=Host;
     end;
 
    {Increment Count}
    Inc(USBHostTableCount);
  
    {Check Started}
    if USBStarted then
     begin
      if not USB_ASYNCSTART then
       begin
        {Start Host}
        Status:=Host.HostStart(Host);
        if Status = USB_STATUS_SUCCESS then
         begin
          if USB_LOG_ENABLED then USBLogInfo(nil,'Successfully started USB host ' + DeviceGetName(@Host.Device));
          
          {Allocate Root Hub}
          RootHub:=USBDeviceAllocate(Host,nil);
          if RootHub <> nil then
           begin
            {$IFDEF USB_DEBUG}
            if USB_LOG_ENABLED then USBLogDebug(nil,'Attaching root hub to USB host ' + DeviceGetName(@Host.Device));         
            {$ENDIF}
            
            {Attach Root Hub}
            Status:=USBDeviceAttach(RootHub);
            if Status = USB_STATUS_SUCCESS then
             begin
              {Update Host}
              Host.RootHub:=RootHub;
              
              {Set State to Enabled}
              USBHostSetState(Host,USBHOST_STATE_ENABLED);
             end
            else
             begin          
              if USB_LOG_ENABLED then USBLogError(nil,'Failed to attach root hub to USB host ' + DeviceGetName(@Host.Device) + ' (Status=' + USBStatusToString(Status) + ')');
              
              {Release Device}
              USBDeviceRelease(RootHub);
              
              {Stop Host}
              Host.HostStop(Host);
             end;
           end
          else
           begin        
            if USB_LOG_ENABLED then USBLogError(nil,'Failed to allocate new USB device');
            
            {Stop Host}
            Host.HostStop(Host);
           end;
         end
        else
         begin
          if USB_LOG_ENABLED then USBLogError(nil,'Failed to start USB host ' + DeviceGetName(@Host.Device) + ' (Status=' + USBStatusToString(Status) + ')');
         end;
       
        {Return Result}
        Result:=ERROR_SUCCESS;
       end
      else
       begin
        {Schedule Worker}
        Result:=WorkerSchedule(0,TWorkerTask(USBAsyncStart),Host,nil)
       end;
     end
    else
     begin
      {Return Result}
      Result:=ERROR_SUCCESS;
     end; 
   finally
    CriticalSectionUnlock(USBHostTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function USBHostDeregister(Host:PUSBHost):LongWord;
{Deregister a Host from the Host table}
var
 Prev:PUSBHost;
 Next:PUSBHost;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Host}
 if Host = nil then Exit;
 if Host.HostId = DEVICE_ID_ANY then Exit;
 if Host.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Host}
 Result:=ERROR_NOT_FOUND;
 if USBHostCheck(Host) <> Host then Exit;
 
 {Check State}
 if Host.Device.DeviceState <> DEVICE_STATE_REGISTERED then Exit;
 
 {Remove Host}
 if CriticalSectionLock(USBHostTableLock) = ERROR_SUCCESS then
  begin
   try
    //To Do //Get Root hub, unbind/detach/destroy devices
    
    //To Do //Stop host if started etc
    
    {Deregister Device}
    Result:=DeviceDeregister(@Host.Device);
    if Result <> ERROR_SUCCESS then Exit;
    
    {Unlink Host}
    Prev:=Host.Prev;
    Next:=Host.Next;
    if Prev = nil then
     begin
      USBHostTable:=Next;
      if Next <> nil then
       begin
        Next.Prev:=nil;
       end;       
     end
    else
     begin
      Prev.Next:=Next;
      if Next <> nil then
       begin
        Next.Prev:=Prev;
       end;       
     end;     
 
    {Decrement Count}
    Dec(USBHostTableCount);
 
    {Update Host}
    Host.HostId:=DEVICE_ID_ANY;
 
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(USBHostTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function USBHostFind(HostId:LongWord):PUSBHost;
var
 Host:PUSBHost;
begin
 {}
 Result:=nil;
 
 {Check Id}
 if HostId = DEVICE_ID_ANY then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(USBHostTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Host}
    Host:=USBHostTable;
    while Host <> nil do
     begin
      {Check State}
      if Host.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Id}
        if Host.HostId = HostId then
         begin
          Result:=Host;
          Exit;
         end;
       end;
       
      {Get Next}
      Host:=Host.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(USBHostTableLock);
   end;
  end;
end;

{==============================================================================}

function USBHostEnumerate(Callback:TUSBHostEnumerate;Data:Pointer):LongWord;
var
 Host:PUSBHost;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Callback}
 if not Assigned(Callback) then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(USBHostTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Host}
    Host:=USBHostTable;
    while Host <> nil do
     begin
      {Check State}
      if Host.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        if Callback(Host,Data) <> ERROR_SUCCESS then Exit;
       end;
       
      {Get Next}
      Host:=Host.Next;
     end;
     
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(USBHostTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function USBHostNotification(Host:PUSBHost;Callback:TUSBHostNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Host}
 if Host = nil then
  begin
   Result:=DeviceNotification(nil,DEVICE_CLASS_USBHOST,Callback,Data,Notification,Flags);
  end
 else
  begin 
   {Check Host}
   if Host.Device.Signature <> DEVICE_SIGNATURE then Exit;

   Result:=DeviceNotification(@Host.Device,DEVICE_CLASS_USBHOST,Callback,Data,Notification,Flags);
  end; 
end;

{==============================================================================}

function USBBufferAllocate(Device:PUSBDevice;Size:LongWord):Pointer; inline;
{Allocate a data buffer for a USB request}
{Device: The device that the request will be sent to}
{Size: The size of the data buffer to allocate}
{Return: The newly allocated buffer or nil on failure}
var
 Flags:LongWord;
begin
 {}
 Result:=USBBufferAllocateEx(Device,Size,Flags);
end;

{==============================================================================}

function USBBufferAllocateEx(Device:PUSBDevice;Size:LongWord;var Flags:LongWord):Pointer;
{Allocate a data buffer for a USB request}
{Device: The device that the request will be sent to}
{Size: The size of the data buffer to allocate}
{Flags: The returned flags for the allocated buffer (eg USB_REQUEST_FLAG_SHARED)}
{Return: The newly allocated buffer or nil on failure}
begin
 {}
 Result:=nil;
 
 {Set Flags}
 Flags:=USB_REQUEST_FLAG_NONE;
 
 {Check Size}
 if Size = 0 then Exit;
 
 {Check Device}
 if Device = nil then Exit;
 if Device.Host = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Size}
 Size:=RoundUp(Size,Device.Host.Multiplier);
 Flags:=Flags or USB_REQUEST_FLAG_SIZED;
 
 {Check Host Flags}
 if (Device.Host.Device.DeviceFlags and USBHOST_FLAG_SHARED) = USBHOST_FLAG_SHARED then
  begin
   {Set Flags}
   Flags:=Flags or USB_REQUEST_FLAG_ALLOCATED or USB_REQUEST_FLAG_COMPATIBLE or USB_REQUEST_FLAG_ALIGNED or USB_REQUEST_FLAG_SHARED;
   
   {Allocate Shared}
   Result:=GetSharedAlignedMem(Size,Device.Host.Alignment);
  end
 else if (Device.Host.Device.DeviceFlags and USBHOST_FLAG_NOCACHE) = USBHOST_FLAG_NOCACHE then  
  begin
   {Set Flags}
   Flags:=Flags or USB_REQUEST_FLAG_ALLOCATED or USB_REQUEST_FLAG_COMPATIBLE or USB_REQUEST_FLAG_ALIGNED or USB_REQUEST_FLAG_NOCACHE;

   {Allocate Non Cached}
   Result:=GetNoCacheAlignedMem(Size,Device.Host.Alignment);
  end
 else
  begin
   {Set Flags}
   Flags:=Flags or USB_REQUEST_FLAG_ALLOCATED or USB_REQUEST_FLAG_COMPATIBLE or USB_REQUEST_FLAG_ALIGNED;

   {Allocate Normal}
   Result:=GetAlignedMem(Size,Device.Host.Alignment);
  end;  
end;

{==============================================================================}

function USBBufferValidate(Device:PUSBDevice;Buffer:Pointer;Size:LongWord;var Flags:LongWord):LongWord;
{Validate a data buffer for a USB request against the USB host requirements}
{Device: The device that the request will be sent to}
{Buffer: The data buffer to validate}
{Size: The size of the data buffer}
{Flags: The returned flags for the validated buffer (eg USB_REQUEST_FLAG_SHARED)}
{Return: USB_STATUS_SUCCESS on success or another error code on failure}
var
 HeapSize:LongWord;
 HeapFlags:LongWord;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Set Flags}
 Flags:=USB_REQUEST_FLAG_NONE;
 
 {Check Size}
 if Size = 0 then Exit;
 
 {Check Buffer}
 if Buffer = nil then Exit;

 {Check Device}
 if Device = nil then Exit;
 if Device.Host = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Get Heap Flags}
 HeapFlags:=MemFlags(Buffer);
 if HeapFlags <> HEAP_FLAG_INVALID then
  begin
   {Check Heap Flags}
   if ((HeapFlags and HEAP_FLAG_SHARED) = HEAP_FLAG_SHARED) or ((HeapFlags = HEAP_FLAG_NORMAL) and HEAP_NORMAL_SHARED) then
    begin
     {Set Flags}
     Flags:=Flags or USB_REQUEST_FLAG_SHARED;
    end
   else if ((HeapFlags and HEAP_FLAG_NOCACHE) = HEAP_FLAG_NOCACHE) or ((HeapFlags = HEAP_FLAG_NORMAL) and HEAP_NORMAL_NOCACHE) then
    begin
     {Set Flags}
     Flags:=Flags or USB_REQUEST_FLAG_NOCACHE;
    end;
  
   {Check Host Alignment}
   if Align(Buffer,Device.Host.Alignment) = Buffer then
    begin
     {Set Flags}
     Flags:=Flags or USB_REQUEST_FLAG_ALIGNED;
    end;
   
   {Check Host Multiplier}
   if RoundUp(Size,Device.Host.Multiplier) = Size then
    begin
     {Set Flags}
     Flags:=Flags or USB_REQUEST_FLAG_SIZED;
    end
   else
    begin
     {Get Heap Size}
     HeapSize:=MemSize(Buffer);
     if HeapSize <> 0 then
      begin
       {Check Host Multiplier}
       if RoundUp(HeapSize,Device.Host.Multiplier) = HeapSize then
        begin
         {Set Flags}
         Flags:=Flags or USB_REQUEST_FLAG_SIZED;
        end;
      end;  
    end;    
   
   {Check Host Flags}
   if (Device.Host.Device.DeviceFlags and USBHOST_FLAG_SHARED) = USBHOST_FLAG_SHARED then
    begin
     {Check Flags}
     if (Flags and (USB_REQUEST_FLAG_SHARED or USB_REQUEST_FLAG_ALIGNED or USB_REQUEST_FLAG_SIZED)) = (USB_REQUEST_FLAG_SHARED or USB_REQUEST_FLAG_ALIGNED or USB_REQUEST_FLAG_SIZED) then
      begin
       {Set Flags}
       Flags:=Flags or USB_REQUEST_FLAG_COMPATIBLE;
      end;
    end
   else if (Device.Host.Device.DeviceFlags and USBHOST_FLAG_NOCACHE) = USBHOST_FLAG_NOCACHE then  
    begin
     {Check Flags}
     if (Flags and (USB_REQUEST_FLAG_NOCACHE or USB_REQUEST_FLAG_ALIGNED or USB_REQUEST_FLAG_SIZED)) = (USB_REQUEST_FLAG_NOCACHE or USB_REQUEST_FLAG_ALIGNED or USB_REQUEST_FLAG_SIZED) then
      begin
       {Set Flags}
       Flags:=Flags or USB_REQUEST_FLAG_COMPATIBLE;
      end;
    end
   else
    begin
     {Check Flags}
     if (Flags and (USB_REQUEST_FLAG_ALIGNED or USB_REQUEST_FLAG_SIZED)) = (USB_REQUEST_FLAG_ALIGNED or USB_REQUEST_FLAG_SIZED) then
      begin
       {Set Flags}
       //Flags:=Flags or USB_REQUEST_FLAG_COMPATIBLE; //Testing
      end; 
    end;    
  end;
  
 {Return Result}
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function USBBufferRelease(Buffer:Pointer):LongWord;
{Release a data buffer from a USB request}
{Data: The buffer to be released}
{Return: USB_STATUS_SUCCESS on success or another error code on failure}
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Buffer}
 if Buffer = nil then Exit;
 
 {Free Buffer}
 FreeMem(Buffer);
 
 {Return Result}
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function USBRequestAllocate(Device:PUSBDevice;Endpoint:PUSBEndpointDescriptor;Callback:TUSBRequestCompleted;Size:LongWord;DriverData:Pointer):PUSBRequest; inline;
{Allocate a new USB request}
{Device: The USB device this request will be sent to}
{Endpoint: The Endpoint descriptor this request will be sent to (Or nil for the default control endpoint)}
{Callback: The callback function to be called on completion of the request}
{Size: The size of the data buffer for the request}
{DriverData: Device driver private data for the callback (Optional)}
{Return: The newly allocated request or nil on failure}
var
 Data:Pointer;
begin
 {}
 {Setup Data}
 Data:=nil;
 
 {Allocate Request}
 Result:=USBRequestAllocateEx(Device,Endpoint,Callback,Data,Size,DriverData);
end;

{==============================================================================}

function USBRequestAllocateEx(Device:PUSBDevice;Endpoint:PUSBEndpointDescriptor;Callback:TUSBRequestCompleted;var Data:Pointer;Size:LongWord;DriverData:Pointer):PUSBRequest;
{Allocate a new USB request}
{Device: The USB device this request will be sent to}
{Endpoint: The Endpoint descriptor this request will be sent to (Or nil for the default control endpoint)}
{Callback: The callback function to be called on completion of the request}
{Data: The returned data buffer allocated for the request (Or nil if size is zero)(Pass an existing buffer to prevent allocation)}
{Size: The size of the data buffer for the request}
{DriverData: Device driver private data for the callback (Optional)}
{Return: The newly allocated request or nil on failure}
var
 Request:PUSBRequest;
begin
 {}
 Result:=nil;
 
 {Check Device}
 if Device = nil then Exit;
 if Device.Host = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Endpoint}
 {if Endpoint = nil then Exit;} {Endpoint may be nil}

 {Check Callback}
 if not Assigned(Callback) then Exit;
 
 {Check Size}
 {if Size = 0 then Exit;} {Size may be zero}
 
 {Check Driver Data}
 {if DriverData = nil then Exit;} {DriverData may be nil}
 
 {Allocate Request}
 Request:=AllocMem(SizeOf(TUSBRequest));
 if Request = nil then Exit;
 
 {Initialize Request}
 Request.Device:=Device;
 Request.Endpoint:=Endpoint;
 Request.Size:=Size;
 Request.Callback:=Callback;
 Request.DriverData:=DriverData;
 Request.Status:=USB_STATUS_NOT_VALID;
 Request.ResubmitThread:=INVALID_HANDLE_VALUE; 
 Request.ResubmitSemaphore:=INVALID_HANDLE_VALUE;
 
 {Check Size}
 if Size > 0 then
  begin
   {Check Data}
   if Data = nil then
    begin
     {Allocate Buffer}
     Data:=USBBufferAllocateEx(Device,Size,Request.Flags);
     if Data = nil then
      begin
       {Release Request}
       FreeMem(Request);
       Exit;
      end;
    end
   else
    begin
     {Validate Buffer}
     if USBBufferValidate(Device,Data,Size,Request.Flags) <> USB_STATUS_SUCCESS then
      begin
       {Release Request}
       FreeMem(Request);
       Exit;
      end;
    end;    
    
   {Assign Data}
   Request.Data:=Data;
  end;
  
 {Return Result}
 Result:=Request;
end;

{==============================================================================}

function USBRequestRelease(Request:PUSBRequest):LongWord;
{Release and destroy a USB request}
{Request: The request to be released}
{Return: USB_STATUS_SUCCESS on success or another error code on failure}
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Request}
 if Request = nil then Exit;

 {Check Resubmit Thread}
 if Request.ResubmitThread <> INVALID_HANDLE_VALUE then
  begin
   {Terminate Thread}
   {KillThread(Request.ResubmitThread);} {Note: Thread will terminate itself when the semaphore is destroyed}
  end;
  
 {Check Resubmit Semaphore}
 if Request.ResubmitSemaphore <> INVALID_HANDLE_VALUE then
  begin
   {Destroy Semaphore}
   SemaphoreDestroy(Request.ResubmitSemaphore);
  end; 
   
 {Check Size and Flags}
 if (Request.Size > 0) and ((Request.Flags and USB_REQUEST_FLAG_ALLOCATED) = USB_REQUEST_FLAG_ALLOCATED) then
  begin
   {Release Buffer}
   Result:=USBBufferRelease(Request.Data);
   if Result <> USB_STATUS_SUCCESS then Exit; 
  end; 
   
 {Release Request}
 FreeMem(Request);
   
 {Return Result}
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function USBRequestInitialize(Request:PUSBRequest;Callback:TUSBRequestCompleted;Data:Pointer;Size:LongWord;DriverData:Pointer):LongWord;
{Initialize or Reinitialize an existing USB request}
{Request: The request to be initialized}
{Callback: The callback function to be called on completion of the request}
{Data: The returned data buffer allocated for the request (Or nil if size is zero)}
{Size: The size of the data buffer for the request}
{DriverData: Device driver private data for the callback (Optional)}

{Return: USB_STATUS_SUCCESS on success or another error code on failure}
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Request}
 if Request = nil then Exit;
 
 {Check Device}
 if Request.Device = nil then Exit;
 if Request.Device.Host = nil then Exit;
 if Request.Device.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Callback}
 if not Assigned(Callback) then Exit;
 
 {Check Size}
 {if Size = 0 then Exit;} {Size may be zero}
 
 {Check Driver Data}
 {if DriverData = nil then Exit;} {DriverData may be nil}
 
 {Initialize Request}
 Request.Data:=Data;
 Request.Size:=Size;
 Request.Callback:=Callback;
 Request.DriverData:=DriverData;
 Request.Status:=USB_STATUS_NOT_VALID;
 Request.ResubmitThread:=INVALID_HANDLE_VALUE; 
 Request.ResubmitSemaphore:=INVALID_HANDLE_VALUE;
 
 {Check Size and Data}
 if (Size > 0) and (Data <> nil) then
  begin
   {Validate Buffer}
   if USBBufferValidate(Request.Device,Data,Size,Request.Flags) <> USB_STATUS_SUCCESS then Exit;
  end
 else
  begin
   Request.Flags:=USB_REQUEST_FLAG_NONE;
  end;
   
 {Return Result}
 Result:=USB_STATUS_SUCCESS;   
end;

{==============================================================================}
//To Do //Remove (Modify SMSC95XX first)
function USBRequestInitializeOld(Request:PUSBRequest):LongWord;
{Initialize a USB request}
{Request: The request to be initialized}
{Return: USB_STATUS_SUCCESS on success or another error code on failure}
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 if Request = nil then Exit;
 
 FillChar(Request^,SizeOf(TUSBRequest),0);
 
 Request.ResubmitThread:=INVALID_HANDLE_VALUE; 
 Request.ResubmitSemaphore:=INVALID_HANDLE_VALUE;
 
 {Return Result}
 Result:=USB_STATUS_SUCCESS;
end;
//To Do //Remove (Modify SMSC95XX first)
{==============================================================================}

function USBRequestSubmit(Request:PUSBRequest):LongWord;
{Submit a USB request to a host controller for execution}
{Request: The request to be submitted}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
{Note: The request will be completed asynchronously by the host controller and the
 completion callback will be called when the request has either succeeded or failed}
var
 Status:LongWord;
 {$IFDEF USB_DEBUG}
 Direction:Byte;
 TransferType:Byte;
 {$ENDIF}
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Request}
 if (Request = nil) or (Request.Device = nil) or (Request.Device.Host = nil) or not(Assigned(Request.Callback)) then
  begin
   if USB_LOG_ENABLED then USBLogError(nil,'Bad USB request, device, host or completion callback function not specified');
   Exit;
  end;

 {Check State}
 if Request.Device.USBState = USB_STATE_DETACHING then
  begin
   {$IFDEF USB_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Request.Device,'Device detaching, refusing request submit');
   {$ENDIF}
   
   Result:=USB_STATUS_DEVICE_DETACHED;
   Exit;
  end;
 
 {$IFDEF USB_DEBUG}
 {Log Request}
 if USB_LOG_ENABLED then 
  begin
   if Request.Endpoint <> nil then
    begin
     TransferType:=Request.Endpoint.bmAttributes and $03;
     Direction:=Request.Endpoint.bEndpointAddress shr 7;
    end
   else
    begin
     TransferType:=USB_TRANSFER_TYPE_CONTROL;
     Direction:=Request.SetupData.bmRequestType shr 7;
    end;
   USBLogDebug(Request.Device,'Submitting request (' + IntToStr(Request.Size) + ' bytes, type=' + USBTransferTypeToString(TransferType) + ', dir=' + USBDirectionToString(Direction) + ')');
   if TransferType = USB_TRANSFER_TYPE_CONTROL then
    begin
     USBLogDebug(Request.Device,'Control message: bmRequestType=' + IntToHex(Request.SetupData.bmRequestType,2) + ', bRequest=' + IntToHex(Request.SetupData.bRequest,2) + ', wValue=' + IntToHex(Request.SetupData.wValue,4) + ', wIndex=' + IntToHex(Request.SetupData.wIndex,4) + ', wLength=' + IntToHex(Request.SetupData.wLength,4));
    end;
  end;  
 {$ENDIF}
 
 {Setup Request} 
 Request.Status:=USB_STATUS_NOT_PROCESSED;
 Request.ActualSize:=0;
 Request.CompleteSplit:=False;
 Request.ControlPhase:=USB_CONTROL_PHASE_SETUP;
 
 {Acquire the Lock}
 if MutexLock(Request.Device.Lock) = ERROR_SUCCESS then
  begin
   try
    {Update Statistics}
    Inc(Request.Device.RequestCount); 
    
    {Update Pending}
    Inc(Request.Device.PendingCount);
    
    {Submit Request}
    Status:=Request.Device.Host.HostSubmit(Request.Device.Host,Request);
    if Status <> USB_STATUS_SUCCESS then
     begin
      {Update Pending}
      Dec(Request.Device.PendingCount);
     end;

    {Return Result}
    Result:=Status;
   finally
    {Release the Lock}
    MutexUnlock(Request.Device.Lock);
   end;   
  end; 
end;

{==============================================================================}

function USBRequestCancel(Request:PUSBRequest):LongWord;
{Cancel a USB request previously submitted to a host controller}
{Request: The request to be cancelled}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Request}
 if (Request = nil) or (Request.Device = nil) or (Request.Device.Host = nil) or not(Assigned(Request.Callback)) then
  begin
   if USB_LOG_ENABLED then USBLogError(nil,'Bad USB request, device, host or completion callback function not specified');
   Exit;
  end;
 
 {Check State}
 if Request.Device.USBState = USB_STATE_DETACHING then
  begin
   {$IFDEF USB_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Request.Device,'Device detaching, refusing request cancel');
   {$ENDIF}
   
   Result:=USB_STATUS_DEVICE_DETACHED;
   Exit;
  end;
 
 {$IFDEF USB_DEBUG}
 {Log Request}
 if USB_LOG_ENABLED then 
  begin
   if Request.Endpoint <> nil then
    begin
     TransferType:=Request.Endpoint.bmAttributes and $03;
     Direction:=Request.Endpoint.bEndpointAddress shr 7;
    end
   else
    begin
     TransferType:=USB_TRANSFER_TYPE_CONTROL;
     Direction:=Request.SetupData.bmRequestType shr 7;
    end;
   USBLogDebug(Request.Device,'Cancelling request (' + IntToStr(Request.Size) + ' bytes, type=' + USBTransferTypeToString(TransferType) + ', dir=' + USBDirectionToString(Direction) + ')');
   if TransferType = USB_TRANSFER_TYPE_CONTROL then
    begin
     USBLogDebug(Request.Device,'Control message: bmRequestType=' + IntToHex(Request.SetupData.bmRequestType,2) + ', bRequest=' + IntToHex(Request.SetupData.bRequest,2) + ', wValue=' + IntToHex(Request.SetupData.wValue,4) + ', wIndex=' + IntToHex(Request.SetupData.wIndex,4) + ', wLength=' + IntToHex(Request.SetupData.wLength,4));
    end;
  end;  
 {$ENDIF}
 
 {Acquire the Lock}
 if MutexLock(Request.Device.Lock) = ERROR_SUCCESS then
  begin
   try
    {Cancel Request}
    Result:=Request.Device.Host.HostCancel(Request.Device.Host,Request);
   finally
    {Release the Lock}
    MutexUnlock(Request.Device.Lock);
   end;   
  end; 
end;

{==============================================================================}

procedure USBRequestComplete(Request:PUSBRequest);
{Called by a host controller when a USB request completes}
{Request: The USB request which has completed}
var
 {$IFDEF USB_DEBUG}
 Direction:Byte;
 TransferType:Byte;
 {$ENDIF}
 Message:TMessage;
begin
 {}
 {Check Request}
 if Request = nil then Exit;
 if Request.Device = nil then Exit;

 {Acquire the Lock}
 if MutexLock(Request.Device.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check State}
    if Request.Device.USBState = USB_STATE_DETACHING then
     begin
      {$IFDEF USB_DEBUG}
      if USB_LOG_ENABLED then USBLogDebug(Request.Device,'Device detachment pending, setting request status to USB_STATUS_DEVICE_DETACHED');
      {$ENDIF}
      
      {Update Request}
      Request.Status:=USB_STATUS_DEVICE_DETACHED;
     end;
 
    {$IFDEF USB_DEBUG}
    {Log Completion}
    if USB_LOG_ENABLED then 
     begin
      if Request.Endpoint <> nil then
       begin
        TransferType:=Request.Endpoint.bmAttributes and USB_TRANSFER_TYPE_MASK;
        Direction:=Request.Endpoint.bEndpointAddress shr 7;
       end
      else
       begin
        TransferType:=USB_TRANSFER_TYPE_CONTROL;
        Direction:=Request.SetupData.bmRequestType shr 7;
       end;
      USBLogDebug(Request.Device,'Calling completion callback (Actual transfer size ' + IntToStr(Request.ActualSize) + ' of ' + IntToStr(Request.Size) + ' bytes, Type=' + USBTransferTypeToString(TransferType) + ', Dir=' + USBDirectionToString(Direction) + ', Status=' + IntToStr(Request.Status) + ')');
     end;
    {$ENDIF}
    
    {Check Result}
    if Request.Status <> USB_STATUS_SUCCESS then
     begin
      {Update Statistics}
      Inc(Request.Device.RequestErrors); 
      
      {Update Status}
      Request.Device.LastError:=Request.Status;
     end;
 
    {Release the Lock}
    MutexUnlock(Request.Device.Lock);
 
    {Completion Callback} 
    Request.Callback(Request);
 
    {Acquire the Lock}
    if MutexLock(Request.Device.Lock) <> ERROR_SUCCESS then Exit;

    {Update Pending}
    Dec(Request.Device.PendingCount); 
    
    {Check State and Pending Requests}
    if (Request.Device.USBState = USB_STATE_DETACHING) and (Request.Device.PendingCount = 0) then
     begin
      {Check Waiter}
      if Request.Device.WaiterThread <> INVALID_HANDLE_VALUE then
       begin
        {$IFDEF USB_DEBUG}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'Device detachment pending, sending message to waiter thread (Thread=' + IntToHex(Request.Device.WaiterThread,8) + ')');
        {$ENDIF}
        
        {Send Message}
        FillChar(Message,SizeOf(TMessage),0);
        ThreadSendMessage(Request.Device.WaiterThread,Message);
        Request.Device.WaiterThread:=INVALID_HANDLE_VALUE;
       end; 
     end;
   finally
    {Release the Lock}
    MutexUnlock(Request.Device.Lock);
   end;   
  end; 
end;

{==============================================================================}

function USBControlRequest(Device:PUSBDevice;Endpoint:PUSBEndpointDescriptor;bRequest,bmRequestType:Byte;wValue,wIndex:Word;Data:Pointer;wLength:Word):LongWord; inline;
{Send of USB control request to the specified device and wait for the request to complete}
{Device: The USB device to send the control request to}
{Endpoint: The Endpoint to use for the control request (or nil for the default control endpoint)}
{bRequest: The request to send (See Section 9.4 of the USB 2.0 specification for Standard requests)}
{bmRequestType: Type of request to send (See Section 9.3.1 of the USB 2.0 specification for Standard request types)}
{wValue: Request specific data}
{wIndex: Request specific data}
{Data: Buffer for the data to be sent or received from the request (Ignored if wLength is 0)}
{wLength: Length of the Data buffer in bytes}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=USBControlRequestEx(Device,Endpoint,bRequest,bmRequestType,wValue,wIndex,Data,wLength,INFINITE,False);
end;

{==============================================================================}

function USBControlRequestEx(Device:PUSBDevice;Endpoint:PUSBEndpointDescriptor;bRequest,bmRequestType:Byte;wValue,wIndex:Word;Data:Pointer;wLength:Word;Timeout:LongWord;AllowShort:Boolean):LongWord;
{Send of USB control request to the specified device and wait for the request to complete}
{Device: The USB device to send the control request to}
{Endpoint: The Endpoint to use for the control request (or nil for the default control endpoint)}
{bRequest: The request to send (See Section 9.4 of the USB 2.0 specification for Standard requests)}
{bmRequestType: Type of request to send (See Section 9.3.1 of the USB 2.0 specification for Standard request types)}
{wValue: Request specific data}
{wIndex: Request specific data}
{Data: Buffer for the data to be sent or received from the request (Ignored if wLength is 0)}
{wLength: Length of the Data buffer in bytes}
{Timeout: Milliseconds to wait for request to complete (INFINITE to wait forever)}
{AllowShort: Allow the return size to be less than the requested size}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var
 Status:LongWord;
 Request:PUSBRequest;
 ResultCode:LongWord;
 Semaphore:TSemaphoreHandle;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;
 
 {Check Timeout}
 if Timeout = 0 then
  begin
   Timeout:=INFINITE;
  end;
  
 {Create Semaphore}
 Semaphore:=SemaphoreCreate(0);
 if Semaphore = INVALID_HANDLE_VALUE then
  begin
   {Return Result}
   Result:=USB_STATUS_OPERATION_FAILED;
   Exit; 
  end;
 
 {Create Request} 
 Request:=USBRequestAllocateEx(Device,Endpoint,USBControlRequestComplete,Data,wLength,Pointer(Semaphore));
 if Request = nil then
  begin
   {Destroy Semaphore}
   SemaphoreDestroy(Semaphore);
   
   {Return Result}
   Result:=USB_STATUS_OPERATION_FAILED;
   Exit;
  end;
 
 {Create Setup Data}
 Request.SetupData:=USBBufferAllocate(Device,SizeOf(TUSBControlSetupData));
 if Request.SetupData = nil then
  begin
   {Release Request}
   USBRequestRelease(Request);
   
   {Destroy Semaphore}
   SemaphoreDestroy(Semaphore);
   
   {Return Result}
   Result:=USB_STATUS_OPERATION_FAILED;
   Exit;
  end;
 
 {Initialize Setup Data}
 Request.SetupData.bmRequestType:=bmRequestType;
 Request.SetupData.bRequest:=bRequest;
 Request.SetupData.wValue:=wValue;
 Request.SetupData.wIndex:=wIndex;
 Request.SetupData.wLength:=wLength;

 {Submit Request} 
 Status:=USBRequestSubmit(Request);
 if Status = USB_STATUS_SUCCESS then
  begin
   {Wait for Completion}
   ResultCode:=SemaphoreWaitEx(Semaphore,Timeout);
   if ResultCode = ERROR_SUCCESS then
    begin
     {Get Status}
     Status:=Request.Status;
     
     {Force error if actual size was not the same as requested size}
     if (Status = USB_STATUS_SUCCESS) and (Request.ActualSize <> Request.Size) and not(AllowShort) then
      begin
       Status:=USB_STATUS_INVALID_DATA;
       
       {Update Statistics}
       Inc(Request.Device.RequestErrors);
       
       {Update Status}
       Request.Device.LastError:=Status;
      end;
    end
   else if ResultCode = ERROR_WAIT_TIMEOUT then
    begin
     if USB_LOG_ENABLED then USBLogError(Device,'Control request timeout (Timeout=' + IntToStr(Timeout) + ')');
     
     {Get Status}
     Status:=USB_STATUS_TIMEOUT;
     
     {Cancel Request}
     USBRequestCancel(Request);
     
     {Wait for Cancel}
     SemaphoreWait(Semaphore);
    end
   else
    begin
     if USB_LOG_ENABLED then USBLogError(Device,'Control request failure (Error=' + ErrorToString(ResultCode) + ')');
     
     {Get Status}
     Status:=USB_STATUS_OPERATION_FAILED;
     
     {Cancel Request}
     USBRequestCancel(Request);
     
     {Wait for Cancel}
     SemaphoreWait(Semaphore); 
    end;    
  end;  
 
 {Release Setup Data}
 USBBufferRelease(Request.SetupData);
 
 {Release Request}
 USBRequestRelease(Request);
 
 {Destroy Semaphore}
 SemaphoreDestroy(Semaphore); 
 
 {Return Result}
 Result:=Status;
end;

{==============================================================================}

procedure USBControlRequestComplete(Request:PUSBRequest);
{Called when a USB request from a USB control endpoint completes}
{Request: The USB request which has completed}
begin
 {}
 {Check Request}
 if Request = nil then Exit;
 
 {Check Semaphore}
 if Request.DriverData = nil then Exit;
 
 {Signal Semaphore}
 SemaphoreSignal(TSemaphoreHandle(Request.DriverData));
end;

{==============================================================================}
{==============================================================================}
{USB Hub Functions}
function USBHubCreatePorts(Hub:PUSBHub):LongWord;
{Create and initialize the ports for a Hub}
{Hub: The hub to initialize ports for}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var
 Count:LongWord;
 Device:PUSBDevice;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Hub}
 if Hub = nil then Exit;
 if Hub.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Get Device}
 Device:=PUSBDevice(Hub.Device.DeviceData);
 if Device = nil then Exit;
 
 {Acquire the Lock}
 if MutexLock(Hub.Lock) = ERROR_SUCCESS then
  begin
   try 
    {Check Ports}
    if Length(Hub.Ports) > 0 then
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'Hub: Hub already initialized for ' + IntToStr(Length(Hub.Ports)) + ' ports');
      
      Result:=USB_STATUS_OPERATION_FAILED;
      Exit;
     end;

    {Create Ports}
    SetLength(Hub.Ports,Hub.Descriptor.bNbrPorts);
    
    {Initialize Ports}
    for Count:=0 to Hub.Descriptor.bNbrPorts - 1 do
     begin
      Hub.Ports[Count].Hub:=Hub;
      Hub.Ports[Count].Number:=Count + 1;   
      Hub.Ports[Count].Child:=nil;
      Hub.Ports[Count].Status.wPortStatus:=0;
      Hub.Ports[Count].Status.wPortChange:=0;
     end;
  
    {Return Result}
    Result:=USB_STATUS_SUCCESS;
   finally
    {Release the Lock}
    MutexUnlock(Hub.Lock);
   end;
  end
 else
  begin
   Result:=USB_STATUS_OPERATION_FAILED;
  end;  
end;
 
{==============================================================================}

function USBHubPowerOnPorts(Hub:PUSBHub):LongWord;
{Power on all ports on a Hub}
{Hub: The hub to power on ports for}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var
 Count:LongWord;
 Status:LongWord;
 Device:PUSBDevice;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Hub}
 if Hub = nil then Exit;
 if Hub.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Get Device}
 Device:=PUSBDevice(Hub.Device.DeviceData);
 if Device = nil then Exit;
 
 {Acquire the Lock}
 if MutexLock(Hub.Lock) = ERROR_SUCCESS then
  begin
   try 
    {$IFDEF USB_DEBUG}
    if USB_LOG_ENABLED then USBLogDebug(Device,'Hub: Powering on ' + IntToStr(Hub.Descriptor.bNbrPorts) + ' USB ports');
    {$ENDIF}
 
    {Power on Ports}
    for Count:=0 to Hub.Descriptor.bNbrPorts - 1 do
     begin
      Status:=USBHubPortSetFeature(@Hub.Ports[Count],USB_PORT_POWER);
      if Status <> USB_STATUS_SUCCESS then
       begin
        if USB_LOG_ENABLED then USBLogError(Device,'Hub: Failed to power on port ' + IntToStr(Count) + ': ' + USBStatusToString(Status));
        Result:=Status;
        Exit;
       end;
     end;
 
    {Wait for Power Good}
    {According to the section 11.11 of the USB 2.0 specification, bPwrOn2PwrGood of the hub descriptor is the
     time (in 2 ms intervals) from the time the power-on sequence begins on a port until power is good on that port}
    ThreadSleep(2 * Hub.Descriptor.bPwrOn2PwrGood);
 
    {Return Result}
    Result:=USB_STATUS_SUCCESS;
   finally
    {Release the Lock}
    MutexUnlock(Hub.Lock);
   end;
  end
 else
  begin
   Result:=USB_STATUS_OPERATION_FAILED;
  end;
end;

{==============================================================================}

function USBHubCreateHubDescriptor(Hub:PUSBHub):LongWord;
{Allocate the hub descriptor for the specified hub}
{Hub: The hub to create the descriptor for}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
{Note: The class specific hub descriptor is not the same as the device descriptor}
var
 Device:PUSBDevice;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Hub}
 if Hub = nil then Exit;
 if Hub.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Get Device}
 Device:=PUSBDevice(Hub.Device.DeviceData);
 if Device = nil then Exit;
 
 {Acquire the Lock}
 if MutexLock(Hub.Lock) = ERROR_SUCCESS then
  begin
   try 
    {$IFDEF USB_DEBUG}
    if USB_LOG_ENABLED then USBLogDebug(Device,'Hub: Allocating hub descriptor');
    {$ENDIF}
    
    {Check Descriptor}
    Result:=USB_STATUS_OPERATION_FAILED;
    if Hub.Descriptor <> nil then Exit;
    
    {Allocate Descriptor}
    Hub.Descriptor:=USBBufferAllocate(Device,SizeOf(TUSBHubDescriptor) + SizeOf(TUSBHubDescriptorData));
    if Hub.Descriptor = nil then Exit;
    
    {Return Result} 
    Result:=USB_STATUS_SUCCESS;
   finally
    {Release the Lock}
    MutexUnlock(Hub.Lock);
   end;
  end
 else
  begin
   Result:=USB_STATUS_OPERATION_FAILED;
  end;  
end; 
 
{==============================================================================}

function USBHubReadHubDescriptor(Hub:PUSBHub):LongWord;
{Read the hub descriptor for the specified hub}
{Hub: The hub to read the descriptor for}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
{Note: The class specific hub descriptor is not the same as the device descriptor}
var
 Status:LongWord;
 Device:PUSBDevice;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Hub}
 if Hub = nil then Exit;
 if Hub.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Get Device}
 Device:=PUSBDevice(Hub.Device.DeviceData);
 if Device = nil then Exit;

 {Check Descriptor}
 if Hub.Descriptor = nil then Exit;
 
 {Acquire the Lock}
 if MutexLock(Hub.Lock) = ERROR_SUCCESS then
  begin
   try 
    {$IFDEF USB_DEBUG}
    if USB_LOG_ENABLED then USBLogDebug(Device,'Hub: Reading hub descriptor');
    {$ENDIF}

    {Get Descriptor}
    Status:=USBDeviceGetDescriptor(Device,USB_HUB_REQUEST_GET_DESCRIPTOR,USB_BMREQUESTTYPE_DIR_IN or USB_BMREQUESTTYPE_TYPE_CLASS or USB_BMREQUESTTYPE_RECIPIENT_DEVICE,(USB_DESCRIPTOR_TYPE_HUB shl 8),0,Hub.Descriptor,SizeOf(TUSBHubDescriptor) + SizeOf(TUSBHubDescriptorData));
    if Status <> USB_STATUS_SUCCESS then
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'Hub: Failed to read hub descriptor: ' + USBStatusToString(Status));
     end;
     
    {Return Result} 
    Result:=Status;
   finally
    {Release the Lock}
    MutexUnlock(Hub.Lock);
   end;
  end
 else
  begin
   Result:=USB_STATUS_OPERATION_FAILED;
  end;  
end;

{==============================================================================}

function USBHubLock(Hub:PUSBHub):LongWord;
{Lock the specified Hub to prevent changes}
{Hub: The hub to lock}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Hub}
 if Hub = nil then Exit;
 if Hub.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 Result:=MutexLock(Hub.Lock);
end; 
 
{==============================================================================}

function USBHubUnlock(Hub:PUSBHub):LongWord;
{Unlock the specified Hub to allow changes}
{Hub: The hub to unlock}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Hub}
 if Hub = nil then Exit;
 if Hub.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Release the Lock}
 Result:=MutexUnlock(Hub.Lock);
end; 

{==============================================================================}

function USBHubSetState(Hub:PUSBHub;State:LongWord):LongWord;
{Set the state of the specified hub and send a notification}
{Hub: The hub to set the state for}
{State: The new state to set and notify}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Hub}
 if Hub = nil then Exit;
 if Hub.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check State}
 if State > USBHUB_STATE_ATTACHED then Exit;
 
 {Check State}
 if Hub.HubState = State then
  begin
   {Return Result}
   Result:=ERROR_SUCCESS;
  end
 else
  begin
   {Acquire the Lock}
   if MutexLock(Hub.Lock) = ERROR_SUCCESS then
    begin
     try 
      {Set State}
      Hub.HubState:=State;
  
      {Notify State}
      NotifierNotify(@Hub.Device,USBHubStateToNotification(State));

      {Return Result}
      Result:=ERROR_SUCCESS;
     finally
      {Release the Lock}
      MutexUnlock(Hub.Lock);
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end;  
end;

{==============================================================================}

function USBHubAllocate(Device:PUSBDevice):PUSBHub;
{Create and Register a new Hub device}
{Device: The USB device that represents this hub}
{Return: Pointer to new Hub entry or nil if hub could not be created}
var
 Hub:PUSBHub;
 HubId:LongWord;
begin
 {}
 Result:=nil;
 
 {Check Device}
 if Device = nil then Exit;
 
 {Create Hub}
 Hub:=PUSBHub(DeviceCreateEx(SizeOf(TUSBHub)));
 if Hub = nil then Exit;
 
 {Update Hub}
 {Device}
 Hub.Device.DeviceBus:=DEVICE_BUS_USB;   
 Hub.Device.DeviceType:=USBHUB_TYPE_NONE;
 Hub.Device.DeviceFlags:=USBHUB_FLAG_NONE;
 Hub.Device.DeviceData:=Device;
 {Hub}
 Hub.HubId:=DEVICE_ID_ANY;
 Hub.HubState:=USBHUB_STATE_DETACHED;
 {Driver}
 Hub.Lock:=INVALID_HANDLE_VALUE;
 Hub.Descriptor:=nil;
 {USB}
 Hub.StatusData:=nil;
 Hub.StatusRequest:=nil;
 Hub.StatusEndpoint:=nil;
 Hub.PendingCount:=0;
 Hub.WaiterThread:=INVALID_HANDLE_VALUE;
 
 {Create Lock}
 Hub.Lock:=MutexCreate;
 if Hub.Lock = INVALID_HANDLE_VALUE then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Hub: Failed to create lock for USB hub');
   
   {Destroy Device}
   DeviceDestroy(@Hub.Device);
   Exit;
  end;
 
 {Insert Hub}
 if CriticalSectionLock(USBHubTableLock) = ERROR_SUCCESS then
  begin
   try
    {Update Hub}
    HubId:=0;
    while USBHubFind(HubId) <> nil do
     begin
      Inc(HubId);
     end;
    Hub.HubId:=HubId;
 
    {Update Device}
    Hub.Device.DeviceName:=USB_HUB_PREFIX + IntToStr(Hub.HubId);
    Hub.Device.DeviceClass:=DEVICE_CLASS_USBHUB;
 
    {Register Device}
    if DeviceRegister(@Hub.Device) <> ERROR_SUCCESS then
     begin
      Hub.HubId:=DEVICE_ID_ANY;
      Exit;
     end; 
 
    {Link Hub}
    if USBHubTable = nil then
     begin
      USBHubTable:=Hub;
     end
    else
     begin
      Hub.Next:=USBHubTable;
      USBHubTable.Prev:=Hub;
      USBHubTable:=Hub;
     end;
 
    {Increment Count}
    Inc(USBHubTableCount);
 
    {Return Result}
    Result:=Hub;
   finally
    CriticalSectionUnlock(USBHubTableLock);
   end;
  end
end;

{==============================================================================}

function USBHubRelease(Hub:PUSBHub):LongWord;
{Deregister and Destroy a Hub device}
{Hub: The hub to deregister and destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Prev:PUSBHub;
 Next:PUSBHub;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Hub}
 if Hub = nil then Exit;
 if Hub.HubId = DEVICE_ID_ANY then Exit;
 if Hub.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Hub}
 Result:=ERROR_NOT_FOUND;
 if USBHubCheck(Hub) <> Hub then Exit;
 
 {Check State}
 if Hub.Device.DeviceState <> DEVICE_STATE_REGISTERED then Exit;
 
 {Acquire the Lock}
 Result:=MutexLock(Hub.Lock);
 if Result <> ERROR_SUCCESS then Exit;
 
 {Remove Hub}
 if CriticalSectionLock(USBHubTableLock) = ERROR_SUCCESS then
  begin
   try
    {Deregister Device}
    Result:=DeviceDeregister(@Hub.Device);
    if Result <> ERROR_SUCCESS then Exit;
 
    {Unlink Hub}
    Prev:=Hub.Prev;
    Next:=Hub.Next;
    if Prev = nil then
     begin
      USBHubTable:=Next;
      if Next <> nil then
       begin
        Next.Prev:=nil;
       end;       
     end
    else
     begin
      Prev.Next:=Next;
      if Next <> nil then
       begin
        Next.Prev:=Prev;
       end;       
     end;     
 
    {Decrement Count}
    Dec(USBHubTableCount);
 
    {Update Hub}
    Hub.HubId:=DEVICE_ID_ANY;
  
    {Free Hub Descriptor}
    if Hub.Descriptor <> nil then
     begin
      USBBufferRelease(Hub.Descriptor);
     end; 
    
    {Free Hub Ports}
    SetLength(Hub.Ports,0);
    
    {Release the Lock}
    MutexUnlock(Hub.Lock);
    
    {Free the Lock}
    MutexDestroy(Hub.Lock);
 
    {Destroy Hub} 
    Result:=DeviceDestroy(@Hub.Device);
   finally
    CriticalSectionUnlock(USBHubTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function USBHubFind(HubId:LongWord):PUSBHub;
var
 Hub:PUSBHub;
begin
 {}
 Result:=nil;
 
 {Check Id}
 if HubId = DEVICE_ID_ANY then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(USBHubTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Hub}
    Hub:=USBHubTable;
    while Hub <> nil do
     begin
      {Check State}
      if Hub.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Id}
        if Hub.HubId = HubId then
         begin
          Result:=Hub;
          Exit;
         end;
       end;
       
      {Get Next}
      Hub:=Hub.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(USBHubTableLock);
   end;
  end;
end;

{==============================================================================}

function USBHubEnumerate(Callback:TUSBHubEnumerate;Data:Pointer):LongWord;
var
 Hub:PUSBHub;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Callback}
 if not Assigned(Callback) then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(USBHubTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Hub}
    Hub:=USBHubTable;
    while Hub <> nil do
     begin
      {Check State}
      if Hub.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        if Callback(Hub,Data) <> ERROR_SUCCESS then Exit;
       end;
       
      {Get Next}
      Hub:=Hub.Next;
     end;
     
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(USBHubTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function USBHubNotification(Hub:PUSBHub;Callback:TUSBHubNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Hub}
 if Hub = nil then
  begin
   Result:=DeviceNotification(nil,DEVICE_CLASS_USBHUB,Callback,Data,Notification,Flags);
  end
 else
  begin
   {Check Hub}
   if Hub.Device.Signature <> DEVICE_SIGNATURE then Exit;

   Result:=DeviceNotification(@Hub.Device,DEVICE_CLASS_USBHUB,Callback,Data,Notification,Flags);
  end; 
end;

{==============================================================================}

procedure USBHubBindDevices(Device:PUSBDevice;Callback:TUSBDeviceBind);
{Enumerate each device in the USB tree and call a bind callback for each one}
{Device: USB device at which to start the enueration}
{Callback: Bind callback function to execute for each device}
var
 Hub:PUSBHub;
 Count:LongWord;
begin
 {}
 {Check Device}
 if Device = nil then Exit;
 
 {Check Callback}
 if not(Assigned(Callback)) then Exit;
 
 {Bind Device}
 Callback(Device);

 {Check Hub}
 if USBIsHub(Device) then
  begin
   {Get Hub}
   Hub:=PUSBHub(Device.DriverData);
   if Hub <> nil then
    begin
     {Acquire the Lock}
     if MutexLock(Hub.Lock) = ERROR_SUCCESS then
      begin
       try 
        {Check Ports}
        for Count:=0 to Hub.Descriptor.bNbrPorts - 1 do
         begin
          {Bind Port Device}
          USBHubBindDevices(Hub.Ports[Count].Child,Callback);
         end;
       finally
        {Release the Lock}
        MutexUnlock(Hub.Lock);
       end;
      end;
    end;  
  end;
end;

{==============================================================================}

procedure USBHubUnbindDevices(Device:PUSBDevice;Driver:PUSBDriver;Callback:TUSBDeviceUnbind);
{Enumerate each device in the USB tree and call an unbind callback for each one}
{Device: USB device at which to start the enueration}
{Callback: Unbind callback function to execute for each device}
var
 Hub:PUSBHub;
 Count:LongWord;
begin
 {}
 {Check Device}
 if Device = nil then Exit;
 
 {Check Driver}
 {if Driver = nil then Exit;} {Can be nil}
 
 {Check Callback}
 if not(Assigned(Callback)) then Exit;

 {Check Hub}
 if USBIsHub(Device) then
  begin
   {Get Hub}
   Hub:=PUSBHub(Device.DriverData);
   if Hub <> nil then
    begin
     {Acquire the Lock}
     if MutexLock(Hub.Lock) = ERROR_SUCCESS then
      begin
       try 
        {Check Ports}
        for Count:=0 to Hub.Descriptor.bNbrPorts - 1 do
         begin
          {Unbind Port Device}
          USBHubUnbindDevices(Hub.Ports[Count].Child,Driver,Callback);
         end;
       finally
        {Release the Lock}
        MutexUnlock(Hub.Lock);
       end;
      end;
    end;  
  end;
  
 {Unbind Device}
 Callback(Device,Driver);
end;

{==============================================================================}

procedure USBHubEnumerateDevices(Device:PUSBDevice;Callback:TUSBDeviceEnumerate;Data:Pointer);
{Enumerate each device in the USB tree and call an enumerate callback for each one}
{Device: USB device at which to start the enueration}
{Callback: Enumerate callback function to execute for each device}
var
 Hub:PUSBHub;
 Count:LongWord;
begin
 {}
 {Check Device}
 if Device = nil then Exit;
 
 {Check Callback}
 if not(Assigned(Callback)) then Exit;
 
 {Enumerate Device}
 Callback(Device,Data);
 
 {Check Hub}
 if USBIsHub(Device) then
  begin
   {Get Hub}
   Hub:=PUSBHub(Device.DriverData);
   if Hub <> nil then
    begin
     {Acquire the Lock}
     if MutexLock(Hub.Lock) = ERROR_SUCCESS then
      begin
       try 
        {Check Ports}
        for Count:=0 to Hub.Descriptor.bNbrPorts - 1 do
         begin
          {Enumerate Port Device}
          USBHubEnumerateDevices(Hub.Ports[Count].Child,Callback,Data);
         end;
       finally
        {Release the Lock}
        MutexUnlock(Hub.Lock);
       end;
      end;
    end;
  end;
end;

{==============================================================================}

function USBHubPortReset(Port:PUSBPort):LongWord;
{Reset a the specified USB port}
{Port: USB port to reset}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}

{Note: Caller must hold the hub lock}
var
 Status:LongWord;
 Timeout:LongWord;
 Device:PUSBDevice;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Port}
 if Port = nil then Exit;
 if Port.Hub = nil then Exit;
 if Port.Hub.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Get Device}
 Device:=PUSBDevice(Port.Hub.Device.DeviceData);
 if Device = nil then Exit;
 
 {$IFDEF USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'Hub: Resetting port ' + IntToStr(Port.Number));
 {$ENDIF}

 {Reset Port}
 Status:=USBHubPortSetFeature(Port,USB_PORT_RESET);
 if Status <> USB_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;
 
 {Check Status}
 Timeout:=0;
 while (Port.Status.wPortStatus and USB_PORT_STATUS_RESET) = USB_PORT_STATUS_RESET do
  begin
   {Wait for Reset}
   ThreadSleep(USB_PORT_RESET_DELAY);
   
   {Get Status}
   Status:=USBHubPortGetStatus(Port);
   if Status <> USB_STATUS_SUCCESS then
    begin
     Result:=Status;
     Exit;
    end;
   
   {Check Timeout}
   if Timeout >= USB_PORT_RESET_TIMEOUT then
    begin
     Result:=USB_STATUS_TIMEOUT;
     Exit;
    end;
   
   {Increment Timeout}
   Timeout:=Timeout + USB_PORT_RESET_DELAY;
  end;
  
 {Wait for Recovery}
 {Section 9.2.6.2 of the USB 2.0 specification states, After a port is reset or resumed, the USB System Software is expected to provide a recovery interval of 10 ms
  before the device attached to the port is expected to respond to data transfers. The device may ignore any data transfers during the recovery interval}
 ThreadSleep(USB_PORT_RESET_RECOVERY);
 
 {Return Result}
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function USBHubPortGetStatus(Port:PUSBPort):LongWord;
{Read the status of the specified USB port}
{Port: USB port to read status for}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}

{Note: Caller must hold the hub lock}
var
 Status:LongWord;
 Device:PUSBDevice;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Port}
 if Port = nil then Exit;
 if Port.Hub = nil then Exit;
 if Port.Hub.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Get Device}
 Device:=PUSBDevice(Port.Hub.Device.DeviceData);
 if Device = nil then Exit;
 
 {$IFDEF USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'Hub: Retrieving status of port ' + IntToStr(Port.Number));
 {$ENDIF}

 {Get Status}
 Status:=USBControlRequest(Device,nil,USB_HUB_REQUEST_GET_STATUS,USB_BMREQUESTTYPE_DIR_IN or USB_BMREQUESTTYPE_TYPE_CLASS or USB_BMREQUESTTYPE_RECIPIENT_OTHER,0,Port.Number,@Port.Status,SizeOf(TUSBPortStatus));
 if Status <> USB_STATUS_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Hub: Failed to get status for port ' + IntToStr(Port.Number) +  ': ' + USBStatusToString(Status));
  end;
 
 {Return Result}
 Result:=Status;
end;

{==============================================================================}

function USBHubPortSetFeature(Port:PUSBPort;Feature:Word):LongWord;
{Enable a feature on the specified USB port}
{Port: USB port to enable the feature on}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}

{Note: Caller must hold the hub lock}
begin
 {}
 Result:=USBHubPortChangeFeature(Port,Feature,True);
end;

{==============================================================================}

function USBHubPortClearFeature(Port:PUSBPort;Feature:Word):LongWord;
{Disable a feature on the specified USB port}
{Port: USB port to disable the feature on}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}

{Note: Caller must hold the hub lock}
begin
 {}
 Result:=USBHubPortChangeFeature(Port,Feature,False);
end;

{==============================================================================}

function USBHubPortChangeFeature(Port:PUSBPort;Feature:Word;Enable:Boolean):LongWord;
{Enable or disable a feature on the specified USB port}
{Port: USB port to enable or disable the feature on}
{Feature: The feature to enable or disable}
{Enable: True to enable the feature or False to disable the feature}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}

{Note: Caller must hold the hub lock}
var
 bRequest:Byte;
 Device:PUSBDevice;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Port}
 if Port = nil then Exit;
 if Port.Hub = nil then Exit;
 if Port.Hub.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Get Device}
 Device:=PUSBDevice(Port.Hub.Device.DeviceData);
 if Device = nil then Exit;
 
 {Check Enable}
 bRequest:=USB_HUB_REQUEST_CLEAR_FEATURE;
 if Enable then bRequest:=USB_HUB_REQUEST_SET_FEATURE;
 
 {Set/Clear Feature}
 Result:=USBControlRequest(Device,nil,bRequest,USB_BMREQUESTTYPE_DIR_OUT or USB_BMREQUESTTYPE_TYPE_CLASS or USB_BMREQUESTTYPE_RECIPIENT_OTHER,Feature,Port.Number,nil,0);
end;

{==============================================================================}

function USBHubPortAttachDevice(Port:PUSBPort):LongWord;
{Attach a newly connected USB device to the specified USB port}
{Port: USB port to attach the new device to}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure} 

{Note: Caller must hold the hub lock} 
{Note: Only called in response to a status change on the hub}
var
 Status:LongWord;
 Device:PUSBDevice;
 Parent:PUSBDevice;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Port}
 if Port = nil then Exit;
 if Port.Hub = nil then Exit;
 if Port.Hub.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Get Parent}
 Parent:=PUSBDevice(Port.Hub.Device.DeviceData);
 if Parent = nil then Exit;
 
 {$IFDEF USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'Hub: Port ' + IntToStr(Port.Number) + ' device attached');
 {$ENDIF}
 
 {Reset Port}
 Status:=USBHubPortReset(Port);
 if Status <> USB_STATUS_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Parent,'Hub: Failed to reset port ' + IntToStr(Port.Number) + ': ' + USBStatusToString(Status));
   
   {Return Result}
   Result:=Status;
   Exit;
  end;
  
 {Allocate Device} 
 Device:=USBDeviceAllocate(Parent.Host,Parent);
 if Device = nil then
  begin
   if USB_LOG_ENABLED then USBLogError(Parent,'Hub: Failed to allocate new USB device');
   
   {Disable Port}
   USBHubPortClearFeature(Port,USB_PORT_ENABLE);
   
   {Return Result}
   Result:=USB_STATUS_OPERATION_FAILED;
   Exit;  
  end;
 
 {Get Port Status}
 Status:=USBHubPortGetStatus(Port);
 if Status <> USB_STATUS_SUCCESS then
  begin
   {Disable Port}
   USBHubPortClearFeature(Port,USB_PORT_ENABLE);
   
   {Release Device}
   USBDeviceRelease(Device);

   {Return Result}
   Result:=Status;
   Exit;
  end;

 {Get Device Speed}
 if (Port.Status.wPortStatus and USB_PORT_STATUS_HIGH_SPEED_ATTACHED) = USB_PORT_STATUS_HIGH_SPEED_ATTACHED then
  begin
   Device.Speed:=USB_SPEED_HIGH;
  end
 else if (Port.Status.wPortStatus and USB_PORT_STATUS_LOW_SPEED_ATTACHED) = USB_PORT_STATUS_LOW_SPEED_ATTACHED then
  begin
   Device.Speed:=USB_SPEED_LOW;  
  end
 else 
  begin
   Device.Speed:=USB_SPEED_FULL;
  end;  

 {Get Port Number}
 if USB_LOG_ENABLED then USBLogInfo(Parent,'Hub: New ' + USBSpeedToString(Device.Speed) + '-speed device connected to port ' + IntToStr(Port.Number));
 Device.PortNumber:=Port.Number;
  
 {Attach Device}
 Status:=USBDeviceAttach(Device);
 if Status <> USB_STATUS_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Parent,'Hub: Failed to attach new device to port ' + IntToStr(Port.Number) + ': ' + USBStatusToString(Status));
   
   {Disable Port}
   USBHubPortClearFeature(Port,USB_PORT_ENABLE);

   {Release Device}
   USBDeviceRelease(Device);

   {Return Result}
   Result:=Status;
   Exit; 
  end;
  
 {Update Port}
 Port.Child:=Device;
 
 {Return Result}
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function USBHubPortDetachDevice(Port:PUSBPort):LongWord;
{Detach a disconnected USB device from the specified USB port}
{Port: USB port to detach the device from}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure} 

{Note: Caller must hold the hub lock} 
{Note: Only called in response to a status change on the hub}
var
 Status:LongWord;
 Device:PUSBDevice;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Port}
 if Port = nil then Exit;
 if Port.Hub = nil then Exit;
 if Port.Hub.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Get Device}
 Device:=PUSBDevice(Port.Hub.Device.DeviceData);
 if Device = nil then Exit;

 {$IFDEF USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'Hub: Port ' + IntToStr(Port.Number) + ' device detached');
 {$ENDIF}
 
 {Check Child} 
 if Port.Child <> nil then
  begin
   if USB_LOG_ENABLED then USBLogInfo(Device,'Hub: Detaching ' + USBDeviceToString(Port.Child));
   
   {Unbind Child}
   Status:=USBDeviceUnbind(Port.Child,nil);
   if (Status <> USB_STATUS_SUCCESS) and (Status <> USB_STATUS_NOT_BOUND) then
    begin
     if USB_LOG_ENABLED then USBLogError(Device,'Hub: Failed to unbind device from port ' + IntToStr(Port.Number) + ': ' + USBStatusToString(Status));
     {Do not exit}
    end;
    
   {Detach Child}
   Status:=USBDeviceDetach(Port.Child);
   if Status <> USB_STATUS_SUCCESS then
    begin
     if USB_LOG_ENABLED then USBLogError(Device,'Hub: Failed to detach device from port ' + IntToStr(Port.Number) + ': ' + USBStatusToString(Status));
     {Do not exit}
    end;
   
   {Release Child}
   USBDeviceRelease(Port.Child);
   
   {Update Port}
   Port.Child:=nil;
  end;
  
 {Return Result}
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function USBHubPortStatusChanged(Port:PUSBPort):LongWord;
{Process a status change for the specified USB port}
{Port: USB port to process the status change for}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure} 

{Note: Caller must hold the hub lock}
{Note: Only called in response to a status change on the hub}
var
 Status:LongWord;
 Device:PUSBDevice;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Port}
 if Port = nil then Exit;
 if Port.Hub = nil then Exit;
 if Port.Hub.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Get Device}
 Device:=PUSBDevice(Port.Hub.Device.DeviceData);
 if Device = nil then Exit;
 
 {$IFDEF USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'Hub: Port ' + IntToStr(Port.Number) + ' status change');
 {$ENDIF}
 
 {Get Port Status}
 Status:=USBHubPortGetStatus(Port); 
 if Status <> USB_STATUS_SUCCESS then
  begin
   Result:=Status;
   Exit;
  end;
  
 {$IFDEF USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'Hub: Port ' + IntToStr(Port.Number) + ' (wPortStatus=' + IntToHex(Port.Status.wPortStatus,4) + ', wPortChange=' + IntToHex(Port.Status.wPortChange,4) + ')');
 {$ENDIF}
 
 {Check Connected}
 if (Port.Status.wPortChange and USB_PORT_CHANGE_CONNECTED) = USB_PORT_CHANGE_CONNECTED then
  begin
   {$IFDEF USB_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Device,'Hub: Port ' + IntToStr(Port.Number) + ' device now ' + USBPortStatusConnectedToString(Port.Status.wPortStatus));
   {$ENDIF}

   {Connected Change (Device was Connected or Disconnected)}
   
   {Acknowledge Connected Change}
   USBHubPortClearFeature(Port,USB_C_PORT_CONNECTION);
   
   {Detach Device}
   USBHubPortDetachDevice(Port);
   
   {Check Connected}
   if (Port.Status.wPortStatus and USB_PORT_STATUS_CONNNECTED) = USB_PORT_STATUS_CONNNECTED then
    begin
     {Attach Device}
     USBHubPortAttachDevice(Port);
    end;
  end;
 
 {Check Enabled}
 if (Port.Status.wPortChange and USB_PORT_CHANGE_ENABLED) = USB_PORT_CHANGE_ENABLED then
  begin
   {$IFDEF USB_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Device,'Hub: Port ' + IntToStr(Port.Number) + ' enabled');
   {$ENDIF}

   {Acknowledge Enabled Change}
   USBHubPortClearFeature(Port,USB_C_PORT_ENABLE);
  end;
 
 {Check Reset} 
 if (Port.Status.wPortChange and USB_PORT_CHANGE_RESET) = USB_PORT_CHANGE_RESET then
  begin
   {$IFDEF USB_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Device,'Hub: Port ' + IntToStr(Port.Number) + ' reset');
   {$ENDIF}

   {Acknowledge Reset Change}
   USBHubPortClearFeature(Port,USB_C_PORT_RESET);
  end;
 
 {Check Suspended}
 if (Port.Status.wPortChange and USB_PORT_CHANGE_SUSPENDED) = USB_PORT_CHANGE_SUSPENDED then
  begin
   {$IFDEF USB_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Device,'Hub: Port ' + IntToStr(Port.Number) + ' suspended');
   {$ENDIF}

   {Acknowledge Suspended Change}
   USBHubPortClearFeature(Port,USB_C_PORT_SUSPEND);
  end;
 
 {Check Overcurrent}
 if (Port.Status.wPortChange and USB_PORT_CHANGE_OVERCURRENT) = USB_PORT_CHANGE_OVERCURRENT then
  begin
   {$IFDEF USB_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Device,'Hub: Port ' + IntToStr(Port.Number) + ' overcurrent');
   {$ENDIF}

   {Acknowledge Overcurrent Change}
   USBHubPortClearFeature(Port,USB_C_PORT_OVER_CURRENT);
  end;
 
 {Return Result}
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function USBHubExecute(Parameter:Pointer):PtrInt;
{The Hub thread procedure which is responsible for receiving completed hub status
 change requests, processing them for changes and then resubmitting the request
 
 One status change request exists for each hub currently connected and is created
 and submitted by the hub driver when it binds to a new hub device
 
 When a hub is disconnected the hub driver will remove the request when the hub is
 detached and destroyed}
var
 Hub:PUSBHub;
 Count:LongWord;
 Status:LongWord;
 Message:TMessage;
 PortMask:LongWord;
 ChangeMask:LongWord;
 Request:PUSBRequest;
begin
 {}
 Result:=0;
 try
  {$IFDEF USB_DEBUG}
  if USB_LOG_ENABLED then USBLogDebug(nil,'Hub: Thread ID = ' + IntToHex(ThreadID,8)); 
  {$ENDIF}
  
  while True do
   begin
    {Get Message}
    FillChar(Message,SizeOf(TMessage),0);
    Result:=MessageslotReceive(USBHubMessageslot,Message);
    if Result = ERROR_SUCCESS then
     begin
      {Get Request}
      Request:=PUSBRequest(Message.Msg);
      if Request <> nil then
       begin
        {Get Hub}
        Hub:=PUSBHub(Request.DriverData);
        if Hub <> nil then
         begin
          {Acquire the Lock}
          if MutexLock(Hub.Lock) = ERROR_SUCCESS then
           begin
            try 
             {Check Result}     
             if Request.Status = USB_STATUS_SUCCESS then
              begin
               {$IFDEF USB_DEBUG}
               if USB_LOG_ENABLED then USBLogDebug(Request.Device,'Hub: Processing hub status change');
               {$ENDIF}
               
               {The format of the message is a bitmap that indicates which ports have status changes, we ignore bit 0, which indicates status change of the hub device itself}
               {Note: The buffer is large enough to hold 64 bits but this code could only support 32 ports (PortMask), need to rework this for expandable support}
               
               {Determine Ports with Status Changes}
               PortMask:=0;
               for Count:=0 to Request.ActualSize - 1 do
                begin
                 ChangeMask:=PByte(LongWord(Request.Data) + Count)^;
                 PortMask:=PortMask or (ChangeMask shl (Count * 8));
                end;
        
               {Process Ports with Status Changes}
               for Count:=0 to Hub.Descriptor.bNbrPorts - 1 do
                begin
                 if (PortMask and (2 shl Count)) <> 0 then
                  begin
                   USBHubPortStatusChanged(@Hub.Ports[Count]);
                  end;
                end;
              end
             else
              begin
               {Note: This should never happen as completion only sends successful requests)}
               if USB_LOG_ENABLED then USBLogError(Request.Device,'Hub: Status change request failed: ' + USBStatusToString(Request.Status));
               
               {Update Statistics}
               Inc(Hub.ReceiveErrors); 
              end;      
 
             {Check State}
             if Hub.HubState <> USBHUB_STATE_DETACHING then
              begin
               {Update Pending}
               Inc(Hub.PendingCount);
                  
               {$IFDEF USB_DEBUG}
               if USB_LOG_ENABLED then USBLogDebug(Request.Device,'Hub: Resubmitting status change request');
               {$ENDIF}
 
               {Resubmit Status Change Request}
               Status:=USBRequestSubmit(Request);
               if Status <> USB_STATUS_SUCCESS then
                begin
                 if USB_LOG_ENABLED then USBLogError(Request.Device,'Hub: Failed to resubmit status change request: ' + USBStatusToString(Status));
    
                 {Update Pending}
                 Dec(Hub.PendingCount);
                end;
              end; 
            finally
             {Release the Lock}
             MutexUnlock(Hub.Lock);
            end;
           end
          else
           begin
            if USB_LOG_ENABLED then USBLogError(nil,'Hub: Failed to acquire hub lock'); 
           end;
         end
        else
         begin
          if USB_LOG_ENABLED then USBLogError(nil,'Hub: Status change request hub invalid (Hub=' + IntToHex(LongWord(Hub),8) + ')'); 
         end;        
       end
      else
       begin
        if USB_LOG_ENABLED then USBLogError(nil,'Hub: Status change request invalid (Request=' + IntToHex(LongWord(Request),8) + ')'); 
       end;
     end
    else
     begin
      if USB_LOG_ENABLED then USBLogError(nil,'Hub: Receive message failed (Result=' + ErrorToString(Result) + ')'); 
     end;    
   end;   
 except
  on E: Exception do
   begin
    if USB_LOG_ENABLED then USBLogError(nil,'HubThread: Exception: ' + E.Message + ' at ' + IntToHex(LongWord(ExceptAddr),8));
   end;
 end; 
end;

{==============================================================================}

procedure USBHubStatusComplete(Request:PUSBRequest);
{Called when the USB request from a USB hub IN interrupt endpoint completes}
{Request: The USB request which has completed}
var
 Hub:PUSBHub;
 Status:LongWord;
 Message:TMessage;
begin
 {}
 {Check Request}
 if Request <> nil then
  begin
   {Get Hub}
   Hub:=PUSBHub(Request.DriverData);
   if Hub <> nil then
    begin
     {Acquire the Lock}
     if MutexLock(Hub.Lock) = ERROR_SUCCESS then
      begin
       try 
        {Update Statistics}
        Inc(Hub.ReceiveCount); 
        
        {Check State}
        if Hub.HubState = USBHUB_STATE_DETACHING then
         begin
          {$IFDEF USB_DEBUG}
          if USB_LOG_ENABLED then USBLogDebug(Request.Device,'Hub: Detachment pending, setting report request status to USB_STATUS_DEVICE_DETACHED');
          {$ENDIF}
          
          {Update Request}
          Request.Status:=USB_STATUS_DEVICE_DETACHED;
         end;
   
        {Check Result (Only send successful requests)}
        if Request.Status = USB_STATUS_SUCCESS then
         begin
          {Send Message to the Hub Thread} 
          FillChar(Message,SizeOf(TMessage),0);
          Message.Msg:=LongWord(Request);
          MessageslotSend(USBHubMessageslot,Message);
         end
        else
         begin
          if USB_LOG_ENABLED then USBLogError(Request.Device,'Hub: Status change request failed: ' + USBStatusToString(Request.Status));
          
          {Update Statistics}
          Inc(Hub.ReceiveErrors); 
         end;
        
        {Update Pending} 
        Dec(Hub.PendingCount); 
        
        {Check State}
        if Hub.HubState = USBHUB_STATE_DETACHING then
         begin
          {Check Pending} 
          if Hub.PendingCount = 0 then
           begin
            {Check Waiter}
            if Hub.WaiterThread <> INVALID_HANDLE_VALUE then
             begin
              {$IFDEF USB_DEBUG}
              if USB_LOG_ENABLED then USBLogDebug(Request.Device,'Hub: Detachment pending, sending message to waiter thread (Thread=' + IntToHex(Hub.WaiterThread,8) + ')');
              {$ENDIF}
              
              {Send Message}
              FillChar(Message,SizeOf(TMessage),0);
              ThreadSendMessage(Hub.WaiterThread,Message);
              Hub.WaiterThread:=INVALID_HANDLE_VALUE;
             end; 
           end;
         end
        else if Request.Status <> USB_STATUS_SUCCESS then
         begin
          {Update Pending}
          Inc(Hub.PendingCount);
             
          {$IFDEF USB_DEBUG}
          if USB_LOG_ENABLED then USBLogDebug(Request.Device,'Hub: Resubmitting status change request');
          {$ENDIF}
 
          {Resubmit Status Change Request}
          Status:=USBRequestSubmit(Request);
          if Status <> USB_STATUS_SUCCESS then
           begin
            if USB_LOG_ENABLED then USBLogError(Request.Device,'Hub: Failed to resubmit status change request: ' + USBStatusToString(Status));
    
            {Update Pending}
            Dec(Hub.PendingCount);
           end;
         end;
       finally
        {Release the Lock}
        MutexUnlock(Hub.Lock);
       end;
      end
     else
      begin
       if USB_LOG_ENABLED then USBLogError(nil,'Hub: Failed to acquire hub lock'); 
      end;
    end
   else
    begin
     if USB_LOG_ENABLED then USBLogError(nil,'Hub: Status change request hub invalid (Hub=' + IntToHex(LongWord(Hub),8) + ')'); 
    end;    
  end
 else
  begin
   if USB_LOG_ENABLED then USBLogError(nil,'Hub: Status change request invalid (Request=' + IntToHex(LongWord(Request),8) + ')'); 
  end;
end;

{==============================================================================}

function USBHubDriverBind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
{Bind the Hub driver to a USB device if it is suitable}
{Device: The USB device to attempt to bind to}
{Interrface: The USB interface to attempt to bind to (or nil for whole device)}
{Return: USB_STATUS_SUCCESS if completed, USB_STATUS_DEVICE_UNSUPPORTED if unsupported or another error code on failure}
var
 Hub:PUSBHub;
 Status:LongWord;
 HubInterface:PUSBInterface;
 StatusEndpoint:PUSBEndpointDescriptor;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;
 
 {$IFDEF USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'Hub: Attempting to bind USB device (' + ': Address ' + IntToStr(Device.Address) + ')'); //To Do //Device.Manufacturer //Device.Product
 {$ENDIF}
 
 {Check Interface (Bind to device only)}
 if Interrface <> nil then
  begin
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Check for Hub (Must be device class)}
 if (Device.Descriptor.bDeviceClass <> USB_CLASS_CODE_HUB) or (Device.Configuration.Descriptor.bNumInterfaces <> 1) then
  begin
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 
 {Check Interfaces (Must have only 1 interface)}
 HubInterface:=USBDeviceFindInterfaceByIndex(Device,0);
 if HubInterface = nil then
  begin
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
  
 {Check Endpoints (Must have only 1 endpoint)}
 if HubInterface.Descriptor.bNumEndpoints <> 1 then
  begin
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
  
 {Check Endpoint (Must be IN interrupt)}
 StatusEndpoint:=USBDeviceFindEndpointByType(Device,HubInterface,USB_DIRECTION_IN,USB_TRANSFER_TYPE_INTERRUPT);
 if StatusEndpoint = nil then
  begin
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 
 {Check Configuration}
 if Device.ConfigurationValue = 0 then
  begin
   {$IFDEF USB_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Device,'Assigning configuration ' + IntToStr(Device.Configuration.Descriptor.bConfigurationValue) + ' (' + IntToStr(Device.Configuration.Descriptor.bNumInterfaces) + ' interfaces available)');
   {$ENDIF}

   {Set Configuration}
   Status:=USBDeviceSetConfiguration(Device,Device.Configuration.Descriptor.bConfigurationValue);
   if Status <> USB_STATUS_SUCCESS then
    begin
     if USB_LOG_ENABLED then USBLogError(Device,'Failed to set device configuration: ' + USBStatusToString(Status));
     
     {Return Result}
     Result:=Status;
     Exit;
    end;
  end;
  
 {Allocate Hub}
 Hub:=USBHubAllocate(Device);
 if Hub = nil then 
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Hub: Failed to allocate new USB hub');
   
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 
 {Assign Endpoint}
 Hub.StatusEndpoint:=StatusEndpoint;
 if Hub.StatusEndpoint = nil then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Hub: Invalid interrupt IN endpoint');
   
   {Release Hub}
   USBHubRelease(Hub);
   
   {Return Result}
   Result:=USB_STATUS_OPERATION_FAILED;
   Exit;
  end;
  
 {Create Hub Descriptor}
 Status:=USBHubCreateHubDescriptor(Hub);
 if Status <> USB_STATUS_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Hub: Failed to create USB hub descriptor');
   
   {Release Hub}
   USBHubRelease(Hub);
   
   {Return Result}
   Result:=Status;
   Exit;
  end;
 
 {Read Hub Descriptor}
 Status:=USBHubReadHubDescriptor(Hub);
 if Status <> USB_STATUS_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Hub: Failed to read USB hub descriptor');
   
   {Release Hub}
   USBHubRelease(Hub);
   
   {Return Result}
   Result:=Status;
   Exit;
  end;
 
 {$IFDEF USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'Hub: Attaching ' + USBHubCharacteristicsToString(Hub.Descriptor.wHubCharacteristics) + ' USB hub with ' + IntToStr(Hub.Descriptor.bNbrPorts) + ' ports');
 {$ENDIF}
 
 {Create Hub Ports}
 Status:=USBHubCreatePorts(Hub);
 if Status <> USB_STATUS_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Hub: Failed to create USB hub ports');
   
   {Release Hub}
   USBHubRelease(Hub);
   
   {Return Result}
   Result:=Status;
   Exit;
  end;
 
 {Power On Hub Ports}
 Status:=USBHubPowerOnPorts(Hub);
 if Status <> USB_STATUS_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Hub: Failed to power on USB hub ports');
   
   {Release Hub}
   USBHubRelease(Hub);
   
   {Return Result}
   Result:=Status;
   Exit;
  end;

 {Allocate Status Data}
 Hub.StatusData:=USBBufferAllocate(Device,SizeOf(TUSBHubData));
 if Hub.StatusData = nil then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Hub: Failed to create USB hub status change buffer');

   {Release Hub}
   USBHubRelease(Hub);
   
   {Return Result}
   Result:=USB_STATUS_OPERATION_FAILED;
   Exit;
  end;
 
 {Allocate Status Request}
 Hub.StatusRequest:=USBRequestAllocateEx(Device,Hub.StatusEndpoint,USBHubStatusComplete,Hub.StatusData,SizeOf(TUSBHubData),Hub);
 if Hub.StatusRequest = nil then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Hub: Failed to create USB hub status change request');
  
   {Release Status Data}
   USBBufferRelease(Hub.StatusData);
   
   {Release Hub}
   USBHubRelease(Hub);
   
   {Return Result}
   Result:=USB_STATUS_OPERATION_FAILED;
   Exit;
  end;
 
 {Update Device}
 Device.DriverData:=Hub;
  
 {Update Pending}
 Inc(Hub.PendingCount);
 
 {$IFDEF USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'Hub: Submitting status change request');
 {$ENDIF}

 {Submit Status Request}
 Status:=USBRequestSubmit(Hub.StatusRequest);
 if Status <> USB_STATUS_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Hub: Failed to submit status change request: ' + USBStatusToString(Status));
   
   {Update Pending}
   Dec(Hub.PendingCount);
  
   {Release Status Request}
   USBRequestRelease(Hub.StatusRequest);

   {Release Status Data}
   USBBufferRelease(Hub.StatusData);
   
   {Release Hub}
   USBHubRelease(Hub);
   
   {Return Result}
   Result:=Status;
   Exit;
  end;
 
 {Set State to Attached}
 if USBHubSetState(Hub,USBHUB_STATE_ATTACHED) <> ERROR_SUCCESS then Exit;
 
 {Return Result}
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function USBHubDriverUnbind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
{Unbind the Hub driver from a USB device}
{Device: The USB device to unbind from}
{Interrface: The USB interface to unbind from (or nil for whole device)}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var
 Hub:PUSBHub;
 Count:LongWord;
 Status:LongWord;
 Message:TMessage;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;

 {Check Interface}
 if Interrface <> nil then Exit;
 
 {Check Driver}
 if Device.Driver <> USBHubDriver then Exit;
 
 {$IFDEF USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'Hub: Unbinding (' + ': Address ' + IntToStr(Device.Address) + ')'); //To Do //Device.Manufacturer //Device.Product
 {$ENDIF}
 
 {Get Hub}
 Hub:=PUSBHub(Device.DriverData);
 if Hub = nil then Exit;
 if Hub.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Set State to Detaching}
 Result:=USB_STATUS_OPERATION_FAILED;
 if USBHubSetState(Hub,USBHUB_STATE_DETACHING) <> ERROR_SUCCESS then Exit;
 
 {Acquire the Lock}
 if MutexLock(Hub.Lock) = ERROR_SUCCESS then
  begin
   try
    {Cancel Status Request}
    USBRequestCancel(Hub.StatusRequest);
   
    {Check Pending}
    if Hub.PendingCount <> 0 then
     begin
      {$IFDEF USB_DEBUG}
      if USB_LOG_ENABLED then USBLogDebug(Device,'Hub: Waiting for ' + IntToStr(Hub.PendingCount) + ' pending requests to complete');
      {$ENDIF}
      
      {Wait for Pending}
      
      {Setup Waiter}
      Hub.WaiterThread:=GetCurrentThreadId;
   
      {Release the Lock}
      MutexUnlock(Hub.Lock);
   
      {Wait for Message}
      ThreadReceiveMessage(Message); 
      
      {Acquire the Lock}
      if MutexLock(Hub.Lock) <> ERROR_SUCCESS then Exit;
     end;
 
    {Detach Devices}
    for Count:=0 to Hub.Descriptor.bNbrPorts - 1 do
     begin
      if Hub.Ports[Count].Child <> nil then
       begin
        if USB_LOG_ENABLED then USBLogInfo(Device,'Hub: Detaching ' + USBDeviceToString(Hub.Ports[Count].Child));
        
        {Unbind Child}
        Status:=USBDeviceUnbind(Hub.Ports[Count].Child,nil);
        if (Status <> USB_STATUS_SUCCESS) and (Status <> USB_STATUS_NOT_BOUND)  then
         begin
          if USB_LOG_ENABLED then USBLogError(Device,'Hub: Failed to unbind device from port ' + IntToStr(Hub.Ports[Count].Number) + ': ' + USBStatusToString(Status));
          {Do not exit}
         end;
        
        {Detach Child}
        Status:=USBDeviceDetach(Hub.Ports[Count].Child);
        if Status <> USB_STATUS_SUCCESS then
         begin
          if USB_LOG_ENABLED then USBLogError(Device,'Hub: Failed to detach device from port ' + IntToStr(Hub.Ports[Count].Number) + ': ' + USBStatusToString(Status));
          {Do not exit}
         end;
        
        {Release Child}
        USBDeviceRelease(Hub.Ports[Count].Child);
        
        {Update Port}
        Hub.Ports[Count].Child:=nil;
       end;
     end;
   finally
    {Release the Lock}
    MutexUnlock(Hub.Lock);
   end;
   
   {Set State to Detached}
   if USBHubSetState(Hub,USBHUB_STATE_DETACHED) <> ERROR_SUCCESS then Exit;

   {Update Device}
   Device.DriverData:=nil;
 
   {Release Status Request}
   USBRequestRelease(Hub.StatusRequest);
 
   {Release Status Data}
   USBBufferRelease(Hub.StatusData);
   
   {Release Hub}
   USBHubRelease(Hub);
 
   {Return Result}
   Result:=USB_STATUS_SUCCESS;
  end; 
end;

{==============================================================================}
{==============================================================================}
{USB Device, Driver and Host Helper Functions}
function USBDeviceGetCount:LongWord; inline;
{Get the current device count}
begin
 {}
 Result:=USBDeviceTableCount;
end;

{==============================================================================}

function USBDeviceCheck(Device:PUSBDevice):PUSBDevice;
{Check if the supplied Device is in the device table}
var
 Current:PUSBDevice;
begin
 {}
 Result:=nil;
 
 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(USBDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Device}
    Current:=USBDeviceTable;
    while Current <> nil do
     begin
      {Check Device}
      if Current = Device then
       begin
        Result:=Device;
        Exit;
       end;
      
      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(USBDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

function USBDriverGetCount:LongWord; inline;
{Get the current driver count}
begin
 {}
 Result:=USBDriverTableCount;
end;

{==============================================================================}

function USBDriverCheck(Driver:PUSBDriver):PUSBDriver;
{Check if the supplied Driver is in the driver table}
var
 Current:PUSBDriver;
begin
 {}
 Result:=nil;
 
 {Check Driver}
 if Driver = nil then Exit;
 if Driver.Driver.Signature <> DRIVER_SIGNATURE then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(USBDriverTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Driver}
    Current:=USBDriverTable;
    while Current <> nil do
     begin
      {Check Driver}
      if Current = Driver then
       begin
        Result:=Driver;
        Exit;
       end;
      
      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(USBDriverTableLock);
   end;
  end;
end;

{==============================================================================}

function USBHostGetCount:LongWord; inline;
{Get the current host count}
begin
 {}
 Result:=USBHostTableCount;
end;

{==============================================================================}

function USBHostCheck(Host:PUSBHost):PUSBHost;
{Check if the supplied Host is in the host table}
var
 Current:PUSBHost;
begin
 {}
 Result:=nil;
 
 {Check Host}
 if Host = nil then Exit;
 if Host.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(USBHostTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Host}
    Current:=USBHostTable;
    while Current <> nil do
     begin
      {Check Host}
      if Current = Host then
       begin
        Result:=Host;
        Exit;
       end;
      
      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(USBHostTableLock);
   end;
  end;
end;

{==============================================================================}

function USBIsHub(Device:PUSBDevice):Boolean;
{Returns True if Device is a Hub or False if not}
begin
 {}
 Result:=False;
 
 {Check Device}
 if Device = nil then Exit;

 {Check Device Class}
 Result:=(Device.Descriptor.bDeviceClass = USB_CLASS_CODE_HUB);
end;

{==============================================================================}

function USBIsRootHub(Device:PUSBDevice):Boolean; 
{Returns True if Device is a Root Hub or False if not}
begin
 {}
 Result:=False;
 
 {Check Device}
 if Device = nil then Exit;
 
 {Check Parent}
 Result:=(Device.Parent = nil);
end;

{==============================================================================}

function USBIsControlRequest(Request:PUSBRequest):Boolean;     
{Returns True if Request is a control request or False if not}
begin
 {}
 Result:=False;
 
 {Check Request}
 if Request = nil then Exit;
 
 {Check Endpoint and Transfer Type}
 Result:=(Request.Endpoint = nil) or ((Request.Endpoint.bmAttributes and USB_TRANSFER_TYPE_MASK) = USB_TRANSFER_TYPE_CONTROL);
end;

{==============================================================================}

function USBIsBulkRequest(Request:PUSBRequest):Boolean;     
{Returns True if Request is a bulk request or False if not}
begin
 {}
 Result:=False;
 
 {Check Request}
 if Request = nil then Exit;
 
 {Check Endpoint and Transfer Type}
 Result:=(Request.Endpoint <> nil) and ((Request.Endpoint.bmAttributes and USB_TRANSFER_TYPE_MASK) = USB_TRANSFER_TYPE_BULK);
end;

{==============================================================================}

function USBIsInterruptRequest(Request:PUSBRequest):Boolean;   
{Returns True if Request is an interrupt request or False if not}
begin
 {}
 Result:=False;
 
 {Check Request}
 if Request = nil then Exit;
 
 {Check Endpoint and Transfer Type}
 Result:=(Request.Endpoint <> nil) and ((Request.Endpoint.bmAttributes and USB_TRANSFER_TYPE_MASK) = USB_TRANSFER_TYPE_INTERRUPT);
end;

{==============================================================================}

function USBIsIsochronousRequest(Request:PUSBRequest):Boolean;     
{Returns True if Request is an isochronous request or False if not}
begin
 {}
 Result:=False;
 
 {Check Request}
 if Request = nil then Exit;
 
 {Check Endpoint and Transfer Type}
 Result:=(Request.Endpoint <> nil) and ((Request.Endpoint.bmAttributes and USB_TRANSFER_TYPE_MASK) = USB_TRANSFER_TYPE_ISOCHRONOUS);
end;

{==============================================================================}

function USBDeviceToString(Device:PUSBDevice):String;
{Return a description of a USB device}
{Device: USB device to get a description of}
{Return: A string describing the device}
var
 Count:LongWord;
 ClassCode:Byte;
begin
 {}
 Result:='';
 
 {Check Device}
 if Device = nil then Exit;
 
 {Add the speed and USB version}
 Result:=USBSpeedToString(Device.Speed) + '-speed USB ' + USBBCDVersionToString(Device.Descriptor.bcdUSB);
 
 {Get the class code of the device from the device descriptor or an interface descriptor}
 ClassCode:=Device.Descriptor.bDeviceClass;
 if ClassCode = USB_CLASS_CODE_INTERFACE_SPECIFIC then
  begin
   for Count:=0 to Device.Configuration.Descriptor.bNumInterfaces - 1 do
    begin
     if Device.Configuration.Interfaces[Count].Descriptor.bInterfaceClass <> USB_CLASS_CODE_INTERFACE_SPECIFIC then
      begin
       ClassCode:=Device.Configuration.Interfaces[Count].Descriptor.bInterfaceClass;
      end;
    end;
  end;
  
 {Add the class description}
 if (ClassCode <> USB_CLASS_CODE_INTERFACE_SPECIFIC) and (ClassCode <> USB_CLASS_CODE_VENDOR_SPECIFIC) and (ClassCode <> USB_CLASS_CODE_MISCELLANEOUS) then
  begin
   Result:=Result + ' ' + USBClassCodeToString(ClassCode) + ' class';
  end;

 {Add device}
 Result:=Result + ' device';
 
 {Add the product name} 
 if Device.Product[0] <> #0 then
  begin
   Result:=Result + ' (' + Device.Product + ')';
  end;
 
 {Add vendor and product ID}
 Result:=Result + ' (idVendor=' + IntToHex(Device.Descriptor.idVendor,4) + ' idProduct=' + IntToHex(Device.Descriptor.idProduct,4) + ')';
end;

{==============================================================================}

function USBStatusToString(Status:LongWord):String;
{Translates a USB status code into a string}
begin
 {}
 Result:='USB_STATUS_UNKNOWN';
 
 case Status of
  USB_STATUS_SUCCESS:Result:='USB_STATUS_SUCCESS';
  USB_STATUS_DEVICE_DETACHED:Result:='USB_STATUS_DEVICE_DETACHED';
  USB_STATUS_DEVICE_UNSUPPORTED:Result:='USB_STATUS_DEVICE_UNSUPPORTED';
  USB_STATUS_HARDWARE_ERROR:Result:='USB_STATUS_HARDWARE_ERROR';
  USB_STATUS_INVALID_DATA:Result:='USB_STATUS_INVALID_DATA';
  USB_STATUS_INVALID_PARAMETER:Result:='USB_STATUS_INVALID_PARAMETER';
  USB_STATUS_NOT_PROCESSED:Result:='USB_STATUS_NOT_PROCESSED';
  USB_STATUS_OUT_OF_MEMORY:Result:='USB_STATUS_OUT_OF_MEMORY';
  USB_STATUS_TIMEOUT:Result:='USB_STATUS_TIMEOUT';
  USB_STATUS_UNSUPPORTED_REQUEST:Result:='USB_STATUS_UNSUPPORTED_REQUEST';
  USB_STATUS_HARDWARE_STALL:Result:='USB_STATUS_HARDWARE_STALL';
  USB_STATUS_OPERATION_FAILED:Result:='USB_STATUS_OPERATION_FAILED';
  USB_STATUS_NOT_BOUND:Result:='USB_STATUS_NOT_BOUND';
  USB_STATUS_ALREADY_BOUND:Result:='USB_STATUS_ALREADY_BOUND';
  USB_STATUS_NOT_READY:Result:='USB_STATUS_NOT_READY';
  USB_STATUS_NOT_COMPLETED:Result:='USB_STATUS_NOT_COMPLETED';
  USB_STATUS_CANCELLED:Result:='USB_STATUS_CANCELLED';
  USB_STATUS_NOT_VALID:Result:='USB_STATUS_NOT_VALID';
 end;
end;

{==============================================================================}

function USBClassCodeToString(ClassCode:Integer):String;
{Translates a USB class code into a string}
begin
 {}
 Result:='Unknown';
 
 case ClassCode of
  USB_CLASS_CODE_INTERFACE_SPECIFIC:Result:='None (see interface descriptors)';
  USB_CLASS_CODE_AUDIO:Result:='Audio';
  USB_CLASS_CODE_COMMUNICATIONS_AND_CDC_CONTROL:Result:='Communications and CDC Control';
  USB_CLASS_CODE_HID:Result:='HID (Human Interface Device)';
  USB_CLASS_CODE_IMAGE:Result:='Image';
  USB_CLASS_CODE_PRINTER:Result:='Printer';
  USB_CLASS_CODE_MASS_STORAGE:Result:='Mass Storage';
  USB_CLASS_CODE_HUB:Result:='Hub';
  USB_CLASS_CODE_VIDEO:Result:='Video';
  USB_CLASS_CODE_WIRELESS_CONTROLLER:Result:='Wireless Controller';
  USB_CLASS_CODE_MISCELLANEOUS:Result:='Miscellaneous';
  USB_CLASS_CODE_VENDOR_SPECIFIC:Result:='Vendor Specific';
 end;
end;

{==============================================================================}

function USBSubClassCodeToString(ClassCode,SubClassCode:Integer):String;
{Translates a USB sub class code into a string}
begin
 {}
 Result:='Unknown';
 
 case ClassCode of
  //To Do //More to do
  USB_CLASS_CODE_MASS_STORAGE:begin
    case SubClassCode of
     USB_SUBCLASS_MASS_STORAGE_DEFAULT:Result:='SCSI (Default)';     
     USB_SUBCLASS_MASS_STORAGE_RBC:Result:='RBC';               
     USB_SUBCLASS_MASS_STORAGE_MMC5:Result:='MMC-5';               
     USB_SUBCLASS_MASS_STORAGE_QIC157:Result:='QIC-157 (Obsolete)';   
     USB_SUBCLASS_MASS_STORAGE_UFI:Result:='UFI';               
     USB_SUBCLASS_MASS_STORAGE_SFF8070I:Result:='SFF-8070i (Obsolete)';  
     USB_SUBCLASS_MASS_STORAGE_SCSI:Result:='SCSI';              
     USB_SUBCLASS_MASS_STORAGE_LSDFS:Result:='LSD FS';               
     USB_SUBCLASS_MASS_STORAGE_IEEE1667:Result:='IEEE 1667';          
     USB_SUBCLASS_MASS_STORAGE_VENDOR_SPECIFIC:Result:='Vendor Specific';
    end;
   end;
  //To Do //More to do
 end;
end;
 
{==============================================================================}

function USBProtocolCodeToString(ClassCode,ProtocolCode:Integer):String;
{Translates a USB protocol code into a string}
begin
 {}
 Result:='Unknown';
 
 case ClassCode of
  //To Do //More to do
  USB_CLASS_CODE_MASS_STORAGE:begin
    case ProtocolCode of
     USB_PROTOCOL_MASS_STORAGE_CBI:Result:='CBI';    
     USB_PROTOCOL_MASS_STORAGE_CB:Result:='CB (No Interrupt)';   
     USB_PROTOCOL_MASS_STORAGE_BBB:Result:='BBB';            
     USB_PROTOCOL_MASS_STORAGE_UAS:Result:='UAS';           
     USB_PROTOCOL_MASS_STORAGE_VENDOR_SPECIFIC:Result:='Vendor Specific'; 
    end;
   end;
  //To Do //More to do
 end;
end;

{==============================================================================}

function USBSpeedToString(Speed:Integer):String;
{Translates a USB speed constant into a string}
begin
 {}
 Result:='unknown';
 
 case Speed of
  USB_SPEED_SUPERPLUS:Result:='superplus';
  USB_SPEED_SUPER:Result:='super';
  USB_SPEED_HIGH:Result:='high';
  USB_SPEED_FULL:Result:='full';
  USB_SPEED_LOW:Result:='low';
 end;
end;

{==============================================================================}

function USBTransferTypeToString(TransferType:Integer):String;
{Translates a USB transfer type constant into a string}
begin
 {}
 Result:='Unknown';

 case TransferType of
  USB_TRANSFER_TYPE_CONTROL:Result:='Control';
  USB_TRANSFER_TYPE_ISOCHRONOUS:Result:='Isochronous';
  USB_TRANSFER_TYPE_BULK:Result:='Bulk';
  USB_TRANSFER_TYPE_INTERRUPT:Result:='Interrupt';
 end;
end;

{==============================================================================}

function USBDirectionToString(Direction:Integer):String;
{Translates a USB direction constant into a string}
begin
 {}
 Result:='Unknown';
 
 case Direction of
  USB_DIRECTION_OUT:Result:='OUT';
  USB_DIRECTION_IN:Result:='IN';
 end;
end;

{==============================================================================}

function USBBCDVersionToString(BCDVersion:Word):String;
{Translates a bcdUSB (binary-coded-decimal USB version) value into a
 human-readable string}
{BCDVersion: The bcdUSB value (e.g. from a USB device descriptor) to translate}
{Return: String describing the USB version}
begin
 {}
 Result:=IntToStr((BCDVersion shr 8) and $FF) + '.' + IntToStr((BCDVersion shr 4) and $0F);
 
 if (BCDVersion and $0F) <> 0 then
  begin
   Result:=Result + '.' + IntToStr(BCDVersion and 31); 
  end;
end;

{==============================================================================}

function USBPortStatusConnectedToString(Status:Word):String;
begin
 {}
 Result:='disconnected';
 
 if (Status and USB_PORT_STATUS_CONNNECTED) = USB_PORT_STATUS_CONNNECTED then Result:='connected';
end;

{==============================================================================}

function USBHubCharacteristicsToString(HubCharacteristics:Word):String;
begin
 {}
 Result:='';
 
 if (HubCharacteristics and USB_HUB_CHARACTERISTIC_IS_COMPOUND_DEVICE) = USB_HUB_CHARACTERISTIC_IS_COMPOUND_DEVICE then Result:='compound device';
end;

{==============================================================================}

function USBDeviceTypeToString(USBType:LongWord):String;
begin
 {}
 Result:='USB_TYPE_UNKNOWN';
 
 if USBType <= USB_TYPE_MAX then
  begin
   Result:=USB_TYPE_NAMES[USBType];
  end;
end;

{==============================================================================}

function USBDeviceStateToString(USBState:LongWord):String;
begin
 {}
 Result:='USB_STATE_UNKNOWN';
 
 if USBState <= USB_STATE_MAX then
  begin
   Result:=USB_STATE_NAMES[USBState];
  end;
end;

{==============================================================================}

function USBDeviceStatusToString(USBStatus:LongWord):String;
begin
 {}
 Result:='USB_STATUS_UNKNOWN';
 
 if USBStatus <= USB_STATUS_MAX then
  begin
   Result:=USB_STATUS_NAMES[USBStatus];
  end;
end;

{==============================================================================}

function USBDeviceStateToNotification(State:LongWord):LongWord;
{Convert a Device state value into the notification code for device notifications}
begin
 {}
 Result:=DEVICE_NOTIFICATION_NONE;
 
 {Check State}
 case State of
  USB_STATE_DETACHED:Result:=DEVICE_NOTIFICATION_DETACH;
  USB_STATE_DETACHING:Result:=DEVICE_NOTIFICATION_DETACHING;
  USB_STATE_ATTACHING:Result:=DEVICE_NOTIFICATION_ATTACHING;
  USB_STATE_ATTACHED:Result:=DEVICE_NOTIFICATION_ATTACH;
 end;
end;

{==============================================================================}

function USBDeviceStatusToNotification(Status:LongWord):LongWord;
{Convert a Device status value into the notification code for device notifications}
begin
 {}
 Result:=DEVICE_NOTIFICATION_NONE;
 
 {Check Status}
 case Status of
  USB_STATUS_UNBOUND:Result:=DEVICE_NOTIFICATION_UNBIND;
  USB_STATUS_BOUND:Result:=DEVICE_NOTIFICATION_BIND;
 end;
end;

{==============================================================================}

function USBHostTypeToString(HostType:LongWord):String;
begin
 {}
 Result:='USBHOST_TYPE_UNKNOWN';
 
 if HostType <= USBHOST_TYPE_MAX then
  begin
   Result:=USBHOST_TYPE_NAMES[HostType];
  end;
end;

{==============================================================================}

function USBHostStateToString(HostState:LongWord):String;
begin
 {}
 Result:='USBHOST_STATE_UNKNOWN';
 
 if HostState <= USBHOST_STATE_MAX then
  begin
   Result:=USBHOST_STATE_NAMES[HostState];
  end;
end;

{==============================================================================}

function USBHostStateToNotification(State:LongWord):LongWord;
{Convert a Host state value into the notification code for device notifications}
begin
 {}
 Result:=DEVICE_NOTIFICATION_NONE;
 
 {Check State}
 case State of
  USBHOST_STATE_DISABLED:Result:=DEVICE_NOTIFICATION_DISABLE;
  USBHOST_STATE_ENABLED:Result:=DEVICE_NOTIFICATION_ENABLE;
 end;
end;

{==============================================================================}

procedure USBLog(Level:LongWord;Device:PUSBDevice;const AText:String);
var
 WorkBuffer:String;
begin
 {}
 {Check Level}
 if Level < USB_DEFAULT_LOG_LEVEL then Exit;
 
 WorkBuffer:='';
 {Check Level}
 if Level = USB_LOG_LEVEL_DEBUG then
  begin
   WorkBuffer:=WorkBuffer + '[DEBUG] ';
  end
 else if Level = USB_LOG_LEVEL_ERROR then
  begin
   WorkBuffer:=WorkBuffer + '[ERROR] ';
  end;
 
 {Add Prefix}
 WorkBuffer:=WorkBuffer + 'USB: ';
 
 {Check Device}
 if Device <> nil then
  begin
   WorkBuffer:=WorkBuffer + 'Device' + IntToStr(Device.Address) + ': ';
  end;

 {Output Logging}  
 LoggingOutputEx(LOGGING_FACILITY_USB,LogLevelToLoggingSeverity(Level),'USB',WorkBuffer + AText);
end;

{==============================================================================}

procedure USBLogInfo(Device:PUSBDevice;const AText:String);
begin
 {}
 USBLog(USB_LOG_LEVEL_INFO,Device,AText);
end;

{==============================================================================}

procedure USBLogError(Device:PUSBDevice;const AText:String);
begin
 {}
 USBLog(USB_LOG_LEVEL_ERROR,Device,AText);
end;

{==============================================================================}

procedure USBLogDebug(Device:PUSBDevice;const AText:String);
begin
 {}
 USBLog(USB_LOG_LEVEL_DEBUG,Device,AText);
end;

{==============================================================================}

procedure USBLogDeviceConfiguration(Device:PUSBDevice);
var
 Count:LongWord;
 Counter:LongWord;
begin
 {}
 {Check Device}
 if Device = nil then Exit;
 
 {Log Configuration Descriptor}
 USBLogConfigurationDescriptor(Device,Device.Configuration.Descriptor);
 {Log Interface Descriptors}
 for Count:=0 to Device.Configuration.Descriptor.bNumInterfaces - 1 do
  begin
   USBLogInterfaceDescriptor(Device,Device.Configuration.Interfaces[Count].Descriptor);
   {Log Endpoint Descriptors}
   for Counter:=0 to Device.Configuration.Interfaces[Count].Descriptor.bNumEndpoints - 1 do
    begin
     USBLogEndpointDescriptor(Device,Device.Configuration.Interfaces[Count].Endpoints[Counter]);
    end;
  end;
end;

{==============================================================================}

procedure USBLogDeviceDescriptor(Device:PUSBDevice;Descriptor:PUSBDeviceDescriptor);
begin
 {}
 {Check Device}
 if Device = nil then Exit;

 {Check Descriptor}
 if Descriptor = nil then Exit;
 
 LoggingOutput('    [Device Descriptor]');
 LoggingOutput('    bLength:             ' + IntToStr(Descriptor.bLength));
 LoggingOutput('    bDescriptorType:     ' + IntToHex(Descriptor.bDescriptorType,2) + ' (Device)');
 LoggingOutput('    bcdUSB:              ' + IntToHex(Descriptor.bcdUSB,2) + ' (USB ' + USBBCDVersionToString(Descriptor.bcdUSB) + ' compliant)');
 LoggingOutput('    bDeviceClass:        ' + IntToHex(Descriptor.bDeviceClass,2) + ' (' + USBClassCodeToString(Descriptor.bDeviceClass) + ')');
 LoggingOutput('    bDeviceSubClass:     ' + IntToHex(Descriptor.bDeviceSubClass,2) + ' (' + USBSubClassCodeToString(Descriptor.bDeviceClass,Descriptor.bDeviceSubClass) + ')');
 LoggingOutput('    bDeviceProtocol:     ' + IntToHex(Descriptor.bDeviceProtocol,2) + ' (' + USBProtocolCodeToString(Descriptor.bDeviceClass,Descriptor.bDeviceProtocol) + ')');
 LoggingOutput('    bMaxPacketSize0:     ' + IntToStr(Descriptor.bMaxPacketSize0));
 LoggingOutput('    idVendor:            ' + IntToHex(Descriptor.idVendor,4));
 LoggingOutput('    idProduct:           ' + IntToHex(Descriptor.idProduct,4));
 LoggingOutput('    iManufacturer:       ' + IntToStr(Descriptor.iManufacturer));
 if Device.Manufacturer[0] <> #0 then
  begin
   LoggingOutput('        (' + Device.Manufacturer + ')');
  end;
 LoggingOutput('    iProduct:            ' + IntToStr(Descriptor.iProduct));
 if Device.Product[0] <> #0 then
  begin
   LoggingOutput('        (' + Device.Product + ')');
  end;
 LoggingOutput('    iSerialNumber:       ' + IntToStr(Descriptor.iSerialNumber));
 LoggingOutput('    bNumConfigurations:  ' + IntToStr(Descriptor.bNumConfigurations));
 LoggingOutput('');
end;

{==============================================================================}

procedure USBLogConfigurationDescriptor(Device:PUSBDevice;Descriptor:PUSBConfigurationDescriptor);
begin
 {}
 {Check Device}
 if Device = nil then Exit;
 
 {Check Descriptor}
 if Descriptor = nil then Exit;
 
 LoggingOutput('        [Configuration Descriptor]');
 LoggingOutput('        bLength:             ' + IntToStr(Descriptor.bLength));
 LoggingOutput('        bDescriptorType:     ' + IntToHex(Descriptor.bDescriptorType,2) + ' (Configuration)');
 LoggingOutput('        wTotalLength:        ' + IntToStr(Descriptor.wTotalLength));
 LoggingOutput('        bNumInterfaces:      ' + IntToStr(Descriptor.bNumInterfaces));
 LoggingOutput('        bConfigurationValue: ' + IntToStr(Descriptor.bConfigurationValue));
 LoggingOutput('        iConfiguration:      ' + IntToStr(Descriptor.iConfiguration));
 LoggingOutput('        bmAttributes:        ' + IntToHex(Descriptor.bmAttributes,2));
 if (Descriptor.bmAttributes and USB_CONFIGURATION_ATTRIBUTE_SELF_POWERED) = USB_CONFIGURATION_ATTRIBUTE_SELF_POWERED then
  begin
   LoggingOutput('            (Self powered)');
  end;
 if (Descriptor.bmAttributes and USB_CONFIGURATION_ATTRIBUTE_REMOTE_WAKEUP) = USB_CONFIGURATION_ATTRIBUTE_REMOTE_WAKEUP then
  begin
   LoggingOutput('            (Remote wakeup)');
  end;
 LoggingOutput('        bMaxPower:           ' + IntToStr(Descriptor.bMaxPower) + ' (' + IntToStr(Descriptor.bMaxPower * 2) + ' mA)');
 LoggingOutput('');
end;

{==============================================================================}

procedure USBLogInterfaceDescriptor(Device:PUSBDevice;Descriptor:PUSBInterfaceDescriptor);
begin
 {}
 {Check Device}
 if Device = nil then Exit;
 
 {Check Descriptor}
 if Descriptor = nil then Exit;
 
 LoggingOutput('            [Interface Descriptor]');
 LoggingOutput('            bLength:             ' + IntToStr(Descriptor.bLength));
 LoggingOutput('            bDescriptorType:     ' + IntToHex(Descriptor.bDescriptorType,2) + ' (Interface)');
 LoggingOutput('            bInterfaceNumber:    ' + IntToStr(Descriptor.bInterfaceNumber));
 LoggingOutput('            bAlternateSetting:   ' + IntToStr(Descriptor.bAlternateSetting));
 LoggingOutput('            bNumEndpoints:       ' + IntToStr(Descriptor.bNumEndpoints));
 LoggingOutput('            bInterfaceClass:     ' + IntToHex(Descriptor.bInterfaceClass,2) + ' (' + USBClassCodeToString(Descriptor.bInterfaceClass) + ')');
 LoggingOutput('            bInterfaceSubClass:  ' + IntToHex(Descriptor.bInterfaceSubClass,2) + ' (' + USBSubClassCodeToString(Descriptor.bInterfaceClass,Descriptor.bInterfaceSubClass) + ')');
 LoggingOutput('            bInterfaceProtocol:  ' + IntToHex(Descriptor.bInterfaceProtocol,2) + ' (' + USBProtocolCodeToString(Descriptor.bInterfaceClass,Descriptor.bInterfaceProtocol) + ')');
 LoggingOutput('            iInterface:          ' + IntToStr(Descriptor.iInterface));
 LoggingOutput('');
end;

{==============================================================================}

procedure USBLogEndpointDescriptor(Device:PUSBDevice;Descriptor:PUSBEndpointDescriptor);
begin
 {}
 {Check Device}
 if Device = nil then Exit;
 
 {Check Descriptor}
 if Descriptor = nil then Exit;
 
 LoggingOutput('                [Endpoint Descriptor]');
 LoggingOutput('                bLength:             ' + IntToStr(Descriptor.bLength));
 LoggingOutput('                bDescriptorType:     ' + IntToHex(Descriptor.bDescriptorType,2) + ' (Endpoint)');
 LoggingOutput('                bEndpointAddress:    ' + IntToHex(Descriptor.bEndpointAddress,2) + ' (Number ' + IntToStr(Descriptor.bEndpointAddress and $0F) + ', ' + USBDirectionToString(Descriptor.bEndpointAddress shr 7) + ')');
 LoggingOutput('                bmAttributes:        ' + IntToHex(Descriptor.bmAttributes,2) + ' (' + USBTransferTypeToString(Descriptor.bmAttributes and $03) + ' endpoint)');
 LoggingOutput('                wMaxPacketSize:      ' + IntToHex(Descriptor.wMaxPacketSize,4) + ' (Max packet size ' + IntToStr(Descriptor.wMaxPacketSize and $7FF) + ' bytes)');
 LoggingOutput('                bInterval:           ' + IntToStr(Descriptor.bInterval));
 LoggingOutput('');
end;

{==============================================================================}

function USBLogDevices:LongWord;
{Print information about all devices attached to the USB}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Host:PUSBHost;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Started}
 if USBStarted then
  begin
   {Acquire the Lock}
   if CriticalSectionLock(USBHostTableLock) = ERROR_SUCCESS then
    begin
     try
      {Get Host}
      Host:=USBHostTable;
      while Host <> nil do
       begin
        {Check Root Hub}
        if Host.RootHub <> nil then
         begin
          {Enumerate USB Devices (Text Output)}
          USBHubEnumerateDevices(Host.RootHub,USBLogDeviceCallback,nil);

          LoggingOutput('');
          LoggingOutput('Diagram of USB:');
          LoggingOutput('');

          {Enumerate USB Devices (Tree Output)}
          USBHubEnumerateDevices(Host.RootHub,USBLogTreeCallback,nil);
         end;
 
        {Get Next}
        Host:=Host.Next;
       end;
   
      {Return Result}
      Result:=ERROR_SUCCESS;
     finally
      {Release the Lock}
      CriticalSectionUnlock(USBHostTableLock);
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;  
  end
 else
  begin
   LoggingOutput('USB subsystem not initialized');
   
   Result:=ERROR_NOT_READY;
  end; 
end;

{==============================================================================}

function USBLogDeviceCallback(Device:PUSBDevice;Data:Pointer):LongWord;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;
 
 LoggingOutput('[USB Device ' + IntToStr(Device.Address) + ']');
 
 USBLogDeviceDescriptor(Device,@Device.Descriptor);
 USBLogDeviceConfiguration(Device);

 LoggingOutput('');
 
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function USBLogTreeCallback(Device:PUSBDevice;Data:Pointer):LongWord;
var
 Count:Integer;
 LinesCount:Integer;
 SpacesCount:Integer;
 WorkBuffer:String;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;
 
 {Output Diagram}
 WorkBuffer:='';
 if Device.Depth <> 0 then
  begin
   SpacesCount:=(Device.Depth - 1) * USB_TREE_SPACES_PER_LEVEL;
   
   for LinesCount:=0 to USB_TREE_LINES_PER_PORT - 1 do
    begin
     WorkBuffer:='';
     for Count:=0 to SpacesCount - 1 do
      begin
       WorkBuffer:=WorkBuffer + ' ';
      end; 
     WorkBuffer:=WorkBuffer + '|';
     LoggingOutput(WorkBuffer);
    end;
   
   WorkBuffer:='';
   for Count:=0 to SpacesCount - 1 do
    begin
     WorkBuffer:=WorkBuffer + ' ';
    end;
   
   for Count:=0 to USB_TREE_SPACES_PER_LEVEL - 1 do
    begin
     WorkBuffer:=WorkBuffer + '-';
    end;    
  end;

 {Output Device}
 WorkBuffer:=WorkBuffer + IntToStr(Device.Address) + ' [' + USBDeviceToString(Device) + ']';
 LoggingOutput(WorkBuffer);
 
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}
{==============================================================================}
{USB Hub Helper Functions}
function USBHubGetCount:LongWord; inline;
{Get the current hub count}
begin
 {}
 Result:=USBHubTableCount;
end;

{==============================================================================}

function USBHubCheck(Hub:PUSBHub):PUSBHub;
{Check if the supplied Hub is in the hub table}
var
 Current:PUSBHub;
begin
 {}
 Result:=nil;
 
 {Check Hub}
 if Hub = nil then Exit;
 if Hub.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(USBHubTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Hub}
    Current:=USBHubTable;
    while Current <> nil do
     begin
      {Check Hub}
      if Current = Hub then
       begin
        Result:=Hub;
        Exit;
       end;
      
      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(USBHubTableLock);
   end;
  end;
end;

{==============================================================================}

function USBHubStateToNotification(State:LongWord):LongWord;
{Convert a Hub state value into the notification code for device notifications}
begin
 {}
 Result:=DEVICE_NOTIFICATION_NONE;
 
 {Check State}
 case State of
  USBHUB_STATE_DETACHED:Result:=DEVICE_NOTIFICATION_DETACH;
  USBHUB_STATE_DETACHING:Result:=DEVICE_NOTIFICATION_DETACHING;
  USBHUB_STATE_ATTACHING:Result:=DEVICE_NOTIFICATION_ATTACHING;
  USBHUB_STATE_ATTACHED:Result:=DEVICE_NOTIFICATION_ATTACH;
 end;
end;

{==============================================================================}
{==============================================================================}

initialization
 USBInit;
 if USB_AUTOSTART then
  begin
   if not USB_ASYNCSTART then
    begin
     {Start USB}
     USBStart;
    end
   else
    begin
     {Schedule Worker}
     WorkerSchedule(0,TWorkerTask(USBAsyncStart),nil,nil);
    end;
  end; 

{==============================================================================}
 
finalization
 USBStop;
 
{==============================================================================}
{==============================================================================}

end.
