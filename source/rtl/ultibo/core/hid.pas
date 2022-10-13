{
Ultibo Human Interface Device (HID) interface unit.

Copyright (C) 2022 - SoftOz Pty Ltd.

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

  HID - https://en.wikipedia.org/wiki/Human_interface_device


HID
===

 The Human Interface Device (HID) class is intended to provide a flexible model
 that allows a wide range of control types to be expressed using a standard set
 of tags in a report descriptor that each device provides during initialization.

 HID devices can represent common items such as mice, keyboards, touchscreens,
 gamepads and joysticks but can also appear as controls within many other types
 of devices.

 A headset for example will primarily be an audio device but the volume and mute
 buttons can be defined using the HID standard and easily recognized by software
 without requiring a custom driver for each and every device.

 The Ultibo HID implementation creates an intermediate device layer that is mostly
 agnostic to both the bus type being used by an underlying provider (such as USB)
 and the devices recognised by HID consumers such as mice and keyboards.

 A provider such as USB HID locates devices from the provider specific bus and
 creates HID devices to represent them along with obtaining report descriptors
 and other information.

 These HID devices are then passed to registered HID consumers (drivers) to
 determine if they recognize the collections, reports and usages described by
 the device. A consumer can accept a device and bind to it during this process
 and create its own devices to represent the functionality described by the
 HID device.

 While HID itself is intended to be bus agnostic this implementation is based
 heavily on the USB HID standards as those are the most widely adopted at this
 time. It is anticipated that HID adoption will expand over time to include a
 range other bus types, Bluetooth is already using HID and there are existing
 implementations of HID over I2C, SPI and PCI.

 Expanding the Ultibo HID support to other bus types simply requires a new HID
 provider for that bus to be written along with any necessary changes or
 extensions to the HID layer itself.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit HID;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {HID specific constants}
 HID_NAME_PREFIX = 'HID';  {Name prefix for HID Devices}

 {HID Device Types}
 HID_TYPE_NONE      = 0;
 HID_TYPE_USB       = 1;

 HID_TYPE_MAX       = 1;

 {HID Type Names}
 HID_TYPE_NAMES:array[HID_TYPE_NONE..HID_TYPE_MAX] of String = (
  'HID_TYPE_NONE',
  'HID_TYPE_USB');

 {HID Device States}
 HID_STATE_DETACHED  = 0;
 HID_STATE_DETACHING = 1;
 HID_STATE_ATTACHING = 2;
 HID_STATE_ATTACHED  = 3;

 HID_STATE_MAX       = 3;

 {HID Device State Names}
 HID_STATE_NAMES:array[HID_STATE_DETACHED..HID_STATE_MAX] of String = (
  'HID_STATE_DETACHED',
  'HID_STATE_DETACHING',
  'HID_STATE_ATTACHING',
  'HID_STATE_ATTACHED');

 {HID Device Flags}
 HID_FLAG_NONE         = $00000000;

 {HID Interface Subclass types (See USB HID v1.11 specification)(Section 4.2)}
 HID_SUBCLASS_NONE           = 0;
 HID_SUBCLASS_BOOT           = 1;

 {HID Interface Protocol types (See USB HID v1.11 specification)(Section 4.3)}
 HID_BOOT_PROTOCOL_NONE      = 0;
 HID_BOOT_PROTOCOL_KEYBOARD  = 1;
 HID_BOOT_PROTOCOL_MOUSE     = 2;

 {HID Class Descriptor Types (See USB HID v1.11 specification)(Section 7.1)}
 HID_DESCRIPTOR_TYPE_HID                  = $21;
 HID_DESCRIPTOR_TYPE_REPORT               = $22;
 HID_DESCRIPTOR_TYPE_PHYSICAL_DESCRIPTOR  = $23;

 {HID Request types (Section 7.2)}
 HID_REQUEST_GET_REPORT      = $01;
 HID_REQUEST_GET_IDLE        = $02;
 HID_REQUEST_GET_PROTOCOL    = $03;
 HID_REQUEST_SET_REPORT      = $09;
 HID_REQUEST_SET_IDLE        = $0A;
 HID_REQUEST_SET_PROTOCOL    = $0B;

 {HID Protocol types (Section 7.2.5)}
 HID_PROTOCOL_BOOT           = 0;
 HID_PROTOCOL_REPORT         = 1;

 {HID Report types (Section 7.2.1)}
 HID_REPORT_INPUT            = 1;
 HID_REPORT_OUTPUT           = 2;
 HID_REPORT_FEATURE          = 3;

 {HID Report IDs (Section 7.2.1)}
 HID_REPORTID_NONE           = 0;

 {HID Item Sizes (Section 6.2.2.2)}
 HID_SIZE_0                  = (0 shl 0);
 HID_SIZE_1                  = (1 shl 0);
 HID_SIZE_2                  = (2 shl 0);
 HID_SIZE_4                  = (3 shl 0);
 HID_SIZE_MASK               = $03;

 {HID Item Types (Section 6.2.2.2)}
 HID_TYPE_MAIN               = (0 shl 2);
 HID_TYPE_GLOBAL             = (1 shl 2);
 HID_TYPE_LOCAL              = (2 shl 2);
 HID_TYPE_RESERVED           = (3 shl 2);
 HID_TYPE_MASK               = $0C;

 {HID Item Tags (Section 6.2.2.1)}
 {HID Main Item Tags (Section 6.2.2.4)}
 HID_TAG_MAIN_INPUT               = $80; {Input}
 HID_TAG_MAIN_OUTPUT              = $90; {Output}
 HID_TAG_MAIN_FEATURE             = $B0; {Feature}
 HID_TAG_MAIN_COLLECTION          = $A0; {Collection}
 HID_TAG_MAIN_END_COLLECTION      = $C0; {End Collection}

 {HID Global Item Tags (Section 6.2.2.7)}
 HID_TAG_GLOBAL_USAGE_PAGE        = $04; {Usage Page}
 HID_TAG_GLOBAL_LOGICAL_MINIMUM   = $14; {Logical Minimum}
 HID_TAG_GLOBAL_LOGICAL_MAXIMUM   = $24; {Logical Maximum}
 HID_TAG_GLOBAL_PHYSICAL_MINIMUM  = $34; {Physical Minimum}
 HID_TAG_GLOBAL_PHYSICAL_MAXIMUM  = $44; {Physical Maximum}
 HID_TAG_GLOBAL_UNIT_EXPONENT     = $54; {Unit Exponent}
 HID_TAG_GLOBAL_UNIT              = $64; {Unit}
 HID_TAG_GLOBAL_REPORT_SIZE       = $74; {Report Size}
 HID_TAG_GLOBAL_REPORT_ID         = $84; {Report ID}
 HID_TAG_GLOBAL_REPORT_COUNT      = $94; {Report Count}
 HID_TAG_GLOBAL_PUSH              = $A4; {Push}
 HID_TAG_GLOBAL_POP               = $B4; {Pop}

 {HID Local Item Tags (Section 6.2.2.8)}
 HID_TAG_LOCAL_USAGE              = $08; {Usage}
 HID_TAG_LOCAL_USAGE_MINIMUM      = $18; {Usage Minimum}
 HID_TAG_LOCAL_USAGE_MAXIMUM      = $28; {Usage Maximum}
 HID_TAG_LOCAL_DESIGNATOR_INDEX   = $38; {Designator Index}
 HID_TAG_LOCAL_DESIGNATOR_MINIMUM = $48; {Designator Minimum}
 HID_TAG_LOCAL_DESIGNATOR_MAXIMUM = $58; {Designator Maximum}
 HID_TAG_LOCAL_STRING_INDEX       = $78; {String Index}
 HID_TAG_LOCAL_STRING_MINIMUM     = $88; {String Minimum}
 HID_TAG_LOCAL_STRING_MAXIMUM     = $98; {String Maximum}
 HID_TAG_LOCAL_DELIMITER          = $A8; {Delimiter}

 HID_TAG_LONG                     = $FC; {Always HID_SIZE_2 (Followed by 1 byte DataSize / 1 byte LongItemTag / n bytes Data)}
 HID_TAG_MASK                     = $FC;

 {HID Main Item Input, Ouput and Feature Values (Section 6.2.2.5)}
 HID_MAIN_ITEM_CONSTANT       = (1 shl 0); {Data (0) | Constant (1)}
 HID_MAIN_ITEM_VARIABLE       = (1 shl 1); {Array (0) | Variable (1)}
 HID_MAIN_ITEM_RELATIVE       = (1 shl 2); {Absolute (0) | Relative (1)}
 HID_MAIN_ITEM_WRAP           = (1 shl 3); {No Wrap (0) | Wrap (1)}
 HID_MAIN_ITEM_NON_LINEAR     = (1 shl 4); {Linear (0) | Non Linear (1)}
 HID_MAIN_ITEM_NO_PREFERRED   = (1 shl 5); {Preferred State (0) | No Preferred (1)}
 HID_MAIN_ITEM_NULL_STATE     = (1 shl 6); {No Null position (0) | Null state(1)}
 HID_MAIN_ITEM_RESERVED1      = (1 shl 7); {Reserved (0)}
 HID_MAIN_ITEM_BUFFERED_BYTES = (1 shl 8); {Bit Field (0) | Buffered Bytes (1)}
 HID_MAIN_ITEM_RESERVED2      = ($FFFFFE00 shl 9); {Reserved (0)}

 {HID Main Item Collection Values (Section 6.2.2.6)}
 HID_MAIN_COLLECTION_PHYSICAL       = $00; {Physical (Group of axes)}
 HID_MAIN_COLLECTION_APPLICATION    = $01; {Application (Mouse, Keyboard)}
 HID_MAIN_COLLECTION_LOGICAL        = $02; {Logical (Interrelated data)}
 HID_MAIN_COLLECTION_REPORT         = $03; {Report}
 HID_MAIN_COLLECTION_NAMED_ARRAY    = $04; {Named Array}
 HID_MAIN_COLLECTION_USAGE_SWITCH   = $05; {Usage Switch}
 HID_MAIN_COLLECTION_USAGE_MODIFIER = $06; {Usage Modifier}
 {0x07 to 0x7F Reserved}
 {0x80 to 0xFF Vendor Defined}

 {HID Global Item Unit Values (Section 6.2.2.7)}
 {For more information see https://physics.nist.gov/cuu/Units/units.html}
 HID_GLOBAL_UNIT_SYSTEM_MASK             = $F; {System of Measurement}
 HID_GLOBAL_UNIT_SYSTEM_SHIFT            = 0;

 HID_GLOBAL_UNIT_SYSTEM_NONE             = 0;
 HID_GLOBAL_UNIT_SYSTEM_SI_LINEAR        = 1;  {SI linear unit}
 HID_GLOBAL_UNIT_SYSTEM_SI_ROTATION      = 2;  {SI rotational units}
 HID_GLOBAL_UNIT_SYSTEM_ENGLISH_LINEAR   = 3;  {English (Imperial) linear units}
 HID_GLOBAL_UNIT_SYSTEM_ENGLISH_ROTATION = 4;  {English (Imperial) rotational units}

 HID_GLOBAL_UNIT_LENGTH_MASK             = $F; {Length (Centimeters in SI, Inches in English)}
 HID_GLOBAL_UNIT_LENGTH_SHIFT            = 4;

 HID_GLOBAL_UNIT_ROTATION_MASK           = $F; {Rotation (Radians in SI, Degrees in English)}
 HID_GLOBAL_UNIT_ROTATION_SHIFT          = 4;

 HID_GLOBAL_UNIT_MASS_MASK               = $F; {Mass (Grams in SI, Slugs in English)}
 HID_GLOBAL_UNIT_MASS_SHIFT              = 8;

 HID_GLOBAL_UNIT_TIME_MASK               = $F; {Time (Seconds)}
 HID_GLOBAL_UNIT_TIME_SHIFT              = 12;

 HID_GLOBAL_UNIT_TEMPERATURE_MASK        = $F; {Temperature (Kelvin in SI, Fahrenheit in English)}
 HID_GLOBAL_UNIT_TEMPERATURE_SHIFT       = 16;

 HID_GLOBAL_UNIT_CURRENT_MASK            = $F; {Current (Amperes)}
 HID_GLOBAL_UNIT_CURRENT_SHIFT           = 20;

 HID_GLOBAL_UNIT_LIGHT_MASK              = $F; {Luminous Intensity (Candelas)}
 HID_GLOBAL_UNIT_LIGHT_SHIFT             = 24;

 {Common HID Global Item Unit Values}
 {For a more detailed list see the k.0UNIT definitions in https://github.com/abend0c1/hidrdd/blob/master/rd.rex}
 {SI Base Units}
 HID_GLOBAL_UNIT_SI_ROTATION             = $00000012; {Rotation (Radians)}
 HID_GLOBAL_UNIT_SI_LENGTH               = $00000011; {Length (Centimeters)}
 HID_GLOBAL_UNIT_SI_MASS                 = $00000101; {Mass (Grams)}
 HID_GLOBAL_UNIT_SI_TIME                 = $00001001; {Time (Seconds)}
 HID_GLOBAL_UNIT_SI_TEMPERATURE          = $00010001; {Temperature (Kelvin)}
 HID_GLOBAL_UNIT_SI_CURRENT              = $00100001; {Current (Amperes)}
 HID_GLOBAL_UNIT_SI_LIGHT                = $01000001; {Luminous Intensity (Candelas)}

 {SI Derived Units}
 HID_GLOBAL_UNIT_SI_AREA                 = $00000021; {Area (Square Centimeters)}
 HID_GLOBAL_UNIT_SI_VOLUME               = $00000031; {Volume (Cubic Centimeters}
 HID_GLOBAL_UNIT_SI_VELOCITY             = $0000F011; {Velocity (Centimeters per second)}
 HID_GLOBAL_UNIT_SI_ACCELERATION         = $0000E011; {Acceleration (Centimeters per second squared)}

 HID_GLOBAL_UNIT_SI_FREQUENCY            = $0000F001; {Frequency (Hertz)}
 HID_GLOBAL_UNIT_SI_FORCE                = $0000E111; {Force (Newtons)}
 HID_GLOBAL_UNIT_SI_PRESSURE             = $0000E1F1; {Pressure (Pascals)}
 HID_GLOBAL_UNIT_SI_ENERGY               = $0000E121; {Energy (Joules)}
 HID_GLOBAL_UNIT_SI_POWER                = $0000D121; {Power (Watts)}

 {English Base Units}
 HID_GLOBAL_UNIT_ENGLISH_ROTATION        = $00000014; {Rotation (Degrees)}
 HID_GLOBAL_UNIT_ENGLISH_LENGTH          = $00000013; {Length (Inches)}
 HID_GLOBAL_UNIT_ENGLISH_TIME            = $00001003; {Time (Seconds)}
 HID_GLOBAL_UNIT_ENGLISH_TEMPERATURE     = $00010003; {Temperature (Fahrenheit)}
 HID_GLOBAL_UNIT_ENGLISH_CURRENT         = $00100003; {Current (Amperes)}
 HID_GLOBAL_UNIT_ENGLISH_LIGHT           = $01000003; {Luminous Intensity (Candelas)}

 HID_GLOBAL_UNIT_ENGLISH_AREA            = $00000023; {Area (Square Inches)}
 HID_GLOBAL_UNIT_ENGLISH_VOLUME          = $00000033; {Volume (Cubic Inches)}

 {HID Global Item Unit Exponent Values (Section 6.2.2.7)}
 HID_GLOBAL_UNIT_EXPONENTS:array[$0..$F] of LongInt = (
  0,
  1,
  2,
  3,
  4,
  5,
  6,
  7,
  -8,
  -7,
  -6,
  -5,
  -4,
  -3,
  -2,
  -1);

 HID_GLOBAL_UNIT_MULTIPLIERS:array[$0..$F] of Double = (
  1,          {10^0}
  10,         {10^1}
  100,        {10^2}
  1000,       {10^3}
  10000,      {10^4}
  100000,     {10^5}
  1000000,    {10^6}
  10000000,   {10^7}
  0.00000001, {10^-8}
  0.0000001,  {10^-7}
  0.000001,   {10^-6}
  0.00001,    {10^-5}
  0.0001,     {10^-4}
  0.001,      {10^-3}
  0.01,       {10^-2}
  0.1);       {10^-1}

 {HID Local Item Delimiter Values (Section 6.2.2.8)}
 HID_LOCAL_DELIMITER_CLOSE = 0;
 HID_LOCAL_DELIMITER_OPEN  = 1;

 {HID Physical Descriptor Bias Values (Section 6.2.3)}
 HID_PHYSICAL_BIAS_NONE        = (0 shl 5); {Not applicable}
 HID_PHYSICAL_BIAS_RIGHT_HAND  = (1 shl 5); {Right hand}
 HID_PHYSICAL_BIAS_LEFT_HAND   = (2 shl 5); {Left hand}
 HID_PHYSICAL_BIAS_BOTH_HANDS  = (3 shl 5); {Both hands}
 HID_PHYSICAL_BIAS_EITHER_HAND = (4 shl 5); {Either hand}

 {HID Physical Descriptor Designator Values (Section 6.2.3)}
 HID_PHYSICAL_DESIGNATOR_NONE          = $00; {None}
 HID_PHYSICAL_DESIGNATOR_HAND          = $01; {Hand}
 HID_PHYSICAL_DESIGNATOR_EYEBALL       = $02; {Eyeball}
 HID_PHYSICAL_DESIGNATOR_EYEBROW       = $03; {Eyebrow}
 HID_PHYSICAL_DESIGNATOR_EYELID        = $04; {Eyelid}
 HID_PHYSICAL_DESIGNATOR_EAR           = $05; {Ear}
 HID_PHYSICAL_DESIGNATOR_NOSE          = $06; {Nose}
 HID_PHYSICAL_DESIGNATOR_MOUTH         = $07; {Mouth}
 HID_PHYSICAL_DESIGNATOR_UPPER_LIP     = $08; {Upper lip}
 HID_PHYSICAL_DESIGNATOR_LOWER_LIP     = $09; {Lower lip}
 HID_PHYSICAL_DESIGNATOR_JAW           = $0A; {Jaw}
 HID_PHYSICAL_DESIGNATOR_NECK          = $0B; {Neck}
 HID_PHYSICAL_DESIGNATOR_UPPER_ARM     = $0C; {Upper arm}
 HID_PHYSICAL_DESIGNATOR_ELBOW         = $0D; {Elbow}
 HID_PHYSICAL_DESIGNATOR_FOREARM       = $0E; {Forearm}
 HID_PHYSICAL_DESIGNATOR_WRIST         = $0F; {Wrist}
 HID_PHYSICAL_DESIGNATOR_PALM          = $10; {Palm}
 HID_PHYSICAL_DESIGNATOR_THUMB         = $11; {Thumb}
 HID_PHYSICAL_DESIGNATOR_INDEX_FINGER  = $12; {Index finger}
 HID_PHYSICAL_DESIGNATOR_MIDDLE_FINGER = $13; {Middle finger}
 HID_PHYSICAL_DESIGNATOR_RING_FINGER   = $14; {Ring finger}
 HID_PHYSICAL_DESIGNATOR_LITTLE_FINGER = $15; {Little finger}
 HID_PHYSICAL_DESIGNATOR_HEAD          = $16; {Head}
 HID_PHYSICAL_DESIGNATOR_SHOULDER      = $17; {Shoulder}
 HID_PHYSICAL_DESIGNATOR_HIP           = $18; {Hip}
 HID_PHYSICAL_DESIGNATOR_WAIST         = $19; {Waist}
 HID_PHYSICAL_DESIGNATOR_THIGH         = $1A; {Thigh}
 HID_PHYSICAL_DESIGNATOR_KNEE          = $1B; {Knee}
 HID_PHYSICAL_DESIGNATOR_CALF          = $1C; {Calf}
 HID_PHYSICAL_DESIGNATOR_ANKLE         = $1D; {Ankle}
 HID_PHYSICAL_DESIGNATOR_FOOT          = $1E; {Foot}
 HID_PHYSICAL_DESIGNATOR_HEEL          = $1F; {Heel}
 HID_PHYSICAL_DESIGNATOR_BALL_OF_FOOT  = $20; {Ball of foot}
 HID_PHYSICAL_DESIGNATOR_BIG_TOE       = $21; {Big toe}
 HID_PHYSICAL_DESIGNATOR_SECOND_TOE    = $22; {Second toe}
 HID_PHYSICAL_DESIGNATOR_THIRD_TOE     = $23; {Third toe}
 HID_PHYSICAL_DESIGNATOR_FOURTH_TOE    = $24; {Fourth toe}
 HID_PHYSICAL_DESIGNATOR_LITTLE_TOE    = $25; {Little toe}
 HID_PHYSICAL_DESIGNATOR_BROW          = $26; {Brow}
 HID_PHYSICAL_DESIGNATOR_CHEEK         = $27; {Cheek}

 {HID Physical Descriptor Qualifier Values (Section 6.2.3)}
 HID_PHYSICAL_QUALIFIER_NONE   = (0 shl 5); {Not applicable}
 HID_PHYSICAL_QUALIFIER_RIGHT  = (1 shl 5); {Right}
 HID_PHYSICAL_QUALIFIER_LEFT   = (2 shl 5); {Left}
 HID_PHYSICAL_QUALIFIER_BOTH   = (3 shl 5); {Both}
 HID_PHYSICAL_QUALIFIER_EITHER = (4 shl 5); {Either}
 HID_PHYSICAL_QUALIFIER_CENTER = (5 shl 5); {Center}

 {HID Usage Pages (See HID Usage Tables 1.3)}
 HID_PAGE_UNDEFINED                 = $00; {Undefined}
 HID_PAGE_GENERIC_DESKTOP           = $01; {Generic Desktop Page}
 HID_PAGE_SIMULATION_CONTROLS       = $02; {Simulation Controls Page}
 HID_PAGE_VR_CONTROLS               = $03; {VR Controls Page}
 HID_PAGE_SPORT_CONTROLS            = $04; {Sport Controls Page}
 HID_PAGE_GAME_CONTROLS             = $05; {Game Controls Page}
 HID_PAGE_GENERIC_DEVICE_CONTROLS   = $06; {Generic Device Controls Page}
 HID_PAGE_KEYBOARD_KEYPAD           = $07; {Keyboard/Keypad Page}
 HID_PAGE_LED                       = $08; {LED Page}
 HID_PAGE_BUTTON                    = $09; {Button Page}
 HID_PAGE_ORDINAL                   = $0A; {Ordinal Page}
 HID_PAGE_TELEPHONY_DEVICE          = $0B; {Telephony Device Page}
 HID_PAGE_CONSUMER                  = $0C; {Consumer Page}
 HID_PAGE_DIGITIZERS                = $0D; {Digitizers Page}
 HID_PAGE_HAPTICS                   = $0E; {Haptics Page}
 HID_PAGE_PHYSICAL_INPUT_DEVICE     = $0F; {Physical Input Device Page}
 HID_PAGE_UNICODE                   = $10; {Unicode Page}
 {0x11-0x11 Reserved}
 HID_PAGE_EYE_AND_HEAD_TRACKERS     = $12; {Eye and Head Trackers Page}
 {0x13-0x13 Reserved}
 HID_PAGE_AUXILIARY_DISPLAY         = $14; {Auxiliary Display Page}
 {0x15-0x1F Reserved}
 HID_PAGE_SENSORS                   = $20; {Sensors Page}
 {0x21-0x3F Reserved}
 HID_PAGE_MEDICAL_INSTRUMENT        = $40; {Medical Instrument Page}
 HID_PAGE_BRAILLE_DISPLAY           = $41; {Braille Display Page}
 {0x42-0x58 Reserved}
 HID_PAGE_LIGHTING_AND_ILLUMINATION = $59; {Lighting And Illumination Page}
 {0x5A-0x7F Reserved}
 HID_PAGE_MONITOR                   = $80; {Monitor Page}
 HID_PAGE_MONITOR_ENUMERATED        = $81; {Monitor Enumerated Page}
 HID_PAGE_VESA_VIRTUAL_CONTROLS     = $82; {VESA Virtual Controls Page}
 {0x83-0x83 Reserved}
 HID_PAGE_POWER                     = $84; {Power Page}
 HID_PAGE_BATTERY_SYSTEM            = $85; {Battery System Page}
 {0x86-0x8B Reserved}
 HID_PAGE_BARCODE_SCANNER           = $8C; {Barcode Scanner Page}
 HID_PAGE_SCALES                    = $8D; {Scales Page}
 HID_PAGE_MAGNETIC_STRIPE_READER    = $8E; {Magnetic Stripe Reader Page}
 {0x8F-0x8F Reserved}
 HID_PAGE_CAMERA_CONTROL            = $90; {Camera Control Page}
 HID_PAGE_ARCADE                    = $91; {Arcade Page}
 HID_PAGE_GAMING_DEVICE             = $92; {Gaming Device Page}
 {0x93-0xF1CF Reserved}
 HID_PAGE_FIDO_ALLIANCE             = $F1D0; {FIDO Alliance Page}
 {0xF1D1-0xFEFF Reserved}
 {0xFF00-0xFFFF Vendor-defined}

 {HID Usage Tables (See HID Usage Tables 1.3)}
 {HID Generic Desktop Page (Partial)}
 HID_DESKTOP_UNDEFINED                     = $00; {Undefined}
 HID_DESKTOP_POINTER                       = $01; {Pointer}
 HID_DESKTOP_MOUSE                         = $02; {Mouse}
 {0x03-0x03 Reserved}
 HID_DESKTOP_JOYSTICK                      = $04; {Joystick}
 HID_DESKTOP_GAMEPAD                       = $05; {Gamepad}
 HID_DESKTOP_KEYBOARD                      = $06; {Keyboard}
 HID_DESKTOP_KEYPAD                        = $07; {Keypad}
 HID_DESKTOP_MULTI_AXIS_CONTROLLER         = $08; {Multi-axis Controller}
 HID_DESKTOP_TABLET_PC_SYSTEM_CONTROLS     = $09; {Tablet PC System Controls}
 HID_DESKTOP_WATER_COOLING_DEVICE          = $0A; {Water Cooling Device}
 HID_DESKTOP_COMPUTER_CHASSIS_DEVICE       = $0B; {Computer Chassis Device}
 HID_DESKTOP_WIRELESS_RADIO_CONTROLS       = $0C; {Wireless Radio Controls}
 HID_DESKTOP_PORTABLE_DEVICE_CONTROL       = $0D; {Portable Device Control}
 HID_DESKTOP_SYSTEM_MULTI_AXIS_CONTROLLER  = $0E; {System Multi-Axis Controller}
 HID_DESKTOP_SPATIAL_CONTROLLER            = $0F; {Spatial Controller}
 HID_DESKTOP_ASSISTIVE_CONTROL             = $10; {Assistive Control}
 HID_DESKTOP_DEVICE_DOCK                   = $11; {Device Dock}
 HID_DESKTOP_DOCKABLE_DEVICE               = $12; {Dockable Device}
 HID_DESKTOP_CALL_STATE_MANAGEMENT_CONTROL = $13; {Call State Management Control}
 {0x14-0x2F Reserved}
 HID_DESKTOP_X                             = $30; {X}
 HID_DESKTOP_Y                             = $31; {Y}
 HID_DESKTOP_Z                             = $32; {Z}
 HID_DESKTOP_RX                            = $33; {Rx}
 HID_DESKTOP_RY                            = $34; {Ry}
 HID_DESKTOP_RZ                            = $35; {Rz}
 HID_DESKTOP_SLIDER                        = $36; {Slider}
 HID_DESKTOP_DIAL                          = $37; {Dial}
 HID_DESKTOP_WHEEL                         = $38; {Wheel}
 HID_DESKTOP_HAT_SWITCH                    = $39; {Hat Switch}
 HID_DESKTOP_COUNTED_BUFFER                = $3A; {Counted Buffer}
 HID_DESKTOP_BYTE_COUNT                    = $3B; {Byte Count}
 HID_DESKTOP_MOTION_WAKEUP                 = $3C; {Motion Wakeup}
 HID_DESKTOP_START                         = $3D; {Start}
 HID_DESKTOP_SELECT                        = $3E; {Select}
 {0x3F-0x3F Reserved}
 HID_DESKTOP_VX                            = $40; {Vx}
 HID_DESKTOP_VY                            = $41; {Vy}
 HID_DESKTOP_VZ                            = $42; {Vz}
 HID_DESKTOP_VBRX                          = $43; {Vbrx}
 HID_DESKTOP_VBRY                          = $44; {Vbry}
 HID_DESKTOP_VBRZ                          = $45; {Vbrz}
 HID_DESKTOP_VNO                           = $46; {Vno}
 HID_DESKTOP_FEATURE_NOTIFICATION          = $47; {Feature Notification}
 HID_DESKTOP_RESOLUTION_MULTIPLIER         = $48; {Resolution Multiplier}
 HID_DESKTOP_QX                            = $49; {Qx}
 HID_DESKTOP_QY                            = $4A; {Qy}
 HID_DESKTOP_QZ                            = $4B; {Qz}
 HID_DESKTOP_QW                            = $4C; {Qw}

 {HID Keyboard/Keypad Page}
 {Note: These are the same as the SCAN_CODE_* values in GlobalConst}

 {HID LED Page (Partial)}
 HID_LED_UNDEFINED      = 00; {Undefined}
 HID_LED_NUM_LOCK       = 01; {Num Lock}
 HID_LED_CAPS_LOCK      = 02; {Caps Lock}
 HID_LED_SCROLL_LOCK    = 03; {Scroll Lock}
 HID_LED_COMPOSE        = 04; {Compose}
 HID_LED_KANA           = 05; {Kana}
 HID_LED_POWER          = 06; {Power}
 HID_LED_SHIFT          = 07; {Shift}
 HID_LED_DO_NOT_DISTURB = 08; {Do Not Disturb}
 HID_LED_MUTE           = 09; {Mute}

 {HID Button Page (Partial)}
 HID_BUTTON_NONE      = 0;
 HID_BUTTON_1         = 1;
 HID_BUTTON_2         = 2;
 HID_BUTTON_3         = 3;
 HID_BUTTON_4         = 4;
 HID_BUTTON_5         = 5;
 HID_BUTTON_PRIMARY   = HID_BUTTON_1;
 HID_BUTTON_SECONDARY = HID_BUTTON_2;
 HID_BUTTON_TERTIARY  = HID_BUTTON_3;
 {Note: Buttons are defined as Button1 to Button65535}
 HID_BUTTON_65535     = $FFFF;

 {HID logging}
 HID_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {HID debugging messages}
 HID_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {HID informational messages, such as a device being attached or detached}
 HID_LOG_LEVEL_WARN      = LOG_LEVEL_WARN;   {HID warning messages}
 HID_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {HID error messages}
 HID_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No HID messages}

var
 HID_DEFAULT_LOG_LEVEL:LongWord = HID_LOG_LEVEL_DEBUG; {Minimum level for HID messages.  Only messages with level greater than or equal to this will be printed}

var
 {HID logging}
 HID_LOG_ENABLED:Boolean;

{==============================================================================}
type
 {HID specific types}
 {HID Descriptor  (Section 6.2.1)}
 PHIDDescriptor = ^THIDDescriptor;
 THIDDescriptor = packed record
  bLength:Byte;
  bDescriptorType:Byte;
  bcdHID:Word;
  bCountryCode:Byte;
  bNumDescriptors:Byte;
  bHIDDescriptorType:Byte;
  wHIDDescriptorLength:Word;
  {Note: Up to two optional bHIDDescriptorType/wHIDDescriptorLength pairs after the Report descriptor details}
 end;

 {HID Report Descriptor  (Section 6.2.2)}
 PHIDReportDescriptor = Pointer;

 {HID Physical Descriptor (Section 6.2.3)}
 PHIDPhysicalDescriptor = ^THIDPhysicalDescriptor;
 THIDPhysicalDescriptor = packed record
  bDesignator:Byte; {Indicates which part of the body affects the item}
  bFlags:Byte;      {Bits specifying flags (7..5 Qualifier / 4..0 Effort)}
 end;

 PHIDPhysicalDescriptorSet0 = ^THIDPhysicalDescriptorSet0;
 THIDPhysicalDescriptorSet0 = packed record
  bNumber:Byte; {Number of Physical Descriptor sets not including Physical Descriptor set 0}
  wLength:Word; {Length of each Physical descriptor set}
 end;

 PHIDPhysicalDescriptorSet = ^THIDPhysicalDescriptorSet;
 THIDPhysicalDescriptorSet = packed record
  bPhysicalInfo:Byte; {Bits specifying physical information (7..5 Bias / 4..0 Preference)}
  wPhysicalDescriptor:array[0..0] of THIDPhysicalDescriptor; {Physical descriptor data}
 end;

 {Structures for HID report parsing}
 {HID State}
 PHIDState = ^THIDState;
 THIDState = record
  {Local State}
  Usage:LongWord;
  UsageCount:LongWord;      {Provides UsageMinimum/UsageMaximum}
  DesignatorIndex:LongWord;
  DesignatorCount:LongWord; {Provides DesignatorMinimum/DesignatorMaximum}
  StringIndex:LongWord;
  StringCount:LongWord;     {Provides StringMinimum/StringMaximum}
  Delimiter:Boolean;
  {Global State}
  UsagePage:Word;
  LogicalMinimum:LongInt;
  LogicalMaximum:LongInt;
  PhysicalMinimum:LongInt;
  PhysicalMaximum:LongInt;
  UnitType:LongWord;
  UnitExponent:LongWord;
  ReportSize:LongWord;
  ReportId:LongWord;
  ReportCount:LongWord;
 end;

 {HID Stack}
 PHIDStack = ^THIDStack;
 THIDStack = record
  State:PHIDState;
  Next:PHIDStack;
 end;

 {HID Usage}
 PHIDUsage = ^THIDUsage;
 PHIDReport = ^THIDReport;         {Forward declared to satisfy HIDUsage}
 PHIDCollection = ^THIDCollection; {Forward declared to satisfy HIDReport}

 PHIDDevice = ^THIDDevice;         {Forward declared to satisfy HIDCollection}
 PHIDConsumer = ^THIDConsumer;     {Forward declared to satisfy HIDCollection}

 PHIDUsages = ^THIDUsages;
 THIDUsages = array[0..0] of PHIDUsage;

 THIDUsage = record
  Page:Word;                {The usage page this usage refers to}
  Usage:Word;               {The usage within the usage page}
  Count:Word;               {The total number of sequential usages where Usage represents the minimum value or 1 for a single usage (Usage range is from Usage to Usage + Count - 1)}

  Index:LongWord;           {The index of this usage in the report (First usage is 0)}

  StringIndex:LongWord;     {The string index for this usage}
  StringCount:LongWord;     {The total number of sequential string index values where string index represents the minimum value or 1 for a single string}

  DesignatorIndex:LongWord; {The designator index for this usage}
  DesignatorCount:LongWord; {The total number of sequential designator index values where designator index represents the minimum value or 1 for a single designator}

  LogicalMinimum:LongInt;   {The logical minimum value for this usage}
  LogicalMaximum:LongInt;   {The logical maximum value for this usage}
  PhysicalMinimum:LongInt;  {The physical minimum value for this usage (in Units)}
  PhysicalMaximum:LongInt;  {The physical maximum value for this usage (in Units)}
  UnitType:LongWord;        {The unit type for this uage}
  UnitExponent:LongWord;    {The unit exponent index for this usage}

  Aliases:PHIDUsages;       {The list of aliased usages for this control (See Delimiters in Section 6.2.2.8)}
  AliasCount:LongWord;      {The number of aliased usages contained for this contro}

  Report:PHIDReport;        {The report this usage belongs to}
 end;

 {HID Report}
 {PHIDReport = ^THIDReport;} {Declared above for HIDUsage}

 PHIDReports = ^THIDReports;
 THIDReports = array[0..0] of PHIDReport;

 THIDReport = record
  Id:Byte;                   {The Id of this report}
  Kind:Byte;                 {The type of report (Input, Output or Feature) (eg HID_REPORT_INPUT)}
  Flags:LongWord;            {The main item flags for this report (eg HID_MAIN_ITEM_VARIABLE)}

  Size:LongWord;             {The number of bits per field in this report}
  Count:LongWord;            {The number of field in this report}

  Index:LongWord;            {The index of this report in the collection (First report is 0)}

  Usages:PHIDUsages;         {The list of usages contained in this report}
  UsageCount:LongWord;       {The number of usages contained in this report}

  Collection:PHIDCollection; {The collection this report belongs to}
 end;

 {HID Collection}
 {PHIDCollection = ^THIDCollection;} {Declared above for HIDReport}

 PHIDCollections = ^THIDCollections;
 THIDCollections = array[0..0] of PHIDCollection;

 THIDCollection = record
  Page:Word;                   {The usage page this collection refers to (eg HID_PAGE_GENERIC_DESKTOP)}
  Usage:Word;                  {The usage within the usage page (eg HID_DESKTOP_MOUSE)}
  Flags:LongWord;              {The main item flags for this collection (eg HID_MAIN_COLLECTION_APPLICATION)}

  Start:LongWord;              {The first byte of this collection in the report descriptor}

  Reports:PHIDReports;         {The list of reports contained in this collection}
  ReportCount:LongWord;        {The number of reports contained in this collection}
  Collections:PHIDCollections; {The list of collections contained in this collection}
  CollectionCount:LongWord;    {The number of collections contained in this collection}

  Parent:PHIDCollection;       {The parent collection or nil if this is a top level collection}

  Device:PHIDDevice;           {The device this collection belongs to}
  Consumer:PHIDConsumer;       {The consumer which is bound to this collection (or nil if not bound)}
  PrivateData:Pointer;         {Private data for the consumer of this collection}
 end;

 {HID Device}
 {PHIDDevice = ^THIDDevice;} {Declared above for HIDCollection}

 {HID Device Enumeration Callback}
 THIDDeviceEnumerate = function(Device:PHIDDevice;Data:Pointer):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 {HID Device Notification Callback}
 THIDDeviceNotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 {HID Device Methods}
 THIDDeviceGetIdle = function(Device:PHIDDevice;var Duration:Word;ReportId:Byte):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 THIDDeviceSetIdle = function(Device:PHIDDevice;Duration:Word;ReportId:Byte):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 THIDDeviceGetReport = function(Device:PHIDDevice;ReportType,ReportId:Byte;ReportData:Pointer;ReportSize:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 THIDDeviceSetReport = function(Device:PHIDDevice;ReportType,ReportId:Byte;ReportData:Pointer;ReportSize:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 THIDDeviceAllocateReport = function(Device:PHIDDevice;Collection:PHIDCollection;ReportId:Byte;ReportSize:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 THIDDeviceReleaseReport = function(Device:PHIDDevice;ReportId:Byte):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 THIDDeviceSubmitReport = function(Device:PHIDDevice;ReportId:Byte):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 THIDDeviceCancelReport = function(Device:PHIDDevice;ReportId:Byte):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 THIDDeviceGetProtocol = function(Device:PHIDDevice;var Protocol:Byte):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 THIDDeviceSetProtocol = function(Device:PHIDDevice;Protocol:Byte):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 THIDDeviceGetInterval = function(Device:PHIDDevice;var Interval:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 THIDDeviceSetInterval = function(Device:PHIDDevice;Interval:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 THIDDeviceGetReportDescriptor = function(Device:PHIDDevice;Descriptor:PHIDReportDescriptor;Size:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 THIDDeviceGetPhysicalDescriptorSet0 = function(Device:PHIDDevice;Descriptor:PHIDPhysicalDescriptorSet0):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 THIDDeviceGetPhysicalDescriptorSet = function(Device:PHIDDevice;Descriptor:PHIDPhysicalDescriptorSet;Index:Byte;Size:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 THIDDevice = record
  {Device Properties}
  Device:TDevice;                                                      {The Device entry for this HID}
  {HID Properties}
  HIDId:LongWord;                                                      {Unique Id of this HID in the HID device table}
  HIDState:LongWord;                                                   {HID device state (eg HID_STATE_ATTACHED)}
  Consumer:PHIDConsumer;                                               {The consumer which is bound to this device (or nil if not bound)}
  DeviceGetIdle:THIDDeviceGetIdle;                                     {A device specific GetIdle method (If supported by provider)}
  DeviceSetIdle:THIDDeviceSetIdle;                                     {A device specific SetIdle method (If supported by provider)}
  DeviceGetReport:THIDDeviceGetReport;                                 {A device specific GetReport method (If supported by provider)}
  DeviceSetReport:THIDDeviceSetReport;                                 {A device specific SetReport method (If supported by provider)}
  DeviceAllocateReport:THIDDeviceAllocateReport;                       {A device specific AllocateReport method (If supported by provider)}
  DeviceReleaseReport:THIDDeviceReleaseReport;                         {A device specific ReleaseReport method (If supported by provider)}
  DeviceSubmitReport:THIDDeviceSubmitReport;                           {A device specific SubmitReport method (If supported by provider)}
  DeviceCancelReport:THIDDeviceCancelReport;                           {A device specific CancelReport method (If supported by provider)}
  DeviceGetProtocol:THIDDeviceGetProtocol;                             {A device specific GetProtocol method (If supported by provider)}
  DeviceSetProtocol:THIDDeviceSetProtocol;                             {A device specific SetProtocol method (If supported by provider)}
  DeviceGetInterval:THIDDeviceGetInterval;                             {A device specific GetInterval method (If supported by provider)}
  DeviceSetInterval:THIDDeviceSetInterval;                             {A device specific SetInterval method (If supported by provider)}
  DeviceGetReportDescriptor:THIDDeviceGetReportDescriptor;             {A device specific GetReportDescriptor method (If supported by provider)}
  DeviceGetPhysicalDescriptorSet0:THIDDeviceGetPhysicalDescriptorSet0; {A device specific GetPhysicalDescriptorSet0 method (If supported by provider)}
  DeviceGetPhysicalDescriptorSet:THIDDeviceGetPhysicalDescriptorSet;   {A device specific GetPhysicalDescriptorSet method (If supported by provider)}
  {Driver Properties}
  Lock:TMutexHandle;                                                   {HID device lock}
  PrivateData:Pointer;                                                 {Private data for the consumer of this HID device (If applicable)}
  Collections:PHIDCollections;                                         {The parsed report descriptor with collections, reports and usages}
  CollectionCount:LongWord;                                            {The number of top level collections contained in the report descriptor}
  Descriptor:PHIDReportDescriptor;                                     {The raw report descriptor obtained from the device}
  DescriptorSize:LongWord;                                             {The size of the data pointed to by Descriptor}
  {Statistics Properties}
  ReceiveCount:LongWord;
  ReceiveErrors:LongWord;
  {Internal Properties}
  Prev:PHIDDevice;                                                     {Previous entry in Device table}
  Next:PHIDDevice;                                                     {Next entry in Device table}
 end;

 {HID Consumer}
 {PHIDConsumer = ^THIDConsumer;} {Declared above for HIDCollection}

 {HID Consumer Enumeration Callback}
 THIDConsumerEnumerate = function(Consumer:PHIDConsumer;Data:Pointer):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 {HID Consumer Methods}
 THIDDeviceBind = function(Device:PHIDDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 THIDDeviceUnbind = function(Device:PHIDDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 THIDCollectionBind = function(Device:PHIDDevice;Collection:PHIDCollection):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 THIDCollectionUnbind = function(Device:PHIDDevice;Collection:PHIDCollection):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 THIDReportReceive = function(Collection:PHIDCollection;ReportId:Byte;ReportData:Pointer;ReportSize:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 THIDConsumer = record
  {Driver Properties}
  Driver:TDriver;                        {The Driver entry for this HID Consumer}
  {Consumer Properties}
  DeviceBind:THIDDeviceBind;             {A Device Bind method to be called when a HID device bind is requested by a provider (Optional)}
  DeviceUnbind:THIDDeviceUnbind;         {A Device Ubind method to be called when a HID device unbind is requested by a provider (Optional)}
  CollectionBind:THIDCollectionBind;     {A Collection Bind method to be called when a HID collection bind is requested by a provider (Mandatory)}
  CollectionUnbind:THIDCollectionUnbind; {A Collection Unbind method to be called when a HID collection unbind is requested by a provider (Mandatory)}
  ReportReceive:THIDReportReceive;       {A Report Receive method to be called when a HID input report is received by a provider (Mandatory)}
  {Interface Properties}
  Lock:TMutexHandle;                     {Consumer lock}
  {Internal Properties}
  Prev:PHIDConsumer;                     {Previous entry in Consumer table}
  Next:PHIDConsumer;                     {Next entry in Consumer table}
 end;

 {Structures for HID consumer report processing}
 {HID Report Field}
 PHIDField = ^THIDField;
 THIDField = record
  Page:Word;              {The usage page of this field}
  Usage:Word;             {The usage within the usage page}
  Count:LongWord;         {The total number of sequential usages where Usage represents the minimum value or 1 for a single usage (Usage range is from Usage to Usage + Count - 1)}

  Flags:LongWord;         {The flags for this field}

  Size:LongWord;          {The length in bytes of this field within the input, output or feature report}
  Bits:LongWord;          {The length in bits of this field within the input, output or feature report}
  Offset:LongWord;        {The byte offset of this field within the input, output or feature report}
  Shift:LongWord;         {The number shift bits to access this field in the input, output or feature report}

  Minimum:LongInt;        {The minimum value for this field (either in logical or physical units)}
  Maximum:LongInt;        {The maximum value for this field (either in logical or physical units)}
  Multiplier:Double;      {The conversion multiplier for this field from logical to physical units}
  Resolution:Double;      {The unit resolution for this field in counts per physical unit}

  Next:PHIDField;         {The next field in the list}
 end;

 {HID Report Definition}
 PHIDDefinition = ^THIDDefinition;
 THIDDefinition = record
  Id:Byte;                {The Id of this report}
  Kind:Byte;              {The type of this report (Input, Output or Feature)}
  Size:LongWord;          {The total length of this input, output or feature report in bytes (Including the Id byte)}
  Fields:PHIDField;       {Linked list of fields in this input, output or feature report}

  Next:PHIDDefinition;    {The next definition in the list}
 end;

{==============================================================================}
{var}
 {HID specific variables}

{==============================================================================}
{Initialization Functions}
procedure HIDInit;

{==============================================================================}
{HID Functions}
function HIDParserParseCollections(Device:PHIDDevice;var Collections:PHIDCollections;var Count:LongWord):LongWord;
function HIDParserFreeCollections(Collections:PHIDCollections;Count:LongWord):LongWord;

function HIDParserCountCollections(Device:PHIDDevice;Parent:PHIDCollection):LongWord;
function HIDParserCountReports(Device:PHIDDevice;Collection:PHIDCollection):LongWord;
function HIDParserCountUsages(Device:PHIDDevice;Report:PHIDReport):LongWord;

function HIDParserAllocateCollection(Device:PHIDDevice;Parent:PHIDCollection;State:PHIDState;Flags,Start:LongWord):PHIDCollection;
function HIDParserAllocateReport(Device:PHIDDevice;Collection:PHIDCollection;State:PHIDState;Kind:Byte;Flags,Index:LongWord):PHIDReport;
function HIDParserAllocateUsage(Device:PHIDDevice;Report:PHIDReport;State:PHIDState;Index:LongWord):PHIDUsage;
function HIDParserUpdateUsage(Device:PHIDDevice;Report:PHIDReport;State:PHIDState;Usage:PHIDUsage):Boolean;

function HIDParserPopStack(var Stack:PHIDStack;var State:PHIDState):LongWord;
function HIDParserPushStack(Stack:PHIDStack):LongWord;
function HIDParserFreeStack(Stack:PHIDStack):LongWord;

function HIDParserResetState(State:PHIDState):LongWord;
function HIDParserCleanState(State:PHIDState):LongWord;

function HIDFindCollection(Device:PHIDDevice;Page,Usage:Word):PHIDCollection;

function HIDFindReportIds(Device:PHIDDevice;Collection:PHIDCollection;var MinId,MaxId:Byte):LongWord;

function HIDAllocateDefinition(Device:PHIDDevice;Collection:PHIDCollection;Kind,Id:Byte):PHIDDefinition;
function HIDFreeDefinition(Definition:PHIDDefinition):LongWord;

function HIDInsertBitField(Field:PHIDField;Buffer:Pointer;Size:LongWord;Value:Boolean):LongWord;
function HIDInsertSignedField(Field:PHIDField;Buffer:Pointer;Size:LongWord;Value:LongInt):LongWord;
function HIDInsertUnsignedField(Field:PHIDField;Buffer:Pointer;Size,Value:LongWord):LongWord;

function HIDExtractBitField(Field:PHIDField;Buffer:Pointer;Size:LongWord;var Value:Boolean):LongWord;
function HIDExtractSignedField(Field:PHIDField;Buffer:Pointer;Size:LongWord;var Value:LongInt):LongWord;
function HIDExtractUnsignedField(Field:PHIDField;Buffer:Pointer;Size:LongWord;var Value:LongWord):LongWord;

{==============================================================================}
{HID Device Functions}
function HIDDeviceSetState(Device:PHIDDevice;State:LongWord):LongWord;

function HIDDeviceGetIdle(Device:PHIDDevice;var Duration:Word;ReportId:Byte):LongWord;
function HIDDeviceSetIdle(Device:PHIDDevice;Duration:Word;ReportId:Byte):LongWord;

function HIDDeviceGetReport(Device:PHIDDevice;ReportType,ReportId:Byte;ReportData:Pointer;ReportSize:LongWord):LongWord;
function HIDDeviceSetReport(Device:PHIDDevice;ReportType,ReportId:Byte;ReportData:Pointer;ReportSize:LongWord):LongWord;

function HIDDeviceAllocateReport(Device:PHIDDevice;Collection:PHIDCollection;ReportId:Byte;ReportSize:LongWord):LongWord;
function HIDDeviceReleaseReport(Device:PHIDDevice;ReportId:Byte):LongWord;

function HIDDeviceSubmitReport(Device:PHIDDevice;ReportId:Byte):LongWord;
function HIDDeviceCancelReport(Device:PHIDDevice;ReportId:Byte):LongWord;

function HIDDeviceGetProtocol(Device:PHIDDevice;var Protocol:Byte):LongWord;
function HIDDeviceSetProtocol(Device:PHIDDevice;Protocol:Byte):LongWord;

function HIDDeviceGetInterval(Device:PHIDDevice;var Interval:LongWord):LongWord;
function HIDDeviceSetInterval(Device:PHIDDevice;Interval:LongWord):LongWord;

function HIDDeviceGetReportDescriptor(Device:PHIDDevice;Descriptor:PHIDReportDescriptor;Size:LongWord):LongWord;
function HIDDeviceGetPhysicalDescriptorSet0(Device:PHIDDevice;Descriptor:PHIDPhysicalDescriptorSet0):LongWord;
function HIDDeviceGetPhysicalDescriptorSet(Device:PHIDDevice;Descriptor:PHIDPhysicalDescriptorSet;Index:Byte;Size:LongWord):LongWord;

function HIDDeviceBindDevice(Device:PHIDDevice):LongWord;
function HIDDeviceUnbindDevice(Device:PHIDDevice;Consumer:PHIDConsumer):LongWord;

function HIDDeviceBindCollections(Device:PHIDDevice):LongWord;
function HIDDeviceUnbindCollections(Device:PHIDDevice;Consumer:PHIDConsumer):LongWord;

function HIDDeviceCreate:PHIDDevice;
function HIDDeviceCreateEx(Size:LongWord):PHIDDevice;
function HIDDeviceDestroy(Device:PHIDDevice):LongWord;

function HIDDeviceRegister(Device:PHIDDevice):LongWord;
function HIDDeviceDeregister(Device:PHIDDevice):LongWord;

function HIDDeviceFind(HIDId:LongWord):PHIDDevice;
function HIDDeviceFindByName(const Name:String):PHIDDevice; inline;
function HIDDeviceFindByDescription(const Description:String):PHIDDevice; inline;
function HIDDeviceEnumerate(Callback:THIDDeviceEnumerate;Data:Pointer):LongWord;

function HIDDeviceNotification(Device:PHIDDevice;Callback:THIDDeviceNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;

{==============================================================================}
{HID Consumer Functions}
function HIDConsumerCreate:PHIDConsumer;
function HIDConsumerCreateEx(Size:LongWord):PHIDConsumer;
function HIDConsumerDestroy(Consumer:PHIDConsumer):LongWord;

function HIDConsumerRegister(Consumer:PHIDConsumer):LongWord;
function HIDConsumerDeregister(Consumer:PHIDConsumer):LongWord;

function HIDConsumerFind(ConsumerId:LongWord):PHIDConsumer;
function HIDConsumerFindByName(const Name:String):PHIDConsumer; inline;
function HIDConsumerEnumerate(Callback:THIDConsumerEnumerate;Data:Pointer):LongWord;

{==============================================================================}
{HID Helper Functions}
function HIDIsBitField(Field:PHIDField):Boolean;
function HIDIsByteField(Field:PHIDField):Boolean;
function HIDIsWordField(Field:PHIDField):Boolean;
function HIDIsLongField(Field:PHIDField):Boolean;
function HIDIsSignedField(Field:PHIDField):Boolean;

function HIDPageToString(Page:Word):String;
function HIDUsageToString(Page,Usage,Count:Word):String;

function HIDUnitTypeToString(UnitType:LongWord):String;

function HIDReportKindToString(Kind:Byte):String;
function HIDReportFlagsToString(Flags:LongWord):String;

function HIDCollectionFlagsToString(Flags:LongWord):String;

procedure HIDLog(Level:LongWord;Device:PHIDDevice;const AText:String);
procedure HIDLogInfo(Device:PHIDDevice;const AText:String); {inline;}
procedure HIDLogWarn(Device:PHIDDevice;const AText:String); {inline;}
procedure HIDLogError(Device:PHIDDevice;const AText:String); {inline;}
procedure HIDLogDebug(Device:PHIDDevice;const AText:String); {inline;}

{==============================================================================}
{HID Device Helper Functions}
function HIDDeviceGetCount:LongWord; inline;

function HIDDeviceCheck(Device:PHIDDevice):PHIDDevice;

function HIDDeviceTypeToString(HIDType:LongWord):String;
function HIDDeviceStateToString(HIDState:LongWord):String;

function HIDDeviceStateToNotification(State:LongWord):LongWord;

{==============================================================================}
{HID Consumer Helper Functions}
function HIDConsumerGetCount:LongWord; inline;

function HIDConsumerCheck(Consumer:PHIDConsumer):PHIDConsumer;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {HID specific variables}
 HIDInitialized:Boolean;

 HIDDeviceTable:PHIDDevice;
 HIDDeviceTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 HIDDeviceTableCount:LongWord;

 HIDConsumerTable:PHIDConsumer;
 HIDConsumerTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 HIDConsumerTableCount:LongWord;

{==============================================================================}
{==============================================================================}
{Forward Declarations}

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure HIDInit;
{Initialize the HID unit, device and consumer tables}

{Note: Called only during system startup}
begin
 {}
 {Check Initialized}
 if HIDInitialized then Exit;

 {Initialize Logging}
 HID_LOG_ENABLED:=(HID_DEFAULT_LOG_LEVEL <> HID_LOG_LEVEL_NONE);

 {Initialize HID Device Table}
 HIDDeviceTable:=nil;
 HIDDeviceTableLock:=CriticalSectionCreate;
 HIDDeviceTableCount:=0;
 if HIDDeviceTableLock = INVALID_HANDLE_VALUE then
  begin
   if HID_LOG_ENABLED then HIDLogError(nil,'Failed to create device table lock');
  end;

 {Initialize HID Consumer Table}
 HIDConsumerTable:=nil;
 HIDConsumerTableLock:=CriticalSectionCreate;
 HIDConsumerTableCount:=0;
 if HIDConsumerTableLock = INVALID_HANDLE_VALUE then
  begin
   if HID_LOG_ENABLED then HIDLogError(nil,'Failed to create consumer table lock');
  end;

 HIDInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{HID Functions}
function HIDParserParseCollections(Device:PHIDDevice;var Collections:PHIDCollections;var Count:LongWord):LongWord;
{Parse the HID report descriptor of the provided device and popuplate the collections, reports and usages}
{Device: The HID device to parse collections for}
{Collections: A pointer to the top level collections array to be populated}
{Count: A variable to return the number of top level collections}
{Return: ERROR_SUCCESS if completed or another error code on failure}

type
 PHIDLink = ^THIDLink;
 THIDLink = record
  Usage:PHIDUsage;
  Next:PHIDLink;
 end;

var
 Item:PByte;
 Level:LongWord;
 Offset:LongWord;

 State:PHIDState;
 Stack:PHIDStack;

 Completed:Boolean;
 AliasCount:LongWord;
 UsageCount:LongWord;
 ReportCount:LongWord;
 CollectionCount:LongWord;

 function HIDSizeToBits(Size:Byte):Byte;
 {Get the number of bits for a HID size value}
 begin
  {}
  Result:=0;

  case Size of
   HID_SIZE_1:Result:=8;
   HID_SIZE_2:Result:=16;
   HID_SIZE_4:Result:=32;
  end;
 end;

 function HIDLinkUsage(var First,Last:PHIDLink;Usage:PHIDUsage):Boolean;
 {Allocate a new link and add it as the last in the list}
 var
  Link:PHIDLink;
 begin
  {}
  Result:=False;

  {Check Usage}
  if Usage = nil then Exit;

  {Allocate Link}
  Link:=AllocMem(SizeOf(THIDLink));
  if Link = nil then Exit;

  {Update Link}
  Link.Usage:=Usage;

  {Check First}
  if First = nil then
   begin
    {Update First}
    First:=Link;

    {Update Last}
    Last:=Link;
   end
  else
   begin
    {Update Last}
    Last.Next:=Link;
    Last:=Link;
   end;

  Result:=True;
 end;

 function HIDUnlinkUsage(var First,Last:PHIDLink;Link:PHIDLink):Boolean;
 {Remove the first link in the list and free it}
 begin
  {}
  Result:=False;

  {Check Link}
  if Link = nil then Exit;

  {Check First}
  if First = nil then Exit;

  {Check Last}
  if Last = nil then Exit;
  if Link <> First then Exit;

  {Update First}
  First:=First.Next;

  {Update Last}
  if First = nil then Last:=nil;

  {Free Link}
  FreeMem(Link);

  Result:=True;
 end;

 function HIDParserParseInternal(Parent:PHIDCollection):Boolean;
 var
  Tag:Byte;
  Len:Byte;
  Kind:Byte;
  Data:LongInt;

  AliasIndex:LongInt;
  UsageIndex:LongInt;
  ReportIndex:LongInt;
  CollectionIndex:LongInt;

  Alias:PHIDUsage;
  Usage:PHIDUsage;
  Report:PHIDReport;
  Collection:PHIDCollection;

  FirstAlias:PHIDLink;
  LastAlias:PHIDLink;
  FirstUsage:PHIDLink;
  LastUsage:PHIDLink;
 begin
  {}
  Result:=False;
  try
   {Set Defaults}
   AliasIndex:=-1;
   UsageIndex:=-1;
   ReportIndex:=-1;
   CollectionIndex:=-1;
   FirstAlias:=nil;
   LastAlias:=nil;
   FirstUsage:=nil;
   LastUsage:=nil;

   {Parse Descriptor}
   while Offset < Device.DescriptorSize do
    begin
     {Get Item}
     Item:=PByte(Device.Descriptor + Offset);

     {Get Length and Tag}
     Len:=Item[0] and HID_SIZE_MASK;
     Tag:=Item[0] and HID_TAG_MASK;

     {Get Data}
     case Len of
      HID_SIZE_0:begin
        {0 bytes}
        Data:=0;

        Inc(Offset,1);
       end;
      HID_SIZE_1:begin
        {1 byte}
        Data:=Item[1];

        Inc(Offset,2);
       end;
      HID_SIZE_2:begin
        {2 bytes}
        Data:=Item[1] or (Item[2] shl 8);

        Inc(Offset,3);
       end;
      HID_SIZE_4:begin
        {4 bytes}
        Data:=Item[1] or (Item[2] shl 8) or (Item[3] shl 16) or (Item[4] shl 24);

        Inc(Offset,5);
       end;
     end;

     {Check Tag}
     case Tag of
      {Main Item Tags}
      HID_TAG_MAIN_INPUT,
      HID_TAG_MAIN_OUTPUT,
      HID_TAG_MAIN_FEATURE:begin
        {Get Type}
        case Tag of
         HID_TAG_MAIN_INPUT:Kind:=HID_REPORT_INPUT;
         HID_TAG_MAIN_OUTPUT:Kind:=HID_REPORT_OUTPUT;
         HID_TAG_MAIN_FEATURE:Kind:=HID_REPORT_FEATURE;
        end;

        {Increment Reports}
        Inc(ReportIndex);
        Inc(ReportCount);

        {Allocate Report}
        Report:=HIDParserAllocateReport(Device,Parent,State,Kind,Data,ReportIndex);
        if Report = nil then Exit;

        {Assign Usages}
        if Report.UsageCount > 0 then
         begin
          while FirstUsage <> nil do
           begin
            {Get Usage}
            Usage:=FirstUsage.Usage;

            {Update Usages}
            Report.Usages[Usage.Index]:=Usage;

            {Update Usage}
            if not HIDParserUpdateUsage(Device,Report,State,Usage) then Exit;

            {Unlink Usage}
            if not HIDUnlinkUsage(FirstUsage,LastUsage,FirstUsage) then Exit;
           end;

          {Reset Aliases}
          AliasIndex:=-1;

          {Reset Usages}
          UsageIndex:=-1;

          {Reset Usage}
          Usage:=nil;
         end;

        {Clean State}
        HIDParserCleanState(State);

        {Update Reports}
        Parent.Reports[ReportIndex]:=Report;
       end;
      HID_TAG_MAIN_COLLECTION:begin
        {Increment Collections}
        Inc(CollectionIndex);
        Inc(CollectionCount);

        {Allocate Collection}
        Collection:=HIDParserAllocateCollection(Device,Parent,State,Data,Offset);
        if Collection = nil then Exit;

        {Clear Usages}
        while FirstUsage <> nil do
         begin
          {Get Usage}
          Usage:=FirstUsage.Usage;

          {Reset Aliases}
          Dec(AliasCount,Usage.AliasCount);

          {Reset Usages}
          Dec(UsageCount);

          {Free Usage}
          FreeMem(Usage);

          {Unlink Usage}
          if not HIDUnlinkUsage(FirstUsage,LastUsage,FirstUsage) then Exit;
         end;

        {Reset Aliases}
        AliasIndex:=-1;

        {Reset Usages}
        UsageIndex:=-1;

        {Reset Usage}
        Usage:=nil;

        {Clean State}
        HIDParserCleanState(State);

        {Check Level}
        if Level = 0 then
         begin
          {Update Collections}
          Collections[CollectionIndex]:=Collection;
         end
        else
         begin
          {Update Collections}
          Parent.Collections[CollectionIndex]:=Collection;
         end;

        {Increment Level}
        Inc(Level);

        {Parse Collections}
        if not HIDParserParseInternal(Collection) then Exit;

        {Check Completed}
        if Completed then Break;
       end;
      HID_TAG_MAIN_END_COLLECTION:begin
        {Clean State}
        HIDParserCleanState(State);

        {Check Level}
        if Level = 0 then Completed:=True;

        {Check Completed}
        if Completed then Break;

        {Decrement Level}
        Dec(Level);

        Break;
       end;

      {Global Item Tags}
      HID_TAG_GLOBAL_USAGE_PAGE:begin
        {Update State}
        State.UsagePage:=Data;
       end;
      HID_TAG_GLOBAL_LOGICAL_MINIMUM:begin
        {Update State}
        State.LogicalMinimum:=SignExtend32(Data,HIDSizeToBits(Len));
       end;
      HID_TAG_GLOBAL_LOGICAL_MAXIMUM:begin
        {Update State}
        State.LogicalMaximum:=SignExtend32(Data,HIDSizeToBits(Len));
       end;
      HID_TAG_GLOBAL_PHYSICAL_MINIMUM:begin
        {Update State}
        State.PhysicalMinimum:=SignExtend32(Data,HIDSizeToBits(Len));
       end;
      HID_TAG_GLOBAL_PHYSICAL_MAXIMUM:begin
        {Update State}
        State.PhysicalMaximum:=SignExtend32(Data,HIDSizeToBits(Len));
       end;
      HID_TAG_GLOBAL_UNIT_EXPONENT:begin
        {Update State}
        State.UnitExponent:=Data;
       end;
      HID_TAG_GLOBAL_UNIT:begin
        {Update State}
        State.UnitType:=Data;
       end;
      HID_TAG_GLOBAL_REPORT_SIZE:begin
        {Update State}
        State.ReportSize:=Data;
       end;
      HID_TAG_GLOBAL_REPORT_ID:begin
        {Update State}
        State.ReportId:=Data;
       end;
      HID_TAG_GLOBAL_REPORT_COUNT:begin
        {Update State}
        State.ReportCount:=Data;
       end;
      HID_TAG_GLOBAL_PUSH:begin
        {Push Stack}
        if HIDParserPushStack(Stack) <> ERROR_SUCCESS then Exit;
       end;
      HID_TAG_GLOBAL_POP:begin
        {Pop Stack}
        if HIDParserPopStack(Stack,State) <> ERROR_SUCCESS then Exit;
       end;

      {Local Item Tags}
      HID_TAG_LOCAL_USAGE:begin
        {Update State}
        State.Usage:=Data;
        State.UsageCount:=1;

        {Check Delimiter}
        if State.Delimiter and (Usage <> nil) then
         begin
          {Increment Aliases}
          Inc(AliasIndex);
          Inc(AliasCount);

          {Allocate Alias}
          Alias:=HIDParserAllocateUsage(Device,nil,State,AliasIndex);
          if Alias = nil then Exit;

          {Link Alias}
          if not HIDLinkUsage(FirstAlias,LastAlias,Alias) then Exit;

          {Increment Usage}
          Inc(Usage.AliasCount);
         end
        else
         begin
          {Increment Usages}
          Inc(UsageIndex);
          Inc(UsageCount);

          {Allocate Usage}
          Usage:=HIDParserAllocateUsage(Device,nil,State,UsageIndex);
          if Usage = nil then Exit;

          {Link Usage}
          if not HIDLinkUsage(FirstUsage,LastUsage,Usage) then Exit;

          {Reset Usage}
          if not State.Delimiter then Usage:=nil;
         end;
       end;
      HID_TAG_LOCAL_USAGE_MINIMUM:begin
        {Update State}
        State.Usage:=Data;
        State.UsageCount:=0;
       end;
      HID_TAG_LOCAL_USAGE_MAXIMUM:begin
        {Update State}
        State.UsageCount:=(Data - State.Usage) + 1;

        {Check Delimiter}
        if State.Delimiter and (Usage <> nil) then
         begin
          {Increment Aliases}
          Inc(AliasIndex);
          Inc(AliasCount);

          {Allocate Alias}
          Alias:=HIDParserAllocateUsage(Device,nil,State,AliasIndex);
          if Alias = nil then Exit;

          {Link Alias}
          if not HIDLinkUsage(FirstAlias,LastAlias,Alias) then Exit;

          {Increment Usage}
          Inc(Usage.AliasCount);
         end
        else
         begin
          {Increment Usages}
          Inc(UsageIndex);
          Inc(UsageCount);

          {Allocate Usage}
          Usage:=HIDParserAllocateUsage(Device,nil,State,UsageIndex);
          if Usage = nil then Exit;

          {Link Usage}
          if not HIDLinkUsage(FirstUsage,LastUsage,Usage) then Exit;

          {Reset Usage}
          if not State.Delimiter then Usage:=nil;
         end;
       end;
      HID_TAG_LOCAL_DESIGNATOR_INDEX:begin
        {Update State}
        State.DesignatorIndex:=Data;
        State.DesignatorCount:=1;
       end;
      HID_TAG_LOCAL_DESIGNATOR_MINIMUM:begin
        {Update State}
        State.DesignatorIndex:=Data;
        State.DesignatorCount:=0;
       end;
      HID_TAG_LOCAL_DESIGNATOR_MAXIMUM:begin
        {Update State}
        State.DesignatorCount:=(Data - State.DesignatorIndex) + 1;
       end;
      HID_TAG_LOCAL_STRING_INDEX:begin
        {Update State}
        State.StringIndex:=Data;
        State.StringCount:=1;
       end;
      HID_TAG_LOCAL_STRING_MINIMUM:begin
        {Update State}
        State.StringIndex:=Data;
        State.StringCount:=0;
       end;
      HID_TAG_LOCAL_STRING_MAXIMUM:begin
        {Update State}
        State.StringCount:=(Data - State.StringIndex) + 1;
       end;
      HID_TAG_LOCAL_DELIMITER:begin
        {Check Data}
        if Data = HID_LOCAL_DELIMITER_OPEN then
         begin
          {Check State}
          if State.Delimiter then Exit;

          {Update State (Start Delimiter)}
          State.Delimiter:=True;

          {Reset Aliases}
          AliasIndex:=-1;

          {Reset Usage}
          Usage:=nil;
         end
        else if Data = HID_LOCAL_DELIMITER_CLOSE then
         begin
          {Check State}
          if not State.Delimiter then Exit;

          {Update State (End Delimiter)}
          State.Delimiter:=False;

          {Check Aliases}
          if (Usage <> nil) and (Usage.AliasCount > 0) then
           begin
            {Allocate Aliases}
            Usage.Aliases:=AllocMem(Usage.AliasCount * SizeOf(PHIDUsage));
            if Usage.Aliases = nil then Exit;

            {Assign Aliases}
            while FirstAlias <> nil do
             begin
              {Get Alias}
              Alias:=FirstAlias.Usage;

              {Update Aliases}
              Usage.Aliases[Alias.Index]:=Alias;

              {Unlink Alias}
              if not HIDUnlinkUsage(FirstAlias,LastAlias,FirstAlias) then Exit;
             end;

            {Reset Aliases}
            AliasIndex:=-1;

            {Reset Usage}
            Usage:=nil;
           end;
         end;
       end;

      {Long Tag}
      HID_TAG_LONG:begin
        {Long Item is HID_SIZE_2 (Followed by 1 byte DataSize / 1 byte LongItemTag / n bytes Data)}
        {Skip Long Item Data}
        Inc(Offset,Data and $FF);
       end;
     end;
    end;

   {Return Result}
   Result:=True;
  finally
   {Check Failure}
   if not Result then Completed:=True;
  end;
 end;

begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Set Defaults}
 Collections:=nil;
 Count:=0;

 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if MutexLock(Device.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Descriptor}
    if Device.Descriptor = nil then Exit;

    {Check Size}
    if Device.DescriptorSize = 0 then Exit;

    {$IFDEF HID_DEBUG}
    if HID_LOG_ENABLED then HIDLogDebug(Device,'Parsing Collections (Descriptor Size=' + IntToStr(Device.DescriptorSize) + ')');
    {$ENDIF}

    {Count Top Level Collections}
    Count:=HIDParserCountCollections(Device,nil);
    if Count = 0 then
     begin
      if HID_LOG_ENABLED then HIDLogError(Device,' Failed to locate any top level collections in HID report descriptor');

      Result:=ERROR_NOT_SUPPORTED;
      Exit;
     end;

    {Set Result}
    Result:=ERROR_OPERATION_FAILED;
    try
     {Allocate Top Level Collections}
     Collections:=AllocMem(Count * SizeOf(PHIDCollection));
     if Collections = nil then Exit;

     {Allocate State}
     State:=AllocMem(SizeOf(THIDState));
     if State = nil then Exit;

     {Allocate Stack}
     Stack:=AllocMem(SizeOf(THIDStack));
     if Stack = nil then Exit;

     {Assign State}
     Stack.State:=State;

     {Set Defaults}
     Level:=0;
     Offset:=0;
     Completed:=False;
     AliasCount:=0;
     UsageCount:=0;
     ReportCount:=0;
     CollectionCount:=0;

     {Parse Top Level Collections}
     HIDParserParseInternal(nil);

     {$IFDEF HID_DEBUG}
     if HID_LOG_ENABLED then HIDLogDebug(Device,'Parsed '  + IntToStr(CollectionCount) + ' Collections, ' + IntToStr(ReportCount) + ' Reports, ' + IntToStr(UsageCount) + ' Usages and ' + IntToStr(AliasCount) + ' Aliases');
     {$ENDIF}

     {Return Result}
     Result:=ERROR_SUCCESS;
    finally
     {Free Stack}
     HIDParserFreeStack(Stack);

     {Check Result}
     if Result <> ERROR_SUCCESS then
      begin
       {Free Collections}
       FreeMem(Collections);

       {Set Defaults}
       Collections:=nil;
       Count:=0;
      end;
    end;
   finally
    {Release the Lock}
    MutexUnlock(Device.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_OPERATION_FAILED;
  end;
end;

{==============================================================================}

function HIDParserFreeCollections(Collections:PHIDCollections;Count:LongWord):LongWord;
{Free the collections, reports and usages parsed from a HID report descriptor}
{Collections: A pointer to the top level collections array to be freed}
{Count: The number of top level collections in the array}
{Return: ERROR_SUCCESS if completed or another error code on failure}

var
 AliasCount:LongWord;
 UsageCount:LongWord;
 ReportCount:LongWord;
 CollectionCount:LongWord;

 function HIDParserFreeInternal(Parent:PHIDCollection):Boolean;
 var
  AliasIndex:LongWord;
  UsageIndex:LongWord;
  ReportIndex:LongWord;
  CollectionIndex:LongWord;

  Alias:PHIDUsage;
  Usage:PHIDUsage;
  Report:PHIDReport;
  Collection:PHIDCollection;
 begin
  {}
  Result:=False;

  {Check Parent}
  if Parent = nil then Exit;

  {Check Collections}
  if Parent.CollectionCount > 0 then
   begin
    for CollectionIndex:=0 to Parent.CollectionCount - 1 do
     begin
      Collection:=Parent.Collections[CollectionIndex];
      if Collection <> nil then
       begin
        {Free Collection}
        if not HIDParserFreeInternal(Collection) then Exit;
       end;
     end;

    {Free Collections}
    FreeMem(Parent.Collections);
   end;

  {Check Reports}
  if Parent.ReportCount > 0 then
   begin
    for ReportIndex:=0 to Parent.ReportCount - 1 do
     begin
      Report:=Parent.Reports[ReportIndex];
      if Report <> nil then
       begin
        {Check Usages}
        if Report.UsageCount > 0 then
         begin
          for UsageIndex:=0 to Report.UsageCount - 1 do
           begin
            Usage:=Report.Usages[UsageIndex];
            if Usage <> nil then
             begin
              {Check Aliases}
              if Usage.AliasCount > 0 then
               begin
                for AliasIndex:=0 to Usage.AliasCount - 1 do
                 begin
                  Alias:=Usage.Aliases[AliasIndex];
                  if Alias <> nil then
                   begin
                    {Free Alias}
                    FreeMem(Alias);

                    {Update Count}
                    Inc(AliasCount);
                   end;
                 end;

                {Free Aliases}
                FreeMem(Usage.Aliases);
               end;

              {Free Usage}
              FreeMem(Usage);

              {Update Count}
              Inc(UsageCount);
             end;
           end;

          {Free Usages}
          FreeMem(Report.Usages);
         end;

        {Free Report}
        FreeMem(Report);

        {Update Count}
        Inc(ReportCount);
       end;
     end;

    {Free Reports}
    FreeMem(Parent.Reports);
   end;

  {Free Collection}
  FreeMem(Parent);

  {Update Count}
  Inc(CollectionCount);

  Result:=True;
 end;

var
 CollectionIndex:LongWord;
 Collection:PHIDCollection;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Collections}
 if Collections = nil then Exit;

 {Check Count}
 if Count = 0 then Exit;

 {$IFDEF HID_DEBUG}
 if HID_LOG_ENABLED then HIDLogDebug(nil,'Freeing Collections (Count=' + IntToStr(Count) + ')');
 {$ENDIF}

 {Set Defaults}
 AliasCount:=0;
 UsageCount:=0;
 ReportCount:=0;
 CollectionCount:=0;

 {Check Collections}
 for CollectionIndex:=0 to Count - 1 do
  begin
   Collection:=Collections[CollectionIndex];
   if Collection <> nil then
    begin
     {Free Collection}
     if not HIDParserFreeInternal(Collection) then Exit;
    end;
  end;

 {Free Collections}
 FreeMem(Collections);

 {$IFDEF HID_DEBUG}
 if HID_LOG_ENABLED then HIDLogDebug(nil,'Freed '  + IntToStr(CollectionCount) + ' Collections, ' + IntToStr(ReportCount) + ' Reports, ' + IntToStr(UsageCount) + ' Usages and ' + IntToStr(AliasCount) + ' Aliases');
 {$ENDIF}

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function HIDParserCountCollections(Device:PHIDDevice;Parent:PHIDCollection):LongWord;
{Count the number of collections found in the HID report descriptor of the provided device}
{Device: The HID device to count collections for}
{Parent: The parent HID collection, if supplied count child collections else count top level collections}
{Return: The number of collections found, 0 if none for or on error}
var
 Tag:Byte;
 Len:Byte;
 Item:PByte;
 Data:LongInt;
 Count:LongWord;
 Level:LongWord;
 Offset:LongWord;
begin
 {}
 Result:=0;

 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if MutexLock(Device.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Check Descriptor}
  if Device.Descriptor = nil then Exit;

  {Check Size}
  if Device.DescriptorSize = 0 then Exit;

  {$IFDEF HID_DEBUG}
  if HID_LOG_ENABLED then HIDLogDebug(Device,'Counting Collections (Descriptor Size=' + IntToStr(Device.DescriptorSize) + ')');
  if HID_LOG_ENABLED and (Parent <> nil) then HIDLogDebug(Device,'                     (Parent Usage=' + IntToHex(Parent.Page,4) + IntToHex(Parent.Usage,4) + ')');
  {$ENDIF}

  {Set Defaults}
  Count:=0;
  Level:=0;

  {Get Offset}
  Offset:=0;
  if Parent <> nil then Offset:=Parent.Start;

  while Offset < Device.DescriptorSize do
   begin
    {Get Item}
    Item:=PByte(Device.Descriptor + Offset);

    {Get Length and Tag}
    Len:=Item[0] and HID_SIZE_MASK;
    Tag:=Item[0] and HID_TAG_MASK;

    {Get Data}
    case Len of
     HID_SIZE_0:begin
       {0 bytes}
       Data:=0;

       Inc(Offset,1);
      end;
     HID_SIZE_1:begin
       {1 byte}
       Data:=Item[1];

       Inc(Offset,2);
      end;
     HID_SIZE_2:begin
       {2 bytes}
       Data:=Item[1] or (Item[2] shl 8);

       Inc(Offset,3);
      end;
     HID_SIZE_4:begin
       {4 bytes}
       Data:=Item[1] or (Item[2] shl 8) or (Item[3] shl 16) or (Item[4] shl 24);

       Inc(Offset,5);
      end;
    end;

    {Check Tag}
    case Tag of
     {Main Item Tags}
     HID_TAG_MAIN_COLLECTION:begin
       {Check Level}
       if Level = 0 then Inc(Count);

       {Increment Level}
       Inc(Level);
      end;
     HID_TAG_MAIN_END_COLLECTION:begin
       {Check Level}
       if Level = 0 then Break;

       {Decrement Level}
       Dec(Level);
      end;

     {Long Tag}
     HID_TAG_LONG:begin
       {Long Item is HID_SIZE_2 (Followed by 1 byte DataSize / 1 byte LongItemTag / n bytes Data)}
       {Skip Long Item Data}
       Inc(Offset,Data and $FF);
      end;
    end;
   end;

  {$IFDEF HID_DEBUG}
  if HID_LOG_ENABLED then HIDLogDebug(Device,'Counted '  + IntToStr(Count) + ' Collections');
  {$ENDIF}

  {Return Result}
  Result:=Count;
 finally
  {Release the Lock}
  MutexUnlock(Device.Lock);
 end;
end;

{==============================================================================}

function HIDParserCountReports(Device:PHIDDevice;Collection:PHIDCollection):LongWord;
{Count the number of reports found in the HID report descriptor of the supplied device and collection}
{Device: The HID device to count reports for}
{Collection: The HID collection to count reports for}
{Return: The number of reports found, 0 if none for or on error}
var
 Tag:Byte;
 Len:Byte;
 Item:PByte;
 Data:LongInt;
 Count:LongWord;
 Level:LongWord;
 Offset:LongWord;
begin
 {}
 Result:=0;

 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Collection}
 if Collection = nil then Exit;

 {Acquire the Lock}
 if MutexLock(Device.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Check Descriptor}
  if Device.Descriptor = nil then Exit;

  {Check Size}
  if Device.DescriptorSize = 0 then Exit;

  {$IFDEF HID_DEBUG}
  if HID_LOG_ENABLED then HIDLogDebug(Device,'Counting Reports (Descriptor Size=' + IntToStr(Device.DescriptorSize) + ' Collection Usage=' + IntToHex(Collection.Page,4) + IntToHex(Collection.Usage,4) + ')');
  {$ENDIF}

  {Set Defaults}
  Count:=0;
  Level:=0;

  {Get Offset}
  Offset:=Collection.Start;

  while Offset < Device.DescriptorSize do
   begin
    {Get Item}
    Item:=PByte(Device.Descriptor + Offset);

    {Get Length and Tag}
    Len:=Item[0] and HID_SIZE_MASK;
    Tag:=Item[0] and HID_TAG_MASK;

    {Get Data}
    case Len of
     HID_SIZE_0:begin
       {0 bytes}
       Data:=0;

       Inc(Offset,1);
      end;
     HID_SIZE_1:begin
       {1 byte}
       Data:=Item[1];

       Inc(Offset,2);
      end;
     HID_SIZE_2:begin
       {2 bytes}
       Data:=Item[1] or (Item[2] shl 8);

       Inc(Offset,3);
      end;
     HID_SIZE_4:begin
       {4 bytes}
       Data:=Item[1] or (Item[2] shl 8) or (Item[3] shl 16) or (Item[4] shl 24);

       Inc(Offset,5);
      end;
    end;

    {Check Tag}
    case Tag of
     {Main Item Tags}
     HID_TAG_MAIN_INPUT,
     HID_TAG_MAIN_OUTPUT,
     HID_TAG_MAIN_FEATURE:begin
       {Check Level}
       if Level = 0 then Inc(Count);
      end;
     HID_TAG_MAIN_COLLECTION:begin
       {Increment Level}
       Inc(Level);
      end;
     HID_TAG_MAIN_END_COLLECTION:begin
       {Check Level}
       if Level = 0 then Break;

       {Decrement Level}
       Dec(Level);
      end;

     {Long Tag}
     HID_TAG_LONG:begin
       {Long Item is HID_SIZE_2 (Followed by 1 byte DataSize / 1 byte LongItemTag / n bytes Data)}
       {Skip Long Item Data}
       Inc(Offset,Data and $FF);
      end;
    end;
   end;

  {$IFDEF HID_DEBUG}
  if HID_LOG_ENABLED then HIDLogDebug(Device,'Counted '  + IntToStr(Count) + ' Reports');
  {$ENDIF}

  {Return Result}
  Result:=Count;
 finally
  {Release the Lock}
  MutexUnlock(Device.Lock);
 end;
end;

{==============================================================================}

function HIDParserCountUsages(Device:PHIDDevice;Report:PHIDReport):LongWord;
{Count the number of usages found in the HID report descriptor for the supplied device and report}
{Device: The HID device to count usages for}
{Report: The HID report to count usages for}
{Return: The number of usages found, 0 if none for or on error}
var
 Tag:Byte;
 Len:Byte;
 Item:PByte;
 Data:LongInt;
 Index:LongWord;
 Count:LongWord;
 Level:LongWord;
 Offset:LongWord;
 Delimiter:Boolean;
 AliasCount:LongWord;
 UsageCount:LongWord;
begin
 {}
 Result:=0;

 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Report}
 if Report = nil then Exit;

 {Acquire the Lock}
 if MutexLock(Device.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Check Descriptor}
  if Device.Descriptor = nil then Exit;

  {Check Size}
  if Device.DescriptorSize = 0 then Exit;

  {Check Collection}
  if Report.Collection = nil then Exit;

  {$IFDEF HID_DEBUG}
  if HID_LOG_ENABLED then HIDLogDebug(Device,'Counting Usages (Descriptor Size=' + IntToStr(Device.DescriptorSize) + ' Report Id=' + IntToStr(Report.Id) + ' Type=' + IntToStr(Report.Kind) + ')');
  {$ENDIF}

  {Set Defaults}
  Index:=0; {Index starts at 0 as the first report begins at the start of the collection}
  Count:=0;
  Level:=0;
  Delimiter:=False;
  AliasCount:=0;
  UsageCount:=0;

  {Get Offset}
  Offset:=Report.Collection.Start;

  while Offset < Device.DescriptorSize do
   begin
    {Get Item}
    Item:=PByte(Device.Descriptor + Offset);

    {Get Length and Tag}
    Len:=Item[0] and HID_SIZE_MASK;
    Tag:=Item[0] and HID_TAG_MASK;

    {Get Data}
    case Len of
     HID_SIZE_0:begin
       {0 bytes}
       Data:=0;

       Inc(Offset,1);
      end;
     HID_SIZE_1:begin
       {1 byte}
       Data:=Item[1];

       Inc(Offset,2);
      end;
     HID_SIZE_2:begin
       {2 bytes}
       Data:=Item[1] or (Item[2] shl 8);

       Inc(Offset,3);
      end;
     HID_SIZE_4:begin
       {4 bytes}
       Data:=Item[1] or (Item[2] shl 8) or (Item[3] shl 16) or (Item[4] shl 24);

       Inc(Offset,5);
      end;
    end;

    {Check Tag}
    case Tag of
     {Main Item Tags}
     HID_TAG_MAIN_INPUT,
     HID_TAG_MAIN_OUTPUT,
     HID_TAG_MAIN_FEATURE:begin
       {Check Level}
       if Level = 0 then Inc(Index);
      end;
     HID_TAG_MAIN_COLLECTION:begin
       {Increment Level}
       Inc(Level);
      end;
     HID_TAG_MAIN_END_COLLECTION:begin
       {Check Level}
       if Level = 0 then Break;

       {Decrement Level}
       Dec(Level);
      end;

     {Local Item Tags}
     HID_TAG_LOCAL_USAGE,
     HID_TAG_LOCAL_USAGE_MAXIMUM:begin
       {Check Level}
       if (Level = 0) and (Index = Report.Index) then
        begin
         if Delimiter then Inc(Count) else Inc(UsageCount);
        end;
      end;
     HID_TAG_LOCAL_DELIMITER:begin
       {Check Data}
       if Data = HID_LOCAL_DELIMITER_OPEN then
        begin
         {Check Delimiter}
         if Delimiter then Exit;

         {Start Delimiter}
         Delimiter:=True;

         {Reset Count}
         Count:=0;
        end
       else if Data = HID_LOCAL_DELIMITER_CLOSE then
        begin
         {Check Delimiter}
         if not Delimiter then Exit;

         {End Delimiter}
         Delimiter:=False;

         {Check Count}
         if Count > 0 then
          begin
           Inc(UsageCount);
           Inc(AliasCount,Count - 1);
          end;
        end;
      end;

     {Long Tag}
     HID_TAG_LONG:begin
       {Long Item is HID_SIZE_2 (Followed by 1 byte DataSize / 1 byte LongItemTag / n bytes Data)}
       {Skip Long Item Data}
       Inc(Offset,Data and $FF);
      end;
    end;
   end;

  {$IFDEF HID_DEBUG}
  if HID_LOG_ENABLED then HIDLogDebug(Device,'Counted '  + IntToStr(UsageCount) + ' Usages and ' + IntToStr(AliasCount) + ' Aliases');
  {$ENDIF}

  {Return Result}
  Result:=UsageCount;
 finally
  {Release the Lock}
  MutexUnlock(Device.Lock);
 end;
end;

{==============================================================================}

function HIDParserAllocateCollection(Device:PHIDDevice;Parent:PHIDCollection;State:PHIDState;Flags,Start:LongWord):PHIDCollection;
{Allocate a HID collection to contain a set of reports and usages from a HID report descriptor}
{Device: The HID device containing the collection}
{Parent: The HID collection containing the collection (or nil for a top level collection)}
{State: The current HID parser state}
{Flags: The flags for the collection from the HID report descriptor}
{Start: The starting byte offset of the collection in the HID report descriptor}
{Return: A pointer to the HID collection or nil on error}
var
 Collection:PHIDCollection;
begin
 {}
 Result:=nil;

 {Check Device}
 if Device = nil then Exit;

 {Check State}
 if State = nil then Exit;

 {$IFDEF HID_DEBUG}
 if HID_LOG_ENABLED then HIDLogDebug(Device,'Allocating Collection (Usage=' + IntToHex(State.UsagePage,4) + IntToHex(State.Usage,4) + ' Flags=' + IntToHex(Flags,2) + ' Start=' + IntToStr(Start) + ')');
 {$ENDIF}

 {Allocate Collection}
 Collection:=AllocMem(SizeOf(THIDCollection));
 if Collection = nil then Exit;
 try
  {Update Collection}
  Collection.Page:=State.UsagePage;
  Collection.Usage:=State.Usage;
  Collection.Flags:=Flags;
  Collection.Start:=Start;
  Collection.Parent:=Parent;
  Collection.Device:=Device;

  {Count Collections}
  Collection.CollectionCount:=HIDParserCountCollections(Device,Collection);

  {Allocate Collections}
  if Collection.CollectionCount > 0 then
   begin
    Collection.Collections:=AllocMem(Collection.CollectionCount * SizeOf(PHIDCollection));
    if Collection.Collections = nil then Exit;
   end;

  {Count Reports}
  Collection.ReportCount:=HIDParserCountReports(Device,Collection);

  {Allocate Reports}
  if Collection.ReportCount > 0 then
   begin
    Collection.Reports:=AllocMem(Collection.ReportCount * SizeOf(PHIDReport));
    if Collection.Reports = nil then Exit;
   end;

  {$IFDEF HID_DEBUG}
  if HID_LOG_ENABLED then HIDLogDebug(Device,'Allocated Collection (Reports=' + IntToStr(Collection.ReportCount) + ' Collections=' + IntToStr(Collection.CollectionCount) + ')');
  {$ENDIF}

  {Return Result}
  Result:=Collection;
 finally
  {Check Failure}
  if Result = nil then FreeMem(Collection);
 end;
end;

{==============================================================================}

function HIDParserAllocateReport(Device:PHIDDevice;Collection:PHIDCollection;State:PHIDState;Kind:Byte;Flags,Index:LongWord):PHIDReport;
{Allocate a HID report to contain a set of usages from a HID report descriptor}
{Device: The HID device containing the report}
{Collection: The HID collection containing the report}
{State: The current HID parser state}
{Kind: The report kind (eg HID_REPORT_INPUT)}
{Flags: The flags for the report from the HID report descriptor}
{Index: The index of this report in the collection (First report is 0)}
{Return: A pointer to the HID report or nil on error}
var
 Report:PHIDReport;
begin
 {}
 Result:=nil;

 {Check Device}
 if Device = nil then Exit;

 {Check Collection}
 if Collection = nil then Exit;

 {Check State}
 if State = nil then Exit;

 {$IFDEF HID_DEBUG}
 if HID_LOG_ENABLED then HIDLogDebug(Device,'Allocating Report (Id=' + IntToStr(State.ReportId) + ' Type=' + IntToStr(Kind) + ' Index=' + IntToStr(Index) + ')');
 {$ENDIF}

 {Allocate Report}
 Report:=AllocMem(SizeOf(THIDReport));
 if Report = nil then Exit;
 try
  {Update Report}
  Report.Id:=State.ReportId;
  Report.Kind:=Kind;
  Report.Flags:=Flags;
  Report.Size:=State.ReportSize;
  Report.Count:=State.ReportCount;
  Report.Index:=Index;
  Report.Collection:=Collection;

  {Count Usages}
  Report.UsageCount:=HIDParserCountUsages(Device,Report);

  {Allocate Reports}
  if Report.UsageCount > 0 then
   begin
    Report.Usages:=AllocMem(Report.UsageCount * SizeOf(PHIDUsage));
    if Report.Usages = nil then Exit;
   end;

  {$IFDEF HID_DEBUG}
  if HID_LOG_ENABLED then HIDLogDebug(Device,'Allocated Report (Size=' + IntToStr(Report.Size) + ' Count=' + IntToStr(Report.Count) + ' Usages=' + IntToStr(Report.UsageCount) + ')');
  {$ENDIF}

  {Return Result}
  Result:=Report;
 finally
  {Check Failure}
  if Result = nil then FreeMem(Report);
 end;
end;

{==============================================================================}

function HIDParserAllocateUsage(Device:PHIDDevice;Report:PHIDReport;State:PHIDState;Index:LongWord):PHIDUsage;
{Allocate a HID usage from a HID report descriptor}
{Device: The HID device containing the usage}
{Report: The HID report containing the usage}
{State: The current HID parser state}
{Index: The index of this usage in the report (First usage is 0)}
{Return: A pointer to the HID usage or nil on error}
var
 Usage:PHIDUsage;
begin
 {}
 Result:=nil;

 {Check Device}
 if Device = nil then Exit;

 {Check Report}
 {if Report = nil then Exit;} {Report can be nil}

 {Check State}
 if State = nil then Exit;

 {$IFDEF HID_DEBUG}
 if HID_LOG_ENABLED then HIDLogDebug(Device,'Allocating Usage (Page=' + IntToHex(State.UsagePage,4) + ' Usage=' + IntToHex(State.Usage,4) + ' Index=' + IntToStr(Index) + ')');
 {$ENDIF}

 {Allocate Usage}
 Usage:=AllocMem(SizeOf(THIDUsage));
 if Usage = nil then Exit;
 try
  {Update Usage}
  Usage.Page:=State.UsagePage;
  if (State.Usage and $FFFF0000) <> 0 then Usage.Page:=(State.Usage and $FFFF0000) shr 16;
  Usage.Usage:=State.Usage;
  Usage.Count:=State.UsageCount;
  Usage.Index:=Index;
  Usage.Report:=Report;

  {$IFDEF HID_DEBUG}
  if HID_LOG_ENABLED then HIDLogDebug(Device,'Allocated Usage (Page=' + IntToHex(Usage.Page,4) + ' Usage=' + IntToHex(Usage.Usage,4) + ' Count=' + IntToStr(Usage.Count) + ')');
  {$ENDIF}

  {Return Result}
  Result:=Usage;
 finally
  {Check Failure}
  if Result = nil then FreeMem(Usage);
 end;
end;

{==============================================================================}

function HIDParserUpdateUsage(Device:PHIDDevice;Report:PHIDReport;State:PHIDState;Usage:PHIDUsage):Boolean;
{Update a HID usage from a HID report descriptor}
{Device: The HID device containing the usage}
{Report: The HID report containing the usage}
{State: The current HID parser state}
{Usage: The HID usage to update}
{Return: True if completed or False on error}

{Note: As usages must precede the main item they relate to in the HID report descriptor they need to be allocated}
{      before all the required information is known, this function updates the usage after the main item is found}
var
 Alias:PHIDUsage;
 AliasIndex:LongWord;
begin
 {}
 Result:=False;

 {Check Device}
 if Device = nil then Exit;

 {Check Report}
 if Report = nil then Exit;

 {Check State}
 if State = nil then Exit;

 {Check Usage}
 if Usage = nil then Exit;

 {$IFDEF HID_DEBUG}
 if HID_LOG_ENABLED then HIDLogDebug(Device,'Updating Usage (Page=' + IntToHex(Usage.Page,4) + ' Usage=' + IntToHex(Usage.Usage,4) + ' Count=' + IntToStr(Usage.Count) + ')');
 {$ENDIF}

 {Update Usage}
 Usage.StringIndex:=State.StringIndex;
 Usage.StringCount:=State.StringCount;
 Usage.DesignatorIndex:=State.DesignatorIndex;
 Usage.DesignatorCount:=State.DesignatorCount;

 Usage.LogicalMinimum:=State.LogicalMinimum;
 Usage.LogicalMaximum:=State.LogicalMaximum;
 Usage.PhysicalMinimum:=State.PhysicalMinimum;
 Usage.PhysicalMaximum:=State.PhysicalMaximum;
 Usage.UnitType:=State.UnitType;
 Usage.UnitExponent:=State.UnitExponent;

 Usage.Report:=Report;

 {Update Aliases}
 if Usage.AliasCount > 0 then
  begin
   for AliasIndex:=0 to Usage.AliasCount - 1 do
    begin
     Alias:=Usage.Aliases[AliasIndex];
     if Alias <> nil then
      begin
       {Update Alias}
       if not HIDParserUpdateUsage(Device,Report,State,Alias) then Exit;
      end;
    end;
  end;

 {Return Result}
 Result:=True;
end;

{==============================================================================}

function HIDParserPopStack(var Stack:PHIDStack;var State:PHIDState):LongWord;
{Replace the current HID parser state with the top item from the parser stack}
{Stack: The HID parser stack}
{State: The HID parser state to replace}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Current:PHIDStack;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Stack}
 if Stack = nil then Exit;

 {Check State}
 if State = nil then Exit;

 {Check Next}
 if Stack.Next = nil then Exit;

 {Get Current}
 Current:=Stack;

 {Replace Stack}
 Stack:=Current.Next;

 {Replace State}
 State:=Stack.State;

 {Free Current State}
 FreeMem(Current.State);

 {Free Current Stack}
 FreeMem(Current);

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function HIDParserPushStack(Stack:PHIDStack):LongWord;
{Place a copy of the current HID parser state on top of the parser stack}
{Stack: The HID parser stack}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Next:PHIDStack;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Stack}
 if Stack = nil then Exit;

 {Allocate Next Stack}
 Next:=AllocMem(SizeOf(THIDStack));
 if Next = nil then Exit;

 {Allocate Next State}
 Next.State:=AllocMem(SizeOf(THIDState));
 if Next.State = nil then Exit;

 {Copy State}
 Next.State^:=Stack.State^;

 {Place on Stack}
 Next.Next:=Stack.Next;
 Stack.Next:=Next;

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function HIDParserFreeStack(Stack:PHIDStack):LongWord;
{Free the HID parser stack and state}
{Stack: The HID parser stack}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Next:PHIDStack;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Stack}
 if Stack = nil then Exit;

 {Free Stack}
 while Stack <> nil do
  begin
   {Save Next}
   Next:=Stack.Next;

   {Free State}
   FreeMem(Stack.State);

   {Free Stack}
   FreeMem(Stack);

   {Get Next}
   Stack:=Next;
  end;

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function HIDParserResetState(State:PHIDState):LongWord;
{Clear the Local and Global HID parser state}
{State: The HID parser state to reset}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check State}
 if State = nil then Exit;

 {Reset Local and Global State}
 FillChar(State^,SizeOf(THIDState),0);

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function HIDParserCleanState(State:PHIDState):LongWord;
{Clear the Local HID parser state}
{State: The HID parser state to clean}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check State}
 if State = nil then Exit;

 {Reset Local State}
 State.Usage:=0;
 State.UsageCount:=0;
 State.DesignatorIndex:=0;
 State.DesignatorCount:=0;
 State.StringIndex:=0;
 State.StringCount:=0;
 State.Delimiter:=False;

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function HIDFindCollection(Device:PHIDDevice;Page,Usage:Word):PHIDCollection;
{Find the first HID collection matching the specified page and usage}
{Device: The HID device to find collections from}
{Page: The HID Usage Page to match (eg HID_PAGE_GENERIC_DESKTOP)}
{Usage: The HID Usage to match (eg HID_DESKTOP_MOUSE)}
{Return: A pointer to the first matching collection or nil if not matched}

 function HIDFindInternal(Parent:PHIDCollection):PHIDCollection;
 var
  Index:LongWord;
  Collection:PHIDCollection;
 begin
  {}
  Result:=nil;

  {Check Parent}
  if Parent = nil then Exit;
  if Parent.Collections = nil then Exit;

  {Find Collection}
  if Parent.CollectionCount > 0 then
   begin
    for Index:=0 to Parent.CollectionCount - 1 do
     begin
      Collection:=Parent.Collections[Index];
      if Collection <> nil then
       begin
        {Check Collection}
        if (Collection.Page = Page) and (Collection.Usage = Usage) then
         begin
          Result:=Collection;
          Exit;
         end;
       end;
     end;
   end;
 end;

var
 Index:LongWord;
 Collection:PHIDCollection;
begin
 {}
 Result:=nil;

 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if MutexLock(Device.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Check Collections}
  if Device.Collections = nil then Exit;

  {Find Collection}
  if Device.CollectionCount > 0 then
   begin
    for Index:=0 to Device.CollectionCount - 1 do
     begin
      Collection:=Device.Collections[Index];
      if Collection <> nil then
       begin
        {Check Collection}
        if (Collection.Page = Page) and (Collection.Usage = Usage) then
         begin
          Result:=Collection;
          Exit;
         end;

        {Check Collections}
        Result:=HIDFindInternal(Collection);
        if Result <> nil then Exit;
       end;
     end;
   end;
 finally
  {Release the Lock}
  MutexUnlock(Device.Lock);
 end;
end;

{==============================================================================}

function HIDFindReportIds(Device:PHIDDevice;Collection:PHIDCollection;var MinId,MaxId:Byte):LongWord;
{Find the minimum and maximum report ids contained in the specified HID collection or all collections}
{Device: The HID device to find report ids from}
{Collection: The HID collection to find report ids from (or nil to find from all collections)}
{MinId: A variable to receive the minimum report id number}
{MaxId: A variable to receive the maximum report id number}
{Return: ERROR_SUCCESS if completed or another error code on failure}

 function HIDFindInternal(Parent:PHIDCollection):Boolean;
 var
   Index:LongWord;
   Report:PHIDReport;
 begin
  {}
  Result:=False;

  {Check Parent}
  if Parent = nil then Exit;

  {Check Reports}
  if Parent.ReportCount > 0 then
   begin
    for Index:=0 to Parent.ReportCount - 1 do
     begin
      Report:=Parent.Reports[Index];
      if Report <> nil then
       begin
        {Check Report}
        if Report.Id < MinId then
         begin
          MinId:=Report.Id;
         end;

        if Report.Id > MaxId then
         begin
          MaxId:=Report.Id;
         end;
       end;
     end;
   end;

  {Check Collections}
  if Parent.CollectionCount > 0 then
   begin
    for Index:=0 to Parent.CollectionCount - 1 do
     begin
      if Parent.Collections[Index] <> nil then
       begin
        {Check Collection}
        if not HIDFindInternal(Parent.Collections[Index]) then Exit;
       end;
     end;
   end;

  Result:=True;
 end;

var
 Index:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Set Defaults}
 MinId:=$FF;
 MaxId:=$00;

 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if MutexLock(Device.Lock) = ERROR_SUCCESS then
  begin
   try
    {Set Result}
    Result:=ERROR_OPERATION_FAILED;

    {Find Report Ids}
    if Collection = nil then
     begin
      {Check Collections}
      if Device.Collections = nil then Exit;

      {Check Collections}
      if Device.CollectionCount > 0 then
       begin
        for Index:=0 to Device.CollectionCount - 1 do
         begin
          if Device.Collections[Index] <> nil then
           begin
            {Check Collection}
            if not HIDFindInternal(Device.Collections[Index]) then Exit;
           end;
         end;
       end;
     end
    else
     begin
      {Check Collection}
      if not HIDFindInternal(Collection) then Exit;
     end;

    {Check Result}
    if MinId > MaxId then MinId:=MaxId;

    {$IFDEF HID_DEBUG}
    if HID_LOG_ENABLED then HIDLogDebug(Device,'Found Report Ids (Minimum=' + IntToStr(MinId) + ' Maximum=' + IntToStr(MaxId) + ')');
    {$ENDIF}

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    MutexUnlock(Device.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_OPERATION_FAILED;
  end;
end;

{==============================================================================}

function HIDAllocateDefinition(Device:PHIDDevice;Collection:PHIDCollection;Kind,Id:Byte):PHIDDefinition;
{Allocate a HID defintion to describe an input, output or feature report contained in the specified collection}
{Device: The HID device to create the report definition from}
{Collection: The HID collection to create the report definition from}
{Kind: The report kind to create a definition for (eg HID_REPORT_INPUT)}
{Id: The report id to create a definition for (must be less than or equal to the maximum report id)}
{Return: A pointer to the allocated definition or nil on error}

var
 Bits:LongWord;
 Definition:PHIDDefinition;

 function HIDLinkField(Field:PHIDField):Boolean;
 var
  Last:PHIDField;
 begin
  {}
  Result:=False;

  {Check Field}
  if Field = nil then Exit;

  {Check Definition}
  if Definition = nil then Exit;

  {Link Field}
  if Definition.Fields = nil then
   begin
    {Link First}
    Definition.Fields:=Field;
   end
  else
   begin
    {Find Last}
    Last:=Definition.Fields;
    while Last.Next <> nil do
     begin
      Last:=Last.Next;
     end;

    {Link Last}
    Last.Next:=Field;
   end;

  Result:=True;
 end;

 function HIDExponentMultiplier(Usage:PHIDUsage):Double;
 begin
  {}
  Result:=HID_GLOBAL_UNIT_MULTIPLIERS[0]; {Default to 10^0}

  {Check Usage}
  if Usage = nil then Exit;

  {Check Exponent}
  if Usage.UnitExponent > $F then Exit;

  Result:=HID_GLOBAL_UNIT_MULTIPLIERS[Usage.UnitExponent];
 end;

 function HIDAllocateInternal(Parent:PHIDCollection):Boolean;
 var
  Count:LongWord;
  Field:PHIDField;
  Usage:PHIDUsage;
  Report:PHIDReport;
  UsageIndex:LongWord;
  ReportIndex:LongWord;
  CollectionIndex:LongWord;
 begin
  {}
  Result:=False;

  {Check Parent}
  if Parent = nil then Exit;

  {Check Reports}
  if Parent.ReportCount > 0 then
   begin
    for ReportIndex:=0 to Parent.ReportCount - 1 do
     begin
      Report:=Parent.Reports[ReportIndex];
      if Report <> nil then
       begin
        {Check Report}
        if (Report.Kind = Kind) and (Report.Id = Id) then
         begin
          {$IFDEF HID_DEBUG}
          if HID_LOG_ENABLED then HIDLogDebug(Device,'Allocating Definition (Report Index=' + IntToStr(Report.Index) + ' Size=' + IntToStr(Report.Size) + ' Count=' + IntToStr(Report.Count) + ' Flags=' + IntToHex(Report.Flags,8) + ')');
          {$ENDIF}

          {Check Definition}
          if Definition = nil then
           begin
            {Allocate Definition}
            Definition:=AllocMem(SizeOf(THIDDefinition));
            if Definition = nil then Exit;

            {Update Definition}
            Definition.Id:=Id;
            Definition.Kind:=Kind;

            {Update Bits}
            if Id <> 0 then Inc(Bits,8);
           end;

          {Check Usages}
          if Report.UsageCount > 0 then
           begin
            {Data}
            for UsageIndex:=0 to Report.UsageCount - 1 do
             begin
              Usage:=Report.Usages[UsageIndex];
              if Usage <> nil then
               begin
                {Check Usage}
                if (Usage.Page = 0) or (Usage.Count = 0) then Exit;

                {$IFDEF HID_DEBUG}
                if HID_LOG_ENABLED then HIDLogDebug(Device,'Allocating Definition (Usage Index=' + IntToStr(Usage.Index) + ' Page=' + IntToHex(Usage.Page,4) + ' Usage=' + IntToHex(Usage.Usage,4) + ' Count=' + IntToStr(Usage.Count) + ')');
                {$ENDIF}

                {Check Flags}
                if (Report.Flags and (HID_MAIN_ITEM_CONSTANT or HID_MAIN_ITEM_VARIABLE)) <> 0 then
                 begin
                  {Constant or Variable}
                  for Count:=0 to Usage.Count - 1 do
                   begin
                    {Allocate Field}
                    Field:=AllocMem(SizeOf(THIDField));
                    if Field = nil then Exit;

                    {Update Field}
                    Field.Page:=Usage.Page;
                    Field.Usage:=Usage.Usage + Count;
                    Field.Count:=1;

                    Field.Flags:=Report.Flags;

                    Field.Size:=(Report.Size + 7) div 8;
                    Field.Bits:=Report.Size;
                    Field.Offset:=Bits div 8;
                    Field.Shift:=Bits mod 8;

                    Field.Minimum:=Usage.LogicalMinimum;
                    Field.Maximum:=Usage.LogicalMaximum;
                    if (Usage.PhysicalMinimum <> 0) or (Usage.PhysicalMaximum <> 0) then
                     begin
                      Field.Minimum:=Usage.PhysicalMinimum;
                      Field.Maximum:=Usage.PhysicalMaximum;
                     end;

                    if (Usage.PhysicalMinimum = Usage.PhysicalMaximum) or (Usage.LogicalMinimum = Usage.LogicalMaximum) then
                     begin
                      Field.Multiplier:=1;
                      Field.Resolution:=1;
                     end
                    else
                     begin
                      Field.Multiplier:=(Usage.PhysicalMaximum - Usage.PhysicalMinimum) / (Usage.LogicalMaximum - Usage.LogicalMinimum);
                      Field.Resolution:=(Usage.LogicalMaximum - Usage.LogicalMinimum) / ((Usage.PhysicalMaximum - Usage.PhysicalMinimum) * HIDExponentMultiplier(Usage));
                     end;

                    {Link Field}
                    if not HIDLinkField(Field) then
                     begin
                      {Free Field}
                      FreeMem(Field);
                      Exit;
                     end;

                    {$IFDEF HID_DEBUG}
                    if HID_LOG_ENABLED then HIDLogDebug(Device,'Allocated Field (Page=' + IntToHex(Field.Page,4) + ' Usage=' + IntToHex(Field.Usage,4) + ' Count=' + IntToStr(Field.Count) + ' Flags=' + IntToHex(Field.Flags,8) + ')');
                    if HID_LOG_ENABLED then HIDLogDebug(Device,'                (Size=' + IntToStr(Field.Size) + ' Bits=' + IntToStr(Field.Bits) + ' Offset=' + IntToStr(Field.Offset) + ' Shift=' + IntToStr(Field.Shift) + ')');
                    if HID_LOG_ENABLED then HIDLogDebug(Device,'                (Minimum=' + IntToStr(Field.Minimum) + ' Maximum=' + IntToStr(Field.Maximum) + ' Multiplier=' + FloatToStr(Field.Multiplier) + ' Resolution=' + FloatToStr(Field.Resolution) + ')');
                    {$ENDIF}

                    {Update Bits}
                    Inc(Bits,Report.Size);

                    {$IFDEF HID_DEBUG}
                    if HID_LOG_ENABLED then HIDLogDebug(Device,'Allocating Definition (Bits=' + IntToStr(Bits) + ')');
                    {$ENDIF}
                   end;
                 end
                else if (Report.Flags and (HID_MAIN_ITEM_CONSTANT or HID_MAIN_ITEM_VARIABLE)) = 0 then
                 begin
                  {Array}
                  for Count:=0 to Report.Count - 1 do
                   begin
                    {Allocate Field}
                    Field:=AllocMem(SizeOf(THIDField));
                    if Field = nil then Exit;

                    {Update Field}
                    Field.Page:=Usage.Page;
                    Field.Usage:=Usage.Usage;
                    Field.Count:=Usage.Count;

                    Field.Flags:=Report.Flags;

                    Field.Size:=(Report.Size + 7) div 8;
                    Field.Bits:=Report.Size;
                    Field.Offset:=Bits div 8;
                    Field.Shift:=Bits mod 8;

                    Field.Minimum:=Usage.LogicalMinimum;
                    Field.Maximum:=Usage.LogicalMaximum;
                    if (Usage.PhysicalMinimum <> 0) or (Usage.PhysicalMaximum <> 0) then
                     begin
                      Field.Minimum:=Usage.PhysicalMinimum;
                      Field.Maximum:=Usage.PhysicalMaximum;
                     end;

                    if (Usage.PhysicalMinimum = Usage.PhysicalMaximum) or (Usage.LogicalMinimum = Usage.LogicalMaximum) then
                     begin
                      Field.Multiplier:=1;
                      Field.Resolution:=1;
                     end
                    else
                     begin
                      Field.Multiplier:=(Usage.PhysicalMaximum - Usage.PhysicalMinimum) / (Usage.LogicalMaximum - Usage.LogicalMinimum);
                      Field.Resolution:=(Usage.LogicalMaximum - Usage.LogicalMinimum) / ((Usage.PhysicalMaximum - Usage.PhysicalMinimum) * HIDExponentMultiplier(Usage));
                     end;

                    {Link Field}
                    if not HIDLinkField(Field) then
                     begin
                      {Free Field}
                      FreeMem(Field);
                      Exit;
                     end;

                    {$IFDEF HID_DEBUG}
                    if HID_LOG_ENABLED then HIDLogDebug(Device,'Allocated Field (Page=' + IntToHex(Field.Page,4) + ' Usage=' + IntToHex(Field.Usage,4) + ' Count=' + IntToStr(Field.Count) + ' Flags=' + IntToHex(Field.Flags,8) + ')');
                    if HID_LOG_ENABLED then HIDLogDebug(Device,'                (Size=' + IntToStr(Field.Size) + ' Bits=' + IntToStr(Field.Bits) + ' Offset=' + IntToStr(Field.Offset) + ' Shift=' + IntToStr(Field.Shift) + ')');
                    if HID_LOG_ENABLED then HIDLogDebug(Device,'                (Minimum=' + IntToStr(Field.Minimum) + ' Maximum=' + IntToStr(Field.Maximum) + ' Multiplier=' + FloatToStr(Field.Multiplier) + ' Resolution=' + FloatToStr(Field.Resolution) + ')');
                    {$ENDIF}

                    {Update Bits}
                    Inc(Bits,Report.Size);

                    {$IFDEF HID_DEBUG}
                    if HID_LOG_ENABLED then HIDLogDebug(Device,'Allocating Definition (Bits=' + IntToStr(Bits) + ')');
                    {$ENDIF}
                   end;
                 end;
               end;
             end;
           end
          else
           begin
            {Padding}
            {Update Bits}
            Inc(Bits,Report.Size * Report.Count);

            {$IFDEF HID_DEBUG}
            if HID_LOG_ENABLED then HIDLogDebug(Device,'Allocating Definition (Bits=' + IntToStr(Bits) + ')');
            {$ENDIF}
           end;
         end;
       end;
     end;
   end;

  {Check Collections}
  if Parent.CollectionCount > 0 then
   begin
    for CollectionIndex:=0 to Parent.CollectionCount - 1 do
     begin
      if Parent.Collections[CollectionIndex] <> nil then
       begin
        {Check Collection}
        if not HIDAllocateInternal(Parent.Collections[CollectionIndex]) then Exit;
       end;
     end;
   end;

  Result:=True;
 end;

var
 Index:LongWord;
begin
 {}
 Result:=nil;

 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Collection}
 if Collection = nil then Exit;

 {$IFDEF HID_DEBUG}
 if HID_LOG_ENABLED then HIDLogDebug(Device,'Allocating Definition (Kind=' + IntToStr(Kind) + ' Id=' + IntToStr(Id) + ')');
 {$ENDIF}

 {Set Defaults}
 Bits:=0;
 Definition:=nil;

 {Acquire the Lock}
 if MutexLock(Device.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Check Collections}
  if Device.Collections = nil then Exit;

  {Check Collections}
  if Device.CollectionCount > 0 then
   begin
    for Index:=0 to Device.CollectionCount - 1 do
     begin
      if Device.Collections[Index] <> nil then
       begin
        {Check Collection}
        if not HIDAllocateInternal(Device.Collections[Index]) then Exit;
       end;
     end;
   end;

  {Check Definition}
  if Definition = nil then Exit;

  {Update Definition}
  Definition.Size:=(Bits + 7) div 8;

  {$IFDEF HID_DEBUG}
  if HID_LOG_ENABLED then HIDLogDebug(Device,'Allocated Definition (Size=' + IntToStr(Definition.Size) + ')');
  {$ENDIF}

  {Return Result}
  Result:=Definition;
 finally
  {Release the Lock}
  MutexUnlock(Device.Lock);
 end;
end;

{==============================================================================}

function HIDFreeDefinition(Definition:PHIDDefinition):LongWord;
{Free a HID defintion to describing an input, output or feature report}
{Definition: The HID definition to be freed}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Field:PHIDField;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Definition}
 if Definition = nil then Exit;

 {$IFDEF HID_DEBUG}
 if HID_LOG_ENABLED then HIDLogDebug(nil,'Freeing Definition (Kind=' + IntToStr(Definition.Kind) + ' Id=' + IntToStr(Definition.Id) + ')');
 {$ENDIF}

 {Free Fields}
 Field:=Definition.Fields;
 while Field <> nil do
  begin
   {Unlink Field}
   Definition.Fields:=Field.Next;

   {Free Field}
   FreeMem(Field);

   {Get Next Field}
   Field:=Definition.Fields;
  end;

 {Free Definition}
 FreeMem(Definition);

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function HIDInsertBitField(Field:PHIDField;Buffer:Pointer;Size:LongWord;Value:Boolean):LongWord;
{Insert a bit field value into a report buffer}
{Field: The field to insert into the report}
{Buffer: A pointer to the report buffer}
{Size: The size in bytes of the report buffer}
{Value: The value to insert into the buffer}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Data:LongWord;
begin
 {}
 {Get Data}
 if Value then Data:=1 else Data:=0;

 {Insert Field}
 Result:=HIDInsertUnsignedField(Field,Buffer,Size,Data);
end;

{==============================================================================}

function HIDInsertSignedField(Field:PHIDField;Buffer:Pointer;Size:LongWord;Value:LongInt):LongWord;
{Insert a signed field value into a report buffer}
{Field: The field to insert into the report}
{Buffer: A pointer to the report buffer}
{Size: The size in bytes of the report buffer}
{Value: The value to insert into the buffer}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=HIDInsertUnsignedField(Field,Buffer,Size,Value);
end;

{==============================================================================}

function HIDInsertUnsignedField(Field:PHIDField;Buffer:Pointer;Size,Value:LongWord):LongWord;
{Insert an unsigned field value into a report buffer}
{Field: The field to insert into the report}
{Buffer: A pointer to the report buffer}
{Size: The size in bytes of the report buffer}
{Value: The value to insert into the buffer}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Data:Pointer;
 Mask:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Field}
 if Field = nil then Exit;
 if Field.Size > 4 then Exit;

 {Check Buffer}
 if Buffer = nil then Exit;

 {Check Size}
 if Size < (Field.Offset + Field.Size) then Exit;

 {Set Result}
 Result:=ERROR_SUCCESS;

 {Check Bits}
 if Field.Bits = 0 then Exit;

 {Get Data}
 Data:=Pointer(Buffer + Field.Offset);

 {Get Mask}
 Mask:=(1 shl Min(Field.Bits,24)) - 1;

 {Set Value}
 PLongWord(Data)^:=(PLongWord(Data)^ and not(Mask shl Field.Shift)) or ((Value and Mask) shl Field.Shift);

 {Check Bits}
 if Field.Bits > 24 then
  begin
   {Get Data}
   Data:=PByte(Buffer + Field.Offset + 3);

   {Get Mask}
   Mask:=(1 shl (Field.Bits - 24)) - 1;

   {Set Remain}
   PLongWord(Data)^:=(PLongWord(Data)^ and not(Mask shl Field.Shift)) or (((Value shr 24) and Mask) shl Field.Shift);
  end;
end;

{==============================================================================}

function HIDExtractBitField(Field:PHIDField;Buffer:Pointer;Size:LongWord;var Value:Boolean):LongWord;
{Extract a bit field value from a report buffer}
{Field: The field to extract from the report}
{Buffer: A pointer to the report buffer}
{Size: The size in bytes of the report buffer}
{Value: A variable to receive the extracted value}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Data:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Set Default}
 Value:=False;

 {Check Field}
 if Field = nil then Exit;

 {Extract Field}
 Result:=HIDExtractUnsignedField(Field,Buffer,Size,Data);
 if (Result = ERROR_SUCCESS) and (Data <> 0) then Value:=True;
end;

{==============================================================================}

function HIDExtractSignedField(Field:PHIDField;Buffer:Pointer;Size:LongWord;var Value:LongInt):LongWord;
{Extract a signed field value from a report buffer}
{Field: The field to extract from the report}
{Buffer: A pointer to the report buffer}
{Size: The size in bytes of the report buffer}
{Value: A variable to receive the extracted value}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Data:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Set Default}
 Value:=0;

 {Check Field}
 if Field = nil then Exit;

 {Extract Field}
 Result:=HIDExtractUnsignedField(Field,Buffer,Size,Data);
 if Result = ERROR_SUCCESS then
  begin
   {Sign Extend}
   Value:=SignExtend32(Data,Field.Bits);
  end;
end;

{==============================================================================}

function HIDExtractUnsignedField(Field:PHIDField;Buffer:Pointer;Size:LongWord;var Value:LongWord):LongWord;
{Extract an unsigned field value from a report buffer}
{Field: The field to extract from the report}
{Buffer: A pointer to the report buffer}
{Size: The size in bytes of the report buffer}
{Value: A variable to receive the extracted value}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Data:Pointer;
 Mask:LongWord;
 Remain:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Set Default}
 Value:=0;

 {Check Field}
 if Field = nil then Exit;
 if Field.Size > 4 then Exit;

 {Check Buffer}
 if Buffer = nil then Exit;

 {Check Size}
 if Size < (Field.Offset + Field.Size) then Exit;

 {Set Result}
 Result:=ERROR_SUCCESS;

 {Check Bits}
 if Field.Bits = 0 then Exit;

 {Get Data}
 Data:=Pointer(Buffer + Field.Offset);

 {Get Mask}
 Mask:=(1 shl Min(Field.Bits,24)) - 1;

 {Get Value}
 Value:=(PLongWord(Data)^ shr Field.Shift) and Mask;

 {Check Bits}
 if Field.Bits > 24 then
  begin
   {Get Data}
   Data:=PByte(Buffer + Field.Offset + 3);

   {Get Mask}
   Mask:=(1 shl (Field.Bits - 24)) - 1;

   {Get Remain}
   Remain:=(PLongWord(Data)^ shr Field.Shift) and Mask;

   {Add Remain}
   Value:=(Remain shl 24) or Value;
  end;
end;

{==============================================================================}
{==============================================================================}
{HID Device Functions}
function HIDDeviceSetState(Device:PHIDDevice;State:LongWord):LongWord;
{Set the state of the specified HID device and send a notification}
{Device: The HID device to set the state for}
{State: The new state to set and notify (eg HID_STATE_ATTACHED)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check State}
 if State > HID_STATE_ATTACHED then Exit;

 {Check State}
 if Device.HIDState = State then
  begin
   {Return Result}
   Result:=ERROR_SUCCESS;
  end
 else
  begin
   {Acquire the Lock}
   if MutexLock(Device.Lock) = ERROR_SUCCESS then
    begin
     try
      {Set State}
      Device.HIDState:=State;

      {Notify State}
      NotifierNotify(@Device.Device,HIDDeviceStateToNotification(State));

      {Return Result}
      Result:=ERROR_SUCCESS;
     finally
      {Release the Lock}
      MutexUnlock(Device.Lock);
     end;
    end
   else
    begin
     Result:=ERROR_OPERATION_FAILED;
    end;
  end;
end;

{==============================================================================}

function HIDDeviceGetIdle(Device:PHIDDevice;var Duration:Word;ReportId:Byte):LongWord;
{Get the idle rate from a HID device for the specified report id}
{Device: The HID device to get the idle rate from}
{Duration: A variable to receive the idle rate (in Milliseconds)}
{ReportId: The report id to get the idle rate from}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Set Defaults}
 Duration:=0;

 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if MutexLock(Device.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Method}
    if Assigned(Device.DeviceGetIdle) then
     begin
      {Provided Method}
      Result:=Device.DeviceGetIdle(Device,Duration,ReportId);
     end
    else
     begin
      {Not Supported}
      Result:=ERROR_NOT_SUPPORTED;
     end;
   finally
    {Release the Lock}
    MutexUnlock(Device.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_OPERATION_FAILED;
  end;
end;

{==============================================================================}

function HIDDeviceSetIdle(Device:PHIDDevice;Duration:Word;ReportId:Byte):LongWord;
{Set the idle rate on a HID device for the specified report id}
{Device: The HID device to set the idle rate for}
{Duration: The idle rate to set (in Milliseconds)}
{ReportId: The report id to set the idle rate for}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if MutexLock(Device.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Method}
    if Assigned(Device.DeviceSetIdle) then
     begin
      {Provided Method}
      Result:=Device.DeviceSetIdle(Device,Duration,ReportId);
     end
    else
     begin
      {Not Supported}
      Result:=ERROR_NOT_SUPPORTED;
     end;
   finally
    {Release the Lock}
    MutexUnlock(Device.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_OPERATION_FAILED;
  end;
end;

{==============================================================================}

function HIDDeviceGetReport(Device:PHIDDevice;ReportType,ReportId:Byte;ReportData:Pointer;ReportSize:LongWord):LongWord;
{Read a report by type and id from a HID device}
{Device: The HID device to read the report from}
{ReportType: The report type to read (eg HID_REPORT_INPUT)}
{ReportId: The report id to read (eg HID_REPORTID_NONE)}
{ReportData: A pointer to a buffer to receive the report data}
{ReportSize: The size in bytes of the buffer pointed to by report data}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if MutexLock(Device.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Method}
    if Assigned(Device.DeviceGetReport) then
     begin
      {Provided Method}
      Result:=Device.DeviceGetReport(Device,ReportType,ReportId,ReportData,ReportSize);
     end
    else
     begin
      {Not Supported}
      Result:=ERROR_NOT_SUPPORTED;
     end;
   finally
    {Release the Lock}
    MutexUnlock(Device.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_OPERATION_FAILED;
  end;
end;

{==============================================================================}

function HIDDeviceSetReport(Device:PHIDDevice;ReportType,ReportId:Byte;ReportData:Pointer;ReportSize:LongWord):LongWord;
{Write a report by type and id to a HID device}
{Device: The HID device to write the report to}
{ReportType: The report type to write (eg HID_REPORT_OUTPUT)}
{ReportId: The report id to write (eg HID_REPORTID_NONE)}
{ReportData: A pointer to a buffer containing the report data}
{ReportSize: The size in bytes of the buffer pointed to by report data}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if MutexLock(Device.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Method}
    if Assigned(Device.DeviceSetReport) then
     begin
      {Provided Method}
      Result:=Device.DeviceSetReport(Device,ReportType,ReportId,ReportData,ReportSize);
     end
    else
     begin
      {Not Supported}
      Result:=ERROR_NOT_SUPPORTED;
     end;
   finally
    {Release the Lock}
    MutexUnlock(Device.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_OPERATION_FAILED;
  end;
end;

{==============================================================================}

function HIDDeviceAllocateReport(Device:PHIDDevice;Collection:PHIDCollection;ReportId:Byte;ReportSize:LongWord):LongWord;
{Allocate and initialize an input report by id on a HID device}
{Device: The HID device to allocate the report on}
{Collection: The HID collection this request corresponds to}
{ReportId: The report id to allocate (eg HID_REPORTID_NONE)}
{ReportSize: The size in bytes to allocate for the report (Provider will handle alignment and other requirements)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: An allocated report must be submitted before reports will be received from the device}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if MutexLock(Device.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Method}
    if Assigned(Device.DeviceAllocateReport) then
     begin
      {Provided Method}
      Result:=Device.DeviceAllocateReport(Device,Collection,ReportId,ReportSize);
     end
    else
     begin
      {Not Supported}
      Result:=ERROR_NOT_SUPPORTED;
     end;
   finally
    {Release the Lock}
    MutexUnlock(Device.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_OPERATION_FAILED;
  end;
end;

{==============================================================================}

function HIDDeviceReleaseReport(Device:PHIDDevice;ReportId:Byte):LongWord;
{Release an input report by id from a HID device}
{Device: The HID device to release the report from}
{ReportId: The report id to allocate (eg HID_REPORTID_NONE)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: If the report has been submitted it must be cancelled before being released}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if MutexLock(Device.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Method}
    if Assigned(Device.DeviceReleaseReport) then
     begin
      {Provided Method}
      Result:=Device.DeviceReleaseReport(Device,ReportId);
     end
    else
     begin
      {Not Supported}
      Result:=ERROR_NOT_SUPPORTED;
     end;
   finally
    {Release the Lock}
    MutexUnlock(Device.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_OPERATION_FAILED;
  end;
end;

{==============================================================================}

function HIDDeviceSubmitReport(Device:PHIDDevice;ReportId:Byte):LongWord;
{Submit an input report by id on a HID device}
{Device: The HID device to submit the report on}
{ReportId: The report id to submit (eg HID_REPORTID_NONE)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: The report must be allocated then submitted before reports will be received from the device}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if MutexLock(Device.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Method}
    if Assigned(Device.DeviceSubmitReport) then
     begin
      {Provided Method}
      Result:=Device.DeviceSubmitReport(Device,ReportId);
     end
    else
     begin
      {Not Supported}
      Result:=ERROR_NOT_SUPPORTED;
     end;
   finally
    {Release the Lock}
    MutexUnlock(Device.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_OPERATION_FAILED;
  end;
end;

{==============================================================================}

function HIDDeviceCancelReport(Device:PHIDDevice;ReportId:Byte):LongWord;
{Cancel an input report by id on a HID device}
{Device: The HID device to cancel the report on}
{ReportId: The report id to cancel (eg HID_REPORTID_NONE)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: The report should be cancelled then released to stop receiving reports from the device}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if MutexLock(Device.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Method}
    if Assigned(Device.DeviceCancelReport) then
     begin
      {Provided Method}
      Result:=Device.DeviceCancelReport(Device,ReportId);
     end
    else
     begin
      {Not Supported}
      Result:=ERROR_NOT_SUPPORTED;
     end;
   finally
    {Release the Lock}
    MutexUnlock(Device.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_OPERATION_FAILED;
  end;
end;

{==============================================================================}

function HIDDeviceGetProtocol(Device:PHIDDevice;var Protocol:Byte):LongWord;
{Get the report protocol from a HID device}
{Device: The HID device to get the report protocol from}
{Protocol: A variable to receive the report protocol (eg HID_PROTOCOL_REPORT)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Set Defaults}
 Protocol:=HID_PROTOCOL_BOOT;

 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if MutexLock(Device.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Method}
    if Assigned(Device.DeviceGetProtocol) then
     begin
      {Provided Method}
      Result:=Device.DeviceGetProtocol(Device,Protocol);
     end
    else
     begin
      {Not Supported}
      Result:=ERROR_NOT_SUPPORTED;
     end;
   finally
    {Release the Lock}
    MutexUnlock(Device.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_OPERATION_FAILED;
  end;
end;

{==============================================================================}

function HIDDeviceSetProtocol(Device:PHIDDevice;Protocol:Byte):LongWord;
{Set the report protocol for a HID device}
{Device: The HID device to set the report protocol for}
{Protocol: The report protocol to set (eg HID_PROTOCOL_REPORT)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if MutexLock(Device.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Method}
    if Assigned(Device.DeviceSetProtocol) then
     begin
      {Provided Method}
      Result:=Device.DeviceSetProtocol(Device,Protocol);
     end
    else
     begin
      {Not Supported}
      Result:=ERROR_NOT_SUPPORTED;
     end;
   finally
    {Release the Lock}
    MutexUnlock(Device.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_OPERATION_FAILED;
  end;
end;

{==============================================================================}

function HIDDeviceGetInterval(Device:PHIDDevice;var Interval:LongWord):LongWord;
{Get the polling interval from a HID device}
{Device: The HID device to get the polling interval from}
{Interval: A variable to receive the polling interval (in Milliseconds)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Set Defaults}
 Interval:=0;

 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if MutexLock(Device.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Method}
    if Assigned(Device.DeviceGetInterval) then
     begin
      {Provided Method}
      Result:=Device.DeviceGetInterval(Device,Interval);
     end
    else
     begin
      {Not Supported}
      Result:=ERROR_NOT_SUPPORTED;
     end;
   finally
    {Release the Lock}
    MutexUnlock(Device.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_OPERATION_FAILED;
  end;
end;

{==============================================================================}

function HIDDeviceSetInterval(Device:PHIDDevice;Interval:LongWord):LongWord;
{Set the polling interval for a HID device}
{Device: The HID device to set the polling interval for}
{Interval: The polling interval to set (in Milliseconds)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if MutexLock(Device.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Method}
    if Assigned(Device.DeviceSetInterval) then
     begin
      {Provided Method}
      Result:=Device.DeviceSetInterval(Device,Interval);
     end
    else
     begin
      {Not Supported}
      Result:=ERROR_NOT_SUPPORTED;
     end;
   finally
    {Release the Lock}
    MutexUnlock(Device.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_OPERATION_FAILED;
  end;
end;

{==============================================================================}

function HIDDeviceGetReportDescriptor(Device:PHIDDevice;Descriptor:PHIDReportDescriptor;Size:LongWord):LongWord;
{Get the Report Descriptor for a HID device}
{Device: The HID device to get the descriptor for}
{Descriptor: Pointer to a buffer to return the HID Report Descriptor}
{Size: The size in bytes of the buffer pointed to by Descriptor}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if MutexLock(Device.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Method}
    if Assigned(Device.DeviceGetReportDescriptor) then
     begin
      {Provided Method}
      Result:=Device.DeviceGetReportDescriptor(Device,Descriptor,Size);
     end
    else
     begin
      {Not Supported}
      Result:=ERROR_NOT_SUPPORTED;
     end;
   finally
    {Release the Lock}
    MutexUnlock(Device.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_OPERATION_FAILED;
  end;
end;

{==============================================================================}

function HIDDeviceGetPhysicalDescriptorSet0(Device:PHIDDevice;Descriptor:PHIDPhysicalDescriptorSet0):LongWord;
{Get the HID Physical Descriptor Set 0 for a HID device}
{Device: The HID device to get the descriptor for}
{Descriptor: Pointer to a HID Physical Descriptor Set 0 structure for the returned data}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if MutexLock(Device.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Method}
    if Assigned(Device.DeviceGetPhysicalDescriptorSet0) then
     begin
      {Provided Method}
      Result:=Device.DeviceGetPhysicalDescriptorSet0(Device,Descriptor);
     end
    else
     begin
      {Default Method}
      Result:=HIDDeviceGetPhysicalDescriptorSet(Device,PHIDPhysicalDescriptorSet(Descriptor),0,SizeOf(THIDPhysicalDescriptorSet0));
     end;
   finally
    {Release the Lock}
    MutexUnlock(Device.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_OPERATION_FAILED;
  end;
end;

{==============================================================================}

function HIDDeviceGetPhysicalDescriptorSet(Device:PHIDDevice;Descriptor:PHIDPhysicalDescriptorSet;Index:Byte;Size:LongWord):LongWord;
{Get a HID Physical Descriptor Set for a HID device}
{Device: The HID device to get the descriptor for}
{Descriptor: Pointer to a HID Physical Descriptor Set structure for the returned data}
{Index: The index of the physical descriptor set to return}
{Size: The size in bytes of the buffer pointed to by Descriptor}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if MutexLock(Device.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Method}
    if Assigned(Device.DeviceGetPhysicalDescriptorSet) then
     begin
      {Provided Method}
      Result:=Device.DeviceGetPhysicalDescriptorSet(Device,Descriptor,Index,Size);
     end
    else
     begin
      {Not Supported}
      Result:=ERROR_NOT_SUPPORTED;
     end;
   finally
    {Release the Lock}
    MutexUnlock(Device.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_OPERATION_FAILED;
  end;
end;

{==============================================================================}

function HIDDeviceBindDevice(Device:PHIDDevice):LongWord;
{Attempt to bind a HID device to one of the registered consumers}
{Device: The HID device to attempt to bind a consumer to}
{Return: ERROR_SUCCESS if completed, ERROR_NOT_SUPPORTED if unsupported or another error code on failure}

 function HIDCheckCollection(Parent:PHIDCollection):Boolean;
 var
  Index:LongWord;
 begin
  {}
  Result:=False;

  {Check Parent}
  if Parent = nil then Exit;

  {Check Consumer}
  if Parent.Consumer <> nil then
   begin
    Result:=True;
    Exit;
   end;

  {Check Collections}
  if Parent.CollectionCount > 0 then
   begin
    for Index:=0 to Parent.CollectionCount - 1 do
     begin
      if Parent.Collections[Index] <> nil then
       begin
        {Check Collection}
        Result:=HIDCheckCollection(Parent.Collections[Index]);
        if Result then Exit;
       end;
     end;
   end;
 end;

var
 Index:LongWord;
 Status:LongWord;
 Consumer:PHIDConsumer;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Device}
 if HIDDeviceCheck(Device) <> Device then Exit;

 {Acquire the Lock}
 if MutexLock(Device.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Consumer}
    if Device.Consumer <> nil then
     begin
      {Already Bound}
      Result:=ERROR_SUCCESS;
      Exit;
     end;

    {Check Collections}
    if Device.CollectionCount > 0 then
     begin
      for Index:=0 to Device.CollectionCount - 1 do
       begin
        if Device.Collections[Index] <> nil then
         begin
          {Check Collection}
          if HIDCheckCollection(Device.Collections[Index]) then
           begin
            {Already Bound on at least one collection}
            Result:=ERROR_NOT_SUPPORTED;
            Exit;
           end;
         end;
       end;
     end;

    {Set Default}
    Status:=ERROR_NOT_SUPPORTED;

    {Acquire the Lock}
    if CriticalSectionLock(HIDConsumerTableLock) = ERROR_SUCCESS then
     begin
      try
       {Get Consumer}
       Consumer:=HIDConsumerTable;
       while Consumer <> nil do
        begin
         {$IFDEF HID_DEBUG}
         if HID_LOG_ENABLED then HIDLogDebug(Device,'Attempting to bind ' + DriverGetName(@Consumer.Driver) + ' to device');
         {$ENDIF}

         {Check Consumer}
         if Assigned(Consumer.DeviceBind) then
          begin
           {Attempt to Bind (Device)}
           Status:=Consumer.DeviceBind(Device);
           if Status <> ERROR_NOT_SUPPORTED then
            begin
             if Status = ERROR_SUCCESS then
              begin
               if HID_LOG_ENABLED then HIDLogInfo(nil,'Bound ' + DriverGetName(@Consumer.Driver) + ' to ' + DeviceGetName(@Device.Device));

               {Set Consumer}
               Device.Consumer:=Consumer;
              end;

             Break; {Break to return Status}
            end;
          end;

         {Get Next}
         Consumer:=Consumer.Next;
        end;
      finally
       {Release the Lock}
       CriticalSectionUnlock(HIDConsumerTableLock);
      end;
     end;

    {Return Result}
    Result:=Status;
   finally
    {Release the Lock}
    MutexUnlock(Device.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_OPERATION_FAILED;
  end;
end;

{==============================================================================}

function HIDDeviceUnbindDevice(Device:PHIDDevice;Consumer:PHIDConsumer):LongWord;
{Unbind a HID device from a consumer}
{Device: The HID device to unbind a consumer from}
{Consumer: The consumer to unbind the device from (nil to unbind from current consumer)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Status:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Device}
 if HIDDeviceCheck(Device) <> Device then Exit;

 {Acquire the Lock}
 if MutexLock(Device.Lock) = ERROR_SUCCESS then
  begin
   try
    {Set Default}
    Status:=ERROR_NOT_ASSIGNED;

    {Check Consumer}
    if Consumer = nil then
     begin
      {Check Device Consumer}
      if (Device.Consumer <> nil) and Assigned(Device.Consumer.DeviceUnbind) then
       begin
        {$IFDEF HID_DEBUG}
        if HID_LOG_ENABLED then HIDLogDebug(Device,'Unbinding ' + DriverGetName(@Device.Consumer.Driver));
        {$ENDIF}

        {Unbind Consumer (Device)}
        Device.Consumer.DeviceUnbind(Device);

        {Reset Consumer}
        Device.Consumer:=nil;

        Status:=ERROR_SUCCESS;
       end;
     end
    else
     begin
      {Check Device Consumer}
      if (Device.Consumer = Consumer) and Assigned(Device.Consumer.DeviceUnbind) then
       begin
        {$IFDEF HID_DEBUG}
        if HID_LOG_ENABLED then HIDLogDebug(Device,'Unbinding ' + DriverGetName(@Device.Consumer.Driver));
        {$ENDIF}

        {Unbind Consumer (Device)}
        Device.Consumer.DeviceUnbind(Device);

        {Reset Consumer}
        Device.Consumer:=nil;

        Status:=ERROR_SUCCESS;
       end;
     end;

    {Return Result}
    Result:=Status;
   finally
    {Release the Lock}
    MutexUnlock(Device.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_OPERATION_FAILED;
  end;
end;

{==============================================================================}

function HIDDeviceBindCollections(Device:PHIDDevice):LongWord;
{Attempt to bind the HID collections in the specified device to one of the registered consumers}
{Device: The HID device containing the collections to attempt to bind a consumer to}
{Return: ERROR_SUCCESS if completed or another error code on failure}

 function HIDBindCollection(Parent:PHIDCollection;Consumer:PHIDConsumer):LongWord;
 var
  Index:LongWord;
  Status:LongWord;
  Collection:PHIDCollection;
 begin
  {}
  Result:=ERROR_INVALID_PARAMETER;

  {Check Parent}
  if Parent = nil then Exit;

  {Check Consumer}
  if Consumer = nil then Exit;

  Result:=ERROR_NOT_SUPPORTED;

  {Attempt to Bind (Collections)}
  Status:=Consumer.CollectionBind(Device,Parent);
  if Status <> ERROR_NOT_SUPPORTED then
   begin
    if Status = ERROR_SUCCESS then
     begin
      if HID_LOG_ENABLED then HIDLogInfo(nil,'Bound ' + DriverGetName(@Consumer.Driver) + ' to ' + DeviceGetName(@Device.Device) + ' collection ' + IntToHex(Parent.Page,4) + IntToHex(Parent.Usage,4));

      {Set Consumer}
      Parent.Consumer:=Consumer;
     end;

    Result:=Status;
   end;

  {Check Result}
  if Result = ERROR_SUCCESS then Exit;

  {Check Collections}
  if Parent.CollectionCount > 0 then
   begin
    for Index:=0 to Parent.CollectionCount - 1 do
     begin
      Collection:=Parent.Collections[Index];
      if Collection <> nil then
       begin
        {Check Consumer}
        if Collection.Consumer = nil then
         begin
          {Bind Collection}
          Status:=HIDBindCollection(Collection,Consumer);
          if Status <> ERROR_NOT_SUPPORTED then Result:=Status;
         end;
       end;
     end;
   end;
 end;

var
 Index:LongWord;
 Status:LongWord;
 Consumer:PHIDConsumer;
 Collection:PHIDCollection;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Device}
 if HIDDeviceCheck(Device) <> Device then Exit;

 {Acquire the Lock}
 if MutexLock(Device.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Consumer}
    if Device.Consumer <> nil then
     begin
      {Already Bound}
      Result:=ERROR_SUCCESS;
      Exit;
     end;

    {Set Default}
    Result:=ERROR_NOT_SUPPORTED;

    {Acquire the Lock}
    if CriticalSectionLock(HIDConsumerTableLock) = ERROR_SUCCESS then
     begin
      try
       {Get Consumer}
       Consumer:=HIDConsumerTable;
       while Consumer <> nil do
        begin
         {$IFDEF HID_DEBUG}
         if HID_LOG_ENABLED then HIDLogDebug(Device,'Attempting to bind ' + DriverGetName(@Consumer.Driver) + ' to collections');
         {$ENDIF}

         {Check Collections}
         if Device.CollectionCount > 0 then
          begin
           for Index:=0 to Device.CollectionCount - 1 do
            begin
             Collection:=Device.Collections[Index];
             if Collection <> nil then
              begin
               {Check Consumer}
               if Collection.Consumer = nil then
                begin
                 {Bind Collection}
                 Status:=HIDBindCollection(Collection,Consumer);
                 if Status <> ERROR_NOT_SUPPORTED then Result:=Status;
                end;
              end;
            end;
          end;

         {Get Next}
         Consumer:=Consumer.Next;
        end;
      finally
       {Release the Lock}
       CriticalSectionUnlock(HIDConsumerTableLock);
      end;
     end;
   finally
    {Release the Lock}
    MutexUnlock(Device.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_OPERATION_FAILED;
  end;
end;

{==============================================================================}

function HIDDeviceUnbindCollections(Device:PHIDDevice;Consumer:PHIDConsumer):LongWord;
{Unbind the HID collections in the specified device from a consumer}
{Device: The HID device containing the collections to unbind a consumer from}
{Consumer: The consumer to unbind the collections from (nil to unbind from current consumer)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

 function HIDUnbindCollection(Parent:PHIDCollection):LongWord;
 var
  Index:LongWord;
  Status:LongWord;
  Collection:PHIDCollection;
 begin
  {}
  Result:=ERROR_INVALID_PARAMETER;

  {Check Parent}
  if Parent = nil then Exit;

  Result:=ERROR_NOT_ASSIGNED;

  {Check Consumer}
  if Consumer = nil then
   begin
    if (Parent.Consumer <> nil) and Assigned(Parent.Consumer.CollectionUnbind) then
     begin
      {$IFDEF HID_DEBUG}
      if HID_LOG_ENABLED then HIDLogDebug(Device,'Unbinding ' + DriverGetName(@Parent.Consumer.Driver));
      {$ENDIF}

      {Unbind Consumer (Collection)}
      Parent.Consumer.CollectionUnbind(Device,Parent);

      {Reset Consumer}
      Parent.Consumer:=nil;

      Result:=ERROR_SUCCESS;
     end;
   end
  else
   begin
    if (Parent.Consumer = Consumer) and Assigned(Parent.Consumer.CollectionUnbind) then
     begin
      {$IFDEF HID_DEBUG}
      if HID_LOG_ENABLED then HIDLogDebug(Device,'Unbinding ' + DriverGetName(@Parent.Consumer.Driver));
      {$ENDIF}

      {Unbind Consumer (Collection)}
      Parent.Consumer.CollectionUnbind(Device,Parent);

      {Reset Consumer}
      Parent.Consumer:=nil;

      Result:=ERROR_SUCCESS;
     end;
   end;

  {Check Collections}
  if Parent.CollectionCount > 0 then
   begin
    for Index:=0 to Parent.CollectionCount - 1 do
     begin
      Collection:=Parent.Collections[Index];
      if Collection <> nil then
       begin
        {Unbind Collection}
        Status:=HIDUnbindCollection(Collection);
        if Status <> ERROR_NOT_SUPPORTED then Result:=Status;
       end;
     end;
   end;
 end;

var
 Index:LongWord;
 Status:LongWord;
 Collection:PHIDCollection;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Device}
 if HIDDeviceCheck(Device) <> Device then Exit;

 {Acquire the Lock}
 if MutexLock(Device.Lock) = ERROR_SUCCESS then
  begin
   try
    {Set Default}
    Status:=ERROR_NOT_ASSIGNED;

    {Check Collections}
    if Device.CollectionCount > 0 then
     begin
      for Index:=0 to Device.CollectionCount - 1 do
       begin
        Collection:=Device.Collections[Index];
        if Collection <> nil then
         begin
          {Unbind Collection}
          Status:=HIDUnbindCollection(Collection);
          if Status <> ERROR_NOT_ASSIGNED then Result:=Status;
         end;
       end;
     end;
   finally
    {Release the Lock}
    MutexUnlock(Device.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_OPERATION_FAILED;
  end;
end;

{==============================================================================}

function HIDDeviceCreate:PHIDDevice;
{Create a new HID device entry}
{Return: Pointer to new HID device entry or nil if HID device could not be created}
begin
 {}
 Result:=HIDDeviceCreateEx(SizeOf(THIDDevice));
end;

{==============================================================================}

function HIDDeviceCreateEx(Size:LongWord):PHIDDevice;
{Create a new HID device entry}
{Size: Size in bytes to allocate for new HID device (Including the HID device entry)}
{Return: Pointer to new HID device entry or nil if HID device could not be created}
begin
 {}
 Result:=nil;

 {Check Size}
 if Size < SizeOf(THIDDevice) then Exit;

 {Create Device}
 Result:=PHIDDevice(DeviceCreateEx(Size));
 if Result = nil then Exit;

 {Update Device}
 Result.Device.DeviceBus:=DEVICE_BUS_NONE;
 Result.Device.DeviceType:=HID_TYPE_NONE;
 Result.Device.DeviceFlags:=HID_FLAG_NONE;
 Result.Device.DeviceData:=nil;

 {Update HID}
 Result.HIDId:=DEVICE_ID_ANY;
 Result.HIDState:=HID_STATE_DETACHED;
 Result.DeviceGetIdle:=nil;
 Result.DeviceSetIdle:=nil;
 Result.DeviceGetReport:=nil;
 Result.DeviceSetReport:=nil;
 Result.DeviceAllocateReport:=nil;
 Result.DeviceReleaseReport:=nil;
 Result.DeviceSubmitReport:=nil;
 Result.DeviceCancelReport:=nil;
 Result.DeviceGetProtocol:=nil;
 Result.DeviceSetProtocol:=nil;
 Result.DeviceGetInterval:=nil;
 Result.DeviceSetInterval:=nil;
 Result.DeviceGetReportDescriptor:=nil;
 Result.DeviceGetPhysicalDescriptorSet0:=nil;
 Result.DeviceGetPhysicalDescriptorSet:=nil;
 Result.Lock:=INVALID_HANDLE_VALUE;

 {Create Lock}
 Result.Lock:=MutexCreateEx(False,MUTEX_DEFAULT_SPINCOUNT,MUTEX_FLAG_RECURSIVE);
 if Result.Lock = INVALID_HANDLE_VALUE then
  begin
   if HID_LOG_ENABLED then HIDLogError(nil,'Failed to create lock for HID device');

   HIDDeviceDestroy(Result);
   Result:=nil;
   Exit;
  end;
end;

{==============================================================================}

function HIDDeviceDestroy(Device:PHIDDevice):LongWord;
{Destroy an existing HID device entry}
{Device: The HID device to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Device}
 Result:=ERROR_IN_USE;
 if HIDDeviceCheck(Device) = Device then Exit;

 {Check State}
 if Device.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;

 {Destroy Lock}
 if Device.Lock <> INVALID_HANDLE_VALUE then
  begin
   MutexDestroy(Device.Lock);
  end;

 {Destroy Device}
 Result:=DeviceDestroy(@Device.Device);
end;

{==============================================================================}

function HIDDeviceRegister(Device:PHIDDevice):LongWord;
{Register a new HID device in the HID device table}
{Device: The HID device to register}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 HIDId:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;
 if Device.HIDId <> DEVICE_ID_ANY then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Device}
 Result:=ERROR_ALREADY_EXISTS;
 if HIDDeviceCheck(Device) = Device then Exit;

 {Check State}
 if Device.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;

 {Insert Device}
 if CriticalSectionLock(HIDDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Update Device}
    HIDId:=0;
    while HIDDeviceFind(HIDId) <> nil do
     begin
      Inc(HIDId);
     end;
    Device.HIDId:=HIDId;

    {Update Device}
    Device.Device.DeviceName:=HID_NAME_PREFIX + IntToStr(Device.HIDId);
    Device.Device.DeviceClass:=DEVICE_CLASS_HID;

    {Register Device}
    Result:=DeviceRegister(@Device.Device);
    if Result <> ERROR_SUCCESS then
     begin
      Device.HIDId:=DEVICE_ID_ANY;
      Exit;
     end;

    {Link Device}
    if HIDDeviceTable = nil then
     begin
      HIDDeviceTable:=Device;
     end
    else
     begin
      Device.Next:=HIDDeviceTable;
      HIDDeviceTable.Prev:=Device;
      HIDDeviceTable:=Device;
     end;

    {Increment Count}
    Inc(HIDDeviceTableCount);

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(HIDDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function HIDDeviceDeregister(Device:PHIDDevice):LongWord;
{Deregister a HID device from the HID device table}
{Device: The HID device to deregister}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Prev:PHIDDevice;
 Next:PHIDDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;
 if Device.HIDId = DEVICE_ID_ANY then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Device}
 Result:=ERROR_NOT_FOUND;
 if HIDDeviceCheck(Device) <> Device then Exit;

 {Check State}
 if Device.Device.DeviceState <> DEVICE_STATE_REGISTERED then Exit;

 {Remove Device}
 if CriticalSectionLock(HIDDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Deregister Device}
    Result:=DeviceDeregister(@Device.Device);
    if Result <> ERROR_SUCCESS then Exit;

    {Unlink Device}
    Prev:=Device.Prev;
    Next:=Device.Next;
    if Prev = nil then
     begin
      HIDDeviceTable:=Next;
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
    Dec(HIDDeviceTableCount);

    {Update Device}
    Device.HIDId:=DEVICE_ID_ANY;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(HIDDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function HIDDeviceFind(HIDId:LongWord):PHIDDevice;
{Find a HID device by ID in the HID device table}
{HIDId: The ID number of the HID device to find}
{Return: Pointer to HID device entry or nil if not found}
var
 Device:PHIDDevice;
begin
 {}
 Result:=nil;

 {Check Id}
 if HIDId = DEVICE_ID_ANY then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(HIDDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Device}
    Device:=HIDDeviceTable;
    while Device <> nil do
     begin
      {Check State}
      if Device.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Id}
        if Device.HIDId = HIDId then
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
    CriticalSectionUnlock(HIDDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

function HIDDeviceFindByName(const Name:String):PHIDDevice; inline;
{Find a HID device by name in the device table}
{Name: The name of the HID device to find (eg HID0)}
{Return: Pointer to HID device entry or nil if not found}
begin
 {}
 Result:=PHIDDevice(DeviceFindByName(Name));
end;

{==============================================================================}

function HIDDeviceFindByDescription(const Description:String):PHIDDevice; inline;
{Find a HID device by description in the device table}
{Description: The description of the HID to find (eg Optical USB Mouse)}
{Return: Pointer to HID device entry or nil if not found}
begin
 {}
 Result:=PHIDDevice(DeviceFindByDescription(Description));
end;

{==============================================================================}

function HIDDeviceEnumerate(Callback:THIDDeviceEnumerate;Data:Pointer):LongWord;
{Enumerate all HID devices in the HID device table}
{Callback: The callback function to call for each HID device in the table}
{Data: A private data pointer to pass to callback for each HID device in the table}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Device:PHIDDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Callback}
 if not Assigned(Callback) then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(HIDDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Device}
    Device:=HIDDeviceTable;
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
    CriticalSectionUnlock(HIDDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function HIDDeviceNotification(Device:PHIDDevice;Callback:THIDDeviceNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;
{Register a notification for HID device changes}
{Device: The HID device to notify changes for (Optional, pass nil for all HID devices)}
{Callback: The function to call when a notification event occurs}
{Data: A private data pointer to pass to callback when a notification event occurs}
{Notification: The events to register for notification of (eg DEVICE_NOTIFICATION_REGISTER)}
{Flags: The flags to control the notification (eg NOTIFIER_FLAG_WORKER)}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then
  begin
   Result:=DeviceNotification(nil,DEVICE_CLASS_HID,Callback,Data,Notification,Flags);
  end
 else
  begin
   {Check Device}
   if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

   Result:=DeviceNotification(@Device.Device,DEVICE_CLASS_HID,Callback,Data,Notification,Flags);
  end;
end;

{==============================================================================}
{==============================================================================}
{HID Consumer Functions}
function HIDConsumerCreate:PHIDConsumer;
{Create a new HID Consumer entry}
{Return: Pointer to new Consumer entry or nil if consumer could not be created}
begin
 {}
 Result:=HIDConsumerCreateEx(SizeOf(THIDConsumer));
end;

{==============================================================================}

function HIDConsumerCreateEx(Size:LongWord):PHIDConsumer;
{Create a new HID Consumer entry}
{Size: Size in bytes to allocate for new consumer (Including the consumer entry)}
{Return: Pointer to new Consumer entry or nil if consumer could not be created}
begin
 {}
 Result:=nil;

 {Check Size}
 if Size < SizeOf(THIDConsumer) then Exit;

 {Create Consumer}
 Result:=PHIDConsumer(DriverCreateEx(Size));
 if Result = nil then Exit;

 {Update Consumer}
 Result.DeviceBind:=nil;
 Result.DeviceUnbind:=nil;
 Result.CollectionBind:=nil;
 Result.CollectionUnbind:=nil;
 Result.ReportReceive:=nil;
 Result.Lock:=INVALID_HANDLE_VALUE;

 {Create Lock}
 Result.Lock:=MutexCreateEx(False,MUTEX_DEFAULT_SPINCOUNT,MUTEX_FLAG_RECURSIVE);
 if Result.Lock = INVALID_HANDLE_VALUE then
  begin
   if HID_LOG_ENABLED then HIDLogError(nil,'Failed to create lock for consumer');

   HIDConsumerDestroy(Result);
   Result:=nil;
   Exit;
  end;
end;

{==============================================================================}

function HIDConsumerDestroy(Consumer:PHIDConsumer):LongWord;
{Destroy an existing HID Consumer entry}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Consumer}
 if Consumer = nil then Exit;
 if Consumer.Driver.Signature <> DRIVER_SIGNATURE then Exit;

 {Check Consumer}
 Result:=ERROR_IN_USE;
 if HIDConsumerCheck(Consumer) = Consumer then Exit;

 {Check State}
 if Consumer.Driver.DriverState <> DRIVER_STATE_UNREGISTERED then Exit;

 {Destroy Lock}
 if Consumer.Lock <> INVALID_HANDLE_VALUE then
  begin
   MutexDestroy(Consumer.Lock);
  end;

 {Destroy Consumer}
 Result:=DriverDestroy(@Consumer.Driver);
end;

{==============================================================================}

function HIDConsumerRegister(Consumer:PHIDConsumer):LongWord;
{Register a new Consumer in the HID Consumer table}
var
 Status:LongWord;
 Device:PHIDDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Consumer}
 if Consumer = nil then Exit;
 if Consumer.Driver.DriverId <> DRIVER_ID_ANY then Exit;
 if Consumer.Driver.Signature <> DRIVER_SIGNATURE then Exit;

 {Check Collection Bind}
 if not(Assigned(Consumer.CollectionBind)) then
  begin
   if HID_LOG_ENABLED then HIDLogError(nil,'Cannot register consumer, Bind function must be implemented');
   Exit;
  end;

 {Check Collection Unbind}
 if not(Assigned(Consumer.CollectionUnbind)) then
  begin
   if HID_LOG_ENABLED then HIDLogError(nil,'Cannot register consumer, Unbind function must be implemented');
   Exit;
  end;

 {Check Report Receive}
 if not(Assigned(Consumer.ReportReceive)) then
  begin
   if HID_LOG_ENABLED then HIDLogError(nil,'Cannot register consumer, Receive function must be implemented');
   Exit;
  end;

 {Check Consumer}
 Result:=ERROR_ALREADY_EXISTS;
 if HIDConsumerCheck(Consumer) = Consumer then
  begin
   if HID_LOG_ENABLED then HIDLogError(nil,'Cannot register consumer, already registered');
   Exit;
  end;

 {Check State}
 if Consumer.Driver.DriverState <> DRIVER_STATE_UNREGISTERED then Exit;

 {Insert Consumer}
 if CriticalSectionLock(HIDConsumerTableLock) = ERROR_SUCCESS then
  begin
   try
    {Update Consumer}
    Consumer.Driver.DriverClass:=DRIVER_CLASS_HID;

    {Register Consumer}
    Result:=DriverRegister(@Consumer.Driver);
    if Result <> ERROR_SUCCESS then Exit;

    {Link Consumer}
    if HIDConsumerTable = nil then
     begin
      HIDConsumerTable:=Consumer;
     end
    else
     begin
      Consumer.Next:=HIDConsumerTable;
      HIDConsumerTable.Prev:=Consumer;
      HIDConsumerTable:=Consumer;
     end;

    {Increment Count}
    Inc(HIDConsumerTableCount);

    if HID_LOG_ENABLED then HIDLogInfo(nil,'Registered ' + DriverGetName(@Consumer.Driver) + ' (Id=' + IntToStr(Consumer.Driver.DriverId) + ')');

    {Release Consumer Table Lock}
    CriticalSectionUnlock(HIDConsumerTableLock);

    {Acquire the Lock}
    if CriticalSectionLock(HIDDeviceTableLock) = ERROR_SUCCESS then
     begin
      try
       {Get Device}
       Device:=HIDDeviceTable;
       while Device <> nil do
        begin
         {Check State}
         if Device.Device.DeviceState = DEVICE_STATE_REGISTERED then
          begin
           {Bind Device}
           Status:=HIDDeviceBindDevice(Device);
           if Status <> ERROR_SUCCESS then
            begin
             if Status <> ERROR_NOT_SUPPORTED then
              begin
               if HID_LOG_ENABLED then HIDLogError(Device,'Failure during device bind: ' + ErrorToString(Status));
              end;

             {Bind Collections}
             Status:=HIDDeviceBindCollections(Device);
             if (Status <> ERROR_SUCCESS) and (Status <> ERROR_NOT_SUPPORTED) then
              begin
               if HID_LOG_ENABLED then HIDLogError(Device,'Failure during collection bind: ' + ErrorToString(Status));
              end;
            end;
          end;

         {Get Next}
         Device:=Device.Next;
        end;

       {Return Result}
       Result:=ERROR_SUCCESS;
      finally
       {Release the Lock}
       CriticalSectionUnlock(HIDDeviceTableLock);
      end;
     end;
   finally
    if Result <> ERROR_SUCCESS then CriticalSectionUnlock(HIDConsumerTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function HIDConsumerDeregister(Consumer:PHIDConsumer):LongWord;
{Deregister a Consumer from the HID Consumer table}
var
 Prev:PHIDConsumer;
 Next:PHIDConsumer;
 Device:PHIDDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Consumer}
 if Consumer = nil then Exit;
 if Consumer.Driver.DriverId = DRIVER_ID_ANY then Exit;
 if Consumer.Driver.Signature <> DRIVER_SIGNATURE then Exit;

 {Check Consumer}
 Result:=ERROR_NOT_FOUND;
 if HIDConsumerCheck(Consumer) <> Consumer then Exit;

 {Check State}
 if Consumer.Driver.DriverState <> DRIVER_STATE_REGISTERED then Exit;

 {Remove Consumer}
 if CriticalSectionLock(HIDConsumerTableLock) = ERROR_SUCCESS then
  begin
   try
    {Acquire the Lock}
    if CriticalSectionLock(HIDDeviceTableLock) = ERROR_SUCCESS then
     begin
      try
       {Get Device}
       Device:=HIDDeviceTable;
       while Device <> nil do
        begin
         {Check State}
         if Device.Device.DeviceState = DEVICE_STATE_REGISTERED then
          begin
           {Unbind Collections}
           HIDDeviceUnbindCollections(Device,Consumer);

           {Unbind Device}
           HIDDeviceUnbindDevice(Device,Consumer);
          end;

         {Get Next}
         Device:=Device.Next;
        end;
      finally
       {Release the Lock}
       CriticalSectionUnlock(HIDDeviceTableLock);
      end;
     end
    else
     begin
      Result:=ERROR_CAN_NOT_COMPLETE;
      Exit;
     end;

    {Deregister Consumer}
    Result:=DriverDeregister(@Consumer.Driver);
    if Result <> ERROR_SUCCESS then Exit;

    {Unlink Consumer}
    Prev:=Consumer.Prev;
    Next:=Consumer.Next;
    if Prev = nil then
     begin
      HIDConsumerTable:=Next;
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
    Dec(HIDConsumerTableCount);

    if HID_LOG_ENABLED then HIDLogInfo(nil,'Deregistered ' + DriverGetName(@Consumer.Driver));

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(HIDConsumerTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function HIDConsumerFind(ConsumerId:LongWord):PHIDConsumer;
{Find a consumer by Id in the HID Consumer table}
var
 Consumer:PHIDConsumer;
begin
 {}
 Result:=nil;

 {Check Id}
 if ConsumerId = DRIVER_ID_ANY then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(HIDConsumerTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Consumer}
    Consumer:=HIDConsumerTable;
    while Consumer <> nil do
     begin
      {Check State}
      if Consumer.Driver.DriverState = DRIVER_STATE_REGISTERED then
       begin
        {Check Id}
        if Consumer.Driver.DriverId = ConsumerId then
         begin
          Result:=Consumer;
          Exit;
         end;
       end;

      {Get Next}
      Consumer:=Consumer.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(HIDConsumerTableLock);
   end;
  end;
end;

{==============================================================================}

function HIDConsumerFindByName(const Name:String):PHIDConsumer; inline;
{Find a consumer by name in the Driver table}
begin
 {}
 Result:=PHIDConsumer(DriverFindByName(Name));
end;

{==============================================================================}

function HIDConsumerEnumerate(Callback:THIDConsumerEnumerate;Data:Pointer):LongWord;
{Enumerate all consumers in the HID Consumer table}
var
 Consumer:PHIDConsumer;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Callback}
 if not Assigned(Callback) then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(HIDConsumerTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Consumer}
    Consumer:=HIDConsumerTable;
    while Consumer <> nil do
     begin
      {Check State}
      if Consumer.Driver.DriverState = DRIVER_STATE_REGISTERED then
       begin
        if Callback(Consumer,Data) <> ERROR_SUCCESS then Exit;
       end;

      {Get Next}
      Consumer:=Consumer.Next;
     end;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(HIDConsumerTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}
{==============================================================================}
{HID Helper Functions}
function HIDIsBitField(Field:PHIDField):Boolean;
{Return True is the supplied field contains a 1 bit value}
begin
 {}
 Result:=False;

 {Check Field}
 if Field = nil then Exit;

 Result:=(Field.Bits = 1);
end;

{==============================================================================}

function HIDIsByteField(Field:PHIDField):Boolean;
{Return True is the supplied HID field contains a 1 byte value}
begin
 {}
 Result:=False;

 {Check Field}
 if Field = nil then Exit;

 Result:=(Field.Size = 1);
end;

{==============================================================================}

function HIDIsWordField(Field:PHIDField):Boolean;
{Return True is the supplied HID field contains a 2 byte value}
begin
 {}
 Result:=False;

 {Check Field}
 if Field = nil then Exit;

 Result:=(Field.Size = 2);
end;

{==============================================================================}

function HIDIsLongField(Field:PHIDField):Boolean;
{Return True is the supplied HID field contains a 3 or 4 byte value}
begin
 {}
 Result:=False;

 {Check Field}
 if Field = nil then Exit;

 Result:=(Field.Size = 3) or (Field.Size = 4);
end;

{==============================================================================}

function HIDIsSignedField(Field:PHIDField):Boolean;
{Return True is the supplied HID field contains a signed value}
begin
 {}
 Result:=False;

 {Check Field}
 if Field = nil then Exit;

 Result:=(Field.Minimum < 0);
end;

{==============================================================================}

function HIDPageToString(Page:Word):String;
{Return a string describing a HID usage page}
begin
 {}
 Result:='Unknown';

 case Page of
  HID_PAGE_GENERIC_DESKTOP:Result:='Generic Desktop';
  HID_PAGE_SIMULATION_CONTROLS:Result:='Simulation Controls';
  HID_PAGE_VR_CONTROLS:Result:='VR Controls';
  HID_PAGE_SPORT_CONTROLS:Result:='Sport Controls';
  HID_PAGE_GAME_CONTROLS:Result:='Game Controls';
  HID_PAGE_GENERIC_DEVICE_CONTROLS:Result:='Generic Device Controls';
  HID_PAGE_KEYBOARD_KEYPAD:Result:='Keyboard/Keypad';
  HID_PAGE_LED:Result:='LED';
  HID_PAGE_BUTTON:Result:='Button';
  HID_PAGE_ORDINAL:Result:='Ordinal';
  HID_PAGE_TELEPHONY_DEVICE:Result:='Telephony Device';
  HID_PAGE_CONSUMER:Result:='Consumer';
  HID_PAGE_DIGITIZERS:Result:='Digitizers';
  HID_PAGE_HAPTICS:Result:='Haptics';
  HID_PAGE_PHYSICAL_INPUT_DEVICE:Result:='Physical Input Device';
  HID_PAGE_UNICODE:Result:='Unicode';
  HID_PAGE_EYE_AND_HEAD_TRACKERS:Result:='Eye and Head Trackers';
  HID_PAGE_AUXILIARY_DISPLAY:Result:='Auxiliary Display';
  HID_PAGE_SENSORS:Result:='Sensors';
  HID_PAGE_MEDICAL_INSTRUMENT:Result:='Medical Instrument';
  HID_PAGE_BRAILLE_DISPLAY:Result:='Braille Display';
  HID_PAGE_LIGHTING_AND_ILLUMINATION:Result:='Lighting And Illumination';
  HID_PAGE_MONITOR:Result:='Monitor';
  HID_PAGE_MONITOR_ENUMERATED:Result:='Monitor Enumerated';
  HID_PAGE_VESA_VIRTUAL_CONTROLS:Result:='VESA Virtual Controls';
  HID_PAGE_POWER:Result:='Power';
  HID_PAGE_BATTERY_SYSTEM:Result:='Battery System';
  HID_PAGE_BARCODE_SCANNER:Result:='Barcode Scanner';
  HID_PAGE_SCALES:Result:='Scales';
  HID_PAGE_MAGNETIC_STRIPE_READER:Result:='Magnetic Stripe Reader';
  HID_PAGE_CAMERA_CONTROL:Result:='Camera Control';
  HID_PAGE_ARCADE:Result:='Arcade';
  HID_PAGE_GAMING_DEVICE:Result:='Gaming Device';
  HID_PAGE_FIDO_ALLIANCE:Result:='FIDO Alliance';
 end;

 Result:=Result + ' Page';
end;

{==============================================================================}

function HIDUsageToString(Page,Usage,Count:Word):String;
{Return a string describing a HID usage within the given page}
begin
 {}
 Result:='Unknown';

 case Page of
  HID_PAGE_GENERIC_DESKTOP:begin
    case Usage of
     HID_DESKTOP_POINTER:Result:='Pointer';
     HID_DESKTOP_MOUSE:Result:='Mouse';
     HID_DESKTOP_JOYSTICK:Result:='Joystick';
     HID_DESKTOP_GAMEPAD:Result:='Gamepad';
     HID_DESKTOP_KEYBOARD:Result:='Keyboard';
     HID_DESKTOP_KEYPAD:Result:='Keypad';
     {Some usages omitted}
     HID_DESKTOP_X:Result:='X';
     HID_DESKTOP_Y:Result:='Y';
     HID_DESKTOP_Z:Result:='Z';
     {Some usages omitted}
     HID_DESKTOP_WHEEL:Result:='Wheel';
     HID_DESKTOP_HAT_SWITCH:Result:='Hat Switch';
     {Some usages omitted}
     HID_DESKTOP_START:Result:='Start';
     HID_DESKTOP_SELECT:Result:='Select';
    end;
   end;
  HID_PAGE_LED:begin
    case Usage of
     HID_LED_NUM_LOCK:Result:='Num Lock';
     HID_LED_CAPS_LOCK:Result:='Caps Lock';
     HID_LED_SCROLL_LOCK:Result:='Scroll Lock';
     HID_LED_COMPOSE:Result:='Compose';
     HID_LED_KANA:Result:='Kana';
     HID_LED_POWER:Result:='Power';
     HID_LED_SHIFT:Result:='Shift';
     HID_LED_DO_NOT_DISTURB:Result:='Do Not Disturb';
     HID_LED_MUTE:Result:='Mute';
    end;

    if Count > 1 then Result:=Result + '..' + HIDUsageToString(Page,Usage + Count - 1,1);
   end;
  HID_PAGE_BUTTON:begin
    case Usage of
     HID_BUTTON_PRIMARY:Result:='Primary';
     HID_BUTTON_SECONDARY:Result:='Secondary';
     HID_BUTTON_TERTIARY:Result:='Tertiary';
     HID_BUTTON_4..HID_BUTTON_65535:Result:='Button' + IntToStr(Usage);
    end;

    if Count > 1 then Result:=Result + '..' + HIDUsageToString(Page,Usage + Count - 1,1);
   end;
 end;
end;

{==============================================================================}

function HIDUnitTypeToString(UnitType:LongWord):String;
{Return a string describing a HID unit type}
begin
 {}
 Result:='Unknown';

 case UnitType of
  {SI Base Units}
  HID_GLOBAL_UNIT_SI_ROTATION:Result:='Radians';
  HID_GLOBAL_UNIT_SI_LENGTH:Result:='Centimeters';
  HID_GLOBAL_UNIT_SI_MASS:Result:='Grams';
  HID_GLOBAL_UNIT_SI_TIME:Result:='Seconds';
  HID_GLOBAL_UNIT_SI_TEMPERATURE:Result:='Kelvin';
  HID_GLOBAL_UNIT_SI_CURRENT:Result:='Amperes';
  HID_GLOBAL_UNIT_SI_LIGHT:Result:='Candelas';
  {English Base Units}
  HID_GLOBAL_UNIT_ENGLISH_ROTATION:Result:='Degrees';
  HID_GLOBAL_UNIT_ENGLISH_LENGTH:Result:='Inches';
  HID_GLOBAL_UNIT_ENGLISH_TIME:Result:='Seconds';
  HID_GLOBAL_UNIT_ENGLISH_TEMPERATURE:Result:='Fahrenheit';
  HID_GLOBAL_UNIT_ENGLISH_CURRENT:Result:='Amperes';
  HID_GLOBAL_UNIT_ENGLISH_LIGHT:Result:='Candelas';
 end;
end;

{==============================================================================}

function HIDReportKindToString(Kind:Byte):String;
{Return a string describing a HID report type}
begin
 {}
 Result:='Unknown';

 case Kind of
  HID_REPORT_INPUT:Result:='Input';
  HID_REPORT_OUTPUT:Result:='Output';
  HID_REPORT_FEATURE:Result:='Feature';
 end;
end;

{==============================================================================}

function HIDReportFlagsToString(Flags:LongWord):String;
{Return a string describing the flags of a HID report}
begin
 {}
 Result:='';

 if (Flags and HID_MAIN_ITEM_CONSTANT) <> 0 then Result:=Result + 'Constant, ' else Result:=Result + 'Data, ';
 if (Flags and HID_MAIN_ITEM_VARIABLE) <> 0 then Result:=Result + 'Variable, ' else Result:=Result + 'Array, ';
 if (Flags and HID_MAIN_ITEM_RELATIVE) <> 0 then Result:=Result + 'Relative, ' else Result:=Result + 'Absolute, ';
 if (Flags and HID_MAIN_ITEM_WRAP) <> 0 then Result:=Result + 'Wrap, ' else Result:=Result + 'No Wrap, ';
 if (Flags and HID_MAIN_ITEM_NON_LINEAR) <> 0 then Result:=Result + 'Non Linear, ' else Result:=Result + 'Linear, ';
 if (Flags and HID_MAIN_ITEM_NO_PREFERRED) <> 0 then Result:=Result + 'No Preferred, ' else Result:=Result + 'Preferred State, ';
 if (Flags and HID_MAIN_ITEM_NULL_STATE) <> 0 then Result:=Result + 'Null state, ' else Result:=Result + 'No Null position, ';
 if (Flags and HID_MAIN_ITEM_BUFFERED_BYTES) <> 0 then Result:=Result + 'Buffered Bytes, ' else Result:=Result + 'Bit Field';
end;

{==============================================================================}

function HIDCollectionFlagsToString(Flags:LongWord):String;
{Return a string describing the flags of a HID collection}
begin
 {}
 Result:='None';

 case Flags of
  HID_MAIN_COLLECTION_PHYSICAL:Result:='Physical';
  HID_MAIN_COLLECTION_APPLICATION:Result:='Application';
  HID_MAIN_COLLECTION_LOGICAL:Result:='Logical';
  HID_MAIN_COLLECTION_REPORT:Result:='Report';
  HID_MAIN_COLLECTION_NAMED_ARRAY:Result:='Named Array';
  HID_MAIN_COLLECTION_USAGE_SWITCH:Result:='Usage Switch';
  HID_MAIN_COLLECTION_USAGE_MODIFIER:Result:='Usage Modifier';
 end;
end;

{==============================================================================}

procedure HIDLog(Level:LongWord;Device:PHIDDevice;const AText:String);
var
 WorkBuffer:String;
begin
 {}
 {Check Level}
 if Level < HID_DEFAULT_LOG_LEVEL then Exit;

 WorkBuffer:='';
 {Check Level}
 if Level = HID_LOG_LEVEL_DEBUG then
  begin
   WorkBuffer:=WorkBuffer + '[DEBUG] ';
  end
 else if Level = HID_LOG_LEVEL_WARN then
  begin
   WorkBuffer:=WorkBuffer + '[WARN] ';
  end
 else if Level = HID_LOG_LEVEL_ERROR then
  begin
   WorkBuffer:=WorkBuffer + '[ERROR] ';
  end;

 {Add Prefix}
 WorkBuffer:=WorkBuffer + 'HID: ';

 {Check Device}
 if Device <> nil then
  begin
   WorkBuffer:=WorkBuffer + HID_NAME_PREFIX + IntToStr(Device.HIDId) + ': ';
  end;

 {Output Logging}
 LoggingOutputEx(LOGGING_FACILITY_HID,LogLevelToLoggingSeverity(Level),'HID',WorkBuffer + AText);
end;

{==============================================================================}

procedure HIDLogInfo(Device:PHIDDevice;const AText:String); {inline;}
begin
 {}
 HIDLog(HID_LOG_LEVEL_INFO,Device,AText);
end;

{==============================================================================}

procedure HIDLogWarn(Device:PHIDDevice;const AText:String); {inline;}
begin
 {}
 HIDLog(HID_LOG_LEVEL_WARN,Device,AText);
end;

{==============================================================================}

procedure HIDLogError(Device:PHIDDevice;const AText:String); {inline;}
begin
 {}
 HIDLog(HID_LOG_LEVEL_ERROR,Device,AText);
end;

{==============================================================================}

procedure HIDLogDebug(Device:PHIDDevice;const AText:String); {inline;}
begin
 {}
 HIDLog(HID_LOG_LEVEL_DEBUG,Device,AText);
end;

{==============================================================================}
{==============================================================================}
{HID Device Helper Functions}
function HIDDeviceGetCount:LongWord; inline;
{Get the current HID Device count}
begin
 {}
 Result:=HIDDeviceTableCount;
end;

{==============================================================================}

function HIDDeviceCheck(Device:PHIDDevice):PHIDDevice;
{Check if the supplied HID Device is in the device table}
var
 Current:PHIDDevice;
begin
 {}
 Result:=nil;

 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(HIDDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Device}
    Current:=HIDDeviceTable;
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
    CriticalSectionUnlock(HIDDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

function HIDDeviceTypeToString(HIDType:LongWord):String;
{Return a string describing the HID device type (eg HID_TYPE_USB)}
begin
 {}
 Result:='HID_TYPE_UNKNOWN';

 if HIDType <= HID_TYPE_MAX then
  begin
   Result:=HID_TYPE_NAMES[HIDType];
  end;
end;

{==============================================================================}

function HIDDeviceStateToString(HIDState:LongWord):String;
{Return a string describing the HID device state (eg HID_STATE_ATTACHED)}
begin
 {}
 Result:='HID_STATE_UNKNOWN';

 if HIDState <= HID_STATE_MAX then
  begin
   Result:=HID_STATE_NAMES[HIDState];
  end;
end;

{==============================================================================}

function HIDDeviceStateToNotification(State:LongWord):LongWord;
{Convert a Device state value into the notification code for device notifications}
begin
 {}
 Result:=DEVICE_NOTIFICATION_NONE;

 {Check State}
 case State of
  HID_STATE_DETACHED:Result:=DEVICE_NOTIFICATION_DETACH;
  HID_STATE_DETACHING:Result:=DEVICE_NOTIFICATION_DETACHING;
  HID_STATE_ATTACHING:Result:=DEVICE_NOTIFICATION_ATTACHING;
  HID_STATE_ATTACHED:Result:=DEVICE_NOTIFICATION_ATTACH;
 end;
end;

{==============================================================================}
{==============================================================================}
{HID Consumer Helper Functions}
function HIDConsumerGetCount:LongWord; inline;
{Get the current HID Consumer count}
begin
 {}
 Result:=HIDConsumerTableCount;
end;

{==============================================================================}

function HIDConsumerCheck(Consumer:PHIDConsumer):PHIDConsumer;
{Check if the supplied HID Consumer is in the consumer table}
var
 Current:PHIDConsumer;
begin
 {}
 Result:=nil;

 {Check Consumer}
 if Consumer = nil then Exit;
 if Consumer.Driver.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(HIDConsumerTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Consumer}
    Current:=HIDConsumerTable;
    while Current <> nil do
     begin
      {Check Consumer}
      if Current = Consumer then
       begin
        Result:=Consumer;
        Exit;
       end;

      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(HIDConsumerTableLock);
   end;
  end;
end;

{==============================================================================}
{==============================================================================}
{HID Internal Functions}

{==============================================================================}
{==============================================================================}

initialization
 HIDInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.

