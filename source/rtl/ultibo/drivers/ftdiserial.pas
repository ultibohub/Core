{
FTDI USB to Serial Driver.

Copyright (C) 2025 - SoftOz Pty Ltd.

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

  Linux - \drivers\usb\serial\ftdi_sio.c - Copyright (C) 2009 - 2013 Johan Hovold and others.
  Linux - \drivers\usb\serial\ftdi_sio.h
  Linux - \drivers\usb\serial\ftdi_sio_ids.h
  Linux - \drivers\usb\serial\usb-serial.c - Copyright (C) 2009 - 2013 Johan Hovold and others.
  Linux - \drivers\usb\serial\generic.c - Copyright (C) 2010 - 2013 Johan Hovold and others.

References
==========

 FTDI - https://en.wikipedia.org/wiki/FTDI
        http://www.ftdichip.com/FTProducts.htm

FTDI Serial
===========

 This driver is intended to support a range of USB to Serial converter chips manufactured by
 Future Technology Devices International Ltd (FTDI). These include devices using the FT232H,
 FT-X (FT201X, FT230X, FT231X), FT8U100AX and FT4232H as well as others, many of these chips
 utilize similar communications protocols and where different the driver attempts to account
 for those differences.

 The driver also supports communication with many Arduino models that contain an FTDI serial
 device on board rather than the USB CDC ADM device found in more recent models.

 Any device recognized as an FTDI Serial is presented as a generic serial interface device that
 can be accessed using the API in the Serial unit. It should not be necessary to directly call
 any of the functions in this unit from application code.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

{$IFNDEF FPC_DOTTEDUNITS}
unit FTDISerial;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Core.GlobalConfig,
  Core.GlobalConst,
  Core.GlobalTypes,
  Core.Platform,
  Core.Threads,
  Core.Devices,
  Core.USB,
  Core.Serial,
  System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Devices,
  USB,
  Serial,
  SysUtils;
{$ENDIF FPC_DOTTEDUNITS}

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {FTDI Serial specific constants}
 FTDISERIAL_DRIVER_NAME = 'FTDI USB to Serial Driver'; {Name of FTDI Serial driver}

 FTDISERIAL_SERIAL_DESCRIPTION = 'FTDI USB to Serial'; {Description of FTDI Serial device}

 FTDISERIAL_MIN_DATABITS = SERIAL_DATA_7BIT;
 FTDISERIAL_MAX_DATABITS = SERIAL_DATA_8BIT;

 FTDISERIAL_MIN_STOPBITS = SERIAL_STOP_1BIT;
 FTDISERIAL_MAX_STOPBITS = SERIAL_STOP_1BIT5;

 FTDISERIAL_MAX_PARITY = SERIAL_PARITY_SPACE;

 FTDISERIAL_MAX_FLOW = SERIAL_FLOW_DSR_DTR;

 {FTDI Serial chip types}
 FTDISERIAL_CHIP_NONE      = 0;
 FTDISERIAL_CHIP_SIO       = 1;
 FTDISERIAL_CHIP_FT8U232AM = 2;
 FTDISERIAL_CHIP_FT232BM   = 3;
 FTDISERIAL_CHIP_FT2232C   = 4;
 FTDISERIAL_CHIP_FT232RL   = 5;
 FTDISERIAL_CHIP_FT2232H   = 6;
 FTDISERIAL_CHIP_FT4232H   = 7;
 FTDISERIAL_CHIP_FT232H    = 8;
 FTDISERIAL_CHIP_FTX       = 9;

 {FTDI Serial SIO Chip baud rate divisors}
 FTDISERIAL_SIO_BAUD300    = 0;
 FTDISERIAL_SIO_BAUD600    = 1;
 FTDISERIAL_SIO_BAUD1200   = 2;
 FTDISERIAL_SIO_BAUD2400   = 3;
 FTDISERIAL_SIO_BAUD4800   = 4;
 FTDISERIAL_SIO_BAUD9600   = 5;
 FTDISERIAL_SIO_BAUD19200  = 6;
 FTDISERIAL_SIO_BAUD38400  = 7;
 FTDISERIAL_SIO_BAUD57600  = 8;
 FTDISERIAL_SIO_BAUD115200 = 9;

 {FTDI Serial Bulk IN/OUT sizes}
 FTDISERIAL_BULK_IN_SIZE  = 512;
 FTDISERIAL_BULK_OUT_SIZE = 256;

 {FTDI Serial USB timeouts}
 FTDISERIAL_TIMEOUT = 5000;
 FTDISERIAL_SHORT_TIMEOUT = 1000;

 {FTDI Serial USB requests (ftdi_sio.h)}
 FTDISERIAL_RESET_REQUEST             = 0;  {Reset the port}
 FTDISERIAL_SET_MODEM_CTRL_REQUEST    = 1;  {Set the modem control register}
 FTDISERIAL_SET_FLOW_CTRL_REQUEST     = 2;  {Set flow control register}
 FTDISERIAL_SET_BAUD_RATE_REQUEST     = 3;  {Set baud rate}
 FTDISERIAL_SET_DATA_REQUEST          = 4;  {Set the data characteristics of the port}
 FTDISERIAL_GET_MODEM_STATUS_REQUEST  = 5;  {Retrieve current value of modem status register}
 FTDISERIAL_SET_EVENT_CHAR_REQUEST    = 6;  {Set the event character}
 FTDISERIAL_SET_ERROR_CHAR_REQUEST    = 7;  {Set the error character}
 FTDISERIAL_SET_LATENCY_TIMER_REQUEST = 9;  {Set the latency timer}
 FTDISERIAL_GET_LATENCY_TIMER_REQUEST = 10; {Get the latency timer}

 {FTDI Serial USB request types (ftdi_sio.h)}
 FTDISERIAL_RESET_REQUEST_TYPE             = $40;
 FTDISERIAL_SET_MODEM_CTRL_REQUEST_TYPE    = $40;
 FTDISERIAL_SET_FLOW_CTRL_REQUEST_TYPE     = $40;
 FTDISERIAL_SET_BAUD_RATE_REQUEST_TYPE     = $40;
 FTDISERIAL_SET_DATA_REQUEST_TYPE           = $40;
 FTDISERIAL_GET_MODEM_STATUS_REQUEST_TYPE  = $C0;
 FTDISERIAL_SET_EVENT_CHAR_REQUEST_TYPE    = $40;
 FTDISERIAL_SET_ERROR_CHAR_REQUEST_TYPE    = $40;
 FTDISERIAL_SET_LATENCY_TIMER_REQUEST_TYPE = $40;
 FTDISERIAL_GET_LATENCY_TIMER_REQUEST_TYPE = $C0;

 {FTDI Serial reset constants (ftdi_sio.h)}  {wValue: Control Value}
 FTDISERIAL_RESET_SIO      = 0;              { 0 = Reset SIO}
 FTDISERIAL_RESET_PURGE_RX = 1;              { 1 = Purge RX buffer}
 FTDISERIAL_RESET_PURGE_TX = 2;              { 2 = Purge TX buffer}

 {FTDI Serial set modem control constants (ftdi_sio.h)}                                        {wValue: ControlValue}
 FTDISERIAL_SET_MODEM_CTRL_NONE       = 0;
 FTDISERIAL_SET_MODEM_CTRL_DTR_ENABLE = 1;                                                     { B0    DTR state (0 = reset / 1 = set)}
 FTDISERIAL_SET_MODEM_CTRL_DTR_HIGH   = (1 or (FTDISERIAL_SET_MODEM_CTRL_DTR_ENABLE  shl 8));  { B1    RTS state (0 = reset / 1 = set)}
 FTDISERIAL_SET_MODEM_CTRL_DTR_LOW    = (0 or (FTDISERIAL_SET_MODEM_CTRL_DTR_ENABLE  shl 8));  { B2..7 Reserved}
 FTDISERIAL_SET_MODEM_CTRL_RTS_ENABLE = 2;                                                     { B8    DTR state enable (0 = ignore / 1 = use DTR state)}
 FTDISERIAL_SET_MODEM_CTRL_RTS_HIGH   = (2 or (FTDISERIAL_SET_MODEM_CTRL_RTS_ENABLE shl 8));   { B9    RTS state enable (0 = ignore / 1 = use RTS state)}
 FTDISERIAL_SET_MODEM_CTRL_RTS_LOW    = (0 or (FTDISERIAL_SET_MODEM_CTRL_RTS_ENABLE shl 8));   { B10..15 Reserved}

 {FTDI Serial set flow control constants (ftdi_sio.h)} {wIndex: Protocol/Port - hIndex is protocol / lIndex is port}
 FTDISERIAL_SET_FLOW_CTRL_NONE     = (0 shl 8);        { hIndex protocol}
 FTDISERIAL_SET_FLOW_CTRL_RTS_CTS  = (1 shl 8);        {  B0 Output handshaking using RTS/CTS (0 = disabled / 1 = enabled)}
 FTDISERIAL_SET_FLOW_CTRL_DTR_DSR  = (2 shl 8);        {  B1 Output handshaking using DTR/DSR (0 = disabled / 1 = enabled)}
 FTDISERIAL_SET_FLOW_CTRL_XON_XOFF = (4 shl 8);        {  B2 Xon/Xoff handshaking (0 = disabled / 1 = enabled)}

 {FTDI Serial set data constants (ftdi_sio.h)}   {Data characteristics}
 FTDISERIAL_SET_DATA_PARITY_NONE  = (0 shl 8);   { B0..7   Number of data bits}
 FTDISERIAL_SET_DATA_PARITY_ODD   = (1 shl 8);   { B8..10  Parity (0 = None / 1 = Odd / 2 = Even / 3 = Mark / 4 = Space)}
 FTDISERIAL_SET_DATA_PARITY_EVEN  = (2 shl 8);   { B11..13 Stop Bits (0 = 1 / 1 = 1.5 / 2 = 2)}
 FTDISERIAL_SET_DATA_PARITY_MARK  = (3 shl 8);   { B14 (1 = TX ON (break) / 0 = TX OFF (normal state))}
 FTDISERIAL_SET_DATA_PARITY_SPACE = (4 shl 8);   { B15 Reserved}
 FTDISERIAL_SET_DATA_STOP_BITS_1  = (0 shl 11);
 FTDISERIAL_SET_DATA_STOP_BITS_15 = (1 shl 11);
 FTDISERIAL_SET_DATA_STOP_BITS_2  = (2 shl 11);
 FTDISERIAL_SET_BREAK             = (1 shl 14);

 {FTDI Serial get modem status constants (ftdi_sio.h)} {One byte of data is returned}
 FTDISERIAL_GET_MODEM_STATUS_CTS  = $10;               { B0..3 0}
 FTDISERIAL_GET_MODEM_STATUS_DSR  = $20;               { B4    CTS (0 = inactive / 1 = active)}
 FTDISERIAL_GET_MODEM_STATUS_RI   = $40;               { B5    DSR (0 = inactive / 1 = active)}
 FTDISERIAL_GET_MODEM_STATUS_RLSD = $80;               { B6    Ring Indicator (RI) (0 = inactive / 1 = active)}
                                                       { B7    Receive Line Signal Detect (RLSD) (0 = inactive / 1 = active)}

 {FTDI Serial receive status constants (ftdi_sio.h)} {First two bytes of received packet}
 FTDISERIAL_RECEIVE_STATUS0_CTS   = (1 shl 4);       {Byte 0: Modem Status}
 FTDISERIAL_RECEIVE_STATUS0_DSR   = (1 shl 5);       { B0    Reserved - must be 1}
 FTDISERIAL_RECEIVE_STATUS0_RI    = (1 shl 6);       { B1..3 Reserved - must be 0}
 FTDISERIAL_RECEIVE_STATUS0_RLSD  = (1 shl 7);       { B4    Clear to Send (CTS)}
                                                     { B5    Data Set Ready (DSR)}
                                                     { B6    Ring Indicator (RI)}
                                                     { B7    Receive Line Signal Detect (RLSD)}
                                                     {Byte 1: Line Status}
 FTDISERIAL_RECEIVE_STATUS1_DR    = (1 shl 0);       { B0    Data Ready (DR)}
 FTDISERIAL_RECEIVE_STATUS1_OE    = (1 shl 1);       { B1    Overrun Error (OE)}
 FTDISERIAL_RECEIVE_STATUS1_PE    = (1 shl 2);       { B2    Parity Error (PE)}
 FTDISERIAL_RECEIVE_STATUS1_FE    = (1 shl 3);       { B3    Framing Error (FE)}
 FTDISERIAL_RECEIVE_STATUS1_BI    = (1 shl 4);       { B4    Break Interrupt (BI)}
 FTDISERIAL_RECEIVE_STATUS1_THRE  = (1 shl 5);       { B5    Transmitter Holding Register (THRE)}
 FTDISERIAL_RECEIVE_STATUS1_TEMT  = (1 shl 6);       { B6    Transmitter Empty (TEMT)}
 FTDISERIAL_RECEIVE_STATUS1_FIFO  = (1 shl 7);       { B7    Error in RCVR FIFO}

 FTDISERIAL_RECEIVE_STATUS0_MASK = FTDISERIAL_RECEIVE_STATUS0_CTS or FTDISERIAL_RECEIVE_STATUS0_DSR or FTDISERIAL_RECEIVE_STATUS0_RI or FTDISERIAL_RECEIVE_STATUS0_RLSD;
 FTDISERIAL_RECEIVE_STATUS1_MASK = FTDISERIAL_RECEIVE_STATUS1_OE or FTDISERIAL_RECEIVE_STATUS1_PE or FTDISERIAL_RECEIVE_STATUS1_FE or FTDISERIAL_RECEIVE_STATUS1_BI;

 {FTDI Serial interface index constants (FT2232, FT2232H and FT4232H) (ftdi_sio.h)}
 FTDISERIAL_INTERFACE_A = 1;
 FTDISERIAL_INTERFACE_B = 2;
 FTDISERIAL_INTERFACE_C = 3;
 FTDISERIAL_INTERFACE_D = 4;

 {FTDI Serial device quirks}
 FTDISERIAL_QUIRK_NONE       = $00000000;
 FTDISERIAL_QUIRK_8U2232C    = $00000001;
 FTDISERIAL_QUIRK_HE_TIRA1   = $00000002;
 FTDISERIAL_QUIRK_USB_UIRT   = $00000004;
 FTDISERIAL_QUIRK_JTAG       = $00000008;
 FTDISERIAL_QUIRK_NDI_DEVICE = $00000010;
 FTDISERIAL_QUIRK_STMCLITE   = $00000020;

 {FTDI Vendor and Product ID constants}
 {Devices using FTDI VID}
 FTDI_VID = $0403; {Vendor Id}

 {Original FTDI device PIDs}
 FTDI_8U232AM_PID = $6001; {Similar device to SIO above}
 FTDI_8U232AM_ALT_PID = $6006; {FTDI's alternate PID for above}
 FTDI_8U2232C_PID = $6010; {Dual channel device}
 FTDI_4232H_PID = $6011; {Quad channel hi-speed device}
 FTDI_232H_PID  = $6014; {Single channel hi-speed device}
 FTDI_FTX_PID   = $6015; {FT-X series (FT201X, FT230X, FT231X, etc)}
 FTDI_SIO_PID = $8372; {Product Id SIO application of 8U100AX}
 FTDI_232RL_PID  = $FBFA; {Product ID for FT232RL}

 {Third-party PIDs (using FTDI_VID)}
 {Certain versions of the official Windows FTDI driver reprogrammed counterfeit FTDI devices to PID 0. Support these devices anyway}
 FTDI_BRICK_PID  = $0000;

 FTDI_LUMEL_PD12_PID = $6002;

 {Cyber Cortex AV by Fabulous Silicon (http://fabuloussilicon.com)}
 CYBER_CORTEX_AV_PID = $8698;

 {Marvell OpenRD Base, Client (http://www.open-rd.org) OpenRD Base, Client use VID = $0403}
 MARVELL_OPENRD_PID = $9e90;

 {www.candapter.com Ewert Energy Systems CANdapter device}
 FTDI_CANDAPTER_PID = $9F80; {Product Id}

 FTDI_BM_ATOM_NANO_PID = $a559; {Basic Micro ATOM Nano USB2Serial}

 {Texas Instruments XDS100v2 JTAG / BeagleBone A3 (http://processors.wiki.ti.com/index.php/XDS100) http://beagleboard.org/bone}
 TI_XDS100V2_PID  = $a6d0;

 FTDI_NXTCAM_PID  = $ABB8; {NXTCam for Mindstorms NXT}
 FTDI_EV3CON_PID  = $ABB9; {Mindstorms EV3 Console Adapter}

 {US Interface Navigator (http://www.usinterface.com/)}
 FTDI_USINT_CAT_PID = $b810; {Navigator CAT and 2nd PTT lines}
 FTDI_USINT_WKEY_PID = $b811; {Navigator WKEY and FSK lines}
 FTDI_USINT_RS232_PID = $b812; {Navigator RS232 and CONFIG lines}

 {OOCDlink by Joern Kaipf (http://www.joernonline.de/)}
 FTDI_OOCDLINK_PID = $baf8; {Amontec JTAGkey}

 {Luminary Micro Stellaris Boards, VID = FTDI_VID}
 {FTDI 2332C Dual channel device, side A=245 FIFO (JTAG), Side B=RS232 UART}
 LMI_LM3S_DEVEL_BOARD_PID = $bcd8;
 LMI_LM3S_EVAL_BOARD_PID  = $bcd9;
 LMI_LM3S_ICDI_BOARD_PID  = $bcda;

 {Revolution Education Limited / PICAXE (http://www.picaxe.com)}
 FTDI_REVED_AXE027_PID = $BD90; {AXE027 PICAXE USB cable}

 FTDI_TURTELIZER_PID = $BDC8; {JTAG/RS-232 adapter by egnite GmbH}

 {OpenDCC (www.opendcc.de) product id}
 FTDI_OPENDCC_PID = $BFD8;
 FTDI_OPENDCC_SNIFFER_PID = $BFD9;
 FTDI_OPENDCC_THROTTLE_PID = $BFDA;
 FTDI_OPENDCC_GATEWAY_PID = $BFDB;
 FTDI_OPENDCC_GBM_PID = $BFDC;
 FTDI_OPENDCC_GBM_BOOST_PID = $BFDD;

 {NZR SEM 16+ USB (http://www.nzr.de)}
 FTDI_NZR_SEM_USB_PID = $C1E0; {NZR SEM-LOG16+}

 {RR-CirKits LocoBuffer USB (http://www.rr-cirkits.com)}
 FTDI_RRCIRKITS_LOCOBUFFER_PID = $c7d0; {LocoBuffer USB}

 {DMX4ALL DMX Interfaces}
 FTDI_DMX4ALL = $C850;

 {ASK.fr devices}
 FTDI_ASK_RDR400_PID = $C991; {ASK RDR 400 series card reader}

 {www.starting-point-systems.com µChameleon device}
 FTDI_MICRO_CHAMELEON_PID = $CAA0; {Product Id}

 {Tactrix OpenPort (ECU) devices}
 FTDI_TACTRIX_OPENPORT_13M_PID = $CC48; {OpenPort 1.3 Mitsubishi}
 FTDI_TACTRIX_OPENPORT_13S_PID = $CC49; {OpenPort 1.3 Subaru}
 FTDI_TACTRIX_OPENPORT_13U_PID = $CC4A; {OpenPort 1.3 Universal}

 FTDI_DISTORTEC_JTAG_LOCK_PICK_PID = $CFF8;

 {SCS HF Radio Modems PID's (http://www.scs-ptc.com) the VID is the standard ftdi vid (FTDI_VID)}
 FTDI_SCS_DEVICE_0_PID = $D010; {SCS PTC-IIusb}
 FTDI_SCS_DEVICE_1_PID = $D011; {SCS Tracker / DSP TNC}
 FTDI_SCS_DEVICE_2_PID = $D012;
 FTDI_SCS_DEVICE_3_PID = $D013;
 FTDI_SCS_DEVICE_4_PID = $D014;
 FTDI_SCS_DEVICE_5_PID = $D015;
 FTDI_SCS_DEVICE_6_PID = $D016;
 FTDI_SCS_DEVICE_7_PID = $D017;

 {iPlus device}
 FTDI_IPLUS_PID = $D070; {Product Id}
 FTDI_IPLUS2_PID = $D071; {Product Id}

 {Gamma Scout (http://gamma-scout.com/)}
 FTDI_GAMMA_SCOUT_PID  = $D678; {Gamma Scout online}

 {Propox devices}
 FTDI_PROPOX_JTAGCABLEII_PID = $D738;
 FTDI_PROPOX_ISPCABLEIII_PID = $D739;

 {Lenz LI-USB Computer Interface.}
 FTDI_LENZ_LIUSB_PID = $D780;

 {Vardaan Enterprises Serial Interface VEUSB422R3}
 FTDI_VARDAAN_PID = $F070;

 {Xsens Technologies BV products (http://www.xsens.com)}
 XSENS_VID  = $2639;
 XSENS_AWINDA_STATION_PID = $0101;
 XSENS_AWINDA_DONGLE_PID = $0102;
 XSENS_MTW_PID  = $0200; {Xsens MTw}
 XSENS_MTDEVBOARD_PID = $0300; {Motion Tracker Development Board}
 XSENS_CONVERTER_PID = $D00D; {Xsens USB-serial converter}

 {Xsens devices using FTDI VID}
 XSENS_CONVERTER_0_PID = $D388; {Xsens USB converter}
 XSENS_CONVERTER_1_PID = $D389; {Xsens Wireless Receiver}
 XSENS_CONVERTER_2_PID = $D38A;
 XSENS_CONVERTER_3_PID = $D38B; {Xsens USB-serial converter}
 XSENS_CONVERTER_4_PID = $D38C; {Xsens Wireless Receiver}
 XSENS_CONVERTER_5_PID = $D38D; {Xsens Awinda Station}
 XSENS_CONVERTER_6_PID = $D38E;
 XSENS_CONVERTER_7_PID = $D38F;

 {Zolix (www.zolix.com.cb) product ids}
 FTDI_OMNI1509   = $D491; {Omni1509 embedded USB-serial}

 {NDI (www.ndigital.com) product ids}
 FTDI_NDI_HUC_PID  = $DA70; {NDI Host USB Converter}
 FTDI_NDI_SPECTRA_SCU_PID = $DA71; {NDI Spectra SCU}
 FTDI_NDI_FUTURE_2_PID  = $DA72; {NDI future device #2}
 FTDI_NDI_FUTURE_3_PID  = $DA73; {NDI future device #3}
 FTDI_NDI_AURORA_SCU_PID  = $DA74; {NDI Aurora SCU}

 {ChamSys Limited (www.chamsys.co.uk) USB wing/interface product IDs}
 FTDI_CHAMSYS_24_MASTER_WING_PID = $DAF8;
 FTDI_CHAMSYS_PC_WING_PID       = $DAF9;
 FTDI_CHAMSYS_USB_DMX_PID       = $DAFA;
 FTDI_CHAMSYS_MIDI_TIMECODE_PID = $DAFB;
 FTDI_CHAMSYS_MINI_WING_PID     = $DAFC;
 FTDI_CHAMSYS_MAXI_WING_PID     = $DAFD;
 FTDI_CHAMSYS_MEDIA_WING_PID    = $DAFE;
 FTDI_CHAMSYS_WING_PID  = $DAFF;

 {Westrex International devices}
 FTDI_WESTREX_MODEL_777_PID = $DC00; {Model 777}
 FTDI_WESTREX_MODEL_8900F_PID = $DC01; {Model 8900F}

 {ACG Identification Technologies GmbH products (http://www.acg.de/)}
 FTDI_ACG_HFDUAL_PID  = $DD20; {HF Dual ISO Reader (RFID)}

 {Definitions for Artemis astronomical USB based cameras Check it at http://www.artemisccd.co.uk/}
 FTDI_ARTEMIS_PID = $DF28; {All Artemis Cameras}

 {Definitions for ATIK Instruments astronomical USB based cameras Check it at http://www.atik-instruments.com/}
 FTDI_ATIK_ATK16_PID = $DF30; {ATIK ATK-16 Grayscale Camera}
 FTDI_ATIK_ATK16C_PID = $DF32; {ATIK ATK-16C Colour Camera}
 FTDI_ATIK_ATK16HR_PID = $DF31; {ATIK ATK-16HR Grayscale Camera}
 FTDI_ATIK_ATK16HRC_PID = $DF33; {ATIK ATK-16HRC Colour Camera}
 FTDI_ATIK_ATK16IC_PID   = $DF35; {ATIK ATK-16IC Grayscale Camera}

 {Yost Engineering, Inc. products (www.yostengineering.com) PID = $E050}
 FTDI_YEI_SERVOCENTER31_PID = $E050; {YEI ServoCenter3.1 USB}

 {ELV USB devices (www.elv.de). Almost all of these devices use FTDI's vendor ID (0x0403)}
 FTDI_ELV_VID = $1B1F; {ELV AG}
 FTDI_ELV_WS300_PID = $C006; {eQ3 WS 300 PC II}
 FTDI_ELV_USR_PID = $E000; {ELV Universal-Sound-Recorder}
 FTDI_ELV_MSM1_PID = $E001; {ELV Mini-Sound-Modul}
 FTDI_ELV_KL100_PID = $E002; {ELV Kfz-Leistungsmesser KL 100}
 FTDI_ELV_WS550_PID = $E004; {WS 550}
 FTDI_ELV_EC3000_PID = $E006; {ENERGY CONTROL 3000 USB}
 FTDI_ELV_WS888_PID = $E008; {WS 888}
 FTDI_ELV_TWS550_PID = $E009; {Technoline WS 550}
 FTDI_ELV_FEM_PID = $E00A; {Funk Energie Monitor}
 FTDI_ELV_FHZ1300PC_PID = $E0E8; {FHZ 1300 PC}
 FTDI_ELV_WS500_PID = $E0E9; {PC-Wetterstation (WS 500)}
 FTDI_ELV_HS485_PID = $E0EA; {USB to RS-485 adapter}
 FTDI_ELV_UMS100_PID = $E0EB; {ELV USB Master-Slave Schaltsteckdose UMS 100}
 FTDI_ELV_TFD128_PID = $E0EC; {ELV Temperatur-Feuchte-Datenlogger TFD 128}
 FTDI_ELV_FM3RX_PID = $E0ED; {ELV Messwertuebertragung FM3 RX}
 FTDI_ELV_WS777_PID = $E0EE; {Conrad WS 777}
 FTDI_ELV_EM1010PC_PID = $E0EF; {Energy monitor EM 1010 PC}
 FTDI_ELV_CSI8_PID = $E0F0; {Computer-Schalt-Interface (CSI 8)}
 FTDI_ELV_EM1000DL_PID = $E0F1; {PC-Datenlogger fuer Energiemonitor (EM 1000 DL)}
 FTDI_ELV_PCK100_PID = $E0F2; {PC-Kabeltester (PCK 100)}
 FTDI_ELV_RFP500_PID = $E0F3; {HF-Leistungsmesser (RFP 500)}
 FTDI_ELV_FS20SIG_PID = $E0F4; {Signalgeber (FS 20 SIG)}
 FTDI_ELV_UTP8_PID = $E0F5; {ELV UTP 8}
 FTDI_ELV_WS300PC_PID = $E0F6; {PC-Wetterstation (WS 300 PC)}
 FTDI_ELV_WS444PC_PID = $E0F7; {Conrad WS 444 PC}
 FTDI_PHI_FISCO_PID      = $E40B ; {PHI Fisco USB to Serial cable}
 FTDI_ELV_UAD8_PID = $F068; {USB-AD-Wandler (UAD 8)}
 FTDI_ELV_UDA7_PID = $F069; {USB-DA-Wandler (UDA 7)}
 FTDI_ELV_USI2_PID = $F06A; {USB-Schrittmotoren-Interface (USI 2)}
 FTDI_ELV_T1100_PID = $F06B; {Thermometer (T 1100)}
 FTDI_ELV_PCD200_PID = $F06C; {PC-Datenlogger (PCD 200)}
 FTDI_ELV_ULA200_PID = $F06D; {USB-LCD-Ansteuerung (ULA 200)}
 FTDI_ELV_ALC8500_PID = $F06E; {ALC 8500 Expert}
 FTDI_ELV_FHZ1000PC_PID = $F06F; {FHZ 1000 PC}
 FTDI_ELV_UR100_PID = $FB58; {USB-RS232-Umsetzer (UR 100)}
 FTDI_ELV_UM100_PID = $FB5A; {USB-Modul UM 100}
 FTDI_ELV_UO100_PID = $FB5B; {USB-Modul UO 100}
 {Additional ELV PIDs that default to using the FTDI D2XX drivers on Windows, rather than the FTDI Virtual Com Port drivers}
 FTDI_ELV_CLI7000_PID = $FB59; {Computer-Light-Interface (CLI 7000)}
 FTDI_ELV_PPS7330_PID = $FB5C; {Processor-Power-Supply (PPS 7330)}
 FTDI_ELV_TFM100_PID = $FB5D; {Temperatur-Feuchte-Messgeraet (TFM 100)}
 FTDI_ELV_UDF77_PID = $FB5E; {USB DCF Funkuhr (UDF 77)}
 FTDI_ELV_UIO88_PID = $FB5F; {USB-I/O Interface (UIO 88)}

 {EVER Eco Pro UPS (http://www.ever.com.pl/)}
 EVER_ECO_PRO_CDS = $e520; {RS-232 converter}

 {Active Robots product ids}
 FTDI_ACTIVE_ROBOTS_PID = $E548; {USB comms board}

 {Pyramid Computer GmbH}
 FTDI_PYRAMID_PID = $E6C8; {Pyramid Appliance Display}

 {www.elsterelectricity.com Elster Unicom III Optical Probe}
 FTDI_ELSTER_UNICOM_PID  = $E700; {Product Id}

 {Gude Analog- und Digitalsysteme GmbH}
 FTDI_GUDEADS_E808_PID    = $E808;
 FTDI_GUDEADS_E809_PID    = $E809;
 FTDI_GUDEADS_E80A_PID    = $E80A;
 FTDI_GUDEADS_E80B_PID    = $E80B;
 FTDI_GUDEADS_E80C_PID    = $E80C;
 FTDI_GUDEADS_E80D_PID    = $E80D;
 FTDI_GUDEADS_E80E_PID    = $E80E;
 FTDI_GUDEADS_E80F_PID    = $E80F;
 FTDI_GUDEADS_E888_PID    = $E888; {Expert ISDN Control USB}
 FTDI_GUDEADS_E889_PID    = $E889; {USB RS-232 OptoBridge}
 FTDI_GUDEADS_E88A_PID    = $E88A;
 FTDI_GUDEADS_E88B_PID    = $E88B;
 FTDI_GUDEADS_E88C_PID    = $E88C;
 FTDI_GUDEADS_E88D_PID    = $E88D;
 FTDI_GUDEADS_E88E_PID    = $E88E;
 FTDI_GUDEADS_E88F_PID    = $E88F;

 {Eclo (http://www.eclo.pt/) product IDs. PID = $EA90}
 FTDI_ECLO_COM_1WIRE_PID = $EA90; {COM to 1-Wire USB adaptor}

 {TNC-X USB-to-packet-radio adapter, versions prior to 3.0 (DLP module)}
 FTDI_TNC_X_PID  = $EBE0;

 {Teratronik product ids}
 FTDI_TERATRONIK_VCP_PID  = $EC88; {Teratronik device (preferring VCP driver on windows)}
 FTDI_TERATRONIK_D2XX_PID = $EC89; {Teratronik device (preferring D2XX driver on windows)}

 {Rig Expert Ukraine devices}
 FTDI_REU_TINY_PID  = $ED22; {RigExpert Tiny}

 {Hameg HO820 and HO870 interface (using VID = $0403)}
 HAMEG_HO820_PID   = $ed74;
 HAMEG_HO730_PID   = $ed73;
 HAMEG_HO720_PID   = $ed72;
 HAMEG_HO870_PID   = $ed71;

 {MaxStream devices www.maxstream.net}
 FTDI_MAXSTREAM_PID = $EE18; {Xbee PKG-U Module}

 {microHAM product IDs (http://www.microham.com)}
 FTDI_MHAM_KW_PID = $EEE8; {USB-KW interface}
 FTDI_MHAM_YS_PID = $EEE9; {USB-YS interface}
 FTDI_MHAM_Y6_PID = $EEEA; {USB-Y6 interface}
 FTDI_MHAM_Y8_PID = $EEEB; {USB-Y8 interface}
 FTDI_MHAM_IC_PID = $EEEC; {USB-IC interface}
 FTDI_MHAM_DB9_PID = $EEED; {USB-DB9 interface}
 FTDI_MHAM_RS232_PID = $EEEE; {USB-RS232 interface}
 FTDI_MHAM_Y9_PID = $EEEF; {USB-Y9 interface}

 {Domintell products  http://www.domintell.com}
 FTDI_DOMINTELL_DGQG_PID = $EF50; {Master}
 FTDI_DOMINTELL_DUSB_PID = $EF51; {DUSB01 module}

 {The following are the values for the Perle Systems UltraPort USB serial converters}
 FTDI_PERLE_ULTRAPORT_PID = $F0C0; {Perle UltraPort Product Id}

 {Sprog II (Andrew Crosland's SprogII DCC interface)}
 FTDI_SPROG_II  = $F0C8;

 {Two of the Tagsys RFID Readers}
 FTDI_TAGSYS_LP101_PID = $F0E9; {Tagsys L-P101 RFID}
 FTDI_TAGSYS_P200X_PID = $F0EE; {Tagsys Medio P200x RFID}

 {an infrared receiver for user access control with IR tags}
 FTDI_PIEGROUP_PID = $F208; {Product Id}

 {ACT Solutions HomePro ZWave interface (http://www.act-solutions.com/HomePro-Product-Matrix.html)}
 FTDI_ACTZWAVE_PID = $F2D0;

 {4N-GALAXY.DE PIDs for CAN-USB, USB-RS232, USB-RS422, USB-RS485, USB-TTY aktiv, USB-TTY passiv}
 FTDI_4N_GALAXY_DE_1_PID = $F3C0;
 FTDI_4N_GALAXY_DE_2_PID = $F3C1;
 FTDI_4N_GALAXY_DE_3_PID = $F3C2;

 {Ivium Technologies product IDs}
 FTDI_PALMSENS_PID = $F440;
 FTDI_IVIUM_XSTAT_PID = $F441;

 {Linx Technologies product ids}
 LINX_SDMUSBQSS_PID = $F448; {Linx SDM-USB-QS-S}
 LINX_MASTERDEVEL2_PID = $F449; {Linx Master Development 2.0}
 LINX_FUTURE_0_PID   = $F44A; {Linx future device}
 LINX_FUTURE_1_PID   = $F44B; {Linx future device}
 LINX_FUTURE_2_PID   = $F44C; {Linx future device}

 {Oceanic product ids}
 FTDI_OCEANIC_PID = $F460; {Oceanic dive instrument}

 {SUUNTO product ids}
 FTDI_SUUNTO_SPORTS_PID = $F680; {Suunto Sports instrument}

 {USB-UIRT - An infrared receiver and transmitter using the 8U232AM chip (http://www.usbuirt.com/)}
 FTDI_USB_UIRT_PID = $F850; {Product Id}

 {CCS Inc. ICDU/ICDU40 product ID - the FT232BM is used in an in-circuit-debugger unit for PIC16's/PIC18's}
 FTDI_CCSICDU20_0_PID    = $F9D0;
 FTDI_CCSICDU40_1_PID    = $F9D1;
 FTDI_CCSMACHX_2_PID     = $F9D2;
 FTDI_CCSLOAD_N_GO_3_PID = $F9D3;
 FTDI_CCSICDU64_4_PID    = $F9D4;
 FTDI_CCSPRIME8_5_PID    = $F9D5;

 {The following are the values for the Matrix Orbital LCD displays, which are the FT232BM (similar to the 8U232AM)}
 FTDI_MTXORB_0_PID      = $FA00; {Matrix Orbital Product Id}
 FTDI_MTXORB_1_PID      = $FA01; {Matrix Orbital Product Id}
 FTDI_MTXORB_2_PID      = $FA02; {Matrix Orbital Product Id}
 FTDI_MTXORB_3_PID      = $FA03; {Matrix Orbital Product Id}
 FTDI_MTXORB_4_PID      = $FA04; {Matrix Orbital Product Id}
 FTDI_MTXORB_5_PID      = $FA05; {Matrix Orbital Product Id}
 FTDI_MTXORB_6_PID      = $FA06; {Matrix Orbital Product Id}

 {Home Electronics (www.home-electro.com) USB gadgets}
 FTDI_HE_TIRA1_PID = $FA78; {Tira-1 IR transceiver}

 {Inside Accesso contactless reader (http://www.insidecontactless.com/)}
 INSIDE_ACCESSO  = $FAD0;

 {ThorLabs USB motor drivers}
 FTDI_THORLABS_PID  = $faf0; {ThorLabs USB motor drivers}

 {Protego product ids}
 PROTEGO_SPECIAL_1 = $FC70; {special/unknown device}
 PROTEGO_R2X0  = $FC71; {R200-USB TRNG unit (R210, R220, and R230)}
 PROTEGO_SPECIAL_3 = $FC72; {special/unknown device}
 PROTEGO_SPECIAL_4 = $FC73; {special/unknown device}

 {Sony Ericsson product ids}
 FTDI_DSS20_PID  = $FC82; {DSS-20 Sync Station for Sony Ericsson P800}
 FTDI_URBAN_0_PID = $FC8A; {Sony Ericsson Urban, uart #0}
 FTDI_URBAN_1_PID = $FC8B; {Sony Ericsson Urban, uart #1}

 {www.irtrans.de device}
 FTDI_IRTRANS_PID = $FC60; {Product Id}

 {RM Michaelides CANview USB (http://www.rmcan.com) (FTDI_VID) CAN fieldbus interface adapter, added by port GmbH www.port.de)}
 FTDI_RM_CANVIEW_PID = $fd60; {Product Id}
 {www.thoughttechnology.com/ TT-USB provide with procomp use ftdi_sio}
 FTDI_TTUSB_PID = $FF20; {Product Id}

 FTDI_USBX_707_PID = $F857; {ADSTech IR Blaster USBX-707 (FTDI_VID)}

 FTDI_RELAIS_PID = $FA10; {Relais device from Rudolf Gugler}

 {PCDJ use ftdi based dj-controllers. The following PID is for their DAC-2 device http://www.pcdjhardware.com/DAC2.asp (the VID is the standard ftdi vid (FTDI_VID))}
 FTDI_PCDJ_DAC2_PID = $FA88;

 FTDI_R2000KU_TRUE_RNG = $FB80; {R2000KU TRUE RNG (FTDI_VID)}

 {DIEBOLD BCS SE923 (FTDI_VID)}
 DIEBOLD_BCS_SE923_PID = $fb99;

 {www.crystalfontz.com devices use the ftdi chipset for the USB interface and the vendor id is the same}
 FTDI_XF_632_PID = $FC08; {632: 16x2 Character Display}
 FTDI_XF_634_PID = $FC09; {634: 20x4 Character Display}
 FTDI_XF_547_PID = $FC0A; {547: Two line Display}
 FTDI_XF_633_PID = $FC0B; {633: 16x2 Character Display with Keys}
 FTDI_XF_631_PID = $FC0C; {631: 20x2 Character Display}
 FTDI_XF_635_PID = $FC0D; {635: 20x4 Character Display}
 FTDI_XF_640_PID = $FC0E; {640: Two line Display}
 FTDI_XF_642_PID = $FC0F; {642: Two line Display}

 {Video Networks Limited / Homechoice in the UK use an ftdi-based device for their 1Mb broadband internet service. (the VID is the standard ftdi vid (FTDI_VID)}
 FTDI_VNHCPCUSB_D_PID = $fe38; {Product Id}

 {AlphaMicro Components AMC-232USB01 device (FTDI_VID)}
 FTDI_AMC232_PID = $FF00; {Product Id}

 {IBS elektronik product ids (FTDI_VID)}
 FTDI_IBS_US485_PID = $ff38; {IBS US485 (USB<-->RS422/485 interface)}
 FTDI_IBS_PICPRO_PID = $ff39; {IBS PIC-Programmer}
 FTDI_IBS_PCMCIA_PID = $ff3a; {IBS Card reader for PCMCIA SRAM-cards}
 FTDI_IBS_PK1_PID = $ff3b; {IBS PK1 - Particel counter}
 FTDI_IBS_RS232MON_PID = $ff3c; {IBS RS232 - Monitor}
 FTDI_IBS_APP70_PID = $ff3d; {APP 70 (dust monitoring system)}
 FTDI_IBS_PEDO_PID = $ff3e; {IBS PEDO-Modem (RF modem 868.35 MHz)}
 FTDI_IBS_PROD_PID = $ff3f; {future device}
 {www.canusb.com Lawicel CANUSB device (FTDI_VID)}
 FTDI_CANUSB_PID = $FFA8; {Product Id}

 {TavIR AVR product ids (FTDI_VID)}
 FTDI_TAVIR_STK500_PID = $FA33; {STK500 AVR programmer}

 {TIAO product ids (FTDI_VID) http://www.tiaowiki.com/w/Main_Page}
 FTDI_TIAO_UMPA_PID = $8a98; {TIAO/DIYGADGET USB Multi-Protocol Adapter}

 {NovaTech product ids (FTDI_VID)}
 FTDI_NT_ORIONLXM_PID = $7c90; {OrionLXm Substation Automation Platform}
 FTDI_NT_ORIONLX_PLUS_PID = $7c91; {OrionLX+ Substation Automation Platform}
 FTDI_NT_ORION_IO_PID = $7c92; {Orion I/O}

 {Synapse Wireless product ids (FTDI_VID) http://www.synapse-wireless.com}
 FTDI_SYNAPSE_SS200_PID = $9090; {SS200 - SNAP Stick 200}

 {CustomWare / ShipModul NMEA multiplexers product ids (FTDI_VID)}
 FTDI_CUSTOMWARE_MINIPLEX_PID = $fd48; {MiniPlex first generation NMEA Multiplexer}
 FTDI_CUSTOMWARE_MINIPLEX2_PID = $fd49; {MiniPlex-USB and MiniPlex-2 series}
 FTDI_CUSTOMWARE_MINIPLEX2WI_PID = $fd4a; {MiniPlex-2Wi}
 FTDI_CUSTOMWARE_MINIPLEX3_PID = $fd4b; {MiniPlex-3 series}

 {Third-party VID/PID combos}
 {Atmel STK541}
 ATMEL_VID  = $03eb; {Vendor ID}
 STK541_PID  = $2109; {Zigbee Controller}

 {Texas Instruments}
 TI_VID = $0451;
 TI_CC3200_LAUNCHPAD_PID = $C32A; {SimpleLink Wi-Fi CC3200 LaunchPad}

 {Blackfin gnICE JTAG http://docs.blackfin.uclinux.org/doku.php?id=hw:jtag:gnice}
 ADI_VID   = $0456;
 ADI_GNICE_PID  = $F000;
 ADI_GNICEPLUS_PID = $F001;

 {Cypress WICED USB UART}
 CYPRESS_VID = $04B4;
 CYPRESS_WICED_BT_USB_PID = $009B;
 CYPRESS_WICED_WL_USB_PID = $F900;

 {Microchip Technology, Inc}
 MICROCHIP_VID  = $04D8;
 MICROCHIP_USB_BOARD_PID = $000A; {CDC RS-232 Emulation Demo}

 {RATOC REX-USB60F}
 RATOC_VENDOR_ID  = $0584;
 RATOC_PRODUCT_ID_USB60F = $b020;
 RATOC_PRODUCT_ID_SCU18 = $b03a;

 {Infineon Technologies}
 INFINEON_VID  = $058b;
 INFINEON_TRIBOARD_TC1798_PID = $0028; {DAS JTAG TriBoard TC1798 V1.0}
 INFINEON_TRIBOARD_TC2X7_PID = $0043; {DAS JTAG TriBoard TC2X7 V1.0}

 {Acton Research Corp.}
 ACTON_VID  = $0647; {Vendor ID}
 ACTON_SPECTRAPRO_PID = $0100;

 {Contec products (http://www.contec.com)}
 CONTEC_VID  = $06CE; {Vendor ID}
 CONTEC_COM1USBH_PID = $8311; {COM-1(USB)H}

 {Mitsubishi Electric Corp. (http://www.meau.com)}
 MITSUBISHI_VID  = $06D3;
 MITSUBISHI_FXUSB_PID = $0284; {USB/RS422 converters: FX-USB-AW/-BD}

 {Definitions for B&B Electronics products}
 BANDB_VID  = $0856; {B&B Electronics Vendor ID}
 BANDB_USOTL4_PID = $AC01; {USOTL4 Isolated RS-485 Converter}
 BANDB_USTL4_PID  = $AC02; {USTL4 RS-485 Converter}
 BANDB_USO9ML2_PID = $AC03; {USO9ML2 Isolated RS-232 Converter}
 BANDB_USOPTL4_PID = $AC11;
 BANDB_USPTL4_PID = $AC12;
 BANDB_USO9ML2DR_2_PID = $AC16;
 BANDB_USO9ML2DR_PID = $AC17;
 BANDB_USOPTL4DR2_PID = $AC18; {USOPTL4R-2 2-port Isolated RS-232 Converter}
 BANDB_USOPTL4DR_PID = $AC19;
 BANDB_485USB9F_2W_PID = $AC25;
 BANDB_485USB9F_4W_PID = $AC26;
 BANDB_232USB9M_PID = $AC27;
 BANDB_485USBTB_2W_PID = $AC33;
 BANDB_485USBTB_4W_PID = $AC34;
 BANDB_TTL5USB9M_PID = $AC49;
 BANDB_TTL3USB9M_PID = $AC50;
 BANDB_ZZ_PROG1_USB_PID = $BA02;

 {Intrepid Control Systems (http://www.intrepidcs.com/) ValueCAN and NeoVI}
 INTREPID_VID  = $093C;
 INTREPID_VALUECAN_PID = $0601;
 INTREPID_NEOVI_PID = $0701;

 {WICED USB UART}
 WICED_VID = $0A5C;
 WICED_USB20706V2_PID = $6422;

 {Definitions for ID TECH (www.idt-net.com) devices}
 IDTECH_VID  = $0ACD; {ID TECH Vendor ID}
 IDTECH_IDT1221U_PID = $0300; {IDT1221U USB to RS-232 adapter}

 {Definitions for Omnidirectional Control Technology, Inc. devices}
 OCT_VID   = $0B39; {OCT vendor ID}
 {Note: OCT US101 is also rebadged as Dick Smith Electronics (NZ) XH6381}
 {Also rebadged as Dick Smith Electronics (Aus) XH6451}
 {Also rebadged as SIIG Inc. model US2308 hardware version 1}
 OCT_DK201_PID  = $0103; {OCT DK201 USB docking station}
 OCT_US101_PID  = $0421; {OCT US101 USB to RS-232}

 {Definitions for Icom Inc. devices}
 ICOM_VID  = $0C26; {Icom vendor ID}
 {Note: ID-1 is a communications tranceiver for HAM-radio operators}
 ICOM_ID_1_PID  = $0004; {ID-1 USB to RS-232}
 {Note: OPC is an Optional cable to connect an Icom Tranceiver}
 ICOM_OPC_U_UC_PID = $0018; {OPC-478UC, OPC-1122U cloning cable}
 {Note: ID-RP* devices are Icom Repeater Devices for HAM-radio}
 ICOM_ID_RP2C1_PID = $0009; {ID-RP2C Asset 1 to RS-232}
 ICOM_ID_RP2C2_PID = $000A; {ID-RP2C Asset 2 to RS-232}
 ICOM_ID_RP2D_PID = $000B; {ID-RP2D configuration port}
 ICOM_ID_RP2VT_PID = $000C; {ID-RP2V Transmit config port}
 ICOM_ID_RP2VR_PID = $000D; {ID-RP2V Receive config port}
 ICOM_ID_RP4KVT_PID = $0010; {ID-RP4000V Transmit config port}
 ICOM_ID_RP4KVR_PID = $0011; {ID-RP4000V Receive config port}
 ICOM_ID_RP2KVT_PID = $0012; {ID-RP2000V Transmit config port}
 ICOM_ID_RP2KVR_PID = $0013; {ID-RP2000V Receive config port}

 {GN Otometrics (http://www.otometrics.com)}
 GN_OTOMETRICS_VID = $0c33; {Vendor ID}
 AURICAL_USB_PID  = $0010; {Aurical USB Audiometer}

 {The following are the values for the Sealevel SeaLINK+ adapters}
 SEALEVEL_VID  = $0c52; {Sealevel Vendor ID}
 SEALEVEL_2101_PID = $2101; {SeaLINK+232 (2101/2105)}
 SEALEVEL_2102_PID = $2102; {SeaLINK+485 (2102)}
 SEALEVEL_2103_PID = $2103; {SeaLINK+232I (2103)}
 SEALEVEL_2104_PID = $2104; {SeaLINK+485I (2104)}
 SEALEVEL_2106_PID = $9020; {SeaLINK+422 (2106)}
 SEALEVEL_2201_1_PID = $2211; {SeaPORT+2/232 (2201) Port 1}
 SEALEVEL_2201_2_PID = $2221; {SeaPORT+2/232 (2201) Port 2}
 SEALEVEL_2202_1_PID = $2212; {SeaPORT+2/485 (2202) Port 1}
 SEALEVEL_2202_2_PID = $2222; {SeaPORT+2/485 (2202) Port 2}
 SEALEVEL_2203_1_PID = $2213; {SeaPORT+2 (2203) Port 1}
 SEALEVEL_2203_2_PID = $2223; {SeaPORT+2 (2203) Port 2}
 SEALEVEL_2401_1_PID = $2411; {SeaPORT+4/232 (2401) Port 1}
 SEALEVEL_2401_2_PID = $2421; {SeaPORT+4/232 (2401) Port 2}
 SEALEVEL_2401_3_PID = $2431; {SeaPORT+4/232 (2401) Port 3}
 SEALEVEL_2401_4_PID = $2441; {SeaPORT+4/232 (2401) Port 4}
 SEALEVEL_2402_1_PID = $2412; {SeaPORT+4/485 (2402) Port 1}
 SEALEVEL_2402_2_PID = $2422; {SeaPORT+4/485 (2402) Port 2}
 SEALEVEL_2402_3_PID = $2432; {SeaPORT+4/485 (2402) Port 3}
 SEALEVEL_2402_4_PID = $2442; {SeaPORT+4/485 (2402) Port 4}
 SEALEVEL_2403_1_PID = $2413; {SeaPORT+4 (2403) Port 1}
 SEALEVEL_2403_2_PID = $2423; {SeaPORT+4 (2403) Port 2}
 SEALEVEL_2403_3_PID = $2433; {SeaPORT+4 (2403) Port 3}
 SEALEVEL_2403_4_PID = $2443; {SeaPORT+4 (2403) Port 4}
 SEALEVEL_2801_1_PID = $2811; {SeaLINK+8/232 (2801) Port 1}
 SEALEVEL_2801_2_PID = $2821; {SeaLINK+8/232 (2801) Port 2}
 SEALEVEL_2801_3_PID = $2831; {SeaLINK+8/232 (2801) Port 3}
 SEALEVEL_2801_4_PID = $2841; {SeaLINK+8/232 (2801) Port 4}
 SEALEVEL_2801_5_PID = $2851; {SeaLINK+8/232 (2801) Port 5}
 SEALEVEL_2801_6_PID = $2861; {SeaLINK+8/232 (2801) Port 6}
 SEALEVEL_2801_7_PID = $2871; {SeaLINK+8/232 (2801) Port 7}
 SEALEVEL_2801_8_PID = $2881; {SeaLINK+8/232 (2801) Port 8}
 SEALEVEL_2802_1_PID = $2812; {SeaLINK+8/485 (2802) Port 1}
 SEALEVEL_2802_2_PID = $2822; {SeaLINK+8/485 (2802) Port 2}
 SEALEVEL_2802_3_PID = $2832; {SeaLINK+8/485 (2802) Port 3}
 SEALEVEL_2802_4_PID = $2842; {SeaLINK+8/485 (2802) Port 4}
 SEALEVEL_2802_5_PID = $2852; {SeaLINK+8/485 (2802) Port 5}
 SEALEVEL_2802_6_PID = $2862; {SeaLINK+8/485 (2802) Port 6}
 SEALEVEL_2802_7_PID = $2872; {SeaLINK+8/485 (2802) Port 7}
 SEALEVEL_2802_8_PID = $2882; {SeaLINK+8/485 (2802) Port 8}
 SEALEVEL_2803_1_PID = $2813; {SeaLINK+8 (2803) Port 1}
 SEALEVEL_2803_2_PID = $2823; {SeaLINK+8 (2803) Port 2}
 SEALEVEL_2803_3_PID = $2833; {SeaLINK+8 (2803) Port 3}
 SEALEVEL_2803_4_PID = $2843; {SeaLINK+8 (2803) Port 4}
 SEALEVEL_2803_5_PID = $2853; {SeaLINK+8 (2803) Port 5}
 SEALEVEL_2803_6_PID = $2863; {SeaLINK+8 (2803) Port 6}
 SEALEVEL_2803_7_PID = $2873; {SeaLINK+8 (2803) Port 7}
 SEALEVEL_2803_8_PID = $2883; {SeaLINK+8 (2803) Port 8}
 SEALEVEL_2803R_1_PID = $a02a; {SeaLINK+8 (2803-ROHS) Port 1+2}
 SEALEVEL_2803R_2_PID = $a02b; {SeaLINK+8 (2803-ROHS) Port 3+4}
 SEALEVEL_2803R_3_PID = $a02c; {SeaLINK+8 (2803-ROHS) Port 5+6}
 SEALEVEL_2803R_4_PID = $a02d; {SeaLINK+8 (2803-ROHS) Port 7+8}

 {JETI SPECTROMETER SPECBOS 1201 http://www.jeti.com/cms/index.php/instruments/other-instruments/specbos-2101}
 JETI_VID  = $0c6c;
 JETI_SPC1201_PID = $04b2;

 {FTDI USB UART chips used in construction projects from the Elektor Electronics magazine (http://www.elektor.com/)}
 ELEKTOR_VID  = $0C7D;
 ELEKTOR_FT323R_PID = $0005; {RFID-Reader, issue 09-2006}

 {Posiflex inc retail equipment (http://www.posiflex.com.tw)}
 POSIFLEX_VID  = $0d3a; {Vendor ID}
 POSIFLEX_PP7000_PID = $0300; {PP-7000II thermal printer}

 {The following are the values for two KOBIL chipcard terminals}
 KOBIL_VID  = $0d46; {KOBIL Vendor ID}
 KOBIL_CONV_B1_PID = $2020; {KOBIL Konverter for B1}
 KOBIL_CONV_KAAN_PID = $2021; {KOBIL_Konverter for KAAN}

 FTDI_NF_RIC_VID = $0DCD; {Vendor Id}
 FTDI_NF_RIC_PID = $0001; {Product Id}

 {Falcom Wireless Communications GmbH}
 FALCOM_VID  = $0F94; {Vendor Id}
 FALCOM_TWIST_PID = $0001; {Falcom Twist USB GPRS modem}
 FALCOM_SAMBA_PID = $0005; {Falcom Samba USB GPRS modem}

 {Larsen and Brusgaard AltiTrack/USBtrack}
 LARSENBRUSGAARD_VID  = $0FD8;
 LB_ALTITRACK_PID  = $0001;

 {TTi (Thurlby Thandar Instruments)}
 TTI_VID   = $103E; {Vendor Id}
 TTI_QL355P_PID  = $03E8; {TTi QL355P power supply}

 {Newport Cooperation (www.newport.com)}
 NEWPORT_VID   = $104D;
 NEWPORT_AGILIS_PID  = $3000;
 NEWPORT_CONEX_CC_PID  = $3002;
 NEWPORT_CONEX_AGP_PID  = $3006;

 {Interbiometrics USB I/O Board}
 {Developed for Interbiometrics by Rudolf Gugler}
 INTERBIOMETRICS_VID              = $1209;
 INTERBIOMETRICS_IOBOARD_PID      = $1002;
 INTERBIOMETRICS_MINI_IOBOARD_PID = $1006;

 {Testo products (http://www.testo.com/)}
 TESTO_VID   = $128D;
 TESTO_1_PID   = $0001;
 TESTO_3_PID   = $0003;

 {Mobility Electronics products.}
 MOBILITY_VID   = $1342;
 MOBILITY_USB_SERIAL_PID  = $0202; {EasiDock USB 200 serial}

 {FIC / OpenMoko, Inc. http://wiki.openmoko.org/wiki/Neo1973_Debug_Board_v3}
 FIC_VID   = $1457;
 FIC_NEO1973_DEBUG_PID = $5118;

 {Actel / Microsemi}
 ACTEL_VID = $1514;
 MICROSEMI_ARROW_SF2PLUS_BOARD_PID = $2008;

 {Olimex}
 OLIMEX_VID   = $15BA;
 OLIMEX_ARM_USB_OCD_PID  = $0003;
 OLIMEX_ARM_USB_TINY_PID = $0004;
 OLIMEX_ARM_USB_TINY_H_PID = $002a;
 OLIMEX_ARM_USB_OCD_H_PID = $002b;

 {Telldus Technologies}
 TELLDUS_VID   = $1781; {Vendor ID}
 TELLDUS_TELLSTICK_PID  = $0C30; {RF control dongle 433 MHz using FT232RL}

 {NOVITUS printers}
 NOVITUS_VID   = $1a28;
 NOVITUS_BONO_E_PID  = $6010;

 {ICPDAS I-756*U devices}
 ICPDAS_VID   = $1b5c;
 ICPDAS_I7560U_PID  = $0103;
 ICPDAS_I7561U_PID  = $0104;
 ICPDAS_I7563U_PID  = $0105;

 {Airbus Defence and Space}
 AIRBUS_DS_VID = $1e8e; {Vendor ID}
 AIRBUS_DS_P8GR = $6001; {Tetra P8GR}

 {RT Systems programming cables for various ham radios}
 {This device uses the VID of FTDI}
 RTSYSTEMS_USB_VX8_PID = $9e50; {USB-VX8 USB to 7 pin modular plug for Yaesu VX-8 radio}

 RTSYSTEMS_VID  = $2100; {Vendor ID}
 RTSYSTEMS_USB_S03_PID = $9001; {RTS-03 USB to Serial Adapter}
 RTSYSTEMS_USB_59_PID = $9e50; {USB-59 USB to 8 pin plug}
 RTSYSTEMS_USB_57A_PID = $9e51; {USB-57A USB to 4pin 3.5mm plug}
 RTSYSTEMS_USB_57B_PID = $9e52; {USB-57B USB to extended 4pin 3.5mm plug}
 RTSYSTEMS_USB_29A_PID = $9e53; {USB-29A USB to 3.5mm stereo plug}
 RTSYSTEMS_USB_29B_PID = $9e54; {USB-29B USB to 6 pin mini din}
 RTSYSTEMS_USB_29F_PID = $9e55; {USB-29F USB to 6 pin modular plug}
 RTSYSTEMS_USB_62B_PID = $9e56; {USB-62B USB to 8 pin mini din plug}
 RTSYSTEMS_USB_S01_PID = $9e57; {USB-RTS01 USB to 3.5 mm stereo plug}
 RTSYSTEMS_USB_63_PID = $9e58; {USB-63 USB to 9 pin female}
 RTSYSTEMS_USB_29C_PID = $9e59; {USB-29C USB to 4 pin modular plug}
 RTSYSTEMS_USB_81B_PID = $9e5A; {USB-81 USB to 8 pin mini din plug}
 RTSYSTEMS_USB_82B_PID = $9e5B; {USB-82 USB to 2.5 mm stereo plug}
 RTSYSTEMS_USB_K5D_PID = $9e5C; {USB-K5D USB to 8 pin modular plug}
 RTSYSTEMS_USB_K4Y_PID = $9e5D; {USB-K4Y USB to 2.5/3.5 mm plugs}
 RTSYSTEMS_USB_K5G_PID = $9e5E; {USB-K5G USB to 8 pin modular plug}
 RTSYSTEMS_USB_S05_PID = $9e5F; {USB-RTS05 USB to 2.5 mm stereo plug}
 RTSYSTEMS_USB_60_PID = $9e60; {USB-60 USB to 6 pin din}
 RTSYSTEMS_USB_61_PID = $9e61; {USB-61 USB to 6 pin mini din}
 RTSYSTEMS_USB_62_PID = $9e62; {USB-62 USB to 8 pin mini din}
 RTSYSTEMS_USB_63B_PID = $9e63; {USB-63 USB to 9 pin female}
 RTSYSTEMS_USB_64_PID = $9e64; {USB-64 USB to 9 pin male}
 RTSYSTEMS_USB_65_PID = $9e65; {USB-65 USB to 9 pin female null modem}
 RTSYSTEMS_USB_92_PID = $9e66; {USB-92 USB to 12 pin plug}
 RTSYSTEMS_USB_92D_PID = $9e67; {USB-92D USB to 12 pin plug data}
 RTSYSTEMS_USB_W5R_PID = $9e68; {USB-W5R USB to 8 pin modular plug}
 RTSYSTEMS_USB_A5R_PID = $9e69; {USB-A5R USB to 8 pin modular plug}
 RTSYSTEMS_USB_PW1_PID = $9e6A; {USB-PW1 USB to 8 pin modular plug}

 {Physik Instrumente http://www.physikinstrumente.com/en/products/}
 {These two devices use the VID of FTDI}
 PI_C865_PID = $e0a0; {PI C-865 Piezomotor Controller}
 PI_C857_PID = $e0a1; {PI Encoder Trigger Box}

 PI_VID      = $1a72; {Vendor ID}
 PI_C866_PID = $1000; {PI C-866 Piezomotor Controller}
 PI_C663_PID = $1001; {PI C-663 Mercury-Step}
 PI_C725_PID = $1002; {PI C-725 Piezomotor Controller}
 PI_E517_PID = $1005; {PI E-517 Digital Piezo Controller Operation Module}
 PI_C863_PID = $1007; {PI C-863}
 PI_E861_PID = $1008; {PI E-861 Piezomotor Controller}
 PI_C867_PID = $1009; {PI C-867 Piezomotor Controller}
 PI_E609_PID = $100D; {PI E-609 Digital Piezo Controller}
 PI_E709_PID = $100E; {PI E-709 Digital Piezo Controller}
 PI_100F_PID = $100F; {PI Digital Piezo Controller}
 PI_1011_PID = $1011; {PI Digital Piezo Controller}
 PI_1012_PID = $1012; {PI Motion Controller}
 PI_1013_PID = $1013; {PI Motion Controller}
 PI_1014_PID = $1014; {PI Device}
 PI_1015_PID = $1015; {PI Device}
 PI_1016_PID = $1016; {PI Digital Servo Module}

 {Kondo Kagaku Co.Ltd. http://www.kondo-robot.com/EN}
 KONDO_VID   = $165c;
 KONDO_USB_SERIAL_PID = $0002;

 {Bayer Ascensia Contour blood glucose meter USB-converter cable. http://winglucofacts.com/cables/}
 BAYER_VID                      = $1A79;
 BAYER_CONTOUR_CABLE_PID        = $6001;

 {Matrix Orbital Intelligent USB displays. http://www.matrixorbital.com}
 MTXORB_VID   = $1B3D;
 MTXORB_FTDI_RANGE_0100_PID = $0100;
 MTXORB_FTDI_RANGE_0101_PID = $0101;
 MTXORB_FTDI_RANGE_0102_PID = $0102;
 MTXORB_FTDI_RANGE_0103_PID = $0103;
 MTXORB_FTDI_RANGE_0104_PID = $0104;
 MTXORB_FTDI_RANGE_0105_PID = $0105;
 MTXORB_FTDI_RANGE_0106_PID = $0106;
 MTXORB_FTDI_RANGE_0107_PID = $0107;
 MTXORB_FTDI_RANGE_0108_PID = $0108;
 MTXORB_FTDI_RANGE_0109_PID = $0109;
 MTXORB_FTDI_RANGE_010A_PID = $010A;
 MTXORB_FTDI_RANGE_010B_PID = $010B;
 MTXORB_FTDI_RANGE_010C_PID = $010C;
 MTXORB_FTDI_RANGE_010D_PID = $010D;
 MTXORB_FTDI_RANGE_010E_PID = $010E;
 MTXORB_FTDI_RANGE_010F_PID = $010F;
 MTXORB_FTDI_RANGE_0110_PID = $0110;
 MTXORB_FTDI_RANGE_0111_PID = $0111;
 MTXORB_FTDI_RANGE_0112_PID = $0112;
 MTXORB_FTDI_RANGE_0113_PID = $0113;
 MTXORB_FTDI_RANGE_0114_PID = $0114;
 MTXORB_FTDI_RANGE_0115_PID = $0115;
 MTXORB_FTDI_RANGE_0116_PID = $0116;
 MTXORB_FTDI_RANGE_0117_PID = $0117;
 MTXORB_FTDI_RANGE_0118_PID = $0118;
 MTXORB_FTDI_RANGE_0119_PID = $0119;
 MTXORB_FTDI_RANGE_011A_PID = $011A;
 MTXORB_FTDI_RANGE_011B_PID = $011B;
 MTXORB_FTDI_RANGE_011C_PID = $011C;
 MTXORB_FTDI_RANGE_011D_PID = $011D;
 MTXORB_FTDI_RANGE_011E_PID = $011E;
 MTXORB_FTDI_RANGE_011F_PID = $011F;
 MTXORB_FTDI_RANGE_0120_PID = $0120;
 MTXORB_FTDI_RANGE_0121_PID = $0121;
 MTXORB_FTDI_RANGE_0122_PID = $0122;
 MTXORB_FTDI_RANGE_0123_PID = $0123;
 MTXORB_FTDI_RANGE_0124_PID = $0124;
 MTXORB_FTDI_RANGE_0125_PID = $0125;
 MTXORB_FTDI_RANGE_0126_PID = $0126;
 MTXORB_FTDI_RANGE_0127_PID = $0127;
 MTXORB_FTDI_RANGE_0128_PID = $0128;
 MTXORB_FTDI_RANGE_0129_PID = $0129;
 MTXORB_FTDI_RANGE_012A_PID = $012A;
 MTXORB_FTDI_RANGE_012B_PID = $012B;
 MTXORB_FTDI_RANGE_012C_PID = $012C;
 MTXORB_FTDI_RANGE_012D_PID = $012D;
 MTXORB_FTDI_RANGE_012E_PID = $012E;
 MTXORB_FTDI_RANGE_012F_PID = $012F;
 MTXORB_FTDI_RANGE_0130_PID = $0130;
 MTXORB_FTDI_RANGE_0131_PID = $0131;
 MTXORB_FTDI_RANGE_0132_PID = $0132;
 MTXORB_FTDI_RANGE_0133_PID = $0133;
 MTXORB_FTDI_RANGE_0134_PID = $0134;
 MTXORB_FTDI_RANGE_0135_PID = $0135;
 MTXORB_FTDI_RANGE_0136_PID = $0136;
 MTXORB_FTDI_RANGE_0137_PID = $0137;
 MTXORB_FTDI_RANGE_0138_PID = $0138;
 MTXORB_FTDI_RANGE_0139_PID = $0139;
 MTXORB_FTDI_RANGE_013A_PID = $013A;
 MTXORB_FTDI_RANGE_013B_PID = $013B;
 MTXORB_FTDI_RANGE_013C_PID = $013C;
 MTXORB_FTDI_RANGE_013D_PID = $013D;
 MTXORB_FTDI_RANGE_013E_PID = $013E;
 MTXORB_FTDI_RANGE_013F_PID = $013F;
 MTXORB_FTDI_RANGE_0140_PID = $0140;
 MTXORB_FTDI_RANGE_0141_PID = $0141;
 MTXORB_FTDI_RANGE_0142_PID = $0142;
 MTXORB_FTDI_RANGE_0143_PID = $0143;
 MTXORB_FTDI_RANGE_0144_PID = $0144;
 MTXORB_FTDI_RANGE_0145_PID = $0145;
 MTXORB_FTDI_RANGE_0146_PID = $0146;
 MTXORB_FTDI_RANGE_0147_PID = $0147;
 MTXORB_FTDI_RANGE_0148_PID = $0148;
 MTXORB_FTDI_RANGE_0149_PID = $0149;
 MTXORB_FTDI_RANGE_014A_PID = $014A;
 MTXORB_FTDI_RANGE_014B_PID = $014B;
 MTXORB_FTDI_RANGE_014C_PID = $014C;
 MTXORB_FTDI_RANGE_014D_PID = $014D;
 MTXORB_FTDI_RANGE_014E_PID = $014E;
 MTXORB_FTDI_RANGE_014F_PID = $014F;
 MTXORB_FTDI_RANGE_0150_PID = $0150;
 MTXORB_FTDI_RANGE_0151_PID = $0151;
 MTXORB_FTDI_RANGE_0152_PID = $0152;
 MTXORB_FTDI_RANGE_0153_PID = $0153;
 MTXORB_FTDI_RANGE_0154_PID = $0154;
 MTXORB_FTDI_RANGE_0155_PID = $0155;
 MTXORB_FTDI_RANGE_0156_PID = $0156;
 MTXORB_FTDI_RANGE_0157_PID = $0157;
 MTXORB_FTDI_RANGE_0158_PID = $0158;
 MTXORB_FTDI_RANGE_0159_PID = $0159;
 MTXORB_FTDI_RANGE_015A_PID = $015A;
 MTXORB_FTDI_RANGE_015B_PID = $015B;
 MTXORB_FTDI_RANGE_015C_PID = $015C;
 MTXORB_FTDI_RANGE_015D_PID = $015D;
 MTXORB_FTDI_RANGE_015E_PID = $015E;
 MTXORB_FTDI_RANGE_015F_PID = $015F;
 MTXORB_FTDI_RANGE_0160_PID = $0160;
 MTXORB_FTDI_RANGE_0161_PID = $0161;
 MTXORB_FTDI_RANGE_0162_PID = $0162;
 MTXORB_FTDI_RANGE_0163_PID = $0163;
 MTXORB_FTDI_RANGE_0164_PID = $0164;
 MTXORB_FTDI_RANGE_0165_PID = $0165;
 MTXORB_FTDI_RANGE_0166_PID = $0166;
 MTXORB_FTDI_RANGE_0167_PID = $0167;
 MTXORB_FTDI_RANGE_0168_PID = $0168;
 MTXORB_FTDI_RANGE_0169_PID = $0169;
 MTXORB_FTDI_RANGE_016A_PID = $016A;
 MTXORB_FTDI_RANGE_016B_PID = $016B;
 MTXORB_FTDI_RANGE_016C_PID = $016C;
 MTXORB_FTDI_RANGE_016D_PID = $016D;
 MTXORB_FTDI_RANGE_016E_PID = $016E;
 MTXORB_FTDI_RANGE_016F_PID = $016F;
 MTXORB_FTDI_RANGE_0170_PID = $0170;
 MTXORB_FTDI_RANGE_0171_PID = $0171;
 MTXORB_FTDI_RANGE_0172_PID = $0172;
 MTXORB_FTDI_RANGE_0173_PID = $0173;
 MTXORB_FTDI_RANGE_0174_PID = $0174;
 MTXORB_FTDI_RANGE_0175_PID = $0175;
 MTXORB_FTDI_RANGE_0176_PID = $0176;
 MTXORB_FTDI_RANGE_0177_PID = $0177;
 MTXORB_FTDI_RANGE_0178_PID = $0178;
 MTXORB_FTDI_RANGE_0179_PID = $0179;
 MTXORB_FTDI_RANGE_017A_PID = $017A;
 MTXORB_FTDI_RANGE_017B_PID = $017B;
 MTXORB_FTDI_RANGE_017C_PID = $017C;
 MTXORB_FTDI_RANGE_017D_PID = $017D;
 MTXORB_FTDI_RANGE_017E_PID = $017E;
 MTXORB_FTDI_RANGE_017F_PID = $017F;
 MTXORB_FTDI_RANGE_0180_PID = $0180;
 MTXORB_FTDI_RANGE_0181_PID = $0181;
 MTXORB_FTDI_RANGE_0182_PID = $0182;
 MTXORB_FTDI_RANGE_0183_PID = $0183;
 MTXORB_FTDI_RANGE_0184_PID = $0184;
 MTXORB_FTDI_RANGE_0185_PID = $0185;
 MTXORB_FTDI_RANGE_0186_PID = $0186;
 MTXORB_FTDI_RANGE_0187_PID = $0187;
 MTXORB_FTDI_RANGE_0188_PID = $0188;
 MTXORB_FTDI_RANGE_0189_PID = $0189;
 MTXORB_FTDI_RANGE_018A_PID = $018A;
 MTXORB_FTDI_RANGE_018B_PID = $018B;
 MTXORB_FTDI_RANGE_018C_PID = $018C;
 MTXORB_FTDI_RANGE_018D_PID = $018D;
 MTXORB_FTDI_RANGE_018E_PID = $018E;
 MTXORB_FTDI_RANGE_018F_PID = $018F;
 MTXORB_FTDI_RANGE_0190_PID = $0190;
 MTXORB_FTDI_RANGE_0191_PID = $0191;
 MTXORB_FTDI_RANGE_0192_PID = $0192;
 MTXORB_FTDI_RANGE_0193_PID = $0193;
 MTXORB_FTDI_RANGE_0194_PID = $0194;
 MTXORB_FTDI_RANGE_0195_PID = $0195;
 MTXORB_FTDI_RANGE_0196_PID = $0196;
 MTXORB_FTDI_RANGE_0197_PID = $0197;
 MTXORB_FTDI_RANGE_0198_PID = $0198;
 MTXORB_FTDI_RANGE_0199_PID = $0199;
 MTXORB_FTDI_RANGE_019A_PID = $019A;
 MTXORB_FTDI_RANGE_019B_PID = $019B;
 MTXORB_FTDI_RANGE_019C_PID = $019C;
 MTXORB_FTDI_RANGE_019D_PID = $019D;
 MTXORB_FTDI_RANGE_019E_PID = $019E;
 MTXORB_FTDI_RANGE_019F_PID = $019F;
 MTXORB_FTDI_RANGE_01A0_PID = $01A0;
 MTXORB_FTDI_RANGE_01A1_PID = $01A1;
 MTXORB_FTDI_RANGE_01A2_PID = $01A2;
 MTXORB_FTDI_RANGE_01A3_PID = $01A3;
 MTXORB_FTDI_RANGE_01A4_PID = $01A4;
 MTXORB_FTDI_RANGE_01A5_PID = $01A5;
 MTXORB_FTDI_RANGE_01A6_PID = $01A6;
 MTXORB_FTDI_RANGE_01A7_PID = $01A7;
 MTXORB_FTDI_RANGE_01A8_PID = $01A8;
 MTXORB_FTDI_RANGE_01A9_PID = $01A9;
 MTXORB_FTDI_RANGE_01AA_PID = $01AA;
 MTXORB_FTDI_RANGE_01AB_PID = $01AB;
 MTXORB_FTDI_RANGE_01AC_PID = $01AC;
 MTXORB_FTDI_RANGE_01AD_PID = $01AD;
 MTXORB_FTDI_RANGE_01AE_PID = $01AE;
 MTXORB_FTDI_RANGE_01AF_PID = $01AF;
 MTXORB_FTDI_RANGE_01B0_PID = $01B0;
 MTXORB_FTDI_RANGE_01B1_PID = $01B1;
 MTXORB_FTDI_RANGE_01B2_PID = $01B2;
 MTXORB_FTDI_RANGE_01B3_PID = $01B3;
 MTXORB_FTDI_RANGE_01B4_PID = $01B4;
 MTXORB_FTDI_RANGE_01B5_PID = $01B5;
 MTXORB_FTDI_RANGE_01B6_PID = $01B6;
 MTXORB_FTDI_RANGE_01B7_PID = $01B7;
 MTXORB_FTDI_RANGE_01B8_PID = $01B8;
 MTXORB_FTDI_RANGE_01B9_PID = $01B9;
 MTXORB_FTDI_RANGE_01BA_PID = $01BA;
 MTXORB_FTDI_RANGE_01BB_PID = $01BB;
 MTXORB_FTDI_RANGE_01BC_PID = $01BC;
 MTXORB_FTDI_RANGE_01BD_PID = $01BD;
 MTXORB_FTDI_RANGE_01BE_PID = $01BE;
 MTXORB_FTDI_RANGE_01BF_PID = $01BF;
 MTXORB_FTDI_RANGE_01C0_PID = $01C0;
 MTXORB_FTDI_RANGE_01C1_PID = $01C1;
 MTXORB_FTDI_RANGE_01C2_PID = $01C2;
 MTXORB_FTDI_RANGE_01C3_PID = $01C3;
 MTXORB_FTDI_RANGE_01C4_PID = $01C4;
 MTXORB_FTDI_RANGE_01C5_PID = $01C5;
 MTXORB_FTDI_RANGE_01C6_PID = $01C6;
 MTXORB_FTDI_RANGE_01C7_PID = $01C7;
 MTXORB_FTDI_RANGE_01C8_PID = $01C8;
 MTXORB_FTDI_RANGE_01C9_PID = $01C9;
 MTXORB_FTDI_RANGE_01CA_PID = $01CA;
 MTXORB_FTDI_RANGE_01CB_PID = $01CB;
 MTXORB_FTDI_RANGE_01CC_PID = $01CC;
 MTXORB_FTDI_RANGE_01CD_PID = $01CD;
 MTXORB_FTDI_RANGE_01CE_PID = $01CE;
 MTXORB_FTDI_RANGE_01CF_PID = $01CF;
 MTXORB_FTDI_RANGE_01D0_PID = $01D0;
 MTXORB_FTDI_RANGE_01D1_PID = $01D1;
 MTXORB_FTDI_RANGE_01D2_PID = $01D2;
 MTXORB_FTDI_RANGE_01D3_PID = $01D3;
 MTXORB_FTDI_RANGE_01D4_PID = $01D4;
 MTXORB_FTDI_RANGE_01D5_PID = $01D5;
 MTXORB_FTDI_RANGE_01D6_PID = $01D6;
 MTXORB_FTDI_RANGE_01D7_PID = $01D7;
 MTXORB_FTDI_RANGE_01D8_PID = $01D8;
 MTXORB_FTDI_RANGE_01D9_PID = $01D9;
 MTXORB_FTDI_RANGE_01DA_PID = $01DA;
 MTXORB_FTDI_RANGE_01DB_PID = $01DB;
 MTXORB_FTDI_RANGE_01DC_PID = $01DC;
 MTXORB_FTDI_RANGE_01DD_PID = $01DD;
 MTXORB_FTDI_RANGE_01DE_PID = $01DE;
 MTXORB_FTDI_RANGE_01DF_PID = $01DF;
 MTXORB_FTDI_RANGE_01E0_PID = $01E0;
 MTXORB_FTDI_RANGE_01E1_PID = $01E1;
 MTXORB_FTDI_RANGE_01E2_PID = $01E2;
 MTXORB_FTDI_RANGE_01E3_PID = $01E3;
 MTXORB_FTDI_RANGE_01E4_PID = $01E4;
 MTXORB_FTDI_RANGE_01E5_PID = $01E5;
 MTXORB_FTDI_RANGE_01E6_PID = $01E6;
 MTXORB_FTDI_RANGE_01E7_PID = $01E7;
 MTXORB_FTDI_RANGE_01E8_PID = $01E8;
 MTXORB_FTDI_RANGE_01E9_PID = $01E9;
 MTXORB_FTDI_RANGE_01EA_PID = $01EA;
 MTXORB_FTDI_RANGE_01EB_PID = $01EB;
 MTXORB_FTDI_RANGE_01EC_PID = $01EC;
 MTXORB_FTDI_RANGE_01ED_PID = $01ED;
 MTXORB_FTDI_RANGE_01EE_PID = $01EE;
 MTXORB_FTDI_RANGE_01EF_PID = $01EF;
 MTXORB_FTDI_RANGE_01F0_PID = $01F0;
 MTXORB_FTDI_RANGE_01F1_PID = $01F1;
 MTXORB_FTDI_RANGE_01F2_PID = $01F2;
 MTXORB_FTDI_RANGE_01F3_PID = $01F3;
 MTXORB_FTDI_RANGE_01F4_PID = $01F4;
 MTXORB_FTDI_RANGE_01F5_PID = $01F5;
 MTXORB_FTDI_RANGE_01F6_PID = $01F6;
 MTXORB_FTDI_RANGE_01F7_PID = $01F7;
 MTXORB_FTDI_RANGE_01F8_PID = $01F8;
 MTXORB_FTDI_RANGE_01F9_PID = $01F9;
 MTXORB_FTDI_RANGE_01FA_PID = $01FA;
 MTXORB_FTDI_RANGE_01FB_PID = $01FB;
 MTXORB_FTDI_RANGE_01FC_PID = $01FC;
 MTXORB_FTDI_RANGE_01FD_PID = $01FD;
 MTXORB_FTDI_RANGE_01FE_PID = $01FE;
 MTXORB_FTDI_RANGE_01FF_PID = $01FF;
 MTXORB_FTDI_RANGE_4701_PID = $4701;
 MTXORB_FTDI_RANGE_9300_PID = $9300;
 MTXORB_FTDI_RANGE_9301_PID = $9301;
 MTXORB_FTDI_RANGE_9302_PID = $9302;
 MTXORB_FTDI_RANGE_9303_PID = $9303;
 MTXORB_FTDI_RANGE_9304_PID = $9304;
 MTXORB_FTDI_RANGE_9305_PID = $9305;
 MTXORB_FTDI_RANGE_9306_PID = $9306;
 MTXORB_FTDI_RANGE_9307_PID = $9307;
 MTXORB_FTDI_RANGE_9308_PID = $9308;
 MTXORB_FTDI_RANGE_9309_PID = $9309;
 MTXORB_FTDI_RANGE_930A_PID = $930A;
 MTXORB_FTDI_RANGE_930B_PID = $930B;
 MTXORB_FTDI_RANGE_930C_PID = $930C;
 MTXORB_FTDI_RANGE_930D_PID = $930D;
 MTXORB_FTDI_RANGE_930E_PID = $930E;
 MTXORB_FTDI_RANGE_930F_PID = $930F;
 MTXORB_FTDI_RANGE_9310_PID = $9310;
 MTXORB_FTDI_RANGE_9311_PID = $9311;
 MTXORB_FTDI_RANGE_9312_PID = $9312;
 MTXORB_FTDI_RANGE_9313_PID = $9313;
 MTXORB_FTDI_RANGE_9314_PID = $9314;
 MTXORB_FTDI_RANGE_9315_PID = $9315;
 MTXORB_FTDI_RANGE_9316_PID = $9316;
 MTXORB_FTDI_RANGE_9317_PID = $9317;
 MTXORB_FTDI_RANGE_9318_PID = $9318;
 MTXORB_FTDI_RANGE_9319_PID = $9319;
 MTXORB_FTDI_RANGE_931A_PID = $931A;
 MTXORB_FTDI_RANGE_931B_PID = $931B;
 MTXORB_FTDI_RANGE_931C_PID = $931C;
 MTXORB_FTDI_RANGE_931D_PID = $931D;
 MTXORB_FTDI_RANGE_931E_PID = $931E;
 MTXORB_FTDI_RANGE_931F_PID = $931F;

 {The Mobility Lab (TML)}
 TML_VID   = $1B91; {Vendor ID}
 TML_USB_SERIAL_PID = $0064; {USB - Serial Converter}

 {Alti-2 products  http://www.alti-2.com}
 ALTI2_VID = $1BC9;
 ALTI2_N3_PID = $6001; {Neptune 3}

 {Ionics PlugComputer}
 IONICS_VID   = $1c0c;
 IONICS_PLUGCOMPUTER_PID  = $0102;

 {EZPrototypes (PID reseller)}
 EZPROTOTYPES_VID = $1c40;
 HJELMSLUND_USB485_ISO_PID = $0477;

 {Dresden Elektronik Sensor Terminal Board}
 DE_VID   = $1cf1; {Vendor ID}
 STB_PID   = $0001; {Sensor Terminal Board}
 WHT_PID   = $0004; {Wireless Handheld Terminal}

 {STMicroelectonics}
 ST_VID   = $0483;
 ST_STMCLT_2232_PID = $3746;
 ST_STMCLT_4232_PID = $3747;

 {Papouch products (http://www.papouch.com/)}
 PAPOUCH_VID   = $5050; {Vendor ID}
 PAPOUCH_SB485_PID  = $0100; {Papouch SB485 USB-485/422 Converter}
 PAPOUCH_AP485_PID  = $0101; {AP485 USB-RS485 Converter}
 PAPOUCH_SB422_PID  = $0102; {Papouch SB422 USB-RS422 Converter }
 PAPOUCH_SB485_2_PID  = $0103; {Papouch SB485 USB-485/422 Converter}
 PAPOUCH_AP485_2_PID  = $0104; {AP485 USB-RS485 Converter}
 PAPOUCH_SB422_2_PID  = $0105; {Papouch SB422 USB-RS422 Converter }
 PAPOUCH_SB485S_PID  = $0106; {Papouch SB485S USB-485/422 Converter}
 PAPOUCH_SB485C_PID  = $0107; {Papouch SB485C USB-485/422 Converter}
 PAPOUCH_LEC_PID   = $0300; {LEC USB Converter}
 PAPOUCH_SB232_PID  = $0301; {Papouch SB232 USB-RS232 Converter}
 PAPOUCH_TMU_PID   = $0400; {TMU USB Thermometer}
 PAPOUCH_IRAMP_PID  = $0500; {Papouch IRAmp Duplex}
 PAPOUCH_DRAK5_PID  = $0700; {Papouch DRAK5}
 PAPOUCH_QUIDO8x8_PID  = $0800; {Papouch Quido 8/8 Module}
 PAPOUCH_QUIDO4x4_PID  = $0900; {Papouch Quido 4/4 Module}
 PAPOUCH_QUIDO2x2_PID  = $0a00; {Papouch Quido 2/2 Module}
 PAPOUCH_QUIDO10x1_PID  = $0b00; {Papouch Quido 10/1 Module}
 PAPOUCH_QUIDO30x3_PID  = $0c00; {Papouch Quido 30/3 Module}
 PAPOUCH_QUIDO60x3_PID  = $0d00; {Papouch Quido 60(100)/3 Module}
 PAPOUCH_QUIDO2x16_PID  = $0e00; {Papouch Quido 2/16 Module}
 PAPOUCH_QUIDO3x32_PID  = $0f00; {Papouch Quido 3/32 Module}
 PAPOUCH_DRAK6_PID  = $1000; {Papouch DRAK6}
 PAPOUCH_UPSUSB_PID  = $8000; {Papouch UPS-USB adapter}
 PAPOUCH_MU_PID   = $8001; {MU controller}
 PAPOUCH_SIMUKEY_PID  = $8002; {Papouch SimuKey}
 PAPOUCH_AD4USB_PID  = $8003; {AD4USB Measurement Module}
 PAPOUCH_GMUX_PID  = $8004; {Papouch GOLIATH MUX}
 PAPOUCH_GMSR_PID  = $8005; {Papouch GOLIATH MSR}

 {Marvell SheevaPlug}
 MARVELL_VID  = $9e88;
 MARVELL_SHEEVAPLUG_PID = $9e8f;

 {Evolution Robotics products (http://www.evolution.com/)}
 EVOLUTION_VID  = $DEEE; {Vendor ID}
 EVOLUTION_ER1_PID = $0300; {ER1 Control Module}
 EVO_8U232AM_PID  = $02FF; {Evolution robotics RCM2 (FT232AM)}
 EVO_HYBRID_PID  = $0302; {Evolution robotics RCM4 PID (FT232BM)}
 EVO_RCM4_PID  = $0303; {Evolution robotics RCM4 PID}

 {MJS Gadgets HD Radio / XM Radio / Sirius Radio interfaces (using VID = $0403)}
 MJSG_GENERIC_PID = $9378;
 MJSG_SR_RADIO_PID = $9379;
 MJSG_XM_RADIO_PID = $937A;
 MJSG_HD_RADIO_PID = $937C;

 {D.O.Tec products (http://www.directout.eu)}
 FTDI_DOTEC_PID = $9868;

 {Xverve Signalyzer tools (http://www.signalyzer.com/)}
 XVERVE_SIGNALYZER_ST_PID = $BCA0;
 XVERVE_SIGNALYZER_SLITE_PID = $BCA1;
 XVERVE_SIGNALYZER_SH2_PID = $BCA2;
 XVERVE_SIGNALYZER_SH4_PID = $BCA4;

 {Segway Robotic Mobility Platform USB interface (using VID = $0403)}
 SEGWAY_RMP200_PID = $e729;

 {Accesio USB Data Acquisition products (http://www.accesio.com/)}
 ACCESIO_COM4SM_PID  = $D578;

 {www.sciencescope.co.uk educational dataloggers}
 FTDI_SCIENCESCOPE_LOGBOOKML_PID  = $FF18;
 FTDI_SCIENCESCOPE_LS_LOGBOOK_PID = $FF1C;
 FTDI_SCIENCESCOPE_HS_LOGBOOK_PID = $FF1D;

 {Milkymist One JTAG/Serial}
 QIHARDWARE_VID   = $20B7;
 MILKYMISTONE_JTAGSERIAL_PID = $0713;

 {CTI GmbH RS485 Converter http://www.cti-lean.com/}
 {USB-485-Mini}
 FTDI_CTI_MINI_PID = $F608;
 {USB-Nano-485}
 FTDI_CTI_NANO_PID = $F60B;

 {ZeitControl cardsystems GmbH rfid-readers http://zeitcontrol.de}
 {TagTracer MIFARE}
 FTDI_ZEITCONTROL_TAGTRACE_MIFARE_PID = $F7C0;

 {Rainforest Automation}
 {ZigBee controller}
 FTDI_RF_R106  = $8A28;

 {Product: HCP HIT GPRS modem  Manufacturer: HCP d.o.o. ATI command output: Cinterion MC55i}
 FTDI_CINTERION_MC55I_PID = $A951;

 {Product: FirmwareHubEmulator Manufacturer: Harman Becker Automotive Systems}
 FTDI_FHE_PID = $A9A0;

 {Product: Comet Caller ID decoder Manufacturer: Crucible Technologies}
 FTDI_CT_COMET_PID = $8e08;

 {Product: Z3X Box Manufacturer: Smart GSM Team}
 FTDI_Z3X_PID  = $0011;

 {Product: Cressi PC Interface Manufacturer: Cressi}
 FTDI_CRESSI_PID  = $87d0;

 {Brainboxes devices}
 BRAINBOXES_VID   = $05d1;
 BRAINBOXES_VX_001_PID  = $1001; {VX-001 ExpressCard 1 Port RS232}
 BRAINBOXES_VX_012_PID  = $1002; {VX-012 ExpressCard 2 Port RS232}
 BRAINBOXES_VX_023_PID  = $1003; {VX-023 ExpressCard 1 Port RS422/485}
 BRAINBOXES_VX_034_PID  = $1004; {VX-034 ExpressCard 2 Port RS422/485}
 BRAINBOXES_US_101_PID  = $1011; {US-101 1xRS232}
 BRAINBOXES_US_324_PID  = $1013; {US-324 1xRS422/485 1Mbaud}
 BRAINBOXES_US_606_1_PID  = $2001; {US-606 6 Port RS232 Serial Port 1 and 2}
 BRAINBOXES_US_606_2_PID  = $2002; {US-606 6 Port RS232 Serial Port 3 and 4}
 BRAINBOXES_US_606_3_PID  = $2003; {US-606 6 Port RS232 Serial Port 4 and 6}
 BRAINBOXES_US_701_1_PID  = $2011; {US-701 4xRS232 1Mbaud Port 1 and 2}
 BRAINBOXES_US_701_2_PID  = $2012; {US-701 4xRS422 1Mbaud Port 3 and 4}
 BRAINBOXES_US_279_1_PID  = $2021; {US-279 8xRS422 1Mbaud Port 1 and 2}
 BRAINBOXES_US_279_2_PID  = $2022; {US-279 8xRS422 1Mbaud Port 3 and 4}
 BRAINBOXES_US_279_3_PID  = $2023; {US-279 8xRS422 1Mbaud Port 5 and 6}
 BRAINBOXES_US_279_4_PID  = $2024; {US-279 8xRS422 1Mbaud Port 7 and 8}
 BRAINBOXES_US_346_1_PID  = $3011; {US-346 4xRS422/485 1Mbaud Port 1 and 2}
 BRAINBOXES_US_346_2_PID  = $3012; {US-346 4xRS422/485 1Mbaud Port 3 and 4}
 BRAINBOXES_US_257_PID  = $5001; {US-257 2xRS232 1Mbaud}
 BRAINBOXES_US_313_PID  = $6001; {US-313 2xRS422/485 1Mbaud}
 BRAINBOXES_US_357_PID  = $7001; {US_357 1xRS232/422/485}
 BRAINBOXES_US_842_1_PID  = $8001; {US-842 8xRS422/485 1Mbaud Port 1 and 2}
 BRAINBOXES_US_842_2_PID  = $8002; {US-842 8xRS422/485 1Mbaud Port 3 and 4}
 BRAINBOXES_US_842_3_PID  = $8003; {US-842 8xRS422/485 1Mbaud Port 5 and 6}
 BRAINBOXES_US_842_4_PID  = $8004; {US-842 8xRS422/485 1Mbaud Port 7 and 8}
 BRAINBOXES_US_160_1_PID  = $9001; {US-160 16xRS232 1Mbaud Port 1 and 2}
 BRAINBOXES_US_160_2_PID  = $9002; {US-160 16xRS232 1Mbaud Port 3 and 4}
 BRAINBOXES_US_160_3_PID  = $9003; {US-160 16xRS232 1Mbaud Port 5 and 6}
 BRAINBOXES_US_160_4_PID  = $9004; {US-160 16xRS232 1Mbaud Port 7 and 8}
 BRAINBOXES_US_160_5_PID  = $9005; {US-160 16xRS232 1Mbaud Port 9 and 10}
 BRAINBOXES_US_160_6_PID  = $9006; {US-160 16xRS232 1Mbaud Port 11 and 12}
 BRAINBOXES_US_160_7_PID  = $9007; {US-160 16xRS232 1Mbaud Port 13 and 14}
 BRAINBOXES_US_160_8_PID  = $9008; {US-160 16xRS232 1Mbaud Port 15 and 16}

 {ekey biometric systems GmbH (http://ekey.net/)}
 FTDI_EKEY_CONV_USB_PID  = $CB08; {Converter USB}

 {GE Healthcare devices}
 GE_HEALTHCARE_VID  = $1901;
 GE_HEALTHCARE_NEMO_TRACKER_PID = $0015;

 {Active Research (Actisense) devices}
 ACTISENSE_NDC_PID  = $D9A8; {NDC USB Serial Adapter}
 ACTISENSE_USG_PID  = $D9A9; {USG USB Serial Adapter}
 ACTISENSE_NGT_PID  = $D9AA; {NGT NMEA2000 Interface}
 ACTISENSE_NGW_PID  = $D9AB; {NGW NMEA2000 Gateway}
 ACTISENSE_D9AC_PID  = $D9AC; {Actisense Reserved}
 ACTISENSE_D9AD_PID  = $D9AD; {Actisense Reserved}
 ACTISENSE_D9AE_PID  = $D9AE; {Actisense Reserved}
 ACTISENSE_D9AF_PID  = $D9AF; {Actisense Reserved}
 CHETCO_SEAGAUGE_PID  = $A548; {SeaGauge USB Adapter}
 CHETCO_SEASWITCH_PID  = $A549; {SeaSwitch USB Adapter}
 CHETCO_SEASMART_NMEA2000_PID = $A54A; {SeaSmart NMEA2000 Gateway}
 CHETCO_SEASMART_ETHERNET_PID = $A54B; {SeaSmart Ethernet Gateway}
 CHETCO_SEASMART_WIFI_PID = $A5AC; {SeaSmart Wifi Gateway}
 CHETCO_SEASMART_DISPLAY_PID = $A5AD; {SeaSmart NMEA2000 Display}
 CHETCO_SEASMART_LITE_PID = $A5AE; {SeaSmart Lite USB Adapter}
 CHETCO_SEASMART_ANALOG_PID = $A5AF; {SeaSmart Analog Adapter}

 {FTDI Serial Device ID constants}
 FTDISERIAL_DEVICE_ID_COUNT = 828; {Number of supported Device IDs}

 FTDISERIAL_DEVICE_ID:array[0..FTDISERIAL_DEVICE_ID_COUNT - 1] of TUSBDeviceId = (
  (idVendor:FTDI_VID;idProduct:FTDI_BRICK_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ZEITCONTROL_TAGTRACE_MIFARE_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_CTI_MINI_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_CTI_NANO_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_AMC232_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_CANUSB_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_CANDAPTER_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_BM_ATOM_NANO_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_NXTCAM_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_EV3CON_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_SCS_DEVICE_0_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_SCS_DEVICE_1_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_SCS_DEVICE_2_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_SCS_DEVICE_3_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_SCS_DEVICE_4_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_SCS_DEVICE_5_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_SCS_DEVICE_6_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_SCS_DEVICE_7_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_USINT_CAT_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_USINT_WKEY_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_USINT_RS232_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ACTZWAVE_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_IRTRANS_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_IPLUS_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_IPLUS2_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_DMX4ALL),
  (idVendor:FTDI_VID;idProduct:FTDI_SIO_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_8U232AM_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_8U232AM_ALT_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_232RL_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_8U2232C_PID), {FTDISERIAL_QUIRK_8U2232C}
  (idVendor:FTDI_VID;idProduct:FTDI_4232H_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_232H_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_FTX_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_MICRO_CHAMELEON_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_RELAIS_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_OPENDCC_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_OPENDCC_SNIFFER_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_OPENDCC_THROTTLE_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_OPENDCC_GATEWAY_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_OPENDCC_GBM_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_OPENDCC_GBM_BOOST_PID),
  (idVendor:NEWPORT_VID;idProduct:NEWPORT_AGILIS_PID),
  (idVendor:NEWPORT_VID;idProduct:NEWPORT_CONEX_CC_PID),
  (idVendor:NEWPORT_VID;idProduct:NEWPORT_CONEX_AGP_PID),
  (idVendor:INTERBIOMETRICS_VID;idProduct:INTERBIOMETRICS_IOBOARD_PID),
  (idVendor:INTERBIOMETRICS_VID;idProduct:INTERBIOMETRICS_MINI_IOBOARD_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_SPROG_II),
  (idVendor:FTDI_VID;idProduct:FTDI_TAGSYS_LP101_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_TAGSYS_P200X_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_LENZ_LIUSB_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_XF_632_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_XF_634_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_XF_547_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_XF_633_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_XF_631_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_XF_635_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_XF_640_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_XF_642_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_DSS20_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_URBAN_0_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_URBAN_1_PID),
  (idVendor:FTDI_NF_RIC_VID;idProduct:FTDI_NF_RIC_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_VNHCPCUSB_D_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_MTXORB_0_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_MTXORB_1_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_MTXORB_2_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_MTXORB_3_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_MTXORB_4_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_MTXORB_5_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_MTXORB_6_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_R2000KU_TRUE_RNG),
  (idVendor:FTDI_VID;idProduct:FTDI_VARDAAN_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0100_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0101_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0102_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0103_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0104_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0105_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0106_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0107_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0108_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0109_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_010A_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_010B_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_010C_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_010D_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_010E_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_010F_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0110_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0111_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0112_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0113_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0114_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0115_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0116_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0117_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0118_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0119_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_011A_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_011B_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_011C_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_011D_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_011E_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_011F_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0120_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0121_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0122_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0123_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0124_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0125_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0126_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0127_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0128_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0129_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_012A_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_012B_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_012C_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_012D_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_012E_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_012F_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0130_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0131_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0132_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0133_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0134_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0135_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0136_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0137_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0138_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0139_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_013A_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_013B_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_013C_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_013D_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_013E_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_013F_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0140_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0141_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0142_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0143_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0144_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0145_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0146_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0147_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0148_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0149_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_014A_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_014B_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_014C_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_014D_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_014E_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_014F_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0150_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0151_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0152_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0153_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0154_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0155_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0156_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0157_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0158_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0159_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_015A_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_015B_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_015C_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_015D_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_015E_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_015F_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0160_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0161_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0162_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0163_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0164_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0165_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0166_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0167_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0168_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0169_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_016A_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_016B_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_016C_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_016D_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_016E_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_016F_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0170_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0171_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0172_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0173_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0174_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0175_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0176_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0177_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0178_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0179_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_017A_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_017B_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_017C_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_017D_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_017E_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_017F_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0180_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0181_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0182_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0183_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0184_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0185_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0186_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0187_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0188_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0189_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_018A_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_018B_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_018C_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_018D_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_018E_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_018F_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0190_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0191_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0192_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0193_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0194_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0195_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0196_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0197_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0198_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_0199_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_019A_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_019B_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_019C_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_019D_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_019E_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_019F_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01A0_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01A1_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01A2_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01A3_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01A4_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01A5_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01A6_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01A7_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01A8_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01A9_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01AA_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01AB_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01AC_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01AD_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01AE_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01AF_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01B0_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01B1_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01B2_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01B3_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01B4_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01B5_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01B6_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01B7_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01B8_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01B9_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01BA_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01BB_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01BC_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01BD_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01BE_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01BF_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01C0_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01C1_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01C2_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01C3_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01C4_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01C5_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01C6_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01C7_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01C8_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01C9_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01CA_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01CB_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01CC_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01CD_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01CE_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01CF_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01D0_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01D1_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01D2_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01D3_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01D4_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01D5_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01D6_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01D7_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01D8_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01D9_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01DA_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01DB_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01DC_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01DD_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01DE_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01DF_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01E0_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01E1_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01E2_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01E3_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01E4_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01E5_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01E6_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01E7_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01E8_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01E9_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01EA_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01EB_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01EC_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01ED_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01EE_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01EF_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01F0_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01F1_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01F2_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01F3_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01F4_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01F5_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01F6_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01F7_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01F8_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01F9_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01FA_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01FB_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01FC_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01FD_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01FE_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_01FF_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_4701_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_9300_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_9301_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_9302_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_9303_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_9304_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_9305_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_9306_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_9307_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_9308_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_9309_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_930A_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_930B_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_930C_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_930D_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_930E_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_930F_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_9310_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_9311_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_9312_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_9313_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_9314_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_9315_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_9316_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_9317_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_9318_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_9319_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_931A_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_931B_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_931C_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_931D_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_931E_PID),
  (idVendor:MTXORB_VID;idProduct:MTXORB_FTDI_RANGE_931F_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_PERLE_ULTRAPORT_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_PIEGROUP_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_TNC_X_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_USBX_707_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2101_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2102_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2103_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2104_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2106_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2201_1_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2201_2_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2202_1_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2202_2_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2203_1_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2203_2_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2401_1_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2401_2_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2401_3_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2401_4_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2402_1_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2402_2_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2402_3_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2402_4_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2403_1_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2403_2_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2403_3_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2403_4_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2801_1_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2801_2_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2801_3_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2801_4_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2801_5_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2801_6_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2801_7_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2801_8_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2802_1_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2802_2_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2802_3_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2802_4_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2802_5_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2802_6_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2802_7_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2802_8_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2803_1_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2803_2_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2803_3_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2803_4_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2803_5_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2803_6_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2803_7_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2803_8_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2803R_1_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2803R_2_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2803R_3_PID),
  (idVendor:SEALEVEL_VID;idProduct:SEALEVEL_2803R_4_PID),
  (idVendor:IDTECH_VID;idProduct:IDTECH_IDT1221U_PID),
  (idVendor:OCT_VID;idProduct:OCT_US101_PID),
  (idVendor:OCT_VID;idProduct:OCT_DK201_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_HE_TIRA1_PID), {FTDISERIAL_QUIRK_HE_TIRA1}
  (idVendor:FTDI_VID;idProduct:FTDI_USB_UIRT_PID), {FTDISERIAL_QUIRK_USB_UIRT}
  (idVendor:FTDI_VID;idProduct:PROTEGO_SPECIAL_1),
  (idVendor:FTDI_VID;idProduct:PROTEGO_R2X0),
  (idVendor:FTDI_VID;idProduct:PROTEGO_SPECIAL_3),
  (idVendor:FTDI_VID;idProduct:PROTEGO_SPECIAL_4),
  (idVendor:FTDI_VID;idProduct:FTDI_GUDEADS_E808_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_GUDEADS_E809_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_GUDEADS_E80A_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_GUDEADS_E80B_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_GUDEADS_E80C_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_GUDEADS_E80D_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_GUDEADS_E80E_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_GUDEADS_E80F_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_GUDEADS_E888_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_GUDEADS_E889_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_GUDEADS_E88A_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_GUDEADS_E88B_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_GUDEADS_E88C_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_GUDEADS_E88D_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_GUDEADS_E88E_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_GUDEADS_E88F_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ELV_UO100_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ELV_UM100_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ELV_UR100_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ELV_ALC8500_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_PYRAMID_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ELV_FHZ1000PC_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_IBS_US485_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_IBS_PICPRO_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_IBS_PCMCIA_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_IBS_PK1_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_IBS_RS232MON_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_IBS_APP70_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_IBS_PEDO_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_IBS_PROD_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_TAVIR_STK500_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_TIAO_UMPA_PID),  {FTDISERIAL_QUIRK_JTAG}
  (idVendor:FTDI_VID;idProduct:FTDI_NT_ORIONLXM_PID), {FTDISERIAL_QUIRK_JTAG}
  (idVendor:FTDI_VID;idProduct:FTDI_NT_ORIONLX_PLUS_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_NT_ORION_IO_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_SYNAPSE_SS200_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_CUSTOMWARE_MINIPLEX_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_CUSTOMWARE_MINIPLEX2_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_CUSTOMWARE_MINIPLEX2WI_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_CUSTOMWARE_MINIPLEX3_PID),
  {ELV devices}
  (idVendor:FTDI_ELV_VID;idProduct:FTDI_ELV_WS300_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ELV_USR_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ELV_MSM1_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ELV_KL100_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ELV_WS550_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ELV_EC3000_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ELV_WS888_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ELV_TWS550_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ELV_FEM_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ELV_CLI7000_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ELV_PPS7330_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ELV_TFM100_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ELV_UDF77_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ELV_UIO88_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ELV_UAD8_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ELV_UDA7_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ELV_USI2_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ELV_T1100_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ELV_PCD200_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ELV_ULA200_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ELV_CSI8_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ELV_EM1000DL_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ELV_PCK100_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ELV_RFP500_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ELV_FS20SIG_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ELV_UTP8_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ELV_WS300PC_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ELV_WS444PC_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ELV_FHZ1300PC_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ELV_EM1010PC_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ELV_WS500_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ELV_HS485_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ELV_UMS100_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ELV_TFD128_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ELV_FM3RX_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ELV_WS777_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_PALMSENS_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_IVIUM_XSTAT_PID),
  (idVendor:FTDI_VID;idProduct:LINX_SDMUSBQSS_PID),
  (idVendor:FTDI_VID;idProduct:LINX_MASTERDEVEL2_PID),
  (idVendor:FTDI_VID;idProduct:LINX_FUTURE_0_PID),
  (idVendor:FTDI_VID;idProduct:LINX_FUTURE_1_PID),
  (idVendor:FTDI_VID;idProduct:LINX_FUTURE_2_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_CCSICDU20_0_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_CCSICDU40_1_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_CCSMACHX_2_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_CCSLOAD_N_GO_3_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_CCSICDU64_4_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_CCSPRIME8_5_PID),
  (idVendor:FTDI_VID;idProduct:INSIDE_ACCESSO),
  (idVendor:INTREPID_VID;idProduct:INTREPID_VALUECAN_PID),
  (idVendor:INTREPID_VID;idProduct:INTREPID_NEOVI_PID),
  (idVendor:FALCOM_VID;idProduct:FALCOM_TWIST_PID),
  (idVendor:FALCOM_VID;idProduct:FALCOM_SAMBA_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_SUUNTO_SPORTS_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_OCEANIC_PID),
  (idVendor:TTI_VID;idProduct:TTI_QL355P_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_RM_CANVIEW_PID),
  (idVendor:ACTON_VID;idProduct:ACTON_SPECTRAPRO_PID),
  (idVendor:CONTEC_VID;idProduct:CONTEC_COM1USBH_PID),
  (idVendor:MITSUBISHI_VID;idProduct:MITSUBISHI_FXUSB_PID),
  (idVendor:BANDB_VID;idProduct:BANDB_USOTL4_PID),
  (idVendor:BANDB_VID;idProduct:BANDB_USTL4_PID),
  (idVendor:BANDB_VID;idProduct:BANDB_USO9ML2_PID),
  (idVendor:BANDB_VID;idProduct:BANDB_USOPTL4_PID),
  (idVendor:BANDB_VID;idProduct:BANDB_USPTL4_PID),
  (idVendor:BANDB_VID;idProduct:BANDB_USO9ML2DR_2_PID),
  (idVendor:BANDB_VID;idProduct:BANDB_USO9ML2DR_PID),
  (idVendor:BANDB_VID;idProduct:BANDB_USOPTL4DR2_PID),
  (idVendor:BANDB_VID;idProduct:BANDB_USOPTL4DR_PID),
  (idVendor:BANDB_VID;idProduct:BANDB_485USB9F_2W_PID),
  (idVendor:BANDB_VID;idProduct:BANDB_485USB9F_4W_PID),
  (idVendor:BANDB_VID;idProduct:BANDB_232USB9M_PID),
  (idVendor:BANDB_VID;idProduct:BANDB_485USBTB_2W_PID),
  (idVendor:BANDB_VID;idProduct:BANDB_485USBTB_4W_PID),
  (idVendor:BANDB_VID;idProduct:BANDB_TTL5USB9M_PID),
  (idVendor:BANDB_VID;idProduct:BANDB_TTL3USB9M_PID),
  (idVendor:BANDB_VID;idProduct:BANDB_ZZ_PROG1_USB_PID),
  (idVendor:FTDI_VID;idProduct:EVER_ECO_PRO_CDS),
  (idVendor:FTDI_VID;idProduct:FTDI_4N_GALAXY_DE_1_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_4N_GALAXY_DE_2_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_4N_GALAXY_DE_3_PID),
  (idVendor:FTDI_VID;idProduct:XSENS_CONVERTER_0_PID),
  (idVendor:FTDI_VID;idProduct:XSENS_CONVERTER_1_PID),
  (idVendor:FTDI_VID;idProduct:XSENS_CONVERTER_2_PID),
  (idVendor:FTDI_VID;idProduct:XSENS_CONVERTER_3_PID),
  (idVendor:FTDI_VID;idProduct:XSENS_CONVERTER_4_PID),
  (idVendor:FTDI_VID;idProduct:XSENS_CONVERTER_5_PID),
  (idVendor:FTDI_VID;idProduct:XSENS_CONVERTER_6_PID),
  (idVendor:FTDI_VID;idProduct:XSENS_CONVERTER_7_PID),
  (idVendor:XSENS_VID;idProduct:XSENS_AWINDA_DONGLE_PID),
  (idVendor:XSENS_VID;idProduct:XSENS_AWINDA_STATION_PID),
  (idVendor:XSENS_VID;idProduct:XSENS_CONVERTER_PID),
  (idVendor:XSENS_VID;idProduct:XSENS_MTDEVBOARD_PID),
  (idVendor:XSENS_VID;idProduct:XSENS_MTW_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_OMNI1509),
  (idVendor:MOBILITY_VID;idProduct:MOBILITY_USB_SERIAL_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ACTIVE_ROBOTS_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_MHAM_KW_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_MHAM_YS_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_MHAM_Y6_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_MHAM_Y8_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_MHAM_IC_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_MHAM_DB9_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_MHAM_RS232_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_MHAM_Y9_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_TERATRONIK_VCP_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_TERATRONIK_D2XX_PID),
  (idVendor:EVOLUTION_VID;idProduct:EVOLUTION_ER1_PID),
  (idVendor:EVOLUTION_VID;idProduct:EVO_HYBRID_PID),
  (idVendor:EVOLUTION_VID;idProduct:EVO_RCM4_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ARTEMIS_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ATIK_ATK16_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ATIK_ATK16C_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ATIK_ATK16HR_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ATIK_ATK16HRC_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ATIK_ATK16IC_PID),
  (idVendor:KOBIL_VID;idProduct:KOBIL_CONV_B1_PID),
  (idVendor:KOBIL_VID;idProduct:KOBIL_CONV_KAAN_PID),
  (idVendor:POSIFLEX_VID;idProduct:POSIFLEX_PP7000_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_TTUSB_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ECLO_COM_1WIRE_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_WESTREX_MODEL_777_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_WESTREX_MODEL_8900F_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_PCDJ_DAC2_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_RRCIRKITS_LOCOBUFFER_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ASK_RDR400_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_NZR_SEM_USB_PID),
  (idVendor:ICOM_VID;idProduct:ICOM_ID_1_PID),
  (idVendor:ICOM_VID;idProduct:ICOM_OPC_U_UC_PID),
  (idVendor:ICOM_VID;idProduct:ICOM_ID_RP2C1_PID),
  (idVendor:ICOM_VID;idProduct:ICOM_ID_RP2C2_PID),
  (idVendor:ICOM_VID;idProduct:ICOM_ID_RP2D_PID),
  (idVendor:ICOM_VID;idProduct:ICOM_ID_RP2VT_PID),
  (idVendor:ICOM_VID;idProduct:ICOM_ID_RP2VR_PID),
  (idVendor:ICOM_VID;idProduct:ICOM_ID_RP4KVT_PID),
  (idVendor:ICOM_VID;idProduct:ICOM_ID_RP4KVR_PID),
  (idVendor:ICOM_VID;idProduct:ICOM_ID_RP2KVT_PID),
  (idVendor:ICOM_VID;idProduct:ICOM_ID_RP2KVR_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ACG_HFDUAL_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_YEI_SERVOCENTER31_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_THORLABS_PID),
  (idVendor:TESTO_VID;idProduct:TESTO_1_PID),
  (idVendor:TESTO_VID;idProduct:TESTO_3_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_GAMMA_SCOUT_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_TACTRIX_OPENPORT_13M_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_TACTRIX_OPENPORT_13S_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_TACTRIX_OPENPORT_13U_PID),
  (idVendor:ELEKTOR_VID;idProduct:ELEKTOR_FT323R_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_NDI_HUC_PID), {FTDISERIAL_QUIRK_NDI_DEVICE}
  (idVendor:FTDI_VID;idProduct:FTDI_NDI_SPECTRA_SCU_PID), {FTDISERIAL_QUIRK_NDI_DEVICE}
  (idVendor:FTDI_VID;idProduct:FTDI_NDI_FUTURE_2_PID), {FTDISERIAL_QUIRK_NDI_DEVICE}
  (idVendor:FTDI_VID;idProduct:FTDI_NDI_FUTURE_3_PID), {FTDISERIAL_QUIRK_NDI_DEVICE}
  (idVendor:FTDI_VID;idProduct:FTDI_NDI_AURORA_SCU_PID), {FTDISERIAL_QUIRK_NDI_DEVICE}
  (idVendor:TELLDUS_VID;idProduct:TELLDUS_TELLSTICK_PID),
  (idVendor:NOVITUS_VID;idProduct:NOVITUS_BONO_E_PID),
  (idVendor:FTDI_VID;idProduct:RTSYSTEMS_USB_VX8_PID),
  (idVendor:RTSYSTEMS_VID;idProduct:RTSYSTEMS_USB_S03_PID),
  (idVendor:RTSYSTEMS_VID;idProduct:RTSYSTEMS_USB_59_PID),
  (idVendor:RTSYSTEMS_VID;idProduct:RTSYSTEMS_USB_57A_PID),
  (idVendor:RTSYSTEMS_VID;idProduct:RTSYSTEMS_USB_57B_PID),
  (idVendor:RTSYSTEMS_VID;idProduct:RTSYSTEMS_USB_29A_PID),
  (idVendor:RTSYSTEMS_VID;idProduct:RTSYSTEMS_USB_29B_PID),
  (idVendor:RTSYSTEMS_VID;idProduct:RTSYSTEMS_USB_29F_PID),
  (idVendor:RTSYSTEMS_VID;idProduct:RTSYSTEMS_USB_62B_PID),
  (idVendor:RTSYSTEMS_VID;idProduct:RTSYSTEMS_USB_S01_PID),
  (idVendor:RTSYSTEMS_VID;idProduct:RTSYSTEMS_USB_63_PID),
  (idVendor:RTSYSTEMS_VID;idProduct:RTSYSTEMS_USB_29C_PID),
  (idVendor:RTSYSTEMS_VID;idProduct:RTSYSTEMS_USB_81B_PID),
  (idVendor:RTSYSTEMS_VID;idProduct:RTSYSTEMS_USB_82B_PID),
  (idVendor:RTSYSTEMS_VID;idProduct:RTSYSTEMS_USB_K5D_PID),
  (idVendor:RTSYSTEMS_VID;idProduct:RTSYSTEMS_USB_K4Y_PID),
  (idVendor:RTSYSTEMS_VID;idProduct:RTSYSTEMS_USB_K5G_PID),
  (idVendor:RTSYSTEMS_VID;idProduct:RTSYSTEMS_USB_S05_PID),
  (idVendor:RTSYSTEMS_VID;idProduct:RTSYSTEMS_USB_60_PID),
  (idVendor:RTSYSTEMS_VID;idProduct:RTSYSTEMS_USB_61_PID),
  (idVendor:RTSYSTEMS_VID;idProduct:RTSYSTEMS_USB_62_PID),
  (idVendor:RTSYSTEMS_VID;idProduct:RTSYSTEMS_USB_63B_PID),
  (idVendor:RTSYSTEMS_VID;idProduct:RTSYSTEMS_USB_64_PID),
  (idVendor:RTSYSTEMS_VID;idProduct:RTSYSTEMS_USB_65_PID),
  (idVendor:RTSYSTEMS_VID;idProduct:RTSYSTEMS_USB_92_PID),
  (idVendor:RTSYSTEMS_VID;idProduct:RTSYSTEMS_USB_92D_PID),
  (idVendor:RTSYSTEMS_VID;idProduct:RTSYSTEMS_USB_W5R_PID),
  (idVendor:RTSYSTEMS_VID;idProduct:RTSYSTEMS_USB_A5R_PID),
  (idVendor:RTSYSTEMS_VID;idProduct:RTSYSTEMS_USB_PW1_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_MAXSTREAM_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_PHI_FISCO_PID),
  (idVendor:TML_VID;idProduct:TML_USB_SERIAL_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_ELSTER_UNICOM_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_PROPOX_JTAGCABLEII_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_PROPOX_ISPCABLEIII_PID),
  (idVendor:FTDI_VID;idProduct:CYBER_CORTEX_AV_PID), {FTDISERIAL_QUIRK_JTAG}
  //(idVendor:OLIMEX_VID;idProduct:OLIMEX_ARM_USB_OCD_PID), {FTDISERIAL_QUIRK_JTAG} // USB_DEVICE_INTERFACE_NUMBER(OLIMEX_VID, OLIMEX_ARM_USB_OCD_PID, 1)
  //(idVendor:OLIMEX_VID;idProduct:OLIMEX_ARM_USB_OCD_H_PID), {FTDISERIAL_QUIRK_JTAG} //USB_DEVICE_INTERFACE_NUMBER(OLIMEX_VID, OLIMEX_ARM_USB_OCD_H_PID, 1)
  //(idVendor:OLIMEX_VID;idProduct:OLIMEX_ARM_USB_TINY_PID), //USB_DEVICE_INTERFACE_NUMBER(OLIMEX_VID, OLIMEX_ARM_USB_TINY_PID, 1)
  //(idVendor:OLIMEX_VID;idProduct:OLIMEX_ARM_USB_TINY_H_PID), //USB_DEVICE_INTERFACE_NUMBER(OLIMEX_VID, OLIMEX_ARM_USB_TINY_H_PID, 1)
  (idVendor:FIC_VID;idProduct:FIC_NEO1973_DEBUG_PID), {FTDISERIAL_QUIRK_JTAG}
  (idVendor:FTDI_VID;idProduct:FTDI_OOCDLINK_PID), {FTDISERIAL_QUIRK_JTAG}
  (idVendor:FTDI_VID;idProduct:LMI_LM3S_DEVEL_BOARD_PID), {FTDISERIAL_QUIRK_JTAG}
  (idVendor:FTDI_VID;idProduct:LMI_LM3S_EVAL_BOARD_PID), {FTDISERIAL_QUIRK_JTAG}
  (idVendor:FTDI_VID;idProduct:LMI_LM3S_ICDI_BOARD_PID), {FTDISERIAL_QUIRK_JTAG}
  (idVendor:FTDI_VID;idProduct:FTDI_REVED_AXE027_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_TURTELIZER_PID), {FTDISERIAL_QUIRK_JTAG}
  (idVendor:RATOC_VENDOR_ID;idProduct:RATOC_PRODUCT_ID_USB60F),
  (idVendor:RATOC_VENDOR_ID;idProduct:RATOC_PRODUCT_ID_SCU18),
  (idVendor:FTDI_VID;idProduct:FTDI_REU_TINY_PID),
  {Papouch devices based on FTDI chip}
  (idVendor:PAPOUCH_VID;idProduct:PAPOUCH_SB485_PID),
  (idVendor:PAPOUCH_VID;idProduct:PAPOUCH_AP485_PID),
  (idVendor:PAPOUCH_VID;idProduct:PAPOUCH_SB422_PID),
  (idVendor:PAPOUCH_VID;idProduct:PAPOUCH_SB485_2_PID),
  (idVendor:PAPOUCH_VID;idProduct:PAPOUCH_AP485_2_PID),
  (idVendor:PAPOUCH_VID;idProduct:PAPOUCH_SB422_2_PID),
  (idVendor:PAPOUCH_VID;idProduct:PAPOUCH_SB485S_PID),
  (idVendor:PAPOUCH_VID;idProduct:PAPOUCH_SB485C_PID),
  (idVendor:PAPOUCH_VID;idProduct:PAPOUCH_LEC_PID),
  (idVendor:PAPOUCH_VID;idProduct:PAPOUCH_SB232_PID),
  (idVendor:PAPOUCH_VID;idProduct:PAPOUCH_TMU_PID),
  (idVendor:PAPOUCH_VID;idProduct:PAPOUCH_IRAMP_PID),
  (idVendor:PAPOUCH_VID;idProduct:PAPOUCH_DRAK5_PID),
  (idVendor:PAPOUCH_VID;idProduct:PAPOUCH_QUIDO8x8_PID),
  (idVendor:PAPOUCH_VID;idProduct:PAPOUCH_QUIDO4x4_PID),
  (idVendor:PAPOUCH_VID;idProduct:PAPOUCH_QUIDO2x2_PID),
  (idVendor:PAPOUCH_VID;idProduct:PAPOUCH_QUIDO10x1_PID),
  (idVendor:PAPOUCH_VID;idProduct:PAPOUCH_QUIDO30x3_PID),
  (idVendor:PAPOUCH_VID;idProduct:PAPOUCH_QUIDO60x3_PID),
  (idVendor:PAPOUCH_VID;idProduct:PAPOUCH_QUIDO2x16_PID),
  (idVendor:PAPOUCH_VID;idProduct:PAPOUCH_QUIDO3x32_PID),
  (idVendor:PAPOUCH_VID;idProduct:PAPOUCH_DRAK6_PID),
  (idVendor:PAPOUCH_VID;idProduct:PAPOUCH_UPSUSB_PID),
  (idVendor:PAPOUCH_VID;idProduct:PAPOUCH_MU_PID),
  (idVendor:PAPOUCH_VID;idProduct:PAPOUCH_SIMUKEY_PID),
  (idVendor:PAPOUCH_VID;idProduct:PAPOUCH_AD4USB_PID),
  (idVendor:PAPOUCH_VID;idProduct:PAPOUCH_GMUX_PID),
  (idVendor:PAPOUCH_VID;idProduct:PAPOUCH_GMSR_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_DOMINTELL_DGQG_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_DOMINTELL_DUSB_PID),
  (idVendor:ALTI2_VID;idProduct:ALTI2_N3_PID),
  (idVendor:FTDI_VID;idProduct:DIEBOLD_BCS_SE923_PID),
  (idVendor:ATMEL_VID;idProduct:STK541_PID),
  (idVendor:DE_VID;idProduct:STB_PID),
  (idVendor:DE_VID;idProduct:WHT_PID),
  (idVendor:ADI_VID;idProduct:ADI_GNICE_PID), {FTDISERIAL_QUIRK_JTAG}
  (idVendor:ADI_VID;idProduct:ADI_GNICEPLUS_PID), {FTDISERIAL_QUIRK_JTAG}
  //(idVendor:ACTEL_VID;idProduct:MICROSEMI_ARROW_SF2PLUS_BOARD_PID), //USB_DEVICE_INTERFACE_NUMBER(ACTEL_VID, MICROSEMI_ARROW_SF2PLUS_BOARD_PID, 2)
  (idVendor:JETI_VID;idProduct:JETI_SPC1201_PID),
  (idVendor:MARVELL_VID;idProduct:MARVELL_SHEEVAPLUG_PID), {FTDISERIAL_QUIRK_JTAG}
  (idVendor:LARSENBRUSGAARD_VID;idProduct:LB_ALTITRACK_PID),
  (idVendor:GN_OTOMETRICS_VID;idProduct:AURICAL_USB_PID),
  (idVendor:FTDI_VID;idProduct:PI_C865_PID),
  (idVendor:FTDI_VID;idProduct:PI_C857_PID),
  (idVendor:PI_VID;idProduct:PI_C866_PID),
  (idVendor:PI_VID;idProduct:PI_C663_PID),
  (idVendor:PI_VID;idProduct:PI_C725_PID),
  (idVendor:PI_VID;idProduct:PI_E517_PID),
  (idVendor:PI_VID;idProduct:PI_C863_PID),
  (idVendor:PI_VID;idProduct:PI_E861_PID),
  (idVendor:PI_VID;idProduct:PI_C867_PID),
  (idVendor:PI_VID;idProduct:PI_E609_PID),
  (idVendor:PI_VID;idProduct:PI_E709_PID),
  (idVendor:PI_VID;idProduct:PI_100F_PID),
  (idVendor:PI_VID;idProduct:PI_1011_PID),
  (idVendor:PI_VID;idProduct:PI_1012_PID),
  (idVendor:PI_VID;idProduct:PI_1013_PID),
  (idVendor:PI_VID;idProduct:PI_1014_PID),
  (idVendor:PI_VID;idProduct:PI_1015_PID),
  (idVendor:PI_VID;idProduct:PI_1016_PID),
  (idVendor:KONDO_VID;idProduct:KONDO_USB_SERIAL_PID),
  (idVendor:BAYER_VID;idProduct:BAYER_CONTOUR_CABLE_PID),
  (idVendor:FTDI_VID;idProduct:MARVELL_OPENRD_PID), {FTDISERIAL_QUIRK_JTAG}
  (idVendor:FTDI_VID;idProduct:TI_XDS100V2_PID), {FTDISERIAL_QUIRK_JTAG}
  (idVendor:FTDI_VID;idProduct:HAMEG_HO820_PID),
  (idVendor:FTDI_VID;idProduct:HAMEG_HO720_PID),
  (idVendor:FTDI_VID;idProduct:HAMEG_HO730_PID),
  (idVendor:FTDI_VID;idProduct:HAMEG_HO870_PID),
  (idVendor:FTDI_VID;idProduct:MJSG_GENERIC_PID),
  (idVendor:FTDI_VID;idProduct:MJSG_SR_RADIO_PID),
  (idVendor:FTDI_VID;idProduct:MJSG_HD_RADIO_PID),
  (idVendor:FTDI_VID;idProduct:MJSG_XM_RADIO_PID),
  (idVendor:FTDI_VID;idProduct:XVERVE_SIGNALYZER_ST_PID), {FTDISERIAL_QUIRK_JTAG}
  (idVendor:FTDI_VID;idProduct:XVERVE_SIGNALYZER_SLITE_PID), {FTDISERIAL_QUIRK_JTAG}
  (idVendor:FTDI_VID;idProduct:XVERVE_SIGNALYZER_SH2_PID), {FTDISERIAL_QUIRK_JTAG}
  (idVendor:FTDI_VID;idProduct:XVERVE_SIGNALYZER_SH4_PID), {FTDISERIAL_QUIRK_JTAG}
  (idVendor:FTDI_VID;idProduct:SEGWAY_RMP200_PID),
  (idVendor:FTDI_VID;idProduct:ACCESIO_COM4SM_PID),
  (idVendor:IONICS_VID;idProduct:IONICS_PLUGCOMPUTER_PID), {FTDISERIAL_QUIRK_JTAG}
  (idVendor:FTDI_VID;idProduct:FTDI_CHAMSYS_24_MASTER_WING_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_CHAMSYS_PC_WING_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_CHAMSYS_USB_DMX_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_CHAMSYS_MIDI_TIMECODE_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_CHAMSYS_MINI_WING_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_CHAMSYS_MAXI_WING_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_CHAMSYS_MEDIA_WING_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_CHAMSYS_WING_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_SCIENCESCOPE_LOGBOOKML_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_SCIENCESCOPE_LS_LOGBOOK_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_SCIENCESCOPE_HS_LOGBOOK_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_CINTERION_MC55I_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_FHE_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_DOTEC_PID),
  (idVendor:QIHARDWARE_VID;idProduct:MILKYMISTONE_JTAGSERIAL_PID), {FTDISERIAL_QUIRK_JTAG}
  (idVendor:ST_VID;idProduct:ST_STMCLT_2232_PID), {FTDISERIAL_QUIRK_JTAG}
  (idVendor:ST_VID;idProduct:ST_STMCLT_4232_PID), {FTDISERIAL_QUIRK_STMCLITE}
  (idVendor:FTDI_VID;idProduct:FTDI_RF_R106),
  (idVendor:FTDI_VID;idProduct:FTDI_DISTORTEC_JTAG_LOCK_PICK_PID), {FTDISERIAL_QUIRK_JTAG}
  (idVendor:FTDI_VID;idProduct:FTDI_LUMEL_PD12_PID),
  {Crucible Devices}
  (idVendor:FTDI_VID;idProduct:FTDI_CT_COMET_PID),
  (idVendor:FTDI_VID;idProduct:FTDI_Z3X_PID),
  {Cressi Devices}
  (idVendor:FTDI_VID;idProduct:FTDI_CRESSI_PID),
  {Brainboxes Devices}
  (idVendor:BRAINBOXES_VID;idProduct:BRAINBOXES_VX_001_PID),
  (idVendor:BRAINBOXES_VID;idProduct:BRAINBOXES_VX_012_PID),
  (idVendor:BRAINBOXES_VID;idProduct:BRAINBOXES_VX_023_PID),
  (idVendor:BRAINBOXES_VID;idProduct:BRAINBOXES_VX_034_PID),
  (idVendor:BRAINBOXES_VID;idProduct:BRAINBOXES_US_101_PID),
  (idVendor:BRAINBOXES_VID;idProduct:BRAINBOXES_US_160_1_PID),
  (idVendor:BRAINBOXES_VID;idProduct:BRAINBOXES_US_160_2_PID),
  (idVendor:BRAINBOXES_VID;idProduct:BRAINBOXES_US_160_3_PID),
  (idVendor:BRAINBOXES_VID;idProduct:BRAINBOXES_US_160_4_PID),
  (idVendor:BRAINBOXES_VID;idProduct:BRAINBOXES_US_160_5_PID),
  (idVendor:BRAINBOXES_VID;idProduct:BRAINBOXES_US_160_6_PID),
  (idVendor:BRAINBOXES_VID;idProduct:BRAINBOXES_US_160_7_PID),
  (idVendor:BRAINBOXES_VID;idProduct:BRAINBOXES_US_160_8_PID),
  (idVendor:BRAINBOXES_VID;idProduct:BRAINBOXES_US_257_PID),
  (idVendor:BRAINBOXES_VID;idProduct:BRAINBOXES_US_279_1_PID),
  (idVendor:BRAINBOXES_VID;idProduct:BRAINBOXES_US_279_2_PID),
  (idVendor:BRAINBOXES_VID;idProduct:BRAINBOXES_US_279_3_PID),
  (idVendor:BRAINBOXES_VID;idProduct:BRAINBOXES_US_279_4_PID),
  (idVendor:BRAINBOXES_VID;idProduct:BRAINBOXES_US_313_PID),
  (idVendor:BRAINBOXES_VID;idProduct:BRAINBOXES_US_324_PID),
  (idVendor:BRAINBOXES_VID;idProduct:BRAINBOXES_US_346_1_PID),
  (idVendor:BRAINBOXES_VID;idProduct:BRAINBOXES_US_346_2_PID),
  (idVendor:BRAINBOXES_VID;idProduct:BRAINBOXES_US_357_PID),
  (idVendor:BRAINBOXES_VID;idProduct:BRAINBOXES_US_606_1_PID),
  (idVendor:BRAINBOXES_VID;idProduct:BRAINBOXES_US_606_2_PID),
  (idVendor:BRAINBOXES_VID;idProduct:BRAINBOXES_US_606_3_PID),
  (idVendor:BRAINBOXES_VID;idProduct:BRAINBOXES_US_701_1_PID),
  (idVendor:BRAINBOXES_VID;idProduct:BRAINBOXES_US_701_2_PID),
  (idVendor:BRAINBOXES_VID;idProduct:BRAINBOXES_US_842_1_PID),
  (idVendor:BRAINBOXES_VID;idProduct:BRAINBOXES_US_842_2_PID),
  (idVendor:BRAINBOXES_VID;idProduct:BRAINBOXES_US_842_3_PID),
  (idVendor:BRAINBOXES_VID;idProduct:BRAINBOXES_US_842_4_PID),
  {ekey Devices}
  (idVendor:FTDI_VID;idProduct:FTDI_EKEY_CONV_USB_PID),
  //{Infineon Devices}
  //(idVendor:INFINEON_VID;idProduct:INFINEON_TRIBOARD_TC1798_PID), //USB_DEVICE_INTERFACE_NUMBER(INFINEON_VID;idProduct:INFINEON_TRIBOARD_TC1798_PID, 1
  //(idVendor:INFINEON_VID;idProduct:INFINEON_TRIBOARD_TC2X7_PID), //USB_DEVICE_INTERFACE_NUMBER(INFINEON_VID;idProduct:INFINEON_TRIBOARD_TC2X7_PID, 1
  {GE Healthcare devices}
  (idVendor:GE_HEALTHCARE_VID;idProduct:GE_HEALTHCARE_NEMO_TRACKER_PID),
  {Active Research (Actisense) devices}
  (idVendor:FTDI_VID;idProduct:ACTISENSE_NDC_PID),
  (idVendor:FTDI_VID;idProduct:ACTISENSE_USG_PID),
  (idVendor:FTDI_VID;idProduct:ACTISENSE_NGT_PID),
  (idVendor:FTDI_VID;idProduct:ACTISENSE_NGW_PID),
  (idVendor:FTDI_VID;idProduct:ACTISENSE_D9AC_PID),
  (idVendor:FTDI_VID;idProduct:ACTISENSE_D9AD_PID),
  (idVendor:FTDI_VID;idProduct:ACTISENSE_D9AE_PID),
  (idVendor:FTDI_VID;idProduct:ACTISENSE_D9AF_PID),
  (idVendor:FTDI_VID;idProduct:CHETCO_SEAGAUGE_PID),
  (idVendor:FTDI_VID;idProduct:CHETCO_SEASWITCH_PID),
  (idVendor:FTDI_VID;idProduct:CHETCO_SEASMART_NMEA2000_PID),
  (idVendor:FTDI_VID;idProduct:CHETCO_SEASMART_ETHERNET_PID),
  (idVendor:FTDI_VID;idProduct:CHETCO_SEASMART_WIFI_PID),
  (idVendor:FTDI_VID;idProduct:CHETCO_SEASMART_DISPLAY_PID),
  (idVendor:FTDI_VID;idProduct:CHETCO_SEASMART_LITE_PID),
  (idVendor:FTDI_VID;idProduct:CHETCO_SEASMART_ANALOG_PID),
  {ICP DAS I-756xU devices}
  (idVendor:ICPDAS_VID;idProduct:ICPDAS_I7560U_PID),
  (idVendor:ICPDAS_VID;idProduct:ICPDAS_I7561U_PID),
  (idVendor:ICPDAS_VID;idProduct:ICPDAS_I7563U_PID),
  {WICED USB UART}
  (idVendor:WICED_VID;idProduct:WICED_USB20706V2_PID),
  {Texas Instruments}
  (idVendor:TI_VID;idProduct:TI_CC3200_LAUNCHPAD_PID), {FTDISERIAL_QUIRK_JTAG}
  {Cypress WICED USB UART}
  (idVendor:CYPRESS_VID;idProduct:CYPRESS_WICED_BT_USB_PID),
  (idVendor:CYPRESS_VID;idProduct:CYPRESS_WICED_WL_USB_PID),
  {Airbus Defence and Space}
  (idVendor:AIRBUS_DS_VID;idProduct:AIRBUS_DS_P8GR),
  {EZPrototypes devices}
  (idVendor:EZPROTOTYPES_VID;idProduct:HJELMSLUND_USB485_ISO_PID)
  );

 {FTDI Serial Device and Interface ID constants}
 FTDISERIAL_DEVICE_INTERFACE_ID_COUNT = 1; {Number of supported Device and Interface IDs}

 FTDISERIAL_DEVICE_INTERFACE_ID:array[0..FTDISERIAL_DEVICE_INTERFACE_ID_COUNT - 1] of TUSBDeviceAndInterfaceId = (
  (idVendor:MICROCHIP_VID;idProduct:MICROCHIP_USB_BOARD_PID;bInterfaceClass:USB_CLASS_CODE_VENDOR_SPECIFIC;bInterfaceSubClass:USB_SUBCLASS_VENDOR_SPECIFIC;bInterfaceProtocol:0));

 {FTDI Serial Device and Interface No constants}
 FTDISERIAL_DEVICE_INTERFACE_NO_COUNT = 7; {Number of supported Device and Interface Nos}

 FTDISERIAL_DEVICE_INTERFACE_NO:array[0..FTDISERIAL_DEVICE_INTERFACE_NO_COUNT - 1] of TUSBDeviceAndInterfaceNo = (
  {Infineon Devices}
  (idVendor:INFINEON_VID;idProduct:INFINEON_TRIBOARD_TC1798_PID;bInterfaceNumber:1),
  (idVendor:INFINEON_VID;idProduct:INFINEON_TRIBOARD_TC2X7_PID;bInterfaceNumber:1),
  (idVendor:ACTEL_VID;idProduct:MICROSEMI_ARROW_SF2PLUS_BOARD_PID;bInterfaceNumber:2),
  (idVendor:OLIMEX_VID;idProduct:OLIMEX_ARM_USB_OCD_PID;bInterfaceNumber:1),
  (idVendor:OLIMEX_VID;idProduct:OLIMEX_ARM_USB_OCD_H_PID;bInterfaceNumber:1),
  (idVendor:OLIMEX_VID;idProduct:OLIMEX_ARM_USB_TINY_PID;bInterfaceNumber:1),
  (idVendor:OLIMEX_VID;idProduct:OLIMEX_ARM_USB_TINY_H_PID;bInterfaceNumber:1)
  );

{==============================================================================}
type
 {FTDI Serial specific types}
 PFTDISerialDevice = ^TFTDISerialDevice;
 TFTDISerialDevice = record
  {Serial Properties}
  Serial:TSerialDevice;
  {USB Properties}
  Chip:LongWord;                            {The FTDI chip type (eg FTDISERIAL_CHIP_SIO)}
  Quirks:LongWord;                          {Unusual behaviours of specific chip versions}
  Index:LongWord;                           {Interface index for USB control requests}
  Latency:LongWord;                         {Latency timer value for the device}
  ForceBaud:LongWord;                       {The detected device has a fixed baud rate}
  ForceRTSCTS:LongBool;                     {The detected device requires RTS-CTS usage}
  BaudBase:LongWord;                        {The baud rate clock base value for the device}
  FixedDivisor:LongWord;                    {The fixed divisor value for fixed a baud rate device}
  LastStatus:LongWord;                      {The modem status received with the last packet (eg FTDISERIAL_RECEIVE_STATUS0_CTS)}
  ReceiveSize:LongWord;                     {Maximum Receive size for Bulk IN Endpoint}
  TransmitSize:LongWord;                    {Maximum Transmit size for Bulk OUT Endpoint}
  ReceiveActive:LongBool;                   {True if a Receive request is currently in progress}
  TransmitActive:LongBool;                  {True if a Transmit request is currently in progress}
  SerialInterface:PUSBInterface;            {USB interface for the serial device}
  ReceiveRequest:PUSBRequest;               {USB request Bulk IN Endpoint}
  ReceiveEndpoint:PUSBEndpointDescriptor;   {FTDI Serial Bulk IN Endpoint}
  TransmitRequest:PUSBRequest;              {USB request for Bulk OUT Endpoint}
  TransmitEndpoint:PUSBEndpointDescriptor;  {FTDI Serial Bulk OUT Endpoint}
  PendingCount:LongWord;                    {Number of USB requests pending for this device}
  WaiterThread:TThreadId;                   {Thread waiting for pending requests to complete (for device detachment)}
  {Statistics Properties}
  ReceiveComplete:LongWord;
  TransmitComplete:LongWord;
 end;

{==============================================================================}
{var}
 {FTDI Serial specific variables}

{==============================================================================}
{Initialization Functions}
procedure FTDISerialInit;

{==============================================================================}
{FTDI Serial Functions}
function FTDISerialDeviceOpen(Serial:PSerialDevice;BaudRate,DataBits,StopBits,Parity,FlowControl,ReceiveDepth,TransmitDepth:LongWord):LongWord;
function FTDISerialDeviceClose(Serial:PSerialDevice):LongWord;

function FTDISerialDeviceRead(Serial:PSerialDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
function FTDISerialDeviceWrite(Serial:PSerialDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;

{==============================================================================}
{FTDI Serial USB Functions}
function FTDISerialDriverBind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
function FTDISerialDriverUnbind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;

procedure FTDISerialReceiveStart(Request:PUSBRequest);
procedure FTDISerialReceiveWorker(Request:PUSBRequest);
procedure FTDISerialReceiveComplete(Request:PUSBRequest);

procedure FTDISerialTransmitStart(Request:PUSBRequest);
procedure FTDISerialTransmitWorker(Request:PUSBRequest);
procedure FTDISerialTransmitComplete(Request:PUSBRequest);

{==============================================================================}
{FTDI Serial Helper Functions}
function FTDISerialCheckDevice(Device:PUSBDevice):LongWord;
function FTDISerialCheckDeviceAndInterface(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;

function FTDISerialPatchDevice(Device:PUSBDevice;var Quirks:LongWord):LongWord;

function FTDISerialCheckJTAGDevice(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
function FTDISerialCheck8U2232CDevice(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
function FTDISerialCheckSTMCLiteDevice(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;

function FTDISerialDetermineType(Serial:PFTDISerialDevice):LongWord;

function FTDISerialGetLatency(Serial:PFTDISerialDevice):LongWord;
function FTDISerialSetLatency(Serial:PFTDISerialDevice):LongWord;

function FTDISerialReset(Serial:PFTDISerialDevice;Value:Word):LongWord;
function FTDISerialSetData(Serial:PFTDISerialDevice;Value:Word):LongWord;
function FTDISerialSetBaudRate(Serial:PFTDISerialDevice;Divisor:LongWord):LongWord;
function FTDISerialSetFlowControl(Serial:PFTDISerialDevice;Index:Word):LongWord;

function FTDISerialGetModemStatus(Serial:PFTDISerialDevice;var Status:Word):LongWord;

function FTDISerialSetModemControl(Serial:PFTDISerialDevice;Value:Word):LongWord;

function FTDISerialGetDivisor(Serial:PFTDISerialDevice;var BaudRate:LongWord):LongWord;

function FTDISerialBaudToDivisor232AM(Serial:PFTDISerialDevice;BaudRate:LongWord):LongWord;
function FTDISerialBaudToDivisor232BM(Serial:PFTDISerialDevice;BaudRate:LongWord):LongWord;
function FTDISerialBaudToDivisor2232H(Serial:PFTDISerialDevice;BaudRate:LongWord):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {FTDI Serial specific variables}
 FTDISerialInitialized:Boolean;

 FTDISerialDriver:PUSBDriver;  {FTDI Serial Driver interface (Set by FTDISerialInit)}

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure FTDISerialInit;
var
 Status:LongWord;
 WorkInt:LongWord;
begin
 {}
 {Check Initialized}
 if FTDISerialInitialized then Exit;

 {Create FTDI Serial Driver}
 FTDISerialDriver:=USBDriverCreate;
 if FTDISerialDriver <> nil then
  begin
   {Update FTDI Serial Driver}
   {Driver}
   FTDISerialDriver.Driver.DriverName:=FTDISERIAL_DRIVER_NAME;
   {USB}
   FTDISerialDriver.DriverBind:=FTDISerialDriverBind;
   FTDISerialDriver.DriverUnbind:=FTDISerialDriverUnbind;

   {Register FTDI Serial Driver}
   Status:=USBDriverRegister(FTDISerialDriver);
   if Status <> USB_STATUS_SUCCESS then
    begin
     if USB_LOG_ENABLED then USBLogError(nil,'FTDI Serial: Failed to register FTDI Serial driver: ' + USBStatusToString(Status));

     {Destroy Driver}
     USBDriverDestroy(FTDISerialDriver);
    end;
  end
 else
  begin
   if USB_LOG_ENABLED then USBLogError(nil,'FTDI Serial: Failed to create FTDI Serial driver');
  end;

 {Check Environment Variables}
 {FTDISERIAL_MAX_TRANSMIT}
 WorkInt:=StrToIntDef(EnvironmentGet('FTDISERIAL_MAX_TRANSMIT'),FTDISERIAL_MAX_TRANSMIT);
 if WorkInt <> FTDISERIAL_MAX_TRANSMIT then FTDISERIAL_MAX_TRANSMIT:=WorkInt;

 FTDISerialInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{FTDI Serial Functions}
function FTDISerialDeviceOpen(Serial:PSerialDevice;BaudRate,DataBits,StopBits,Parity,FlowControl,ReceiveDepth,TransmitDepth:LongWord):LongWord;
{Implementation of SerialDeviceOpen API for FTDI Serial}
{Note: Not intended to be called directly by applications, use SerialDeviceOpen instead}
var
 Data:Word;
 Flow:Word;
 Modem:Word;
 Status:LongWord;
 Divisor:LongWord;
 Device:PUSBDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Serial}
 if Serial = nil then Exit;

 {$IF DEFINED(FTDISERIAL_DEBUG) or DEFINED(SERIAL_DEBUG)}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'FTDI Serial: Device Open (BaudRate=' + IntToStr(BaudRate) + ' DataBits=' + SerialDataBitsToString(DataBits) + ' StopBits=' + SerialStopBitsToString(StopBits) + ' Parity=' + SerialParityToString(Parity) + ' FlowControl=' + SerialFlowControlToString(FlowControl) + ')');
 {$ENDIF}

 {Get Device}
 Device:=PUSBDevice(Serial.Device.DeviceData);
 if Device = nil then Exit;

 {Check Baud Rate}
 if ((BaudRate < Serial.Properties.MinRate) or (BaudRate > Serial.Properties.MaxRate)) and (BaudRate <> SERIAL_BAUD_RATE_DEFAULT) then Exit;

 {Check Data Bits}
 if (DataBits < FTDISERIAL_MIN_DATABITS) or (DataBits > FTDISERIAL_MAX_DATABITS) then Exit;

 {Check Stop Bits}
 if (StopBits < FTDISERIAL_MIN_STOPBITS) or (StopBits > FTDISERIAL_MAX_STOPBITS) then Exit;

 {Check Parity}
 if Parity > FTDISERIAL_MAX_PARITY then Exit;

 {Check Flow Control}
 if FlowControl > FTDISERIAL_MAX_FLOW then Exit;

 {Adjust Baud Rate}
 if BaudRate = SERIAL_BAUD_RATE_DEFAULT then
  begin
   BaudRate:=SERIAL_BAUD_RATE_STANDARD;
   if (BaudRate > Serial.Properties.MaxRate) then BaudRate:=SERIAL_BAUD_RATE_FALLBACK;
  end;

 {Reset Device}
 if FTDISerialReset(PFTDISerialDevice(Serial),FTDISERIAL_RESET_SIO) <> ERROR_SUCCESS then
  begin
   if SERIAL_LOG_ENABLED then SerialLogError(Serial,'FTDI Serial: Reset device failed');

   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Setup Data}
 Data:=0;
 {Data Bits}
 case DataBits of
  SERIAL_DATA_8BIT:Data:=Data or 8;
  SERIAL_DATA_7BIT:Data:=Data or 7;
 end;
 {Stop Bits}
 case StopBits of
  SERIAL_STOP_1BIT:Data:=Data or FTDISERIAL_SET_DATA_STOP_BITS_1;
  SERIAL_STOP_1BIT5:Data:=Data or FTDISERIAL_SET_DATA_STOP_BITS_15;
  SERIAL_STOP_2BIT:Data:=Data or FTDISERIAL_SET_DATA_STOP_BITS_2;
 end;
 {Parity}
 case Parity of
  SERIAL_PARITY_NONE:Data:=Data or FTDISERIAL_SET_DATA_PARITY_NONE;
  SERIAL_PARITY_ODD:Data:=Data or FTDISERIAL_SET_DATA_PARITY_ODD;
  SERIAL_PARITY_EVEN:Data:=Data or FTDISERIAL_SET_DATA_PARITY_EVEN;
  SERIAL_PARITY_MARK:Data:=Data or FTDISERIAL_SET_DATA_PARITY_MARK;
  SERIAL_PARITY_SPACE:Data:=Data or FTDISERIAL_SET_DATA_PARITY_SPACE;
 end;

 {Set Data}
 if FTDISerialSetData(PFTDISerialDevice(Serial),Data) <> ERROR_SUCCESS then
  begin
   if SERIAL_LOG_ENABLED then SerialLogError(Serial,'FTDI Serial: Set data failed');

   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Get Divisor}
 Divisor:=FTDISerialGetDivisor(PFTDISerialDevice(Serial),BaudRate);
 if Divisor = LongWord(-1) then
  begin
   if SERIAL_LOG_ENABLED then SerialLogError(Serial,'FTDI Serial: Get divisor failed');

   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
 {Check Force Baud}
 if PFTDISerialDevice(Serial).ForceBaud <> 0 then
  begin
   Divisor:=PFTDISerialDevice(Serial).FixedDivisor;
   BaudRate:=PFTDISerialDevice(Serial).ForceBaud;
  end;

 {$IF DEFINED(FTDISERIAL_DEBUG) or DEFINED(SERIAL_DEBUG)}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'FTDI Serial:  BaudRate=' + IntToStr(BaudRate) + ' Divisor=' + IntToStr(Divisor));
 {$ENDIF}

 {Set Baud Rate}
 if FTDISerialSetBaudRate(PFTDISerialDevice(Serial),Divisor) <> ERROR_SUCCESS then
  begin
   if SERIAL_LOG_ENABLED then SerialLogError(Serial,'FTDI Serial: Set baud rate failed');

   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Setup Flow Control}
 Flow:=FTDISERIAL_SET_FLOW_CTRL_NONE;
 Modem:=FTDISERIAL_SET_MODEM_CTRL_NONE;
 {Check Force RTS-CTS}
 if PFTDISerialDevice(Serial).ForceRTSCTS then
  begin
   FlowControl:=SERIAL_FLOW_RTS_CTS;
  end;
 {Check Flow Control}
 case FlowControl of
  SERIAL_FLOW_RTS_CTS:begin
    Flow:=FTDISERIAL_SET_FLOW_CTRL_RTS_CTS;
    Modem:=FTDISERIAL_SET_MODEM_CTRL_RTS_HIGH;
   end;
  SERIAL_FLOW_DSR_DTR:begin
    Flow:=FTDISERIAL_SET_FLOW_CTRL_DTR_DSR;
    Modem:=FTDISERIAL_SET_MODEM_CTRL_DTR_HIGH;
   end;
 end;

 {Clear Hardware Flow Control}
 if FTDISerialSetFlowControl(PFTDISerialDevice(Serial),FTDISERIAL_SET_FLOW_CTRL_NONE) <> ERROR_SUCCESS then
  begin
   if SERIAL_LOG_ENABLED then SerialLogError(Serial,'FTDI Serial: Clear flow control failed');

   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Check Modem Control}
 if Modem <> FTDISERIAL_SET_MODEM_CTRL_NONE then
  begin
   {Set Modem Control (RTS/DTR)}
   if FTDISerialSetModemControl(PFTDISerialDevice(Serial),Modem) <> ERROR_SUCCESS then
    begin
     if SERIAL_LOG_ENABLED then SerialLogError(Serial,'FTDI Serial: Set modem control (RTS/DTR) failed');

     Result:=ERROR_OPERATION_FAILED;
     Exit;
    end;
  end;

 {Check Flow Control}
 if Flow <> FTDISERIAL_SET_FLOW_CTRL_NONE then
  begin
   {Set Hardware Flow Control}
   if FTDISerialSetFlowControl(PFTDISerialDevice(Serial),Flow) <> ERROR_SUCCESS then
    begin
     if SERIAL_LOG_ENABLED then SerialLogError(Serial,'FTDI Serial: Set flow control failed');

     Result:=ERROR_OPERATION_FAILED;
     Exit;
    end;
  end;

 {Check Max Transmit}
 if FTDISERIAL_MAX_TRANSMIT = 0 then FTDISERIAL_MAX_TRANSMIT:=FTDISERIAL_BULK_OUT_SIZE;

 {Check Receive Depth}
 if ReceiveDepth = 0 then ReceiveDepth:=SERIAL_RECEIVE_DEPTH_DEFAULT;

 {Check Transmit Depth}
 if TransmitDepth = 0 then TransmitDepth:=SERIAL_TRANSMIT_DEPTH_DEFAULT;

 {Allocate Recieve}
 EventReset(Serial.Receive.Wait);
 Serial.Receive.Start:=0;
 Serial.Receive.Count:=0;
 Serial.Receive.Size:=ReceiveDepth;
 Serial.Receive.Data:=GetMem(ReceiveDepth);
 if Serial.Receive.Data = nil then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Allocate Transmit}
 EventSet(Serial.Transmit.Wait);
 Serial.Transmit.Start:=0;
 Serial.Transmit.Count:=0;
 Serial.Transmit.Size:=TransmitDepth;
 Serial.Transmit.Data:=GetMem(TransmitDepth);
 if Serial.Transmit.Data = nil then
  begin
   {Release Receive}
   FreeMem(Serial.Receive.Data);

   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Allocate Receive Request}
 PFTDISerialDevice(Serial).ReceiveRequest:=USBRequestAllocate(Device,PFTDISerialDevice(Serial).ReceiveEndpoint,FTDISerialReceiveComplete,FTDISERIAL_BULK_IN_SIZE,Serial);
 if PFTDISerialDevice(Serial).ReceiveRequest = nil then
  begin
   {Release Receive}
   FreeMem(Serial.Receive.Data);

   {Release Transmit}
   FreeMem(Serial.Transmit.Data);

   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Allocate Transmit Request}
 PFTDISerialDevice(Serial).TransmitRequest:=USBRequestAllocate(Device,PFTDISerialDevice(Serial).TransmitEndpoint,FTDISerialTransmitComplete,FTDISERIAL_BULK_OUT_SIZE,Serial);
 if PFTDISerialDevice(Serial).TransmitRequest = nil then
  begin
   {Release Receive}
   FreeMem(Serial.Receive.Data);

   {Release Transmit}
   FreeMem(Serial.Transmit.Data);

   {Release Receive Request}
   USBRequestRelease(PFTDISerialDevice(Serial).ReceiveRequest);

   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Update Pending}
 Inc(PFTDISerialDevice(Serial).PendingCount);

 {Set Active}
 PFTDISerialDevice(Serial).ReceiveActive:=True;

 {$IF DEFINED(FTDISERIAL_DEBUG) or DEFINED(SERIAL_DEBUG)}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'FTDI Serial: Submitting receive request');
 {$ENDIF}

 {Submit Receive Request}
 Status:=USBRequestSubmit(PFTDISerialDevice(Serial).ReceiveRequest);
 if Status <> USB_STATUS_SUCCESS then
  begin
   if SERIAL_LOG_ENABLED then SerialLogError(Serial,'FTDI Serial: Failed to submit receive request: ' + USBStatusToString(Status));

   {Reset Active}
   PFTDISerialDevice(Serial).ReceiveActive:=False;

   {Update Pending}
   Dec(PFTDISerialDevice(Serial).PendingCount);

   {Release Receive}
   FreeMem(Serial.Receive.Data);

   {Release Transmit}
   FreeMem(Serial.Transmit.Data);

   {Release Receive Request}
   USBRequestRelease(PFTDISerialDevice(Serial).ReceiveRequest);

   {Release Transmit Request}
   USBRequestRelease(PFTDISerialDevice(Serial).TransmitRequest);

   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Update Properties}
 Serial.Properties.BaudRate:=BaudRate;
 Serial.Properties.DataBits:=DataBits;
 Serial.Properties.StopBits:=StopBits;
 Serial.Properties.Parity:=Parity;
 Serial.Properties.FlowControl:=FlowControl;
 Serial.Properties.ReceiveDepth:=ReceiveDepth;
 Serial.Properties.TransmitDepth:=TransmitDepth;

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function FTDISerialDeviceClose(Serial:PSerialDevice):LongWord;
{Implementation of SerialDeviceClose API for FTDI Serial}
{Note: Not intended to be called directly by applications, use SerialDeviceClose instead}
var
 Message:TMessage;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Serial}
 if Serial = nil then Exit;

 {$IF DEFINED(FTDISERIAL_DEBUG) or DEFINED(SERIAL_DEBUG)}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'FTDI Serial: Device Close');
 {$ENDIF}

 {Cancel Receive Request}
 USBRequestCancel(PFTDISerialDevice(Serial).ReceiveRequest);

 {Check Pending}
 if PFTDISerialDevice(Serial).PendingCount <> 0 then
  begin
   {$IF DEFINED(FTDISERIAL_DEBUG) or DEFINED(SERIAL_DEBUG)}
   if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'FTDI Serial: Waiting for ' + IntToStr(PFTDISerialDevice(Serial).PendingCount) + ' pending requests to complete');
   {$ENDIF}

   {Wait for Pending}

   {Setup Waiter}
   PFTDISerialDevice(Serial).WaiterThread:=GetCurrentThreadId;

   {Release the Lock}
   MutexUnlock(Serial.Lock);

   {Wait for Message}
   ThreadReceiveMessage(Message);

   {Acquire the Lock}
   if MutexLock(Serial.Lock) <> ERROR_SUCCESS then Exit;
  end;

 {Release Transmit Request}
 USBRequestRelease(PFTDISerialDevice(Serial).TransmitRequest);

 {Release Receive Request}
 USBRequestRelease(PFTDISerialDevice(Serial).ReceiveRequest);

 {Release Receive}
 Serial.Receive.Start:=0;
 Serial.Receive.Count:=0;
 Serial.Receive.Size:=0;
 FreeMem(Serial.Receive.Data);

 {Release Transmit}
 Serial.Transmit.Start:=0;
 Serial.Transmit.Count:=0;
 Serial.Transmit.Size:=0;
 FreeMem(Serial.Transmit.Data);

 {Update Properties}
 Serial.Properties.BaudRate:=SERIAL_BAUD_RATE_DEFAULT;
 Serial.Properties.DataBits:=SERIAL_DATA_8BIT;
 Serial.Properties.StopBits:=SERIAL_STOP_1BIT;
 Serial.Properties.Parity:=SERIAL_PARITY_NONE;
 Serial.Properties.FlowControl:=SERIAL_FLOW_NONE;
 Serial.Properties.ReceiveDepth:=SERIAL_RECEIVE_DEPTH_DEFAULT;
 Serial.Properties.TransmitDepth:=SERIAL_TRANSMIT_DEPTH_DEFAULT;

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function FTDISerialDeviceRead(Serial:PSerialDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
{Implementation of SerialDeviceRead API for FTDI Serial}
{Note: Not intended to be called directly by applications, use SerialDeviceRead instead}
var
 Data:Pointer;
 Total:LongWord;
 Offset:PtrUint;
 Removed:LongWord;
 Available:LongWord;
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;

 {Check Buffer}
 if Buffer = nil then Exit;

 {Check Serial}
 if Serial = nil then Exit;

 {$IF DEFINED(FTDISERIAL_DEBUG) or DEFINED(SERIAL_DEBUG)}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'FTDI Serial: Device Read (Size=' + IntToStr(Size) + ')');
 {$ENDIF}

 {Read to Buffer}
 Offset:=0;
 Total:=Size;
 while Size > 0 do
  begin
   {Check State}
   if not(PFTDISerialDevice(Serial).ReceiveActive) and ((Serial.Receive.Size - Serial.Receive.Count) >= PFTDISerialDevice(Serial).ReceiveSize) then
    begin
     {Start Receive}
     FTDISerialReceiveStart(PFTDISerialDevice(Serial).ReceiveRequest);
    end;

   {Check Non Blocking}
   if ((Flags and SERIAL_READ_NON_BLOCK) <> 0) and (Serial.Receive.Count = 0) then
    begin
     Result:=ERROR_NO_MORE_ITEMS;
     Break;
    end;

   {Check Peek Buffer}
   if (Flags and SERIAL_READ_PEEK_BUFFER) <> 0 then
    begin
     Count:=Serial.Receive.Count;
     Result:=ERROR_SUCCESS;
     Break;
    end;

   {Release the Lock}
   MutexUnlock(Serial.Lock);

   {Wait for Data}
   if EventWait(Serial.Receive.Wait) = ERROR_SUCCESS then
    begin
     {Acquire the Lock}
     if MutexLock(Serial.Lock) = ERROR_SUCCESS then
      begin
       {Start Read}
       Data:=SerialBufferReadStart(@Serial.Receive,Available);
       while (Data <> nil) and (Available > 0) and (Size > 0) do
        begin
         {Get Removed}
         Removed:=Min(Size,Available);

         {Copy Data}
         System.Move(Data^,Pointer(Buffer + Offset)^,Removed);

         {Update Statistics}
         Inc(Serial.ReceiveCount,Removed);

         {Update Count}
         Inc(Count,Removed);

         {Update Size and Offset}
         Dec(Size,Removed);
         Inc(Offset,Removed);

         {Complete Read}
         SerialBufferReadComplete(@Serial.Receive,Removed);

         {Start Read}
         Data:=SerialBufferReadStart(@Serial.Receive,Available);
        end;

       {Check Available}
       if Available = 0 then
        begin
         {Reset Event}
         EventReset(Serial.Receive.Wait);
        end;

       {Check State}
       if (Size = 0) and not(PFTDISerialDevice(Serial).ReceiveActive) and ((Serial.Receive.Size - Serial.Receive.Count) >= PFTDISerialDevice(Serial).ReceiveSize) then
        begin
         {Start Receive}
         FTDISerialReceiveStart(PFTDISerialDevice(Serial).ReceiveRequest);
        end;
      end
     else
      begin
       Result:=ERROR_CAN_NOT_COMPLETE;
       Exit;
      end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
     Exit;
    end;
  end;

 {$IF DEFINED(FTDISERIAL_DEBUG) or DEFINED(SERIAL_DEBUG)}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'FTDI Serial:  Return Count=' + IntToStr(Count));
 {$ENDIF}

 {Return Result}
 if (Total = Count) then Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function FTDISerialDeviceWrite(Serial:PSerialDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
{Implementation of SerialDeviceWrite API for FTDI Serial}
{Note: Not intended to be called directly by applications, use SerialDeviceWrite instead}
var
 Data:Pointer;
 Empty:Boolean;
 Total:LongWord;
 Offset:PtrUint;
 Added:LongWord;
 Available:LongWord;
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;

 {Check Buffer}
 if Buffer = nil then Exit;

 {Check Serial}
 if Serial = nil then Exit;

 {$IF DEFINED(FTDISERIAL_DEBUG) or DEFINED(SERIAL_DEBUG)}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'FTDI Serial: Device Write (Size=' + IntToStr(Size) + ')');
 {$ENDIF}

 {Write from Buffer}
 Offset:=0;
 Total:=Size;
 while Size > 0 do
  begin
   {Check Non Blocking}
   if ((Flags and SERIAL_WRITE_NON_BLOCK) <> 0) and ((Serial.Transmit.Size - Serial.Transmit.Count) = 0) then
    begin
     Result:=ERROR_INSUFFICIENT_BUFFER;
     Break;
    end;

   {Check Peek Buffer}
   if (Flags and SERIAL_WRITE_PEEK_BUFFER) <> 0 then
    begin
     Count:=Serial.Transmit.Size - Serial.Transmit.Count;
     Result:=ERROR_SUCCESS;
     Break;
    end;

   {Release the Lock}
   MutexUnlock(Serial.Lock);

   {Wait for Space}
   if EventWait(Serial.Transmit.Wait) = ERROR_SUCCESS then
    begin
     {Acquire the Lock}
     if MutexLock(Serial.Lock) = ERROR_SUCCESS then
      begin
       {Check Empty}
       Empty:=(Serial.Transmit.Count = 0);

       {Start Write}
       Data:=SerialBufferWriteStart(@Serial.Transmit,Available);
       while (Data <> nil) and (Available > 0) and (Size > 0) do
        begin
         {Get Added}
         Added:=Min(Size,Available);

         {Copy Data}
         System.Move(Pointer(Buffer + Offset)^,Data^,Added);

         {Update Statistics}
         Inc(Serial.TransmitCount,Added);

         {Update Count}
         Inc(Count,Added);

         {Update Size and Offset}
         Dec(Size,Added);
         Inc(Offset,Added);

         {Complete Write}
         SerialBufferWriteComplete(@Serial.Transmit,Added);

         {Start Write}
         Data:=SerialBufferWriteStart(@Serial.Transmit,Available);
        end;

       {Check Available}
       if Available = 0 then
        begin
         {Reset Event}
         EventReset(Serial.Transmit.Wait);
        end;

       {Check Empty}
       if Empty and not(PFTDISerialDevice(Serial).TransmitActive) then
        begin
         {Start Transmit}
         FTDISerialTransmitStart(PFTDISerialDevice(Serial).TransmitRequest);
        end;
      end
     else
      begin
       Result:=ERROR_CAN_NOT_COMPLETE;
       Exit;
      end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
     Exit;
    end;
  end;

 {$IF DEFINED(FTDISERIAL_DEBUG) or DEFINED(SERIAL_DEBUG)}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'FTDI Serial:  Return Count=' + IntToStr(Count));
 {$ENDIF}

 {Return Result}
 if (Total = Count) then Result:=ERROR_SUCCESS;
end;

{==============================================================================}
{==============================================================================}
{FTDI Serial USB Functions}
function FTDISerialDriverBind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
{Bind the FTDI Serial driver to a USB device if it is suitable}
{Device: The USB device to attempt to bind to}
{Interrface: The USB interface to attempt to bind to (or nil for whole device)}
{Return: USB_STATUS_SUCCESS if completed, USB_STATUS_DEVICE_UNSUPPORTED if unsupported or another error code on failure}
var
 Status:LongWord;
 Quirks:LongWord;
 Serial:PFTDISerialDevice;
 SerialInterface:PUSBInterface;
 ReceiveEndpoint:PUSBEndpointDescriptor;
 TransmitEndpoint:PUSBEndpointDescriptor;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {$IFDEF FTDISERIAL_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'FTDI Serial: Attempting to bind USB device (Manufacturer=' + Device.Manufacturer + ' Product=' + Device.Product + ' Address=' + IntToStr(Device.Address) + ')');
 {$ENDIF}

 {Check Interface (Bind to either device or interface depending on match)}
 if Interrface = nil then
  begin
   {Check FTDI Serial Device}
   if FTDISerialCheckDevice(Device) <> USB_STATUS_SUCCESS then
    begin
     {$IFDEF FTDISERIAL_DEBUG}
      if USB_LOG_ENABLED then USBLogDebug(Device,'FTDI Serial: Device not found in supported device list');
     {$ENDIF}
     {Return Result}
     Result:=USB_STATUS_DEVICE_UNSUPPORTED;
     Exit;
    end;
  end
 else
  begin
   {Check FTDI Serial Device and Interface}
   if FTDISerialCheckDeviceAndInterface(Device,Interrface) <> USB_STATUS_SUCCESS then
    begin
     {$IFDEF FTDISERIAL_DEBUG}
      if USB_LOG_ENABLED then USBLogDebug(Device,'FTDI Serial: Device not found in supported interface list');
     {$ENDIF}
     {Return Result}
     Result:=USB_STATUS_DEVICE_UNSUPPORTED;
     Exit;
    end;
  end;

 {Check Interface}
 if Interrface = nil then
  begin
   {Get Interface (Index 0)}
   SerialInterface:=USBDeviceFindInterfaceByIndex(Device,0);
   if SerialInterface = nil then
    begin
     {$IFDEF FTDISERIAL_DEBUG}
      if USB_LOG_ENABLED then USBLogDebug(Device,'FTDI Serial: Device has no available interface');
     {$ENDIF}
     {Return Result}
     Result:=USB_STATUS_DEVICE_UNSUPPORTED;
     Exit;
    end;
   end
  else
   begin
    {Get Interface (Supplied)}
    SerialInterface:=Interrface;
   end;

 {Patch FTDI Serial Device}
 if FTDISerialPatchDevice(Device,Quirks) <> USB_STATUS_SUCCESS then
  begin
   {$IFDEF FTDISERIAL_DEBUG}
    if USB_LOG_ENABLED then USBLogDebug(Device,'FTDI Serial: Unable to determine device information');
   {$ENDIF}
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Check Quirks}
 if (Quirks and FTDISERIAL_QUIRK_8U2232C) <> 0 then
  begin
   {Check 8U2232C}
   if FTDISerialCheck8U2232CDevice(Device,Interrface) <> USB_STATUS_SUCCESS then
    begin
     {$IFDEF FTDISERIAL_DEBUG}
      if USB_LOG_ENABLED then USBLogDebug(Device,'FTDI Serial: Ignoring 8U2232C JTAG interface');
     {$ENDIF}
     {Return Result}
     Result:=USB_STATUS_DEVICE_UNSUPPORTED;
     Exit;
    end;

   {Get Interface (Index 1)}
   SerialInterface:=USBDeviceFindInterfaceByIndex(Device,1);
   if SerialInterface = nil then
    begin
     {$IFDEF FTDISERIAL_DEBUG}
      if USB_LOG_ENABLED then USBLogDebug(Device,'FTDI Serial: Device has no available interface');
     {$ENDIF}
     {Return Result}
     Result:=USB_STATUS_DEVICE_UNSUPPORTED;
     Exit;
    end;
  end;
 if (Quirks and FTDISERIAL_QUIRK_JTAG) <> 0 then
  begin
   {Check JTAG}
   if FTDISerialCheckJTAGDevice(Device,Interrface) <> USB_STATUS_SUCCESS then
    begin
     {$IFDEF FTDISERIAL_DEBUG}
      if USB_LOG_ENABLED then USBLogDebug(Device,'FTDI Serial: Ignoring JTAG interface');
     {$ENDIF}
     {Return Result}
     Result:=USB_STATUS_DEVICE_UNSUPPORTED;
     Exit;
    end;

   {Get Interface (Index 1)}
   SerialInterface:=USBDeviceFindInterfaceByIndex(Device,1);
   if SerialInterface = nil then
    begin
     {$IFDEF FTDISERIAL_DEBUG}
      if USB_LOG_ENABLED then USBLogDebug(Device,'FTDI Serial: Device has no available interface');
     {$ENDIF}
     {Return Result}
     Result:=USB_STATUS_DEVICE_UNSUPPORTED;
     Exit;
    end;
  end;
 if (Quirks and FTDISERIAL_QUIRK_STMCLITE) <> 0 then
  begin
   {Check STMCLite}
   if FTDISerialCheckSTMCLiteDevice(Device,Interrface) <> USB_STATUS_SUCCESS then
    begin
     {$IFDEF FTDISERIAL_DEBUG}
      if USB_LOG_ENABLED then USBLogDebug(Device,'FTDI Serial: Ignoring STMCLite JTAG interface');
     {$ENDIF}
     {Return Result}
     Result:=USB_STATUS_DEVICE_UNSUPPORTED;
     Exit;
    end;

   {Get Interface (Index 2)}
   SerialInterface:=USBDeviceFindInterfaceByIndex(Device,2);
   if SerialInterface = nil then
    begin
     {$IFDEF FTDISERIAL_DEBUG}
      if USB_LOG_ENABLED then USBLogDebug(Device,'FTDI Serial: Device has no available interface');
     {$ENDIF}
     {Return Result}
     Result:=USB_STATUS_DEVICE_UNSUPPORTED;
     Exit;
    end;
  end;
 {$IFDEF FTDISERIAL_DEBUG}
  if USB_LOG_ENABLED then USBLogDebug(Device,'FTDI Serial: Interface.bInterfaceNumber=' + IntToStr(SerialInterface.Descriptor.bInterfaceNumber));
 {$ENDIF}

 {Check Bulk IN Endpoint}
 ReceiveEndpoint:=USBDeviceFindEndpointByType(Device,SerialInterface,USB_DIRECTION_IN,USB_TRANSFER_TYPE_BULK);
 if ReceiveEndpoint = nil then
  begin
   {$IFDEF FTDISERIAL_DEBUG}
    if USB_LOG_ENABLED then USBLogDebug(Device,'FTDI Serial: Device has no BULK IN endpoint');
   {$ENDIF}
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 {$IFDEF FTDISERIAL_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'FTDI Serial: BULK IN Endpoint Count=' + IntToStr(USBDeviceCountEndpointsByType(Device,SerialInterface,USB_DIRECTION_IN,USB_TRANSFER_TYPE_BULK)));
 {$ENDIF}

 {Check Bulk OUT Endpoint}
 TransmitEndpoint:=USBDeviceFindEndpointByType(Device,SerialInterface,USB_DIRECTION_OUT,USB_TRANSFER_TYPE_BULK);
 if TransmitEndpoint = nil then
  begin
   {$IFDEF FTDISERIAL_DEBUG}
    if USB_LOG_ENABLED then USBLogDebug(Device,'FTDI Serial: Device has no BULK OUT endpoint');
   {$ENDIF}
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 {$IFDEF FTDISERIAL_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'FTDI Serial: BULK OUT Endpoint Count=' + IntToStr(USBDeviceCountEndpointsByType(Device,SerialInterface,USB_DIRECTION_OUT,USB_TRANSFER_TYPE_BULK)));
 {$ENDIF}

 {Check Configuration}
 if (Interrface = nil) and (Device.ConfigurationValue = 0) then
  begin
   {$IFDEF FTDISERIAL_DEBUG}
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

 {USB device reset not required because the USB core already did a reset on the port during attach}

 {Create Serial}
 Serial:=PFTDISerialDevice(SerialDeviceCreateEx(SizeOf(TFTDISerialDevice)));
 if Serial = nil then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'FTDI Serial: Failed to create new serial device');

   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Update Serial}
 {Device}
 Serial.Serial.Device.DeviceBus:=DEVICE_BUS_USB;
 Serial.Serial.Device.DeviceType:=SERIAL_TYPE_USB;
 Serial.Serial.Device.DeviceFlags:=SERIAL_FLAG_DATA_8BIT or SERIAL_FLAG_DATA_7BIT or SERIAL_FLAG_STOP_1BIT or SERIAL_FLAG_STOP_2BIT or SERIAL_FLAG_STOP_1BIT5 or SERIAL_FLAG_PARITY_ODD or SERIAL_FLAG_PARITY_EVEN or SERIAL_FLAG_PARITY_MARK or SERIAL_FLAG_PARITY_SPACE or SERIAL_FLAG_FLOW_RTS_CTS or SERIAL_FLAG_FLOW_DSR_DTR;
 Serial.Serial.Device.DeviceData:=Device;
 Serial.Serial.Device.DeviceDescription:=FTDISERIAL_SERIAL_DESCRIPTION;
 {Serial}
 Serial.Serial.SerialState:=SERIAL_STATE_CLOSED;
 Serial.Serial.SerialStatus:=SERIAL_STATUS_NONE;
 Serial.Serial.DeviceOpen:=FTDISerialDeviceOpen;
 Serial.Serial.DeviceClose:=FTDISerialDeviceClose;
 Serial.Serial.DeviceRead:=FTDISerialDeviceRead;
 Serial.Serial.DeviceWrite:=FTDISerialDeviceWrite;
 {Driver}
 Serial.Serial.Properties.Flags:=Serial.Serial.Device.DeviceFlags;
 Serial.Serial.Properties.MinRate:=0;
 Serial.Serial.Properties.MaxRate:=0;
 Serial.Serial.Properties.BaudRate:=SERIAL_BAUD_RATE_DEFAULT;
 Serial.Serial.Properties.DataBits:=SERIAL_DATA_8BIT;
 Serial.Serial.Properties.StopBits:=SERIAL_STOP_1BIT;
 Serial.Serial.Properties.Parity:=SERIAL_PARITY_NONE;
 Serial.Serial.Properties.FlowControl:=SERIAL_FLOW_NONE;
 Serial.Serial.Properties.ReceiveDepth:=SERIAL_RECEIVE_DEPTH_DEFAULT;
 Serial.Serial.Properties.TransmitDepth:=SERIAL_TRANSMIT_DEPTH_DEFAULT;
 {USB}
 Serial.Quirks:=Quirks;
 Serial.ReceiveSize:=ReceiveEndpoint.wMaxPacketSize;
 Serial.TransmitSize:=TransmitEndpoint.wMaxPacketSize;
 Serial.SerialInterface:=SerialInterface;
 Serial.ReceiveEndpoint:=ReceiveEndpoint;
 Serial.TransmitEndpoint:=TransmitEndpoint;
 Serial.WaiterThread:=INVALID_HANDLE_VALUE;

 {Determine Type}
 FTDISerialDetermineType(Serial);

 {Check Type}
 case Serial.Chip of
  FTDISERIAL_CHIP_SIO:begin
    {SIO}
    Serial.Serial.Properties.MinRate:=300;
    Serial.Serial.Properties.MaxRate:=115200;
   end;
  FTDISERIAL_CHIP_FT8U232AM:begin
    {FT8U232AM}
    Serial.Serial.Properties.MinRate:=300;
    Serial.Serial.Properties.MaxRate:=3000000;
   end;
  FTDISERIAL_CHIP_FT232BM,FTDISERIAL_CHIP_FT2232C,FTDISERIAL_CHIP_FT232RL,FTDISERIAL_CHIP_FTX:begin
    {FT232BM / FT2232C / FT232RL / FT-X series}
    Serial.Serial.Properties.MinRate:=300;
    Serial.Serial.Properties.MaxRate:=3000000;
   end;
  FTDISERIAL_CHIP_FT2232H,FTDISERIAL_CHIP_FT4232H,FTDISERIAL_CHIP_FT232H:begin
    {FT2232H / FT4232H / FT232H}
    Serial.Serial.Properties.MinRate:=300;
    Serial.Serial.Properties.MaxRate:=12000000;
   end;
 end;

 {Check Quirks}
 if (Quirks and FTDISERIAL_QUIRK_HE_TIRA1) <> 0 then
  begin
   {Force Baud to 100000}
   Serial.ForceBaud:=100000;
   Serial.ForceRTSCTS:=True;
   Serial.FixedDivisor:=240;
  end;
 if (Quirks and FTDISERIAL_QUIRK_USB_UIRT) <> 0 then
  begin
   {Force Baud to 312500}
   Serial.ForceBaud:=312500;
   Serial.FixedDivisor:=77;
  end;
 if (Quirks and FTDISERIAL_QUIRK_NDI_DEVICE) <> 0 then
  begin
   {Setup Latency}
   Serial.Latency:=1;
   FTDISerialSetLatency(Serial);
  end;

 {Setup Latency Timer}
 if FTDISerialGetLatency(Serial) <> ERROR_SUCCESS then
  begin
   Serial.Latency:=16;
  end;
 FTDISerialSetLatency(Serial);

 {Register Serial}
 if SerialDeviceRegister(@Serial.Serial) <> ERROR_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'FTDI Serial: Failed to register new serial device');

   {Destroy Serial}
   SerialDeviceDestroy(@Serial.Serial);

   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Check Interface}
 if Interrface = nil then
  begin
   {Update Device}
   Device.DriverData:=Serial;
  end
 else
  begin
   {Update Interface}
   Interrface.DriverData:=Serial;
  end;

 {Return Result}
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function FTDISerialDriverUnbind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
{Unbind the FTDI Serial driver from a USB device}
{Device: The USB device to unbind from}
{Interrface: The USB interface to unbind from (or nil for whole device)}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var
 Serial:PFTDISerialDevice;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {Check Interface}
 if Interrface = nil then
  begin
   {Check Driver}
   if Device.Driver <> FTDISerialDriver then Exit;
  end
 else
  begin
   {Check Driver}
   if Interrface.Driver <> FTDISerialDriver then Exit;
  end;

 {$IFDEF FTDISERIAL_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'FTDI Serial: Unbinding USB device (Manufacturer=' + Device.Manufacturer + ' Product=' + Device.Product + ' Address=' + IntToStr(Device.Address) + ')');
 {$ENDIF}

 {Check Interface}
 if Interrface = nil then
  begin
   {Get Serial}
   Serial:=PFTDISerialDevice(Device.DriverData);
  end
 else
  begin
   {Get Serial}
   Serial:=PFTDISerialDevice(Interrface.DriverData);
  end;
 if Serial = nil then Exit;

 {Close Serial}
 SerialDeviceClose(@Serial.Serial);

 {Check Interface}
 if Interrface = nil then
  begin
   {Update Device}
   Device.DriverData:=nil;
  end
 else
  begin
   {Update Interface}
   Interrface.DriverData:=nil;
  end;

 {Deregister Serial}
 if SerialDeviceDeregister(@Serial.Serial) <> ERROR_SUCCESS then Exit;

 {Destroy Serial}
 SerialDeviceDestroy(@Serial.Serial);

 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

procedure FTDISerialReceiveStart(Request:PUSBRequest);
{Called to continue reception of data to the receive buffer}
{Request: The USB receive request to use}

{Note: Caller must hold the lock on the serial device}
var
 Count:LongWord;
 Available:LongWord;
 Status:LongWord;
 Serial:PFTDISerialDevice;
begin
 {}
 {Check Request}
 if Request = nil then Exit;

 {Get Serial}
 Serial:=PFTDISerialDevice(Request.DriverData);
 if Serial = nil then Exit;

 {Setup Count}
 Count:=0;
 Available:=Serial.Serial.Receive.Size - Serial.Serial.Receive.Count;
 if Available >= FTDISERIAL_BULK_IN_SIZE then
  begin
   Count:=FTDISERIAL_BULK_IN_SIZE;
  end
 else if Available >= Serial.ReceiveSize then
  begin
   Count:=Serial.ReceiveSize;
  end;

 {Check Count}
 if Count > 0 then
  begin
   {Update Request}
   Request.Size:=Count;

   {Update Pending}
   Inc(Serial.PendingCount);

   {Set Active}
   Serial.ReceiveActive:=True;

   {$IFDEF FTDISERIAL_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Request.Device,'FTDI Serial: Resubmitting receive request');
   {$ENDIF}

   {Resubmit Request}
   Status:=USBRequestSubmit(Request);
   if Status <> USB_STATUS_SUCCESS then
    begin
     if USB_LOG_ENABLED then USBLogError(Request.Device,'FTDI Serial: Failed to resubmit receive request: ' + USBStatusToString(Status));

     {Reset Active}
     Serial.ReceiveActive:=False;

     {Update Pending}
     Dec(Serial.PendingCount);
    end;
  end;
end;

{==============================================================================}

procedure FTDISerialReceiveWorker(Request:PUSBRequest);
{Called (by a Worker thread) to process a completed USB request from the FTDI Serial bulk IN endpoint}
{Request: The USB request which has completed}

{Note: The first two bytes of the received packet will be:
        Byte 0: Modem Status
         Bit0 Reserved - must be 1
         Bit1 Reserved - must be 0
         Bit2 Reserved - must be 0
         Bit3 Reserved - must be 0
         Bit4 Clear to Send (CTS)
         Bit5 Data Set Ready (DSR)
         Bit6 Ring Indicator (RI)
         Bit7 Receive Line Signal Detect (RLSD)

        Byte 1: Line Status
         Bit0 Data Ready (DR)
         Bit1 Overrun Error (OE)
         Bit2 Parity Error (PE)
         Bit3 Framing Error (FE)
         Bit4 Break Interrupt (BI)
         Bit5 Transmitter Holding Register (THRE)
         Bit6 Transmitter Empty (TEMT)
         Bit7 Error in RCVR FIFO}
var
 Len:LongWord;
 Buffer:PByte;
 Data:Pointer;
 Size:LongWord;
 Offset:PtrUInt;
 Count:LongWord;
 Start:PtrUInt;
 Added:LongWord;
 Status:LongWord;
 Message:TMessage;
 Available:LongWord;
 Serial:PFTDISerialDevice;
begin
 {}
 {Check Request}
 if Request = nil then Exit;

 {Get Serial}
 Serial:=PFTDISerialDevice(Request.DriverData);
 if Serial <> nil then
  begin
   {Acquire the Lock}
   if MutexLock(Serial.Serial.Lock) = ERROR_SUCCESS then
    begin
     try
      {Update Statistics}
      Inc(Serial.ReceiveComplete);

      {Check State}
      if Serial.Serial.SerialState = SERIAL_STATE_CLOSING then
       begin
        {$IFDEF FTDISERIAL_DEBUG}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'FTDI Serial: Close pending, setting receive request status to USB_STATUS_DEVICE_DETACHED');
        {$ENDIF}

        {Update Request}
        Request.Status:=USB_STATUS_DEVICE_DETACHED;
       end;

      {Check Result}
      if Request.Status = USB_STATUS_SUCCESS then
       begin
        {$IFDEF FTDISERIAL_DEBUG}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'FTDI Serial: Receive complete (Size=' + IntToStr(Request.Size) + ' Actual Size=' + IntToStr(Request.ActualSize) + ')');
        {$ENDIF}

        {Update Status}
        Serial.Serial.SerialStatus:=Serial.Serial.SerialStatus and not(SERIAL_STATUS_BREAK_ERROR or SERIAL_STATUS_PARITY_ERROR or SERIAL_STATUS_FRAMING_ERROR or SERIAL_STATUS_OVERRUN_ERROR);

        {Check Size}
        if Request.ActualSize > 0 then
         begin
          {Setup Count and Size}
          Size:=Request.ActualSize;
          Count:=0;

          {Setup Start}
          Start:=0;
          while Start < Size do
           begin
            {Get Length}
            Len:=Min(Size - Start,Serial.ReceiveSize);
            if Len >= 2 then
             begin
              {Get Buffer}
              Buffer:=PByte(Request.Data + Start);

              {Get Modem Status}
              Status:=Buffer[0];
              if (Status and FTDISERIAL_RECEIVE_STATUS0_MASK) <> (Serial.LastStatus and FTDISERIAL_RECEIVE_STATUS0_MASK) then
               begin
                {Set Status}
                Serial.LastStatus:=Status;
                Serial.Serial.SerialStatus:=Serial.Serial.SerialStatus and not(SERIAL_STATUS_CTS or SERIAL_STATUS_DSR or SERIAL_STATUS_RI);

                {Update Status}
                if (Status and FTDISERIAL_RECEIVE_STATUS0_CTS) <> 0 then
                 begin
                  Serial.Serial.SerialStatus:=Serial.Serial.SerialStatus or SERIAL_STATUS_CTS;
                 end;
                if (Status and FTDISERIAL_RECEIVE_STATUS0_DSR) <> 0 then
                 begin
                  Serial.Serial.SerialStatus:=Serial.Serial.SerialStatus or SERIAL_STATUS_DSR;
                 end;
                if (Status and FTDISERIAL_RECEIVE_STATUS0_RI) <> 0 then
                 begin
                  Serial.Serial.SerialStatus:=Serial.Serial.SerialStatus or SERIAL_STATUS_RI;
                 end;
               end;

              {Get Line Status}
              Status:=Buffer[1];
              if (Status and FTDISERIAL_RECEIVE_STATUS1_MASK) <> 0 then
               begin
                {Update Status}
                if (Status and FTDISERIAL_RECEIVE_STATUS1_OE) <> 0 then
                 begin
                  Serial.Serial.SerialStatus:=Serial.Serial.SerialStatus or SERIAL_STATUS_OVERRUN_ERROR;

                  if SERIAL_LOG_ENABLED then SerialLogError(@Serial.Serial,'FTDI Serial: Overrun error on receive character');
                 end;
                if (Status and FTDISERIAL_RECEIVE_STATUS1_PE) <> 0 then
                 begin
                  Serial.Serial.SerialStatus:=Serial.Serial.SerialStatus or SERIAL_STATUS_PARITY_ERROR;

                  if SERIAL_LOG_ENABLED then SerialLogError(@Serial.Serial,'FTDI Serial: Parity error on receive character');
                 end;
                if (Status and FTDISERIAL_RECEIVE_STATUS1_FE) <> 0 then
                 begin
                  Serial.Serial.SerialStatus:=Serial.Serial.SerialStatus or SERIAL_STATUS_FRAMING_ERROR;

                  if SERIAL_LOG_ENABLED then SerialLogError(@Serial.Serial,'FTDI Serial: Framing error on receive character');
                 end;
                if (Status and FTDISERIAL_RECEIVE_STATUS1_BI) <> 0 then
                 begin
                  Serial.Serial.SerialStatus:=Serial.Serial.SerialStatus or SERIAL_STATUS_BREAK_ERROR;

                  if SERIAL_LOG_ENABLED then SerialLogError(@Serial.Serial,'FTDI Serial: Break error on receive character');
                 end;
               end;

              {Update Length}
              Dec(Len,2);

              {Check Length}
              if Len > 0 then
               begin
                {Setup Offset}
                Offset:=0;

                {Update Buffer}
                Inc(Buffer,2);

                {Start Write}
                Data:=SerialBufferWriteStart(@Serial.Serial.Receive,Available);
                while (Data <> nil) and (Available > 0) and (Len > 0) do
                 begin
                  {Get Added}
                  Added:=Min(Len,Available);

                  {Copy Data}
                  System.Move(Pointer(Buffer + Offset)^,Data^,Added);

                  {Update Count}
                  Inc(Count,Added);

                  {Update Length and Offset}
                  Dec(Len,Added);
                  Inc(Offset,Added);

                  {Complete Write}
                  SerialBufferWriteComplete(@Serial.Serial.Receive,Added);

                  {Start Write}
                  Data:=SerialBufferWriteStart(@Serial.Serial.Receive,Available);
                 end;

                {Check Length}
                if Len > 0 then
                 begin
                  if USB_LOG_ENABLED then USBLogError(Request.Device,'FTDI Serial: Buffer overrun, ' + IntToStr(Len) + ' bytes discarded');
                 end;
               end;
             end
            else
             begin
              if USB_LOG_ENABLED then USBLogError(Request.Device,'FTDI Serial: Malformed packet (Len=' + IntToStr(Len) + ')');

              {Update Statistics}
              Inc(Serial.Serial.ReceiveErrors);
             end;

            {Update Start}
            Inc(Start,Serial.ReceiveSize)
           end;

          {Check Count}
          if Count > 0 then
           begin
            {Set Event}
            EventSet(Serial.Serial.Receive.Wait);
           end;
         end;
       end
      else
       begin
        if USB_LOG_ENABLED then USBLogError(Request.Device,'FTDI Serial: Failed receive request (Status=' + USBStatusToString(Request.Status) + ')');

        {Update Statistics}
        Inc(Serial.Serial.ReceiveErrors);
       end;

      {Reset Active}
      Serial.ReceiveActive:=False;

      {Update Pending}
      Dec(Serial.PendingCount);

      {Check State}
      if Serial.Serial.SerialState = SERIAL_STATE_CLOSING then
       begin
        {Check Pending}
        if Serial.PendingCount = 0 then
         begin
          {Check Waiter}
          if Serial.WaiterThread <> INVALID_HANDLE_VALUE then
           begin
            {$IFDEF FTDISERIAL_DEBUG}
            if USB_LOG_ENABLED then USBLogDebug(Request.Device,'FTDI Serial: Close pending, sending message to waiter thread (Thread=' + IntToHex(Serial.WaiterThread,8) + ')');
            {$ENDIF}

            {Send Message}
            FillChar(Message,SizeOf(TMessage),0);
            ThreadSendMessage(Serial.WaiterThread,Message);
            Serial.WaiterThread:=INVALID_HANDLE_VALUE;
           end;
         end;
       end
      else
       begin
        {Start Receive}
        FTDISerialReceiveStart(Request);
       end;
     finally
      {Release the Lock}
      MutexUnlock(Serial.Serial.Lock);
     end;
    end
   else
    begin
     if USB_LOG_ENABLED then USBLogError(Request.Device,'FTDI Serial: Failed to acquire lock');
    end;
  end
 else
  begin
   if USB_LOG_ENABLED then USBLogError(Request.Device,'FTDI Serial: Receive request invalid');
  end;
end;

{==============================================================================}

procedure FTDISerialReceiveComplete(Request:PUSBRequest);
{Called when a USB request from the FTDI Serial bulk IN endpoint completes}
{Request: The USB request which has completed}
{Note: Request is passed to worker thread for processing to prevent blocking the USB completion}
begin
 {}
 {Check Request}
 if Request = nil then Exit;

 WorkerSchedule(0,TWorkerTask(FTDISerialReceiveWorker),Request,nil)
end;

{==============================================================================}

procedure FTDISerialTransmitStart(Request:PUSBRequest);
{Called to continue transmission of data from the transmit buffer}
{Request: The USB transmit request to use}

{Note: The FTDISERIAL_CHIP_SIO device requires the first byte to be:
        Bit0 1  Reserved must be 1
        Bit1 0  Reserved must be 0
        Bit2..7 Length of message (not including this byte)}

{Note: Caller must hold the lock on the serial device}
var
 Len:LongWord;
 Buffer:PByte;
 Data:Pointer;
 Size:LongWord;
 Offset:PtrUInt;
 Count:LongWord;
 Start:LongWord;
 Status:LongWord;
 Current:LongWord;
 Removed:LongWord;
 Available:LongWord;
 Serial:PFTDISerialDevice;
begin
 {}
 {Check Request}
 if Request = nil then Exit;

 {Get Serial}
 Serial:=PFTDISerialDevice(Request.DriverData);
 if Serial = nil then Exit;

 {Setup Count, Size and Offset}
 Size:=Min(FTDISERIAL_MAX_TRANSMIT,FTDISERIAL_BULK_OUT_SIZE);
 Count:=0;
 Offset:=0;

 {Check Type}
 if Serial.Chip = FTDISERIAL_CHIP_SIO then
  begin
   {Setup Start}
   Start:=0;
   Buffer:=Request.Data;
   while Start < (Size - 1) do
    begin
     {Get Length}
     Len:=Min(Size - Start,Serial.TransmitSize) - 1;
     Current:=0;
     Offset:=Start;

     {Start Read}
     Data:=SerialBufferReadStart(@Serial.Serial.Transmit,Available);
     while (Data <> nil) and (Available > 0) and (Len > 0) do
      begin
       {Get Removed}
       Removed:=Min(Len,Available);

       {Copy Data}
       System.Move(Data^,Pointer(Request.Data + Offset + 1)^,Removed);

       {Update Current}
       Inc(Current,Removed);

       {Update Len and Offset}
       Dec(Len,Removed);
       Inc(Offset,Removed);

       {Complete Read}
       SerialBufferReadComplete(@Serial.Serial.Transmit,Removed);

       {Start Read}
       Data:=SerialBufferReadStart(@Serial.Serial.Transmit,Available);
      end;

     {Check Current}
     if Current = 0 then Break;

     {Set Length}
     Buffer[Start]:=(Current shl 2) or 1;

     {Update Count}
     Inc(Count,Current + 1);

     {Update Start}
     Inc(Start,Serial.TransmitSize)
    end;
  end
 else
  begin
   {Start Read}
   Data:=SerialBufferReadStart(@Serial.Serial.Transmit,Available);
   while (Data <> nil) and (Available > 0) and (Size > 0) do
    begin
     {Get Removed}
     Removed:=Min(Size,Available);

     {Copy Data}
     System.Move(Data^,Pointer(Request.Data + Offset)^,Removed);

     {Update Count}
     Inc(Count,Removed);

     {Update Size and Offset}
     Dec(Size,Removed);
     Inc(Offset,Removed);

     {Complete Read}
     SerialBufferReadComplete(@Serial.Serial.Transmit,Removed);

     {Start Read}
     Data:=SerialBufferReadStart(@Serial.Serial.Transmit,Available);
    end;
  end;

 {Check Count}
 if Count > 0 then
  begin
   {Set Event}
   EventSet(Serial.Serial.Transmit.Wait);

   {Update Request}
   Request.Size:=Count;

   {Update Pending}
   Inc(Serial.PendingCount);

   {Set Active}
   Serial.TransmitActive:=True;

   {$IFDEF FTDISERIAL_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Request.Device,'FTDI Serial: Submitting transmit request');
   {$ENDIF}

   {Submit Request}
   Status:=USBRequestSubmit(Request);
   if Status <> USB_STATUS_SUCCESS then
    begin
     if USB_LOG_ENABLED then USBLogError(Request.Device,'FTDI Serial: Failed to submit transmit request: ' + USBStatusToString(Status));

     {Reset Active}
     Serial.TransmitActive:=False;

     {Update Pending}
     Dec(Serial.PendingCount);
    end;
  end;
end;

{==============================================================================}

procedure FTDISerialTransmitWorker(Request:PUSBRequest);
{Called (by a Worker thread) to process a completed USB request to the FTDI Serial bulk OUT endpoint}
{Request: The USB request which has completed}
var
 Message:TMessage;
 Serial:PFTDISerialDevice;
begin
 {}
 {Check Request}
 if Request = nil then Exit;

 {Get Serial}
 Serial:=PFTDISerialDevice(Request.DriverData);
 if Serial <> nil then
  begin
   {Acquire the Lock}
   if MutexLock(Serial.Serial.Lock) = ERROR_SUCCESS then
    begin
     try
      {Update Statistics}
      Inc(Serial.TransmitComplete);

      {Check State}
      if Serial.Serial.SerialState = SERIAL_STATE_CLOSING then
       begin
        {$IFDEF FTDISERIAL_DEBUG}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'FTDI Serial: Close pending, setting transmit request status to USB_STATUS_DEVICE_DETACHED');
        {$ENDIF}

        {Update Request}
        Request.Status:=USB_STATUS_DEVICE_DETACHED;
       end;

      {Check Result}
      if Request.Status = USB_STATUS_SUCCESS then
       begin
        {$IFDEF FTDISERIAL_DEBUG}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'FTDI Serial: Transmit complete (Size=' + IntToStr(Request.Size) + ' Actual Size=' + IntToStr(Request.ActualSize) + ')');
        {$ENDIF}
       end
      else
       begin
        if USB_LOG_ENABLED then USBLogError(Request.Device,'FTDI Serial: Failed transmit request (Status=' + USBStatusToString(Request.Status) + ')');

        {Update Statistics}
        Inc(Serial.Serial.TransmitErrors);
       end;

      {Reset Active}
      Serial.TransmitActive:=False;

      {Update Pending}
      Dec(Serial.PendingCount);

      {Check State}
      if Serial.Serial.SerialState = SERIAL_STATE_CLOSING then
       begin
        {Check Pending}
        if Serial.PendingCount = 0 then
         begin
          {Check Waiter}
          if Serial.WaiterThread <> INVALID_HANDLE_VALUE then
           begin
            {$IFDEF FTDISERIAL_DEBUG}
            if USB_LOG_ENABLED then USBLogDebug(Request.Device,'FTDI Serial: Close pending, sending message to waiter thread (Thread=' + IntToHex(Serial.WaiterThread,8) + ')');
            {$ENDIF}

            {Send Message}
            FillChar(Message,SizeOf(TMessage),0);
            ThreadSendMessage(Serial.WaiterThread,Message);
            Serial.WaiterThread:=INVALID_HANDLE_VALUE;
           end;
         end;
       end
      else
       begin
        {Start Transmit}
        FTDISerialTransmitStart(Request);
       end;
     finally
      {Release the Lock}
      MutexUnlock(Serial.Serial.Lock);
     end;
    end
   else
    begin
     if USB_LOG_ENABLED then USBLogError(Request.Device,'FTDI Serial: Failed to acquire lock');
    end;
  end
 else
  begin
   if USB_LOG_ENABLED then USBLogError(Request.Device,'FTDI Serial: Transmit request invalid');
  end;
end;

{==============================================================================}

procedure FTDISerialTransmitComplete(Request:PUSBRequest);
{Called when a USB request to the FTDI Serial bulk OUT endpoint completes}
{Request: The USB request which has completed}
{Note: Request is passed to worker thread for processing to prevent blocking the USB completion}
begin
 {}
 {Check Request}
 if Request = nil then Exit;

 WorkerSchedule(0,TWorkerTask(FTDISerialTransmitWorker),Request,nil)
end;

{==============================================================================}
{==============================================================================}
{FTDI Serial Helper Functions}
function FTDISerialCheckDevice(Device:PUSBDevice):LongWord;
{Check the Vendor and Device ID against the supported devices}
{Device: USB device to check}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var
 Count:Integer;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {Check Device IDs}
 for Count:=0 to FTDISERIAL_DEVICE_ID_COUNT - 1 do
  begin
   if (FTDISERIAL_DEVICE_ID[Count].idVendor = Device.Descriptor.idVendor) and (FTDISERIAL_DEVICE_ID[Count].idProduct = Device.Descriptor.idProduct) then
    begin
     Result:=USB_STATUS_SUCCESS;
     Exit;
    end;
  end;

 Result:=USB_STATUS_DEVICE_UNSUPPORTED;
end;

{==============================================================================}

function FTDISerialCheckDeviceAndInterface(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
{Check the Device and Interface against the supported devices}
{Device: USB device to check}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var
 Count:Integer;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {Check Interface}
 if Interrface = nil then Exit;

 {Check Device and Interface IDs}
 for Count:=0 to FTDISERIAL_DEVICE_INTERFACE_ID_COUNT - 1 do
  begin
   if (FTDISERIAL_DEVICE_INTERFACE_ID[Count].idVendor = Device.Descriptor.idVendor) and (FTDISERIAL_DEVICE_INTERFACE_ID[Count].idProduct = Device.Descriptor.idProduct) then
    begin
     if (FTDISERIAL_DEVICE_INTERFACE_ID[Count].bInterfaceClass = Interrface.Descriptor.bInterfaceClass) and (FTDISERIAL_DEVICE_INTERFACE_ID[Count].bInterfaceSubClass = Interrface.Descriptor.bInterfaceSubClass) and  (FTDISERIAL_DEVICE_INTERFACE_ID[Count].bInterfaceProtocol = Interrface.Descriptor.bInterfaceProtocol) then
      begin
       Result:=USB_STATUS_SUCCESS;
       Exit;
      end;
    end;
  end;

 {Check Device and Interface Nos}
 for Count:=0 to FTDISERIAL_DEVICE_INTERFACE_NO_COUNT - 1 do
  begin
   if (FTDISERIAL_DEVICE_INTERFACE_NO[Count].idVendor = Device.Descriptor.idVendor) and (FTDISERIAL_DEVICE_INTERFACE_NO[Count].idProduct = Device.Descriptor.idProduct) then
    begin
     if (FTDISERIAL_DEVICE_INTERFACE_NO[Count].bInterfaceNumber = Interrface.Descriptor.bInterfaceNumber) then
      begin
       Result:=USB_STATUS_SUCCESS;
       Exit;
      end;
    end;
  end;

 Result:=USB_STATUS_DEVICE_UNSUPPORTED;
end;

{==============================================================================}

function FTDISerialPatchDevice(Device:PUSBDevice;var Quirks:LongWord):LongWord;
{Check the USB device for quirks information needed by the driver}
{Device: USB device to check}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {Setup Defaults}
 Quirks:=FTDISERIAL_QUIRK_NONE;

 {Check Device ID}
 if (Device.Descriptor.idVendor = FTDI_VID) and (Device.Descriptor.idProduct = FTDI_8U2232C_PID) then
  begin
   Quirks:=Quirks or FTDISERIAL_QUIRK_8U2232C;
  end
 else if (Device.Descriptor.idVendor = FTDI_VID) and (Device.Descriptor.idProduct = FTDI_HE_TIRA1_PID) then
  begin
   Quirks:=Quirks or FTDISERIAL_QUIRK_HE_TIRA1;
  end
 else if (Device.Descriptor.idVendor = FTDI_VID) and (Device.Descriptor.idProduct = FTDI_USB_UIRT_PID) then
  begin
   Quirks:=Quirks or FTDISERIAL_QUIRK_USB_UIRT;
  end
 else if (Device.Descriptor.idVendor = FTDI_VID) and (Device.Descriptor.idProduct = FTDI_TIAO_UMPA_PID) then
  begin
   Quirks:=Quirks or FTDISERIAL_QUIRK_JTAG;
  end
 else if (Device.Descriptor.idVendor = FTDI_VID) and (Device.Descriptor.idProduct = FTDI_NT_ORIONLXM_PID) then
  begin
   Quirks:=Quirks or FTDISERIAL_QUIRK_JTAG;
  end
 else if (Device.Descriptor.idVendor = FTDI_VID) and (Device.Descriptor.idProduct = FTDI_NDI_HUC_PID) then
  begin
   Quirks:=Quirks or FTDISERIAL_QUIRK_NDI_DEVICE;
  end
 else if (Device.Descriptor.idVendor = FTDI_VID) and (Device.Descriptor.idProduct = FTDI_NDI_SPECTRA_SCU_PID) then
  begin
   Quirks:=Quirks or FTDISERIAL_QUIRK_NDI_DEVICE;
  end
 else if (Device.Descriptor.idVendor = FTDI_VID) and (Device.Descriptor.idProduct = FTDI_NDI_FUTURE_2_PID) then
  begin
   Quirks:=Quirks or FTDISERIAL_QUIRK_NDI_DEVICE;
  end
 else if (Device.Descriptor.idVendor = FTDI_VID) and (Device.Descriptor.idProduct = FTDI_NDI_FUTURE_3_PID) then
  begin
   Quirks:=Quirks or FTDISERIAL_QUIRK_NDI_DEVICE;
  end
 else if (Device.Descriptor.idVendor = FTDI_VID) and (Device.Descriptor.idProduct = FTDI_NDI_AURORA_SCU_PID) then
  begin
   Quirks:=Quirks or FTDISERIAL_QUIRK_NDI_DEVICE;
  end
 else if (Device.Descriptor.idVendor = FTDI_VID) and (Device.Descriptor.idProduct = CYBER_CORTEX_AV_PID) then
  begin
   Quirks:=Quirks or FTDISERIAL_QUIRK_JTAG;
  end
 else if (Device.Descriptor.idVendor = OLIMEX_VID) and (Device.Descriptor.idProduct = OLIMEX_ARM_USB_OCD_PID) then
  begin
   Quirks:=Quirks or FTDISERIAL_QUIRK_JTAG;
  end
 else if (Device.Descriptor.idVendor = OLIMEX_VID) and (Device.Descriptor.idProduct = OLIMEX_ARM_USB_OCD_H_PID) then
  begin
   Quirks:=Quirks or FTDISERIAL_QUIRK_JTAG;
  end
 else if (Device.Descriptor.idVendor = FIC_VID) and (Device.Descriptor.idProduct = FIC_NEO1973_DEBUG_PID) then
  begin
   Quirks:=Quirks or FTDISERIAL_QUIRK_JTAG;
  end
 else if (Device.Descriptor.idVendor = FTDI_VID) and (Device.Descriptor.idProduct = FTDI_OOCDLINK_PID) then
  begin
   Quirks:=Quirks or FTDISERIAL_QUIRK_JTAG;
  end
 else if (Device.Descriptor.idVendor = FTDI_VID) and (Device.Descriptor.idProduct = LMI_LM3S_DEVEL_BOARD_PID) then
  begin
   Quirks:=Quirks or FTDISERIAL_QUIRK_JTAG;
  end
 else if (Device.Descriptor.idVendor = FTDI_VID) and (Device.Descriptor.idProduct = LMI_LM3S_EVAL_BOARD_PID) then
  begin
   Quirks:=Quirks or FTDISERIAL_QUIRK_JTAG;
  end
 else if (Device.Descriptor.idVendor = FTDI_VID) and (Device.Descriptor.idProduct = LMI_LM3S_ICDI_BOARD_PID) then
  begin
   Quirks:=Quirks or FTDISERIAL_QUIRK_JTAG;
  end
 else if (Device.Descriptor.idVendor = FTDI_VID) and (Device.Descriptor.idProduct = FTDI_TURTELIZER_PID) then
  begin
   Quirks:=Quirks or FTDISERIAL_QUIRK_JTAG;
  end
 else if (Device.Descriptor.idVendor = ADI_VID) and (Device.Descriptor.idProduct = ADI_GNICE_PID) then
  begin
   Quirks:=Quirks or FTDISERIAL_QUIRK_JTAG;
  end
 else if (Device.Descriptor.idVendor = ADI_VID) and (Device.Descriptor.idProduct = ADI_GNICEPLUS_PID) then
  begin
   Quirks:=Quirks or FTDISERIAL_QUIRK_JTAG;
  end
 else if (Device.Descriptor.idVendor = MARVELL_VID) and (Device.Descriptor.idProduct = MARVELL_SHEEVAPLUG_PID) then
  begin
   Quirks:=Quirks or FTDISERIAL_QUIRK_JTAG;
  end
 else if (Device.Descriptor.idVendor = FTDI_VID) and (Device.Descriptor.idProduct = MARVELL_OPENRD_PID) then
  begin
   Quirks:=Quirks or FTDISERIAL_QUIRK_JTAG;
  end
 else if (Device.Descriptor.idVendor = FTDI_VID) and (Device.Descriptor.idProduct = TI_XDS100V2_PID) then
  begin
   Quirks:=Quirks or FTDISERIAL_QUIRK_JTAG;
  end
 else if (Device.Descriptor.idVendor = FTDI_VID) and (Device.Descriptor.idProduct = XVERVE_SIGNALYZER_ST_PID) then
  begin
   Quirks:=Quirks or FTDISERIAL_QUIRK_JTAG;
  end
 else if (Device.Descriptor.idVendor = FTDI_VID) and (Device.Descriptor.idProduct = XVERVE_SIGNALYZER_SLITE_PID) then
  begin
   Quirks:=Quirks or FTDISERIAL_QUIRK_JTAG;
  end
 else if (Device.Descriptor.idVendor = FTDI_VID) and (Device.Descriptor.idProduct = XVERVE_SIGNALYZER_SH2_PID) then
  begin
   Quirks:=Quirks or FTDISERIAL_QUIRK_JTAG;
  end
 else if (Device.Descriptor.idVendor = FTDI_VID) and (Device.Descriptor.idProduct = XVERVE_SIGNALYZER_SH4_PID) then
  begin
   Quirks:=Quirks or FTDISERIAL_QUIRK_JTAG;
  end
 else if (Device.Descriptor.idVendor = IONICS_VID) and (Device.Descriptor.idProduct = IONICS_PLUGCOMPUTER_PID) then
  begin
   Quirks:=Quirks or FTDISERIAL_QUIRK_JTAG;
  end
 else if (Device.Descriptor.idVendor = QIHARDWARE_VID) and (Device.Descriptor.idProduct = MILKYMISTONE_JTAGSERIAL_PID) then
  begin
   Quirks:=Quirks or FTDISERIAL_QUIRK_JTAG;
  end
 else if (Device.Descriptor.idVendor = ST_VID) and (Device.Descriptor.idProduct = ST_STMCLT_2232_PID) then
  begin
   Quirks:=Quirks or FTDISERIAL_QUIRK_JTAG;
  end
 else if (Device.Descriptor.idVendor = ST_VID) and (Device.Descriptor.idProduct = ST_STMCLT_4232_PID) then
  begin
   Quirks:=Quirks or FTDISERIAL_QUIRK_STMCLITE;
  end
 else if (Device.Descriptor.idVendor = FTDI_VID) and (Device.Descriptor.idProduct = FTDI_DISTORTEC_JTAG_LOCK_PICK_PID) then
  begin
   Quirks:=Quirks or FTDISERIAL_QUIRK_JTAG;
  end;

 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function FTDISerialCheckJTAGDevice(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
{Check for the first port (interface) on JTAG adapters which is reserved for JTAG}
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {Check Interface}
 if Interrface <> nil then
  begin
   if Interrface = Device.Configuration.Interfaces[0] then
    begin
     Result:=USB_STATUS_DEVICE_UNSUPPORTED;
     Exit;
    end;
  end;

 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function FTDISerialCheck8U2232CDevice(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
{Check for the first port (interface) on 8U2232C adapters which is reserved for JTAG}
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 //To Do //Check Manufacturer / Product (Exclude certain types) //See: ftdi_8u2232c_probe

 {Check Interface}
 if Interrface <> nil then
  begin
   if Interrface = Device.Configuration.Interfaces[0] then
    begin
     Result:=USB_STATUS_DEVICE_UNSUPPORTED;
     Exit;
    end;
  end;

 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function FTDISerialCheckSTMCLiteDevice(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
{Check for the first and second ports (interfaces) on FT4232 adapters which are reserved for JTAG
 or other non UART interfaces. Port (interface) 2 is a standard RS232 UART}
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {Check Interface}
 if Interrface <> nil then
  begin
   if (Interrface = Device.Configuration.Interfaces[0]) or (Interrface = Device.Configuration.Interfaces[1]) then
    begin
     Result:=USB_STATUS_DEVICE_UNSUPPORTED;
     Exit;
    end;
  end;

 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function FTDISerialDetermineType(Serial:PFTDISerialDevice):LongWord;
var
 Version:Word;
 Device:PUSBDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Serial}
 if Serial = nil then Exit;

 {Get Device}
 Device:=PUSBDevice(Serial.Serial.Device.DeviceData);
 if Device = nil then Exit;

 {Setup Defaults}
 Serial.BaudBase:=48000000 div 2;

 {Get Version}
 Version:=WordLEToN(Device.Descriptor.bcdDevice);

 {Check Interface Count}
 if Device.Configuration.Descriptor.bNumInterfaces > 1 then
  begin
   {Multiple Interfaces}
   {Check Version}
   if Version = $0800 then
    begin
     {FT4232H}
     Serial.Chip:=FTDISERIAL_CHIP_FT4232H;
     Serial.BaudBase:=120000000 div 2; {120MHz baud clock}
    end
   else if Version = $0700 then
    begin
     {FT2232H}
     Serial.Chip:=FTDISERIAL_CHIP_FT2232H;
     Serial.BaudBase:=120000000 div 2; {120MHz baud clock}
    end
   else
    begin
     {FT2232C}
     Serial.Chip:=FTDISERIAL_CHIP_FT2232C;
    end;

   {Determine Interface Index}
   case Serial.SerialInterface.Descriptor.bInterfaceNumber of
    0:Serial.Index:=FTDISERIAL_INTERFACE_A;
    1:Serial.Index:=FTDISERIAL_INTERFACE_B;
    2:Serial.Index:=FTDISERIAL_INTERFACE_C;
    3:Serial.Index:=FTDISERIAL_INTERFACE_D;
   end;

   {Check Version}
   if Version < $0500 then
    begin
     if USB_LOG_ENABLED then USBLogInfo(Device,'FTDI Serial: bcdDevice value not valid for multi-interface device');
    end;
  end
 else
  begin
   {Single Interface}
   {Check Version}
   if Version < $0200 then
    begin
     {Original SIO}
     Serial.Chip:=FTDISERIAL_CHIP_SIO;
     Serial.BaudBase:=12000000 div 16;
    end
   else if Version < $0400 then
    begin
     {FT8U232AM (or FT8U245AM)}
     Serial.Chip:=FTDISERIAL_CHIP_FT8U232AM;
    end
   else if Version < $0600 then
    begin
     {FT232BM (or FT245BM)}
     Serial.Chip:=FTDISERIAL_CHIP_FT232BM;
    end
   else if Version < $0900 then
    begin
     {FT232RL}
     Serial.Chip:=FTDISERIAL_CHIP_FT232RL;
    end
   else if version < $1000 then
    begin
     {FT232H}
     Serial.Chip:=FTDISERIAL_CHIP_FT232H;
    end
   else
    begin
     {FT-X series}
     Serial.Chip:=FTDISERIAL_CHIP_FTX;
    end;
  end;

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function FTDISerialGetLatency(Serial:PFTDISerialDevice):LongWord;
var
 Value:Byte;
 Device:PUSBDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Serial}
 if Serial = nil then Exit;

 {Get Device}
 Device:=PUSBDevice(Serial.Serial.Device.DeviceData);
 if Device = nil then Exit;

 {Send Control Request}
 if USBControlRequestEx(Device,nil,FTDISERIAL_GET_LATENCY_TIMER_REQUEST,FTDISERIAL_GET_LATENCY_TIMER_REQUEST_TYPE,0,Serial.Index,@Value,SizeOf(Byte),FTDISERIAL_TIMEOUT,False) <> USB_STATUS_SUCCESS then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Get Latency}
 Serial.Latency:=Value;

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function FTDISerialSetLatency(Serial:PFTDISerialDevice):LongWord;
var
 Value:Word;
 Device:PUSBDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Serial}
 if Serial = nil then Exit;

 {Get Device}
 Device:=PUSBDevice(Serial.Serial.Device.DeviceData);
 if Device = nil then Exit;

 {Send Control Request}
 if USBControlRequestEx(Device,nil,FTDISERIAL_SET_LATENCY_TIMER_REQUEST,FTDISERIAL_SET_LATENCY_TIMER_REQUEST_TYPE,Serial.Latency,Serial.Index,nil,0,FTDISERIAL_TIMEOUT,False) <> USB_STATUS_SUCCESS then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function FTDISerialReset(Serial:PFTDISerialDevice;Value:Word):LongWord;
var
 Device:PUSBDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Serial}
 if Serial = nil then Exit;

 {Get Device}
 Device:=PUSBDevice(Serial.Serial.Device.DeviceData);
 if Device = nil then Exit;

 {Send Control Request}
 if USBControlRequestEx(Device,nil,FTDISERIAL_RESET_REQUEST,FTDISERIAL_RESET_REQUEST_TYPE,Value,Serial.Index,nil,0,FTDISERIAL_TIMEOUT,False) <> USB_STATUS_SUCCESS then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function FTDISerialSetData(Serial:PFTDISerialDevice;Value:Word):LongWord;
var
 Device:PUSBDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Serial}
 if Serial = nil then Exit;

 {Get Device}
 Device:=PUSBDevice(Serial.Serial.Device.DeviceData);
 if Device = nil then Exit;

 {Send Control Request}
 if USBControlRequestEx(Device,nil,FTDISERIAL_SET_DATA_REQUEST,FTDISERIAL_SET_DATA_REQUEST_TYPE,Value,Serial.Index,nil,0,FTDISERIAL_SHORT_TIMEOUT,False) <> USB_STATUS_SUCCESS then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function FTDISerialSetBaudRate(Serial:PFTDISerialDevice;Divisor:LongWord):LongWord;
var
 Value:Word;
 Index:Word;
 Device:PUSBDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Serial}
 if Serial = nil then Exit;

 {Get Device}
 Device:=PUSBDevice(Serial.Serial.Device.DeviceData);
 if Device = nil then Exit;

 {Get Values}
 Value:=Divisor;
 Index:=Divisor shr 16;

 {Check Type}
 case Serial.Chip of
  FTDISERIAL_CHIP_FT2232C,FTDISERIAL_CHIP_FT2232H,FTDISERIAL_CHIP_FT4232H,FTDISERIAL_CHIP_FT232H:begin
    Index:=(Index shl 8) or Serial.Index;
   end;
 end;

 {Send Control Request}
 if USBControlRequestEx(Device,nil,FTDISERIAL_SET_BAUD_RATE_REQUEST,FTDISERIAL_SET_BAUD_RATE_REQUEST_TYPE,Value,Index,nil,0,FTDISERIAL_SHORT_TIMEOUT,False) <> USB_STATUS_SUCCESS then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function FTDISerialSetFlowControl(Serial:PFTDISerialDevice;Index:Word):LongWord;
var
 Device:PUSBDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Serial}
 if Serial = nil then Exit;

 {Get Device}
 Device:=PUSBDevice(Serial.Serial.Device.DeviceData);
 if Device = nil then Exit;

 {Send Control Request}
 if USBControlRequestEx(Device,nil,FTDISERIAL_SET_FLOW_CTRL_REQUEST,FTDISERIAL_SET_FLOW_CTRL_REQUEST_TYPE,0,Index or Serial.Index,nil,0,FTDISERIAL_TIMEOUT,False) <> USB_STATUS_SUCCESS then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function FTDISerialGetModemStatus(Serial:PFTDISerialDevice;var Status:Word):LongWord;
var
 Len:LongWord;
 Device:PUSBDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Setup Defaults}
 Status:=0;

 {Check Serial}
 if Serial = nil then Exit;

 {Get Device}
 Device:=PUSBDevice(Serial.Serial.Device.DeviceData);
 if Device = nil then Exit;

 {Check Type}
 case Serial.Chip of
  FTDISERIAL_CHIP_SIO:begin
    Len:=1;
   end;
  FTDISERIAL_CHIP_FT8U232AM,FTDISERIAL_CHIP_FT232BM,FTDISERIAL_CHIP_FT2232C,FTDISERIAL_CHIP_FT232RL,FTDISERIAL_CHIP_FT2232H,FTDISERIAL_CHIP_FT4232H,FTDISERIAL_CHIP_FT232H,FTDISERIAL_CHIP_FTX:begin
    Len:=2;
   end;
  else
   begin
    Exit;
   end;
 end;

 {Send Control Request}
 if USBControlRequestEx(Device,nil,FTDISERIAL_GET_MODEM_STATUS_REQUEST,FTDISERIAL_GET_MODEM_STATUS_REQUEST_TYPE,0,Serial.Index,@Status,Len,FTDISERIAL_TIMEOUT,False) <> USB_STATUS_SUCCESS then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function FTDISerialSetModemControl(Serial:PFTDISerialDevice;Value:Word):LongWord;
var
 Device:PUSBDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Serial}
 if Serial = nil then Exit;

 {Get Device}
 Device:=PUSBDevice(Serial.Serial.Device.DeviceData);
 if Device = nil then Exit;

 {Send Control Request}
 if USBControlRequestEx(Device,nil,FTDISERIAL_SET_MODEM_CTRL_REQUEST,FTDISERIAL_SET_MODEM_CTRL_REQUEST_TYPE,Value,Serial.Index,nil,0,FTDISERIAL_TIMEOUT,False) <> USB_STATUS_SUCCESS then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function FTDISerialGetDivisor(Serial:PFTDISerialDevice;var BaudRate:LongWord):LongWord;
var
 ProductId:Word;
 Divisor:LongWord;
 Device:PUSBDevice;
begin
 {}
 Result:=LongWord(-1);

 {Check Serial}
 if Serial = nil then Exit;

 {Setup Defaults}
 Divisor:=0;

 {Check Baud}
 if BaudRate = 0 then BaudRate:=SERIAL_BAUD_RATE_STANDARD;

 {Check Type}
 case Serial.Chip of
  FTDISERIAL_CHIP_SIO:begin
    {SIO}
    {Check Baud}
    case BaudRate of
     300:Divisor:=FTDISERIAL_SIO_BAUD300;
     600:Divisor:=FTDISERIAL_SIO_BAUD600;
     1200:Divisor:=FTDISERIAL_SIO_BAUD1200;
     2400:Divisor:=FTDISERIAL_SIO_BAUD2400;
     4800:Divisor:=FTDISERIAL_SIO_BAUD4800;
     9600:Divisor:=FTDISERIAL_SIO_BAUD9600;
     19200:Divisor:=FTDISERIAL_SIO_BAUD19200;
     38400:Divisor:=FTDISERIAL_SIO_BAUD38400;
     57600:Divisor:=FTDISERIAL_SIO_BAUD57600;
     115200:Divisor:=FTDISERIAL_SIO_BAUD115200;
    end;

    {Check Divisor}
    if Divisor = 0 then
     begin
      {Default to 9600}
      BaudRate:=9600;
      Divisor:=FTDISERIAL_SIO_BAUD9600;
     end;
   end;
  FTDISERIAL_CHIP_FT8U232AM:begin
    {FT8U232AM}
    {Check Baud}
    if BaudRate <= 3000000 then
     begin
      {Get Divisor}
      Divisor:=FTDISerialBaudToDivisor232AM(Serial,BaudRate);
     end
    else
     begin
      {Default to 9600}
      BaudRate:=9600;
      Divisor:=FTDISerialBaudToDivisor232AM(Serial,BaudRate);
     end;
   end;
  FTDISERIAL_CHIP_FT232BM,FTDISERIAL_CHIP_FT2232C,FTDISERIAL_CHIP_FT232RL,FTDISERIAL_CHIP_FTX:begin
    {FT232BM / FT2232C / FT232RL / FT-X series}
    {Check Baud}
    if BaudRate <= 3000000 then
     begin
      {Get Device}
      Device:=PUSBDevice(Serial.Serial.Device.DeviceData);
      if Device = nil then Exit;

      {Check Product Id}
      ProductId:=Device.Descriptor.idProduct;
      if ((ProductId = FTDI_NDI_HUC_PID) or (ProductId = FTDI_NDI_SPECTRA_SCU_PID) or (ProductId = FTDI_NDI_FUTURE_2_PID) or (ProductId = FTDI_NDI_FUTURE_3_PID) or (ProductId = FTDI_NDI_AURORA_SCU_PID)) and (BaudRate = 19200) then
       begin
        BaudRate:=1200000;
       end;

      {Get Divisor}
      Divisor:=FTDISerialBaudToDivisor232BM(Serial,BaudRate);
     end
    else
     begin
      {Default to 9600}
      BaudRate:=9600;
      Divisor:=FTDISerialBaudToDivisor232BM(Serial,BaudRate);
     end;
   end;
  FTDISERIAL_CHIP_FT2232H,FTDISERIAL_CHIP_FT4232H,FTDISERIAL_CHIP_FT232H:begin
    {FT2232H / FT4232H / FT232H}
    if (BaudRate <= 12000000) and (BaudRate >= 1200) then
     begin
      {Get Divisor}
      Divisor:=FTDISerialBaudToDivisor2232H(Serial,BaudRate);
     end
    else if BaudRate < 1200 then
     begin
      {Get Divisor}
      Divisor:=FTDISerialBaudToDivisor232BM(Serial,BaudRate);
     end
    else
     begin
      {Default to 9600}
      BaudRate:=9600;
      Divisor:=FTDISerialBaudToDivisor232BM(Serial,BaudRate);
     end;
   end;
 end;

 {Return Result}
 Result:=Divisor;
end;

{==============================================================================}

function FTDISerialBaudToDivisor232AM(Serial:PFTDISerialDevice;BaudRate:LongWord):LongWord;
var
 Divisor:LongWord;
 Divisor3:LongWord;
 BaudBase:LongWord;
begin
 {}
 Result:=LongWord(-1);

 {Check Serial}
 if Serial = nil then Exit;

 {Check Baud}
 if BaudRate = 0 then Exit;

 {Setup Defaults}
 Divisor:=0;
 BaudBase:=48000000;

 {Get Divisor}
 Divisor3:=BaudBase div 2 div BaudRate;
 if (Divisor3 and $07) = $07 then Inc(Divisor3);
 Divisor:=Divisor3 shr 3;
 Divisor3:=Divisor3 and $07;
 if Divisor3 = 1 then
  begin
   Divisor:=Divisor or $c000;
  end
 else if Divisor3 >= 4 then
  begin
   Divisor:=Divisor or $4000;
  end
 else if Divisor3 <> 0 then
  begin
   Divisor:=Divisor or $8000;
  end
 else if Divisor = 1 then
  begin
   {Special case for maximum baud rate}
   Divisor:=0;
  end;

 {Return Result}
 Result:=Divisor;
end;

{==============================================================================}

function FTDISerialBaudToDivisor232BM(Serial:PFTDISerialDevice;BaudRate:LongWord):LongWord;
var
 Divisor:LongWord;
 Divisor3:LongWord;
 BaudBase:LongWord;

const
 DivisorFraction:array[0..7] of Byte = (0,3,2,4,1,5,6,7);

begin
 {}
 Result:=LongWord(-1);

 {Check Serial}
 if Serial = nil then Exit;

 {Check Baud}
 if BaudRate = 0 then Exit;

 {Setup Defaults}
 Divisor:=0;
 BaudBase:=48000000;

 {Get Divisor}
 Divisor3:=BaudBase div 2 div BaudRate;
 Divisor:=Divisor3 shr 3;
 Divisor:=Divisor or (DivisorFraction[Divisor3 and $07] shl 14);

 {Check Divisor (Special cases for highest baud rates)}
 if Divisor = 1 then
  begin
   Divisor:=0;
  end
 else if Divisor = $4001 then
  begin
   Divisor:=1;
  end;

 {Return Result}
 Result:=Divisor;
end;

{==============================================================================}

function FTDISerialBaudToDivisor2232H(Serial:PFTDISerialDevice;BaudRate:LongWord):LongWord;
var
 Divisor:LongWord;
 Divisor3:LongWord;
 BaudBase:LongWord;

const
 DivisorFraction:array[0..7] of Byte = (0,3,2,4,1,5,6,7);

begin
 {}
 Result:=LongWord(-1);

 {Check Serial}
 if Serial = nil then Exit;

 {Check Baud}
 if BaudRate = 0 then Exit;

 {Setup Defaults}
 Divisor:=0;
 BaudBase:=120000000;

 {Hi-speed baud rate is 10-bit sampling instead of 16-bit}
 Divisor3:=BaudBase * 8 div (BaudRate * 10);
 Divisor:=Divisor3 shr 3;
 Divisor:=Divisor or (DivisorFraction[Divisor3 and $07] shl 14);

 {Check Divisor (Special cases for highest baud rates)}
 if Divisor = 1 then
  begin
   Divisor:=0;
  end
 else if Divisor = $4001 then
  begin
   Divisor:=1;
  end;

 {Set this bit to turn off a divide by 2.5 on baud rate generator (This enables baud rates up to 12M but cannot go below 1200 with this bit set)}
 Divisor:=Divisor or $00020000;

 {Return Result}
 Result:=Divisor;
end;

{==============================================================================}
{==============================================================================}

initialization
 FTDISerialInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.

