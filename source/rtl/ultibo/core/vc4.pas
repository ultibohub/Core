{
Ultibo Broadcom VideoCoreIV interface unit.

Copyright (C) 2022 - SoftOz Pty Ltd.

Arch
====

 ARMv6 (ARM1176)
 ARMv7 (Cortex A7)
 ARMv8 (Cortex A53)

Boards
======

 Raspberry Pi - Model A/B/A+/B+/CM1
 Raspberry Pi - Model Zero/ZeroW
 Raspberry Pi 2 - Model B
 Raspberry Pi 3 - Model B/B+/A+
 Raspberry Pi CM3/CM3+
 Raspberry Pi 4 - Model B
 Raspberry Pi 400
 Raspberry Pi CM4
 
Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)
 
Credits
=======

 Information for this unit was obtained from:

  Linux - \drivers\staging\vc04_services\interface\vchiq_arm\* - Copyright (c) 2010-2012 Broadcom

  Linux - \drivers\char\broadcom\vc_sm\* - Copyright 2011-2012 Broadcom Corporation

  Linux - \drivers\char\broadcom\vc_mem.c - Copyright 2010 - 2011 Broadcom Corporation
  
References
==========

 VideoCore APIs - http://elinux.org/Raspberry_Pi_VideoCore_APIs
 
 Raspberry Pi Userland - https://github.com/raspberrypi/userland

 VideoCore IV Documentation - https://docs.broadcom.com/docs/12358545
 
 Windows 10 IoT Userland - https://github.com/ms-iot/userland
 
 Windows 10 IoT VCHIQ - https://github.com/ms-iot/bsp/tree/master/drivers/misc/vchiq
 
VideoCore IV
============
 
 The VideoCore IV (VC4) is the graphics processor (GPU) contained in the Broadcom BCM2835, 2836 and 2837
 SoC used in the Raspberry Pi A/B/A+/B+/CM/2B/3B/3A+/3B+/CM3/CM3+. The Broadcom BCM2838 SoC used in the
 Raspberry Pi 4B/400 uses a new VideoCore VI (VC6) graphics processor however some of the core functionality
 of this unit such as VCHIQ and DispmanX is still compatible and the functionality provided here for those
 subsystems still works.
 
 The VC4 is made up of many subsystems that perform functions ranging from 2D and 3D graphics acceleration
 to hardware audio, video and image encoding and decoding as well as display and camera interfaces.

 This unit provides common definitions, imports and support routines needed to enable all of the VC4 functionality
 for use in Ultibo. In addition to the functionality provided by this unit there are a number of drivers 
 that implement specific parts of the control and communication such as VCHIQ, VCSM and VCCMA.
 
 The majority of the interface between the ARM CPU and the VC4 GPU is contained with the userland libraries
 which are maintained by both Broadcom and Raspberry Pi. These have been ported to Ultibo and are provided
 as a series of static libraries that are included as required in order to expose the appropriate parts
 of the interface.

 There is currently no intention to directly port these libraries to Pascal as they represent a vast and
 actively changing codebase, a direct port would offer no advantages and would require more maintenance
 than the current form.
  
 Building the Userland libraries:
 
  Flags: __DYNAMIC_REENT__
 
  Options: 
 
   Raspberry Pi
   
    -mabi=aapcs
    -marm
    -march=armv6
    -mcpu=arm1176jzf-s
    -mfpu=vfp
    -mfloat-abi=hard
   
   Raspberry Pi2/3/4 and QEMU VersatilePB (32-bit)

    -mabi=aapcs
    -marm
    -march=armv7-a
    -mfpu=vfpv3-d16
    -mfloat-abi=hard
   
   Raspberry Pi3/4 and QEMU VersatilePB (64-bit)
   
    -mabi=lp64 (Note: Supported only by later versions of GCC)
    -march=armv8-a
 
  Build:
 
   Download Userland from https://github.com/ultibohub/userland
  
   Unpack to folder $HOME/userland
   
   (or git clone https://github.com/ultibohub/userland.git)

  Build ARMv6: 
   
   cd
  
   cd userland
  
   export PATH=$HOME/gcc-arm-none-eabi-5_4-2016q2/bin:$PATH
   
   ./buildme.armv6-ultibo
   
  Build ARMv7: 

   cd
  
   cd userland
  
   export PATH=$HOME/gcc-arm-none-eabi-5_4-2016q2/bin:$PATH
   
   ./buildme.armv7-ultibo
  
  Build ARMv8: 

   cd
  
   cd userland
  
   export PATH=$HOME/gcc-linaro-aarch64-none-elf-4.8-2014.04_linux/bin:$PATH
   
   ./buildme.armv8-ultibo
  
 Notes:
 
  Building for 64-bit is not currently fully implemented and is not expected to work.
  
  This is not an Ultibo limitation but applies to the original Linux version of the
  libraries as well.
  
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit VC4;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,HeapManager,Syscalls,Devices,VC4V3D,VC4VCHIQ,{VC4VCSM,}Audio,Video,CTypes,SysUtils; //To Do //Add VCSM when completed

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc} 

{==============================================================================}
{const}
 {VC4 specific constants}
 
const
 {VC4 core libraries}
 libvcos = 'vcos';
 libvchostif = 'vchostif';
 libvchiq_arm = 'vchiq_arm';
 libvcsm = 'vcsm';
 libbcm_host = 'bcm_host';
 
 {VC4 Kronos libraries}
 libEGL = 'EGL';
 libGLESv2 = 'GLESv2';
 libOpenVG = 'OpenVG';
 libWFC = 'WFC';
 
 libbrcmEGL = 'brcmEGL';
 libbrcmGLESv2 = 'brcmGLESv2'; 
 libkhrn_client = 'khrn_client';
 
 libbrcmjpeg = 'brcmjpeg';
 libbrcmOpenVG = 'brcmOpenVG';
 libbrcmWFC = 'brcmWFC';
 
 libopenmaxil = 'openmaxil';
 
 {VC4 MMAL (Multimedia Abstraction Layer) libraries}
 libmmal = 'mmal';
 libmmal_components = 'mmal_components';
 libmmal_core = 'mmal_core';
 libmmal_util = 'mmal_util';
 libmmal_omx = 'mmal_omx';
 libmmal_omxutil = 'mmal_omxutil';
 libmmal_vc_client = 'mmal_vc_client';
 
 {VC4 Container (Reader/Writer) libraries}
 libcontainers = 'containers';
 
 libreader_asf = 'reader_asf';
 libreader_avi = 'reader_avi';
 libreader_binary = 'reader_binary';
 libreader_flv = 'reader_flv';
 libreader_metadata_id3 = 'reader_metadata_id3';
 libreader_mkv = 'reader_mkv';
 libreader_mp4 = 'reader_mp4';
 libreader_mpga = 'reader_mpga';
 libreader_ps = 'reader_ps';
 libreader_qsynth = 'reader_qsynth';
 libreader_raw_video = 'reader_raw_video';
 libreader_rcv = 'reader_rcv';
 libreader_rtp = 'reader_rtp';
 libreader_rtsp = 'reader_rtsp';
 libreader_rv9 = 'reader_rv9';
 libreader_simple = 'reader_simple';
 libreader_wav = 'reader_wav';

 libwriter_asf = 'writer_asf';
 libwriter_avi = 'writer_avi';
 libwriter_binary = 'writer_binary';
 libwriter_dummy = 'writer_dummy';
 libwriter_mp4 = 'writer_mp4';
 libwriter_raw_video = 'writer_raw_video';
 libwriter_simple = 'writer_simple';
 
 {VC4 Client libraries}
 libilclient = 'ilclient';
 libvgfont = 'vgfont';
  
 libvcilcs = 'vcilcs';
  
{==============================================================================}
{Note: The order of these is extremely important}

{VC4 Client libraries}
{$linklib libilclient}
{--$linklib libvgfont} //To Do //Continuing //Not compiling yet

{$linklib libvcilcs}

{VC4 MMAL (Multimedia Abstraction Layer) libraries}
{$linklib libmmal_vc_client}
{$linklib libmmal}
{$linklib libmmal_components}
{$linklib libmmal_core}
{$linklib libmmal_util} 
{$linklib libmmal_omx}
{$linklib libmmal_omxutil}

{VC4 Container (Reader/Writer) libraries}
{$linklib libcontainers}

{$linklib libreader_asf}
{$linklib libreader_avi}
{$linklib libreader_binary}
{$linklib libreader_flv}
{$linklib libreader_metadata_id3}
{$linklib libreader_mkv}
{$linklib libreader_mp4}
{$linklib libreader_mpga}
{$linklib libreader_ps} 
{$linklib libreader_qsynth}
{$linklib libreader_raw_video}
{$linklib libreader_rcv}
{$linklib libreader_rtp}
{$linklib libreader_rtsp}
{$linklib libreader_rv9}
{$linklib libreader_simple}
{$linklib libreader_wav}

{$linklib libwriter_asf}
{$linklib libwriter_avi}
{$linklib libwriter_binary}
{$linklib libwriter_dummy}
{$linklib libwriter_mp4}
{$linklib libwriter_raw_video}
{$linklib libwriter_simple}

{VC4 Kronos libraries}
{$linklib libEGL}
{$linklib libGLESv2}
{$linklib libOpenVG}
{$linklib libWFC}
 
{$linklib libbrcmEGL}
{$linklib libbrcmGLESv2}
{$linklib libkhrn_client}
 
{$linklib libbrcmjpeg}
{$linklib libbrcmOpenVG}
{$linklib libbrcmWFC}
 
{$linklib libopenmaxil}

{VC4 core libraries} 
{$linklib libbcm_host}
{$linklib libvchostif}
{$linklib libvchiq_arm}
{$linklib libvcsm}
{$linklib libvcos}

{Math library (Required for EGL)}
{$linklib m}

{==============================================================================}
{type}
 {VC4 specific types}
 
{==============================================================================}
var
 {VC4 specific variables}
 VC4_FILESYS_START:LongBool = False; {If True then start the VC File Service}
 {Note: According to several reports the vcfiled service is no longer supported. 
  It is disabled here by default as it appears to conflict with MMAL and OpenMAX}
 
{==============================================================================}
{VC4 VCOS Types (From interface\vcos_types.h)}
{$PACKRECORDS C}
{Error return codes - chosen to be similar to errno values}
type
 VCOS_STATUS_T = (
  VCOS_SUCCESS,
  VCOS_EAGAIN,
  VCOS_ENOENT,
  VCOS_ENOSPC,
  VCOS_EINVAL,
  VCOS_EACCESS,
  VCOS_ENOMEM,
  VCOS_ENOSYS,
  VCOS_EEXIST,
  VCOS_ENXIO,
  VCOS_EINTR
 );
 
{$PACKRECORDS DEFAULT}
{==============================================================================}
{VC4 VC Display Types (From interface\vctypes\vc_display_types.h)}
{$PACKRECORDS C}
{Enums of display input format }
type
 VCOS_DISPLAY_INPUT_FORMAT_T = (
  VCOS_DISPLAY_INPUT_FORMAT_INVALID = 0,
  VCOS_DISPLAY_INPUT_FORMAT_RGB888,
  VCOS_DISPLAY_INPUT_FORMAT_RGB565
 );

{For backward compatibility}
const
 DISPLAY_INPUT_FORMAT_INVALID = VCOS_DISPLAY_INPUT_FORMAT_INVALID;    
 DISPLAY_INPUT_FORMAT_RGB888 = VCOS_DISPLAY_INPUT_FORMAT_RGB888;    
 DISPLAY_INPUT_FORMAT_RGB565 = VCOS_DISPLAY_INPUT_FORMAT_RGB565;    
  
type
 DISPLAY_INPUT_FORMAT_T = VCOS_DISPLAY_INPUT_FORMAT_T;
 
 {Enum determining how image data for 3D displays has to be supplied}
 DISPLAY_3D_FORMAT_T = (
  DISPLAY_3D_UNSUPPORTED = 0, {Default}
  DISPLAY_3D_INTERLEAVED,      {For autosteroscopic displays}
  DISPLAY_3D_SBS_FULL_AUTO,    {Side-By-Side, Full Width (also used by some autostereoscopic displays)}
  DISPLAY_3D_SBS_HALF_HORIZ,   {Side-By-Side, Half Width, Horizontal Subsampling (see HDMI spec)}
  DISPLAY_3D_TB_HALF,          {Top-bottom 3D}
  DISPLAY_3D_FRAME_PACKING,    {Frame Packed 3D}
  DISPLAY_3D_FRAME_SEQUENTIAL, {Output left on even frames and right on odd frames (typically 120Hz)}
  DISPLAY_3D_FORMAT_MAX
 );
  
 {Enums of display types}
 DISPLAY_INTERFACE_T = (
  DISPLAY_INTERFACE_MIN,
  DISPLAY_INTERFACE_SMI,
  DISPLAY_INTERFACE_DPI,
  DISPLAY_INTERFACE_DSI,
  DISPLAY_INTERFACE_LVDS,
  DISPLAY_INTERFACE_MAX
 );

 {Display dither setting, used on B0}
 DISPLAY_DITHER_T = (
  DISPLAY_DITHER_NONE = 0,   {Default if not set}
  DISPLAY_DITHER_RGB666 = 1,
  DISPLAY_DITHER_RGB565 = 2,
  DISPLAY_DITHER_RGB555 = 3,
  DISPLAY_DITHER_MAX);
  
 {Info struct}
 PDISPLAY_INFO_T = ^DISPLAY_INFO_T;
 DISPLAY_INFO_T = record
  _type : DISPLAY_INTERFACE_T;           {Type}
  width : uint32_t;                      {Width / Height}
  height : uint32_t;
  input_format : DISPLAY_INPUT_FORMAT_T; {Output format}
  interlaced : uint32_t;                 {Interlaced?}
  output_dither : DISPLAY_DITHER_T;      {Output dither setting (if required)}
  pixel_freq : uint32_t;                 {Pixel frequency}
  line_rate : uint32_t;                  {Line rate in lines per second}
  format_3d : DISPLAY_3D_FORMAT_T;       {Format required for image data for 3D displays}
  use_pixelvalve_1 : uint32_t;           {If display requires PV1 (e.g. DSI1), special config is required in HVS}
  dsi_video_mode : uint32_t;             {Set for DSI displays which use video mode}
  hvs_channel : uint32_t;                {Select HVS channel (usually 0)}
 end;
 
{$PACKRECORDS DEFAULT}
{==============================================================================}
{VC4 VC Image Types (From interface\vctypes\vc_image_types.h)}
{$PACKRECORDS C}
{We have so many rectangle types; let's try to introduce a common one.  }
type
 Ptag_VC_RECT_T = ^tag_VC_RECT_T;
 tag_VC_RECT_T = record
  x : int32_t;
  y : int32_t;
  width : int32_t;
  height : int32_t;
 end;
 VC_RECT_T = tag_VC_RECT_T;
 PVC_RECT_T = ^VC_RECT_T;

 {Types of image supported}
 VC_IMAGE_TYPE_T = (
  VC_IMAGE_MIN = 0,      {Bounds for error checking}
  
  VC_IMAGE_RGB565 = 1,
  VC_IMAGE_1BPP,
  VC_IMAGE_YUV420,
  VC_IMAGE_48BPP,
  VC_IMAGE_RGB888,
  VC_IMAGE_8BPP,
  VC_IMAGE_4BPP,          {4bpp palettised image}
  VC_IMAGE_3D32,          {A separated format of 16 colour/light shorts followed by 16 z values}
  VC_IMAGE_3D32B,         {16 colours followed by 16 z values}
  VC_IMAGE_3D32MAT,       {A separated format of 16 material/colour/light shorts followed by 16 z values}
  VC_IMAGE_RGB2X9,        {32 bit format containing 18 bits of 6.6.6 RGB, 9 bits per short}
  VC_IMAGE_RGB666,        {32-bit format holding 18 bits of 6.6.6 RGB}
  VC_IMAGE_PAL4_OBSOLETE, {4bpp palettised image with embedded palette}
  VC_IMAGE_PAL8_OBSOLETE, {8bpp palettised image with embedded palette}
  VC_IMAGE_RGBA32,        {RGB888 with an alpha byte after each pixel (Isn't it BEFORE each pixel?)}
  VC_IMAGE_YUV422,        {A line of Y (32-byte padded), a line of U (16-byte padded), and a line of V (16-byte padded)}
  VC_IMAGE_RGBA565,       {RGB565 with a transparent patch}
  VC_IMAGE_RGBA16,        {Compressed (4444) version of RGBA32}
  VC_IMAGE_YUV_UV,        {VCIII codec format}
  VC_IMAGE_TF_RGBA32,     {VCIII T-format RGBA8888}
  VC_IMAGE_TF_RGBX32,     {VCIII T-format RGBx8888}
  VC_IMAGE_TF_FLOAT,      {VCIII T-format float}
  VC_IMAGE_TF_RGBA16,     {VCIII T-format RGBA4444}
  VC_IMAGE_TF_RGBA5551,   {VCIII T-format RGB5551}
  VC_IMAGE_TF_RGB565,     {VCIII T-format RGB565}
  VC_IMAGE_TF_YA88,       {VCIII T-format 8-bit luma and 8-bit alpha}
  VC_IMAGE_TF_BYTE,       {VCIII T-format 8 bit generic sample}
  VC_IMAGE_TF_PAL8,       {VCIII T-format 8-bit palette}
  VC_IMAGE_TF_PAL4,       {VCIII T-format 4-bit palette}
  VC_IMAGE_TF_ETC1,       {VCIII T-format Ericsson Texture Compressed}
  VC_IMAGE_BGR888,        {RGB888 with R & B swapped}
  VC_IMAGE_BGR888_NP,     {RGB888 with R & B swapped, but with no pitch, i.e. no padding after each row of pixels}
  VC_IMAGE_BAYER,         {Bayer image, extra defines which variant is being used}
  VC_IMAGE_CODEC,         {General wrapper for codec images e.g. JPEG from camera}
  VC_IMAGE_YUV_UV32,      {VCIII codec format}
  VC_IMAGE_TF_Y8,         {VCIII T-format 8-bit luma}
  VC_IMAGE_TF_A8,         {VCIII T-format 8-bit alpha}
  VC_IMAGE_TF_SHORT,      {VCIII T-format 16-bit generic sample}
  VC_IMAGE_TF_1BPP,       {VCIII T-format 1bpp black/white}
  VC_IMAGE_OPENGL,        
  VC_IMAGE_YUV444I,       {VCIII-B0 HVS YUV 4:4:4 interleaved samples}
  VC_IMAGE_YUV422PLANAR,  {Y, U, & V planes separately (VC_IMAGE_YUV422 has them interleaved on a per line basis)}
  VC_IMAGE_ARGB8888,      {32bpp with 8bit alpha at MS byte, with R, G, B (LS byte)}
  VC_IMAGE_XRGB8888,      {32bpp with 8bit unused at MS byte, with R, G, B (LS byte)}
  
  VC_IMAGE_YUV422YUYV,    {Interleaved 8 bit samples of Y, U, Y, V}
  VC_IMAGE_YUV422YVYU,    {Interleaved 8 bit samples of Y, V, Y, U}
  VC_IMAGE_YUV422UYVY,    {Interleaved 8 bit samples of U, Y, V, Y}
  VC_IMAGE_YUV422VYUY,    {Interleaved 8 bit samples of V, Y, U, Y}
  
  VC_IMAGE_RGBX32,        {32bpp like RGBA32 but with unused alpha}
  VC_IMAGE_RGBX8888,      {32bpp, corresponding to RGBA with unused alpha}
  VC_IMAGE_BGRX8888,      {32bpp, corresponding to BGRA with unused alpha}
  
  VC_IMAGE_YUV420SP,      {Y as a plane, then UV byte interleaved in plane with with same pitch, half height}
  
  VC_IMAGE_YUV444PLANAR,  {Y, U, & V planes separately 4:4:4}
  
  VC_IMAGE_TF_U8,         {T-format 8-bit U - same as TF_Y8 buf from U plane}
  VC_IMAGE_TF_V8,         {T-format 8-bit U - same as TF_Y8 buf from V plane}
  
  VC_IMAGE_YUV420_16,     {YUV4:2:0 planar, 16bit values}
  VC_IMAGE_YUV_UV_16,     {YUV4:2:0 codec format, 16bit values}
  
  VC_IMAGE_MAX,           {Bounds for error checking}
  VC_IMAGE_FORCE_ENUM_16BIT = $ffff
 );
 
  {Image transformations (flips and 90 degree rotations). These are made out of 3 primitives (transpose is done first). These must match the DISPMAN and Media Player definitions}
const
 TRANSFORM_HFLIP = 1 shl 0;    
 TRANSFORM_VFLIP = 1 shl 1;    
 TRANSFORM_TRANSPOSE = 1 shl 2;    

type
 VC_IMAGE_TRANSFORM_T = (
  VC_IMAGE_ROT0 = 0,
  VC_IMAGE_MIRROR_ROT0 = TRANSFORM_HFLIP,
  VC_IMAGE_MIRROR_ROT180 = TRANSFORM_VFLIP,
  VC_IMAGE_ROT180 = TRANSFORM_HFLIP or TRANSFORM_VFLIP,
  VC_IMAGE_MIRROR_ROT90 = TRANSFORM_TRANSPOSE,
  VC_IMAGE_ROT270 = TRANSFORM_TRANSPOSE or TRANSFORM_HFLIP,
  VC_IMAGE_ROT90 = TRANSFORM_TRANSPOSE or TRANSFORM_VFLIP,
  VC_IMAGE_MIRROR_ROT270 = (TRANSFORM_TRANSPOSE or TRANSFORM_HFLIP) or TRANSFORM_VFLIP
 );
 
 {Defined to be identical to register bits }
 VC_IMAGE_BAYER_ORDER_T = (
  VC_IMAGE_BAYER_RGGB = 0,
  VC_IMAGE_BAYER_GBRG = 1,
  VC_IMAGE_BAYER_BGGR = 2,
  VC_IMAGE_BAYER_GRBG = 3
 );
 
 {Defined to be identical to register bits }
 VC_IMAGE_BAYER_FORMAT_T = (
  VC_IMAGE_BAYER_RAW6 = 0,
  VC_IMAGE_BAYER_RAW7 = 1,
  VC_IMAGE_BAYER_RAW8 = 2,
  VC_IMAGE_BAYER_RAW10 = 3,
  VC_IMAGE_BAYER_RAW12 = 4,
  VC_IMAGE_BAYER_RAW14 = 5,
  VC_IMAGE_BAYER_RAW16 = 6,
  VC_IMAGE_BAYER_RAW10_8 = 7,
  VC_IMAGE_BAYER_RAW12_8 = 8,
  VC_IMAGE_BAYER_RAW14_8 = 9,
  VC_IMAGE_BAYER_RAW10L = 11,
  VC_IMAGE_BAYER_RAW12L = 12,
  VC_IMAGE_BAYER_RAW14L = 13,
  VC_IMAGE_BAYER_RAW16_BIG_ENDIAN = 14,
  VC_IMAGE_BAYER_RAW4 = 15
 );

{$PACKRECORDS DEFAULT}
{==============================================================================}
{VC4 VC Image Structures (From interface\vctypes\vc_image_structs.h)}
{$PACKRECORDS C}
const
 {Format specific infos for vc images}
 {YUV information, co-sited h/v flags & colour space words}
 VC_IMAGE_YUVINFO_UNSPECIFIED = 0;            {Unknown or unset - defaults to BT601 interstitial}
 {Colour-space conversions data [4 bits]}
 {Note that colour conversions for SMPTE 170M are identical to BT.601}
 VC_IMAGE_YUVINFO_CSC_ITUR_BT601 = 1;         {ITU-R BT.601-5 [SDTV] (compatible with VideoCore-II)}  
 VC_IMAGE_YUVINFO_CSC_ITUR_BT709 = 2;         {ITU-R BT.709-3 [HDTV]}
 VC_IMAGE_YUVINFO_CSC_JPEG_JFIF = 3;          {JPEG JFIF}
 VC_IMAGE_YUVINFO_CSC_FCC = 4;                {Title 47 Code of Federal Regulations (2003) 73.682 (a) (20)}
 VC_IMAGE_YUVINFO_CSC_SMPTE_240M = 5;         {Society of Motion Picture and Television Engineers 240M (1999)}
 VC_IMAGE_YUVINFO_CSC_ITUR_BT470_2_M = 6;     {ITU-R BT.470-2 System M}
 VC_IMAGE_YUVINFO_CSC_ITUR_BT470_2_BG = 7;    {ITU-R BT.470-2 System B,G}
 VC_IMAGE_YUVINFO_CSC_JPEG_JFIF_Y16_255 = 8;  {JPEG JFIF, but with 16..255 luma}
 VC_IMAGE_YUVINFO_CSC_CUSTOM = 15;            {Custom colour matrix follows header}
 VC_IMAGE_YUVINFO_CSC_SMPTE_170M = VC_IMAGE_YUVINFO_CSC_ITUR_BT601;
 {Co-sited flags, assumed interstitial if not co-sited [2 bits]} 
 VC_IMAGE_YUVINFO_H_COSITED = 256;
 VC_IMAGE_YUVINFO_V_COSITED = 512;
  
 VC_IMAGE_YUVINFO_TOP_BOTTOM = 1024;
 VC_IMAGE_YUVINFO_DECIMATED = 2048;
 VC_IMAGE_YUVINFO_PACKED = 4096;
 {Certain YUV image formats can either be V/U interleaved or U/V interleaved}
 VC_IMAGE_YUVINFO_IS_VU = $8000;
 {Force Metaware to use 16 bits}
 VC_IMAGE_YUVINFO_FORCE_ENUM_16BIT = $ffff;

const
 VC_IMAGE_YUV_UV_STRIPE_WIDTH_LOG2 = 7;    
 VC_IMAGE_YUV_UV_STRIPE_WIDTH = 1 shl VC_IMAGE_YUV_UV_STRIPE_WIDTH_LOG2;    
 VC_IMAGE_YUV_UV32_STRIPE_WIDTH_LOG2 = 5;    
 VC_IMAGE_YUV_UV32_STRIPE_WIDTH = 1 shl VC_IMAGE_YUV_UV32_STRIPE_WIDTH_LOG2;    
 
 {64 pixel wide stripes, 128 byte wide as 16bits/component}
 VC_IMAGE_YUV_UV_16_STRIPE_WIDTH_LOG2 = 6;    
 VC_IMAGE_YUV_UV_16_STRIPE_WIDTH = 1 shl VC_IMAGE_YUV_UV_16_STRIPE_WIDTH_LOG2;    
 VC_IMAGE_YUV_UV_16_STRIPE_STRIDE_LOG2 = 7;    
 VC_IMAGE_YUV_UV_16_STRIPE_STRIDE = 1 shl VC_IMAGE_YUV_UV_16_STRIPE_STRIDE_LOG2;    
      
 {The image structure.}
type
 Pvc_image_extra_uv_s = ^vc_image_extra_uv_s;
 vc_image_extra_uv_s = record
  u : pointer;
  v : pointer;
  vpitch : longint;
 end;
 VC_IMAGE_EXTRA_UV_T = vc_image_extra_uv_s;
 PVC_IMAGE_EXTRA_UV_T = ^VC_IMAGE_EXTRA_UV_T;
      
 Pvc_image_extra_rgba_s = ^vc_image_extra_rgba_s;
 vc_image_extra_rgba_s = record
  value: LongWord; {component_order   : 24 (diagnostic use only)}
                   {normalised_alpha  : 1}
                   {transparent_colour: 1}
                   {unused_26_31      : 6}
  arg: LongWord;
 end;
 VC_IMAGE_EXTRA_RGBA_T = vc_image_extra_rgba_s;
 PVC_IMAGE_EXTRA_RGBA_T = ^VC_IMAGE_EXTRA_RGBA_T;
 
 Pvc_image_extra_pal_s = ^vc_image_extra_pal_s;
 vc_image_extra_pal_s = record
  palette: Psmallint;
  palette32: int;  {palette32 : 1}
 end;
 VC_IMAGE_EXTRA_PAL_T = vc_image_extra_pal_s;
 PVC_IMAGE_EXTRA_PAL_T = ^VC_IMAGE_EXTRA_PAL_T;
 
 {These fields are subject to change / being moved around}
 Pvc_image_extra_tf_s = ^vc_image_extra_tf_s;
 vc_image_extra_tf_s = record
  value: LongWord; {mipmap_levels  : 8}
                   {xxx            : 23}
                   {cube_map       : 1}
  palette: Pointer;
 end;
 VC_IMAGE_EXTRA_TF_T = vc_image_extra_tf_s;
 PVC_IMAGE_EXTRA_TF_T = ^VC_IMAGE_EXTRA_TF_T;

 Pvc_image_extra_bayer_s = ^vc_image_extra_bayer_s;
 vc_image_extra_bayer_s = record
  order : word;
  format : word;
  block_length : longint;
 end;
 VC_IMAGE_EXTRA_BAYER_T = vc_image_extra_bayer_s;
 PVC_IMAGE_EXTRA_BAYER_T = ^VC_IMAGE_EXTRA_BAYER_T;
      
 {The next block can be used with Visual C++ which treats enums as long ints}
 Pvc_image_extra_msbayer_s = ^vc_image_extra_msbayer_s;
 vc_image_extra_msbayer_s = record
  order : byte;
  format : byte;
  dummy1 : byte;
  dummy2 : byte;
  block_length : longint;
 end;
 VC_IMAGE_EXTRA_MSBAYER_T = vc_image_extra_msbayer_s;
 PVC_IMAGE_EXTRA_MSBAYER_T = ^VC_IMAGE_EXTRA_MSBAYER_T;
      
 {NB this will be copied to image.size in parmalloc()}
 Pvc_image_extra_codec_s = ^vc_image_extra_codec_s;
 vc_image_extra_codec_s = record
  fourcc : longint;
  maxsize : longint;
 end;
 VC_IMAGE_EXTRA_CODEC_T = vc_image_extra_codec_s;
 PVC_IMAGE_EXTRA_CODEC_T = ^VC_IMAGE_EXTRA_CODEC_T;
      
const
 VC_IMAGE_OPENGL_RGBA32 = $14011908;    {GL_UNSIGNED_BYTE GL_RGBA}
 VC_IMAGE_OPENGL_RGB24 = $14011907;    {GL_UNSIGNED_BYTE GL_RGB}
 VC_IMAGE_OPENGL_RGBA16 = $80331908;    {GL_UNSIGNED_SHORT_4_4_4_4 GL_RGBA}
 VC_IMAGE_OPENGL_RGBA5551 = $80341908;    {GL_UNSIGNED_SHORT_5_5_5_1 GL_RGBA}
 VC_IMAGE_OPENGL_RGB565 = $83631907;    {GL_UNSIGNED_SHORT_5_6_5 GL_RGB}
 VC_IMAGE_OPENGL_YA88 = $1401190A;    {GL_UNSIGNED_BYTE GL_LUMINANCE_ALPHA}
 VC_IMAGE_OPENGL_Y8 = $14011909;    {GL_UNSIGNED_BYTE GL_LUMINANCE}
 VC_IMAGE_OPENGL_A8 = $14011906;    {GL_UNSIGNED_BYTE GL_ALPHA}
 VC_IMAGE_OPENGL_ETC1 = $8D64;    {GL_ETC1_RGB8_OES}
 VC_IMAGE_OPENGL_PALETTE4_RGB24 = $8B90;    {GL_PALETTE4_RGB8_OES}
 VC_IMAGE_OPENGL_PALETTE4_RGBA32 = $8B91;    {GL_PALETTE4_RGBA8_OES}
 VC_IMAGE_OPENGL_PALETTE4_RGB565 = $8B92;    {GL_PALETTE4_R5_G6_B5_OES}
 VC_IMAGE_OPENGL_PALETTE4_RGBA16 = $8B93;    {GL_PALETTE4_RGBA4_OES}
 VC_IMAGE_OPENGL_PALETTE4_RGB5551 = $8B94;    {GL_PALETTE4_RGB5_A1_OES}
 VC_IMAGE_OPENGL_PALETTE8_RGB24 = $8B95;    {GL_PALETTE8_RGB8_OES}
 VC_IMAGE_OPENGL_PALETTE8_RGBA32 = $8B96;    {GL_PALETTE8_RGBA8_OES}
 VC_IMAGE_OPENGL_PALETTE8_RGB565 = $8B97;    {GL_PALETTE8_R5_G6_B5_OES}
 VC_IMAGE_OPENGL_PALETTE8_RGBA16 = $8B98;    {GL_PALETTE8_RGBA4_OES}
 VC_IMAGE_OPENGL_PALETTE8_RGB5551 = $8B99;    {GL_PALETTE8_RGB5_A1_OES}
     
type
 Pvc_image_extra_opengl_s = ^vc_image_extra_opengl_s;
 vc_image_extra_opengl_s = record
  format_and_type : dword;
  palette : pointer;
 end;
 VC_IMAGE_EXTRA_OPENGL_T = vc_image_extra_opengl_s;
 PVC_IMAGE_EXTRA_OPENGL_T = ^VC_IMAGE_EXTRA_OPENGL_T;

 PVC_IMAGE_EXTRA_T = ^VC_IMAGE_EXTRA_T;
 VC_IMAGE_EXTRA_T = record
  case longint of
   0 : ( uv : VC_IMAGE_EXTRA_UV_T );
   1 : ( rgba : VC_IMAGE_EXTRA_RGBA_T );
   2 : ( pal : VC_IMAGE_EXTRA_PAL_T );
   3 : ( tf : VC_IMAGE_EXTRA_TF_T );
   4 : ( bayer : VC_IMAGE_EXTRA_BAYER_T );
   5 : ( msbayer : VC_IMAGE_EXTRA_MSBAYER_T );
   6 : ( codec : VC_IMAGE_EXTRA_CODEC_T );
   7 : ( opengl : VC_IMAGE_EXTRA_OPENGL_T );
 end;
     
 {Structure containing various colour meta-data for each format}
type
 PVC_IMAGE_INFO_T = ^VC_IMAGE_INFO_T;
 VC_IMAGE_INFO_T = record
  case longint of
   1 : ( yuv : word );               {Information pertinent to all YUV implementations}
   2 : ( info : word );              {Dummy, force size to min 16 bits}
 end;
 
 {Image handle object, which must be locked before image data becomes accessible.
  A handle to an image where the image data does not have a guaranteed storage location.  A call to vc_image_lock() must be made to convert
  this into a VC_IMAGE_BUF_T, which guarantees that image data can be accessed safely.
  This type will also be used in cases where it's unclear whether or not the buffer is already locked, and in legacy code}
type
 PVC_IMAGE_T = ^VC_IMAGE_T;
 VC_IMAGE_T = record
  _type : word;                            {Should restrict to 16 bits}
  info : VC_IMAGE_INFO_T;                  {Format-specific info; zero for VC02 behaviour}
  width : word;                            {Width in pixels}
  height : word;                           {Height in pixels}
  pitch : longint;                         {Pitch of image_data array in bytes}
  size : longint;                          {Number of bytes available in image_data array}
  image_data : Pointer;                    {Pixel data}
  extra : VC_IMAGE_EXTRA_T;                {Extra data like palette pointer}
  metadata : Pointer;                      {Metadata header for the image (vc_metadata_header_s)}
  pool_object : Pointer;                   {Non NULL if image was allocated from a vc_pool (opaque_vc_pool_object_s)}
  mem_handle : uint32_t;                   {The mem handle for relocatable memory storage}
  metadata_size : longint;                 {Size of metadata of each channel in bytes}
  channel_offset : longint;                {Offset of consecutive channels in bytes}
  video_timestamp : uint32_t;              {90000 Hz RTP times domain - derived from audio timestamp}
  num_channels : uint8_t;                  {Number of channels (2 for stereo)}
  current_channel : uint8_t;               {The channel this header is currently pointing to}
  linked_multichann_flag : uint8_t;        {Indicate the header has the linked-multichannel structure}
  is_channel_linked : uint8_t;             {Track if the above structure is been used to link the header into a linked-mulitchannel image}
  channel_index : uint8_t;                 {Index of the channel this header represents while it is being linked}
  _dummy : array[0..2] of uint8_t;         {Pad struct to 64 bytes}
 end;
 
{$PACKRECORDS DEFAULT}
{==============================================================================}
{VC4 VCHI Mem Handle (From interface\vchi\vchi_mh.h)}
{$PACKRECORDS C}
type
 VCHI_MEM_HANDLE_T = int32_t;
 
const 
 VCHI_MEM_HANDLE_INVALID = 0;
 
{$PACKRECORDS DEFAULT}
{==============================================================================}
{VC4 VCHI Connection Types (From interface\vchi\connections\connection.h)}
{$PACKRECORDS C}
type
 PPVCHI_CONNECTION_T = ^PVCHI_CONNECTION_T;
 PVCHI_CONNECTION_T = ^VCHI_CONNECTION_T;
 VCHI_CONNECTION_T = record
  {Opaque structure}
 end;

 //To Do //Continuing
 
{$PACKRECORDS DEFAULT}
{==============================================================================}
{VC4 VCHI Types (From interface\vchi\vchi.h)}
{$PACKRECORDS C}
type
 PPVCHI_INSTANCE_T = ^PVCHI_INSTANCE_T;
 PVCHI_INSTANCE_T = ^VCHI_INSTANCE_T;
 VCHI_INSTANCE_T = record
  {Opaque structure}
 end;

//To Do //Continuing

{$PACKRECORDS DEFAULT}
{==============================================================================}
{VC4 VCHI Common Types (From vchi_common.h)}
{$PACKRECORDS C}
type
 PVCHI_CONNECTION_API_T = ^VCHI_CONNECTION_API_T;
 VCHI_CONNECTION_API_T = record
  {Opaque structure}
 end;

 PVCHI_MESSAGE_DRIVER_T = ^VCHI_MESSAGE_DRIVER_T;
 VCHI_MESSAGE_DRIVER_T = record
  {Opaque structure}
 end;

//To Do //Continuing

{$PACKRECORDS DEFAULT}
{==============================================================================}
{VC4 VCHIQ Interface Types (From vchiq_if.h)}
{$PACKRECORDS C}
type
 VCHIQ_STATUS_T = (
  VCHIQ_ERROR   = -1,
  VCHIQ_SUCCESS = 0,
  VCHIQ_RETRY   = 1
 );
 
 PPVCHIQ_INSTANCE_T = ^PVCHIQ_INSTANCE_T;
 PVCHIQ_INSTANCE_T = ^VCHIQ_INSTANCE_T;
 VCHIQ_INSTANCE_T = record
  {Opaque structure}
 end;
 
//To Do //Continuing

{$PACKRECORDS DEFAULT}
{==============================================================================}
{VC4 ????? (From ??????.h)}
{$PACKRECORDS C}

//To Do //Continuing

{$PACKRECORDS DEFAULT}
{==============================================================================}
{VC4 DispmanX Types (From interface\vmcs_host\vc_dispmanx_types.h)}
{$PACKRECORDS C}
type
 PDISPMANX_ELEMENT_HANDLE_T = ^DISPMANX_ELEMENT_HANDLE_T;
 DISPMANX_ELEMENT_HANDLE_T = uint32_t;
 
{$PACKRECORDS DEFAULT}
{==============================================================================}
{VC4 EGL Platform Types (From EGL\eglplatform.h)}
{$PACKRECORDS C}
type
 PEGL_DISPMANX_WINDOW_T = ^EGL_DISPMANX_WINDOW_T;
 EGL_DISPMANX_WINDOW_T = record
  element: DISPMANX_ELEMENT_HANDLE_T;
  width: int;
  height: int;
 end;

{$PACKRECORDS DEFAULT}
{==============================================================================}
{Initialization Functions}
procedure VC4Init;

function VC4Start:LongWord;
function VC4Stop:LongWord;

{==============================================================================}
{VC4 Mailbox Functions}
function VC4MemoryAllocate(Length,Alignment,Flags:LongWord):THandle; inline;
function VC4MemoryRelease(Handle:THandle):LongWord; inline;
function VC4MemoryLock(Handle:THandle):LongWord; inline;
function VC4MemoryUnlock(Handle:THandle):LongWord; inline;

function VC4ExecuteCode(Address:Pointer;R0,R1,R2,R3,R4,R5:LongWord):LongWord; inline;

function VC4QPUEnable(Enable:LongWord):LongWord; inline;
function VC4QPUExecute(NumQPUs,Control,NoFlush,Timeout:LongWord):LongWord; inline;

function VC4VCHIQEnable(Address:LongWord):LongWord; inline;

{==============================================================================}
{VC4 VCOS Interface Functions (From \opt\vc\include\interface\vcos\*)}
function VCOSInit: VCOS_STATUS_T; cdecl; external libvcos name 'vcos_init';
procedure VCOSDeinit; cdecl; external libvcos name 'vcos_deinit';

//To Do //More

{==============================================================================}
{VC4 VCHOSTIF / VCILCS Interface Functions (From \opt\vc\include\interface\vmcs_host\*)}
//To Do
//vc_vchi_cec_init
//vc_cec_*

//vc_vchi_tv_init
//vc_tv_*

//vc_gencmd_init
//vc_gencmd_*

//To Do //Note: These are in the DispmanX unit
//vc_vchi_dispmanx_init
//vc_dispmanx_*

function VCVCHIFilesysInit(instance: PVCHI_INSTANCE_T; connections: PPVCHI_CONNECTION_T; num_connections: uint32_t): int; cdecl; external libvchostif name 'vc_vchi_filesys_init';
procedure VCFilesysDeinit; cdecl; external libvchostif name 'vc_filesys_stop';

//To Do //Note: Only for implementation of an equivalent to vcfilesysd
//vc_filesys_*

{==============================================================================}
{VC4 VCHI Interface Functions (From \opt\vc\include\interface\vchi\*)}
{Note: These import the C functions from the Userland libraries, not the functions from the VC4VCHIQ driver unit}
function VCHIInitialise(instance: PPVCHI_INSTANCE_T): int32_t; cdecl; external libvchiq_arm name 'vchi_initialise'; 
function VCHIConnect(connections: PPVCHI_CONNECTION_T; num_connections: uint32_t; instance: PVCHI_INSTANCE_T): int32_t; cdecl; external libvchiq_arm name 'vchi_connect'; 
function VCHICreateConnection(function_table: PVCHI_CONNECTION_API_T; low_level: PVCHI_MESSAGE_DRIVER_T): PVCHI_CONNECTION_T; cdecl; external libvchiq_arm name 'vchi_create_connection'; 

function VCHISingleGetFuncTable: PVCHI_CONNECTION_API_T; cdecl; external libvchiq_arm name 'single_get_func_table'; 
function VCHIMPHIMessageDriverFuncTable: PVCHI_MESSAGE_DRIVER_T; cdecl; external libvchiq_arm name 'vchi_mphi_message_driver_func_table'; 

//To Do //More

{==============================================================================}
{VC4 VCHIQ_ARM Interface Functions (From \opt\vc\include\interface\vchiq_arm\*)}
{Note: These import the C functions from the Userland libraries, not the functions from the VC4VCHIQ driver unit}
function VCHIQInitialise(Instance: PPVCHIQ_INSTANCE_T): VCHIQ_STATUS_T; cdecl; external libvchiq_arm name 'vchiq_initialise'; 

//To Do //More

{==============================================================================}
{VC4 VCSM Interface Functions (From \opt\vc\include\interface\vcsm\user-vcsm.h)}
{Note: These import the C functions from the Userland libraries, not the functions from the VC4VCSM driver unit}
procedure VCSMInit; cdecl; external libvcsm name 'vcsm_init';
procedure VCSMExit; cdecl; external libvcsm name 'vcsm_exit';

//To Do
//void vcsm_status( VCSM_STATUS_T status, int pid );
//unsigned int vcsm_malloc( unsigned int size, char *name );
//unsigned int vcsm_malloc_cache( unsigned int size, VCSM_CACHE_TYPE_T cache, char *name );
//unsigned int vcsm_malloc_share( unsigned int handle );
//int vcsm_resize( unsigned int handle, unsigned int new_size );
//void vcsm_free( unsigned int handle );
//unsigned int vcsm_vc_hdl_from_ptr( void *usr_ptr );
//unsigned int vcsm_vc_hdl_from_hdl( unsigned int handle );
//unsigned int vcsm_usr_handle( void *usr_ptr );
//void *vcsm_usr_address( unsigned int handle );
//void *vcsm_lock( unsigned int handle );
//void *vcsm_lock_cache( unsigned int handle, VCSM_CACHE_TYPE_T cache_update, VCSM_CACHE_TYPE_T *cache_result );
//int vcsm_unlock_ptr( void *usr_ptr );
//int vcsm_unlock_ptr_sp( void *usr_ptr, int cache_no_flush );
//int vcsm_unlock_hdl( unsigned int handle );
//int vcsm_unlock_hdl_sp( unsigned int handle, int cache_no_flush );
//int vcsm_clean_invalid( struct vcsm_user_clean_invalid_s *s );
//Plus extras //vcsm_clean_invalid2 etc

{==============================================================================}
{VC4 BCMHost Interface Functions (From \opt\vc\include\bcm_host.h)}
procedure BCMHostInit; cdecl; external libbcm_host name 'bcm_host_init';
procedure BCMHostDeinit; cdecl; external libbcm_host name 'bcm_host_deinit';

function BCMHostGetPeripheralAddress: cunsigned; cdecl; external libbcm_host name 'bcm_host_get_peripheral_address';
function BCMHostGetPeripheralSize: cunsigned; cdecl; external libbcm_host name 'bcm_host_get_peripheral_size';
function BCMHostGetSdramAddress: cunsigned; cdecl; external libbcm_host name 'bcm_host_get_sdram_address';

function BCMHostGraphicsGetDisplaySize(display_number: uint16_t; var width,height: uint32_t): int32_t; cdecl; external libbcm_host name 'graphics_get_display_size';

{==============================================================================}
{VC4 MMAL Interface Functions (From \opt\vc\include\interface\mmal\*)}
//To Do //Move to separate MMAL unit

{==============================================================================}
{VC4 MMAL Component Registration Functions (MMAL_CONSTRUCTOR)}
{Note: MMAL Components are registered during startup using a constructor, these functions force the import of the appropriate static library}
procedure MMALIncludeComponentAggregator; cdecl; external libmmal_components name 'mmal_include_component_aggregator';
procedure MMALIncludeComponentArtificialCamera; cdecl; external libmmal_components name 'mmal_include_component_artificial_camera';
procedure MMALIncludeComponentAVCodecAudio; cdecl; external libmmal_components name 'mmal_include_component_avcodec_audio';
procedure MMALIncludeComponentClock; cdecl; external libmmal_components name 'mmal_include_component_clock';
procedure MMALIncludeComponentContainerReader; cdecl; external libmmal_components name 'mmal_include_component_container_reader';
procedure MMALIncludeComponentContainerWriter; cdecl; external libmmal_components name 'mmal_include_component_container_writer';
procedure MMALIncludeComponentCopy; cdecl; external libmmal_components name 'mmal_include_component_copy';
procedure MMALIncludeComponentNullSink; cdecl; external libmmal_components name 'mmal_include_component_null_sink';
procedure MMALIncludeComponentPassthrough; cdecl; external libmmal_components name 'mmal_include_component_passthrough';
procedure MMALIncludeComponentScheduler; cdecl; external libmmal_components name 'mmal_include_component_scheduler';
procedure MMALIncludeComponentSDLAudio; cdecl; external libmmal_components name 'mmal_include_component_sdl_audio';
procedure MMALIncludeComponentSDL; cdecl; external libmmal_components name 'mmal_include_component_sdl';
procedure MMALIncludeComponentSPDIF; cdecl; external libmmal_components name 'mmal_include_component_spdif';
procedure MMALIncludeComponentSplitter; cdecl; external libmmal_components name 'mmal_include_component_splitter';
procedure MMALIncludeComponentVideocore; cdecl; external libmmal_vc_client name 'mmal_include_component_videocore';

{==============================================================================}
{VC4 OpenMAX IL Interface Functions (From \opt\vc\include\IL\*)}
//To Do //Move to separate OpenMAX IL unit

{==============================================================================}
{VC4 Container Interface Functions}
//To Do //Part of MMAL
//See: Userland\containers\core\containers.c

{==============================================================================}
{VC4 Container and Packetizer Registration Functions (VC_PACKETIZER_REGISTER / VC_CONTAINER_CONSTRUCTOR)}
{Note: Containers and Packetizers are registered during startup using a constructor, these functions force the import of the appropriate static library}
procedure VC4IncludePacketizerAVC1; cdecl; external libcontainers name 'avc1_packetizer_include';
procedure VC4IncludePacketizerMPGA; cdecl; external libcontainers name 'mpga_packetizer_include';
procedure VC4IncludePacketizerMPGV; cdecl; external libcontainers name 'mpgv_packetizer_include';
procedure VC4IncludePacketizerPCM; cdecl; external libcontainers name 'pcm_packetizer_include';

{==============================================================================}
{VC4 Helper Functions}
function vc4_mbox_open: int; cdecl; public name 'vc4_mbox_open';
procedure vc4_mbox_close(file_desc: int); cdecl; public name 'vc4_mbox_close';

function vc4_mem_alloc(file_desc: int; size, align, flags: cunsigned): cunsigned; cdecl; public name 'vc4_mem_alloc';
function vc4_mem_free(file_desc: int; handle: cunsigned): cunsigned; cdecl; public name 'vc4_mem_free';
function vc4_mem_lock(file_desc: int; handle: cunsigned): cunsigned; cdecl; public name 'vc4_mem_lock';
function vc4_mem_unlock(file_desc: int; handle: cunsigned): cunsigned; cdecl; public name 'vc4_mem_unlock';

function vc4_mapmem(base, size: cunsigned): Pointer; cdecl; public name 'vc4_mapmem';
procedure vc4_unmapmem(addr: Pointer; size: cunsigned); cdecl; public name 'vc4_unmapmem';

function vc4_execute_code(file_desc: int; code, r0, r1, r2, r3, r4, r5: cunsigned): cunsigned; cdecl; public name 'vc4_execute_code';
function vc4_execute_qpu(file_desc: int; num_qpus, control, noflush, timeout: cunsigned): cunsigned; cdecl; public name 'vc4_execute_qpu';
function vc4_qpu_enable(file_desc: int; enable: cunsigned): cunsigned; cdecl; public name 'vc4_qpu_enable';
        
//procedure vc4_clean_cache_range(); cdecl; public name 'vc4_clean_cache_range'; //To do
//procedure vc4_invalidate_cache_range(); cdecl; public name 'vc4_invalidate_cache_range'; //To do
//procedure vc4_clean_invalidate_cache_range(); cdecl; public name 'vc4_invalidate_cache_range'; //To do
        
{==============================================================================}
{VC4 VCOS Helper Functions}
function vc4_vcos_get_ticks_per_second: uint32_t; cdecl; public name 'vc4_vcos_get_ticks_per_second';

{==============================================================================}
{VC4 VCHOSTIF Helper Functions}
function VCFilesysStart:LongWord;
function VCFilesysStop:LongWord;

procedure VCFilesysAsyncStart(Data:Pointer);

{==============================================================================}
{VC4 VCHIQ_ARM Helper Functions}
function vc4_vchiq_open: int; cdecl; public name 'vc4_vchiq_open';
function vc4_vchiq_dup(device: int): int; cdecl; public name 'vc4_vchiq_dup';
function vc4_vchiq_close(device: int): int; cdecl; public name 'vc4_vchiq_close';

function vc4_vchiq_ioctl(device, ioctl_code, argument: int): int; cdecl; public name 'vc4_vchiq_ioctl';

{==============================================================================}
{VC4 VCSM Helper Functions}
function vc4_vcsm_open: int; cdecl; public name 'vc4_vcsm_open';
function vc4_vcsm_close(device: int): int; cdecl; public name 'vc4_vcsm_close';

function vc4_vcsm_ioctl(device, ioctl_code, argument: int): int; cdecl; public name 'vc4_vcsm_ioctl';

{==============================================================================}
{VC4 BCMHost Helper Functions}
function vc4_bcm_host_get_peripheral_address: cunsigned; cdecl; public name 'vc4_bcm_host_get_peripheral_address';
function vc4_bcm_host_get_peripheral_size: cunsigned; cdecl; public name 'vc4_bcm_host_get_peripheral_size';
function vc4_bcm_host_get_sdram_address: cunsigned; cdecl; public name 'vc4_bcm_host_get_sdram_address';

{==============================================================================}
{==============================================================================}

implementation

uses {$IFDEF CPUARMV6}BCM2835{$ELSE}BCM2837{$ENDIF};

{==============================================================================}
{==============================================================================}
const
 {VC4 mailbox constants}
 {$IFDEF CPUARMV6}
 VC4_MBOX_REQUEST_CODE = BCM2835_MBOX_REQUEST_CODE;

 VC4_MBOX_TAG_END = BCM2835_MBOX_TAG_END;
 VC4_MBOX_TAG_EXECUTE_QPU = BCM2835_MBOX_TAG_EXECUTE_QPU;
 VC4_MBOX_TAG_ENABLE_QPU = BCM2835_MBOX_TAG_ENABLE_QPU;
 
 VC4_MBOX_TAG_VCHIQ_INIT = BCM2835_MBOX_TAG_VCHIQ_INIT;
 
 VC4_MAILBOX_0 = BCM2835_MAILBOX_0;
 VC4_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC = BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC;
 {$ELSE}
 VC4_MBOX_REQUEST_CODE = BCM2837_MBOX_REQUEST_CODE;

 VC4_MBOX_TAG_END = BCM2837_MBOX_TAG_END;
 VC4_MBOX_TAG_EXECUTE_QPU = BCM2837_MBOX_TAG_EXECUTE_QPU;
 VC4_MBOX_TAG_ENABLE_QPU = BCM2837_MBOX_TAG_ENABLE_QPU;
 
 VC4_MBOX_TAG_VCHIQ_INIT = BCM2837_MBOX_TAG_VCHIQ_INIT;
 
 VC4_MAILBOX_0 = BCM2837_MAILBOX_0;
 VC4_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC = BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC;
 {$ENDIF}
{==============================================================================}
{==============================================================================}
type
 {VC4 mailbox types}
 {$IFDEF CPUARMV6}
 TVC4MailboxHeader = TBCM2835MailboxHeader;
 PVC4MailboxHeader = PBCM2835MailboxHeader;
 
 TVC4MailboxFooter = TBCM2835MailboxFooter;
 PVC4MailboxFooter = PBCM2835MailboxFooter;
 
 TVC4MailboxTagHeader = TBCM2835MailboxTagHeader;
 PVC4MailboxTagHeader = PBCM2835MailboxTagHeader;
 
 TVC4MailboxTagExecuteQPU = TBCM2835MailboxTagExecuteQPU;
 PVC4MailboxTagExecuteQPU = PBCM2835MailboxTagExecuteQPU;
 
 TVC4MailboxTagEnableQPU = TBCM2835MailboxTagEnableQPU;
 PVC4MailboxTagEnableQPU = PBCM2835MailboxTagEnableQPU;
 {$ELSE}
 TVC4MailboxHeader = TBCM2837MailboxHeader;
 PVC4MailboxHeader = PBCM2837MailboxHeader;
 
 TVC4MailboxFooter = TBCM2837MailboxFooter;
 PVC4MailboxFooter = PBCM2837MailboxFooter;
 
 TVC4MailboxTagHeader = TBCM2837MailboxTagHeader;
 PVC4MailboxTagHeader = PBCM2837MailboxTagHeader;
 
 TVC4MailboxTagExecuteQPU = TBCM2837MailboxTagExecuteQPU;
 PVC4MailboxTagExecuteQPU = PBCM2837MailboxTagExecuteQPU;
 
 TVC4MailboxTagEnableQPU = TBCM2837MailboxTagEnableQPU;
 PVC4MailboxTagEnableQPU = PBCM2837MailboxTagEnableQPU;
 {$ENDIF}
{==============================================================================}
{==============================================================================}
var
 {VC4 specific variables}
 VC4Initialized:Boolean;
 
 VC4Lock:TMutexHandle = INVALID_HANDLE_VALUE;
 VC4StartupCount:LongWord;
 VC4StartupError:LongWord;
 
 VCFilesysInitialized:Boolean;
 VCFilesysInstance:PVCHI_INSTANCE_T;
 VCFilesysConnection:PVCHI_CONNECTION_T;
 
{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure VC4Init;
{Initialize the VC4 unit and devices}

{Note: Called only during system startup}
begin
 {}
 {Check Initialized}
 if VC4Initialized then Exit;
 
 {Create Lock}
 VC4Lock:=MutexCreateEx(False,MUTEX_DEFAULT_SPINCOUNT,MUTEX_FLAG_RECURSIVE);
 if VC4Lock = INVALID_HANDLE_VALUE then
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'VC4: Failed to create lock');
  end;

 {Set Startup Defaults}
 VC4StartupCount:=0;
 VC4StartupError:=ERROR_NOT_READY;
 
 VC4Initialized:=True;
end;

{==============================================================================}

function VC4Start:LongWord;
{Start the VC4 VCHIQ and VCSM devices}
{Return: ERROR_SUCCESS if completed successfully or another error code on failure}

{Note: May be called multiple times, each call to VC4Start must have a matching call to VC4Stop}
var
 //VCSM:PVCSMDevice; //To Do 
 VCHIQ:PVCHIQDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Acquire the Lock}
 if MutexLock(VC4Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Count}
    if VC4StartupCount > 0 then
     begin
      {$IFDEF VC4_DEBUG}
      if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VC4: Additional start call');
      {$ENDIF}
 
      {Increment Count}
      Inc(VC4StartupCount);
 
      {Return Result}
      Result:=VC4StartupError; 
     end
    else
     begin
      {$IFDEF VC4_DEBUG}
      if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VC4: Initial start call');
      {$ENDIF}
 
      {Increment Count}
      Inc(VC4StartupCount);
 
      {Initialize Components}
      Result:=ERROR_OPERATION_FAILED;
      VC4StartupError:=ERROR_OPERATION_FAILED;
 
      {Find VCHIQ}
      VCHIQ:=PVCHIQDevice(DeviceFindByDescription(VCHIQ_DESCRIPTION));
      if VCHIQ = nil then Exit;
      
      {Start VCHIQ}
      if VCHIQDeviceStart(VCHIQ) <> ERROR_SUCCESS then Exit;
      
      {Find VCSM}
      //VCSM:=PVCSMDevice(DeviceFindByDescription(VCSM_DESCRIPTION)); //To Do 
      //if VCSM = nil then Exit;
      
      {Start VCSM}
      //if VCSMDeviceStart(VCSM) <> ERROR_SUCCESS then Exit; //To Do 
      
      {Start VCFilesys (Using a worker thread)}
      WorkerSchedule(500,TWorkerTask(VCFilesysAsyncStart),nil,nil); 
      
      {$IFDEF VC4_DEBUG}
      if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VC4: Start completed');
      {$ENDIF}
 
      {Return Result} 
      Result:=ERROR_SUCCESS;
      VC4StartupError:=ERROR_SUCCESS;
     end;
   finally
    {Release the Lock}
    MutexUnlock(VC4Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function VC4Stop:LongWord;
{Stop the VC4 VCHIQ and VCSM devices}
{Return: ERROR_SUCCESS if completed successfully or another error code on failure}

{Note: May be called multiple times, each call to VC4Stop must have a matching call to VC4Start}
var
 //VCSM:PVCSMDevice; //To Do 
 VCHIQ:PVCHIQDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Acquire the Lock}
 if MutexLock(VC4Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Started}
    Result:=ERROR_OPERATION_FAILED;
    if VC4StartupCount = 0 then Exit;
    
    {Decrement Count}
    Dec(VC4StartupCount);
    Result:=ERROR_SUCCESS;
    if VC4StartupCount > 0 then Exit;
    
    {$IFDEF VC4_DEBUG}
    if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VC4: Final stop call');
    {$ENDIF}
    
    {Shutdown and Cleanup}
    Result:=ERROR_OPERATION_FAILED;
 
    {Stop VCFilesys}
    if VCFilesysStop <> ERROR_SUCCESS then Exit;
 
    {Find VCHIQ}
    VCHIQ:=PVCHIQDevice(DeviceFindByDescription(VCHIQ_DESCRIPTION));
    if VCHIQ = nil then Exit;
 
    {Stop VCHIQ}
    if VCHIQDeviceStop(VCHIQ) <> ERROR_SUCCESS then Exit;
      
    {Find VCSM}
    //VCSM:=PVCSMDevice(DeviceFindByDescription(VCSM_DESCRIPTION)); //To Do 
    //if VCSM = nil then Exit;
      
    {Stop VCSM}
    //if VCSMDeviceStop(VCSM) <> ERROR_SUCCESS then Exit; //To Do 
    
    {Return Result} 
    Result:=ERROR_SUCCESS;
    VC4StartupError:=ERROR_NOT_READY;
   finally
    {Release the Lock}
    MutexUnlock(VC4Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}
{==============================================================================}
{VC4 Mailbox Functions}
function VC4MemoryAllocate(Length,Alignment,Flags:LongWord):THandle; inline;
{Allocate memory from the VC4 GPU}
begin
 {}
 if Assigned(GPUMemoryAllocateHandler) then
  begin
   Result:=GPUMemoryAllocateHandler(Length,Alignment,Flags);
  end
 else
  begin
   Result:=INVALID_HANDLE_VALUE;
  end;
end;

{==============================================================================}

function VC4MemoryRelease(Handle:THandle):LongWord; inline;
{Release memory allocated from the VC4 GPU}
begin
 {}
 if Assigned(GPUMemoryReleaseHandler) then
  begin
   Result:=GPUMemoryReleaseHandler(Handle);
  end
 else
  begin
   Result:=ERROR_CALL_NOT_IMPLEMENTED;
  end;
end;

{==============================================================================}

function VC4MemoryLock(Handle:THandle):LongWord; inline;
{Lock memory allocated from the VC4 GPU and return an address}
begin
 {}
 if Assigned(GPUMemoryLockHandler) then
  begin
   Result:=GPUMemoryLockHandler(Handle);
  end
 else
  begin
   Result:=0;
  end;
end;

{==============================================================================}

function VC4MemoryUnlock(Handle:THandle):LongWord; inline;
{Unlock memory allocated from the VC4 GPU}
begin
 {}
 if Assigned(GPUMemoryUnlockHandler) then
  begin
   Result:=GPUMemoryUnlockHandler(Handle);
  end
 else
  begin
   Result:=ERROR_CALL_NOT_IMPLEMENTED;
  end;
end;

{==============================================================================}

function VC4ExecuteCode(Address:Pointer;R0,R1,R2,R3,R4,R5:LongWord):LongWord; inline;
{Execute a block of code on the VC4 GPU}
begin
 {}
 if Assigned(GPUExecuteCodeHandler) then
  begin
   Result:=GPUExecuteCodeHandler(Address,R0,R1,R2,R3,R4,R5);
  end
 else
  begin
   Result:=0;
  end;
end;

{==============================================================================}

function VC4QPUEnable(Enable:LongWord):LongWord; inline;
{Enable QPUs using the Mailbox property tags channel}
begin
 {}
 Result:=V3DQPUEnable(Enable);
end;

{==============================================================================}

function VC4QPUExecute(NumQPUs,Control,NoFlush,Timeout:LongWord):LongWord; inline;
{Execute QPU code using the Mailbox property tags channel}
begin
 {}
 Result:=V3DQPUExecute(NumQPUs,Control,NoFlush,Timeout);
end;

{==============================================================================}

function VC4VCHIQEnable(Address:LongWord):LongWord; inline;
{Enable the VCHIQ (Master) using the Mailbox property tags channel}
begin
 {}
 Result:=VCHIQEnable(Address);
end;

{==============================================================================}
{==============================================================================}
{VC4 VCOS Interface Functions}

{==============================================================================}
{==============================================================================}
{VC4 VCHOSTIF Interface Functions}

{==============================================================================}
{==============================================================================}
{VC4 VCHIQ_ARM Interface Functions}

{==============================================================================}
{==============================================================================}
{VC4 VCSM Interface Functions}

{==============================================================================}
{==============================================================================}
{VC4 BCMHost Interface Functions}

{==============================================================================}
{==============================================================================}
{VC4 MMAL Interface Functions}

{==============================================================================}
{==============================================================================}
{VC4 Container Interface Functions}

{==============================================================================}
{==============================================================================}
{VC4 Helper Functions}
function vc4_mbox_open: int; cdecl;
{Open the VC4 mailbox (Dummy only)}

{Note: Exported function for use by C libraries, not intended to be called by applications}
var
 ptr:P_reent;
 Entry:PHandleEntry;
begin
 {Dummy only}
 Result:=-1;
 
 {$IFDEF VC4_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VC4: vc4_mbox_open');
 {$ENDIF}
 
 {Create Handle}
 Entry:=HandleCreateEx('',HANDLE_FLAG_NONE,THandle(-1),HANDLE_TYPE_DEVICE);
 if Entry = nil then 
  begin
   {Return Error}
   ptr:=__getreent;
   if ptr <> nil then ptr^._errno:=EINVAL;
   
   Exit;
  end;
 
 {Return Handle}
 Result:=Entry^.Handle;
 
 {$IFDEF VC4_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VC4: vc4_mbox_open (Result=' + IntToHex(Result,8) + ')');
 {$ENDIF}
end;

{==============================================================================}

procedure vc4_mbox_close(file_desc: int); cdecl;
{Close the VC4 mailbox (Dummy only)}

{Note: Exported function for use by C libraries, not intended to be called by applications}
var
 ptr:P_reent;
 Entry:PHandleEntry;
 ResultCode:LongWord;
begin
 {Dummy only}
 {$IFDEF VC4_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VC4: vc4_mbox_close (file_desc=' + IntToStr(file_desc) + ')');
 {$ENDIF}
 
 {Get Handle}
 Entry:=HandleGet(file_desc);
 if Entry = nil then
  begin
   {Return Error}
   ptr:=__getreent;
   if ptr <> nil then ptr^._errno:=EINVAL;
   
   Exit;
  end;
 
 {Destroy Handle}
 ResultCode:=HandleDestroy(file_desc);
 if (ResultCode <> ERROR_SUCCESS) and (ResultCode <> ERROR_IN_USE) then
  begin
   {Return Error}
   ptr:=__getreent;
   if ptr <> nil then ptr^._errno:=EINVAL;
  end;
end;

{==============================================================================}

function vc4_mem_alloc(file_desc: int; size, align, flags: cunsigned): cunsigned; cdecl;
{Allocate memory from the VC4 GPU}

{Note: Exported function for use by C libraries, not intended to be called by applications}
begin
 {}
 {$IFDEF VC4_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VC4: vc4_mem_alloc (file_desc=' + IntToStr(file_desc) + ' size=' + IntToStr(size) + ' align=' + IntToStr(align) + ' flags=' + IntToHex(flags,8) + ')');
 {$ENDIF}
 
 if Assigned(GPUMemoryAllocateHandler) then
  begin
   Result:=GPUMemoryAllocateHandler(size,align,flags);
  end
 else
  begin
   Result:=LongWord(INVALID_HANDLE_VALUE);
  end;
 
 {$IFDEF VC4_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VC4: vc4_mem_alloc (Result=' + IntToHex(Result,8) + ')');
 {$ENDIF}
end;

{==============================================================================}

function vc4_mem_free(file_desc: int; handle: cunsigned): cunsigned; cdecl;
{Release memory allocated from the VC4 GPU}

{Note: Exported function for use by C libraries, not intended to be called by applications}
begin
 {}
 {$IFDEF VC4_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VC4: vc4_mem_free (file_desc=' + IntToStr(file_desc) + ' handle=' + IntToHex(handle,8) + ')');
 {$ENDIF}
 
 if Assigned(GPUMemoryReleaseHandler) then
  begin
   Result:=GPUMemoryReleaseHandler(handle);
  end
 else
  begin
   Result:=ERROR_CALL_NOT_IMPLEMENTED;
  end;
 
 {$IFDEF VC4_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VC4: vc4_mem_free (Result=' + IntToHex(Result,8) + ')');
 {$ENDIF}
end;

{==============================================================================}

function vc4_mem_lock(file_desc: int; handle: cunsigned): cunsigned; cdecl;
{Lock memory allocated from the VC4 GPU and return an address}

{Note: Exported function for use by C libraries, not intended to be called by applications}
begin
 {}
 {$IFDEF VC4_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VC4: vc4_mem_lock (file_desc=' + IntToStr(file_desc) + ' handle=' + IntToHex(handle,8) + ')');
 {$ENDIF}
 
 if Assigned(GPUMemoryLockHandler) then
  begin
   Result:=GPUMemoryLockHandler(handle);
  end
 else
  begin
   Result:=0;
  end;
 
 {$IFDEF VC4_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VC4: vc4_mem_lock (Result=' + IntToHex(Result,8) + ')');
 {$ENDIF}
end;

{==============================================================================}

function vc4_mem_unlock(file_desc: int; handle: cunsigned): cunsigned; cdecl;
{Unlock memory allocated from the VC4 GPU}

{Note: Exported function for use by C libraries, not intended to be called by applications}
begin
 {}
 {$IFDEF VC4_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VC4: vc4_mem_unlock (file_desc=' + IntToStr(file_desc) + ' handle=' + IntToHex(handle,8) + ')');
 {$ENDIF}
 
 if Assigned(GPUMemoryUnlockHandler) then
  begin
   Result:=GPUMemoryUnlockHandler(handle);
  end
 else
  begin
   Result:=ERROR_CALL_NOT_IMPLEMENTED;
  end;
 
 {$IFDEF VC4_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VC4: vc4_mem_unlock (Result=' + IntToHex(Result,8) + ')');
 {$ENDIF}
end;

{==============================================================================}

function vc4_mapmem(base, size: cunsigned): Pointer; cdecl;
{Map a block of memory into the current address space (Dummy only)}

{Note: Exported function for use by C libraries, not intended to be called by applications}
begin
 {Dummy only}
 {$IFDEF VC4_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VC4: vc4_mapmem (base=' + IntToHex(base,8) + ' size=' + IntToStr(size) + ')');
 {$ENDIF}
 
 Result:=Pointer(base);
 
 {$IFDEF VC4_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VC4: vc4_mapmem (Result=' + IntToHex(PtrUInt(Result),8) + ')');
 {$ENDIF}
end;

{==============================================================================}

procedure vc4_unmapmem(addr: Pointer; size: cunsigned); cdecl;
{Unmap a block of memory from the current address space (Dummy only)}

{Note: Exported function for use by C libraries, not intended to be called by applications}
begin
 {Dummy only}
 {$IFDEF VC4_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VC4: vc4_unmapmem (addr=' + IntToHex(PtrUInt(addr),8) + ' size=' + IntToStr(size) + ')');
 {$ENDIF}
end;

{==============================================================================}

function vc4_execute_code(file_desc: int; code, r0, r1, r2, r3, r4, r5: cunsigned): cunsigned; cdecl;
{Execute a block of code on the VC4 GPU}

{Note: Exported function for use by C libraries, not intended to be called by applications}
begin
 {}
 {$IFDEF VC4_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VC4: vc4_execute_code (file_desc=' + IntToStr(file_desc) + ' code=' + IntToHex(code,8) + ' r0=' + IntToStr(r0) + ' r1=' + IntToStr(r1) + ' r2=' + IntToStr(r2) + ' r3=' + IntToStr(r3) + ' r4=' + IntToStr(r4) + ' r5=' + IntToStr(r5) + ')');
 {$ENDIF}
 
 if Assigned(GPUExecuteCodeHandler) then
  begin
   Result:=GPUExecuteCodeHandler(Pointer(code),r0,r1,r2,r3,r4,r5);
  end
 else
  begin
   Result:=0;
  end;
 
 {$IFDEF VC4_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VC4: vc4_execute_code (Result=' + IntToHex(Result,8) + ')');
 {$ENDIF}
end;

{==============================================================================}

function vc4_execute_qpu(file_desc: int; num_qpus, control, noflush, timeout: cunsigned): cunsigned; cdecl;
{Execute QPU code using the Mailbox property tags channel}

{Note: Exported function for use by C libraries, not intended to be called by applications}
begin
 {}
 {$IFDEF VC4_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VC4: vc4_execute_qpu (file_desc=' + IntToStr(file_desc) + ' num_qpus=' + IntToStr(num_qpus) + ' control=' + IntToStr(control) + ' noflush=' + IntToStr(noflush) + ' timeout=' + IntToStr(timeout) + ')');
 {$ENDIF}
 
 Result:=V3DQPUExecute(num_qpus,control,noflush,timeout);
 
 {$IFDEF VC4_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VC4: vc4_execute_qpu (Result=' + IntToHex(Result,8) + ')');
 {$ENDIF}
end;

{==============================================================================}

function vc4_qpu_enable(file_desc: int; enable: cunsigned): cunsigned; cdecl;
{Enable QPUs using the Mailbox property tags channel}

{Note: Exported function for use by C libraries, not intended to be called by applications}
begin
 {}
 {$IFDEF VC4_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VC4: vc4_qpu_enable (file_desc=' + IntToStr(file_desc) + ' enable=' + IntToStr(enable) + ')');
 {$ENDIF}
 
 Result:=V3DQPUEnable(enable);
  
 {$IFDEF VC4_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VC4: vc4_qpu_enable (Result=' + IntToHex(Result,8) + ')');
 {$ENDIF}
end;
        
{==============================================================================}
{==============================================================================}
{VC4 VCOS Helper Functions}
function vc4_vcos_get_ticks_per_second: uint32_t; cdecl;
{Get the number of scheduler ticks per second for the current platform}

{Note: Exported function for use by C libraries, not intended to be called by applications}
begin
 {}
 Result:=SCHEDULER_INTERRUPTS_PER_SECOND;
 
 {$IFDEF VC4_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VC4: vc4_vcos_get_ticks_per_second (Result=' + IntToStr(Result) + ')');
 {$ENDIF}
end;

{==============================================================================}
{==============================================================================}
{VC4 VCHOSTIF Helper Functions}
function VCFilesysStart:LongWord;
{Start the VC File Service}
var
 Status:VCHIQ_STATUS_T;
 Success:Integer;
 VCHIQInstance:PVCHIQ_INSTANCE_T;
begin
 {}
 Result:=ERROR_OPERATION_FAILED;

 {$IFDEF VC4_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VC4: VCFilesysStart');
 {$ENDIF}
 
 if VC4_FILESYS_START then
  begin
   if VCFilesysInitialized then Exit;
   
   {Initialize VCOS}
   VCOSInit;
   
   {Initialize VCHIQ}
   Status:=VCHIQInitialise(@VCHIQInstance);
   if Status <> VCHIQ_SUCCESS then Exit;
   
   {Initialize VCHI}
   Success:=VCHIInitialise(@VCFilesysInstance);
   if Success <> 0 then Exit;
   
   VCHIQInstance:=PVCHIQ_INSTANCE_T(VCFilesysInstance);
   
   {Create VCHI Connection}
   VCFilesysConnection:=VCHICreateConnection(VCHISingleGetFuncTable,VCHIMPHIMessageDriverFuncTable);
   
   {Connect VCHI}
   Success:=VCHIConnect(@VCFilesysConnection,1,VCFilesysInstance);
   
   {Initialize File Service}
   Success:=VCVCHIFilesysInit(VCFilesysInstance,@VCFilesysConnection,1);
  
   VCFilesysInitialized:=True;
  end; 
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function VCFilesysStop:LongWord;
{Stop the VC File Service}
begin
 {}
 Result:=ERROR_OPERATION_FAILED;

 {$IFDEF VC4_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VC4: VCFilesysStop');
 {$ENDIF}
 
 if VC4_FILESYS_START then
  begin
   if not VCFilesysInitialized then Exit;
 
   {Deinitialize File Service}
   VCFilesysDeinit;
 
   {Deinitialize VCOS}
   VCOSDeinit;
 
   VCFilesysInitialized:=False;
  end;  
 
 Result:=ERROR_SUCCESS;
end;
 
{==============================================================================}
 
procedure VCFilesysAsyncStart(Data:Pointer);
{Asynchronously start the VC File Service (Using a worker thread)}
begin
 {}
 {Wait for Ready}
 while not(SysInitCompleted) do
  begin
   ThreadSleep(0);
  end;
 
 {Start VC File Service}
 VCFilesysStart;
end;
 
{==============================================================================}
{==============================================================================}
{VC4 VCHIQ_ARM Helper Functions}
function vc4_vchiq_open: int; cdecl;
{Open a handle to the VCHIQ device}

{Note: Exported function for use by C libraries, not intended to be called by applications}
var
 ptr:P_reent;
 Status:LongWord;
 VCHIQ:PVCHIQDevice;
 Entry:PHandleEntry;
 Instance:PVCHIQInstance;
begin
 {}
 Result:=-1;
 
 {$IFDEF VC4_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VC4: vc4_vchiq_open');
 {$ENDIF}
 
 {Start VC4}
 Status:=VC4Start;
 if Status <> ERROR_SUCCESS then
  begin
   {Return Error}
   ptr:=__getreent;
   if ptr <> nil then ptr^._errno:=EINVAL;
   
   Exit;
  end;
 
 {Find VCHIQ}
 VCHIQ:=PVCHIQDevice(DeviceFindByDescription(VCHIQ_DESCRIPTION));
 if VCHIQ = nil then
  begin
   {Return Error}
   ptr:=__getreent;
   if ptr <> nil then ptr^._errno:=EINVAL;
   
   Exit;
  end; 
 
 {Open VCHIQ}
 Instance:=VCHIQDeviceOpen(VCHIQ);
 if Instance = nil then 
  begin
   {Return Error}
   ptr:=__getreent;
   if ptr <> nil then ptr^._errno:=EINVAL;
   
   Exit;
  end; 
  
 {Create Handle}
 Entry:=HandleCreateEx('',HANDLE_FLAG_DUPLICATE,THandle(Instance),HANDLE_TYPE_DEVICE);
 if Entry = nil then 
  begin
   {Return Error}
   ptr:=__getreent;
   if ptr <> nil then ptr^._errno:=EINVAL;
   
   Exit;
  end;
 
 {Return Handle}
 Result:=Entry^.Handle;
 
 {$IFDEF VC4_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VC4: vc4_vchiq_open (Result=' + IntToHex(Result,8) + ')');
 {$ENDIF}
end;

{==============================================================================}

function vc4_vchiq_dup(device: int): int; cdecl;
{Duplicate a VCHIQ device handle}

{Note: Exported function for use by C libraries, not intended to be called by applications}
var
 ptr:P_reent;
 Handle:THandle;
 Status:LongWord;
 Entry:PHandleEntry;
begin
 {}
 Result:=-1;
 
 {$IFDEF VC4_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VC4: vc4_vchiq_dup (device=' + IntToHex(device,8) + ')');
 {$ENDIF}

 {Get Handle}
 Entry:=HandleGet(device);
 if Entry = nil then
  begin
   {Return Error}
   ptr:=__getreent;
   if ptr <> nil then ptr^._errno:=EINVAL;
   
   Exit;
  end;
 
 {Start VC4}
 Status:=VC4Start;
 if Status <> ERROR_SUCCESS then
  begin
   {Return Error}
   ptr:=__getreent;
   if ptr <> nil then ptr^._errno:=EINVAL;
   
   Exit;
  end;
 
 //To Do //Instance //Open etc ?
 
 {Duplicate Handle}
 Handle:=HandleDuplicate(Entry^.Handle);
 if Handle = INVALID_HANDLE_VALUE then 
  begin
   {Return Error}
   ptr:=__getreent;
   if ptr <> nil then ptr^._errno:=EINVAL;
   
   Exit;
  end;
  
 {Return Result}
 Result:=Handle; 
end;

{==============================================================================}

function vc4_vchiq_close(device: int): int; cdecl;
{Close a handle to the VCHIQ device}

{Note: Exported function for use by C libraries, not intended to be called by applications}
var
 ptr:P_reent;
 Status:LongWord;
 Entry:PHandleEntry;
 ResultCode:LongWord;
 Instance:PVCHIQInstance;
begin
 {}
 Result:=-1;
 
 {$IFDEF VC4_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VC4: vc4_vchiq_close (device=' + IntToHex(device,8) + ')');
 {$ENDIF}

 {Get Handle}
 Entry:=HandleGet(device);
 if Entry = nil then
  begin
   {Return Error}
   ptr:=__getreent;
   if ptr <> nil then ptr^._errno:=EINVAL;
   
   Exit;
  end;
 
 {Get Instance}
 Instance:=PVCHIQInstance(Entry^.Data);
 if Instance = nil then
  begin
   {Return Error}
   ptr:=__getreent;
   if ptr <> nil then ptr^._errno:=EINVAL;
   
   Exit;
  end;
 
 {Close VCHIQ}
 Status:=VCHIQDeviceClose(Instance.VCHIQ,Instance);
 if Status <> ERROR_SUCCESS then
  begin
   {Return Error}
   ptr:=__getreent;
   if ptr <> nil then ptr^._errno:=EINVAL;
   
   Exit;
  end;
 
 {Stop VC4}
 Status:=VC4Stop;
 if Status <> ERROR_SUCCESS then
  begin
   {Return Error}
   ptr:=__getreent;
   if ptr <> nil then ptr^._errno:=EINVAL;
   
   Exit;
  end;
 
 {Destroy Handle}
 ResultCode:=HandleDestroy(device);
 if (ResultCode = ERROR_SUCCESS) or (ResultCode = ERROR_IN_USE) then
  begin
   {Return Result}
   Result:=0;
  end
 else
  begin
   {Return Error}
   ptr:=__getreent;
   if ptr <> nil then ptr^._errno:=EINVAL;
  end;
end;

{==============================================================================}

function vc4_vchiq_ioctl(device, ioctl_code, argument: int): int; cdecl;
{Send an IO Control request to the VCHIQ device}

{Note: Exported function for use by C libraries, not intended to be called by applications}
var
 ptr:P_reent;
 Status:LongWord;
 Response:PtrUInt;
 Entry:PHandleEntry;
 Instance:PVCHIQInstance;
begin
 {}
 Result:=-1;
 
 {$IFDEF VC4_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VC4: vc4_vchiq_ioctl (device=' + IntToHex(device,8) + ' ioctl_code=' + IntToHex(ioctl_code,8) + ' argument=' + IntToHex(argument,8) + ')');
 {$ENDIF}

 {Get Handle}
 Entry:=HandleGet(device);
 if Entry = nil then
  begin
   {Return Error}
   ptr:=__getreent;
   if ptr <> nil then ptr^._errno:=EINVAL;
   
   Exit;
  end;
  
 {Get Instance}
 Instance:=PVCHIQInstance(Entry^.Data);
 if Instance = nil then
  begin
   {Return Error}
   ptr:=__getreent;
   if ptr <> nil then ptr^._errno:=EINVAL;
   
   Exit;
  end;
 
 {Control VCHIQ}
 Status:=VCHIQDeviceControl(Instance.VCHIQ,Instance,ioctl_code,argument,Response);
 if Status <> ERROR_SUCCESS then
  begin
   {Return Error}
   ptr:=__getreent;
   if ptr <> nil then
    begin
     {Check Status}
     case Status of
      ERROR_INVALID_PARAMETER:ptr^._errno:=EINVAL;
      ERROR_NOT_ENOUGH_MEMORY:ptr^._errno:=ENOMEM;
      ERROR_NOT_OPEN:ptr^._errno:=ENOTCONN;
      ERROR_ALREADY_EXISTS:ptr^._errno:=EEXIST;
      ERROR_IN_USE:ptr^._errno:=EWOULDBLOCK;
      ERROR_OPERATION_FAILED:ptr^._errno:=EIO;
      ERROR_INSUFFICIENT_BUFFER:ptr^._errno:=EMSGSIZE;
      ERROR_INVALID_DATA:ptr^._errno:=EFAULT;
      ERROR_NOT_FOUND:ptr^._errno:=ESRCH;
     else
      begin
       ptr^._errno:=EINVAL;
      end;
     end;
     
     {Return Error}
     Result:=-ptr^._errno;
    end;

   //To Do // EINTR ?
           // ENOTTY ?
   Exit;
  end;
 
 {Return Response}
 Result:=Response;
end;

{==============================================================================}
{==============================================================================}
{VC4 VCSM Helper Functions}
function vc4_vcsm_open: int; cdecl;
{Open a handle to the VCSM device}

{Note: Exported function for use by C libraries, not intended to be called by applications}
var
 ptr:P_reent;
 Status:LongWord;
 //VCSM:PVCSMDevice; //To Do 
 Entry:PHandleEntry;
begin
 {}
 Result:=-1;
 
 {$IFDEF VC4_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VC4: vc4_vcsm_open');
 {$ENDIF}
 
 {Start VC4}
 Status:=VC4Start;
 if Status <> ERROR_SUCCESS then
  begin
   {Return Error}
   ptr:=__getreent;
   if ptr <> nil then ptr^._errno:=EINVAL;
   
   Exit;
  end;
 
 {Find VCSM}
 //VCSM:=PVCSMDevice(DeviceFindByDescription(VCSM_DESCRIPTION)); //To Do 
 //if VCSM = nil then
 // begin
 //  {Return Error}
 //  ptr:=__getreent;
 //  if ptr <> nil then ptr^._errno:=EINVAL;
 //  
 //  Exit;
 // end; 
 
 {Create Handle}
 //Entry:=HandleCreateEx('',HANDLE_FLAG_DUPLICATE,THandle(VCSM),HANDLE_TYPE_DEVICE); //To Do 
 //if Entry = nil then 
 // begin
 //  {Return Error}
 //  ptr:=__getreent;
 //  if ptr <> nil then ptr^._errno:=EINVAL;
 //  
 //  Exit;
 // end;
 
 {Return Handle}
 //Result:=Entry^.Handle;
 
 {$IFDEF VC4_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VC4: vc4_vcsm_open (Result=' + IntToHex(Result,8) + ')');
 {$ENDIF}
end;

{==============================================================================}

function vc4_vcsm_close(device: int): int; cdecl;
{Close a handle to the VCSM device}

{Note: Exported function for use by C libraries, not intended to be called by applications}
var
 ptr:P_reent;
 Status:LongWord;
 Entry:PHandleEntry;
 ResultCode:LongWord;
begin
 {}
 Result:=-1;
 
 {$IFDEF VC4_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VC4: vc4_vcsm_close (device=' + IntToHex(device,8) + ')');
 {$ENDIF}

 {Get Handle}
 Entry:=HandleGet(device);
 if Entry = nil then
  begin
   {Return Error}
   ptr:=__getreent;
   if ptr <> nil then ptr^._errno:=EINVAL;
   
   Exit;
  end;
 
 {Stop VC4}
 Status:=VC4Stop;
 if Status <> ERROR_SUCCESS then
  begin
   {Return Error}
   ptr:=__getreent;
   if ptr <> nil then ptr^._errno:=EINVAL;
   
   Exit;
  end;
 
 {Destroy Handle}
 ResultCode:=HandleDestroy(device);
 if (ResultCode = ERROR_SUCCESS) or (ResultCode = ERROR_IN_USE) then
  begin
   {Return Result}
   Result:=0;
  end
 else
  begin
   {Return Error}
   ptr:=__getreent;
   if ptr <> nil then ptr^._errno:=EINVAL;
  end;
end;

{==============================================================================}

function vc4_vcsm_ioctl(device, ioctl_code, argument: int): int; cdecl;
{Send an IO Control request to the VCSM device}

{Note: Exported function for use by C libraries, not intended to be called by applications}
var
 ptr:P_reent;
 Status:LongWord;
 Response:LongWord;
 Entry:PHandleEntry;
begin
 {}
 Result:=-1;
 
 {$IFDEF VC4_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VC4: vc4_vcsm_ioctl (device=' + IntToHex(device,8) + ' ioctl_code=' + IntToHex(ioctl_code,8) + ' argument=' + IntToHex(argument,8) + ')');
 {$ENDIF}

 {Get Handle}
 Entry:=HandleGet(device);
 if Entry = nil then
  begin
   {Return Error}
   ptr:=__getreent;
   if ptr <> nil then ptr^._errno:=EINVAL;
   
   Exit;
  end;
 
 {Call Device Control}
 //Status:=VCSMDeviceControl(PVCSMDevice(Entry^.Data),ioctl_code,argument,Response); //To Do 
 //if Status <> ERROR_SUCCESS then
 // begin
 //  {Return Error}
 //  ptr:=__getreent;
 //  if ptr <> nil then
 //   begin
 //    {Check Status}
 //    case Status of
 //     ERROR_INVALID_PARAMETER:ptr^._errno:=EINVAL;
 //     ERROR_NOT_ENOUGH_MEMORY:ptr^._errno:=ENOMEM;
 //     ERROR_NOT_OPEN:ptr^._errno:=ENOTCONN;
 //     ERROR_ALREADY_EXISTS:ptr^._errno:=EEXIST;
 //     ERROR_IN_USE:ptr^._errno:=EWOULDBLOCK;
 //     ERROR_OPERATION_FAILED:ptr^._errno:=EIO;
 //     ERROR_INSUFFICIENT_BUFFER:ptr^._errno:=EMSGSIZE;
 //     ERROR_INVALID_DATA:ptr^._errno:=EFAULT;
 //     ERROR_NOT_FOUND:ptr^._errno:=ESRCH;
 //    else
 //     begin
 //      ptr^._errno:=EINVAL;
 //     end;
 //    end;
 //    
 //    {Return Error}
 //    Result:=-ptr^._errno;
 //   end;
 //
 //  //To Do // EINTR ?
 //          // ENOTTY ?
 //  Exit;
 // end;
 
 //{Return Response}
 //Result:=Response;
end;

{==============================================================================}
{==============================================================================}
{VC4 BCMHost Helper Functions}
function vc4_bcm_host_get_peripheral_address: cunsigned; cdecl;
{Get the peripheral base address for the current host}

{Note: Exported function for use by C libraries, not intended to be called by applications}
begin
 {}
 Result:=PeripheralGetBase;
 
 {$IFDEF VC4_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VC4: vc4_bcm_host_get_peripheral_address (Result=' + IntToHex(Result,8) + ')');
 {$ENDIF}
end;

{==============================================================================}

function vc4_bcm_host_get_peripheral_size: cunsigned; cdecl;
{Get the peripheral address size for the current host}

{Note: Exported function for use by C libraries, not intended to be called by applications}
begin
 {}
 Result:=PeripheralGetSize;
 
 {$IFDEF VC4_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VC4: vc4_bcm_host_get_peripheral_size (Result=' + IntToStr(Result) + ')');
 {$ENDIF}
end;

{==============================================================================}

function vc4_bcm_host_get_sdram_address: cunsigned; cdecl;
{Get the physical addrss of the peripheral address space for the current host}

{Note: Exported function for use by C libraries, not intended to be called by applications}
begin
 {}
 Result:=BUS_ALIAS;
 
 {$IFDEF VC4_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'VC4: vc4_bcm_host_get_sdram_address (Result=' + IntToHex(Result,8) + ')');
 {$ENDIF}
end;

{==============================================================================}
{==============================================================================}

initialization
 VC4Init;

{==============================================================================}
 
finalization
 VC4Stop;

{==============================================================================}
{==============================================================================}

end.

