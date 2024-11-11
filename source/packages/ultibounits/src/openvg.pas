{OpenVG Headers:
 
  Ported to FreePascal by Garry Wood <garry@softoz.com.au>
 
 Original Copyright:

  * OpenVG 1.1 Reference Implementation
  * -------------------------------------
  *
  * Copyright (c) 2008 The Khronos Group Inc.
  *
  * Permission is hereby granted, free of charge, to any person obtaining a
  * copy of this software and /or associated documentation files
  * (the "Materials "), to deal in the Materials without restriction,
  * including without limitation the rights to use, copy, modify, merge,
  * publish, distribute, sublicense, and/or sell copies of the Materials,
  * and to permit persons to whom the Materials are furnished to do so,
  * subject to the following conditions:
  *
  * The above copyright notice and this permission notice shall be included
  * in all copies or substantial portions of the Materials.
  *
  * THE MATERIALS ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
  * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
  * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
  * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
  * OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE MATERIALS OR
  * THE USE OR OTHER DEALINGS IN THE MATERIALS.
   
}

{$IFNDEF FPC_DOTTEDUNITS}
unit OpenVG;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc} {Default to ObjFPC compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}
 
interface
 
{$IFDEF FPC_DOTTEDUNITS}
uses
  {$ifdef ultibo}
  Core.GlobalTypes,
  Core.Syscalls,
  {$endif}
  System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  {$ifdef ultibo}
  GlobalTypes,
  Syscalls,
  {$endif}
  SysUtils;
{$ENDIF FPC_DOTTEDUNITS}
 
{$PACKRECORDS C}

const
 libOpenVG = 'OpenVG';
 
{$linklib libOpenVG}
 
{$ifndef ultibo}
type
 int8_t = Shortint;
 int16_t = Smallint;
 int32_t = Longint;
 
 uint8_t = Byte;
 uint16_t = Word;
 uint32_t = LongWord;
{$endif}
 
{From VG\vgplatform.h}
type 
 PVGfloat  = ^VGfloat;
 PVGbyte  = ^VGbyte;
 PVGubyte  = ^VGubyte;
 PVGshort  = ^VGshort;
 PVGint  = ^VGint;
 PVGuint  = ^VGuint;
 PVGbitfield  = ^VGbitfield;
 
 VGfloat = float_t;     {khronos_float_t}
 VGbyte = int8_t;       {khronos_int8_t}
 VGubyte = uint8_t;     {khronos_uint8_t}
 VGshort = int16_t;     {khronos_int16_t}
 VGint = int32_t;       {khronos_int32_t}
 VGuint = uint32_t;     {khronos_uint32_t}
 VGbitfield = uint32_t; {khronos_uint32_t}
 
{From VG\openvg.h} 
const
 OPENVG_VERSION_1_0 = 1;    
 OPENVG_VERSION_1_0_1 = 1;    
 OPENVG_VERSION_1_1 = 2;    
 
const
 VG_MAXSHORT = $7FFF;    
 
const
 VG_MAXINT = $7FFFFFFF;    

const
 VG_MAX_ENUM = $7FFFFFFF;    
 
type
 PVGHandle = ^VGHandle;
 VGHandle = VGuint;

 PVGPath = ^VGPath;
 VGPath = VGHandle;

 PVGImage = ^VGImage;
 VGImage = VGHandle;

 PVGMaskLayer = ^VGMaskLayer;
 VGMaskLayer = VGHandle;

 PVGFont = ^VGFont;
 VGFont = VGHandle;

 PVGPaint = ^VGPaint;
 VGPaint = VGHandle;
 
const 
 VG_INVALID_HANDLE:VGHandle = 0;
 
type
 PVGboolean = ^VGboolean;
 VGboolean = VGuint;

const
 VG_FALSE = 0;    
 VG_TRUE = 1;    
 
type
 PVGErrorCode = ^VGErrorCode;
 VGErrorCode = VGint;
const 
  VG_NO_ERROR = 0;
  VG_BAD_HANDLE_ERROR = $1000;
  VG_ILLEGAL_ARGUMENT_ERROR = $1001;
  VG_OUT_OF_MEMORY_ERROR = $1002;
  VG_PATH_CAPABILITY_ERROR = $1003;
  VG_UNSUPPORTED_IMAGE_FORMAT_ERROR = $1004;
  VG_UNSUPPORTED_PATH_FORMAT_ERROR = $1005;
  VG_IMAGE_IN_USE_ERROR = $1006;
  VG_NO_CONTEXT_ERROR = $1007;
  
  VG_ERROR_CODE_FORCE_SIZE = VG_MAX_ENUM;
 
type
 PVGParamType = ^VGParamType;
 VGParamType = VGint;
const 
 {Mode settings}
 VG_MATRIX_MODE = $1100;
 VG_FILL_RULE = $1101;
 VG_IMAGE_QUALITY = $1102;
 VG_RENDERING_QUALITY = $1103;
 VG_BLEND_MODE = $1104;
 VG_IMAGE_MODE = $1105;
 
 {Scissoring rectangles}
 VG_SCISSOR_RECTS = $1106;
 
 {Color Transformation}
 VG_COLOR_TRANSFORM = $1170;
 VG_COLOR_TRANSFORM_VALUES = $1171;
 
 {Stroke parameters}
 VG_STROKE_LINE_WIDTH = $1110;
 VG_STROKE_CAP_STYLE = $1111;
 VG_STROKE_JOIN_STYLE = $1112;
 VG_STROKE_MITER_LIMIT = $1113;
 VG_STROKE_DASH_PATTERN = $1114;
 VG_STROKE_DASH_PHASE = $1115;
 VG_STROKE_DASH_PHASE_RESET = $1116;
 
 {Edge fill color for VG_TILE_FILL tiling mode}
 VG_TILE_FILL_COLOR = $1120;
 
 {Color for vgClear}
 VG_CLEAR_COLOR = $1121;
 
 {Glyph origin}
 VG_GLYPH_ORIGIN = $1122;
 
 {Enable/disable alpha masking and scissoring}
 VG_MASKING = $1130;
 VG_SCISSORING = $1131;
 
 {Pixel layout information}
 VG_PIXEL_LAYOUT = $1140;
 VG_SCREEN_LAYOUT = $1141;
 
 {Source format selection for image filters}
 VG_FILTER_FORMAT_LINEAR = $1150;
 VG_FILTER_FORMAT_PREMULTIPLIED = $1151;
 
 {Destination write enable mask for image filters}
 VG_FILTER_CHANNEL_MASK = $1152;
 
 {Implementation limits (read-only)}
 VG_MAX_SCISSOR_RECTS = $1160;
 VG_MAX_DASH_COUNT = $1161;
 VG_MAX_KERNEL_SIZE = $1162;
 VG_MAX_SEPARABLE_KERNEL_SIZE = $1163;
 VG_MAX_COLOR_RAMP_STOPS = $1164;
 VG_MAX_IMAGE_WIDTH = $1165;
 VG_MAX_IMAGE_HEIGHT = $1166;
 VG_MAX_IMAGE_PIXELS = $1167;
 VG_MAX_IMAGE_BYTES = $1168;
 VG_MAX_FLOAT = $1169;
 VG_MAX_GAUSSIAN_STD_DEVIATION = $116A;
 
 VG_PARAM_TYPE_FORCE_SIZE = VG_MAX_ENUM;

type  
 PVGRenderingQuality = ^VGRenderingQuality;
 VGRenderingQuality = VGint;
const 
 VG_RENDERING_QUALITY_NONANTIALIASED = $1200;
 VG_RENDERING_QUALITY_FASTER = $1201;
 VG_RENDERING_QUALITY_BETTER = $1202; {Default}
  
 VG_RENDERING_QUALITY_FORCE_SIZE = VG_MAX_ENUM;
 
type
 PVGPixelLayout = ^VGPixelLayout;
 VGPixelLayout = VGint;
const 
 VG_PIXEL_LAYOUT_UNKNOWN = $1300;
 VG_PIXEL_LAYOUT_RGB_VERTICAL = $1301;
 VG_PIXEL_LAYOUT_BGR_VERTICAL = $1302;
 VG_PIXEL_LAYOUT_RGB_HORIZONTAL = $1303;
 VG_PIXEL_LAYOUT_BGR_HORIZONTAL = $1304;
 
 VG_PIXEL_LAYOUT_FORCE_SIZE = VG_MAX_ENUM;

type 
 PVGMatrixMode = ^VGMatrixMode;
 VGMatrixMode = VGint;
const 
 VG_MATRIX_PATH_USER_TO_SURFACE = $1400;
 VG_MATRIX_IMAGE_USER_TO_SURFACE = $1401;
 VG_MATRIX_FILL_PAINT_TO_USER = $1402;
 VG_MATRIX_STROKE_PAINT_TO_USER = $1403;
 VG_MATRIX_GLYPH_USER_TO_SURFACE = $1404;
     
 VG_MATRIX_MODE_FORCE_SIZE = VG_MAX_ENUM;

type 
 PVGMaskOperation = ^VGMaskOperation;
 VGMaskOperation = VGint;
const
 VG_CLEAR_MASK = $1500;
 VG_FILL_MASK = $1501;
 VG_SET_MASK = $1502;
 VG_UNION_MASK = $1503;
 VG_INTERSECT_MASK = $1504;
 VG_SUBTRACT_MASK = $1505;
     
 VG_MASK_OPERATION_FORCE_SIZE = VG_MAX_ENUM;
 
const
 VG_PATH_FORMAT_STANDARD = 0;    
 
type
 PVGPathDatatype = ^VGPathDatatype;
 VGPathDatatype = VGint;
const 
 VG_PATH_DATATYPE_S_8 = 0;
 VG_PATH_DATATYPE_S_16 = 1;
 VG_PATH_DATATYPE_S_32 = 2;
 VG_PATH_DATATYPE_F = 3;
 
 VG_PATH_DATATYPE_FORCE_SIZE = VG_MAX_ENUM;

type 
 PVGPathAbsRel = ^VGPathAbsRel;
 VGPathAbsRel = VGint;
const 
 VG_ABSOLUTE = 0;
 VG_RELATIVE = 1;
  
 VG_PATH_ABS_REL_FORCE_SIZE = VG_MAX_ENUM;

type 
 PVGPathSegment = ^VGPathSegment;
 VGPathSegment = VGint;
const 
 VG_CLOSE_PATH = 0 shl 1;
 VG_MOVE_TO = 1 shl 1;
 VG_LINE_TO = 2 shl 1;
 VG_HLINE_TO = 3 shl 1;
 VG_VLINE_TO = 4 shl 1;
 VG_QUAD_TO = 5 shl 1;
 VG_CUBIC_TO = 6 shl 1;
 VG_SQUAD_TO = 7 shl 1;
 VG_SCUBIC_TO = 8 shl 1;
 VG_SCCWARC_TO = 9 shl 1;
 VG_SCWARC_TO = 10 shl 1;
 VG_LCCWARC_TO = 11 shl 1;
 VG_LCWARC_TO = 12 shl 1;
 
 VG_SEGMENT_MASK = $f shl 1;
 
 VG_PATH_SEGMENT_FORCE_SIZE = VG_MAX_ENUM;

type
 PVGPathCommand = ^VGPathCommand;
 VGPathCommand = VGint;
const 
 VG_MOVE_TO_ABS = VG_MOVE_TO or VG_ABSOLUTE;
 VG_MOVE_TO_REL = VG_MOVE_TO or VG_RELATIVE;
 VG_LINE_TO_ABS = VG_LINE_TO or VG_ABSOLUTE;
 VG_LINE_TO_REL = VG_LINE_TO or VG_RELATIVE;
 VG_HLINE_TO_ABS = VG_HLINE_TO or VG_ABSOLUTE;
 VG_HLINE_TO_REL = VG_HLINE_TO or VG_RELATIVE;
 VG_VLINE_TO_ABS = VG_VLINE_TO or VG_ABSOLUTE;
 VG_VLINE_TO_REL = VG_VLINE_TO or VG_RELATIVE;
 VG_QUAD_TO_ABS = VG_QUAD_TO or VG_ABSOLUTE;
 VG_QUAD_TO_REL = VG_QUAD_TO or VG_RELATIVE;
 VG_CUBIC_TO_ABS = VG_CUBIC_TO or VG_ABSOLUTE;
 VG_CUBIC_TO_REL = VG_CUBIC_TO or VG_RELATIVE;
 VG_SQUAD_TO_ABS = VG_SQUAD_TO or VG_ABSOLUTE;
 VG_SQUAD_TO_REL = VG_SQUAD_TO or VG_RELATIVE;
 VG_SCUBIC_TO_ABS = VG_SCUBIC_TO or VG_ABSOLUTE;
 VG_SCUBIC_TO_REL = VG_SCUBIC_TO or VG_RELATIVE;
 VG_SCCWARC_TO_ABS = VG_SCCWARC_TO or VG_ABSOLUTE;
 VG_SCCWARC_TO_REL = VG_SCCWARC_TO or VG_RELATIVE;
 VG_SCWARC_TO_ABS = VG_SCWARC_TO or VG_ABSOLUTE;
 VG_SCWARC_TO_REL = VG_SCWARC_TO or VG_RELATIVE;
 VG_LCCWARC_TO_ABS = VG_LCCWARC_TO or VG_ABSOLUTE;
 VG_LCCWARC_TO_REL = VG_LCCWARC_TO or VG_RELATIVE;
 VG_LCWARC_TO_ABS = VG_LCWARC_TO or VG_ABSOLUTE;
 VG_LCWARC_TO_REL = VG_LCWARC_TO or VG_RELATIVE;
 
 VG_PATH_COMMAND_FORCE_SIZE = VG_MAX_ENUM;

type 
 PVGPathCapabilities = ^VGPathCapabilities;
 VGPathCapabilities = VGbitfield;
const 
 VG_PATH_CAPABILITY_APPEND_FROM = 1 shl 0;
 VG_PATH_CAPABILITY_APPEND_TO = 1 shl 1;
 VG_PATH_CAPABILITY_MODIFY = 1 shl 2;
 VG_PATH_CAPABILITY_TRANSFORM_FROM = 1 shl 3;
 VG_PATH_CAPABILITY_TRANSFORM_TO = 1 shl 4;
 VG_PATH_CAPABILITY_INTERPOLATE_FROM = 1 shl 5;
 VG_PATH_CAPABILITY_INTERPOLATE_TO = 1 shl 6;
 VG_PATH_CAPABILITY_PATH_LENGTH = 1 shl 7;
 VG_PATH_CAPABILITY_POINT_ALONG_PATH = 1 shl 8;
 VG_PATH_CAPABILITY_TANGENT_ALONG_PATH = 1 shl 9;
 VG_PATH_CAPABILITY_PATH_BOUNDS = 1 shl 10;
 VG_PATH_CAPABILITY_PATH_TRANSFORMED_BOUNDS = 1 shl 11;
 VG_PATH_CAPABILITY_ALL = (1 shl 12) -1;
  
 VG_PATH_CAPABILITIES_FORCE_SIZE = VG_MAX_ENUM;

type 
 PVGPathParamType = ^VGPathParamType;
 VGPathParamType = VGint;
const 
 VG_PATH_FORMAT = $1600;
 VG_PATH_DATATYPE = $1601;
 VG_PATH_SCALE = $1602;
 VG_PATH_BIAS = $1603;
 VG_PATH_NUM_SEGMENTS = $1604;
 VG_PATH_NUM_COORDS = $1605;
 
 VG_PATH_PARAM_TYPE_FORCE_SIZE = VG_MAX_ENUM;

type
 PVGCapStyle = ^VGCapStyle;
 VGCapStyle = VGint;
const 
 VG_CAP_BUTT = $1700;
 VG_CAP_ROUND = $1701;
 VG_CAP_SQUARE = $1702;
  
 VG_CAP_STYLE_FORCE_SIZE = VG_MAX_ENUM;

type
 PVGJoinStyle = ^VGJoinStyle;
 VGJoinStyle = VGint;
const 
 VG_JOIN_MITER = $1800;
 VG_JOIN_ROUND = $1801;
 VG_JOIN_BEVEL = $1802;
  
 VG_JOIN_STYLE_FORCE_SIZE = VG_MAX_ENUM;

type
 PVGFillRule = ^VGFillRule;
 VGFillRule = VGint;
const 
 VG_EVEN_ODD = $1900;
 VG_NON_ZERO = $1901;
  
 VG_FILL_RULE_FORCE_SIZE = VG_MAX_ENUM;
 
type
 PVGPaintMode = ^VGPaintMode;
 VGPaintMode = VGbitfield;
const 
 VG_STROKE_PATH = 1 shl 0;
 VG_FILL_PATH = 1 shl 1;
  
 VG_PAINT_MODE_FORCE_SIZE = VG_MAX_ENUM;
 
type 
 PVGPaintParamType = ^VGPaintParamType;
 VGPaintParamType = VGint;
const
 {Color paint parameters}
 VG_PAINT_TYPE = $1A00;
 VG_PAINT_COLOR = $1A01;
 VG_PAINT_COLOR_RAMP_SPREAD_MODE = $1A02;
 VG_PAINT_COLOR_RAMP_PREMULTIPLIED = $1A07;
 VG_PAINT_COLOR_RAMP_STOPS = $1A03;
  
 {Linear gradient paint parameters}
 VG_PAINT_LINEAR_GRADIENT = $1A04;
  
 {Radial gradient paint parameters}
 VG_PAINT_RADIAL_GRADIENT = $1A05;
  
 {Pattern paint parameters}
 VG_PAINT_PATTERN_TILING_MODE = $1A06;
  
 VG_PAINT_PARAM_TYPE_FORCE_SIZE = VG_MAX_ENUM;
 
type 
 PVGPaintType = ^VGPaintType;
 VGPaintType = VGint;
const 
 VG_PAINT_TYPE_COLOR = $1B00;
 VG_PAINT_TYPE_LINEAR_GRADIENT = $1B01;
 VG_PAINT_TYPE_RADIAL_GRADIENT = $1B02;
 VG_PAINT_TYPE_PATTERN = $1B03;
  
 VG_PAINT_TYPE_FORCE_SIZE = VG_MAX_ENUM;
 
type 
 PVGColorRampSpreadMode = ^VGColorRampSpreadMode;
 VGColorRampSpreadMode = VGint;
const 
 VG_COLOR_RAMP_SPREAD_PAD = $1C00;
 VG_COLOR_RAMP_SPREAD_REPEAT = $1C01;
 VG_COLOR_RAMP_SPREAD_REFLECT = $1C02;
  
 VG_COLOR_RAMP_SPREAD_MODE_FORCE_SIZE = VG_MAX_ENUM;

type
 PVGTilingMode = ^VGTilingMode;
 VGTilingMode = VGint;
const 
 VG_TILE_FILL = $1D00;
 VG_TILE_PAD = $1D01;
 VG_TILE_REPEAT = $1D02;
 VG_TILE_REFLECT = $1D03;
  
 VG_TILING_MODE_FORCE_SIZE = VG_MAX_ENUM;

type 
 PVGImageFormat = ^VGImageFormat;
 VGImageFormat = VGint;
const 
 {RGB(A,X) channel ordering}
 VG_sRGBX_8888 = 0;
 VG_sRGBA_8888 = 1;
 VG_sRGBA_8888_PRE = 2;
 VG_sRGB_565 = 3;
 VG_sRGBA_5551 = 4;
 VG_sRGBA_4444 = 5;
 VG_sL_8 = 6;
 VG_lRGBX_8888 = 7;
 VG_lRGBA_8888 = 8;
 VG_lRGBA_8888_PRE = 9;
 VG_lL_8 = 10;
 VG_A_8 = 11;
 VG_BW_1 = 12;
 VG_A_1 = 13;
 VG_A_4 = 14;
 
 {(A,X)RGB channel ordering}
 VG_sXRGB_8888 = 0 or (1 shl 6);
 VG_sARGB_8888 = 1 or (1 shl 6);
 VG_sARGB_8888_PRE = 2 or (1 shl 6);
 VG_sARGB_1555 = 4 or (1 shl 6);
 VG_sARGB_4444 = 5 or (1 shl 6);
 VG_lXRGB_8888 = 7 or (1 shl 6);
 VG_lARGB_8888 = 8 or (1 shl 6);
 VG_lARGB_8888_PRE = 9 or (1 shl 6);
 
 {BGR(A,X) channel ordering}
 VG_sBGRX_8888 = 0 or (1 shl 7);
 VG_sBGRA_8888 = 1 or (1 shl 7);
 VG_sBGRA_8888_PRE = 2 or (1 shl 7);
 VG_sBGR_565 = 3 or (1 shl 7);
 VG_sBGRA_5551 = 4 or (1 shl 7);
 VG_sBGRA_4444 = 5 or (1 shl 7);
 VG_lBGRX_8888 = 7 or (1 shl 7);
 VG_lBGRA_8888 = 8 or (1 shl 7);
 VG_lBGRA_8888_PRE = 9 or (1 shl 7);
 
 {(A,X)BGR channel ordering}
 VG_sXBGR_8888 = (0 or (1 shl 6)) or (1 shl 7);
 VG_sABGR_8888 = (1 or (1 shl 6)) or (1 shl 7);
 VG_sABGR_8888_PRE = (2 or (1 shl 6)) or (1 shl 7);
 VG_sABGR_1555 = (4 or (1 shl 6)) or (1 shl 7);
 VG_sABGR_4444 = (5 or (1 shl 6)) or (1 shl 7);
 VG_lXBGR_8888 = (7 or (1 shl 6)) or (1 shl 7);
 VG_lABGR_8888 = (8 or (1 shl 6)) or (1 shl 7);
 VG_lABGR_8888_PRE = (9 or (1 shl 6)) or (1 shl 7);
 
 VG_IMAGE_FORMAT_FORCE_SIZE = VG_MAX_ENUM;

type
 PVGImageQuality = ^VGImageQuality;
 VGImageQuality = VGbitfield;
const
 VG_IMAGE_QUALITY_NONANTIALIASED = 1 shl 0;
 VG_IMAGE_QUALITY_FASTER = 1 shl 1;
 VG_IMAGE_QUALITY_BETTER = 1 shl 2;
 
 VG_IMAGE_QUALITY_FORCE_SIZE = VG_MAX_ENUM;

type 
 PVGImageParamType = ^VGImageParamType;
 VGImageParamType = VGint;
const 
 VG_IMAGE_FORMAT = $1E00;
 VG_IMAGE_WIDTH = $1E01;
 VG_IMAGE_HEIGHT = $1E02;
  
 VG_IMAGE_PARAM_TYPE_FORCE_SIZE = VG_MAX_ENUM;

type 
 PVGImageMode = ^VGImageMode;
 VGImageMode = VGint;
const 
 VG_DRAW_IMAGE_NORMAL = $1F00;
 VG_DRAW_IMAGE_MULTIPLY = $1F01;
 VG_DRAW_IMAGE_STENCIL = $1F02;
  
 VG_IMAGE_MODE_FORCE_SIZE = VG_MAX_ENUM;

type
 PVGImageChannel = ^VGImageChannel;
 VGImageChannel = VGint;
const 
 VG_RED = 1 shl 3;
 VG_GREEN = 1 shl 2;
 VG_BLUE = 1 shl 1;
 VG_ALPHA = 1 shl 0;
  
 VG_IMAGE_CHANNEL_FORCE_SIZE = VG_MAX_ENUM;

type
 PVGBlendMode = ^VGBlendMode;
 VGBlendMode = VGint;
const 
 VG_BLEND_SRC = $2000;
 VG_BLEND_SRC_OVER = $2001;
 VG_BLEND_DST_OVER = $2002;
 VG_BLEND_SRC_IN = $2003;
 VG_BLEND_DST_IN = $2004;
 VG_BLEND_MULTIPLY = $2005;
 VG_BLEND_SCREEN = $2006;
 VG_BLEND_DARKEN = $2007;
 VG_BLEND_LIGHTEN = $2008;
 VG_BLEND_ADDITIVE = $2009;
 
 VG_BLEND_MODE_FORCE_SIZE = VG_MAX_ENUM;

type
 PVGFontParamType = ^VGFontParamType;
 VGFontParamType = VGint;
const 
 VG_FONT_NUM_GLYPHS = $2F00;
  
 VG_FONT_PARAM_TYPE_FORCE_SIZE = VG_MAX_ENUM;

type
 PVGHardwareQueryType = ^VGHardwareQueryType;
 VGHardwareQueryType = VGint;
const 
 VG_IMAGE_FORMAT_QUERY = $2100;
 VG_PATH_DATATYPE_QUERY = $2101;
  
 VG_HARDWARE_QUERY_TYPE_FORCE_SIZE = VG_MAX_ENUM;

type
 PVGHardwareQueryResult = ^VGHardwareQueryResult;
 VGHardwareQueryResult = VGint;
const 
 VG_HARDWARE_ACCELERATED = $2200;
 VG_HARDWARE_UNACCELERATED = $2201;
  
 VG_HARDWARE_QUERY_RESULT_FORCE_SIZE = VG_MAX_ENUM;

type
 PVGStringID = ^VGStringID;
 VGStringID = VGint;
const 
 VG_VENDOR = $2300;
 VG_RENDERER = $2301;
 VG_VERSION = $2302;
 VG_EXTENSIONS = $2303;
  
 VG_STRING_ID_FORCE_SIZE = VG_MAX_ENUM;
 
 function vgGetError:VGErrorCode; cdecl; external libOpenVG name 'vgGetError';
 procedure vgFlush; cdecl; external libOpenVG name 'vgFlush';
 procedure vgFinish; cdecl; external libOpenVG name 'vgFinish';

 { Getters and Setters  }
 procedure vgSetf(_type:VGParamType; value:VGfloat); cdecl; external libOpenVG name 'vgSetf';
 procedure vgSeti(_type:VGParamType; value:VGint); cdecl; external libOpenVG name 'vgSeti';
 procedure vgSetfv(_type:VGParamType; count:VGint; values:PVGfloat); cdecl; external libOpenVG name 'vgSetfv';
 procedure vgSetiv(_type:VGParamType; count:VGint; values:PVGint); cdecl; external libOpenVG name 'vgSetiv';
 function vgGetf(_type:VGParamType):VGfloat; cdecl; external libOpenVG name 'vgGetf';
 function vgGeti(_type:VGParamType):VGint; cdecl; external libOpenVG name 'vgGeti';
 function vgGetVectorSize(_type:VGParamType):VGint; cdecl; external libOpenVG name 'vgGetVectorSize';
 procedure vgGetfv(_type:VGParamType; count:VGint; values:PVGfloat); cdecl; external libOpenVG name 'vgGetfv';
 procedure vgGetiv(_type:VGParamType; count:VGint; values:PVGint); cdecl; external libOpenVG name 'vgGetiv';
 procedure vgSetParameterf(_object:VGHandle; paramType:VGint; value:VGfloat); cdecl; external libOpenVG name 'vgSetParameterf';
 procedure vgSetParameteri(_object:VGHandle; paramType:VGint; value:VGint); cdecl; external libOpenVG name 'vgSetParameteri';
 procedure vgSetParameterfv(_object:VGHandle; paramType:VGint; count:VGint; values:PVGfloat); cdecl; external libOpenVG name 'vgSetParameterfv';
 procedure vgSetParameteriv(_object:VGHandle; paramType:VGint; count:VGint; values:PVGint); cdecl; external libOpenVG name 'vgSetParameteriv';
 function vgGetParameterf(_object:VGHandle; paramType:VGint):VGfloat; cdecl; external libOpenVG name 'vgGetParameterf';
 function vgGetParameteri(_object:VGHandle; paramType:VGint):VGint; cdecl; external libOpenVG name 'vgGetParameteri';
 function vgGetParameterVectorSize(_object:VGHandle; paramType:VGint):VGint; cdecl; external libOpenVG name 'vgGetParameterVectorSize';
 procedure vgGetParameterfv(_object:VGHandle; paramType:VGint; count:VGint; values:PVGfloat); cdecl; external libOpenVG name 'vgGetParameterfv';
 procedure vgGetParameteriv(_object:VGHandle; paramType:VGint; count:VGint; values:PVGint); cdecl; external libOpenVG name 'vgGetParameteriv';

 { Matrix Manipulation  }
 procedure vgLoadIdentity; cdecl; external libOpenVG name 'vgLoadIdentity';
 procedure vgLoadMatrix(m:PVGfloat); cdecl; external libOpenVG name 'vgLoadMatrix';
 procedure vgGetMatrix(m:PVGfloat); cdecl; external libOpenVG name 'vgGetMatrix';
 procedure vgMultMatrix(m:PVGfloat); cdecl; external libOpenVG name 'vgMultMatrix';
 procedure vgTranslate(tx:VGfloat; ty:VGfloat); cdecl; external libOpenVG name 'vgTranslate';
 procedure vgScale(sx:VGfloat; sy:VGfloat); cdecl; external libOpenVG name 'vgScale';
 procedure vgShear(shx:VGfloat; shy:VGfloat); cdecl; external libOpenVG name 'vgShear';
 procedure vgRotate(angle:VGfloat); cdecl; external libOpenVG name 'vgRotate';

 { Masking and Clearing  }
 procedure vgMask(mask:VGHandle; operation:VGMaskOperation; x:VGint; y:VGint; width:VGint; 
            height:VGint); cdecl; external libOpenVG name 'vgMask';
 procedure vgRenderToMask(path:VGPath; paintModes:VGbitfield; operation:VGMaskOperation); cdecl; external libOpenVG name 'vgRenderToMask';
 function vgCreateMaskLayer(width:VGint; height:VGint):VGMaskLayer; cdecl; external libOpenVG name 'vgCreateMaskLayer';
 procedure vgDestroyMaskLayer(maskLayer:VGMaskLayer); cdecl; external libOpenVG name 'vgDestroyMaskLayer';
 procedure vgFillMaskLayer(maskLayer:VGMaskLayer; x:VGint; y:VGint; width:VGint; height:VGint; 
            value:VGfloat); cdecl; external libOpenVG name 'vgFillMaskLayer';
 procedure vgCopyMask(maskLayer:VGMaskLayer; dx:VGint; dy:VGint; sx:VGint; sy:VGint; 
            width:VGint; height:VGint); cdecl; external libOpenVG name 'vgCopyMask';
 procedure vgClear(x:VGint; y:VGint; width:VGint; height:VGint); cdecl; external libOpenVG name 'vgClear';

 { Paths  }
 function vgCreatePath(pathFormat:VGint; datatype:VGPathDatatype; scale:VGfloat; bias:VGfloat; segmentCapacityHint:VGint; 
           coordCapacityHint:VGint; capabilities:VGbitfield):VGPath; cdecl; external libOpenVG name 'vgCreatePath';
 procedure vgClearPath(path:VGPath; capabilities:VGbitfield); cdecl; external libOpenVG name 'vgClearPath';
 procedure vgDestroyPath(path:VGPath); cdecl; external libOpenVG name 'vgDestroyPath';
 procedure vgRemovePathCapabilities(path:VGPath; capabilities:VGbitfield); cdecl; external libOpenVG name 'vgRemovePathCapabilities';
 function vgGetPathCapabilities(path:VGPath):VGbitfield; cdecl; external libOpenVG name 'vgGetPathCapabilities';
 procedure vgAppendPath(dstPath:VGPath; srcPath:VGPath); cdecl; external libOpenVG name 'vgAppendPath';
 procedure vgAppendPathData(dstPath:VGPath; numSegments:VGint; pathSegments:PVGubyte; pathData:pointer); cdecl; external libOpenVG name 'vgAppendPathData';
 procedure vgModifyPathCoords(dstPath:VGPath; startIndex:VGint; numSegments:VGint; pathData:pointer); cdecl; external libOpenVG name 'vgModifyPathCoords';
 procedure vgTransformPath(dstPath:VGPath; srcPath:VGPath); cdecl; external libOpenVG name 'vgTransformPath';
 function vgInterpolatePath(dstPath:VGPath; startPath:VGPath; endPath:VGPath; amount:VGfloat):VGboolean; cdecl; external libOpenVG name 'vgInterpolatePath';
 function vgPathLength(path:VGPath; startSegment:VGint; numSegments:VGint):VGfloat; cdecl; external libOpenVG name 'vgPathLength';
 procedure vgPointAlongPath(path:VGPath; startSegment:VGint; numSegments:VGint; distance:VGfloat; x:PVGfloat; 
            y:PVGfloat; tangentX:PVGfloat; tangentY:PVGfloat); cdecl; external libOpenVG name 'vgPointAlongPath';
 procedure vgPathBounds(path:VGPath; minX:PVGfloat; minY:PVGfloat; width:PVGfloat; height:PVGfloat); cdecl; external libOpenVG name 'vgPathBounds';
 procedure vgPathTransformedBounds(path:VGPath; minX:PVGfloat; minY:PVGfloat; width:PVGfloat; height:PVGfloat); cdecl; external libOpenVG name 'vgPathTransformedBounds';
 procedure vgDrawPath(path:VGPath; paintModes:VGbitfield); cdecl; external libOpenVG name 'vgDrawPath';

 { Paint  }
 function vgCreatePaint:VGPaint; cdecl; external libOpenVG name 'vgCreatePaint';
 procedure vgDestroyPaint(paint:VGPaint); cdecl; external libOpenVG name 'vgDestroyPaint';
 procedure vgSetPaint(paint:VGPaint; paintModes:VGbitfield); cdecl; external libOpenVG name 'vgSetPaint';
 function vgGetPaint(paintMode:VGPaintMode):VGPaint; cdecl; external libOpenVG name 'vgGetPaint';
 procedure vgSetColor(paint:VGPaint; rgba:VGuint); cdecl; external libOpenVG name 'vgSetColor';
 function vgGetColor(paint:VGPaint):VGuint; cdecl; external libOpenVG name 'vgGetColor';
 procedure vgPaintPattern(paint:VGPaint; pattern:VGImage); cdecl; external libOpenVG name 'vgPaintPattern';

 { Images  }
 function vgCreateImage(format:VGImageFormat; width:VGint; height:VGint; allowedQuality:VGbitfield):VGImage; cdecl; external libOpenVG name 'vgCreateImage';
 procedure vgDestroyImage(image:VGImage); cdecl; external libOpenVG name 'vgDestroyImage';
 procedure vgClearImage(image:VGImage; x:VGint; y:VGint; width:VGint; height:VGint); cdecl; external libOpenVG name 'vgClearImage';
 procedure vgImageSubData(image:VGImage; data:pointer; dataStride:VGint; dataFormat:VGImageFormat; x:VGint; 
            y:VGint; width:VGint; height:VGint); cdecl; external libOpenVG name 'vgImageSubData';
 procedure vgGetImageSubData(image:VGImage; data:pointer; dataStride:VGint; dataFormat:VGImageFormat; x:VGint; 
            y:VGint; width:VGint; height:VGint); cdecl; external libOpenVG name 'vgGetImageSubData';
 function vgChildImage(parent:VGImage; x:VGint; y:VGint; width:VGint; height:VGint):VGImage; cdecl; external libOpenVG name 'vgChildImage';
 function vgGetParent(image:VGImage):VGImage; cdecl; external libOpenVG name 'vgGetParent';
 procedure vgCopyImage(dst:VGImage; dx:VGint; dy:VGint; src:VGImage; sx:VGint; 
            sy:VGint; width:VGint; height:VGint; dither:VGboolean); cdecl; external libOpenVG name 'vgCopyImage';
 procedure vgDrawImage(image:VGImage); cdecl; external libOpenVG name 'vgDrawImage';
 procedure vgSetPixels(dx:VGint; dy:VGint; src:VGImage; sx:VGint; sy:VGint; 
            width:VGint; height:VGint); cdecl; external libOpenVG name 'vgSetPixels';
 procedure vgWritePixels(data:pointer; dataStride:VGint; dataFormat:VGImageFormat; dx:VGint; dy:VGint; 
            width:VGint; height:VGint); cdecl; external libOpenVG name 'vgWritePixels';
 procedure vgGetPixels(dst:VGImage; dx:VGint; dy:VGint; sx:VGint; sy:VGint; 
            width:VGint; height:VGint); cdecl; external libOpenVG name 'vgGetPixels';
 procedure vgReadPixels(data:pointer; dataStride:VGint; dataFormat:VGImageFormat; sx:VGint; sy:VGint; 
            width:VGint; height:VGint); cdecl; external libOpenVG name 'vgReadPixels';
 procedure vgCopyPixels(dx:VGint; dy:VGint; sx:VGint; sy:VGint; width:VGint; 
            height:VGint); cdecl; external libOpenVG name 'vgCopyPixels';

 { Text  }
type 
 PVGfloat_array2 = ^VGfloat_array2;
 VGfloat_array2 = array[0..1] of VGfloat;
 
 function vgCreateFont(glyphCapacityHint:VGint):VGFont; cdecl; external libOpenVG name 'vgCreateFont';
 procedure vgDestroyFont(font:VGFont); cdecl; external libOpenVG name 'vgDestroyFont';
 procedure vgSetGlyphToPath(font:VGFont; glyphIndex:VGuint; path:VGPath; isHinted:VGboolean; glyphOrigin:PVGfloat_array2; 
            escapement:PVGfloat_array2); cdecl; external libOpenVG name 'vgSetGlyphToPath';
 procedure vgSetGlyphToImage(font:VGFont; glyphIndex:VGuint; image:VGImage; glyphOrigin:PVGfloat_array2; escapement:PVGfloat_array2); cdecl; external libOpenVG name 'vgSetGlyphToImage';
 procedure vgClearGlyph(font:VGFont; glyphIndex:VGuint); cdecl; external libOpenVG name 'vgClearGlyph';
 procedure vgDrawGlyph(font:VGFont; glyphIndex:VGuint; paintModes:VGbitfield; allowAutoHinting:VGboolean); cdecl; external libOpenVG name 'vgDrawGlyph';
 procedure vgDrawGlyphs(font:VGFont; glyphCount:VGint; glyphIndices:PVGuint; adjustments_x:PVGfloat; adjustments_y:PVGfloat; 
            paintModes:VGbitfield; allowAutoHinting:VGboolean); cdecl; external libOpenVG name 'vgDrawGlyphs';

 { Image Filters  }
 procedure vgColorMatrix(dst:VGImage; src:VGImage; matrix:PVGfloat); cdecl; external libOpenVG name 'vgColorMatrix';
 procedure vgConvolve(dst:VGImage; src:VGImage; kernelWidth:VGint; kernelHeight:VGint; shiftX:VGint; 
            shiftY:VGint; kernel:PVGshort; scale:VGfloat; bias:VGfloat; tilingMode:VGTilingMode); cdecl; external libOpenVG name 'vgConvolve';
 procedure vgSeparableConvolve(dst:VGImage; src:VGImage; kernelWidth:VGint; kernelHeight:VGint; shiftX:VGint; 
            shiftY:VGint; kernelX:PVGshort; kernelY:PVGshort; scale:VGfloat; bias:VGfloat; 
            tilingMode:VGTilingMode); cdecl; external libOpenVG name 'vgSeparableConvolve';
 procedure vgGaussianBlur(dst:VGImage; src:VGImage; stdDeviationX:VGfloat; stdDeviationY:VGfloat; tilingMode:VGTilingMode); cdecl; external libOpenVG name 'vgGaussianBlur';
 procedure vgLookup(dst:VGImage; src:VGImage; redLUT:PVGubyte; greenLUT:PVGubyte; blueLUT:PVGubyte; 
            alphaLUT:PVGubyte; outputLinear:VGboolean; outputPremultiplied:VGboolean); cdecl; external libOpenVG name 'vgLookup';
 procedure vgLookupSingle(dst:VGImage; src:VGImage; lookupTable:PVGuint; sourceChannel:VGImageChannel; outputLinear:VGboolean; 
            outputPremultiplied:VGboolean); cdecl; external libOpenVG name 'vgLookupSingle';

 { Hardware Queries  }
 function vgHardwareQuery(key:VGHardwareQueryType; setting:VGint):VGHardwareQueryResult; cdecl; external libOpenVG name 'vgHardwareQuery';

 { Renderer and Extension Information  }
 function vgGetString(name:VGStringID):PVGubyte; cdecl; external libOpenVG name 'vgGetString';

{From VG\vgu.h}
const
 VGU_VERSION_1_0 = 1;    
 VGU_VERSION_1_1 = 2;    

type
 PVGUErrorCode = ^VGUErrorCode;
 VGUErrorCode = VGint;
const 
 VGU_NO_ERROR = 0;
 VGU_BAD_HANDLE_ERROR = $F000;
 VGU_ILLEGAL_ARGUMENT_ERROR = $F001;
 VGU_OUT_OF_MEMORY_ERROR = $F002;
 VGU_PATH_CAPABILITY_ERROR = $F003;
 VGU_BAD_WARP_ERROR = $F004;
  
 VGU_ERROR_CODE_FORCE_SIZE = VG_MAX_ENUM;

type
 PVGUArcType = ^VGUArcType;
 VGUArcType = VGint;
const 
 VGU_ARC_OPEN = $F100;
 VGU_ARC_CHORD = $F101;
 VGU_ARC_PIE = $F102;
  
 VGU_ARC_TYPE_FORCE_SIZE = VG_MAX_ENUM;
 
 function vguLine(path:VGPath; x0:VGfloat; y0:VGfloat; x1:VGfloat; y1:VGfloat):VGUErrorCode; cdecl; external libOpenVG name 'vguLine';
 function vguPolygon(path:VGPath; points:PVGfloat; count:VGint; closed:VGboolean):VGUErrorCode; cdecl; external libOpenVG name 'vguPolygon';
 function vguRect(path:VGPath; x:VGfloat; y:VGfloat; width:VGfloat; height:VGfloat):VGUErrorCode; cdecl; external libOpenVG name 'vguRect';
 function vguRoundRect(path:VGPath; x:VGfloat; y:VGfloat; width:VGfloat; height:VGfloat; 
           arcWidth:VGfloat; arcHeight:VGfloat):VGUErrorCode; cdecl; external libOpenVG name 'vguRoundRect';
 function vguEllipse(path:VGPath; cx:VGfloat; cy:VGfloat; width:VGfloat; height:VGfloat):VGUErrorCode; cdecl; external libOpenVG name 'vguEllipse';
 function vguArc(path:VGPath; x:VGfloat; y:VGfloat; width:VGfloat; height:VGfloat; 
           startAngle:VGfloat; angleExtent:VGfloat; arcType:VGUArcType):VGUErrorCode; cdecl; external libOpenVG name 'vguArc';
 function vguComputeWarpQuadToSquare(sx0:VGfloat; sy0:VGfloat; sx1:VGfloat; sy1:VGfloat; sx2:VGfloat; 
           sy2:VGfloat; sx3:VGfloat; sy3:VGfloat; matrix:PVGfloat):VGUErrorCode; cdecl; external libOpenVG name 'vguComputeWarpQuadToSquare';
 function vguComputeWarpSquareToQuad(dx0:VGfloat; dy0:VGfloat; dx1:VGfloat; dy1:VGfloat; dx2:VGfloat; 
           dy2:VGfloat; dx3:VGfloat; dy3:VGfloat; matrix:PVGfloat):VGUErrorCode; cdecl; external libOpenVG name 'vguComputeWarpSquareToQuad';
 function vguComputeWarpQuadToQuad(dx0:VGfloat; dy0:VGfloat; dx1:VGfloat; dy1:VGfloat; dx2:VGfloat; 
           dy2:VGfloat; dx3:VGfloat; dy3:VGfloat; sx0:VGfloat; sy0:VGfloat; 
           sx1:VGfloat; sy1:VGfloat; sx2:VGfloat; sy2:VGfloat; sx3:VGfloat; 
           sy3:VGfloat; matrix:PVGfloat):VGUErrorCode; cdecl; external libOpenVG name 'vguComputeWarpQuadToQuad';

{From VG\vgext.h}
type
 PVGParamTypeKHR = ^VGParamTypeKHR;
 VGParamTypeKHR = VGint;
const 
 VG_MAX_AVERAGE_BLUR_DIMENSION_KHR = $116B;
 VG_AVERAGE_BLUR_DIMENSION_RESOLUTION_KHR = $116C;
 VG_MAX_AVERAGE_BLUR_ITERATIONS_KHR = $116D;
  
 VG_PARAM_TYPE_KHR_FORCE_SIZE = VG_MAX_ENUM;

 { VGEGLImageKHR is an opaque handle to an EGLImage  }
type
 PVGeglImageKHR = ^VGeglImageKHR;
 VGeglImageKHR = Pointer;

 function vgCreateEGLImageTargetKHR(image:VGeglImageKHR):VGImage; cdecl; external libOpenVG name 'vgCreateEGLImageTargetKHR';
 procedure vgIterativeAverageBlurKHR(dst:VGImage; src:VGImage; dimX:VGfloat; dimY:VGfloat; iterative:VGuint; 
            tilingMode:VGTilingMode); cdecl; external libOpenVG name 'vgIterativeAverageBlurKHR';
      
type
 PVGBlendModeKHR = ^VGBlendModeKHR;
 VGBlendModeKHR = VGint;
const 
 VG_BLEND_OVERLAY_KHR = $2010;
 VG_BLEND_HARDLIGHT_KHR = $2011;
 VG_BLEND_SOFTLIGHT_SVG_KHR = $2012;
 VG_BLEND_SOFTLIGHT_KHR = $2013;
 VG_BLEND_COLORDODGE_KHR = $2014;
 VG_BLEND_COLORBURN_KHR = $2015;
 VG_BLEND_DIFFERENCE_KHR = $2016;
 VG_BLEND_SUBTRACT_KHR = $2017;
 VG_BLEND_INVERT_KHR = $2018;
 VG_BLEND_EXCLUSION_KHR = $2019;
 VG_BLEND_LINEARDODGE_KHR = $201a;
 VG_BLEND_LINEARBURN_KHR = $201b;
 VG_BLEND_VIVIDLIGHT_KHR = $201c;
 VG_BLEND_LINEARLIGHT_KHR = $201d;
 VG_BLEND_PINLIGHT_KHR = $201e;
 VG_BLEND_HARDMIX_KHR = $201f;
 VG_BLEND_CLEAR_KHR = $2020;
 VG_BLEND_DST_KHR = $2021;
 VG_BLEND_SRC_OUT_KHR = $2022;
 VG_BLEND_DST_OUT_KHR = $2023;
 VG_BLEND_SRC_ATOP_KHR = $2024;
 VG_BLEND_DST_ATOP_KHR = $2025;
 VG_BLEND_XOR_KHR = $2026;
 
 VG_BLEND_MODE_KHR_FORCE_SIZE = VG_MAX_ENUM;
      
type
 PVGPfTypeKHR = ^VGPfTypeKHR;
 VGPfTypeKHR = VGint;
const 
 VG_PF_OBJECT_VISIBLE_FLAG_KHR = 1 shl 0;
 VG_PF_KNOCKOUT_FLAG_KHR = 1 shl 1;
 VG_PF_OUTER_FLAG_KHR = 1 shl 2;
 VG_PF_INNER_FLAG_KHR = 1 shl 3;
 
 VG_PF_TYPE_KHR_FORCE_SIZE = VG_MAX_ENUM;

type
 PVGUErrorCodeKHR = ^VGUErrorCodeKHR;
 VGUErrorCodeKHR = VGint;
const
 VGU_IMAGE_IN_USE_ERROR = $F010;
  
 VGU_ERROR_CODE_KHR_FORCE_SIZE = VG_MAX_ENUM;

 procedure vgParametricFilterKHR(dst:VGImage; src:VGImage; blur:VGImage; strength:VGfloat; offsetX:VGfloat; 
            offsetY:VGfloat; filterFlags:VGbitfield; highlightPaint:VGPaint; shadowPaint:VGPaint); cdecl; external libOpenVG name 'vgParametricFilterKHR';
 function vguDropShadowKHR(dst:VGImage; src:VGImage; dimX:VGfloat; dimY:VGfloat; iterative:VGuint; 
            strength:VGfloat; distance:VGfloat; angle:VGfloat; filterFlags:VGbitfield; allowedQuality:VGbitfield; 
            shadowColorRGBA:VGuint):VGUErrorCode; cdecl; external libOpenVG name 'vguDropShadowKHR';
 function vguGlowKHR(dst:VGImage; src:VGImage; dimX:VGfloat; dimY:VGfloat; iterative:VGuint; 
            strength:VGfloat; filterFlags:VGbitfield; allowedQuality:VGbitfield; glowColorRGBA:VGuint):VGUErrorCode; cdecl; external libOpenVG name 'vguGlowKHR';
 function vguBevelKHR(dst:VGImage; src:VGImage; dimX:VGfloat; dimY:VGfloat; iterative:VGuint; 
            strength:VGfloat; distance:VGfloat; angle:VGfloat; filterFlags:VGbitfield; allowedQuality:VGbitfield; 
            highlightColorRGBA:VGuint; shadowColorRGBA:VGuint):VGUErrorCode; cdecl; external libOpenVG name 'vguBevelKHR';
 function vguGradientGlowKHR(dst:VGImage; src:VGImage; dimX:VGfloat; dimY:VGfloat; iterative:VGuint; 
            strength:VGfloat; distance:VGfloat; angle:VGfloat; filterFlags:VGbitfield; allowedQuality:VGbitfield; 
            stopsCount:VGuint; glowColorRampStops:PVGfloat):VGUErrorCode; cdecl; external libOpenVG name 'vguGradientGlowKHR';
 function vguGradientBevelKHR(dst:VGImage; src:VGImage; dimX:VGfloat; dimY:VGfloat; iterative:VGuint; 
            strength:VGfloat; distance:VGfloat; angle:VGfloat; filterFlags:VGbitfield; allowedQuality:VGbitfield; 
            stopsCount:VGuint; bevelColorRampStops:PVGfloat):VGUErrorCode; cdecl; external libOpenVG name 'vguGradientBevelKHR';
      
type
 PVGPaintParamTypeNds = ^VGPaintParamTypeNds;
 VGPaintParamTypeNds = VGint;
const 
 VG_PAINT_COLOR_RAMP_LINEAR_NDS = $1A10;
 VG_COLOR_MATRIX_NDS = $1A11;
 VG_PAINT_COLOR_TRANSFORM_LINEAR_NDS = $1A12;
  
 VG_PAINT_PARAM_TYPE_NDS_FORCE_SIZE = VG_MAX_ENUM;

type
 PVGImageModeNds = ^VGImageModeNds;
 VGImageModeNds = VGint;
const 
 VG_DRAW_IMAGE_COLOR_MATRIX_NDS = $1F10;
  
 VG_IMAGE_MODE_NDS_FORCE_SIZE = VG_MAX_ENUM;
      
type
 PVGParamTypeNds = ^VGParamTypeNds;
 VGParamTypeNds = VGint;
const 
 VG_CLIP_MODE_NDS = $1180;
 VG_CLIP_LINES_NDS = $1181;
 VG_MAX_CLIP_LINES_NDS = $1182;
  
 VG_PARAM_TYPE_NDS_FORCE_SIZE = VG_MAX_ENUM;

type
 PVGClipModeNds = ^VGClipModeNds;
 VGClipModeNds = VGint;
const 
 VG_CLIPMODE_NONE_NDS = $3000;
 VG_CLIPMODE_CLIP_CLOSED_NDS = $3001;
 VG_CLIPMODE_CLIP_OPEN_NDS = $3002;
 VG_CLIPMODE_CULL_NDS = $3003;
  
 VG_CLIPMODE_NDS_FORCE_SIZE = VG_MAX_ENUM;

type
 PVGPathSegmentNds = ^VGPathSegmentNds;
 VGPathSegmentNds = VGint;
const 
 VG_RQUAD_TO_NDS = 13 shl 1;
 VG_RCUBIC_TO_NDS = 14 shl 1;
  
 VG_PATH_SEGMENT_NDS_FORCE_SIZE = VG_MAX_ENUM;

type
 PVGPathCommandNds = ^VGPathCommandNds;
 VGPathCommandNds = VGint;
const 
 VG_RQUAD_TO_ABS_NDS = VG_RQUAD_TO_NDS or VG_ABSOLUTE;
 VG_RQUAD_TO_REL_NDS = VG_RQUAD_TO_NDS or VG_RELATIVE;
 VG_RCUBIC_TO_ABS_NDS = VG_RCUBIC_TO_NDS or VG_ABSOLUTE;
 VG_RCUBIC_TO_REL_NDS = VG_RCUBIC_TO_NDS or VG_RELATIVE;

 VG_PATH_COMMAND_NDS_FORCE_SIZE = VG_MAX_ENUM;

 procedure vgProjectiveMatrixNDS(enable:VGboolean); cdecl; external libOpenVG name 'vgProjectiveMatrixNDS';
 function vguTransformClipLineNDS(Ain:VGfloat; Bin:VGfloat; Cin:VGfloat; matrix:PVGfloat; inverse:VGboolean; 
            Aout:PVGfloat; Bout:PVGfloat; Cout:PVGfloat):VGUErrorCode; cdecl; external libOpenVG name 'vguTransformClipLineNDS';
            
implementation

end.
