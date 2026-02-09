{EGL Headers:

  Ported to FreePascal by Garry Wood <garry@softoz.com.au>

 Original Copyright:

  ** Reference version of egl.h for EGL 1.4.
  **
  ** Copyright (c) 2007-2009 The Khronos Group Inc.
  **
  ** Permission is hereby granted, free of charge, to any person obtaining a
  ** copy of this software and/or associated documentation files (the
  ** "Materials"), to deal in the Materials without restriction, including
  ** without limitation the rights to use, copy, modify, merge, publish,
  ** distribute, sublicense, and/or sell copies of the Materials, and to
  ** permit persons to whom the Materials are furnished to do so, subject to
  ** the following conditions:
  **
  ** The above copyright notice and this permission notice shall be included
  ** in all copies or substantial portions of the Materials.
  **
  ** THE MATERIALS ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  ** EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
  ** MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
  ** IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
  ** CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
  ** TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
  ** MATERIALS OR THE USE OR OTHER DEALINGS IN THE MATERIALS.

}

{$IFNDEF FPC_DOTTEDUNITS}
unit EGL;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc} {Default to ObjFPC compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  {$ifdef ultibo}
  Core.GlobalTypes,
  {$endif}
  {$ifndef ultibo}
  System.DynLibs,
  {$endif}
  {$ifdef X}
  Api.X11.X,
  Api.X11.Xlib,
  {$endif}
  {$ifdef windows}
  WinApi.Windows,
  {$endif}
  {$ifdef ultibo}
  Core.Syscalls,
  {$endif}
  System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  {$ifdef ultibo}
  GlobalTypes,
  {$endif}
  {$ifndef ultibo}
  dynlibs,
  {$endif}
  {$ifdef X}
  x,
  xlib,
  {$endif}
  {$ifdef windows}
  Windows,
  {$endif}
  {$ifdef ultibo}
  Syscalls,
  {$endif}
  SysUtils;
{$ENDIF FPC_DOTTEDUNITS}

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

{$ifdef ultibo}
  {$linklib libEGL}
{$endif}

{From EGL\eglplatform.h}
type
  {$ifdef X}
  EGLNativeDisplayType = PDisplay;

  EGLNativeWindowType = TWindow;

  EGLNativePixmapType = TPixmap;
  {$else X}
  {$ifdef windows}
  EGLNativeDisplayType = HDC;

  EGLNativeWindowType = HWND;

  EGLNativePixmapType = HBITMAP;
  {$else windows}
  EGLNativeDisplayType = Pointer;

  EGLNativeWindowType = Pointer;

  EGLNativePixmapType = Pointer;
  {$endif windows}
  {$endif X}

  { Define EGLint. This must be a signed integral type large enough to contain
    all legal attribute names and values passed into and out of EGL, whether
    their type is boolean, bitmask, enumerant (symbolic constant), integer,
    handle, or other.  While in general a 32-bit integer will suffice, if
    handles are 64 bit types, then EGLint should be defined as a signed 64-bit
    integer type. }
  PEGLint  = ^EGLint;
  EGLint = {$ifdef win64}int64{$else}longint{$endif};

{From EGL\egl.h}
type
  PEGLBoolean = ^EGLBoolean;
  EGLBoolean = dword;

  PEGLenum = ^EGLenum;
  EGLenum = dword;

  PEGLConfig = ^EGLConfig;
  EGLConfig = pointer;

  PEGLContext = ^EGLContext;
  EGLContext = pointer;

  PEGLDisplay = ^EGLDisplay;
  EGLDisplay = pointer;

  PEGLSurface = ^EGLSurface;
  EGLSurface = pointer;

  PEGLClientBuffer = ^EGLClientBuffer;
  EGLClientBuffer = pointer;

  { EGL Versioning  }
const
  EGL_VERSION_1_0 = 1;
  EGL_VERSION_1_1 = 1;
  EGL_VERSION_1_2 = 1;
  EGL_VERSION_1_3 = 1;
  EGL_VERSION_1_4 = 1;

  { EGL Enumerants. Bitmasks and other exceptional cases aside, most
    enums are assigned unique values starting at 0x3000. }
  { EGL aliases  }
  EGL_FALSE = 0;
  EGL_TRUE = 1;
  { Out-of-band handle values  }
  EGL_DEFAULT_DISPLAY : EGLNativeDisplayType = nil;
  EGL_NO_CONTEXT : EGLContext = nil;
  EGL_NO_DISPLAY : EGLDisplay = nil;
  EGL_NO_SURFACE : EGLSurface = nil;

  { Out-of-band attribute value  }
  EGL_DONT_CARE : EGLint = -1;

  { Errors / GetError return values  }
const
  EGL_SUCCESS = $3000;
  EGL_NOT_INITIALIZED = $3001;
  EGL_BAD_ACCESS = $3002;
  EGL_BAD_ALLOC = $3003;
  EGL_BAD_ATTRIBUTE = $3004;
  EGL_BAD_CONFIG = $3005;
  EGL_BAD_CONTEXT = $3006;
  EGL_BAD_CURRENT_SURFACE = $3007;
  EGL_BAD_DISPLAY = $3008;
  EGL_BAD_MATCH = $3009;
  EGL_BAD_NATIVE_PIXMAP = $300A;
  EGL_BAD_NATIVE_WINDOW = $300B;
  EGL_BAD_PARAMETER = $300C;
  EGL_BAD_SURFACE = $300D;
  { EGL 1.1 - IMG_power_management  }
  EGL_CONTEXT_LOST = $300E;
  { Reserved 0x300F-0x301F for additional errors  }
  { Config attributes  }
  EGL_BUFFER_SIZE = $3020;
  EGL_ALPHA_SIZE = $3021;
  EGL_BLUE_SIZE = $3022;
  EGL_GREEN_SIZE = $3023;
  EGL_RED_SIZE = $3024;
  EGL_DEPTH_SIZE = $3025;
  EGL_STENCIL_SIZE = $3026;
  EGL_CONFIG_CAVEAT = $3027;
  EGL_CONFIG_ID = $3028;
  EGL_LEVEL = $3029;
  EGL_MAX_PBUFFER_HEIGHT = $302A;
  EGL_MAX_PBUFFER_PIXELS = $302B;
  EGL_MAX_PBUFFER_WIDTH = $302C;
  EGL_NATIVE_RENDERABLE = $302D;
  EGL_NATIVE_VISUAL_ID = $302E;
  EGL_NATIVE_VISUAL_TYPE = $302F;
  EGL_PRESERVED_RESOURCES = $3030;
  EGL_SAMPLES = $3031;
  EGL_SAMPLE_BUFFERS = $3032;
  EGL_SURFACE_TYPE = $3033;
  EGL_TRANSPARENT_TYPE = $3034;
  EGL_TRANSPARENT_BLUE_VALUE = $3035;
  EGL_TRANSPARENT_GREEN_VALUE = $3036;
  EGL_TRANSPARENT_RED_VALUE = $3037;
  { Attrib list terminator  }
  EGL_NONE = $3038;
  EGL_BIND_TO_TEXTURE_RGB = $3039;
  EGL_BIND_TO_TEXTURE_RGBA = $303A;
  EGL_MIN_SWAP_INTERVAL = $303B;
  EGL_MAX_SWAP_INTERVAL = $303C;
  EGL_LUMINANCE_SIZE = $303D;
  EGL_ALPHA_MASK_SIZE = $303E;
  EGL_COLOR_BUFFER_TYPE = $303F;
  EGL_RENDERABLE_TYPE = $3040;
  { Pseudo-attribute (not queryable)  }
  EGL_MATCH_NATIVE_PIXMAP = $3041;
  EGL_CONFORMANT = $3042;
  { Reserved 0x3041-0x304F for additional config attributes  }
  { Config attribute values  }
  { EGL_CONFIG_CAVEAT value  }
  EGL_SLOW_CONFIG = $3050;
  { EGL_CONFIG_CAVEAT value  }
  EGL_NON_CONFORMANT_CONFIG = $3051;
  { EGL_TRANSPARENT_TYPE value  }
  EGL_TRANSPARENT_RGB = $3052;
  { EGL_COLOR_BUFFER_TYPE value  }
  EGL_RGB_BUFFER = $308E;
  { EGL_COLOR_BUFFER_TYPE value  }
  EGL_LUMINANCE_BUFFER = $308F;
  { More config attribute values, for EGL_TEXTURE_FORMAT  }
  EGL_NO_TEXTURE = $305C;
  EGL_TEXTURE_RGB = $305D;
  EGL_TEXTURE_RGBA = $305E;
  EGL_TEXTURE_2D = $305F;
  { Config attribute mask bits  }
  { EGL_SURFACE_TYPE mask bits  }
  EGL_PBUFFER_BIT = $0001;
  { EGL_SURFACE_TYPE mask bits  }
  EGL_PIXMAP_BIT = $0002;
  { EGL_SURFACE_TYPE mask bits  }
  EGL_WINDOW_BIT = $0004;
  { EGL_SURFACE_TYPE mask bits  }
  EGL_VG_COLORSPACE_LINEAR_BIT = $0020;
  { EGL_SURFACE_TYPE mask bits  }
  EGL_VG_ALPHA_FORMAT_PRE_BIT = $0040;
  { EGL_SURFACE_TYPE mask bits  }
  EGL_MULTISAMPLE_RESOLVE_BOX_BIT = $0200;
  { EGL_SURFACE_TYPE mask bits  }
  EGL_SWAP_BEHAVIOR_PRESERVED_BIT = $0400;
  { EGL_RENDERABLE_TYPE mask bits  }
  EGL_OPENGL_ES_BIT = $0001;
  { EGL_RENDERABLE_TYPE mask bits  }
  EGL_OPENVG_BIT = $0002;
  { EGL_RENDERABLE_TYPE mask bits  }
  EGL_OPENGL_ES2_BIT = $0004;
  { EGL_RENDERABLE_TYPE mask bits  }
  EGL_OPENGL_BIT = $0008;
  { QueryString targets  }
  EGL_VENDOR = $3053;
  EGL_VERSION = $3054;
  EGL_EXTENSIONS = $3055;
  EGL_CLIENT_APIS = $308D;
  { QuerySurface / SurfaceAttrib / CreatePbufferSurface targets  }
  EGL_HEIGHT = $3056;
  EGL_WIDTH = $3057;
  EGL_LARGEST_PBUFFER = $3058;
  EGL_TEXTURE_FORMAT = $3080;
  EGL_TEXTURE_TARGET = $3081;
  EGL_MIPMAP_TEXTURE = $3082;
  EGL_MIPMAP_LEVEL = $3083;
  EGL_RENDER_BUFFER = $3086;
  EGL_VG_COLORSPACE = $3087;
  EGL_VG_ALPHA_FORMAT = $3088;
  EGL_HORIZONTAL_RESOLUTION = $3090;
  EGL_VERTICAL_RESOLUTION = $3091;
  EGL_PIXEL_ASPECT_RATIO = $3092;
  EGL_SWAP_BEHAVIOR = $3093;
  EGL_MULTISAMPLE_RESOLVE = $3099;
  { EGL_RENDER_BUFFER values / BindTexImage / ReleaseTexImage buffer targets  }
  EGL_BACK_BUFFER = $3084;
  EGL_SINGLE_BUFFER = $3085;
  { OpenVG color spaces  }
  { EGL_VG_COLORSPACE value  }
  EGL_VG_COLORSPACE_sRGB = $3089;
  { EGL_VG_COLORSPACE value  }
  EGL_VG_COLORSPACE_LINEAR = $308A;
  { OpenVG alpha formats  }
  { EGL_ALPHA_FORMAT value  }
  EGL_VG_ALPHA_FORMAT_NONPRE = $308B;
  { EGL_ALPHA_FORMAT value  }
  EGL_VG_ALPHA_FORMAT_PRE = $308C;
  { Constant scale factor by which fractional display resolutions &
    aspect ratio are scaled when queried as integer values. }
  EGL_DISPLAY_SCALING = 10000;
  { Unknown display resolution/aspect ratio  }
  { was #define dname def_expr }
  EGL_UNKNOWN : EGLint = -1;

  { Back buffer swap behaviors  }
  { EGL_SWAP_BEHAVIOR value  }
const
  EGL_BUFFER_PRESERVED = $3094;
  { EGL_SWAP_BEHAVIOR value  }
  EGL_BUFFER_DESTROYED = $3095;
  { CreatePbufferFromClientBuffer buffer types  }
  EGL_OPENVG_IMAGE = $3096;
  { QueryContext targets  }
  EGL_CONTEXT_CLIENT_TYPE = $3097;
  { CreateContext attributes  }
  EGL_CONTEXT_CLIENT_VERSION = $3098;
  { Multisample resolution behaviors  }
  { EGL_MULTISAMPLE_RESOLVE value  }
  EGL_MULTISAMPLE_RESOLVE_DEFAULT = $309A;
  { EGL_MULTISAMPLE_RESOLVE value  }
  EGL_MULTISAMPLE_RESOLVE_BOX = $309B;
  { BindAPI/QueryAPI targets  }
  EGL_OPENGL_ES_API = $30A0;
  EGL_OPENVG_API = $30A1;
  EGL_OPENGL_API = $30A2;
  { GetCurrentSurface targets  }
  EGL_DRAW = $3059;
  EGL_READ = $305A;
  { WaitNative engines  }
  EGL_CORE_NATIVE_ENGINE = $305B;
  { EGL 1.2 tokens renamed for consistency in EGL 1.3  }
  EGL_COLORSPACE = EGL_VG_COLORSPACE;
  EGL_ALPHA_FORMAT = EGL_VG_ALPHA_FORMAT;
  EGL_COLORSPACE_sRGB = EGL_VG_COLORSPACE_sRGB;
  EGL_COLORSPACE_LINEAR = EGL_VG_COLORSPACE_LINEAR;
  EGL_ALPHA_FORMAT_NONPRE = EGL_VG_ALPHA_FORMAT_NONPRE;
  EGL_ALPHA_FORMAT_PRE = EGL_VG_ALPHA_FORMAT_PRE;
  { EGL extensions must request enum blocks from the Khronos
    API Registrar, who maintains the enumerant registry. Submit
    a bug in Khronos Bugzilla against task "Registry". }
  { EGL Functions  }

{$ifdef ultibo}
const
  libEGL = 'EGL';

  function eglGetError:EGLint; cdecl; external libEGL name 'eglGetError';
  function eglGetDisplay(display_id:EGLNativeDisplayType):EGLDisplay; cdecl; external libEGL name 'eglGetDisplay';
  function eglInitialize(dpy:EGLDisplay; major:pEGLint; minor:pEGLint):EGLBoolean; cdecl; external libEGL name 'eglInitialize';
  function eglTerminate(dpy:EGLDisplay):EGLBoolean; cdecl; external libEGL name 'eglTerminate';
  function eglQueryString(dpy:EGLDisplay; name:EGLint):pchar; cdecl; external libEGL name 'eglQueryString';
  function eglGetConfigs(dpy:EGLDisplay; configs:pEGLConfig; config_size:EGLint; num_config:pEGLint):EGLBoolean; cdecl; external libEGL name 'eglGetConfigs';
  function eglChooseConfig(dpy:EGLDisplay; attrib_list:pEGLint; configs:pEGLConfig; config_size:EGLint; num_config:pEGLint):EGLBoolean; cdecl; external libEGL name 'eglChooseConfig';
  function eglGetConfigAttrib(dpy:EGLDisplay; config:EGLConfig; attribute:EGLint; value:pEGLint):EGLBoolean; cdecl; external libEGL name 'eglGetConfigAttrib';
  function eglCreateWindowSurface(dpy:EGLDisplay; config:EGLConfig; win:EGLNativeWindowType; attrib_list:pEGLint):EGLSurface; cdecl; external libEGL name 'eglCreateWindowSurface';
  function eglCreatePbufferSurface(dpy:EGLDisplay; config:EGLConfig; attrib_list:pEGLint):EGLSurface; cdecl; external libEGL name 'eglCreatePbufferSurface';
  function eglCreatePixmapSurface(dpy:EGLDisplay; config:EGLConfig; pixmap:EGLNativePixmapType; attrib_list:pEGLint):EGLSurface; cdecl; external libEGL name 'eglCreatePixmapSurface';
  function eglDestroySurface(dpy:EGLDisplay; surface:EGLSurface):EGLBoolean; cdecl; external libEGL name 'eglDestroySurface';
  function eglQuerySurface(dpy:EGLDisplay; surface:EGLSurface; attribute:EGLint; value:pEGLint):EGLBoolean; cdecl; external libEGL name 'eglQuerySurface';
  function eglBindAPI(api:EGLenum):EGLBoolean; cdecl; external libEGL name 'eglBindAPI';
  function eglQueryAPI:EGLenum; cdecl; external libEGL name 'eglQueryAPI';
  function eglWaitClient:EGLBoolean; cdecl; external libEGL name 'eglWaitClient';
  function eglReleaseThread:EGLBoolean; cdecl; external libEGL name 'eglReleaseThread';
  function eglCreatePbufferFromClientBuffer(dpy:EGLDisplay; buftype:EGLenum; buffer:EGLClientBuffer; config:EGLConfig; attrib_list:pEGLint):EGLSurface; cdecl; external libEGL name 'eglCreatePbufferFromClientBuffer';
  function eglSurfaceAttrib(dpy:EGLDisplay; surface:EGLSurface; attribute:EGLint; value:EGLint):EGLBoolean; cdecl; external libEGL name 'eglSurfaceAttrib';
  function eglBindTexImage(dpy:EGLDisplay; surface:EGLSurface; buffer:EGLint):EGLBoolean; cdecl; external libEGL name 'eglBindTexImage';
  function eglReleaseTexImage(dpy:EGLDisplay; surface:EGLSurface; buffer:EGLint):EGLBoolean; cdecl; external libEGL name 'eglReleaseTexImage';
  function eglSwapInterval(dpy:EGLDisplay; interval:EGLint):EGLBoolean; cdecl; external libEGL name 'eglSwapInterval';
  function eglCreateContext(dpy:EGLDisplay; config:EGLConfig; share_context:EGLContext; attrib_list:pEGLint):EGLContext; cdecl; external libEGL name 'eglCreateContext';
  function eglDestroyContext(dpy:EGLDisplay; ctx:EGLContext):EGLBoolean; cdecl; external libEGL name 'eglDestroyContext';
  function eglMakeCurrent(dpy:EGLDisplay; draw:EGLSurface; read:EGLSurface; ctx:EGLContext):EGLBoolean; cdecl; external libEGL name 'eglMakeCurrent';
  function eglGetCurrentContext:EGLContext; cdecl; external libEGL name 'eglGetCurrentContext';
  function eglGetCurrentSurface(readdraw:EGLint):EGLSurface; cdecl; external libEGL name 'eglGetCurrentSurface';
  function eglGetCurrentDisplay:EGLDisplay; cdecl; external libEGL name 'eglGetCurrentDisplay';
  function eglQueryContext(dpy:EGLDisplay; ctx:EGLContext; attribute:EGLint; value:pEGLint):EGLBoolean; cdecl; external libEGL name 'eglQueryContext';
  function eglWaitGL:EGLBoolean; cdecl; external libEGL name 'eglWaitGL';
  function eglWaitNative(engine:EGLint):EGLBoolean; cdecl; external libEGL name 'eglWaitNative';
  function eglSwapBuffers(dpy:EGLDisplay; surface:EGLSurface):EGLBoolean; cdecl; external libEGL name 'eglSwapBuffers';
  function eglCopyBuffers(dpy:EGLDisplay; surface:EGLSurface; target:EGLNativePixmapType):EGLBoolean; cdecl; external libEGL name 'eglCopyBuffers';

  { This is a generic function pointer type, whose name indicates it must
    be cast to the proper type *and calling convention* before use. }
type
  __eglMustCastToProperFunctionPointerType = procedure (_para1:pointer); cdecl;

  { Now, define eglGetProcAddress using the generic function ptr. type  }
  function eglGetProcAddress(procname:pchar):__eglMustCastToProperFunctionPointerType; cdecl; external libEGL name 'eglGetProcAddress';
{$else}
var
    eglGetError : function:EGLint;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglGetDisplay : function(display_id:EGLNativeDisplayType):EGLDisplay;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglInitialize : function(dpy:EGLDisplay; major:pEGLint; minor:pEGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglTerminate : function(dpy:EGLDisplay):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglQueryString : function(dpy:EGLDisplay; name:EGLint):pchar;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglGetConfigs : function(dpy:EGLDisplay; configs:pEGLConfig; config_size:EGLint; num_config:pEGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglChooseConfig : function(dpy:EGLDisplay; attrib_list:pEGLint; configs:pEGLConfig; config_size:EGLint; num_config:pEGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglGetConfigAttrib : function(dpy:EGLDisplay; config:EGLConfig; attribute:EGLint; value:pEGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglCreateWindowSurface : function(dpy:EGLDisplay; config:EGLConfig; win:EGLNativeWindowType; attrib_list:pEGLint):EGLSurface;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglCreatePbufferSurface : function(dpy:EGLDisplay; config:EGLConfig; attrib_list:pEGLint):EGLSurface;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglCreatePixmapSurface : function(dpy:EGLDisplay; config:EGLConfig; pixmap:EGLNativePixmapType; attrib_list:pEGLint):EGLSurface;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglDestroySurface : function(dpy:EGLDisplay; surface:EGLSurface):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglQuerySurface : function(dpy:EGLDisplay; surface:EGLSurface; attribute:EGLint; value:pEGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglBindAPI : function(api:EGLenum):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglQueryAPI : function:EGLenum;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglWaitClient : function:EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglReleaseThread : function:EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglCreatePbufferFromClientBuffer : function(dpy:EGLDisplay; buftype:EGLenum; buffer:EGLClientBuffer; config:EGLConfig; attrib_list:pEGLint):EGLSurface;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglSurfaceAttrib : function(dpy:EGLDisplay; surface:EGLSurface; attribute:EGLint; value:EGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglBindTexImage : function(dpy:EGLDisplay; surface:EGLSurface; buffer:EGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglReleaseTexImage : function(dpy:EGLDisplay; surface:EGLSurface; buffer:EGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglSwapInterval : function(dpy:EGLDisplay; interval:EGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglCreateContext : function(dpy:EGLDisplay; config:EGLConfig; share_context:EGLContext; attrib_list:pEGLint):EGLContext;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglDestroyContext : function(dpy:EGLDisplay; ctx:EGLContext):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglMakeCurrent : function(dpy:EGLDisplay; draw:EGLSurface; read:EGLSurface; ctx:EGLContext):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglGetCurrentContext : function:EGLContext;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglGetCurrentSurface : function(readdraw:EGLint):EGLSurface;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglGetCurrentDisplay : function:EGLDisplay;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglQueryContext : function(dpy:EGLDisplay; ctx:EGLContext; attribute:EGLint; value:pEGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglWaitGL : function:EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglWaitNative : function(engine:EGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglSwapBuffers : function(dpy:EGLDisplay; surface:EGLSurface):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
    eglCopyBuffers : function(dpy:EGLDisplay; surface:EGLSurface; target:EGLNativePixmapType):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  { This is a generic function pointer type, whose name indicates it must
    be cast to the proper type *and calling convention* before use. }
type
  __eglMustCastToProperFunctionPointerType = procedure (_para1:pointer);{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  { Now, define eglGetProcAddress using the generic function ptr. type  }
var
  eglGetProcAddress : function(procname:pchar):__eglMustCastToProperFunctionPointerType;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
{$endif}

{From EGL\eglext.h}
  { Header file version number  }
  { Current version at http://www.khronos.org/registry/egl/  }
const
  EGL_EGLEXT_VERSION = 3;
  EGL_KHR_config_attribs = 1;
  { EGLConfig attribute  }
  EGL_CONFORMANT_KHR = $3042;
  { EGL_SURFACE_TYPE bitfield  }
  EGL_VG_COLORSPACE_LINEAR_BIT_KHR = $0020;
  { EGL_SURFACE_TYPE bitfield  }
  EGL_VG_ALPHA_FORMAT_PRE_BIT_KHR = $0040;
  EGL_KHR_lock_surface = 1;
  { EGL_LOCK_USAGE_HINT_KHR bitfield  }
  EGL_READ_SURFACE_BIT_KHR = $0001;
  { EGL_LOCK_USAGE_HINT_KHR bitfield  }
  EGL_WRITE_SURFACE_BIT_KHR = $0002;
  { EGL_SURFACE_TYPE bitfield  }
  EGL_LOCK_SURFACE_BIT_KHR = $0080;
  { EGL_SURFACE_TYPE bitfield  }
  EGL_OPTIMAL_FORMAT_BIT_KHR = $0100;
  { EGLConfig attribute  }
  EGL_MATCH_FORMAT_KHR = $3043;
  { EGL_MATCH_FORMAT_KHR value  }
  EGL_FORMAT_RGB_565_EXACT_KHR = $30C0;
  { EGL_MATCH_FORMAT_KHR value  }
  EGL_FORMAT_RGB_565_KHR = $30C1;
  { EGL_MATCH_FORMAT_KHR value  }
  EGL_FORMAT_RGBA_8888_EXACT_KHR = $30C2;
  { EGL_MATCH_FORMAT_KHR value  }
  EGL_FORMAT_RGBA_8888_KHR = $30C3;
  { eglLockSurfaceKHR attribute  }
  EGL_MAP_PRESERVE_PIXELS_KHR = $30C4;
  { eglLockSurfaceKHR attribute  }
  EGL_LOCK_USAGE_HINT_KHR = $30C5;
  { eglQuerySurface attribute  }
  EGL_BITMAP_POINTER_KHR = $30C6;
  { eglQuerySurface attribute  }
  EGL_BITMAP_PITCH_KHR = $30C7;
  { eglQuerySurface attribute  }
  EGL_BITMAP_ORIGIN_KHR = $30C8;
  { eglQuerySurface attribute  }
  EGL_BITMAP_PIXEL_RED_OFFSET_KHR = $30C9;
  { eglQuerySurface attribute  }
  EGL_BITMAP_PIXEL_GREEN_OFFSET_KHR = $30CA;
  { eglQuerySurface attribute  }
  EGL_BITMAP_PIXEL_BLUE_OFFSET_KHR = $30CB;
  { eglQuerySurface attribute  }
  EGL_BITMAP_PIXEL_ALPHA_OFFSET_KHR = $30CC;
  { eglQuerySurface attribute  }
  EGL_BITMAP_PIXEL_LUMINANCE_OFFSET_KHR = $30CD;
  { EGL_BITMAP_ORIGIN_KHR value  }
  EGL_LOWER_LEFT_KHR = $30CE;
  { EGL_BITMAP_ORIGIN_KHR value  }
  EGL_UPPER_LEFT_KHR = $30CF;

const
  EGL_KHR_image = 1;
  { eglCreateImageKHR target  }
  EGL_NATIVE_PIXMAP_KHR = $30B0;

type
  EGLImageKHR = Pointer;

const
  EGL_NO_IMAGE_KHR : EGLImageKHR = nil;

const
  EGL_KHR_vg_parent_image = 1;
  { eglCreateImageKHR target  }
  EGL_VG_PARENT_IMAGE_KHR = $30BA;
  EGL_KHR_gl_texture_2D_image = 1;
  { eglCreateImageKHR target  }
  EGL_GL_TEXTURE_2D_KHR = $30B1;
  { eglCreateImageKHR attribute  }
  EGL_GL_TEXTURE_LEVEL_KHR = $30BC;
  EGL_KHR_gl_texture_cubemap_image = 1;
  { eglCreateImageKHR target  }
  EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_X_KHR = $30B3;
  { eglCreateImageKHR target  }
  EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_X_KHR = $30B4;
  { eglCreateImageKHR target  }
  EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_Y_KHR = $30B5;
  { eglCreateImageKHR target  }
  EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_KHR = $30B6;
  { eglCreateImageKHR target  }
  EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_Z_KHR = $30B7;
  { eglCreateImageKHR target  }
  EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_KHR = $30B8;
  EGL_KHR_gl_texture_3D_image = 1;
  { eglCreateImageKHR target  }
  EGL_GL_TEXTURE_3D_KHR = $30B2;
  { eglCreateImageKHR attribute  }
  EGL_GL_TEXTURE_ZOFFSET_KHR = $30BD;
  EGL_KHR_gl_renderbuffer_image = 1;
  { eglCreateImageKHR target  }
  EGL_GL_RENDERBUFFER_KHR = $30B9;
  EGL_KHR_image_base = 1;
  { Most interfaces defined by EGL_KHR_image_pixmap above  }
  { eglCreateImageKHR attribute  }
  EGL_IMAGE_PRESERVED_KHR = $30D2;
  EGL_KHR_image_pixmap = 1;
  { Interfaces defined by EGL_KHR_image above  }
  EGL_KHR_fence_sync = 1;

type
  { EGLSyncKHR is an opaque handle to an EGL sync object }
  EGLSyncKHR = Pointer;
  { EGLTimeKHR is a 64-bit unsigned integer representing intervals
   in nanoseconds.}
  EGLTimeKHR = Qword;

const
  EGL_SYNC_FENCE_KHR = $30F9;
  EGL_SYNC_REUSABLE_KHR = $30FA;
  EGL_SYNC_TYPE_KHR = $30F7;
  EGL_SYNC_STATUS_KHR = $30F1;
  EGL_SYNC_CONDITION_KHR = $30F8;
  EGL_SIGNALED_KHR = $30F2;
  EGL_UNSIGNALED_KHR = $30F3;
  EGL_SYNC_PRIOR_COMMANDS_COMPLETE_KHR = $30F0;
  EGL_SYNC_FLUSH_COMMANDS_BIT_KHR = $0001;
  EGL_TIMEOUT_EXPIRED_KHR = $30F5;
  EGL_CONDITION_SATISFIED_KHR = $30F6;

  EGL_FOREVER_KHR = $FFFFFFFFFFFFFFFF;

  EGL_NO_SYNC_KHR : EGLSyncKHR = nil;

{$ifdef ultibo}
  function eglCreateSyncKHR(dpy:EGLDisplay; _type:EGLenum; attrib_list:PEGLint):EGLSyncKHR; cdecl; external libEGL name 'eglCreateSyncKHR';
  function eglDestroySyncKHR(dpy:EGLDisplay; sync:EGLSyncKHR):EGLBoolean; cdecl; external libEGL name 'eglDestroySyncKHR';
  function eglClientWaitSyncKHR(dpy:EGLDisplay; sync:EGLSyncKHR; flags:EGLint; timeout:EGLTimeKHR):EGLint; cdecl; external libEGL name 'eglClientWaitSyncKHR';
  function eglSignalSyncKHR(dpy:EGLDisplay; sync:EGLSyncKHR; mode:EGLenum):EGLBoolean; cdecl; external libEGL name 'eglSignalSyncKHR';
  function eglGetSyncAttribKHR(dpy:EGLDisplay; sync:EGLSyncKHR; attribute:EGLint; value:PEGLint):EGLBoolean; cdecl; external libEGL name 'eglGetSyncAttribKHR';
{$else}
var
  eglCreateSyncKHR : function(dpy:EGLDisplay; _type:EGLenum; attrib_list:PEGLint):EGLSyncKHR;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  eglDestroySyncKHR : function(dpy:EGLDisplay; sync:EGLSyncKHR):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  eglClientWaitSyncKHR : function(dpy:EGLDisplay; sync:EGLSyncKHR; flags:EGLint; timeout:EGLTimeKHR):EGLint;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  eglSignalSyncKHR : function(dpy:EGLDisplay; sync:EGLSyncKHR; mode:EGLenum):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
  eglGetSyncAttribKHR : function(dpy:EGLDisplay; sync:EGLSyncKHR; attribute:EGLint; value:PEGLint):EGLBoolean;{$ifdef windows}stdcall;{$else}cdecl;{$endif}
{$endif}

{$ifndef ultibo}
function eglGetProcedure(ahlib:tlibhandle;ProcName:pchar):pointer;
{$endif}

implementation

{$ifndef ultibo}
  function eglGetProcedure(ahlib:tlibhandle;ProcName:pchar):pointer;
  begin
    result:=dynlibs.GetProcAddress(ahlib,ProcName);
    if assigned(eglGetProcAddress) and not assigned(result) then
      result:=eglGetProcAddress(ProcName);
  end;

  var
    EGLLib : tlibhandle;

  procedure FreeEGL;
  begin
    if EGLLib<>0 then
      FreeLibrary(EGLLib);

    eglGetError:=nil;
    eglGetDisplay:=nil;
    eglInitialize:=nil;
    eglTerminate:=nil;
    eglQueryString:=nil;
    eglGetConfigs:=nil;
    eglChooseConfig:=nil;
    eglGetConfigAttrib:=nil;
    eglCreateWindowSurface:=nil;
    eglCreatePbufferSurface:=nil;
    eglCreatePixmapSurface:=nil;
    eglDestroySurface:=nil;
    eglQuerySurface:=nil;
    eglBindAPI:=nil;
    eglQueryAPI:=nil;
    eglWaitClient:=nil;
    eglReleaseThread:=nil;
    eglCreatePbufferFromClientBuffer:=nil;
    eglSurfaceAttrib:=nil;
    eglBindTexImage:=nil;
    eglReleaseTexImage:=nil;
    eglSwapInterval:=nil;
    eglCreateContext:=nil;
    eglDestroyContext:=nil;
    eglMakeCurrent:=nil;
    eglGetCurrentContext:=nil;
    eglGetCurrentSurface:=nil;
    eglGetCurrentDisplay:=nil;
    eglQueryContext:=nil;
    eglWaitGL:=nil;
    eglWaitNative:=nil;
    eglSwapBuffers:=nil;
    eglCopyBuffers:=nil;
    eglGetProcAddress:=nil;

    eglCreateSyncKHR:=nil;
    eglDestroySyncKHR:=nil;
    eglClientWaitSyncKHR:=nil;
    eglSignalSyncKHR:=nil;
    eglGetSyncAttribKHR:=nil;
  end;

  procedure LoadEGL(lib : pchar);
  begin
    FreeEGL;
    EGLLib:=dynlibs.LoadLibrary(lib);
    if EGLLib=0 then
      raise Exception.Create(format('Could not load library: %s',[lib]));

    pointer(eglGetProcAddress):=GetProcAddress(EGLLib,'glGetProcAddress');

    pointer(eglGetError):=eglGetProcedure(EGLLib,'eglGetError');
    pointer(eglGetDisplay):=eglGetProcedure(EGLLib,'eglGetDisplay');
    pointer(eglInitialize):=eglGetProcedure(EGLLib,'eglInitialize');
    pointer(eglTerminate):=eglGetProcedure(EGLLib,'eglTerminate');
    pointer(eglQueryString):=eglGetProcedure(EGLLib,'eglQueryString');
    pointer(eglGetConfigs):=eglGetProcedure(EGLLib,'eglGetConfigs');
    pointer(eglChooseConfig):=eglGetProcedure(EGLLib,'eglChooseConfig');
    pointer(eglGetConfigAttrib):=eglGetProcedure(EGLLib,'eglGetConfigAttrib');
    pointer(eglCreateWindowSurface):=eglGetProcedure(EGLLib,'eglCreateWindowSurface');
    pointer(eglCreatePbufferSurface):=eglGetProcedure(EGLLib,'eglCreatePbufferSurface');
    pointer(eglCreatePixmapSurface):=eglGetProcedure(EGLLib,'eglCreatePixmapSurface');
    pointer(eglDestroySurface):=eglGetProcedure(EGLLib,'eglDestroySurface');
    pointer(eglQuerySurface):=eglGetProcedure(EGLLib,'eglQuerySurface');
    pointer(eglBindAPI):=eglGetProcedure(EGLLib,'eglBindAPI');
    pointer(eglQueryAPI):=eglGetProcedure(EGLLib,'eglQueryAPI');
    pointer(eglWaitClient):=eglGetProcedure(EGLLib,'eglWaitClient');
    pointer(eglReleaseThread):=eglGetProcedure(EGLLib,'eglReleaseThread');
    pointer(eglCreatePbufferFromClientBuffer):=eglGetProcedure(EGLLib,'eglCreatePbufferFromClientBuffer');
    pointer(eglSurfaceAttrib):=eglGetProcedure(EGLLib,'eglSurfaceAttrib');
    pointer(eglBindTexImage):=eglGetProcedure(EGLLib,'eglBindTexImage');
    pointer(eglReleaseTexImage):=eglGetProcedure(EGLLib,'eglReleaseTexImage');
    pointer(eglSwapInterval):=eglGetProcedure(EGLLib,'eglSwapInterval');
    pointer(eglCreateContext):=eglGetProcedure(EGLLib,'eglCreateContext');
    pointer(eglDestroyContext):=eglGetProcedure(EGLLib,'eglDestroyContext');
    pointer(eglMakeCurrent):=eglGetProcedure(EGLLib,'eglMakeCurrent');
    pointer(eglGetCurrentContext):=eglGetProcedure(EGLLib,'eglGetCurrentContext');
    pointer(eglGetCurrentSurface):=eglGetProcedure(EGLLib,'eglGetCurrentSurface');
    pointer(eglGetCurrentDisplay):=eglGetProcedure(EGLLib,'eglGetCurrentDisplay');
    pointer(eglQueryContext):=eglGetProcedure(EGLLib,'eglQueryContext');
    pointer(eglWaitGL):=eglGetProcedure(EGLLib,'eglWaitGL');
    pointer(eglWaitNative):=eglGetProcedure(EGLLib,'eglWaitNative');
    pointer(eglSwapBuffers):=eglGetProcedure(EGLLib,'eglSwapBuffers');
    pointer(eglCopyBuffers):=eglGetProcedure(EGLLib,'eglCopyBuffers');

    pointer(eglCreateSyncKHR):=eglGetProcedure(EGLLib,'eglCreateSyncKHR');
    pointer(eglDestroySyncKHR):=eglGetProcedure(EGLLib,'eglDestroySyncKHR');
    pointer(eglClientWaitSyncKHR):=eglGetProcedure(EGLLib,'eglClientWaitSyncKHR');
    pointer(eglSignalSyncKHR):=eglGetProcedure(EGLLib,'eglSignalSyncKHR');
    pointer(eglGetSyncAttribKHR):=eglGetProcedure(EGLLib,'eglGetSyncAttribKHR');
  end;
{$endif}

initialization
{$ifndef ultibo}
  EGLLib:=0;
  LoadEGL({$ifdef windows}'libEGL.dll'{$else}'libEGL.so'{$endif});
{$endif}
finalization
{$ifndef ultibo}
  FreeEGL;
{$endif}
end.

