{DispmanX Headers:
 
  Ported to FreePascal by Garry Wood <garry@softoz.com.au>
 
 Original Copyright:
 
  Copyright (c) 2012, Broadcom Europe Ltd
  All rights reserved.
  
  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:
      * Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.
      * Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.
      * Neither the name of the copyright holder nor the
        names of its contributors may be used to endorse or promote products
        derived from this software without specific prior written permission.
  
  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY
  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  
 }

{$IFNDEF FPC_DOTTEDUNITS}
unit DispmanX;
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
  Drivers.VC4,
  {$endif}
  System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  {$ifdef ultibo}
  GlobalTypes,
  Syscalls,
  VC4,
  {$endif}
  SysUtils;
{$ENDIF FPC_DOTTEDUNITS}
 
{$PACKRECORDS C}

const
 libvchostif = 'vchostif';

{$linklib libvchostif}

{$ifndef ultibo}
type
 int32_t = Longint;
 
 uint8_t = Byte;
 uint32_t = LongWord;
 Puint32_t = ^uint32_t;
{$endif}

{From interface\vmcs_host\vc_dispmanx_types.h}
const
 VC_DISPMANX_VERSION = 1;    

 { Opaque handles }
type
 PDISPMANX_DISPLAY_HANDLE_T = ^DISPMANX_DISPLAY_HANDLE_T;
 DISPMANX_DISPLAY_HANDLE_T = uint32_t;

 PDISPMANX_UPDATE_HANDLE_T = ^DISPMANX_UPDATE_HANDLE_T;
 DISPMANX_UPDATE_HANDLE_T = uint32_t;

 PDISPMANX_ELEMENT_HANDLE_T = ^DISPMANX_ELEMENT_HANDLE_T;
 DISPMANX_ELEMENT_HANDLE_T = uint32_t;

 PDISPMANX_RESOURCE_HANDLE_T = ^DISPMANX_RESOURCE_HANDLE_T;
 DISPMANX_RESOURCE_HANDLE_T = uint32_t;

 PDISPMANX_PROTECTION_T = ^DISPMANX_PROTECTION_T;
 DISPMANX_PROTECTION_T = uint32_t;

const
 DISPMANX_NO_HANDLE = 0;   
 
 DISPMANX_PROTECTION_MAX = $0f;    
 DISPMANX_PROTECTION_NONE = 0;    
 DISPMANX_PROTECTION_HDCP = 11;    { Derived from the WM DRM levels, 101-300 }
 
 
 { Default display IDs.
   Note: if you overwrite with your own dispmanx_platform_init function, you
   should use IDs you provided during dispmanx_display_attach. }
 DISPMANX_ID_MAIN_LCD = 0;    
 DISPMANX_ID_AUX_LCD = 1;    
 DISPMANX_ID_HDMI = 2;    
 DISPMANX_ID_SDTV = 3;    
 DISPMANX_ID_FORCE_LCD = 4;    
 DISPMANX_ID_FORCE_TV = 5;    
 DISPMANX_ID_FORCE_OTHER = 6;  { non-default display  }
 
type
 PDISPMANX_STATUS_T = ^DISPMANX_STATUS_T;
 DISPMANX_STATUS_T = int32_t; 
const
 { Return codes. Nonzero ones indicate failure. }
 DISPMANX_SUCCESS = 0;
 DISPMANX_INVALID = -1;
 { XXX others TBA  }
 
type
 PDISPMANX_TRANSFORM_T = ^DISPMANX_TRANSFORM_T;
 DISPMANX_TRANSFORM_T = uint32_t;
const
 { Bottom 2 bits sets the orientation  }
 DISPMANX_NO_ROTATE = 0;
 DISPMANX_ROTATE_90 = 1;
 DISPMANX_ROTATE_180 = 2;
 DISPMANX_ROTATE_270 = 3;
 
 DISPMANX_FLIP_HRIZ = 1 shl 16;
 DISPMANX_FLIP_VERT = 1 shl 17;
 
 { invert left/right images  }
 DISPMANX_STEREOSCOPIC_INVERT = 1 shl 19;
 
 { extra flags for controlling 3d duplication behaviour  }
 DISPMANX_STEREOSCOPIC_NONE = 0 shl 20;
 DISPMANX_STEREOSCOPIC_MONO = 1 shl 20;
 DISPMANX_STEREOSCOPIC_SBS = 2 shl 20;
 DISPMANX_STEREOSCOPIC_TB = 3 shl 20;
 DISPMANX_STEREOSCOPIC_MASK = 15 shl 20;
 
 { extra flags for controlling snapshot behaviour  }
 DISPMANX_SNAPSHOT_NO_YUV = 1 shl 24;
 DISPMANX_SNAPSHOT_NO_RGB = 1 shl 25;
 DISPMANX_SNAPSHOT_FILL = 1 shl 26;
 DISPMANX_SNAPSHOT_SWAP_RED_BLUE = 1 shl 27;
 DISPMANX_SNAPSHOT_PACK = 1 shl 28;

type
 PDISPMANX_FLAGS_ALPHA_T = ^DISPMANX_FLAGS_ALPHA_T;
 DISPMANX_FLAGS_ALPHA_T = uint32_t;
const 
 { Bottom 2 bits sets the alpha mode  }
 DISPMANX_FLAGS_ALPHA_FROM_SOURCE = 0;
 DISPMANX_FLAGS_ALPHA_FIXED_ALL_PIXELS = 1;
 DISPMANX_FLAGS_ALPHA_FIXED_NON_ZERO = 2;
 DISPMANX_FLAGS_ALPHA_FIXED_EXCEED_0X07 = 3;
  
 DISPMANX_FLAGS_ALPHA_PREMULT = 1 shl 16;
 DISPMANX_FLAGS_ALPHA_MIX = 1 shl 17;

type
 PDISPMANX_ALPHA_T = ^DISPMANX_ALPHA_T;
 DISPMANX_ALPHA_T = record
  flags : DISPMANX_FLAGS_ALPHA_T;
  opacity : uint32_t;
  mask : PVC_IMAGE_T;
 end;

 PVC_DISPMANX_ALPHA_T = ^VC_DISPMANX_ALPHA_T; { for use with vmcs_host  }
 VC_DISPMANX_ALPHA_T = record
  flags : DISPMANX_FLAGS_ALPHA_T;
  opacity : uint32_t;
  mask : DISPMANX_RESOURCE_HANDLE_T; 
 end;
  
type
 PDISPMANX_FLAGS_CLAMP_T = ^DISPMANX_FLAGS_CLAMP_T;
 DISPMANX_FLAGS_CLAMP_T = uint32_t;
const 
 DISPMANX_FLAGS_CLAMP_NONE = 0;
 DISPMANX_FLAGS_CLAMP_LUMA_TRANSPARENT = 1;
 {DISPMANX_FLAGS_CLAMP_TRANSPARENT = 2;}
 {DISPMANX_FLAGS_CLAMP_REPLACE = 3;}
 DISPMANX_FLAGS_CLAMP_CHROMA_TRANSPARENT = 2;
 DISPMANX_FLAGS_CLAMP_TRANSPARENT = 3;

type
 PDISPMANX_FLAGS_KEYMASK_T = ^DISPMANX_FLAGS_KEYMASK_T;
 DISPMANX_FLAGS_KEYMASK_T = uint32_t;
const 
 DISPMANX_FLAGS_KEYMASK_OVERRIDE = 1;
 DISPMANX_FLAGS_KEYMASK_SMOOTH = 1 shl 1;
 DISPMANX_FLAGS_KEYMASK_CR_INV = 1 shl 2;
 DISPMANX_FLAGS_KEYMASK_CB_INV = 1 shl 3;
 DISPMANX_FLAGS_KEYMASK_YY_INV = 1 shl 4;

type
 PDISPMANX_CLAMP_KEYS_T = ^DISPMANX_CLAMP_KEYS_T;
 DISPMANX_CLAMP_KEYS_T = record
  case longint of
   0 : ( yuv : record
    yy_upper : uint8_t;
    yy_lower : uint8_t;
    cr_upper : uint8_t;
    cr_lower : uint8_t;
    cb_upper : uint8_t;
    cb_lower : uint8_t;
   end );
   1 : ( rgb : record
    red_upper : uint8_t;
    red_lower : uint8_t;
    blue_upper : uint8_t;
    blue_lower : uint8_t;
    green_upper : uint8_t;
    green_lower : uint8_t;
   end );
 end;

 PDISPMANX_CLAMP_T = ^DISPMANX_CLAMP_T;
 DISPMANX_CLAMP_T = record
  mode : DISPMANX_FLAGS_CLAMP_T;
  key_mask : DISPMANX_FLAGS_KEYMASK_T;
  key_value : DISPMANX_CLAMP_KEYS_T;
  replace_value : uint32_t;
 end;

 PDISPMANX_MODEINFO_T = ^DISPMANX_MODEINFO_T;
 DISPMANX_MODEINFO_T = record
  width : int32_t;
  height : int32_t;
  transform : DISPMANX_TRANSFORM_T;
  input_format : DISPLAY_INPUT_FORMAT_T;
  display_num : uint32_t;
 end;

 { Update callback.  }
 DISPMANX_CALLBACK_FUNC_T = procedure (u:DISPMANX_UPDATE_HANDLE_T; arg:pointer); cdecl;
 
 { Progress callback  }
 DISPMANX_PROGRESS_CALLBACK_FUNC_T = procedure (u:DISPMANX_UPDATE_HANDLE_T; line:uint32_t; arg:pointer); cdecl;
 
 { Pluggable display interface  }
 Pint32_t_array3 = ^int32_t_array3;
 int32_t_array3 = array[0..2] of int32_t;

 Puint32_t_array3 = ^uint32_t_array3;
 uint32_t_array3 = array[0..2] of uint32_t;
 
 Ptag_DISPMANX_DISPLAY_FUNCS_T = ^tag_DISPMANX_DISPLAY_FUNCS_T;
 tag_DISPMANX_DISPLAY_FUNCS_T = record
  {Get essential HVS configuration to be passed to the HVS driver. Options
   is any combination of the following flags: HVS_ONESHOT, HVS_FIFOREG,
   HVS_FIFO32, HVS_AUTOHSTART, HVS_INTLACE; and if HVS_FIFOREG, one of;
   ( HVS_FMT_RGB888, HVS_FMT_RGB565, HVS_FMT_RGB666, HVS_FMT_YUV) }
  get_hvs_config : function (instance:pointer; pchan:Puint32_t; poptions:Puint32_t; info:PDISPLAY_INFO_T; bg_colour:Puint32_t; 
                    test_mode:Puint32_t):int32_t; cdecl;
  
  {Get optional HVS configuration for gamma tables, OLED matrix and dither controls.
   Set these function pointers to NULL if the relevant features are not required}
  get_gamma_params : function (instance:pointer; gain:Pint32_t_array3; offset:Pint32_t_array3; gamma:Pint32_t_array3):int32_t; cdecl;
  get_oled_params : function (instance:pointer; poffsets:Puint32_t; coeffs:Puint32_t_array3):int32_t; cdecl;
  get_dither : function (instance:pointer; dither_depth:Puint32_t; dither_type:Puint32_t):int32_t; cdecl;
  
  {Get mode information, which may be returned to the applications as a courtesy.
   Transform should be set to 0, and (width,height) should be final dimensions}
  get_info : function (instance:pointer; info:PDISPMANX_MODEINFO_T):int32_t; cdecl;
  
  {Inform driver that the application refcount has become nonzero / zero
   These callbacks might perhaps be used for backlight and power management}
  open : function (instance:pointer):int32_t; cdecl;
  close : function (instance:pointer):int32_t; cdecl;
  
  {Display list updated callback. Primarily of use to a "one-shot" display.
   For convenience of the driver, we pass the register address of the HVS FIFO}
  dlist_updated : procedure (instance:pointer; fifo_reg:Puint32_t);
  
  {End-of-field callback. This may occur in an interrupt context}
  eof_callback : procedure (instance:pointer); cdecl;
 
  {Return screen resolution format}
  get_input_format : function (instance:pointer):DISPLAY_INPUT_FORMAT_T; cdecl;
  
  suspend_resume : function (instance:pointer; up:longint):int32_t; cdecl;
  
  get_3d_format : function (instance:pointer):DISPLAY_3D_FORMAT_T; cdecl;
 end;
 DISPMANX_DISPLAY_FUNCS_T = tag_DISPMANX_DISPLAY_FUNCS_T;
 PDISPMANX_DISPLAY_FUNCS_T = ^DISPMANX_DISPLAY_FUNCS_T;
 
{From interface\vmcs_host\vc_dispmanx.h}
 { Display manager service API }
 { Same function as above, to aid migration of code. }
 function vc_dispman_init:longint; cdecl; external libvchostif name 'vc_dispman_init';

 { Stop the service from being used }
 procedure vc_dispmanx_stop; cdecl; external libvchostif name 'vc_dispmanx_stop';

 { Set the entries in the rect structure }
 function vc_dispmanx_rect_set(rect:PVC_RECT_T; x_offset:uint32_t; y_offset:uint32_t; width:uint32_t; height:uint32_t):longint; cdecl; external libvchostif name 'vc_dispmanx_rect_set';

 { Resources }
 { Create a new resource }
 function vc_dispmanx_resource_create(_type:VC_IMAGE_TYPE_T; width:uint32_t; height:uint32_t; native_image_handle:Puint32_t):DISPMANX_RESOURCE_HANDLE_T; cdecl; external libvchostif name 'vc_dispmanx_resource_create';

 { Write the bitmap data to VideoCore memory }
 function vc_dispmanx_resource_write_data(res:DISPMANX_RESOURCE_HANDLE_T; src_type:VC_IMAGE_TYPE_T; src_pitch:longint; src_address:pointer; rect:PVC_RECT_T):longint; cdecl; external libvchostif name 'vc_dispmanx_resource_write_data';

 function vc_dispmanx_resource_write_data_handle(res:DISPMANX_RESOURCE_HANDLE_T; src_type:VC_IMAGE_TYPE_T; src_pitch:longint; handle:VCHI_MEM_HANDLE_T; offset:uint32_t; 
            rect:PVC_RECT_T):longint; cdecl; external libvchostif name 'vc_dispmanx_resource_write_data_handle';

 function vc_dispmanx_resource_read_data(handle:DISPMANX_RESOURCE_HANDLE_T; p_rect:PVC_RECT_T; dst_address:pointer; dst_pitch:uint32_t):longint; cdecl; external libvchostif name 'vc_dispmanx_resource_read_data';

 { Delete a resource }
 function vc_dispmanx_resource_delete(res:DISPMANX_RESOURCE_HANDLE_T):longint; cdecl; external libvchostif name 'vc_dispmanx_resource_delete';

 { Displays }
 { Opens a display on the given device }
 function vc_dispmanx_display_open(device:uint32_t):DISPMANX_DISPLAY_HANDLE_T; cdecl; external libvchostif name 'vc_dispmanx_display_open';

 { Opens a display on the given device in the request mode }
 function vc_dispmanx_display_open_mode(device:uint32_t; mode:uint32_t):DISPMANX_DISPLAY_HANDLE_T; cdecl; external libvchostif name 'vc_dispmanx_display_open_mode';

 { Open an offscreen display }
 function vc_dispmanx_display_open_offscreen(dest:DISPMANX_RESOURCE_HANDLE_T; orientation:DISPMANX_TRANSFORM_T):DISPMANX_DISPLAY_HANDLE_T; cdecl; external libvchostif name 'vc_dispmanx_display_open_offscreen';

 { Change the mode of a display }
 function vc_dispmanx_display_reconfigure(display:DISPMANX_DISPLAY_HANDLE_T; mode:uint32_t):longint; cdecl; external libvchostif name 'vc_dispmanx_display_reconfigure';

 { Sets the desstination of the display to be the given resource }
 function vc_dispmanx_display_set_destination(display:DISPMANX_DISPLAY_HANDLE_T; dest:DISPMANX_RESOURCE_HANDLE_T):longint; cdecl; external libvchostif name 'vc_dispmanx_display_set_destination';

 { Set the background colour of the display }
 function vc_dispmanx_display_set_background(update:DISPMANX_UPDATE_HANDLE_T; display:DISPMANX_DISPLAY_HANDLE_T; red:uint8_t; green:uint8_t; blue:uint8_t):longint; cdecl; external libvchostif name 'vc_dispmanx_display_set_background';

 { get the width, height, frame rate and aspect ratio of the display }
 function vc_dispmanx_display_get_info(display:DISPMANX_DISPLAY_HANDLE_T; pinfo:PDISPMANX_MODEINFO_T):longint; cdecl; external libvchostif name 'vc_dispmanx_display_get_info';

 { Closes a display }
 function vc_dispmanx_display_close(display:DISPMANX_DISPLAY_HANDLE_T):longint; cdecl; external libvchostif name 'vc_dispmanx_display_close';

 { Updates }
 { Start a new update, DISPMANX_NO_HANDLE on error }
 function vc_dispmanx_update_start(priority:int32_t):DISPMANX_UPDATE_HANDLE_T; cdecl; external libvchostif name 'vc_dispmanx_update_start';

 { Add an elment to a display as part of an update }
 function vc_dispmanx_element_add(update:DISPMANX_UPDATE_HANDLE_T; display:DISPMANX_DISPLAY_HANDLE_T; layer:int32_t; dest_rect:PVC_RECT_T; src:DISPMANX_RESOURCE_HANDLE_T; 
            src_rect:PVC_RECT_T; protection:DISPMANX_PROTECTION_T; alpha:PVC_DISPMANX_ALPHA_T; clamp:PDISPMANX_CLAMP_T; transform:DISPMANX_TRANSFORM_T):DISPMANX_ELEMENT_HANDLE_T; cdecl; external libvchostif name 'vc_dispmanx_element_add';

 { Change the source image of a display element }
 function vc_dispmanx_element_change_source(update:DISPMANX_UPDATE_HANDLE_T; element:DISPMANX_ELEMENT_HANDLE_T; src:DISPMANX_RESOURCE_HANDLE_T):longint; cdecl; external libvchostif name 'vc_dispmanx_element_change_source';

 { Change the layer number of a display element }
 function vc_dispmanx_element_change_layer(update:DISPMANX_UPDATE_HANDLE_T; element:DISPMANX_ELEMENT_HANDLE_T; layer:int32_t):longint; cdecl; external libvchostif name 'vc_dispmanx_element_change_layer';

 { Signal that a region of the bitmap has been modified }
 function vc_dispmanx_element_modified(update:DISPMANX_UPDATE_HANDLE_T; element:DISPMANX_ELEMENT_HANDLE_T; rect:PVC_RECT_T):longint; cdecl; external libvchostif name 'vc_dispmanx_element_modified';

 { Remove a display element from its display }
 function vc_dispmanx_element_remove(update:DISPMANX_UPDATE_HANDLE_T; element:DISPMANX_ELEMENT_HANDLE_T):longint; cdecl; external libvchostif name 'vc_dispmanx_element_remove';

 { Ends an update }
 function vc_dispmanx_update_submit(update:DISPMANX_UPDATE_HANDLE_T; cb_func:DISPMANX_CALLBACK_FUNC_T; cb_arg:pointer):longint; cdecl; external libvchostif name 'vc_dispmanx_update_submit';

 { End an update and wait for it to complete }
 function vc_dispmanx_update_submit_sync(update:DISPMANX_UPDATE_HANDLE_T):longint; cdecl; external libvchostif name 'vc_dispmanx_update_submit_sync';

 { Query the image formats supported in the VMCS build }
 function vc_dispmanx_query_image_formats(supported_formats:Puint32_t):longint; cdecl; external libvchostif name 'vc_dispmanx_query_image_formats';

 {New function added to VCHI to change attributes, set_opacity does not work there. }
 function vc_dispmanx_element_change_attributes(update:DISPMANX_UPDATE_HANDLE_T; element:DISPMANX_ELEMENT_HANDLE_T; change_flags:uint32_t; layer:int32_t; opacity:uint8_t; 
            dest_rect:PVC_RECT_T; src_rect:PVC_RECT_T; mask:DISPMANX_RESOURCE_HANDLE_T; transform:DISPMANX_TRANSFORM_T):longint; cdecl; external libvchostif name 'vc_dispmanx_element_change_attributes';

 {xxx hack to get the image pointer from a resource handle, will be obsolete real soon }
 function vc_dispmanx_resource_get_image_handle(res:DISPMANX_RESOURCE_HANDLE_T):uint32_t; cdecl; external libvchostif name 'vc_dispmanx_resource_get_image_handle';

 {Call this instead of vc_dispman_init }
 procedure vc_vchi_dispmanx_init(initialise_instance:PVCHI_INSTANCE_T; connections:PPVCHI_CONNECTION_T; num_connections:uint32_t); cdecl; external libvchostif name 'vc_vchi_dispmanx_init';

 { Take a snapshot of a display in its current state. }
 { This call may block for a time; when it completes, the snapshot is ready. }
 { only transform=0 is supported }
 function vc_dispmanx_snapshot(display:DISPMANX_DISPLAY_HANDLE_T; snapshot_resource:DISPMANX_RESOURCE_HANDLE_T; transform:DISPMANX_TRANSFORM_T):longint; cdecl; external libvchostif name 'vc_dispmanx_snapshot';

 { Set the resource palette (for VC_IMAGE_4BPP and VC_IMAGE_8BPP) }
 function vc_dispmanx_resource_set_palette(handle:DISPMANX_RESOURCE_HANDLE_T; src_address:pointer; offset:longint; size:longint):longint; cdecl; external libvchostif name 'vc_dispmanx_resource_set_palette';

 { Start triggering callbacks synced to vsync }
 function vc_dispmanx_vsync_callback(display:DISPMANX_DISPLAY_HANDLE_T; cb_func:DISPMANX_CALLBACK_FUNC_T; cb_arg:pointer):longint; cdecl; external libvchostif name 'vc_dispmanx_vsync_callback';
 
implementation

end.
