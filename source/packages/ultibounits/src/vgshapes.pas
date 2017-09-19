{VGShapes Library:
 
  Ported to FreePascal by Garry Wood <garry@softoz.com.au>

  From C source available at https://github.com/ajstarks/openvg
  
 Original Copyright:
  
  Copyright (C) 2012 Anthony Starks
  
  Permission is hereby granted, free of charge, to any person obtaining a copy of this software 
  and associated documentation files (the "Software"), to deal in the Software without 
  restriction, including without limitation the rights to use, copy, modify, merge, publish, 
  distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the
  Software is furnished to do so, subject to the following conditions:
  
  The above copyright notice and this permission notice shall be included in all copies or
  substantial portions of the Software.
  
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, 
  INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE 
  AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, 
  DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, 
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
  
  The images in this repository are Licensed under 
  the Creative Commons Attribution 3.0 license as described in
  http://creativecommons.org/licenses/by/3.0/us/  

}

unit VGShapes;

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}
 
interface
 
uses GlobalConst,SysUtils,Classes,EGL,DispmanX,OpenVG,VC4,JPEGLib,Jerror,JDataSrc,JdAPImin,JdAPIstd;
 
{==============================================================================}
const
 {Font Information}
 VGSHAPES_MAXFONTPATH = 500;

{==============================================================================}
type
 {Font Information}
 PVGShapesFontInfo = ^TVGShapesFontInfo;
 TVGShapesFontInfo = record
  CharacterMap:PSmallint;
  GlyphAdvances:PInteger;
  Count:Integer;
  DescenderHeight:Integer;
  FontHeight:Integer;
  Glyphs:array[0..VGSHAPES_MAXFONTPATH - 1] of VGPath;
 end;
 
 {Color Information}
 PVGShapesColor = ^TVGShapesColor;
 TVGShapesColor = array[0..3] of VGfloat;
 
{==============================================================================}
var
 {Font Information}
 VGShapesSansTypeface:TVGShapesFontInfo;
 VGShapesSerifTypeface:TVGShapesFontInfo;
 VGShapesMonoTypeface:TVGShapesFontInfo;
 
{==============================================================================}
{Library Functions}
{Initialization}
procedure VGShapesInitWindowSize(x,y:Integer;w,h:LongWord);
procedure VGShapesInit(var w,h:Integer);
procedure VGShapesFinish;

{Font}
function VGShapesLoadFont(Points,PointIndices:PInteger;Instructions:PByte;InstructionIndices,InstructionCounts,adv:PInteger;cmap:PSmallInt;ng:Integer):TVGShapesFontInfo;
procedure VGShapesUnloadFont(glyphs:PVGPath;n:Integer);

{Image}
procedure VGShapesMakeImage(x,y:VGfloat;w,h:Integer;data:PVGubyte);
procedure VGShapesImage(x,y:VGfloat;w,h:Integer;const filename:String);
function VGShapesCreateImageFromJpeg(const filename:String):VGImage;

{Transformation}
procedure VGShapesTranslate(x,y:VGfloat); inline;
procedure VGShapesRotate(r:VGfloat); inline;
procedure VGShapesShear(x,y:VGfloat); inline;
procedure VGShapesScale(x,y:VGfloat); inline;
 
{Style} 
procedure VGShapesSetFill(const color:TVGShapesColor);
procedure VGShapesSetStroke(const color:TVGShapesColor);
procedure VGShapesStrokeWidth(width:VGfloat);

{Color}
procedure VGShapesRGBA(r,g,b:LongWord;a:VGfloat;var color:TVGShapesColor);
procedure VGShapesRGB(r,g,b:LongWord;var color:TVGShapesColor); inline;

procedure VGShapesStroke(r,g,b:LongWord;a:VGfloat);
procedure VGShapesFill(r,g,b:LongWord;a:VGfloat);
procedure VGShapesFillLinearGradient(x1,y1,x2,y2:VGfloat;stops:PVGfloat;ns:Integer);
procedure VGShapesFillRadialGradient(cx,cy,fx,fy,radius:VGfloat;stops:PVGfloat;ns:Integer);
procedure VGShapesClipRect(x,y,w,h:VGint);
procedure VGShapesClipEnd; inline;

{Text}
procedure VGShapesText(x,y:VGfloat;const s:UTF8String;const f:TVGShapesFontInfo;pointsize:Integer);
function VGShapesTextWidth(const s:UTF8String;const f:TVGShapesFontInfo;pointsize:Integer):VGfloat;
procedure VGShapesTextMid(x,y:VGfloat;const s:UTF8String;const f:TVGShapesFontInfo;pointsize:Integer);
procedure VGShapesTextEnd(x,y:VGfloat;const s:UTF8String;const f:TVGShapesFontInfo;pointsize:Integer);
function VGShapesTextHeight(const f:TVGShapesFontInfo;pointsize:Integer):VGfloat;
function VGShapesTextDepth(const f:TVGShapesFontInfo;pointsize:Integer):VGfloat;

{Shape}
procedure VGShapesCbezier(sx,sy,cx,cy,px,py,ex,ey:VGfloat);
procedure VGShapesQbezier(sx,sy,cx,cy,ex,ey:VGfloat);
procedure VGShapesPolygon(x,y:PVGfloat;n:VGint);
procedure VGShapesPolyline(x,y:PVGfloat;n:VGint);
procedure VGShapesRect(x,y,w,h:VGfloat);
procedure VGShapesLine(x1,y1,x2,y2:VGfloat);
procedure VGShapesRoundrect(x,y,w,h,rw,rh:VGfloat);
procedure VGShapesEllipse(x,y,w,h:VGfloat);
procedure VGShapesCircle(x,y,r:VGfloat); inline;
procedure VGShapesArc(x,y,w,h,sa,aext:VGfloat);

{Rendering}
procedure VGShapesStart(width,height:Integer);
function VGShapesEnd:Boolean;
function VGShapesSaveEnd(const filename:String):Boolean;
procedure VGShapesBackground(r,g,b:LongWord);
procedure VGShapesBackgroundRGB(r,g,b:LongWord;a:VGfloat);
procedure VGShapesWindowClear; inline;
procedure VGShapesAreaClear(x,y,w,h:LongWord); inline;
procedure VGShapesWindowOpacity(a:LongWord);
procedure VGShapesWindowPosition(x,y:Integer);

{Outlined Shapes}
procedure VGShapesCbezierOutline(sx,sy,cx,cy,px,py,ex,ey:VGfloat);
procedure VGShapesQbezierOutline(sx,sy,cx,cy,ex,ey:VGfloat);
procedure VGShapesRectOutline(x,y,w,h:VGfloat);
procedure VGShapesRoundrectOutline(x,y,w,h,rw,rh:VGfloat);
procedure VGShapesEllipseOutline(x,y,w,h:VGfloat);
procedure VGShapesCircleOutline(x,y,r:VGfloat); inline;
procedure VGShapesArcOutline(x,y,w,h,sa,aext:VGfloat);

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
type
 {EGL State Type}
 PEGLState = ^TEGLState;
 TEGLState = record
  {Screen dimentions}
  ScreenWidth:LongWord;
  ScreenHeight:LongWord;
  {Window dimentions}
  WindowX:LongInt;
  WindowY:LongInt;
  WindowWidth:LongWord;
  WindowHeight:LongWord;
  {DispmanX window}
  Element:DISPMANX_ELEMENT_HANDLE_T;
  {EGL data}
  Display:EGLDisplay;
  Surface:EGLSurface;
  Context:EGLContext;
  {EGL config}
  Alpha:VC_DISPMANX_ALPHA_T;
  NativeWindow:EGL_DISPMANX_WINDOW_T;
  AttributeList:array[0..10] of EGLint;
 end;
 
{==============================================================================}
{==============================================================================}
var
 {Global Variables}
 VGShapesState:TEGLState;

 VGShapesInitX:Integer = 0;  {Initial Window Position and Size}
 VGShapesInitY:Integer = 0;
 VGShapesInitW:LongWord = 0;
 VGShapesInitH:LongWord = 0;
 
 VGShapesInitialized:Boolean;
 
{==============================================================================}
{Included Fonts}
{$INCLUDE .\vgfonts\DejaVuSans.inc}
{$INCLUDE .\vgfonts\DejaVuSansMono.inc}
{$INCLUDE .\vgfonts\DejaVuSerif.inc}

{==============================================================================}
{==============================================================================}
{Internal Functions}
procedure SetWindowParams(State:PEGLState;X,Y:Integer;SourceRect,DestRect:PVC_RECT_T);
{SetWindowParams sets the window's position, adjusting if need be to prevent
 it from going fully off screen. Also sets the dispman rects for displaying}
var
 dx:LongWord;
 dy:LongWord;
 w:LongWord;
 h:LongWord;
 sx:LongWord;
 sy:LongWord;
begin
 {}
 {Check State}
 if State = nil then Exit;
 
 {Set source & destination rectangles so that the image is clipped if it goes off screen (else dispman won't show it properly)}
 if X < (1 - Integer(State.WindowWidth)) then {Too far off left}
  begin
   X:=1;
   dx:=0;
   sx:=State.WindowWidth - 1;
   w:=1;
  end
 else if X < 0 then {Part of left is off}
  begin
   dx:=0;
   sx:=-X;
   w:=State.WindowWidth - sx;
  end
 else if X < (State.ScreenWidth - State.WindowWidth) then
  begin
   dx:=X;
   sx:=0;
   w:=State.WindowWidth;
  end
 else if X < State.ScreenWidth then {Part of right is off}
  begin
   dx:=X;
   sx:=0;
   w:=State.ScreenWidth - X;
  end
 else {Too far off right}
  begin
   X:=State.ScreenWidth - 1;
   dx:=State.ScreenWidth - 1;
   sx:=0;
   w:=1;
  end;  
  
 if Y < (1 - Integer(State.WindowHeight)) then {Too far off top}
  begin
   Y:=1 - Integer(State.WindowHeight);
   dy:=0;
   sy:=State.WindowHeight - 1;
   h:=1;
  end
 else if Y < 0 then {Part of top is off}
  begin
   dy:=0;
   sy:=-Y;
   h:=State.WindowHeight - sy;
  end
 else if Y < (State.ScreenHeight - State.WindowHeight) then
  begin
   dy:=Y;
   sy:=0;
   h:=State.WindowHeight;
  end  
 else if Y < State.ScreenHeight then {Part of bottom is off}
  begin
   dy:=Y;
   sy:=0;
   h:=State.ScreenHeight - Y;
  end
 else {Wholly off bottom}
  begin
   Y:=State.ScreenHeight - 1;
   dy:=State.ScreenHeight - 1;
   sy:=0;
   h:=1;
  end;  

 {Update Position}
 State.WindowX:=X;
 State.WindowY:=Y;
 
 vc_dispmanx_rect_set(DestRect,dx,dy,w,h);
 vc_dispmanx_rect_set(SourceRect,sx shl 16,sy shl 16,w shl 16,h shl 16);
end;

{==============================================================================}

function eglInit(State:PEGLState):Boolean;
{eglInit sets the display, context and screen information, state holds the display information}
var
 Config:EGLConfig;
 ConfigCount:EGLint;
 EGLResult:EGLBoolean;

 DestRect:VC_RECT_T;
 SourceRect:VC_RECT_T;
 DispmanDisplay:DISPMANX_DISPLAY_HANDLE_T;
 DispmanUpdate:DISPMANX_UPDATE_HANDLE_T;
 DispmanElement:DISPMANX_ELEMENT_HANDLE_T;
begin
 {}
 Result:=False;
 
 {Check State}
 if State = nil then Exit;
 
 {Setup Defaults}
 DispmanDisplay:=DISPMANX_NO_HANDLE;
 DispmanUpdate:=DISPMANX_NO_HANDLE;
 DispmanElement:=DISPMANX_NO_HANDLE;
 State.Display:=EGL_NO_DISPLAY;
 State.Surface:=EGL_NO_SURFACE;
 State.Context:=EGL_NO_CONTEXT;
 
 {Setup Alpha}
 State.Alpha.flags:=DISPMANX_FLAGS_ALPHA_FIXED_ALL_PIXELS;
 State.Alpha.opacity:=255;
 State.Alpha.mask:=0;
 
 {Setup Attribute List}
 State.AttributeList[0]:=EGL_RED_SIZE;
 State.AttributeList[1]:=8;
 State.AttributeList[2]:=EGL_GREEN_SIZE;
 State.AttributeList[3]:=8;
 State.AttributeList[4]:=EGL_BLUE_SIZE;
 State.AttributeList[5]:=8;
 State.AttributeList[6]:=EGL_ALPHA_SIZE;
 State.AttributeList[7]:=8;
 State.AttributeList[8]:=EGL_SURFACE_TYPE;
 State.AttributeList[9]:=EGL_WINDOW_BIT;
 State.AttributeList[10]:=EGL_NONE;
 
 try
  {Get an EGL display connection}
  State.Display:=eglGetDisplay(EGL_DEFAULT_DISPLAY);
  if State.Display = EGL_NO_DISPLAY then Exit;
  
  {Initialize the EGL display connection}
  EGLResult:=eglInitialize(State.Display,nil,nil);
  if EGLResult = EGL_FALSE then Exit;
  
  {Bind OpenVG API}
  EGLResult:=eglBindAPI(EGL_OPENVG_API);
  if EGLResult = EGL_FALSE then Exit;
  
  {Get an appropriate EGL framebuffer configuration}
  EGLResult:=eglChooseConfig(State.Display,@State.AttributeList,@Config,1,@ConfigCount);
  if EGLResult = EGL_FALSE then Exit;
  
  {Create an EGL rendering context}
  State.Context:=eglCreateContext(State.Display,Config,EGL_NO_CONTEXT,nil);
  if State.Context = EGL_NO_CONTEXT then Exit;
  
  {Create an EGL window surface}
  if BCMHostGraphicsGetDisplaySize(DISPMANX_ID_MAIN_LCD,State.ScreenWidth,State.ScreenHeight) < 0 then Exit;
  
  if (State.WindowWidth = 0) or (State.WindowWidth > State.ScreenWidth) then
   begin
    State.WindowWidth:=State.ScreenWidth;
   end;
   
  if (State.WindowHeight = 0) or (State.WindowHeight > State.ScreenHeight) then
   begin
    State.WindowHeight:=State.ScreenHeight;
   end;
  
  {Adjust position so that at least one pixel is on screen and set up the dispman rects}
  SetWindowParams(State,State.WindowX,State.WindowY,@SourceRect,@DestRect);
  
  {Open Dispman Display}
  DispmanDisplay:=vc_dispmanx_display_open(DISPMANX_ID_MAIN_LCD);
  if DispmanDisplay = DISPMANX_NO_HANDLE then Exit;
  
  {Start Dispman Update}
  DispmanUpdate:=vc_dispmanx_update_start(0);
  if DispmanUpdate = DISPMANX_NO_HANDLE then Exit;
  
  {Add Dispman Element}
  DispmanElement:=vc_dispmanx_element_add(DispmanUpdate,DispmanDisplay,0 {Layer},@DestRect,0 {Source},@SourceRect,DISPMANX_PROTECTION_NONE,@State.Alpha,nil {Clamp},DISPMANX_NO_ROTATE {Transform});
  if DispmanElement = DISPMANX_NO_HANDLE then Exit;
  
  State.Element:=DispmanElement;
  State.NativeWindow.Element:=DispmanElement;
  State.NativeWindow.Width:=State.WindowWidth;
  State.NativeWindow.Height:=State.WindowHeight;
  
  {Submit Dispman Update}
  vc_dispmanx_update_submit_sync(DispmanUpdate);
 
  {Create an EGL window surface}
  State.Surface:=eglCreateWindowSurface(State.Display,Config,@State.NativeWindow,nil);
  if State.Surface = EGL_NO_SURFACE then Exit;
  
  {Preserve the buffers on swap}
  EGLResult:=eglSurfaceAttrib(State.Display,State.Surface,EGL_SWAP_BEHAVIOR,EGL_BUFFER_PRESERVED);
  if EGLResult = EGL_FALSE then Exit;
 
  {Connect the context to the surface}
  EGLResult:=eglMakeCurrent(State.Display,State.Surface,State.Surface,State.Context);
  if EGLResult = EGL_FALSE then Exit;
  
  Result:=True;
 finally
  if not Result then
   begin
    {Close Dispman Display}
    if DispmanDisplay <> DISPMANX_NO_HANDLE then vc_dispmanx_display_close(DispmanDisplay);
    
    if State.Display <> EGL_NO_DISPLAY then
     begin
      {Terminate EGL}
      eglMakeCurrent(State.Display,EGL_NO_SURFACE,EGL_NO_SURFACE,EGL_NO_CONTEXT);
      
      {Destroy Surface}
      if State.Surface <> EGL_NO_SURFACE then eglDestroySurface(State.Display,State.Surface);
      
      {Destroy Context}
      if State.Context <> EGL_NO_CONTEXT then eglDestroyContext(State.Display,State.Context);

      {Terminate Display}
      eglTerminate(State.Display);
     end;
   end;
 end; 
end;

{==============================================================================}

procedure DispmanXMoveWindow(State:PEGLState;X,Y:Integer); 
{DispmanXMoveWindow repositions the openVG window to given coords -ve coords are allowed upto (1-width,1-height),
 max (screen_width-1,screen_height-1). i.e. at least one pixel must be on the screen}
var
 DestRect:VC_RECT_T;
 SourceRect:VC_RECT_T;
 DispmanUpdate:DISPMANX_UPDATE_HANDLE_T;
begin
 {}
 {Check State}
 if State = nil then Exit;
 
 {Update Window}
 SetWindowParams(State,X,Y,@SourceRect,@DestRect);
 
 {Start Update}
 DispmanUpdate:=vc_dispmanx_update_start(0);
 
 {Change Element}
 vc_dispmanx_element_change_attributes(DispmanUpdate,State.Element,0,0,0,@DestRect,@SourceRect,0,DISPMANX_NO_ROTATE);

 {Submit Update}
 vc_dispmanx_update_submit_sync(DispmanUpdate);
end;

{==============================================================================}

procedure DispmanXChangeWindowOpacity(State:PEGLState;Alpha:LongWord); 
{DispmanXChangeWindowOpacity changes the window's opacity 0 = transparent, 255 = opaque}
var
 DispmanUpdate:DISPMANX_UPDATE_HANDLE_T;
begin
 {}
 {Check State}
 if State = nil then Exit;
 
 {Check Alpha}
 if Alpha > 255 then Alpha:=255;

 {Start Update}
 DispmanUpdate:=vc_dispmanx_update_start(0);

 {Change Element (1 shl 1 equals the Alpha value)}
 vc_dispmanx_element_change_attributes(DispmanUpdate,State.Element,1 shl 1,0,Alpha,nil,nil,0,DISPMANX_NO_ROTATE);

 {Submit Update}
 vc_dispmanx_update_submit_sync(DispmanUpdate);
end;

{==============================================================================}
{==============================================================================}
{Library Functions}
{Initialization}
procedure VGShapesInitWindowSize(x,y:Integer;w,h:LongWord);
{InitWindowSize requests a specific window size & position, if not called then VGShapesInit() will open a full screen window}
begin
 {}
 if VGShapesInitialized then Exit;
 
 VGShapesInitX:=x;
 VGShapesInitY:=y;
 VGShapesInitW:=w;
 VGShapesInitH:=h;
end;

{==============================================================================}

procedure VGShapesInit(var w,h:Integer);
{Init sets the system to its initial state}
begin
 {}
 {Setup Defaults}
 w:=-1;
 h:=-1;
 
 if not VGShapesInitialized then
  begin
   {Initialize Host}
   BCMHostInit;
   
   {Setup State}
   FillChar(VGShapesState,SizeOf(TEGLState),0);
   VGShapesState.WindowX:=VGShapesInitX;
   VGShapesState.WindowY:=VGShapesInitY;
   VGShapesState.WindowWidth:=VGShapesInitW;
   VGShapesState.WindowHeight:=VGShapesInitH;
   
   {Initialize EGL}
   if not eglInit(@VGShapesState) then
    begin
     BCMHostDeinit;
     Exit;
    end; 
   
   {Load Sans Font}
   VGShapesSansTypeface:=VGShapesLoadFont(@DejaVuSans_glyphPoints,
                                          @DejaVuSans_glyphPointIndices,
                                          @DejaVuSans_glyphInstructions,
                                          @DejaVuSans_glyphInstructionIndices,
                                          @DejaVuSans_glyphInstructionCounts,
                                          @DejaVuSans_glyphAdvances,
                                          @DejaVuSans_characterMap,
                                          DejaVuSans_glyphCount);
   VGShapesSansTypeface.DescenderHeight:=DejaVuSans_descender_height;
   VGShapesSansTypeface.FontHeight:=DejaVuSans_font_height;
   
   {Load Serif Font}
   VGShapesSerifTypeface:=VGShapesLoadFont(@DejaVuSerif_glyphPoints,
                                           @DejaVuSerif_glyphPointIndices,
                                           @DejaVuSerif_glyphInstructions,
                                           @DejaVuSerif_glyphInstructionIndices,
                                           @DejaVuSerif_glyphInstructionCounts,
                                           @DejaVuSerif_glyphAdvances,
                                           @DejaVuSerif_characterMap,
                                           DejaVuSerif_glyphCount);
   VGShapesSerifTypeface.DescenderHeight:=DejaVuSerif_descender_height;
   VGShapesSerifTypeface.FontHeight:=DejaVuSerif_font_height;
   
   {Load Mono Font}
   VGShapesMonoTypeface:=VGShapesLoadFont(@DejaVuSansMono_glyphPoints,
                                          @DejaVuSansMono_glyphPointIndices,
                                          @DejaVuSansMono_glyphInstructions,
                                          @DejaVuSansMono_glyphInstructionIndices,
                                          @DejaVuSansMono_glyphInstructionCounts,
                                          @DejaVuSansMono_glyphAdvances,
                                          @DejaVuSansMono_characterMap,
                                          DejaVuSansMono_glyphCount);
   VGShapesMonoTypeface.DescenderHeight:=DejaVuSansMono_descender_height;
   VGShapesMonoTypeface.FontHeight:=DejaVuSansMono_font_height;
   
   VGShapesInitialized:=True;
  end; 
 
 {Return Window Size}
 w:=VGShapesState.WindowWidth;
 h:=VGShapesState.WindowHeight;
end;

{==============================================================================}

procedure VGShapesFinish;
{Finish cleans up}
begin
 {}
 if not VGShapesInitialized then Exit;

 {Unload Sans Font}
 VGShapesUnloadFont(VGShapesSansTypeface.Glyphs,VGShapesSansTypeface.Count);
 
 {Unload Serif Font}
 VGShapesUnloadFont(VGShapesSerifTypeface.Glyphs,VGShapesSerifTypeface.Count);
   
 {Unload Mono Font}
 VGShapesUnloadFont(VGShapesMonoTypeface.Glyphs,VGShapesMonoTypeface.Count);
    
 {Terminate EGL}    
 eglSwapBuffers(VGShapesState.Display,VGShapesState.Surface);
 eglMakeCurrent(VGShapesState.Display,EGL_NO_SURFACE,EGL_NO_SURFACE,EGL_NO_CONTEXT);
 eglDestroySurface(VGShapesState.Display,VGShapesState.Surface);
 eglDestroyContext(VGShapesState.Display,VGShapesState.Context);
 eglTerminate(VGShapesState.Display);
 
 VGShapesInitialized:=False;
end;

{==============================================================================}
{Font}
function VGShapesLoadFont(Points,PointIndices:PInteger;Instructions:PByte;InstructionIndices,InstructionCounts,adv:PInteger;cmap:PSmallInt;ng:Integer):TVGShapesFontInfo;
{Loadfont loads font path data derived from http://web.archive.org/web/20070808195131/http://developer.hybrid.fi/font2openvg/renderFont.cpp.txt}
var
 Count:Integer;
 Path:VGPath;
 Point:PInteger;
 Instruction:PByte;
 InstructionCount:Integer;
begin
 {}
 FillChar(Result,SizeOf(TVGShapesFontInfo),0);

 {Check Parameters}
 if Points = nil then Exit;
 if PointIndices = nil then Exit;
 if Instructions = nil then Exit;
 if InstructionIndices = nil then Exit;
 if InstructionCounts = nil then Exit;
 if adv = nil then Exit;
 if cmap = nil then Exit;
 
 if ng > VGSHAPES_MAXFONTPATH then Exit;
 
 for Count:=0 to ng - 1 do
  begin
   Point:=@Points[PointIndices[Count] * 2];
   Instruction:=@Instructions[InstructionIndices[Count]];
   InstructionCount:=InstructionCounts[Count];
   
   {Create Path}
   Path:=vgCreatePath(VG_PATH_FORMAT_STANDARD,VG_PATH_DATATYPE_S_32,1.0 / 65536.0,0.0,0,0,VG_PATH_CAPABILITY_ALL);
   Result.Glyphs[Count]:=Path;
   
   if InstructionCount <> 0 then
    begin
     vgAppendPathData(Path,InstructionCount,Instruction,Point);
    end;
  end;
 
 Result.CharacterMap:=cmap;
 Result.GlyphAdvances:=adv;
 Result.Count:=ng;
 Result.DescenderHeight:=0; {Updated by caller}
 Result.FontHeight:=0; {Updated by caller}
end;

{==============================================================================}

procedure VGShapesUnloadFont(glyphs:PVGPath;n:Integer);
{Unloadfont frees font path data}
var
 Count:Integer;
begin
 {}
 {Check Glyphs}
 if glyphs = nil then Exit;
 
 for Count:=0 to n - 1 do
  begin
   {Destroy Path}
   vgDestroyPath(glyphs[Count]);
  end;
end;

{==============================================================================}
{Image}
procedure VGShapesMakeImage(x,y:VGfloat;w,h:Integer;data:PVGubyte);
{MakeImage makes an image from a raw raster of red, green, blue, alpha values}
var
 Stride:LongWord;
 Image:VGImage;
 ImageFormat:VGImageFormat;
begin
 {}
 {Setup Stride}
 Stride:=w * 4;
 
 {Setup Format}
 ImageFormat:=VG_sABGR_8888;
 
 {Create Image}
 Image:=vgCreateImage(ImageFormat,w,h,VG_IMAGE_QUALITY_BETTER);
 if Image = VG_INVALID_HANDLE then Exit;
 
 {Copy Pixels to Image}
 vgImageSubData(Image,data,Stride,ImageFormat,0,0,w,h);
 
 {Display Image}
 vgSetPixels(Trunc(x),Trunc(y),Image,0,0,w,h);
 
 {Destroy Image}
 vgDestroyImage(Image);
end;

{==============================================================================}

procedure VGShapesImage(x,y:VGfloat;w,h:Integer;const filename:String);
{Image places an image at the specifed location}
var
 Image:VGImage;
begin
 {}
 {Create Image}
 Image:=VGShapesCreateImageFromJpeg(filename);
 if Image = VG_INVALID_HANDLE then Exit;

 {Display Image}
 vgSetPixels(Trunc(x),Trunc(y),Image,0,0,w,h);
 
 {Destroy Image}
 vgDestroyImage(Image);
end;

{==============================================================================}

function VGShapesCreateImageFromJpeg(const filename:String):VGImage;
{CreateImageFromJpeg decompresses a JPEG image to the standard image format
 source: https://github.com/ileben/ShivaVG/blob/master/examples/test_image.c}
var
 FileStream:TFileStream;
 
 jerr:jpeg_error_mgr;
 jdc:jpeg_decompress_struct;
 
 Data:PVGubyte;
 DataStride:LongWord;
 DataBPP:LongWord;
 
 Buffer:JSAMPARRAY;
 BufferStride:LongWord;
 BufferBPP:LongWord;
 
 Image:VGImage;
 Width:LongWord;
 Height:LongWord;
 
 X:LongWord;
 DataRow:PVGubyte;
 BufferRow:PVGubyte;
 EndianTest:LongWord;
 ImageFormat:VGImageFormat;
begin
 {}
 Result:=VG_INVALID_HANDLE;
 
 {Check for endianness}
 EndianTest:=1;
 if PByte(@EndianTest)[0] = 1 then
  begin
   ImageFormat:=VG_sABGR_8888;
  end
 else
  begin 
   ImageFormat:=VG_sRGBA_8888;
  end; 
  
 {Try to open image file}
 FileStream:=TFileStream.Create(Filename,fmOpenRead or fmShareDenyNone);
 try
  {Setup default error handling}
  jdc.err:=jpeg_std_error(jerr);
  jpeg_create_decompress(@jdc);
  
  {Set input file}
  jpeg_stdio_src(@jdc,@FileStream);
    
  {Read header and start}
  jpeg_read_header(@jdc,True);
  jpeg_start_decompress(@jdc);
  Width:=jdc.output_width;
  Height:=jdc.output_height;
  
  {Allocate buffer using jpeg allocator}
  BufferBPP:=jdc.output_components;
  BufferStride:=Width * BufferBPP;
  Buffer:=jdc.mem.alloc_sarray(@jdc,JPOOL_IMAGE,BufferStride,1);
  
  {Allocate image data buffer}
  DataBPP:=4;
  DataStride:=Width * DataBPP;
  Data:=GetMem(DataStride * Height);
  
  {Iterate until all scanlines processed}
  while jdc.output_scanline < Height do
   begin
    {Read scanline into buffer}
    jpeg_read_scanlines(@jdc,Buffer,1);
    
    {Get Rows}
    DataRow:=Data + (Height - jdc.output_scanline) * DataStride;
    BufferRow:=PVGubyte(Buffer[0]);
    
    {Expand to RGBA}
    X:=0;
    while X < Width do
     begin
      case BufferBPP of
       4:begin
         {RGBA}
         DataRow[0]:=BufferRow[0];
         DataRow[1]:=BufferRow[1];
         DataRow[2]:=BufferRow[2];
         DataRow[3]:=BufferRow[3];
        end;
       3:begin
         {RGB}
         DataRow[0]:=BufferRow[0];
         DataRow[1]:=BufferRow[1];
         DataRow[2]:=BufferRow[2];
         DataRow[3]:=255;
        end;
      end;
      
      {Update X}
      Inc(X);
      
      {Update Rows}
      DataRow:=DataRow + DataBPP;
      BufferRow:=BufferRow + BufferBPP;
     end;
   end;
 
  {Create VG image}
  Image:=vgCreateImage(ImageFormat,Width,Height,VG_IMAGE_QUALITY_BETTER);
  vgImageSubData(Image,Data,DataStride,ImageFormat,0,0,Width,Height);
  
  {Destroy decompress}
  jpeg_destroy_decompress(@jdc);
  
  {Free data}
  FreeMem(Data);
  
  {Return Image}
  Result:=Image;
 finally
  FileStream.Free;
 end;
end;

{==============================================================================}
{Transformation}
procedure VGShapesTranslate(x,y:VGfloat); inline;
{Translate the coordinate system to x,y}
begin
 {}
 vgTranslate(x,y);
end;

{==============================================================================}

procedure VGShapesRotate(r:VGfloat); inline;
{Rotate around angle r}
begin
 {}
 vgRotate(r);
end;

{==============================================================================}

procedure VGShapesShear(x,y:VGfloat); inline;
{Shear shears the x coordinate by x degrees, the y coordinate by y degrees}
begin
 {}
 vgShear(x,y);
end;

{==============================================================================}

procedure VGShapesScale(x,y:VGfloat); inline;
{Scale scales by  x, y}
begin
 {}
 vgScale(x,y);
end;

{==============================================================================}
{Style} 
procedure VGShapesSetFill(const color:TVGShapesColor);
{SetFill sets the fill color}
var
 fillPaint:VGPaint;
begin
 {}
 {Create Paint}
 fillPaint:=vgCreatePaint;

 {Set Parameters} 
 vgSetParameteri(fillPaint,VG_PAINT_TYPE,VG_PAINT_TYPE_COLOR);
 vgSetParameterfv(fillPaint,VG_PAINT_COLOR,4,@color);
 
 {Set Paint}
 vgSetPaint(fillPaint,VG_FILL_PATH);
 
 {Destroy Paint}
 vgDestroyPaint(fillPaint);
end;

{==============================================================================}

procedure VGShapesSetStroke(const color:TVGShapesColor);
{SetStroke sets the stroke color}
var
 strokePaint:VGPaint;
begin
 {}
 {Create Paint}
 strokePaint:=vgCreatePaint;

 {Set Parameters} 
 vgSetParameteri(strokePaint,VG_PAINT_TYPE,VG_PAINT_TYPE_COLOR);
 vgSetParameterfv(strokePaint,VG_PAINT_COLOR,4,@color);
 
 {Set Paint}
 vgSetPaint(strokePaint,VG_STROKE_PATH);
 
 {Destroy Paint}
 vgDestroyPaint(strokePaint);
end;

{==============================================================================}

procedure VGShapesStrokeWidth(width:VGfloat);
{StrokeWidth sets the stroke width}
begin
 {}
 vgSetf(VG_STROKE_LINE_WIDTH,width);
 vgSeti(VG_STROKE_CAP_STYLE,VG_CAP_BUTT);
 vgSeti(VG_STROKE_JOIN_STYLE,VG_JOIN_MITER);
end;

{==============================================================================}
{Color}
procedure VGShapesRGBA(r,g,b:LongWord;a:VGfloat;var color:TVGShapesColor);
{RGBA fills a color vectors from a RGBA quad}
begin
 {}
 {Check Red}
 if r > 255 then r:=0;
 
 {Check Green}
 if g > 255 then g:=0;
 
 {Check Blue}
 if b > 255 then b:=0;
 
 {Check Alpha}
 if (a < 0.0) or (a > 1.0) then a:=1.0;

 {Set Color}
 Color[0]:=r / 255.0;
 Color[1]:=g / 255.0;
 Color[2]:=b / 255.0;
 Color[3]:=a;
end;

{==============================================================================}

procedure VGShapesRGB(r,g,b:LongWord;var color:TVGShapesColor); inline;
{RGB returns a solid color from a RGB triple}
begin
 {}
 VGShapesRGBA(r,g,b,1.0,color);
end;

{==============================================================================}

procedure VGShapesStroke(r,g,b:LongWord;a:VGfloat);
{Stroke sets the stroke color, defined as a RGB triple.}
var
 Color:TVGShapesColor;
begin
 {}
 VGShapesRGBA(r,g,b,a,Color);
 VGShapesSetStroke(Color);
end;

{==============================================================================}

procedure VGShapesFill(r,g,b:LongWord;a:VGfloat);
{Fill sets the fillcolor, defined as a RGBA quad}
var
 Color:TVGShapesColor;
begin
 {}
 VGShapesRGBA(r,g,b,a,Color);
 VGShapesSetFill(Color);
end;

{==============================================================================}

procedure VGShapesSetStops(paint:VGPaint;stops:PVGfloat;n:Integer);
{SetStops sets color stops for gradients}
var
 MultiMode:VGboolean;
 SpreadMode:VGColorRampSpreadMode;
begin
 {}
 {Set Defaults}
 MultiMode:=VG_FALSE;
 SpreadMode:=VG_COLOR_RAMP_SPREAD_REPEAT;
 
 {Set Parameters}
 vgSetParameteri(paint,VG_PAINT_COLOR_RAMP_SPREAD_MODE,SpreadMode);
 vgSetParameteri(paint,VG_PAINT_COLOR_RAMP_PREMULTIPLIED,MultiMode);
 vgSetParameterfv(paint,VG_PAINT_COLOR_RAMP_STOPS,5 * n,stops);
 vgSetPaint(paint,VG_FILL_PATH);
end;

{==============================================================================}

procedure VGShapesFillLinearGradient(x1,y1,x2,y2:VGfloat;stops:PVGfloat;ns:Integer);
{LinearGradient fills with a linear gradient}
var
 Paint:VGPaint;
 LinearCoordinates:array[0..3] of VGfloat;
begin
 {}
 {Setup Coordinates}
 LinearCoordinates[0]:=x1;
 LinearCoordinates[1]:=y1;
 LinearCoordinates[2]:=x2;
 LinearCoordinates[3]:=y2;
 
 {Create Paint}
 Paint:=vgCreatePaint;
 
 {Set Parameters}
 vgSetParameteri(Paint,VG_PAINT_TYPE,VG_PAINT_TYPE_LINEAR_GRADIENT);
 vgSetParameterfv(Paint,VG_PAINT_LINEAR_GRADIENT,4,@LinearCoordinates);
 
 {Set Stops}
 VGShapesSetStops(Paint,stops,ns);
 
 {Destroy Paint}
 vgDestroyPaint(Paint);
end;

{==============================================================================}

procedure VGShapesFillRadialGradient(cx,cy,fx,fy,radius:VGfloat;stops:PVGfloat;ns:Integer);
{RadialGradient fills with a linear gradient}
var
 Paint:VGPaint;
 RadialCoordinates:array[0..4] of VGfloat;
begin
 {}
 {Setup Coordinates}
 RadialCoordinates[0]:=cx;
 RadialCoordinates[1]:=cy;
 RadialCoordinates[2]:=fx;
 RadialCoordinates[3]:=fy;
 RadialCoordinates[4]:=radius;
 
 {Create Paint}
 Paint:=vgCreatePaint;
 
 {Set Parameters}
 vgSetParameteri(Paint,VG_PAINT_TYPE,VG_PAINT_TYPE_RADIAL_GRADIENT);
 vgSetParameterfv(Paint,VG_PAINT_RADIAL_GRADIENT,5,@RadialCoordinates);
 
 {Set Stops}
 VGShapesSetStops(Paint,stops,ns);
 
 {Destroy Paint}
 vgDestroyPaint(Paint);
end;

{==============================================================================}

procedure VGShapesClipRect(x,y,w,h:VGint);
{ClipRect limits the drawing area to specified rectangle}
var
 Coordinates:array[0..3] of VGint;
begin
 {}
 {Setup Coordinates}
 Coordinates[0]:=x;
 Coordinates[1]:=y;
 Coordinates[2]:=w;
 Coordinates[3]:=h;
 
 {Set Scissor}
 vgSeti(VG_SCISSORING,VG_TRUE);
 vgSetiv(VG_SCISSOR_RECTS,4,@Coordinates);
end;

{==============================================================================}

procedure VGShapesClipEnd; inline;
{ClipEnd stops limiting drawing area to specified rectangle}
begin
 {}
 vgSeti(VG_SCISSORING,VG_FALSE);
end;

{==============================================================================}
{Text}
function VGShapesNextUTF8Char(utf8:PChar;var codepoint:Integer):PChar;
{Next UTF8 char handles UTF encoding}
var
 Next:PChar;
 DataLength:Integer;
 SequenceLength:Integer;
begin
 {}
 Result:=nil;
 
 {Check UTF8}
 if utf8 = nil then Exit;
 
 {Get Next}
 Next:=utf8;
 
 {Get Data Length}
 DataLength:=StrLen(utf8);
 
 if (DataLength < 1) or (Next^ = #0) then Exit; {End of string}
 
 if (Ord(utf8[0]) and $80) = 0 then {0xxxxxxx}
  begin
   codepoint:=Ord(utf8[0]);
   SequenceLength:=1;
  end
 else if (Ord(utf8[0]) and $E0) = $C0 then {110xxxxx}
  begin
   codepoint:=((Ord(utf8[0]) and $1F) shl 6) or (Ord(utf8[1]) and $3F);
   SequenceLength:=2;
  end
 else if (Ord(utf8[0]) and $F0) = $E0 then {1110xxxx}
  begin
   codepoint:=((Ord(utf8[0]) and $0F) shl 12) or ((Ord(utf8[1]) and $3F) shl 6) or (Ord(utf8[2]) and $3F);
   SequenceLength:=3;
  end
 else
  begin
   Exit; {No code points this high here}
  end;  
 
 {Return Next} 
 Result:=Next + SequenceLength;
end;

{==============================================================================}

procedure VGShapesText(x,y:VGfloat;const s:UTF8String;const f:TVGShapesFontInfo;pointsize:Integer);
{Text renders a string of text at a specified location, size, using the specified font glyphs derived
 from http://web.archive.org/web/20070808195131/http://developer.hybrid.fi/font2openvg/renderFont.cpp.txt}
var 
 Size:VGfloat;
 Next:PChar;
 NextX:VGfloat;
 Glyph:Integer;
 Character:Integer;
 Matrix:array[0..8] of VGfloat;
 CharMatrix:array[0..8] of VGfloat;
begin
 {}
 {Setup Next}
 NextX:=x;
 
 {Setup Size}
 Size:=pointsize;
 
 {Get Matrix}
 vgGetMatrix(Matrix);
 
 {Get Character}
 Next:=PChar(s);
 Next:=VGShapesNextUTF8Char(Next,Character);
 while Next <> nil do
  begin
   {Get Glyph}
   Glyph:=f.CharacterMap[Character];
   if Glyph <> -1 then
    begin
     {Create Matrix}
     CharMatrix[0]:=Size;
     CharMatrix[1]:=0.0;
     CharMatrix[2]:=0.0;
     CharMatrix[3]:=0.0;
     CharMatrix[4]:=Size;
     CharMatrix[5]:=0.0;
     CharMatrix[6]:=NextX;
     CharMatrix[7]:=y;
     CharMatrix[8]:=1.0;
 
     {Load Matrix}
     vgLoadMatrix(Matrix);
     
     {Multiply Matrix}
     vgMultMatrix(CharMatrix);
     
     {Draw Path}
     vgDrawPath(f.Glyphs[Glyph],VG_FILL_PATH);
 
     {Update Next}
     NextX:=NextX + (Size * f.GlyphAdvances[Glyph] / 65536.0);
    end;
    
   {Get Next Character}
   Next:=VGShapesNextUTF8Char(Next,Character);
  end;
  
 {Load Matrix}
 vgLoadMatrix(Matrix);
end;

{==============================================================================}

function VGShapesTextWidth(const s:UTF8String;const f:TVGShapesFontInfo;pointsize:Integer):VGfloat;
{TextWidth returns the width of a text string at the specified font and size}
var
 Width:VGfloat;
 Size:VGfloat;
 Next:PChar;
 Glyph:Integer;
 Character:Integer;
begin
 {}
 Result:=0;
 
 {Setup Width}
 Width:=0.0;
 
 {Setup Size}
 Size:=pointsize;
 
 {Get Character}
 Next:=PChar(s);
 Next:=VGShapesNextUTF8Char(Next,Character);
 while Next <> nil do
  begin
   {Get Glyph}
   Glyph:=f.CharacterMap[Character];
   if Glyph <> -1 then
    begin
     {Update Width}
     Width:=Width + (Size * f.GlyphAdvances[Glyph] / 65536.0); 
    end;
   
   {Get Next Character}
   Next:=VGShapesNextUTF8Char(Next,Character);
  end;
  
 Result:=Width;
end;

{==============================================================================}

procedure VGShapesTextMid(x,y:VGfloat;const s:UTF8String;const f:TVGShapesFontInfo;pointsize:Integer);
{TextMid draws text, centered on (x,y)}
var
 Width:VGfloat;
begin
 {}
 {Get Width}
 Width:=VGShapesTextWidth(s,f,pointsize);
 
 {Draw Text}
 VGShapesText(x - (Width / 2.0),y,s,f,pointsize);
end;

{==============================================================================}

procedure VGShapesTextEnd(x,y:VGfloat;const s:UTF8String;const f:TVGShapesFontInfo;pointsize:Integer);
{TextEnd draws text, with its end aligned to (x,y)}
var
 Width:VGfloat;
begin
 {}
 {Get Width}
 Width:=VGShapesTextWidth(s,f,pointsize);
 
 {Draw Text}
 VGShapesText(x - Width,y,s,f,pointsize);
end;

{==============================================================================}

function VGShapesTextHeight(const f:TVGShapesFontInfo;pointsize:Integer):VGfloat;
{TextHeight reports a font's height}
begin
 {}
 Result:=(f.FontHeight * pointsize) / 65536;
end;

{==============================================================================}

function VGShapesTextDepth(const f:TVGShapesFontInfo;pointsize:Integer):VGfloat;
{TextDepth reports a font's depth (how far under the baseline it goes)}
begin
 {}
 Result:=(-f.DescenderHeight * pointsize) / 65536;
end;

{==============================================================================}
{Shape}
function VGShapesNewPath:VGPath; inline;
{Newpath creates path data}
begin
 {}
 Result:=vgCreatePath(VG_PATH_FORMAT_STANDARD,VG_PATH_DATATYPE_F,1.0,0.0,0,0,VG_PATH_CAPABILITY_APPEND_TO); {Other capabilities not needed}
end;

{==============================================================================}

procedure VGShapesMakeCurve(segments:PVGubyte;coords:PVGfloat;flags:VGbitfield);
{Makecurve makes path data using specified segments and coordinates}
var
 Path:VGPath;
begin
 {}
 {Create Path}
 Path:=VGShapesNewPath;
 
 {Append Path}
 vgAppendPathData(Path,2,segments,coords);
 
 {Draw Path}
 vgDrawPath(Path,flags);
 
 {Destroy Path}
 vgDestroyPath(Path);
end;

{==============================================================================}

procedure VGShapesCbezier(sx,sy,cx,cy,px,py,ex,ey:VGfloat);
{CBezier makes a quadratic bezier curve}
var
 Segments:array[0..1] of VGubyte;
 Coordinates:array[0..7] of VGfloat;
begin
 {}
 {Setup Segments}
 Segments[0]:=VG_MOVE_TO_ABS;
 Segments[1]:=VG_CUBIC_TO;

 {Setup Coordinates}
 Coordinates[0]:=sx;
 Coordinates[1]:=sy;
 Coordinates[2]:=cx;
 Coordinates[3]:=cy;
 Coordinates[4]:=px;
 Coordinates[5]:=py;
 Coordinates[6]:=ex;
 Coordinates[7]:=ey;
 
 {Make Curve}
 VGShapesMakeCurve(@Segments,@Coordinates,VG_FILL_PATH or VG_STROKE_PATH);
end;

{==============================================================================}

procedure VGShapesQbezier(sx,sy,cx,cy,ex,ey:VGfloat);
{QBezier makes a quadratic bezier curve}
var
 Segments:array[0..1] of VGubyte;
 Coordinates:array[0..5] of VGfloat;
begin
 {}
 {Setup Segments}
 Segments[0]:=VG_MOVE_TO_ABS;
 Segments[1]:=VG_QUAD_TO;

 {Setup Coordinates}
 Coordinates[0]:=sx;
 Coordinates[1]:=sy;
 Coordinates[2]:=cx;
 Coordinates[3]:=cy;
 Coordinates[4]:=ex;
 Coordinates[5]:=ey;
 
 {Make Curve}
 VGShapesMakeCurve(@Segments,@Coordinates,VG_FILL_PATH or VG_STROKE_PATH);
end;

{==============================================================================}

procedure VGShapesInterleave(x,y:PVGfloat;n:Integer;points:PVGfloat);
{Interleave interleaves arrays of x, y into a single array}
begin
 {}
 while n > 0 do
  begin
   {Update Points X}
   points^:=x^;
   Inc(points);
   Inc(x);
   
   {Update Points Y}
   points^:=y^;
   Inc(points);
   Inc(y);
   
   {Update n}
   Dec(n);
  end;
end;

{==============================================================================}

procedure VGShapesPoly(x,y:PVGfloat;n:VGint;flag:VGbitfield);
{Poly makes either a polygon or polyline}
var
 Path:VGPath;
 Points:array of VGfloat;
begin
 {}
 {Setup Points}
 SetLength(Points,n * 2);
 
 {Create Path}
 Path:=VGShapesNewPath;
 
 {Interleave Points}
 VGShapesInterleave(x,y,n,@Points[0]);
 
 {Create Polygon}
 vguPolygon(Path,@Points[0],n,VG_FALSE);
 
 {Draw Path}
 vgDrawPath(Path,flag);
 
 {Destroy Path}
 vgDestroyPath(Path);
end;

{==============================================================================}

procedure VGShapesPolygon(x,y:PVGfloat;n:VGint);
{Polygon makes a filled polygon with vertices in x, y arrays}
begin
 {}
 VGShapesPoly(x,y,n,VG_FILL_PATH);
end;

{==============================================================================}

procedure VGShapesPolyline(x,y:PVGfloat;n:VGint);
{Polyline makes a polyline with vertices at x, y arrays}
begin
 {}
 VGShapesPoly(x,y,n,VG_STROKE_PATH);
end;

{==============================================================================}

procedure VGShapesRect(x,y,w,h:VGfloat);
{Rect makes a rectangle at the specified location and dimensions}
var
 Path:VGPath;
begin
 {}
 {Create Path}
 Path:=VGShapesNewPath;
 
 {Create Rect}
 vguRect(Path,x,y,w,h);
 
 {Draw Path}
 vgDrawPath(Path,VG_FILL_PATH or VG_STROKE_PATH);
 
 {Destroy Path}
 vgDestroyPath(Path);
end;

{==============================================================================}

procedure VGShapesLine(x1,y1,x2,y2:VGfloat);
{Line makes a line from (x1,y1) to (x2,y2)}
var
 Path:VGPath;
begin
 {}
 {Create Path}
 Path:=VGShapesNewPath;
 
 {Create Line}
 vguLine(Path,x1,y1,x2,y2);
 
 {Draw Path}
 vgDrawPath(Path,VG_STROKE_PATH);
 
 {Destroy Path}
 vgDestroyPath(Path);
end;

{==============================================================================}

procedure VGShapesRoundrect(x,y,w,h,rw,rh:VGfloat);
{Roundrect makes an rounded rectangle at the specified location and dimensions}
var
 Path:VGPath;
begin
 {}
 {Create Path}
 Path:=VGShapesNewPath;
 
 {Create Round Rect}
 vguRoundRect(Path,x,y,w,h,rw,rh);
 
 {Draw Path}
 vgDrawPath(Path,VG_FILL_PATH or VG_STROKE_PATH);
 
 {Destroy Path}
 vgDestroyPath(Path);
end;

{==============================================================================}

procedure VGShapesEllipse(x,y,w,h:VGfloat);
{Ellipse makes an ellipse at the specified location and dimensions}
var
 Path:VGPath;
begin
 {}
 {Create Path}
 Path:=VGShapesNewPath;
 
 {Create Ellipse}
 vguEllipse(Path,x,y,w,h);
 
 {Draw Path}
 vgDrawPath(Path,VG_FILL_PATH or VG_STROKE_PATH);
 
 {Destroy Path}
 vgDestroyPath(Path);
end;

{==============================================================================}

procedure VGShapesCircle(x,y,r:VGfloat); inline;
{Circle makes a circle at the specified location and dimensions}
begin
 {}
 VGShapesEllipse(x,y,r,r);
end;

{==============================================================================}

procedure VGShapesArc(x,y,w,h,sa,aext:VGfloat);
{Arc makes an elliptical arc at the specified location and dimensions}
var
 Path:VGPath;
begin
 {}
 {Create Path}
 Path:=VGShapesNewPath;
 
 {Create Arc}
 vguArc(Path,x,y,w,h,sa,aext,VGU_ARC_OPEN);
 
 {Draw Path}
 vgDrawPath(Path,VG_FILL_PATH or VG_STROKE_PATH);
 
 {Destroy Path}
 vgDestroyPath(Path);
end;

{==============================================================================}
{Rendering}
procedure VGShapesDumpscreen(w,h:Integer;fp:THandle);
{Dumpscreen writes a raster of the current screen to a file}
var
 ScreenBuffer:Pointer;
begin
 {}
 {Allocate Buffer}
 ScreenBuffer:=GetMem(w * h * 4);
 
 {Read Pixels}
 vgReadPixels(ScreenBuffer,(w * 4),VG_sABGR_8888,0,0,w,h);
 
 {Write to File}
 FileWrite(fp,ScreenBuffer^,w * h * 4);
 
 {Free Buffer}
 FreeMem(ScreenBuffer);
end;

{==============================================================================}

procedure VGShapesStart(width,height:Integer);
{Start begins the picture, clearing a rectangular region with a specified color}
var
 Color:TVGShapesColor;
begin
 {}
 {Set Color}
 Color[0]:=1;
 Color[1]:=1;
 Color[2]:=1;
 Color[3]:=1;

 {Clear} 
 vgSetfv(VG_CLEAR_COLOR,4,@Color);
 vgClear(0,0,width,height);

 {Set Color}
 Color[0]:=0;
 Color[1]:=0;
 Color[2]:=0;
 
 {Set Fill and Stroke}
 VGShapesSetFill(Color);
 VGShapesSetStroke(Color);
 VGShapesStrokeWidth(0);

 vgLoadIdentity;
end;

{==============================================================================}

function VGShapesEnd:Boolean;
{End checks for errors, and renders to the display}
begin
 {}
 Result:=False;
 
 {Check Errors}
 if vgGetError <> VG_NO_ERROR then Exit;
 
 {Swap Buffers}
 eglSwapBuffers(VGShapesState.Display,VGShapesState.Surface);
 
 {Check Errors}
 if eglGetError <> EGL_SUCCESS then Exit;
 
 Result:=True;
end;

{==============================================================================}

function VGShapesSaveEnd(const filename:String):Boolean;
{SaveEnd dumps the raster before rendering to the display}
var
 Handle:THandle;
begin
 {}
 Result:=False;
 
 {Check Errors}
 if vgGetError <> VG_NO_ERROR then Exit;
 
 if Length(filename) <> 0 then
  begin
   {Create File}
   Handle:=FileCreate(filename);
   if Handle <> INVALID_HANDLE_VALUE then
    begin
     {Dump Screen}
     VGShapesDumpscreen(VGShapesState.ScreenWidth,VGShapesState.ScreenHeight,Handle);
     
     {Close File}
     FileClose(Handle);
    end;
  end;
 
 {Swap Buffers}
 eglSwapBuffers(VGShapesState.Display,VGShapesState.Surface);
 
 {Check Errors}
 if eglGetError <> EGL_SUCCESS then Exit;
 
 Result:=True;
end;

{==============================================================================}

procedure VGShapesBackground(r,g,b:LongWord);
{Backgroud clears the screen to a solid background color}
var
 Color:TVGShapesColor;
begin
 {}
 VGShapesRGB(r,g,b,Color);
 vgSetfv(VG_CLEAR_COLOR,4,@Color);
 vgClear(0,0,VGShapesState.WindowWidth,VGShapesState.WindowHeight);
end;

{==============================================================================}

procedure VGShapesBackgroundRGB(r,g,b:LongWord;a:VGfloat);
{BackgroundRGB clears the screen to a background color with alpha}
var
 Color:TVGShapesColor;
begin
 {}
 VGShapesRGBA(r,g,b,a,Color);
 vgSetfv(VG_CLEAR_COLOR,4,@Color);
 vgClear(0,0,VGShapesState.WindowWidth,VGShapesState.WindowHeight);
end;

{==============================================================================}

procedure VGShapesWindowClear; inline;
{WindowClear clears the window to previously set background colour}
begin
 {}
 vgClear(0,0,VGShapesState.WindowWidth,VGShapesState.WindowHeight);
end;

{==============================================================================}

procedure VGShapesAreaClear(x,y,w,h:LongWord); inline;
{AreaClear clears a given rectangle in window coordinates (not affected by transformations)}
begin
 {}
 vgClear(x,y,w,h);
end;

{==============================================================================}

procedure VGShapesWindowOpacity(a:LongWord);
{WindowOpacity sets the  window opacity}
begin
 {}
 DispmanXChangeWindowOpacity(@VGShapesState,a);
end;

{==============================================================================}

procedure VGShapesWindowPosition(x,y:Integer);
{WindowPosition moves the window to given position}
begin
 {}
 DispmanXMoveWindow(@VGShapesState,x,y);
end;

{==============================================================================}
{Outlined Shapes}
procedure VGShapesCbezierOutline(sx,sy,cx,cy,px,py,ex,ey:VGfloat);
{CBezier makes a quadratic bezier curve, stroked}
var
 Segments:array[0..1] of VGubyte;
 Coordinates:array[0..7] of VGfloat;
begin
 {}
 {Setup Segments}
 Segments[0]:=VG_MOVE_TO_ABS;
 Segments[1]:=VG_CUBIC_TO;

 {Setup Coordinates}
 Coordinates[0]:=sx;
 Coordinates[1]:=sy;
 Coordinates[2]:=cx;
 Coordinates[3]:=cy;
 Coordinates[4]:=px;
 Coordinates[5]:=py;
 Coordinates[6]:=ex;
 Coordinates[7]:=ey;

 {Make Curve}
 VGShapesMakeCurve(@Segments,@Coordinates,VG_STROKE_PATH);
end;

{==============================================================================}

procedure VGShapesQbezierOutline(sx,sy,cx,cy,ex,ey:VGfloat);
{QBezierOutline makes a quadratic bezier curve, outlined}
var
 Segments:array[0..1] of VGubyte;
 Coordinates:array[0..5] of VGfloat;
begin
 {}
 {Setup Segments}
 Segments[0]:=VG_MOVE_TO_ABS;
 Segments[1]:=VG_QUAD_TO;

 {Setup Coordinates}
 Coordinates[0]:=sx;
 Coordinates[1]:=sy;
 Coordinates[2]:=cx;
 Coordinates[3]:=cy;
 Coordinates[4]:=ex;
 Coordinates[5]:=ey;
 
 {Make Curve}
 VGShapesMakeCurve(@Segments,@Coordinates,VG_STROKE_PATH);
end;

{==============================================================================}

procedure VGShapesRectOutline(x,y,w,h:VGfloat);
{RectOutline makes a rectangle at the specified location and dimensions, outlined}
var
 Path:VGPath;
begin
 {}
 {Create Path}
 Path:=VGShapesNewPath;
 
 {Create Rect}
 vguRect(Path,x,y,w,h);
 
 {Draw Path}
 vgDrawPath(Path,VG_STROKE_PATH);
 
 {Destroy Path}
 vgDestroyPath(Path);
end;

{==============================================================================}

procedure VGShapesRoundrectOutline(x,y,w,h,rw,rh:VGfloat);
{RoundrectOutline  makes an rounded rectangle at the specified location and dimensions, outlined}
var
 Path:VGPath;
begin
 {}
 {Create Path}
 Path:=VGShapesNewPath;
 
 {Create Round Rect}
 vguRoundRect(Path,x,y,w,h,rw,rh);
 
 {Draw Path}
 vgDrawPath(Path,VG_STROKE_PATH);
 
 {Destroy Path}
 vgDestroyPath(Path);
end;

{==============================================================================}

procedure VGShapesEllipseOutline(x,y,w,h:VGfloat);
{EllipseOutline makes an ellipse at the specified location and dimensions, outlined}
var
 Path:VGPath;
begin
 {}
 {Create Path}
 Path:=VGShapesNewPath;
 
 {Create Ellipse}
 vguEllipse(Path,x,y,w,h);
 
 {Draw Path}
 vgDrawPath(Path, VG_STROKE_PATH);
 
 {Destroy Path}
 vgDestroyPath(Path);
end;

{==============================================================================}

procedure VGShapesCircleOutline(x,y,r:VGfloat); inline;
{CircleOutline makes a circle at the specified location and dimensions, outlined}
begin
 {}
 VGShapesEllipseOutline(x,y,r,r);
end;

{==============================================================================}

procedure VGShapesArcOutline(x,y,w,h,sa,aext:VGfloat);
{ArcOutline makes an elliptical arc at the specified location and dimensions, outlined}
var
 Path:VGPath;
begin
 {}
 {Create Path}
 Path:=VGShapesNewPath;
 
 {Create Arc}
 vguArc(Path,x,y,w,h,sa,aext,VGU_ARC_OPEN);
 
 {Draw Path}
 vgDrawPath(Path,VG_STROKE_PATH);
 
 {Destroy Path}
 vgDestroyPath(Path);
end;

{==============================================================================}
{==============================================================================}

end.
