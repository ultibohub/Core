{VGShapes Library:

  Ported to FreePascal by Garry Wood <garry@softoz.com.au>

  From C source available at https://github.com/ajstarks/openvg

  Multiple layer, app font and shared context support by Richard Metcalfe <richard@richmet.com>

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

{$IFNDEF FPC_DOTTEDUNITS}
unit VGShapes;
{$ENDIF FPC_DOTTEDUNITS}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Core.GlobalConst,
  Core.GlobalConfig,
  Core.Platform,
  System.SysUtils,
  System.Classes,
  Ultibo.EGL,
  Ultibo.DispmanX,
  Ultibo.OpenVG,
  Drivers.VC4,
  System.Jpeg.JPEGLib,
  System.Jpeg.Jerror,
  System.Jpeg.JDataSrc,
  System.Jpeg.JdAPImin,
  System.Jpeg.JdAPIstd;
{$ELSE FPC_DOTTEDUNITS}
uses
  GlobalConst,
  GlobalConfig,
  Platform,
  SysUtils,
  Classes,
  EGL,
  DispmanX,
  OpenVG,
  VC4,
  JPEGLib,
  Jerror,
  JDataSrc,
  JdAPImin,
  JdAPIstd;
{$ENDIF FPC_DOTTEDUNITS}

{==============================================================================}
const
 {Font Information}
 VGSHAPES_MAXFONTPATH = 500;
 VGSHAPES_MAXLAYERS = 16;

 VGSHAPES_NOLAYER = LongInt($80000000);
 VGSHAPES_NODISPLAY = LongWord(-1);

 VGSHAPES_FONTNAME_SANSSERIF = 'sans';
 VGSHAPES_FONTNAME_SERIF = 'serif';
 VGSHAPES_FONTNAME_MONO = 'mono';

 {Text Rotation}
 VGSHAPES_ROTATE_START = 0; {Rotate around the start point of the text}
 VGSHAPES_ROTATE_MID = 1;   {Rotate around the mid point of the text}
 VGSHAPES_ROTATE_END = 2;   {Rotate around the end point of the text}

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
  ReferenceCount:Integer;
 end;

 {Color Information}
 PVGShapesColor = ^TVGShapesColor;
 TVGShapesColor = array[0..3] of VGfloat;

{==============================================================================}
{Library Functions}
{Initialization}
procedure VGShapesInitLayerId(layerid:LongInt);
procedure VGShapesInitDisplayId(displayid:LongWord);
procedure VGShapesInitAlphaMaskSize(aphamasksize:LongInt);
procedure VGShapesInitWindowSize(x,y:Integer;w,h:LongWord);

function VGShapesInit(var w,h:Integer;alphaflags:DISPMANX_FLAGS_ALPHA_T = DISPMANX_FLAGS_ALPHA_FIXED_ALL_PIXELS;layer:LongInt = VGSHAPES_NOLAYER;ShareContextWithLayer:LongInt = VGSHAPES_NOLAYER):Boolean;
procedure VGShapesFinish;

function VGShapesGetLayer:LongInt;
procedure VGShapesSetLayer(layer:LongInt);

function VGShapesLayerHasSharedEGLContext(layer:LongInt = VGSHAPES_NOLAYER):Boolean;

{Font}
function VGShapesLoadFont(Points,PointIndices:PInteger;Instructions:PByte;InstructionIndices,InstructionCounts,adv:PInteger;cmap:PSmallInt;ng:Integer):TVGShapesFontInfo;
procedure VGShapesUnloadFont(glyphs:PVGPath;n:Integer);
function VGShapesLoadAppFont(const appfontname:String;
      Points,PointIndices:PInteger;Instructions:PByte;
      InstructionIndices,InstructionCounts,adv:PInteger;
      cmap:PSmallInt;ng:Integer;
      DescenderHeight:Integer;FontHeight:Integer):PVGShapesFontInfo;
function VGShapesLoadSharedAppFont(const appfontname:String):PVGShapesFontInfo;
procedure VGShapesUnloadAppFont(const appfontname:String);
function VGShapesGetAppFontByName(const appfontname:String;layer:LongInt = VGSHAPES_NOLAYER):PVGShapesFontinfo;

function VGShapesSansTypeface:PVGShapesFontInfo;
function VGShapesSerifTypeface:PVGShapesFontInfo;
function VGShapesMonoTypeface:PVGShapesFontInfo;

{Image}
procedure VGShapesMakeImage(x,y:VGfloat;w,h:Integer;data:PVGubyte);
procedure VGShapesImage(x,y:VGfloat;w,h:Integer;const filename:String);
function VGShapesCreateImageFromJpeg(const filename:String):VGImage;

{Transformation}
procedure VGShapesTranslate(x,y:VGfloat);
procedure VGShapesRotate(r:VGfloat);
procedure VGShapesShear(x,y:VGfloat);
procedure VGShapesScale(x,y:VGfloat);

{Style}
procedure VGShapesSetFill(const color:TVGShapesColor);
procedure VGShapesSetStroke(const color:TVGShapesColor);
procedure VGShapesStrokeWidth(width:VGfloat;cap:VGCapStyle = VG_CAP_BUTT;join:VGJoinStyle = VG_JOIN_MITER);

{Color}
procedure VGShapesRGBA(r,g,b:LongWord;a:VGfloat;var color:TVGShapesColor);
procedure VGShapesRGB(r,g,b:LongWord;var color:TVGShapesColor); inline;

procedure VGShapesStroke(r,g,b:LongWord;a:VGfloat);
procedure VGShapesFill(r,g,b:LongWord;a:VGfloat);
procedure VGShapesFillLinearGradient(x1,y1,x2,y2:VGfloat;stops:PVGfloat;ns:Integer);
procedure VGShapesFillRadialGradient(cx,cy,fx,fy,radius:VGfloat;stops:PVGfloat;ns:Integer);
procedure VGShapesClipRect(x,y,w,h:VGint);
procedure VGShapesClipEnd;

{Text}
procedure VGShapesText(x,y:VGfloat;const s:UTF8String;f:PVGShapesFontInfo;pointsize:Integer);
procedure VGShapesTextMid(x,y:VGfloat;const s:UTF8String;f:PVGShapesFontInfo;pointsize:Integer);
procedure VGShapesTextEnd(x,y:VGfloat;const s:UTF8String;f:PVGShapesFontInfo;pointsize:Integer);
procedure VGShapesTextRotate(x,y:VGfloat;const s:UTF8String;f:PVGShapesFontInfo;pointsize:Integer;angle:VGfloat;position:Integer);
function VGShapesTextWidth(const s:UTF8String;f:PVGShapesFontInfo;pointsize:Integer):VGfloat;
function VGShapesTextHeight(f:PVGShapesFontInfo;pointsize:Integer):VGfloat;
function VGShapesTextDepth(f:PVGShapesFontInfo;pointsize:Integer):VGfloat;

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
procedure VGShapesStart(width,height:Integer;transparent:Boolean = False);
function VGShapesEnd:Boolean;
function VGShapesSaveEnd(const filename:String):Boolean;
procedure VGShapesBackground(r,g,b:LongWord);
procedure VGShapesBackgroundRGB(r,g,b:LongWord;a:VGfloat);
procedure VGShapesWindowClear;
procedure VGShapesAreaClear(x,y,w,h:LongWord);
procedure VGShapesWindowLayer(layerid:LongInt);
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
  {Screen dimensions}
  ScreenWidth:LongWord;
  ScreenHeight:LongWord;
  ScreenPitch:LongWord;
  {Window dimensions}
  WindowX:LongInt;
  WindowY:LongInt;
  WindowWidth:LongWord;
  WindowHeight:LongWord;
  {Initialization}
  Initialized:Boolean;
  {DispmanX data}
  DisplayHandle:DISPMANX_DISPLAY_HANDLE_T;
  Element:DISPMANX_ELEMENT_HANDLE_T;
  {EGL data}
  Display:EGLDisplay;
  Surface:EGLSurface;
  Context:EGLContext;
  {EGL config}
  Alpha:VC_DISPMANX_ALPHA_T;
  NativeWindow:EGL_DISPMANX_WINDOW_T;
  AttributeList:array[0..31] of EGLint;
 end;

 {Application loaded font definition}
 TVGShapesAppFont = record
   Fontname:String;
   FontInfoP:PVGShapesFontInfo;
   IsInternal:Boolean;
 end;

 {Application Font list}
 TVGShapesAppFontList = array of TVGShapesAppFont;

 {Layer Information}
 PVGShapesLayer = ^TVGShapesLayer;
 TVGShapesLayer = record
  {State data}
  State:PEGLState;
  {Window size}
  InitX:Integer;
  InitY:Integer;
  InitW:LongWord;
  InitH:LongWord;
  {DispmanX Parameters}
  {Layer / Display Id}
  LayerId:LongInt;
  DisplayId:LongWord;
  {EGL Parameters}
  {Alpha Masking}
  AlphaMaskSize:LongInt;
  {Initialization}
  Initialized:Boolean;
  {Resource Sharing}
  SharedContextWithLayer:LongInt;
  {Font information}
  SansTypeface:PVGShapesFontInfo;
  SerifTypeface:PVGShapesFontInfo;
  MonoTypeface:PVGShapesFontInfo;
  {User loaded fonts list}
  AppFontList:TVGShapesAppFontList;
  AppFontListCount:LongWord;
 end;

{==============================================================================}
{==============================================================================}
var
 {Global Variables}
 Initialized:Boolean;

 Current:LongInt;
 Layers:array[0..VGSHAPES_MAXLAYERS - 1] of TVGShapesLayer;

{==============================================================================}
{Included Fonts}
{$INCLUDE .\vgfonts\DejaVuSans.inc}
{$INCLUDE .\vgfonts\DejaVuSansMono.inc}
{$INCLUDE .\vgfonts\DejaVuSerif.inc}

{==============================================================================}
{==============================================================================}
{Internal Functions}
{$IFDEF VGSHAPES_DEBUG}
procedure LogDebug(const AText:String);
begin
 {}
 LoggingOutput('[DEBUG] VGShapes: ' + AText);
end;
{$ENDIF}
{==============================================================================}

procedure InitLayers;
{Initialize the array of layers, only called during system start}
var
 Count:Integer;
begin
 {}
 for Count:=0 to VGSHAPES_MAXLAYERS - 1 do
  begin
   Layers[Count].LayerId:=VGSHAPES_NOLAYER;
   Layers[Count].DisplayId:=DISPMANX_ID_MAIN_LCD;
   Layers[Count].SharedContextWithLayer:=VGSHAPES_NOLAYER;

   {Initialize application font list}
   SetLength(Layers[Count].AppFontList,0);
   Layers[Count].AppFontListCount:=0;
  end;
end;

{==============================================================================}

function CountLayers(DisplayId:LongWord = VGSHAPES_NODISPLAY):LongWord;
{Count the number of currently initialized layers}
var
 Count:Integer;
begin
 {}
 Result:=0;

 for Count:=0 to VGSHAPES_MAXLAYERS - 1 do
  begin
   if Layers[Count].Initialized then
    begin
     if (DisplayId = VGSHAPES_NODISPLAY) or (DisplayId = Layers[Count].DisplayId) then
      begin
       Inc(Result);
      end;
    end;
  end;
end;

{==============================================================================}

function FirstLayer(DisplayId:LongWord = VGSHAPES_NODISPLAY):LongInt;
{Find the layer number of the first initialized layer}
var
 Count:Integer;
begin
 {}
 Result:=VGSHAPES_NOLAYER;

 for Count:=0 to VGSHAPES_MAXLAYERS - 1 do
  begin
   if Layers[Count].Initialized then
    begin
     if (DisplayId = VGSHAPES_NODISPLAY) or (DisplayId = Layers[Count].DisplayId) then
      begin
       Result:=Count;
       Exit;
      end;
    end;
  end;
end;

{==============================================================================}

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

function eglInit(State:PEGLState;AlphaFlags:DISPMANX_FLAGS_ALPHA_T;LayerId:LongInt;DisplayId:LongWord;AlphaMaskSize:LongInt;ShareContextWithLayer:LongInt):Boolean;
{eglInit sets the display, context and screen information, state holds the display information}
var
 First:LongInt;
 Offset:LongWord;
 Previous:PEGLState;

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
 State.Alpha.flags:=AlphaFlags;
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
 Offset:=8;
 if AlphaMaskSize > 0 then
  begin
   State.AttributeList[Offset]:=EGL_ALPHA_MASK_SIZE;
   State.AttributeList[Offset + 1]:=AlphaMaskSize;
   Inc(Offset,2);
  end;
 State.AttributeList[Offset]:=EGL_SURFACE_TYPE;
 State.AttributeList[Offset + 1]:=EGL_WINDOW_BIT;
 State.AttributeList[Offset + 2]:=EGL_RENDERABLE_TYPE;
 State.AttributeList[Offset + 3]:=EGL_OPENVG_BIT;
 State.AttributeList[Offset + 4]:=EGL_NONE;

 {Get Previous}
 Previous:=nil;
 First:=FirstLayer(DisplayId);
 if First <> VGSHAPES_NOLAYER then Previous:=Layers[First].State;

 try
  {Check Previous}
  if Previous = nil then
   begin
    {Get an EGL display connection}
    State.Display:=eglGetDisplay(EGL_DEFAULT_DISPLAY);
    if State.Display = EGL_NO_DISPLAY then Exit;

    {Initialize the EGL display connection}
    EGLResult:=eglInitialize(State.Display,nil,nil);
    if EGLResult = EGL_FALSE then Exit;

    {Bind OpenVG API}
    EGLResult:=eglBindAPI(EGL_OPENVG_API);
    if EGLResult = EGL_FALSE then Exit;
   end
  else
   begin
    {Copy Display}
    State.Display:=Previous.Display;
   end;

  {Get an appropriate EGL framebuffer configuration}
  EGLResult:=eglChooseConfig(State.Display,@State.AttributeList,@Config,1,@ConfigCount);
  if EGLResult = EGL_FALSE then Exit;

  {Create an EGL rendering context}
  if (ShareContextWithLayer = VGSHAPES_NOLAYER) then
   begin
    {$IFDEF VGSHAPES_DEBUG}
    LogDebug('Init layer ' + LayerId.ToString + ' using a non-shared context');
    {$ENDIF}

    State.Context:=eglCreateContext(State.Display,Config,EGL_NO_CONTEXT,nil)
   end
  else
   begin
    {$IFDEF VGSHAPES_DEBUG}
    LogDebug('Init shared context for layer ' + LayerId.ToString + ' (shared with layer ' + ShareContextWithLayer.ToString + ')');
    {$ENDIF}

    State.Context:=eglCreateContext(State.Display,Config,Layers[ShareContextWithLayer].State.Context,nil)
   end;
  if State.Context = EGL_NO_CONTEXT then Exit;

  {Get the current screen parameters}
  if BCMHostGraphicsGetDisplaySize(DisplayId,State.ScreenWidth,State.ScreenHeight) < 0 then Exit;
  State.ScreenPitch:=4 * ((State.WindowWidth + 15) and not(15));

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
  if Previous = nil then DispmanDisplay:=vc_dispmanx_display_open(DisplayId) else DispmanDisplay:=Previous.DisplayHandle;
  if DispmanDisplay = DISPMANX_NO_HANDLE then Exit;

  {Start Dispman Update}
  DispmanUpdate:=vc_dispmanx_update_start(0);
  if DispmanUpdate = DISPMANX_NO_HANDLE then Exit;

  {Add Dispman Element}
  DispmanElement:=vc_dispmanx_element_add(DispmanUpdate,DispmanDisplay,LayerId {Layer},@DestRect,0 {Source},@SourceRect,DISPMANX_PROTECTION_NONE,@State.Alpha,nil {Clamp},DISPMANX_NO_ROTATE {Transform});
  if DispmanElement = DISPMANX_NO_HANDLE then Exit;

  State.DisplayHandle:=DispmanDisplay;
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
    if State.Display <> EGL_NO_DISPLAY then
     begin
      {Check Layers}
      if CountLayers(DisplayId) > 0 then
       begin
        {Change Layer}
        VGShapesSetLayer(FirstLayer(DisplayId));
       end
      else
       begin
        {Terminate EGL}
        eglMakeCurrent(State.Display,EGL_NO_SURFACE,EGL_NO_SURFACE,EGL_NO_CONTEXT);
       end;

      {Check Element}
      if DispmanElement <> DISPMANX_NO_HANDLE then
       begin
        {Start Dispman Update}
        DispmanUpdate:=vc_dispmanx_update_start(0);
        if DispmanUpdate <> DISPMANX_NO_HANDLE then
         begin
          {Remove Dispman Element}
          vc_dispmanx_element_remove(DispmanUpdate,DispmanElement);

          {Submit Dispman Update}
          vc_dispmanx_update_submit_sync(DispmanUpdate);
         end;
       end;

      {Destroy Surface}
      if State.Surface <> EGL_NO_SURFACE then eglDestroySurface(State.Display,State.Surface);

      {Destroy Context}
      if State.Context <> EGL_NO_CONTEXT then eglDestroyContext(State.Display,State.Context);

      {Check Layers}
      if CountLayers(DisplayId) = 0 then
       begin
        {Close Dispman Display}
        if DispmanDisplay <> DISPMANX_NO_HANDLE then vc_dispmanx_display_close(DispmanDisplay);

        {Terminate Display}
        eglTerminate(State.Display);
       end;
     end;
   end;
 end;
end;

{==============================================================================}

procedure DispmanXMoveWindow(State:PEGLState;X,Y:Integer);
{DispmanXMoveWindow repositions an OpenVG window to given coords. Negative coords are allowed up to 1 - width, 1 - height,
 Maximum ScreenWidth - 1, ScreenHeight - 1. i.e. at least one pixel must be on the screen}
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

procedure DispmanXChangeWindowLayer(State:PEGLState;LayerId:LongInt);
{DispmanXChangeWindowLayer changes the layer id of the current window}
var
 DispmanUpdate:DISPMANX_UPDATE_HANDLE_T;
begin
 {}
 {Check State}
 if State = nil then Exit;

 {Start Update}
 DispmanUpdate:=vc_dispmanx_update_start(0);

 {Change Element}
 vc_dispmanx_element_change_layer(DispmanUpdate,State.Element,LayerId);

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
procedure VGShapesInitLayerId(layerid:LongInt);
{InitLayerId sets the underlying layer number passed to DispmanX, if not called then the current layer number will be used.
 This allows setting a layer number outside of the range of 0 to 15 so the layer can appear anywhere in the Z order}
begin
 {}
 if Layers[Current].Initialized then Exit;

 Layers[Current].LayerId:=layerid;
end;

{==============================================================================}

procedure VGShapesInitDisplayId(displayid:LongWord);
{InitDisplayId sets the underlying display id passed to DispmanX, if not called then the default display will be used.
 This allows setting a layer to appear on a different display where supported}
begin
 {}
 if Layers[Current].Initialized then Exit;

 Layers[Current].DisplayId:=displayid;
end;

{==============================================================================}

procedure VGShapesInitAlphaMaskSize(aphamasksize:LongInt);
{InitAlphaMaskSize sets the size of the alpha mask passed to EGL for the current layer, if not called then the default
 value is 0 which will disable the use of masking even if vgSeti is called with the VG_MASKING value set to VG_TRUE}
{See eglChooseConfig (https://www.khronos.org/registry/EGL/sdk/docs/man/html/eglChooseConfig.xhtml) for more information}
begin
 {}
 if Layers[Current].Initialized then Exit;

 {Size must not be negative}
 if aphamasksize < 0 then Exit;

 Layers[Current].AlphaMaskSize:=aphamasksize;
end;

{==============================================================================}

procedure VGShapesInitWindowSize(x,y:Integer;w,h:LongWord);
{InitWindowSize requests a specific window size & position, if not called then VGShapesInit() will open a full screen window}
begin
 {}
 if Layers[Current].Initialized then Exit;

 Layers[Current].InitX:=x;
 Layers[Current].InitY:=y;
 Layers[Current].InitW:=w;
 Layers[Current].InitH:=h;
end;

{==============================================================================}

function VGShapesInit(var w,h:Integer;alphaflags:DISPMANX_FLAGS_ALPHA_T = DISPMANX_FLAGS_ALPHA_FIXED_ALL_PIXELS;layer:LongInt = VGSHAPES_NOLAYER;ShareContextWithLayer:LongInt = VGSHAPES_NOLAYER):Boolean;
{Init performs host initialization and creates the current layer, must be called once for each layer to be created}

 procedure AddInternalFont(const appfontname:String;existingFontP:PVGShapesFontInfo);
 var
  Index:Integer;
 begin
  {$IFDEF VGSHAPES_DEBUG}
  LogDebug('Add internal font ' + appfontname + ' on layer ' + Current.ToString + ' at address ' + PtrToHex(existingfontp));
  {$ENDIF}

  {Update font count}
  Inc(Layers[Current].AppFontListCount);

  {Add space for application font}
  SetLength(Layers[Current].AppFontList,Layers[Current].AppFontListCount);

  {Insert font into application font list}
  Index:=Layers[Current].AppFontListCount - 1;
  Layers[Current].AppFontList[Index].Fontname:=appfontname;
  Layers[Current].AppFontList[Index].FontInfoP:=existingFontP;
  Layers[Current].AppFontList[Index].IsInternal:=True;
 end;

var
 First:Boolean;
begin
 {}
 Result:=False;

 {Setup Defaults}
 w:=-1;
 h:=-1;

 {Check Layer}
 if layer <> VGSHAPES_NOLAYER then
  begin
   if (layer < 0) or (layer >= VGSHAPES_MAXLAYERS) then Exit;

   {Set Layer}
   VGShapesSetLayer(layer);
  end;

 {Check share context layer}
 if (ShareContextWithLayer <> VGSHAPES_NOLAYER) then
  begin
   if (ShareContextWithLayer < 0) or (ShareContextWithLayer >= VGSHAPES_MAXLAYERS) then Exit;

   {Shared layer must have been initialised}
   if not Layers[ShareContextWithLayer].Initialized then Exit;
  end;

 {Check First Layer}
 First:=not(Initialized);

 {Sharing not possible until there is at least one context}
 if (First) and (ShareContextWithLayer <> VGSHAPES_NOLAYER) then Exit;

 {Initialize Host}
 if First then BCMHostInit;

 {Check Initialized}
 if not Layers[Current].Initialized then
  begin
   {Allocate State}
   Layers[Current].State:=AllocMem(SizeOf(TEGLState));
   if Layers[Current].State = nil then Exit;

   {Initialize State}
   Layers[Current].State.WindowX:=Layers[Current].InitX;
   Layers[Current].State.WindowY:=Layers[Current].InitY;
   Layers[Current].State.WindowWidth:=Layers[Current].InitW;
   Layers[Current].State.WindowHeight:=Layers[Current].InitH;
   if Layers[Current].LayerId = VGSHAPES_NOLAYER then Layers[Current].LayerId:=Current;
   Layers[Current].SharedContextWithLayer:=ShareContextWithLayer;

   {Initialize EGL}
   if not eglInit(Layers[Current].State,alphaflags,Layers[Current].LayerId,Layers[Current].DisplayId,Layers[Current].AlphaMaskSize,Layers[Current].SharedContextWithLayer) then
    begin
     if First then BCMHostDeinit;
     Exit;
    end;

   Layers[Current].Initialized:=True;

   {Note: Layer will be made current by eglInit}

   if First then
    begin
     {Allocate Sans Font}
     Layers[Current].SansTypeface:=AllocMem(SizeOf(TVGShapesFontInfo));
     if Layers[Current].SansTypeface <> nil then
      begin
       {Load Sans Font}
       Layers[Current].SansTypeface^:=VGShapesLoadFont(@DejaVuSans_glyphPoints,
                                                       @DejaVuSans_glyphPointIndices,
                                                       @DejaVuSans_glyphInstructions,
                                                       @DejaVuSans_glyphInstructionIndices,
                                                       @DejaVuSans_glyphInstructionCounts,
                                                       @DejaVuSans_glyphAdvances,
                                                       @DejaVuSans_characterMap,
                                                       DejaVuSans_glyphCount);
       Layers[Current].SansTypeface.DescenderHeight:=DejaVuSans_descender_height;
       Layers[Current].SansTypeface.FontHeight:=DejaVuSans_font_height;
       Layers[Current].SansTypeface.ReferenceCount:=1;

       AddInternalFont(VGSHAPES_FONTNAME_SANSSERIF,Layers[Current].SansTypeface);
      end;

     {Allocate Serif Font}
     Layers[Current].SerifTypeface:=AllocMem(SizeOf(TVGShapesFontInfo));
     if Layers[Current].SerifTypeface <> nil then
      begin
       {Load Serif Font}
       Layers[Current].SerifTypeface^:=VGShapesLoadFont(@DejaVuSerif_glyphPoints,
                                                        @DejaVuSerif_glyphPointIndices,
                                                        @DejaVuSerif_glyphInstructions,
                                                        @DejaVuSerif_glyphInstructionIndices,
                                                        @DejaVuSerif_glyphInstructionCounts,
                                                        @DejaVuSerif_glyphAdvances,
                                                        @DejaVuSerif_characterMap,
                                                        DejaVuSerif_glyphCount);
       Layers[Current].SerifTypeface.DescenderHeight:=DejaVuSerif_descender_height;
       Layers[Current].SerifTypeface.FontHeight:=DejaVuSerif_font_height;
       Layers[Current].SerifTypeface.ReferenceCount:=1;

       AddInternalFont(VGSHAPES_FONTNAME_SERIF,Layers[Current].SerifTypeface);
      end;

     {Allocate Mono Font}
     Layers[Current].MonoTypeface:=AllocMem(SizeOf(TVGShapesFontInfo));
     if Layers[Current].MonoTypeface <> nil then
      begin
       {Load Mono Font}
       Layers[Current].MonoTypeface^:=VGShapesLoadFont(@DejaVuSansMono_glyphPoints,
                                                       @DejaVuSansMono_glyphPointIndices,
                                                       @DejaVuSansMono_glyphInstructions,
                                                       @DejaVuSansMono_glyphInstructionIndices,
                                                       @DejaVuSansMono_glyphInstructionCounts,
                                                       @DejaVuSansMono_glyphAdvances,
                                                       @DejaVuSansMono_characterMap,
                                                       DejaVuSansMono_glyphCount);
       Layers[Current].MonoTypeface.DescenderHeight:=DejaVuSansMono_descender_height;
       Layers[Current].MonoTypeface.FontHeight:=DejaVuSansMono_font_height;
       Layers[Current].MonoTypeface.ReferenceCount:=1;

       AddInternalFont(VGSHAPES_FONTNAME_MONO,Layers[Current].MonoTypeface);
      end;

     Initialized:=True;
    end
    else
    begin
     {Add internal fonts to list, not created until used}
     AddInternalFont(VGSHAPES_FONTNAME_SANSSERIF,nil);
     AddInternalFont(VGSHAPES_FONTNAME_SERIF,nil);
     AddInternalFont(VGSHAPES_FONTNAME_MONO,nil);
    end;

   {Return Result}
   Result:=True;
  end;

 {Return Window Size}
 w:=Layers[Current].State.WindowWidth;
 h:=Layers[Current].State.WindowHeight;
end;

{==============================================================================}

procedure VGShapesFinish;
{Finish cleans up and removes the current layer}
var
 Layer:LongInt;
 DispmanUpdate:DISPMANX_UPDATE_HANDLE_T;
 Index:Integer;
begin
 {}
 if not Layers[Current].Initialized then Exit;

 {Save Layer}
 Layer:=Current;

 {Unload internal fonts (Unloaded and freed below)}
 Layers[Layer].SansTypeface:=nil;
 Layers[Layer].SerifTypeface:=nil;
 Layers[Layer].MonoTypeface:=nil;

 {$IFDEF VGSHAPES_DEBUG}
 LogDebug('Finish layer ' + Layer.ToString + ' appFontList length='+ IntToStr(Length(Layers[Layer].AppFontList)));
 {$ENDIF}

 {Unload any application fonts}
 for Index:=0 to Layers[Layer].AppFontListCount - 1 do
  begin
   {$IFDEF VGSHAPES_DEBUG}
   LogDebug('Finish layer ' + Layer.ToString + ' appFontList dispose index=' + Index.ToString + ' name=' + Layers[Layer].AppFontList[Index].Fontname);
   {$ENDIF}

   if Layers[Layer].AppFontList[Index].FontInfoP <> nil then
   begin
     {Decrement reference count}
     if Layers[Layer].AppFontList[Index].FontInfoP^.ReferenceCount > 0 then
     begin
       Dec(Layers[Layer].AppFontList[Index].FontInfoP^.ReferenceCount);
     end;

     {Release memory when no references remaining}
     if Layers[Layer].AppFontList[Index].FontInfoP^.ReferenceCount = 0 then
     begin
       {$IFDEF VGSHAPES_DEBUG}
       LogDebug('Finish layer ' + Layer.ToString + ' reference count of font ' + Layers[Layer].AppFontList[Index].Fontname + ' is 0, releasing memory and unloading font');
       {$ENDIF}

       {Unload font}
       VGShapesUnloadFont(Layers[Layer].AppFontList[Index].FontInfoP^.Glyphs,Layers[Layer].AppFontList[Index].FontInfoP^.Count);

       FreeMem(Layers[Layer].AppFontList[Index].FontInfoP);
     end
     else
     begin
      {$IFDEF VGSHAPES_DEBUG}
      LogDebug('Finish layer ' + Layer.ToString + ' reference count of font ' + Layers[Layer].AppFontList[Index].Fontname + ' is still ' + IntToStr(Layers[Layer].AppFontList[Index].FontInfoP^.ReferenceCount));
      {$ENDIF}
     end;
   end
   else
   begin
    {$IFDEF VGSHAPES_DEBUG}
    LogDebug('Finish layer ' + Layer.ToString + ' not disposing of font ' + Layers[Layer].AppFontList[Index].Fontname + ' as it was never initialised');
    {$ENDIF}
   end;
  end;
 SetLength(Layers[Layer].AppFontList,0);
 Layers[Layer].AppFontListCount:=0;

 {Check whether other layers use this layer as a share conext and remove it}
 for Index:=0 to VGSHAPES_MAXLAYERS - 1 do
 begin
  if Layers[Index].SharedContextWithLayer = Layer then
    Layers[Index].SharedContextWithLayer:=VGSHAPES_NOLAYER;
 end;

 Layers[Layer].Initialized:=False;

 {Swap Buffers}
 eglSwapBuffers(Layers[Layer].State.Display,Layers[Layer].State.Surface);

 {Check Layers}
 if CountLayers(Layers[Layer].DisplayId) > 0 then
  begin
   {Change Layer}
   VGShapesSetLayer(FirstLayer(Layers[Layer].DisplayId));
  end
 else
  begin
   {Terminate EGL}
   eglMakeCurrent(Layers[Layer].State.Display,EGL_NO_SURFACE,EGL_NO_SURFACE,EGL_NO_CONTEXT);
  end;

 {Check Element}
 if Layers[Layer].State.Element <> DISPMANX_NO_HANDLE then
  begin
   {Start Dispman Update}
   DispmanUpdate:=vc_dispmanx_update_start(0);
   if DispmanUpdate = DISPMANX_NO_HANDLE then Exit;

   {Remove Dispman Element}
   vc_dispmanx_element_remove(DispmanUpdate,Layers[Layer].State.Element);

   {Submit Dispman Update}
   vc_dispmanx_update_submit_sync(DispmanUpdate);
  end;

 {Destroy Surface}
 if Layers[Layer].State.Surface <> EGL_NO_SURFACE then eglDestroySurface(Layers[Layer].State.Display,Layers[Layer].State.Surface);

 {Destroy Context}
 if Layers[Layer].State.Context <> EGL_NO_CONTEXT then eglDestroyContext(Layers[Layer].State.Display,Layers[Layer].State.Context);

 {Check Layers}
 if CountLayers(Layers[Layer].DisplayId) = 0 then
  begin
   {Close Dispman Display}
   if Layers[Layer].State.DisplayHandle <> DISPMANX_NO_HANDLE then vc_dispmanx_display_close(Layers[Layer].State.DisplayHandle);

   {Terminate Display}
   eglTerminate(Layers[Layer].State.Display);
  end;

 {Free State}
 FreeMem(Layers[Layer].State);

 {Reset Layer}
 FillChar(Layers[Layer],SizeOf(TVGShapesLayer),0);
 Layers[Layer].LayerId:=VGSHAPES_NOLAYER;
 Layers[Layer].DisplayId:=DISPMANX_ID_MAIN_LCD;
 Layers[Layer].SharedContextWithLayer:=VGSHAPES_NOLAYER;
end;

{==============================================================================}

function VGShapesGetLayer:LongInt;
{GetLayer returns the layer that is currently selected for VGShapes operations}
begin
 {}
 Result:=Current;
end;

{==============================================================================}

procedure VGShapesSetLayer(layer:LongInt);
{SetLayer sets the current layer for all VGShapes operations, drawing can only occur on the currently selected layer}
var
 EGLResult:EGLBoolean;
begin
 {}
 {Check Layer}
 if (layer < 0) or (layer >= VGSHAPES_MAXLAYERS) then Exit;

 {Make the layer current}
 if (Layers[layer].Initialized) then
  begin
   EGLResult:=eglMakeCurrent(Layers[layer].State.Display,Layers[layer].State.Surface,Layers[layer].State.Surface,Layers[layer].State.Context);
   if EGLResult = EGL_FALSE then Exit;
  end;

 {Change the current layer even if not initialized, so that you can call
  VGShapesSetLayer followed by VGShapesInit to initialize a specific layer}
 Current:=layer;
end;

{==============================================================================}

function VGShapesLayerHasSharedEGLContext(layer:LongInt = VGSHAPES_NOLAYER):Boolean;
begin
 Result:=False;

 {Use current layer if none specified}
 if (layer = VGSHAPES_NOLAYER) then
  layer:=Current;

 {Check Layer}
 if (layer < 0) or (layer >= VGSHAPES_MAXLAYERS) then Exit;

 {Check layer initialized}
 if not Layers[layer].Initialized then Exit;

 Result:=Layers[layer].SharedContextWithLayer <> VGSHAPES_NOLAYER;
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

 {Check Initialized}
 if not Layers[Current].Initialized then Exit;

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
 {Check Initialized}
 if not Layers[Current].Initialized then Exit;

 {Check Glyphs}
 if glyphs = nil then Exit;

 for Count:=0 to n - 1 do
  begin
   {Destroy Path}
   vgDestroyPath(glyphs[Count]);
  end;
end;

{==============================================================================}

function VGShapesSansTypeface:PVGShapesFontInfo;
{Return a pointer to the Sans Typeface for the current layer}
var
 Index:Integer;
begin
 {}
 Result:=nil;

 {Check Initialized}
 if not Layers[Current].Initialized then Exit;

 {Check egl context sharing}
 if (Layers[Current].SharedContextWithLayer <> VGSHAPES_NOLAYER) then
  begin
   {$IFDEF VGSHAPES_DEBUG}
   LogDebug('SansTypeface layer ' + Current.ToString + ' has shared context');
   {$ENDIF}

   {Check font reference not already copied}
   if (Layers[Current].SansTypeFace = nil) then
    begin
     {Check font shared context layer has a font already created}
     if (Layers[Layers[Current].SharedContextWithLayer].SansTypeface <> nil) then
      begin
       {$IFDEF VGSHAPES_DEBUG}
       LogDebug('SansTypeface layer ' + Current.ToString + ', assiging shared font from layer ' + IntToStr(Layers[Current].SharedContextWithLayer));
       {$ENDIF}

       Layers[Current].SansTypeface:=Layers[Layers[Current].SharedContextWithLayer].SansTypeface;

       {Update app font entry with font info pointer}
       for Index:=0 to Layers[Current].AppFontListCount - 1 do
        begin
         if Lowercase(Layers[Current].AppFontList[Index].Fontname) = VGSHAPES_FONTNAME_SANSSERIF then
          begin
           {$IFDEF VGSHAPES_DEBUG}
           LogDebug('SansTypeface updating app font list for layer ' + Current.ToString + ' font ' + VGSHAPES_FONTNAME_SANSSERIF + ', incrementing reference count of sans typeface at address ' + PtrToHex(Layers[Current].SansTypeface));
           {$ENDIF}

           Layers[Current].AppFontList[Index].FontInfoP:=Layers[Current].SansTypeface;
           Inc(Layers[Current].SansTypeface.ReferenceCount);
           Break;
          end;
        end;

       {$IFDEF VGSHAPES_DEBUG}
       LogDebug('SansTypeface reference count of layer ' + IntToStr(Layers[Current].SharedContextWithLayer) + ' set to ' + IntToStr(Layers[Layers[Current].SharedContextWithLayer].SansTypeface.ReferenceCount));
       {$ENDIF}
      end
     else
      begin
       {Shared context layer's font not allocated yet, allocate a local copy only}
       {$IFDEF VGSHAPES_DEBUG}
       LogDebug('SansTypeface no font allocated for shared context layer, allocating local font');
       {$ENDIF}
      end;
    end
   else
    begin
     {$IFDEF VGSHAPES_DEBUG}
     LogDebug('SansTypeface already assigned, shared address ' + PtrToHex(Layers[Layers[Current].SharedContextWithLayer].SansTypeface) + ' current address ' + PtrToHex(Layers[Current].SansTypeface));
     {$ENDIF}
    end;
  end;

 {Check Sans Font}
 if Layers[Current].SansTypeface = nil then
  begin
   {Allocate Sans Font}
   Layers[Current].SansTypeface:=AllocMem(SizeOf(TVGShapesFontInfo));
   if Layers[Current].SansTypeface = nil then Exit;

   {Load Sans Font}
   Layers[Current].SansTypeface^:=VGShapesLoadFont(@DejaVuSans_glyphPoints,
                                                   @DejaVuSans_glyphPointIndices,
                                                   @DejaVuSans_glyphInstructions,
                                                   @DejaVuSans_glyphInstructionIndices,
                                                   @DejaVuSans_glyphInstructionCounts,
                                                   @DejaVuSans_glyphAdvances,
                                                   @DejaVuSans_characterMap,
                                                   DejaVuSans_glyphCount);
   Layers[Current].SansTypeface.DescenderHeight:=DejaVuSans_descender_height;
   Layers[Current].SansTypeface.FontHeight:=DejaVuSans_font_height;
   Layers[Current].SansTypeface.ReferenceCount:=1;

   {Update appfontlist entry}
   for Index:=0 to Layers[Current].AppFontListCount - 1 do
    begin
     if Lowercase(Layers[Current].AppFontList[Index].Fontname) = VGSHAPES_FONTNAME_SANSSERIF then
      begin
       Layers[Current].AppFontList[Index].FontInfoP:=Layers[Current].SansTypeface;
       Break;
      end;
    end;

   {$IFDEF VGSHAPES_DEBUG}
   LogDebug('SansTypeface layer ' + Current.ToString + ', allocated sans typeface at address ' + PtrToHex(Layers[Current].SansTypeface));
   {$ENDIF}
  end;

 Result:=Layers[Current].SansTypeface;
end;

{==============================================================================}

function VGShapesSerifTypeface:PVGShapesFontInfo;
{Return a pointer to the Serif Typeface for the current layer}
var
 Index:Integer;
begin
 {}
 Result:=nil;

 {Check Initialized}
 if not Layers[Current].Initialized then Exit;

 {Check egl context sharing}
 if (Layers[Current].SharedContextWithLayer <> VGSHAPES_NOLAYER) then
  begin
   {$IFDEF VGSHAPES_DEBUG}
   LogDebug('SerifTypeface layer ' + Current.ToString + ' has shared context');
   {$ENDIF}

   {Check font reference not already copied}
   if (Layers[Current].SerifTypeFace = nil) then
    begin
     {Check font shared context layer has a font already created}
     if (Layers[Layers[Current].SharedContextWithLayer].SerifTypeface <> nil) then
      begin
       {$IFDEF VGSHAPES_DEBUG}
       LogDebug('SerifTypeface layer ' + Current.ToString + ', assiging shared font from layer ' + IntToStr(Layers[Current].SharedContextWithLayer));
       {$ENDIF}

       Layers[Current].SerifTypeface:=Layers[Layers[Current].SharedContextWithLayer].SerifTypeface;

       {Update app font entry with font info pointer}
       for Index:=0 to Layers[Current].AppFontListCount - 1 do
        begin
         if Lowercase(Layers[Current].AppFontList[Index].Fontname) = VGSHAPES_FONTNAME_SERIF then
          begin
           {$IFDEF VGSHAPES_DEBUG}
           LogDebug('SerifTypeface updating app font list for layer ' + Current.ToString + ' font ' + VGSHAPES_FONTNAME_SERIF + ', incrementing reference count of serif typeface at address ' + PtrToHex(Layers[Current].SerifTypeface));
           {$ENDIF}

           Layers[Current].AppFontList[Index].FontInfoP:=Layers[Current].SerifTypeface;
           Inc(Layers[Current].SerifTypeface.ReferenceCount);
           Break;
          end;
        end;

       {$IFDEF VGSHAPES_DEBUG}
       LogDebug('SerifTypeface reference count of layer ' + IntToStr(Layers[Current].SharedContextWithLayer) + ' set to ' + IntToStr(Layers[Layers[Current].SharedContextWithLayer].SerifTypeface.ReferenceCount));
       {$ENDIF}
      end
     else
      begin
       {Shared context layer's font not allocated yet, allocate a local copy only}
       {$IFDEF VGSHAPES_DEBUG}
       LogDebug('SerifTypeface no font allocated for shared context layer, allocating local font');
       {$ENDIF}
      end;
    end
   else
    begin
     {$IFDEF VGSHAPES_DEBUG}
     LogDebug('SerifTypeface already assigned, shared address ' + PtrToHex(Layers[Layers[Current].SharedContextWithLayer].SerifTypeface) + ' current address ' + PtrToHex(Layers[Current].SerifTypeface));
     {$ENDIF}
    end;
  end;

 {Check Serif Font}
 if Layers[Current].SerifTypeface = nil then
  begin
   {Allocate Serif Font}
   Layers[Current].SerifTypeface:=AllocMem(SizeOf(TVGShapesFontInfo));
   if Layers[Current].SerifTypeface = nil then Exit;

   {Load Serif Font}
   Layers[Current].SerifTypeface^:=VGShapesLoadFont(@DejaVuSerif_glyphPoints,
                                                    @DejaVuSerif_glyphPointIndices,
                                                    @DejaVuSerif_glyphInstructions,
                                                    @DejaVuSerif_glyphInstructionIndices,
                                                    @DejaVuSerif_glyphInstructionCounts,
                                                    @DejaVuSerif_glyphAdvances,
                                                    @DejaVuSerif_characterMap,
                                                    DejaVuSerif_glyphCount);
   Layers[Current].SerifTypeface.DescenderHeight:=DejaVuSerif_descender_height;
   Layers[Current].SerifTypeface.FontHeight:=DejaVuSerif_font_height;
   Layers[Current].SerifTypeface.ReferenceCount:=1;

   {Update appfontlist entry}
   for Index:=0 to Layers[Current].AppFontListCount - 1 do
    begin
     if Lowercase(Layers[Current].AppFontList[Index].Fontname) = VGSHAPES_FONTNAME_SERIF then
      begin
       Layers[Current].AppFontList[Index].FontInfoP:=Layers[Current].SerifTypeface;
       Break;
      end;
    end;

   {$IFDEF VGSHAPES_DEBUG}
   LogDebug('SerifTypeface layer ' + Current.ToString + ', allocated serif typeface at address ' + PtrToHex(Layers[Current].SerifTypeface));
   {$ENDIF}
  end;

 Result:=Layers[Current].SerifTypeface;
end;

{==============================================================================}

function VGShapesMonoTypeface:PVGShapesFontInfo;
{Return a pointer to the Mono Typeface for the current layer}
var
 Index:Integer;
begin
 {}
 Result:=nil;

 {Check Initialized}
 if not Layers[Current].Initialized then Exit;

 {Check egl context sharing}
 if (Layers[Current].SharedContextWithLayer <> VGSHAPES_NOLAYER) then
  begin
   {$IFDEF VGSHAPES_DEBUG}
   LogDebug('MonoTypeface layer ' + Current.ToString + ' has shared context');
   {$ENDIF}

   {Check font reference not already copied}
   if (Layers[Current].MonoTypeFace = nil) then
    begin
     {Check font shared context layer has a font already created}
     if (Layers[Layers[Current].SharedContextWithLayer].MonoTypeface <> nil) then
      begin
       {$IFDEF VGSHAPES_DEBUG}
       LogDebug('MonoTypeface layer ' + Current.ToString + ', assiging shared font from layer ' + IntToStr(Layers[Current].SharedContextWithLayer));
       {$ENDIF}

       Layers[Current].MonoTypeface:=Layers[Layers[Current].SharedContextWithLayer].MonoTypeface;

       {Update app font entry with font info pointer}
       for Index:=0 to Layers[Current].AppFontListCount - 1 do
        begin
         if Lowercase(Layers[Current].AppFontList[Index].Fontname) = VGSHAPES_FONTNAME_MONO then
          begin
           {$IFDEF VGSHAPES_DEBUG}
           LogDebug('MonoTypeface updating app font list for layer ' + Current.ToString + ' font ' + VGSHAPES_FONTNAME_MONO + ', incrementing reference count of mono typeface at address ' + PtrToHex(Layers[Current].MonoTypeface));
           {$ENDIF}

           Layers[Current].AppFontList[Index].FontInfoP:=Layers[Current].MonoTypeface;
           Inc(Layers[Current].MonoTypeface.ReferenceCount);
           Break;
          end;
        end;

       {$IFDEF VGSHAPES_DEBUG}
       LogDebug('MonoTypeface reference count of layer ' + IntToStr(Layers[Current].SharedContextWithLayer) + ' set to ' + IntToStr(Layers[Layers[Current].SharedContextWithLayer].MonoTypeface.ReferenceCount));
       {$ENDIF}
      end
     else
      begin
       {Shared context layer's font not allocated yet, allocate a local copy only}
       {$IFDEF VGSHAPES_DEBUG}
       LogDebug('MonoTypeface no font allocated for shared context layer, allocating local font');
       {$ENDIF}
      end;
    end
   else
    begin
     {$IFDEF VGSHAPES_DEBUG}
     LogDebug('MonoTypeface already assigned, shared address ' + PtrToHex(Layers[Layers[Current].SharedContextWithLayer].MonoTypeface) + ' current address ' + PtrToHex(Layers[Current].MonoTypeface));
     {$ENDIF}
    end;
  end;

 {Check Mono Font}
 if Layers[Current].MonoTypeface = nil then
  begin
   {Allocate Mono Font}
   Layers[Current].MonoTypeface:=AllocMem(SizeOf(TVGShapesFontInfo));
   if Layers[Current].MonoTypeface = nil then Exit;

   {Load Mono Font}
   Layers[Current].MonoTypeface^:=VGShapesLoadFont(@DejaVuSansMono_glyphPoints,
                                                   @DejaVuSansMono_glyphPointIndices,
                                                   @DejaVuSansMono_glyphInstructions,
                                                   @DejaVuSansMono_glyphInstructionIndices,
                                                   @DejaVuSansMono_glyphInstructionCounts,
                                                   @DejaVuSansMono_glyphAdvances,
                                                   @DejaVuSansMono_characterMap,
                                                   DejaVuSansMono_glyphCount);
   Layers[Current].MonoTypeface.DescenderHeight:=DejaVuSansMono_descender_height;
   Layers[Current].MonoTypeface.FontHeight:=DejaVuSansMono_font_height;
   Layers[Current].MonoTypeface.ReferenceCount:=1;

   {Update appfontlist entry}
   for Index:=0 to Layers[Current].AppFontListCount - 1 do
    begin
     if Lowercase(Layers[Current].AppFontList[Index].Fontname) = VGSHAPES_FONTNAME_MONO then
      begin
       Layers[Current].AppFontList[Index].FontInfoP:=Layers[Current].MonoTypeface;
       Break;
      end;
    end;

   {$IFDEF VGSHAPES_DEBUG}
   LogDebug('MonoTypeface layer ' + Current.ToString + ', allocated mono typeface at address ' + PtrToHex(Layers[Current].MonoTypeface));
   {$ENDIF}
  end;

 Result:=Layers[Current].MonoTypeface;
end;

{==============================================================================}

function VGShapesLoadSharedAppFont(const appfontname:String):PVGShapesFontInfo;
{Makes a font loaded into a different layer accessible from the current layer,
 PROVIDED a share context request has been made when the current layer was created,
 AND the font was first loaded into the other layer with VGShapesLoadAppFont().

 Example:
 VGShapesSetLayer(0);
 VGShapesInit(width, height);
 VGShapesLoadAppFont(fontname,...) //with all font definition parameters set
 VGShapesInit(width, height, alphaflags, 1, 0); // create layer 1, share with layer 0
 VGShapesLoadSharedAppFont(fontname); // font becomes accessible on this layer without
                                      // loading a duplicate into GPU memory.
}
var
 appFontInfoP:PVGShapesFontInfo;
 Index:Integer;
begin
 Result:=nil;

 {Check Initialized}
 if not Layers[Current].Initialized then Exit;

 {Check existing font}
 Result:=VGShapesGetAppFontByName(appfontname,Current);
 if Result <> nil then Exit;

 {Check shared context}
 if (Layers[Current].SharedContextWithLayer <> VGSHAPES_NOLAYER) then
 begin
  {Locate font}
  appFontInfoP:=VGShapesGetAppFontByName(appfontname,Layers[Current].SharedContextWithLayer);
  if (appFontInfoP = nil) then Exit;

  {$IFDEF VGSHAPES_DEBUG}
  LogDebug('LoadSharedAppFont ' + appfontname + ' using shared context on layer ' + Current.ToString);
  {$ENDIF}

  {Increment reference count}
  Inc(appFontInfoP^.ReferenceCount);

  {$IFDEF VGSHAPES_DEBUG}
  LogDebug('LoadSharedAppFont reference count for ' + appfontname + ' is now ' + IntToStr(appFontInfoP^.ReferenceCount));
  {$ENDIF}

  {Update font count}
  Inc(Layers[Current].AppFontListCount);

  {Add space for application font}
  SetLength(Layers[Current].AppFontList,Layers[Current].AppFontListCount);

  {Insert font into application font list}
  Index:=Layers[Current].AppFontListCount - 1;
  Layers[Current].AppFontList[Index].Fontname:=appfontname;
  Layers[Current].AppFontList[Index].FontInfoP:=appFontInfoP;
  Layers[Current].AppFontList[Index].IsInternal:=false;

  {Return font}
  Result:=appFontInfoP;
 end;
end;

{==============================================================================}

function VGShapesLoadAppFont(const appfontname:String;
      Points,PointIndices:PInteger;Instructions:PByte;
      InstructionIndices,InstructionCounts,adv:PInteger;
      cmap:PSmallInt;ng:Integer;
      DescenderHeight:Integer;FontHeight:Integer):PVGShapesFontInfo;
{Load the supplied font info as the specified font name in the current layer}
{If there is a shared egl context specified, the parameter to this function
 except for the font name are ignored and the shared reference is used instead}
var
 appFontInfoP:PVGShapesFontInfo;
 Index:Integer;
begin
 {}
 Result:=nil;

 {Set Defaults}
 appFontInfoP:=nil;

 {Check Initialized}
 if not Layers[Current].Initialized then Exit;

 {Check existing font}
 Result:=VGShapesGetAppFontByName(appfontname,Current);
 if Result <> nil then Exit;

 {Check shared context}
 if (Layers[Current].SharedContextWithLayer <> VGSHAPES_NOLAYER) then
 begin
  appFontInfoP:=VGShapesLoadSharedAppFont(appfontname);
 end;

 if appFontInfoP = nil then
 begin
  {Allocate memory for font}
  appfontInfoP:=AllocMem(SizeOf(TVGShapesFontInfo));
  if appfontInfoP = nil then Exit;

  {Load the font}
  appFontInfoP^:=VGShapesLoadFont(Points,
                                  PointIndices,
                                  Instructions,
                                  InstructionIndices,
                                  InstructionCounts,
                                  adv,
                                  cmap,
                                  ng);
  appFontInfoP^.DescenderHeight:=DescenderHeight;
  appFontInfoP^.FontHeight:=FontHeight;
  appFontInfoP^.ReferenceCount:=1;

  {Update font count}
  Inc(Layers[Current].AppFontListCount);

  {Add space for application font}
  SetLength(Layers[Current].AppFontList,Layers[Current].AppFontListCount);

  {Insert font into application font list}
  Index:=Layers[Current].AppFontListCount - 1;
  Layers[Current].AppFontList[Index].Fontname:=appfontname;
  Layers[Current].AppFontList[Index].FontInfoP:=appFontInfoP;
  Layers[Current].AppFontList[Index].IsInternal:=false;

  {$IFDEF VGSHAPES_DEBUG}
  LogDebug('LoadAppFont ' + appfontname + ' on layer ' + Current.ToString + ', allocated font at address ' + PtrToHex(appFontInfoP));
  {$ENDIF}
 end;

 {Return font}
 Result:=appFontInfoP;
end;

{==============================================================================}

procedure VGShapesUnloadAppFont(const appfontname:String);
{Unload the named font from the current layer}
var
 Index:Integer;
 Found:Boolean;
 FontName:String;
begin
 {Check Initialized}
 if not Layers[Current].Initialized then Exit;

 {Find index of font}
 Index:=0;
 Found:=False;
 FontName:=Lowercase(appfontname);
 while Index < Layers[Current].AppFontListCount do
  begin
   if Lowercase(Layers[Current].AppFontList[Index].Fontname) = FontName then
    begin
     Found:=True;
     Break;
    end;

   Inc(Index);
  end;

 if Found then
  begin
   {Release font from GPU}
   if Layers[Current].AppFontList[Index].FontInfoP <> nil then
    begin
     {Check for internal font}
     if Layers[Current].AppFontList[Index].IsInternal then
      begin
       if Layers[Current].AppFontList[Index].FontInfoP = Layers[Current].SansTypeface then
        Layers[Current].SansTypeface:=nil
       else if Layers[Current].AppFontList[Index].FontInfoP = Layers[Current].SerifTypeface then
        Layers[Current].SerifTypeface:=nil
       else if Layers[Current].AppFontList[Index].FontInfoP = Layers[Current].MonoTypeface then
        Layers[Current].MonoTypeface:=nil;
      end;

     {Decrement reference count}
     if Layers[Current].AppFontList[Index].FontInfoP^.ReferenceCount > 0 then
      begin
       Dec(Layers[Current].AppFontList[Index].FontInfoP^.ReferenceCount);
      end;

     {Release memory when no references remaining}
     if Layers[Current].AppFontList[Index].FontInfoP^.ReferenceCount = 0 then
      begin
       {$IFDEF VGSHAPES_DEBUG}
       LogDebug('UnloadAppFont unloading font ' + appfontname + ' from gpu memory');
       {$ENDIF}

       {Unload font}
       VGShapesUnloadFont(Layers[Current].AppFontList[Index].FontInfoP^.Glyphs,Layers[Current].AppFontList[Index].FontInfoP^.Count);

       {$IFDEF VGSHAPES_DEBUG}
       LogDebug('UnloadAppFont ref count of ' + Layers[Current].AppFontList[Index].Fontname + ' is 0, releasing memory layer=' + Current.ToString);
       {$ENDIF}

       FreeMem(Layers[Current].AppFontList[Index].FontInfoP);
      end
      else
      begin
       {$IFDEF VGSHAPES_DEBUG}
       LogDebug('UnloadAppFont ref count still ' + IntToStr(Layers[Current].AppFontList[Index].FontInfoP^.ReferenceCount) + ' for ' + Layers[Current].AppFontList[Index].Fontname + ' layer=' + Current.ToString);
       {$ENDIF}
      end;
    end
   else
    begin
     {$IFDEF VGSHAPES_DEBUG}
     LogDebug('UnloadAppFont font ' + appfontname+' was never initialised layer=' + Current.ToString);
     {$ENDIF}
    end;

   {Remove gap for entry if not the last one}
   if Index < Layers[Current].AppFontListCount - 1 then
    begin
     Move(Layers[Current].AppFontList[Index + 1],Layers[Current].AppFontList[Index],(Layers[Current].AppFontListCount - (Index + 1)) * SizeOf(TVGShapesAppFont));
    end;

   {Reduce font count}
   Dec(Layers[Current].AppFontListCount);

   {Reduce size of list}
   SetLength(Layers[Current].AppFontList,Layers[Current].AppFontListCount);
  end
 else
  begin
   {$IFDEF VGSHAPES_DEBUG}
   LogDebug('UnloadAppFont font ' + appfontname + ' not found, layer ' + Current.ToString);
   {$ENDIF}
  end;
end;

{==============================================================================}

function VGShapesGetAppFontByName(const appfontname:String;layer:LongInt = VGSHAPES_NOLAYER):PVGShapesFontinfo;
{Return a pointer to the named font for the specified layer (or the current layer if none specified)}
var
 Index:Integer;
 FontName:String;
begin
 {}
 Result := nil;

 {Check layer}
 if (layer = VGSHAPES_NOLAYER) then
  layer:=Current;

 {Check Layer}
 if (layer < 0) or (layer >= VGSHAPES_MAXLAYERS) then Exit;

 {Check Initialized}
 if not Layers[layer].Initialized then Exit;

 {Find font}
 FontName:=Lowercase(appfontname);
 for Index:=0 to Layers[layer].AppFontListCount - 1 do
  begin
   if Lowercase(Layers[layer].AppFontList[Index].Fontname) = FontName then
    begin
     {Create internal fonts if necessary}
     if (layer = Current) and (Layers[layer].AppFontList[Index].IsInternal) and (Layers[layer].AppFontList[Index].FontInfoP = nil) then
      begin
       if FontName = VGSHAPES_FONTNAME_SANSSERIF then
        Layers[layer].AppFontList[Index].FontInfoP:=VGShapesSansTypeface
       else if FontName = VGSHAPES_FONTNAME_SERIF then
        Layers[layer].AppFontList[Index].FontInfoP:=VGShapesSerifTypeface
       else if FontName = VGSHAPES_FONTNAME_MONO then
        Layers[layer].AppFontList[Index].FontInfoP:=VGShapesMonoTypeface;
      end;

     Result:=Layers[layer].AppFontList[Index].FontInfoP;
     Exit;
    end;
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
 {Check Initialized}
 if not Layers[Current].Initialized then Exit;

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
 {Check Initialized}
 if not Layers[Current].Initialized then Exit;

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

 {Check Initialized}
 if not Layers[Current].Initialized then Exit;

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

 {Check if the file exists}
 if not FileExists(Filename) then Exit;

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
procedure VGShapesTranslate(x,y:VGfloat);
{Translate the coordinate system to x,y}
begin
 {}
 {Check Initialized}
 if not Layers[Current].Initialized then Exit;

 vgTranslate(x,y);
end;

{==============================================================================}

procedure VGShapesRotate(r:VGfloat);
{Rotate around angle r}
begin
 {}
 {Check Initialized}
 if not Layers[Current].Initialized then Exit;

 vgRotate(r);
end;

{==============================================================================}

procedure VGShapesShear(x,y:VGfloat);
{Shear shears the x coordinate by x degrees, the y coordinate by y degrees}
begin
 {}
 {Check Initialized}
 if not Layers[Current].Initialized then Exit;

 vgShear(x,y);
end;

{==============================================================================}

procedure VGShapesScale(x,y:VGfloat);
{Scale scales by  x, y}
begin
 {}
 {Check Initialized}
 if not Layers[Current].Initialized then Exit;

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
 {Check Initialized}
 if not Layers[Current].Initialized then Exit;

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
 {Check Initialized}
 if not Layers[Current].Initialized then Exit;

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

procedure VGShapesStrokeWidth(width:VGfloat;cap:VGCapStyle = VG_CAP_BUTT;join:VGJoinStyle = VG_JOIN_MITER);
{StrokeWidth sets the stroke width, cap style and join style}
begin
 {}
 {Check Initialized}
 if not Layers[Current].Initialized then Exit;

 vgSetf(VG_STROKE_LINE_WIDTH,width);
 vgSeti(VG_STROKE_CAP_STYLE,cap);
 vgSeti(VG_STROKE_JOIN_STYLE,join);
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
 {Check Initialized}
 if not Layers[Current].Initialized then Exit;

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
 {Check Initialized}
 if not Layers[Current].Initialized then Exit;

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
 {Check Initialized}
 if not Layers[Current].Initialized then Exit;

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
 {Check Initialized}
 if not Layers[Current].Initialized then Exit;

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

procedure VGShapesClipEnd;
{ClipEnd stops limiting drawing area to specified rectangle}
begin
 {}
 {Check Initialized}
 if not Layers[Current].Initialized then Exit;

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

procedure VGShapesText(x,y:VGfloat;const s:UTF8String;f:PVGShapesFontInfo;pointsize:Integer);
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
 {Check Initialized}
 if not Layers[Current].Initialized then Exit;

 {Check Font}
 if f = nil then Exit;

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

procedure VGShapesTextMid(x,y:VGfloat;const s:UTF8String;f:PVGShapesFontInfo;pointsize:Integer);
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

procedure VGShapesTextEnd(x,y:VGfloat;const s:UTF8String;f:PVGShapesFontInfo;pointsize:Integer);
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

procedure VGShapesTextRotate(x,y:VGfloat;const s:UTF8String;f:PVGShapesFontInfo;pointsize:Integer;angle:VGfloat;position:Integer);
{TextRotate renders a string of text at a specified location, size and angle using the specified font glyphs}
var
 Width:VGfloat;
 Matrix:array[0..8] of VGfloat;
begin
 {}
 {Setup Angle}
 if angle > 360 then angle:=0;

 {Save Matrix}
 vgGetMatrix(Matrix);

 {Translate}
 vgTranslate(x,y);

 {Rotate}
 vgRotate(angle);

 {Get Width}
 Width:=VGShapesTextWidth(s,f,pointsize);

 {Check Position}
 if position = VGSHAPES_ROTATE_MID then
  begin
   {Draw Text}
   VGShapesText(0 - (Width / 2.0),0,s,f,pointsize);
  end
 else if position = VGSHAPES_ROTATE_END then
  begin
   {Draw Text}
   VGShapesText(0 - Width,0,s,f,pointsize);
  end
 else
  begin
   {Draw Text}
   VGShapesText(0,0,s,f,pointsize);
  end;

 {Restore Matrix}
 vgLoadMatrix(Matrix);
end;

{==============================================================================}

function VGShapesTextWidth(const s:UTF8String;f:PVGShapesFontInfo;pointsize:Integer):VGfloat;
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

 {Check Font}
 if f = nil then Exit;

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

function VGShapesTextHeight(f:PVGShapesFontInfo;pointsize:Integer):VGfloat;
{TextHeight reports a font's height}
begin
 {}
 Result:=0;

 {Check Font}
 if f = nil then Exit;

 Result:=(f.FontHeight * pointsize) / 65536;
end;

{==============================================================================}

function VGShapesTextDepth(f:PVGShapesFontInfo;pointsize:Integer):VGfloat;
{TextDepth reports a font's depth (how far under the baseline it goes)}
begin
 {}
 Result:=0;

 {Check Font}
 if f = nil then Exit;

 Result:=(-f.DescenderHeight * pointsize) / 65536;
end;

{==============================================================================}
{Shape}
function VGShapesNewPath:VGPath; inline;
{Newpath creates path data}
begin
 {}
 Result:=VG_INVALID_HANDLE;

 {Check Initialized}
 if not Layers[Current].Initialized then Exit;

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
 if Path = VG_INVALID_HANDLE then Exit;

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
 if Path = VG_INVALID_HANDLE then Exit;

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
 if Path = VG_INVALID_HANDLE then Exit;

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
 if Path = VG_INVALID_HANDLE then Exit;

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
 if Path = VG_INVALID_HANDLE then Exit;

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
 if Path = VG_INVALID_HANDLE then Exit;

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
 if Path = VG_INVALID_HANDLE then Exit;

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
 {Check Initialized}
 if not Layers[Current].Initialized then Exit;

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

procedure VGShapesStart(width,height:Integer;transparent:Boolean = False);
{Start begins the picture, clearing a rectangular region with a specified color}
var
 Color:TVGShapesColor;
begin
 {}
 {Check Initialized}
 if not Layers[Current].Initialized then Exit;

 {Set Color}
 Color[0]:=1;
 Color[1]:=1;
 Color[2]:=1;
 if transparent then Color[3]:=0 else Color[3]:=1;

 {Clear}
 vgSetfv(VG_CLEAR_COLOR,4,@Color);
 vgClear(0,0,width,height);

 {Set Color}
 Color[0]:=0;
 Color[1]:=0;
 Color[2]:=0;
 Color[3]:=1;

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

 {Check Initialized}
 if not Layers[Current].Initialized then Exit;

 {Check Errors}
 if vgGetError <> VG_NO_ERROR then Exit;

 {Swap Buffers}
 eglSwapBuffers(Layers[Current].State.Display,Layers[Current].State.Surface);

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

 {Check Initialized}
 if not Layers[Current].Initialized then Exit;

 {Check Errors}
 if vgGetError <> VG_NO_ERROR then Exit;

 if Length(filename) <> 0 then
  begin
   {Create File}
   Handle:=FileCreate(filename);
   if Handle <> INVALID_HANDLE_VALUE then
    begin
     {Dump Screen}
     VGShapesDumpscreen(Layers[Current].State.ScreenWidth,Layers[Current].State.ScreenHeight,Handle);

     {Close File}
     FileClose(Handle);
    end;
  end;

 {Swap Buffers}
 eglSwapBuffers(Layers[Current].State.Display,Layers[Current].State.Surface);

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
 {Check Initialized}
 if not Layers[Current].Initialized then Exit;

 VGShapesRGB(r,g,b,Color);
 vgSetfv(VG_CLEAR_COLOR,4,@Color);
 vgClear(0,0,Layers[Current].State.WindowWidth,Layers[Current].State.WindowHeight);
end;

{==============================================================================}

procedure VGShapesBackgroundRGB(r,g,b:LongWord;a:VGfloat);
{BackgroundRGB clears the screen to a background color with alpha}
var
 Color:TVGShapesColor;
begin
 {}
 {Check Initialized}
 if not Layers[Current].Initialized then Exit;

 VGShapesRGBA(r,g,b,a,Color);
 vgSetfv(VG_CLEAR_COLOR,4,@Color);
 vgClear(0,0,Layers[Current].State.WindowWidth,Layers[Current].State.WindowHeight);
end;

{==============================================================================}

procedure VGShapesWindowClear;
{WindowClear clears the window to previously set background colour}
begin
 {}
 {Check Initialized}
 if not Layers[Current].Initialized then Exit;

 vgClear(0,0,Layers[Current].State.WindowWidth,Layers[Current].State.WindowHeight);
end;

{==============================================================================}

procedure VGShapesAreaClear(x,y,w,h:LongWord);
{AreaClear clears a given rectangle in window coordinates (not affected by transformations)}
begin
 {}
 {Check Initialized}
 if not Layers[Current].Initialized then Exit;

 vgClear(x,y,w,h);
end;

{==============================================================================}

procedure VGShapesWindowLayer(layerid:LongInt);
{WindowLayer changes the layer id of the current window}
begin
 {}
 {Check Initialized}
 if not Layers[Current].Initialized then Exit;

 DispmanXChangeWindowLayer(Layers[Current].State,layerid);

 Layers[Current].LayerId:=layerid;
end;

{==============================================================================}

procedure VGShapesWindowOpacity(a:LongWord);
{WindowOpacity changes the opacity of the current window}
begin
 {}
 {Check Initialized}
 if not Layers[Current].Initialized then Exit;

 DispmanXChangeWindowOpacity(Layers[Current].State,a);
end;

{==============================================================================}

procedure VGShapesWindowPosition(x,y:Integer);
{WindowPosition moves the current window to the given position}
begin
 {}
 {Check Initialized}
 if not Layers[Current].Initialized then Exit;

 DispmanXMoveWindow(Layers[Current].State,x,y);
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
 if Path = VG_INVALID_HANDLE then Exit;

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
 if Path = VG_INVALID_HANDLE then Exit;

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
 if Path = VG_INVALID_HANDLE then Exit;

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
 if Path = VG_INVALID_HANDLE then Exit;

 {Create Arc}
 vguArc(Path,x,y,w,h,sa,aext,VGU_ARC_OPEN);

 {Draw Path}
 vgDrawPath(Path,VG_STROKE_PATH);

 {Destroy Path}
 vgDestroyPath(Path);
end;

{==============================================================================}
{==============================================================================}

initialization
 InitLayers;

{==============================================================================}

{finalization}
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
