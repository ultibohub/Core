{
Ultibo Web Status unit.

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

 
References
==========

 

Web Status
==========

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit WebStatus;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,{$IFDEF CPUARM}PlatformARM,{$ENDIF}Threads,SysUtils,Classes,Ultibo,UltiboClasses,UltiboUtils,Winsock2,HTTP,
     HeapManager,Devices,USB,MMC,Network,Transport,Protocol,Storage,FileSystem,Keyboard,Mouse,Console,Framebuffer,Font,Logging,Timezone,Locale,Unicode;

//To Do //Look for:

//--
     
{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {Web Status specific constants}
 RtlMaxUnits = 1024; {See maxunits in system.inc}
 
{==============================================================================}
type
 {Web Status specific types}
 TRtlInitFinalRec = record {See TInitFinalRec in system.inc}
  InitProc:TProcedure;
  FinalProc:TProcedure;
 end;
 
 TRtlInitFinalTable = record {See TInitFinalTable in system.inc}
  TableCount:LongWord;
  InitCount:LongWord;
  Procs:array[1..RtlMaxUnits] of TRtlInitFinalRec;
 end;
 PRtlInitFinalTable = ^TRtlInitFinalTable;
 
{==============================================================================}
type
 {Web Status specific clases}
 TWebStatusSub = class;
 TWebStatusMain = class(THTTPDocument)
 public
  {}
  constructor Create;
  destructor Destroy; override;
 private
  {Internal Variables}
  FTitle:String;
  FCaption:String; //To Do //Hash ?
  FSubPages:TStringList; //To Do //TLinkedStringList ?
  
  {Internal Methods}
  function GetTitle:String;
  procedure SetTitle(const ATitle:String);
  function GetCaption:String;
 protected
  {Internal Variables}

  {Internal Methods}
  function MakeBold(const AName:String):String;
  function MakeLink(const AName,ALink:String):String;
  
  function AddBlank(AResponse:THTTPServerResponse):Boolean;
  function AddBlankEx(AResponse:THTTPServerResponse;AColumns:LongWord):Boolean;

  function AddItem(AResponse:THTTPServerResponse;const AName,AValue:String):Boolean;
  function AddItemEx(AResponse:THTTPServerResponse;const AName,AValue:String;AIndent:LongWord):Boolean;

  function AddItem3Column(AResponse:THTTPServerResponse;const AName,AValue1,AValue2:String):Boolean;
  function AddItem4Column(AResponse:THTTPServerResponse;const AName,AValue1,AValue2,AValue3:String):Boolean;
  function AddItem5Column(AResponse:THTTPServerResponse;const AName,AValue1,AValue2,AValue3,AValue4:String):Boolean;
  
  function AddBold(AResponse:THTTPServerResponse;const AName,AValue:String):Boolean;
  function AddBoldEx(AResponse:THTTPServerResponse;const AName,AValue:String;AIndent:LongWord):Boolean;
  
  function AddBold3Column(AResponse:THTTPServerResponse;const AName,AValue1,AValue2:String):Boolean;
  function AddBold4Column(AResponse:THTTPServerResponse;const AName,AValue1,AValue2,AValue3:String):Boolean;
  function AddBold5Column(AResponse:THTTPServerResponse;const AName,AValue1,AValue2,AValue3,AValue4:String):Boolean;
  
  function AddHeader(AResponse:THTTPServerResponse;const ATitle:String;ASub:TWebStatusSub):Boolean;
  function AddHeaderEx(AResponse:THTTPServerResponse;const ATitle:String;ASub:TWebStatusSub;AColumns:LongWord):Boolean;
  
  function AddFooter(AResponse:THTTPServerResponse):Boolean;
  function AddFooterEx(AResponse:THTTPServerResponse;AColumns:LongWord):Boolean;
  
  function AddContent(AResponse:THTTPServerResponse;const AContent:String):Boolean;
  
  function DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
 public
  {Public Properties}
  property Title:String read GetTitle write SetTitle;
  property Caption:String read GetCaption;
  
  {Public Methods}
  function RegisterSubPage(ASub:TWebStatusSub):Boolean;
  function DeregisterSubPage(ASub:TWebStatusSub):Boolean;
 end;
 
 TWebStatusSub = class(THTTPDocument)
 public
  {}
  constructor Create(AMain:TWebStatusMain);
  destructor Destroy; override;
 private
  {Internal Variables}
  FMain:TWebStatusMain;
  
  {Internal Methods}
  function GetTitle:String;
  function GetCaption:String;
 protected
  {Internal Variables}
  FCaption:String; //To Do //Hash ?
  
  {Internal Methods}
  function MakeBold(const AName:String):String;
  function MakeLink(const AName,ALink:String):String;

  function AddBlank(AResponse:THTTPServerResponse):Boolean;
  function AddBlankEx(AResponse:THTTPServerResponse;AColumns:LongWord):Boolean;
  
  function AddItem(AResponse:THTTPServerResponse;const AName,AValue:String):Boolean;
  function AddItemEx(AResponse:THTTPServerResponse;const AName,AValue:String;AIndent:LongWord):Boolean;

  function AddItem3Column(AResponse:THTTPServerResponse;const AName,AValue1,AValue2:String):Boolean;
  function AddItem4Column(AResponse:THTTPServerResponse;const AName,AValue1,AValue2,AValue3:String):Boolean;
  function AddItem5Column(AResponse:THTTPServerResponse;const AName,AValue1,AValue2,AValue3,AValue4:String):Boolean;
  
  function AddBold(AResponse:THTTPServerResponse;const AName,AValue:String):Boolean;
  function AddBoldEx(AResponse:THTTPServerResponse;const AName,AValue:String;AIndent:LongWord):Boolean;
  
  function AddBold3Column(AResponse:THTTPServerResponse;const AName,AValue1,AValue2:String):Boolean;
  function AddBold4Column(AResponse:THTTPServerResponse;const AName,AValue1,AValue2,AValue3:String):Boolean;
  function AddBold5Column(AResponse:THTTPServerResponse;const AName,AValue1,AValue2,AValue3,AValue4:String):Boolean;
  
  function AddHeader(AResponse:THTTPServerResponse;const ATitle:String;ASub:TWebStatusSub):Boolean;
  function AddHeaderEx(AResponse:THTTPServerResponse;const ATitle:String;ASub:TWebStatusSub;AColumns:LongWord):Boolean;
  
  function AddFooter(AResponse:THTTPServerResponse):Boolean;
  function AddFooterEx(AResponse:THTTPServerResponse;AColumns:LongWord):Boolean;
  
  function AddContent(AResponse:THTTPServerResponse;const AContent:String):Boolean;
 
  function DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
 public
  {Public Properties}
  property Main:TWebStatusMain read FMain;
  property Caption:String read GetCaption;
  
  {Public Methods}
  
 end;
 
 TWebStatusPlatform = class(TWebStatusSub)
 public
  {}
  constructor Create(AMain:TWebStatusMain);
 private
  {Internal Variables}
  
 protected
  {Internal Variables}
 
  {Internal Methods}
 
  function DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
 public
  {Public Properties}
 
  {Public Methods}
  
 end;
 
 TWebStatusMemory = class(TWebStatusSub)
 public
  {}
  constructor Create(AMain:TWebStatusMain);
 private
  {Internal Variables}
  
 protected
  {Internal Variables}
 
  {Internal Methods}
 
  function DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
 public
  {Public Properties}
 
  {Public Methods}
  
 end;

 TWebStatusHeap = class(TWebStatusSub)
 public
  {}
  constructor Create(AMain:TWebStatusMain);
 private
  {Internal Variables}
  function FlagsToFlagName(AFlags:LongWord):String;
  
 protected
  {Internal Variables}
 
  {Internal Methods}
 
  function DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
 public
  {Public Properties}
 
  {Public Methods}
  
 end;
 
 TWebStatusCPU = class(TWebStatusSub)
 public
  {}
  constructor Create(AMain:TWebStatusMain);
 private
  {Internal Variables}
  
 protected
  {Internal Variables}
 
  {Internal Methods}
 
  function DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
 public
  {Public Properties}
 
  {Public Methods}
  
 end;

 TWebStatusFPU = class(TWebStatusSub)
 public
  {}
  constructor Create(AMain:TWebStatusMain);
 private
  {Internal Variables}
  
 protected
  {Internal Variables}
 
  {Internal Methods}
 
  function DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
 public
  {Public Properties}
 
  {Public Methods}
  
 end;

 TWebStatusGPU = class(TWebStatusSub)
 public
  {}
  constructor Create(AMain:TWebStatusMain);
 private
  {Internal Variables}
  
 protected
  {Internal Variables}
 
  {Internal Methods}
 
  function DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
 public
  {Public Properties}
 
  {Public Methods}
  
 end;

 TWebStatusRTL = class(TWebStatusSub)
 public
  {}
  constructor Create(AMain:TWebStatusMain);
 private
  {Internal Variables}
  
 protected
  {Internal Variables}
 
  {Internal Methods}
 
  function DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
 public
  {Public Properties}
 
  {Public Methods}
  
 end;
 
 TWebStatusClock = class(TWebStatusSub)
 public
  {}
  constructor Create(AMain:TWebStatusMain);
 private
  {Internal Variables}
  
 protected
  {Internal Variables}
 
  {Internal Methods}
 
  function DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
 public
  {Public Properties}
 
  {Public Methods}
  
 end;

 TWebStatusLocale = class(TWebStatusSub)
 public
  {}
  constructor Create(AMain:TWebStatusMain);
 private
  {Internal Variables}
  
 protected
  {Internal Variables}
 
  {Internal Methods}
 
  function DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
 public
  {Public Properties}
 
  {Public Methods}
  
 end;
 
 TWebStatusThreading = class(TWebStatusSub)
 public
  {}
  constructor Create(AMain:TWebStatusMain);
 private
  {Internal Variables}
  
 protected
  {Internal Variables}
 
  {Internal Methods}
 
  function DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
 public
  {Public Properties}
 
  {Public Methods}
  
 end;

 TWebStatusThreadList = class(TWebStatusSub)
 public
  {}
  constructor Create(AMain:TWebStatusMain);
 private
  {Internal Variables}
  
 protected
  {Internal Variables}
 
  {Internal Methods}
 
  function DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
 public
  {Public Properties}
 
  {Public Methods}
  
 end;
 
 TWebStatusScheduler = class(TWebStatusSub)
 public
  {}
  constructor Create(AMain:TWebStatusMain);
 private
  {Internal Variables}
  
 protected
  {Internal Variables}
 
  {Internal Methods}
 
  function DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
 public
  {Public Properties}
 
  {Public Methods}
  
 end;
 
 TWebStatusDevices = class(TWebStatusSub)
 public
  {}
  constructor Create(AMain:TWebStatusMain);
 private
  {Internal Variables}
  
 protected
  {Internal Variables}
 
  {Internal Methods}
 
  function DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
 public
  {Public Properties}
 
  {Public Methods}
  
 end;

 TWebStatusDrivers = class(TWebStatusSub)
 public
  {}
  constructor Create(AMain:TWebStatusMain);
 private
  {Internal Variables}
  
 protected
  {Internal Variables}
 
  {Internal Methods}
 
  function DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
 public
  {Public Properties}
 
  {Public Methods}
  
 end;
 
 TWebStatusUSB = class(TWebStatusSub)
 public
  {}
  constructor Create(AMain:TWebStatusMain);
 private
  {Internal Variables}
  
 protected
  {Internal Variables}
 
  {Internal Methods}
 
  function DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
 public
  {Public Properties}
 
  {Public Methods}
  
 end;

 TWebStatusMMC = class(TWebStatusSub)
 public
  {}
  constructor Create(AMain:TWebStatusMain);
 private
  {Internal Variables}
  
 protected
  {Internal Variables}
 
  {Internal Methods}
 
  function DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
 public
  {Public Properties}
 
  {Public Methods}
  
 end;

 TWebStatusNetwork = class(TWebStatusSub)
 public
  {}
  constructor Create(AMain:TWebStatusMain);
 private
  {Internal Variables}
  
 protected
  {Internal Variables}
 
  {Internal Methods}
 
  function DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
 public
  {Public Properties}
 
  {Public Methods}
  
 end;
 
 TWebStatusStorage = class(TWebStatusSub)
 public
  {}
  constructor Create(AMain:TWebStatusMain);
 private
  {Internal Variables}
  
 protected
  {Internal Variables}
 
  {Internal Methods}
 
  function DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
 public
  {Public Properties}
 
  {Public Methods}
  
 end;

 TWebStatusFilesystem = class(TWebStatusSub)
 public
  {}
  constructor Create(AMain:TWebStatusMain);
 private
  {Internal Variables}
  
 protected
  {Internal Variables}
 
  {Internal Methods}
 
  function DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
 public
  {Public Properties}
 
  {Public Methods}
  
 end;

 TWebStatusCache = class(TWebStatusSub)
 public
  {}
  constructor Create(AMain:TWebStatusMain);
 private
  {Internal Variables}
  
 protected
  {Internal Variables}
 
  {Internal Methods}
 
  function DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
 public
  {Public Properties}
 
  {Public Methods}
  
 end;
 
 TWebStatusKeyboard = class(TWebStatusSub)
 public
  {}
  constructor Create(AMain:TWebStatusMain);
 private
  {Internal Variables}
  
 protected
  {Internal Variables}
 
  {Internal Methods}
 
  function DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
 public
  {Public Properties}
 
  {Public Methods}
  
 end;

 TWebStatusMouse = class(TWebStatusSub)
 public
  {}
  constructor Create(AMain:TWebStatusMain);
 private
  {Internal Variables}
  
 protected
  {Internal Variables}
 
  {Internal Methods}
 
  function DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
 public
  {Public Properties}
 
  {Public Methods}
  
 end;

 TWebStatusFramebuffer = class(TWebStatusSub)
 public
  {}
  constructor Create(AMain:TWebStatusMain);
 private
  {Internal Variables}
  
 protected
  {Internal Variables}
 
  {Internal Methods}
 
  function DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
 public
  {Public Properties}
 
  {Public Methods}
  
 end;

 //TWebStatusConsole = class(TWebStatusSub) //To Do
 
 //TWebStatusLogging = class(TWebStatusSub) //To Do
 
 //TWebStatusTimezone = class(TWebStatusSub) //To Do
 
 TWebStatusEnvironment = class(TWebStatusSub)
 public
  {}
  constructor Create(AMain:TWebStatusMain);
 private
  {Internal Variables}
  
 protected
  {Internal Variables}
 
  {Internal Methods}
 
  function DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
 public
  {Public Properties}
 
  {Public Methods}
  
 end;

 TWebStatusPageTables = class(TWebStatusSub)
 public
  {}
  constructor Create(AMain:TWebStatusMain);
 private
  {Internal Variables}
  function FlagsToFlagNames(AFlags:LongWord):TStringList;
  
 protected
  {Internal Variables}
 
  {Internal Methods}
 
  function DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
 public
  {Public Properties}
 
  {Public Methods}
  
 end;
 
 TWebStatusConfiguration = class(TWebStatusSub)
 public
  {}
  constructor Create(AMain:TWebStatusMain);
 private
  {Internal Variables}
  
 protected
  {Internal Variables}
 
  {Internal Methods}
 
  function DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
 public
  {Public Properties}
 
  {Public Methods}
  
 end;
 
{$IF DEFINED(LOCK_DEBUG) or DEFINED(SPIN_DEBUG) or DEFINED(MUTEX_DEBUG) or DEFINED(CLOCK_DEBUG) or DEFINED(SCHEDULER_DEBUG) or DEFINED(INTERRUPT_DEBUG) or DEFINED(EXCEPTION_DEBUG)}
 TWebStatusDebug = class(TWebStatusSub)
 public
  {}
  constructor Create(AMain:TWebStatusMain);
 private
  {Internal Variables}
  
 protected
  {Internal Variables}
 
  {Internal Methods}
 
  function DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
 public
  {Public Properties}
 
  {Public Methods}
  
 end;
{$ENDIF}
 
 TWebStatusContent = function(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean of Object;
 
 TWebStatusCustom = class(TWebStatusSub)
 public
  {}
  constructor Create(const AName,APath:String;AColumns:LongWord);
 private
  {Internal Variables}
  FColumns:LongWord;
  
  FOnContent:TWebStatusContent;
  
 protected
  {Internal Variables}
 
  {Internal Methods}
 
  function DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
  
  function DoContent(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; virtual;
 public
  {Public Properties}
  property Columns:LongWord read FColumns write FColumns;
  
  property OnContent:TWebStatusContent read FOnContent write FOnContent;
  
  {Public Methods}
  
 end;

{==============================================================================}
type
 {Web Status specific types}
 PWebStatusData = ^TWebStatusData;
 TWebStatusData = record
  Document:TWebStatusSub;
  Host:THTTPHost;
  Request:THTTPServerRequest;
  Response:THTTPServerResponse;
 end;
 
{==============================================================================}
{var}
 {Web Status specific variables}

{==============================================================================}
{Initialization Functions}

{==============================================================================}
{Web Status Functions}
function WebStatusRegister(AListener:THTTPListener;const AHost,AURL:String;ARedirect:Boolean):Boolean;
function WebStatusDeregister(AListener:THTTPListener;const AHost:String):Boolean;

{==============================================================================}
{Web Status Helper Functions}
function WebStatusDeviceEnumerate(Device:PDevice;Data:Pointer):LongWord;
function WebStatusDriverEnumerate(Driver:PDriver;Data:Pointer):LongWord;
function WebStatusUSBDeviceEnumerate(Device:PUSBDevice;Data:Pointer):LongWord;
function WebStatusUSBHostEnumerate(Host:PUSBHost;Data:Pointer):LongWord;
function WebStatusUSBDriverEnumerate(Driver:PUSBDriver;Data:Pointer):LongWord;
function WebStatusMMCEnumerate(MMC:PMMCDevice;Data:Pointer):LongWord;
function WebStatusSDHCIEnumerate(SDHCI:PSDHCIHost;Data:Pointer):LongWord;
function WebStatusNetworkEnumerate(Network:PNetworkDevice;Data:Pointer):LongWord;
function WebStatusStorageEnumerate(Storage:PStorageDevice;Data:Pointer):LongWord;
function WebStatusMouseEnumerate(Mouse:PMouseDevice;Data:Pointer):LongWord;
function WebStatusKeyboardEnumerate(Keyboard:PKeyboardDevice;Data:Pointer):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Web Status specific variables}
 WebStatusMain:TWebStatusMain;
 WebStatusPlatform:TWebStatusPlatform;
 WebStatusMemory:TWebStatusMemory;
 WebStatusHeap:TWebStatusHeap;
 WebStatusCPU:TWebStatusCPU;
 WebStatusFPU:TWebStatusFPU;
 WebStatusGPU:TWebStatusGPU;
 WebStatusRTL:TWebStatusRTL;
 WebStatusClock:TWebStatusClock;
 WebStatusLocale:TWebStatusLocale;
 WebStatusThreading:TWebStatusThreading;
 WebStatusThreadList:TWebStatusThreadList;
 WebStatusScheduler:TWebStatusScheduler;
 WebStatusDevices:TWebStatusDevices;
 WebStatusDrivers:TWebStatusDrivers;
 WebStatusUSB:TWebStatusUSB;
 WebStatusMMC:TWebStatusMMC;
 WebStatusNetwork:TWebStatusNetwork;
 WebStatusStorage:TWebStatusStorage;
 WebStatusFilesystem:TWebStatusFilesystem;
 WebStatusCache:TWebStatusCache;
 WebStatusKeyboard:TWebStatusKeyboard;
 WebStatusMouse:TWebStatusMouse;
 WebStatusFramebuffer:TWebStatusFramebuffer;
 WebStatusEnvironment:TWebStatusEnvironment;
 WebStatusPageTables:TWebStatusPageTables;
 WebStatusConfiguration:TWebStatusConfiguration;
 {$IF DEFINED(LOCK_DEBUG) or DEFINED(SPIN_DEBUG) or DEFINED(MUTEX_DEBUG) or DEFINED(CLOCK_DEBUG) or DEFINED(SCHEDULER_DEBUG) or DEFINED(INTERRUPT_DEBUG) or DEFINED(EXCEPTION_DEBUG)}
 WebStatusDebug:TWebStatusDebug;
 {$ENDIF}
 
 WebStatusRedirect:THTTPRedirect;
 
 WEBSTATUS_HEAP_FREE_COUNT:LongWord = 250;  {Maximum number of free heap blocks to display}
 WEBSTATUS_HEAP_USED_COUNT:LongWord = 250;  {Maximum number of used heap blocks to display}
 
{==============================================================================}
{==============================================================================}
{TWebStatusMain}
constructor TWebStatusMain.Create;
begin
 {}
 inherited Create;
 Name:='/status';
 FTitle:='Ultibo Core (Release: ' + ULTIBO_RELEASE_NAME + ' Version: ' + ULTIBO_RELEASE_VERSION + ' Date: ' + ULTIBO_RELEASE_DATE + ')';
 FCaption:='General';
 FSubPages:=TStringList.Create;
end;

{==============================================================================}

destructor TWebStatusMain.Destroy; 
begin
 {}
 AcquireLock;
 try
  FSubPages.Free;
 finally
  ReleaseLock;
  inherited Destroy;
 end;
end;

{==============================================================================}

function TWebStatusMain.GetTitle:String;
begin
 {}
 AcquireLock;
 
 Result:=FTitle;
 UniqueString(Result);
 
 ReleaseLock;
end;

{==============================================================================}

procedure TWebStatusMain.SetTitle(const ATitle:String);
begin
 {}
 AcquireLock;
 
 FTitle:=ATitle;
 UniqueString(FTitle);
 
 ReleaseLock;
end;

{==============================================================================}

function TWebStatusMain.GetCaption:String;
begin
 {}
 AcquireLock;
 
 Result:=FCaption;
 UniqueString(Result);
 
 ReleaseLock;
end;

{==============================================================================}

function TWebStatusMain.MakeBold(const AName:String):String;
begin
 {}
 Result:='<strong>' + AName + '</strong>';
end;

{==============================================================================}

function TWebStatusMain.MakeLink(const AName,ALink:String):String;
begin
 {}
 Result:='<a href="' + ALink + '">' + AName + '</a>';
end;

{==============================================================================}

function TWebStatusMain.AddBlank(AResponse:THTTPServerResponse):Boolean;
begin
 {}
 Result:=AddBlankEx(AResponse,2);
end;

{==============================================================================}

function TWebStatusMain.AddBlankEx(AResponse:THTTPServerResponse;AColumns:LongWord):Boolean;
var
 Count:Integer;
 Percent:LongWord;
begin
 {}
 Result:=False;
 
 {Check Response}
 if AResponse = nil then Exit;

 {Check Columns}
 if AColumns < 2 then AColumns:=2;
 if AColumns > 5 then AColumns:=5;
 
 {Get Percent}
 Percent:=100 div AColumns;
 
 {Add Content}
 AddContent(AResponse,'               <tr>');
 for Count:=1 to AColumns - 1 do
  begin
   AddContent(AResponse,'                 <td style="text-align: left; width: ' + IntToStr(Percent) + '%;">');
   AddContent(AResponse,'                 </td>');
  end; 
 AddContent(AResponse,'                 <td style="text-align: left; width: ' + IntToStr(Percent) + '%;"><br>');
 AddContent(AResponse,'                 </td>');
 AddContent(AResponse,'               </tr>');

 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TWebStatusMain.AddItem(AResponse:THTTPServerResponse;const AName,AValue:String):Boolean;
begin
 {}
 Result:=False;
 
 {Check Response}
 if AResponse = nil then Exit;

 {Add Content}
 AddContent(AResponse,'               <tr>');
 AddContent(AResponse,'                 <td style="text-align: left; width: 50%;">' + AName + '<br>');
 AddContent(AResponse,'                 </td>');
 AddContent(AResponse,'                 <td style="text-align: left; width: 50%;">' + AValue + '<br>');
 AddContent(AResponse,'                 </td>');
 AddContent(AResponse,'               </tr>');

 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TWebStatusMain.AddItemEx(AResponse:THTTPServerResponse;const AName,AValue:String;AIndent:LongWord):Boolean;
var
 Count:LongWord;
 WorkBuffer:String;
begin
 {}
 Result:=False;
 
 {Check Response}
 if AResponse = nil then Exit;

 {Check Indent}
 if AIndent < 1 then
  begin
   Result:=AddItem(AResponse,AName,AValue);
  end
 else
  begin 
   {Get Indent}
   WorkBuffer:='';
   for Count:=1 to AIndent do
    begin
     WorkBuffer:=WorkBuffer + '&nbsp;';
    end;
    
   {Add Content}
   AddContent(AResponse,'               <tr>');
   AddContent(AResponse,'                 <td style="text-align: left; width: 50%;">' + WorkBuffer + AName + '<br>');
   AddContent(AResponse,'                 </td>');
   AddContent(AResponse,'                 <td style="text-align: left; width: 50%;">' + AValue + '<br>');
   AddContent(AResponse,'                 </td>');
   AddContent(AResponse,'               </tr>');

   {Return Result}
   Result:=True;
  end; 
end;

{==============================================================================}

function TWebStatusMain.AddItem3Column(AResponse:THTTPServerResponse;const AName,AValue1,AValue2:String):Boolean;
begin
 {}
 Result:=False;
 
 {Check Response}
 if AResponse = nil then Exit;

 {Add Content}
 AddContent(AResponse,'               <tr>');
 AddContent(AResponse,'                 <td style="text-align: left; width: 33%;">' + AName + '<br>');
 AddContent(AResponse,'                 </td>');
 AddContent(AResponse,'                 <td style="text-align: left; width: 33%;">' + AValue1 + '<br>');
 AddContent(AResponse,'                 </td>');
 AddContent(AResponse,'                 <td style="text-align: left; width: 33%;">' + AValue2 + '<br>');
 AddContent(AResponse,'                 </td>');
 AddContent(AResponse,'               </tr>');

 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TWebStatusMain.AddItem4Column(AResponse:THTTPServerResponse;const AName,AValue1,AValue2,AValue3:String):Boolean;
begin
 {}
 Result:=False;
 
 {Check Response}
 if AResponse = nil then Exit;

 {Add Content}
 AddContent(AResponse,'               <tr>');
 AddContent(AResponse,'                 <td style="text-align: left; width: 25%;">' + AName + '<br>');
 AddContent(AResponse,'                 </td>');
 AddContent(AResponse,'                 <td style="text-align: left; width: 25%;">' + AValue1 + '<br>');
 AddContent(AResponse,'                 </td>');
 AddContent(AResponse,'                 <td style="text-align: left; width: 25%;">' + AValue2 + '<br>');
 AddContent(AResponse,'                 </td>');
 AddContent(AResponse,'                 <td style="text-align: left; width: 25%;">' + AValue3 + '<br>');
 AddContent(AResponse,'                 </td>');
 AddContent(AResponse,'               </tr>');

 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TWebStatusMain.AddItem5Column(AResponse:THTTPServerResponse;const AName,AValue1,AValue2,AValue3,AValue4:String):Boolean;
begin
 {}
 Result:=False;
 
 {Check Response}
 if AResponse = nil then Exit;

 {Add Content}
 AddContent(AResponse,'               <tr>');
 AddContent(AResponse,'                 <td style="text-align: left; width: 20%;">' + AName + '<br>');
 AddContent(AResponse,'                 </td>');
 AddContent(AResponse,'                 <td style="text-align: left; width: 20%;">' + AValue1 + '<br>');
 AddContent(AResponse,'                 </td>');
 AddContent(AResponse,'                 <td style="text-align: left; width: 20%;">' + AValue2 + '<br>');
 AddContent(AResponse,'                 </td>');
 AddContent(AResponse,'                 <td style="text-align: left; width: 20%;">' + AValue3 + '<br>');
 AddContent(AResponse,'                 </td>');
 AddContent(AResponse,'                 <td style="text-align: left; width: 20%;">' + AValue4 + '<br>');
 AddContent(AResponse,'                 </td>');
 AddContent(AResponse,'               </tr>');

 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TWebStatusMain.AddBold(AResponse:THTTPServerResponse;const AName,AValue:String):Boolean;
begin
 {}
 Result:=False;
 
 {Check Response}
 if AResponse = nil then Exit;

 {Add Content}
 AddContent(AResponse,'               <tr>');
 AddContent(AResponse,'                 <td style="text-align: left; width: 50%;"><strong>' + AName + '</strong><br>');
 AddContent(AResponse,'                 </td>');
 AddContent(AResponse,'                 <td style="text-align: left; width: 50%;"><strong>' + AValue + '</strong><br>');
 AddContent(AResponse,'                 </td>');
 AddContent(AResponse,'               </tr>');

 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TWebStatusMain.AddBoldEx(AResponse:THTTPServerResponse;const AName,AValue:String;AIndent:LongWord):Boolean;
var
 Count:LongWord;
 WorkBuffer:String;
begin
 {}
 Result:=False;
 
 {Check Response}
 if AResponse = nil then Exit;

 {Check Indent}
 if AIndent < 1 then
  begin
   Result:=AddBold(AResponse,AName,AValue);
  end
 else
  begin 
   {Get Indent}
   WorkBuffer:='';
   for Count:=1 to AIndent do
    begin
     WorkBuffer:=WorkBuffer + '&nbsp;';
    end;
    
   {Add Content}
   AddContent(AResponse,'               <tr>');
   AddContent(AResponse,'                 <td style="text-align: left; width: 50%;"><strong>' + WorkBuffer + AName + '</strong><br>');
   AddContent(AResponse,'                 </td>');
   AddContent(AResponse,'                 <td style="text-align: left; width: 50%;"><strong>' + AValue + '</strong><br>');
   AddContent(AResponse,'                 </td>');
   AddContent(AResponse,'               </tr>');

   {Return Result}
   Result:=True;
  end; 
end;

{==============================================================================}

function TWebStatusMain.AddBold3Column(AResponse:THTTPServerResponse;const AName,AValue1,AValue2:String):Boolean;
begin
 {}
 Result:=False;
 
 {Check Response}
 if AResponse = nil then Exit;

 {Add Content}
 AddContent(AResponse,'               <tr>');
 AddContent(AResponse,'                 <td style="text-align: left; width: 33%;"><strong>' + AName + '</strong><br>');
 AddContent(AResponse,'                 </td>');
 AddContent(AResponse,'                 <td style="text-align: left; width: 33%;"><strong>' + AValue1 + '</strong><br>');
 AddContent(AResponse,'                 </td>');
 AddContent(AResponse,'                 <td style="text-align: left; width: 33%;"><strong>' + AValue2 + '</strong><br>');
 AddContent(AResponse,'                 </td>');
 AddContent(AResponse,'               </tr>');

 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TWebStatusMain.AddBold4Column(AResponse:THTTPServerResponse;const AName,AValue1,AValue2,AValue3:String):Boolean;
begin
 {}
 Result:=False;
 
 {Check Response}
 if AResponse = nil then Exit;

 {Add Content}
 AddContent(AResponse,'               <tr>');
 AddContent(AResponse,'                 <td style="text-align: left; width: 25%;"><strong>' + AName + '</strong><br>');
 AddContent(AResponse,'                 </td>');
 AddContent(AResponse,'                 <td style="text-align: left; width: 25%;"><strong>' + AValue1 + '</strong><br>');
 AddContent(AResponse,'                 </td>');
 AddContent(AResponse,'                 <td style="text-align: left; width: 25%;"><strong>' + AValue2 + '</strong><br>');
 AddContent(AResponse,'                 </td>');
 AddContent(AResponse,'                 <td style="text-align: left; width: 25%;"><strong>' + AValue3 + '</strong><br>');
 AddContent(AResponse,'                 </td>');
 AddContent(AResponse,'               </tr>');

 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TWebStatusMain.AddBold5Column(AResponse:THTTPServerResponse;const AName,AValue1,AValue2,AValue3,AValue4:String):Boolean;
begin
 {}
 Result:=False;
 
 {Check Response}
 if AResponse = nil then Exit;

 {Add Content}
 AddContent(AResponse,'               <tr>');
 AddContent(AResponse,'                 <td style="text-align: left; width: 20%;"><strong>' + AName + '</strong><br>');
 AddContent(AResponse,'                 </td>');
 AddContent(AResponse,'                 <td style="text-align: left; width: 20%;"><strong>' + AValue1 + '</strong><br>');
 AddContent(AResponse,'                 </td>');
 AddContent(AResponse,'                 <td style="text-align: left; width: 20%;"><strong>' + AValue2 + '</strong><br>');
 AddContent(AResponse,'                 </td>');
 AddContent(AResponse,'                 <td style="text-align: left; width: 20%;"><strong>' + AValue3 + '</strong><br>');
 AddContent(AResponse,'                 </td>');
 AddContent(AResponse,'                 <td style="text-align: left; width: 20%;"><strong>' + AValue4 + '</strong><br>');
 AddContent(AResponse,'                 </td>');
 AddContent(AResponse,'               </tr>');

 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TWebStatusMain.AddHeader(AResponse:THTTPServerResponse;const ATitle:String;ASub:TWebStatusSub):Boolean;
begin
 {}
 Result:=AddHeaderEx(AResponse,ATitle,ASub,2);
end;

{==============================================================================}

function TWebStatusMain.AddHeaderEx(AResponse:THTTPServerResponse;const ATitle:String;ASub:TWebStatusSub;AColumns:LongWord):Boolean;
var
 Count:Integer;
 Percent:LongWord;
 Sub:TWebStatusSub;
begin
 {}
 AcquireLock;
 try
  Result:=False;
 
  {Check Response}
  if AResponse = nil then Exit;
 
  {Check Columns}
  if AColumns < 2 then AColumns:=2;
  if AColumns > 5 then AColumns:=5;
  
  {Get Percent}
  Percent:=100 div AColumns;
  
  {Set Response}
  AResponse.Version:=HTTP_VERSION;
  AResponse.Status:=HTTP_STATUS_OK;
  AResponse.Reason:=HTTP_REASON_200;
  
  {Add Content}
  AddContent(AResponse,'<html>');
  AddContent(AResponse,' <head>');
  AddContent(AResponse,'   <meta content="text/html; charset=ISO-8859-1" http-equiv="Content-Type">');
  AddContent(AResponse,'   <title>' + ATitle + '</title>');
  AddContent(AResponse,' </head>');
  AddContent(AResponse,' <body>');
  AddContent(AResponse,'   <table style=" text-align: left; width: 75%; height: 100%; margin-left: auto; margin-right: auto; font-family: Arial;" border="0" cellpadding="2" cellspacing="2">');
  AddContent(AResponse,'     <tbody>');
  AddContent(AResponse,'       <tr>');
  AddContent(AResponse,'         <td colspan="' + IntToStr(AColumns + 1) + '" rowspan="1" style=" text-align: center; vertical-align: middle; background-color: rgb(255, 153, 0); height: 65px;"><strong>' + ATitle + '</strong><br>');
  AddContent(AResponse,'         </td>');
  AddContent(AResponse,'       </tr>');
  AddContent(AResponse,'       <tr>');
  AddContent(AResponse,'         <td colspan="1" rowspan="1" style=" vertical-align: top; width: 20%;">');
  AddContent(AResponse,'           <table style=" text-align: left; width: 100%;" border="0" cellpadding="2" cellspacing="2">');
  AddContent(AResponse,'             <tbody>');
 
  {Add Main Page}
  AddContent(AResponse,'               <tr>');
  if ASub = nil then
   begin
    AddContent(AResponse,'                 <td style="text-align: center; background-color: rgb(130, 130, 130)"><span style="color: rgb(255, 255, 255);"><a href="' + Name + '">' + GetCaption + '</a></span><br>');
   end
  else
   begin
    AddContent(AResponse,'                 <td style="text-align: center; background-color: rgb(192, 192, 192)"><a href="' + Name + '">' + GetCaption + '</a><br>');
   end;   
  AddContent(AResponse,'                 </td>');
  AddContent(AResponse,'               </tr>');
  
  {Add Sub Pages}
  for Count:=0 to FSubPages.Count - 1 do
   begin
    Sub:=TWebStatusSub(FSubPages.Objects[Count]);
    if Sub <> nil then
     begin
      AddContent(AResponse,'               <tr>');
      if Sub = ASub then
       begin
        AddContent(AResponse,'                 <td style="text-align: center; background-color: rgb(130, 130, 130)"><span style="color: rgb(255, 255, 255);"><a href="' + Sub.Name + '">' + Sub.Caption + '</a></span><br>');
       end
      else
       begin      
        AddContent(AResponse,'                 <td style="text-align: center; background-color: rgb(192, 192, 192)"><a href="' + Sub.Name + '">' + Sub.Caption + '</a><br>');
       end; 
      AddContent(AResponse,'                 </td>');
      AddContent(AResponse,'               </tr>');
     end;
   end;
 
  AddContent(AResponse,'             </tbody>');
  AddContent(AResponse,'           </table>');
  AddContent(AResponse,'           <br>');
  AddContent(AResponse,'         </td>');
  AddContent(AResponse,'         <td rowspan="1" colspan="1" style=" vertical-align: top;">');
  AddContent(AResponse,'           <table style=" text-align: left; width: 100%;" border="0" cellpadding="2" cellspacing="2">');
  AddContent(AResponse,'             <tbody>');
  AddContent(AResponse,'               <tr>');
  if ASub = nil then 
   begin
    AddContent(AResponse,'                 <td colspan="' + IntToStr(AColumns) + '" rowspan="1" style=" text-align: center;"><strong>' + GetCaption + '</strong><br>');
   end
  else
   begin
    AddContent(AResponse,'                 <td colspan="' + IntToStr(AColumns) + '" rowspan="1" style=" text-align: center;"><strong>' + ASub.Caption + '</strong><br>');
   end;
  AddContent(AResponse,'                 </td>');
  AddContent(AResponse,'               </tr>');
  AddContent(AResponse,'               <tr>');
  for Count:=1 to AColumns - 1 do
   begin
    AddContent(AResponse,'                 <td style=" text-align: left; width: ' + IntToStr(Percent) + '%;"></td>');
   end; 
  AddContent(AResponse,'                 <td style="text-align: left; width: ' + IntToStr(Percent) + '%;"><br>');
  AddContent(AResponse,'                 </td>');
  AddContent(AResponse,'               </tr>');
 
  {Return Result}
  Result:=True;
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TWebStatusMain.AddFooter(AResponse:THTTPServerResponse):Boolean;
begin
 {}
 Result:=AddFooterEx(AResponse,2);
end;

{==============================================================================}

function TWebStatusMain.AddFooterEx(AResponse:THTTPServerResponse;AColumns:LongWord):Boolean;
var
 Count:Integer;
 Percent:LongWord;
begin
 {}
 Result:=False;
 
 {Check Response}
 if AResponse = nil then Exit;

 {Check Columns}
 if AColumns < 2 then AColumns:=2;
 if AColumns > 5 then AColumns:=5;
 
 {Get Percent}
 Percent:=100 div AColumns;
 
 {Add Content}
 AddContent(AResponse,'               <tr>');
 for Count:=1 to AColumns - 1 do
  begin
   AddContent(AResponse,'                 <td style=" text-align: left; width: ' + IntToStr(Percent) + '%;"></td>');
  end; 
 AddContent(AResponse,'                 <td style="text-align: left; width: ' + IntToStr(Percent) + '%;"><br>');
 AddContent(AResponse,'                 </td>');
 AddContent(AResponse,'               </tr>');
 AddContent(AResponse,'             </tbody>');
 AddContent(AResponse,'           </table>');
 AddContent(AResponse,'           <br>');
 AddContent(AResponse,'         </td>');
 AddContent(AResponse,'         <td style=" vertical-align: top; width: 10%;"><br>');
 AddContent(AResponse,'         </td>');
 AddContent(AResponse,'       </tr>');
 AddContent(AResponse,'     </tbody>');
 AddContent(AResponse,'   </table>');
 AddContent(AResponse,'   <br>');
 AddContent(AResponse,' </body>');
 AddContent(AResponse,'</html>');
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TWebStatusMain.AddContent(AResponse:THTTPServerResponse;const AContent:String):Boolean;
begin
 {}
 Result:=False;
 
 {Check Response}
 if AResponse = nil then Exit;
 
 {Add Content}
 AResponse.ContentString:=AResponse.ContentString + AContent + HTTP_LINE_END;
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TWebStatusMain.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; 
var
 WorkTime:TDateTime;
begin
 {}
 Result:=False;
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;

 {Add Header}
 AddHeader(AResponse,GetTitle,nil); 
 
 {Add Release Name}
 AddItem(AResponse,'Release Name:',ULTIBO_RELEASE_NAME);

 {Add Release Version}
 AddItem(AResponse,'Release Version:',ULTIBO_RELEASE_VERSION);

 {Add Release Date}
 AddItem(AResponse,'Release Date:',ULTIBO_RELEASE_DATE);

 {Add Time (Local)}
 AddBlank(AResponse);
 AddItem(AResponse,'Time (Local):',DateTimeToStr(Now));
 
 {Add Time (UTC)}
 AddItem(AResponse,'Time (UTC):',DateTimeToStr(SystemFileTimeToDateTime(GetCurrentTime)));
 
 {Add Timezone}
 AddBlank(AResponse);
 AddItem(AResponse,'Timezone:',GetCurrentTimezone);
 AddBlank(AResponse);
 AddItemEx(AResponse,'Daylight Start:',GetTimezoneDaylightStart,2);
 AddItemEx(AResponse,'Daylight Date:',DateTimeToStr(GetTimezoneDaylightDate),2);
 AddBlank(AResponse);
 AddItemEx(AResponse,'Standard Start:',GetTimezoneStandardStart,2);
 AddItemEx(AResponse,'Standard Date:',DateTimeToStr(GetTimezoneStandardDate),2);

 //To Do //Locale
 
 //To Do //Codepage
 
 {Add Uptime}
 WorkTime:=SystemFileTimeToDateTime(Uptime); {No Conversion}
 AddBlank(AResponse);
 AddItem(AResponse,'Uptime:',IntToStr(Trunc(WorkTime)) + ' days ' + TimeToStr(WorkTime));
 
 {Add Footer}
 AddFooter(AResponse); 
 
 {Return Result}
 Result:=True;
end;
 
{==============================================================================}

function TWebStatusMain.RegisterSubPage(ASub:TWebStatusSub):Boolean;
begin
 {}
 AcquireLock;
 try
  Result:=False;
 
  {Check Sub Page}
  if ASub = nil then Exit;
  
  {Check Caption}
  //--if FSubPages.IndexOf(ASub.Caption) <> -1 then Exit; //To Do //Need to fix Ansi functions in Unicode.pas (see AnsiCompareStr etc in sysstr.inc)
  
  {Check Object}
  if FSubPages.IndexOfObject(ASub) <> -1 then Exit;
  
  {Add Sub Page}
  Result:=(FSubPages.AddObject(ASub.Caption,ASub) <> -1);
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TWebStatusMain.DeregisterSubPage(ASub:TWebStatusSub):Boolean;
var
 Index:Integer;
begin
 {}
 AcquireLock;
 try
  Result:=False;
  
  {Check Sub Page}
  if ASub = nil then Exit;
  
  {Check Object}
  Index:=FSubPages.IndexOfObject(ASub);
  if Index = -1 then Exit;
 
  {Remove Sub Page}
  FSubPages.Delete(Index);
  
  Result:=True;
 finally
  ReleaseLock;
 end; 
end;
 
{==============================================================================}
{==============================================================================}
{TWebStatusSub}
constructor TWebStatusSub.Create(AMain:TWebStatusMain);
begin
 {}
 inherited Create;
 FMain:=AMain;
 {FCaption:='';} {Must be set by descendant}
 
 if FMain <> nil then FMain.RegisterSubPage(Self);
end;

{==============================================================================}

destructor TWebStatusSub.Destroy; 
begin
 {}
 AcquireLock;
 try
  if FMain <> nil then FMain.DeregisterSubPage(Self);
  FMain:=nil;
 finally
  ReleaseLock;
  inherited Destroy;
 end;
end;

{==============================================================================}

function TWebStatusSub.GetTitle:String;
begin
 {}
 Result:='';
 
 if FMain = nil then Exit;
 
 Result:=FMain.Title;
end;

{==============================================================================}

function TWebStatusSub.GetCaption:String;
begin
 {}
 AcquireLock;
 
 Result:=FCaption;
 UniqueString(Result);
 
 ReleaseLock;
end;

{==============================================================================}

function TWebStatusSub.MakeBold(const AName:String):String;
begin
 {}
 Result:='';
 
 if FMain = nil then Exit;
 
 Result:=FMain.MakeBold(AName);
end;

{==============================================================================}

function TWebStatusSub.MakeLink(const AName,ALink:String):String;
begin
 {}
 Result:='';
 
 if FMain = nil then Exit;
 
 Result:=FMain.MakeLink(AName,ALink);
end;

{==============================================================================}

function TWebStatusSub.AddBlank(AResponse:THTTPServerResponse):Boolean;
begin
 {}
 Result:=False;
 
 if FMain = nil then Exit;
 
 Result:=FMain.AddBlank(AResponse);
end;

{==============================================================================}

function TWebStatusSub.AddBlankEx(AResponse:THTTPServerResponse;AColumns:LongWord):Boolean;
begin
 {}
 Result:=False;
 
 if FMain = nil then Exit;
 
 Result:=FMain.AddBlankEx(AResponse,AColumns);
end;

{==============================================================================}

function TWebStatusSub.AddItem(AResponse:THTTPServerResponse;const AName,AValue:String):Boolean;
begin
 {}
 Result:=False;
 
 if FMain = nil then Exit;
 
 Result:=FMain.AddItem(AResponse,AName,AValue);
end;

{==============================================================================}

function TWebStatusSub.AddItemEx(AResponse:THTTPServerResponse;const AName,AValue:String;AIndent:LongWord):Boolean;
begin
 {}
 Result:=False;
 
 if FMain = nil then Exit;
 
 Result:=FMain.AddItemEx(AResponse,AName,AValue,AIndent);
end;

{==============================================================================}

function TWebStatusSub.AddItem3Column(AResponse:THTTPServerResponse;const AName,AValue1,AValue2:String):Boolean;
begin
 {}
 Result:=False;
 
 if FMain = nil then Exit;
 
 Result:=FMain.AddItem3Column(AResponse,AName,AValue1,AValue2);
end;

{==============================================================================}

function TWebStatusSub.AddItem4Column(AResponse:THTTPServerResponse;const AName,AValue1,AValue2,AValue3:String):Boolean;
begin
 {}
 Result:=False;
 
 if FMain = nil then Exit;
 
 Result:=FMain.AddItem4Column(AResponse,AName,AValue1,AValue2,AValue3);
end;

{==============================================================================}

function TWebStatusSub.AddItem5Column(AResponse:THTTPServerResponse;const AName,AValue1,AValue2,AValue3,AValue4:String):Boolean;
begin
 {}
 Result:=False;
 
 if FMain = nil then Exit;
 
 Result:=FMain.AddItem5Column(AResponse,AName,AValue1,AValue2,AValue3,AValue4);
end;

{==============================================================================}

function TWebStatusSub.AddBold(AResponse:THTTPServerResponse;const AName,AValue:String):Boolean;
begin
 {}
 Result:=False;
 
 if FMain = nil then Exit;
 
 Result:=FMain.AddBold(AResponse,AName,AValue);
end;

{==============================================================================}

function TWebStatusSub.AddBoldEx(AResponse:THTTPServerResponse;const AName,AValue:String;AIndent:LongWord):Boolean;
begin
 {}
 Result:=False;
 
 if FMain = nil then Exit;
 
 Result:=FMain.AddBoldEx(AResponse,AName,AValue,AIndent);
end;

{==============================================================================}

function TWebStatusSub.AddBold3Column(AResponse:THTTPServerResponse;const AName,AValue1,AValue2:String):Boolean;
begin
 {}
 Result:=False;
 
 if FMain = nil then Exit;
 
 Result:=FMain.AddBold3Column(AResponse,AName,AValue1,AValue2);
end;

{==============================================================================}

function TWebStatusSub.AddBold4Column(AResponse:THTTPServerResponse;const AName,AValue1,AValue2,AValue3:String):Boolean;
begin
 {}
 Result:=False;
 
 if FMain = nil then Exit;
 
 Result:=FMain.AddBold4Column(AResponse,AName,AValue1,AValue2,AValue3);
end;

{==============================================================================}

function TWebStatusSub.AddBold5Column(AResponse:THTTPServerResponse;const AName,AValue1,AValue2,AValue3,AValue4:String):Boolean;
begin
 {}
 Result:=False;
 
 if FMain = nil then Exit;
 
 Result:=FMain.AddBold5Column(AResponse,AName,AValue1,AValue2,AValue3,AValue4);
end;

{==============================================================================}
 
function TWebStatusSub.AddHeader(AResponse:THTTPServerResponse;const ATitle:String;ASub:TWebStatusSub):Boolean;
begin
 {}
 Result:=False;
 
 if FMain = nil then Exit;
 
 Result:=FMain.AddHeader(AResponse,ATitle,ASub);
end;

{==============================================================================}

function TWebStatusSub.AddHeaderEx(AResponse:THTTPServerResponse;const ATitle:String;ASub:TWebStatusSub;AColumns:LongWord):Boolean;
begin
 {}
 Result:=False;
 
 if FMain = nil then Exit;
 
 Result:=FMain.AddHeaderEx(AResponse,ATitle,ASub,AColumns);
end;

{==============================================================================}

function TWebStatusSub.AddFooter(AResponse:THTTPServerResponse):Boolean;
begin
 {}
 Result:=False;
 
 if FMain = nil then Exit;
 
 Result:=FMain.AddFooter(AResponse);
end;

{==============================================================================}

function TWebStatusSub.AddFooterEx(AResponse:THTTPServerResponse;AColumns:LongWord):Boolean;
begin
 {}
 Result:=False;
 
 if FMain = nil then Exit;
 
 Result:=FMain.AddFooterEx(AResponse,AColumns);
end;

{==============================================================================}

function TWebStatusSub.AddContent(AResponse:THTTPServerResponse;const AContent:String):Boolean;
begin
 {}
 Result:=False;
 
 if FMain = nil then Exit;
 
 Result:=FMain.AddContent(AResponse,AContent);
end;

{==============================================================================}

function TWebStatusSub.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
begin
 {}
 Result:=False;
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;

 {Add Header}
 AddHeader(AResponse,GetTitle,Self); 

 {Add Content}
 {Must be done by descendant}
 
 {Add Footer}
 AddFooter(AResponse); 
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TWebStatusPlatform}
constructor TWebStatusPlatform.Create(AMain:TWebStatusMain);
begin
 {}
 FCaption:='Platform'; {Must be before create for register}
 inherited Create(AMain);
 Name:='/platform';
 
 if FMain <> nil then Name:=FMain.Name + Name;
end; 

{==============================================================================}

function TWebStatusPlatform.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
var
 Count:LongWord;
begin
 {}
 Result:=False;
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;

 {Add Header}
 AddHeader(AResponse,GetTitle,Self); 

 {Add Board Type}
 AddItem(AResponse,'Board Type:',BoardTypeToString(BoardGetType));

 {Add Board Model}
 AddItem(AResponse,'Board Model:',IntToStr(BoardGetModel));

 {Add Board Serial}
 AddItem(AResponse,'Board Serial:','0x' + IntToHex(BoardGetSerial,16));
 
 {Add Board Revision}
 AddItem(AResponse,'Board Revision:','0x' + IntToHex(BoardGetRevision,8));

 {Add Firmware Revision}
 AddBlank(AResponse);
 AddItem(AResponse,'Firmware Revision:','0x' + IntToHex(FirmwareGetRevision,8));
 
 {Add Machine Type}
 AddBlank(AResponse);
 AddItem(AResponse,'Machine Type:',MachineTypeToString(MachineGetType));

 {Add Memory Base}
 AddBlank(AResponse);
 AddItem(AResponse,'Memory Base:','0x' + IntToHex(MemoryGetBase,8));

 {Add Memory Size}
 AddItem(AResponse,'Memory Size:',IntToStr(MemoryGetSize));

 {Add Page Size}
 AddBlank(AResponse);
 AddItem(AResponse,'Page Size:',IntToStr(MemoryGetPageSize));
 AddItem(AResponse,'Large Page Size:',IntToStr(MemoryGetLargePageSize));

 {Add Power States}
 AddBlank(AResponse);
 AddItem(AResponse,'Power State','');
 for Count:=POWER_ID_MMC0 to POWER_ID_CCP2TX do
  begin
   AddItemEx(AResponse,PowerIDToString(Count) + ':',PowerStateToString(PowerGetState(Count)),3);
  end; 
 
 {Add Clock Rates}
 AddBlank(AResponse);
 AddItem(AResponse,'Clock Rate','');
 for Count:=CLOCK_ID_MMC0 to CLOCK_ID_PWM do
  begin
   AddItemEx(AResponse,ClockIDToString(Count) + ':',IntToStr(ClockGetRate(Count)),3);
  end; 
 
 {Add Clock States}
 AddBlank(AResponse);
 AddItem(AResponse,'Clock State','');
 for Count:=CLOCK_ID_MMC0 to CLOCK_ID_PWM do
  begin
   AddItemEx(AResponse,ClockIDToString(Count) + ':',ClockStateToString(ClockGetState(Count)),3);
  end; 

 {Add Clock Min/Max}
 AddBlank(AResponse);
 AddItem(AResponse,'Clock Min/Max Rate','');
 for Count:=CLOCK_ID_MMC0 to CLOCK_ID_PWM do
  begin
   AddItemEx(AResponse,ClockIDToString(Count) + ':',IntToStr(ClockGetMinRate(Count)) + ' / ' + IntToStr(ClockGetMaxRate(Count)),3);
  end; 
 
 {Add Turbo State}
 AddBlank(AResponse);
 AddItem(AResponse,'Turbo State','');
 AddItemEx(AResponse,'TURBO_ID_SOC:',IntToStr(TurboGetState(TURBO_ID_SOC)),3);
 
 {Add Voltage Values}
 AddBlank(AResponse);
 AddItem(AResponse,'Voltage Value','');
 for Count:=VOLTAGE_ID_CORE to VOLTAGE_ID_SDRAM_I do
  begin
   AddItemEx(AResponse,VoltageIDToString(Count) + ':',IntToStr(VoltageGetValue(Count)),3);
  end; 
 
 {Add Voltage Min/Max}
 AddBlank(AResponse);
 AddItem(AResponse,'Voltage Min/Max Value','');
 for Count:=VOLTAGE_ID_CORE to VOLTAGE_ID_SDRAM_I do
  begin
   AddItemEx(AResponse,VoltageIDToString(Count) + ':',IntToStr(VoltageGetMinValue(Count)) + ' / ' + IntToStr(VoltageGetMaxValue(Count)),3);
  end; 
 
 {Add Temperature Current}
 AddBlank(AResponse);
 AddItem(AResponse,'Temperature Current','');
 AddItemEx(AResponse,'TEMPERATURE_ID_SOC:',IntToStr(TemperatureGetCurrent(TEMPERATURE_ID_SOC)),3);

 {Add Temperature Maximum}
 AddBlank(AResponse);
 AddItem(AResponse,'Temperature Maximum','');
 AddItemEx(AResponse,'TEMPERATURE_ID_SOC:',IntToStr(TemperatureGetMaximum(TEMPERATURE_ID_SOC)),3);
 
 {Add DMA Channels}
 AddBlank(AResponse);
 AddItem(AResponse,'DMA Channels:','0x' + IntToHex(DMAGetChannels,8));

 {Add IO Base}
 AddBlank(AResponse);
 AddItem(AResponse,'IO Base:','0x' + IntToHex(IO_BASE,8));
 
 {Add IO Alias}
 AddBlank(AResponse);
 AddItem(AResponse,'IO Alias:','0x' + IntToHex(IO_ALIAS,8));

 {Add Bus Alias}
 AddBlank(AResponse);
 AddItem(AResponse,'Bus Alias:','0x' + IntToHex(BUS_ALIAS,8));
 
 {Add Secure Boot}
 AddBlank(AResponse);
 AddItem(AResponse,'Secure Boot:',BooleanToString(SECURE_BOOT));
 
 {Add Startup Address}
 AddBlank(AResponse);
 AddItem(AResponse,'Startup Address:','0x' + IntToHex(STARTUP_ADDRESS,8));
 
 {Add Peripheral Base}
 AddBlank(AResponse);
 AddItem(AResponse,'Peripheral Base:','0x' + IntToHex(PeripheralGetBase,8));

 {Add Peripheral Size}
 AddItem(AResponse,'Peripheral Size:',IntToStr(PeripheralGetSize));

 {Add Local Peripheral Base}
 AddBlank(AResponse);
 AddItem(AResponse,'Local Peripheral Base:','0x' + IntToHex(LocalPeripheralGetBase,8));

 {Add Local Peripheral Size}
 AddItem(AResponse,'Local Peripheral Size:',IntToStr(LocalPeripheralGetSize));
 
 {Add Page Table Base}
 AddBlank(AResponse);
 AddItem(AResponse,'Page Table Base:','0x' + IntToHex(PageTableGetBase,8));

 {Add Page Table Size}
 AddItem(AResponse,'Page Table Size:',IntToStr(PageTableGetSize));

 {Add Page Tables Address}
 AddBlank(AResponse);
 AddItem(AResponse,'Page Tables Address:','0x' + IntToHex(PageTablesGetAddress,8));
 
 {Add Page Tables Length}
 AddItem(AResponse,'Page Tables Length:',IntToStr(PageTablesGetLength));

 {Add Page Tables Count}
 AddItem(AResponse,'Page Tables Count:',IntToStr(PageTablesGetCount));
 
 {Add Page Tables Shift}
 AddItem(AResponse,'Page Tables Shift:',IntToStr(PageTablesGetShift));
 
 {Add Vector Table Base}
 AddBlank(AResponse);
 AddItem(AResponse,'Vector Table Base:','0x' + IntToHex(VectorTableGetBase,8));

 {Add Vector Table Size}
 AddItem(AResponse,'Vector Table Size:',IntToStr(VectorTableGetSize));

 {Add Clock Frequency/Ticks/Cycles}
 AddBlank(AResponse);
 AddItem(AResponse,'Clock Frequency:',IntToStr(CLOCK_FREQUENCY));
 AddItem(AResponse,'Clock Ticks per Second:',IntToStr(CLOCK_TICKS_PER_SECOND));
 AddItem(AResponse,'Clock Ticks per Millisecond:',IntToStr(CLOCK_TICKS_PER_MILLISECOND));
 AddItem(AResponse,'Clock Cycles per Tick:',IntToStr(CLOCK_CYCLES_PER_TICK));
 AddItem(AResponse,'Clock Cycles per Millisecond:',IntToStr(CLOCK_CYCLES_PER_MILLISECOND));
 AddItem(AResponse,'Clock Cycles per Microsecond:',IntToStr(CLOCK_CYCLES_PER_MICROSECOND));
 AddItem(AResponse,'Clock Cycles per Nanosecond:',IntToStr(CLOCK_CYCLES_PER_NANOSECOND));
 AddItem(AResponse,'Clock Cycles Tolerance:',IntToStr(CLOCK_CYCLES_TOLERANCE));
 
 {Add Timer Thread Count}
 AddBlank(AResponse);
 AddItem(AResponse,'Timer Thread Count:',IntToStr(TIMER_THREAD_COUNT));

 {Add Worker Thread Count}
 AddBlank(AResponse);
 AddItem(AResponse,'Worker Thread Count (Current):',IntToStr(WorkerGetCount));
 AddItem(AResponse,'Worker Thread Count (Default):',IntToStr(WORKER_THREAD_COUNT));
 
 {Add Footer}
 AddFooter(AResponse); 
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TWebStatusMemory}
constructor TWebStatusMemory.Create(AMain:TWebStatusMain);
begin
 {}
 FCaption:='Memory'; {Must be before create for register}
 inherited Create(AMain);
 Name:='/memory';
 
 if FMain <> nil then Name:=FMain.Name + Name;
end; 

{==============================================================================}

function TWebStatusMemory.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
var
 Status:THeapStatus;
 FPCStatus:TFPCHeapStatus;
 {$IFDEF HEAP_STATISTICS}
 Statistics:THeapStatistics;
 {$ENDIF}
begin
 {}
 Result:=False;
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;

 {Add Header}
 AddHeader(AResponse,GetTitle,Self); 

 {Add Initial Heap}
 AddBold(AResponse,'Initial Heap','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'INITIAL_HEAP_SIZE',IntToStr(INITIAL_HEAP_SIZE),3);
 AddItemEx(AResponse,'INITIAL_HEAP_BASE','0x' + IntToHex(INITIAL_HEAP_BASE,8),3);
 AddBlank(AResponse);
 
 {Add Heap Status}
 Status:=GetHeapStatus;
 AddBold(AResponse,'Heap Status','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'TotalAddrSpace:',IntToStr(Status.TotalAddrSpace),3);
 AddItemEx(AResponse,'TotalUncommitted:',IntToStr(Status.TotalUncommitted),3);
 AddItemEx(AResponse,'TotalCommitted:',IntToStr(Status.TotalCommitted),3);
 AddItemEx(AResponse,'TotalAllocated:',IntToStr(Status.TotalAllocated),3);
 AddItemEx(AResponse,'TotalFree:',IntToStr(Status.TotalFree),3);
 AddItemEx(AResponse,'FreeSmall:',IntToStr(Status.FreeSmall),3);
 AddItemEx(AResponse,'FreeBig:',IntToStr(Status.FreeBig),3);
 AddItemEx(AResponse,'Unused:',IntToStr(Status.Unused),3);
 AddItemEx(AResponse,'Overhead:',IntToStr(Status.Overhead),3);
 AddItemEx(AResponse,'HeapErrorCode:',IntToStr(Status.HeapErrorCode),3);
 AddBlank(AResponse);

 {Add FPC Heap Status}
 FPCStatus:=GetFPCHeapStatus;
 AddBold(AResponse,'FPC Heap Status','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'MaxHeapSize:',IntToStr(FPCStatus.MaxHeapSize),3);
 AddItemEx(AResponse,'MaxHeapUsed:',IntToStr(FPCStatus.MaxHeapUsed),3);
 AddItemEx(AResponse,'CurrHeapSize:',IntToStr(FPCStatus.CurrHeapSize),3);
 AddItemEx(AResponse,'CurrHeapUsed:',IntToStr(FPCStatus.CurrHeapUsed),3);
 AddItemEx(AResponse,'CurrHeapFree:',IntToStr(FPCStatus.CurrHeapFree),3);
 AddBlank(AResponse);

 {Add Heap Statistics}
 {$IFDEF HEAP_STATISTICS}
 Statistics:=GetHeapStatistics;
 {$ENDIF}
 AddBold(AResponse,'Heap Statistics','');
 {$IFDEF HEAP_STATISTICS}
 {Get/Alloc/Realloc}
 AddBlank(AResponse);
 AddItemEx(AResponse,'GetCount:',IntToStr(Statistics.GetCount),3);
 AddItemEx(AResponse,'AllocCount:',IntToStr(Statistics.AllocCount),3);
 AddItemEx(AResponse,'ReallocCount:',IntToStr(Statistics.ReallocCount),3);
 AddItemEx(AResponse,'GetAlignedCount:',IntToStr(Statistics.GetAlignedCount),3);
 AddItemEx(AResponse,'AllocAlignedCount:',IntToStr(Statistics.AllocAlignedCount),3);
 AddItemEx(AResponse,'ReallocAlignedCount:',IntToStr(Statistics.ReallocAlignedCount),3);
 AddItemEx(AResponse,'GetSharedCount:',IntToStr(Statistics.GetSharedCount),3);
 AddItemEx(AResponse,'AllocSharedCount:',IntToStr(Statistics.AllocSharedCount),3);
 AddItemEx(AResponse,'ReallocSharedCount:',IntToStr(Statistics.ReallocSharedCount),3);
 AddItemEx(AResponse,'GetLocalCount:',IntToStr(Statistics.GetLocalCount),3);
 AddItemEx(AResponse,'AllocLocalCount:',IntToStr(Statistics.AllocLocalCount),3);
 AddItemEx(AResponse,'ReallocLocalCount:',IntToStr(Statistics.ReallocLocalCount),3);
 AddItemEx(AResponse,'GetCodeCount:',IntToStr(Statistics.GetCodeCount),3);
 AddItemEx(AResponse,'AllocCodeCount:',IntToStr(Statistics.AllocCodeCount),3);
 AddItemEx(AResponse,'ReallocCodeCount:',IntToStr(Statistics.ReallocCodeCount),3);
 AddItemEx(AResponse,'GetDeviceCount:',IntToStr(Statistics.GetDeviceCount),3);
 AddItemEx(AResponse,'AllocDeviceCount:',IntToStr(Statistics.AllocDeviceCount),3);
 AddItemEx(AResponse,'ReallocDeviceCount:',IntToStr(Statistics.ReallocDeviceCount),3);
 AddItemEx(AResponse,'GetNoCacheCount:',IntToStr(Statistics.GetNoCacheCount),3);
 AddItemEx(AResponse,'AllocNoCacheCount:',IntToStr(Statistics.AllocNoCacheCount),3);
 AddItemEx(AResponse,'ReallocNoCacheCount:',IntToStr(Statistics.ReallocNoCacheCount),3);
 AddItemEx(AResponse,'GetNonSharedCount:',IntToStr(Statistics.GetNonSharedCount),3);
 AddItemEx(AResponse,'AllocNonSharedCount:',IntToStr(Statistics.AllocNonSharedCount),3);
 AddItemEx(AResponse,'ReallocNonSharedCount:',IntToStr(Statistics.ReallocNonSharedCount),3);
 AddItemEx(AResponse,'GetIRQCount:',IntToStr(Statistics.GetIRQCount),3);
 AddItemEx(AResponse,'AllocIRQCount:',IntToStr(Statistics.AllocIRQCount),3);
 AddItemEx(AResponse,'ReallocIRQCount:',IntToStr(Statistics.ReallocIRQCount),3);
 AddItemEx(AResponse,'GetFIQCount:',IntToStr(Statistics.GetFIQCount),3);
 AddItemEx(AResponse,'AllocFIQCount:',IntToStr(Statistics.AllocFIQCount),3);
 AddItemEx(AResponse,'ReallocFIQCount:',IntToStr(Statistics.ReallocFIQCount),3);
 {Free}
 AddBlank(AResponse);
 AddItemEx(AResponse,'FreeCount:',IntToStr(Statistics.FreeCount),3);
 AddItemEx(AResponse,'FreeIRQCount:',IntToStr(Statistics.FreeIRQCount),3);
 AddItemEx(AResponse,'FreeFIQCount:',IntToStr(Statistics.FreeFIQCount),3);
 AddItemEx(AResponse,'FreeSizeCount:',IntToStr(Statistics.FreeSizeCount),3);
 {Size}
 AddBlank(AResponse);
 AddItemEx(AResponse,'SizeCount:',IntToStr(Statistics.SizeCount),3);
 AddItemEx(AResponse,'SizeIRQCount:',IntToStr(Statistics.SizeIRQCount),3);
 AddItemEx(AResponse,'SizeFIQCount:',IntToStr(Statistics.SizeFIQCount),3);
 {Flags}
 AddBlank(AResponse);
 AddItemEx(AResponse,'FlagsCount:',IntToStr(Statistics.FlagsCount),3);
 AddItemEx(AResponse,'FlagsIRQCount:',IntToStr(Statistics.FlagsIRQCount),3);
 AddItemEx(AResponse,'FlagsFIQCount:',IntToStr(Statistics.FlagsFIQCount),3);
 {Register}
 AddBlank(AResponse);
 AddItemEx(AResponse,'RegisterCount:',IntToStr(Statistics.RegisterCount),3);
 {Request}
 AddBlank(AResponse);
 AddItemEx(AResponse,'RequestCount:',IntToStr(Statistics.RequestCount),3);
 AddItemEx(AResponse,'RequestSharedCount:',IntToStr(Statistics.RequestSharedCount),3);
 AddItemEx(AResponse,'RequestLocalCount:',IntToStr(Statistics.RequestLocalCount),3);
 AddItemEx(AResponse,'RequestCodeCount:',IntToStr(Statistics.RequestCodeCount),3);
 AddItemEx(AResponse,'RequestDeviceCount:',IntToStr(Statistics.RequestDeviceCount),3);
 AddItemEx(AResponse,'RequestNoCacheCount:',IntToStr(Statistics.RequestNoCacheCount),3);
 AddItemEx(AResponse,'RequestNonSharedCount:',IntToStr(Statistics.RequestNonSharedCount),3);
 AddItemEx(AResponse,'RequestIRQCount:',IntToStr(Statistics.RequestIRQCount),3);
 AddItemEx(AResponse,'RequestFIQCount:',IntToStr(Statistics.RequestFIQCount),3);
 {Get Internal}
 AddBlank(AResponse);
 AddItemEx(AResponse,'GetZeroCount:',IntToStr(Statistics.GetZeroCount),3);
 AddItemEx(AResponse,'GetRemainCount:',IntToStr(Statistics.GetRemainCount),3);
 AddItemEx(AResponse,'GetInvalidCount:',IntToStr(Statistics.GetInvalidCount),3);
 AddItemEx(AResponse,'GetUnavailableCount:',IntToStr(Statistics.GetUnavailableCount),3);
 AddItemEx(AResponse,'GetAddFailCount:',IntToStr(Statistics.GetAddFailCount),3);
 AddItemEx(AResponse,'GetSplitFailCount:',IntToStr(Statistics.GetSplitFailCount),3);
 AddItemEx(AResponse,'GetRemoveFailCount:',IntToStr(Statistics.GetRemoveFailCount),3);
 {Realloc Internal}
 AddBlank(AResponse);
 AddItemEx(AResponse,'ReallocZeroCount:',IntToStr(Statistics.ReallocZeroCount),3);
 AddItemEx(AResponse,'ReallocSmallerCount:',IntToStr(Statistics.ReallocSmallerCount),3);
 AddItemEx(AResponse,'ReallocLargerCount:',IntToStr(Statistics.ReallocLargerCount),3);
 AddItemEx(AResponse,'ReallocReleaseCount:',IntToStr(Statistics.ReallocReleaseCount),3);
 AddItemEx(AResponse,'ReallocReleaseBytes:',IntToStr(Statistics.ReallocReleaseBytes),3);
 AddItemEx(AResponse,'ReallocAddFailCount:',IntToStr(Statistics.ReallocAddFailCount),3);
 AddItemEx(AResponse,'ReallocSplitFailCount:',IntToStr(Statistics.ReallocSplitFailCount),3);
 AddItemEx(AResponse,'ReallocRemoveFailCount:',IntToStr(Statistics.ReallocRemoveFailCount),3);
 {GetAligned Internal}
 AddBlank(AResponse);
 AddItemEx(AResponse,'GetAlignedRemainCount:',IntToStr(Statistics.GetAlignedRemainCount),3);
 AddItemEx(AResponse,'GetAlignedInvalidCount:',IntToStr(Statistics.GetAlignedInvalidCount),3);
 AddItemEx(AResponse,'GetAlignedUndersizeCount:',IntToStr(Statistics.GetAlignedUndersizeCount),3);
 AddItemEx(AResponse,'GetAlignedUnavailableCount:',IntToStr(Statistics.GetAlignedUnavailableCount),3);
 AddItemEx(AResponse,'GetAlignedAddFailCount:',IntToStr(Statistics.GetAlignedAddFailCount),3);
 AddItemEx(AResponse,'GetAlignedSplitFailCount:',IntToStr(Statistics.GetAlignedSplitFailCount),3);
 AddItemEx(AResponse,'GetAlignedRemoveFailCount:',IntToStr(Statistics.GetAlignedRemoveFailCount),3);
 AddItemEx(AResponse,'GetAlignedOrphanCount:',IntToStr(Statistics.GetAlignedOrphanCount),3);
 AddItemEx(AResponse,'GetAlignedOrphanBytes:',IntToStr(Statistics.GetAlignedOrphanBytes),3);
 AddItemEx(AResponse,'GetAlignedReleaseCount:',IntToStr(Statistics.GetAlignedReleaseCount),3);
 AddItemEx(AResponse,'GetAlignedReleaseBytes:',IntToStr(Statistics.GetAlignedReleaseBytes),3);
 {Free Internal}
 AddBlank(AResponse);
 AddItemEx(AResponse,'FreeInvalidCount:',IntToStr(Statistics.FreeInvalidCount),3);
 AddItemEx(AResponse,'FreeAddFailCount:',IntToStr(Statistics.FreeAddFailCount),3);
 AddItemEx(AResponse,'FreeMergeFailCount:',IntToStr(Statistics.FreeMergeFailCount),3);
 AddItemEx(AResponse,'FreeRemoveFailCount:',IntToStr(Statistics.FreeRemoveFailCount),3);
 {Size Internal}
 AddBlank(AResponse);
 AddItemEx(AResponse,'SizeInvalidCount:',IntToStr(Statistics.SizeInvalidCount),3);
 {Flags Internal}
 AddBlank(AResponse);
 AddItemEx(AResponse,'FlagsInvalidCount:',IntToStr(Statistics.FlagsInvalidCount),3);
 {Register Internal}
 AddBlank(AResponse);
 AddItemEx(AResponse,'RegisterInvalidCount:',IntToStr(Statistics.RegisterInvalidCount),3);
 AddItemEx(AResponse,'RegisterAddFailCount:',IntToStr(Statistics.RegisterAddFailCount),3);
 {Request Internal}
 AddBlank(AResponse);
 AddItemEx(AResponse,'RequestInvalidCount:',IntToStr(Statistics.RequestInvalidCount),3);
 AddItemEx(AResponse,'RequestAddFailCount:',IntToStr(Statistics.RequestAddFailCount),3);
 AddItemEx(AResponse,'RequestSplitFailCount:',IntToStr(Statistics.RequestSplitFailCount),3);
 AddItemEx(AResponse,'RequestRemoveFailCount:',IntToStr(Statistics.RequestRemoveFailCount),3);
 AddItemEx(AResponse,'RequestUnavailableCount:',IntToStr(Statistics.RequestUnavailableCount),3);
 {Split Internal}
 AddBlank(AResponse);
 AddItemEx(AResponse,'SplitCount:',IntToStr(Statistics.SplitCount),3);
 {Merge Internal}
 AddBlank(AResponse);
 AddItemEx(AResponse,'MergePrevCount:',IntToStr(Statistics.MergePrevCount),3);
 AddItemEx(AResponse,'MergeNextCount:',IntToStr(Statistics.MergeNextCount),3);
 {Block Internal}
 AddBlank(AResponse);
 AddItemEx(AResponse,'GetSmallCount:',IntToStr(Statistics.GetSmallCount),3);
 AddItemEx(AResponse,'GetLargeCount:',IntToStr(Statistics.GetLargeCount),3);
 AddItemEx(AResponse,'AddSmallCount:',IntToStr(Statistics.AddSmallCount),3);
 AddItemEx(AResponse,'AddLargeCount:',IntToStr(Statistics.AddLargeCount),3);
 AddItemEx(AResponse,'RemoveSmallCount:',IntToStr(Statistics.RemoveSmallCount),3);
 AddItemEx(AResponse,'RemoveLargeCount:',IntToStr(Statistics.RemoveLargeCount),3);
 AddItemEx(AResponse,'SmallUnavailableCount:',IntToStr(Statistics.SmallUnavailableCount),3);
 {$ELSE}
 {Not Defined} 
 AddBlank(AResponse);
 AddItemEx(AResponse,'HEAP_STATISTICS not defined','',3);
 {$ENDIF}
 AddBlank(AResponse);
 
 {Add Heap Blocks}
 AddBold(AResponse,'Heap Blocks','');
 {Free}
 AddBlank(AResponse);
 AddItemEx(AResponse,'FreeHeapBlockCount:',IntToStr(GetHeapBlockCount(HEAP_STATE_FREE)),3);
 AddItemEx(AResponse,'FreeHeapBlockMin:',IntToStr(GetHeapBlockMin(HEAP_STATE_FREE)),3);
 AddItemEx(AResponse,'FreeHeapBlockMax:',IntToStr(GetHeapBlockMax(HEAP_STATE_FREE)),3);
 {Used}
 AddBlank(AResponse);
 AddItemEx(AResponse,'UsedHeapBlockCount:',IntToStr(GetHeapBlockCount(HEAP_STATE_USED)),3);
 AddItemEx(AResponse,'UsedHeapBlockMin:',IntToStr(GetHeapBlockMin(HEAP_STATE_USED)),3);
 AddItemEx(AResponse,'UsedHeapBlockMax:',IntToStr(GetHeapBlockMax(HEAP_STATE_USED)),3);
 AddBlank(AResponse);
 
 {Add Footer}
 AddFooter(AResponse); 
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TWebStatusHeap}
constructor TWebStatusHeap.Create(AMain:TWebStatusMain);
begin
 {}
 FCaption:='Heap Blocks'; {Must be before create for register}
 inherited Create(AMain);
 Name:='/heap';
 
 if FMain <> nil then Name:=FMain.Name + Name;
end; 

{==============================================================================}

function TWebStatusHeap.FlagsToFlagName(AFlags:LongWord):String;
begin
 {}
 Result:='';
 
 {Check Flags}
 if (AFlags and HEAP_FLAG_SHARED) = HEAP_FLAG_SHARED then
  begin
   Result:='HEAP_FLAG_SHARED';
  end
 else if (AFlags and HEAP_FLAG_LOCAL) = HEAP_FLAG_LOCAL then 
  begin
   Result:='HEAP_FLAG_LOCAL';
  end
 else if (AFlags and HEAP_FLAG_CODE) = HEAP_FLAG_CODE then 
  begin
   Result:='HEAP_FLAG_CODE';
  end
 else if (AFlags and HEAP_FLAG_DEVICE) = HEAP_FLAG_DEVICE then 
  begin
   Result:='HEAP_FLAG_DEVICE';
  end
 else if (AFlags and HEAP_FLAG_NOCACHE) = HEAP_FLAG_NOCACHE then 
  begin
   Result:='HEAP_FLAG_NOCACHE';
  end
 else if (AFlags and HEAP_FLAG_NONSHARED) = HEAP_FLAG_NONSHARED then 
  begin
   Result:='HEAP_FLAG_NONSHARED';
  end
 else if (AFlags and HEAP_FLAG_LOCKED) = HEAP_FLAG_LOCKED then 
  begin
   Result:='HEAP_FLAG_LOCKED';
  end
 else if (AFlags and HEAP_FLAG_IRQ) = HEAP_FLAG_IRQ then 
  begin
   Result:='HEAP_FLAG_IRQ';
  end
 else if (AFlags and HEAP_FLAG_FIQ) = HEAP_FLAG_FIQ then 
  begin
   Result:='HEAP_FLAG_FIQ';
  end
 else if (AFlags and HEAP_FLAG_RECLAIM) = HEAP_FLAG_RECLAIM then 
  begin
   Result:='HEAP_FLAG_RECLAIM';
  end;

 {Check Result}
 if Length(Result) = 0 then
  begin
   Result:='HEAP_FLAG_NORMAL';
  end; 
end;
 
{==============================================================================}

function TWebStatusHeap.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
var
 Count:LongWord;
 Current:PHeapSnapshot;
 Snapshot:PHeapSnapshot;
begin
 {}
 Result:=False;
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;

 {Add Header (5 column)}
 AddHeaderEx(AResponse,GetTitle,Self,5); 

 {Add Free Blocks} 
 AddBold5Column(AResponse,'Address','Size','State','Flags','Affinity');
 AddBlankEx(AResponse,5);
 
 {Create Snapshot (Free)}
 Snapshot:=CreateHeapSnapshot(HEAP_STATE_FREE);
 if Snapshot <> nil then
  begin
   Count:=0;
   
   {Get First}
   Current:=Snapshot;
   while Current <> nil do
    begin
     {Add Item}
     AddItem5Column(AResponse,'0x' + IntToHex(Current.Adddress,8),IntToStr(Current.Size),HeapStateToString(Current.State),FlagsToFlagName(Current.Flags),'0x' + IntToHex(Current.Affinity,8));
   
     {Update Count}
     Inc(Count);
     if (WEBSTATUS_HEAP_FREE_COUNT > 0) and (Count >= WEBSTATUS_HEAP_FREE_COUNT) then
      begin
       AddItem5Column(AResponse,'','(Terminated due to WEBSTATUS_HEAP_FREE_COUNT limit)','','','');
       Break;
      end; 
   
     {Get Next}
     Current:=Current.Next;
    end;
   
   {Destroy Snapshot}
   DestroyHeapSnapshot(Snapshot);
  end; 
 
 {Add Used Blocks} 
 AddBlankEx(AResponse,5);
 AddBold5Column(AResponse,'Address','Size','State','Flags','Affinity');
 AddBlankEx(AResponse,5);
 
 {Create Snapshot (Used)}
 Snapshot:=CreateHeapSnapshot(HEAP_STATE_USED);
 if Snapshot <> nil then
  begin
   Count:=0;
   
   {Get First}
   Current:=Snapshot;
   while Current <> nil do
    begin
     {Add Item}
     AddItem5Column(AResponse,'0x' + IntToHex(Current.Adddress,8),IntToStr(Current.Size),HeapStateToString(Current.State),FlagsToFlagName(Current.Flags),'0x' + IntToHex(Current.Affinity,8));

     {Update Count}
     Inc(Count);
     if (WEBSTATUS_HEAP_USED_COUNT > 0) and (Count >= WEBSTATUS_HEAP_USED_COUNT) then
      begin
       AddItem5Column(AResponse,'','(Terminated due to WEBSTATUS_HEAP_USED_COUNT limit)','','','');
       Break;
      end; 
     
     {Get Next}
     Current:=Current.Next;
    end;
   
   {Destroy Snapshot}
   DestroyHeapSnapshot(Snapshot);
  end; 
 
 {Add Footer (5 column)}
 AddFooterEx(AResponse,5); 
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TWebStatusCPU}
constructor TWebStatusCPU.Create(AMain:TWebStatusMain);
begin
 {}
 FCaption:='CPU'; {Must be before create for register}
 inherited Create(AMain);
 Name:='/cpu';
 
 if FMain <> nil then Name:=FMain.Name + Name;
end; 

{==============================================================================}

function TWebStatusCPU.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
var
 Count:LongWord;
 Address:PtrUInt;
 Length:LongWord;
begin
 {}
 Result:=False;
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;

 {Add Header}
 AddHeader(AResponse,GetTitle,Self); 

 {Add CPU Arch}
 AddItem(AResponse,'CPU Arch:',CPUArchToString(CPUGetArch));
 
 {Add CPU Type}
 AddItem(AResponse,'CPU Type:',CPUTypeToString(CPUGetType));

 {Add CPU Model}
 AddItem(AResponse,'CPU Model:',CPUModelToString(CPUGetModel));

 {Add CPU Revision}
 AddItem(AResponse,'CPU Revision:','0x' + IntToHex(CPUGetRevision,8));
 
 {Add CPU Description}
 AddItem(AResponse,'CPU Description:',CPUGetDescription);
 
 {Add CPU Boot}
 AddItem(AResponse,'CPU Boot:',CPUIDToString(CPUGetBoot));

 {Add CPU Mask}
 AddItem(AResponse,'CPU Mask:','0x' + IntToHex(CPUGetMask,8));
 
 {Add CPU Count}
 AddItem(AResponse,'CPU Count:',IntToStr(CPUGetCount));
 
 {Add CPU Mode}
 AddItem(AResponse,'CPU Mode:','0x' + IntToHex(CPUGetMode,8));
 
 //To Do //CPU State
 
 {Add CPU Mode}
 AddItem(AResponse,'CPU Current:',CPUIDToString(CPUGetCurrent));

 {Add CPU Memory}
 CPUGetMemory(Address,Length);
 AddItem(AResponse,'CPU Memory :','Address: ' + '0x' + IntToHex(Address,8));
 AddItem(AResponse,'','Size: ' + IntToStr(Length));
 
 {Add CPU Utilization}
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     {AddItem(AResponse,'CPU Utilization :',CPUIDToString(Count) + ': ' + IntToStr(CPUGetUtilization(Count)));}
     AddItem(AResponse,'CPU Utilization :',CPUIDToString(Count) + ': ' + FloatToStr(CPUGetPercentage(Count)) + '%');
    end
   else
    begin
     {AddItem(AResponse,'',CPUIDToString(Count) + ': ' + IntToStr(CPUGetUtilization(Count)));}
     AddItem(AResponse,'',CPUIDToString(Count) + ': ' + FloatToStr(CPUGetPercentage(Count)) + '%');
    end;    
  end; 
 
 {Add System Utilization}
 AddBlank(AResponse);
 AddItem(AResponse,'System Utilization (Average):',FloatToStr(CPUGetPercentage(CPU_ID_ALL)) + '%');
 
 {Add L1 Cache}
 AddBlank(AResponse);
 AddItem(AResponse,'L1 Cache Type:',CacheTypeToString(L1CacheGetType));
 AddItem(AResponse,'L1 Data Cache Size:',IntToStr(L1DataCacheGetSize));
 AddItem(AResponse,'L1 Instruction Cache Size:',IntToStr(L1InstructionCacheGetSize));
 AddItem(AResponse,'L1 Data Cache Line Size:',IntToStr(L1DataCacheGetLineSize));
 AddItem(AResponse,'L1 Instruction Cache Line Size:',IntToStr(L1InstructionCacheGetLineSize));
 
 {Add L2 Cache}
 AddBlank(AResponse);
 AddItem(AResponse,'L2 Cache Type:',CacheTypeToString(L2CacheGetType));
 AddItem(AResponse,'L2 Cache Size:',IntToStr(L2CacheGetSize));
 AddItem(AResponse,'L2 Cache Line Size:',IntToStr(L2CacheGetLineSize));
 
 {Add Footer}
 AddFooter(AResponse); 
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TWebStatusFPU}
constructor TWebStatusFPU.Create(AMain:TWebStatusMain);
begin
 {}
 FCaption:='FPU'; {Must be before create for register}
 inherited Create(AMain);
 Name:='/fpu';
 
 if FMain <> nil then Name:=FMain.Name + Name;
end; 

{==============================================================================}

function TWebStatusFPU.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
begin
 {}
 Result:=False;
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;

 {Add Header}
 AddHeader(AResponse,GetTitle,Self); 

 {Add FPU Type}
 AddItem(AResponse,'FPU Type:',FPUTypeToString(FPUGetType));

 //To Do //FPU State
 
 {Add Footer}
 AddFooter(AResponse); 
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TWebStatusGPU}
constructor TWebStatusGPU.Create(AMain:TWebStatusMain);
begin
 {}
 FCaption:='GPU'; {Must be before create for register}
 inherited Create(AMain);
 Name:='/gpu';
 
 if FMain <> nil then Name:=FMain.Name + Name;
end; 

{==============================================================================}

function TWebStatusGPU.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
var
 Address:PtrUInt;
 Length:LongWord;
begin
 {}
 Result:=False;
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;

 {Add Header}
 AddHeader(AResponse,GetTitle,Self); 

 {Add GPU Type}
 AddItem(AResponse,'GPU Type:',GPUTypeToString(GPUGetType));

 //To Do //GPU State

 {Add GPU Memory}
 GPUGetMemory(Address,Length);
 AddItem(AResponse,'GPU Memory :','Address: ' + '0x' + IntToHex(Address,8));
 AddItem(AResponse,'','Size: ' + IntToStr(Length));
 
 {Add Footer}
 AddFooter(AResponse); 
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TWebStatusRTL}
constructor TWebStatusRTL.Create(AMain:TWebStatusMain);
begin
 {}
 FCaption:='RTL'; {Must be before create for register}
 inherited Create(AMain);
 Name:='/rtl';
 
 if FMain <> nil then Name:=FMain.Name + Name;
end; 

{==============================================================================}

function TWebStatusRTL.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
var
 Count:LongWord;
 Table:PRtlInitFinalTable;
begin
 {}
 Result:=False;
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;

 {Add Header}
 AddHeader(AResponse,GetTitle,Self); 

 {Add TEXT Start}
 AddItem(AResponse,'TEXT Start:','0x' + IntToHex(LongWord(@_text_start),8));

 {Add ETEXT}
 AddItem(AResponse,'ETEXT:','0x' + IntToHex(LongWord(@_etext),8));

 {Add DATA}
 AddItem(AResponse,'DATA:','0x' + IntToHex(LongWord(@_data),8));

 {Add EDATA}
 AddItem(AResponse,'EDATA:','0x' + IntToHex(LongWord(@_edata),8));

 {Add BSS Start}
 AddItem(AResponse,'BSS Start:','0x' + IntToHex(LongWord(@_bss_start),8));

 {Add BSS End}
 AddItem(AResponse,'BSS End:','0x' + IntToHex(LongWord(@_bss_end),8));

 {Add ThreadVarBlockSize}
 AddBlank(AResponse);
 AddItem(AResponse,'ThreadVarBlockSize:',IntToStr(ThreadVarBlockSize));

 {Add InitProc/ExitProc}
 AddBlank(AResponse); 
 AddItem(AResponse,'InitProc:','0x' + IntToHex(LongWord(InitProc),8));
 AddItem(AResponse,'ExitProc:','0x' + IntToHex(LongWord(ExitProc),8));
 
 {Add ErrorBase/ErrorAddr/ErrorCode}
 AddBlank(AResponse); 
 AddItem(AResponse,'ErrorBase:','0x' + IntToHex(LongWord(RtlErrorBase),8));
 AddItem(AResponse,'ErrorAddr:','0x' + IntToHex(LongWord(ErrorAddr),8));
 AddItem(AResponse,'ErrorCode:','0x' + IntToHex(ErrorCode,4));
 
 {Add InitFinalTable}
 Table:=PRtlInitFinalTable(@RtlInitFinalTable);
 if Table <> nil then
  begin
   AddBlank(AResponse);
   AddItem(AResponse,'InitFinalTable:','');
   AddItemEx(AResponse,'TableCount:',IntToStr(Table.TableCount),2);
   AddItemEx(AResponse,'InitCount:',IntToStr(Table.InitCount),2);
   for Count:=1 to Table.InitCount do
    begin
     AddItemEx(AResponse,'Procs[' + IntToStr(Count) + '].InitProc:','0x' + IntToHex(LongWord(@Table.Procs[Count].InitProc),8),2);
     AddItemEx(AResponse,'Procs[' + IntToStr(Count) + '].FinalProc:','0x' + IntToHex(LongWord(@Table.Procs[Count].FinalProc),8),2);
    end;
  end;
  
 {Add DefaultFormatSettings}
 AddBlank(AResponse);
 AddItem(AResponse,'DefaultFormatSettings:','');
 AddItemEx(AResponse,'CurrencyFormat:',IntToStr(DefaultFormatSettings.CurrencyFormat),2);
 AddItemEx(AResponse,'NegCurrFormat:',IntToStr(DefaultFormatSettings.NegCurrFormat),2);
 AddItemEx(AResponse,'ThousandSeparator:',DefaultFormatSettings.ThousandSeparator,2);
 AddItemEx(AResponse,'DecimalSeparator:',DefaultFormatSettings.DecimalSeparator,2);
 AddItemEx(AResponse,'CurrencyDecimals:',IntToStr(DefaultFormatSettings.CurrencyDecimals),2);
 AddItemEx(AResponse,'DateSeparator:',DefaultFormatSettings.DateSeparator,2);
 AddItemEx(AResponse,'TimeSeparator:',DefaultFormatSettings.TimeSeparator,2);
 AddItemEx(AResponse,'ListSeparator:',DefaultFormatSettings.ListSeparator,2);
 AddItemEx(AResponse,'CurrencyString:',DefaultFormatSettings.CurrencyString,2);
 AddItemEx(AResponse,'ShortDateFormat:',DefaultFormatSettings.ShortDateFormat,2);
 AddItemEx(AResponse,'LongDateFormat:',DefaultFormatSettings.LongDateFormat,2);
 AddItemEx(AResponse,'TimeAMString:',DefaultFormatSettings.TimeAMString,2);
 AddItemEx(AResponse,'TimePMString:',DefaultFormatSettings.TimePMString,2);
 AddItemEx(AResponse,'ShortTimeFormat:',DefaultFormatSettings.ShortTimeFormat,2);
 AddItemEx(AResponse,'LongTimeFormat:',DefaultFormatSettings.LongTimeFormat,2);
 for Count:=1 to 12 do
  begin
   if Count = 1 then 
    begin
     AddItemEx(AResponse,'ShortMonthNames:',DefaultFormatSettings.ShortMonthNames[Count],2);
    end
   else
    begin
     AddItemEx(AResponse,'',DefaultFormatSettings.ShortMonthNames[Count],2);
    end;
  end; 
 for Count:=1 to 12 do
  begin
   if Count = 1 then 
    begin
     AddItemEx(AResponse,'LongMonthNames:',DefaultFormatSettings.LongMonthNames[Count],2);
    end
   else
    begin
     AddItemEx(AResponse,'',DefaultFormatSettings.LongMonthNames[Count],2);
    end;
  end; 
 for Count:=1 to 7 do
  begin
   if Count = 1 then 
    begin
     AddItemEx(AResponse,'ShortDayNames:',DefaultFormatSettings.ShortDayNames[Count],2);
    end
   else
    begin
     AddItemEx(AResponse,'',DefaultFormatSettings.ShortDayNames[Count],2);
    end;
  end; 
 for Count:=1 to 7 do
  begin
   if Count = 1 then 
    begin
     AddItemEx(AResponse,'LongDayNames:',DefaultFormatSettings.LongDayNames[Count],2);
    end
   else
    begin
     AddItemEx(AResponse,'',DefaultFormatSettings.LongDayNames[Count],2);
    end;
  end; 
 AddItemEx(AResponse,'TwoDigitYearCenturyWindow:',IntToStr(DefaultFormatSettings.TwoDigitYearCenturyWindow),2);
 
 {Add SysLocale}
 //To Do 
 
 {Add ???}
 //To Do //Items from System unit //See: http://www.freepascal.org/docs-html/rtl/system/index-6.html
 
 {Add Footer}
 AddFooter(AResponse); 
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TWebStatusClock}
constructor TWebStatusClock.Create(AMain:TWebStatusMain);
begin
 {}
 FCaption:='Clock'; {Must be before create for register}
 inherited Create(AMain);
 Name:='/clock';
 
 if FMain <> nil then Name:=FMain.Name + Name;
end; 

{==============================================================================}

function TWebStatusClock.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
begin
 {}
 Result:=False;
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;

 {Add Header}
 AddHeader(AResponse,GetTitle,Self); 

 {Add Clock Total}
 AddItem(AResponse,'Clock Total:',IntToStr(ClockGetTotal));

 {Add Clock Count}
 AddItem(AResponse,'Clock Count:',IntToStr(ClockGetCount));
 
 {Add Clock Last}
 AddBlank(AResponse);
 AddItem(AResponse,'Clock Last:',IntToStr(ClockLast));

 {Add Clock Ticks}
 AddItem(AResponse,'Clock Ticks:',IntToStr(ClockTicks));

 {Add Clock Seconds}
 AddItem(AResponse,'Clock Seconds:',IntToStr(ClockSeconds));
 
 {Add Current Time}
 AddBlank(AResponse);
 AddItem(AResponse,'Current Time (UTC):',DateTimeToStr(SystemFileTimeToDateTime(GetCurrentTime)));
 
 {Add Footer}
 AddFooter(AResponse); 
 
 {Return Result}
 Result:=True;
end;
 
{==============================================================================}
{==============================================================================}
{TWebStatusLocale}
constructor TWebStatusLocale.Create(AMain:TWebStatusMain);
begin
 {}
 FCaption:='Locale'; {Must be before create for register}
 inherited Create(AMain);
 Name:='/locale';
 
 if FMain <> nil then Name:=FMain.Name + Name;
end; 

{==============================================================================}

function TWebStatusLocale.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
begin
 {}
 Result:=False;
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;

 {Add Header}
 AddHeader(AResponse,GetTitle,Self); 
 
 {Add Country Code}
 AddItem(AResponse,'Country Code:',IntToStr(COUNTRY_DEFAULT));
 AddBlank(AResponse);
 
 {Add ANSI Code Page}
 AddItem(AResponse,'ANSI Code Page:',IntToStr(GetACP));
 AddBlank(AResponse);
 
 {Add OEM Code Page}
 AddItem(AResponse,'OEM Code Page:',IntToStr(GetOEMCP));
 AddBlank(AResponse);
 
 {Add Default Locale}
 AddItem(AResponse,'Default Locale:',IntToStr(GetSystemDefaultLCID));
 AddBlank(AResponse);
 
 //To Do
 
 {Add Footer}
 AddFooter(AResponse); 
 
 {Return Result}
 Result:=True;
end;
 
{==============================================================================}
{==============================================================================}
{TWebStatusThreading}
constructor TWebStatusThreading.Create(AMain:TWebStatusMain);
begin
 {}
 FCaption:='Threading'; {Must be before create for register}
 inherited Create(AMain);
 Name:='/threading';
 
 if FMain <> nil then Name:=FMain.Name + Name;
end; 

{==============================================================================}

function TWebStatusThreading.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
var
 Count:LongWord;
begin
 {}
 Result:=False;
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;

 {Add Header}
 AddHeader(AResponse,GetTitle,Self); 

 AddBold(AResponse,'Statistics','');
 AddBlank(AResponse);
 
 {Add Thread Count}
 AddItemEx(AResponse,'Thread Count:',IntToStr(ThreadGetCount),2);

 {Add TLS Index Count}
 AddItemEx(AResponse,'TLS Index Count:',IntToStr(ThreadTlsGetCount),2);
 
 {Add Spin Count}
 AddItemEx(AResponse,'Spin Lock Count:',IntToStr(SpinGetCount),2);

 {Add Mutex Count}
 AddItemEx(AResponse,'Mutex Count:',IntToStr(MutexGetCount),2);

 {Add Critical Section Count}
 AddItemEx(AResponse,'Critical Section Count:',IntToStr(CriticalSectionGetCount),2);

 {Add Semaphore Count}
 AddItemEx(AResponse,'Semaphore Count:',IntToStr(SemaphoreGetCount),2);

 {Add Synchronizer Count}
 AddItemEx(AResponse,'Synchronizer Count:',IntToStr(SynchronizerGetCount),2);

 {Add List Count}
 AddItemEx(AResponse,'Thread List Count:',IntToStr(ListGetCount),2);

 {Add Queue Count}
 AddItemEx(AResponse,'Thread Queue Count:',IntToStr(QueueGetCount),2);

 {Add Messageslot Count}
 AddItemEx(AResponse,'Messageslot Count:',IntToStr(MessageslotGetCount),2);

 {Add Mailslot Count}
 AddItemEx(AResponse,'Mailslot Count:',IntToStr(MailslotGetCount),2);

 {Add Buffer Count}
 AddItemEx(AResponse,'Buffer Count:',IntToStr(BufferGetCount),2);

 {Add Event Count}
 AddItemEx(AResponse,'Event Count:',IntToStr(EventGetCount),2);

 {Add Timer Count}
 AddItemEx(AResponse,'Timer Count:',IntToStr(TimerGetCount),2);
 
 {Add Initial Thread}
 AddBlank(AResponse);
 AddBold(AResponse,'Initial Thread','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'INITIAL_TLS_SIZE',IntToStr(INITIAL_TLS_SIZE),3);
 AddItemEx(AResponse,'INITIAL_STACK_SIZE',IntToStr(INITIAL_STACK_SIZE),3);
 AddItemEx(AResponse,'INITIAL_STACK_BASE','0x' + IntToHex(INITIAL_STACK_BASE,8),3);
 
 {Add Boot Stack}
 AddBlank(AResponse);
 AddBold(AResponse,'Boot Thread','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'BOOT_STACK_SIZE',IntToStr(BOOT_STACK_SIZE),3);
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'BOOT_STACK_BASE',CPUIDToString(Count) + ': ' + '0x' + IntToHex(BOOT_STACK_BASE[Count],8),3);
    end
   else
    begin   
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + '0x' + IntToHex(BOOT_STACK_BASE[Count],8),3);
    end; 
  end;
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'BOOT_THREAD_HANDLE',CPUIDToString(Count) + ': ' + '0x' + IntToHex(BOOT_THREAD_HANDLE[Count],8),3);
    end
   else
    begin   
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + '0x' + IntToHex(BOOT_THREAD_HANDLE[Count],8),3);
    end; 
  end;

 {Add Idle Thread}
 AddBlank(AResponse);
 AddBold(AResponse,'Idle Thread','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'IDLE_STACK_SIZE',IntToStr(IDLE_STACK_SIZE),3);
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'IDLE_THREAD_HANDLE',CPUIDToString(Count) + ': ' + '0x' + IntToHex(IDLE_THREAD_HANDLE[Count],8),3);
    end
   else
    begin   
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + '0x' + IntToHex(IDLE_THREAD_HANDLE[Count],8),3);
    end; 
  end;

 {Add IRQ Thread}
 AddBlank(AResponse);
 AddBold(AResponse,'IRQ Thread','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'IRQ_STACK_SIZE',IntToStr(IRQ_STACK_SIZE),3);
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'IRQ_STACK_BASE',CPUIDToString(Count) + ': ' + '0x' + IntToHex(IRQ_STACK_BASE[Count],8),3);
    end
   else
    begin   
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + '0x' + IntToHex(IRQ_STACK_BASE[Count],8),3);
    end; 
  end;
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'IRQ_THREAD_HANDLE',CPUIDToString(Count) + ': ' + '0x' + IntToHex(IRQ_THREAD_HANDLE[Count],8),3);
    end
   else
    begin   
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + '0x' + IntToHex(IRQ_THREAD_HANDLE[Count],8),3);
    end; 
  end;

 {Add FIQ Thread}
 AddBlank(AResponse);
 AddBold(AResponse,'FIQ Thread','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'FIQ_STACK_SIZE',IntToStr(FIQ_STACK_SIZE),3);
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'FIQ_STACK_BASE',CPUIDToString(Count) + ': ' + '0x' + IntToHex(FIQ_STACK_BASE[Count],8),3);
    end
   else
    begin   
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + '0x' + IntToHex(FIQ_STACK_BASE[Count],8),3);
    end; 
  end;
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'FIQ_THREAD_HANDLE',CPUIDToString(Count) + ': ' + '0x' + IntToHex(FIQ_THREAD_HANDLE[Count],8),3);
    end
   else
    begin   
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + '0x' + IntToHex(FIQ_THREAD_HANDLE[Count],8),3);
    end; 
  end;
  
 {Add SWI Thread}
 AddBlank(AResponse);
 AddBold(AResponse,'SWI Thread','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'SWI_STACK_SIZE',IntToStr(SWI_STACK_SIZE),3);
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'SWI_STACK_BASE',CPUIDToString(Count) + ': ' + '0x' + IntToHex(SWI_STACK_BASE[Count],8),3);
    end
   else
    begin   
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + '0x' + IntToHex(SWI_STACK_BASE[Count],8),3);
    end; 
  end;
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'SWI_THREAD_HANDLE',CPUIDToString(Count) + ': ' + '0x' + IntToHex(SWI_THREAD_HANDLE[Count],8),3);
    end
   else
    begin   
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + '0x' + IntToHex(SWI_THREAD_HANDLE[Count],8),3);
    end; 
  end;

 {Add ABORT Stack}
 AddBlank(AResponse);
 AddBold(AResponse,'ABORT Stack','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'ABORT_STACK_SIZE',IntToStr(ABORT_STACK_SIZE),3);
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'ABORT_STACK_BASE',CPUIDToString(Count) + ': ' + '0x' + IntToHex(ABORT_STACK_BASE[Count],8),3);
    end
   else
    begin   
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + '0x' + IntToHex(ABORT_STACK_BASE[Count],8),3);
    end; 
  end;

 {Add UNDEFINED Stack}
 AddBlank(AResponse);
 AddBold(AResponse,'UNDEFINED Stack','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'UNDEFINED_STACK_SIZE',IntToStr(UNDEFINED_STACK_SIZE),3);
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'UNDEFINED_STACK_BASE',CPUIDToString(Count) + ': ' + '0x' + IntToHex(UNDEFINED_STACK_BASE[Count],8),3);
    end
   else
    begin   
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + '0x' + IntToHex(UNDEFINED_STACK_BASE[Count],8),3);
    end; 
  end;
 
 {Add Footer}
 AddFooter(AResponse); 
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TWebStatusThreadList}
constructor TWebStatusThreadList.Create(AMain:TWebStatusMain);
begin
 {}
 FCaption:='Thread List'; {Must be before create for register}
 inherited Create(AMain);
 Name:='/threadlist';
 
 if FMain <> nil then Name:=FMain.Name + Name;
end; 

{==============================================================================}

function TWebStatusThreadList.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
var
 Current:PThreadSnapshot;
 Snapshot:PThreadSnapshot;
begin
 {}
 Result:=False;
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;

 {Add Header (5 column)}
 AddHeaderEx(AResponse,GetTitle,Self,5); 
  
 {Add Thread List} 
 AddBold5Column(AResponse,'Handle','Name','State','Priority','CPU');
 AddBlankEx(AResponse,5);
 
 {Create Snapshot}
 Snapshot:=ThreadSnapshotCreate;
 if Snapshot <> nil then
  begin
   {Get First}
   Current:=Snapshot;
   while Current <> nil do
    begin
     {Add Item}
     AddItem5Column(AResponse,'0x' + IntToHex(Current.Handle,8),Current.Name,ThreadStateToString(Current.State),ThreadPriorityToString(Current.Priority),CPUIDToString(Current.CPU));
   
     {Get Next}
     Current:=Current.Next;
    end;
    
   {Destroy Snapshot}
   ThreadSnapshotDestroy(Snapshot);
  end; 
 
 {Add Footer (5 column)}
 AddFooterEx(AResponse,5); 
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TWebStatusScheduler}
constructor TWebStatusScheduler.Create(AMain:TWebStatusMain);
begin
 {}
 FCaption:='Scheduler'; {Must be before create for register}
 inherited Create(AMain);
 Name:='/scheduler';
 
 if FMain <> nil then Name:=FMain.Name + Name;
end; 

{==============================================================================}

function TWebStatusScheduler.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
var
 Count:LongWord;
 Counter:LongWord;
begin
 {}
 Result:=False;
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;

 {Add Header}
 AddHeader(AResponse,GetTitle,Self); 

 AddBold(AResponse,'Information','');
 
 {Add Scheduler Interrupts/Clocks}
 AddBlank(AResponse);
 AddItemEx(AResponse,'Interrupts per Second:',IntToStr(SCHEDULER_INTERRUPTS_PER_SECOND),2);
 AddItemEx(AResponse,'Interrupts per Millisecond:',IntToStr(SCHEDULER_INTERRUPTS_PER_MILLISECOND),2);
 AddItemEx(AResponse,'Clocks per Interrupt:',IntToStr(SCHEDULER_CLOCKS_PER_INTERRUPT),2);
 AddItemEx(AResponse,'Clocks Tolerance:',IntToStr(SCHEDULER_CLOCKS_TOLERANCE),2);

 {Add CPU Count/Mask/Boot}
 AddBlank(AResponse);
 AddItemEx(AResponse,'CPU Count:',IntToStr(SCHEDULER_CPU_COUNT),2);
 AddItemEx(AResponse,'CPU Mask:','0x' + IntToHex(SCHEDULER_CPU_MASK,8),2);
 AddItemEx(AResponse,'CPU Boot:',CPUIDToString(SCHEDULER_CPU_BOOT),2);
 
 {Add Scheduler Queue Counts}
 AddBlank(AResponse);
 AddBold(AResponse,'Scheduler Queue Counts','');
 AddBlank(AResponse);
 for Count:=QUEUE_TYPE_SCHEDULE_SLEEP to QUEUE_TYPE_SCHEDULE_CRITICAL do
  begin
   for Counter:=0 to CPUGetCount - 1 do
    begin
     if Counter = CPU_ID_0 then
      begin
       AddItemEx(AResponse,QueueTypeToString(Count) + ':',CPUIDToString(Counter) + ': ' + IntToStr(QueueCount(SchedulerGetQueueHandle(Counter,Count))),3);
      end
     else 
      begin
       AddItemEx(AResponse,'',CPUIDToString(Counter) + ': ' + IntToStr(QueueCount(SchedulerGetQueueHandle(Counter,Count))),3);
      end;      
    end;  
   AddBlank(AResponse); 
  end;

 {Add Scheduler Thread Counts}
 AddBlank(AResponse);
 AddBold(AResponse,'Scheduler Thread Counts','');
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   AddItemEx(AResponse,CPUIDToString(Count) + ':',IntToStr(SchedulerGetThreadCount(Count)),3);
  end;

 {Add Scheduler Thread Quantums}
 AddBlank(AResponse);
 AddBold(AResponse,'Scheduler Thread Quantums','');
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   AddItemEx(AResponse,CPUIDToString(Count) + ':',IntToStr(SchedulerGetThreadQuantum(Count)),3);
  end;
  
 {Add Scheduler Priority Masks}
 AddBlank(AResponse);
 AddBold(AResponse,'Scheduler Priority Masks','');
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   AddItemEx(AResponse,CPUIDToString(Count) + ':','0x' + IntToHex(SchedulerGetPriorityMask(Count),8),3);
  end;
 
 {Add Scheduler Migration Quantum}
 AddBlank(AResponse);
 AddBold(AResponse,'Scheduler Migration Quantum','');
 AddBlank(AResponse);
 AddItemEx(AResponse,CPUIDToString(CPU_ID_0) + ':',IntToStr(SchedulerGetMigrationQuantum),3);
 
 {Add Scheduler Starvation Quantums}
 AddBlank(AResponse);
 AddBold(AResponse,'Scheduler Starvation Quantums','');
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   AddItemEx(AResponse,CPUIDToString(Count) + ':',IntToStr(SchedulerGetStarvationQuantum(Count)),3);
  end;
 
 {Add Scheduler Priority Quantums}
 AddBlank(AResponse);
 AddBold(AResponse,'Scheduler Priority Quantums','');
 AddBlank(AResponse);
 for Count:=0 to THREAD_PRIORITY_COUNT - 1 do
  begin
   AddItemEx(AResponse,ThreadPriorityToString(Count) + ':',IntToStr(SchedulerGetPriorityQuantum(Count)),3);
  end;
 
 {Add Scheduler Termination Quantum}
 AddBlank(AResponse);
 AddItem(AResponse,'Scheduler Termination Quantum:',IntToStr(SCHEDULER_TERMINATION_QUANTUM));
 
 {Add Scheduler Thread Next}
 AddBlank(AResponse);
 AddItem(AResponse,'Scheduler Thread Next:',CPUIDToString(SchedulerGetThreadNext));
 
 {Add Scheduler Thread Migration}
 AddBlank(AResponse);
 AddItem(AResponse,'Scheduler Thread Migration:',SchedulerMigrationToString(SchedulerGetThreadMigration));
 
 {Add Scheduler Thread Preemption}
 AddBlank(AResponse);
 AddBold(AResponse,'Scheduler Thread Preemption','');
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   AddItemEx(AResponse,CPUIDToString(Count) + ':',SchedulerPreemptToString(SchedulerGetThreadPreempt(Count)),3);
  end;

 {Add Scheduler Thread Allocation}
 AddBlank(AResponse);
 AddBold(AResponse,'Scheduler Thread Allocation','');
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   AddItemEx(AResponse,CPUIDToString(Count) + ':',SchedulerAllocationToString(SchedulerGetThreadAllocation(Count)),3);
  end;
 
 {Add Footer}
 AddFooter(AResponse); 
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TWebStatusDevices}
constructor TWebStatusDevices.Create(AMain:TWebStatusMain);
begin
 {}
 FCaption:='Devices'; {Must be before create for register}
 inherited Create(AMain);
 Name:='/devices';
 
 if FMain <> nil then Name:=FMain.Name + Name;
end; 

{==============================================================================}

function TWebStatusDevices.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
var
 Data:TWebStatusData;
begin
 {}
 Result:=False;
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;

 {Add Header (4 column)}
 AddHeaderEx(AResponse,GetTitle,Self,4); 

 {Add Device List} 
 AddBold4Column(AResponse,'Device Id','Name','Class','Bus Type');
 AddBlankEx(AResponse,4);
 
 {Setup Data}
 Data.Document:=Self;
 Data.Host:=AHost;
 Data.Request:=ARequest;
 Data.Response:=AResponse;
 
 {Enumerate Devices}
 DeviceEnumerate(DEVICE_CLASS_ANY,WebStatusDeviceEnumerate,@Data);
 
 {Add Footer (4 column)}
 AddFooterEx(AResponse,4); 
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TWebStatusDrivers}
constructor TWebStatusDrivers.Create(AMain:TWebStatusMain);
begin
 {}
 FCaption:='Drivers'; {Must be before create for register}
 inherited Create(AMain);
 Name:='/drivers';
 
 if FMain <> nil then Name:=FMain.Name + Name;
end; 

{==============================================================================}

function TWebStatusDrivers.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
var
 Data:TWebStatusData;
begin
 {}
 Result:=False;
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;

 {Add Header (4 column)}
 AddHeaderEx(AResponse,GetTitle,Self,4); 

 {Add Driver List} 
 AddBold4Column(AResponse,'Driver Id','Name','Class','State');
 AddBlankEx(AResponse,4);
 
 {Setup Data}
 Data.Document:=Self;
 Data.Host:=AHost;
 Data.Request:=ARequest;
 Data.Response:=AResponse;
 
 {Enumerate Drivers}
 DriverEnumerate(DRIVER_CLASS_ANY,WebStatusDriverEnumerate,@Data);
 
 {Add Footer (4 column)}
 AddFooterEx(AResponse,4); 
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TWebStatusUSB}
constructor TWebStatusUSB.Create(AMain:TWebStatusMain);
begin
 {}
 FCaption:='USB'; {Must be before create for register}
 inherited Create(AMain);
 Name:='/usb';
 
 if FMain <> nil then Name:=FMain.Name + Name;
end; 

{==============================================================================}

function TWebStatusUSB.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
var
 Data:TWebStatusData;
begin
 {}
 Result:=False;
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;

 {Add Header (4 column)}
 AddHeaderEx(AResponse,GetTitle,Self,4); 

 {Add USB Device List} 
 AddBold4Column(AResponse,'Devices','','','');
 AddBlankEx(AResponse,4);
 AddBold4Column(AResponse,'USB Id','Name','Class','Status');
 AddBlankEx(AResponse,4);

 {Setup Data}
 Data.Document:=Self;
 Data.Host:=AHost;
 Data.Request:=ARequest;
 Data.Response:=AResponse;
 
 {Enumerate USB Devices}
 USBDeviceEnumerate(WebStatusUSBDeviceEnumerate,@Data);
 
 {Add USB Host List} 
 AddBlankEx(AResponse,4);
 AddBold4Column(AResponse,'Hosts','','','');
 AddBlankEx(AResponse,4);
 AddBold4Column(AResponse,'Host Id','Name','State','Type');
 AddBlankEx(AResponse,4);
 
 {Setup Data}
 Data.Document:=Self;
 Data.Host:=AHost;
 Data.Request:=ARequest;
 Data.Response:=AResponse;
 
 {Enumerate USB Hosts}
 USBHostEnumerate(WebStatusUSBHostEnumerate,@Data);

 {Add USB Driver List} 
 AddBlankEx(AResponse,4);
 AddBold4Column(AResponse,'Drivers','','','');
 AddBlankEx(AResponse,4);
 AddBold4Column(AResponse,'Driver Id','Name','State','');
 AddBlankEx(AResponse,4);
 
 {Setup Data}
 Data.Document:=Self;
 Data.Host:=AHost;
 Data.Request:=ARequest;
 Data.Response:=AResponse;
 
 {Enumerate USB Drivers}
 USBDriverEnumerate(WebStatusUSBDriverEnumerate,@Data);
 
 {Add Footer (4 column)}
 AddFooterEx(AResponse,4); 
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TWebStatusMMC}
constructor TWebStatusMMC.Create(AMain:TWebStatusMain);
begin
 {}
 FCaption:='MMC/SD'; {Must be before create for register}
 inherited Create(AMain);
 Name:='/mmc';
 
 if FMain <> nil then Name:=FMain.Name + Name;
end; 

{==============================================================================}

function TWebStatusMMC.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
var
 Data:TWebStatusData;
begin
 {}
 Result:=False;
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;

 {Add Header (4 column)}
 AddHeaderEx(AResponse,GetTitle,Self,4); 

 {Add MMC List} 
 AddBold4Column(AResponse,'Devices','','','');
 AddBlankEx(AResponse,4);
 AddBold4Column(AResponse,'MMC Id','Name','State','Type');
 AddBlankEx(AResponse,4);
 
 {Setup Data}
 Data.Document:=Self;
 Data.Host:=AHost;
 Data.Request:=ARequest;
 Data.Response:=AResponse;
 
 {Enumerate MMCs}
 MMCDeviceEnumerate(WebStatusMMCEnumerate,@Data);

 {Add SDHCI List} 
 AddBlankEx(AResponse,4);
 AddBold4Column(AResponse,'Hosts','','','');
 AddBlankEx(AResponse,4);
 AddBold4Column(AResponse,'SDHCI Id','Name','State','Type');
 AddBlankEx(AResponse,4);
 
 {Setup Data}
 Data.Document:=Self;
 Data.Host:=AHost;
 Data.Request:=ARequest;
 Data.Response:=AResponse;
 
 {Enumerate SDHCIs}
 SDHCIHostEnumerate(WebStatusSDHCIEnumerate,@Data);
 
 {Add Footer (4 column)}
 AddFooterEx(AResponse,4); 
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TWebStatusNetwork}
constructor TWebStatusNetwork.Create(AMain:TWebStatusMain);
begin
 {}
 FCaption:='Network'; {Must be before create for register}
 inherited Create(AMain);
 Name:='/network';
 
 if FMain <> nil then Name:=FMain.Name + Name;
end; 

{==============================================================================}

function TWebStatusNetwork.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
var
 Data:TWebStatusData;
begin
 {}
 Result:=False;
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;

 {Add Header (5 column)}
 AddHeaderEx(AResponse,GetTitle,Self,5); 

 {Add Network Info} 
 AddBold5Column(AResponse,'Details','','','','');
 AddBlankEx(AResponse,5);

 {Add Host Name/Domain}
 AddItem5Column(AResponse,'Host Name:',HostGetName,'','','');
 AddItem5Column(AResponse,'Domain Name:',HostGetDomain,'','','');

 //To Do //Host name / IP / Netmask / DNS / DHCP etc
 
 {Add Network List}
 AddBlankEx(AResponse,5); 
 AddBold5Column(AResponse,'Devices','','','',''); 
 AddBlankEx(AResponse,5);
 AddBold5Column(AResponse,'Network Id','Name','State','Status','Type');
 AddBlankEx(AResponse,5);
 
 {Setup Data}
 Data.Document:=Self;
 Data.Host:=AHost;
 Data.Request:=ARequest;
 Data.Response:=AResponse;
 
 {Enumerate Networks}
 NetworkDeviceEnumerate(WebStatusNetworkEnumerate,@Data);
 
 {Add Footer (5 column)}
 AddFooterEx(AResponse,5); 
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TWebStatusStorage}
constructor TWebStatusStorage.Create(AMain:TWebStatusMain);
begin
 {}
 FCaption:='Storage'; {Must be before create for register}
 inherited Create(AMain);
 Name:='/storage';
 
 if FMain <> nil then Name:=FMain.Name + Name;
end; 

{==============================================================================}

function TWebStatusStorage.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
var
 Data:TWebStatusData;
begin
 {}
 Result:=False;
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;

 {Add Header (4 column)}
 AddHeaderEx(AResponse,GetTitle,Self,4); 

 {Add Storage List} 
 AddBold4Column(AResponse,'Storage Id','Name','State','Type');
 AddBlankEx(AResponse,4);
 
 {Setup Data}
 Data.Document:=Self;
 Data.Host:=AHost;
 Data.Request:=ARequest;
 Data.Response:=AResponse;
 
 {Enumerate Storage}
 StorageDeviceEnumerate(WebStatusStorageEnumerate,@Data);
 
 {Add Footer (4 column)}
 AddFooterEx(AResponse,4); 
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TWebStatusFilesystem}
constructor TWebStatusFilesystem.Create(AMain:TWebStatusMain);
begin
 {}
 FCaption:='Filesystem'; {Must be before create for register}
 inherited Create(AMain);
 Name:='/filesystem';
 
 if FMain <> nil then Name:=FMain.Name + Name;
end; 

{==============================================================================}

function TWebStatusFilesystem.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
var
 Drive:TDiskDrive;
 Device:TDiskDevice;
 Volume:TDiskVolume;
 Recognizer:TRecognizer;
 Partition:TDiskPartition;
 Controller:TDiskController;
begin
 {}
 Result:=False;
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;

 {Add Header}
 AddHeader(AResponse,GetTitle,Self); 

 //To Do //Change these to list only and click the link for details etc
 
 {Check Driver}
 if FileSysDriver <> nil then
  begin
   {Add Filesystems}
   AddBold(AResponse,'Filesystems','');
   AddBlank(AResponse);
   Recognizer:=FileSysDriver.GetRecognizerByNext(nil,True,False,FILESYS_LOCK_READ);
   while Recognizer <> nil do
    begin
     AddItemEx(AResponse,'Name:',MakeLink(Recognizer.Name,Name + '?action=filesystem&name=' + Recognizer.Name),2);
     AddBlank(AResponse);
   
     Recognizer:=FileSysDriver.GetRecognizerByNext(Recognizer,True,True,FILESYS_LOCK_READ);
    end;

    {Add Controllers}
   AddBold(AResponse,'Controllers','');
   AddBlank(AResponse);
   Controller:=FileSysDriver.GetControllerByNext(nil,True,False,FILESYS_LOCK_READ);
   while Controller <> nil do
    begin
     AddItemEx(AResponse,'Name:',MakeLink(Controller.Name,Name + '?action=controller&no=' + IntToStr(Controller.ControllerNo)),2);
     AddItemEx(AResponse,'ControllerNo:',IntToStr(Controller.ControllerNo),5);
     AddItemEx(AResponse,'Description:',Controller.Description,5);
     AddBlank(AResponse);
     AddBlank(AResponse);
   
     Controller:=FileSysDriver.GetControllerByNext(Controller,True,True,FILESYS_LOCK_READ);
    end;
   
   {Add Devices}
   AddBold(AResponse,'Devices','');
   AddBlank(AResponse);
   Device:=FileSysDriver.GetDeviceByNext(nil,True,False,FILESYS_LOCK_READ);
   while Device <> nil do
    begin
     AddItemEx(AResponse,'Name:',MakeLink(Device.Name,Name + '?action=device&no=' + IntToStr(Device.DeviceNo)),2);
     AddItemEx(AResponse,'DeviceNo:',IntToStr(Device.DeviceNo),5);
     AddItemEx(AResponse,'Identifier:',Device.Identifier,5);
     AddItemEx(AResponse,'Information:',Device.Information,5);
     AddBlank(AResponse);
     AddItemEx(AResponse,'MediaType:',MediaTypeToString(Device.MediaType),5);
     AddItemEx(AResponse,'FloppyType:',FloppyTypeToString(Device.FloppyType),5);
     AddBlank(AResponse);
     AddItemEx(AResponse,'LBA:',BooleanToString(Device.LBA),5);
     AddItemEx(AResponse,'Ready:',BooleanToString(Device.Ready),5);
     AddItemEx(AResponse,'Locked:',BooleanToString(Device.Locked),5);
     AddItemEx(AResponse,'Removable:',BooleanToString(Device.Removable),5);
     AddBlank(AResponse);
     AddItemEx(AResponse,'Physical C:H:S',IntToStr(Device.PhysicalCylinders) + ':' + IntToStr(Device.PhysicalHeads) + ':' + IntToStr(Device.PhysicalSectors),5);
     AddItemEx(AResponse,'Logical C:H:S',IntToStr(Device.LogicalCylinders) + ':' + IntToStr(Device.LogicalHeads) + ':' + IntToStr(Device.LogicalSectors),5);
     AddBlank(AResponse);
     AddItemEx(AResponse,'SectorSize:',IntToStr(Device.SectorSize),5);
     AddItemEx(AResponse,'SectorCount:',IntToStr(Device.SectorCount),5);
     AddBlank(AResponse);
     AddItemEx(AResponse,'FreeSectors:',IntToStr(Device.FreeSectors),5);
     AddItemEx(AResponse,'AvailableSectors:',IntToStr(Device.AvailableSectors),5);
     AddBlank(AResponse);
     AddBlank(AResponse);
   
     Device:=FileSysDriver.GetDeviceByNext(Device,True,True,FILESYS_LOCK_READ);
    end;
   
   {Add Partitions}
   AddBold(AResponse,'Partitions','');
   AddBlank(AResponse);
   Partition:=FileSysDriver.GetPartitionByNext(nil,True,False,FILESYS_LOCK_READ);
   while Partition <> nil do
    begin
     AddItemEx(AResponse,'Name:',MakeLink(Partition.Name,Name + '?action=partition&no=' + IntToStr(Partition.PartitionNo)),2);
     AddItemEx(AResponse,'Path:',Partition.Path,5);
     AddItemEx(AResponse,'Parent:',Partition.Parent,5);
     AddItemEx(AResponse,'PartitionNo:',IntToStr(Partition.PartitionNo),5);
     AddBlank(AResponse);
     AddItemEx(AResponse,'EntryNo:',IntToStr(Partition.EntryNo),5);
     AddItemEx(AResponse,'PartitionId:',PartitionIdToString(Partition.PartitionId) + ' (Type: ' + IntToStr(Partition.PartitionId) + ')',5);
     AddBlank(AResponse);
     AddItemEx(AResponse,'Begin C:H:S',IntToStr(Partition.BeginCylinder) + ':' + IntToStr(Partition.BeginHead) + ':' + IntToStr(Partition.BeginSector),5);
     AddItemEx(AResponse,'End C:H:S',IntToStr(Partition.EndCylinder) + ':' + IntToStr(Partition.EndHead) + ':' + IntToStr(Partition.EndSector),5);
     AddBlank(AResponse);
     AddItemEx(AResponse,'SectorOffset:',IntToStr(Partition.SectorOffset),5);
     AddItemEx(AResponse,'StartSector:',IntToStr(Partition.StartSector),5);
     AddItemEx(AResponse,'SectorCount:',IntToStr(Partition.SectorCount),5);
     AddBlank(AResponse);
     AddItemEx(AResponse,'Active:',BooleanToString(Partition.Active),5);
     AddItemEx(AResponse,'Extended:',BooleanToString(Partition.Extended),5);
     AddItemEx(AResponse,'Recognized:',BooleanToString(Partition.Recognized),5);
     AddBlank(AResponse);
     AddItemEx(AResponse,'FreeSectors:',IntToStr(Partition.FreeSectors),5);
     AddItemEx(AResponse,'AvailableSectors:',IntToStr(Partition.AvailableSectors),5);
     AddBlank(AResponse);
     AddBlank(AResponse);
   
     Partition:=FileSysDriver.GetPartitionByNext(Partition,True,True,FILESYS_LOCK_READ);
    end;
   
   {Add Volumes}
   AddBold(AResponse,'Volumes','');
   AddBlank(AResponse);
   Volume:=FileSysDriver.GetVolumeByNext(nil,True,False,FILESYS_LOCK_READ);
   while Volume <> nil do
    begin
     AddItemEx(AResponse,'Name:',MakeLink(Volume.Name,Name + '?action=volume&no=' + IntToStr(Volume.VolumeNo)),2);
     AddItemEx(AResponse,'Parent:',Volume.Parent,5);
     AddItemEx(AResponse,'VolumeNo:',IntToStr(Volume.VolumeNo),5);
     AddBlank(AResponse);
     AddItemEx(AResponse,'MaxFile:',IntToStr(Volume.MaxFile),5);
     AddItemEx(AResponse,'MaxPath:',IntToStr(Volume.MaxPath),5);
     AddItemEx(AResponse,'Attributes:','0x' + IntToHex(Volume.Attributes,8),5);
     AddItemEx(AResponse,'SystemName:',Volume.SystemName,5);
     AddItemEx(AResponse,'VolumeName:',Volume.VolumeName,5);
     AddItemEx(AResponse,'VolumeGUID:',Volume.VolumeGUID,5);
     AddItemEx(AResponse,'VolumeSerial:','0x' + IntToHex(Volume.VolumeSerial,8),5);
     AddBlank(AResponse);
     AddItemEx(AResponse,'DriveType:',DriveTypeToString(Volume.DriveType),5);
     AddItemEx(AResponse,'FileSysType:',FilesysTypeToString(Volume.FileSysType),5);
     AddBlank(AResponse);
     AddItemEx(AResponse,'SectorSize:',IntToStr(Volume.SectorSize),5);
     AddItemEx(AResponse,'StartSector:',IntToStr(Volume.StartSector),5);
     AddItemEx(AResponse,'SectorCount:',IntToStr(Volume.SectorCount),5);
     if Volume.FileSystem <> nil then
      begin
       AddBlank(AResponse);
       AddItemEx(AResponse,'File System','',7);
       AddBlank(AResponse);
       AddItemEx(AResponse,'Base Name:',Volume.FileSystem.RootName,10);
       AddItemEx(AResponse,'Base Path:',Volume.FileSystem.RootPath,10);
       AddItemEx(AResponse,'Drive Label:',Volume.FileSystem.GetDriveLabel,10);
       AddItemEx(AResponse,'Drive Serial:','0x' + IntToHex(Volume.FileSystem.GetDriveSerial,8),10);
       AddItemEx(AResponse,'Total Space:',IntToStr(Volume.FileSystem.GetDriveTotalSpaceEx),10);
       AddItemEx(AResponse,'Free Space:',IntToStr(Volume.FileSystem.GetDriveFreeSpaceEx),10);
       AddBlank(AResponse);
       AddItemEx(AResponse,'LogFile Dirty on Mount:',BooleanToString(Volume.FileSystem.LogDirty),10);
       AddItemEx(AResponse,'Volume Dirty on Mount:',BooleanToString(Volume.FileSystem.MountDirty),10);
       AddItemEx(AResponse,'Mark Clean on Dismount:',BooleanToString(Volume.FileSystem.MarkClean),10);
       AddItemEx(AResponse,'Mark Dirty on Dismount:',BooleanToString(Volume.FileSystem.MarkDirty),10);
      end;
     AddBlank(AResponse);
     AddBlank(AResponse);
   
     Volume:=FileSysDriver.GetVolumeByNext(Volume,True,True,FILESYS_LOCK_READ);
    end;

   {Add Drives}
   AddBold(AResponse,'Drives','');
   AddBlank(AResponse);
   Drive:=FileSysDriver.GetDriveByNext(nil,True,False,FILESYS_LOCK_READ);
   while Drive <> nil do
    begin
     AddItemEx(AResponse,'Name:',MakeLink(Drive.Name,Name + '?action=drive&no=' + IntToStr(Drive.DriveNo)),2);
     AddItemEx(AResponse,'Root:',Drive.Root,5);
     AddItemEx(AResponse,'Parent:',Drive.Parent,5);
     AddItemEx(AResponse,'DriveNo:',IntToStr(Drive.DriveNo),5);
     AddBlank(AResponse);
     AddItemEx(AResponse,'MaxFile:',IntToStr(Drive.MaxFile),5);
     AddItemEx(AResponse,'MaxPath:',IntToStr(Drive.MaxPath),5);
     AddItemEx(AResponse,'Attributes:','0x' + IntToHex(Drive.Attributes,8),5);
     AddItemEx(AResponse,'SystemName:',Drive.SystemName,5);
     AddItemEx(AResponse,'VolumeName:',Drive.VolumeName,5);
     AddItemEx(AResponse,'VolumeGUID:',Drive.VolumeGUID,5);
     AddItemEx(AResponse,'VolumeSerial:',IntToHex(Drive.VolumeSerial,8),5);
     AddBlank(AResponse);
     AddItemEx(AResponse,'DriveType:',DriveTypeToString(Drive.DriveType),5);
     AddItemEx(AResponse,'FileSysType:',FilesysTypeToString(Drive.FileSysType),5);
     AddBlank(AResponse);
     AddItemEx(AResponse,'SectorSize:',IntToStr(Drive.SectorSize),5);
     AddItemEx(AResponse,'StartSector:',IntToStr(Drive.StartSector),5);
     AddItemEx(AResponse,'SectorCount:',IntToStr(Drive.SectorCount),5);
     if Drive.FileSystem <> nil then
      begin
       AddBlank(AResponse);
       AddItemEx(AResponse,'File System','',7);
       AddBlank(AResponse);
       AddItemEx(AResponse,'Base Name:',Drive.FileSystem.RootName,10);
       AddItemEx(AResponse,'Base Path:',Drive.FileSystem.RootPath,10);
       AddItemEx(AResponse,'Drive Label:',Drive.FileSystem.GetDriveLabel,10);
       AddItemEx(AResponse,'Drive Serial:',IntToHex(Drive.FileSystem.GetDriveSerial,8),10);
       AddItemEx(AResponse,'Total Space:',IntToStr(Drive.FileSystem.GetDriveTotalSpaceEx),10);
       AddItemEx(AResponse,'Free Space:',IntToStr(Drive.FileSystem.GetDriveFreeSpaceEx),10);
       AddBlank(AResponse);
       AddItemEx(AResponse,'LogFile Dirty on Mount:',BooleanToString(Drive.FileSystem.LogDirty),10);
       AddItemEx(AResponse,'Volume Dirty on Mount:',BooleanToString(Drive.FileSystem.MountDirty),10);
       AddItemEx(AResponse,'Mark Clean on Dismount:',BooleanToString(Drive.FileSystem.MarkClean),10);
       AddItemEx(AResponse,'Mark Dirty on Dismount:',BooleanToString(Drive.FileSystem.MarkDirty),10);
      end;
     AddBlank(AResponse);
     AddBlank(AResponse);
     
     Drive:=FileSysDriver.GetDriveByNext(Drive,True,True,FILESYS_LOCK_READ);
    end;
  end;  
 
 {Add Footer}
 AddFooter(AResponse); 
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TWebStatusCache}
constructor TWebStatusCache.Create(AMain:TWebStatusMain);
begin
 {}
 FCaption:='Disk Cache'; {Must be before create for register}
 inherited Create(AMain);
 Name:='/cache';
 
 if FMain <> nil then Name:=FMain.Name + Name;
end; 

{==============================================================================}

function TWebStatusCache.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
var
 WorkTime:TDateTime;
 Statistics:TCacheStatistics;
begin
 {}
 Result:=False;
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;

 {Add Header}
 AddHeader(AResponse,GetTitle,Self); 

 {Check Driver}
 if FileSysDriver <> nil then
  begin
   {Add Cache Statistics}
   AddBold(AResponse,'Cache Statistics','');
   AddBlank(AResponse);
   if FileSysDriver.GetCacheStatistics(Statistics) then
    begin
     AddItemEx(AResponse,'Page Size:',IntToStr(Statistics.PageSize),2);
     AddItemEx(AResponse,'Page Count:',IntToStr(Statistics.PageCount),2);
     AddItemEx(AResponse,'Cache Size:',IntToStr(Statistics.CacheSize),2);
     AddItemEx(AResponse,'Cache Mode:',CacheModeToString(Statistics.CacheMode),2);
     AddItemEx(AResponse,'Cache State:',CacheStateToString(Statistics.CacheState),2);
     AddItemEx(AResponse,'Flush Timeout:',IntToStr(Statistics.FlushTimeout),2);
     AddItemEx(AResponse,'Discard Timeout:',IntToStr(Statistics.DiscardTimeout),2);
     AddBlank(AResponse);
     AddItemEx(AResponse,'Pages Read Cached:',IntToStr(Statistics.ReadCached),2);
     AddItemEx(AResponse,'Pages Written Back:',IntToStr(Statistics.WriteBack),2);
     AddItemEx(AResponse,'Pages Written Through:',IntToStr(Statistics.WriteThrough),2);
     AddBlank(AResponse);
     AddItemEx(AResponse,'Direct Reads:',IntToStr(Statistics.ReadDirect),2);
     AddItemEx(AResponse,'Direct Writes:',IntToStr(Statistics.WriteDirect),2);
     AddBlank(AResponse);
     AddItemEx(AResponse,'Page Hits:',IntToStr(Statistics.HitCount),2);
     AddItemEx(AResponse,'Page Misses:',IntToStr(Statistics.MissCount),2);
     AddBlank(AResponse);
     AddItemEx(AResponse,'Allocate Failures:',IntToStr(Statistics.FailCount),2);
     AddItemEx(AResponse,'Allocate Successes:',IntToStr(Statistics.SuccessCount),2);
     AddBlank(AResponse);
     AddItemEx(AResponse,'Pages Flushed:',IntToStr(Statistics.FlushCount),2);
     AddItemEx(AResponse,'Pages Discarded:',IntToStr(Statistics.DiscardCount),2);
     AddItemEx(AResponse,'Pages Marked Unknown:', IntToStr(Statistics.UnknownCount),2);
     AddBlank(AResponse);
     WorkTime:=CachePageTimeToDateTime(Statistics.OldestClean);
     AddItemEx(AResponse,'Oldest Clean Page:',IntToStr(Trunc(WorkTime)) + ' days ' + TimeToStr(WorkTime),2);
     WorkTime:=CachePageTimeToDateTime(Statistics.NewestClean);
     AddItemEx(AResponse,'Newest Clean Page:',IntToStr(Trunc(WorkTime)) + ' days ' + TimeToStr(WorkTime),2);
     WorkTime:=CachePageTimeToDateTime(Statistics.OldestDirty);
     AddItemEx(AResponse,'Oldest Dirty Page:',IntToStr(Trunc(WorkTime)) + ' days ' + TimeToStr(WorkTime),2);
     WorkTime:=CachePageTimeToDateTime(Statistics.NewestDirty);
     AddItemEx(AResponse,'Newest Dirty Page:',IntToStr(Trunc(WorkTime)) + ' days ' + TimeToStr(WorkTime),2);
    end;
  end; 
 
 {Add Footer}
 AddFooter(AResponse); 
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TWebStatusKeyboard}
constructor TWebStatusKeyboard.Create(AMain:TWebStatusMain);
begin
 {}
 FCaption:='Keyboard'; {Must be before create for register}
 inherited Create(AMain);
 Name:='/keyboard';
 
 if FMain <> nil then Name:=FMain.Name + Name;
end; 

{==============================================================================}

function TWebStatusKeyboard.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
var
 Data:TWebStatusData;
begin
 {}
 Result:=False;
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;

 {Add Header (4 column)}
 AddHeaderEx(AResponse,GetTitle,Self,4); 

 {Add Keyboard List} 
 AddBold4Column(AResponse,'Keyboard Id','Name','State','Type');
 AddBlankEx(AResponse,4);
 
 {Setup Data}
 Data.Document:=Self;
 Data.Host:=AHost;
 Data.Request:=ARequest;
 Data.Response:=AResponse;
 
 {Enumerate Keyboards}
 KeyboardDeviceEnumerate(WebStatusKeyboardEnumerate,@Data);
 
 {Add Footer (4 column)}
 AddFooterEx(AResponse,4); 
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TWebStatusMouse}
constructor TWebStatusMouse.Create(AMain:TWebStatusMain);
begin
 {}
 FCaption:='Mouse'; {Must be before create for register}
 inherited Create(AMain);
 Name:='/mouse';
 
 if FMain <> nil then Name:=FMain.Name + Name;
end; 

{==============================================================================}

function TWebStatusMouse.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
var
 Data:TWebStatusData;
begin
 {}
 Result:=False;
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;

 {Add Header (4 column)}
 AddHeaderEx(AResponse,GetTitle,Self,4); 

 {Add Mouse List} 
 AddBold4Column(AResponse,'Mouse Id','Name','State','Type');
 AddBlankEx(AResponse,4);
 
 {Setup Data}
 Data.Document:=Self;
 Data.Host:=AHost;
 Data.Request:=ARequest;
 Data.Response:=AResponse;
 
 {Enumerate Mice}
 MouseDeviceEnumerate(WebStatusMouseEnumerate,@Data);
 
 {Add Footer (4 column)}
 AddFooterEx(AResponse,4); 
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TWebStatusFramebuffer}
constructor TWebStatusFramebuffer.Create(AMain:TWebStatusMain);
begin
 {}
 FCaption:='Framebuffer'; {Must be before create for register}
 inherited Create(AMain);
 Name:='/framebuffer';
 
 if FMain <> nil then Name:=FMain.Name + Name;
end; 

{==============================================================================}

function TWebStatusFramebuffer.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
var
 X:LongWord;
 Y:LongWord;
 Top:LongWord;
 Left:LongWord;
 Mode:LongWord;
 Depth:LongWord;
 Order:LongWord;
 Pitch:LongWord;
 Right:LongWord;
 Width:LongWord;
 Height:LongWord;
 Bottom:LongWord;
 FramebufferDevice:PFramebufferDevice;
begin
 {}
 Result:=False;
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;

 {Add Header}
 AddHeader(AResponse,GetTitle,Self); 

 AddBold(AResponse,'Settings','');
 
 {Add Framebuffer Physical Width/Height}
 if FramebufferGetPhysical(Width,Height) = ERROR_SUCCESS then
  begin
   AddBlank(AResponse);
   AddItemEx(AResponse,'Physical Width (Pixels):',IntToStr(Width),2);
   AddItemEx(AResponse,'Physical Height (Pixels):',IntToStr(Height),2);
  end;

 {Add Framebuffer Virtual Width/Height}
 if FramebufferGetVirtual(Width,Height) = ERROR_SUCCESS then
  begin
   AddBlank(AResponse);
   AddItemEx(AResponse,'Virtual Width (Pixels):',IntToStr(Width),2);
   AddItemEx(AResponse,'Virtual Height (Pixels):',IntToStr(Height),2);
  end;

 {Add Framebuffer Depth}
 if FramebufferGetDepth(Depth) = ERROR_SUCCESS then
  begin
   AddBlank(AResponse);
   AddItemEx(AResponse,'Colour Depth (Bits per Pixel):',IntToStr(Depth),2);
  end;

 {Add Framebuffer Pixel Order}
 if FramebufferGetPixelOrder(Order) = ERROR_SUCCESS then
  begin
   AddBlank(AResponse);
   AddItemEx(AResponse,'Pixel Order (BGR/RGB):',IntToStr(Order),2);
  end;

 {Add Framebuffer Alpha Mode}
 if FramebufferGetAlphaMode(Mode) = ERROR_SUCCESS then
  begin
   AddBlank(AResponse);
   AddItemEx(AResponse,'Alpha Mode:',IntToStr(Mode),2);
  end;

 {Add Framebuffer Pitch}
 Pitch:=FramebufferGetPitch;
 if Pitch > 0 then
  begin
   AddBlank(AResponse);
   AddItemEx(AResponse,'Pitch (Bytes per Line):',IntToStr(Pitch),2);
  end;
  
 {Add Framebuffer Virtual Offset}
 if FramebufferGetOffset(X,Y) = ERROR_SUCCESS then
  begin
   AddBlank(AResponse);
   AddItemEx(AResponse,'Virtual Offset X:',IntToStr(X),2);
   AddItemEx(AResponse,'Virtual Offset Y:',IntToStr(Y),2);
  end;

 {Add Framebuffer Overscan}
 if FramebufferGetOverscan(Top,Bottom,Left,Right) = ERROR_SUCCESS then
  begin
   AddBlank(AResponse);
   AddItemEx(AResponse,'Overscan Top (Pixels):',IntToStr(Top),2);
   AddItemEx(AResponse,'Overscan Bottom (Pixels):',IntToStr(Bottom),2);
   AddItemEx(AResponse,'Overscan Left (Pixels):',IntToStr(Left),2);
   AddItemEx(AResponse,'Overscan Right (Pixels):',IntToStr(Right),2);
  end;
 
 AddBlank(AResponse);
 AddBold(AResponse,'Default Device','');
 
 {Get Default Device}
 FramebufferDevice:=FramebufferDeviceGetDefault;
 if FramebufferDevice <> nil then
  begin
   AddBlank(AResponse);
   AddItemEx(AResponse,'Address:','0x' + IntToHex(FramebufferDevice.Address,8),2);
   AddItemEx(AResponse,'Size:',IntToStr(FramebufferDevice.Size),2);
   AddBlank(AResponse);
   AddItemEx(AResponse,'Pitch (Bytes per Line):',IntToStr(Pitch),2);
   AddBlank(AResponse);
   AddItemEx(AResponse,'Colour Depth (Bits per Pixel):',IntToStr(Depth),2);
   AddBlank(AResponse);
   AddItemEx(AResponse,'Pixel Order (BGR/RGB):',IntToStr(Order),2);
   AddBlank(AResponse);
   AddItemEx(AResponse,'Alpha Mode:',IntToStr(Mode),2);
   AddBlank(AResponse);
   AddItemEx(AResponse,'Physical Width (Pixels):',IntToStr(Width),2);
   AddItemEx(AResponse,'Physical Height (Pixels):',IntToStr(Height),2);
   AddBlank(AResponse);
   AddItemEx(AResponse,'Virtual Width (Pixels):',IntToStr(Width),2);
   AddItemEx(AResponse,'Virtual Height (Pixels):',IntToStr(Height),2);
   AddBlank(AResponse);
   AddItemEx(AResponse,'Virtual Offset X:',IntToStr(X),2);
   AddItemEx(AResponse,'Virtual Offset Y:',IntToStr(Y),2);
   AddBlank(AResponse);
   AddItemEx(AResponse,'Overscan Top (Pixels):',IntToStr(Top),2);
   AddItemEx(AResponse,'Overscan Bottom (Pixels):',IntToStr(Bottom),2);
   AddItemEx(AResponse,'Overscan Left (Pixels):',IntToStr(Left),2);
   AddItemEx(AResponse,'Overscan Right (Pixels):',IntToStr(Right),2);
  end;
 
 {Add Footer}
 AddFooter(AResponse); 
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TWebStatusEnvironment}
constructor TWebStatusEnvironment.Create(AMain:TWebStatusMain);
begin
 {}
 FCaption:='Environment'; {Must be before create for register}
 inherited Create(AMain);
 Name:='/environment';
 
 if FMain <> nil then Name:=FMain.Name + Name;
end; 

{==============================================================================}

function TWebStatusEnvironment.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
var
 Count:LongWord;
begin
 {}
 Result:=False;
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;

 {Add Header}
 AddHeader(AResponse,GetTitle,Self); 

 {Add Command Line}
 AddBold(AResponse,'Command Line','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'argc:',IntToStr(argc),2);
 AddBlank(AResponse);
 AddItemEx(AResponse,'ParamCount:',IntToStr(ParamCount),2);
 AddBlank(AResponse);
 for Count:=0 to ParamCount do
  begin
   AddItemEx(AResponse,'ParamStr(' + IntToStr(Count) + '):',ParamStr(Count),2);
  end; 
 AddBlank(AResponse);
 AddItemEx(AResponse,'cmdline:',cmdline,2);
 AddBlank(AResponse);

 {Add Environment Variables}
 AddBold(AResponse,'Environment Variables','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'GetEnvironmentVariableCount:',IntToStr(GetEnvironmentVariableCount),2);
 AddBlank(AResponse);
 for Count:=1 to GetEnvironmentVariableCount do
  begin
   AddItemEx(AResponse,'GetEnvironmentString(' + IntToStr(Count) + '):',GetEnvironmentString(Count),2);
  end;
 AddBlank(AResponse);
 
 {$IFDEF CPUARM}
 AddBold(AResponse,'ARM Specific','');
 AddBlank(AResponse);
 
 {Add ARM Boot Mode}
 AddItemEx(AResponse,'ARM Boot Mode:','0x' + IntToHex(ARMBootMode,8) + ' (' + ARMModeToString(ARMBootMode) + ')',2);

 {Add ARM Boot Vectors}
 AddItemEx(AResponse,'ARM Boot Vectors:','0x' + IntToHex(ARMBootVectors,8),2);
 
 {Add ARM Machine Type}
 AddItemEx(AResponse,'ARM Machine Type:','0x' + IntToHex(ARMMachineType,8),2);
 AddBlank(AResponse);
 
 {Add ARM Boot Tags}
 AddBold(AResponse,'ARM Boot Tags','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'Tags Address:','0x' + IntToHex(ARMTagsAddress,8),2);
 AddBlank(AResponse);
 
 AddItemEx(AResponse,'Tag None Count:',IntToStr(TagNoneCount),2);
 AddBlank(AResponse);
 
 AddItemEx(AResponse,'Tag Core Count:',IntToStr(TagCoreCount),2);
 AddItemEx(AResponse,'Core Flags:','0x' + IntToHex(TagCoreFlags,8),4);
 AddItemEx(AResponse,'Core Page Size:','0x' + IntToHex(TagCorePageSize,8),4);
 AddItemEx(AResponse,'Core Root Device:','0x' + IntToHex(TagCoreRootDevice,8),4);
 AddBlank(AResponse);
 
 AddItemEx(AResponse,'Tag Memory Count:',IntToStr(TagMemoryCount),2);
 AddItemEx(AResponse,'Memory Size:','0x' + IntToHex(TagMemorySize,8),4);
 AddItemEx(AResponse,'Memory Start:','0x' + IntToHex(TagMemoryStart,8),4);
 AddItemEx(AResponse,'Memory Length:','0x' + IntToHex(TagMemoryLength,8),4);
 AddItemEx(AResponse,'Memory Address:','0x' + IntToHex(TagMemoryAddress,8),4);
 AddBlank(AResponse);
 
 AddItemEx(AResponse,'Tag Video Text Count:',IntToStr(TagVideoTextCount),2);
 AddBlank(AResponse);
 
 AddItemEx(AResponse,'Tag Ramdisk Count:',IntToStr(TagRamdiskCount),2);
 AddBlank(AResponse);
 
 AddItemEx(AResponse,'Tag Init RD2 Count:',IntToStr(TagInitRd2Count),2);
 AddBlank(AResponse);
 
 AddItemEx(AResponse,'Tag Serial Count:',IntToStr(TagSerialCount),2);
 AddItemEx(AResponse,'Serial No Low:','0x' + IntToHex(TagSerialNoLow,8),4);
 AddItemEx(AResponse,'Serial No Hight:','0x' + IntToHex(TagSerialNoHigh,8),4);
 AddBlank(AResponse);
 
 AddItemEx(AResponse,'Tag Revision Count:',IntToStr(TagRevisionCount),2);
 AddItemEx(AResponse,'Revision No:','0x' + IntToHex(TagRevisionNo,8),4);
 AddBlank(AResponse);
 
 AddItemEx(AResponse,'Tag Video FB Count:',IntToStr(TagVideoFBCount),2);
 AddBlank(AResponse);
 
 AddItemEx(AResponse,'Tag Command Count:',IntToStr(TagCmdCount),2);
 AddItemEx(AResponse,'Command Size:',IntToStr(TagCommandSize),4);
 AddItemEx(AResponse,'Command Count:',IntToStr(TagCommandCount),4);
 AddItemEx(AResponse,'Command Address:','0x' + IntToHex(LongWord(TagCommandAddress),8),4);
 AddBlank(AResponse);
 {$ENDIF}
 
 {Add Footer}
 AddFooter(AResponse); 
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TWebStatusPageTables}
constructor TWebStatusPageTables.Create(AMain:TWebStatusMain);
begin
 {}
 FCaption:='Page Tables'; {Must be before create for register}
 inherited Create(AMain);
 Name:='/pagetables';
 
 if FMain <> nil then Name:=FMain.Name + Name;
end; 

{==============================================================================}

function TWebStatusPageTables.FlagsToFlagNames(AFlags:LongWord):TStringList;
begin
 {}
 Result:=TStringList.Create;
 
 {Check Flags}
 if (AFlags and PAGE_TABLE_FLAG_NORMAL) = PAGE_TABLE_FLAG_NORMAL then
  begin
   Result.Add('PAGE_TABLE_FLAG_NORMAL');
  end;
 if (AFlags and PAGE_TABLE_FLAG_DEVICE) = PAGE_TABLE_FLAG_DEVICE then
  begin
   Result.Add('PAGE_TABLE_FLAG_DEVICE');
  end;
 if (AFlags and PAGE_TABLE_FLAG_ORDERED) = PAGE_TABLE_FLAG_ORDERED then
  begin
   Result.Add('PAGE_TABLE_FLAG_ORDERED');
  end;
 if (AFlags and PAGE_TABLE_FLAG_SHARED) = PAGE_TABLE_FLAG_SHARED then
  begin
   Result.Add('PAGE_TABLE_FLAG_SHARED');
  end;
 if (AFlags and PAGE_TABLE_FLAG_CACHEABLE) = PAGE_TABLE_FLAG_CACHEABLE then
  begin
   Result.Add('PAGE_TABLE_FLAG_CACHEABLE');
  end;
 if (AFlags and PAGE_TABLE_FLAG_READONLY) = PAGE_TABLE_FLAG_READONLY then
  begin
   Result.Add('PAGE_TABLE_FLAG_READONLY');
  end;
 if (AFlags and PAGE_TABLE_FLAG_READWRITE) = PAGE_TABLE_FLAG_READWRITE then
  begin
   Result.Add('PAGE_TABLE_FLAG_READWRITE');
  end;
 if (AFlags and PAGE_TABLE_FLAG_EXECUTABLE) = PAGE_TABLE_FLAG_EXECUTABLE then
  begin
   Result.Add('PAGE_TABLE_FLAG_EXECUTABLE');
  end;
 if (AFlags and PAGE_TABLE_FLAG_WRITEBACK) = PAGE_TABLE_FLAG_WRITEBACK then
  begin
   Result.Add('PAGE_TABLE_FLAG_WRITEBACK');
  end;
 if (AFlags and PAGE_TABLE_FLAG_WRITETHROUGH) = PAGE_TABLE_FLAG_WRITETHROUGH then
  begin
   Result.Add('PAGE_TABLE_FLAG_WRITETHROUGH');
  end;
 if (AFlags and PAGE_TABLE_FLAG_WRITEALLOCATE) = PAGE_TABLE_FLAG_WRITEALLOCATE then
  begin
   Result.Add('PAGE_TABLE_FLAG_WRITEALLOCATE');
  end;
 
 {Check Flags}
 if Result.Count = 0 then
  begin
   Result.Add('PAGE_TABLE_FLAG_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusPageTables.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
var
 Count:LongWord;
 Address:LongWord;
 Repeated:LongWord;
 FlagNames:TStringList;
 NextEntry:TPageTableEntry;
 CurrentEntry:TPageTableEntry;
begin
 {}
 Result:=False;
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;

 {Add Header (4 column)}
 AddHeaderEx(AResponse,GetTitle,Self,4); 

 {Add Page Tables}
 Address:=$00000000;
 Repeated:=0;
 FillChar(NextEntry,SizeOf(TPageTableEntry),0);
 FillChar(CurrentEntry,SizeOf(TPageTableEntry),0);
 AddBold4Column(AResponse,'Virtual','Physical','Size','Flags');
 AddBlankEx(AResponse,4);
 {Get First} 
 NextEntry:=PageTableGetEntry(Address);
 while NextEntry.Size > 0 do
  begin
   if (NextEntry.Size <> CurrentEntry.Size) or (NextEntry.Flags <> CurrentEntry.Flags) then
    begin
     {Check Repeat}
     if (Address <> $00000000) and ((CurrentEntry.VirtualAddress + CurrentEntry.Size) < NextEntry.VirtualAddress) then
      begin
       {Add Repeated}
       AddItem4Column(AResponse,'','(Repeated for ' + IntToStr(Repeated) + ' entries)','','');
       AddBlankEx(AResponse,4);
       
       {Reset Repeated}
       Repeated:=0;
      end; 
     
     {Get Flag Names}
     FlagNames:=FlagsToFlagNames(NextEntry.Flags);
     
     {Add Item}
     AddItem4Column(AResponse,'0x' + IntToHex(NextEntry.VirtualAddress,8),'0x' + IntToHex(NextEntry.PhysicalAddress,8),'0x' + IntToHex(NextEntry.Size,8),FlagNames.Strings[0]);
     
     {Check Flag Count}
     if FlagNames.Count > 1 then
      begin
       for Count:=1 to FlagNames.Count - 1 do
        begin
         {Add Flag Name}
         AddItem4Column(AResponse,'','','',FlagNames.Strings[count]);
        end;
      end;
     FlagNames.Free;
     
     {Add Blank}
     AddBlankEx(AResponse,4);
     
     {Save Current}
     CurrentEntry:=NextEntry;
    end
   else
    begin
     {Increment Repeated}
     Inc(Repeated);
    end;    

   {Check Address}
   if (Address + NextEntry.Size) = $00000000 then
    begin
     if Repeated > 0 then
      begin
       {Add Repeated}
       AddItem4Column(AResponse,'','(Repeated for ' + IntToStr(Repeated) + ' entries)','','');
      end;
      
     Break;
    end; 
   
   {Increment Address}
   Inc(Address,NextEntry.Size);
   
   {Get Next}
   NextEntry:=PageTableGetEntry(Address);
  end;
 
 {Add Footer (4 column)}
 AddFooterEx(AResponse,4); 
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TWebStatusConfiguration}
constructor TWebStatusConfiguration.Create(AMain:TWebStatusMain);
begin
 {}
 FCaption:='Configuration'; {Must be before create for register}
 inherited Create(AMain);
 Name:='/configuration';
 
 if FMain <> nil then Name:=FMain.Name + Name;
end; 

{==============================================================================}

function TWebStatusConfiguration.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
begin
 {}
 Result:=False;
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;

 {Add Header}
 AddHeader(AResponse,GetTitle,Self); 

 {Add Kernel Image Sections}
 AddBold(AResponse,'Kernel Image Sections','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'_text_start:','0x' + IntToHex(LongWord(@_text_start),8),2);
 AddItemEx(AResponse,'_etext:','0x' + IntToHex(LongWord(@_etext),8),2);
 AddItemEx(AResponse,'_data:','0x' + IntToHex(LongWord(@_data),8),2);
 AddItemEx(AResponse,'_edata:','0x' + IntToHex(LongWord(@_edata),8),2);
 AddItemEx(AResponse,'_bss_start:','0x' + IntToHex(LongWord(@_bss_start),8),2);
 AddItemEx(AResponse,'_bss_end:','0x' + IntToHex(LongWord(@_bss_end),8),2);
 AddBlank(AResponse);

 {Add RTL Initial Heap Allocation} 
 AddBold(AResponse,'RTL Initial Heap Allocation','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'RtlHeapAddr:','0x' + IntToHex(LongWord(@RtlHeapAddr),8),2);
 AddItemEx(AResponse,'RtlHeapSize:','0x' + IntToHex(RtlHeapSize,8),2);
 AddBlank(AResponse);
 
 {Add RTL Error Handling}
 AddBold(AResponse,'RTL Error Handling','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'RtlErrorBase:','0x' + IntToHex(LongWord(RtlErrorBase),8),2);
 AddBlank(AResponse);
 
 {Add RTL Initialization}
 AddBold(AResponse,'RTL Initialization','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'RtlInitFinalTable:','0x' + IntToHex(LongWord(@RtlInitFinalTable),8),2);
 AddBlank(AResponse);
 
 {Add Memory and Peripheral Mapping}
 AddBold(AResponse,'Memory and Peripheral Mapping','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'IO_BASE:','0x' + IntToHex(IO_BASE,8),2);
 AddItemEx(AResponse,'IO_ALIAS:','0x' + IntToHex(IO_ALIAS,8),2);
 AddItemEx(AResponse,'BUS_ALIAS:','0x' + IntToHex(BUS_ALIAS,8),2);
 AddBlank(AResponse);

 {Add Secure Boot}
 AddBold(AResponse,'Secure Boot','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'SECURE_BOOT:',BooleanToString(SECURE_BOOT),2);
 AddBlank(AResponse);

 {Add Startup Handler Address}
 AddBold(AResponse,'Startup Handler Address','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'STARTUP_ADDRESS:','0x' + IntToHex(STARTUP_ADDRESS,8),2);
 AddBlank(AResponse);

 {Add Memory Base Mapping}
 AddBold(AResponse,'Memory Base Mapping','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'MEMORY_BASE:','0x' + IntToHex(MEMORY_BASE,8),2);
 AddItemEx(AResponse,'MEMORY_SIZE:','0x' + IntToHex(MEMORY_SIZE,8),2);
 AddBlank(AResponse);

 {Add Memory Sizes}
 AddBold(AResponse,'Memory Sizes','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'MEMORY_IRQ_SIZE:',IntToStr(MEMORY_IRQ_SIZE),2);
 AddItemEx(AResponse,'MEMORY_FIQ_SIZE:',IntToStr(MEMORY_FIQ_SIZE),2);
 AddItemEx(AResponse,'MEMORY_LOCAL_SIZE:',IntToStr(MEMORY_LOCAL_SIZE),2);
 AddItemEx(AResponse,'MEMORY_SHARED_SIZE:',IntToStr(MEMORY_SHARED_SIZE),2);
 AddItemEx(AResponse,'MEMORY_DEVICE_SIZE:',IntToStr(MEMORY_DEVICE_SIZE),2);
 AddItemEx(AResponse,'MEMORY_NOCACHE_SIZE:',IntToStr(MEMORY_NOCACHE_SIZE),2);
 AddItemEx(AResponse,'MEMORY_NONSHARED_SIZE:',IntToStr(MEMORY_NONSHARED_SIZE),2);
 AddBlank(AResponse);
 
 {Add }
 AddBold(AResponse,'','');
 AddBlank(AResponse);
 //To Do
 AddBlank(AResponse);

 {Add }
 AddBold(AResponse,'','');
 AddBlank(AResponse);
 //To Do
 AddBlank(AResponse);

 {Add }
 AddBold(AResponse,'','');
 AddBlank(AResponse);
 //To Do
 AddBlank(AResponse);

 {Add }
 AddBold(AResponse,'','');
 AddBlank(AResponse);
 //To Do
 AddBlank(AResponse);

 {Add }
 AddBold(AResponse,'','');
 AddBlank(AResponse);
 //To Do
 AddBlank(AResponse);

 {Add }
 AddBold(AResponse,'','');
 AddBlank(AResponse);
 //To Do
 AddBlank(AResponse);

 {Add }
 AddBold(AResponse,'','');
 AddBlank(AResponse);
 //To Do
 AddBlank(AResponse);

 {Add }
 AddBold(AResponse,'','');
 AddBlank(AResponse);
 //To Do
 AddBlank(AResponse);

 {Add }
 AddBold(AResponse,'','');
 AddBlank(AResponse);
 //To Do
 AddBlank(AResponse);
 
 {Add Footer}
 AddFooter(AResponse); 
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TWebStatusDebug}
{$IF DEFINED(LOCK_DEBUG) or DEFINED(SPIN_DEBUG) or DEFINED(MUTEX_DEBUG) or DEFINED(CLOCK_DEBUG) or DEFINED(SCHEDULER_DEBUG) or DEFINED(INTERRUPT_DEBUG) or DEFINED(EXCEPTION_DEBUG)}
constructor TWebStatusDebug.Create(AMain:TWebStatusMain);
begin
 {}
 FCaption:='Debug'; {Must be before create for register}
 inherited Create(AMain);
 Name:='/debug';
 
 if FMain <> nil then Name:=FMain.Name + Name;
end; 

{==============================================================================}

function TWebStatusDebug.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
var
 Count:LongWord;
 Thread:TThreadHandle;
begin
 {}
 Result:=False;
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;

 {Add Header}
 AddHeader(AResponse,GetTitle,Self); 

 {$IFDEF LOCK_DEBUG}
 {Add Lock Debug}
 AddBold(AResponse,'Lock Debug','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'SpinDeadlockCounter:',IntToStr(SpinDeadlockCounter),2);
 AddItemEx(AResponse,'SpinRecursionCounter:',IntToStr(SpinRecursionCounter),2);
 AddItemEx(AResponse,'SpinRecursionThread:','0x' + IntToHex(LongWord(SpinRecursionThread),8),2);
 AddItemEx(AResponse,'SpinIRQThreadCounter:',IntToStr(SpinIRQThreadCounter),2);
 AddItemEx(AResponse,'SpinFIQThreadCounter:',IntToStr(SpinFIQThreadCounter),2);
 AddItemEx(AResponse,'SpinSWIThreadCounter:',IntToStr(SpinSWIThreadCounter),2);
 AddItemEx(AResponse,'SpinIdleThreadCounter:',IntToStr(SpinIdleThreadCounter),2);
 AddItemEx(AResponse,'MutexDeadlockCounter:',IntToStr(MutexDeadlockCounter),2);
 AddItemEx(AResponse,'MutexRecursionCounter:',IntToStr(MutexRecursionCounter),2);
 AddItemEx(AResponse,'MutexRecursionThread:','0x' + IntToHex(LongWord(MutexRecursionThread),8),2);
 AddItemEx(AResponse,'MutexIRQThreadCounter:',IntToStr(MutexIRQThreadCounter),2);
 AddItemEx(AResponse,'MutexFIQThreadCounter:',IntToStr(MutexFIQThreadCounter),2);
 AddItemEx(AResponse,'MutexSWIThreadCounter:',IntToStr(MutexSWIThreadCounter),2);
 AddItemEx(AResponse,'MutexIdleThreadCounter:',IntToStr(MutexIdleThreadCounter),2);
 AddItemEx(AResponse,'CriticalSectionDeadlockCounter:',IntToStr(CriticalSectionDeadlockCounter),2);
 AddItemEx(AResponse,'SemaphoreDeadlockCounter:',IntToStr(SemaphoreDeadlockCounter),2);
 AddItemEx(AResponse,'SynchronizerDeadlockCounter:',IntToStr(SynchronizerDeadlockCounter),2);
 AddItemEx(AResponse,'SynchronizerRecursionCounter:',IntToStr(SynchronizerRecursionCounter),2);
 AddItemEx(AResponse,'MessageslotDeadlockCounter:',IntToStr(MessageslotDeadlockCounter),2);
 AddItemEx(AResponse,'MailslotDeadlockCounter:',IntToStr(MailslotDeadlockCounter),2);
 AddItemEx(AResponse,'BufferDeadlockCounter:',IntToStr(BufferDeadlockCounter),2);
 AddItemEx(AResponse,'EventDeadlockCounter:',IntToStr(EventDeadlockCounter),2);
 AddBlank(AResponse);
 {$ENDIF}

 {$IFDEF SPIN_DEBUG}
 {Add Spin Debug}
 AddBold(AResponse,'Spin Debug','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'SpinLockEntry:',IntToStr(SpinLockEntry),2);
 AddItemEx(AResponse,'SpinUnlockEntry:',IntToStr(SpinUnlockEntry),2);
 AddBlank(AResponse);
 AddItemEx(AResponse,'SpinLockExit:',IntToStr(SpinLockExit),2);
 AddItemEx(AResponse,'SpinUnlockExit:',IntToStr(SpinUnlockExit),2);
 AddBlank(AResponse);
 AddItemEx(AResponse,'SpinUnlockNoLock:',IntToStr(SpinUnlockNoLock),2);
 AddItemEx(AResponse,'SpinUnlockNoOwner:',IntToStr(SpinUnlockNoOwner),2);
 AddBlank(AResponse);
 AddItemEx(AResponse,'SpinLockCounter:',IntToStr(SpinLockCounter),2);
 AddItemEx(AResponse,'SpinUnlockCounter:',IntToStr(SpinUnlockCounter),2);
 AddItemEx(AResponse,'SpinDestroyCounter:',IntToStr(SpinDestroyCounter),2);
 AddBlank(AResponse);
 AddItemEx(AResponse,'SpinLockIRQCounter:',IntToStr(SpinLockIRQCounter),2);
 AddItemEx(AResponse,'SpinUnlockIRQCounter:',IntToStr(SpinUnlockIRQCounter),2);
 AddBlank(AResponse);
 AddItemEx(AResponse,'SpinLockFIQCounter:',IntToStr(SpinLockFIQCounter),2);
 AddItemEx(AResponse,'SpinUnlockFIQCounter:',IntToStr(SpinUnlockFIQCounter),2);
 AddBlank(AResponse);
 AddItemEx(AResponse,'SpinLockIRQFIQCounter:',IntToStr(SpinLockIRQFIQCounter),2);
 AddItemEx(AResponse,'SpinUnlockIRQFIQCounter:',IntToStr(SpinUnlockIRQFIQCounter),2);
 AddBlank(AResponse);
 {$ENDIF}

 {$IFDEF MUTEX_DEBUG}
 {Add Mutex Debug}
 AddBold(AResponse,'Mutex Debug','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'MutexLockEntry:',IntToStr(MutexLockEntry),2);
 AddItemEx(AResponse,'MutexUnlockEntry:',IntToStr(MutexUnlockEntry),2);
 AddBlank(AResponse);
 AddItemEx(AResponse,'MutexLockExit:',IntToStr(MutexLockExit),2);
 AddItemEx(AResponse,'MutexUnlockExit:',IntToStr(MutexUnlockExit),2);
 AddBlank(AResponse);
 AddItemEx(AResponse,'MutexUnlockNoLock:',IntToStr(MutexUnlockNoLock),2);
 AddItemEx(AResponse,'MutexUnlockNoOwner:',IntToStr(MutexUnlockNoOwner),2);
 AddBlank(AResponse);
 AddItemEx(AResponse,'MutexLockCounter:',IntToStr(MutexLockCounter),2);
 AddItemEx(AResponse,'MutexUnlockCounter:',IntToStr(MutexUnlockCounter),2);
 AddItemEx(AResponse,'MutexDestroyCounter:',IntToStr(MutexDestroyCounter),2);
 AddBlank(AResponse);
 {$ENDIF}

 {$IFDEF CLOCK_DEBUG}
 {Add Clock Debug}
 AddBold(AResponse,'Clock Debug','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'ClockInterruptCounter:',IntToStr(ClockInterruptCounter),2);
 AddItemEx(AResponse,'ClockInterruptOffset:',IntToStr(ClockInterruptOffset),2);
 AddItemEx(AResponse,'ClockInterruptMinOffset:',IntToStr(ClockInterruptMinOffset),2);
 AddItemEx(AResponse,'ClockInterruptMaxOffset:',IntToStr(ClockInterruptMaxOffset),2);
 AddItemEx(AResponse,'ClockInterruptRollover:',IntToStr(ClockInterruptRollover),2);
 AddBlank(AResponse);
 {$ENDIF}
 
 {$IFDEF SCHEDULER_DEBUG}
 {Add Scheduler Debug}
 AddBold(AResponse,'Scheduler Debug','');
 AddBlank(AResponse);
 {SchedulerInterruptCounter/Offset/MinOffset/MaxOffset}
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'SchedulerInterruptCounter:',CPUIDToString(Count) + ': ' + IntToStr(SchedulerInterruptCounter[Count]),2);
    end
   else
    begin
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + IntToStr(SchedulerInterruptCounter[Count]),2);
    end;    
  end; 
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'SchedulerInterruptOffset:',CPUIDToString(Count) + ': ' + IntToStr(SchedulerInterruptOffset[Count]),2);
    end
   else
    begin
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + IntToStr(SchedulerInterruptOffset[Count]),2);
    end;    
  end; 
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'SchedulerInterruptMinOffset:',CPUIDToString(Count) + ': ' + IntToStr(SchedulerInterruptMinOffset[Count]),2);
    end
   else
    begin
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + IntToStr(SchedulerInterruptMinOffset[Count]),2);
    end;    
  end; 
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'SchedulerInterruptMaxOffset:',CPUIDToString(Count) + ': ' + IntToStr(SchedulerInterruptMaxOffset[Count]),2);
    end
   else
    begin
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + IntToStr(SchedulerInterruptMaxOffset[Count]),2);
    end;    
  end; 
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'SchedulerInterruptRollover:',CPUIDToString(Count) + ': ' + IntToStr(SchedulerInterruptRollover[Count]),2);
    end
   else
    begin
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + IntToStr(SchedulerInterruptRollover[Count]),2);
    end;    
  end; 
 AddBlank(AResponse);
 {SchedulerSelectEntry/Yield/Force/Default/Failure/CPU/Priority/Affinity}
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'SchedulerSelectEntry:',CPUIDToString(Count) + ': ' + IntToStr(SchedulerSelectEntry[Count]),2);
    end
   else
    begin
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + IntToStr(SchedulerSelectEntry[Count]),2);
    end;    
  end; 
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'SchedulerSelectYield:',CPUIDToString(Count) + ': ' + IntToStr(SchedulerSelectYield[Count]),2);
    end
   else
    begin
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + IntToStr(SchedulerSelectYield[Count]),2);
    end;    
  end; 
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'SchedulerSelectForce:',CPUIDToString(Count) + ': ' + IntToStr(SchedulerSelectForce[Count]),2);
    end
   else
    begin
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + IntToStr(SchedulerSelectForce[Count]),2);
    end;    
  end; 
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'SchedulerSelectNoMask:',CPUIDToString(Count) + ': ' + IntToStr(SchedulerSelectNoMask[Count]),2);
    end
   else
    begin
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + IntToStr(SchedulerSelectNoMask[Count]),2);
    end;    
  end; 
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'SchedulerSelectNormal:',CPUIDToString(Count) + ': ' + IntToStr(SchedulerSelectNormal[Count]),2);
    end
   else
    begin
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + IntToStr(SchedulerSelectNormal[Count]),2);
    end;    
  end; 
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'SchedulerSelectInvalid:',CPUIDToString(Count) + ': ' + IntToStr(SchedulerSelectInvalid[Count]),2);
    end
   else
    begin
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + IntToStr(SchedulerSelectInvalid[Count]),2);
    end;    
  end; 
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'SchedulerSelectFailure:',CPUIDToString(Count) + ': ' + IntToStr(SchedulerSelectFailure[Count]),2);
    end
   else
    begin
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + IntToStr(SchedulerSelectFailure[Count]),2);
    end;    
  end; 
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'SchedulerSelectNoReady:',CPUIDToString(Count) + ': ' + IntToStr(SchedulerSelectNoReady[Count]),2);
    end
   else
    begin
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + IntToStr(SchedulerSelectNoReady[Count]),2);
    end;    
  end; 
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'SchedulerSelectDefaulted:',CPUIDToString(Count) + ': ' + IntToStr(SchedulerSelectDefaulted[Count]),2);
    end
   else
    begin
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + IntToStr(SchedulerSelectDefaulted[Count]),2);
    end;    
  end; 
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'SchedulerStarvationReset:',CPUIDToString(Count) + ': ' + IntToStr(SchedulerStarvationReset[Count]),2);
    end
   else
    begin
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + IntToStr(SchedulerStarvationReset[Count]),2);
    end;    
  end; 
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'SchedulerStarvationDecrement:',CPUIDToString(Count) + ': ' + IntToStr(SchedulerStarvationDecrement[Count]),2);
    end
   else
    begin
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + IntToStr(SchedulerStarvationDecrement[Count]),2);
    end;    
  end; 
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'SchedulerSelectCPU:',CPUIDToString(Count) + ': ' + IntToStr(SchedulerSelectCPU[Count]),2);
    end
   else
    begin
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + IntToStr(SchedulerSelectCPU[Count]),2);
    end;    
  end; 
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'SchedulerSelectPriority:',CPUIDToString(Count) + ': ' + IntToStr(SchedulerSelectPriority[Count]),2);
    end
   else
    begin
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + IntToStr(SchedulerSelectPriority[Count]),2);
    end;    
  end; 
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'SchedulerSelectAffinity:',CPUIDToString(Count) + ': ' + IntToStr(SchedulerSelectAffinity[Count]),2);
    end
   else
    begin
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + IntToStr(SchedulerSelectAffinity[Count]),2);
    end;    
  end; 
 AddBlank(AResponse);
 {SchedulerSwitchEntry/Thread/Counter/Current/Invalid}
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'SchedulerSwitchEntry:',CPUIDToString(Count) + ': ' + IntToStr(SchedulerSwitchEntry[Count]),2);
    end
   else
    begin
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + IntToStr(SchedulerSwitchEntry[Count]),2);
    end;    
  end; 
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     Thread:=SchedulerSwitchThread[Count];
     AddItemEx(AResponse,'SchedulerSwitchThread:',CPUIDToString(Count) + ': ' + '0x' + IntToHex(Thread,8) + ' (' + ThreadGetName(Thread) + ')',2);
    end
   else
    begin
     Thread:=SchedulerSwitchThread[Count];
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + '0x' + IntToHex(Thread,8) + ' (' + ThreadGetName(Thread) + ')',2);
    end;    
  end; 
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'SchedulerSwitchCounter:',CPUIDToString(Count) + ': ' + IntToStr(SchedulerSwitchCounter[Count]),2);
    end
   else
    begin
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + IntToStr(SchedulerSwitchCounter[Count]),2);
    end;    
  end; 
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'SchedulerSwitchCurrent:',CPUIDToString(Count) + ': ' + IntToStr(SchedulerSwitchCurrent[Count]),2);
    end
   else
    begin
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + IntToStr(SchedulerSwitchCurrent[Count]),2);
    end;    
  end; 
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'SchedulerSwitchInvalid:',CPUIDToString(Count) + ': ' + IntToStr(SchedulerSwitchInvalid[Count]),2);
    end
   else
    begin
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + IntToStr(SchedulerSwitchInvalid[Count]),2);
    end;    
  end; 
 AddBlank(AResponse);
 {SchedulerRescheduleEntry/Thread/Counter/Current/Invalid}
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'SchedulerRescheduleEntry:',CPUIDToString(Count) + ': ' + IntToStr(SchedulerRescheduleEntry[Count]),2);
    end
   else
    begin
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + IntToStr(SchedulerRescheduleEntry[Count]),2);
    end;    
  end; 
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     Thread:=SchedulerRescheduleThread[Count];
     AddItemEx(AResponse,'SchedulerRescheduleThread:',CPUIDToString(Count) + ': ' + '0x' + IntToHex(Thread,8) + ' (' + ThreadGetName(Thread) + ')',2);
    end
   else
    begin
     Thread:=SchedulerRescheduleThread[Count];
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + '0x' + IntToHex(Thread,8) + ' (' + ThreadGetName(Thread) + ')',2);
    end;    
  end; 
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'SchedulerRescheduleCounter:',CPUIDToString(Count) + ': ' + IntToStr(SchedulerRescheduleCounter[Count]),2);
    end
   else
    begin
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + IntToStr(SchedulerRescheduleCounter[Count]),2);
    end;    
  end; 
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'SchedulerRescheduleCurrent:',CPUIDToString(Count) + ': ' + IntToStr(SchedulerRescheduleCurrent[Count]),2);
    end
   else
    begin
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + IntToStr(SchedulerRescheduleCurrent[Count]),2);
    end;    
  end; 
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'SchedulerRescheduleInvalid:',CPUIDToString(Count) + ': ' + IntToStr(SchedulerRescheduleInvalid[Count]),2);
    end
   else
    begin
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + IntToStr(SchedulerRescheduleInvalid[Count]),2);
    end;    
  end; 
 AddBlank(AResponse);
 {SchedulerMigrationCounter}
 AddItemEx(AResponse,'SchedulerMigrationCounter:',CPUIDToString(SCHEDULER_CPU_BOOT) + ': ' + IntToStr(SchedulerMigrationCounter),2);
 AddBlank(AResponse);
 {SchedulerTerminationCounter}
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'SchedulerTerminationCounter:',CPUIDToString(Count) + ': ' + IntToStr(SchedulerTerminationCounter[Count]),2);
    end
   else
    begin
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + IntToStr(SchedulerTerminationCounter[Count]),2);
    end;    
  end; 
 AddBlank(AResponse);
 {$ENDIF}

 {$IFDEF INTERRUPT_DEBUG}
 {Add Interrupt Debug}
 AddBold(AResponse,'Interrupt Debug','');
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'DispatchInterruptCounter:',CPUIDToString(Count) + ': ' + IntToStr(DispatchInterruptCounter[Count]),2);
    end
   else
    begin
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + IntToStr(DispatchInterruptCounter[Count]),2);
    end;    
  end; 
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'DispatchFastInterruptCounter:',CPUIDToString(Count) + ': ' + IntToStr(DispatchFastInterruptCounter[Count]),2);
    end
   else
    begin
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + IntToStr(DispatchFastInterruptCounter[Count]),2);
    end;    
  end; 
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'DispatchSoftwareInterruptCounter:',CPUIDToString(Count) + ': ' + IntToStr(DispatchSoftwareInterruptCounter[Count]),2);
    end
   else
    begin
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + IntToStr(DispatchSoftwareInterruptCounter[Count]),2);
    end;    
  end; 
 AddBlank(AResponse);
 {$ENDIF}
 
 {$IFDEF EXCEPTION_DEBUG}
 {Add Exception Debug}
 AddBold(AResponse,'Exception Debug','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'HardwareExceptionCounter:',IntToStr(HardwareExceptionCounter),2);
 AddItemEx(AResponse,'UnhandledExceptionCounter:',IntToStr(UnhandledExceptionCounter),2);
 AddBlank(AResponse);
 AddItemEx(AResponse,'HardwareExceptionAddress:','0x' + IntToHex(HardwareExceptionAddress,8),2);
 AddBlank(AResponse);
 {$ENDIF}
 
 {Add Footer}
 AddFooter(AResponse); 
 
 {Return Result}
 Result:=True;
end;
{$ENDIF}
{==============================================================================}
{==============================================================================}
{TWebStatusCustom}
constructor TWebStatusCustom.Create(const AName,APath:String;AColumns:LongWord);
begin
 {}
 FCaption:=AName; {Must be before create for register}
 inherited Create(WebStatusMain);
 Name:=APath;
 FColumns:=AColumns;
 
 if FMain <> nil then Name:=FMain.Name + Name;
end; 

{==============================================================================}

function TWebStatusCustom.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;

 {Add Header}
 if FColumns = 2 then
  begin
   AddHeader(AResponse,GetTitle,Self); 
  end
 else
  begin
   AddHeaderEx(AResponse,GetTitle,Self,FColumns); 
  end;

 {Add Content}
 Result:=DoContent(AHost,ARequest,AResponse);
 
 {Add Footer}
 if FColumns = 2 then
  begin
   AddFooter(AResponse); 
  end
 else
  begin
   AddFooterEx(AResponse,FColumns); 
  end;  
end;

{==============================================================================}

function TWebStatusCustom.DoContent(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; 
begin
 {}
 if Assigned(FOnContent) then
  begin
   Result:=FOnContent(AHost,ARequest,AResponse);
  end
 else
  begin 
   Result:=True;
  end; 
end;

{==============================================================================}
{==============================================================================}
{Initialization Functions}

{==============================================================================}
{==============================================================================}
{Web Status Functions}
function WebStatusRegister(AListener:THTTPListener;const AHost,AURL:String;ARedirect:Boolean):Boolean;
var
 WorkInt:LongWord;
begin
 {}
 Result:=False;
 
 {Check Listener}
 if AListener = nil then Exit;
 
 {Register Main Page}
 WebStatusMain:=TWebStatusMain.Create;
 if Length(AURL) <> 0 then WebStatusMain.Name:=AURL;
 AListener.RegisterDocument(AHost,WebStatusMain);
 
 {Register Platform Page}
 WebStatusPlatform:=TWebStatusPlatform.Create(WebStatusMain);
 AListener.RegisterDocument(AHost,WebStatusPlatform);
 
 {Register Memory Page}
 WebStatusMemory:=TWebStatusMemory.Create(WebStatusMain);
 AListener.RegisterDocument(AHost,WebStatusMemory);

 {Register Heap Page}
 WebStatusHeap:=TWebStatusHeap.Create(WebStatusMain);
 AListener.RegisterDocument(AHost,WebStatusHeap);
 
 {Register CPU Page}
 WebStatusCPU:=TWebStatusCPU.Create(WebStatusMain);
 AListener.RegisterDocument(AHost,WebStatusCPU);

 {Register FPU Page}
 WebStatusFPU:=TWebStatusFPU.Create(WebStatusMain);
 AListener.RegisterDocument(AHost,WebStatusFPU);

 {Register GPU Page}
 WebStatusGPU:=TWebStatusGPU.Create(WebStatusMain);
 AListener.RegisterDocument(AHost,WebStatusGPU);

 {Register RTL Page}
 WebStatusRTL:=TWebStatusRTL.Create(WebStatusMain);
 AListener.RegisterDocument(AHost,WebStatusRTL);

 {Register Clock Page}
 WebStatusClock:=TWebStatusClock.Create(WebStatusMain);
 AListener.RegisterDocument(AHost,WebStatusClock);

 {Register Locale Page}
 WebStatusLocale:=TWebStatusLocale.Create(WebStatusMain);
 AListener.RegisterDocument(AHost,WebStatusLocale);
 
 {Register Threading Page}
 WebStatusThreading:=TWebStatusThreading.Create(WebStatusMain);
 AListener.RegisterDocument(AHost,WebStatusThreading);

 {Register ThreadList Page}
 WebStatusThreadList:=TWebStatusThreadList.Create(WebStatusMain);
 AListener.RegisterDocument(AHost,WebStatusThreadList);
 
 {Register Scheduler Page}
 WebStatusScheduler:=TWebStatusScheduler.Create(WebStatusMain);
 AListener.RegisterDocument(AHost,WebStatusScheduler);

 {Register Devices Page}
 WebStatusDevices:=TWebStatusDevices.Create(WebStatusMain);
 AListener.RegisterDocument(AHost,WebStatusDevices);

 {Register Drivers Page}
 WebStatusDrivers:=TWebStatusDrivers.Create(WebStatusMain);
 AListener.RegisterDocument(AHost,WebStatusDrivers);
 
 {Register USB Page}
 WebStatusUSB:=TWebStatusUSB.Create(WebStatusMain);
 AListener.RegisterDocument(AHost,WebStatusUSB);

 {Register MMC Page}
 WebStatusMMC:=TWebStatusMMC.Create(WebStatusMain);
 AListener.RegisterDocument(AHost,WebStatusMMC);

 {Register Network Page}
 WebStatusNetwork:=TWebStatusNetwork.Create(WebStatusMain);
 AListener.RegisterDocument(AHost,WebStatusNetwork);

 {Register Storage Page}
 WebStatusStorage:=TWebStatusStorage.Create(WebStatusMain);
 AListener.RegisterDocument(AHost,WebStatusStorage);

 {Register Filesystem Page}
 WebStatusFilesystem:=TWebStatusFilesystem.Create(WebStatusMain);
 AListener.RegisterDocument(AHost,WebStatusFilesystem);

 {Register Cache Page}
 WebStatusCache:=TWebStatusCache.Create(WebStatusMain);
 AListener.RegisterDocument(AHost,WebStatusCache);
 
 {Register Keyboard Page}
 WebStatusKeyboard:=TWebStatusKeyboard.Create(WebStatusMain);
 AListener.RegisterDocument(AHost,WebStatusKeyboard);

 {Register Mouse Page}
 WebStatusMouse:=TWebStatusMouse.Create(WebStatusMain);
 AListener.RegisterDocument(AHost,WebStatusMouse);

 {Register Framebuffer Page}
 WebStatusFramebuffer:=TWebStatusFramebuffer.Create(WebStatusMain);
 AListener.RegisterDocument(AHost,WebStatusFramebuffer);

 {Register Environment Page}
 WebStatusEnvironment:=TWebStatusEnvironment.Create(WebStatusMain);
 AListener.RegisterDocument(AHost,WebStatusEnvironment);

 {Register PageTables Page}
 WebStatusPageTables:=TWebStatusPageTables.Create(WebStatusMain);
 AListener.RegisterDocument(AHost,WebStatusPageTables);
 
 {Register Configuration Page}
 WebStatusConfiguration:=TWebStatusConfiguration.Create(WebStatusMain);
 AListener.RegisterDocument(AHost,WebStatusConfiguration);

 {$IF DEFINED(LOCK_DEBUG) or DEFINED(SPIN_DEBUG) or DEFINED(MUTEX_DEBUG) or DEFINED(CLOCK_DEBUG) or DEFINED(SCHEDULER_DEBUG) or DEFINED(INTERRUPT_DEBUG)}
 {Register Debug Page}
 WebStatusDebug:=TWebStatusDebug.Create(WebStatusMain);
 AListener.RegisterDocument(AHost,WebStatusDebug);
 {$ENDIF}
 
 {Register Redirect Page}
 WebStatusRedirect:=nil;
 if ARedirect then
  begin
   WebStatusRedirect:=THTTPRedirect.Create;
   WebStatusRedirect.Name:='/';
   WebStatusRedirect.Location:=WebStatusMain.Name;
   AListener.RegisterDocument(AHost,WebStatusRedirect);
  end;
  
 {Check Environment Variables}
 {WEBSTATUS_HEAP_FREE_COUNT}
 WorkInt:=StrToIntDef(SysUtils.GetEnvironmentVariable('WEBSTATUS_HEAP_FREE_COUNT'),0);
 if WorkInt > 0 then WEBSTATUS_HEAP_FREE_COUNT:=WorkInt;
  
 {WEBSTATUS_HEAP_USED_COUNT}
 WorkInt:=StrToIntDef(SysUtils.GetEnvironmentVariable('WEBSTATUS_HEAP_USED_COUNT'),0);
 if WorkInt > 0 then WEBSTATUS_HEAP_USED_COUNT:=WorkInt;
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function WebStatusDeregister(AListener:THTTPListener;const AHost:String):Boolean;
begin
 {}
 Result:=False;
 
 {Check Listener}
 if AListener = nil then Exit;
 
 {Deregister Redirect Page}
 if WebStatusRedirect <> nil then
  begin
   AListener.DeregisterDocument(AHost,WebStatusRedirect);
   WebStatusRedirect.Free;
  end;
  
 {$IF DEFINED(LOCK_DEBUG) or DEFINED(SPIN_DEBUG) or DEFINED(MUTEX_DEBUG) or DEFINED(CLOCK_DEBUG) or DEFINED(SCHEDULER_DEBUG) or DEFINED(INTERRUPT_DEBUG)}
 {Deregister Debug Page}
 AListener.DeregisterDocument(AHost,WebStatusDebug);
 WebStatusDebug.Free;
 {$ENDIF}
 
 {Deregister Configuration Page}
 AListener.DeregisterDocument(AHost,WebStatusConfiguration);
 WebStatusConfiguration.Free;
  
 {Deregister PageTables Page}
 AListener.DeregisterDocument(AHost,WebStatusPageTables);
 WebStatusPageTables.Free;
  
 {Deregister Environment Page}
 AListener.DeregisterDocument(AHost,WebStatusEnvironment);
 WebStatusEnvironment.Free;
  
 {Deregister Framebuffer Page}
 AListener.DeregisterDocument(AHost,WebStatusFramebuffer);
 WebStatusFramebuffer.Free;
  
 {Deregister Mouse Page}
 AListener.DeregisterDocument(AHost,WebStatusMouse);
 WebStatusMouse.Free;
 
 {Deregister Keyboard Page}
 AListener.DeregisterDocument(AHost,WebStatusKeyboard);
 WebStatusKeyboard.Free;

 {Deregister Cache Page}
 AListener.DeregisterDocument(AHost,WebStatusCache);
 WebStatusCache.Free;
 
 {Deregister Filesystem Page}
 AListener.DeregisterDocument(AHost,WebStatusFilesystem);
 WebStatusFilesystem.Free;
  
 {Deregister Storage Page}
 AListener.DeregisterDocument(AHost,WebStatusStorage);
 WebStatusStorage.Free;

 {Deregister Network Page}
 AListener.DeregisterDocument(AHost,WebStatusNetwork);
 WebStatusNetwork.Free;

 {Deregister MMC Page}
 AListener.DeregisterDocument(AHost,WebStatusMMC);
 WebStatusMMC.Free;

 {Deregister USB Page}
 AListener.DeregisterDocument(AHost,WebStatusUSB);
 WebStatusUSB.Free;

 {Deregister Drivers Page}
 AListener.DeregisterDocument(AHost,WebStatusDrivers);
 WebStatusDrivers.Free;
 
 {Deregister Devices Page}
 AListener.DeregisterDocument(AHost,WebStatusDevices);
 WebStatusDevices.Free;

 {Deregister Scheduler Page}
 AListener.DeregisterDocument(AHost,WebStatusScheduler);
 WebStatusScheduler.Free;

 {Deregister ThreadList Page}
 AListener.DeregisterDocument(AHost,WebStatusThreadList);
 WebStatusThreadList.Free;
 
 {Deregister Threading Page}
 AListener.DeregisterDocument(AHost,WebStatusThreading);
 WebStatusThreading.Free;

 {Deregister Locale Page}
 AListener.DeregisterDocument(AHost,WebStatusLocale);
 WebStatusLocale.Free;
 
 {Deregister Clock Page}
 AListener.DeregisterDocument(AHost,WebStatusClock);
 WebStatusClock.Free;
 
 {Deregister RTL Page}
 AListener.DeregisterDocument(AHost,WebStatusRTL);
 WebStatusRTL.Free;
 
 {Deregister GPU Page}
 AListener.DeregisterDocument(AHost,WebStatusGPU);
 WebStatusGPU.Free;

 {Deregister FPU Page}
 AListener.DeregisterDocument(AHost,WebStatusFPU);
 WebStatusFPU.Free;

 {Deregister CPU Page}
 AListener.DeregisterDocument(AHost,WebStatusCPU);
 WebStatusCPU.Free;

 {Deregister Heap Page}
 AListener.DeregisterDocument(AHost,WebStatusHeap);
 WebStatusHeap.Free;
 
 {Deregister Memory Page}
 AListener.DeregisterDocument(AHost,WebStatusMemory);
 WebStatusMemory.Free;

 {Deregister Platform Page}
 AListener.DeregisterDocument(AHost,WebStatusPlatform);
 WebStatusPlatform.Free;
 
 {Deregister Main Page}
 AListener.DeregisterDocument(AHost,WebStatusMain);
 WebStatusMain.Free;

 {Return Result}
 Result:=True;
end;
 
{==============================================================================}
{==============================================================================}
{Web Status Helper Functions}
function WebStatusDeviceEnumerate(Device:PDevice;Data:Pointer):LongWord;
var
 Document:TWebStatusSub;
 Response:THTTPServerResponse;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;
 
 {Check Data}
 if Data = nil then Exit;
 
 {Get Document}
 Document:=PWebStatusData(Data).Document;
 if Document = nil then Exit;
 
 {Get Response}
 Response:=PWebStatusData(Data).Response;
 if Response = nil then Exit;
 
 {Add Device}
 Document.AddItem4Column(Response,IntToStr(Device.DeviceId),DeviceGetName(Device),DeviceClassToString(Device.DeviceClass),DeviceBusToString(Device.DeviceBus));
 
 Result:=ERROR_SUCCESS;
end;
 
{==============================================================================}
 
function WebStatusDriverEnumerate(Driver:PDriver;Data:Pointer):LongWord;
var
 Document:TWebStatusSub;
 Response:THTTPServerResponse;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Driver}
 if Driver = nil then Exit;
 
 {Check Data}
 if Data = nil then Exit;
 
 {Get Document}
 Document:=PWebStatusData(Data).Document;
 if Document = nil then Exit;
 
 {Get Response}
 Response:=PWebStatusData(Data).Response;
 if Response = nil then Exit;
 
 {Add Driver}
 Document.AddItem4Column(Response,IntToStr(Driver.DriverId),DriverGetName(Driver),DriverClassToString(Driver.DriverClass),DriverStateToString(Driver.DriverState));
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function WebStatusUSBDeviceEnumerate(Device:PUSBDevice;Data:Pointer):LongWord;
var
 Document:TWebStatusSub;
 Response:THTTPServerResponse;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;
 
 {Check Data}
 if Data = nil then Exit;
 
 {Get Document}
 Document:=PWebStatusData(Data).Document;
 if Document = nil then Exit;
 
 {Get Response}
 Response:=PWebStatusData(Data).Response;
 if Response = nil then Exit;
 
 {Add Device}
 Document.AddItem4Column(Response,IntToStr(Device.USBId),DeviceGetName(@Device.Device),USBClassCodeToString(Device.Descriptor.bDeviceClass),USBDeviceStatusToString(Device.USBStatus));
 
 Result:=ERROR_SUCCESS;
end;
 
{==============================================================================}

function WebStatusUSBHostEnumerate(Host:PUSBHost;Data:Pointer):LongWord;
var
 Document:TWebStatusSub;
 Response:THTTPServerResponse;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Host}
 if Host = nil then Exit;
 
 {Check Data}
 if Data = nil then Exit;
 
 {Get Document}
 Document:=PWebStatusData(Data).Document;
 if Document = nil then Exit;
 
 {Get Response}
 Response:=PWebStatusData(Data).Response;
 if Response = nil then Exit;
 
 {Add Host}
 Document.AddItem4Column(Response,IntToStr(Host.HostId),DeviceGetName(@Host.Device),USBHostStateToString(Host.HostState),USBHostTypeToString(Host.Device.DeviceType));
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function WebStatusUSBDriverEnumerate(Driver:PUSBDriver;Data:Pointer):LongWord;
var
 Document:TWebStatusSub;
 Response:THTTPServerResponse;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Driver}
 if Driver = nil then Exit;
 
 {Check Data}
 if Data = nil then Exit;
 
 {Get Document}
 Document:=PWebStatusData(Data).Document;
 if Document = nil then Exit;
 
 {Get Response}
 Response:=PWebStatusData(Data).Response;
 if Response = nil then Exit;
 
 {Add Driver}
 Document.AddItem4Column(Response,IntToStr(Driver.Driver.DriverId),DriverGetName(@Driver.Driver),DriverStateToString(Driver.Driver.DriverState),'');
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function WebStatusMMCEnumerate(MMC:PMMCDevice;Data:Pointer):LongWord;
var
 Document:TWebStatusSub;
 Response:THTTPServerResponse;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check MMC}
 if MMC = nil then Exit;
 
 {Check Data}
 if Data = nil then Exit;
 
 {Get Document}
 Document:=PWebStatusData(Data).Document;
 if Document = nil then Exit;
 
 {Get Response}
 Response:=PWebStatusData(Data).Response;
 if Response = nil then Exit;
 
 {Add MMC}
 Document.AddItem4Column(Response,IntToStr(MMC.MMCId),DeviceGetName(@MMC.Device),MMCDeviceStateToString(MMC.MMCState),MMCDeviceTypeToString(MMC.Device.DeviceType));
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function WebStatusSDHCIEnumerate(SDHCI:PSDHCIHost;Data:Pointer):LongWord;
var
 Document:TWebStatusSub;
 Response:THTTPServerResponse;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check SDHCI}
 if SDHCI = nil then Exit;
 
 {Check Data}
 if Data = nil then Exit;
 
 {Get Document}
 Document:=PWebStatusData(Data).Document;
 if Document = nil then Exit;
 
 {Get Response}
 Response:=PWebStatusData(Data).Response;
 if Response = nil then Exit;
 
 {Add SDHCI}
 Document.AddItem4Column(Response,IntToStr(SDHCI.SDHCIId),DeviceGetName(@SDHCI.Device),SDHCIDeviceStateToString(SDHCI.SDHCIState),SDHCIDeviceTypeToString(SDHCI.Device.DeviceType));
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function WebStatusNetworkEnumerate(Network:PNetworkDevice;Data:Pointer):LongWord;
var
 Document:TWebStatusSub;
 Response:THTTPServerResponse;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Network}
 if Network = nil then Exit;
 
 {Check Data}
 if Data = nil then Exit;
 
 {Get Document}
 Document:=PWebStatusData(Data).Document;
 if Document = nil then Exit;
 
 {Get Response}
 Response:=PWebStatusData(Data).Response;
 if Response = nil then Exit;
 
 {Add Network}
 Document.AddItem5Column(Response,IntToStr(Network.NetworkId),DeviceGetName(@Network.Device),NetworkDeviceStateToString(Network.NetworkState),NetworkDeviceStatusToString(Network.NetworkStatus),NetworkDeviceTypeToString(Network.Device.DeviceType));
 
 Result:=ERROR_SUCCESS;
end;
 
{==============================================================================}

function WebStatusStorageEnumerate(Storage:PStorageDevice;Data:Pointer):LongWord;
var
 Document:TWebStatusSub;
 Response:THTTPServerResponse;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Storage}
 if Storage = nil then Exit;
 
 {Check Data}
 if Data = nil then Exit;
 
 {Get Document}
 Document:=PWebStatusData(Data).Document;
 if Document = nil then Exit;
 
 {Get Response}
 Response:=PWebStatusData(Data).Response;
 if Response = nil then Exit;
 
 {Add Storage}
 Document.AddItem4Column(Response,IntToStr(Storage.StorageId),DeviceGetName(@Storage.Device),StorageDeviceStateToString(Storage.StorageState),StorageDeviceTypeToString(Storage.Device.DeviceType));
 
 Result:=ERROR_SUCCESS;
end;
 
{==============================================================================}

function WebStatusMouseEnumerate(Mouse:PMouseDevice;Data:Pointer):LongWord;
var
 Document:TWebStatusSub;
 Response:THTTPServerResponse;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Mouse}
 if Mouse = nil then Exit;
 
 {Check Data}
 if Data = nil then Exit;
 
 {Get Document}
 Document:=PWebStatusData(Data).Document;
 if Document = nil then Exit;
 
 {Get Response}
 Response:=PWebStatusData(Data).Response;
 if Response = nil then Exit;
 
 {Add Mouse}
 Document.AddItem4Column(Response,IntToStr(Mouse.MouseId),DeviceGetName(@Mouse.Device),MouseDeviceStateToString(Mouse.MouseState),MouseDeviceTypeToString(Mouse.Device.DeviceType));
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function WebStatusKeyboardEnumerate(Keyboard:PKeyboardDevice;Data:Pointer):LongWord;
var
 Document:TWebStatusSub;
 Response:THTTPServerResponse;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Keyboard}
 if Keyboard = nil then Exit;
 
 {Check Data}
 if Data = nil then Exit;
 
 {Get Document}
 Document:=PWebStatusData(Data).Document;
 if Document = nil then Exit;
 
 {Get Response}
 Response:=PWebStatusData(Data).Response;
 if Response = nil then Exit;
 
 {Add Keyboard}
 Document.AddItem4Column(Response,IntToStr(Keyboard.KeyboardId),DeviceGetName(@Keyboard.Device),KeyboardDeviceStateToString(Keyboard.KeyboardState),KeyboardDeviceTypeToString(Keyboard.Device.DeviceType));
 
 Result:=ERROR_SUCCESS;
end;
 
{==============================================================================}
{==============================================================================}

end.
 