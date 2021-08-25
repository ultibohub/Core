{
Ultibo Web Status unit.

Copyright (C) 2021 - SoftOz Pty Ltd.

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

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,{$IFDEF CPUARM}PlatformARM,{$ENDIF}{$IFDEF CPUAARCH64}PlatformAARCH64,{$ENDIF}Threads,SysUtils,Classes,Ultibo,UltiboClasses,UltiboUtils,Winsock2,HTTP,
     HeapManager,DeviceTree,Devices,USB,PCI,MMC,Network,Transport,Protocol,Storage,FileSystem,Keyboard,Keymap,Mouse,Touch,Console,Framebuffer,Font,Logging,
     Timezone,Locale,Unicode,Iphlpapi,GPIO,UART,Serial,I2C,SPI,PWM,DMA,RTC;

//To Do //Look for:

//--
     
{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {Web Status specific constants}
 RtlMaxUnits = 1024; {See maxunits in system.inc}
 
 DeviceTreeMaxColumns = 140;
 DeviceTreeColumnOffset = 20;
 
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
 {Web Status specific classes}
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
  
  function NormalizedDateTimeToStr(const DateTime:TDateTime):String;
 protected
  {Internal Variables}

  {Internal Methods}
  function MakeBold(const AName:String):String;
  function MakeLink(const AName,ALink:String):String;
  
  function AddBlank(AResponse:THTTPServerResponse):Boolean;
  function AddBlankEx(AResponse:THTTPServerResponse;AColumns:LongWord):Boolean;

  function AddItem(AResponse:THTTPServerResponse;const AName,AValue:String):Boolean;
  function AddItemEx(AResponse:THTTPServerResponse;const AName,AValue:String;AIndent:LongWord):Boolean;
  
  function AddItemSpan(AResponse:THTTPServerResponse;const AValue:String;AColumns:LongWord;ABreak:Boolean = True):Boolean;
  function AddItemSpanEx(AResponse:THTTPServerResponse;const AValue:String;AColumns,AIndent:LongWord;ABreak:Boolean = True):Boolean;
  
  function AddItem3Column(AResponse:THTTPServerResponse;const AName,AValue1,AValue2:String):Boolean;
  function AddItem4Column(AResponse:THTTPServerResponse;const AName,AValue1,AValue2,AValue3:String):Boolean;
  function AddItem5Column(AResponse:THTTPServerResponse;const AName,AValue1,AValue2,AValue3,AValue4:String):Boolean;
  
  function AddBold(AResponse:THTTPServerResponse;const AName,AValue:String):Boolean;
  function AddBoldEx(AResponse:THTTPServerResponse;const AName,AValue:String;AIndent:LongWord):Boolean;
  
  function AddBold3Column(AResponse:THTTPServerResponse;const AName,AValue1,AValue2:String):Boolean;
  function AddBold4Column(AResponse:THTTPServerResponse;const AName,AValue1,AValue2,AValue3:String):Boolean;
  function AddBold5Column(AResponse:THTTPServerResponse;const AName,AValue1,AValue2,AValue3,AValue4:String):Boolean;
  
  function AddHeader(AResponse:THTTPServerResponse;const ATitle:String;ASub:TWebStatusSub):Boolean;
  function AddHeaderEx(AResponse:THTTPServerResponse;const ATitle,ACaption:String;ASub:TWebStatusSub;AColumns:LongWord):Boolean;
  
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
  
  function NormalizedDateTimeToStr(const DateTime:TDateTime):String;
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

  function AddItemSpan(AResponse:THTTPServerResponse;const AValue:String;AColumns:LongWord;ABreak:Boolean = True):Boolean;
  function AddItemSpanEx(AResponse:THTTPServerResponse;const AValue:String;AColumns,AIndent:LongWord;ABreak:Boolean = True):Boolean;

  function AddItem3Column(AResponse:THTTPServerResponse;const AName,AValue1,AValue2:String):Boolean;
  function AddItem4Column(AResponse:THTTPServerResponse;const AName,AValue1,AValue2,AValue3:String):Boolean;
  function AddItem5Column(AResponse:THTTPServerResponse;const AName,AValue1,AValue2,AValue3,AValue4:String):Boolean;
  
  function AddBold(AResponse:THTTPServerResponse;const AName,AValue:String):Boolean;
  function AddBoldEx(AResponse:THTTPServerResponse;const AName,AValue:String;AIndent:LongWord):Boolean;
  
  function AddBold3Column(AResponse:THTTPServerResponse;const AName,AValue1,AValue2:String):Boolean;
  function AddBold4Column(AResponse:THTTPServerResponse;const AName,AValue1,AValue2,AValue3:String):Boolean;
  function AddBold5Column(AResponse:THTTPServerResponse;const AName,AValue1,AValue2,AValue3,AValue4:String):Boolean;
  
  function AddHeader(AResponse:THTTPServerResponse;const ATitle:String;ASub:TWebStatusSub):Boolean;
  function AddHeaderEx(AResponse:THTTPServerResponse;const ATitle,ACaption:String;ASub:TWebStatusSub;AColumns:LongWord):Boolean;
  
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

  {Internal Methods}

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
  
  {Internal Methods}
  
  function FlagsToFlagNames(AFlags:LongWord):TStringList;
  function AffinityToAffinityNames(AAffinity:LongWord):TStringList;
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
  
  {Internal Methods}
  function MMCFlagsToFlagNames(AFlags:LongWord):TStringList;
  function SDHCIFlagsToFlagNames(AFlags:LongWord):TStringList;

  function USBFlagsToFlagNames(AFlags:LongWord):TStringList;
  function USBHostFlagsToFlagNames(AFlags:LongWord):TStringList;

  function PCIFlagsToFlagNames(AFlags:LongWord):TStringList;
  function PCIHostFlagsToFlagNames(AFlags:LongWord):TStringList;

  function DMAFlagsToFlagNames(AFlags:LongWord):TStringList;
  function I2CFlagsToFlagNames(AFlags:LongWord):TStringList;
  function SPIFlagsToFlagNames(AFlags:LongWord):TStringList;
  function PWMFlagsToFlagNames(AFlags:LongWord):TStringList;
  function RTCFlagsToFlagNames(AFlags:LongWord):TStringList;
  function GPIOFlagsToFlagNames(AFlags:LongWord):TStringList;
  function UARTFlagsToFlagNames(AFlags:LongWord):TStringList;
  function ClockFlagsToFlagNames(AFlags:LongWord):TStringList;
  function MouseFlagsToFlagNames(AFlags:LongWord):TStringList;
  function TouchFlagsToFlagNames(AFlags:LongWord):TStringList;
  function TimerFlagsToFlagNames(AFlags:LongWord):TStringList;
  function SerialFlagsToFlagNames(AFlags:LongWord):TStringList;
  function RandomFlagsToFlagNames(AFlags:LongWord):TStringList;
  function MailboxFlagsToFlagNames(AFlags:LongWord):TStringList;
  function WatchdogFlagsToFlagNames(AFlags:LongWord):TStringList;
  function NetworkFlagsToFlagNames(AFlags:LongWord):TStringList; 
  function LoggingFlagsToFlagNames(AFlags:LongWord):TStringList;
  function StorageFlagsToFlagNames(AFlags:LongWord):TStringList; 
  function KeyboardFlagsToFlagNames(AFlags:LongWord):TStringList; 
  function ConsoleFlagsToFlagNames(AFlags:LongWord):TStringList;
  function FramebufferFlagsToFlagNames(AFlags:LongWord):TStringList;
  function ConsoleWindowFlagsToFlagNames(AFlags:LongWord):TStringList;
  
  function UARTStatusToStatusNames(AStatus:LongWord):TStringList;
  function SerialStatusToStatusNames(AStatus:LongWord):TStringList;
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
 
 TWebStatusHandles = class(TWebStatusSub)
 public
  {}
  constructor Create(AMain:TWebStatusMain);
 private
  {Internal Variables}

  {Internal Methods}
  
  function FlagsToFlagNames(AFlags:LongWord):TStringList;
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
  function USBFlagsToFlagNames(AFlags:LongWord):TStringList;
  function USBHostFlagsToFlagNames(AFlags:LongWord):TStringList;
  
 protected
  {Internal Variables}
  
  {Internal Methods}
 
  function DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
 public
  {Public Properties}
 
  {Public Methods}
  
 end;

 TWebStatusPCI = class(TWebStatusSub)
 public
  {}
  constructor Create(AMain:TWebStatusMain);
 private
  {Internal Variables}
  function PCIFlagsToFlagNames(AFlags:LongWord):TStringList;
  function PCIHostFlagsToFlagNames(AFlags:LongWord):TStringList;
  
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
  function MMCFlagsToFlagNames(AFlags:LongWord):TStringList;
  function SDHCIFlagsToFlagNames(AFlags:LongWord):TStringList;
  
  function MMCVoltagesToNames(AVoltages:LongWord):TStringList;
  function MMCCapabilitiesToNames(ACapabilities:LongWord):TStringList;
  function MMCCapabilities2ToNames(ACapabilities2:LongWord):TStringList;

  function SDHCIQuirksToNames(AQuirks:LongWord):TStringList;
  function SDHCIQuirks2ToNames(AQuirks2:LongWord):TStringList;
  function SDHCIInterruptsToNames(AInterrupts:LongWord):TStringList;
  function SDHCIVoltagesToNames(AVoltages:LongWord):TStringList;
  function SDHCICapabilitiesToNames(ACapabilities:LongWord):TStringList;
  function SDHCICapabilities2ToNames(ACapabilities2:LongWord):TStringList;
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
  
  {Internal Methods}
  
  function NetworkFlagsToFlagNames(AFlags:LongWord):TStringList; 
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

 TWebStatusTouch = class(TWebStatusSub)
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
  
  {Internal Methods}
  
  function FlagsToFlagNames(AFlags:LongWord):TStringList;
 protected
  {Internal Variables}
 
  {Internal Methods}
 
  function DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
 public
  {Public Properties}
 
  {Public Methods}
  
 end;

 TWebStatusVectorTables = class(TWebStatusSub)
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

 TWebStatusIRQFIQSWI = class(TWebStatusSub)
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

 TWebStatusGPIO = class(TWebStatusSub)
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

 TWebStatusDeviceTree = class(TWebStatusSub)
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
  Data:Pointer;
 end;
 
{==============================================================================}
var
 {Web Status specific variables}
 WEBSTATUS_FONT_NAME:String = 'Arial';
 WEBSTATUS_HEAP_FREE_COUNT:LongWord = 250;  {Maximum number of free heap blocks to display}
 WEBSTATUS_HEAP_USED_COUNT:LongWord = 250;  {Maximum number of used heap blocks to display}

{==============================================================================}
{Initialization Functions}

{==============================================================================}
{Web Status Functions}
function WebStatusRegister(AListener:THTTPListener;const AHost,AURL:String;ARedirect:Boolean):Boolean;
function WebStatusDeregister(AListener:THTTPListener;const AHost:String):Boolean;

{==============================================================================}
{Web Status Helper Functions}
procedure WebStatusDeviceTreeLogOutput(const AText:String;Data:Pointer);
function WebStatusDeviceEnumerate(Device:PDevice;Data:Pointer):LongWord;
function WebStatusDriverEnumerate(Driver:PDriver;Data:Pointer):LongWord;
function WebStatusHandleEnumerate(Handle:PHandleEntry;Data:Pointer):LongWord;
function WebStatusUSBDeviceEnumerate(Device:PUSBDevice;Data:Pointer):LongWord;
function WebStatusUSBHostEnumerate(Host:PUSBHost;Data:Pointer):LongWord;
function WebStatusUSBDriverEnumerate(Driver:PUSBDriver;Data:Pointer):LongWord;
procedure WebStatusUSBLogOutput(const AText:String;Data:Pointer); 
function WebStatusUSBLogDeviceCallback(Device:PUSBDevice;Data:Pointer):LongWord;
function WebStatusUSBLogTreeCallback(Device:PUSBDevice;Data:Pointer):LongWord;
function WebStatusPCIDeviceEnumerate(Device:PPCIDevice;Data:Pointer):LongWord;
function WebStatusPCIHostEnumerate(Host:PPCIHost;Data:Pointer):LongWord;
function WebStatusPCIDriverEnumerate(Driver:PPCIDriver;Data:Pointer):LongWord;
function WebStatusMMCEnumerate(MMC:PMMCDevice;Data:Pointer):LongWord;
function WebStatusSDHCIEnumerate(SDHCI:PSDHCIHost;Data:Pointer):LongWord;
function WebStatusSDIODriverEnumerate(Driver:PSDIODriver;Data:Pointer):LongWord;
function WebStatusNetworkEnumerate(Network:PNetworkDevice;Data:Pointer):LongWord;
function WebStatusStorageEnumerate(Storage:PStorageDevice;Data:Pointer):LongWord;
function WebStatusMouseEnumerate(Mouse:PMouseDevice;Data:Pointer):LongWord;
function WebStatusTouchEnumerate(Touch:PTouchDevice;Data:Pointer):LongWord;
function WebStatusKeyboardEnumerate(Keyboard:PKeyboardDevice;Data:Pointer):LongWord;

function WebStatusConsoleWindowEnumerate(Console:PConsoleDevice;Handle:TWindowHandle;Data:Pointer):LongWord;

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
 WebStatusHandles:TWebStatusHandles;
 WebStatusUSB:TWebStatusUSB;
 WebStatusPCI:TWebStatusPCI;
 WebStatusMMC:TWebStatusMMC;
 WebStatusNetwork:TWebStatusNetwork;
 WebStatusStorage:TWebStatusStorage;
 WebStatusFilesystem:TWebStatusFilesystem;
 WebStatusCache:TWebStatusCache;
 WebStatusKeyboard:TWebStatusKeyboard;
 WebStatusMouse:TWebStatusMouse;
 WebStatusTouch:TWebStatusTouch;
 WebStatusFramebuffer:TWebStatusFramebuffer;
 WebStatusEnvironment:TWebStatusEnvironment;
 WebStatusPageTables:TWebStatusPageTables;
 WebStatusVectorTables:TWebStatusVectorTables;
 WebStatusIRQFIQSWI:TWebStatusIRQFIQSWI;
 WebStatusGPIO:TWebStatusGPIO;
 WebStatusConfiguration:TWebStatusConfiguration;
 WebStatusDeviceTree:TWebStatusDeviceTree;
 {$IF DEFINED(LOCK_DEBUG) or DEFINED(SPIN_DEBUG) or DEFINED(MUTEX_DEBUG) or DEFINED(CLOCK_DEBUG) or DEFINED(SCHEDULER_DEBUG) or DEFINED(INTERRUPT_DEBUG) or DEFINED(EXCEPTION_DEBUG)}
 WebStatusDebug:TWebStatusDebug;
 {$ENDIF}
 
 WebStatusRedirect:THTTPRedirect;
 
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

function TWebStatusMain.NormalizedDateTimeToStr(const DateTime:TDateTime):String;
begin
 {}
 if DateTime = 0 then 
  begin
   Result:='N/A';
  end
 else
  begin
   Result:=DateTimeToStr(DateTime);
  end;  
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

function TWebStatusMain.AddItemSpan(AResponse:THTTPServerResponse;const AValue:String;AColumns:LongWord;ABreak:Boolean):Boolean;
var
 WorkBuffer:String;
begin
 {}
 Result:=False;
 
 {Check Response}
 if AResponse = nil then Exit;

 {Check Columns}
 if AColumns < 2 then AColumns:=2;
 if AColumns > 5 then AColumns:=5;

 {Check Break}
 WorkBuffer:=AValue;
 if ABreak then
  begin
   WorkBuffer:=WorkBuffer + '<br>';
  end;
  
 {Add Content}
 AddContent(AResponse,'               <tr>');
 AddContent(AResponse,'                 <td colspan="' + IntToStr(AColumns) + '" style="text-align: left; width: 100%;">' + WorkBuffer);
 AddContent(AResponse,'                 </td>');
 AddContent(AResponse,'               </tr>');
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TWebStatusMain.AddItemSpanEx(AResponse:THTTPServerResponse;const AValue:String;AColumns,AIndent:LongWord;ABreak:Boolean):Boolean;
var
 Count:LongWord;
 
 WorkBuffer:String;
begin
 {}
 Result:=False;
 
 {Check Response}
 if AResponse = nil then Exit;

 {Check Columns}
 if AColumns < 2 then AColumns:=2;
 if AColumns > 5 then AColumns:=5;

 {Check Indent}
 if AIndent < 1 then
  begin
   Result:=AddItemSpan(AResponse,AValue,AColumns,ABreak);
  end
 else
  begin 
   {Get Indent}
   WorkBuffer:='';
   for Count:=1 to AIndent do
    begin
     WorkBuffer:=WorkBuffer + '&nbsp;';
    end;
 
   {Check Break}
   WorkBuffer:=WorkBuffer + AValue;
   if ABreak then
    begin
     WorkBuffer:=WorkBuffer + '<br>';
    end;
 
   {Add Content}
   AddContent(AResponse,'               <tr>');
   AddContent(AResponse,'                 <td colspan="' + IntToStr(AColumns) + '" style="text-align: left; width: 100%;">' + WorkBuffer);
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
 Result:=AddHeaderEx(AResponse,ATitle,'',ASub,2);
end;

{==============================================================================}

function TWebStatusMain.AddHeaderEx(AResponse:THTTPServerResponse;const ATitle,ACaption:String;ASub:TWebStatusSub;AColumns:LongWord):Boolean;
var
 Title:String;
 Caption:String;
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
  
  {Check Title}
  Title:=ATitle;
  if Length(Title) = 0 then
   begin
    Title:=GetTitle;
   end; 
  
  {Check Caption}
  Caption:=ACaption;
  if Length(ACaption) = 0 then
   begin
    if ASub = nil then Caption:=GetCaption else Caption:=ASub.Caption;
   end; 
  
  {Add Content}
  AddContent(AResponse,'<html>');
  AddContent(AResponse,' <head>');
  AddContent(AResponse,'   <meta content="text/html; charset=ISO-8859-1" http-equiv="Content-Type">');
  AddContent(AResponse,'   <title>' + Title + '</title>');
  AddContent(AResponse,' </head>');
  AddContent(AResponse,' <body link=#4dad00 vlink=#4dad00 alink=#00cc00>');
  AddContent(AResponse,'   <table style=" text-align: left; width: 75%; height: 100%; margin-left: auto; margin-right: auto; font-family: ' + WEBSTATUS_FONT_NAME + ';" border="0" cellpadding="2" cellspacing="2">');
  AddContent(AResponse,'     <tbody>');
  AddContent(AResponse,'       <tr>');
  AddContent(AResponse,'         <td colspan="' + IntToStr(AColumns + 1) + '" rowspan="1" style=" text-align: center; vertical-align: middle; color: rgb(231, 231, 231); background-color: rgb(0, 187, 0); height: 65px;"><strong>' + Title + '</strong><br>');
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
    AddContent(AResponse,'                 <td style="text-align: center; background-color: rgb(192, 192, 192)"><span style="color: rgb(255, 255, 255);"><a href="' + Name + '">' + GetCaption + '</a></span><br>');
   end
  else
   begin
    AddContent(AResponse,'                 <td style="text-align: center; background-color: rgb(231, 231, 231)"><a href="' + Name + '">' + GetCaption + '</a><br>');
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
        AddContent(AResponse,'                 <td style="text-align: center; background-color: rgb(192, 192, 192)"><span style="color: rgb(255, 255, 255);"><a href="' + Sub.Name + '">' + Sub.Caption + '</a></span><br>');
       end
      else
       begin      
        AddContent(AResponse,'                 <td style="text-align: center; background-color: rgb(231, 231, 231)"><a href="' + Sub.Name + '">' + Sub.Caption + '</a><br>');
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
    AddContent(AResponse,'                 <td colspan="' + IntToStr(AColumns) + '" rowspan="1" style=" text-align: center;"><strong>' + Caption + '</strong><br>');
   end
  else
   begin
    AddContent(AResponse,'                 <td colspan="' + IntToStr(AColumns) + '" rowspan="1" style=" text-align: center;"><strong>' + Caption + '</strong><br>');
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
 WorkTemp:Double;
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
 AddItemEx(AResponse,'Daylight Date:',NormalizedDateTimeToStr(GetTimezoneDaylightDate),2);
 AddBlank(AResponse);
 AddItemEx(AResponse,'Standard Start:',GetTimezoneStandardStart,2);
 AddItemEx(AResponse,'Standard Date:',NormalizedDateTimeToStr(GetTimezoneStandardDate),2);

 //To Do //Locale
 
 //To Do //Codepage
 
 {Add Temperature}
 WorkTemp:=TemperatureGetCurrent(TEMPERATURE_ID_SOC);
 AddBlank(AResponse);
 AddItem(AResponse,'Temperature (SoC):',FloatToStr(WorkTemp / 1000) + ' degrees Celcius');
 
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

function TWebStatusSub.NormalizedDateTimeToStr(const DateTime:TDateTime):String;
begin
 {}
 Result:='';
 
 if FMain = nil then Exit;

 Result:=FMain.NormalizedDateTimeToStr(DateTime);
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

function TWebStatusSub.AddItemSpan(AResponse:THTTPServerResponse;const AValue:String;AColumns:LongWord;ABreak:Boolean):Boolean;
begin
 {}
 Result:=False;
 
 if FMain = nil then Exit;
 
 Result:=FMain.AddItemSpan(AResponse,AValue,AColumns,ABreak);
end;

{==============================================================================}

function TWebStatusSub.AddItemSpanEx(AResponse:THTTPServerResponse;const AValue:String;AColumns,AIndent:LongWord;ABreak:Boolean):Boolean;
begin
 {}
 Result:=False;
 
 if FMain = nil then Exit;
 
 Result:=FMain.AddItemSpanEx(AResponse,AValue,AColumns,AIndent,ABreak);
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

function TWebStatusSub.AddHeaderEx(AResponse:THTTPServerResponse;const ATitle,ACaption:String;ASub:TWebStatusSub;AColumns:LongWord):Boolean;
begin
 {}
 Result:=False;
 
 if FMain = nil then Exit;
 
 Result:=FMain.AddHeaderEx(AResponse,ATitle,ACaption,ASub,AColumns);
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
 Address:PtrUInt;
 MaxClock:LongWord;
 MaxPower:LongWord;
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

 {Add Chip Revision}
 AddBlank(AResponse);
 AddItem(AResponse,'Chip Revision:','0x' + IntToHex(ChipGetRevision,8));

 {Add Firmware Revision}
 AddBlank(AResponse);
 AddItem(AResponse,'Firmware Revision:','0x' + IntToHex(FirmwareGetRevision,8) + ' (' + IntToStr(FirmwareGetRevision) + ')');
 
 {Add Machine Type}
 AddBlank(AResponse);
 AddItem(AResponse,'Machine Type:',MachineTypeToString(MachineGetType));

 {Add Memory Base}
 AddBlank(AResponse);
 AddItem(AResponse,'Memory Base:','0x' + AddrToHex(MemoryGetBase));

 {Add Memory Size}
 AddItem(AResponse,'Memory Size:',IntToStr(MemoryGetSize));

 {Add Page Size}
 AddBlank(AResponse);
 AddItem(AResponse,'Page Size:',IntToStr(MemoryGetPageSize));
 if MemoryGetLargePageSize > 0 then AddItem(AResponse,'Large Page Size:',IntToStr(MemoryGetLargePageSize));

 {Add Section Size}
 AddBlank(AResponse);
 AddItem(AResponse,'Section Size:',IntToStr(MemoryGetSectionSize));
 if MemoryGetLargeSectionSize > 0 then AddItem(AResponse,'Large Section Size:',IntToStr(MemoryGetLargeSectionSize));

 {Check Board Type}
 MaxClock:=CLOCK_ID_SPI3;
 MaxPower:=POWER_ID_CCP2TX;
 case BoardGetType of
  BOARD_TYPE_RPI4B,BOARD_TYPE_RPI400,BOARD_TYPE_RPI_COMPUTE4:begin
    MaxClock:=CLOCK_ID_SPI9;
    MaxPower:=POWER_ID_SPI9;
   end; 
 end;

 {Add Power States}
 AddBlank(AResponse);
 AddItem(AResponse,'Power State','');
 for Count:=POWER_ID_MMC0 to MaxPower do
  begin
   AddItemEx(AResponse,PowerIDToString(Count) + ':',PowerStateToString(PowerGetState(Count)),3);
  end; 
 
 {Add Clock Rates}
 AddBlank(AResponse);
 AddItem(AResponse,'Clock Rate','');
 for Count:=CLOCK_ID_MMC0 to MaxClock do
  begin
   AddItemEx(AResponse,ClockIDToString(Count) + ':',IntToStr(ClockGetRate(Count)),3);
  end; 
 
 {Add Clock States}
 AddBlank(AResponse);
 AddItem(AResponse,'Clock State','');
 for Count:=CLOCK_ID_MMC0 to MaxClock do
  begin
   AddItemEx(AResponse,ClockIDToString(Count) + ':',ClockStateToString(ClockGetState(Count)),3);
  end; 

 {Add Clock Min/Max}
 AddBlank(AResponse);
 AddItem(AResponse,'Clock Min/Max Rate','');
 for Count:=CLOCK_ID_MMC0 to MaxClock do
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

 {Add Firmware Throttling}
 AddBlank(AResponse);
 AddItem(AResponse,'Firmware Throttling:','0x' + IntToHex(FirmwareGetThrottled,8));

 {Add DMA Channels}
 AddBlank(AResponse);
 AddItem(AResponse,'DMA Channels:','0x' + IntToHex(DMAGetChannels,8));

 {Add IO Base}
 AddBlank(AResponse);
 AddItem(AResponse,'IO Base:','0x' + AddrToHex(IO_BASE));
 
 {Add IO Alias}
 AddBlank(AResponse);
 AddItem(AResponse,'IO Alias:','0x' + AddrToHex(IO_ALIAS));

 {Add Bus Alias}
 AddBlank(AResponse);
 AddItem(AResponse,'Bus Alias:','0x' + AddrToHex(BUS_ALIAS));
 
 {Add Secure Boot}
 AddBlank(AResponse);
 AddItem(AResponse,'Secure Boot:',BooleanToString(SECURE_BOOT));

 {Add Emulator Mode}
 AddBlank(AResponse);
 AddItem(AResponse,'Emulator Mode:',BooleanToString(EMULATOR_MODE));

 {Add Startup Address}
 AddBlank(AResponse);
 AddItem(AResponse,'Startup Address:','0x' + AddrToHex(STARTUP_ADDRESS));
 
 {Add Peripheral Base}
 AddBlank(AResponse);
 AddItem(AResponse,'Peripheral Base:','0x' + AddrToHex(PeripheralGetBase));

 {Add Peripheral Size}
 AddItem(AResponse,'Peripheral Size:',IntToStr(PeripheralGetSize));

 {Add Local Peripheral Base}
 AddBlank(AResponse);
 AddItem(AResponse,'Local Peripheral Base:','0x' + AddrToHex(LocalPeripheralGetBase));

 {Add Local Peripheral Size}
 AddItem(AResponse,'Local Peripheral Size:',IntToStr(LocalPeripheralGetSize));
 
 {Add Page Table Levels}
 AddBlank(AResponse);
 AddItem(AResponse,'Page Table Levels:',IntToStr(PageTableGetLevels));

 {Check Levels}
 case PageTableGetLevels of
  2:begin
    {2 level page table}
    {Add Page Table Base}
    AddBlank(AResponse);
    AddItem(AResponse,'Level 1 Page Table Base:','0x' + AddrToHex(PageTableGetBase));
    
    {Add Page Table Size}
    AddItem(AResponse,'Level 1 Page Table Size:',IntToStr(PageTableGetSize));

    {Add Page Tables Address}
    AddBlank(AResponse);
    AddItem(AResponse,'Level 2 Page Tables Address:','0x' + AddrToHex(PageTablesGetAddress));
    
    {Add Page Tables Length}
    AddItem(AResponse,'Level 2 Page Tables Length:',IntToStr(PageTablesGetLength));
   
    {Add Page Tables Count}
    AddItem(AResponse,'Level 2 Page Tables Count:',IntToStr(PageTablesGetCount));
    
    {Add Page Tables Shift}
    AddItem(AResponse,'Level 2 Page Tables Shift:',IntToStr(PageTablesGetShift));
    
    {Add Page Tables Next}
    AddBlank(AResponse);
    AddItem(AResponse,'Level 2 Page Tables Next:','0x' + AddrToHex(PageTablesGetNext));
    
    {Add Page Tables Used}
    AddItem(AResponse,'Level 2 Page Tables Used:',IntToStr(PageTablesGetUsed));
   
    {Add Page Tables Free}
    AddItem(AResponse,'Level 2 Page Tables Free:',IntToStr(PageTablesGetFree));
   end;
  3:begin
    {3 level page table}
    {Add Page Directory Base}
    AddBlank(AResponse);
    AddItem(AResponse,'Level 1 Page Directory Base:','0x' + AddrToHex(PageDirectoryGetBase));

    {Add Page Directory Size}
    AddItem(AResponse,'Level 1 Page Directory Size:',IntToStr(PageDirectoryGetSize));

    {Add Page Table Base}
    AddBlank(AResponse);
    AddItem(AResponse,'Level 2 Page Table Base:','0x' + AddrToHex(PageTableGetBase));
   
    {Add Page Table Size}
    AddItem(AResponse,'Level 2 Page Table Size:',IntToStr(PageTableGetSize));

    {Add Page Tables Address}
    AddBlank(AResponse);
    AddItem(AResponse,'Level 3 Page Tables Address:','0x' + AddrToHex(PageTablesGetAddress));
    
    {Add Page Tables Length}
    AddItem(AResponse,'Level 3 Page Tables Length:',IntToStr(PageTablesGetLength));
   
    {Add Page Tables Count}
    AddItem(AResponse,'Level 3 Page Tables Count:',IntToStr(PageTablesGetCount));
    
    {Add Page Tables Shift}
    AddItem(AResponse,'Level 3 Page Tables Shift:',IntToStr(PageTablesGetShift));
    
    {Add Page Tables Next}
    AddBlank(AResponse);
    AddItem(AResponse,'Level 3 Page Tables Next:','0x' + AddrToHex(PageTablesGetNext));
    
    {Add Page Tables Used}
    AddItem(AResponse,'Level 3 Page Tables Used:',IntToStr(PageTablesGetUsed));
   
    {Add Page Tables Free}
    AddItem(AResponse,'Level 3 Page Tables Free:',IntToStr(PageTablesGetFree));
   end;
 end;

 {Add Vector Table Base}
 AddBlank(AResponse);
 AddItem(AResponse,'Vector Table Base:','0x' + AddrToHex(VectorTableGetBase));

 {Add Vector Table Size}
 AddItem(AResponse,'Vector Table Size:',IntToStr(VectorTableGetSize));

 {Add Vector Table Count}
 AddItem(AResponse,'Vector Table Count:',IntToStr(VectorTableGetCount));
 
 {Add Interrupt Count}
 AddBlank(AResponse);
 AddItem(AResponse,'Interrupt Count:',IntToStr(GetInterruptCount));

 {Add Interrupt Start}
 AddItem(AResponse,'Interrupt Start:',IntToStr(GetInterruptStart));

 {Add Local Interrupt Count}
 AddBlank(AResponse);
 AddItem(AResponse,'Local Interrupt Count:',IntToStr(GetLocalInterruptCount));

 {Add Local Interrupt Start}
 AddItem(AResponse,'Local Interrupt Start:',IntToStr(GetLocalInterruptStart));

 {Add Software Interrupt Count}
 AddBlank(AResponse);
 AddItem(AResponse,'Software Interrupt Count:',IntToStr(GetSoftwareInterruptCount));

 {Add Software Interrupt Start}
 AddItem(AResponse,'Software Interrupt Start:',IntToStr(GetSoftwareInterruptStart));

 {Add System Call Count}
 AddBlank(AResponse);
 AddItem(AResponse,'System Call Count:',IntToStr(GetSystemCallCount));
 
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
 AddItem(AResponse,'Time Ticks per Interrupt:',IntToStr(TIME_TICKS_PER_CLOCK_INTERRUPT));
 
 {Add Timer Thread Count}
 AddBlank(AResponse);
 AddItem(AResponse,'Timer Thread Count:',IntToStr(TIMER_THREAD_COUNT));

 {Add Timer Priority Thread Count}
 AddBlank(AResponse);
 AddItem(AResponse,'Timer Priority Thread Count:',IntToStr(TIMER_PRIORITY_THREAD_COUNT));
 
 {Add Worker Thread Count}
 AddBlank(AResponse);
 AddItem(AResponse,'Worker Thread Count (Current):',IntToStr(WorkerGetCount));
 AddItem(AResponse,'Worker Thread Count (Default):',IntToStr(WORKER_THREAD_COUNT));

 {Add Priority Worker Thread Count}
 AddBlank(AResponse);
 AddItem(AResponse,'Worker Priority Thread Count (Current):',IntToStr(WorkerGetPriorityCount));
 AddItem(AResponse,'Worker Priority Thread Count (Default):',IntToStr(WORKER_PRIORITY_THREAD_COUNT));

 {Add Touch Buffer Address}
 TouchGetBuffer(Address);
 AddBlank(AResponse);
 AddItem(AResponse,'Touch Buffer Address:','0x' + AddrToHex(Address));
 
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
 {Reserve}
 AddBlank(AResponse);
 AddItemEx(AResponse,'ReserveCount:',IntToStr(Statistics.ReserveCount),3);
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
 {Reserve Internal}
 AddBlank(AResponse);
 AddItemEx(AResponse,'ReserveInvalidCount:',IntToStr(Statistics.ReserveInvalidCount),3);
 AddItemEx(AResponse,'ReserveAddFailCount:',IntToStr(Statistics.ReserveAddFailCount),3);
 AddItemEx(AResponse,'ReserveSplitFailCount:',IntToStr(Statistics.ReserveSplitFailCount),3);
 AddItemEx(AResponse,'ReserveRemoveFailCount:',IntToStr(Statistics.ReserveRemoveFailCount),3);
 AddItemEx(AResponse,'ReserveUnavailableCount:',IntToStr(Statistics.ReserveUnavailableCount),3);
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
 AddHeaderEx(AResponse,GetTitle,'',Self,5); 

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
     AddItem5Column(AResponse,'0x' + AddrToHex(Current.Adddress),IntToStr(Current.Size),HeapStateToString(Current.State),FlagsToFlagName(Current.Flags),'0x' + IntToHex(Current.Affinity,8));
   
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
     AddItem5Column(AResponse,'0x' + AddrToHex(Current.Adddress),IntToStr(Current.Size),HeapStateToString(Current.State),FlagsToFlagName(Current.Flags),'0x' + IntToHex(Current.Affinity,8));

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
 Length:UInt64;
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

 {Add CPU Max Count}
 AddItem(AResponse,'CPU Max Count:',IntToStr(CPU_MAX_COUNT));
 
 {Add CPU Mode}
 AddItem(AResponse,'CPU Mode:','0x' + IntToHex(CPUGetMode,8));
 
 //To Do //CPU State

 {Add CPU Group}
 AddItem(AResponse,'CPU Group:',CPUGroupToString(CPUGetGroup));

 {Add CPU Current}
 AddItem(AResponse,'CPU Current:',CPUIDToString(CPUGetCurrent));

 {Add CPU Memory}
 CPUGetMemory(Address,Length);
 AddItem(AResponse,'CPU Memory:','Address: ' + '0x' + AddrToHex(Address));
 AddItem(AResponse,'','Size: ' + IntToStr(Length));
 
 {Add CPU Clock Speed}
 AddItem(AResponse,'CPU Clock Speed:',IntToStr(ClockGetRate(CLOCK_ID_CPU)) + ' Hz');
 
 {Add CPU Utilization}
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     {AddItem(AResponse,'CPU Utilization:',CPUIDToString(Count) + ': ' + IntToStr(CPUGetUtilization(Count)));}
     AddItem(AResponse,'CPU Utilization:',CPUIDToString(Count) + ': ' + FloatToStr(CPUGetPercentage(Count)) + '%');
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
 Length:UInt64;
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
 AddItem(AResponse,'GPU Memory :','Address: ' + '0x' + AddrToHex(Address));
 AddItem(AResponse,'','Size: ' + IntToStr(Length));
 
 {Add GPU Clock Speed}
 AddItem(AResponse,'GPU Clock Speed:',IntToStr(ClockGetRate(CLOCK_ID_GPU)) + ' Hz');
 
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
 TableStart:PPointer;
 TableEnd:PPointer;
 TableProc:Pointer;
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
 AddItem(AResponse,'TEXT Start:','0x' + PtrToHex(@_text_start));

 {Add ETEXT}
 AddItem(AResponse,'ETEXT:','0x' + PtrToHex(@_etext));

 {Add DATA}
 AddItem(AResponse,'DATA:','0x' + PtrToHex(@_data));

 {Add EDATA}
 AddItem(AResponse,'EDATA:','0x' + PtrToHex(@_edata));

 {Add BSS Start}
 AddItem(AResponse,'BSS Start:','0x' + PtrToHex(@_bss_start));

 {Add BSS End}
 AddItem(AResponse,'BSS End:','0x' + PtrToHex(@_bss_end));

 {Add Preinit Array Start / Preinit Array End}
 TableStart:=@__preinit_array_start;
 TableEnd:=@__preinit_array_end;
 AddBlank(AResponse);
 AddItem(AResponse,'Preinit Array Start:','0x' + PtrToHex(@__preinit_array_start));
 AddItem(AResponse,'Preinit Array End:','0x' + PtrToHex(@__preinit_array_end));
 while TableStart < TableEnd do
  begin
   {Get Proc}
   TableProc:=TableStart^;
   AddItemEx(AResponse,'TableProc:','0x' + PtrToHex(TableProc),2);
   
   {Update Start}
   Inc(TableStart); {Increment PPointer increments by SizeOf(Pointer)}
  end; 
 
 {Add Init Array Start / Init Array End}
 TableStart:=@__init_array_start;
 TableEnd:=@__init_array_end;
 AddBlank(AResponse);
 AddItem(AResponse,'Init Array Start:','0x' + PtrToHex(@__init_array_start));
 AddItem(AResponse,'Init Array End:','0x' + PtrToHex(@__init_array_end));
 while TableStart < TableEnd do
  begin
   {Get Proc}
   TableProc:=TableStart^;
   AddItemEx(AResponse,'TableProc:','0x' + PtrToHex(TableProc),2);
   
   {Update Start}
   Inc(TableStart); {Increment PPointer increments by SizeOf(Pointer)}
  end; 

 {Add Ctors Start / Ctors End}
 TableStart:=@__ctors_start;
 TableEnd:=@__ctors_end;
 AddBlank(AResponse);
 AddItem(AResponse,'Ctors Start:','0x' + PtrToHex(@__ctors_start));
 AddItem(AResponse,'Ctors End:','0x' + PtrToHex(@__ctors_end));
 while TableStart < TableEnd do
  begin
   {Get Proc}
   TableProc:=TableStart^;
   AddItemEx(AResponse,'TableProc:','0x' + PtrToHex(TableProc),2);
   
   {Update Start}
   Inc(TableStart); {Increment PPointer increments by SizeOf(Pointer)}
  end; 
 
 {Add Fini Array Start / Fini Array End}
 TableStart:=@__fini_array_start;
 TableEnd:=@__fini_array_end;
 AddBlank(AResponse);
 AddItem(AResponse,'Fini Array Start:','0x' + PtrToHex(@__fini_array_start));
 AddItem(AResponse,'Fini Array End:','0x' + PtrToHex(@__fini_array_end));
 while TableStart < TableEnd do
  begin
   {Get Proc}
   TableProc:=TableStart^;
   AddItemEx(AResponse,'TableProc:','0x' + PtrToHex(TableProc),2);
   
   {Update Start}
   Inc(TableStart); {Increment PPointer increments by SizeOf(Pointer)}
  end; 

 {Add Dtors Start / Dtors End}
 TableStart:=@__dtors_start;
 TableEnd:=@__dtors_end;
 AddBlank(AResponse);
 AddItem(AResponse,'Dtors Start:','0x' + PtrToHex(@__dtors_start));
 AddItem(AResponse,'Dtors End:','0x' + PtrToHex(@__dtors_end));
 while TableStart < TableEnd do
  begin
   {Get Proc}
   TableProc:=TableStart^;
   AddItemEx(AResponse,'TableProc:','0x' + PtrToHex(TableProc),2);
   
   {Update Start}
   Inc(TableStart); {Increment PPointer increments by SizeOf(Pointer)}
  end; 
 
 {Add ThreadVarBlockSize}
 AddBlank(AResponse);
 AddItem(AResponse,'ThreadVarBlockSize:',IntToStr(ThreadVarBlockSize));

 {Add InitProc/ExitProc}
 AddBlank(AResponse); 
 AddItem(AResponse,'InitProc:','0x' + PtrToHex(InitProc));
 AddItem(AResponse,'ExitProc:','0x' + PtrToHex(ExitProc));
 
 {Add ErrorBase/ErrorAddr/ErrorCode}
 AddBlank(AResponse); 
 AddItem(AResponse,'ErrorBase:','0x' + PtrToHex(RtlErrorBase));
 AddItem(AResponse,'ErrorAddr:','0x' + PtrToHex(ErrorAddr));
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
     AddItemEx(AResponse,'Procs[' + IntToStr(Count) + '].InitProc:','0x' + PtrToHex(@Table.Procs[Count].InitProc),2);
     AddItemEx(AResponse,'Procs[' + IntToStr(Count) + '].FinalProc:','0x' + PtrToHex(@Table.Procs[Count].FinalProc),2);
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

 {Add Clock Milliseconds}
 AddItem(AResponse,'Clock Milliseconds:',IntToStr(ClockMilliseconds));

 {Add Clock Microseconds}
 AddItem(AResponse,'Clock Microseconds:',IntToStr(ClockMicroseconds));

 {Add Clock Nanoseconds}
 AddItem(AResponse,'Clock Nanoseconds:',IntToStr(ClockNanoseconds));

 {Add Tick Count}
 AddBlank(AResponse);
 AddItem(AResponse,'Tick Count:',IntToStr(GetTickCount));

 {Add Tick Count 64}
 AddItem(AResponse,'Tick Count 64:',IntToStr(GetTickCount64));

 {Add Clock Base}
 AddBlank(AResponse);
 AddItem(AResponse,'Clock Base (100ns ticks):',IntToStr(ClockBase));
 
 {Add Clock Time}
 AddItem(AResponse,'Clock Time (100ns ticks):',IntToStr(ClockGetTime));
 
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
 AddItem(AResponse,'Country Code:',IntToStr(COUNTRY_DEFAULT)); //To Do //Change to API ?
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

 {Add Default Language}
 AddItem(AResponse,'Default Language:',IntToHex(GetSystemDefaultLangID,4));
 AddBlank(AResponse);
 
 {Add Default Keymap}
 AddItem(AResponse,'Default Keymap:',KeymapGetName(KeymapGetDefault));
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

 {Add Condition Count}
 AddItemEx(AResponse,'Condition Count:',IntToStr(ConditionGetCount),2);

 {Add Completion Count}
 AddItemEx(AResponse,'Completion Count:',IntToStr(CompletionGetCount),2);
 
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

 {Add Tasker Count}
 AddItemEx(AResponse,'Tasker Count:',IntToStr(TaskerGetCount),2);
 
 {Add Thread Stack}
 AddBlank(AResponse);
 AddBold(AResponse,'Thread Stack','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'THREAD_STACK_DEFAULT_SIZE:',IntToStr(THREAD_STACK_DEFAULT_SIZE),2);
 AddItemEx(AResponse,'THREAD_STACK_MINIMUM_SIZE:',IntToStr(THREAD_STACK_MINIMUM_SIZE),2);
 AddItemEx(AResponse,'THREAD_STACK_MAXIMUM_SIZE:',IntToStr(THREAD_STACK_MAXIMUM_SIZE),2);
 AddItemEx(AResponse,'THREAD_STACK_GUARD_ENABLED:',BooleanToString(THREAD_STACK_GUARD_ENABLED),2);
 
 {Add Initial Thread}
 AddBlank(AResponse);
 AddBold(AResponse,'Initial Thread','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'INITIAL_TLS_SIZE',IntToStr(INITIAL_TLS_SIZE),3);
 AddItemEx(AResponse,'INITIAL_STACK_SIZE',IntToStr(INITIAL_STACK_SIZE),3);
 AddItemEx(AResponse,'INITIAL_STACK_BASE','0x' + AddrToHex(INITIAL_STACK_BASE),3);
 
 {Add Boot Thread}
 AddBlank(AResponse);
 AddBold(AResponse,'Boot Thread','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'BOOT_STACK_SIZE',IntToStr(BOOT_STACK_SIZE),3);
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'BOOT_STACK_BASE',CPUIDToString(Count) + ': ' + '0x' + AddrToHex(BOOT_STACK_BASE[Count]),3);
    end
   else
    begin   
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + '0x' + AddrToHex(BOOT_STACK_BASE[Count]),3);
    end; 
  end;
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItemEx(AResponse,'BOOT_THREAD_HANDLE',CPUIDToString(Count) + ': ' + '0x' + HandleToHex(BOOT_THREAD_HANDLE[Count]),3);
    end
   else
    begin   
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + '0x' + HandleToHex(BOOT_THREAD_HANDLE[Count]),3);
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
     AddItemEx(AResponse,'IDLE_THREAD_HANDLE',CPUIDToString(Count) + ': ' + '0x' + HandleToHex(IDLE_THREAD_HANDLE[Count]),3);
    end
   else
    begin   
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + '0x' + HandleToHex(IDLE_THREAD_HANDLE[Count]),3);
    end; 
  end;

 {Add IRQ Thread}
 if IRQ_ENABLED then
  begin
   AddBlank(AResponse);
   AddBold(AResponse,'IRQ Thread','');
   AddBlank(AResponse);
   AddItemEx(AResponse,'IRQ_STACK_SIZE',IntToStr(IRQ_STACK_SIZE),3);
   AddBlank(AResponse);
   for Count:=0 to CPUGetCount - 1 do
    begin
     if Count = CPU_ID_0 then
      begin
       AddItemEx(AResponse,'IRQ_STACK_BASE',CPUIDToString(Count) + ': ' + '0x' + AddrToHex(IRQ_STACK_BASE[Count]),3);
      end
     else
      begin   
       AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + '0x' + AddrToHex(IRQ_STACK_BASE[Count]),3);
      end; 
    end;
   AddBlank(AResponse);
   for Count:=0 to CPUGetCount - 1 do
    begin
     if Count = CPU_ID_0 then
      begin
       AddItemEx(AResponse,'IRQ_THREAD_HANDLE',CPUIDToString(Count) + ': ' + '0x' + HandleToHex(IRQ_THREAD_HANDLE[Count]),3);
      end
     else
      begin   
       AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + '0x' + HandleToHex(IRQ_THREAD_HANDLE[Count]),3);
      end; 
    end;
  end;

 {Add FIQ Thread}
 if FIQ_ENABLED then
  begin
   AddBlank(AResponse);
   AddBold(AResponse,'FIQ Thread','');
   AddBlank(AResponse);
   AddItemEx(AResponse,'FIQ_STACK_SIZE',IntToStr(FIQ_STACK_SIZE),3);
   AddBlank(AResponse);
   for Count:=0 to CPUGetCount - 1 do
    begin
     if Count = CPU_ID_0 then
      begin
       AddItemEx(AResponse,'FIQ_STACK_BASE',CPUIDToString(Count) + ': ' + '0x' + AddrToHex(FIQ_STACK_BASE[Count]),3);
      end
     else
      begin   
       AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + '0x' + AddrToHex(FIQ_STACK_BASE[Count]),3);
      end; 
    end;
   AddBlank(AResponse);
   for Count:=0 to CPUGetCount - 1 do
    begin
     if Count = CPU_ID_0 then
      begin
       AddItemEx(AResponse,'FIQ_THREAD_HANDLE',CPUIDToString(Count) + ': ' + '0x' + HandleToHex(FIQ_THREAD_HANDLE[Count]),3);
      end
     else
      begin   
       AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + '0x' + HandleToHex(FIQ_THREAD_HANDLE[Count]),3);
      end; 
    end;
  end;
  
 {Add SWI Thread}
 if SWI_ENABLED then
  begin
   AddBlank(AResponse);
   AddBold(AResponse,'SWI Thread','');
   AddBlank(AResponse);
   AddItemEx(AResponse,'SWI_STACK_SIZE',IntToStr(SWI_STACK_SIZE),3);
   AddBlank(AResponse);
   for Count:=0 to CPUGetCount - 1 do
    begin
     if Count = CPU_ID_0 then
      begin
       AddItemEx(AResponse,'SWI_STACK_BASE',CPUIDToString(Count) + ': ' + '0x' + AddrToHex(SWI_STACK_BASE[Count]),3);
      end
     else
      begin   
       AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + '0x' + AddrToHex(SWI_STACK_BASE[Count]),3);
      end; 
    end;
   AddBlank(AResponse);
   for Count:=0 to CPUGetCount - 1 do
    begin
     if Count = CPU_ID_0 then
      begin
       AddItemEx(AResponse,'SWI_THREAD_HANDLE',CPUIDToString(Count) + ': ' + '0x' + HandleToHex(SWI_THREAD_HANDLE[Count]),3);
      end
     else
      begin   
       AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + '0x' + HandleToHex(SWI_THREAD_HANDLE[Count]),3);
      end; 
    end;
  end;

 {Add ABORT Stack}
 if ABORT_ENABLED then
  begin
   AddBlank(AResponse);
   AddBold(AResponse,'ABORT Stack','');
   AddBlank(AResponse);
   AddItemEx(AResponse,'ABORT_STACK_SIZE',IntToStr(ABORT_STACK_SIZE),3);
   AddBlank(AResponse);
   for Count:=0 to CPUGetCount - 1 do
    begin
     if Count = CPU_ID_0 then
      begin
       AddItemEx(AResponse,'ABORT_STACK_BASE',CPUIDToString(Count) + ': ' + '0x' + AddrToHex(ABORT_STACK_BASE[Count]),3);
      end
     else
      begin   
       AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + '0x' + AddrToHex(ABORT_STACK_BASE[Count]),3);
      end; 
    end;
  end;

 {Add UNDEFINED Stack}
 if UNDEFINED_ENABLED then
  begin
   AddBlank(AResponse);
   AddBold(AResponse,'UNDEFINED Stack','');
   AddBlank(AResponse);
   AddItemEx(AResponse,'UNDEFINED_STACK_SIZE',IntToStr(UNDEFINED_STACK_SIZE),3);
   AddBlank(AResponse);
   for Count:=0 to CPUGetCount - 1 do
    begin
     if Count = CPU_ID_0 then
      begin
       AddItemEx(AResponse,'UNDEFINED_STACK_BASE',CPUIDToString(Count) + ': ' + '0x' + AddrToHex(UNDEFINED_STACK_BASE[Count]),3);
      end
     else
      begin   
       AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + '0x' + AddrToHex(UNDEFINED_STACK_BASE[Count]),3);
      end; 
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

function TWebStatusThreadList.FlagsToFlagNames(AFlags:LongWord):TStringList;
begin
 {}
 Result:=TStringList.Create;
 
 {Check Flags}
 if (AFlags and THREAD_FLAG_PERSIST) = THREAD_FLAG_PERSIST then
  begin
   Result.Add('THREAD_FLAG_PERSIST');
  end;
 if (AFlags and THREAD_FLAG_CANCELLED) = THREAD_FLAG_CANCELLED then
  begin
   Result.Add('THREAD_FLAG_CANCELLED');
  end;

 {Check Flags}
 if Result.Count = 0 then
  begin
   Result.Add('THREAD_FLAG_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusThreadList.AffinityToAffinityNames(AAffinity:LongWord):TStringList;
var
 Mask:LongWord;
 Count:LongWord;
begin
 {}
 Result:=TStringList.Create;
 
 {Check Affinity}
 if AAffinity = CPU_AFFINITY_NONE then
  begin
   Result.Add('CPU_AFFINITY_NONE');
  end
 else if AAffinity = CPU_AFFINITY_ALL then
  begin
   Result.Add('CPU_AFFINITY_ALL');
  end
 else
  begin
   for Count:=0 to 31 do
    begin
     Mask:=1 shl Count;
     if (AAffinity and Mask) = Mask then
      begin
       Result.Add('CPU_AFFINITY_' + IntToStr(Count));
      end;
    end;
  end;  
end;
 
{==============================================================================}

function TWebStatusThreadList.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
var
 Action:String;
 Handle:THandle;
 Count:LongWord;
 WorkBuffer:String;
 WorkTime:TDateTime;
 FlagNames:TStringList;
 AffinityNames:TStringList;
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

 {Get Action}
 Action:=Uppercase(ARequest.GetParam('ACTION'));

 {Get Handle}
 WorkBuffer:=Uppercase(ARequest.GetParam('HANDLE'));
 
 if (Action = 'THREAD') and (Length(WorkBuffer) > 0) then
  begin
   {Add Header (2 column with Caption)}
   AddHeaderEx(AResponse,GetTitle,'Thread Information',Self,2);
   
   {Get Handle}
   Handle:=StrToIntDef('$' + WorkBuffer,0);
   
   {Create Snapshot}
   Snapshot:=ThreadSnapshotCreate;
   if Snapshot <> nil then
    begin
     {Get First}
     Current:=Snapshot;
     while Current <> nil do
      begin
       {Check Handle}
       if Current.Handle = Handle then
        begin
         {Get Flag Names}
         FlagNames:=FlagsToFlagNames(Current.Flags);
         
         {Get Affinity Names}
         AffinityNames:=AffinityToAffinityNames(Current.Affinity);
         
         {Add Thread Information}
         AddItem(AResponse,'Handle:','0x' + HandleToHex(Current.Handle));
         AddBlank(AResponse);
         AddItem(AResponse,'Name:',Current.Name);
         AddBlank(AResponse);
         AddItem(AResponse,'State:',ThreadStateToString(Current.State));
         AddItem(AResponse,'Flags:',FlagNames.Strings[0]);
         
         {Check Flag Count}
         if FlagNames.Count > 1 then
          begin
           for Count:=1 to FlagNames.Count - 1 do
            begin
             {Add Flag Name}
             AddItem(AResponse,'',FlagNames.Strings[Count]);
            end;
          end;
          
         AddItem(AResponse,'CPU:',CPUIDToString(Current.CPU));
         AddItem(AResponse,'Priority:',ThreadPriorityToString(Current.Priority));
         AddItem(AResponse,'Affinity:',AffinityNames.Strings[0]);
         
         {Check Affinity Count}
         if AffinityNames.Count > 1 then
          begin
           for Count:=1 to AffinityNames.Count - 1 do
            begin
             {Add Affinity Name}
             AddItem(AResponse,'',AffinityNames.Strings[Count]);
            end;
          end;
         
         AddBlank(AResponse);
         AddItem(AResponse,'StackBase:','0x' + PtrToHex(Current.StackBase));
         AddItem(AResponse,'StackSize:',IntToStr(Current.StackSize));
         AddItem(AResponse,'StackFree:',IntToStr(PtrUInt(Current.StackPointer) - (PtrUInt(Current.StackBase) - Current.StackSize)));
         AddItem(AResponse,'StackPointer:','0x' + PtrToHex(Current.StackPointer));
         AddBlank(AResponse);
         AddItem(AResponse,'Parent:',ThreadGetName(Current.Parent) + ' (0x' + HandleToHex(Current.Parent) + ')');
         AddItem(AResponse,'ExitCode:',IntToStr(Current.ExitCode));
         AddItem(AResponse,'LastError:',ErrorToString(Current.LastError));
         AddItem(AResponse,'Locale:',IntToStr(Current.Locale));
         AddBlank(AResponse);
         AddItem(AResponse,'TargetCPU:',CPUIDToString(Current.TargetCPU));
         AddItem(AResponse,'TargetPriority:',ThreadPriorityToString(Current.TargetPriority));
         AddBlank(AResponse);
         
         WorkTime:=SystemFileTimeToDateTime(TFileTime(Current.CreateTime));
         if WorkTime <> 0 then AddItem(AResponse,'CreateTime:',DateTimeToStr(WorkTime)) else AddItem(AResponse,'CreateTime:','N/A');
         
         WorkTime:=SystemFileTimeToDateTime(TFileTime(Current.ExitTime));
         if WorkTime <> 0 then AddItem(AResponse,'ExitTime:',DateTimeToStr(WorkTime)) else AddItem(AResponse,'ExitTime:','N/A');
         
         WorkTime:=SystemFileTimeToDateTime(TFileTime(Current.KernelTime)); {No Conversion}
         AddItem(AResponse,'KernelTime:',IntToStr(Trunc(WorkTime)) + ' days ' + TimeToStr(WorkTime));
         AddItem(AResponse,'SwitchCount:',IntToStr(Current.SwitchCount));
         
         FlagNames.Free;
         AffinityNames.Free;
         
         Break;
        end; 
       
       {Get Next}
       Current:=Current.Next;
      end;
     
     {Check Current}
     if Current = nil then
      begin
       AddItem(AResponse,'Not Found','');
      end;
      
     {Destroy Snapshot}
     ThreadSnapshotDestroy(Snapshot);
    end; 
    
   {Add Footer}
   AddFooter(AResponse); 
  end
 else
  begin 
   {Add Header (5 column)}
   AddHeaderEx(AResponse,GetTitle,'',Self,5); 
    
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
       AddItem5Column(AResponse,MakeLink('0x' + HandleToHex(Current.Handle),Name + '?action=thread&handle=' + IntToHex(Current.Handle,8)),Current.Name,ThreadStateToString(Current.State),ThreadPriorityToString(Current.Priority),CPUIDToString(Current.CPU));
     
       {Get Next}
       Current:=Current.Next;
      end;
      
     {Destroy Snapshot}
     ThreadSnapshotDestroy(Snapshot);
    end; 
   
   {Add Footer (5 column)}
   AddFooterEx(AResponse,5); 
  end;
  
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
 AddItemEx(AResponse,'Time Ticks per Interrupt:',IntToStr(TIME_TICKS_PER_SCHEDULER_INTERRUPT),2);

 {Add CPU Count/Mask/Boot}
 AddBlank(AResponse);
 AddItemEx(AResponse,'CPU Count:',IntToStr(SCHEDULER_CPU_COUNT),2);
 AddItemEx(AResponse,'CPU Mask:','0x' + IntToHex(SCHEDULER_CPU_MASK,8),2);
 AddItemEx(AResponse,'CPU Boot:',CPUIDToString(SCHEDULER_CPU_BOOT),2);
 AddItemEx(AResponse,'CPU Reserve:',IntToHex(SCHEDULER_CPU_RESERVE,8),2);
 
 {Add Scheduler Idle/Wait/Offset}
 AddBlank(AResponse);
 AddItemEx(AResponse,'Idle Wait:',BooleanToString(SCHEDULER_IDLE_WAIT),2);
 AddItemEx(AResponse,'Idle Offset:',IntToStr(SCHEDULER_IDLE_OFFSET),2);
 AddItemEx(AResponse,'Idle per Second:',IntToStr(SCHEDULER_IDLE_PER_SECOND),2);
 
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

 {Add Scheduler Thread Quanta}
 AddBlank(AResponse);
 AddBold(AResponse,'Scheduler Thread Quanta','');
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
 
 {Add Scheduler Starvation Quanta}
 AddBlank(AResponse);
 AddBold(AResponse,'Scheduler Starvation Quanta','');
 AddBlank(AResponse);
 for Count:=0 to CPUGetCount - 1 do
  begin
   AddItemEx(AResponse,CPUIDToString(Count) + ':',IntToStr(SchedulerGetStarvationQuantum(Count)),3);
  end;
 
 {Add Scheduler Priority Quanta}
 AddBlank(AResponse);
 AddBold(AResponse,'Scheduler Priority Quanta','');
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

function TWebStatusDevices.MMCFlagsToFlagNames(AFlags:LongWord):TStringList;
begin
 {}
 Result:=TStringList.Create;

 {Check Flags}
 if (AFlags and MMC_FLAG_CARD_PRESENT) = MMC_FLAG_CARD_PRESENT then
  begin
   Result.Add('MMC_FLAG_CARD_PRESENT');
  end;
 if (AFlags and MMC_FLAG_WRITE_PROTECT) = MMC_FLAG_WRITE_PROTECT then
  begin
   Result.Add('MMC_FLAG_WRITE_PROTECT');
  end;
 if (AFlags and MMC_FLAG_HIGH_CAPACITY) = MMC_FLAG_HIGH_CAPACITY then
  begin
   Result.Add('MMC_FLAG_HIGH_CAPACITY');
  end;
 if (AFlags and MMC_FLAG_EXT_CAPACITY) = MMC_FLAG_EXT_CAPACITY then
  begin
   Result.Add('MMC_FLAG_EXT_CAPACITY');
  end;
 if (AFlags and MMC_FLAG_UHS_I) = MMC_FLAG_UHS_I then
  begin
   Result.Add('MMC_FLAG_UHS_I');
  end;
 if (AFlags and MMC_FLAG_UHS_II) = MMC_FLAG_UHS_II then
  begin
   Result.Add('MMC_FLAG_UHS_II');
  end;
 if (AFlags and MMC_FLAG_BLOCK_ADDRESSED) = MMC_FLAG_BLOCK_ADDRESSED then
  begin
   Result.Add('MMC_FLAG_BLOCK_ADDRESSED');
  end;
 if (AFlags and MMC_FLAG_AUTO_BLOCK_COUNT) = MMC_FLAG_AUTO_BLOCK_COUNT then
  begin
   Result.Add('MMC_FLAG_AUTO_BLOCK_COUNT');
  end;
 if (AFlags and MMC_FLAG_AUTO_COMMAND_STOP) = MMC_FLAG_AUTO_COMMAND_STOP then
  begin
   Result.Add('MMC_FLAG_AUTO_COMMAND_STOP');
  end;
 if (AFlags and MMC_FLAG_DDR_MODE) = MMC_FLAG_DDR_MODE then
  begin
   Result.Add('MMC_FLAG_DDR_MODE');
  end;
 if (AFlags and MMC_FLAG_NON_REMOVABLE) = MMC_FLAG_NON_REMOVABLE then
  begin
   Result.Add('MMC_FLAG_NON_REMOVABLE');
  end;
 if (AFlags and MMC_FLAG_SET_BLOCK_COUNT) = MMC_FLAG_SET_BLOCK_COUNT then
  begin
   Result.Add('MMC_FLAG_SET_BLOCK_COUNT');
  end;

 {Check Flags}
 if Result.Count = 0 then
  begin
   Result.Add('MMC_FLAG_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusDevices.SDHCIFlagsToFlagNames(AFlags:LongWord):TStringList;
begin
 {}
 Result:=TStringList.Create;

 {Check Flags}
 if (AFlags and SDHCI_FLAG_SDMA) = SDHCI_FLAG_SDMA then
  begin
   Result.Add('SDHCI_FLAG_SDMA');
  end;
 if (AFlags and SDHCI_FLAG_ADMA) = SDHCI_FLAG_ADMA then
  begin
   Result.Add('SDHCI_FLAG_ADMA');
  end;
 if (AFlags and SDHCI_FLAG_SPI) = SDHCI_FLAG_SPI then
  begin
   Result.Add('SDHCI_FLAG_SPI');
  end;
 if (AFlags and SDHCI_FLAG_CRC_ENABLE) = SDHCI_FLAG_CRC_ENABLE then
  begin
   Result.Add('SDHCI_FLAG_CRC_ENABLE');
  end;
 if (AFlags and SDHCI_FLAG_NON_STANDARD) = SDHCI_FLAG_NON_STANDARD then
  begin
   Result.Add('SDHCI_FLAG_NON_STANDARD');
  end;
 if (AFlags and SDHCI_FLAG_AUTO_CMD12) = SDHCI_FLAG_AUTO_CMD12 then
  begin
   Result.Add('SDHCI_FLAG_AUTO_CMD12');
  end;
 if (AFlags and SDHCI_FLAG_AUTO_CMD23) = SDHCI_FLAG_AUTO_CMD23 then
  begin
   Result.Add('SDHCI_FLAG_AUTO_CMD23');
  end;
 if (AFlags and SDHCI_FLAG_EXTERNAL_DMA) = SDHCI_FLAG_EXTERNAL_DMA then
  begin
   Result.Add('SDHCI_FLAG_EXTERNAL_DMA');
  end;
 if (AFlags and SDHCI_FLAG_BUS_ADDRESSES) = SDHCI_FLAG_BUS_ADDRESSES then
  begin
   Result.Add('SDHCI_FLAG_BUS_ADDRESSES');
  end;

 {Check Flags}
 if Result.Count = 0 then
  begin
   Result.Add('SDHCI_FLAG_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusDevices.USBFlagsToFlagNames(AFlags:LongWord):TStringList;
begin
 {}
 Result:=TStringList.Create;

 {Check Flags}
 {Nothing}

 {Check Flags}
 if Result.Count = 0 then
  begin
   Result.Add('USB_FLAG_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusDevices.USBHostFlagsToFlagNames(AFlags:LongWord):TStringList;
begin
 {}
 Result:=TStringList.Create;

 {Check Flags}
 if (AFlags and USBHOST_FLAG_SHARED) = USBHOST_FLAG_SHARED then
  begin
   Result.Add('USBHOST_FLAG_SHARED');
  end;
 if (AFlags and USBHOST_FLAG_NOCACHE) = USBHOST_FLAG_NOCACHE then
  begin
   Result.Add('USBHOST_FLAG_NOCACHE');
  end;

 {Check Flags}
 if Result.Count = 0 then
  begin
   Result.Add('USBHOST_FLAG_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusDevices.PCIFlagsToFlagNames(AFlags:LongWord):TStringList;
begin
 {}
 Result:=TStringList.Create;

 {Check Flags}
 {Nothing}

 {Check Flags}
 if Result.Count = 0 then
  begin
   Result.Add('PCI_FLAG_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusDevices.PCIHostFlagsToFlagNames(AFlags:LongWord):TStringList;
begin
 {}
 Result:=TStringList.Create;

 {Check Flags}
 if (AFlags and PCIHOST_FLAG_SHARED) = PCIHOST_FLAG_SHARED then
  begin
   Result.Add('PCIHOST_FLAG_SHARED');
  end;
 if (AFlags and PCIHOST_FLAG_NOCACHE) = PCIHOST_FLAG_NOCACHE then
  begin
   Result.Add('PCIHOST_FLAG_NOCACHE');
  end;

 {Check Flags}
 if Result.Count = 0 then
  begin
   Result.Add('PCIHOST_FLAG_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusDevices.DMAFlagsToFlagNames(AFlags:LongWord):TStringList;
begin
 {}
 Result:=TStringList.Create;

 {Check Flags}
 if (AFlags and DMA_FLAG_SHARED) = DMA_FLAG_SHARED then
  begin
   Result.Add('DMA_FLAG_SHARED');
  end;
 if (AFlags and DMA_FLAG_NOCACHE) = DMA_FLAG_NOCACHE then
  begin
   Result.Add('DMA_FLAG_NOCACHE');
  end;
 if (AFlags and DMA_FLAG_COHERENT) = DMA_FLAG_COHERENT then
  begin
   Result.Add('DMA_FLAG_COHERENT');
  end;
 if (AFlags and DMA_FLAG_STRIDE) = DMA_FLAG_STRIDE then
  begin
   Result.Add('DMA_FLAG_STRIDE');
  end;
 if (AFlags and DMA_FLAG_DREQ) = DMA_FLAG_DREQ then
  begin
   Result.Add('DMA_FLAG_DREQ');
  end;
 if (AFlags and DMA_FLAG_NOINCREMENT) = DMA_FLAG_NOINCREMENT then
  begin
   Result.Add('DMA_FLAG_NOINCREMENT');
  end;
 if (AFlags and DMA_FLAG_NOREAD) = DMA_FLAG_NOREAD then
  begin
   Result.Add('DMA_FLAG_NOREAD');
  end;
 if (AFlags and DMA_FLAG_NOWRITE) = DMA_FLAG_NOWRITE then
  begin
   Result.Add('DMA_FLAG_NOWRITE');
  end;
 if (AFlags and DMA_FLAG_WIDE) = DMA_FLAG_WIDE then
  begin
   Result.Add('DMA_FLAG_WIDE');
  end;
 if (AFlags and DMA_FLAG_BULK) = DMA_FLAG_BULK then
  begin
   Result.Add('DMA_FLAG_BULK');
  end;
 if (AFlags and DMA_FLAG_LITE) = DMA_FLAG_LITE then
  begin
   Result.Add('DMA_FLAG_LITE');
  end;
 if (AFlags and DMA_FLAG_40BIT) = DMA_FLAG_40BIT then
  begin
   Result.Add('DMA_FLAG_40BIT');
  end;

 {Check Flags}
 if Result.Count = 0 then
  begin
   Result.Add('DMA_FLAG_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusDevices.I2CFlagsToFlagNames(AFlags:LongWord):TStringList;
begin
 {}
 Result:=TStringList.Create;

 {Check Flags}
 if (AFlags and I2C_FLAG_SLAVE) = I2C_FLAG_SLAVE then
  begin
   Result.Add('I2C_FLAG_SLAVE');
  end;
 if (AFlags and I2C_FLAG_10BIT) = I2C_FLAG_10BIT then
  begin
   Result.Add('I2C_FLAG_10BIT');
  end;
 if (AFlags and I2C_FLAG_16BIT) = I2C_FLAG_16BIT then
  begin
   Result.Add('I2C_FLAG_16BIT');
  end;

 {Check Flags}
 if Result.Count = 0 then
  begin
   Result.Add('I2C_FLAG_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusDevices.SPIFlagsToFlagNames(AFlags:LongWord):TStringList;
begin
 {}
 Result:=TStringList.Create;

 {Check Flags}
 if (AFlags and SPI_FLAG_SLAVE) = SPI_FLAG_SLAVE then
  begin
   Result.Add('SPI_FLAG_SLAVE');
  end;
 if (AFlags and SPI_FLAG_4WIRE) = SPI_FLAG_4WIRE then
  begin
   Result.Add('SPI_FLAG_4WIRE');
  end;
 if (AFlags and SPI_FLAG_3WIRE) = SPI_FLAG_3WIRE then
  begin
   Result.Add('SPI_FLAG_3WIRE');
  end;
 if (AFlags and SPI_FLAG_LOSSI) = SPI_FLAG_LOSSI then
  begin
   Result.Add('SPI_FLAG_LOSSI');
  end;
 if (AFlags and SPI_FLAG_CPOL) = SPI_FLAG_CPOL then
  begin
   Result.Add('SPI_FLAG_CPOL');
  end;
 if (AFlags and SPI_FLAG_CPHA) = SPI_FLAG_CPHA then
  begin
   Result.Add('SPI_FLAG_CPHA');
  end;
 if (AFlags and SPI_FLAG_CSPOL) = SPI_FLAG_CSPOL then
  begin
   Result.Add('SPI_FLAG_CSPOL');
  end;
 if (AFlags and SPI_FLAG_NO_CS) = SPI_FLAG_NO_CS then
  begin
   Result.Add('SPI_FLAG_NO_CS');
  end;
 if (AFlags and SPI_FLAG_DMA) = SPI_FLAG_DMA then
  begin
   Result.Add('SPI_FLAG_DMA');
  end;

 {Check Flags}
 if Result.Count = 0 then
  begin
   Result.Add('SPI_FLAG_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusDevices.PWMFlagsToFlagNames(AFlags:LongWord):TStringList;
begin
 {}
 Result:=TStringList.Create;

 {Check Flags}
 if (AFlags and PWM_FLAG_GPIO) = PWM_FLAG_GPIO then
  begin
   Result.Add('PWM_FLAG_GPIO');
  end;
 if (AFlags and PWM_FLAG_MODE) = PWM_FLAG_MODE then
  begin
   Result.Add('PWM_FLAG_MODE');
  end;
 if (AFlags and PWM_FLAG_RANGE) = PWM_FLAG_RANGE then
  begin
   Result.Add('PWM_FLAG_RANGE');
  end;
 if (AFlags and PWM_FLAG_FREQUENCY) = PWM_FLAG_FREQUENCY then
  begin
   Result.Add('PWM_FLAG_FREQUENCY');
  end;
 if (AFlags and PWM_FLAG_POLARITY) = PWM_FLAG_POLARITY then
  begin
   Result.Add('PWM_FLAG_POLARITY');
  end;

 {Check Flags}
 if Result.Count = 0 then
  begin
   Result.Add('PWM_FLAG_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusDevices.RTCFlagsToFlagNames(AFlags:LongWord):TStringList;
begin
 {}
 Result:=TStringList.Create;

 {Check Flags}
 if (AFlags and RTC_FLAG_ALARM) = RTC_FLAG_ALARM then
  begin
   Result.Add('RTC_FLAG_ALARM');
  end;
 if (AFlags and RTC_FLAG_WATCHDOG) = RTC_FLAG_WATCHDOG then
  begin
   Result.Add('RTC_FLAG_WATCHDOG');
  end;

 {Check Flags}
 if Result.Count = 0 then
  begin
   Result.Add('RTC_FLAG_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusDevices.GPIOFlagsToFlagNames(AFlags:LongWord):TStringList;
begin
 {}
 Result:=TStringList.Create;

 {Check Flags}
 if (AFlags and GPIO_FLAG_PULL_UP) = GPIO_FLAG_PULL_UP then
  begin
   Result.Add('GPIO_FLAG_PULL_UP');
  end;
 if (AFlags and GPIO_FLAG_PULL_DOWN) = GPIO_FLAG_PULL_DOWN then
  begin
   Result.Add('GPIO_FLAG_PULL_DOWN');
  end;
 if (AFlags and GPIO_FLAG_TRIGGER_LOW) = GPIO_FLAG_TRIGGER_LOW then
  begin
   Result.Add('GPIO_FLAG_TRIGGER_LOW');
  end;
 if (AFlags and GPIO_FLAG_TRIGGER_HIGH) = GPIO_FLAG_TRIGGER_HIGH then
  begin
   Result.Add('GPIO_FLAG_TRIGGER_HIGH');
  end;
 if (AFlags and GPIO_FLAG_TRIGGER_RISING) = GPIO_FLAG_TRIGGER_RISING then
  begin
   Result.Add('GPIO_FLAG_TRIGGER_RISING');
  end;
 if (AFlags and GPIO_FLAG_TRIGGER_FALLING) = GPIO_FLAG_TRIGGER_FALLING then
  begin
   Result.Add('GPIO_FLAG_TRIGGER_FALLING');
  end;
 if (AFlags and GPIO_FLAG_TRIGGER_EDGE) = GPIO_FLAG_TRIGGER_EDGE then
  begin
   Result.Add('GPIO_FLAG_TRIGGER_EDGE');
  end;
 if (AFlags and GPIO_FLAG_TRIGGER_ASYNC) = GPIO_FLAG_TRIGGER_ASYNC then
  begin
   Result.Add('GPIO_FLAG_TRIGGER_ASYNC');
  end;

 {Check Flags}
 if Result.Count = 0 then
  begin
   Result.Add('GPIO_FLAG_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusDevices.UARTFlagsToFlagNames(AFlags:LongWord):TStringList;
begin
 {}
 Result:=TStringList.Create;

 {Check Flags}
 if (AFlags and UART_FLAG_DATA_8BIT) = UART_FLAG_DATA_8BIT then
  begin
   Result.Add('UART_FLAG_DATA_8BIT');
  end;
 if (AFlags and UART_FLAG_DATA_7BIT) = UART_FLAG_DATA_7BIT then
  begin
   Result.Add('UART_FLAG_DATA_7BIT');
  end;
 if (AFlags and UART_FLAG_DATA_6BIT) = UART_FLAG_DATA_6BIT then
  begin
   Result.Add('UART_FLAG_DATA_6BIT');
  end;
 if (AFlags and UART_FLAG_DATA_5BIT) = UART_FLAG_DATA_5BIT then
  begin
   Result.Add('UART_FLAG_DATA_5BIT');
  end;
 if (AFlags and UART_FLAG_STOP_1BIT) = UART_FLAG_STOP_1BIT then
  begin
   Result.Add('UART_FLAG_STOP_1BIT');
  end;
 if (AFlags and UART_FLAG_STOP_2BIT) = UART_FLAG_STOP_2BIT then
  begin
   Result.Add('UART_FLAG_STOP_2BIT');
  end;
 if (AFlags and UART_FLAG_STOP_1BIT5) = UART_FLAG_STOP_1BIT5 then
  begin
   Result.Add('UART_FLAG_STOP_1BIT5');
  end;
 if (AFlags and UART_FLAG_PARITY_ODD) = UART_FLAG_PARITY_ODD then
  begin
   Result.Add('UART_FLAG_PARITY_ODD');
  end;
 if (AFlags and UART_FLAG_PARITY_EVEN) = UART_FLAG_PARITY_EVEN then
  begin
   Result.Add('UART_FLAG_PARITY_EVEN');
  end;
 if (AFlags and UART_FLAG_PARITY_MARK) = UART_FLAG_PARITY_MARK then
  begin
   Result.Add('UART_FLAG_PARITY_MARK');
  end;
 if (AFlags and UART_FLAG_PARITY_SPACE) = UART_FLAG_PARITY_SPACE then
  begin
   Result.Add('UART_FLAG_PARITY_SPACE');
  end;
 if (AFlags and UART_FLAG_FLOW_RTS_CTS) = UART_FLAG_FLOW_RTS_CTS then
  begin
   Result.Add('UART_FLAG_FLOW_RTS_CTS');
  end;
 if (AFlags and UART_FLAG_FLOW_DSR_DTR) = UART_FLAG_FLOW_DSR_DTR then
  begin
   Result.Add('UART_FLAG_FLOW_DSR_DTR');
  end;
 if (AFlags and UART_FLAG_PUSH_RX) = UART_FLAG_PUSH_RX then
  begin
   Result.Add('UART_FLAG_PUSH_RX');
  end;
 if (AFlags and UART_FLAG_PUSH_TX) = UART_FLAG_PUSH_TX then
  begin
   Result.Add('UART_FLAG_PUSH_TX');
  end;

 {Check Flags}
 if Result.Count = 0 then
  begin
   Result.Add('UART_FLAG_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusDevices.ClockFlagsToFlagNames(AFlags:LongWord):TStringList;
begin
 {}
 Result:=TStringList.Create;
 
 {Check Flags}
 if (AFlags and CLOCK_FLAG_WRITABLE) = CLOCK_FLAG_WRITABLE then
  begin
   Result.Add('CLOCK_FLAG_WRITABLE');
  end;
 if (AFlags and CLOCK_FLAG_VARIABLE) = CLOCK_FLAG_VARIABLE then
  begin
   Result.Add('CLOCK_FLAG_VARIABLE');
  end;

 {Check Flags}
 if Result.Count = 0 then
  begin
   Result.Add('CLOCK_FLAG_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusDevices.MouseFlagsToFlagNames(AFlags:LongWord):TStringList;
begin
 {}
 Result:=TStringList.Create;
 
 {Check Flags}
 if (AFlags and MOUSE_FLAG_NON_BLOCK) = MOUSE_FLAG_NON_BLOCK then
  begin
   Result.Add('MOUSE_FLAG_NON_BLOCK');
  end;
 if (AFlags and MOUSE_FLAG_DIRECT_READ) = MOUSE_FLAG_DIRECT_READ then
  begin
   Result.Add('MOUSE_FLAG_DIRECT_READ');
  end;
 if (AFlags and MOUSE_FLAG_SWAP_BUTTONS) = MOUSE_FLAG_SWAP_BUTTONS then
  begin
   Result.Add('MOUSE_FLAG_SWAP_BUTTONS');
  end;
 if (AFlags and MOUSE_FLAG_PEEK_BUFFER) = MOUSE_FLAG_PEEK_BUFFER then
  begin
   Result.Add('MOUSE_FLAG_PEEK_BUFFER');
  end;

 {Check Flags}
 if Result.Count = 0 then
  begin
   Result.Add('MOUSE_FLAG_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusDevices.TouchFlagsToFlagNames(AFlags:LongWord):TStringList;
begin
 {}
 Result:=TStringList.Create;
 
 {Check Flags}
 if (AFlags and TOUCH_FLAG_NON_BLOCK) = TOUCH_FLAG_NON_BLOCK then
  begin
   Result.Add('TOUCH_FLAG_NON_BLOCK');
  end;
 if (AFlags and TOUCH_FLAG_PEEK_BUFFER) = TOUCH_FLAG_PEEK_BUFFER then
  begin
   Result.Add('TOUCH_FLAG_PEEK_BUFFER');
  end;
 if (AFlags and TOUCH_FLAG_MOUSE_DATA) = TOUCH_FLAG_MOUSE_DATA then
  begin
   Result.Add('TOUCH_FLAG_MOUSE_DATA');
  end;
 if (AFlags and TOUCH_FLAG_MULTI_POINT) = TOUCH_FLAG_MULTI_POINT then
  begin
   Result.Add('TOUCH_FLAG_MULTI_POINT');
  end;
 if (AFlags and TOUCH_FLAG_PRESSURE) = TOUCH_FLAG_PRESSURE then
  begin
   Result.Add('TOUCH_FLAG_PRESSURE');
  end;

 {Check Flags}
 if Result.Count = 0 then
  begin
   Result.Add('TOUCH_FLAG_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusDevices.TimerFlagsToFlagNames(AFlags:LongWord):TStringList;
begin
 {}
 Result:=TStringList.Create;
 
 {Check Flags}
 if (AFlags and TIMER_FLAG_WRAPPING) = TIMER_FLAG_WRAPPING then
  begin
   Result.Add('TIMER_FLAG_WRAPPING');
  end;
 if (AFlags and TIMER_FLAG_COUNTER) = TIMER_FLAG_COUNTER then
  begin
   Result.Add('TIMER_FLAG_COUNTER');
  end;
 if (AFlags and TIMER_FLAG_DOWN) = TIMER_FLAG_DOWN then
  begin
   Result.Add('TIMER_FLAG_DOWN');
  end;

 {Check Flags}
 if Result.Count = 0 then
  begin
   Result.Add('TIMER_FLAG_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusDevices.SerialFlagsToFlagNames(AFlags:LongWord):TStringList;
begin
 {}
 Result:=TStringList.Create;
 
 {Check Flags}
 if (AFlags and SERIAL_FLAG_DATA_8BIT) = SERIAL_FLAG_DATA_8BIT then
  begin
   Result.Add('SERIAL_FLAG_DATA_8BIT');
  end;
 if (AFlags and SERIAL_FLAG_DATA_7BIT) = SERIAL_FLAG_DATA_7BIT then
  begin
   Result.Add('SERIAL_FLAG_DATA_7BIT');
  end;
 if (AFlags and SERIAL_FLAG_DATA_6BIT) = SERIAL_FLAG_DATA_6BIT then
  begin
   Result.Add('SERIAL_FLAG_DATA_6BIT');
  end;
 if (AFlags and SERIAL_FLAG_DATA_5BIT) = SERIAL_FLAG_DATA_5BIT then
  begin
   Result.Add('SERIAL_FLAG_DATA_5BIT');
  end;
 if (AFlags and SERIAL_FLAG_STOP_1BIT) = SERIAL_FLAG_STOP_1BIT then
  begin
   Result.Add('SERIAL_FLAG_STOP_1BIT');
  end;
 if (AFlags and SERIAL_FLAG_STOP_2BIT) = SERIAL_FLAG_STOP_2BIT then
  begin
   Result.Add('SERIAL_FLAG_STOP_2BIT');
  end;
 if (AFlags and SERIAL_FLAG_STOP_1BIT5) = SERIAL_FLAG_STOP_1BIT5 then
  begin
   Result.Add('SERIAL_FLAG_STOP_1BIT5');
  end;
 if (AFlags and SERIAL_FLAG_PARITY_ODD) = SERIAL_FLAG_PARITY_ODD then
  begin
   Result.Add('SERIAL_FLAG_PARITY_ODD');
  end;
 if (AFlags and SERIAL_FLAG_PARITY_EVEN) = SERIAL_FLAG_PARITY_EVEN then
  begin
   Result.Add('SERIAL_FLAG_PARITY_EVEN');
  end;
 if (AFlags and SERIAL_FLAG_PARITY_MARK) = SERIAL_FLAG_PARITY_MARK then
  begin
   Result.Add('SERIAL_FLAG_PARITY_MARK');
  end;
 if (AFlags and SERIAL_FLAG_PARITY_SPACE) = SERIAL_FLAG_PARITY_SPACE then
  begin
   Result.Add('SERIAL_FLAG_PARITY_SPACE');
  end;
 if (AFlags and SERIAL_FLAG_FLOW_RTS_CTS) = SERIAL_FLAG_FLOW_RTS_CTS then
  begin
   Result.Add('SERIAL_FLAG_FLOW_RTS_CTS');
  end;
 if (AFlags and SERIAL_FLAG_FLOW_DSR_DTR) = SERIAL_FLAG_FLOW_DSR_DTR then
  begin
   Result.Add('SERIAL_FLAG_FLOW_DSR_DTR');
  end;
 if (AFlags and SERIAL_FLAG_PUSH_RX) = SERIAL_FLAG_PUSH_RX then
  begin
   Result.Add('SERIAL_FLAG_PUSH_RX');
  end;
 if (AFlags and SERIAL_FLAG_PUSH_TX) = SERIAL_FLAG_PUSH_TX then
  begin
   Result.Add('SERIAL_FLAG_PUSH_TX');
  end;

 {Check Flags}
 if Result.Count = 0 then
  begin
   Result.Add('SERIAL_FLAG_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusDevices.RandomFlagsToFlagNames(AFlags:LongWord):TStringList;
begin
 {}
 Result:=TStringList.Create;
 
 {Check Flags}
 {Nothing}

 {Check Flags}
 if Result.Count = 0 then
  begin
   Result.Add('RANDOM_FLAG_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusDevices.MailboxFlagsToFlagNames(AFlags:LongWord):TStringList;
begin
 {}
 Result:=TStringList.Create;
 
 {Check Flags}
 {Nothing}

 {Check Flags}
 if Result.Count = 0 then
  begin
   Result.Add('MAILBOX_FLAG_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusDevices.WatchdogFlagsToFlagNames(AFlags:LongWord):TStringList;
begin
 {}
 Result:=TStringList.Create;
 
 {Check Flags}
 {Nothing}

 {Check Flags}
 if Result.Count = 0 then
  begin
   Result.Add('WATCHDOG_FLAG_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusDevices.NetworkFlagsToFlagNames(AFlags:LongWord):TStringList; 
begin
 {}
 Result:=TStringList.Create;
 
 {Check Flags}
 if (AFlags and NETWORK_FLAG_RX_BUFFER) = NETWORK_FLAG_RX_BUFFER then
  begin
   Result.Add('NETWORK_FLAG_RX_BUFFER');
  end;
 if (AFlags and NETWORK_FLAG_TX_BUFFER) = NETWORK_FLAG_TX_BUFFER then
  begin
   Result.Add('NETWORK_FLAG_TX_BUFFER');
  end;
 if (AFlags and NETWORK_FLAG_RX_MULTIPACKET) = NETWORK_FLAG_RX_MULTIPACKET then
  begin
   Result.Add('NETWORK_FLAG_RX_MULTIPACKET');
  end;
 if (AFlags and NETWORK_FLAG_TX_MULTIPACKET) = NETWORK_FLAG_TX_MULTIPACKET then
  begin
   Result.Add('NETWORK_FLAG_TX_MULTIPACKET');
  end;

 {Check Flags}
 if Result.Count = 0 then
  begin
   Result.Add('NETWORK_FLAG_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusDevices.LoggingFlagsToFlagNames(AFlags:LongWord):TStringList;
begin
 {}
 Result:=TStringList.Create;
 
 {Check Flags}
 {Nothing}

 {Check Flags}
 if Result.Count = 0 then
  begin
   Result.Add('LOGGING_FLAG_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusDevices.StorageFlagsToFlagNames(AFlags:LongWord):TStringList; 
begin
 {}
 Result:=TStringList.Create;
 
 {Check Flags}
 if (AFlags and STORAGE_FLAG_REMOVABLE) = STORAGE_FLAG_REMOVABLE then
  begin
   Result.Add('STORAGE_FLAG_REMOVABLE');
  end;
 if (AFlags and STORAGE_FLAG_LBA48) = STORAGE_FLAG_LBA48 then
  begin
   Result.Add('STORAGE_FLAG_LBA48');
  end;
 if (AFlags and STORAGE_FLAG_NOT_READY) = STORAGE_FLAG_NOT_READY then
  begin
   Result.Add('STORAGE_FLAG_NOT_READY');
  end;
 if (AFlags and STORAGE_FLAG_NO_MEDIA) = STORAGE_FLAG_NO_MEDIA then
  begin
   Result.Add('STORAGE_FLAG_NO_MEDIA');
  end;
 if (AFlags and STORAGE_FLAG_READ_ONLY) = STORAGE_FLAG_READ_ONLY then
  begin
   Result.Add('STORAGE_FLAG_READ_ONLY');
  end;
 if (AFlags and STORAGE_FLAG_WRITE_ONLY) = STORAGE_FLAG_WRITE_ONLY then
  begin
   Result.Add('STORAGE_FLAG_WRITE_ONLY');
  end;
 if (AFlags and STORAGE_FLAG_ERASEABLE) = STORAGE_FLAG_ERASEABLE then
  begin
   Result.Add('STORAGE_FLAG_ERASEABLE');
  end;
 if (AFlags and STORAGE_FLAG_LOCKABLE) = STORAGE_FLAG_LOCKABLE then
  begin
   Result.Add('STORAGE_FLAG_LOCKABLE');
  end;
 if (AFlags and STORAGE_FLAG_LOCKED) = STORAGE_FLAG_LOCKED then
  begin
   Result.Add('STORAGE_FLAG_LOCKED');
  end;
 if (AFlags and STORAGE_FLAG_EJECTABLE) = STORAGE_FLAG_EJECTABLE then
  begin
   Result.Add('STORAGE_FLAG_EJECTABLE');
  end;
 if (AFlags and STORAGE_FLAG_CHANGABLE) = STORAGE_FLAG_CHANGABLE then
  begin
   Result.Add('STORAGE_FLAG_CHANGABLE');
  end;

 {Check Flags}
 if Result.Count = 0 then
  begin
   Result.Add('STORAGE_FLAG_NONE');
  end; 
end;

{==============================================================================}


function TWebStatusDevices.KeyboardFlagsToFlagNames(AFlags:LongWord):TStringList; 
begin
 {}
 Result:=TStringList.Create;
 
 {Check Flags}
 if (AFlags and KEYBOARD_FLAG_NON_BLOCK) = KEYBOARD_FLAG_NON_BLOCK then
  begin
   Result.Add('KEYBOARD_FLAG_NON_BLOCK');
  end;
 if (AFlags and KEYBOARD_FLAG_DIRECT_READ) = KEYBOARD_FLAG_DIRECT_READ then
  begin
   Result.Add('KEYBOARD_FLAG_DIRECT_READ');
  end;
 if (AFlags and KEYBOARD_FLAG_PEEK_BUFFER) = KEYBOARD_FLAG_PEEK_BUFFER then
  begin
   Result.Add('KEYBOARD_FLAG_PEEK_BUFFER');
  end;

 {Check Flags}
 if Result.Count = 0 then
  begin
   Result.Add('KEYBOARD_FLAG_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusDevices.ConsoleFlagsToFlagNames(AFlags:LongWord):TStringList;
begin
 {}
 Result:=TStringList.Create;
 
 {Check Flags}
 if (AFlags and CONSOLE_FLAG_LINE_WRAP) = CONSOLE_FLAG_LINE_WRAP then
  begin
   Result.Add('CONSOLE_FLAG_LINE_WRAP');
  end;
 if (AFlags and CONSOLE_FLAG_DMA_BOX) = CONSOLE_FLAG_DMA_BOX then
  begin
   Result.Add('CONSOLE_FLAG_DMA_BOX');
  end;
 if (AFlags and CONSOLE_FLAG_DMA_LINE) = CONSOLE_FLAG_DMA_LINE then
  begin
   Result.Add('CONSOLE_FLAG_DMA_LINE');
  end;
 if (AFlags and CONSOLE_FLAG_DMA_FILL) = CONSOLE_FLAG_DMA_FILL then
  begin
   Result.Add('CONSOLE_FLAG_DMA_FILL');
  end;
 if (AFlags and CONSOLE_FLAG_DMA_CLEAR) = CONSOLE_FLAG_DMA_CLEAR then
  begin
   Result.Add('CONSOLE_FLAG_DMA_CLEAR');
  end;
 if (AFlags and CONSOLE_FLAG_DMA_SCROLL) = CONSOLE_FLAG_DMA_SCROLL then
  begin
   Result.Add('CONSOLE_FLAG_DMA_SCROLL');
  end;
 if (AFlags and CONSOLE_FLAG_SINGLE_WINDOW) = CONSOLE_FLAG_SINGLE_WINDOW then
  begin
   Result.Add('CONSOLE_FLAG_SINGLE_WINDOW');
  end;
 if (AFlags and CONSOLE_FLAG_HARDWARE_CURSOR) = CONSOLE_FLAG_HARDWARE_CURSOR then
  begin
   Result.Add('CONSOLE_FLAG_HARDWARE_CURSOR');
  end;
 if (AFlags and CONSOLE_FLAG_HARDWARE_CARET) = CONSOLE_FLAG_HARDWARE_CARET then
  begin
   Result.Add('CONSOLE_FLAG_HARDWARE_CARET');
  end;
 if (AFlags and CONSOLE_FLAG_BLINK_CARET) = CONSOLE_FLAG_BLINK_CARET then
  begin
   Result.Add('CONSOLE_FLAG_BLINK_CARET');
  end;
 if (AFlags and CONSOLE_FLAG_TEXT_MODE) = CONSOLE_FLAG_TEXT_MODE then
  begin
   Result.Add('CONSOLE_FLAG_TEXT_MODE');
  end;
 if (AFlags and CONSOLE_FLAG_TEXT_BLINK) = CONSOLE_FLAG_TEXT_BLINK then
  begin
   Result.Add('CONSOLE_FLAG_TEXT_BLINK');
  end;
 if (AFlags and CONSOLE_FLAG_COLOR) = CONSOLE_FLAG_COLOR then
  begin
   Result.Add('CONSOLE_FLAG_COLOR');
  end;
 if (AFlags and CONSOLE_FLAG_FONT) = CONSOLE_FLAG_FONT then
  begin
   Result.Add('CONSOLE_FLAG_FONT');
  end;
 if (AFlags and CONSOLE_FLAG_FULLSCREEN) = CONSOLE_FLAG_FULLSCREEN then
  begin
   Result.Add('CONSOLE_FLAG_FULLSCREEN');
  end;
 if (AFlags and CONSOLE_FLAG_AUTO_SCROLL) = CONSOLE_FLAG_AUTO_SCROLL then
  begin
   Result.Add('CONSOLE_FLAG_AUTO_SCROLL');
  end;
 if (AFlags and CONSOLE_FLAG_DMA_TEXT) = CONSOLE_FLAG_DMA_TEXT then
  begin
   Result.Add('CONSOLE_FLAG_DMA_TEXT');
  end;
 if (AFlags and CONSOLE_FLAG_COLOR_REVERSE) = CONSOLE_FLAG_COLOR_REVERSE then
  begin
   Result.Add('CONSOLE_FLAG_COLOR_REVERSE');
  end;
 if (AFlags and CONSOLE_FLAG_TEXT_CARET) = CONSOLE_FLAG_TEXT_CARET then
  begin
   Result.Add('CONSOLE_FLAG_TEXT_CARET');
  end;
 if (AFlags and CONSOLE_FLAG_FOCUS_CARET) = CONSOLE_FLAG_FOCUS_CARET then
  begin
   Result.Add('CONSOLE_FLAG_FOCUS_CARET');
  end;
  
 {Check Flags}
 if Result.Count = 0 then
  begin
   Result.Add('CONSOLE_FLAG_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusDevices.FramebufferFlagsToFlagNames(AFlags:LongWord):TStringList;
begin
 {}
 Result:=TStringList.Create;
 
 {Check Flags}
 if (AFlags and FRAMEBUFFER_FLAG_DMA) = FRAMEBUFFER_FLAG_DMA then
  begin
   Result.Add('FRAMEBUFFER_FLAG_DMA');
  end;
 if (AFlags and FRAMEBUFFER_FLAG_MARK) = FRAMEBUFFER_FLAG_MARK then
  begin
   Result.Add('FRAMEBUFFER_FLAG_MARK');
  end;
 if (AFlags and FRAMEBUFFER_FLAG_COMMIT) = FRAMEBUFFER_FLAG_COMMIT then
  begin
   Result.Add('FRAMEBUFFER_FLAG_COMMIT');
  end;
 if (AFlags and FRAMEBUFFER_FLAG_BLANK) = FRAMEBUFFER_FLAG_BLANK then
  begin
   Result.Add('FRAMEBUFFER_FLAG_BLANK');
  end;
 if (AFlags and FRAMEBUFFER_FLAG_CACHED) = FRAMEBUFFER_FLAG_CACHED then
  begin
   Result.Add('FRAMEBUFFER_FLAG_CACHED');
  end;
 if (AFlags and FRAMEBUFFER_FLAG_SWAP) = FRAMEBUFFER_FLAG_SWAP then
  begin
   Result.Add('FRAMEBUFFER_FLAG_SWAP');
  end;
 if (AFlags and FRAMEBUFFER_FLAG_BACKLIGHT) = FRAMEBUFFER_FLAG_BACKLIGHT then
  begin
   Result.Add('FRAMEBUFFER_FLAG_BACKLIGHT');
  end;
 if (AFlags and FRAMEBUFFER_FLAG_VIRTUAL) = FRAMEBUFFER_FLAG_VIRTUAL then
  begin
   Result.Add('FRAMEBUFFER_FLAG_VIRTUAL');
  end;
 if (AFlags and FRAMEBUFFER_FLAG_OFFSETX) = FRAMEBUFFER_FLAG_OFFSETX then
  begin
   Result.Add('FRAMEBUFFER_FLAG_OFFSETX');
  end;
 if (AFlags and FRAMEBUFFER_FLAG_OFFSETY) = FRAMEBUFFER_FLAG_OFFSETY then
  begin
   Result.Add('FRAMEBUFFER_FLAG_OFFSETY');
  end;
 if (AFlags and FRAMEBUFFER_FLAG_SYNC) = FRAMEBUFFER_FLAG_SYNC then
  begin
   Result.Add('FRAMEBUFFER_FLAG_SYNC');
  end;
 if (AFlags and FRAMEBUFFER_FLAG_CURSOR) = FRAMEBUFFER_FLAG_CURSOR then
  begin
   Result.Add('FRAMEBUFFER_FLAG_CURSOR');
  end;

 {Check Flags}
 if Result.Count = 0 then
  begin
   Result.Add('FRAMEBUFFER_FLAG_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusDevices.ConsoleWindowFlagsToFlagNames(AFlags:LongWord):TStringList;
begin
 {}
 Result:=TStringList.Create;
 
 {Check Flags}
 if (AFlags and WINDOW_FLAG_LINE_WRAP) = WINDOW_FLAG_LINE_WRAP then
  begin
   Result.Add('WINDOW_FLAG_LINE_WRAP');
  end;
 if (AFlags and WINDOW_FLAG_BUFFERED) = WINDOW_FLAG_BUFFERED then
  begin
   Result.Add('WINDOW_FLAG_BUFFERED');
  end;
 if (AFlags and WINDOW_FLAG_FULLSCREEN) = WINDOW_FLAG_FULLSCREEN then
  begin
   Result.Add('WINDOW_FLAG_FULLSCREEN');
  end;
 if (AFlags and WINDOW_FLAG_AUTO_SCROLL) = WINDOW_FLAG_AUTO_SCROLL then
  begin
   Result.Add('WINDOW_FLAG_AUTO_SCROLL');
  end;
 if (AFlags and WINDOW_FLAG_CHARACTER) = WINDOW_FLAG_CHARACTER then
  begin
   Result.Add('WINDOW_FLAG_CHARACTER');
  end;
 if (AFlags and WINDOW_FLAG_AUTO_UPDATE) = WINDOW_FLAG_AUTO_UPDATE then
  begin
   Result.Add('WINDOW_FLAG_AUTO_UPDATE');
  end;
 if (AFlags and WINDOW_FLAG_FOCUS_CURSOR) = WINDOW_FLAG_FOCUS_CURSOR then
  begin
   Result.Add('WINDOW_FLAG_FOCUS_CURSOR');
  end;
  
 {Check Flags}
 if Result.Count = 0 then
  begin
   Result.Add('WINDOW_FLAG_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusDevices.UARTStatusToStatusNames(AStatus:LongWord):TStringList;
begin
 {}
 Result:=TStringList.Create;
 
 {Check Status}
 if (AStatus and UART_STATUS_RTS) = UART_STATUS_RTS then
  begin
   Result.Add('UART_STATUS_RTS');
  end;
 if (AStatus and UART_STATUS_CTS) = UART_STATUS_CTS then
  begin
   Result.Add('UART_STATUS_CTS');
  end;
 if (AStatus and UART_STATUS_DSR) = UART_STATUS_DSR then
  begin
   Result.Add('UART_STATUS_DSR');
  end;
 if (AStatus and UART_STATUS_DTR) = UART_STATUS_DTR then
  begin
   Result.Add('UART_STATUS_DTR');
  end;
 if (AStatus and UART_STATUS_RX_FULL) = UART_STATUS_RX_FULL then
  begin
   Result.Add('UART_STATUS_RX_FULL');
  end;
 if (AStatus and UART_STATUS_RX_EMPTY) = UART_STATUS_RX_EMPTY then
  begin
   Result.Add('UART_STATUS_RX_EMPTY');
  end;
 if (AStatus and UART_STATUS_TX_FULL) = UART_STATUS_TX_FULL then
  begin
   Result.Add('UART_STATUS_TX_FULL');
  end;
 if (AStatus and UART_STATUS_TX_EMPTY) = UART_STATUS_TX_EMPTY then
  begin
   Result.Add('UART_STATUS_TX_EMPTY');
  end;
 if (AStatus and UART_STATUS_BUSY) = UART_STATUS_BUSY then
  begin
   Result.Add('UART_STATUS_BUSY');
  end;
 if (AStatus and UART_STATUS_BREAK_ERROR) = UART_STATUS_BREAK_ERROR then
  begin
   Result.Add('UART_STATUS_BREAK_ERROR');
  end;
 if (AStatus and UART_STATUS_PARITY_ERROR) = UART_STATUS_PARITY_ERROR then
  begin
   Result.Add('UART_STATUS_PARITY_ERROR');
  end;
 if (AStatus and UART_STATUS_FRAMING_ERROR) = UART_STATUS_FRAMING_ERROR then
  begin
   Result.Add('UART_STATUS_FRAMING_ERROR');
  end;
 if (AStatus and UART_STATUS_OVERRUN_ERROR) = UART_STATUS_OVERRUN_ERROR then
  begin
   Result.Add('UART_STATUS_OVERRUN_ERROR');
  end;
 if (AStatus and UART_STATUS_DCD) = UART_STATUS_DCD then
  begin
   Result.Add('UART_STATUS_DCD');
  end;
 if (AStatus and UART_STATUS_RI) = UART_STATUS_RI then
  begin
   Result.Add('UART_STATUS_RI');
  end;

 {Check Status}
 if Result.Count = 0 then
  begin
   Result.Add('UART_STATUS_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusDevices.SerialStatusToStatusNames(AStatus:LongWord):TStringList;
begin
 {}
 Result:=TStringList.Create;
 
 {Check Status}
 if (AStatus and SERIAL_STATUS_RTS) = SERIAL_STATUS_RTS then
  begin
   Result.Add('SERIAL_STATUS_RTS');
  end;
 if (AStatus and SERIAL_STATUS_CTS) = SERIAL_STATUS_CTS then
  begin
   Result.Add('SERIAL_STATUS_CTS');
  end;
 if (AStatus and SERIAL_STATUS_DSR) = SERIAL_STATUS_DSR then
  begin
   Result.Add('SERIAL_STATUS_DSR');
  end;
 if (AStatus and SERIAL_STATUS_DTR) = SERIAL_STATUS_DTR then
  begin
   Result.Add('SERIAL_STATUS_DTR');
  end;
 if (AStatus and SERIAL_STATUS_RX_FULL) = SERIAL_STATUS_RX_FULL then
  begin
   Result.Add('SERIAL_STATUS_RX_FULL');
  end;
 if (AStatus and SERIAL_STATUS_RX_EMPTY) = SERIAL_STATUS_RX_EMPTY then
  begin
   Result.Add('SERIAL_STATUS_RX_EMPTY');
  end;
 if (AStatus and SERIAL_STATUS_TX_FULL) = SERIAL_STATUS_TX_FULL then
  begin
   Result.Add('SERIAL_STATUS_TX_FULL');
  end;
 if (AStatus and SERIAL_STATUS_TX_EMPTY) = SERIAL_STATUS_TX_EMPTY then
  begin
   Result.Add('SERIAL_STATUS_TX_EMPTY');
  end;
 if (AStatus and SERIAL_STATUS_BUSY) = SERIAL_STATUS_BUSY then
  begin
   Result.Add('SERIAL_STATUS_BUSY');
  end;
 if (AStatus and SERIAL_STATUS_BREAK_ERROR) = SERIAL_STATUS_BREAK_ERROR then
  begin
   Result.Add('SERIAL_STATUS_BREAK_ERROR');
  end;
 if (AStatus and SERIAL_STATUS_PARITY_ERROR) = SERIAL_STATUS_PARITY_ERROR then
  begin
   Result.Add('SERIAL_STATUS_PARITY_ERROR');
  end;
 if (AStatus and SERIAL_STATUS_FRAMING_ERROR) = SERIAL_STATUS_FRAMING_ERROR then
  begin
   Result.Add('SERIAL_STATUS_FRAMING_ERROR');
  end;
 if (AStatus and SERIAL_STATUS_OVERRUN_ERROR) = SERIAL_STATUS_OVERRUN_ERROR then
  begin
   Result.Add('SERIAL_STATUS_OVERRUN_ERROR');
  end;
 if (AStatus and SERIAL_STATUS_DCD) = SERIAL_STATUS_DCD then
  begin
   Result.Add('SERIAL_STATUS_DCD');
  end;
 if (AStatus and SERIAL_STATUS_RI) = SERIAL_STATUS_RI then
  begin
   Result.Add('SERIAL_STATUS_RI');
  end;
 
 {Check Status}
 if Result.Count = 0 then
  begin
   Result.Add('SERIAL_STATUS_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusDevices.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
var
 Id:LongWord;
 Action:String;
 Device:PDevice;
 Count:LongWord;
 WorkBuffer:String;
 Data:TWebStatusData;
 FlagNames:TStringList;
 StatusNames:TStringList;
 
 DMAHost:PDMAHost;
 USBHost:PUSBHost;
 PCIHost:PPCIHost;
 SDHCIHost:PSDHCIHost;
 MMCDevice:PMMCDevice;
 USBDevice:PUSBDevice;
 PCIDevice:PPCIDevice;
 I2CDevice:PI2CDevice;
 SPIDevice:PSPIDevice;
 PWMDevice:PPWMDevice;
 RTCDevice:PRTCDevice;
 GPIODevice:PGPIODevice;
 UARTDevice:PUARTDevice;
 ClockDevice:PClockDevice;
 MouseDevice:PMouseDevice;
 TouchDevice:PTouchDevice;
 TimerDevice:PTimerDevice;
 SerialDevice:PSerialDevice;
 RandomDevice:PRandomDevice;
 MailboxDevice:PMailboxDevice;
 WatchdogDevice:PWatchdogDevice;
 LoggingDevice:PLoggingDevice;
 NetworkDevice:PNetworkDevice;
 StorageDevice:PStorageDevice;
 KeyboardDevice:PKeyboardDevice;
 ConsoleDevice:PConsoleDevice;
 FramebufferDevice:PFramebufferDevice;
 
 DMAProperties:TDMAProperties;
 I2CProperties:TI2CProperties;
 SPIProperties:TSPIProperties;
 PWMProperties:TPWMProperties;
 GPIOProperties:TGPIOProperties;
 UARTProperties:TUARTProperties;
 SerialProperties:TSerialProperties;
 FramebufferProperties:TFramebufferProperties;
begin
 {}
 Result:=False;
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;

 {Get Action}
 Action:=Uppercase(ARequest.GetParam('ACTION'));

 {Get Id}
 WorkBuffer:=Uppercase(ARequest.GetParam('ID'));
 
 if (Action = 'DEVICE') and (Length(WorkBuffer) > 0) then
  begin
   {Add Header (2 column with Caption)}
   AddHeaderEx(AResponse,GetTitle,'Device Information',Self,2);
 
   {Get Id}
   Id:=StrToIntDef(WorkBuffer,0);
 
   {Get Device}
   Device:=DeviceFind(DEVICE_CLASS_ANY,Id);
   if Device <> nil then
    begin
     AddBold(AResponse,'Device','');
     AddBlank(AResponse);
     AddItem(AResponse,'Signature:',IntToHex(Device.Signature,8));
     AddItem(AResponse,'Id:',IntToStr(Device.DeviceId));
     AddItem(AResponse,'State:',DeviceStateToString(Device.DeviceState));
     AddItem(AResponse,'Name:',Device.DeviceName);
     AddItem(AResponse,'Class:',DeviceClassToString(Device.DeviceClass));
     AddItem(AResponse,'Bus:',DeviceBusToString(Device.DeviceBus));
     AddItem(AResponse,'Type:',IntToStr(Device.DeviceType) + ' (Class specific)');
     AddItem(AResponse,'Flags:','0x' + IntToHex(Device.DeviceFlags,8) + ' (Class specific)');
     AddItem(AResponse,'Data:','0x' + IntToHex(PtrUInt(Device.DeviceData),8));
     AddItem(AResponse,'Description:',Device.DeviceDescription);
     AddBlank(AResponse);
     
     {Check Class}
     case Device.DeviceClass of
      DEVICE_CLASS_CLOCK:begin
        {Get Flags Names}
        FlagNames:=ClockFlagsToFlagNames(Device.DeviceFlags);
        
        {Get Clock}
        ClockDevice:=PClockDevice(Device);
       
        AddBold(AResponse,'Clock','');
        AddBlank(AResponse);
        AddItem(AResponse,'Type:',ClockTypeToString(Device.DeviceType));
        AddItem(AResponse,'Flags:',FlagNames.Strings[0]);
        
        {Check Flag Count}
        if FlagNames.Count > 1 then
         begin
          for Count:=1 to FlagNames.Count - 1 do
           begin
            {Add Flag Name}
            AddItem(AResponse,'',FlagNames.Strings[Count]);
           end;
         end;
        
        AddBlank(AResponse);
        AddItem(AResponse,'Id:',IntToStr(ClockDevice.ClockId));
        AddItem(AResponse,'State:',ClockStateToString(ClockDevice.ClockState));
        AddBlank(AResponse);
        AddItem(AResponse,'Read Count:',IntToStr(ClockDevice.ReadCount));
        AddBlank(AResponse);
        AddItem(AResponse,'Rate:',IntToStr(ClockDevice.Rate) + ' Hz');
        AddItem(AResponse,'Min Rate:',IntToStr(ClockDevice.MinRate) + ' Hz');
        AddItem(AResponse,'Max Rate:',IntToStr(ClockDevice.MaxRate) + ' Hz');
        
        FlagNames.Free;
       end;
      DEVICE_CLASS_TIMER:begin
        {Get Flags Names}
        FlagNames:=TimerFlagsToFlagNames(Device.DeviceFlags);
        
        {Get Timer}
        TimerDevice:=PTimerDevice(Device);
        
        AddBold(AResponse,'Timer','');
        AddBlank(AResponse);
        AddItem(AResponse,'Type:',TimerTypeToString(Device.DeviceType));
        AddItem(AResponse,'Flags:',FlagNames.Strings[0]);
        
        {Check Flag Count}
        if FlagNames.Count > 1 then
         begin
          for Count:=1 to FlagNames.Count - 1 do
           begin
            {Add Flag Name}
            AddItem(AResponse,'',FlagNames.Strings[Count]);
           end;
         end;
        
        AddBlank(AResponse);
        AddItem(AResponse,'Id:',IntToStr(TimerDevice.TimerId));
        AddItem(AResponse,'State:',TimerStateToString(TimerDevice.TimerState));
        AddBlank(AResponse);
        AddItem(AResponse,'Read Count:',IntToStr(TimerDevice.ReadCount));
        AddItem(AResponse,'Wait Count:',IntToStr(TimerDevice.WaitCount));
        AddItem(AResponse,'Event Count:',IntToStr(TimerDevice.EventCount));
        AddBlank(AResponse);
        AddItem(AResponse,'Rate:',IntToStr(TimerDevice.Rate) + ' Hz');
        AddItem(AResponse,'Interval:',IntToStr(TimerDevice.Interval) + ' ticks');
        AddItem(AResponse,'Bits:',IntToStr(TimerDevice.Properties.Bits));
        AddItem(AResponse,'Min Rate:',IntToStr(TimerDevice.Properties.MinRate) + ' Hz');
        AddItem(AResponse,'Max Rate:',IntToStr(TimerDevice.Properties.MaxRate) + ' Hz');
        AddItem(AResponse,'Min Interval:',IntToStr(TimerDevice.Properties.MinInterval) + ' ms');
        AddItem(AResponse,'Max Interval:',IntToStr(TimerDevice.Properties.MaxInterval) + ' ms');
        
        FlagNames.Free;
       end;
      DEVICE_CLASS_RANDOM:begin
        {Get Flags Names}
        FlagNames:=RandomFlagsToFlagNames(Device.DeviceFlags);
        
        {Get Random}
        RandomDevice:=PRandomDevice(Device);
        
        AddBold(AResponse,'Random','');
        AddBlank(AResponse);
        AddItem(AResponse,'Type:',RandomTypeToString(Device.DeviceType));
        AddItem(AResponse,'Flags:',FlagNames.Strings[0]); 
        
        {Check Flag Count}
        if FlagNames.Count > 1 then
         begin
          for Count:=1 to FlagNames.Count - 1 do
           begin
            {Add Flag Name}
            AddItem(AResponse,'',FlagNames.Strings[Count]);
           end;
         end;
        
        AddBlank(AResponse);
        AddItem(AResponse,'Id:',IntToStr(RandomDevice.RandomId));
        AddItem(AResponse,'State:',RandomStateToString(RandomDevice.RandomState));
        AddBlank(AResponse);
        AddItem(AResponse,'Seed Count:',IntToStr(RandomDevice.SeedCount));
        AddItem(AResponse,'Read Count:',IntToStr(RandomDevice.ReadCount));
        
        FlagNames.Free;
       end;
      DEVICE_CLASS_MAILBOX:begin
        {Get Flags Names}
        FlagNames:=MailboxFlagsToFlagNames(Device.DeviceFlags);
        
        {Get Mailbox}
        MailboxDevice:=PMailboxDevice(Device);
        
        AddBold(AResponse,'Mailbox','');
        AddBlank(AResponse);
        AddItem(AResponse,'Type:',MailboxTypeToString(Device.DeviceType));
        AddItem(AResponse,'Flags:',FlagNames.Strings[0]); 
        
        {Check Flag Count}
        if FlagNames.Count > 1 then
         begin
          for Count:=1 to FlagNames.Count - 1 do
           begin
            {Add Flag Name}
            AddItem(AResponse,'',FlagNames.Strings[Count]);
           end;
         end;
        
        AddBlank(AResponse);
        AddItem(AResponse,'Id:',IntToStr(MailboxDevice.MailboxId));
        AddItem(AResponse,'State:',MailboxStateToString(MailboxDevice.MailboxState));
        AddBlank(AResponse);
        AddItem(AResponse,'Receive Count:',IntToStr(MailboxDevice.ReceiveCount));
        AddItem(AResponse,'Send Count:',IntToStr(MailboxDevice.SendCount));
        AddItem(AResponse,'Call Count:',IntToStr(MailboxDevice.CallCount));
        AddBlank(AResponse);
        AddItem(AResponse,'Timeout:',IntToStr(MailboxDevice.Timeout) + ' ms');
        
        FlagNames.Free;
       end;
      DEVICE_CLASS_WATCHDOG:begin      
        {Get Flags Names}
        FlagNames:=WatchdogFlagsToFlagNames(Device.DeviceFlags);
        
        {Get Watchdog}
        WatchdogDevice:=PWatchdogDevice(Device);
        
        AddBold(AResponse,'Watchdog','');
        AddBlank(AResponse);
        AddItem(AResponse,'Type:',WatchdogTypeToString(Device.DeviceType));
        AddItem(AResponse,'Flags:',FlagNames.Strings[0]);
        
        {Check Flag Count}
        if FlagNames.Count > 1 then
         begin
          for Count:=1 to FlagNames.Count - 1 do
           begin
            {Add Flag Name}
            AddItem(AResponse,'',FlagNames.Strings[Count]);
           end;
         end;
        
        AddBlank(AResponse);
        AddItem(AResponse,'Id:',IntToStr(WatchdogDevice.WatchdogId));
        AddItem(AResponse,'State:',WatchdogStateToString(WatchdogDevice.WatchdogState));
        AddBlank(AResponse);
        AddItem(AResponse,'Start Count:',IntToStr(WatchdogDevice.StartCount));
        AddItem(AResponse,'Stop Count:',IntToStr(WatchdogDevice.StopCount));
        AddItem(AResponse,'Refresh Count:',IntToStr(WatchdogDevice.RefreshCount));
        AddBlank(AResponse);
        AddItem(AResponse,'Timeout:',IntToStr(WatchdogDevice.Timeout) + ' ms');
        
        FlagNames.Free;
       end;
      DEVICE_CLASS_GPIO:begin      
        {Get Flags Names}
        FlagNames:=GPIOFlagsToFlagNames(Device.DeviceFlags);
        
        {Get GPIO}
        GPIODevice:=PGPIODevice(Device);
       
        AddBold(AResponse,'GPIO','');
        AddBlank(AResponse);
        AddItem(AResponse,'Flags:',FlagNames.Strings[0]);
       
        {Check Flag Count}
        if FlagNames.Count > 1 then
         begin
          for Count:=1 to FlagNames.Count - 1 do
           begin
            {Add Flag Name}
            AddItem(AResponse,'',FlagNames.Strings[Count]);
           end;
         end;
       
        AddBlank(AResponse);
        AddItem(AResponse,'Id:',IntToStr(GPIODevice.GPIOId));
        AddItem(AResponse,'State:',GPIOStateToString(GPIODevice.GPIOState));
        AddBlank(AResponse);
        AddItem(AResponse,'Get Count:',IntToStr(GPIODevice.GetCount));
        AddItem(AResponse,'Set Count:',IntToStr(GPIODevice.SetCount));
        AddItem(AResponse,'Wait Count:',IntToStr(GPIODevice.WaitCount));
        AddItem(AResponse,'Event Count:',IntToStr(GPIODevice.EventCount));
        AddBlank(AResponse);
       
        {Get Properties}
        if GPIODeviceGetProperties(GPIODevice,@GPIOProperties) = ERROR_SUCCESS then
         begin
          AddItem(AResponse,'Pin Min:',IntToStr(GPIOProperties.PinMin));
          AddItem(AResponse,'Pin Max:',IntToStr(GPIOProperties.PinMax));
          AddItem(AResponse,'Pin Count:',IntToStr(GPIOProperties.PinCount));
          AddItem(AResponse,'Function Min:',IntToStr(GPIOProperties.FunctionMin));
          AddItem(AResponse,'Function Max:',IntToStr(GPIOProperties.FunctionMax));
          AddItem(AResponse,'Function Count:',IntToStr(GPIOProperties.FunctionCount));
         end;
        
        FlagNames.Free;
       end;
      DEVICE_CLASS_FRAMEBUFFER:begin      
        {Get Flags Names}
        FlagNames:=FramebufferFlagsToFlagNames(Device.DeviceFlags);
        
        {Get Framebuffer}
        FramebufferDevice:=PFramebufferDevice(Device);
       
        AddBold(AResponse,'Framebuffer','');
        AddBlank(AResponse);
        AddItem(AResponse,'Type:',FramebufferTypeToString(Device.DeviceType));
        AddItem(AResponse,'Flags:',FlagNames.Strings[0]);
       
        {Check Flag Count}
        if FlagNames.Count > 1 then
         begin
          for Count:=1 to FlagNames.Count - 1 do
           begin
            {Add Flag Name}
            AddItem(AResponse,'',FlagNames.Strings[Count]);
           end;
         end;
       
        AddBlank(AResponse);
        AddItem(AResponse,'Id:',IntToStr(FramebufferDevice.FramebufferId));
        AddItem(AResponse,'State:',FramebufferStateToString(FramebufferDevice.FramebufferState));
        AddBlank(AResponse);
        AddItem(AResponse,'Allocate Count:',IntToStr(FramebufferDevice.AllocateCount));
        AddItem(AResponse,'Release Count:',IntToStr(FramebufferDevice.ReleaseCount));
        AddItem(AResponse,'Read Count:',IntToStr(FramebufferDevice.ReadCount));
        AddItem(AResponse,'Write Count:',IntToStr(FramebufferDevice.WriteCount));
        AddItem(AResponse,'Get Count:',IntToStr(FramebufferDevice.GetCount));
        AddItem(AResponse,'Put Count:',IntToStr(FramebufferDevice.PutCount));
        AddItem(AResponse,'Copy Count:',IntToStr(FramebufferDevice.CopyCount));
        AddItem(AResponse,'Fill Count:',IntToStr(FramebufferDevice.FillCount));
        AddBlank(AResponse);
      
        {Get Properties}
        if FramebufferDeviceGetProperties(FramebufferDevice,@FramebufferProperties) = ERROR_SUCCESS then
         begin
          AddItem(AResponse,'Address:',AddrToHex(FramebufferProperties.Address));
          AddItem(AResponse,'Size:',IntToStr(FramebufferProperties.Size));
          AddBlank(AResponse);
          AddItem(AResponse,'Pitch (Bytes per Line):',IntToStr(FramebufferProperties.Pitch));
          AddItem(AResponse,'Colour Depth (Bits per Pixel):',FramebufferDepthToString(FramebufferProperties.Depth));
          AddItem(AResponse,'Pixel Order (BGR/RGB):',FramebufferOrderToString(FramebufferProperties.Order));
          AddItem(AResponse,'Alpha Mode:',FramebufferModeToString(FramebufferProperties.Mode));
          AddItem(AResponse,'Color Format:',ColorFormatToString(FramebufferProperties.Format));
          AddBlank(AResponse);
          AddItem(AResponse,'Physical Width (Pixels):',IntToStr(FramebufferProperties.PhysicalWidth));
          AddItem(AResponse,'Physical Height (Pixels):',IntToStr(FramebufferProperties.PhysicalHeight));
          AddBlank(AResponse);
          AddItem(AResponse,'Virtual Width (Pixels):',IntToStr(FramebufferProperties.VirtualWidth));
          AddItem(AResponse,'Virtual Height (Pixels):',IntToStr(FramebufferProperties.VirtualHeight));
          AddItem(AResponse,'Virtual Offset X (Pixels):',IntToStr(FramebufferProperties.OffsetX));
          AddItem(AResponse,'Virtual Offset Y (Pixels):',IntToStr(FramebufferProperties.OffsetY));
          AddBlank(AResponse);
          AddItem(AResponse,'Overscan Top (Pixels):',IntToStr(FramebufferProperties.OverscanTop));
          AddItem(AResponse,'Overscan Bottom (Pixels):',IntToStr(FramebufferProperties.OverscanBottom));
          AddItem(AResponse,'Overscan Left (Pixels):',IntToStr(FramebufferProperties.OverscanLeft));
          AddItem(AResponse,'Overscan Right (Pixels):',IntToStr(FramebufferProperties.OverscanRight));
          AddBlank(AResponse);
          AddItem(AResponse,'Rotation:',FramebufferRotationToString(FramebufferProperties.Rotation));
          AddBlank(AResponse);
          AddItem(AResponse,'Cursor X (Pixels):',IntToStr(FramebufferProperties.CursorX));
          AddItem(AResponse,'Cursor Y (Pixels):',IntToStr(FramebufferProperties.CursorY));
          AddItem(AResponse,'Cursor State:',FramebufferCursorToString(FramebufferProperties.CursorState));
         end;
         
        FlagNames.Free;
       end;
      DEVICE_CLASS_CONSOLE:begin      
        {Get Flags Names}
        FlagNames:=ConsoleFlagsToFlagNames(Device.DeviceFlags);
        
        {Get Console}
        ConsoleDevice:=PConsoleDevice(Device);
       
        AddBold(AResponse,'Console','');
        AddBlank(AResponse);
        AddItem(AResponse,'Type:',ConsoleTypeToString(Device.DeviceType));
        AddItem(AResponse,'Flags:',FlagNames.Strings[0]);
       
        {Check Flag Count}
        if FlagNames.Count > 1 then
         begin
          for Count:=1 to FlagNames.Count - 1 do
           begin
            {Add Flag Name}
            AddItem(AResponse,'',FlagNames.Strings[Count]);
           end;
         end;
       
        AddBlank(AResponse);
        AddItem(AResponse,'Id:',IntToStr(ConsoleDevice.ConsoleId));
        AddItem(AResponse,'State:',ConsoleStateToString(ConsoleDevice.ConsoleState));
        AddBlank(AResponse);
        AddItem(AResponse,'Open Count:',IntToStr(ConsoleDevice.OpenCount));
        AddItem(AResponse,'Close Count:',IntToStr(ConsoleDevice.CloseCount));
        AddItem(AResponse,'Clear Count:',IntToStr(ConsoleDevice.ClearCount));
        AddItem(AResponse,'Scroll Count:',IntToStr(ConsoleDevice.ScrollCount));
        AddItem(AResponse,'Draw Count:',IntToStr(ConsoleDevice.DrawCount));
        AddItem(AResponse,'Get Count:',IntToStr(ConsoleDevice.GetCount));
        AddItem(AResponse,'Put Count:',IntToStr(ConsoleDevice.PutCount));
        AddItem(AResponse,'Copy Count:',IntToStr(ConsoleDevice.CopyCount));
        AddBlank(AResponse);
        AddItem(AResponse,'Width:',IntToStr(ConsoleDevice.Width));
        AddItem(AResponse,'Height:',IntToStr(ConsoleDevice.Height));
        AddItem(AResponse,'Color Format:',ColorFormatToString(ConsoleDevice.Format));
        AddItem(AResponse,'Forecolor:','0x' + IntToHex(ConsoleDevice.Forecolor,8));
        AddItem(AResponse,'Backcolor:','0x' + IntToHex(ConsoleDevice.Backcolor,8));
        AddItem(AResponse,'Borderwidth:',IntToStr(ConsoleDevice.Borderwidth));
        AddItem(AResponse,'Bordercolor:','0x' + IntToHex(ConsoleDevice.Bordercolor,8));
        AddBlank(AResponse);
        AddItem(AResponse,'FontRatio:',IntToStr(ConsoleDevice.FontRatio));
        AddBlank(AResponse);
        AddItem(AResponse,'Cursor Update:',BooleanToString(ConsoleDevice.CursorUpdate));
        AddItem(AResponse,'Cursor X:',IntToStr(ConsoleDevice.CursorX));
        AddItem(AResponse,'Cursor Y:',IntToStr(ConsoleDevice.CursorY));
        AddItem(AResponse,'Cursor Width:',IntToStr(ConsoleDevice.CursorWidth));
        AddItem(AResponse,'Cursor Height:',IntToStr(ConsoleDevice.CursorHeight));
        AddItem(AResponse,'Cursor Visible:',BooleanToString(ConsoleDevice.CursorVisible));
        AddBlank(AResponse);
        AddItem(AResponse,'Caret Count:',IntToStr(ConsoleDevice.CaretCount));
        AddBlank(AResponse);
        AddItem(AResponse,'Window Count:',IntToStr(ConsoleDevice.WindowCount));
        AddBlank(AResponse);
       
        {Add Windows}
        AddBold(AResponse,'Windows','');
        AddBlank(AResponse);
       
        {Setup Data}
        Data.Document:=Self;
        Data.Host:=AHost;
        Data.Request:=ARequest;
        Data.Response:=AResponse;
        Data.Data:=nil;
        
        {Enumerate Windows}
        ConsoleWindowEnumerate(ConsoleDevice,WebStatusConsoleWindowEnumerate,@Data);
        
        FlagNames.Free;
       end;
      DEVICE_CLASS_LOGGING:begin      
        {Get Flags Names}
        FlagNames:=LoggingFlagsToFlagNames(Device.DeviceFlags);
        
        {Get Logging}
        LoggingDevice:=PLoggingDevice(Device);
       
        AddBold(AResponse,'Logging','');
        AddBlank(AResponse);
        AddItem(AResponse,'Type:',LoggingTypeToString(Device.DeviceType));
        AddItem(AResponse,'Flags:',FlagNames.Strings[0]);
       
        {Check Flag Count}
        if FlagNames.Count > 1 then
         begin
          for Count:=1 to FlagNames.Count - 1 do
           begin
            {Add Flag Name}
            AddItem(AResponse,'',FlagNames.Strings[Count]);
           end;
         end;
       
        AddBlank(AResponse);
        AddItem(AResponse,'Id:',IntToStr(LoggingDevice.LoggingId));
        AddItem(AResponse,'State:',LoggingStateToString(LoggingDevice.LoggingState));
        AddBlank(AResponse);
        AddItem(AResponse,'Output Count:',IntToStr(LoggingDevice.OutputCount));
        AddBlank(AResponse);
        AddItem(AResponse,'Output Target:',LoggingDevice.Target);
        AddItem(AResponse,'Preferred Default:',BooleanToString(LoggingDevice.Default));
        AddItem(AResponse,'Current Default:',BooleanToString(LoggingDeviceGetDefault = LoggingDevice));
        
        FlagNames.Free;
       end;
      DEVICE_CLASS_UART:begin      
        {Get Flags Names}
        FlagNames:=UARTFlagsToFlagNames(Device.DeviceFlags);
        
        {Get UART}
        UARTDevice:=PUARTDevice(Device);
       
        AddBold(AResponse,'UART','');
        AddBlank(AResponse);
        AddItem(AResponse,'Type:',UARTTypeToString(Device.DeviceType));
        AddItem(AResponse,'Flags:',FlagNames.Strings[0]);
      
        {Check Flag Count}
        if FlagNames.Count > 1 then
         begin
          for Count:=1 to FlagNames.Count - 1 do
           begin
            {Add Flag Name}
            AddItem(AResponse,'',FlagNames.Strings[Count]);
           end;
         end;
       
        AddBlank(AResponse);
        AddItem(AResponse,'Id:',IntToStr(UARTDevice.UARTId));
        AddItem(AResponse,'Mode:',UARTModeToString(UARTDevice.UARTMode));
        AddItem(AResponse,'State:',UARTStateToString(UARTDevice.UARTState));
        {Get Status Names}
        StatusNames:=UARTStatusToStatusNames(UARTDevice.UARTStatus);
        AddItem(AResponse,'Status:',StatusNames.Strings[0]);
        
        {Check Status Count}
        if StatusNames.Count > 1 then
         begin
          for Count:=1 to StatusNames.Count - 1 do
           begin
            {Add Status Name}
            AddItem(AResponse,'',StatusNames.Strings[Count]);
           end;
         end;
        AddBlank(AResponse);
        AddItem(AResponse,'Receive Count:',IntToStr(UARTDevice.ReceiveCount));
        AddItem(AResponse,'Receive Errors:',IntToStr(UARTDevice.ReceiveErrors));
        AddItem(AResponse,'Transmit Count:',IntToStr(UARTDevice.TransmitCount));
        AddItem(AResponse,'Transmit Errors:',IntToStr(UARTDevice.TransmitErrors));
        AddBlank(AResponse);
        
        {Get Properties}
        if UARTDeviceGetProperties(UARTDevice,@UARTProperties) = ERROR_SUCCESS then
         begin
          AddItem(AResponse,'Min Rate:',IntToStr(UARTProperties.MinRate) + ' Baud');
          AddItem(AResponse,'Max Rate:',IntToStr(UARTProperties.MaxRate) + ' Baud');
          AddBlank(AResponse);
          AddItem(AResponse,'Baud Rate:',IntToStr(UARTProperties.BaudRate));
          AddItem(AResponse,'Data Bits:',SerialDataBitsToString(UARTProperties.DataBits)); 
          AddItem(AResponse,'Stop Bits:',SerialStopBitsToString(UARTProperties.StopBits));
          AddItem(AResponse,'Parity:',SerialParityToString(UARTProperties.Parity));
          AddItem(AResponse,'Flow Control:',SerialFlowControlToString(UARTProperties.FlowControl));
         end;
        
        StatusNames.Free;
        FlagNames.Free;
       end;
      DEVICE_CLASS_SERIAL:begin      
        {Get Flags Names}
        FlagNames:=SerialFlagsToFlagNames(Device.DeviceFlags);
        
        {Get Serial}
        SerialDevice:=PSerialDevice(Device);
       
        AddBold(AResponse,'Serial','');
        AddBlank(AResponse);
        AddItem(AResponse,'Type:',SerialTypeToString(Device.DeviceType));
        AddItem(AResponse,'Flags:',FlagNames.Strings[0]);
      
        {Check Flag Count}
        if FlagNames.Count > 1 then
         begin
          for Count:=1 to FlagNames.Count - 1 do
           begin
            {Add Flag Name}
            AddItem(AResponse,'',FlagNames.Strings[Count]);
           end;
         end;
       
        AddBlank(AResponse);
        AddItem(AResponse,'Id:',IntToStr(SerialDevice.SerialId));
        AddItem(AResponse,'State:',SerialStateToString(SerialDevice.SerialState));
        {Get Status Names}
        StatusNames:=SerialStatusToStatusNames(SerialDevice.SerialStatus);
        AddItem(AResponse,'Status:',StatusNames.Strings[0]);
        
        {Check Status Count}
        if StatusNames.Count > 1 then
         begin
          for Count:=1 to StatusNames.Count - 1 do
           begin
            {Add Status Name}
            AddItem(AResponse,'',StatusNames.Strings[Count]);
           end;
         end;
        AddBlank(AResponse);
        AddItem(AResponse,'Receive Count:',IntToStr(SerialDevice.ReceiveCount));
        AddItem(AResponse,'Receive Errors:',IntToStr(SerialDevice.ReceiveErrors));
        AddItem(AResponse,'ReceiveOverruns:',IntToStr(SerialDevice.ReceiveOverruns));
        AddItem(AResponse,'Transmit Count:',IntToStr(SerialDevice.TransmitCount));
        AddItem(AResponse,'Transmit Errors:',IntToStr(SerialDevice.TransmitErrors));
        AddItem(AResponse,'TransmitOverruns:',IntToStr(SerialDevice.TransmitOverruns));
        AddBlank(AResponse);
      
        {Get Properties}
        if SerialDeviceGetProperties(SerialDevice,@SerialProperties) = ERROR_SUCCESS then
         begin
          AddItem(AResponse,'Min Rate:',IntToStr(SerialProperties.MinRate) + ' Baud');
          AddItem(AResponse,'Max Rate:',IntToStr(SerialProperties.MaxRate) + ' Baud');
          AddBlank(AResponse);
          AddItem(AResponse,'Baud Rate:',IntToStr(SerialProperties.BaudRate));
          AddItem(AResponse,'Data Bits:',SerialDataBitsToString(SerialProperties.DataBits)); 
          AddItem(AResponse,'Stop Bits:',SerialStopBitsToString(SerialProperties.StopBits));
          AddItem(AResponse,'Parity:',SerialParityToString(SerialProperties.Parity));
          AddItem(AResponse,'Flow Control:',SerialFlowControlToString(SerialProperties.FlowControl));
          AddBlank(AResponse);
          AddItem(AResponse,'Receive Depth:',IntToStr(SerialProperties.ReceiveDepth) + ' Bytes');
          AddItem(AResponse,'Transmit Depth:',IntToStr(SerialProperties.TransmitDepth) + ' Bytes');
         end;
         
        StatusNames.Free;
        FlagNames.Free;
       end;
      DEVICE_CLASS_I2C:begin      
        {Get Flags Names}
        FlagNames:=I2CFlagsToFlagNames(Device.DeviceFlags);
        
        {Get I2C}
        I2CDevice:=PI2CDevice(Device);
       
        AddBold(AResponse,'I2C','');
        AddBlank(AResponse);
        AddItem(AResponse,'Type:',I2CTypeToString(Device.DeviceType));
        AddItem(AResponse,'Flags:',FlagNames.Strings[0]);
      
        {Check Flag Count}
        if FlagNames.Count > 1 then
         begin
          for Count:=1 to FlagNames.Count - 1 do
           begin
            {Add Flag Name}
            AddItem(AResponse,'',FlagNames.Strings[Count]);
           end;
         end;
       
        AddBlank(AResponse);
        AddItem(AResponse,'Id:',IntToStr(I2CDevice.I2CId));
        AddItem(AResponse,'State:',I2CStateToString(I2CDevice.I2CState));
        AddBlank(AResponse);
        AddItem(AResponse,'Read Count:',IntToStr(I2CDevice.ReadCount));
        AddItem(AResponse,'Write Count:',IntToStr(I2CDevice.WriteCount));
        AddItem(AResponse,'Read Errors:',IntToStr(I2CDevice.ReadErrors));
        AddItem(AResponse,'Write Errors:',IntToStr(I2CDevice.WriteErrors));
        AddBlank(AResponse);
      
        {Get Properties}
        if I2CDeviceGetProperties(I2CDevice,@I2CProperties) = ERROR_SUCCESS then
         begin
          AddItem(AResponse,'Max Size:',IntToStr(I2CProperties.MaxSize) + ' Bytes');
          AddItem(AResponse,'Min Clock:',IntToStr(I2CProperties.MinClock) + ' Hz');
          AddItem(AResponse,'Max Clock:',IntToStr(I2CProperties.MaxClock) + ' Hz');
          AddBlank(AResponse);
          AddItem(AResponse,'Clock Rate:',IntToStr(I2CProperties.ClockRate) + ' Hz');
          AddItem(AResponse,'Slave Address:','0x' + IntToHex(I2CProperties.SlaveAddress,4));
         end;
      
        FlagNames.Free;
       end;
      DEVICE_CLASS_SPI:begin      
        {Get Flags Names}
        FlagNames:=SPIFlagsToFlagNames(Device.DeviceFlags);
        
        {Get SPI}
        SPIDevice:=PSPIDevice(Device);
       
        AddBold(AResponse,'SPI','');
        AddBlank(AResponse);
        AddItem(AResponse,'Type:',SPITypeToString(Device.DeviceType));
        AddItem(AResponse,'Flags:',FlagNames.Strings[0]);
      
        {Check Flag Count}
        if FlagNames.Count > 1 then
         begin
          for Count:=1 to FlagNames.Count - 1 do
           begin
            {Add Flag Name}
            AddItem(AResponse,'',FlagNames.Strings[Count]);
           end;
         end;
       
        AddBlank(AResponse);
        AddItem(AResponse,'Id:',IntToStr(SPIDevice.SPIId));
        AddItem(AResponse,'State:',SPIStateToString(SPIDevice.SPIState));
        AddBlank(AResponse);
        AddItem(AResponse,'Transfer Count:',IntToStr(SPIDevice.TransferCount));
        AddItem(AResponse,'Transfer Errors:',IntToStr(SPIDevice.TransferErrors));
        AddBlank(AResponse);
      
        {Get Properties}
        if SPIDeviceGetProperties(SPIDevice,@SPIProperties) = ERROR_SUCCESS then
         begin
          AddItem(AResponse,'Max Size:',IntToStr(SPIProperties.MaxSize) + ' Bytes');
          AddItem(AResponse,'Min Clock:',IntToStr(SPIProperties.MinClock) + ' Hz');
          AddItem(AResponse,'Max Clock:',IntToStr(SPIProperties.MaxClock) + ' Hz');
          AddItem(AResponse,'Select Count:',IntToStr(SPIProperties.SelectCount));
          AddBlank(AResponse);
          AddItem(AResponse,'Mode:',SPIModeToString(SPIProperties.Mode));
          AddItem(AResponse,'Clock Rate:',IntToStr(SPIProperties.ClockRate) + ' Hz');
          AddItem(AResponse,'Clock Phase:',SPIClockPhaseToString(SPIProperties.ClockPhase));
          AddItem(AResponse,'Clock Polarity:',SPIClockPolarityToString(SPIProperties.ClockPolarity));
          AddItem(AResponse,'Select Polarity:',SPISelectPolarityToString(SPIProperties.SelectPolarity));
          AddItem(AResponse,'Byte Delay:',IntToStr(SPIProperties.ByteDelay) + ' uS');
         end;
        
        FlagNames.Free;
       end;
      DEVICE_CLASS_PWM:begin      
        {Get Flags Names}
        FlagNames:=PWMFlagsToFlagNames(Device.DeviceFlags);
        
        {Get PWM}
        PWMDevice:=PPWMDevice(Device);
       
        AddBold(AResponse,'PWM','');
        AddBlank(AResponse);
        AddItem(AResponse,'Type:',PWMTypeToString(Device.DeviceType));
        AddItem(AResponse,'Flags:',FlagNames.Strings[0]);
      
        {Check Flag Count}
        if FlagNames.Count > 1 then
         begin
          for Count:=1 to FlagNames.Count - 1 do
           begin
            {Add Flag Name}
            AddItem(AResponse,'',FlagNames.Strings[Count]);
           end;
         end;
       
        AddBlank(AResponse);
        AddItem(AResponse,'Id:',IntToStr(PWMDevice.PWMId));
        AddItem(AResponse,'State:',PWMStateToString(PWMDevice.PWMState));
        AddBlank(AResponse);
        AddItem(AResponse,'Get Count:',IntToStr(PWMDevice.GetCount));
        AddItem(AResponse,'Set Count:',IntToStr(PWMDevice.SetCount));
        AddItem(AResponse,'Write Count:',IntToStr(PWMDevice.WriteCount));
        AddItem(AResponse,'Config Count:',IntToStr(PWMDevice.ConfigCount));
        AddBlank(AResponse);
      
        {Get Properties}
        if PWMDeviceGetProperties(PWMDevice,@PWMProperties) = ERROR_SUCCESS then
         begin
          AddItem(AResponse,'GPIO:',GPIOPinToString(PWMProperties.GPIO));
          AddBlank(AResponse);
          AddItem(AResponse,'Mode:',PWMModeToString(PWMProperties.Mode));
          AddItem(AResponse,'Range:',IntToStr(PWMProperties.Range));
          AddItem(AResponse,'Frequency:',IntToStr(PWMProperties.Frequency) + ' Hz');
          AddItem(AResponse,'Polarity:',PWMPolarityToString(PWMProperties.Polarity));
          AddItem(AResponse,'Duty:',IntToStr(PWMProperties.DutyNS) + ' ns');
          AddItem(AResponse,'Period:',IntToStr(PWMProperties.PeriodNS) + ' ns');
          AddBlank(AResponse);
          AddItem(AResponse,'MinPeriod:',IntToStr(PWMProperties.MinPeriod) + ' ns');
         end;

        FlagNames.Free;
       end;
      DEVICE_CLASS_DMA:begin      
        {Get Flags Names}
        FlagNames:=DMAFlagsToFlagNames(Device.DeviceFlags);
        
        {Get DMA}
        DMAHost:=PDMAHost(Device);
       
        AddBold(AResponse,'DMA','');
        AddBlank(AResponse);
        AddItem(AResponse,'Type:',DMATypeToString(Device.DeviceType));
        AddItem(AResponse,'Flags:',FlagNames.Strings[0]);
      
        {Check Flag Count}
        if FlagNames.Count > 1 then
         begin
          for Count:=1 to FlagNames.Count - 1 do
           begin
            {Add Flag Name}
            AddItem(AResponse,'',FlagNames.Strings[Count]);
           end;
         end;
       
        AddBlank(AResponse);
        AddItem(AResponse,'Id:',IntToStr(DMAHost.DMAId));
        AddItem(AResponse,'State:',DMAStateToString(DMAHost.DMAState));
        AddBlank(AResponse);
        AddItem(AResponse,'Request Count:',IntToStr(DMAHost.RequestCount));
        AddItem(AResponse,'Request Errors:',IntToStr(DMAHost.RequestErrors));
        AddBlank(AResponse);
      
        {Get Properties}
        if DMAHostProperties(DMAHost,@DMAProperties) = ERROR_SUCCESS then
         begin
          AddItem(AResponse,'Alignment:',IntToStr(DMAProperties.Alignment) + ' Bytes');
          AddItem(AResponse,'Multiplier:',IntToStr(DMAProperties.Multiplier) + ' Bytes');
          AddBlank(AResponse);
          AddItem(AResponse,'Channels:',IntToStr(DMAProperties.Channels));
          AddItem(AResponse,'MaxSize:',IntToStr(DMAProperties.MaxSize) + ' Bytes');
          AddItem(AResponse,'MaxCount:',IntToStr(DMAProperties.MaxCount));
          AddItem(AResponse,'MaxLength:',IntToStr(DMAProperties.MaxLength) + ' Bytes');
          AddItem(AResponse,'MinStride:',IntToStr(DMAProperties.MinStride) + ' Bytes');
          AddItem(AResponse,'MaxStride:',IntToStr(DMAProperties.MaxStride) + ' Bytes');
         end;
       
        FlagNames.Free;
       end;
      DEVICE_CLASS_NETWORK:begin      
        {Get Flags Names}
        FlagNames:=NetworkFlagsToFlagNames(Device.DeviceFlags);
        
        {Get Network}
        NetworkDevice:=PNetworkDevice(Device);
       
        AddBold(AResponse,'Network','');
        AddBlank(AResponse);
        AddItem(AResponse,'Type:',NetworkDeviceTypeToString(Device.DeviceType));
        AddItem(AResponse,'Flags:',FlagNames.Strings[0]);
      
        {Check Flag Count}
        if FlagNames.Count > 1 then
         begin
          for Count:=1 to FlagNames.Count - 1 do
           begin
            {Add Flag Name}
            AddItem(AResponse,'',FlagNames.Strings[Count]);
           end;
         end;
       
        AddBlank(AResponse);
        AddItem(AResponse,'Id:',IntToStr(NetworkDevice.NetworkId));
        AddItem(AResponse,'State:',NetworkDeviceStateToString(NetworkDevice.NetworkState));
        AddItem(AResponse,'Status:',NetworkDeviceStatusToString(NetworkDevice.NetworkStatus));
        AddBlank(AResponse);
        AddItem(AResponse,'Receive Bytes:',IntToStr(NetworkDevice.ReceiveBytes));
        AddItem(AResponse,'Receive Count:',IntToStr(NetworkDevice.ReceiveCount));
        AddItem(AResponse,'Receive Errors:',IntToStr(NetworkDevice.ReceiveErrors));
        AddItem(AResponse,'Transmit Bytes:',IntToStr(NetworkDevice.TransmitBytes));
        AddItem(AResponse,'Transmit Count:',IntToStr(NetworkDevice.TransmitCount));
        AddItem(AResponse,'Transmit Errors:',IntToStr(NetworkDevice.TransmitErrors));
        AddItem(AResponse,'Status Count:',IntToStr(NetworkDevice.StatusCount));
        AddItem(AResponse,'Status Errors:',IntToStr(NetworkDevice.StatusErrors));
        AddItem(AResponse,'Buffer Overruns:',IntToStr(NetworkDevice.BufferOverruns));
        AddItem(AResponse,'Buffer Unavailable:',IntToStr(NetworkDevice.BufferUnavailable));
        AddBlank(AResponse);
       
        FlagNames.Free;
       end;
      DEVICE_CLASS_STORAGE:begin      
        {Get Flags Names}
        FlagNames:=StorageFlagsToFlagNames(Device.DeviceFlags);
        
        {Get Storage}
        StorageDevice:=PStorageDevice(Device);
       
        AddBold(AResponse,'Storage','');
        AddBlank(AResponse);
        AddItem(AResponse,'Type:',StorageDeviceTypeToString(Device.DeviceType));
        AddItem(AResponse,'Flags:',FlagNames.Strings[0]);
      
        {Check Flag Count}
        if FlagNames.Count > 1 then
         begin
          for Count:=1 to FlagNames.Count - 1 do
           begin
            {Add Flag Name}
            AddItem(AResponse,'',FlagNames.Strings[Count]);
           end;
         end;
       
        AddBlank(AResponse);
        AddItem(AResponse,'Id:',IntToStr(StorageDevice.StorageId));
        AddItem(AResponse,'State:',StorageDeviceStateToString(StorageDevice.StorageState));
        AddBlank(AResponse);
        AddItem(AResponse,'Read Count:',IntToStr(StorageDevice.ReadCount));
        AddItem(AResponse,'Read Errors:',IntToStr(StorageDevice.ReadErrors));
        AddItem(AResponse,'Write Count:',IntToStr(StorageDevice.WriteCount));
        AddItem(AResponse,'Write Errors:',IntToStr(StorageDevice.WriteErrors));
        AddItem(AResponse,'Erase Count:',IntToStr(StorageDevice.EraseCount));
        AddItem(AResponse,'Erase Errors:',IntToStr(StorageDevice.EraseErrors));
        AddBlank(AResponse);
        AddItem(AResponse,'Target ID:',IntToStr(StorageDevice.TargetID));
        AddItem(AResponse,'Target LUN:',IntToStr(StorageDevice.TargetLUN));
        AddItem(AResponse,'Block Size:',IntToStr(StorageDevice.BlockSize) + ' Bytes');
        AddItem(AResponse,'Block Count:',IntToStr(StorageDevice.BlockCount));
        AddItem(AResponse,'Block Shift:',IntToStr(StorageDevice.BlockShift));
        AddItem(AResponse,'Vendor:',String(StorageDevice.Vendor));
        AddItem(AResponse,'Product:',String(StorageDevice.Product));
        AddItem(AResponse,'Revision:',String(StorageDevice.Revision));
        AddBlank(AResponse);
        
        FlagNames.Free;
       end;
      DEVICE_CLASS_KEYBOARD:begin      
        {Get Flags Names}
        FlagNames:=KeyboardFlagsToFlagNames(Device.DeviceFlags);
        
        {Get Keyboard}
        KeyboardDevice:=PKeyboardDevice(Device);
       
        AddBold(AResponse,'Keyboard','');
        AddBlank(AResponse);
        AddItem(AResponse,'Type:',KeyboardDeviceTypeToString(Device.DeviceType));
        AddItem(AResponse,'Flags:',FlagNames.Strings[0]);
      
        {Check Flag Count}
        if FlagNames.Count > 1 then
         begin
          for Count:=1 to FlagNames.Count - 1 do
           begin
            {Add Flag Name}
            AddItem(AResponse,'',FlagNames.Strings[Count]);
           end;
         end;
       
        AddBlank(AResponse);
        AddItem(AResponse,'Id:',IntToStr(KeyboardDevice.KeyboardId));
        AddItem(AResponse,'State:',KeyboardDeviceStateToString(KeyboardDevice.KeyboardState));
        AddBlank(AResponse);
        AddItem(AResponse,'Receive Count:',IntToStr(KeyboardDevice.ReceiveCount));
        AddItem(AResponse,'Receive Errors:',IntToStr(KeyboardDevice.ReceiveErrors));
        AddItem(AResponse,'Buffer Overruns:',IntToStr(KeyboardDevice.BufferOverruns));
        AddBlank(AResponse);
        
        FlagNames.Free;
       end;
      DEVICE_CLASS_MOUSE:begin
        {Get Flags Names}
        FlagNames:=MouseFlagsToFlagNames(Device.DeviceFlags);
        
        {Get Mouse}
        MouseDevice:=PMouseDevice(Device);
       
        AddBold(AResponse,'Mouse','');
        AddBlank(AResponse);
        AddItem(AResponse,'Type:',MouseDeviceTypeToString(Device.DeviceType));
        AddItem(AResponse,'Flags:',FlagNames.Strings[0]);
      
        {Check Flag Count}
        if FlagNames.Count > 1 then
         begin
          for Count:=1 to FlagNames.Count - 1 do
           begin
            {Add Flag Name}
            AddItem(AResponse,'',FlagNames.Strings[Count]);
           end;
         end;
       
        AddBlank(AResponse);
        AddItem(AResponse,'Id:',IntToStr(MouseDevice.MouseId));
        AddItem(AResponse,'State:',MouseDeviceStateToString(MouseDevice.MouseState));
        AddBlank(AResponse);
        AddItem(AResponse,'Receive Count:',IntToStr(MouseDevice.ReceiveCount));
        AddItem(AResponse,'Receive Errors:',IntToStr(MouseDevice.ReceiveErrors));
        AddItem(AResponse,'Buffer Overruns:',IntToStr(MouseDevice.BufferOverruns));
        AddBlank(AResponse);
        
        FlagNames.Free;
       end;
      DEVICE_CLASS_TOUCH:begin
        {Get Flags Names}
        FlagNames:=TouchFlagsToFlagNames(Device.DeviceFlags);
        
        {Get Touch}
        TouchDevice:=PTouchDevice(Device);
       
        AddBold(AResponse,'Touch','');
        AddBlank(AResponse);
        AddItem(AResponse,'Type:',TouchDeviceTypeToString(Device.DeviceType));
        AddItem(AResponse,'Flags:',FlagNames.Strings[0]);
      
        {Check Flag Count}
        if FlagNames.Count > 1 then
         begin
          for Count:=1 to FlagNames.Count - 1 do
           begin
            {Add Flag Name}
            AddItem(AResponse,'',FlagNames.Strings[Count]);
           end;
         end;
       
        AddBlank(AResponse);
        AddItem(AResponse,'Id:',IntToStr(TouchDevice.TouchId));
        AddItem(AResponse,'State:',TouchDeviceStateToString(TouchDevice.TouchState));
        AddBlank(AResponse);
        AddItem(AResponse,'Receive Count:',IntToStr(TouchDevice.ReceiveCount));
        AddItem(AResponse,'Receive Errors:',IntToStr(TouchDevice.ReceiveErrors));
        AddItem(AResponse,'Buffer Overruns:',IntToStr(TouchDevice.BufferOverruns));
        AddBlank(AResponse);
        
        FlagNames.Free;
       end;
      DEVICE_CLASS_RTC:begin
        {Get Flags Names}
        FlagNames:=RTCFlagsToFlagNames(Device.DeviceFlags);
        
        {Get RTC}
        RTCDevice:=PRTCDevice(Device);
       
        AddBold(AResponse,'RTC','');
        AddBlank(AResponse);
        AddItem(AResponse,'Type:',RTCDeviceTypeToString(Device.DeviceType));
        AddItem(AResponse,'Flags:',FlagNames.Strings[0]);
      
        {Check Flag Count}
        if FlagNames.Count > 1 then
         begin
          for Count:=1 to FlagNames.Count - 1 do
           begin
            {Add Flag Name}
            AddItem(AResponse,'',FlagNames.Strings[Count]);
           end;
         end;
       
        AddBlank(AResponse);
        AddItem(AResponse,'Id:',IntToStr(RTCDevice.RTCId));
        AddItem(AResponse,'State:',RTCDeviceStateToString(RTCDevice.RTCState));
        AddBlank(AResponse);
        AddItem(AResponse,'Min Time:',DateTimeToStr(SystemFileTimeToDateTime(TFileTime(RTCDevice.Properties.MinTime))));
        AddItem(AResponse,'Max Time:',DateTimeToStr(SystemFileTimeToDateTime(TFileTime(RTCDevice.Properties.MaxTime))));
        AddItem(AResponse,'Alarm Count:',IntToStr(RTCDevice.Properties.AlarmCount));
        AddBlank(AResponse);
        AddItem(AResponse,'Get Count:',IntToStr(RTCDevice.GetCount));
        AddItem(AResponse,'Set Count:',IntToStr(RTCDevice.SetCount));
        AddBlank(AResponse);
        
        FlagNames.Free;
       end;
      DEVICE_CLASS_MMC,DEVICE_CLASS_SD:begin
        {Get Flags Names}
        FlagNames:=MMCFlagsToFlagNames(Device.DeviceFlags);
      
        {Get MMC}
        MMCDevice:=PMMCDevice(Device);
       
        AddBold(AResponse,'MMC','');
        AddBlank(AResponse);
        AddItem(AResponse,'Type:',MMCDeviceTypeToString(Device.DeviceType));
        AddItem(AResponse,'Flags:',FlagNames.Strings[0]);
        
        {Check Flag Count}
        if FlagNames.Count > 1 then
         begin
          for Count:=1 to FlagNames.Count - 1 do
           begin
            {Add Flag Name}
            AddItem(AResponse,'',FlagNames.Strings[Count]);
           end;
         end;

        AddBlank(AResponse);
        AddItem(AResponse,'Id:',IntToStr(MMCDevice.MMCId));
        AddItem(AResponse,'State:',MMCDeviceStateToString(MMCDevice.MMCState));
        AddBlank(AResponse);
        AddItem(AResponse,'Version:',MMCVersionToString(MMCDevice.Version));
        AddItem(AResponse,'Clock:',IntToStr(MMCDevice.Clock));
        AddItem(AResponse,'Timing:',MMCTimingToString(MMCDevice.Timing));
        WorkBuffer:=MMCBusWidthToString(MMCDevice.BusWidth);
        if MMCIsSD(MMCDevice) then WorkBuffer:=SDBusWidthToString(MMCDevice.BusWidth);
        AddItem(AResponse,'Bus Width:',WorkBuffer);
        AddBlank(AResponse);
     
        FlagNames.Free;
       end;
      DEVICE_CLASS_SDHCI:begin
        {Get Flags Names}
        FlagNames:=SDHCIFlagsToFlagNames(Device.DeviceFlags);
      
        {Get SDHCI}
        SDHCIHost:=PSDHCIHost(Device);
       
        AddBold(AResponse,'SDHCI','');
        AddBlank(AResponse);
        AddItem(AResponse,'Type:',SDHCIHostTypeToString(Device.DeviceType));
        AddItem(AResponse,'Flags:',FlagNames.Strings[0]);
      
        {Check Flag Count}
        if FlagNames.Count > 1 then
         begin
          for Count:=1 to FlagNames.Count - 1 do
           begin
            {Add Flag Name}
            AddItem(AResponse,'',FlagNames.Strings[Count]);
           end;
         end;
        
        AddBlank(AResponse);
        AddItem(AResponse,'Id:',IntToStr(SDHCIHost.SDHCIId));
        AddItem(AResponse,'State:',SDHCIHostStateToString(SDHCIHost.SDHCIState));
        AddBlank(AResponse);
        AddItem(AResponse,'Version:',SDHCIVersionToString(SDHCIGetVersion(SDHCIHost)));
        AddItem(AResponse,'Clock:',IntToStr(SDHCIHost.Clock));
        AddItem(AResponse,'Power:',SDHCIPowerToString(SDHCIHost.Power));
        AddItem(AResponse,'Timing:',MMCTimingToString(SDHCIHost.Timing));
        AddItem(AResponse,'Bus Width:',MMCBusWidthToString(SDHCIHost.BusWidth));
        AddBlank(AResponse);
        AddItem(AResponse,'Clock Minimum:',IntToStr(SDHCIHost.ClockMinimum));
        AddItem(AResponse,'Clock Maximum:',IntToStr(SDHCIHost.ClockMaximum));
        AddItem(AResponse,'Minimum Frequency:',IntToStr(SDHCIHost.MinimumFrequency));
        AddItem(AResponse,'Maximum Frequency:',IntToStr(SDHCIHost.MaximumFrequency));
        AddBlank(AResponse);
        AddItem(AResponse,'Maximum Block Size:',IntToStr(SDHCIHost.MaximumBlockSize));
        AddItem(AResponse,'Maximum Block Count:',IntToStr(SDHCIHost.MaximumBlockCount));
        AddItem(AResponse,'Maximum Request Size:',IntToStr(SDHCIHost.MaximumRequestSize));
        AddItem(AResponse,'Minimum DMA Size:',IntToStr(SDHCIHost.MinimumDMASize));
        AddItem(AResponse,'Maximum PIO Blocks:',IntToStr(SDHCIHost.MaximumPIOBlocks));
        AddBlank(AResponse);
        AddItem(AResponse,'DMA Slave:',IntToStr(SDHCIHost.DMASlave));
        AddItem(AResponse,'SDMA Boundary:',IntToStr(SDHCIHost.SDMABoundary));
        AddItem(AResponse,'ADMA Table Size:',IntToStr(SDHCIHost.ADMATableSize));
        AddItem(AResponse,'ADMA Table Count:',IntToStr(SDHCIHost.ADMATableCount));
        AddItem(AResponse,'ADMA Buffer Size:',IntToStr(SDHCIHost.ADMABufferSize));
        AddItem(AResponse,'ADMA Descriptor Size:',IntToStr(SDHCIHost.ADMADescriptorSize));
        AddBlank(AResponse);
        AddItem(AResponse,'Request Count:',IntToStr(SDHCIHost.RequestCount));
        AddItem(AResponse,'Request Errors:',IntToStr(SDHCIHost.RequestErrors));
        AddItem(AResponse,'Data Request Count:',IntToStr(SDHCIHost.DataRequestCount));
        AddItem(AResponse,'Command Request Count:',IntToStr(SDHCIHost.CommandRequestCount));
        AddItem(AResponse,'PIO Data Transfer Count:',IntToStr(SDHCIHost.PIODataTransferCount));
        AddItem(AResponse,'DMA Data Transfer Count:',IntToStr(SDHCIHost.DMADataTransferCount));
        AddBlank(AResponse);
        AddItem(AResponse,'Interrupt Count:',IntToStr(SDHCIHost.InterruptCount));
        AddItem(AResponse,'Data Interrupt Count:',IntToStr(SDHCIHost.DataInterruptCount));
        AddItem(AResponse,'Command Interrupt Count:',IntToStr(SDHCIHost.CommandInterruptCount));
      
        FlagNames.Free;
       end;
      DEVICE_CLASS_USB:begin
        {Get Flags Names}
        FlagNames:=USBFlagsToFlagNames(Device.DeviceFlags);
      
        {Get USB}
        USBDevice:=PUSBDevice(Device);
       
        AddBold(AResponse,'USB','');
        AddBlank(AResponse);
        AddItem(AResponse,'Type:',USBDeviceTypeToString(Device.DeviceType));
        AddItem(AResponse,'Flags:',FlagNames.Strings[0]);
      
        {Check Flag Count}
        if FlagNames.Count > 1 then
         begin
          for Count:=1 to FlagNames.Count - 1 do
           begin
            {Add Flag Name}
            AddItem(AResponse,'',FlagNames.Strings[Count]);
           end;
         end;
       
        AddBlank(AResponse);
        AddItem(AResponse,'Id:',IntToStr(USBDevice.USBId));
        AddItem(AResponse,'State:',USBDeviceStateToString(USBDevice.USBState));
        AddItem(AResponse,'Status:',USBDeviceStatusToString(USBDevice.USBStatus));
        AddBlank(AResponse);
        AddItem(AResponse,'Address:',IntToStr(USBDevice.Address));
        AddItem(AResponse,'Speed:',USBSpeedToStringAlt(USBDevice.Speed));
        AddItem(AResponse,'Depth:',IntToStr(USBDevice.Depth));
        AddItem(AResponse,'Port Number:',IntToStr(USBDevice.PortNumber));
        AddItem(AResponse,'Configuration Value:',IntToStr(USBDevice.ConfigurationValue));
        AddBlank(AResponse);
        
        WorkBuffer:='';
        if USBDevice.Parent <> nil then WorkBuffer:=DeviceGetName(@USBDevice.Parent.Device);
        AddItem(AResponse,'Parent:',WorkBuffer);
        
        WorkBuffer:='';
        if USBDevice.Driver <> nil then WorkBuffer:=DriverGetName(@USBDevice.Driver.Driver);
        
        AddItem(AResponse,'Driver:',WorkBuffer);
        AddBlank(AResponse);
        AddItem(AResponse,'Product:',USBDevice.Product);
        AddItem(AResponse,'Manufacturer:',USBDevice.Manufacturer);
        AddItem(AResponse,'SerialNumber:',USBDevice.SerialNumber);
        AddBlank(AResponse);
        AddItem(AResponse,'Request Count:',IntToStr(USBDevice.RequestCount));
        AddItem(AResponse,'Request Errors:',IntToStr(USBDevice.RequestErrors));
        AddBlank(AResponse);
        AddItem(AResponse,'Class:',USBClassCodeToString(USBDevice.Descriptor.bDeviceClass));
        AddItem(AResponse,'VID/PID:',IntToHex(USBDevice.Descriptor.idVendor,4) + ':' + IntToHex(USBDevice.Descriptor.idProduct,4));
        AddBlank(AResponse);
     
        FlagNames.Free;
       end;
      DEVICE_CLASS_USBHUB:begin
        
        //To Do //
        
       end;
      DEVICE_CLASS_USBHOST:begin
        {Get Flags Names}
        FlagNames:=USBHostFlagsToFlagNames(Device.DeviceFlags);
      
        {Get USBHost}
        USBHost:=PUSBHost(Device);
       
        AddBold(AResponse,'USB Host','');
        AddBlank(AResponse);
        AddItem(AResponse,'Type:',USBHostTypeToString(Device.DeviceType));
        AddItem(AResponse,'Flags:',FlagNames.Strings[0]);
      
        {Check Flag Count}
        if FlagNames.Count > 1 then
         begin
          for Count:=1 to FlagNames.Count - 1 do
           begin
            {Add Flag Name}
            AddItem(AResponse,'',FlagNames.Strings[Count]);
           end;
         end;
       
        AddBlank(AResponse);
        AddItem(AResponse,'Id:',IntToStr(USBHost.HostId));
        AddItem(AResponse,'State:',USBHostStateToString(USBHost.HostState));
        AddBlank(AResponse);
        AddItem(AResponse,'Alignment:',IntToStr(USBHost.Alignment));
        AddItem(AResponse,'Multiplier:',IntToStr(USBHost.Multiplier));
        AddItem(AResponse,'Max Transfer:',IntToStr(USBHost.MaxTransfer));
        AddBlank(AResponse);
        AddItem(AResponse,'Request Count:',IntToStr(USBHost.RequestCount));
        AddItem(AResponse,'Request Errors:',IntToStr(USBHost.RequestErrors));
        AddBlank(AResponse);
      
        FlagNames.Free;
       end;
      DEVICE_CLASS_PCI:begin
        {Get Flags Names}
        FlagNames:=PCIFlagsToFlagNames(Device.DeviceFlags);
      
        {Get PCI}
        PCIDevice:=PPCIDevice(Device);
       
        AddBold(AResponse,'PCI','');
        AddBlank(AResponse);
        AddItem(AResponse,'Type:',PCIDeviceTypeToString(Device.DeviceType));
        AddItem(AResponse,'Flags:',FlagNames.Strings[0]);
      
        {Check Flag Count}
        if FlagNames.Count > 1 then
         begin
          for Count:=1 to FlagNames.Count - 1 do
           begin
            {Add Flag Name}
            AddItem(AResponse,'',FlagNames.Strings[Count]);
           end;
         end;
       
        AddBlank(AResponse);
        AddItem(AResponse,'Id:',IntToStr(PCIDevice.PCIId));
        AddItem(AResponse,'State:',PCIDeviceStateToString(PCIDevice.PCIState));
        AddItem(AResponse,'Status:',PCIDeviceStatusToString(PCIDevice.PCIStatus));
        AddBlank(AResponse);
        
        //To Do //TestingPCI
        
        WorkBuffer:='';
        if PCIDevice.Parent <> nil then WorkBuffer:=DeviceGetName(@PCIDevice.Parent.Device);
        AddItem(AResponse,'Parent:',WorkBuffer);
   
        WorkBuffer:='';
        if PCIDevice.Driver <> nil then WorkBuffer:=DriverGetName(@PCIDevice.Driver.Driver);
        AddItem(AResponse,'Driver:',WorkBuffer);
   
        //To Do //TestingPCI
      
        FlagNames.Free;
       end;
      DEVICE_CLASS_PCIHOST:begin
        {Get Flags Names}
        FlagNames:=PCIHostFlagsToFlagNames(Device.DeviceFlags);
      
        {Get PCIHost}
        PCIHost:=PPCIHost(Device);
       
        AddBold(AResponse,'PCI Host','');
        AddBlank(AResponse);
        AddItem(AResponse,'Type:',PCIHostTypeToString(Device.DeviceType));
        AddItem(AResponse,'Flags:',FlagNames.Strings[0]);
      
        {Check Flag Count}
        if FlagNames.Count > 1 then
         begin
          for Count:=1 to FlagNames.Count - 1 do
           begin
            {Add Flag Name}
            AddItem(AResponse,'',FlagNames.Strings[Count]);
           end;
         end;
       
        AddBlank(AResponse);
        AddItem(AResponse,'Id:',IntToStr(PCIHost.HostId));
        AddItem(AResponse,'State:',PCIHostStateToString(PCIHost.HostState));

        //To Do //TestingPCI
       
        FlagNames.Free;
       end;
     end;
    end
   else
    begin
     AddItem(AResponse,'Not Found','');
    end;    
   
   {Add Footer}
   AddFooter(AResponse); 
  end
 else
  begin 
   {Add Header (4 column)}
   AddHeaderEx(AResponse,GetTitle,'',Self,4); 
  
   {Add Device List} 
   AddBold4Column(AResponse,'Device Id','Name','Class','Bus Type');
   AddBlankEx(AResponse,4);
   
   {Setup Data}
   Data.Document:=Self;
   Data.Host:=AHost;
   Data.Request:=ARequest;
   Data.Response:=AResponse;
   Data.Data:=nil;
   
   {Enumerate Devices}
   DeviceEnumerate(DEVICE_CLASS_ANY,WebStatusDeviceEnumerate,@Data);
   
   {Add Footer (4 column)}
   AddFooterEx(AResponse,4); 
  end;
  
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
 AddHeaderEx(AResponse,GetTitle,'',Self,4); 

 {Add Driver List} 
 AddBold4Column(AResponse,'Driver Id','Name','Class','State');
 AddBlankEx(AResponse,4);
 
 {Setup Data}
 Data.Document:=Self;
 Data.Host:=AHost;
 Data.Request:=ARequest;
 Data.Response:=AResponse;
 Data.Data:=nil;
 
 {Enumerate Drivers}
 DriverEnumerate(DRIVER_CLASS_ANY,WebStatusDriverEnumerate,@Data);
 
 {Add Footer (4 column)}
 AddFooterEx(AResponse,4); 
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TWebStatusHandles}
constructor TWebStatusHandles.Create(AMain:TWebStatusMain);
begin
 {}
 FCaption:='Handles'; {Must be before create for register}
 inherited Create(AMain);
 Name:='/handles';
 
 if FMain <> nil then Name:=FMain.Name + Name;
end; 

{==============================================================================}

function TWebStatusHandles.FlagsToFlagNames(AFlags:LongWord):TStringList;
begin
 {}
 Result:=TStringList.Create;
 
 {Check Flags}
 if (AFlags and HANDLE_FLAG_NAMED) = HANDLE_FLAG_NAMED then
  begin
   Result.Add('HANDLE_FLAG_NAMED');
  end;
 if (AFlags and HANDLE_FLAG_DUPLICATE) = HANDLE_FLAG_DUPLICATE then
  begin
   Result.Add('HANDLE_FLAG_DUPLICATE');
  end;

 {Check Flags}
 if Result.Count = 0 then
  begin
   Result.Add('HANDLE_FLAG_NONE');
  end; 
end;
  
{==============================================================================}

function TWebStatusHandles.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
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
 AddHeaderEx(AResponse,GetTitle,'',Self,5); 

 {Add Handle List} 
 AddBold5Column(AResponse,'Handle','Type','Name','Count','Flags');
 AddBlankEx(AResponse,5);
 
 {Setup Data}
 Data.Document:=Self;
 Data.Host:=AHost;
 Data.Request:=ARequest;
 Data.Response:=AResponse;
 Data.Data:=nil;
 
 {Enumerate Handles}
 HandleEnumerate(WebStatusHandleEnumerate,@Data);
 
 {Add Footer (5 column)}
 AddFooterEx(AResponse,5); 
 
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

function TWebStatusUSB.USBFlagsToFlagNames(AFlags:LongWord):TStringList;
begin
 {}
 Result:=TStringList.Create;

 {Check Flags}
 {Nothing}

 {Check Flags}
 if Result.Count = 0 then
  begin
   Result.Add('USB_FLAG_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusUSB.USBHostFlagsToFlagNames(AFlags:LongWord):TStringList;
begin
 {}
 Result:=TStringList.Create;

 {Check Flags}
 if (AFlags and USBHOST_FLAG_SHARED) = USBHOST_FLAG_SHARED then
  begin
   Result.Add('USBHOST_FLAG_SHARED');
  end;
 if (AFlags and USBHOST_FLAG_NOCACHE) = USBHOST_FLAG_NOCACHE then
  begin
   Result.Add('USBHOST_FLAG_NOCACHE');
  end;

 {Check Flags}
 if Result.Count = 0 then
  begin
   Result.Add('USBHOST_FLAG_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusUSB.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
var
 Id:LongWord;
 Action:String;
 Count:LongWord;
 WorkBuffer:String;
 Data:TWebStatusData;
 FlagNames:TStringList;
 
 USBHost:PUSBHost;
 USBDevice:PUSBDevice;
begin
 {}
 Result:=False;
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;

 {Get Action}
 Action:=Uppercase(ARequest.GetParam('ACTION'));

 {Get Id}
 WorkBuffer:=Uppercase(ARequest.GetParam('ID'));
 
 if (Action = 'USBDEVICE') and (Length(WorkBuffer) > 0) then
  begin
   {Add Header (2 column with Caption)}
   AddHeaderEx(AResponse,GetTitle,'USB Device Information',Self,2);

   {Get Id}
   Id:=StrToIntDef(WorkBuffer,0);
 
   {Get USB Device}
   USBDevice:=USBDeviceFind(Id);
   if USBDevice <> nil then
    begin
     {Get Flags Names}
     FlagNames:=USBFlagsToFlagNames(USBDevice.Device.DeviceFlags);
     
     AddBold(AResponse,'Device','');
     AddBlank(AResponse);
     AddItem(AResponse,'Name:',DeviceGetName(@USBDevice.Device));
     AddItem(AResponse,'Type:',USBDeviceTypeToString(USBDevice.Device.DeviceType));
     AddItem(AResponse,'Flags:',FlagNames.Strings[0]);
      
     {Check Flag Count}
     if FlagNames.Count > 1 then
      begin
       for Count:=1 to FlagNames.Count - 1 do
        begin
         {Add Flag Name}
         AddItem(AResponse,'',FlagNames.Strings[Count]);
        end;
      end;

     AddItem(AResponse,'Description:',USBDevice.Device.DeviceDescription);
     AddBlank(AResponse);
     AddItem(AResponse,'Id:',IntToStr(USBDevice.USBId));
     AddItem(AResponse,'State:',USBDeviceStateToString(USBDevice.USBState));
     AddItem(AResponse,'Status:',USBDeviceStatusToString(USBDevice.USBStatus));
     AddBlank(AResponse);
     AddItem(AResponse,'Address:',IntToStr(USBDevice.Address));
     AddItem(AResponse,'Speed:',USBSpeedToStringAlt(USBDevice.Speed));
     AddItem(AResponse,'Depth:',IntToStr(USBDevice.Depth));
     AddItem(AResponse,'Port Number:',IntToStr(USBDevice.PortNumber));
     AddItem(AResponse,'Configuration Value:',IntToStr(USBDevice.ConfigurationValue));
     AddBlank(AResponse);
     
     WorkBuffer:='';
     if USBDevice.Parent <> nil then WorkBuffer:=DeviceGetName(@USBDevice.Parent.Device);
     AddItem(AResponse,'Parent:',WorkBuffer);

     WorkBuffer:='';
     if USBDevice.Driver <> nil then WorkBuffer:=DriverGetName(@USBDevice.Driver.Driver);
     AddItem(AResponse,'Driver:',WorkBuffer);

     AddBlank(AResponse);
     AddItem(AResponse,'Product:',USBDevice.Product);
     AddItem(AResponse,'Manufacturer:',USBDevice.Manufacturer);
     AddItem(AResponse,'SerialNumber:',USBDevice.SerialNumber);
     AddBlank(AResponse);
     AddItem(AResponse,'Request Count:',IntToStr(USBDevice.RequestCount));
     AddItem(AResponse,'Request Errors:',IntToStr(USBDevice.RequestErrors));
     AddBlank(AResponse);
     AddItem(AResponse,'Class:',USBClassCodeToString(USBDevice.Descriptor.bDeviceClass));
     AddItem(AResponse,'VID/PID:',IntToHex(USBDevice.Descriptor.idVendor,4) + ':' + IntToHex(USBDevice.Descriptor.idProduct,4));
     AddBlank(AResponse);
    
     AddBold(AResponse,'Device Descriptors','');
     AddBlank(AResponse);
     
     {Setup Data}
     Data.Document:=Self;
     Data.Host:=AHost;
     Data.Request:=ARequest;
     Data.Response:=AResponse;
     Data.Data:=nil;
     
     {Display Device}
     USBLogDevicesEx(USBDevice,WebStatusUSBLogOutput,WebStatusUSBLogDeviceCallback,nil,@Data);
     
     FlagNames.Free;
    end
   else
    begin
     AddItem(AResponse,'Not Found','');
    end;    
   
   {Add Footer}
   AddFooter(AResponse); 
  end
 else if (Action = 'USBHOST') and (Length(WorkBuffer) > 0) then
  begin
   {Add Header (2 column with Caption)}
   AddHeaderEx(AResponse,GetTitle,'USB Host Information',Self,2);
  
   {Get Id}
   Id:=StrToIntDef(WorkBuffer,0);
 
   {Get USB Host}
   USBHost:=USBHostFind(Id);
   if USBHost <> nil then
    begin
     {Get Flags Names}
     FlagNames:=USBHostFlagsToFlagNames(USBHost.Device.DeviceFlags);
     
     AddBold(AResponse,'Host','');
     AddBlank(AResponse);
     AddItem(AResponse,'Name:',DeviceGetName(@USBHost.Device));
     AddItem(AResponse,'Type:',USBHostTypeToString(USBHost.Device.DeviceType));
     AddItem(AResponse,'Flags:',FlagNames.Strings[0]);
      
     {Check Flag Count}
     if FlagNames.Count > 1 then
      begin
       for Count:=1 to FlagNames.Count - 1 do
        begin
         {Add Flag Name}
         AddItem(AResponse,'',FlagNames.Strings[Count]);
        end;
      end;

     AddItem(AResponse,'Description:',USBHost.Device.DeviceDescription);
     AddBlank(AResponse);
     AddItem(AResponse,'Id:',IntToStr(USBHost.HostId));
     AddItem(AResponse,'State:',USBHostStateToString(USBHost.HostState));
     AddBlank(AResponse);
     AddItem(AResponse,'Alignment:',IntToStr(USBHost.Alignment));
     AddItem(AResponse,'Multiplier:',IntToStr(USBHost.Multiplier));
     AddItem(AResponse,'Max Transfer:',IntToStr(USBHost.MaxTransfer));
     AddBlank(AResponse);
     AddItem(AResponse,'Request Count:',IntToStr(USBHost.RequestCount));
     AddItem(AResponse,'Request Errors:',IntToStr(USBHost.RequestErrors));
     AddBlank(AResponse);
     
     AddBold(AResponse,'Device Tree','');
     AddBlank(AResponse);
     
     {Setup Data}
     Data.Document:=Self;
     Data.Host:=AHost;
     Data.Request:=ARequest;
     Data.Response:=AResponse;
     Data.Data:=USBHost;
     
     {Display Tree}
     USBLogDevicesEx(nil,WebStatusUSBLogOutput,nil,WebStatusUSBLogTreeCallback,@Data);

     FlagNames.Free;
    end
   else
    begin
     AddItem(AResponse,'Not Found','');
    end;    
   
   {Add Footer}
   AddFooter(AResponse); 
  end
 else
  begin 
   {Add Header (4 column)}
   AddHeaderEx(AResponse,GetTitle,'',Self,4); 
  
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
   Data.Data:=nil;
   
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
   Data.Data:=nil;
   
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
   Data.Data:=nil;
   
   {Enumerate USB Drivers}
   USBDriverEnumerate(WebStatusUSBDriverEnumerate,@Data);
   
   {Add Footer (4 column)}
   AddFooterEx(AResponse,4); 
  end; 
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TWebStatusPCI}
constructor TWebStatusPCI.Create(AMain:TWebStatusMain);
begin
 {}
 FCaption:='PCI'; {Must be before create for register}
 inherited Create(AMain);
 Name:='/pci';
 
 if FMain <> nil then Name:=FMain.Name + Name;
end; 

{==============================================================================}

function TWebStatusPCI.PCIFlagsToFlagNames(AFlags:LongWord):TStringList;
begin
 {}
 Result:=TStringList.Create;

 {Check Flags}
 {Nothing}

 {Check Flags}
 if Result.Count = 0 then
  begin
   Result.Add('PCI_FLAG_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusPCI.PCIHostFlagsToFlagNames(AFlags:LongWord):TStringList;
begin
 {}
 Result:=TStringList.Create;

 {Check Flags}
 if (AFlags and PCIHOST_FLAG_SHARED) = PCIHOST_FLAG_SHARED then
  begin
   Result.Add('PCIHOST_FLAG_SHARED');
  end;
 if (AFlags and PCIHOST_FLAG_NOCACHE) = PCIHOST_FLAG_NOCACHE then
  begin
   Result.Add('PCIHOST_FLAG_NOCACHE');
  end;

 {Check Flags}
 if Result.Count = 0 then
  begin
   Result.Add('PCIHOST_FLAG_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusPCI.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
var
 Id:LongWord;
 Action:String;
 Count:LongWord;
 WorkBuffer:String;
 Data:TWebStatusData;
 FlagNames:TStringList;
 
 PCIHost:PPCIHost;
 PCIDevice:PPCIDevice;
begin
 {}
 Result:=False;
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;

 {Get Action}
 Action:=Uppercase(ARequest.GetParam('ACTION'));

 {Get Id}
 WorkBuffer:=Uppercase(ARequest.GetParam('ID'));
 
 if (Action = 'PCIDEVICE') and (Length(WorkBuffer) > 0) then
  begin
   {Add Header (2 column with Caption)}
   AddHeaderEx(AResponse,GetTitle,'PCI Device Information',Self,2);

   {Get Id}
   Id:=StrToIntDef(WorkBuffer,0);
 
   {Get PCI Device}
   PCIDevice:=PCIDeviceFind(Id);
   if PCIDevice <> nil then
    begin
     {Get Flags Names}
     FlagNames:=PCIFlagsToFlagNames(PCIDevice.Device.DeviceFlags);
     
     AddBold(AResponse,'Device','');
     AddBlank(AResponse);
     AddItem(AResponse,'Name:',DeviceGetName(@PCIDevice.Device));
     AddItem(AResponse,'Type:',PCIDeviceTypeToString(PCIDevice.Device.DeviceType));
     AddItem(AResponse,'Flags:',FlagNames.Strings[0]);
      
     {Check Flag Count}
     if FlagNames.Count > 1 then
      begin
       for Count:=1 to FlagNames.Count - 1 do
        begin
         {Add Flag Name}
         AddItem(AResponse,'',FlagNames.Strings[Count]);
        end;
      end;

     AddItem(AResponse,'Description:',PCIDevice.Device.DeviceDescription);
     AddBlank(AResponse);
     AddItem(AResponse,'Id:',IntToStr(PCIDevice.PCIId));
     AddItem(AResponse,'State:',PCIDeviceStateToString(PCIDevice.PCIState));
     AddItem(AResponse,'Status:',PCIDeviceStatusToString(PCIDevice.PCIStatus));
     AddBlank(AResponse);
     
     //To Do //TestingPCI
     
     WorkBuffer:='';
     if PCIDevice.Parent <> nil then WorkBuffer:=DeviceGetName(@PCIDevice.Parent.Device);
     AddItem(AResponse,'Parent:',WorkBuffer);

     WorkBuffer:='';
     if PCIDevice.Driver <> nil then WorkBuffer:=DriverGetName(@PCIDevice.Driver.Driver);
     AddItem(AResponse,'Driver:',WorkBuffer);

     //To Do //TestingPCI
     
     FlagNames.Free;
    end
   else
    begin
     AddItem(AResponse,'Not Found','');
    end;    
   
   {Add Footer}
   AddFooter(AResponse); 
  end
 else if (Action = 'PCIHOST') and (Length(WorkBuffer) > 0) then
  begin
   {Add Header (2 column with Caption)}
   AddHeaderEx(AResponse,GetTitle,'PCI Host Information',Self,2);
  
   {Get Id}
   Id:=StrToIntDef(WorkBuffer,0);
 
   {Get PCI Host}
   PCIHost:=PCIHostFind(Id);
   if PCIHost <> nil then
    begin
     {Get Flags Names}
     FlagNames:=PCIHostFlagsToFlagNames(PCIHost.Device.DeviceFlags);
     
     AddBold(AResponse,'Host','');
     AddBlank(AResponse);
     AddItem(AResponse,'Name:',DeviceGetName(@PCIHost.Device));
     AddItem(AResponse,'Type:',PCIHostTypeToString(PCIHost.Device.DeviceType));
     AddItem(AResponse,'Flags:',FlagNames.Strings[0]);
      
     {Check Flag Count}
     if FlagNames.Count > 1 then
      begin
       for Count:=1 to FlagNames.Count - 1 do
        begin
         {Add Flag Name}
         AddItem(AResponse,'',FlagNames.Strings[Count]);
        end;
      end;

     AddItem(AResponse,'Description:',PCIHost.Device.DeviceDescription);
     AddBlank(AResponse);
     AddItem(AResponse,'Id:',IntToStr(PCIHost.HostId));
     AddItem(AResponse,'State:',PCIHostStateToString(PCIHost.HostState));
     
     //To Do //TestingPCI

     FlagNames.Free;
    end
   else
    begin
     AddItem(AResponse,'Not Found','');
    end;    
   
   {Add Footer}
   AddFooter(AResponse); 
  end
 else
  begin 
   {Add Header (4 column)}
   AddHeaderEx(AResponse,GetTitle,'',Self,4); 
  
   {Add PCI Device List} 
   AddBold4Column(AResponse,'Devices','','','');
   AddBlankEx(AResponse,4);
   AddBold4Column(AResponse,'PCI Id','Name','Class','Status');
   AddBlankEx(AResponse,4);
  
   {Setup Data}
   Data.Document:=Self;
   Data.Host:=AHost;
   Data.Request:=ARequest;
   Data.Response:=AResponse;
   Data.Data:=nil;
   
   {Enumerate PCI Devices}
   PCIDeviceEnumerate(WebStatusPCIDeviceEnumerate,@Data);
   
   {Add PCI Host List} 
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
   Data.Data:=nil;
   
   {Enumerate PCI Hosts}
   PCIHostEnumerate(WebStatusPCIHostEnumerate,@Data);
  
   {Add PCI Driver List} 
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
   Data.Data:=nil;
   
   {Enumerate PCI Drivers}
   PCIDriverEnumerate(WebStatusPCIDriverEnumerate,@Data);
   
   {Add Footer (4 column)}
   AddFooterEx(AResponse,4); 
  end; 
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TWebStatusMMC}
constructor TWebStatusMMC.Create(AMain:TWebStatusMain);
begin
 {}
 FCaption:='MMC / SD / SDIO'; {Must be before create for register}
 inherited Create(AMain);
 Name:='/mmc';
 
 if FMain <> nil then Name:=FMain.Name + Name;
end; 

{==============================================================================}

function TWebStatusMMC.MMCFlagsToFlagNames(AFlags:LongWord):TStringList;
begin
 {}
 Result:=TStringList.Create;

 {Check Flags}
 if (AFlags and MMC_FLAG_CARD_PRESENT) = MMC_FLAG_CARD_PRESENT then
  begin
   Result.Add('MMC_FLAG_CARD_PRESENT');
  end;
 if (AFlags and MMC_FLAG_WRITE_PROTECT) = MMC_FLAG_WRITE_PROTECT then
  begin
   Result.Add('MMC_FLAG_WRITE_PROTECT');
  end;
 if (AFlags and MMC_FLAG_HIGH_CAPACITY) = MMC_FLAG_HIGH_CAPACITY then
  begin
   Result.Add('MMC_FLAG_HIGH_CAPACITY');
  end;
 if (AFlags and MMC_FLAG_EXT_CAPACITY) = MMC_FLAG_EXT_CAPACITY then
  begin
   Result.Add('MMC_FLAG_EXT_CAPACITY');
  end;
 if (AFlags and MMC_FLAG_UHS_I) = MMC_FLAG_UHS_I then
  begin
   Result.Add('MMC_FLAG_UHS_I');
  end;
 if (AFlags and MMC_FLAG_UHS_II) = MMC_FLAG_UHS_II then
  begin
   Result.Add('MMC_FLAG_UHS_II');
  end;
 if (AFlags and MMC_FLAG_BLOCK_ADDRESSED) = MMC_FLAG_BLOCK_ADDRESSED then
  begin
   Result.Add('MMC_FLAG_BLOCK_ADDRESSED');
  end;
 if (AFlags and MMC_FLAG_AUTO_BLOCK_COUNT) = MMC_FLAG_AUTO_BLOCK_COUNT then
  begin
   Result.Add('MMC_FLAG_AUTO_BLOCK_COUNT');
  end;
 if (AFlags and MMC_FLAG_AUTO_COMMAND_STOP) = MMC_FLAG_AUTO_COMMAND_STOP then
  begin
   Result.Add('MMC_FLAG_AUTO_COMMAND_STOP');
  end;
 if (AFlags and MMC_FLAG_DDR_MODE) = MMC_FLAG_DDR_MODE then
  begin
   Result.Add('MMC_FLAG_DDR_MODE');
  end;
 if (AFlags and MMC_FLAG_NON_REMOVABLE) = MMC_FLAG_NON_REMOVABLE then
  begin
   Result.Add('MMC_FLAG_NON_REMOVABLE');
  end;
 if (AFlags and MMC_FLAG_SET_BLOCK_COUNT) = MMC_FLAG_SET_BLOCK_COUNT then
  begin
   Result.Add('MMC_FLAG_SET_BLOCK_COUNT');
  end;

 {Check Flags}
 if Result.Count = 0 then
  begin
   Result.Add('MMC_FLAG_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusMMC.SDHCIFlagsToFlagNames(AFlags:LongWord):TStringList;
begin
 {}
 Result:=TStringList.Create;

 {Check Flags}
 if (AFlags and SDHCI_FLAG_SDMA) = SDHCI_FLAG_SDMA then
  begin
   Result.Add('SDHCI_FLAG_SDMA');
  end;
 if (AFlags and SDHCI_FLAG_ADMA) = SDHCI_FLAG_ADMA then
  begin
   Result.Add('SDHCI_FLAG_ADMA');
  end;
 if (AFlags and SDHCI_FLAG_SPI) = SDHCI_FLAG_SPI then
  begin
   Result.Add('SDHCI_FLAG_SPI');
  end;
 if (AFlags and SDHCI_FLAG_CRC_ENABLE) = SDHCI_FLAG_CRC_ENABLE then
  begin
   Result.Add('SDHCI_FLAG_CRC_ENABLE');
  end;
 if (AFlags and SDHCI_FLAG_NON_STANDARD) = SDHCI_FLAG_NON_STANDARD then
  begin
   Result.Add('SDHCI_FLAG_NON_STANDARD');
  end;
 if (AFlags and SDHCI_FLAG_AUTO_CMD12) = SDHCI_FLAG_AUTO_CMD12 then
  begin
   Result.Add('SDHCI_FLAG_AUTO_CMD12');
  end;
 if (AFlags and SDHCI_FLAG_AUTO_CMD23) = SDHCI_FLAG_AUTO_CMD23 then
  begin
   Result.Add('SDHCI_FLAG_AUTO_CMD23');
  end;
 if (AFlags and SDHCI_FLAG_EXTERNAL_DMA) = SDHCI_FLAG_EXTERNAL_DMA then
  begin
   Result.Add('SDHCI_FLAG_EXTERNAL_DMA');
  end;
 if (AFlags and SDHCI_FLAG_BUS_ADDRESSES) = SDHCI_FLAG_BUS_ADDRESSES then
  begin
   Result.Add('SDHCI_FLAG_BUS_ADDRESSES');
  end;

 {Check Flags}
 if Result.Count = 0 then
  begin
   Result.Add('SDHCI_FLAG_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusMMC.MMCVoltagesToNames(AVoltages:LongWord):TStringList;
begin
 Result:=TStringList.Create;

 {Check Voltages}
 if (AVoltages and MMC_VDD_165_195) = MMC_VDD_165_195 then
  begin
   Result.Add('MMC_VDD_165_195');
  end;
 if (AVoltages and MMC_VDD_20_21) = MMC_VDD_20_21 then
  begin
   Result.Add('MMC_VDD_20_21');
  end;
 if (AVoltages and MMC_VDD_21_22) = MMC_VDD_21_22 then
  begin
   Result.Add('MMC_VDD_21_22');
  end;
 if (AVoltages and MMC_VDD_22_23) = MMC_VDD_22_23 then
  begin
   Result.Add('MMC_VDD_22_23');
  end;
 if (AVoltages and MMC_VDD_23_24) = MMC_VDD_23_24 then
  begin
   Result.Add('MMC_VDD_23_24');
  end;
 if (AVoltages and MMC_VDD_24_25) = MMC_VDD_24_25 then
  begin
   Result.Add('MMC_VDD_24_25');
  end;
 if (AVoltages and MMC_VDD_25_26) = MMC_VDD_25_26 then
  begin
   Result.Add('MMC_VDD_25_26');
  end;
 if (AVoltages and MMC_VDD_26_27) = MMC_VDD_26_27 then
  begin
   Result.Add('MMC_VDD_26_27');
  end;
 if (AVoltages and MMC_VDD_27_28) = MMC_VDD_27_28 then
  begin
   Result.Add('MMC_VDD_27_28');
  end;
 if (AVoltages and MMC_VDD_28_29) = MMC_VDD_28_29 then
  begin
   Result.Add('MMC_VDD_28_29');
  end;
 if (AVoltages and MMC_VDD_29_30) = MMC_VDD_29_30 then
  begin
   Result.Add('MMC_VDD_29_30');
  end;
 if (AVoltages and MMC_VDD_30_31) = MMC_VDD_30_31 then
  begin
   Result.Add('MMC_VDD_30_31');
  end;
 if (AVoltages and MMC_VDD_31_32) = MMC_VDD_31_32 then
  begin
   Result.Add('MMC_VDD_31_32');
  end;
 if (AVoltages and MMC_VDD_32_33) = MMC_VDD_32_33 then
  begin
   Result.Add('MMC_VDD_32_33');
  end;
 if (AVoltages and MMC_VDD_33_34) = MMC_VDD_33_34 then
  begin
   Result.Add('MMC_VDD_33_34');
  end;
 if (AVoltages and MMC_VDD_34_35) = MMC_VDD_34_35 then
  begin
   Result.Add('MMC_VDD_34_35');
  end;
 if (AVoltages and MMC_VDD_35_36) = MMC_VDD_35_36 then
  begin
   Result.Add('MMC_VDD_35_36');
  end;

 {Check Voltages}
 if Result.Count = 0 then
  begin
   Result.Add('MMC_VDD_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusMMC.MMCCapabilitiesToNames(ACapabilities:LongWord):TStringList;
begin
 Result:=TStringList.Create;

 {Check Capabilities}
 if (ACapabilities and MMC_CAP_4_BIT_DATA) = MMC_CAP_4_BIT_DATA then
  begin
   Result.Add('MMC_CAP_4_BIT_DATA');
  end;
 if (ACapabilities and MMC_CAP_MMC_HIGHSPEED) = MMC_CAP_MMC_HIGHSPEED then
  begin
   Result.Add('MMC_CAP_MMC_HIGHSPEED');
  end;
 if (ACapabilities and MMC_CAP_SD_HIGHSPEED) = MMC_CAP_SD_HIGHSPEED then
  begin
   Result.Add('MMC_CAP_SD_HIGHSPEED');
  end;
 if (ACapabilities and MMC_CAP_SDIO_IRQ) = MMC_CAP_SDIO_IRQ then
  begin
   Result.Add('MMC_CAP_SDIO_IRQ');
  end;
 if (ACapabilities and MMC_CAP_SPI) = MMC_CAP_SPI then
  begin
   Result.Add('MMC_CAP_SPI');
  end;
 if (ACapabilities and MMC_CAP_NEEDS_POLL) = MMC_CAP_NEEDS_POLL then
  begin
   Result.Add('MMC_CAP_NEEDS_POLL');
  end;
 if (ACapabilities and MMC_CAP_8_BIT_DATA) = MMC_CAP_8_BIT_DATA then
  begin
   Result.Add('MMC_CAP_8_BIT_DATA');
  end;
 if (ACapabilities and MMC_CAP_AGGRESSIVE_PM) = MMC_CAP_AGGRESSIVE_PM then
  begin
   Result.Add('MMC_CAP_AGGRESSIVE_PM');
  end;
 if (ACapabilities and MMC_CAP_NONREMOVABLE) = MMC_CAP_NONREMOVABLE then
  begin
   Result.Add('MMC_CAP_NONREMOVABLE');
  end;
 if (ACapabilities and MMC_CAP_WAIT_WHILE_BUSY) = MMC_CAP_WAIT_WHILE_BUSY then
  begin
   Result.Add('MMC_CAP_WAIT_WHILE_BUSY');
  end;
 if (ACapabilities and MMC_CAP_3_3V_DDR) = MMC_CAP_3_3V_DDR then
  begin
   Result.Add('MMC_CAP_3_3V_DDR');
  end;
 if (ACapabilities and MMC_CAP_1_8V_DDR) = MMC_CAP_1_8V_DDR then
  begin
   Result.Add('MMC_CAP_1_8V_DDR');
  end;
 if (ACapabilities and MMC_CAP_1_2V_DDR) = MMC_CAP_1_2V_DDR then
  begin
   Result.Add('MMC_CAP_1_2V_DDR');
  end;
 if (ACapabilities and MMC_CAP_POWER_OFF_CARD) = MMC_CAP_POWER_OFF_CARD then
  begin
   Result.Add('MMC_CAP_POWER_OFF_CARD');
  end;
 if (ACapabilities and MMC_CAP_BUS_WIDTH_TEST) = MMC_CAP_BUS_WIDTH_TEST then
  begin
   Result.Add('MMC_CAP_BUS_WIDTH_TEST');
  end;
 if (ACapabilities and MMC_CAP_UHS_SDR12) = MMC_CAP_UHS_SDR12 then
  begin
   Result.Add('MMC_CAP_UHS_SDR12');
  end;
 if (ACapabilities and MMC_CAP_UHS_SDR25) = MMC_CAP_UHS_SDR25 then
  begin
   Result.Add('MMC_CAP_UHS_SDR25');
  end;
 if (ACapabilities and MMC_CAP_UHS_SDR50) = MMC_CAP_UHS_SDR50 then
  begin
   Result.Add('MMC_CAP_UHS_SDR50');
  end;
 if (ACapabilities and MMC_CAP_UHS_SDR104) = MMC_CAP_UHS_SDR104 then
  begin
   Result.Add('MMC_CAP_UHS_SDR104');
  end;
 if (ACapabilities and MMC_CAP_UHS_DDR50) = MMC_CAP_UHS_DDR50 then
  begin
   Result.Add('MMC_CAP_UHS_DDR50');
  end;
 if (ACapabilities and MMC_CAP_SYNC_RUNTIME_PM) = MMC_CAP_SYNC_RUNTIME_PM then
  begin
   Result.Add('MMC_CAP_SYNC_RUNTIME_PM');
  end;
 if (ACapabilities and MMC_CAP_NEED_RSP_BUSY) = MMC_CAP_NEED_RSP_BUSY then
  begin
   Result.Add('MMC_CAP_NEED_RSP_BUSY');
  end;
 if (ACapabilities and MMC_CAP_DRIVER_TYPE_A) = MMC_CAP_DRIVER_TYPE_A then
  begin
   Result.Add('MMC_CAP_DRIVER_TYPE_A');
  end;
 if (ACapabilities and MMC_CAP_DRIVER_TYPE_C) = MMC_CAP_DRIVER_TYPE_C then
  begin
   Result.Add('MMC_CAP_DRIVER_TYPE_C');
  end;
 if (ACapabilities and MMC_CAP_DRIVER_TYPE_D) = MMC_CAP_DRIVER_TYPE_D then
  begin
   Result.Add('MMC_CAP_DRIVER_TYPE_D');
  end;
 if (ACapabilities and MMC_CAP_DONE_COMPLETE) = MMC_CAP_DONE_COMPLETE then
  begin
   Result.Add('MMC_CAP_DONE_COMPLETE');
  end;
 if (ACapabilities and MMC_CAP_CD_WAKE) = MMC_CAP_CD_WAKE then
  begin
   Result.Add('MMC_CAP_CD_WAKE');
  end;
 if (ACapabilities and MMC_CAP_CMD_DURING_TFR) = MMC_CAP_CMD_DURING_TFR then
  begin
   Result.Add('MMC_CAP_CMD_DURING_TFR');
  end;
 if (ACapabilities and MMC_CAP_CMD23) = MMC_CAP_CMD23 then
  begin
   Result.Add('MMC_CAP_CMD23');
  end;
 if (ACapabilities and MMC_CAP_HW_RESET) = MMC_CAP_HW_RESET then
  begin
   Result.Add('MMC_CAP_HW_RESET');
  end;
 
 {Check Capabilities}
 if Result.Count = 0 then
  begin
   Result.Add('MMC_CAP_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusMMC.MMCCapabilities2ToNames(ACapabilities2:LongWord):TStringList;
begin
 Result:=TStringList.Create;

 {Check Capabilities2}
 if (ACapabilities2 and MMC_CAP2_BOOTPART_NOACC) = MMC_CAP2_BOOTPART_NOACC then
  begin
   Result.Add('MMC_CAP2_BOOTPART_NOACC');
  end;
 if (ACapabilities2 and MMC_CAP2_FULL_PWR_CYCLE) = MMC_CAP2_FULL_PWR_CYCLE then
  begin
   Result.Add('MMC_CAP2_FULL_PWR_CYCLE');
  end;
 if (ACapabilities2 and MMC_CAP2_FULL_PWR_CYCLE_IN_SUSPEND) = MMC_CAP2_FULL_PWR_CYCLE_IN_SUSPEND then
  begin
   Result.Add('MMC_CAP2_FULL_PWR_CYCLE_IN_SUSPEND');
  end;
 if (ACapabilities2 and MMC_CAP2_HS200_1_8V_SDR) = MMC_CAP2_HS200_1_8V_SDR then
  begin
   Result.Add('MMC_CAP2_HS200_1_8V_SDR');
  end;
 if (ACapabilities2 and MMC_CAP2_HS200_1_2V_SDR) = MMC_CAP2_HS200_1_2V_SDR then
  begin
   Result.Add('MMC_CAP2_HS200_1_2V_SDR');
  end;
 if (ACapabilities2 and MMC_CAP2_CD_ACTIVE_HIGH) = MMC_CAP2_CD_ACTIVE_HIGH then
  begin
   Result.Add('MMC_CAP2_CD_ACTIVE_HIGH');
  end;
 if (ACapabilities2 and MMC_CAP2_RO_ACTIVE_HIGH) = MMC_CAP2_RO_ACTIVE_HIGH then
  begin
   Result.Add('MMC_CAP2_RO_ACTIVE_HIGH');
  end;
 if (ACapabilities2 and MMC_CAP2_NO_PRESCAN_POWERUP) = MMC_CAP2_NO_PRESCAN_POWERUP then
  begin
   Result.Add('MMC_CAP2_NO_PRESCAN_POWERUP');
  end;
 if (ACapabilities2 and MMC_CAP2_HS400_1_8V) = MMC_CAP2_HS400_1_8V then
  begin
   Result.Add('MMC_CAP2_HS400_1_8V');
  end;
 if (ACapabilities2 and MMC_CAP2_HS400_1_2V) = MMC_CAP2_HS400_1_2V then
  begin
   Result.Add('MMC_CAP2_HS400_1_2V');
  end;
 if (ACapabilities2 and MMC_CAP2_SDIO_IRQ_NOTHREAD) = MMC_CAP2_SDIO_IRQ_NOTHREAD then
  begin
   Result.Add('MMC_CAP2_SDIO_IRQ_NOTHREAD');
  end;
 if (ACapabilities2 and MMC_CAP2_NO_WRITE_PROTECT) = MMC_CAP2_NO_WRITE_PROTECT then
  begin
   Result.Add('MMC_CAP2_NO_WRITE_PROTECT');
  end;
 if (ACapabilities2 and MMC_CAP2_NO_SDIO) = MMC_CAP2_NO_SDIO then
  begin
   Result.Add('MMC_CAP2_NO_SDIO');
  end;
 if (ACapabilities2 and MMC_CAP2_HS400_ES) = MMC_CAP2_HS400_ES then
  begin
   Result.Add('MMC_CAP2_HS400_ES');
  end;
 if (ACapabilities2 and MMC_CAP2_NO_SD) = MMC_CAP2_NO_SD then
  begin
   Result.Add('MMC_CAP2_NO_SD');
  end;
 if (ACapabilities2 and MMC_CAP2_NO_MMC) = MMC_CAP2_NO_MMC then
  begin
   Result.Add('MMC_CAP2_NO_MMC');
  end;
 if (ACapabilities2 and MMC_CAP2_CQE) = MMC_CAP2_CQE then
  begin
   Result.Add('MMC_CAP2_CQE');
  end;
 if (ACapabilities2 and MMC_CAP2_CQE_DCMD) = MMC_CAP2_CQE_DCMD then
  begin
   Result.Add('MMC_CAP2_CQE_DCMD');
  end;
 if (ACapabilities2 and MMC_CAP2_AVOID_3_3V) = MMC_CAP2_AVOID_3_3V then
  begin
   Result.Add('MMC_CAP2_AVOID_3_3V');
  end;
 if (ACapabilities2 and MMC_CAP2_MERGE_CAPABLE) = MMC_CAP2_MERGE_CAPABLE then
  begin
   Result.Add('MMC_CAP2_MERGE_CAPABLE');
  end;

 {Check Capabilities2}
 if Result.Count = 0 then
  begin
   Result.Add('MMC_CAP2_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusMMC.SDHCIQuirksToNames(AQuirks:LongWord):TStringList;
begin
 Result:=TStringList.Create;

 {Check Quirks}
 if (AQuirks and SDHCI_QUIRK_CLOCK_BEFORE_RESET) = SDHCI_QUIRK_CLOCK_BEFORE_RESET then
  begin
   Result.Add('SDHCI_QUIRK_CLOCK_BEFORE_RESET');
  end;
 if (AQuirks and SDHCI_QUIRK_FORCE_DMA) = SDHCI_QUIRK_FORCE_DMA then
  begin
   Result.Add('SDHCI_QUIRK_FORCE_DMA');
  end;
 if (AQuirks and SDHCI_QUIRK_NO_CARD_NO_RESET) = SDHCI_QUIRK_NO_CARD_NO_RESET then
  begin
   Result.Add('SDHCI_QUIRK_NO_CARD_NO_RESET');
  end;
 if (AQuirks and SDHCI_QUIRK_SINGLE_POWER_WRITE) = SDHCI_QUIRK_SINGLE_POWER_WRITE then
  begin
   Result.Add('SDHCI_QUIRK_SINGLE_POWER_WRITE');
  end;
 if (AQuirks and SDHCI_QUIRK_RESET_CMD_DATA_ON_IOS) = SDHCI_QUIRK_RESET_CMD_DATA_ON_IOS then
  begin
   Result.Add('SDHCI_QUIRK_RESET_CMD_DATA_ON_IOS');
  end;
 if (AQuirks and SDHCI_QUIRK_BROKEN_DMA) = SDHCI_QUIRK_BROKEN_DMA then
  begin
   Result.Add('SDHCI_QUIRK_BROKEN_DMA');
  end;
 if (AQuirks and SDHCI_QUIRK_BROKEN_ADMA) = SDHCI_QUIRK_BROKEN_ADMA then
  begin
   Result.Add('SDHCI_QUIRK_BROKEN_ADMA');
  end;
 if (AQuirks and SDHCI_QUIRK_32BIT_DMA_ADDR) = SDHCI_QUIRK_32BIT_DMA_ADDR then
  begin
   Result.Add('SDHCI_QUIRK_32BIT_DMA_ADDR');
  end;
 if (AQuirks and SDHCI_QUIRK_32BIT_DMA_SIZE) = SDHCI_QUIRK_32BIT_DMA_SIZE then
  begin
   Result.Add('SDHCI_QUIRK_32BIT_DMA_SIZE');
  end;
 if (AQuirks and SDHCI_QUIRK_32BIT_ADMA_SIZE) = SDHCI_QUIRK_32BIT_ADMA_SIZE then
  begin
   Result.Add('SDHCI_QUIRK_32BIT_ADMA_SIZE');
  end;
 if (AQuirks and SDHCI_QUIRK_RESET_AFTER_REQUEST) = SDHCI_QUIRK_RESET_AFTER_REQUEST then
  begin
   Result.Add('SDHCI_QUIRK_RESET_AFTER_REQUEST');
  end;
 if (AQuirks and SDHCI_QUIRK_NO_SIMULT_VDD_AND_POWER) = SDHCI_QUIRK_NO_SIMULT_VDD_AND_POWER then
  begin
   Result.Add('SDHCI_QUIRK_NO_SIMULT_VDD_AND_POWER');
  end;
 if (AQuirks and SDHCI_QUIRK_BROKEN_TIMEOUT_VAL) = SDHCI_QUIRK_BROKEN_TIMEOUT_VAL then
  begin
   Result.Add('SDHCI_QUIRK_BROKEN_TIMEOUT_VAL');
  end;
 if (AQuirks and SDHCI_QUIRK_BROKEN_SMALL_PIO) = SDHCI_QUIRK_BROKEN_SMALL_PIO then
  begin
   Result.Add('SDHCI_QUIRK_BROKEN_SMALL_PIO');
  end;
 if (AQuirks and SDHCI_QUIRK_NO_BUSY_IRQ) = SDHCI_QUIRK_NO_BUSY_IRQ then
  begin
   Result.Add('SDHCI_QUIRK_NO_BUSY_IRQ');
  end;
 if (AQuirks and SDHCI_QUIRK_BROKEN_CARD_DETECTION) = SDHCI_QUIRK_BROKEN_CARD_DETECTION then
  begin
   Result.Add('SDHCI_QUIRK_BROKEN_CARD_DETECTION');
  end;
 if (AQuirks and SDHCI_QUIRK_INVERTED_WRITE_PROTECT) = SDHCI_QUIRK_INVERTED_WRITE_PROTECT then
  begin
   Result.Add('SDHCI_QUIRK_INVERTED_WRITE_PROTECT');
  end;
 if (AQuirks and SDHCI_QUIRK_PIO_NEEDS_DELAY) = SDHCI_QUIRK_PIO_NEEDS_DELAY then
  begin
   Result.Add('SDHCI_QUIRK_PIO_NEEDS_DELAY');
  end;
 if (AQuirks and SDHCI_QUIRK_FORCE_BLK_SZ_2048) = SDHCI_QUIRK_FORCE_BLK_SZ_2048 then
  begin
   Result.Add('SDHCI_QUIRK_FORCE_BLK_SZ_2048');
  end;
 if (AQuirks and SDHCI_QUIRK_NO_MULTIBLOCK) = SDHCI_QUIRK_NO_MULTIBLOCK then
  begin
   Result.Add('SDHCI_QUIRK_NO_MULTIBLOCK');
  end;
 if (AQuirks and SDHCI_QUIRK_FORCE_1_BIT_DATA) = SDHCI_QUIRK_FORCE_1_BIT_DATA then
  begin
   Result.Add('SDHCI_QUIRK_FORCE_1_BIT_DATA');
  end;
 if (AQuirks and SDHCI_QUIRK_DELAY_AFTER_POWER) = SDHCI_QUIRK_DELAY_AFTER_POWER then
  begin
   Result.Add('SDHCI_QUIRK_DELAY_AFTER_POWER');
  end;
 if (AQuirks and SDHCI_QUIRK_DATA_TIMEOUT_USES_SDCLK) = SDHCI_QUIRK_DATA_TIMEOUT_USES_SDCLK then
  begin
   Result.Add('SDHCI_QUIRK_DATA_TIMEOUT_USES_SDCLK');
  end;
 if (AQuirks and SDHCI_QUIRK_CAP_CLOCK_BASE_BROKEN) = SDHCI_QUIRK_CAP_CLOCK_BASE_BROKEN then
  begin
   Result.Add('SDHCI_QUIRK_CAP_CLOCK_BASE_BROKEN');
  end;
 if (AQuirks and SDHCI_QUIRK_NO_ENDATTR_IN_NOPDESC) = SDHCI_QUIRK_NO_ENDATTR_IN_NOPDESC then
  begin
   Result.Add('SDHCI_QUIRK_NO_ENDATTR_IN_NOPDESC');
  end;
 if (AQuirks and SDHCI_QUIRK_MISSING_CAPS) = SDHCI_QUIRK_MISSING_CAPS then
  begin
   Result.Add('SDHCI_QUIRK_MISSING_CAPS');
  end;
 if (AQuirks and SDHCI_QUIRK_MULTIBLOCK_READ_ACMD12) = SDHCI_QUIRK_MULTIBLOCK_READ_ACMD12 then
  begin
   Result.Add('SDHCI_QUIRK_MULTIBLOCK_READ_ACMD12');
  end;
 if (AQuirks and SDHCI_QUIRK_NO_HISPD_BIT) = SDHCI_QUIRK_NO_HISPD_BIT then
  begin
   Result.Add('SDHCI_QUIRK_NO_HISPD_BIT');
  end;
 if (AQuirks and SDHCI_QUIRK_BROKEN_ADMA_ZEROLEN_DESC) = SDHCI_QUIRK_BROKEN_ADMA_ZEROLEN_DESC then
  begin
   Result.Add('SDHCI_QUIRK_BROKEN_ADMA_ZEROLEN_DESC');
  end;
 if (AQuirks and SDHCI_QUIRK_UNSTABLE_RO_DETECT) = SDHCI_QUIRK_UNSTABLE_RO_DETECT then
  begin
   Result.Add('SDHCI_QUIRK_UNSTABLE_RO_DETECT');
  end;
 
 {Check Quirks}
 if Result.Count = 0 then
  begin
   Result.Add('SDHCI_QUIRK_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusMMC.SDHCIQuirks2ToNames(AQuirks2:LongWord):TStringList;
begin
 Result:=TStringList.Create;

 {Check AQuirks2}
 if (AQuirks2 and SDHCI_QUIRK2_HOST_OFF_CARD_ON) = SDHCI_QUIRK2_HOST_OFF_CARD_ON then
  begin
   Result.Add('SDHCI_QUIRK2_HOST_OFF_CARD_ON');
  end;
 if (AQuirks2 and SDHCI_QUIRK2_HOST_NO_CMD23) = SDHCI_QUIRK2_HOST_NO_CMD23 then
  begin
   Result.Add('SDHCI_QUIRK2_HOST_NO_CMD23');
  end;
 if (AQuirks2 and SDHCI_QUIRK2_NO_1_8_V) = SDHCI_QUIRK2_NO_1_8_V then
  begin
   Result.Add('SDHCI_QUIRK2_NO_1_8_V');
  end;
 if (AQuirks2 and SDHCI_QUIRK2_PRESET_VALUE_BROKEN) = SDHCI_QUIRK2_PRESET_VALUE_BROKEN then
  begin
   Result.Add('SDHCI_QUIRK2_PRESET_VALUE_BROKEN');
  end;
 if (AQuirks2 and SDHCI_QUIRK2_CARD_ON_NEEDS_BUS_ON) = SDHCI_QUIRK2_CARD_ON_NEEDS_BUS_ON then
  begin
   Result.Add('SDHCI_QUIRK2_CARD_ON_NEEDS_BUS_ON');
  end;
 if (AQuirks2 and SDHCI_QUIRK2_BROKEN_HOST_CONTROL) = SDHCI_QUIRK2_BROKEN_HOST_CONTROL then
  begin
   Result.Add('SDHCI_QUIRK2_BROKEN_HOST_CONTROL');
  end;
 if (AQuirks2 and SDHCI_QUIRK2_BROKEN_HS200) = SDHCI_QUIRK2_BROKEN_HS200 then
  begin
   Result.Add('SDHCI_QUIRK2_BROKEN_HS200');
  end;
 if (AQuirks2 and SDHCI_QUIRK2_BROKEN_DDR50) = SDHCI_QUIRK2_BROKEN_DDR50 then
  begin
   Result.Add('SDHCI_QUIRK2_BROKEN_DDR50');
  end;
 if (AQuirks2 and SDHCI_QUIRK2_STOP_WITH_TC) = SDHCI_QUIRK2_STOP_WITH_TC then
  begin
   Result.Add('SDHCI_QUIRK2_STOP_WITH_TC');
  end;
 if (AQuirks2 and SDHCI_QUIRK2_BROKEN_64_BIT_DMA) = SDHCI_QUIRK2_BROKEN_64_BIT_DMA then
  begin
   Result.Add('SDHCI_QUIRK2_BROKEN_64_BIT_DMA');
  end;
 if (AQuirks2 and SDHCI_QUIRK2_CLEAR_TRANSFERMODE_REG_BEFORE_CMD) = SDHCI_QUIRK2_CLEAR_TRANSFERMODE_REG_BEFORE_CMD then
  begin
   Result.Add('SDHCI_QUIRK2_CLEAR_TRANSFERMODE_REG_BEFORE_CMD');
  end;
 if (AQuirks2 and SDHCI_QUIRK2_CAPS_BIT63_FOR_HS400) = SDHCI_QUIRK2_CAPS_BIT63_FOR_HS400 then
  begin
   Result.Add('SDHCI_QUIRK2_CAPS_BIT63_FOR_HS400');
  end;
 if (AQuirks2 and SDHCI_QUIRK2_TUNING_WORK_AROUND) = SDHCI_QUIRK2_TUNING_WORK_AROUND then
  begin
   Result.Add('SDHCI_QUIRK2_TUNING_WORK_AROUND');
  end;
 if (AQuirks2 and SDHCI_QUIRK2_SUPPORT_SINGLE) = SDHCI_QUIRK2_SUPPORT_SINGLE then
  begin
   Result.Add('SDHCI_QUIRK2_SUPPORT_SINGLE');
  end;
 if (AQuirks2 and SDHCI_QUIRK2_ACMD23_BROKEN) = SDHCI_QUIRK2_ACMD23_BROKEN then
  begin
   Result.Add('SDHCI_QUIRK2_ACMD23_BROKEN');
  end;
 if (AQuirks2 and SDHCI_QUIRK2_CLOCK_DIV_ZERO_BROKEN) = SDHCI_QUIRK2_CLOCK_DIV_ZERO_BROKEN then
  begin
   Result.Add('SDHCI_QUIRK2_CLOCK_DIV_ZERO_BROKEN');
  end;
 if (AQuirks2 and SDHCI_QUIRK2_RSP_136_HAS_CRC) = SDHCI_QUIRK2_RSP_136_HAS_CRC then
  begin
   Result.Add('SDHCI_QUIRK2_RSP_136_HAS_CRC');
  end;
 if (AQuirks2 and SDHCI_QUIRK2_DISABLE_HW_TIMEOUT) = SDHCI_QUIRK2_DISABLE_HW_TIMEOUT then
  begin
   Result.Add('SDHCI_QUIRK2_DISABLE_HW_TIMEOUT');
  end;
 if (AQuirks2 and SDHCI_QUIRK2_USE_32BIT_BLK_CNT) = SDHCI_QUIRK2_USE_32BIT_BLK_CNT then
  begin
   Result.Add('SDHCI_QUIRK2_USE_32BIT_BLK_CNT');
  end;
 
 {Check Quirks2}
 if Result.Count = 0 then
  begin
   Result.Add('SDHCI_QUIRK2_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusMMC.SDHCIInterruptsToNames(AInterrupts:LongWord):TStringList;
begin
 Result:=TStringList.Create;

 {Check Interrupts}
 if (AInterrupts and SDHCI_INT_RESPONSE) = SDHCI_INT_RESPONSE then
  begin
   Result.Add('SDHCI_INT_RESPONSE');
  end;
 if (AInterrupts and SDHCI_INT_DATA_END) = SDHCI_INT_DATA_END then
  begin
   Result.Add('SDHCI_INT_DATA_END');
  end;
 if (AInterrupts and SDHCI_INT_BLK_GAP) = SDHCI_INT_BLK_GAP then
  begin
   Result.Add('SDHCI_INT_BLK_GAP');
  end;
 if (AInterrupts and SDHCI_INT_DMA_END) = SDHCI_INT_DMA_END then
  begin
   Result.Add('SDHCI_INT_DMA_END');
  end;
 if (AInterrupts and SDHCI_INT_SPACE_AVAIL) = SDHCI_INT_SPACE_AVAIL then
  begin
   Result.Add('SDHCI_INT_SPACE_AVAIL');
  end;
 if (AInterrupts and SDHCI_INT_DATA_AVAIL) = SDHCI_INT_DATA_AVAIL then
  begin
   Result.Add('SDHCI_INT_DATA_AVAIL');
  end;
 if (AInterrupts and SDHCI_INT_CARD_INSERT) = SDHCI_INT_CARD_INSERT then
  begin
   Result.Add('SDHCI_INT_CARD_INSERT');
  end;
 if (AInterrupts and SDHCI_INT_CARD_REMOVE) = SDHCI_INT_CARD_REMOVE then
  begin
   Result.Add('SDHCI_INT_CARD_REMOVE');
  end;
 if (AInterrupts and SDHCI_INT_CARD_INT) = SDHCI_INT_CARD_INT then
  begin
   Result.Add('SDHCI_INT_CARD_INT');
  end;
 if (AInterrupts and SDHCI_INT_RETUNE) = SDHCI_INT_RETUNE then
  begin
   Result.Add('SDHCI_INT_RETUNE');
  end;
 if (AInterrupts and SDHCI_INT_CQE) = SDHCI_INT_CQE then
  begin
   Result.Add('SDHCI_INT_CQE');
  end;
 if (AInterrupts and SDHCI_INT_ERROR) = SDHCI_INT_ERROR then
  begin
   Result.Add('SDHCI_INT_ERROR');
  end;
 if (AInterrupts and SDHCI_INT_TIMEOUT) = SDHCI_INT_TIMEOUT then
  begin
   Result.Add('SDHCI_INT_TIMEOUT');
  end;
 if (AInterrupts and SDHCI_INT_CRC) = SDHCI_INT_CRC then
  begin
   Result.Add('SDHCI_INT_CRC');
  end;
 if (AInterrupts and SDHCI_INT_END_BIT) = SDHCI_INT_END_BIT then
  begin
   Result.Add('SDHCI_INT_END_BIT');
  end;
 if (AInterrupts and SDHCI_INT_INDEX) = SDHCI_INT_INDEX then
  begin
   Result.Add('SDHCI_INT_INDEX');
  end;
 if (AInterrupts and SDHCI_INT_DATA_TIMEOUT) = SDHCI_INT_DATA_TIMEOUT then
  begin
   Result.Add('SDHCI_INT_DATA_TIMEOUT');
  end;
 if (AInterrupts and SDHCI_INT_DATA_CRC) = SDHCI_INT_DATA_CRC then
  begin
   Result.Add('SDHCI_INT_DATA_CRC');
  end;
 if (AInterrupts and SDHCI_INT_DATA_END_BIT) = SDHCI_INT_DATA_END_BIT then
  begin
   Result.Add('SDHCI_INT_DATA_END_BIT');
  end;
 if (AInterrupts and SDHCI_INT_BUS_POWER) = SDHCI_INT_BUS_POWER then
  begin
   Result.Add('SDHCI_INT_BUS_POWER');
  end;
 if (AInterrupts and SDHCI_INT_AUTO_CMD_ERR) = SDHCI_INT_AUTO_CMD_ERR then
  begin
   Result.Add('SDHCI_INT_AUTO_CMD_ERR');
  end;
 if (AInterrupts and SDHCI_INT_ADMA_ERROR) = SDHCI_INT_ADMA_ERROR then
  begin
   Result.Add('SDHCI_INT_ADMA_ERROR');
  end;
 
 {Check Interrupts}
 if Result.Count = 0 then
  begin
   Result.Add('SDHCI_INT_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusMMC.SDHCIVoltagesToNames(AVoltages:LongWord):TStringList;
begin
 {}
 Result:=MMCVoltagesToNames(AVoltages);
end;

{==============================================================================}

function TWebStatusMMC.SDHCICapabilitiesToNames(ACapabilities:LongWord):TStringList;
begin
 {}
 Result:=MMCCapabilitiesToNames(ACapabilities);
end;

{==============================================================================}

function TWebStatusMMC.SDHCICapabilities2ToNames(ACapabilities2:LongWord):TStringList;
begin
 {}
 Result:=MMCCapabilities2ToNames(ACapabilities2);
end;

{==============================================================================}

function TWebStatusMMC.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
var
 Id:LongWord;
 Action:String;
 Count:LongWord;
 WorkBuffer:String;
 Data:TWebStatusData;
 Names:TStringList;
 FlagNames:TStringList;
 
 MMCDevice:PMMCDevice;
 SDHCIHost:PSDHCIHost;
 SDIODevice:PSDIODevice;
begin
 {}
 Result:=False;
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;

 {Get Action}
 Action:=Uppercase(ARequest.GetParam('ACTION'));

 {Get Id}
 WorkBuffer:=Uppercase(ARequest.GetParam('ID'));

 if (Action = 'MMCDEVICE') and (Length(WorkBuffer) > 0) then
  begin
   {Add Header (2 column with Caption)}
   AddHeaderEx(AResponse,GetTitle,'MMC Device Information',Self,2);

   {Get Id}
   Id:=StrToIntDef(WorkBuffer,0);
   
   {Get MMC Device}
   MMCDevice:=MMCDeviceFind(Id);
   if MMCDevice <> nil then
    begin
     {Get Flags Names}
     FlagNames:=MMCFlagsToFlagNames(MMCDevice.Device.DeviceFlags);
     
     AddBold(AResponse,'Device','');
     AddBlank(AResponse);
     AddItem(AResponse,'Name:',DeviceGetName(@MMCDevice.Device));
     AddItem(AResponse,'Type:',MMCDeviceTypeToString(MMCDevice.Device.DeviceType));
     AddItem(AResponse,'Flags:',FlagNames.Strings[0]);

     {Check Flag Count}
     if FlagNames.Count > 1 then
      begin
       for Count:=1 to FlagNames.Count - 1 do
        begin
         {Add Flag Name}
         AddItem(AResponse,'',FlagNames.Strings[Count]);
        end;
      end;

     AddItem(AResponse,'Description:',MMCDevice.Device.DeviceDescription);
     AddBlank(AResponse);
     AddItem(AResponse,'Id:',IntToStr(MMCDevice.MMCId));
     AddItem(AResponse,'State:',MMCDeviceStateToString(MMCDevice.MMCState));
     AddBlank(AResponse);
     AddItem(AResponse,'Version:',MMCVersionToString(MMCDevice.Version));
     AddItem(AResponse,'Clock:',IntToStr(MMCDevice.Clock));
     AddItem(AResponse,'Timing:',MMCTimingToString(MMCDevice.Timing));
     WorkBuffer:=MMCBusWidthToString(MMCDevice.BusWidth);
     if MMCIsSD(MMCDevice) then WorkBuffer:=SDBusWidthToString(MMCDevice.BusWidth);
     AddItem(AResponse,'Bus Width:',WorkBuffer);
     AddItem(AResponse,'Driver Type:',MMCDriverTypeToString(MMCDevice.DriverType));
     AddItem(AResponse,'Signal Voltage:',MMCSignalVoltageToString(MMCDevice.SignalVoltage)); 
     AddBlank(AResponse);
     
     {Get Voltage Names}
     Names:=MMCVoltagesToNames(MMCDevice.Voltages);
     AddItem(AResponse,'Voltages:',Names.Strings[0]);
     {Check Voltage Count}
     if Names.Count > 1 then
      begin
       for Count:=1 to Names.Count - 1 do
        begin
         {Add Voltage Name}
         AddItem(AResponse,'',Names.Strings[Count]);
        end;
      end;
     Names.Free;
     
     AddBlank(AResponse);
     
     {Get Capability Names}
     Names:=MMCCapabilitiesToNames(MMCDevice.Capabilities);
     AddItem(AResponse,'Capabilities:',Names.Strings[0]);
     {Check Capability Count}
     if Names.Count > 1 then
      begin
       for Count:=1 to Names.Count - 1 do
        begin
         {Add Capability Name}
         AddItem(AResponse,'',Names.Strings[Count]);
        end;
      end;
     Names.Free;

     AddBlank(AResponse);

     {Get Capability2 Names}
     Names:=MMCCapabilities2ToNames(MMCDevice.Capabilities2);
     AddItem(AResponse,'Capabilities 2:',Names.Strings[0]);
     {Check Capability2 Count}
     if Names.Count > 1 then
      begin
       for Count:=1 to Names.Count - 1 do
        begin
         {Add Capability2 Name}
         AddItem(AResponse,'',Names.Strings[Count]);
        end;
      end;
     Names.Free;

     AddBlank(AResponse);
     
     AddItem(AResponse,'EraseSize:',IntToStr(MMCDevice.EraseSize));
     AddItem(AResponse,'EraseShift:',IntToStr(MMCDevice.EraseShift));
     AddItem(AResponse,'EraseArgument:',IntToHex(MMCDevice.EraseArgument,8));
     AddItem(AResponse,'PreferredEraseSize:',IntToStr(MMCDevice.PreferredEraseSize));
     AddItem(AResponse,'EnhancedStrobe:',BooleanToString(MMCDevice.EnhancedStrobe));
     AddBlank(AResponse);

     case MMCDevice.Device.DeviceType of
      MMC_TYPE_SDIO,MMC_TYPE_SD_COMBO:begin
        SDIODevice:=PSDIODevice(MMCDevice);
        AddItem(AResponse,'State:',SDIODeviceStateToString(SDIODevice.SDIOState));
        AddItem(AResponse,'Status:',SDIODeviceStatusToString(SDIODevice.SDIOStatus));
        //To Do //TestingSDIO
        AddBlank(AResponse);
        
        WorkBuffer:='';
        if SDIODevice.Host <> nil then WorkBuffer:=DeviceGetName(@SDIODevice.Host);
        AddItem(AResponse,'Host:',WorkBuffer);

        WorkBuffer:='';
        if SDIODevice.Driver <> nil then WorkBuffer:=DriverGetName(@SDIODevice.Driver.Driver);
        AddItem(AResponse,'Driver:',WorkBuffer);
        AddBlank(AResponse);
       end;
     end;  
     
     WorkBuffer:='';
     if MMCDevice.Storage <> nil then WorkBuffer:=DeviceGetName(@MMCDevice.Storage.Device);
     AddItem(AResponse,'Storage:',WorkBuffer);
     
     FlagNames.Free;
    end
   else
    begin
     AddItem(AResponse,'Not Found','');
    end;    
 
   {Add Footer}
   AddFooter(AResponse); 
  end
 else if (Action = 'SDHCIHOST') and (Length(WorkBuffer) > 0) then
  begin
   {Add Header (2 column with Caption)}
   AddHeaderEx(AResponse,GetTitle,'SDHCI Host Information',Self,2);
  
   {Get Id}
   Id:=StrToIntDef(WorkBuffer,0);
 
   {Get SDHCI Host}
   SDHCIHost:=SDHCIHostFind(Id);
   if SDHCIHost <> nil then
    begin
     {Get Flags Names}
     FlagNames:=SDHCIFlagsToFlagNames(SDHCIHost.Device.DeviceFlags);
     
     AddBold(AResponse,'Host','');
     AddBlank(AResponse);
     AddItem(AResponse,'Name:',DeviceGetName(@SDHCIHost.Device));
     AddItem(AResponse,'Type:',SDHCIHostTypeToString(SDHCIHost.Device.DeviceType));
     AddItem(AResponse,'Flags:',FlagNames.Strings[0]);

     {Check Flag Count}
     if FlagNames.Count > 1 then
      begin
       for Count:=1 to FlagNames.Count - 1 do
        begin
         {Add Flag Name}
         AddItem(AResponse,'',FlagNames.Strings[Count]);
        end;
      end;

     AddItem(AResponse,'Description:',SDHCIHost.Device.DeviceDescription);
     AddBlank(AResponse);
     AddItem(AResponse,'Id:',IntToStr(SDHCIHost.SDHCIId));
     AddItem(AResponse,'State:',SDHCIHostStateToString(SDHCIHost.SDHCIState));
     AddBlank(AResponse);
     AddItem(AResponse,'Address:','0x' + PtrToHex(SDHCIHost.Address));
     AddItem(AResponse,'Version:',SDHCIVersionToString(SDHCIGetVersion(SDHCIHost)));
     AddBlank(AResponse);
     
     {Get Quirk Names}
     Names:=SDHCIQuirksToNames(SDHCIHost.Quirks);
     AddItem(AResponse,'Quirks:',Names.Strings[0]);
     {Check Quirk Count}
     if Names.Count > 1 then
      begin
       for Count:=1 to Names.Count - 1 do
        begin
         {Add Quirk Name}
         AddItem(AResponse,'',Names.Strings[Count]);
        end;
      end;
     Names.Free;
     
     {Get Quirk2 Names}
     Names:=SDHCIQuirks2ToNames(SDHCIHost.Quirks2);
     AddItem(AResponse,'Quirks 2:',Names.Strings[0]);
     {Check Quirk2 Count}
     if Names.Count > 1 then
      begin
       for Count:=1 to Names.Count - 1 do
        begin
         {Add Quirk2 Name}
         AddItem(AResponse,'',Names.Strings[Count]);
        end;
      end;
     Names.Free;

     AddBlank(AResponse);
     AddItem(AResponse,'Clock:',IntToStr(SDHCIHost.Clock));
     AddItem(AResponse,'Power:',SDHCIPowerToString(SDHCIHost.Power));
     AddItem(AResponse,'Timing:',MMCTimingToString(SDHCIHost.Timing));
     AddItem(AResponse,'Bus Width:',MMCBusWidthToString(SDHCIHost.BusWidth));
     AddBlank(AResponse);
     
     {Get Interrupt Names}
     Names:=SDHCIInterruptsToNames(SDHCIHost.Interrupts);
     AddItem(AResponse,'Interrupts:',Names.Strings[0]);
     {Check Interrupt Count}
     if Names.Count > 1 then
      begin
       for Count:=1 to Names.Count - 1 do
        begin
         {Add Interrupt Name}
         AddItem(AResponse,'',Names.Strings[Count]);
        end;
      end;
     Names.Free;
     
     AddBlank(AResponse);
     
     {Get Voltage Names}
     Names:=SDHCIVoltagesToNames(SDHCIHost.Voltages);
     AddItem(AResponse,'Voltages:',Names.Strings[0]);
     {Check Voltage Count}
     if Names.Count > 1 then
      begin
       for Count:=1 to Names.Count - 1 do
        begin
         {Add Voltage Name}
         AddItem(AResponse,'',Names.Strings[Count]);
        end;
      end;
     Names.Free;

     AddBlank(AResponse);
     
     {Get Capability Names}
     Names:=SDHCICapabilitiesToNames(SDHCIHost.Capabilities);
     AddItem(AResponse,'Capabilities:',Names.Strings[0]);
     {Check Capability Count}
     if Names.Count > 1 then
      begin
       for Count:=1 to Names.Count - 1 do
        begin
         {Add Capability Name}
         AddItem(AResponse,'',Names.Strings[Count]);
        end;
      end;
     Names.Free;

     AddBlank(AResponse);
     
     {Get Capability2 Names}
     Names:=SDHCICapabilities2ToNames(SDHCIHost.Capabilities2);
     AddItem(AResponse,'Capabilities 2:',Names.Strings[0]);
     {Check Capability2 Count}
     if Names.Count > 1 then
      begin
       for Count:=1 to Names.Count - 1 do
        begin
         {Add Capability2 Name}
         AddItem(AResponse,'',Names.Strings[Count]);
        end;
      end;
     Names.Free;
     
     AddBlank(AResponse);
     AddItem(AResponse,'Minimum Frequency:',IntToStr(SDHCIHost.MinimumFrequency));
     AddItem(AResponse,'Maximum Frequency:',IntToStr(SDHCIHost.MaximumFrequency));
     AddBlank(AResponse);
     AddItem(AResponse,'Maximum Block Size:',IntToStr(SDHCIHost.MaximumBlockSize));
     AddItem(AResponse,'Maximum Block Count:',IntToStr(SDHCIHost.MaximumBlockCount));
     AddItem(AResponse,'Maximum Request Size:',IntToStr(SDHCIHost.MaximumRequestSize));
     AddItem(AResponse,'Minimum DMA Size:',IntToStr(SDHCIHost.MinimumDMASize));
     AddItem(AResponse,'Maximum PIO Blocks:',IntToStr(SDHCIHost.MaximumPIOBlocks));
     AddBlank(AResponse);
     AddItem(AResponse,'DMA Slave:',IntToStr(SDHCIHost.DMASlave));
     AddItem(AResponse,'SDMA Boundary:',IntToStr(SDHCIHost.SDMABoundary));
     AddItem(AResponse,'ADMA Table Size:',IntToStr(SDHCIHost.ADMATableSize));
     AddItem(AResponse,'ADMA Table Count:',IntToStr(SDHCIHost.ADMATableCount));
     AddItem(AResponse,'ADMA Buffer Size:',IntToStr(SDHCIHost.ADMABufferSize));
     AddItem(AResponse,'ADMA Descriptor Size:',IntToStr(SDHCIHost.ADMADescriptorSize));
     AddBlank(AResponse);
     
     {Get Preset Voltage Names}
     Names:=MMCVoltagesToNames(SDHCIHost.PresetVoltages);
     AddItem(AResponse,'Preset Voltages:',Names.Strings[0]);
     {Check Preset Voltage Count}
     if Names.Count > 1 then
      begin
       for Count:=1 to Names.Count - 1 do
        begin
         {Add Preset Voltage Name}
         AddItem(AResponse,'',Names.Strings[Count]);
        end;
      end;
     Names.Free;

     AddBlank(AResponse);
     
     {Get Preset Capability Names}
     Names:=MMCCapabilitiesToNames(SDHCIHost.PresetCapabilities);
     AddItem(AResponse,'Preset Capabilities:',Names.Strings[0]);
     {Check Preset Capability Count}
     if Names.Count > 1 then
      begin
       for Count:=1 to Names.Count - 1 do
        begin
         {Add Preset Capability Name}
         AddItem(AResponse,'',Names.Strings[Count]);
        end;
      end;
     Names.Free;

     AddBlank(AResponse);
     
     {Get Preset Capability2 Names}
     Names:=MMCCapabilities2ToNames(SDHCIHost.PresetCapabilities2);
     AddItem(AResponse,'Preset Capabilities 2:',Names.Strings[0]);
     {Check Preset Capability2 Count}
     if Names.Count > 1 then
      begin
       for Count:=1 to Names.Count - 1 do
        begin
         {Add Preset Capability2 Name}
         AddItem(AResponse,'',Names.Strings[Count]);
        end;
      end;
     Names.Free;
     
     AddBlank(AResponse);
     AddItem(AResponse,'Clock Minimum:',IntToStr(SDHCIHost.ClockMinimum));
     AddItem(AResponse,'Clock Maximum:',IntToStr(SDHCIHost.ClockMaximum));
     AddItem(AResponse,'Driver Stage Register:',IntToStr(SDHCIHost.DriverStageRegister));
     AddBlank(AResponse);
     AddItem(AResponse,'Request Count:',IntToStr(SDHCIHost.RequestCount));
     AddItem(AResponse,'Request Errors:',IntToStr(SDHCIHost.RequestErrors));
     AddItem(AResponse,'Data Request Count:',IntToStr(SDHCIHost.DataRequestCount));
     AddItem(AResponse,'Command Request Count:',IntToStr(SDHCIHost.CommandRequestCount));
     AddItem(AResponse,'PIO Data Transfer Count:',IntToStr(SDHCIHost.PIODataTransferCount));
     AddItem(AResponse,'DMA Data Transfer Count:',IntToStr(SDHCIHost.DMADataTransferCount));
     AddBlank(AResponse);
     AddItem(AResponse,'Interrupt Count:',IntToStr(SDHCIHost.InterruptCount));
     AddItem(AResponse,'Data Interrupt Count:',IntToStr(SDHCIHost.DataInterruptCount));
     AddItem(AResponse,'Command Interrupt Count:',IntToStr(SDHCIHost.CommandInterruptCount));
    
     FlagNames.Free;
    end
   else
    begin
     AddItem(AResponse,'Not Found','');
    end;    
 
   {Add Footer}
   AddFooter(AResponse); 
  end
 else
  begin 
   {Add Header (4 column)}
   AddHeaderEx(AResponse,GetTitle,'',Self,4); 
  
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
   Data.Data:=nil;
   
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
   Data.Data:=nil;
   
   {Enumerate SDHCIs}
   SDHCIHostEnumerate(WebStatusSDHCIEnumerate,@Data);
   
   {Add SDIO Driver List} 
   AddBlankEx(AResponse,4);
   AddBold4Column(AResponse,'SDIO Drivers','','','');
   AddBlankEx(AResponse,4);
   AddBold4Column(AResponse,'Driver Id','Name','State','');
   AddBlankEx(AResponse,4);
     
   {Setup Data}
   Data.Document:=Self;
   Data.Host:=AHost;
   Data.Request:=ARequest;
   Data.Response:=AResponse;
   Data.Data:=nil;
     
   {Enumerate SDIO Drivers}
   SDIODriverEnumerate(WebStatusSDIODriverEnumerate,@Data);
   
   {Add Footer (4 column)}
   AddFooterEx(AResponse,4); 
  end; 
 
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

function TWebStatusNetwork.NetworkFlagsToFlagNames(AFlags:LongWord):TStringList; 
begin
 {}
 Result:=TStringList.Create;
 
 {Check Flags}
 if (AFlags and NETWORK_FLAG_RX_BUFFER) = NETWORK_FLAG_RX_BUFFER then
  begin
   Result.Add('NETWORK_FLAG_RX_BUFFER');
  end;
 if (AFlags and NETWORK_FLAG_TX_BUFFER) = NETWORK_FLAG_TX_BUFFER then
  begin
   Result.Add('NETWORK_FLAG_TX_BUFFER');
  end;
 if (AFlags and NETWORK_FLAG_RX_MULTIPACKET) = NETWORK_FLAG_RX_MULTIPACKET then
  begin
   Result.Add('NETWORK_FLAG_RX_MULTIPACKET');
  end;
 if (AFlags and NETWORK_FLAG_TX_MULTIPACKET) = NETWORK_FLAG_TX_MULTIPACKET then
  begin
   Result.Add('NETWORK_FLAG_TX_MULTIPACKET');
  end;

 {Check Flags}
 if Result.Count = 0 then
  begin
   Result.Add('NETWORK_FLAG_NONE');
  end; 
end;

{==============================================================================}

function TWebStatusNetwork.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
var
 Id:LongWord;
 Name:String;
 Action:String;
 Count:LongWord;
 WorkBuffer:String;
 Data:TWebStatusData;
 FlagNames:TStringList;
 Adapter:TNetworkAdapter;
 Transport:TNetworkTransport;
 Protocol:TNetworkProtocol;
 NetworkDevice:PNetworkDevice;
 
 Size:LongWord;
 IfTable:PMIB_IFTABLE;
 TcpTable:PMIB_TCPTABLE;
 UdpTable:PMIB_UDPTABLE;
 IpAddrTable:PMIB_IPADDRTABLE;
 IpNetTable:PMIB_IPNETTABLE;
 IpForwardTable:PMIB_IPFORWARDTABLE;
 
 IPAddress:TInAddr;
 HardwareAddress:THardwareAddress;
 AdapterStatistics:TAdapterStatistics;
begin
 {}
 Result:=False;
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;

 {Get Action}
 Action:=Uppercase(ARequest.GetParam('ACTION'));

 {Get Id}
 WorkBuffer:=Uppercase(ARequest.GetParam('ID'));

 {Get Name}
 Name:=Uppercase(ARequest.GetParam('NAME'));

 if (Action = 'NETWORK') and (Length(WorkBuffer) > 0) then
  begin
   {Add Header (2 column with Caption)}
   AddHeaderEx(AResponse,GetTitle,'Network Device Information',Self,2);
 
   {Get Id}
   Id:=StrToIntDef(WorkBuffer,0);
 
   {Get Network}
   NetworkDevice:=NetworkDeviceFind(Id);
   if NetworkDevice <> nil then
    begin
     {Get Flags Names}
     FlagNames:=NetworkFlagsToFlagNames(NetworkDevice.Device.DeviceFlags);
     
     AddBold(AResponse,'Network Device','');
     AddBlank(AResponse);
     AddItem(AResponse,'Name:',NetworkDevice.Device.DeviceName);
     AddItem(AResponse,'Class:',DeviceClassToString(NetworkDevice.Device.DeviceClass));
     AddItem(AResponse,'Bus:',DeviceBusToString(NetworkDevice.Device.DeviceBus));
     AddItem(AResponse,'Type:',NetworkDeviceTypeToString(NetworkDevice.Device.DeviceType));
     AddItem(AResponse,'Flags:',FlagNames.Strings[0]);
     
     {Check Flag Count}
     if FlagNames.Count > 1 then
      begin
       for Count:=1 to FlagNames.Count - 1 do
        begin
         {Add Flag Name}
         AddItem(AResponse,'',FlagNames.Strings[Count]);
        end;
      end;
     
     AddItem(AResponse,'Description:',NetworkDevice.Device.DeviceDescription);
     AddBlank(AResponse);
     AddItem(AResponse,'Id:',IntToStr(NetworkDevice.NetworkId));
     AddItem(AResponse,'State:',NetworkDeviceStateToString(NetworkDevice.NetworkState));
     AddItem(AResponse,'Status:',NetworkDeviceStatusToString(NetworkDevice.NetworkStatus));
     AddBlank(AResponse);
     AddBold(AResponse,'Statistics','');
     AddBlank(AResponse);
     AddItem(AResponse,'Receive Bytes:',IntToStr(NetworkDevice.ReceiveBytes));
     AddItem(AResponse,'Receive Count:',IntToStr(NetworkDevice.ReceiveCount));
     AddItem(AResponse,'Receive Errors:',IntToStr(NetworkDevice.ReceiveErrors));
     AddItem(AResponse,'Transmit Bytes:',IntToStr(NetworkDevice.TransmitBytes));
     AddItem(AResponse,'Transmit Count:',IntToStr(NetworkDevice.TransmitCount));
     AddItem(AResponse,'Transmit Errors:',IntToStr(NetworkDevice.TransmitErrors));
     AddItem(AResponse,'Buffer Overruns:',IntToStr(NetworkDevice.BufferOverruns));
     AddItem(AResponse,'Buffer Unavailable:',IntToStr(NetworkDevice.BufferUnavailable));
     
     FlagNames.Free;
    end
   else
    begin
     AddItem(AResponse,'Not Found','');
    end;    
 
   {Add Footer}
   AddFooter(AResponse); 
  end
 else if (Action = 'ADAPTER') and (Length(Name) > 0) then
  begin
   {Add Header (2 column with Caption)}
   AddHeaderEx(AResponse,GetTitle,'Adapter Information',Self,2);

   {Get Adapter}
   Adapter:=AdapterManager.GetAdapterByName(Name,True,NETWORK_LOCK_READ);
   if Adapter <> nil then
    begin
     AddBold(AResponse,'Adapter','');
     AddBlank(AResponse);
     AddItem(AResponse,'Name:',Adapter.Name);
     AddBlank(AResponse);
     AddItem(AResponse,'State:',AdapterStateToString(Adapter.State));
     AddItem(AResponse,'Status:',AdapterStatusToString(Adapter.Status));
     AddItem(AResponse,'Media Type:',Network.MediaTypeToString(Adapter.MediaType));
     AddItem(AResponse,'Adapter Type:',AdapterTypeToString(Adapter.AdapterType));
     AddBlank(AResponse);
     AddItem(AResponse,'Buffered Receive:',BooleanToString(Adapter.BufferedReceive));
     AddItem(AResponse,'Buffered Transmit:',BooleanToString(Adapter.BufferedTransmit));
     AddBlank(AResponse);
     AddItem(AResponse,'Last Error:',ErrorToString(Adapter.LastError));
     AddItem(AResponse,'Thread:',ThreadGetName(Adapter.ThreadID) + ' (0x' + IntToHex(Adapter.ThreadID,8) + ')');
     AddBlank(AResponse);
     AddItem(AResponse,'Hardware Address:',HardwareAddressToString(Adapter.GetHardwareAddress(INVALID_HANDLE_VALUE)));
     AddBlank(AResponse);
     
     {Get Statistics}
     AdapterStatistics:=Adapter.GetStatistics(INVALID_HANDLE_VALUE);
     AddBold(AResponse,'Statistics','');
     AddBlank(AResponse);
     AddItem(AResponse,'Packets In:',IntToStr(AdapterStatistics.PacketsIn));
     AddItem(AResponse,'Packets Out:',IntToStr(AdapterStatistics.PacketsOut));
     AddItem(AResponse,'Bytes In:',IntToStr(AdapterStatistics.BytesIn));
     AddItem(AResponse,'Bytes Out:',IntToStr(AdapterStatistics.BytesOut));
     AddItem(AResponse,'Errors In:',IntToStr(AdapterStatistics.ErrorsIn));
     AddItem(AResponse,'Errors Out:',IntToStr(AdapterStatistics.ErrorsOut));
     AddItem(AResponse,'Packets Lost:',IntToStr(AdapterStatistics.PacketsLost));
     
     Adapter.ReaderUnlock;
    end
   else
    begin   
     AddItem(AResponse,'Not Found','');
    end; 
 
   {Add Footer}
   AddFooter(AResponse);
  end
 else if (Action = 'TRANSPORT') and (Length(Name) > 0) then
  begin
   {Add Header (2 column with Caption)}
   AddHeaderEx(AResponse,GetTitle,'Transport Information',Self,2);

   {Get Transport}
   Transport:=TransportManager.GetTransportByName(Name,True,NETWORK_LOCK_READ);
   if Transport <> nil then
    begin
     AddBold(AResponse,'Transport','');
     AddBlank(AResponse);
     AddItem(AResponse,'Name:',Transport.Name);
     AddBlank(AResponse);
     AddItem(AResponse,'Family:',AddressFamilyToString(Transport.Family));
     AddItem(AResponse,'Packet Type:',PacketTypeToString(Transport.PacketType));

     {Check Transport}
     if Transport.Name = 'ARP' then
      begin
       {GetIpNetTable}
       AddBlank(AResponse);
       AddBold(AResponse,'ARP Table','');
       Size:=0;
       IpNetTable:=nil;
       if (GetIpNetTable(nil,Size,False) = ERROR_INSUFFICIENT_BUFFER) and (Size > 0) then {First call with zero size}
        begin
         IpNetTable:=GetMem(Size);
        end; 
       if IpNetTable <> nil then
        begin
         if GetIpNetTable(IpNetTable,Size,False) = ERROR_SUCCESS then 
          begin
           for Count:=0 to IpNetTable.dwNumEntries - 1 do
            begin
             AddBlank(AResponse);
             System.Move(IpNetTable.table[Count].bPhysAddr[0],HardwareAddress[0],HARDWARE_ADDRESS_SIZE);
             AddItem(AResponse,'Physical Address:',HardwareAddressToString(HardwareAddress));
             IPAddress.S_addr:=IpNetTable.table[Count].dwAddr;
             AddItem(AResponse,'IP Address:',InAddrToString(IPAddress));
             AddItem(AResponse,'Type:',IntToStr(IpNetTable.table[Count].dwType));
             AddItem(AResponse,'Index:',IntToStr(IpNetTable.table[Count].dwIndex));
            end;
          end;
          
         FreeMem(IpNetTable); 
        end;
      end
     else if Transport.Name = 'IP' then
      begin
       {GetNumberOfInterfaces}
       GetNumberOfInterfaces(Count);
       AddBlank(AResponse);
       AddItem(AResponse,'Number of Interfaces:',IntToStr(Count));
       
       {GetIfTable}
       if Count > 0 then
        begin
         AddBlank(AResponse);
         AddBold(AResponse,'Interfaces','');
         Size:=SizeOf(MIB_IFTABLE) + (Count * SizeOf(MIB_IFROW));
         IfTable:=GetMem(Size);
         if IfTable <> nil then
          begin
           if GetIfTable(IfTable,Size,False) = ERROR_SUCCESS then
            begin
             for Count:=0 to IfTable.dwNumEntries - 1 do
              begin
               AddBlank(AResponse);
               AddItem(AResponse,'Name:',IfTable.table[Count].wszName);
               AddItem(AResponse,'Index:',IntToStr(IfTable.table[Count].dwIndex));
               AddItem(AResponse,'Type:',IntToStr(IfTable.table[Count].dwType));
               AddItem(AResponse,'MTU:',IntToStr(IfTable.table[Count].dwMtu));
               System.Move(IfTable.table[Count].bPhysAddr[0],HardwareAddress[0],HARDWARE_ADDRESS_SIZE);
               AddItem(AResponse,'Physical Address:',HardwareAddressToString(HardwareAddress));
              end;
            end;  
            
           FreeMem(IfTable);
          end;
        end;
       
       {GetNumberOfInterfaces}
       GetNumberOfInterfaces(Count);
       
       {GetIpAddrTable}
       if Count > 0 then
        begin
         AddBlank(AResponse);
         AddBold(AResponse,'Addresses','');
         Size:=SizeOf(MIB_IPADDRTABLE) + (Count * SizeOf(MIB_IPADDRROW));
         IpAddrTable:=GetMem(Size);
         if IpAddrTable <> nil then
          begin
           if GetIpAddrTable(IpAddrTable,Size,False) = ERROR_SUCCESS then
            begin
             for Count:=0 to IpAddrTable.dwNumEntries - 1 do
              begin
               AddBlank(AResponse);
               IPAddress.S_addr:=IpAddrTable.table[Count].dwAddr;
               AddItem(AResponse,'Address:',InAddrToString(IPAddress));
               IPAddress.S_addr:=IpAddrTable.table[Count].dwMask;
               AddItem(AResponse,'Netmask:',InAddrToString(IPAddress));
               IPAddress.S_addr:=IpAddrTable.table[Count].dwBCastAddr;
               AddItem(AResponse,'Broadcast:',InAddrToString(IPAddress));
               AddItem(AResponse,'Index:',IntToStr(IpAddrTable.table[Count].dwIndex));
              end;
            end;  
           
           FreeMem(IpAddrTable);
          end;
        end;  
       
       {GetIpForwardTable}
       AddBlank(AResponse);
       AddBold(AResponse,'Routes','');
       Size:=0;
       IpForwardTable:=nil;
       if (GetIpForwardTable(nil,Size,False) = ERROR_INSUFFICIENT_BUFFER) and (Size > 0) then {First call with zero size}
        begin
         IpForwardTable:=GetMem(Size);
        end; 
       if IpForwardTable <> nil then
        begin
         if GetIpForwardTable(IpForwardTable,Size,False) = ERROR_SUCCESS then 
          begin
           for Count:=0 to IpForwardTable.dwNumEntries - 1 do
            begin
             AddBlank(AResponse);
             IPAddress.S_addr:=IpForwardTable.table[Count].dwForwardDest;
             AddItem(AResponse,'Destination:',InAddrToString(IPAddress));
             IPAddress.S_addr:=IpForwardTable.table[Count].dwForwardMask;
             AddItem(AResponse,'Netmask:',InAddrToString(IPAddress));
             AddItem(AResponse,'Policy:',IntToStr(IpForwardTable.table[Count].dwForwardPolicy));
             if IpForwardTable.table[Count].dwForwardNextHop = 0 then
              begin
               AddItem(AResponse,'Gateway:','Local');
              end
             else
              begin
               IPAddress.S_addr:=IpForwardTable.table[Count].dwForwardNextHop;
               AddItem(AResponse,'Gateway:',InAddrToString(IPAddress));
              end;
             AddItem(AResponse,'Index:',IntToStr(IpForwardTable.table[Count].dwForwardIfIndex));
             AddItem(AResponse,'Type:',IntToStr(IpForwardTable.table[Count].dwForwardType));
             AddItem(AResponse,'Protocol:',IntToStr(IpForwardTable.table[Count].dwForwardProto));
             AddItem(AResponse,'Age:',IntToStr(IpForwardTable.table[Count].dwForwardAge));
             AddItem(AResponse,'Metric:',IntToStr(IpForwardTable.table[Count].dwForwardMetric1));
            end;
          end;
          
         FreeMem(IpForwardTable); 
        end;
      
       {GetIpStatistics}
       AddBlank(AResponse);
       AddBold(AResponse,'Statistics','');
       //To Do 
      end;
     
     Transport.ReaderUnlock;
    end
   else
    begin   
     AddItem(AResponse,'Not Found','');
    end; 
 
   {Add Footer}
   AddFooter(AResponse);
  end
 else if (Action = 'PROTOCOL') and (Length(Name) > 0) then
  begin
   {Add Header (2 column with Caption)}
   AddHeaderEx(AResponse,GetTitle,'Protocol Information',Self,2);

   {Get Protocol}
   Protocol:=ProtocolManager.GetProtocolByName(Name,True,NETWORK_LOCK_READ);
   if Protocol <> nil then
    begin
     AddBold(AResponse,'Protocol','');
     AddBlank(AResponse);
     AddItem(AResponse,'Name:',Protocol.Name);
     AddBlank(AResponse);
     AddItem(AResponse,'Protocol:',ProtocolToString(Protocol.Protocol));
     AddItem(AResponse,'Socket Type:',SocketTypeToString(Protocol.SocketType));

     {Check Protocol}
     if Protocol.Name = 'ICMP' then
      begin
       {GetIcmpStatistics}
       AddBlank(AResponse);
       AddBold(AResponse,'Statistics','');
       //To Do //
      
      end
     else if Protocol.Name = 'TCP' then
      begin
       {GetTcpTable}
       AddBlank(AResponse);
       AddBold(AResponse,'TCP Table','');
       Size:=0;
       TcpTable:=nil;
       if (GetTcpTable(nil,Size,False) = ERROR_INSUFFICIENT_BUFFER) and (Size > 0) then {First call with zero size}
        begin
         TcpTable:=GetMem(Size);
        end; 
       if TcpTable <> nil then
        begin
         if GetTcpTable(TcpTable,Size,False) = ERROR_SUCCESS then 
          begin
           for Count:=0 to TcpTable.dwNumEntries - 1 do
            begin
             AddBlank(AResponse);
             AddItem(AResponse,'State:',MIBTCPStateToString(TcpTable.table[Count].dwState));
             IPAddress.S_addr:=TcpTable.table[Count].dwLocalAddr;
             AddItem(AResponse,'Local Address:',InAddrToString(IPAddress));
             AddItem(AResponse,'Local Port:',IntToStr(WordBEtoN(TcpTable.table[Count].dwLocalPort)));
             IPAddress.S_addr:=TcpTable.table[Count].dwRemoteAddr;
             AddItem(AResponse,'Remote Address:',InAddrToString(IPAddress));
             AddItem(AResponse,'Remote Port:',IntToStr(WordBEtoN(TcpTable.table[Count].dwRemotePort)));
            end;
          end;
          
         FreeMem(TcpTable); 
        end;
       
       {GetTcpStatistics}
       AddBlank(AResponse);
       AddBold(AResponse,'Statistics','');
       //To Do //
      
      end
     else if Protocol.Name = 'UDP' then
      begin
       {GetUdpTable}
       AddBlank(AResponse);
       AddBold(AResponse,'UDP Table','');
       Size:=0;
       UdpTable:=nil;
       if (GetUdpTable(nil,Size,False) = ERROR_INSUFFICIENT_BUFFER) and (Size > 0) then {First call with zero size}
        begin
         UdpTable:=GetMem(Size);
        end; 
       if UdpTable <> nil then
        begin
         if GetUdpTable(UdpTable,Size,False) = ERROR_SUCCESS then 
          begin
           for Count:=0 to UdpTable.dwNumEntries - 1 do
            begin
             AddBlank(AResponse);
             IPAddress.S_addr:=UdpTable.table[Count].dwLocalAddr;
             AddItem(AResponse,'Local Address:',InAddrToString(IPAddress));
             AddItem(AResponse,'Local Port:',IntToStr(WordBEtoN(UdpTable.table[Count].dwLocalPort)));
            end;
          end;
          
         FreeMem(UdpTable); 
        end;
       
       {GetUdpStatistics}
       AddBlank(AResponse);
       AddBold(AResponse,'Statistics','');
       //To Do //
      
      end;
    
     Protocol.ReaderUnlock;
    end
   else
    begin   
     AddItem(AResponse,'Not Found','');
    end; 
 
   {Add Footer}
   AddFooter(AResponse);
  end
 else
  begin 
   {Add Header (5 column)}
   AddHeaderEx(AResponse,GetTitle,'',Self,5); 
  
   {Add Network Info} 
   AddBold5Column(AResponse,'Details','','','','');
   AddBlankEx(AResponse,5);
  
   {Add Host Name/Domain}
   AddItem5Column(AResponse,'Host Name:',HostGetName,'','','');
   AddItem5Column(AResponse,'Domain Name:',HostGetDomain,'','','');
   
   {Add Network Devices}
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
   Data.Data:=nil;
   
   {Enumerate Networks}
   NetworkDeviceEnumerate(WebStatusNetworkEnumerate,@Data);
   
   {Add Adapters}
   AddBlankEx(AResponse,5); 
   AddBold5Column(AResponse,'Adapters','','','',''); 
   AddBlankEx(AResponse,5);
   AddBold5Column(AResponse,'Name','State','Status','Media Type','');
   AddBlankEx(AResponse,5);
  
   Adapter:=AdapterManager.GetAdapterByNext(nil,True,False,NETWORK_LOCK_READ);
   while Adapter <> nil do
    begin
     {Add Adapter}
     AddItem5Column(AResponse,MakeLink(Adapter.Name,Name + '?action=adapter&name=' + Adapter.Name),AdapterStateToString(Adapter.State),AdapterStatusToString(Adapter.Status),Network.MediaTypeToString(Adapter.MediaType),''); 
     
     Adapter:=AdapterManager.GetAdapterByNext(Adapter,True,True,NETWORK_LOCK_READ);
    end;
   
   {Add Transports}
   AddBlankEx(AResponse,5); 
   AddBold5Column(AResponse,'Transports','','','',''); 
   AddBlankEx(AResponse,5);
   AddBold5Column(AResponse,'Name','Family','Packet Type','','');
   AddBlankEx(AResponse,5);
   
   Transport:=TransportManager.GetTransportByNext(nil,True,False,NETWORK_LOCK_READ);
   while Transport <> nil do
    begin
     {Add Transport}
     AddItem5Column(AResponse,MakeLink(Transport.Name,Name + '?action=transport&name=' + Transport.Name),AddressFamilyToString(Transport.Family),PacketTypeToString(Transport.PacketType),'',''); 
    
     Transport:=TransportManager.GetTransportByNext(Transport,True,True,NETWORK_LOCK_READ);
    end;
   
   {Add Protocols}
   AddBlankEx(AResponse,5); 
   AddBold5Column(AResponse,'Protocols','','','',''); 
   AddBlankEx(AResponse,5);
   AddBold5Column(AResponse,'Name','Protocol','Socket Type','','');
   AddBlankEx(AResponse,5);
   
   Protocol:=ProtocolManager.GetProtocolByNext(nil,True,False,NETWORK_LOCK_READ);
   while Protocol <> nil do
    begin
     {Add Protocol}
     AddItem5Column(AResponse,MakeLink(Protocol.Name,Name + '?action=protocol&name=' + Protocol.Name),ProtocolToString(Protocol.Protocol),SocketTypeToString(Protocol.SocketType),'',''); 
    
     Protocol:=ProtocolManager.GetProtocolByNext(Protocol,True,True,NETWORK_LOCK_READ);
    end;
   
   {Add Footer (5 column)}
   AddFooterEx(AResponse,5); 
  end;
  
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
 AddHeaderEx(AResponse,GetTitle,'',Self,4); 

 {Add Storage List} 
 AddBold4Column(AResponse,'Storage Id','Name','State','Type');
 AddBlankEx(AResponse,4);
 
 {Setup Data}
 Data.Document:=Self;
 Data.Host:=AHost;
 Data.Request:=ARequest;
 Data.Response:=AResponse;
 Data.Data:=nil;
 
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
     AddItemEx(AResponse,'MediaType:',FileSystem.MediaTypeToString(Device.MediaType),5);
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
 AddHeaderEx(AResponse,GetTitle,'',Self,4); 

 {Add Keyboard List} 
 AddBold4Column(AResponse,'Keyboard Id','Name','State','Type');
 AddBlankEx(AResponse,4);
 
 {Setup Data}
 Data.Document:=Self;
 Data.Host:=AHost;
 Data.Request:=ARequest;
 Data.Response:=AResponse;
 Data.Data:=nil;
 
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
 AddHeaderEx(AResponse,GetTitle,'',Self,4); 

 {Add Mouse List} 
 AddBold4Column(AResponse,'Mouse Id','Name','State','Type');
 AddBlankEx(AResponse,4);
 
 {Setup Data}
 Data.Document:=Self;
 Data.Host:=AHost;
 Data.Request:=ARequest;
 Data.Response:=AResponse;
 Data.Data:=nil;
 
 {Enumerate Mice}
 MouseDeviceEnumerate(WebStatusMouseEnumerate,@Data);
 
 {Add Footer (4 column)}
 AddFooterEx(AResponse,4); 
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TWebStatusTouch}
constructor TWebStatusTouch.Create(AMain:TWebStatusMain);
begin
 {}
 FCaption:='Touch'; {Must be before create for register}
 inherited Create(AMain);
 Name:='/touch';
 
 if FMain <> nil then Name:=FMain.Name + Name;
end; 

{==============================================================================}

function TWebStatusTouch.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
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
 AddHeaderEx(AResponse,GetTitle,'',Self,4); 

 {Add Touch List} 
 AddBold4Column(AResponse,'Touch Id','Name','State','Type');
 AddBlankEx(AResponse,4);
 
 {Setup Data}
 Data.Document:=Self;
 Data.Host:=AHost;
 Data.Request:=ARequest;
 Data.Response:=AResponse;
 Data.Data:=nil;
 
 {Enumerate Touch}
 TouchDeviceEnumerate(WebStatusTouchEnumerate,@Data);
 
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
 DisplayCount:LongWord;
 FramebufferCount:LongWord;
 FramebufferDevice:PFramebufferDevice;
 FramebufferProperties:TFramebufferProperties;
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

 AddBold(AResponse,'Default Settings','');
 
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
   AddItemEx(AResponse,'Colour Depth (Bits per Pixel):',FramebufferDepthToString(Depth),2);
  end;

 {Add Framebuffer Pixel Order}
 if FramebufferGetPixelOrder(Order) = ERROR_SUCCESS then
  begin
   AddBlank(AResponse);
   AddItemEx(AResponse,'Pixel Order (BGR/RGB):',FramebufferOrderToString(Order),2);
  end;

 {Add Framebuffer Alpha Mode}
 if FramebufferGetAlphaMode(Mode) = ERROR_SUCCESS then
  begin
   AddBlank(AResponse);
   AddItemEx(AResponse,'Alpha Mode:',FramebufferModeToString(Mode),2);
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
 
 {Add Multiple Displays}
 if FramebufferGetNumDisplays(DisplayCount) = ERROR_SUCCESS then
  begin
   AddBlank(AResponse);
   AddItemEx(AResponse,'Display Count:',IntToStr(DisplayCount),2);
  end;
 
 {Get Default Device}
 if FramebufferDeviceGetDefault <> nil then
  begin
   AddBlank(AResponse);
   AddBold(AResponse,FramebufferDeviceGetDefault.Device.DeviceName + ' Properties (Default)','');
   
   {Get Default Device Properties}
   if FramebufferDeviceGetProperties(FramebufferDeviceGetDefault,@FramebufferProperties) = ERROR_SUCCESS then
    begin
     AddBlank(AResponse);
     AddItemEx(AResponse,'Flags:',IntToHex(FramebufferProperties.Flags,8),2);
     AddBlank(AResponse);
     AddItemEx(AResponse,'Address:','0x' + AddrToHex(FramebufferProperties.Address),2);
     AddItemEx(AResponse,'Size:',IntToStr(FramebufferProperties.Size),2);
     AddBlank(AResponse);
     AddItemEx(AResponse,'Pitch (Bytes per Line):',IntToStr(FramebufferProperties.Pitch),2);
     AddBlank(AResponse);
     AddItemEx(AResponse,'Colour Depth (Bits per Pixel):',FramebufferDepthToString(FramebufferProperties.Depth),2);
     AddBlank(AResponse);
     AddItemEx(AResponse,'Pixel Order (BGR/RGB):',FramebufferOrderToString(FramebufferProperties.Order),2);
     AddBlank(AResponse);
     AddItemEx(AResponse,'Alpha Mode:',FramebufferModeToString(FramebufferProperties.Mode),2);
     AddBlank(AResponse);
     AddItemEx(AResponse,'Color Format:',ColorFormatToString(FramebufferProperties.Format),2);
     AddBlank(AResponse);
     AddItemEx(AResponse,'Physical Width (Pixels):',IntToStr(FramebufferProperties.PhysicalWidth),2);
     AddItemEx(AResponse,'Physical Height (Pixels):',IntToStr(FramebufferProperties.PhysicalHeight),2);
     AddBlank(AResponse);
     AddItemEx(AResponse,'Virtual Width (Pixels):',IntToStr(FramebufferProperties.VirtualWidth),2);
     AddItemEx(AResponse,'Virtual Height (Pixels):',IntToStr(FramebufferProperties.VirtualHeight),2);
     AddBlank(AResponse);
     AddItemEx(AResponse,'Virtual Offset X (Pixels):',IntToStr(FramebufferProperties.OffsetX),2);
     AddItemEx(AResponse,'Virtual Offset Y (Pixels):',IntToStr(FramebufferProperties.OffsetY),2);
     AddBlank(AResponse);
     AddItemEx(AResponse,'Overscan Top (Pixels):',IntToStr(FramebufferProperties.OverscanTop),2);
     AddItemEx(AResponse,'Overscan Bottom (Pixels):',IntToStr(FramebufferProperties.OverscanBottom),2);
     AddItemEx(AResponse,'Overscan Left (Pixels):',IntToStr(FramebufferProperties.OverscanLeft),2);
     AddItemEx(AResponse,'Overscan Right (Pixels):',IntToStr(FramebufferProperties.OverscanRight),2);
     AddBlank(AResponse);
     AddItemEx(AResponse,'Rotation:',FramebufferRotationToString(FramebufferProperties.Rotation),2);
     AddBlank(AResponse);
     AddItemEx(AResponse,'Cursor X (Pixels):',IntToStr(FramebufferProperties.CursorX),2);
     AddItemEx(AResponse,'Cursor Y (Pixels):',IntToStr(FramebufferProperties.CursorY),2);
     AddItemEx(AResponse,'Cursor State:',FramebufferCursorToString(FramebufferProperties.CursorState),2);
    end;
  end;
  
 {Check Device Count}
 if FramebufferDeviceGetCount > 1 then
  begin
   for FramebufferCount:=1 to FramebufferDeviceGetCount - 1 do
    begin
     {Get Framebuffer Device}
     FramebufferDevice:=FramebufferDeviceFind(FramebufferCount);
     if FramebufferDevice <> nil then
      begin
       AddBlank(AResponse);
       AddBold(AResponse,FramebufferDevice.Device.DeviceName + ' Properties','');
     
       {Get Framebuffer Device Properties}
       if FramebufferDeviceGetProperties(FramebufferDevice,@FramebufferProperties) = ERROR_SUCCESS then
        begin
         AddBlank(AResponse);
         AddItemEx(AResponse,'Flags:',IntToHex(FramebufferProperties.Flags,8),2);
         AddBlank(AResponse);
         AddItemEx(AResponse,'Address:','0x' + AddrToHex(FramebufferProperties.Address),2);
         AddItemEx(AResponse,'Size:',IntToStr(FramebufferProperties.Size),2);
         AddBlank(AResponse);
         AddItemEx(AResponse,'Pitch (Bytes per Line):',IntToStr(FramebufferProperties.Pitch),2);
         AddBlank(AResponse);
         AddItemEx(AResponse,'Colour Depth (Bits per Pixel):',FramebufferDepthToString(FramebufferProperties.Depth),2);
         AddBlank(AResponse);
         AddItemEx(AResponse,'Pixel Order (BGR/RGB):',FramebufferOrderToString(FramebufferProperties.Order),2);
         AddBlank(AResponse);
         AddItemEx(AResponse,'Alpha Mode:',FramebufferModeToString(FramebufferProperties.Mode),2);
         AddBlank(AResponse);
         AddItemEx(AResponse,'Color Format:',ColorFormatToString(FramebufferProperties.Format),2);
         AddBlank(AResponse);
         AddItemEx(AResponse,'Physical Width (Pixels):',IntToStr(FramebufferProperties.PhysicalWidth),2);
         AddItemEx(AResponse,'Physical Height (Pixels):',IntToStr(FramebufferProperties.PhysicalHeight),2);
         AddBlank(AResponse);
         AddItemEx(AResponse,'Virtual Width (Pixels):',IntToStr(FramebufferProperties.VirtualWidth),2);
         AddItemEx(AResponse,'Virtual Height (Pixels):',IntToStr(FramebufferProperties.VirtualHeight),2);
         AddBlank(AResponse);
         AddItemEx(AResponse,'Virtual Offset X (Pixels):',IntToStr(FramebufferProperties.OffsetX),2);
         AddItemEx(AResponse,'Virtual Offset Y (Pixels):',IntToStr(FramebufferProperties.OffsetY),2);
         AddBlank(AResponse);
         AddItemEx(AResponse,'Overscan Top (Pixels):',IntToStr(FramebufferProperties.OverscanTop),2);
         AddItemEx(AResponse,'Overscan Bottom (Pixels):',IntToStr(FramebufferProperties.OverscanBottom),2);
         AddItemEx(AResponse,'Overscan Left (Pixels):',IntToStr(FramebufferProperties.OverscanLeft),2);
         AddItemEx(AResponse,'Overscan Right (Pixels):',IntToStr(FramebufferProperties.OverscanRight),2);
         AddBlank(AResponse);
         AddItemEx(AResponse,'Rotation:',FramebufferRotationToString(FramebufferProperties.Rotation),2);
         AddBlank(AResponse);
         AddItemEx(AResponse,'Cursor X (Pixels):',IntToStr(FramebufferProperties.CursorX),2);
         AddItemEx(AResponse,'Cursor Y (Pixels):',IntToStr(FramebufferProperties.CursorY),2);
         AddItemEx(AResponse,'Cursor State:',FramebufferCursorToString(FramebufferProperties.CursorState),2);
        end;
      end;
    end;
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
 
 {Add Device Tree}
 AddBold(AResponse,'Device Tree','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'Device Tree Base:','0x' + AddrToHex(DeviceTreeGetBase),2);
 AddItemEx(AResponse,'Device Tree Size:',IntToStr(DeviceTreeGetSize),2);
 AddItemEx(AResponse,'Device Tree Valid:',BooleanToString(DeviceTreeValid),2);
 AddBlank(AResponse);

 {Add Initial Ramdisk}
 AddBold(AResponse,'Initial Ramdisk','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'Initial Ramdisk Base:','0x' + AddrToHex(INITIAL_RAMDISK_BASE),2);
 AddItemEx(AResponse,'Initial Ramdisk Size:',IntToStr(INITIAL_RAMDISK_SIZE),2);
 AddBlank(AResponse);
 
 {$IFDEF CPUARM}
 {Add ARM Specific}
 AddBold(AResponse,'ARM Specific','');
 AddBlank(AResponse);
 
 {Add ARM Boot Mode}
 AddItemEx(AResponse,'ARM Boot Mode:','0x' + IntToHex(ARMBootMode,8) + ' (' + ARMModeToString(ARMBootMode) + ')',2);

 {Add ARM Boot Vectors}
 AddItemEx(AResponse,'ARM Boot Vectors:','0x' + IntToHex(ARMBootVectors,8),2);
 
 {Add ARM Machine Type}
 AddItemEx(AResponse,'ARM Machine Type:','0x' + IntToHex(ARMMachineType,8),2);
 AddBlank(AResponse);
 
 {Add ARM Secure Boot}
 AddItemEx(AResponse,'ARM Secure Boot:',IntToStr(ARMSecureBoot),2);
 AddBlank(AResponse);

 {Add ARM Emulator Mode}
 AddItemEx(AResponse,'ARM Emulator Mode:',IntToStr(ARMEmulatorMode),2);
 AddBlank(AResponse);

 {Add ARM Boot Tags}
 AddBold(AResponse,'ARM Boot Tags','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'Tags Address:','0x' + AddrToHex(ARMTagsAddress),2);
 AddItemEx(AResponse,'Tags Count:',IntToStr(ARMTagsCount),2);
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
 AddItemEx(AResponse,'Memory Address:','0x' + AddrToHex(TagMemoryAddress),4);
 AddBlank(AResponse);
 
 AddItemEx(AResponse,'Tag Video Text Count:',IntToStr(TagVideoTextCount),2);
 AddBlank(AResponse);
 
 AddItemEx(AResponse,'Tag Ramdisk Count:',IntToStr(TagRamdiskCount),2);
 AddBlank(AResponse);
 
 AddItemEx(AResponse,'Tag Init RD Count:',IntToStr(TagInitRdCount),2);
 AddBlank(AResponse);

 AddItemEx(AResponse,'Tag Init RD2 Count:',IntToStr(TagInitRd2Count),2);
 AddItemEx(AResponse,'Ramdisk Start:','0x' + IntToHex(TagInitRd2Start,8),4);
 AddItemEx(AResponse,'Ramdisk Size:',IntToStr(TagInitRd2Size),4);
 AddBlank(AResponse);
 
 AddItemEx(AResponse,'Tag Serial Count:',IntToStr(TagSerialCount),2);
 AddItemEx(AResponse,'Serial No Low:','0x' + IntToHex(TagSerialNoLow,8),4);
 AddItemEx(AResponse,'Serial No High:','0x' + IntToHex(TagSerialNoHigh,8),4);
 AddBlank(AResponse);
 
 AddItemEx(AResponse,'Tag Revision Count:',IntToStr(TagRevisionCount),2);
 AddItemEx(AResponse,'Revision No:','0x' + IntToHex(TagRevisionNo,8),4);
 AddBlank(AResponse);
 
 AddItemEx(AResponse,'Tag Video FB Count:',IntToStr(TagVideoFBCount),2);
 AddBlank(AResponse);
 
 AddItemEx(AResponse,'Tag Command Count:',IntToStr(TagCmdCount),2);
 AddItemEx(AResponse,'Command Size:',IntToStr(TagCommandSize),4);
 AddItemEx(AResponse,'Command Count:',IntToStr(TagCommandCount),4);
 AddItemEx(AResponse,'Command Address:','0x' + PtrToHex(TagCommandAddress),4);
 AddBlank(AResponse);
 {$ENDIF}
 
 {$IFDEF CPUAARCH64}
 {Add AARCH64 Specific}
 AddBold(AResponse,'AARCH64 Specific','');
 AddBlank(AResponse);

 //To Do
 
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
 Address:PtrUInt;
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
 AddHeaderEx(AResponse,GetTitle,'',Self,4); 

 {Add Page Tables}
 Address:=$00000000;
 Repeated:=0;
 FillChar(NextEntry,SizeOf(TPageTableEntry),0);
 FillChar(CurrentEntry,SizeOf(TPageTableEntry),0);
 AddBold4Column(AResponse,'Virtual','Physical','Size','Flags');
 AddBlankEx(AResponse,4);
 {Get First} 
 PageTableGetEntry(Address,NextEntry);
 while NextEntry.Size > 0 do
  begin
   if (NextEntry.Size <> CurrentEntry.Size) or (NextEntry.Flags <> CurrentEntry.Flags){$IFDEF CPU32} or (NextEntry.PhysicalRange <> CurrentEntry.PhysicalRange){$ENDIF CPU32} then
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
     AddItem4Column(AResponse,'0x' + AddrToHex(NextEntry.VirtualAddress),'0x' + {$IFDEF CPU32}IntToHex(NextEntry.PhysicalRange,8) + ':' + {$ENDIF CPU32}AddrToHex(NextEntry.PhysicalAddress),'0x' + IntToHex(NextEntry.Size,8),FlagNames.Strings[0]);
     
     {Check Flag Count}
     if FlagNames.Count > 1 then
      begin
       for Count:=1 to FlagNames.Count - 1 do
        begin
         {Add Flag Name}
         AddItem4Column(AResponse,'','','',FlagNames.Strings[Count]);
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
   PageTableGetEntry(Address,NextEntry);
  end;
 
 {Add Footer (4 column)}
 AddFooterEx(AResponse,4); 
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TWebStatusVectorTables}
constructor TWebStatusVectorTables.Create(AMain:TWebStatusMain);
begin
 {}
 FCaption:='Vector Tables'; {Must be before create for register}
 inherited Create(AMain);
 Name:='/vectortables';
 
 if FMain <> nil then Name:=FMain.Name + Name;
end; 

{==============================================================================}

function TWebStatusVectorTables.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
var
 Count:LongWord;
 Number:LongWord;
 Address:PtrUInt;
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

 {Add Vector Tables}
 AddBold(AResponse,'Number','Address');
 AddBlank(AResponse);
 
 {Get Count}
 Count:=VectorTableGetCount;
 if Count > 0 then
  begin
   for Number:=0 to Count - 1 do
    begin
     {Get Entry}
     Address:=VectorTableGetEntry(Number);
     
     {Add Entry}
     AddItem(AResponse,IntToStr(Number),'0x' + AddrToHex(Address));
    end; 
    
   {Add Blank}
   AddBlankEx(AResponse,4);
  end; 
  
 {Add Footer}
 AddFooter(AResponse); 
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TWebStatusIRQFIQSWI}
constructor TWebStatusIRQFIQSWI.Create(AMain:TWebStatusMain);
begin
 {}
 FCaption:='IRQ';
 if FIQ_ENABLED then FCaption:=FCaption + ' / FIQ';
 if IPI_ENABLED then FCaption:=FCaption + ' / IPI';
 if SWI_ENABLED then FCaption:=FCaption + ' / SWI';
 
 inherited Create(AMain);
 Name:='/irqfiqswi';
 
 if FMain <> nil then Name:=FMain.Name + Name;
end; 

{==============================================================================}

function TWebStatusIRQFIQSWI.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
var
 Start:LongWord;
 Count:LongWord;
 Number:LongWord;
 Instance:LongWord;
 CPUCount:LongWord;
 CPUString:String;
 InterruptEntry:TInterruptEntry;
 SystemCallEntry:TSystemCallEntry;
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
 AddHeaderEx(AResponse,GetTitle,'',Self,5); 

 {Get Count}
 Count:=GetInterruptCount;
 if Count > 0 then
  begin
   {Add Interrupt Entries}
   AddBold5Column(AResponse,'Interrupts','','','','');
   AddBlankEx(AResponse,5);
   if IRQ_ENABLED then AddItem5Column(AResponse,'IRQ_ROUTING',CPUIDToString(IRQ_ROUTING),'','','');
   if FIQ_ENABLED then AddItem5Column(AResponse,'FIQ_ROUTING',CPUIDToString(FIQ_ROUTING),'','','');
   AddBlankEx(AResponse,5);
   AddBold5Column(AResponse,'Number','CPUID','Handler','Extended Handler','Shared Handler');
   AddBlankEx(AResponse,5);
   
   {Get Start}
   Start:=GetInterruptStart;
   
   {Add Entries}
   for Number:=Start to (Start + Count) - 1 do
    begin
     Instance:=0;
     {Get Entry}
     while GetInterruptEntry(Number,Instance,InterruptEntry) = ERROR_SUCCESS do
      begin
       if Assigned(InterruptEntry.Handler) or Assigned(InterruptEntry.HandlerEx) or Assigned(InterruptEntry.SharedHandler) then
        begin
         {Get CPUID}
         if InterruptEntry.CPUMask = CPUGetMask then
          begin
           CPUString:=CPUIDToString(CPU_ID_ALL) + ' (' + IntToStr(CPUMaskCount(InterruptEntry.CPUMask)) + ')';
          end
         else
          begin
           CPUString:=CPUIDToString(InterruptEntry.CPUID);
          end;
         
         if Instance = 0 then
          begin
           {Add Entry}
           AddItem5Column(AResponse,IntToStr(InterruptEntry.Number),CPUString,'0x' + PtrToHex(@InterruptEntry.Handler),'0x' + PtrToHex(@InterruptEntry.HandlerEx),'0x' + PtrToHex(@InterruptEntry.SharedHandler));
          end
         else
          begin
           {Add Instance}
           AddItem5Column(AResponse,'',CPUString,'0x' + PtrToHex(@InterruptEntry.Handler),'0x' + PtrToHex(@InterruptEntry.HandlerEx),'0x' + PtrToHex(@InterruptEntry.SharedHandler));
          end;
        end; 
      
       Inc(Instance);
      end;
    end;

   {Add Blank}
   AddBlankEx(AResponse,5);
  end; 
 
 {Get Count}
 Count:=GetLocalInterruptCount;
 if Count > 0 then
  begin
   {Add Local Interrupt Entries}
   AddBold5Column(AResponse,'Local Interrupts','','','','');
   AddBlankEx(AResponse,5);
   AddBold5Column(AResponse,'Number','CPUID','Handler','Extended Handler','Shared Handler');
   AddBlankEx(AResponse,5);
   
   {Get Start}
   Start:=GetLocalInterruptStart;
   
   {Add Entries}
   for Number:=Start to (Start + Count) - 1 do
    begin
     for CPUCount:=0 to CPUGetCount - 1 do
      begin
       Instance:=0;
       {Get Entry}
       while GetLocalInterruptEntry(CPUCount,Number,Instance,InterruptEntry) = ERROR_SUCCESS do
        begin
         if Assigned(InterruptEntry.Handler) or Assigned(InterruptEntry.HandlerEx) or Assigned(InterruptEntry.SharedHandler) then
          begin
           {Get CPUID}
           if InterruptEntry.CPUMask = CPUGetMask then
            begin
             CPUString:=CPUIDToString(CPU_ID_ALL) + ' (' + IntToStr(CPUMaskCount(InterruptEntry.CPUMask)) + ')';
            end
           else
            begin
             CPUString:=CPUIDToString(InterruptEntry.CPUID);
            end;
           
           if Instance = 0 then
            begin
             {Add Entry}
             AddItem5Column(AResponse,IntToStr(InterruptEntry.Number),CPUString,'0x' + PtrToHex(@InterruptEntry.Handler),'0x' + PtrToHex(@InterruptEntry.HandlerEx),'0x' + PtrToHex(@InterruptEntry.SharedHandler));
            end
           else
            begin
             {Add Instance}
             AddItem5Column(AResponse,'',CPUString,'0x' + PtrToHex(@InterruptEntry.Handler),'0x' + PtrToHex(@InterruptEntry.HandlerEx),'0x' + PtrToHex(@InterruptEntry.SharedHandler));
            end; 
          end; 
        
         Inc(Instance);
        end;
      end;  
    end; 
    
   {Add Blank}
   AddBlankEx(AResponse,5);
  end; 
 
 {Get Count}
 Count:=GetSoftwareInterruptCount;
 if Count > 0 then
  begin
   {Add Software Interrupt Entries}
   AddBold5Column(AResponse,'Software Interrupts','','','','');
   AddBlankEx(AResponse,5);
   AddBold5Column(AResponse,'Number','CPUID','Handler','Extended Handler','Shared Handler');
   AddBlankEx(AResponse,5);
   
   {Get Start}
   Start:=GetSoftwareInterruptStart;
   
   {Add Entries}
   for Number:=Start to (Start + Count) - 1 do
    begin
     for CPUCount:=0 to CPUGetCount - 1 do
      begin
       Instance:=0;
       {Get Entry}
       while GetSoftwareInterruptEntry(CPUCount,Number,Instance,InterruptEntry) = ERROR_SUCCESS do
        begin
         if Assigned(InterruptEntry.Handler) or Assigned(InterruptEntry.HandlerEx) or Assigned(InterruptEntry.SharedHandler) then
          begin
           {Get CPUID}
           if InterruptEntry.CPUMask = CPUGetMask then
            begin
             CPUString:=CPUIDToString(CPU_ID_ALL) + ' (' + IntToStr(CPUMaskCount(InterruptEntry.CPUMask)) + ')';
            end
           else
            begin
             CPUString:=CPUIDToString(InterruptEntry.CPUID);
            end;
           
           if Instance = 0 then
            begin
             {Add Entry}
             AddItem5Column(AResponse,IntToStr(InterruptEntry.Number),CPUString,'0x' + PtrToHex(@InterruptEntry.Handler),'0x' + PtrToHex(@InterruptEntry.HandlerEx),'0x' + PtrToHex(@InterruptEntry.SharedHandler));
            end
           else
            begin
             {Add Instance}
             AddItem5Column(AResponse,'',CPUString,'0x' + PtrToHex(@InterruptEntry.Handler),'0x' + PtrToHex(@InterruptEntry.HandlerEx),'0x' + PtrToHex(@InterruptEntry.SharedHandler));
            end; 
          end; 
        
         Inc(Instance);
        end;
      end;  
    end; 
   
   {Add Blank}
   AddBlankEx(AResponse,5);
  end; 
 
 {Get Count}
 Count:=GetSystemCallCount;
 if Count > 0 then
  begin
   {Add System Call Entries}
   AddBold5Column(AResponse,'System Calls','','','','');
   AddBlankEx(AResponse,5);
   AddBold5Column(AResponse,'Number','CPUID','Handler','Extended Handler','');
   AddBlankEx(AResponse,5);

   {Add Entries}
   for Number:=0 to Count - 1 do
    begin
     {Get Entry}
     SystemCallEntry:=GetSystemCallEntry(Number);
     if Assigned(SystemCallEntry.Handler) or Assigned(SystemCallEntry.HandlerEx) then
      begin
       {Get CPUID}
       if SystemCallEntry.CPUID = CPU_ID_ALL then
        begin
         CPUString:=CPUIDToString(CPU_ID_ALL) + ' (' + IntToStr(CPUMaskCount(CPUGetMask)) + ')';
        end
       else
        begin
         CPUString:=CPUIDToString(SystemCallEntry.CPUID);
        end;
       
       {Add Entry}
       AddItem5Column(AResponse,IntToStr(SystemCallEntry.Number),CPUString,'0x' + PtrToHex(@SystemCallEntry.Handler),'0x' + PtrToHex(@SystemCallEntry.HandlerEx),'');
      end; 
    end; 
    
   {Add Blank}
   AddBlankEx(AResponse,5);
  end; 
 
 {$IF DEFINED(IRQ_STATISTICS) or DEFINED(FIQ_STATISTICS) or DEFINED(SWI_STATISTICS)}
 {Add Interrupt Debug}
 AddBold5Column(AResponse,'Interrupt Statistics','','','','');
 AddBlankEx(AResponse,5);
 AddBold5Column(AResponse,'Type','CPUID','Count','','');
 AddBlankEx(AResponse,5);
 {$IFDEF IRQ_STATISTICS}
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItem5Column(AResponse,'IRQ',CPUIDToString(Count),IntToStr(DispatchInterruptCounter[Count]),'','');
    end
   else
    begin
     AddItem5Column(AResponse,'',CPUIDToString(Count),IntToStr(DispatchInterruptCounter[Count]),'','');
    end;    
  end; 
 AddBlankEx(AResponse,5);
 {$ENDIF}
 {$IFDEF FIQ_STATISTICS}
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItem5Column(AResponse,'FIQ',CPUIDToString(Count),IntToStr(DispatchFastInterruptCounter[Count]),'','');
    end
   else
    begin
     AddItem5Column(AResponse,'',CPUIDToString(Count),IntToStr(DispatchFastInterruptCounter[Count]),'','');
    end;    
  end; 
 AddBlankEx(AResponse,5);
 {$ENDIF}
 {$IFDEF SWI_STATISTICS}
 for Count:=0 to CPUGetCount - 1 do
  begin
   if Count = CPU_ID_0 then
    begin
     AddItem5Column(AResponse,'SWI',CPUIDToString(Count),IntToStr(DispatchSystemCallCounter[Count]),'','');
    end
   else
    begin
     AddItem5Column(AResponse,'',CPUIDToString(Count),IntToStr(DispatchSystemCallCounter[Count]),'','');
    end;    
  end; 
 AddBlankEx(AResponse,5);
 {$ENDIF}
 {$ENDIF}
 
 {Add Footer (5 column)}
 AddFooterEx(AResponse,5); 
 
 {Return Result}
 Result:=True;
end;
 
{==============================================================================}
{==============================================================================}
{TWebStatusGPIO}
constructor TWebStatusGPIO.Create(AMain:TWebStatusMain);
begin
 {}
 FCaption:='GPIO'; {Must be before create for register}
 inherited Create(AMain);
 Name:='/gpio';
 
 if FMain <> nil then Name:=FMain.Name + Name;
end; 

{==============================================================================}

function TWebStatusGPIO.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; 
var
 Count:LongWord;
 GPIOCount:LongWord;
 GPIODevice:PGPIODevice;
 GPIOProperties:TGPIOProperties;
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
 AddHeaderEx(AResponse,GetTitle,'',Self,4); 

 {Add GPIO Pin Counts}
 AddItem4Column(AResponse,'GPIO Pin Count:',IntToStr(GPIO_PIN_COUNT),'','');
 AddItem4Column(AResponse,'Virtual GPIO Pin Count:',IntToStr(VIRTUAL_GPIO_PIN_COUNT),'','');
 AddBlankEx(AResponse,4);

 {Check Default Device} 
 GPIODevice:=GPIODeviceGetDefault;
 if GPIODevice <> nil then
  begin
   AddBold4Column(AResponse,GPIODevice.Device.DeviceName + ' (Default)','','','');
   AddBlankEx(AResponse,4);
   AddBold4Column(AResponse,'Pin','Function','Pull','Value');
   AddBlankEx(AResponse,4);

   {Get Default Device Properties} 
   if GPIODeviceGetProperties(GPIODevice,@GPIOProperties) = ERROR_SUCCESS then
    begin
     for Count:=GPIOProperties.PinMin to GPIOProperties.PinMax do
      begin
       AddItem4Column(AResponse,GPIOPinToString(Count),GPIOFunctionToString(GPIODeviceFunctionGet(GPIODevice,Count)),GPIOPullToString(GPIODevicePullGet(GPIODevice,Count)),GPIOLevelToString(GPIODeviceInputGet(GPIODevice,Count)));
      end;
    end;
   
   AddBlankEx(AResponse,4); 
  end;
  
 {Check Device Count}
 if GPIOGetCount > 1 then
  begin
   for GPIOCount:=1 to GPIOGetCount - 1 do
    begin
     {Get GPIO Device}
     GPIODevice:=GPIODeviceFind(GPIOCount);
     if GPIODevice <> nil then
      begin
       AddBold4Column(AResponse,GPIODevice.Device.DeviceName,'','','');
       AddBlankEx(AResponse,4);
       AddBold4Column(AResponse,'Pin','Function','Pull','Value');
       AddBlankEx(AResponse,4);
       
       {Get GPIO Device Properties} 
       if GPIODeviceGetProperties(GPIODevice,@GPIOProperties) = ERROR_SUCCESS then
        begin
         for Count:=GPIOProperties.PinMin to GPIOProperties.PinMax do
          begin
           AddItem4Column(AResponse,GPIOPinToString(Count),GPIOFunctionToString(GPIODeviceFunctionGet(GPIODevice,Count)),GPIOPullToString(GPIODevicePullGet(GPIODevice,Count)),GPIOLevelToString(GPIODeviceInputGet(GPIODevice,Count)));
          end;
        end; 
       
       AddBlankEx(AResponse,4);  
      end;
    end;  
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
 AddItemEx(AResponse,'_text_start:','0x' + PtrToHex(@_text_start),2);
 AddItemEx(AResponse,'_etext:','0x' + PtrToHex(@_etext),2);
 AddItemEx(AResponse,'_data:','0x' + PtrToHex(@_data),2);
 AddItemEx(AResponse,'_edata:','0x' + PtrToHex(@_edata),2);
 AddItemEx(AResponse,'_bss_start:','0x' + PtrToHex(@_bss_start),2);
 AddItemEx(AResponse,'_bss_end:','0x' + PtrToHex(@_bss_end),2);
 AddBlank(AResponse);

 {Add RTL Initial Heap Allocation} 
 AddBold(AResponse,'RTL Initial Heap Allocation','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'RtlHeapAddr:','0x' + PtrToHex(@RtlHeapAddr),2);
 AddItemEx(AResponse,'RtlHeapSize:','0x' + IntToHex(RtlHeapSize,8),2);
 AddBlank(AResponse);
 
 {Add RTL Error Handling}
 AddBold(AResponse,'RTL Error Handling','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'RtlErrorBase:','0x' + PtrToHex(RtlErrorBase),2);
 AddBlank(AResponse);
 
 {Add RTL Initialization}
 AddBold(AResponse,'RTL Initialization','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'RtlInitFinalTable:','0x' + PtrToHex(@RtlInitFinalTable),2);
 AddBlank(AResponse);
 
 {Add Heap, Stack and Alignment}
 AddBold(AResponse,'Heap, Stack and Alignment','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'HEAP_MIN_ALIGNMENT:',IntToStr(HEAP_MIN_ALIGNMENT),2);
 AddItemEx(AResponse,'HEAP_REQUEST_ALIGNMENT:',IntToStr(HEAP_REQUEST_ALIGNMENT),2);
 AddBlank(AResponse);
 AddItemEx(AResponse,'STACK_MIN_ALIGNMENT:',IntToStr(STACK_MIN_ALIGNMENT),2);
 AddItemEx(AResponse,'THREADVAR_MIN_ALIGNMENT:',IntToStr(THREADVAR_MIN_ALIGNMENT),2);
 AddBlank(AResponse);
 AddItemEx(AResponse,'INITIAL_HEAP_SIZE:',IntToStr(INITIAL_HEAP_SIZE),2);
 AddItemEx(AResponse,'INITIAL_HEAP_BASE:','0x' + AddrToHex(INITIAL_HEAP_BASE),2);
 AddBlank(AResponse);
 AddItemEx(AResponse,'INITIAL_TLS_SIZE:',IntToStr(INITIAL_TLS_SIZE),2);
 AddItemEx(AResponse,'INITIAL_STACK_SIZE:',IntToStr(INITIAL_STACK_SIZE),2);
 AddItemEx(AResponse,'INITIAL_STACK_BASE:','0x' + AddrToHex(INITIAL_STACK_BASE),2);
 AddBlank(AResponse);
 
 {Add Interrupt and Exception Handling}
 AddBold(AResponse,'Interrupt and Exception Handling','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'IRQ_ENABLED:',BooleanToString(IRQ_ENABLED),2);
 AddItemEx(AResponse,'FIQ_ENABLED:',BooleanToString(FIQ_ENABLED),2);
 AddItemEx(AResponse,'IPI_ENABLED:',BooleanToString(IPI_ENABLED),2);
 AddItemEx(AResponse,'SWI_ENABLED:',BooleanToString(SWI_ENABLED),2);
 AddItemEx(AResponse,'ABORT_ENABLED:',BooleanToString(ABORT_ENABLED),2);
 AddItemEx(AResponse,'UNDEFINED_ENABLED:',BooleanToString(UNDEFINED_ENABLED),2);
 AddBlank(AResponse);
 AddItemEx(AResponse,'IRQ_STACK_ENABLED:',BooleanToString(IRQ_STACK_ENABLED),2);
 AddItemEx(AResponse,'FIQ_STACK_ENABLED:',BooleanToString(FIQ_STACK_ENABLED),2);
 AddItemEx(AResponse,'SWI_STACK_ENABLED:',BooleanToString(SWI_STACK_ENABLED),2);
 AddItemEx(AResponse,'ABORT_STACK_ENABLED:',BooleanToString(ABORT_STACK_ENABLED),2);
 AddItemEx(AResponse,'UNDEFINED_STACK_ENABLED:',BooleanToString(UNDEFINED_STACK_ENABLED),2);
 AddBlank(AResponse);
 
 {Add Memory and Peripheral Mapping}
 AddBold(AResponse,'Memory and Peripheral Mapping','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'IO_BASE:','0x' + AddrToHex(IO_BASE),2);
 AddItemEx(AResponse,'IO_ALIAS:','0x' + AddrToHex(IO_ALIAS),2);
 AddItemEx(AResponse,'BUS_ALIAS:','0x' + AddrToHex(BUS_ALIAS),2);
 AddBlank(AResponse);

 {Add Secure Boot}
 AddBold(AResponse,'Secure Boot','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'SECURE_BOOT:',BooleanToString(SECURE_BOOT),2);
 AddBlank(AResponse);

 {Add Startup Handler Address}
 AddBold(AResponse,'Startup Handler Address','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'STARTUP_ADDRESS:','0x' + AddrToHex(STARTUP_ADDRESS),2);
 AddBlank(AResponse);

 {Add Memory Base Mapping}
 AddBold(AResponse,'Memory Base Mapping','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'MEMORY_BASE:','0x' + AddrToHex(MEMORY_BASE),2);
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

 {Add DMA configuration}
 AddBold(AResponse,'DMA configuration','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'DMA_ALIGNMENT:',IntToStr(DMA_ALIGNMENT),2);
 AddItemEx(AResponse,'DMA_MULTIPLIER:',IntToStr(DMA_MULTIPLIER),2);
 AddItemEx(AResponse,'DMA_SHARED_MEMORY:',BooleanToString(DMA_SHARED_MEMORY),2);
 AddItemEx(AResponse,'DMA_NOCACHE_MEMORY:',BooleanToString(DMA_NOCACHE_MEMORY),2);
 AddItemEx(AResponse,'DMA_BUS_ADDRESSES:',BooleanToString(DMA_BUS_ADDRESSES),2);
 AddItemEx(AResponse,'DMA_CACHE_COHERENT:',BooleanToString(DMA_CACHE_COHERENT),2);
 AddBlank(AResponse);
 
 {Add GPIO configuration}
 AddBold(AResponse,'GPIO configuration','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'GPIO_PIN_COUNT:',IntToStr(GPIO_PIN_COUNT),2);
 AddBlank(AResponse);
 
 {Add Virtual GPIO configuration}
 AddBold(AResponse,'Virtual GPIO configuration','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'VIRTUAL_GPIO_PIN_COUNT:',IntToStr(VIRTUAL_GPIO_PIN_COUNT),2);
 AddBlank(AResponse);

 {Add Disk, Controller, Filesystem and Cache configuration}
 AddBold(AResponse,'Filesystem configuration','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'FILESYS_AUTOSTART:',BooleanToString(FILESYS_AUTOSTART),2);
 AddItemEx(AResponse,'FILESYS_ASYNCSTART:',BooleanToString(FILESYS_ASYNCSTART),2);
 AddItemEx(AResponse,'FILESYS_STARTDELAY:',IntToStr(FILESYS_STARTDELAY),2);
 AddBlank(AResponse);
 AddItemEx(AResponse,'FILESYS_REGISTER_LOGGING:',BooleanToString(FILESYS_REGISTER_LOGGING),2);
 AddItemEx(AResponse,'FILESYS_LOGGING_DEFAULT:',BooleanToString(FILESYS_LOGGING_DEFAULT),2);
 AddItemEx(AResponse,'FILESYS_LOGGING_FILE:',FILESYS_LOGGING_FILE,2);
 AddItemEx(AResponse,'FILESYS_LOGGING_MAXSIZE:',IntToStr(FILESYS_LOGGING_MAXSIZE),2);
 AddItemEx(AResponse,'FILESYS_LOGGING_MAXCOPIES:',IntToStr(FILESYS_LOGGING_MAXCOPIES),2);
 AddItemEx(AResponse,'FILESYS_LOGGING_RESET:',BooleanToString(FILESYS_LOGGING_RESET),2);
 AddBlank(AResponse);
 AddItemEx(AResponse,'FILESYS_FLOPPY_ENABLED:',BooleanToString(FILESYS_FLOPPY_ENABLED),2);
 AddItemEx(AResponse,'FILESYS_DRIVES_ENABLED:',BooleanToString(FILESYS_DRIVES_ENABLED),2);
 AddBlank(AResponse);
 AddItemEx(AResponse,'FILESYS_ATA_ENABLED:',BooleanToString(FILESYS_ATA_ENABLED),2);
 AddItemEx(AResponse,'FILESYS_ATAPI_ENABLED:',BooleanToString(FILESYS_ATAPI_ENABLED),2);
 AddItemEx(AResponse,'FILESYS_SCSI_ENABLED:',BooleanToString(FILESYS_SCSI_ENABLED),2);
 AddItemEx(AResponse,'FILESYS_USB_ENABLED:',BooleanToString(FILESYS_USB_ENABLED),2);
 AddItemEx(AResponse,'FILESYS_MMC_ENABLED:',BooleanToString(FILESYS_MMC_ENABLED),2);
 AddItemEx(AResponse,'FILESYS_VIRTUAL_ENABLED:',BooleanToString(FILESYS_VIRTUAL_ENABLED),2);
 AddBlank(AResponse);
 AddItemEx(AResponse,'FILESYS_FAT_ENABLED:',BooleanToString(FILESYS_FAT_ENABLED),2);
 AddItemEx(AResponse,'FILESYS_NTFS_ENABLED:',BooleanToString(FILESYS_NTFS_ENABLED),2);
 AddItemEx(AResponse,'FILESYS_EXTFS_ENABLED:',BooleanToString(FILESYS_EXTFS_ENABLED),2);
 AddItemEx(AResponse,'FILESYS_CDFS_ENABLED:',BooleanToString(FILESYS_CDFS_ENABLED),2);
 AddBlank(AResponse);
 AddItemEx(AResponse,'FILESYS_CASE_FLAGS:',BooleanToString(FILESYS_CASE_FLAGS),2);
 AddItemEx(AResponse,'FILESYS_LONG_NAMES:',BooleanToString(FILESYS_LONG_NAMES),2);
 AddItemEx(AResponse,'FILESYS_OEM_CONVERT:',BooleanToString(FILESYS_OEM_CONVERT),2);
 AddItemEx(AResponse,'FILESYS_NUMERIC_TAIL:',BooleanToString(FILESYS_NUMERIC_TAIL),2);
 AddItemEx(AResponse,'FILESYS_DIRTY_CHECK:',BooleanToString(FILESYS_DIRTY_CHECK),2);
 AddItemEx(AResponse,'FILESYS_QUICK_CHECK:',BooleanToString(FILESYS_QUICK_CHECK),2);
 AddItemEx(AResponse,'FILESYS_UPDATE_ACCESSTIME:',BooleanToString(FILESYS_UPDATE_ACCESSTIME),2);
 AddBlank(AResponse);
 AddItemEx(AResponse,'FILESYS_GLOBAL_CURRENTDIR:',BooleanToString(FILESYS_GLOBAL_CURRENTDIR),2);
 AddBlank(AResponse);
 AddBold(AResponse,'Cache configuration','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'FILESYS_CACHE_SIZE:',IntToStr(FILESYS_CACHE_SIZE),2);
 AddItemEx(AResponse,'FILESYS_CACHE_PAGE:',IntToStr(FILESYS_CACHE_PAGE),2);
 AddItemEx(AResponse,'FILESYS_CACHE_KEYS:',IntToStr(FILESYS_CACHE_KEYS),2);
 case FILESYS_CACHE_MODE of
  FILESYS_CACHE_MODE_NONE:AddItemEx(AResponse,'FILESYS_CACHE_MODE:','FILESYS_CACHE_MODE_NONE',2);
  FILESYS_CACHE_MODE_READONLY:AddItemEx(AResponse,'FILESYS_CACHE_MODE:','FILESYS_CACHE_MODE_READONLY',2);
  FILESYS_CACHE_MODE_READWRITE:AddItemEx(AResponse,'FILESYS_CACHE_MODE:','FILESYS_CACHE_MODE_READWRITE',2);
 end;
 AddBlank(AResponse);
 AddBold(AResponse,'FAT configuration','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'FAT_DEFAULT:',BooleanToString(FAT_DEFAULT),2);
 AddItemEx(AResponse,'FAT_CASE_FLAGS:',BooleanToString(FAT_CASE_FLAGS),2);
 AddItemEx(AResponse,'FAT_LONG_NAMES:',BooleanToString(FAT_LONG_NAMES),2);
 AddItemEx(AResponse,'FAT_OEM_CONVERT:',BooleanToString(FAT_OEM_CONVERT),2);
 AddItemEx(AResponse,'FAT_NUMERIC_TAIL:',BooleanToString(FAT_NUMERIC_TAIL),2);
 AddItemEx(AResponse,'FAT_DIRTY_CHECK:',BooleanToString(FAT_DIRTY_CHECK),2);
 AddItemEx(AResponse,'FAT_QUICK_CHECK:',BooleanToString(FAT_QUICK_CHECK),2);
 AddItemEx(AResponse,'FAT_INFO_SECTOR_ENABLE:',BooleanToString(FAT_INFO_SECTOR_ENABLE),2);
 AddItemEx(AResponse,'FAT_INFO_IMMEDIATE_UPDATE:',BooleanToString(FAT_INFO_IMMEDIATE_UPDATE),2);
 AddBlank(AResponse);
 AddBold(AResponse,'NTFS configuration','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'NTFS_DEFAULT:',BooleanToString(NTFS_DEFAULT),2);
 AddItemEx(AResponse,'NTFS_RESET_LOG:',BooleanToString(NTFS_RESET_LOG),2);
 AddItemEx(AResponse,'NTFS_FIXED_ZONE:',BooleanToString(NTFS_FIXED_ZONE),2);
 AddItemEx(AResponse,'NTFS_ALT_LAYOUT:',BooleanToString(NTFS_ALT_LAYOUT),2);
 AddItemEx(AResponse,'NTFS_LENIENT:',BooleanToString(NTFS_LENIENT),2);
 AddItemEx(AResponse,'NTFS_DEFENSIVE:',BooleanToString(NTFS_DEFENSIVE),2);
 AddItemEx(AResponse,'NTFS_AGGRESSIVE:',BooleanToString(NTFS_AGGRESSIVE),2);
 AddItemEx(AResponse,'NTFS_NO_SHORT_NAMES:',BooleanToString(NTFS_NO_SHORT_NAMES),2);
 AddItemEx(AResponse,'NTFS_NULL_SECURITY:',BooleanToString(NTFS_NULL_SECURITY),2);
 AddItemEx(AResponse,'NTFS_DEFAULT_SECURITY:',BooleanToString(NTFS_DEFAULT_SECURITY),2);
 AddBlank(AResponse);
 AddBold(AResponse,'EXTFS configuration','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'EXTFS_DEFAULT:',BooleanToString(EXTFS_DEFAULT),2);
 AddBlank(AResponse);
 AddBold(AResponse,'CDFS configuration','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'CDFS_DEFAULT:',BooleanToString(CDFS_DEFAULT),2);
 AddItemEx(AResponse,'CDFS_LONG_NAMES:',BooleanToString(CDFS_LONG_NAMES),2);
 AddItemEx(AResponse,'CDFS_SWAP_SERIAL:',BooleanToString(CDFS_SWAP_SERIAL),2);
 AddBlank(AResponse);
 
 {Add Network, Transport, Protocol and Sockets configuration}
 AddBold(AResponse,'Host configuration','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'HOST_NAME:',HOST_NAME,2);
 AddItemEx(AResponse,'HOST_DOMAIN:',HOST_DOMAIN,2);
 AddBlank(AResponse);
 AddBold(AResponse,'Winsock configuration','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'WINSOCK_NAME:',WINSOCK_NAME,2);
 AddItemEx(AResponse,'WINSOCK_LOW_VERSION:','0x' + IntToHex(WINSOCK_LOW_VERSION,4),2);
 AddItemEx(AResponse,'WINSOCK_HIGH_VERSION:','0x' + IntToHex(WINSOCK_HIGH_VERSION,4),2);
 AddItemEx(AResponse,'WINSOCK_BUILD_VERSION:',WINSOCK_BUILD_VERSION,2);
 AddBlank(AResponse);
 AddItemEx(AResponse,'WINSOCK_MAX_SOCKETS:',IntToStr(WINSOCK_MAX_SOCKETS),2);
 AddItemEx(AResponse,'WINSOCK_MAX_UDP:',IntToStr(WINSOCK_MAX_UDP),2);
 AddBlank(AResponse);
 AddItemEx(AResponse,'WINSOCK_AUTOSTART:',BooleanToString(WINSOCK_AUTOSTART),2);
 AddItemEx(AResponse,'WINSOCK_ASYNCSTART:',BooleanToString(WINSOCK_ASYNCSTART),2);
 AddItemEx(AResponse,'WINSOCK_STARTDELAY:',IntToStr(WINSOCK_STARTDELAY),2);
 AddBlank(AResponse);
 AddBold(AResponse,'Winsock2 configuration','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'WINSOCK2_NAME:',WINSOCK2_NAME,2);
 AddItemEx(AResponse,'WINSOCK2_LOW_VERSION:','0x' + IntToHex(WINSOCK2_LOW_VERSION,4),2);
 AddItemEx(AResponse,'WINSOCK2_HIGH_VERSION:','0x' + IntToHex(WINSOCK2_HIGH_VERSION,4),2);
 AddItemEx(AResponse,'WINSOCK2_BUILD_VERSION:',WINSOCK2_BUILD_VERSION,2);
 AddBlank(AResponse);
 AddItemEx(AResponse,'WINSOCK2_MAX_SOCKETS:',IntToStr(WINSOCK2_MAX_SOCKETS),2);
 AddItemEx(AResponse,'WINSOCK2_MAX_UDP:',IntToStr(WINSOCK2_MAX_UDP),2);
 AddBlank(AResponse);
 AddItemEx(AResponse,'WINSOCK2_AUTOSTART:',BooleanToString(WINSOCK2_AUTOSTART),2);
 AddItemEx(AResponse,'WINSOCK2_ASYNCSTART:',BooleanToString(WINSOCK2_ASYNCSTART),2);
 AddItemEx(AResponse,'WINSOCK2_STARTDELAY:',IntToStr(WINSOCK2_STARTDELAY),2);
 AddBlank(AResponse);
 AddBold(AResponse,'Sockets configuration','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'SOCKETS_AUTOSTART:',BooleanToString(SOCKETS_AUTOSTART),2);
 AddItemEx(AResponse,'SOCKETS_ASYNCSTART:',BooleanToString(SOCKETS_ASYNCSTART),2);
 AddItemEx(AResponse,'SOCKETS_STARTDELAY:',IntToStr(SOCKETS_STARTDELAY),2);
 AddBlank(AResponse);
 AddBold(AResponse,'Client configuration','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'DNS_CLIENT_ENABLED:',BooleanToString(DNS_CLIENT_ENABLED),2);
 AddBlank(AResponse);
 AddBold(AResponse,'Protocol configuration','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'RAW_PROTOCOL_ENABLED:',BooleanToString(RAW_PROTOCOL_ENABLED),2);
 AddItemEx(AResponse,'UDP_PROTOCOL_ENABLED:',BooleanToString(UDP_PROTOCOL_ENABLED),2);
 AddItemEx(AResponse,'TCP_PROTOCOL_ENABLED:',BooleanToString(TCP_PROTOCOL_ENABLED),2);
 AddItemEx(AResponse,'ICMP_PROTOCOL_ENABLED:',BooleanToString(ICMP_PROTOCOL_ENABLED),2);
 AddItemEx(AResponse,'ICMP6_PROTOCOL_ENABLED:',BooleanToString(ICMP6_PROTOCOL_ENABLED),2);
 AddItemEx(AResponse,'IGMP_PROTOCOL_ENABLED:',BooleanToString(IGMP_PROTOCOL_ENABLED),2);
 AddBlank(AResponse);
 AddItemEx(AResponse,'TCP_MIN_BACKLOG:',IntToStr(TCP_MIN_BACKLOG),2);
 AddItemEx(AResponse,'TCP_MAX_BACKLOG:',IntToStr(TCP_MAX_BACKLOG),2);
 AddItemEx(AResponse,'TCP_RECEIVE_BACKLOG:',IntToStr(TCP_RECEIVE_BACKLOG),2);
 AddBlank(AResponse);
 AddItemEx(AResponse,'ARP_CONFIG_ENABLED:',BooleanToString(ARP_CONFIG_ENABLED),2);
 AddItemEx(AResponse,'RARP_CONFIG_ENABLED:',BooleanToString(RARP_CONFIG_ENABLED),2);
 AddItemEx(AResponse,'BOOTP_CONFIG_ENABLED:',BooleanToString(BOOTP_CONFIG_ENABLED),2);
 AddItemEx(AResponse,'DHCP_CONFIG_ENABLED:',BooleanToString(DHCP_CONFIG_ENABLED),2);
 AddItemEx(AResponse,'STATIC_CONFIG_ENABLED:',BooleanToString(STATIC_CONFIG_ENABLED),2);
 AddItemEx(AResponse,'LOOPBACK_CONFIG_ENABLED:',BooleanToString(LOOPBACK_CONFIG_ENABLED),2);
 AddBlank(AResponse);
 AddBold(AResponse,'Transport configuration','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'IP_TRANSPORT_ENABLED:',BooleanToString(IP_TRANSPORT_ENABLED),2);
 AddItemEx(AResponse,'IP6_TRANSPORT_ENABLED:',BooleanToString(IP6_TRANSPORT_ENABLED),2);
 AddItemEx(AResponse,'ARP_TRANSPORT_ENABLED:',BooleanToString(ARP_TRANSPORT_ENABLED),2);
 AddItemEx(AResponse,'RARP_TRANSPORT_ENABLED:',BooleanToString(RARP_TRANSPORT_ENABLED),2);
 AddBlank(AResponse);
 AddItemEx(AResponse,'RSN_TRANSPORT_ENABLED:',BooleanToString(RSN_TRANSPORT_ENABLED),2);
 AddItemEx(AResponse,'EAPOL_TRANSPORT_ENABLED:',BooleanToString(EAPOL_TRANSPORT_ENABLED),2);
 AddBlank(AResponse);
 AddBold(AResponse,'Network configuration','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'WIRED_NETWORK_ENABLED:',BooleanToString(WIRED_NETWORK_ENABLED),2);
 AddItemEx(AResponse,'LOOPBACK_NETWORK_ENABLED:',BooleanToString(LOOPBACK_NETWORK_ENABLED),2);
 AddItemEx(AResponse,'WIRELESS_NETWORK_ENABLED:',BooleanToString(WIRELESS_NETWORK_ENABLED),2);
 AddBlank(AResponse);
 
 {Add Keyboard configuration}
 AddBold(AResponse,'Keyboard configuration','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'KEYBOARD_NUM_LOCK_DEFAULT:',BooleanToString(KEYBOARD_NUM_LOCK_DEFAULT),2);
 AddItemEx(AResponse,'KEYBOARD_CAPS_LOCK_DEFAULT:',BooleanToString(KEYBOARD_CAPS_LOCK_DEFAULT),2);
 AddItemEx(AResponse,'KEYBOARD_SCROLL_LOCK_DEFAULT:',BooleanToString(KEYBOARD_SCROLL_LOCK_DEFAULT),2);
 AddBlank(AResponse);
 AddItemEx(AResponse,'KEYBOARD_CTRL_ALT_IS_ALTGR:',BooleanToString(KEYBOARD_CTRL_ALT_IS_ALTGR),2);
 AddItemEx(AResponse,'KEYBOARD_SHIFT_IS_CAPS_LOCK_OFF:',BooleanToString(KEYBOARD_SHIFT_IS_CAPS_LOCK_OFF),2);
 AddBlank(AResponse);
 
 {Add Mouse configuration}
 AddBold(AResponse,'Mouse configuration','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'MOUSE_SWAP_BUTTONS_DEFAULT:',BooleanToString(MOUSE_SWAP_BUTTONS_DEFAULT),2);
 AddBlank(AResponse);

 {Add Touch configuration}
 AddBold(AResponse,'Touch configuration','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'TOUCH_MOUSE_DATA_DEFAULT:',BooleanToString(TOUCH_MOUSE_DATA_DEFAULT),2);
 AddBlank(AResponse);
 
 {Add PCI configuration}
 AddBold(AResponse,'PCI configuration','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'PCI_AUTOSTART:',BooleanToString(PCI_AUTOSTART),2);
 AddItemEx(AResponse,'PCI_ASYNCSTART:',BooleanToString(PCI_ASYNCSTART),2);
 AddItemEx(AResponse,'PCI_STARTDELAY:',IntToStr(PCI_STARTDELAY),2);
 AddBlank(AResponse);
 
 {Add USB configuration}
 AddBold(AResponse,'USB configuration','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'USB_AUTOSTART:',BooleanToString(USB_AUTOSTART),2);
 AddItemEx(AResponse,'USB_ASYNCSTART:',BooleanToString(USB_ASYNCSTART),2);
 AddItemEx(AResponse,'USB_STARTDELAY:',IntToStr(USB_STARTDELAY),2);
 AddBlank(AResponse);
 AddItemEx(AResponse,'USB_DMA_ALIGNMENT:',IntToStr(USB_DMA_ALIGNMENT),2);
 AddItemEx(AResponse,'USB_DMA_MULTIPLIER:',IntToStr(USB_DMA_MULTIPLIER),2);
 AddItemEx(AResponse,'USB_DMA_SHARED_MEMORY:',BooleanToString(USB_DMA_SHARED_MEMORY),2);
 AddItemEx(AResponse,'USB_DMA_NOCACHE_MEMORY:',BooleanToString(USB_DMA_NOCACHE_MEMORY),2);
 AddItemEx(AResponse,'USB_DMA_BUS_ADDRESSES:',BooleanToString(USB_DMA_BUS_ADDRESSES),2);
 AddItemEx(AResponse,'USB_DMA_CACHE_COHERENT:',BooleanToString(USB_DMA_CACHE_COHERENT),2);
 AddBlank(AResponse);
 AddItemEx(AResponse,'USB_HUB_MESSAGESLOT_MAXIMUM:',IntToStr(USB_HUB_MESSAGESLOT_MAXIMUM),2);
 AddItemEx(AResponse,'USB_HUB_REGISTER_DRIVER:',BooleanToString(USB_HUB_REGISTER_DRIVER),2);
 AddBlank(AResponse);
 AddItemEx(AResponse,'USB_KEYBOARD_POLLING_INTERVAL:',IntToStr(USB_KEYBOARD_POLLING_INTERVAL),2);
 AddItemEx(AResponse,'USB_KEYBOARD_REGISTER_DRIVER:',BooleanToString(USB_KEYBOARD_REGISTER_DRIVER),2);
 AddBlank(AResponse);
 AddItemEx(AResponse,'USB_MOUSE_POLLING_INTERVAL:',IntToStr(USB_MOUSE_POLLING_INTERVAL),2);
 AddItemEx(AResponse,'USB_MOUSE_REGISTER_DRIVER:',BooleanToString(USB_MOUSE_REGISTER_DRIVER),2);
 AddBlank(AResponse);
 AddItemEx(AResponse,'USB_STORAGE_FORCE_REMOVABLE:',BooleanToString(USB_STORAGE_FORCE_REMOVABLE),2);
 AddItemEx(AResponse,'USB_STORAGE_REGISTER_DRIVER:',BooleanToString(USB_STORAGE_REGISTER_DRIVER),2);
 AddBlank(AResponse);

 {Add MMC configuration}
 AddBold(AResponse,'MMC configuration','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'MMC_AUTOSTART:',BooleanToString(MMC_AUTOSTART),2);
 AddItemEx(AResponse,'MMC_ASYNCSTART:',BooleanToString(MMC_ASYNCSTART),2);
 AddItemEx(AResponse,'MMC_STARTDELAY:',IntToStr(MMC_STARTDELAY),2);
 AddBlank(AResponse);
 AddItemEx(AResponse,'MMC_DMA_ALIGNMENT:',IntToStr(MMC_DMA_ALIGNMENT),2);
 AddItemEx(AResponse,'MMC_DMA_MULTIPLIER:',IntToStr(MMC_DMA_MULTIPLIER),2);
 AddItemEx(AResponse,'MMC_DMA_SHARED_MEMORY:',BooleanToString(MMC_DMA_SHARED_MEMORY),2);
 AddItemEx(AResponse,'MMC_DMA_NOCACHE_MEMORY:',BooleanToString(MMC_DMA_NOCACHE_MEMORY),2);
 AddItemEx(AResponse,'MMC_DMA_BUS_ADDRESSES:',BooleanToString(MMC_DMA_BUS_ADDRESSES),2);
 AddItemEx(AResponse,'MMC_DMA_CACHE_COHERENT:',BooleanToString(MMC_DMA_CACHE_COHERENT),2);
 AddBlank(AResponse);
 
 {Add DWCOTG configuration}
 AddBold(AResponse,'DWCOTG configuration','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'DWCOTG_REGISTER_HOST:',BooleanToString(DWCOTG_REGISTER_HOST),2);
 AddBlank(AResponse);
 AddItemEx(AResponse,'DWCOTG_IRQ:',IntToStr(DWCOTG_IRQ),2);
 AddItemEx(AResponse,'DWCOTG_POWER_ID:',IntToStr(DWCOTG_POWER_ID),2);
 AddItemEx(AResponse,'DWCOTG_REGS_BASE:','0x' + AddrToHex(DWCOTG_REGS_BASE),2);
 AddItemEx(AResponse,'DWCOTG_FIQ_ENABLED:',BooleanToString(DWCOTG_FIQ_ENABLED),2);
 AddItemEx(AResponse,'DWCOTG_DMA_ALIGNMENT:',IntToStr(DWCOTG_DMA_ALIGNMENT),2);
 AddItemEx(AResponse,'DWCOTG_DMA_MULTIPLIER:',IntToStr(DWCOTG_DMA_MULTIPLIER),2);
 AddItemEx(AResponse,'DWCOTG_DMA_SHARED_MEMORY:',BooleanToString(DWCOTG_DMA_SHARED_MEMORY),2);
 AddItemEx(AResponse,'DWCOTG_DMA_NOCACHE_MEMORY:',BooleanToString(DWCOTG_DMA_NOCACHE_MEMORY),2);
 AddItemEx(AResponse,'DWCOTG_DMA_BUS_ADDRESSES:',BooleanToString(DWCOTG_DMA_BUS_ADDRESSES),2);
 AddItemEx(AResponse,'DWCOTG_DMA_CACHE_COHERENT:',BooleanToString(DWCOTG_DMA_CACHE_COHERENT),2);
 AddItemEx(AResponse,'DWCOTG_HOST_FRAME_INTERVAL:',BooleanToString(DWCOTG_HOST_FRAME_INTERVAL),2);
 AddBlank(AResponse);

 {Add BCMSDHOST configuration}
 AddBold(AResponse,'BCMSDHOST configuration','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'BCMSDHOST_DELAY_AFTER_STOP:',IntToStr(BCMSDHOST_DELAY_AFTER_STOP),2);
 AddItemEx(AResponse,'BCMSDHOST_OVERCLOCK_50:',IntToStr(BCMSDHOST_OVERCLOCK_50),2);
 AddItemEx(AResponse,'BCMSDHOST_PIO_LIMIT:',IntToStr(BCMSDHOST_PIO_LIMIT),2);
 AddItemEx(AResponse,'BCMSDHOST_FORCE_PIO:',BooleanToString(BCMSDHOST_FORCE_PIO),2);
 AddBlank(AResponse);

 {Add LAN78XX configuration}
 AddBold(AResponse,'LAN78XX configuration','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'LAN78XX_MAC_ADDRESS:',LAN78XX_MAC_ADDRESS,2);
 AddBlank(AResponse);

 {Add SMSC95XX configuration}
 AddBold(AResponse,'SMSC95XX configuration','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'SMSC95XX_MAC_ADDRESS:',SMSC95XX_MAC_ADDRESS,2);
 AddBlank(AResponse);

 {Add GENET configuration}
 AddBold(AResponse,'GENET configuration','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'GENET_MAC_ADDRESS:',GENET_MAC_ADDRESS,2);
 AddBlank(AResponse);

 case MachineGetType of
  MACHINE_TYPE_BCM2708:begin
    {Add BCM2708 configuration}
    AddBold(AResponse,'BCM2708 configuration','');
    AddBlank(AResponse);
    AddItemEx(AResponse,'BCM2708DMA_ALIGNMENT:',IntToStr(BCM2708DMA_ALIGNMENT),2);
    AddItemEx(AResponse,'BCM2708DMA_MULTIPLIER:',IntToStr(BCM2708DMA_MULTIPLIER),2);
    AddItemEx(AResponse,'BCM2708DMA_SHARED_MEMORY:',BooleanToString(BCM2708DMA_SHARED_MEMORY),2);
    AddItemEx(AResponse,'BCM2708DMA_NOCACHE_MEMORY:',BooleanToString(BCM2708DMA_NOCACHE_MEMORY),2);
    AddItemEx(AResponse,'BCM2708DMA_BUS_ADDRESSES:',BooleanToString(BCM2708DMA_BUS_ADDRESSES),2);
    AddItemEx(AResponse,'BCM2708DMA_CACHE_COHERENT:',BooleanToString(BCM2708DMA_CACHE_COHERENT),2);
    AddBlank(AResponse);
    //To Do
   end; 
  MACHINE_TYPE_BCM2709:begin
    {Add BCM2709 configuration}
    AddBold(AResponse,'BCM2709 configuration','');
    AddBlank(AResponse);
    AddItemEx(AResponse,'BCM2709DMA_ALIGNMENT:',IntToStr(BCM2709DMA_ALIGNMENT),2);
    AddItemEx(AResponse,'BCM2709DMA_MULTIPLIER:',IntToStr(BCM2709DMA_MULTIPLIER),2);
    AddItemEx(AResponse,'BCM2709DMA_SHARED_MEMORY:',BooleanToString(BCM2709DMA_SHARED_MEMORY),2);
    AddItemEx(AResponse,'BCM2709DMA_NOCACHE_MEMORY:',BooleanToString(BCM2709DMA_NOCACHE_MEMORY),2);
    AddItemEx(AResponse,'BCM2709DMA_BUS_ADDRESSES:',BooleanToString(BCM2709DMA_BUS_ADDRESSES),2);
    AddItemEx(AResponse,'BCM2709DMA_CACHE_COHERENT:',BooleanToString(BCM2709DMA_CACHE_COHERENT),2);
    AddBlank(AResponse);
    //To Do
   end;
  MACHINE_TYPE_BCM2710:begin
    //To Do //What if this is a Pi3 running Pi2 code ?
    {Add BCM2710 configuration}
    AddBold(AResponse,'BCM2710 configuration','');
    AddBlank(AResponse);
    AddItemEx(AResponse,'BCM2710DMA_ALIGNMENT:',IntToStr(BCM2710DMA_ALIGNMENT),2);
    AddItemEx(AResponse,'BCM2710DMA_MULTIPLIER:',IntToStr(BCM2710DMA_MULTIPLIER),2);
    AddItemEx(AResponse,'BCM2710DMA_SHARED_MEMORY:',BooleanToString(BCM2710DMA_SHARED_MEMORY),2);
    AddItemEx(AResponse,'BCM2710DMA_NOCACHE_MEMORY:',BooleanToString(BCM2710DMA_NOCACHE_MEMORY),2);
    AddItemEx(AResponse,'BCM2710DMA_BUS_ADDRESSES:',BooleanToString(BCM2710DMA_BUS_ADDRESSES),2);
    AddItemEx(AResponse,'BCM2710DMA_CACHE_COHERENT:',BooleanToString(BCM2710DMA_CACHE_COHERENT),2);
    AddBlank(AResponse);
    //To Do
   end; 
  MACHINE_TYPE_BCM2711:begin
    {Add BCM2711 configuration}
    AddBold(AResponse,'BCM2711 configuration','');
    AddBlank(AResponse);
    AddItemEx(AResponse,'BCM2711DMA_ALIGNMENT:',IntToStr(BCM2711DMA_ALIGNMENT),2);
    AddItemEx(AResponse,'BCM2711DMA_MULTIPLIER:',IntToStr(BCM2711DMA_MULTIPLIER),2);
    AddItemEx(AResponse,'BCM2711DMA_SHARED_MEMORY:',BooleanToString(BCM2711DMA_SHARED_MEMORY),2);
    AddItemEx(AResponse,'BCM2711DMA_NOCACHE_MEMORY:',BooleanToString(BCM2711DMA_NOCACHE_MEMORY),2);
    AddItemEx(AResponse,'BCM2711DMA_BUS_ADDRESSES:',BooleanToString(BCM2711DMA_BUS_ADDRESSES),2);
    AddItemEx(AResponse,'BCM2711DMA_CACHE_COHERENT:',BooleanToString(BCM2711DMA_CACHE_COHERENT),2);
    AddBlank(AResponse);
    //To Do
   end; 
 end;

 {Add Country, CodePage, Locale and Language}
 AddBold(AResponse,'Country, CodePage, Locale and Language','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'COUNTRY_DEFAULT:',IntToStr(COUNTRY_DEFAULT),2);
 AddItemEx(AResponse,'CODEPAGE_OEM_DEFAULT:',IntToStr(CODEPAGE_OEM_DEFAULT),2);
 AddItemEx(AResponse,'CODEPAGE_ANSI_DEFAULT:',IntToStr(CODEPAGE_ANSI_DEFAULT),2);
 AddItemEx(AResponse,'CODEPAGE_CONSOLE_INPUT:',IntToStr(CODEPAGE_CONSOLE_INPUT),2);
 AddItemEx(AResponse,'CODEPAGE_CONSOLE_OUTPUT:',IntToStr(CODEPAGE_CONSOLE_OUTPUT),2);
 AddItemEx(AResponse,'LOCALE_DEFAULT:',IntToStr(LOCALE_DEFAULT),2);
 AddItemEx(AResponse,'KEYMAP_DEFAULT:',KEYMAP_DEFAULT,2);
 AddBlank(AResponse);
 
 {Add Footer}
 AddFooter(AResponse); 
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TWebStatusDeviceTree}
constructor TWebStatusDeviceTree.Create(AMain:TWebStatusMain);
begin
 {}
 FCaption:='Device Tree'; {Must be before create for register}
 inherited Create(AMain);
 Name:='/devicetree';
 
 if FMain <> nil then Name:=FMain.Name + Name;
end; 

{==============================================================================}

function TWebStatusDeviceTree.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
var
 Index:LongWord;
 Size:UInt64;
 {$IFDEF CPU32}
 Range:LongWord;
 {$ENDIF CPU32}
 Address:PtrUInt;
 Header:PDTBHeader;
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

 {Add Header}
 AddHeader(AResponse,GetTitle,Self); 

 {Add Information}
 AddBold(AResponse,'DTB Information','');
 AddBlank(AResponse);
 AddItemEx(AResponse,'Data Valid:',BooleanToString(DeviceTreeValid),2);
 AddBlank(AResponse);
 AddItemEx(AResponse,'Base Address:','0x' + AddrToHex(DeviceTreeGetBase),2);
 AddItemEx(AResponse,'Total Size:',IntToStr(DeviceTreeGetSize),2);
 AddBlank(AResponse);

 {Check Valid}
 if DeviceTreeValid then
  begin
   {Get Header}
   Header:=PDTBHeader(DeviceTreeGetBase);
   
   {Add Header}
   AddBold(AResponse,'DTB Header','');
   AddBlank(AResponse);
   AddItemEx(AResponse,'Magic:','0x' + IntToHex(LongWordBEtoN(Header.Magic),8),2);
   AddItemEx(AResponse,'Total Size:',IntToStr(LongWordBEtoN(Header.TotalSize)),2);
   AddItemEx(AResponse,'Structure Offset:',IntToStr(LongWordBEtoN(Header.StructureOffset)),2);
   AddItemEx(AResponse,'Strings Offset:',IntToStr(LongWordBEtoN(Header.StringsOffset)),2);
   AddItemEx(AResponse,'Reservation Offset:',IntToStr(LongWordBEtoN(Header.ReservationOffset)),2);
   AddItemEx(AResponse,'Version:',IntToStr(LongWordBEtoN(Header.Version)),2);
   AddItemEx(AResponse,'Compatible Version:',IntToStr(LongWordBEtoN(Header.CompatibleVersion)),2);
   AddItemEx(AResponse,'Boot CPUID:',IntToStr(LongWordBEtoN(Header.BootCPUID)),2);
   AddItemEx(AResponse,'Strings Size:',IntToStr(LongWordBEtoN(Header.StringsSize)),2);
   AddItemEx(AResponse,'Structure Size:',IntToStr(LongWordBEtoN(Header.StructureSize)),2);
   AddBlank(AResponse);
  
   {Add Memory}
   AddBold(AResponse,'DTB Memory','');
   AddBlank(AResponse);
   Index:=0;
   while DeviceTreeGetMemory(Index,{$IFDEF CPU32}Range,{$ENDIF CPU32}Address,Size) do
    begin
     AddItemEx(AResponse,IntToStr(Index) + ' - Address: 0x' + {$IFDEF CPU32}IntToHex(Range,8) + ':' + {$ENDIF CPU32}AddrToHex(Address),'Size: ' + IntToStr(Size),2);
     
     Inc(Index);
    end;
   AddBlank(AResponse);

   {Add Reservations}
   AddBold(AResponse,'DTB Reservations','');
   AddBlank(AResponse);
   Index:=0;
   while DeviceTreeGetReservation(Index,Address,Size) do
    begin
     AddItemEx(AResponse,IntToStr(Index) + ': Address: 0x' + AddrToHex(Address),'Size: ' + IntToStr(Size),2);
    end;
   AddBlank(AResponse);
  
   {$IFDEF DEVICE_TREE_ENUMERATION}
   {Add Tree}
   AddBold(AResponse,'DTB Tree','');
   AddBlank(AResponse);
   
   {Setup Data}
   Data.Document:=Self;
   Data.Host:=AHost;
   Data.Request:=ARequest;
   Data.Response:=AResponse;
   Data.Data:=nil;
   
   {Display Tree}
   DeviceTreeLogTreeEx(INVALID_HANDLE_VALUE,WebStatusDeviceTreeLogOutput,nil,@Data);
   
   AddBlank(AResponse);
   {$ENDIF DEVICE_TREE_ENUMERATION}
  end;
  
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
 AddItemEx(AResponse,'SpinRecursionThread:','0x' + HandleToHex(SpinRecursionThread),2);
 AddItemEx(AResponse,'SpinIRQThreadCounter:',IntToStr(SpinIRQThreadCounter),2);
 AddItemEx(AResponse,'SpinFIQThreadCounter:',IntToStr(SpinFIQThreadCounter),2);
 AddItemEx(AResponse,'SpinSWIThreadCounter:',IntToStr(SpinSWIThreadCounter),2);
 AddItemEx(AResponse,'SpinIdleThreadCounter:',IntToStr(SpinIdleThreadCounter),2);
 AddItemEx(AResponse,'MutexDeadlockCounter:',IntToStr(MutexDeadlockCounter),2);
 AddItemEx(AResponse,'MutexRecursionCounter:',IntToStr(MutexRecursionCounter),2);
 AddItemEx(AResponse,'MutexRecursionThread:','0x' + HandleToHex(MutexRecursionThread),2);
 AddItemEx(AResponse,'MutexIRQThreadCounter:',IntToStr(MutexIRQThreadCounter),2);
 AddItemEx(AResponse,'MutexFIQThreadCounter:',IntToStr(MutexFIQThreadCounter),2);
 AddItemEx(AResponse,'MutexSWIThreadCounter:',IntToStr(MutexSWIThreadCounter),2);
 AddItemEx(AResponse,'MutexIdleThreadCounter:',IntToStr(MutexIdleThreadCounter),2);
 AddItemEx(AResponse,'CriticalSectionDeadlockCounter:',IntToStr(CriticalSectionDeadlockCounter),2);
 AddItemEx(AResponse,'SemaphoreDeadlockCounter:',IntToStr(SemaphoreDeadlockCounter),2);
 AddItemEx(AResponse,'SynchronizerDeadlockCounter:',IntToStr(SynchronizerDeadlockCounter),2);
 AddItemEx(AResponse,'SynchronizerRecursionCounter:',IntToStr(SynchronizerRecursionCounter),2);
 AddItemEx(AResponse,'ConditionDeadlockCounter:',IntToStr(ConditionDeadlockCounter),2);
 AddItemEx(AResponse,'CompletionDeadlockCounter:',IntToStr(CompletionDeadlockCounter),2);
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
     AddItemEx(AResponse,'SchedulerSwitchThread:',CPUIDToString(Count) + ': ' + '0x' + HandleToHex(Thread) + ' (' + ThreadGetName(Thread) + ')',2);
    end
   else
    begin
     Thread:=SchedulerSwitchThread[Count];
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + '0x' + HandleToHex(Thread) + ' (' + ThreadGetName(Thread) + ')',2);
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
     AddItemEx(AResponse,'SchedulerRescheduleThread:',CPUIDToString(Count) + ': ' + '0x' + HandleToHex(Thread) + ' (' + ThreadGetName(Thread) + ')',2);
    end
   else
    begin
     Thread:=SchedulerRescheduleThread[Count];
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + '0x' + HandleToHex(Thread) + ' (' + ThreadGetName(Thread) + ')',2);
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
     AddItemEx(AResponse,'DispatchSystemCallCounter:',CPUIDToString(Count) + ': ' + IntToStr(DispatchSystemCallCounter[Count]),2);
    end
   else
    begin
     AddItemEx(AResponse,'',CPUIDToString(Count) + ': ' + IntToStr(DispatchSystemCallCounter[Count]),2);
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
 AddItemEx(AResponse,'HardwareExceptionAddress:','0x' + AddrToHex(HardwareExceptionAddress),2);
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
   AddHeaderEx(AResponse,GetTitle,'',Self,FColumns); 
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

 {Register Handles Page}
 WebStatusHandles:=TWebStatusHandles.Create(WebStatusMain);
 AListener.RegisterDocument(AHost,WebStatusHandles);
 
 {Register USB Page}
 WebStatusUSB:=TWebStatusUSB.Create(WebStatusMain);
 AListener.RegisterDocument(AHost,WebStatusUSB);

 {Register PCI Page}
 WebStatusPCI:=TWebStatusPCI.Create(WebStatusMain);
 AListener.RegisterDocument(AHost,WebStatusPCI);

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

 {Register Touch Page}
 WebStatusTouch:=TWebStatusTouch.Create(WebStatusMain);
 AListener.RegisterDocument(AHost,WebStatusTouch);

 {Register Framebuffer Page}
 WebStatusFramebuffer:=TWebStatusFramebuffer.Create(WebStatusMain);
 AListener.RegisterDocument(AHost,WebStatusFramebuffer);

 {Register Environment Page}
 WebStatusEnvironment:=TWebStatusEnvironment.Create(WebStatusMain);
 AListener.RegisterDocument(AHost,WebStatusEnvironment);

 {Register PageTables Page}
 WebStatusPageTables:=TWebStatusPageTables.Create(WebStatusMain);
 AListener.RegisterDocument(AHost,WebStatusPageTables);

 {Register VectorTables Page}
 WebStatusVectorTables:=TWebStatusVectorTables.Create(WebStatusMain);
 AListener.RegisterDocument(AHost,WebStatusVectorTables);

 {Register IRQFIQSWI Page}
 WebStatusIRQFIQSWI:=TWebStatusIRQFIQSWI.Create(WebStatusMain);
 AListener.RegisterDocument(AHost,WebStatusIRQFIQSWI);

 {Register GPIO Page}
 WebStatusGPIO:=TWebStatusGPIO.Create(WebStatusMain);
 AListener.RegisterDocument(AHost,WebStatusGPIO);
 
 {Register Configuration Page}
 WebStatusConfiguration:=TWebStatusConfiguration.Create(WebStatusMain);
 AListener.RegisterDocument(AHost,WebStatusConfiguration);

 {Register DeviceTree Page}
 WebStatusDeviceTree:=TWebStatusDeviceTree.Create(WebStatusMain);
 AListener.RegisterDocument(AHost,WebStatusDeviceTree);

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

 {Deregister DeviceTree Page}
 AListener.DeregisterDocument(AHost,WebStatusDeviceTree);
 WebStatusDeviceTree.Free;
 
 {Deregister Configuration Page}
 AListener.DeregisterDocument(AHost,WebStatusConfiguration);
 WebStatusConfiguration.Free;
  
 {Deregister IRQFIQSWI Page}
 AListener.DeregisterDocument(AHost,WebStatusIRQFIQSWI);
 WebStatusIRQFIQSWI.Free;
  
 {Deregister VectorTables Page}
 AListener.DeregisterDocument(AHost,WebStatusVectorTables);
 WebStatusVectorTables.Free;
  
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

 {Deregister Touch Page}
 AListener.DeregisterDocument(AHost,WebStatusTouch);
 WebStatusTouch.Free;
 
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

 {Deregister PCI Page}
 AListener.DeregisterDocument(AHost,WebStatusPCI);
 WebStatusPCI.Free;

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
procedure WebStatusDeviceTreeLogOutput(const AText:String;Data:Pointer);
var
 Value:String;
 Offset:LongWord;
 Document:TWebStatusSub;
 Response:THTTPServerResponse;
begin
 {}
 {Check Data}
 if Data = nil then Exit;

 {Get Document}
 Document:=PWebStatusData(Data).Document;
 if Document = nil then Exit;
 
 {Get Response}
 Response:=PWebStatusData(Data).Response;
 if Response = nil then Exit;

 {Check Length}
 if Length(AText) <= DeviceTreeMaxColumns then
  begin
   {Add Output}
   Document.AddItemSpan(Response,'<pre>' + AText + '</pre>',2,False);
  end
 else
  begin
   {Split Output}
   Value:=AText;
   
   {Output Sections}
   while Length(Value) > DeviceTreeMaxColumns do
    begin
     {Start at Max Columns}
     Offset:=DeviceTreeMaxColumns;
     
     {Step Backward}
     while Value[Offset] <> ' ' do
      begin
       Dec(Offset);
       
       if Offset <= (DeviceTreeMaxColumns - DeviceTreeColumnOffset)  then Break;
      end;
      
     {Step Forward}
     while Value[Offset] <> ' ' do
      begin
       Inc(Offset);
       
       if Offset >= (DeviceTreeMaxColumns + DeviceTreeColumnOffset) then Break;
      end;
     
     {Default to Max Columns}
     if Value[Offset] <> ' ' then Offset:=DeviceTreeMaxColumns;
     
     {Output Text}
     Document.AddItemSpan(Response,'<pre>' + Copy(Value,1,Offset) + '</pre>',2,False); 
     
     {Split Text}
     Delete(Value,1,Offset);
    end;
   
   {Output Last}
   if Length(Value) > 0 then
    begin
     Document.AddItemSpan(Response,'<pre>' + Value + '</pre>',2,False);
    end;
  end;
end;

{==============================================================================}

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
 Document.AddItem4Column(Response,Document.MakeLink(IntToStr(Device.DeviceId),Document.Name + '?action=device&id=' + IntToStr(Device.DeviceId)),DeviceGetName(Device),DeviceClassToString(Device.DeviceClass),DeviceBusToString(Device.DeviceBus));
 
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

function WebStatusHandleEnumerate(Handle:PHandleEntry;Data:Pointer):LongWord;
var
 Count:LongWord;
 FlagNames:TStringList;
 Document:TWebStatusSub;
 Response:THTTPServerResponse;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Handle}
 if Handle = nil then Exit;
 
 {Check Data}
 if Data = nil then Exit;
 
 {Get Document}
 Document:=PWebStatusData(Data).Document;
 if Document = nil then Exit;
 
 {Get Response}
 Response:=PWebStatusData(Data).Response;
 if Response = nil then Exit;
 
 {Get Flag Names}
 FlagNames:=TWebStatusHandles(Document).FlagsToFlagNames(Handle.Flags);
 
 {Add Handle}
 Document.AddItem5Column(Response,IntToStr(Handle.Handle),HandleTypeToString(Handle.HandleType),Handle.Name,IntToStr(Handle.Count),FlagNames.Strings[0]);
 
 {Check Flag Count}
 if FlagNames.Count > 1 then
  begin
   for Count:=1 to FlagNames.Count - 1 do
    begin
     {Add Flag Name}
     Document.AddItem5Column(Response,'','','','',FlagNames.Strings[Count]);
    end;
  end;
 FlagNames.Free;
 
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
 Document.AddItem4Column(Response,Document.MakeLink(IntToStr(Device.USBId),Document.Name + '?action=usbdevice&id=' + IntToStr(Device.USBId)),DeviceGetName(@Device.Device),USBClassCodeToString(Device.Descriptor.bDeviceClass),USBDeviceStatusToString(Device.USBStatus));
 
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
 Document.AddItem4Column(Response,Document.MakeLink(IntToStr(Host.HostId),Document.Name + '?action=usbhost&id=' + IntToStr(Host.HostId)),DeviceGetName(@Host.Device),USBHostStateToString(Host.HostState),USBHostTypeToString(Host.Device.DeviceType));
 
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

procedure WebStatusUSBLogOutput(const AText:String;Data:Pointer); 
var
 Document:TWebStatusSub;
 Response:THTTPServerResponse;
begin
 {}
 {Check Data}
 if Data = nil then Exit;

 {Get Document}
 Document:=PWebStatusData(Data).Document;
 if Document = nil then Exit;
 
 {Get Response}
 Response:=PWebStatusData(Data).Response;
 if Response = nil then Exit;

 {Add Output}
 Document.AddItemSpan(Response,'<pre>' + AText + '</pre>',2,False);
end;

{==============================================================================}

function WebStatusUSBLogDeviceCallback(Device:PUSBDevice;Data:Pointer):LongWord;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;
 
 WebStatusUSBLogOutput('[USB Device Id: ' + IntToStr(Device.USBId) + ' Address: ' + IntToStr(Device.Address) + ']',Data);
 
 USBLogDeviceDescriptor(Device,Device.Descriptor,WebStatusUSBLogOutput,Data);
 USBLogDeviceConfiguration(Device,WebStatusUSBLogOutput,Data);

 WebStatusUSBLogOutput('',Data);
 
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function WebStatusUSBLogTreeCallback(Device:PUSBDevice;Data:Pointer):LongWord;
const
 WEBSTATUS_USB_TREE_SPACES_PER_LEVEL = 2;
 WEBSTATUS_USB_TREE_LINES_PER_PORT = 2;
 
var
 Count:Integer;
 WorkBuffer:String;
 LinesCount:Integer;
 SpacesCount:Integer;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Data}
 if Data = nil then Exit;
 
 {Check Device}
 if Device = nil then Exit;
 
 {Check Host}
 if Device.Host = PWebStatusData(Data).Data then
  begin
   {Output Diagram}
   WorkBuffer:='';
   if Device.Depth <> 0 then
    begin
     SpacesCount:=(Device.Depth - 1) * (WEBSTATUS_USB_TREE_SPACES_PER_LEVEL + WEBSTATUS_USB_TREE_SPACES_PER_LEVEL - 1);
     
     if WEBSTATUS_USB_TREE_LINES_PER_PORT > 1 then
      begin
       for LinesCount:=0 to WEBSTATUS_USB_TREE_LINES_PER_PORT - 2 do
        begin
         WorkBuffer:=StringOfChar(' ',SpacesCount) + '|';
         
         WebStatusUSBLogOutput(WorkBuffer,Data);
        end;
      end;  
     
     WorkBuffer:=StringOfChar(' ',SpacesCount) + '|' + StringOfChar('_',WEBSTATUS_USB_TREE_SPACES_PER_LEVEL);
    end;
  
   {Output Device}
   WorkBuffer:=WorkBuffer + 'Id: ' + IntToStr(Device.USBId) + ' / Addr: ' + IntToStr(Device.Address) + ' [' + USBDeviceToString(Device) + ']';
   WebStatusUSBLogOutput(WorkBuffer,Data);
  end;
  
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function WebStatusPCIDeviceEnumerate(Device:PPCIDevice;Data:Pointer):LongWord;
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
 Document.AddItem4Column(Response,Document.MakeLink(IntToStr(Device.PCIId),Document.Name + '?action=pcidevice&id=' + IntToStr(Device.PCIId)),DeviceGetName(@Device.Device),'',PCIDeviceStatusToString(Device.PCIStatus));
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function WebStatusPCIHostEnumerate(Host:PPCIHost;Data:Pointer):LongWord;
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
 Document.AddItem4Column(Response,Document.MakeLink(IntToStr(Host.HostId),Document.Name + '?action=pcihost&id=' + IntToStr(Host.HostId)),DeviceGetName(@Host.Device),PCIHostStateToString(Host.HostState),PCIHostTypeToString(Host.Device.DeviceType));
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function WebStatusPCIDriverEnumerate(Driver:PPCIDriver;Data:Pointer):LongWord;
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
 Document.AddItem4Column(Response,Document.MakeLink(IntToStr(MMC.MMCId),Document.Name + '?action=mmcdevice&id=' + IntToStr(MMC.MMCId)),DeviceGetName(@MMC.Device),MMCDeviceStateToString(MMC.MMCState),MMCDeviceTypeToString(MMC.Device.DeviceType));
 
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
 Document.AddItem4Column(Response,Document.MakeLink(IntToStr(SDHCI.SDHCIId),Document.Name + '?action=sdhcihost&id=' + IntToStr(SDHCI.SDHCIId)),DeviceGetName(@SDHCI.Device),SDHCIDeviceStateToString(SDHCI.SDHCIState),SDHCIDeviceTypeToString(SDHCI.Device.DeviceType));
  
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function WebStatusSDIODriverEnumerate(Driver:PSDIODriver;Data:Pointer):LongWord;
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
 Document.AddItem5Column(Response,Document.MakeLink(IntToStr(Network.NetworkId),Document.Name + '?action=network&id=' + IntToStr(Network.NetworkId)),DeviceGetName(@Network.Device),NetworkDeviceStateToString(Network.NetworkState),NetworkDeviceStatusToString(Network.NetworkStatus),NetworkDeviceTypeToString(Network.Device.DeviceType));
 
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
 Document.AddItem4Column(Response,Document.MakeLink(IntToStr(Mouse.MouseId),Document.Name + '?action=mouse&id=' + IntToStr(Mouse.MouseId)),DeviceGetName(@Mouse.Device),MouseDeviceStateToString(Mouse.MouseState),MouseDeviceTypeToString(Mouse.Device.DeviceType));
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function WebStatusTouchEnumerate(Touch:PTouchDevice;Data:Pointer):LongWord;
var
 Document:TWebStatusSub;
 Response:THTTPServerResponse;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Touch}
 if Touch = nil then Exit;
 
 {Check Data}
 if Data = nil then Exit;
 
 {Get Document}
 Document:=PWebStatusData(Data).Document;
 if Document = nil then Exit;
 
 {Get Response}
 Response:=PWebStatusData(Data).Response;
 if Response = nil then Exit;
 
 {Add Touch}
 Document.AddItem4Column(Response,Document.MakeLink(IntToStr(Touch.TouchId),Document.Name + '?action=touch&id=' + IntToStr(Touch.TouchId)),DeviceGetName(@Touch.Device),TouchDeviceStateToString(Touch.TouchState),TouchDeviceTypeToString(Touch.Device.DeviceType));
 
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
 Document.AddItem4Column(Response,Document.MakeLink(IntToStr(Keyboard.KeyboardId),Document.Name + '?action=keyboard&id=' + IntToStr(Keyboard.KeyboardId)),DeviceGetName(@Keyboard.Device),KeyboardDeviceStateToString(Keyboard.KeyboardState),KeyboardDeviceTypeToString(Keyboard.Device.DeviceType));
 
 Result:=ERROR_SUCCESS;
end;
 
{==============================================================================}
 
function WebStatusConsoleWindowEnumerate(Console:PConsoleDevice;Handle:TWindowHandle;Data:Pointer):LongWord;
var
 Window:PConsoleWindow;
 FlagNames:TStringList;
 Document:TWebStatusDevices;
 Response:THTTPServerResponse;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Console}
 if Console = nil then Exit;
 
 {Check Data}
 if Data = nil then Exit;
 
 {Get Document}
 Document:=TWebStatusDevices(PWebStatusData(Data).Document);
 if Document = nil then Exit;
 
 {Get Response}
 Response:=PWebStatusData(Data).Response;
 if Response = nil then Exit;
 
 {Get Window}
 Window:=ConsoleWindowCheck(Console,PConsoleWindow(Handle));
 if Window <> nil then
  begin
   {Lock Window}
   if MutexLock(Window.Lock) = ERROR_SUCCESS then
    begin
     try
      {Get Flags Names}
      FlagNames:=Document.ConsoleWindowFlagsToFlagNames(Window.WindowFlags);
      
      Document.AddItem(Response,'Handle:',HandleToHex(Handle));
      Document.AddItem(Response,'Position:',ConsolePositionToString(Window.Position));
      Document.AddItem(Response,'State:',ConsoleWindowStateToString(Window.WindowState));
      Document.AddItem(Response,'Mode:',ConsoleWindowModeToString(Window.WindowMode));
      Document.AddBlank(Response);
      Document.AddItem(Response,'X1:',IntToStr(Window.X1));
      Document.AddItem(Response,'Y1:',IntToStr(Window.Y1));
      Document.AddItem(Response,'X2:',IntToStr(Window.X2));
      Document.AddItem(Response,'Y2:',IntToStr(Window.Y2));
      Document.AddItem(Response,'Width:',IntToStr(Window.Width));
      Document.AddItem(Response,'Height:',IntToStr(Window.Height));
      Document.AddItem(Response,'Offset X:',IntToStr(Window.OffsetX));
      Document.AddItem(Response,'Offset Y:',IntToStr(Window.OffsetY));
      Document.AddItem(Response,'Min X:',IntToStr(Window.MinX));
      Document.AddItem(Response,'Min Y:',IntToStr(Window.MinY));
      Document.AddItem(Response,'Max X:',IntToStr(Window.MaxX));
      Document.AddItem(Response,'Max Y:',IntToStr(Window.MaxY));
      Document.AddItem(Response,'X:',IntToStr(Window.X));
      Document.AddItem(Response,'Y:',IntToStr(Window.Y));
      Document.AddItem(Response,'Cols:',IntToStr(Window.Cols));
      Document.AddItem(Response,'Rows:',IntToStr(Window.Rows));
      Document.AddBlank(Response);
      Document.AddItem(Response,'Format:',ColorFormatToString(Window.Format));
      Document.AddItem(Response,'Forecolor:','0x' + IntToHex(Window.Forecolor,8));
      Document.AddItem(Response,'Backcolor:','0x' + IntToHex(Window.Backcolor,8));
      Document.AddItem(Response,'Borderwidth:',IntToStr(Window.Borderwidth));
      Document.AddItem(Response,'Bordercolor:','0x' + IntToHex(Window.Bordercolor,8));
      Document.AddBlank(Response);
      Document.AddItem(Response,'Font Width:',IntToStr(Window.FontWidth));
      Document.AddItem(Response,'Font Height:',IntToStr(Window.FontHeight));
      Document.AddBlank(Response);
      Document.AddItem(Response,'Cursor X:',IntToStr(Window.CursorX));
      Document.AddItem(Response,'Cursor Y:',IntToStr(Window.CursorY));
      Document.AddBlank(Response);
      Document.AddItem(Response,'Caret X:',IntToStr(Window.CaretX));
      Document.AddItem(Response,'Caret Y:',IntToStr(Window.CaretY));
      Document.AddBlank(Response);
      Document.AddItem(Response,'History Count:',IntToStr(Window.HistoryCount));
      Document.AddBlank(Response);
      Document.AddBlank(Response);
    
      FlagNames.Free;
     finally
      {Unlock Window}
      MutexUnlock(Window.Lock);
     end; 
    end; 
  end;

 Result:=ERROR_SUCCESS;
end; 

{==============================================================================}
{==============================================================================}

end.
 