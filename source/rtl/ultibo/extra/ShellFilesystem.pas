{
Ultibo FileSystem Shell extension unit.

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

 

Shell FileSystem
================

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit ShellFilesystem;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,FileSystem,SysUtils,Classes,Ultibo,UltiboClasses,UltiboUtils,Shell;

//To Do //Change some of the direct calls to use FileSysDriver instead (eg Device.ActivatePartition etc)

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {Shell FileSystem specific constants}
 
 {Shell FileSystem Command constants}
 SHELL_FILESYS_COMMAND_FILESYSTEM = 'FILESYSTEM';
 SHELL_FILESYS_COMMAND_CONTROLLER = 'CONTROLLER';
 SHELL_FILESYS_COMMAND_DISK       = 'DISK';
 SHELL_FILESYS_COMMAND_PARTITION  = 'PARTITION';
 SHELL_FILESYS_COMMAND_VOLUME     = 'VOLUME';
 SHELL_FILESYS_COMMAND_DRIVE      = 'DRIVE';
 
 SHELL_FILESYS_COMMAND_CACHE      = 'CACHE';
 
 SHELL_FILESYS_COMMAND_DIR        = 'DIR';
 SHELL_FILESYS_COMMAND_CHDIR      = 'CD';
 SHELL_FILESYS_COMMAND_MKDIR      = 'MD';
 SHELL_FILESYS_COMMAND_RMDIR      = 'RD';

 SHELL_FILESYS_COMMAND_TYPE       = 'TYPE';
 SHELL_FILESYS_COMMAND_COPY       = 'COPY';
 SHELL_FILESYS_COMMAND_MOVE       = 'MOVE';
 SHELL_FILESYS_COMMAND_DEL        = 'DEL';
 SHELL_FILESYS_COMMAND_REN        = 'REN';
 SHELL_FILESYS_COMMAND_ATTRIB     = 'ATTRIB';
 SHELL_FILESYS_COMMAND_TOUCH      = 'TOUCH';

 SHELL_FILESYS_COMMAND_VOL        = 'VOL';
 SHELL_FILESYS_COMMAND_LABEL      = 'LABEL';

 SHELL_FILESYS_COMMAND_DELTREE    = 'DELTREE';
 SHELL_FILESYS_COMMAND_XCOPY      = 'XCOPY';
 
 {Shell FileSystem Alias constants}
 SHELL_FILESYS_ALIAS_LS       = 'LS';
 SHELL_FILESYS_ALIAS_CD       = 'CHDIR';
 SHELL_FILESYS_ALIAS_CDSLASH  = 'CD\';
 SHELL_FILESYS_ALIAS_CDDOTDOT = 'CD..';
 SHELL_FILESYS_ALIAS_MD       = 'MKDIR';
 SHELL_FILESYS_ALIAS_RD       = 'RMDIR';
 SHELL_FILESYS_ALIAS_RM       = 'RM';
 SHELL_FILESYS_ALIAS_DELETE   = 'DELETE';
 SHELL_FILESYS_ALIAS_ERASE    = 'ERASE';
 SHELL_FILESYS_ALIAS_RENAME   = 'RENAME';
 
 {Shell FileSystem Action constants}
 SHELL_FILESYS_ACTION_LIST       = 'LIST';
 SHELL_FILESYS_ACTION_SHOW       = 'SHOW';
 
 SHELL_FILESYS_ACTION_LABEL      = 'LABEL';
 SHELL_FILESYS_ACTION_FILL       = 'FILL';
 SHELL_FILESYS_ACTION_SIGNATURE  = 'SIGNATURE';
 SHELL_FILESYS_ACTION_INITIALIZE = 'INITIALIZE';

 SHELL_FILESYS_ACTION_EJECT      = 'EJECT';
 SHELL_FILESYS_ACTION_LOCK       = 'LOCK';
 SHELL_FILESYS_ACTION_UNLOCK     = 'UNLOCK';
 
 SHELL_FILESYS_ACTION_ADD        = 'ADD';
 SHELL_FILESYS_ACTION_DELETE     = 'DELETE';
 SHELL_FILESYS_ACTION_ACTIVATE   = 'ACTIVATE';
 SHELL_FILESYS_ACTION_DEACTIVATE = 'DEACTIVATE';

 SHELL_FILESYS_ACTION_FORMAT     = 'FORMAT';
 SHELL_FILESYS_ACTION_DEFRAG     = 'DEFRAG';
 SHELL_FILESYS_ACTION_CHECK      = 'CHECK';
 SHELL_FILESYS_ACTION_CLEAN      = 'CLEAN';
 SHELL_FILESYS_ACTION_DIRTY      = 'DIRTY';
 SHELL_FILESYS_ACTION_MOUNT      = 'MOUNT';
 SHELL_FILESYS_ACTION_DISMOUNT   = 'DISMOUNT';

 SHELL_FILESYS_ACTION_STATS      = 'STATS';
 SHELL_FILESYS_ACTION_FLUSH      = 'FLUSH';
 SHELL_FILESYS_ACTION_DISCARD    = 'DISCARD';

 SHELL_FILESYS_ACTION_START      = 'START';
 SHELL_FILESYS_ACTION_STOP       = 'STOP';
 
 {Shell FileSystem Parameter constants}
 SHELL_FILESYS_PARAMETER_HELP        = '?';
 SHELL_FILESYS_PARAMETER_PAGE        = 'P';
 SHELL_FILESYS_PARAMETER_SHORT       = 'X';
 SHELL_FILESYS_PARAMETER_SUBDIR      = 'S';
 SHELL_FILESYS_PARAMETER_OVERWRITE   = 'O';

 SHELL_FILESYS_PARAMETER_READONLY    = '+R';
 SHELL_FILESYS_PARAMETER_ARCHIVE     = '+A';
 SHELL_FILESYS_PARAMETER_SYSTEM      = '+S';
 SHELL_FILESYS_PARAMETER_HIDDEN      = '+H';

 SHELL_FILESYS_PARAMETER_NOTREADONLY = '-R';
 SHELL_FILESYS_PARAMETER_NOTARCHIVE  = '-A';
 SHELL_FILESYS_PARAMETER_NOTSYSTEM   = '-S';
 SHELL_FILESYS_PARAMETER_NOTHIDDEN   = '-H';
 
 {Shell FileSystem Mask constants}
 SHELL_FILESYS_ALLFILES_MASK = '*.*';
 
{==============================================================================}
{type}
 {Shell FileSystem specific types}
 
{==============================================================================}
type
 {Shell FileSystem specific clases}
 TShellFileSysFileSystem = class(TShellCommand)
 public
  {}
  constructor Create;
 private
  {Internal Variables}
 
  {Internal Methods}
 
 protected
  {Internal Variables}

  {Internal Methods}
  
 public
  {Public Properties}

  {Public Methods}
  function DoHelp(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoInfo(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; override;
 end;

 TShellFileSysController = class(TShellCommand)
 public
  {}
  constructor Create;
 private
  {Internal Variables}
 
  {Internal Methods}
 
 protected
  {Internal Variables}

  {Internal Methods}
  
 public
  {Public Properties}

  {Public Methods}
  function DoHelp(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoInfo(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; override;
 end;

 TShellFileSysDisk = class(TShellCommand)
 public
  {}
  constructor Create;
 private
  {Internal Variables}
 
  {Internal Methods}
 
 protected
  {Internal Variables}

  {Internal Methods}
  
 public
  {Public Properties}

  {Public Methods}
  function DoHelp(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoInfo(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; override;
 end;

 TShellFileSysPartition = class(TShellCommand)
 public
  {}
  constructor Create;
 private
  {Internal Variables}
 
  {Internal Methods}
 
 protected
  {Internal Variables}

  {Internal Methods}
  
 public
  {Public Properties}

  {Public Methods}
  function DoHelp(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoInfo(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; override;
 end;

 TShellFileSysVolume = class(TShellCommand)
 public
  {}
  constructor Create;
 private
  {Internal Variables}
 
  {Internal Methods}
 
 protected
  {Internal Variables}

  {Internal Methods}
  
 public
  {Public Properties}

  {Public Methods}
  function DoHelp(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoInfo(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; override;
 end;

 TShellFileSysDrive = class(TShellCommand)
 public
  {}
  constructor Create;
 private
  {Internal Variables}
 
  {Internal Methods}
 
 protected
  {Internal Variables}

  {Internal Methods}
  
 public
  {Public Properties}

  {Public Methods}
  function DoHelp(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoInfo(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; override;
 end;
 
 TShellFileSysCache = class(TShellCommand)
 public
  {}
  constructor Create;
 private
  {Internal Variables}
 
  {Internal Methods}
 
 protected
  {Internal Variables}

  {Internal Methods}
  
 public
  {Public Properties}

  {Public Methods}
  function DoHelp(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoInfo(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; override;
 end;
 
 TShellFileSysDir = class(TShellCommand)
 public
  {}
  constructor Create;
  destructor Destroy; override;
 private
  {Internal Variables}
 
  {Internal Methods}
  function DirSearch(AShell:TShell;ASession:TShellSession;const APath,AName:String;AShort,ASubdir:Boolean;var AFiles,AFolders,ABytes:Int64):Boolean;
  function DirOutput(AShell:TShell;ASession:TShellSession;const ASearchRec:TFileSearchRec;AShort:Boolean):Boolean;
 protected
  {Internal Variables}

  {Internal Methods}
  
 public
  {Public Properties}

  {Public Methods}
  function DoHelp(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoInfo(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; override;
 end;

 TShellFileSysChdir = class(TShellCommand)
 public
  {}
  constructor Create;
  destructor Destroy; override;
 private
  {Internal Variables}
 
  {Internal Methods}
 
 protected
  {Internal Variables}

  {Internal Methods}
  
 public
  {Public Properties}

  {Public Methods}
  function DoHelp(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoInfo(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoDefault(AShell:TShell;ASession:TShellSession;const AName:String;AParameters:TStrings):Boolean; override;
  function DoExtended(AShell:TShell;ASession:TShellSession;const AName:String;AParameters:TStrings):Boolean; override;
 end;
  
 TShellFileSysMkdir = class(TShellCommand)
 public
  {}
  constructor Create;
  destructor Destroy; override;
 private
  {Internal Variables}
 
  {Internal Methods}
 
 protected
  {Internal Variables}

  {Internal Methods}
  
 public
  {Public Properties}

  {Public Methods}
  function DoHelp(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoInfo(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; override;
 end;

 TShellFileSysRmdir = class(TShellCommand)
 public
  {}
  constructor Create;
  destructor Destroy; override;
 private
  {Internal Variables}
 
  {Internal Methods}
 
 protected
  {Internal Variables}

  {Internal Methods}
  
 public
  {Public Properties}

  {Public Methods}
  function DoHelp(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoInfo(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; override;
 end;
 
 TShellFileSysType = class(TShellCommand)
 public
  {}
  constructor Create;
 private
  {Internal Variables}
 
  {Internal Methods}
 
 protected
  {Internal Variables}

  {Internal Methods}
  
 public
  {Public Properties}

  {Public Methods}
  function DoHelp(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoInfo(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; override;
 end;

 TShellFileSysCopy = class(TShellCommand)
 public
  {}
  constructor Create;
 private
  {Internal Variables}
 
  {Internal Methods}
 
 protected
  {Internal Variables}

  {Internal Methods}
  
 public
  {Public Properties}

  {Public Methods}
  function DoHelp(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoInfo(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; override;
 end;

 TShellFileSysMove = class(TShellCommand)
 public
  {}
  constructor Create;
 private
  {Internal Variables}
 
  {Internal Methods}
 
 protected
  {Internal Variables}

  {Internal Methods}
  
 public
  {Public Properties}

  {Public Methods}
  function DoHelp(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoInfo(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; override;
 end;

 TShellFileSysDel = class(TShellCommand)
 public
  {}
  constructor Create;
  destructor Destroy; override;
 private
  {Internal Variables}
 
  {Internal Methods}
 
 protected
  {Internal Variables}

  {Internal Methods}
  
 public
  {Public Properties}

  {Public Methods}
  function DoHelp(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoInfo(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; override;
 end;

 TShellFileSysRen = class(TShellCommand)
 public
  {}
  constructor Create;
  destructor Destroy; override;
 private
  {Internal Variables}
 
  {Internal Methods}
 
 protected
  {Internal Variables}

  {Internal Methods}
  
 public
  {Public Properties}

  {Public Methods}
  function DoHelp(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoInfo(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; override;
 end;

 TShellFileSysAttrib = class(TShellCommand)
 public
  {}
  constructor Create;
 private
  {Internal Variables}
 
  {Internal Methods}
  function AttribSearch(AShell:TShell;ASession:TShellSession;const APath,AName:String;ASubdir:Boolean;AMask,AUnmask:LongWord):Boolean;
  function AttribOutput(AShell:TShell;ASession:TShellSession;const ASearchRec:TFileSearchRec):Boolean;
 protected
  {Internal Variables}

  {Internal Methods}
  
 public
  {Public Properties}

  {Public Methods}
  function DoHelp(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoInfo(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; override;
 end;

 TShellFileSysTouch = class(TShellCommand)
 public
  {}
  constructor Create;
 private
  {Internal Variables}
 
  {Internal Methods}
 
 protected
  {Internal Variables}

  {Internal Methods}
  
 public
  {Public Properties}

  {Public Methods}
  function DoHelp(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoInfo(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; override;
 end;

 TShellFileSysVol = class(TShellCommand)
 public
  {}
  constructor Create;
 private
  {Internal Variables}
 
  {Internal Methods}
 
 protected
  {Internal Variables}

  {Internal Methods}
  
 public
  {Public Properties}

  {Public Methods}
  function DoHelp(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoInfo(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; override;
 end;

 TShellFileSysLabel = class(TShellCommand)
 public
  {}
  constructor Create;
 private
  {Internal Variables}
 
  {Internal Methods}
 
 protected
  {Internal Variables}

  {Internal Methods}
  
 public
  {Public Properties}

  {Public Methods}
  function DoHelp(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoInfo(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; override;
 end;
 
 TShellFileSysDeltree = class(TShellCommand)
 public
  {}
  constructor Create;
 private
  {Internal Variables}
 
  {Internal Methods}
  //function DeltreeSearch(AShell:TShell;ASession:TShellSession;const APath,AName:String):Boolean;
  //function DeltreeOutput(AShell:TShell;ASession:TShellSession;const ASearchRec:TFileSearchRec):Boolean;
 protected
  {Internal Variables}

  {Internal Methods}
  
 public
  {Public Properties}

  {Public Methods}
  function DoHelp(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoInfo(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; override;
 end;

 TShellFileSysXcopy = class(TShellCommand)
 public
  {}
  constructor Create;
 private
  {Internal Variables}
 
  {Internal Methods}
 
 protected
  {Internal Variables}

  {Internal Methods}
  
 public
  {Public Properties}

  {Public Methods}
  function DoHelp(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoInfo(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; override;
 end;
 
{==============================================================================}
{var}
 {Shell FileSystem specific variables}

{==============================================================================}
{Initialization Functions}
procedure ShellFileSysInit;

{==============================================================================}
{Shell FileSystem Functions}
 
{==============================================================================}
{Shell FileSystem Helper Functions}
 
{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Shell FileSystem specific variables}
 ShellFileSysInitialized:Boolean;
 
{==============================================================================}
{==============================================================================}
{TShellFileSysFileSystem}
constructor TShellFileSysFileSystem.Create;
begin
 {}
 inherited Create;

 Name:=SHELL_FILESYS_COMMAND_FILESYSTEM;
 Flags:=SHELL_COMMAND_FLAG_INFO or SHELL_COMMAND_FLAG_HELP;
end;

{==============================================================================}

function TShellFileSysFileSystem.DoHelp(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Help}
 AShell.DoOutput(ASession,'List or display currently available file systems');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,' ' + Name + ' LIST              (List all currently available file systems)');
 AShell.DoOutput(ASession,' ' + Name + ' SHOW <FILESYSTEM> (Display information for the specified file system)');
 AShell.DoOutput(ASession,'');
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TShellFileSysFileSystem.DoInfo(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Info}
 Result:=AShell.DoOutput(ASession,'List or display currently available file systems');
end;

{==============================================================================}

function TShellFileSysFileSystem.DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; 
var 
 Parameter:String;
 WorkBuffer:String;
 Recognizer:TRecognizer;
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;

 {Check Parameters}
 if AParameters = nil then Exit;
 
 {Get Parameter}
 Parameter:=AShell.ParameterIndex(0,AParameters);
 
 {Check Parameter}
 if (Length(Parameter) = 0) or (Uppercase(Parameter) = SHELL_FILESYS_ACTION_LIST) then
  begin
   AShell.DoOutput(ASession,'File System Listing');
   AShell.DoOutput(ASession,'-------------------');
   AShell.DoOutput(ASession,'');
   WorkBuffer:='';
   AShell.AddOutput(WorkBuffer,3,'Name');
   AShell.DoOutput(ASession,WorkBuffer);
   AShell.DoOutput(ASession,'');
   
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Add FileSystems}
     Recognizer:=FileSysDriver.GetRecognizerByNext(nil,True,False,FILESYS_LOCK_READ);
     while Recognizer <> nil do
      begin
       WorkBuffer:='';
       AShell.AddOutput(WorkBuffer,3,Recognizer.Name);
       AShell.DoOutput(ASession,WorkBuffer);
       
       Recognizer:=FileSysDriver.GetRecognizerByNext(Recognizer,True,True,FILESYS_LOCK_READ);
      end;
      
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
    end;  
     
   {Return Result}
   Result:=True;
  end
 else if Uppercase(Parameter) = SHELL_FILESYS_ACTION_SHOW then
  begin
   AShell.DoOutput(ASession,'File System Information');
   AShell.DoOutput(ASession,'-----------------------');
   AShell.DoOutput(ASession,'');
  
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     //To Do //GetRecognizerByName
     
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
    end;  
   
   {Return Result}
   Result:=True;
  end
 else
  begin
   {Show Error}
   Result:=AShell.DoError(ASession);
  end;  
end;

{==============================================================================}
{==============================================================================}
{TShellFileSysController}
constructor TShellFileSysController.Create;
begin
 {}
 inherited Create;

 Name:=SHELL_FILESYS_COMMAND_CONTROLLER;
 Flags:=SHELL_COMMAND_FLAG_INFO or SHELL_COMMAND_FLAG_HELP;
end;

{==============================================================================}

function TShellFileSysController.DoHelp(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Help}
 AShell.DoOutput(ASession,'List or display currently available disk controllers');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,' ' + Name + ' LIST              (List all currently available disk controllers)');
 AShell.DoOutput(ASession,' ' + Name + ' SHOW <CONTROLLER> (Display information for the specified disk controller)');
 AShell.DoOutput(ASession,'');
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TShellFileSysController.DoInfo(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Info}
 Result:=AShell.DoOutput(ASession,'List or display currently available disk controllers');
end;

{==============================================================================}

function TShellFileSysController.DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; 
var 
 Parameter:String;
 WorkBuffer:String;
 Controller:TDiskController;
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;

 {Check Parameters}
 if AParameters = nil then Exit;
 
 {Get Parameter}
 Parameter:=AShell.ParameterIndex(0,AParameters);
 
 {Check Parameter}
 if (Length(Parameter) = 0) or (Uppercase(Parameter) = SHELL_FILESYS_ACTION_LIST) then
  begin
   AShell.DoOutput(ASession,'Controller Listing');
   AShell.DoOutput(ASession,'------------------');
   AShell.DoOutput(ASession,'');
   WorkBuffer:='';
   AShell.AddOutput(WorkBuffer,3,'Name');
   AShell.AddOutput(WorkBuffer,20,'Description');
   AShell.DoOutput(ASession,WorkBuffer);
   AShell.DoOutput(ASession,'');
   
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Add Controllers}
     Controller:=FileSysDriver.GetControllerByNext(nil,True,False,FILESYS_LOCK_READ);
     while Controller <> nil do
      begin
       WorkBuffer:='';
       AShell.AddOutput(WorkBuffer,3,Controller.Name);
       AShell.AddOutput(WorkBuffer,20,Controller.Description);
       AShell.DoOutput(ASession,WorkBuffer);
       
       Controller:=FileSysDriver.GetControllerByNext(Controller,True,True,FILESYS_LOCK_READ);
      end;
      
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
    end;  
   
   {Return Result}
   Result:=True;
  end
 else if Uppercase(Parameter) = SHELL_FILESYS_ACTION_SHOW then
  begin
   AShell.DoOutput(ASession,'Controller Information');
   AShell.DoOutput(ASession,'----------------------');
   AShell.DoOutput(ASession,'');
  
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     //To Do
     
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
    end;  
   
   {Return Result}
   Result:=True;
  end
 else
  begin
   {Show Error}
   Result:=AShell.DoError(ASession);
  end;  
end;

{==============================================================================}
{==============================================================================}
{TShellFileSysDisk}
constructor TShellFileSysDisk.Create;
begin
 {}
 inherited Create;

 Name:=SHELL_FILESYS_COMMAND_DISK;
 Flags:=SHELL_COMMAND_FLAG_INFO or SHELL_COMMAND_FLAG_HELP;
end;

{==============================================================================}

function TShellFileSysDisk.DoHelp(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Help}
 AShell.DoOutput(ASession,'List, display or modify currently available disk devices');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,' ' + Name + ' LIST              (List all currently available disk devices)');
 AShell.DoOutput(ASession,' ' + Name + ' SHOW <DISK>       (Display information for the specified disk device)');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,' ' + Name + ' SIGNATURE <DISK>  (Display or modify signature for the specified disk)');
 AShell.DoOutput(ASession,' ' + Name + ' INITIALIZE <DISK> (Remove all partitions and initialize specified disk)');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,' ' + Name + ' EJECT <DISK>      (Eject the disk media from the drive if supported)');
 AShell.DoOutput(ASession,' ' + Name + ' LOCK <DISK>       (Lock the disk media in the drive if supported)');
 AShell.DoOutput(ASession,' ' + Name + ' UNLOCK <DISK>     (Unlock the disk media in the drive if supported)');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,'   Examples:');
 AShell.DoOutput(ASession,'    ' + Name + ' SHOW \Harddisk0');
 AShell.DoOutput(ASession,'    ' + Name + ' SIGNATURE \Harddisk0 12345678');
 AShell.DoOutput(ASession,'    ' + Name + ' INITIALIZE \Harddisk0');
 AShell.DoOutput(ASession,'    ' + Name + ' EJECT \Cdrom0');
 AShell.DoOutput(ASession,'');
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TShellFileSysDisk.DoInfo(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Info}
 Result:=AShell.DoOutput(ASession,'List, display or modify currently available disk devices');
end;

{==============================================================================}

function TShellFileSysDisk.DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; 
var 
 Name:String;
 Start:Int64;
 Count:LongWord;
 Handle:THandle;
 Buffer:Pointer;
 Parameter:String;
 WorkBuffer:String;
 Device:TDiskDevice;
 DiskSignature:LongWord;
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;

 {Check Parameters}
 if AParameters = nil then Exit;
 
 {Get Parameter}
 Parameter:=AShell.ParameterIndex(0,AParameters);
 
 {Check Parameter}
 if (Length(Parameter) = 0) or (Uppercase(Parameter) = SHELL_FILESYS_ACTION_LIST) then
  begin
   AShell.DoOutput(ASession,'Disk Listing');
   AShell.DoOutput(ASession,'------------');
   AShell.DoOutput(ASession,'');
   WorkBuffer:='';
   AShell.AddOutput(WorkBuffer,3,'Name');
   AShell.AddOutput(WorkBuffer,20,'Type');
   AShell.AddOutput(WorkBuffer,50,'Removable');
   AShell.DoOutput(ASession,WorkBuffer);
   AShell.DoOutput(ASession,'');
   
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Add Devices}
     Device:=FileSysDriver.GetDeviceByNext(nil,True,False,FILESYS_LOCK_READ);
     while Device <> nil do
      begin
       WorkBuffer:='';
       AShell.AddOutput(WorkBuffer,3,Device.Name);
       AShell.AddOutput(WorkBuffer,20,MediaTypeToString(Device.MediaType));
       AShell.AddOutput(WorkBuffer,50,BooleanToString(Device.Removable));
       AShell.DoOutput(ASession,WorkBuffer);
       
       Device:=FileSysDriver.GetDeviceByNext(Device,True,True,FILESYS_LOCK_READ);
      end;
      
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
    end;  
   
   {Return Result}
   Result:=True;
  end
 else if Uppercase(Parameter) = SHELL_FILESYS_ACTION_SHOW then
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Get Name}
     Name:=AddLeadingSlash(StripTrailingSlash(AShell.ParameterIndex(1,AParameters)));
     if Length(Name) > 0 then
      begin
       {Get Device}
       Device:=FileSysDriver.GetDeviceByName(Name,True,FILESYS_LOCK_READ);
       if Device <> nil then
        begin
         AShell.DoOutput(ASession,'Disk Information');
         AShell.DoOutput(ASession,'----------------');
         AShell.DoOutput(ASession,'');
         AShell.DoOutput(ASession,'  Name:        ' + Device.Name);
         AShell.DoOutput(ASession,'  Description: ' + Device.Information);
         AShell.DoOutput(ASession,'');
         AShell.DoOutput(ASession,'  VendorId:    0x' + IntToHex(Device.VendorId,4));
         AShell.DoOutput(ASession,'  DeviceId:    0x' + IntToHex(Device.DeviceId,4));
         AShell.DoOutput(ASession,'');
         AShell.DoOutput(ASession,'  Manufacturer: ' + Device.Manufacturer);
         AShell.DoOutput(ASession,'  Product:      ' + Device.Product);
         AShell.DoOutput(ASession,'  SerialNumber: ' + Device.SerialNumber);
         AShell.DoOutput(ASession,'');
         AShell.DoOutput(ASession,'  Signature:   ' + IntToHex(Device.DiskSignature,8));
         AShell.DoOutput(ASession,'');
         AShell.DoOutput(ASession,'  Media Type:  ' + MediaTypeToString(Device.MediaType));
         AShell.DoOutput(ASession,'  Floppy Type: ' + FloppyTypeToString(Device.FloppyType));
         AShell.DoOutput(ASession,'');
         AShell.DoOutput(ASession,'  LBA:         ' + BooleanToString(Device.LBA));
         AShell.DoOutput(ASession,'  Ready:       ' + BooleanToString(Device.Ready));
         AShell.DoOutput(ASession,'  Locked:      ' + BooleanToString(Device.Locked));

         AShell.DoOutput(ASession,'  Lockable:    ' + BooleanToString(Device.Lockable));
         AShell.DoOutput(ASession,'  Ejectable:   ' + BooleanToString(Device.Ejectable));
         AShell.DoOutput(ASession,'  Readable:    ' + BooleanToString(Device.Readable));
         AShell.DoOutput(ASession,'  Writeable:   ' + BooleanToString(Device.Writeable));
         AShell.DoOutput(ASession,'  Eraseable:   ' + BooleanToString(Device.Eraseable));
         AShell.DoOutput(ASession,'  Removable:   ' + BooleanToString(Device.Removable));
         AShell.DoOutput(ASession,'  Change Line: ' + BooleanToString(Device.ChangeLine));
         AShell.DoOutput(ASession,'');
         AShell.DoOutput(ASession,'  Physical C:H:S - ' + IntToStr(Device.PhysicalCylinders) + ':' + IntToStr(Device.PhysicalHeads) + ':' + IntToStr(Device.PhysicalSectors));
         AShell.DoOutput(ASession,'  Logical  C:H:S - ' + IntToStr(Device.LogicalCylinders) + ':' + IntToStr(Device.LogicalHeads) + ':' + IntToStr(Device.LogicalSectors));
         AShell.DoOutput(ASession,'');
         AShell.DoOutput(ASession,'  Sector Size:  ' + IntToStr(Device.SectorSize));
         AShell.DoOutput(ASession,'  Sector Count: ' + IntToStr(Device.SectorCount));
         AShell.DoOutput(ASession,'');
         AShell.DoOutput(ASession,'  Free Sectors:      ' + IntToStr(Device.FreeSectors));
         AShell.DoOutput(ASession,'  Available Sectors: ' + IntToStr(Device.AvailableSectors));
       
         Device.ReaderUnlock;
        end
       else
        begin
         AShell.DoOutput(ASession,'Disk ' + Name + ' not found');
        end;        
      end
     else
      begin
       AShell.DoOutput(ASession,'Disk name not supplied');
      end;      
      
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
    end;
    
   {Return Result}
   Result:=True;
  end
 else if Uppercase(Parameter) = SHELL_FILESYS_ACTION_FILL then
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Get Name}
     Name:=AddLeadingSlash(StripTrailingSlash(AShell.ParameterIndex(1,AParameters)));
     if Length(Name) > 0 then
      begin
       {Get Device}
       Device:=FileSysDriver.GetDeviceByName(Name,True,FILESYS_LOCK_READ);
       if Device <> nil then
        begin
         GetMem(Buffer,Device.SectorSize);
         try
          {Open Device}
          Handle:=FileSysDriver.OpenDevice(Device.Name,fmOpenReadWrite or fmShareDenyNone);
          if Handle <> INVALID_HANDLE_VALUE then
           begin
            try
             {Get Start}
             Start:=StrToIntDef(AShell.ParameterIndex(2,AParameters),0); {Default to Sector 0}
             
             {Get Count}
             Count:=StrToIntDef(AShell.ParameterIndex(3,AParameters),1); {Default to 1 Sector}
             
             {Fill Buffer}
             FillChar(Buffer^,Device.SectorSize,$BA);
             
             {Write Sectors}
             FileSysDriver.SeekDevice(Handle,(Start * Device.SectorSize),soFromBeginning);
             while Count > 0 do
              begin
               if FileSysDriver.WriteDevice(Handle,Buffer^,Device.SectorSize) <> Device.SectorSize then
                begin
                 AShell.DoOutput(ASession,'Unable to write to sector ' + IntToStr(Start) + ' of disk ' + Name);
                 Break;
                end;
               Inc(Start);
               Dec(Count);
              end;
            finally
             FileSysDriver.CloseDevice(Handle);
            end;
           end
          else
           begin
            AShell.DoOutput(ASession,'Unable to open disk ' + Name);
           end;
         finally
          FreeMem(Buffer);
         end;
         
         Device.ReaderUnlock;
        end
       else
        begin
         AShell.DoOutput(ASession,'Disk ' + Name + ' not found');
        end;        
      end
     else
      begin
       AShell.DoOutput(ASession,'Disk name not supplied');
      end;      
    end;
  
   {Return Result}
   Result:=True;
  end
 else if Uppercase(Parameter) = SHELL_FILESYS_ACTION_SIGNATURE then
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Get Name}
     Name:=AddLeadingSlash(StripTrailingSlash(AShell.ParameterIndex(1,AParameters)));
     if Length(Name) > 0 then
      begin
       {Get Device}
       Device:=FileSysDriver.GetDeviceByName(Name,True,FILESYS_LOCK_READ);
       if Device <> nil then
        begin
         try
          {Get Signature}
          DiskSignature:=StrToIntDef('$' + AShell.ParameterIndex(2,AParameters),0);
          if DiskSignature = 0 then
           begin
            AShell.DoOutput(ASession,'Device ' + Device.Name + ' signature is ' + IntToHex(Device.DiskSignature,8));
            
            (*{Prompt Signature} //To Do
            AShell.DoOutput(ASession,'Device signature (8 hex digits, ENTER for none)? ',2,False);
            DiskSignature:=StrToDiskSignature(AShell.DoInput(ASession,'',ConsoleWhereX,True,False));
            if DiskSignature = 0 then
             begin
              {Prompt Delete}
              AShell.DoOutput(ASession,'');
              AShell.DoOutput(ASession,'Delete current signature (Y/N)? ',2,False);
              if Uppercase(AShell.DoInput(ASession,'',ConsoleWhereX,True,False)) <> 'Y' then Exit;
             end;*)
           end;
           
          {Set Signature}
          //Device.DiskSignature:=DiskSignature; //To Do
         finally 
          Device.ReaderUnlock;
         end; 
        end
       else
        begin
         AShell.DoOutput(ASession,'Disk ' + Name + ' not found');
        end;        
      end
     else
      begin
       AShell.DoOutput(ASession,'Disk name not supplied');
      end;      
    end;
  
   {Return Result}
   Result:=True;
  end
 else if Uppercase(Parameter) = SHELL_FILESYS_ACTION_INITIALIZE then
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Get Name}
     Name:=AddLeadingSlash(StripTrailingSlash(AShell.ParameterIndex(1,AParameters)));
     if Length(Name) > 0 then
      begin
       {Get Device}
       Device:=FileSysDriver.GetDeviceByName(Name,True,FILESYS_LOCK_READ);
       if Device <> nil then
        begin
         GetMem(Buffer,Device.SectorSize);
         try
          {Open Device}
          Handle:=FileSysDriver.OpenDevice(Device.Name,fmOpenReadWrite or fmShareDenyNone);
          if Handle <> INVALID_HANDLE_VALUE then
           begin
            try
             {Read Device}
             FileSysDriver.SeekDevice(Handle,0,soFromBeginning);
             if FileSysDriver.ReadDevice(Handle,Buffer^,Device.SectorSize) = Device.SectorSize then
              begin
               {Zero Sector}
               ZeroMemory(Buffer,Device.SectorSize);
               
               {Write Device}
               FileSysDriver.SeekDevice(Handle,0,soFromBeginning);
               if FileSysDriver.WriteDevice(Handle,Buffer^,Device.SectorSize) = Device.SectorSize then
                begin
                 AShell.DoOutput(ASession,'Disk ' + Name + ' successfully initialized');
                end
               else
                begin
                 AShell.DoOutput(ASession,'Unable to write to disk ' + Name);
                end;
              end
             else
              begin
               AShell.DoOutput(ASession,'Unable to read from disk ' + Name);
              end;
            finally
             FileSysDriver.CloseDevice(Handle);
            end;
           end
          else
           begin
            AShell.DoOutput(ASession,'Unable to open disk ' + Name);
           end;
         finally
          FreeMem(Buffer);
         end;
         
         Device.ReaderUnlock;
        end
       else
        begin
         AShell.DoOutput(ASession,'Disk ' + Name + ' not found');
        end;        
      end
     else
      begin
       AShell.DoOutput(ASession,'Disk name not supplied');
      end;      
    end;
  
   {Return Result}
   Result:=True;
  end
 else if Uppercase(Parameter) = SHELL_FILESYS_ACTION_EJECT then
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Get Name}
     Name:=AddLeadingSlash(StripTrailingSlash(AShell.ParameterIndex(1,AParameters)));
     if Length(Name) > 0 then
      begin
       {Get Device}
       Device:=FileSysDriver.GetDeviceByName(Name,True,FILESYS_LOCK_READ);
       if Device <> nil then
        begin
         {Check Ejectable}
         if Device.Ejectable then
          begin
           if not Device.EjectMedia then
            begin
             AShell.DoOutput(ASession,'Disk ' + Name + ' eject failed');
            end;
          end
         else
          begin
           AShell.DoOutput(ASession,'Disk ' + Name + ' is not ejectable');
          end;          
  
         Device.ReaderUnlock;
        end
       else
        begin
         AShell.DoOutput(ASession,'Disk ' + Name + ' not found');
        end;        
      end
     else
      begin
       AShell.DoOutput(ASession,'Disk name not supplied');
      end;      
    end;
  
   {Return Result}
   Result:=True;
  end
 else if Uppercase(Parameter) = SHELL_FILESYS_ACTION_LOCK then
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Get Name}
     Name:=AddLeadingSlash(StripTrailingSlash(AShell.ParameterIndex(1,AParameters)));
     if Length(Name) > 0 then
      begin
       {Get Device}
       Device:=FileSysDriver.GetDeviceByName(Name,True,FILESYS_LOCK_READ);
       if Device <> nil then
        begin
         {Check Lockable}
         if Device.Lockable then
          begin
           {Check Locked}
           if not Device.Locked then
            begin
             if not Device.LockMedia then
              begin
               AShell.DoOutput(ASession,'Disk ' + Name + ' lock failed');
              end;
            end
           else
            begin
             AShell.DoOutput(ASession,'Disk ' + Name + ' is already locked');
            end;
          end
         else
          begin
           AShell.DoOutput(ASession,'Disk ' + Name + ' is not lockable');
          end;          
  
         Device.ReaderUnlock;
        end
       else
        begin
         AShell.DoOutput(ASession,'Disk ' + Name + ' not found');
        end;        
      end
     else
      begin
       AShell.DoOutput(ASession,'Disk name not supplied');
      end;      
    end;

   {Return Result}
   Result:=True;
  end
 else if Uppercase(Parameter) = SHELL_FILESYS_ACTION_UNLOCK then
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Get Name}
     Name:=AddLeadingSlash(StripTrailingSlash(AShell.ParameterIndex(1,AParameters)));
     if Length(Name) > 0 then
      begin
       {Get Device}
       Device:=FileSysDriver.GetDeviceByName(Name,True,FILESYS_LOCK_READ);
       if Device <> nil then
        begin
         {Check Lockable}
         if Device.Lockable then
          begin
           {Check Locked}
           if Device.Locked then
            begin
             if not Device.UnlockMedia then
              begin
               AShell.DoOutput(ASession,'Disk ' + Name + ' unlock failed');
              end;
            end
           else
            begin
             AShell.DoOutput(ASession,'Disk ' + Name + ' is not locked');
            end;
          end
         else
          begin
           AShell.DoOutput(ASession,'Disk ' + Name + ' is not lockable');
          end;          
  
         Device.ReaderUnlock;
        end
       else
        begin
         AShell.DoOutput(ASession,'Disk ' + Name + ' not found');
        end;        
      end
     else
      begin
       AShell.DoOutput(ASession,'Disk name not supplied');
      end;      
    end;
  
   {Return Result}
   Result:=True;
  end
 else
  begin
   {Show Error}
   Result:=AShell.DoError(ASession);
  end;  
end;

{==============================================================================}
{==============================================================================}
{TShellFileSysPartition}
constructor TShellFileSysPartition.Create;
begin
 {}
 inherited Create;

 Name:=SHELL_FILESYS_COMMAND_PARTITION;
 Flags:=SHELL_COMMAND_FLAG_INFO or SHELL_COMMAND_FLAG_HELP;
end;

{==============================================================================}

function TShellFileSysPartition.DoHelp(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Help}
 AShell.DoOutput(ASession,'List, display or modify currently available disk partitions');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,' ' + Name + ' LIST                   (List all currently available disk partitions)');
 AShell.DoOutput(ASession,' ' + Name + ' SHOW <PARTITION>       (Display information for the specified disk partition)');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,' ' + Name + ' ADD <PARENT>           (Add a new partition of the type and size supplied)');
 AShell.DoOutput(ASession,' ' + Name + ' DELETE <PARTITION>     (Delete the specified disk partition)');
 AShell.DoOutput(ASession,' ' + Name + ' ACTIVATE <PARTITION>   (Mark the specified disk partition as active)');
 AShell.DoOutput(ASession,' ' + Name + ' DEACTIVATE <PARTITION> (Mark the specified disk partition as inactive)');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,'   Examples:');
 AShell.DoOutput(ASession,'    ' + Name + ' SHOW \Harddisk0\Partition1');
 AShell.DoOutput(ASession,'    ' + Name + ' ADD \Harddisk1 /TYPE=FAT16 /SIZE=1000000');
 AShell.DoOutput(ASession,'    ' + Name + ' ADD \Harddisk1 /TYPE=FAT32 /MAX /ACTIVE');
 AShell.DoOutput(ASession,'    ' + Name + ' DELETE \Harddisk1\Partition2');
 AShell.DoOutput(ASession,'    ' + Name + ' ACTIVATE \Harddisk0\Partition1');
 AShell.DoOutput(ASession,'    ' + Name + ' DEACTIVATE \Harddisk0\Partition2');
 AShell.DoOutput(ASession,'');
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TShellFileSysPartition.DoInfo(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Info}
 Result:=AShell.DoOutput(ASession,'List, display or modify currently available disk partitions');
end;

{==============================================================================}

function TShellFileSysPartition.DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; 
var 
 Name:String;
 Parameter:String;
 WorkBuffer:String;
 PartitionId:Byte;
 SectorCount:LongWord;
 Device:TDiskDevice;
 Partition:TDiskPartition;
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;

 {Check Parameters}
 if AParameters = nil then Exit;
 
 {Get Parameter}
 Parameter:=AShell.ParameterIndex(0,AParameters);
 
 {Check Parameter}
 if (Length(Parameter) = 0) or (Uppercase(Parameter) = SHELL_FILESYS_ACTION_LIST) then
  begin
   AShell.DoOutput(ASession,'Partition Listing');
   AShell.DoOutput(ASession,'-----------------');
   AShell.DoOutput(ASession,'');
   WorkBuffer:='';
   AShell.AddOutput(WorkBuffer,3,'Name');
   AShell.AddOutput(WorkBuffer,30,'Type');
   AShell.AddOutput(WorkBuffer,60,'Active');
   AShell.DoOutput(ASession,WorkBuffer);
   AShell.DoOutput(ASession,'');

   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Add Partitions}
     Partition:=FileSysDriver.GetPartitionByNext(nil,True,False,FILESYS_LOCK_READ);
     while Partition <> nil do
      begin
       WorkBuffer:='';
       AShell.AddOutput(WorkBuffer,3,Partition.Path);
       AShell.AddOutput(WorkBuffer,30,PartitionIdToString(Partition.PartitionId));
       AShell.AddOutput(WorkBuffer,60,BooleanToString(Partition.Active));
       AShell.DoOutput(ASession,WorkBuffer);
       
       Partition:=FileSysDriver.GetPartitionByNext(Partition,True,True,FILESYS_LOCK_READ);
      end;

     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
    end;  
  
   {Return Result}
   Result:=True;
  end
 else if Uppercase(Parameter) = SHELL_FILESYS_ACTION_SHOW then
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Get Name}
     Name:=AddLeadingSlash(StripTrailingSlash(AShell.ParameterIndex(1,AParameters)));
     if Length(Name) > 0 then
      begin
       {Get Partition}
       Partition:=FileSysDriver.GetPartitionByPath(Name,True,FILESYS_LOCK_READ);
       if Partition <> nil then
        begin
         AShell.DoOutput(ASession,'Partition Information');
         AShell.DoOutput(ASession,'---------------------');
         AShell.DoOutput(ASession,'');
         AShell.DoOutput(ASession,'  Name: ' + Partition.Name);
         AShell.DoOutput(ASession,'  Path: ' + Partition.Path);
         AShell.DoOutput(ASession,'');
         AShell.DoOutput(ASession,'  Entry No:     ' + IntToStr(Partition.EntryNo));
         AShell.DoOutput(ASession,'  Partition Id: ' + PartitionIdToString(Partition.PartitionId) + ' (Type: ' + IntToStr(Partition.PartitionId) + ')');
         AShell.DoOutput(ASession,'');
         AShell.DoOutput(ASession,'  Begin C:H:S - ' + IntToStr(Partition.BeginCylinder) + ':' + IntToStr(Partition.BeginHead) + ':' + IntToStr(Partition.BeginSector));
         AShell.DoOutput(ASession,'  End   C:H:S - ' + IntToStr(Partition.EndCylinder) + ':' + IntToStr(Partition.EndHead) + ':' + IntToStr(Partition.EndSector));
         AShell.DoOutput(ASession,'');
         AShell.DoOutput(ASession,'  Sector Offset: ' + IntToStr(Partition.SectorOffset));
         AShell.DoOutput(ASession,'  Start Sector:  ' + IntToStr(Partition.StartSector));
         AShell.DoOutput(ASession,'  Sector Count:  ' + IntToStr(Partition.SectorCount));
         AShell.DoOutput(ASession,'');
         AShell.DoOutput(ASession,'  Active:     ' + BooleanToString(Partition.Active));
         AShell.DoOutput(ASession,'  Extended:   ' + BooleanToString(Partition.Extended));
         AShell.DoOutput(ASession,'  Recognized: ' + BooleanToString(Partition.Recognized));
         AShell.DoOutput(ASession,'');
         AShell.DoOutput(ASession,'  Free Sectors:      ' + IntToStr(Partition.FreeSectors));
         AShell.DoOutput(ASession,'  Available Sectors: ' + IntToStr(Partition.AvailableSectors));
         
         Partition.ReaderUnlock;
        end
       else
        begin
         AShell.DoOutput(ASession,'Partition ' + Name + ' not found');
        end;
      end
     else
      begin
       AShell.DoOutput(ASession,'Partition name not supplied');
      end;      

     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
    end;
    
   {Return Result}
   Result:=True;
  end
 else if Uppercase(Parameter) = SHELL_FILESYS_ACTION_ADD then
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Get Name}
     Name:=AddLeadingSlash(StripTrailingSlash(AShell.ParameterIndex(1,AParameters)));
     if Length(Name) > 0 then
      begin
       {Get PartitionId}
       PartitionId:=StringToPartitionId(AShell.ParameterValue('TYPE',AParameters));
       if PartitionId <> pidUnused then
        begin
         {Get SectorCount}
         SectorCount:=StrToIntDef(AShell.ParameterValue('SIZE',AParameters),0);
         if (SectorCount > 0) or (AShell.ParameterExists('MAX',AParameters)) then
          begin
           {Get Device}
           Device:=FileSysDriver.GetDeviceByName(Name,True,FILESYS_LOCK_WRITE);
           if Device <> nil then
            begin
             if SectorCount = 0 then SectorCount:=Device.AvailableSectors;
             
             if not Device.CreatePartition(nil,PartitionId,SectorCount,AShell.ParameterExists('ACTIVE',AParameters)) then
              begin
               AShell.DoOutput(ASession,'Partition could not be created');
              end;
            
             Device.WriterUnlock;
            end
           else
            begin
             {Get Partition}
             Partition:=FileSysDriver.GetPartitionByPath(Name,True,FILESYS_LOCK_WRITE);
             if Partition <> nil then
              begin
               if SectorCount = 0 then SectorCount:=Partition.AvailableSectors;
               
               {Get Device}
               if FileSysDriver.CheckDevice(Partition.Device,True,FILESYS_LOCK_WRITE) then
                begin
                 Device:=Partition.Device;
                 
                 if not Device.CreatePartition(Partition,PartitionId,SectorCount,AShell.ParameterExists('ACTIVE',AParameters)) then
                  begin
                   AShell.DoOutput(ASession,'Partition could not be created');
                  end;
                 
                 Device.WriterUnlock;
                end; 
                
               Partition.WriterUnlock;
              end
             else
              begin
               AShell.DoOutput(ASession,'Parent ' + Name + ' not found');
              end;
            end;
          end
         else
          begin
           AShell.DoOutput(ASession,'Invalid partition size or size not supplied');
          end;
        end
       else
        begin
         AShell.DoOutput(ASession,'Invalid partition type or type not supplied');
        end;
      end
     else
      begin
       AShell.DoOutput(ASession,'Partition name not supplied');
      end;      
    end;
   
   {Return Result}
   Result:=True;
  end
 else if Uppercase(Parameter) = SHELL_FILESYS_ACTION_DELETE then
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Get Name}
     Name:=AddLeadingSlash(StripTrailingSlash(AShell.ParameterIndex(1,AParameters)));
     if Length(Name) > 0 then
      begin
       {Get Partition}
       Partition:=FileSysDriver.GetPartitionByPath(Name,True,FILESYS_LOCK_WRITE);
       if Partition <> nil then
        begin
         {Get Device}
         if FileSysDriver.CheckDevice(Partition.Device,True,FILESYS_LOCK_WRITE) then
          begin
           Device:=Partition.Device;

           if not Device.DeletePartition(Partition) then
            begin
             AShell.DoOutput(ASession,'Partition ' + Name + ' could not be deleted');
            end;
          
           Device.WriterUnlock;
          end
         else
          begin
           AShell.DoOutput(ASession,'Invalid device for partition ' + Name);
          end;
        
         {Check Partition}
         if FileSysDriver.CheckPartition(Partition,False,FILESYS_LOCK_NONE) then
          begin
           Partition.WriterUnlock;
          end; 
        end
       else
        begin
         AShell.DoOutput(ASession,'Partition ' + Name + ' not found');
        end;
      end
     else
      begin
       AShell.DoOutput(ASession,'Partition name not supplied');
      end;      
    end;
   
   {Return Result}
   Result:=True;
  end
 else if Uppercase(Parameter) = SHELL_FILESYS_ACTION_ACTIVATE then
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Get Name}
     Name:=AddLeadingSlash(StripTrailingSlash(AShell.ParameterIndex(1,AParameters)));
     if Length(Name) > 0 then
      begin
       {Get Partition}
       Partition:=FileSysDriver.GetPartitionByPath(Name,True,FILESYS_LOCK_WRITE);
       if Partition <> nil then
        begin
         {Get Device}
         if FileSysDriver.CheckDevice(Partition.Device,True,FILESYS_LOCK_READ) then
          begin
           Device:=Partition.Device;

           if not Device.ActivatePartition(Partition,True) then
            begin
             AShell.DoOutput(ASession,'Partition ' + Name + ' could not be activated');
            end;
            
           Device.ReaderUnlock;
          end
         else
          begin
           AShell.DoOutput(ASession,'Invalid device for partition ' + Name);
          end;
        
         Partition.WriterUnlock;
        end
       else
        begin
         AShell.DoOutput(ASession,'Partition ' + Name + ' not found');
        end;
      end
     else
      begin
       AShell.DoOutput(ASession,'Partition name not supplied');
      end;      
    end;
   
   {Return Result}
   Result:=True;
  end
 else if Uppercase(Parameter) = SHELL_FILESYS_ACTION_DEACTIVATE then
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Get Name}
     Name:=AddLeadingSlash(StripTrailingSlash(AShell.ParameterIndex(1,AParameters)));
     if Length(Name) > 0 then
      begin
       {Get Partition}
       Partition:=FileSysDriver.GetPartitionByPath(Name,True,FILESYS_LOCK_WRITE);
       if Partition <> nil then
        begin
         {Get Device}
         if FileSysDriver.CheckDevice(Partition.Device,True,FILESYS_LOCK_READ) then
          begin
           Device:=Partition.Device;
           
           if not Device.ActivatePartition(Partition,False) then
            begin
             AShell.DoOutput(ASession,'Partition ' + Name + ' could not be deactivated');
            end;
            
           Device.ReaderUnlock;
          end
         else
          begin
           AShell.DoOutput(ASession,'Invalid device for partition ' + Name);
          end;
        
         Partition.WriterUnlock;
        end
       else
        begin
         AShell.DoOutput(ASession,'Partition ' + Name + ' not found');
        end;
      end
     else
      begin
       AShell.DoOutput(ASession,'Partition name not supplied');
      end;      
    end;
   
   {Return Result}
   Result:=True;
  end
 else
  begin
   {Show Error}
   Result:=AShell.DoError(ASession);
  end;  
end;

{==============================================================================}
{==============================================================================}
{TShellFileSysVolume}
constructor TShellFileSysVolume.Create;
begin
 {}
 inherited Create;

 Name:=SHELL_FILESYS_COMMAND_VOLUME;
 Flags:=SHELL_COMMAND_FLAG_INFO or SHELL_COMMAND_FLAG_HELP;
end;

{==============================================================================}

function TShellFileSysVolume.DoHelp(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Help}
 AShell.DoOutput(ASession,'List, display or modify currently available disk volumes');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,' ' + Name + ' LIST              (List all currently available disk volumes)');
 AShell.DoOutput(ASession,' ' + Name + ' SHOW <VOLUME>     (Display information for the specified disk volume)');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,' ' + Name + ' LABEL <VOLUME>    (Display or modify label for the specified disk volume)');
 AShell.DoOutput(ASession,' ' + Name + ' FORMAT <VOLUME>   (Format the specified disk volume)');
 AShell.DoOutput(ASession,' ' + Name + ' DEFRAG <VOLUME>   (Defragment the specified disk volume)');
 AShell.DoOutput(ASession,' ' + Name + ' CHECK <VOLUME>    (Check and repair the specified disk volume)');
 AShell.DoOutput(ASession,' ' + Name + ' CLEAN <VOLUME>    (Mark the specified disk volume as clean on dismount)');
 AShell.DoOutput(ASession,' ' + Name + ' DIRTY <VOLUME>    (Mark the specified disk volume as dirty on dismount)');
 AShell.DoOutput(ASession,' ' + Name + ' MOUNT <VOLUME>    (Mount the disk volume as the specified drive letter)');
 AShell.DoOutput(ASession,' ' + Name + ' DISMOUNT <VOLUME> (Dismount the specified disk volume)');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,'   Examples:');
 AShell.DoOutput(ASession,'    ' + Name + ' SHOW \Volume1');
 AShell.DoOutput(ASession,'    ' + Name + ' LABEL \Volume1 TEST');
 AShell.DoOutput(ASession,'    ' + Name + ' FORMAT \Volume2 /TYPE=FAT32');
 AShell.DoOutput(ASession,'    ' + Name + ' MOUNT \Volume1 C:');
 AShell.DoOutput(ASession,'    ' + Name + ' DISMOUNT \Volume2');
 AShell.DoOutput(ASession,'');
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TShellFileSysVolume.DoInfo(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Info}
 Result:=AShell.DoOutput(ASession,'List, display or modify currently available disk volumes');
end;

{==============================================================================}

function TShellFileSysVolume.DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; 
var 
 Name:String;
 Value:String;
 Drive:String;
 DriveNo:Integer;
 Parameter:String;
 WorkBuffer:String;
 FloppyType:TFloppyType;
 FileSysType:TFileSysType;
 Device:TDiskDevice;
 Volume:TDiskVolume;
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;

 {Check Parameters}
 if AParameters = nil then Exit;
 
 {Get Parameter}
 Parameter:=AShell.ParameterIndex(0,AParameters);
 
 {Check Parameter}
 if (Length(Parameter) = 0) or (Uppercase(Parameter) = SHELL_FILESYS_ACTION_LIST) then
  begin
   AShell.DoOutput(ASession,'Volume Listing');
   AShell.DoOutput(ASession,'--------------');
   AShell.DoOutput(ASession,'');
   WorkBuffer:='';
   AShell.AddOutput(WorkBuffer,3,'Name');
   AShell.AddOutput(WorkBuffer,20,'Type');
   AShell.AddOutput(WorkBuffer,50,'Format');
   AShell.DoOutput(ASession,WorkBuffer);
   AShell.DoOutput(ASession,'');
   
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Add Volumes}
     Volume:=FileSysDriver.GetVolumeByNext(nil,True,False,FILESYS_LOCK_READ);
     while Volume <> nil do
      begin
       WorkBuffer:='';
       AShell.AddOutput(WorkBuffer,3,Volume.Name);
       AShell.AddOutput(WorkBuffer,20,DriveTypeToString(Volume.DriveType));
       AShell.AddOutput(WorkBuffer,50,FileSysTypeToString(Volume.FileSysType));
       AShell.DoOutput(ASession,WorkBuffer);
       
       Volume:=FileSysDriver.GetVolumeByNext(Volume,True,True,FILESYS_LOCK_READ);
      end;
      
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
    end;  
   
   {Return Result}
   Result:=True;
  end
 else if Uppercase(Parameter) = SHELL_FILESYS_ACTION_SHOW then
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Get Name}
     Name:=AddLeadingSlash(StripTrailingSlash(AShell.ParameterIndex(1,AParameters)));
     if Length(Name) > 0 then
      begin
       {Get Volume}
       Volume:=FileSysDriver.GetVolumeByName(Name,True,FILESYS_LOCK_READ);
       if Volume <> nil then
        begin
         AShell.DoOutput(ASession,'Volume Information');
         AShell.DoOutput(ASession,'------------------');
         AShell.DoOutput(ASession,'');
         AShell.DoOutput(ASession,' Name:   ' + Volume.Name);
         AShell.DoOutput(ASession,' Parent: ' + Volume.Parent);
         AShell.DoOutput(ASession,'');
         AShell.DoOutput(ASession,' Drive Type:  ' + DriveTypeToString(Volume.DriveType));
         AShell.DoOutput(ASession,' System Type: ' + FileSysTypeToString(Volume.FileSysType));
         AShell.DoOutput(ASession,'');
         AShell.DoOutput(ASession,' Max File Name: ' + IntToStr(Volume.MaxFile));
         AShell.DoOutput(ASession,' Max File Path: ' + IntToStr(Volume.MaxPath));
         AShell.DoOutput(ASession,' Attributes:    ' + IntToHex(Volume.Attributes,8));
         AShell.DoOutput(ASession,' System Name:   ' + Volume.SystemName);
         AShell.DoOutput(ASession,' Volume Name:   ' + Volume.VolumeName);
         AShell.DoOutput(ASession,' Volume Serial: ' + IntToHex(Volume.VolumeSerial,8));
         AShell.DoOutput(ASession,'');
         AShell.DoOutput(ASession,' Removable:  ' + BooleanToString(Volume.Removable));
         AShell.DoOutput(ASession,' Recognized: ' + BooleanToString(Volume.Recognized));
         AShell.DoOutput(ASession,'');
         AShell.DoOutput(ASession,' Sector Size:  ' + IntToStr(Volume.SectorSize));
         AShell.DoOutput(ASession,' Start Sector: ' + IntToStr(Volume.StartSector));
         AShell.DoOutput(ASession,' Sector Count: ' + IntToStr(Volume.SectorCount));
         if Volume.FileSystem <> nil then
          begin
           AShell.DoOutput(ASession,'');
           AShell.DoOutput(ASession,'  File System Information');
           AShell.DoOutput(ASession,'');
           AShell.DoOutput(ASession,'   Base Name: ' + Volume.FileSystem.RootName);
           AShell.DoOutput(ASession,'   Base Path: ' + Volume.FileSystem.RootPath);
           AShell.DoOutput(ASession,'   Drive Label:  ' + Volume.FileSystem.GetDriveLabel);
           AShell.DoOutput(ASession,'   Drive Serial: ' + IntToHex(Volume.FileSystem.GetDriveSerial,8));
           AShell.DoOutput(ASession,'   Total Space: ' + IntToStr(Volume.FileSystem.GetDriveTotalSpaceEx));
           AShell.DoOutput(ASession,'   Free Space:  ' + IntToStr(Volume.FileSystem.GetDriveFreeSpaceEx));
           AShell.DoOutput(ASession,'');
           AShell.DoOutput(ASession,'   LogFile Dirty on Mount: ' + BooleanToString(Volume.FileSystem.LogDirty));
           AShell.DoOutput(ASession,'   Volume Dirty on Mount: ' + BooleanToString(Volume.FileSystem.MountDirty));
           AShell.DoOutput(ASession,'   Mark Clean on Dismount: ' + BooleanToString(Volume.FileSystem.MarkClean));
           AShell.DoOutput(ASession,'   Mark Dirty on Dismount: ' + BooleanToString(Volume.FileSystem.MarkDirty));
          end;
          
         Volume.ReaderUnlock; 
        end
       else
        begin
         AShell.DoOutput(ASession,'Volume ' + Name + ' not found');
        end;
      end
     else
      begin
       AShell.DoOutput(ASession,'Volume name not supplied');
      end;      
      
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
    end;
   
   {Return Result}
   Result:=True;
  end
 else if Uppercase(Parameter) = SHELL_FILESYS_ACTION_LABEL then
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Get Name}
     Name:=AddLeadingSlash(StripTrailingSlash(AShell.ParameterIndex(1,AParameters)));
     if Length(Name) > 0 then
      begin
       {Get Volume}
       Volume:=FileSysDriver.GetVolumeByName(Name,True,FILESYS_LOCK_READ);
       if Volume <> nil then
        begin
         try
          {Get Label}
          Value:=AShell.ParameterIndex(2,AParameters);
          if Value = '' then
           begin
            Value:=' has no label';
            if Volume.FileSystem <> nil then
             begin
              Value:=Volume.FileSystem.GetDriveLabel;
              if Value = '' then Value:=' has no label' else Value:=' is ' + Value;
             end;
            AShell.DoOutput(ASession,'Volume ' + Volume.Name + Value);
            AShell.DoOutput(ASession,'Volume Serial Number is ' + IntToHex(Volume.VolumeSerial,8));
            
            (*{Prompt Label} //To Do
            AShell.DoOutput(ASession,'Volume label (11 characters, ENTER for none)? ',2,False);
            Value:=Uppercase(AShell.DoInput(ASession,'',ConsoleWhereX,True,False));
            if Value = '' then
             begin
              {Prompt Delete}
              AShell.DoOutput(ASession,'');
              AShell.DoOutput(ASession,'Delete current label (Y/N)? ',2,False);
              if Uppercase(AShell.DoInput(ASession,'',ConsoleWhereX,True,False)) <> 'Y' then Exit;
             end;*)
           end;
           
          {Set Label}
          //Volume.FileSystem.SetDriveLabel(Value); //To Do
         finally 
          Volume.ReaderUnlock; 
         end; 
        end
       else
        begin
         AShell.DoOutput(ASession,'Volume ' + Name + ' not found');
        end;
      end
     else
      begin
       AShell.DoOutput(ASession,'Volume name not supplied');
      end;      
      
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
    end;
   
   {Return Result}
   Result:=True;
  end
 else if Uppercase(Parameter) = SHELL_FILESYS_ACTION_FORMAT then
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Get Name}
     Name:=AddLeadingSlash(StripTrailingSlash(AShell.ParameterIndex(1,AParameters)));
     if Length(Name) > 0 then
      begin
       {Get Volume}
       Volume:=FileSysDriver.GetVolumeByName(Name,True,FILESYS_LOCK_WRITE);
       if Volume <> nil then
        begin
         {Get FileSysType}
         FileSysType:=StringToFileSysType(AShell.ParameterValue('TYPE',AParameters));
         if FileSysType <> fsUNKNOWN then
          begin
           {Get Device}
           if FileSysDriver.CheckDevice(Volume.Device,True,FILESYS_LOCK_READ) then
            begin
             Device:=Volume.Device;
             
             {Get FloppyType}
             FloppyType:=Device.FloppyType;
             if not Volume.FormatVolume(FloppyType,FileSysType) then
              begin
               AShell.DoOutput(ASession,'Volume could not be formatted');
              end;
              
             Device.ReaderUnlock; 
            end
           else
            begin
             AShell.DoOutput(ASession,'Invalid device for volume ' + Name);
            end;
          end
         else
          begin
           AShell.DoOutput(ASession,'Invalid file system type or type not supplied');
          end;
         
         Volume.WriterUnlock; 
        end
       else
        begin
         AShell.DoOutput(ASession,'Volume ' + Name + ' not found');
        end;
      end
     else
      begin
       AShell.DoOutput(ASession,'Volume name not supplied');
      end;      
      
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
    end;
   
   {Return Result}
   Result:=True;
  end
 else if Uppercase(Parameter) = SHELL_FILESYS_ACTION_DEFRAG then
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Get Name}
     Name:=AddLeadingSlash(StripTrailingSlash(AShell.ParameterIndex(1,AParameters)));
     if Length(Name) > 0 then
      begin
       {Get Volume}
       Volume:=FileSysDriver.GetVolumeByName(Name,True,FILESYS_LOCK_WRITE);
       if Volume <> nil then
        begin
         AShell.DoOutput(ASession,'Command not implemented');
         //To Do
         
         Volume.WriterUnlock; 
        end
       else
        begin
         AShell.DoOutput(ASession,'Volume ' + Name + ' not found');
        end;
      end
     else
      begin
       AShell.DoOutput(ASession,'Volume name not supplied');
      end;      
      
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
    end;
   
   {Return Result}
   Result:=True;
  end
 else if Uppercase(Parameter) = SHELL_FILESYS_ACTION_CHECK then
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Get Name}
     Name:=AddLeadingSlash(StripTrailingSlash(AShell.ParameterIndex(1,AParameters)));
     if Length(Name) > 0 then
      begin
       {Get Volume}
       Volume:=FileSysDriver.GetVolumeByName(Name,True,FILESYS_LOCK_WRITE);
       if Volume <> nil then
        begin
         AShell.DoOutput(ASession,'Command not implemented');
         //To Do
         
         Volume.WriterUnlock; 
        end
       else
        begin
         AShell.DoOutput(ASession,'Volume ' + Name + ' not found');
        end;
      end
     else
      begin
       AShell.DoOutput(ASession,'Volume name not supplied');
      end;      
      
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
    end;
   
   {Return Result}
   Result:=True;
  end
 else if Uppercase(Parameter) = SHELL_FILESYS_ACTION_CLEAN then
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Get Name}
     Name:=AddLeadingSlash(StripTrailingSlash(AShell.ParameterIndex(1,AParameters)));
     if Length(Name) > 0 then
      begin
       {Get Volume}
       Volume:=FileSysDriver.GetVolumeByName(Name,True,FILESYS_LOCK_WRITE);
       if Volume <> nil then
        begin
         if Volume.FileSystem <> nil then
          begin
           Volume.FileSystem.MarkClean:=True;
           Volume.FileSystem.MarkDirty:=False;
           AShell.DoOutput(ASession,'Volume '  + Volume.Name + ' set to mark clean on dismount');
          end;
          
         Volume.WriterUnlock; 
        end
       else
        begin
         AShell.DoOutput(ASession,'Volume ' + Name + ' not found');
        end;
      end
     else
      begin
       AShell.DoOutput(ASession,'Volume name not supplied');
      end;      
      
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
    end;
   
   {Return Result}
   Result:=True;
  end
 else if Uppercase(Parameter) = SHELL_FILESYS_ACTION_DIRTY then
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Get Name}
     Name:=AddLeadingSlash(StripTrailingSlash(AShell.ParameterIndex(1,AParameters)));
     if Length(Name) > 0 then
      begin
       {Get Volume}
       Volume:=FileSysDriver.GetVolumeByName(Name,True,FILESYS_LOCK_WRITE);
       if Volume <> nil then
        begin
         if Volume.FileSystem <> nil then
          begin
           Volume.FileSystem.MarkClean:=False;
           Volume.FileSystem.MarkDirty:=True;
           AShell.DoOutput(ASession,'Volume '  + Volume.Name + ' set to mark dirty on dismount');
          end;
          
         Volume.WriterUnlock;  
        end
       else
        begin
         AShell.DoOutput(ASession,'Volume ' + Name + ' not found');
        end;
      end
     else
      begin
       AShell.DoOutput(ASession,'Volume name not supplied');
      end;      
      
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
    end;
   
   {Return Result}
   Result:=True;
  end
 else if Uppercase(Parameter) = SHELL_FILESYS_ACTION_MOUNT then
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Get Name}
     Name:=AddLeadingSlash(StripTrailingSlash(AShell.ParameterIndex(1,AParameters)));
     if Length(Name) > 0 then
      begin
       {Get Volume}
       Volume:=FileSysDriver.GetVolumeByName(Name,True,FILESYS_LOCK_WRITE);
       if Volume <> nil then
        begin
         {Get Drive}
         Drive:=AddTrailingSlash(AShell.ParameterIndex(2,AParameters));
         DriveNo:=INVALID_DRIVE;
         if Drive <> '' then DriveNo:=FileSysDriver.GetPathDrive(Drive);
         if not Volume.MountVolume(DriveNo) then
          begin
           AShell.DoOutput(ASession,'Volume could not be mounted');
          end;
          
         Volume.WriterUnlock;  
        end
       else
        begin
         AShell.DoOutput(ASession,'Volume ' + Name + ' not found');
        end;
      end
     else
      begin
       AShell.DoOutput(ASession,'Volume name not supplied');
      end;      
      
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
    end;
   
   {Return Result}
   Result:=True;
  end
 else if Uppercase(Parameter) = SHELL_FILESYS_ACTION_DISMOUNT then
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Get Name}
     Name:=AddLeadingSlash(StripTrailingSlash(AShell.ParameterIndex(1,AParameters)));
     if Length(Name) > 0 then
      begin
       {Get Volume}
       Volume:=FileSysDriver.GetVolumeByName(Name,True,FILESYS_LOCK_WRITE);
       if Volume <> nil then
        begin
         if not Volume.DismountVolume then
          begin
           AShell.DoOutput(ASession,'Volume could not be dismounted');
          end;
          
         Volume.WriterUnlock;  
        end
       else
        begin
         AShell.DoOutput(ASession,'Volume ' + Name + ' not found');
        end;
       
      end
     else
      begin
       AShell.DoOutput(ASession,'Volume name not supplied');
      end;      
      
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
    end;
   
   {Return Result}
   Result:=True;
  end
 else
  begin
   {Show Error}
   Result:=AShell.DoError(ASession);
  end;  
end;

{==============================================================================}
{==============================================================================}
{TShellFileSysDrive}
constructor TShellFileSysDrive.Create;
begin
 {}
 inherited Create;

 Name:=SHELL_FILESYS_COMMAND_DRIVE;
 Flags:=SHELL_COMMAND_FLAG_INFO or SHELL_COMMAND_FLAG_HELP;
end;

{==============================================================================}

function TShellFileSysDrive.DoHelp(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Help}
 AShell.DoOutput(ASession,'List, display or modify currently available disk drives');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,' ' + Name + ' LIST                 (List all currently available disk drives)');
 AShell.DoOutput(ASession,' ' + Name + ' SHOW <DRIVE>         (Display information for the specified disk drive)');
 AShell.DoOutput(ASession,'');                                
 AShell.DoOutput(ASession,' ' + Name + ' LABEL <DRIVE>        (Display or modify label for the specified disk drive)');
 AShell.DoOutput(ASession,' ' + Name + ' FORMAT <DRIVE>       (Format the specified disk drive)');
 AShell.DoOutput(ASession,' ' + Name + ' CLEAN <DRIVE>        (Mark the specified disk drive as clean on dismount)');
 AShell.DoOutput(ASession,' ' + Name + ' DIRTY <DRIVE>        (Mark the specified disk drive as dirty on dismount)');
 AShell.DoOutput(ASession,' ' + Name + ' MOUNT <DRIVE> <DISK> (Mount the specified disk drive)');
 AShell.DoOutput(ASession,' ' + Name + ' DISMOUNT <DRIVE>     (Dismount the specified disk drive)');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,'   Examples:');
 AShell.DoOutput(ASession,'    ' + Name + ' SHOW C:');
 AShell.DoOutput(ASession,'    ' + Name + ' LABEL C: TEST');
 AShell.DoOutput(ASession,'    ' + Name + ' FORMAT D: /TYPE=FAT32');
 AShell.DoOutput(ASession,'    ' + Name + ' MOUNT C: \Harddisk0');
 AShell.DoOutput(ASession,'    ' + Name + ' DISMOUNT D:');
 AShell.DoOutput(ASession,'');
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TShellFileSysDrive.DoInfo(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Info}
 Result:=AShell.DoOutput(ASession,'List, display or modify currently available disk drives');
end;

{==============================================================================}

function TShellFileSysDrive.DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; 
var 
 Name:String;
 Value:String;
 Device:String;
 Parameter:String;
 WorkBuffer:String;
 FloppyType:TFloppyType;
 FileSysType:TFileSysType;
 Drive:TDiskDrive;
 Volume:TDiskVolume;
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;

 {Check Parameters}
 if AParameters = nil then Exit;
 
 {Get Parameter}
 Parameter:=AShell.ParameterIndex(0,AParameters);
 
 {Check Parameter}
 if (Length(Parameter) = 0) or (Uppercase(Parameter) = SHELL_FILESYS_ACTION_LIST) then
  begin
   AShell.DoOutput(ASession,'Drive Listing');
   AShell.DoOutput(ASession,'-------------');
   AShell.DoOutput(ASession,'');
   WorkBuffer:='';
   AShell.AddOutput(WorkBuffer,3,'Name');
   AShell.AddOutput(WorkBuffer,20,'Type');
   AShell.AddOutput(WorkBuffer,50,'Format');
   AShell.DoOutput(ASession,WorkBuffer);
   AShell.DoOutput(ASession,'');
   
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Add Drives}
     Drive:=FileSysDriver.GetDriveByNext(nil,True,False,FILESYS_LOCK_READ);
     while Drive <> nil do
      begin
       WorkBuffer:='';
       AShell.AddOutput(WorkBuffer,3,Drive.Name);
       AShell.AddOutput(WorkBuffer,20,DriveTypeToString(Drive.DriveType));
       AShell.AddOutput(WorkBuffer,50,FileSysTypeToString(Drive.FileSysType));
       AShell.DoOutput(ASession,WorkBuffer);
       
       Drive:=FileSysDriver.GetDriveByNext(Drive,True,True,FILESYS_LOCK_READ);
      end;
      
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
    end;  
   
   {Return Result}
   Result:=True;
  end
 else if Uppercase(Parameter) = SHELL_FILESYS_ACTION_SHOW then
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Get Name}
     Name:=AddTrailingSlash(AShell.ParameterIndex(1,AParameters));
     if Length(Name) > 0 then
      begin
       {Get Drive}
       Drive:=FileSysDriver.GetDriveByName(Name,True,FILESYS_LOCK_READ);
       if Drive <> nil then
        begin
         AShell.DoOutput(ASession,' Drive Information');
         AShell.DoOutput(ASession,' -----------------');
         AShell.DoOutput(ASession,'');
         AShell.DoOutput(ASession,' Name:   ' + Drive.Name);
         AShell.DoOutput(ASession,' Parent: ' + Drive.Parent);
         AShell.DoOutput(ASession,'');
         AShell.DoOutput(ASession,' Drive Type:   ' + DriveTypeToString(Drive.DriveType));
         AShell.DoOutput(ASession,' System Type:  ' + FileSysTypeToString(Drive.FileSysType));
         AShell.DoOutput(ASession,'');
         AShell.DoOutput(ASession,' Max File Name: ' + IntToStr(Drive.MaxFile));
         AShell.DoOutput(ASession,' Max File Path: ' + IntToStr(Drive.MaxPath));
         AShell.DoOutput(ASession,' Attributes:    ' + IntToHex(Drive.Attributes,8));
         AShell.DoOutput(ASession,' System Name:   ' + Drive.SystemName);
         AShell.DoOutput(ASession,' Volume Name:   ' + Drive.VolumeName);
         AShell.DoOutput(ASession,' Volume Serial: ' + IntToHex(Drive.VolumeSerial,8));
         AShell.DoOutput(ASession,'');
         AShell.DoOutput(ASession,' Removable:  ' + BooleanToString(Drive.Removable));
         AShell.DoOutput(ASession,' Recognized: ' + BooleanToString(Drive.Recognized));
         AShell.DoOutput(ASession,'');
         AShell.DoOutput(ASession,' Sector Size:  ' + IntToStr(Drive.SectorSize));
         AShell.DoOutput(ASession,' Start Sector: ' + IntToStr(Drive.StartSector));
         AShell.DoOutput(ASession,' Sector Count: ' + IntToStr(Drive.SectorCount));
         if Drive.FileSystem <> nil then
          begin
           AShell.DoOutput(ASession,'');
           AShell.DoOutput(ASession,'  File System Information');
           AShell.DoOutput(ASession,'');
           AShell.DoOutput(ASession,'   Base Name: ' + Drive.FileSystem.RootName);
           AShell.DoOutput(ASession,'   Base Path: ' + Drive.FileSystem.RootPath);
           AShell.DoOutput(ASession,'   Drive Label:  ' + Drive.FileSystem.GetDriveLabel);
           AShell.DoOutput(ASession,'   Drive Serial: ' + IntToHex(Drive.FileSystem.GetDriveSerial,8));
           AShell.DoOutput(ASession,'   Total Space: ' + IntToStr(Drive.FileSystem.GetDriveTotalSpaceEx));
           AShell.DoOutput(ASession,'   Free Space:  ' + IntToStr(Drive.FileSystem.GetDriveFreeSpaceEx));
           AShell.DoOutput(ASession,'');
           AShell.DoOutput(ASession,'   LogFile Dirty on Mount: ' + BooleanToString(Drive.FileSystem.LogDirty));
           AShell.DoOutput(ASession,'   Volume Dirty on Mount: ' + BooleanToString(Drive.FileSystem.MountDirty));
           AShell.DoOutput(ASession,'   Mark Clean on Dismount: ' + BooleanToString(Drive.FileSystem.MarkClean));
           AShell.DoOutput(ASession,'   Mark Dirty on Dismount: ' + BooleanToString(Drive.FileSystem.MarkDirty));
          end;
        
         Drive.ReaderUnlock;
        end
       else
        begin
         AShell.DoOutput(ASession,'Drive ' + Name + ' not found');
        end;
      end
     else
      begin
       AShell.DoOutput(ASession,'Drive name not supplied');
      end;      
      
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
    end;
   
   {Return Result}
   Result:=True;
  end
 else if Uppercase(Parameter) = SHELL_FILESYS_ACTION_LABEL then
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Get Name}
     Name:=AddTrailingSlash(AShell.ParameterIndex(1,AParameters));
     if Length(Name) > 0 then
      begin
       {Get Drive}
       Drive:=FileSysDriver.GetDriveByName(Name,True,FILESYS_LOCK_READ);
       if Drive <> nil then
        begin
         try
          {Get Label}
          Value:=AShell.ParameterIndex(2,AParameters);
          if Value = '' then
           begin
            Value:=' has no label';
            if Drive.FileSystem <> nil then
             begin
              Value:=Drive.FileSystem.GetDriveLabel;
              if Value = '' then Value:=' has no label' else Value:=' is ' + Value;
             end;
            AShell.DoOutput(ASession,'Volume in drive ' + Drive.Name + Value);
            AShell.DoOutput(ASession,'Volume Serial Number is ' + IntToHex(Drive.VolumeSerial,8));
            
            (*{Prompt Label} //To Do
            AShell.DoOutput(ASession,'Volume label (11 characters, ENTER for none)? ',2,False);
            Value:=Uppercase(AShell.DoInput(ASession,'',ConsoleWhereX,True,False));
            if Value = '' then
             begin
              {Prompt Delete}
              AShell.DoOutput(ASession,'');
              AShell.DoOutput(ASession,'Delete current label (Y/N)? ',2,False);
              if Uppercase(AShell.DoInput(ASession,'',ConsoleWhereX,True,False)) <> 'Y' then Exit;
             end;*)
           end;
          
          {Set Label}
          //Drive.FileSystem.SetDriveLabel(Value); //To Do
         finally 
          Drive.ReaderUnlock;
         end; 
        end
       else
        begin
         AShell.DoOutput(ASession,'Drive ' + Name + ' not found');
        end;
      end
     else
      begin
       AShell.DoOutput(ASession,'Drive name not supplied');
      end;      
      
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
    end;
   
   {Return Result}
   Result:=True;
  end
 else if Uppercase(Parameter) = SHELL_FILESYS_ACTION_FORMAT then
  begin
   if FileSysDriver <> nil then
    begin
     {Get Name}
     Name:=AddTrailingSlash(AShell.ParameterIndex(1,AParameters));
     if Length(Name) > 0 then
      begin
       {Get Drive}
       Drive:=FileSysDriver.GetDriveByName(Name,True,FILESYS_LOCK_READ);
       if Drive <> nil then
        begin
         Volume:=Drive.Volume;
         
         Drive.ReaderUnlock; 
        end
       else
        begin
         Volume:=nil;
         
         AShell.DoOutput(ASession,'Drive ' + Name + ' not found');
        end;
       
       {Get Volume}
       if FileSysDriver.CheckVolume(Volume,True,FILESYS_LOCK_WRITE) then
        begin
         {Get FileSysType}
         FileSysType:=StringToFileSysType(AShell.ParameterValue('TYPE',AParameters));
         if FileSysType <> fsUNKNOWN then
          begin
           {Get Device}
           if FileSysDriver.CheckDevice(Volume.Device,True,FILESYS_LOCK_READ) then
            begin
             {Get FloppyType}
             FloppyType:=Volume.Device.FloppyType;
             if not Volume.FormatVolume(FloppyType,FileSysType) then
              begin
               AShell.DoOutput(ASession,'Drive could not be formatted');
              end;
              
             Volume.Device.ReaderUnlock; 
            end
           else
            begin
             AShell.DoOutput(ASession,'Invalid disk for drive ' + Name);
            end;
          end
         else
          begin
           AShell.DoOutput(ASession,'Invalid file system type or type not supplied');
          end;
          
         Volume.WriterUnlock; 
        end
       else
        begin
         AShell.DoOutput(ASession,'Invalid volume for drive ' + Name);
        end;        
      end
     else
      begin
       AShell.DoOutput(ASession,'Drive name not supplied');
      end;      
      
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
    end;
   
   {Return Result}
   Result:=True;
  end
 else if Uppercase(Parameter) = SHELL_FILESYS_ACTION_CLEAN then
  begin
   if FileSysDriver <> nil then
    begin
     {Get Name}
     Name:=AddTrailingSlash(AShell.ParameterIndex(1,AParameters));
     if Length(Name) > 0 then
      begin
       {Get Drive}
       Drive:=FileSysDriver.GetDriveByName(Name,True,FILESYS_LOCK_WRITE);
       if Drive <> nil then
        begin
         if Drive.FileSystem <> nil then
          begin
           Drive.FileSystem.MarkClean:=True;
           Drive.FileSystem.MarkDirty:=False;
           AShell.DoOutput(ASession,'Drive '  + Drive.Name + ' set to mark clean on dismount');
          end;
          
         Drive.WriterUnlock; 
        end
       else
        begin
         AShell.DoOutput(ASession,'Drive ' + Name + ' not found');
        end;
      end
     else
      begin
       AShell.DoOutput(ASession,'Drive name not supplied');
      end;      
      
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
    end;
   
   {Return Result}
   Result:=True;
  end
 else if Uppercase(Parameter) = SHELL_FILESYS_ACTION_DIRTY then
  begin
   if FileSysDriver <> nil then
    begin
     {Get Name}
     Name:=AddTrailingSlash(AShell.ParameterIndex(1,AParameters));
     if Length(Name) > 0 then
      begin
       {Get Drive}
       Drive:=FileSysDriver.GetDriveByName(Name,True,FILESYS_LOCK_WRITE);
       if Drive <> nil then
        begin
         if Drive.FileSystem <> nil then
          begin
           Drive.FileSystem.MarkClean:=False;
           Drive.FileSystem.MarkDirty:=True;
           AShell.DoOutput(ASession,'Drive '  + Drive.Name + ' set to mark dirty on dismount');
          end;
         
         Drive.WriterUnlock; 
        end
       else
        begin
         AShell.DoOutput(ASession,'Drive ' + Name + ' not found');
        end;
      end
     else
      begin
       AShell.DoOutput(ASession,'Drive name not supplied');
      end;      
      
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
    end;
   
   {Return Result}
   Result:=True;
  end
 else if Uppercase(Parameter) = SHELL_FILESYS_ACTION_MOUNT then
  begin
   if FileSysDriver <> nil then
    begin
     {Get Name}
     Name:=AddTrailingSlash(AShell.ParameterIndex(1,AParameters));
     if Length(Name) > 0 then
      begin
       {Get Device}
       Device:=AddLeadingSlash(StripTrailingSlash(AShell.ParameterIndex(2,AParameters)));
       if Trim(Device) <> '' then
        begin
         {Get Volume}
         Volume:=FileSysDriver.GetVolumeByParent(Device,True,FILESYS_LOCK_WRITE);
         if Volume <> nil then
          begin
           if not FileSysDriver.CreateDrive(Name,Device) then
            begin
             AShell.DoOutput(ASession,'Drive ' + Name + ' could not be mounted on disk ' + Device);
            end;
           
           Volume.WriterUnlock; 
          end
         else
          begin
           AShell.DoOutput(ASession,'Disk ' + Device + ' not found');
          end;
        end
       else
        begin
         AShell.DoOutput(ASession,'Disk name not supplied');
        end;
      end
     else
      begin
       AShell.DoOutput(ASession,'Drive name not supplied');
      end;      
      
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
    end;
   
   {Return Result}
   Result:=True;
  end
 else if Uppercase(Parameter) = SHELL_FILESYS_ACTION_DISMOUNT then
  begin
   if FileSysDriver <> nil then
    begin
     {Get Name}
     Name:=AddTrailingSlash(AShell.ParameterIndex(1,AParameters));
     if Length(Name) > 0 then
      begin
       {Get Drive}
       Drive:=FileSysDriver.GetDriveByName(Name,True,FILESYS_LOCK_WRITE);
       if Drive <> nil then
        begin
         if not FileSysDriver.DeleteDrive(Drive.Name) then
          begin
           AShell.DoOutput(ASession,'Drive ' + Name + ' could not be dismounted');
           
           Drive.WriterUnlock; 
          end;
        end
       else
        begin
         AShell.DoOutput(ASession,'Drive ' + Name + ' not found');
        end;
      end
     else
      begin
       AShell.DoOutput(ASession,'Drive name not supplied');
      end;      
      
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
    end;
   
   {Return Result}
   Result:=True;
  end
 else
  begin
   {Show Error}
   Result:=AShell.DoError(ASession);
  end;  
end;

{==============================================================================}
{==============================================================================}
{TShellFileSysCache}
constructor TShellFileSysCache.Create;
begin
 {}
 inherited Create;

 Name:=SHELL_FILESYS_COMMAND_CACHE;
 Flags:=SHELL_COMMAND_FLAG_INFO or SHELL_COMMAND_FLAG_HELP;
end;

{==============================================================================}

function TShellFileSysCache.DoHelp(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Help}
 AShell.DoOutput(ASession,'Display disk cache information');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,' ' + Name + ' STATS   (Display disk cache statistics)');
 AShell.DoOutput(ASession,' ' + Name + ' FLUSH   (Flush contents of disk cache)');
 AShell.DoOutput(ASession,' ' + Name + ' DISCARD (Discard contents of disk cache)');
 AShell.DoOutput(ASession,'');
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TShellFileSysCache.DoInfo(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Info}
 Result:=AShell.DoOutput(ASession,'Display disk cache information');
end;

{==============================================================================}

function TShellFileSysCache.DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; 
var 
 Parameter:String;
 WorkTime:TDateTime;
 Statistics:TCacheStatistics;
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;

 {Check Parameters}
 if AParameters = nil then Exit;
 
 {Get Parameter}
 Parameter:=AShell.ParameterIndex(0,AParameters);
 
 {Check Parameter}
 if (Length(Parameter) = 0) or (Uppercase(Parameter) = SHELL_FILESYS_ACTION_STATS) then
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     if FileSysDriver.GetCacheStatistics(Statistics) then
      begin
       AShell.DoOutput(ASession,'Cache Statistics');
       AShell.DoOutput(ASession,'----------------');
       AShell.DoOutput(ASession,'');
       AShell.DoOutput(ASession,'  Page Size:       ' + IntToStr(Statistics.PageSize));
       AShell.DoOutput(ASession,'  Page Count:      ' + IntToStr(Statistics.PageCount));
       AShell.DoOutput(ASession,'  Cache Size:      ' + IntToStr(Statistics.CacheSize));
       AShell.DoOutput(ASession,'  Cache Mode:      ' + CacheModeToString(Statistics.CacheMode));
       AShell.DoOutput(ASession,'  Cache State:     ' + CacheStateToString(Statistics.CacheState));
       AShell.DoOutput(ASession,'  Flush Timeout:   ' + IntToStr(Statistics.FlushTimeout));
       AShell.DoOutput(ASession,'  Discard Timeout: ' + IntToStr(Statistics.DiscardTimeout));
       AShell.DoOutput(ASession,'');
       AShell.DoOutput(ASession,'  Pages Read Cached:     ' + IntToStr(Statistics.ReadCached));
       AShell.DoOutput(ASession,'  Pages Written Back:    ' + IntToStr(Statistics.WriteBack));
       AShell.DoOutput(ASession,'  Pages Written Through: ' + IntToStr(Statistics.WriteThrough));
       AShell.DoOutput(ASession,'');
       AShell.DoOutput(ASession,'  Direct Reads:  ' + IntToStr(Statistics.ReadDirect));
       AShell.DoOutput(ASession,'  Direct Writes: ' + IntToStr(Statistics.WriteDirect));
       AShell.DoOutput(ASession,'');
       AShell.DoOutput(ASession,'  Page Hits:   ' + IntToStr(Statistics.HitCount));
       AShell.DoOutput(ASession,'  Page Misses: ' + IntToStr(Statistics.MissCount));
       AShell.DoOutput(ASession,'');
       AShell.DoOutput(ASession,'  Allocate Failures:  ' + IntToStr(Statistics.FailCount));
       AShell.DoOutput(ASession,'  Allocate Successes: ' + IntToStr(Statistics.SuccessCount));
       AShell.DoOutput(ASession,'');
       AShell.DoOutput(ASession,'  Pages Flushed:        ' + IntToStr(Statistics.FlushCount));
       AShell.DoOutput(ASession,'  Pages Discarded:      ' + IntToStr(Statistics.DiscardCount));
       AShell.DoOutput(ASession,'  Pages Marked Unknown: ' + IntToStr(Statistics.UnknownCount));
       AShell.DoOutput(ASession,'');
       WorkTime:=CachePageTimeToDateTime(Statistics.OldestClean);
       AShell.DoOutput(ASession,'  Oldest Clean Page: ' + IntToStr(Trunc(WorkTime)) + ' days ' + TimeToStr(WorkTime));
       WorkTime:=CachePageTimeToDateTime(Statistics.NewestClean);
       AShell.DoOutput(ASession,'  Newest Clean Page: ' + IntToStr(Trunc(WorkTime)) + ' days ' + TimeToStr(WorkTime));
       WorkTime:=CachePageTimeToDateTime(Statistics.OldestDirty);
       AShell.DoOutput(ASession,'  Oldest Dirty Page: ' + IntToStr(Trunc(WorkTime)) + ' days ' + TimeToStr(WorkTime));
       WorkTime:=CachePageTimeToDateTime(Statistics.NewestDirty);
       AShell.DoOutput(ASession,'  Newest Dirty Page: ' + IntToStr(Trunc(WorkTime)) + ' days ' + TimeToStr(WorkTime));
      end 
     else
      begin
       AShell.DoOutput(ASession,'Get cache statistics failed');
      end;      
      
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
    end;
    
   {Return Result}
   Result:=True;
  end
 else if Uppercase(Parameter) = SHELL_FILESYS_ACTION_FLUSH then
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     if FileSysDriver.FlushCache(True) then
      begin
       AShell.DoOutput(ASession,'Cache flushed sucessfully');
      end
     else
      begin
       AShell.DoOutput(ASession,'Cache flush failed');
      end;
      
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
    end;
   
   {Return Result}
   Result:=True;
  end
 else if Uppercase(Parameter) = SHELL_FILESYS_ACTION_DISCARD then
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     if FileSysDriver.DiscardCache(True) then
      begin
       AShell.DoOutput(ASession,'Cache discarded sucessfully');
      end
     else
      begin
       AShell.DoOutput(ASession,'Cache discard failed');
      end;
      
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
    end;
   
   {Return Result}
   Result:=True;
  end
 else
  begin
   {Show Error}
   Result:=AShell.DoError(ASession);
  end;  
end;

{==============================================================================}
{==============================================================================}
{TShellFileSysDir}
constructor TShellFileSysDir.Create;
var
 Alias:TShellAlias;
begin
 {}
 inherited Create;

 Name:=SHELL_FILESYS_COMMAND_DIR;
 Flags:=SHELL_COMMAND_FLAG_INFO or SHELL_COMMAND_FLAG_HELP;
 
 {Create Alias (LS)}
 Alias:=TShellAlias.Create;
 Alias.Name:=SHELL_FILESYS_ALIAS_LS;
 
 {Register Alias}
 if not RegisterAlias(Alias) then
  begin
   {Destroy Alias}
   Alias.Free;
  end;
end;

{==============================================================================}
 
destructor TShellFileSysDir.Destroy; 
var
 Alias:TShellAlias;
begin
 {}
 {Get Alias (DELETE)}
 Alias:=FindAlias(SHELL_FILESYS_ALIAS_LS);
 
 {Check Alias}
 if Alias <> nil then
  begin
   {Degister Alias}
   DeregisterAlias(Alias);
   
   {Destroy Alias}
   Alias.Free;
  end;
  
 inherited Destroy;
end;
  
{==============================================================================}

function TShellFileSysDir.DirSearch(AShell:TShell;ASession:TShellSession;const APath,AName:String;AShort,ASubdir:Boolean;var AFiles,AFolders,ABytes:Int64):Boolean;
var
 Code:Integer;
 WorkBuffer:String;
 FileSize:Int64;
 FileCount:Integer;
 FolderCount:Integer;
 SearchRec:TFileSearchRec;
begin
 {}
 Result:=False;
 
 FileSize:=0;
 FileCount:=0;
 FolderCount:=0;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Check Session}
 if ASession = nil then Exit;
 
 {Check Driver}
 if FileSysDriver = nil then Exit;
 
 {Display Header}
 if not ASubdir then
  begin
   AShell.DoOutput(ASession,'  Directory of ' + APath);
   AShell.DoOutput(ASession,'');
  end;
 
 {Find First}
 Code:=FileSysDriver.FindFirstEx(APath + AName,SearchRec);
 while Code = 0 do
  begin
   {Display Header}
   if (ASubdir) and (FileCount = 0) and (FolderCount = 0) then
    begin
     AShell.DoOutput(ASession,'  Directory of ' + APath);
     AShell.DoOutput(ASession,'');
    end;
    
   {Display Output}
   DirOutput(AShell,ASession,SearchRec,AShort);
   
   {Check Folder}
   if (SearchRec.FindData.dwFileAttributes and faDirectory) = faNone then
    begin
     Inc(FileSize,SearchRec.FindData.nFileSizeLow);
     Inc(ABytes,SearchRec.FindData.nFileSizeLow);
     Inc(FileCount);
     Inc(AFiles);
    end
   else
    begin
     Inc(FolderCount);
     Inc(AFolders);
    end;
    
   {Find Next}
   Code:=FileSysDriver.FindNextEx(SearchRec);
  end;
  
 {Find Close}
 FileSysDriver.FindCloseEx(SearchRec);
 
 {Check Subdir}
 if ASubdir then
  begin
   {Display Footer}
   if (FileCount <> 0) or (FolderCount <> 0) then
    begin
     WorkBuffer:='';
     AShell.AddOutput(WorkBuffer,10,IntToStr(FileCount) + ' file(s) ' + IntToStr(FileSize) + ' bytes');
     AShell.DoOutput(ASession,WorkBuffer);
     AShell.DoOutput(ASession,'');
    end;
    
   {Find First}
   Code:=FileSysDriver.FindFirstEx(APath + SHELL_FILESYS_ALLFILES_MASK,SearchRec);
   while Code = 0 do
    begin
     if (SearchRec.FindData.dwFileAttributes and faDirectory) = faDirectory then
      begin
       if (Trim(SearchRec.FindData.cFileName) <> '.') and (Trim(SearchRec.FindData.cFileName) <> '..') then
        begin
         if not DirSearch(AShell,ASession,AddTrailingSlash(APath + SearchRec.FindData.cFileName),AName,AShort,ASubdir,AFiles,AFolders,ABytes) then Break;
        end;
      end;
      
     {Find Next}
     Code:=FileSysDriver.FindNextEx(SearchRec);
    end;
    
   {Find Close}
   FileSysDriver.FindCloseEx(SearchRec);
  end
 else
  begin
   {Display Footer}
   WorkBuffer:='';
   AShell.AddOutput(WorkBuffer,10,IntToStr(FileCount) + ' file(s) ' + IntToStr(FileSize) + ' bytes');
   AShell.DoOutput(ASession,WorkBuffer);
   
   WorkBuffer:='';
   AShell.AddOutput(WorkBuffer,10,IntToStr(FolderCount) + ' dir(s)');
   AShell.DoOutput(ASession,WorkBuffer);
  end;
  
 Result:=True;
end;

{==============================================================================}

function TShellFileSysDir.DirOutput(AShell:TShell;ASession:TShellSession;const ASearchRec:TFileSearchRec;AShort:Boolean):Boolean;
var
 Name:String;
 AltName:String;
 WorkBuffer:String;
begin
 {}
 Result:=False;
 
 WorkBuffer:='';
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Check Session}
 if ASession = nil then Exit;

 {Get Names} 
 Name:=ASearchRec.FindData.cFileName;
 AltName:=ASearchRec.FindData.cAlternateFileName;
 
 {Display Date and Time}
 AShell.AddOutput(WorkBuffer,1,DateTimeToStr(FileTimeToDateTime(ASearchRec.FindData.ftLastWriteTime)));
 
 {Display Size}
 if (ASearchRec.FindData.dwFileAttributes and faDirectory) = faNone then
  begin
   AShell.AddOutput(WorkBuffer,37 - Length(IntToStr(ASearchRec.FindData.nFileSizeLow)),IntToStr(ASearchRec.FindData.nFileSizeLow));
  end
 else
  begin
   AShell.AddOutput(WorkBuffer,24,'<DIR>');
  end;
  
 {Check Short}
 if AShort then
  begin
   {Display AltName}
   AShell.AddOutput(WorkBuffer,39,Uppercase(AltName));
   
   {Display Name}
   AShell.AddOutput(WorkBuffer,55,Name);
  end
 else
  begin
   {Display Name}
   AShell.AddOutput(WorkBuffer,39,Name);
  end;
 
 {Write Output}
 AShell.DoOutput(ASession,WorkBuffer);
 
 Result:=True;
end;

{==============================================================================}

function TShellFileSysDir.DoHelp(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Help}
 AShell.DoOutput(ASession,'Displays a list of files and subdirectories in a directory');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,' ' + Name + ' [/S] [/X]');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,'  /S            Lists files and folders in all subfolders');
 AShell.DoOutput(ASession,'  /X            Displays the alternate file name for each file');
 AShell.DoOutput(ASession,'');
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TShellFileSysDir.DoInfo(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Info}
 Result:=AShell.DoOutput(ASession,'Displays a list of files and subdirectories in a directory');
end;

{==============================================================================}

function TShellFileSysDir.DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; 
var
 Name:String;
 Path:String;
 WorkBuffer:String;
 
 TotalBytes:Int64;
 TotalFiles:Int64;
 TotalFolders:Int64;

 Code:Integer;
 Short:Boolean;
 Subdir:Boolean;
 SearchRec:TFileSearchRec;
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;

 {Check Parameters}
 if AParameters = nil then Exit;
 
 {Check Help}
 if AShell.ParameterExists(SHELL_FILESYS_PARAMETER_HELP,AParameters) then
  begin
   {Show Help}
   Result:=DoHelp(AShell,ASession);
  end
 else
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Get Name}
     Name:=AShell.ParameterIndex(0,AParameters);
     if Name = '' then Name:=SHELL_FILESYS_ALLFILES_MASK;
     if (Name[1] = '/') or (Name[1] = '-') then Name:=SHELL_FILESYS_ALLFILES_MASK;
     
     {Setup Path}
     Path:=AddTrailingSlash(ExtractFilePath(Name));
     if Path = '' then Path:=AddTrailingSlash(FileSysDriver.GetCurrentDir);
     
     {Check Folder}
     Name:=ExtractFileName(Name);
     if FileSysDriver.DirectoryExists(Path + Name) then
      begin
       Path:=AddTrailingSlash(Path + Name);
       Name:=SHELL_FILESYS_ALLFILES_MASK;
      end;
 
     {Setup Paging}
     //CommandReset(AShell.ParameterExists(SHELL_FILESYS_PARAMETER_PAGE,AParameters)); //To Do
     
     {Get Options}
     Short:=AShell.ParameterExists(SHELL_FILESYS_PARAMETER_SHORT,AParameters);
     Subdir:=AShell.ParameterExists(SHELL_FILESYS_PARAMETER_SUBDIR,AParameters);
     TotalBytes:=0;
     TotalFiles:=0;
     TotalFolders:=0;
     
     {Perform Search}
     DirSearch(AShell,ASession,Path,Name,Short,Subdir,TotalFiles,TotalFolders,TotalBytes);
     
     {Display Totals}
     if Subdir then
      begin
       WorkBuffer:='';
       AShell.AddOutput(WorkBuffer,10,'Total Files Listed:');
       AShell.DoOutput(ASession,WorkBuffer);
       
       WorkBuffer:='';
       AShell.AddOutput(WorkBuffer,10,IntToStr(TotalFiles) + ' file(s) ' + IntToStr(TotalBytes) + ' bytes');
       AShell.DoOutput(ASession,WorkBuffer);
       
       WorkBuffer:='';
       AShell.AddOutput(WorkBuffer,10,IntToStr(TotalFolders) + ' dir(s)');
       AShell.DoOutput(ASession,WorkBuffer);
      end;
     AShell.DoOutput(ASession,'');
 
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
 
     {Return Result}
     Result:=True;
    end;
  end; 
end;

{==============================================================================}
{==============================================================================}
{TShellFileSysChdir}
constructor TShellFileSysChdir.Create;
var
 Alias:TShellAlias;
begin
 {}
 inherited Create;

 Name:=SHELL_FILESYS_COMMAND_CHDIR;
 Flags:=SHELL_COMMAND_FLAG_INFO or SHELL_COMMAND_FLAG_HELP or SHELL_COMMAND_FLAG_DEFAULT or SHELL_COMMAND_FLAG_EXTENDED;
 
 {Create Alias (CD)}
 Alias:=TShellAlias.Create;
 Alias.Name:=SHELL_FILESYS_ALIAS_CD;
 
 {Register Alias}
 if not RegisterAlias(Alias) then
  begin
   {Destroy Alias}
   Alias.Free;
  end;
  
 {Create Alias (CD\)}
 Alias:=TShellAlias.Create;
 Alias.Name:=SHELL_FILESYS_ALIAS_CDSLASH;
 Alias.Flags:=SHELL_ALIAS_FLAG_HIDDEN;
 
 {Register Alias}
 if not RegisterAlias(Alias) then
  begin
   {Destroy Alias}
   Alias.Free;
  end;
  
 {Create Alias (CD..)}
 Alias:=TShellAlias.Create;
 Alias.Name:=SHELL_FILESYS_ALIAS_CDDOTDOT;
 Alias.Flags:=SHELL_ALIAS_FLAG_HIDDEN;
 
 {Register Alias}
 if not RegisterAlias(Alias) then
  begin
   {Destroy Alias}
   Alias.Free;
  end;
end;

{==============================================================================}
 
destructor TShellFileSysChdir.Destroy; 
var
 Alias:TShellAlias;
begin
 {}
 {Get Alias (CD)}
 Alias:=FindAlias(SHELL_FILESYS_ALIAS_CD);
 
 {Check Alias}
 if Alias <> nil then
  begin
   {Degister Alias}
   DeregisterAlias(Alias);
   
   {Destroy Alias}
   Alias.Free;
  end;

 {Get Alias (CD\)}
 Alias:=FindAlias(SHELL_FILESYS_ALIAS_CDSLASH);
 
 {Check Alias}
 if Alias <> nil then
  begin
   {Degister Alias}
   DeregisterAlias(Alias);
   
   {Destroy Alias}
   Alias.Free;
  end;

 {Get Alias (CD..)}
 Alias:=FindAlias(SHELL_FILESYS_ALIAS_CDDOTDOT);
 
 {Check Alias}
 if Alias <> nil then
  begin
   {Degister Alias}
   DeregisterAlias(Alias);
   
   {Destroy Alias}
   Alias.Free;
  end;
  
 inherited Destroy;
end;

{==============================================================================}

function TShellFileSysChdir.DoHelp(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Help}
 AShell.DoOutput(ASession,'Displays the name of or changes the current directory');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,' ' + Name + ' [drive:][path]');
 AShell.DoOutput(ASession,' ' + Name + ' [..]');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,' ..   Specifies that you want to change to the parent directory');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,' Type ' + Name + ' drive: to display the current directory in the specified drive');
 AShell.DoOutput(ASession,' Type ' + Name + ' without parameters to display the current drive and directory');
 AShell.DoOutput(ASession,'');
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TShellFileSysChdir.DoInfo(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Info}
 Result:=AShell.DoOutput(ASession,'Displays the name of or changes the current directory');
end;

{==============================================================================}

function TShellFileSysChdir.DoDefault(AShell:TShell;ASession:TShellSession;const AName:String;AParameters:TStrings):Boolean; 
var
 Name:String;
 Drive:TDiskDrive;
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;

 {Check Parameters}
 if AParameters = nil then Exit;
 
 {Check Driver}
 if FileSysDriver <> nil then
  begin
   {Get Name}
   Name:=AddTrailingSlash(AName);
   if Trim(Name) <> '' then
    begin
     {Get Drive}
     Drive:=FileSysDriver.GetDriveByName(Name,True,FILESYS_LOCK_READ);
     if Drive <> nil then
      begin
       {Set Current}
       if FileSysDriver.SetCurrentDrive(Drive.Name) then
        begin
         {Update Prompt}
         ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
         
         {Return Result}
         Result:=True;
        end;
      
       Drive.ReaderUnlock;
      end;
    end;
  end;
end;

{==============================================================================}

function TShellFileSysChdir.DoExtended(AShell:TShell;ASession:TShellSession;const AName:String;AParameters:TStrings):Boolean; 
var
 Name:String;
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;

 {Check Parameters}
 if AParameters = nil then Exit;
 
 {Check Name}
 if Uppercase(AName) = SHELL_FILESYS_ALIAS_CDSLASH then
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Get Name}
     Name:=DRIVE_NAMES[FileSysDriver.GetCurrentDrive];

     {Set Current}
     FileSysDriver.SetCurrentDir(Name);
       
     AShell.DoOutput(ASession,'');
  
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
      
     {Return Result}
     Result:=True;
    end;
  end
 else if Uppercase(AName) = SHELL_FILESYS_ALIAS_CDDOTDOT then 
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Get Name}
     Name:='..';

     {Set Current}
     FileSysDriver.SetCurrentDir(Name);
       
     AShell.DoOutput(ASession,'');
  
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
      
     {Return Result}
     Result:=True;
    end;
  end
 else
  begin 
   {Check Help}
   if AShell.ParameterExists(SHELL_FILESYS_PARAMETER_HELP,AParameters) then
    begin
     {Show Help}
     Result:=DoHelp(AShell,ASession);
    end
   else
    begin
     {Check Driver}
     if FileSysDriver <> nil then
      begin
       {Get Name}
       Name:=AShell.ParameterIndex(0,AParameters);
       if (Name <> '') and (Name[1] <> '/') and (Name[1] <> '-') then
        begin
         {Check Name}
         if Name = '\' then Name:=DRIVE_NAMES[FileSysDriver.GetCurrentDrive];
         
         {Set Current}
         FileSysDriver.SetCurrentDir(Name);
        end
       else
        begin
         {Get Current}
         AShell.DoOutput(ASession,FileSysDriver.GetCurrentDir);
        end;
       AShell.DoOutput(ASession,'');
  
       {Update Prompt}
       ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
       
       {Return Result}
       Result:=True;
      end;
    end; 
  end;
end;

{==============================================================================}
{==============================================================================}
{TShellFileSysMkdir}
constructor TShellFileSysMkdir.Create;
var
 Alias:TShellAlias;
begin
 {}
 inherited Create;

 Name:=SHELL_FILESYS_COMMAND_MKDIR;
 Flags:=SHELL_COMMAND_FLAG_INFO or SHELL_COMMAND_FLAG_HELP;
 
 {Create Alias}
 Alias:=TShellAlias.Create;
 Alias.Name:=SHELL_FILESYS_ALIAS_MD;
 
 {Register Alias}
 if not RegisterAlias(Alias) then
  begin
   {Destroy Alias}
   Alias.Free;
  end;
end;

{==============================================================================}
 
destructor TShellFileSysMkdir.Destroy; 
var
 Alias:TShellAlias;
begin
 {}
 {Get Alias}
 Alias:=FindAlias(SHELL_FILESYS_ALIAS_MD);
 
 {Check Alias}
 if Alias <> nil then
  begin
   {Degister Alias}
   DeregisterAlias(Alias);
   
   {Destroy Alias}
   Alias.Free;
  end;
  
 inherited Destroy;
end;

{==============================================================================}

function TShellFileSysMkdir.DoHelp(AShell:TShell;ASession:TShellSession):Boolean;
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Help}
 AShell.DoOutput(ASession,'Creates a directory');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,' ' + Name + ' [drive:]path [alternatename]');
 AShell.DoOutput(ASession,'');
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TShellFileSysMkdir.DoInfo(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Info}
 Result:=AShell.DoOutput(ASession,'Creates a directory');
end;

{==============================================================================}

function TShellFileSysMkdir.DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; 
var
 Name:String;
 Path:String;
 ShortName:String;
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;

 {Check Parameters}
 if AParameters = nil then Exit;
 
 {Check Help}
 if AShell.ParameterExists(SHELL_FILESYS_PARAMETER_HELP,AParameters) then
  begin
   {Show Help}
   Result:=DoHelp(AShell,ASession);
  end
 else
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Get Name}
     Name:=AShell.ParameterIndex(0,AParameters);
     if (Name <> '') and (Name[1] <> '/') and (Name[1] <> '-') then
      begin
       {Get Path}
       Path:=AddTrailingSlash(ExtractFilePath(Name));
       if Path = '' then Path:=AddTrailingSlash(FileSysDriver.GetCurrentDir);
       
       {Get Name}
       Name:=ExtractFileName(Name);
       
       {Get Short Name}
       ShortName:=AShell.ParameterIndex(1,AParameters);
       if (ShortName = '') or (ShortName[1] = '/') or (ShortName[1] = '-') then ShortName:='';
    
       {Check Existing}
       if not FileSysDriver.DirectoryExists(Path + Name) then
        begin
         if Length(ShortName) = 0 then
          begin
           {Create Directory}
           if not FileSysDriver.CreateDir(Path + Name) then
            begin
             AShell.DoOutput(ASession,'Unable to create directory');
            end;
          end
         else
          begin
           if FileSysDriver <> nil then
            begin
             if not FileSysDriver.CreateDirEx(Path + Name,ShortName) then
              begin
               AShell.DoOutput(ASession,'Unable to create directory');
              end;
            end;
          end;
        end
       else
        begin
         AShell.DoOutput(ASession,'Directory already exists');
        end;
      end
     else
      begin
       AShell.DoOutput(ASession,'Required parameter missing');
      end;
     AShell.DoOutput(ASession,'');
 
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
    
     {Return Result}
     Result:=True;
    end;
  end; 
end;

{==============================================================================}
{==============================================================================}
{TShellFileSysRmdir}
constructor TShellFileSysRmdir.Create;
var
 Alias:TShellAlias;
begin
 {}
 inherited Create;

 Name:=SHELL_FILESYS_COMMAND_RMDIR;
 Flags:=SHELL_COMMAND_FLAG_INFO or SHELL_COMMAND_FLAG_HELP;
 
 {Create Alias}
 Alias:=TShellAlias.Create;
 Alias.Name:=SHELL_FILESYS_ALIAS_RD;
 
 {Register Alias}
 if not RegisterAlias(Alias) then
  begin
   {Destroy Alias}
   Alias.Free;
  end;
end;

{==============================================================================}
 
destructor TShellFileSysRmdir.Destroy; 
var
 Alias:TShellAlias;
begin
 {}
 {Get Alias}
 Alias:=FindAlias(SHELL_FILESYS_ALIAS_RD);
 
 {Check Alias}
 if Alias <> nil then
  begin
   {Degister Alias}
   DeregisterAlias(Alias);
   
   {Destroy Alias}
   Alias.Free;
  end;
  
 inherited Destroy;
end;

{==============================================================================}

function TShellFileSysRmdir.DoHelp(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Help}
 AShell.DoOutput(ASession,'Removes (deletes) a directory');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,' ' + Name + ' [drive:]path [/X]');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,'  /X            Deletes the alternate file name only');
 AShell.DoOutput(ASession,'');
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TShellFileSysRmdir.DoInfo(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Info}
 Result:=AShell.DoOutput(ASession,'Removes (deletes) a directory');
end;

{==============================================================================}

function TShellFileSysRmdir.DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; 
var
 Name:String;
 Path:String;
 Short:Boolean;
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;

 {Check Parameters}
 if AParameters = nil then Exit;
 
 {Check Help}
 if AShell.ParameterExists(SHELL_FILESYS_PARAMETER_HELP,AParameters) then
  begin
   {Show Help}
   Result:=DoHelp(AShell,ASession);
  end
 else
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Get Name}
     Name:=AShell.ParameterIndex(0,AParameters);
     if (Name <> '') and (Name[1] <> '/') and (Name[1] <> '-') then
      begin
       {Get Path}
       Path:=AddTrailingSlash(ExtractFilePath(Name));
       if Path = '' then Path:=AddTrailingSlash(FileSysDriver.GetCurrentDir);
       
       {Get Name}
       Name:=ExtractFileName(Name);
       
       {Get Options}
       Short:=AShell.ParameterExists(SHELL_FILESYS_PARAMETER_SHORT,AParameters);
       
       {Check Existing}
       if FileSysDriver.DirectoryExists(Path + Name) then
        begin
         if Short then
          begin
           {Delete Short Name}
           if not FileSysDriver.SetFileShortName(Path + Name,'') then
            begin
             AShell.DoOutput(ASession,'Unable to delete alternate file name');
            end;
          end
         else
          begin
           {Delete Directory}
           if not FileSysDriver.RemoveDir(Path + Name) then
            begin
             AShell.DoOutput(ASession,'Invalid path, not directory, or directory not empty');
            end;
          end;
        end
       else
        begin
         AShell.DoOutput(ASession,'Invalid path, not directory, or directory not empty');
        end;
      end
     else
      begin
       AShell.DoOutput(ASession,'Required parameter missing');
      end;
     AShell.DoOutput(ASession,'');
 
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
     
     {Return Result}
     Result:=True;
    end;
  end; 
end;

{==============================================================================}
{==============================================================================}
{TShellFileSysType}
constructor TShellFileSysType.Create;
begin
 {}
 inherited Create;

 Name:=SHELL_FILESYS_COMMAND_TYPE;
 Flags:=SHELL_COMMAND_FLAG_INFO or SHELL_COMMAND_FLAG_HELP;
end;

{==============================================================================}

function TShellFileSysType.DoHelp(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Help}
 AShell.DoOutput(ASession,'Displays the contents of text files');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,' ' + Name + ' [drive:][path]filename');
 AShell.DoOutput(ASession,'');
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TShellFileSysType.DoInfo(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Info}
 Result:=AShell.DoOutput(ASession,'Displays the contents of text files');
end;

{==============================================================================}

function TShellFileSysType.DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; 
var
 Name:String;
 Path:String;
 
 Count:Integer;
 Lines:TStringList;
 FileStream:TFSFileStream;
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;

 {Check Parameters}
 if AParameters = nil then Exit;
 
 {Check Help}
 if AShell.ParameterExists(SHELL_FILESYS_PARAMETER_HELP,AParameters) then
  begin
   {Show Help}
   Result:=DoHelp(AShell,ASession);
  end
 else
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Get Name}
     Name:=AShell.ParameterIndex(0,AParameters);
     if (Name <> '') and (Name[1] <> '/') and (Name[1] <> '-') then
      begin
       {Get Path}
       Path:=AddTrailingSlash(ExtractFilePath(Name));
       if Path = '' then Path:=AddTrailingSlash(FileSysDriver.GetCurrentDir);
 
       {Get Name}
       Name:=ExtractFileName(Name);
       
       {Check Existing}
       if FileSysDriver.FileExists(Path + Name) then
        begin
         try
          {Open File}
          FileStream:=TFSFileStream.Create(Path + Name,fmOpenRead or fmShareDenyNone);
          try
           {Load Lines}
           Lines:=TStringList.Create;
           try
            Lines.LoadFromStream(FileStream);
            for Count:=0 to Lines.Count - 1 do
             begin
              AShell.DoOutput(ASession,Lines.Strings[Count]);
             end;
           finally
            Lines.Free;
           end;
          finally
           FileStream.Free;
          end;
         except
          AShell.DoOutput(ASession,'Failed to open file - ' + Name);
         end;
        end
       else
        begin
         AShell.DoOutput(ASession,'File not found - ' + Name);
        end;
      end
     else
      begin
       AShell.DoOutput(ASession,'Required parameter missing');
      end;
     AShell.DoOutput(ASession,'');
 
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
     
     {Return Result}
     Result:=True;
    end;
  end; 
end;

{==============================================================================}
{==============================================================================}
{TShellFileSysCopy}
constructor TShellFileSysCopy.Create;
begin
 {}
 inherited Create;

 Name:=SHELL_FILESYS_COMMAND_COPY;
 Flags:=SHELL_COMMAND_FLAG_INFO or SHELL_COMMAND_FLAG_HELP;
end;

{==============================================================================}

function TShellFileSysCopy.DoHelp(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Help}
 AShell.DoOutput(ASession,'Copies one or more files to another location');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,' ' + Name + ' [drive:][path]filename [drive:][path]filename [/O]');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,'  [drive:][path]filename  Specifies the file or files to be copied');
 AShell.DoOutput(ASession,'                          Specify multiple files by using wildcards');
 AShell.DoOutput(ASession,'  [drive:][path]filename  Specifies the directory and/or filename for');
 AShell.DoOutput(ASession,'                          the new file(s)');
 AShell.DoOutput(ASession,'  /O            Overwrite existing file(s)');
 AShell.DoOutput(ASession,'');
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TShellFileSysCopy.DoInfo(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Info}
 Result:=AShell.DoOutput(ASession,'Copies one or more files to another location');
end;

{==============================================================================}

function TShellFileSysCopy.DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; 
var
 DestName:String;
 DestPath:String;
 SourceName:String;
 SourcePath:String;
 WorkBuffer:String;
 
 Code:Integer;
 TotalFiles:Int64;
 Wildcard:Boolean;
 Overwrite:Boolean;
 SearchRec:TFileSearchRec;
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;

 {Check Parameters}
 if AParameters = nil then Exit;
 
 {Check Help}
 if AShell.ParameterExists(SHELL_FILESYS_PARAMETER_HELP,AParameters) then
  begin
   {Show Help}
   Result:=DoHelp(AShell,ASession);
  end
 else
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Get Source Name}
     SourceName:=AShell.ParameterIndex(0,AParameters);
     if SourceName = '' then SourceName:=SHELL_FILESYS_ALLFILES_MASK;
     if (SourceName <> '') and (SourceName[1] <> '/') and (SourceName[1] <> '-') then
      begin
       {Get Source Path}
       SourcePath:=AddTrailingSlash(ExtractFilePath(SourceName));
       if SourcePath = '' then SourcePath:=AddTrailingSlash(FileSysDriver.GetCurrentDir);
       
       {Get Source Name}
       SourceName:=ExtractFileName(SourceName);
    
       {Check Source Folder}
       if FileSysDriver.DirectoryExists(SourcePath + SourceName) then
        begin
         SourcePath:=AddTrailingSlash(SourcePath + SourceName);
         SourceName:=SHELL_FILESYS_ALLFILES_MASK;
        end;
       
       {Get Dest Name}
       DestName:=AShell.ParameterIndex(1,AParameters);
       if DestName = '' then DestName:=SHELL_FILESYS_ALLFILES_MASK;
       if (DestName <> '') and (DestName[1] <> '/') and (DestName[1] <> '-') then
        begin
         {Get Dest Path}
         DestPath:=AddTrailingSlash(ExtractFilePath(DestName));
         if DestPath = '' then DestPath:=AddTrailingSlash(FileSysDriver.GetCurrentDir);
         
         {Get Dest Name}
         DestName:=ExtractFileName(DestName);
    
         {Check Dest Folder}
         if FileSysDriver.DirectoryExists(DestPath + DestName) then
          begin
           DestPath:=AddTrailingSlash(DestPath + DestName);
           DestName:=SHELL_FILESYS_ALLFILES_MASK;
          end;
    
         {Get Options}
         TotalFiles:=0;
         Wildcard:=False;
         Overwrite:=AShell.ParameterExists(SHELL_FILESYS_PARAMETER_OVERWRITE,AParameters);
         
         {Check Source Wildcard}
         if (Pos('?',SourceName) <> 0) or (Pos('*',SourceName) <> 0) then
          begin
           DestName:='';
           Wildcard:=True;
          end
         else
          begin
           {Check Dest Wildcard}
           if (Pos('?',DestName) <> 0) or (Pos('*',DestName) <> 0) then
            begin
             DestName:='';
            end;
          end;          
         
         {Check Source and Dest Paths}
         if (Uppercase(SourcePath) <> Uppercase(DestPath)) or (not(Wildcard) and (Uppercase(SourceName) <> Uppercase(DestName))) then
          begin
           {Find First}
           Code:=FileSysDriver.FindFirstEx(SourcePath + SourceName,SearchRec);
           if Code = 0 then
            begin
             while Code = 0 do
              begin
               if (SearchRec.FindData.dwFileAttributes and faDirectory) = faNone then
                begin
                 {Check Wildcard}
                 if Wildcard then
                  begin
                   {Copy File}
                   if FileSysDriver.FileCopy(SourcePath + SearchRec.FindData.cFileName,DestPath + SearchRec.FindData.cFileName,not(Overwrite)) then
                    begin
                     {Update Total}
                     Inc(TotalFiles);
                     
                     AShell.DoOutput(ASession,SourcePath + SearchRec.FindData.cFileName);
                    end
                   else 
                    begin
                     AShell.DoOutput(ASession,'Failed to copy file: ' + SearchRec.FindData.cFileName);
                    end;
                  end
                 else
                  begin
                   {Check Dest Name}
                   if DestName = '' then
                    begin
                     DestName:=SearchRec.FindData.cFileName;
                    end;
                   
                   {Check Source and Dest Names}
                   if Uppercase(SourcePath + SearchRec.FindData.cFileName) <> Uppercase(DestPath + DestName) then
                    begin
                     {Copy File}
                     if FileSysDriver.FileCopy(SourcePath + SearchRec.FindData.cFileName,DestPath + DestName,not(Overwrite)) then
                      begin
                       {Update Total}
                       Inc(TotalFiles);
                       
                       AShell.DoOutput(ASession,SourcePath + SearchRec.FindData.cFileName);
                      end
                     else 
                      begin
                       AShell.DoOutput(ASession,'Failed to copy file: ' + SearchRec.FindData.cFileName);
                      end;
                    end
                   else
                    begin
                     AShell.DoOutput(ASession,'File cannot be copied to itself');
                    end;
                  end;                  
                end;
                
               {Find Next}
               Code:=FileSysDriver.FindNextEx(SearchRec);
              end;
              
             {Find Close}
             FileSysDriver.FindCloseEx(SearchRec);
            end
           else
            begin
             AShell.DoOutput(ASession,'File not found');
            end;
           
           {Add Footer}
           WorkBuffer:='';
           AShell.AddOutput(WorkBuffer,10,IntToStr(TotalFiles) + ' file(s) copied');
           AShell.DoOutput(ASession,WorkBuffer);
          end
         else
          begin
           AShell.DoOutput(ASession,'Source and destination cannot be the same');
          end;
        end
       else
        begin
         AShell.DoOutput(ASession,'Required parameter missing');
        end;
      end
     else
      begin
       AShell.DoOutput(ASession,'Required parameter missing');
      end;
     AShell.DoOutput(ASession,'');
     
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
     
     {Return Result}
     Result:=True;
    end;
  end; 
end;

{==============================================================================}
{==============================================================================}
{TShellFileSysMove}
constructor TShellFileSysMove.Create;
begin
 {}
 inherited Create;

 Name:=SHELL_FILESYS_COMMAND_MOVE;
 Flags:=SHELL_COMMAND_FLAG_INFO or SHELL_COMMAND_FLAG_HELP;
end;

{==============================================================================}

function TShellFileSysMove.DoHelp(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Help}
 AShell.DoOutput(ASession,'Moves one or more files to another location');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,' ' + Name + ' [drive:][path]filename [drive:][path]filename [/O]');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,'  [drive:][path]filename  Specifies the file or files to be moved');
 AShell.DoOutput(ASession,'                          Specify multiple files by using wildcards');
 AShell.DoOutput(ASession,'  [drive:][path]filename  Specifies the new location for the file(s)');
 AShell.DoOutput(ASession,'  /O            Overwrite existing file(s)');
 AShell.DoOutput(ASession,'');
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TShellFileSysMove.DoInfo(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Info}
 Result:=AShell.DoOutput(ASession,'Moves one or more files to another location');
end;

{==============================================================================}

function TShellFileSysMove.DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean;
var
 DestName:String;
 DestPath:String;
 SourceName:String;
 SourcePath:String;
 WorkBuffer:String;
 
 Code:Integer;
 TotalFiles:Int64;
 Wildcard:Boolean;
 Overwrite:Boolean;
 SearchRec:TFileSearchRec;
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;

 {Check Parameters}
 if AParameters = nil then Exit;
 
 {Check Help}
 if AShell.ParameterExists(SHELL_FILESYS_PARAMETER_HELP,AParameters) then
  begin
   {Show Help}
   Result:=DoHelp(AShell,ASession);
  end
 else
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Get Source Name}
     SourceName:=AShell.ParameterIndex(0,AParameters);
     if SourceName = '' then SourceName:=SHELL_FILESYS_ALLFILES_MASK;
     if (SourceName <> '') and (SourceName[1] <> '/') and (SourceName[1] <> '-') then
      begin
       {Get Source Path}
       SourcePath:=AddTrailingSlash(ExtractFilePath(SourceName));
       if SourcePath = '' then SourcePath:=AddTrailingSlash(FileSysDriver.GetCurrentDir);
       
       {Get Source Name}
       SourceName:=ExtractFileName(SourceName);
    
       {Check Source Folder}
       if FileSysDriver.DirectoryExists(SourcePath + SourceName) then
        begin
         SourcePath:=AddTrailingSlash(SourcePath + SourceName);
         SourceName:=SHELL_FILESYS_ALLFILES_MASK;
        end;
       
       {Get Dest Name}
       DestName:=AShell.ParameterIndex(1,AParameters);
       if DestName = '' then DestName:=SHELL_FILESYS_ALLFILES_MASK;
       if (DestName <> '') and (DestName[1] <> '/') and (DestName[1] <> '-') then
        begin
         {Get Dest Path}
         DestPath:=AddTrailingSlash(ExtractFilePath(DestName));
         if DestPath = '' then DestPath:=AddTrailingSlash(FileSysDriver.GetCurrentDir);
         
         {Get Dest Name}
         DestName:=ExtractFileName(DestName);
    
         {Check Dest Folder}
         if FileSysDriver.DirectoryExists(DestPath + DestName) then
          begin
           DestPath:=AddTrailingSlash(DestPath + DestName);
           DestName:=SHELL_FILESYS_ALLFILES_MASK;
          end;
    
         {Get Options}
         TotalFiles:=0;
         Wildcard:=False;
         Overwrite:=AShell.ParameterExists(SHELL_FILESYS_PARAMETER_OVERWRITE,AParameters);
         
         {Check Source Wildcard}
         if (Pos('?',SourceName) <> 0) or (Pos('*',SourceName) <> 0) then
          begin
           DestName:='';
           Wildcard:=True;
          end
         else
          begin
           {Check Dest Wildcard}
           if (Pos('?',DestName) <> 0) or (Pos('*',DestName) <> 0) then
            begin
             DestName:='';
            end;
          end;          
         
         {Check Source and Dest Paths}
         if (Uppercase(SourcePath) <> Uppercase(DestPath)) or (not(Wildcard) and (Uppercase(SourceName) <> Uppercase(DestName))) then
          begin
           {Find First}
           Code:=FileSysDriver.FindFirstEx(SourcePath + SourceName,SearchRec);
           if Code = 0 then
            begin
             while Code = 0 do
              begin
               if (SearchRec.FindData.dwFileAttributes and faDirectory) = faNone then
                begin
                 {Check Wildcard}
                 if Wildcard then
                  begin
                   {Copy File}
                   if FileSysDriver.FileMove(SourcePath + SearchRec.FindData.cFileName,DestPath + SearchRec.FindData.cFileName,not(Overwrite)) then
                    begin
                     {Update Total}
                     Inc(TotalFiles);
                     
                     AShell.DoOutput(ASession,SourcePath + SearchRec.FindData.cFileName);
                    end
                   else 
                    begin
                     AShell.DoOutput(ASession,'Failed to move file: ' + SearchRec.FindData.cFileName);
                    end;
                  end
                 else
                  begin
                   {Check Dest Name}
                   if DestName = '' then
                    begin
                     DestName:=SearchRec.FindData.cFileName;
                    end;
                   
                   {Check Source and Dest Names}
                   if Uppercase(SourcePath + SearchRec.FindData.cFileName) <> Uppercase(DestPath + DestName) then
                    begin
                     {Copy File}
                     if FileSysDriver.FileMove(SourcePath + SearchRec.FindData.cFileName,DestPath + DestName,not(Overwrite)) then
                      begin
                       {Update Total}
                       Inc(TotalFiles);
                       
                       AShell.DoOutput(ASession,SourcePath + SearchRec.FindData.cFileName);
                      end
                     else 
                      begin
                       AShell.DoOutput(ASession,'Failed to move file: ' + SearchRec.FindData.cFileName);
                      end;
                    end
                   else
                    begin
                     AShell.DoOutput(ASession,'File cannot be copied to itself');
                    end;
                  end;                  
                end;
                
               {Find Next}
               Code:=FileSysDriver.FindNextEx(SearchRec);
              end;
              
             {Find Close}
             FileSysDriver.FindCloseEx(SearchRec);
            end
           else
            begin
             AShell.DoOutput(ASession,'File not found');
            end;
           
           {Add Footer}
           WorkBuffer:='';
           AShell.AddOutput(WorkBuffer,10,IntToStr(TotalFiles) + ' file(s) copied');
           AShell.DoOutput(ASession,WorkBuffer);
          end
         else
          begin
           AShell.DoOutput(ASession,'Source and destination cannot be the same');
          end;
        end
       else
        begin
         AShell.DoOutput(ASession,'Required parameter missing');
        end;
      end
     else
      begin
       AShell.DoOutput(ASession,'Required parameter missing');
      end;
     AShell.DoOutput(ASession,'');
 
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
     
     {Return Result}
     Result:=True;
    end;
  end; 
end;

{==============================================================================}
{==============================================================================}
{TShellFileSysDel}
constructor TShellFileSysDel.Create;
var
 Alias:TShellAlias;
begin
 {}
 inherited Create;

 Name:=SHELL_FILESYS_COMMAND_DEL;
 Flags:=SHELL_COMMAND_FLAG_INFO or SHELL_COMMAND_FLAG_HELP;
 
 {Create Alias (DELETE)}
 Alias:=TShellAlias.Create;
 Alias.Name:=SHELL_FILESYS_ALIAS_DELETE;
 
 {Register Alias}
 if not RegisterAlias(Alias) then
  begin
   {Destroy Alias}
   Alias.Free;
  end;

 {Create Alias (ERASE)}
 Alias:=TShellAlias.Create;
 Alias.Name:=SHELL_FILESYS_ALIAS_ERASE;
 
 {Register Alias}
 if not RegisterAlias(Alias) then
  begin
   {Destroy Alias}
   Alias.Free;
  end;
  
 {Create Alias (RM)}
 Alias:=TShellAlias.Create;
 Alias.Name:=SHELL_FILESYS_ALIAS_RM;
 
 {Register Alias}
 if not RegisterAlias(Alias) then
  begin
   {Destroy Alias}
   Alias.Free;
  end;
end;

{==============================================================================}
 
destructor TShellFileSysDel.Destroy; 
var
 Alias:TShellAlias;
begin
 {}
 {Get Alias (DELETE)}
 Alias:=FindAlias(SHELL_FILESYS_ALIAS_DELETE);
 
 {Check Alias}
 if Alias <> nil then
  begin
   {Degister Alias}
   DeregisterAlias(Alias);
   
   {Destroy Alias}
   Alias.Free;
  end;

 {Get Alias (ERASE)}
 Alias:=FindAlias(SHELL_FILESYS_ALIAS_ERASE);
  
 {Check Alias}
 if Alias <> nil then
  begin
   {Degister Alias}
   DeregisterAlias(Alias);
   
   {Destroy Alias}
   Alias.Free;
  end;

 {Get Alias (RM)}
 Alias:=FindAlias(SHELL_FILESYS_ALIAS_RM);
  
 {Check Alias}
 if Alias <> nil then
  begin
   {Degister Alias}
   DeregisterAlias(Alias);
   
   {Destroy Alias}
   Alias.Free;
  end;
  
 inherited Destroy;
end;

{==============================================================================}

function TShellFileSysDel.DoHelp(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Help}
 AShell.DoOutput(ASession,'Deletes one or more files');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,' ' + Name + ' [drive:][path]filename [/X]');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,'  [drive:][path]filename  Specifies the file(s) to delete.  Specify multiple');
 AShell.DoOutput(ASession,'                          files by using wildcards');
 AShell.DoOutput(ASession,'  /X            Deletes the alternate file name only');
 AShell.DoOutput(ASession,'');
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TShellFileSysDel.DoInfo(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Info}
 Result:=AShell.DoOutput(ASession,'Deletes one or more files');
end;

{==============================================================================}

function TShellFileSysDel.DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; 
var
 Name:String;
 Path:String;
 Code:Integer;
 Short:Boolean;
 SearchRec:TFileSearchRec;
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;

 {Check Parameters}
 if AParameters = nil then Exit;
 
 {Check Help}
 if AShell.ParameterExists(SHELL_FILESYS_PARAMETER_HELP,AParameters) then
  begin
   {Show Help}
   Result:=DoHelp(AShell,ASession);
  end
 else
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Get Name}
     Name:=AShell.ParameterIndex(0,AParameters);
     if (Name <> '') and (Name[1] <> '/') and (Name[1] <> '-') then
      begin
       {Get Path}
       Path:=AddTrailingSlash(ExtractFilePath(Name));
       if Path = '' then Path:=AddTrailingSlash(FileSysDriver.GetCurrentDir);
       
       {Get Name}
       Name:=ExtractFileName(Name);
       
       {Get Options}
       Short:=AShell.ParameterExists(SHELL_FILESYS_PARAMETER_SHORT,AParameters);
       
       {Check Wildcard}
       if (Pos('?',Name) = 0) and (Pos('*',Name) = 0) then
        begin
         {Delete File}
         if FileSysDriver.FileExists(Path + Name) then
          begin
           if Short then
            begin
             if not FileSysDriver.SetFileShortName(Path + Name,'') then
              begin
               AShell.DoOutput(ASession,'Unable to delete alternate file name');
              end;
            end
           else
            begin
             if not FileSysDriver.DeleteFile(Path + Name) then
              begin
               AShell.DoOutput(ASession,'Unable to delete file: ' + Name);
              end;
            end;
          end
         else
          begin
           AShell.DoOutput(ASession,'File not found');
          end;          
        end
       else
        begin       
         {Search Files}
         Code:=FileSysDriver.FindFirstEx(Path + Name,SearchRec);
         if Code = 0 then
          begin
           while Code = 0 do
            begin
             if (SearchRec.FindData.dwFileAttributes and faDirectory) = faNone then
              begin
               if Short then
                begin
                 if not FileSysDriver.SetFileShortName(Path + SearchRec.FindData.cFileName,'') then
                  begin
                   AShell.DoOutput(ASession,'Unable to delete alternate file name: ' + SearchRec.FindData.cAlternateFileName);
                  end;
                end
               else
                begin
                 if not FileSysDriver.DeleteFile(Path + SearchRec.FindData.cFileName) then
                  begin
                   AShell.DoOutput(ASession,'Unable to delete file: ' + SearchRec.FindData.cFileName);
                  end;
                end;
              end;
              
             Code:=FileSysDriver.FindNextEx(SearchRec);
            end;
            
           FileSysDriver.FindCloseEx(SearchRec);
          end
         else
          begin
           AShell.DoOutput(ASession,'File not found');
          end;
        end;
      end
     else
      begin
       AShell.DoOutput(ASession,'Required parameter missing');
      end;
     AShell.DoOutput(ASession,'');
 
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
     
     {Return Result}
     Result:=True;
    end;
  end; 
end;

{==============================================================================}
{==============================================================================}
{TShellFileSysRen}
constructor TShellFileSysRen.Create;
var
 Alias:TShellAlias;
begin
 {}
 inherited Create;

 Name:=SHELL_FILESYS_COMMAND_REN;
 Flags:=SHELL_COMMAND_FLAG_INFO or SHELL_COMMAND_FLAG_HELP;
 
 {Create Alias}
 Alias:=TShellAlias.Create;
 Alias.Name:=SHELL_FILESYS_ALIAS_RENAME;
 
 {Register Alias}
 if not RegisterAlias(Alias) then
  begin
   {Destroy Alias}
   Alias.Free;
  end;
end;

{==============================================================================}
 
destructor TShellFileSysRen.Destroy; 
var
 Alias:TShellAlias;
begin
 {}
 {Get Alias}
 Alias:=FindAlias(SHELL_FILESYS_ALIAS_RENAME);
 
 {Check Alias}
 if Alias <> nil then
  begin
   {Degister Alias}
   DeregisterAlias(Alias);
   
   {Destroy Alias}
   Alias.Free;
  end;
  
 inherited Destroy;
end;

{==============================================================================}

function TShellFileSysRen.DoHelp(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Help}
 AShell.DoOutput(ASession,'Renames a file/directory or files/directories');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,' ' + Name + ' [drive:][path][directoryname1 | filename1] [directoryname2 | filename2]');
 AShell.DoOutput(ASession,' ' + Name + ' [drive:][path][directoryname1 | filename1] [alternamefilename] /X');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,'  /X            Renames the alternate file name only');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,' Note that you cannot specify a new drive or path for your destination');
 AShell.DoOutput(ASession,'');
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TShellFileSysRen.DoInfo(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Info}
 Result:=AShell.DoOutput(ASession,'Renames a file/directory or files/directories');
end;

{==============================================================================}

function TShellFileSysRen.DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; 
var
 Name:String;
 Path:String;
 NewName:String;
 ShortName:String;
 
 Code:Integer;
 Short:Boolean;
 SearchRec:TFileSearchRec;
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;

 {Check Parameters}
 if AParameters = nil then Exit;
 
 {Check Help}
 if AShell.ParameterExists(SHELL_FILESYS_PARAMETER_HELP,AParameters) then
  begin
   {Show Help}
   Result:=DoHelp(AShell,ASession);
  end
 else
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Get Name}
     Name:=AShell.ParameterIndex(0,AParameters);
     if (Name <> '') and (Name[1] <> '/') and (Name[1] <> '-') then
      begin
       {Get Path}
       Path:=AddTrailingSlash(ExtractFilePath(Name));
       if Path = '' then Path:=AddTrailingSlash(FileSysDriver.GetCurrentDir);
       
       {Get Name}
       Name:=ExtractFileName(Name);
       
       {Get Options}
       Short:=AShell.ParameterExists(SHELL_FILESYS_PARAMETER_SHORT,AParameters);
       
       //To Do //Wildcards //See DEL
       
       if Short then
        begin
         {Get Short Name}
         ShortName:=AShell.ParameterIndex(1,AParameters);
         if (ShortName <> '') and (ShortName[1] <> '/') and (ShortName[1] <> '-') then
          begin
           if not FileSysDriver.SetFileShortName(Path + Name,ShortName) then
            begin
             AShell.DoOutput(ASession,'Unable to rename file');
            end;
          end
         else
          begin
           AShell.DoOutput(ASession,'Required parameter missing');
          end;
        end
       else
        begin
         {Get New Name}
         NewName:=AShell.ParameterIndex(1,AParameters);
         if (NewName <> '') and (NewName[1] <> '/') and (NewName[1] <> '-') then
          begin
           if not FileSysDriver.RenameFile(Path + Name,NewName) then
            begin
             AShell.DoOutput(ASession,'Unable to rename file');
            end;
          end
         else
          begin
           AShell.DoOutput(ASession,'Required parameter missing');
          end;
        end;
      end
     else
      begin
       AShell.DoOutput(ASession,'Required parameter missing');
      end;
     AShell.DoOutput(ASession,'');
 
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
     
     {Return Result}
     Result:=True;
    end;
  end; 
end;

{==============================================================================}
{==============================================================================}
{TShellFileSysAttrib}
constructor TShellFileSysAttrib.Create;
begin
 {}
 inherited Create;

 Name:=SHELL_FILESYS_COMMAND_ATTRIB;
 Flags:=SHELL_COMMAND_FLAG_INFO or SHELL_COMMAND_FLAG_HELP;
end;

{==============================================================================}

function TShellFileSysAttrib.AttribSearch(AShell:TShell;ASession:TShellSession;const APath,AName:String;ASubdir:Boolean;AMask,AUnmask:LongWord):Boolean;
var
 Code:Integer;
 Name:String;
 AltName:String;
 SearchRec:TFileSearchRec;
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Check Session}
 if ASession = nil then Exit;
 
 {Check Driver}
 if FileSysDriver = nil then Exit;
 
 {Display Header}
 AShell.DoOutput(ASession,' ' + APath);
 
 {Find First}
 Code:=FileSysDriver.FindFirstEx(APath + AName,SearchRec);
 while Code = 0 do
  begin
   if (SearchRec.FindData.dwFileAttributes and faDirectory) = faNone then
    begin
     {Set Attributes}
     if AMask <> faNone then
      begin
       Name:=SearchRec.FindData.cFileName;
       AltName:=SearchRec.FindData.cAlternateFileName;
       if Trim(AltName) = '' then
        begin
         FileSysDriver.FileSetAttr(APath + Name,LongWord(FileSysDriver.FileGetAttr(APath + Name)) or AMask);
        end
       else
        begin
         FileSysDriver.FileSetAttr(APath + AltName,LongWord(FileSysDriver.FileGetAttr(APath + AltName)) or AMask);
        end;
      end;
      
     {Clear Attributes}
     if AUnmask <> faNone then
      begin
       Name:=SearchRec.FindData.cFileName;
       AltName:=SearchRec.FindData.cAlternateFileName;
       if Trim(AltName) = '' then
        begin
         FileSysDriver.FileSetAttr(APath + Name,LongWord(FileSysDriver.FileGetAttr(APath + Name)) and not(AUnmask));
        end
       else
        begin
         FileSysDriver.FileSetAttr(APath + AltName,LongWord(FileSysDriver.FileGetAttr(APath + AltName)) and not(AUnmask));
        end;
      end;
      
     {Display Attributes}
     if (AMask = faNone) and (AUnmask = faNone) then
      begin
       AttribOutput(AShell,ASession,SearchRec);
      end;
    end;
    
   {Find Next}
   Code:=FileSysDriver.FindNextEx(SearchRec);
  end;
  
 {Find Close}
 FileSysDriver.FindCloseEx(SearchRec);
 
 {Check Subdir}
 if ASubdir then
  begin
   {Find First}
   Code:=FileSysDriver.FindFirstEx(APath + SHELL_FILESYS_ALLFILES_MASK,SearchRec);
   while Code = 0 do
    begin
     if (SearchRec.FindData.dwFileAttributes and faDirectory) = faDirectory then
      begin
       if (Trim(SearchRec.FindData.cFileName) <> '.') and (Trim(SearchRec.FindData.cFileName) <> '..') then
        begin
         if not AttribSearch(AShell,ASession,AddTrailingSlash(APath + SearchRec.FindData.cFileName),AName,ASubdir,AMask,AUnmask) then Break;
        end;
      end;
      
     {Find Next}
     Code:=FileSysDriver.FindNextEx(SearchRec);
    end;
    
   {Find Close}
   FileSysDriver.FindCloseEx(SearchRec);
  end;
  
 AShell.DoOutput(ASession,'');
 
 Result:=True;
end;

{==============================================================================}

function TShellFileSysAttrib.AttribOutput(AShell:TShell;ASession:TShellSession;const ASearchRec:TFileSearchRec):Boolean;
var
 Name:String;
 AltName:String;
 WorkBuffer:String;
begin
 {}
 Result:=False;
 
 WorkBuffer:='';
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Check Session}
 if ASession = nil then Exit;
 
 {Check Driver}
 if FileSysDriver = nil then Exit;
 
 {Display Attributes}
 if (ASearchRec.FindData.dwFileAttributes and faArchive) = faArchive then
  begin
   AShell.AddOutput(WorkBuffer,1,'A');
  end;
 if (ASearchRec.FindData.dwFileAttributes and faSysFile) = faSysFile then
  begin
   AShell.AddOutput(WorkBuffer,4,'S');
  end;
 if (ASearchRec.FindData.dwFileAttributes and faHidden) = faHidden then
  begin
   AShell.AddOutput(WorkBuffer,5,'H');
  end;
 if (ASearchRec.FindData.dwFileAttributes and faReadOnly) = faReadOnly then
  begin
   AShell.AddOutput(WorkBuffer,6,'R');
  end;
 
 //To Do Extended Attributes
 
 {Display Name}
 Name:=ASearchRec.FindData.cFileName;
 AltName:=ASearchRec.FindData.cAlternateFileName;
 if Trim(AltName) = '' then
  begin
   AShell.AddOutput(WorkBuffer,12,Name);
  end
 else
  begin
   AShell.AddOutput(WorkBuffer,12,Uppercase(AltName));
  end;
  
 {Write Output}
 AShell.DoOutput(ASession,WorkBuffer);
  
 Result:=True;
end;

{==============================================================================}

function TShellFileSysAttrib.DoHelp(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Help}
 AShell.DoOutput(ASession,'Displays or changes file attributes');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,' ' + Name + ' [[drive:][path]filename] [+R | -R] [+A | -A] [+S | -S] [+H | -H] [/S]');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,'  +   Sets an attribute');
 AShell.DoOutput(ASession,'  -   Clears an attribute');
 AShell.DoOutput(ASession,'  R   Read-only file attribute');
 AShell.DoOutput(ASession,'  A   Archive file attribute');
 AShell.DoOutput(ASession,'  S   System file attribute');
 AShell.DoOutput(ASession,'  H   Hidden file attribute');
 AShell.DoOutput(ASession,'  /S  Processes files in all subfolders in the specified path');
 AShell.DoOutput(ASession,'');
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TShellFileSysAttrib.DoInfo(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Info}
 Result:=AShell.DoOutput(ASession,'Displays or changes file attributes');
end;

{==============================================================================}

function TShellFileSysAttrib.DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; 
var
 Name:String;
 Path:String;
 
 Mask:LongWord;
 Unmask:LongWord;

 Code:Integer;
 Subdir:Boolean;
 SearchRec:TFileSearchRec;
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;

 {Check Parameters}
 if AParameters = nil then Exit;
 
 {Check Help}
 if AShell.ParameterExists(SHELL_FILESYS_PARAMETER_HELP,AParameters) then
  begin
   {Show Help}
   Result:=DoHelp(AShell,ASession);
  end
 else
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Setup Name}
     Name:=AShell.ParameterIndex(0,AParameters);
     if Name = '' then Name:=SHELL_FILESYS_ALLFILES_MASK;
     if (Name[1] = '/') or (Name[1] = '-') or (Name[1] = '+') then Name:=SHELL_FILESYS_ALLFILES_MASK;
     
     {Setup Path}
     Path:=AddTrailingSlash(ExtractFilePath(Name));
     if Path = '' then Path:=AddTrailingSlash(FileSysDriver.GetCurrentDir);
     
     {Check Folder}
     Name:=ExtractFileName(Name);
     if FileSysDriver.DirectoryExists(Path + Name) then
      begin
       Path:=AddTrailingSlash(Path + Name);
       Name:=SHELL_FILESYS_ALLFILES_MASK;
      end;
     
     {Get Options}
     Subdir:=AShell.ParameterExists(SHELL_FILESYS_PARAMETER_SUBDIR,AParameters);
     Mask:=faNone;
     if AShell.ParameterExistsEx(SHELL_FILESYS_PARAMETER_READONLY,AParameters,False,False) then Mask:=Mask or faReadOnly;
     if AShell.ParameterExistsEx(SHELL_FILESYS_PARAMETER_ARCHIVE,AParameters,False,False) then Mask:=Mask or faArchive;
     if AShell.ParameterExistsEx(SHELL_FILESYS_PARAMETER_SYSTEM,AParameters,False,False) then Mask:=Mask or faSysFile;
     if AShell.ParameterExistsEx(SHELL_FILESYS_PARAMETER_HIDDEN,AParameters,False,False) then Mask:=Mask or faHidden;
     Unmask:=faNone;
     if AShell.ParameterExistsEx(SHELL_FILESYS_PARAMETER_NOTREADONLY,AParameters,False,False) then Unmask:=Unmask or faReadOnly;
     if AShell.ParameterExistsEx(SHELL_FILESYS_PARAMETER_NOTARCHIVE,AParameters,False,False) then Unmask:=Unmask or faArchive;
     if AShell.ParameterExistsEx(SHELL_FILESYS_PARAMETER_NOTSYSTEM,AParameters,False,False) then Unmask:=Unmask or faSysFile;
     if AShell.ParameterExistsEx(SHELL_FILESYS_PARAMETER_NOTHIDDEN,AParameters,False,False) then Unmask:=Unmask or faHidden;
     
     {Perform Search}
     AttribSearch(AShell,ASession,Path,Name,Subdir,Mask,Unmask);
     
     AShell.DoOutput(ASession,'');
 
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
    
     {Return Result}
     Result:=True;
    end;
  end; 
end;

{==============================================================================}
{==============================================================================}
{TShellFileSysTouch}
constructor TShellFileSysTouch.Create;
begin
 {}
 inherited Create;

 Name:=SHELL_FILESYS_COMMAND_TOUCH;
 Flags:=SHELL_COMMAND_FLAG_INFO or SHELL_COMMAND_FLAG_HELP;
end;

{==============================================================================}

function TShellFileSysTouch.DoHelp(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Help}
 AShell.DoOutput(ASession,'Updates file create, modify or access times');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,' ' + Name + ' [drive:][path]filename [/C] [/M] [/A]');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,'  [drive:][path]filename  Specifies the file(s) to update.  Specify multiple');
 AShell.DoOutput(ASession,'                          files by using wildcards');
 AShell.DoOutput(ASession,'  /C                      Update the create time stamp');
 AShell.DoOutput(ASession,'  /M                      Update the modify time stamp');
 AShell.DoOutput(ASession,'  /A                      Update the access time stamp');
 AShell.DoOutput(ASession,'');
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TShellFileSysTouch.DoInfo(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Info}
 Result:=AShell.DoOutput(ASession,'Updates file create, modify or access times');
end;

{==============================================================================}

function TShellFileSysTouch.DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; 
var
 Name:String;
 Path:String;
 
 Code:Integer;
 SearchRec:TFileSearchRec;
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;

 {Check Parameters}
 if AParameters = nil then Exit;
 
 {Check Help}
 if AShell.ParameterExists(SHELL_FILESYS_PARAMETER_HELP,AParameters) then
  begin
   {Show Help}
   Result:=DoHelp(AShell,ASession);
  end
 else
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
    
     //To Do
     Result:= AShell.DoOutput(ASession,'Sorry, not implemented yet');
 
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
     
     {Return Result}
     Result:=True;
    end;
  end; 
end;

{==============================================================================}
{==============================================================================}
{TShellFileSysVol}
constructor TShellFileSysVol.Create;
begin
 {}
 inherited Create;

 Name:=SHELL_FILESYS_COMMAND_VOL;
 Flags:=SHELL_COMMAND_FLAG_INFO or SHELL_COMMAND_FLAG_HELP;
end;

{==============================================================================}

function TShellFileSysVol.DoHelp(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Help}
 AShell.DoOutput(ASession,'Displays the disk volume label and serial number, if they exist');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,' ' + Name + ' [drive:]');
 AShell.DoOutput(ASession,'');
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TShellFileSysVol.DoInfo(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Info}
 Result:=AShell.DoOutput(ASession,'Displays the disk volume label and serial number, if they exist');
end;

{==============================================================================}

function TShellFileSysVol.DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; 
var
 Name:String;
 Value:String;
 Drive:TDiskDrive;
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;

 {Check Parameters}
 if AParameters = nil then Exit;
 
 {Check Help}
 if AShell.ParameterExists(SHELL_FILESYS_PARAMETER_HELP,AParameters) then
  begin
   {Show Help}
   Result:=DoHelp(AShell,ASession);
  end
 else
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Get Name}
     Name:=AddTrailingSlash(AShell.ParameterIndex(0,AParameters));
     if Name = '' then Name:=DRIVE_NAMES[FileSysDriver.GetCurrentDrive];
    
     {Get Drive}
     Drive:=FileSysDriver.GetDriveByName(Name,True,FILESYS_LOCK_READ);
     if Drive <> nil then
      begin
       Value:=' has no label';
       if Drive.FileSystem <> nil then
        begin
         Value:=Drive.FileSystem.GetDriveLabel;
         if Value = '' then Value:=' has no label' else Value:=' is ' + Value;
        end;
       AShell.DoOutput(ASession,'Volume in drive ' + Drive.Name + Value);
       AShell.DoOutput(ASession,'Volume Serial Number is ' + IntToHex(Drive.VolumeSerial,8));
      
       Drive.ReaderUnlock;
      end
     else
      begin
       AShell.DoOutput(ASession,'Drive ' + Name + ' not found');
      end;
     AShell.DoOutput(ASession,'');
     
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
     
     {Return Result}
     Result:=True;
    end;
  end; 
end;

{==============================================================================}
{==============================================================================}
{TShellFileSysLabel}
constructor TShellFileSysLabel.Create;
begin
 {}
 inherited Create;

 Name:=SHELL_FILESYS_COMMAND_LABEL;
 Flags:=SHELL_COMMAND_FLAG_INFO or SHELL_COMMAND_FLAG_HELP;
end;

{==============================================================================}

function TShellFileSysLabel.DoHelp(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Help}
 AShell.DoOutput(ASession,'Creates, changes, or deletes the volume label of a disk');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,' ' + Name + ' [drive:][label]');
 AShell.DoOutput(ASession,'');
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TShellFileSysLabel.DoInfo(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Info}
 Result:=AShell.DoOutput(ASession,'Creates, changes, or deletes the volume label of a disk');
end;

{==============================================================================}

function TShellFileSysLabel.DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; 
var
 Name:String;
 Value:String;
 Drive:TDiskDrive;
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;

 {Check Parameters}
 if AParameters = nil then Exit;
 
 {Check Help}
 if AShell.ParameterExists(SHELL_FILESYS_PARAMETER_HELP,AParameters) then
  begin
   {Show Help}
   Result:=DoHelp(AShell,ASession);
  end
 else
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     {Get Name}
     Name:=AddTrailingSlash(AShell.ParameterIndex(0,AParameters));
     if Name = '' then Name:=DRIVE_NAMES[FileSysDriver.GetCurrentDrive];
     
     {Get Drive}
     Drive:=FileSysDriver.GetDriveByName(Name,True,FILESYS_LOCK_READ);
     if Drive <> nil then
      begin
       try
        {Get Label}
        Value:=AShell.ParameterIndex(1,AParameters);
        if Value = '' then
         begin
          Value:=' has no label';
          if Drive.FileSystem <> nil then
           begin
            Value:=Drive.FileSystem.GetDriveLabel;
            if Value = '' then Value:=' has no label' else Value:=' is ' + Value;
           end;
          AShell.DoOutput(ASession,'Volume in drive ' + Drive.Name + Value);
          AShell.DoOutput(ASession,'Volume Serial Number is ' + IntToHex(Drive.VolumeSerial,8));
          
          (*{Prompt Label} //To Do
          AShell.DoOutput(ASession,'Volume label (11 characters, ENTER for none)? ',2,False);
          Value:=Uppercase(AShell.DoInput(ASession,'',ConsoleWhereX,True,False));
          if Value = '' then
           begin
            {Prompt Delete}
            AShell.DoOutput(ASession,'');
            AShell.DoOutput(ASession,'Delete current label (Y/N)? ',2,False);
            if Uppercase(AShell.DoInput(ASession,'',ConsoleWhereX,True,False)) <> 'Y' then Exit;
           end;*)
         end;
         
        //{Set Label}
        //Drive.FileSystem.SetDriveLabel(Value); //To Do
       finally
        Drive.ReaderUnlock;
       end; 
      end
     else
      begin
       AShell.DoOutput(ASession,'Drive ' + Name + ' not found');
      end;
     AShell.DoOutput(ASession,'');
     
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
     
     {Return Result}
     Result:=True;
    end;
  end; 
end;

{==============================================================================}
{==============================================================================}
{TShellFileSysDeltree}
constructor TShellFileSysDeltree.Create;
begin
 {}
 inherited Create;

 Name:=SHELL_FILESYS_COMMAND_DELTREE;
 Flags:=SHELL_COMMAND_FLAG_INFO or SHELL_COMMAND_FLAG_HELP;
end;

{==============================================================================}

function TShellFileSysDeltree.DoHelp(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Help}
 AShell.DoOutput(ASession,'Deletes a directory and all the subdirectories and files in it');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,' To delete one or more files and directories:');
 AShell.DoOutput(ASession,' ' + Name + ' [drive:]path');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,'  [drive:]path    Specifies the name of the directory you want to delete');
 AShell.DoOutput(ASession,'');
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TShellFileSysDeltree.DoInfo(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Info}
 Result:=AShell.DoOutput(ASession,'Deletes a directory and all the subdirectories and files in it');
end;

{==============================================================================}

function TShellFileSysDeltree.DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; 
var
 Name:String;
 Path:String;
 
 Code:Integer;
 SearchRec:TFileSearchRec;
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;

 {Check Parameters}
 if AParameters = nil then Exit;
 
 {Check Help}
 if AShell.ParameterExists(SHELL_FILESYS_PARAMETER_HELP,AParameters) then
  begin
   {Show Help}
   Result:=DoHelp(AShell,ASession);
  end
 else
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
    
     //To Do
     Result:= AShell.DoOutput(ASession,'Sorry, not implemented yet');
     
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
     
     {Return Result}
     Result:=True;
    end;
  end; 
end;

{==============================================================================}
{==============================================================================}
{TShellFileSysXcopy}
 constructor TShellFileSysXcopy.Create;
begin
 {}
 inherited Create;

 Name:=SHELL_FILESYS_COMMAND_XCOPY;
 Flags:=SHELL_COMMAND_FLAG_INFO or SHELL_COMMAND_FLAG_HELP;
end;

{==============================================================================}

function TShellFileSysXcopy.DoHelp(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Help}
 AShell.DoOutput(ASession,'Copies files and directory trees');
 AShell.DoOutput(ASession,'');
 //To Do
 AShell.DoOutput(ASession,'');
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TShellFileSysXcopy.DoInfo(AShell:TShell;ASession:TShellSession):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;
 
 {Do Info}
 Result:=AShell.DoOutput(ASession,'Copies files and directory trees');
end;

{==============================================================================}

function TShellFileSysXcopy.DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; 
var
 Name:String;
 Path:String;
 
 Code:Integer;
 SearchRec:TFileSearchRec;
begin
 {}
 Result:=False;
 
 {Check Shell}
 if AShell = nil then Exit;

 {Check Parameters}
 if AParameters = nil then Exit;
 
 {Check Help}
 if AShell.ParameterExists(SHELL_FILESYS_PARAMETER_HELP,AParameters) then
  begin
   {Show Help}
   Result:=DoHelp(AShell,ASession);
  end
 else
  begin
   {Check Driver}
   if FileSysDriver <> nil then
    begin
     
     //To Do
     Result:= AShell.DoOutput(ASession,'Sorry, not implemented yet');
     
     {Update Prompt}
     ASession.Prompt:=FileSysDriver.GetCurrentDir + SHELL_DEFAULT_PROMPT;
     
     {Return Result}
     Result:=True;
    end;
  end; 
end;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure ShellFileSysInit;
begin
 {}
 {Check Initialized}
 if ShellFileSysInitialized then Exit;
 
 {Register FileSystem Commands}
 ShellRegisterCommand(TShellFileSysFileSystem.Create);
 ShellRegisterCommand(TShellFileSysController.Create);
 ShellRegisterCommand(TShellFileSysDisk.Create);
 ShellRegisterCommand(TShellFileSysPartition.Create);
 ShellRegisterCommand(TShellFileSysVolume.Create);
 ShellRegisterCommand(TShellFileSysDrive.Create);
 
 ShellRegisterCommand(TShellFileSysCache.Create);
 
 ShellRegisterCommand(TShellFileSysDir.Create);
 ShellRegisterCommand(TShellFileSysChdir.Create);
 ShellRegisterCommand(TShellFileSysMkdir.Create);
 ShellRegisterCommand(TShellFileSysRmdir.Create);
 
 ShellRegisterCommand(TShellFileSysType.Create);
 ShellRegisterCommand(TShellFileSysCopy.Create);
 ShellRegisterCommand(TShellFileSysMove.Create);
 ShellRegisterCommand(TShellFileSysDel.Create);
 ShellRegisterCommand(TShellFileSysRen.Create);
 ShellRegisterCommand(TShellFileSysAttrib.Create);
 ShellRegisterCommand(TShellFileSysTouch.Create);
 
 ShellRegisterCommand(TShellFileSysVol.Create);
 ShellRegisterCommand(TShellFileSysLabel.Create);
 
 ShellRegisterCommand(TShellFileSysDeltree.Create);
 ShellRegisterCommand(TShellFileSysXcopy.Create);
 
 ShellFileSysInitialized:=True;
end;
 
{==============================================================================}
{==============================================================================}
{Shell FileSystem Functions}
 
{==============================================================================}
{==============================================================================}
{Shell FileSystem Helper Functions}
 
{==============================================================================}
{==============================================================================}

initialization
 ShellFileSysInit;
 
{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}
 
end.
 