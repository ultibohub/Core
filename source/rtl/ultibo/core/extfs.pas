{
Ultibo EXT2/3/4 File System interface unit.

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


EXT FileSystem
==============


}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit EXTFS;

interface

uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  FileSystem,
  SysUtils,
  Classes,
  Unicode,
  Ultibo,
  UltiboUtils,
  UltiboClasses;

//To Do //See: http://wiki.osdev.org/Ext2

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {EXTFS specific constants}
 extfsEXT2 = 0;
 extfsEXT3 = 1;
 rxtfsEXT4 = 2;

 extfsNames:array[0..2] of String = (
  'EXT2',
  'EXT3',
  'EXT4');

 //To Do

{==============================================================================}
type
 {EXTFS specific types}
 TEXTFSType = (etNONE,etEXT2,etEXT3,etEXT4);

 //To Do

{==============================================================================}
type
 {EXTFS specific classes}
 TEXTFSRecognizer = class(TRecognizer)
   constructor Create(ADriver:TFileSysDriver);
  private
   {Private Variables}

  protected
   {Protected Variables}

   {Protected Methods}
   function GetName:String; override;
  public
   {Public Variables}

   {Public Methods}
   function RecognizePartitionId(APartitionId:Byte):Boolean; override;
   function RecognizeBootSector(ABootSector:PBootSector;const AStartSector,ASectorCount:Int64):Boolean; override;

   function RecognizePartition(APartition:TDiskPartition):Boolean; override;
   function RecognizeVolume(AVolume:TDiskVolume):Boolean; override;
   function MountVolume(AVolume:TDiskVolume;ADrive:TDiskDrive):Boolean; override;
 end;

 TEXTFSPartitioner = class(TDiskPartitioner)
   constructor Create(ADriver:TFileSysDriver;ARecognizer:TRecognizer);
  private
   {Private Variables}

   {Private Methods}
  protected
   {Protected Variables}

   {Protected Methods}
   function CheckLogical(ADevice:TDiskDevice;AParent:TDiskPartition;APartitionId:Byte):Boolean; override;
   function CheckExtended(ADevice:TDiskDevice;AParent:TDiskPartition;APartitionId:Byte):Boolean; override;

   function GetPartitionId(ADevice:TDiskDevice;AParent:TDiskPartition;AStart,ACount:LongWord;APartitionId:Byte):Byte; override;

   function InitPartition(ADevice:TDiskDevice;AParent:TDiskPartition;AStart,ACount:LongWord;APartitionId:Byte):Boolean; override;
  public
   {Public Variables}

   {Public Methods}
   function AcceptPartition(ADevice:TDiskDevice;APartition,AParent:TDiskPartition;APartitionId:Byte):Boolean; override;
 end;

 TEXTFSFileSystem = class(TFileSystem)
   constructor Create(ADriver:TFileSysDriver;AVolume:TDiskVolume;ADrive:TDiskDrive);
   destructor Destroy; override;
  private
   {Private Variables}
  public
   {Public Variables}
   //To Do
 end;

{==============================================================================}
{var}
 {EXTFS specific variables}

{==============================================================================}
{Initialization Functions}
procedure EXTFSInit;
procedure EXTFSQuit;

{==============================================================================}
{EXTFS Functions}
//To Do

{==============================================================================}
{EXTFS Helper Functions}
//To Do

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {EXTFS specific variables}
 EXTFSInitialized:Boolean;

{==============================================================================}
{==============================================================================}
{TEXTFSRecognizer}
constructor TEXTFSRecognizer.Create(ADriver:TFileSysDriver);
begin
 {}
 inherited Create(ADriver);
 FAllowDrive:=False;
 FAllowDefault:=False;

 FPartitioner:=TEXTFSPartitioner.Create(FDriver,Self);
 //To Do
end;

{==============================================================================}

function TEXTFSRecognizer.GetName:String;
begin
 {}
 Result:='EXTFS';
end;

{==============================================================================}

function TEXTFSRecognizer.RecognizePartitionId(APartitionId:Byte):Boolean;
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;

  {$IFDEF EXTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('TEXTFSRecognizer.RecognizePartitionId (PartitionId = ' + IntToStr(APartitionId) + ')');
  {$ENDIF}

  case APartitionId of
   pidLinuxExtended,pidExtended,pidExtLBA:begin
     {Linux or DOS Extended Partition}
     Result:=True;
    end;
   pidLinuxSwap:begin
     {Linux Swap Partition}
     Result:=True;
    end;
   pidLinuxNative:begin
     {Linux Native Partition}
     Result:=True;
    end;
  end;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TEXTFSRecognizer.RecognizeBootSector(ABootSector:PBootSector;const AStartSector,ASectorCount:Int64):Boolean;
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;

  //To Do
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TEXTFSRecognizer.RecognizePartition(APartition:TDiskPartition):Boolean;
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if APartition = nil then Exit;

  case APartition.PartitionId of
   pidLinuxExtended,pidExtended,pidExtLBA:begin
     {Linux or DOS Extended Partition}
     APartition.Extended:=True;
     APartition.Recognized:=True;

     Result:=True;
    end;
   pidLinuxSwap:begin
     {Linux Swap Partition}
     APartition.Recognized:=True;
     APartition.NonVolume:=True;

     Result:=True;
    end;
   pidLinuxNative:begin
     {Linux Native Partition}
     APartition.Recognized:=True;

     Result:=True;
    end;
  end;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TEXTFSRecognizer.RecognizeVolume(AVolume:TDiskVolume):Boolean;
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if AVolume = nil then Exit;

  //To Do
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TEXTFSRecognizer.MountVolume(AVolume:TDiskVolume;ADrive:TDiskDrive):Boolean;
var
 FileSystem:TEXTFSFileSystem;
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if AVolume = nil then Exit;

  {Check Recognized}
  if not RecognizeVolume(AVolume) then Exit;

  {Create FileSystem}
  FileSystem:=TEXTFSFileSystem.Create(FDriver,AVolume,ADrive);
  FileSystem.FileSystemInit;
  FileSystem.MountFileSystem;

  Result:=True;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}
{==============================================================================}
{TEXTFSPartitioner}
constructor TEXTFSPartitioner.Create(ADriver:TFileSysDriver;ARecognizer:TRecognizer);
begin
 {}
 inherited Create(ADriver,ARecognizer);
end;

{==============================================================================}

function TEXTFSPartitioner.CheckLogical(ADevice:TDiskDevice;AParent:TDiskPartition;APartitionId:Byte):Boolean;
{Note: Caller must hold the device and parent lock}
begin
 {}
 Result:=False;

 if ADevice = nil then Exit;

 {Check Type}
 case APartitionId of
  pidLinuxNative:begin
    if AParent = nil then Exit;

    Result:=True;
   end;
 end;
end;

{==============================================================================}

function TEXTFSPartitioner.CheckExtended(ADevice:TDiskDevice;AParent:TDiskPartition;APartitionId:Byte):Boolean;
{Note: Caller must hold the device and parent lock}
begin
 {}
 Result:=False;

 if ADevice = nil then Exit;

 {Check Type}
 case APartitionId of
  pidLinuxExtended,pidExtended,pidExtLBA:begin
    Result:=True;
   end;
 end;
end;

{==============================================================================}

function TEXTFSPartitioner.GetPartitionId(ADevice:TDiskDevice;AParent:TDiskPartition;AStart,ACount:LongWord;APartitionId:Byte):Byte;
{Note: Start is the absolute start sector on the device}
{Note: Caller must hold the device and parent lock}
var
 LBA:Boolean;
begin
 {}
 Result:=pidUnused;

 if ACount = 0 then Exit;
 if ADevice = nil then Exit;

 {Get LBA}
 LBA:=(ADevice.LBA and ((ADevice.PhysicalCylinders > 1024) or (ADevice.PhysicalCylinders = 0)));
 {Check Type}
 case APartitionId of
  pidLinuxExtended:begin
    Result:=APartitionId;
    if AParent <> nil then
     begin
      {Check Parent}
      case AParent.PartitionId of
       pidExtended,pidExtLBA:begin
         {Parent is DOS Extended}
         Result:=pidExtended;

         {DOS only allows standard type for second level Extended}
         {if (LBA) and (AParent = nil) then Result:=pidExtLBA;}
        end;
      end;
     end;
   end;
  pidExtended,pidExtLBA:begin
    Result:=APartitionId;
    {DOS only allows standard type for second level Extended}
    if (LBA) and (AParent = nil) then Result:=pidExtLBA;
    if AParent <> nil then
     begin
      {Check Parent}
      case AParent.PartitionId of
       pidLinuxExtended:begin
         Result:=pidLinuxExtended;
        end;
      end;
     end;
   end;
  pidLinuxNative,pidLinuxSwap:begin
    Result:=APartitionId;
   end;
 end;
end;

{==============================================================================}

function TEXTFSPartitioner.InitPartition(ADevice:TDiskDevice;AParent:TDiskPartition;AStart,ACount:LongWord;APartitionId:Byte):Boolean;
{Note: Start is the absolute start sector on the device}
{Note: Caller must hold the device and parent lock}
begin
 {}
 Result:=False;

 if ACount = 0 then Exit;
 if ADevice = nil then Exit;

 {Check Type}
 case APartitionId of
  pidLinuxExtended,pidExtended,pidExtLBA:begin
    {Initialize Partition Record}
    Result:=FillSectors(ADevice,nil,AStart,1,FInitChar);
   end;
  pidLinuxNative,pidLinuxSwap:begin
    {Initialize Boot Sectors}
    Result:=FillSectors(ADevice,nil,AStart,16,FInitChar);
   end;
 end;
end;

{==============================================================================}

function TEXTFSPartitioner.AcceptPartition(ADevice:TDiskDevice;APartition,AParent:TDiskPartition;APartitionId:Byte):Boolean;
{Note: Caller must hold the device, partition and parent lock}
var
 Volume:TDiskVolume;
begin
 {}
 Result:=False;

 if not ReaderLock then Exit;
 try
  if FDriver = nil then Exit;
  if FRecognizer = nil then Exit;

  if APartition = nil then
   begin
    {Accept Create Partition}
    if ADevice = nil then Exit;

    {Check Device}
    if (ADevice.MediaType <> mtFIXED) and (ADevice.MediaType <> mtREMOVABLE) then Exit;

    {Check Partition and Volume}
    if FDriver.GetPartitionByDevice(ADevice,False,FILESYS_LOCK_NONE) = nil then {Do not lock}
     begin
      Volume:=FDriver.GetVolumeByDevice(ADevice,True,FILESYS_LOCK_READ);
      if Volume <> nil then
       begin
        try
         {Check File System Type}
         if Volume.FileSysType <> fsUNKNOWN then Exit;
        finally
         Volume.ReaderUnlock;
        end;
       end;
     end;

    {Check Parent}
    if AParent <> nil then
     begin
      {Check Extended}
      if not AParent.Extended then Exit;

      {Check First Level}
      if AParent.Partition <> nil then Exit;
     end;

    {Check Type}
    case APartitionId of
     pidLinuxExtended,pidExtended,pidExtLBA,pidLinuxSwap:begin
       {Check Parent}
       if AParent <> nil then Exit;

       Result:=True;
      end;
     pidLinuxNative:begin
       Result:=True;
      end;
    end;
   end
  else
   begin
    if APartitionId = pidUnused then
     begin
      {Accept Delete Partition}
      {Check Children}
      if (AParent = nil) and (FDriver.GetPartitionByPartition(APartition,False,FILESYS_LOCK_NONE) <> nil) then Exit; {Do not lock}

      Result:=True;
     end
    else if APartitionId <> APartition.PartitionId then
     begin
      {Accept Modify Partition}
      {Check Extended}
      if APartition.Extended then Exit;

      {Nothing}
     end
    else if APartitionId = APartition.PartitionId then
     begin
      {Accept Activate Partition}
      {Check Primary}
      if not APartition.Primary then Exit;

      {Check Type}
      case APartitionId of
       pidLinuxNative:begin
         Result:=True;
        end;
      end;
     end;
   end;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}
{==============================================================================}
{TEXTFSFileSystem}
constructor TEXTFSFileSystem.Create(ADriver:TFileSysDriver;AVolume:TDiskVolume;ADrive:TDiskDrive);
begin
 {}
 inherited Create(ADriver,AVolume,ADrive);
 FReadOnly:=False;
 FLongNames:=True;
 FDataStreams:=False;
 FReparsePoints:=False;
 FCaseSensitive:=True;
 //To Do
end;

{==============================================================================}

destructor TEXTFSFileSystem.Destroy;
begin
 {}
 //To Do
 inherited Destroy;
end;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure EXTFSInit;
var
 Recognizer:TEXTFSRecognizer;
begin
 {}
 {Check Initialized}
 if EXTFSInitialized then Exit;

 {Check Driver}
 if FileSysDriver = nil then Exit;

 {Create EXTFS Recognizer}
 if FILESYS_EXTFS_ENABLED then
  begin
   Recognizer:=TEXTFSRecognizer.Create(FileSysDriver);
   Recognizer.AllowDrive:=FILESYS_DRIVES_ENABLED;
   Recognizer.AllowDefault:=EXTFS_DEFAULT;
  end;

 EXTFSInitialized:=True;
end;

{==============================================================================}

procedure EXTFSQuit;
var
 NextRecognizer:TRecognizer;
 CurrentRecognizer:TRecognizer;
 NextFileSystem:TFileSystem;
 CurrentFileSystem:TFileSystem;
begin
 {}
 {Check Initialized}
 if not EXTFSInitialized then Exit;

 {Check Driver}
 if FileSysDriver = nil then Exit;

 {Terminate FileSystems}
 NextFileSystem:=FileSysDriver.GetFileSystemByNext(nil,True,False,FILESYS_LOCK_READ);
 while NextFileSystem <> nil do
  begin
   CurrentFileSystem:=NextFileSystem;
   NextFileSystem:=FileSysDriver.GetFileSystemByNext(CurrentFileSystem,True,False,FILESYS_LOCK_READ);

   if CurrentFileSystem is TEXTFSFileSystem then
    begin
     {Convert FileSystem}
     CurrentFileSystem.ReaderConvert;

     {FileSysDriver.RemoveFileSystem(CurrentFileSystem);} {Done by Destroy}
     CurrentFileSystem.DismountFileSystem;
     CurrentFileSystem.Free;
    end
   else
    begin
     {Unlock FileSystem}
     CurrentFileSystem.ReaderUnlock;
    end;
  end;

 {Terminate Recognizer}
 NextRecognizer:=FileSysDriver.GetRecognizerByNext(nil,True,False,FILESYS_LOCK_READ);
 while NextRecognizer <> nil do
  begin
   CurrentRecognizer:=NextRecognizer;
   NextRecognizer:=FileSysDriver.GetRecognizerByNext(CurrentRecognizer,True,False,FILESYS_LOCK_READ);

   if CurrentRecognizer is TEXTFSRecognizer then
    begin
     {Convert Recognizer}
     CurrentRecognizer.ReaderConvert;

     {FileSysDriver.RemoveRecognizer(CurrentRecognizer);} {Done by Destroy}
     CurrentRecognizer.Free;
    end
   else
    begin
     {Unlock Recognizer}
     CurrentRecognizer.ReaderUnlock;
    end;
  end;

 EXTFSInitialized:=False;
end;

{==============================================================================}
{==============================================================================}
{EXTFS Functions}

{==============================================================================}
{==============================================================================}
{EXTFS Helper Functions}

{==============================================================================}
{==============================================================================}

initialization
 EXTFSInit;

{==============================================================================}

finalization
 EXTFSQuit;

{==============================================================================}
{==============================================================================}

end.


