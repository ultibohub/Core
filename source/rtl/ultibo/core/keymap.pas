{
Ultibo Keymap interface unit.

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


Keymaps
=======

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Keymap;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {Keymap specific constants}

 {Keymap Signature}
 KEYMAP_SIGNATURE = $863FDBA1;
 
 {Keymap Type constants}
 KEYMAP_TYPE_NONE = 0;
 
 {Keymap Flag constants}
 KEYMAP_FLAG_NONE = $00000000;
             
{==============================================================================}
type
 {Keymap specific types}

 {Keymap Header}
 PKeymapHeader = ^TKeymapHeader;
 TKeymapHeader = record
  Name:String[255];
 end;
 
 {Keymap Data}
 PKeymapData = ^TKeymapData;
 TKeymapData = record
  Name:String[255];
  Data:array[0..0] of Byte;
 end;
 
 {Keymap Properties}
 PKeymapProperties = ^TKeymapProperties;
 TKeymapProperties = record
  KeymapType:LongWord;       {Keymap type}
  KeymapFlags:LongWord;      {Keymap flags}
  KeymapName:String;         {Keymap name}
 end;

 PKeymapEntry = ^TKeymapEntry;
 
 {Keymap Enumeration Callback}
 TKeymapEnumerate = function(Handle:TKeymapHandle;Data:Pointer):LongWord;
 
 {Keymap Entry}
 TKeymapEntry = record
  {Keymap Properties}
  Signature:LongWord;            {Signature for entry validation}
  KeymapType:LongWord;           {Keymap type}
  KeymapFlags:LongWord;          {Keymap flags}
  KeymapName:String;             {Keymap name}
  {Driver Properties}
  KeyData:Pointer;               {Keymap key data}
  {Internal Properties}
  Prev:PKeymapEntry;             {Previous entry in Keymap table}
  Next:PKeymapEntry;             {Next entry in Keymap table}
 end;
 
{==============================================================================}
{var}
 {Keymap specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure KeymapInit;

{==============================================================================}
{Keymap Functions}
function KeymapLoad(Data:PKeymapData;Size:LongWord):TKeymapHandle;
function KeymapLoadEx(Data:PKeymapData;Size:LongWord;Properties:PKeymapProperties):TKeymapHandle;
function KeymapUnload(Handle:TKeymapHandle):LongWord;

function KeymapGetName(Handle:TKeymapHandle):String;

function KeymapGetProperties(Handle:TKeymapHandle;Properties:PKeymapProperties):LongWord;

function KeymapEnumerate(Callback:TKeymapEnumerate;Data:Pointer):LongWord;

{==============================================================================}
{Keymap Helper Functions}
function KeymapGetCount:LongWord; inline;
function KeymapGetDefault:TKeymapHandle; inline;

function KeymapCheck(Keymap:PKeymapEntry):PKeymapEntry;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Keymap specific variables}
 KeymapInitialized:Boolean;

 KeymapTable:PKeymapEntry;
 KeymapTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 KeymapTableCount:LongWord;
 
 KeymapDefault:TKeymapHandle = INVALID_HANDLE_VALUE;
 
//var 
 {Default Keymap}
 //To Do
 
{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure KeymapInit;
begin
 {}
 {Check Initialized}
 if KeymapInitialized then Exit;
 
 {Initialize Keymap Table}
 KeymapTable:=nil;
 KeymapTableLock:=CriticalSectionCreate; 
 KeymapTableCount:=0;
 if KeymapTableLock = INVALID_HANDLE_VALUE then
  begin
   if PLATFORM_LOG_ENABLED then PlatformLogError('Failed to create keymap table lock');
  end;
 KeymapDefault:=INVALID_HANDLE_VALUE;
 
 {Load Default Keymap}
 //KeymapDefault:=KeymapLoad(@???????,SizeOf(TKeymapData));
 //if KeymapDefault = INVALID_HANDLE_VALUE then
 // begin
 //  if PLATFORM_LOG_ENABLED then PlatformLogError('Failed to load default keymap');
 // end;
 //To Do
 
 KeymapInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Keymap Functions}
function KeymapLoad(Data:PKeymapData;Size:LongWord):TKeymapHandle;
{Load a Keymap from a keymap data block and add to the Keymap table}
begin
 {}
 Result:=KeymapLoadEx(Data,Size,nil);
end;

{==============================================================================}

function KeymapLoadEx(Data:PKeymapData;Size:LongWord;Properties:PKeymapProperties):TKeymapHandle;
{Load a Keymap from a keymap data block and add to the Keymap table}
var
 Keymap:PKeymapEntry;
 TotalSize:LongWord;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Check Data}
 if Data = nil then Exit;
 
 {Check Size}
 if Size < SizeOf(TKeymapData) then Exit;
 
 {Check Properties}
 if Properties = nil then
  begin
   {Get Size}
   TotalSize:=1; //To Do //See Font
   
   {Check Size}
   //To Do //See Font
   
   {Create Keymap}
   Keymap:=PKeymapEntry(AllocMem(SizeOf(TKeymapEntry)));
   if Keymap = nil then Exit;
   
   {Update Keymap}
   Keymap.Signature:=KEYMAP_SIGNATURE;
   Keymap.KeymapType:=KEYMAP_TYPE_NONE;
   Keymap.KeymapFlags:=KEYMAP_FLAG_NONE;
   Keymap.KeymapName:=Data.Name;
   Keymap.KeyData:=nil;
   
   {Update Keymap}
   UniqueString(Keymap.KeymapName);
   Keymap.KeyData:=GetMem(TotalSize);
   if Keymap.KeyData = nil then
    begin
     FreeMem(Keymap);
     Exit;
    end;
    
   {Copy Data}
   //To Do //See Font
  end
 else
  begin 
   {Get Size}
   TotalSize:=1; //To Do //See Font
   
   {Check Size}
   //To Do //See Font

   {Create Keymap}
   Keymap:=PKeymapEntry(AllocMem(SizeOf(TKeymapEntry)));
   if Keymap = nil then Exit;
   
   {Update Keymap}
   Keymap.Signature:=KEYMAP_SIGNATURE;
   Keymap.KeymapType:=Properties.KeymapType;
   Keymap.KeymapFlags:=Properties.KeymapFlags;
   Keymap.KeymapName:=Properties.KeymapName;
   Keymap.KeyData:=nil;
   
   {Update Keymap}
   UniqueString(Keymap.KeymapName);
   Keymap.KeyData:=GetMem(TotalSize);
   if Keymap.KeyData = nil then
    begin
     FreeMem(Keymap);
     Exit;
    end;
   
   {Copy Data}
   //To Do //See Font
  end;
 
 {Insert Keymap}
 if CriticalSectionLock(KeymapTableLock) = ERROR_SUCCESS then
  begin
   try
    {Link Keymap}
    if KeymapTable = nil then
     begin
      KeymapTable:=Keymap;
     end
    else
     begin
      Keymap.Next:=KeymapTable;
      KeymapTable.Prev:=Keymap;
      KeymapTable:=Keymap;
     end;
 
    {Increment Count}
    Inc(KeymapTableCount);
    
    {Check Default}
    if KeymapDefault = INVALID_HANDLE_VALUE then
     begin
      KeymapDefault:=TKeymapHandle(Keymap);
     end;
     
    {Return Result}
    Result:=TKeymapHandle(Keymap);
   finally
    CriticalSectionUnlock(KeymapTableLock);
   end;
  end
 else
  begin
   {Free Keys}
   FreeMem(Keymap.KeyData);
   
   {Free Keymap}
   FreeMem(Keymap);
  end;  
end;
  
{==============================================================================}

function KeymapUnload(Handle:TKeymapHandle):LongWord;
var
 Prev:PKeymapEntry;
 Next:PKeymapEntry;
 Keymap:PKeymapEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Keymap}
 Keymap:=PKeymapEntry(Handle);
 if Keymap = nil then Exit;
 if Keymap.Signature <> KEYMAP_SIGNATURE then Exit;
 
 {Check Keymap}
 Result:=ERROR_NOT_FOUND;
 if KeymapCheck(Keymap) <> Keymap then Exit;
 
 {Remove Keymap}
 if CriticalSectionLock(KeymapTableLock) = ERROR_SUCCESS then
  begin
   try
    {Unlink Keymap}
    Prev:=Keymap.Prev;
    Next:=Keymap.Next;
    if Prev = nil then
     begin
      KeymapTable:=Next;
      if Next <> nil then
       begin
        Next.Prev:=nil;
       end;       
     end
    else
     begin
      Prev.Next:=Next;
      if Next <> nil then
       begin
        Next.Prev:=Prev;
       end;       
     end;     
 
    {Decrement Count}
    Dec(KeymapTableCount);
 
    {Check Default}
    if KeymapDefault = Handle then
     begin
      KeymapDefault:=TKeymapHandle(KeymapTable);
     end;
     
    {Update Keymap}
    Keymap.Signature:=0;
 
    {Free Keys}
    FreeMem(Keymap.KeyData);
   
    {Free Keymap}
    FreeMem(Keymap);
 
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(KeymapTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function KeymapGetName(Handle:TKeymapHandle):String;
var
 Keymap:PKeymapEntry;
begin
 {}
 Result:='';
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Keymap}
 Keymap:=PKeymapEntry(Handle);
 if Keymap = nil then Exit;
 if Keymap.Signature <> KEYMAP_SIGNATURE then Exit;
 
 {Acquire Lock}
 if CriticalSectionLock(KeymapTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Name}
    Result:=Keymap.KeymapName;
    
    UniqueString(Result);
   finally
    CriticalSectionUnlock(KeymapTableLock);
   end;
  end;
end;

{==============================================================================}

function KeymapGetProperties(Handle:TKeymapHandle;Properties:PKeymapProperties):LongWord;
var
 Keymap:PKeymapEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Check Properties}
 if Properties = nil then Exit;
 
 {Get Keymap}
 Keymap:=PKeymapEntry(Handle);
 if Keymap = nil then Exit;
 if Keymap.Signature <> KEYMAP_SIGNATURE then Exit;
 
 {Acquire Lock}
 if CriticalSectionLock(KeymapTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Properties}
    Properties.KeymapType:=Keymap.KeymapType;
    Properties.KeymapFlags:=Keymap.KeymapFlags;
    Properties.KeymapName:=Keymap.KeymapName;
    
    UniqueString(Properties.KeymapName);
    
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(KeymapTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function KeymapEnumerate(Callback:TKeymapEnumerate;Data:Pointer):LongWord;
var
 Keymap:PKeymapEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Callback}
 if not Assigned(Callback) then Exit;

 {Acquire Lock}
 if CriticalSectionLock(KeymapTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Keymap}
    Keymap:=KeymapTable;
    while Keymap <> nil do
     begin
      {Check State}
      if Keymap.Signature = KEYMAP_SIGNATURE then
       begin
        if Callback(TKeymapHandle(Keymap),Data) <> ERROR_SUCCESS then Exit;
       end;
       
      {Get Next}
      Keymap:=Keymap.Next;
     end;
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(KeymapTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}
{==============================================================================}
{Keymap Helper Functions}
function KeymapGetCount:LongWord; inline;
{Get the current keymap count}
begin
 {}
 Result:=KeymapTableCount;
end;

{==============================================================================}

function KeymapGetDefault:TKeymapHandle; inline;
{Get the current default keymap}
begin
 {}
 Result:=KeymapDefault;
end;

{==============================================================================}

function KeymapCheck(Keymap:PKeymapEntry):PKeymapEntry;
{Check if the supplied Keymap is in the Keymap table}
var
 Current:PKeymapEntry;
begin
 {}
 Result:=nil;
 
 {Check Keymap}
 if Keymap = nil then Exit;
 if Keymap.Signature <> KEYMAP_SIGNATURE then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(KeymapTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Keymap}
    Current:=KeymapTable;
    while Current <> nil do
     begin
      {Check Keymap}
      if Current = Keymap then
       begin
        Result:=Keymap;
        Exit;
       end;
      
      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(KeymapTableLock);
   end;
  end;
end;

{==============================================================================}
{==============================================================================}

initialization
 KeymapInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
