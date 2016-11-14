{
Ultibo NTFS utilities unit.

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

 <See NTFS.pas>
 
References
==========

 <See NTFS.pas>
 
NT Filesystem
=============

 <See NTFS.pas>

 Notes: NTFS uses 64 bit cluster values in all cases

        All structures in NTFS are 8 byte aligned
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit NTFSUtils;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,FileSystem,SysUtils,Classes,Unicode,Security,Ultibo,UltiboUtils,UltiboClasses,
     NTFSConst,NTFSTypes;
    
//To Do //Look for:

//Lock
     
//WideString
  //Change to UnicodeString for FPC
     
{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}
     
{==============================================================================}
var
 {NTFS specific variables}
 UpCase:PNTFSUpCaseData = nil; {A default $UpCase table}
     
{==============================================================================}
{NTFS Support Functions}
function NTFSInitUpCase:Boolean;
//To Do //Add NTFSQuitUpCase //Add an UpCaseLock:TMutexHandle as well to protect
function NTFSGetUpCase:PNTFSUpCaseData;

{==============================================================================}
{NTFS Compression Functions}
function NTFSGetUnitUsed(ABuffer:Pointer;ASize,AShift:LongWord):LongWord;

function NTFSCompressUnit(ASource,ADest:Pointer;ASize,AShift,AStart,ACount,ATotal:LongWord):Boolean;
function NTFSDecompressUnit(ASource,ADest:Pointer;ASize,AShift,AStart,ACount,ATotal:LongWord):Boolean;

function NTFSGetBlockShift(ASize:LongWord):LongWord;

function NTFSCompressBlock(ASource,ADest:Pointer;ASize,ATotal:LongWord):Boolean;
function NTFSDecompressBlock(ASource,ADest:Pointer;ASize:LongWord):Boolean;

function NTFSGetTagShiftMask(AOffset:LongWord;var AMask,AShift:Word):Boolean;

{==============================================================================}
{NTFS Encryption Functions}
//function NTFSEncryptBlock    //To Do 
//function NTFSDecryptBlock    //To Do 

{==============================================================================}
{NTFS String Functions}
function NTFSBufferToString(ABuffer:Pointer;AOffset:LongWord;ALength:LongWord):String;
function NTFSStringToBuffer(const AString:String;ABuffer:Pointer;AOffset:LongWord;ALength:LongWord):Boolean;

function NTFSWideBufferToString(ABuffer:Pointer;AOffset:LongWord;ALength:LongWord):String;
function NTFSStringToWideBuffer(const AString:String;ABuffer:Pointer;AOffset:LongWord;ALength:LongWord):Boolean;

//function NTFSWideBufferToWideString   //To Do
//function NTFSWideStringToWideBuffer   //To Do

function NTFSTypeToString(AType:TNTFSType):String;

{==============================================================================}
{NTFS Hash Functions}
function NTFSGenerateNameHash(AName:PWideChar;ASize:Integer;AUpCase:PNTFSUpCaseData):LongWord;
function NTFSGenerateSecurityHash(ADescriptor:Pointer;ASize:LongWord):LongWord;

{==============================================================================}
{NTFS Sid Functions}
function NTFSCreateDefaultSid(AType:LongWord;var ACreated:Pointer;AVersion:Word):Boolean;
function NTFSDestroyDefaultSid(ASid:Pointer;AVersion:Word):Boolean;

{==============================================================================}
{NTFS Descriptor Functions}
function NTFSCreateDefaultDescriptor(AType:LongWord;var ACreated:Pointer;AVersion:Word):Boolean;

function NTFSCreateInheritedDescriptor(AParent:Pointer;var ACreated:Pointer;AVersion:Word):Boolean;
function NTFSCreateMergedDescriptor(AParent,AChild:Pointer;var ACreated:Pointer;AVersion:Word):Boolean;

function NTFSDestroyDefaultDescriptor(ADescriptor:Pointer;AVersion:Word):Boolean;

function NTFSDestroyInheritedDescriptor(ADescriptor:Pointer;AVersion:Word):Boolean;
function NTFSDestroyMergedDescriptor(ADescriptor:Pointer;AVersion:Word):Boolean;

{==============================================================================}
{NTFS Conversion Functions}
function NTFSAttributeNameToStreamName(AType:LongWord;const AName:String):String;
function NTFSStreamNameToAttributeName(AType:LongWord;const AName:String):String;

{==============================================================================}
{NTFS Rounding Functions}
function NTFSRoundWordTo8Bytes(AValue:Word):Word;
function NTFSRoundWordTo16Bytes(AValue:Word):Word;
function NTFSRoundWordTo512Bytes(AValue:Word):Word;

function NTFSRoundLongWordTo8Bytes(AValue:LongWord):LongWord;
function NTFSRoundLongWordTo16Bytes(AValue:LongWord):LongWord;
function NTFSRoundLongWordTo512Bytes(AValue:LongWord):LongWord;

function NTFSRoundQuadWordTo8Bytes(const AValue:Int64):Int64;
function NTFSRoundQuadWordTo16Bytes(const AValue:Int64):Int64;
function NTFSRoundQuadWordTo512Bytes(const AValue:Int64):Int64;

function NTFSRoundWordToUnitSize(AValue:Word;AShift,ASize:LongWord):Word;
function NTFSRoundLongWordToUnitSize(AValue,AShift,ASize:LongWord):LongWord;
function NTFSRoundQuadWordToUnitSize(const AValue:Int64;AShift,ASize:LongWord):Int64;

function NTFSRoundWordToClusterSize(AValue:Word;AShift,ASize:LongWord):Word;
function NTFSRoundLongWordToClusterSize(AValue,AShift,ASize:LongWord):LongWord;
function NTFSRoundQuadWordToClusterSize(const AValue:Int64;AShift,ASize:LongWord):Int64;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{NTFS Support Functions}
function NTFSInitUpCase:Boolean;
var
 Count:LongWord;
 Offset:LongWord;
begin
 {}
 //To Do //Lock
 
 Result:=False;
 try
  if UpCase = nil then
   begin
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSInitUpCase - Initializing Default Table');
    {$ENDIF}
    
    {Allocate memory}
    UpCase:=GetMem(ntfsFileSizeUpCase);
    if UpCase = nil then Exit;
    
    {Create default table}
    Count:=0;
    Offset:=0;
    while Offset < ntfsFileSizeUpCase do
     begin
      PWord(LongWord(UpCase) + Offset)^:=Count;
      Inc(Count);
      Inc(Offset,2);
     end;
    
    {Initialize default values}
    for Count:=0 to ntfsMaxUpcaseConvert do
     begin
      Offset:=(ntfsUpcaseConverts[Count].Count shl 1);
      PWord(LongWord(UpCase) + Offset)^:=ntfsUpcaseConverts[Count].Value;
     end;
     
    Result:=True;
   end
  else
   begin
    Result:=True;
   end;
 except
  {}
 end;
end;

{==============================================================================}

function NTFSGetUpCase:PNTFSUpCaseData;
begin
 {}
 Result:=nil;
 if NTFSInitUpCase then Result:=UpCase;
end;

{==============================================================================}
{==============================================================================}
{NTFS Compression Functions}
function NTFSGetUnitUsed(ABuffer:Pointer;ASize,AShift:LongWord):LongWord;
{Get the actual size consumed by the compressed data in the unit}
{Size is the size of a cluster}
{Shift is the cluster to compression unit shift count (Usually 4)}
{Note: Buffer must always be a full compression unit in size}
{Note: Caller must have confirmed that the unit is compressed}
var
 Size:LongWord;     {Size of the compression unit}
 Offset:LongWord;   {Current offset into the buffer}

 Length:Word;       {Length of the current block}
begin
 {}
 Result:=0;
 if ASize = 0 then Exit;
 if AShift = 0 then Exit;
 if ABuffer = nil then Exit;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSGetUnitUsed - Size = ' + IntToStr(ASize) + ' Shift = ' + IntToStr(AShift));
 {$ENDIF}
 {Get Size}
 Size:=(ASize shl AShift);
 {Get Offset}
 Offset:=0;
 {Get Length}
 Length:=Word(Pointer(LongWord(ABuffer) + Offset)^);
 while Length <> 0 do
  begin
   Inc(Offset,(Length and ntfsCompressionLengthMask) + 3);
   Inc(Result,(Length and ntfsCompressionLengthMask) + 3);
   if Offset >= Size then Break;
   {Get Length}
   Length:=Word(Pointer(LongWord(ABuffer) + Offset)^);
  end;
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSGetUnitUsed - Size = ' + IntToStr(Size) + ' Used = ' + IntToStr(Result));
 {$ENDIF}
end;

{==============================================================================}

function NTFSCompressUnit(ASource,ADest:Pointer;ASize,AShift,AStart,ACount,ATotal:LongWord):Boolean;
{Compress a unit of data from source to dest buffers}
{Size is the size of a cluster}
{Shift is the cluster to compression unit shift count (Usually 4)}
{Start is the starting cluster for the compression (zero to start at the first cluster)}
{Count is the number of clusters to be compressed}
{Total is the number of bytes to be compressed (from start) (may be less than a full unit)}
{Note: Source buffer contains the uncompressed data on entry}
{      Dest buffer will contain the compressed data on exit}
{Note: Source buffer must always be a full compression unit in size}
{      Dest buffer must always be a full compression unit in size}

{Note: Compress block will detect an uncompressable block as the final length after compression will}
{      be greater than 4096 in length. If compress block reaches the end of the dest buffer then it}
{      will fail the compress block and compress unit and the whole unit will be considered as}
{      uncompressable. If the data available for the block is less then 4096 then it may grow}
{      after compression but will not be marked as uncompressed unless it exceeds 4096 bytes}
var
 Size:LongWord;      {Size of the compression unit}
 Shift:LongWord;     {Block to cluster shift count}
 Start:LongWord;     {Start block for compression}
 Count:LongWord;     {Block count for compression}

 Block:LongWord;     {Current block in the dest buffer}
 Remain:LongWord;    {Blocks remaining to be compressed}
 Available:LongWord; {Bytes remaining to be compressed}

 Dest:LongWord;      {Current offset into the dest buffer}
 Source:LongWord;    {Current offset into the source buffer}

 Length:Word;        {Length of the current block}
begin
 {}
 Result:=False;
 if ASize = 0 then Exit;
 if AShift = 0 then Exit;
 if ACount = 0 then Exit;
 if ATotal = 0 then Exit;
 if ADest = nil then Exit;
 if ASource = nil then Exit;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSCompressUnit - Size = ' + IntToStr(ASize) + ' Shift = ' + IntToStr(AShift) + ' Start = ' + IntToStr(AStart) + ' Count = ' + IntToStr(ACount) + ' Total = ' + IntToStr(ATotal));
 {$ENDIF}
 {Get Size}
 Size:=(ASize shl AShift);
 {Get Shift}
 Shift:=NTFSGetBlockShift(ASize);
 {Get Start}
 if ASize < ntfsCompressionBlockSize then Start:=(AStart shr Shift) else Start:=(AStart shl Shift);
 {Get Count}
 if ASize < ntfsCompressionBlockSize then Count:=(ACount shr Shift) else Count:=(ACount shl Shift);
 if Count = 0 then Inc(Count);
 if (Start > 0) and (Count < ((1 shl AShift) - Start)) then Exit; {Must compress from Start to end of unit}
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSCompressUnit - Size = ' + IntToStr(Size) + ' Shift = ' + IntToStr(Shift) + ' Start = ' + IntToStr(Start) + ' Count = ' + IntToStr(Count) + ' (Calculated)');
 {$ENDIF}
 {Get Position}
 Dest:=0;
 Source:=0;
 Block:=0;
 Remain:=Count;
 Available:=ATotal;
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSCompressUnit - Source = ' + IntToStr(Source) + ' Dest = ' + IntToStr(Dest) + ' Block = ' + IntToStr(Block) + ' Remain = ' + IntToStr(Remain) + ' Available = ' + IntToStr(Available));
 {$ENDIF}
 while Remain > 0 do
  begin
   {Check Offsets}
   if Dest >= Size then Exit;
   if Source >= Size then Exit;
   {Check Block}
   if (Block >= Start) and (Block < (Start + Count)) then
    begin
     {Compress Block}
     if not NTFSCompressBlock(Pointer(LongWord(ASource) + Source),Pointer(LongWord(ADest) + Dest),Size - Dest,MinEx(ntfsCompressionBlockSize,Available)) then Exit;
     {Get Length}
     Length:=Word(Pointer(LongWord(ADest) + Dest)^);
     if Length = 0 then Exit;
     {Update Position}
     Inc(Source,ntfsCompressionBlockSize);
     Inc(Dest,(Length and ntfsCompressionLengthMask) + 3);
     Inc(Block);
     Dec(Remain);
     Dec(Available,MinEx(ntfsCompressionBlockSize,Available));
    end
   else
    begin
     {Get Length}
     Length:=Word(Pointer(LongWord(ADest) + Dest)^);
     if Length = 0 then Exit;
     {Update Position}
     Inc(Source,ntfsCompressionBlockSize);
     Inc(Dest,(Length and ntfsCompressionLengthMask) + 3);
     Inc(Block);
    end;
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSCompressUnit - Source = ' + IntToStr(Source) + ' Dest = ' + IntToStr(Dest) + ' Block = ' + IntToStr(Block) + ' Remain = ' + IntToStr(Remain) + ' Available = ' + IntToStr(Available));
   {$ENDIF}
   if Available = 0 then Break; {Break to allow success}
  end;
 {Check Offset}
 if Dest >= Size then Exit;
 {Check Used}
 if (Size - Dest) < ASize then Exit; {Does not save at least one cluster, store as uncompressed}
 {Zero Remaining}
 FillChar(Pointer(LongWord(ADest) + Dest)^,(Size - Dest),0);
 Result:=True;
end;

{==============================================================================}

function NTFSDecompressUnit(ASource,ADest:Pointer;ASize,AShift,AStart,ACount,ATotal:LongWord):Boolean;
{Decompress a unit (or partial unit) of data from source to dest buffers}
{Size is the size of a cluster}
{Shift is the cluster to compression unit shift count (Usually 4)}
{Start is the starting cluster for the decompression (zero to start at the first cluster)}
{Count is the number of blocks to be decompressed}
{Total is the number of bytes to be decompressed (from start) (may be less than a full unit)}
{Note: Source buffer contains the compressed data on entry}
{      Dest buffer will contain the uncompressed data on exit}
{Note: Source buffer must always be a full compression unit in size}
{      Dest buffer must always be a full compression unit in size}
{Note: Caller must have confirmed that the unit is compressed}
var
 Size:LongWord;      {Size of the compression unit}
 Shift:LongWord;     {Block to cluster shift count}
 Start:LongWord;     {Start block for decompression}
 Count:LongWord;     {Block count for decompression}

 Block:LongWord;     {Current block in the source buffer}
 Remain:LongWord;    {Blocks remaining to be decompressed}
 Available:LongWord; {Bytes remaining to be decompressed}

 Dest:LongWord;      {Current offset into the dest buffer}
 Source:LongWord;    {Current offset into the source buffer}

 Length:Word;        {Length of the current block}
begin
 {}
 Result:=False;
 if ASize = 0 then Exit;
 if AShift = 0 then Exit;
 if ACount = 0 then Exit;
 if ADest = nil then Exit;
 if ASource = nil then Exit;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSDecompressUnit - Size = ' + IntToStr(ASize) + ' Shift = ' + IntToStr(AShift) + ' Start = ' + IntToStr(AStart) + ' Count = ' + IntToStr(ACount) + ' Total = ' + IntToStr(ATotal));
 {$ENDIF}
 {Get Size}
 Size:=(ASize shl AShift);
 {Get Shift}
 Shift:=NTFSGetBlockShift(ASize);
 {Get Start}
 if ASize < ntfsCompressionBlockSize then Start:=(AStart shr Shift) else Start:=(AStart shl Shift);
 {Get Count}
 if ASize < ntfsCompressionBlockSize then Count:=(ACount shr Shift) else Count:=(ACount shl Shift);
 if Count = 0 then Inc(Count);
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSDecompressUnit - Size = ' + IntToStr(Size) + ' Shift = ' + IntToStr(Shift) + ' Start = ' + IntToStr(Start) + ' Count = ' + IntToStr(Count) + ' (Calculated)');
 {$ENDIF}
 {Get Position}
 Dest:=0;
 Source:=0;
 Block:=0;
 Remain:=Count;
 Available:=ATotal;
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSDecompressUnit - Dest = ' + IntToStr(Dest) + ' Source = ' + IntToStr(Source) + ' Block = ' + IntToStr(Block) + ' Remain = ' + IntToStr(Remain) + ' Available = ' + IntToStr(Available));
 {$ENDIF}
 while Remain > 0 do
  begin
   {Check Offsets}
   if Dest >= Size then Exit;
   if Source >= Size then Exit;
   {Get Length}
   Length:=Word(Pointer(LongWord(ASource) + Source)^);
   if Length = 0 then Exit;
   {Check Block}
   if (Block >= Start) and (Block < (Start + Count)) then
    begin
     {Decompress Block}
     if not NTFSDecompressBlock(Pointer(LongWord(ASource) + Source),Pointer(LongWord(ADest) + Dest),Size - Source) then Exit;
     {Update Position}
     Inc(Dest,ntfsCompressionBlockSize);
     Inc(Source,(Length and ntfsCompressionLengthMask) + 3);
     Inc(Block);
     Dec(Remain);
     Dec(Available,MinEx(ntfsCompressionBlockSize,Available));
    end
   else
    begin
     {Update Position}
     Inc(Dest,ntfsCompressionBlockSize);
     Inc(Source,(Length and ntfsCompressionLengthMask) + 3);
     Inc(Block);
    end;
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSDecompressUnit - Dest = ' + IntToStr(Dest) + ' Source = ' + IntToStr(Source) + ' Block = ' + IntToStr(Block) + ' Remain = ' + IntToStr(Remain) + ' Available = ' + IntToStr(Available));
   {$ENDIF}
   if Available = 0 then Break; {Break to allow success}
  end;
 {Cannot zero remaining (Decompressed size not known)}
 Result:=True;
end;

{==============================================================================}

function NTFSGetBlockShift(ASize:LongWord):LongWord;
{Get the shift count for cluster to 4k block conversion}
{Size is the size of a cluster}
begin
 {}
 Result:=0;
 if ASize = 0 then Exit;

 {Check Cluster equal to Block}
 if ASize = ntfsCompressionBlockSize then Exit;
 {Check Cluster smaller than Block}
 if ASize < ntfsCompressionBlockSize then
  begin
   {Cluster Smaller}
   while (ASize shl Result) < ntfsCompressionBlockSize do
    begin
     Inc(Result);
    end;
  end
 else
  begin
   {Cluster Larger}
   while (ASize shr Result) > ntfsCompressionBlockSize do
    begin
     Inc(Result);
    end;
  end;
end;

{==============================================================================}

function NTFSCompressBlock(ASource,ADest:Pointer;ASize,ATotal:LongWord):Boolean;
{Compress a 4k block of data from source to dest buffers}
{Size is the available bytes in the dest buffer}
{Total is the number of bytes to be compressed (may be less than a full block)}
{Note: Source buffer contains the uncompressed data on entry}
{      Dest buffer will contain the compressed data on exit}
{Note: Source buffer must have at least one 4k block available}

{Note: Compress block will detect an uncompressable block as the final length after compression will}
{      be greater than 4096 in length. If compress block reaches the end of the dest buffer then it}
{      will fail the compress block and compress unit and the whole unit will be considered as}
{      uncompressable. If the data available for the block is less then 4096 then it may grow}
{      after compression but will not be marked as uncompressed unless it exceeds 4096 bytes}
var
 Tag:Word;           {The encoded offset and size for the current tag}
 Tags:Byte;          {The tags byte for the current group}
 Count:Byte;         {Counter for the current tag group}
 Length:Word;        {The length of the compressed block}
 Origin:LongWord;    {The offset of the current tags byte in the dest buffer}

 Size:Word;          {The actual size of the current tag}
 Offset:Word;        {The actual offset of the current tag}
 {Offset:SmallInt;}  {The actual offset of the current tag}

 MaxSize:Word;       {The maximum size of the current tag}
 MaxOffset:Word;     {The maximum offset of the current tag}
 {MaxOffset:SmallInt}{The maximum offset of the current tag}

 Mask:Word;          {The tag encode mask for the current position}
 Shift:Word;         {The tag encode shift for the current position}

 Dest:LongWord;      {Current offset into the dest buffer}
 Source:LongWord;    {Current offset into the source buffer}

 Next:Byte;                   {Next value in the source}
 Value:Byte;                  {Current value in the source}
 Current:LongWord;            {Current offset of current value}
 Previous:LongWord;           {Previous offset of current value}
 Table:array[0..255] of Word; {Table of previously seen values}
begin
 {}
 Result:=False;
 if ASize = 0 then Exit;
 if ATotal = 0 then Exit;
 if ADest = nil then Exit;
 if ASource = nil then Exit;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSCompressBlock - Size = ' + IntToStr(ASize) + ' Total = ' + IntToStr(ATotal));
 {$ENDIF}
 {Fill Table}
 FillChar(Table,SizeOf(Table),ntfsCompressionTableInit);
 {Get Start}
 Dest:=0;
 Source:=0;
 {Get Length}
 Length:=0;
 Inc(Dest,2);   {SizeOf(Word)}
 Inc(Length,2); {SizeOf(Word)}
 while Source < MinEx(ntfsCompressionBlockSize,ATotal) do
  begin
   {Get Tags}
   Tags:=0;
   Origin:=Dest;
   Inc(Dest);   {SizeOf(Byte)}
   Inc(Length); {SizeOf(Byte)}
   {Check Offset}
   if Dest >= ASize then Exit;
   {Get Count}
   Count:=1;
   while Count <= 8 do
    begin
     {Get Value}
     Value:=Byte(Pointer(LongWord(ASource) + Source)^);
     {Get Current}
     Current:=Source;
     {Get Previous}
     Previous:=Table[Value];
     if Previous <> ntfsCompressionTableUnused then
      begin
       {Seen Before}
       {Check Next Values}
       if ((Current + 2) < MinEx(ntfsCompressionBlockSize,ATotal)) and (Byte(Pointer(LongWord(ASource) + Current + 1)^) = Byte(Pointer(LongWord(ASource) + Previous + 1)^)) and (Byte(Pointer(LongWord(ASource) + Current + 2)^) = Byte(Pointer(LongWord(ASource) + Previous + 2)^)) then
        begin
         {Matched}
         {Get Mask and Shift}
         NTFSGetTagShiftMask(Source,Mask,Shift);
         {Get Max Size and Offset}
         MaxSize:=(Mask + 3);
         MaxOffset:=((not(Mask) shr Shift) + 1);
         {Get Size and Offset}
         Size:=0; {Start at 0 not 3 so that Table and Position are updated}
         Offset:=Current - Previous;
         if Offset <= MaxOffset then
          begin
           {Below Maximum}
           while (Size < MaxSize) and (Source < MinEx(ntfsCompressionBlockSize,ATotal)) do
            begin
             {Get Next Value}
             Next:=Byte(Pointer(LongWord(ASource) + Current + Size)^);
             {Check Next Value}
             if Next = Byte(Pointer(LongWord(ASource) + Previous + Size)^) then
              begin
               {Update Table}
               Table[Next]:=Current + Size;
               {Update Size}
               Inc(Size);   {SizeOf(Byte)}
               {Update Position}
               Inc(Source); {SizeOf(Byte)}
              end
             else
              begin
               Break;
              end;
            end;
           {Set Tag}
           Tag:=((Offset - 1) shl Shift) or ((Size - 3) and Mask);
           Word(Pointer(LongWord(ADest) + Dest)^):=Tag;
           {$IFDEF NTFS_DEBUG}
           if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSCompressBlock - Current = ' + IntToHex(Current,8) + ' Previous = ' + IntToHex(Previous,8));
           if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSCompressBlock - Tag = ' + IntToHex(Tag,4) + ' Mask = ' + IntToHex(Mask,4) + ' Shift = ' + IntToStr(Shift));
           if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSCompressBlock - Offset = ' + IntToStr(Offset) + ' Size = ' + IntToStr(Size));
           {$ENDIF}
           {Update Tags}
           Tags:=(Tags or ntfsCompressionTags[Count]);
           {Update Position}
           Inc(Count);       {SizeOf(Byte)}
           Inc(Dest,2);      {SizeOf(Word)}
           Inc(Length,2);    {SizeOf(Word)}
          end
         else
          begin
           {Above Maximum}
           {Copy Byte}
           Byte(Pointer(LongWord(ADest) + Dest)^):=Value;
           {Update Table}
           Table[Value]:=Current;
           {Update Position}
           Inc(Count);  {SizeOf(Byte)}
           Inc(Dest);   {SizeOf(Byte)}
           Inc(Length); {SizeOf(Byte)}
           Inc(Source); {SizeOf(Byte)}
          end;
        end
       else
        begin
         {Not Matched}
         {Copy Byte}
         Byte(Pointer(LongWord(ADest) + Dest)^):=Value;
         {Update Table}
         Table[Value]:=Current;
         {Update Position}
         Inc(Count);  {SizeOf(Byte)}
         Inc(Dest);   {SizeOf(Byte)}
         Inc(Length); {SizeOf(Byte)}
         Inc(Source); {SizeOf(Byte)}
        end;
      end
     else
      begin
       {Never Seen}
       {Copy Byte}
       Byte(Pointer(LongWord(ADest) + Dest)^):=Value;
       {Update Table}
       Table[Value]:=Current;
       {Update Position}
       Inc(Count);  {SizeOf(Byte)}
       Inc(Dest);   {SizeOf(Byte)}
       Inc(Length); {SizeOf(Byte)}
       Inc(Source); {SizeOf(Byte)}
      end;
     {Check Offset}
     if Dest >= ASize then Exit;
     if Source >= MinEx(ntfsCompressionBlockSize,ATotal) then Break;
    end;
   {Set Tags}
   Byte(Pointer(LongWord(ADest) + Origin)^):=Tags;
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSCompressBlock - Dest = ' + IntToHex(Dest,8) + ' Origin = ' + IntToHex(Origin,8) + ' Tags = ' + IntToHex(Tags,2));
   {$ENDIF}
  end;
 {Check Length}
 if (Length - 2) > ntfsCompressionBlockSize then
  begin
   {Uncompressed Block}
   {Set Length}
   Length:=MinEx(ntfsCompressionBlockSize,ATotal);
   Word(ADest^):=(Length - 1);
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSCompressBlock - Length = ' + IntToStr(Length) + ' (Uncompressed Block)');
   {$ENDIF}
   {Copy Block}
   System.Move(Pointer(PtrUInt(ASource))^,Pointer(PtrUInt(ADest) + 2)^,MinEx(ntfsCompressionBlockSize,ATotal));
  end
 else
  begin
   {Compressed Block}
   Word(ADest^):=((Length - 3) or ntfsCompressionMarkerMask);
  end;
 Result:=True;
end;

{==============================================================================}

function NTFSDecompressBlock(ASource,ADest:Pointer;ASize:LongWord):Boolean;
{Decompress a 4k block of data from source to dest buffers}
{Size is the available bytes in the source buffer}
{Note: Source buffer contains the compressed data on entry}
{      Dest buffer will contain the uncompressed data on exit}
{Note: Dest buffer must have at least one 4k block available}
var
 Tag:Word;           {The encoded offset and size for the current tag}
 Tags:Byte;          {The tags byte for the current group}
 Count:Byte;
 Length:Word;        {The length of the compressed block}

 Size:Word;          {The decoded size of the current tag}
 Offset:Word;        {The decoded offset of the current tag}
 {Offset:SmallInt;}  {The decoded offset of the current tag}

 Mask:Word;          {The tag decode mask for the current position}
 Shift:Word;         {The tag decode shift for the current position}

 Dest:LongWord;      {Current offset into the dest buffer}
 Source:LongWord;    {Current offset into the source buffer}
begin
 {}
 Result:=False;
 if ASize = 0 then Exit;
 if ADest = nil then Exit;
 if ASource = nil then Exit;

 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSDecompressBlock - Size = ' + IntToStr(ASize));
 {$ENDIF}
 {Get Start}
 Dest:=0;
 Source:=0;
 {Get Length}
 Length:=Word(Pointer(LongWord(ASource) + Source)^);
 if Length = 0 then Exit;
 Inc(Source,2); {SizeOf(Word)}
 {Check Length}
 if (Length and ntfsCompressionMarkerTest) = ntfsCompressionMarkerTest then
  begin
   {Block is Compressed}
   Length:=(Length and ntfsCompressionLengthMask) + 3;
   if Length > ASize then Exit;
   while Source < Length do
    begin
     {Check Offset}
     if Source >= ASize then Exit;
     {Get Tags}
     Tags:=Byte(Pointer(LongWord(ASource) + Source)^);
     Inc(Source); {SizeOf(Byte)}
     if Tags <> 0 then
      begin
       {Group is Encoded}
       {$IFDEF NTFS_DEBUG}
       if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSDecompressBlock - Source = ' + IntToHex(Source,8) + ' Tags = ' + IntToHex(Tags,2));
       {$ENDIF}
       for Count:=1 to 8 do
        begin
         if (Tags and ntfsCompressionTags[Count]) = ntfsCompressionTags[Count] then
          begin
           {Encoded Tag}
           {Get Tag}
           Tag:=Word(Pointer(LongWord(ASource) + Source)^);
           {Get Mask and Shift}
           NTFSGetTagShiftMask(Dest,Mask,Shift);
           {$IFDEF NTFS_DEBUG}
           if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSDecompressBlock - Tag = ' + IntToHex(Tag,4) + ' Mask = ' + IntToHex(Mask,4) + ' Shift = ' + IntToStr(Shift));
           {$ENDIF}
           {Get Size and Offset}
           Size:=(Tag and Mask) + 3;
           Offset:=((Tag and not(Mask)) shr Shift) + 1;  {Treat offset as positive and add one}
           {Offset:=not((Tag and not(Mask)) shr Shift);} {Treat offset as negative using not}
           {$IFDEF NTFS_DEBUG}
           if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSDecompressBlock - Offset = ' + IntToStr(Offset) + ' Size = ' + IntToStr(Size));
           {$ENDIF}
           {Copy Bytes}
           while Size > 0 do
            begin
             Byte(Pointer(PtrUInt(ADest) + Dest)^):=Byte(Pointer(PtrUInt(ADest) + PtrUInt(Dest - Offset))^);
             {Byte(Pointer(LongWord(ADest) + Dest)^):=Byte(Pointer(LongWord(ADest) + LongWord(LongInt(Dest) + Offset))^);}
             Dec(Size);   {SizeOf(Byte)}
             Inc(Dest);   {SizeOf(Byte)}
            end;
           {Update Position}
           Inc(Source,2); {SizeOf(Word)}
           if Source >= ASize then Exit;
           if Source >= Length then Break;
          end
         else
          begin
           {Normal Tag}
           {Copy Byte}
           Byte(Pointer(LongWord(ADest) + Dest)^):=Byte(Pointer(LongWord(ASource) + Source)^);
           {Update Position}
           Inc(Dest);   {SizeOf(Byte)}
           Inc(Source); {SizeOf(Byte)}
           if Source >= ASize then Exit;
           if Source >= Length then Break;
          end;
        end;
      end
     else
      begin
       {Group is not Encoded}
       {$IFDEF NTFS_DEBUG}
       if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSDecompressBlock - Source = ' + IntToHex(Source,8) + ' (Uncompressed Group)');
       {$ENDIF}
       {Copy Group}
       System.Move(Pointer(LongWord(ASource) + Source)^,Pointer(LongWord(ADest) + Dest)^,8);
       {Update Position}
       Inc(Dest,8);    {SizeOf(Byte) * 8}
       Inc(Source,8);  {SizeOf(Byte) * 8}
      end;
    end;
  end
 else
  begin
   {Block is not Compressed}
   Length:=(Length and ntfsCompressionLengthMask) + 1;
   if Length > ASize then Exit;
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSDecompressBlock - Length = ' + IntToStr(Length) + ' (Uncompressed Block)');
   {$ENDIF}
   {Copy Block}
   System.Move(Pointer(LongWord(ASource) + Source)^,Pointer(LongWord(ADest) + Dest)^,Length);
  end;
 Result:=True;
end;

{==============================================================================}

function NTFSGetTagShiftMask(AOffset:LongWord;var AMask,AShift:Word):Boolean;
{Get the Mask bits and Shift count for Tag encode and decode based on the offset}
{Offset is the offset into the uncompressed data at the point of the tag}
var
 Offset:LongWord;
begin
 {}
 Result:=False;
 if AOffset = 0 then Exit;

 AMask:=ntfsCompressionTagMask;
 AShift:=ntfsCompressionTagShift;
 Offset:=AOffset - 1;
 while Offset >= $10 do
  begin
   Offset:=(Offset shr 1);
   AMask:=(AMask shr 1);
   Dec(AShift);
  end;
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{NTFS Encryption Functions}

{==============================================================================}
{==============================================================================}
{NTFS String Functions}
function NTFSBufferToString(ABuffer:Pointer;AOffset:LongWord;ALength:LongWord):String;
begin
 {}
 Result:=ntfsBlankName;
 if ABuffer = nil then Exit;
 {Check Length}
 if ALength > 0 then
  begin
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSBufferToString - Length = ' + IntToStr(ALength));
   {$ENDIF}
   SetString(Result,nil,ALength); {Length does not include null terminator}
   Unicode.OemToCharBuff(PChar(LongWord(ABuffer) + AOffset),PChar(Result),ALength);
  end;
end;

{==============================================================================}

function NTFSStringToBuffer(const AString:String;ABuffer:Pointer;AOffset:LongWord;ALength:LongWord):Boolean;
begin
 {}
 Result:=False;
 if ABuffer = nil then Exit;
 {Check Length}
 if ALength > 0 then
  begin
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSStringToBuffer - Length = ' + IntToStr(ALength));
   {$ENDIF}
   FillChar(Pointer(LongWord(ABuffer) + AOffset)^,ALength,0);
   if Length(AString) > 0 then
    begin
     Unicode.CharToOemBuff(PChar(AString),PChar(LongWord(ABuffer) + AOffset),ALength);
    end;
   {Result:=True;} {Modified 29/4/2009 to support 0 length names}
  end;
 Result:=True;
end;

{==============================================================================}

function NTFSWideBufferToString(ABuffer:Pointer;AOffset:LongWord;ALength:LongWord):String;
var
 Count:LongWord;
begin
 {}
 Result:=ntfsBlankName;
 if ABuffer = nil then Exit;
 {Check Length}
 if ALength > 0 then
  begin
   {Check Count}
   Count:=Unicode.WideCharToMultiByte(CP_ACP,0,PWideChar(LongWord(ABuffer) + AOffset),ALength,nil,0,nil,nil);
   {$IFDEF NTFS_DEBUG}
   if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSWideBufferToString - Length = ' + IntToStr(ALength) + ' Count = ' + IntToStr(Count));
   {$ENDIF}
   if Count <= ALength then
    begin
     SetString(Result,nil,Count); {Count does not include null terminator}
     Unicode.WideCharToMultiByte(CP_ACP,0,PWideChar(LongWord(ABuffer) + AOffset),ALength,PChar(Result),Count,nil,nil);
    end;
  end;
end;

{==============================================================================}

function NTFSStringToWideBuffer(const AString:String;ABuffer:Pointer;AOffset:LongWord;ALength:LongWord):Boolean;
var
 Size:LongWord;
 Count:LongWord;
begin
 {}
 Result:=False;
 if ABuffer = nil then Exit;
 {Check Length}
 if ALength > 0 then
  begin
   {Get Size}
   Size:=(ALength shl 1);
   FillChar(Pointer(LongWord(ABuffer) + AOffset)^,Size,0);
   if Length(AString) > 0 then
    begin
     {Check Count}
     Count:=Unicode.MultiByteToWideChar(CP_ACP,0,PChar(AString),Length(AString),nil,0);
     {$IFDEF NTFS_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSStringToWideBuffer - Size = ' + IntToStr(Size) + ' Length = ' + IntToStr(ALength) + ' Count = ' + IntToStr(Count));
     {$ENDIF}
     if Count > ALength then Exit;
     if Unicode.MultiByteToWideChar(CP_ACP,0,PChar(AString),Length(AString),PWideChar(LongWord(ABuffer) + AOffset),ALength) = 0 then Exit;
    end;
   {Result:=True;} {Modified 29/4/2009 to support 0 length names}
  end;
 Result:=True;
end;

{==============================================================================}

function NTFSTypeToString(AType:TNTFSType):String;
begin
 {}
 Result:='ntNONE';
 
 {Check Type}
 case AType of
  ntNTFS12:Result:='ntNTFS12';
  ntNTFS30:Result:='ntNTFS30';
  ntNTFS31:Result:='ntNTFS31';
 end;
end;

{==============================================================================}
{==============================================================================}
{NTFS Hash Functions}
function NTFSGenerateNameHash(AName:PWideChar;ASize:Integer;AUpCase:PNTFSUpCaseData):LongWord;
{Note: Size is the length of the Name in characters (WideChar or Word)}
{      UpCase is the UpCase conversion table from NTFS volume or defaults}
var
 Count:Integer;
 Value:LongWord;
 Offset:LongWord;
begin
 {}
 Result:=0;
 if ASize > 0 then
  begin
   Offset:=0;
   for Count:=1 to ASize do
    begin
     Value:=AUpCase.Data[Word(Pointer(LongWord(AName) + Offset)^)];
     Result:=Result + ((Value + 1) * (LongWord(Count) + 257));
     Inc(Offset,2);
    end;
  end;
end;

{==============================================================================}

function NTFSGenerateSecurityHash(ADescriptor:Pointer;ASize:LongWord):LongWord;
var
 Hash:LongWord;
 Size:LongWord;
 Count:LongWord;
 Descriptor:Pointer;
begin
 {}
 Result:=0;
 try
  if ADescriptor = nil then Exit;
  if ASize < SECURITY_DESCRIPTOR_MIN_LENGTH then Exit;
  
  {Check Revision}
  if PNTFSSecurityData(ADescriptor).Reserved1 <> 0 then Exit;
  if PNTFSSecurityData(ADescriptor).Revision <> SECURITY_DESCRIPTOR_REVISION1 then Exit;
  
  {Check Control}
  if (PNTFSSecurityData(ADescriptor).Control and SE_SELF_RELATIVE) = SE_SELF_RELATIVE then
   begin
    {Relative Descriptor}
    Hash:=0;
    Count:=0;
    while Count < ASize do
     begin
      Hash:=LongWord(Pointer(LongWord(ADescriptor) + Count)^) + Rol32(Hash,3);
      Inc(Count,4);
     end;
     
    Result:=Hash;
   end
  else
   begin
    {Absolute Descriptor}
    Hash:=0;
    Size:=ASize;
    Descriptor:=AllocMem(Size);
    if Descriptor = nil then Exit;
    try
     if Security.MakeSelfRelativeSD(ADescriptor,Descriptor,Size) then
      begin
       Count:=0;
       while Count < Size do
        begin
         Hash:=LongWord(Pointer(LongWord(Descriptor) + Count)^) + Rol32(Hash,3);
         Inc(Count,4);
        end;
       
       Result:=Hash;
      end;
    finally
     FreeMem(Descriptor);
    end;
   end;
 except
  {}
 end;
end;

{==============================================================================}
{==============================================================================}
{NTFS Sid Functions}
function NTFSCreateDefaultSid(AType:LongWord;var ACreated:Pointer;AVersion:Word):Boolean;
var
 Sid:PSID;
 SidSize:LongWord;

 DefaultSid:PNTFSDefaultSid;
begin
 {}
 Result:=False;
 
 if ACreated <> nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSCreateDefaultSid - Type = ' + IntToStr(AType) + ' Version = ' + IntToStr(AVersion));
 {$ENDIF}
 
 {Check Type}
 DefaultSid:=nil;
 case AType of
  ntfsDefaultSid100:begin
    DefaultSid:=@ntfsDefaultSids[ntfsDefaultSid100];
    
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSCreateDefaultSid - ntfsDefaultSid100');
    {$ENDIF}
   end;
 end;
 if DefaultSid = nil then Exit;
 
 {Create Sid}
 Sid:=AllocMem(SECURITY_MAX_SID_SIZE);
 if Sid = nil then Exit;
 try
  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSCreateDefaultSid - Creating Sid');
  {$ENDIF}
  
  SidSize:=SECURITY_MAX_SID_SIZE;
  if not Security.CreateWellKnownSid(DefaultSid.Sid,nil,Sid,SidSize) then
   begin
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSCreateDefaultSid - CreateWellKnownSid returned ' + IntToHex(GetLastError,8));
    {$ENDIF}
    
    Exit;
   end;
  
  {Copy Sid}
  ACreated:=AllocMem(SidSize);
  if ACreated = nil then Exit;
  try
   if not Security.CopySid(SidSize,ACreated,Sid) then
    begin
     {$IFDEF NTFS_DEBUG}
     if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSCreateDefaultSid - CopySid returned ' + IntToHex(GetLastError,8));
     {$ENDIF}
     
     Exit;
    end;
    
   Result:=True;
  finally
   if not Result then FreeMem(ACreated);
   if not Result then ACreated:=nil;
  end;
 finally
  FreeMem(Sid);
 end;
end;

{==============================================================================}

function NTFSDestroyDefaultSid(ASid:Pointer;AVersion:Word):Boolean;
begin
 {}
 Result:=False;
 if ASid = nil then Exit;
 FreeMem(ASid);
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{NTFS Descriptor Functions}
function NTFSCreateDefaultDescriptor(AType:LongWord;var ACreated:Pointer;AVersion:Word):Boolean;
var
 Acl:PACL;
 Sid:PSID;
 Ace:PAceHeader;

 Count:Integer;
 Offset:LongWord;
 SidSize:LongWord;

 Descriptor:PSecurityDescriptorRelative;
 DefaultDescriptor:PNTFSDefaultDescriptor;
begin
 {}
 Result:=False;
 
 if ACreated <> nil then Exit;
 
 {$IFDEF NTFS_DEBUG}
 if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSCreateDefaultDescriptor - Type = ' + IntToStr(AType) + ' Version = ' + IntToStr(AVersion));
 {$ENDIF}
 
 {Check Type}
 DefaultDescriptor:=nil;
 case AType of
  ntfsDefaultDescriptorVolume:begin  {Also ntfsDefaultDescriptor101}
    DefaultDescriptor:=@ntfsDefaultDescriptors[ntfsDefaultDescriptorVolume];
    
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSCreateDefaultDescriptor - VolumeDefaultDescriptor');
    {$ENDIF}
   end;
  ntfsDefaultDescriptorAttrDef:begin {Also ntfsDefaultDescriptorBoot,ntfsDefaultDescriptor100}
    DefaultDescriptor:=@ntfsDefaultDescriptors[ntfsDefaultDescriptorAttrDef];
    
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSCreateDefaultDescriptor - AttrDefDefaultDescriptor');
    {$ENDIF}
   end;
  ntfsDefaultDescriptorRoot:begin
    DefaultDescriptor:=@ntfsDefaultDescriptors[ntfsDefaultDescriptorRoot];
    
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSCreateDefaultDescriptor - RootDefaultDescriptor');
    {$ENDIF}
   end;
  ntfsDefaultDescriptor102:begin
    DefaultDescriptor:=@ntfsDefaultDescriptors[ntfsDefaultDescriptor102];
    
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSCreateDefaultDescriptor - DefaultDescriptor102');
    {$ENDIF}
   end;
  ntfsDefaultDescriptor103:begin
    DefaultDescriptor:=@ntfsDefaultDescriptors[ntfsDefaultDescriptor103];
    
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSCreateDefaultDescriptor - DefaultDescriptor103');
    {$ENDIF}
   end;
  ntfsDefaultDescriptorFile:begin
    DefaultDescriptor:=@ntfsDefaultDescriptors[ntfsDefaultDescriptorFile];
    
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSCreateDefaultDescriptor - DefaultDescriptorFile');
    {$ENDIF}
   end;
  ntfsDefaultDescriptorFolder:begin
    DefaultDescriptor:=@ntfsDefaultDescriptors[ntfsDefaultDescriptorFolder];
    
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSCreateDefaultDescriptor - DefaultDescriptorFolder');
    {$ENDIF}
   end;
 end;
 if DefaultDescriptor = nil then Exit;
 
 {Create Descriptor}
 ACreated:=AllocMem(DefaultDescriptor.Size);
 if ACreated = nil then Exit;
 try
  {$IFDEF NTFS_DEBUG}
  if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSCreateDefaultDescriptor - Creating Descriptor');
  {$ENDIF}
  
  Descriptor:=PSecurityDescriptorRelative(ACreated);
  Descriptor.Revision:=DefaultDescriptor.Revision;
  Descriptor.Control:=DefaultDescriptor.Control;
  Descriptor.Owner:=DefaultDescriptor.OwnerOffset;
  Descriptor.Group:=DefaultDescriptor.GroupOffset;
  Descriptor.Sacl:=DefaultDescriptor.SaclOffset;
  Descriptor.Dacl:=DefaultDescriptor.DaclOffset;
  
  {Check Owner}
  if DefaultDescriptor.OwnerOffset <> 0 then
   begin
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSCreateDefaultDescriptor - Creating Owner');
    {$ENDIF}
    
    {Get Sid}
    Sid:=PSID(LongWord(Descriptor) + DefaultDescriptor.OwnerOffset);
    SidSize:=SECURITY_MAX_SID_SIZE;
    if not Security.CreateWellKnownSid(DefaultDescriptor.Owner,nil,Sid,SidSize) then
     begin
      {$IFDEF NTFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSCreateDefaultDescriptor - CreateWellKnownSid returned ' + IntToHex(GetLastError,8));
      {$ENDIF}
      
      Exit;
     end;
   end;
   
  {Check Group}
  if DefaultDescriptor.GroupOffset <> 0 then
   begin
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSCreateDefaultDescriptor - Creating Group');
    {$ENDIF}
    
    {Get Sid}
    Sid:=PSID(LongWord(Descriptor) + DefaultDescriptor.GroupOffset);
    SidSize:=SECURITY_MAX_SID_SIZE;
    if not Security.CreateWellKnownSid(DefaultDescriptor.Group,nil,Sid,SidSize) then
     begin
      {$IFDEF NTFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSCreateDefaultDescriptor - CreateWellKnownSid returned ' + IntToHex(GetLastError,8));
      {$ENDIF}
      
      Exit;
     end;
   end;
   
  {Check Sacl}
  if DefaultDescriptor.SaclOffset <> 0 then
   begin
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSCreateDefaultDescriptor - Creating SACL');
    {$ENDIF}
    
    {Get Acl}
    Acl:=PACL(LongWord(Descriptor) + DefaultDescriptor.SaclOffset);
    Acl.AclRevision:=DefaultDescriptor.Sacl.AclRevision;
    Acl.AclSize:=DefaultDescriptor.Sacl.AclSize;
    Acl.AceCount:=DefaultDescriptor.Sacl.AceCount;
    
    {Get Offset}
    Offset:=SizeOf(TACL);
    
    {Check Aces}
    for Count:=0 to DefaultDescriptor.Sacl.AceCount - 1 do
     begin
      {$IFDEF NTFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSCreateDefaultDescriptor - Creating SACL Ace ' + IntToStr(Count));
      {$ENDIF}
      
      {Get Offset}
      if Count > 0 then Inc(Offset,DefaultDescriptor.Sacl.Aces[Count - 1].AceSize);
      
      {Get Ace}
      Ace:=PAceHeader(LongWord(Acl) + Offset);
      Ace.AceType:=DefaultDescriptor.Sacl.Aces[Count].AceType;
      Ace.AceFlags:=DefaultDescriptor.Sacl.Aces[Count].AceFlags;
      Ace.AceSize:=DefaultDescriptor.Sacl.Aces[Count].AceSize;
      PAccessAllowedAce(Ace).Mask:=DefaultDescriptor.Sacl.Aces[Count].Mask;
      
      {Get Sid}
      Sid:=PSID(LongWord(Ace) + SizeOf(TAceHeader) + SizeOf(ACCESS_MASK));
      SidSize:=SECURITY_MAX_SID_SIZE;
      if not Security.CreateWellKnownSid(DefaultDescriptor.Sacl.Aces[Count].Sid,nil,Sid,SidSize) then
       begin
        {$IFDEF NTFS_DEBUG}
        if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSCreateDefaultDescriptor - CreateWellKnownSid returned ' + IntToHex(GetLastError,8));
        {$ENDIF}
        
        Exit;
       end;
     end;
   end;
   
  {Check Dacl}
  if DefaultDescriptor.DaclOffset <> 0 then
   begin
    {$IFDEF NTFS_DEBUG}
    if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSCreateDefaultDescriptor - Creating DACL');
    {$ENDIF}
    
    {Get Acl}
    Acl:=PACL(LongWord(Descriptor) + DefaultDescriptor.DaclOffset);
    Acl.AclRevision:=DefaultDescriptor.Dacl.AclRevision;
    Acl.AclSize:=DefaultDescriptor.Dacl.AclSize;
    Acl.AceCount:=DefaultDescriptor.Dacl.AceCount;
    
    {Get Offset}
    Offset:=SizeOf(TACL);
    
    {Check Aces}
    for Count:=0 to DefaultDescriptor.Dacl.AceCount - 1 do
     begin
      {$IFDEF NTFS_DEBUG}
      if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSCreateDefaultDescriptor - Creating DACL Ace ' + IntToStr(Count));
      {$ENDIF}
      
      {Get Offset}
      if Count > 0 then Inc(Offset,DefaultDescriptor.Dacl.Aces[Count - 1].AceSize);
      
      {Get Ace}
      Ace:=PAceHeader(LongWord(Acl) + Offset);
      Ace.AceType:=DefaultDescriptor.Dacl.Aces[Count].AceType;
      Ace.AceFlags:=DefaultDescriptor.Dacl.Aces[Count].AceFlags;
      Ace.AceSize:=DefaultDescriptor.Dacl.Aces[Count].AceSize;
      PAccessAllowedAce(Ace).Mask:=DefaultDescriptor.Dacl.Aces[Count].Mask;
      
      {Get Sid}
      Sid:=PSID(LongWord(Ace) + SizeOf(TAceHeader) + SizeOf(ACCESS_MASK));
      SidSize:=SECURITY_MAX_SID_SIZE;
      if not Security.CreateWellKnownSid(DefaultDescriptor.Dacl.Aces[Count].Sid,nil,Sid,SidSize) then
       begin
        {$IFDEF NTFS_DEBUG}
        if FILESYS_LOG_ENABLED then FileSysLogDebug('NTFSCreateDefaultDescriptor - CreateWellKnownSid returned ' + IntToHex(GetLastError,8));
        {$ENDIF}
        
        Exit;
       end;
     end;
   end;
   
  Result:=True;
 finally
  if not Result then FreeMem(ACreated);
  if not Result then ACreated:=nil;
 end;
end;

{==============================================================================}

function NTFSCreateInheritedDescriptor(AParent:Pointer;var ACreated:Pointer;AVersion:Word):Boolean;
begin
 {}
 Result:=False;
 case AVersion of
  ntfsNTFS12:begin
    Result:=Security.CreateInheritedSecurityDescriptorNT(AParent,PSecurityDescriptor(ACreated));
   end;
  ntfsNTFS30,ntfsNTFS31:begin
    Result:=Security.CreateInheritedSecurityDescriptor2K(AParent,PSecurityDescriptor(ACreated));
   end;
 end;
end;

{==============================================================================}

function NTFSCreateMergedDescriptor(AParent,AChild:Pointer;var ACreated:Pointer;AVersion:Word):Boolean;
begin
 {}
 Result:=False;
 case AVersion of
  ntfsNTFS12:begin
    Result:=Security.CreateInheritedSecurityDescriptorNT(AParent,PSecurityDescriptor(ACreated));
   end;
  ntfsNTFS30,ntfsNTFS31:begin
    Result:=Security.CreateMergedSecurityDescriptor2K(AParent,AChild,PSecurityDescriptor(ACreated));
   end;
 end;
end;

{==============================================================================}

function NTFSDestroyDefaultDescriptor(ADescriptor:Pointer;AVersion:Word):Boolean;
begin
 {}
 Result:=False;
 if ADescriptor = nil then Exit;
 FreeMem(ADescriptor);
 Result:=True;
end;

{==============================================================================}

function NTFSDestroyInheritedDescriptor(ADescriptor:Pointer;AVersion:Word):Boolean;
begin
 {}
 Result:=False;
 case AVersion of
  ntfsNTFS12:begin
    Result:=Security.DestroyInheritedSecurityDescriptor(ADescriptor);
   end;
  ntfsNTFS30,ntfsNTFS31:begin
    Result:=Security.DestroyInheritedSecurityDescriptor(ADescriptor);
   end;
 end;
end;

{==============================================================================}

function NTFSDestroyMergedDescriptor(ADescriptor:Pointer;AVersion:Word):Boolean;
begin
 {}
 Result:=False;
 case AVersion of
  ntfsNTFS12:begin
    Result:=Security.DestroyInheritedSecurityDescriptor(ADescriptor);
   end;
  ntfsNTFS30,ntfsNTFS31:begin
    Result:=Security.DestroyMergedSecurityDescriptor(ADescriptor);
   end;
 end;
end;

{==============================================================================}
{==============================================================================}
{NTFS Conversion Functions}
function NTFSAttributeNameToStreamName(AType:LongWord;const AName:String):String;
{Format Attribute Name as a Stream Name (eg :<Name>:<Type>)}
var
 WorkBuffer:String;
begin
 {}
 Result:=ntfsBlankName;
 if Length(AName) = 0 then Exit;

 WorkBuffer:=FAT_NAME_CHAR + AName;
 case AType of
  ntfsAttrTypeStandardInformation:Result:=WorkBuffer + FAT_NAME_CHAR + ntfsAttrNameStandardInformation;
  ntfsAttrTypeAttributeList:Result:=WorkBuffer + FAT_NAME_CHAR + ntfsAttrNameAttributeList;
  ntfsAttrTypeFileName:Result:=WorkBuffer + FAT_NAME_CHAR + ntfsAttrNameFileName;
  ntfsAttrTypeObjectId:Result:=WorkBuffer + FAT_NAME_CHAR + ntfsAttrNameObjectId;
  {ntfsAttrTypeVolumeVersion:Result:=WorkBuffer + FAT_NAME_CHAR + ntfsAttrNameVolumeVersion;} {Version 1.2 only}
  ntfsAttrTypeSecurityDescriptor:Result:=WorkBuffer + FAT_NAME_CHAR + ntfsAttrNameSecurityDescriptor;
  ntfsAttrTypeVolumeName:Result:=WorkBuffer + FAT_NAME_CHAR + ntfsAttrNameVolumeName;
  ntfsAttrTypeVolumeInformation:Result:=WorkBuffer + FAT_NAME_CHAR + ntfsAttrNameVolumeInformation;
  ntfsAttrTypeData:Result:=WorkBuffer + FAT_NAME_CHAR + ntfsAttrNameData;
  ntfsAttrTypeIndexRoot:Result:=WorkBuffer + FAT_NAME_CHAR + ntfsAttrNameIndexRoot;
  ntfsAttrTypeIndexAllocation:Result:=WorkBuffer + FAT_NAME_CHAR + ntfsAttrNameIndexAllocation;
  ntfsAttrTypeBitmap:Result:=WorkBuffer + FAT_NAME_CHAR + ntfsAttrNameBitmap;
  ntfsAttrTypeReparsePoint:Result:=WorkBuffer + FAT_NAME_CHAR + ntfsAttrNameReparsePoint;
  {ntfsAttrTypeSymbolicLink:Result:=WorkBuffer + FAT_NAME_CHAR + ntfsAttrNameSymbolicLink;} {Version 1.2 only}
  ntfsAttrTypeExtendedAttrInformation:Result:=WorkBuffer + FAT_NAME_CHAR + ntfsAttrNameExtendedAttrInformation;
  ntfsAttrTypeExtendedAttr:Result:=WorkBuffer + FAT_NAME_CHAR + ntfsAttrNameExtendedAttr;
  ntfsAttrTypePropertySet:Result:=WorkBuffer + FAT_NAME_CHAR + ntfsAttrNamePropertySet;
  ntfsAttrTypeLoggedUtilityStream:Result:=WorkBuffer + FAT_NAME_CHAR + ntfsAttrNameLoggedUtilityStream;
 end;
end;

{==============================================================================}

function NTFSStreamNameToAttributeName(AType:LongWord;const AName:String):String;
{Extract Attribute Name from a Stream Name (eg :<Name>:<Type>)}
var
 PosIdx:Integer;
 WorkBuffer:String;
begin
 {}
 Result:=ntfsBlankName;
 if Length(AName) = 0 then Exit;

 {Check Separator}
 WorkBuffer:=AName;
 if WorkBuffer[1] = FAT_NAME_CHAR then
  begin
   Delete(WorkBuffer,1,1);
  end;
 {Check Separator}
 PosIdx:=Pos(FAT_NAME_CHAR,WorkBuffer);
 if PosIdx <> 0 then
  begin
   {Get Name}
   Result:=Copy(WorkBuffer,1,PosIdx - 1);
  end
 else
  begin
   {Get Name}
   Result:=WorkBuffer;
  end;
end;

{==============================================================================}
{==============================================================================}
{NTFS Rounding Functions}
function NTFSRoundWordTo8Bytes(AValue:Word):Word;
begin
 {}
 Result:=(AValue shr 3) shl 3;
 if Result < AValue then Inc(Result,8);
end;

{==============================================================================}

function NTFSRoundWordTo16Bytes(AValue:Word):Word;
begin
 {}
 Result:=(AValue shr 4) shl 4;
 if Result < AValue then Inc(Result,16);
end;

{==============================================================================}

function NTFSRoundWordTo512Bytes(AValue:Word):Word;
begin
 {}
 Result:=(AValue shr 9) shl 9;
 if Result < AValue then Inc(Result,512);
end;

{==============================================================================}

function NTFSRoundLongWordTo8Bytes(AValue:LongWord):LongWord;
begin
 {}
 Result:=(AValue shr 3) shl 3;
 if Result < AValue then Inc(Result,8);
end;

{==============================================================================}

function NTFSRoundLongWordTo16Bytes(AValue:LongWord):LongWord;
begin
 {}
 Result:=(AValue shr 4) shl 4;
 if Result < AValue then Inc(Result,16);
end;

{==============================================================================}

function NTFSRoundLongWordTo512Bytes(AValue:LongWord):LongWord;
begin
 {}
 Result:=(AValue shr 9) shl 9;
 if Result < AValue then Inc(Result,512);
end;

{==============================================================================}

function NTFSRoundQuadWordTo8Bytes(const AValue:Int64):Int64;
begin
 {}
 Result:=(AValue shr 3) shl 3;
 if Result < AValue then Inc(Result,8);
end;

{==============================================================================}

function NTFSRoundQuadWordTo16Bytes(const AValue:Int64):Int64;
begin
 {}
 Result:=(AValue shr 4) shl 4;
 if Result < AValue then Inc(Result,16);
end;

{==============================================================================}

function NTFSRoundQuadWordTo512Bytes(const AValue:Int64):Int64;
begin
 {}
 Result:=(AValue shr 9) shl 9;
 if Result < AValue then Inc(Result,512);
end;

{==============================================================================}

function NTFSRoundWordToUnitSize(AValue:Word;AShift,ASize:LongWord):Word;
begin
 {}
 Result:=(AValue shr AShift) shl AShift;
 if Result < AValue then Inc(Result,ASize);
end;

{==============================================================================}

function NTFSRoundLongWordToUnitSize(AValue,AShift,ASize:LongWord):LongWord;
begin
 {}
 Result:=(AValue shr AShift) shl AShift;
 if Result < AValue then Inc(Result,ASize);
end;

{==============================================================================}

function NTFSRoundQuadWordToUnitSize(const AValue:Int64;AShift,ASize:LongWord):Int64;
begin
 {}
 Result:=(AValue shr AShift) shl AShift;
 if Result < AValue then Inc(Result,ASize);
end;

{==============================================================================}

function NTFSRoundWordToClusterSize(AValue:Word;AShift,ASize:LongWord):Word;
begin
 {}
 Result:=(AValue shr AShift) shl AShift;
 if Result < AValue then Inc(Result,ASize);
end;

{==============================================================================}

function NTFSRoundLongWordToClusterSize(AValue,AShift,ASize:LongWord):LongWord;
begin
 {}
 Result:=(AValue shr AShift) shl AShift;
 if Result < AValue then Inc(Result,ASize);
end;

{==============================================================================}

function NTFSRoundQuadWordToClusterSize(const AValue:Int64;AShift,ASize:LongWord):Int64;
begin
 {}
 Result:=(AValue shr AShift) shl AShift;
 if Result < AValue then Inc(Result,ASize);
end;

{==============================================================================}
{==============================================================================}

initialization
 UpCase:=nil;

{==============================================================================}
{==============================================================================}

finalization
 if UpCase <> nil then FreeMem(UpCase);
 UpCase:=nil;

{==============================================================================}
{==============================================================================}

end. 