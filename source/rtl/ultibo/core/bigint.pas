{
Ultibo Big Integer interface unit.

Copyright (C) 2024 - SoftOz Pty Ltd.

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

  AXTLS - \crypto\bigint.c - Copyright (c) 2007, Cameron Rich

References
==========

  Handbook of Applied Cryptography (Chapter 14) - http://cacr.uwaterloo.ca/hac/about/chap14.pdf

Big Integer Arithmetic
======================

 This unit implements multiple precision integer arithmetic operations as well as
 multiple precision modular arithmetic including addition, subtraction, multiplication,
 division, square, modular reduction and modular exponentiation.

 The unit is primarily intended to support the RSA functions within the Crypto unit
 as well as other cryptographic functionality.

}

{$mode delphi}    {Default to Delphi compatible syntax}
{$H+}             {Default to AnsiString}
{$inline on}      {Allow use of Inline procedures}
{$pointermath on} {Allow pointer arithmetic}

unit BigInt;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {BigInt specific constants}
 {Maintain a number of precomputed variables when doing reduction}
 BIGINT_M_OFFSET = 0;    {Normal modulo offset}
 BIGINT_P_OFFSET = 1;    {P modulo offset}
 BIGINT_Q_OFFSET = 2;    {Q modulo offset}
 BIGINT_NUM_MODS = 3;    {The number of modulus constants used}

const
 BIGINT_COMP_RADIX       = UInt64(4294967296);        {Max component + 1}
 BIGINT_COMP_MAX         = UInt64($FFFFFFFFFFFFFFFF); {Max dbl component - 1}
 BIGINT_COMP_BIT_SIZE    = 32;                        {Number of bits in a component}
 BIGINT_COMP_BYTE_SIZE   = 4;                         {Number of bytes in a component}
 BIGINT_COMP_NUM_NIBBLES = 8;                         {Used for diagnostics only}

 BIGINT_PERMANENT = $7FFF55AA;  {A magic number for permanents}

{==============================================================================}
type
 {BigInt specific types}
 PComponent = ^TComponent;
 TComponent = LongWord;    {A single precision component}

 PLongComponent = ^TLongComponent;
 TLongComponent = UInt64;  {A double precision component}

 PSignedLongComponent = ^TSignedLongComponent;
 TSignedLongComponent = Int64; {A signed double precision component}

type
 {A big integer basic object}
 PPBigInt = ^PBigInt;
 PBigInt = ^TBigInt;
 TBigInt = record
  Next:PBigInt;           {The next bigint in the cache}
  Size:Integer;           {The number of components in this bigint}
  MaxComponents:Integer;  {The number of components allocated for this bigint}
  References:Integer;     {An internal reference count}
  Components:PComponent;  {A ptr to the actual component data}

  procedure Zero;
  procedure Clear;

  function ToString:String;
 end;

 {Maintains the state of the cache, and a number of variables used in reduction}
 PBigIntContext = ^TBigIntContext;
 TBigIntContext = record
  ActiveList:PBigInt;                                       {Bigints currently used}
  FreeList:PBigInt;                                         {Bigints not used}
  BIRadix:PBigInt;                                          {The radix used}
  BIMod:array[0..BIGINT_NUM_MODS - 1] of PBigInt;           {Modulus}

  BImu:array[0..BIGINT_NUM_MODS - 1] of PBigInt;            {Storage for mu}
  BIbk1:array[0..BIGINT_NUM_MODS - 1] of PBigInt;           {Storage for b(k+1)}
  BINormalisedMod:array[0..BIGINT_NUM_MODS - 1] of PBigInt; {Normalised mod storage}
  G:PPBigInt;                                               {Used by sliding-window}
  Window:Integer;                                           {The size of the sliding window}
  ActiveCount:Integer;                                      {Number of active bigints}
  FreeCount:Integer;                                        {Number of free bigints}

  ModOffset:Byte;                                           {The mod offset we are using}
 end;

{==============================================================================}
{var}
 {BigInt specific variables}

{==============================================================================}
{BigInt Functions}
function BIInitialize:PBigIntContext;
procedure BITerminate(Context:PBigIntContext);
procedure BIPermanent(BI:PBigInt);
procedure BIDepermanent(BI:PBigInt);
procedure BIClearCache(Context:PBigIntContext);
procedure BIFree(Context:PBigIntContext;BI:PBigInt);

function BICopy(BI:PBigInt):PBigInt;
function BIClone(Context:PBigIntContext;const BI:TBigInt):PBigInt;

procedure BIExport(Context:PBigIntContext;BI:PBigInt;Data:PByte;Size:Integer);
function BIImport(Context:PBigIntContext;Data:PByte;Size:Integer):PBigInt;
function IntToBI(Context:PBigIntContext;I:TComponent):PBigInt;

function BIAdd(Context:PBigIntContext;BIA,BIB:PBigInt):PBigInt;
function BISubtract(Context:PBigIntContext;BIA,BIB:PBigInt;var IsNegative:Boolean):PBigInt;
function BIDivide(Context:PBigIntContext;U,V:PBigInt;IsMod:Boolean):PBigInt;
function BIMultiply(Context:PBigIntContext;BIA,BIB:PBigInt):PBigInt;
function BIModPower(Context:PBigIntContext;BI,BIExp:PBigInt):PBigInt;
function BIModPower2(Context:PBigIntContext;BI,BIM,BIExp:PBigInt):PBigInt;

function BICompare(BIA,BIB:PBigInt):Integer;
procedure BISetMod(Context:PBigIntContext;BIM:PBigInt;ModOffset:Integer);
procedure BIFreeMod(Context:PBigIntContext;ModOffset:Integer);

function BIMod(Context:PBigIntContext;BI:PBigInt):PBigInt; inline;
function BIResidue(Context:PBigIntContext;BI:PBigInt):PBigInt; inline;
function BIBarrett(Context:PBigIntContext;BI:PBigInt):PBigInt;

function BISquare(Context:PBigIntContext;BI:PBigInt):PBigInt;{$IFNDEF BIGINT_SQUARE} inline;{$ENDIF}

function BICRT(Context:PBigIntContext;BI,DP,DQ,P,Q,QInv:PBigInt):PBigInt;

{==============================================================================}
{BigInt Helper Functions}
function BIToString(BI:PBigInt):String;
function StringToBI(Context:PBigIntContext;const Value:String):PBigInt;

{==============================================================================}
{==============================================================================}
 
implementation

{==============================================================================}
{==============================================================================}
{var}
 {BigInt specific variables}

{==============================================================================}
{==============================================================================}
{TBigInt}
procedure TBigInt.Zero;
begin
 {}
 Size:=1;
 Components[0]:=0;
end;

{==============================================================================}

procedure TBigInt.Clear;
begin
 {}
 FillChar(Components^,Size * BIGINT_COMP_BYTE_SIZE,0);
end;

{==============================================================================}

function TBigInt.ToString:String;
begin
 {}
 Result:=BIToString(@Self);
end;

{==============================================================================}
{==============================================================================}
{Internal Functions}
function BICheck(const BI:TBigInt):Boolean; inline;
{Perform a sanity check on bi}
begin
 {$IFDEF BIGINT_DEBUG}
 {}
 if BI.References <= 0 then
  begin
   if PLATFORM_LOG_ENABLED then PlatformLogError('BICheck - Zero or negative References in TBigInt');

   Result:=False;
   Exit;
  end;

 if BI.Next <> nil then
  begin
   if PLATFORM_LOG_ENABLED then PlatformLogError('BICheck - Attempt to use a TBigInt from the free list');

   Result:=False;
   Exit;
  end;

 Result:=True;
 {$ELSE}
 {}
 Result:=True;
 {$ENDIF}
end;

{==============================================================================}

function BITrim(BI:PBigInt):PBigInt;
{Delete any leading 0's (and allow for 0)}
begin
 {}
 if not BICheck(BI^) then
  begin
   Result:=nil;
   Exit;
  end;

 while (BI.Components[BI.Size - 1] = 0) and (BI.Size > 1) do
  begin
   Dec(BI.Size);
  end;

 Result:=BI;
end;

{==============================================================================}

procedure BIAddComponents(BI:PBigInt;N:Integer);
{Allocate and zero more components. (Does not consume BI)}
begin
 {}
 if N > BI.MaxComponents then
  begin
   {Expand the components array}
   BI.MaxComponents:=Max(BI.MaxComponents * 2,N);
   BI.Components:=ReAllocMem(Pointer(BI.Components),BI.MaxComponents * BIGINT_COMP_BYTE_SIZE);
  end;

 if N > BI.Size then
  begin
   {Clear the added components}
   FillChar(BI.Components[BI.Size],(N - BI.Size) * BIGINT_COMP_BYTE_SIZE,0);
  end;

 {Update Size}
 BI.Size:=N;
end;

{==============================================================================}

function BIAllocate(Context:PBigIntContext;Size:Integer;Clear:Boolean = False):PBigInt;
{Allocate a new BigInt or reuse a free one}
var
 BIR:PBigInt;
begin
 {}
 {Check free List}
 if Context.FreeList <> nil then
  begin
   {Reuse free BigInt}
   BIR:=Context.FreeList;
   Context.FreeList:=BIR.Next;
   Dec(Context.FreeCount);

   if BIR.References <> 0 then
    begin
     if PLATFORM_LOG_ENABLED then PlatformLogError('BIAllocate - Free TBigInt with References not equal to zero');

     Result:=nil;
     Exit;
    end;

   BIAddComponents(BIR,Size);
  end
 else
  begin
   {Create new BigInt}
   BIR:=GetMem(SizeOf(TBigInt));
   BIR.Components:=GetMem((Size * 2) * BIGINT_COMP_BYTE_SIZE);
   BIR.MaxComponents:=(Size * 2); {Allow space to expand}

   {Clear used components}
   if Clear then FillChar(BIR.Components^,Size * BIGINT_COMP_BYTE_SIZE,0);
  end;

 {Update BigInt}
 BIR.Size:=Size;
 BIR.References:=1;
 BIR.Next:=nil;

 {Update Count}
 Inc(Context.ActiveCount);

 Result:=BIR;
end;

{==============================================================================}

function BIFindMaxExponentIndex(BIExp:PBigInt):Integer;
{Work out the highest '1' bit in an exponent. Used when doing sliding-window exponentiation}
var
 I:Integer;
 Shift:TComponent;
 Test:TComponent;
begin
 {}
 I:=BIGINT_COMP_BIT_SIZE - 1;
 Shift:=BIGINT_COMP_RADIX div 2;
 Test:=BIExp.Components[BIExp.Size - 1]; {Assume no leading zeroes}

 if not BICheck(BIExp^) then
  begin
   Result:=-1;
   Exit;
  end;

 repeat
  if (Test and Shift) <> 0 then
   begin
    Result:=I + (BIExp.Size - 1) * BIGINT_COMP_BIT_SIZE;
    Exit;
   end;

  Shift:=Shift shr 1;

  Dec(I);
 until I < 0;

 Result:=-1; {Error - must have been a leading 0}
end;

{==============================================================================}

function BIExpBitIsOne(BIExp:PBigInt;Offset:Integer):Boolean;
{Is a particular bit is an exponent 1 or 0? Used when doing sliding-window exponentiation}
var
 I:Integer;
 Test:TComponent;
 Shift:TComponent;
 NumShifts:Integer;
begin
 {}
 Test:=BIExp.Components[Offset div BIGINT_COMP_BIT_SIZE];
 NumShifts:=Offset mod BIGINT_COMP_BIT_SIZE;
 Shift:=1;

 if not BICheck(BIExp^) then
  begin
   Result:=False;
   Exit;
  end;

 for I:=0 to NumShifts - 1 do
  begin
   Shift:=Shift shl 1;
  end;

 Result:=(Test and Shift) <> 0;
end;

{==============================================================================}

function BIIntMultiply(Context:PBigIntContext;BIA:PBigInt;B:TComponent):PBigInt;
{Perform a multiply between a bigint an an (unsigned) integer}
var
 N:Integer;
 J:Integer;
 BIR:PBigInt;

 R:PComponent;
 A:PComponent;
 Carry:TComponent;
 Tmp:TLongComponent;
begin
 {}
 if not BICheck(BIA^) then
  begin
   Result:=nil;
   Exit;
  end;

 J:=0;
 N:=BIA.Size;
 Carry:=0;

 {Allocate}
 BIR:=BIAllocate(Context,N + 1);
 R:=BIR.Components;
 A:=BIA.Components;

 {Clear}
 FillChar(R^,(N + 1) * BIGINT_COMP_BYTE_SIZE,0);

 repeat
  Tmp:=R^ + TLongComponent(A[J]) * B + Carry; {Avoid overflow}
  R^:=Tmp; {Downsize}
  Inc(R);
  Carry:=Tmp shr BIGINT_COMP_BIT_SIZE;

  Inc(J);
 until J >= N;

 R^:=Carry;
 BIFree(Context,BIA);

 Result:=BITrim(BIR);
end;

{==============================================================================}

function BIIntDivide(Context:PBigIntContext;BIR:PBigInt;Denom:TComponent):PBigInt;
{Perform an integer divide on a bigint}
var
 I:Integer;
 R:TLongComponent;
begin
 {}
 if not BICheck(BIR^) then
  begin
   Result:=nil;
   Exit;
  end;

 I:=BIR.Size - 1;
 R:=0;

 repeat
  R:=(R shl BIGINT_COMP_BIT_SIZE) + BIR.Components[I];
  BIR.Components[I]:=R div Denom;
  R:=R mod Denom;

  Dec(I);
 until I < 0;

 Result:=BITrim(BIR);
end;

{==============================================================================}

function BICompRightShift(BIR:PBigInt;NumShifts:Integer):PBigInt;
{Take each component and shift down (in terms of components) }
var
 I:Integer;
 X:PComponent;
 Y:PComponent;
begin
 {}
 if not BICheck(BIR^) then
  begin
   Result:=nil;
   Exit;
  end;

 I:=BIR.Size - NumShifts;
 X:=BIR.Components;
 Y:=@BIR.Components[NumShifts];

 if I <= 0 then {Have we completely right shifted?}
 begin
  BIR.Components[0]:=0; {Return 0}
  BIR.Size:=1;

  Result:=BIR;
  Exit;
 end;

 repeat
  X^:=Y^;
  Inc(X);
  Inc(Y);

  Dec(I);
 until I <= 0;

 Dec(BIR.Size,NumShifts);

 Result:=BIR;
end;

{==============================================================================}

function BICompLeftShift(BIR:PBigInt;NumShifts:Integer):PBigInt;
{Take each component and shift it up (in terms of components) }
var
 I:Integer;
 X:PComponent;
 Y:PComponent;
begin
 {}
 if not BICheck(BIR^) then
  begin
   Result:=nil;
   Exit;
  end;

 I:=BIR.Size - 1;
 if NumShifts <= 0 then
  begin
   Result:=BIR;
   Exit;
  end;

 BIAddComponents(BIR,BIR.Size + NumShifts);
 X:=@BIR.Components[I + NumShifts];
 Y:=@BIR.Components[I];

 repeat
  X^:=Y^;
  Dec(X);
  Dec(Y);

  Dec(I);
 until I < 0;

 {Zero least significant components}
 FillChar(BIR.Components^,NumShifts * BIGINT_COMP_BYTE_SIZE,0);

 Result:=BIR;
end;

{==============================================================================}

function BIRegularMultiply(Context:PBigIntContext;BIA,BIB:PBigInt;InnerPartial,OuterPartial:Integer):PBigInt;
{Perform a standard multiplication between two bigints}
var
 I:Integer;
 J:Integer;
 N:Integer;
 T:Integer;
 BIR:PBigInt;
 SR:PComponent;
 SA:PComponent;
 SB:PComponent;
 Tmp:TLongComponent;
 Carry:TComponent;
 RIndex:Integer;
begin
 {}
 if not BICheck(BIA^) or not BICheck(BIB^) then
  begin
   Result:=nil;
   Exit;
  end;

 I:=0;
 N:=BIA.Size;
 T:=BIB.Size;

 {Allocate}
 BIR:=BIAllocate(Context,N + T);
 SR:=BIR.Components;
 SA:=BIA.Components;
 SB:=BIB.Components;

 {Clear}
 FillChar(BIR.Components^,(N + T) * BIGINT_COMP_BYTE_SIZE,0);

 repeat
  Carry:=0;
  RIndex:=I;
  J:=0;

  if (OuterPartial > 0) and ((OuterPartial - I) > 0) and (OuterPartial < N) then
   begin
    RIndex:=OuterPartial - 1;
    J:=OuterPartial - I - 1;
   end;

  repeat
   if (InnerPartial > 0) and (RIndex >= InnerPartial) then
    begin
     Break;
    end;

   Tmp:=SR[RIndex] + TLongComponent(SA[J]) * SB[I] + Carry; {Avoid overflow}
   SR[RIndex]:=Tmp; {Downsize}
   Inc(RIndex);
   Carry:=Tmp shr BIGINT_COMP_BIT_SIZE;

   Inc(J);
  until J >= N;

  SR[RIndex]:=Carry;

  Inc(I);
 until I >= T;

 BIFree(Context,BIA);
 BIFree(Context,BIB);

 Result:=BITrim(BIR);
end;

{==============================================================================}

function BIRegularSquare(Context:PBigIntContext;BI:PBigInt):PBigInt;
{Perform the actual square operation. It takes into account overflow}
var
 T:Integer;
 I:Integer;
 J:Integer;
 BIR:PBigInt;
 W:PComponent;
 X:PComponent;
 Carry:TLongComponent;
 Tmp:TLongComponent;
 C:Byte;
 XX:TLongComponent;
begin
 {}
 T:=BI.Size;
 I:=0;

 {Allocate}
 BIR:=BIAllocate(Context,T * 2 + 1);
 W:=BIR.Components;
 X:=BI.Components;

 {Clear}
 FillChar(W^,BIR.Size * BIGINT_COMP_BYTE_SIZE,0);

 repeat
  Tmp:=W[2 * I] + TLongComponent(X[I]) * X[I]; {Avoid overflow}
  W[2 * I]:=Tmp;
  Carry:=Tmp shr BIGINT_COMP_BIT_SIZE;

  J:=I + 1;
  while J < T do
   begin
    C:=0;
    XX:=TLongComponent(X[I]) * X[J]; {Avoid overflow}
    if (BIGINT_COMP_MAX - XX) < XX then C:=1;

    Tmp:=XX shl 1;
    if (BIGINT_COMP_MAX - Tmp) < W[I + J] then C:=1;

    Tmp:=Tmp + W[I + J];
    if (BIGINT_COMP_MAX - Tmp) < Carry then C:=1;

    Tmp:=Tmp + Carry;
    W[I + J]:=Tmp;
    Carry:=Tmp shr BIGINT_COMP_BIT_SIZE;

    if C > 0 then Carry:=Carry + BIGINT_COMP_RADIX;

    Inc(J);
   end;

  Tmp:=W[I + T] + Carry;
  W[I + T]:=Tmp;
  W[I + T + 1]:=Tmp shr BIGINT_COMP_BIT_SIZE;

  Inc(I);
 until I >= T;

 BIFree(Context,BI);

 Result:=BITrim(BIR);
end;

{==============================================================================}

function BICompMod(BI:PBigInt;Modulus:Integer):PBigInt;
{Stomp on the most significant components to give the illusion of a "mod base radix" operation}
begin
 {}
 if not BICheck(BI^) then
  begin
   Result:=nil;
   Exit;
  end;

 if BI.Size > Modulus then
  begin
   BI.Size:=Modulus;
  end;

 Result:=BI;
end;

{==============================================================================}

procedure BIPrecomputeSlideWindow(Context:PBigIntContext;Window:Integer;G1:PBigInt);
{Work out g1, g3, g5, g7... etc for the sliding-window algorithm}
var
 K:Integer;
 I:Integer;
 G2:PBigInt;
begin
 {}
 K:=1;

 I:=0;
 while I < Window - 1 do {Compute 2^(window-1)}
  begin
   K:=K shl 1;

   Inc(I);
  end;

 Context.G:=GetMem(K * SizeOf(PBigInt));
 Context.G[0]:=BIClone(Context,G1^);
 BIPermanent(Context.G[0]);
 G2:=BIResidue(Context,BISquare(Context,Context.G[0])); {g^2}

 for I:=1 to K - 1 do
  begin
   Context.G[I]:=BIResidue(Context,BIMultiply(Context,Context.G[I - 1],BICopy(G2)));
   BIPermanent(Context.G[I]);
  end;

 BIFree(Context,G2);
 Context.Window:=K;
end;

{==============================================================================}
{==============================================================================}
{BigInt functions}
function BIInitialize:PBigIntContext;
{Start a new bigint context}
{Return: A bigint context}
var
 Context:PBigIntContext;
begin
 {}
 {Allocate Context}
 Context:=AllocMem(SizeOf(TBigIntContext));

 {Allocate Radix} 
 Context.BIRadix:=BIAllocate(Context,2);
 Context.BIRadix.Components[0]:=0;
 Context.BIRadix.Components[1]:=1;
 BIPermanent(Context.BIRadix);
 
 Result:=Context;
end;

{==============================================================================}

procedure BITerminate(Context:PBigIntContext);
{Close the bigint context and free any resources}
{Context: The bigint session context}
begin
 {}
 {Free Radix}
 BIDepermanent(Context.BIRadix);
 BIFree(Context,Context.BIRadix);

 {Check Active}
 if Context.ActiveCount <> 0 then
  begin
   if PLATFORM_LOG_ENABLED then PlatformLogError('BITerminate - ActiveCount not equal to zero');

   Exit;
  end;

 {Clear Cache}
 BIClearCache(Context);

 {Free Context}
 FreeMem(Context);
end;

{==============================================================================}

procedure BIPermanent(BI:PBigInt);
{Make a bigint object "unfreeable" if BIFree() is called on it}
{BI: The bigint to be made permanent}
begin
 {}
 if not BICheck(BI^) then Exit;

 if BI.References <> 1 then
  begin
   if PLATFORM_LOG_ENABLED then PlatformLogError('BIPermanent - References not equal to one');

   Exit;
  end;

 {Set Permanent}
 BI.References:=BIGINT_PERMANENT;
end;

{==============================================================================}

procedure BIDepermanent(BI:PBigInt);
{Take a permanent object and make it freeable}
{BI: The bigint to be made freeable}
begin
 {}
 if not BICHeck(BI^) then Exit;

 if BI.References <> BIGINT_PERMANENT then
  begin
   if PLATFORM_LOG_ENABLED then PlatformLogError('BIDepermanent - References not equal to BIGINT_PERMANENT');

   Exit;
  end;
 
 {Clear Permanent} 
 BI.References:=1;
end;

{==============================================================================}

procedure BIClearCache(Context:PBigIntContext);
{Clear the memory cache}
var
 Next:PBigInt;
 Current:PBigInt;
begin
 {}
 {Check Free List}
 if Context.FreeList = nil then Exit;
 
 {Free Cache}
 Next:=Context.FreeList;
 while Next <> nil do
  begin
   {Get Next}
   Current:=Next;
   Next:=Current.Next;
   
   {Free Components}
   FreeMem(Current.Components);

   {Free Current}
   FreeMem(Current);
  end;

 Context.FreeCount:=0;
 Context.FreeList:=nil;
end;

{==============================================================================}

procedure BIFree(Context:PBigIntContext;BI:PBigInt);
{Free a bigint object so it can be used again}
{Context: The bigint session context}
{BI: The bigint to be freed}
begin
 {}
 if not BICheck(BI^) then Exit;

 {Check Permanent}
 if BI.References = BIGINT_PERMANENT then Exit;

 {Decrement References}
 Dec(BI.References);
 if Bi.References > 0 then Exit;

 {Add to Free List}
 BI.Next:=Context.FreeList;
 Context.FreeList:=BI;
 Inc(Context.FreeCount);

 {Decrement Active Count}
 Dec(Context.ActiveCount);
 if Context.ActiveCount < 0 then
  begin
   if PLATFORM_LOG_ENABLED then PlatformLogError('BIFree - ActiveCount less than zero');
   end;
end;

{==============================================================================}

function BICopy(BI:PBigInt):PBigInt;
{Increment the number of references to this object. }
{BI: The bigint to copy}
{Return:  A reference to the same bigint}
begin
 {}
 if not BICheck(BI^) then
  begin
   Result:=nil;
   Exit;
  end;

 {Increment references}
 if BI.References <> BIGINT_PERMANENT then Inc(BI.References);
  
 Result:=BI;
end;

{==============================================================================}

function BIClone(Context:PBigIntContext;const BI:TBigInt):PBigInt;
{Do a full copy of the bigint object}
{Context: The bigint session context}
{BI: The bigint object to be copied}
{Return: A copy of the bigint object}
var
 BIR:PBigInt;
begin
 {}
 if not BICheck(BI) then
  begin
   Result:=nil;
   Exit;
  end;

 {Allocate}
 BIR:=BIAllocate(Context,BI.Size);

 {Copy}
 System.Move(BI.Components^,BIR.Components^,BI.Size * BIGINT_COMP_BYTE_SIZE);

 Result:=BIR;
end;

{==============================================================================}

procedure BIExport(Context:PBigIntContext;BI:PBigInt;Data:PByte;Size:Integer);
{Take a bigint and convert it into a byte sequence}
{Context: The bigint session context}
{BI: The bigint to be converted}
{Data: The converted data as a byte stream}
{Size: The maximum size of the byte stream. Unused bytes will be zeroed}
var
 I:Integer;
 J:Integer;
 K:Integer;
 Mask:TComponent;
 Num:Integer;
begin
 {}
 if not BICheck(BI^) then Exit;

 FillChar(Data^,Size,0);

 K:=Size - 1;
 try
  for I:=0 to BI.Size - 1 do
   begin
    for J:=0 to BIGINT_COMP_BYTE_SIZE - 1 do
     begin
      Mask:=$FF shl (J * 8);
      Num:=(BI.Components[I] and Mask) shr (J * 8);

      Data[K]:=Num;
      Dec(K);

      if K < 0 then Exit;
     end;
   end;
 finally
  BIFree(Context,BI);
 end;
end;

{==============================================================================}

function BIImport(Context:PBigIntContext;Data:PByte;Size:Integer):PBigInt;
{Allow a binary sequence to be imported as a bigint}
{Context: The bigint session context}
{Data: The data to be converted}
{Size: The number of bytes of data}
{Return: A bigint representing this data}
var
 I:Integer;
 J:Integer;
 Offset:Integer;
 BIR:PBigInt;
begin
 {}
 J:=0;
 Offset:=0;

 {Allocate}
 BIR:=BIAllocate(Context,(Size + BIGINT_COMP_BYTE_SIZE - 1) div BIGINT_COMP_BYTE_SIZE);

 {Clear}
 FillChar(BIR.Components^,BIR.Size * BIGINT_COMP_BYTE_SIZE,0);

 for I:=Size - 1 downto 0 do
  begin
   BIR.Components[Offset]:=BIR.Components[Offset] + (Data[I] shl (J * 8));

   Inc(J);
   if J = BIGINT_COMP_BYTE_SIZE then
    begin
     J:=0;
     Inc(Offset);
    end;
  end;

 Result:=BITrim(BIR);
end;

{==============================================================================}

function IntToBI(Context:PBigIntContext;I:TComponent):PBigInt;
{Convert an (unsigned) integer into a bigint}
{Context: The bigint session context}
{I: The (unsigned) integer to be converted}
var
 BIR:PBigInt;
begin
 {}
 {Allocate}
 BIR:=BIAllocate(Context,1);

 {Assign}
 BIR.Components[0]:=I;

 Result:=BIR;
end;

{==============================================================================}

function BIAdd(Context:PBigIntContext;BIA,BIB:PBigInt):PBigInt;
{Perform an addition operation between two bigints}
{Context: The bigint session context}
{BIA: A bigint}
{BIB: Another bigint}
{Return: The result of the addition}
var
 N:Integer;
 PA:PComponent;
 PB:PComponent;
 Carry:TComponent;
 
 SL:TComponent;
 RL:TComponent;
 CY1:TComponent;
begin
 {}
 Carry:=0;
 
 if not BICheck(BIA^) or not BICheck(BIB^) then
  begin
   Result:=nil;
   Exit;
  end;

 N:=Max(BIA.Size,BIB.Size);
 BIAddComponents(BIA,N + 1);
 BIAddComponents(BIB,N);
 
 PA:=BIA.Components;
 PB:=BIB.Components;
 
 repeat
  SL:=PA^ + PB^;
  Inc(PB);
  
  RL:=SL + Carry;
  if SL < PA^ then CY1:=1 else CY1:=0;
  if RL < SL then Carry:=CY1 or 1 else Carry:=CY1 or 0;
  
  PA^:=RL;
  Inc(PA);
  
  Dec(N);
 until N = 0;
 
 PA^:=Carry; {Do overflow}
 BIFree(Context,BIB);

 Result:=BITrim(BIA); 
end;

{==============================================================================}

function BISubtract(Context:PBigIntContext;BIA,BIB:PBigInt;var IsNegative:Boolean):PBigInt;
{Perform a subtraction operation between two bigints}
{Context: The bigint session context}
{BIA: A bigint}
{BIB: Another bigint}
{IsNegative: Indicates that the result was negative}
{Return: The result of the subtraction. The result is always positive}
var
 N:Integer;
 PA:PComponent;
 PB:PComponent;
 Carry:TComponent;

 SL:TComponent;
 RL:TComponent;
 CY1:TComponent;
begin
 {}
 Carry:=0;

 if not BICheck(BIA^) or not BICheck(BIB^) then
  begin
   Result:=nil;
   Exit;
  end;

 N:=BIA.Size;
 BIAddComponents(BIB,N);
 
 PA:=BIA.Components;
 PB:=BIB.Components;
 
 repeat 
  SL:=PA^ - PB^;
  Inc(PB);
  
  RL:=SL - Carry;
  if SL > PA^ then CY1:=1 else CY1:=0;
  if RL > SL then Carry:=CY1 or 1 else Carry:=CY1 or 0;

  PA^:=RL;
  Inc(PA);
  
  Dec(N);
 until N = 0;

 if Carry <> 0 then IsNegative:=True else IsNegative:=False;

 BIFree(Context,BITrim(BIB)); {Put BIB back to the way it was}

 Result:=BITrim(BIA); 
end;

{==============================================================================}

function BIDivide(Context:PBigIntContext;U,V:PBigInt;IsMod:Boolean):PBigInt;
{Does both division and modulo calculations}
{Context: The bigint session context}
{U: A bigint which is the numerator}
{V: Either the denominator or the modulus depending on the mode}
{IsMod: Determines if this is a normal division (False) or a reduction (True)}
{Return: The result of the division/reduction}

 function BIDivide_V1(V:PBigInt):TComponent; inline;
 {V1 for division}
 begin
  {}
  Result:=V.Components[V.Size - 1];
 end;

 function BIDivide_V2(V:PBigInt):TComponent; inline;
 {V2 for division}
 begin
  {}
  Result:=V.Components[V.Size - 2];
 end;

 function BIDivide_U(TmpU:PBigInt;J:Integer):TComponent; inline;
 {U(J) for division}
 begin
  {}
  Result:=TmpU.Components[TmpU.Size - J - 1];
 end;

var
 N:Integer;
 M:Integer;
 J:Integer;
 OrigUSize:Integer;
 ModOffset:Byte;

 D:TComponent;
 QDash:TComponent;

 Quotient:PBigInt;
 TmpU:PBigInt;

 Inner:TComponent;
 IsNegative:Boolean;
begin
 {}
 N:=V.Size;
 M:=U.Size - N;
 J:=0;
 OrigUSize:=U.Size;
 ModOffset:=Context.ModOffset;

 if not BICheck(U^) or not BICheck(V^) then
  begin
   Result:=nil;
   Exit;
  end;

 {If doing reduction and we are < mod, then return mod}
 if IsMod and (BICompare(V,U) > 0) then
  begin
   BIFree(Context,V);
   Result:=U;
   Exit;
  end;

 {Allocate}
 Quotient:=BIAllocate(Context,M + 1);
 TmpU:=BIAllocate(Context,N + 1);
 V:=BITrim(V); {Make sure we have no leading 0's}
 D:=BIGINT_COMP_RADIX div (TLongComponent(BIDivide_V1(V)) + 1);

 {Clear}
 FillChar(Quotient.Components^,Quotient.Size * BIGINT_COMP_BYTE_SIZE,0);

 {Normalise}
 if D > 1 then
  begin
   U:=BIIntMultiply(Context,U,D);

   if IsMod then
    begin
     V:=Context.BINormalisedMod[ModOffset];
    end
   else
    begin
     V:=BIIntMultiply(Context,V,D);
    end;
  end;

 {New digit position u0}
 if OrigUSize = U.Size then
  begin
   BIAddComponents(U,OrigUSize + 1);
  end;

 repeat
  {Get a temporary short version of u}
  System.Move(U.Components[U.Size - N - 1 - J],TmpU.Components^,(N + 1) * BIGINT_COMP_BYTE_SIZE);

  {Calculate q'}
  if BIDivide_U(TmpU,0) = BIDivide_V1(V) then
   begin
    QDash:=BIGINT_COMP_RADIX - 1;
   end
  else
   begin
    QDash:=(TLongComponent(BIDivide_U(TmpU,0)) * BIGINT_COMP_RADIX + BIDivide_U(TmpU,1)) div BIDivide_V1(V);

    if (V.Size > 1) and (BIDivide_V2(V) > 0) then
    begin
     {We are implementing the following:
      if (V2*q_dash > (((U(0)*COMP_RADIX + U(1) - q_dash*V1)*COMP_RADIX) + U(2))) ... }
     Inner:=BIGINT_COMP_RADIX * BIDivide_U(TmpU,0) + BIDivide_U(TmpU,1) - TLongComponent(QDash) * BIDivide_V1(V); {Avoid overflow}
     if (TLongComponent(BIDivide_V2(V)) * QDash) > (TLongComponent(Inner) * BIGINT_COMP_RADIX + BIDivide_U(TmpU,2)) then {Avoid overflow}
      begin
       Dec(QDash);
      end;
    end;
  end;

  {Multiply and subtract}
  if QDash > 0 then
   begin
    TmpU:=BISubtract(Context,TmpU,BIIntMultiply(Context,BICopy(V),QDash),IsNegative);
    BIAddComponents(TmpU,N + 1);

    Quotient.Components[Quotient.Size - J - 1]:=QDash;

    {Add back}
    if IsNegative then
     begin
      Dec(Quotient.Components[Quotient.Size - J - 1]);
      TmpU:=BIAdd(Context,TmpU,BICopy(V));

      {Lop off the carry}
      Dec(TmpU.Size);
      Dec(V.Size);
     end;
   end
  else
   begin
    Quotient.Components[Quotient.Size - J - 1]:=0;
   end;

  {Copy back to U}
  System.Move(TmpU.Components^,U.Components[U.Size - N - 1 - J],(N + 1) * BIGINT_COMP_BYTE_SIZE);

  Inc(J);
 until J > M;

 BIFree(Context,TmpU);
 BIFree(Context,V);

 if IsMod then
  begin
   {Get the remainder}
   BIFree(Context,Quotient);

   Result:=BIIntDivide(Context,BITrim(U),D);
  end
 else
  begin
   {Get the quotient}
   BIFree(Context,U);

   Result:=BITrim(Quotient);
  end
end;

{==============================================================================}

function BIMultiply(Context:PBigIntContext;BIA,BIB:PBigInt):PBigInt;
{Perform a multiplication operation between two bigints}
{Context: The bigint session context}
{BIA: A bigint}
{BIB: Another bigint}
{Return: The result of the multiplication}
begin
 {}
 if not BICheck(BIA^) or not BICheck(BIB^) then
  begin
   Result:=nil;
   Exit;
  end;

 Result:=BIRegularMultiply(Context,BIA,BIB,0,0);
end;

{==============================================================================}

function BIModPower(Context:PBigIntContext;BI,BIExp:PBigInt):PBigInt;
{Perform a modular exponentiation.

 This function requires BISetMod() to have been called previously.
 This is one of the optimisations used for performance}
{Context: The bigint session context}
{BI: The bigint on which to perform the mod power operation}
{BIExp: The bigint exponent}
{Return: The result of the mod exponentiation operation}
var
 I:Integer;
 J:Integer;
 L:Integer;
 BIR:PBigInt;
 PartExp:Integer;
 WindowSize:Integer;
begin
 {}
 I:=BIFindMaxExponentIndex(BIExp);
 BIR:=IntToBI(Context,1);
 WindowSize:=1;

 if not BICheck(BI^) or not BICheck(BIExp^) then
  begin
   Result:=nil;
   Exit;
  end;

 {Work out an optimum size}
 J:=I;
 while J > 32 do
  begin
   Inc(WindowSize);
   J:=J div 5;
  end;

 {Work out the sliding window constants}
 BIPrecomputeSlideWindow(Context,WindowSize,BI);

 {If sliding-window is off, then only one bit will be done at a
  time and will reduce to standard left-to-right exponentiation}
 repeat
  if BIExpBitIsOne(BIExp,I) then
   begin
    L:=I - WindowSize + 1;
    PartExp:=0;

    if L < 0 then {LSB of exponent will always be 1}
     begin
      L:=0;
     end
    else
     begin
      while not BIExpBitIsOne(BIExp,L) do
       begin
        {Go back up}
        Inc(L);
       end;
     end;

    {Build up the section of the exponent}
    J:=I;
    while J >= L do
     begin
      BIR:=BIResidue(Context,BISquare(Context,BIR));
      if BIExpBitIsOne(BIExp,J) then
       begin
        Inc(PartExp);
       end;

      if J <> L then
       begin
        PartExp:=PartExp shl 1;
       end;

      Dec(J);
     end;

    PartExp:=(PartExp - 1) div 2; {Adjust for array}
    BIR:=BIResidue(Context,BIMultiply(Context,BIR,Context.G[PartExp]));

    I:=L - 1;
   end
  else
   begin
    {Square it}
    BIR:=BIResidue(Context,BISquare(Context,BIR));

    Dec(I);
   end;
 until I < 0;

 {Cleanup}
 for I:=0 to Context.Window - 1 do
  begin
   BIDepermanent(Context.G[I]);
   BIFree(Context,Context.G[I]);
  end;

 FreeMem(Context.G);
 BIFree(Context,BI);
 BIFree(Context,BIExp);

 Result:=BIR;
end;

{==============================================================================}

function BIModPower2(Context:PBigIntContext;BI,BIM,BIExp:PBigInt):PBigInt;
{Perform a modular exponentiation using a temporary modulus.

 We need this function to check the signatures of certificates. The modulus
 of this function is temporary as it's just used for authentication}
{Context: The bigint session context}
{BI: The bigint to perform the exp/mod}
{BIM: The temporary modulus}
{BIExp: The bigint exponent}
{Return: The result of the mod exponentiation operation}
var
 BIR:PBigInt;
 TmpBIR:PBigInt;
 TmpContext:PBigIntContext;
begin
 {}
 {Set up a temporary bigint context and transfer what we need between them.
  We need to do this since we want to keep the original modulus which is
  already in this context. This operation is only called when doing peer
  verification, and so is not expensive}
 TmpContext:=BIInitialize;
 BISetMod(TmpContext,BIClone(TmpContext,BIM^),BIGINT_M_OFFSET);

 TmpBIR:=BIModPower(TmpContext,BIClone(TmpContext,BI^),BIClone(TmpContext,BIExp^));
 BIR:=BIClone(Context,TmpBIR^);

 BIFree(TmpContext,TmpBIR);
 BIFreeMod(TmpContext,BIGINT_M_OFFSET);
 BITerminate(TmpContext);

 BIFree(Context,BI);
 BIFree(Context,BIM);
 BIFree(Context,BIExp);

 Result:=BIR;
end;

{==============================================================================}

function BICompare(BIA,BIB:PBigInt):Integer;
{Compare two bigints}
{BIA: A bigint}
{BIB: Another bigint}
{Return: -1 if smaller, 1 if larger and 0 if equal}
var
 I:Integer;
 A:PComponent;
 B:PComponent;
begin
 {}
 if not BICheck(BIA^) or not BICheck(BIB^) then
  begin
   Result:=0;
   Exit;
  end;

 if BIA.Size > BIB.Size then
  begin
   Result:=1;
   Exit;
  end
 else if BIA.Size < BIB.Size then
  begin
   Result:=-1;
   Exit;
  end
 else
  begin
   A:=BIA.Components;
   B:=BIB.Components;

   {Same number of components. Compare starting from the high end and working down}
   Result:=0;
   I:=BIA.Size - 1;

   repeat
    if A[I] > B[I] then
     begin
      Result:=1;
      Exit;
     end
    else if A[I] < B[I] then
     begin
      Result:=-1;
      Exit;
     end;

    Dec(I);
   until I < 0;
  end;
end;

{==============================================================================}

procedure BISetMod(Context:PBigIntContext;BIM:PBigInt;ModOffset:Integer);
{Pre-calculate some of the expensive steps in reduction.

 This function should only be called once (normally when a session starts).
 When the session is over, BIFreeMod() should be called. BIModPower() and BIMod()
 rely on this function being called}
{Context: The bigint session context}
{BIM: The bigint modulus that will be used}
{ModOffset: There are three moduluii that can be stored - the  standard modulus,
  and its two primes p and q. This offset refers to which modulus we are referring to}
var
 K:Integer;
 D:TComponent;
begin
 {}
 K:=BIM.Size;
 D:=BIGINT_COMP_RADIX div (TLongComponent(BIM.Components[K - 1]) + 1);

 Context.BIMod[ModOffset]:=BIM;
 BIPermanent(Context.BIMod[ModOffset]);

 Context.BINormalisedMod[ModOffset]:=BIIntMultiply(Context,BIM,D);
 BIPermanent(Context.BINormalisedMod[ModOffset]);

 Context.BImu[ModOffset]:=BIDivide(Context,BICompLeftShift(BIClone(Context,Context.BIRadix^),K * 2 - 1),Context.BIMod[ModOffset],False);
 BIPermanent(Context.BImu[ModOffset]);

 Context.BIbk1[ModOffset]:=BICompLeftShift(IntToBI(Context,1),K + 1);
 BIPermanent(Context.BIbk1[ModOffset]);
end;

{==============================================================================}

procedure BIFreeMod(Context:PBigIntContext;ModOffset:Integer);
{Used when cleaning various bigints at the end of a session}
{Context: The bigint session context}
{ModOffset: The offset to use}
begin
 {}
 BIDepermanent(Context.BIMod[ModOffset]);
 BIFree(Context,Context.BIMod[ModOffset]);

 BIDepermanent(Context.BImu[ModOffset]);
 BIFree(Context,Context.BImu[ModOffset]);

 BIDepermanent(Context.BIbk1[ModOffset]);
 BIFree(Context,Context.BIbk1[ModOffset]);

 BIDepermanent(Context.BINormalisedMod[ModOffset]);
 BIFree(Context,Context.BINormalisedMod[ModOffset]);
end;

{==============================================================================}

function BIMod(Context:PBigIntContext;BI:PBigInt):PBigInt; inline;
{Find the residue of BI. BISetMod() must be called beforehand}
begin
 {}
 Result:=BIDivide(Context,BI,Context.BIMod[Context.ModOffset],True);
end;

{==============================================================================}

function BIResidue(Context:PBigIntContext;BI:PBigInt):PBigInt; inline;
{BIResidue is simply an alias for BIBarrett}
begin
 {}
 {$IFDEF BIGINT_BARRETT}
 Result:=BIBarrett(Context,BI);
 {$ELSE}
 Result:=BIMod(Context,BI);
 {$ENDIF}
end;

{==============================================================================}

function BIBarrett(Context:PBigIntContext;BI:PBigInt):PBigInt;
{Perform a single Barrett reduction}
{Context: The bigint session context}
{BI: A bigint}
{Return: The result of the Barrett reduction}
var
 Q1:PBigInt;
 Q2:PBigInt;
 Q3:PBigInt;
 R1:PBigInt;
 R2:PBigInt;
 R:PBigInt;
 ModOffset:Byte;
 BIM:PBigInt;
 K:Integer;
 IsNegative:Boolean;
begin
 {}
 ModOffset:=Context.ModOffset;
 BIM:=Context.BIMod[ModOffset];
 K:=BIM.Size;

 if not BICheck(BI^) or not BICheck(BIM^) then
  begin
   Result:=nil;
   Exit;
  end;

 {Use Classical method instead  - Barrett cannot help here}
 if BI.Size > K * 2 then
  begin
   Result:=BIMod(Context,BI);
  end
 else
  begin
   {q1 = [x / b**(k-1)]}
   Q1:=BICompRightshift(BIClone(Context,BI^),K - 1);

   {Do outer partial multiply}
   {q2 = q1 * mu}
   Q2:=BIRegularMultiply(Context,Q1,Context.BImu[ModOffset],0,K - 1);
   {q3 = [q2 / b**(k+1)]}
   Q3:=BICompRightShift(Q2,K + 1);
   {r1 = x mod b**(k+1)}
   R1:=BICompMod(BI,K + 1);

   {Do inner partial multiply}
   {r2 = q3 * m mod b**(k+1)}
   R2:=BICompMod(BIRegularMultiply(Context,Q3,BIM,K + 1,0),K + 1);

   {if (r1 < r2) r1 = r1 + b**(k+1)}
   if BICompare(R1,R2) < 0 then
    begin
     R1:=BIAdd(Context,R1,Context.BIbk1[ModOffset]);
    end;

   {r = r1 - r2}
   R:=BISubtract(Context,R1,R2,IsNegative);

   {while (r >= m) do r = r - m;}
   while BICompare(R,BIM) >= 0 do
    begin
     R:=BISubtract(Context,R,BIM,IsNegative);
    end;

   Result:=R;
  end;
end;

{==============================================================================}
{$IFDEF BIGINT_SQUARE}
function BISquare(Context:PBigIntContext;BI:PBigInt):PBigInt;
{Perform a square operation on a bigint}
{Context: The bigint session context}
{BI: A bigint}
{Return: The result of the multiplication}
begin
 {}
 if not BICheck(BI^) then
  begin
   Result:=nil;
   Exit;
  end;

 Result:=BIRegularSquare(Context,BI);
end;
{$ELSE}
function BISquare(Context:PBigIntContext;BI:PBigInt):PBigInt; inline;
{Perform a square operation on a bigint}
{Context: The bigint session context}
{BI: A bigint}
{Return: The result of the multiplication}
begin
 {}
 Result:=BIMultiply(Context,BICOpy(BI),BI)
end;
{$ENDIF}
{==============================================================================}

function BICRT(Context:PBigIntContext;BI,DP,DQ,P,Q,QInv:PBigInt):PBigInt;
{Use the Chinese Remainder Theorem to quickly perform RSA decrypts}
{Context: The bigint session context}
{BI: The bigint to perform the exp/mod}
{DP: CRT's dP bigint}
{DQ: CRT's dQ bigint}
{P: CRT's p bigint}
{Q: CRT's q bigint}
{QInv: CRT's qInv bigint}
{Return: The result of the CRT operation}
var
 M1:PBigInt;
 M2:PBigInt;
 H:PBigInt;
 IsNegative:Boolean;
begin
 {}
 Context.ModOffset:=BIGINT_P_OFFSET;
 M1:=BIModPower(Context,BICopy(BI),DP);

 Context.ModOffset:=BIGINT_Q_OFFSET;
 M2:=BIModPower(Context,BI,DQ);

 H:=BISubtract(Context,BIAdd(Context,M1,P),BICopy(M2),IsNegative);
 H:=BIMultiply(Context,H,QInv);

 Context.ModOffset:=BIGINT_P_OFFSET;
 H:=BIResidue(Context,H);

 Result:=BIAdd(Context,M2,BIMultiply(Context,Q,H));
end;

{==============================================================================}
{==============================================================================}
{BigInt Helper Functions}
function BIToString(BI:PBigInt):String;
{Convert a bigint to a string of hex characters}
{BI: The bigint to convert}
{Return: A string representing the bigint}
var
 I:Integer;
 J:Integer;
 Num:TComponent;
 Mask:TComponent;
begin
 {}
 Result:='';

 if BI = nil then Exit;

 for I:=BI.Size - 1 downto 0 do
  begin
   for J:=BIGINT_COMP_NUM_NIBBLES - 1 downto 0 do
    begin
     Mask:=$0F shl (J * 4);
     Num:=(BI.Components[I] and Mask) shr (J * 4);

     Result:=Result + IntToHex(Num,1);
    end;
  end;
end;

{==============================================================================}

function StringToBI(Context:PBigIntContext;const Value:String):PBigInt;
{Convert a string of hex characters to a bigint}
{Context: The bigint session context}
{Value: A string consisting of hex characters}
{Return: A bigint representing this data}
var
 I:Integer;
 J:Integer;
 Size:Integer;
 Offset:Integer;
 BIR:PBigInt;
 Num:Integer;
begin
 {}
 J:=0;
 Offset:=0;
 Size:=Length(Value);

 {Allocate}
 BIR:=BIAllocate(Context,(Size + BIGINT_COMP_NUM_NIBBLES - 1) div BIGINT_COMP_NUM_NIBBLES);

 {Clear}
 FillChar(BIR.Components^,BIR.Size * BIGINT_COMP_BYTE_SIZE,0);

 for I:=Size - 1 downto 0 do
  begin
   Num:=StrToIntDef(Value[I],-1);
   if Num = -1 then
    begin
     Result:=nil;
     Exit;
    end;

   BIR.Components[Offset]:=BIR.Components[Offset] + LongWord(Num shl (J * 4));

   Inc(J);
   if J = BIGINT_COMP_NUM_NIBBLES then
    begin
     J:=0;
     Inc(Offset);
    end;
  end;

 Result:=BIR;
end;

{==============================================================================}
{==============================================================================}

initialization
 {Nothing}

{==============================================================================}

finalization
 {Nothing}

end.
