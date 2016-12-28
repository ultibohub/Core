{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2015 by Free Pascal development team

    System unit for Ultibo target.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Unit System;

{*****************************************************************************}
                                    interface
{*****************************************************************************}

{$define FPC_IS_SYSTEM}
{$define HAS_CMDLINE}
{$define DISABLE_NO_THREAD_MANAGER}

{ Do not use standard memory manager }
{$define HAS_MEMORYMANAGER}

{$define FPC_ANSI_TEXTFILEREC}

{$I check.inc}

{$I systemh.inc}

const
{$ifdef FPC_HAS_FEATURE_TEXTIO}
  LineEnding = #13#10;
{$endif FPC_HAS_FEATURE_TEXTIO}
{$ifdef FPC_HAS_FEATURE_FILEIO}
  LFNSupport = true;
  DirectorySeparator = '\';
  DriveSeparator = ':';
  ExtensionSeparator = '.';
  PathSeparator = ':';
  AllowDirectorySeparators : set of char = ['\','/'];
  AllowDriveSeparators : set of char = [':'];
{$endif FPC_HAS_FEATURE_FILEIO}

{ FileNameCaseSensitive and FileNameCasePreserving are defined below! }

{$ifdef FPC_HAS_FEATURE_EXITCODE}
  maxExitCode = 255;
{$endif FPC_HAS_FEATURE_EXITCODE}
{$ifdef FPC_HAS_FEATURE_FILEIO}

  MaxPathLen = 260;
  AllFilesMask = '*';

  UnusedHandle    = -1;
  StdInputHandle  = 0;
  StdOutputHandle = 1;
  StdErrorHandle  = 2;

  FileNameCaseSensitive : boolean = false;
  FileNameCasePreserving: boolean = true;
{$endif FPC_HAS_FEATURE_FILEIO}
{$ifdef FPC_HAS_FEATURE_TEXTIO}
  CtrlZMarksEOF: boolean = false; (* #26 not considered as end of file *)

  sLineBreak = LineEnding;
  DefaultTextLineBreakStyle : TTextLineBreakStyle = tlbsCrLF;
{$endif FPC_HAS_FEATURE_TEXTIO}

{$ifdef FPC_HAS_FEATURE_COMMANDARGS}
var
  argc: LongInt = 0;
  argv: PPChar = nil;
  envp: PPChar = nil;
  cmdline: PChar = nil;
{$endif FPC_HAS_FEATURE_COMMANDARGS}

{$ifndef FPUNONE}
{$ifdef FPC_HAS_FEATURE_SOFTFPU}

{$define fpc_softfpu_interface}
{$i softfpu.pp}
{$undef fpc_softfpu_interface}

{$endif FPC_HAS_FEATURE_SOFTFPU}
{$endif FPUNONE}

type
 {Process Functions}
 TSysGetProcessID = function:SizeUInt;
 
 {Random Functions}
 TSysRandomize = procedure;
 
 {File Functions}
 TSysDoClose = procedure(handle:thandle);
 TSysDoErase = procedure(p:pchar;pchangeable:boolean);
 TSysDoRename = procedure(p1,p2:pchar;p1changeable,p2changeable:boolean);
 TSysDoWrite = function(h:thandle;addr:pointer;len:longint):longint;
 TSysDoRead = function(h:thandle;addr:pointer;len:longint):longint;
 TSysDoFilePos = function(handle:thandle):longint;
 TSysDoSeek = procedure(handle:thandle;pos:longint);
 TSysDoSeekEnd = function(handle:thandle):longint;
 TSysDoFileSize = function(handle:thandle):longint;
 TSysDoTruncate = procedure(handle:thandle;pos:longint);
 TSysDoOpen = procedure(var f;p:PFileTextRecChar;flags:longint;pchangeable:boolean);
 
 {Directory Functions}
 TSysDoMkDir = procedure(const Dir:RawByteString);
 TSysDoRmDir = procedure(const Dir:RawByteString);
 TSysDoChDir = procedure(const Dir:RawByteString);
 TSysDoGetDir = procedure(DriveNr:Byte;var Dir:RawByteString);

var
 {Process Functions}
 SysGetProcessIDHandler:TSysGetProcessID;
 
 {Random Functions}
 SysRandomizeHandler:TSysRandomize;
 
 {File Functions}
 SysDoCloseHandler:TSysDoClose;
 SysDoEraseHandler:TSysDoErase;
 SysDoRenameHandler:TSysDoRename;
 SysDoWriteHandler:TSysDoWrite;
 SysDoReadHandler:TSysDoRead;
 SysDoFilePosHandler:TSysDoFilePos;
 SysDoSeekHandler:TSysDoSeek;
 SysDoSeekEndHandler:TSysDoSeekEnd;
 SysDoFileSizeHandler:TSysDoFileSize;
 SysDoTruncateHandler:TSysDoTruncate;
 SysDoOpenHandler:TSysDoOpen;
 
 {Directory Functions}
 SysDoMkDirHandler:TSysDoMkDir;
 SysDoRmDirHandler:TSysDoRmDir;
 SysDoChDirHandler:TSysDoChDir;
 SysDoGetDirHandler:TSysDoGetDir;
 
{*****************************************************************************}
                                 implementation
{*****************************************************************************}

{ Include ELF resources }

const calculated_cmdline:Pchar=nil;

{$ifndef FPUNONE}
{$ifdef FPC_HAS_FEATURE_SOFTFPU}

{$define fpc_softfpu_implementation}
{$i softfpu.pp}
{$undef fpc_softfpu_implementation}

{ we get these functions and types from the softfpu code }
{$define FPC_SYSTEM_HAS_float64}
{$define FPC_SYSTEM_HAS_float32}
{$define FPC_SYSTEM_HAS_flag}
{$define FPC_SYSTEM_HAS_extractFloat64Frac0}
{$define FPC_SYSTEM_HAS_extractFloat64Frac1}
{$define FPC_SYSTEM_HAS_extractFloat64Exp}
{$define FPC_SYSTEM_HAS_extractFloat64Frac}
{$define FPC_SYSTEM_HAS_extractFloat64Sign}
{$define FPC_SYSTEM_HAS_ExtractFloat32Frac}
{$define FPC_SYSTEM_HAS_extractFloat32Exp}
{$define FPC_SYSTEM_HAS_extractFloat32Sign}

{$endif FPC_HAS_FEATURE_SOFTFPU}
{$endif FPUNONE}

{$I system.inc}

{*****************************************************************************
                       Misc. System Dependent Functions
*****************************************************************************}

(*{$ifdef DISABLE_NO_THREAD_MANAGER}
Procedure InitSystemThreads; public name '_FPC_InitSystemThreads';
begin
  {No function}
end;
{$endif DISABLE_NO_THREAD_MANAGER}*)

procedure haltproc(e:longint);cdecl;external name '_haltproc';

procedure System_exit;
begin
{$ifdef FPC_HAS_FEATURE_EXITCODE}
  haltproc(ExitCode);
{$else FPC_HAS_FEATURE_EXITCODE}
  haltproc(0);
{$endif FPC_HAS_FEATURE_EXITCODE}
End;


{$ifdef FPC_HAS_FEATURE_PROCESSES}
function GetProcessID: SizeUInt;
begin
 if Assigned(SysGetProcessIDHandler) then
  begin
   GetProcessID:=SysGetProcessIDHandler();
  end
 else
  begin 
   GetProcessID:=0;
  end; 
end;
{$endif}


{$ifdef FPC_HAS_FEATURE_COMMANDARGS}
function ParamCount: Longint;
begin
 {}
 ParamCount:=argc - 1
end;

function ParamStr(l: longint) : string;
begin
 {}
 if (l >= 0) and (l + 1 <= argc) then
  begin
   ParamStr:=StrPas(argv[l]);
  end
 else
  begin
   ParamStr:='';
  end; 
end;
{$endif FPC_HAS_FEATURE_COMMANDARGS}

{const
  QRAN_SHIFT  = 15;
  QRAN_MASK   = ((1 shl QRAN_SHIFT) - 1);
  QRAN_MAX    = QRAN_MASK;
  QRAN_A      = 1664525;
  QRAN_C      = 1013904223;}

{$ifdef FPC_HAS_FEATURE_RANDOM}
procedure Randomize();
begin
 if Assigned(SysRandomizeHandler) then
  begin
   SysRandomizeHandler();
  end
 else
  begin 
   RandSeed:=63458;
  end; 
end;

{procedure randomize(value: integer);
begin
  RandSeed := value;
end;}

{function random(): integer;
begin
  RandSeed := QRAN_A * RandSeed + QRAN_C;
  random := (RandSeed shr 16) and QRAN_MAX;
end;}

{function random(value: integer): integer;
var
  a: integer;
begin
  RandSeed := QRAN_A * RandSeed + QRAN_C;
  a := (RandSeed shr 16) and QRAN_MAX;
  random := (a * value) shr 15;
end;}
{$endif FPC_HAS_FEATURE_RANDOM}

{*****************************************************************************
                         SystemUnit Initialization
*****************************************************************************}

{$ifdef FPC_HAS_FEATURE_STACKCHECK}

function CheckInitialStkLen(stklen : SizeUInt) : SizeUInt;inline;
begin
  result := stklen;
end;

var
  initialstkptr : Pointer; // external name '__stkptr';
{$endif FPC_HAS_FEATURE_STACKCHECK}

begin
  { FPU (hard or soft) is initialized from fpc_cpuinit, which is included
    per-cpu unconditionally.
  SysResetFPU;
  if not(IsLibrary) then
    SysInitFPU;
  }

{$ifdef FPC_HAS_FEATURE_CONSOLEIO}
  IsConsole := TRUE;
{$endif FPC_HAS_FEATURE_CONSOLEIO}

{$ifdef FPC_HAS_FEATURE_STACKCHECK}
  StackLength := CheckInitialStkLen(initialStkLen);
  StackBottom := initialstkptr - StackLength;
{$endif FPC_HAS_FEATURE_STACKCHECK}

{$ifdef FPC_HAS_FEATURE_EXCEPTIONS}
  { SysInitExceptions initializes only ExceptObjectstack and ExceptAddrStack
    with nil since both are located in the bss section, they are zeroed at startup
    anyways so not calling SysInitExceptions saves some bytes for simple programs. Even for threaded
    programs this does not matter because in the main thread, the variables are located
    in bss

    SysInitExceptions;
  }
{$endif FPC_HAS_FEATURE_EXCEPTIONS}

{$ifdef FPC_HAS_FEATURE_CONSOLEIO}
  { Reset IO Error }
  InOutRes:=0;
{$endif FPC_HAS_FEATURE_CONSOLEIO}

{$ifdef FPC_HAS_FEATURE_THREADING}
  { threading }
  InitSystemThreads;
{$endif FPC_HAS_FEATURE_THREADING}

{$ifdef FPC_HAS_FEATURE_WIDESTRINGS}
//  initunicodestringmanager;
{$endif FPC_HAS_FEATURE_WIDESTRINGS}
end.

