{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2022 by Free Pascal development team

    This file implements all the base types and limits required
    for a minimal POSIX compliant subset required to port the compiler
    to a new OS.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{Platform specific information}
type
  { A handle in Ultibo is normally a pointer }
  {$ifdef CPU64}
  THandle = Int64;
  {$else CPU64}
  THandle = Longint;
  {$endif CPU64}
  { A thread id is the same as a thread handle }
  TThreadID = THandle;
  
  TOSTimestamp = {$if defined(FPC_STABLE) or defined(FPC_FIXES) or defined(FPC_LEGACY)}Longint{$else}Int64{$endif};

  { pthread_mutex_t }
  PRTLCriticalSection = ^TRTLCriticalSection;
  TRTLCriticalSection = record
    __m_reserved: longint;
    __m_count: longint;
    __m_owner: pointer;
    __m_kind:  longint;
    __m_lock:  record
       __status: sizeint;
      __spinlock: longint;
    end;
  end;

{Delphi/Windows compatible priority constants, they are also defined for Unix and Win32}
//const
//   THREAD_PRIORITY_IDLE          = -15;
//   THREAD_PRIORITY_LOWEST        = -2;
//   THREAD_PRIORITY_BELOW_NORMAL  = -1;
//   THREAD_PRIORITY_NORMAL        = 0;
//   THREAD_PRIORITY_ABOVE_NORMAL  = 1;
//   THREAD_PRIORITY_HIGHEST       = 2;
//   THREAD_PRIORITY_TIME_CRITICAL = 15;
//Ultibo //Should we add these here, differs from Threads.pas values ?
         //See: source\rtl\netware\sysosh.inc

var
 SystemCPUCount:LongWord = 1;
                      
