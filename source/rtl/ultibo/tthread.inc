{ Thread management routines }

const
 CREATE_SUSPENDED = $00000004;

procedure TThread.SysCreate(CreateSuspended: Boolean;
                            const StackSize: SizeUInt);
begin
  FSuspended := CreateSuspended;
  FInitialSuspended := CreateSuspended;
  { Always start in suspended state, will be resumed in AfterConstruction if necessary
    See Mantis #16884 }
  FHandle := BeginThread(nil, StackSize, @ThreadProc, pointer(self), CREATE_SUSPENDED,
                         FThreadID);
  if FHandle = TThreadID(0) then
    //raise EThread.CreateFmt(SThreadCreateError, [SysErrorMessage(getlasterror)]); //Ultibo
    raise EThread.create('Failed to create new thread');

  FFatalException := nil;
end;


procedure TThread.SysDestroy;
begin
  if FHandle<>0 then
    begin
      { Don't check Suspended. If the thread has been externally suspended (which is
        deprecated and strongly discouraged), it's better to deadlock here than
        to silently free the object and leave OS resources leaked. }
      if not FFinished {and not Suspended} then
        begin
          Terminate;
          { Allow the thread function to perform the necessary cleanup. Since
            we've just set Terminated flag, it won't call Execute. }
          if FInitialSuspended then
            Start;
          WaitFor;
        end;
    end;

  FFatalException.Free;
  FFatalException := nil;
end;

procedure TThread.CallOnTerminate;
begin
  FOnTerminate(Self);
end;

procedure TThread.DoTerminate;
begin
  if Assigned(FOnTerminate) then
    Synchronize(@CallOnTerminate);
end;

{Delphi/Windows compatible priority constants, they are also defined for Unix and Win32}
const
   TTHREAD_PRIORITY_IDLE          = -15;
   TTHREAD_PRIORITY_LOWEST        = -2;
   TTHREAD_PRIORITY_BELOW_NORMAL  = -1;
   TTHREAD_PRIORITY_NORMAL        = 0;
   TTHREAD_PRIORITY_ABOVE_NORMAL  = 1;
   TTHREAD_PRIORITY_HIGHEST       = 2;
   TTHREAD_PRIORITY_TIME_CRITICAL = 15;

const
  Priorities: array [TThreadPriority] of Integer =
   (TTHREAD_PRIORITY_IDLE, TTHREAD_PRIORITY_LOWEST, TTHREAD_PRIORITY_BELOW_NORMAL,
    TTHREAD_PRIORITY_NORMAL, TTHREAD_PRIORITY_ABOVE_NORMAL,
    TTHREAD_PRIORITY_HIGHEST, TTHREAD_PRIORITY_TIME_CRITICAL);

function TThread.GetPriority: TThreadPriority;
var
  P: Integer;
  I: TThreadPriority;
begin
  P := System.ThreadGetPriority(FHandle);
  Result := tpNormal;
  for I := Low(TThreadPriority) to High(TThreadPriority) do
    if Priorities[I] = P then Result := I;
end;

procedure TThread.SetPriority(Value: TThreadPriority);
begin
  System.ThreadSetPriority(FHandle, Priorities[Value]);
end;


procedure TThread.SetSuspended(Value: Boolean);
begin
  if Value <> FSuspended then
    if Value then
      Suspend
    else
      Resume;
end;

procedure TThread.Suspend;
begin
  FSuspended := True;
  SuspendThread(FHandle);
end;

procedure TThread.Resume;
begin
  if ResumeThread(FHandle) = 0 then FSuspended := False;
end;

{$if defined(FPC_STABLE) or defined(FPC_FIXES) or defined(FPC_LEGACY)}
procedure TThread.Terminate;
begin
  FTerminated := True;
  {$ifndef FPC_LEGACY}
  TerminatedSet;
  {$endif}
end;
{$endif}

function TThread.WaitFor: Integer;
begin
  Result:=WaitForThreadTerminate(FHandle,0);
  FHandle:=0;
end;
