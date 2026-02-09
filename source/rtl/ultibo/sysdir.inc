{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2015 by Free Pascal development team

    Low level directory functions for Ultibo

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{*****************************************************************************
                           Directory Handling
*****************************************************************************}
{$if defined(FPC_HAS_FEATURE_ANSISTRINGS)}
procedure do_mkdir(const s: rawbytestring);
begin
 if Assigned(SysDoMkDirHandler) then
  begin
   SysDoMkDirHandler(s);
  end
 else
  begin
   InOutRes:=3;
  end;
end;

procedure do_rmdir(const s: rawbytestring);
begin
 if Assigned(SysDoRmDirHandler) then
  begin
   SysDoRmDirHandler(s);
  end
 else
  begin
   InOutRes:=3;
  end;
end;

procedure do_chdir(const s: rawbytestring);
begin
 if Assigned(SysDoChDirHandler) then
  begin
   SysDoChDirHandler(s);
  end
 else
  begin
   InOutRes:=3;
  end;
end;

procedure do_GetDir (DriveNr: byte; var Dir: RawByteString);
begin
 if Assigned(SysDoGetDirHandler) then
  begin
   SysDoGetDirHandler(DriveNr,Dir);
  end
 else
  begin
   InOutRes:=3;
  end;
end;

{$else FPC_HAS_FEATURE_ANSISTRINGS}

procedure mkdir(const s: shortstring);
begin
  InOutRes:=3;
end;

procedure rmdir(const s: shortstring);
begin
  InOutRes:=3;
end;

procedure chdir(const s: shortstring);
begin
  InOutRes:=3;
end;

procedure GetDir (DriveNr: byte; var Dir: ShortString);
begin
  InOutRes:=3;
end;
{$endif FPC_HAS_FEATURE_ANSISTRINGS}



