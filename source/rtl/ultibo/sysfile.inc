{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2015 by Free Pascal development team

    Low level file functions for Ultibo

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{****************************************************************************
                        Low level File Routines
               All these functions can set InOutRes on errors
****************************************************************************}

{ close a file from the handle value }
procedure do_close(handle : thandle);
begin
 if Assigned(SysDoCloseHandler) then
  begin
   SysDoCloseHandler(handle);
  end;
end;

procedure do_erase(p : pchar; pchangeable: boolean);
begin
 if Assigned(SysDoEraseHandler) then
  begin
   SysDoEraseHandler(p,pchangeable);
  end;
end;

procedure do_rename(p1,p2 : pchar; p1changeable, p2changeable: boolean);
begin
 if Assigned(SysDoRenameHandler) then
  begin
   SysDoRenameHandler(p1,p2,p1changeable,p2changeable);
  end;
end;

function do_write(h: thandle; addr: pointer; len: longint) : longint;
begin
 if Assigned(SysDoWriteHandler) then
  begin
   Result:=SysDoWriteHandler(h,addr,len);
  end
 else
  begin
   Result:=-1;
  end;
end;

function do_read(h: thandle; addr: pointer; len: longint) : longint;
begin
 if Assigned(SysDoReadHandler) then
  begin
   Result:=SysDoReadHandler(h,addr,len);
  end
 else
  begin
   Result:=-1;
  end;
end;

function do_filepos(handle: thandle) : longint;
begin
 if Assigned(SysDoFilePosHandler) then
  begin
   Result:=SysDoFilePosHandler(handle);
  end
 else
  begin
   Result:=-1;
  end;
end;

procedure do_seek(handle: thandle; pos: longint);
begin
 if Assigned(SysDoSeekHandler) then
  begin
   SysDoSeekHandler(handle,pos);
  end;
end;

function do_seekend(handle: thandle):longint;
begin
 if Assigned(SysDoSeekEndHandler) then
  begin
   Result:=SysDoSeekEndHandler(handle);
  end
 else
  begin
   Result:=-1;
  end;
end;

function do_filesize(handle : thandle) : longint;
begin
 if Assigned(SysDoFileSizeHandler) then
  begin
   Result:=SysDoFileSizeHandler(handle);
  end
 else
  begin
   Result:=-1;
  end;
end;

{ truncate at a given position }
procedure do_truncate(handle: thandle; pos: longint);
begin
 if Assigned(SysDoTruncateHandler) then
  begin
   SysDoTruncateHandler(handle,pos);
  end;
end;

procedure do_open(var f;p:PFileTextRecChar;flags:longint; pchangeable: boolean);
begin
 if Assigned(SysDoOpenHandler) then
  begin
   SysDoOpenHandler(f,p,flags,pchangeable);
  end;
end;

function do_isdevice(handle: thandle): boolean;
begin
 Result:=False; {Not required by Ultibo}
end;

