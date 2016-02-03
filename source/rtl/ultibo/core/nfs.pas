{
Ultibo NFS interface unit.

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


Network File System (NFS)
=========================

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit NFS;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,FileSystem,SysUtils,Classes,UltiboClasses,Winsock2;

//To Do //See: https://en.wikipedia.org/wiki/Network_File_System

//To Do //This unit will provide the NFS client and NFS server (using Winsock2)
                      //As well as the TNFSFileSystem and TNFSRedirector (using FileSystem)
                      
{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
//const
 {NFS specific constants}

//To Do //See: POP3 for framework
                           
              
{==============================================================================}
//type
 {NFS specific types}
//To Do

{==============================================================================}
var
 {NFS specific variables}
 NFSInitialized:Boolean;

 //To Do
 
{==============================================================================}
{Initialization Functions}
procedure NFSInit;

{==============================================================================}
{NFS Functions}
//To Do

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure NFSInit;
begin
 {}
 {Check Initialized}
 if NFSInitialized then Exit;
 
 //To Do
 
 NFSInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{NFS Functions}
//To Do

{==============================================================================}
{==============================================================================}

initialization
 NFSInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
