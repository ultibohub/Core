{
Ultibo CIFS interface unit.

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

 Based on sources from SAMBA and others.


Common Internet File System (CIFS)
==================================

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit CIFS;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,FileSystem,SysUtils,Classes,UltiboClasses,Winsock2;

//To Do //See: https://en.wikipedia.org/wiki/Server_Message_Block

//To Do //This unit will provide the CIFS client and CIFS server (using Winsock2)
        //As well as the TCIFSFileSystem and TCIFSRedirector (using FileSystem)

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
//const
 {CIFS specific constants}

//To Do //A generic CIFS unit 
                           
              
{==============================================================================}
//type
 {CIFS specific types}
//To Do

{==============================================================================}
var
 {CIFS specific variables}
 CIFSInitialized:Boolean;

 //To Do
 
{==============================================================================}
{Initialization Functions}
procedure CIFSInit;

{==============================================================================}
{CIFS Functions}
//To Do

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure CIFSInit;
begin
 {}
 {Check Initialized}
 if CIFSInitialized then Exit;
 
 //To Do
 
 CIFSInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{CIFS Functions}
//To Do

{==============================================================================}
{==============================================================================}

initialization
 CIFSInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
