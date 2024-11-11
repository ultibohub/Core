{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  P : TPackage;
  T : TTarget;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    P:=AddPackage('ultibounits');
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.3.1';
    P.Dependencies.Add('pasjpeg'); //Add pasjpeg for JPEGLib units
    P.Dependencies.Add('fcl-image'); //Add fcl-image for BMPcomn unit
    
    P.Author := 'Garry Wood';
    P.License := 'LGPL with modification';
    P.HomepageURL := 'ultibo.org';
    P.Email := '';
    P.Description := 'Additional units for Ultibo core';
    P.NeedLibC:= false;

    //P.CPUs:=[arm,aarch64,i386];
    P.OSes:=[ultibo];

    P.SourcePath.Add('src');
    P.IncludePath.Add('src');

    T:=P.Targets.AddUnit('ubitmap.pas');
    T:=P.Targets.AddUnit('uscreenshot.pas');

    T:=P.Targets.AddUnit('dispmanx.pas');
    T:=P.Targets.AddUnit('egl.pas');
    T:=P.Targets.AddUnit('openvg.pas');
    T:=P.Targets.AddUnit('vgshapes.pas');
    
{$if FPC_FULLVERSION>=30301}
    P.NamespaceMap:='namespaces.lst';
{$endif}

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
