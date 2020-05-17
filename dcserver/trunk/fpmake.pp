{
   File generated automatically by Lazarus Package Manager
   Created with the Fppkgpackagemanager package installed

   fpmake.pp for DCServer 0.9

   This file was generated on 17-05-20
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_DCServer(const ADirectory: string);

var
  P : TPackage;
  T : TTarget;
  D : TDependency;

begin
  with Installer do
    begin
    P:=AddPackage('dcserver');
    P.Version:='0.9.0-0';

    P.Directory:=ADirectory;

    P.Author:='Joost van der Sluis / CNOC';
    P.License:='LGPL with modification';
    P.Description:='Framework for building applications which have to process different commands one-by-one. Commands can be initiated and monitored in several ways, like from the comment-line or by tcp/ip-connected clients.';

    D := P.Dependencies.Add('fcl-web');
    D := P.Dependencies.Add('fcl-net');
    D := P.Dependencies.Add('fcl-process');
    D := P.Dependencies.Add('fcl-json');
    D := P.Dependencies.Add('cnocThreadingTools');
    D := P.Dependencies.Add('fcl-base');
    P.Options.Add('-MObjFPC');
    P.Options.Add('-Scghi');
    P.Options.Add('-O1');
    P.Options.Add('-g');
    P.Options.Add('-gl');
    P.Options.Add('-l');
    P.Options.Add('-vewnhibq');
    P.UnitPath.Add('.');
    T:=P.Targets.AddUnit('dcserver.pas');
    D := T.Dependencies.AddUnit('dcsHandler');
    D := T.Dependencies.AddUnit('dcsConsoleServer');
    D := T.Dependencies.AddUnit('dcsInOutputProcessor');
    D := T.Dependencies.AddUnit('dcsThreadCommandFactory');
    D := T.Dependencies.AddUnit('DCSTCPServer');
    D := T.Dependencies.AddUnit('DCSClientThread');
    D := T.Dependencies.AddUnit('dcsListenerThread');
    D := T.Dependencies.AddUnit('DCSHTTPRestServer');
    D := T.Dependencies.AddUnit('dcsGlobalSettings');
    T := P.Targets.AddImplicitUnit('dcshandler.pas');
    T := P.Targets.AddImplicitUnit('dcsconsoleserver.pas');
    T := P.Targets.AddImplicitUnit('dcsinoutputprocessor.pas');
    T := P.Targets.AddImplicitUnit('dcsthreadcommandfactory.pas');
    T := P.Targets.AddImplicitUnit('dcstcpserver.pas');
    T := P.Targets.AddImplicitUnit('dcsclientthread.pas');
    T := P.Targets.AddImplicitUnit('dcslistenerthread.pas');
    T := P.Targets.AddImplicitUnit('dcshttprestserver.pas');
    T := P.Targets.AddImplicitUnit('dcsglobalsettings.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.InstallFiles.Add('DCServer.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_DCServer('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
