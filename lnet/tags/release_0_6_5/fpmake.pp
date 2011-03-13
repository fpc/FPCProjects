{$mode objfpc}{$H+}
program fpmake;

{$IFDEF COMPILED_BY_FPPKG}
{$IFNDEF HAS_PACKAGE_LAZMKUNIT}
{$FATAL This package depends on the lazmkunit package which is not installed}
{$ENDIF}
{$ENDIF}

uses fpmkunit, lazmkunit;

Var
  P : TLazPackage;
  T : TTarget;

begin
  With Installer(TLazInstaller) do
    begin
    P:=AddPackage('lnet') as TLazPackage;
    p.AfterInstall := @TLazInstaller(Installer).DoRegisterLazarusPackages;

    P.Version:='0.6.4-2527';
    P.OSes:=AllUnixOSes+[Win32,Win64];
    P.Author := 'Aleš Katona';
    P.License := 'LGPL with modification, Examples: GPL2';
    P.HomepageURL := 'http://lnet.wordpress.com/';
    P.Email := 'almindor@gmail.com';
    P.Description := 'Collection of classes and components to enable event-driven TCP or UDP networking';
{$IFDEF VER_2_4_0}
    P.Options := '-Sm';
{$ELSE VER_2_4_0}
    P.Options.add('-Sm');
{$ENDIF VER_2_4_0}

    P.Dependencies.Add('fcl-net');
    P.Dependencies.Add('fcl-base');
    P.Dependencies.Add('fcl-process');
    p.Dependencies.Add('winunits-jedi',[win32,win64]);
//    P.NeedLibC:= true;  // true for headers that indirectly link to libc?

    T:=P.Targets.AddUnit('lib/lws2tcpip.pp',AllOSes-AllUnixOSes);
    T:=P.Targets.AddUnit('lib/lcommon.pp');
    with T.Dependencies do
      begin
      AddUnit('lws2tcpip',AllOSes-AllUnixOSes);
      AddInclude('lib/sys/osunits.inc');
      end;
    T:=P.Targets.AddUnit('lib/levents.pp');
    with T.Dependencies do
      begin
      AddInclude('lib/sys/lkqueueeventerh.inc');
      AddInclude('lib/sys/lepolleventerh.inc');
      AddInclude('lib/sys/lkqueueeventer.inc');
      AddInclude('lib/sys/lepolleventer.inc');
      end;
    T:=P.Targets.AddUnit('lib/lcontrolstack.pp');
    T:=P.Targets.AddUnit('lib/lmimetypes.pp');
    T:=P.Targets.AddUnit('lib/lmimestreams.pp');
    T:=P.Targets.AddUnit('lib/lmimewrapper.pp');
    T:=P.Targets.AddUnit('lib/lprocess.pp');
    T:=P.Targets.AddUnit('lib/lspawnfcgi.pp');
    with T.Dependencies do
      begin
      AddInclude('lib/sys/lspawnfcgiunix.inc',AllUnixOSes);
      AddInclude('lib/sys/lspawnfcgiwin.inc',AllOSes-AllUnixOSes);
      end;
    T:=P.Targets.AddUnit('lib/lfastcgi.pp');
    T:=P.Targets.AddUnit('lib/lstrbuffer.pp');
    T:=P.Targets.AddUnit('lib/lthreadevents.pp');
    T:=P.Targets.AddUnit('lib/ltimer.pp');
    T:=P.Targets.AddUnit('lib/lwebserver.pp');
    T:=P.Targets.AddUnit('lib/openssl.pas');
    T:=P.Targets.AddUnit('lib/lnet.pp');
    T:=P.Targets.AddUnit('lib/lnetssl.pp');
    T:=P.Targets.AddUnit('lib/ltelnet.pp');
    T:=P.Targets.AddUnit('lib/lftp.pp');
    with T.Dependencies do
      begin
      AddInclude('lib/lcontainers.inc');
      AddInclude('lib/lcontainersh.inc');
      end;
    T:=P.Targets.AddUnit('lib/lsmtp.pp');
    T:=P.Targets.AddUnit('lib/lhttp.pp');
    T:=P.Targets.AddUnit('lib/lhttputil.pp');
    T:=P.Targets.AddUnit('lib/fastcgi_base.pp');

    T:=P.Targets.AddExampleProgram('examples/console/ltelnet/ltclient.pp');
    T:=P.Targets.AddExampleProgram('examples/console/lftp/lftpclient.pp');
    T:=P.Targets.AddExampleProgram('examples/console/lsmtp/lsmtpclient.pp');
    T:=P.Targets.AddExampleProgram('examples/console/ltcp/lclient.pp');
    T:=P.Targets.AddExampleProgram('examples/console/ltcp/lserver.pp');
    T:=P.Targets.AddExampleProgram('examples/console/ludp/ludp.pp');
    T:=P.Targets.AddExampleProgram('examples/console/lhttp/fphttpd.pp');
    T:=P.Targets.AddExampleProgram('examples/console/lhttp/fpget.pp');
    P.Sources.AddExampleFiles('examples/console/lhttp/*',false,'examples/console/lhttp');
    P.Sources.AddExampleFiles('examples/console/lftp/*',false,'examples/console/lftp');
    P.Sources.AddExampleFiles('examples/console/lsmtp/*',false,'examples/console/lsmtp');
    P.Sources.AddExampleFiles('examples/console/ltcp/*',false,'examples/console/ltcp');
    P.Sources.AddExampleFiles('examples/console/ltelnet/*',false,'examples/console/ltelnet');
    P.Sources.AddExampleFiles('examples/console/ludp/*',false,'examples/console/ludp');
    P.Sources.AddExample('examples/console/Makefile','examples/console');
    P.Sources.AddExampleFiles('examples/visual/ftp/*',false,'examples/visual/ftp');
    P.Sources.AddExampleFiles('examples/visual/http/*',false,'examples/visual/http');
    P.Sources.AddExampleFiles('examples/visual/smtp/*',false,'examples/visual/smtp');
    P.Sources.AddExampleFiles('examples/visual/tcpudp/*',false,'examples/visual/tcpudp');
    P.Sources.AddExampleFiles('examples/visual/telnet/*',false,'examples/visual/telnet');
    P.Sources.AddExample('examples/console/units/empty.txt','examples/console/units');

    P.Sources.AddDoc('README');
    P.Sources.AddDoc('LICENSE.examples');
    P.Sources.AddDoc('CHANGELOG');
    P.Sources.AddDoc('INSTALL');
    P.Sources.AddDocFiles('doc/*');
    P.Sources.AddDocFiles('doc/en/*',false,'en');

    P.LazPackageFiles.AddLazPackageTemplate('lazaruspackage/lnetvisual.template');
    P.LazPackageFiles.AddLazFile('lazaruspackage/lnetcomponents.pas');
    P.LazPackageFiles.AddLazFile('lazaruspackage/lnetvisualreg.lrs');
    P.LazPackageFiles.AddLazFile('lazaruspackage/CHANGELOG');
    P.LazPackageFiles.AddLazFile('lazaruspackage/README');
    P.LazPackageFiles.AddLazFile('lazaruspackage/lclwineventer.inc');
    P.LazPackageFiles.AddLazFile('lazaruspackage/LICENSE');
    P.LazPackageFiles.AddLazFile('lazaruspackage/lclgtkeventer.inc');
    P.LazPackageFiles.AddLazFile('lazaruspackage/LICENSE.ADDON');
    P.LazPackageFiles.AddLazFile('lazaruspackage/INSTALL');
    P.LazPackageFiles.AddLazFile('lazaruspackage/lclwinceeventer.inc');
    P.LazPackageFiles.AddLazFile('lazaruspackage/lnetvisualreg.pas');
    P.LazPackageFiles.AddLazFile('lazaruspackage/lclnet.pas');

    P.LazPackageFiles.AddLazFile('lazaruspackage/icons/TLHTTPClientComponent.xpm','icons');
    P.LazPackageFiles.AddLazFile('lazaruspackage/icons/TLTelnetClientComponent.xpm','icons');
    P.LazPackageFiles.AddLazFile('lazaruspackage/icons/TLUdpComponent.xpm','icons');
    P.LazPackageFiles.AddLazFile('lazaruspackage/icons/TLTcpComponent.xpm','icons');
    P.LazPackageFiles.AddLazFile('lazaruspackage/icons/TLFtpClientComponent.xpm','icons');
    P.LazPackageFiles.AddLazFile('lazaruspackage/icons/TLHTTPServerComponent.xpm','icons');
    P.LazPackageFiles.AddLazFile('lazaruspackage/icons/TLSMTPClientComponent.xpm','icons');
    P.LazPackageFiles.AddLazFile('lazaruspackage/icons/TLSSLSessionComponent.xpm','icons');

    Run;
    end;
end.

