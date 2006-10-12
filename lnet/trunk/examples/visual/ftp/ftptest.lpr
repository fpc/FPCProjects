program ftptest;

{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, Main, lnetpackage, lnetidepackage, sitesunit, 
dleparsers;

begin
  Application.Title:='FTP Test case';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

