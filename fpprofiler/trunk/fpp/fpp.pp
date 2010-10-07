{
    This file is part of the Free Pascal Profiler.
    Copyright (c) 2007 by Darius Blaszyk

    Free Pascal Profile application

    See the file COPYING.GPL, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

program fpp;

{$mode objfpc}{$H+}

uses
  Classes, Process, CustApp, SysUtils, FPPUtils, fppEnvironment,
  fppInsertCode, fppRemoveCode, fppApplication;

type

  { TFPPApplication }

  TFPPApplication = class(TCustomFPPApplication)
  private
    Verbose: boolean;
    procedure Compile;
  protected
    procedure Usage; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Run; override;
  end;

var
  Application: TFPPApplication;

  { TFPPApplication }

  procedure TFPPApplication.Usage;

    procedure ShowOption(const C, LC, Msg: string);
    begin
      writeln(Format(' -%s --%-20s %s', [C, LC, MSG]));
    end;

    procedure ShowArgOption(const LC, Msg: string); overload;
    begin
      writeln(Format('    --%-20s %s', [LC, MSG]));
    end;

    procedure ShowArgOption(const C, LC, Msg: string); overload;
    begin
      writeln(Format(' -%s --%-20s %s', [C, LC, MSG]));
    end;

    procedure ShowArgOption(const C, LC, Value, Msg: string); overload;
    begin
      writeln(Format(' -%s --%-20s %s', [C, LC + '=' + Value, MSG]));
    end;

  begin
    inherited Usage;

    ShowOption('h', 'help', 'This screen.');
    ShowArgOption('backup', 'Backup profiled code.');
    ShowArgOption('i', 'no-insert', 'Do not insert profiling code.');
    ShowArgOption('r', 'no-remove', 'Do not remove profiling code.');
    writeln;
    writeln('Environment variable used:');
    writeln('  fpprof      points to the directory containing the fpprof.pas unit.');
    writeln('              current value is: ' + GetEnvironmentVariable('fpprof'));
    writeln('  fpc         points to the compiler binary to be used.');
    writeln('              current value is: ' + GetEnvironmentVariable('fpc'));
    writeln;
  end;

  procedure TFPPApplication.Compile;
  var
    FPCProcess: TProcess;
    lFPC: string;
  begin
    FPCProcess := TProcess.Create(nil);
    try
      lFPC := GetEnvironmentVariable('fpc');
      if lFPC = '' then
        lFPC := 'fpc';
      FPCProcess.CommandLine := lFPC + ' ' + Environment.CommandLine;

      Environment.WriteLn('executing: ' + FPCProcess.CommandLine);
      Environment.WriteLn;

      FPCProcess.Options := FPCProcess.Options + [poWaitOnExit];
      FPCProcess.Execute;
    finally
      FPCProcess.Free;
    end;
  end;

  constructor TFPPApplication.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
  end;

  destructor TFPPApplication.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TFPPApplication.Run;
  var
    ins: TFPPInsertCode;
    rem: TfppRemoveCode;
    fileList: TStrings;
  begin
    Environment.ShowProductInfo;

    if HasOption('h', 'help') then
    begin
      Usage;
      exit;
    end;

    //insert profiling code
    if not HasOption('i', 'no-insert') then
    begin
      try
        ins := TFPPInsertCode.Create(Environment);
        ins.Run;
      finally
        ins.Free;
      end;
    end;

    //compile the sources
    Compile;

    //backup profiled code
    if HasOption('backup') then
    begin
      fileList := Environment.FileList('.fpprof');
      BackupProfilingCode(fileList);
      fileList.Free;
    end;

    //remove the profiling code
    if not HasOption('r', 'no-remove') then
    begin
      try
        rem := TFPPRemoveCode.Create(Environment);
        rem.Run;
      finally
        rem.Free;
      end;
    end;
  end;

begin
  Application := TFPPApplication.Create(nil);
  Application.Run;
  Application.Free;
end.
