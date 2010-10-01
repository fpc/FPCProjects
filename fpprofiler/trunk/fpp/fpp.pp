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
  Classes, Process, CustApp, SysUtils, FPPEnvironment, FPPUtils, FPPModifyCode;

type

  { TFPPApplication }

  TFPPApplication = class(TCustomApplication)
  private
    Environment: TEnvironment;
    Verbose: boolean;
    procedure ShowProductInfo;
    procedure Show(msg: string);
    procedure Compile;
    procedure Usage;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Run;
  end;

var
  Application: TFPPApplication;

  { TFPPApplication }

  procedure TFPPApplication.ShowProductInfo;
  begin
    writeln('GNU FreePascal profiler 0.1');
    writeln('Copyright 2007 Darius Blaszyk.');
    writeln('FPP is free software, covered by the GNU General Public License, and you are');
    writeln('welcome to change it and/or distribute copies of it under certain conditions.');
    writeln('There is absolutely no warranty for FPP.');
    writeln;
  end;

  procedure TFPPApplication.Show(msg: string);
  begin
    if Verbose then
      Writeln(msg);
  end;

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
    writeln(Format('Usage: %s filename [options]', [ParamStr(0)]));
    writeln;
    writeln('Where options is one or more of the following:');
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

      writeln('executing: ', FPCProcess.CommandLine);
      writeln;

      FPCProcess.Options := FPCProcess.Options + [poWaitOnExit];
      FPCProcess.Execute;
    finally
      FPCProcess.Free;
    end;
  end;

  constructor TFPPApplication.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    Environment := TEnvironment.Create;
  end;

  destructor TFPPApplication.Destroy;
  begin
    Environment.Free;
    inherited Destroy;
  end;

  procedure TFPPApplication.Run;
  begin
    // TODO: Add a Silent option with no output
    ShowProductInfo;

    if HasOption('h', 'help') then
    begin
      Usage;
      exit;
    end;

    //insert profiling code
    if not HasOption('i', 'no-insert') then
      InsertProfilingCode(Environment.FileList('.pp;.pas;.inc;.lpr'), @ModifyCode);

    //compile the sources
    Compile;

    //backup profiled code
    if HasOption('backup') then
      BackupProfilingCode(Environment.FileList('.fpprof'));

    //remove the profiling code
    if not HasOption('r', 'no-remove') then
      RemoveProfilingCode(Environment.FileList('.fpprof'));
  end;

begin
  Application := TFPPApplication.Create(nil);
  Application.Run;
  Application.Free;
end.
