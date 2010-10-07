{
    This file is part of the Free Pascal Profiler.
    Copyright (c) 2010 by Darius Blaszyk

    Free Pascal Profile application

    See the file COPYING.GPL, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit fppApplication;

{$mode objfpc}{$H+}

interface

uses
  Classes, CustApp, SysUtils, fppEnvironment;

type

  { TCustomFPPApplication }

  TCustomFPPApplication = class(TCustomApplication)
  protected
    Environment: TEnvironment;
    procedure Usage; virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure ShowProductInfo;
    procedure Run; virtual; abstract;
  end;

implementation

  { TCustomFPPApplication }

  procedure TCustomFPPApplication.ShowProductInfo;
  begin
    with Environment do
    begin
      WriteLn('GNU FreePascal profiler 0.1');
      WriteLn('Copyright 2007 Darius Blaszyk.');
      WriteLn('FPP is free software, covered by the GNU General Public License, and you are');
      WriteLn('welcome to change it and/or distribute copies of it under certain conditions.');
      WriteLn('There is absolutely no warranty for FPP.');
      WriteLn;
    end;
  end;

  procedure TCustomFPPApplication.Usage;
  begin
    writeln(Format('Usage: %s filename [options]', [ParamStr(0)]));
    writeln;
    writeln('Where options is one or more of the following:');
    writeln;
    writeln('    --help          This help screen');
    writeln('    --recursive     Recursively parse the unit paths');
    writeln('    --silent        Set verbosity off');
    writeln;
  end;

  constructor TCustomFPPApplication.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    Environment := TEnvironment.Create;
  end;

  destructor TCustomFPPApplication.Destroy;
  begin
    Environment.Free;
    inherited Destroy;
  end;

begin
end.

