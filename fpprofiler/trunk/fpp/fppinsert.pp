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

program fppinsert;

{$mode objfpc}{$H+}

uses
  Classes, fppEnvironment, fppInsertCode, fppApplication;

type

  { TFPPInstertApp }

  TFPPInsertApp = class(TCustomFPPApplication)
  private
  protected
    procedure Usage; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure Run; override;
  end;

var
  Application: TFPPInsertApp;

  { TFPPInsertApp }

  procedure TFPPInsertApp.Usage;
  begin
    inherited Usage;

    writeln('    -Fu<x>          Add <x> to unit path');
    writeln;
  end;

  constructor TFPPInsertApp.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
  end;

  destructor TFPPInsertApp.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TFPPInsertApp.Run;
  var
    ins: TFPPInsertCode;
  begin
    ShowProductInfo;

    if HasOption('h') then
    begin
      Usage;
      exit;
    end;

    //insert profiling code
    try
      ins := TFPPInsertCode.Create(Environment);
      ins.Run
    finally
      ins.Free;
    end;
  end;

begin
  Application := TFPPInsertApp.Create(nil);
  Application.Run;
  Application.Free;
end.
