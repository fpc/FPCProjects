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

program fppremove;

{$mode objfpc}{$H+}

uses
  Classes, fppEnvironment, fppRemoveCode, fppApplication;

type

  { TFPPApplication }

  TFPPRemoveApp = class(TCustomFPPApplication)
  protected
    procedure Usage; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Run; override;
  end;

var
  Application: TFPPRemoveApp;

  { TFPPRemoveApp }

  procedure TFPPRemoveApp.Usage;
  begin
    inherited Usage;

    writeln('    -Fu<x>          Add <x> to unit path');
    writeln;
    writeln;
  end;

  constructor TFPPRemoveApp.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
  end;

  destructor TFPPRemoveApp.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TFPPRemoveApp.Run;
  var
    rem: TFPPRemoveCode;
  begin
    ShowProductInfo;

    if HasOption('h') then
    begin
      Usage;
      exit;
    end;

    //remove profiling code
    try
      rem := TFPPRemoveCode.Create(Environment);
      rem.Run
    finally
      rem.Free;
    end;
  end;

{$R *.res}

begin
  Application := TFPPRemoveApp.Create(nil);
  Application.Run;
  Application.Free;
end.

