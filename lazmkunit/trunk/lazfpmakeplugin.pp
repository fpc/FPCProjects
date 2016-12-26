unit LazFPMakePlugin;

{$mode objfpc}{$H+}

interface

uses
  fpmkunit;

type

  { TfpmResolvePackagePathsPlugin }

  TLazFPMakePlugin = class(TfpmResolvePackagePathsPlugin)
  public
    procedure ResolvePackagePath(ABuildEngine: TBuildEngine; APackage: TPackage;
      SearchPath: string; out AContinue: Boolean); override;
  end;

implementation

{ TfpmResolvePackagePathsPlugin }

procedure TLazFPMakePlugin.ResolvePackagePath(ABuildEngine: TBuildEngine;
  APackage: TPackage; SearchPath: string; out AContinue: Boolean);
begin
  AContinue := True;

  if APackage.Name = 'fcl' then
  begin
    ABuildEngine.Log(vldebug, 'Searching for fcl package as registration');
    if APackage.State=tsNotFound then
      begin
      APackage.State := tsNeutral;
      APackage.Directory := 'registration';
      inherited;
      APackage.State := tsNotFound;
      APackage.Directory := '';
      end;
  end;
  if APackage.Name = 'lclbase' then
  begin
    ABuildEngine.Log(vldebug, 'Searching for lclbase package as lcl');
    if APackage.State=tsNotFound then
      begin
      APackage.State := tsNeutral;
      APackage.Directory := 'lcl';
      inherited;
      APackage.State := tsNotFound;
      APackage.Directory := '';
      end;
  end;
  if APackage.Name = 'lcl' then
  begin
    ABuildEngine.Log(vldebug, 'Searching for lcl package as interfaces');
    if APackage.State=tsNotFound then
      begin
      APackage.State := tsNeutral;
      APackage.Directory := 'interfaces';
      inherited;
      APackage.State := tsNotFound;
      APackage.Directory := '';
      end;
  end;
end;

initialization
  GetPluginManager.RegisterPlugin(TLazFPMakePlugin);
end.

