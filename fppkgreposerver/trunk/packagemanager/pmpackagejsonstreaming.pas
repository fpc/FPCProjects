unit pmPackageJSonStreaming;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpjsonrtti,
  fpjson,
  typinfo,
  pmErrorHandling,
  pmPackage;

type

  { TpmPackageJSonStreaming }

  TpmPackageJSonStreaming = class
  private
    Procedure StreamerStreamProperty(Sender: TObject; AObject: TObject; Info: PPropInfo; var Res: TJSONData);
  public
    function PackageToJSon(APackage: TpmPackage): string;
    procedure JSonToPackage(AJSonStr: String; APackage: TpmPackage);

    function PackageListToJSon(APackageList: TpmPackageList): string;
  end;

implementation

{ TpmPackageJSonStreaming }

Procedure TpmPackageJSonStreaming.StreamerStreamProperty(Sender: TObject; AObject: TObject; Info: PPropInfo; var Res: TJSONData);
var
  PackageState: TpmPackageState;
begin
  if Info^.Name='PackageState' then
    begin
    PackageState := TpmPackageState(GetOrdProp(AObject, Info));
    Res := TJSONString.Create(CpmPackageStateString[PackageState]);
    end;
end;

function TpmPackageJSonStreaming.PackageToJSon(APackage: TpmPackage): string;
var
  Streamer: TJSONStreamer;
  JO: TJSONObject;
begin
  Streamer := TJSONStreamer.Create(nil);
  try
    Streamer.OnStreamProperty := @StreamerStreamProperty;
    Streamer.Options := Streamer.Options + [jsoLowerPropertyNames];
    JO := Streamer.ObjectToJSON(APackage);
    try
      Result := JO.AsJSON;
    finally
      JO.Free;
    end;
  finally
    Streamer.Free;
  end;
end;

procedure TpmPackageJSonStreaming.JSonToPackage(AJSonStr: String; APackage: TpmPackage);
var
  DeStreamer: TJSONDeStreamer;
begin
  DeStreamer := TJSONDeStreamer.Create(nil);
  try
    DeStreamer.Options := [jdoCaseInsensitive];
    try
      DeStreamer.JSONToObject(AJSonStr, APackage);
    except
      on E: Exception do
        raise EJsonWebException.CreateFmt('Failed to parse package-data: %s', [E.Message]);
    end;
  finally
    DeStreamer.Free;
  end;
end;

function TpmPackageJSonStreaming.PackageListToJSon(APackageList: TpmPackageList): string;
var
  Streamer: TJSONStreamer;
  JA: TJSONArray;
  i : Integer;
begin
  Streamer := TJSONStreamer.Create(nil);
  try
    Streamer.OnStreamProperty := @StreamerStreamProperty;
    Streamer.Options := Streamer.Options + [jsoLowerPropertyNames];

    JA:=TJSONArray.Create;
    try
      For I:=0 to APackageList.Count-1 do
        JA.Add(Streamer.ObjectToJSON(APackageList.Items[i]));
      Result := JA.AsJSON;
    finally
      JA.Free;
    end;

  finally
    Streamer.Free;
  end;
end;

end.

