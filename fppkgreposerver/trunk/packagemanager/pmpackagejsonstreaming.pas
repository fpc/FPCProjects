unit pmPackageJSonStreaming;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fgl,
  fpjsonrtti,
  fpjson,
  typinfo,
  fprErrorHandling,
  pmPackage;

type

  { TpmPackageJSonStreaming }

  TpmPackageJSonStreaming = class
  private
    function ListToJSon(AList: TFPSList): TJSONArray;

    Procedure StreamerStreamProperty(Sender: TObject; AObject: TObject; Info: PPropInfo; var Res: TJSONData);
  public
    function PackageToJSon(APackage: TpmPackage): string;
    procedure JSonToPackage(AJSonStr: String; APackage: TpmPackage);

    function PackageListToJSon(APackageList: TpmPackageList): string;
    function PackageVersionListToJSon(APackageVersionList: TpmPackageVersionList): string;
  end;

implementation

{ TpmPackageJSonStreaming }

function TpmPackageJSonStreaming.ListToJSon(AList: TFPSList): TJSONArray;
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
      For I:=0 to AList.Count-1 do
        JA.Add(Streamer.ObjectToJSON(TObject(AList.Items[i]^)));
      Result := JA;
      JA := nil;
    finally
      JA.Free;
    end;

  finally
    Streamer.Free;
  end;
end;

Procedure TpmPackageJSonStreaming.StreamerStreamProperty(Sender: TObject; AObject: TObject; Info: PPropInfo; var Res: TJSONData);
var
  PackageState: TpmPackageState;
  AnObject: TObject;
begin
  if Info^.Name='PackageState' then
    begin
    PackageState := TpmPackageState(GetOrdProp(AObject, Info));
    Res := TJSONString.Create(CpmPackageStateString[PackageState]);
    end
  else if Info^.PropType^.Kind = tkClass then
    begin
    AnObject := GetObjectProp(AObject, Info);
    if AnObject is TpmPackageVersionList then
      Res := ListToJSon(TpmPackageVersionList(AnObject));
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
  JSONArr: TJSONArray;
begin
  JSONArr := ListToJSon(APackageList);
  try
    Result := JSONArr.AsJSON;
  finally
    JSONArr.Free;
  end;
end;

function TpmPackageJSonStreaming.PackageVersionListToJSon(APackageVersionList: TpmPackageVersionList): string;
var
  JSONArr: TJSONArray;
begin
  JSONArr := ListToJSon(APackageVersionList);
  try
    Result := JSONArr.AsJSON;
  finally
    JSONArr.Free;
  end;
end;

end.

