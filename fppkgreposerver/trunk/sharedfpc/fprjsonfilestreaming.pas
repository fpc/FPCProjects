unit fprJSONFileStreaming;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpjson,
  fpjsonrtti,
  fprJSONRTTI;

procedure LoadCollectionFromJSONFile(ACollection: TCollection; AFilename: string);
procedure SaveCollectionToJSONFile(ACollection: TCollection; AFilename: string);

implementation

procedure LoadCollectionFromJSONFile(ACollection: TCollection; AFilename: string);
var
  DeStreamer: TfprJSONDeStreamer;
  FS: TStringStream;
begin
  DeStreamer := TfprJSONDeStreamer.Create(nil);
  try
    FS := TStringStream.Create();
    try
      FS.LoadFromFile(AFileName);
      FS.Seek(0, soFromBeginning);
      DeStreamer.JSONToCollection(FS.DataString, ACollection);
    finally
      FS.Free;
    end;
  finally
    DeStreamer.Free;
  end;

end;

procedure SaveCollectionToJSONFile(ACollection: TCollection; AFilename: string);
var
  Streamer: TJSONStreamer;
  FS: TStringStream;
begin
  Streamer := TJSONStreamer.Create(nil);
  try
    FS := TStringStream.Create(Streamer.ObjectToJSONString(ACollection));
    try
      FS.SaveToFile(AFileName);
    finally
      FS.Free;
    end;
  finally
    Streamer.Free;
  end;
end;

end.

