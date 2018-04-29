unit fprBuildAgentResponse;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  contnrs,
  fpjson,
  fprInterfacedCollection,
  fgl,
  fprGCollection,
  fprJSONRTTI;

type

  { TfprBuildAgentResponse }

  TfprBuildAgentResponse = class(TfprInterfacedCollectionItem, IfprJSONPropertyAttributes)
  private
    FManifest: TJSONData;
    FManifestXML: string;
    FMessage: string;
    FSourceArchive: string;
    FTimeStamp: string;
    FType: string;
  public
    function GetPropertyAttributes(PropertyName: string; out ElementName: string; out PropertyOptions: TfprJSONPropertyOptions): Boolean;
  published
    property TimeStamp: string read FTimeStamp write FTimeStamp;
    property Message: string read FMessage write FMessage;
    property AType: string read FType write FType;
    property SourceArchive: string read FSourceArchive write FSourceArchive;
    property Manifest: TJSONData read FManifest write FManifest;
    property ManifestXML: string read FManifestXML write FManifestXML;
  end;

  TfprBuildAgentResponseList = specialize TcnocGCollection<tfprBuildAgentResponse>;

implementation

{ TfprBuildAgentResponse }

function TfprBuildAgentResponse.GetPropertyAttributes(PropertyName: string; out ElementName: string;
  out PropertyOptions: TfprJSONPropertyOptions): Boolean;
begin
  Result := False;
  PropertyOptions := [];
  if PropertyName='AType' then
    begin
    ElementName := 'type';
    Result := True;
    end;
  if PropertyName='Manifest' then
    begin
    ElementName := 'manifest';
    PropertyOptions := [jpoJSONContents];
    Result := True;
    end;
end;

end.

