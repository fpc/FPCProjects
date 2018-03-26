unit fprBuildAgentResponse;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  contnrs,
  generics.Collections,
  fgl,
  fprGCollection;

type

  { TfprBuildAgentResponse }

  TfprBuildAgentResponse = class(TCollectionItem)
  private
    FManifest: string;
    FMessage: string;
    FSourceArchive: string;
    FTimeStamp: string;
    FType: string;
  published
    property TimeStamp: string read FTimeStamp write FTimeStamp;
    property Message: string read FMessage write FMessage;
    property AType: string read FType write FType;
    property SourceArchive: string read FSourceArchive write FSourceArchive;
    property Manifest: string read FManifest write FManifest;
  end;

  TfprBuildAgentResponseList = specialize TcnocGCollection<tfprBuildAgentResponse>;

implementation

end.

