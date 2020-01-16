unit pmPackage;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpjsonrtti,
  fprGCollection,
  fprFPCVersion,
  fprInterfaceClasses,
  fprModel;

type
  { TpmManifest }

  TpmManifest = class
  private
    FAuthor: string;
    FFilename: string;
  published
    property Filename: string read FFilename write FFilename;
    property Author: string read FAuthor write FAuthor;
  end;

  { TpmPatchPackage }

  TpmPatchPackage = class
  private
    FCategoryId: Integer;
    FKeywordIds: TfprArrayOfInteger;
    FSupport: string;
  published
    property CategoryId: Integer read FCategoryId write FCategoryId;
    property KeywordIds: TfprArrayOfInteger read FKeywordIds write FKeywordIds;
    property Support: string read FSupport write FSupport;
  end;

implementation

end.

