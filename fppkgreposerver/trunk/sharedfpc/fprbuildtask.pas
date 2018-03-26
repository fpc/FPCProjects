unit fprBuildTask;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fgl,
  Generics.Collections;

type

  TfprBuildTaskState = (btsUnknown, btsWaiting, btsBuilding, btsFailed, btsSuccess);

  { TfprCustomBuildTask }

  TfprCustomBuildTask = class(TObject)
  private
    FUniqueString: string;
  public
    constructor Create(); virtual;
  published
    property UniqueString: string read FUniqueString write FUniqueString;
  end;

  { TfprSubBuildTask }

  TfprSubBuildTask = class(TfprCustomBuildTask)
  private
    FBuildAgent: string;
    FLog: string;
    FSourceBuild: Boolean;
    FState: TfprBuildTaskState;
  published
    property BuildAgent: string read FBuildAgent write FBuildAgent;
    property State: TfprBuildTaskState read FState write FState;
    property SourceBuild: Boolean read FSourceBuild write FSourceBuild;
    property Log: string read FLog write FLog;
  end;

  TfprSubBuildTaskList = specialize TFPGObjectList<TfprSubBuildTask>;

  { TfprBuildTask }

  TfprBuildTask = class(TfprCustomBuildTask)
  private
    FErrorMessage: string;
    FPackageName: string;
    FState: TfprBuildTaskState;
    FSubBuildTaskList: TfprSubBuildTaskList;
    FTag: string;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property PackageName: string read FPackageName write FPackageName;
    property State: TfprBuildTaskState read FState write FState;
    property Tag: string read FTag write FTag;
    property SubTasks: TfprSubBuildTaskList read FSubBuildTaskList;
    property ErrorMessage: string read FErrorMessage write FErrorMessage;
  end;

  TfprCustomBuildTaskList = specialize TFPGObjectList<TfprBuildTask>;

  { TfprBuildTaskList }

  TfprBuildTaskList = class(TfprCustomBuildTaskList)
  public
    function FindByUniqueString(AnUniqueString: string): TfprCustomBuildTask;
  end;

implementation

{ TfprBuildTaskList }

function TfprBuildTaskList.FindByUniqueString(AnUniqueString: string): TfprCustomBuildTask;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to count -1 do
    begin
    if Items[i].UniqueString = AnUniqueString then
      begin
      Result := Items[i];
      break;
      end;
    end;
end;

{ TfprCustomBuildTask }

constructor TfprCustomBuildTask.Create;
var
  i: Integer;
begin
  for i := 0 to 15 do
    begin
    FUniqueString := FUniqueString + chr(ord('a') + Random(26));
    end;
end;

{ TfprBuildTask }

constructor TfprBuildTask.Create;
begin
  inherited;
  FSubBuildTaskList := TfprSubBuildTaskList.Create(True);
end;

destructor TfprBuildTask.Destroy;
begin
  FSubBuildTaskList.Free;
  inherited Destroy;
end;

end.

