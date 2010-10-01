unit FPPEnvironment;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPPUtils;

type
  { TEnvironment }

  TEnvironment = class(TObject)
  private
    FCommandLine: string;
    FPathList: TStrings;
    procedure AddSearchPath(path: string);
    procedure AddCmdParameter(Parameter: string);
  public
    constructor Create;
    destructor Destroy; override;
    property CommandLine: string read FCommandLine write FCommandLine;
    property PathList: TStrings read FPathList write FPathList;
    function FileList(ExtensionMask: string): TStrings;
  end;

implementation

procedure TEnvironment.AddSearchPath(path: string);
begin
  if DirectoryExists(path) then
    PathList.Add(path);
end;

procedure TEnvironment.AddCmdParameter(Parameter: string);
begin
  //check if commandline parameter needs skipping
  if Parameter = '-r' then
    exit;
  if Parameter = '--backup' then
    exit;

  //add the commandline parameter so it get's passed to FPC
  CommandLine := CommandLine + ' ' + Parameter;
end;

constructor TEnvironment.Create;
var
  i: integer;
  param: string;
begin
  inherited Create;
  PathList := TStringList.Create;

  //add debugging info and fpprof unit path
  AddCmdParameter('-gl');
  AddCmdParameter('-Fu' + GetEnvironmentVariable('fpprof'));

  for i := 1 to ParamCount do
  begin
    AddCmdParameter(ParamStr(i));
    param := ParamStr(i);
    case param[1] of
      '-':
        if pos('-Fu', ParamStr(i)) <> 0 then
          AddSearchPath(copy(ParamStr(i), 4, Length(ParamStr(i)) - 3));
      else
        AddSearchPath(ExtractFilePath(ExpandFileName(param)));
    end;
  end;
end;

destructor TEnvironment.Destroy;
begin
  PathList.Free;
  inherited Destroy;
end;

function TEnvironment.FileList(ExtensionMask: string): TStrings;
var
  i: integer;
begin
  Result := TStringList.Create;
  for i := 0 to PathList.Count - 1 do
    FileSearch(PathList[i], ExtensionMask, Result);
end;

end.

