unit fppEnvironment;

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
    FRecursive: boolean;
    FVerbose: boolean;
    procedure AddCmdParameter(Parameter: string);
    procedure AddSearchPath(path: string);
    procedure SetRecursive(const AValue: boolean);
    procedure SetVerbose(const AValue: boolean);
  public
    constructor Create;
    destructor Destroy; override;

    function FileList(ExtensionMask: string): TStrings;
    property CommandLine: string read FCommandLine write FCommandLine;
    property PathList: TStrings read FPathList write FPathList;
    property Recursive: boolean read FRecursive write SetRecursive;
    procedure ShowProductInfo;
    property Verbose: boolean read FVerbose write SetVerbose;

    procedure Write(msg: string);
    procedure WriteLn(msg: string);
    procedure WriteLn; overload;
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
  if Parameter = '--help' then
    exit;
  if Parameter = '--recursive' then
    exit;
  if Parameter = '--silent' then
    exit;

  //add the commandline parameter so it get's passed to FPC
  CommandLine := CommandLine + ' ' + Parameter;
end;

procedure TEnvironment.SetRecursive(const AValue: boolean);
begin
  FRecursive:=AValue;
end;

procedure TEnvironment.SetVerbose(const AValue: boolean);
begin
  FVerbose := AValue;
end;

procedure TEnvironment.WriteLn(msg: string);
begin
  if Verbose then
    System.Writeln(msg);
end;

procedure TEnvironment.WriteLn;
begin
  WriteLn('');
end;

procedure TEnvironment.Write(msg: string);
begin
  if Verbose then
    System.Write(msg);
end;

constructor TEnvironment.Create;
var
  i: integer;
  param: string;
  fppPath: string;
begin
  inherited Create;
  PathList := TStringList.Create;

  //add debugging info
  AddCmdParameter('-gl');

  //add fpprof unit path
  fppPath := GetEnvironmentVariable('fpprof');
  if fppPath <> '' then
    AddCmdParameter('-Fu' + fppPath);

  //default option settings
  Verbose := True;
  Recursive := False;

  //parse remaining parameters
  for i := 1 to ParamCount do
  begin
    param := ParamStr(i);
    AddCmdParameter(param);
    case param[1] of
      '-': begin
        if pos('-Fu', param) <> 0 then
          AddSearchPath(copy(param, 4, Length(param) - 3));
        if pos('--silent', param) <> 0 then
          Verbose := False;
        if pos('--recursive', param) <> 0 then
          Recursive := True;
      end
      else
        //assume the switchless parameter is a sourcefile
        //so check if it exists, should be a good indication if it's a filename
        if FileExists(ExpandFileName(param)) then
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
    FileSearch(PathList[i], ExtensionMask, Result, Recursive);
end;

procedure TEnvironment.ShowProductInfo;
begin
  WriteLn('GNU FreePascal profiler 0.1');
  WriteLn('Copyright 2007 Darius Blaszyk.');
  WriteLn('FPP is free software, covered by the GNU General Public License, and you are');
  WriteLn('welcome to change it and/or distribute copies of it under certain conditions.');
  WriteLn('There is absolutely no warranty for FPP.');
  WriteLn;
end;

end.

