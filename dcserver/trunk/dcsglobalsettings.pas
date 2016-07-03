unit dcsGlobalSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  lazCollections,
  fgl,
  CustApp;

type

  { TDCSSetting }
  TDCSSettingValueType = (dcsVTString, dcsVTBoolean);
  TDCSSettingParameter = (dcsPNoParameter, dcsPOptionalParameter, dcsPHasParameter);

  TDCSSetting = class
  private
    FKey: string;
    FParameter: TDCSSettingParameter;
    FParameterLetter: Char;
    FParameterName: string;
    FSection: string;
    FSettingName: string;
    FValueType: TDCSSettingValueType;
    FBooleanValue: Boolean;
    FStringValue: string;
    FDefaultValue: string;
  public
    constructor Create(ASettingName, ASection, AKey, AParameterName: string; AParameterLetter: char; AParameter: TDCSSettingParameter; ADefaultValue: string);
    procedure SetAsBoolean(AValue: Boolean);
    procedure SetAsString(AValue: String);
    function GetAsBoolean: Boolean;
    function GetAsString: String;

    property SettingName: string read FSettingName;
    property Section: string read FSection;
    property Key: string read FKey;
    property ParameterName: string read FParameterName;
    property ParameterLetter: Char read FParameterLetter;
    property Parameter: TDCSSettingParameter read FParameter;
    property DefaultValue: string read FDefaultValue;
  end;

  TDCSSettingList = specialize TFPGObjectList<TDCSSetting>;

  { TDCSGlobalSettings }

  TDCSGlobalSettings = class
  private
    class var FInstance: TDCSGlobalSettings;
  private
    FMonitor: TLazMonitor;
    FSettingList: TDCSSettingList;
    function GetSetting(ASettingName: string): TDCSSetting;
  public
    constructor Create();
    destructor Destroy; override;
    class destructor Destroy;
    procedure AddSetting(ASettingName, ASection, AKey, AParameterName: string; AParameterLetter: char;
      AParameter: TDCSSettingParameter; ADefaultValue: string = '');
    procedure LoadSettingsFromIniStream(AStream: TStream);
    function CheckProgramParameters(AnApplication: TCustomApplication; AllErrors: Boolean = False): string;
    procedure EvaluateProgramParameters(AnApplication: TCustomApplication);

    function GetSettingAsBoolean(ASettingName: string): Boolean;
    function GetSettingAsString(ASettingName: string): String;
    procedure SetSettingAsBoolean(ASettingName: string; AValue: Boolean);
    procedure SetSettingAsString(ASettingName: string; AValue: String);

    class function GetInstance: TDCSGlobalSettings;
  end;

implementation


constructor TDCSSetting.Create(ASettingName, ASection, AKey, AParameterName: string;
  AParameterLetter: char; AParameter: TDCSSettingParameter; ADefaultValue: string);
begin
  inherited Create;
  if ASettingName = '' then
    raise Exception.Create('Can not create a setting without a name');
  FSettingName := ASettingName;
  FSection := ASection;
  FKey := AKey;
  FParameterName := AParameterName;
  FParameterLetter := AParameterLetter;
  FDefaultValue := ADefaultValue;
  FParameter := AParameter;
end;

procedure TDCSSetting.SetAsBoolean(AValue: Boolean);
begin
  FBooleanValue := AValue;
  FValueType := dcsVTBoolean;
end;

procedure TDCSSetting.SetAsString(AValue: String);
begin
  FStringValue := AValue;
  FValueType := dcsVTString;
end;

function TDCSSetting.GetAsBoolean: Boolean;
begin
  case FValueType of
    dcsVTString: Result := StrToBoolDef(FStringValue, False);
    dcsVTBoolean: Result := FBooleanValue;
  end;
end;

function TDCSSetting.GetAsString: String;
begin
  case FValueType of
    dcsVTString: Result := FStringValue;
    dcsVTBoolean: Result := BoolToStr(FBooleanValue);
  end;
end;

{ TDCSGlobalSettings }

function TDCSGlobalSettings.GetSetting(ASettingName: string): TDCSSetting;
var
  Setting: TDCSSetting;
  i: Integer;
begin
  for i := 0 to FSettingList.Count-1 do
    begin
    Setting := FSettingList.Items[i];
    if Setting.SettingName=ASettingName then
      begin
      Result := Setting;
      Exit;
      end;
    end;
  Result := nil;
end;

constructor TDCSGlobalSettings.Create;
begin
  FMonitor := TLazMonitor.create;
  FSettingList := TDCSSettingList.Create(True);
end;

destructor TDCSGlobalSettings.Destroy;
begin
  FMonitor.Free;
  FSettingList.Free;
  inherited Destroy;
end;

class destructor TDCSGlobalSettings.Destroy;
begin
  FInstance.Free;
end;

procedure TDCSGlobalSettings.AddSetting(ASettingName, ASection, AKey, AParameterName: string;
  AParameterLetter: char; AParameter: TDCSSettingParameter; ADefaultValue: string = '');
var
  Setting: TDCSSetting;
begin
  FMonitor.Acquire;
  try
    Setting := GetSetting(ASettingName);
    if Assigned(Setting) then
      raise Exception.CreateFmt('Parameter %s is already defined.',[ASettingName]);
    Setting := TDCSSetting.Create(ASettingName, ASection, AKey, AParameterName, AParameterLetter, AParameter, ADefaultValue);
    FSettingList.Add(Setting);
  finally
    FMonitor.Release;
  end;
end;

procedure TDCSGlobalSettings.LoadSettingsFromIniStream(AStream: TStream);
begin

end;

function TDCSGlobalSettings.CheckProgramParameters(AnApplication: TCustomApplication; AllErrors: Boolean = False): string;
var
  i: integer;
  Setting: TDCSSetting;
  ShortOpts: string;
  LongOpts: TStrings;
begin
  LongOpts := TStringList.Create;
  try
    FMonitor.Acquire;
    try
      for i := 0 to FSettingList.Count -1 do
        begin
        Setting := FSettingList.Items[i];
        if Setting.ParameterLetter<>'' then
          begin
          ShortOpts := ShortOpts + Setting.ParameterLetter;
          case Setting.Parameter of
            dcsPHasParameter : ShortOpts := ShortOpts + ':';
            dcsPOptionalParameter : ShortOpts := ShortOpts + '::';
          end; {case}
          end;
        if Setting.ParameterName<>'' then
          begin
          case Setting.Parameter of
            dcsPHasParameter : LongOpts.Add(Setting.ParameterName+':');
            dcsPOptionalParameter : LongOpts.Add(Setting.ParameterName+'::');
            dcsPNoParameter: LongOpts.Add(Setting.ParameterName);
          end; {case}
          end;
        end;
    finally
      FMonitor.Release;
    end;
    Result := AnApplication.CheckOptions(ShortOpts, LongOpts, AllErrors);
  finally
    LongOpts.Free;
  end;
end;

procedure TDCSGlobalSettings.EvaluateProgramParameters(AnApplication: TCustomApplication);
var
  i: integer;
  Setting: TDCSSetting;
  s: string;
begin
  FMonitor.Acquire;
  try
    for i := 0 to FSettingList.Count -1 do
      begin
      Setting := FSettingList.Items[i];

      if ((Setting.ParameterLetter<>'') or (Setting.ParameterName<>'')) and AnApplication.HasOption(Setting.ParameterLetter, Setting.ParameterName) then
        begin
        case Setting.Parameter of
          dcsPNoParameter: Setting.SetAsBoolean(True);
          dcsPHasParameter: Setting.SetAsString(AnApplication.GetOptionValue(Setting.ParameterLetter, Setting.ParameterName));
          dcsPOptionalParameter:
            begin
            s := AnApplication.GetOptionValue(Setting.ParameterLetter, Setting.ParameterName);
            if s <> '' then
              Setting.SetAsString(s)
            else if Setting.DefaultValue <> '' then
              Setting.SetAsString(Setting.DefaultValue)
            else
              Setting.SetAsBoolean(True);
            end;
        end; {case}
        end;
      end;
  finally
    FMonitor.Release;
  end;
end;

function TDCSGlobalSettings.GetSettingAsBoolean(ASettingName: string): Boolean;
var
  Setting: TDCSSetting;
begin
  FMonitor.Acquire;
  try
    Setting := GetSetting(ASettingName);
    if not assigned(Setting) then
      raise Exception.CreateFmt('Parameter %s has not been defined.', [ASettingName]);
    Result := Setting.GetAsBoolean
  finally
    FMonitor.Release;
  end;
end;

function TDCSGlobalSettings.GetSettingAsString(ASettingName: string): String;
var
  Setting: TDCSSetting;
begin
  FMonitor.Acquire;
  try
    Setting := GetSetting(ASettingName);
    if not assigned(Setting) then
      raise Exception.CreateFmt('Parameter %s has not been defined.', [ASettingName]);
    Result := Setting.GetAsString
  finally
    FMonitor.Release;
  end;
end;

procedure TDCSGlobalSettings.SetSettingAsBoolean(ASettingName: string; AValue: Boolean);
var
  Setting: TDCSSetting;
begin
  FMonitor.Acquire;
  try
    Setting := GetSetting(ASettingName);
    if not assigned(Setting) then
      raise Exception.CreateFmt('Parameter %s has not been defined.', [ASettingName]);
    Setting.SetAsBoolean(AValue);
  finally
    FMonitor.Release;
  end;
end;

procedure TDCSGlobalSettings.SetSettingAsString(ASettingName: string; AValue: String);
var
  Setting: TDCSSetting;
begin
  FMonitor.Acquire;
  try
    Setting := GetSetting(ASettingName);
    if not assigned(Setting) then
      raise Exception.CreateFmt('Parameter %s has not been defined.', [ASettingName]);
    Setting.SetAsString(AValue);
  finally
    FMonitor.Release;
  end;
end;

class function TDCSGlobalSettings.GetInstance: TDCSGlobalSettings;
begin
  if not Assigned(FInstance) then
    FInstance := TDCSGlobalSettings.Create;
  Result := FInstance;
end;

end.

