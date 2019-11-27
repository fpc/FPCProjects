unit dcsGlobalSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  IniFiles,
  lazCollections,
  fgl,
  CustApp;

type

  { TDCSSetting }
  TDCSSettingValueType = (dcsVTString, dcsVTBoolean);
  TDCSSettingParameter = (dcsPNoParameter, dcsPOptionalParameter, dcsPHasParameter, dcsPDictionary);

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
    FNameValueList: TStringList;
  public
    constructor Create(ASettingName, ASection, AKey, AParameterName: string; AParameterLetter: char; AParameter: TDCSSettingParameter; ADefaultValue: string);
    destructor Destroy; override;
    procedure SetAsBoolean(AValue: Boolean);
    procedure SetAsString(AValue: String);
    procedure SetNameValueAsString(AName, AValue: string);
    function GetNameValueAsString(AName: string): string;
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

  { TDCSSettingTemplate }

  TDCSSettingTemplate = class
  private
    FSectionPrefix: string;
    FValues: TStringList;
  public
    constructor Create(ASectionPrefix: string);
    destructor Destroy; override;
    property SectionPrefix: string read FSectionPrefix write FSectionPrefix;
    property Values: TStringList read FValues;
  end;

  TDCSCustomSettingTemplateList = specialize TFPGObjectList<TDCSSettingTemplate>;

  { TDCSSettingTemplateList }

  TDCSSettingTemplateList = class(TDCSCustomSettingTemplateList)
  public
    function FindSettingTemplate(ASectionPrefix: string): TDCSSettingTemplate;
  end;

  { TDCSGlobalSettings }

  TDCSGlobalSettings = class
  private
    class var FInstance: TDCSGlobalSettings;
  private
    FMonitor: TLazMonitor;
    FSettingList: TDCSSettingList;
    FDefinedSettingTemplateList: TDCSSettingList;
    FSettingTemplateList: TDCSSettingTemplateList;
    function GetSettingFromList(AList: TDCSSettingList; ASettingName: string): TDCSSetting;
    function GetSetting(ASettingName: string): TDCSSetting;
    function GetSettingTemplate(ASettingNamePrefix: string): TDCSSetting;
    function GetSettingByKey(ASection, AKey: string): TDCSSetting;
    procedure AddSettingsForTemplate(ASectionPrefix, ASectionSuffix: string);
  public
    constructor Create();
    destructor Destroy; override;
    class destructor Destroy;
    procedure AddSetting(ASettingName, ASection, AKey, AParameterName: string; AParameterLetter: char;
      AParameter: TDCSSettingParameter; ADefaultValue: string = '');
    procedure LoadSettingsFromIniStream(AStream: TStream);
    procedure LoadSettingsFromIniFile(AFilename: string);
    procedure LoadSettingsFromEnvironment();
    function CheckProgramParameters(AnApplication: TCustomApplication; AllErrors: Boolean = False): string;
    procedure EvaluateProgramParameters(AnApplication: TCustomApplication);

    procedure AddSettingTemplate(ASectionPrefix, AKey, ASettingNamePrefix, ADefaultValue: string);

    function GetDictionarySettingAsString(ASettingName, AName: string): string;

    function GetSettingAsBoolean(ASettingName: string): Boolean;
    function GetSettingAsString(ASettingName: string): String;
    procedure SetSettingAsBoolean(ASettingName: string; AValue: Boolean);
    procedure SetSettingAsString(ASettingName: string; AValue: String);

    function GetSettingAsBooleanByKey(ASection, AKey: string): Boolean;
    function GetSettingAsStringByKey(ASection, AKey: string): String;
    procedure SetSettingAsBooleanByKey(ASection, AKey: string; AValue: Boolean);
    procedure SetSettingAsStringByKey(ASection, AKey: string; AValue: String);

    procedure FillSectionList(SectionList: TStrings);

    property SettingTemplateList: TDCSSettingTemplateList read FSettingTemplateList;

    class function GetInstance: TDCSGlobalSettings;
  end;

implementation

{ TDCSSettingTemplateList }

function TDCSSettingTemplateList.FindSettingTemplate(ASectionPrefix: string): TDCSSettingTemplate;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count -1 do
    begin
    if Items[i].SectionPrefix = ASectionPrefix then
      begin
      Result := Items[i];
      break;
      end;
    end;
end;

{ TDCSSettingTemplate }

constructor TDCSSettingTemplate.Create(ASectionPrefix: string);
begin
  FSectionPrefix := ASectionPrefix;
  FValues := TStringList.Create;
  FValues.Sorted := True;
  FValues.Duplicates := dupIgnore;
end;

destructor TDCSSettingTemplate.Destroy;
begin
  FValues.Free;
  inherited Destroy;
end;


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
  if FParameter = dcsPDictionary then
    FNameValueList := TStringList.Create;
end;

destructor TDCSSetting.Destroy;
begin
  FNameValueList.Free;
  inherited Destroy;
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

procedure TDCSSetting.SetNameValueAsString(AName, AValue: string);
begin
  FNameValueList.Values[AName] := AValue;
end;

function TDCSSetting.GetNameValueAsString(AName: string): string;
begin
  Result := FNameValueList.Values[AName];
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

function TDCSGlobalSettings.GetSettingFromList(AList: TDCSSettingList; ASettingName: string): TDCSSetting;
var
  Setting: TDCSSetting;
  i: Integer;
begin
  for i := 0 to AList.Count-1 do
    begin
    Setting := AList.Items[i];
    if Setting.SettingName=ASettingName then
      begin
      Result := Setting;
      Exit;
      end;
    end;
  Result := nil;
end;

function TDCSGlobalSettings.GetSetting(ASettingName: string): TDCSSetting;
begin
  Result := GetSettingFromList(FSettingList, ASettingName);
end;

function TDCSGlobalSettings.GetSettingTemplate(ASettingNamePrefix: string): TDCSSetting;
begin
  Result := GetSettingFromList(FDefinedSettingTemplateList, ASettingNamePrefix);
end;

function TDCSGlobalSettings.GetSettingByKey(ASection, AKey: string): TDCSSetting;
var
  Setting: TDCSSetting;
  i: Integer;
begin
  for i := 0 to FSettingList.Count-1 do
    begin
    Setting := FSettingList.Items[i];
    if SameText(Setting.Section, ASection) then
      begin
      if (Setting.FParameter = dcsPDictionary) or SameText(Setting.Key, AKey) then
        begin
        Result := Setting;
        Exit;
        end;
      end;
    end;
  Result := nil;
end;

procedure TDCSGlobalSettings.AddSettingsForTemplate(ASectionPrefix, ASectionSuffix: string);
var
  i: Integer;
  Setting: TDCSSetting;
  SettingName: String;
  SettingTemplate: TDCSSettingTemplate;
begin
  for i := 0 to FDefinedSettingTemplateList.Count -1 do
    begin
    Setting := FDefinedSettingTemplateList.Items[i];
    if Setting.Section = ASectionPrefix then
      begin
      SettingTemplate := FSettingTemplateList.FindSettingTemplate(ASectionPrefix);
      if Assigned(SettingTemplate) then
        SettingTemplate.Values.Add(ASectionSuffix);

      SettingName := Setting.SettingName+ASectionSuffix;
      Setting := TDCSSetting.Create(SettingName, ASectionPrefix+ASectionSuffix, Setting.Key, '', #0, dcsPHasParameter, Setting.DefaultValue);
      FSettingList.Add(Setting);
      end;
    end;
end;

constructor TDCSGlobalSettings.Create();
begin
  FMonitor := TLazMonitor.create;
  FSettingList := TDCSSettingList.Create(True);
  FSettingTemplateList := TDCSSettingTemplateList.Create(True);
  FDefinedSettingTemplateList := TDCSSettingList.Create(True);
end;

destructor TDCSGlobalSettings.Destroy;
begin
  FMonitor.Free;
  FSettingList.Free;
  FDefinedSettingTemplateList.Free;
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
  if (AParameter <> dcsPOptionalParameter) and (ADefaultValue<>'') then
    raise Exception.Create('A default value only makes sense for optional parameters.');
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
var
  IniFile: TIniFile;
  SectionList: TStringList;
  ValueList: TStringList;
  Section, Key, Value: string;
  i, j, k: Integer;
  Setting: TDCSSetting;
begin
  IniFile := TIniFile.Create(AStream);
  try
    SectionList := TStringList.Create;
    try
      IniFile.ReadSections(SectionList);
      for i := 0 to SectionList.Count -1 do
        begin
        Section := SectionList.Strings[i];
        ValueList := TStringList.Create;
        try
          IniFile.ReadSectionValues(Section, ValueList, []);
          for j := 0 to ValueList.Count -1 do
            begin
            Key := ValueList.Names[j];
            Value := ValueList.ValueFromIndex[j];
            if Assigned(GetSettingByKey(Section, Key)) then
              SetSettingAsStringByKey(Section, Key, Value)
            else
              begin
              for k := 0 to FDefinedSettingTemplateList.Count-1 do
                begin
                Setting := FDefinedSettingTemplateList.Items[k];
                if SameText(Setting.Section, copy(Section, 1, Length(Setting.Section))) then
                  begin
                  AddSettingsForTemplate(Setting.Section, copy(Section, Length(Setting.Section) +1, MaxSIntValue));
                  Break;
                  end
                end;
              SetSettingAsStringByKey(Section, Key, Value)
              end;
            end;
        finally
          ValueList.Free;
        end;
        end;
    finally
      SectionList.Free;
    end;
  finally
    IniFile.Free;
  end;
end;

procedure TDCSGlobalSettings.LoadSettingsFromEnvironment();
var
  Setting: TDCSSetting;
  i,k,p: Integer;
  Key, Section, Value, s: String;
begin
  for i := 0 to GetEnvironmentVariableCount -1 do
    begin
    s := GetEnvironmentString(i);
    p := pos('__', s);
    if p > 0 then
      begin
      Section := Copy(s, 1, p-1);
      s := Copy(s, p+2, Length(s));
      p := pos('=', s);
      if p > 0 then
        begin
        Key := copy(s,1,p-1);
        Value := copy(s, p+1, Length(s));

        Setting := GetSettingByKey(Section, Key);
        if Assigned(Setting) then
          begin
          Setting.SetAsString(Value);
          end
        else
          begin
          for k := 0 to FDefinedSettingTemplateList.Count-1 do
            begin
            Setting := FDefinedSettingTemplateList.Items[k];
            if SameText(Setting.Section, copy(Section, 1, Length(Setting.Section))) then
              begin
              AddSettingsForTemplate(Setting.Section, copy(Section, Length(Setting.Section) +1, MaxSIntValue));
              SetSettingAsStringByKey(Section, Key, Value);
              Break;
              end
            end;
          end;
        end;
      end;
    end;
end;

function TDCSGlobalSettings.CheckProgramParameters(AnApplication: TCustomApplication; AllErrors: Boolean = False): string;
var
  i: integer;
  Setting: TDCSSetting;
  ShortOpts: string;
  LongOpts: TStrings;
begin
  ShortOpts := '';
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

procedure TDCSGlobalSettings.AddSettingTemplate(ASectionPrefix, AKey, ASettingNamePrefix,
  ADefaultValue: string);
var
  Setting: TDCSSetting;
begin
  FMonitor.Acquire;
  try
    Setting := GetSettingTemplate(ASettingNamePrefix);
    if Assigned(Setting) then
      raise Exception.CreateFmt('Setting-template with settingnameprefix %s is already defined.',[ASettingNamePrefix]);
    Setting := TDCSSetting.Create(ASettingNamePrefix, ASectionPrefix, AKey, '', #0, dcsPHasParameter, ADefaultValue);
    FDefinedSettingTemplateList.Add(Setting);

    if not Assigned(FSettingTemplateList.FindSettingTemplate(ASectionPrefix)) then
      begin
      FSettingTemplateList.Add(TDCSSettingTemplate.Create(ASectionPrefix));
      end;
  finally
    FMonitor.Release;
  end;
end;

function TDCSGlobalSettings.GetDictionarySettingAsString(ASettingName, AName: string): string;
var
  Setting: TDCSSetting;
begin
  FMonitor.Acquire;
  try
    Setting := GetSetting(ASettingName);
    if not assigned(Setting) then
      raise Exception.CreateFmt('Parameter %s has not been defined.', [ASettingName]);
    if Setting.Parameter <> dcsPDictionary then
      raise Exception.CreateFmt('Parameter %s is not a dictionary parameter.', [ASettingName]);
    Result := Setting.GetNameValueAsString(AName);
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

function TDCSGlobalSettings.GetSettingAsBooleanByKey(ASection, AKey: string): Boolean;
var
  Setting: TDCSSetting;
begin
  FMonitor.Acquire;
  try
    Setting := GetSettingByKey(ASection, AKey);
    if not assigned(Setting) then
      raise Exception.CreateFmt('Parameter [%s]:[%s] has not been defined.', [ASection, AKey]);
    Result := Setting.GetAsBoolean
  finally
    FMonitor.Release;
  end;
end;

function TDCSGlobalSettings.GetSettingAsStringByKey(ASection, AKey: string): String;
var
  Setting: TDCSSetting;
begin
  FMonitor.Acquire;
  try
    Setting := GetSettingByKey(ASection, AKey);
    if not assigned(Setting) then
      raise Exception.CreateFmt('Parameter [%s]:[%s] has not been defined.', [ASection, AKey]);
    Result := Setting.GetAsString
  finally
    FMonitor.Release;
  end;
end;

procedure TDCSGlobalSettings.SetSettingAsBooleanByKey(ASection, AKey: string; AValue: Boolean);
var
  Setting: TDCSSetting;
begin
  FMonitor.Acquire;
  try
    Setting := GetSettingByKey(ASection, AKey);
    if not assigned(Setting) then
      raise Exception.CreateFmt('Parameter [%s]:[%s] has not been defined.', [ASection, AKey]);
    Setting.SetAsBoolean(AValue);
  finally
    FMonitor.Release;
  end;
end;

procedure TDCSGlobalSettings.SetSettingAsStringByKey(ASection, AKey: string; AValue: String);
var
  Setting: TDCSSetting;
begin
  FMonitor.Acquire;
  try
    Setting := GetSettingByKey(ASection, AKey);
    if not assigned(Setting) then
      raise Exception.CreateFmt('Parameter [%s]:[%s] has not been defined.', [ASection, AKey]);
    if Setting.Parameter = dcsPDictionary then
      Setting.SetNameValueAsString(AKey, AValue)
    else
      Setting.SetAsString(AValue);
  finally
    FMonitor.Release;
  end;
end;

procedure TDCSGlobalSettings.FillSectionList(SectionList: TStrings);
var
  i: Integer;
begin
  for i := 0 to FSettingList.Count -1 do
    begin
    if SectionList.IndexOf(FSettingList[i].Section) = -1 then
      SectionList.Add(FSettingList[i].Section);
    end;
end;

class function TDCSGlobalSettings.GetInstance: TDCSGlobalSettings;
begin
  if not Assigned(FInstance) then
    FInstance := TDCSGlobalSettings.Create;
  Result := FInstance;
end;

procedure TDCSGlobalSettings.LoadSettingsFromIniFile(AFilename: string);
var
  ConfigFileStream: TFileStream;
begin
  ConfigFileStream := TFileStream.Create(AFilename, fmOpenRead);
  try
    LoadSettingsFromIniStream(ConfigFileStream);
  finally
    ConfigFileStream.Free;
  end;
end;

end.

