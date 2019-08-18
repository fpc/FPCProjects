unit fprJSONRTTI;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  typinfo,
  fpjson,
  fpjsonrtti;

type
  TfprJSONPropertyOption = (jpoJSONContents);
  TfprJSONPropertyOptions = set of TfprJSONPropertyOption;

  IfprJSONPropertyAttributes = Interface(IUnknown) ['{B1EDD0DB-EA3B-435C-B04D-545FC0DC5727}']
    function GetPropertyAttributes(PropertyName: string; out ElementName: string; out PropertyOptions: TfprJSONPropertyOptions): Boolean;
  end;

  { TfprJSONDeStreamer }

  TfprJSONDeStreamer = class(TJSONDeStreamer)
  protected
    function DoMapProperty(AObject: TObject; PropInfo: PPropInfo; JSON: TJSONObject): TJSONData; override;
    procedure DoRestoreProperty(AObject: TObject; PropInfo: PPropInfo; PropData: TJSONData); override;
  end;

implementation

resourcestring
  SErrJSONContentsNotSupportedPropertyKind = 'JSONContents not supported on type [%s].';
  SErrJSONContentsNotSupportedOnAssignedValues = 'JSONContents not supported on types which are assigned.';
  SErrJSONContentsOnJSONDataOnly = 'JSONContents only work on TJSONData derivates.';

{ TfprJSONDeStreamer }

function TfprJSONDeStreamer.DoMapProperty(AObject: TObject; PropInfo: PPropInfo; JSON: TJSONObject): TJSONData;
var
  ElementName: string;
  PropOptions: TfprJSONPropertyOptions;
  CurrentObjectAttributes: IfprJSONPropertyAttributes;
  J: Integer;
begin
  if Supports(AObject, IfprJSONPropertyAttributes, CurrentObjectAttributes) and
     CurrentObjectAttributes.GetPropertyAttributes(PropInfo^.Name, ElementName, PropOptions) then
    begin
    J := JSON.IndexOfName(ElementName,(jdoCaseInsensitive in Options));
    if J > -1 then
      Result := JSON.Items[J]
    else
      Result := nil;
    end
  else
    Result := inherited DoMapProperty(AObject, PropInfo, JSON);
end;

procedure TfprJSONDeStreamer.DoRestoreProperty(AObject: TObject; PropInfo: PPropInfo; PropData: TJSONData);
var
  ElementName: string;
  PropOptions: TfprJSONPropertyOptions;
  CurrentObjectAttributes: IfprJSONPropertyAttributes;
  Obj: TObject;
  C: TClass;
begin
  if Supports(AObject, IfprJSONPropertyAttributes, CurrentObjectAttributes) and
     CurrentObjectAttributes.GetPropertyAttributes(PropInfo^.Name, ElementName, PropOptions) then
    begin
    if jpoJSONContents in PropOptions then
      begin
      case PropInfo^.PropType^.Kind of
        tkSString,
        tkLString,
        tkAString :
          SetStrProp(AObject, PropInfo, PropData.AsJSON);
        tkClass :
          begin
          Obj := GetObjectProp(AObject, PropInfo);
          if Assigned(Obj) then
            begin
            Error(SErrJSONContentsNotSupportedOnAssignedValues);
            end
          else
            begin
            C:=GetTypeData(Propinfo^.PropType)^.ClassType;
            If PropData.ClassType.InheritsFrom(C) then
              SetObjectProp(AObject, PropInfo, PropData.Clone)
            else
              Error(SErrJSONContentsOnJSONDataOnly);
            end;
          end
      else
        Error(SErrJSONContentsNotSupportedPropertyKind,[PropInfo^.Name]);
      end;
      Exit;
      end;
    end;
  if not IsWriteableProp(PropInfo) then
    Exit;
  Inherited DoRestoreProperty(AObject, PropInfo, PropData);
end;

end.

