{
This unit has been produced by ws_helper.
  Input unit name : "mantis".
  This unit name  : "mantis_proxy".
  Date            : "17-1-16 13:38:46".
}

Unit mantis_proxy;
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}
Interface

Uses SysUtils, Classes, TypInfo, base_service_intf, service_intf, mantis;

Type


  TMantisConnectPortType_Proxy=class(TBaseProxy,MantisConnectPortType)
  Protected
    class function GetServiceType() : PTypeInfo;override;
    function mc_version():string;
    function mc_login(
      const  username : string; 
      const  password : string
    ):UserData;
    function mc_enum_status(
      const  username : string; 
      const  password : string
    ):ObjectRefArray;
    function mc_enum_priorities(
      const  username : string; 
      const  password : string
    ):ObjectRefArray;
    function mc_enum_severities(
      const  username : string; 
      const  password : string
    ):ObjectRefArray;
    function mc_enum_reproducibilities(
      const  username : string; 
      const  password : string
    ):ObjectRefArray;
    function mc_enum_projections(
      const  username : string; 
      const  password : string
    ):ObjectRefArray;
    function mc_enum_etas(
      const  username : string; 
      const  password : string
    ):ObjectRefArray;
    function mc_enum_resolutions(
      const  username : string; 
      const  password : string
    ):ObjectRefArray;
    function mc_enum_access_levels(
      const  username : string; 
      const  password : string
    ):ObjectRefArray;
    function mc_enum_project_status(
      const  username : string; 
      const  password : string
    ):ObjectRefArray;
    function mc_enum_project_view_states(
      const  username : string; 
      const  password : string
    ):ObjectRefArray;
    function mc_enum_view_states(
      const  username : string; 
      const  password : string
    ):ObjectRefArray;
    function mc_enum_custom_field_types(
      const  username : string; 
      const  password : string
    ):ObjectRefArray;
    function mc_enum_get(
      const  username : string; 
      const  password : string; 
      const  enumeration : string
    ):string;
    function mc_issue_exists(
      const  username : string; 
      const  password : string; 
      const  issue_id : integer
    ):boolean;
    function mc_issue_get(
      const  username : string; 
      const  password : string; 
      const  issue_id : integer
    ):IssueData;
    function mc_issue_get_biggest_id(
      const  username : string; 
      const  password : string; 
      const  project_id : integer
    ):integer;
    function mc_issue_get_id_from_summary(
      const  username : string; 
      const  password : string; 
      const  summary : string
    ):integer;
    function mc_issue_add(
      const  username : string; 
      const  password : string; 
      const  issue : IssueData
    ):integer;
    function mc_issue_update(
      const  username : string; 
      const  password : string; 
      const  issueId : integer; 
      const  issue : IssueData
    ):boolean;
    function mc_issue_set_tags(
      const  username : string; 
      const  password : string; 
      const  issue_id : integer; 
      const  tags : TagDataArray
    ):boolean;
    function mc_issue_delete(
      const  username : string; 
      const  password : string; 
      const  issue_id : integer
    ):boolean;
    function mc_issue_note_add(
      const  username : string; 
      const  password : string; 
      const  issue_id : integer; 
      const  note : IssueNoteData
    ):integer;
    function mc_issue_note_delete(
      const  username : string; 
      const  password : string; 
      const  issue_note_id : integer
    ):boolean;
    function mc_issue_note_update(
      const  username : string; 
      const  password : string; 
      const  note : IssueNoteData
    ):boolean;
    function mc_issue_relationship_add(
      const  username : string; 
      const  password : string; 
      const  issue_id : integer; 
      const  relationship : RelationshipData
    ):integer;
    function mc_issue_relationship_delete(
      const  username : string; 
      const  password : string; 
      const  issue_id : integer; 
      const  relationship_id : integer
    ):boolean;
    function mc_issue_attachment_add(
      const  username : string; 
      const  password : string; 
      const  issue_id : integer; 
      const  name : string; 
      const  file_type : string; 
      const  content : TBase64StringRemotable
    ):integer;
    function mc_issue_attachment_delete(
      const  username : string; 
      const  password : string; 
      const  issue_attachment_id : integer
    ):boolean;
    function mc_issue_attachment_get(
      const  username : string; 
      const  password : string; 
      const  issue_attachment_id : integer
    ):TBase64StringRemotable;
    function mc_project_add(
      const  username : string; 
      const  password : string; 
      const  project : ProjectData
    ):integer;
    function mc_project_delete(
      const  username : string; 
      const  password : string; 
      const  project_id : integer
    ):boolean;
    function mc_project_update(
      const  username : string; 
      const  password : string; 
      const  project_id : integer; 
      const  project : ProjectData
    ):boolean;
    function mc_project_get_id_from_name(
      const  username : string; 
      const  password : string; 
      const  project_name : string
    ):integer;
    function mc_project_get_issues(
      const  username : string; 
      const  password : string; 
      const  project_id : integer; 
      const  page_number : integer; 
      const  per_page : integer
    ):IssueDataArray;
    function mc_project_get_issue_headers(
      const  username : string; 
      const  password : string; 
      const  project_id : integer; 
      const  page_number : integer; 
      const  per_page : integer
    ):IssueHeaderDataArray;
    function mc_project_get_users(
      const  username : string; 
      const  password : string; 
      const  project_id : integer; 
      const  access : integer
    ):AccountDataArray;
    function mc_projects_get_user_accessible(
      const  username : string; 
      const  password : string
    ):ProjectDataArray;
    function mc_project_get_categories(
      const  username : string; 
      const  password : string; 
      const  project_id : integer
    ):StringArray;
    function mc_project_add_category(
      const  username : string; 
      const  password : string; 
      const  project_id : integer; 
      const  p_category_name : string
    ):integer;
    function mc_project_delete_category(
      const  username : string; 
      const  password : string; 
      const  project_id : integer; 
      const  p_category_name : string
    ):integer;
    function mc_project_rename_category_by_name(
      const  username : string; 
      const  password : string; 
      const  project_id : integer; 
      const  p_category_name : string; 
      const  p_category_name_new : string; 
      const  p_assigned_to : integer
    ):integer;
    function mc_project_get_versions(
      const  username : string; 
      const  password : string; 
      const  project_id : integer
    ):ProjectVersionDataArray;
    function mc_project_version_add(
      const  username : string; 
      const  password : string; 
      const  version : ProjectVersionData
    ):integer;
    function mc_project_version_update(
      const  username : string; 
      const  password : string; 
      const  version_id : integer; 
      const  version : ProjectVersionData
    ):boolean;
    function mc_project_version_delete(
      const  username : string; 
      const  password : string; 
      const  version_id : integer
    ):boolean;
    function mc_project_get_released_versions(
      const  username : string; 
      const  password : string; 
      const  project_id : integer
    ):ProjectVersionDataArray;
    function mc_project_get_unreleased_versions(
      const  username : string; 
      const  password : string; 
      const  project_id : integer
    ):ProjectVersionDataArray;
    function mc_project_get_attachments(
      const  username : string; 
      const  password : string; 
      const  project_id : integer
    ):ProjectAttachmentDataArray;
    function mc_project_get_custom_fields(
      const  username : string; 
      const  password : string; 
      const  project_id : integer
    ):CustomFieldDefinitionDataArray;
    function mc_project_attachment_get(
      const  username : string; 
      const  password : string; 
      const  project_attachment_id : integer
    ):TBase64StringRemotable;
    function mc_project_attachment_add(
      const  username : string; 
      const  password : string; 
      const  project_id : integer; 
      const  name : string; 
      const  title : string; 
      const  description : string; 
      const  file_type : string; 
      const  content : TBase64StringRemotable
    ):integer;
    function mc_project_attachment_delete(
      const  username : string; 
      const  password : string; 
      const  project_attachment_id : integer
    ):boolean;
    function mc_project_get_all_subprojects(
      const  username : string; 
      const  password : string; 
      const  project_id : integer
    ):StringArray;
    function mc_filter_get(
      const  username : string; 
      const  password : string; 
      const  project_id : integer
    ):FilterDataArray;
    function mc_filter_get_issues(
      const  username : string; 
      const  password : string; 
      const  project_id : integer; 
      const  filter_id : integer; 
      const  page_number : integer; 
      const  per_page : integer
    ):IssueDataArray;
    function mc_filter_get_issue_headers(
      const  username : string; 
      const  password : string; 
      const  project_id : integer; 
      const  filter_id : integer; 
      const  page_number : integer; 
      const  per_page : integer
    ):IssueHeaderDataArray;
    function mc_config_get_string(
      const  username : string; 
      const  password : string; 
      const  config_var : string
    ):string;
    function mc_issue_checkin(
      const  username : string; 
      const  password : string; 
      const  issue_id : integer; 
      const  comment : string; 
      const  fixed : boolean
    ):boolean;
    function mc_user_pref_get_pref(
      const  username : string; 
      const  password : string; 
      const  project_id : integer; 
      const  pref_name : string
    ):string;
    function mc_user_profiles_get_all(
      const  username : string; 
      const  password : string; 
      const  page_number : integer; 
      const  per_page : integer
    ):ProfileDataSearchResult;
    function mc_tag_get_all(
      const  username : string; 
      const  password : string; 
      const  page_number : integer; 
      const  per_page : integer
    ):TagDataSearchResult;
    function mc_tag_add(
      const  username : string; 
      const  password : string; 
      const  tag : TagData
    ):integer;
    function mc_tag_delete(
      const  username : string; 
      const  password : string; 
      const  tag_id : integer
    ):boolean;
  End;

  Function wst_CreateInstance_MantisConnectPortType(const AFormat : string = 'SOAP:'; const ATransport : string = 'HTTP:'; const AAddress : string = ''):MantisConnectPortType;

Implementation
uses wst_resources_imp, metadata_repository;


Function wst_CreateInstance_MantisConnectPortType(const AFormat : string; const ATransport : string; const AAddress : string):MantisConnectPortType;
Var
  locAdr : string;
Begin
  locAdr := AAddress;
  if ( locAdr = '' ) then
    locAdr := GetServiceDefaultAddress(TypeInfo(MantisConnectPortType));
  Result := TMantisConnectPortType_Proxy.Create('MantisConnectPortType',AFormat+GetServiceDefaultFormatProperties(TypeInfo(MantisConnectPortType)),ATransport + 'address=' + locAdr);
End;

{ TMantisConnectPortType_Proxy implementation }

class function TMantisConnectPortType_Proxy.GetServiceType() : PTypeInfo;
begin
  result := TypeInfo(MantisConnectPortType);
end;

function TMantisConnectPortType_Proxy.mc_version():string;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_version', GetTarget(),locCallContext);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(string), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_login(
  const  username : string; 
  const  password : string
):UserData;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_login', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(UserData), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_enum_status(
  const  username : string; 
  const  password : string
):ObjectRefArray;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_enum_status', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(ObjectRefArray), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_enum_priorities(
  const  username : string; 
  const  password : string
):ObjectRefArray;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_enum_priorities', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(ObjectRefArray), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_enum_severities(
  const  username : string; 
  const  password : string
):ObjectRefArray;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_enum_severities', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(ObjectRefArray), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_enum_reproducibilities(
  const  username : string; 
  const  password : string
):ObjectRefArray;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_enum_reproducibilities', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(ObjectRefArray), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_enum_projections(
  const  username : string; 
  const  password : string
):ObjectRefArray;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_enum_projections', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(ObjectRefArray), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_enum_etas(
  const  username : string; 
  const  password : string
):ObjectRefArray;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_enum_etas', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(ObjectRefArray), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_enum_resolutions(
  const  username : string; 
  const  password : string
):ObjectRefArray;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_enum_resolutions', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(ObjectRefArray), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_enum_access_levels(
  const  username : string; 
  const  password : string
):ObjectRefArray;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_enum_access_levels', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(ObjectRefArray), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_enum_project_status(
  const  username : string; 
  const  password : string
):ObjectRefArray;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_enum_project_status', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(ObjectRefArray), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_enum_project_view_states(
  const  username : string; 
  const  password : string
):ObjectRefArray;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_enum_project_view_states', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(ObjectRefArray), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_enum_view_states(
  const  username : string; 
  const  password : string
):ObjectRefArray;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_enum_view_states', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(ObjectRefArray), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_enum_custom_field_types(
  const  username : string; 
  const  password : string
):ObjectRefArray;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_enum_custom_field_types', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(ObjectRefArray), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_enum_get(
  const  username : string; 
  const  password : string; 
  const  enumeration : string
):string;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_enum_get', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('enumeration', TypeInfo(string), enumeration);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(string), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_issue_exists(
  const  username : string; 
  const  password : string; 
  const  issue_id : integer
):boolean;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_issue_exists', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('issue_id', TypeInfo(integer), issue_id);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(boolean), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_issue_get(
  const  username : string; 
  const  password : string; 
  const  issue_id : integer
):IssueData;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_issue_get', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('issue_id', TypeInfo(integer), issue_id);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(IssueData), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_issue_get_biggest_id(
  const  username : string; 
  const  password : string; 
  const  project_id : integer
):integer;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_issue_get_biggest_id', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('project_id', TypeInfo(integer), project_id);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(integer), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_issue_get_id_from_summary(
  const  username : string; 
  const  password : string; 
  const  summary : string
):integer;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_issue_get_id_from_summary', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('summary', TypeInfo(string), summary);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(integer), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_issue_add(
  const  username : string; 
  const  password : string; 
  const  issue : IssueData
):integer;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_issue_add', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('issue', TypeInfo(IssueData), issue);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(integer), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_issue_update(
  const  username : string; 
  const  password : string; 
  const  issueId : integer; 
  const  issue : IssueData
):boolean;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_issue_update', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('issueId', TypeInfo(integer), issueId);
      locSerializer.Put('issue', TypeInfo(IssueData), issue);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(boolean), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_issue_set_tags(
  const  username : string; 
  const  password : string; 
  const  issue_id : integer; 
  const  tags : TagDataArray
):boolean;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_issue_set_tags', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('issue_id', TypeInfo(integer), issue_id);
      locSerializer.Put('tags', TypeInfo(TagDataArray), tags);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(boolean), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_issue_delete(
  const  username : string; 
  const  password : string; 
  const  issue_id : integer
):boolean;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_issue_delete', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('issue_id', TypeInfo(integer), issue_id);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(boolean), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_issue_note_add(
  const  username : string; 
  const  password : string; 
  const  issue_id : integer; 
  const  note : IssueNoteData
):integer;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_issue_note_add', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('issue_id', TypeInfo(integer), issue_id);
      locSerializer.Put('note', TypeInfo(IssueNoteData), note);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(integer), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_issue_note_delete(
  const  username : string; 
  const  password : string; 
  const  issue_note_id : integer
):boolean;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_issue_note_delete', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('issue_note_id', TypeInfo(integer), issue_note_id);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(boolean), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_issue_note_update(
  const  username : string; 
  const  password : string; 
  const  note : IssueNoteData
):boolean;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_issue_note_update', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('note', TypeInfo(IssueNoteData), note);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(boolean), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_issue_relationship_add(
  const  username : string; 
  const  password : string; 
  const  issue_id : integer; 
  const  relationship : RelationshipData
):integer;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_issue_relationship_add', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('issue_id', TypeInfo(integer), issue_id);
      locSerializer.Put('relationship', TypeInfo(RelationshipData), relationship);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(integer), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_issue_relationship_delete(
  const  username : string; 
  const  password : string; 
  const  issue_id : integer; 
  const  relationship_id : integer
):boolean;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_issue_relationship_delete', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('issue_id', TypeInfo(integer), issue_id);
      locSerializer.Put('relationship_id', TypeInfo(integer), relationship_id);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(boolean), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_issue_attachment_add(
  const  username : string; 
  const  password : string; 
  const  issue_id : integer; 
  const  name : string; 
  const  file_type : string; 
  const  content : TBase64StringRemotable
):integer;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_issue_attachment_add', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('issue_id', TypeInfo(integer), issue_id);
      locSerializer.Put('name', TypeInfo(string), name);
      locSerializer.Put('file_type', TypeInfo(string), file_type);
      locSerializer.Put('content', TypeInfo(TBase64StringRemotable), content);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(integer), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_issue_attachment_delete(
  const  username : string; 
  const  password : string; 
  const  issue_attachment_id : integer
):boolean;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_issue_attachment_delete', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('issue_attachment_id', TypeInfo(integer), issue_attachment_id);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(boolean), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_issue_attachment_get(
  const  username : string; 
  const  password : string; 
  const  issue_attachment_id : integer
):TBase64StringRemotable;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_issue_attachment_get', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('issue_attachment_id', TypeInfo(integer), issue_attachment_id);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(TBase64StringRemotable), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_project_add(
  const  username : string; 
  const  password : string; 
  const  project : ProjectData
):integer;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_project_add', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('project', TypeInfo(ProjectData), project);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(integer), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_project_delete(
  const  username : string; 
  const  password : string; 
  const  project_id : integer
):boolean;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_project_delete', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('project_id', TypeInfo(integer), project_id);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(boolean), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_project_update(
  const  username : string; 
  const  password : string; 
  const  project_id : integer; 
  const  project : ProjectData
):boolean;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_project_update', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('project_id', TypeInfo(integer), project_id);
      locSerializer.Put('project', TypeInfo(ProjectData), project);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(boolean), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_project_get_id_from_name(
  const  username : string; 
  const  password : string; 
  const  project_name : string
):integer;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_project_get_id_from_name', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('project_name', TypeInfo(string), project_name);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(integer), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_project_get_issues(
  const  username : string; 
  const  password : string; 
  const  project_id : integer; 
  const  page_number : integer; 
  const  per_page : integer
):IssueDataArray;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_project_get_issues', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('project_id', TypeInfo(integer), project_id);
      locSerializer.Put('page_number', TypeInfo(integer), page_number);
      locSerializer.Put('per_page', TypeInfo(integer), per_page);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(IssueDataArray), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_project_get_issue_headers(
  const  username : string; 
  const  password : string; 
  const  project_id : integer; 
  const  page_number : integer; 
  const  per_page : integer
):IssueHeaderDataArray;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_project_get_issue_headers', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('project_id', TypeInfo(integer), project_id);
      locSerializer.Put('page_number', TypeInfo(integer), page_number);
      locSerializer.Put('per_page', TypeInfo(integer), per_page);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(IssueHeaderDataArray), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_project_get_users(
  const  username : string; 
  const  password : string; 
  const  project_id : integer; 
  const  access : integer
):AccountDataArray;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_project_get_users', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('project_id', TypeInfo(integer), project_id);
      locSerializer.Put('access', TypeInfo(integer), access);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(AccountDataArray), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_projects_get_user_accessible(
  const  username : string; 
  const  password : string
):ProjectDataArray;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_projects_get_user_accessible', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(ProjectDataArray), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_project_get_categories(
  const  username : string; 
  const  password : string; 
  const  project_id : integer
):StringArray;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_project_get_categories', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('project_id', TypeInfo(integer), project_id);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(StringArray), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_project_add_category(
  const  username : string; 
  const  password : string; 
  const  project_id : integer; 
  const  p_category_name : string
):integer;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_project_add_category', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('project_id', TypeInfo(integer), project_id);
      locSerializer.Put('p_category_name', TypeInfo(string), p_category_name);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(integer), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_project_delete_category(
  const  username : string; 
  const  password : string; 
  const  project_id : integer; 
  const  p_category_name : string
):integer;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_project_delete_category', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('project_id', TypeInfo(integer), project_id);
      locSerializer.Put('p_category_name', TypeInfo(string), p_category_name);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(integer), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_project_rename_category_by_name(
  const  username : string; 
  const  password : string; 
  const  project_id : integer; 
  const  p_category_name : string; 
  const  p_category_name_new : string; 
  const  p_assigned_to : integer
):integer;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_project_rename_category_by_name', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('project_id', TypeInfo(integer), project_id);
      locSerializer.Put('p_category_name', TypeInfo(string), p_category_name);
      locSerializer.Put('p_category_name_new', TypeInfo(string), p_category_name_new);
      locSerializer.Put('p_assigned_to', TypeInfo(integer), p_assigned_to);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(integer), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_project_get_versions(
  const  username : string; 
  const  password : string; 
  const  project_id : integer
):ProjectVersionDataArray;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_project_get_versions', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('project_id', TypeInfo(integer), project_id);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(ProjectVersionDataArray), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_project_version_add(
  const  username : string; 
  const  password : string; 
  const  version : ProjectVersionData
):integer;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_project_version_add', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('version', TypeInfo(ProjectVersionData), version);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(integer), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_project_version_update(
  const  username : string; 
  const  password : string; 
  const  version_id : integer; 
  const  version : ProjectVersionData
):boolean;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_project_version_update', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('version_id', TypeInfo(integer), version_id);
      locSerializer.Put('version', TypeInfo(ProjectVersionData), version);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(boolean), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_project_version_delete(
  const  username : string; 
  const  password : string; 
  const  version_id : integer
):boolean;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_project_version_delete', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('version_id', TypeInfo(integer), version_id);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(boolean), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_project_get_released_versions(
  const  username : string; 
  const  password : string; 
  const  project_id : integer
):ProjectVersionDataArray;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_project_get_released_versions', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('project_id', TypeInfo(integer), project_id);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(ProjectVersionDataArray), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_project_get_unreleased_versions(
  const  username : string; 
  const  password : string; 
  const  project_id : integer
):ProjectVersionDataArray;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_project_get_unreleased_versions', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('project_id', TypeInfo(integer), project_id);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(ProjectVersionDataArray), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_project_get_attachments(
  const  username : string; 
  const  password : string; 
  const  project_id : integer
):ProjectAttachmentDataArray;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_project_get_attachments', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('project_id', TypeInfo(integer), project_id);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(ProjectAttachmentDataArray), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_project_get_custom_fields(
  const  username : string; 
  const  password : string; 
  const  project_id : integer
):CustomFieldDefinitionDataArray;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_project_get_custom_fields', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('project_id', TypeInfo(integer), project_id);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(CustomFieldDefinitionDataArray), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_project_attachment_get(
  const  username : string; 
  const  password : string; 
  const  project_attachment_id : integer
):TBase64StringRemotable;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_project_attachment_get', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('project_attachment_id', TypeInfo(integer), project_attachment_id);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(TBase64StringRemotable), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_project_attachment_add(
  const  username : string; 
  const  password : string; 
  const  project_id : integer; 
  const  name : string; 
  const  title : string; 
  const  description : string; 
  const  file_type : string; 
  const  content : TBase64StringRemotable
):integer;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_project_attachment_add', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('project_id', TypeInfo(integer), project_id);
      locSerializer.Put('name', TypeInfo(string), name);
      locSerializer.Put('title', TypeInfo(string), title);
      locSerializer.Put('description', TypeInfo(string), description);
      locSerializer.Put('file_type', TypeInfo(string), file_type);
      locSerializer.Put('content', TypeInfo(TBase64StringRemotable), content);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(integer), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_project_attachment_delete(
  const  username : string; 
  const  password : string; 
  const  project_attachment_id : integer
):boolean;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_project_attachment_delete', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('project_attachment_id', TypeInfo(integer), project_attachment_id);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(boolean), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_project_get_all_subprojects(
  const  username : string; 
  const  password : string; 
  const  project_id : integer
):StringArray;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_project_get_all_subprojects', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('project_id', TypeInfo(integer), project_id);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(StringArray), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_filter_get(
  const  username : string; 
  const  password : string; 
  const  project_id : integer
):FilterDataArray;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_filter_get', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('project_id', TypeInfo(integer), project_id);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(FilterDataArray), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_filter_get_issues(
  const  username : string; 
  const  password : string; 
  const  project_id : integer; 
  const  filter_id : integer; 
  const  page_number : integer; 
  const  per_page : integer
):IssueDataArray;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_filter_get_issues', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('project_id', TypeInfo(integer), project_id);
      locSerializer.Put('filter_id', TypeInfo(integer), filter_id);
      locSerializer.Put('page_number', TypeInfo(integer), page_number);
      locSerializer.Put('per_page', TypeInfo(integer), per_page);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(IssueDataArray), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_filter_get_issue_headers(
  const  username : string; 
  const  password : string; 
  const  project_id : integer; 
  const  filter_id : integer; 
  const  page_number : integer; 
  const  per_page : integer
):IssueHeaderDataArray;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_filter_get_issue_headers', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('project_id', TypeInfo(integer), project_id);
      locSerializer.Put('filter_id', TypeInfo(integer), filter_id);
      locSerializer.Put('page_number', TypeInfo(integer), page_number);
      locSerializer.Put('per_page', TypeInfo(integer), per_page);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(IssueHeaderDataArray), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_config_get_string(
  const  username : string; 
  const  password : string; 
  const  config_var : string
):string;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_config_get_string', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('config_var', TypeInfo(string), config_var);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(string), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_issue_checkin(
  const  username : string; 
  const  password : string; 
  const  issue_id : integer; 
  const  comment : string; 
  const  fixed : boolean
):boolean;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_issue_checkin', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('issue_id', TypeInfo(integer), issue_id);
      locSerializer.Put('comment', TypeInfo(string), comment);
      locSerializer.Put('fixed', TypeInfo(boolean), fixed);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(boolean), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_user_pref_get_pref(
  const  username : string; 
  const  password : string; 
  const  project_id : integer; 
  const  pref_name : string
):string;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_user_pref_get_pref', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('project_id', TypeInfo(integer), project_id);
      locSerializer.Put('pref_name', TypeInfo(string), pref_name);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(string), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_user_profiles_get_all(
  const  username : string; 
  const  password : string; 
  const  page_number : integer; 
  const  per_page : integer
):ProfileDataSearchResult;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_user_profiles_get_all', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('page_number', TypeInfo(integer), page_number);
      locSerializer.Put('per_page', TypeInfo(integer), per_page);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(ProfileDataSearchResult), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_tag_get_all(
  const  username : string; 
  const  password : string; 
  const  page_number : integer; 
  const  per_page : integer
):TagDataSearchResult;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_tag_get_all', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('page_number', TypeInfo(integer), page_number);
      locSerializer.Put('per_page', TypeInfo(integer), per_page);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      TObject(Result) := Nil;
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(TagDataSearchResult), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_tag_add(
  const  username : string; 
  const  password : string; 
  const  tag : TagData
):integer;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_tag_add', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('tag', TypeInfo(TagData), tag);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(integer), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TMantisConnectPortType_Proxy.mc_tag_delete(
  const  username : string; 
  const  password : string; 
  const  tag_id : integer
):boolean;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('mc_tag_delete', GetTarget(),locCallContext);
      locSerializer.Put('username', TypeInfo(string), username);
      locSerializer.Put('password', TypeInfo(string), password);
      locSerializer.Put('tag_id', TypeInfo(integer), tag_id);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'return';
      locSerializer.Get(TypeInfo(boolean), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;


initialization
  {$i mantis.wst}

  {$IF DECLARED(Register_mantis_ServiceMetadata)}
  Register_mantis_ServiceMetadata();
  {$IFEND}
End.
