{
This unit has been produced by ws_helper.
  Input unit name : "mantis".
  This unit name  : "mantis".
  Date            : "17-1-16 13:38:46".
}
unit mantis;
{$IFDEF FPC}
  {$mode objfpc} {$H+}
{$ENDIF}
{$IFNDEF FPC}
  {$DEFINE WST_RECORD_RTTI}
{$ENDIF}
interface

uses SysUtils, Classes, TypInfo, base_service_intf, service_intf;

const
  sNAME_SPACE = 'http://futureware.biz/mantisconnect';
  sUNIT_NAME = 'mantis';

type

  UserData = class;
  ObjectRefArray = class;
  IssueData = class;
  TagDataArray = class;
  IssueNoteData = class;
  RelationshipData = class;
  ProjectData = class;
  IssueDataArray = class;
  IssueHeaderDataArray = class;
  AccountDataArray = class;
  ProjectDataArray = class;
  StringArray = class;
  ProjectVersionDataArray = class;
  ProjectVersionData = class;
  ProjectAttachmentDataArray = class;
  CustomFieldDefinitionDataArray = class;
  FilterDataArray = class;
  ProfileDataSearchResult = class;
  TagDataSearchResult = class;
  TagData = class;
  ObjectRef = class;
  AccountData = class;
  AttachmentData = class;
  AttachmentDataArray = class;
  ProjectAttachmentData = class;
  RelationshipDataArray = class;
  IssueNoteDataArray = class;
  IssueHeaderData = class;
  FilterData = class;
  CustomFieldDefinitionData = class;
  CustomFieldLinkForProjectData = class;
  CustomFieldLinkForProjectDataArray = class;
  CustomFieldValueForIssueData = class;
  CustomFieldValueForIssueDataArray = class;
  ProfileData = class;
  ProfileDataArray = class;

  UserData = class(TBaseComplexRemotable)
  private
    Faccount_data : AccountData;
    Faccess_level : integer;
    Ftimezone : string;
  private
    function wstHas_account_data() : Boolean;
    function wstHas_access_level() : Boolean;
    function wstHas_timezone() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property account_data : AccountData read Faccount_data write Faccount_data stored wstHas_account_data;
    property access_level : integer read Faccess_level write Faccess_level stored wstHas_access_level;
    property timezone : string read Ftimezone write Ftimezone stored wstHas_timezone;
  end;

  IssueData = class(TBaseComplexRemotable)
  private
    Fid : integer;
    Fview_state : ObjectRef;
    Flast_updated : TDateTimeRemotable;
    Fproject : ObjectRef;
    Fcategory : string;
    Fpriority : ObjectRef;
    Fseverity : ObjectRef;
    Fstatus : ObjectRef;
    Freporter : AccountData;
    Fsummary : string;
    Fversion : string;
    Fbuild : string;
    Fplatform : string;
    Fos : string;
    Fos_build : string;
    Freproducibility : ObjectRef;
    Fdate_submitted : TDateTimeRemotable;
    Fsponsorship_total : integer;
    Fhandler : AccountData;
    Fprojection : ObjectRef;
    Feta : ObjectRef;
    Fresolution : ObjectRef;
    Ffixed_in_version : string;
    Ftarget_version : string;
    Fdescription : string;
    Fsteps_to_reproduce : string;
    Fadditional_information : string;
    Fattachments : AttachmentDataArray;
    Frelationships : RelationshipDataArray;
    Fnotes : IssueNoteDataArray;
    Fcustom_fields : CustomFieldValueForIssueDataArray;
    Fdue_date : TDateTimeRemotable;
    Fmonitors : AccountDataArray;
    Fsticky : boolean;
    Ftags : ObjectRefArray;
  private
    function wstHas_id() : Boolean;
    function wstHas_view_state() : Boolean;
    function wstHas_last_updated() : Boolean;
    function wstHas_project() : Boolean;
    function wstHas_category() : Boolean;
    function wstHas_priority() : Boolean;
    function wstHas_severity() : Boolean;
    function wstHas_status() : Boolean;
    function wstHas_reporter() : Boolean;
    function wstHas_summary() : Boolean;
    function wstHas_version() : Boolean;
    function wstHas_build() : Boolean;
    function wstHas_platform() : Boolean;
    function wstHas_os() : Boolean;
    function wstHas_os_build() : Boolean;
    function wstHas_reproducibility() : Boolean;
    function wstHas_date_submitted() : Boolean;
    function wstHas_sponsorship_total() : Boolean;
    function wstHas_handler() : Boolean;
    function wstHas_projection() : Boolean;
    function wstHas_eta() : Boolean;
    function wstHas_resolution() : Boolean;
    function wstHas_fixed_in_version() : Boolean;
    function wstHas_target_version() : Boolean;
    function wstHas_description() : Boolean;
    function wstHas_steps_to_reproduce() : Boolean;
    function wstHas_additional_information() : Boolean;
    function wstHas_attachments() : Boolean;
    function wstHas_relationships() : Boolean;
    function wstHas_notes() : Boolean;
    function wstHas_custom_fields() : Boolean;
    function wstHas_due_date() : Boolean;
    function wstHas_monitors() : Boolean;
    function wstHas_sticky() : Boolean;
    function wstHas_tags() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property id : integer read Fid write Fid stored wstHas_id;
    property view_state : ObjectRef read Fview_state write Fview_state stored wstHas_view_state;
    property last_updated : TDateTimeRemotable read Flast_updated write Flast_updated stored wstHas_last_updated;
    property project : ObjectRef read Fproject write Fproject stored wstHas_project;
    property category : string read Fcategory write Fcategory stored wstHas_category;
    property priority : ObjectRef read Fpriority write Fpriority stored wstHas_priority;
    property severity : ObjectRef read Fseverity write Fseverity stored wstHas_severity;
    property status : ObjectRef read Fstatus write Fstatus stored wstHas_status;
    property reporter : AccountData read Freporter write Freporter stored wstHas_reporter;
    property summary : string read Fsummary write Fsummary stored wstHas_summary;
    property version : string read Fversion write Fversion stored wstHas_version;
    property build : string read Fbuild write Fbuild stored wstHas_build;
    property platform : string read Fplatform write Fplatform stored wstHas_platform;
    property os : string read Fos write Fos stored wstHas_os;
    property os_build : string read Fos_build write Fos_build stored wstHas_os_build;
    property reproducibility : ObjectRef read Freproducibility write Freproducibility stored wstHas_reproducibility;
    property date_submitted : TDateTimeRemotable read Fdate_submitted write Fdate_submitted stored wstHas_date_submitted;
    property sponsorship_total : integer read Fsponsorship_total write Fsponsorship_total stored wstHas_sponsorship_total;
    property handler : AccountData read Fhandler write Fhandler stored wstHas_handler;
    property projection : ObjectRef read Fprojection write Fprojection stored wstHas_projection;
    property eta : ObjectRef read Feta write Feta stored wstHas_eta;
    property resolution : ObjectRef read Fresolution write Fresolution stored wstHas_resolution;
    property fixed_in_version : string read Ffixed_in_version write Ffixed_in_version stored wstHas_fixed_in_version;
    property target_version : string read Ftarget_version write Ftarget_version stored wstHas_target_version;
    property description : string read Fdescription write Fdescription stored wstHas_description;
    property steps_to_reproduce : string read Fsteps_to_reproduce write Fsteps_to_reproduce stored wstHas_steps_to_reproduce;
    property additional_information : string read Fadditional_information write Fadditional_information stored wstHas_additional_information;
    property attachments : AttachmentDataArray read Fattachments write Fattachments stored wstHas_attachments;
    property relationships : RelationshipDataArray read Frelationships write Frelationships stored wstHas_relationships;
    property notes : IssueNoteDataArray read Fnotes write Fnotes stored wstHas_notes;
    property custom_fields : CustomFieldValueForIssueDataArray read Fcustom_fields write Fcustom_fields stored wstHas_custom_fields;
    property due_date : TDateTimeRemotable read Fdue_date write Fdue_date stored wstHas_due_date;
    property monitors : AccountDataArray read Fmonitors write Fmonitors stored wstHas_monitors;
    property sticky : boolean read Fsticky write Fsticky stored wstHas_sticky;
    property tags : ObjectRefArray read Ftags write Ftags stored wstHas_tags;
  end;

  IssueNoteData = class(TBaseComplexRemotable)
  private
    Fid : integer;
    Freporter : AccountData;
    Ftext : string;
    Fview_state : ObjectRef;
    Fdate_submitted : TDateTimeRemotable;
    Flast_modified : TDateTimeRemotable;
    Ftime_tracking : integer;
    Fnote_type : integer;
    Fnote_attr : string;
  private
    function wstHas_id() : Boolean;
    function wstHas_reporter() : Boolean;
    function wstHas_text() : Boolean;
    function wstHas_view_state() : Boolean;
    function wstHas_date_submitted() : Boolean;
    function wstHas_last_modified() : Boolean;
    function wstHas_time_tracking() : Boolean;
    function wstHas_note_type() : Boolean;
    function wstHas_note_attr() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property id : integer read Fid write Fid stored wstHas_id;
    property reporter : AccountData read Freporter write Freporter stored wstHas_reporter;
    property text : string read Ftext write Ftext stored wstHas_text;
    property view_state : ObjectRef read Fview_state write Fview_state stored wstHas_view_state;
    property date_submitted : TDateTimeRemotable read Fdate_submitted write Fdate_submitted stored wstHas_date_submitted;
    property last_modified : TDateTimeRemotable read Flast_modified write Flast_modified stored wstHas_last_modified;
    property time_tracking : integer read Ftime_tracking write Ftime_tracking stored wstHas_time_tracking;
    property note_type : integer read Fnote_type write Fnote_type stored wstHas_note_type;
    property note_attr : string read Fnote_attr write Fnote_attr stored wstHas_note_attr;
  end;

  RelationshipData = class(TBaseComplexRemotable)
  private
    Fid : integer;
    F_type : ObjectRef;
    Ftarget_id : integer;
  private
    function wstHas_id() : Boolean;
    function wstHas__type() : Boolean;
    function wstHas_target_id() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property id : integer read Fid write Fid stored wstHas_id;
    property _type : ObjectRef read F_type write F_type stored wstHas__type;
    property target_id : integer read Ftarget_id write Ftarget_id stored wstHas_target_id;
  end;

  ProjectData = class(TBaseComplexRemotable)
  private
    Fid : integer;
    Fname : string;
    Fstatus : ObjectRef;
    Fenabled : boolean;
    Fview_state : ObjectRef;
    Faccess_min : ObjectRef;
    Ffile_path : string;
    Fdescription : string;
    Fsubprojects : ProjectDataArray;
    Finherit_global : boolean;
  private
    function wstHas_id() : Boolean;
    function wstHas_name() : Boolean;
    function wstHas_status() : Boolean;
    function wstHas_enabled() : Boolean;
    function wstHas_view_state() : Boolean;
    function wstHas_access_min() : Boolean;
    function wstHas_file_path() : Boolean;
    function wstHas_description() : Boolean;
    function wstHas_subprojects() : Boolean;
    function wstHas_inherit_global() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property id : integer read Fid write Fid stored wstHas_id;
    property name : string read Fname write Fname stored wstHas_name;
    property status : ObjectRef read Fstatus write Fstatus stored wstHas_status;
    property enabled : boolean read Fenabled write Fenabled stored wstHas_enabled;
    property view_state : ObjectRef read Fview_state write Fview_state stored wstHas_view_state;
    property access_min : ObjectRef read Faccess_min write Faccess_min stored wstHas_access_min;
    property file_path : string read Ffile_path write Ffile_path stored wstHas_file_path;
    property description : string read Fdescription write Fdescription stored wstHas_description;
    property subprojects : ProjectDataArray read Fsubprojects write Fsubprojects stored wstHas_subprojects;
    property inherit_global : boolean read Finherit_global write Finherit_global stored wstHas_inherit_global;
  end;

  ProjectVersionData = class(TBaseComplexRemotable)
  private
    Fid : integer;
    Fname : string;
    Fproject_id : integer;
    Fdate_order : TDateTimeRemotable;
    Fdescription : string;
    Freleased : boolean;
    Fobsolete : boolean;
  private
    function wstHas_id() : Boolean;
    function wstHas_name() : Boolean;
    function wstHas_project_id() : Boolean;
    function wstHas_date_order() : Boolean;
    function wstHas_description() : Boolean;
    function wstHas_released() : Boolean;
    function wstHas_obsolete() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property id : integer read Fid write Fid stored wstHas_id;
    property name : string read Fname write Fname stored wstHas_name;
    property project_id : integer read Fproject_id write Fproject_id stored wstHas_project_id;
    property date_order : TDateTimeRemotable read Fdate_order write Fdate_order stored wstHas_date_order;
    property description : string read Fdescription write Fdescription stored wstHas_description;
    property released : boolean read Freleased write Freleased stored wstHas_released;
    property obsolete : boolean read Fobsolete write Fobsolete stored wstHas_obsolete;
  end;

  ProfileDataSearchResult = class(TBaseComplexRemotable)
  private
    Fresults : ProfileDataArray;
    Ftotal_results : integer;
  private
    function wstHas_results() : Boolean;
    function wstHas_total_results() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property results : ProfileDataArray read Fresults write Fresults stored wstHas_results;
    property total_results : integer read Ftotal_results write Ftotal_results stored wstHas_total_results;
  end;

  TagDataSearchResult = class(TBaseComplexRemotable)
  private
    Fresults : TagDataArray;
    Ftotal_results : integer;
  private
    function wstHas_results() : Boolean;
    function wstHas_total_results() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property results : TagDataArray read Fresults write Fresults stored wstHas_results;
    property total_results : integer read Ftotal_results write Ftotal_results stored wstHas_total_results;
  end;

  TagData = class(TBaseComplexRemotable)
  private
    Fid : integer;
    Fuser_id : AccountData;
    Fname : string;
    Fdescription : string;
    Fdate_created : TDateTimeRemotable;
    Fdate_updated : TDateTimeRemotable;
  private
    function wstHas_id() : Boolean;
    function wstHas_user_id() : Boolean;
    function wstHas_name() : Boolean;
    function wstHas_description() : Boolean;
    function wstHas_date_created() : Boolean;
    function wstHas_date_updated() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property id : integer read Fid write Fid stored wstHas_id;
    property user_id : AccountData read Fuser_id write Fuser_id stored wstHas_user_id;
    property name : string read Fname write Fname stored wstHas_name;
    property description : string read Fdescription write Fdescription stored wstHas_description;
    property date_created : TDateTimeRemotable read Fdate_created write Fdate_created stored wstHas_date_created;
    property date_updated : TDateTimeRemotable read Fdate_updated write Fdate_updated stored wstHas_date_updated;
  end;

  ObjectRef = class(TBaseComplexRemotable)
  private
    Fid : integer;
    Fname : string;
  private
    function wstHas_id() : Boolean;
    function wstHas_name() : Boolean;
  published
    property id : integer read Fid write Fid stored wstHas_id;
    property name : string read Fname write Fname stored wstHas_name;
  end;

  AccountData = class(TBaseComplexRemotable)
  private
    Fid : integer;
    Fname : string;
    Freal_name : string;
    Femail : string;
  private
    function wstHas_id() : Boolean;
    function wstHas_name() : Boolean;
    function wstHas_real_name() : Boolean;
    function wstHas_email() : Boolean;
  published
    property id : integer read Fid write Fid stored wstHas_id;
    property name : string read Fname write Fname stored wstHas_name;
    property real_name : string read Freal_name write Freal_name stored wstHas_real_name;
    property email : string read Femail write Femail stored wstHas_email;
  end;

  AttachmentData = class(TBaseComplexRemotable)
  private
    Fid : integer;
    Ffilename : string;
    Fsize : integer;
    Fcontent_type : string;
    Fdate_submitted : TDateTimeRemotable;
    Fdownload_url : anyURI;
    Fuser_id : integer;
  private
    function wstHas_id() : Boolean;
    function wstHas_filename() : Boolean;
    function wstHas_size() : Boolean;
    function wstHas_content_type() : Boolean;
    function wstHas_date_submitted() : Boolean;
    function wstHas_download_url() : Boolean;
    function wstHas_user_id() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property id : integer read Fid write Fid stored wstHas_id;
    property filename : string read Ffilename write Ffilename stored wstHas_filename;
    property size : integer read Fsize write Fsize stored wstHas_size;
    property content_type : string read Fcontent_type write Fcontent_type stored wstHas_content_type;
    property date_submitted : TDateTimeRemotable read Fdate_submitted write Fdate_submitted stored wstHas_date_submitted;
    property download_url : anyURI read Fdownload_url write Fdownload_url stored wstHas_download_url;
    property user_id : integer read Fuser_id write Fuser_id stored wstHas_user_id;
  end;

  ProjectAttachmentData = class(TBaseComplexRemotable)
  private
    Fid : integer;
    Ffilename : string;
    Ftitle : string;
    Fdescription : string;
    Fsize : integer;
    Fcontent_type : string;
    Fdate_submitted : TDateTimeRemotable;
    Fdownload_url : anyURI;
    Fuser_id : integer;
  private
    function wstHas_id() : Boolean;
    function wstHas_filename() : Boolean;
    function wstHas_title() : Boolean;
    function wstHas_description() : Boolean;
    function wstHas_size() : Boolean;
    function wstHas_content_type() : Boolean;
    function wstHas_date_submitted() : Boolean;
    function wstHas_download_url() : Boolean;
    function wstHas_user_id() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property id : integer read Fid write Fid stored wstHas_id;
    property filename : string read Ffilename write Ffilename stored wstHas_filename;
    property title : string read Ftitle write Ftitle stored wstHas_title;
    property description : string read Fdescription write Fdescription stored wstHas_description;
    property size : integer read Fsize write Fsize stored wstHas_size;
    property content_type : string read Fcontent_type write Fcontent_type stored wstHas_content_type;
    property date_submitted : TDateTimeRemotable read Fdate_submitted write Fdate_submitted stored wstHas_date_submitted;
    property download_url : anyURI read Fdownload_url write Fdownload_url stored wstHas_download_url;
    property user_id : integer read Fuser_id write Fuser_id stored wstHas_user_id;
  end;

  IssueHeaderData = class(TBaseComplexRemotable)
  private
    Fid : integer;
    Fview_state : integer;
    Flast_updated : TDateTimeRemotable;
    Fproject : integer;
    Fcategory : string;
    Fpriority : integer;
    Fseverity : integer;
    Fstatus : integer;
    Freporter : integer;
    Fsummary : string;
    Fhandler : integer;
    Fresolution : integer;
    Fattachments_count : integer;
    Fnotes_count : integer;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property id : integer read Fid write Fid;
    property view_state : integer read Fview_state write Fview_state;
    property last_updated : TDateTimeRemotable read Flast_updated write Flast_updated;
    property project : integer read Fproject write Fproject;
    property category : string read Fcategory write Fcategory;
    property priority : integer read Fpriority write Fpriority;
    property severity : integer read Fseverity write Fseverity;
    property status : integer read Fstatus write Fstatus;
    property reporter : integer read Freporter write Freporter;
    property summary : string read Fsummary write Fsummary;
    property handler : integer read Fhandler write Fhandler;
    property resolution : integer read Fresolution write Fresolution;
    property attachments_count : integer read Fattachments_count write Fattachments_count;
    property notes_count : integer read Fnotes_count write Fnotes_count;
  end;

  FilterData = class(TBaseComplexRemotable)
  private
    Fid : integer;
    Fowner : AccountData;
    Fproject_id : integer;
    Fis_public : boolean;
    Fname : string;
    Ffilter_string : string;
    Furl : string;
  private
    function wstHas_id() : Boolean;
    function wstHas_owner() : Boolean;
    function wstHas_project_id() : Boolean;
    function wstHas_is_public() : Boolean;
    function wstHas_name() : Boolean;
    function wstHas_filter_string() : Boolean;
    function wstHas_url() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property id : integer read Fid write Fid stored wstHas_id;
    property owner : AccountData read Fowner write Fowner stored wstHas_owner;
    property project_id : integer read Fproject_id write Fproject_id stored wstHas_project_id;
    property is_public : boolean read Fis_public write Fis_public stored wstHas_is_public;
    property name : string read Fname write Fname stored wstHas_name;
    property filter_string : string read Ffilter_string write Ffilter_string stored wstHas_filter_string;
    property url : string read Furl write Furl stored wstHas_url;
  end;

  CustomFieldDefinitionData = class(TBaseComplexRemotable)
  private
    Ffield : ObjectRef;
    F_type : integer;
    Fpossible_values : string;
    Fdefault_value : string;
    Fvalid_regexp : string;
    Faccess_level_r : integer;
    Faccess_level_rw : integer;
    Flength_min : integer;
    Flength_max : integer;
    Fadvanced : boolean;
    Fdisplay_report : boolean;
    Fdisplay_update : boolean;
    Fdisplay_resolved : boolean;
    Fdisplay_closed : boolean;
    Frequire_report : boolean;
    Frequire_update : boolean;
    Frequire_resolved : boolean;
    Frequire_closed : boolean;
  private
    function wstHas_field() : Boolean;
    function wstHas__type() : Boolean;
    function wstHas_possible_values() : Boolean;
    function wstHas_default_value() : Boolean;
    function wstHas_valid_regexp() : Boolean;
    function wstHas_access_level_r() : Boolean;
    function wstHas_access_level_rw() : Boolean;
    function wstHas_length_min() : Boolean;
    function wstHas_length_max() : Boolean;
    function wstHas_advanced() : Boolean;
    function wstHas_display_report() : Boolean;
    function wstHas_display_update() : Boolean;
    function wstHas_display_resolved() : Boolean;
    function wstHas_display_closed() : Boolean;
    function wstHas_require_report() : Boolean;
    function wstHas_require_update() : Boolean;
    function wstHas_require_resolved() : Boolean;
    function wstHas_require_closed() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property field : ObjectRef read Ffield write Ffield stored wstHas_field;
    property _type : integer read F_type write F_type stored wstHas__type;
    property possible_values : string read Fpossible_values write Fpossible_values stored wstHas_possible_values;
    property default_value : string read Fdefault_value write Fdefault_value stored wstHas_default_value;
    property valid_regexp : string read Fvalid_regexp write Fvalid_regexp stored wstHas_valid_regexp;
    property access_level_r : integer read Faccess_level_r write Faccess_level_r stored wstHas_access_level_r;
    property access_level_rw : integer read Faccess_level_rw write Faccess_level_rw stored wstHas_access_level_rw;
    property length_min : integer read Flength_min write Flength_min stored wstHas_length_min;
    property length_max : integer read Flength_max write Flength_max stored wstHas_length_max;
    property advanced : boolean read Fadvanced write Fadvanced stored wstHas_advanced;
    property display_report : boolean read Fdisplay_report write Fdisplay_report stored wstHas_display_report;
    property display_update : boolean read Fdisplay_update write Fdisplay_update stored wstHas_display_update;
    property display_resolved : boolean read Fdisplay_resolved write Fdisplay_resolved stored wstHas_display_resolved;
    property display_closed : boolean read Fdisplay_closed write Fdisplay_closed stored wstHas_display_closed;
    property require_report : boolean read Frequire_report write Frequire_report stored wstHas_require_report;
    property require_update : boolean read Frequire_update write Frequire_update stored wstHas_require_update;
    property require_resolved : boolean read Frequire_resolved write Frequire_resolved stored wstHas_require_resolved;
    property require_closed : boolean read Frequire_closed write Frequire_closed stored wstHas_require_closed;
  end;

  CustomFieldLinkForProjectData = class(TBaseComplexRemotable)
  private
    Ffield : ObjectRef;
    Fsequence : ShortInt;
  private
    function wstHas_field() : Boolean;
    function wstHas_sequence() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property field : ObjectRef read Ffield write Ffield stored wstHas_field;
    property sequence : ShortInt read Fsequence write Fsequence stored wstHas_sequence;
  end;

  CustomFieldValueForIssueData = class(TBaseComplexRemotable)
  private
    Ffield : ObjectRef;
    Fvalue : string;
  private
    function wstHas_field() : Boolean;
    function wstHas_value() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property field : ObjectRef read Ffield write Ffield stored wstHas_field;
    property value : string read Fvalue write Fvalue stored wstHas_value;
  end;

  ProfileData = class(TBaseComplexRemotable)
  private
    Fid : integer;
    Fuser_id : AccountData;
    Fplatform : string;
    Fos : string;
    Fos_build : string;
    Fdescription : string;
  private
    function wstHas_id() : Boolean;
    function wstHas_user_id() : Boolean;
    function wstHas_platform() : Boolean;
    function wstHas_os() : Boolean;
    function wstHas_os_build() : Boolean;
    function wstHas_description() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property id : integer read Fid write Fid stored wstHas_id;
    property user_id : AccountData read Fuser_id write Fuser_id stored wstHas_user_id;
    property platform : string read Fplatform write Fplatform stored wstHas_platform;
    property os : string read Fos write Fos stored wstHas_os;
    property os_build : string read Fos_build write Fos_build stored wstHas_os_build;
    property description : string read Fdescription write Fdescription stored wstHas_description;
  end;

  ObjectRefArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): ObjectRef;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : ObjectRef Read GetItem;Default;
  end;

  TagDataArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TagData;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TagData Read GetItem;Default;
  end;

  IssueDataArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): IssueData;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : IssueData Read GetItem;Default;
  end;

  IssueHeaderDataArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): IssueHeaderData;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : IssueHeaderData Read GetItem;Default;
  end;

  AccountDataArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): AccountData;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : AccountData Read GetItem;Default;
  end;

  ProjectDataArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): ProjectData;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : ProjectData Read GetItem;Default;
  end;

  StringArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of string;
  private
    function GetItem(AIndex: Integer): string;
    procedure SetItem(AIndex: Integer; const AValue: string);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    procedure Assign(Source: TPersistent); override;
    property Item[AIndex:Integer] : string read GetItem write SetItem; default;
  end;

  ProjectVersionDataArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): ProjectVersionData;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : ProjectVersionData Read GetItem;Default;
  end;

  ProjectAttachmentDataArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): ProjectAttachmentData;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : ProjectAttachmentData Read GetItem;Default;
  end;

  CustomFieldDefinitionDataArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): CustomFieldDefinitionData;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : CustomFieldDefinitionData Read GetItem;Default;
  end;

  FilterDataArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): FilterData;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : FilterData Read GetItem;Default;
  end;

  AttachmentDataArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): AttachmentData;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : AttachmentData Read GetItem;Default;
  end;

  RelationshipDataArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): RelationshipData;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : RelationshipData Read GetItem;Default;
  end;

  IssueNoteDataArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): IssueNoteData;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : IssueNoteData Read GetItem;Default;
  end;

  CustomFieldLinkForProjectDataArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): CustomFieldLinkForProjectData;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : CustomFieldLinkForProjectData Read GetItem;Default;
  end;

  CustomFieldValueForIssueDataArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): CustomFieldValueForIssueData;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : CustomFieldValueForIssueData Read GetItem;Default;
  end;

  ProfileDataArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): ProfileData;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : ProfileData Read GetItem;Default;
  end;

  MantisConnectPortType = interface(IInvokable)
    ['{8B3AA134-04F1-48BA-9E8E-078E89D96506}']
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
  end;

  procedure Register_mantis_ServiceMetadata();

Implementation
uses metadata_repository, record_rtti, wst_types;

{ UserData }

constructor UserData.Create();
begin
  inherited Create();
  Faccount_data := AccountData.Create();
end;

procedure UserData.FreeObjectProperties();
begin
  if Assigned(Faccount_data) then
    FreeAndNil(Faccount_data);
  inherited FreeObjectProperties();
end;

function UserData.wstHas_account_data() : Boolean;
begin
  Result := ( Faccount_data <> nil );
end;

function UserData.wstHas_access_level() : Boolean;
begin
  Result := ( Faccess_level <> integer(0) );
end;

function UserData.wstHas_timezone() : Boolean;
begin
  Result := ( Ftimezone <> '' );
end;

{ IssueData }

constructor IssueData.Create();
begin
  inherited Create();
  Fview_state := ObjectRef.Create();
  Flast_updated := TDateTimeRemotable.Create();
  Fproject := ObjectRef.Create();
  Fpriority := ObjectRef.Create();
  Fseverity := ObjectRef.Create();
  Fstatus := ObjectRef.Create();
  Freporter := AccountData.Create();
  Freproducibility := ObjectRef.Create();
  Fdate_submitted := TDateTimeRemotable.Create();
  Fhandler := AccountData.Create();
  Fprojection := ObjectRef.Create();
  Feta := ObjectRef.Create();
  Fresolution := ObjectRef.Create();
  Fattachments := AttachmentDataArray.Create();
  Frelationships := RelationshipDataArray.Create();
  Fnotes := IssueNoteDataArray.Create();
  Fcustom_fields := CustomFieldValueForIssueDataArray.Create();
  Fdue_date := TDateTimeRemotable.Create();
  Fmonitors := AccountDataArray.Create();
  Ftags := ObjectRefArray.Create();
end;

procedure IssueData.FreeObjectProperties();
begin
  if Assigned(Fview_state) then
    FreeAndNil(Fview_state);
  if Assigned(Flast_updated) then
    FreeAndNil(Flast_updated);
  if Assigned(Fproject) then
    FreeAndNil(Fproject);
  if Assigned(Fpriority) then
    FreeAndNil(Fpriority);
  if Assigned(Fseverity) then
    FreeAndNil(Fseverity);
  if Assigned(Fstatus) then
    FreeAndNil(Fstatus);
  if Assigned(Freporter) then
    FreeAndNil(Freporter);
  if Assigned(Freproducibility) then
    FreeAndNil(Freproducibility);
  if Assigned(Fdate_submitted) then
    FreeAndNil(Fdate_submitted);
  if Assigned(Fhandler) then
    FreeAndNil(Fhandler);
  if Assigned(Fprojection) then
    FreeAndNil(Fprojection);
  if Assigned(Feta) then
    FreeAndNil(Feta);
  if Assigned(Fresolution) then
    FreeAndNil(Fresolution);
  if Assigned(Fattachments) then
    FreeAndNil(Fattachments);
  if Assigned(Frelationships) then
    FreeAndNil(Frelationships);
  if Assigned(Fnotes) then
    FreeAndNil(Fnotes);
  if Assigned(Fcustom_fields) then
    FreeAndNil(Fcustom_fields);
  if Assigned(Fdue_date) then
    FreeAndNil(Fdue_date);
  if Assigned(Fmonitors) then
    FreeAndNil(Fmonitors);
  if Assigned(Ftags) then
    FreeAndNil(Ftags);
  inherited FreeObjectProperties();
end;

function IssueData.wstHas_id() : Boolean;
begin
  Result := ( Fid <> integer(0) );
end;

function IssueData.wstHas_view_state() : Boolean;
begin
  Result := ( Fview_state <> nil );
end;

function IssueData.wstHas_last_updated() : Boolean;
begin
  Result := ( Flast_updated <> nil );
end;

function IssueData.wstHas_project() : Boolean;
begin
  Result := ( Fproject <> nil );
end;

function IssueData.wstHas_category() : Boolean;
begin
  Result := ( Fcategory <> '' );
end;

function IssueData.wstHas_priority() : Boolean;
begin
  Result := ( Fpriority <> nil );
end;

function IssueData.wstHas_severity() : Boolean;
begin
  Result := ( Fseverity <> nil );
end;

function IssueData.wstHas_status() : Boolean;
begin
  Result := ( Fstatus <> nil );
end;

function IssueData.wstHas_reporter() : Boolean;
begin
  Result := ( Freporter <> nil );
end;

function IssueData.wstHas_summary() : Boolean;
begin
  Result := ( Fsummary <> '' );
end;

function IssueData.wstHas_version() : Boolean;
begin
  Result := ( Fversion <> '' );
end;

function IssueData.wstHas_build() : Boolean;
begin
  Result := ( Fbuild <> '' );
end;

function IssueData.wstHas_platform() : Boolean;
begin
  Result := ( Fplatform <> '' );
end;

function IssueData.wstHas_os() : Boolean;
begin
  Result := ( Fos <> '' );
end;

function IssueData.wstHas_os_build() : Boolean;
begin
  Result := ( Fos_build <> '' );
end;

function IssueData.wstHas_reproducibility() : Boolean;
begin
  Result := ( Freproducibility <> nil );
end;

function IssueData.wstHas_date_submitted() : Boolean;
begin
  Result := ( Fdate_submitted <> nil );
end;

function IssueData.wstHas_sponsorship_total() : Boolean;
begin
  Result := ( Fsponsorship_total <> integer(0) );
end;

function IssueData.wstHas_handler() : Boolean;
begin
  Result := ( Fhandler <> nil );
end;

function IssueData.wstHas_projection() : Boolean;
begin
  Result := ( Fprojection <> nil );
end;

function IssueData.wstHas_eta() : Boolean;
begin
  Result := ( Feta <> nil );
end;

function IssueData.wstHas_resolution() : Boolean;
begin
  Result := ( Fresolution <> nil );
end;

function IssueData.wstHas_fixed_in_version() : Boolean;
begin
  Result := ( Ffixed_in_version <> '' );
end;

function IssueData.wstHas_target_version() : Boolean;
begin
  Result := ( Ftarget_version <> '' );
end;

function IssueData.wstHas_description() : Boolean;
begin
  Result := ( Fdescription <> '' );
end;

function IssueData.wstHas_steps_to_reproduce() : Boolean;
begin
  Result := ( Fsteps_to_reproduce <> '' );
end;

function IssueData.wstHas_additional_information() : Boolean;
begin
  Result := ( Fadditional_information <> '' );
end;

function IssueData.wstHas_attachments() : Boolean;
begin
  Result := ( Fattachments <> AttachmentDataArray(0) );
end;

function IssueData.wstHas_relationships() : Boolean;
begin
  Result := ( Frelationships <> RelationshipDataArray(0) );
end;

function IssueData.wstHas_notes() : Boolean;
begin
  Result := ( Fnotes <> IssueNoteDataArray(0) );
end;

function IssueData.wstHas_custom_fields() : Boolean;
begin
  Result := ( Fcustom_fields <> CustomFieldValueForIssueDataArray(0) );
end;

function IssueData.wstHas_due_date() : Boolean;
begin
  Result := ( Fdue_date <> nil );
end;

function IssueData.wstHas_monitors() : Boolean;
begin
  Result := ( Fmonitors <> AccountDataArray(0) );
end;

function IssueData.wstHas_sticky() : Boolean;
begin
  Result := ( Fsticky <> boolean(0) );
end;

function IssueData.wstHas_tags() : Boolean;
begin
  Result := ( Ftags <> ObjectRefArray(0) );
end;

{ IssueNoteData }

constructor IssueNoteData.Create();
begin
  inherited Create();
  Freporter := AccountData.Create();
  Fview_state := ObjectRef.Create();
  Fdate_submitted := TDateTimeRemotable.Create();
  Flast_modified := TDateTimeRemotable.Create();
end;

procedure IssueNoteData.FreeObjectProperties();
begin
  if Assigned(Freporter) then
    FreeAndNil(Freporter);
  if Assigned(Fview_state) then
    FreeAndNil(Fview_state);
  if Assigned(Fdate_submitted) then
    FreeAndNil(Fdate_submitted);
  if Assigned(Flast_modified) then
    FreeAndNil(Flast_modified);
  inherited FreeObjectProperties();
end;

function IssueNoteData.wstHas_id() : Boolean;
begin
  Result := ( Fid <> integer(0) );
end;

function IssueNoteData.wstHas_reporter() : Boolean;
begin
  Result := ( Freporter <> nil );
end;

function IssueNoteData.wstHas_text() : Boolean;
begin
  Result := ( Ftext <> '' );
end;

function IssueNoteData.wstHas_view_state() : Boolean;
begin
  Result := ( Fview_state <> nil );
end;

function IssueNoteData.wstHas_date_submitted() : Boolean;
begin
  Result := ( Fdate_submitted <> nil );
end;

function IssueNoteData.wstHas_last_modified() : Boolean;
begin
  Result := ( Flast_modified <> nil );
end;

function IssueNoteData.wstHas_time_tracking() : Boolean;
begin
  Result := ( Ftime_tracking <> integer(0) );
end;

function IssueNoteData.wstHas_note_type() : Boolean;
begin
  Result := ( Fnote_type <> integer(0) );
end;

function IssueNoteData.wstHas_note_attr() : Boolean;
begin
  Result := ( Fnote_attr <> '' );
end;

{ RelationshipData }

constructor RelationshipData.Create();
begin
  inherited Create();
  F_type := ObjectRef.Create();
end;

procedure RelationshipData.FreeObjectProperties();
begin
  if Assigned(F_type) then
    FreeAndNil(F_type);
  inherited FreeObjectProperties();
end;

function RelationshipData.wstHas_id() : Boolean;
begin
  Result := ( Fid <> integer(0) );
end;

function RelationshipData.wstHas__type() : Boolean;
begin
  Result := ( F_type <> nil );
end;

function RelationshipData.wstHas_target_id() : Boolean;
begin
  Result := ( Ftarget_id <> integer(0) );
end;

{ ProjectData }

constructor ProjectData.Create();
begin
  inherited Create();
  Fstatus := ObjectRef.Create();
  Fview_state := ObjectRef.Create();
  Faccess_min := ObjectRef.Create();
  Fsubprojects := ProjectDataArray.Create();
end;

procedure ProjectData.FreeObjectProperties();
begin
  if Assigned(Fstatus) then
    FreeAndNil(Fstatus);
  if Assigned(Fview_state) then
    FreeAndNil(Fview_state);
  if Assigned(Faccess_min) then
    FreeAndNil(Faccess_min);
  if Assigned(Fsubprojects) then
    FreeAndNil(Fsubprojects);
  inherited FreeObjectProperties();
end;

function ProjectData.wstHas_id() : Boolean;
begin
  Result := ( Fid <> integer(0) );
end;

function ProjectData.wstHas_name() : Boolean;
begin
  Result := ( Fname <> '' );
end;

function ProjectData.wstHas_status() : Boolean;
begin
  Result := ( Fstatus <> nil );
end;

function ProjectData.wstHas_enabled() : Boolean;
begin
  Result := ( Fenabled <> boolean(0) );
end;

function ProjectData.wstHas_view_state() : Boolean;
begin
  Result := ( Fview_state <> nil );
end;

function ProjectData.wstHas_access_min() : Boolean;
begin
  Result := ( Faccess_min <> nil );
end;

function ProjectData.wstHas_file_path() : Boolean;
begin
  Result := ( Ffile_path <> '' );
end;

function ProjectData.wstHas_description() : Boolean;
begin
  Result := ( Fdescription <> '' );
end;

function ProjectData.wstHas_subprojects() : Boolean;
begin
  Result := ( Fsubprojects <> ProjectDataArray(0) );
end;

function ProjectData.wstHas_inherit_global() : Boolean;
begin
  Result := ( Finherit_global <> boolean(0) );
end;

{ ProjectVersionData }

constructor ProjectVersionData.Create();
begin
  inherited Create();
  Fdate_order := TDateTimeRemotable.Create();
end;

procedure ProjectVersionData.FreeObjectProperties();
begin
  if Assigned(Fdate_order) then
    FreeAndNil(Fdate_order);
  inherited FreeObjectProperties();
end;

function ProjectVersionData.wstHas_id() : Boolean;
begin
  Result := ( Fid <> integer(0) );
end;

function ProjectVersionData.wstHas_name() : Boolean;
begin
  Result := ( Fname <> '' );
end;

function ProjectVersionData.wstHas_project_id() : Boolean;
begin
  Result := ( Fproject_id <> integer(0) );
end;

function ProjectVersionData.wstHas_date_order() : Boolean;
begin
  Result := ( Fdate_order <> nil );
end;

function ProjectVersionData.wstHas_description() : Boolean;
begin
  Result := ( Fdescription <> '' );
end;

function ProjectVersionData.wstHas_released() : Boolean;
begin
  Result := ( Freleased <> boolean(0) );
end;

function ProjectVersionData.wstHas_obsolete() : Boolean;
begin
  Result := ( Fobsolete <> boolean(0) );
end;

{ ProfileDataSearchResult }

constructor ProfileDataSearchResult.Create();
begin
  inherited Create();
  Fresults := ProfileDataArray.Create();
end;

procedure ProfileDataSearchResult.FreeObjectProperties();
begin
  if Assigned(Fresults) then
    FreeAndNil(Fresults);
  inherited FreeObjectProperties();
end;

function ProfileDataSearchResult.wstHas_results() : Boolean;
begin
  Result := ( Fresults <> ProfileDataArray(0) );
end;

function ProfileDataSearchResult.wstHas_total_results() : Boolean;
begin
  Result := ( Ftotal_results <> integer(0) );
end;

{ TagDataSearchResult }

constructor TagDataSearchResult.Create();
begin
  inherited Create();
  Fresults := TagDataArray.Create();
end;

procedure TagDataSearchResult.FreeObjectProperties();
begin
  if Assigned(Fresults) then
    FreeAndNil(Fresults);
  inherited FreeObjectProperties();
end;

function TagDataSearchResult.wstHas_results() : Boolean;
begin
  Result := ( Fresults <> TagDataArray(0) );
end;

function TagDataSearchResult.wstHas_total_results() : Boolean;
begin
  Result := ( Ftotal_results <> integer(0) );
end;

{ TagData }

constructor TagData.Create();
begin
  inherited Create();
  Fuser_id := AccountData.Create();
  Fdate_created := TDateTimeRemotable.Create();
  Fdate_updated := TDateTimeRemotable.Create();
end;

procedure TagData.FreeObjectProperties();
begin
  if Assigned(Fuser_id) then
    FreeAndNil(Fuser_id);
  if Assigned(Fdate_created) then
    FreeAndNil(Fdate_created);
  if Assigned(Fdate_updated) then
    FreeAndNil(Fdate_updated);
  inherited FreeObjectProperties();
end;

function TagData.wstHas_id() : Boolean;
begin
  Result := ( Fid <> integer(0) );
end;

function TagData.wstHas_user_id() : Boolean;
begin
  Result := ( Fuser_id <> nil );
end;

function TagData.wstHas_name() : Boolean;
begin
  Result := ( Fname <> '' );
end;

function TagData.wstHas_description() : Boolean;
begin
  Result := ( Fdescription <> '' );
end;

function TagData.wstHas_date_created() : Boolean;
begin
  Result := ( Fdate_created <> nil );
end;

function TagData.wstHas_date_updated() : Boolean;
begin
  Result := ( Fdate_updated <> nil );
end;

function ObjectRef.wstHas_id() : Boolean;
begin
  Result := ( Fid <> integer(0) );
end;

function ObjectRef.wstHas_name() : Boolean;
begin
  Result := ( Fname <> '' );
end;

function AccountData.wstHas_id() : Boolean;
begin
  Result := ( Fid <> integer(0) );
end;

function AccountData.wstHas_name() : Boolean;
begin
  Result := ( Fname <> '' );
end;

function AccountData.wstHas_real_name() : Boolean;
begin
  Result := ( Freal_name <> '' );
end;

function AccountData.wstHas_email() : Boolean;
begin
  Result := ( Femail <> '' );
end;

{ AttachmentData }

constructor AttachmentData.Create();
begin
  inherited Create();
  Fdate_submitted := TDateTimeRemotable.Create();
end;

procedure AttachmentData.FreeObjectProperties();
begin
  if Assigned(Fdate_submitted) then
    FreeAndNil(Fdate_submitted);
  inherited FreeObjectProperties();
end;

function AttachmentData.wstHas_id() : Boolean;
begin
  Result := ( Fid <> integer(0) );
end;

function AttachmentData.wstHas_filename() : Boolean;
begin
  Result := ( Ffilename <> '' );
end;

function AttachmentData.wstHas_size() : Boolean;
begin
  Result := ( Fsize <> integer(0) );
end;

function AttachmentData.wstHas_content_type() : Boolean;
begin
  Result := ( Fcontent_type <> '' );
end;

function AttachmentData.wstHas_date_submitted() : Boolean;
begin
  Result := ( Fdate_submitted <> nil );
end;

function AttachmentData.wstHas_download_url() : Boolean;
begin
  Result := ( Fdownload_url <> '' );
end;

function AttachmentData.wstHas_user_id() : Boolean;
begin
  Result := ( Fuser_id <> integer(0) );
end;

{ ProjectAttachmentData }

constructor ProjectAttachmentData.Create();
begin
  inherited Create();
  Fdate_submitted := TDateTimeRemotable.Create();
end;

procedure ProjectAttachmentData.FreeObjectProperties();
begin
  if Assigned(Fdate_submitted) then
    FreeAndNil(Fdate_submitted);
  inherited FreeObjectProperties();
end;

function ProjectAttachmentData.wstHas_id() : Boolean;
begin
  Result := ( Fid <> integer(0) );
end;

function ProjectAttachmentData.wstHas_filename() : Boolean;
begin
  Result := ( Ffilename <> '' );
end;

function ProjectAttachmentData.wstHas_title() : Boolean;
begin
  Result := ( Ftitle <> '' );
end;

function ProjectAttachmentData.wstHas_description() : Boolean;
begin
  Result := ( Fdescription <> '' );
end;

function ProjectAttachmentData.wstHas_size() : Boolean;
begin
  Result := ( Fsize <> integer(0) );
end;

function ProjectAttachmentData.wstHas_content_type() : Boolean;
begin
  Result := ( Fcontent_type <> '' );
end;

function ProjectAttachmentData.wstHas_date_submitted() : Boolean;
begin
  Result := ( Fdate_submitted <> nil );
end;

function ProjectAttachmentData.wstHas_download_url() : Boolean;
begin
  Result := ( Fdownload_url <> '' );
end;

function ProjectAttachmentData.wstHas_user_id() : Boolean;
begin
  Result := ( Fuser_id <> integer(0) );
end;

{ IssueHeaderData }

constructor IssueHeaderData.Create();
begin
  inherited Create();
  Flast_updated := TDateTimeRemotable.Create();
end;

procedure IssueHeaderData.FreeObjectProperties();
begin
  if Assigned(Flast_updated) then
    FreeAndNil(Flast_updated);
  inherited FreeObjectProperties();
end;

{ FilterData }

constructor FilterData.Create();
begin
  inherited Create();
  Fowner := AccountData.Create();
end;

procedure FilterData.FreeObjectProperties();
begin
  if Assigned(Fowner) then
    FreeAndNil(Fowner);
  inherited FreeObjectProperties();
end;

function FilterData.wstHas_id() : Boolean;
begin
  Result := ( Fid <> integer(0) );
end;

function FilterData.wstHas_owner() : Boolean;
begin
  Result := ( Fowner <> nil );
end;

function FilterData.wstHas_project_id() : Boolean;
begin
  Result := ( Fproject_id <> integer(0) );
end;

function FilterData.wstHas_is_public() : Boolean;
begin
  Result := ( Fis_public <> boolean(0) );
end;

function FilterData.wstHas_name() : Boolean;
begin
  Result := ( Fname <> '' );
end;

function FilterData.wstHas_filter_string() : Boolean;
begin
  Result := ( Ffilter_string <> '' );
end;

function FilterData.wstHas_url() : Boolean;
begin
  Result := ( Furl <> '' );
end;

{ CustomFieldDefinitionData }

constructor CustomFieldDefinitionData.Create();
begin
  inherited Create();
  Ffield := ObjectRef.Create();
end;

procedure CustomFieldDefinitionData.FreeObjectProperties();
begin
  if Assigned(Ffield) then
    FreeAndNil(Ffield);
  inherited FreeObjectProperties();
end;

function CustomFieldDefinitionData.wstHas_field() : Boolean;
begin
  Result := ( Ffield <> nil );
end;

function CustomFieldDefinitionData.wstHas__type() : Boolean;
begin
  Result := ( F_type <> integer(0) );
end;

function CustomFieldDefinitionData.wstHas_possible_values() : Boolean;
begin
  Result := ( Fpossible_values <> '' );
end;

function CustomFieldDefinitionData.wstHas_default_value() : Boolean;
begin
  Result := ( Fdefault_value <> '' );
end;

function CustomFieldDefinitionData.wstHas_valid_regexp() : Boolean;
begin
  Result := ( Fvalid_regexp <> '' );
end;

function CustomFieldDefinitionData.wstHas_access_level_r() : Boolean;
begin
  Result := ( Faccess_level_r <> integer(0) );
end;

function CustomFieldDefinitionData.wstHas_access_level_rw() : Boolean;
begin
  Result := ( Faccess_level_rw <> integer(0) );
end;

function CustomFieldDefinitionData.wstHas_length_min() : Boolean;
begin
  Result := ( Flength_min <> integer(0) );
end;

function CustomFieldDefinitionData.wstHas_length_max() : Boolean;
begin
  Result := ( Flength_max <> integer(0) );
end;

function CustomFieldDefinitionData.wstHas_advanced() : Boolean;
begin
  Result := ( Fadvanced <> boolean(0) );
end;

function CustomFieldDefinitionData.wstHas_display_report() : Boolean;
begin
  Result := ( Fdisplay_report <> boolean(0) );
end;

function CustomFieldDefinitionData.wstHas_display_update() : Boolean;
begin
  Result := ( Fdisplay_update <> boolean(0) );
end;

function CustomFieldDefinitionData.wstHas_display_resolved() : Boolean;
begin
  Result := ( Fdisplay_resolved <> boolean(0) );
end;

function CustomFieldDefinitionData.wstHas_display_closed() : Boolean;
begin
  Result := ( Fdisplay_closed <> boolean(0) );
end;

function CustomFieldDefinitionData.wstHas_require_report() : Boolean;
begin
  Result := ( Frequire_report <> boolean(0) );
end;

function CustomFieldDefinitionData.wstHas_require_update() : Boolean;
begin
  Result := ( Frequire_update <> boolean(0) );
end;

function CustomFieldDefinitionData.wstHas_require_resolved() : Boolean;
begin
  Result := ( Frequire_resolved <> boolean(0) );
end;

function CustomFieldDefinitionData.wstHas_require_closed() : Boolean;
begin
  Result := ( Frequire_closed <> boolean(0) );
end;

{ CustomFieldLinkForProjectData }

constructor CustomFieldLinkForProjectData.Create();
begin
  inherited Create();
  Ffield := ObjectRef.Create();
end;

procedure CustomFieldLinkForProjectData.FreeObjectProperties();
begin
  if Assigned(Ffield) then
    FreeAndNil(Ffield);
  inherited FreeObjectProperties();
end;

function CustomFieldLinkForProjectData.wstHas_field() : Boolean;
begin
  Result := ( Ffield <> nil );
end;

function CustomFieldLinkForProjectData.wstHas_sequence() : Boolean;
begin
  Result := ( Fsequence <> ShortInt(0) );
end;

{ CustomFieldValueForIssueData }

constructor CustomFieldValueForIssueData.Create();
begin
  inherited Create();
  Ffield := ObjectRef.Create();
end;

procedure CustomFieldValueForIssueData.FreeObjectProperties();
begin
  if Assigned(Ffield) then
    FreeAndNil(Ffield);
  inherited FreeObjectProperties();
end;

function CustomFieldValueForIssueData.wstHas_field() : Boolean;
begin
  Result := ( Ffield <> nil );
end;

function CustomFieldValueForIssueData.wstHas_value() : Boolean;
begin
  Result := ( Fvalue <> '' );
end;

{ ProfileData }

constructor ProfileData.Create();
begin
  inherited Create();
  Fuser_id := AccountData.Create();
end;

procedure ProfileData.FreeObjectProperties();
begin
  if Assigned(Fuser_id) then
    FreeAndNil(Fuser_id);
  inherited FreeObjectProperties();
end;

function ProfileData.wstHas_id() : Boolean;
begin
  Result := ( Fid <> integer(0) );
end;

function ProfileData.wstHas_user_id() : Boolean;
begin
  Result := ( Fuser_id <> nil );
end;

function ProfileData.wstHas_platform() : Boolean;
begin
  Result := ( Fplatform <> '' );
end;

function ProfileData.wstHas_os() : Boolean;
begin
  Result := ( Fos <> '' );
end;

function ProfileData.wstHas_os_build() : Boolean;
begin
  Result := ( Fos_build <> '' );
end;

function ProfileData.wstHas_description() : Boolean;
begin
  Result := ( Fdescription <> '' );
end;

{ ObjectRefArray }

function ObjectRefArray.GetItem(AIndex: Integer): ObjectRef;
begin
  Result := ObjectRef(Inherited GetItem(AIndex));
end;

class function ObjectRefArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= ObjectRef;
end;

{ TagDataArray }

function TagDataArray.GetItem(AIndex: Integer): TagData;
begin
  Result := TagData(Inherited GetItem(AIndex));
end;

class function TagDataArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TagData;
end;

{ IssueDataArray }

function IssueDataArray.GetItem(AIndex: Integer): IssueData;
begin
  Result := IssueData(Inherited GetItem(AIndex));
end;

class function IssueDataArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= IssueData;
end;

{ IssueHeaderDataArray }

function IssueHeaderDataArray.GetItem(AIndex: Integer): IssueHeaderData;
begin
  Result := IssueHeaderData(Inherited GetItem(AIndex));
end;

class function IssueHeaderDataArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= IssueHeaderData;
end;

{ AccountDataArray }

function AccountDataArray.GetItem(AIndex: Integer): AccountData;
begin
  Result := AccountData(Inherited GetItem(AIndex));
end;

class function AccountDataArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= AccountData;
end;

{ ProjectDataArray }

function ProjectDataArray.GetItem(AIndex: Integer): ProjectData;
begin
  Result := ProjectData(Inherited GetItem(AIndex));
end;

class function ProjectDataArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= ProjectData;
end;

{ StringArray }

function StringArray.GetItem(AIndex: Integer): string;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure StringArray.SetItem(AIndex: Integer;const AValue: string);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function StringArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure StringArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('item',TypeInfo(string),FData[AIndex]);
end;

procedure StringArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'item';
  AStore.Get(TypeInfo(string),sName,FData[AIndex]);
end;

class function StringArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(string);
end;

procedure StringArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

procedure StringArray.Assign(Source: TPersistent);
var
  src : StringArray;
  i, c : PtrInt;
begin
  if Assigned(Source) and Source.InheritsFrom(StringArray) then begin
    src := StringArray(Source);
    c := src.Length;
    Self.SetLength(c);
    if ( c > 0 ) then begin
      for i := 0 to Pred(c) do begin
        Self[i] := src[i];
      end;
    end;
  end else begin
    inherited Assign(Source);
  end;
end;

{ ProjectVersionDataArray }

function ProjectVersionDataArray.GetItem(AIndex: Integer): ProjectVersionData;
begin
  Result := ProjectVersionData(Inherited GetItem(AIndex));
end;

class function ProjectVersionDataArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= ProjectVersionData;
end;

{ ProjectAttachmentDataArray }

function ProjectAttachmentDataArray.GetItem(AIndex: Integer): ProjectAttachmentData;
begin
  Result := ProjectAttachmentData(Inherited GetItem(AIndex));
end;

class function ProjectAttachmentDataArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= ProjectAttachmentData;
end;

{ CustomFieldDefinitionDataArray }

function CustomFieldDefinitionDataArray.GetItem(AIndex: Integer): CustomFieldDefinitionData;
begin
  Result := CustomFieldDefinitionData(Inherited GetItem(AIndex));
end;

class function CustomFieldDefinitionDataArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= CustomFieldDefinitionData;
end;

{ FilterDataArray }

function FilterDataArray.GetItem(AIndex: Integer): FilterData;
begin
  Result := FilterData(Inherited GetItem(AIndex));
end;

class function FilterDataArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= FilterData;
end;

{ AttachmentDataArray }

function AttachmentDataArray.GetItem(AIndex: Integer): AttachmentData;
begin
  Result := AttachmentData(Inherited GetItem(AIndex));
end;

class function AttachmentDataArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= AttachmentData;
end;

{ RelationshipDataArray }

function RelationshipDataArray.GetItem(AIndex: Integer): RelationshipData;
begin
  Result := RelationshipData(Inherited GetItem(AIndex));
end;

class function RelationshipDataArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= RelationshipData;
end;

{ IssueNoteDataArray }

function IssueNoteDataArray.GetItem(AIndex: Integer): IssueNoteData;
begin
  Result := IssueNoteData(Inherited GetItem(AIndex));
end;

class function IssueNoteDataArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= IssueNoteData;
end;

{ CustomFieldLinkForProjectDataArray }

function CustomFieldLinkForProjectDataArray.GetItem(AIndex: Integer): CustomFieldLinkForProjectData;
begin
  Result := CustomFieldLinkForProjectData(Inherited GetItem(AIndex));
end;

class function CustomFieldLinkForProjectDataArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= CustomFieldLinkForProjectData;
end;

{ CustomFieldValueForIssueDataArray }

function CustomFieldValueForIssueDataArray.GetItem(AIndex: Integer): CustomFieldValueForIssueData;
begin
  Result := CustomFieldValueForIssueData(Inherited GetItem(AIndex));
end;

class function CustomFieldValueForIssueDataArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= CustomFieldValueForIssueData;
end;

{ ProfileDataArray }

function ProfileDataArray.GetItem(AIndex: Integer): ProfileData;
begin
  Result := ProfileData(Inherited GetItem(AIndex));
end;

class function ProfileDataArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= ProfileData;
end;


procedure Register_mantis_ServiceMetadata();
var
  mm : IModuleMetadataMngr;
begin
  mm := GetModuleMetadataMngr();
  mm.SetRepositoryNameSpace(sUNIT_NAME, sNAME_SPACE);
  mm.SetServiceCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'TRANSPORT_Address',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php'
  );
  mm.SetServiceCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'FORMAT_Style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_version',
    '_E_N_',
    'mc_version'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_version',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_version',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_version'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_version',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_version',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_login',
    '_E_N_',
    'mc_login'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_login',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_login',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_login'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_login',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_login',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_status',
    '_E_N_',
    'mc_enum_status'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_status',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_status',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_enum_status'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_status',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_status',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_priorities',
    '_E_N_',
    'mc_enum_priorities'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_priorities',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_priorities',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_enum_priorities'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_priorities',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_priorities',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_severities',
    '_E_N_',
    'mc_enum_severities'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_severities',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_severities',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_enum_severities'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_severities',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_severities',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_reproducibilities',
    '_E_N_',
    'mc_enum_reproducibilities'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_reproducibilities',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_reproducibilities',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_enum_reproducibilities'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_reproducibilities',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_reproducibilities',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_projections',
    '_E_N_',
    'mc_enum_projections'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_projections',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_projections',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_enum_projections'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_projections',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_projections',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_etas',
    '_E_N_',
    'mc_enum_etas'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_etas',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_etas',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_enum_etas'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_etas',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_etas',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_resolutions',
    '_E_N_',
    'mc_enum_resolutions'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_resolutions',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_resolutions',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_enum_resolutions'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_resolutions',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_resolutions',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_access_levels',
    '_E_N_',
    'mc_enum_access_levels'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_access_levels',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_access_levels',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_enum_access_levels'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_access_levels',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_access_levels',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_project_status',
    '_E_N_',
    'mc_enum_project_status'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_project_status',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_project_status',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_enum_project_status'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_project_status',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_project_status',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_project_view_states',
    '_E_N_',
    'mc_enum_project_view_states'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_project_view_states',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_project_view_states',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_enum_project_view_states'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_project_view_states',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_project_view_states',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_view_states',
    '_E_N_',
    'mc_enum_view_states'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_view_states',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_view_states',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_enum_view_states'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_view_states',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_view_states',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_custom_field_types',
    '_E_N_',
    'mc_enum_custom_field_types'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_custom_field_types',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_custom_field_types',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_enum_custom_field_types'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_custom_field_types',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_custom_field_types',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_get',
    '_E_N_',
    'mc_enum_get'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_get',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_get',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_enum_get'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_get',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_enum_get',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_exists',
    '_E_N_',
    'mc_issue_exists'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_exists',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_exists',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_issue_exists'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_exists',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_exists',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_get',
    '_E_N_',
    'mc_issue_get'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_get',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_get',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_issue_get'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_get',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_get',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_get_biggest_id',
    '_E_N_',
    'mc_issue_get_biggest_id'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_get_biggest_id',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_get_biggest_id',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_issue_get_biggest_id'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_get_biggest_id',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_get_biggest_id',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_get_id_from_summary',
    '_E_N_',
    'mc_issue_get_id_from_summary'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_get_id_from_summary',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_get_id_from_summary',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_issue_get_id_from_summary'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_get_id_from_summary',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_get_id_from_summary',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_add',
    '_E_N_',
    'mc_issue_add'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_add',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_add',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_issue_add'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_add',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_add',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_update',
    '_E_N_',
    'mc_issue_update'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_update',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_update',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_issue_update'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_update',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_update',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_set_tags',
    '_E_N_',
    'mc_issue_set_tags'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_set_tags',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_set_tags',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_issue_set_tags'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_set_tags',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_set_tags',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_delete',
    '_E_N_',
    'mc_issue_delete'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_delete',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_delete',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_issue_delete'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_delete',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_delete',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_note_add',
    '_E_N_',
    'mc_issue_note_add'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_note_add',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_note_add',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_issue_note_add'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_note_add',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_note_add',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_note_delete',
    '_E_N_',
    'mc_issue_note_delete'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_note_delete',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_note_delete',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_issue_note_delete'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_note_delete',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_note_delete',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_note_update',
    '_E_N_',
    'mc_issue_note_update'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_note_update',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_note_update',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_issue_note_update'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_note_update',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_note_update',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_relationship_add',
    '_E_N_',
    'mc_issue_relationship_add'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_relationship_add',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_relationship_add',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_issue_relationship_add'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_relationship_add',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_relationship_add',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_relationship_delete',
    '_E_N_',
    'mc_issue_relationship_delete'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_relationship_delete',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_relationship_delete',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_issue_relationship_delete'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_relationship_delete',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_relationship_delete',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_attachment_add',
    '_E_N_',
    'mc_issue_attachment_add'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_attachment_add',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_attachment_add',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_issue_attachment_add'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_attachment_add',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_attachment_add',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_attachment_delete',
    '_E_N_',
    'mc_issue_attachment_delete'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_attachment_delete',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_attachment_delete',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_issue_attachment_delete'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_attachment_delete',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_attachment_delete',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_attachment_get',
    '_E_N_',
    'mc_issue_attachment_get'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_attachment_get',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_attachment_get',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_issue_attachment_get'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_attachment_get',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_attachment_get',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_add',
    '_E_N_',
    'mc_project_add'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_add',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_add',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_project_add'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_add',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_add',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_delete',
    '_E_N_',
    'mc_project_delete'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_delete',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_delete',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_project_delete'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_delete',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_delete',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_update',
    '_E_N_',
    'mc_project_update'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_update',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_update',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_project_update'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_update',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_update',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_id_from_name',
    '_E_N_',
    'mc_project_get_id_from_name'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_id_from_name',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_id_from_name',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_project_get_id_from_name'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_id_from_name',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_id_from_name',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_issues',
    '_E_N_',
    'mc_project_get_issues'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_issues',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_issues',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_project_get_issues'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_issues',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_issues',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_issue_headers',
    '_E_N_',
    'mc_project_get_issue_headers'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_issue_headers',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_issue_headers',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_project_get_issue_headers'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_issue_headers',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_issue_headers',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_users',
    '_E_N_',
    'mc_project_get_users'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_users',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_users',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_project_get_users'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_users',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_users',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_projects_get_user_accessible',
    '_E_N_',
    'mc_projects_get_user_accessible'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_projects_get_user_accessible',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_projects_get_user_accessible',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_projects_get_user_accessible'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_projects_get_user_accessible',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_projects_get_user_accessible',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_categories',
    '_E_N_',
    'mc_project_get_categories'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_categories',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_categories',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_project_get_categories'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_categories',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_categories',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_add_category',
    '_E_N_',
    'mc_project_add_category'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_add_category',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_add_category',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_project_add_category'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_add_category',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_add_category',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_delete_category',
    '_E_N_',
    'mc_project_delete_category'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_delete_category',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_delete_category',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_project_delete_category'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_delete_category',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_delete_category',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_rename_category_by_name',
    '_E_N_',
    'mc_project_rename_category_by_name'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_rename_category_by_name',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_rename_category_by_name',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_project_rename_category_by_name'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_rename_category_by_name',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_rename_category_by_name',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_versions',
    '_E_N_',
    'mc_project_get_versions'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_versions',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_versions',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_project_get_versions'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_versions',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_versions',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_version_add',
    '_E_N_',
    'mc_project_version_add'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_version_add',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_version_add',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_project_version_add'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_version_add',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_version_add',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_version_update',
    '_E_N_',
    'mc_project_version_update'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_version_update',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_version_update',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_project_version_update'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_version_update',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_version_update',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_version_delete',
    '_E_N_',
    'mc_project_version_delete'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_version_delete',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_version_delete',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_project_version_delete'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_version_delete',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_version_delete',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_released_versions',
    '_E_N_',
    'mc_project_get_released_versions'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_released_versions',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_released_versions',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_project_get_released_versions'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_released_versions',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_released_versions',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_unreleased_versions',
    '_E_N_',
    'mc_project_get_unreleased_versions'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_unreleased_versions',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_unreleased_versions',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_project_get_unreleased_versions'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_unreleased_versions',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_unreleased_versions',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_attachments',
    '_E_N_',
    'mc_project_get_attachments'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_attachments',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_attachments',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_project_get_attachments'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_attachments',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_attachments',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_custom_fields',
    '_E_N_',
    'mc_project_get_custom_fields'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_custom_fields',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_custom_fields',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_project_get_custom_fields'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_custom_fields',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_custom_fields',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_attachment_get',
    '_E_N_',
    'mc_project_attachment_get'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_attachment_get',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_attachment_get',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_project_attachment_get'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_attachment_get',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_attachment_get',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_attachment_add',
    '_E_N_',
    'mc_project_attachment_add'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_attachment_add',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_attachment_add',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_project_attachment_add'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_attachment_add',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_attachment_add',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_attachment_delete',
    '_E_N_',
    'mc_project_attachment_delete'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_attachment_delete',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_attachment_delete',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_project_attachment_delete'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_attachment_delete',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_attachment_delete',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_all_subprojects',
    '_E_N_',
    'mc_project_get_all_subprojects'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_all_subprojects',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_all_subprojects',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_project_get_all_subprojects'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_all_subprojects',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_project_get_all_subprojects',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_filter_get',
    '_E_N_',
    'mc_filter_get'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_filter_get',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_filter_get',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_filter_get'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_filter_get',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_filter_get',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_filter_get_issues',
    '_E_N_',
    'mc_filter_get_issues'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_filter_get_issues',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_filter_get_issues',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_filter_get_issues'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_filter_get_issues',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_filter_get_issues',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_filter_get_issue_headers',
    '_E_N_',
    'mc_filter_get_issue_headers'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_filter_get_issue_headers',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_filter_get_issue_headers',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_filter_get_issue_headers'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_filter_get_issue_headers',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_filter_get_issue_headers',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_config_get_string',
    '_E_N_',
    'mc_config_get_string'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_config_get_string',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_config_get_string',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_config_get_string'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_config_get_string',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_config_get_string',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_checkin',
    '_E_N_',
    'mc_issue_checkin'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_checkin',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_checkin',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_issue_checkin'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_checkin',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_issue_checkin',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_user_pref_get_pref',
    '_E_N_',
    'mc_user_pref_get_pref'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_user_pref_get_pref',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_user_pref_get_pref',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_user_pref_get_pref'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_user_pref_get_pref',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_user_pref_get_pref',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_user_profiles_get_all',
    '_E_N_',
    'mc_user_profiles_get_all'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_user_profiles_get_all',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_user_profiles_get_all',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_user_profiles_get_all'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_user_profiles_get_all',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_user_profiles_get_all',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_tag_get_all',
    '_E_N_',
    'mc_tag_get_all'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_tag_get_all',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_tag_get_all',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_tag_get_all'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_tag_get_all',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_tag_get_all',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_tag_add',
    '_E_N_',
    'mc_tag_add'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_tag_add',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_tag_add',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_tag_add'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_tag_add',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_tag_add',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_tag_delete',
    '_E_N_',
    'mc_tag_delete'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_tag_delete',
    'style',
    'rpc'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_tag_delete',
    'TRANSPORT_soapAction',
    'http://bugs.freepascal.org/api/soap/mantisconnect.php/mc_tag_delete'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_tag_delete',
    'FORMAT_Input_EncodingStyle',
    'encoded'
  );
  mm.SetOperationCustomData(
    sUNIT_NAME,
    'MantisConnectPortType',
    'mc_tag_delete',
    'FORMAT_OutputEncodingStyle',
    'encoded'
  );
end;


var
  typeRegistryInstance : TTypeRegistry = nil;
initialization
  typeRegistryInstance := GetTypeRegistry();

  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(UserData),'UserData');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(IssueData),'IssueData');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(IssueNoteData),'IssueNoteData');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(RelationshipData),'RelationshipData');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(ProjectData),'ProjectData');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(ProjectVersionData),'ProjectVersionData');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(ProfileDataSearchResult),'ProfileDataSearchResult');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TagDataSearchResult),'TagDataSearchResult');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TagData),'TagData');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(ObjectRef),'ObjectRef');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(AccountData),'AccountData');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(AttachmentData),'AttachmentData');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(ProjectAttachmentData),'ProjectAttachmentData');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(IssueHeaderData),'IssueHeaderData');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(FilterData),'FilterData');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(CustomFieldDefinitionData),'CustomFieldDefinitionData');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(CustomFieldLinkForProjectData),'CustomFieldLinkForProjectData');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(CustomFieldValueForIssueData),'CustomFieldValueForIssueData');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(ProfileData),'ProfileData');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(ObjectRefArray),'ObjectRefArray');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TagDataArray),'TagDataArray');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(IssueDataArray),'IssueDataArray');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(IssueHeaderDataArray),'IssueHeaderDataArray');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(AccountDataArray),'AccountDataArray');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(ProjectDataArray),'ProjectDataArray');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(StringArray),'StringArray');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(ProjectVersionDataArray),'ProjectVersionDataArray');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(ProjectAttachmentDataArray),'ProjectAttachmentDataArray');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(CustomFieldDefinitionDataArray),'CustomFieldDefinitionDataArray');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(FilterDataArray),'FilterDataArray');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(AttachmentDataArray),'AttachmentDataArray');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(RelationshipDataArray),'RelationshipDataArray');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(IssueNoteDataArray),'IssueNoteDataArray');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(CustomFieldLinkForProjectDataArray),'CustomFieldLinkForProjectDataArray');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(CustomFieldValueForIssueDataArray),'CustomFieldValueForIssueDataArray');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(ProfileDataArray),'ProfileDataArray');

  typeRegistryInstance.ItemByTypeInfo[TypeInfo(RelationshipData)].RegisterExternalPropertyName('_type','type');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(CustomFieldDefinitionData)].RegisterExternalPropertyName('_type','type');


End.
