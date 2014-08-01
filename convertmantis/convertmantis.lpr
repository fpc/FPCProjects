{
  Convert Mantis Bugtracker database from MySQL to Postgres

  Copyright (C) 2014 Michael Van Canneyt <michael@freepascal.org>

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.
  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.
  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

program convertmantis;

{$linklib pthread}
{$mode objfpc}{$h+}

uses cthreads, sysutils, classes, db, sqldb, pqconnection, mysql55conn, inifiles;

{
Configure your source and target databases connection details in a .ini file
named convertmantis.ini.
Needs 2 sections: [Source] and [Target]
Fill in the keys as needed, they have the same names as the relevant properties
in the TSQLConnection component.

[Source]
HostName=
DatabaseName=
UserName=
Password=
CharSet=

[Target]
HostName=
DatabaseName=
UserName=
Password=
CharSet=
}

Const
  CommitAt = 1000; // Commit after this number of records have been processed.

Var
  DBSrc,
  DBTarget : TSQLConnection;
  ConvertedRecordCount : Int64;
  TotalTableCount,
  ConvertedTableCount : Integer;
  FTables : TStrings;

Function CreateQuery(DB : TSQLConnection; SQL : String) : TSQLQuery;

begin
  Result:=TSQLQuery.Create(DB);
  Result.Database:=DB;
  Result.Transaction:=DB.Transaction;
  Result.SQL.Text:=SQL;
end;

Function SrcQuery(SQL : String) : TSQLQuery;

begin
  Result:=CreateQuery(DBSrc,SQL);
end;

Function TargetQuery(SQL : String) : TSQLQuery;

begin
  Result:=CreateQuery(DBTarget,SQL);
end;

Procedure DoQueryLoop(QSrc,QTarget : TSQLQuery; DoID : Boolean = True);

Var
  DS : TDataSource;
  UnBind : Boolean;
  P,I,C : Integer;


begin
  DS:=Nil;
  try
    DS:=TDataSource.Create(Qsrc);
    DS.Dataset:=QSrc;
    QTarget.DataSource:=DS;
    QSrc.Open;
    C:=0;
    While Not QSrc.EOF do
      begin
      Inc(C);
      if not (QTarget.TRansaction as TSQLTransaction).Active then
        (QTarget.TRansaction as TSQLTransaction).StartTransaction;
      if DoID then // Workaround for autoInc params
        QTarget.ParamByName('id').AsInteger:=QSrc.FieldByName('id').AsInteger;
      // If we concatenated a null character, we need to unbind the parameters.
      if UnBind then
        begin
        UnBind:=False;
        For I:=0 to QTarget.Params.Count-1 do
          if (QTarget.Params[i].Bound) and (QTarget.Params[i].Name<>'id') then
            QTarget.Params[i].Bound:=False;
        end;
      For I:=0 to QSrc.Fields.Count-1 do
        if (QSrc.Fields[i].DataType = ftMemo) then
          begin
          P:=Pos(#0,QSrc.Fields[i].AsString);
          if (P<>0) then
            begin
            Unbind:=True; // Make sure we unbind in next iteration.
            Writeln('Null detected, Truncating field ',QSrc.Fields[i].FieldName,' to pos ',P);
            QTarget.ParamByName(QSrc.Fields[i].FieldName).AsString:=Copy(QSrc.Fields[i].AsString,1,P-1);
            Writeln('New value: ',QTarget.ParamByName(QSrc.Fields[i].FieldName).AsString);
            end;
          end;
      try
        QTarget.ExecSQL;
      except
        On E : Exception do
          begin
          Writeln('Error: ',E.Message);
          Writeln('Source record contents:');
          For I:=0 to QSrc.Fields.Count-1 do
            Writeln(QSrc.Fields[i].FieldName,' : ',QSrc.Fields[I].AsString);
          Writeln('-----------------------');
          Raise;
          end;
      end;
      QSrc.Next;
      if (C mod CommitAt) = 0 then
        begin
        Writeln('Committing at ',C,' records');
        (QTarget.TRansaction as TSQLTransaction).Commit;
        end;
      end;
    Inc(ConvertedRecordCount,C);
    Writeln('Committing at ',C,' records (total: ',ConvertedRecordCount,')');
    (QTarget.TRansaction as TSQLTransaction).Commit;
  finally
    QSrc.Close;
    DS.Free;
  end;
end;

Procedure ClearTable(Const ATableName: String);

begin
  Writeln('Truncating table: '+ATableName);
  // PostGres specific: faster than delete * from ...
  With TargetQuery('TRUNCATE '+ATableName) do
    try
      ExecSQL;
      (TRansaction as TSQLTransaction).Commit;
    finally
      Free;
    end;
end;

Procedure DoTable(Const ATableName,SQLSelect,SQLInsert : String; TreatID : Boolean; Clear : Boolean);

Var
  QSrc,QTarget : TSQLQuery;
  I : integer;

begin
  Inc(ConvertedTableCount);
  Writeln('Converting table ',ConvertedTableCount,' of ',TotalTableCount,' : ', ATableName, '. Clear table: ',Clear);
  I:=FTables.IndexOf(ATableName);
  if I<>-1 then
    FTables.Delete(i);
  QSrc:=Nil;
  QTarget:=Nil;
  try
    QSrc:=SrcQuery(SQLSelect);
    QTarget:=TargetQuery(SQLinsert);
    if Clear then
      ClearTable(ATableName);
    DoQueryLoop(QSrc,QTarget,TreatID);
  Finally
    QSrc.Free;
    QTarget.Free;
  end;
end;

Procedure ConvertUser(Clear : Boolean);

Const
  TableName = 'mantis_user_table';
  SQLInsert =
  'INSERT INTO mantis_user_table('+
  ' id, username, realname, email, password, enabled, protected,'+
  ' access_level, login_count, lost_password_request_count, failed_login_count,'+
  ' cookie_string, last_visit, date_created)'+
  '  VALUES (:id, :username, :realname, :email, :password, ((:enabled)::int)::boolean, ((:protected)::int)::boolean,'+
  ' :access_level, :login_count, :lost_password_request_count, :failed_login_count,'+
  ' :cookie_string, :last_visit, :date_created)';
  SQLSelect =
  'SELECT `id`, `username`, `realname`, `email`, `password`, `enabled`, `protected`, '+
  ' `access_level`, `login_count`, `lost_password_in_progress_count`, `failed_login_count`, '+
  ' `cookie_string`, `lost_password_request_count`, `last_visit`, `date_created` FROM `mantis_user_table`';

begin
  DoTable(TableName,SQLSelect,SQLInsert,True,Clear);
end;

Procedure ConvertUserProfile(Clear : Boolean);

Const
  TableName = 'mantis_user_profile_table';
  SQLInsert = 'INSERT INTO mantis_user_profile_table( '+
            ' id, user_id, platform, os, os_build, description) '+
            ' VALUES (:id, :user_id, :platform, :os, :os_build, :description)';
  SQLSelect = 'SELECT `id`, `user_id`, `platform`, `os`, `os_build`, `description` FROM `mantis_user_profile_table`';

begin
  DoTable(TableName,SQLSelect,SQLInsert,True,Clear);
end;

Procedure ConvertUserPrintPref(Clear : Boolean);

Const
  TableName = 'mantis_user_print_pref_table';
  SQLInsert = 'INSERT INTO mantis_user_print_pref_table(user_id, print_pref) '+
              'VALUES (:user_id, :print_pref)';
  SQLSelect = 'SELECT `user_id`, `print_pref` FROM `mantis_user_print_pref_table`';

begin
  DoTable(TableName,SQLSelect,SQLInsert,False,Clear);
end;

Procedure ConvertUserPref(Clear : Boolean);

Const
  TableName = 'mantis_user_pref_table';
  SQLInsert = 'INSERT INTO mantis_user_pref_table( '+
            ' id, user_id, project_id, default_profile, default_project, refresh_delay, '+
            ' redirect_delay, bugnote_order, email_on_new, email_on_assigned, '+
            ' email_on_feedback, email_on_resolved, email_on_closed, email_on_reopened, '+
            ' email_on_bugnote, email_on_status, email_on_priority, email_on_priority_min_severity, '+
            ' email_on_status_min_severity, email_on_bugnote_min_severity, '+
            ' email_on_reopened_min_severity, email_on_closed_min_severity, '+
            ' email_on_resolved_min_severity, email_on_feedback_min_severity, '+
            ' email_on_assigned_min_severity, email_on_new_min_severity, email_bugnote_limit, '+
            ' language, timezone) '+
            ' VALUES ( '+
            ' :id, :user_id, :project_id, :default_profile, :default_project, :refresh_delay, '+
            ' :redirect_delay, :bugnote_order, ((:email_on_new)::int)::boolean, ((:email_on_assigned)::int)::boolean, '+
            ' ((:email_on_feedback)::int)::boolean, ((:email_on_resolved)::int)::boolean, ((:email_on_closed)::int)::boolean, ((:email_on_reopened)::int)::boolean, '+
            ' ((:email_on_bugnote)::int)::boolean, ((:email_on_status)::int)::boolean, ((:email_on_priority)::int)::boolean, :email_on_priority_min_severity, '+
            ' :email_on_status_min_severity, :email_on_bugnote_min_severity, '+
            ' :email_on_reopened_min_severity, :email_on_closed_min_severity, '+
            ' :email_on_resolved_min_severity, :email_on_feedback_min_severity, '+
            ' :email_on_assigned_min_severity, :email_on_new_min_severity, :email_bugnote_limit, '+
            ' :language, :timezone)';
  SQLSelect = 'SELECT `id`, `user_id`, `project_id`, `default_profile`, `default_project`, '+
              ' `refresh_delay`, `redirect_delay`, `bugnote_order`, `email_on_new`, `email_on_assigned`, '+
              ' `email_on_feedback`, `email_on_resolved`, `email_on_closed`, `email_on_reopened`, '+
              ' `email_on_bugnote`, `email_on_status`, `email_on_priority`, `email_on_priority_minimum_severity`, '+
              ' `email_on_status_minimum_severity`, `email_on_bugnote_minimum_severity`, `email_on_reopened_minimum_severity`, '+
              ' `email_on_closed_minimum_severity`, `email_on_resolved_minimum_severity`, `email_on_feedback_minimum_severity`, '+
              ' `email_on_assigned_minimum_severity`, `email_on_new_minimum_severity`, `email_bugnote_limit`, `language`, '+
              ' `email_on_priority_min_severity`, `email_on_status_min_severity`, `email_on_bugnote_min_severity`,'+
              ' `email_on_reopened_min_severity`, `email_on_closed_min_severity`, `email_on_resolved_min_severity`, '+
              ' `email_on_feedback_min_severity`, `email_on_assigned_min_severity`, `email_on_new_min_severity`, `timezone` FROM `mantis_user_pref_table`';

begin
  DoTable(TableName,SQLSelect,SQLInsert,True,Clear);
end;

Procedure ConvertTokens(Clear : Boolean);

Const
  TableName = 'mantis_tokens_table';
  SQLInsert = 'INSERT INTO mantis_tokens_table('+
              ' id, owner, type, value, "timestamp", expiry)'+
              'VALUES (:id, :owner, :type, :value, :timestamp, :expiry)';
  SQLSelect = 'SELECT `id`, `owner`, `type`, `value`, `timestamp`, `expiry` FROM `mantis_tokens_table`';

begin
  DoTable(TableName,SQLSelect,SQLInsert,True,Clear);
end;

Procedure ConvertTag(Clear : Boolean);

Const
  TableName = 'mantis_tag_table';
  SQLInsert = 'INSERT INTO mantis_tag_table(id, user_id, name, description, date_created, date_updated)'+
              ' VALUES (:id, :user_id, :name, :description, :date_created, :date_updated)';
  SQLSelect = 'SELECT `id`, `user_id`, `name`, `description`, `date_created`, `date_updated` FROM `mantis_tag_table`';

begin
  DoTable(TableName,SQLSelect,SQLInsert,True,Clear);
end;

Procedure ConvertProjectVersion(Clear : Boolean);

Const
  TableName = 'mantis_project_version_table';
  SQLInsert = 'INSERT INTO mantis_project_version_table(id, project_id, version, description, released, obsolete, date_order) '+
              ' VALUES (:id, :project_id, :version, :description, ((:released)::int)::boolean, ((:obsolete)::int)::boolean, :date_order)';
  SQLSelect = 'SELECT `id`, `project_id`, `version`, `description`, `released`, `obsolete`, `date_order` FROM `mantis_project_version_table`';

begin
  DoTable(TableName,SQLSelect,SQLInsert,True,Clear);
end;

Procedure ConvertProjectUserList(Clear : Boolean);

Const
  TableName = 'mantis_project_user_list_table';
  SQLInsert = 'INSERT INTO mantis_project_user_list_table(project_id, user_id, access_level)'+
              'VALUES (:project_id, :user_id, :access_level)';
  SQLSelect = 'SELECT `project_id`, `user_id`, `access_level` FROM `mantis_project_user_list_table`';

begin
  DoTable(TableName,SQLSelect,SQLInsert,False,Clear);
end;

Procedure ConvertProjectFile(Clear : Boolean);

Const
  TableName = 'mantis_project_file_table';
  SQLInsert = 'INSERT INTO mantis_project_file_table('+
              'id, project_id, title, description, diskfile, filename, folder,'+
              'filesize, file_type, content, date_added, user_id)'+
              'VALUES (:id, :project_id, :title, :description, :diskfile, :filename, :folder,'+
              ':filesize, :file_type, :content, :date_added, :user_id)';
  SQLSelect = 'SELECT `id`, `project_id`, `title`, `description`, `diskfile`, `filename`, `folder`, `filesize`, `file_type`, `content`, `date_added`, `user_id` FROM `mantis_project_file_table` WHERE 1';

begin
  DoTable(TableName,SQLSelect,SQLInsert,True,Clear);
end;



Procedure ConvertProject(Clear : Boolean);

Const
  TableName = 'mantis_project_table';
  SQLInsert = 'INSERT INTO mantis_project_table '+
              '  (access_min, category_id, description, enabled, file_path, id, inherit_global, name, status, view_state) '+
              ' VALUES (:access_min, :category_id, :description, :enabled, :file_path, :id, :inherit_global, :name, :status, :view_state)';
  SQLSelect = 'SELECT `id`, `name`, `status`, `enabled`, `view_state`, `access_min`, `file_path`, `description`, `category_id`, `inherit_global` FROM `mantis_project_table`';

begin
  DoTable(TableName,SQLSelect,SQLInsert,True,Clear);
end;

Procedure ConvertProjectHierarchy(Clear : Boolean);

Const
  TableName = 'mantis_project_hierarchy_table';
  SQLInsert = 'INSERT INTO mantis_project_hierarchy_table (child_id, inherit_parent, parent_id) '+
              'VALUES  (:child_id, :inherit_parent, :parent_id)';
  SQLSelect = 'SELECT `child_id`, `parent_id`, `inherit_parent` FROM `mantis_project_hierarchy_table`';

begin
  DoTable(TableName,SQLSelect,SQLInsert,False,Clear);
end;
Procedure ConvertPlugin(Clear : Boolean);

Const
  TableName = 'mantis_plugin_table';
  SQLInsert = 'INSERT INTO mantis_plugin_table( basename, enabled, protected, priority) '+
              ' VALUES (:basename, ((:enabled)::int)::boolean, ((:protected)::int)::boolean, :priority)';
  SQLSelect = ' SELECT `basename`, `enabled`, `protected`, `priority` FROM `mantis_plugin_table`';

begin
  DoTable(TableName,SQLSelect,SQLInsert,False,Clear);
end;

Procedure ConvertNews(Clear : Boolean);

Const
  TableName = 'mantis_news_table';
  SQLInsert = 'INSERT INTO mantis_news_table(id, project_id, poster_id, view_state, announcement, headline,'+
              ' body, last_modified, date_posted)'+
              ' VALUES (:id, :project_id, :poster_id, :view_state, ((:announcement)::int)::boolean, :headline,'+
              ' :body, :last_modified, :date_posted)';
  SQLSelect = 'SELECT `id`, `project_id`, `poster_id`, `view_state`, `announcement`, `headline`, `body`, `last_modified`, `date_posted` FROM `mantis_news_table`';

begin
  DoTable(TableName,SQLSelect,SQLInsert,True,Clear);
end;

Procedure ConvertFilters(Clear : Boolean);

Const
  TableName = 'mantis_filters_table';
  SQLInsert = 'INSERT INTO mantis_filters_table(id, user_id, project_id, is_public, name, filter_string) '+
              '  VALUES (:id, :user_id, :project_id, ((:is_public)::int)::boolean, :name, :filter_string)';
  SQLSelect = 'SELECT `id`, `user_id`, `project_id`, `is_public`, `name`, `filter_string` FROM `mantis_filters_table`';

begin
  DoTable(TableName,SQLSelect,SQLInsert,True,Clear);
end;

Procedure ConvertCustomFields(Clear : Boolean);

Const
  TableName = 'mantis_custom_field_table';
  SQLInsert = 'INSERT INTO mantis_custom_field_table(id, name, type, possible_values, default_value, valid_regexp, '+
              ' access_level_r, access_level_rw, length_min, length_max, require_report, '+
              ' require_update, display_report, display_update, require_resolved, '+
              ' display_resolved, display_closed, require_closed, filter_by) '+
              ' VALUES (:id, :name, :type, :possible_values, :default_value, :valid_regexp, '+
              ' :access_level_r, :access_level_rw, :length_min, :length_max, ((:require_report)::int)::boolean, '+
              ' ((:require_update)::int)::boolean, ((:display_report)::int)::boolean, ((:display_update)::int)::boolean, ((:require_resolved)::int)::boolean, '+
              ' ((:display_resolved)::int)::boolean, ((:display_closed)::int)::boolean, ((:require_closed)::int)::boolean, ((:filter_by)::int)::boolean)';

  SQLSelect = 'SELECT `id`, `name`, `type`, `possible_values`, `default_value`, `valid_regexp`, `access_level_r`, `access_level_rw`, `length_min`, `length_max`, `require_report`, `require_update`, `display_report`, `display_update`, `require_resolved`, `display_resolved`, `display_closed`, `require_closed`, `filter_by` FROM `mantis_custom_field_table` ';

begin
  DoTable(TableName,SQLSelect,SQLInsert,True,Clear);
end;

Procedure ConvertCustomFieldString(Clear : Boolean);

Const
  TableName = 'mantis_custom_field_string_table';
  SQLInsert = 'INSERT INTO mantis_custom_field_string_table( field_id, bug_id, value)'+
              'VALUES (:field_id, :bug_id, :value);';
  SQLSelect = 'SELECT `field_id`, `bug_id`, `value` FROM `mantis_custom_field_string_table`';

begin
  DoTable(TableName,SQLSelect,SQLInsert,False,Clear);
end;

Procedure ConvertCustomFieldProject(Clear : Boolean);

Const
  TableName = 'mantis_custom_field_project_table';
  SQLInsert = 'INSERT INTO mantis_custom_field_project_table(field_id, project_id, sequence) '+
              ' VALUES (:field_id, :project_id, :sequence);';
  SQLSelect = 'SELECT `field_id`, `project_id`, `sequence` FROM `mantis_custom_field_project_table`';

begin
  DoTable(TableName,SQLSelect,SQLInsert,False,Clear);
end;

Procedure ConvertConfig(Clear : Boolean);

Const
  TableName = 'mantis_config_table';
  SQLInsert = 'INSERT INTO mantis_config_table(config_id, project_id, user_id, access_reqd, type, value)'+
              ' VALUES (:config_id, :project_id, :user_id, :access_reqd, :type, :value)';
  SQLSelect = 'SELECT `config_id`, `project_id`, `user_id`, `access_reqd`, `type`, `value` FROM `mantis_config_table`';

begin
  DoTable(TableName,SQLSelect,SQLInsert,False,Clear);
end;

Procedure ConvertCategory(Clear : Boolean);

Const
  TableName = 'mantis_category_table';
  SQLInsert = 'INSERT INTO mantis_category_table(id, project_id, user_id, name, status) '+
               '    VALUES (:id, :project_id, :user_id, :name, :status)';
  SQLSelect = 'SELECT `id`, `project_id`, `user_id`, `name`, `status` FROM `mantis_category_table` ';

begin
  DoTable(TableName,SQLSelect,SQLInsert,True,Clear);
end;

Procedure ConvertBugText(Clear : Boolean);

Const
  TableName = 'mantis_bug_text_table';
  SQLInsert = 'INSERT INTO mantis_bug_text_table(id, description, steps_to_reproduce, additional_information) '+
              ' VALUES (:id, :description, :steps_to_reproduce, :additional_information)';
  SQLSelect = 'SELECT `id`, `description`, `steps_to_reproduce`, `additional_information` FROM `mantis_bug_text_table` ';

begin
  DoTable(TableName,SQLSelect,SQLInsert,True,Clear);
end;

Procedure ConvertBugTag(Clear : Boolean);

Const
  TableName = 'mantis_bug_tag_table';
  SQLInsert = 'INSERT INTO mantis_bug_tag_table(bug_id, tag_id, user_id, date_attached) '+
              ' VALUES (:bug_id, :tag_id, :user_id, :date_attached)';
  SQLSelect = 'SELECT `bug_id`, `tag_id`, `user_id`, `date_attached` FROM `mantis_bug_tag_table`';

begin
  DoTable(TableName,SQLSelect,SQLInsert,False,Clear);
end;

Procedure ConvertBug(Clear : Boolean);

Const
  TableName = 'mantis_bug_table';
  SQLInsert = 'INSERT INTO mantis_bug_table( '+
              '  id, project_id, reporter_id, handler_id, duplicate_id, priority, '+
              '  severity, reproducibility, status, resolution, projection, eta, '+
              '  bug_text_id, os, os_build, platform, version, fixed_in_version, '+
              '  build, profile_id, view_state, summary, sponsorship_total, sticky, '+
              '  target_version, category_id, date_submitted, due_date, last_updated) '+
              ' VALUES (:id, :project_id, :reporter_id, :handler_id, :duplicate_id, :priority, '+
              '  :severity, :reproducibility, :status, :resolution, :projection, :eta, '+
              '  :bug_text_id, :os, :os_build, :platform, :version, :fixed_in_version, '+
              '  :build, :profile_id, :view_state, :summary, :sponsorship_total, ((:sticky)::int)::boolean, '+
              '  :target_version, :category_id, :date_submitted, :due_date, :last_updated) ';
  SQLSelect = 'SELECT `id`, `project_id`, `reporter_id`, `handler_id`, `duplicate_id`, `priority`, `severity`, `reproducibility`, `status`, `resolution`, `projection`, `eta`, `bug_text_id`, `os`, `os_build`, `platform`, `version`, `fixed_in_version`, `build`, `profile_id`, `view_state`, `summary`, `sponsorship_total`, `sticky`, `target_version`, `category_id`, `date_submitted`, `due_date`, `last_updated` FROM `mantis_bug_table`';

begin
  DoTable(TableName,SQLSelect,SQLInsert,True,Clear);
end;

Procedure ConvertBugRevision(Clear : Boolean);

Const
  TableName = 'mantis_bug_revision_table';
  SQLInsert = 'INSERT INTO mantis_bug_revision_table(id, bug_id, bugnote_id, user_id, type, value, "timestamp") '+
              'VALUES (:id, :bug_id, :bugnote_id, :user_id, :type, :value, :timestamp)';
  SQLSelect = 'SELECT `id`, `bug_id`, `bugnote_id`, `user_id`, `type`, `value`, `timestamp` FROM `mantis_bug_revision_table`';

begin
  DoTable(TableName,SQLSelect,SQLInsert,True,Clear);
end;

Procedure ConvertBugRelationship(Clear : Boolean);

Const
  TableName = 'mantis_bug_relationship_table';
  SQLInsert = 'INSERT INTO mantis_bug_relationship_table(id, source_bug_id, destination_bug_id, relationship_type) '+
              ' VALUES (:id, :source_bug_id, :destination_bug_id, :relationship_type)';
  SQLSelect = 'SELECT `id`, `source_bug_id`, `destination_bug_id`, `relationship_type` FROM `mantis_bug_relationship_table` ';

begin
  DoTable(TableName,SQLSelect,SQLInsert,True,Clear);
end;

Procedure ConvertBugMonitor(Clear : Boolean);
Const
  TableName = 'mantis_bug_monitor_table';
  SQLInsert = 'INSERT INTO mantis_bug_monitor_table(user_id, bug_id) '+
              ' VALUES (:user_id, :bug_id)';
  SQLSelect = 'SELECT `user_id`, `bug_id` FROM `mantis_bug_monitor_table`';

begin
  DoTable(TableName,SQLSelect,SQLInsert,False,Clear);
end;

Procedure ConvertBugHistory(Clear : Boolean);
Const
  TableName = 'mantis_bug_history_table';
  SQLInsert = 'INSERT INTO mantis_bug_history_table(id, user_id, bug_id, field_name, old_value, new_value, type, date_modified) '+
              ' VALUES (:id, :user_id, :bug_id, :field_name, :old_value, :new_value, :type, :date_modified)';
  SQLSelect = 'SELECT `id`, `user_id`, `bug_id`, `field_name`, `old_value`, `new_value`, `type`, `date_modified` FROM `mantis_bug_history_table` ';

begin
  DoTable(TableName,SQLSelect,SQLInsert,True,Clear);
end;

Procedure ConvertBugFile(Clear : Boolean);

Const
  TableName = 'mantis_bug_file_table';
  SQLInsert = 'INSERT INTO mantis_bug_file_table(id, bug_id, title, description, diskfile, filename, folder, filesize, file_type, content, date_added, user_id)'+
              '  VALUES (:id, :bug_id, :title, :description, :diskfile, :filename, :folder, :filesize, :file_type, :content, :date_added, :user_id)';
  SQLSelect = 'SELECT `id`, `bug_id`, `title`, `description`, `diskfile`, `filename`, `folder`, `filesize`, `file_type`, `content`, `date_added`, `user_id` FROM `mantis_bug_file_table`';

begin
  DoTable(TableName,SQLSelect,SQLInsert,True,Clear);
end;

Procedure ConvertBugNoteText(Clear : Boolean);

Const
  TableName = 'mantis_bugnote_text_table';
  SQLInsert = 'INSERT INTO mantis_bugnote_text_table(id, note) VALUES (:id, :note)';
  SQLSelect = 'SELECT `id`, `note` FROM `mantis_bugnote_text_table` ';

begin
  DoTable(TableName,SQLSelect,SQLInsert,True,Clear);
end;

Procedure ConvertEmail(Clear : Boolean);

Const
  TableName = 'mantis_email_table';
  SQLInsert = 'INSERT INTO mantis_email_table(email_id, email, subject, metadata, body, submitted) '+
              ' VALUES (:email_id, :email, :subject, :metadata, :body, :submitted)';
  SQLSelect = 'SELECT `email_id`, `email`, `subject`, `metadata`, `body`, `submitted` FROM `mantis_email_table`';

begin
  DoTable(TableName,SQLSelect,SQLInsert,False,Clear);
end;


Procedure ConvertBugNote(Clear : Boolean);

Const
  TableName = 'mantis_bugnote_table';
  SQLInsert = 'INSERT INTO mantis_bugnote_table(id, bug_id, reporter_id, bugnote_text_id, view_state, note_type, '+
              '  note_attr, time_tracking, last_modified, date_submitted) '+
              ' VALUES (:id, :bug_id, :reporter_id, :bugnote_text_id, :view_state, :note_type, '+
              '        :note_attr, :time_tracking, :last_modified, :date_submitted) ';
  SQLSelect = 'SELECT `id`, `bug_id`, `reporter_id`, `bugnote_text_id`, `view_state`, `note_type`, `note_attr`, `time_tracking`, `last_modified`, `date_submitted` FROM `mantis_bugnote_table`';

begin
  DoTable(TableName,SQLSelect,SQLInsert,True,Clear);
end;

Procedure ConvertSponsorShip(Clear : Boolean);

Const
  TableName = 'mantis_sponsorship_table';
  SQLInsert = 'INSERT INTO mantis_sponsorship_table(id, bug_id, user_id, amount, logo, url, paid, date_submitted,last_updated) '+
              'VALUES (:id, :bug_id, :user_id, :amount, :logo, :url, ((:paid)::int)::boolean, :date_submitted, :last_updated) ';
  SQLSelect = 'SELECT `id`, `bug_id`, `user_id`, `amount`, `logo`, `url`, `paid`, `date_submitted`, `last_updated` FROM `mantis_sponsorship_table`';

begin
  DoTable(TableName,SQLSelect,SQLInsert,True,Clear);
end;

Procedure ConvertMantis;

  Procedure Config(DB : TSQLConnection; Ini : TCustomIniFile; ASection : String);

  begin
    With Ini,DB do
      begin
      Name:=ASection;
      HostName:=ReadString(ASection,'Hostname',HostName);
      DatabaseName:=ReadString(ASection,'DatabaseName',DatabaseName);
      UserName:=ReadString(ASection,'UserName',UserName);
      Password:=ReadString(ASection,'Password',Password);
      CharSet:=ReadString(ASection,'CharSet',CharSet);
      end;
  end;
Var
  ini : TMemIniFile;


begin
  DBSrc:=Nil;
  DBTarget:=Nil;
  try
    ConvertedRecordCount:=0;
    FTables:=TStringList.Create;
    DBSrc:=TMySQL55Connection.Create(Nil);
    DBSrc.Transaction:=TSQLTransaction.Create(DBSrc);
    DBTarget:=TPQConnection.Create(Nil);
    DBTarget.Transaction:=TSQLTransaction.Create(DBTarget);
    Ini:=TMemIniFile.Create('convertmantis.ini');
    try
      Config(DBSrc,Ini,'Source');
      Config(DBTarget,Ini,'Target');
    finally
      Ini.Free;
    end;
    DBSrc.Connected:=True;
    DBTarget.Connected:=True;
// Get table names for statistics.
    DBSrc.GetTableNames(FTables);
    TotalTableCount:=FTables.Count;
    ConvertedTableCount:=0;
    // Start
    ConvertUser(True);
    ConvertUserProfile(True);
    ConvertUserPrintPref(True);
    ConvertUserPref(True);
    ConvertTokens(True);
    ConvertTag(True);
    ConvertProjectVersion(True);
    ConvertProjectUserList(True);
    ConvertProject(True);
    ConvertProjectHierarchy(True);
    ConvertProjectFile(True);
    ConvertSponsorShip(True);
    ConvertPlugin(True);
    ConvertNews(True);
    ConvertFilters(True);
    ConvertCustomFields(True);
    ConvertCustomFieldString(True);
    ConvertCustomFieldProject(True);
    ConvertConfig(True);
    ConvertCategory(True);
    ConvertBugText(True);
    ConvertBugTag(True);
    ConvertBug(True);
    ConvertBugRevision(True);
    ConvertBugRelationShip(True);
    ConvertBugMonitor(True);
    ConvertBugHistory(True);
    ConvertBugFile(True);
    ConvertBugNoteText(True);
    ConvertBugNote(True);
    ConvertEmail(True);
    if FTables.Count>0 then
      begin
      Writeln('Not converted tables from source DB:');
      Writeln(FTables.Text);
      end;
  finally
    DBTarget.Free;
    DBSrc.Free;
  end;
end;

Var
  S : TDateTime;

begin
  S:=Now;
  ConvertMantis;
  Writeln('Converted ',ConvertedTableCount,' tables.');
  Writeln('Converted ',ConvertedRecordCount,' records.');
  Writeln('Elapsed time: ',FormatDateTime('hh:nn:ss',Now-S));
end.

