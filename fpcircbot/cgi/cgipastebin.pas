program cgiPastebin;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, PQConnection, SqlDB,
  PWU, PWUEnvVar;
  
procedure Main;

const
  {$Warning Don't forget to make hiddeninc.inc file!}
  {$i hiddeninc.inc}

var
  LogFBConnection : TPQConnection;
  LogTransaction  : TSQLTransaction;
  LogQuery        : TSQLQuery;
  WriteQuery      : TSQLQuery;
  ChanQuery       : TSQLQuery;
  Channel, Sender : string;
  Title, PasteID  : string;
  pText, HL       : string;
  i               : Integer;
  HTMLCode        : TStringList;
  ChanList        : TStringList;
  GetList         : TStringList;
  
procedure InitDB;
begin
  LogFBConnection := TPQConnection.Create(nil);
  with LogFBConnection do begin
    HostName := 'localhost';
    DatabaseName := 'fpcbot';
    UserName := CgiDBName;
    Password := CgiDBPass;
  end;

  LogTransaction := tsqltransaction.create(nil);
  LogTransaction.database := LogFBConnection;

  LogQuery := tsqlquery.Create(nil);
  with LogQuery do begin
    ReadOnly:=True;
    DataBase := LogFBConnection;
    transaction := LogTransaction;
  end;
  
  WriteQuery := tsqlquery.Create(nil);
  with WriteQuery do begin
    DataBase := LogFBConnection;
    transaction := LogTransaction;

    sql.clear;
    sql.add('insert into tbl_pastes(pasteid, title, sender, pastetext,highlight) ' +
            'values (gen_id(GEN_PASTEID,1),:title,:sender,:ptext,:hl)');
    prepare;
  end;

  ChanQuery := tsqlquery.Create(nil);
  with ChanQuery do begin
    DataBase := LogFBConnection;
    transaction := LogTransaction;
    ReadOnly:=True;
  end;
end;

procedure FreeDB;
begin
  LogFBConnection.Close;
  LogFBConnection.free;
  LogTransaction.free;
  LogQuery.free;
  WriteQuery.Free;
  ChanQuery.Free;
end;

procedure InitCommon;
begin
  Channel:='';
  HTMLCode:=TStringList.Create;
  ChanList:=TStringList.Create;
  GetList:=TStringList.Create;
  GetList.CommaText:=StringReplace(GetEnvironmentVariable('QUERY_STRING'), '&', ',', [rfReplaceAll]);
end;

procedure Init;
begin
  InitCommon;
  InitDB;
end;

procedure FreeCommon;
begin
  HTMLCode.Free;
  ChanList.Free;
  GetList.Free;
end;

procedure Free;
begin
  FreeCommon;
  FreeDB;
end;

procedure GetWebVars;

  procedure TryGetWebVar(var LocalVar: string; const VarName, DefValue: string);

    function GetValue: string;
    var
      m, n, i: Integer;
      From: string;
    begin
      Result:='';
      for i:=0 to GetList.Count-1 do begin
        From:=GetList[i];
        n:=Pos(VarName, From);
        m:=Pos('=', From);
        if (m > 0) and (Length(From) > m)
        and (n > 0) then begin
          Result:=Copy(From, m+1, Length(From));
          Exit;
        end;
      end;
    end;

  begin
    if (GetList.Count > 0) and ((Length(GetValue)) > 0) then begin
      LocalVar:=GetValue;
      if VarName = 'channel' then LocalVar:='#' + LocalVar;
      SetWebVar(VarName, LocalVar);
    end else if Length(GetWebVar(VarName)) > 0 then try
      LocalVar:=GetWebVar(VarName);
      if Length(LocalVar) = 0 then LocalVar:=DefValue;
      if VarName = 'channel' then LocalVar:='#' + LocalVar;
    except
      LocalVar:=DefValue;
    end else begin
      SetWebVar(VarName, DefValue);
      LocalVar:=DefValue;
    end;
  end;

var
  DefChan: string;
begin
  DefChan:='';
  if ChanList.Count > 0 then DefChan:='#' + ChanList[0];

  TryGetWebVar(Channel, 'channel', DefChan);
//  Channel := return_Channel_sanitize(Channel); //Make sure to have only allowed chars ... :)

  TryGetWebVar(Title, 'title', '');
//  Title := return_Nick_sanitize (Title); //Making sure that only RFC allowed chars exists

  TryGetWebVar(Sender, 'sender', '');
//  Sender := return_Nick_sanitize (Sender); //Making sure that only RFC allowed chars exists
  
  TryGetWebVar(PasteID, 'pasteid', '0');
//  PasteID:=return_Decimal_sanitize(PasteID);

  TryGetWebVar(pText, 'ptext', '');
  pText:=FilterHTML(pText);
end;

procedure FillChannels;
begin
  with ChanQuery do try
    SQL.Clear;
    SQL.Add('select channelname from tbl_channels');
    open;
    ChanList.Add('----');
    while not eof do begin
      ChanList.Add(FieldByName('channelname').AsString);
      Next;
    end;
    close;
  except on e: Exception do
    Writeln('SQL Error: ', FilterHTML(e.message));
  end;
end;

function GetHTMLChannels: string;
var
  s: string;
  i: Integer;
begin
  Result:='';
  if ChanList.Count > 0 then
    for i:=0 to ChanList.Count-1 do begin
      s:=ChanList[i];
      if  (Length(s) > 0)
      and (s[1] = '#') then System.Delete(s, 1, 1); // Delete #
      Result:=Result + '<option value="' + s + '"';
      if '#' + s <> Channel then
        Result:=Result + '>'
      else
        Result:=Result + ' selected>';
      if s <> '----' then
        Result:=Result + '#' + s + '</option>'
      else
        Result:=Result + s + '</option>';
    end;
end;


var
  Started: Boolean;
begin
  HL:='0';
  Init;

//  Web_Header;
  FillChannels;
  GetWebVars;

  HTMLCode.LoadFromFile('html' + PathDelim + 'pastebin1.html');
  HTMLCode.Text:=StringReplace(HTMLCode.Text, '$channels', GetHTMLChannels, [rfReplaceAll]);
  
  WebWriteln(HTMLCode.Text);

  if Channel = '#' then
    if ChanList.Count > 0 then Channel:=ChanList[0];
  HTMLCode.Clear;
  
  WebTemplateOut('html' + PathDelim + 'pastebin2.html', False);
  if PasteID <> '0' then begin
    with LogQuery do try
      Sql.Clear;
      Writeln('select first 1 pastetext from tbl_pastes where pasteid=''' + PasteID + '''');
      Sql.Add('select first 1 pastetext from tbl_pastes where pasteid=''' + PasteID + '''');
      Open;
      if not Eof then begin
        pText:=FieldByName('pastetext').AsString;
        Writeln('Text = "', pText, '"');
      end;
      Close;
    except on e: Exception do
      Writeln(e.message);
    end;
  end else if pText <> '' then begin
    with WriteQuery, WriteQuery.Params do try
      ParamByName('title').AsString:=Title;
      ParamByName('sender').AsString:=Sender;
      ParamByName('ptext').AsString:=pText;
      ParamByName('hl').AsString:=HL;
      ExecSQL;
      LogTransaction.CommitRetaining;
    except on e: Exception do
      Writeln(e.message);
    end;
    
    with LogQuery do try
      Sql.Clear;
      Sql.Add('select first 1 pasteid from tbl_pastes order by pasteid desc');
      Open;
      if not Eof then
        PasteID:=FieldByName('pasteid').AsString;
      Close;
    except on e: Exception do
      Writeln(e.message);
    end;
    SetWebVar('pasteid', PasteID);
  end;
  
  Writeln('<input type="hidden" name="pasteid" value="', PasteID, '">');
  Writeln('<textarea name="ptext" rows="30" cols="100">', pText, '</textarea>');
  Writeln(PasteID);
  writeln('</form></body></html>');
//  Free;
end;

begin
  Main;
end.

