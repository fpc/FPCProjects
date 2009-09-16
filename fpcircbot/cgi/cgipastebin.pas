program cgiPasteBin;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Math,
  SqlDB, PQConnection,
  PWU, PWUEnvVar,
  lNet, StringUtils;
//  PasHiliter;
  
type
  TDoer = class
   public
    procedure OnError(const msg: string; aSocket: TLSocket);
  end;
  
procedure TDoer.OnError(const msg: string; aSocket: TLSocket);
begin
  WebWriteLn(msg);
end;

procedure Main;

const
  {$Warning Don't forget to make hiddeninc.inc file!}
  {$i hiddeninc.inc}
  { highlighters }
  HL_NONE     = 0;
  HL_PAS      = 1;
  HL_LAST     = 2;
  
var
  PasteConnection   : TPQConnection;
  PasteTransaction  : TSQLTransaction;
  PasteQuery        : TSQLQuery;
  ChanQuery         : TSQLQuery;
  ChanList          : TStringList;

  procedure InitDB;
  begin
    PasteConnection := TPQConnection.Create(nil);
    with PasteConnection do begin
      HostName := 'localhost';
      DatabaseName := 'fpcbot';
      UserName := CgiDBName;
      Password := CgiDBPass;
    end;

    PasteTransaction := TSQLTransaction.create(nil);
    PasteTransaction.Database := PasteConnection;

    PasteQuery := TSQLQuery.Create(nil);
    with PasteQuery do begin
      DataBase := PasteConnection;
      Transaction := PasteTransaction;
      ReadOnly := True;
    end;

    ChanQuery := tsqlquery.Create(nil);
    with ChanQuery do begin
      DataBase := PasteConnection;
      transaction := PasteTransaction;
      ReadOnly := True;
    end;
  end;

  procedure FreeDB;
  begin
    PasteConnection.Close;
    PasteConnection.free;
    PasteTransaction.free;
    PasteQuery.free;
  end;

  procedure InitCommon;
  begin
    ChanList := TStringList.Create;
  end;

  procedure Init;
  begin
    Randomize;
    ShortDateFormat := 'dd"-"mmm"-"yy';
    InitCommon;
    InitDB;
  end;

  procedure FreeCommon;
  begin
    ChanList.Free;
  end;

  procedure Free;
  begin
    FreeCommon;
    FreeDB;
  end;

  procedure FillChannels;
  begin
    with ChanQuery do try
      SQL.Clear;
      SQL.Add('select channelname from tbl_channels');
      open;
      while not eof do begin
        ChanList.Add(FieldByName('channelname').AsString);
        Next;
      end;
      close;
    except on e: Exception do
      WebWriteln('<h1>Unable to fetch channel list: ' + e.Message + '</h1>');
    end;
  end;

  function CheckOut(const Str: string): string;
  begin
    if LowerCase(str) = 'on' then
      Result := 'checked'
    else
      Result := '';
  end;

  function GetHTMLChannels: string;
  var
    s: string;
    i: Integer;
  begin
    Result := '';
    if ChanList.Count > 0 then
      for i := 0 to ChanList.Count-1 do begin
        s := ChanList[i];
        if Length(s) > 0 then System.Delete(s, 1, 1); // Delete #
        Result := Result + '<option value="' + s + '"';
        if s = GetCGIVar('channel') then
          Result := Result + ' selected>'
        else
          Result := Result + '>';
        Result := Result + '#' + s + '</option>';
      end;
  //  Result := FilterHtml(Result);
  end;

  function GetMsgID: Integer;
  begin
    Result := -1;
    try
      Result := StrToInt(GetCGIVar('msgid'));
    except
      Result := -1;
    end;
  end;
  
  function GetListID: Integer;
  begin
    Result := -1;
    try
      Result := StrToInt(GetCGIVar('start'));
    except
      Result := -1;
    end;
  end;

  function CleanVar(const VarName: string; const CleanFully: Boolean = True): string;
  begin
    if CleanFully then
      Result := SQLEscape(FilterHTML(GetCGIVar(VarName)))
    else
      Result := SQLEscape(FilterHTMLWeak(GetCGIVar_S(VarName, 0)));
  end;
  
  function ParsePaste(const aPaste: string): string;
  
    function RemoveSlashAt: string;
    begin
      if Length(aPaste) > 2 then begin
        Result := aPaste;
        if Pos('\@', Result) = 1 then // if it's on 1st line
          Result := Copy(Result, 3, Length(Result));
        Result := StringReplace(Result, #13#10'\@', #13#10, [rfReplaceAll]); // for subsequents
      end;
    end;
    
    function WordWrap(s: string): string;
    const
      SEARCH_STRING = '&nbsp;&nbsp;';
    var
      i, n: Integer;
      Spaced: Boolean;
    begin
      Spaced := False;
      i := 0;
      Result := '';
      if Length(s) <= 80 then
        Result := s
      else repeat
        n := Pos(SEARCH_STRING, s);
        if (n > 0) and (n + i <= 80) then begin // copy space to space until space behind 80
          Result := Result + Copy(s, 1, n + Length(SEARCH_STRING) - 1);
          Delete(s, 1, n + Length(SEARCH_STRING) - 1);
          Inc(i, n + Length(SEARCH_STRING) - 1);
          Spaced := False;
        end else if (n + i > 80) and not Spaced then begin // if space behind 80, add newline and reset counter
          Result := Result + #13#10;
          i := 0;
          Spaced := True;
        end else begin // no spaces in part, copy 80, reset counter
          Result := Result + Copy(s, 1, 80) + #13#10;
          Delete(s, 1, 80);
          i := 0;
          Spaced := False;
        end;
      until Length(s) = 0;
    end;
  
  var
    i: Integer;
    List: TStringList;
    s: string;
  begin
    List := TStringList.Create;
    List.Text := aPaste;

{    if GetWebVar('highlight') = 'Pascal' then
      List.Text := PasStrToHtmStr(RestoreHTMLWeak(aPaste));}

    if List.Count > 0 then begin

      Result := #13#10'<br>'#13#10'<a href="cgipastebin">Add new paste</a>'#13#10 +
              '<table summary="paste text" class="paste" width="80%">'#13#10;
      for i := 0 to List.Count-1 do begin
        s := StringReplace(List[i], ' ', '&nbsp;&nbsp;', [rfReplaceAll]);
        if (Length(s) > 2) and (s[1] = '\') and (s[2] = '@') then begin
          s := '<span class="selected">' + Copy(s, 3, Length(s)) + '</span>';
          Result := Result + '<tr><td width="3%"><span class="selected">' +
                           IntToStr(i + 1) + '. </span></td>'#13#10;
        end else
          Result := Result + '<tr><td width="3%">' + IntToStr(i + 1) + '. </td>'#13#10;
        Result := Result + '<td>' + WordWrap(s) + '</td></tr>'#13#10;
      end;
      Result := Result + #13#10'</table>'#13#10;
      // "paste textarea"
      Result := Result + '<textarea cols="80" rows="' +
                       IntToStr(Min(List.Count, 25)) + '">' +
              RemoveSlashAt + '</textarea>';
    end else
      Result := '';
    List.Free;
  end;
 
  procedure SetMath(out ms, mr: string);
  var
    o1, o2: Integer;
  begin
    o1 := Random(21);
    o2 := Random(21);
    case Random(4) of
      0: begin
	   mr := IntToStr(o1 + o2);
           ms := IntToStr(o1) + ' + ' + IntToStr(o2);
	 end;
      1: begin
	   mr := IntToStr(o1 - o2);
           ms := IntToStr(o1) + ' - ' + IntToStr(o2);
	 end;
      2: begin
	   mr := IntToStr(o1 * o2);
           ms := IntToStr(o1) + ' times ' + IntToStr(o2);
	 end;
      3: begin
	   mr := IntToStr(o1 mod o2);
           ms := IntToStr(o1) + ' mod ' + IntToStr(o2);
	 end;
    end;
  end;

  procedure SetPasteVars;
  var
    mr: string = '';
    ms: string = '';
  begin
    SetWebVar('sender', FilterHTML(GetCGIVar('sender')));
    SetWebVar('title', FilterHTML(GetCGIVar('title')));
    SetWebVar('ancheck', CheckOut(GetCGIVar('ancheck')));

    SetMath(ms, mr);

    SetWebVar('ms', ms);
    SetWebVar('mr', mr);
  end;

  function SetViewVars(const MsgID: Integer): string;
  const
    StrAr: array[HL_NONE..HL_LAST] of string = ('None', 'Pascal', 'Last');
  begin
    Result := '';
    with PasteQuery do try
      PasteTransaction.EndTransaction;

      SQL.Clear;
      SQL.Add('select title, sender, pastetext, highlight from tbl_pastes where pasteid = ' + IntToStr(MsgID));

      Open;
      SetWebVar('title', FieldByName('title').AsString);
      SetWebVar('sender', FieldByName('sender').AsString);
      SetWebVar('highlight', StrAr[FieldByName('highlight').AsInteger]);
      Result := ParsePaste(FieldByName('pastetext').AsString);
      Close;
    except on e: Exception do
      begin
        WebWriteln(e.Message);
        PasteTransaction.EndTransaction;
      end;
    end;
  end;
  
  procedure SetStartID;
  begin
    SetWebVar('startid', '0');
  end;

  procedure DoPaste;
  var
    s: string;
    UDP: TLUdp;
    Doer: TDoer;
  begin
    SetPasteVars;
    WebTemplateOut('html' + PathDelim + 'pastebin1.html', False);
    WebWriteln(GetHTMLChannels);
    WebTemplateOut('html' + PathDelim + 'pastebin2.html', False);
    
    s := CleanVar('pastetext', False);

    if  (Length(s) > 0)
    and (Length(CleanVar('sender')) > 0)
    and (Length(CleanVar('title')) <= 100) then begin
      with PasteQuery do try
        PasteTransaction.EndTransaction;
        PasteTransaction.StartTransaction;
        SQL.Clear;
        SQL.Add('insert into tbl_pastes(title, sender, pastetext, highlight) values(' +
                '''' + CleanVar('title') + ''', ''' + CleanVar('sender') + ''', ''' +
                s + ''', ''' + CleanVar('highlight') + ''')');
        ExecSQL;
        PasteTransaction.Commit;

        SQL.Clear;
        SQL.Add('select pasteid from tbl_pastes order by pasteid desc limit 1');
        Open;
        s := CGIURL + 'cgipastebin?msgid=' + FieldByName('pasteid').AsString;
        Close;

        WebWriteln('You can view your paste at: <a href="' + s + '">' + s + '</a>');

        if GetCGIVar('ancheck') = 'on' then begin
          Doer := TDoer.Create;
          UDP := TLUdp.Create(nil);
          UDP.OnError := @Doer.OnError;
          UDP.Connect('localhost', PastePort);
          UDP.SendMessage('#' + GetCGIVar('channel') + '~' + GetCGIVar('sender') +
                          ' pasted "' + GetCGIVar('title') + '" at: ' + s);
          UDP.Free;
          Doer.Free;
        end;
      except on e: Exception do
        begin
          WebWriteln(e.Message);
          PasteTransaction.EndTransaction;
        end;
      end;
    end else begin
      WebWriteln('No text to paste or no sender or title too long (max 100)<br>'#13#10);
      WebWriteln('<textarea name="pastetext" rows="15" cols="80">'#13#10 +
                 GetCGIVar_S('pastetext', 0) + #13#10'</textarea><br>'#13#10);
      Webwriteln('<input type="submit" value="Paste">'#13#10);
    end;

    WebWriteln('  </center>');
    WebWriteLn('</form>');
  end;

  procedure TryPaste;
  begin
    if Trim(GetCGIVar('mr')) = Trim(GetCGIVar('mathcheck')) then
      DoPaste
    else
      WebWriteln('Wrong answer to person check question!');
  end;

  procedure DoView(const MsgID: Integer);
  var
    PT: string;
  begin
    PT := SetViewVars(MsgID);
    if Length(PT) > 0 then begin
      SetStartID;
      WebTemplateOut('html' + PathDelim + 'viewpaste.html', False);
      WebWriteln(PT);
    end else
      WebWriteln('No such paste');
    WebWriteln('<br><a href="cgipastebin">Add new paste</a>');
    WebWriteln('  </center>');
  end;
  
  procedure DoList(StartID: Integer);
  const
    MaxPerPage = 20;
  var
    URL, Title, Time, Sender: string;
    i: Integer;
  begin
    WebFileOut('html' + PathDelim + 'viewall.html');
    WebWriteln('<table class="header_style" width="80%">');
    
    if StartID < 0 then with PasteQuery do try
      SQL.Clear;
      SQL.Add('select pasteid from tbl_pastes order by pasteid desc limit 1');

      Open;

      StartID := FieldByName('pasteid').AsInteger;
      StartID := Max(StartID - MaxPerPage, 0);

      Close;
    except
      StartID := 0;
    end;

    with PasteQuery do try
      SQL.Clear;
      SQL.Add('select pasteid, title, sender, pastetime from tbl_pastes ' +
              ' where pasteid > ''' + IntToStr(StartID) + ''' order by pastetime limit ' + IntToStr(MaxPerPage));
      
      Open;
      
      i := -1;
      while not Eof do begin
        Title := FieldByName('title').AsString;
        if Length(Trim(Title)) = 0 then
          Title := 'no title';
        Sender := FieldByName('sender').AsString;
        Time := FieldByName('pastetime').AsString;
        URL := CGIURL + 'cgipastebin?msgid=' + FieldByName('pasteid').AsString;
        
        WebWrite('<tr><td width="20%">' + Time + '</td><td width="10%">' + Sender + '</td><td>');
        
        WebWriteln('<a href="' + URL + '">' + Title + '</a></td></tr>');
        
        Next;
        if i = -MaxPerPage then
          i := FieldByName('pasteid').AsInteger
        else
          Dec(i);
      end;
      
      Close;
      
    except on e: Exception do
      begin
        WebWriteln(e.Message);
        PasteTransaction.EndTransaction;
      end;
    end;
    WebWriteln('</table>');
    
    if StartID > 0 then
      WebWriteln('<a href="' + CGIURL + 'cgipastebin?viewall=yes&start=' + IntToStr(Max(StartID - MaxPerPage, 0)) + '">PREV</a>');
    if i >= 0 then
      WebWriteln('<a href="' + CGIURL + 'cgipastebin?viewall=yes&start=' + IntToStr(i) + '">NEXT</a>');
  end;

begin
  Init;
  
  FillChannels;

  if GetCGIVar('dopaste') = 'yes' then
    TryPaste
  else if GetCGIVar('viewall') = 'yes' then
    DoList(GetListID)
  else if GetMsgID >= 0 then
    DoView(GetMsgID)
  else begin
    SetPasteVars;
    WebTemplateOut('html' + PathDelim + 'pastebin1.html', False);
    WebWriteln(GetHTMLChannels);
    WebTemplateOut('html' + PathDelim + 'pastebin2.html', False);

    WebWriteln('<textarea name="pastetext" rows="15" cols="80">' +
               GetCGIVar_S('pastetext', 0) + '</textarea><br>');
    Webwriteln('<input type="submit" value="Paste">');
    WebWriteln('  </center>');
    WebWriteLn('</form>');
  end;
  
  WebWriteln('<p><a href="http://validator.w3.org/check?uri=referer"><img ' +
          'style="border:0;width:88px;height:31px" ' +
          'src="http://www.w3.org/Icons/valid-html401" alt="Valid HTML 4.01 ' +
          'Transitional" height="31" width="88"></a>');
  WebWriteln('<a href="http://jigsaw.w3.org/css-validator/"> ' +
          '<img style="border:0;width:88px;height:31px" ' +
          'src="http://jigsaw.w3.org/css-validator/images/vcss" ' +
          'alt="Valid CSS!"></a></p>');

  WebWriteLn('</body>');
  WebWriteLn('</html>');

  Free;
end;

begin
  Main;
end.

