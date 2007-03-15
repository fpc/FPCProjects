program cgiFpcBot;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils,
  SqlDB, PQConnection,
  PWU, PWUEnvVar, StringUtils;
  
procedure Main;

const
  {$Warning Don't forget to make hiddeninc.inc file!}
  {$i hiddeninc.inc}
  ColorAr: array[Boolean] of string = ('#FFFFFF', '#E0E0E0');
  HTML_GRAY   = '#cccccc';
  HTML_RED    = '#9d1010';
  HTML_ORANGE = '#ad10ad';
  HTML_GREEN  = '#109d10';
  HTML_BLUE   = '#10109d';
  
var
  LogConnection   : TPQConnection;
  LogTransaction  : TSQLTransaction;
  LogQuery        : TSQLQuery;
  ChanQuery       : TSQLQuery;
  Channel, Sender : string;
  Count, Msg      : string;
  FromDate, ToDate: string;
  FromTime, ToTime: string;
  ChanList        : TStringList;
  LineList        : TStringList;

procedure InitDB;
begin
  LogConnection := TPQConnection.Create(nil);
  with LogConnection do begin
    HostName := 'localhost';
    DatabaseName := 'fpcbot';
    UserName := CgiDBName;
    Password := CgiDBPass;
  end;

  LogTransaction := tsqltransaction.create(nil);
  LogTransaction.database := LogConnection;

  LogQuery := tsqlquery.Create(nil);
  with LogQuery do begin
    DataBase := LogConnection;
    transaction := LogTransaction;
    ReadOnly := True;
  end;

  ChanQuery := tsqlquery.Create(nil);
  with ChanQuery do begin
    DataBase := LogConnection;
    transaction := LogTransaction;
    ReadOnly := True;
  end;
end;

procedure FreeDB;
begin
  LogConnection.Close;
  LogConnection.free;
  LogTransaction.free;
  LogQuery.free;
  ChanQuery.Free;
end;

procedure InitCommon;
begin
  Channel := '';
  Sender := '';
  Count := '50';
  Msg := '';
  ChanList := TStringList.Create;
  LineList := TStringList.Create;
end;

procedure Init;
begin
  ShortDateFormat := 'dd"-"mmm"-"yy';
  InitCommon;
  InitDB;
end;

procedure FreeCommon;
begin
  ChanList.Free;
  LineList.Free;
end;

procedure Free;
begin
  FreeCommon;
  FreeDB;
end;

procedure GetWebVars;

  procedure TryGetWebVar(var LocalVar: string; const VarName, DefValue: string);
  begin
    if Length(GetCGIVar(VarName)) > 0 then begin
      LocalVar := GetCGIVar(VarName);
      if VarName = 'channel' then
        LocalVar := '#' + LocalVar;
      SetWebVar(VarName, LocalVar);
    end else if Length(GetWebVar(VarName)) > 0 then try
      LocalVar := GetWebVar(VarName);
      if Length(LocalVar) = 0 then
        LocalVar := DefValue;
      if VarName = 'channel' then
        LocalVar := '#' + LocalVar;
    except
      LocalVar := DefValue;
    end else begin
      SetWebVar(VarName, DefValue);
      LocalVar := DefValue;
    end;
  end;

var
  DefChan: string;
begin
  DefChan := '';
  if ChanList.Count > 0 then
    DefChan := '#' + ChanList[0];

  TryGetWebVar(Channel, 'channel', DefChan);
	
	Channel := return_Channel_sanitize(Channel); //Make sure to have only allowed chars ... :)

  TryGetWebVar(FromDate, 'fromdate', '');
	FromDate := return_datetimestamp_sanitize (FromDate); //Making sure that only allowed chars can be for the date
  TryGetWebVar(ToDate, 'todate', '');
	ToDate := return_datetimestamp_sanitize (ToDate); //Making sure that only allowed chars can be for the date

  TryGetWebVar(Count, 'linecount', '50');
 	Count := return_Number_sanitize (Count); //Making sure that we have only number chars ...

  TryGetWebVar(FromTime, 'fromtime', '');
        FromTime := return_datetimestamp_sanitize (FromTime);

  TryGetWebVar(ToTime, 'totime', '');
        ToTime := return_datetimestamp_sanitize (ToTime);

  TryGetWebVar(Sender, 'sender', '');
	Sender := return_Nick_sanitize (Sender); //Making sure that only RFC allowed chars exists

  TryGetWebVar(Msg, 'msg', '');
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
      if '#' + s <> Channel then
        Result := Result + '>'
      else
        Result := Result + ' selected>';
      Result := Result + '#' + s + '</option>';
    end;
end;

function HighlightHyperlinks(const inStr: string): string;
var
  s, tmp: string;
  n: Integer;

  procedure Convert(const x: Integer; const aps: string);
  var
    i: Integer;
  begin
    if x > 0 then begin
      for i := x + 1 to Length(s) do
        if s[i] = ' ' then Break;
      tmp := Trim(Copy(s, x + 1, i - x));
      s := StringReplace(s, ' ' + tmp, ' <a href="' + aps + tmp + '" target="new">' + tmp + '</a>', [rfReplaceAll]);
    end;
  end;

begin
  s := ' ' + InStr;

  n := Pos(' http://', LowerCase(s));
  Convert(n, '');

  n := Pos(' https://', LowerCase(s));
  Convert(n, '');

  n := Pos(' www.', LowerCase(s));
  Convert(n, 'http://');

  s := Trim(s);

  if Pos(' www.', s) + Pos(' http://', s) + Pos(' https://', s) > 0 then
    Result := HighlightHyperlinks(s)
  else
    Result := s;
end;

function ValidRequest: Boolean;
{var
  d1, d2: TDateTime;
  i: Integer;}
begin
  Result := True;
end;

function ColorCodeToHtml(var s: string; Tmp: string; var i: Integer): Boolean;
type
  TMethod = (meNormal, meMIRC);

const
  CTRL_C = #3;
  CTRL_END = #15;

  Crap = [CTRL_C, CTRL_END, '0'];
  Ctrls = [CTRL_C, CTRL_END];

  ColorCodeAr: array[#48..#75] of string = ('FFFFFF', '000000', 'FF0000', 'FF8000',
                                            'FFFF00', '80FF00', '00FF00', '00FF80',
                                            '00FFFF', '0080FF', '0000FF', '8000FF',
                                            'FF00FF', 'FF0080', 'C0C0C0', '404040',
                                            '800000', '804000', '808000', '408000',
                                            '008000', '008040', '008080', '004080',
                                            '000080', '400080', '800080', '800040');

  ColorCodeMIRC: array[0..15] of string = ('white', 'black', 'blue', 'green', 'red',
                                           'brown', 'purple', 'orange', 'yellow',
                                           'lt.green', 'teal', 'cyan', 'lt.blue',
                                           'pink', 'grey', 'lt.grey');
                                                                  
var
  Method: TMethod = meMIRC;
  Code: Integer;
begin
  Result := False;

  if Tmp[1] in Ctrls then begin
    Delete(s, i - Length(Tmp), Length(Tmp));
    Dec(i, Length(Tmp));
    
    while (Length(Tmp) > 0) and (Tmp[1] in Crap) do
      Delete(Tmp, 1, 1);

    if Pos(',', Tmp) > 0 then
      Delete(Tmp, Pos(',', Tmp), Length(Tmp)); // remove background color crap

    if Length(Tmp) = 0 then begin
      Insert('</font>', s, i);
      Inc(i, Length('</font>'));
      Exit;
    end;

    if Length(Tmp) > 1 then
      Method := meMIRC
    else if Tmp[1] > '9' then
      Method := meNormal;

    if Method = meNormal then begin
      Insert('<font color="#' + ColorCodeAr[Tmp[1]] + '">', s, i);
      Inc(i, Length('<font color="#' + ColorCodeAr[Tmp[1]] + '">'));
    end else try
      Code := StrToInt(Tmp);
      Insert('<font color="' + ColorCodeMIRC[Code] + '">', s, i);
      Inc(i, Length('<font color="' + ColorCodeMIRC[Code] + '">'));
    except
      on e: Exception do
        Insert('<' + e.Message + '>', s, i);
    end;
    Result := True;
  end;
end;

function DecodeColor(s: string): string;
const
  AllowedChars = [#3, #15, '0'..'9', ','];

var
  i: Integer;
  Tmp: string;
  InFont: Boolean = False;
  s2 : string;
begin
  Result := '';
  i := 1;
  Tmp := '';

  while i <= Length(s) do begin
    if s[i] in AllowedChars then
      Tmp := Tmp + s[i]
    else if Length(Tmp) > 0 then begin
      InFont := ColorCodeToHtml(s, Tmp, i);
      Tmp := '';
    end;
    
//    Tmp := Tmp + IntToStr(Ord(s[i])) + ' '; /////

    Inc(i);
  end;
  
//  s:= Tmp; ////

  if InFont then
    s := s + '</font>';

  Result := s;
end;

var
  LN, s, smsg, TheColor: string;
  Flip: Boolean;
begin
  Init;
  LN := '';
  s := '';

  FillChannels;
  GetWebVars;
  
  if Channel = '#' then
    if ChanList.Count > 0 then Channel := ChanList[0];

  WebTemplateOut('html' + PathDelim + 'header1.html', False);

  WebWriteln(GetHTMLChannels);

  WebTemplateOut('html' + PathDelim + 'header2.html', False);

  WebWriteln('<table class="body_style">');

  if Length(Sender) > 0 then s := s + ' and UPPER(sender)=' + UpperCase(AnsiQuotedStr(SQLEscape(Sender), #39));
  if Length(Msg) > 0 then s := s + ' and msg like ''%' + SQLEscape(Msg) + '%''';
  if Length(FromDate) > 0 then s := s + ' and CAST (logtime as date) >= ' + AnsiQuotedStr(SQLEscape (FromDate), #39) ;
  if Length(ToDate) > 0 then s := s + ' and CAST (logtime as date) <= ' + AnsiQuotedStr(SQLEscape (ToDate), #39) ;
  if Length(FromTime) > 0 then s := s + ' and CAST (logtime as time) >= ' + AnsiQuotedStr(SQLEscape (FromTime), #39) ;
  if Length(ToTime) > 0 then s := s + ' and CAST (logtime as time) <= ' + AnsiQuotedStr(SQLEscape (ToTime), #39) ;

  if  (Count = '9999')
  and (Length(FromDate) = 0)
  and (Length(ToDate) = 0) then
    s := s + ' and CAST (logtime as date) >= ' + AnsiQuotedStr(SQLEscape (DateTimeToStr(Now)), #39) ;

  if ValidRequest then begin
    with LogQuery do try
      sql.clear;
      sql.add('select * from (select sender, msg, logtime from tbl_loglines ' +
              'where (reciever=' + AnsiQuotedStr(SQLEscape(Channel), #39) + s + ') ' +
              'order by logtime desc limit ' + SQLEscape(Count) + ') as selection order by logtime');
      open;
      Flip := False;
      LN := FilterHtml(fieldbyname('sender').asstring);
      while not eof do begin
        s := FilterHtml(fieldbyname('sender').asstring);
        smsg := FilterHtml(fieldbyname('msg').asstring);
        if s <> LN then Flip := not Flip;

        TheColor := '';
        if Pos(s, smsg) = 1 then begin
          if Pos('quits(', smsg) = Length(s) + 2 then TheColor := ';color:'+ HTML_RED;
          if Pos('leaves ' + Channel, smsg) = Length(s) + 2 then TheColor := ';color:'+ HTML_RED;
          if Pos('joins ' + Channel, smsg) = Length(s) + 2 then TheColor := ';color:' + HTML_GREEN;
          if Pos(s + ' is now known as ', smsg) = 1 then TheColor := ';color:' + HTML_ORANGE;
        end else if s = '*' then TheColor := ';color:' + HTML_BLUE;
        
        smsg := DecodeColor(smsg);
        
        WebWriteln('<tr style="background-color:' + ColorAr[Flip] + TheColor + '">' +
                   '<td nowrap width="1%">[' +
                    fieldbyname('logtime').asstring +
                   ']' + '</td><td nowrap width="1%">' + s +
                   ': </td><td>' + HighlightHyperLinks(smsg) + '</td></tr>');

        LN := s;
        Next;
      end;
      close;

    except
      on e: Exception do
        WebWriteln('<h1>Invalid input: ' + e.Message + '</h1>');
    end;
  end else WebWriteln('<h1>Invalid input, date difference too big?</h1>');

  WebWriteln('</table><br>');
  WebWriteln('<p><a href="http://validator.w3.org/check?uri=referer"><img ' +
          'style="border:0;width:88px;height:31px" ' +
          'src="http://www.w3.org/Icons/valid-html401" alt="Valid HTML 4.01 ' +
          'Transitional" height="31" width="88"></a>');
  WebWriteln('<a href="http://jigsaw.w3.org/css-validator/"> ' +
          '<img style="border:0;width:88px;height:31px" ' +
          'src="http://jigsaw.w3.org/css-validator/images/vcss" ' +
          'alt="Valid CSS!"></a></p>');
  WebWriteln('</body></html>');

  Free;
end;

begin
  Main;
end.

