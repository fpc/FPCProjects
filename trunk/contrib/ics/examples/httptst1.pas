{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Fran�ois PIETTE
Creation:     November 23, 1997
Version:      1.07
Description:  Sample program to demonstrate some of the THttpCli features.
EMail:        http://www.overbyte.be        http://www.rtfm.be/fpiette
              francois.piette@overbyte.be   francois.piette@rtfm.be
                                            francois.piette@pophost.eunet.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2003 by Fran�ois PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@overbyte.be>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

Updates:
Jan 16, 1998  V1.00 Adapted for reviced HTTP component.
Apr 13, 1998  V1.01 Call HttpCli1DocEnd when to request failed, to close the
              document file in the case it is already opened.
Jul 09, 1998  V1.02 Adapted for Delphi 4
Sep 15, 1998  V1.03 Added some code to check for file creation errors (those
              errors mostly comes from document names not suitable as file
              names).
              Added code to save form size and position and to resize it
              correctly.
Sep 25, 1999  V1.04 Added display proxy setting
Aug 18, 2001  V1.05 Checked for document name before trying to display.
              Wrapped document file access to a try/finally.
              Checked ContentType to change document name extension according
              to some content type.
              Added display of copyright notice and version informations.
May 01, 2003  V1.06 Display Header checkbox added
May 09, 2003  V1.07 Implemented PUT

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit HttpTst1;

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, HttpProt, WinSock, WSocket, StdCtrls, ExtCtrls, IniFiles;

const
  HttpTstVersion         = 107;
  CopyRight : String     = 'HttpTst (c) 1999-2003 Francois Piette  V1.07 ';

type
  THttpTestForm = class(TForm)
    Panel1: TPanel;
    HttpCli1: THttpCli;
    URLEdit: TEdit;
    DisplayMemo: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    ProxyHostEdit: TEdit;
    ProxyPortEdit: TEdit;
    DataEdit: TEdit;
    Label3: TLabel;
    DateTimeEdit: TEdit;
    DocumentMemo: TMemo;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    HttpVersionComboBox: TComboBox;
    Label8: TLabel;
    Panel2: TPanel;
    GetButton: TButton;
    HeadButton: TButton;
    PostButton: TButton;
    AbortButton: TButton;
    ParseButton: TButton;
    PutButton: TButton;
    CloseButton: TButton;
    ClearButton: TButton;
    DisplayHeaderCheckBox: TCheckBox;
    Label9: TLabel;
    Label10: TLabel;
    PostContentTypeEdit: TEdit;
    procedure GetButtonClick(Sender: TObject);
    procedure HttpCli1Command(Sender: TObject; var s: String);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure HttpCli1DocBegin(Sender: TObject);
    procedure HttpCli1DocEnd(Sender: TObject);
    procedure PostButtonClick(Sender: TObject);
    procedure HeadButtonClick(Sender: TObject);
    procedure HttpCli1RequestDone(Sender: TObject; RqType: THttpRequest;
      ErrCode: Word);
    procedure AbortButtonClick(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure ParseButtonClick(Sender: TObject);
    procedure HttpCli1HeaderData(Sender: TObject);
    procedure HttpCli1Cookie(Sender: TObject; const Data: String;
      var Accept: Boolean);
    procedure HttpCli1LocationChange(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure PutButtonClick(Sender: TObject);
  private
    Initialized : Boolean;
    DocFileName : String;
    procedure SetButtonState(State : Boolean);
    procedure Display(const Msg : String);
    procedure PostOrPut(Request: THttpRequest);
  end;

var
  HttpTestForm: THttpTestForm;

implementation

{$R *.DFM}

const
    IniFileName    = 'httptest';
    SectionWindow  = 'WindowMain';
    KeyTop         = 'Top';
    KeyLeft        = 'Left';
    KeyWidth       = 'Width';
    KeyHeight      = 'Height';
    SectionData    = 'Data';
    KeyUrl         = 'URL';
    KeyProxyHost   = 'ProxyHost';
    KeyProxyPort   = 'ProxyPort';
    KeyData        = 'Data';
    KeyDateTime    = 'DateTime';
    KeyHttpVer     = 'HttpVer';
    KeyPostType    = 'PostContentType';


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.FormShow(Sender: TObject);
var
    IniFile : TIniFile;
    wsi     : TWSADATA;
begin
    if not Initialized then begin
        Initialized  := TRUE;
        IniFile      := TIniFile.Create(IniFileName);
        Width        := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        Height       := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        Top          := IniFile.ReadInteger(SectionWindow, KeyTop,
                                            (Screen.Height - Height) div 2);
        Left         := IniFile.ReadInteger(SectionWindow, KeyLeft,
                                            (Screen.Width  - Width)  div 2);
        URLEdit.Text       := IniFile.ReadString(SectionData, KeyURL,       '');
        ProxyHostEdit.Text := IniFile.ReadString(SectionData, KeyProxyHost, '');
        ProxyPortEdit.Text := IniFile.ReadString(SectionData, KeyProxyPort, '80');
        DataEdit.Text      := IniFile.ReadString(SectionData, KeyData,      '');
        DateTimeEdit.Text  := IniFile.ReadString(SectionData, KeyDateTime,  '');
        PostContentTypeEdit.Text := IniFile.ReadString(SectionData, KeyPostType, 'text/plain');
        HttpVersionComboBox.ItemIndex := IniFile.ReadInteger(SectionData, KeyHttpVer, 0);
        IniFile.Free;
        Panel2.BevelOuter := bvNone;
        { Display version info for program and used components }
        wsi := WinsockInfo;
        DisplayMemo.Clear;
        Display(CopyRight);
        Display('Using:');
        Display('   ' + WSocket.CopyRight);
        Display('   ' + HttpProt.CopyRight);
        Display('    Winsock:');
        Display('        Version ' +
                Format('%d.%d', [WinsockInfo.wHighVersion shr 8,
                                 WinsockInfo.wHighVersion and 15]));
        Display('        ' + StrPas(@wsi.szDescription));
        Display('        ' + StrPas(@wsi.szSystemStatus));
{$IFNDEF VER100}
        { A bug in Delphi 3 makes lpVendorInfo invalid }
        if wsi.lpVendorInfo <> nil then
            Display('        ' + StrPas(wsi.lpVendorInfo));
{$ENDIF}
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(IniFileName);
    IniFile.WriteInteger(SectionWindow, KeyTop,       Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,      Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,     Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight,    Height);
    IniFile.WriteString(SectionData,    KeyURL,       URLEdit.Text);
    IniFile.WriteString(SectionData,    KeyProxyHost, ProxyHostEdit.Text);
    IniFile.WriteString(SectionData,    KeyProxyPort, ProxyPortEdit.Text);
    IniFile.WriteString(SectionData,    KeyData,      DataEdit.Text);
    IniFile.WriteString(SectionData,    KeyDateTime,  DateTimeEdit.Text);
    IniFile.WriteInteger(SectionData,   KeyHttpVer,   HttpVersionComboBox.ItemIndex);
    IniFile.WriteString(SectionData,    KeyPostType,  PostContentTypeEdit.Text);
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Display a message in display memo box, making sure we don't overflow it.  }
procedure THttpTestForm.Display(const Msg : String);
begin
    DisplayMemo.Lines.BeginUpdate;
    try
        if DisplayMemo.Lines.Count > 200 then begin
            { We preserve only 200 lines }
            while DisplayMemo.Lines.Count > 200 do
                DisplayMemo.Lines.Delete(0);
        end;
        DisplayMemo.Lines.Add(Msg);
    finally
        DisplayMemo.Lines.EndUpdate;
        { Makes last line visible }
        {$IFNDEF VER80}
        SendMessage(DisplayMemo.Handle, EM_SCROLLCARET, 0, 0);
        {$ENDIF}
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.HeadButtonClick(Sender: TObject);
var
    I       : Integer;
begin
    DisplayMemo.Clear;
    DocumentMemo.Clear;
    SetButtonState(FALSE);

    try
        HttpCli1.URL        := URLEdit.Text;
        HttpCli1.Proxy      := ProxyHostEdit.Text;
        HttpCli1.ProxyPort  := ProxyPortEdit.Text;
        HttpCli1.Connection := 'Keep-Alive';
        HttpCli1.RequestVer := '1.' + IntToStr(HttpVersionComboBox.ItemIndex);
        HttpCli1.RcvdStream := nil;
        if DateTimeEdit.Text <> '' then
            HttpCli1.ModifiedSince := StrToDateTime(DateTimeEdit.Text)
        else
            HttpCli1.ModifiedSince := 0;

        if HttpCli1.Proxy <> '' then
            Display('Using proxy ''' + HttpCli1.Proxy + ':' +
                    HttpCli1.ProxyPort + '''')
        else
            Display('Not using proxy');

        try
            HttpCli1.Head;
        except
            Display('HEAD Failed !');
            Display('StatusCode   = ' + IntToStr(HttpCli1.StatusCode));
            Display('ReasonPhrase = ' + HttpCli1.ReasonPhrase);
            Exit;
        end;

        Display('StatusCode = ' + IntToStr(HttpCli1.StatusCode));

        if DisplayHeaderCheckBox.Checked then
            for I := 0 to HttpCli1.RcvdHeader.Count - 1 do
                Display('hdr>' + HttpCli1.RcvdHeader.Strings[I]);
    finally
        SetButtonState(TRUE);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.GetButtonClick(Sender: TObject);
var
    I       : Integer;
    DataIn  : TStream;
begin
    DisplayMemo.Clear;
    DocumentMemo.Clear;
    SetButtonState(FALSE);

    try
        HttpCli1.URL            := URLEdit.Text;
        HttpCli1.Proxy          := ProxyHostEdit.Text;
        HttpCli1.ProxyPort      := ProxyPortEdit.Text;
        HttpCli1.AcceptLanguage := 'en, fr';
        HttpCli1.Connection     := 'Keep-Alive';
        HttpCli1.RequestVer     := '1.' + IntToStr(HttpVersionComboBox.ItemIndex);
        HttpCli1.RcvdStream := nil;
        if DateTimeEdit.Text <> '' then
            HttpCli1.ModifiedSince := StrToDateTime(DateTimeEdit.Text)
        else
            HttpCli1.ModifiedSince := 0;

        if HttpCli1.Proxy <> '' then
            Display('Using proxy ''' + HttpCli1.Proxy + ':' +
                    HttpCli1.ProxyPort + '''')
        else
            Display('Not using proxy');

        try
            HttpCli1.Get;
        except
            Display('GET Failed !');
            Display('StatusCode   = ' + IntToStr(HttpCli1.StatusCode));
            Display('ReasonPhrase = ' + HttpCli1.ReasonPhrase);
            HttpCli1DocEnd(nil);  { This will close the file }
            Exit;
        end;

        Display('StatusCode = ' + IntToStr(HttpCli1.StatusCode));

        if DisplayHeaderCheckBox.Checked then 
            for I := 0 to HttpCli1.RcvdHeader.Count - 1 do
                Display('hdr>' + HttpCli1.RcvdHeader.Strings[I]);

        if Length(DocFileName) = 0 then begin
            DocumentMemo.Lines.Add('*** NO DOCUMENT FILE NAME ***');
        end
        else begin
            DataIn := TFileStream.Create(DocFileName, fmOpenRead);
            try
                if Copy(HttpCli1.ContentType, 1, 5) = 'text/' then
                    DocumentMemo.Lines.LoadFromStream(DataIn)
                else begin
                    DocumentMemo.Lines.Add('Content type is ' +
                                           HttpCli1.ContentType);
                    DocumentMemo.Lines.Add('Document stored in ''' +
                                           DocFileName +
                                           ''' Size=' + IntToStr(DataIn.Size));
                end;
            finally
                DataIn.Free;
            end;
        end;
    finally
        SetButtonState(TRUE);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.CloseButtonClick(Sender: TObject);
begin
    try
        HttpCli1.Close;
    except
        Display('CLOSE Failed !');
        Display('StatusCode   = ' + IntToStr(HttpCli1.StatusCode));
        Display('ReasonPhrase = ' + HttpCli1.ReasonPhrase);
        Exit;
    end;
    Display('StatusCode = ' + IntToStr(HttpCli1.StatusCode));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.PutButtonClick(Sender: TObject);
begin
    PostOrPut(httpPUT);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.PostButtonClick(Sender: TObject);
begin
    PostOrPut(httpPOST);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.PostOrPut(Request: THttpRequest);
var
    DataOut : TMemoryStream;
    DataIn  : TFileStream;
    Buf     : String;
    I       : Integer;
begin
    DisplayMemo.Clear;
    DocumentMemo.Clear;
    SetButtonState(FALSE);

    try
        DataOut := TMemoryStream.Create;
        Buf     := DataEdit.Text;
        if Length(Buf) > 0 then      { Check if some data to post }
            DataOut.Write(Buf[1], Length(Buf));
        DataOut.Seek(0, soFromBeginning);

        HttpCli1.SendStream      := DataOut;
        HttpCli1.Proxy           := ProxyHostEdit.Text;
        HttpCli1.ProxyPort       := ProxyPortEdit.Text;
        HttpCli1.Connection      := 'Keep-Alive';
        HttpCli1.RcvdStream      := nil;
        HttpCli1.ContentTypePost := PostContentTypeEdit.Text;
        HttpCli1.URL             := URLEdit.Text;
        HttpCli1.RequestVer      := '1.' +
                                    IntToStr(HttpVersionComboBox.ItemIndex);

        if HttpCli1.Proxy <> '' then
            Display('Using proxy ''' + HttpCli1.Proxy + ':' +
                                  HttpCli1.ProxyPort + '''')
        else
            Display('Not using proxy');

        try
            if Request = httpPOST then
                HttpCli1.Post
            else
                HttpCli1.Put;
        except
            DataOut.Free;
            Display('POST Failed !');
            Display('StatusCode   = ' + IntToStr(HttpCli1.StatusCode));
            Display('ReasonPhrase = ' + HttpCli1.ReasonPhrase);
            Exit;
        end;
        DataOut.Free;

        Display('StatusCode = ' + IntToStr(HttpCli1.StatusCode) +
                ' (' + HttpCli1.ReasonPhrase + ')');

        if DisplayHeaderCheckBox.Checked then
            for I := 0 to HttpCli1.RcvdHeader.Count - 1 do
                Display('hdr>' + HttpCli1.RcvdHeader.Strings[I]);

        if HttpCli1.ContentLength = 0 then
            DocumentMemo.Lines.Add('No document received.')
        else begin
            DataIn := TFileStream.Create(HttpCli1.DocName, fmOpenRead);
            try
                if Copy(HttpCli1.ContentType, 1, 5) = 'text/' then
                    DocumentMemo.Lines.LoadFromStream(DataIn)
                else begin
                    DocumentMemo.Lines.Add('Content type is ' +
                                           HttpCli1.ContentType);
                    DocumentMemo.Lines.Add('Document stored in ''' +
                                           DocFileName +
                                           ''' Size=' + IntToStr(DataIn.Size));
                end;
            finally
                DataIn.Free;
            end;
        end;
    finally
        SetButtonState(TRUE);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ WARNING: With DELPHI1, change "s: String" to "s: OpenString"              }
procedure THttpTestForm.HttpCli1Command(Sender: TObject; var s: String);
begin
    Display('cmd> ' + s);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.HttpCli1DocBegin(Sender: TObject);
begin
    Display(HttpCli1.ContentType + ' => ' + HttpCli1.DocName);
    Display('Location = ' + HttpCli1.Location);
    Display('URL = ' + HttpCli1.URL);
    Display('Document = ' + HttpCli1.DocName);

    DocFileName := HttpCli1.DocName;

    if HttpCli1.ContentType = 'image/gif' then
        ReplaceExt(DocFileName, 'gif')
    else if HttpCli1.ContentType = 'image/jpeg' then
        ReplaceExt(DocFileName, 'jpg')
    else if HttpCli1.ContentType = 'image/bmp' then
        ReplaceExt(DocFileName, 'bmp');

    if DocFileName = '' then
        DocFileName := 'HttpTst.htm';
    try
        HttpCli1.RcvdStream := TFileStream.Create(DocFileName, fmCreate);
    except
        on E:Exception do begin
            Display('Error opening file: ' + E.Message);
            DocFileName := 'HttpTst.htm';
            Display('Using default file name: ' + DocFileName);
            HttpCli1.RcvdStream := TFileStream.Create(DocFileName, fmCreate);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.HttpCli1DocEnd(Sender: TObject);
begin
    if HttpCli1.RcvdStream <> nil then begin
        HttpCli1.RcvdStream.Free;
        HttpCli1.RcvdStream := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.SetButtonState(State : Boolean);
begin
    GetButton.Enabled   := State;
    PostButton.Enabled  := State;
    HeadButton.Enabled  := State;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.HttpCli1RequestDone(
    Sender  : TObject;
    RqType  : THttpRequest;
    ErrCode : Word);
begin
    SetButtonState(TRUE);
    if ErrCode <> 0 then
        Display('RequestDone Error = ' + IntToStr(ErrCode))
    else
        Display('RequestDone, no error');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.AbortButtonClick(Sender: TObject);
begin
    HttpCli1.Abort;
    Display('StatusCode = ' + IntToStr(HttpCli1.StatusCode));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.Panel1Resize(Sender: TObject);
begin
    URLEdit.Width             := Panel1.Width - Panel2.Width - URLEdit.Left - 8;
    DataEdit.Width            := URLEdit.Width;
    PostContentTypeEdit.Width := Panel1.Width - Panel2.Width -
                                 PostContentTypeEdit.Left - 8
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.ParseButtonClick(Sender: TObject);
var
    Proto, User, Pass, Host, Port, Path : String;
begin
    ParseURL(URLEdit.Text, Proto, User, Pass, Host, Port, Path);
    Display('URL   = ''' + URLEdit.Text + '''');
    Display('Proto = ''' + Proto + '''');
    Display('Host  = ''' + Host  + '''');
    Display('Path  = ''' + Path  + '''');
    Display('Port  = ''' + Port  + '''');
    Display('User  = ''' + User  + '''');
    Display('Pass  = ''' + Pass  + '''');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.HttpCli1HeaderData(Sender: TObject);
begin
{    Display('Header: "' + HttpCli1.LastResponse + '"'); }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.HttpCli1Cookie(
    Sender     : TObject;
    const Data : String;
    var Accept : Boolean);
begin
    Display('Cookie: "' + Data + '"');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.HttpCli1LocationChange(Sender: TObject);
begin
    Display('Location changed to "' + HttpCli1.Location + '"');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.ClearButtonClick(Sender: TObject);
begin
    DocumentMemo.Clear;
    DisplayMemo.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

