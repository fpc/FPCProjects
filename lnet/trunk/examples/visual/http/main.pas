unit main; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  lNetComponents, SynHighlighterHTML, SynEdit, ExtCtrls, StdCtrls, Buttons;
  
type

  { TMainForm }

  TMainForm = class(TForm)
    ButtonSendRequest: TButton;
    EditPort: TEdit;
    EditURI: TEdit;
    EditHost: TEdit;
    HTTPClient: TLHTTPClientComponent;
    LabelPort: TLabel;
    LabelURI: TLabel;
    LabelHost: TLabel;
    MemoHTML: TMemo;
    MemoStatus: TMemo;
    MenuPanel: TPanel;
    procedure ButtonSendRequestClick(Sender: TObject);
    procedure EditHostKeyPress(Sender: TObject; var Key: char);
    procedure HTTPClientDisconnect(aSocket: TLSocket);
    procedure HTTPClientDoneInput(ASocket: TLHTTPClientSocket);
    procedure HTTPClientError(const msg: string; aSocket: TLSocket);
    function HTTPClientInput(ASocket: TLHTTPClientSocket; ABuffer: pchar;
      ASize: dword): dword;
    procedure HTTPClientProcessHeaders(ASocket: TLHTTPClientSocket);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

uses
  lHTTP;

{ TMainForm }

procedure TMainForm.HTTPClientError(const msg: string; aSocket: TLSocket);
begin
  MessageDlg(msg, mtError, [mbOK], 0);
end;

procedure TMainForm.HTTPClientDisconnect(aSocket: TLSocket);
begin
  MemoStatus.Append('Disconnected.');
end;

procedure TMainForm.ButtonSendRequestClick(Sender: TObject);
begin
  HTTPClient.Host:=EditHost.Text;
  HTTPClient.Port:=Word(StrToInt(EditPort.Text));
  HTTPClient.URI:=EditURI.Text;
  HTTPClient.SendRequest;
end;

procedure TMainForm.EditHostKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    ButtonSendRequestClick(Sender);
end;

procedure TMainForm.HTTPClientDoneInput(ASocket: TLHTTPClientSocket);
begin
  aSocket.Disconnect;
  MemoStatus.Append('Finished.');
end;

function TMainForm.HTTPClientInput(ASocket: TLHTTPClientSocket; ABuffer: pchar;
  ASize: dword): dword;
var
  s: string;
begin
  SetLength(s, aSize);
  Move(aBuffer^, s[1], aSize);
  MemoHTML.Append(s);
  MemoStatus.Append(IntToStr(ASize) + '...');
end;

procedure TMainForm.HTTPClientProcessHeaders(ASocket: TLHTTPClientSocket);
begin
  MemoStatus.Append('Response: ' + IntToStr(HTTPStatusCodes[ASocket.ResponseStatus]) +
                    ' ' + ASocket.ResponseReason + ', data...');
end;

initialization
  {$I main.lrs}
  {$error Bugged, wait for new release shortly}

end.

