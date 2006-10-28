unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, lNetComponents, lSMTP, ComCtrls, Menus;

type

  { TMainForm }

  TMainForm = class(TForm)
    ButtonSend: TButton;
    ButtonConnect: TButton;
    GBConnection: TGroupBox;
    EditFrom: TEdit;
    EditSubject: TEdit;
    EditTo: TEdit;
    GBEmail: TGroupBox;
    MainMenu: TMainMenu;
    MemoText: TMemo;
    LabelSubject: TLabel;
    LabelTo: TLabel;
    LabelFrom: TLabel;
    MenuItemAbout: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemFile: TMenuItem;
    SMTP: TLSMTPClientComponent;
    EditServer: TEdit;
    EditPort: TEdit;
    LabelPort: TLabel;
    LabelServer: TLabel;
    SB: TStatusBar;
    procedure ButtonConnectClick(Sender: TObject);
    procedure ButtonSendClick(Sender: TObject);
    procedure EditFromKeyPress(Sender: TObject; var Key: Char);
    procedure EditServerKeyPress(Sender: TObject; var Key: Char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure SMTPConnect(Sender: TLSMTPClient);
    procedure SMTPDisconnect(Sender: TLSMTPClient);
    procedure SMTPError(const msg: string; aSocket: TLSocket);
    procedure SMTPFailure(Sender: TLSMTPClient; const aStatus: TLSMTPStatus);
    procedure SMTPReceive(Sender: TLSMTPClient);
    procedure SMTPSuccess(Sender: TLSMTPClient; const aStatus: TLSMTPStatus);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

{ TMainForm }

procedure TMainForm.SMTPConnect(Sender: TLSMTPClient);
begin
  SB.SimpleText:='Connected to server...';
  SMTP.Helo('mail.chello.sk');
  if SMTP.Connected then begin
    ButtonSend.Enabled:=SMTP.Connected;
    ButtonConnect.Caption:='Disconnect';
  end;
end;

procedure TMainForm.SMTPDisconnect(Sender: TLSMTPClient);
begin
  SB.SimpleText:='Disconnected from server';
  ButtonSend.Enabled:=SMTP.Connected;
  ButtonConnect.Caption:='Connect';
end;

procedure TMainForm.SMTPError(const msg: string; aSocket: TLSocket);
begin
  SMTPDisconnect(nil);
  SB.SimpleText:=msg;
end;

procedure TMainForm.SMTPFailure(Sender: TLSMTPClient;
  const aStatus: TLSMTPStatus);
begin
  case aStatus of
    ssMail: MessageDlg('Error sending message', mtError, [mbOK], 0);
    ssQuit: begin
              SMTP.Disconnect;
              Close;
            end;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  SMTP.OnError:=@SMTPError;
end;

procedure TMainForm.MenuItemAboutClick(Sender: TObject);
begin
  MessageDlg('SMTP example copyright (c) 2006 by Ales Katona. All rights deserved ;)',
             mtInformation, [mbOK], 0);
end;

procedure TMainForm.MenuItemExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.ButtonConnectClick(Sender: TObject);
begin
  if (not SMTP.Connected) and (ButtonConnect.Caption = 'Connect') then begin
    SMTP.Connect(EditServer.Text, Word(StrToInt(EditPort.Text)));
    ButtonConnect.Caption:='Connecting';
  end else
    SMTP.Quit; // server will respond and we'll make a clean disconnect (see SMTP rfc)
end;

procedure TMainForm.ButtonSendClick(Sender: TObject);
begin
  if Length(EditFrom.Text) < 6 then
    SB.SimpleText:='"Mail from" info is missing or irrelevant'
  else if Length(EditTo.Text) < 6 then
    SB.SimpleText:='"Mail to" info is missing or irrelevant'
  else begin
    SMTP.SendMail(EditFrom.Text, EditTo.Text, EditSubject.Text, MemoText.Text);
  end;
end;

procedure TMainForm.EditFromKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    ButtonSendClick(nil);
end;

procedure TMainForm.EditServerKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    ButtonConnectClick(nil);
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:=caFree;
  if SMTP.Connected then begin
    CloseAction:=caNone; // make sure we quit gracefuly
    SMTP.Quit; // the quit success/failure CBs will close our form
  end;
end;

procedure TMainForm.SMTPReceive(Sender: TLSMTPClient);
var
  s: string;
begin
  SMTP.GetMessage(s);
  SB.SimpleText:=s;
end;

procedure TMainForm.SMTPSuccess(Sender: TLSMTPClient;
  const aStatus: TLSMTPStatus);
begin
  case aStatus of
    ssMail: MessageDlg('Message sent successfuly', mtInformation, [mbOK], 0);
    ssQuit: begin
              SMTP.Disconnect;
              Close;
            end;
  end;
end;

initialization
  {$I main.lrs}

end.

