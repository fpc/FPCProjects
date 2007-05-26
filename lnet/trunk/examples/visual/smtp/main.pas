unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ComCtrls, Menus, ExtCtrls,
  lMimeWrapper, lNetComponents, lSMTP, lNet;

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
    ListBoxAttachments: TListBox;
    MainMenu: TMainMenu;
    LabelSubject: TLabel;
    LabelTo: TLabel;
    LabelFrom: TLabel;
    MemoText: TMemo;
    MenuItemLogs: TMenuItem;
    MenuItemDelete: TMenuItem;
    MenuItemAdd: TMenuItem;
    MenuItemAbout: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemFile: TMenuItem;
    OD: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    PopupMenuAttachments: TPopupMenu;
    SMTP: TLSMTPClientComponent;
    EditServer: TEdit;
    EditPort: TEdit;
    LabelPort: TLabel;
    LabelServer: TLabel;
    SB: TStatusBar;
    TimerQuit: TTimer;
    procedure ButtonConnectClick(Sender: TObject);
    procedure ButtonSendClick(Sender: TObject);
    procedure EditFromKeyPress(Sender: TObject; var Key: Char);
    procedure EditServerKeyPress(Sender: TObject; var Key: Char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemAddClick(Sender: TObject);
    procedure MenuItemDeleteClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure MenuItemLogsClick(Sender: TObject);
    procedure SMTPConnect(aSocket: TLSocket);
    procedure SMTPDisconnect(aSocket: TLSocket);
    procedure SMTPError(const msg: string; aSocket: TLSocket);
    procedure SMTPReceive(aSocket: TLSocket);
    procedure SMTPFailure(aSocket: TLSocket; const aStatus: TLSMTPStatus);
    procedure SMTPSuccess(aSocket: TLSocket; const aStatus: TLSMTPStatus);
    procedure TimerQuitTimer(Sender: TObject);
  private
    FMimeStream: TMimeStream;
    FQuit: Boolean; // to see if we force quitting
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

uses
  Logs;

{ TMainForm }

procedure TMainForm.SMTPConnect(aSocket: TLSocket);
begin
  SB.SimpleText := 'Connected to server...';
  FormLogs.MemoLogs.Append(SB.SimpleText);
  SMTP.Helo(EditServer.Text);
  if SMTP.Connected then begin
    ButtonSend.Enabled := SMTP.Connected;
    ButtonConnect.Caption := 'Disconnect';
  end;
end;

procedure TMainForm.SMTPDisconnect(aSocket: TLSocket);
begin
  SB.SimpleText := 'Disconnected from server';
  FormLogs.MemoLogs.Append(SB.SimpleText);
  ButtonSend.Enabled := SMTP.Connected;
  ButtonConnect.Caption := 'Connect';
end;

procedure TMainForm.SMTPError(const msg: string; aSocket: TLSocket);
begin
  SMTPDisconnect(nil);
  SB.SimpleText := msg;
  FormLogs.MemoLogs.Append(msg);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  SMTP.OnError := @SMTPError;
  FMimeStream := TMimeStream.Create;
  FMimeStream.AddTextSection(''); // for the memo
end;

procedure TMainForm.MenuItemAboutClick(Sender: TObject);
begin
  MessageDlg('SMTP example copyright (c) 2006-2007 by Ales Katona. All rights deserved ;)',
             mtInformation, [mbOK], 0);
end;

procedure TMainForm.MenuItemAddClick(Sender: TObject);
begin
  if OD.Execute then
    if FileExists(OD.FileName) then begin
      FMimeStream.AddFileSection(OD.FileName);
      ListBoxAttachments.Items.Add(ExtractFileName(OD.FileName));
    end;
end;

procedure TMainForm.MenuItemDeleteClick(Sender: TObject);
begin
  if  (ListBoxAttachments.ItemIndex >= 0)
  and (ListBoxAttachments.ItemIndex < FMimeStream.Count - 1) then begin
    FMimeStream.Delete(ListBoxAttachments.ItemIndex + 1); // 0th is the text of memo
    ListBoxAttachments.Items.Delete(ListBoxAttachments.ItemIndex);
  end;
end;

procedure TMainForm.MenuItemExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.MenuItemLogsClick(Sender: TObject);
begin
  FormLogs.Show;
end;

procedure TMainForm.ButtonConnectClick(Sender: TObject);
begin
  if (not SMTP.Connected) and (ButtonConnect.Caption = 'Connect') then begin
    SMTP.Connect(EditServer.Text, Word(StrToInt(EditPort.Text)));
    ButtonConnect.Caption := 'Connecting';
    SB.SimpleText := 'Connecting...';
  end else if ButtonConnect.Caption = 'Connecting' then begin
    SMTP.Disconnect;
    ButtonConnect.Caption := 'Connect';
    SB.SimpleText := 'Aborted connect!';
  end else
    SMTP.Quit; // server will respond and we'll make a clean disconnect (see SMTP rfc)
end;

procedure TMainForm.ButtonSendClick(Sender: TObject);
begin
  if Length(EditFrom.Text) < 6 then
    SB.SimpleText := '"Mail from" info is missing or irrelevant'
  else if Length(EditTo.Text) < 6 then
    SB.SimpleText := '"Mail to" info is missing or irrelevant'
  else begin
    FMimeStream.Reset; // make sure we can read it again
    TMimeTextSection(FMimeStream[0]).Text := MemoText.Text; // change to text
    SMTP.SendMail(EditFrom.Text, EditTo.Text, EditSubject.Text, FMimeStream); // send the stream
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

procedure TMainForm.SMTPReceive(aSocket: TLSocket);
var
  s: string;
begin
  if SMTP.GetMessage(s) > 0 then begin
    SB.SimpleText := StringReplace(s, #13#10, '', [rfReplaceAll]);
    FormLogs.MemoLogs.Append(SB.SimpleText);
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;

  if not FQuit and SMTP.Connected then begin
    CloseAction := caNone; // make sure we quit gracefuly
    SMTP.Quit; // the quit success/failure CBs will close our form
    TimerQuit.Enabled := True; // if this runs out, quit ungracefully
  end else
    FMimeStream.Free;
end;

procedure TMainForm.SMTPFailure(aSocket: TLSocket;
  const aStatus: TLSMTPStatus);
begin
  case aStatus of
    ssData: MessageDlg('Error sending message', mtError, [mbOK], 0);
    ssQuit: begin
              SMTP.Disconnect;
              Close;
            end;
  end;
end;

procedure TMainForm.SMTPSuccess(aSocket: TLSocket;
  const aStatus: TLSMTPStatus);
begin
  case aStatus of
    ssData: MessageDlg('Message sent successfuly', mtInformation, [mbOK], 0);
    ssQuit: begin
              SMTP.Disconnect;
              Close;
            end;
  end;
end;

procedure TMainForm.TimerQuitTimer(Sender: TObject);
begin
  FQuit := True;
  Close;
end;

initialization
  {$I main.lrs}

end.

