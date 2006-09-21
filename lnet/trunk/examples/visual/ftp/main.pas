unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, LCLType,
  LNetComponents, ComCtrls, ExtCtrls, Buttons, StdCtrls, Menus, FileCtrl;

type

  { TMainForm }

  TMainForm = class(TForm)
    ButtonDisconnect: TButton;
    ButtonConnect: TButton;
    LeftView: TFileListBox;
    LabelHost: TLabel;
    PopupDelete: TMenuItem;
    FTP: TLFTPClientComponent;
    PopupLeft: TPopupMenu;
    PopupLInfo: TMenuItem;
    PopupLDelete: TMenuItem;
    PopupLRename: TMenuItem;
    PupupGet: TMenuItem;
    MemoText: TMemo;
    PopupRename: TMenuItem;
    PopupRight: TPopupMenu;
    RightView: TListBox;
    MainMenu: TMainMenu;
    MenuItemFile: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemAbout: TMenuItem;
    LabelPort: TLabel;
    EditPort: TEdit;
    EditIP: TEdit;
    procedure AboutMenuItemClick(Sender: TObject);
    procedure ConnectButtonClick(Sender: TObject);
    procedure DeletePopupClick(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure ExitMenuItemClick(Sender: TObject);
    procedure FTPConnect(Sender: TLFTPClient);
    procedure FTPControl(Sender: TLFTPClient);
    procedure FTPError(const msg: string; aSocket: TLSocket);
    procedure FTPReceive(Sender: TLFTPClient);
    procedure FTPSent(Sender: TLFTPClient; const Bytes: Integer);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GetPupupClick(Sender: TObject);
    procedure IPEditKeyPress(Sender: TObject; var Key: char);
    procedure LDeletePopupClick(Sender: TObject);
    procedure LInfoPopupClick(Sender: TObject);
    procedure LRenamePopupClick(Sender: TObject);
    procedure LeftViewDblClick(Sender: TObject);
    procedure ListPopupClick(Sender: TObject);
    procedure RenamePopupClick(Sender: TObject);
    procedure RightViewDblClick(Sender: TObject);
  private
    FLastN: Integer;
    FList: TStringList;
    FFile: TFileStream;
    FInfoExpected: Boolean;
    FDLSize: Integer;
    FDLDone: Integer;
    CreateFilePath: string;
    function CleanLine(const aLine: string): string;
    function CleanSize(const aLine: string): Integer;
    procedure DoList(const FileName: string);
    { private declarations }
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

var
  Dir: string;

{ TMainForm }

procedure TMainForm.ExitMenuItemClick(Sender: TObject);
begin
  MainForm.Close;
end;

procedure TMainForm.FTPConnect(Sender: TLFTPClient);
var
  aName, Pass: string;
begin
  if InputQuery('Name', 'Please type in your username', False, aName) then
    Pass:=PasswordBox('Password', 'Please type in your password');
  FTP.Authenticate(aName, Pass);
  FTP.Binary:=True;
  DoList('');
end;

procedure TMainForm.FTPControl(Sender: TLFTPClient);
var
  s: string;
begin
  if Sender.GetMessage(s) > 0 then begin
    MemoText.Lines.Append(s);
    MemoText.SelStart:=Length(MemoText.Text);
  end;
end;

procedure TMainForm.FTPError(const msg: string; aSocket: TLSocket);
begin
  MemoText.Append(msg);
  CreateFilePath:='';
end;

procedure TMainForm.FTPReceive(Sender: TLFTPClient);
var
  Line: TStringList;

  procedure FindNames;
  var
    i, j, k, n: Integer;
    s: string;
  begin
    if FList.Count > 0 then
      for i:=0 to FList.Count-1 do begin
        Line.Clear;
        
        s:=FList[i];
        while Pos('  ', s) > 0 do
          s:=StringReplace(s, '  ', ' ', [rfReplaceAll]);
          
        for k:=1 to 8 do try
          n:=Pos(' ', s);
          Line.Add(Copy(s, 1, n-1));
          s:=Copy(s, n+1, Length(s));
        except
          Line.Add('*PARSE ERROR*');
        end;
        Line.Add(s);
        
        n:=Pos(' -> ', Line[Line.Count-1]);
        if n > 0 then
          Line[Line.Count-1]:=Copy(Line[Line.Count-1], 1, n-1);

        if Line.Count > 8 then begin
          s:='';
          for j:=8 to Line.Count-1 do
            s:=s + Line[j] + ' ';
          SetLength(s, Length(s) - 1);
          if Line[0][1] in ['d', 'l'] then
            RightView.Items.Add('[' + s + ']  /' + Line[4] + '/' + Line[0] + '/')
          else
            RightView.Items.Add(s + '  /' + Line[4] + '/' + Line[0] + '/')
        end;
      end;
  end;

var
  s: string;
  i: Integer;
  Buf: array[0..65535] of Byte;
begin
  if FInfoExpected then begin
    Line:=TStringList.Create;
    s:=Sender.GetDataMessage;
    if Length(s) > 0 then begin
      FList.Text:=FList.Text + s;
    end else begin
      FindNames;
      FInfoExpected:=False;
      FList.Clear;
    end;
    Line.Free;
  end else begin
    i:=Sender.GetData(Buf, 65535);
    if i > 0 then begin
      if Length(CreateFilePath) > 0 then begin
        FFile:=TFileStream.Create(CreateFilePath, fmCreate or fmOpenWrite);
        CreateFilePath:='';
      end;
      FFile.Write(Buf, i);
    end else begin
      FreeAndNil(FFile);
      CreateFilePath:='';
      DoList('');
    end;
    Inc(FDLDone, i);
    if MemoText.Lines.Count > 0 then
      MemoText.Lines[MemoText.Lines.Count-1]:=IntToStr(Round(FDLDone / FDLSize * 100)) + '%';
  end;
end;

procedure TMainForm.FTPSent(Sender: TLFTPClient; const Bytes: Integer);
var
  n: Integer;
begin
  if Bytes > 0 then begin
    Inc(FDLDone, Bytes);
    if MemoText.Lines.Count > 0 then begin
      n:=Round(FDLDone / FDLSize * 100);
      if n <> FLastN then begin
        MemoText.Lines[MemoText.Lines.Count-1]:=IntToStr(n) + '%';
        FLastN:=n;
      end;
    end;
  end else begin
    if MemoText.Lines.Count > 0 then
      MemoText.Lines[MemoText.Lines.Count-1]:='100%';
    DoList('');
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FList.Free;
  FreeAndNil(FFile);
end;

procedure TMainForm.ConnectButtonClick(Sender: TObject);
begin
  if Length(EditIP.Text) > 0 then begin
    if Length(EditPort.Text) = 0 then
      EditPort.Text:='21';

    FTP.Connect(EditIP.Text, StrToInt(EditPort.Text));
  end else ShowMessage('Please specify IP or hostname for ftp server');
end;

procedure TMainForm.AboutMenuItemClick(Sender: TObject);
begin
  MessageDlg('lFTP test program copyright (c) 2005-2006 by Ales Katona. All rights deserved :)',
             mtInformation, [mbOK], 0);
end;

procedure TMainForm.DeletePopupClick(Sender: TObject);
begin
  FTP.DeleteFile(CleanLine(RightView.Items[RightView.ItemIndex]));
  DoList('');
end;

procedure TMainForm.DisconnectButtonClick(Sender: TObject);
begin
  FTP.Disconnect;
  RightView.Clear;
  MemoText.Clear;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Dir:=ExtractFilePath(ParamStr(0));
  FInfoExpected:=False;
  FList:=TStringList.Create;
  FFile:=nil;
  LeftView.Directory:=Dir;
  CreateFilePath:='';
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_F5: begin end;
  end;
end;

procedure TMainForm.GetPupupClick(Sender: TObject);
var
  s: string;
begin
  s:=CleanLine(RightView.Items[RightView.ItemIndex]);
  FDLSize:=CleanSize(RightView.Items[RightView.ItemIndex]);
  FDLDone:=0;

  FreeAndNil(FFile); // if the last get failed, we need to free memory here
  CreateFilePath:=Dir + s;
  FTP.Retrieve(s);
end;

procedure TMainForm.IPEditKeyPress(Sender: TObject; var Key: char);
begin
  if Byte(Key) in [VK_EXECUTE, VK_RETURN] then
    ConnectButtonClick(Sender);
end;

procedure TMainForm.LDeletePopupClick(Sender: TObject);
begin
  if FileExists(LeftView.Directory + LeftView.Items[LeftView.ItemIndex]) then
    DeleteFile(LeftView.Directory + LeftView.Items[LeftView.ItemIndex]);
end;

procedure TMainForm.LInfoPopupClick(Sender: TObject);
var
  f: file;
begin
  if FileExists(LeftView.Directory + LeftView.Items[LeftView.ItemIndex]) then begin
    AssignFile(f, LeftView.Directory + LeftView.Items[LeftView.ItemIndex]);
    Reset(f, 1);
    ShowMessage('FileSize: ' + IntToStr(FileSize(f)) + ' bytes');
    CloseFile(f);
  end;
end;

procedure TMainForm.LRenamePopupClick(Sender: TObject);
var
  aName: string;
begin
  if FileExists(LeftView.Directory + LeftView.Items[LeftView.ItemIndex]) then begin
    if InputQuery('New Name', 'Please type in new filename', False, aName) then
      RenameFile(LeftView.Directory + LeftView.Items[LeftView.ItemIndex],
                 LeftView.Directory + aName);
  end;
end;

procedure TMainForm.LeftViewDblClick(Sender: TObject);

  function GetParentDirectory(Path: string): string;
  var
    i: Integer;
  begin
    Path:=StringReplace(Path, PathDelim + PathDelim, PathDelim, [rfReplaceAll]);
    if Length(Path) > 1 then
      for i:=Length(Path)-1 downto 1 do
        if Path[i] = PathDelim then begin
          Result:=Copy(Path, 1, i);
          Exit;
        end;
    Result:=Path;
  end;

var
  s: string;
  FF: TFileStream;
begin
  if ExtractFileName(LeftView.FileName) = '[.]' then
    Exit;
    
  if ExtractFileName(LeftView.FileName) = '[..]' then
    s:=GetParentDirectory(LeftView.Directory)
  else begin
    s:=ExtractFileName(LeftView.FileName);
    s:=LeftView.Directory + Copy(s, 2, Length(s)-2) + PathDelim;
  end;

  if DirectoryExists(s) then
    LeftView.Directory:=s
  else if FTP.Connected then begin
    FDLDone:=0;
    FF:=TFileStream.Create(LeftView.FileName, fmOpenRead);
    FDLSize:=FF.Size;
    FF.Free;
    FLastN:=0;
    FTP.Put(LeftView.FileName);
  end;
end;

procedure TMainForm.ListPopupClick(Sender: TObject);
begin
  DoList(RightView.Items[RightView.ItemIndex]);
end;

procedure TMainForm.RenamePopupClick(Sender: TObject);
var
  aName: string;
begin
  if InputQuery('New Name', 'Please type in new filename', False, aName) then begin
    FTP.Rename(CleanLine(RightView.Items[RightView.ItemIndex]), aName);
    DoList('');
  end;
end;

procedure TMainForm.RightViewDblClick(Sender: TObject);
var
  s: string;
begin
  s:=CleanLine(RightView.Items[RightView.ItemIndex]);
  FDLSize:=CleanSize(RightView.Items[RightView.ItemIndex]);
  FDLDone:=0;
  if s[1] = '[' then begin
    FTP.ChangeDirectory(Copy(s, 2, Length(s) - 2));
    DoList('');
  end else begin
    FreeAndNil(FFile);
    CreateFilePath:=Dir + s;
    FTP.Retrieve(s);
  end;
end;

function TMainForm.CleanLine(const aLine: string): string;
var
  n: Integer;
begin
  Result:=aLine;
  n:=Pos('/', aLine);
  if n > 0 then
    Result:=Copy(aLine, 1, n-3);
end;

function TMainForm.CleanSize(const aLine: string): Integer;
var
  n: Integer;
  s: string;
begin
  Result:=0;
  n:=Pos('/', aLine);
  if n > 0 then begin
    s:=Copy(aLine, n+1, Length(aLine));
    n:=Pos('/', s);
    Result:=StrToInt(Copy(s, 1, n-1));
  end;
end;

procedure TMainForm.DoList(const FileName: string);
begin
  RightView.Clear;
  RightView.Items.Add('[..]');
  FInfoExpected:=True;
  FTP.List(FileName);
end;

initialization
  {$I main.lrs}

end.

