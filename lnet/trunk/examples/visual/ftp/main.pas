unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, LCLType,
  LNetComponents, ComCtrls, ExtCtrls, Buttons, StdCtrls, Menus, FileCtrl, IniFiles,
  ActnList, Grids;

type
  TIconRec=record
    Bmp: TBitmap;
    Ext: String; // comma separated string of extensions supported by icon
                 // ex. .zip,.rar,.tar.gz
  end;
  TParserResult=(
    prOK,        // directory listing entry (DLE) was parsed successful
    prError,     // DLE was recognized but an error occur while parsing
    prNoImp      // there is no parser for this DLE
  );

type

  { TMainForm }

  TMainForm = class(TForm)
    accSiteManager: TAction;
    accConnect: TAction;
    accDisconnect: TAction;
    ActionList1: TActionList;
    LeftView: TFileListBox;
    MenuItem1: TMenuItem;
    MenuSiteManager: TMenuItem;
    Panel1: TPanel;
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
    MainMenu: TMainMenu;
    MenuItemFile: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemAbout: TMenuItem;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    SBar: TStatusBar;
    rmtGrid: TStringGrid;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    procedure AboutMenuItemClick(Sender: TObject);
    procedure accConnectExecute(Sender: TObject);
    procedure accDisconnectExecute(Sender: TObject);
    procedure accSiteManagerExecute(Sender: TObject);
    procedure DeletePopupClick(Sender: TObject);
    procedure ExitMenuItemClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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
    procedure rmtGridCompareCells(Sender: TObject; Acol, ARow, Bcol,
      BRow: Integer; var Result: integer);
    procedure rmtGridDblClick(Sender: TObject);
    procedure rmtGridDrawCell(Sender: TObject; Col, Row: Integer; aRect: TRect;
      aState: TGridDrawState);
    procedure rmtGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure ToolBar1Click(Sender: TObject);
  private
    FLastN: Integer;
    FList: TStringList;
    FFile: TFileStream;
    FInfoExpected: Boolean;
    FDLSize: Int64;
    FDLDone: Int64;
    FIcons: array of TIconRec;
    CreateFilePath: string;
    FTmpStrList: TStringList;
    procedure DoList(const FileName: string);
    procedure UpdateSite;
    function CurrentName: string;
    function CurrentNameLink: string;
    function CurrentSize: Int64;
    function CurrentIsDirectory: boolean;
    function CurrentIsLink: boolean;
    function GetIconIndexObj(aName: string): TObject;
    function IndexOfExt(aExt: string):Integer;
    procedure ChangeDirectory(aDir: string);
    procedure Disconnect(ClearLog: boolean);

    { private declarations }
  public
    { public declarations }
    procedure RegisterExt(const LazResName,FileExt:string);
  end; 

var
  MainForm: TMainForm;

implementation

uses SitesUnit, DLEParsers;

var
  Dir: string;
  itDirUp   : TObject;
  itDir     : TObject;
  itLink    : TObject;
  itError   : TObject;
  itFile    : TObject;
  

function RevPos(const substr,str:string): integer;
var
  i,j: Integer;
begin
  result := 0;
  j:=Length(SubStr);
  if (j>0)and(j<=Length(Str)) then begin
    for i:=Length(Str) downto 1 do
      if Str[i]<>SubStr[j] then
        exit
      else begin
        dec(j);
        if j=0 then
          break;
      end;
    result := i;
  end;
end;


{ TMainForm }

procedure TMainForm.ExitMenuItemClick(Sender: TObject);
begin
  MainForm.Close;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  FTmpStrList.Free;
  for i:=0 to Length(FIcons)-1 do
    if FIcons[i].Bmp<>nil then
      FIcons[i].bmp.Free;
  SetLength(FIcons, 0);
end;

procedure TMainForm.FTPConnect(Sender: TLFTPClient);
var
  aName, Pass: string;
begin
  aName := Site.User;
  if (aName='') and not InputQuery('Name', 'Please type in your username',
    False, aName)
  then
    exit;
  Pass := Site.Pass;
  if (Pass='') then
    Pass:=PasswordBox('Password', 'Please type in your password');

  FTP.Authenticate(aName, Pass);
  FTP.Binary:=True;
  
  aName:=Site.path;
  if aName='/' then
    aName:='';
    
  DoList(aName);
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
  if assigned(aSocket) then
    if not aSocket.Connected then begin
      // connection has been closed, update gui
      Disconnect(false);
      ToolButton2.Down:=True;
    end;
  CreateFilePath:='';
end;

procedure TMainForm.FTPReceive(Sender: TLFTPClient);
  procedure FindNames;
  var
    i, j, n: Integer;
    Parser: TDirEntryParser;
    IndxObj: TObject;
    k: boolean;
  begin
    rmtGrid.BeginUpdate;
    try
      // each row encode information about file type in objects[] array
      // col 0 default entry type (icon)
      // col 2 specific entry type (icon)
      rmtGrid.RowCount := 2;
      rmtGrid.Cells[1,1] := '..';
      rmtGrid.objects[0,1] := itDirUp;
      
      if FList.Count > 0 then begin
        rmtGrid.RowCount := FList.Count + 2;
        FList.SaveToFile('last.txt');
        for i:=0 to FList.Count-1 do begin
          n:=i+2;
          rmtGrid.Objects[2,n] := nil; // no special icon index

          Parser := DirParser.Parse(pchar(FList[i]));
          if Assigned(Parser) then begin
            DirParser.PrefParser := Parser;
            
            // default icon index/entry type
            if Parser.IsLink then
              IndxObj := itLink
            else if Parser.IsDir then
              IndxObj := itDir
            else begin
              IndxObj := itFile;
              rmtGrid.Objects[2,n] := GetIconIndexObj(Parser.EntryName);
            end;
            // text properties
            rmtGrid.Cells[1, n] := Parser.EntryName;
            if Parser.IsDir or Parser.IsLink then
              rmtGrid.Cells[2, n] := ''
            else
              rmtGrid.Cells[2, n] := IntToStr(Parser.EntrySize);
            rmtGrid.Cells[3, n] := FormatDateTime(
              ShortDateFormat+' '+ShortTimeFormat, Parser.Date);
            rmtGrid.Cells[4, n] := Parser.Attributes;
            rmtGrid.Cells[5, n] := Parser.LinkName;
          end else begin
            rmtGrid.Cells[1, n] := FList[i];
            IndxObj := itError;
          end;
          rmtGrid.Objects[0, n] := IndxObj;
        end;
        
        if FList.Count>1 then
          rmtGrid.SortColRow(True, 0, 1, rmtGrid.RowCount-1);
        
      end;
    finally
      rmtGrid.EndUpdate;
    end;
  end;

var
  s: string;
  i: Integer;
  Buf: array[0..65535] of Byte;
begin
  if FInfoExpected then begin
    s:=Sender.GetDataMessage;
    if Length(s) > 0 then begin
      FList.Text:=FList.Text + s;
    end else begin
      FindNames;
      FInfoExpected:=False;
      FList.Clear;
    end;
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
      n:=Integer(Round(FDLDone / FDLSize * 100));
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

procedure TMainForm.AboutMenuItemClick(Sender: TObject);
begin
  MessageDlg('lFTP test program copyright (c) 2005-2006 by Ales Katona. All rights deserved :)',
             mtInformation, [mbOK], 0);
end;

procedure TMainForm.accConnectExecute(Sender: TObject);
begin
  FTP.Connect(Site.Host, Word(StrToInt(Site.Port)));
  ToolButton4.Down := True;
end;

procedure TMainForm.accDisconnectExecute(Sender: TObject);
begin
  Disconnect(true);
end;

procedure TMainForm.accSiteManagerExecute(Sender: TObject);
var
  F: TFrmSites;
  Res: TModalResult;
begin
  F := TFrmSites.Create(Self);
  try
    Res := F.ShowModal;
    if Res<>mrCancel then begin
      UpdateSite;
      if Res=mrYes then
        accConnectExecute(Self);
    end;
  finally
    F.Free;
  end;
end;

procedure TMainForm.DeletePopupClick(Sender: TObject);
begin
  FTP.DeleteFile(CurrentName);
  DoList('');
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Dir:=ExtractFilePath(ParamStr(0));
  FInfoExpected:=False;
  FList:=TStringList.Create;
  FFile:=nil;
  LeftView.Mask:='*';
  LeftView.Directory:=Dir;
  CreateFilePath:='';
  TFrmSites.LoadLastSite;
  UpdateSite;
  FTmpStrList:=TStringList.Create;
  // register our special icons
  RegisterExt('ftp_dirup',  '1');
  RegisterExt('ftp_dir',    '2');
  RegisterExt('ftp_link',   '3');
  RegisterExt('ftp_file',   '4');
  RegisterExt('ftp_error',  '5');
  // register additional icons
  RegisterExt('ftp_archive','.zip,.gz,.rar,.tar,.bz2');
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
  s := CurrentName;
  FDLSize := CurrentSize;
  FDLDone:=0;
  FreeAndNil(FFile); // if the last get failed, we need to free memory here
  CreateFilePath:=Dir + s;
  FTP.Retrieve(s);
end;

procedure TMainForm.IPEditKeyPress(Sender: TObject; var Key: char);
begin
  if Byte(Key) in [VK_EXECUTE, VK_RETURN] then
    accConnectExecute(Sender);
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
  DoList(CurrentName);
end;

procedure TMainForm.RenamePopupClick(Sender: TObject);
var
  aName: string;
begin
  if InputQuery('New Name', 'Please type in new filename', False, aName) then begin
    FTP.Rename(CurrentName, aName);
    DoList('');
  end;
end;

procedure TMainForm.rmtGridCompareCells(Sender: TObject; Acol, ARow, Bcol,
  BRow: Integer; var Result: integer);
var
  A,B: Integer;
begin
  if aCol=0 then begin
    // lets do a simple sort, Dirs->Files->Alphabetic order
    A := ptrInt(rmtGrid.Objects[ACol,ARow]);
    B := ptrInt(rmtGrid.Objects[BCol,BRow]);
    result := A-B;
    if Result=0 then begin
      // both rows are of the same kind
      // do it alphabetically
      result := CompareText(rmtGrid.Cells[1,ARow],rmtGrid.Cells[1,BRow]);
    end;
  end else
  if aCol=1 then begin
    Result := CompareText(rmtGrid.Cells[ACol,ARow],rmtGrid.Cells[ACol,BRow]);
  end;
end;

procedure TMainForm.rmtGridDblClick(Sender: TObject);
var
  item: string;
begin
  item := CurrentName;
  if CurrentIsDirectory then
    ChangeDirectory(Item)
  else if CurrentIsLink then
    ChangeDirectory(CurrentNameLink)
  else
  if rmtGrid.Objects[0, rmtGrid.Row]<>itError then begin
    FreeAndNil(FFile);
    CreateFilePath:=Dir+item;
    FTP.Retrieve(item);
  end;
end;

procedure TMainForm.rmtGridDrawCell(Sender: TObject; Col, Row: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  i: Integer;
begin
  if (Row>0)and(Col=0) then begin
    i := PtrInt(rmtGrid.Objects[2,Row]);
    if i=0 then
      i := PtrInt(rmtGrid.Objects[0,Row])-1;
    if i<Length(FIcons) then
      rmtGrid.Canvas.Draw(aRect.Left+2,aRect.Top+2, FIcons[i].Bmp);
  end else
    rmtGrid.DefaultDrawCell(Col,Row,aRect,aState);
end;

procedure TMainForm.rmtGridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key=VK_BACK then begin
    ChangeDirectory('..');
    key := 0;
  end else
  if Key=VK_RETURN then begin
    rmtGridDblClick(Sender);
    Key:=0;
  end;
end;

procedure TMainForm.ToolBar1Click(Sender: TObject);
  procedure Test(s:string);
  var
    res:boolean;
    Parser: TDirEntryParser;
  begin
    Parser := DirParser.Parse(pchar(s));
    WriteLn('Testing dir  =',s);
    if Parser<>nil then begin
      writeLn('Parser       =',Parser.Description);
      Write('    ');

      if Parser.IsDir then  Write('Dir=') else
      if Parser.IsLink then Write('Link=')
      else                  Write('File=');

      Write(Parser.EntryName);
      if Parser.IsLink then
        Write(' -> ', Parser.LinkName);
      Write(' Size=', Parser.EntrySize);
      WriteLn(' attr=',Parser.Attributes);
    end else
      WriteLn('Parser       = Can''t find a suitable parser');
  end;
begin
  Test('total 24');
  Test('-rw-rw-r--   1 53       53           2024 May 29  2006 index.html');
  Test('drwxrwxr-x   8 53       53           4096 Sep  8 01:29 lazarus');
  Test('lrwxrwxrwx   1 root     other           7 Jul 15  1997 README -> WELCOME');
  Test('lrwxrwxrwx   1 root     other          13 Jun 04  1998 ls-lR -> IAFA-LISTINGS');
end;

procedure TMainForm.DoList(const FileName: string);
begin
  FInfoExpected:=True;
  FTP.List(FileName);
end;

procedure TMainForm.UpdateSite;
begin
  SBar.Panels[0].Text := Site.site;
  SBar.Panels[1].Text := Site.user;
  if Site.Host<>'' then
    SBar.Panels[2].Text := Site.Host+GetSitePath
  else
    SBar.Panels[2].Text := '';
end;

function TMainForm.CurrentName: string;
begin
  result := rmtGrid.Cells[1, rmtGrid.Row];
end;

function TMainForm.CurrentNameLink: string;
begin
  result := rmtGrid.Cells[5, rmtGrid.Row];
end;

function TMainForm.CurrentSize: Int64;
begin
  result := StrToInt64Def(rmtGrid.Cells[2, rmtGrid.Row], 0);
end;

function TMainForm.CurrentIsDirectory: boolean;
begin
  result :=
    (rmtGrid.Objects[0, rmtGrid.Row]=itDirUp) or
    (rmtGrid.Objects[0, rmtGrid.Row]=itDir);
end;

function TMainForm.CurrentIsLink: boolean;
begin
  result :=
    (rmtGrid.Objects[0, rmtGrid.Row]=itLink);
end;

function TMainForm.GetIconIndexObj(aName: string): TObject;
var
  i,j: Integer;
begin
  result:=nil;
  aName:=lowercase(aName);
  for i:=5 to Length(FIcons)-1 do begin
    FtmpStrList.CommaText := FIcons[i].Ext;
    for j:=0 to FtmpStrList.Count-1 do
      if RevPos(FtmpStrList[j],aName)<>0 then begin
        result := TObject(PtrInt(i));
        exit;
      end;
  end;
end;

function TMainForm.IndexOfExt(aExt: string): Integer;
var
  i: Integer;
begin
  result := -1;
  for i:=5 to Length(FIcons)-1 do
    if CompareText(aExt, FIcons[i].Ext)=0 then begin
      result := i;
      break;
    end;
end;

procedure TMainForm.ChangeDirectory(aDir: string);
begin
  // todo: implement refresh
  // todo: implement quick parent directory
  if aDir='..' then begin
    FTP.ChangeDirectory(aDir);
    DoList('');
  end else begin
    FTP.ChangeDirectory(aDir);
    DoList('');
  end;
end;

procedure TMainForm.Disconnect(ClearLog: boolean);
begin
  FTP.Disconnect;
  rmtGrid.RowCount:=1;
  if ClearLog then
    MemoText.Clear;
end;

procedure TMainForm.RegisterExt(const LazResName,FileExt: string);
var
  i: LongInt;
begin
  // check if not exists already
  i := IndexOfExt(FileExt);
  if (i<0) and (LazarusResources.Find(LazResName)<>nil) then begin
    i := Length(FIcons);
    SetLength(FIcons, i+1);
    FIcons[i].Bmp := TBitmap.Create;
    FIcons[i].Bmp.LoadFromLazarusResource(LazResName);
    FIcons[i].Ext := lowercase(FileExt);
  end;
end;

initialization
  itDirUp:= TObject(ptrInt(1));
  itDir  := TObject(ptrInt(2));
  itLink := TObject(ptrInt(3));
  itFile := TObject(ptrInt(4));
  itError:= TObject(ptrInt(5));
  {$I main.lrs}
  {$I icons.lrs}
  //add additional icons in a new file iconsextra.lrs
  //register an extensiona and icon with function RegisterExt()
  {$I iconsextra.lrs}

end.

