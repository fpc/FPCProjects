{--------------------------------------------------------------------------}
{                         TechnoJock's Turbo Toolkit                       }
{                                                                          }
{                              Version   5.10                              }
{                                                                          }
{                                                                          }
{               Copyright 1986-1993 TechnoJock Software, Inc.              }
{                           All Rights Reserved                            }
{                          Restricted by License                           }
{--------------------------------------------------------------------------}

          {--------------------------------}
          {       Unit:   DirTTT5          }
          {--------------------------------}


{$S-,R-,V-}
{$IFNDEF DEBUG}
{$D-}
{$ENDIF}


unit  DirTTT5;

{Change History:    04/01/89  Changed logic if no file found and DIRFULL
              is false - Line 1174
       5.01A  05/18/89  Added line to allow only directores to be displayed.
              Line 923
     Special  05/25/89  Changed memory disposal scheme
       5.01b  07/23/89  Removed a '+' from string expression to enable compile
              with Quick Pascal (!)
             5.02a  01/23/90  Added check for AllowEsc
             5.02b  04/04/90  Corrected pointer problem on empty directories
             5.02c  09/17/90  Corrected pointer problem on empty directories
             5.10   01/04/93  DPMI compatible version
}

(*
{$DEFINE DIRFULL}
*)

INTERFACE

Uses DOS,CRT,FastTTT5,WinTTT5,StrnTTT5,KeyTTT5,ReadTTT5;

Const
   DHelpKey = #187;                     {Alter these keys if desired.       }
   DHelpStr:string[2] = 'F1';           {Note: to disable these keys, set   }
   DToggleKey = #32;                    {      appropriate flags in Var D.   }
   DToggleStr: string[5] = 'Space';
   DZoomKey = #172;
{$IFDEF DIRFULL}
   DZoomStr: string[5] = 'Alt-Z';
   DJumpParentKey = #176;
   DJumpParentStr: string[5] = 'Alt-B';
   DChangeDirKey = #174;
   DChangeDirStr: string[5] = 'Alt-C';
   DSortOrderKey = #152;
   DSortOrderStr: string[5] = 'Alt-O';
   DSortSizeKey = #159;
   DSortSizeStr: string[5] = 'Alt-S';
   DSortNameKey = #177;
   DSortNameStr: string[5] = 'Alt-N';
   DSortExtKey = #146;
   DSortExtStr: string[5] = 'Alt-E';
   DSortTimeKey = #148;
   DSortTimeStr: string[5] = 'Alt-T';
   DSortDOSKey = #160;
   DSortDOSStr: string[5] = 'Alt-D';
   DSortDos  = 1;
   DSortName = 2;
   DSortExt  = 3;
   DSortSize = 4;
   DSortTime = 5;
   Ascending = 1;
   Descending = 2;
{$ENDIF}

Type
   DirDisplay = record
          TopX       : byte;
          TopY       : Byte;
          Rows       : byte;
          Attrib     : byte;
          BoxType    : byte;
          BoxFCol    : byte;
          BoxBCol    : byte;
          KeyFCol    : byte;
          BacCol     : byte;
          NorFCol    : byte;
          DirFCol    : byte;
          HiFCol     : byte;
          HiBCol     : byte;
          AllowEsc   : boolean;
          ShowDetails: boolean;
          Colswide   : byte;
          DisplayInfo: boolean;
          RestoreScreen : boolean;
          AllowHelp     : boolean;
          AllowToggle   : boolean;
          AllowZoom     : boolean;
          ZoomLine      : byte;
          AllowSort     : boolean;
          InitSort      : byte;
          Asc           : byte;
          AllowCD       : boolean;
          SelectDir     : boolean;
          AllowInput    : boolean;
      end;

Var
   DTTT : DirDisplay;
   NoMemory : boolean;

Function Display_Directory(DIRFULLFileName: StrScreen;var Retcode:integer): StrScreen;
Procedure Default_Settings;

IMPLEMENTATION


Procedure Default_Settings;
begin
    With  DTTT  do
    begin
  TopX    := 0;
  TopY    := 0;
  Rows    := 0;
  AllowEsc := true;
  Attrib := Readonly + Directory + Archive;
  BoxType := 1;
  ShowDetails := true;
  ColsWide := 5;
{$IFDEF DIRFULL}
  DisplayInfo := true;
  AllowHelp := true;
  AllowZoom   := true;
  ZoomLine := 25;
  AllowSort := true;
  InitSort := DSortDOS;     {sort in DOS order}
  AllowInput := True;
{$ELSE}
  DisplayInfo := false;
{$ENDIF}
  AllowCD := true;
  SelectDir := false;
  RestoreSCreen := true;
  AllowToggle := true;
  Asc := 1;
  If not ColorScreen then
  begin
      BoxFCol := white;
      BoxBCol := black;
      KeyFCol := white;
      BacCol := black;
      NorFCol := white;
      DirFCol := lightgray;
      HiFcol := black;
      HiBcol := lightgray
  end
  else
  begin
      BoxFCol := lightgray;
      BoxBCol := blue;
      KeyFCol := yellow;
      BacCol := black;
      NorFCol := white;
      DirFCol := yellow;
      HiFcol := black;
      HiBcol := cyan;
  end;
    end; {with}
end;



Function Display_Directory( DIRFULLFilename: StrScreen; var Retcode : integer): StrScreen;
{
        X1                                    X2
     Y1 >      _____________________________________
        |                                     | >
        |                                     | >  Infodepth
        |                                     | >
     Y2 >     |_____________________________________| >
        |                                     |
        |                                     |
        |                                     |
        |                                     |
        |                                     |
        |                                     |
     Y3 >     |_____________________________________|


    Retcodes >    0  -  filechosen
        1  -  user escaped
        2  -  not enough memory
        3  -  no files matching
        99 -  unexpected error

}
Type
  FRptr = ^FR;
  FR = record
      Name : string[8];
      Ext  : string[3];
      Size : longint;
      Time : longint;
      Attr : byte;
      Fn : integer;
      PrevFR: FRptr;
      NextFR : FRptr;
       end;

const
  OKCode = 0;           {ret codes}
  EscCode = 1;
  MemCode = 2;
  NofilesCode = 3;
  UnKnownCode = 99;
  InfoDepth = 4;        {no of lines in information box, i.e.Y1 to Y2}
  ReadMsg = 'Reading files';
  SortMsg = 'Sorting files';
  NoneMsg = 'No files ... ';
var
  X1,X2,Y1,Y2,Y3,R,Y3_Unzoomed : byte;{box dimensions}
  StartDir : StrScreen;      {default directory}
  ColumnsWide : byte;
  TopFn : integer;           {file number of top file in the display}
  BotFn : integer;           {file number of bottom file in the display}
  HiFn  : integer;           {file number of hilighted file}
  Zoomed: boolean;           {is file display extended to bottom of screen}
  ShowingDetails : boolean;
  PathName : StrScreen;      {the path section of filename}
  FileMask : StrScreen;
  FirstFile : FRptr;
  List_End : FRptr;
  ChosenFile: strscreen;
  TotalFiles: word;
  TotalDirs : word;
  TotalBytes: LongInt;
  Ftemp : FRPtr;
  Scrn : pointer;
  CursRec : array[1..4] of byte;
  SortOrder : byte;               {1-DOS, 2-Name, 3-Ext, 4-Size, 5-Time}
  SortAsc : boolean;

    Function Subdirectory(B : byte):boolean;
    begin
  Subdirectory := ((B and Directory) = Directory);
    end;

    Function FileAttribs(B:byte):StrScreen;
    var
      S : StrScreen;
    begin
  S := '    ';
  If ((B and ReadOnly) = Readonly) then
     S[1] := 'R';
  If ((B and Hidden) = Hidden) then
     S[2] := 'H';
  If ((B and SysFile) = SysFile) then
     S[3] := 'S';
  If ((B and Archive) = Archive) then
     S[4] := 'A';
  FileAttribs := S;
     end;

     Function LongFileDesc(F:FRptr):StrScreen;
     var
       DT :datetime;
       S  : StrScreen;
     begin
   If ShowingDetails then
   begin
       with F^ do
       begin
      UnPackTime(Time,DT);
      With DT do
      begin
          If Ext = '' then
        S := Padleft(Name,12,' ')
          else
        S :=  Padleft(Name+'.'+Ext,12,' ');                 {start with name}
          If Subdirectory(Attr) then                  {add file size}
        S := S + Padright('<DIR>',8,' ')
          else
        S := S + Padright(Int_to_Str(Size),8,' ');
          S := S + '    ';
          Case Month of                               {add month}
          1 : S := S + 'Jan ';
          2 : S := S + 'Feb ';
          3 : S := S + 'Mar ';
          4 : S := S + 'Apr ';
          5 : S := S + 'May ';
          6 : S := S + 'Jun ';
          7 : S := S + 'Jul ';
          8 : S := S + 'Aug ';
          9 : S := S + 'Sep ';
          10: S := S + 'Oct ';
          11: S := S + 'Nov ';
          12: S := S + 'Dec ';
          end;
          S :=   S                                   {add the day,year}
          + Padright(Int_to_Str(Day),2,'0')
          + ','
          + Int_to_Str(Year)
          + '    ';
          If Hour > 12 then                          {add a/p time}
        S :=  S
             +Padright(Int_to_Str(Hour-12),2,' ')
             +':'
             +Padright(Int_to_Str(Min),2,'0')
             +'p'
          else
        S :=  S
              +Padright(Int_to_Str(Hour),2,' ')
              +':'
              +Padright(Int_to_Str(Min),2,'0')
              +'a';
        S := S + '  '+FileAttribs(Attr);
      end;   {with DT}
       end; {with F^}
   end
   else    {not one column}
    If F^.Ext = '' then
       S := Padleft(F^.Name,12,' ')
    else
       S := Padleft(F^.Name+'.'+F^.Ext,12,' ');
   LongFileDesc := S;
     end;

    Function PathSlash(S : StrScreen):StrScreen;
    begin
  If S[length(S)] <> '\' then
     S := S + '\';
  PathSlash := S;
    end;  {Sub Func PathSlash}

    Function PathNoSlash(S : StrScreen):StrScreen;
    begin
  If S[length(S)] = '\' then
     Delete(S,length(S),1);
  PathNoSlash := S;
    end;  {Sub Func PathSlash}

    Function PathParent(S : StrScreen):StrScreen;
    var P1 : byte;
    begin
  S := PathNoSlash(S);
  P1 := LastPos('\',S);
  PathParent := copy(S,1,P1);
    end;

    Function PathChild(S : StrScreen):StrScreen;
    begin
  PathChild := PathSlash(PathName + S);
    end;

    Procedure Extract_Path_Mask;
    var P1,P2 : byte;
    begin
  P1 := LastPos('\',DIRFULLFileName);
  P2 := Pos(':',DIRFULLFilename);
  If (P1 = 0) and (P2 = 0) then
  begin
      FileMask := DIRFULLFileName;
      PathName := PathSlash(StartDir);
      exit;
  end;
  If P1 = length(DIRFULLFileName) then
  begin
      FileMask := '*.*';
      PathName := DIRFULLFileName;
      exit;
  end;
  If (P1 = 0) and (P2 = 2) then   { x:filename.ext}
  begin
     Filemask := copy(DIRFULLFileName,3,length(DIRFULLFileName));
     {$I-}
     GetDir(ord(upcase(DIRFULLFileName[1]))-64,PathName);
     {$I-}
     If IOResult <> 0 then
        PathName := PathSlash(StartDir)
     else
        PathName := PathSlash(PathName);
     exit;
  end;
  Filemask := copy(DIRFULLFileName,succ(P1),12);
  PathName := copy(DIRFULLFileName,1,P1);
    end;  {Extract_Path_Mask}

    Procedure LoadFiles(Mask:StrScreen;Attrib:byte);
    var
      Finfo : SearchRec;
      Recsize : word;

      Procedure PushOnHeap(var F:FrPtr);
      var P : byte;
      begin
    with F^ do
    begin
        Attr := Finfo.Attr;
        Time := Finfo.Time;
        Size := Finfo.Size;
        If FInfo.Name = '..' then
        begin
       Name := '..';
       Ext := '';
        end
        else
        begin
       P := pos('.',Finfo.Name);
       If P = 0 then
       begin
           Name := Finfo.Name;
           Ext := '';
      end
      else
      begin
          Name := copy(FInfo.Name,1, pred(P));
          Ext := copy(Finfo.Name,succ(P),3);
      end;
        end;
        Fn := succ(TotalFiles);
        NextFR := nil;
        PrevFr := nil;
        TotalBytes := TotalBytes + Size;
    end;
    Inc(TotalFiles);
    If Finfo.Attr = Directory then
       Inc(TotalDirs);
      end;   {sub sub proc TransferFileToHeap}

      Procedure AllocHeap;
      begin
    If ( (Attrib = Directory) and (FInfo.Attr <> Directory) ) then
       exit;   {if only looking for directory entries}
    If (Finfo.Name <> '.') and (DosError = 0) then
    begin
        If (TotalFiles = 0) then
        begin
       PushOnHeap(FirstFile);
       FirstFile^.PrevFR := nil;
       Ftemp :=  FirstFile;
       List_End := FirstFile;
        end
        else
        begin
       GetMem(Ftemp^.NextFR,Recsize);
       PushOnHeap(FTemp^.NextFr);
       FTemp := Ftemp^.NextFR;
       FTemp^.PrevFR := List_End;
       List_End := Ftemp;
        end; {If TotalFiles = 0}
   end; { If name <> '.'}
      end;

    begin
  RecSize := Sizeof(FirstFile^);
{$ifndef fpc}
  If MaxAvail < 2*Recsize then
  begin
      NoMemory := true;
      exit;
  end;
{$endif not fpc}
  Fastwrite(X1+2,Y2+1,attr(DTTT.NorFcol+blink,DTTT.BacCol),ReadMsg);
  FindFirst(PathName+Mask,Attrib,Finfo);
  If DosError <> 0 then
     exit;
  If TotalFiles = 0 then
  begin
     GetMem(FirstFile,RecSize);
           FirstFile^.NextFr := nil; {5.02c}
  end;
  AllocHeap;
  While (DosError = 0) and (NoMemory = false) do
  begin
{$ifndef fpc}
      If MaxAvail < RecSize then
         NoMemory := true
      else
{$endif not fpc}
      begin
     FindNext(Finfo);
     AllocHeap;
      end; {If MaxAvail}
  end; {while}
    end; {Sub Proc Loadfiles}

    Procedure UnLoadFiles;
    {runs down the linked list and FREES all the data
    on the heap}
    var
       Temp1,
       Temp2 : FRPtr;
    begin
       Temp1 := FirstFile;
       while Temp1 <> Nil do
       begin
    Temp2 := Temp1;
    Temp1 := Temp1^.NextFr;
    FreeMem(Temp2,sizeof(Temp2^));
       end;
       FirstFile := nil;   {5.02c}
    end; {of proc UnLoadFiles}


    Procedure Calculate_Box_Dimensions;
    var
      Boxwidth : byte;
    begin
  If ShowingDetails then
     Boxwidth := 54
  else
     Boxwidth := succ(DTTT.Colswide*14);
  with DTTT do
  begin
      If (TopX < 1) or (TopX > 80) then
         X1 :=  (80 - Boxwidth) div 2
      else
      begin
         If TopX <= (80 - Boxwidth) then
       X1 := TopX
         else                               {move box left until it fits}
       X1 := 80 - Boxwidth;
      end;
      X2 := X1 + Boxwidth;
      If Rows in [1..23] then
         R := Rows
      else
         R := 8;
      If (TopY < 1) or (TopY > DisplayLines - 2) then
         Y1 := 5
      else
         Y1 := TopY;
      If not DisplayInfo then
         Y2 := Y1
      else
      begin
     If Y1 + InfoDepth < DisplayLines - 2 then
        Y2 := Y1 + InfoDepth
     else                               {no room for info}
        Y2 := Y1;
      end;
      Y3 := Y2 + succ(R);
      If Y3 > DisplayLines then
      begin
         Y3 := DisplayLines;
         If Y2 <> Y1 then
         begin
        Y2 := Y3 - succ(R);
        Y1 := Y2 - InfoDepth;
         end
         else
         begin
        Y2 := Y3 - succ(R);
        Y1 := Y2;
         end;
      end;
  end;
    end;  {sub proc Calculate_Box_Dimensions}

    Procedure Display_Box;
    var
      LChar,Rchar : char;
      Col,
      I : integer;
    begin
  with DTTT do
  begin
      If Y2 = Y1 then
         ClearText(X1,Y1,X2,Y3,NorFCol,Baccol)
      else
      begin
     ClearText(X1,Y1,X2,pred(Y2),BoxFCol,BoxBcol);
     ClearText(X1,Y2,X2,Y3,NorFCol,Baccol);
      end;
      Col := attr(BoxFcol,BoxBCol);
      If (BoxType in [5..9]) then
      begin
     Box(X1,Y1,X2,Y3,BoxFcol,BoxBcol,Boxtype-5);
     If (X2 < 80) and (Y3 < DisplayLines) then
     begin
         For I := succ(Y1) to succ(Y3) do
        Fastwrite(succ(X2),I,ShadColor,chr(219));
         Fastwrite(succ(X1),succ(Y3),ShadColor,replicate(X2-X1,chr(219)));
     end;
      end
      else
         Box(X1,Y1,X2,Y3,BoxFcol,BoxBcol,Boxtype);
      If Y2 > Y1 then
      begin
     Horizline(succ(X1),pred(X2),Y2,BoxFCol,BoxBcol,Boxtype);
     Case Boxtype of
     1,6 : begin
          LChar := chr(195);
          RChar := chr(180);
           end;
     2,7 : begin
          LChar := chr(204);
          RChar := chr(185);
           end;
     3,8 : begin
          LChar := chr(199);
          RChar := chr(182);
           end;
     4,9 : begin
          LChar := chr(181);
          RChar := chr(198);
           end;
     else      Lchar := ' ';
          Rchar := ' ';
     end;  {case}
     Fastwrite(X1,Y2,Col,Lchar);
     Fastwrite(X2,Y2,Col,Rchar);
      end;
  end;
    end;  {sub proc display box}

    Procedure DisplayPath;
    var
       L : byte;
       Y : byte;
       P : StrScreen;
    begin
  P := Pathname+Filemask;
  L := length(P);
  If Y2 = Y1 then
  begin
     Y := Y1;
     If L > (X2-X1-2) then
        P := chr(17)+copy(P,L-(X2-X1)+4,L);
  end
  else
  begin
     Y := Y1 + 2;
     If L > (X2-X1-2) then
        P := chr(17)+copy(P,L-(X2-X1-1)+4,L);
  end;
  Fastwrite(X1+2,Y,attr(DTTT.BoxFcol,DTTT.BoxBCol),P);
    end;  {sub Proc DisplayPath}


    Procedure FillInfo;
    var
      TB,Di : StrScreen;
      C,H,L  : byte;
    begin
  with DTTT do
  begin
      C := attr(BoxFCol,BoxBCol);
      H := attr(KeyFcol,BoxBCol);
      If (Y2 = Y1) then
      begin
     DisplayPath;
     exit;
      end;
{$IFDEF DIRFULL}
      If  (ColumnsWide < 3 ) and (ShowingDetails = false) then
      begin
     DisplayPath;
     Fastwrite(X1+2,Y1+1,H,chr(17)+char(217));
     Fastwrite(X1+5,Y1+1,C,'Select');
     Fastwrite(X1+2,Y1+3,C,'Files: ');
     Fastwrite(X1+9,Y1+3,C,Int_To_Str(TotalFiles-TotalDirs));
     exit;
      end;
      ClearText(succ(X1),Succ(Y1),pred(X2),Pred(Y2),BoxFcol,BoxBCol);
      Fastwrite(X1 + 2,Y1 + 3,C,'Matching files: ');
      Fastwrite(X1 + 18,Y1 + 3,C,Int_To_Str(TotalFiles-TotalDirs));
      TB := 'Total bytes: '+Int_To_Str(TotalBytes);
      Fastwrite(X2 -length(TB) - 1,Y1 + 3,C,TB);
      If AllowHelp then
      begin
     Fastwrite(X1+2,Y1+1,H,DHelpStr);    {Prompt at left}
     Fastwrite(X1+3+length(DHelpStr),Y1+1,C,'Help');
      end;
      L := pred(X1)
         + ((X2-X1) div 2)
         - (length(DToggleStr)+ 7) div 2;     {next prompt in center}
      Fastwrite(L,Y1+1,H,chr(17)+char(217));
      L := L + 3;
      Fastwrite(L,Y1+1,C,'Select');
      If AllowToggle then
      begin
     L := X2 - length(DToggleStr) - 8;   {right justified}
     Fastwrite(L,Y1+1,H,DToggleStr);
     L := L + 1 + length(DToggleStr);
     Fastwrite(L,Y1+1,C,'Toggle');
      end;
  end;
  DisplayPath;
{$ELSE}
       end;
{$ENDIF}
    end;  {sub proc Fillinfo}

    Function FilePointer(Fn:word): FRptr;
    {MODIFY to go from current pointer - for speed}
    var
      P : FRptr;
      I : integer;
    begin
  If  SortAsc then
  begin
      P := FirstFile;
      If Fn > 1 then
         For I := 2 to Fn do
        P := P^.NextFr;
  end
  else {Descending}
  begin
      P := List_End;
      If Fn > 1 then
         For I := 2 to Fn do
        P := P^.PrevFr;
  end;
  FilePointer := P;
    end;  {sub proc filepointer}

    Function Y_Coord(Fn : word):byte;
    begin
   Y_Coord := Succ(Y2) + ((Fn-TopFn) DIV ColumnsWide);
    end;

    Function X_Coord(Fn : word):byte;
    begin
     X_Coord := succ(X1) + 14*((Fn-TopFn) MOD Columnswide);
    end;

    Function TopLine:Boolean;
    begin
    TopLine := (HiFn <= ColumnsWide);
    end;

    Function BottomLine:Boolean;
    begin
    BottomLine := (HiFn + ColumnsWide > TotalFiles);
    end;

    Function FirstColumn:boolean;
    begin
     If Columnswide = 1 then
        FirstColumn := true
     else
        FirstColumn := (HiFn MOD ColumnsWide = 1);
    end;

    Function LastColumn:boolean;
    begin
     LastColumn := (HiFn MOD ColumnsWide = 0);
    end;

    Procedure RecalcTopFn;
    begin
  If ColumnsWide = 1 then
     TopFn := succ(BotFn -R)
  else
      TopFn :=  Succ(   BotFn
            - pred(R)*ColumnsWide
            - BotFn MOD ColumnsWide
          );
    end;

    Procedure RecalcBotFn;
    begin
  BotFn := pred( TopFn + ColumnsWide*R);
  If BotFn > TotalFiles then
     BotFn := TotalFiles;
    end;

    Procedure LolightFile(Fn:word);
    var
      C : byte;
      F : FRptr;
    begin
  If (Fn < TopFn) or (Fn > BotFn ) then
     exit;    {file not in display area}
  F := Filepointer(Fn);
  If Subdirectory(F^.Attr) then
     C := attr(DTTT.DirFcol,DTTT.BacCol)
  else
     C := attr(DTTT.NorFCol,DTTT.BacCol);
  Fastwrite(X_Coord(Fn),
       Y_Coord(Fn),
       C,
       ' '+LongFileDesc(F)+' ');
    end;

    Procedure HilightFile(Fn:word);
    var
      F : FRptr;
    begin
  If (Fn < TopFn) or (Fn > BotFn) then
     exit;    {file not in display area}
  F := Filepointer(Fn);
  Fastwrite(X_Coord(Fn),
       Y_Coord(Fn),
       attr(DTTT.HiFcol,DTTT.HiBCol),
       ' '+LongFileDesc(F)+' ')
    end;

    Function File_name(Fn : word):StrScreen;
    var
       F : FRPtr;
       Fname : strscreen;
    begin
  F := FilePointer(Fn);
  Fname := F^.Name;
  If F^.Ext <> '' then
     Fname := Fname+'.'+F^.Ext;
  File_Name := Fname;
    end;   {Sub Funct File_name}

    Procedure DisplayFiles;
    var
      I : integer;
    begin
  If (Columnswide > 1) and (BotFn = TotalFiles) then    {clear line}
     ClearText(succ(X1),pred(Y3),Pred(X2),pred(Y3),DTTT.NorFcol,DTTT.BacCol);
  For I := TopFn to BotFn do
      If (I <> HiFn) and (I <= TotalFiles) then
         LolightFile(I);
  HiLightFile(HiFn);
    end; {sub proc DisplayFiles}

    Procedure Scroll_Down;
    begin
  TopFn := TopFn + Columnswide;
  RecalcBotFn;
  DisplayFiles;
    end; {scroll_down}

    Procedure Scroll_Up;
    begin
  TopFn := TopFn - Columnswide;
  RecalcBotFn;
  DisplayFiles;
    end; {scroll_up}

    Procedure Scroll_Top;
    begin
  TopFn := 1;
  RecalcBotFn;
  HiFn := 1;
  DisplayFiles;
    end; {scroll_Home}

    Procedure Scroll_Bottom;
    begin
  TopFn := succ(TotalFiles - R);
  BotFn := TotalFiles;
  HiFn := TotalFiles;
  DisplayFiles;
    end; {scroll_Home}


{\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\    SORTING   \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\}
{$IFDEF DIRFULL}

Function Larger(Ptr1,Ptr2: FRptr) : boolean;
var
   N1,N2 : string[8];
   E1,E2 : string[8];
begin
    Case SortOrder of
    DSortDos   : Larger := (Ptr1^.Fn > Ptr2^.Fn);
    DSortNAME  : If Ptr1^.Name = Ptr2^.Name then
         Larger := Ptr1^.Ext > Ptr2^.Ext
      else
         Larger := Ptr1^.Name > Ptr2^.Name;
    DSortEXT   : If Ptr1^.Ext = Ptr2^.Ext then
         Larger := Ptr1^.Name > Ptr2^.Name
      else
         Larger := Ptr1^.Ext > Ptr2^.Ext;
    DSortSIZE  : Larger := (Ptr1^.Size > Ptr2^.Size);
    DSortTIME  : Larger := (Ptr1^.Time > Ptr2^.Time);
    else Larger := false;
    end; {Case}
end; {suc proc larger}

Procedure SwapIt(var Ptr1,Ptr2: FRPtr);
var
   Temp : FR;
   Size : integer;
begin
    Temp := Ptr2^;
    Size := sizeof(Temp) - 8;
    Move(Ptr1^,Ptr2^,Size);
    Move(Temp,Ptr1^,Size);
end;  {sub proc Swap}

Procedure ShellSort;
var
   I,J,Delta : longint;
   Swapped : boolean;
   Ptr1,Ptr2 : FRPtr;

begin
    Delta := TotalFiles div 2;
    repeat
   Repeat
        Swapped := false;
        Ptr1 := FirstFile;
        Ptr2 := Ptr1;
        For I := 1 to Delta do
       Ptr2 := Ptr2^.NextFr;
        For I := 1 to TotalFiles - Delta do
        begin
       If I > 1 then
       begin
           Ptr1 := Ptr1^.NextFr;
           Ptr2 := Ptr2^.NextFr;
       end;
       If Larger(Ptr1,Ptr2) then
       begin
           SwapIt(Ptr1,Ptr2);
           Swapped := true;
       end;
        end;
   Until (not Swapped);
   Delta := delta div 2;
    Until Delta = 0;
end;

           Procedure ReSort;
           begin
           ClearText(succ(X1),Succ(Y2),pred(X2),pred(Y3),DTTT.NorFcol,DTTT.BacCol);
           Fastwrite(X1 + 2,succ(Y2),attr(DTTT.NorFcol+blink,DTTT.BacCol),SortMsg);
           ShellSort;
           TopFn := 1;
           HiFn := 1;
           RecalcBotFn;
           DisplayFiles;
           end;
{$ENDIF}

    Procedure DisplayNewDirectory;
    var A : byte;
    begin
  A := DTTT.attrib and (AnyFile - VolumeID);
  Display_Box;
  TotalFiles := 0;
  TotalBytes := 0;
  TotalDirs  := 0;
  If DTTT.AllowCd or DTTT.SelectDir then
  begin
      If Subdirectory(A) then
      begin
      LoadFiles('*.*',Directory);                {load directory details first}
      If A <> Directory then                     {Fix 5.01A}
         Loadfiles(Filemask,A and (anyfile - Directory));  {then load other files with mask}
      end
      else
      LoadFiles(Filemask,A and (Anyfile - Directory));
  end
  else                  {automatically removed directory type files}
       LoadFiles(Filemask,A and (anyfile - Directory));
  FillInfo;
{$IFDEF DIRFULL}
  If SortOrder <> DSortDOS then
     ShellSort;
{$ENDIF}
  If TotalFiles = 0 then
     Fastwrite(X1+2,Y2+1,attr(DTTT.NorFcol,DTTT.BacCol),NoneMsg)
  else
     Scroll_Top;
    end;  {sub proc DisplayNewDirectory}

{$IFDEF DIRFULL}
    Procedure ShowHelpScreen;
    const
  width = 55;
  depth = 14;
    var
      Str : StrScreen;
      S  : word;
      Sc : pointer;
      X,Y : byte;
      ChH : char;
    begin
  If X1 + width > 80 then
     X := pred((80 - width) div 2)
  else
     X := X1;
  If Y1 + Depth > DisplayLines then
     Y := pred((DisplayLines -Depth) div 2)
  else
     Y := Y1;
  S := 160*DisplayLines;
{$ifndef fpc}
  If MaxAvail < S then
     exit;
{$endif not fpc}
  GetMem(Sc,S);
{$ifdef fpc}
  MoveFromScreen((longint(BaseOfScreen) and $ffff) shl 16,Sc^,S Div 2);
{$else fpc}
  MoveFromScreen(BaseOfScreen^,Sc^,S Div 2);
{$endif fpc}
  FBox(X,Y,pred(X+ width),pred(Y+Depth),DTTT.BoxFCol,DTTT.BoxBCol,1);
  Case SortOrder of
  DSortDos  : Str := ' DOS';
  DSortName : Str := ' NAME';
  DSortExt  : Str := ' EXT';
  DSortSize : Str := ' SIZE';
  DSortTime : Str := ' TIME';
  end; {case}
  If SortAsc then
     Str := Str +' in ASCENDING order'
  else
     Str := Str +' in DESCENDING order';
  If Zoomed then
     Str := Str +' (Zoomed) '
  else
     Str := Str+' (not zoomed) ';
  Str := ' Current: '+Str;
  WriteBetween(X,X + Width,pred(Y)+depth,DTTT.KeyFCol,DTTT.BoxBCol,Str);
  If DTTT.AllowSort then
  begin
      Fastwrite(X+4,Y+2,attr(DTTT.KeyFCol,DTTT.BoxBCol),DSortDOSStr);
      Fastwrite(X+7+length(DSortDOSStr),Y+2,
           attr(DTTT.BoxFCol,DTTT.BoxBCol),
           'sort in native DOS order');
      Fastwrite(X+4,Y+3,attr(DTTT.KeyFCol,DTTT.BoxBCol),DSortNameStr);
      Fastwrite(X+7+length(DSortNameStr),Y+3,
           attr(DTTT.BoxFCol,DTTT.BoxBCol),
           'sort alphabetically by file Name');
      Fastwrite(X+4,Y+4,attr(DTTT.KeyFCol,DTTT.BoxBCol),DSortExtStr);
      Fastwrite(X+7+length(DSortExtStr),Y+4,
           attr(DTTT.BoxFCol,DTTT.BoxBCol),
           'sort alphabetically by file Extension');
      Fastwrite(X+4,Y+5,attr(DTTT.KeyFCol,DTTT.BoxBCol),DSortSizeStr);
      Fastwrite(X+7+length(DSortSizeStr),Y+5,
           attr(DTTT.BoxFCol,DTTT.BoxBCol),
           'sort by file Size');
      Fastwrite(X+4,Y+6,attr(DTTT.KeyFCol,DTTT.BoxBCol),DSortTimeStr);
      Fastwrite(X+7+length(DSortTimeStr),Y+6,
           attr(DTTT.BoxFCol,DTTT.BoxBCol),
           'sort by date/Time of file');
      Fastwrite(X+4,Y+7,attr(DTTT.KeyFCol,DTTT.BoxBCol),DSortOrderStr);
      Fastwrite(X+7+length(DSortOrderStr),Y+7,
           attr(DTTT.BoxFCol,DTTT.BoxBCol),
           'sort in ascending or descending Order');
  end
  else
     WriteBetween(X,X+Width,Y+3,DTTT.BoxFCol,DTTT.BoxBCol,'SORTING DISABLED');
  If DTTT.AllowZoom then
  begin
      Fastwrite(X+4,Y+9,attr(DTTT.KeyFCol,DTTT.BoxBCol),DZoomStr);
      Fastwrite(X+7+length(DZoomStr),Y+9,
           attr(DTTT.BoxFCol,DTTT.BoxBCol),
           'toggle long/short box size');
  end;
  If DTTT.AllowCD then
  begin
      Fastwrite(X+4,Y+11,attr(DTTT.KeyFCol,DTTT.BoxBCol),DChangeDirStr);
      Fastwrite(X+7+length(DChangeDirStr),Y+11,
           attr(DTTT.BoxFCol,DTTT.BoxBCol),
           'change to new drive/directory');
      Fastwrite(X+4,Y+12,attr(DTTT.KeyFCol,DTTT.BoxBCol),DJumpParentSTr);
      Fastwrite(X+7+length(DJumpParentStr),Y+12,
           attr(DTTT.BoxFCol,DTTT.BoxBCol),
           'backup to parent directory');
  end;
  WriteBetween(X, X + Width, Y,
          DTTT.BoxFCol + Blink, DTTT.BoxBCol,
          ' press any key ... ');

  ChH := upcase(GetKey);
{$ifdef fpc}
  MoveToScreen(Sc^,(longint(BaseOfScreen) and $ffff) shl 16, S Div 2);
{$else fpc}
  MoveToScreen(Sc^,BaseOfScreen^, S Div 2);
{$endif fpc}
  FreeMem(Sc,S);
    end;

    Procedure PromptForDirectory;
    const
       width = 55;
    var
       S : word;
       Sc : pointer;
       X : byte;
       OldP,OldM,Strng : String;
    begin
  S := 160*DisplayLines;
{$ifndef fpc}
  If MaxAvail < S then
     exit;
{$endif not fpc}
  OldP := Pathname;
  OldM := FileMask;
  GetMem(Sc,S);
{$ifdef fpc}
  MoveFromScreen((longint(BaseOfScreen) and $ffff) shl 16,Sc^,S Div 2);
{$else fpc}
  MoveFromScreen(BaseOfScreen^,Sc^,S Div 2);    {SaveThescreen}
{$endif fpc}
  If X1 + width > 80 then
     X := pred((80 - width) div 2)
  else
     X := X1;
  FBox(X,Y1,pred(X + width),Y1 + 2,DTTT.BoxFCol,DTTT.BoxBCol,2);
  WriteBetween(X,X+Width,Y1,DTTT.KeyFCol,DTTT.BoxBCol,'  Directory of Files  ');
  Strng := PathName+FileMask;
  Read_String_Upper(X+1,Y1+1,width - 2,'',0,Strng);
{$ifdef fpc}
  MoveToScreen(Sc^,(longint(BaseOfScreen) and $ffff) shl 16, S Div 2);
{$else fpc}
  MoveToScreen(Sc^,BaseOfScreen^, S Div 2);
{$endif fpc}
  FreeMem(Sc,S);
  If (R_Char <> #027) then
  begin
      DIRFULLFileName := Strng;
      Extract_Path_Mask;
      UnLoadFiles;
      DisplayNewDirectory;
      If TotalFiles = 0 then   {re-read original directory}
      begin
         sound(800);delay(200);nosound;
         PathName := OldP;
         FileMask := OldM;
         UnLoadFiles;
         DisplayNewDirectory;
      end;
  end;
    end;

    Function PromptForFilename(C:char):string;
    const
       width = 55;
    var
       S : word;
       Sc : pointer;
       X : byte;
       Strng : String;
       Msg : Strscreen;
    begin
  S := 160*DisplayLines;
{$ifndef fpc}
  If MaxAvail < S then
     exit;
{$endif not fpc}
  GetMem(Sc,S);
{$ifdef fpc}
  MoveFromScreen((longint(BaseOfScreen) and $ffff) shl 16,Sc^,S Div 2);
{$else fpc}
  MoveFromScreen(BaseOfScreen^,Sc^,S Div 2);    {SaveThescreen}
{$endif fpc}
  If X1 + width > 80 then
     X := pred((80 - width) div 2)
  else
     X := X1;
  FBox(X,Y1,pred(X + width),Y1 + 2,DTTT.BoxFCol,DTTT.BoxBCol,2);
  If C = #0 then
  begin
     Msg := '  No files  - enter filename  ';
     Strng := '';
  end
  else
  begin
     Msg := '  Enter filename (or Esc)  ';
     Strng := C;
  end;
  WriteBetween(X,X+Width,Y1,DTTT.KeyFCol,DTTT.BoxBCol,Msg);
  Read_String_Upper(X+1,Y1+1,width-2,'',0,Strng);
{$ifdef fpc}
  MoveToScreen(Sc^,(longint(BaseOfScreen) and $ffff) shl 16, S Div 2);
{$else fpc}
  MoveToScreen(Sc^,BaseOfScreen^, S Div 2);
{$endif fpc}
  FreeMem(Sc,S);
  If (R_Char <> #027) then
      PromptForFilename := Strng
  else
      PromptForFilename := '';
    end;

{$ENDIF}

{\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\}
{$IFDEF DIRFULL}
     Function No_Files_Found: integer;
     {returns 99 if user escaped
     or 0  if user enter a file
     }
     begin

   ChosenFile := PromptForFilename(#0);
   If ChosenFile = '' then
   begin
       No_Files_Found := 99;
       exit;
   end;
   If (pos('*',ChosenFile) > 0)
   or (pos('?',ChosenFile) > 0)
   or (ChosenFile[Length(ChosenFile)] = '\') then
   begin
       DIRFULLFileName := ChosenFile;
       Extract_Path_Mask;
       UnLoadFiles;
       DisplayNewDirectory;
   end
   else
   begin
       If  (pos('\',ChosenFile) = 0)
       and (pos(':',ChosenFile) = 0) then
       begin
      ChosenFile := PathName + ChosenFile;
      No_Files_Found := 0;
      exit;
       end;
   end;
   No_Files_Found := 1;
     end; {of func No_Files_Found}
{$ENDIF}

    Procedure Process_Keys;
    var
      ChD : char;
      Finished : Boolean;
    begin
  Finished := false;
  If TotalFiles = 0 then
  begin
{$IFDEF DIRFULL}
      Repeat
      Case No_Files_Found of
      0 : exit;  {user selected a file}
      99: begin  {user escaped}
         Retcode := NoFilesCode;
         Exit;
          end;
      end; {case}
      until TotalFiles <> 0;
{$ELSE}
     Retcode := NoFilesCode;
     WriteAt(succ(X1),succ(Y2),DTTT.DirFCol,DTTT.BoxBCol,       {5.01}
     'No files found.... press any key');
     ChD := GetKey;
     Exit;
{$ENDIF}
  end;
  Repeat
       ChD := upcase(GetKey);
       Case ChD of
       #129,                  {mouse down, down arrow}
       #208 :  If not BottomLine then
          begin
         LoLightFile(HiFn);
         Hifn := HiFn + Columnswide;
         If HiFn <= BotFn then
            HiLightFile(HiFn)
         else
            Scroll_Down;
          end;
       #128,                      {mouse up, up arrow}
       #200 : If not TopLine then
         begin
        LoLightFile(HiFn);
        Hifn := HiFn - Columnswide;
        If HiFn >= TopFn then
           HiLightFile(HiFn)
        else
           Scroll_Up;
         end;
       #205 : If HiFn < TotalFiles then  {right arrow}
         begin
        LolightFile(HiFn);
        Inc(HiFn);
        If HiFn > BotFn then
           Scroll_Down
        else
           HiLightFile(HiFn);
         end;
       #131 : If  (LastColumn = false) and (HiFn < BotFn) then  {mouse right}
         begin
        LolightFile(HiFn);
        Inc(HiFn);
        HiLightFile(HiFn);
         end;
       #130 : If (FirstColumn = false) then   {mouse left}
         begin
            LolightFile(HiFn);
            Dec(HiFn);
            HiLightFile(HiFn);
         end;
       #203 : If HiFn > 1 then {Left arrow}
         begin
        LolightFile(HiFn);
        Dec(HiFn);
        If HiFn < TopFn then
           Scroll_Up
        else
           HiLightFile(HiFn);
         end;
       #199 : If Columnswide = 1 then
         begin
        If TopFn = 1 then
        begin
            LoLightFile(HiFn);
            HiFn := 1;
             HiLightFile(HiFn);
        end
        else
           Scroll_Top;
         end
         else  {multiple column}
         begin
        If not FirstColumn then   {home}
        begin
            LoLightFile(HiFn);
            HiFn := HiFn - (pred(HiFn) mod ColumnsWide);
            HiLightFile(HiFn);
        end;
         end;
       #207 : If ColumnsWide = 1  then   {end}
         begin
        If TotalFiles <= BotFn then
        begin
             LoLightFile(HiFn);
             HiFn := TotalFiles;
             HiLightFile(HiFn);
        end
        else
           Scroll_Bottom;
         end
         else
         begin
        If not LastColumn then
        begin
            LoLightFile(HiFn);
            HiFn := HiFn
             + Columnswide
             - HiFn mod ColumnsWide;
            If HiFn > BotFn then
               HiFn := BotFn;
            HiLightFile(HiFn);
        end;
         end;
       #245 : If HiFn < TotalFiles then      {Ctrl End}
         begin
        If BotFn = TotalFiles then
        begin
             LoLightFile(HiFn);
             HiFn := TotalFiles;
             HiLightFile(HiFn);
        end
        else
        begin
           BotFn := TotalFiles;
           RecalcTopFn;
           HiFn := TotalFiles;
           DisplayFiles;
        end;
         end;
       #201 : If HiFn > 1 then               {PgUp}
         begin
        If TopFn > 1 then
        begin
            TopFn := TopFn - R*ColumnsWide;
            If TopFn < 1 then
               TopFn := 1;
        end;
        RecalcBotFn;
        HiFN := HiFn - R*ColumnsWide;
        If HiFn < 1 then
           HiFn := 1;
        DisplayFiles;
         end;
       #209 : If Hifn < TotalFiles then      {PgDn}
         begin
        If BotFn < TotalFiles then
        begin
            TopFn := TopFN + R*ColumnsWide;
            BotFn := BotFn + R*ColumnsWide;
            HiFn := HiFn + R*ColumnsWide;
            If BotFn > TotalFiles then
            begin
           BotFn := TotalFiles;
           RecalcTopFn;
           If  (HiFn < TopFn) then
               Repeat
              HiFn := HiFn + ColumnsWide;
               Until HiFN >= TopFN
           else
               If (HiFn > BotFn)  then
              HiFn := BotFn;
            end;
            DisplayFiles;
        end
        else     {Botfn is last file}
        begin
            LoLightFile(HiFn);
            If BottomLine then
           HiFn := BotFn
            else
           HiFn := HiFn + R*ColumnsWide;
            If HiFn > BotFn then
               HiFn := BotFn;
            HiLightFile(HiFn);
        end;
         end;
       #247 : If HiFn > 1 then      {Ctrl Home}
         begin
        If TopFn = 1 then
        begin
             LoLightFile(HiFn);
             HiFn := 1;
             HiLightFile(HiFn);
        end
        else
           Scroll_Top;
         end;
       DTogglekey : If DTTT.AllowToggle then
         begin
        ShowingDetails := not ShowingDetails;
        If Not ShowingDetails then
           ColumnsWide := DTTT.ColsWide
        else
           Columnswide := 1;
{$ifdef fpc}
        MoveToScreen(Scrn^,(longint(BaseOfScreen) and $ffff) shl 16,80*DisplayLines);
{$else fpc}
        MoveToScreen(Scrn^,BaseofScreen^,80*DisplayLines);
{$endif fpc}
        Calculate_Box_Dimensions;
        If Zoomed then
        begin
            Y3 := DTTT.Zoomline;
            R := pred(Y3 - Y2);
        end;
        TopFn := 0;
        Repeat
            If TopFN = 0 then
               TopFn := 1
            else
               TopFn := TopFN + R*ColumnsWide;
            BotFn := pred( TopFn + ColumnsWide*R);
            If BotFn > TotalFiles then
            begin
               BotFn := TotalFiles;
               If BotFn - pred(R*ColumnsWide) > 0 then
             TopFn := BotFN - pred(R*ColumnsWide);
            end;
        until ((HiFn >= TopFn) and (HiFn <= BotFn));
        Display_Box;
        FillInfo;
        DisplayFiles;
         end;
{$IFDEF DIRFULL}
   DZoomKey : If DTTT.AllowZoom then
         begin
        If Zoomed then
        begin
{$ifdef fpc}
            MoveToScreen(Scrn^,(longint(BaseOfScreen) and $ffff) shl 16,80*DisplayLines);
{$else fpc}
            MoveToScreen(Scrn^,BaseofScreen^,80*DisplayLines);
{$endif fpc}
            Zoomed := false;
            Y3 := Y3_Unzoomed;
            R := pred(Y3 - Y2);
            RecalcBotFn;
            If HiFn > BotFn then
               HiFn := BotFn;
            Display_Box;
            FillInfo;
            DisplayFiles;
        end
        else
        begin
            If (DTTT.ZoomLine > Y3) and (DTTT.ZoomLine <= DisplayLines) then
            begin
{$ifdef fpc}
           MoveToScreen(Scrn^,(longint(BaseOfScreen) and $ffff) shl 16,80*DisplayLines);
{$else fpc}
           MoveToScreen(Scrn^,BaseofScreen^,80*DisplayLines);
{$endif fpc}
           Zoomed := true;
           Y3 := DTTT.ZoomLine;
           R := pred(Y3 - Y2);
           RecalcBotFn;
           Display_Box;
           FillInfo;
           DisplayFiles;
            end;
        end;
         end;
    DSortOrderKey : If DTTT.AllowSort then
         begin
        SortAsc := not SortAsc;
        TopFn := 1;
        HiFn := 1;
        RecalcBotFn;
        DisplayFiles;
         end;
    DSortNameKey  : If DTTT.AllowSort and (SortOrder <> DSortName) then
         begin
        SortOrder := DSortName;
        ReSort;
         end;
    DSortExtKey   : If DTTT.AllowSort and (SortOrder <> DSortExt) then
         begin
        SortOrder := DSortExt;
        ReSort;
         end;
    DSortSizeKey  : If DTTT.AllowSort and (SortOrder <> DSortSize) then
         begin
        SortOrder := DSortSize;
        ReSort;
         end;
    DSortTimeKey  : If DTTT.AllowSort and (SortOrder <> DSortTime) then
         begin
        SortOrder := DSortTime;
        ReSort;
         end;
    DSortDOSKey   : If DTTT.AllowSort and (SortOrder <> DSortDOS) then
         begin
        SortOrder := DSortDOS;
        ReSort;
         end;
    DHelpKey      : If DTTT.AllowHelp then
            ShowHelpScreen;
    DJumpParentKey: If DTTT.AllowCD and (length(PathName) > 3) then  {Enter}
         begin
        PathName := PathParent(PathName);
        UnLoadFiles;
        DisplayNewDirectory;
         end;
    DChangeDirKey : If DTTT.AllowCD then
            PromptForDirectory;
    #33..#126     :  If DTTT.AllowInput then
          begin               {user entered an alpha numeric}
         ChosenFile := PromptForFilename(ChD);
         If ChosenFile <> '' then
         begin
             If (ChosenFile[Length(ChosenFile)] = ':') then
            ChosenFile := ChosenFile +'*.*';
             If (pos('*',ChosenFile) > 0)
             or (pos('?',ChosenFile) > 0)
             or (ChosenFile[Length(ChosenFile)] = '\') then
             begin
            DIRFULLFileName := ChosenFile;
            Extract_Path_Mask;
            UnLoadFiles;
            DisplayNewDirectory;
             end
             else
             begin
           If (pos('\',ChosenFile) = 0)
           and (pos(':',ChosenFile) = 0) then
               ChosenFile := PathName + ChosenFile;
           Finished := true;
             end;
         end;
          end;
{$ENDIF}
       #133,                                                 {Mouse left}
       #13  : If SubDirectory(FilePointer(HiFn)^.Attr) then  {Enter}
         begin
        If File_Name(HiFn) = '..' then
           PathName := PathParent(PathName)
        else
           PathName := PathChild(File_Name(HiFn));
        If (DTTT.SelectDir = false) then
        begin
           UnLoadFiles;
           DisplayNewDirectory;
        end
        else                      {return the Directory}
        begin
            Finished := true;
            ChosenFile := PathNoSlash(PathName);
        end;
         end
         else
         begin
        Finished := true;
        ChosenFile := PathName+File_Name(HiFn);
         end;
       #132,                          {mouse right}
       #027 : if DTTT.AllowEsc then
                    begin                   {esc}
        Finished := true;
        Retcode := EscCode;
        ChosenFile := '';
         end;
       end;  {case}
       If TotalFiles = 0 then
       begin
       {$IFDEF DIRFULL}
        Repeat
           Case No_Files_Found of
           0 : exit;  {user selected a file}
           99: begin  {user escaped}
               Retcode := NoFilesCode;
               Exit;
            end;
           end; {case}
        until TotalFiles <> 0;
       {$ELSE}
     Retcode := NoFilesCode;
     Exit;
       {$ENDIF}
       end;
  Until Finished;
    end; {sub proc Process_Keys}

    Procedure SaveInitScreen;
    var S : word;
    begin
  S := 160*DisplayLines;
{$ifndef fpc}
  If MaxAvail < S then
     NoMemory := true
  else
{$endif not fpc}
  begin
      Getmem(Scrn,160*DisplayLines);
{$ifdef fpc}
      MoveFromScreen((longint(BaseOfScreen) and $ffff) shl 16,Scrn^,S div 2);
{$else fpc}
      MoveFromScreen(BaseOfScreen^,Scrn^,S div 2);
{$endif fpc}
      FindCursor(CursRec[1],Cursrec[2],Cursrec[3],Cursrec[4]);
      OffCursor;
  end;
    end;

    Procedure Clear;
    begin
  If DTTT.RestoreScreen then
{$ifdef fpc}
      MoveToScreen(Scrn^,(longint(BaseOfScreen) and $ffff) shl 16,80*DisplayLines);
{$else fpc}
      MoveToScreen(Scrn^,BaseofScreen^,80*DisplayLines);
{$endif fpc}
  PosCursor(Cursrec[1],Cursrec[2]);
  SizeCursor(Cursrec[3],Cursrec[4]);
  UnLoadFiles;
  If Scrn <> Nil then
    FreeMem(Scrn,160*DisplayLines);
    end;

begin          {main procedure}
    FirstFile := nil;    {5.02b}
    NoMemory := False;
    Zoomed := False;
    ShowingDetails := DTTT.ShowDetails;
    SortAsc := DTTT.Asc = 1;
    SortOrder := DTTT.InitSort;
    If Not ShowingDetails then
       ColumnsWide := DTTT.ColsWide
    else
       Columnswide := 1;
    SaveInitScreen;
    If NoMemory then
    begin
  Retcode := Memcode;
  exit;
    end;
    {$I-}
    GetDir(0,StartDir);
    {SI+}
    If IOResult <> 0 then
    begin
  Retcode := UnknownCode;
  exit;
    end;
    Retcode := OKCode;     {assume it will succeed!}
    Extract_Path_Mask;
    Calculate_Box_Dimensions;
    Y3_unzoomed := Y3;   {ugh?}
    DisplayNewDirectory;
    If NoMemory then
    begin
       Clear;
       Retcode := Memcode;
    end
    else
    begin
       Process_Keys;
       Clear;
    end;
    Display_Directory := ChosenFile;
end;

begin
    Default_Settings;
    Horiz_Sensitivity := 3;
end.
