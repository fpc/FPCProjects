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
                     {       Unit:  NestTTT5          }
                     {--------------------------------}

{Revision History:    2/13/89    5.00a corrected calculation of Y2 lines
                                 542 and 544. (thanks Mike!)
                                 5.01a  removed refrences to VER50 and
                                        added DEBUG compiler directive
                     01/04/93    5.10   DPMI compatible version
}
{$ifdef fpc}
{$mode fpc}
{$endif fpc}

{$S-,R-,V-}
{$IFNDEF DEBUG}
{$D-}
{$ENDIF}

Unit NestTTT5;

INTERFACE

Uses CRT, FastTTT5, DOS, WinTTT5, KeyTTT5, StrnTTT5;

CONST
   Max_Levels = 10;        {maximum number of nested menus - alter if necessary}
   MenuStrLength = 40;     {maximum length of a menu topic - alter if necessary}
   DontClear    = 0;       {signal to return to same position in menu}
   RefreshTopic = 1;       {signal to rewrite highlighted topic}
   RefreshMenu  = 2;       {signal to reload current menu}
   ClearCurrent = 3;       {signal to remove current menu}
   ClearAll     = 4;       {signal to remove all menus}
   Undefined    = 99;      {despatcher has not been assigned}

Type
   {$IFNDEF VER40}
   Nest_Key_Proc =   procedure(var Ch:char; Code:Integer);
   Despatcher_Proc = procedure(Var Code: integer; var Finish:byte);
   {$ENDIF}

   MenuStr = string[MenuStrLength];

   N_Display = record
                     X           : byte;     {top X coord}
                     Y           : byte;     {top Y coord}
                     LeftSide    : boolean;  {does menu start on left or right}
                     AllowEsc    : boolean;  {can user escape from the top level}
                     BoxType     : byte;     {single,double etc}
                     BoxFCol     : byte;     {Border foreground color}
                     BoxBCol     : byte;     {Border background color}
                     CapFCol     : byte;     {Capital letter foreground color}
                     BacCol      : byte;     {menu background color}
                     NorFCol     : byte;     {normal foreground color}
                     LoFCol      : byte;     {inactive topic foreground color}
                     HiFCol      : byte;     {highlighted topic foreground color}
                     HiBCol      : byte;     {highlighted topic background color}
                     LeftChar    : char;     {left-hand topic highlight character}
                     RightChar   : char;     {right-hand topic highlight character}
                     {$IFNDEF VER40}
                     Hook        : Nest_Key_Proc;   { a procedure called after every key is pressed}
                     Despatcher  : Despatcher_proc;     { the main procedure to execute}
                     {$ENDIF}
               end;

    TopicPtr    = ^TopicRecord;

    MenuPtr     = ^Nest_Menu;

    TopicRecord = record
                        Name : MenuStr;
                        Active: boolean;
                        HotKey : char;
                        RetCode : integer;
                        Sub_Menu: MenuPtr;
                        Next_Topic: TopicPtr;
                   end;

    Nest_Menu  = record
                        Title: MenuStr;          {title for menu}
                        Topic_Width: byte;       {width of topics in menu}
                        Visible_Lines : word;    {no. topics in box, 0 is DisplayLines - 2}
                        First_Topic : TopicPtr;      {used internally, do not alter}
                        Total_Topics: word;          {used internally, do not alter}
                   end;

  VAR
    {$IFDEF VER40}
    Nest_UserHook : pointer;
    Nest_Despatcher: pointer;
    {$ENDIF}
    N_fatal : Boolean;
    N_Error : Integer;
    NTTT    : N_Display;

  Procedure Default_Settings;
  {$IFNDEF VER40}
  Procedure Assign_Despatcher(D:Despatcher_Proc);
  {$ENDIF}

  Procedure Initialize_Menu(var Menu:Nest_Menu;
                                Tit: menuStr;
                                Width: byte;
                                Display_Lines: word);

  Procedure Add_Topic(var Menu:Nest_Menu;
                          Nam : MenuStr;
                          Activ : boolean;
                          HKey : char;
                          Code : integer;
                          Sub: MenuPtr);

  Procedure Modify_Topic(var Menu:Nest_Menu;
                             TopicNo : word;
                             Nam : MenuStr;
                             Activ : boolean;
                             HKey  : char;
                             Code : integer;
                             Sub: MenuPtr);

  Procedure Modify_Topic_Name(var Menu:Nest_Menu;
                                  TopicNo : word;
                                  Nam : MenuStr);

  Procedure Modify_Topic_Active(var Menu:Nest_Menu;
                                  TopicNo : word;
                                  Activ : Boolean);

  Procedure Modify_Topic_HotKey(var Menu:Nest_Menu;
                                    TopicNo : word;
                                    HKey : char);

  Procedure Modify_Topic_RetCode(var Menu:Nest_Menu;
                                     TopicNo : word;
                                     Code : integer);

  Procedure Modify_Topic_SubMenu(var Menu:Nest_Menu;
                                     TopicNo : word;
                                     Sub : MenuPtr);

  Procedure Delete_A_Topic(var Menu:Nest_Menu;TopicNo: word);

  Procedure Delete_All_Topics(var Menu:Nest_Menu);

  Procedure Show_Nest(var Menu:Nest_Menu);

IMPLEMENTATION
var
  Despatcher_Assigned : boolean;

  Procedure NestTTT_Error(No : byte);
  {Updates N_error and optionally displays error message then halts program}
  var Msg : String;
  begin
      N_error := No;
      If N_fatal = true then
      begin
          Case No of
          1 :  Msg := 'Insufficient memory to add topic';
          2 :  Msg := 'Insufficient memory to save screen';
          3 :  Msg := 'No active picks in menu';
          4 :  Msg := 'Screen was not previously saved cannot restore';
          5 :  Msg := 'Too many levels in menu. Change Max_Levels in NestTTT';
          6 :  Msg := 'Topic does not exist, cannot modify';
          7 :  Msg := 'A user procedure has not been assigned to despatcher';
          else Msg := '?) -- Utterly confused';
          end; {Case}
          Msg := 'Fatal Error (NestTTT -- '+Msg;
          Writeln(Msg);
          Delay(5000);    {display long enough to read if child process}
          Halt;
      end;
  end;

{$F+}
  Procedure Empty_Despatcher(Var Code: integer; var Finish:byte);
  {}
  begin
      Finish := Undefined;
  end; {of proc Empty_Despatcher}

  Procedure No_Nest_Hook(var Ch : char; Code: Integer);
  {}
  begin
  end; {of proc No_Nest_Hook}
{$F-}

   {$IFDEF VER40}
   Procedure CallFromNestUserHook(var Ch:char; code:integer);
          Inline($FF/$1E/Nest_UserHook);

   Procedure CallFromNestDespatcher(Var Code: integer; var Finish:byte);
          Inline($FF/$1E/Nest_Despatcher);
   {$ENDIF}

  Procedure Default_Settings;
  begin
      with NTTT do
      begin
          X := 0;
          Y := 0;
          Despatcher_Assigned := false;
          LeftSide     := true;
          AllowEsc := true;
          BoxType      := 1;
          If ColorScreen then
          begin
              BoxFCol      := yellow;
              BoxBCol      := blue;
              CapFCol      := White;
              BacCol       := blue;
              NorFCol      := lightgray;
              LoFCol       := black;
              HiFCol       := white;
              HiBCol       := red;
          end
          else
          begin
              BoxFCol      := white;
              BoxBCol      := black;
              CapFCol      := White;
              BacCol       := black;
              NorFCol      := lightgray;
              LoFCol       := darkgray;
              HiFCol       := white;
              HiBCol       := black;
          end;
          LeftChar     := Chr(16);
          RightChar    := Chr(17);
          {$IFNDEF VER40}
          Hook := {$ifdef fpc}@{$endif}No_Nest_Hook;
          Despatcher   := {$ifdef fpc}@{$endif fpc}Empty_Despatcher;
          {$ELSE}
           Nest_UserHook := nil;
           Nest_Despatcher:= nil;
          {$ENDIF}
      end;  {with}
  end;  {Default_Settings}

  {$IFNDEF VER40}
  Procedure Assign_Despatcher(D:Despatcher_Proc);
  begin
      NTTT.Despatcher := D;
      Despatcher_Assigned := true;
  end;
  {$ENDIF}

  Procedure Initialize_Menu(var Menu:Nest_Menu;
                                Tit: menuStr;
                                Width: byte;
                                Display_Lines: word);
  {}
  begin
      With Menu do
      begin
          Title         := Tit;
          Topic_Width   := Width;
          Visible_Lines := Display_Lines;
          First_Topic   := nil;
          Total_Topics  := 0;
      end; {with}
  end; {of proc Initialize_Menu}

  Procedure Add_Topic(var Menu:Nest_Menu;
                          Nam : MenuStr;
                          Activ : boolean;
                          HKey  : char;
                          Code : integer;
                          Sub: MenuPtr);
  {Adds a new topic to the menu.}
  var
     TempPtr : TopicPtr;
  begin
{$ifndef fpc}
      If MaxAvail < SizeOf(TempPtr^) then
      begin
          NestTTT_Error(1);   {not enough memory}
          exit;
      end
      else
{$endif not fpc}
         N_Error := 0;
      If Menu.First_Topic = nil then
      begin
         getmem(Menu.First_Topic,SizeOf(TempPtr^));
         TempPtr := Menu.First_Topic;
      end
      else
      begin
         TempPtr := Menu.First_Topic;          {start at bottom}
         while TempPtr^.Next_Topic <> nil do               {loop to unallocated block}
            TempPtr := TempPtr^.Next_Topic;
         GetMem(TempPtr^.Next_Topic,SizeOf(TempPtr^));
         TempPtr := TempPtr^.Next_Topic;
      end;
      with TempPtr^ do
      begin
          Name := Nam;
          If (Name = '-') or (Name = '=') then
             Active := false
          else
             Active := Activ;
          HotKey := Hkey;
          RetCode := Code;
          Sub_Menu := Sub;
          Next_Topic := nil;
      end;
      Inc(Menu.Total_Topics);
  end; {of proc Add_Topic}

  Function Pointer_to_Topic(Men:Nest_Menu;TopicNo:word): TopicPtr;
  {returns a pointer to the TopicNo'th entry in menu, or nil
   if greater than Total_Topics}
  var
     W       : word;
     TempPtr : TopicPtr;
  begin
      with Men do
      begin
          If TopicNo > Total_Topics then
             TempPtr := nil
          else
          begin
              TempPtr := First_Topic;
              For W := 2 to TopicNo do
                      TempPtr := TempPtr^.Next_Topic
          end;
      end;
      Pointer_to_Topic := TempPtr;
  end; {of func Pointer_to_Topic}

  Procedure Modify_Topic(var Menu:Nest_Menu;
                             TopicNo : word;
                             Nam : MenuStr;
                             Activ : boolean;
                             HKey  : char;
                             Code : integer;
                             Sub: MenuPtr);
  {Changes all the settings for a topic}
  var TempPtr : TopicPtr;
  begin
      TempPtr := Pointer_To_Topic(Menu,TopicNo);
      If TempPtr = nil then
         NestTTT_Error(6);
      With TempPtr^ do
      begin
          Name := Nam;
          If (Name = '-') or (Name = '=') then
             Active := false
          else
             Active := Activ;
          HotKey := Hkey;
          RetCode := Code;
          Sub_Menu := Sub;
      end; {with}
  end; {of proc Modify_Topic}

  Procedure Modify_Topic_Name(var Menu:Nest_Menu;
                                  TopicNo : word;
                                  Nam : MenuStr);
  {Change title or name of a topic}
  var TempPtr : TopicPtr;
  begin
      TempPtr := Pointer_To_Topic(Menu,TopicNo);
      If TempPtr = nil then
         NestTTT_Error(6);
      TempPtr^.Name := Nam;
      If (Nam = '-') or (Nam = '=') then
             TempPtr^.Active := false;
  end; {of proc Modify_Topic_Name}

  Procedure Modify_Topic_Active(var Menu:Nest_Menu;
                                  TopicNo : word;
                                  Activ : Boolean);
  {Changes active status of a topic}
  var TempPtr : TopicPtr;
  begin
      TempPtr := Pointer_To_Topic(Menu,TopicNo);
      If TempPtr = nil then
         NestTTT_Error(6);
      TempPtr^.Active := Activ;
  end; {of proc Modify_Topic_Active}

  Procedure Modify_Topic_HotKey(var Menu:Nest_Menu;
                                    TopicNo : word;
                                    HKey : char);
  {Changes Hotkey character of a topic}
  var TempPtr : TopicPtr;
  begin
      TempPtr := Pointer_To_Topic(Menu,TopicNo);
      If TempPtr = nil then
         NestTTT_Error(6);
      TempPtr^.HotKey := HKey;
  end; {of proc Modify_Topic_HotKey}

  Procedure Modify_Topic_RetCode(var Menu:Nest_Menu;
                                     TopicNo : word;
                                     Code : integer);
  {Changes Return code for a topic}
  var TempPtr : TopicPtr;
  begin
      TempPtr := Pointer_To_Topic(Menu,TopicNo);
      If TempPtr = nil then
         NestTTT_Error(6);
      TempPtr^.Retcode := Code;
  end; {of proc Modify_Topic_HotKey}

  Procedure Modify_Topic_SubMenu(var Menu:Nest_Menu;
                                     TopicNo : word;
                                     Sub : MenuPtr);
  {Changes Return code for a topic}
  var TempPtr : TopicPtr;
  begin
      TempPtr := Pointer_To_Topic(Menu,TopicNo);
      If TempPtr = nil then
         NestTTT_Error(6);
      TempPtr^.Sub_Menu := Sub;
  end; {of proc Modify_Topic_HotKey}

  Procedure Delete_A_Topic(var Menu:Nest_Menu;TopicNo: word);
  {}
  var TempPtrA,TempPtrB : TopicPtr;
  begin
      If TopicNo = 1 then
      begin
          If Menu.First_Topic = nil then
             NestTTT_Error(6);
          TempPtrA := Menu.First_Topic^.Next_Topic;
          FreeMem(Menu.First_Topic,SizeOf(TempPtrA^));
          Menu.First_Topic := TempPtrA;
      end
      else
      begin
          TempPtrA := Pointer_To_Topic(Menu,pred(TopicNo));
          If TempPtrA = nil then
             NestTTT_Error(6);
          TempPtrB := Pointer_To_Topic(Menu,TopicNo);
          If TempPtrB = nil then
             NestTTT_Error(6);
          TempPtrA^.Next_Topic := TempPtrB^.Next_Topic;
          FreeMem(TempPtrB,SizeOf(TempPtrB^));
      end;
      Dec(Menu.Total_Topics);
  end; {of proc Delete_A_Topic}

  Procedure Delete_All_Topics(var Menu:Nest_Menu);
  {}
  var TempPtrA,TempPtrB : TopicPtr;
  begin
      TempPtrA := Menu.First_Topic;
      While (TempPtrA <> nil) do
      begin
          TempPtrB := TempPtrA^.Next_Topic;
          If TempPtrA <> nil then
          begin
              FreeMem(TempPtrA,SizeOf(TempPtrA^));
              TempPtrA := TempPtrB;
          end;
      end;
      Menu.First_Topic := nil;
  end; {of proc Delete_All_Topics}

  Procedure Show_Nest(var Menu:Nest_Menu);
  Type
     LevelInfo = record
                      Pick : word;
                      TheMenu : MenuPtr;     {link to menu}
                      X1   : integer;           {coords of saved screens}
                      Y1   : integer;
                      X2   : integer;
                      Y2   : integer;
                      TopPick : byte;
                      HiPick  : byte;
                      Saved_Screen: Pointer; {location of saved screen}
                 end;
  Var
     I : word;
     TempPtr : TopicPtr;
     FinCode : byte;
     Nest : array[1..Max_Levels] of LevelInfo;
     Current_Level : byte;
     LiveMenu : Nest_menu;
     ChL : char;
     Found,
     Finished : boolean;

      Function Topic_Pointer(TopicNo:word): TopicPtr;
      begin
          Topic_Pointer := Pointer_to_Topic(LiveMenu,TopicNo);
      end; {of func Topic_Pointer}


      Procedure Compute_Coords(var LiveMenu:Nest_Menu);
      {determines X1,Y1,X2,Y2 for new menu}
      begin
          With Nest[Current_level] do
          begin
              If LiveMenu.Visible_Lines = 0 then
                 LiveMenu.Visible_Lines := DisplayLines-2;
              If LiveMenu.Total_Topics < LiveMenu.Visible_Lines then
                 LiveMenu.Visible_Lines := LiveMenu.Total_Topics;
              If Current_Level = 1 then
              begin
                  If NTTT.X = 0 then
                  begin
                      If NTTT.LeftSide then
                      begin
                          X1 := 1;
                          X2 := LiveMenu.Topic_Width + 4;
                      end
                      else    {RightSide}
                      begin
                          X2 := 80;
                          X1 := 80 - LiveMenu.Topic_Width - 3;
                      end;
                  end
                  else {X not Zero}
                  begin
                      If NTTT.LeftSide then
                      begin
                          X1 := NTTT.X;
                          X2 := pred(X1)+LiveMenu.Topic_Width + 4;
                          If X2 > 80 then
                          begin
                              X2 := 80;
                              X1 := X2 - 3 - LiveMenu.Topic_Width;
                          end;
                      end
                      else    {RightSide}
                      begin
                          X2 := NTTT.X;
                          X1 := X2 - LiveMenu.Topic_Width - 3;
                          If X1 < 1 then
                          begin
                              X1 := 1;
                              X2 := X1 +LiveMenu.Topic_Width +3;
                          end;
                      end;
                  end;
                  If NTTT.Y = 0 then
                     Y1 := 1
                  else
                     Y1 := NTTT.Y;
                  If LiveMenu.Total_Topics >= LiveMenu.Visible_Lines then
{mod 5.00a}          Y2 := Y1 + succ(LiveMenu.Visible_Lines)
                  else
                     Y2 := Y1 + succ(LiveMenu.Total_Topics);
                  If Y2 > DisplayLines then
                  begin
                     Y2 := DisplayLines;
                     LiveMenu.Visible_Lines := Y2 - succ(Y1);
                  end;
              end
              else   {not the first level menu}
              begin
                  If NTTT.LeftSide then
                  begin
                      X1 := pred(Nest[pred(Current_Level)].X2);
                      X2 := X1 + 3 + LiveMenu.Topic_Width;
                      If X2 > 80 then
                      begin
                          X2 := 80;
                          X1 := X2 - 4 - LiveMenu.Topic_Width;
                      end;
                  end
                  else   {rightside}
                  begin
                      X2 := succ(Nest[pred(Current_Level)].X1);
                      X1 := X2 - LiveMenu.Topic_Width - 3;
                      If X1 < 1 then
                      begin
                          X1 := 1;
                          X2 := X1 +LiveMenu.Topic_Width +3;
                      end;
                  end;
                  Y1 := succ(Nest[Pred(Current_Level)].Y1) +
                        Nest[Pred(Current_Level)].HiPick -
                        Nest[Pred(Current_Level)].TopPick;
                  If LiveMenu.Total_Topics >= LiveMenu.Visible_Lines then
                     Y2 := succ(Y1) + LiveMenu.Visible_Lines
                  else
                     Y2 := succ(Y1) + LiveMenu.Total_Topics;
                  If Y2 > DisplayLines then
                  begin
                     Y2 := DisplayLines;
                     If Y2 - succ(LiveMenu.Visible_Lines) >= 1 then
                        Y1 := Y2 - succ(LiveMenu.Visible_Lines)
                     else
                     begin
                         Y1 := 1;
                         LiveMenu.Visible_Lines := DisplayLines - 2;
                     end;
                  end;
              end;
          end; {With}
      end; {of proc Compute_Coords}

      Procedure Save_Screen;
      {saved part of screen overlayed by menu}
      begin
          with Nest[Current_Level] do
          begin
{$ifndef fpc}
              If MaxAvail < succ(Y2-Y1)*succ(X2-X1)*2 then
                  NestTTT_Error(2)
              else
{$endif not fpc}
              begin
                  GetMem(Saved_Screen,succ(Y2-Y1)*succ(X2-X1)*2);
                  PartSave(X1,Y1,X2,Y2,Saved_Screen^);
              end;
          end;
      end; {of proc Save_Screen}

      Procedure Restore_Screen;
      {saved part of screen overlayed by menu}
      begin
          with Nest[Current_Level] do
          begin
              If Saved_Screen = nil then
                  NestTTT_Error(4)
              else
              begin
                  PartRestore(X1,Y1,X2,Y2,Saved_Screen^);
                  FreeMem(Saved_Screen,succ(Y2-Y1)*succ(X2-X1)*2);
              end;
          end;
      end; {of proc Restore_Screen}

      Procedure Compute_First_Active_Pick;
      {}
      var I : word;
      begin
          With Nest[Current_level] do
          begin
              TopPick := 1;
              HiPick := 1;
              While (Topic_Pointer(HiPick)^.Active = false)
              and   (HiPick < LiveMenu.Total_Topics) do
                    Inc(HiPick);
              If (Topic_Pointer(HiPick)^.Active = false) then {no active picks in menu}
              begin
                  NestTTT_Error(3);
                  exit;
              end;
              If HiPick > LiveMenu.Visible_Lines then
                 TopPick := HiPick - pred(LiveMenu.Visible_Lines);
          end; {with}
      end; {of proc Compute_First_Active_Pick}

      Procedure Compute_Topic_Width(var Livemenu:Nest_Menu);
      {}
      var
        I : word;
        W,Biggest : Byte;
      begin
          Biggest := 0;
          For I := 1 To LiveMenu.Total_Topics do
          begin
              W := length(Topic_Pointer(I)^.Name);
              If Biggest < W then
                 Biggest := W;
          end;
          If Biggest < length(LiveMenu.Title) then
             Biggest := length(LiveMenu.Title);
          LiveMenu.Topic_Width := Biggest;
      end; {of proc Compute_Topic_Width}

      Procedure Write_Topic(TopicNo:word;Hilight:boolean);
      {}
      var
        A,Y : byte;
        T : TopicPtr;
      begin
         T := Topic_Pointer(TopicNo);
         If T = Nil then
            exit;
         If HiLight then
            A := attr(NTTT.HiFCol,NTTT.HiBCol)
         else
         begin
             If T^.Active then
                A := attr(NTTT.NorFcol,NTTT.BacCol)
             else
                A := attr(NTTT.LoFcol,NTTT.BacCol);
         end;
         with Nest[Current_level] do
         begin
             Y := succ(Y1) + TopicNo - TopPick;
             If HiLight then
                Fastwrite(succ(X1),Y,A,
                          NTTT.LeftChar+
                          PadLeft(T^.Name,LiveMenu.Topic_Width,' ')+
                          NTTT.Rightchar)
             else
                Case T^.Name[1] of
                '-': HorizLine(Succ(X1),Pred(X2),Y,NTTT.BoxFCol,NTTT.BacCol,1);
                '=': HorizLine(Succ(X1),Pred(X2),Y,NTTT.BoxFCol,NTTT.BacCol,1);
                else
                    begin
                        Fastwrite(succ(X1),Y,A,
                                  ' '+
                                  PadLeft(T^.Name,LiveMenu.Topic_Width,' ')+
                                  ' ');
                        If (T^.Active) and (First_Capital_Pos(T^.Name) > 0) then
                           Fastwrite(succ(X1)+First_Capital_Pos(T^.Name),
                                     Y,
                                     attr(NTTT.CapFCol,NTTT.BacCol),
                                     First_Capital(T^.Name));
                    end;
                end; {Case}
         end;
      end; {of proc Write_Topic}

      Procedure Display_All_Topics;
      {}
      var I : Integer;
      begin
          with Nest[Current_Level] do
          begin
              For I := TopPick to TopPick+pred(LiveMenu.Visible_Lines) do
                  Write_Topic(I,false);
              Write_Topic(HiPick,true);
          end;
      end; {of proc Display_All_Topics}

      Procedure Display_LiveMenu;
      {}
      begin
          with Nest[Current_Level] do
          begin
              FBox(X1,Y1,X2,Y2,NTTT.BoxFCol,NTTT.BoxBCol,NTTT.BoxType);
              WriteBetween(X1,X2,Y1,NTTT.BoxFCol,NTTT.BoxBCol,Livemenu.Title);
          end;
          Display_All_Topics;
      end; {of proc Display_LiveMenu}

      Function Next_Pick_Down(Wrap:boolean): word;
      {}
      var P : word;
      begin
          with Nest[Current_Level] do
          begin
              P := HiPick;
              If P < LiveMenu.Total_Topics then
              begin
                  inc(P);
                  while (P < LiveMenu.Total_Topics)
                  and   (Topic_Pointer(P)^.Active = false) do
                        Inc(P);
                  If Topic_Pointer(P)^.Active = false then
                  begin
                      If Wrap and (LiveMenu.Total_Topics <= LiveMenu.Visible_Lines) then
                      begin
                         P := TopPick;  {scroll to top}
                         while (P < LiveMenu.Total_Topics)
                         and   (Topic_Pointer(P)^.Active = false) do
                               Inc(P);
                      end
                      else
                         P := Hipick;
                  end;
              end
              else     {P is at bottom of menu}
              begin
                  If Wrap and (LiveMenu.Total_Topics <= LiveMenu.Visible_Lines) then
                     P := TopPick;  {scroll to top}
                  while (P < LiveMenu.Total_Topics)
                  and   (Topic_Pointer(P)^.Active = false) do
                        Inc(P);
              end;
              Next_Pick_Down := P;
          end; {with}
      end; {of func Next_Pick_Down}

      Function Next_Pick_Up(Wrap:boolean): word;
      {}
      var P : word;
      begin
          with Nest[Current_Level] do
          begin
              P := HiPick;
              If P > 1 then
              begin
                  dec(P);
                  while (P > 1)
                  and   (Topic_Pointer(P)^.Active = false) do
                        Dec(P);
                  If Topic_Pointer(P)^.Active = false then
                  begin
                      If Wrap and (LiveMenu.Total_Topics <= LiveMenu.Visible_Lines) then
                      begin
                         P := LiveMenu.Total_Topics;  {scroll to top}
                         while (P > 1)
                         and   (Topic_Pointer(P)^.Active = false) do
                               Dec(P);
                      end
                      else
                         P := Hipick;
                  end;
              end
              else     {P is at top of menu}
              begin
                  If Wrap and (LiveMenu.Total_Topics <= LiveMenu.Visible_Lines) then
                  begin
                     P := LiveMenu.Total_Topics;  {scroll to top}
                     while (P > 1)
                     and   (Topic_Pointer(P)^.Active = false) do
                           Dec(P);
                  end;
              end;
              Next_Pick_Up := P;
          end; {with}
      end; {of func Next_Pick_Up}

      Procedure Load_Menu(var NewMenu:Nest_Menu);
      {}
      begin
          If Current_Level < Max_Levels then
             Inc(Current_Level)
          else
             NestTTT_Error(5);
          Nest[Current_Level].TheMenu := @NewMenu;
          LiveMenu := NewMenu;
          If LiveMenu.Topic_Width <= 0 then
          begin
             Compute_Topic_Width(LiveMenu);
             NewMenu.Topic_Width := LiveMenu.Topic_Width;
          end;
          Compute_Coords(LiveMenu);
          Compute_Coords(NewMenu);
          Compute_First_Active_Pick;
          Save_Screen;
          Display_LiveMenu;
      end; {of proc Load_Menu;}

      Procedure Execute_Command;
      {}
      var
         TempPtr : TopicPtr;
         Code : integer;
      begin
          TempPtr := Topic_Pointer(Nest[Current_Level].HiPick);
          If TempPtr^.Sub_Menu <> nil then
             Load_Menu(TempPtr^.Sub_Menu^)
          else
          begin
              Code := TempPtr^.Retcode;
              {$IFNDEF VER40}
              NTTT.Despatcher(Code,Fincode);
              {$ELSE}
              If Nest_Despatcher <> Nil then
                 CallFromNestDespatcher(Code,Fincode)
              else
                 Fincode := Undefined;
              {$ENDIF}
              Case Fincode of
              Undefined    :NestTTT_Error(7);
              DontClear    :;
              RefreshTopic : Write_Topic(Nest[Current_Level].HiPick,True);
              RefreshMenu  : Display_All_Topics;
              ClearCurrent : begin
                                 Restore_Screen;
                                 If Current_Level > 1 then
                                 begin
                                    Dec(Current_Level);
                                    LiveMenu := Nest[Current_Level].TheMenu^;
                                 end
                                 else
                                    Finished := true;
                             end;
              ClearAll     : begin
                                 While Current_Level > 0 do
                                 begin
                                     Restore_Screen;
                                     Dec(Current_Level);
                                     LiveMenu := Nest[Current_Level].TheMenu^;
                                 end;
                                 Finished := true;
                             end;
              end; {Case}
          end;
      end; {of proc Execute_Command}

     Procedure Display_More;
     {}
     var A : byte;
     begin
         If LiveMenu.Visible_Lines < Livemenu.Total_Topics then
            with  Nest[Current_Level] do
            begin
                A := attr(NTTT.CapFCol,NTTT.BoxBCol);
                If TopPick > 1 then
                   Fastwrite(X2,Succ(Y1),A,chr(24))
                else
                   VertLine(X2,Succ(Y1),Succ(Y1),NTTT.BoxFcol,NTTT.BoxBCol,Nttt.Boxtype);
                If TopPick + Pred(LiveMenu.Visible_Lines) < LiveMenu.Total_Topics then
                   Fastwrite(X2,Pred(Y2),A,chr(25))
                else
                   VertLine(X2,Pred(Y2),Pred(Y2),NTTT.BoxFcol,NTTT.BoxBCol,Nttt.Boxtype);
            end;
     end; {of proc Display_More}

  begin
      Current_level := 0;
      {$IFNDEF VER40}
      If not Despatcher_Assigned then
         NestTTT_Error(7);
      {$ELSE}
      If Nest_Despatcher = nil then
         NestTTT_Error(7);
      {$ENDIF}
      Load_Menu(Menu);
      Finished := False;
      Repeat
           Display_More;
           ChL := GetKey;
           {$IFNDEF VER40}
           NTTT.Hook(ChL,Topic_Pointer(Nest[Current_Level].HiPick)^.RetCode);
           {$ELSE}
           If Nest_UserHook <> Nil then
              CallFromNestUserHook(ChL,Topic_Pointer(Nest[Current_Level].HiPick)^.RetCode);
           {$ENDIF}
           If ChL <> #0 then
           Case upcase(ChL) of
           #132,                               {right button}
           #027    : If Current_Level = 1 then
                     begin
                         If NTTT.AllowEsc then
                         begin
                             Restore_Screen;
                             Finished := true;
                         end;
                     end
                     else
                     begin
                         Restore_Screen;
                         Dec(Current_Level);
                         LiveMenu := Nest[Current_Level].TheMenu^;
                     end;
           #133,                                       {Mouse left button}
           #13     : begin                             {Enter}
                         Execute_Command;
                     end;
           ' ',
           #129,                                       {Mouse down}
           #208    : with Nest[Current_Level] do       {Down arrow}
                     begin
                         Write_Topic(HiPick,False);
                         HiPick := Next_Pick_Down(ChL = #208);
                         If HiPick >= TopPick + LiveMenu.Visible_Lines then
                         begin
                             TopPick := HiPick - pred(LiveMenu.Visible_Lines);
                             Display_All_Topics;
                         end;
                         Write_Topic(HiPick,True);
                     end;
           #128,                                       {Mouse up}
           #200    : with Nest[Current_Level] do       {Up arrow}
                     begin
                         Write_Topic(HiPick,False);
                         HiPick := Next_Pick_Up(ChL = #200);
                         If HiPick < TopPick  then
                         begin
                             TopPick := HiPick;
                             Display_All_Topics;
                         end;
                         Write_Topic(HiPick,True);
                     end;
            #199   : If Nest[Current_Level].HiPick <> 1 then      {Home}
                     begin
                         Compute_First_Active_Pick;
                         Display_All_Topics;
                     end;
            #207   : With Nest[Current_Level] do
                     begin
                         Write_Topic(HiPick,False);
                         HiPick := LiveMenu.Total_Topics;
                         While (HiPick > 0)
                         and (Topic_Pointer(HiPick)^.Active =false) do
                              Dec(HiPick);
                         If HiPick >= TopPick + LiveMenu.Visible_Lines then
                         begin
                             TopPick := HiPick - pred(LiveMenu.Visible_Lines);
                             Display_All_Topics;
                         end;
                         Write_Topic(HiPick,True);
                     end;
           'A'..'Z': with Nest[Current_Level] do
                     begin
                         Found := false;
                         I := HiPick;
                         Repeat
                              TempPtr := Topic_Pointer(I);
                              If  (First_Capital(TempPtr^.Name) = upcase(ChL))
                              and (TempPtr^.Active) then
                              begin
                                  Found := true;
                                  Write_Topic(HiPick,false);
                                  HiPick := I;
                                  If HiPick >= TopPick + LiveMenu.Visible_Lines then
                                  begin
                                      TopPick := HiPick - pred(LiveMenu.Visible_Lines);
                                      Display_All_Topics;
                                  end
                                  else
                                     If HiPick < TopPick  then
                                     begin
                                         TopPick := HiPick;
                                         Display_All_Topics;
                                     end;
                                     Write_Topic(HiPick,true);
                              end
                              else
                                  If I = LiveMenu.Total_Topics then
                                     I := 1
                                  else
                                     Inc(I);
                         Until Found or (I = HiPick);
                         If Found then
                            Execute_Command;
                     end;
           else   {see if the user pressed a special key}
               with Nest[Current_Level] do
               begin
               Found := false;
               I := HiPick;
               Repeat
                    TempPtr := Topic_Pointer(I);
                    If  ((TempPtr^.Hotkey) = ChL)
                    and (TempPtr^.Active) then
                    begin
                        Found := true;
                        Write_Topic(HiPick,false);
                        HiPick := I;
                        If HiPick >= TopPick + LiveMenu.Visible_Lines then
                        begin
                            TopPick := HiPick - pred(LiveMenu.Visible_Lines);
                            Display_All_Topics;
                        end
                        else
                           If HiPick < TopPick  then
                           begin
                               TopPick := HiPick;
                               Display_All_Topics;
                           end;
                           Write_Topic(HiPick,true);
                    end
                    else
                        If I = LiveMenu.Total_Topics then
                           I := 1
                        else
                           Inc(I);
               Until Found or (I = HiPick);
               If Found then
                  Execute_Command;
               end;
      end; {case}
      Until Finished;
  end; {of proc Show_Nest}


begin
    Default_Settings;
    N_Fatal := true;
end.

