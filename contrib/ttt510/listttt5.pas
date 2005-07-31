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
                     {       Unit:  ListTTT5          }
                     {--------------------------------}


{Update History:     5.01a   Removed refrences to VER50 and added DEBUG
                             compiler directive, added Mouse enter
                     5.01b   Added first character highlight selection
                     5.02a   Move TopPick and HiPick to globals
          01/04/93   5.10   DPMI compatible version
}
{$ifdef fpc}
{$mode tp}
{$endif fpc}

{$S-,R-,V-}
{$IFNDEF DEBUG}
{$D-}
{$ENDIF}

Unit ListTTT5;

interface

Uses CRT, DOS, FastTTT5, WinTTT5, KeyTTT5, StrnTTT5;

const
     Max_Topics = 255;

Type
   Choices = array[1..Max_Topics] of boolean;
   {$IFNDEF VER40}
    List_Hook = Procedure(var Ch: char; HiPick:byte);
   {$ENDIF}
   L_Display = record
                     X           : byte;             {top X coord}
                     Y           : byte;             {top Y coord}
                     LeftSide    : Boolean;          {X,Y is leftside of box}
                     Lines       : byte;             {max no of lines to display in box}
                     TopicWidth  : byte;             {width of the slection bar}
                     AllowEsc    : boolean;          {allow the user to escape?}
                     BoxType     : byte;             {single,double etc}
                     BoxFCol     : byte;             {Border foreground color}
                     BoxBCol     : byte;             {Border background color}
                     CapFCol     : byte;             {Capital letter foreground color}
                     BacCol      : byte;             {menu background color}
                     NorFCol     : byte;             {normal foreground color}
                     HiFCol      : byte;             {highlighted topic foreground color}
                     HiBCol      : byte;             {highlighted topic background color}
                     LeftChar    : char;             {left-hand topic highlight character}
                     RightChar   : char;             {right-hand topic highlight character}
                     ToggleChar  : char;             {indicates if a topic has been selected}
                     AllowToggle : Boolean;          {can user select more than one topic}
                     End_Chars   : set of char;      {end of input chars}
                     Select_Chars: set of char;      {keys for user to select topic}
                     {$IFNDEF VER40}
                     Hook: List_Hook; {a procedure called after every key is pressed}
                     {$ENDIF}
               end;

Var
   LTTT    : L_Display;
   L_Picks : Choices;
   L_Char  : Char;
   L_Pick  : Byte;
   TopPick     : byte;
   HiPick      : byte;
   {$IFDEF VER40}
   L_UserHook  : pointer;
   {$ENDIF}

Procedure Default_Settings;
Procedure Show_List(var StrArray;StrLength:Byte;TotalPicks:byte);
Procedure New_Show_List(var StrArray;StrLength:Byte;TotalPicks:byte);

IMPLEMENTATION
const
    Default_Display_Lines = 10;
    Default_Y1            = 7;

  {$IFNDEF VER40}
  {$F+}
  Procedure No_Hook(var Ch: char; HiPick :byte);
  {}
  begin
  end; {of proc No_Hook}
  {$F-}
  {$ENDIF}

  Procedure Default_Settings;
  begin
      with LTTT do
      begin
          AlloWEsc := true;
          X := 0;
          Y := 0;
          LeftSide := true;
          BoxType      := 1;
          Lines := 0;
          TopicWidth   := 0;
          If ColorScreen then
          begin
              BoxFCol      := yellow;
              BoxBCol      := blue;
              CapFCol      := White;
              BacCol       := blue;
              NorFCol      := lightgray;
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
              HiFCol       := white;
              HiBCol       := black;
          end;
          LeftChar     := Chr(16);
          RightChar    := Chr(17);
          ToggleChar   := Chr(251);
          AllowToggle  := true;
          End_Chars    := [#13,#133];
          Select_Chars := [' '];
          {$IFNDEF VER40}
          Hook := No_Hook;
          {$ELSE}
          L_UserHook := nil;
          {$ENDIF}
      end;  {with}
      TopPick := 1;
      HiPick := 1;
  end;  {Default_Settings}

  {$IFDEF VER40}
   Procedure CallFromListUserHook(var Ch:char;Hipick:byte);
             Inline($FF/$1E/L_UserHook);
  {$ENDIF}

 Procedure New_Show_List(var StrArray;StrLength:Byte;TotalPicks:byte);
 {}
 var
   X1,Y1,X2,Y2 : byte;
   ListWidth   : byte;
   ListLines   : byte;
(*
   TopPick     : byte;
   HiPick      : byte;
*)
   Selected    : Choices;
   Finished    : boolean;
   Scrolling   : boolean;
   ChL         : char;

         Function TopicStr(StrNo:byte): StrScreen;
         {searches through string array and returns the string}
{$ifdef fpc}
         type
           pbytearray = ^tbytearray;
           tbytearray = array[0..$ffff] of byte;
{$endif fpc}
         var
           W : word;
           TempStr : String;
           ArrayOffset: word;
         begin
{$ifdef fpc}
             arrayOffset := pred(StrNo)*succ(StrLength);
             w := pbytearray(@strArray)^[arrayOffset];
             move(pbytearray(@strArray)^[arrayOffset],TempStr,succ(w));
{$else fpc}
             W := pred(StrNo) * succ(StrLength);
             ArrayOffset := Ofs(StrArray) + W;
             Move(Mem[Seg(StrArray):ArrayOffset],TempStr,1);            {string length in byte 0}
             Move(Mem[Seg(StrArray):succ(ArrayOffset)],TempStr[1],ord(TempStr[0]));
{$endif fpc}
             TopicStr := TempStr;
         end; {of func TopicStr}

         Procedure Write_Topic(TopicNo:word;Hilight:boolean);
         {}
         var
           A, Y : byte;
           Tick : char;
         begin
             Y := Succ(Y1) + TopicNo - TopPick;
             If Selected[TopicNo] then
                Tick := LTTT.ToggleChar
             else
                Tick := ' ';
             If HiLight then
                Fastwrite(succ(X1),Y,
                          attr(LTTT.HiFCol,LTTT.HiBCol),
                          LTTT.LeftChar+Tick+' '+padleft(TopicStr(TopicNo),ListWidth,' ')+LTTT.RightChar)
             else
             begin
                Fastwrite(succ(X1),Y,
                          attr(LTTT.NorFCol,LTTT.BacCol),
                          ' '+Tick+' '+padleft(TopicStr(TopicNo),ListWidth,' ')+' ');
                A := First_Capital_Pos(TopicStr(TopicNo));
                If A > 0 then
                   Fastwrite(X1+3+A,Y,attr(LTTT.CapFCol,LTTT.BacCol),copy(TopicStr(TopicNo),A,1));
             end;
         end; {of proc Write_Topic}

         Procedure Compute_Topic_Width;
         {}
         var
           I : word;
           W : Byte;
         begin
             ListWidth := 0;
             For I := 1 To TotalPicks do
             begin
                 W := length(TopicStr(I));
                 If ListWidth < W then
                    ListWidth := W;
             end;
             Inc(ListWidth);  {add one char space to right}
         end; {of proc Compute_Topic_Width}

         Procedure Compute_Coords;
         {determines the X Y coords of the list box}
         begin
             With LTTT do
             begin
                 If TopicWidth <> 0 then
                    ListWidth := TopicWidth
                 else
                    Compute_Topic_Width;
                 ListWidth := ListWidth + 6;
                 If Lines <> 0 then
                    ListLines := Lines
                 else
                    ListLines := Default_Display_Lines;
                 If ListLines > TotalPicks then
                    ListLines := TotalPicks;
                 If X <> 0 then
                 begin
                     If LeftSide then
                     begin
                         X1 := X;
                         X2 := X1 + Pred(ListWidth);
                     end
                     else
                     begin
                         X2 := X;
                         X1 := X2 - pred(ListWidth);
                     end;
                 end
                 else
                 begin
                     X1 :=  (80 - ListWidth) div 2;
                     X2 :=   X1 + Pred(ListWidth);
                 end;
                 If Y <> 0 then
                     Y1 := Y
                 else
                     Y1 := Default_Y1;
                 If Y1 + succ(ListLines) > DisplayLines then
                 begin
                     Y2 := DisplayLines;
                     ListLines := Y2 - succ(Y1);
                 end
                 else
                     Y2 :=  Y1 + Succ(ListLines);
                 ListWidth := ListWidth - 6;    {set to actual topic width}
                 If ListLines < TotalPicks then
                    Scrolling := true
                 else
                    Scrolling := false;
             end;  {with LTTT}
         end; {of proc Compute_Coords}

         Procedure Draw_List_Box;
         {}
         begin
             with LTTT do
             begin
                 Box(X1,Y1,X2,Y2,BoxFCol,BoxBCol,BoxType);
                 ClearText(succ(X1),Succ(Y1),Pred(X2),Pred(Y2),NorFcol,BacCol);
             end; {with}
         end; {of proc Draw_List_Box}

         Procedure Set_Parameters;
         {}
         var I : integer;
         begin
             For I := 1 to Max_Topics do
                 Selected[I] := false;
         (*
             TopPick := 1;
             HiPick := 1;
         *)
         end; {of proc Set_Parameters}

         Procedure Display_More;
         {}
         var A : byte;
         begin
             If Scrolling then
             begin
                    A := attr(LTTT.BoxFCol,LTTT.BoxBCol);
                    If TopPick > 1 then
                       Fastwrite(X2,Succ(Y1),A,chr(24))
                    else
                       VertLine(X2,Succ(Y1),Succ(Y1),LTTT.BoxFcol,LTTT.BoxBCol,Lttt.Boxtype);
                    If TopPick + Pred(ListLines) < TotalPicks then
                       Fastwrite(X2,Pred(Y2),A,chr(25))
                    else
                       VertLine(X2,Pred(Y2),Pred(Y2),LTTT.BoxFcol,LTTT.BoxBCol,Lttt.Boxtype);
             end;
         end; {of proc Display_More}

         Procedure Display_All_Topics;
         {}
         var  I : Integer;
         begin
             For I := TopPick to TopPick+pred(ListLines) do
                 Write_Topic(I,false);
             Write_Topic(HiPick,True);
             Display_More;
         end; {of proc Display_All_Topics}

         Procedure Find_Capital_Letter(Ch : char);
         {Goes down the list to the bottom and then searches down from top}
         var
           I : integer;
         begin
             I := HiPick;
             Repeat
                  If I < TotalPicks then
                     inc(I)
                  else
                     I := 1;
                  If I = HiPick then exit;
             Until  (First_Capital(TopicStr(I)) = Ch);
             If (I >= TopPick) and (I <= TopPick + Pred(ListLines)) then
             begin
                 Write_Topic(HiPick,False);
                 HiPick := I;
                 Write_Topic(HiPick,true);
             end
             else
             begin
                 HiPick := I;
                 If HiPick + pred(ListLines) > TotalPicks then
                    TopPick := TotalPicks - pred(ListLines)
                 else
                    TopPick := HiPick;
                 Display_All_Topics;
             end;
         end;

 begin
     Set_Parameters;
     Compute_Coords;
     Draw_List_Box;
     Display_All_Topics;
     Finished := false;
     Repeat
          ChL := GetKey;
          {$IFNDEF VER40}
          LTTT.Hook(ChL,HiPick);
          {$ELSE}
          If L_UserHook <> nil then
             CallFromListUserHook(ChL,HiPick);
          {$ENDIF}
          If ChL in LTTT.End_Chars then
             Finished := true
          else
              If ChL <> #0 then
              If (ChL in LTTT.Select_Chars) and LTTT.AllowToggle then
              begin
                   Selected[HiPick] := not Selected[HiPick];
                   Write_Topic(HiPick,True);
              end
              else
                 Case UpCase(ChL) of
                 #132,
                 #027: If LTTT.AllowEsc then       {Esc}
                          Finished := True;
                 #129,                             {Mouse_Down}
                 #208: begin                       {Down_Arrow}
                           Write_Topic(HiPick,False);
                           If HiPick < TotalPicks then
                              Inc(HiPick)
                           else
                              If (Scrolling = false) and (Chl <> #129) then
                                 HiPick := 1;
                           If HiPick > TopPick + Pred(ListLines) then
                           begin
                               Inc(TopPick);
                               Display_All_Topics;
                           end
                           else
                              Write_Topic(HiPick,True);
                       end;
                 #128,                             {Mouse_Up}
                 #200: begin                       {Up_Arrow}
                           Write_Topic(HiPick,False);
                           If HiPick > 1 then
                              Dec(HiPick)
                           else
                              If (Scrolling = false) and (Chl <> #128) then
                                 HiPick := TotalPicks;
                           If HiPick < TopPick then
                           begin
                               Dec(TopPick);
                               Display_All_Topics;
                           end
                           else
                              Write_Topic(HiPick,True);
                       end;
                 #199: If HiPick <> 1 then       {Home}
                       begin
                           HiPick := 1;
                           TopPick := 1;
                           Display_All_Topics;
                       end;
                 #207: If HiPick <> TotalPicks then   {end}
                       begin
                           HiPick := TotalPicks;
                           TopPick := HiPick - pred(ListLines);
                           Display_All_Topics;
                       end;
                 #201: If Scrolling then   {PgUp}
                       begin
                          If HiPick > ListLines then
                          begin
                             HiPick := HiPick - ListLines;
                             If TopPick > ListLines then
                                TopPick := TopPick - ListLines
                             else
                                TopPick := 1;
                          end
                          else
                          begin
                             HiPick := 1;
                             TopPick := 1;
                          end;
                          Display_All_Topics;
                      end
                      else
                      begin
                          If HiPick > 1 then
                          begin
                              Write_Topic(HiPick,False);
                              HiPick := 1;
                              Write_Topic(HiPick,True);
                          end;
                      end;
                 #209:If Scrolling then   {PgDn}
                      begin
                          If HiPick + ListLines <= TotalPicks then
                          begin
                             HiPick := HiPick + ListLines;
                             If TopPick + ListLines +pred(ListLines) > TotalPicks then
                                TopPick := TotalPicks - pred(ListLines)
                             else
                                TopPick := TopPick + ListLines;
                          end
                          else
                          begin
                             HiPick := TotalPicks;
                             TopPick := TotalPicks - pred(ListLines);
                          end;
                          Display_All_Topics;
                      end
                      else
                      begin
                          If HiPick < TotalPicks then
                          begin
                              Write_Topic(HiPick,False);
                              HiPick := TotalPicks;
                              Write_Topic(HiPick,True);
                          end;
                      end;
                 'A'..'Z' : Find_Capital_Letter(upcase(ChL));
                 end;  {case}
     Until Finished;
     L_Char := ChL;
     L_Picks := Selected;
     L_Pick := HiPick;
 end; {of proc New_Show_List}

 Procedure Show_List(var StrArray;StrLength:Byte;TotalPicks:byte);
 begin
    TopPick := 1;
    HiPick := 1;
    New_Show_List(StrArray,StrLength,TotalPicks);
 end; {Show_List}

begin
    Default_Settings;
end.
