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
                     {       Unit:  MenuTTT5          }
                     {--------------------------------}


{History:     2/13/89          Mod 5.00a changed Y2 calculation in proc
                               Determine_Y_Dimensions
                        5.01a  Removed refrences to VER50 and added DEBUG
                               compiler directive
             01/04/93   5.10   DPMI compatible version
}
{$ifdef fpc}
{$mode fpc}
{$endif fpc}


{$S-,R-,V-}
{$IFNDEF DEBUG}
{$D-}
{$ENDIF}

Unit MenuTTT5;

INTERFACE

Uses CRT, FastTTT5, DOS, WinTTT5, KeyTTT5, StrnTTT5;

const
   Max_Choices = 30;
   MenuStrLength = 40;     {make longer if necessary}
type
{$IFNDEF VER40}
   Menu_Hook = Procedure(var Ch:char; Choice:integer; var Ecode:integer);
{$ENDIF}
   Menu_record = record
                  Heading1     : string[MenuStrLength];   { '' for no heading}
                  Heading2     : string[MenuStrLength];
                  Topic        : array[1..Max_Choices] of string[MenuStrLength];
                  TotalPicks   : integer;
                  PicksPerLine : byte;
                  AddPrefix    : byte;                    {0 no, 1 No.'s, 2 Lets}
                  TopLeftXY    : array[1..2] of byte;     {X,Y}
                  Boxtype      : byte;                    {0,1,2,3, >3}
                  Colors       : array[1..5] of byte;     {HF,HB,LF,LB,Box}
                  Margins      : byte;
                  AllowEsc     : boolean;                 {true if Esc will exit}
                  {$IFNDEF VER40}
                  Hook         : Menu_hook;
                  {$ENDIF}
                end;
{$IFDEF VER40}
Var
  M_UserHook : Pointer;
{$ENDIF}
Procedure No_Hook(var Ch:char; Choice:integer; var Ecode : integer);
Procedure Menu_Set(var M : Menu_record);
Procedure DisplayMenu(MenuDef: Menu_record;
                      Window:Boolean;
                      var Choice,Errorcode : integer);

IMPLEMENTATION

{$IFDEF VER40}
   Procedure Call_Hook(var Ch:char; Choice:integer; var Ecode:integer);
          Inline($FF/$1E/M_UserHook);
{$ENDIF}

{$F+}
 Procedure No_Hook(var Ch:char; Choice:integer; var Ecode : integer);
 {}
 begin
 end; {of proc No_Hook}
{$F-}

 Procedure Menu_Set(var M : Menu_record);
 {}
 begin
     with M do
     begin
         Heading1     := '';
         Heading2     := '';
         Topic[1]     := '';
         TotalPicks   := 0;
         PicksPerLine := 1;
         AddPrefix    := 1;
         TopLeftXY[1] := 0;
         TopLeftXY[2] := 0;
         Boxtype      := 5;
         If ColorScreen then
         begin
             Colors[1]    := white;
             Colors[2]    := red;
             Colors[3]    := lightgray;
             Colors[4]    := blue;;
             Colors[5]    := lightred;
         end
         else
         begin
             Colors[1]    := white;
             Colors[2]    := black;
             Colors[3]    := lightgray;
             Colors[4]    := black;
             Colors[5]    := white;
         end;
         Margins      := 5;
         AllowEsc     := true;
         {$IFNDEF VER40}
                 Hook         := {$ifdef fpc}@{$endif fpc}NO_Hook;
         {$ELSE}
                 M_UserHook := Nil;
         {$ENDIF}
     end;
 end; {of proc Menu_Set}

 Procedure MenuError(Code:byte);    {fatal error -- msg and halt}
 var Message:string;
 begin
     {Clrscr;}
     Case Code of
     1 : Message := 'Fatal Error 1: Too Many Picks to display. Change PicksPerLine';
     else Message := 'Aborting';
     end; {case}
     WriteAT(1,12,black,lightgray,Message);
     Repeat Until keypressed;
     Halt;
 end;    {proc MenuError}

Procedure DisplayMenu(MenuDef: Menu_record;
                      Window:Boolean;
                      var Choice,Errorcode : integer);
Const
Alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
Numbers  = '123456789';
var
I,J,X2,Y2,heading_Lines : integer;
TextWidth : byte;


    Function Int_to_Str(Number:Integer):string;
    var Temp : string;
    begin
       Str(Number,temp);
       Int_to_Str := temp;
    end;

    Function  Str_to_Int(Str:string):integer;
    var temp,code : integer;
    begin
        If length(Str) = 0 then
           Str_to_Int := 0
        else
        begin
            val(Str,temp,code);
            if code = 0 then
               Str_to_Int := temp
            else
               Str_to_Int := 0;
        end;
    end;

   Procedure GetDimensions;
   var Fullwidth,MaxWidth: integer;

     Procedure Validate_Prefix;                          { 0   no prefix  }
     begin                                               { 1   numbers prefix}
         with MenuDef do                                 { 2   letters prefix}
         begin                                           { 3   function key prefix}
             If PicksPerLine < 1 then PicksPerLine := 1; { 4   capital letter selection}
             If (TotalPicks = 10) and (AddPrefix = 1) then
                AddPrefix := 3;
             If (TotalPicks > 10) and (AddPrefix in [1,3]) then
                AddPrefix := 2;
             If (Addprefix > 4) or (TotalPicks > 26) or (Addprefix < 0) then
                Addprefix := 0;
             end; {do}
     end; {Validate_Prefix}

   Procedure Add_Prefix;
   var I : integer;
   begin
       With MenuDef do
       begin
           Case AddPrefix of
           1 : for I := 1 to TotalPicks do
                   Topic[I] := int_to_str(I) + ' ' + Topic[I];
           2 : for I := 1 to TotalPicks do
                   Topic[I] := Copy(Alphabet,I,1) + ' ' + Topic[I];
           3 : If TotalPicks < 10 then
                  for I := 1 to TotalPicks do
                      Topic[I] := 'F'+Int_to_Str(I) + ' ' + Topic[I]
               else
               begin                           {add extra space for F10 }
                   for I := 1 to 9 do
                       Topic[I] := 'F'+Int_to_Str(I) + '  ' + Topic[I];
                   Topic[10] := 'F10 '+ Topic[10];
               end;
           end; {case}
       end;  {do}
   end;  {proc Add_Prefix}

     Procedure Find_Longest_Topic;
     var
       I,J: integer;
     begin
         with MenuDef do
         begin
             Textwidth := 0;
             For I := 1 to TotalPicks do
                 If length(Topic[I]) > TextWidth then
                    Textwidth := length(Topic[I]);         {find the longest text}
         end;  {with}
     end;   {Proc Find_Widest_Line}

   Procedure Adjust_Text_Width(Len:integer);
   var I,J : integer;
   begin
       With MenuDef do
       begin
           For I := 1 to TotalPicks do
               If length(Topic[I]) > Len then         {reduce it}
                  Delete(Topic[I],succ(Len),length(Topic[I]) - Len)
               else                                  {expand it}
                  For J := length(Topic[I]) + 1 to Textwidth do
                      Topic[I] :=  Topic[I] + ' ';
       end; {do}
   end;

   Procedure Determine_MaxWidth;
   {findout the max internal menu space - MaxWidth}
   begin
       with MenuDef do
       begin
           If margins < 0 then Margins := 0;
           If not (BoxType in [0..9]) then
              BoxType := 0;
           MaxWidth := 80 - 2*Margins - 1; {-1 for arrow symbol to left of pick}
           Case BoxType of
           1..4 : MaxWidth := MaxWidth - 2;     {box sides}
           5    : MaxWidth := pred(MaxWidth);    {box shadow}
           6..9 : MaxWidth := MaxWidth - 3;     {box sides and shadow}
           end;
       end; {with}
   end;

   Procedure Validate_PicksPerLine;
   begin
       With MenuDef do
       begin
           If succ(TextWidth)*PicksPerLine <= MaxWidth then
              exit;  {no adjustment necessary, everything fits}
           If (TextWidth-2)*PicksPerLine <= Maxwidth  then
               TextWidth := pred(MaxWidth div PicksperLine)
           else
           begin
               While succ(TextWidth)*PicksPerLine > MaxWidth do
                     PicksPerLine := pred(PicksPerLine);
               If PicksPerLine = 0 then
               begin
                   TextWidth := pred(MaxWidth);
                   PicksPerLine := 1;
               end;
           end;
       end; {with}
   end;  {Proc Validate_PicksPerLine}

   Procedure Determine_X_Dimensions;
   {Checks to see if the menu will fit, if it won't it changes something!}
   begin
       With MenuDef do
       begin
           Fullwidth := succ(Textwidth)*PicksPerLine + 2*Margins;
           Case BoxType of
           1..4 : FullWidth := FullWidth + 2;     {box sides}
           5    : FullWidth := succ(FullWidth);   {box shadow}
           6..9 : FullWidth := FullWidth + 3;     {box sides and shadow}
           end; {Case}
           If TopleftXY[1] < 1 then
              TopleftXY[1] := (80 - Fullwidth)  div 2;
           If TopLeftXY[1] + Fullwidth < 80 then
              X2 := TopleftXY[1] + Fullwidth
           else
           begin
               X2 := 80;
               TopLeftXY[1] := 80 - Fullwidth + 1;
           end;
       end; {with}
   end; {Proc Determine_X_Dimensions}

   Procedure Determine_Y_Dimensions;
   var
      BoxLines,
      TopicLines,
      FullDepth  : integer;
   begin
       With MenuDef do
       begin
           TopicLines := TotalPicks div PicksPerLine;  {no of full rows of picks}
           If TotalPicks mod PicksPerLine > 0 then     {+1 if partial row of picks}
              TopicLines := succ(TopicLines);
           Case BoxType of
           0    : Boxlines := 0;
           1..5 : BoxLines :=  2;     {box sides}
           6..9: BoxLines :=  3;     {box sides and shadow}
           end;
           Heading_Lines := 0;
           If length(Heading1) > 0 then
              Heading_Lines := succ(Heading_Lines);
           If length(Heading2) > 0 then
              Heading_Lines := succ(Heading_Lines);
           If Heading_Lines > 0 then                   {add a line for a gap}
              Heading_Lines := succ(Heading_Lines);    {gap above topics}
           If BoxType = 5 then
              Heading_Lines := succ(Heading_Lines);
           Fulldepth := BoxLines+TopicLines+Heading_Lines;
           If Heading_Lines > 0 then
             Fulldepth := succ(Fulldepth);  {+1 gap below topics if headings}
           If FullDepth > DisplayLines then   {if it doesn't fit, drop off topics}
           begin
               If Heading_Lines > 0 then
                  TotalPicks :=  (DisplayLines - BoxLines -Heading_Lines-1)*PicksPerLine
               else
                  TotalPicks :=  (DisplayLines - BoxLines - Heading_Lines)*PicksPerLine;
               FullDepth := 25;
           end;
           If TopLeftXY[2] <= 0 then
              TopLeftXY[2] := (DisplayLines - Fulldepth) div 2 +1;
           If TopLeftXY[2] + Fulldepth - 1 <= DisplayLines then
           begin
               If BoxType > 4 then   {shadow}
                  Y2 := TopleftXY[2] + (Fulldepth) - 2     {Mod 5.00a}
               else
                  Y2 := TopleftXY[2] + pred(Fulldepth);    {Mod 5.00a}
           end
           else
           begin
               If BoxType > 4 then   {shadow}
                  Y2 := pred(DisplayLines)
               else
                  Y2 := DisplayLines;
               TopLeftXY[2] := DisplayLines - Fulldepth {+ 1};   {WZ}
           end;
   end;   {do}
   end; {Proc Determine_Y_Dimensions}

   begin                              {Get_Dimensions}
       Validate_Prefix;
       Add_Prefix;
       Find_Longest_Topic;
       Determine_MaxWidth;
       Validate_PicksPerLine;
       Adjust_Text_Width(TextWidth);
       Determine_X_Dimensions;
       Determine_Y_Dimensions;
   end;   {proc GetDimensions}

   Procedure Write_Text(Item:integer;Highlight:boolean);
   Var X,Y,A:integer;
   begin
       With MenuDEf do
       begin
           A := Item mod PicksPerLine;
           Y := Item div PicksPerLine +TopleftXY[2] + ord(A <> 0);
           Y := Y + Heading_lines - ord(Boxtype = 0);
           If A = 0 then A := PicksPerLine;      {A is now the no of picks from left}
           X := (A - 1)*(TextWidth + 1)+Margins+
                TopleftXY[1]+1 + ord(BoxType > 0);          {title width + 1 for a space}
           If Highlight then
           begin
               WriteAt(X,Y,colors[1],colors[2],Topic[item]);
               WriteAT(pred(X),Y,colors[5],colors[2],chr(16));  {write arrow head}
           end
           else
           begin
               WriteAT(X,Y,colors[3],colors[4],Topic[item]);
               WriteAT(pred(X),Y,colors[3],colors[4],' ');       {remove arrow head}
               If AddPrefix = 4 then                             {highlight the capital letter}
                  WriteAT(Pred(X)+First_Capital_Pos(Topic[Item]),Y,
                          colors[1],colors[4],
                          First_Capital(Topic[Item]));
           end;
       end;  {do}
   end;  {Proc Write_Text}

   Procedure CreateMenu;
   var I : integer;
   begin
   with MenuDef do
   begin
    If Window then
           MkWin(TopleftXY[1],TopLeftXY[2],X2,Y2,colors[3],colors[4],boxtype)
    else
    begin
        ClearText(TopleftXY[1],TopLeftXY[2],X2,Y2,colors[3],colors[4]);
        If (BoxType in [5..9]) and (TopleftXY[1] > 1) then      {draw a shadow}
        begin
            For I := TopleftXY[2]+1 to Y2+1 do
                WriteAt(pred(TopLeftXY[1]),I,colors[3],black,' ');
            WriteAt(TopLeftXY[1],succ(Y2),colors[3],black,
                replicate(X2-succ(TopLeftXY[1]),' '));
        end;
    end;
    Case Boxtype of
    1..4: Box(TopLeftXY[1],TopLeftXY[2],X2,Y2,colors[5],colors[4],Boxtype);
    5   : begin
              WriteAT(TopleftXY[1],TopleftXY[2],colors[5],colors[4],
                      replicate(succ(X2 - TopleftXY[1]),chr(223)));
              WriteAT(TopleftXY[1],TopleftXY[2]+Heading_Lines-1,colors[5],colors[4],
                      replicate(succ(X2 - TopleftXY[1]),chr(196)));
          end;
    6..9:Box(TopLeftXY[1],TopLeftXY[2],X2,Y2,colors[5],colors[4],Boxtype-5);
    end; {case}

    If length(Heading1) > 0 then
       WriteBetween(TopleftXY[1],X2,
                    TopLeftXY[2]+ord(BoxType > 0),
                    colors[1],colors[4],Heading1);
    If length(Heading2) > 0 then
       WriteBetween(TopleftXY[1],X2,
                    TopLeftXY[2]+ord(BoxType > 0)+ord(Heading_Lines <> 2),
                    colors[1],colors[4],Heading2);
    For I := 1 to TotalPicks do
        Write_Text(I,false);
    Write_Text(Choice,True);       {Highlight Default}
   end; {do}
   end; {Proc CreateMenu}

   Procedure Process_Keystrokes;
   var
     Found,
     Selected: Boolean;
     ChT,
     CHpk:char;
     Oldchoice:integer;
     I,
     Ecode:integer;
     ScanTop,ScanBot,Cx,Cy : byte;
   begin
       Selected := false;
       Found := false;
       FindCursor(Cx,Cy,ScanTop,ScanBot);
       OffCursor;
       With MenuDef do
       begin
       Repeat
             Chpk := GetKey;
{$IFNDEF VER40}
             MenuDef.Hook(Chpk,Choice,Ecode);   {call the user hook}
{$ELSE}
             If M_UserHook <> Nil then
                Call_Hook(Chpk,Choice,Ecode);
{$ENDIF}
             Case upcase(CHpk) of
             #208 : begin       {Cursor Down}
                        Write_text(Choice,false);
                        Choice := Choice + PicksPerLine;
                        If Choice > TotalPicks then
                           Choice := (Choice mod PicksPerLine) + 1;
                        Write_Text(Choice,true);
                    end;
             #129 : If Choice + PicksPerLine  <= TotalPicks then  {Mouse Down}
                    begin
                        Write_text(Choice,false);
                        Choice := Choice + PicksPerLine;
                        Write_Text(Choice,true);
                    end;
             #200 : begin       {cursor up}
                        Write_Text(Choice,false);
                        Choice := Choice - PicksPerLine;
                        If Choice < 1 then
                        begin
                           Choice := Choice + PicksPerline;
                           Choice :=
                             ((TotalPicks div PicksPerLine)*PicksPerLine)
                             - PicksPerLine + 1 + Choice - 2;
                           If Choice + PicksPerLine <= TotalPicks then
                              Choice := Choice + PicksPerLine;   {phew!}
                        end;
                        Write_Text(Choice,true);
                    end;
             #128 : If Choice - PicksPerLine > 0 then   {Mouse up}
                    begin
                        Write_Text(Choice,false);
                        Choice := Choice - PicksPerLine;
                        Write_Text(Choice,true);
                    end;
             #203 : begin       {cursor left}
                        Write_Text(Choice,False);
                        Choice := pred(choice);
                        If choice = 0 then Choice := TotalPicks;
                        Write_Text(Choice,true);
                    end;
             #130 : If (pred(Choice) > 0)  {mouse left}
                    and ( Choice mod PicksPerLine <> 1) then
                    begin
                        Write_Text(Choice,False);
                        Choice := pred(choice);
                        Write_Text(Choice,true);
                    end;
             ' ',
             #205 : begin        {cursor right}
                        Write_Text(Choice,false);
                        Choice := succ(Choice);
                        If choice > TotalPicks then Choice := 1;
                        Write_Text(Choice,true);
                    end;
             #131 : If (succ(Choice) <= TotalPicks) {Mouse right}
                    and ( Choice mod PicksPerLine <> 0) then
                    begin
                        Write_Text(Choice,false);
                        Choice := succ(Choice);
                        Write_Text(Choice,true);
                    end;
             #199 : begin         {home key}
                        Write_Text(Choice,false);
                        Choice := 1;
                        Write_Text(Choice,true);
                    end;
             #207 : begin         {end key}
                        Write_Text(Choice,false);
                        Choice := TotalPicks;
                        Write_Text(Choice,true);
                    end;
             #133,                 {Mouse enter}
             #13  : begin          {enter key}
                        Selected := true;
                        Errorcode := 0;
                    end;
             #0   : begin
                        Selected := true;
                        ErrorCode := Ecode;
                    end;
             #132,                    {Mouse Esc}
             #27  : If AllowEsc then  {Esc}
                    begin
                        Selected := true;
                        ErrorCode := 1;
                    end
                    else
                    begin
                        Write_Text(Choice,false);
                        Choice := TotalPicks;
                        Write_Text(Choice,true);
                    end;
             #187..#196 : If Addprefix = 3 then   {F1 to F10}
                          begin
                              Oldchoice := Choice;
                              Case Upcase(Chpk) of
                              #187 : If TotalPicks >= 1  then choice := 1 else choice := 0;
                              #188 : If TotalPicks >= 2  then choice := 2 else choice := 0;
                              #189 : If TotalPicks >= 3  then choice := 3 else choice := 0;
                              #190 : If TotalPicks >= 4  then choice := 4 else choice := 0;
                              #191 : If TotalPicks >= 5  then choice := 5 else choice := 0;
                              #192 : If TotalPicks >= 6  then choice := 6 else choice := 0;
                              #193 : If TotalPicks >= 7  then choice := 7 else choice := 0;
                              #194 : If TotalPicks >= 8  then choice := 8 else choice := 0;
                              #195 : If TotalPicks >= 9  then choice := 9 else choice := 0;
                              #196 : If TotalPicks >= 10 then choice := 10 else choice := 0;
                              end;  {case}
                              If Choice = 0 then
                                 Choice := Oldchoice
                              else
                              begin
                                  Write_Text(Oldchoice,false);
                                  Write_Text(Choice,true);
                                  Selected := true;
                                  Errorcode := 0;
                              end;
                          end;
             '0'..'9': If (AddPrefix in [1,3]) then   {Number or Function Prefix} {4.02}
                       begin
                           If (Str_to_int(CHpk) in [1..TotalPicks]) then
                           begin
                               Write_Text(Choice,false);
                               Choice := Str_to_Int(CHpk);
                               Write_Text(Choice,true);
                               Selected := true;
                               ErrorCode := 0;
                           end;
                       end;
             'A'..'Z': If AddPrefix = 2 then
                       begin
                          If (pos(upcase(CHpk),Alphabet) in [1..TotalPicks]) then
                          begin
                              Write_Text(Choice,false);
                              Choice := pos(upcase(CHpk),Alphabet);
                              Write_Text(Choice,true);
                              Selected := true;
                              Errorcode := 0;
                          end;
                       end
                       else
                       begin
                           If AddPrefix = 4 then
                           begin
                               Found := false;
                               I := Choice;
                               Repeat
                                    If First_Capital(Topic[I]) = upcase(ChPk) then
                                    begin
                                        Found := true;
                                        Write_Text(Choice,false);
                                        Choice := I;
                                        Write_Text(Choice,true);
                                        Selected := true;
                                        Errorcode := 0;
                                    end
                                    else
                                        If I = TotalPicks then
                                           I := 1
                                        else
                                           Inc(I);
                               Until Found or (I = Choice);
                           end;
                        end;
                    end;
       Until Selected;
       SizeCursor(ScanTop,ScanBot);
       end; {do}
  end; {proc Process_keystrokes}

begin
   GetDimensions;
   CreateMenu;
   Horiz_Sensitivity := 2;  {two cursors left/right before mouse returns a keypress}
   Process_Keystrokes;
   If Window then RmWin;
end;        {Main Procedure DisplayMenu}

begin
end.
