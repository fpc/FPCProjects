{--------------------------------------------------------------------------}
{                         TechnoJock's Turbo Toolkit                       }
{                                                                          }
{                              Version   5.10                              }
{                                (Europe)                                  }
{                                                                          }
{               Copyright 1986-1993 TechnoJock Software, Inc.              }
{                           All Rights Reserved                            }
{                          Restricted by License                           }
{--------------------------------------------------------------------------}

                     {--------------------------------}
                     {       Unit:  ReadTTT5          }
                     {--------------------------------}


{History:         2/24/89   5.00a  Reversed return codes in ReadLine
                  3/05/89   5.00b  Added Box proc to Read_Real
                            5.01a  Added DEBUG compiler directive and added
                                   mouse Enter/Esc support
                 01/04/93   5.10   DPMI compatible version
}
{$ifdef fpc}
{$mode tp}
{$endif fpc}


{$S-,R-,V-}
{$IFNDEF DEBUG}
{$D-}
{$ENDIF}

Unit ReadTTT5;

Interface

Uses CRT,FastTTT5,WinTTT5,StrnTTT5,KeyTTT5;

Type
   R_Display = record
                    WhiteSpace  : char;        {used to pad input field - default תתתתתתתתתת}
                    AllowEsc    : boolean;     {allow the he user to escape?}
                    Beep        : Boolean;     {allow the old proverbial beep}
                    Insert      : boolean;     {initially in insert mode?}
                    BegCursor   : boolean;     {place cursor at beginning of line}
                    AllowNull   : boolean;     {allow user to input a '' or null value}
                    RightJustify: Boolean;     {right justify string on termination}
                    EraseDefault: Boolean;     {clear entry of alphanumeric pressed}
                    SuppressZero: Boolean;     {have empty field is value = zero}
                    FCol        : byte;        {normal foreground color of input field}
                    BCol        : byte;        {normal background of input field}
                    HiFCol      : byte;        {highlighted fgnd color for Read_Select}
                    HiBCol      : byte;        {highlighted bgnd color for Read_Select}
                    LoFCol      : byte;        {normal fgnd color for Read_Select}
                    LoBCol      : byte;        {normal bgnd color for Read_Select}
                    PFcol       : byte;        {prompt foreground color}
                    PBCol       : byte;        {prompt background color}
                    BoxFCol     : byte;        {box foreground color}
                    BoxBCol     : byte;        {Box background color}
                    Msg_FCol    : byte;        {Foreground color for error messages}
                    Msg_BCol    : byte;        {Background color for error messages}
                    Msg_Line    : byte;        {line for error messages}
                    End_chars   : set of char; {end of input chars}
                    RealDP      : byte;        {no of decimal places on real}
               end;

const
    NoPrompt:string[1] = '';
Var
  RTTT : R_Display;
  R_Char : char;
  R_Null : boolean;

Procedure Default_Settings;
Procedure ReadLine(X,Y,L,F,B:byte;var Text: string;var Retcode:integer);
Procedure Read_String(X,Y,L:byte;
                      Prompt:StrScreen;
                      BoxType: byte;
                      Var Txt:StrScreen);
Procedure Read_String_Upper(X,Y,L:byte;
                            Prompt:StrScreen;
                            BoxType: byte;
                            Var Txt:StrScreen);
Procedure Read_Password(X,Y,L:byte;
                        Prompt:StrScreen;
                        BoxType: byte;
                        Var Txt:StrScreen);
Procedure Read_Alpha(X,Y,L:byte;
                     Prompt:StrScreen;
                     BoxType: byte;
                     Var Txt:StrScreen);
Procedure Read_YN(X,Y:byte;
                  Prompt:StrScreen;
                  BoxType: byte;
                  Var Yes:Boolean);
Procedure Read_Byte(X,Y,L:byte;
                    Prompt:StrScreen;
                    BoxType: byte;
                    Var B : Byte;
                    Min, Max : Byte);
Procedure Read_Word(X,Y,L:byte;
                    Prompt:StrScreen;
                    BoxType: byte;
                    Var W : word;
                    Min, Max : word);
Procedure Read_Int(X,Y,L:byte;
                   Prompt:StrScreen;
                   BoxType: byte;
                   Var W : integer;
                   Min, Max : integer);
Procedure Read_LongInt(X,Y,L:byte;
                       Prompt:StrScreen;
                       BoxType: byte;
                       Var W : longint;
                       Min, Max : longint);
Procedure Read_Real(X,Y,L:byte;
                    Prompt:StrScreen;
                    BoxType: byte;
                    Var W : real;
                    Min, Max : real);
Procedure Read_Select(X,Y:byte;Pmt,Txt:StrScreen;var Choice:byte);
Implementation

CONST
    PassChar    = #15;
    CursorRight = #205;
    CursorLeft  = #203;
    CursorDown  = #208;
    CursorUp    = #200;
    EnterKey    = #13;
    EscKey      = #27;
    EndKey      = #207;
    HomeKey     = #199;
    DelKey      = #211;
    Backspace   = #8;
    InsKey      = #210;
    Zap         = #160;      {Alt D to delete the field}
    MinInt              = -32768;
    MaxLongInt:longint  =  2147483647;
    MinLongInt:longint  = -2147483647;
    MaxWord             =  65535;
    MinWord             =  0;

VAR
   Cursor_X,
   Cursor_Y,
   ScanTop,
   ScanBot   : byte;

Procedure Default_Settings;
begin
   with RTTT do
   begin
       WhiteSpace   := #250;
       Beep         := true;
       BegCursor    := false;
       Insert       := false;
       AllowEsc     := true;
       AllowNull    := true;
       RightJustify := false;
       EraseDefault := false;
       SuppressZero := true;
       End_Chars := [#13,#133];  {Enter}
       RealDP := 2;
       If not ColorScreen then
       begin
           FCol := black;
           BCol := lightgray;
           HiFCol := white;
           HiBCol := black;
           LoFCol := lightgray;
           LoBCol := black;
           PFCol := white;
           PBCol := black;
           BoxFCol := white;
           BoxBCol := black;
           Msg_FCol := white;
           Msg_BCol := black;
           Msg_Line := 0;
       end
       else
       begin
           FCol := black;
           BCol := lightgray;
           HiFCol := black;
           HiBCol := lightgray;
           LoFCol := lightgray;
           LoBCol := black;
           PFCol := white;
           PBCol := black;
           BoxFCol := white;
           BoxBCol := black;
           Msg_FCol := lightred;
           Msg_BCol := black;
           Msg_Line := 0;
       end;
   end;
end;

Procedure Clang;
begin
    If RTTT.Beep then
    begin
        sound(500);
        delay(50);
        nosound;
    end;
end;

Procedure Read_Line(X,Y,L,F,B,Format:byte;
                     var Text   :string);
{
X is X coord of first character in field
Y is Y coord of field
L is the maximum length of the input field
F is the foreground color
B is the background color
Fornat Codes:      1   Any String
                   2   Force Upper String
                   3   Yes/No
                   4   Alphabetics only
                   5   Integer
                   6   LongInteger
                   7   Real
                   8   Word
                   (*   Maybe
                   9   Date    (MM/DD/YY)
                   10  Date    (DD/MM/YY)
                   *)
                   11  Echo a Password
Text is a string updated with the string equivalent of user input
}
var
    TempText : string;
    CursorPos : byte;
    InsertMode,
    Password,
    Alldone : boolean;
    FirstCharPress: boolean;
    Ch : char;

    Procedure Check_Parameters;
    begin
        TempText := Text;
        If length(TempText) > L then
           Delete(Temptext,L+1,length(TempText)-L);
        If not X in [1..80] then
           X := 1;
        If X + L - 1 > 80 then X := 81 - L;
        If not Y in [1..25] then
           Y := 1;
        If RTTT.BegCursor then
           CursorPos := 1
        else
        begin
            If length(TempText) < L then
               CursorPos := length(TempText) + 1
            else
               CursorPos := length(TempText);
        end;
        InsertMode  := RTTT.Insert;
        Alldone := False;
        If Format = 11 then
        begin
            Password := true;
            Format := 1;
        end
        else
           Password := false;
    end;  {sub Proc Check_Parameters}

    Function FillWhiteSpace(Str:string):string;
    var I : integer;
    begin
        If Password then
           Str := replicate(length(Str),PassChar);
        while length(Str) < L do
              Str := Str + RTTT.WhiteSpace;
        FillWhiteSpace := Str;
    end; {sub Func FillWhiteSpace}

    Procedure MoveTheCursor;
    begin
        GotoXY(X+CursorPos-1,Y);
    end;  {sub Proc MoveTheCursor}

    Procedure Write_String;
    begin
        Fastwrite(X,Y,attr(F,B),FillWhiteSpace(TempText));
        MoveTheCursor;
    end;

    Procedure Erase_Field;
    begin
        TempText := '';
        CursorPos := 1;
        Write_String;
    end;

    Procedure Char_Backspace;
    begin
        If CursorPos > 1 then
        begin
            CursorPos := Pred(CursorPos);
            Delete(TempText,CursorPos,1);
            Write_String;
       end;
    end;   {sub Proc Char_Backspace}

    Procedure Char_Del;
    begin
        If CursorPos <= length(TempText) then
        begin
            Delete(TempText,CursorPos,1);
            Write_String;
        end;
    end;   {sub Proc Char_Del}

    Procedure Add_Char(Ch:char);
    begin
        If InsertMode then
        begin
            If length(TempText) < L then
            begin
                Insert(Ch,TempText,CursorPos);
                If CursorPos < L then
                   CursorPos := Succ(CursorPos);
           end;
        end
        else {not insertmode}
        begin
            Delete(TempText,CursorPos,1);
            Insert(Ch,TempText,CursorPos);
            If CursorPos < L then
               CursorPos := Succ(CursorPos);
        end;   {if insert}
        Write_String;
    end;   {sub proc Add_Char}


begin                  {main Procedure Read_Line}
    Check_Parameters;
    R_Null := false;
    FindCursor(Cursor_X,Cursor_Y,ScanTop,ScanBot);
    If RTTT.Insert then
       HalfCursor
    else
       OnCursor;
    Write_String;
    FirstCharPress := true;
    Repeat
         Ch := Getkey;
         If Format in [2,3] then
            Ch := upcase(Ch);
         If Ch in RTTT.End_Chars then
         begin
            AllDone := True;
            If Ch <> #027 then Text := TempText;
         end
         else
         if Extended then
         begin
             Case Ch of
             #131,              {mouseright}
             CursorRight   :  begin
                                  If (CursorPos < L)
                                  and (CursorPos <= length(TempText)) then
                                  begin
                                      CursorPos := Succ(CursorPos);
                                      MoveTheCursor;
                                  end;
                              end;
             #130,               {mouseleft}
             CursorLeft    :  begin
                                  If CursorPos > 1 then
                                  begin
                                      CursorPos := Pred(CursorPos);
                                      MoveTheCursor;
                                  end;
                              end;
             HomeKey       :  begin
                                  CursorPos := 1;
                                  MoveTheCursor;
                              end;
             EndKey        :  begin
                                  If CursorPos < L then
                                  If length(TempText) < L then
                                      CursorPos := length(TempText) + 1
                                  else
                                      CursorPos := L;
                                  MoveTheCursor;
                              end;
            InsKey        :  If Format <> 3 then   {don't allow insert on Y/N!}
                             begin
                                 InsertMode := not InsertMode;
                                 If InsertMode then
                                    HalfCursor
                                 else
                                    OnCursor;
                             end;
            DelKey        :  Char_Del;
            Zap           :  Erase_Field;
            #132          :  If RTTT.AllowEsc then
                                 Alldone := true
                             else
                                Clang;
            #133          :  begin
                                 Alldone := true;
                                 Text := TempText;
                             end;
            #128,#129     :;    {absorb stray mouse movement to avoid Clang'n}
          else Clang;
      end; {case}
      end
      else  {not extended}
      begin
          Case Ch of
           BackSpace     :  Char_Backspace;
           EnterKey      :  begin
                                 Alldone := true;
                                 Text := TempText;
                            end;
           { moved from the extended part (esc is not an extended key, not }
           { under TP nor under FPC (JM)                                   }
           EscKey        :  If RTTT.AllowEsc then
                                 Alldone := true
                             else
                                Clang;
           #33 .. #42,                                 {! to *}
           #44,#47,                                    {, /}
           #58 .. #64,                                 {: to @}
           #91 .. #96,                                 {[ to '}
           #123 .. #126   :  If (Format in [1,2]) then {{ to ~}
                             begin
                                 If FirstCharPress and RTTT.EraseDefault then
                                    Erase_Field;
                                 Add_Char(Ch);
                             end
                             else
                                 Clang;
           #43, #45       : If (Format in [1,2])       { + - }
                            or ( (CursorPos=1) and (Format in [5,6,7])) then
                            begin
                                If FirstCharPress and RTTT.EraseDefault then
                                    Erase_Field;
                                Add_Char(Ch);
                            end
                            else
                               Clang;
           #46            : If (Format in [1,2])       {.}
                            or ( (Pos('.',TempText)=0) and (Format = 7)) then
                            begin
                                If FirstCharPress and RTTT.EraseDefault then
                                    Erase_Field;
                                Add_Char(Ch);
                            end
                            else
                               Clang;
           #48..#57       : If (Format in [1..2,5..8]) then {0 to 9}
                            begin
                                If FirstCharPress and RTTT.EraseDefault then
                                    Erase_Field;
                                Add_Char(Ch);
                            end
                            else
                               Clang;
           #32,                                              {space}
           #65..#77,                                         {A to M}
           #79..#88,                                         {O to X}
           #90,                                              {Z}
           #97..#122,#127..#255:
                            If (Format in [1,2,4]) then      {a to z}
                            begin
                                If FirstCharPress and RTTT.EraseDefault then
                                    Erase_Field;
                                Add_Char(Ch);
                            end
                            else
                               Clang;
           #78,#89        : If (Format in [1..4]) then        {N Y}
                            begin
                                Add_Char(Ch);
                                If Format = 3 then
                                begin
                                    Alldone := true;
                                    Text := TempText;
                                end;
                            end
                            else
                               Clang;

          end; {case}
      end;
      FirstCharPress := false;
      Until Alldone;
      R_Char := Ch;
      If  RTTT.RightJustify
      and (Format > 4) then
      begin
          Fastwrite(X,Y,attr(F,B),replicate(L,RTTT.Whitespace));
          Fastwrite(X+L-Length(TempText),Y,attr(F,B),Text);
      end
      else
        Fastwrite(X,Y,attr(F,B),FillWhiteSpace(Text));
      GotoXY(Cursor_X,Cursor_Y);
      SizeCursor(ScanTop,ScanBot);
end;  {Proc Read_Line}

Procedure Display_Box_and_Prompt(var X1,Y: byte;
                                 BoxType:byte;
                                 Prompt: StrScreen;
                                 L:byte);
{ensures that the input will fit on the screen, then draws box and prompt}
const
   Upchar = '^';
   DnChar = '_';
var
  P,
  width:byte;
  InBorder : byte;    {is title in box border - 0 no, 1 upper, 2 lower}
begin
    If not ( (Y-ord(BoxType > 0)) in [1..DisplayLines] ) then
       Y := 2;
    If (X1 < 1) then
       X1 := 2;
    P := length(Prompt);
    If (P > 1) and (Boxtype > 0) then    {check and see if prompt is in box}
    begin
       If Prompt[1] = Upchar then
       begin
           delete(Prompt,1,1);
           dec(P);
           InBorder := 1;
       end
       else
          If Prompt[1] = DnChar then
          begin
              delete(Prompt,1,1);
              dec(P);
              InBorder := 2;
          end
          else
             InBorder := 0;
    end
    else
       InBorder := 0;
    If InBorder > 0 then                      {determine dimensions of box}
    begin
        If P > L then
           width := succ(P)
        else
           width := succ(L);
    end
    else
       width := succ(P+l);
    If pred(X1 + width) > 80 then
       X1 :=  succ(80 - width);
    If BoxType > 0 then         {draw the box}
       FBox(X1,pred(Y),X1+width,succ(Y),RTTT.BoxFCol,RTTT.BoxBCol,BoxType);
    If P > 0 then               {Draw the prompt}
        Case InBorder of
        0 : If BoxType> 0 then
               Fastwrite(succ(X1),Y,attr(RTTT.PFcol,RTTT.PBCol),Prompt) {left Justified in upper border}
            else
               Fastwrite(X1,Y,attr(RTTT.PFcol,RTTT.PBCol),Prompt);
        1 : FastWrite(succ(X1),pred(Y),attr(RTTT.PFcol,RTTT.PBCol),Prompt);
        2 : FastWrite(X1+width-P,succ(Y),attr(RTTT.PFcol,RTTT.PBCol),Prompt);   {right justified in lower border}
        end;
    If InBorder > 0 then        {return var X1 adjusted to position of input field}
    begin
       If Boxtype > 0 then
          X1 := succ(X1);
    end
    else
    begin
       If Boxtype > 0 then
          X1 := succ(X1) + p
       else
          X1 := X1 + P;
    end;
end;
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

 Procedure ReadLine(X,Y,L,F,B:byte;var Text: string;var Retcode:integer);
 {compatibility module with TTT 4.0}
 begin
     Read_Line(X,Y,L,F,B,1,Text);
     If R_Char = #027 then
        RetCode := 1         {5.00a}
     else
        Retcode := 0;        {5.00a}
 end; {of proc ReadLine}


Procedure Read_String(X,Y,L:byte;
                      Prompt:StrScreen;
                      BoxType: byte;
                      Var Txt:StrScreen);
begin
    Display_Box_and_Prompt(X,Y,Boxtype,Prompt,L);
    Read_Line(X,Y,L,RTTT.FCol,RTTT.BCol,1,Txt);
end;

Procedure Read_String_Upper(X,Y,L:byte;
                            Prompt:StrScreen;
                            BoxType: byte;
                            Var Txt:StrScreen);
begin
    Txt :=  Upper(Txt);
    Display_Box_and_Prompt(X,Y,Boxtype,Prompt,L);
    Read_Line(X,Y,L,RTTT.FCol,RTTT.BCol,2,Txt);
end;

Procedure Read_Password(X,Y,L:byte;
                        Prompt:StrScreen;
                        BoxType: byte;
                        Var Txt:StrScreen);
begin
    Display_Box_and_Prompt(X,Y,Boxtype,Prompt,L);
    Read_Line(X,Y,L,RTTT.FCol,RTTT.BCol,11,Txt);
end;

Procedure Read_Alpha(X,Y,L:byte;
                     Prompt:StrScreen;
                     BoxType: byte;
                     Var Txt:StrScreen);
begin
    Display_Box_and_Prompt(X,Y,Boxtype,Prompt,L);
    Read_Line(X,Y,L,RTTT.FCol,RTTT.BCol,4,Txt);
end;

Procedure Read_YN(X,Y:byte;
                  Prompt:StrScreen;
                  BoxType: byte;
                  Var Yes:Boolean);

var
  Global_Insert : boolean;
  Txt : StrScreen;
begin
    If Yes then
       Txt := 'Y'
    else
       Txt := 'N';
    Global_Insert := RTTT.insert;
    RTTT.Insert := false;            {force to overwrite mode}
    Display_Box_and_Prompt(X,Y,Boxtype,Prompt,1);
    Read_Line(X,Y,1,RTTT.FCol,RTTT.BCol,3,Txt);
    RTTT.Insert := Global_Insert;    {reset back}
    If Txt = 'Y' then
       Yes := true
    else
       Yes := false;
end;

{\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\}

Procedure Invalid_Message(Y : byte; var CH : char);
begin
   Clang;
   TempMessageCH(1,Y,RTTT.Msg_Fcol,RTTT.Msg_BCol,
               PadCenter('Invalid number - press any key to resume',80,' '),CH);
end;

Procedure OutOfRange_Message(Y : byte;MinS,MaxS : StrScreen;var CH:char);
var S : StrScreen;
begin
   Clang;
   S := 'Error value must be in the range '+MinS+' to '+MaxS+' - press any key to resume';
   TempMessageCh(1,Y,RTTT.Msg_Fcol,RTTT.Msg_BCol,PadCenter(S,80,' '),CH);
end;

Function MessageLine(Y : byte):byte;
begin
    If (RTTT.Msg_Line = 0) or (RTTT.Msg_Line > DisplayLines) then
    begin
        If Y < DisplayLines then    {set message Line}
           MessageLine := succ(Y)
        else
           MessageLine := pred(Y);
    end
    else
       MessageLine := RTTT.Msg_Line;
end;

{\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\}

Procedure Read_Byte(X,Y,L:byte;
                    Prompt:StrScreen;
                    BoxType: byte;
                    Var B : byte;
                    Min, Max : byte);
var
   Temp : byte;
   Txt : StrScreen;
   Valid : boolean;
   Code : integer;
   YT : byte;
   CHB : char;
begin
    If Max = 0 then
      Max := 255;
    If Min >= Max then
       Min := 0;
    If (B < Min) or (B > Max) then
        B := Min;
    If ((B = 0) and RTTT.SuppressZero) then
       Txt := ''
    else
       Txt := Int_To_Str(B);
    Temp := B;
    Valid := false;
    Display_Box_and_Prompt(X,Y,Boxtype,Prompt,L);
    YT := MessageLine(Y);
    Repeat
         Read_Line(X,Y,L,RTTT.FCol,RTTT.BCol,8,Txt);
         If ((R_Char = #027) and RTTT.AllowEsc)
         or ((Txt = '') and (RTTT.AllowNull)) then
         begin
             If Txt = '' then R_Null := true;
             exit;
         end
         else
         begin
             val(Txt,Temp,code);
             If code <> 0 then
             begin
                Invalid_Message(YT,CHB);
                If ChB = #027 then
                        Txt := Int_To_Str(B);
             end
             else
             begin
                 If (Temp < Min)
                 or (Temp > Max)
                 or ((length(Txt) > 2) and (Txt > '255')) then
                 begin
                    OutOfRange_Message(Yt,Int_To_Str(Min),Int_To_Str(Max),CHB);
                    If ChB = #027 then
                        Txt := Int_To_Str(B);
                 end
                 else
                 begin
                     B := temp;
                     Valid := true;
                 end;
             end;
         end;
    Until Valid or ((R_Char = #027) and RTTT.AllowEsc);
end;

Procedure Read_Word(X,Y,L:byte;
                    Prompt:StrScreen;
                    BoxType: byte;
                    Var W : word;
                    Min, Max : word);
var
   Temp : word;
   Txt : StrScreen;
   Valid : boolean;
   Code : integer;
   YT : byte;
   ChW : char;
begin
    If Max = 0 then
      Max := MaxWord;
    If Min >= Max then
       Min := MinWord;
    If (W < Min) or (W > Max) then
        W := Min;
    If ((W = 0) and RTTT.SuppressZero) then
       Txt := ''
    else
       Txt := Int_To_Str(W);
    Temp := W;
    Valid := false;
    Display_Box_and_Prompt(X,Y,Boxtype,Prompt,L);
    YT := MessageLine(Y);
    Repeat
         Read_Line(X,Y,L,RTTT.FCol,RTTT.BCol,8,Txt);
         If ((R_Char = #027) and RTTT.AllowEsc)
         or ((Txt = '') and (RTTT.AllowNull)) then
         begin
             If Txt = '' then R_Null := true;
             exit;
         end
         else
         begin
             val(Txt,Temp,code);
             If code <> 0 then
             begin
                Invalid_Message(YT,ChW);
                If ChW = #027 then
                        Txt := Int_To_Str(W);
             end
             else
             begin
                 If (Temp < Min)
                 or (Temp > Max)
                 or ((length(Txt) > 4) and (Txt > Int_To_Str(MaxWord))) then
                 begin
                    OutOfRange_Message(Yt,Int_To_Str(Min),Int_To_Str(Max),ChW);
                    If ChW = #027 then
                        Txt := Int_To_Str(W);
                 end
                 else
                 begin
                     W := temp;
                     Valid := true;
                 end;
             end;
         end;
    Until Valid  or ((R_Char = #027) and RTTT.AllowEsc);
end;


Procedure Read_Int(X,Y,L:byte;
                   Prompt:StrScreen;
                   BoxType: byte;
                   Var W : integer;
                   Min, Max : integer);
var
   Temp : integer;
   Txt : StrScreen;
   Valid : boolean;
   Code : integer;
   YT : byte;
   ChI : char;
begin
    If Max = 0 then
      Max := MaxInt;
    If Min >= Max then
       Min := MinInt;
    If (W < Min) or (W > Max) then
        W := Min;
    If ((W = 0) and RTTT.SuppressZero) then
       Txt := ''
    else
       Txt := Int_To_Str(W);
    Temp := W;
    Valid := false;
    Display_Box_and_Prompt(X,Y,Boxtype,Prompt,L);
    YT := MessageLine(Y);
    Repeat
         Read_Line(X,Y,L,RTTT.FCol,RTTT.BCol,5,Txt);
         If ((R_Char = #027) and RTTT.AllowEsc)
         or ((Txt = '') and (RTTT.AllowNull)) then
         begin
             If Txt = '' then R_Null := true;
             exit;
         end
         else
         begin
             val(Txt,Temp,code);
             If code <> 0 then
             begin
                Invalid_Message(YT,ChI);
                If ChI = #027 then
                   Txt := Int_to_Str(W);

             end
             else
             begin
                 If (Temp < Min) or (Temp > Max) then
                 begin
                    OutOfRange_Message(Yt,Int_To_Str(Min),Int_To_Str(Max),ChI);
                    If ChI = #027 then
                       Txt := Int_to_Str(W);
                 end
                 else
                 begin
                     W := temp;
                     Valid := true;
                 end;
            end;
        end;
    Until Valid  or ((R_Char = #027) and RTTT.AllowEsc);
end;

Procedure Read_LongInt(X,Y,L:byte;
                   Prompt:StrScreen;
                   BoxType: byte;
                   Var W : longint;
                   Min, Max : longint);
var
   Temp : longint;
   Txt : StrScreen;
   Valid : boolean;
   Code : integer;
   YT : byte;
   ChI : char;
begin
    If Max = 0 then
      Max := MaxLongInt;
    If Min >= Max then
       Min := MinLongInt;
    If (W < Min) or (W > Max) then
        W := Min;
    If ((W = 0) and RTTT.SuppressZero) then
       Txt := ''
    else
       Txt := Int_To_Str(W);
    Temp := W;
    Valid := false;
    Display_Box_and_Prompt(X,Y,Boxtype,Prompt,L);
    YT := MessageLine(Y);
    Repeat
         Read_Line(X,Y,L,RTTT.FCol,RTTT.BCol,5,Txt);
         If ((R_Char = #027) and RTTT.AllowEsc)
         or ((Txt = '') and (RTTT.AllowNull)) then
         begin
             If Txt = '' then R_Null := true;
             exit;
         end
         else
         begin
             val(Txt,Temp,code);
             If code <> 0 then
             begin
                Invalid_Message(YT,ChI);
                If ChI = #027 then
                   Txt := Int_to_Str(W);
             end
             else
             begin
                 If (Temp < Min) or (Temp > Max) then
                 begin
                    OutOfRange_Message(Yt,Int_To_Str(Min),Int_To_Str(Max),ChI);
                    If ChI = #027 then
                       Txt := Int_to_Str(W);
                 end
                 else
                 begin
                     W := temp;
                     Valid := true;
                 end;
            end;
        end;
    Until Valid  or ((R_Char = #027) and RTTT.AllowEsc);
end;

Procedure Read_Real(X,Y,L:byte;
                    Prompt:StrScreen;
                    BoxType: byte;
                    Var W : real;
                    Min, Max : real);
var
   Temp : Real;
   Txt : StrScreen;
   Valid : boolean;
   Code : integer;
   YT : byte;
   ChR : char;
begin
    If Max = 0 then
      Max := 99999999;
    If Min >= Max then
       Min := -99999999;
    If (W < Min) or (W > Max) then
        W := Min;
    If Min < 0 then    {add room for - sign}
       Inc(L);
    If ((W = 0.0) and RTTT.SuppressZero) then
       Txt := ''
    else
       Txt := Real_To_Str(W,RTTT.RealDP);
    Temp := W;
    Valid := false;
    Display_Box_and_Prompt(X,Y,Boxtype,Prompt,L);      {5.00b}
    YT := MessageLine(Y);
    Repeat
         Read_Line(X,Y,L,RTTT.FCol,RTTT.BCol,7,Txt);
         If ((R_Char = #027) and RTTT.AllowEsc)
         or ((Txt = '') and (RTTT.AllowNull)) then
         begin
             If Txt = '' then R_Null := true;
             exit;
         end
         else
         begin
             val(Txt,Temp,code);
             If code <> 0 then
             begin
                Invalid_Message(YT,ChR);
                If ChR = #027 then
                   Txt := Real_to_Str(W,RTTT.RealDP);
             end
             else
             begin
                 If (Temp < Min) or (Temp > Max) then
                 begin
                    OutOfRange_Message(Yt,Real_To_Str(Min,RTTT.RealDP),Real_To_Str(Max,RTTT.RealDP),ChR);
                    If ChR = #027 then
                       Txt := Real_to_Str(W,RTTT.RealDP);
                 end
                 else
                 begin
                     W := temp;
                     Valid := true;
                 end;
            end;
        end;
    Until Valid  or ((R_Char = #027) and RTTT.AllowEsc);
end;

Procedure Read_Select(X,Y:byte;Pmt,Txt:StrScreen;var Choice:byte);
Const
     UpChar:string[1] = '^';
     JoinChar:string[1] = '_';
var
  W : byte;
  I : integer;
  Horiz : boolean;
     Function Replace_JoinChar(Str:string): string;
     {}
     var I : integer;
     begin
         For I := 1 to length(Str) do
             If Str[I] = JoinChar then
                Str[I] := ' ';
         Replace_JoinChar := Str;
     end; {of func Replace_JoinChar}

     Procedure HiLightWord(W:byte;Hi:boolean);
     var Col : byte;
     begin
         If Hi then
            Col := attr(RTTT.HiFCol,RTTT.HiBcol)
         else
            Col := attr(RTTT.LoFcol,RTTT.LoBcol);
         If Horiz then
             Fastwrite(pred(X)+PosWord(W,Txt),Y,Col,Replace_JoinChar(ExtractWords(W,1,Txt)))
         else
             Fastwrite(X,pred(Y)+W,Col,Replace_JoinChar(ExtractWords(W,1,Txt)));
         If Hi then
         begin
            If Horiz then
               GotoXY(pred(X)+PosWord(W,Txt),Y)
            else
               GotoXY(X,Pred(Y)+W);
         end;
     end;

     Procedure Process_Keys;
     var
       ChP : char;
       Finished : boolean;
     begin
         Finished := false;
         Repeat
              ChP := getKey;
              If ChP in RTTT.End_Chars then
                  Finished := True
              else
              Case upcase(ChP) of
              #132,
              EscKey      : If RTTT.AllowEsc then
                                Finished := true;
              ' ',#9,                                 {tab}
              CursorDown,
              CursorRight : begin
                                HiLightWord(Choice,false);
                                If Choice < W then
                                   Inc(Choice)
                                else
                                   Choice := 1;
                                HiLightWord(Choice,true);
                            end;
              #143,                     {Shift tab}
              CursorUp,
              CursorLeft  : begin
                                HiLightWord(Choice,false);
                                If Choice > 1 then
                                   Dec(Choice)
                                else
                                   Choice := W;
                                HiLightWord(Choice,true);
                            end;
              #131        : If (Choice < W) and Horiz then    {mouse right}
                            begin
                                HiLightWord(Choice,false);
                                Inc(Choice);
                                HiLightWord(Choice,true);
                            end;
              #130        : If (Choice > 1) and Horiz then    {mouse left}
                            begin
                                HiLightWord(Choice,false);
                                Dec(Choice);
                                HiLightWord(Choice,true);
                            end;
              #129        : If (Choice < W) and (Horiz = false) then    {mouse down}
                            begin
                                HiLightWord(Choice,false);
                                Inc(Choice);
                                HiLightWord(Choice,true);
                            end;
              #128        : If (Choice > 1) and (Horiz = false) then    {mouse up}
                            begin
                                HiLightWord(Choice,false);
                                Dec(Choice);
                                HiLightWord(Choice,true);
                            end;

              end; {case}
         until Finished;
         R_Char := ChP;
     end;

begin
    If Txt[1] = UpChar then
    begin
        Horiz := False;
        Delete(Txt,1,1);
    end
    else
       Horiz := true;
    W := Wordcnt(Txt);
    If W < 2 then exit;              {only show choices if there are two or more}
    FindCursor(Cursor_X,Cursor_Y,ScanTop,ScanBot);   {record cursor settings}
    If (Choice > W) or (Choice < 1) then               {check that W is sensible}
       Choice := 1;
    If Pmt <> '' then
    begin
        Fastwrite(X,Y,attr(RTTT.PFcol,RTTT.PBCol),Pmt);
        X := X+length(Pmt);
    end;
    For I := 1 to W do
        HiLightWord(I,False);
    OnCursor;
    HiLightWord(Choice,True);
    Process_keys;
    GotoXY(Cursor_X,Cursor_Y);           {reset cursor}
    SizeCursor(ScanTop,ScanBot);
end;  {proc Read_Select}

begin
   Default_Settings;
end.
