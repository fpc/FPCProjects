{--------------------------------------------------------------------------}
{                         TechnoJock's Turbo Toolkit                       }
{                                                                          }
{                              Version   5.10                              }
{                                 (Europe)                                 }
{                                                                          }
{               Copyright 1986-1993 TechnoJock Software, Inc.              }
{                           All Rights Reserved                            }
{                          Restricted by License                           }
{--------------------------------------------------------------------------}
                     {--------------------------------}
                     {       Unit:    IOTTT5          }
                     {--------------------------------}
{Change history:  2/24/89 5.00a    Added default Jump_Full setting line 900
                  2/26/89 5.00b    Added exit statement line 1339
                  2/28/89 5.00c    Modified insert proc line 1497
                          5.00d    Expanded Display_All_Fields line 1188
                  3/05/89 5.00e    Changed default Allow_Esc to true
                          5.00f    Reduced size of Table Settings structure
                  3/12/89 5.00g    Added cursor keys etc. to Allow_Char logic
                                   lines 226 & 1568
                          5.00h    Modified field rules logic to permit
                                   Field_Rules to be called before XXX_Field
                                   e.g. Real_Field
                          5.00i    Changed Cursor positioning logic for
                                   fields  line 593, 1315, 1331
                          5.00j    Improved insert procedure and added proc
                                   Init_Insert_Mode;
                          5.00k    Corrected Refresh_Fields bug in non IOFULL
                                   state.
                          5.00l    Changed Erase_Default logic to work when
                                   jumping
                          5.00m    Added Enter Field Hook for first field
            April 1, 89   5.01     Added error checking for TableSet,
                                   and changed error level on fatal
                          5.01a    Added debug compiler directive, fixed
                                   global erase, remove references to VER50
                          5.01b    Fixed AllowNull for string fields
                          5.01c    Added a Update_Variables proc for hooks
                          5.01d    Allowed Leave_Field_Hook to force back
                                   to current field.
             Sep 30, 89   5.02     Changed minimum logic in real/int/longint
                                   fields.
             Oct 9, 89    5.02a    modified dis_allow Char in
                                   Field Rules.
             Jan 24, 90   5.02b    changed cursor movement logic for fields
                                   ending in a mask character.
             Feb 22, 90   5.02c    Changed field validation for null fields
             Jul 31, 90   5.02d    Remove last message when finished
             01/04/93     5.10     DPMI compatible version
}
{$ifdef fpc}
{$mode fpc}
{$endif fpc}

{$S-,R-,V-}

{$IFNDEF DEBUG}
{$D-}
{$ENDIF}

Unit IOTTT5;
(*
{$DEFINE IOFULL}
*)
INTERFACE

uses CRT, FastTTT5, DOS, WinTTT5, KeyTTT5, StrnTTT5, MiscTTT5;

CONST
MaxTables      = 10;       {alter as necessary}
MaxInputFields = 40;       {alter as necessary}
IntCharacters: set of char = [#129, #132,#142,#148,#153,#154,#225]; {international users modify for your country}
IOUndefined = 0;
{$IFDEF IOFULL}
IOString   = 1;
IOByte     = 2;
IOWord     = 3;
IOInteger  = 4;
IOLongInt  = 5;
IOReal     = 6;
IOPassword = 7;
IOSelect   = 8;
IODate     = 9;

AllowNull    = $01;
SuppressZero = $02;
RightJustify = $04;
EraseDefault = $08;
JumpIfFull   = $10;

Default_Allow_Null    :boolean = true;
Default_Suppress_Zero :boolean = true;
Default_Right_Justify :boolean = false;
Default_Erase_Default :boolean = false;
Default_Jump_Full     :boolean = false;
Default_Allow_Char    :set of char = [#0];
Default_DisAllow_Char :set of char = [#0];
{$ENDIF}
Refresh_None    = 0;
Refresh_Current = 1;
Refresh_All     = 2;
End_Input       = 99;
No_Char         = #0;

TYPE
{$IFNDEF VER40}
Move_Field_Proc = procedure(var CurrentField:byte;var Refresh:byte);
Char_Hook_Proc   = procedure(var Ch : char; var CurrentField:byte;var Refresh:byte);
Insert_Proc      = procedure(Insert:boolean);
{$ENDIF}

IOCharSet = Set of char;
Str_Field_Defn = record
      Upfield   : byte;
      Downfield : byte;
      Leftfield : byte;
      Rightfield: byte;
      X         : byte;
      Y         : byte;
      Message   : strscreen;        {5.00f}
      MsgX      : byte;
      MsgY      : byte;
      CursorX   : byte;
      StrLocX   : byte;
      FieldLen  : byte;
      FieldStr  : strscreen;
      FieldFmt    : strscreen;       {5.00f}
      Right_Justify : boolean;
      {$IFDEF IOFULL}
      RealDP        : byte;
      Allow_Null    : boolean;
      Suppress_Zero : Boolean;
      Erase_Default : boolean;
      Jump_Full     : boolean;
      Allow_Char    : set of char;
      DisAllow_Char : set of char;
      Rules_Set     : Boolean;    {5.00h}
      case FieldType:byte of
           IOString   : (SPtr: ^string);
           IOByte     : (BPtr: ^Byte;BMax:byte;BMin:byte);
           IOWord     : (WPtr: ^Word;WMax:word;WMin:word);
           IOInteger  : (IPtr: ^Integer;IMax:integer;IMin:Integer);
           IOLongInt  : (LPtr: ^LongInt;LMax:longint;LMin:longInt);
           IOReal     : (RPtr: ^Real;RMax:real;RMin:Real);
           IODate     : (DPtr: ^Dates;DFormat:byte;DMax:Dates;DMin:Dates);
      {$ELSE}
      FieldType : byte;
      SPtr : ^string;
      {$ENDIF}
end;

Str_Field_Ptr = ^Str_Field_Defn;

TableSettings = record
     HiFCol  : byte;
     HiBCol  : byte;
     LoFCol  : byte;
     LoBCol  : byte;
     MsgFCol : byte;
     MsgBCol : byte;
     TotalFields: byte;
     CurrentField : byte;
     AllowEsc : boolean;
     IO_FieldsSet : boolean;
     Displayed   : boolean;
     Beep : boolean;
     WhiteSpace : char;
     ErrorLine : byte;
     Insert : boolean;
     {$IFNDEF VER40}
     LeaveFieldHook : Move_Field_Proc;
     EnterFieldHook : Move_Field_Proc;
     CharHook   : Char_Hook_Proc;
     InsertProc : Insert_Proc;
     {$ENDIF}
     FinishChar : char;
end;

TableRec = record
     FieldDefn: array[0..MaxInputFields] of Str_Field_Ptr;
     ITTT: TableSettings;
end;

TablePtr = ^TableRec;

VAR
  CurrentTable : byte;
  TableSet: boolean;
  TotalTables : byte;
  Table : array[1..MaxTables] of TablePtr;
  I_Char : char;
  {$IFDEF VER40}
  IO_LeaveHook,
  IO_EnterHook,
  IO_CharHook,
  IO_InsertHook : pointer;
  {$ENDIF}

Procedure Create_Tables(No_Of_Tables:byte);
Procedure Activate_Table(Table_no:byte);
{$IFNDEF VER40}
Procedure Assign_LeaveFieldHook(Proc:Move_Field_Proc);
Procedure Assign_EnterFieldHook(Proc:Move_Field_Proc);
Procedure Assign_CharHook(Proc:Char_Hook_Proc);
Procedure Assign_InsHook(Proc:Insert_Proc);
{$ENDIF}
Procedure Create_Fields(No_of_fields:byte);
Procedure Define_Colors(HiF,HiB,LoF,LoB,MsgF,MsgB:byte);
Procedure Add_Message(DefID,DefX,DefY : byte; DefString : string);
Procedure Add_Field(DefID,DefU,DefD,DefL,DefR,DefX,DefY:byte);
Procedure String_Field(DefID:byte;var Strvar:String;DefFormat:string);
{$IFDEF IOFULL}
Procedure Assign_Finish_Char(Ch : char);
Procedure Byte_Field(DefID:byte;var ByteVar:Byte;DefFormat:string;Min,Max:byte);
Procedure Word_Field(DefID:byte;var Wordvar:Word;DefFormat:string;Min,Max:word);
Procedure Integer_Field(DefID:byte;var Integervar:Integer;DefFormat:string;Min,Max:integer);
Procedure LongInt_Field(DefID:byte;var LongIntvar:LongInt;DefFormat:string;Min,Max:LongInt);
Procedure Date_Field(DefID:byte;var Datevar:Dates;DateFormat:byte;DefFormat:string;
                      Min,Max : Dates);
Procedure Real_Field(DefID:byte;var Realvar:Real;DefFormat:string;Min,Max:real);
Procedure Set_Default_Rules(Rules:word);
Procedure Field_Rules(DefID:byte;Rules:word;AChar:IOcharset;DChar:IOcharset);
{$ENDIF}
Procedure Update_Variables;   {fix 5.01 c}
Procedure Display_All_Fields;
Procedure Allow_Esc(OK:boolean);
Procedure Allow_Beep(OK:boolean);
Procedure Init_Insert_Mode(ON:boolean);         {5.00j}
Procedure Dispose_Fields;
Procedure Dispose_Tables;
Procedure Process_Input(StartField:byte);

implementation

Const
    Valid    = 0;
    NotValid = 1;
    EscValid = 2;

    FmtChars  : set of char = ['!','#','@','*'];
    IOUp       = #200;
    IODown     = #208;
    IORight    = #205;
    IOLeft     = #203;
    IODel      = #211;
    IOTotErase = #146;    {Alt-E}
    IOErase    = #160;    {Alt-D}
    IOFinish   = #196;    {F10}   {can be over ridden with ASSIGN_FINISH_CHAR}
    IOEsc      = #27;
    IOTab      = #9;
    IOShiftTab = #143;
    IOEnter    = #13;
    IOIns      = #210;
    IOBackSp   = #8;
    IORightFld = #244;
    IOLeftFld  = #243;
    Control_Char : set of char = [IOUp,IODown,IORight,IOLeft,IODel,    {5.00g}
                                  IOTotErase,IOErase, IOEsc,
                                  IOTab, IOShiftTab, IOEnter, IOIns,
                                  IOBackSp, IORightFld, IOLeftFld];
VAR
   FirstCharPress : boolean;

{$F+}
procedure NoFieldHook(var CurrentField:byte;var Refresh:byte);
begin
end;

procedure NoCharHook(var Ch : char; var CurrentField:byte;var Refresh:byte);
begin
end;

Procedure DefaultInsertHook(On:boolean);
begin
    If ON then
       OnCursor
    else
       FullCursor;
end;
{$F-}

{$IFDEF VER40}
Procedure CallEnterFieldHook(var CurrentField:byte;var Refresh:byte);
          Inline($FF/$1E/IO_EnterHook);

Procedure CallLeaveFieldHook(var CurrentField:byte;var Refresh:byte);
          Inline($FF/$1E/IO_LeaveHook);

Procedure CallCharHook(var Ch : char; var CurrentField:byte;var Refresh:byte);
          Inline($FF/$1E/IO_CharHook);

Procedure CallInsertHook(On:boolean);
          Inline($FF/$1E/IO_InsertHook);
{$ENDIF}

Procedure IOTTT_Error(Code:byte;value:real);    {fatal error -- msg and halt}
var Message:string;
begin
    Case Code of
    1 : Message := 'Error 1: Invalid value of '+Real_to_Str(value,0)
                   +' in Create_Fields with a MaxInputFields of '
                   +Real_to_Str(MaxInputFields,0);
    2 : Message := 'Error 2 : Insufficient Memory on Heap. Available '
                   +Real_to_Str(MaxAvail,0)+'. Required '
                   +Real_to_Str(value,0);
    3 : Message := 'Error 3 : Field operation not allowed before before Create_Fields';
    4 : Message := 'Error 4 : Field ID: '
                   +Real_to_Str(value,0)+' out of range';
    5 : Message := 'Error 5 : cannot change fields, invalid target field ID: '
                   +Real_to_Str(value,0);
    6 : message := 'Error 6 : Invalid X or Y value defined in Add_Field ID: '
                   +Real_to_Str(value,0);
    7 : Message := 'Error 7 : Cannot Add_message before calling Add_Field';
    8 : Message := 'Error 8 : Cannot Add_Message, invalid Field ID: '+Real_to_Str(value,0);
    9 : message := 'Error 9 : Invalid X or Y coordinate defined in Add_Message ID: '
                   +Real_to_Str(value,0);
    10 : Message := 'Error 10 : Cannot Dispose_fields, no fields exist';
    11 : Message := 'Error 11 : Cannot Create_Fields - fields already created,'
                    +' reset with Dispose_fields';
    12 : Message := 'Error 12 : Use Create_Tables before Activate_Table';
    13 : Message := 'Error 13 : Cannot Activate_Table - Table outside range';
    14 : Message := 'Error 14 : call Create_Tables or Create_Fields first';
    else Message := 'Aborting';
    end; {case}
    WriteAT(1,12,black,lightgray,Message);
    Repeat Until keypressed;
    Halt(10);     {IO fatal error returns an error level of 10}  {5.01}
end;    {proc IOTTT_Error}

Procedure Ding;
begin
    If Table[CurrentTable]^.ITTT.Beep then
    begin
       sound(750);delay(150);nosound;
    end;
end;    {proc Ding}

Procedure Reset_Table(var T: TableSettings);
begin
    with T do
    begin
        HiFCol := white;
        HiBCol := blue;
        LoFCol := blue;
        LoBCol := lightgray;
        MsgFCol:= yellow;
        MsgBCol:= red;
        TotalFields:=MaxInputFields;
        CurrentField := 1;
        AllowEsc := true;                  {5.00e}
        IO_FieldsSet := false;
        Displayed    := false;
        Beep    := true;
        WhiteSpace   := #250;
        ErrorLine := 24;
        Insert := true;
        {$IFNDEF VER40}
        LeaveFieldHook := {$ifdef fpc}@{$endif}NoFieldHook;
        EnterFieldHook := {$ifdef fpc}@{$endif}NoFieldHook;
        CharHook := {$ifdef fpc}@{$endif}NoCharHook;
        InsertProc := {$ifdef fpc}@{$endif}DefaultInsertHook;
        {$ELSE}
        IO_LeaveHook  := nil;
        IO_EnterHook  := nil;
        IO_CharHook   := nil;
        IO_InsertHook := @DefaultInsertHook;
        {$ENDIF}
        FinishChar := IOFinish;
    end;
end;

Procedure Create_Tables(No_Of_Tables:byte);
var
  I:integer;
  Room_needed : integer;
begin
    If No_of_Tables in [1..MaxTables] then
    begin
        Room_needed := sizeof(Table[1]^);
        For I := 1 to No_of_Tables do
        begin
{$ifndef fpc}
            If MaxAvail >= Room_needed then
{$endif not fpc}
            begin
                GetMem(Table[I],Room_Needed);
                Reset_Table(Table[I]^.ITTT)
            end
{$ifndef fpc}
            else  {not enough heap space}
                    IOTTT_Error(2,Room_needed); {end MemAvail If clause}
{$endif not fpc}
        end;
        TotalTables := No_Of_Tables;
    end;
    TableSet := true;
end;   {IO_SetTables}

 Procedure Activate_Table(Table_No:byte);
 {}
 begin
     If not TableSet then
        IOTTT_Error(12,0.0);
     If Table_No > TotalTables then
        IOTTT_Error(13,0.0);
     CurrentTable := Table_No
 end; {of proc Activate_Table}
{$IFNDEF VER40}

 Procedure Assign_LeaveFieldHook(Proc:Move_Field_Proc);
 {}
 begin
     If not TableSet then
        IOTTT_Error(14,0.0);
     Table[CurrentTable]^.ITTT.LeaveFieldHook := proc;
 end; {of proc Assign_Field_Proc}

 Procedure Assign_EnterFieldHook(Proc:Move_Field_Proc);
 {}
 begin
     Table[CurrentTable]^.ITTT.EnterFieldHook := proc;
 end; {of proc Assign_Field_Proc}

 Procedure Assign_CharHook(Proc:Char_Hook_Proc);
 {}
 begin
     If not TableSet then
        IOTTT_Error(14,0.0);
     Table[CurrentTable]^.ITTT.CharHook := proc;
 end; {of proc Assign_Char_Proc}

 Procedure Assign_InsHook(Proc:Insert_Proc);
 {}
 begin
     If not TableSet then
        IOTTT_Error(14,0.0);
     Table[CurrentTable]^.ITTT.InsertProc := proc;
 end; {of proc Assign_Char_Proc}
{$ENDIF}

 Procedure Assign_Finish_Char(Ch : char);
 {}
 begin
     If not TableSet then
        IOTTT_Error(14,0.0);
     Table[CurrentTable]^.ITTT.FinishChar := Ch;
 end; {of proc Assign_Finish_Char}

{$IFDEF IOFULL}
 Procedure Set_Default_Rules(Rules:word);
 {}
 begin
     If not TableSet then
        IOTTT_Error(14,0.0);
     Default_Allow_Null    := (Rules and AllowNull) = AllowNull;
     Default_Suppress_Zero := (Rules and SuppressZero) = SuppressZero;
     Default_Right_Justify := (Rules and RightJustify) = RightJustify;
     Default_Erase_Default := (Rules and EraseDefault) = EraseDefault;
     Default_Jump_Full     := (Rules and JumpIfFull) = JumpIfFull;
 end; {of proc Set_Default_Rules}
{$ENDIF}

Procedure Create_Fields(No_of_fields:byte);
var
  I:integer;
  Room_needed : integer;
begin
    If not TableSet then
       Create_Tables(1);
    with Table[CurrentTable]^ do
    begin
    (*
        If ITTT.IO_FieldsSet then IOTTT_Error(11,0);       {already set}
    *)
        If No_of_Fields in [1..MaxInputFields] then
        begin
            Room_needed := sizeof(FieldDefn[0]^);
            For I := 0 to No_of_fields do
            begin
{$ifndef fpc}
                If MaxAvail >= Room_needed then
{$endif not fpc}
                begin
                    GetMem(FieldDefn[I],Room_Needed);
                    with FieldDefn[I]^ do
                    begin
                        Message     := '';
                        MsgX        := 81;     {zero means auto-center}
                        MsgY        := 0;
                        FieldType   := IOUndefined;
                        SPtr        := nil;
                        FieldLen    := 0;
                        FieldStr    := '';
                        FieldFmt    := '';
                        Right_Justify := false;
                        {$IFDEF IOFULL}
                        Rules_Set := False;     {5.00h}
                        {$ENDIF}
                    end;   {With}
                end
{$ifndef fpc}
                else  {not enough heap space}
                    IOTTT_Error(2,Room_needed); {end MemAvail If clause}
{$endif not fpc}
            end;
            ITTT.TotalFields := No_of_Fields;
            ITTT.IO_FieldsSet := true;
        end
        else  {Invalid No_of_fields}
           IOTTT_Error(1,No_of_fields);
   end; {with table}
end;  {Proc Create_Fields}

 Procedure Define_Colors(HiF,HiB,LoF,LoB,MsgF,MsgB:byte);
 {}
 begin
     If not TableSet then
        IOTTT_Error(14,0.0);
     With Table[CurrentTable]^.ITTT do
     begin
         HiFCol := HiF;
         HiBCol := HiB;
         LoFCol := LoF;
         LoBCol := LoB;
         MsgFCol := MsgF;
         MsgBCol := MsgB;
     end;
 end;    {Proc Define_Colors}

 Procedure Check_Field_Number(DefId : byte);
 {internal}
 begin
     If not TableSet then
        IOTTT_Error(14,0.0);
     with Table[CurrentTable]^ do
     begin
         If not ITTT.IO_FieldsSet then IOTTT_Error(3,0);
         If (DefID < 1) or (DefID>ITTT.TotalFields) then
            IOTTT_Error(4,Defid);
     end;
 end; {of proc Check_Field_Number}

Procedure Add_Field(DefID,DefU,DefD,DefL,DefR,DefX,DefY:byte);
begin
    with Table[CurrentTable]^ do
    begin
        Check_Field_Number(DefID);
        If  (DefX < 1) or (DefX > 80)
        or  (DefY < 1) or (DefY > DisplayLines) then
           IOTTT_Error(6,Defid);
        With FieldDefn[DefID]^ do
        begin
            If DefU <= ITTT.TotalFields then
               Upfield    := DefU;
            If DefD <= ITTT.TotalFields then
               Downfield  := DefD;
            If DefL <= ITTT.TotalFields then
               Leftfield  := DefL;
            If DefR <= ITTT.TotalFields then
               Rightfield := DefR;
            X          := DefX;
            Y          := DefY;
        end;
   end; {with Table}
end; {proc ADD_Field}

Procedure Add_Message(DefID,DefX,DefY : byte; DefString : string);
begin
    Check_Field_Number(DefId);   {5.01}
    with Table[CurrentTable]^ do
    begin
        If not ITTT.IO_FieldsSet then IOTTT_Error(7,0);
        If (DefID < 1) or (DefID > ITTT.TotalFields) then IOTTT_Error(8,DefID);
        If (DefX < 0) or (DefX > 80) or (DefY < 1) or (DefY > 25) then IOTTT_Error(9,DefID);
        With FieldDefn[Defid]^ do
        begin
            MsgX := DefX;
            MsgY := DefY;
            Message := DefString;
        end;
    end; {with Table}
end;  {proc ADD_Message}

 Function Max_string_length(DefFormat:string) : byte;
 var I,Counter : byte;
 begin
     Counter := 0;
     For I := 1 to length(DefFormat) do
         if (DefFormat[I] in FmtChars) then
            Counter := succ(counter);
     Max_string_length := Counter;
 end;  {sub func Max_String_Length}

 Function  Last_Char_Left_Justified(Str,Fmt:string): byte;
 var
    LenS,LenF,S,
    Counter : byte;
 begin
     Counter := 0;
     S := 0;
     LenF := Length(Fmt);
     LenS := Length(Str);
     Repeat
          Inc(Counter);
          If Fmt[Counter] in FmtChars then
             Inc(S);
     Until (S > LenS) or (Counter > LenF);
     Last_Char_Left_Justified := counter;
 end;

 Function  Pos_of_Last_Input_Char(DefFormat:string): byte;
 var
    Counter : byte;
 begin
     Counter := Succ(Length(DefFormat));
     Repeat
          Dec(Counter);
     Until (DefFormat[Counter] in FmtChars) or (Counter = 0);
     Pos_of_Last_Input_Char := counter;
 end;

Procedure Set_Cursor(DefID:byte);
begin
    with Table[CurrentTable]^.FieldDefn[DefID]^ do
    begin
{$IFDEF IOFULL}
        If Right_Justify then
        begin
            CursorX := pred(X) + Pos_of_Last_Input_Char(FieldFmt);
            StrLocX := length(FieldStr);
        end
        else       {left Justified}
        begin
{$ENDIF}
           If FieldStr = '' then
              StrLocX := 1
           else
           begin
               StrLocX := succ(Length(FieldStr));
               If StrLocX > FieldLen then
                  StrLocX := FieldLen;
           end;
           CursorX := Last_Char_Left_Justified(FieldStr,FieldFmt);
           If CursorX > length(FieldFmt) then       {5.00 I}
              dec(CursorX);
           while ( (FieldFmt[CursorX] in FmtChars) = false)   {5.02b}
           and   (CursorX > 0) do
              dec(CursorX);
           CursorX := CursorX + pred(X);
{$IFDEF IOFULL}
        end;
{$ENDIF}
    end;
end;


Function Var_To_String(DefID : byte):String;
var Str : string;
begin
    with Table[CurrentTable]^.FieldDefn[DefID]^ do
    begin
{$IFDEF IOFULL}
        Case FieldType of
        IOString  : Str := SPtr^;
        IOByte    : If Suppress_Zero and (BPtr^ = 0) then
                       Str := ''
                    else
                       Str := Int_To_Str(BPtr^);
        IOWord    : If Suppress_Zero and (WPtr^ = 0) then
                       Str := ''
                    else
                       Str := Int_To_Str(WPtr^);
        IOInteger : If Suppress_Zero and (IPtr^ = 0) then
                       Str := ''
                    else
                       Str := Int_To_Str(IPtr^);
        IOLongInt : If Suppress_Zero and (LPtr^ = 0) then
                       Str := ''
                    else
                       Str := Int_To_Str(LPtr^);
        IODate    : If Suppress_Zero and (DPtr^ = 0) then
                       Str := ''
                    else
                       Str := Unformatted_date(Julian_to_date(WPtr^,DFormat));
        IOReal    : If Suppress_Zero and (RPtr^ = 0.0) then
                       Str := ''
                    else
                    begin
                        Str := Real_To_Str(RPtr^,RealDP);
                        If RealDP <> Floating then
                            Delete(Str,LastPos('.',Str),1);
                    end;
        end; {case}
{$ELSE}
      Str := SPtr^;
{$ENDIF}
    end;   {with}
    Var_To_String := Str;
    Set_Cursor(DefID);
 end; {func Var_To_String}

 Function Formatted_String(Str,Fmt:string;RJ:boolean):string;
 var
 TempStr : string;
 I,J : byte;
 K : integer;
 begin
{$IFDEF IOFULL}
     If RJ then
     begin
         J := succ(Length(Fmt));
         K := length(Str);
         For I := length(Fmt) downto 1 do
         begin
             If not (Fmt[I] in FmtChars) then
             begin
                 TempStr[I] := Fmt[I] ;  {force any none format charcters into string}
                 dec(J);
             end
             else    {format character}
             begin
                 If K > 0  then
                    TempStr[I] := Str[K]
                 else
                    TempStr[I] := Table[CurrentTable]^.ITTT.WhiteSpace;
                 Dec(K);
             end;
         end;
     end
     else   {left Justified}
     begin
{$ENDIF}
         J := 0;
         For I := 1 to length(Fmt) do
         begin
             If not (Fmt[I] in FmtChars) then
             begin
                 TempStr[I] := Fmt[I] ;  {force any none format charcters into string}
                 inc(J);
             end
             else    {format character}
             begin
                 If I - J <= length(Str) then
                    TempStr[I] := Str[I - J]
                 else
                    TempStr[I] := Table[CurrentTable]^.ITTT.WhiteSpace;
             end;
         end;
{$IFDEF IOFULL}
     end;
{$ENDIF}
     TempStr[0] := char(length(Fmt));  {set initial byte to string length}
     Formatted_String := Tempstr;
 end;  {Func Formatted_String}

{$IFDEF IOFULL}
 Procedure Invalid_Message(var CH : char);
 begin
   Ding;
   With Table[CurrentTable]^.ITTT do
   TempMessageCH(1,ErrorLine,MsgFCol,MsgBCol,
               PadCenter('Invalid number - press any key ... and make correction!',80,' '),CH);
 end;

 Procedure Invalid_Date_Message(var CH : char;Format:byte);
 var FmtStr : string;
 begin
   Ding;
   Case Format of
   MMDDYY   : FmtStr := 'MM/DD/YY';
   MMDDYYYY : FmtStr := 'MM/DD/YYYY';
   MMYY     : FmtStr := 'MM/YY';
   MMYYYY   : FmtStr := 'MM/YYYY';
   DDMMYY   : FmtStr := 'DD/MM/YY';
   DDMMYYYY : FmtStr := 'DD/MM/YYYY';
   end; {case}
   With Table[CurrentTable]^.ITTT do
   TempMessageCH(1,ErrorLine,MsgFCol,MsgBCol,
               PadCenter('Error format is '+FmtStr+'  - press any key ... and make correction!',80,' '),CH);
 end;

 Procedure OutOfRange_Message(MinS,MaxS : StrScreen;var CH:char);
 var
   S : StrScreen;
 begin
     Ding;
     S := 'Error value must be in the range '+MinS+' to '+MaxS+' - press any key & correct';
     With Table[CurrentTable]^.ITTT do
          TempMessageCh(1,ErrorLine,MsgFCol,MsgBCol,PadCenter(S,80,' '),CH);
 end;

 Procedure Validate_Field(DefID:byte; var result:byte);
 {}
 var
   VL : longint;
   VR : Real;
   ChV : char;
   RetCode : integer;

   Procedure Check_Number(Min,Max: longint;
                          Len : byte;
                          StrMax : string);
   {}
   begin
       with Table[CurrentTable]^.FieldDefn[DefID]^ do
       begin
           If (FieldStr = '') and Suppress_Zero then {5.02c}
           begin
               VL := 0;
               Retcode := 0;
           end
           else
              val(FieldStr,VL,Retcode);
           If Retcode <> 0 then
           begin
               Invalid_Message(ChV);
               If ChV = #027 then
               begin
                  Result := EscValid;
                  FieldStr := Var_To_String(DefID);
               end
               else
                  Result := NotValid;
           end
           else
           begin
               If (VL < Min)
               or (VL > Max)
               or ((length(FieldStr) > Len) and (FieldStr > StrMax)) then
               begin
                  OutOfRange_Message(Int_To_Str(Min),Int_To_Str(Max),ChV);
                  If ChV = #027 then
                  begin
                     FieldStr := Var_To_String(DefID);
                     Result := EscValid;
                  end
                  else
                     Result := NotValid;
               end
               else
               begin
                   Result := valid;
               end;
           end;
       end; {with}
   end; {of proc Check_Number}

   Procedure Check_date;
   {}
   begin
       with Table[CurrentTable]^.FieldDefn[DefID]^ do
       begin
           If not Valid_Date(FieldStr,DFormat) then
           begin
               Invalid_Date_Message(ChV,DFormat);
               If ChV = #027 then
               begin
                  Result := EscValid;
                  FieldStr := Var_To_String(DefID);
               end
               else
                  Result := NotValid;
           end
           else
           begin
               VL := Date_to_Julian(FieldStr,DFormat);
               If (VL < DMin)
               or (VL > DMax) then
               begin
                  OutOfRange_Message(Julian_to_date(DMin,DFormat),Julian_to_date(DMax,DFormat),ChV);
                  If ChV = #027 then
                  begin
                     FieldStr := Var_To_String(DefID);
                     Result := EscValid;
                  end
                  else
                     Result := NotValid;
               end
               else
               begin
                   Result := valid;
               end;
           end;
       end; {with}
   end; {of proc Check_date}

 begin
     Result := Valid; {assume alls well}
     with Table[CurrentTable]^ do
          with FieldDefn[DefID]^ do
     begin
         If (FieldStr = '') and Allow_Null then
            exit;
         Case FieldType of
         IOString  : If FieldStr = '' then
                     begin
                        Result := NotValid;
                        Ding;
                     end;
         IOByte    : Check_Number(BMin,BMax,2,'255');
         IOWord    : Check_Number(WMin,WMax,4,'65535');
         IOInteger : Check_Number(IMin,IMax,5,'32767');
         IOLongInt : Check_Number(LMin,LMax,11,'2147483647');
         IODate    : Check_Date;
         IOReal    : begin
                         val(  Strip('B',ITTT.WhiteSpace,
                                     Formatted_String(FieldStr,FieldFmt,Right_Justify)),
                               VR,
                               Retcode
                            );
                         If Retcode <> 0 then
                         begin
                             Invalid_Message(ChV);
                             If ChV = #027 then
                             begin
                                Result := EscValid;
                                FieldStr := Var_To_String(DefID);
                             end
                             else
                                Result := NotValid;
                         end
                         else
                         begin
                             If (VR < RMin)
                             or (VR > RMax) then
                             begin
                                OutOfRange_Message(Real_To_Str(RMin,RealDP),Real_To_Str(RMax,RealDP),ChV);
                                If ChV = #027 then
                                begin
                                   FieldStr := Var_To_String(DefID);
                                   Result := EscValid;
                                end
                                else
                                   Result := NotValid;
                             end
                             else
                             begin
                                 Result := valid;
                             end;
                         end;
                     end;
         end; {case}
     end;   {with}
 end; {of proc Validate_Field}
{$ENDIF}

 Procedure String_To_Var(DefID : byte);
 begin
    with Table[CurrentTable]^ do
         with FieldDefn[DefID]^ do
{$IFDEF IOFULL}
         begin
             Case FieldType of
             IOString  : SPtr^ := FieldStr;
             IOByte    : BPtr^ := Str_to_Int(FieldStr);
             IOWord    : WPtr^ := Str_to_Int(FieldStr);
             IOInteger : IPtr^ := Str_to_Int(FieldStr);
             IOLongInt : LPtr^ := Str_to_Long(FieldStr);
             IOReal    : RPtr^ := Str_to_Real(Strip('B',ITTT.WhiteSpace,
                                              Formatted_String(FieldStr,FieldFmt,Right_Justify)));
             IODate    : If FieldStr = '' then
                            DPtr^ := 0
                         else
                            DPtr^ := Date_to_Julian(FieldStr,Dformat);
             end; {case}
        end;   {with}
{$ELSE}
       SPTR^ := FieldStr;
{$ENDIF}
 end; {proc String_to_var}


 Procedure Update_Variables;   {fix 5.01 c}
 {}
 var I : integer;
 begin
     with Table[CurrentTable]^ do
          For I :=  1 to ITTT.TotalFields do
              String_to_var(I);
 end;

{$IFDEF IOFULL}
 Procedure Set_Misc_Field_Defaults(DefID:byte);
 {}
 begin
     Check_Field_Number(DefId);   {5.01}
     with Table[CurrentTable]^.FieldDefn[DefID]^ do
     begin
         Allow_Null    := Default_Allow_Null;
         Suppress_Zero := Default_Suppress_Zero;
         Right_Justify := Default_Right_Justify;
         Erase_Default := Default_Erase_Default;
         Allow_Char    := Default_Allow_Char;
         DisAllow_Char := Default_DisAllow_Char;
         Jump_Full     := Default_Jump_Full;    {fix 5.00a}
         Set_Cursor(DefID);
         Rules_Set := true;   {5.00h}
     end;  {with}
 end; {of proc Set_Misc_Field_Defaults}

 Procedure Field_Rules(DefID:byte;
                       Rules:word;
                       AChar: IOCharSet;
                       DChar: IOCharSet);
 {}
 begin
     Check_Field_Number(DefId);   {5.01}
     with Table[CurrentTable]^.FieldDefn[DefID]^ do
     begin
         Allow_Null     := (Rules and AllowNull) = AllowNull;
         Suppress_Zero  := (Rules and SuppressZero) = SuppressZero;
         If (FieldType = IOReal)
         and (RealDP > 0)
         and (RealDp <> Floating) then
             Right_Justify := true       {force Right_Justify}
         else
             Right_Justify := (Rules and RightJustify) = RightJustify;
         Erase_Default := (Rules and EraseDefault) = EraseDefault;
         Jump_Full := (Rules and JumpIfFull) = JumpIfFull;
         Allow_Char    := Achar;
         If (RealDP <> Floating) and (DChar = [#0])  and (FieldType = IOReal) then
            DisAllow_Char := ['.']
         else
            DisAllow_Char := Dchar;
         FieldStr      := Var_To_String(DefID);
         Rules_Set := true;   {5.00h}
     end;  {with}
 end; {of proc Field_Rules}
{$ENDIF}

 Procedure String_Field(DefID:byte;
                        var Strvar:String;
                        DefFormat:string);
 {}
 begin
     with Table[CurrentTable]^.FieldDefn[DefID]^ do
     begin
         Check_Field_Number(DefID);
{$IFDEF IOFULL}
         FieldType     := IOString;
{$ENDIF}
         SPtr          := @StrVar;
         FieldStr      := Sptr^;
         FieldFmt      := DefFormat;
         FieldLen      := Max_String_Length(FieldFmt);
{$IFDEF IOFULL}
         If Rules_Set then                 {5.00h}
            Set_Cursor(DefID)
         else
            Set_Misc_Field_Defaults(DefID);
{$ELSE}
         Set_Cursor(DefID);
{$ENDIF}
     end;
 end; {of proc String_Field}

{$IFDEF IOFULL}
 Procedure Byte_Field(DefID:byte;
                      var Bytevar:Byte;
                      DefFormat:string;
                      Min,Max : byte);
 {}
 begin
     with Table[CurrentTable]^.FieldDefn[DefID]^ do
     begin
         Check_Field_Number(DefID);
         FieldType     := IOByte;
         If Rules_Set then                 {5.00h}
            Set_Cursor(DefID)
         else
            Set_Misc_Field_Defaults(DefID);
         SPtr          := @Bytevar;
         FieldStr := Var_To_String(DefID);
         If DefFormat = '' then
            FieldFmt := '###'
         else
            FieldFmt := DefFormat;
         If (Max = 0) or (Max < Min) then
            BMax := 255
         else
            BMax := Max;
         If Min > BMax then
            BMin := 0
         else
            BMin := Min;
         FieldLen      := Max_String_Length(FieldFmt);
         Set_Cursor(DefID);             {5.00h}
     end;
 end; {of proc Byte_Field}

 Procedure Word_Field(DefID:byte;
                      var Wordvar:Word;
                      DefFormat:string;
                      Min,Max : word);
 {}
 begin
     with Table[CurrentTable]^.FieldDefn[DefID]^ do
     begin
         Check_Field_Number(DefID);
         FieldType     := IOWord;
         If Rules_Set then                 {5.00h}
            Set_Cursor(DefID)
         else
            Set_Misc_Field_Defaults(DefID);
         SPtr          := @WordVar;
         FieldStr      := Var_to_String(DefID);
         If DefFormat = '' then
            FieldFmt := '#####'
         else
            FieldFmt := DefFormat;
         If (Max = 0) or (Max < Min) then
             WMax := 65535
         else
            WMax := Max;
         If Min > WMax then
            WMin := 0
         else
            WMin := MIn;
         FieldLen      := Max_String_Length(FieldFmt);
         Set_Cursor(DefID);          {5.00h}
     end;
 end; {of proc Word_Field}

 Procedure Integer_Field(DefID:byte;
                      var Integervar:Integer;
                      DefFormat:string;
                      Min,Max:Integer);
 {}
 begin
     with Table[CurrentTable]^.FieldDefn[DefID]^ do
     begin
         Check_Field_Number(DefID);
         FieldType     := IOInteger;
         If Rules_Set then                 {5.00h}
            Set_Cursor(DefID)
         else
            Set_Misc_Field_Defaults(DefID);
         Set_Misc_Field_Defaults(DefID);
         SPtr          := @IntegerVar;
         FieldStr      := Var_to_String(DefID);
         If DefFormat = '' then
            FieldFmt := '######'
         else
            FieldFmt := DefFormat;
         If (Max = 0) or (Max < Min) then
            IMax := 32767
         else
            IMax := Max;
         If ((Min = 0) and (Max = 0)) or (Min > WMax) then      {5.02}
            IMin := -32768
         else
            IMin := Min;
         FieldLen      := Max_String_Length(FieldFmt);
         Set_Cursor(DefID);   {5.00h}
     end;
 end; {of proc Integer_Field}

 Procedure LongInt_Field(DefID:byte;
                      var LongIntvar:LongInt;
                      DefFormat:string;
                      Min,Max : LongInt);
 {}
 begin
     with Table[CurrentTable]^.FieldDefn[DefID]^ do
     begin
         Check_Field_Number(DefID);
         FieldType     := IOLongInt;
         If Rules_Set then                 {5.00h}
            Set_Cursor(DefID)
         else
            Set_Misc_Field_Defaults(DefID);
         SPtr          := @LongIntVar;
         FieldStr      := Var_to_String(DefID);
         If DefFormat = '' then
            FieldFmt := '###########'
         else
            FieldFmt := DefFormat;
         If (max = 0) or (Max < Min) then
            LMax := 2147483647
         else
            LMax := Max;
         If ((Min = 0) and (Max = 0)) or (Min > LMax) then   {5.02}
            LMin := -2147483647
         else
            LMin := Min;
         FieldLen      := Max_String_Length(FieldFmt);
         Set_Cursor(DefID);           {5.00h}
     end;
 end; {of proc LongInt_Field}

 Procedure Date_Field(DefID:byte;
                      var Datevar:Dates;
                      DateFormat:byte;
                      DefFormat:string;
                      Min,Max : Dates);
 {}
 begin
     with Table[CurrentTable]^.FieldDefn[DefID]^ do
     begin
         Check_Field_Number(DefID);
         FieldType     := IODate;
         If Rules_Set then                 {5.00h}
            Set_Cursor(DefID)
         else
            Set_Misc_Field_Defaults(DefID);
         SPtr          := @DateVar;
         If DateVar = 0 then
            FieldStr := ''
         else
            FieldStr      := Unformatted_date(Julian_to_Date(DateVar,DateFormat));
         If DefFormat = '' then
         begin
             Case DateFormat of
             DDMMYY,MMDDYY :       FieldFmt := '##/##/##';
             MMYY          :       FIeldFmt := '##/##';
             MMYYYY        :       FieldFmt := '##/####';
             DDMMYYYY,
             MMDDYYYY      :       FieldFmt := '##/##/####';
             end; {Case}
         end
         else
            FieldFmt := DefFormat;
         If (Max = 0) or (Max < Min) then
             DMax := 65535
         else
            DMax := Max;
         If Min > WMax then
            DMin := 0
         else
            DMin := MIn;
         DFormat := DateFormat;
         FieldLen      := Max_String_Length(FieldFmt);
         Set_Cursor(DefID);   {5.00h}
     end;
 end; {of proc Date_Field}

 Procedure Real_Field(DefID:byte;
                      var Realvar:Real;
                      DefFormat:string;
                      Min,Max : real);
 {}
 var p : byte;
 begin
     with Table[CurrentTable]^.FieldDefn[DefID]^ do
     begin
         Check_Field_Number(DefID);
         FieldType     := IOReal;
         If Rules_Set then                 {5.00h}
            Set_Cursor(DefID)
         else
            Set_Misc_Field_Defaults(DefID);
         SPtr          := @RealVar;
         If DefFormat = '' then
            FieldFmt := '############'
         else
            FieldFmt := DefFormat;
         P := LastPos('.',FieldFmt);
         If P = 0 then
            RealDP  := Floating
         else
            RealDP := Length(FieldFmt) - P;
         If RealDP = 0 then
            Delete(FieldFmt,P,1);            {remove the end decimal place}
         If (Max = 0.0) or (Max < Min) then
            RMax := 1.7E+37                  {for compatibiltity with Turbo4}
         else
            RMax := Max;
         If ((Min = 0.0) and (Max = 0.0)) or (Min > RMax) then  {5.02}
            RMin := -1.7E+37                 {for compatibiltity with Turbo4}
         else
            RMin := Min;
         If (RealDP <> 0) and (RealDP <> Floating) then
            Right_Justify := true;
         If RealDP <> Floating then
            DisAllow_Char := ['.'];
         FieldStr      := Var_to_String(DefID);
         FieldLen      := Max_String_Length(FieldFmt);
         Set_Cursor(DefID);   {5.00h}
     end;
 end; {of proc Real_Field}
{$ENDIF}

Procedure Hilight(ID:byte);      {display cell in bright colors}
begin
    with Table[CurrentTable]^ do
         with FieldDefn[ID]^ do
              WriteAT(X,Y,ITTT.HiFCol,ITTT.HiBCol,
                      Formatted_String(FieldStr,FieldFmt,Right_Justify));
end;

Procedure LoLight(ID:byte);      {display cell in dim colors}
begin
    with Table[CurrentTable]^ do
         with FieldDefn[ID]^ do
             WriteAT(X,Y,ITTT.LoFCol,ITTT.LoBCol,
                      Formatted_String(FieldStr,FieldFmt,Right_Justify));
end;

Procedure Display_All_Fields;
var I : integer;
begin
    If not TableSet then
        IOTTT_Error(14,0.0);  {5.01}
    with Table[CurrentTable]^ do
    begin
        For I :=  1 to ITTT.TotalFields do
        begin
            FieldDefn[I]^.FieldStr := Var_To_String(I);    {fix 5.00 d}
            Set_Cursor(I);
            LoLight(I);
        end;
        ITTT.Displayed  := true;
    end; {with Table}
end;

Procedure Allow_Esc(OK:boolean);
begin
    If not TableSet then
        IOTTT_Error(14,0.0);  {5.01}
    Table[CurrentTable]^.ITTT.AllowEsc := OK;
end;    {proc Allow_Esc}

Procedure Allow_Beep(OK:boolean);
begin
    Table[CurrentTable]^.ITTT.Beep := OK;
end;    {proc Allow_Beep}

Procedure Init_Insert_Mode(ON:boolean);
begin
    Table[CurrentTable]^.ITTT.Insert := ON;
end;    {proc Init_Insert_Mode}

Procedure Dispose_Fields;
var I : integer;
begin
    If not TableSet then
        IOTTT_Error(14,0.0);  {5.01}
    with Table[CurrentTable]^ do
    begin
        If not ITTT.IO_FieldsSet then IOTTT_Error(10,0);
        For I := 0 to ITTT.TotalFields do
            FreeMem(FieldDefn[I],sizeof(FieldDefn[I]^));
        Reset_Table(ITTT);
    end; {with Table}
end; { proc Dispose_Fields}

Procedure Dispose_Tables;
var I : integer;
begin
    If not TableSet then
        IOTTT_Error(14,0.0);  {5.01}
    For I := 1 to TotalTables do
        FreeMem(Table[I],sizeOf(Table[I]^));
    TotalTables := 0;
end;

{
****************************
*      Main Procedure      *
****************************
}

Procedure Process_Input(StartField:byte);
var
    OldLine : array[1..160] of byte;
    Finished : boolean;
    SRefresh,SField : Byte;

    Procedure DisplayMessage(ID:byte);
    begin
        With Table[CurrentTable]^ do
             with FieldDefn[ID]^ do
             begin
                If MsgX = 0 then   {Center the message}
                   MsgX := (80 - length(Message)) div 2;
                PartSave(MsgX,MsgY,MsgX+length(Message),MsgY,OldLine);
                WriteAT(MsgX,MsgY,ITTT.MsgFCol,ITTT.MsgBCol,Message);
             end;
    end;

    Procedure RemoveMessage(ID:byte);
    var I,LocC : integer;
    begin
        With Table[CurrentTable]^.FieldDefn[ID]^ do
             PartRestore(MsgX,MsgY,MsgX+length(Message),MsgY,OldLine);
    end; {sub sub proc RemoveMessage}

    Procedure Check_Refresh_State(Refresh:byte);
    {}
    var I : integer;
    begin
        with Table[CurrentTable]^ do
        Case Refresh of
{$IFDEF IOFULL}
        Refresh_None :; {do nothing}
        Refresh_Current: begin
                             FieldDefn[ITTT.CurrentField]^.FieldStr := Var_to_String(ITTT.CurrentField);
                             Set_Cursor(ITTT.CurrentField);  {5.00i}
                             LoLight(ITTT.CurrentField);
                         end;
        Refresh_All: begin
                         Display_All_Fields;
                     end;
        End_Input : begin
                        Display_All_Fields;
                        Finished := true;
                    end;
{$ELSE}
        Refresh_None   :; {do nothing}
        Refresh_Current: begin
{
                             FieldDefn[I]^.FieldStr := Var_To_String(I);}{5.00k}
                             { changed by (JM) }
                             FieldDefn[ITTT.CurrentField]^.FieldStr :=
                               Var_To_String(ITTT.CurrentField);
                             Set_Cursor(ITTT.CurrentField);   {5.00i}
                             LoLight(ITTT.CurrentField);
                         end;
        Refresh_All    : Display_All_Fields;
        End_Input      : begin
                             Display_All_Fields;
                             Finished := true;
                         end;
{$ENDIF}
        end; {Case}
    end; {of proc Check_refresh_State}

  Procedure Change_Fields(ID:byte);
  var
    ValidInput:byte;
    CField : byte;
    Refresh : byte;
  begin
      with Table[CurrentTable]^ do
      begin
{$IFDEF IOFULL}
          Validate_Field(ITTT.CurrentField,ValidInput);
          If ValidInput <> Valid then
             exit;
{$ENDIF}
          String_to_Var(ITTT.CurrentField);
          LoLight(ITTT.CurrentField);
          If FieldDefn[ITTT.CurrentField]^.MsgX <= 80 then
             RemoveMessage(ITTT.CurrentField);
          {Now call the "leave field" hook}
          CField := ITTT.CurrentField;
          Refresh := Refresh_None;
          {$IFNDEF VER40}
          ITTT.LeaveFieldHook(CField,Refresh);
          {$ELSE}
          If IO_LeaveHook <> Nil then
             CallLeaveFieldHook(CField,Refresh);
          {$ENDIF}
          If CField = 0 then                 {Fix 5.01d}
             ID := ITTT.CurrentField         {stay put!}
          else
          begin
              If CField <> ITTT.CurrentField then
                 ID := CField; {user wants to go to a specific field}
          end;
          Check_Refresh_State(Refresh);
          If Finished then exit;
          If ID = 0 then
          begin
              Finished := true;
          end
          else
          begin
              ITTT.CurrentField := ID;
              CField := ID;
              {Enter Field Hook}
              Repeat
                   ITTT.CurrentField := CField;
                   Refresh := Refresh_None;
                   {$IFNDEF VER40}
                   ITTT.EnterFieldHook(CField,Refresh);
                   {$ELSE}
                   If IO_EnterHook <> Nil then
                      CallEnterFieldHook(CField,Refresh);
                   {$ENDIF}
                   Check_Refresh_State(Refresh);
                   If Finished then exit;
              until CField = ITTT.CurrentField;
              If (ITTT.CurrentField < 1)
              or (ITTT.CurrentField > ITTT.TotalFields) then
                  exit;                      {5.00b}
              HiLight(ITTT.CurrentField);
              If FieldDefn[ITTT.CurrentField]^.MsgX <= 80 then
                 DisplayMessage(ITTT.CurrentField);
              With FieldDefn[ITTT.CurrentField]^ do
                  GotoXY(CursorX,Y);
              {Ding;}
          end;  {If ID = 0};
     end; {with Table}
  end;  {proc change fields}

  Procedure Erase_Field(ID:byte);
  begin
      with Table[CurrentTable]^.FieldDefn[ID]^ do
      begin
          FieldStr := '';
          String_to_Var(ID);
          Set_Cursor(ID);
      end;
  end;

  Procedure Global_Erase;
  var
     I : integer;
     S : string;
     Ch : char;
  begin
      Ding;
      S := 'Erase all entries!  Are you sure? (Y/N)';
      With Table[CurrentTable]^.ITTT do
          TempMessageCh(1,ErrorLine,MsgFCol,MsgBCol,PadCenter(S,80,' '),CH);
      If Upcase(Ch) <> 'Y' then exit;
      with Table[CurrentTable]^ do
      begin
          For I :=  1 to ITTT.TotalFields do
              Erase_Field(I);
          Display_All_Fields;
          ITTT.CurrentField := 1;
      end;
  end;

  Procedure Cursor_Right;
  begin
      With Table[CurrentTable]^ do
           with FieldDefn[ITTT.CurrentField]^ do
           begin
              If (Right_Justify and (StrLocX < length(FieldStr)) and (StrLocX < FieldLen)) or
                 ((Right_Justify = false) and (StrLocX <= length(FieldStr)) and (StrLocX < FieldLen))then
              begin
                  Inc(StrLocX);
                  Repeat
                       Inc(CursorX);
                  Until FieldFmt[CursorX + 1 - X] in FmtChars;
              end;
              GotoXY(CursorX,Y);
          end; {with}
  end; {Proc Cursor_Right}

  Procedure Cursor_Left;
  begin
      with Table[CurrentTable]^ do
           With FieldDefn[ITTT.CurrentField]^ do
           begin
               If (StrLocX > 1)
               or ( Right_Justify and (StrLocX > 0) and (length(FieldStr) <> FieldLen) ) then
               begin
                   dec(StrLocX);
                   Repeat
                        dec(CursorX);
                   Until FieldFmt[CursorX + 1 - X] in FmtChars;
               end;
           end;  {with}
  end;  {Proc Cursor_left}

  Procedure Cursor_Home;
  var
    Counter1, Counter2 : byte;
  begin
      with Table[CurrentTable]^ do
           With FieldDefn[ITTT.CurrentField]^ do
                Repeat
                     Counter1 := CursorX;
                     Cursor_Left;
                Until Counter1 = CursorX;
  end;  {Proc Cursor_Home}

  Procedure Delete_Char;
  var
    I : integer;
  begin
      with Table[CurrentTable]^ do
           with FieldDefn[ITTT.CurrentField]^ do   {non format characters}
           begin
               If StrLocX > 0 then
               begin
                  Delete(FieldStr,StrLocX,1);
                  If Right_Justify then
                     Dec(StrLocX);
               end;
           end;  {with}
  end;  {Delete_Chars}

  Procedure Backspaced;
  begin
      with Table[CurrentTable]^ do
           with FieldDefn[ITTT.CurrentField]^ do
           begin
               If StrLocX > 1 then
               begin
                   If Right_Justify then
                   begin
                       Delete(FieldStr,pred(StrLocX),1);
                       Dec(StrLocX);
                   end
                   else
                   begin
                       Cursor_Left;
                       Delete(FieldStr,StrLocX,1);
                   end;
               end;
           end;  {with}
  end;  { Proc Backspaced }

  Procedure Finish_Input;
  {}
  var ValidInput : byte;
  begin
{$IFDEF IOFULL}
      Validate_Field(Table[CurrentTable]^.ITTT.CurrentField,ValidInput);
      If ValidInput = Valid then
      begin
{$ENDIF}
          String_to_Var(Table[CurrentTable]^.ITTT.CurrentField);
          Finished := true;
{$IFDEF IOFULL}
      end;
{$ENDIF}
  end; {of proc Finish_Input}

  Procedure Insert_Character(K : char);
  begin
      with Table[CurrentTable]^ do
           with FieldDefn[ITTT.CurrentField]^ do
           begin
               If (length(FieldStr) < FieldLen) then
               begin
                   If Right_Justify then
                   begin
                       Inc(StrLocX);
                       Insert(K,FieldStr,StrLocX);
                   end
                   else
                   begin
                       Insert(K,FieldStr,StrLocX);
                       Cursor_Right;
                   end;
               end
               else
               If (FieldLen = 1) then    {fix 5.00c}
                   FieldStr := K
               else
                   Ding;
      end;
  end;

  Procedure OverType_Character(K : char);
  begin
      with Table[CurrentTable]^ do
           with FieldDefn[ITTT.CurrentField]^ do
           begin
               If (StrLocX = 0) and Right_Justify then
               begin
                   Insert(K,FieldStr,StrLocX);
                   Inc(StrLocX);
               end
               else
               begin
                   Delete(FieldStr,StrLocX,1);
                   Insert(K,FieldStr,StrLocX);
                   Cursor_Right;
               end;
           end;
  end;

  Procedure Activity;
  var
    K : char;
    ReturnStr: string;
    Prior_CursorX : byte;
    ValidInput : byte;
    OldField : byte;
    CField : byte;
    Refresh: byte;
  begin
      OldField := Table[CurrentTable]^.ITTT.CurrentField;
      K := Getkey;
      {now the character hook}
      With Table[CurrentTable]^ do
      begin
          CField := ITTT.CurrentField;
          ReFresh := Refresh_None;
          {$IFNDEF VER40}
          ITTT.CharHook(K,CField,Refresh);
          {$ELSE}
          If IO_CharHook <> Nil then
              CallCharHook(K,CField,Refresh);
          {$ENDIF}
          Check_Refresh_State(Refresh);
          If CField <> ITTT.CurrentField then
             Change_Fields(CField); {user wants to go to a specific field}
          If K = ITTT.FinishChar then
             Finish_Input
          else
{$IFDEF IOFULL}
             If  (FieldDefn[ITTT.CurrentField]^.Allow_Char <> [#0])
             and (not (K in FieldDefn[ITTT.CurrentField]^.Allow_Char))
             and (not (K in Control_Char)) then
             begin
                 If K <> No_Char then          {5.00g}
                    Ding;
                 Exit;
             end;
{$ELSE}
;
{$ENDIF}
      end;

      If (K <> No_Char)
      and (Finished = false) then
      begin
          If Extended then
          begin
          Case K of
          #132   {mouse right but}
                : If Table[CurrentTable]^.ITTT.AllowEsc then
                     begin
                         Finished := true;
                      end
                      else Ding;

          #133,      {mouse left but}
          #131,      {mouse right}
          IORightFld
                 :  with Table[CurrentTable]^ do
                         Change_Fields(FieldDefn[ITTT.CurrentField]^.RightField);
          #130,      {mouse left}
          IOLeftFld,
          IOShiftTab : with Table[CurrentTable]^ do
                           Change_Fields(FieldDefn[ITTT.CurrentField]^.LeftField);
          IODel    : Delete_Char;
          IOLeft   : Cursor_Left;
          IORight  : Cursor_Right;
          #128,    {mouse up}
          IOUp     : with Table[CurrentTable]^ do
                          Change_Fields(FieldDefn[ITTT.CurrentField]^.UpField);
          #129,    {mouse down}
          IODown   : with Table[CurrentTable]^ do
                          Change_Fields(FieldDefn[ITTT.CurrentField]^.DownField);
          IOErase    :with Table[CurrentTable]^ do
                           Erase_Field(ITTT.CurrentField);
          IOTotErase : Global_Erase;
          IOIns      : with Table[CurrentTable]^ do
                       begin
                           ITTT.Insert := not ITTT.Insert;
                           {$IFNDEF VER40}
                           ITTT.InsertProc(ITTT.Insert);
                           {$ELSE}
                            If IO_InsertHook <> Nil then
                               CallInsertHook(ITTT.Insert);
                           {$ENDIF}
                       end;
          #199       : Cursor_Home;
          #207       : with Table[CurrentTable]^ do
                          Set_Cursor(ITTT.CurrentField);
          else Ding;
      end; {case}
      end
      else                        {ordinary character}
      begin
         case K of
          IOEsc : If Table[CurrentTable]^.ITTT.AllowEsc then
                     begin
                         Finished := true;
                      end
                      else Ding;
          IOBackSp : Backspaced;
          IOTab,
          IOEnter :  with Table[CurrentTable]^ do
                         Change_Fields(FieldDefn[ITTT.CurrentField]^.RightField);
          #32..#255 : with Table[CurrentTable]^ do
                with FieldDefn[ITTT.CurrentField]^ do
                begin
                    If FieldFmt[CursorX - X + 1] = '!' then K := upcase(K);
          {$IFDEF IOFULL}
                    If (
                         (Allow_Char = [#0])
                         or ((Allow_Char <> [#0]) and (K in Allow_Char))
                       )
                    and
                       (
                         (DisAllow_Char = [#0])
                         or ((DisAllow_Char <> [#0]) and ((K in DisAllow_Char)= false))
                       )
                    then
                    begin
           {$ENDIF}
                        If ((K in ['0'..'9','.','-','e','E']) and (FieldFmt[CursorX - X + 1] = '#'))
                        or (((K in ['a'..'z','A'..'Z',' ',',','.',';',':']) or (K in IntCharacters )) and
                                                  (FieldFmt[CursorX - X + 1] = '@'))
                        or (FieldFmt[CursorX - X + 1] = '*')
                        or (FieldFmt[CursorX - X + 1] = '!') then
                        begin
           {$IFDEF IOFULL}
                            If FirstCharPress then
                            begin
                                If Erase_Default then
                                   Erase_Field(ITTT.CurrentField);
                                FirstCharPress := false;
                            end;
            {$ENDIF}
                            If (ITTT.Insert) then
                               Insert_Character(K)
                            else
                               OverType_Character(K);
                        end
                        else Ding; {end if K in statement}
            {$IFDEF IOFULL}
                    end; {if}
            {$ENDIF}
                end;  {with}

         end; {case}
      end;
      end;
      HiLight(Table[CurrentTable]^.ITTT.CurrentField);
      with Table[CurrentTable]^ do
           with FieldDefn[ITTT.CurrentField]^ do
                GotoXY(CursorX,Y);

{$IFDEF IOFULL}
      with Table[CurrentTable]^ do
           with FieldDefn[ITTT.CurrentField]^ do
           begin
               If  (FirstCharPress = false)
               and (Jump_Full)
               and (StrLocX = FieldLen)
               and (Length(FieldStr) = FieldLen)
               and (ITTT.Insert)
               and (K in [#32..#126])
               and (Jump_Full) then
                   Change_Fields(FieldDefn[ITTT.CurrentField]^.RightField);
           end;
{$ENDIF}
      If Table[CurrentTable]^.ITTT.CurrentField <> OldField then  {5.00l}
         FirstCharPress := true
      else
         FirstCharPress := false;
      I_Char := K;
  end;    {Proc Activity}


begin   {Process_Input}
    If not TableSet then
        IOTTT_Error(14,0.0);  {5.01}
    with Table[CurrentTable]^ do
    begin
        If ITTT.Displayed = false then Display_All_Fields;
        If StartField in [1..ITTT.TotalFields] then
           ITTT.CurrentField := StartField
        else
           StartField := 1;
        {Enter Field Hook}        {5.00m}
        SField := StartField;
        Finished := false;
        Repeat
             ITTT.CurrentField := SField;
             SRefresh := Refresh_None;
             {$IFNDEF VER40}
             ITTT.EnterFieldHook(SField,SRefresh);
             {$ELSE}
             If IO_EnterHook <> Nil then
                CallEnterFieldHook(SField,SRefresh);
             {$ENDIF}
             Check_Refresh_State(SRefresh);
             If Finished then exit;
        until SField = ITTT.CurrentField;
        Hilight(ITTT.CurrentField);
        If FieldDefn[ITTT.CurrentField]^.MsgX <= 80 then
        DisplayMessage(Table[CurrentTable]^.ITTT.CurrentField);
        GotoXY(FieldDefn[ITTT.CurrentField]^.CursorX,
               FieldDefn[ITTT.CurrentField]^.Y);
        FirstCharPress := true;
        {$IFNDEF VER40}                          {5.00j}
        ITTT.InsertProc(ITTT.Insert);
        {$ELSE}
        If IO_InsertHook <> Nil then
           CallInsertHook(ITTT.Insert);
        {$ENDIF}
        repeat
             Activity;
        until Finished;
        If FieldDefn[ITTT.CurrentField]^.MsgX <= 80 then   {5.02d}
             RemoveMessage(ITTT.CurrentField);
    end;
end;   {Process_Input}

begin  {Initial Auto proc}
    CurrentTable := 1;
    TableSet := False;
end.

