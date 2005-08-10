Unit Break;

Interface

Type
    TTokenList = Array[0..255] Of String;

Function BreakApart(LineText : AnsiString): TTokenList;

Implementation

Function IsWhite(C : Char): Boolean;
Begin
	IsWhite := C In [
	#$20..#$2C,
	#$3A..#$40,
	#$7E..#$FF,
	#$2E, 
	#$2F,
	#$5E,
	#$60
	];
End;

Function IsAlpha(C : Char): Boolean;
Begin
	IsAlpha := C In ['a'..'z', 'A'..'Z', '0'..'9', '-', '_'];
End;

Function BreakApart(LineText : AnsiString): TTokenList;
Var
    Ctrl1      : Byte;
    Ctrl2      : Byte;
    LineTokens : TTokenList;

	Procedure ParseWhite;
	Begin
		While IsWhite(LineText[Ctrl1]) And (Ctrl1 <= Length(LineText)) Do
			Inc(Ctrl1);
	End;
	
	Procedure ParseAlpha;
	Begin
		LineTokens[Ctrl2] := '';
		While IsAlpha(LineText[Ctrl1]) And (Ctrl1 <= Length(LineText)) Do
		Begin
			LineTokens[Ctrl2] := LineTokens[Ctrl2] + '' + LineText[Ctrl1];
			Inc(Ctrl1);
		End;
		Inc(Ctrl2);
	End;

Begin
	For Ctrl2 := 0 To 255 Do
        LineTokens[Ctrl1] := '';
	Ctrl2 := 0;
	Ctrl1 := 1;
	While (Ctrl1 <= Length(LineText)) Do
		If IsAlpha(LineText[Ctrl1]) Then
			ParseAlpha
		Else
			If IsWhite(LineText[Ctrl1]) Then
				ParseWhite
			Else
				Inc(Ctrl1);
	For Ctrl1 := Ctrl2 + 1 To 255 Do
		LineTokens[Ctrl1] := '';
	BreakApart := LineTokens;
End;

End.
