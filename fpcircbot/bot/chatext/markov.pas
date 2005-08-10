Unit Markov;

{$mode objfpc}

Interface

Uses Dict, Break, MarkovTable;

Const
	DictFile = 'words.txt';
	MarkovFile = 'markov.txt';
	ErrorMargin = 25; // 25%

Var
	CorrectStrings : Boolean; // If you need chars corrected from latin to english script

Procedure InitDict;
Procedure DoneDict;

Procedure TalkTo(St : AnsiString);
Function TalkFrom(N : Byte): AnsiString;

Function Talk(St : AnsiString): AnsiString;

Implementation

Var
	Dictionary  : TDict;
	MTable      : TMarkovTable;
	Tokens      : TTokenList;
	Coins       : Array[0..255] Of Integer;
	NCoins      : Byte;
	Key         : Integer;

Procedure InitDict;
Begin
	Dictionary := TDict.Create(DictFile);
	Dictionary.Load;
	MTable := TMarkovTable.Create(MarkovFile);
	MTable.Load;
End;

Procedure DoneDict;
Begin
	Dictionary.Flush;
	Dictionary.Destroy;
	MTable.Flush;
	MTable.Destroy;
End;

Function Prune(C : Char): Char;
Begin
	If C In [#001..#096, #123..#255] Then
		Prune := ' '
	Else
		Prune := C;
End;

Function Translate(C : Char): Char;
Begin
	If C In ['ä', 'â', 'ã', 'à', 'á', 'Ä', 'Â', 'Ã', 'À', 'Á'] Then 
		Translate := 'a'
	Else
	If C In ['ö', 'ô', 'õ', 'ò', 'ó', 'Ö', 'Ô', 'Õ', 'Ò', 'Ó'] Then 
		Translate := 'o'
	Else	
	If C In ['ë', 'ê', 'è', 'é', 'Ë', 'Ê', 'È', 'É'] Then 
		Translate := 'e'
	Else
	If C In ['ï', 'î', 'ì', 'í', 'Ï', 'Î', 'Ì', 'Í'] Then 
		Translate := 'i'
	Else
	If C In ['ü', 'û', 'ù', 'ú', 'Ü', 'Û', 'Ù', 'Ú'] Then 
		Translate := 'u'
	Else
	If C In ['ç', 'Ç'] Then 
		Translate := 'c' 
	Else
		Translate := C;
End;

Function Correct(St : AnsiString): AnsiString;
Var 
	Tmp : AnsiString;
	Ctrl : Byte;
Begin
	Tmp := St;
	For Ctrl := 1 To Length(St) Do
		Tmp := Tmp + LowerCase(Prune(Translate(St[Ctrl])));
	Correct := Tmp;
End;

Procedure TalkTo(St : AnsiString);
Var
	Ctrl : Byte;
Begin
	If St = '' Then
		Exit;
	If CorrectStrings Then
		Tokens := BreakApart(Correct(St))
	Else
		Tokens := BreakApart(St);
	For Ctrl := 0 To 255 Do
		Coins[Ctrl] := -1;
	For Ctrl := 0 To 255 Do
		If Tokens[Ctrl] <> '' Then
			Coins[Ctrl] := Dictionary.InsertWord(Tokens[Ctrl]);
	For Ctrl := 0 To 255 Do
		If Coins[Ctrl] <> -1 Then
		Begin
			MTable.AppendWord(Coins[Ctrl]);
			NCoins := Ctrl;
		End;
	MTable.AppendWord(-1); // separator
End;

Function RandomFromTo(X, Y : Integer): Integer;
Var Tmp : Integer;
Begin
	Repeat
		Tmp := X + Random(Y - X);
	Until ((Tmp >= X) and (Tmp <= Y));
	RandomFromTo := Tmp;
End;

Function MostProbable(K, E : Integer): Integer;
Var
	Ctrl    : Integer;
	Total   : Cardinal;
	Partial : Array Of Integer;
Begin
	SetLength(Partial, Dictionary.Count + 1);
	Total := 1;
	For Ctrl := 0 To (Dictionary.Count - 1) Do
	Begin
		Partial[Ctrl] := MTable.Hits(K, Ctrl);
		Inc(Total, MTable.Hits(K, Ctrl));
	End;
	For Ctrl := 0 To (Dictionary.Count - 1) Do
		Partial[Ctrl] := Round((Partial[Ctrl] / (Total / 100)));
	MostProbable := -1;
	For Ctrl := 0 To (Dictionary.Count - 1) Do
		If (Partial[Ctrl] >= E - ErrorMargin) And (Partial[Ctrl] <= E + ErrorMargin) Then
		Begin
			MostProbable := Ctrl;
			Exit;
		End;
	SetLength(Partial, 0);
End;

Function TalkFrom(N : Byte): AnsiString;
Var 
	Ctrl  : Byte;
	Temp  : String;
Begin
	Ctrl := 1;
	Key := Coins[RandomFromTo(0, N)];
	Temp := Dictionary.Words[Key];
	While (Ctrl <= N - 1) And (Key >= 0) Do
	Begin
		Key := MostProbable(Key, RandomFromTo(50, 100));
		If Key >= 0 Then
			Temp := Temp + ' ' + Dictionary.Words[Key];
		Inc(Ctrl);
	End;
	TalkFrom := Temp;
End;

Function Talk(St : AnsiString): AnsiString;
Begin
	TalkTo(St);
	Talk := TalkFrom(NCoins);
End;

Begin
	CorrectStrings := False;
End.
