Unit MarkovDict;

{$mode objfpc}{$h+}

Interface

Type
	TArrayOfString = Array Of AnsiString;

	TMarkovDict = Class
	Private
		fName   : String;
		fNumber : Integer;
		fWords  : TArrayOfString;
	Public
		Constructor Create(Arq : String);
		Destructor Destroy; Override;
		Procedure Load;
		Procedure Flush;
		Function InsertWord(St : AnsiString): Integer;
		Function FindWord(St : AnsiString): Integer;
		Property Words : TArrayOfString Read fWords Write fWords;
		Property Count : Integer Read fNumber;
	End;

Implementation

Constructor TMarkovDict.Create(Arq : String);
Begin
	Inherited Create;
	fName := Arq;
	fNumber := 0;
	SetLength(fWords, 0);
End;

Destructor TMarkovDict.Destroy;
Begin
	fNumber := 0;
	fName := '';
	SetLength(fWords, 0);
	Inherited Destroy;
End;

// Loads the list from a file
Procedure TMarkovDict.Load;
Var 
	Handler : Text;
	Temp    : String;
Begin
	Assign(Handler, fName);
	Reset(Handler);
	While Not(Eof(Handler)) Do
	Begin
		ReadLn(Handler, Temp);
		InsertWord(Temp);
	End;
	Close(Handler);
	fNumber := Length(fWords);
End;

// Saves the list to a file
Procedure TMarkovDict.Flush;
Var 
	Handler : Text;
	Ctrl    : Cardinal;
Begin
	If fNumber <= 0 Then
		Exit;
	Assign(Handler, fName);
	Rewrite(Handler);
	For Ctrl := 0 To (fNumber - 1) Do
		WriteLn(Handler, fWords[Ctrl]);
	Close(Handler);
	fNumber := Length(fWords);
End;

// adds a new word to the list returning his value or 
// returns the value of a word if it is already in the list
Function TMarkovDict.InsertWord(St : AnsiString): Integer;
Var
	Ctrl  : Cardinal;
	Found : Boolean;
Begin
	fNumber := Length(fWords);
	Found := False;
	If (St = '') Then
	Begin
		InsertWord := (-1);
		Exit;
	End;
	If fNumber > 0 Then
		For Ctrl := 0 To (fNumber - 1) Do // Finds if the word already exists in the list
			If fWords[Ctrl] = St Then
			Begin
				Found := True;
				InsertWord := (Ctrl);
				Exit;
			End;
	If Not Found Then
	Begin
		Inc(fNumber);
		SetLength(fWords, fNumber);
		fWords[fNumber - 1] := St;
		InsertWord := (fNumber - 1);
	End;
End;

// Finds a word in the list, returning -1 if the word
// isn't already there
Function TMarkovDict.FindWord(St : AnsiString): Integer;
Var
	Ctrl  : Cardinal;
	Found : Boolean;
Begin
	fNumber := Length(fWords);
	Found := False;
	If (St = '') Then
	Begin
		FindWord := (-1);
		Exit;
	End;
	If fNumber > 0 Then
		For Ctrl := 0 To (fNumber - 1) Do
			If fWords[Ctrl] = St Then
			Begin
				Found := True;
				FindWord := (Ctrl);
				Exit;
			End;
	If Not Found Then
		FindWord := -1;
End;

End.