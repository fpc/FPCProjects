Unit Dict;

{$mode objfpc}

Interface

Type
	TArrayOfString = Array Of String;
	
	TDict = Class
	Private
		fName   : String;
		fNumber : Integer;
		fWords  : TArrayOfString;
	Public
		Constructor Create(Arq : String);
		Destructor Destroy; Override;
		Procedure Load;
		Procedure Flush;
		Function InsertWord(St : String): Integer;
		Property Words : TArrayOfString Read fWords Write fWords;
		Property Count : Integer Read fNumber;
	End;

Implementation

Constructor TDict.Create(Arq : String);
Begin
	Inherited Create;
	fName := Arq;
	fNumber := 0;
	SetLength(fWords, 0);
End;

Destructor TDict.Destroy;
Begin
	fNumber := 0;
	fName := '';
	SetLength(fWords, 0);
	Inherited Destroy;
End;

Procedure TDict.Load;
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

Procedure TDict.Flush;
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

Function TDict.InsertWord(St : String): Integer;
Var
	Ctrl  : Cardinal;
	Found : Boolean;
Begin
	fNumber := Length(fWords);
	Found := False;
	If St = '' Then
	Begin
		InsertWord := (-1);
		Exit;
	End;
	If fNumber > 0 Then
		For Ctrl := 0 To (fNumber - 1) Do
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

End.