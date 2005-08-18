Unit Markov;

{$mode objfpc}{$h+}

Interface

Uses MarkovDict, MarkovTable, Classes;

Type
	// Holds the last words used by the engine in order to 
	// avoid recursions
	TLastWordsUsed = Class
	Private
		fBuffer : Array Of Integer;
		fPos    : Integer;
	Public
		Constructor Create(Size : Cardinal);
		Destructor Destroy; Override;
		Procedure WordSaid(W : Integer);
		Function WordUsed(W : Integer): Boolean;
		Procedure Show(Dict : TMarkovDict);
	End;

	// A generic markov text generator
	TMarkov = Class
	Private
		fErrorMargin  : Byte; // Deviation
		fThreshold    : Byte; // Limiar level for a construction to be considered "right"
		fDict         : TMarkovDict;
		fTable        : TMarkovTable;
		fUsed         : TLastWordsUsed;
		fStartCode    : Integer;
		fEndCode      : Integer;
		// Number of words in the dictionary
		Function WordsCount : Integer;
		// Number of entries in the markov chain
		Function MarkovCount : Integer;
		// A random word to follow K, with at least E% usage
		Function MostProbable(K, E : SmallInt): SmallInt;
	Public
		// D = Dict file
		// P = Phrase file 
		// M = Deviation 
		// T = Threshold
		Constructor Create(D, P : String; M, T : Byte);
		Destructor Destroy; Override;
		// Use talkto when you need to write a new phrase into the database
		// without generating a response (I.E. Write-Only)
		Procedure TalkTo(W : TStringList);
		// Use talkfrom when you want to generate a new phrase from the
		// current database
		Function TalkFrom(K : SmallInt): AnsiString;
		// Use talk to write to and generate a response at the same time
		Function Talk(W : TStringList): AnsiString;
		// Error margin is the max deviation from a central threshold level
		Property ErrorMargin : Byte Read fErrorMargin Write fErrorMargin;
		// Threshold is the level where a construction is marked as good
		// language one
		Property Threshold : Byte Read fThreshold Write fThreshold;
		// The ammount of words in the dictionary
		Property DictionaryWords : Integer Read WordsCount;
		// The ammount of words in the markov chain
		Property EntriesMarkov : Integer Read MarkovCount;
	End;

Implementation

// Generates (guaranteed) a random number wich value in the [X, Y] Range
Function RandomFromTo(X, Y : SmallInt): SmallInt;
Var 
	Tmp : SmallInt;
Begin
	Repeat
		Tmp := X + Random(Y - X);
	Until ((Tmp >= X) and (Tmp <= Y));
	RandomFromTo := Tmp;
End;

// TMarkov

Constructor TMarkov.Create(D, P : String; M, T : Byte);
Begin
	Inherited Create;
	fDict := TMarkovDict.Create(D);
	fDict.Load;
	fTable := TMarkovTable.Create(P);
	fTable.Load;
	fUsed := TLastWordsUsed.Create(16);
	fErrorMargin := M;
	fThreshold   := T;
	fStartCode   := fDict.InsertWord(ccStartToken);
	fEndCode     := fDict.InsertWord(ccEndToken);
	WriteLn('Phrase start code : ', fStartCode);
	WriteLn('Phrase end code   : ', fEndCode);
End;

Destructor TMarkov.Destroy;
Begin
	fDict.Flush;
	fDict.Destroy;
	fTable.Flush;
	fTable.Destroy;
	fUsed.Destroy;
	Inherited Destroy;
End;

Procedure TMarkov.TalkTo(W : TStringList);
Var
	Ctrl : Cardinal;
Begin
	If Assigned(W) Then 
	Begin
		If W.Count <= 0 Then
			Exit; // Do not even try...
		// Simply converts each word into a dictionary entry point
		// and then add the phrase to the end of the serial "markov chain"
		// (the transition matrix is actually generated on the fly from 
		// the list of phrases feed into the engine)
		fTable.AppendWord(fStartCode);
		For Ctrl := 0 To W.Count - 1 Do
			fTable.AppendWord(fDict.InsertWord(W[Ctrl]));
		// keeps one phrase apart from the next
		fTable.AppendWord(fEndCode); 
	End;
End;

// Number of words in the dictionary
Function TMarkov.WordsCount : Integer;
Begin
	WordsCount := fDict.Count;
End;

// Number of entries in the markov chain
Function TMarkov.MarkovCount : Integer;
Begin
	MarkovCount := fTable.Count;
End;

Function TMarkov.MostProbable(K, E : SmallInt): SmallInt;
Var
	Ctrl     : SmallInt;
	Highest  : SmallInt;
	NextKey  : SmallInt;
	Partial  : Array Of SmallInt;
	Penalty  : Array Of Double;
	Possible : Array Of Boolean;
	Found    : Boolean;

Begin
	Highest := 0;
	SetLength(Partial, fDict.Count);
	SetLength(Possible, fDict.Count);
	SetLength(Penalty, fDict.Count);
	Write('Generating transition matrix ... ');
	// Generates the transition matrix for K
	// from K to each possible word
	For Ctrl := 0 To (fDict.Count - 1) Do
	Begin
		Partial[Ctrl] := fTable.Hits(K, Ctrl);
		// Sets the penalty for every word
		// If a word was already used, 
		// the penalty is 25% of its hit value
		// else, penalty is 0% 
		// (something * 0.5 is equivalent to 0.5 penalty)
		If fUsed.WordUsed(Ctrl) Then
			Penalty[Ctrl] := 0.5
		Else
			Penalty[Ctrl] := 1;
		If Partial[Ctrl] > Highest Then
			Highest := Partial[Ctrl];
	End;
	WriteLn('Done.');
	// Transforms from "hits" into percentual (%) values
	// relative to the highest scoring word in the markov table
	// If a value is higher than the threshold (E) then this is a "possible"
	// word to follow K in a normal text
	Write('Applying penalties... ');
	For Ctrl := 0 To (fDict.Count - 1) Do
		If Partial[Ctrl] > 0 Then
			Possible[Ctrl] := Round((Partial[Ctrl] * Penalty[Ctrl]) / (Highest / 100)) > E
		Else
			Possible[Ctrl] := False;
	WriteLn('Done.');
	// next value for K (nextkey) is a random word, but
	// it will only be accepted when its a "possible" value
	Ctrl := 0; // Ctrl Will work as a sort of timeout;
	Found := False;
	NextKey := 0;
	// loop until find a possible word or tried to much...
	// (try at least twice the ammount of words in the dictionary)
	Write('Seeking probable key... ');
	While Not(Possible[NextKey]) And (Ctrl <= fDict.Count * 2) Do 
	Begin
		NextKey := RandomFromTo(fEndCode, (fDict.Count - 1));
		Inc(Ctrl);
		If Possible[NextKey] Then
			Found := True;
	End;
	WriteLn('Done.');
	// most probable value
	If Found Then
		MostProbable := NextKey
	Else
		MostProbable := fEndCode;
End;

Function TMarkov.TalkFrom(K : SmallInt): AnsiString;
Var 
	Temp  : AnsiString;
	Key   : SmallInt;
Begin
	Key  := K;
	If Key < 0 Then 
		Exit;
	WriteLn('Last used words');
	fUsed.Show(fDict);
	WriteLn('End of used words');
	Temp := '';
	// Start with the word corresponding to K
	Temp := fDict.Words[Key];
	// while theres not enought words in the buffer
	// and the next key isnt a unaceptable value
	While (Key > fEndCode) Do
	Begin
		// The next word will be the most probable
		Key := MostProbable(Key, RandomFromTo(fThreshold - fErrorMargin, fThreshold + fErrorMargin));
		If Key > fEndCode Then
		Begin
			WriteLn('Found next keyword. ', fDict.Words[Key]);
			// Append the next word in the phrase
			Temp := Temp + ' ' + fDict.Words[Key];
			fUsed.WordSaid(Key);
		End;
	End;
	TalkFrom := Temp;
End;

Function TMarkov.Talk(W : TStringList): AnsiString;
Var
	Ctrl     : Integer;
	Start    : TArrayOfInteger;
	Highest  : Integer;
	Where    : Integer;
	Temp     : AnsiString;
	WordUse  : TArrayOfInteger;
	MostUse  : Integer;
	Possible : Array Of Boolean;

Begin
	Temp := '';
	If Assigned(W) Then 
		While Temp = '' Do
		Begin
			WriteLn('<< Generating text...');
			TalkTo(W);
			WriteLn('Input words : ');
			For Ctrl := 0 To W.Count - 1 Do
				Write(fDict.FindWord(W[Ctrl]), ' ');
			WriteLn;
			WriteLn('End of input words.');
			SetLength(Start, W.Count);
			Highest := 0;
			Where := 0;
			SetLength(Possible, fDict.Count);
			// Detects the word most likely to start a phrase
			Write('Calculating combination levels for start words... ');
			For Ctrl := 0 To W.Count - 1 Do
			Begin
				Start[Ctrl] := fTable.Hits(fStartCode, fDict.FindWord(W[Ctrl]));
				If Start[Ctrl] > Highest Then
					Highest := Start[Ctrl];
			End;
			WriteLn('Done.');
			WordUse := fTable.CountWords(fDict.Count);
			MostUse := 0;
			Write('Calculating usage levels for start words... ');
			For Ctrl := 0 To fDict.Count - 1 Do
				If WordUse[Ctrl] > MostUse Then
					MostUse := WordUse[Ctrl];
			WriteLn('Done.');
			Write('Setting up a reconciliation matrix... (Word pair, ');
			For Ctrl := 0 To W.Count - 1 Do
				Possible[fDict.FindWord(W[Ctrl])] := (Start[Ctrl] > Round(Highest * 0.50));
			Write('Word significance, ');
			For Ctrl := 0 To fDict.Count - 1 Do
				Possible[Ctrl] := Possible[Ctrl] And (WordUse[Ctrl] < Round(MostUse * 0.50));
			Write('Already used) ');
			For Ctrl := 0 To fDict.Count - 1 Do
				Possible[Ctrl] := Possible[Ctrl] And Not(fUsed.WordUsed(Ctrl));
			WriteLn('Done.');
			Where := RandomFromTo(fEndCode + 1, fDict.Count - 1);
			// in the input where is the most likely word to start a phrase ?
			Write('Selecting starting keyword... ');
			For Ctrl := 0 To fDict.Count - 1 Do
				If Possible[Ctrl] Then
					Where := Ctrl;
			WriteLn('Done : ', Where);
			SetLength(WordUse, 0);
			fUsed.WordSaid(Where);
			WriteLn('Generating text now : ');
			// Talk using it as the start keyword
			Temp := TalkFrom(Where);
			WriteLn('<< Done : ', Temp);
		End;
	Talk := Temp;
End;

// TLastWordsUsed

Constructor TLastWordsUsed.Create(Size : Cardinal);
Var
	Ctrl : Cardinal;
Begin
	Inherited Create;
	SetLength(fBuffer, Size);
	For Ctrl := 0 To (Size - 1) Do
		fBuffer[Ctrl] := -1;
	fPos := 0;
End;

Destructor TLastWordsUsed.Destroy; 
Begin
	SetLength(fBuffer, 0);
	fPos := -1;
	Inherited Destroy;
End;

Procedure TLastWordsUsed.WordSaid(W : Integer);
Begin
	If fPos >= Length(fBuffer) Then
		fPos := 0;
	fBuffer[fPos] := W;
	Inc(fPos);
End;

Function TLastWordsUsed.WordUsed(W : Integer): Boolean;
Var
	Ctrl : Cardinal;
Begin
	WordUsed := False;
	For Ctrl := 0 To (Length(fBuffer) - 1) Do
		If fBuffer[Ctrl] = W Then
		Begin
			WordUsed := True;
			Exit;
		End;
End;

Procedure TLastWordsUsed.Show(Dict : TMarkovDict);
Var
	Ctrl : Cardinal;
Begin
	For Ctrl := 0 To (Length(fBuffer) - 1) Do
		If fBuffer[Ctrl] > 0 Then
			Write(Dict.Words[fBuffer[Ctrl]], ' ');
	WriteLn;
End;

End.
