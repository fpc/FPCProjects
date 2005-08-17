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
	End;

	// A generic markov text generator
	TMarkov = Class
	Private
		fErrorMargin : Byte; // Deviation
		fThreshold   : Byte; // Limiar level for a construction to be considered "right"
		fDict        : TMarkovDict;
		fTable       : TMarkovTable;
		fUsed        : TLastWordsUsed;
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
		Function TalkFrom(K, N : SmallInt): AnsiString;
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
		For Ctrl := 0 To W.Count - 1 Do
			fTable.AppendWord(fDict.InsertWord(W[Ctrl]));
		// keeps one phrase apart from the next
		fTable.AppendWord(-1); 
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
	Penalty  : Array Of Boolean;
	Possible : Array Of Boolean;
	Found    : Boolean;

Begin
	Highest := 0;
	SetLength(Partial, fDict.Count);
	SetLength(Possible, fDict.Count);
	SetLength(Penalty, fDict.Count);
	// Generates the transition matrix for K
	// from K to each possible word
	For Ctrl := 0 To (fDict.Count - 1) Do
	Begin
		Partial[Ctrl] := fTable.Hits(K, Ctrl);
		Penalty[Ctrl] := fUsed.WordUsed(Ctrl);
		If Partial[Ctrl] > Highest Then
			Highest := Partial[Ctrl];
	End;
	// Transforms from "hits" into percentual (%) values
	// relative to the highest scoring word in the markov table
	// If a value is higher than the threshold (E) then this is a "possible"
	// word to follow K in a normal text
	For Ctrl := 0 To (fDict.Count - 1) Do
		If Partial[Ctrl] > 0 Then
			If Not(Penalty[Ctrl]) Then
				Possible[Ctrl] := Round(Partial[Ctrl] / (Highest / 100)) > E
			Else
				Possible[Ctrl] := False
		Else
			Possible[Ctrl] := False;
	// next value for K (nextkey) is a random word, but
	// it will only be accepted when its a "possible" value
	Ctrl := 0; // Ctrl Will work as a sort of timeout;
	Found := False;
	NextKey := 0;
	// loop until find a possible word or tried to much...
	// (try at least twice the ammount of words in the dictionary)
	While Not(Possible[NextKey]) And (Ctrl <= fDict.Count * 2) Do 
	Begin
		NextKey := RandomFromTo(0, (fDict.Count - 1));
		Inc(Ctrl);
		If Possible[NextKey] Then
			Found := True;
	End;
	// most probable value
	If Found Then
		MostProbable := NextKey
	Else
		MostProbable := -1;
End;

Function TMarkov.TalkFrom(K, N : SmallInt): AnsiString;
Var 
	Ctrl  : SmallInt;
	Temp  : AnsiString;
	Key   : SmallInt;
Begin
	If N <= 3 Then 
		N := 3; // Minimal ammount that makes sense
	Key  := K;
        if Key < 0 then Exit;
	Ctrl := 1;
	Temp := '';
	// Start with the word corresponding to K
	Temp := fDict.Words[Key];
	// while theres not enought words in the buffer
	// and the next key isnt a unaceptable value
	While (Ctrl <= N) And (Key >= 0) Do
	Begin
		// The next word will be the most probable
		Key := MostProbable(Key, RandomFromTo(fThreshold - fErrorMargin, fThreshold + fErrorMargin));
		If Key >= 0 Then
		Begin
			// Append the next word in the phrase
			Temp := Temp + ' ' + fDict.Words[Key];
			fUsed.WordSaid(Key);
			Inc(Ctrl);
		End;
	End;
	TalkFrom := Temp;
End;

Function TMarkov.Talk(W : TStringList): AnsiString;
Begin
	If Assigned(W) Then 
	Begin
		TalkTo(W);
		// Selects a random word on the input list
		// Detects this word position in the word list
		// And feed it as a seed to the phrase,
		// Generating at least the same ammount of words or 
		// at most twice as much
		Talk := TalkFrom(
		fDict.FindWord( // Translate from word to index
		W[RandomFromTo(0, W.Count - 1)] // Grabs a random word in the input
		), 
		W.Count + RandomFromTo(0, W.Count) // Generates at least equal or twice ammount of words
		);
	End;
End;

// TLastWordsUsed

Constructor TLastWordsUsed.Create(Size : Cardinal);
Begin
	Inherited Create;
	SetLength(fBuffer, Size);
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

End.
