Uses Markov;

Var
	Tmp : String;
Begin
	InitDict;
	ReadLn(Tmp);
	While Tmp <> 'quit' Do
	Begin
		WriteLn(Talk(Tmp));
		ReadLn(Tmp);
	End;
	DoneDict;
End.