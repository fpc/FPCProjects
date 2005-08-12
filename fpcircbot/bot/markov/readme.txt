Hidden Markov Text Generator 
Written by J. Aldo G. de Freitas Junior - Aka : Pepe Le Pew (c) 2005 
Released under GPL 

This is just a small markov based text generation engine...

Usage :

	{...}
	Uses Markov;
	{...}
	Var
		MyMarkov : TMarkov;
	{...}
	MyMarkov := TMarkov.Create('dict.txt', 'phrase.txt', 60, 40);
	{...}
	WriteLn(MyMarkov.Talk(--TStringList--));
	{...}
	MyMarkov.Destroy;
	{...}
	
* Make sure you create the text files before using TMarkov or you will receive a nice
expection. 
* Do not use too high values for threshold or the engine will generate very short text
* You will need to convert from a AnsiString to a TStringList, 

And be happy :)