{$IFDEF FPC}{$H+}{$MODE OBJFPC}{$R+}{$Q+}{$CHECKPOINTER ON}{$ENDIF}
unit regexp;
{
 *******************************************************************************
 *              -== Pascal Server Pages unit - Regular Expressions ==-         *
 *******************************************************************************
 * Perl Compatible Regular Expressions unit, Pascal implementation.            *
 * Simplified functional oriented API for TRegExpr class written by Andrey V.  *
 * Sorokin.                                                                    *
 *******************************************************************************
 * See the Pascal Server Pages Documentation for more information.             *
 *******************************************************************************
 *   Written by Vladimir Sibirov a.k.a. Trustmaster                            *
 *   http://www.psp.furtopia.org                                               *
 *   mailto:psp@furtopia.org                                                   *
 *******************************************************************************
 * Partial Copyright (c) 1999-2004 Andrey V. Sorokin.                          *
 *   http://RegExpStudio.com                                                   *
 *   mailto:anso@mail.ru                                                       *
 * See aux_regexpr.pp head for detailed TRegExpr licence information.          *
 *******************************************************************************
 * Copyright (c) 2003-2005 by Pascal Server Pages development team.            *
 * See the Pascal Server Pages License for more information.                   *
 *******************************************************************************
 * [PSP 1.4.0 - 17.08.05 - Trustmaster]:                                       *
 * - finished with first full implementation of this unit.                     *
 *******************************************************************************
}

{==============================================================================}
{================================== INTERFACE =================================}
{==============================================================================}

interface
uses regexp_tregexpr, substrings;

{==============================================================================}
{================================   Types   ===================================}
{==============================================================================}

type RegexpEntry = Pointer;
    // RegExp entry struct pointer

    RegexpResult = Pointer;
    // Global match result pointer

    StrArray = array of string;

{==============================================================================}
{========================= Procedures and Functions ===========================}
{==============================================================================}

function regexp_check(const pattern, str: string): boolean;
// Checks if str matches RegExp pattern

function regexp_count(const pattern, str: string): longword;
// Returns number of full-mask RegExp entries in str

function regexp_count_all(rr: RegexpResult): longword;
// Returns number of entries dumped by regexp_match_all

function regexp_entry(rr: RegexpResult; index: longword): RegexpEntry;
// Fetches single global match result entry

function regexp_entry_count(re: RegexpEntry): longint;
// Returns number of submasks in the entry

function regexp_entry_item(re: RegexpEntry; index: longword): string;
// Returns RegExp entry item (submask)

function regexp_entry_length(re: RegexpEntry; index: longword): longword;
// Returns length of RegExp entry item (submask)

function regexp_entry_pos(re: RegexpEntry; index: longword): longword;
// Returns position of RegExp entry item (submask) in source string

function regexp_error(re: RegexpEntry): string;
// Returns error message caused by regexp_match

function regexp_error_all(rr: RegexpResult): string;
// Returns error message caused by regexp_match_all

procedure regexp_free(re: RegexpEntry);
// Frees memory occupied by RegExp entry

procedure regexp_free_all(rr: RegexpResult);
// Frees memory occupied by RegExp global match result

function regexp_match(const pattern, str: string; var matches: RegexpEntry): boolean;
// Custom regular expression match

function regexp_match_all(const pattern, str: string; var matches: RegexpResult): longword;
// Performs global regular expression match, number of full-mask entries is returned

function regexp_replace(const pattern, replacement, str: string): string;
// Replaces data in string by RegExp pattern.

procedure regexp_split(const pattern, str: string; var pieces: StrArray);
// Splits a string into array by delimiter defined by RegExp pattern.


{==============================================================================}
{================================ IMPLEMENTAION ===============================}
{==============================================================================}

implementation

{==============================================================================}
{================================   Types   ===================================}
{==============================================================================}

type SizeArray = array of longword;
    // Dynamic array of longword.
    
    TRegexpEntry = record
    // Struct that implements single regular expression entry
        count: longint; // Number of subentries
        items: StrArray; // Array containing all the entries
        posns: SizeArray; // Array containing positions for each subentry
        lents: SizeArray; // Array containing lengths for each subentry
        error: string; // Last error message or empty string
    end;

    PTRegexpEntry = ^TRegexpEntry;
    // Pointer to RegexpEntry

    TRegexpResult = record
    // Global match result
        count: longword; // Number of entries
        entries: array of PTRegexpEntry; // Entries
        error: string; // Error message
    end;

    PTRegexpResult = ^TRegexpResult;
    // Pointer to RegexpResult

{==============================================================================}
{========================= Procedures and Functions ===========================}
{==============================================================================}

{--[ regexp_check ]------------------------------------------------------------}
// Checks if str matches RegExp pattern

function regexp_check(const pattern, str: string): boolean;
var rh: TRegExpr;
    lim_rpos: longword;
    pat_lim: string;
begin
    // Init
    result := false;
    // Determine pattern boundaries
    pat_lim := copy(pattern, 1, 1);
    lim_rpos := substr_rpos(pattern, pat_lim);
    // Building
    rh := TRegExpr.Create;
    // Split pattern and modifiers
    rh.Expression := copy(pattern, 2, lim_rpos - 2);
    rh.ModifierStr := copy(pattern, lim_rpos + 1, longword(length(pattern)) - lim_rpos);
    // Execution
    result := rh.Exec(str);
    // Finalization
    rh.Free;
end;

{------------------------------------------------------------------------------}

{--[ regexp_count ]------------------------------------------------------------}
// Returns number of full-mask RegExp entries in str

function regexp_count(const pattern, str: string): longword;
var rh: TRegExpr;
    lim_rpos: longword;
    pat_lim: string;
begin
    // Init
    result := 0;
    // Determine pattern boundaries
    pat_lim := copy(pattern, 1, 1);
    lim_rpos := substr_rpos(pattern, pat_lim);
    // Building
    rh := TRegExpr.Create;
    // Split pattern and modifiers
    rh.Expression := copy(pattern, 2, lim_rpos - 2);
    rh.ModifierStr := copy(pattern, lim_rpos + 1, longword(length(pattern)) - lim_rpos);
    // Execution
    if rh.Exec(str) then
        repeat
            inc(result);
        until not rh.ExecNext;
    // Finalization
    rh.Free;
end;

{------------------------------------------------------------------------------}

{--[ regexp_count_all ]--------------------------------------------------------}
// Returns number of entries dumped by regexp_match_all

function regexp_count_all(rr: RegexpResult): longword;
var rh: PTRegexpResult;
begin
    rh := PTRegexpResult(rr);
    result := rh^.count;
end;

{------------------------------------------------------------------------------}


{--[ regexp_entry ]------------------------------------------------------------}
// Fetches global match result entry

function regexp_entry(rr: RegexpResult; index: longword): RegexpEntry;
var rh: PTRegexpResult;
begin
    rh := PTRegexpResult(rr);
    result := RegexpEntry(rh^.entries[index]);
end;

{------------------------------------------------------------------------------}

{--[ regexp_entry_count ]------------------------------------------------------}
// Returns number of submasks in the entry

function regexp_entry_count(re: RegexpEntry): longint;
var mh: PTRegexpEntry;
begin
    mh := PTRegexpEntry(re);
    result := mh^.count;
end;

{------------------------------------------------------------------------------}

{--[ regexp_entry_item ]-------------------------------------------------------}
// Returns RegExp entry item (submask)

function regexp_entry_item(re: RegexpEntry; index: longword): string;
var mh: PTRegexpEntry;
begin
    mh := PTRegexpEntry(re);
    result := mh^.items[index];
end;

{------------------------------------------------------------------------------}

{--[ regexp_entry_length ]-----------------------------------------------------}
// Returns length of RegExp entry item (submask)

function regexp_entry_length(re: RegexpEntry; index: longword): longword;
var mh: PTRegexpEntry;
begin
    mh := PTRegexpEntry(re);
    result := mh^.lents[index];
end;

{------------------------------------------------------------------------------}

{--[ regexp_entry_pos ]--------------------------------------------------------}
// Returns position of RegExp entry item (submask) in source string

function regexp_entry_pos(re: RegexpEntry; index: longword): longword;
var mh: PTRegexpEntry;
begin
    mh := PTRegexpEntry(re);
    result := mh^.posns[index];
end;

{------------------------------------------------------------------------------}

{--[ regexp_error ]------------------------------------------------------------}
// Returns error message caused by regexp_match

function regexp_error(re: RegexpEntry): string;
var mh: PTRegexpEntry;
begin
    mh := PTRegexpEntry(re);
    result := mh^.error;
end;

{------------------------------------------------------------------------------}

{--[ regexp_error_all ]--------------------------------------------------------}
// Returns error message caused by regexp_match_all

function regexp_error_all(rr: RegexpResult): string;
var rh: PTRegexpResult;
begin
    rh := PTRegexpResult(rr);
    result := rh^.error;
end;

{------------------------------------------------------------------------------}

{--[ regexp_free ]-------------------------------------------------------------}
// Frees memory occupied by RegExp entry

procedure regexp_free(re: RegexpEntry);
var mh: PTRegexpEntry;
begin
    mh := PTRegexpEntry(re);
    dispose(mh);
end;

{------------------------------------------------------------------------------}

{--[ regexp_free_all ]---------------------------------------------------------}
// Frees memory occupied by RegExp global match result

procedure regexp_free_all(rr: RegexpResult);
var rh: PTRegexpResult;
    i: longword;
begin
    rh := PTRegexpResult(rr);
    if rh^.count > 0 then for i := 0 to rh^.count - 1 do dispose(rh^.entries[i]);
    dispose(rh);
end;

{------------------------------------------------------------------------------}

{--[ regexp_match ]------------------------------------------------------------}
// Custom regular expression match

function regexp_match(const pattern, str: string; var matches: RegexpEntry): boolean;
var rh: TRegExpr;
    pat_lim: string;
    lim_rpos, i: longword;
    mh: PTRegexpEntry;
begin
    // Init
    result := false;
    new(mh);
    // Determine pattern boundaries
    pat_lim := copy(pattern, 1, 1);
    lim_rpos := substr_rpos(pattern, pat_lim);
    // Building
    rh := TRegExpr.Create;
    // Split pattern and modifiers
    rh.Expression := copy(pattern, 2, lim_rpos - 2);
    rh.ModifierStr := copy(pattern, lim_rpos + 1, longword(length(pattern)) - lim_rpos);
    // Execution
    result := rh.Exec(str);
    // Dumping match
    if result then
        begin
            SetLength(mh^.items, rh.SubExprMatchCount + 1);
            SetLength(mh^.posns, rh.SubExprMatchCount + 1);
            SetLength(mh^.lents, rh.SubExprMatchCount + 1);
            mh^.count := rh.SubExprMatchCount;
            for i := 0 to mh^.count do
                begin
                    mh^.items[i] := rh.Match[i];
                    mh^.posns[i] := rh.MatchPos[i];
                    mh^.lents[i] := rh.MatchLen[i];
                end;
        end
    else
        begin
            mh^.count := -1;
            mh^.error := rh.ErrorMsg(rh.LastError);
        end;
    // Finalization
    rh.Free;
    // Dump result
    matches := RegexpEntry(mh);
end;

{------------------------------------------------------------------------------}

{--[ regexp_match_all ]--------------------------------------------------------}
// Performs global regular expression match, number of full-mask entries is returned

function regexp_match_all(const pattern, str: string; var matches: RegexpResult): longword;
var rh: TRegExpr;
    rp: PTRegexpResult;
    pat_lim: string;
    lim_rpos, i: longword;
begin
    // Initialization
    result := 0;
    new(rp);
    rp^.count := 0;
    // Determine pattern boundaries
    pat_lim := copy(pattern, 1, 1);
    lim_rpos := substr_rpos(pattern, pat_lim);
    // Building
    rh := TRegExpr.Create;
    // Split pattern and modifiers
    rh.Expression := copy(pattern, 2, lim_rpos - 2);
    rh.ModifierStr := copy(pattern, lim_rpos + 1, longword(length(pattern)) - lim_rpos);
    // Execution
    if rh.Exec(str) then
        repeat
            inc(result);
            inc(rp^.count);
            SetLength(rp^.entries, rp^.count);
            new(rp^.entries[rp^.count - 1]);
            SetLength(rp^.entries[rp^.count - 1]^.items, rh.SubExprMatchCount + 1);
            SetLength(rp^.entries[rp^.count - 1]^.posns, rh.SubExprMatchCount + 1);
            SetLength(rp^.entries[rp^.count - 1]^.lents, rh.SubExprMatchCount + 1);
            rp^.entries[rp^.count - 1]^.count := rh.SubExprMatchCount;
            for i := 0 to rp^.entries[rp^.count - 1]^.count do
                begin
                    rp^.entries[rp^.count - 1]^.items[i] := rh.Match[i];
                    rp^.entries[rp^.count - 1]^.posns[i] := rh.MatchPos[i];
                    rp^.entries[rp^.count - 1]^.lents[i] := rh.MatchLen[i];
                end;
        until not rh.ExecNext
    else rp^.error := rh.ErrorMsg(rh.LastError);
    // Finalization
    rh.Free;
    // Dump result
    matches := RegexpResult(rp);
end;

{------------------------------------------------------------------------------}

{--[ regexp_replace ]----------------------------------------------------------}
// Replaces data in string by RegExp pattern

function regexp_replace(const pattern, replacement, str: string): string;
var rh: TRegExpr;
    pat_lim: string;
    lim_rpos: longword;
begin
    // Initialization
    result := str;
    // Determine pattern boundaries
    pat_lim := copy(pattern, 1, 1);
    lim_rpos := substr_rpos(pattern, pat_lim);
    // Building
    rh := TRegExpr.Create;
    // Split pattern and modifiers
    rh.Expression := copy(pattern, 2, lim_rpos - 2);
    rh.ModifierStr := copy(pattern, lim_rpos + 1, longword(length(pattern)) - lim_rpos);
    // Execution
    result := rh.Replace(str, replacement, true);
    // Finalization
    rh.Free;
end;

{------------------------------------------------------------------------------}

{--[ regexp_split ]------------------------------------------------------------}
// Splits a string into array by delimiter defined by RegExp pattern

procedure regexp_split(const pattern, str: string; var pieces: StrArray);
var rh: TRegExpr;
    pat_lim: string;
    lim_rpos: longword;
begin
    // Determine pattern boundaries
    pat_lim := copy(pattern, 1, 1);
    lim_rpos := substr_rpos(pattern, pat_lim);
    // Building
    rh := TRegExpr.Create;
    // Split pattern and modifiers
    rh.Expression := copy(pattern, 2, lim_rpos - 2);
    rh.ModifierStr := copy(pattern, lim_rpos + 1, longword(length(pattern)) - lim_rpos);
    // Execution
    rh.Split(str, pieces);
    // Finalization
    rh.Free;
end;

{------------------------------------------------------------------------------}


{==============================================================================}
{===================================== END ====================================}
{==============================================================================}

end.
