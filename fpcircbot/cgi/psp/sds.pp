{$H+}{$MODE OBJFPC}
unit sds;
{
 *******************************************************************************
 *                  -== Simple Data Storage Unit (SDS) ==-                     *
 *******************************************************************************
 * A bit more comfortable than plain text way to store data based on electronic*
 *tables.                                                                      *
 *******************************************************************************
 * See the Pascal Server Pages Documentation for more information.             *
 *******************************************************************************
 *   Written by Vladimir Sibirov a.k.a. Trustmaster                            *
 *   http://www.psp.furtopia.org                                               *
 *   mailto:psp@furtopia.org                                                   *
 *******************************************************************************
 * v.1.3 note: changed file format, rowcount is now defined in file header to  *
 *             optimise NumOfRows() calls,                                     *
 *             using sds_safeline instead of httpenc to make it work a little  *
 *             faster,                                                         *
 *             using dynamic arrays in rows and results,                       *
 *             code optimisations,                                             *
 *             unified table structure: ID is accessible as field[0],          *
 *             frequently called algoritms are stored in dedicated functions   *
 *             to avoid bugs in program logic (there is such a problem).       *
 * v.1.2.2 note: some code optimisations: speed, size and memory usage.        *
 * v.1.2.1 note: all integer variables are defined as longints (to support very*
 *             large tables).                                                  *
 * v.1.2 note: changed file format, last ID is registered in file header for   *
 *             every new ID to be unique.                                      *
 * v.1.1 note: memory optimisation,                                            *
 *             data is HTTP-encoded to prevent from internal errors (but it's  *
 *             a bit slower),                                                  *
 *             fixed some bugs,                                                *
 *             added new functions.                                            *
 *******************************************************************************
 * Copyright (c) 2003-2004 by Pascal Server Pages development team.            *
 * See the Pascal Server Pages License for more information.                   *
 *******************************************************************************
 }

{==============================================================================}
{================================== INTERFACE =================================}
{==============================================================================}

interface
uses substrings, fileshare;

{==============================================================================}
{================================   Types   ===================================}
{==============================================================================}

type SDS_Row = array of string;
    // Type of table row

    SDS_PRow = ^SDS_Row;
    // Pointer to SDS_Row

    SDS_Result = array of SDS_Row;
    // Type for query result in memory

    SDS_PResult = ^SDS_Result;
    // Pointer to SDS_Result
    
    SDS_Header = record
    // SDS table header
        intro: string;
        fields, rows, last_id: longint;
    end;
    
    SDS_PHeader = ^SDS_Header;
    // Pointer to SDS_Header

{==============================================================================}
{========================= Procedures and Functions ===========================}
{==============================================================================}

// Auxilliary functions
// Very common auxilliary functions

function SDS_Encode(str: string): string;
// Replaces CR, LF with special boundaries
 
function SDS_Decode(str: string): string;
// Replaces SDS-boundaries with CR and LF

// Internal functions
// Since SDS 1.3 these simple functions are used in the main algoritms to
// avoid bug occurances.

function SDS_ReadRow(var fh: text; fields: longint): SDS_Row;
// Reads the row from the table file stream into the array

procedure SDS_WriteRow(var fh: text; row: SDS_PRow);
// Implements the array in table file stream

function SDS_ReadHeader(var fh: text): SDS_Header;
// Reads the table header from open file stream

procedure SDS_WriteHeader(var fh: text; hdr: SDS_PHeader);
// Writes the table header into the open file stream

// Public functions
// Read the documentation for the information about these functions

function SDS_Create(from: string; fields: longint; cols: SDS_PRow): boolean;
// Creates a new table

function SDS_Insert(from: string; row: SDS_PRow): longint;
// Inserts a new row into a table

function SDS_MatchedRows(from, where, equals: string): longint;
// Returns number of rows in the table with field 'where' matching 'equals'

function SDS_SelectRow(from, where, equals: string): SDS_Row;
// Selects a row by field value

function SDS_SelectField(field, from, where, equals: string): string;
// Custom field selection

function SDS_SelectAll(from, where, equals: string): SDS_Result;
// Custom multirow select

function SDS_SelectWhole(from: string): SDS_Result;
// Selects whole table into memory

function SDS_ResultRows(res: SDS_PResult): longint;
// Returns number of rows in result

function SDS_FetchRow(res: SDS_PResult; id: longint): SDS_Row;
// Returns a row from result

function SDS_UpdateRow(from, where, equals: string; row: SDS_PRow): longint;
// Updates whole row by its field value

function SDS_UpdateFieldByValue(from, where, equals, setval: string): longint;
// Updates a field by its value

function SDS_UpdateField(from, where, equals, setfield, toval: string): longint;
// Custom field update

function SDS_Delete(from, where, equals: string): longint;
// Deletes a row by its field value

function SDS_NumOfRows(from: string): longint;
// Returns total number of rows in the table

function SDS_GetLastID(from: string): longint;
// Gets last ID in the table

function SDS_NumOfFields(from: string): longint;
// Returns number of fields in the table

function SDS_TotalFields(from: string): longint;
// Returns total number of cells in the table

function SDS_ColumnNames(from: string): SDS_Row;
// Fetches a row containing column names in the table

{==============================================================================}
{================================ IMPLEMENTAION ===============================}
{==============================================================================}

implementation

{--[ SDS_Encode ]--------------------------------------------------------------}
// Replaces CR, LF with special boundaries
 
function SDS_Encode(str: string): string;
begin
    result := str;
    if pos(#13, result) > 0 then
    result := SubstrReplace(result, #13, '#SDS_CHAR_CR#');
    if pos(#10, result) > 0 then
    result := SubstrReplace(result, #10, '#SDS_CHAR_LF#');
end;

{------------------------------------------------------------------------------}
 
{--[ SDS_Decode ]--------------------------------------------------------------}
// Replaces SDS-boundaries with CR and LF

function SDS_Decode(str: string): string;
begin
    result := str;
    if pos('#SDS_CHAR_CR#', result) > 0 then
    result := SubstrReplace(result, '#SDS_CHAR_CR#', #13);
    if pos('#SDS_CHAR_LF#', result) > 0 then
    result := SubstrReplace(result, '#SDS_CHAR_LF#', #10);
end;

{------------------------------------------------------------------------------}

{--[ SDS_ReadRow ]-------------------------------------------------------------}
// Reads the row from the table stream into the array

function SDS_ReadRow(var fh: text; fields: longint): SDS_Row;
var cnt: longint;
begin
    SetLength(result, fields);
    for cnt := 0 to (fields - 1) do
        begin
            readln(fh, result[cnt]);
            result[cnt] := SDS_Decode(result[cnt]);
        end;
end;

{------------------------------------------------------------------------------}
 
{--[ SDS_WriteRow ]------------------------------------------------------------}
// Implements the array in table stream

procedure SDS_WriteRow(var fh: text; row: SDS_PRow);
var cnt: longint;
begin
    for cnt := 0 to (length(row^) - 1) do
        begin
            writeln(fh, SDS_Encode(row^[cnt]));
        end;
end;

{------------------------------------------------------------------------------}
 
{--[ SDS_ReadHeader ]----------------------------------------------------------}
// Reads the table header from open file stream

function SDS_ReadHeader(var fh: text): SDS_Header;
begin
    readln(fh, result.intro);
    readln(fh, result.fields);
    readln(fh, result.last_id);
    readln(fh, result.rows);
    readln(fh);
end;

{------------------------------------------------------------------------------}
 
{--[ SDS_WriteHeader ]---------------------------------------------------------}
// Writes the table header into the open file stream

procedure SDS_WriteHeader(var fh: text; hdr: SDS_PHeader);
begin
    writeln(fh, hdr^.intro);
    writeln(fh, hdr^.fields);
    writeln(fh, hdr^.last_id);
    writeln(fh, hdr^.rows);
    writeln(fh);
end;

{------------------------------------------------------------------------------}

{--[ SDS_Create ]--------------------------------------------------------------}
// Creates a new table

function SDS_Create(from: string; fields: longint; cols: SDS_PRow): boolean;
var fh: text;
    hdr: SDS_Header;
begin
    FSuspend(from);
    FLock(from);
    assign(fh, from);
    rewrite(fh);
    hdr.intro := '[-- Simple Data Storage File --]';
    hdr.fields := fields;
    hdr.last_id := -1;
    hdr.rows := 0;
    SDS_WriteHeader(fh, @hdr);
    SDS_WriteRow(fh, cols);
    close(fh);
    FUnlock(from);
    result := true;
end;

{------------------------------------------------------------------------------}

{--[ SDS_NumOfFields ]---------------------------------------------------------}
// Returns number of fields in the table

function SDS_NumOfFields(from: string): longint;
var fh: text;
    hdr: SDS_Header;
begin
    result := 0;
    if not FExists(from) then exit(0);
    FSuspend(from);
    assign(fh, from);
    reset(fh);
    hdr := SDS_ReadHeader(fh);
    close(fh);
    result := hdr.fields;
end;

{------------------------------------------------------------------------------}

{--[ SDS_GetLastID ]-----------------------------------------------------------}
// Gets last ID in the table

function SDS_GetLastID(from: string): longint;
var fh: text;
    hdr: SDS_Header;
begin
    result := 0;
    if not FExists(from) then exit(0);
    FSuspend(from);
    assign(fh, from);
    reset(fh);
    hdr := SDS_ReadHeader(fh);
    close(fh);
    result := hdr.last_id;
end;

{------------------------------------------------------------------------------}

{--[ SDS_NumOfRows ]-----------------------------------------------------------}
// Returns total number of rows in the table

function SDS_NumOfRows(from: string): longint;
var fh: text;
    hdr: SDS_Header;
begin
    result := 0;
    if not FExists(from) then exit(0);
    FSuspend(from);
    assign(fh, from);
    reset(fh);
    hdr := SDS_ReadHeader(fh);
    close(fh);
    result := hdr.rows;
end;

{------------------------------------------------------------------------------}

{--[ SDS_TotalFields ]---------------------------------------------------------}
// Returns total number of cells in the table

function SDS_TotalFields(from: string): longint;
var fh: text;
    hdr: SDS_Header;
begin
    result := 0;
    if not FExists(from) then exit(0);
    FSuspend(from);
    assign(fh, from);
    reset(fh);
    hdr := SDS_ReadHeader(fh);
    close(fh);
    result := hdr.fields * hdr.rows;
end;

{------------------------------------------------------------------------------}

{--[ SDS_ColumnNames ]---------------------------------------------------------}
// Fetches a row containing column names in the table

function SDS_ColumnNames(from: string): SDS_Row;
var fh: text;
    hdr: SDS_Header;
begin
    if not FExists(from) then exit(nil);
    FSuspend(from);
    assign(fh, from);
    reset(fh);
    hdr := SDS_ReadHeader(fh);
    SetLength(result, hdr.fields);
    result := SDS_ReadRow(fh, hdr.fields);
    close(fh);
end;

{------------------------------------------------------------------------------}

{--[ SDS_Insert ]--------------------------------------------------------------}
// Inserts a new row into a table

function SDS_Insert(from: string; row: SDS_PRow): longint;
var fh, fhn: text;
    buff: string;
    hdr: SDS_Header;
begin
    result := 0;
    if not FExists(from) then exit(0);
    FSuspend(from);
    FLock(from);
    assign(fh, from);
    assign(fhn, from + '.tmp');
    reset(fh);
    rewrite(fhn);
    hdr := SDS_ReadHeader(fh);
    inc(hdr.last_id);
    inc(hdr.rows);
    SDS_WriteHeader(fhn, @hdr);
    while not eof(fh) do
        begin
            readln(fh, buff);
            writeln(fhn, buff);
        end;
    str(hdr.last_id, buff);
    row^[0] := buff;
    SDS_WriteRow(fhn, row);
    inc(result);
    close(fh);
    close(fhn);
    erase(fh);
    rename(fhn, from);
    FUnlock(from);
end;

{------------------------------------------------------------------------------}

{--[ SDS_MatchedRows ]---------------------------------------------------------}
// Returns number of rows in the table with field 'where' matching 'equals'
function SDS_MatchedRows(from, where, equals: string): longint;
var fh: text;
    field, cnt: longint;
    row: SDS_Row;
    hdr: SDS_Header;
begin
    result := 0;
    if not FExists(from) then exit(0);
    FSuspend(from);
    assign(fh, from);
    reset(fh);
    hdr := SDS_ReadHeader(fh);
    SetLength(row, hdr.fields);
    row := SDS_ReadRow(fh, hdr.fields);
    for cnt := 0 to (hdr.fields - 1) do
        if row[cnt] = where then field := cnt;
    while not eof(fh) do
        begin
            row := SDS_ReadRow(fh, hdr.fields);
            if row[field] = equals then inc(result);
        end;
    close(fh);
end;

{------------------------------------------------------------------------------}

{--[ SDS_SelectRow ]-----------------------------------------------------------}
// Selects a row by field value

function SDS_SelectRow(from, where, equals: string): SDS_Row;
var fh: text;
    field, cnt: longint;
    row: SDS_Row;
    hdr: SDS_Header;
begin
    if not FExists(from) then exit(nil);
    FSuspend(from);
    assign(fh, from);
    reset(fh);
    hdr := SDS_ReadHeader(fh);
    SetLength(row, hdr.fields);
    SetLength(result, hdr.fields);
    row := SDS_ReadRow(fh, hdr.fields);
    for cnt := 0 to (hdr.fields - 1) do
        if row[cnt] = where then field := cnt;
    while not eof(fh) do
        begin
            row := SDS_ReadRow(fh, hdr.fields);
            if row[field] = equals then
                begin
                    result := row;
                    break;
                end;
        end;
    close(fh);
end;

{------------------------------------------------------------------------------}

{--[ SDS_SelectField ]---------------------------------------------------------}
// Custom field selection

function SDS_SelectField(field, from, where, equals: string): string;
var fh: text;
    cnt, col, whr: longint;
    row: SDS_Row;
    hdr: SDS_Header;
begin
    if not FExists(from) then exit('');
    FSuspend(from);
    assign(fh, from);
    reset(fh);
    hdr := SDS_ReadHeader(fh);
    SetLength(row, hdr.fields);
    row := SDS_ReadRow(fh, hdr.fields);
    for cnt := 0 to (hdr.fields - 1) do
        begin
            if row[cnt] = field then col := cnt;
            if row[cnt] = where then whr := cnt;
        end;
    while not eof(fh) do
        begin
            row := SDS_ReadRow(fh, hdr.fields);
            if row[whr] = equals then
                begin
                    result := row[col];
                    break;
                end;
        end;
    close(fh);
end;

{------------------------------------------------------------------------------}

{--[ SDS_UpdateRowByField ]----------------------------------------------------}
// Updates whole row by its field value

function SDS_UpdateRow(from, where, equals: string; row: SDS_PRow): longint;
var fh, fhn: text;
    cnt, field: longint;
    tmprow: SDS_Row;
    hdr: SDS_Header;
begin
    if not FExists(from) then exit(0);
    FSuspend(from);
    FLock(from);
    result := 0;
    assign(fh, from);
    assign(fhn, from + '.tmp');
    reset(fh);
    rewrite(fhn);
    hdr := SDS_ReadHeader(fh);
    SDS_WriteHeader(fhn, @hdr);
    SetLength(tmprow, hdr.fields);
    tmprow := SDS_ReadRow(fh, hdr.fields);
    for cnt := 0 to (hdr.fields - 1) do
        if tmprow[cnt] = where then field := cnt;
    SDS_WriteRow(fhn, @tmprow);
    while not eof(fh) do
        begin
            tmprow := SDS_ReadRow(fh, hdr.fields);
            if tmprow[field] = equals then
                begin
                    row^[0] := tmprow[0];
                    SDS_WriteRow(fhn, row);
                    inc(result);
                end
            else SDS_WriteRow(fhn, @tmprow);
        end;
    close(fh);
    close(fhn);
    erase(fh);
    rename(fhn, from);
    FUnlock(from);
end;

{------------------------------------------------------------------------------}

{--[ SDS_UpdateFieldByValue ]--------------------------------------------------}
// Updates a field by its value

function SDS_UpdateFieldByValue(from, where, equals, setval: string): longint;
var fh, fhn: text;
    cnt, field: longint;
    row: SDS_Row;
    hdr: SDS_Header;
begin
    result := 0;
    if not FExists(from) then exit(0);
    FSuspend(from);
    FLock(from);
    assign(fh, from);
    assign(fhn, from + '.tmp');
    reset(fh);
    rewrite(fhn);
    hdr := SDS_ReadHeader(fh);
    SDS_WriteHeader(fhn, @hdr);
    SetLength(row, hdr.fields);
    row := SDS_ReadRow(fh, hdr.fields);
    for cnt := 0 to (hdr.fields - 1) do
        if row[cnt] = where then field := cnt;
    SDS_WriteRow(fhn, @row);
    while not eof(fh) do
        begin
            row := SDS_ReadRow(fh, hdr.fields);
            if row[field] = equals then
                begin
                    row[field] := setval;
                    inc(result);
                end;
            SDS_WriteRow(fhn, @row);
        end;
    close(fh);
    close(fhn);
    erase(fh);
    rename(fhn, from);
    FUnlock(from);
end;

{------------------------------------------------------------------------------}

{--[ SDS_UpdateField ]---------------------------------------------------------}
// Custom field update

function SDS_UpdateField(from, where, equals, setfield, toval: string): longint;
var fh, fhn: text;
    cnt, field, another: longint;
    row: SDS_Row;
    hdr: SDS_Header;
begin
    result := 0;
    if not FExists(from) then exit(0);
    FSuspend(from);
    FLock(from);
    assign(fh, from);
    assign(fhn, from + '.tmp');
    reset(fh);
    rewrite(fhn);
    hdr := SDS_ReadHeader(fh);
    SDS_WriteHeader(fhn, @hdr);
    SetLength(row, hdr.fields);
    row := SDS_ReadRow(fh, hdr.fields);
    for cnt := 0 to (hdr.fields - 1) do
        begin
            if row[cnt] = where then another := cnt;
            if row[cnt] = setfield then field := cnt;
        end;
    SDS_WriteRow(fhn, @row);
    while not eof(fh) do
        begin
            row := SDS_ReadRow(fh, hdr.fields);
            if row[another] = equals then
                begin
                    row[field] := toval;
                    inc(result);
                end;
            SDS_WriteRow(fhn, @row);
        end;
    close(fh);
    close(fhn);
    erase(fh);
    rename(fhn, from);
    FUnlock(from);
end;

{------------------------------------------------------------------------------}

{--[ SDS_Delete ]--------------------------------------------------------------}
// Deletes a row by its field value

function SDS_Delete(from, where, equals: string): longint;
var fh, fhn: text;
    cnt, field, matches: longint;
    row: SDS_Row;
    hdr: SDS_Header;
begin
    result := 0;
    if not FExists(from) then exit(0);
    FSuspend(from);
    matches := SDS_MatchedRows(from, where, equals);
    FLock(from);
    assign(fh, from);
    assign(fhn, from + '.tmp');
    reset(fh);
    rewrite(fhn);
    hdr := SDS_ReadHeader(fh);
    SetLength(row, hdr.fields);
    hdr.rows := hdr.rows - matches;
    SDS_WriteHeader(fhn, @hdr);
    row := SDS_ReadRow(fh, hdr.fields);
    for cnt := 0 to (hdr.fields - 1) do
        if row[cnt] = where then field := cnt;
    SDS_WriteRow(fhn, @row);
    while not eof(fh) do
        begin
            row := SDS_ReadRow(fh, hdr.fields);
            if row[field] = equals then inc(result)
            else SDS_WriteRow(fhn, @row);
        end;
    close(fh);
    close(fhn);
    erase(fh);
    rename(fhn, from);
    FUnlock(from);
end;

{------------------------------------------------------------------------------}

{--[ SDS_SelectAll ]-----------------------------------------------------------}
// Custom multirow select

function SDS_SelectAll(from, where, equals: string): SDS_Result;
var fh: text;
    cnt, rows, field: longint;
    row: SDS_Row;
    hdr: SDS_Header;
begin
    if not FExists(from) then exit(nil);
    FSuspend(from);
    SetLength(result, 1);
    assign(fh, from);
    reset(fh);
    hdr := SDS_ReadHeader(fh);
    SetLength(row, hdr.fields);
    row := SDS_ReadRow(fh, hdr.fields);
    for cnt := 0 to (hdr.fields - 1) do
        if row[cnt] = where then field := cnt;
    result[0] := row;
    rows := 1;
    while not eof(fh) do
        begin
            row := SDS_ReadRow(fh, hdr.fields);
            if row[field] = equals then
                begin
                    SetLength(result, length(result) + 1);
                    result[rows] := row;
                    inc(rows);
                end;
        end;
    close(fh);
end;

{------------------------------------------------------------------------------}

{--[ SDS_SelectWhole ]---------------------------------------------------------}
// Selects whole table into memory

function SDS_SelectWhole(from: string): SDS_Result;
var fh: text;
    cnt: longint;
    row: SDS_Row;
    hdr: SDS_Header;
begin
    if not FExists(from) then exit(nil);
    FSuspend(from);
    assign(fh, from);
    reset(fh);
    hdr := SDS_ReadHeader(fh);
    SetLength(row, hdr.fields);
    SetLength(result, hdr.rows);
    for cnt := 0 to (hdr.fields - 1) do
        begin
            row := SDS_ReadRow(fh, hdr.fields);
            result[cnt] := row;
        end;
    close(fh);
end;

{------------------------------------------------------------------------------}

{--[ SDS_ResultRows ]----------------------------------------------------------}
// Returns number of rows in result

function SDS_ResultRows(res: SDS_PResult): longint;
begin
    result := length(res^) - 1;
end;

{------------------------------------------------------------------------------}

{--[ SDS_FetchRow ]------------------------------------------------------------}
// Returns a row from result

function SDS_FetchRow(res: SDS_PResult; id: longint): SDS_Row;
begin
    if id <= SDS_ResultRows(res) then result := res^[id];
end;

{==============================================================================}
{===================================== END ====================================}
{==============================================================================}

end.