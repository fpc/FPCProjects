{;                  Copyright 1991 TechnoJock Software, Inc.
;                             All Rights Reserved
;                            Restricted by License

;                                 FastTTT5.ASM

;                                  Version 5.1

%Title "TTT Toolkit screen writing and memory moving routines"

Conversion to Pascal/FPC: Copyright (c) 2000 Jonas Maebe

}
{$ifdef go32v2}
procedure AsmWrite(scrptr: pointer; Wid,Col,Row,Attr:byte; St:String);
var
  c, offset, attrShl8: word;
  data: array[1..255] of word;
begin
  if length(st) = 0 then
    exit;
  offset := (pred(row)*wid+pred(col))*2;
  attrShl8 := attr shl 8;
  for c := 1 to length(st) do
    data[c] := ord(st[c]) or attrShl8;
  if (longint(ScrPtr) and $ffff0000) = $ffff0000 then
    dosmemput(longint(scrptr) and $ffff, offset, data, length(st)*2)
  else
    move(data,scrptr^,length(st)*2);
end;

procedure AsmPWrite(scrptr: pointer; Wid,Col,Row:byte; St:String);
type
  tscrdata = record
    case byte of
      0: (w: array[1..255] of word);
      1: (b: array[1..255*2] of byte);
  end;
var
  c, offset: word;
  data: tscrdata;
begin
  if length(st) = 0 then
    exit;
  offset := (pred(row)*wid+pred(col))*2;
  if (longint(ScrPtr) and $ffff0000) = $ffff0000 then
    begin
      dosmemget(longint(scrptr) and $ffff, offset, data, length(st)*2);
      for c := 1 to length(st) do
        data.b[c*2-1] := ord(st[c]);
      dosmemput(longint(scrptr) and $ffff, offset, data, length(st)*2)
    end
  else
    for c := 1 to length(st) do
      tscrdata(scrptr^).b[offset+c*2-1] := ord(st[c]);
end;

procedure AsmAttr(scrptr: pointer; Wid,Col,Row,Attr,Len:byte);
type
  tscrdata = record
    case byte of
      0: (w: array[1..255] of word);
      1: (b: array[1..255*2] of byte);
  end;
var
  c, offset: word;
  data: tscrdata;
begin
  if len = 0 then
    exit;
  offset := (pred(row)*wid+pred(col))*2;
  if (longint(ScrPtr) and $ffff0000) = $ffff0000 then
    begin
      dosmemget(longint(scrptr) and $ffff, offset, data, len*2);
      for c := 1 to len do
        data.b[c*2] := Attr;
      dosmemput(longint(scrptr) and $ffff, offset, data, len*2)
    end
  else
    for c := 1 to len do
      tscrdata(scrptr^).b[offset+c*2] := attr;
end;

{$else go32v2}

procedure AsmWrite(scrptr: pointer; Wid,Col,Row,Attr:byte; St:String);
var
  c, offset, attrShl8: word;
  data: array[1..255] of word;
begin
  if length(st) = 0 then
    exit;
  offset := (pred(row)*wid+pred(col))*2;
  attrShl8 := attr shl 8;
  for c := 1 to length(st) do
    data[c] := ord(st[c]) or attrShl8;
  move(data,scrptr^,length(st)*2);
  updatescreen(false);
end;

procedure AsmPWrite(scrptr: pointer; Wid,Col,Row:byte; St:String);
type
  tscrdata = record
    case byte of
      0: (w: array[1..255] of word);
      1: (b: array[1..255*2] of byte);
  end;
var
  c, offset: word;
  data: tscrdata;
begin
  if length(st) = 0 then
    exit;
  offset := (pred(row)*wid+pred(col))*2;
  for c := 1 to length(st) do
    tscrdata(scrptr^).b[offset+c*2-1] := ord(st[c]);
  updatescreen(false);
end;

procedure AsmAttr(scrptr: pointer; Wid,Col,Row,Attr,Len:byte);
type
  tscrdata = record
    case byte of
      0: (w: array[1..255] of word);
      1: (b: array[1..255*2] of byte);
  end;
var
  c, offset: word;
  data: tscrdata;
begin
  if len = 0 then
    exit;
  offset := (pred(row)*wid+pred(col))*2;
  for c := 1 to len do
    tscrdata(scrptr^).b[offset+c*2] := attr;
  updatescreen(false);
end;
{$endif go32v2}