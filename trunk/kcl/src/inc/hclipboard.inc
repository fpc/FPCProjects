{
    $Id$

    KCL  -  Kassandra Component Library
    Copyright (C) 1999  by the KCL team
      see file AUTHORS in base directory of this distribution

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// ===================================================================
//   Clipboard Support
// ===================================================================

type

{$IFDEF KCL_USEINTERNALCLIPBOARD}

  PClipboardEntry = ^TClipboardEntry;
  TClipboardEntry = record
    Next: PClipboardEntry;
    Format: PChar;
    DataSize: LongWord;
    Data: Pointer;
  end;

  TClipboard_private = record
    IsOpen: Boolean;
    NumFormats: Integer;
    FirstEntry, LastEntry: PClipboardEntry;
  end;

{$ENDIF KCL_USEINTERNALCLIPBOARD}


  TClipboard = class
  protected
    priv: TClipboard_private;
    function  GetFormatCount: Integer;
    function  GetFormat(Index: Integer): String;
    function  GetAsText: String;
    procedure SetAsText(AText: String);
  public
    procedure Open;
    procedure Close;
    procedure Clear;
    procedure SetData(MimeFormat: String; const Data; Size: LongWord);
    function  GetDataSize(MimeFormat: String): LongWord;
    procedure GetData(MimeFormat: String; var Data; MaxSize: LongWord);

    // Persistence
    procedure SetComponent(Component: TComponent);
    function GetComponent(AOwner, AParent: TComponent): TComponent;

    // Text handling
    procedure SetTextBuf(Buffer: PChar);
    property  AsText: String read GetAsText write SetAsText;

    // Format handling
    function  HasFormat(MimeFormat: String): Boolean;
    property  FormatCount: Integer read GetFormatCount;
    property  Formats[Index: Integer]: String read GetFormat;
  end;


function Clipboard: TClipboard;
// function SetClipboard(NewClipboard: TClipboard): TClipboard;


{
  $Log$
  Revision 1.1  1999/12/30 21:33:22  sg
  Initial revision

}
