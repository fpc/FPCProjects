{
    $Id$

    KCL  -  Kassandra Component Library
    Copyright (C) 1999 - 2000  by the KCL team
      see file AUTHORS in base directory of this distribution

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// ===================================================================
//   Dialog Box Classes
// ===================================================================

// -------------------------------------------------------------------
//   Message and standard dialogs
// -------------------------------------------------------------------

type

  TMsgDlgBtn = (mbYes, mbNo, mbOK, mbCancel, mbApply, mbAbort, mbRetry,
    mbIgnore, mbAll, mbNoToAll, mbYesToAll, mbHelp);
  TMsgDlgButtons = set of TMsgDlgBtn;

resourcestring
  mbText_Yes = 'Yes';
  mbText_No = 'No';
  mbText_Ok = 'Ok';
  mbText_Cancel = 'Cancel';
  mbText_Apply = 'Apply';
  mbText_Abort = 'Abort';
  mbText_Retry = 'Retry';
  mbText_Ignore = 'Ignore';
  mbText_All = 'All';
  mbText_NoToAll = 'No to all';
  mbText_YesToAll = 'Yes to all';
  mbText_Help = 'Help';

  dlgText_FileDialog = 'Select file';

type

  TStandardDialog = class(TCustomForm)
  protected
    FButtons: TMsgDlgButtons;
    FDlgContent: TControl;
    MainLayout, BtnLayout: TBoxLayout;
    // !!!: Separator: TSeparator;
    procedure SetButtons(AButtons: TMsgDlgButtons);
    procedure StdBtnClicked(Sender: TObject);
    procedure OnFinishCreation; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Buttons: TMsgDlgButtons read FButtons write SetButtons default [mbOk, mbCancel];
    property Content: TControl read FDlgContent write FDlgContent;
  end;


// -------------------------------------------------------------------
//   Common dialogs
// -------------------------------------------------------------------

  TFileDialog = class(TWinControl)
  protected
    ResultCode: Boolean;

    FFilename: String;
    procedure OnFinishCreation; override;
  public
    function Run: Boolean;
  published
    property Filename: String read FFilename write FFilename;
  end;


{
  $Log$
  Revision 1.4  2000/03/19 00:29:31  sg
  * Huge shift towards more VCL compatiblity (NOTE: Many widgets/controls
    are not working correctly at the moment!)

  Revision 1.3  2000/02/17 22:21:56  sg
  * Added TCustomDialog
  * Several improvements...

  Revision 1.2  2000/01/05 19:13:30  sg
  * Added "Abort" button for DefButton dialog
  * Added resourcestring for TFileDialog caption

  Revision 1.1.1.1  1999/12/30 21:33:20  sg
  Initial import

}
