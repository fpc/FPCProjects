�
 THTMLNEWSFORM 0"  TPF0THtmlNewsFormHtmlNewsFormLeftITop�Width3Height�Caption"Html News - http://www.overbyte.beColor	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style OldCreateOrder	OnClose	FormCloseOnCreate
FormCreateOnShowFormShowPixelsPerInch`
TextHeight TPanel
ToolsPanelLeft Top Width+HeightYAlignalTopTabOrder  TLabelLabel1LeftTopWidthHeightCaptionServer  TLabelLabel4LeftTop"WidthHeightCaptionGroup  TLabelLabel2LeftTop<Width$HeightCaptionSubject  TLabelLabel3Left� TopWidthHeightCaptionPort  TLabelLabel5Left� Top$WidthHeightCaptionFrom  TEdit
ServerEditLeft0TopWidth� HeightHintEnter the NNTP server host nameParentShowHintShowHint	TabOrder Text
ServerEdit  TEdit	GroupEditLeft0Top Width� HeightHint?Enter the newsgroup name such as borland.public.delphi.internetParentShowHintShowHint	TabOrderText	GroupEdit  TButton
PostButtonLeftXTopWidthKHeightHint&Post a hard coded article to the groupCaption&PostParentShowHintShowHint	TabOrderOnClickPostButtonClick  TEditSubjectEditLeft0Top8WidthqHeightTabOrderTextSubjectEdit  TEditPortEditLeft TopWidthUHeightTabOrderTextPortEdit  TEditFromEditLeft Top Width� HeightTabOrderTextFromEdit  TButtonButton1Left�Top$WidthKHeightCaptionButton1TabOrderOnClickButton1Click   TMemoDisplayMemoLeft Top2Width+HeighthAlignalBottomFont.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameCourier New
Font.Style Lines.StringsDisplayMemo 
ParentFont
ScrollBarsssBothTabOrder  TPanelPanel1Left TopYWidth� Height� AlignalLeftTabOrder TMemoPlainTextMemoLeftTopWidth� Height� Hint'Enter the plain text message text here.AlignalClientFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameCourier New
Font.Style Lines.StringsPlainTextMemo 
ParentFontParentShowHint
ScrollBarsssBothShowHint	TabOrder   TMemoImageFilesMemoLeftTop� Width� HeightPHint1Enter the list of image files here, one per line.AlignalBottomFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameCourier New
Font.Style Lines.StringsImageFilesMemo 
ParentFontParentShowHint
ScrollBarsssBothShowHint	TabOrder   TMemoHtmlTextMemoLeft� TopYWidth1Height� HintaEnter the HTML text for the message. Special tags <#IMAGEn> will be replaced by image references.AlignalClientFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameCourier New
Font.Style Lines.StringsHtmlTextMemo 
ParentFontParentShowHint
ScrollBarsssBothShowHint	TabOrderWordWrap  THtmlNntpCliHtmlNntpClientPortnntp	LineLimit   OnSessionConnectedHtmlNntpClientSessionConnectedOnRequestDoneHtmlNntpClientRequestDone	OnDisplayHtmlNntpClientDisplayContentTypenntpHtmlLeft8Top�    