�
 THTMLMAILFORM 0�  TPF0THtmlMailFormHtmlMailFormLeft� Top�WidthxHeight�Caption(Html Mail - ICS - http://www.overbyte.beColor	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style OldCreateOrder	OnClose	FormCloseOnCreate
FormCreateOnShowFormShowPixelsPerInch`
TextHeight TPanel
ToolsPanelLeft Top WidthpHeightuAlignalTopTabOrder  TLabelLabel1LeftTopWidth7HeightCaption	SMTP Host  TLabelLabel4Left� TopWidthHeightCaptionPort  TLabelLabel2Left6Top$WidthHeightCaptionFrom  TLabelLabel3Left� Top$WidthHeightCaptionTo  TLabelLabel12Left@Top<WidthHeightCaptionCc  TLabelLabel13Left� Top<WidthHeightCaptionBcc  TLabelSubjectLeft)TopRWidth$HeightCaptionSubject  TLabelLabel8Left� TopTWidthHeightCaptionSign  TEditHostEditLeftPTopWidthyHeightHint"Mail server hostname or IP addressParentShowHintShowHint	TabOrder TextHostEdit  TEditPortEditLeft� TopWidthyHeightHint!Mail server port (should be smtp)ParentShowHintShowHint	TabOrderTextPortEdit  TEditFromEditLeftPTop WidthyHeightHintAuthor's EMailParentShowHintShowHint	TabOrderTextFromEdit  TEditToEditLeft� Top WidthyHeightHint$Destinators, delimited by semicolonsParentShowHintShowHint	TabOrderTextToEdit  TEditCcEditLeftPTop8WidthyHeightParentShowHintShowHint	TabOrderTextCcEdit  TEditBccEditLeft� Top8WidthyHeightParentShowHintShowHint	TabOrderTextBccEdit  TEditSubjectEditLeftPTopPWidthyHeightHintMessage subjectParentShowHintShowHint	TabOrderTextSubjectEdit  TEdit
SignOnEditLeft� TopPWidthyHeightHint#Signon message for the HELO commandParentShowHintShowHint	TabOrderText
SignOnEdit  TButton
SendButtonLeftlTopWidthKHeightHintNConnect, Helo, MailFrom, RcptTo, Data and Quit all chained in a single action.Caption&Send ParentShowHintShowHint	TabOrderOnClickSendButtonClick  TButtonAbortButtonLeftlTop WidthKHeightCaption&AbortTabOrder	OnClickAbortButtonClick  	TCheckBoxPlainTextCheckBoxLeftlTop<WidthMHeightCaption
Plain TextTabOrder
   TMemoDisplayMemoLeft TopWidthpHeightbAlignalBottomFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameCourier New
Font.Style Lines.StringsDisplayMemo 
ParentFontParentShowHint
ScrollBarsssBothShowHint	TabOrder  TPanelPanel1Left TopuWidth� Height� AlignalLeftTabOrder TMemoPlainTextMemoLeftTopWidth� HeightXHint'Enter the plain text message text here.AlignalClientFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameCourier New
Font.Style Lines.StringsPlainTextMemo 
ParentFontParentShowHint
ScrollBarsssBothShowHint	TabOrder   TMemoImageFilesMemoLeftTopYWidth� HeightPHint1Enter the list of image files here, one per line.AlignalBottomFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameCourier New
Font.Style Lines.StringsImageFilesMemo 
ParentFontParentShowHint
ScrollBarsssBothShowHint	TabOrder   TMemoHtmlTextMemoLeft� TopuWidthvHeight� HintaEnter the HTML text for the message. Special tags <#IMAGEn> will be replaced by image references.AlignalClientFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameCourier New
Font.Style Lines.StringsHtmlTextMemo 
ParentFontParentShowHint
ScrollBarsssBothShowHint	TabOrderWordWrap  THtmlSmtpCliHtmlSmtpClientTag 	ShareModesmtpShareDenyWrite	LocalAddr0.0.0.0PortsmtpAuthTypesmtpAuthNoneHdrPrioritysmtpPriorityNoneCharSet
iso-8859-1ContentTypesmtpHtml
OwnHeaders	OnDisplayHtmlSmtpClientDisplayOnRequestDoneHtmlSmtpClientRequestDoneOnSessionClosedHtmlSmtpClientSessionClosedLeft@Top�    