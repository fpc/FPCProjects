// FXCollectionEditor
{: Egg<p>

	Edits a TXCollection<p>

	<b>Historique : </b><font size=-1><ul>
      <li>03/07/04 - LR - Make change for Linux
      <li>12/07/03 - DanB - Fixed crash when owner deleted        
      <li>27/02/02 - Egg - Fixed crash after item deletion
      <li>11/04/00 - Egg - Fixed crashes in IDE
		<li>06/04/00 - Egg - Creation
	</ul></font>
}
unit FXCollectionEditor;

interface

{$i GLScene.inc}

{$IFDEF MSWINDOWS}
uses
  Windows, Forms, XCollection, Messages, ImgList, Controls, Classes, ActnList, 
  Menus, ComCtrls, ToolWin, 
  {$ifdef GLS_DELPHI_6_UP} DesignEditors, DesignIntf {$else} DsgnIntf {$endif};
{$ENDIF}
{$IFDEF LINUX}
uses
  QForms, XCollection, QImgList, QControls, Classes, QActnList, 
  QMenus, QComCtrls, DesignEditors, DesignIntf; 
{$ENDIF}


type
  TXCollectionEditor = class(TForm)
	 ListView: TListView;
    PMListView: TPopupMenu;
    ActionList: TActionList;
    ACRemove: TAction;
    ACMoveUp: TAction;
    ACMoveDown: TAction;
    ImageList: TImageList;
    MIAdd: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
	 Moveup1: TMenuItem;
    Movedown1: TMenuItem;
    ToolBar1: TToolBar;
    TBAdd: TToolButton;
    ToolButton2: TToolButton;
	 ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    PMToolBar: TPopupMenu;
	 procedure TBAddClick(Sender: TObject);
	 procedure ListViewChange(Sender: TObject; Item: TListItem;
		Change: TItemChange);
    procedure ACRemoveExecute(Sender: TObject);
    procedure ACMoveUpExecute(Sender: TObject);
    procedure ACMoveDownExecute(Sender: TObject);
    procedure PMToolBarPopup(Sender: TObject);
    procedure PMListViewPopup(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
  private
	 { Déclarations privées }
	 FXCollection : TXCollection;
//    ownerComponent : TComponent;
    FDesigner : {$ifdef GLS_DELPHI_6_UP} IDesigner {$else} IFormDesigner {$endif};
    updatingListView : Boolean;
	 procedure PrepareListView;
	 procedure PrepareXCollectionItemPopup(parent : TMenuItem);
	 procedure OnAddXCollectionItemClick(Sender : TObject);
    procedure OnNameChanged(Sender : TObject);
    procedure OnXCollectionDestroyed(Sender : TObject);
  protected
	 procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
	 { Déclarations publiques }
    procedure SetXCollection(aXCollection: TXCollection;
         designer: {$ifdef GLS_DELPHI_6_UP} IDesigner {$else} IFormDesigner {$endif});
  end;

function XCollectionEditor : TXCollectionEditor;
procedure ReleaseXCollectionEditor;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

{$IFDEF MSWINDOWS}
{$R *.dfm}
{$ENDIF}
{$IFDEF LINUX}
{$R *.xfm}
{$ENDIF}


uses
{$IFDEF MSWINDOWS}
  GLMisc, SysUtils, GLBehaviours, GLScene, Dialogs; 
{$ENDIF}
{$IFDEF LINUX}
  GLMisc, SysUtils, GLBehaviours, GLScene, QDialogs; 
{$ENDIF}


resourcestring
   cXCollectionEditor = 'XCollection editor';

var
	vXCollectionEditor : TXCollectionEditor;

function XCollectionEditor : TXCollectionEditor;
begin
	if not Assigned(vXCollectionEditor) then
      vXCollectionEditor:=TXCollectionEditor.Create(nil);
	Result:=vXCollectionEditor;
end;

procedure ReleaseXCollectionEditor;
begin
	if Assigned(vXCollectionEditor) then begin
		vXCollectionEditor.Release;
		vXCollectionEditor:=nil;
	end;
end;

// FormCreate
procedure TXCollectionEditor.FormCreate(Sender: TObject);
begin
   RegisterGLBehaviourNameChangeEvent(OnNameChanged);
   RegisterXCollectionDestroyEvent(OnXCollectionDestroyed);
end;

// FormDestroy
//
procedure TXCollectionEditor.FormDestroy(Sender: TObject);
begin
	DeRegisterGLBehaviourNameChangeEvent(OnNameChanged);
        DeRegisterXCollectionDestroyEvent(OnXCollectionDestroyed);        
end;

// FormHide
//
procedure TXCollectionEditor.FormHide(Sender: TObject);
begin
   SetXCollection(nil, nil);
   ReleaseXCollectionEditor;
end;

// SetXCollection
//
procedure TXCollectionEditor.SetXCollection(aXCollection: TXCollection;
   designer: {$ifdef GLS_DELPHI_6_UP} IDesigner {$else} IFormDesigner {$endif});
begin
//	if Assigned(ownerComponent) then
//		ownerComponent.RemoveFreeNotification(Self);
	FXCollection:=aXCollection;
	FDesigner:=designer;
	if Assigned(FXCollection) then begin
//		if Assigned(FXCollection.Owner) and (FXCollection.Owner is TComponent) then
//		ownerComponent:=TComponent(FXCollection.Owner);
//		if Assigned(ownerComponent) then
//			ownerComponent.FreeNotification(Self);
      Caption:=FXCollection.GetNamePath;
   end else begin
//      ownerComponent:=nil;
      Caption:=cXCollectionEditor;
   end;
   PrepareListView;
end;

// TBAddClick
//
procedure TXCollectionEditor.TBAddClick(Sender: TObject);
begin
	TBAdd.CheckMenuDropdown;
end;

// ListViewChange
//
procedure TXCollectionEditor.ListViewChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
var
	sel : Boolean;
begin
	if (Change=ctState) and Assigned(FDesigner) and (not updatingListView) then begin
		// setup enablings
		sel:=(ListView.Selected<>nil);
      TBAdd.Enabled:=Assigned(FDesigner);
		ACRemove.Enabled:=sel;
		ACMoveUp.Enabled:=sel and (ListView.Selected.Index>0);
		ACMoveDown.Enabled:=sel and (ListView.Selected.Index<ListView.Items.Count-1);
      if Assigned(FDesigner) then
         if sel then
            FDesigner.SelectComponent(TXCollectionItem(ListView.Selected.Data))
{$ifndef GLS_DELPHI_4}
         else FDesigner.NoSelection;
{$else}
         else FDesigner.SelectComponent(nil);
{$endif}
	end;
end;

// PrepareListView
//
procedure TXCollectionEditor.PrepareListView;
var
	i : Integer;
	prevSelData : Pointer;
	XCollectionItem : TXCollectionItem;
begin
   Assert(Assigned(ListView));
   updatingListView:=True;
   try
      if ListView.Selected<>nil then
         prevSelData:=ListView.Selected.Data
      else prevSelData:=nil;
      with ListView.Items do begin
         BeginUpdate;
         Clear;
         if Assigned(FXCollection) then begin
            for i:=0 to FXCollection.Count-1 do with Add do begin
               XCollectionItem:=FXCollection[i];
               Caption:=Format('%d - %s', [i, XCollectionItem.Name]);
               SubItems.Add(XCollectionItem.FriendlyName);
               Data:=XCollectionItem;
            end;
            if prevSelData<>nil then
               ListView.Selected:=ListView.FindData(0, prevSelData, True, False);
         end;
         EndUpdate;
      end;
   finally
      updatingListView:=False;
   end;
   ListViewChange(Self, nil, ctState);
end;

// PrepareXCollectionItemPopup
//
procedure TXCollectionEditor.PrepareXCollectionItemPopup(parent : TMenuItem);
var
	i : Integer;
	list : TList;
	XCollectionItemClass : TXCollectionItemClass;
	mi : TMenuItem;
begin
	list:=GetXCollectionItemClassesList(FXCollection.ItemsClass);
	try
{$ifdef GLS_DELPHI_5_UP}
		parent.Clear;
{$else}
		for i:=parent.Count-1 downto 0 do parent.Delete(i); 
{$endif}
		for i:=0 to list.Count-1 do begin
			XCollectionItemClass:=TXCollectionItemClass(list[i]);
			mi:=TMenuItem.Create(owner);
			mi.Caption:=XCollectionItemClass.FriendlyName;
			mi.OnClick:=OnAddXCollectionItemClick;
			mi.Tag:=Integer(XCollectionItemClass);
			mi.Enabled:=Assigned(FXCollection) and FXCollection.CanAdd(XCollectionItemClass);
			parent.Add(mi);
		end;
	finally
		list.Free;
	end;
end;

// OnNameChanged
//
procedure TXCollectionEditor.OnNameChanged(Sender : TObject);
begin
   if TXCollectionItem(Sender).Owner=FXCollection then
      PrepareListView;
end;

// OnXCollectionDestroyed
//
procedure TXCollectionEditor.OnXCollectionDestroyed(Sender : TObject);
begin
   if TXCollection(Sender)=FXCollection then
      Close;
end;

// Notification
//
procedure TXCollectionEditor.Notification(AComponent: TComponent; Operation: TOperation);
begin
     {	if (Operation=opRemove) and (AComponent=ownerComponent) then begin
		ownerComponent:=nil;
		SetXCollection(nil, nil);
		Close;
	end;
        }
	inherited;
end;

// OnAddXCollectionItemClick
//
procedure TXCollectionEditor.OnAddXCollectionItemClick(Sender : TObject);
var
	XCollectionItemClass : TXCollectionItemClass;
	XCollectionItem : TXCollectionItem;
begin
	XCollectionItemClass:=TXCollectionItemClass((Sender as TMenuItem).Tag);
	XCollectionItem:=XCollectionItemClass.Create(FXCollection);
	PrepareListView;
	ListView.Selected:=ListView.FindData(0, XCollectionItem, True, False);
   FDesigner.Modified;
end;

// ACRemoveExecute
//
procedure TXCollectionEditor.ACRemoveExecute(Sender: TObject);
begin
	if ListView.Selected<>nil then begin
      FDesigner.Modified;
{$ifndef GLS_DELPHI_4}
      FDesigner.NoSelection;
{$else}
      FDesigner.SelectComponent(nil);
{$endif}
		TXCollectionItem(ListView.Selected.Data).Free;
      ListView.Selected.Free;
      ListViewChange(Self, nil, ctState);
	end;
end;

// ACMoveUpExecute
//
procedure TXCollectionEditor.ACMoveUpExecute(Sender: TObject);
begin
	if ListView.Selected<>nil then begin
		TXCollectionItem(ListView.Selected.Data).MoveUp;
		PrepareListView;
      FDesigner.Modified;
	end;
end;

// ACMoveDownExecute
//
procedure TXCollectionEditor.ACMoveDownExecute(Sender: TObject);
begin
	if ListView.Selected<>nil then begin
		TXCollectionItem(ListView.Selected.Data).MoveDown;
		PrepareListView;
      FDesigner.Modified;
	end;
end;

// PMToolBarPopup
//
procedure TXCollectionEditor.PMToolBarPopup(Sender: TObject);
begin
	PrepareXCollectionItemPopup(PMToolBar.Items);
end;

// PMListViewPopup
//
procedure TXCollectionEditor.PMListViewPopup(Sender: TObject);
begin
	PrepareXCollectionItemPopup(MIAdd);
end;

initialization

finalization

   ReleaseXCollectionEditor;

end.


