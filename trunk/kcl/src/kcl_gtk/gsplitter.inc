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
//   Splitter Widget Implementation
// ===================================================================

procedure TSplitter.SetPane1(APane1: TWidget);
begin
  FPane1 := APane1;
  if Assigned(FPane1) then
    FPane1.FParent := Self;
  if FCreationState >= wcsCreationInProgress then begin
    if Assigned(FPane1) then begin
      FPane1.FinishCreation;
      gtk_paned_pack1(PGtkPaned(FHandle), FPane1.FHandle,
        FResizePolicy <> srFixedPane1, True);
    end; //### else...
  end;
end;

procedure TSplitter.SetPane2(APane2: TWidget);
begin
  FPane2 := APane2;
  if Assigned(FPane2) then
    FPane2.FParent := Self;
  if FCreationState >= wcsCreationInProgress then begin
    if Assigned(FPane2) then begin
      FPane2.FinishCreation;
      gtk_paned_pack2(PGtkPaned(FHandle), FPane2.FHandle,
        FResizePolicy <> srFixedPane2, True);
    end; // ### else...
  end;
end;

procedure TSplitter.SetOrientation(AOrientation: TSplitterOrientation);
begin
  FOrientation := AOrientation;
end;

procedure TSplitter.SetResizePolicy(APolicy: TSplitterResizePolicy);
begin
  FResizePolicy := APolicy;
end;

procedure TSplitter.OnFinishCreation;
begin
  if FOrientation = soHorizontal then
    FHandle := gtk_vpaned_new
  else
    FHandle := gtk_hpaned_new;
  gtk_paned_set_gutter_size(PGtkPaned(FHandle), 10);

  if Assigned(FPane1) then SetPane1(FPane1);
  if Assigned(FPane2) then SetPane2(FPane2);

  inherited OnFinishCreation;
end;


{
  $Log$
  Revision 1.2  2000/01/24 00:27:43  sg
  * The "Parent" property of the panes are now set

  Revision 1.1.1.1  1999/12/30 21:33:27  sg
  Initial import

}
