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
//   Splitter Widget Implementation
// ===================================================================

procedure TSplitter.SetPane1(APane1: TWidget);
begin
  FPane1 := APane1;
  if FCreationState >= wcsCreationInProgress then begin
    if Assigned(FPane1) then begin
      FPane1.FinishCreation;
      gtk_paned_pack1(PGtkPaned(Handle.Widget), FPane1.Handle.Widget,
        FResizePolicy <> srFixedPane1, True);
    end; //### else...
  end;
end;

procedure TSplitter.SetPane2(APane2: TWidget);
begin
  FPane2 := APane2;
  if FCreationState >= wcsCreationInProgress then begin
    if Assigned(FPane2) then begin
      FPane2.FinishCreation;
      gtk_paned_pack2(PGtkPaned(Handle.Widget), FPane2.Handle.Widget,
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
    Handle.Widget := gtk_vpaned_new
  else
    Handle.Widget := gtk_hpaned_new;
  gtk_paned_set_gutter_size(PGtkPaned(Handle.Widget), 10);

  if Assigned(FPane1) then SetPane1(FPane1);
  if Assigned(FPane2) then SetPane2(FPane2);

  inherited OnFinishCreation;
end;


{
  $Log$
  Revision 1.1  1999/12/30 21:33:27  sg
  Initial revision

}
