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
//   Common implementation for TNotebook methods
// ===================================================================

{constructor TNotebook.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPages := TCollection.Create(TNotebookPage);
end;

destructor TNotebook.Destroy;
begin
  FPages.Free;
  inherited Destroy;
end;

function TNotebook.GetPages(AIndex: Integer): TNotebookPage;
begin
  Result := TNotebookPage(FPages.Items[AIndex]);
end;}


{
  $Log$
  Revision 1.3  2000/03/19 00:29:31  sg
  * Huge shift towards more VCL compatiblity (NOTE: Many widgets/controls
    are not working correctly at the moment!)

  Revision 1.2  2000/02/19 19:09:00  sg
  * Fixed layouting bugs; minor improvements

  Revision 1.1  2000/02/10 18:51:59  sg
  * First implementation; most methods have been moved from the GTK target
    implementation to this file.

}
