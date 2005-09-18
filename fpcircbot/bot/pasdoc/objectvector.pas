{
  @cvs($Date: 2005/05/24 21:02:39 $)
  @author(Johannes Berg <johannes@sipsolutions.de>)
  @author(Michalis Kamburelis)
  a simple object vector
}
unit ObjectVector;

{$mode delphi}{$H+}

interface
uses
  Contnrs,
  Classes;

type
  TObjectVector = class(TObjectList)
  public
    { This is only to make constructor virtual, while original
      TObjectList has a static constructor. }
    constructor Create(const AOwnsObject: boolean); virtual;
  end;

function ObjectVectorIsNilOrEmpty(const AOV: TObjectVector): boolean; 

implementation

function ObjectVectorIsNilOrEmpty(const AOV: TObjectVector): boolean;
begin
  Result := not Assigned(AOV);
  if not Result then begin
    Result := AOV.Count = 0;
  end;
end;

{ TObjectVector }

constructor TObjectVector.Create(const AOwnsObject: boolean);
begin
  inherited Create(AOwnsObject);
end;

end.
