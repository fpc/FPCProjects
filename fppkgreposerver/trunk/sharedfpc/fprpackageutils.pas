unit fprPackageUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DOM,
  fpjson;

function ManifestToJSON(AManifest: TXMLDocument): TJSONArray;

implementation

function ManifestToJSON(AManifest: TXMLDocument): TJSONArray;
var
  PackagesNode: TDOMNode;
  PackageNode: TDOMNode;
  Node1: TDOMNode;
  JA: TJSONArray;
  JO: TJSONObject;
begin
  try
    JA := TJSONArray.Create;

    PackagesNode := AManifest.FindNode('packages');
    if Assigned(PackagesNode) then
      begin
      PackageNode := PackagesNode.FirstChild;
      while Assigned(PackageNode) do
        begin
        JO := TJSONObject.Create;
        JA.Add(JO);
        JO.Add('name', PackageNode.Attributes.GetNamedItem('name').TextContent);
        Node1 := PackageNode.FirstChild;
        while Assigned(Node1) do
          begin
          JO.Add(Node1.NodeName, Node1.TextContent);
          Node1 := Node1.NextSibling;
          end;
        PackageNode := PackageNode.NextSibling;
        end;
      end;

    Result := JA;
    JA := nil;
  finally
    JA.Free;
  end;
end;


end.

