//
// this unit is part of the glscene project, http://glscene.org
//
{: registerxcollection<p>

	Register TXCollection property editor<p>

	<b>History : </b><font size=-1><ul>
           <li>03/07/04 - LR - Removed ..\ from the GLScene.inc
	   <li>16/04/00 - Egg - Creation
	</ul></font>
}
unit registerxcollection;

interface

{$i GLScene.inc}

uses classes, xcollection
   {$ifdef fpc}
   ,componenteditors, propedits
   {$else}
   {$ifdef gls_delphi_6_up} ,designeditors, designintf
   {$else} ,dsgnintf
   {$endif}
   {$endif};

type

	// TGLXCollectionProperty
	//
	TXCollectionProperty = class(TClassProperty)
		public
			{ Public Declarations }
			function GetAttributes: TPropertyAttributes; override;
			procedure Edit; override;
	end;

procedure Register;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses fxcollectioneditor;


procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TXCollection), nil, '', TXCollectionProperty);
end;

//----------------- TXCollectionProperty ------------------------------------

// GetAttributes
//
function TXCollectionProperty.GetAttributes: TPropertyAttributes;
begin
	Result:=[paDialog];
end;

// Edit
//
procedure TXCollectionProperty.Edit;
begin
   with XCollectionEditor do begin
      SetXCollection(TXCollection(GetOrdValue), Self.Designer);
      Show;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

	// class registrations
   
end.

