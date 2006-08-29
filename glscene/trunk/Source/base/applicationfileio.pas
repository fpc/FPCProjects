//
// this unit is part of the glscene project, http://glscene.org
//
{: applicationfileio<p>

   Components and fonction that abstract file I/O access for an application.<br>
   Allows re-routing file reads to reads from a single archive file f.i.<p>

      $Log: applicationfileio.pas,v $
      Revision 1.1  2006/01/10 20:50:44  z0m3ie
      recheckin to make shure that all is lowercase

      Revision 1.1  2006/01/09 21:01:42  z0m3ie
      *** empty log message ***

      Revision 1.2  2005/12/04 16:52:58  z0m3ie
      renamed everything to lowercase to get better codetools support and avoid unit finding bugs

      Revision 1.1  2005/12/01 21:24:10  z0m3ie
      *** empty log message ***

      Revision 1.2  2005/08/03 00:41:37  z0m3ie
      - added automatical generated History from CVS

	<b>History : </b><font size=-1><ul>
      <li>05/06/03 - EG - TDataFile moved in from GLMisc
      <li>31/01/03 - EG - Added FileExists mechanism
	   <li>21/11/02 - EG - Creation
	</ul></font>
}
unit applicationfileio;

interface

uses classes, sysutils;

type

   // TAFIOCreateFileStream
   //
   TAFIOCreateFileStream = function (const fileName : String; mode : Word) : TStream;

   // TAFIOFileStreamExists
   //
   TAFIOFileStreamExists = function (const fileName : String) : Boolean;

   // TAFIOFileStreamEvent
   //
   TAFIOFileStreamEvent = function (const fileName : String; mode : Word) : TStream of object;

   // TAFIOFileStreamExistsEvent
   //
   TAFIOFileStreamExistsEvent = function (const fileName : String) : Boolean of object;

	// TApplicationFileIO
	//
   {: Allows specifying a custom behaviour for ApplicationFileIO's CreateFileStream.<p>
      The component should be considered a helper only, you can directly specify
      a function via the vAFIOCreateFileStream variable.<br>
      If multiple TApplicationFileIO components exist in the application,
      the last one created will be the active one. }
	TApplicationFileIO = class (TComponent)
	   private
	      { Private Declarations }
         FOnFileStream : TAFIOFileStreamEvent;
         FOnFileStreamExists : TAFIOFileStreamExistsEvent;

	   protected
	      { Protected Declarations }

      public
	      { Public Declarations }
	      constructor Create(AOwner : TComponent); override;
	      destructor Destroy; override;

	   published
	      { Published Declarations }
         {: Event that allows you to specify a stream for the file.<p>
            Destruction of the stream is at the discretion of the code that
            invoked CreateFileStream. Return nil to let the default mechanism
            take place (ie. attempt a regular file system access). }
         property OnFileStream : TAFIOFileStreamEvent read FOnFileStream write FOnFileStream;
         {: Event that allows you to specify if a stream for the file exists.<p> }
         property OnFileStreamExists : TAFIOFileStreamExistsEvent read FOnFileStreamExists write FOnFileStreamExists;
	end;

   // TDataFileCapabilities
   //
   TDataFileCapability = (dfcRead, dfcWrite);
   TDataFileCapabilities = set of TDataFileCapability;

   // TDataFile
   //
   {: Abstract base class for data file formats interfaces.<p>
      This class declares base file-related behaviours, ie. ability to load/save
      from a file or a stream.<p>
      It is highly recommended to overload ONLY the stream based methods, as the
      file-based one just call these, and stream-based behaviours allow for more
      enhancement (such as other I/O abilities, compression, cacheing, etc.)
      to this class, without the need to rewrite subclasses. }
   TDataFile = class (TPersistent)
      private
         { Private Declarations }
         FOwner : TPersistent;
         FResourceName : String;

      protected
         { Protected Declarations }
         function GetOwner : TPersistent; override;

      public
         { Public Declarations }
	      constructor Create(AOwner: TPersistent); virtual;
         destructor Destroy; override;

         {: Describes what the TDataFile is capable of.<p>
            Default value is [dfcRead]. }
         class function Capabilities : TDataFileCapabilities; virtual;

         {: Duplicates Self and returns a copy.<p>
            Subclasses should override this method to duplicate their data. }
         function CreateCopy(AOwner: TPersistent) : TDataFile; dynamic;

         procedure LoadFromFile(const fileName : String); dynamic;
         procedure SaveToFile(const fileName : String); dynamic;
         procedure LoadFromStream(stream : TStream); dynamic; abstract;
         procedure SaveToStream(stream : TStream); dynamic;

         {: Optionnal resource name.<p>
            When using LoadFromFile/SaveToFile, the filename is placed in it,
            when using the Stream variants, the caller may place the resource
            name in it for parser use. }
         property ResourceName : String read FResourceName write FResourceName;
   end;

   TDataFileClass = class of TDataFile;

//: Returns true if an ApplicationFileIO has been defined
function ApplicationFileIODefined : Boolean;

{: Creates a file stream corresponding to the fileName.<p>
   If the file does not exists, an exception will be triggered.<br>
   Default mechanism creates a regular TFileStream, the 'mode' parameter
   is similar to the one for TFileStream. }
function CreateFileStream(const fileName : String;
                          mode : Word = fmOpenRead+fmShareDenyNone) : TStream;
{: Queries is a file stream corresponding to the fileName exists.<p> }
function FileStreamExists(const fileName : String) : Boolean;

procedure Register;

var
   vAFIOCreateFileStream : TAFIOCreateFileStream = nil;
   vAFIOFileStreamExists : TAFIOFileStreamExists = nil;

// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
implementation
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------

var
   vAFIO : TApplicationFileIO = nil;

procedure Register;
begin
	RegisterComponents('GLScene Utils', [TApplicationFileIO]);
end;

// ApplicationFileIODefined
//
function ApplicationFileIODefined : Boolean;
begin
   Result:=   (Assigned(vAFIOCreateFileStream) and Assigned(vAFIOFileStreamExists))
           or Assigned(vAFIO);
end;

// CreateFileStream
//
function CreateFileStream(const fileName : String;
                          mode : Word = fmOpenRead+fmShareDenyNone) : TStream;
begin
   if Assigned(vAFIOCreateFileStream) then
      Result:=vAFIOCreateFileStream(fileName, mode)
   else begin
      Result:=nil;
      if Assigned(vAFIO) and Assigned(vAFIO.FOnFileStream) then
         Result:=vAFIO.FOnFileStream(fileName, mode);
      if not Assigned(Result) then begin
         if ((mode and fmCreate)=fmCreate) or FileExists(fileName) then
            Result:=TFileStream.Create(fileName, mode)
         else raise Exception.Create('File not found: "'+fileName+'"');
      end;
   end;
end;

// FileStreamExists
//
function FileStreamExists(const fileName : String) : Boolean;
begin
   if Assigned(vAFIOFileStreamExists) then
      Result:=vAFIOFileStreamExists(fileName)
   else begin
      if Assigned(vAFIO) and Assigned(vAFIO.FOnFileStreamExists) then
         Result:=vAFIO.FOnFileStreamExists(fileName)
      else Result:=FileExists(fileName);
   end;
end;

// ------------------
// ------------------ TApplicationFileIO ------------------
// ------------------

// Create
//
constructor TApplicationFileIO.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);
   vAFIO:=Self;
end;

// Destroy
//
destructor TApplicationFileIO.Destroy;
begin
   vAFIO:=nil;
	inherited Destroy;
end;

// ------------------
// ------------------ TDataFile ------------------
// ------------------

// Create
//
constructor TDataFile.Create(AOwner: TPersistent);
begin
   inherited Create;
   FOwner:=AOwner;
end;

// Destroy
//
destructor TDataFile.Destroy;
begin
   inherited;
end;

// Capabilities
//
class function TDataFile.Capabilities : TDataFileCapabilities;
begin
   Result:=[dfcRead];
end;

// GetOwner
//
function TDataFile.GetOwner : TPersistent;
begin
   Result:=FOwner;
end;

// CreateCopy
//
function TDataFile.CreateCopy(AOwner: TPersistent) : TDataFile;
begin
   if Self<>nil then
      Result:=TDataFileClass(Self.ClassType).Create(AOwner)
   else Result:=nil;
end;

// LoadFromFile
//
procedure TDataFile.LoadFromFile(const fileName : String);
var
   fs : TStream;
begin
   ResourceName:=ExtractFileName(fileName);
   fs:=CreateFileStream(fileName, fmOpenRead+fmShareDenyNone);
   try
      LoadFromStream(fs);
   finally
      fs.Free;
   end;
end;

// SaveToFile
//
procedure TDataFile.SaveToFile(const fileName : String);
var
   fs : TStream;
begin
   ResourceName:=ExtractFileName(fileName);
   fs:=CreateFileStream(fileName, fmCreate);
   try
      SaveToStream(fs);
   finally
      fs.Free;
   end;
end;

// SaveToStream
//
procedure TDataFile.SaveToStream(stream : TStream);
begin
   Assert(False, 'Export for '+ClassName+' to '+stream.ClassName+' not available.');
end;

end.
