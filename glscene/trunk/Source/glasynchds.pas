// GLAsyncHDS
{: Implements a HDS Filter that generates HeightData tiles in a seperate thread.<p>

   This component is a THeightDataSourceFilter, which uses a THeightDataSourceThread,
   to asyncronously search the HeightData cache for any queued tiles.
   When found, it then prepares the queued tile in its own THeightDataThread.

   This allows the GUI to remain responsive, and prevents freezes when new tiles are
   being prepared.  Although this keeps the framerate up, it may cause holes in the
   terrain to show, if the HeightDataThreads cant keep up with the TerrainRenderer's
   requests for new tiles.
   <p>

	<b>History : </b><font size=-1><ul>
      <li>05/03/07 - LIN - Added ThreadCount and WaitFor
      <li>12/02/07 - LIN - Creation
	</ul></font>
}

unit GLAsyncHDS;

interface

uses Classes, GLHeightData, GLGraphics, SyncObjs, Dialogs;

type
  TGLAsyncHDS = class;
  TIdleEvent = procedure(Sender:TGLAsyncHDS) of object;
  TNewTilePreparedEvent = procedure (Sender : TGLAsyncHDS; heightData : THeightData) of object;

	 TGLAsyncHDS = class (THeightDataSourceFilter)
	   private
	      { Private Declarations }
       FOnIdleEvent :TIdleEvent;
       FOnNewTilePrepared : TNewTilePreparedEvent;
	   protected
	      { Protected Declarations }
    public
	      { Public Declarations }
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure StartPreparingData(heightData : THeightData); override;
      procedure ThreadIsIdle; override;
      procedure NewTilePrepared(heightData:THeightData);
      function  ThreadCount:integer;
      procedure WaitFor;

	   published
	      { Published Declarations }
      property OnIdle : TIdleEvent read FOnIdleEvent write FOnIdleEvent;
      property OnNewTilePrepared : TNewTilePreparedEvent read FOnNewTilePrepared write FOnNewTilePrepared;
      property MaxThreads;         //sets the maximum number of simultaineous threads that will prepare tiles.(>1 is rarely needed)
      property Active;             //set to false, to ignore new queued tiles.(Partially processed tiles will still be completed)
  end;

  TGLAsyncHDThread = class(THeightDataThread)
    public
      Owner : TGLAsyncHDS;
      HDS   : THeightDataSource;
      Procedure Execute; override;
      Procedure Sync;
  end;


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, OpenGL1x, GLUtils;

// ------------------
// ------------------ TGLAsyncHDS ------------------
// ------------------

// Create
//
constructor TGLAsyncHDS.Create(AOwner: TComponent);
begin
	 inherited Create(AOwner);
  MaxThreads:=1;
end;

// Destroy
//
destructor TGLAsyncHDS.Destroy;
begin
	inherited Destroy;
end;

// StartPreparingData
//
procedure TGLAsyncHDS.StartPreparingData(heightData : THeightData);
var HDThread : TGLAsyncHDThread;
    HDS:THeightDataSource;
begin
  HDS:=self.HeightDataSource;
  //---if there is no linked HDS then return an empty tile--
  if not Assigned(HDS) then begin
    heightData.DataState:=hdsNone;
    exit;
  end;
  if (Active=false) then exit;

  HDS.ReadyWhenDone:=true;
  //---If not using threads then prepare the HD tile directly---  (everything else freezes until done)
  if MaxThreads=0 then HDS.StartPreparingData(HeightData)
  else begin //--MaxThreads>0 : start the thread and go back to start the next one--
    heightData.DataState:=hdsPreparing; //prevent other threads from preparing this HD.
    HDThread:=TGLAsyncHDThread.Create(true);
    HDThread.Owner:=self;
    HDThread.HDS:=self.HeightDataSource;
    HDThread.HeightData:=HeightData;
    heightData.Thread:=HDThread;
    HDThread.FreeOnTerminate:=false;
    HDThread.Resume;
  end;
end;

//OnIdle event
//
procedure TGLAsyncHDS.ThreadIsIdle;
begin
  if Assigned(FOnIdleEvent) then FOnIdleEvent(Self);
end;

//OnNewTilePrepared event
//
procedure TGLAsyncHDS.NewTilePrepared(heightData:THeightData);
begin
  if Assigned(FOnNewTilePrepared) then FOnNewTilePrepared(Self,HeightData);
end;

//ThreadCount
//  Count the active threads
//
function TGLAsyncHDS.ThreadCount:integer;
var lst: Tlist;
    i,TdCtr:integer;
    HD:THeightData;
begin
  lst:=self.Data.LockList;
  i:=0;TdCtr:=0;
  while(i<lst.Count)and(TdCtr<self.MaxThreads) do begin
    HD:=THeightData(lst.Items[i]);
    if HD.Thread<>nil then Inc(TdCtr);
    inc(i);
  end;
  self.Data.UnlockList;
  result:=TdCtr;
end;

//WaitFor
//  Wait for all running threads to finish.
//  Should only be called after setting Active to false.
procedure TGLAsyncHDS.WaitFor;
begin
  While ThreadCount>0 do sleep(0);
end;

//-------------------HD Thread----------------
Procedure TGLAsyncHDThread.Execute;
Begin
  HDS.ReadyWhenDone:=true;
  HDS.StartPreparingData(HeightData);
  HeightData.Thread:=nil;
  Synchronize(sync);
end;

Procedure TGLAsyncHDThread.Sync;
begin
  Owner.NewTilePrepared(heightData);
end;

//--------------------------------------------

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

	// class registrations
   RegisterClass(TGLAsyncHDS);

end.
