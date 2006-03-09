{% lkqueueeventerh.inc included by levents.pas }

{$ifdef BSD}

  { TLKQueueEventer }

  TLKQueueEventer = class(TLEventer)
   protected
    FTimeout: TTimeSpec;
    FEvents: array of TKEvent;
    FChanges: array of TKEvent;
    FFreeSlot: Integer;
    FQueue: THandle;
    FFreeRoot: TLHandle; // the root of "free" list if any
    FFreeIter: TLHandle; // the last of "free" list if any
    function GetTimeout: DWord; override;
    procedure SetTimeout(const Value: DWord); override;
    procedure Inflate;
    procedure AddForFree(aHandle: TLHandle);
    procedure FreeHandles;
   public
    constructor Create; override;
    destructor Destroy; override;
    function AddHandle(aHandle: TLHandle): Boolean; override;
    function CallAction: Boolean; override;
  end;

{$endif}
