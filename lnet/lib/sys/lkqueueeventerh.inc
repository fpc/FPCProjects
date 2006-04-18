{% lkqueueeventerh.inc included by levents.pas }

{$ifdef BSD}
// only for 2.1.1+ or 2.0.3+
{$if defined(ver2) and (fpc_release = 0) and (fpc_patch > 2)}

  { TLKQueueEventer }

  TLKQueueEventer = class(TLEventer)
   protected
    FTimeout: TTimeSpec;
    FEvents: array of TKEvent;
    FChanges: array of TKEvent;
    FFreeSlot: Integer;
    FQueue: THandle;
    function GetTimeout: DWord; override;
    procedure SetTimeout(const Value: DWord); override;
    procedure Inflate;
   public
    constructor Create; override;
    destructor Destroy; override;
    function AddHandle(aHandle: TLHandle): Boolean; override;
    function CallAction: Boolean; override;
  end;
  
{$endif} // version
{$endif} // bsd
