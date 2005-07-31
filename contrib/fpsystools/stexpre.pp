unit stexpre;

Interface

uses stexpr, SysUtils, Classes, Controls, StdCtrls, ////TL removed Q from QControls and QStdCtrls
  {$IFDEF UseMathUnit} Math, {$ENDIF}
  StBase, StConst, StMath;

type
  TStExpressionEdit = class(TEdit)
  {.Z+}
  protected {private}
    {property variables}
    FAutoEval : Boolean;
    FExpr     : TStExpression;
    FOnError  : TStExprErrorEvent;

    {property methods}
    function GetOnAddIdentifier : TNotifyEvent;
    function GetOnGetIdentValue : TStGetIdentValueEvent;
    procedure SetOnAddIdentifier(Value : TNotifyEvent);
    procedure SetOnGetIdentValue(Value : TStGetIdentValueEvent);

    {VCL control methods}
    procedure DoExit; override;
    procedure DoEvaluate;
  {.Z-}

  protected
    procedure KeyPress(var Key: Char);
      override;

  public
    constructor Create(AOwner : TComponent);
      override;
    destructor Destroy;
      override;

    function Evaluate : TStFloat;

    property Expr : TStExpression
      read FExpr;

  published
    property AutoEval : Boolean
      read FAutoEval write FAutoEval;

    property OnAddIdentifier : TNotifyEvent
      read GetOnAddIdentifier write SetOnAddIdentifier;
    property OnError : TStExprErrorEvent
      read FOnError write FOnError;
    property OnGetIdentValue : TStGetIdentValueEvent
      read GetOnGetIdentValue write SetOnGetIdentValue;
  end;

Implementation

{*** TStExpressionEdit ***}
procedure TStExpressionEdit.DoExit;
begin
  inherited;

  if FAutoEval then begin
    try
      DoEvaluate;
    except
      SetFocus;
      raise;
    end;
  end;
end;

constructor TStExpressionEdit.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  FExpr := TStExpression.Create(Self);
end;

destructor TStExpressionEdit.Destroy;
begin
  FExpr.Free;

  inherited Destroy;
end;

procedure TStExpressionEdit.DoEvaluate;
var
  V : TStFloat;
begin
  if Text > '' then begin
    V := Evaluate;
    if FExpr.FLastError = 0 then
      Text := FloatToStr(V)
    else
      SelStart := FExpr.FErrorPos;
  end else
    Text := '0';
end;

function TStExpressionEdit.Evaluate : TStFloat;
begin
  Result := 0;
  FExpr.Expression := Text;
  try
    Result := FExpr.AnalyzeExpression;
  except
    on E : EStExprError do begin
      SelStart := FExpr.FErrorPos;
      if Assigned(FOnError) then
        FOnError(Self, E.ErrorCode, E.Message)
      else
        raise;
    end else
      raise;
  end;
end;

function TStExpressionEdit.GetOnAddIdentifier : TNotifyEvent;
begin
  Result := FExpr.OnAddIdentifier;
end;

function TStExpressionEdit.GetOnGetIdentValue : TStGetIdentValueEvent;
begin
  Result := FExpr.OnGetIdentValue;
end;

procedure TStExpressionEdit.KeyPress(var Key : Char);
begin
  if Key = #13 then begin
    DoEvaluate;
    Key := #0;
    SelStart := Length(Text);
  end;

  inherited KeyPress(Key);
end;

procedure TStExpressionEdit.SetOnAddIdentifier(Value : TNotifyEvent);
begin
  FExpr.OnAddIdentifier := Value;
end;

procedure TStExpressionEdit.SetOnGetIdentValue(Value : TStGetIdentValueEvent);
begin
  FExpr.OngetIdentValue := Value;
end;

end.