unit DrawShapes;

{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics, FPCanvas, Math;

type

  { TShapeBase }

  TShapeBase = class
  private
    pC: TColor;
    pW: Integer;
    pS: TFPPenStyle;
  public
    procedure Draw(canvas: TCanvas); virtual;
    procedure Point(point: TPoint; new: Boolean = true); virtual; abstract;
    function BoundingRect:TRect; virtual; abstract;
  published
    property penC: TColor read pC write pC;
    property penW: Integer read pW write pW;
    property penS: TFPPenStyle read pS write pS;
  end;

  { TS2Point }

  TS2Point = class(TShapeBase)
  private
    p1, p2: TPoint;
  public
    procedure Point(point: TPoint; new: Boolean = true); override;
    function BoundingRect:TRect; override;
  end;

  { TS2Line }

  TS2Line = class(TS2Point)
  public
    procedure Draw(canvas: TCanvas); override;
  end;

  { TS2Fill }

  TS2Fill = class(TS2Point)
  private
    bC: TColor;
    bS: TFPBrushStyle;
  published
    property brushC: TColor read bC write bC;
    property brushS: TFPBrushStyle read bS write bS;
  end;

  { TS2FRectangle }

  TS2FRectangle = class(TS2Fill)
  public
    procedure Draw(canvas: TCanvas); override;
  end;

  { TS2FEllipce }

  TS2FEllipce = class(TS2Fill)
  public
    procedure Draw(canvas: TCanvas); override;
  end;

  { TS2FRectangleRound }

  TS2FRectangleRound = class(TS2Fill)
  private
    r: integer;
  published
    property radius: integer read r write r;
  public
    procedure Draw(canvas: TCanvas); override;
  end;

  { TSMPoint }

  TSMPoint = class(TShapeBase)
  private
    arr: array of TPoint;
  public
    procedure Point(point: TPoint; new: Boolean = true); override;
    procedure Draw(canvas: TCanvas); override;
    function BoundingRect:TRect; override;
  end;

implementation

{ TSMPoint }

procedure TSMPoint.Point(point: TPoint; new: Boolean);
begin
  if new then begin
    setLength(arr, length(arr)+1);
    arr[high(arr)]:= point;
  end else begin
    if length(arr) = 1 Then setLength(arr, 2);
    arr[high(arr)]:= point;
  end;
end;

procedure TSMPoint.Draw(canvas: TCanvas);
begin
  inherited Draw(canvas);
  canvas.Polyline(arr);
end;

function TSMPoint.BoundingRect: TRect;
var i: integer;
begin
  for i:= 0 to high(arr) do begin
    If arr[i].X < Result.Left Then Result.Left:= arr[i].X;
    If arr[i].X > Result.Right Then Result.Right:= arr[i].X;
    If arr[i].Y < Result.Top Then Result.Top:= arr[i].Y;
    If arr[i].Y > Result.Bottom Then Result.Bottom:= arr[i].Y;
  end;
  Result.TopLeft.x := Result.Left;
  Result.TopLeft.y := Result.Top;
  Result.BottomRight.x := Result.Right;
  Result.BottomRight.y := Result.Bottom;
end;

{ TS2FRectangleRound }

procedure TS2FRectangleRound.Draw(canvas: TCanvas);
begin
  inherited Draw(canvas);
  canvas.RoundRect(p1.x, p1.y, p2.x, p2.y, r, r);
end;

{ TS2FEllipce }

procedure TS2FEllipce.Draw(canvas: TCanvas);
begin
  inherited Draw(canvas);
  canvas.Ellipse(p1.x, p1.y, p2.x, p2.y);
end;

{ TS2FRectangle }

procedure TS2FRectangle.Draw(canvas: TCanvas);
begin
  inherited Draw(canvas);
  canvas.Rectangle(p1.x, p1.y, p2.x, p2.y);
end;

{ TShapeBase }

procedure TShapeBase.Draw(canvas: TCanvas);
begin
  canvas.pen.Color := pC;
  canvas.pen.Width := pW;
end;

{ TS2Line }

procedure TS2Line.Draw(canvas: TCanvas);
begin
  inherited Draw(canvas);
  canvas.Line(p1, p2);
end;


{ TS2Point }

procedure TS2Point.Point(point: TPoint; new: Boolean);
begin
  if new then begin
    p1:= point;
    p2:= p1;
  end
  else p2:= point;
end;

function TS2Point.BoundingRect: TRect;
begin
  Result.Left := Min(p1.x, p2.x);
  Result.Right := Max(p1.x, p2.x);
  Result.Top := Min(p1.y, p2.y);
  Result.Bottom := Max(p1.y, p2.y);
  Result.TopLeft.x := Result.Left;
  Result.TopLeft.y := Result.Top;
  Result.BottomRight.x := Result.Right;
  Result.BottomRight.y := Result.Bottom;
end;

end.
