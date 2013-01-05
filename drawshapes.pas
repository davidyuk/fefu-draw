unit DrawShapes;

{$mode objfpc}{$H+}{$M+}

interface

uses
  Classes, Graphics, FPCanvas, Math, DrawZoom;

type

  { TShapeBase }

  TShapeBase = class
  private
    pC: TColor;
    pW: Integer;
    pS: TFPPenStyle;
    sel: boolean;
  public
    procedure Draw(canvas: TCanvas); virtual;
    procedure Point(point: TPoint; new: Boolean = true); virtual; abstract;
    function BoundingRect:TRectReal; virtual; abstract;
    property Selected: boolean read sel write sel;
    procedure Select(aRect: TRect); virtual; abstract;
  published
    property penC: TColor read pC write pC;
    property penW: Integer read pW write pW;
    property penS: TFPPenStyle read pS write pS;
    constructor Create; virtual;
  end;

  { TS2Point }

  TS2Point = class(TShapeBase)
  private
    p1r, p2r: TPointReal;
    function p1read:TPoint;
    function p2read:TPoint;
    procedure p1write(p: TPoint);
    procedure p2write(p: TPoint);
    property p1: TPoint read p1read write p1write;
    property p2: TPoint read p2read write p2write;
  public
    procedure Point(point: TPoint; new: Boolean = true); override;
    function BoundingRect:TRectReal; override;
    procedure Select(aRect: TRect); override;
    procedure Draw(canvas: TCanvas); override;
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
  public
    procedure Draw(canvas: TCanvas); override;
    constructor Create; override;
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
    constructor Create; override;
  end;

  { TSMPoint }

  TSMPoint = class(TShapeBase)
  private
    points: array of TPointReal;
  public
    procedure Point(point: TPoint; new: Boolean = true); override;
    procedure Draw(canvas: TCanvas); override;
    procedure Select(aRect: TRect); override;
    function BoundingRect:TRectReal; override;
  end;

implementation

{ TS2Fill }

procedure TS2Fill.Draw(canvas: TCanvas);
begin
  inherited Draw(canvas);
  canvas.Brush.Color:= bC;
  canvas.Brush.Style:= bS;
end;

constructor TS2Fill.Create;
begin
  inherited Create;
  bS:= bsSolid;
end;

{ TSMPoint }

procedure TSMPoint.Point(point: TPoint; new: Boolean);
begin
  if new then begin
    setLength(points, length(points)+1);
    points[high(points)]:= VP.StoW(point);
  end else begin
    if length(points) = 1 Then setLength(points, 2);
    points[high(points)]:= VP.StoW(point);
  end;
end;

procedure TSMPoint.Draw(canvas: TCanvas);
var i: integer;
  t: TRectReal;
  p1, p2: TPointReal;
  p3, p4: TPoint;
begin
  inherited Draw(canvas);
  canvas.MoveTo(VP.WtoS(points[0])); //может стоит проверить есть-ли в массиве элементы
  for i:= 0 to high(points) do
    canvas.LineTo(VP.WtoS(points[i]));
  if sel then begin
    canvas.pen.width:= 1;
    canvas.Pen.Color:= clGreen;
    canvas.Brush.Style:= bsClear;
    t:= BoundingRect;
    p1.x:= t.Left;
    p1.y:= t.Top;
    p2.x:= t.Right;
    p2.y:= t.Bottom;
    p3:= VP.WtoS(p1);
    p4:= VP.WtoS(p2);
    canvas.Rectangle(p3.x-5, p3.y-5, p4.x+5, p4.y+5);
  end;
end;

procedure TSMPoint.Select(aRect: TRect);
var
  i: integer;
  p: TPoint;
begin
  sel:= true;
  for i:= 0 to high(points) do begin
    p:= VP.WtoS(points[i]);
    if not((p.x >= aRect.Left) and (p.x <= aRect.Right) and (p.y >= aRect.Top) and (p.y <= aRect.Bottom)) then begin
      sel:= false;
      exit;
    end;
  end;
end;

function TSMPoint.BoundingRect: TRectReal;
var i: integer;
begin
  for i:= 0 to high(points) do begin
    if i = 0 then begin
      Result.Left:= points[i].X;
      Result.Right:= points[i].X;
      Result.Top:= points[i].Y;
      Result.Bottom:= points[i].Y;
      Continue;
    end;
    If points[i].X < Result.Left Then Result.Left:= points[i].X;
    If points[i].X > Result.Right Then Result.Right:= points[i].X;
    If points[i].Y < Result.Top Then Result.Top:= points[i].Y;
    If points[i].Y > Result.Bottom Then Result.Bottom:= points[i].Y;
  end;
end;

{ TS2FRectangleRound }

procedure TS2FRectangleRound.Draw(canvas: TCanvas);
begin
  inherited Draw(canvas);
  canvas.RoundRect(p1.x, p1.y, p2.x, p2.y, r, r);
end;

constructor TS2FRectangleRound.Create;
begin
  inherited Create;
  radius:= 1;
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
  canvas.pen.Width := Round(pW*VP.Scale);
  canvas.pen.Style := pS;
end;

constructor TShapeBase.Create;
begin
  pW:= 1;
  pS:= psSolid;
end;

{ TS2Line }

procedure TS2Line.Draw(canvas: TCanvas);
begin
  inherited Draw(canvas);
  canvas.Line(p1, p2);
end;


{ TS2Point }

function TS2Point.p1read: TPoint;
begin
  result:= VP.WtoS(p1r);
end;

function TS2Point.p2read: TPoint;
begin
  result:= VP.WtoS(p2r);
end;

procedure TS2Point.p1write(p: TPoint);
begin
  p1r:= VP.StoW(p);
end;

procedure TS2Point.p2write(p: TPoint);
begin
  p2r:= VP.StoW(p);
end;

procedure TS2Point.Point(point: TPoint; new: Boolean);
begin
  if new then begin
    p1:= point;
    p2:= p1;
  end
  else p2:= point;
end;

function TS2Point.BoundingRect: TRectReal;
begin
  Result.Left := Min(p1r.x, p2r.x);
  Result.Right := Max(p1r.x, p2r.x);
  Result.Top := Min(p1r.y, p2r.y);
  Result.Bottom := Max(p1r.y, p2r.y);
end;

procedure TS2Point.Select(aRect: TRect);
begin
  if (p1.x >= aRect.Left) and (p1.x <= aRect.Right) and
     (p2.x >= aRect.Left) and (p2.x <= aRect.Right) and
     (p1.y >= aRect.Top) and (p1.y <= aRect.Bottom) and
     (p2.y >= aRect.Top) and (p2.y <= aRect.Bottom) then sel:= true
  else sel:= false;
end;

procedure TS2Point.Draw(canvas: TCanvas);
const
  m = 5;
var
  width: integer;
  color: TColor;
begin
  width:= canvas.Pen.Width;
  color:= canvas.Pen.Color;
  if sel then begin
    canvas.pen.width:= 1;
    canvas.Pen.Color:= clGreen;
    canvas.Pen.Style:= psDash;
    canvas.Brush.Style:= bsClear;
    canvas.Rectangle(Min(p1.x, p2.x)-m, Min(p1.y, p2.y)-m, Max(p1.x, p2.x)+m, Max(p1.y, p2.y)+m);
  end;
  canvas.Pen.Width:= width;
  canvas.Pen.Color:= color;
  inherited Draw(canvas);
end;

end.
