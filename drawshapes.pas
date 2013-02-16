unit DrawShapes;

{$mode objfpc}{$H+}{$M+}

interface

uses
  Classes, Graphics, FPCanvas, Math, DrawZoom, DrawTypes;

const
  SelMargin = 5;

type

  { TShapeBase }

  TShapeBase = class(TPersistent)
  private
    pC: TColor;
    pW: Integer;
    pS: TFPPenStyle;
    sel: boolean;
    pointArr: array of TPointReal;
  public
    procedure Draw(canvas: TCanvas); virtual;
    procedure Point(point: TPoint; new: Boolean = true); virtual; abstract;
    function BoundingRect:TRectReal; virtual; abstract;
    property Selected: boolean read sel write sel;
    procedure Select(aRect: TRect); virtual; abstract;
    function PointOnShape(p: TPoint):boolean; virtual; abstract;
    constructor Create; virtual;
    property Points: TPointRealArr read pointArr write pointArr;
    procedure setPointsLength(i: integer);
  published
    property penC: TColor read pC write pC;
    property penW: Integer read pW write pW;
    property penS: TFPPenStyle read pS write pS;
  end;

  { TS2Point }

  TS2Point = class(TShapeBase)
  private
    function p1read:TPoint;
    function p2read:TPoint;
    procedure p1write(p: TPoint);
    procedure p2write(p: TPoint);
    property p1: TPoint read p1read write p1write;
    property p2: TPoint read p2read write p2write;
  public
    procedure Point(point: TPoint; new: Boolean = true); override;
    function BoundingRect:TRectReal; override;
    procedure Draw(canvas: TCanvas); override;
    constructor Create; override;
    function PointOnShape(p: TPoint):boolean; override;
  end;

  { TS2Line }

  TS2Line = class(TS2Point)
  public
    procedure Draw(canvas: TCanvas); override;
    procedure Select(aRect: TRect); override;
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
    procedure Select(aRect: TRect); override;
  end;

  { TS2FEllipce }

  TS2FEllipce = class(TS2Fill)
  private
    invisibleBorder: array of TPointReal;
    size, pos: TPointReal;
    procedure ReCalculateBorder;
  public
    procedure Draw(canvas: TCanvas); override;
    procedure Select(aRect: TRect); override;
  end;

  { TS2FRectangleRound }

  TS2FRectangleRound = class(TS2Fill)
  private
    r: integer;
  published
    property radius: integer read r write r;
  public
    procedure Draw(canvas: TCanvas); override;
    procedure Select(aRect: TRect); override;
  end;

  { TSMPoint }

  TSMPoint = class(TShapeBase)
  public
    procedure Point(point: TPoint; new: Boolean = true); override;
    procedure Draw(canvas: TCanvas); override;
    procedure Select(aRect: TRect); override;
    function BoundingRect:TRectReal; override;
    function PointOnShape(p: TPoint):boolean; override;
  end;

implementation

uses types;

procedure DrawSelect(canvas: TCanvas; boundingRect: TRectReal);
var
  t: TRect;
  m: integer;
begin
  canvas.pen.width:= 1;
  canvas.Pen.Color:= clGreen;
  canvas.Pen.Style:= psDash;
  canvas.Brush.Style:= bsClear;
  t:= VP.WtoS(BoundingRect);
  m:= Round(SelMargin*VP.Scale);
  canvas.Rectangle(t.Left-m, t.Top-m, t.Right+m, t.Bottom+m);
end;

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
    setLength(pointArr, length(pointArr)+1);
    pointArr[high(pointArr)]:= VP.StoW(point);
  end else begin
    if length(pointArr) = 1 Then setLength(pointArr, 2);
    points[high(pointArr)]:= VP.StoW(point);
  end;
end;

procedure TSMPoint.Draw(canvas: TCanvas);
var i: integer;
begin
  inherited Draw(canvas);
  canvas.MoveTo(VP.WtoS(points[0])); //может стоит проверить есть-ли в массиве элементы
  for i:= 0 to high(points) do
    canvas.LineTo(VP.WtoS(points[i]));
  if sel then DrawSelect(canvas, BoundingRect);
end;

procedure TSMPoint.Select(aRect: TRect);
var
  i: integer;
begin
  sel:= false;
  for i:= 0 to High(pointArr)-1 do
    if Intersection(aRect, VP.WToS(pointArr[i]), VP.WtoS(pointArr[i+1])) Then begin
      sel:= true;
      exit;
    end;
end;

function TSMPoint.BoundingRect: TRectReal;
var i: integer;
begin
  Result.Left:= points[0].X;
  Result.Right:= points[0].X;
  Result.Top:= points[0].Y;
  Result.Bottom:= points[0].Y;
  for i:= 1 to high(points) do begin
    Result.Left:= Min(points[i].X, Result.Left);
    Result.Right:= Max(points[i].X, Result.Right);
    Result.Top:= Min(points[i].Y, Result.Top);
    Result.Bottom:= Max(points[i].Y, Result.Bottom);
  end;
end;

function TSMPoint.PointOnShape(p: TPoint): boolean;
var
  rect: TRect;
begin
  rect:= VP.WtoS(BoundingRect);
  if (p.x >= rect.Left) and
     (p.x <= rect.Right) and
     (p.y >= rect.Top) and
     (p.y <= rect.Bottom) Then Result:= true
  else Result:= false;
end;

{ TS2FRectangleRound }

procedure TS2FRectangleRound.Draw(canvas: TCanvas);
begin
  inherited Draw(canvas);
  canvas.RoundRect(p1.x, p1.y, p2.x, p2.y, r, r);
end;

procedure TS2FRectangleRound.Select(aRect: TRect);
begin
  sel:= false;
  if (abs(arect.right - arect.left) < 2) and (abs(arect.bottom - arect.top) < 2) Then exit;
  sel:= Intersection(aRect, DrawTypes.Rect(p1, p2));
end;

{ TS2FEllipce }

procedure TS2FEllipce.ReCalculateBorder;
const
  quality = 101;
var
  i: integer;
  t1, t2, aspectRatio: double;
  center: TPointReal;
begin
  SetLength(invisibleBorder, quality);
  pos.x:= min(pointArr[0].x, pointArr[1].x); //левый верхний угол фигуры
  pos.y:= min(pointArr[0].y, pointArr[1].y);
  size.x:= abs(pointArr[0].x-pointArr[1].x); //размер фигуры
  size.y:= abs(pointArr[0].y-pointArr[1].y);
  center.x:= pos.x + size.x/2; //абсолютное положение центра
  center.y:= pos.y + size.y/2;
  aspectRatio:= size.Y/size.X;
  t1:= sqr(size.x/2);
  for i:= 0 to 50 do begin
    t2:= size.x/50*i;
    invisibleBorder[50+i].x := pos.x+t2;
    invisibleBorder[50-i].x := pos.x+t2;
    t2:= sqr(t2 - size.X/2);
    invisibleBorder[50+i].y := center.y + sqrt(t1-t2)*aspectRatio;
    invisibleBorder[50-i].y := center.y - sqrt(t1-t2)*aspectRatio;
  end;
end;

procedure TS2FEllipce.Draw(canvas: TCanvas);
var
  i: integer;
  t: TPoint;
begin
  inherited Draw(canvas);
  canvas.Ellipse(p1.x, p1.y, p2.x, p2.y);
  //show invisible border
  {if length(invisibleBorder) = 0 then exit;
  canvas.Pen.Width := 1;
  canvas.Pen.Color := clGreen;
  canvas.Pen.Style := psSolid;
  t:= VP.WtoS(invisibleBorder[0]);
  canvas.MoveTo(t.x, t.y);
  for i:= 1 to high(invisibleBorder) do begin
    t:= VP.WtoS(invisibleBorder[i]);
    canvas.LineTo(t.x, t.y);
  end;}
end;

procedure TS2FEllipce.Select(aRect: TRect);
var
  i: integer;
begin
  if (size.x <> abs(pointArr[0].x-pointArr[1].x)) or
     (size.y <> abs(pointArr[0].y-pointArr[1].y)) or
     (pos.x <> min(pointArr[0].x, pointArr[1].x)) or
     (pos.y <> min(pointArr[0].y, pointArr[1].y)) Then
    ReCalculateBorder;
  sel:= false;
  for i:= 0 to High(invisibleBorder)-1 do
    if Intersection(aRect, VP.WToS(invisibleBorder[i]), VP.WtoS(invisibleBorder[i+1])) Then begin
      sel:= true;
      exit;
    end;
  {size.x:= Max(p1.x, p2.x) - Min(p1.x, p2.x);
  size.y:= Max(p1.y, p2.y) - Min(p1.y, p2.y);
  pos.x := Min(p1.x, p2.x) + size.x div 2;
  pos.y := Min(p1.y, p2.y) + size.y div 2;
  sel := IntersectionWithEllipse(aRect.TopLeft, aRect.BottomRight, pos, size);}
end;

{ TS2FRectangle }

procedure TS2FRectangle.Draw(canvas: TCanvas);
begin
  inherited Draw(canvas);
  canvas.Rectangle(p1.x, p1.y, p2.x, p2.y);
end;

procedure TS2FRectangle.Select(aRect: TRect);
begin
  sel:= false;
  if (abs(arect.right - arect.left) < 2) and (abs(arect.bottom - arect.top) < 2) Then exit;
  sel:= Intersection(aRect, DrawTypes.Rect(p1, p2));
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

procedure TShapeBase.setPointsLength(i: integer);
begin
  setLength(pointArr, i);
end;

{ TS2Line }

procedure TS2Line.Draw(canvas: TCanvas);
begin
  inherited Draw(canvas);
  canvas.Line(p1, p2);
end;

procedure TS2Line.Select(aRect: TRect);
begin
  sel:= Intersection(aRect, p1, p2);
end;


{ TS2Point }

function TS2Point.p1read: TPoint;
begin
  result:= VP.WtoS(pointArr[0]);
end;

function TS2Point.p2read: TPoint;
begin
  result:= VP.WtoS(pointArr[1]);
end;

procedure TS2Point.p1write(p: TPoint);
begin
  pointArr[0]:= VP.StoW(p);
end;

procedure TS2Point.p2write(p: TPoint);
begin
  pointArr[1]:= VP.StoW(p);
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
  Result.Left := Min(pointArr[0].x, pointArr[1].x);
  Result.Right := Max(pointArr[0].x, pointArr[1].x);
  Result.Top := Min(pointArr[0].y, pointArr[1].y);
  Result.Bottom := Max(pointArr[0].y, pointArr[1].y);
end;

procedure TS2Point.Draw(canvas: TCanvas);
var
  width: integer;
  color: TColor;
begin
  width:= canvas.Pen.Width;
  color:= canvas.Pen.Color;
  if sel then DrawSelect(canvas, BoundingRect);
  canvas.Pen.Width:= width;
  canvas.Pen.Color:= color;
  inherited Draw(canvas);
end;

constructor TS2Point.Create;
begin
  inherited Create;
  setLength(pointArr, 2);
end;

function TS2Point.PointOnShape(p: TPoint): boolean;
var rect: TRect;
begin
  rect:= VP.WtoS(BoundingRect);
  if (p.x >= rect.Left) and (p.x <= rect.Right) and
     (p.y >= rect.Top) and (p.y <= rect.Bottom) then Result := true
  else Result := false;
end;

initialization

RegisterClass(TS2FEllipce);
RegisterClass(TS2FRectangle);
RegisterClass(TS2FRectangleRound);
RegisterClass(TS2Line);
RegisterClass(TSMPoint);

end.
