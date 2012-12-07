unit DrawShapes;

{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics;

type
  TShapeBase = class
  public
    procedure Draw(canvas: TCanvas); virtual; abstract;
    procedure NewPoint(point: TPoint); virtual; abstract;
    procedure EditPoint(point: TPoint); virtual; abstract;
    constructor Create(point: TPoint; nPenC, nBrushC: TColor; nPenW: integer);
      virtual; abstract;
  end;

  TTwoPoint = class(TShapeBase)
  private
    pBegin, pEnd: TPoint;
    penC: TColor;
    penW: integer;
  public
    procedure Draw(canvas: TCanvas); override;
    procedure NewPoint(point: TPoint); override;
    constructor Create(point: TPoint; nPenC, nBrushC: TColor; nPenW: integer); virtual;
  end;

  TShapeLine = class(TTwoPoint)
  public
    procedure Draw(canvas: TCanvas); override;
  end;

  TShapeRectangle = class(TTwoPoint)
  private
    brushC: TColor;
  public
    procedure Draw(canvas: TCanvas); override;
    constructor Create(point: TPoint; nPenC, nBrushC: TColor; nPenW: integer);
      override;
  end;

  { TShapeEllipce }

  TShapeEllipse = class(TTwoPoint)
  private
    brushC: TColor;
  public
    procedure Draw(canvas: TCanvas); override;
    constructor Create(point: TPoint; nPenC, nBrushC: TColor; nPenW: integer);
      override;
  end;

  { TShapeFreeHand }

  TShapeFreeHand = class(TShapeBase)
  private
    penC: TColor;
    penW: integer;
    points: array of TPoint;
  public
    procedure Draw(canvas: TCanvas); override;
    procedure NewPoint(point: TPoint); override;
    constructor Create(
      point: TPoint; nPenC, nBrushC: TColor; nPenW: integer); override;
  end;

  { TShapePolyline }

  TShapePolyline = class(TShapeFreeHand)
  public
    procedure EditPoint(point: TPoint); override;
    procedure NewPoint(point: TPoint); override;
  end;

implementation

{ TShapePolyline }

procedure TShapePolyline.EditPoint(point: TPoint);
begin
  if high(points) = 0 Then Self.NewPoint(point);
  points[high(points)]:= point;
end;

procedure TShapePolyline.NewPoint(point: TPoint);
begin
  setLength(points, length(points)+1);
  points[high(points)]:= point;
end;

{ TShapeEllipce }

procedure TShapeEllipse.Draw(canvas: TCanvas);
begin
  inherited Draw(canvas);
  canvas.brush.Color := brushC;
  canvas.Ellipse(pBegin.x, pBegin.y, pEnd.x, pEnd.y);
end;

constructor TShapeEllipse.Create(point: TPoint; nPenC, nBrushC: TColor;
  nPenW: integer);
begin
  inherited Create(point, nPenC, nBrushC, nPenW);
  brushC := nBrushC;
end;

{ TShapeFreeHand }

procedure TShapeFreeHand.Draw(canvas: TCanvas);
begin
  canvas.pen.color := penC;
  canvas.pen.Width := penW;
  canvas.Line(points[0], points[0]);
  canvas.Polyline(points);
end;

procedure TShapeFreeHand.NewPoint(point: TPoint);
begin
  setLength(points, length(points)+1);
  points[high(points)] := point;
end;

constructor TShapeFreeHand.Create(point: TPoint; nPenC, nBrushC: TColor;
  nPenW: integer);
begin
  penC := nPenC;
  penW := nPenW;
  setLength(points, 1);
  points[0] := point;
end;

{ TShapeRectangle }

procedure TShapeRectangle.Draw(canvas: TCanvas);
begin
  inherited Draw(canvas);
  canvas.brush.Color := brushC;
  canvas.Rectangle(pBegin.x, pBegin.y, pEnd.x, pEnd.y);
end;

constructor TShapeRectangle.Create(point: TPoint; nPenC, nBrushC: TColor;
  nPenW: integer);
begin
  inherited Create(point, nPenC, nBrushC, nPenW);
  brushC := nBrushC;
end;

{ TShapeLine }

procedure TShapeLine.Draw(canvas: TCanvas);
begin
  inherited Draw(canvas);
  canvas.Line(pBegin, pEnd);
end;


{ TTwoPoint }

procedure TTwoPoint.Draw(canvas: TCanvas);
begin
  canvas.Pen.color := penC;
  canvas.Pen.Width := penW;
end;

procedure TTwoPoint.NewPoint(point: TPoint);
begin
  pEnd := point;
end;

constructor TTwoPoint.Create(point: TPoint; nPenC, nBrushC: TColor; nPenW: integer);
begin
  pBegin := point;
  pEnd := point;
  penC := nPenC;
  penW := nPenW;
end;

end.
