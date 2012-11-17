unit DrawShape;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, ExtCtrls;

type
  Shape = (sEllipse, sRectangle, sLine, sPen, sCursor); //добавить

  { TFBase }

  TFBase = class
  public
    lineWidth: integer;
    penColor, brushColor: TColor;
    name: String;
    isVisible: boolean;
    procedure Paint(Canvas: TCanvas); virtual; abstract;
    procedure Point(p: TPoint); virtual; abstract;
    constructor Create(fName: string; line: integer; pen, brush: TColor; dot: TPoint); virtual;
  end;

  TFUtil = class
  const
    shapeN: array [Shape] of string = ('Эллипс','Прямоугольник','Отрезок','Карандаш','Курсор');
  public
    shapeC: array [Shape] of integer;
    function createShape(shape: Shape; line: integer; pen, brush: TColor; dot: TPoint):TFBase;
  end;

  { TFPen }

  TFPen = class(TFBase)
  public
    arr: array of TPoint;
    procedure Paint(Canvas: TCanvas); override;
    procedure Point(p: TPoint); override;
    constructor Create(fName: string; line: integer; pen, brush: TColor; dot: TPoint); override;
  end;

  { TFLine }

  TFLine = class(TFBase)
  public
    b, e: TPoint;
    procedure Paint(Canvas: TCanvas); override;
    procedure Point(p: TPoint); override;
    constructor Create(fName: string; line: integer; pen, brush: TColor; dot: TPoint); override;
  end;

  { TFRectangle }

  TFRectangle = class(TFBase)
  public
    b, e: TPoint;
    procedure Paint(Canvas: TCanvas); override;
    procedure Point(p: TPoint); override;
    constructor Create(fName: string; line: integer; pen, brush: TColor; dot: TPoint); override;
  end;

  { TFEllipse }

  TFEllipse = class(TFBase)
  public
    b, e: TPoint;
    procedure Paint(Canvas: TCanvas); override;
    procedure Point(p: TPoint); override;
    constructor Create(fName: string; line: integer; pen, brush: TColor; dot: TPoint); override;
  end;


implementation

{ TFEllipse }

procedure TFEllipse.Paint(Canvas: TCanvas);
begin
  if not(isVisible) Then exit;
  Canvas.Pen.Width:=lineWidth;
  Canvas.Pen.Color:=penColor;
  Canvas.Brush.Color:=brushColor;
  Canvas.Ellipse(b.x, b.y, e.x, e.y);
end;

procedure TFEllipse.Point(p: TPoint);
begin
  e:= p;
end;

constructor TFEllipse.Create(fName: string; line: integer; pen, brush: TColor;
  dot: TPoint);
begin
  inherited Create(fName, line, pen, brush, dot);
  b:= dot;
  e:= dot;
end;

{ TFRectangle }

procedure TFRectangle.Paint(Canvas: TCanvas);
begin
  if not(isVisible) Then exit;
  Canvas.Pen.Width:=lineWidth;
  Canvas.Pen.Color:=penColor;
  Canvas.Brush.Color:=brushColor;
  Canvas.Rectangle(b.x, b.y, e.x, e.y);
end;

procedure TFRectangle.Point(p: TPoint);
begin
  e:= p;
end;

constructor TFRectangle.Create(fName: string; line: integer; pen,
  brush: TColor; dot: TPoint);
begin
  inherited Create(fName, line, pen, brush, dot);
  b:= dot;
  e:= dot;
end;

{ TFLine }

procedure TFLine.Paint(Canvas: TCanvas);
begin
  if not(isVisible) Then exit;
  Canvas.Pen.Width:=lineWidth;
  Canvas.Pen.Color:=penColor;
  Canvas.Brush.Color:=brushColor;
  Canvas.MoveTo(b);
  Canvas.LineTo(e);
end;

procedure TFLine.Point(p: TPoint);
begin
  e:= p;
end;

constructor TFLine.Create(fName: string; line: integer; pen, brush: TColor;
  dot: TPoint);
begin
  inherited Create(fName, line, pen, brush, dot);
  b:= dot;
  e:= dot;
end;

function TFUtil.createShape(shape: Shape; line: integer; pen, brush: TColor; dot: TPoint):TFBase;
var t: string;
begin
  //возвращает объект - фигуру
  inc(shapeC[shape]);
  t:= shapeN[shape]+' '+intToStr(shapeC[shape]);
  case shape of
    sPen: result:= TFPen.Create(t,line, pen, brush, dot);
    sLine: result:= TFLine.Create(t,line, pen, brush, dot);
    sRectangle: result:= TFRectangle.Create(t,line, pen, brush, dot);
    sEllipse: result:= TFEllipse.Create(t,line, pen, brush, dot);
  end;
end;

procedure TFPen.Paint(Canvas: TCanvas);
begin
  if not(isVisible) Then exit;
  Canvas.Pen.Width:=lineWidth;
  Canvas.Pen.Color:=penColor;
  Canvas.Brush.Color:=brushColor;
  Canvas.Polyline(arr);
  //Canvas.Polygon(arr);
end;

procedure TFPen.Point(p: TPoint);
begin
  setLength(arr, length(arr)+1);
  arr[high(arr)]:= p;
end;

constructor TFPen.Create(fName: string; line: integer; pen, brush: TColor;
  dot: TPoint);
begin
  inherited Create(fName, line, pen, brush, dot);
  setLength(arr, 1);
  arr[0]:= dot;
end;

constructor TFBase.Create(fName: string; line: integer; pen, brush: TColor;
  dot: TPoint);
begin
  self.name:= fName;
  self.lineWidth:= line;
  self.penColor:= pen;
  self.brushColor:= brush;
  self.isVisible:= true;
end;

end.

