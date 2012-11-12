unit DrawShape;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, ExtCtrls,
  ComCtrls, StdCtrls;

type
  Shape = (sCursor, sPen, sLine, sEllipse, sRectangle);

  PPaintBoxShape = ^PaintBoxShape;
  PaintBoxShape = class(TObject)
    Kind: Shape;
    Point: array of TPoint;
    LineSize: integer;
    PenColor: TColor;
    BrushColor: TColor;
    Name: String;
    IsVisible: boolean;
    Before, Next: PPaintBoxShape;
    procedure Paint(PaintBox: TPaintBox);
    procedure editPoint(x, y: integer; new: boolean);
  public
    constructor Create(newKind: Shape; newLineSize: integer; pen, brush: TColor);
  end;

var
  shapeCounter: array [Shape] of integer;

implementation

function shapeToString(shape: Shape):string;
begin
  //возвращает название типа
  case shape of
    sCursor: result:= 'Курсор';
    sPen: result:= 'Карандаш';
    sLine: result:= 'Линия';
    sEllipse: result:= 'Окружность';
    sRectangle: result:= 'Прямоугольник';
  end;
end;

constructor PaintBoxShape.Create(newKind: Shape; newLineSize: integer; pen, brush: TColor);
begin
  //инициализация объекта
  self.Kind := newKind;
  self.LineSize := newLineSize;
  self.penColor:=pen;
  self.brushColor:=brush;
  self.IsVisible:=True;
  inc(shapeCounter[newKind]);
  self.Name:=shapeToString(newKind)+' '+intToStr(shapeCounter[newKind]);
end;


procedure PaintBoxShape.Paint(PaintBox: TPaintBox);
var i: integer;
begin
  //метод объекта PaintBoxShape, рисующий объект
  if not(IsVisible) Then exit;
  PaintBox.Canvas.Pen.Width:=LineSize;
  PaintBox.Canvas.Pen.Color:=PenColor;
  PaintBox.Canvas.Brush.Color:=BrushColor;
  case Kind of
    sPen: begin
      PaintBox.Canvas.MoveTo(Point[0]);
      for i:= 1 to High(Point) do
        PaintBox.Canvas.LineTo(Point[i]);
    end;
    sLine: begin
      PaintBox.Canvas.MoveTo(Point[0]);
      PaintBox.Canvas.LineTo(Point[1]);
    end;
    sEllipse: PaintBox.Canvas.Ellipse(Point[0].X, Point[0].Y, Point[1].X, Point[1].Y);
    sRectangle: PaintBox.Canvas.Rectangle(Point[0].X, Point[0].Y, Point[1].X, Point[1].Y);
  end;
end;

procedure PaintBoxShape.editPoint(x, y: integer; new: boolean);
var newPoint: TPoint;
begin
  //изменяет последнюю точку в массиве или создаёт новую точку
  newPoint.x:= x;
  newPoint.y:= y;
  if new then setLength(self.Point, length(self.Point)+1);
  Point[High(Point)]:= newPoint;
end;

end.

