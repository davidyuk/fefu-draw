unit DrawToolsMouse;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DrawTools, Graphics, DrawShapes, Math;

type

  { TTMouse }

  TTMouse = class(TToolBase)
  private
    Shape: TShapeBase;
    IsDown: boolean;
    p: TPoint;
  public
    procedure MMove(point: TPoint; isDrawing: boolean; shift: TShiftState); override;
    procedure MDown(point: TPoint; shift: TShiftState); override;
    procedure MUp(point: TPoint; shift: TShiftState); override;
    function GetParamObj:TPersistent; override;
    function CreateParamObj:TPersistent; override;
  end;

implementation

uses
  Main;

{ TTMouse }

procedure TTMouse.MMove(point: TPoint; isDrawing: boolean; shift: TShiftState);
var
  i:integer;
  t: TRect;
begin
  if not isDrawing then
    exit;
  Scene.getLast.Point(point, false);
  t.Left:= Min(p.x, point.x);
  t.Right:= max(p.x, point.x);
  t.Top:= Min(p.y, point.y);
  t.Bottom:= Max(p.y, point.y);
  for i:= 0 to High(Scene.Shapes)-1 do
    Scene.Shapes[i].Select(t);
end;

procedure TTMouse.MDown(point: TPoint; shift: TShiftState);
var
  Rec: TS2FRectangle;
begin
  Shape:= TS2FRectangle.Create;
  Scene.addShape(Shape);
  Rec:= TS2FRectangle(Shape);
  Rec.penC := clRed;
  Rec.penS := psDot;
  Rec.brushS := bsClear;
  Rec.brushC := $eeeeee;
  Shape.Point(point);
  p:= point;
  IsDown:= true;
end;

procedure TTMouse.MUp(point: TPoint; shift: TShiftState);
var
  i:integer;
  t: TRect;
begin
  if not IsDown then exit;
  Scene.delLastShape;
  Shape.Destroy;

  t.Left:= Min(p.x, point.x);
  t.Right:= max(p.x, point.x);
  t.Top:= Min(p.y, point.y);
  t.Bottom:= Max(p.y, point.y);
  for i:= 0 to High(Scene.Shapes) do
    Scene.Shapes[i].Select(t);
  MainF.LoadSelectedShapes;

  IsDown:= false;
end;

function TTMouse.GetParamObj: TPersistent;
begin
  result:= nil;
end;

function TTMouse.CreateParamObj: TPersistent;
begin
  result:= nil;
end;

initialization

ToolContainer.addTool(TTMouse, 'Рука');

end.

