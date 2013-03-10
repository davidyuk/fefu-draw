unit DrawToolsMouse;

{$mode objfpc}{$H+}

interface

uses
  Classes, DrawTools, Graphics, DrawShapes, Math;

type

  { TTMouse }

  TTMouseMode = (tmmNone, tmmSelect, tmmRightB, tmmMove);

  TTMouse = class(TToolBase)
  private
    Shape: TShapeBase;
    mode: TTMouseMode;
    isSelected: boolean;
    p: TPoint;
  public
    procedure MMove(nP: TPoint; isDrawing: boolean; shift: TShiftState); override;
    procedure MDown(point: TPoint; shift: TShiftState); override;
    procedure MUp(point: TPoint; shift: TShiftState); override;
    procedure Leave; override;
    function GetParamObj:TPersistent; override;
    function CreateParamObj:TPersistent; override;
  end;

implementation

uses
  Main, DrawScene;

{ TTMouse }

procedure TTMouse.MMove(nP: TPoint; isDrawing: boolean; shift: TShiftState);
var
  i:integer;
  t: TRect;
begin
  If mode = tmmSelect then begin
    Scene.getLast.Point(nP, false);
    t.Left:= Min(p.x, nP.x);
    t.Right:= max(p.x, nP.x);
    t.Top:= Min(p.y, nP.y);
    t.Bottom:= Max(p.y, nP.y);
    for i:= 0 to High(Scene.Shapes)-1 do
      Scene.Shapes[i].Select(t);
  end;
  if mode = tmmMove then begin
    Scene.ShapeSelShift(Point(nP.x-p.x, nP.y-p.y));
    p:= nP;
  end;
end;

procedure TTMouse.MDown(point: TPoint; shift: TShiftState);
var
  Rec: TS2FRectangle;
  i: integer;
begin
  MainF.ShapeDeleteMI.Enabled := isSelected;
  MainF.ShapeZindexMI.Enabled := isSelected;
  if ssRight in shift Then begin
    mode := tmmRightB;
    exit;
  end;
  for i:= 0 to high(Scene.Shapes) do
    if Scene.Shapes[i].Selected and Scene.Shapes[i].PointOnShape(point) then begin
      p:= point;
      mode:= tmmMove;
      exit;
    end;
  Shape:= TS2FRectangle.Create;
  Scene.addShape(Shape);
  Rec:= TS2FRectangle(Shape);
  Rec.penC := clRed;
  Rec.penS := psDot;
  Rec.brushS := bsClear;
  Rec.brushC := $eeeeee;
  Shape.Point(point);
  p:= point;
  mode:= tmmSelect;
end;

procedure TTMouse.MUp(point: TPoint; shift: TShiftState);
var
  i:integer;
  t: TRect;
begin
  if mode = tmmSelect then begin
    Scene.delLastShape;
    Shape.Destroy;
    t.Left:= Min(p.x, point.x);
    t.Right:= max(p.x, point.x);
    t.Top:= Min(p.y, point.y);
    t.Bottom:= Max(p.y, point.y);
    for i:= 0 to High(Scene.Shapes) do
      Scene.Shapes[i].Select(t);
    for i:= 0 to high(Scene.Shapes) do
      if Scene.Shapes[i].Selected Then begin
        isSelected := true;
        MainF.LoadSelectedShapes;
        break;
      end;
  end;
  mode := tmmNone;
end;

procedure TTMouse.Leave;
begin
  inherited Leave;
  MainF.ShapeEditPM.AutoPopup := false;
  mode:= tmmNone;
end;

function TTMouse.GetParamObj: TPersistent;
begin
  result:= nil;
end;

function TTMouse.CreateParamObj: TPersistent;
begin
  MainF.ShapeEditPM.AutoPopup := true;
  result:= nil;
end;

initialization

ToolContainer.addTool(TTMouse, 'Выделение');

end.

