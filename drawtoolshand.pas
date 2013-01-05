unit DrawToolsHand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DrawTools, Graphics, DrawShapes;

type

  { TTHand }

  TTHand = class(TToolBase)
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

{ TTHand }

procedure TTHand.MMove(point: TPoint; isDrawing: boolean; shift: TShiftState);
begin
  if not isDrawing then
    exit;
  Scene.getLast.Point(point, false);
end;

procedure TTHand.MDown(point: TPoint; shift: TShiftState);
var
  Rec: TS2FRectangle;
begin
  Shape:= TS2FRectangle.Create;
  Scene.addShape(Shape);
  Rec:= TS2FRectangle(Shape);
  Rec.penC := clRed;
  Rec.penS := psDot;
  Rec.brushS := bsDiagCross;
  Rec.brushC := $eeeeee;
  Shape.Point(point);
  p:= point;
  IsDown:= true;
end;

procedure TTHand.MUp(point: TPoint; shift: TShiftState);
begin
  if not IsDown then exit;
  Scene.delLastShape;
  //Shape.Destroy;
  //VP.ScaleTo(p, point);
end;

function TTHand.GetParamObj: TPersistent;
begin
  result:= nil;
end;

function TTHand.CreateParamObj: TPersistent;
begin
  result:= nil;
end;

initialization

ToolContainer.addTool(TTHand, 'Рука');

end.

