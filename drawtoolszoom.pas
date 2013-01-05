unit DrawToolsZoom;

{$mode objfpc}{$H+}{$M+}

interface

uses
  Classes, SysUtils, DrawTools, DrawShapes, Controls, Graphics , DrawZoom;

type

  TScale = type real;

  { TTScaleP }

  TTScaleP = class(TPersistent)
  private
    procedure SetScale(scale: TScale);
    function GetScale: TScale;
  published
    property scale: TScale read GetScale write SetScale;
  end;

  { TTScale }

  TTScale = class(TToolBase)
  private
    Param: TTScaleP;
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

  { TTShift }

  TTShift = class(TToolBase)
  private
    LastPoint: TPoint;
  public
    procedure MMove(point: TPoint; isDrawing: boolean; shift: TShiftState); override;
    procedure MDown(point: TPoint; shift: TShiftState); override;
    procedure MUp(point: TPoint; shift: TShiftState); override;
    function GetParamObj:TPersistent; override;
    function CreateParamObj:TPersistent; override;
  end;

implementation

{ TTShift }

procedure TTShift.MMove(point: TPoint; isDrawing: boolean; shift: TShiftState);
var t: TPoint;
begin
  if not isDrawing then exit;
  t.x:= LastPoint.x - point.x;
  t.y:= LastPoint.y - point.y;
  VP.SetWorldPosShift(t);
  LastPoint := point;
end;

procedure TTShift.MDown(point: TPoint; shift: TShiftState);
begin
  LastPoint := point;
end;

procedure TTShift.MUp(point: TPoint; shift: TShiftState);
begin

end;

function TTShift.GetParamObj: TPersistent;
begin
  Result:= nil;
end;

function TTShift.CreateParamObj: TPersistent;
begin
  Result:= nil;
end;

{ TTScaleP }

procedure TTScaleP.SetScale(scale: TScale);
begin
  VP.Scale:= scale;
end;

function TTScaleP.GetScale: TScale;
begin
  result:= VP.Scale;
end;

{ TTScale }

procedure TTScale.MMove(point: TPoint; isDrawing: boolean; shift: TShiftState);
begin
  if not isDrawing then
    exit;
  Scene.getLast.Point(point, false);
end;

procedure TTScale.MDown(point: TPoint; shift: TShiftState);
var
  Rec: TS2FRectangle;
begin
  Shape:= TS2FRectangle.Create;
  Scene.addShape(Shape);
  Rec:= TS2FRectangle(Shape);
  Rec.penC := clBlue;
  Rec.penS := psDot;
  Rec.brushS := bsDiagCross;
  Rec.brushC := $eeeeee;
  Shape.Point(point);
  p:= point;
  IsDown:= true;
end;

procedure TTScale.MUp(point: TPoint; shift: TShiftState);
begin
  if not IsDown then exit;
  Scene.delLastShape;
  Shape.Destroy;
  VP.ScaleTo(p, point);
  IsDown:= false;
end;

function TTScale.GetParamObj: TPersistent;
begin
  result:= Param;
end;

function TTScale.CreateParamObj: TPersistent;
begin
  Param := TTScaleP.Create;
  result:= Param;
end;


initialization

ToolContainer.addTool(TTScale, 'Увеличить');
ToolContainer.addTool(TTShift, 'Переместить холст');

end.

