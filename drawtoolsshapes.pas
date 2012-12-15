unit DrawToolsShapes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DrawShapes, DrawTools, Controls;

type
  { TTSLine }

  TTSLine = class(TTShape)
    function Select:TShapeBase; override;
  end;

  { TTSRectangle }

  TTSRectangle = class(TTShape)
    function Select:TShapeBase; override;
  end;

  { TTSEllipce }

  TTSEllipce = class(TTShape)
    function Select:TShapeBase; override;
  end;

  { TTSRectangleRound }

  TTSRectangleRound = class(TTShape)
    function Select:TShapeBase; override;
  end;

  { TTSFreeHand }

  TTSFreeHand = class(TTShape)
    function Select:TShapeBase; override;
    procedure MMove(point: TPoint; isDrawing: boolean); override;
  end;

  { TTSPolyline }

  TTSPolyline = class(TTShape)
    function Select:TShapeBase; override;
    procedure MDown(point: TPoint; button: TMouseButton); override;
  end;

implementation

{ TTSPolyline }

function TTSPolyline.Select: TShapeBase;
begin
  Shape:= TSMPoint.Create;
  Result:=inherited Select;
end;

procedure TTSPolyline.MDown(point: TPoint; button: TMouseButton);
begin
  if button = mbRight Then begin isTemp := true; exit; end;
  if isTemp Then begin
    Shape:= TSMPoint.Create;
    Scene.addShape(Shape);
    isTemp:= false;
  end;
  Shape.Point(point);
end;

{ TTSFreeHand }

function TTSFreeHand.Select: TShapeBase;
begin
  Shape:= TSMPoint.Create;
  Result:=inherited Select;
end;

procedure TTSFreeHand.MMove(point: TPoint; isDrawing: boolean);
begin
  if not isDrawing then
    exit;
  Scene.getLast.Point(point);
end;

{ TTSRectangleRound }

function TTSRectangleRound.Select: TShapeBase;
begin
  Shape :=TS2FRectangleRound.Create;
  Result:=inherited Select;
end;

{ TTSEllipce }

function TTSEllipce.Select: TShapeBase;
begin
  Shape := TS2FEllipce.Create;
  Result:=inherited Select;
end;

{ TTSRectangle }

function TTSRectangle.Select: TShapeBase;
begin
  Shape := TS2FRectangle.Create;
  Result:=inherited Select;
end;

{ TTSLine }

function TTSLine.Select: TShapeBase;
begin
  Shape := TS2Line.Create;
  Result := inherited Select;
end;

initialization
  ToolContainer := TToolContainer.Create;
  ToolContainer.addTool(TTSFreeHand, 'Карандаш');
  ToolContainer.addTool(TTSLine, 'Линия');
  ToolContainer.addTool(TTSRectangle, 'Прямоугольник');
  ToolContainer.addTool(TTSRectangleRound, 'Прямоугольник со скруглёнными углами');
  ToolContainer.addTool(TTSEllipce, 'Эллипс');
  ToolContainer.addTool(TTSPolyline, 'Ломоная');

end.

