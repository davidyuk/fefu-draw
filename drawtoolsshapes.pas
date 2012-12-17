unit DrawToolsShapes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DrawShapes, DrawTools, Controls;

type
  { TTSLine }

  TTSLine = class(TTShape)
    function CreateShape:TShapeBase; override;
  end;

  { TTSRectangle }

  TTSRectangle = class(TTShape)
    function CreateShape:TShapeBase; override;
  end;

  { TTSEllipce }

  TTSEllipce = class(TTShape)
    function CreateShape:TShapeBase; override;
  end;

  { TTSRectangleRound }

  TTSRectangleRound = class(TTShape)
    function CreateShape:TShapeBase; override;
  end;

  { TTSFreeHand }

  TTSFreeHand = class(TTShape)
    function CreateShape:TShapeBase; override;
    procedure MMove(point: TPoint; isDrawing: boolean); override;
  end;

  { TTSPolyline }

  TTSPolyline = class(TTShape)
    function CreateShape:TShapeBase; override;
    procedure MDown(point: TPoint; button: TMouseButton); override;
    procedure MUp(point: TPoint); override;
  end;

implementation

{ TTSPolyline }

function TTSPolyline.CreateShape: TShapeBase;
begin
  Shape:= TSMPoint.Create;
  Result:=inherited CreateShape;
end;

procedure TTSPolyline.MDown(point: TPoint; button: TMouseButton);
begin
  if button = mbRight Then begin isTemp := true; exit; end;
  if isTemp Then begin
    CreateShape;
    Scene.addShape(Shape);
    isTemp:= false;
  end;
  Shape.Point(point);
end;

procedure TTSPolyline.MUp(point: TPoint);
begin
  //Не нужно создавать новую фигуру
end;

{ TTSFreeHand }

function TTSFreeHand.CreateShape: TShapeBase;
begin
  Shape:= TSMPoint.Create;
  Result:=inherited CreateShape;
end;

procedure TTSFreeHand.MMove(point: TPoint; isDrawing: boolean);
begin
  if not isDrawing then
    exit;
  Scene.getLast.Point(point);
end;

{ TTSRectangleRound }

function TTSRectangleRound.CreateShape: TShapeBase;
begin
  Shape :=TS2FRectangleRound.Create;
  Result:=inherited CreateShape;
end;

{ TTSEllipce }

function TTSEllipce.CreateShape: TShapeBase;
begin
  Shape := TS2FEllipce.Create;
  Result:=inherited CreateShape;
end;

{ TTSRectangle }

function TTSRectangle.CreateShape: TShapeBase;
begin
  Shape := TS2FRectangle.Create;
  Result:=inherited CreateShape;
end;

{ TTSLine }

function TTSLine.CreateShape: TShapeBase;
begin
  Shape := TS2Line.Create;
  Result := inherited CreateShape;
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

