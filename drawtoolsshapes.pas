unit DrawToolsShapes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DrawShapes, Controls, DrawTools;

type
  { TTSLine }

  TTSLine = class(TTShape)
    function CreateParamObj:TPersistent; override;
  end;

  { TTSRectangle }

  TTSRectangle = class(TTShape)
    function CreateParamObj:TPersistent; override;
  end;

  { TTSEllipce }

  TTSEllipce = class(TTShape)
    function CreateParamObj:TPersistent; override;
  end;

  { TTSRectangleRound }

  TTSRectangleRound = class(TTShape)
    function CreateParamObj:TPersistent; override;
  end;

  { TTSFreeHand }

  TTSFreeHand = class(TTShape)
    function CreateParamObj:TPersistent; override;
    procedure MMove(point: TPoint; isDrawing: boolean); override;
  end;

  { TTSPolyline }

  TTSPolyline = class(TTShape)
    function CreateParamObj:TPersistent; override;
    procedure MDown(point: TPoint; button: TMouseButton); override;
    procedure MUp(point: TPoint); override;
  end;

implementation

{ TTSPolyline }

function TTSPolyline.CreateParamObj: TPersistent;
begin
  Shape:= TSMPoint.Create;
  Result:=inherited CreateParamObj;
end;

procedure TTSPolyline.MDown(point: TPoint; button: TMouseButton);
begin
  if button = mbRight Then begin isTemp := true; exit; end;
  if isTemp Then begin
    CreateParamObj;
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

function TTSFreeHand.CreateParamObj: TPersistent;
begin
  Shape:= TSMPoint.Create;
  Result:=inherited CreateParamObj;
end;

procedure TTSFreeHand.MMove(point: TPoint; isDrawing: boolean);
begin
  if not isDrawing then
    exit;
  Scene.getLast.Point(point);
end;

{ TTSRectangleRound }

function TTSRectangleRound.CreateParamObj: TPersistent;
begin
  Shape :=TS2FRectangleRound.Create;
  Result:=inherited CreateParamObj;
end;

{ TTSEllipce }

function TTSEllipce.CreateParamObj: TPersistent;
begin
  Shape := TS2FEllipce.Create;
  Result:=inherited CreateParamObj;
end;

{ TTSRectangle }

function TTSRectangle.CreateParamObj: TPersistent;
begin
  Shape := TS2FRectangle.Create;
  Result:=inherited CreateParamObj;
end;

{ TTSLine }

function TTSLine.CreateParamObj: TPersistent;
begin
  Shape := TS2Line.Create;
  Result := inherited CreateParamObj;
end;

initialization

ToolContainer.addTool(TTSFreeHand,'Карандаш');
ToolContainer.addTool(TTSLine,'Линия');
ToolContainer.addTool(TTSRectangle,'Прямоугольник');
ToolContainer.addTool(TTSEllipce,'Эллипс');
ToolContainer.addTool(TTSRectangleRound,'Прямоугольник со скругленными углами');
ToolContainer.addTool(TTSPolyline,'Ломоная');

end.

