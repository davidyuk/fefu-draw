unit DrawTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics, DrawShapes, Controls;

type

  { TScene }

  TScene = class
  private
    scene: array of TShapeBase;
    procedure addShape(shape: TShapeBase);
    function getLast(): TShapeBase;
  public
    procedure drawScene(canvas: TCanvas);
    constructor Create();
  end;

  { TToolBase }

  TToolBase = class
  public
    class procedure MMove(point: TPoint; isDrawing: boolean); virtual;
    class procedure MDown(point: TPoint; button: TMouseButton; penC, brushC: TColor; penW: integer);
      virtual; abstract;
    class procedure MUp(point: TPoint); virtual;
  end;

  ClassOfTool = class of TToolBase;
  ArrOfClassOfTool = array of ClassOfTool;
  ArrOfString = array of string;

  { TToolContainer }

  TToolContainer = class
  private
    tools: array of ClassOfTool;
    names: array of string;
    procedure addTool(t: ClassOfTool; n: string);
  public
    isEdit: boolean;
    property tool: ArrOfClassOfTool read tools;
    property name: ArrOfString read names;
  end;

  { TToolLine }

  TToolLine = class(TToolBase)
  public
    class procedure MDown(point: TPoint; button: TMouseButton; penC, brushC: TColor; penW: integer); override;
  end;

  { TToolRectangle }

  TToolRectangle = class(TToolBase)
  public
    class procedure MDown(point: TPoint; button: TMouseButton; penC, brushC: TColor; penW: integer); override;
  end;

  { TToolEllipse }

  TToolEllipse = class(TToolBase)
  public
    class procedure MDown(point: TPoint; button: TMouseButton; penC, brushC: TColor; penW: integer); override;
  end;

  { TToolFreeHand }

  TToolFreeHand = class(TToolBase)
  public
    class procedure MDown(point: TPoint; button: TMouseButton; penC, brushC: TColor; penW: integer); override;
  end;

  { TToolPolyline }

  TToolPolyline = class(TToolBase)
  public
    class procedure MDown(point: TPoint; button: TMouseButton; penC, brushC: TColor; penW: integer); override;
    class procedure MMove(point: TPoint; isDrawing: boolean); override;
  end;

var
  ToolContainer: TToolContainer;
  Scene: TScene;

implementation

{ TToolPolyline }

class procedure TToolPolyline.MDown(point: TPoint; button: TMouseButton; penC, brushC: TColor; penW: integer);
begin
  If button = mbRight Then begin ToolContainer.isEdit:=false; exit; end;
  If button = mbLeft Then begin
    if not ToolContainer.isEdit Then begin
      Scene.addShape(TShapePolyline.Create(point, penC, brushC, penW));
      ToolContainer.isEdit:=true;
    end else
    If Scene.getLast is TShapePolyline Then
      Scene.getLast.NewPoint(point);
  end;
end;

class procedure TToolPolyline.MMove(point: TPoint; isDrawing: boolean);
begin
  If ToolContainer.isEdit and (Scene.getLast is TShapePolyline) Then
    Scene.getLast.EditPoint(point);
end;

{ TToolEllipse }

class procedure TToolEllipse.MDown(point: TPoint; button: TMouseButton; penC, brushC: TColor; penW: integer);
begin
  Scene.addShape(TShapeEllipse.Create(point, penC, brushC, penW));
end;

{ TToolFreeHand }

class procedure TToolFreeHand.MDown(point: TPoint; button: TMouseButton; penC, brushC: TColor; penW: integer);
begin
  Scene.addShape(TShapeFreeHand.Create(point, penC, brushC, penW));
end;

{ TToolRectangle }

class procedure TToolRectangle.MDown(point: TPoint; button: TMouseButton; penC, brushC: TColor; penW: integer);
begin
  Scene.addShape(TShapeRectangle.Create(point, penC, brushC, penW));
end;

{ TToolLine }

class procedure TToolLine.MDown(point: TPoint; button: TMouseButton; penC, brushC: TColor; penW: integer);
begin
  Scene.addShape(TShapeLine.Create(point, penC, brushC, penW));
end;

{ TToolBase }

class procedure TToolBase.MMove(point: TPoint; isDrawing: boolean);
begin
  if not isDrawing then
    exit;
  Scene.getLast.NewPoint(point);
end;

class procedure TToolBase.MUp(point: TPoint);
begin
  {??}
end;

{ TScene }

procedure TScene.addShape(shape: TShapeBase);
begin
  setLength(scene, length(scene) + 1);
  scene[high(scene)] := shape;
end;

function TScene.getLast: TShapeBase;
begin
  if length(scene) = 0 Then begin result:= nil; exit; end;
  Result := Scene[high(Scene)];
end;

procedure TScene.drawScene(canvas: TCanvas);
var
  i: integer;
begin
  for i := 0 to high(self.scene) do
    self.scene[i].Draw(canvas);
end;

constructor TScene.Create;
begin
  setLength(scene, 0);
end;

{ TToolContainer }

procedure TToolContainer.addTool(t: ClassOfTool; n: string);
begin
  setLength(tools, length(tools) + 1);
  tools[high(tools)] := t;
  setLength(names, length(names) + 1);
  names[high(names)] := n;
end;

initialization

  ToolContainer := TToolContainer.Create;
  ToolContainer.addTool(TToolFreeHand, 'icons/FreePen.bmp');
  ToolContainer.addTool(TToolLine, 'icons/Line.bmp');
  ToolContainer.addTool(TToolRectangle, 'icons/Rectangle.bmp');
  ToolContainer.addTool(TToolEllipse, 'icons/Ellipse.bmp');
  ToolContainer.addTool(TToolPolyline, 'icons/Polyline.bmp');

end.
