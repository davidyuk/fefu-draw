unit DrawTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics, DrawShapes, Controls, DrawEditors;

type

  { TToolBase }

  TToolBase = class
  protected
    n: string;
  public
    property name: string read n;
    procedure MMove(point: TPoint; isDrawing: boolean); virtual; abstract;
    procedure MDown(point: TPoint; button: TMouseButton); virtual; abstract;
    procedure MUp(point: TPoint); virtual; abstract;
    function GetShape:TShapeBase; virtual; abstract;
    function Select:TShapeBase; virtual; abstract;
    constructor Create(nName: string); virtual;
  end;

  { TTShape }

  TTShape = class(TToolBase)
  protected
    Shape: TShapeBase;
    isTemp: boolean;
  public
    procedure MMove(point: TPoint; isDrawing: boolean); override;
    procedure MDown(point: TPoint; button: TMouseButton); override;
    procedure MUp(point: TPoint); override;
    function GetShape:TShapeBase; override;
    function Select:TShapeBase; override;
    constructor Create(nName: string); override;
  end;

  ClassOfTool = class of TToolBase;
  ArrOfTool = array of TToolBase;

  { TToolContainer }

  TToolContainer = class
  private
    tArr: array of TToolBase;
  public
    procedure addTool(t: ClassOfTool; n: string);
    property tool: ArrOfTool read tArr;
  end;

  { TScene }

  TScene = class
  private
    scene: array of TShapeBase;
  public
    procedure addShape(shape: TShapeBase);
    function getLast(): TShapeBase;
    procedure Draw(canvas: TCanvas);
  end;

var
  ToolContainer: TToolContainer;
  Scene: TScene;

implementation

{ TTShape }

procedure TTShape.MMove(point: TPoint; isDrawing: boolean);
begin
  if not isDrawing then
    exit;
  Scene.getLast.Point(point, false);
end;

procedure TTShape.MDown(point: TPoint; button: TMouseButton);
begin
  Scene.addShape(Shape);
  isTemp:= false;
  Shape.Point(point);
end;

procedure TTShape.MUp(point: TPoint);
begin
  //у фигур нечего делать на MouseUp
end;

function TTShape.GetShape: TShapeBase;
begin
  Result:= Shape;
end;

function TTShape.Select: TShapeBase;
begin
  Result:= Shape;
end;

constructor TTShape.Create(nName: string);
begin
  inherited Create(nName);
  isTemp:= true;
end;

{ TToolBase }

constructor TToolBase.Create(nName: string);
begin
  n:= nName;
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

procedure TScene.Draw(canvas: TCanvas);
var
  i: integer;
begin
  for i := 0 to high(self.scene) do
    self.scene[i].Draw(canvas);
end;

{ TToolContainer }

procedure TToolContainer.addTool(t: ClassOfTool; n: string);
begin
  setLength(tArr, length(tArr) + 1);
  tArr[high(tArr)] := t.Create(n);
end;

initialization


end.
