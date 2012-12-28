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
    procedure MMove(point: TPoint; isDrawing: boolean; shift: TShiftState); virtual; abstract;
    procedure MDown(point: TPoint; shift: TShiftState); virtual; abstract;
    procedure MUp(point: TPoint; shift: TShiftState); virtual; abstract;
    function GetParamObj:TPersistent; virtual; abstract;
    function CreateParamObj:TPersistent; virtual; abstract;
    constructor Create(nName: string); virtual;
  end;

  { TTShape }

  TTShape = class(TToolBase)
  protected
    Shape: TShapeBase;
    isTemp: boolean;
  public
    procedure MMove(point: TPoint; isDrawing: boolean; shift: TShiftState); override;
    procedure MDown(point: TPoint; shift: TShiftState); override;
    procedure MUp(point: TPoint; shift: TShiftState); override;
    function GetParamObj:TPersistent; override;
    function CreateParamObj:TPersistent; override;
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

  ArrOfShape = array of TShapeBase;

  { TScene }

  TScene = class
  private
    scene: ArrOfShape;
  public
    property Shapes: ArrOfShape read scene;
    procedure addShape(shape: TShapeBase);
    function getLast(): TShapeBase;
    procedure delLastShape;
    procedure Draw(canvas: TCanvas);
  end;

var
  ToolContainer: TToolContainer;
  Scene: TScene;

implementation
{ TTShape }

procedure TTShape.MMove(point: TPoint; isDrawing: boolean; shift: TShiftState);
begin
  if not isDrawing then
    exit;
  Shape.Point(point, false);
end;

procedure TTShape.MDown(point: TPoint; shift: TShiftState);
begin
  Scene.addShape(Shape);
  isTemp:= false;
  Shape.Point(point);
end;

procedure TTShape.MUp(point: TPoint; shift: TShiftState);
begin
  CreateParamObj;
end;

function TTShape.GetParamObj: TPersistent;
begin
  Result:= TPersistent(Shape);
end;

function TTShape.CreateParamObj:TPersistent;
begin
  isTemp:= true;
  Result:= TPersistent(Shape);
end;

constructor TTShape.Create(nName: string);
begin
  inherited Create(nName);
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

procedure TScene.delLastShape;
begin
  setLength(scene, length(scene)-1);
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

ToolContainer := TToolContainer.Create;

end.
