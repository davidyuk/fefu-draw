unit DrawTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, DrawShapes, DrawObjectInspector;

type

  { TToolBase }

  TToolBase = class
  protected
    n: string;
  public
    property Name: string read n;
    procedure MMove(point: TPoint; isDrawing: boolean; shift: TShiftState); virtual; abstract;
    procedure MDown(point: TPoint; shift: TShiftState); virtual; abstract;
    procedure MUp(point: TPoint; shift: TShiftState); virtual; abstract;
    procedure Leave; virtual;
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



var
  ToolContainer: TToolContainer;

implementation
{ TTShape }

uses DrawScene;

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
  Inspector.LoadNew(CreateParamObj);
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

procedure TToolBase.Leave;
begin
end;

constructor TToolBase.Create(nName: string);
begin
  n:= nName;
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
