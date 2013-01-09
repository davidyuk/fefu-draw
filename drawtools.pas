unit DrawTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics, DrawShapes, Controls, DrawObjectInspector, DOM, typinfo, DrawTypes, Dialogs;

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

  ArrOfShape = array of TShapeBase;

  { TScene }

  TScene = class
  const
    xmlVer = '1';
  private
    scene: ArrOfShape;
    procedure SwapShapes(e1, e2:integer);
  public
    property Shapes: ArrOfShape read scene;
    procedure addShape(shape: TShapeBase);
    function getLast(): TShapeBase;
    procedure delLastShape;
    procedure Draw(canvas: TCanvas);
    function SetXML(imageFile: TXMLDocument):boolean;
    function GetXML: TXMLDocument;
    procedure NewXML;
    procedure ShapeSelZIndex(byOne, up: Boolean);
    procedure ShapeSelDelete;
    procedure ShapeSelShift(shift: TPoint);
  end;

var
  ToolContainer: TToolContainer;
  Scene: TScene;

implementation
{ TTShape }

uses sysutils, DrawZoom;

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

function TScene.SetXML(imageFile: TXMLDocument): boolean;
function SpiltPoint(t: string):TPointReal; //5,6
var
  s: string;
begin
  s:= copy(t, 1, pos(':', t)-1);
  result.x:= strToFloat(s);
  result.y:= strToFloat(copy(t, pos(':', t)+1, length(t)-1-pos(':', t)));
end;

var
  CurrNode: TDOMNode;
  i, len: integer;
  newScene: ArrOfShape;
  s: String;
begin
  setLength(newScene, 0);
  try
    CurrNode := imageFile.DocumentElement.FirstChild;
    while CurrNode <> nil do begin
      setLength(newScene, length(NewScene)+1);
      newScene[high(newScene)]:= TShapeBase(GetClass(CurrNode.NodeName).Create);
      for i:= 0 to CurrNode.Attributes.Length -1 do
        SetPropValue(newScene[high(newScene)], CurrNode.Attributes.Item[i].NodeName, CurrNode.Attributes.Item[i].NodeValue);

      s:= CurrNode.FirstChild.NodeValue;
      newScene[high(newScene)].setPointsLength(0);
      len:= 0;
      while pos(';',s) > 0 do begin
          inc(len);
          newScene[high(newScene)].setPointsLength(len);
          newScene[high(newScene)].Points[len-1]:= SpiltPoint(copy(s,1, pos(';',s)));
          s:=copy(s,pos(';',s)+1, length(s));
      end;
      CurrNode:= CurrNode.NextSibling;
    end;
    scene := newScene;
    result:= true;
  except
    on E: Exception do begin
      MessageDlg('Произошла ошибка при открытии файла:'+#13#10+E.Message, mtError,  [mbOK], 0);
      result:= false;
    end;
  end;
end;

function TScene.GetXML: TXMLDocument;
var
  RootNode, CurrNode: TDOMNode;
  i, j, count: integer;
  list: PPropList;
  pointS: string;
begin
  result:= TXMLDocument.Create;
  RootNode := Result.CreateElement('file');
  TDOMElement(RootNode).SetAttribute('version', xmlVer);
  Result.AppendChild(RootNode);
  RootNode:= Result.DocumentElement;
  for i:= 0 to high(scene) do begin
    CurrNode := Result.CreateElement(scene[i].ClassName);
    count:= GetPropList(scene[i], list);
    for j:= 0 to count-1 do
      TDOMElement(CurrNode).SetAttribute(list^[j]^.Name, String(GetPropValue(Scene[i], list^[j]^.Name)));
    pointS := '';
    for j:= 0 to High(Scene[i].Points) do
      pointS += floatToStr(Scene[i].Points[j].X) + ':' + floatToStr(Scene[i].Points[j].Y) + ';';
    CurrNode.AppendChild(Result.CreateTextNode(pointS));
    RootNode.AppendChild(CurrNode);
  end;
end;

procedure TScene.NewXML;
var
  i: integer;
begin
  for i:= 0 to high(scene) do
    scene[i].Free;
  setLength(scene, 0);
end;

procedure TScene.ShapeSelZIndex(byOne, up: Boolean);
var i, j, last: integer;
begin
  if up then begin
    last:= High(scene);
    for i := High(scene) downto 0 do
      if scene[i].Selected then begin
        if byOne then begin
          if i < last Then SwapShapes(i, i+1)
          else dec(last);
        end else begin
          for j:= i to last-1 do
            SwapShapes(j, j+1);
          dec(last);
        end;
      end;
  end else begin
    last:= 0;
    for i := 0 to High(scene) do
      if scene[i].Selected then begin
        if byOne then begin
          if i > last Then SwapShapes(i, i-1)
          else inc(last);
        end else begin
          for j:= i downto last+1 do
            SwapShapes(j, j-1);
          inc(last);
        end;
      end;
  end;
  {//вверх на один
  last:= High(scene); //последняя фигура которую можно менять
  for i := High(scene) downto 0 do
    if scene[i].Selected then begin
      if i < last Then SwapShapes(i, i+1)
      else dec(last);
    end;
  //вниз на один
  last:= 0; //последняя фигура которую можно менять
  for i := 0 to High(scene) do
    if scene[i].Selected then begin
      if i > last Then SwapShapes(i, i-1)
      else inc(last);
    end;
  //вверх полностью
  last:= High(scene); //последняя фигура которую можно менять
  for i := High(scene) downto 0 do
    if scene[i].Selected then begin
      SwapShapes(i, last);
      dec(last);
    end;
  //вниз полностью
  last:= 0; //последняя фигура которую можно менять
  for i := 0 to High(scene) do
    if scene[i].Selected then begin
      SwapShapes(i, last);
      inc(last);
    end;}
end;

procedure TScene.ShapeSelDelete;
var i, count, len: integer;
begin
  len := Length(scene);
  for i:= 0 to High(scene) do
    if scene[i].Selected Then begin FreeAndNil(scene[i]); dec(len); end;
  if len = length(scene) then exit;
  count:= 0;
  for i:= 0 to High(scene) do begin
    while ((i+count) <= High(scene)) and (scene[i+count] = Nil) do
        inc(count);
    SwapShapes(i, i+count);
    if Scene[i] = nil then break;
  end;
  SetLength(scene, len);
end;

procedure TScene.ShapeSelShift(shift: TPoint);
var i, j: integer;
begin
  for i:= 0 to High(scene) do
    for j:= 0 to High(scene[i].Points) do begin
      if not scene[i].Selected Then Continue;
      scene[i].Points[j].X += shift.x / VP.Scale;
      scene[i].Points[j].Y += shift.y / VP.Scale;
      //scene[i].Points[j] += PointReal(shift)*VP.Scale;
    end;
end;

procedure TScene.SwapShapes(e1, e2: integer);
var t: TShapeBase;
begin
  if e1 = e2 then exit;
  if e2 > high(scene) Then scene[e1]:= nil;
  t:= scene[e1];
  scene[e1]:= scene[e2];
  scene[e2]:= t;
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
