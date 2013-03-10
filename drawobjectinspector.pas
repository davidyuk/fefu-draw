unit DrawObjectInspector;

{$mode objfpc}{$H+}

interface

uses
  Classes, typinfo, Graphics, ExtCtrls, StdCtrls;

type

  { TIEditor }

  TIEditor = class
  protected
    lbl: TLabel;
    obj: array of TPersistent;
    prop: PPropInfo;
    procedure Change(Sender: TObject); virtual;
  public
    destructor Destroy; override;
    constructor Create(nObj: array of TPersistent; nProp: PPropInfo; panel: TPanel; defaultParams: boolean); virtual;
    procedure Refresh; virtual; abstract;
  end;

  { TInspector }

  TInspector = class
  private
    obj: array of TPersistent;
    editors: array of TIEditor;
    penC, brushC: TColor;
    defaultParams: boolean;
    panel: TPanel;
  public
    procedure LoadNew(nObj: TPersistent);
    procedure Load(aObj: array of TPersistent);
    procedure Refresh;
    procedure SetPenColor(color: TColor);
    procedure SetBrushColor(color: TColor);
    procedure Clean;
    constructor Create(nPanel: TPanel);
  end;

  ClassOfEditor = class of TIEditor;
  ArrOfEditor = array of record
    item: ClassOfEditor;
    kind: ShortString;
  end;

  { TIEditorContainer }

  TIEditorContainer = class
  private
    eArr: ArrOfEditor;
  public
    procedure addTool(e: ClassOfEditor; kind: ShortString);
    property editor: ArrOfEditor read eArr;
  end;

var
  Inspector: TInspector;
  EditorContainer: TIEditorContainer;

implementation

uses
  main, DrawObjectEditors;

var
  PropNames : TStringList;

{ TIEditorContainer }

procedure TIEditorContainer.addTool(e: ClassOfEditor; kind: ShortString);
begin
  setLength(eArr, length(eArr) + 1);
  eArr[high(eArr)].item := e;
  eArr[high(eArr)].kind := kind;
end;

{ TInspector }

procedure TInspector.LoadNew(nObj: TPersistent);
var
  arr: array of TPersistent;
  i, j: integer;
  list: PPropList;
begin
  if nObj = nil then begin
    Clean;
    exit;
  end;
  defaultParams := true;
  setLength(arr, 1);
  arr[0]:= nObj;
  Load(arr);
end;

procedure TInspector.Load(aObj: array of TPersistent);
function searchPropInObj(obj: TPersistent; prop: PPropInfo): boolean;
var
  i, j: integer;
  list: PPropList;
begin
  j:= GetPropList(obj, list);
  result:= false;
  for i:= 0 to j-1 do
    if list^[i] = prop then begin
      result:= true;
      exit;
    end;
end;
var
  list: PPropList;
  i, j, k, count: integer;
  b: boolean;
begin
  Clean;
  //obj:= aObj;
  if length(aObj) = 0 Then exit;
  setLength(obj, length(aObj));
  for i:= 0 to high(aObj) do
    obj[i]:= aObj[i];

  panel.caption:= '';
  panel.tag := 0;

  j:= GetPropList(obj[0], list);
  for i:= 0 to j-1 do begin
    b:= true;
    for j:= 1 to high(obj) do
      If not searchPropInObj(obj[j], list^[i]) Then begin
        b:= false;
        break;
      end;
    if b then begin
      for j:= 0 to high(EditorContainer.editor) do
        if list^[i]^.PropType^.Name = EditorContainer.editor[j].kind Then begin
          SetLength(editors, length(editors)+1);
          editors[high(editors)]:= EditorContainer.editor[j].item.Create(obj,list^[i],panel, defaultParams);
          break;
        end;
    end;
  end;
  if defaultParams then begin
    SetPenColor(penC);
    SetBrushColor(brushC);
  end;
  defaultParams := false;
end;

procedure TInspector.Refresh;
var i: integer;
begin
  for i:= 0 to high(editors) do
    editors[i].Refresh;
end;

procedure TInspector.SetPenColor(color: TColor);
var
  list: PPropList;
  i, j, k: integer;
begin
  penC:= color;
  if obj[0] = nil then exit;
  for k:= 0 to high(obj) do begin
    j:= GetPropList(obj[k], list);
    for i:= 0 to j-1 do
      if list^[i]^.Name = 'penC' Then SetInt64Prop(obj[k], list^[i], color);
  end;
  MainF.PBInvalidate;
end;

procedure TInspector.SetBrushColor(color: TColor);
var
  list: PPropList;
  i, j, k: integer;
begin
  brushC:= color;
  if obj = nil then exit;
  for k:= 0 to high(obj) do begin
    j:= GetPropList(obj[k], list);
    for i:= 0 to j-1 do
      if list^[i]^.Name = 'brushC' Then SetInt64Prop(obj[k], list^[i], color);
  end;
  MainF.PBInvalidate;
end;

procedure TInspector.Clean;
var i: integer;
begin
  for i:= 0 to high(editors) do
    editors[i].Destroy;
  setLength(editors, 0);
end;

constructor TInspector.Create(nPanel: TPanel);
begin
  panel:= nPanel;
  penC:= clBlack;
  brushC:= clWhite;
end;

{ TIEditor }

constructor TIEditor.Create(nObj: array of TPersistent; nProp: PPropInfo;
  panel: TPanel; defaultParams: boolean);
const
  lblTopMatgin = 4;
var i: integer;
begin
  //obj:= nObj;
  setLength(obj, length(nObj));
  for i:= 0 to high(nObj) do
    obj[i]:= nObj[i];
  prop:= nProp;
  lbl:= TLabel.Create(nil);
  lbl.Parent := panel;
  lbl.Caption:= PropNames.Values[nProp^.Name];
  lbl.Left:= margin;
  lbl.Width := trunc(panel.width * ratio) - margin*2;
  lbl.Top:= panel.tag+lblTopMatgin;
  if lbl.Caption = '' Then lbl.Caption := nProp^.Name;
  panel.tag := panel.tag + 25;
end;

procedure TIEditor.Change(Sender: TObject);
begin
  MainF.PBInvalidate;
end;

destructor TIEditor.Destroy;
begin
  lbl.Destroy;
end;

initialization

PropNames := TStringList.Create;

PropNames.Values['brushC']:='Цвет заливки';
PropNames.Values['brushS']:='Вид заливки';
PropNames.Values['penW']:='Толщина пера';
PropNames.Values['penC']:='Цвет пера';
PropNames.Values['penS']:='Вид пера';
PropNames.Values['radius']:='Радиус скругления';
PropNames.Values['scale']:='Масштаб (%)';

end.

