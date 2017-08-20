unit DrawObjectEditors;

{$mode objfpc}{$H+}

interface

uses
  Classes, typinfo, DrawObjectInspector, Spin, ExtCtrls, StdCtrls, Controls, FPCanvas;

const
  margin = 2;
  ratio = 8/14;

type

  { TIEInteger }

  TIEInteger = class(TIEditor)
  private
    spin: TSpinEdit;
    noChange: boolean;
    procedure Change(Sender: TObject); override;
  public
    constructor Create(nObj: array of TPersistent; nProp: PPropInfo; panel: TPanel; defaultParams: boolean); override;
    destructor Destroy; override;
    procedure Refresh; override;
  end;

  { TIEFloat }

  TIEFloat = class(TIEditor)
  private
    spin: TFloatSpinEdit;
    pc: boolean;
    noChange: boolean;
    procedure Change(Sender: TObject); override;
  public
    constructor Create(nObj: array of TPersistent; nProp: PPropInfo; panel: TPanel; defaultParams: boolean); override;
    destructor Destroy; override;
    procedure Refresh; override;
  end;

  { TIEScale }

  TIEScale = class(TIEditor)
  private
    cmbbox: TComboBox;
    procedure Change(Sender: TObject); override;
  public
    constructor Create(nObj: array of TPersistent; nProp: PPropInfo; panel: TPanel; defaultParams: boolean); override;
    destructor Destroy; override;
    procedure Refresh; override;
  end;

  { TIEPenStyle }

  TIEPenStyle = class(TIEditor)
  private
    cmbbox: TComboBox;
    procedure cmbboxDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
    procedure Change(Sender: TObject); override;
  public
    constructor Create(nObj: array of TPersistent; nProp: PPropInfo; panel: TPanel; defaultParams: boolean); override;
    destructor Destroy; override;
    procedure Refresh; override;
  end;

  { TIEBrushStyle }

  TIEBrushStyle = class(TIEditor)
  private
    cmbbox: TComboBox;
    procedure cmbboxDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
    procedure Change(Sender: TObject); override;
  public
    constructor Create(nObj: array of TPersistent; nProp: PPropInfo; panel: TPanel; defaultParams: boolean); override;
    destructor Destroy; override;
    procedure Refresh; override;
  end;

var
  Inspector: TInspector;

implementation

uses
  LCLType, Graphics, SysUtils;

var
  PropValues : TStringList;

{ TIEScale }

procedure TIEScale.Change(Sender: TObject);
var t: real; i: integer;
begin
  t:= StrToFloatDef(TComboBox(Sender).Text, 100)/100;
  if t <= 0 then t:= 100;
  for i:= 0 to high(obj) do
    SetFloatProp(obj[i], prop, t);
  inherited Change(Sender);
end;

constructor TIEScale.Create(nObj: array of TPersistent; nProp: PPropInfo;
  panel: TPanel; defaultParams: boolean);
var i: integer;
begin
  cmbbox := TComboBox.Create(panel);
  cmbbox.AddItem('25',nil);
  cmbbox.AddItem('50',nil);
  cmbbox.AddItem('75',nil);
  cmbbox.AddItem('100',nil);
  cmbbox.AddItem('150',nil);
  cmbbox.AddItem('300',nil);
  cmbbox.AddItem('500',nil);
  cmbbox.AddItem('800',nil);
  cmbbox.parent:= panel;
  cmbbox.Left:= trunc(panel.width * ratio) + margin;
  cmbbox.width:= trunc(panel.Width * (1-ratio)) - margin*2;
  cmbbox.OnChange := @Change;
  cmbbox.Top:= panel.tag;
  inherited Create(nObj, nProp, panel, defaultParams);
  if defaultParams and (PropValues.Values[prop^.Name] <> '') Then
    for i:= 0 to high(obj) do
      SetFloatProp(obj[i], prop, StrToFloat(PropValues.Values[prop^.Name]));
  Refresh;
end;

destructor TIEScale.Destroy;
begin
  cmbbox.Destroy;
  inherited Destroy;
end;

procedure TIEScale.Refresh;
var j: double; i: integer;
begin
  j:= GetFloatProp(obj[0], prop)*100;
  for i:= 1 to high(obj) do
    if (GetFloatProp(obj[i], prop)*100) <> j Then begin
      j:= 100;
      break;
    end;
  cmbbox.text := floatToStr(j);
end;

{ TIEFloat }

procedure TIEFloat.Change(Sender: TObject);
var i: integer;
begin
  if noChange Then begin
    noChange:=false;
    exit;
  end;
  for i:= 0 to high(obj) do
    SetFloatProp(obj[i], prop, TFloatSpinEdit(Sender).Value);
  PropValues.Values[prop^.Name]:= floatToStr(TFloatSpinEdit(Sender).Value);
  inherited Change(Sender);
end;

constructor TIEFloat.Create(nObj: array of TPersistent; nProp: PPropInfo;
  panel: TPanel; defaultParams: boolean);
var i: integer;
begin
  spin := TFloatSpinEdit.Create(nil);
  spin.Increment := 0.5;
  spin.MinValue := -10000;
  spin.MaxValue := 10000;
  spin.parent:= panel;
  spin.Left:= trunc(panel.width * ratio) + margin;
  spin.width:= trunc(panel.Width * (1-ratio)) - margin*2;
  spin.OnChange := @Change;
  spin.Top:= panel.tag;
  inherited Create(nObj, nProp, panel, defaultParams);
  if defaultParams and (PropValues.Values[prop^.Name] <> '') Then
    for i:= 0 to high(obj) do
      SetFloatProp(obj[i], prop, StrToFloat(PropValues.Values[prop^.Name]));
  Refresh;
end;

destructor TIEFloat.Destroy;
begin
  spin.Destroy;
  inherited Destroy;
end;

procedure TIEFloat.Refresh;
var j: Double; i: integer;
begin
    j:= GetFloatProp(obj[0], prop);
    for i:= 0 to high(obj) do
      if GetFloatProp(obj[i], prop) <> j then begin
        j:= -1;
        break;
      end;
    spin.value := j;
end;

{ TIEBrushStyle }

procedure TIEBrushStyle.cmbboxDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
const
  height = 7; //это половина высоты отрисованного внутри cbox кубика
var
  cbox: TCombobox;
begin
  cbox:= TCombobox(Control);
  cbox.Canvas.Brush.Color:= clWhite;
  If odFocused in State Then cbox.Canvas.Brush.Color:= cl3DLight;
  cbox.Canvas.FillRect(ARect);
  cbox.Canvas.Brush.Color:= clBlack;
  cbox.Canvas.Brush.Style := TFPBrushStyle(PtrInt(cbox.Items.Objects[Index]));
  cbox.Canvas.FillRect(ARect.Left+1, (ARect.Bottom+ARect.Top) div 2-height, ARect.Right-1, (ARect.Bottom+ARect.Top) div 2+height);
end;

procedure TIEBrushStyle.Change(Sender: TObject);
var i: integer;
begin
  for i:= 0 to high(obj) do
    SetInt64Prop(obj[i], prop, TCombobox(sender).ItemIndex);
  PropValues.Values[prop^.Name]:= intToStr(TCombobox(sender).ItemIndex);
  inherited Change(Sender);
end;

constructor TIEBrushStyle.Create(nObj: array of TPersistent; nProp: PPropInfo;
  panel: TPanel; defaultParams: boolean);
var
  i: TFPBrushStyle; j: integer;
begin
  cmbbox := TComboBox.Create(nil);
  cmbbox.parent:= panel;
  cmbbox.Left:= trunc(panel.width * ratio) + margin;
  cmbbox.width:= trunc(panel.Width * (1-ratio)) - margin*2;
  for i in TFPBrushStyle do
    if not (i in [bsImage,bsPattern]) then
      cmbbox.AddItem('', TObject(PtrInt(i)));
  cmbbox.OnDrawItem := @cmbboxDrawItem;
  cmbbox.OnChange := @Change;
  cmbbox.Style := csOwnerDrawFixed;
  cmbbox.ReadOnly:= True;
  cmbbox.Top:= panel.tag;
  inherited Create(nObj, nProp, panel, defaultParams);
  if defaultParams and (PropValues.Values[prop^.Name] <> '') Then
    for j:= 0 to high(obj) do
      SetInt64Prop(obj[j], prop, StrToInt64(PropValues.Values[prop^.Name]));
  Refresh;
end;

destructor TIEBrushStyle.Destroy;
begin
  cmbbox.Destroy;
  inherited Destroy;
end;

procedure TIEBrushStyle.Refresh;
var j: int64; i: integer;
begin
  j:= GetInt64Prop(obj[0], prop);
  for i:= 1 to high(obj) do
    if GetInt64Prop(obj[i], prop) <> j then begin
      j:= integer(bsClear);
      break;
    end;
  for i:= 0 to cmbbox.Items.Count  do
    if PtrInt(cmbbox.Items.Objects[i]) = j Then begin
      cmbbox.ItemIndex:= i;
      exit;
    end;
end;

{ TIEPenStyle }

procedure TIEPenStyle.cmbboxDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  cbox: TCombobox;
begin
  cbox:= TCombobox(Control);
  cbox.Canvas.Brush.Color:= clWhite;
  If odFocused in State Then cbox.Canvas.Brush.Color:= cl3DLight;
  cbox.Canvas.FillRect(ARect);
  cbox.Canvas.Pen.Color:= clBlack;
  cbox.Canvas.Pen.Style := TFPPenStyle(PtrInt(cbox.Items.Objects[Index]));
  cbox.Canvas.Line(ARect.Left, (ARect.Bottom+ARect.Top) div 2, ARect.Right, (ARect.Bottom+ARect.Top) div 2);
end;

procedure TIEPenStyle.Change(Sender: TObject);
var i: integer;
begin
  for i:= 0 to high(obj) do
    SetInt64Prop(obj[i], prop, cmbbox.ItemIndex);
  PropValues.Values[prop^.Name]:= intToStr(cmbbox.ItemIndex);
  inherited Change(Sender);
end;

constructor TIEPenStyle.Create(nObj: array of TPersistent; nProp: PPropInfo;
  panel: TPanel; defaultParams: boolean);
var
  i: TFPPenStyle; j: integer;
begin
  cmbbox := TComboBox.Create(nil);
  cmbbox.parent:= panel;
  cmbbox.Left:= trunc(panel.width * ratio) + margin;
  cmbbox.width:= trunc(panel.Width * (1-ratio)) - margin*2;
  for i in TFPPenStyle do
    if not (i in [psinsideFrame,psPattern,psClear]) then
      cmbbox.AddItem('', TObject(PtrInt(i)));
  cmbbox.OnDrawItem := @cmbboxDrawItem;
  cmbbox.OnChange := @Change;
  cmbbox.Style := csOwnerDrawFixed;
  cmbbox.ReadOnly:= True;
  cmbbox.Top:= panel.tag;
  inherited Create(nObj, nProp, panel, defaultParams);
  if defaultParams and (PropValues.Values[prop^.Name] <> '') Then
    for j:= 0 to high(obj) do
      SetInt64Prop(obj[j], prop, StrToInt64(PropValues.Values[prop^.Name]));
  Refresh;
end;

destructor TIEPenStyle.Destroy;
begin
  cmbbox.Destroy;
  inherited Destroy;
end;

procedure TIEPenStyle.Refresh;
var j: int64; i: integer;
begin
  j:= GetInt64Prop(obj[0], prop);
  for i:= 1 to high(obj) do
    if GetInt64Prop(obj[i], prop) <> j then begin
      j:= integer(psSolid);
      break;
    end;
  for i:= 0 to cmbbox.Items.Count-1 do begin
    if PtrInt(cmbbox.Items.Objects[i]) = j Then begin
      cmbbox.ItemIndex:= i;
      exit;
    end;
  end;
end;

{ TIEInteger }

procedure TIEInteger.Change(Sender: TObject);
var i: integer;
begin
  if noChange then begin
    noChange:= false;
    exit;
  end;
  for i:= 0 to high(obj) do
    SetInt64Prop(obj[i], prop, TSpinEdit(Sender).Value);
  PropValues.Values[prop^.Name]:= intToStr(TSpinEdit(Sender).Value);
  inherited Change(Sender);
end;

constructor TIEInteger.Create(nObj: array of TPersistent; nProp: PPropInfo;
  panel: TPanel; defaultParams: boolean);
var
  i: integer;
begin
  spin := TSpinEdit.Create(nil);
  spin.MinValue := 1;
  spin.MaxValue := 100;
  spin.parent:= panel;
  spin.Left:= trunc(panel.width * ratio) + margin;
  spin.width:= trunc(panel.Width * (1-ratio)) - margin*2;
  spin.OnChange := @Change;
  spin.Top:= panel.tag;
  inherited Create(nObj, nProp, panel, defaultParams);
  if defaultParams and (PropValues.Values[prop^.Name] <> '') Then
    for i:= 0 to high(obj) do
      SetInt64Prop(obj[i], prop, StrToInt64(PropValues.Values[prop^.Name]));
  Refresh;
end;

destructor TIEInteger.Destroy;
begin
  spin.Destroy;
  inherited Destroy;
end;

procedure TIEInteger.Refresh;
var j: Int64; i: integer;
begin
  j:= GetInt64Prop(obj[0], prop);
  for i:= 0 to high(obj) do
    if GetInt64Prop(obj[i], prop) <> j then begin
      j:= 50;
      noChange:=true;
      break;
    end;
  spin.value := j;
end;


initialization

EditorContainer := TIEditorContainer.create;

EditorContainer.addTool(TIEInteger, 'LongInt');
EditorContainer.addTool(TIEScale, 'TScale');
EditorContainer.addTool(TIEPenStyle, 'TFPPenStyle');
EditorContainer.addTool(TIEBrushStyle, 'TFPBrushStyle');
EditorContainer.addTool(TIEFloat, 'Real');

PropValues:= TStringList.Create;
PropValues.Values['scale']:='1';
PropValues.Values['penW']:= '3';
PropValues.Values['penS']:= '0';
PropValues.Values['brushS']:= '0';
PropValues.Values['radius']:= '10';

end.

