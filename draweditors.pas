unit DrawEditors;

{$mode objfpc}{$H+}

interface

uses
  Classes, Dialogs, typinfo, LCLType , Graphics , SysUtils, Spin, ExtCtrls, StdCtrls, DrawShapes, Controls, FPCanvas;

type

  { TIEditor }

  TIEditor = class
  private
    lbl: TLabel;
    obj: TPersistent;
    prop: PPropInfo;
  public
    constructor Create(nObj: TPersistent; nProp: PPropInfo; panel: TPanel); virtual;
    destructor Destroy; override;
  end;

  { TInspector }

  TInspector = class
  private
    obj: TPersistent;
    editors: array of TIEditor;
    penC, brushC: TColor;
  public
    procedure Load(nObj: TPersistent; panel: TPanel);
    procedure SetPenColor(color: TColor);
    procedure SetBrushColor(color: TColor);
  end;

var
  Inspector: TInspector;

implementation

type

  { TIEInteger }

  TIEInteger = class(TIEditor)
  private
    spin: TSpinEdit;
    procedure spinChange(Sender: TObject);
  public
    constructor Create(nObj: TPersistent; nProp: PPropInfo; panel: TPanel); override;
    destructor Destroy; override;
  end;

  { TIEPenStyle }

  TIEPenStyle = class(TIEditor)
  private
    cmbbox: TComboBox;
    procedure cmbboxDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
    procedure cmbboxChange(Sender: TObject);
  public
    constructor Create(nObj: TPersistent; nProp: PPropInfo; panel: TPanel); override;
    destructor Destroy; override;
  end;

  { TIEBrushStyle }

  TIEBrushStyle = class(TIEditor)
  private
    cmbbox: TComboBox;
    procedure cmbboxDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
    procedure cmbboxChange(Sender: TObject);
  public
    constructor Create(nObj: TPersistent; nProp: PPropInfo; panel: TPanel); override;
    destructor Destroy; override;
  end;

const
  margin = 2;
  ratio = 2/3;

var
  PropNames : TStringList; //содержит исправления имён полей фигур

{ TIEBrushStyle }

procedure TIEBrushStyle.cmbboxDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
const
  height = 7;
var
  cbox: TCombobox;
begin
  cbox:= TCombobox(Control);
  cbox.Canvas.Brush.Color:= clWhite;
  If odFocused in State Then cbox.Canvas.Brush.Color:= cl3DLight;
  cbox.Canvas.FillRect(ARect);
  cbox.Canvas.Brush.Color:= clBlack;
  cbox.Canvas.Brush.Style := TFPBrushStyle(cbox.Items.Objects[Index]);
  cbox.Canvas.FillRect(ARect.Left+1, (ARect.Bottom+ARect.Top) div 2-height, ARect.Right-1, (ARect.Bottom+ARect.Top) div 2+height);
end;

procedure TIEBrushStyle.cmbboxChange(Sender: TObject);
begin
  SetInt64Prop(obj, prop, TCombobox(sender).ItemIndex);
end;

constructor TIEBrushStyle.Create(nObj: TPersistent; nProp: PPropInfo;
  panel: TPanel);
var
  i: TFPBrushStyle;
begin
  cmbbox := TComboBox.Create(nil);
  cmbbox.parent:= panel;
  cmbbox.Left:= trunc(panel.width * ratio) + margin;
  cmbbox.width:= trunc(panel.Width * (1-ratio)) - margin*2;
  for i in TFPBrushStyle do
    if not (i in [bsImage,bsPattern]) then cmbbox.AddItem('',TObject(i));
  cmbbox.OnDrawItem := @cmbboxDrawItem;
  cmbbox.OnChange := @cmbboxChange;
  cmbbox.Style := csOwnerDrawFixed;
  cmbbox.ReadOnly:= True;
  cmbbox.ItemIndex:= GetInt64Prop(nObj, nProp);
  cmbbox.Top:= panel.tag;
  inherited Create(nObj, nProp, panel);
end;

destructor TIEBrushStyle.Destroy;
begin
  cmbbox.Destroy;
  inherited Destroy;
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
  cbox.Canvas.Pen.Style := TFPPenStyle(cbox.Items.Objects[Index]);
  cbox.Canvas.Line(ARect.Left, (ARect.Bottom+ARect.Top) div 2, ARect.Right, (ARect.Bottom+ARect.Top) div 2);
end;

procedure TIEPenStyle.cmbboxChange(Sender: TObject);
begin
  SetInt64Prop(obj, prop, TCombobox(sender).ItemIndex);
end;

constructor TIEPenStyle.Create(nObj: TPersistent; nProp: PPropInfo;
  panel: TPanel);
var
  i: TFPPenStyle;
begin
  cmbbox := TComboBox.Create(nil);
  cmbbox.parent:= panel;
  cmbbox.Left:= trunc(panel.width * ratio) + margin;
  cmbbox.width:= trunc(panel.Width * (1-ratio)) - margin*2;
  for i in TFPPenStyle do
    if not (i in [psinsideFrame,psPattern,psClear]) then cmbbox.AddItem('',TObject(i));
  cmbbox.OnDrawItem := @cmbboxDrawItem;
  cmbbox.OnChange := @cmbboxChange;
  cmbbox.Style := csOwnerDrawFixed;
  cmbbox.ReadOnly:= True;
  cmbbox.ItemIndex:= GetInt64Prop(nObj, nProp);
  cmbbox.Top:= panel.tag;
  inherited Create(nObj, nProp, panel);
end;

destructor TIEPenStyle.Destroy;
begin
  cmbbox.Destroy;
  inherited Destroy;
end;

{ TIEInteger }

procedure TIEInteger.spinChange(Sender: TObject);
begin
  SetInt64Prop(obj, prop, TSpinEdit(Sender).Value);
end;

constructor TIEInteger.Create(nObj: TPersistent; nProp: PPropInfo; panel: TPanel);
begin
  spin := TSpinEdit.Create(nil);
  spin.MinValue := 1;
  spin.MaxValue := 100;
  spin.value := GetInt64Prop(nObj, nProp);
  spin.parent:= panel;
  spin.Left:= trunc(panel.width * ratio) + margin;
  spin.width:= trunc(panel.Width * (1-ratio)) - margin*2;
  spin.OnChange := @spinChange;
  spin.Top:= panel.tag;
  inherited Create(nObj, nProp, panel);
end;

destructor TIEInteger.Destroy;
begin
  spin.Destroy;
  inherited Destroy;
end;

{ TIEditor }

constructor TIEditor.Create(nObj: TPersistent; nProp: PPropInfo; panel: TPanel);
begin
  obj:= nObj;
  prop:= nProp;
  lbl:= TLabel.Create(nil);
  lbl.Parent := panel;
  lbl.Caption:= PropNames.Values[nProp^.Name];
  lbl.Left:= margin;
  lbl.Width := trunc(panel.width * ratio) - margin*2;
  lbl.Top:= panel.tag;
  if lbl.Caption = '' Then lbl.Caption := nProp^.Name;
  panel.tag := panel.tag + 25;
end;

destructor TIEditor.Destroy;
begin
  lbl.Destroy;
end;

{ TInspector }

procedure TInspector.Load(nObj: TPersistent; panel: TPanel);
var
  list: PPropList;
  i, j: integer;
begin
  for i:= 0 to high(editors) do
    editors[i].Destroy;
  setLength(editors, 0);
  obj:= nObj;
  panel.caption:= '';
  panel.tag := 0;
  j:= GetPropList(nObj, list);
  for i:= 0 to j-1 do begin
    case list^[i]^.PropType^.Name of
      'LongInt': begin
        SetLength(editors, length(editors)+1);
        editors[high(editors)]:= TIEInteger.Create(nObj,list^[i],panel);
      end;
      'TFPPenStyle': begin
        SetLength(editors, length(editors)+1);
        editors[high(editors)]:= TIEPenStyle.Create(nObj,list^[i],panel);
      end;
      'TFPBrushStyle': begin
        SetLength(editors, length(editors)+1);
        editors[high(editors)]:= TIEBrushStyle.Create(nObj,list^[i],panel);
      end;
    end;
  end;
  SetPenColor(penC);
  SetBrushColor(brushC);
end;

procedure TInspector.SetPenColor(color: TColor);
var
  list: PPropList;
  i, j: integer;
begin
  penC:= color;
  if obj = nil then exit;
  j:= GetPropList(obj, list);
  for i:= 0 to j-1 do begin
    if list^[i]^.Name = 'penC' Then SetInt64Prop(obj, list^[i], color);
  end;
end;

procedure TInspector.SetBrushColor(color: TColor);
var
  list: PPropList;
  i, j: integer;
begin
  brushC:= color;
  if obj = nil then exit;
  j:= GetPropList(obj, list);
  for i:= 0 to j-1 do begin
    if list^[i]^.Name = 'brushC' Then SetInt64Prop(obj, list^[i], color);
  end;
end;

initialization

Inspector:= TInspector.Create;

PropNames := TStringList.Create;
PropNames.Values['brushC']:='Цвет заливки';
PropNames.Values['brushS']:='Вид заливки';
PropNames.Values['penW']:='Толщина пера';
PropNames.Values['penC']:='Цвет пера';
PropNames.Values['penS']:='Вид пера';
PropNames.Values['radius']:='Радиус скругления';

end.

