unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, Menus, Buttons, StdCtrls, DrawShape, DrawLayers, AddObjects;

type
  { TFormMain }

  TFormMain = class(TForm)
    ColorDialog: TColorDialog;
    LabelLineSize: TLabel;
    LabelLayers: TLabel;
    Layers: TListBox;
    MainMenu: TMainMenu;
    MenuItemEdit: TMenuItem;
    MenuItemCleanAll: TMenuItem;
    MenuItemHideTools: TMenuItem;
    MenuItemHidePalette: TMenuItem;
    MenuItemFile: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemView: TMenuItem;
    MenuItemAbout: TMenuItem;
    MenuItemExit: TMenuItem;
    PaintBox: TPaintBox;
    PanelLayersTools: TPanel;
    PanelLineSize: TPanel;
    PanelTools: TPanel;
    PanelBottom: TPanel;
    PanelLayers: TPanel;
    PanelColor: TPanel;
    PanelPalette: TPanel;
    PanelPaint: TPanel;
    LayersToolsDelete: TSpeedButton;
    LayersToolsHide: TSpeedButton;
    LayersToolsRename: TSpeedButton;
    SplitterLayers: TSplitter;
    StatusBar: TStatusBar;
    BrushPre: TShape;
    PenPre: TShape;
    LineSize: TTrackBar;
    procedure BrushPreMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LayersMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LayersSelectionChange(Sender: TObject; User: boolean);
    procedure LayersToolsDeleteClick(Sender: TObject);
    procedure LayersToolsHideClick(Sender: TObject);
    procedure LayersToolsRenameClick(Sender: TObject);
    procedure MenuItemCleanAllClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LineSizeChange(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure MenuItemHidePaletteClick(Sender: TObject);
    procedure MenuItemHideToolsClick(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseLeave(Sender: TObject);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxPaint(Sender: TObject);
    procedure PenPreMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SpeedButtonToolClick(Sender: TObject);
    procedure ChangeColor(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    procedure LayersSync();
  public
  end;

var
  FormMain: TFormMain;
  PBShapes: TLayers;
  tool: Shape;
  isDrawing: boolean;
  Param: TFParam;
  SHUtil: TFUtil;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.MenuItemAboutClick(Sender: TObject);
begin
  ShowMessage('Векторный редактор'+#13+'Денис Давидюк Б8103А');
end;

procedure TFormMain.FormCreate(Sender: TObject);
var i, j, k, c, x, y: integer; s: Shape; b: boolean;
  procedure createBtn(color: TColor);
  const
    border = 1;
    buttonSide = 16;
    rows = 5;
  var SButton: TShape;
  begin
    SButton:= TShape.Create(self);
    SButton.Parent := PanelPalette;
    SButton.Left := x;
    SButton.Name := 'PaletteButton'+intToStr(c);
    inc(c);
    SButton.Top := (buttonSide+border)*y;
    SButton.Width := buttonSide;
    SButton.Height := buttonSide;
    SButton.Brush.Color := color;
    SButton.OnMouseDown:=@ChangeColor;
    if y = (rows-1) Then inc(x, (buttonSide+border));
    y:= (y+1) mod rows;
  end;

  procedure createTool(s: string; b: boolean);
  var btn: TSpeedButton;
  begin
    btn:= TSpeedButton.Create(self);
    btn.Parent := PanelTools;
    btn.Name := 'Tool'+intToStr(c);
    inc(c);
    btn.Caption := s;
    btn.Flat := true;
    btn.GroupIndex:=5;
    btn.Down:= b;
    btn.Align:=alTop;
    btn.BorderSpacing.Around:=2;
    btn.OnClick:=@SpeedButtonToolClick;
  end;

begin
  PBShapes := TLayers.Create;
  Param:= TFParam.Create(PenPre, BrushPre, LineSize, PBShapes.selected, LabelLineSize, StatusBar.Panels.Items[2]);
  SHUtil:= TFUtil.Create();
  tool:= sPen;
  //создание палитры
  c:= 0; x:= 0; y:= 0;
  for i:= 0 to 9 do createBtn(RGBToColor(round(i*28.3), round(i*28.3), round(i*28.3)));
  for i:= 0 to 4 do
    for j:= 0 to 4 do
      for k:= 0 to 4 do createBtn(RGBToColor(round(i*63.75), round(j*63.75), round(k*63.75)));
  //создание набора инструментов
  c:= 0;
  for s in Shape do begin
    if s = sPen Then b:= true
    else b:= false;
    createTool(SHUtil.shapeN[s], b);
  end;
end;

procedure TFormMain.MenuItemCleanAllClick(Sender: TObject);
var s: Shape;
begin
  //очищает холст
  PBShapes.Clear;
  PaintBox.Invalidate;
  LayersSync;
  for s in Shape do SHUtil.shapeC[s]:= 0;
end;

procedure TFormMain.LayersSelectionChange(Sender: TObject; User: boolean);
  procedure changeButtons(b1, b2, b3: boolean);
  begin
    LayersToolsDelete.Enabled := b1;
    LayersToolsHide.Enabled := b2;
    LayersToolsRename.Enabled := b3;
  end;

begin
  ///
  If (Layers.Count = 0) or (Layers.ItemIndex = -1) Then begin
    changeButtons(false, false, false);
    PBShapes.selected := nil;
    PBShapes.selectedId:= -1;
    Param.sel:= nil;
    Exit;
  end;
  PBShapes.selectedId := Layers.ItemIndex;
  PBShapes.selected := PBShapes.Get(Layers.ItemIndex);
  Param.sel:= PBShapes.selected;
  If PBShapes.selected.IsVisible Then LayersToolsHide.Caption:= 'Скрыть'
  else LayersToolsHide.Caption:= 'Отобразить';
  Param.Update;
  changeButtons(true, true, true);
end;

procedure TFormMain.LayersMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  //для сброса выделения, при щелчке в пустую область
  with Sender as TListBox do
    if ItemAtPos(Point(X, Y), True) = -1 then ItemIndex:=-1;
  LayersSelectionChange(nil, false);
end;

procedure TFormMain.BrushPreMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ColorDialog.Color:= Param.BrushGet;
  If ColorDialog.Execute Then Param.BrushSet(ColorDialog.Color);
  PaintBox.Invalidate;
end;

procedure TFormMain.LayersToolsDeleteClick(Sender: TObject);
begin
  //удаление слоя
  PBShapes.Remove(PBShapes.selectedId);
  PaintBox.Invalidate;
  LayersSync();
end;

procedure TFormMain.LayersToolsHideClick(Sender: TObject);
begin
  PBShapes.selected.IsVisible := not(PBShapes.selected.IsVisible);
  if LayersToolsHide.Caption = 'Скрыть' Then LayersToolsHide.Caption:= 'Отобразить'
  else LayersToolsHide.Caption:= 'Скрыть';
  PaintBox.Invalidate;
end;

procedure TFormMain.LayersToolsRenameClick(Sender: TObject);
begin
  PBShapes.selected.name:= InputBox('Введите новое имя','Введите новое имя для элемента',PBShapes.selected.name);
  LayersSync();
end;

procedure TFormMain.LineSizeChange(Sender: TObject);
begin
  Param.LineSet(-1);
  PaintBox.Invalidate;
end;


procedure TFormMain.ChangeColor(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var sh: TShape;
begin
  //изменение основного или дополнительного цвета
  sh := TShape(Sender);
  If Button = mbLeft Then Param.PenSet(sh.Brush.Color);
  If Button = mbRight Then Param.BrushSet(sh.Brush.Color);
  PaintBox.Invalidate;
end;

procedure TFormMain.LayersSync;
var i, j: integer;
begin
  ///
  i:= Layers.ItemIndex;
  Layers.Clear;
  for j:= 0 to PBShapes.length-1 do
    Layers.addItem(PBShapes.Get(j).name, PBShapes.Get(j));
  If i < Layers.Count Then Layers.ItemIndex:= i
  else Layers.ItemIndex:= -1;
  LayersSelectionChange(nil, false);
end;

procedure TFormMain.MenuItemExitClick(Sender: TObject);
begin
  FormMain.Close; //Hatl(0); Application.Terminate;
end;

procedure TFormMain.MenuItemHidePaletteClick(Sender: TObject);
begin
  //скрывает/отображает палитру
  MenuItemHidePalette.Checked := not(MenuItemHidePalette.Checked);
  If MenuItemHidePalette.Checked Then PanelBottom.Show
  else PanelBottom.Hide;
end;

procedure TFormMain.MenuItemHideToolsClick(Sender: TObject);
begin
  //скрывает/отображает меню инструментов
  MenuItemHideTools.Checked := not(MenuItemHideTools.Checked);
  If MenuItemHideTools.Checked Then PanelTools.Show
  else PanelTools.Hide;
end;

procedure TFormMain.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  //обработка события нажатие кнопки мыши на PaintBox
  If tool = sCursor Then exit;
  isDrawing:= true;
  PBShapes.Add(SHUtil.createShape(tool, Param.LineGet(), Param.PenGet(), Param.BrushGet(), Point(x, y))); //добавление нового слоя
  LayersSync();
  PaintBox.Invalidate;
end;

procedure TFormMain.PaintBoxMouseLeave(Sender: TObject);
begin
  StatusBar.Panels.Items[1].Text := '';
end;

procedure TFormMain.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  //обработка события перемещение мыши по PaintBox
  StatusBar.Panels.Items[1].Text := intToStr(X)+ ','+intToStr(Y);
  If not(isDrawing) Then exit;
  PBShapes.Get(PBShapes.length-1).Point(Point(X, Y));
  PaintBox.Invalidate;
end;

procedure TFormMain.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  //обработка события отжатие кнопки мыши на PaintBox
  If tool = sCursor Then exit;
  PaintBox.Invalidate;
  LayersSync();
  isDrawing:= false;
end;

procedure TFormMain.PaintBoxPaint(Sender: TObject);
var i: integer;
begin
  //вызывает поочерёдно метод Paint каждого слоя
  PaintBox.Canvas.Brush.Color:=clWhite;
  PaintBox.Canvas.FillRect(0, 0, PaintBox.Width, PaintBox.Height);
  for i:= 0 to PBShapes.length-1 do
    PBShapes.Get(i).Paint(PaintBox.Canvas);
end;

procedure TFormMain.PenPreMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ColorDialog.Color:= Param.PenGet;
  if ColorDialog.Execute Then Param.PenSet(ColorDialog.Color);
  PaintBox.Invalidate;
end;

procedure TFormMain.SpeedButtonToolClick(Sender: TObject);
var i: Shape; s: TButton;
begin
  //обработчик события нажатие на кнопку-инструмент
  s:= TButton(Sender);
  PaintBox.Cursor:=crCross;
  for i in Shape do
    if SHUtil.shapeN[i] = s.Caption Then break;
  tool:= i;
  If tool = sCursor Then PaintBox.Cursor:= crDefault;
end;


end.

