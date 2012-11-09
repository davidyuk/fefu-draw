unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, Menus, Buttons, StdCtrls;

type
  Shape = (sCursor, sPen, sLine, sEllipse, sRectangle);

  PPaintBoxShape = ^PaintBoxShape;
  PaintBoxShape = class(TObject)
    Kind: Shape;
    Point: array of TPoint;
    LineSize: integer;
    PenColor: TColor;
    BrushColor: TColor;
    Name: String;
    IsVisible: boolean;
    Before, Next: PPaintBoxShape;
    procedure Paint();
    procedure editPoint(x, y: integer; new: boolean);
  public
    constructor Create(newKind: Shape; newLineSize: integer; pen, brush: TColor);
  end;

  { TFormMain }

  TFormMain = class(TForm)
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
    brushPre: TShape;
    penPre: TShape;
    LayersToolsDelete: TSpeedButton;
    LayersToolsHide: TSpeedButton;
    LayersToolsRename: TSpeedButton;
    SpeedButtonCursor: TSpeedButton;
    SpeedButtonPen: TSpeedButton;
    SpeedButtonEllipse: TSpeedButton;
    SpeedButtonRectangle: TSpeedButton;
    SpeedButtonLine: TSpeedButton;
    SplitterLayers: TSplitter;
    StatusBar: TStatusBar;
    LineSize: TTrackBar;
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
    procedure SpeedButtonToolClick(Sender: TObject);
    procedure ChangeColor(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    procedure LayersSync();
    function LayerSelected():PPaintBoxShape;
  public
    { public declarations }
  end;

var
  FormMain: TFormMain;
  FirstPBShape, LastPBShape, TempPBShape: PPaintBoxShape;
  tool: Shape;
  isDrawing: boolean;
  penColor, brushColor: TColor;
  shapeCounter: array [Shape] of integer;
  tempPoint: TPoint;

implementation

{$R *.lfm}

function shapeToString(shape: Shape):string;
begin
  //возвращает название типа
  case shape of
    sCursor: result:= 'Курсор';
    sPen: result:= 'Карандаш';
    sLine: result:= 'Линия';
    sEllipse: result:= 'Окружность';
    sRectangle: result:= 'Прямоугольник';
  end;
end;

constructor PaintBoxShape.Create(newKind: Shape; newLineSize: integer; pen, brush: TColor);
begin
  //инициализация объекта
  self.Kind := newKind;
  self.LineSize := newLineSize;
  self.penColor:=pen;
  self.brushColor:=brush;
  self.IsVisible:=True;
  inc(shapeCounter[newKind]);
  self.Name:=shapeToString(newKind)+' '+intToStr(shapeCounter[newKind]);
end;


procedure PaintBoxShape.Paint();
var i: integer;
begin
  //метод объекта PaintBoxShape, рисующий объект
  if not(IsVisible) Then exit;
  FormMain.PaintBox.Canvas.Pen.Width:=LineSize;
  FormMain.PaintBox.Canvas.Pen.Color:=PenColor;
  FormMain.PaintBox.Canvas.Brush.Color:=BrushColor;
  case Kind of
    sPen: begin
      FormMain.PaintBox.Canvas.MoveTo(Point[0]);
      for i:= 1 to High(Point) do
        FormMain.PaintBox.Canvas.LineTo(Point[i]);
    end;
    sLine: begin
      FormMain.PaintBox.Canvas.MoveTo(Point[0]);
      FormMain.PaintBox.Canvas.LineTo(Point[1]);
    end;
    sEllipse: FormMain.PaintBox.Canvas.Ellipse(Point[0].X, Point[0].Y, Point[1].X, Point[1].Y);
    sRectangle: FormMain.PaintBox.Canvas.Rectangle(Point[0].X, Point[0].Y, Point[1].X, Point[1].Y);
  end;
end;

procedure PaintBoxShape.editPoint(x, y: integer; new: boolean);
var newPoint: TPoint;
begin
  //изменяет последнюю точку в массиве или создаёт новую точку
  newPoint.x:= x;
  newPoint.y:= y;
  if new then setLength(self.Point, length(self.Point)+1);
  Point[High(Point)]:= newPoint;
end;

{ TFormMain }

procedure TFormMain.MenuItemAboutClick(Sender: TObject);
begin
  ShowMessage('Векторный редактор'+#13+'Денис Давидюк Б8103А');
end;

procedure TFormMain.FormCreate(Sender: TObject);
var i, j, k, c, x, y: integer;
  procedure createBtn(color: TColor);
  var SButton: TShape;
  begin
    SButton:= TShape.Create(self);
    SButton.Parent := PanelPalette;
    SButton.Left := x;
    SButton.Name := 'PaletteButton'+intToStr(c);
    inc(c);
    SButton.Top := 17*y;
    SButton.Width := 16;
    SButton.Height := 16;
    SButton.Brush.Color := color;
    SButton.OnMouseDown:=@ChangeColor;
    if y = 4 Then inc(x, 17);
    y:= (y+1) mod 5;
  end;

begin
  //создание палитры
  x:= 0; y:= 0; c:= 0;
  for i:= 0 to 9 do createBtn(RGBToColor(round(i*28.3), round(i*28.3), round(i*28.3)));
  for i:= 0 to 4 do
    for j:= 0 to 4 do
      for k:= 0 to 4 do createBtn(RGBToColor(round(i*63.75), round(j*63.75), round(k*63.75)));
  penColor:= $0;
  brushColor:= $ffffff;
  penPre.Brush.Color := penColor;
  brushPre.Brush.Color := brushColor;

  FirstPBShape:= Nil;
  LastPBShape:= Nil;
end;

procedure TFormMain.MenuItemCleanAllClick(Sender: TObject);
var s: Shape;
begin
  //очищает холст
  TempPBShape:=LastPBShape;
  While TempPBShape <> Nil do begin
    TempPBShape:=LastPBShape^.Before;
    Dispose(LastPBShape);
    LastPBShape:=TempPBShape;
  end;
  Dispose(TempPBShape);
  FirstPBShape:=Nil;
  LastPBShape:=Nil;
  PaintBox.Invalidate;
  LayersSync();
  for s:= sPen to sRectangle do shapeCounter[s]:= 0;
end;

procedure TFormMain.LayersSelectionChange(Sender: TObject; User: boolean);
var i: integer;
begin
  If (Layers.Count = 0) or (Layers.ItemIndex = -1) Then begin
    LayersToolsDelete.Enabled := false;
    LayersToolsHide.Enabled := false;
    LayersToolsRename.Enabled := false;
    Exit;
  end;
  If LayerSelected()^.IsVisible Then LayersToolsHide.Caption:= 'Скрыть'
  else LayersToolsHide.Caption:= 'Отобразить';
  LineSize.Position:=LayerSelected()^.LineSize;
  penColor:= LayerSelected^.PenColor;
  brushColor:= LayerSelected^.BrushColor;
  penPre.Brush.Color := penColor;
  brushPre.Brush.Color := brushColor;
  LayersToolsDelete.Enabled := true;
  LayersToolsHide.Enabled := true;
  LayersToolsRename.Enabled := true;
end;

procedure TFormMain.LayersMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  //для сброса выделения, при щелчке в пустую область
  with Sender as TListBox do
    if ItemAtPos(Point(X, Y), True) = -1 then ItemIndex:=-1;
end;

procedure TFormMain.LayersToolsDeleteClick(Sender: TObject);
begin
  //удаление слоя
  if Layers.Count = 1 Then begin MenuItemCleanAllClick(nil); exit; end;
  LayerSelected()^.Before^.next := LayerSelected()^.Next;
  If LayerSelected()^.Next <> nil Then
    LayerSelected()^.Next^.before := LayerSelected()^.Before
  else
    LastPBShape := LayerSelected()^.Before;
  dispose(LayerSelected());
  LayersSync();
  PaintBox.Invalidate;
end;

procedure TFormMain.LayersToolsHideClick(Sender: TObject);
begin
  LayerSelected()^.IsVisible := not(LayerSelected()^.IsVisible);
  if LayersToolsHide.Caption = 'Скрыть' Then LayersToolsHide.Caption:= 'Отобразить'
  else LayersToolsHide.Caption:= 'Скрыть';
  PaintBox.Invalidate;
end;

procedure TFormMain.LayersToolsRenameClick(Sender: TObject);
begin
  LayerSelected()^.Name:= InputBox('Введите новое имя','Введите новое имя для элемента',LayerSelected()^.Name);
  LayersSync();
end;

procedure TFormMain.LineSizeChange(Sender: TObject);
begin
  LabelLineSize.Caption := 'Толщина линии - '+intToStr(LineSize.Position)+'px';
  If LayerSelected() <> Nil Then LayerSelected()^.LineSize:=LineSize.Position;
  PaintBox.Invalidate;
end;


procedure TFormMain.ChangeColor(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var sh: TShape;
begin
  //изменение основного или дополнительного цвета
  sh := TShape(Sender);
  If Button = mbLeft Then begin
    penColor:= sh.Brush.Color;
    penPre.Brush.Color := penColor;
    if LayerSelected() <> nil Then LayerSelected()^.PenColor:=penColor;
  end;
  If Button = mbRight Then begin
    brushColor:= sh.Brush.Color;
    brushPre.Brush.Color := brushColor;
    if LayerSelected() <> nil Then LayerSelected()^.BrushColor:=brushColor;
  end;
  StatusBar.Panels.Items[2].Text := intToHex(penColor, 6)+','+intToHex(brushColor, 6);
  PaintBox.Invalidate;
end;

procedure TFormMain.LayersSync;
var p: PPaintBoxShape; i: integer;
begin
  p:= FirstPBShape;
  i:= Layers.ItemIndex;
  Layers.Clear;
  while p <> Nil do begin
    Layers.addItem(p^.name, TObject(p));
    p:= p^.next;
  end;
  If i < Layers.Count Then Layers.ItemIndex:= i;
  LayersSelectionChange(nil, false);
end;

function TFormMain.LayerSelected: PPaintBoxShape;
begin
  If Layers.ItemIndex = -1 Then result:= nil
  else result:= PPaintBoxShape(Layers.Items.Objects[Layers.ItemIndex]);
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
  If (tool = sCursor) and (Layers.ItemIndex = -1) Then exit;
  isDrawing:= true;
  If tool = sCursor Then begin
    TempPBShape:=LayerSelected();
    tempPoint.x:= x;
    tempPoint.y:= y;
    exit;
  end;
  TempPBShape:= Nil; //добавление нового слоя
  New(TempPBShape);
  TempPBShape^ := PaintBoxShape.Create(tool, LineSize.Position, penColor, brushColor);
  If FirstPBShape=Nil Then begin
    FirstPBShape:=TempPBShape;
    LastPBShape:=TempPBShape;
    TempPBShape^.Before:=Nil;
  end else begin
    LastPBShape^.Next:= TempPBShape;
    TempPBShape^.Before:= LastPBShape;
    LastPBShape:=TempPBShape;
  end;
  TempPBShape^.Next:= Nil; //слой добавлен
  case tool of
    sPen: TempPBShape^.editPoint(X, Y, true);
    sRectangle, sEllipse, sLine: with TempPBShape^ do begin
      editPoint(X, Y, true);
      editPoint(X, Y, true);
    end;
  end;
end;

procedure TFormMain.PaintBoxMouseLeave(Sender: TObject);
begin
  StatusBar.Panels.Items[1].Text := '';
end;

procedure TFormMain.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var i: integer;
begin
  //обработка события перемещение мыши по PaintBox
  StatusBar.Panels.Items[1].Text := intToStr(X)+ ','+intToStr(Y);
  If not(isDrawing) Then exit;
  case tool of
    sPen: TempPBShape^.editPoint(X, Y, true);
    sLine, sEllipse, sRectangle: TempPBShape^.editPoint(X, Y, false);
    sCursor: begin
      for i:= 0 to high(TempPBShape^.Point) do begin
        TempPBShape^.Point[i].x -= tempPoint.x-x;
        TempPBShape^.Point[i].y -= tempPoint.y-y;
      end;
      tempPoint.x:= x;
      tempPoint.y:= y;
    end;
  end;
  PaintBox.Invalidate;
end;

procedure TFormMain.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  //обработка события отжатие кнопки мыши на PaintBox
  If (tool = sCursor) and (Layers.ItemIndex = -1) Then exit;
  PaintBox.Invalidate;
  LayersSync();
  isDrawing:= false;
end;

procedure TFormMain.PaintBoxPaint(Sender: TObject);
var i: integer; p: PPaintBoxShape;
begin
  //вызывает поочерёдно метод объектов PPBShape
  PaintBox.Canvas.Brush.Color:=clWhite;
  PaintBox.Canvas.FillRect(0, 0, PaintBox.Width, PaintBox.Height);
  p:= FirstPBShape;
  if p = nil Then exit;
  While p^.next <> nil do begin
    p^.Paint;
    p:=p^.next;
  end;
  p^.Paint;
end;

procedure TFormMain.SpeedButtonToolClick(Sender: TObject);
begin
  //обработчик события нажатие на кнопку-инструмент
  Sender:= TButton(Sender);
  PaintBox.Cursor:=crCross;
  If Sender = SpeedButtonCursor Then begin tool:= sCursor; PaintBox.Cursor:= crDefault; end;
  If Sender = SpeedButtonPen Then tool:= sPen;
  If Sender = SpeedButtonLine Then tool:= sLine;
  If Sender = SpeedButtonEllipse Then tool:= sEllipse;
  If Sender = SpeedButtonRectangle Then tool:= sRectangle;
end;


end.

