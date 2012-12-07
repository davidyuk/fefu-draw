unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, FileUtil, Graphics, Forms, Controls, Dialogs, ExtCtrls,
  Menus, ComCtrls, StdCtrls, Buttons, Grids, TypInfo, DrawTools;

type

  { TMainF }

  TMainF = class(TForm)
    ColorDialog: TColorDialog;
    PaletteG: TDrawGrid;
    PaletteP: TPanel;
    ParamToolP: TPanel;
    ToolsL: TLabel;
    ToolsP: TPanel;
    MainMenu: TMainMenu;
    FileMI: TMenuItem;
    HelpMI: TMenuItem;
    ExitMI: TMenuItem;
    AboutMI: TMenuItem;
    MainPB: TPaintBox;
    LeftP: TPanel;
    PaletteMainP: TPanel;
    PenS: TShape;
    BrushS: TShape;
    procedure AboutMIClick(Sender: TObject);
    procedure BrushSMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PaletteGDblClick(Sender: TObject);
    procedure PaletteGDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure ExitMIClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MainPBMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure MainPBMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure MainPBMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure MainPBPaint(Sender: TObject);
    procedure PaletteGMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PenSMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure ToolClick(Sender: TObject);
  private
    isDrawing: boolean;
    selectedToolId, tempNum: integer;
    paletteColors: array of TColor;
  public

  end;

var
  MainF: TMainF;

implementation

{$R *.lfm}

{ TMainF }

procedure TMainF.FormCreate(Sender: TObject);
const
  toolSide = 30;
  toolSpace = 2;
  toolPTopSpacing = 22;
var
  i, j, k, t1, t2, cellInRow: integer;
  b: TSpeedButton;
  m: TBitMap;
begin
  Scene := TScene.Create();
  //Генерация кнопок
  selectedToolId := 0; t1:= 0; t2:= 0;
  cellInRow:= ToolsP.Width div (toolSide+toolSpace);
  for i := 0 to high(ToolContainer.tool) do
  begin
    m:= TBitmap.Create;
    m.LoadFromFile(ToolContainer.Name[i]);
    b := TSpeedButton.Create(self);
    b.Name := 'Tool' + IntToStr(i);
    b.Parent := ToolsP;
    b.Glyph := m;
    b.Flat := True;
    b.GroupIndex := 1;
    b.Down := i = 0;
    b.Width := toolSide;
    b.Height := toolSide;
    b.Left := (toolSide+toolSpace)*t2;
    b.Top := toolPTopSpacing+(toolSide+toolSpace)*t1;
    inc(t2);
    if t2 = cellInRow Then begin
      t2:= 0;
      inc(t1);
    end;
    b.OnClick := @ToolClick;
    b.Tag := i;
  end;
  ToolsP.Height:= toolPTopSpacing+(toolSide+toolSpace)*(t1+1)+toolSpace;
  //Генерация палитры
  setLength(paletteColors, PaletteG.ColCount*PaletteG.RowCount);
  for i:= 0 to high(paletteColors) do paletteColors[i]:= clWhite;
  t1:= trunc(power(trunc(PaletteG.ColCount*PaletteG.RowCount/2), 1/3));
  t2:= PaletteG.ColCount;
  for i:= 0 to t1 do
    for j:= 0 to t1 do
      for k:= 0 to t1 do begin
        paletteColors[t2]:= RGBToColor(round(i*255/t1), round(j*255/t1), round(k*255/t1));
        inc(t2);
      end;
  t1:= high(paletteColors)-t2+1;
  for i:= t2 to high(paletteColors) do
    paletteColors[i]:=RGBToColor(round((i-t2)*255/t1), round((i-t2)*255/t1), round((i-t2)*255/t1));
end;

procedure TMainF.ToolClick(Sender: TObject);
var
  b: TSpeedButton;
begin
  b := TSpeedButton(Sender);
  selectedToolId := b.Tag;
end;

procedure TMainF.MainPBMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  isDrawing := True;
  ToolContainer.tool[selectedToolId].MDown(Point(X, Y), button, PenS.Brush.Color,
    BrushS.Brush.Color, 3); {!!!!}
  MainPB.Invalidate;
end;

procedure TMainF.MainPBMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  ToolContainer.tool[selectedToolId].MMove(Point(X, Y), isDrawing);
  MainPB.Invalidate;
end;

procedure TMainF.MainPBMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  isDrawing := False;
  ToolContainer.tool[selectedToolId].MUp(Point(X, Y));
  MainPB.Invalidate;
end;

procedure TMainF.MainPBPaint(Sender: TObject);
begin
  Scene.drawScene(MainPB.Canvas);
end;

procedure TMainF.PaletteGMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  tCol, tRow: longint;
begin
  PaletteG.MouseToCell(x, y, tCol, tRow);
  tempNum:= PaletteG.ColCount * tRow + tCol;
  if Button = mbLeft Then
    PenS.Brush.Color:= paletteColors[tempNum];
  if Button = mbRight Then
    BrushS.Brush.Color:= paletteColors[tempNum];
end;

procedure TMainF.PenSMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  ColorDialog.Color := PenS.Brush.Color;
  if ColorDialog.Execute then
    PenS.Brush.Color := ColorDialog.Color;
end;

procedure TMainF.ExitMIClick(Sender: TObject);
begin
  MainF.Close;
end;

procedure TMainF.AboutMIClick(Sender: TObject);
begin
  ShowMessage('Векторный редактор' + #13 +
    'Денис Давидюк Б8103А' + #13 + '06 декабря 2012');
end;

procedure TMainF.BrushSMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  ColorDialog.Color := BrushS.Brush.Color;
  if ColorDialog.Execute then
    BrushS.Brush.Color := ColorDialog.Color;
end;

procedure TMainF.PaletteGDblClick(Sender: TObject);
begin
  if not (tempNum in [0..PaletteG.ColCount]) Then exit;
  ColorDialog.Color := paletteColors[tempNum];
  if ColorDialog.Execute then
    paletteColors[tempNum] := ColorDialog.Color;
  PenS.Brush.Color := paletteColors[tempNum];
  PaletteG.InvalidateCell(tempNum mod PaletteG.ColCount, tempNum div PaletteG.ColCount);
end;

procedure TMainF.PaletteGDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin
  PaletteG.Canvas.Brush.Color:= paletteColors[(aRow)*PaletteG.ColCount+aCol];
  PaletteG.Canvas.FillRect(aRect.Left, aRect.Top, aRect.Right, aRect.Bottom);
end;

end.
