unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, FileUtil, Graphics, Forms, Controls,
  Dialogs, ExtCtrls, Menus, ComCtrls, StdCtrls, Buttons, Grids, Spin, TypInfo,
  DrawTools, DrawEditors, DrawZoom, types;

type

  { TMainF }

  TMainF = class(TForm)
    ColorDialog: TColorDialog;
    ViewMI: TMenuItem;
    FillMI: TMenuItem;
    PaletteG: TDrawGrid;
    PaletteP: TPanel;
    MainP: TPanel;
    ParamToolP: TPanel;
    HorizontalSB: TScrollBar;
    VerticalSB: TScrollBar;
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
    procedure FillMIClick(Sender: TObject);
    procedure HorizontalSBScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure MainPBMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
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
    procedure VerticalSBScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
  private
    isDrawing: boolean;
    selectedToolId: integer;
    paletteColors: array of TColor;
    paletteCell: TPoint;
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
  VP := TViewport.Create(@MainPB.Invalidate);
  //Генерация кнопок
  t1:= 0; t2:= 0;
  cellInRow:= ToolsP.Width div (toolSide+toolSpace);
  for i := 0 to high(ToolContainer.tool) do
  begin
    m:= TBitmap.Create;
    m.LoadFromFile('icons/'+ToolContainer.tool[i].ClassName+'.bmp');
    b := TSpeedButton.Create(ToolsP);
    b.HelpKeyword:= ToolContainer.tool[i].name;
    b.Name := 'Tool' + IntToStr(i);
    b.Parent := ToolsP;
    b.Glyph := m;
    b.Flat := True;
    b.GroupIndex := 1;
    if i = 0 then begin
      b.Down:= true;
      ToolClick(b);
    end;
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
  Inspector.Load(TPersistent(ToolContainer.tool[selectedToolId].CreateParamObj), ParamToolP);
end;

procedure TMainF.VerticalSBScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  if (VerticalSB.Position+VerticalSB.PageSize) > VerticalSB.Max Then exit;;
  VP.SetSb(Point(0,VerticalSB.Position));
end;

procedure TMainF.MainPBMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  isDrawing := True;
  ToolContainer.tool[selectedToolId].MDown(Point(X, Y), button);
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
  Inspector.Load(TPersistent(ToolContainer.tool[selectedToolId].GetParamObj), ParamToolP);
end;

procedure TMainF.MainPBPaint(Sender: TObject);
begin
  MainPB.Canvas.Brush.Color:= clWhite;
  MainPB.Canvas.Brush.Style:= bsSolid;
  MainPB.Canvas.FillRect(0,0,MainPB.Width, MainPB.Height);
  Scene.Draw(MainPB.Canvas);
  VP.ReCalculate(MainPB, HorizontalSB, VerticalSB);
end;

procedure TMainF.PaletteGMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  tCol, tRow: longint;
  t: integer;
begin
  PaletteG.MouseToCell(x, y, tCol, tRow);
  paletteCell.x:=tCol;
  paletteCell.y:=tRow;
  t:= PaletteG.ColCount * paletteCell.y + paletteCell.x;
  if Button = mbLeft Then begin
    PenS.Brush.Color:= paletteColors[t];
    Inspector.SetPenColor(PenS.Brush.Color);
  end;
  if Button = mbRight Then begin
    BrushS.Brush.Color:= paletteColors[t];
    Inspector.SetBrushColor(BrushS.Brush.Color);
  end;
end;

procedure TMainF.PenSMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  ColorDialog.Color := PenS.Brush.Color;
  if ColorDialog.Execute then begin
    PenS.Brush.Color := ColorDialog.Color;
    Inspector.SetPenColor(PenS.Brush.Color);
  end;
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
  if ColorDialog.Execute then begin
    BrushS.Brush.Color := ColorDialog.Color;
    Inspector.SetBrushColor(BrushS.Brush.Color);
  end;
end;

procedure TMainF.FillMIClick(Sender: TObject);
begin
  VP.ScaleTo(Point(0,0), Point(0,0));
end;

procedure TMainF.HorizontalSBScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  if (HorizontalSB.Position+HorizontalSB.PageSize) > HorizontalSB.Max Then exit;;
  VP.SetSb(Point(HorizontalSB.Position, 0));
end;

procedure TMainF.MainPBMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  VP.ScaleMouseWhell(MousePos, WheelDelta>0);
end;

procedure TMainF.PaletteGDblClick(Sender: TObject);
var
  t: integer;
begin
  t:= PaletteG.ColCount * paletteCell.y + paletteCell.x;
  if not (t in [0..PaletteG.ColCount]) Then exit;
  ColorDialog.Color := paletteColors[t];
  if not ColorDialog.Execute then exit;
  paletteColors[t] := ColorDialog.Color;
  PenS.Brush.Color := paletteColors[t];
  Inspector.SetPenColor(PenS.Brush.Color);
  PaletteG.InvalidateCell(paletteCell.x, paletteCell.y);
end;

procedure TMainF.PaletteGDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin
  PaletteG.Canvas.Brush.Color:= paletteColors[(aRow)*PaletteG.ColCount+aCol];
  PaletteG.Canvas.FillRect(aRect.Left, aRect.Top, aRect.Right, aRect.Bottom);
end;

end.
