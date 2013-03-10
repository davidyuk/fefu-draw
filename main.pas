unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Graphics, Forms, Controls,
  Dialogs, ExtCtrls, Menus, StdCtrls, Buttons, Grids,
  DrawTools, DrawObjectInspector, DrawZoom, XMLWrite, XMLRead, DOM, DrawScene;

type

  { TMainF }

  TMainF = class(TForm)
    ColorDialog: TColorDialog;
    ShapeBottomMI: TMenuItem;
    ShapeMoveUpMI: TMenuItem;
    ShapeMoveDownMI: TMenuItem;
    ShapeZindexMI: TMenuItem;
    ShapeTopMI: TMenuItem;
    ShapeDeleteMI: TMenuItem;
    OpenDialog: TOpenDialog;
    ShapeEditPM: TPopupMenu;
    SaveDialog: TSaveDialog;
    SeparatorMI: TMenuItem;
    OpenMI: TMenuItem;
    SaveMI: TMenuItem;
    SaveAsMI: TMenuItem;
    NewMI: TMenuItem;
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
    PB: TPaintBox;
    LeftP: TPanel;
    PaletteMainP: TPanel;
    PenS: TShape;
    BrushS: TShape;
    procedure AboutMIClick(Sender: TObject);
    procedure BrushSMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure NewMIClick(Sender: TObject);
    procedure FillMIClick(Sender: TObject);
    procedure HorizontalSBScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure PBMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure OpenMIClick(Sender: TObject);
    procedure PaletteGDblClick(Sender: TObject);
    procedure PaletteGDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure ExitMIClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PBMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PBMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure PBMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PBPaint(Sender: TObject);
    procedure PaletteGMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PenSMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    function SaveAsMIClick(Sender: TObject):boolean;
    function SaveMIClick(Sender: TObject):boolean;
    procedure ShapeBottomMIClick(Sender: TObject);
    procedure ShapeDeleteMIClick(Sender: TObject);
    procedure ShapeMoveDownMIClick(Sender: TObject);
    procedure ShapeMoveUpMIClick(Sender: TObject);
    procedure ShapeTopMIClick(Sender: TObject);
    procedure ToolClick(Sender: TObject);
    procedure VerticalSBScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure SetTitle;
  const
    ProgramName = 'Векторный редактор';
  private
    isDrawing, isEdited: boolean;
    fileName, fileAdr: string;
    selectedToolId: integer;
    paletteColors: array of TColor;
    paletteCell: TPoint;
  public
    PBInvalidate: procedure of object;
    procedure LoadSelectedShapes;
  end;

var
  MainF: TMainF;

implementation

{$R *.lfm}

{ TMainF }

procedure TMainF.FormCreate(Sender: TObject);
const
  toolSide = 30; //сторона кнопки
  toolSpace = 2; //отступ вокруг кнопки
  toolPTopSpacing = 22; //отступ перед первым рядом кнопок
var
  i, j, k, t1, t2, cellInRow: integer;
  b: TSpeedButton;
  m: TBitMap;
begin
  PBInvalidate := @PB.Invalidate;
  Scene := TScene.Create();
  VP := TViewport.Create();
  Inspector := TInspector.Create(ParamToolP);
  fileName := 'Безымянный';
  isEdited := false;
  SetTitle;
  //Генерация кнопок
  t1:= 0; t2:= 0;
  cellInRow:= ToolsP.Width div (toolSide+toolSpace);
  selectedToolId := 0;
  for i := 0 to high(ToolContainer.tool) do
  begin
    m:= TBitmap.Create;
    m.LoadFromFile('icons/'+ToolContainer.tool[i].ClassName+'.bmp');
    b := TSpeedButton.Create(ToolsP);
    b.ShowHint:= true;
    b.Hint := ToolContainer.tool[i].Name;
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
begin
  ToolContainer.tool[selectedToolId].Leave;
  selectedToolId := TSpeedButton(Sender).Tag;
  Inspector.LoadNew(TPersistent(ToolContainer.tool[selectedToolId].CreateParamObj));
end;

procedure TMainF.VerticalSBScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  PB.Invalidate;
end;

procedure TMainF.SetTitle;
var
  c: string;
begin
  if fileAdr <> '' Then begin
    fileName:= fileAdr;
    while pos('\', fileName) > 0 do
      fileName:= copy(fileName, pos('\', fileName)+1, length(fileName));
  end;
  c:= ' - ';
  if isEdited Then c := '*' + c;
  MainF.Caption := fileName + c + ProgramName;
end;

procedure TMainF.LoadSelectedShapes;
var
  a: array of TPersistent;
  i: integer;
begin
  setLength(a, 0);
  for i:= 0 to High(Scene.Shapes) do
    if Scene.Shapes[i].Selected Then begin
      setLength(a, length(a)+1);
      a[high(a)]:= TPersistent(Scene.Shapes[i]);
    end;
  Inspector.Load(a);
end;

procedure TMainF.PBMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  isEdited := true;
  SetTitle;
  isDrawing := true;
  ToolContainer.tool[selectedToolId].MDown(Point(X, Y), shift);
  PB.Invalidate;
end;

procedure TMainF.PBMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  ToolContainer.tool[selectedToolId].MMove(Point(X, Y), isDrawing, shift);
  PB.Invalidate;
end;

procedure TMainF.PBMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  isDrawing := False;
  ToolContainer.tool[selectedToolId].MUp(Point(X, Y), shift);
  PB.Invalidate;
end;

procedure TMainF.PBPaint(Sender: TObject);
begin
  PB.Canvas.Brush.Color:= clWhite;
  PB.Canvas.Brush.Style:= bsSolid;
  PB.Canvas.FillRect(0,0,PB.Width, PB.Height);
  VP.ReCalculate(PB, HorizontalSB, VerticalSB);
  Scene.Draw(PB.Canvas);
end;

procedure TMainF.PaletteGMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  tCol, tRow: longint;
  t: integer;
begin
  PaletteG.MouseToCell(x, y, tCol, tRow);
  paletteCell.x:=tCol; //paletteCell - объявленно глобально
  paletteCell.y:=tRow; //используется для передачи номера ячейки в double click
  t:= PaletteG.ColCount * tRow + tCol;
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

function TMainF.SaveAsMIClick(Sender: TObject):boolean;
begin
  result:= false;
  if fileAdr <> '' then SaveDialog.FileName := fileName;
  if not SaveDialog.Execute Then exit;
  WriteXMLFile(Scene.GetXML, SaveDialog.FileName);
  fileAdr := SaveDialog.FileName;
  isEdited := false;
  SetTitle;
  result:= true;
end;

function TMainF.SaveMIClick(Sender: TObject): boolean;
begin
  if not isEdited Then exit;
  if fileAdr = '' then begin
    result := SaveAsMIClick(nil);
    exit;
  end;
  WriteXMLFile(Scene.GetXML, fileAdr);
  isEdited := false;
  SetTitle;
  result := true;
end;

procedure TMainF.ShapeBottomMIClick(Sender: TObject);
begin
  Scene.ShapeSelZIndex(false, false);
  PB.Invalidate;
end;

procedure TMainF.ShapeDeleteMIClick(Sender: TObject);
begin
  Scene.ShapeSelDelete;
  PB.Invalidate;
end;

procedure TMainF.ShapeMoveDownMIClick(Sender: TObject);
begin
  Scene.ShapeSelZIndex(true, false);
  PB.Invalidate;
end;

procedure TMainF.ShapeMoveUpMIClick(Sender: TObject);
begin
  Scene.ShapeSelZIndex(true, true);
  PB.Invalidate;
end;

procedure TMainF.ShapeTopMIClick(Sender: TObject);
begin
  Scene.ShapeSelZIndex(false, true);
  PB.Invalidate;
end;

procedure TMainF.ExitMIClick(Sender: TObject);
begin
  MainF.Close;
end;

procedure TMainF.AboutMIClick(Sender: TObject);
begin
  ShowMessage('Векторный редактор' + #13#10 +
    'Денис Давидюк Б8103А' + #13#10 + '31 марта 2013');
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

procedure TMainF.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var ans: integer;
begin
  CanClose := true;
  if isEdited = true then begin
    ans:= MessageDlg('Сохранить изменения в файле '+fileName, mtInformation, [mbYes, mbNo, mbCancel], 0);
    if ans = mrCancel Then CanClose := false;
    if ans = mrYes Then CanClose := SaveMIClick(nil);
  end;
end;

procedure TMainF.NewMIClick(Sender: TObject);
var ans: integer;
begin
  if isEdited then begin
    ans:= MessageDlg('Сохранить изменения в файле '+fileName, mtInformation, [mbYes, mbNo, mbCancel], 0);
    if ans = mrCancel Then exit;
    if (ans = mrYes) and not(SaveMIClick(nil)) Then exit;
  end;
  fileAdr:='';
  fileName:= 'Безымянный';
  isEdited := false;
  SetTitle;
  Scene.NewXML;
  PB.Invalidate;
end;

procedure TMainF.FillMIClick(Sender: TObject);
begin
  VP.ScaleTo(Point(0,0), Point(0,0));
  PB.Invalidate;
end;

procedure TMainF.HorizontalSBScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  PB.Invalidate;
end;

procedure TMainF.PBMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  VP.ScaleMouseWhell(MousePos, WheelDelta>0);
  PB.Invalidate;
  Inspector.Refresh;
end;

procedure TMainF.OpenMIClick(Sender: TObject);
var
  image: TXMLDocument;
  ans: integer;
begin
  if OpenDialog.Execute Then begin
    if isEdited then begin
      ans:= MessageDlg('Сохранить изменения в файле '+fileName, mtInformation, [mbYes, mbNo, mbCancel], 0);
      if ans = mrCancel Then exit;
      if (ans = mrYes) and not(SaveMIClick(nil)) Then exit;
    end;
    ReadXMLFile(image, OpenDialog.FileName);
    if not Scene.SetXML(image) then exit;
    fileAdr := OpenDialog.FileName;
    isEdited := false;
    SetTitle;
    PB.Invalidate;
  end;
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
  PaletteG.Canvas.Brush.Color:= paletteColors[aRow*PaletteG.ColCount+aCol];
  PaletteG.Canvas.FillRect(aRect);
end;

end.
