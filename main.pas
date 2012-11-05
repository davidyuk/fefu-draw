unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, Menus, Buttons, StdCtrls;

type
   Shape = (sCursor, sPen, sLine, sPolygon, sEllipse, sRectangle);

  PaintBoxShape = class(TObject)
    Kind: Shape;
    Point: array of TPoint;
    LineSize: integer;
    PenColor: TColor;
    BrushColor: TColor;
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
    MenuItemHideTools: TMenuItem;
    MenuItemHidePalette: TMenuItem;
    MenuItemFile: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemView: TMenuItem;
    MenuItemAbout: TMenuItem;
    MenuItemExit: TMenuItem;
    PaintBox: TPaintBox;
    PanelLineSize: TPanel;
    PanelTools: TPanel;
    PanelBottom: TPanel;
    PanelLayers: TPanel;
    PanelColor: TPanel;
    PanelPalette: TPanel;
    PanelPaint: TPanel;
    brushPre: TShape;
    penPre: TShape;
    SpeedButtonCursor: TSpeedButton;
    SpeedButtonPen: TSpeedButton;
    SpeedButtonPolygon: TSpeedButton;
    SpeedButtonEllipse: TSpeedButton;
    SpeedButtonRectangle: TSpeedButton;
    SpeedButtonLine: TSpeedButton;
    SplitterLayers: TSplitter;
    StatusBar: TStatusBar;
    LineSize: TTrackBar;
    procedure FormCreate(Sender: TObject);
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

  public
    { public declarations }
  end;

var
  FormMain: TFormMain;
  PBShape: array of PaintBoxShape;
  tool: Shape;
  isDrawing: boolean;
  penColor, brushColor: TColor;

implementation

{$R *.lfm}

constructor PaintBoxShape.Create(newKind: Shape; newLineSize: integer; pen, brush: TColor);
begin
  self.Kind := newKind;
  self.LineSize := newLineSize;
  self.penColor:=pen;
  self.brushColor:=brush;
end;

function shapeToString(shape: Shape):string;
begin
  case shape of
    sCursor: result:= 'Курсор';
    sPen: result:= 'Карандаш';
    sLine: result:= 'Линия';
    sPolygon: result:= 'Многоугольник';
    sEllipse: result:= 'Окружность';
    sRectangle: result:= 'Прямоугольник';
  end;
end;

procedure PaintBoxShape.Paint();
var i: integer;
begin
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
    sEllipse: begin
      FormMain.PaintBox.Canvas.Ellipse(Point[0].X, Point[0].Y, Point[1].X, Point[1].Y);
    end;
    sRectangle: begin
      FormMain.PaintBox.Canvas.Rectangle(Point[0].X, Point[0].Y, Point[1].X, Point[1].Y);
    end;
    sPolygon: begin
      FormMain.PaintBox.Canvas.Polygon(Point);
    end;
  end;
end;

procedure PaintBoxShape.editPoint(x, y: integer; new: boolean);
var newPoint: TPoint;
begin
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
var i, j, k, x, y: integer;
  SButton: TShape;
begin
  x:= 0;
  y:= 0;
  for i:= 0 to 3 do
    for j:= 0 to 3 do
      for k:= 0 to 3 do begin
        SButton:= TShape.Create(self);
        SButton.Parent := PanelPalette;
        SButton.Left := x;
        SButton.Name := 'PaletteB'+intToStr(i*100+j*10+k);
        SButton.Top := 17*y;
        SButton.Width := 16;
        SButton.Height := 16;
        SButton.Brush.Color := round(i/3*255)+round(j/3*255)*255+round(k/3*255)*255*255;

        if y = 1 Then inc(x, 17);
        y:= (y+1) mod 3;
        SButton.OnMouseDown:=@ChangeColor;
      end;
  penColor:= $0;
  brushColor:= $ffffff;
  penPre.Brush.Color := penColor;
  brushPre.Brush.Color := brushColor;
end;

procedure TFormMain.ChangeColor(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var sh: TShape;
begin
  sh := TShape(Sender);
  If Button = mbLeft Then begin
    penColor:= sh.Brush.Color;
    penPre.Brush.Color := penColor;
  end;
  If Button = mbRight Then begin
    brushColor:= sh.Brush.Color;
    brushPre.Brush.Color := brushColor;
  end;
end;

procedure TFormMain.MenuItemExitClick(Sender: TObject);
begin
  FormMain.Close; //Hatl(0); Application.Terminate;
end;

procedure TFormMain.MenuItemHidePaletteClick(Sender: TObject);
begin
  MenuItemHidePalette.Checked := not(MenuItemHidePalette.Checked);
  If MenuItemHidePalette.Checked Then PanelBottom.Show
  else PanelBottom.Hide;
end;

procedure TFormMain.MenuItemHideToolsClick(Sender: TObject);
begin
  MenuItemHideTools.Checked := not(MenuItemHideTools.Checked);
  If MenuItemHideTools.Checked Then PanelTools.Show
  else PanelTools.Hide;
end;

procedure TFormMain.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  If tool = sCursor Then exit;
  isDrawing:= true;
  setLength(PBShape, length(PBShape)+1);
  PBShape[High(PBShape)]:= PaintBoxShape.Create(tool, LineSize.Position, penColor, brushColor);
  case tool of
    sPen: PBShape[High(PBShape)].editPoint(X, Y, true);
    sRectangle, sEllipse, sLine: with PBShape[High(PBShape)] do begin
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
begin
  StatusBar.Panels.Items[1].Text := intToStr(X)+ ','+intToStr(Y);
  If not(isDrawing) Then exit;
  case tool of
    sPen: with PBShape[High(PBShape)] do begin
      editPoint(X, Y, true);
    end;
    sLine, sEllipse, sRectangle: PBShape[High(PBShape)].editPoint(X, Y, false);
  end;
  PaintBox.Invalidate;
end;

procedure TFormMain.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  If tool = sCursor Then exit;
  PaintBox.Invalidate;
  Layers.addItem(shapeToString(tool),Layers);
  isDrawing:= false;
end;

procedure TFormMain.PaintBoxPaint(Sender: TObject);
var i: integer;
begin
  PaintBox.Canvas.Brush.Color:=clWhite;
  PaintBox.Canvas.FillRect(0, 0, PaintBox.Width, PaintBox.Height);
  for i:= 0 to High(PBShape) do
    PBShape[i].Paint;
end;

procedure TFormMain.SpeedButtonToolClick(Sender: TObject);
begin
  Sender:= TButton(Sender);
  If Sender = SpeedButtonCursor Then tool:= sCursor;
  If Sender = SpeedButtonPen Then tool:= sPen;
  If Sender = SpeedButtonLine Then tool:= sLine;
  If Sender = SpeedButtonPolygon Then tool:= sPolygon;
  If Sender = SpeedButtonEllipse Then tool:= sEllipse;
  If Sender = SpeedButtonRectangle Then tool:= sRectangle;
end;


end.

