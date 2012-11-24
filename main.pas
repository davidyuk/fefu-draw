unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Graphics, Forms, Controls, Dialogs, ExtCtrls,
  Menus, ComCtrls, StdCtrls, Buttons, DrawTools;

type

  { TMainF }

  TMainF = class(TForm)
    ColorDialog: TColorDialog;
    ToolsP: TPanel;
    PenWidthL: TLabel;
    MainMenu: TMainMenu;
    FileMI: TMenuItem;
    HelpMI: TMenuItem;
    ExitMI: TMenuItem;
    AboutMI: TMenuItem;
    MainPB: TPaintBox;
    LeftP: TPanel;
    PaletteP: TPanel;
    PenS: TShape;
    BrushS: TShape;
    PenWidthTB: TTrackBar;
    procedure AboutMIClick(Sender: TObject);
    procedure BrushSMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure ExitMIClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MainPBMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure MainPBMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure MainPBMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure MainPBPaint(Sender: TObject);
    procedure PenSMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PenWidthTBChange(Sender: TObject);
    procedure ToolClick(Sender: TObject);
  private
    isDrawing: boolean;
    selectedToolId: integer;
  public

  end;

var
  MainF: TMainF;

implementation

{$R *.lfm}

{ TMainF }

procedure TMainF.FormCreate(Sender: TObject);
var
  i: integer;
  b: TSpeedButton;
  m: TBitMap;
begin
  Scene := TScene.Create();
  selectedToolId := 0;
  for i := high(ToolContainer.tool) downto 0 do
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
    b.Align := alTop;
    b.Width := 30;
    b.BorderSpacing.Around := 2;
    b.OnClick := @ToolClick;
    b.Tag := i;
  end;
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
    BrushS.Brush.Color, PenWidthTB.Position);
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
  MainPB.Invalidate;
end;

procedure TMainF.MainPBPaint(Sender: TObject);
begin
  Scene.drawScene(MainPB.Canvas);
end;

procedure TMainF.PenSMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  ColorDialog.Color := PenS.Brush.Color;
  if ColorDialog.Execute then
    PenS.Brush.Color := ColorDialog.Color;
end;

procedure TMainF.PenWidthTBChange(Sender: TObject);
begin
  PenWidthL.Caption := 'Толщина пера: ' + IntToStr(PenWidthTB.Position) + 'px';
end;

procedure TMainF.ExitMIClick(Sender: TObject);
begin
  MainF.Close;
end;

procedure TMainF.AboutMIClick(Sender: TObject);
begin
  ShowMessage('Векторный редактор' + #13 +
    'Денис Давидюк Б8103А' + #13 + '23 ноября 2012');
end;

procedure TMainF.BrushSMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  ColorDialog.Color := BrushS.Brush.Color;
  if ColorDialog.Execute then
    BrushS.Brush.Color := ColorDialog.Color;
end;

end.
