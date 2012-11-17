unit AddObjects;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, Menus, Buttons, StdCtrls, DrawShape;

type

  { TFParam }

  TFParam = class
    private
      PenPre, BrushPre: TShape;
      LinePre: TTrackBar;
      LineText: TLabel;
      ColorText: TStatusPanel;
    public
      sel: TFBase;
      procedure Update;
      procedure PenSet(c: TColor);
      procedure BrushSet(c: TColor);
      procedure LineSet(s: integer);
      function PenGet():TColor;
      function BrushGet():TColor;
      function LineGet():integer;
      constructor Create(penP, brushP: TShape; lineP: TTrackBar; s: TFBase; lineT: TLabel; colorT: TStatusPanel);
  end;



implementation

{ TFParam }

procedure TFParam.Update;
begin
  If sel <> nil then begin
    PenPre.Brush.Color:= sel.penColor;
    BrushPre.Brush.Color:= sel.brushColor;
    LinePre.Position:= sel.lineWidth;
  end;
  ColorText.Text := intToHex(PenPre.Brush.Color, 6)+','+intToHex(BrushPre.Brush.Color, 6);
  LineText.Caption := 'Толщина линии - '+intToStr(LinePre.Position)+'px';
end;

procedure TFParam.PenSet(c: TColor);
begin
  PenPre.Brush.Color:= c;
  If sel <> nil then sel.penColor:= c;
  ColorText.Text := intToHex(PenPre.Brush.Color, 6)+','+intToHex(BrushPre.Brush.Color, 6);
end;

procedure TFParam.BrushSet(c: TColor);
begin
  BrushPre.Brush.Color:= c;
  If sel <> nil then sel.brushColor:= c;
  ColorText.Text := intToHex(PenPre.Brush.Color, 6)+','+intToHex(BrushPre.Brush.Color, 6);
end;

procedure TFParam.LineSet(s: integer);
begin
  if s <> -1 Then LinePre.Position := s;
  LineText.Caption := 'Толщина линии - '+intToStr(LinePre.Position)+'px';
  if sel <> nil then sel.lineWidth:=LinePre.Position;
end;

function TFParam.PenGet: TColor;
begin
  result:= PenPre.Brush.Color;
end;

function TFParam.BrushGet: TColor;
begin
  result:= BrushPre.Brush.Color;
end;

function TFParam.LineGet: integer;
begin
  result:= LinePre.Position;
end;

constructor TFParam.Create(penP, brushP: TShape; lineP: TTrackBar; s: TFBase;
  lineT: TLabel; colorT: TStatusPanel);
begin
  PenPre:= penP;
  BrushPre:= brushP;
  LinePre:= lineP;
  sel:= s;
  LinePre.Position:= 1;
  PenPre.Brush.Color:= $0;
  BrushPre.Brush.Color:= $ffffff;
  LineText:= lineT;
  ColorText:= colorT;
  Update();
end;

end.

