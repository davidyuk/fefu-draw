unit DrawZoom;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, Forms, StdCtrls, ExtCtrls;

type

  TRectReal = record
    Left, Right, Top, Bottom: real;
  end;

  TPointReal = record
    X, Y: real;
  end;

  ProcedureOfObject = procedure of object;

  { TViewport }

  TViewport = class
  const
    c_BorderMargin = 10;
  private
    Invalid: ProcedureOfObject;
    Border: TRectReal;
    WScale: real;
    WorldPos, VisPart: TPointReal;
    PBCenter: TPoint;
    HrSbLen, VrSbLen: Integer;
    invBySb: boolean;
  public
    function WtoS(world: TPointReal):TPoint;
    function StoW(screen: TPoint):TPointReal;
    procedure SetScale(Scale: real);
    function GetScale: real;
    procedure SetSb(position: TPoint);
    procedure SetWorldPosShift(shift:TPoint);
    function GetWorldPosShift:TPointReal;
    procedure ReCalculate(PaintB: TPaintbox; HrSb, VrSb: TScrollBar);
    procedure ScaleTo(p1, p2: TPoint);
    procedure ScaleMouseWhell(point: TPoint; way: boolean);
    constructor Create(invalidate: ProcedureOfObject);
  end;

var
  VP: TViewport;

implementation

uses
  DrawTools;

{ TViewport }

function TViewport.WtoS(world: TPointReal): TPoint;
begin
  result.x:= trunc((world.x - WorldPos.X)*WScale)+PBCenter.x;
  result.y:= trunc((world.y - WorldPos.y)*WScale)+PBCenter.y;
end;

function TViewport.StoW(screen: TPoint): TPointReal;
begin
  Result.x:= (screen.x - PBCenter.x)/WScale + WorldPos.x;
  Result.y:= (screen.y - PBCenter.y)/WScale + WorldPos.y;
end;

procedure TViewport.SetScale(Scale: real);
begin
  WScale:= Scale;
  Invalid;
end;

function TViewport.GetScale: real;
begin
  result:= WScale;
end;

procedure TViewport.SetSb(position: TPoint);
begin
  if position.x <> 0 Then WorldPos.X:= Border.Left+(Border.Right-Border.Left)/HrSbLen*position.x;
  if position.y <> 0 Then WorldPos.Y:= Border.Top+(Border.Bottom-Border.Top)/VrSbLen*position.y;
  invBySb := true;
  invalid;
end;

procedure TViewport.SetWorldPosShift(shift: TPoint);
begin
  WorldPos.X += shift.X/WScale;
  WorldPos.Y += shift.Y/WScale;
  invalid;
end;

function TViewport.GetWorldPosShift: TPointReal;
begin
  Result.X := WorldPos.X;
  Result.Y := WorldPos.Y;
end;

procedure TViewport.ReCalculate(PaintB: TPaintbox; HrSb,
  VrSb: TScrollBar);
var
  i: integer;
  t: TRectReal;
  p1, p2: TPointReal;
begin
  PBCenter.X:= PaintB.Width div 2;
  PBCenter.Y:= PaintB.Height div 2;
  //вычисление и отрисовка общего BoundingRect
  if length(Scene.Shapes) <> 0 Then begin
    Border:= Scene.Shapes[0].BoundingRect;
    for i:= 0 to High(Scene.Shapes) do begin
      t:= Scene.Shapes[i].BoundingRect;
      if Border.Left > t.Left Then Border.Left:= t.Left;
      if Border.Right < t.Right Then Border.Right:= t.Right;
      if Border.Top > t.Top Then Border.Top:= t.Top;
      if Border.Bottom < t.Bottom Then Border.Bottom:= t.Bottom;
    end;
  end else begin
    HrSb.PageSize:= HrSb.Max-HrSb.Min;
    VrSb.PageSize:= VrSb.Max-VrSb.Min;
    exit;
  end;
  PaintB.Canvas.Pen.Color:= $bb99bb;
  PaintB.Canvas.Pen.Width:= 1;
  PaintB.Canvas.Pen.Style:= psSolid;
  PaintB.Canvas.Brush.Style := bsClear;
  PaintB.Canvas.Brush.Color := $eeeeee;
  Border.Left -= c_BorderMargin/WScale;
  Border.Top -= c_BorderMargin/WScale;
  Border.Right += c_BorderMargin/WScale;
  Border.Bottom += c_BorderMargin/WScale;
  p1.X:= Border.Left;
  p1.Y:= Border.Top;
  p2.X:= Border.Right;
  p2.Y:= Border.Bottom;
  PaintB.Canvas.Rectangle(VP.WtoS(p1).X, VP.WtoS(p1).Y, VP.WtoS(p2).X, VP.WtoS(p2).Y);
  //установка значений ScrollBar
  if invBySb Then invBySb:= false //invalidation by scrollBar
  else begin
    HrSbLen := HrSb.Max-HrSb.Min;
    VrSbLen := VrSb.Max-VrSb.Min;
    VisPart.X:= PaintB.Width / WScale;
    VisPart.Y:= PaintB.Height / WScale;
    t.Left := Border.Right-Border.Left+VisPart.X;
    t.Top := Border.Bottom-Border.Top+VisPart.Y;

    //ОтображаемаяЧастьW / (РазмерW+ОтображаемаяЧасть) * ДиаппазонЗначений
    HrSb.PageSize := trunc(VisPart.X  / t.Left * HrSbLen);
    VrSb.PageSize := trunc(VisPart.Y  / t.Top * VrSbLen);
    //Расстояние(От меньшего края до позиции точки)/(РазмерW+ОтображаемаяЧасть) * ДиаппазонЗначений
    if HrSb.PageSize < HrSb.Max Then HrSb.Position := trunc((WorldPos.X - Border.Left)/t.Left*HrSbLen);
    if VrSb.PageSize < VrSb.Max Then VrSb.Position := trunc((WorldPos.Y - Border.Top)/t.Top*VrSbLen);
    HrSbLen := HrSbLen-HrSb.PageSize; //Используется в SetSb
    VrSbLen := VrSbLen-VrSb.PageSize;
  end;
end;

procedure TViewport.ScaleTo(p1, p2: TPoint);
var p1r, p2r: TPointReal;
begin
  if (p1.x + p1.y + p2.x + p2.y) <> 0 then begin
    p1r:= StoW(p1);
    p2r:= StoW(p2);
  end else begin
    p1r.x:= Border.Left;
    p1r.Y:= Border.Top;
    p2r.x:= Border.Right;
    p2r.Y:= Border.Bottom;
  end;
  WorldPos.X := (p1r.X + p2r.X)/2;
  WorldPos.Y := (p1r.Y + p2r.Y)/2;
  if abs(p1r.X - p2r.X) > abs(p1r.Y - p2r.Y) then
    SetScale(VisPart.X*WScale/abs(p1r.X - p2r.X)*0.9)
  else SetScale(VisPart.Y*WScale/abs(p1r.Y - p2r.Y)*0.9);
end;

procedure TViewport.ScaleMouseWhell(point: TPoint; way: boolean);
var
  step: real;
  t, p: TPoint;
  t1, t2: TPointReal;
begin
  t1:= VP.StoW(point);
  step:= 0.2;
  if not way then step *= -1;
  if (GetScale < 0.25) and (step<0) Then exit;
  WScale += step;
  t2.x:= (point.x - PBCenter.x)/WScale + WorldPos.x;
  t2.y:= (point.y - PBCenter.y)/WScale + WorldPos.y;
  WorldPos.x:= WorldPos.x - (t2.x-t1.x);
  WorldPos.y:= WorldPos.y - (t2.y-t1.y);
  invalid;
end;

constructor TViewport.Create(invalidate: ProcedureOfObject);
begin
  Invalid := Invalidate;
  WScale := 1;
  WorldPos.x:= 0;
  WorldPos.y:= 0;
end;

end.

