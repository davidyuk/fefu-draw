unit DrawZoom;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, Forms, StdCtrls, ExtCtrls, Math, Dialogs;

type

  TRectReal = record
    Left, Right, Top, Bottom: real;
  end;

  TPointReal = record
    X, Y: real;
  end;

  { TViewport }

  TViewport = class
  const
    c_BorderMargin = 20;
  private
    Border: TRectReal;                   //общий bounding rect
    WScale: real;                        //масштаб
    WorldPos, VisPart: TPointReal;       //положение центра VP в мировых координатах,
    PBCenter: TPoint;                    //1/4 размера PB
    HrSbP, VrSbP: Integer;               //Положения SB
  public
    property Scale: real read WScale write WScale;
    function WtoS(world: TPointReal):TPoint;                          //Мировые -> Экранные
    function StoW(screen: TPoint):TPointReal;                         //Экранные -> Мировые
    procedure SetWorldPosShift(shift:TPoint);                         //Переместить VP относительно холста
    procedure ReCalculate(PaintB: TPaintbox; HrSb, VrSb: TScrollBar); //Перерасчёт размеров SB
    procedure ScaleTo(p1, p2: TPoint);                                //Вписать в VP указанную область
    procedure ScaleMouseWhell(point: TPoint; way: boolean);           //Изменить масштаб колесом прокрутки
    constructor Create();
  end;

var
  VP: TViewport;

implementation

uses
  DrawTools, DrawObjectInspector;

{ TViewport }

function TViewport.WtoS(world: TPointReal): TPoint;
begin
  result.x:= round((world.x - WorldPos.X)*WScale)+PBCenter.x;
  result.y:= round((world.y - WorldPos.y)*WScale)+PBCenter.y;
end;

function TViewport.StoW(screen: TPoint): TPointReal;
begin
  Result.x:= (screen.x - PBCenter.x)/WScale + WorldPos.x;
  Result.y:= (screen.y - PBCenter.y)/WScale + WorldPos.y;
end;

procedure TViewport.SetWorldPosShift(shift: TPoint);
begin
  WorldPos.X += shift.X/WScale;
  WorldPos.Y += shift.Y/WScale;
end;

procedure TViewport.ReCalculate(PaintB: TPaintbox; HrSb,
  VrSb: TScrollBar);
var
  i, HrSbLen, VrSbLen: integer;
  t: TRectReal;
  p1, p2, all: TPointReal;
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
    PaintB.Canvas.Pen.Color:= $ff8888;
    PaintB.Canvas.Pen.Width:= 1;
    PaintB.Canvas.Pen.Style:= psSolid;
    PaintB.Canvas.Brush.Style := bsClear;
    Border.Left -= c_BorderMargin*WScale;
    Border.Top -= c_BorderMargin*WScale;
    Border.Right += c_BorderMargin*WScale;
    Border.Bottom += c_BorderMargin*WScale;
    p1.X:= Border.Left;
    p1.Y:= Border.Top;
    p2.X:= Border.Right;
    p2.Y:= Border.Bottom;

    //установка значений ScrollBar
    HrSbLen := HrSb.Max-HrSb.Min;
    VrSbLen := VrSb.Max-VrSb.Min;
    VisPart.X:= PaintB.Width / WScale; //видимая часть
    VisPart.Y:= PaintB.Height / WScale;
    all.x := Border.Right-Border.Left+VisPart.X; //сторона холста+видимая часть
    all.y := Border.Bottom-Border.Top+VisPart.Y;
    //ОтображаемаяЧастьW / (РазмерW+ОтображаемаяЧасть) * ДиаппазонЗначений
    HrSb.PageSize := trunc(VisPart.X  / all.x * HrSbLen);
    VrSb.PageSize := trunc(VisPart.Y  / all.y * VrSbLen);
    //Расстояние(От меньшего края до позиции точки)/(РазмерW+ОтображаемаяЧасть) * ДиаппазонЗначений
    if HrSb.PageSize < HrSb.Max Then begin
      HrSb.Visible:= true;
      If HrSb.Position <> HrSbP Then WorldPos.X:= Border.Left+(Border.Right-Border.Left)/(HrSbLen-HrSb.PageSize)*HrSb.position
      else HrSb.Position := trunc((WorldPos.X - Border.Left)/all.x*HrSbLen);
    end else HrSb.Visible:= false;
    if VrSb.PageSize < VrSb.Max Then begin
      VrSb.Visible:= true;
      If VrSb.Position <> VrSbP Then WorldPos.Y:= Border.Top+(Border.Bottom-Border.Top)/(VrSbLen-VrSb.PageSize)*VrSb.position
      else VrSb.Position := trunc((WorldPos.Y - Border.Top)/all.y*VrSbLen);
    end else VrSb.Visible:= false;
    HrSbP:= HrSb.Position;
    VrSbP:= VrSb.Position;

    PaintB.Canvas.Rectangle(VP.WtoS(p1).X, VP.WtoS(p1).Y, VP.WtoS(p2).X, VP.WtoS(p2).Y);
  end else begin
    HrSb.Visible:= false;
    VrSb.Visible:= false;
  end;
end;

procedure TViewport.ScaleTo(p1, p2: TPoint);
var p1r, p2r: TPointReal;
begin
  if (p1.x = p2.x) and (p1.y = p2.y) then begin
    p1r.x:= Border.Left;
    p1r.Y:= Border.Top;
    p2r.x:= Border.Right;
    p2r.Y:= Border.Bottom;
  end else begin
    p1r:= StoW(p1);
    p2r:= StoW(p2);
  end;
  WorldPos.X := (p1r.X + p2r.X)/2;
  WorldPos.Y := (p1r.Y + p2r.Y)/2;
  if (abs(p1r.X - p2r.X)/VisPart.X) < (abs(p1r.Y - p2r.Y)/VisPart.Y) then
    WScale:= (VisPart.Y*WScale)/abs(p1r.Y-p2r.Y)
  else WScale:= (VisPart.X*WScale)/abs(p1r.X-p2r.X);
  Inspector.Refresh;
end;

procedure TViewport.ScaleMouseWhell(point: TPoint; way: boolean);
var
  step: real;
  t1, t2: TPointReal;
begin
  t1:= VP.StoW(point);
  step:= 2;
  if not way then step := 1/step;
  if (VP.Scale < 0.03) and (not way) Then exit;
  WScale := WScale * step;
  t2 := VP.StoW(point);
  WorldPos.x:= WorldPos.x - (t2.x-t1.x);
  WorldPos.y:= WorldPos.y - (t2.y-t1.y);
end;

constructor TViewport.Create();
begin
  WScale := 1;
  WorldPos.x:= 0;
  WorldPos.y:= 0;
end;

end.

