unit DrawZoom;

{$mode objfpc}{$H+}

{
Этот модуль содержит описание класса для преобразования координат, описание функции обновления позиций scroll bar'ов.
Состояние: требуется переработка.
}

interface

uses
  Classes, SysUtils, Controls, Graphics, Forms, StdCtrls, ExtCtrls, Math, DrawTypes;

type

  { TViewport }

  TViewport = class
  const
    c_BorderMargin = 20;
  private
    Border: TRectFloat;                   //общий bounding rect
    scl: Double;                        //масштаб
    WorldPos, VisPart: TPointFloat;       //положение центра VP в мировых координатах,
    PBCenter: TPoint;                    //1/4 размера PB
    HrSbP, VrSbP: Integer;               //Положения SB
  public
    property Scale: real read scl write scl;
    function WtoS(world: TPointFloat):TPoint;                          //Мировые -> Экранные
    function StoW(screen: TPoint):TPointFloat;                         //Экранные -> Мировые
    function WtoS(world: TRectFloat):TRect; overload;
    function StoW(world: TRect):TRectFloat; overload;
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
  DrawTools, DrawObjectInspector, DrawScene;

{ TViewport }

function TViewport.WtoS(world: TPointFloat): TPoint;
begin
  result.x:= round((world.x - WorldPos.X)*scl)+PBCenter.x;
  result.y:= round((world.y - WorldPos.y)*scl)+PBCenter.y;
end;

function TViewport.StoW(screen: TPoint): TPointFloat;
begin
  Result.x:= (screen.x - PBCenter.x)/scl + WorldPos.x;
  Result.y:= (screen.y - PBCenter.y)/scl + WorldPos.y;
end;

function TViewport.WtoS(world: TRectFloat): TRect;
begin
  result:= Rect(WtoS(PointFloat(world.Left, world.Top)), WtoS(PointFloat(world.Right, world.Bottom)));
end;

function TViewport.StoW(world: TRect): TRectFloat;
begin
  Result:= RectReal(StoW(Point(world.Left, world.Top)), StoW(Point(world.Right, world.Bottom)));
end;

procedure TViewport.SetWorldPosShift(shift: TPoint);
begin
  WorldPos.X += shift.X/scl;
  WorldPos.Y += shift.Y/scl;
end;

procedure TViewport.ReCalculate(PaintB: TPaintbox; HrSb,
  VrSb: TScrollBar);
var
  i, HrSbLen, VrSbLen: integer;
  t: TRectFloat;
  p1, p2, all: TPointFloat;
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
    Border.Left -= c_BorderMargin*scl;
    Border.Top -= c_BorderMargin*scl;
    Border.Right += c_BorderMargin*scl;
    Border.Bottom += c_BorderMargin*scl;
    p1.X:= Border.Left;
    p1.Y:= Border.Top;
    p2.X:= Border.Right;
    p2.Y:= Border.Bottom;

    //установка значений ScrollBar
    HrSbLen := HrSb.Max-HrSb.Min;
    VrSbLen := VrSb.Max-VrSb.Min;
    VisPart.X:= PaintB.Width / scl; //видимая часть
    VisPart.Y:= PaintB.Height / scl;
    all.x := Border.Right-Border.Left+VisPart.X; //сторона холста+видимая часть
    all.y := Border.Bottom-Border.Top+VisPart.Y;
    //ОтображаемаяЧастьW / (РазмерW+ОтображаемаяЧасть) * ДиаппазонЗначений
    HrSb.PageSize := trunc(VisPart.X  / all.x * HrSbLen);
    VrSb.PageSize := trunc(VisPart.Y  / all.y * VrSbLen);
    //Расстояние(От меньшего края до позиции точки)/(РазмерW+ОтображаемаяЧасть) * ДиаппазонЗначений
    if HrSb.PageSize < HrSb.Max Then begin
      HrSb.Visible:= true;
      If HrSb.Position <> HrSbP Then
        WorldPos.X:= Border.Left+(Border.Right-Border.Left)/(HrSbLen-HrSb.PageSize)*HrSb.position
      else
        HrSb.Position := trunc((WorldPos.X - Border.Left)/all.x*HrSbLen);
    end else HrSb.Visible:= false;
    if VrSb.PageSize < VrSb.Max Then begin
      VrSb.Visible:= true;
      If VrSb.Position <> VrSbP Then
        WorldPos.Y:= Border.Top+(Border.Bottom-Border.Top)/(VrSbLen-VrSb.PageSize)*VrSb.position
      else
        VrSb.Position := trunc((WorldPos.Y - Border.Top)/all.y*VrSbLen);
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
var p1r, p2r: TPointFloat;
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
  WorldPos := (p1r +p2r ) /2;
  if (abs(p1r.X - p2r.X)/VisPart.X) < (abs(p1r.Y - p2r.Y)/VisPart.Y) then
    scl:= (VisPart.Y*scl)/abs(p1r.Y-p2r.Y)
  else scl:= (VisPart.X*scl)/abs(p1r.X-p2r.X);
  Inspector.Refresh;
end;

procedure TViewport.ScaleMouseWhell(point: TPoint; way: boolean);
var
  step: real;
  t1, t2: TPointFloat;
begin
  t1:= VP.StoW(point);
  step:= 2;
  if not way then step := 1/step;
  if (VP.Scale < 0.03) and (not way) Then exit;
  scl := scl * step;
  t2 := VP.StoW(point);
  WorldPos:= WorldPos - (t2-t1);
end;

constructor TViewport.Create();
begin
  scl := 1;
  WorldPos.x:= 0;
  WorldPos.y:= 0;
end;

end.

