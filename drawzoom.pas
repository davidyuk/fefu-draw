unit DrawZoom;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, StdCtrls, ExtCtrls, Math, DrawTypes;

type

  { TViewport }

  TViewport = class
  const
    maxScale = 100;
    minScale = 0.03;
  private
    scl: Double;                        //масштаб
    WorldPos: TPointFloat;              //положение центра VP в мировых координатах,
    HrSbP, VrSbP: Integer;              //Положения SB
    VPsize: TPoint;                     //Размер видимой части
    procedure SetScale(AValue: double);
  public
    property Scale: double read scl write SetScale;
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
  DrawObjectInspector, DrawScene;

{ TViewport }

procedure TViewport.SetScale(AValue: double);
var
  p1, p2: TPointFloat;
begin
  if (scl=AValue) or (AValue>maxScale) or (AValue<minScale) then Exit;
  p1:= StoW(Point(VPsize.x, VPsize.y));
  scl:= AValue;
  p2:= StoW(Point(VPsize.x, VPsize.y));
  WorldPos += (p1-p2)/2;
end;

function TViewport.WtoS(world: TPointFloat): TPoint;
begin
  result.x:= trunc((world.x - WorldPos.X)*scl);
  result.y:= trunc((world.y - WorldPos.y)*scl);
end;

function TViewport.StoW(screen: TPoint): TPointFloat;
begin
  Result.x:= screen.x/scl + WorldPos.x;
  Result.y:= screen.y/scl + WorldPos.y;
end;

function TViewport.WtoS(world: TRectFloat): TRect;
begin
  result:= Rect(WtoS(PointFloat(world.Left, world.Top)), WtoS(PointFloat(world.Right, world.Bottom)));
end;

function TViewport.StoW(world: TRect): TRectFloat;
begin
  Result:= RectFloat(StoW(Point(world.Left, world.Top)), StoW(Point(world.Right, world.Bottom)));
end;

procedure TViewport.SetWorldPosShift(shift: TPoint);
begin
  WorldPos.X += shift.X/scl;
  WorldPos.Y += shift.Y/scl;
end;

procedure TViewport.ReCalculate(PaintB: TPaintbox; HrSb,
  VrSb: TScrollBar);
var
  worldSize: TPointFloat;
  pf1, pf2: TPointFloat;
  br: TRectFloat;
  a: integer;
begin
  VPsize.X:= PaintB.Width;
  VPsize.Y:= PaintB.Height;

  br := Scene.GetImageSize;

  pf1:= StoW(Point(0, 0));
  if br.Left > pf1.x Then br.left := pf1.x;
  if br.Top > pf1.y Then br.Top := pf1.y;
  pf2:= StoW(Point(PaintB.Width, PaintB.Height));
  if br.Right < pf2.x Then br.Right := pf2.x;
  if br.Bottom < pf2.y Then br.Bottom := pf2.y;
  worldSize.X := br.Right-br.Left;
  worldSize.Y := br.Bottom-br.Top;
  if worldSize.x * worldSize.y = 0 Then exit;

  a:= HrSb.Max-HrSb.Min;
  HrSb.PageSize := round(PaintB.Width / (worldSize.X*scl) * a);
  if HrSb.PageSize >= a Then
    HrSb.Visible := false
  else begin
    HrSb.Visible := true;
    if (HrSbP = HrSb.Position) Then
      HrSb.Position := round((WorldPos.X - br.Left)/worldSize.X *a)
    else
      WorldPos.X := HrSb.Position/a*worldSize.X+br.Left;
    HrSbP:= HrSb.Position;
  end;

  a:= VrSb.Max-VrSb.Min;
  VrSb.PageSize := round(PaintB.Height / (worldSize.Y*scl) *a);
  if VrSb.PageSize >= a Then
    VrSb.Visible:= false
  else begin
    VrSb.Visible:= true;
    if (VrSbP = VrSb.Position) Then
      VrSb.Position := round((WorldPos.Y - br.Top)/worldSize.Y *a)
    else
      WorldPos.Y := VrSb.Position/a*worldSize.Y+br.Top;
    VrSbP:= VrSb.Position;
  end;
end;

procedure TViewport.ScaleTo(p1, p2: TPoint);
var
  p1r, p2r: TPointFloat;
  t: TRectFloat;
begin
  if (p1.x = p2.x) and (p1.y = p2.y) then begin
    t:= Scene.GetImageSize;
    p1r.x:= t.Left;
    p1r.Y:= t.Top;
    p2r.x:= t.Right;
    p2r.Y:= t.Bottom;
  end else begin
    p1r:= StoW(p1);
    p2r:= StoW(p2);
  end;
  scl := min(VPsize.Y/abs(p1r.Y-p2r.Y), VPsize.X/abs(p1r.X-p2r.X));
  if scl < minScale Then scl := minScale;
  if scl > maxScale Then scl := maxScale;
  if p1r < p2r Then
    WorldPos:= p1r - (PointFloat(VPsize)/scl-(p2r-p1r))/2
  else
    WorldPos:= p2r - (PointFloat(VPsize)/scl-(p1r-p2r))/2;
  Inspector.Refresh;
end;

procedure TViewport.ScaleMouseWhell(point: TPoint; way: boolean);
var t: TPointFloat;
begin
  if ((VP.Scale < minScale) and (not way)) or ((VP.Scale > maxScale) and way) Then exit;
  t:= VP.StoW(point);
  if not way then scl /= 2
  else scl *= 2;
  WorldPos:= WorldPos - (VP.StoW(point)-t);
end;

constructor TViewport.Create();
begin
  scl := 1;
  WorldPos.x:= 0;
  WorldPos.y:= 0;
end;

end.

