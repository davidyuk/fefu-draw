unit DrawTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

type


  { TRectReal }

  TRectReal = record
    Left, Right, Top, Bottom: double;
  end;

  { TPointReal }

  TPointReal = record
    X, Y: double;
  end;

  TPointRealArr = array of TPointReal;

function PointReal(p1, p2: Double): TPointReal;
function PointReal(p: TPoint): TPointReal;
function Rect(p1, p2: TPoint):TRect; overload;
function RectReal(p1, p2: TPointReal):TRectReal;
function Intersection(a1, a2, b1, b2: TPoint): boolean;
function Intersection(rect: TRect; p1, p2: TPoint): boolean; overload;
function Intersection(rect1, rect2: TRect): boolean; overload;
function IntersectionWithEllipse(line1, line2, ellipsePos, ellipseSize: TPoint): boolean;
function IsPointIn(p, p1, p2: TPoint):boolean;
function IsPointIn(p: TPoint; rect: TRect):boolean; overload;
function IsRectIn(rect1, rect2: TRect): boolean;
operator +(a, b: TPointReal):TPointReal;
operator -(a, b: TPointReal):TPointReal;
operator +(a, b: TPoint):TPoint;
operator -(a, b: TPoint):TPoint;
operator >(a, b: TPointReal):boolean;
operator <(a, b: TPointReal):boolean;
operator >=(a, b: TPointReal):boolean;
operator <=(a, b: TPointReal):boolean;
operator /(a: TPointReal; b: double):TPointReal;
operator *(a: TPointReal; b: Double):TPointReal;

implementation

function PointReal(p1, p2: Double): TPointReal;
begin
  Result.X := p1;
  Result.Y := p2;
end;

function RectReal(p1, p2: TPointReal): TRectReal;
begin
  Result.Left:= p1.x;
  Result.Top:= p1.y;
  Result.Right:= p2.x;
  Result.Bottom:= p2.y;
end;

function Intersection(a1, a2, b1, b2: TPoint): boolean;
var
  ca1, cb1, cc1, ca2, cb2, cc2: Integer;
  intersec: TPoint;
begin
  result:= false;
  if (abs(a1.x - a2.x) < 2) and (abs(a1.y - a2.y) < 2) or
     (abs(b1.x - b2.x) < 2) and (abs(b1.y - b2.y) < 2) Then exit;
  ca1:= a1.y-a2.y;
  cb1:= a2.x-a1.x;
  cc1:= a1.x*a2.y-a1.y*a2.x;
  ca2:= b1.y-b2.y;
  cb2:= b2.x-b1.x;
  cc2:= b1.x*b2.y-b1.y*b2.x;
  if ca2*cb2*cc2 <> 0 Then begin
    if (ca1/ca2 = cb1/cb2) and (ca1/ca2 <> cc1/cc2) then begin //прямые параллельны
      exit;
    end;
    if (ca1/ca2 = cb1/cb2) and (ca1/ca2 = cc1/cc2) then begin //прямые совпадают
      if IsPointIn(a1, b1, b2) or IsPointIn(a2, b1, b2) Then result:= true;
      if IsPointIn(a1, b1, b2) and IsPointIn(a2, b1, b2) Then result:= true;
      If IsPointIn(b1, a1, a2) and IsPointIn(b2, a1, a2) Then Result:= true;
      exit;
    end;
  end;
  if (ca1*cb2-ca2*cb1) = 0 then
    exit;
  intersec.x:= Round((cc2*cb1-cc1*cb2)/(ca1*cb2-ca2*cb1));
  intersec.y:= Round((cc1*ca2-ca1*cc2)/(ca1*cb2-ca2*cb1));
  if IsPointIn(intersec, a1, a2) and IsPointIn(intersec, b1, b2) Then result:= true;
  {c1:= b2.x-b1.x; //расстояние между точками второй прямой в проекции на оx
  c2:= b2.y-b1.y; //расстояние между точками второй прямой в проекции на оy
  c3:= a2.x-a1.x; //расстояние между точками первой прямой в проекции на оx
  c4:= a2.y-a1.y; //расстояние между точками первой прямой в проекции на оy

  v1:=c1*(a1.y-b1.y)-c2*(a1.x-b1.x);
  v2:=c1*(a2.y-b1.y)-c2*(a2.x-b1.x);
  v3:=c3*(b1.y-a1.y)-c4*(b1.x-a1.x);
  v4:=c3*(b2.y-a1.y)-c4*(b2.x-a1.x);

  v1:=(b2.x-b1.x)*(a1.y-b1.y)-(b2.y-b1.y)*(a1.x-b1.x);
  v2:=(b2.x-b1.x)*(a2.y-b1.y)-(b2.y-b1.y)*(a2.x-b1.x);
  v3:=(a2.x-a1.x)*(b1.y-a1.y)-(a2.y-a1.y)*(b1.x-a1.x);
  v4:=(a2.x-a1.x)*(b2.y-a1.y)-(a2.y-a1.y)*(b2.x-a1.x);
  Result:=(v1*v2<0) and (v3*v4<0);}
end;

function Intersection(rect: TRect; p1, p2: TPoint): boolean;
begin
  if IsPointIn(p1, rect) or IsPointIn(p2, rect) Then begin
    result:= true;
    exit;
  end;
  result:= Intersection(Point(rect.Left, rect.Top), Point(rect.Right, rect.Top), p1, p2) or
           Intersection(Point(rect.Left, rect.Bottom), Point(rect.Right, rect.Bottom), p1, p2) or
           Intersection(Point(rect.Left, rect.Top), Point(rect.Left, rect.Bottom), p1, p2) or
           Intersection(Point(rect.Right, rect.Top), Point(rect.Right, rect.Bottom), p1, p2);
end;

function Intersection(rect1, rect2: TRect): boolean;
begin
  result:= true;
  if IsRectIn(rect1, rect2) or IsRectIn(rect2, rect1) then exit;
  result:= Intersection(rect2, Point(rect1.Left, rect1.Top), Point(rect1.Right, rect1.Top)) or
           Intersection(rect2, Point(rect1.Left, rect1.Bottom), Point(rect1.Right, rect1.Bottom)) or
           Intersection(rect2, Point(rect1.Left, rect1.Top), Point(rect1.Left, rect1.Bottom)) or
           Intersection(rect2, Point(rect1.Right, rect1.Top), Point(rect1.Right, rect1.Bottom));
end;

function IntersectionWithEllipse(line1, line2, ellipsePos, ellipseSize: TPoint
  ): boolean;
var
  lA, lB, lC, eA, eB: integer;
  a, b, c, D: double;
  inters, inters2: TPoint;
begin
  //неудалась функция
  result:= false;
  if (abs(line1.x - line2.x) < 2) and (abs(line1.y - line2.y) < 2) Then exit;
  line1.x -= ellipsePos.x;
  line2.x -= ellipsePos.x;
  line1.y -= ellipsePos.y;
  line2.y -= ellipsePos.y;
  lA:= line1.y-line2.y;
  lB:= line2.x-line1.x;
  lC:= line1.x*line2.y-line1.y*line2.x;

  eA:= round(ellipseSize.x / 2);
  eB:= round(ellipseSize.y / 2);

  a:= sqr(eB) + sqr(lA)*sqr(eA)/sqr(lB);
  b:= (lA*lC*sqr(eA)*-2)/sqr(lB);
  c:= sqr(lC)*sqr(eA)/sqr(lB)-sqr(eA)*sqr(eB);
  D:= sqrt(sqr(b)-4*a*c);

  try
  inters.x:= round((-b+D)/(2*a));
  inters2.x:= round((-b-D)/(2*a));
  inters.y:= round((-lC-lA*inters.x)/lB);
  inters2.y:= round((-lC-lA*inters.x)/lB);
  except
    result:= false;
    exit;
  end;

  result:= IsPointIn(inters, line1, line2) or IsPointIn(inters2, line1, line2);
end;

function IsPointIn(p, p1, p2: TPoint): boolean;
begin
  result:= (p.X <= Max(p1.x, p2.x)) and (p.X >= min(p1.x, p2.x)) and
       (p.Y <= Max(p1.y, p2.y)) and (p.Y >= min(p1.y, p2.y));
end;

function IsPointIn(p: TPoint; rect: TRect): boolean;
begin
  result:= IsPointIn(p, Point(rect.Left,rect.Top), Point(rect.Right, rect.Bottom));
end;

function IsRectIn(rect1, rect2: TRect): boolean;
begin
  result:= IsPointIn(Point(rect1.left, rect1.top), rect2) or
           IsPointIn(Point(rect1.right, rect1.top), rect2) or
           IsPointIn(Point(rect1.left, rect1.bottom), rect2) or
           IsPointIn(Point(rect1.right, rect1.bottom), rect2);
end;

operator+(a, b: TPointReal): TPointReal;
begin
  a.X += b.X;
  a.Y += b.Y;
end;

operator-(a, b: TPointReal): TPointReal;
begin
  a.X -= b.X;
  a.Y -= b.Y;
end;

operator-(a, b: TPoint): TPoint;
begin
  a.X -= b.X;
  a.Y -= b.Y;
end;

operator>(a, b: TPointReal): boolean;
begin
  result:= (a.x > b.x) and (a.y > b.y);
end;

operator<(a, b: TPointReal): boolean;
begin
  result:= (a.x < b.x) and (a.y < b.y);
end;

operator>=(a, b: TPointReal): boolean;
begin
  result:= (a.x >= b.x) and (a.y >= b.y);
end;

operator<=(a, b: TPointReal): boolean;
begin
  result:= (a.x <= b.x) and (a.y <= b.y);
end;

operator/(a: TPointReal; b: double): TPointReal;
begin
  a.X := a.X / b;
  a.Y := a.Y / b;
end;

operator+(a, b: TPoint): TPoint;
begin
  a.X += b.X;
  a.Y += b.Y;
end;

operator*(a: TPointReal; b: Double): TPointReal;
begin
  a.X *= b;
  a.Y *= b;
end;

function PointReal(p: TPoint): TPointReal;
begin
  Result.X := p.x;
  Result.Y := p.y;
end;

function Rect(p1, p2: TPoint): TRect;
begin
  Result.Left:= Min(p1.x,p2.x);
  Result.Top:= Min(p1.y,p2.y);
  Result.Right:= Max(p1.x,p2.x);
  Result.Bottom:= Max(p1.y,p2.y);
end;

end.

