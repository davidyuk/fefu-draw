unit DrawTypes;

{$mode objfpc}{$H+}

{
Этот модуль содержит описание дополнительных типов данных, используемых в приложении,
описание перегруженных методов и функций для работы с ними.
Состояние: более-менее, есть логические ошибки.
}

interface

uses
  Classes, SysUtils, Math;

type
  { TRectFloat }

  TRectFloat = record
    Left, Right, Top, Bottom: double;
  end;

  { TPointFloat }

  TPointFloat = record
    X, Y: double;
  end;

  TPointFloatArr = array of TPointFloat;

function PointFloat(p1, p2: Double): TPointFloat;
function PointFloat(p: TPoint): TPointFloat;
function Rect(p1, p2: TPoint):TRect; overload;
function RectReal(p1, p2: TPointFloat):TRectFloat;
function Intersection(a1, a2, b1, b2: TPoint): boolean;
function Intersection(rect: TRect; p1, p2: TPoint): boolean; overload;
function Intersection(rect1, rect2: TRect): boolean; overload;
function IsPointIn(p, p1, p2: TPoint):boolean;
function IsPointIn(p: TPoint; rect: TRect):boolean; overload;
function IsRectIn(rect1, rect2: TRect): boolean;
operator +(a, b: TPointFloat):TPointFloat;
operator -(a, b: TPointFloat):TPointFloat;
operator +(a, b: TPoint):TPoint;
operator -(a, b: TPoint):TPoint;
operator >(a, b: TPointFloat):boolean;
operator <(a, b: TPointFloat):boolean;
operator >=(a, b: TPointFloat):boolean;
operator <=(a, b: TPointFloat):boolean;
operator /(a: TPointFloat; b: double):TPointFloat;
operator *(a: TPointFloat; b: Double):TPointFloat;

implementation

function PointFloat(p1, p2: Double): TPointFloat;
begin
  Result.X := p1;
  Result.Y := p2;
end;

function RectReal(p1, p2: TPointFloat): TRectFloat;
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

operator+(a, b: TPointFloat): TPointFloat;
begin
  result.x:= a.X + b.X;
  result.y:= a.Y + b.Y;
end;

operator-(a, b: TPointFloat): TPointFloat;
begin
  result.x:= a.X - b.X;
  result.y:= a.Y - b.Y;
end;

operator-(a, b: TPoint): TPoint;
begin
  result.x:= a.X - b.X;
  result.x:= a.Y - b.Y;
end;

operator>(a, b: TPointFloat): boolean;
begin
  result:= (a.x > b.x) and (a.y > b.y);
end;

operator<(a, b: TPointFloat): boolean;
begin
  result:= (a.x < b.x) and (a.y < b.y);
end;

operator>=(a, b: TPointFloat): boolean;
begin
  result:= (a.x >= b.x) and (a.y >= b.y);
end;

operator<=(a, b: TPointFloat): boolean;
begin
  result:= (a.x <= b.x) and (a.y <= b.y);
end;

operator/(a: TPointFloat; b: double): TPointFloat;
begin
  result.x := a.X / b;
  result.y := a.Y / b;
end;

operator+(a, b: TPoint): TPoint;
begin
  result.x := a.x+b.x;
  result.y := a.y+b.y;
end;

operator*(a: TPointFloat; b: Double): TPointFloat;
begin
  result.x := a.x*b;
  result.y := a.y*b;
end;

function PointFloat(p: TPoint): TPointFloat;
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

