unit DrawTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TRectReal }

  TRectReal = record
    Left, Right, Top, Bottom: double;
  end;

  { TPointReal }

  TPointReal = record
    X, Y: double;
  end;

function PointReal(p1, p2: Double): TPointReal;
function PointReal(p: TPoint): TPointReal;
function Rect(p1, p2: TPoint):TRect; overload;
function RectReal(p1, p2: TPointReal):TRectReal;
operator +(a, b: TPointReal):TPointReal;
operator -(a, b: TPointReal):TPointReal;
operator +(a, b: TPoint):TPoint;
operator -(a, b: TPoint):TPoint;
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
  Result.Left:= p1.x;
  Result.Top:= p1.y;
  Result.Right:= p2.x;
  Result.Bottom:= p2.y;
end;

end.

