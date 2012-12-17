unit DrawZoom;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

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
  private
    Invalid: ProcedureOfObject;
    Border: TRectReal;
    Scale: real;
    ScreenC: TPointReal;
  public
    function WtoS(world: TPointReal):TPoint;
    function StoW(screen: TPoint):TPointReal;
    procedure ReCalculate(canvas: TCanvas);
    constructor Create(invalidate: ProcedureOfObject);
  end;

var
  Viewport: TViewport;

implementation

uses
  DrawTools;

{ TViewport }

function TViewport.WtoS(world: TPointReal): TPoint;
begin
  result.x:= trunc(world.x*Scale - ScreenC.X);
  result.y:= trunc(world.y*Scale - ScreenC.y);
end;

function TViewport.StoW(screen: TPoint): TPointReal;
begin
  Result.x:= screen.x/Scale + screenc.x;
  Result.y:= screen.y/Scale + screenc.y;
end;

procedure TViewport.ReCalculate(canvas: TCanvas);
var
  i: integer;
  t: TRectReal;
begin
  Border:= Scene.Shapes[i].BoundingRect;
  for i:= 0 to High(Scene.Shapes) do begin
    t:= Scene.Shapes[i].BoundingRect;
    if Border.Left > t.Left Then Border.Left:= t.Left;
    if Border.Right < t.Right Then Border.Right:= t.Right;
    if Border.Top > t.Top Then Border.Top:= t.Top;
    if Border.Bottom < t.Bottom Then Border.Bottom:= t.Bottom;
  end;
  Invalid;
end;

constructor TViewport.Create(invalidate: ProcedureOfObject);
begin
  Invalid := Invalidate;
end;

end.

