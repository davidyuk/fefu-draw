unit DrawLayers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DrawShape;

type

  { TLayers }

  TLayers = class
    private
      arr: array of TFBase;
    public
      length: integer;
      selected: TFBase;
      selectedId: integer;
      procedure Add(v: TFBase);
      function Get(i: integer):TFBase;
      procedure Remove(i: integer);
      procedure Swap(fi, ti: integer);
      procedure Clear;
      constructor Create;
  end;

implementation

{ TLayers }

procedure TLayers.Add(v: TFBase);
begin
  inc(length);
  setLength(arr, length);
  arr[high(arr)]:= v;
end;

function TLayers.Get(i: integer): TFBase;
begin
  result:= arr[i];
end;

procedure TLayers.Remove(i: integer);
var j: integer;
begin
  arr[i].free;
  for j:= i to high(arr)-1 do
    arr[j]:= arr[j+1];
  dec(length);
  setLength(arr, length);
end;

procedure TLayers.Swap(fi, ti: integer);
var t: TFBase;
begin
  t:= arr[fi];
  arr[fi]:= arr[ti];
  arr[ti]:= t;
end;

procedure TLayers.Clear;
var i: integer;
begin
  for i:= 0 to length-1 do
    arr[i].free;
  length:= 0;
  setLength(arr, length);
end;

constructor TLayers.Create;
begin
  inherited;
  length:= 0;
  setLength(arr, length);
  selectedId:= -1;
  selected:= nil;
end;

end.

