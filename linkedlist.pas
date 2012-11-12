unit LinkedList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{
этот модуль содержит описание объекта TLinkedList, для создания списков
методы объекта:
get(ITEM_number)
write(ITEM_number, value)
add(ITEM_number, value) //-1 - в конец
remove(ITEM_number)
move(FROM_number, TO_number)
}

type

  PNode = ^TNode;

  TNode = record
    next: PNode;
    value: TObject;
  end;

  { TLinkedList }

  TLinkedList = class(TObject)
    private
      cache: array of PNode;
      first: PNode;
      procedure update;
    public
      length: integer;
      procedure add(i: integer; v: TObject);
      function get(i: integer):TObject;
      procedure write(i: integer; v: TObject);
      procedure remove(i: integer);
      procedure move(fi, ti: integer);
      constructor create; overload;
  end;

implementation

procedure TLinkedList.update;
var i: integer; p: PNode;
begin
  if first <> nil then begin
    p:= first;
    length:= 1;
    while p^.next <> nil do begin
      inc(length);
      p:= p^.next;
    end;
  end else length:= 0;
  setLength(cache, length-1);
  p:= first;
  for i:= 0 to length-1 do begin
    cache[i]:= p;
    p:= p^.next;
  end;
end;

procedure TLinkedList.add(i: integer; v: TObject);
var t: PNode;
begin
  new(t);
  t^.value:= v;
  if i = -1 Then begin
    i:= length;
    t^.next:= nil;
  end else if i = 0 then t^.next:= nil
  else t^.next:= cache[i];
  if i = 0 then first:= t
  else cache[i-1]^.next:= t;
  update;
end;

function TLinkedList.get(i: integer): TObject;
begin
  result:= cache[i]^.value;
end;

procedure TLinkedList.write(i: integer; v: TObject);
begin
  cache[i]^.value.Destroy;
  cache[i]^.value:= v;
end;

procedure TLinkedList.remove(i: integer);
begin
  if i = 0 then first:= cache[1]
  else cache[i-1]^.next := cache[i+1];
  cache[i]^.value.Destroy;
  update;
end;

procedure TLinkedList.move(fi, ti: integer);
var p: PNode;
begin
  if fi = ti then exit;
  p:= cache[fi];
  cache[fi]:= cache[ti];
  cache[ti]:= p;
  if fi - 1 >= 0 then cache[fi-1]^.next := cache[fi]
  else first:= cache[fi];
  if fi + 1 <= length then cache[fi]^.next := cache[fi+1]
  else cache[fi]^.next:= nil;
  if ti - 1 >=0 then cache[ti-1]^.next := cache[ti]
  else first:= cache[ti];
  if ti + 1 >= length then cache[ti]^.next := cache[ti+1]
  else cache[ti]^.next:= nil;
  update;
end;

constructor TLinkedList.create;
begin
  inherited;
  first := nil;
  length := 0;
  update;
end;

end.

