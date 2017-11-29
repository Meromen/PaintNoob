unit Transformation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  WorldPoint = record
    X: Double;
    Y: Double;
  end;

  ArrayWorldPoint = array of WorldPoint;
  ScreenPoint = TPoint;
  ArrayScreenPoint = array of ScreenPoint;



  function ScreenToWorld (SPoints: ArrayScreenPoint): ArrayWorldPoint;
  function ScreenToWorld (SPoint: ScreenPoint): WorldPoint;
  function ScreenToWorldX (X: Integer): Double;
  function ScreenToWorldY (Y: Integer): Double;
  function WorldToScreen (WPoints: ArrayWorldPoint): ArrayScreenPoint;
  function WorldToScreen (WPoint: WorldPoint): ScreenPoint;
  function WorldToScreenX (X: Double): Integer;
  function WorldToScreenY (Y: Double): Integer;
  function WPoint(X,Y: Double):WorldPoint;
 {function OffSetXY(x1,y1:Integer; x2,y2,x3,y3:Double):Tpoint;
  function Offsetout():Tpoint;   }
  procedure Zoom(Apoint: Tpoint; AScale: Double);
  procedure SetScale(AScale: double);

  const
  MinScale = 0.01;
  MaxScale = 50;

  var
    scale:Double;
    Offset:WorldPoint;
    PBWidth, PBHeight: integer;


implementation

{function OffSetXY(x1,y1: Integer; x2,y2, x3,y3: Double): TPoint;
begin
  Offset.x := (x1 + Round(x2 - x3)) ;
  Offset.y := (y1 + Round(y2 - y3)) ;
  Result.x:=  Offset.x ;
  Result.y:=  Offset.y ;
end;
function Offsetout():TPoint;
begin
  Result:= Offset;
end;    }


   {Преобразование в Мировые}

function ScreenToWorldX(X: Integer): Double;
begin
   Result:= X / scale + Offset.x;
end;

function ScreenToWorldY(Y: Integer): Double;
begin
   Result:= Y / scale + Offset.y;
end;

function ScreenToWorld(SPoint: ScreenPoint): WorldPoint;
begin
  Result.X:= ScreenToWorldX(Spoint.X);
  Result.Y:= ScreenToWorldY(Spoint.Y);
end;
function ScreenToWorld (SPoints: ArrayScreenPoint): ArrayWorldPoint;
var
  i:integer;
begin
  SetLength(Result,Length(SPoints));
  for i:= 0 to High(SPoints) do
    Result[i]:= ScreenToWorld(SPoints[i]);
end;
{________________________________________________________________________}

   {Преобразование в Экранные}

function WorldToScreenX(X: Double): Integer;
begin
  Result:= round((X - Offset.X) * scale );
end;

function WorldToScreenY(Y: Double): Integer;
begin
  Result:= round((Y  - Offset.Y) * scale );
end;

function WorldToScreen(WPoint: WorldPoint): ScreenPoint;
begin
  Result.X:= WorldToScreenX(WPoint.x);
  Result.y:= WorldToScreenY(WPoint.Y);
end;

function WorldToScreen(WPoints: ArrayWorldPoint): ArrayScreenPoint;
var
  i:Integer;
begin
  SetLength(Result,Length(WPoints));
  for i:= 0 to High(WPoints) do
    Result[i]:= WorldToScreen(WPoints[i]);
end;

function WPoint(X, Y: Double): WorldPoint;
begin
  Result.X:=X;
  Result.Y:=Y;
end;

   {_______________________________________________________________________}

   {Zoom}

procedure Zoom(Apoint: Tpoint; AScale: Double);
var
  Point1, Point2: WorldPoint;
begin
 Point1 := ScreenToWorld(APoint);
 SetScale(Ascale);
 Point2 := ScreenToWorld(APoint);
 Offset.x := Offset.x - Round(Point2.x - Point1.x);
 Offset.y := Offset.y - Round(Point2.y - Point1.y);
end;

procedure SetScale(AScale: double);
begin
  if AScale > MaxScale then
    Scale:= MaxScale
  else if AScale < MinScale then
    Scale:= MinScale
  else
    Scale:= AScale;
end;

initialization

Offset:= WPoint(0,0);
scale:= 1;


end.

