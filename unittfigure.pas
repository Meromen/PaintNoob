unit UnitTFigure;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Transformation, Controls,
  Grids, LCLIntf,LCLType,
   Buttons, GraphMath, Math, Spin, FPCanvas, TypInfo, LCL, Windows, UnitConstants;

type


TFigures = Class
  Points: ArrayWorldPoint;
  ScreenPoints: Array of TPoint;
  FillColor, PenColor: TColor;
  LineWidth: Integer;
  PenStyle: TPenStyle;
  FillStyle: TBrushStyle;
  Selected: boolean;
  Region: HRGN;
  WRadius, HRadius: Integer;
  constructor Create(APoint : WorldPoint);
  Procedure Draw(ACanvas: TCanvas); Virtual;
  function FindTopLeft: WorldPoint;
  procedure CreateRegion; virtual; abstract;
  function FindBottomRight: WorldPoint;
  procedure DrawOutline(Point1,Point2: WorldPoint; Canvas: TCanvas);
  function Save:TStringArray; virtual;
  procedure Load; virtual;
  function GetParametersList: TStringArray; virtual;
end;

TFigureClass = class of TFigures;
TFigureList = array of TFigureClass;

  {TPolyLine}
TPolyline = Class(TFigures)
Public
 procedure Draw(ACanvas: TCanvas); override;
 procedure CreateRegion; override;
 function Save: TStringArray; override;
 procedure Load; override;
end;

  {TLine}
TLine = Class(TFigures)
Public
 procedure Draw(ACanvas: TCanvas); override;
 procedure CreateRegion; override;
 function Save: TStringArray; override;
 procedure Load; override;
end;

  {TRectangle}
TRectangle = Class(TFigures)
Public
 procedure Draw(ACanvas: TCanvas); override;
 procedure CreateRegion; override;
 function Save: TStringArray; override;
 procedure Load; override;
end;

  {TEllipse}
TEllipse = Class(TFigures)
Public
 procedure Draw(ACanvas: TCanvas); override;
 procedure CreateRegion; override;
 function Save: TStringArray; override;
 procedure Load; override;
end;

  {TRoundRect}
TRoundRect = Class(TFigures)
Public
 procedure Draw(ACanvas: TCanvas); override;
 procedure CreateRegion; override;
 function Save: TStringArray; override;
 procedure Load; override;
 function GetParametersList: TStringArray; override;
end;



TFfigureClass = class of TFigures;

 procedure LineRegion(p1,p2:TPoint;var tempPoints: array of TPoint;Width:integer);
 function CasePenStyle(Index: integer):TPenStyle;
 function CaseBrushStyle(Index: integer):TBrushStyle;
 function CasePenStyleIndex(Style: TPenStyle): integer;
 function CaseBrushStyleIndex(BrushStyle: TBrushStyle): integer;

var
  FigureList: TFigureList;
  CanvasFigures: Array of TFigures;



implementation

Constructor TFigures.Create(Apoint: WorldPoint) ;
begin
  SetLength(Points, Length(Points) + 1);
  Points[high(points)]:= Apoint;
end;

procedure TFigures.Draw(ACanvas: TCanvas);
var
  i: Integer;
begin
  with ACanvas do
  begin
    Pen.Width := LineWidth;
    Pen.Color := PenColor;
    Pen.Style := PenStyle;
    Brush.Color := FillColor;
    Brush.Style := FillStyle;
  end;
  SetLength(ScreenPoints, Length(Points));
  for i := low(Points) to High(Points) do
    ScreenPoints[i]:= WorldToScreen(Points[i]);
end;

function TFigures.GetParametersList: TStringArray;
begin
  SetLength(Result, 5);
  Result[4] := LineWidthLabel;
  Result[3] := FillStyleLabel;
  Result[2] := FillStyleLabel;
  Result[1] := PenColorLabel;
  Result[0] := FillColorLabel;
end;

procedure TFigures.DrawOutline(Point1,Point2: WorldPoint; Canvas: TCanvas);
var
  a:WorldPoint;
begin
  if (Point1.X>Point2.X) then
    begin
      a.X:=Point1.X;
      Point1.X:=Point2.X;
      Point2.X:=a.X;
    end;
  if (Point1.Y>Point2.Y) then
    begin
      a.Y:=Point1.Y;
      Point1.Y:=Point2.Y;
      Point2.Y:=a.Y;
    end;
  Canvas.Pen.Color := clBlack;
  Canvas.Pen.Width := 1;
  Canvas.Pen.Style := psDash;
  Canvas.Frame  (WorldToScreen(Point1).x-5-round(LineWidth/2),WorldToScreen(Point1).y-5-round(LineWidth/2),
                 WorldToScreen(Point2).x+5+round(LineWidth/2),WorldToScreen(Point2).y+5+round(LineWidth/2));
end;

procedure LineRegion(p1,p2:TPoint;var tempPoints: array of TPoint;Width:integer);
begin
      if (abs(p2.x-p1.x)>45) then
    begin
      tempPoints[0].x := p1.x-Width div 2;
      tempPoints[0].y := p1.y-5-Width;
      tempPoints[1].x := p2.x+Width div 2;
      tempPoints[1].y := p2.y-5-Width;
      tempPoints[2].x := p2.x+Width div 2;
      tempPoints[2].y := p2.y+5+Width;
      tempPoints[3].x := p1.x-Width div 2;
      tempPoints[3].y := p1.y+5+Width;
    end else
    begin
      tempPoints[0].x := p1.x-5-Width;
      tempPoints[0].y := p1.y-Width div 2;
      tempPoints[1].x := p2.x-5-Width;
      tempPoints[1].y := p2.y+Width div 2;
      tempPoints[2].x := p2.x+5+Width;
      tempPoints[2].y := p2.y+Width div 2;
      tempPoints[3].x := p1.x+5+Width;
      tempPoints[3].y := p1.y-Width div 2;
    end;
end;

function TFigures.Save:TStringArray;
var
  i: integer;
begin
  SetLength(Result, 2);
  Result[0] := ClassName;
  Result[1] := IntToStr(Length(Points)) + ' ';
  for i:=0 to High(Points) do
    begin
    Result[1] += FloatToStr(Points[i].X) + ' ';
    Result[1] += FloatToStr(Points[i].Y) + ' ';
    end;
end;

procedure TFigures.Load;
var
  i, n: integer;
begin
  read(n);
  SetLength(Points, n);
  for i:=0 to n-1 do
    begin
      read(Points[i].X);
      read(Points[i].Y);
    end;
  readln();
end;


function CasePenStyle(Index: integer): TPenStyle;
begin
  case Index of
    0:Result := psSolid;
    1:Result := psDash;
    2:Result := psDot;
    3:Result := psDashDot;
    4:Result := psDashDotDot;
  end;
end;

function CasePenStyleIndex(Style: TPenStyle): integer;
begin
    case Style of
    psSolid:Result := 0;
    psDash:Result := 1;
    psDot:Result := 2;
    psDashDot:Result := 3;
    psDashDotDot:Result := 4;
  end;
end;

function CaseBrushStyle(Index: integer): TBrushStyle;
begin
  case Index of
    0:Result := bsSolid;
    1:Result := bsBDiagonal;
    2:Result := bsDiagCross;
    3:Result := bsVertical;
    4:Result := bsCross;
    5:Result := bsFDiagonal;
    6:Result := bsHorizontal;
  end;
end;

function CaseBrushStyleIndex(BrushStyle: TBrushStyle): integer;
begin
  case BrushStyle of
    bsSolid: Result := 0;
    bsBDiagonal:Result := 1;
    bsDiagCross:Result := 2;
    bsVertical:Result := 3;
    bsCross:Result := 4;
    bsFDiagonal:Result := 5;
    bsHorizontal:Result := 6;
  end;
end;


 {PolyLine}

procedure TPolyline.Draw(ACanvas: TCanvas);
var
  i: integer;
  max,min: WorldPoint;
begin
  inherited;
  ACanvas.Polyline(ScreenPoints);
  if (Selected = true) then
  begin
    DeleteObject(Region);
    Min:= WPoint(MaxFloat,MaxFloat);
    Max := WPoint(MinFloat,MinFloat);
   for i := 1 to high(Points) do
      begin
        if Points[i].X > Max.X then Max.X:=Points[i].X;
        if Points[i].Y > Max.Y then Max.Y:=Points[i].Y;
        if Points[i].X < Min.X then Min.X:=Points[i].X;
        if Points[i].Y < Min.Y then Min.Y:=Points[i].Y;
      end;
      DrawOutline(Min,Max,ACanvas);
  end;
end;

procedure TPolyline.CreateRegion;
var
  RegionPoints: array[0..3] of TPoint;
  p1,p2: TPoint;
  curRgn: HRGN;
  i: integer;
begin
  for i := 0 to high(Points)-1 do
  begin
    p1 := WorldToScreen(Points[i]);
    p2 := WorldToScreen(Points[i+1]);
    LineRegion(p1,p2,RegionPoints,linewidth);
    if (i=low(Points)) then Region := CreatePolygonRgn (RegionPoints,3,2);
    curRgn := CreatePolygonRgn (RegionPoints,3,2);
    CombineRgn (Region,Region,curRgn,RGN_OR);
    DeleteObject(curRgn);
  end;
end;

function TPolyLine.Save:TStringArray;
begin
  Result := Inherited;
  SetLength(Result, Length(Result) + 3);
  Result[High(Result)-2] := IntToStr(LineWidth);
  Result[High(Result)-1] := ColorToString(PenColor);
  Result[High(Result)] := IntToStr(CasePenStyleIndex(PenStyle));
end;

procedure TPolyLine.Load;
var
  a: integer;
  s: string;
begin
  Inherited;
  readln(a);
  LineWidth := a;
  readln(s);
  PenColor := StringToColor(s);
  readln(a);
  PenStyle := CasePenStyle(a);
end;

 {Line}

procedure TLine.CreateRegion;
var
  RegionPoints: array[0..3] of TPoint;
  p1,p2: TPoint;
begin
  p1 := WorldToScreen(Points[0]);
  p2 := WorldToScreen(Points[high(Points)]);
  LineRegion(p1,p2,RegionPoints,lineWidth);
  Region := CreatePolygonRgn(RegionPoints,3,2);
end;

procedure TLine.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Line(
    ScreenPoints[High(ScreenPoints)],
    ScreenPoints[Low(ScreenPoints)]);
  if (Selected = true) then
  begin
    DeleteObject(Region);
    DrawOutline(Points[0],Points[high(Points)],aCanvas);
  end;
end;

function TLine.Save:TStringArray;
begin
  Result := Inherited;
  SetLength(Result, Length(Result) + 3);
  Result[High(Result)-2] := IntToStr(LineWidth);
  Result[High(Result)-1] := ColorToString(PenColor);
  Result[High(Result)] := IntToStr(CasePenStyleIndex(PenStyle));
end;

procedure TLine.Load;
var
  a: integer;
  s: string;
begin
  Inherited;
  readln(a);
  LineWidth := a;
  readln(s);
  PenColor := StringToColor(s);
  readln(a);
  PenStyle := CasePenStyle(a);
end;

 {Rectangle}

procedure TRectangle.CreateRegion;
var
  RegionRect: TRect;
begin
  RegionRect.TopLeft := WorldToScreen(Points[0]);
  RegionRect.BottomRight := WorldToScreen(Points[high(Points)]);
  Region := CreateRectRgn (RegionRect.Left,RegionRect.Top,
    RegionRect.Right,RegionRect.Bottom);
end;

procedure TRectangle.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Rectangle(
    ScreenPoints[Low(ScreenPoints)].x,
    ScreenPoints[Low(ScreenPoints)].y,
    ScreenPoints[High(ScreenPoints)].x,
    ScreenPoints[High(ScreenPoints)].y);
  if (Selected = true) then
  begin
    DeleteObject(Region);
    DrawOutline(Points[0],Points[high(Points)],aCanvas);
  end;
end;

function TRectangle.Save:TStringArray;
begin
  Result := Inherited;
  SetLength(Result, Length(Result) + 5);
  Result[High(Result)-4] := ColorToString(PenColor);
  Result[High(Result)-3] := ColorToString(FillColor);
  Result[High(Result)-2] := IntToStr(LineWidth);
  Result[High(Result)-1] := IntToStr(CaseBrushStyleIndex(FillStyle));
  Result[High(Result)] := IntToStr(CasePenStyleIndex(PenStyle));
end;

procedure TRectangle.Load;
var
  a: integer;
  s: string;
begin
  Inherited;
  readln(s);
  PenColor:= StringToColor(s);
  readln(s);
  FillColor:= StringToColor(s);
  readln(a);
  LineWidth:= a;
  readln(a);
  FillStyle:= CaseBrushStyle(a);
  readln(a);
  PenStyle:= CasePenStyle(a);
end;

 {TEllipse}

procedure TEllipse.CreateRegion;
var
  RegionRect: TRect;
begin
  RegionRect.TopLeft := WorldToScreen(Points[0]);
  RegionRect.BottomRight := WorldToScreen(Points[high(Points)]);
  Region := CreateEllipticRgn (RegionRect.Left,RegionRect.Top,RegionRect.Right,RegionRect.Bottom);
end;

procedure TEllipse.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Ellipse(
    ScreenPoints[Low(ScreenPoints)].x,
    ScreenPoints[Low(ScreenPoints)].y,
    ScreenPoints[High(ScreenPoints)].x,
    ScreenPoints[High(ScreenPoints)].y);
  if (Selected = true) then
  begin
    DeleteObject(Region);
    DrawOutline(Points[0],Points[high(Points)],aCanvas);
  end;
end;

function TEllipse.Save:TStringArray;
begin
  Result := Inherited;
  SetLength(Result, Length(Result) + 5);
  Result[High(Result)-4] := ColorToString(PenColor);
  Result[High(Result)-3] := ColorToString(FillColor);
  Result[High(Result)-2] := IntToStr(LineWidth);
  Result[High(Result)-1] := IntToStr(CaseBrushStyleIndex(FillStyle));
  Result[High(Result)] := IntToStr(CasePenStyleIndex(PenStyle));
end;

procedure TEllipse.Load;
var
  a: integer;
  s: string;
begin
  Inherited;
  readln(s);
  PenColor:= StringToColor(s);
  readln(s);
  FillColor:= StringToColor(s);
  readln(a);
  LineWidth:= a;
  readln(a);
  FillStyle:= CaseBrushStyle(a);
  readln(a);
  PenStyle:= CasePenStyle(a);
end;

 {TRoundRect}

procedure TRoundRect.CreateRegion;
var
  RegionRect: TRect;
begin
  RegionRect.TopLeft := WorldToScreen(Points[0]);
  RegionRect.BottomRight := WorldToScreen(Points[high(Points)]);
  Region := CreateRoundRectRgn (RegionRect.Left,RegionRect.Top,RegionRect.Right,
    RegionRect.Bottom,WRadius,WRadius);

end;

procedure TRoundRect.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.RoundRect(
    ScreenPoints[Low(ScreenPoints)].x,
    ScreenPoints[Low(ScreenPoints)].y,
    ScreenPoints[High(ScreenPoints)].x,
    ScreenPoints[High(ScreenPoints)].y,
    HRadius, WRadius);
  if (Selected = true) then
  begin
    DeleteObject(Region);
    DrawOutline(Points[0],Points[high(Points)],aCanvas);
  end;
end;

function TRoundRect.GetParametersList: TStringArray;
begin
  Result := inherited;
  SetLength(Result, Length(Result) + 1);
  Result[High(Result)] := Radius;
end;

function TRoundRect.Save:TStringArray;
begin
  Result := Inherited;
  SetLength(Result, Length(Result) + 6);
  Result[High(Result)-5] := ColorToString(PenColor);
  Result[High(Result)-4] := ColorToString(FillColor);
  Result[High(Result)-3] := IntToStr(LineWidth);
  Result[High(Result)-2] := IntToStr(CaseBrushStyleIndex(FillStyle));
  Result[High(Result)-1] := IntToStr(CasePenStyleIndex(PenStyle));
  Result[High(Result)] := IntToStr(WRadius);
  Result[High(Result)] += ' ' + IntToStr(HRadius);
end;

procedure TRoundRect.Load;
var
  a: integer;
  s: string;
begin
  Inherited;
  readln(s);
  PenColor:= StringToColor(s);
  readln(s);
  FillColor:= StringToColor(s);
  readln(a);
  LineWidth:= a;
  readln(a);
  FillStyle:= CaseBrushStyle(a);
  readln(a);
  PenStyle:= CasePenStyle(a);
  read(a);
  WRadius := a;
  readln(a);
  HRadius := a;
end;
  {Coners of canvas}

function TFigures.FindTopLeft: WorldPoint;
var
  i: WorldPoint;
begin
  Result := Points[Low(Points)];
  for i in Points do
  begin
    if (i.x < Result.x) then
      Result.x := i.x;
    if (i.y < Result.y) then
      Result.y := i.y;
  end;
end;

function TFigures.FindBottomRight: WorldPoint;
var
  i: WorldPoint;
begin
  Result := Points[Low(Points)];
  for i in Points do
  begin
    if (i.x > Result.x) then
      Result.x := i.x;
    if (i.y > Result.y) then
      Result.y := i.y;
  end;
end;

end.

