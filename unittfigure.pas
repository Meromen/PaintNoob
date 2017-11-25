unit UnitTFigure;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Transformation, Controls;

type


TFigures = Class
protected
  Points: ArrayWorldPoint;
  FillColor, LineColor: TColor;
  LineWidth: Integer;
  FillStyle: TBrushStyle;
  WRadius, HRadius: Integer;

public
 constructor Create(X,Y: Integer;
   ALineColor: TColor;
   AFillColor: TColor;
   ALineWidth: integer;
   AFillStyle: TBrushStyle;
   ARadiusW: Integer;
   ARadiusH: Integer);
 procedure MouseMove(X, Y: Integer); Virtual; Abstract;
 procedure MouseDown(X, Y: Integer); Virtual; Abstract;
 procedure MouseUp  (X, Y: Integer); Virtual; Abstract;
 Procedure Draw     (ACanvas: TCanvas); Virtual; Abstract;
end;

TFigureClass = class of TFigures;
TFigureList = array of TFigureClass;

  {TPolyLine}
TPolyline = Class(TFigures)
Public
 procedure Draw(ACanvas: TCanvas); override;
 procedure MouseMove(X, Y: Integer); override;
end;

  {TLine}
TLine = Class(TFigures)
Public
 procedure MouseMove(X, Y: Integer); override;
 procedure Draw(ACanvas: TCanvas); override;
end;

  {TRectangle}
TRectangle = Class(TFigures)
Public
 procedure MouseMove(X, Y: Integer); override;
 procedure Draw(ACanvas: TCanvas); override;
end;

  {TEllipse}
TEllipse = Class(TFigures)
Public
 Procedure MouseMove(X, Y: Integer); override;
 procedure Draw(ACanvas: TCanvas); override;
end;

  {TRoundRect}
TRoundRect = Class(TFigures)
Public
 procedure MouseMove(X, Y: Integer); override;
 procedure Draw(ACanvas: TCanvas); override;
end;

procedure RegisterFigures(AFigures: array of TClass);

var
  FigureList: TFigureList;


implementation

procedure RegisterFigures(AFigures: array of TClass);
var
  i: TClass;
begin
  for i in AFigures do
  begin
    SetLength(FigureList, Length(FigureList) + 1);
    FigureList[High(FigureList)] := TFigureClass(i);
  end;
end;

Constructor TFigures.Create(X,Y: Integer; ALineColor:Tcolor; AFillColor: TColor;
  ALineWidth: integer; AFillStyle: TBrushStyle; ARadiusW: Integer; ARadiusH: Integer) ;
begin
  SetLength(Points, 2);
  Points[0]:= WPoint((x / (scale * scale)  - (Offset.x * 2))  , (y / (scale * scale) - (Offset.y * 2)));
  Points[1]:= Points[0];
  LineColor:= ALineColor;
  FillColor:= AFillColor;
  LineWidth:= ALineWidth;
  FillStyle:= AFillStyle;
  WRadius:= ARadiusW;
  HRadius:= ARadiusH;
end;


 {PolyLine}

procedure TPolyline.Draw(ACanvas: TCanvas);
begin
  with ACanvas do
  begin
    Pen.Color:= LineColor;
    Pen.Width:= LineWidth;
    Polyline(WorldToScreen(Points));
  end;
end;

procedure TPolyline.MouseMove(X, Y: Integer);
begin
  SetLength(Points, Length(Points) + 1);
  Points[High(Points)]:= WPoint(X / scale - Offset.x, Y / scale - Offset.y);
end;

 {Line}
procedure TLine.Draw(ACanvas: TCanvas);
begin
  with ACanvas do
  begin
    Pen.Color:= LineColor;
    Pen.Width:= LineWidth;
    Line(
       WorldToScreenX(Points[0].x), WorldToScreenY(Points[0].Y),
       WorldToScreenX(Points[1].x), WorldToScreenY(Points[1].y));
  end;
end;

procedure TLine.MouseMove(X, Y: Integer);
begin
  Points[High(Points)]:= WPoint(X / scale - Offset.x, Y / scale - Offset.y);
end;

 {Rectangle}

procedure TRectangle.Draw(ACanvas: TCanvas);
begin
  with ACanvas do
  begin
    Pen.Color:= LineColor;
    Pen.Width:= LineWidth;
    Brush.Color:= FillColor;
    Brush.Style:= FillStyle;
    Rectangle(
       WorldToScreenX(Points[0].x), WorldToScreenY(Points[0].Y),
       WorldToScreenX(Points[1].x), WorldToScreenY(Points[1].y));
  end;
end;

procedure TRectangle.MouseMove(X, Y:Integer);
begin
  Points[High(Points)]:= WPoint(X / scale - Offset.x, Y / scale - Offset.y);
end;

 {TEllipse}

procedure TEllipse.Draw(ACanvas: TCanvas);
begin
  With ACanvas do
  begin
    Pen.Color:= LineColor;
    Pen.Width:= LineWidth;
    Brush.Color:= FillColor;
    Brush.Style:= FillStyle;
    Ellipse(
      WorldToScreenX(Points[0].x), WorldToScreenY(Points[0].Y),
      WorldToScreenX(Points[1].x), WorldToScreenY(Points[1].y));
  end;
end;

procedure TEllipse.MouseMove(X, Y:integer);
Begin
  Points[High(Points)]:= WPoint(X / scale - Offset.x, Y / scale - Offset.y);
end;

 {TRoundRect}

procedure TRoundRect.Draw(ACanvas: TCanvas);
begin
  with ACanvas do
  begin
    Pen.Color:= LineColor;
    Pen.Width:= LineWidth;
    Brush.Color:= FillColor;
    Brush.Style:= FillStyle;
    RoundRect(
      WorldToScreenX(Points[0].x), WorldToScreenY(Points[0].Y),
      WorldToScreenX(Points[1].x), WorldToScreenY(Points[1].y), WRadius, HRadius);
  end;
end;

procedure TRoundRect.MouseMove(X, Y:integer);
Begin
  Points[high(Points)]:= WPoint(X / scale - Offset.x, Y / scale  - Offset.y);
end;

initialization
RegisterFigures([
TPolyline,
TEllipse,
TLine,
TRectangle,
TRoundRect
]);

end.

