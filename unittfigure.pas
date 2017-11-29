unit UnitTFigure;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Transformation, Controls, UnitParams;

type


TFigures = Class
  Points: ArrayWorldPoint;
  ScreenPoints: Array of TPoint;
  FillColor, LineColor: TColor;
  LineWidth: Integer;
  PenStyle: TPenStyle;
  FillStyle: TBrushStyle;
  WRadius, HRadius: Integer;
  constructor Create(APoint : WorldPoint);
  Procedure Draw(ACanvas: TCanvas); Virtual;
end;

TFigureClass = class of TFigures;
TFigureList = array of TFigureClass;

  {TPolyLine}
TPolyline = Class(TFigures)
Public
 procedure Draw(ACanvas: TCanvas); override;
end;

  {TLine}
TLine = Class(TFigures)
Public
 procedure Draw(ACanvas: TCanvas); override;
end;

  {TRectangle}
TRectangle = Class(TFigures)
Public
 procedure Draw(ACanvas: TCanvas); override;
end;

  {TEllipse}
TEllipse = Class(TFigures)
Public
 procedure Draw(ACanvas: TCanvas); override;
end;

  {TRoundRect}
TRoundRect = Class(TFigures)
Public
 procedure Draw(ACanvas: TCanvas); override;
end;

TFfigureClass = class of TFigures;


var
  FigureList: TFigureList;
  CanvasFigures: Array of TFigures;


implementation

Constructor TFigures.Create(Apoint: WorldPoint) ;
begin
  SetLength(Points, Length(Points) + 1);
  Points[high(points)]:= Apoint;

  {PPoints[0]:= WPoint((x / (scale * scale)  - (Offset.x * 2))  , (y / (scale * scale) - (Offset.y * 2)));
  Points[1]:= Points[0];}
end;

procedure TFigures.Draw(ACanvas: TCanvas);
var
  i: Integer;
begin
  with ACanvas do
  begin
    Pen.Width := LineWidth;
    Pen.Color := LineColor;
    Pen.Style := PenStyle;
    Brush.Color := FillColor;
    Brush.Style := FillStyle;
  end;
  SetLength(ScreenPoints, Length(Points));
  for i := low(Points) to High(Points) do
    ScreenPoints[i]:= WorldToScreen(Points[i]);
end;


 {PolyLine}

procedure TPolyline.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Polyline(ScreenPoints);
end;


 {Line}
procedure TLine.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Line(
    ScreenPoints[High(ScreenPoints)],
    ScreenPoints[Low(ScreenPoints)]);
end;

 {Rectangle}

procedure TRectangle.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Rectangle(
    ScreenPoints[Low(ScreenPoints)].x,
    ScreenPoints[Low(ScreenPoints)].y,
    ScreenPoints[High(ScreenPoints)].x,
    ScreenPoints[High(ScreenPoints)].y);
end;

 {TEllipse}

procedure TEllipse.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Ellipse(
    ScreenPoints[Low(ScreenPoints)].x,
    ScreenPoints[Low(ScreenPoints)].y,
    ScreenPoints[High(ScreenPoints)].x,
    ScreenPoints[High(ScreenPoints)].y);
end;

 {TRoundRect}

procedure TRoundRect.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.RoundRect(
    ScreenPoints[Low(ScreenPoints)].x,
    ScreenPoints[Low(ScreenPoints)].y,
    ScreenPoints[High(ScreenPoints)].x,
    ScreenPoints[High(ScreenPoints)].y,
    HRadius, WRadius);
end;

end.

