unit UnitTTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Transformation, UnitTFigure, UnitParams, Graphics, Controls, math, ExtCtrls;

type
TToolClass = Class of TTool;


TTool = Class
  Bitmap: TPicture;
  Panel: TPanel;
  Params: TParamsArray;
  FigureClass: TFigureClass;
  procedure AddPenParams(APanel: TPanel);
  procedure AddBrushParams(APanel: TPanel);
  procedure CreateFigure(APoint: TPoint); virtual;
  procedure AddPoint(APoint: TPoint); virtual;
  procedure MouseMove(X, Y: Integer; ALastOffset: Tpoint); Virtual;
  procedure MouseUp(Button: TMouseButton; X,Y:integer); Virtual;
  procedure CreateParams(APanel: TPanel); virtual;
  procedure ShowParameters;
  procedure HideParameters;
end;

  {TwoPointTool}
TTwoPointTool = Class(TTool)
  procedure AddPoint(APoint: TPoint); virtual;
end;

  {ActionTool}
TActionTool = Class (TTool)
  procedure CreateFigure(APoint: TPoint); override;
  procedure CreateParams(APanel: TPanel); override;
end;

 {InvisibleTool}
TInvisibleActionTool = Class(TActionTool)
  procedure CreateFigure(APoint: TPoint); override;
end;

 {THandTool}
THand = Class (TInvisibleActionTool)
  procedure MouseMove(X, Y: Integer; ALastOffset: TPoint); Override;
end;


 {TMagnifierTool}
TMagnifier = Class(TActionTool)
  procedure MouseUp(Button: TMouseButton; X,Y:integer);  Override;
end;

 {TPenTool}
TPenTool = class(TTool)
  procedure CreateParams(APanel: TPanel); override;
end;

 {TRectangleTool}
TRectangleTool = class(TTwoPointTool)
  procedure CreateParams(APanel: TPanel); override;
end;

 {TRoudRectangleTool}
TRoundRectangleTool = class(TTwoPointTool)
  procedure CreateParams(APanel: TPanel); override;
end;

 {TEllipseTool}
TEllipseTool = class(TTwoPointTool)
  procedure CreateParams(APanel: TPanel); override;
end;

 {TLineTool}
TLineTool = class(TTwoPointTool)
  procedure CreateParams(APanel: TPanel); override;
end;

 {TPolyLineTool}
 TPolyLineTool = class(TTool)
  procedure MouseMove(X, Y: Integer; ALastOffset: TPoint); Override;
  procedure CreateParams(APanel: TPanel); override;
end;

procedure RegisterTool(ATool: TTool; AFigureClass: TFigureClass; BMPSorce: String);



var
ToolsList: array of TTool;


implementation

procedure TTool.MouseMove(X, Y: Integer; ALastOffset: Tpoint);
begin
  AddPoint(Point(X,Y));
end;

procedure TTool.MouseUp(Button: TMouseButton; X,Y:integer);
begin

end;

procedure TTool.CreateFigure(APoint: TPoint);
var
  p: TParams;
begin
  SetLength(CanvasFigures, Length(CanvasFigures) + 1);
  CanvasFigures[High(CanvasFigures)] := FigureClass.Create(ScreenToWorld(APoint));
  with CanvasFigures[High(CanvasFigures)] do
  begin
    LineWidth:= MLineWidth;
    LineColor:= MLineColor;
    FillColor:= MFillColor;
    FillStyle:= MFillStyle;
    WRadius:= MRadiusW;
    HRadius:= MRadiusH;

  for p in Params do
    case p.ParamLabel.Caption of
      LineWidthLabel: LineWidth:= (p as TSpinParams).Param;
      LineColorLabel: LineColor:= (p as TColorParams).Param;
      FillColorLabel: FillColor:= (p as TColorParams).Param;
      FillStyleLabel: FillStyle:= (p as TBrushStyleParams).Param;
      Radius:
        begin
        WRadius:= (p as TSpinParams).Param;
        HRadius:= (p as TSpinParams).Param;
        end;
    end;
  end;
end;

procedure TTool.AddPoint(APoint: TPoint);
begin
  with CanvasFigures[High(CanvasFigures)] do
  begin
    SetLength(Points, Length(Points) + 1);
    Points[High(Points)]:= ScreenToWorld(APoint);
  end;
end;

procedure TTool.CreateParams(APanel: TPanel);
begin
  Panel:= TPanel.Create(APanel);
  with Panel do
  begin
    Parent:= APanel;
    Visible:= False;
    Width:= Parent.Width;
    Height:= Parent.Height;
  end;
end;

procedure TTool.ShowParameters;
begin
  Panel.Visible:= True;
end;

procedure TTool.HideParameters;
var
  i: TParams;
begin
  Panel.Visible:= False;
  for i in Params do
    i.SetParamToInit;
end;

procedure TTool.AddPenParams(APanel: TPanel);
var
  i: integer;
begin
  i:= High(Params);
  SetLength(Params, Length(Params) + 2);
  Params[i + 2]:= TColorParams.Create(APanel, LineColorLabel, MLineColor);
  Params[i + 1]:= TSpinParams.Create(APanel, LineWidthLabel, MLineWidth);
end;

procedure TTool.AddBrushParams(APanel: TPanel);
var
  i: integer;
begin
  i := High(Params); SetLength(Params, Length(Params) + 2);
  Params[i + 2]:= TColorParams.Create(APanel, FillColorLabel, MFillColor);
  Params[i + 1]:= TBrushStyleParams.Create(APanel, FillStyleLabel, MFillStyle);
end;

procedure TTwoPointTool.AddPoint(APoint: TPoint);
begin
  with CanvasFigures[High(CanvasFigures)] do
  begin
    SetLength(Points, 2);
    Points[High(Points)]:= ScreenToWorld(APoint);
  end;
end;

procedure TActionTool.CreateFigure(APoint: TPoint);
begin
  SetLength(CanvasFigures, Length(CanvasFigures) + 1);
  CanvasFigures[High(CanvasFigures)] := FigureClass.Create(ScreenToWorld(APoint));
  with CanvasFigures[High(CanvasFigures)] do
  begin
    FillStyle:= bsClear;
  end;
end;

procedure TActionTool.CreateParams(APanel: TPanel);
begin
  inherited;
end;

procedure TInvisibleActionTool.CreateFigure(APoint: TPoint);
begin
  inherited;
  CanvasFigures[High(CanvasFigures)].PenStyle := psClear;
end;

procedure TPenTool.CreateParams(APanel: TPanel);
begin
  inherited;
  AddPenParams(Panel);
end;

procedure TRectangleTool.CreateParams(APanel: TPanel);
begin
  inherited;
  AddPenParams(Panel);
  AddBrushParams(Panel);
end;

procedure TRoundRectangleTool.CreateParams(APanel: TPanel);
var
  i: integer;
begin
  inherited;
  AddPenParams(Panel);
  AddBrushParams(Panel);
  i := High(Params);
  SetLength(Params, Length(Params) + 1);
  Params[i+1] := TSpinParams.Create(Panel, Radius, MRadiusH);
end;

procedure TEllipseTool.CreateParams(APanel: TPanel);
begin
  inherited;
  AddPenParams(Panel);
  AddBrushParams(Panel);
end;

procedure TLineTool.CreateParams(APanel: TPanel);
begin
  inherited;
  AddPenParams(Panel);
end;

procedure TPolylineTool.MouseMove(X, Y: Integer; ALastOffset: TPoint);
begin
  with CanvasFigures[High(CanvasFigures)] do
    Points[High(Points)] := ScreenToWorld(Point(x,y));
end;

procedure TPolyLineTool.CreateParams(APanel: TPanel);
begin
  inherited;
  AddPenParams(Panel);
end;

procedure RegisterTool(ATool: TTool; AFigureClass: TFigureClass; BMPSorce: String);
begin
  SetLength(ToolsList, Length(ToolsList) + 1);
  ToolsList[High(ToolsList)] := ATool;
  with ToolsList[High(ToolsList)] do
  begin
    FigureClass := AFigureClass;
    Bitmap := TPicture.Create;
    Bitmap.LoadFromFile(BMPSorce);
  end;
end;





 {TMagnifier}
procedure TMagnifier.MouseUp(Button: TMouseButton; X,Y:integer);
begin
  case Button of
    mbLeft:
      begin
        Zoom(Point(X,Y), scale*2);
      end;
    mbRight:
      begin
        Zoom(Point(X,Y), scale/2);
      end;
  end;
end;


 {THand}

procedure THand.MouseMove(X, Y: Integer; ALastOffset: TPoint);
begin
 inherited;
  with CanvasFigures[High(CanvasFigures)] do
  begin
    Offset.x := Offset.x + Points[Low(Points)].x - Points[High(Points)].x;
    Offset.y := Offset.y + Points[Low(Points)].y - Points[High(Points)].y;
  end;
end;




Initialization
RegisterTool(TPenTool.Create, TPolyLine, 'TPen.ico');
RegisterTool(TLineTool.Create, TLine, 'TLine.ico');
//RegisterTool(TPolyLineTool.Create, TPolyLine, 'TPolyLine.ico');
RegisterTool(TRectangleTool.Create, TRectangle, 'TRectangle.ico');
RegisterTool(TRoundRectangleTool.Create, TRoundRect, 'TRoundRect.ico');
RegisterTool(TEllipseTool.Create, TEllipse, 'TEllipse.ico');
RegisterTool(THand.Create, TRectangle, 'THand.ico');
RegisterTool(TMagnifier.Create, TRectangle, 'TMagnifier.ico');
end.

