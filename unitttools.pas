unit UnitTTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Transformation, UnitTFigure, UnitParams, Graphics, Controls,
  ExtCtrls, StdCtrls, LCLIntf, LCLType, Buttons,
  Math, FPCanvas, TypInfo, LCL, UnitConstants;

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
  procedure UpdateParameters;
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
  constructor Create;
  procedure CreateParams(APanel: TPanel); override;
end;

 {TRoudRectangleTool}
TRoundRectangleTool = class(TTwoPointTool)
  constructor Create;
  procedure CreateParams(APanel: TPanel); override;
end;

 {TEllipseTool}
TEllipseTool = class(TTwoPointTool)
  constructor Create;
  procedure CreateParams(APanel: TPanel); override;
end;

 {TLineTool}
TLineTool = class(TTwoPointTool)
  constructor Create;
  procedure CreateParams(APanel: TPanel); override;
end;

 {TPolyLineTool}
TPolyLineTool = class(TTool)
  constructor Create;
  procedure MouseMove(X, Y: Integer; ALastOffset: TPoint); Override;
  procedure CreateParams(APanel: TPanel); override;
end;

 {TSelectTool}
TSelectTool = class(TActionTool)
  procedure MouseUp(Button: TMouseButton; X, Y: integer); override;
  procedure PointSelectTool(Point: TPoint);
  procedure RectSelectTool(Point: TPoint);
end;

 {TMoveTool}
TMoveTool = class(TInvisibleActionTool)
  Apoint: Tpoint;
  procedure AddPoint(Point: TPoint); override;
  procedure CreateFigure(Point: TPoint); override;
end;

procedure RegisterTool(ATool: TTool; AFigureClass: TFigureClass; BMPSorce: String);
function GetParametersList: TStringArray;
procedure RegisterParam(AName: String; AParamClass: TParamClass);
procedure CreateParametersFromList(List: TStringArray; APanel: TPanel;
  var Params: TParamsArray);



var
ParamsRegister: TParamRecordArray;
ToolsList: array of TTool;


implementation

constructor TPolylineTool.Create;
begin
end;

constructor TRectangleTool.Create;
begin
end;

constructor TRoundRectangleTool.Create;
begin
end;

constructor TEllipseTool.Create;
begin
end;

constructor TLineTool.Create;
begin
end;

function GetParametersList: TStringArray;
var p: TParamRecord;
begin
  for p in ParamsRegister do begin
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := p.Name;
  end;
end;

procedure CreateParametersFromList(List: TStringArray; APanel: TPanel; var Params: TParamsArray);
var
  p: TParamRecord; s: String;
begin
  for s in List do
    for p in ParamsRegister do
      if (s = p.Name) then
      begin
        SetLength(Params, Length(Params) + 1);
        Params[High(Params)] := p.ParamClass.Create(APanel, s);
        break;
      end;
end;

procedure RegisterParam(AName: String; AParamClass: TParamClass);
begin
  SetLength(ParamsRegister, Length(ParamsRegister) + 1);
  with ParamsRegister[High(ParamsRegister)] do
  begin
    Name := AName;
    ParamClass := AParamClass;
  end;
end;


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
    PenColor:= MPenColor;
    FillColor:= MFillColor;
    FillStyle:= MFillStyle;
    PenStyle:= MLineStyle;
    WRadius:= MRadiusW;
    HRadius:= MRadiusH;

  for p in Params do
    case p.ParamLabel.Caption of
      PenStyleLabel: PenStyle:= MLineStyle;
      LineWidthLabel: LineWidth:= MLineWidth;
      PenColorLabel: PenColor:= MPenColor;
      FillColorLabel: FillColor:= MFillColor;
      FillStyleLabel: FillStyle:= MFillStyle;
      Radius:
        begin
        WRadius:= MRadiusW;
        HRadius:= MRadiusH;
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
var
  i: TParams;
begin
  Panel.Visible:= True;

  for i in Params do
    i.SetParamToInit;
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
  Params[i + 1]:= TPenStyleParams.Create(APanel, PenStyleLabel);
  Params[i + 3]:= TColorParams.Create(APanel, PenColorLabel);
  Params[i + 2]:= TSpinParams.Create(APanel, LineWidthLabel);
end;

procedure TTool.AddBrushParams(APanel: TPanel);
var
  i: integer;
begin
  i := High(Params); SetLength(Params, Length(Params) + 1);
  Params[i + 2]:= TColorParams.Create(APanel, FillColorLabel);
  Params[i + 1]:= TBrushStyleParams.Create(APanel, FillStyleLabel);
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
  CreateParametersFromList(GetParametersList, Panel, Params);
  UpdateParameters;
end;

procedure TTool.UpdateParameters;
var
  ParamsList, FigureList: TStringArray;
  figure: TFigures;
  param: TParams;
  contains: boolean;
  sParam: String;
  i, j, k: integer;
begin
  for param in Params do
    param.ParamPanel.Visible := False;
  k := 0;

  ParamsList := GetParametersList;
  for figure in CanvasFigures do
    if figure.Selected then
    begin
      k := k + 1;
      j := 0;
      FigureList := figure.GetParametersList;
      for i := Low(ParamsList) to High(ParamsList) do
      begin
        contains := False;
        for sParam in FigureList do
          if ParamsList[i] = sParam
            then contains := True;
        if contains then
        begin
          ParamsList[j] := ParamsList[i];
          j := j + 1;
        end;
      end;
      SetLength(ParamsList, j);
    end;
  if (k > 0) then
    for param in Params do
      for sParam in ParamsList do
        if (sParam = param.ParamLabel.Caption) then
        begin
          param.ParamPanel.Visible := True;
          break;
        end;
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
  Params[i+1] := TSpinParams.Create(Panel, Radius);
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

procedure TSelectTool.MouseUp (Button: TMouseButton; X, Y: integer);
var
  ToolRegion: HRGN;
  i: integer;
  //Prop: array of TProperty;
  //Values: array of integer;
begin
  with CanvasFigures[high(CanvasFigures)] do
    begin
      Region := CreateRectRgn(
      WorldToScreen(Points[0]).x,WorldToScreen(Points[0]).y,
      WorldToScreen(Points[high(points)]).x,WorldToScreen(Points[high(points)]).y);
    end;

  //if (not CtrlPressed) then
   // begin
      for i :=0 to high(CanvasFigures)-1 do
        begin
          if (CombineRgn(ToolRegion,CanvasFigures[i].Region,CanvasFigures[high(CanvasFigures)].Region,RGN_AND)
            <> NullRegion) then
            CanvasFigures[i].Selected := false;
        end;
    //end;

  with CanvasFigures[high(CanvasFigures)] do
    begin
      If not((Points[0].X=Points[high(points)].X) and (Points[0].Y=Points[high(points)].Y)) then
        RectSelectTool(Point(X, Y))
      else
        PointSelectTool(Point(X, Y));
     end;
  SetLength(CanvasFigures, length(CanvasFigures) - 1);

end;

procedure TSelectTool.RectSelectTool(Point: TPoint);
var
  i:integer;
  ToolRegioN: HRGN;
begin
    for i := 0 to high(CanvasFigures)-1 do
    begin
        DeleteObject(CanvasFigures[i].Region);
        CanvasFigures[i].CreateRegion;
        ToolRegioN := CreateRectRgn(1,1,2,2);
        if (CombineRgn(ToolRegioN,CanvasFigures[i].Region,CanvasFigures[high(CanvasFigures)].Region,RGN_AND)
          <> NULLREGION) then
            begin
              if CanvasFigures[i].Selected = false then
                CanvasFigures[i].Selected := true
              else
                CanvasFigures[i].Selected := false;
            end;
        DeleteObject(ToolRegion);
    end;
end;

procedure TSelectTool.PointSelectTool(Point: TPoint);
var
  i:integer;
begin
      for i := high(CanvasFigures)-1 downto low(CanvasFigures)  do
      begin
        with CanvasFigures[i] do
        begin
          DeleteObject(Region);
          CreateRegion;
          if PtInRegion(Region,Point.X,Point.Y)=true then
            begin
              if Selected = false then
                Selected := true
              else
                Selected := false;
            end;
        end;
      end;
end;

procedure TMoveTool.AddPoint(Point: TPoint);
var
  i, j: integer;
  P: TPoint;
begin
  P.x:= Point.x - APoint.x;
  P.y:= Point.y - APoint.y;
  for i:=0 to High(CanvasFigures) do
    if CanvasFigures[i].Selected then
      for j:=0 to High(CanvasFigures[i].Points) do
        begin
        CanvasFigures[i].Points[j].X := CanvasFigures[i].Points[j].X + ScreenToWorld(P).X - Offset.x;
        CanvasFigures[i].Points[j].Y := CanvasFigures[i].Points[j].Y + ScreenToWorld(P).Y - offset.y;
        end;
  APoint:= Point;
end;

procedure TMoveTool.CreateFigure(Point: TPoint);
begin
  APoint := Point;
  //Drawing := True;
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
var
  TopLeft, BottomRight: WorldPoint;
  NewScale: double;
begin
  case Button of
    mbRight: Zoom(Point(X,Y), scale - 0.5);
    mbLeft:
      begin
        with CanvasFigures[High(CanvasFigures)] do
          begin
            TopLeft:= FindTopLeft;
            BottomRight:= FindBottomRight;
          end;
        if (sqr(TopLeft.x - BottomRight.x) + sqr(TopLeft.y - BottomRight.y) < 16*16) then
           Zoom(Point(X, Y), Scale + 0.5)
        else
          begin
            NewScale := Scale * Min(PBWidth / Scale / (BottomRight.x - TopLeft.x),
                        PBHeight / Scale / (BottomRight.y - TopLeft.y));
            Zoom(WorldToScreen(WPoint((TopLeft.x + BottomRight.x) / 2,
                (TopLeft.y + BottomRight.y) / 2)), NewScale);
            inherited;
          end;
        SetLength(CanvasFigures, Length(CanvasFigures) - 1);
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
RegisterTool(TRectangleTool.Create, TRectangle, 'TRectangle.ico');
RegisterTool(TRoundRectangleTool.Create, TRoundRect, 'TRoundRect.ico');
RegisterTool(TEllipseTool.Create, TEllipse, 'TEllipse.ico');
RegisterTool(THand.Create, TRectangle, 'THand.ico');
RegisterTool(TMagnifier.Create, TRectangle, 'TMagnifier.ico');
RegisterTool(TSelectTool.Create, TRectangle, 'TSelect.ico');
RegisterTool(TMoveTool.Create, TRectangle, 'TMove.ico');
end.

