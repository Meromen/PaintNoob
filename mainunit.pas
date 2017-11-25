unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, Spin, ActnList, Buttons, StdCtrls, ComCtrls, UnitTFigure, UnitParams,
  UnitTTools, Transformation ;

Type
    TPointList = array of TPoint;

    TFillStyleItem = record
    Name: string;
    BrushStyle: TBrushStyle;
    end;

  { TPaintForm }

  TPaintForm = class(TForm)
    DrawPlace: TPaintBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    ParamsPanel: TPanel;
    SaveDialog1: TSaveDialog;
    SpinEdit1: TSpinEdit;
    ToolsPanel: TPanel;
    HScrollBar: TScrollBar;
    VScrollBar: TScrollBar;
    PanelForAll: TPanel;
    procedure DrawPlaceClick(Sender: TObject);
    procedure DrawPlaceMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawPlaceMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DrawPlaceMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawPlacePaint(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure TRectangleClick(Sender: TObject);
    procedure TElipseClick(Sender: TObject);
    procedure TLineClick(Sender: TObject);
    procedure TPolyLineClick(Sender: TObject);
    procedure TRoundRectClick(Sender: TObject);
    procedure THandClick(Sender: TObject);
    procedure TMagnifierClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ToolsButClick(Sender: TObject);


  private
    { private declarations }
  public
    { public declarations }
  end;

var
  PaintForm: TPaintForm;
  CurrentFillColor, CurrentLineColor: TColor;
  CurrentLineWidth, CurrentFillStyle, CurrentRadius: Integer;
  CanvasFigures: Array of TFigures;
  CanvasTools: Array of TTool;
  CurrentFigure: TFigureClass;
  CurrentTool: TToolClass;
  Offset1: Tpoint;
  LastOffset: TPoint;

  IsDrawing: Boolean;
  FigureNow: Boolean;
  ToolNow: Boolean;
  FillStyles: array[0..3] of TFillStyleItem =
    ( ( Name: 'Clear';
        BrushStyle: bsClear),
      ( Name: 'Solid';
        BrushStyle: bsSolid),
      ( Name: 'Horizontal';
        BrushStyle: bsHorizontal),
      ( Name: 'Vertical' ;
        BrushStyle: bsVertical ) );


implementation

{$R *.lfm}

{ TPaintForm }

const
  ButtonSize = 32;



procedure TPaintForm.FormCreate(Sender: TObject);
var
  MyButton: TSpeedButton;
  CurrentIcon: TPicture;

  FillStylesList: TFillStyleItem;
  i: Integer;
begin
  FillStyle:= bsSolid;
  LineWidth:= 1;
  FillColor:= clWhite;
  LineColor:= clBlack;
  RadiusW:= 10;
  SpinEdit1.Value:= 100;
  IsDrawing:= false;

for i:= 0 to High(FigureList)do
  begin
    MyButton:= TSpeedButton.Create(PanelForAll);
    MyButton.Parent:= PanelForAll;
    MyButton.Name:= 'Button' + FigureList[i].ClassName;
    MyButton.Tag:= i;
    MyButton.Color:= clBlack;
    MyButton.OnClick:= @ToolsButClick;

    CurrentIcon:= TPicture.Create;
    CurrentIcon.LoadFromFile(FigureList[i].ClassName + '.ico');
    MyButton.Glyph:= CurrentIcon.Bitmap;

    MyButton.Width:= ButtonSize;
    MyButton.Height:= ButtonSize;
    MyButton.Left:= (i mod 2) * (ButtonSize + 12);
    MyButton.Top:= (i div 2) * (ButtonSize + 5);
  end;

for i:= 0 to High(ToolsList) do
  begin
    MyButton:= TSpeedButton.Create(PanelForAll);
    MyButton.Parent:= PanelForAll;
    MyButton.Name:= 'Button' + ToolsList[i].ClassName;
    MyButton.Tag:= Length(FigureList) + i;
    MyButton.Color:= clBlack;
    MyButton.OnClick:= @ToolsButClick;

    CurrentIcon:= TPicture.Create;
    CurrentIcon.LoadFromFile(ToolsList[i].ClassName + '.ico');
    MyButton.Glyph:= CurrentIcon.Bitmap;

    MyButton.Width:= ButtonSize;
    MyButton.Height:= ButtonSize;
    MyButton.Left:= ((Length(FigureList)+i) mod 2) * (ButtonSize + 12);
    MyButton.Top:= ((Length(FigureList)+i) div 2) * (ButtonSize + 5);
  end;

{for FillStylesList in FillStyles do
  FillStyle.Items.Add(FillStylesList.Name);
  FillStyle.ItemIndex:= 0; }

CurrentFigure:= FigureList[0];
FigureNow:= True;
ToolNow:= False;
end;


procedure TPaintForm.ToolsButClick(Sender: TObject);
begin
//CurrentFigure:= FigureList[(Sender as TSpeedButton).Tag];

  Case (Sender as TSpeedButton).Tag of
  0: TPolyLineClick(Sender);
  1: TElipseClick(Sender);
  2: TLineClick(Sender);
  3: TRectangleClick(Sender);
  4: TRoundRectClick(Sender);
  5: THandClick(Sender);
  6: TMagnifierClick(Sender);
  end;
end;

procedure TPaintForm.TRectangleClick(Sender: TObject);
begin
  FigureNow:= True;
  ToolNow:= False;

  CurrentFigure:= FigureList[(Sender as TSpeedButton).Tag];
  ParamsPanel.DestroyComponents;
  TRectangleParams.Create(LineColor, FillColor, LineWidth, FillStyle, RadiusH, RadiusW, ParamsPanel );
end;

procedure TPaintForm.TElipseClick(Sender: TObject);
begin
  FigureNow:= True;
  ToolNow:= False;

  CurrentFigure:= FigureList[(Sender as TSpeedButton).Tag];
  ParamsPanel.DestroyComponents;
  TElipseParams.Create(LineColor, FillColor, LineWidth, FillStyle, RadiusH, RadiusW, ParamsPanel );
end;

procedure TPaintForm.TRoundRectClick(Sender: TObject);
begin
  FigureNow:= True;

  CurrentFigure:= FigureList[(Sender as TSpeedButton).Tag];
  ToolNow:= False;

  ParamsPanel.DestroyComponents;
  TRoundRectParams.Create(LineColor, FillColor, LineWidth, FillStyle, RadiusH, RadiusW, ParamsPanel );
end;

procedure TPaintForm.TPolyLineClick(Sender: TObject);
begin
  FigureNow:= True;
  ToolNow:= False;

  CurrentFigure:= FigureList[(Sender as TSpeedButton).Tag];
  ParamsPanel.DestroyComponents;
  TPolyLineParams.Create(LineColor, FillColor, LineWidth, FillStyle, RadiusH, RadiusW, ParamsPanel );
end;

procedure TPaintForm.TLineClick(Sender: TObject);
begin
  FigureNow:= True;
  ToolNow:= False;

  CurrentFigure:= FigureList[(Sender as TSpeedButton).Tag];

  ParamsPanel.DestroyComponents;
  TLineParams.Create(LineColor, FillColor, LineWidth, FillStyle, RadiusH, RadiusW, ParamsPanel );
end;

procedure TPaintForm.THandClick(Sender: TObject);
begin
  FigureNow:= False;
  ToolNow:= True;

  CurrentTool:= ToolsList[(Sender as TSpeedButton).Tag-Length(FigureList)];
  ParamsPanel.DestroyComponents;

end;

procedure TPaintForm.TMagnifierClick(Sender: TObject);
begin
  FigureNow:= False;
  ToolNow:= True;
  CurrentTool:= ToolsList[(Sender as TSpeedButton).Tag-Length(FigureList)];
  ParamsPanel.DestroyComponents;
 end;


procedure TPaintForm.DrawPlaceMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    If FigureNow then
    begin
      IsDrawing:= True;
      SetLength(CanvasFigures, Length(CanvasFigures) + 1);
      CanvasFigures[High(CanvasFigures)]:= CurrentFigure.Create(WorldToScreenX(x), WorldToScreenY(y), LineColor, FillColor, LineWidth, FillStyles[CurrentFillStyle].BrushStyle, RadiusW, RadiusW);
    end;
    If ToolNow then
    begin
      IsDrawing:= True;
      SetLength(CanvasTools, Length(CanvasTools) + 1);
      CanvasTools[High(CanvasTools)]:= CurrentTool.Create((x), (y));
      LastOffset:= Offset1;
    end;
  end;
  DrawPlace.Invalidate;
end;

procedure TPaintForm.DrawPlaceClick(Sender: TObject);
begin

end;

procedure TPaintForm.DrawPlaceMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  If FigureNow then
  if IsDrawing then
  begin
    CanvasFigures[High(CanvasFigures)].MouseMove(x, y);
    DrawPlace.Invalidate;
  end;
  If ToolNow then
  If IsDrawing then
  begin
    CanvasTools[High(CanvasTools)].MouseMove(x, y, LastOffset);
    Offset1:= Offsetout();
    DrawPlace.Invalidate;
  end;
end;

procedure TPaintForm.DrawPlaceMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ToolNow then begin
    CanvasTools[High(CanvasTools)].MouseUp(Button,X,Y);
    DrawPlace.Invalidate;
  end;
  If Button = mbLeft then
  begin
    IsDrawing:= False;
    DrawPlace.Invalidate;
  end;
end;

procedure TPaintForm.DrawPlacePaint(Sender: TObject);
var
  i: TFigures;
begin
 with DrawPlace.Canvas do
   begin
     Pen.Color := clWhite;
     Brush.Color := clWhite;
     Brush.Style := bsSolid;
     Rectangle(0, 0, DrawPlace.Width, DrawPlace.Height);
   end;

  For i in CanvasFigures do
    i.Draw(DrawPlace.Canvas);
end;

procedure TPaintForm.MenuItem2Click(Sender: TObject);
var bmp: TBitmap;
begin
  SaveDialog1.Title:='';
  if SaveDialog1.Execute then
    begin
    bmp := TBitmap.Create;
    try
      bmp.Width := DrawPlace.Width;
      bmp.Height := DrawPlace.Height;
      bmp.Canvas.CopyRect(Rect(0, 0, bmp.Width, bmp.Height), DrawPlace.Canvas, rect (0,0,DrawPlace.Width, DrawPlace.Height));
      bmp.SaveToFile(SaveDialog1.Filename);
      finally
        bmp.Free;
      end;
    end;
end;

procedure TPaintForm.SpinEdit1Change(Sender: TObject);
begin
  scale:= SpinEdit1.Value / 100;
  DrawPlace.Invalidate;
end;


initialization
Offset1.x:= 0;
Offset1.y:= 0;
end.

