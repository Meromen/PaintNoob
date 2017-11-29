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
    procedure DrawPlaceMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawPlaceMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DrawPlaceMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawPlacePaint(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
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
  CanvasTools: Array of TTool;
  CurrentTool: TTool;
  Offset1: Tpoint;
  LastOffset: TPoint;
  IsDrawing: Boolean;



implementation

{$R *.lfm}

{ TPaintForm }

const
  ButtonSize = 32;



procedure TPaintForm.FormCreate(Sender: TObject);
var
  MyButton: TSpeedButton;
  i: Integer;

begin
  MFillStyle:= bsSolid;
  MLineWidth:= 1;
  MFillColor:= clWhite;
  MLineColor:= clBlack;
  MRadiusW:= 10;
  SpinEdit1.Value:= 100;
  IsDrawing:= false;


for i:= 0 to High(ToolsList) do
  begin
    ToolsList[i].CreateParams(ParamsPanel);
    MyButton:= TSpeedButton.Create(PanelForAll);
    MyButton.Parent:= PanelForAll;
    MyButton.Name:= 'Button' + ToolsList[i].ClassName;
    MyButton.Tag:= i;
    MyButton.OnClick:= @ToolsButClick;
    MyButton.Glyph:= ToolsList[i].Bitmap.Bitmap;
    MyButton.Width:= ButtonSize;
    MyButton.Height:= ButtonSize;
    MyButton.Left:= (i mod 2) * (ButtonSize + 12);
    MyButton.Top:= (i div 2) * (ButtonSize + 5);
  end;

CurrentTool:= ToolsList[0];
CurrentTool.ShowParameters;
end;


procedure TPaintForm.ToolsButClick(Sender: TObject);
begin
  CurrentTool.HideParameters;
  CurrentTool:= ToolsList[(Sender as TSpeedButton).Tag];
  CurrentTool.ShowParameters;
end;



procedure TPaintForm.DrawPlaceMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
      IsDrawing:= True;
      CurrentTool.CreateFigure(Point(X,Y));
    end;
  DrawPlace.Invalidate;
end;



procedure TPaintForm.DrawPlaceMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if IsDrawing then
  begin
    currentTool.MouseMove(X, Y, LastOffset);
    DrawPlace.Invalidate;
  end;

end;

procedure TPaintForm.DrawPlaceMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  CurrentTool.MouseUp(Button,X,Y);
  IsDrawing:= False;
  DrawPlace.Invalidate;
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

