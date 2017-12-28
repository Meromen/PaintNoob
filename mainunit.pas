unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, Spin, ActnList, Buttons, StdCtrls, ComCtrls, UnitTFigure, UnitParams,
  UnitTTools, Transformation, LCLType, LCLIntf, UnitConstants, windows;

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
    FileMenu: TMenuItem;
    EditBut: TMenuItem;
    FigureDownBut: TMenuItem;
    FigureUpBut: TMenuItem;
    CopyBut: TMenuItem;
    PustBut: TMenuItem;
    UndoBut: TMenuItem;
    RedoBut: TMenuItem;
    SaveAsButPic: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveAsBut: TMenuItem;
    SaveBut: TMenuItem;
    LoadBut: TMenuItem;
    ParamsPanel: TPanel;
    SaveDialog: TSaveDialog;
    SpinEdit1: TSpinEdit;
    ToolsPanel: TPanel;
    HScrollBar: TScrollBar;
    VScrollBar: TScrollBar;
    PanelForAll: TPanel;
    procedure CopyButClick(Sender: TObject);
    procedure DrawPlaceMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawPlaceMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DrawPlaceMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawPlacePaint(Sender: TObject);
    procedure FigureDownButClick(Sender: TObject);
    procedure FigureUpButClick(Sender: TObject);
    procedure LoadButClick(Sender: TObject);
    procedure EditButClick(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure PustButClick(Sender: TObject);
    procedure RedoButClick(Sender: TObject);
    procedure SaveAsButClick(Sender: TObject);
    procedure SaveAsButPicClick(Sender: TObject);
    procedure SaveButClick(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ToolsButClick(Sender: TObject);
    procedure SaveFile(APictureName: string);
    procedure UndoButClick(Sender: TObject);
    procedure WriteTitle;


  private
    { private declarations }
  public
    { public declarations }
  end;

var
  PaintForm: TPaintForm;
  CurrentFillColor, CurrentPenColor: TColor;
  CurrentLineWidth, CurrentFillStyle, CurrentRadius: Integer;
  CanvasTools: Array of TTool;
  CurrentTool: TTool;
  Offset1: Tpoint;
  LastOffset: TPoint;
  IsDrawing: Boolean;
  PictureName:String = 'New Picture';
  PreviousPicture: String;
  PictureChanged: boolean = false;



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
  MPenColor:= clBlack;
  MRadiusW:= 10;
  MRadiusH:= 10;
  SpinEdit1.Value:= 100;
  IsDrawing:= false;
  PBHeight:= DrawPlace.Height;
  PBWidth:= DrawPlace.Width;


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
  SaveInBuffer;
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

  if BufferPointer <> BufferBegin then
    UndoBut.Enabled := True;
  if (Buffer[(BufferPointer + 1) mod 100] <> Nil)
    and (((BufferPointer + 1) mod 100) <> BufferBegin) then
      RedoBut.Enabled := True;
end;

procedure TPaintForm.FigureDownButClick(Sender: TObject);
var
 t: TFigures;
 i: integer;
begin
 if currentTool is TSelectTool then
     begin
       for i:= 0 to High(CanvasFigures)  do
         if CanvasFigures[i].Selected then
           if i <>  0  then
             begin
               t:= CanvasFigures[i];
               CanvasFigures[i]:= CanvasFigures[i - 1];
               CanvasFigures[i - 1]:= t;
             end;
     end;
   DrawPlace.Invalidate;
end;

procedure TPaintForm.FigureUpButClick(Sender: TObject);
 var
 t: TFigures;
 i: integer;
 begin
  if currentTool is TSelectTool then
     begin
       for i:= 0 to High(CanvasFigures)  do
         if CanvasFigures[i].Selected then
           if i <>  High(CanvasFigures)  then
              begin
                t:= CanvasFigures[i];
                CanvasFigures[i]:= CanvasFigures[i + 1];
                CanvasFigures[i + 1]:= t;
              end;
     end;
   DrawPlace.Invalidate;
end;

procedure TPaintForm.LoadButClick(Sender: TObject);
var
  i,n,j: integer;
  s: string;
begin
  OpenDialog.Filter := 'Picture|*.pnt|';
  OpenDialog.Title := 'Load';
  OpenDialog.Execute;

  PictureName := OpenDialog.FileName;

  AssignFile(input,PictureName);
  Reset(input);
  readln(s);
  if s = '&*_MyPaint_*&' then
    begin
      readln(n);
      SetLength(CanvasFigures, n);
      for i:=0 to n-1 do
        begin
          readln(s);
          for j:=0 to High(ToolsList) do
            if ToolsList[j].FigureClass <> Nil then
              if ToolsList[j].FigureClass.ClassName = s then
                begin
                  CanvasFigures[i] := ToolsList[j].FigureClass.Create(WPoint(0,0));
                  CanvasFigures[i].Load;
                  Break;
                end;
        end;
    end;

  CloseFile(input);
  Invalidate;

end;

procedure TPaintForm.SaveAsButClick(Sender: TObject);
begin
 SaveDialog.InitialDir := GetCurrentDir;
    SaveDialog.Title := 'Save As';
    SaveDialog.DefaultExt := 'pnt';
    SaveDialog.Filter := 'Picture|*.pnt|';
    SaveDialog.FileName := PictureName;
    if SaveDialog.Execute then
      begin
      if FileExists(SaveDialog.FileName) then
        begin
          if (Application.MessageBox('Overwrite file?',
            '', MB_ICONQUESTION + MB_YESNO) = IDYES) then
            begin
              SaveFile(SaveDialog.FileName);
          end else
            begin
              SaveAsBut.Click;
              Exit;
            end;
        end else
          begin
            SaveFile(SaveDialog.FileName);
          end;
    end;
end;

procedure TPaintForm.SaveAsButPicClick(Sender: TObject);
var bmp: TBitmap;
begin
  SaveDialog.Title:='Save as picture';
  SaveDialog.DefaultExt := 'Bmp';
  if SaveDialog.Execute then
    begin
    bmp := TBitmap.Create;
    try
      bmp.Width := DrawPlace.Width;
      bmp.Height := DrawPlace.Height;
      bmp.Canvas.CopyRect(Rect(0, 0, bmp.Width, bmp.Height), DrawPlace.Canvas, rect (0,0,DrawPlace.Width, DrawPlace.Height));
      bmp.SaveToFile(SaveDialog.Filename);
      finally
        bmp.Free;
      end;
    end;
end;


procedure TPaintForm.CopyButClick(Sender: TObject);
var
  i,j: integer;
  s: TStringArray;
begin
  AssignFile(output,'MyClipBoard.pnt');
  rewrite(output);
  writeln('&*_MyPaint_*&');
  writeln(length(CanvasFigures));
  for i:= 0 to High(CanvasFigures)  do
    if CanvasFigures[i].Selected then
      begin
        s:= CanvasFigures[i].Save;
        for j:=0 to high(s) do
          writeln(s[j]);
      end;
   CloseFile(output);
end;

procedure TPaintForm.PustButClick(Sender: TObject);
var
  i,n,j: integer;
  s: string;
begin
  AssignFile(input,'MyClipBoard.pnt');
  Reset(input);
  readln(s);
  if s = '&*_MyPaint_*&' then
    begin
      readln(n);
      SetLength(CanvasFigures, Length(CanvasFigures) + n);
      for i:= Length(CanvasFigures) - n  to Length(CanvasFigures) - 1 do
        begin
          readln(s);
          for j:=0 to High(ToolsList) do
            if ToolsList[j].FigureClass <> Nil then
              if ToolsList[j].FigureClass.ClassName = s then
                begin
                  CanvasFigures[i] := ToolsList[j].FigureClass.Create(WPoint(0,0));
                  CanvasFigures[i].Load;
                  Break;
                end;
        end;
    end;
  CloseFile(input);
  Invalidate;
end;


procedure TPaintForm.SaveButClick(Sender: TObject);
begin
  if (PreviousPicture = PictureName) then
    SaveFile(PictureName)
  else
    SaveAsButClick(TObject.Create);
end;

procedure TPaintForm.SaveFile(APictureName: string);
var
  i,j: integer;
  s: TStringArray;
begin
  AssignFile(output,APictureName);
  rewrite(output);
  writeln('&*_MyPaint_*&');
  writeln(length(CanvasFigures));
  for i:=0 to high(CanvasFigures) do

  CloseFile(output);
  PictureName := APictureName;
  PreviousPicture := APictureName;
  PictureChanged:=false;
  WriteTitle;
end;


procedure TPaintForm.RedoButClick(Sender: TObject);
begin
  if (Buffer[BufferPointer + 1] <> Nil) and (BufferPointer + 1 <> BufferBegin) then
    begin
      BufferPointer := (BufferPointer + 1) mod 100;
      LoadFromBuffer;
      if (Buffer[BufferPointer + 1] = Nil) or (BufferPointer + 1 = BufferBegin) then
        RedoBut.Enabled := False;
    Invalidate;

  end;
end;


procedure TPaintForm.UndoButClick(Sender: TObject);
begin
  if BufferPointer <> BufferBegin then
    BufferPointer := BufferPointer - 1;
  if BufferPointer = -1 then
    BufferPointer := 99;
  LoadFromBuffer;
  if BufferBegin = BufferPointer then
    UndoBut.Enabled := False;
  Invalidate;
end;

procedure TPaintForm.WriteTitle;
begin
  PaintForm.Caption := PictureName;
  if PictureChanged then
    PaintForm.Caption := PaintForm.Caption + '*';
  PaintForm.Caption := PaintForm.Caption + ' -- My Paint';
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

