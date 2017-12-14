unit UnitParams;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Graphics, ExtCtrls, Forms, Controls, Buttons,
  Dialogs, Spin, UnitTFigure, UnitConstants;

type

  TParams = Class
    ParamPanel: TPanel;
    ParamLabel: TLabel;
    constructor Create(APanel: TPanel; ACaption: String);
    procedure ChangeParameter(Sender: TObject); virtual; abstract;
    procedure SetParamToInit; virtual; abstract;
    procedure ParamsInvalidate;
  end;


  TColorParams = class(TParams)

    ParamControl: TColorButton;
    constructor Create(APanel: TPanel; ACaption: String);
    procedure ChangeParameter(Sender: TObject); override;
    procedure SetParamToInit; override;
  end;

  TSpinParams = class(TParams)

    ParamControl: TSpinEdit;
    constructor Create(APanel: TPanel; ACaption: String);
    procedure ChangeParameter(Sender: TObject); override;
    procedure SetParamToInit; override;
  end;

  TComboBoxParams = class(TParams)
    ParamControl: TComboBox;
    constructor Create(APanel: TPanel; ACaption: String);
  end;

  TPenStyleParams = class(TComboBoxParams)

    constructor Create(APanel: TPanel; ACaption: String);
    procedure ChangeParameter(Sender: TObject); override;
    procedure SetParamToInit; override;
  end;

  TBrushStyleParams = class(TComboBoxParams)

    constructor Create(APanel: TPanel; ACaption: String);
    procedure ChangeParameter(Sender: TObject); override;
    procedure SetParamToInit; override;
  end;


  TParamClass = class of TParams;
  TParamsArray = Array of TParams;
  TParamRecord = record
    Name: String;
    ParamClass: TParamClass;
  end;

  TParamRecordArray = array of TParamRecord;

  TBrushStyleRecord = record
    Name: String;
    BrushStyle: TBrushStyle;
  end;

   TPenStyleRecord = record
    Name: String;
    PenStyle: TPenStyle;
  end;


   const

   BRUSH_STYLES: array[0..7] of TBrushStyleRecord = (
    (Name: 'Solid'; BrushStyle: bsSolid),
    (Name: 'Clear'; BrushStyle: bsClear),
    (Name: 'Hstripes'; BrushStyle: bsHorizontal),
    (Name: 'Vstripes'; BrushStyle: bsVertical),
    (Name: 'Left diagonal'; BrushStyle: bsFDiagonal),
    (Name: 'Right diagonal'; BrushStyle: bsBDiagonal),
    (Name: 'Cross'; BrushStyle: bsCross),
    (Name: 'Diagonal cross'; BrushStyle: bsDiagCross) );


  PEN_STYLES: array[0..5] of TPenStyleRecord = (
    (Name: 'Solid'; PenStyle: psSolid),
    (Name: 'No line'; PenStyle: psClear),
    (Name: 'Dots'; PenStyle: psDot),
    (Name: 'Dashes'; PenStyle: psDash),
    (Name: 'Dash dots'; PenStyle: psDashDot),
    (Name: 'Dash dot dots'; PenStyle: psDashDotDot) );


   var
    MPenColor, MFillColor: TColor;
    MLineWidth, MRadiusW, MRadiusH : Integer;
    MFillStyle: TBrushStyle;
    MLineStyle: TPenStyle;


implementation

procedure TParams.ParamsInvalidate;
begin
  ParamPanel.Parent.Invalidate;
end;

constructor TParams.Create(APanel: TPanel; ACaption: String);
begin
  ParamPanel := TPanel.Create(APanel);
  with ParamPanel do
  begin
    Align := alBottom;
    Parent := APanel;
    Width := Parent.Width - 4;
    Height := 52;
    Left := 2;
    Caption := '';
  end;

  ParamLabel := TLabel.Create(ParamPanel);
  with ParamLabel do
  begin
    Parent := ParamPanel;
    AutoSize := False;
    Caption := ACaption;
    Width := Parent.Width;
    Height := 20;
    Top := 0;
    Alignment := taCenter;
    Font.Height := 20;
  end;
end;

constructor TColorParams.Create (APanel: TPanel; ACaption: String);
Begin
  inherited Create(APanel, ACaption);
  ParamControl := TColorButton.Create(ParamPanel);
  with ParamControl do
  begin
    Parent := ParamPanel;
    AutoSize := False;
    ColorDialog := TColorDialog.Create(ParamPanel);
    Height := 30;
    Top := 22;
    Caption := '';
    Width := Parent.Width;
    OnColorChanged := @ChangeParameter;
  end;
end;

procedure TColorParams.ChangeParameter(Sender: TObject);
var
  i: TFigures;
  Param: TColor;
begin
  Param := ParamControl.ButtonColor;
  case ParamLabel.Caption of
    PenColorLabel: MPenColor := Param;
    FillColorLabel: MFillColor := Param;
  end;
  ParamControl.Color := Param;
  for i in CanvasFigures do
    if i.Selected then
      case ParamLabel.Caption of
        PenColorLabel: i.PenColor := Param;
        FillColorLabel: i.FillColor := Param;
      end;
  ParamsInvalidate;
end;

procedure TColorParams.SetParamToInit;
var
  InitValue: TColor;
begin
  case ParamLabel.Caption of
    PenColorLabel: InitValue := MPenColor;
    FillColorLabel: InitValue := MFillColor;
  end;
  ParamControl.Color := InitValue;
  ParamControl.ButtonColor := InitValue;
end;

constructor TSpinParams.Create(APanel: TPanel; ACaption: String);
begin
  inherited Create(APanel, ACaption);
  ParamControl := TSpinEdit.Create(ParamPanel);
  with ParamControl do
  begin
    Parent := ParamPanel;
    AutoSize := False;
    Height := 30;
    Top := 22;
    Width := Parent.Width;
    Font.Size := 12;
    MinValue := 1;
    MaxValue := 200;
    OnEditingDone := @ChangeParameter;
  end;
end;

procedure TSpinParams.ChangeParameter(Sender: TObject);
var
  i: TFigures;
  Param: Integer;
begin
  with ParamControl do
  begin
    try
      if StrToInt(Caption) > MaxValue then
        Caption := IntToStr(MaxValue)
      else if StrToInt(Caption) < MinValue then
        Caption := IntToStr(MinValue);
    except on EConvertError do
      Caption := IntToStr(MinValue);
    end;
  end;
  Param := ParamControl.Value;
  case ParamLabel.Caption of
    LineWidthLabel: MLineWidth := Param;
    Radius: MRadiusH := Param;
  end;
  for i in CanvasFigures do
    if i.Selected then
    begin
      case ParamLabel.Caption of
        LineWidthLabel: i.LineWidth := Param;
        Radius: i.HRadius := Param;
      end;
    end;
  ParamsInvalidate;
end;

procedure TSpinParams.SetParamToInit;
  var
  InitValue: Integer;
begin
  case ParamLabel.Caption of
    LineWidthLabel: InitValue := MLineWidth;
    Radius: InitValue := MRadiusH;
  end;
  ParamControl.Value := InitValue;
  ParamControl.Caption := IntToStr(InitValue);
end;

constructor TComboBoxParams.Create(APanel: TPanel; ACaption: String);
begin
  inherited Create(APanel, ACaption);
  ParamControl := TComboBox.Create(ParamPanel);
  with ParamControl do
  begin
    Parent := ParamPanel;
    ReadOnly := True;
    AutoSize := False;
    Height := 30;
    Width := Parent.Width;
    Top := 22;
    Font.Size := 10;
    OnChange := @ChangeParameter;
  end;
end;

constructor TBrushStyleParams.Create(APanel: TPanel; ACaption: String);
var
 i: TBrushStyleRecord;
begin
  inherited Create(APanel, ACaption);
  with ParamControl do
  begin
    for i in BRUSH_STYLES do
      Items.Add(i.Name);
  end;
end;

procedure TBrushStyleParams.ChangeParameter(Sender: TObject);
var
  i: TFigures;
begin
  MFillStyle := BRUSH_STYLES[ParamControl.ItemIndex].BrushStyle;
  for i in CanvasFigures do
    if i.Selected then i.FillStyle := MFillStyle;
  ParamsInvalidate;
end;

procedure TBrushStyleParams.SetParamToInit;
var
  i: integer;
begin
  for i := Low(BRUSH_STYLES) to High(BRUSH_STYLES) do
    if BRUSH_STYLES[i].BrushStyle = MFillStyle then
      ParamControl.ItemIndex := i;
end;

constructor TPenStyleParams.Create(APanel: TPanel;
  ACaption: String);
var i: TPenStyleRecord;
begin
  inherited Create(APanel, ACaption);
  for i in PEN_STYLES do
    ParamControl.Items.Add(i.Name);
end;

procedure TPenStyleParams.ChangeParameter(Sender: TObject);
var
  i: TFigures;
begin
  MLineStyle := PEN_STYLES[ParamControl.ItemIndex].PenStyle;
  for i in CanvasFigures do
    if i.Selected then i.PenStyle := MLineStyle;
 // ParamsInvalidate;
end;

procedure TPenStyleParams.SetParamToInit;
var
  i: integer;
begin
  for i := Low(PEN_STYLES) to High(PEN_STYLES) do
    if PEN_STYLES[i].PenStyle = MLineStyle then
      ParamControl.ItemIndex := i;
end;

end.


