unit UnitParams;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Graphics, ExtCtrls, Forms, Controls, Buttons,
  Dialogs, Spin;

type

  TParams = Class
    ParamPanel: TPanel;
    ParamLabel: TLabel;
    constructor Create(APanel: TPanel; ACaption: String);
    procedure ChangeParameter(Sender: TObject); virtual; abstract;
    procedure SetParamToInit; virtual; abstract;
  end;


  TColorParams = class(TParams)
    Param, InitValue: TColor;
    ParamControl: TColorButton;
    constructor Create(APanel: TPanel; ACaption: String; Init: TColor);
    procedure ChangeParameter(Sender: TObject); override;
    procedure SetParamToInit; override;
  end;

  TSpinParams = class(TParams)
    Param, InitValue: Integer;
    ParamControl: TSpinEdit;
    constructor Create(APanel: TPanel; ACaption: String; Init: Integer);
    procedure ChangeParameter(Sender: TObject); override;
    procedure SetParamToInit; override;
  end;

  TComboBoxParams = class(TParams)
    ParamControl: TComboBox;
    constructor Create(APanel: TPanel; ACaption: String);
  end;

  TPenStyleParams = class(TComboBoxParams)
    Param, InitValue: TPenStyle;
    constructor Create(APanel: TPanel; ACaption: String; Init: TPenStyle);
    procedure ChangeParameter(Sender: TObject); override;
    procedure SetParamToInit; override;
  end;

  TBrushStyleParams = class(TComboBoxParams)
    Param, InitValue: TBrushStyle;
    constructor Create(APanel: TPanel; ACaption: String; Init: TBrushStyle);
    procedure ChangeParameter(Sender: TObject); override;
    procedure SetParamToInit; override;
  end;

  TParamsArray = Array of TParams;

  TBrushStyleRecord = record
    Name: String;
    BrushStyle: TBrushStyle;
  end;

   TPenStyleRecord = record
    Name: String;
    PenStyle: TPenStyle;
  end;

   const
     LineWidthLabel = 'Pen Width';
     LineColorLabel = 'Pen Color';
     FillColorLabel = 'Fill Color';
     FillStyleLabel = 'Fill Style';
     Radius = 'Radius';


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
    MLineColor, MFillColor: TColor;
    MLineWidth, MRadiusW, MRadiusH : Integer;
    MFillStyle: TBrushStyle;


implementation

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

constructor TColorParams.Create (APanel: TPanel; ACaption: String; Init: TColor);
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
  InitValue := Init;
  SetParamToInit;
end;

procedure TColorParams.ChangeParameter(Sender: TObject);
begin
  Param := ParamControl.ButtonColor;
  ParamControl.Color := Param;
end;

procedure TColorParams.SetParamToInit;
begin
  Param := InitValue;
  ParamControl.Color := InitValue;
  ParamControl.ButtonColor := InitValue;
end;

constructor TSpinParams.Create(APanel: TPanel; ACaption: String; Init: Integer);
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
  InitValue := Init;
  SetParamToInit;
end;

procedure TSpinParams.ChangeParameter(Sender: TObject);
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
    Param := ParamControl.Value;
  end;
end;

procedure TSpinParams.SetParamToInit;
begin
  Param := InitValue;
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

constructor TBrushStyleParams.Create(APanel: TPanel; ACaption: String; Init: TBrushStyle);
var
 i: TBrushStyleRecord;
begin
  inherited Create(APanel, ACaption);
  with ParamControl do
  begin
    for i in BRUSH_STYLES do
      Items.Add(i.Name);
    InitValue := Init;
    SetParamToInit;
  end;
end;

procedure TBrushStyleParams.ChangeParameter(Sender: TObject);
begin
  Param := BRUSH_STYLES[ParamControl.ItemIndex].BrushStyle;
end;

procedure TBrushStyleParams.SetParamToInit;
var i: integer;
begin
  for i := Low(BRUSH_STYLES) to High(BRUSH_STYLES) do
    if BRUSH_STYLES[i].BrushStyle = InitValue then
      ParamControl.ItemIndex := i;
  Param := InitValue;
end;

constructor TPenStyleParams.Create(APanel: TPanel;
  ACaption: String; Init: TPenStyle);
var i: TPenStyleRecord;
begin
  inherited Create(APanel, ACaption);
  for i in PEN_STYLES do
    ParamControl.Items.Add(i.Name);
  InitValue := Init;
  SetParamToInit;
end;

procedure TPenStyleParams.ChangeParameter(Sender: TObject);
begin
  Param := PEN_STYLES[ParamControl.ItemIndex].PenStyle;
end;

procedure TPenStyleParams.SetParamToInit;
var i: integer;
begin
  for i := Low(PEN_STYLES) to High(PEN_STYLES) do
    if PEN_STYLES[i].PenStyle = InitValue then
      ParamControl.ItemIndex := i;
  Param := InitValue;
end;

end.


