unit UnitParams;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, Forms, ExtCtrls, StdCtrls, Dialogs, Spin;

type

  TParams = Class
    ParamsPanel: TPanel;
  Protected
    FillStyleParamName, LineColorParamName, LineWidthParamName, RadiusParamName, FillColorParamName : TLabel;
    FillStyleParam: TComboBox;
    LineColorParam, FillColorParam: TColorButton;
    LineWidthParam, RadiusParam: TSpinEdit;
    LineColorDialogParam, FillColorDialogParam: TColorDialog;
    LineColor, FillColor: TColor;
    LineWidth, RadiusW, RadiusH : Integer;
  Public
    Constructor Create(
      ALineColor: TColor;
      AFillColor: TColor;
      ALineWidth: Integer;
      AFillStyle: TBrushStyle;
      ARadiusW: Integer;
      ARadiusH: Integer); Virtual; Abstract;
    procedure LineColorDialogClose(Sender: TObject);
    procedure FillColorDialogClose(Sender: TObject);
    procedure WidthChange(Sender: Tobject);
    procedure RadiusChange(Sender: TObject);





  end;

   TPolyLineParams = Class(TParams)
   Public
    Constructor Create(
      ALineColor: TColor;
      AFillColor: TColor;
      ALineWidth: Integer;
      AFillStyle: TBrushStyle;
      ARadiusW: Integer;
      ARadiusH: Integer); Override;
   end;

  TLineParams = Class(TParams)
  Public
    Constructor Create(
      ALineColor: TColor;
      AFillColor: TColor;
      ALineWidth: Integer;
      AFillStyle: TBrushStyle;
      ARadiusW: Integer;
      ARadiusH: Integer); Override;
   end;

   TRectangleParams = Class(TParams)
   Public
    Constructor Create(
      ALineColor: TColor;
      AFillColor: TColor;
      ALineWidth: Integer;
      AFillStyle: TBrushStyle;
      ARadiusW: Integer;
      ARadiusH: Integer); Override;
   end;

   TElipseParams = Class(TParams)
   Public
    Constructor Create(
      ALineColor: TColor;
      AFillColor: TColor;
      ALineWidth: Integer;
      AFillStyle: TBrushStyle;
      ARadiusW: Integer;
      ARadiusH: Integer); Override;
   end;

   TRoundRectParams = Class(TParams)
   Public
    Constructor Create(
      ALineColor: TColor;
      AFillColor: TColor;
      ALineWidth: Integer;
      AFillStyle: TBrushStyle;
      ARadiusW: Integer;
      ARadiusH: Integer); Override;
   end;


implementation

   procedure TParams.RadiusChange(Sender: TObject);
   begin
     RadiusW:= RadiusParam.Value;
     RadiusH:= RadiusParam.Value;
   end;

   procedure TParams.FillColorDialogClose(Sender: TObject);
   begin
     FillColor:= FillColorDialogParam.Color;
   end;

   procedure TParams.LineColorDialogClose(Sender: TObject);
   begin
     LineColor:= LineColorDialogParam.Color;
   end;

   procedure TParams.WidthChange(Sender: TObject);
   begin
     LineWidth:= LineWidthParam.Value;
   end;

   Constructor TPolyLineParams.Create( ALineColor: TColor; AFillColor: TColor;
     ALineWidth: integer; AFillStyle: TBrushStyle; ARadiusW: Integer; ARadiusH: Integer);
   begin
     LineColorDialogParam:= TColorDialog.Create(ParamsPanel);
     LineColorDialogParam.OnClose:= @LineColorDialogClose;

     LineColorParam:= TColorButton.Create(ParamsPanel);
     With LineColorParam do
     begin
       Name:= 'LineColorParam';
       ColorDialog:= LineColorDialogParam;
       Align:= AlBottom;
       Width:= 77;
       Height:= 29;
       ButtonColor:= LineColor;
     end;

     LineColorParamName:= TLabel.Create(ParamsPanel);
     With LineColorParamName do
     begin
       Name:= 'LineColorName';
       Align:= AlBottom;
       Width:= 76;
       Height:= 20;
       Caption:= 'Line Color';
     end;

     LineWidthParam:= TSpinEdit.Create(ParamsPanel);
     With LineWidthParam do
     begin
       Name:= 'LineWidth';
       Align:= AlBottom;
       Width:= 74;
       Height:= 23;
       Value:= LineWidth;
       MinValue:= 1;
       MaxValue:= 100;
       OnChange:=  @WidthChange;
     end;

     LineWidthParamName:= TLabel.Create(ParamsPanel);
     With LineWidthParamName do
     begin
       Name:= 'LineWidthName';
       Align:= AlBottom;
       Width:= 76;
       Height:= 20;
       Caption:= 'Line Width';
     end;
   end;

   Constructor TLineParams.Create( ALineColor: TColor; AFillColor: TColor;
     ALineWidth: integer; AFillStyle: TBrushStyle; ARadiusW: Integer; ARadiusH: Integer);
   begin
     LineColorDialogParam:= TColorDialog.Create(ParamsPanel);
     LineColorDialogParam.OnClose:= @LineColorDialogClose;

     LineColorParam:= TColorButton.Create(ParamsPanel);
     With LineColorParam do
     begin
       Name:= 'LineColorParam';
       ColorDialog:= LineColorDialogParam;
       Align:= AlBottom;
       Width:= 77;
       Height:= 29;
       ButtonColor:= LineColor;
     end;

     LineColorParamName:= TLabel.Create(ParamsPanel);
     With LineColorParamName do
     begin
       Name:= 'LineColorName';
       Align:= AlBottom;
       Width:= 76;
       Height:= 20;
       Caption:= 'Line Color';
     end;

     LineWidthParam:= TSpinEdit.Create(ParamsPanel);
     With LineWidthParam do
     begin
       Name:= 'LineWidth';
       Align:= AlBottom;
       Width:= 74;
       Height:= 23;
       Value:= LineWidth;
       MinValue:= 1;
       MaxValue:= 100;
       OnChange:=  @WidthChange;
     end;

     LineWidthParamName:= TLabel.Create(ParamsPanel);
     With LineWidthParamName do
     begin
       Name:= 'LineWidthName';
       Align:= AlBottom;
       Width:= 76;
       Height:= 20;
       Caption:= 'Line Width';
     end;
   end;

   Constructor TRectangleParams.Create ( ALineColor: TColor; AFillColor: TColor;
     ALineWidth: integer; AFillStyle: TBrushStyle; ARadiusW: Integer; ARadiusH: Integer);
   begin
     LineColorDialogParam:= TColorDialog.Create(ParamsPanel);
     LineColorDialogParam.OnClose:= @LineColorDialogClose;

     FillColorDialogParam:= TColorDialog.Create(ParamsPanel);
     FillColorDialogParam.OnClose:= @FillColorDialogClose;

     LineColorParam:= TColorButton.Create(ParamsPanel);
     With LineColorParam do
     begin
       Name:= 'LineColorParam';
       ColorDialog:= LineColorDialogParam;
       Align:= AlBottom;
       Width:= 77;
       Height:= 29;
       ButtonColor:= LineColor;
     end;

     LineColorParamName:= TLabel.Create(ParamsPanel);
     With LineColorParamName do
     begin
       Name:= 'LineColorName';
       Align:= AlBottom;
       Width:= 76;
       Height:= 20;
       Caption:= 'Line Color';
     end;

     LineWidthParam:= TSpinEdit.Create(ParamsPanel);
     With LineWidthParam do
     begin
       Name:= 'LineWidth';
       Align:= AlBottom;
       Width:= 74;
       Height:= 23;
       Value:= LineWidth;
       MinValue:= 1;
       MaxValue:= 100;
       OnChange:=  @WidthChange;
     end;

     LineWidthParamName:= TLabel.Create(ParamsPanel);
     With LineWidthParamName do
     begin
       Name:= 'LineWidthName';
       Align:= AlBottom;
       Width:= 76;
       Height:= 20;
       Caption:= 'Line Width';
     end;

     FillColorParam:= TColorButton.Create(ParamsPanel);
     With FillColorParam Do
     begin
       Name:= 'FillColor';
       Align:=AlBottom;
       Width:= 77;
       Height:= 29;
       ButtonColor:= FillColor;
       ColorDialog:= FillColorDialogParam;
     end;

     FillColorParamName:= TLabel.Create(ParamsPanel);
     With FillColorParamName do
     begin
       Name:= 'FillColorName';
       Align:= AlBottom;
       Width:= 76;
       Height:= 20;
       Caption:= 'Fill Color';
     end;

     FillStyleParam:= TComboBox.Create(ParamsPanel);
     With FillStyleParam do
     begin
       Name:= 'FillStyle';
       Align:= AlBottom;
       Width:= 76;
       Height:= 23;
       //
       //
       //
       //
       //
       //
       //
     end;

     FillStyleParamName:= TLabel.Create(ParamsPanel);
     With FillStyleParamName do
     begin
       Name:= 'FillStyleName';
       Align:= AlBottom;
       Width:= 76;
       Height:= 20;
       Caption:= 'Fill Style';
     end;


   end;

   Constructor TElipseParams.Create ( ALineColor: TColor; AFillColor: TColor;
     ALineWidth: integer; AFillStyle: TBrushStyle; ARadiusW: Integer; ARadiusH: Integer);
   begin
     LineColorDialogParam:= TColorDialog.Create(ParamsPanel);
     LineColorDialogParam.OnClose:= @LineColorDialogClose;

     FillColorDialogParam:= TColorDialog.Create(ParamsPanel);
     FillColorDialogParam.OnClose:= @FillColorDialogClose;

     LineColorParam:= TColorButton.Create(ParamsPanel);
     With LineColorParam do
     begin
       Name:= 'LineColorParam';
       ColorDialog:= LineColorDialogParam;
       Align:= AlBottom;
       Width:= 77;
       Height:= 29;
       ButtonColor:= LineColor;
     end;

     LineColorParamName:= TLabel.Create(ParamsPanel);
     With LineColorParamName do
     begin
       Name:= 'LineColorName';
       Align:= AlBottom;
       Width:= 76;
       Height:= 20;
       Caption:= 'Line Color';
     end;

     LineWidthParam:= TSpinEdit.Create(ParamsPanel);
     With LineWidthParam do
     begin
       Name:= 'LineWidth';
       Align:= AlBottom;
       Width:= 74;
       Height:= 23;
       Value:= LineWidth;
       MinValue:= 1;
       MaxValue:= 100;
       OnChange:=  @WidthChange;
     end;

     LineWidthParamName:= TLabel.Create(ParamsPanel);
     With LineWidthParamName do
     begin
       Name:= 'LineWidthName';
       Align:= AlBottom;
       Width:= 76;
       Height:= 20;
       Caption:= 'Line Width';
     end;

     FillColorParam:= TColorButton.Create(ParamsPanel);
     With FillColorParam Do
     begin
       Name:= 'FillColor';
       Align:=AlBottom;
       Width:= 77;
       Height:= 29;
       ButtonColor:= FillColor;
       ColorDialog:= FillColorDialogParam;
     end;

     FillColorParamName:= TLabel.Create(ParamsPanel);
     With FillColorParamName do
     begin
       Name:= 'FillColorName';
       Align:= AlBottom;
       Width:= 76;
       Height:= 20;
       Caption:= 'Fill Color';
     end;

     FillStyleParam:= TComboBox.Create(ParamsPanel);
     With FillStyleParam do
     begin
       Name:= 'FillStyle';
       Align:= AlBottom;
       Width:= 76;
       Height:= 23;
       //
       //
       //
       //
       //
       //
       //
     end;

     FillStyleParamName:= TLabel.Create(ParamsPanel);
     With FillStyleParamName do
     begin
       Name:= 'FillStyleName';
       Align:= AlBottom;
       Width:= 76;
       Height:= 20;
       Caption:= 'Fill Style';
     end;
   end;

   Constructor TRoundRectParams.Create ( ALineColor: TColor; AFillColor: TColor;
     ALineWidth: integer; AFillStyle: TBrushStyle; ARadiusW: Integer; ARadiusH: Integer);
   begin
     LineColorDialogParam:= TColorDialog.Create(ParamsPanel);
     LineColorDialogParam.OnClose:= @LineColorDialogClose;

     FillColorDialogParam:= TColorDialog.Create(ParamsPanel);
     FillColorDialogParam.OnClose:= @FillColorDialogClose;

     LineColorParam:= TColorButton.Create(ParamsPanel);
     With LineColorParam do
     begin
       Name:= 'LineColorParam';
       ColorDialog:= LineColorDialogParam;
       Align:= AlBottom;
       Width:= 77;
       Height:= 29;
       ButtonColor:= LineColor;
     end;

     LineColorParamName:= TLabel.Create(ParamsPanel);
     With LineColorParamName do
     begin
       Name:= 'LineColorName';
       Align:= AlBottom;
       Width:= 76;
       Height:= 20;
       Caption:= 'Line Color';
     end;

     LineWidthParam:= TSpinEdit.Create(ParamsPanel);
     With LineWidthParam do
     begin
       Name:= 'LineWidth';
       Align:= AlBottom;
       Width:= 74;
       Height:= 23;
       Value:= LineWidth;
       MinValue:= 1;
       MaxValue:= 100;
       OnChange:=  @WidthChange;
     end;

     LineWidthParamName:= TLabel.Create(ParamsPanel);
     With LineWidthParamName do
     begin
       Name:= 'LineWidthName';
       Align:= AlBottom;
       Width:= 76;
       Height:= 20;
       Caption:= 'Line Width';
     end;

     FillColorParam:= TColorButton.Create(ParamsPanel);
     With FillColorParam Do
     begin
       Name:= 'FillColor';
       Align:=AlBottom;
       Width:= 77;
       Height:= 29;
       ButtonColor:= FillColor;
       ColorDialog:= FillColorDialogParam;
     end;

     FillColorParamName:= TLabel.Create(ParamsPanel);
     With FillColorParamName do
     begin
       Name:= 'FillColorName';
       Align:= AlBottom;
       Width:= 76;
       Height:= 20;
       Caption:= 'Fill Color';
     end;

     FillStyleParam:= TComboBox.Create(ParamsPanel);
     With FillStyleParam do
     begin
       Name:= 'FillStyle';
       Align:= AlBottom;
       Width:= 76;
       Height:= 23;
       //
       //
       //
       //
       //
       //
       //
     end;

     FillStyleParamName:= TLabel.Create(ParamsPanel);
     With FillStyleParamName do
     begin
       Name:= 'FillStyleName';
       Align:= AlBottom;
       Width:= 76;
       Height:= 20;
       Caption:= 'Fill Style';
     end;

     RadiusParam:= TSpinEdit.Create(ParamsPanel);
     With RadiusParam do
     begin
       Name:= 'Radius';
       Align:= AlBottom;
       Width:= 74;
       Height:= 23;
       MaxValue:= 100;
       MinValue:= 1;
       Value:= RadiusH;
       OnChange:= @RadiusChange;
     end;

     RadiusParamName:= TLabel.Create(ParamsPanel);
     With RadiusParamName do
     begin
       Name:= 'RadiusStyleName';
       Align:= AlBottom;
       Width:= 76;
       Height:= 20;
       Caption:= 'Radius';
     end;

   end;

end.

