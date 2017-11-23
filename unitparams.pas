unit UnitParams;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, Forms, ExtCtrls, StdCtrls, Dialogs, Spin;

type

  TParams = Class
  Protected
    FillStyleParamName, LineColorParamName, LineWidthParamName, RadiusParamName, FillColorParamName : TLabel;
    FillStyleParam: TComboBox;
    LineColorParam, FillColorParam: TColorButton;
    LineWidthParam, RadiusParam: TSpinEdit;
    LineColorDialogParam, FillColorDialogParam: TColorDialog;

  Public
    Constructor Create(
      ALineColor: TColor;
      AFillColor: TColor;
      ALineWidth: Integer;
      AFillStyle: TBrushStyle;
      ARadiusW: Integer;
      ARadiusH: Integer;
      APanel: TPanel); Virtual; Abstract;
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
      ARadiusH: Integer;
      APanel: TPanel); Override;
   end;

  TLineParams = Class(TParams)
  Public
    Constructor Create(
      ALineColor: TColor;
      AFillColor: TColor;
      ALineWidth: Integer;
      AFillStyle: TBrushStyle;
      ARadiusW: Integer;
      ARadiusH: Integer;
      APanel: TPanel); Override;
   end;

   TRectangleParams = Class(TParams)
   Public
    Constructor Create(
      ALineColor: TColor;
      AFillColor: TColor;
      ALineWidth: Integer;
      AFillStyle: TBrushStyle;
      ARadiusW: Integer;
      ARadiusH: Integer;
      APanel: TPanel); Override;
   end;

   TElipseParams = Class(TParams)
   Public
    Constructor Create(
      ALineColor: TColor;
      AFillColor: TColor;
      ALineWidth: Integer;
      AFillStyle: TBrushStyle;
      ARadiusW: Integer;
      ARadiusH: Integer;
      APanel: TPanel); Override;
   end;

   TRoundRectParams = Class(TParams)
   Public
    Constructor Create(
      ALineColor: TColor;
      AFillColor: TColor;
      ALineWidth: Integer;
      AFillStyle: TBrushStyle;
      ARadiusW: Integer;
      ARadiusH: Integer;
      APanel: TPanel); Override;
   end;

   var
    LineColor, FillColor: TColor;
    LineWidth, RadiusW, RadiusH : Integer;
    FillStyle: TBrushStyle;


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
     ALineWidth: integer; AFillStyle: TBrushStyle; ARadiusW: Integer; ARadiusH: Integer;
      APanel: TPanel);
   begin
     LineColorDialogParam:= TColorDialog.Create(APanel);
     LineColorDialogParam.OnClose:= @LineColorDialogClose;

     LineColorParam:= TColorButton.Create(APanel);
     With LineColorParam do
     begin
       Name:= 'LineColorParam';
       ColorDialog:= LineColorDialogParam;
       Align:= AlBottom;
       Width:= 77;
       Height:= 29;
       ButtonColor:= LineColor;
       Parent:= APanel;
     end;

     LineColorParamName:= TLabel.Create(APanel);
     With LineColorParamName do
     begin
       Name:= 'LineColorName';
       Align:= AlBottom;
       Width:= 76;
       Height:= 20;
       Caption:= 'Line Color';
       Parent:= APanel;
     end;

     LineWidthParam:= TSpinEdit.Create(APanel);
     With LineWidthParam do
     begin
       Name:= 'LineWidth';
       Align:= AlBottom;
       Width:= 74;
       Height:= 23;
       Value:= LineWidth;
       Parent:= APanel;
       MinValue:= 1;
       MaxValue:= 100;
       OnChange:=  @WidthChange;
     end;

     LineWidthParamName:= TLabel.Create(APanel);
     With LineWidthParamName do
     begin
       Name:= 'LineWidthName';
       Align:= AlBottom;
       Width:= 76;
       Height:= 20;
       Caption:= 'Line Width';
       Parent:= APanel;
     end;
   end;

   Constructor TLineParams.Create( ALineColor: TColor; AFillColor: TColor;
     ALineWidth: integer; AFillStyle: TBrushStyle; ARadiusW: Integer; ARadiusH: Integer;
      APanel: TPanel);
   begin
     LineColorDialogParam:= TColorDialog.Create(APanel);
     LineColorDialogParam.OnClose:= @LineColorDialogClose;

     LineColorParam:= TColorButton.Create(APanel);
     With LineColorParam do
     begin
       Name:= 'LineColorParam';
       ColorDialog:= LineColorDialogParam;
       Align:= AlBottom;
       Width:= 77;
       Height:= 29;
       ButtonColor:= LineColor;
       Parent:= APanel;
     end;

     LineColorParamName:= TLabel.Create(APanel);
     With LineColorParamName do
     begin
       Name:= 'LineColorName';
       Align:= AlBottom;
       Width:= 76;
       Height:= 20;
       Caption:= 'Line Color';
       Parent:= APanel;
     end;

     LineWidthParam:= TSpinEdit.Create(APanel);
     With LineWidthParam do
     begin
       Name:= 'LineWidth';
       Align:= AlBottom;
       Width:= 74;
       Height:= 23;
       Parent:= APanel;
       Value:= LineWidth;
       MinValue:= 1;
       MaxValue:= 100;
       OnChange:=  @WidthChange;
     end;

     LineWidthParamName:= TLabel.Create(APanel);
     With LineWidthParamName do
     begin
       Name:= 'LineWidthName';
       Align:= AlBottom;
       Width:= 76;
       Height:= 20;
       Parent:= APanel;
       Caption:= 'Line Width';
     end;
   end;

   Constructor TRectangleParams.Create ( ALineColor: TColor; AFillColor: TColor;
     ALineWidth: integer; AFillStyle: TBrushStyle; ARadiusW: Integer; ARadiusH: Integer;
      APanel: TPanel);
   begin
     LineColorDialogParam:= TColorDialog.Create(APanel);
     LineColorDialogParam.OnClose:= @LineColorDialogClose;

     FillColorDialogParam:= TColorDialog.Create(APanel);
     FillColorDialogParam.OnClose:= @FillColorDialogClose;

     LineColorParam:= TColorButton.Create(APanel);
     With LineColorParam do
     begin
       Name:= 'LineColorParam';
       ColorDialog:= LineColorDialogParam;
       Align:= AlBottom;
       Width:= 77;
       Height:= 29;
       Parent:= APanel;
       ButtonColor:= LineColor;
     end;

     LineColorParamName:= TLabel.Create(APanel);
     With LineColorParamName do
     begin
       Name:= 'LineColorName';
       Align:= AlBottom;
       Width:= 76;
       Height:= 20;
       Parent:= APanel;
       Caption:= 'Line Color';
     end;

     LineWidthParam:= TSpinEdit.Create(APanel);
     With LineWidthParam do
     begin
       Name:= 'LineWidth';
       Align:= AlBottom;
       Width:= 74;
       Height:= 23;
       Parent:= APanel;
       Value:= LineWidth;
       MinValue:= 1;
       MaxValue:= 100;
       OnChange:=  @WidthChange;
     end;

     LineWidthParamName:= TLabel.Create(APanel);
     With LineWidthParamName do
     begin
       Name:= 'LineWidthName';
       Align:= AlBottom;
       Width:= 76;
       Height:= 20;
       Parent:= APanel;
       Caption:= 'Line Width';
     end;

     FillColorParam:= TColorButton.Create(APanel);
     With FillColorParam Do
     begin
       Name:= 'FillColor';
       Align:=AlBottom;
       Width:= 77;
       Height:= 29;
       Parent:= APanel;
       ButtonColor:= FillColor;
       ColorDialog:= FillColorDialogParam;
     end;

     FillColorParamName:= TLabel.Create(APanel);
     With FillColorParamName do
     begin
       Name:= 'FillColorName';
       Align:= AlBottom;
       Width:= 76;
       Height:= 20;
       Parent:= APanel;
       Caption:= 'Fill Color';
     end;

     FillStyleParam:= TComboBox.Create(APanel);
     With FillStyleParam do
     begin
       Name:= 'FillStyle';
       Align:= AlBottom;
       Width:= 76;
       Height:= 23;
       Parent:= APanel;
       //
       //
       //
       //
       //
       //
       //
     end;

     FillStyleParamName:= TLabel.Create(APanel);
     With FillStyleParamName do
     begin
       Name:= 'FillStyleName';
       Align:= AlBottom;
       Width:= 76;
       Height:= 20;
       Parent:= APanel;
       Caption:= 'Fill Style';
     end;


   end;

   Constructor TElipseParams.Create ( ALineColor: TColor; AFillColor: TColor;
     ALineWidth: integer; AFillStyle: TBrushStyle; ARadiusW: Integer; ARadiusH: Integer;
      APanel: TPanel);
   begin
     LineColorDialogParam:= TColorDialog.Create(APanel);
     LineColorDialogParam.OnClose:= @LineColorDialogClose;

     FillColorDialogParam:= TColorDialog.Create(APanel);
     FillColorDialogParam.OnClose:= @FillColorDialogClose;

     LineColorParam:= TColorButton.Create(APanel);
     With LineColorParam do
     begin
       Name:= 'LineColorParam';
       ColorDialog:= LineColorDialogParam;
       Align:= AlBottom;
       Width:= 77;
       Height:= 29;
       Parent:= APanel;
       ButtonColor:= LineColor;
     end;

     LineColorParamName:= TLabel.Create(APanel);
     With LineColorParamName do
     begin
       Name:= 'LineColorName';
       Align:= AlBottom;
       Width:= 76;
       Height:= 20;
       Parent:= APanel;
       Caption:= 'Line Color';
     end;

     LineWidthParam:= TSpinEdit.Create(APanel);
     With LineWidthParam do
     begin
       Name:= 'LineWidth';
       Align:= AlBottom;
       Width:= 74;
       Height:= 23;
       Value:= LineWidth;
       MinValue:= 1;
       MaxValue:= 100;
       Parent:= APanel;
       OnChange:=  @WidthChange;
     end;

     LineWidthParamName:= TLabel.Create(APanel);
     With LineWidthParamName do
     begin
       Name:= 'LineWidthName';
       Align:= AlBottom;
       Width:= 76;
       Height:= 20;
       Parent:= APanel;
       Caption:= 'Line Width';
     end;

     FillColorParam:= TColorButton.Create(APanel);
     With FillColorParam Do
     begin
       Name:= 'FillColor';
       Align:=AlBottom;
       Width:= 77;
       Height:= 29;
       Parent:= APanel;
       ButtonColor:= FillColor;
       ColorDialog:= FillColorDialogParam;
     end;

     FillColorParamName:= TLabel.Create(APanel);
     With FillColorParamName do
     begin
       Name:= 'FillColorName';
       Align:= AlBottom;
       Width:= 76;
       Height:= 20;
       Parent:= APanel;
       Caption:= 'Fill Color';
     end;

     FillStyleParam:= TComboBox.Create(APanel);
     With FillStyleParam do
     begin
       Name:= 'FillStyle';
       Align:= AlBottom;
       Width:= 76;
       Height:= 23;
       Parent:= APanel;
       //
       //
       //
       //
       //
       //
       //
     end;

     FillStyleParamName:= TLabel.Create(APanel);
     With FillStyleParamName do
     begin
       Name:= 'FillStyleName';
       Align:= AlBottom;
       Width:= 76;
       Height:= 20;
       Parent:= APanel;
       Caption:= 'Fill Style';
     end;
   end;

   Constructor TRoundRectParams.Create ( ALineColor: TColor; AFillColor: TColor;
     ALineWidth: integer; AFillStyle: TBrushStyle; ARadiusW: Integer; ARadiusH: Integer;
      APanel: TPanel);
   begin
     LineColorDialogParam:= TColorDialog.Create(APanel);
     LineColorDialogParam.OnClose:= @LineColorDialogClose;

     FillColorDialogParam:= TColorDialog.Create(APanel);
     FillColorDialogParam.OnClose:= @FillColorDialogClose;

     LineColorParam:= TColorButton.Create(APanel);
     With LineColorParam do
     begin
       Name:= 'LineColorParam';
       ColorDialog:= LineColorDialogParam;
       Align:= AlBottom;
       Width:= 77;
       Parent:= APanel;
       Height:= 29;
       ButtonColor:= LineColor;
     end;

     LineColorParamName:= TLabel.Create(APanel);
     With LineColorParamName do
     begin
       Name:= 'LineColorName';
       Align:= AlBottom;
       Width:= 76;
       Parent:= APanel;
       Height:= 20;
       Caption:= 'Line Color';
     end;

     LineWidthParam:= TSpinEdit.Create(APanel);
     With LineWidthParam do
     begin
       Name:= 'LineWidth';
       Align:= AlBottom;
       Width:= 74;
       Height:= 23;
       Value:= LineWidth;
       MinValue:= 1;
       MaxValue:= 100;
       Parent:= APanel;
       OnChange:=  @WidthChange;
     end;

     LineWidthParamName:= TLabel.Create(APanel);
     With LineWidthParamName do
     begin
       Name:= 'LineWidthName';
       Align:= AlBottom;
       Width:= 76;
       Height:= 20;
       Parent:= APanel;
       Caption:= 'Line Width';
     end;

     FillColorParam:= TColorButton.Create(APanel);
     With FillColorParam Do
     begin
       Name:= 'FillColor';
       Align:=AlBottom;
       Width:= 77;
       Height:= 29;
       Parent:= APanel;
       ButtonColor:= FillColor;
       ColorDialog:= FillColorDialogParam;
     end;

     FillColorParamName:= TLabel.Create(APanel);
     With FillColorParamName do
     begin
       Name:= 'FillColorName';
       Align:= AlBottom;
       Width:= 76;
       Height:= 20;
       Parent:= APanel;
       Caption:= 'Fill Color';
     end;

     FillStyleParam:= TComboBox.Create(APanel);
     With FillStyleParam do
     begin
       Name:= 'FillStyle';
       Align:= AlBottom;
       Width:= 76;
       Height:= 23;
       Parent:= APanel;
       //
       //
       //
       //
       //
       //
       //
     end;

     FillStyleParamName:= TLabel.Create(APanel);
     With FillStyleParamName do
     begin
       Name:= 'FillStyleName';
       Align:= AlBottom;
       Width:= 76;
       Height:= 20;
       Parent:= APanel;
       Caption:= 'Fill Style';
     end;

     RadiusParam:= TSpinEdit.Create(APanel);
     With RadiusParam do
     begin
       Name:= 'Radius';
       Align:= AlBottom;
       Width:= 74;
       Parent:= APanel;
       Height:= 23;
       MaxValue:= 100;
       MinValue:= 1;
       Value:= RadiusH;
       OnChange:= @RadiusChange;
     end;

     RadiusParamName:= TLabel.Create(APanel);
     With RadiusParamName do
     begin
       Name:= 'RadiusStyleName';
       Align:= AlBottom;
       Width:= 76;
       Height:= 20;
       Parent:= APanel;
       Caption:= 'Radius';
     end;

   end;

end.

