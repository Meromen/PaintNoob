program LikePaint;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainUnit, UnitTFigure, UnitTTools, Transformation, unitparams ;
  { you can add units after this }

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TPaintForm, PaintForm);
  Application.Run;
end.

