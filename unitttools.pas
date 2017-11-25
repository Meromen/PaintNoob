unit UnitTTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Transformation, Graphics, Controls;

type
TToolClass = Class of TTool;
TToolsList = Array of TToolClass;

TTool = Class
Protected
  Points: ArrayWorldPoint;

Public
  constructor Create(X,Y: Integer);
  procedure MouseMove(X, Y: Integer; ALastOffset: Tpoint); Virtual; Abstract;
  procedure MouseUp(Button: TMouseButton; X,Y:integer); Virtual; Abstract;
  procedure Draw(Acanvas: TCanvas); Virtual; Abstract;
end;

 {TMagnifier}
TMagnifier = Class (TTool)
Public
procedure MouseMove(X, Y: Integer; ALastOFfset: TPoint); Override;
procedure MouseUp(Button: TMouseButton; X,Y:integer);  Override;
end;

 {THand}
THand = Class (TTool)
Public
procedure MouseMove(X, Y: Integer; ALastOffset: TPoint); Override;
procedure MouseUp(Button: TMouseButton; X,Y:integer);  Override;
end;


procedure RegisterTools(ATools: Array of TClass);

var
ToolsList: TToolsList;


implementation

procedure RegisterTools(ATools: array of TClass);
var
  i: TClass;
begin
  for i in ATools do
  begin
    SetLength(ToolsList, Length(ToolsList)+1);
    ToolsList[High(ToolsList)]:= TToolClass(i);
  end;
end;

constructor TTool.Create(X,Y: Integer);
begin
  SetLength(Points, 2);
  Points[0]:= WPoint(X, Y);
  Points[1]:= Points[0];
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

Procedure TMagnifier.MouseMove(X, Y: Integer; ALastOffset: Tpoint);
begin

end;

 {THand}
procedure THand.MouseUp(Button: TMouseButton; X,Y:integer);
begin

end;

procedure THand.MouseMove(X, Y: Integer; ALastOffset: TPoint);
begin
  Points[1]:= Wpoint(x, y);
   OffSetXY(ALastOffset.x, ALastOffset.y, Points[1].x, Points[1].y, Points[0].x, Points[0].y);

end;



initialization
RegisterTools([
THand,
TMagnifier
]);

end.

