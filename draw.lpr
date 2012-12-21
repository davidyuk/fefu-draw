program draw;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, DrawTools, DrawShapes, DrawEditors, runtimetypeinfocontrols,
  DrawToolsShapes, DrawZoom, DrawToolsZoom;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainF, MainF);
  Application.Run;
end.

