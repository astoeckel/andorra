program NoVCL;

//Set a icon for the application when using windows
{$IFDEF WIN32}
  {$R '..\..\icon.res' '..\..\icon.rc'}
{$ENDIF}

uses
  Main in 'Main.pas';

var
  appl:TAdAppl;

begin
  ReportMemoryLeaksOnShutdown := true;

  appl := TAdAppl.Create;
  appl.Run;
  appl.Free;
end.
