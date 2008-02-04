program NoVCL;

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
