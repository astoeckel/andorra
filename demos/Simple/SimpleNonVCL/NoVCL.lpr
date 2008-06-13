program NoVCL;

{$APPTYPE CONSOLE}
{$MODE DELPHI}

uses
  Main in 'Main.pas', AdLCLOGLWindow, LazOpenGLContext;
  
var
  appl:TAdAppl;

begin
  appl := TAdAppl.Create;
  appl.Run;
  appl.Free;
end.

