program Simple3D;

//Set a icon for the application when using windows
{$IFDEF WIN32}
  {$R '..\..\icon.res' '..\..\icon.rc'}
{$ENDIF}

uses
  SysUtils,
  Main in 'Main.pas',
  AdConsts in '..\..\..\src\AdConsts.pas';

var
  Appl: TAdAppl;

begin
  Appl := TAdAppl.Create;
  Appl.Run;
  Appl.Free;
end.
