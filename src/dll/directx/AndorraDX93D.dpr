library AndorraDX93D;

uses
  SysUtils,
  Classes,
  DX3DMain in 'DX3DMain.pas',
  AdClasses in '..\..\AdClasses.pas';

{$E .dll}

{$R *.res}

function CreateApplication:TAdApplication;stdcall;
begin
  result := TDXApplication.Create;
end;

exports
  CreateApplication;

begin
end.
