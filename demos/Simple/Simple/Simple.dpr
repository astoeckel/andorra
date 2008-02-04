program Simple;

uses
  Forms,
  Main in 'Main.pas',
  AdVCLComponentWindow in '..\..\..\src\AdVCLComponentWindow.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Andorra 2D Simple Demo';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
