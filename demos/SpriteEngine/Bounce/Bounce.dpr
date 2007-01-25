program Bounce;

uses
  Forms,
  Main in 'Main.pas' {Form1},
  AdDraws in '..\..\..\src\AdDraws.pas',
  AdSprites in '..\..\..\src\AdSprites.pas',
  AdParticles in '..\..\..\src\AdParticles.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Andorra 2D Bounce Demo';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
