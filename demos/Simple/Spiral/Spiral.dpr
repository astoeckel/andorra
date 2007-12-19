program Spiral;

uses
  Forms,
  Main in 'Main.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Andorra 2D Spiral';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
