program TextDemo;

uses
  Forms,
  Main in 'Main.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Andorra 2D Simple Demo';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
