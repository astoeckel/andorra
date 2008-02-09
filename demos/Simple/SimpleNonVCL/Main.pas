unit Main;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  AdClasses, AdEvents, AdDraws, AdDevIL,
  AdGUI, AdComponents, AdGUIConnector, AdStdWindow;

type
  TAdAppl = class
    private
      AdDraw:TAdDraw;
      AdGUI:TAdGUI;
      AdGUIConnector:TAdGUIConnector;
    public
      MouseX, MouseY : integer;
      procedure Idle(Sender:TObject; var Done:boolean);
      procedure Run;
      procedure MouseMove(Sender: TObject; Shift:TAdShiftState; X, Y:integer);
      procedure KeyDown(Sender: TObject; Key: Word; Shift:TAdShiftState);
  end;

implementation

{ TAdAppl }

procedure TAdAppl.Idle(Sender: TObject; var Done: boolean);
begin
  if AdDraw.CanDraw then
  begin
    AdDraw.ClearSurface($FF00FF);
    AdDraw.BeginScene;

    AdGUI.Update(1);

    AdDraw.EndScene;
    AdDraw.Flip;
  end;

  Done := true;
end;

procedure TAdAppl.KeyDown(Sender: TObject; Key: Word; Shift: TAdShiftState);
begin
  if Key = AVK_ESCAPE then
    AdDraw.Window.Terminate;
end;

procedure TAdAppl.MouseMove(Sender: TObject; Shift: TAdShiftState; X,
  Y: integer);
begin
  MouseX := X;
  MouseY := Y;
end;

procedure TAdAppl.Run;
begin
  AdDraw := TAdDraw.Create(nil);

  {$IFDEF FPC}
    {$IFDEF WIN32}
      AdDraw.DllName := 'AndorraOGLLaz.dll';
    {$ELSE}
      AdDraw.DllName := 'libAndorraOGLLaz.so';
    {$ENDIF}
  {$ELSE}
    AdDraw.DllName := 'AndorraOGL.dll';
  {$ENDIF}

  AdDraw.Display.Width := 800;
  AdDraw.Display.Height := 600;
  AdDraw.Display.BitCount := 32;
  AdDraw.Options := AdDraw.Options + [doMipmaps];
  if AdDraw.Initialize then
  begin
    AdDraw.Window.Events.OnIdle := Idle;
    AdDraw.Window.Events.OnMouseMove := MouseMove;
    AdDraw.Window.Events.OnKeyDown := KeyDown;
    AdDraw.Window.Title := 'Andorra 2D';

    AdDraw.TextureFilter := atLinear;

    AdGUI := TAdGUI.Create(AdDraw);
    AdGUI.Cursors.LoadFromFile('cursors.xml');
    AdGUI.Skin.LoadFromFile('sunna.axs');
    AdGUI.LoadFromFile('test.axg');

    AdGUIConnector := TAdGUIConnector.Create(AdGUI);
    AdGUIConnector.ConnectEventHandlers(AdDraw.Window);

    AdDraw.Window.CursorVisible := false;

    AdDraw.Run;

    AdGUIConnector.Free;
    AdGUI.Free;
  end;
  AdDraw.Free;
end;

end.
