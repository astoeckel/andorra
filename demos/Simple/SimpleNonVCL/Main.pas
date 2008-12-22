{
* This program is licensed under the Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more
* informations.
*
* Inspite of the incompatibility between the Common Public License (CPL) and
* the GNU General Public License (GPL) you're allowed to use this program
* under the GPL.
* You also should have recieved a copy of this license with this file.
* If not, see http://www.gnu.org/licenses/gpl.txt for more informations.
}
unit Main;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  AdFreeImage, AdStdWindow, AdClasses, AdEvents, AdDraws, AdTypes, AdFont,
  AdDLLExplorer;

type
  TAdAppl = class
    private
      AdDraw:TAdDraw;
      AdImage:TAdImage;
    public
      MouseX, MouseY : integer;
      procedure Idle(Sender:TObject; var Done:boolean);
      procedure Run;
      procedure MouseMove(Sender: TObject; Shift:TAdShiftState; X, Y:integer);
      procedure KeyDown(Sender: TObject; Key: Word; Shift:TAdShiftState);
  end;

implementation

{ TAdAppl }

{You should also use AdGLFWWindow, AdSDLWindow or AdWin32Window
 To get rid of the VCL, you should also activate the following compiler switches:
  DO_NOT_INCLUDE_STD_FORMATS (remember that fonts are deactivated too)
  DO_NOT_INCLUDE_STD_WINDOWMGR}

procedure TAdAppl.Run;
begin
  //Only for debuging - you can remove this line
  ReportMemoryLeaksOnShutdown := true;

  //Crate the main surface.
  AdDraw := TAdDraw.Create(nil);

  //Get a plugin. Default plugin is declared within the AdDLLExplorer unit.
  AdDraw.DllName := DefaultPlugin;

  with AdDraw.Display do
  begin
    Width := 800;
    Height := 600;
    BitDepth := ad32Bit;
  end;
  if AdDraw.Initialize then
  begin
    AdDraw.Window.Events.OnIdle := Idle;
    AdDraw.Window.Events.OnMouseMove := MouseMove;
    AdDraw.Window.Events.OnKeyDown := KeyDown;
    AdDraw.Window.Title := 'Andorra 2D';

    AdImage := TAdImage.Create(AdDraw);
    AdImage.Texture.LoadGraphicFromFile('icon64.png');
    AdImage.Restore;

    AdDraw.Run;

    AdImage.Free;
  end;
  AdDraw.Free;
end;

procedure TAdAppl.Idle(Sender: TObject; var Done: boolean);
begin
  if AdDraw.CanDraw then
  begin
    AdDraw.ClearSurface(0);
    AdDraw.BeginScene;

    AdImage.Draw(AdDraw, 0, 0, 0);

    with AdDraw.Canvas do
    begin
      TextOut(MouseX, MouseY, 'Andorra 2D ['+AdDraw.DllName+','+AdDraw.Window.ClassName+']');
      Release;
    end;
    
    AdDraw.EndScene;
    AdDraw.Flip;
  end;

  Done := false;
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

end.
