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

interface

uses
  Dialogs, SysUtils, Forms, Types, Classes, 
  AdPNG, AdDraws, AdClasses, AdPerformanceCounter, AdSetupDlg, AdConsts, AdTypes,
  AdCanvas;
  
type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure Idle(Sender: TObject; var Done: boolean);
  public
    AdDraw:TAdDraw;
    AdPerCounter:TAdPerformanceCounter;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  AdSetup: TAdSetup;
begin
  //Only for debuging - you can remove this line
  ReportMemoryLeaksOnShutdown := true;

  //Crate the main surface.
  AdDraw := TAdDraw.Create(self);

  //Create the setup dialog and pass the main surface
  AdSetup := TAdSetup.Create(AdDraw);
  AdSetup.Image := 'logo1.png';

  if AdSetup.Execute then
  begin
    //Free the setup dialog
    AdSetup.Free;

    //Try to initialize the TAdDraw
    if AdDraw.Initialize then
    begin
      //Create the performance counter. This class is used for measuring the time
      //that passes between two frames.
      AdPerCounter := TAdPerformanceCounter.Create;

      //Connect the on idle event
      AdDraw.Window.Events.OnIdle := Idle;
    end
    else
    begin
      ShowMessage(AdDraw.GetLastError);
      halt;
    end;
  end else
  begin
    AdSetup.Free;
    halt;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  AdPerCounter.Free;
  AdDraw.Free;
end;

procedure TForm1.Idle(Sender: TObject; var Done: boolean);
begin
  //Draw on the TAdDraw if drawing is possible
  if AdDraw.CanDraw then
  begin
    //Calculate the time difference. The information gained here is not used
    //in this demo - But you'll need this class in most cases
    AdPerCounter.Calculate;
    
    //Clear the surface of the TAdDraw with black color. You may also use delphi
    //colors here
    AdDraw.ClearSurface(AdCol24_White);

    AdDraw.BeginScene;        

    //Place all your drawing code between these two lines

    AdDraw.EndScene;

    //Bring the drawn things on the screen
    AdDraw.Flip;
  end;
  Done := false;
end;

end.

