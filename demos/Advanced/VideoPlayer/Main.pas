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

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

uses
  SysUtils, Dialogs, AdStdWindow, AdDraws, AdPNG,
  AdClasses, AdTypes, AdPerformanceCounter, AdVideo,
  AdGUI, AdComponents, AdGUIConnector, AdSetupDlg,

  //Select one of the following video decoders here
  //AdAcinerella; {GPL only!}
  AdMPEG2Video;

type
  //This class is used to manage our demo application.
  TAdAppl = class
    private
      procedure Idle(Sender:TObject;var Done:boolean);
      procedure OpenClick(Sender: TObject);
      procedure CloseClick(Sender: TObject);
      procedure PlayClick(Sender: TObject);
      procedure PauseClick(Sender: TObject);
      procedure StopClick(Sender: TObject);
      procedure CheckBoxClick(Sender: TObject);
    public
      AdDraw:TAdDraw;
      AdPerCounter:TAdPerformanceCounter;
      AdVideo:TAdVideoPlayer;
      AdGUI: TAdGUI;
      AdGUIConnector: TAdGUIConnector;

      procedure Run;
  end;

const
  Path = './resources/';

implementation

procedure TAdAppl.Idle(Sender: TObject; var Done: boolean);
begin
  //Calculate the time difference
  AdPerCounter.Calculate;

  if AdDraw.CanDraw then
  begin
    //Clear the surface
    AdDraw.ClearSurface(0);
    AdDraw.BeginScene;

    //Move the video forward
    AdVideo.Move(AdPerCounter.TimeGap / 1000);
    //Draw it to the specified area
    AdVideo.Draw(AdDraw,
      AdRect(0,0,AdDraw.Window.ClientWidth,AdDraw.Window.ClientHeight));

    //Display some information about the video
    with AdDraw.Canvas do
    begin
      TextOut(0,0,'FPS: '+inttostr(AdPerCounter.FPS));
      TextOut(0,16,'Video - FPS: '+inttostr(AdVideo.CurrentFPS));
      Release;
    end;

    //Draw/Update the gui
    AdGUI.Update(AdPerCounter.TimeGap / 1000);

    AdDraw.EndScene;
    AdDraw.Flip;
  end;

  Done := false;
end;

procedure TAdAppl.Run;
var
  AdSetup: TAdSetup;
begin
  AdDraw := TAdDraw.Create(nil);

  AdSetup := TAdSetup.Create(AdDraw);
  AdSetup.Image := 'logo1.png';

  if AdSetup.Execute then
  begin
    if AdDraw.Initialize then
    begin
      //Connect the OnIdle event
      AdDraw.Window.Events.OnIdle := Idle;
      //Disable the cursor
      AdDraw.Window.CursorVisible := false;
      //Set the window title
      AdDraw.Window.Title := 'Andorra 2D Video Player';

      //Create the preformance counter
      AdPerCounter := TAdPerformanceCounter.Create;
      //Limit the frame rate to 100 FPS
      AdPerCounter.MaximumFrameRate := 100;

      //Set the current work directory
      if DirectoryExists(path) then
        SetCurrentDir(path);

      //Load the gui
      AdGUI := TAdGUI.Create(AdDraw);
      AdGUI.Skin.LoadFromFile('sunna.axs');
      AdGUI.Cursors.LoadFromFile('cursors.xml');
      AdGUI.LoadFromFile('VideoGUI.axg');

      //Setup event handlers
      AdGUI.FindComponent('btn_open').OnClick := OpenClick;
      AdGUI.FindComponent('btn_close').OnClick := CloseClick;
      AdGUI.FindComponent('btn_play').OnClick := PlayClick;
      AdGUI.FindComponent('btn_pause').OnClick := PauseClick;
      AdGUI.FindComponent('btn_stop').OnClick := StopClick;
      AdGUI.FindComponent('chk_loop').OnClick := CheckBoxClick;
      AdGUI.FindComponent('chk_stretch').OnClick := CheckBoxClick;
      AdGUI.FindComponent('chk_proportional').OnClick := CheckBoxClick;
      AdGUI.FindComponent('chk_center').OnClick := CheckBoxClick;

      //Connect all event handlers to the AdDraw.Window
      AdGUIConnector := TAdGUIConnector.Create(AdGUI);
      AdGUIConnector.ConnectEventHandlers(AdDraw.Window);

      //Create the video player object
      AdVideo := TAdVideoPlayer.Create(AdDraw);
      AdVideo.Image.Texture.Filter := atLinear;

      AdDraw.Run;

      //Free all objects
      AdGUIConnector.Free;
      AdGUI.Free;
      AdVideo.Free;
      AdPerCounter.Free;
    end else
      ShowMessage('Error while initializing Andorra 2D. Try to use another display '+
                  'mode or another video adapter.');
  end;

  //Free all objects
  AdSetup.Free;
  AdDraw.Free;
end;

//
//--- GUI event handlers ---
//

procedure TAdAppl.CheckBoxClick(Sender: TObject);
begin
  //Set the video properties
  AdVideo.Stretch := TAdCheckBox(AdGUI.FindComponent('chk_stretch')).Checked;
  AdVideo.Loop := TAdCheckBox(AdGUI.FindComponent('chk_loop')).Checked;
  AdVideo.Proportional := TAdCheckBox(AdGUI.FindComponent('chk_proportional')).Checked;
  AdVideo.Center := TAdCheckBox(AdGUI.FindComponent('chk_center')).Checked;
end;

procedure TAdAppl.CloseClick(Sender: TObject);
begin
  AdVideo.Close;
end;

procedure TAdAppl.OpenClick(Sender: TObject);
var
  od: TOpenDialog;
begin
  od := TOpenDialog.Create(nil);
  od.Options := od.Options + [ofNoChangeDir];
  if od.Execute then
    AdVideo.Open(od.FileName);
    
  od.Free;
end;

procedure TAdAppl.PauseClick(Sender: TObject);
begin
  AdVideo.Pause;
end;

procedure TAdAppl.PlayClick(Sender: TObject);
begin
  AdVideo.Play;
end;

procedure TAdAppl.StopClick(Sender: TObject);
begin
  AdVideo.Stop;
end;

end.
