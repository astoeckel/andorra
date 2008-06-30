{
* This program is licensed under the Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more informations.
* 
* Inspite of the incompatibility between the Common Public License (CPL) and the GNU General Public License (GPL) you're allowed to use this program * under the GPL. 
* You also should have recieved a copy of this license with this file. 
* If not, see http://www.gnu.org/licenses/gpl.txt for more informations.
*
* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: AdLCLOGLComponentWindow.pas
* Comment: Contains a window framework class for Lazarus which uses TOpenGLControl to create an OpenGL context on a new, LCL created window
}

{Contains a window framework class for Lazarus which uses TOpenGLControl to create an OpenGL context on a new, LCL created window}
unit AdLCLOGLWindow;

{$MODE DELPHI}

interface

uses
  Interfaces, Classes, Controls, Forms, OpenGLContext,
  AdEvents, AdTypes, AdWindowFramework, AdVCLComponentEventConnector;

type

  { TAdLCLOGLWindow }

  TAdLCLOGLWindow = class(TAdGLContextGeneratingWindowFramework)
    private
      FForm:TForm;
      FContext:TOpenGLControl;
      FBinded:boolean;
      FInitialized:boolean;
      FConnector: TAdVCLComponentEventConnector;
    protected
      procedure SetTitle(AValue:string);override;

      function GetClientWidth:integer;override;
      function GetClientHeight:integer;override;

      procedure SetCursorVisible(AValue:Boolean);override;
    public
      constructor Create;override;
      destructor Destroy;override;

      function BindTo(AObj:Pointer):boolean;override;
      function InitDisplay(AProps:TAdDisplayProperties):boolean;override;

      procedure Run;override;
      procedure Terminate;override;
      
      procedure Swap;override;
  end;

implementation

{ TAdLCLOGLWindow }

constructor TAdLCLOGLWindow.Create;
begin
  inherited;
end;

destructor TAdLCLOGLWindow.Destroy;
begin
  //Important: FConnector has to be freed before everything else!
  if FConnector <> nil then
    FConnector.Free;

  if FContext <> nil then
    FContext.Free;

  if FForm <> nil then
    FForm.Free;

  inherited;
end;

function TAdLCLOGLWindow.GetClientHeight: integer;
begin
  result := 0;
  if FForm <> nil then
    result := FForm.ClientHeight;
end;

function TAdLCLOGLWindow.GetClientWidth: integer;
begin
  result := 0;
  if FForm <> nil then
    result := FForm.ClientWidth;
end;

function TAdLCLOGLWindow.BindTo(AObj: Pointer): boolean;
begin
  FBinded := AObj = nil;
  result := FBinded;
end;

function TAdLCLOGLWindow.InitDisplay(AProps:TAdDisplayProperties): boolean;
begin
  result := false;
  if (FBinded) and not (FInitialized) then
  begin
    Application.Initialize;

    Application.CreateForm(TForm, FForm);

    FForm.Color := 0;
    FForm.BorderIcons := [biSystemMenu];
    FForm.Caption := Title;

    if (AProps.Mode = dmWindowed) or (AProps.Mode = dmDefault) then
    begin
      FForm.BorderStyle := bsSingle;
      FForm.Width := AProps.Width;
      FForm.Height := AProps.Height;
      FForm.Position := poScreenCenter;
    end else
    if (AProps.Mode = dmScreenRes) or (AProps.Mode = dmFullscreen) then
    begin
      if AProps.Mode = dmFullScreen then
      begin
        //
      end;
      FForm.BorderStyle := bsNone;
      FForm.Width := Screen.Width;
      FForm.Height := Screen.Height;
      FForm.Top := 0;
      FForm.Left := 0;
    end else exit;
    
    FContext := TOpenGLControl.Create(FForm);
    FContext.Parent := FForm;
    FContext.Align := alClient;
    FContext.MakeCurrent;

    result := true;
    FInitialized := true;

    SetCursorVisible(true);

    FConnector := TAdVCLComponentEventConnector.Create(FContext, self);
  end;
end;

procedure TAdLCLOGLWindow.Run;
begin
  if FInitialized then
  begin
    Application.Run;
  end;
end;

procedure TAdLCLOGLWindow.Terminate;
begin
  if FForm <> nil then
  begin
    FForm.Close;
  end;
end;

procedure TAdLCLOGLWindow.Swap;
begin
  if FInitialized then
  begin
    FContext.SwapBuffers;
  end;
end;

procedure TAdLCLOGLWindow.SetCursorVisible(AValue: Boolean);
begin
  inherited;
  if FInitialized then
  begin
    if AValue then
      FForm.Cursor := crDefault
    else
      FForm.Cursor := crNone;
  end;
end;

procedure TAdLCLOGLWindow.SetTitle(AValue: string);
begin
  inherited;
  FForm.Caption := AValue;
  Application.Title := AValue;
end;

initialization
  RegisterWindowFramework(TAdLCLOGLWindow);

end.

