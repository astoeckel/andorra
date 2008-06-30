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
* Comment: Contains a window framework class for Lazarus which uses TOpenGLControl to create an OpenGL context on an existsing control.
}

{Contains a window framework class for Lazarus which uses TOpenGLControl to create an OpenGL context on an existsing control.}
unit AdLCLOGLComponentWindow;

{$MODE DELPHI}

interface

uses
  Interfaces, Classes, Controls, Forms, OpenGLContext,
  AdEvents, AdTypes, AdWindowFramework, AdVCLComponentEventConnector;
  
type

  { TAdLCLOGLWindow }

  TAdLCLOGLComponentWindow = class(TAdGLContextGeneratingWindowFramework)
    private
      FControl:TWinControl;
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

{ TAdLCLOGLComponentWindow }

constructor TAdLCLOGLComponentWindow.Create;
begin
  inherited;
end;

destructor TAdLCLOGLComponentWindow.Destroy;
begin
  if FContext <> nil then
    FContext.Free;
    
  if FConnector <> nil then
    FConnector.Free;
    
  inherited;
end;

function TAdLCLOGLComponentWindow.GetClientHeight: integer;
begin
  result := 0;
  if FContext <> nil then
    result := FContext.ClientHeight;
end;

function TAdLCLOGLComponentWindow.GetClientWidth: integer;
begin
  result := 0;
  if FContext <> nil then
    result := FContext.ClientWidth;
end;

function TAdLCLOGLComponentWindow.BindTo(AObj: Pointer): boolean;
begin
  result := false;
  if (TObject(AObj) is TWinControl) and (not FInitialized) then
  begin
    FControl := TWinControl(AObj);
    FBinded := true;
    result := FBinded;
  end;
end;

function TAdLCLOGLComponentWindow.InitDisplay(AProps:TAdDisplayProperties): boolean;
begin
  result := false;
  if (FBinded) and not (FInitialized) then
  begin
    FContext := TOpenGLControl.Create(FControl.Owner);
    FContext.Parent := FControl;
    FContext.Align := alClient;
    FContext.MakeCurrent;

    result := true;
    FInitialized := true;

    SetCursorVisible(true);
    
    FConnector := TAdVCLComponentEventConnector.Create(FContext, self);
  end;
end;

procedure TAdLCLOGLComponentWindow.Run;
begin
  if FInitialized then
  begin
    Application.Run;
  end;
end;

procedure TAdLCLOGLComponentWindow.Terminate;
begin
  Application.Terminate;
end;

procedure TAdLCLOGLComponentWindow.Swap;
begin
  if FInitialized then
  begin
    FContext.SwapBuffers;
  end;
end;

procedure TAdLCLOGLComponentWindow.SetCursorVisible(AValue: Boolean);
begin
  inherited;
  if FInitialized then
  begin
    if AValue then
      FContext.Cursor := crDefault
    else
      FContext.Cursor := crNone;
  end;
end;

procedure TAdLCLOGLComponentWindow.SetTitle(AValue: string);
begin
  inherited;
  Application.Title := AValue;
end;

initialization
  RegisterWindowFramework(TAdLCLOGLComponentWindow);
  
end.
