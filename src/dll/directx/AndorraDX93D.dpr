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
*
* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: AndorraDX93D.dpr
* Comment: The directx 9 library
}

library AndorraDX93D;

{$IFDEF FPC}
  {$MODE delphi}
{$ENDIF}

uses
  SysUtils,
  AdClasses,
  AdShaderClasses,
  DX3DMain in 'DX3DMain.pas',
  DX3DShader in 'DX3DShader.pas',
  DX3DShaderClasses in 'DX3DShaderClasses.pas',
  DX3DHLSL in 'DX3DHLSL.pas';

{$IFNDEF FPC}
  {$E .dll}
  {$R *.res}
{$ENDIF}

function CreateApplication:TAd2DApplication;stdcall;
begin
  result := TDXApplication.Create;
end;

function CreateShaderSystem:TAd2dShaderSystem;stdcall;
begin
  result := TDXShaderSystem.Create;
end;

procedure Andorra2DLibraryInformation(var libinfo:TAd2DLibInfo);stdcall;
begin
  with libinfo do
  begin
    LibTitle := 'Andorra DirectX9 Plugin';
    LibAuthor := '(c) by Andreas Stöckel 2008';
    LibDescription := 'This plugin wraps around Direct3D 9.';
    LibVersion := LibraryVersion;
    LibImage := 'dx93d.png';
  end;
end;

procedure Andorra2DApplicationProperties(const ASender: TObject;
  const AddPropertyProc: TAd2dPropertyProc);stdcall;
var
  prop: TAd2dProperty;
begin
  //Write properties that have to do with the resolution
  prop.PropGroup := 'Resolution';
  prop.PropViewName := '';

  prop.PropName := 'fullscreen';
  prop.PropType := ptBoolean;
  AddPropertyProc(ASender, prop);

  prop.PropName := 'fullscreen_res';
  prop.PropType := ptResolution;
  AddPropertyProc(ASender, prop);

  prop.PropName := 'adapterindex';
  prop.PropType := ptInteger;
  AddPropertyProc(ASender, prop);

  //Write misc properties
  prop.PropGroup := 'Misc';
  prop.PropType := ptBoolean;

  prop.PropName := 'vsync';
  prop.PropViewName := 'Vertical synchronization';
  AddPropertyProc(ASender, prop);

  prop.PropName := 'antialias';
  prop.PropViewName := 'Antialias';
  AddPropertyProc(ASender, prop);

  prop.PropName := 'bckbuffercount';
  prop.PropType := ptInteger;
  AddPropertyProc(ASender, prop);

  //Write capabilities
  prop.PropGroup := '';
  prop.PropViewName := '';
  prop.PropType := ptReadOnly;

  prop.PropName := 'shaders';
  AddPropertyProc(ASender, prop);
end;

exports
  CreateApplication,
  CreateShaderSystem,
  Andorra2DLibraryInformation,
  Andorra2DApplicationProperties;


begin
end.
