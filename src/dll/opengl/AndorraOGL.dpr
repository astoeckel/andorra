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
* Author: Andreas Stöckel
* Filename: AndorraOGL.dpr
}

library AndorraOGL;

{$IFDEF FPC}
  {$MODE delphi}
{$ENDIF}

uses
  SysUtils,
  AdClasses,
  AdShaderClasses,
  OGLMain in 'OGLMain.pas',
  OGLShader in 'OGLShader.pas',
  OGLShaderClasses in 'OGLShaderClasses.pas';

{$IFNDEF FPC}
  {$E .dll}
  {$R *.res}
{$ENDIF}

function CreateApplication:TAd2DApplication;stdcall;
begin
  result := TOGLApplication.Create;
end;

function CreateShaderSystem:TAd2dShaderSystem;stdcall;
begin
  result := TOGLShaderSystem.Create;
end;

procedure Andorra2DLibraryInformation(var libinfo:TAd2DLibInfo);stdcall;
begin
  with libinfo do
  begin
    LibTitle := 'Andorra OpenGL Plugin';
    LibAuthor := '(c) by Andreas Stöckel 2008';
    LibDescription := 'This plugin wraps around OpenGL.';
    LibVersion := LibraryVersion;
    LibImage := 'ogl.png';
  end;
end;

procedure Andorra2DApplicationProperties(const ASender: TObject;
  const AddPropertyProc: TAd2dPropertyProc);stdcall;
var
  prop: TAd2dProperty;
begin
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

