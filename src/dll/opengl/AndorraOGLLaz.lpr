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
* Filename: AndorraOGL.lpr
}

library AndorraOGLLaz;

{$mode delphi}{$H+}

uses
  AdClasses,
  OGLMain in 'OGLMain.pas';
  
function CreateApplication:TAd2DApplication;stdcall;
begin
  result := TOGLApplication.Create;
end;

procedure Andorra2DLibraryInformation(var libinfo:TAd2DLibInfo);stdcall;
begin
  with libinfo do
  begin
    LibTitle := 'Andorra OpenGL Plugin';
    LibAuthor := '(c) by Andreas Stöckel 2007';
    LibDescription := 'This plugin wraps around the OpenGL graphicssystem.';
    LibVersion := '0.2 ALPHA';
    LibImage := 'ogl.png';
  end;
end;

procedure Andorra2DLibraryAbilities(var libabilities:TAd2DLibAbilities);stdcall;
begin
  with libabilities do
  begin
    LibFullscreen := false;
    LibWindowed := true;
    LibHardware := true;
    LibSoftware := false;
    LibAntialias := false;
    LibLights := true;
    Lib3D := true;
    LibVSync := false;
  end;
end;

exports
  CreateApplication,
  Andorra2DLibraryInformation,
  Andorra2DLibraryAbilities;

begin
end.

