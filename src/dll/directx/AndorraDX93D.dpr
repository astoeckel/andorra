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

uses
  SysUtils, AdClasses,
  DX3DMain in 'DX3DMain.pas';

{$E .dll}

{$R *.res}

function CreateApplication:TAd2DApplication;stdcall;
begin
  result := TDXApplication.Create;
end;

procedure Andorra2DLibraryInformation(var libinfo:TAd2DLibInfo);stdcall;
begin
  with libinfo do
  begin
    LibTitle := 'Andorra DirectX9 Plugin';
    LibAuthor := '(c) by Andreas Stöckel 2007';
    LibDescription := 'This plugin wraps around Direct3D 9.';
    LibVersion := LibraryVersion;
    LibImage := 'dx93d.png';
  end;
end;

procedure Andorra2DLibraryAbilities(var libabilities:TAd2DLibAbilities);stdcall;
begin
  with libabilities do
  begin
    LibFullscreen := true;
    LibWindowed := true;
    LibHardware := true;
    LibSoftware := false;
    LibAntialias := true;
    LibLights := true;
    Lib3D := true;
    LibVSync := true;
  end;
end;

exports
  CreateApplication,
  Andorra2DLibraryInformation,
  Andorra2DLibraryAbilities;

begin
end.
