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
program VideoPlayer;

//Set a icon for the application when using windows
{$IFDEF WIN32}
  {$R '..\..\icon.res' '..\..\icon.rc'}
{$ENDIF}

uses
  Main in 'Main.pas';

var
  Appl: TAdAppl;

begin
  Appl := TAdAppl.Create;
  Appl.Run;
  Appl.Free;
end.
