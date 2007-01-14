{
* This program is licensed under the to Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more informations
*
* Author: Andreas Stöckel
* Filename: AndorraDll.dpr
}

library AndorraDX93D;

uses
  SysUtils,
  Classes,
  DllMain in 'DllMain.pas',

{$E .dll}

{$R *.res}

function CreateApplication:TAd2DApplication;stdcall;
begin
  //Returns an instance of TXXApplication.
  result := TXXApplication.Create;
end;

exports
  CreateApplication;

begin
end.

