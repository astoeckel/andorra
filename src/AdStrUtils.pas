{
* This program is licensed under the Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more informations.
* 
* Inspite of the incompatibility between the Common Public License (CPL) and the GNU General Public License (GPL) you're allowed to use this program 
* under the GPL. 
* You also should have recieved a copy of this license with this file. 
* If not, see http://www.gnu.org/licenses/gpl.txt for more informations.
*
* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: AdStrUtils.pas
* Comment: Implements functions for the work with strings.
}

{Implements functions for the work with strings.}
unit AdStrUtils;

interface

uses
  SysUtils;

type
  {A simple array of strings used in the explode function.
   @seealso(Explode)}
  TAdStringArray = array of string;
  
{Seperates a string with the given seperator char and puts all string parts in the AResult array.}
procedure Explode(AString:string; AChar:Char; var AResult:TAdStringArray);

implementation

procedure Explode(AString:string; AChar:Char; var AResult:TAdStringArray);
var
  s:string;
  p:integer;
begin
  SetLength(AResult,0);

  while length(AString) > 0 do
  begin
    //Search the next place where "AChar" apears in "AString"
    p := Pos(AChar,AString);

    if p = 0 then
    begin
      //If not found copy the whole string
      s := copy(AString,1,length(AString));
      AString := '';
    end
    else
    begin
      s := copy(AString,1,p-1);
      AString := copy(AString,p+1,length(AString)-p+1);
    end;

    //Add item
    SetLength(AResult,Length(AResult)+1);
    AResult[high(AResult)] := s;
  end;
end;

end.
