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
* File: AdBitmapEffects.pas
* Comment: This unit contains effects which may be assigned to TAdBitmaps
}

unit AdBitmapEffects;

interface

uses
  AdTypes, AdBitmapClass;

type
  TAdBitmapEffect = class
    public
      procedure AssignEffect(Dest:TAd2dBitmap);virtual;abstract;
  end;

  TAdBitmapBlur = class(TAdBitmapEffect)
    private
      FRadius: Single;
    public
      property Radius:Single read FRadius write FRadius;
      procedure AssignEffect(Dest:TAd2dBitmap);override;
  end;


implementation

{ TAdBitmapBlur }

procedure TAdBitmapBlur.AssignEffect(Dest: TAd2dBitmap);
begin

end;

end.
