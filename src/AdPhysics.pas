{
* This program is licensed under the to Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more informations
*
* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: AdPhysics.pas
* Comment: Contains a link to newton
* To use the AdPhysics unit you'll need the Newton headers for Delphi from
*    http://newton.delphigl.de/
* and a version of Newton itsself from
*    http://www.newtongamedynamics.com/
* where you have to download the SDK for your platform and copy the .dll/.so/.dynlib to your Andorra 2D directory.
}

unit AdPhysics;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses AdClasses, AdDraws, AdSprites, NewtonImport;

type
  TPhysicalApplication = class(TSprite)
    private
    protected
      procedure DoMove(timegap:double);override;
    public
      NewtonWorld:PNewtonWorld;

      constructor Create(AParent:TSprite);override;
      destructor Destroy;override;
  end;

  TPhysicalSprite = class(TImageSpriteEx)
    private
    protected
      Physics:TPhysicalApplication;
    public

      constructor Create(AParent:TSprite);override;
      destructor Destroy;override;
  end;

implementation

{ TPhysicalSprite }

constructor TPhysicalSprite.Create(AParent: TSprite);
begin
  inherited;

end;

destructor TPhysicalSprite.Destroy;
begin

  inherited;
end;

{ TPhysicalApplication }

constructor TPhysicalApplication.Create(AParent: TSprite);
begin
  inherited;
  NewtonWorld := NewtonCreate(nil,nil);
end;

destructor TPhysicalApplication.Destroy;
begin
  NewtonDestroy(NewtonWorld);
  inherited;
end;

procedure TPhysicalApplication.DoMove(timegap: double);
begin
  NewtonUpdate(NewtonWorld, timegap);
end;

end.
