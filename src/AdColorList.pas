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
* File: AdColorList.pas
* Comment: Contains a simple list that is capable of doing color interpolations
}
{Contains a simple list that is capable of doing color interpolations.}
unit AdColorList;

interface

uses
  Classes, AdList, AdClasses, AdTypes;

type
  {Contains a variable count of color values and returns an interpolated color
   between two of them. This class is used in the particle system.}
  TAdColorList = class(TAdList)
    private
    	function GetItem(AIndex:integer):TAndorraColor;
    	procedure SetItem(AIndex:integer;AItem:TAndorraColor);
    protected
      procedure Notify(Ptr: Pointer; Action: TListNotification);override;
    public
      //The "items" property of a list
    	property Items[AIndex:integer]:TAndorraColor read GetItem write SetItem;default;
      //Returns a mixed color value
      function GetColor(Max,Pos:double):TAndorraColor;
      //Add a color
      procedure Add(AColor:TAndorraColor);
      //Save the color list to the stream
      procedure SaveToStream(AStream:TStream);
      //Load the color list from a stream
      procedure LoadFromStream(AStream:TStream);
  end;

implementation

{ TAdColorList }

procedure TAdColorList.Add(AColor: TAndorraColor);
var temp:pAndorraColor;
begin
  new(temp);
  temp^ := AColor;
  inherited Add(temp);
end;

function TAdColorList.GetColor(Max, Pos: double): TAndorraColor;

  function ColorBetween(C1, C2 : TAndorraColor; blend:Double):TAndorraColor; inline;
  begin
     result.r := Round(C1.r + (C2.r-C1.r) * blend);
     result.g := Round(C1.g + (C2.g-C1.g) * blend);
     result.b := Round(C1.b + (C2.b-C1.b) * blend);
     result.a := Round(C1.a + (C2.a-C1.a) * blend);
  end;

var v1,v2:integer;
    v:single;
begin
  if pos > max then
  begin
    result := Items[count-1];
  end
  else
  begin
    if count > 0 then
    begin
      if (count > 1) and (pos > 0) then
      begin
        v := 1/(max/((count-1)*pos));
        if v > (count-1) then
        begin
          v := (count-1)
        end;
        if trunc(v) <> v then
        begin
          v1 := trunc(v);
          v2 := v1+1;
          result := ColorBetween(Items[v1],Items[v2],v-trunc(v));
        end
        else
        begin
          result := Items[round(v)];
        end;
      end
      else
      begin
        result := Items[0];
      end;
    end;
  end;
end;

function TAdColorList.GetItem(AIndex:integer):TAndorraColor;
begin
  result := TAndorraColor(inherited Items[AIndex]^);
end;

procedure TAdColorList.LoadFromStream(AStream: TStream);
var c,i:integer;
    tmp:TAndorraColor;
begin
  Clear;
  AStream.Read(c,sizeof(c));
  for i := 0 to c-1 do
  begin
    AStream.Read(tmp,SizeOf(tmp));
    Add(tmp);
  end;
end;

procedure TAdColorList.SaveToStream(AStream: TStream);
var i:integer;
    tmp:TAndorraColor;
begin
  i := Count;
  AStream.Write(i,sizeof(i));
  for i := 0 to Count - 1 do
  begin
    tmp := Items[i];
    AStream.Write(tmp,SizeOf(TAndorraColor))
  end;
end;

procedure TAdColorList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if ( Action = lnDeleted ) then
  begin
    Dispose(PAndorraColor(Ptr));
  end;
  Inherited;
end;        

procedure TAdColorList.SetItem(AIndex:integer;AItem:TAndorraColor);
begin
  PAndorraColor(inherited Items[AIndex])^ := AItem;
end;

end.
