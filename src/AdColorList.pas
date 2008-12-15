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

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$INCLUDE andorra2d.inc}

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
      {Returns a mixed color value. Max represents a maximum value, which pos is relative to.}
      function GetColor(Max,Pos:double):TAndorraColor;
      //Add a color to the lists
      function Add(AColor:TAndorraColor): integer;
      //Inserts a color to the specified position
      procedure Insert(AIndex: integer; AColor: TAndorraColor);
      //Save the color list to the stream
      procedure SaveToStream(AStream:TStream);
      //Load the color list from a stream
      procedure LoadFromStream(AStream:TStream);
  end;

implementation

{ TAdColorList }

function TAdColorList.Add(AColor: TAndorraColor): integer;
var
  temp: PAndorraColor;
begin
  //Reserve memory for a new RGBA-Color variable and assign the given color value to it
  New(temp);
  temp^ := AColor;  
  
  result := inherited Add(temp);
end;

procedure TAdColorList.Insert(AIndex: integer; AColor: TAndorraColor);
var
  temp: pAndorraColor;
begin
  //Match AIndex to the list bounds
  if AIndex < 0 then
    AIndex := 0;

  if AIndex > Count then
    AIndex := Count;
    
  //Reserve memory for a new RGBA-Color variable and assign the given color value to it
  new(temp);
  temp^ := AColor;
  
  inherited Insert(AIndex, temp);
end;

function TAdColorList.GetColor(Max, Pos: double): TAndorraColor;

  //Returns the color between two given RGBA colors. 
  //!May be replaced by the interpolator classes used in my game CrashPoint/the PerlinNoise algorithm
  function ColorBetween(C1, C2 : TAndorraColor; blend:Double):TAndorraColor; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
  begin
     result.r := Round(C1.r + (C2.r-C1.r) * blend);
     result.g := Round(C1.g + (C2.g-C1.g) * blend);
     result.b := Round(C1.b + (C2.b-C1.b) * blend);
     result.a := Round(C1.a + (C2.a-C1.a) * blend);
  end;

var v1,v2:integer;
    v:single;
begin
  //Deal with the case that pos is greater than max
  if pos > max then
  begin
    //Return the last color in the list
    result := Items[count-1];
  end
  else
  begin
    if count > 0 then
    begin
      if (count > 1) and (pos > 0) then
      begin
        //Calculate the list entry...        
        //! Optimize!
        v := 1 / (max / ((count - 1) * pos));

        //Check whether the calculated list entry is in the lists bounds
        if v > (count-1) then
          v := (count-1);
          
        //!?
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
var 
  c, i: integer;
  tmp: TAndorraColor;
begin
  //Clear all items in that are currently in the list
  Clear;
  
  //Read the itemcount that was written to the list
  //! Probably the stream content should be verified! Might be a possible security 
  //problem. Use XML in network applications for single point of failure.
  AStream.Read(c, sizeof(c));
  
  for i := 0 to c-1 do
  begin
    //Read a single color and add it to the list
    AStream.Read(tmp, SizeOf(tmp));
    Add(tmp);
  end;
end;

procedure TAdColorList.SaveToStream(AStream: TStream);
var 
  i, c:integer;
  tmp:TAndorraColor;
begin
  //Save the count of list items to the stream
  c := Count;  
  AStream.Write(c, SizeOf(c));  
  
  //And save every single color to the stream now.
  for i := 0 to Count - 1 do
  begin
    tmp := Items[i];
    AStream.Write(tmp, SizeOf(TAndorraColor));
  end;
end;

procedure TAdColorList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  //Free the list entry if a item is deleted.
  if Action = lnDeleted then
    Dispose(PAndorraColor(Ptr));
    
  Inherited;
end;        

procedure TAdColorList.SetItem(AIndex:integer;AItem:TAndorraColor);
begin
  PAndorraColor(inherited Items[AIndex])^ := AItem;
end;

end.
