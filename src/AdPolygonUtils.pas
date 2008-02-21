{
* This program is licensed under the Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more informations.
* 
* Inspite of the incompatibility between the Common Public License (CPL) and the GNU General Public License (GPL) you're allowed to use this program * under the GPL. 
* You also should have recieved a copy of this license with this file. 
* If not, see http://www.gnu.org/licenses/gpl.txt for more informations.
*
* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: AdPolygonUtils.pas
* Comment: Contains a simple function to triangulate a given polygon
}

{ Contains a simple function to triangulate a given polygon }
unit AdPolygonUtils;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, AdTypes;

{Returns whether a point lies inside a triangle. ap1 specifies the point,
 tp1, tp2 and tp3 the triangle points.}
function PointInTriangle(const ap1, tp1, tp2, tp3 : TAdPoint): boolean;
{Triangulates the given polygon. The polygon must not contain any intersections or holes/islands.}
function Triangulate(const APolygon: TAdPolygon; var ATriangles: TAdTriangles):boolean;
{Returns true if the polygon is in clockwise orientation, false if it is counter-clockwise}
function PolygonIsClockwise(const APolygon : TAdPolygon):boolean;

implementation

//This code was written by Andreas Stöckel in February 2008. You can use this code for any purpose,
//as long as you do not remove this credits.
//Sources:
//http://www.geometrictools.com/Documentation/TriangulationByEarClipping.pdf
//http://www.iti.fh-flensburg.de/lang/algorithmen/geo/polygon.htm
//http://nuttybar.drama.uga.edu/pipermail/dirgames-l/2003-December/027342.html

function PolygonIsClockwise(const APolygon : TAdPolygon):boolean;
begin
  //result := APolygon[0].X < APolygon[1].X;
  result := false;
end;

//Überprüft ob ein Punkt in einem Dreieck liegt
function PointInTriangle(const ap1, tp1, tp2, tp3 : TAdPoint): boolean;
var
  b0, b1, b2, b3: Double;
begin
  b0 := ((tp2.x - tp1.x) * (tp3.y - tp1.y) - (tp3.x - tp1.x) * (tp2.y - tp1.y));
  if b0 <> 0 then
  begin
    b1 := (((tp2.x - ap1.x) * (tp3.y - ap1.y) - (tp3.x - ap1.x) * (tp2.y - ap1.y)) / b0);
    b2 := (((tp3.x - ap1.x) * (tp1.y - ap1.y) - (tp1.x - ap1.x) * (tp3.y - ap1.y)) / b0);
    b3 := 1 - b1 - b2;

    result := (b1 > 0) and (b2 > 0) and (b3 > 0);
  end else
    result := false;
end;

function Triangulate(const APolygon: TAdPolygon; var ATriangles: TAdTriangles):boolean;
var
  lst:TList;
  i, j:integer;
  p, p1, p2, pt:PAdPoint;
  l:double;
  intriangle : boolean;
  lastear :  integer;

  //Berechnet aus einem Index, der auch die Listen-Grenzen über- oder unterschreiten
  //kann einen validen Listenindex.
  function GetItem(const ai, amax:integer):integer;
  begin
    result := ai mod amax;
    if result < 0 then
      result := amax + result;
  end;

begin
  lst := TList.Create;

  //Kopiere die Punkte des Polygons in eine TList (also eine Vektordatenstruktur)
  for i := 0 to High(APolygon) do
  begin
    New(p);
    p^.X := APolygon[i].X;
    p^.Y := APolygon[i].Y;
    lst.Add(p);
  end;

  i := 0;
  lastear := -1;
  repeat
    lastear := lastear + 1;

    //Suche drei benachbarte Punkte aus der Liste
    p1 := lst.Items[GetItem(i - 1, lst.Count)];
    p  := lst.Items[GetItem(i, lst.Count)];
    p2 := lst.Items[GetItem(i + 1, lst.Count)];


    //Berechne, ob die Ecke konvex oder konkav ist
    l := ((p1.X - p.X) * (p2.Y - p.Y) - (p1.Y - p.Y) * (p2.X - p.X));

    //Nur weitermachen, wenn die Ecke konkav ist
    if l < 0 then
    begin
      //Überprüfe ob irgendein anderer Punkt aus dem Polygon
      //das ausgewählte Dreieck schneidet
      intriangle := false;
      for j := 2 to lst.Count - 2 do
      begin
        pt := lst.Items[GetItem(i + j, lst.Count)];
        if PointInTriangle(pt^, p1^, p^, p2^) then
        begin
          intriangle := true;
          break;
        end;
      end;
     
      //Ist dies nicht der Fall, so entferne die ausgwewählte Ecke und bilde
      //ein neues Dreieck
      if not intriangle then
      begin
        SetLength(ATriangles, Length(ATriangles) + 1);
        ATriangles[High(ATriangles)][0] := AdPoint(p1^.X, p1^.Y);
        ATriangles[High(ATriangles)][1] := AdPoint(p^.X, p^.Y);
        ATriangles[High(ATriangles)][2] := AdPoint(p2^.X, p2^.Y);

        lst.Delete(GetItem(i, lst.Count));
        Dispose(p);

        lastear := 0;

        i := i-1;
      end;
    end;

    i := i + 1;
    if i > lst.Count - 1 then
      i := 0;

  //Abbrechen, wenn nach zwei ganzen Durchläufen keine Ecke gefunden wurde, oder nur noch
  //drei Ecken übrig sind.
  until (lastear > lst.Count*2) or (lst.Count = 3);

  if lst.Count = 3 then
  begin
    p1 := lst.Items[GetItem(0, lst.Count)];
    p  := lst.Items[GetItem(1, lst.Count)];
    p2 := lst.Items[GetItem(2, lst.Count)];
    SetLength(ATriangles, Length(ATriangles) + 1);
    ATriangles[High(ATriangles)][0] := AdPoint(p1^.X, p1^.Y);
    ATriangles[High(ATriangles)][1] := AdPoint(p^.X, p^.Y);
    ATriangles[High(ATriangles)][2] := AdPoint(p2^.X, p2^.Y);
  end;

  result := lst.Count = 3;

  for i := 0 to lst.Count - 1 do
  begin
    Dispose(PAdPoint(lst.Items[i]));
  end;
  lst.Clear;
  lst.Free;
end;

end.
