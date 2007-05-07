{
* This program is licensed under the Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more informations.
* 
* Inspite of the incompatibility between the Common Public License (CPL) and the GNU General Public License (GPL) you're allowed to use this program * under the GPL. 
* You also should have recieved a copy of this license with this file. 
* If not, see http://www.gnu.org/licenses/gpl.txt for more informations.

* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: AdXML.pas
* Comment: Contains a few functions, which make it easier to work with the JvSimpleXML.pas
}

unit AdXML;

interface

uses JvSimpleXML, Classes, AdDraws, AdClasses, SysUtils;

function WriteStreamToString(AStream:TStream):string;
procedure ReadStreamFromString(AStream:TStream;AString:string);
procedure WriteStream(AStream:TStream;XMLElem:TJvSimpleXMLElem;ElemName:string='data');
procedure ReadStream(AStream:TStream;XMLElem:TJvSimpleXMLElem;ElemName:string='data');

implementation

function WriteStreamToString(AStream:TStream):string;
var
  i:integer;
  b:byte;
begin
  result := '';
  for i := AStream.Position to AStream.Size-1 do
  begin
    AStream.Read(b,1);
    result := result + inttohex(b,2);
  end;
end;

procedure ReadStreamFromString(AStream:TStream;AString:string);
var
  i:integer;
  b:byte;
begin
  for i := 1 to length(AString) div 2 do
  begin
    b := strtoint('$'+copy(AString,((i-1)*2)+1,2));
    AStream.Write(b,1);
  end;
end;

procedure WriteStream(AStream:TStream;XMLElem:TJvSimpleXMLElem;ElemName:string='data');
begin
  AStream.Position := 0;
  XMLElem.Properties.Add('data',WriteStreamToString(AStream));
end;

procedure ReadStream(AStream:TStream;XMLElem:TJvSimpleXMLElem;ElemName:string='data');
var
  i:integer;
  b:byte;
  ms:TMemoryStream;
begin
  if XMLElem.Properties.Value('source','') <> '' then
  begin
    ms := TMemoryStream.Create;
    ms.LoadFromFile(XMLElem.Properties.Value('source',''));
    ms.Position := 0;
    AStream.CopyFrom(ms,ms.Size);    
    ms.Free;
  end
  else
  begin
    ReadStreamFromString(AStream, XMLElem.Properties.Value('data',''));
  end;
end;

end.
