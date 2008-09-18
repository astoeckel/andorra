unit AdStrUtils;

interface

uses
  SysUtils;

type
  TAdStringArray = array of string;

procedure DevideString(AString:string; AChar:Char; var AResult:TAdStringArray);

implementation

procedure DevideString(AString:string; AChar:Char; var AResult:TAdStringArray);
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
