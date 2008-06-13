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
* File: AdLog.pas
* Comment: A unit which contains the Andorra 2D log system.
}

{A unit which contains the Andorra 2D log system.}
unit AdLog;

interface

uses
  Classes;

type
  {A record type containing information about a log entry in the log system.}
  TAdLogMessage = record
    Text:string;//< the text of the message
    Sender:string;//< the sender of the message. (May be the plugin or something else.)
    Typ:string;//< the typ of the message
  end;

  //The log system class
  TAdLog = class
    private
      FItems:TStringList;
      FAutoSave: boolean;
      FFileName: string;
    public
      {Creates an instance of TAdLog.}
      constructor Create;
      {Destroys the instance of TAdLog.}
      destructor Destroy;override;
      {Loads the log file from a file.}
      procedure LoadFromFile(AFile:string);
      {Stores the log file in a file.}
      procedure SaveToFile(AFile:string);
     
      {Adds a new message to the log file.
       @seealso(TAdLogMessage)}
      procedure AddMessage(AMessage:TAdLogMessage);virtual;

      {Allows access on the string list managed by TAdLog.}
      property Items:TStringList read FItems;
      {When true, all changes on the log file are automatically saved.}
      property AutoSave: boolean read FAutoSave write FAutoSave;
      {The file the log should be saved to.}
      property FileName: string read FFileName write FFileName;
  end;

implementation

{ TAdLog }

constructor TAdLog.Create;
begin
  inherited Create;

  FItems := TStringList.Create;
  FAutoSave := false;
  FFileName := 'adlog.txt';
end;

destructor TAdLog.Destroy;
begin
  Items.Free;
  inherited Destroy;
end;

procedure TAdLog.AddMessage(AMessage: TAdLogMessage);
const
  cTabulator = 20;
var
  Space:integer;
  LMessage:string;
  i:integer;
begin
  LMessage := '[' + AMessage.Typ + ']';

  Space := cTabulator - length(AMessage.Typ) - 2;
  if Space < 1 then Space := 1;
  for i := 0 to Space do
    LMessage := LMessage + ' ';


  LMessage := LMessage + AMessage.Sender + ':';

  Space := cTabulator - length(AMessage.Sender) - 1;
  if Space < 1 then Space := 1;
  for i := 0 to Space do
    LMessage := LMessage + ' ';


  LMessage := LMessage + AMessage.Text;

  self.Items.Add(LMessage);
  if AutoSave and (FileName <> '') then
  begin
    try
      SaveToFile(FileName);
    except
      AutoSave := false;
    end;
  end;
end;

procedure TAdLog.LoadFromFile(AFile: string);
begin
  Items.LoadFromFile(AFile);
  Items.Add('');
end;

procedure TAdLog.SaveToFile(AFile: string);
begin
  Items.SaveToFile(AFile);
end;

end.
