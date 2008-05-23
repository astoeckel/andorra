unit AdLog;

interface

uses
  Classes;

type
  //A record for adding a new log entry into the log system
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
      constructor Create;
      destructor Destroy;override;
      procedure LoadFromFile(AFile:string);
      procedure SaveToFile(AFile:string);
      procedure AddMessage(AMessage:TAdLogMessage);virtual;

      property Items:TStringList read FItems;
      property AutoSave: boolean read FAutoSave write FAutoSave;
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
