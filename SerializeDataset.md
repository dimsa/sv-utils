# Dataset serialization #

TSvSerializer can save many types, including TDataset descendants.
Suppose we have declared our class as shown below:
```
TJQGridData = class
  private
    FTotalPages: Integer;
    FCurrPage: Integer;
    FTotalRecords: Integer;
    FInvData: TClientDataset;
  public
    constructor Create(); virtual;
    destructor Destroy; override;

    procedure FillData(); //fills data with some values
    procedure ClearData(); //clears dataset records
    function ToJSON(): string; 
    procedure FromJSON(const AJSONString: string);


    [SvSerialize('totalpages')]
    property TotalPages: Integer read FTotalPages write FTotalPages;
    [SvSerialize('currpage')]
    property CurrentPage: Integer read FCurrPage write FCurrPage;
    [SvSerialize('totalrecords')]
    property TotalRecords: Integer read FTotalRecords write FTotalRecords;
    [SvSerialize('invdata')]
    property InvData: TClientDataset read FInvData write FInvData;
  end;
```


And our implementation section:
```
{ TJQGridData }

procedure TJQGridData.ClearData;
begin
  FInvData.First;
  while not FInvData.Eof do
  begin
    FInvData.Delete;
  end;
end;

constructor TJQGridData.Create;
begin
  inherited Create();
  FInvData := TClientDataSet.Create(nil);
  FInvData.FieldDefs.Add('invid', ftInteger);
  FInvData.FieldDefs.Add('invdate',ftDateTime);
  FInvData.FieldDefs.Add('amount',ftFloat);
  FInvData.FieldDefs.Add('tax',ftInteger);
  FInvData.FieldDefs.Add('total',ftFloat);
  FInvData.FieldDefs.Add('note',ftWideString, 255);
  FInvData.CreateDataSet;

  FTotalPages := 2;
  FCurrPage := 1;
  FTotalRecords := 10;
end;

destructor TJQGridData.Destroy;
begin
  FInvData.Free;
  inherited Destroy;
end;

procedure TJQGridData.FillData;
var
  i: Integer;
begin
  for i := 1 to 10 do
  begin
    FInvData.Append;

    FInvData.FieldByName('invid').AsInteger := i;
    FInvData.FieldByName('invdate').AsDateTime := Today;
    FInvData.FieldByName('amount').AsFloat := i * 100;
    FInvData.FieldByName('tax').AsInteger := i + 30;
    FInvData.FieldByName('total').AsFloat := i * 101;
    FInvData.FieldByName('note').AsString := PROP_STRING;

    FInvData.Post;
  end;
end;

procedure TJQGridData.FromJSON(const AJSONString: string);
var
  FSer: TSvSerializer;
begin
  FSer := TSvSerializer.Create();
  try
    //add our object without name. In this case the root node will be unnamed
    FSer.AddObject('', Self);
    //Load our object properties from the JSON string
    FSer.DeSerialize(AJSONString, TEncoding.UTF8);
  finally
    FSer.Free;
  end;
end;

function TJQGridData.ToJSON: string;
var
  FSer: TSvSerializer;
begin
  Result := '';
  FSer := TSvSerializer.Create();
  try
    //add our object without name. In this case the root node will be unnamed
    FSer.AddObject('', Self);
    //Serialize our object properties into JSON string
    FSer.Serialize(Result, TEncoding.UTF8);
  finally
    FSer.Free;
  end;
end;
```

And the possible usage of our class could be:
```
procedure TestTSvJsonSerializerFactory.TestJQGrid;
var
  obj: TJQGridData;
  sJson: string;
begin
  obj := TJQGridData.Create;
  try
    obj.FillData;
    //sJson is json string of our object
    sJson := obj.ToJSON;
    //clear the dataset records
    obj.ClearData;
    //load properties from json string
    obj.FromJSON(sJson);
    //now our object is the same as we had before
  finally
    obj.Free;
  end;
end;
```

Generated JSON (this format is fully compatible with jqGrid):
```
{"totalpages":2,"currpage":1,"totalrecords":10,"invdata":[{"invid":"1","invdate":"2011-12-12","amount":"100","tax":"31","total":"101","note":"Some unicode Portugu\u00EAs \" \u0420\u0443\u0441\u0441\u043A\u0438\u0439\/ \u0395\u03BB\u03BB\u03B7\u03BD\u03B9\u03BA\u03AC"},{"invid":"2","invdate":"2011-12-12","amount":"200","tax":"32","total":"202","note":"Some unicode Portugu\u00EAs \" \u0420\u0443\u0441\u0441\u043A\u0438\u0439\/ \u0395\u03BB\u03BB\u03B7\u03BD\u03B9\u03BA\u03AC"},{"invid":"3","invdate":"2011-12-12","amount":"300","tax":"33","total":"303","note":"Some unicode Portugu\u00EAs \" \u0420\u0443\u0441\u0441\u043A\u0438\u0439\/ \u0395\u03BB\u03BB\u03B7\u03BD\u03B9\u03BA\u03AC"},{"invid":"4","invdate":"2011-12-12","amount":"400","tax":"34","total":"404","note":"Some unicode Portugu\u00EAs \" \u0420\u0443\u0441\u0441\u043A\u0438\u0439\/ \u0395\u03BB\u03BB\u03B7\u03BD\u03B9\u03BA\u03AC"},{"invid":"5","invdate":"2011-12-12","amount":"500","tax":"35","total":"505","note":"Some unicode Portugu\u00EAs \" \u0420\u0443\u0441\u0441\u043A\u0438\u0439\/ \u0395\u03BB\u03BB\u03B7\u03BD\u03B9\u03BA\u03AC"},{"invid":"6","invdate":"2011-12-12","amount":"600","tax":"36","total":"606","note":"Some unicode Portugu\u00EAs \" \u0420\u0443\u0441\u0441\u043A\u0438\u0439\/ \u0395\u03BB\u03BB\u03B7\u03BD\u03B9\u03BA\u03AC"},{"invid":"7","invdate":"2011-12-12","amount":"700","tax":"37","total":"707","note":"Some unicode Portugu\u00EAs \" \u0420\u0443\u0441\u0441\u043A\u0438\u0439\/ \u0395\u03BB\u03BB\u03B7\u03BD\u03B9\u03BA\u03AC"},{"invid":"8","invdate":"2011-12-12","amount":"800","tax":"38","total":"808","note":"Some unicode Portugu\u00EAs \" \u0420\u0443\u0441\u0441\u043A\u0438\u0439\/ \u0395\u03BB\u03BB\u03B7\u03BD\u03B9\u03BA\u03AC"},{"invid":"9","invdate":"2011-12-12","amount":"900","tax":"39","total":"909","note":"Some unicode Portugu\u00EAs \" \u0420\u0443\u0441\u0441\u043A\u0438\u0439\/ \u0395\u03BB\u03BB\u03B7\u03BD\u03B9\u03BA\u03AC"},{"invid":"10","invdate":"2011-12-12","amount":"1000","tax":"40","total":"1010","note":"Some unicode Portugu\u00EAs \" \u0420\u0443\u0441\u0441\u043A\u0438\u0439\/ \u0395\u03BB\u03BB\u03B7\u03BD\u03B9\u03BA\u03AC"}]}
```