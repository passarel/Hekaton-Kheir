unit UnRafaVisual;

//------------------------------------------------------------------------------------------------

interface

uses SysUtils, Classes, ExtCtrls, Graphics, Contnrs, Controls;

//------------------------------------------------------------------------------------------------

type TRafaVisualStatus = (RVSSynapse, RVSMove, RVSFixed);

//------------------------------------------------------------------------------------------------

type TRafaVisualConnection = class
  private
    FSource    : integer;
    FTarget    : integer;
    FColor     : TColor;
    FIsVisible : boolean;

    procedure SetSource(Value : integer);
    procedure SetTarget(Value : integer);
    procedure SetColor (Value : TColor);
    procedure SetVisible(Value : boolean);

    function GetSource : integer;
    function GetTarget : integer;
    function GetColor  : TColor;
    function GetVisible : boolean;

  public
    property Source    : integer read GetSource  write SetSource;
    property Target    : integer read GetTarget  write SetTarget;
    property Color     : TColor  read GetColor   write SetColor;
    property IsVisible : boolean read GetVisible write SetVisible;

    constructor Create;
    destructor  Destroy; override;
  end;

//------------------------------------------------------------------------------------------------

type TRafaVisualItem = class
  private
    FTop    : integer;
    FLeft   : integer;
    FHeight : integer;
    FWidth  : integer;
    FTag    : integer;
    FOrder  : integer;
    FImage  : integer;

    procedure SetTop(Value : integer);
    procedure SetLeft(Value : integer);
//    procedure SetHeight(Value : integer);
//    procedure SetWidth(Value : integer);
    procedure SetTag(Value : integer);
    procedure SetOrder(Value : integer);
    procedure SetCenterX(Value : integer);
    procedure SetCenterY(Value : integer);


    function GetTop     : integer;
    function GetLeft    : integer;
    function GetHeight  : integer;
    function GetWidth   : integer;
    function GetTag     : integer;
    function GetOrder   : integer;
    function GetImage   : integer;
    function GetCenterX : integer;
    function GetCenterY : integer;
    function GetBottom  : integer;
    function GetRight   : integer;

  public
    property Top    : integer read GetTop    write SetTop;
    property Left   : integer read GetLeft   write SetLeft;
    property Height : integer read GetHeight; // write SetHeight;
    property Width  : integer read GetWidth;  // write SetWidth;
    property Tag    : integer read GetTag    write SetTag;
    property Order  : integer read GetOrder  write SetOrder;
    property Image  : integer read GetImage;

    property CenterX : integer read GetCenterX write SetCenterX;
    property CenterY : integer read GetCenterY write SetCenterY;
    property Bottom  : integer read GetBottom;
    property Right   : integer read GetRight;

    constructor Create(Order, ImageIndex, ImageHeight, ImageWidth : integer);
    destructor  Destroy; override;

    procedure  SetImage(ImageIndex, ImageHeight, ImageWidth : integer);
    function   ContainPoint(x, y : integer) : boolean;
  end;

//------------------------------------------------------------------------------------------------

type TRafaVisualEvent = procedure(item : integer) of object;

//------------------------------------------------------------------------------------------------

type TRafaVisualConteiner = class
  private
    FBackground : TImage;
    FItems : TObjectList;
    FConnections : TObjectList;
    FIsMouseDown : boolean;
    FSelectedItem : integer;
    FStatus : TRafaVisualStatus;
    FOnItemEvent : TRafaVisualEvent;
    FOnConnectionEvent : TRafaVisualEvent;
    FImages  : TObjectList;
    FRelativePositionX : integer;
    FRelativePositionY : integer;

    FTmpSource : integer;

    function GetItem(index : integer) : TRafaVisualItem;
    function GetIndexAtPosition(x, y : integer) : integer;
    function GetIndexByTag(tag : integer) : integer;
    function GetItemAtPosition(x, y : integer) : TRafaVisualItem;
    function GetItemByTag(tag : integer) : TRafaVisualItem;
    function GetBackground : TImage;
    function GetSelectedItem : integer;

    function GetConnection(index : integer) : TRafaVisualConnection;
    function GetImage(index : integer) : TBitmap;

    procedure SetOnItemEvent(event : TRafaVisualEvent);
    function  GetOnItemEvent : TRafaVisualEvent;
    procedure SetOnConnectionEvent(event : TRafaVisualEvent);
    function  GetOnConnectionEvent : TRafaVisualEvent;
    procedure SetSelectedItem(value : integer);

    procedure BgClick(Sender: TObject);
    procedure BgDblClick(Sender: TObject);
    procedure BgMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure BgMouseUp(Sender: TObject; Button: TMouseButton;  Shift: TShiftState; X, Y: Integer);

  public
    property Background : TImage read GetBackground;
    property OnItemEvent : TRafaVisualEvent read GetOnItemEvent write SetOnItemEvent;
    property OnConnectionEvent : TRafaVisualEvent read GetOnConnectionEvent write SetOnConnectionEvent;
    property SelectedItem : integer read GetSelectedItem write SetSelectedItem;

    constructor Create(Background : TImage);
    destructor  Destroy; override;

    function  AddItem(image : integer; tag : integer = -1) : integer;
    procedure DeleteItem(index : integer);
    procedure DeleteItemByTag(tag : integer);
    property  Item[index : integer] : TRafaVisualItem read GetItem;
    property  ItemByTag[tag : integer] : TRafaVisualItem read GetItem;

    function  AddImage(FileName : TFileName) : integer;
    property  Image[index : integer] : TBitmap read GetImage;

    function  AddConnection(Source, Target : integer; Color : TColor = ClBlack) : integer;
    function  GetConnectionIndex(Source, Target : integer) : integer;
    procedure DeleteConnection(index : integer);
    property  Connection[index : integer] : TRafaVisualConnection read GetConnection;

    procedure ClearConnections;
    procedure ClearItems;
    procedure ClearImages;

    procedure Repaint;

  end;

//------------------------------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------------------------

{ TRafaVisualConnection }

//------------------------------------------------------------------------------------------------

constructor TRafaVisualConnection.Create;
  begin
  inherited Create;
  FSource := -1;
  FTarget := -1;
  FColor := ClBlack;
  end;

//------------------------------------------------------------------------------------------------

destructor  TRafaVisualConnection.Destroy;
  begin
  inherited Destroy;
  end;

//------------------------------------------------------------------------------------------------

function TRafaVisualConnection.GetColor  : TColor;
  begin
  result := FColor;
  end;

//------------------------------------------------------------------------------------------------

function TRafaVisualConnection.GetSource : integer;
  begin
  result := FSource;
  end;

//------------------------------------------------------------------------------------------------

function TRafaVisualConnection.GetTarget : integer;
  begin
  result := FTarget;
  end;

//------------------------------------------------------------------------------------------------

function TRafaVisualConnection.GetVisible : boolean;
  begin
  result := FIsVisible;
  end;

//------------------------------------------------------------------------------------------------

procedure TRafaVisualConnection.SetColor (Value : TColor);
  begin
  FColor := Value;
  end;

//------------------------------------------------------------------------------------------------

procedure TRafaVisualConnection.SetSource(Value : integer);
  begin
  if Value < 0 then
     FSource := -1
  else
     FSource := Value;
  end;

//------------------------------------------------------------------------------------------------

procedure TRafaVisualConnection.SetTarget(Value : integer);
  begin
  if Value < 0 then
     FTarget := -1
  else
     FTarget := Value;
  end;

//------------------------------------------------------------------------------------------------

procedure TRafaVisualConnection.SetVisible(Value : boolean);
  begin
  FIsVisible := Value;
  end;

//------------------------------------------------------------------------------------------------

{ TRafaVisualItem }

//------------------------------------------------------------------------------------------------

function   TRafaVisualItem.ContainPoint(x, y : integer) : boolean;
  var b : boolean;
  begin
  b := (x >= FLeft);
  b := b and (x < (FLeft + FWidth));
  b := b and (y >= FTop);
  b := b and (y < (FTop + FHeight));
  result := b;
  end;

//------------------------------------------------------------------------------------------------

constructor TRafaVisualItem.Create(Order, ImageIndex, ImageHeight, ImageWidth : integer);
  begin
  inherited Create;
  FTop    := 0;
  FLeft   := 0;
  FHeight := ImageHeight;
  FWidth  := ImageWidth;
  FTag    := -1;
  FOrder  := Order;
  FImage  := ImageIndex;
  end;

//------------------------------------------------------------------------------------------------

destructor  TRafaVisualItem.Destroy;
  begin
  inherited;
  end;

//------------------------------------------------------------------------------------------------

function TRafaVisualItem.GetBottom  : integer;
  begin
  result := FTop + FHeight;
  end;

//------------------------------------------------------------------------------------------------

function TRafaVisualItem.GetCenterX : integer;
  begin
  result := FLeft + (FWidth div 2);
  end;

//------------------------------------------------------------------------------------------------

function TRafaVisualItem.GetCenterY : integer;
  begin
  result := FTop + (FHeight div 2);
  end;

//------------------------------------------------------------------------------------------------

function  TRafaVisualItem.GetHeight : integer;
  begin
  result := FHeight;
  end;

//------------------------------------------------------------------------------------------------

function  TRafaVisualItem.GetImage  : integer;
  begin
  result := FImage;
  end;

//------------------------------------------------------------------------------------------------

function  TRafaVisualItem.GetLeft   : integer;
  begin
  result := FLeft;
  end;

//------------------------------------------------------------------------------------------------

function  TRafaVisualItem.GetOrder  : integer;
  begin
  result := FOrder;
  end;

//------------------------------------------------------------------------------------------------

function  TRafaVisualItem.GetTag    : integer;
  begin
  result := FTag;
  end;

//------------------------------------------------------------------------------------------------

function  TRafaVisualItem.GetTop    : integer;
  begin
  result := FTop;
  end;

//------------------------------------------------------------------------------------------------

function  TRafaVisualItem.GetWidth  : integer;
  begin
  result := FWidth;
  end;

//------------------------------------------------------------------------------------------------

function TRafaVisualItem.GetRight : integer;
  begin
  result := FLeft + FWidth;
  end;

//------------------------------------------------------------------------------------------------

procedure TRafaVisualItem.SetCenterX(Value : integer);
  begin
  FLeft := value - (FWidth div 2);
  end;
//------------------------------------------------------------------------------------------------

procedure TRafaVisualItem.SetCenterY(Value : integer);
  begin
  FTop := value - (FHeight div 2);
  end;

//------------------------------------------------------------------------------------------------


procedure TRafaVisualItem.SetImage(ImageIndex, ImageHeight, ImageWidth : integer);
  begin
  FImage := ImageIndex;
  FHeight := ImageHeight;
  FWidth := ImageWidth;
  end;

//------------------------------------------------------------------------------------------------

procedure TRafaVisualItem.SetLeft(Value : integer);
  begin
  FLeft := Value;
  end;

//------------------------------------------------------------------------------------------------

procedure TRafaVisualItem.SetOrder(Value : integer);
  begin
  FOrder := Value;
  end;

//------------------------------------------------------------------------------------------------

procedure TRafaVisualItem.SetTag(Value : integer);
  begin
  FTag := Value;
  end;

//------------------------------------------------------------------------------------------------

procedure TRafaVisualItem.SetTop(Value : integer);
  begin
  FTop := Value;
  end;

//------------------------------------------------------------------------------------------------

{ TRafaVisualConteiner }

//------------------------------------------------------------------------------------------------

function  TRafaVisualConteiner.AddConnection(Source, Target : integer; Color : TColor = ClBlack) : integer;
  var XConnection : TRafaVisualConnection;
  begin
  if (source >= 0) and (source < FItems.Count) and (target >= 0) and (target < FItems.Count) then
     begin
     XConnection := TRafaVisualConnection.Create;
     XConnection.Source := Source;
     XConnection.Target := Target;
     XConnection.Color  := Color;
     FConnections.Add(XConnection);
     FOnConnectionEvent(FConnections.Count - 1);
     result := FConnections.Count - 1;
     end
  else
     result := -1;
  end;

//------------------------------------------------------------------------------------------------

function TRafaVisualConteiner.AddImage(FileName : TFileName) : integer;
  var BMP : TBitmap;
  begin
  try
    BMP := TBitmap.Create;
    BMP.LoadFromFile(FileName);
    FImages.Add(BMP);
    result := FImages.Count - 1;
  except
    result := -1;
    end;
  end;
//------------------------------------------------------------------------------------------------

function  TRafaVisualConteiner.AddItem(image : integer; tag : integer = -1) : integer;
  var
    XHeight, XWidth : integer;
    XItem : TRafaVisualItem;
  begin
  if (image >= 0) and (image < FImages.Count) then
     begin
     XHeight := GetImage(image).Height;
     XWidth  := GetImage(image).Width;
     XItem := TRafaVisualItem.Create(FItems.Count, image, XHeight, XWidth);
     XItem.Tag := tag;
     FItems.Add(XITem);
     FOnItemEvent(FItems.Count - 1);
     result := FItems.Count - 1;
     end
  else
     result := - 1;
  end;

//------------------------------------------------------------------------------------------------

procedure TRafaVisualConteiner.BgClick(Sender: TObject);
  begin
  if FStatus = RVSSynapse then
     begin
     if FSelectedItem >= 0 then
        AddConnection(FTmpSource, FSelectedItem);
     FStatus := RVSFixed;
     end;
  end;

//------------------------------------------------------------------------------------------------

procedure TRafaVisualConteiner.BgDblClick(Sender: TObject);
  begin
  if FSelectedItem >= 0 then
     begin
     FStatus := RVSSynapse;
     FTmpSource := FSelectedItem;
     end;
  end;

//------------------------------------------------------------------------------------------------

procedure TRafaVisualConteiner.BgMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
  if Button = mbLeft then
    begin
    FIsMouseDown := true;
    FSelectedItem := GetIndexAtPosition(x, y);
    if FSelectedItem >= 0 then
       begin
       FRelativePositionX := (X - GetItem(FSelectedItem).FLeft);
       FRelativePositionY := (Y - GetItem(FSelectedItem).FTop);
       end
    else
       FIsMouseDown := false
    end;
  end;

//------------------------------------------------------------------------------------------------

procedure TRafaVisualConteiner.BgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  begin
  if not FIsMouseDown then
     begin
     if GetIndexAtPosition(x, y) >= 0 then
        Background.Cursor := crHandPoint
     else
        Background.Cursor := crDefault;
     end;
  end;

//------------------------------------------------------------------------------------------------

procedure TRafaVisualConteiner.BgMouseUp(Sender: TObject; Button: TMouseButton;  Shift: TShiftState; X, Y: Integer);
  begin
  if FIsMouseDown and (Button = mbLeft) then
    begin
    GetItem(FSelectedItem).Left := X - FRelativePositionX;
    GetItem(FSelectedItem).Top  := Y - FRelativePositionY;
    FIsMouseDown := false;
    FOnItemEvent(FSelectedItem);
    Repaint;
    end;
  end;

//------------------------------------------------------------------------------------------------

procedure TRafaVisualConteiner.ClearConnections;
  var i : integer;
  begin
  for i := FConnections.Count - 1 downto 0 do
    FConnections.Items[i].Free;
  FConnections.Clear;
  end;

//------------------------------------------------------------------------------------------------

procedure TRafaVisualConteiner.ClearImages;
  var i : integer;
  begin
  for i := FImages.Count - 1 downto 0 do
    FImages.Items[i].Free;
  FImages.Clear;
  end;

//------------------------------------------------------------------------------------------------

procedure TRafaVisualConteiner.ClearItems;
  var i : integer;
  begin
  for i := FItems.Count - 1 downto 0 do
    FItems.Items[i].Free;
  FItems.Clear;
  end;

//------------------------------------------------------------------------------------------------

constructor TRafaVisualConteiner.Create(Background : TImage);
  begin
  inherited Create;
  if BackGround <> nil then
     begin
     FBackground := Background;
     FBackground.OnClick     := BgClick;
     FBackground.OnDblClick  := BgDblClick;
     FBackground.OnMouseDown := BgMouseDown;
     FBackground.OnMouseMove := BgMouseMove;
     FBackground.OnMouseUp   := BgMouseUp;
     FItems := TObjectList.Create(false);
     FConnections := TObjectList.Create(false);
     FImages := TObjectList.Create(false);
     FIsMouseDown := false;
     FSelectedItem := -1;
     FStatus := RVSFixed;
     end
  else
     begin
     FBackground := nil;
     FItems := nil;
     FConnections := nil;
     FIsMouseDown := false;
     FSelectedItem := -1;
     FStatus := RVSFixed;
     end;
  end;

//------------------------------------------------------------------------------------------------

procedure TRafaVisualConteiner.DeleteConnection(index : integer);
  begin
  if (index >= 0) and (index < FConnections.Count - 1) then
     begin
     FConnections.Items[index].Free;
     FConnections.Delete(index);
     FOnConnectionEvent(-(index + 1));
     end;
  end;

//------------------------------------------------------------------------------------------------

procedure TRafaVisualConteiner.DeleteItem(index : integer);
  var i : integer;
  begin
  if (index >= 0) and (index < FItems.Count - 1) then
     begin
     for i := FConnections.Count - 1 downto 0 do
       begin
       if (GetConnection(i).Source = index) or (GetConnection(i).Target = index) then
          DeleteConnection(i)
       else
          begin
          if GetConnection(i).Source > index then
             GetConnection(i).Source := GetConnection(i).Source - 1;
          if GetConnection(i).Target > index then
             GetConnection(i).Target := GetConnection(i).Target - 1;
          end;
       end;

     FItems.Items[index].Free;
     FItems.Delete(index);
     FOnItemEvent(-(index + 1));
     end;
  end;

//------------------------------------------------------------------------------------------------

procedure TRafaVisualConteiner.DeleteItemByTag(tag : integer);
  begin
  DeleteItem(GetIndexByTag(tag));
  end;

//------------------------------------------------------------------------------------------------

destructor TRafaVisualConteiner.Destroy;
  var i : integer;
  begin
  for i := FItems.Count - 1 downto 0 do
    FItems.Items[i].Free;
  for i := FConnections.Count - 1 downto 0 do
    FConnections.Items[i].Free;
  for i := FImages.Count - 1 downto 0 do
    FImages.Items[i].Free;
  FItems.Free;
  FConnections.Free;
  inherited Destroy;
  end;

//------------------------------------------------------------------------------------------------

function TRafaVisualConteiner.GetBackground : TImage;
  begin
  result := FBackground;
  end;

//------------------------------------------------------------------------------------------------

function TRafaVisualConteiner.GetConnection(index : integer) : TRafaVisualConnection;
  begin
  if (index >= 0) and (index < FConnections.Count) then
     result := FConnections.items[index] as TRafaVisualConnection
  else
     result := nil;
  end;

//------------------------------------------------------------------------------------------------

function  TRafaVisualConteiner.GetConnectionIndex(Source, Target : integer) : integer;
  var i : integer;
  begin
  i := 0;
  while (i < FConnections.Count) and
        ((Source <> GetConnection(i).source) or (Target <> GetConnection(i).Target)) do
    i := i + 1;
  if i < FConnections.Count then
     result := i
  else
     result := -i;
  end;

//------------------------------------------------------------------------------------------------

function TRafaVisualConteiner.GetImage(index : integer) : TBitmap;
  begin
  if (index >= 0) and (index < FImages.Count) then
     result := FImages.items[index] as TBitmap
  else
     result := nil;
  end;

//------------------------------------------------------------------------------------------------

function TRafaVisualConteiner.GetIndexAtPosition(x, y : integer) : integer;
  var i : integer;
  begin
  i := FItems.Count - 1;
  while (i >= 0) and not GetItem(i).ContainPoint(x, y) do
    i := i - 1;
  result := i;
  end;

//------------------------------------------------------------------------------------------------

function TRafaVisualConteiner.GetIndexByTag(tag : integer) : integer;
  var i : integer;
  begin
  i := 0;
  while (i < FItems.Count - 1) and (GetItem(i).Tag <> tag) do
    i := i + 1;
  if i < FItems.Count - 1 then
     result := i
  else
     result := -1;
  end;

//------------------------------------------------------------------------------------------------

function TRafaVisualConteiner.GetItem(index : integer) : TRafaVisualItem;
  begin
  if (index >= 0) and (index < FItems.Count) then
     result := FItems.items[index] as TRafaVisualItem
  else
     result := nil;
  end;

//------------------------------------------------------------------------------------------------

function TRafaVisualConteiner.GetItemAtPosition(x, y : integer) : TRafaVisualItem;
  var i : integer;
  begin
  i := GetIndexAtPosition(x, y);
  if i >= 0 then
     result := GetItem(i)
  else
     result := nil;
  end;

//------------------------------------------------------------------------------------------------

function TRafaVisualConteiner.GetItemByTag(tag : integer) : TRafaVisualItem;
  var i : integer;
  begin
  i := GetIndexByTag(tag);
  if i >= 0 then
     result := GetItem(i)
  else
     result := nil;
  end;

//------------------------------------------------------------------------------------------------

function  TRafaVisualConteiner.GetOnConnectionEvent : TRafaVisualEvent;
  begin
  result := FOnConnectionEvent;
  end;

//------------------------------------------------------------------------------------------------

function  TRafaVisualConteiner.GetOnItemEvent : TRafaVisualEvent;
  begin
  result := FOnItemEvent;
  end;

//------------------------------------------------------------------------------------------------

function TRafaVisualConteiner.GetSelectedItem : integer;
  begin
  result := FSelectedItem
  end;

//------------------------------------------------------------------------------------------------

procedure TRafaVisualConteiner.Repaint;
  var i : integer;
  begin
  FBackground.Canvas.Pen.Color := clwhite;
  FBackground.Canvas.Brush.Color := clwhite;
  FBackground.Canvas.Rectangle(0, 0, FBackground.Width - 1, FBackground.Height - 1);

  for i := 0 to FConnections.Count - 1 do
    begin
    FBackground.Canvas.Pen.Color := GetConnection(i).Color;
    FBackground.Canvas.MoveTo(GetItem(GetConnection(i).Source).CenterX,
                              GetItem(GetConnection(i).Source).CenterY);
    FBackground.Canvas.LineTo(GetItem(GetConnection(i).Target).CenterX,
                              GetItem(GetConnection(i).Target).CenterY);
    end;

  for i := 0 to FItems.Count - 1 do
    if i <> FSelectedItem then
      FBackground.Canvas.Draw(GetItem(i).Left, GetItem(i).Top, GetImage(GetItem(i).Image));
  if (FSelectedItem >= 0) and (FItems.Count > 0) then
     begin
     FBackground.Canvas.Pen.Color := clGray;
     FBackground.Canvas.Pen.Width := 3;

     FBackground.Canvas.Rectangle(GetItem(FSelectedItem).Left - 1, GetItem(FSelectedItem).Top - 1,
                                    GetItem(FSelectedItem).Right + 1, GetItem(FSelectedItem).Bottom + 1);
     FBackground.Canvas.Draw(GetItem(FSelectedItem).Left, GetItem(FSelectedItem).Top, GetImage(GetItem(FSelectedItem).Image));
     FBackground.Canvas.Pen.Width := 1;

     end;
  end;

//------------------------------------------------------------------------------------------------

procedure TRafaVisualConteiner.SetOnConnectionEvent(event : TRafaVisualEvent);
  begin
  FOnConnectionEvent := event;
  end;

//------------------------------------------------------------------------------------------------

procedure TRafaVisualConteiner.SetOnItemEvent(event : TRafaVisualEvent);
  begin
  FOnItemEvent := event;
  end;

//------------------------------------------------------------------------------------------------

procedure TRafaVisualConteiner.SetSelectedItem(value : integer);
  begin
  if (value >= 0) and (value < FItems.Count) then
     FSelectedItem := value
  else
     FSelectedItem := -1;
  end;

//------------------------------------------------------------------------------------------------

end.
