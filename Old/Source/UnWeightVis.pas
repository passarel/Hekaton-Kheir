unit UnWeightVis;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, ExtCtrls, Grids, ComCtrls;

type
  TFmWeightVis = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    ScrollBox1: TScrollBox;
    ScrollBox2: TScrollBox;
    StringGrid1: TStringGrid;
    Image1: TImage;
    Image2: TImage;
    Label1: TLabel;
    Label2: TLabel;
    MainMenu1: TMainMenu;
    Visualisation1: TMenuItem;
    LoadNetwork1: TMenuItem;
    PonderedView1: TMenuItem;
    OpenDialog1: TOpenDialog;
    TrackBar1: TTrackBar;
    SaveGrid1: TMenuItem;
    SaveDialog1: TSaveDialog;
    procedure PonderedView1Click(Sender: TObject);
    procedure LoadNetwork1Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure SaveGrid1Click(Sender: TObject);
  private
    { Private declarations }
    FData : array of array of array of double;
    FBias : array of array of double;
    FlayerCount : integer;
    FNodesPerLayer : array of Integer;
    Procedure UpdateVisualisation;
  public
    { Public declarations }
  end;

var
  FmWeightVis: TFmWeightVis;

implementation

uses UnNetRep;

{$R *.dfm}

procedure TFmWeightVis.PonderedView1Click(Sender: TObject);
  begin
  PonderedView1.Checked := not PonderedView1.Checked;
  UpdateVisualisation;
  end;

procedure TFmWeightVis.LoadNetwork1Click(Sender: TObject);
  var
    XNet : TNetworkRep;
    i, j, k, max: integer;
    XLayers, XLayerIndex : array of integer;
  begin
  if OpenDialog1.Execute then
     begin
     XNet := TNetworkRep.Create(nil, nil);
     XNet.LoadFromXML(OpenDialog1.FileName);
     SetLength(XLayers,XNet.NoNodes);
     SetLength(XLayerIndex, XNet.NoNodes);

     for i := 0 to XNet.NoNodes - 1 do
       begin
       XLayers[i] := -1;
       XLayerIndex[i] := -1;
       end;

     fLayerCount := XNet.GetLayers(XLayers);
     SetLength(FNodesPerLayer, fLayerCount);
     for i := 0 to fLayerCount - 1 do
       FNodesPerLayer[i] := 0;

     max := 0;

     for i := 0 to XNet.NoNodes - 1 do
       begin
       if XLayers[i] >= 0 then
          begin
          XLayerIndex[i] := FNodesPerLayer[XLayers[i]];
          FNodesPerLayer[XLayers[i]] := FNodesPerLayer[XLayers[i]] + 1;
          if FNodesPerLayer[XLayers[i]] > max then
             max := FNodesPerLayer[XLayers[i]];
          end;
       end;

     SetLength(FData, FLayerCount - 1, max, max);
     SetLength(FBias, FLayerCount, max);

     for i := 0 to FLayerCount - 2 do
       for j := 0 to max - 1 do
         for k := 0 to max - 1 do
           FData[i, j, k] := 0;

     for i := 0 to FLayerCount - 1 do
       for j := 0 to max - 1 do
         FBias[i, j] := 0;


     for i := 0 to XNet.NoArcs - 1 do
       begin
       with XNet.Arc[i] do
         if (XNet.Node[Source].Kind = UKNeuron) and (XNet.Node[Target].Kind = UKNeuron) then
            FData[XLayers[Source], XLayerIndex[Source], XLayerIndex[Target]] := Weight;
       end;

     for i := 0 to XNet.NoNodes - 1 do
       if XNet.Node[i].Kind = UkNeuron then
          FBias[XLayers[i], XLayerIndex[i]] := (XNet.Node[i] as TNeuronRep).BiasWeight;
     XNet.Free;

     UpdateVisualisation;
     end
  end;

procedure TFmWeightVis.UpdateVisualisation;
  var i, j, x, y, cl : integer;
  begin
  if (FlayerCount > 3) or (FLayerCount < 2)then
     MessageDlg('Network with unaccepted number of layers',mtError, [MbOK], 0)
  else
     begin
     StringGrid1.ColCount := FNodesPerLayer[1] + 3;
     StringGrid1.RowCount := FNodesPerLayer[0] + 4;
     if FlayerCount > 2 then
        StringGrid1.RowCount := StringGrid1.RowCount + FNodesPerLayer[2];
     StringGrid1.FixedCols := 1;
     StringGrid1.FixedRows := 1;
     for i := 0  to FNodesPerLayer[1] - 1 do
       StringGrid1.Cells[i + 1, 0] := 'Hidden' + inttostr(i);
     StringGrid1.Cells[FNodesPerLayer[1] + 2, 0] := 'Biases';
     for j := 0  to FNodesPerLayer[0] - 1 do
       StringGrid1.Cells[0, j + 1] := 'Input' + inttostr(j);
     if FlayerCount > 2 then
       for j := 0 to FNodesPerLayer[2] - 1 do
         StringGrid1.Cells[0, j + FNodesPerLayer[0] + 2] := 'Output' + inttostr(j);
     StringGrid1.Cells[0, StringGrid1.RowCount - 1] := 'Biases';
     for i := 0 to FNodesPerLayer[1] - 1 do
       for j := 0 to FNodesPerLayer[0] - 1 do
         StringGrid1.Cells[i + 1, j + 1] := FloatToStr(FData[0, j, i]);
     if FlayerCount > 2 then
        for i := 0 to FNodesPerLayer[1] - 1 do
          for j := 0 to FNodesPerLayer[2] - 1 do
            StringGrid1.Cells[i + 1, j + FNodesPerLayer[0] + 2] := FloatToStr(FData[1, i, j]);
     for i := 0 to FNodesPerLayer[0] - 1 do
       StringGrid1.Cells[FNodesPerLayer[1] + 2, i + 1] := FloatToStr(FBias[0, i]);
     for i := 0 to FNodesPerLayer[1] - 1 do
       StringGrid1.Cells[i + 1, StringGrid1.RowCount - 1] := FloatToStr(FBias[1, i]);
     if FLayerCount > 2 then
        for i := 0 to FNodesPerLayer[2] - 1 do
          StringGrid1.Cells[FNodesPerLayer[1] + 2, FNodesPerLayer[0] + i + 2] := FloatToStr(FBias[2, i]);

     x := image1.Width div FNodesPerLayer[1];
     y := image1.Height div FNodesPerLayer[0];
     for i := 0 to FNodesPerLayer[1] - 1 do
       for j := 0 to FNodesPerLayer[0] - 1 do
         begin
         cl := round(255 * (FData[0, j, i] / TrackBar1.Position));
         if cl > 255 then cl := 255;
         if cl < -255 then cl := -255;
         //image1.Canvas.Brush.Color := $00000000;
         image1.Canvas.Brush.Color := $00FFFFFF;
         if cl >= 0 then
            //image1.Canvas.Brush.Color := image1.Canvas.Brush.Color + cl
            image1.Canvas.Brush.Color := image1.Canvas.Brush.Color - (cl + (256 * cl))
         else
            //image1.Canvas.Brush.Color := image1.Canvas.Brush.Color + ((-cl) * 256 * 256);
            image1.Canvas.Brush.Color := image1.Canvas.Brush.Color - ((256 * 256 * (-cl)) + (256 * (-cl)));
         image1.Canvas.Rectangle(x * i, y * j, x * (i + 1), y * (j + 1));
         end;

     x := image1.Width div FNodesPerLayer[1];
     y := image1.Height div FNodesPerLayer[2];
     if FlayerCount > 2 then
        for i := 0 to FNodesPerLayer[1] - 1 do
          for j := 0 to FNodesPerLayer[2] - 1 do
            begin
            cl := round(255 * (FData[1, i, j] / TrackBar1.Position));
            if cl > 255 then cl := 255;
            if cl < -255 then cl := -255;
//            image2.Canvas.Brush.Color := $00000000;
            image2.Canvas.Brush.Color := $00FFFFFF;
            if cl >= 0 then
//               image2.Canvas.Brush.Color := image2.Canvas.Brush.Color + cl
               image2.Canvas.Brush.Color := image2.Canvas.Brush.Color - (cl + (256 * cl))
            else
//               image2.Canvas.Brush.Color := image2.Canvas.Brush.Color + ((-cl) * 256 * 256);
            image2.Canvas.Brush.Color := image2.Canvas.Brush.Color - ((256 * 256 * (-cl)) + (256 * (-cl)));
            image2.Canvas.Rectangle(x * i, y * j, x * (i + 1), y * (j + 1));
            end;
     end;


  end;

procedure TFmWeightVis.TrackBar1Change(Sender: TObject);
  begin
  UpdateVisualisation;
  end;

procedure TFmWeightVis.SaveGrid1Click(Sender: TObject);
  var
    i : integer;
    StrL : TStringList;
    s : string;
  begin
  if SaveDialog1.Execute then
     begin
     Strl := TStringList.Create;
     for i := 0 to StringGrid1.RowCount - 1 do
       begin
       StringGrid1.Rows[i].Delimiter := #9;
       s := StringGrid1.Rows[i].DelimitedText;
       Strl.Add(s);
       end;
     Strl.SaveToFile(SaveDialog1.FileName);
     StrL.Free;
     end;
  end;

end.
