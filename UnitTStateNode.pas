unit UnitTStateNode;

interface

uses
  Messages, Windows, SysUtils, UnitTBoardGame, UnitCommon, Generics.Collections;

type
  TStateNode = class;

  TListOfNodes = TList<TStateNode>;

  TStateNode = class
  protected
    FparentNode: TStateNode;
    Fstep_j: Integer;
    Fstep_i: Integer;
    FnextNodes: TListOfNodes;
    Fdata: TBoardGame;
    function getData: TBoardGame;
    function getI: Integer;
    function getJ: Integer;
    procedure Setdata(const Value: TBoardGame);
    procedure SetnextNodes(const Value: TListOfNodes);
    procedure SetparentNode(const Value: TStateNode);
    procedure Setstep_i(const Value: Integer);
    procedure Setstep_j(const Value: Integer);
  public
    constructor Create();
    destructor Destroy(); override;
    property data: TBoardGame read Fdata write Setdata;
    property step_i: Integer read Fstep_i write Setstep_i;
    property step_j: Integer read Fstep_j write Setstep_j;
    property parentNode: TStateNode read FparentNode write SetparentNode;
    property nextNodes: TListOfNodes read FnextNodes write SetnextNodes;
  end;

implementation

{ TStateNode }

constructor TStateNode.Create;
begin
  inherited;
end;

destructor TStateNode.Destroy;
begin
  FreeAndNil(FnextNodes);
  FreeAndNil(Fdata);
  inherited;
end;

function TStateNode.getData: TBoardGame;
begin
  result := data;
end;

function TStateNode.getI: Integer;
begin
  result := step_i;
end;

function TStateNode.getJ: Integer;
begin
  result := step_j;
end;

procedure TStateNode.Setdata(const Value: TBoardGame);
begin
  Fdata := Value;
end;

procedure TStateNode.SetnextNodes(const Value: TListOfNodes);
begin
  FnextNodes := Value;
end;

procedure TStateNode.SetparentNode(const Value: TStateNode);
begin
  FparentNode := Value;
end;

procedure TStateNode.Setstep_i(const Value: Integer);
begin
  Fstep_i := Value;
end;

procedure TStateNode.Setstep_j(const Value: Integer);
begin
  Fstep_j := Value;
end;


end.
