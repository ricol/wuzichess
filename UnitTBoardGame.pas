unit UnitTBoardGame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, ExtCtrls, StdCtrls, ToolWin, UnitCommon;

type
  TBoardGame = class
  protected
    FLenX: Double;
    FLenY: Double;
    FBlackColor: TColor;
    FTurn: TTurn;
    FWhiteColor: TColor;
    FGridColor: TColor;
    FLastMove: TPoint;
    FOldLastMove: TPoint;
    FOldAvailableMoveData: TListOfPoints;
    FAvailableMoveData: TListOfPoints;
    FOldAvailableMoveNumber: Integer;
    FAvailableMoveNumber: Integer;
    FProgressBar: TProgressBar;
    FListBox: TListBox;
    FLastTurn: TTurn;
    FIsTempGame: Boolean;
    FIsPlaying: Boolean;
    FSize: Integer;
    FPaintBox: TPaintBox;
    FBoard: TBoard;
    FSoundEffect_Regret: string;
    FSoundEffect_Win: string;
    FSoundEffect_Click: string;
    FBackgroundMusic: string;
    FSoundEffect_Lose: string;
    FSoundEffect_Draw: string;
    FBackgroundColor: TColor;
    FDifficulty: TDifficulties;
    procedure SetIsTempGame(const Value: Boolean);
    procedure SetBackgroundColor(const Value: TColor); virtual;
    procedure SetBackgroundMusic(const Value: string); virtual;
    procedure SetDifficulty(const Value: TDifficulties); virtual;
    procedure SetSoundEffect_Click(const Value: string); virtual;
    procedure SetSoundEffect_Draw(const Value: string); virtual;
    procedure SetSoundEffect_Lose(const Value: string); virtual;
    procedure SetSoundEffect_Regret(const Value: string); virtual;
    procedure SetSoundEffect_Win(const Value: string); virtual;
    procedure SetIsPlaying(const Value: Boolean); virtual;
    procedure SetBlackColor(const Value: TColor);
    procedure SetGridColor(const Value: TColor);
    procedure SetProgressBar(const Value: TProgressBar);
    procedure SetTurn(const Value: TTurn);
    procedure SetWhiteColor(const Value: TColor);
    procedure SetPiece(piece: TPiece; i, j: integer);
    function GetPiece(i, j: integer): TPiece;
    procedure SetListBox(const Value: TListBox);
    procedure SetLenX(const Value: Double);
    procedure SetLenY(const Value: Double);
  public
    constructor Create(PaintBox: TPaintBox; size: Integer; TempObject: Boolean); overload; virtual;
    constructor Create(boardGame: TBoardGame); overload; virtual;
    destructor Destroy(); override;
    property Difficulty: TDifficulties read FDifficulty write SetDifficulty;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
    property BackgroundMusic: string read FBackgroundMusic write SetBackgroundMusic;
    property SoundEffect_Click: string read FSoundEffect_Click write SetSoundEffect_Click;
    property SoundEffect_Regret: string read FSoundEffect_Regret write SetSoundEffect_Regret;
    property SoundEffect_Win: string read FSoundEffect_Win write SetSoundEffect_Win;
    property SoundEffect_Lose: string read FSoundEffect_Lose write SetSoundEffect_Lose;
    property SoundEffect_Draw: string read FSoundEffect_Draw write SetSoundEffect_Draw;
    property IsPlaying: Boolean read FIsPlaying write SetIsPlaying;
    property IsTempGame: Boolean read FIsTempGame write SetIsTempGame;
    property LastTurn: TTurn read FLastTurn;
    property WhiteColor: TColor read FWhiteColor write SetWhiteColor;
    property BlackColor: TColor read FBlackColor write SetBlackColor;
    property GridColor: TColor read FGridColor write SetGridColor;
    property Turn: TTurn read FTurn write SetTurn;
    property ProgressBar: TProgressBar read FProgressBar write SetProgressBar;
    property ListBox: TListBox read FListBox write SetListBox;
    property LastMove: TPoint read FLastMove;
    property LenX: Double read FLenX write SetLenX;
    property LenY: Double read FLenY write SetLenY;
    procedure About(); virtual; abstract;
    procedure NewGame(); virtual; abstract;
    procedure CloseGame(); virtual; abstract;
    procedure SaveGame(); virtual; abstract;
    procedure LoadGame(); virtual; abstract;
    procedure ResetGame; virtual; abstract;
    procedure Refresh(); virtual; abstract;
    procedure Swap(); virtual; abstract;
    function XToJ(x: Double): integer;
    function YToI(y: Double): integer;
    function JToX(j: integer): Double;
    function IToY(i: integer): Double;
    function IsValidI(i: Integer): Boolean;
    function IsValidJ(j: Integer): Boolean;
    function IsValidIJ(i, j: Integer): Boolean;
    class function GetOpponent(piece: TPiece): TPiece;
  end;

implementation

{ TBoardGame }

constructor TBoardGame.Create(PaintBox: TPaintBox; size: Integer; TempObject: Boolean);
begin
  inherited Create();
  FSize := size;
  SetLength(Self.FBoard, 0, 0);
  SetLength(Self.FBoard, size, size);
  FSoundEffect_Lose := '';
  FSoundEffect_Draw := '';
  FSoundEffect_Win := '';
  FSoundEffect_Regret := '';
  FBackgroundMusic := '';
  FSoundEffect_Click := '';
  FBackgroundColor := COLOR_BACKGROUND;
  FDifficulty := DIFF_LOW;
  FIsTempGame := TempObject;
  FPaintBox := PaintBox;
  FLenX := PaintBox.ClientWidth / FSize;
  FLenY := PaintBox.ClientHeight / FSize;
  FOldAvailableMoveData := TListOfPoints.Create;
  FAvailableMoveData := TListOfPoints.Create;
end;

constructor TBoardGame.Create(boardGame: TBoardGame);
var
  i: Integer;
  j: Integer;
begin
  inherited Create();
  FIsTempGame := boardGame.FIsTempGame;
  FIsPlaying := boardGame.FIsPlaying;
  FSize := boardGame.FSize;
  FPaintBox := boardGame.FPaintBox;
  FLenX := boardGame.FLenX;
  FLenY := boardGame.FLenY;
  SetLength(Self.FBoard, 0, 0);
  SetLength(Self.FBoard, boardGame.FSize, boardGame.FSize);
  for i := 0 to FSize - 1 do
    for j := 0 to FSize - 1 do
      FBoard[i, j] := boardGame.FBoard[i, j];

  FSoundEffect_Lose := boardGame.FSoundEffect_Lose;
  FSoundEffect_Draw := boardGame.FSoundEffect_Draw;
  FSoundEffect_Win := boardGame.FSoundEffect_Win;
  FSoundEffect_Regret := boardGame.FSoundEffect_Regret;
  FBackgroundMusic := boardGame.FBackgroundMusic;
  FSoundEffect_Click := boardGame.FSoundEffect_Click;
  FBackgroundColor := boardGame.FBackgroundColor;
  FDifficulty := boardGame.FDifficulty;
  FOldAvailableMoveData := TListOfPoints.Create;
  FAvailableMoveData := TListOfPoints.Create;
end;

destructor TBoardGame.Destroy;
begin
  SetLength(Self.FBoard, 0, 0);
  inherited;
end;

function TBoardGame.IsValidI(i: Integer): Boolean;
begin
  Result := (i <= NUMBER - 1) and (i >= 0);
end;

function TBoardGame.IsValidIJ(i, j: Integer): Boolean;
begin
  Result := Self.IsValidI(i) and Self.IsValidJ(j);
end;

function TBoardGame.IsValidJ(j: Integer): Boolean;
begin
  Result := (j <= NUMBER - 1) and (j >= 0);
end;

function TBoardGame.IToY(i: integer): Double;
begin
  result := i * FLenY;
end;

function TBoardGame.JToX(j: integer): Double;
begin
  result := j * FLenX;
end;

procedure TBoardGame.SetBackgroundColor(const Value: TColor);
begin
  FBackgroundColor := Value;
end;

procedure TBoardGame.SetBackgroundMusic(const Value: string);
begin
  FBackgroundMusic := Value;
end;

procedure TBoardGame.SetBlackColor(const Value: TColor);
begin
  FBlackColor := Value;
end;

procedure TBoardGame.SetDifficulty(const Value: TDifficulties);
begin
  FDifficulty := Value;
end;

procedure TBoardGame.SetGridColor(const Value: TColor);
begin
  FGridColor := Value;
end;

procedure TBoardGame.SetIsPlaying(const Value: Boolean);
begin
  FIsPlaying := Value;
end;

procedure TBoardGame.SetIsTempGame(const Value: Boolean);
begin
  FIsTempGame := Value;
end;

procedure TBoardGame.SetLenX(const Value: Double);
begin
  FLenX := Value;
end;

procedure TBoardGame.SetLenY(const Value: Double);
begin
  FLenY := Value;
end;

procedure TBoardGame.SetListBox(const Value: TListBox);
begin
  FListBox := Value;
end;

procedure TBoardGame.SetProgressBar(const Value: TProgressBar);
begin
  FProgressBar := Value;
end;

procedure TBoardGame.SetSoundEffect_Click(const Value: string);
begin
  FSoundEffect_Click := Value;
end;

procedure TBoardGame.SetSoundEffect_Draw(const Value: string);
begin
  FSoundEffect_Draw := Value;
end;

procedure TBoardGame.SetSoundEffect_Lose(const Value: string);
begin
  FSoundEffect_Lose := Value;
end;

procedure TBoardGame.SetSoundEffect_Regret(const Value: string);
begin
  FSoundEffect_Regret := Value;
end;

procedure TBoardGame.SetSoundEffect_Win(const Value: string);
begin
  FSoundEffect_Win := Value;
end;

procedure TBoardGame.SetTurn(const Value: TTurn);
begin
  FTurn := Value;
end;

procedure TBoardGame.SetWhiteColor(const Value: TColor);
begin
  FWhiteColor := Value;
end;

function TBoardGame.XToJ(x: Double): integer;
begin
  result := Trunc(x / FLenx);
end;

function TBoardGame.YToI(y: Double): integer;
begin
  result := Trunc(y / FLenY);
end;

class function TBoardGame.GetOpponent(piece: TPiece): TPiece;
begin
  if piece = PIECE_WHITE then
    Result := PIECE_BLACK
  else
    Result := PIECE_WHITE
end;

function TBoardGame.GetPiece(i, j: integer): TPiece;
begin
  Result := FBoard[i, j];
end;

procedure TBoardGame.SetPiece(piece: TPiece; i, j: integer);
begin
  FBoard[i, j] := piece;
end;

end.
