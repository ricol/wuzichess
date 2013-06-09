unit UnitTWuziChessGame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, ExtCtrls, StdCtrls, ToolWin, UnitCommon, UnitTBoardGame, UnitTStateNode, UnitTStateTree;

type
  TWuziChessGame = class(TBoardGame)
  protected
    function GetPieceFromTurn(turn: TTurn): TPiece;
    function AnalyzeTheStateTree(stateTree: TStateTree; turn: TTurn): TPoint;
    procedure SetIsPlaying(const Value: Boolean); override;
    procedure DrawBoard();
    procedure DrawLastMove();
    procedure DrawPiece(piece: TPiece; i, j: integer);
    procedure DrawAvailableMove();
    function GoToLevel(var x, y: Integer; piece: TPiece): boolean;
    function AnalyzeToLevel(level: Integer; var x, y: Integer; piece: TPiece): boolean;
    procedure NextLevel(const totalLevel: Integer; piece: TPiece; const step: TPoint; gameOld: TWuziChessGame; var stateTree: TStateTree; var currentNode: TStateNode);
  public
    constructor Create(PaintBox: TPaintBox; size: Integer; TempObject: Boolean); overload; override;
    constructor Create(wuziChess: TBoardGame); overload; override;
    destructor Destroy(); override;
    function IsAvailableMove(i, j: Integer; piece: TPiece): Boolean;
    procedure PlayAtMove(i, j: Integer; piece: TPiece);
    procedure EndGameAndPrint();
    function GetAllAvailableMove(var data: TListOfPoints; piece: TPiece): Integer;
    procedure DrawAllAvailableMoves(turn: TTurn);
    function AutoPlay(var i, j: Integer; piece: TPiece; way: TWay): Boolean;
    procedure BlinkLastMove();
    procedure About(); override;
    procedure NewGame(); override;
    procedure CloseGame(); override;
    procedure ResetGame; override;
    procedure SaveGame(); override;
    procedure LoadGame(); override;
    procedure Swap(); override;
    procedure Refresh(); override;
    procedure Print(node: TStateNode);
    function TimeToCheck(): Boolean;
    function GetPiecesNumber(piece: TPiece): Integer;
  end;

implementation

{ TWuziChessGame }

uses
  UnitTWuziChessStateTree;

var
  GStateTree: TStateTree;

procedure TWuziChessGame.About;
begin
  inherited;
  MessageBox(Application.Handle,
           'Game: WuziChess' + #$D + #$A +
           'Programmer: Ricol Wang' + #$D + #$A +
           'Date: 09/06/2013 - V1.0', 'About', MB_OK);
end;

function TWuziChessGame.AnalyzeTheStateTree(stateTree: TStateTree;
  turn: TTurn): TPoint;
var
  tmpStateTree: TStateTree;
  tmpAllLeaves: TListOfNodes;
  tmpCurrentNode: TStateNode;
  tmpGameCurrent: TWuziChessGame;
  tmpMax, k, l, tmpNumberOfBlack, tmpNumberOfWhite, tmpResult: Integer;
  tmpPoint: TPoint;
begin
  //analyze the state tree and find the optimum node
  tmpStateTree := stateTree;
  tmpAllLeaves := tmpStateTree.getAllLeaves(True);

  tmpMax := 0;
  l := 0;
  OutputDebugString(PChar(Format('Analyzing...Total Leaves: %d', [tmpAllLeaves.Count])));

  if turn = WHITE then
  begin
    for k := 0 to tmpAllLeaves.Count - 1 do
    begin
      tmpCurrentNode := tmpAllLeaves[k];

      tmpGameCurrent := TWuziChessGame(tmpCurrentNode.data);

      tmpNumberOfBlack := tmpGameCurrent.GetPiecesNumber(PIECE_BLACK);
      tmpNumberOfWhite := tmpGameCurrent.GetPiecesNumber(PIECE_WHITE);
      tmpResult := tmpNumberOfWhite - tmpNumberOfBlack;

      if tmpNumberOfBlack <= 0 then
      begin
        l := k;
        OutputDebugString(PChar(Format('#######: Found The Optimal Result! Level %d', [TStateTree.getLevel(tmpCurrentNode)])));
        OutputDebugString(PChar(Format('tmpResult: %d', [tmpResult])));
        Beep;
        break;
      end;

      if tmpResult >= tmpMax then
      begin
        l := k;
        tmpMax := tmpResult;
        OutputDebugString(PChar(Format('tmpMax: %d', [tmpMax])));
      end;
    end;
  end else begin
    for k := 0 to tmpAllLeaves.Count - 1 do
    begin
      tmpCurrentNode := tmpAllLeaves[k];
      tmpGameCurrent := TWuziChessGame(tmpCurrentNode.data);

      tmpNumberOfBlack := tmpGameCurrent.GetPiecesNumber(PIECE_BLACK);
      tmpNumberOfWhite := tmpGameCurrent.GetPiecesNumber(PIECE_WHITE);
      tmpResult := tmpNumberOfBlack - tmpNumberOfWhite;

      if tmpNumberOfWhite <= 0 then
      begin
        l := k;
        OutputDebugString(PChar(Format('#######: Found The Optimal Result! Level %d', [TStateTree.getLevel(tmpCurrentNode)])));
        OutputDebugString(PChar(Format('tmpResult: %d', [tmpResult])));
        Beep;
        break;
      end;

      if tmpResult >= tmpMax then
      begin
        l := k;
        tmpMax := tmpResult;
        OutputDebugString(PChar(Format('tmpMax: %d', [tmpMax])));
      end;
    end;
  end;

  tmpCurrentNode := tmpAllLeaves[l];
  tmpPoint.X := tmpCurrentNode.step_i;
  tmpPoint.Y := tmpCurrentNode.step_j;
  FreeAndNil(tmpAllLeaves);

  Result := tmpPoint;
end;


function TWuziChessGame.AnalyzeToLevel(level: Integer; var x, y: Integer;
  piece: TPiece): boolean;
var
  k: Integer;
  tmpNumber: Integer;
  tmpData: TListOfPoints;
  tmpGame: TWuziChessGame;

  tmpCurrentNode: TStateNode;
  tmpPoint: TPoint;
  tmpHead: TBoardGame;
begin
  //create the State Tree and save all possible chess state into the tree
  OutputDebugString('Analyzing by recursive...');

  tmpHead := TWuziChessGame.Create(Self);
  GStateTree := TWuziChessStateTree.Create(tmpHead);
  GStateTree.ListBox := Self.ListBox;
  tmpCurrentNode := GStateTree.Head;

  //get all possible moves for piece.
  tmpNumber := GetAllAvailableMove(tmpData, piece);

  if tmpNumber > 0 then
  begin
    FProgressBar.Min := 0;
    FProgressBar.Max := tmpNumber - 1;
    FProgressBar.Position := 0;
    FProgressBar.Visible := True;
    for k := 0 to tmpNumber - 1 do
    begin
      FProgressBar.Position := k;
      Application.ProcessMessages;
      tmpGame := TWuziChessGame.Create(Self);
      tmpGame.IsTempGame := True;
      tmpPoint := tmpData[k];
      tmpGame.PlayAtMove(tmpPoint.x, tmpPoint.y, piece);
      tmpCurrentNode := GStateTree.InsertTheNode(tmpGame, tmpPoint.x, tmpPoint.y, tmpCurrentNode);

      self.NextLevel(level, piece, tmpPoint, tmpGame, GStateTree, tmpCurrentNode);

      tmpCurrentNode := tmpCurrentNode.parentNode;
    end;
    //return a optimal result by analyzing the state tree.
    tmpPoint := Self.AnalyzeTheStateTree(GStateTree, Self.Turn);
    x := tmpPoint.X;
    y := tmpPoint.Y;
    OutputDebugString(PChar(Format('Choose: %d, %d', [x + 1, y + 1])));
    Result := true;
    FreeAndNil(tmpData);
    GStateTree.Print;
    FreeAndNil(GStateTree);
    FProgressBar.Visible := False;
    exit;
  end else
  begin
    result := False;
    FreeAndNil(tmpData);
    GStateTree.Print;
    FreeAndNil(GStateTree);
    FProgressBar.Visible := False;
  end;
end;

function TWuziChessGame.AutoPlay(var i, j: Integer; piece: TPiece;
  way: TWay): Boolean;
var
  tmpI, tmpJ: Integer;
begin
  Result := Self.GetPiecesNumber(PIECE_BLANK) > 0;
  if Result then
  begin
    repeat
      tmpI := random(NUMBER);
      tmpJ := random(NUMBER);
    until self.GetPiece(tmpI, tmpJ) = PIECE_BLANK;
    i := tmpI;
    j := tmpJ;
  end;
//  if way = LOOP then
//    Result := self.GoToLevel(i, j, piece)
//  else
//    Result := self.AnalyzeToLevel(GComputerCalculateLevel, i, j, piece);
end;

procedure TWuziChessGame.BlinkLastMove;
begin
  if Self.IsTempGame then Exit;
  DrawPiece(GetPiece(FLastMove.x, FLastMove.y), FLastMove.x, FLastMove.y);
  Application.ProcessMessages;
  Sleep(300);
  DrawLastMove();
  Application.ProcessMessages;
  Sleep(300);
  DrawPiece(GetPiece(FLastMove.x, FLastMove.y), FLastMove.x, FLastMove.y);
  Application.ProcessMessages;
  Sleep(300);
  DrawLastMove();
end;

procedure TWuziChessGame.CloseGame;
begin
  inherited;
  IsPlaying := False;
  Self.Refresh;
end;

constructor TWuziChessGame.Create(wuziChess: TBoardGame);
var
  tmpGame: TWuziChessGame;
begin
  inherited Create(wuziChess);
  tmpGame := TWuziChessGame(wuziChess);
  FGridColor := tmpGame.FGridColor;
  FBlackColor := tmpGame.FBlackColor;
  FWhiteColor := tmpGame.FWhiteColor;
  FTurn := tmpGame.FTurn;
  FLastMove := tmpGame.FLastMove;
  FOldLastMove := tmpGame.FOldLastMove;
  FOldAvailableMoveNumber := tmpGame.FOldAvailableMoveNumber;
  if not FIsTempGame then
  begin
    FOldAvailableMoveData := TListOfPoints.Create(tmpGame.FOldAvailableMoveData);
    FAvailableMoveData := TListOfPoints.Create(tmpGame.FAvailableMoveData);
  end;
  FProgressBar := tmpGame.FProgressBar;
  FListBox := tmpGame.FListBox;
end;


constructor TWuziChessGame.Create(PaintBox: TPaintBox; size: Integer;
  TempObject: Boolean);
var
  i, j: integer;
begin
  inherited Create(PaintBox, size, TempObject);
  for i := 0 to size - 1 do
    for j := 0 to size - 1 do
      FBoard[i, j] := PIECE_BLANK;
  Self.FWhiteColor := COLOR_WHITE;
  Self.FGridColor := COLOR_GRID;
  Self.FBlackColor := COLOR_BLACK;
  Self.FTurn := WHITE;
  Self.FOldAvailableMoveNumber := 0;
  Self.FAvailableMoveNumber := 0;
end;

destructor TWuziChessGame.Destroy;
begin
  FreeAndNil(FOldAvailableMoveData);
  FreeAndNil(FAvailableMoveData);
  inherited;
end;

procedure TWuziChessGame.DrawAllAvailableMoves(turn: TTurn);
begin
//  if Self.IsTempGame then Exit;
//  FreeAndNil(FOldAvailableMoveData);
//  if FAvailableMoveData <> nil then
//    FOldAvailableMoveData := TListOfPoints.Create(FAvailableMoveData)
//  else
//    FOldAvailableMoveData := TListOfPoints.Create;
//  FOldAvailableMoveNumber := FAvailableMoveNumber;
//  FAvailableMoveNumber := GetAllAvailableMove(FAvailableMoveData, GetPieceFromTurn(turn));
//  DrawAvailableMove();
end;

procedure TWuziChessGame.DrawAvailableMove;
var
  tmpI, tmpJ, tmpX, tmpY, tmpStartX, tmpStartY, tmpStartLenX, tmpStartLenY, k: Integer;
  tmpPoint: TPoint;
begin
  if Self.IsTempGame then Exit;
  for k := 0 to FOldAvailableMoveNumber - 1 do
  begin
    tmpPoint := FOldAvailableMoveData[k];
    tmpI := tmpPoint.x;
    tmpJ := tmpPoint.y;
    DrawPiece(GetPiece(tmpI, tmpJ), tmpI, tmpJ);
  end;
  if not IsPlaying then exit;
  for k := 0 to FAvailableMoveNumber - 1 do
  begin
    tmpPoint := FAvailableMoveData[k];
    tmpI := tmpPoint.x;
    tmpJ := tmpPoint.y;
    tmpX := JToX(tmpJ);
    tmpY := IToY(tmpI);
    tmpStartX := 3;
    tmpStartY := 3;
    tmpStartLenX := 5;
    tmpStartLenY := 5;
    with FPaintBox do
    begin
      if Turn = WHITE then
        Canvas.Pen.Color := COLOR_AVAILABLEMOVE_WHITE
      else
        Canvas.Pen.Color := COLOR_AVAILABLEMOVE_BLACK;
      Canvas.Pen.Width := 1;
      Canvas.MoveTo(tmpX + tmpStartX, tmpY + tmpStartY);
      Canvas.LineTo(tmpX + tmpStartX, tmpY + tmpStartY + tmpStartLenY);
      Canvas.MoveTo(tmpX + tmpStartX, tmpY + tmpStartY);
      Canvas.LineTo(tmpX + tmpStartX + tmpStartLenX, tmpY + tmpStartY);
      Canvas.MoveTo(tmpX + tmpStartX, tmpY + FLenY - tmpStartY);
      Canvas.LineTo(tmpX + tmpStartX, tmpY + FLenY - tmpStartY - tmpStartLenY);
      Canvas.MoveTo(tmpX + tmpStartX, tmpY + FLenY - tmpStartY);
      Canvas.LineTo(tmpX + tmpStartX + tmpStartLenX, tmpY + FLenY - tmpStartY);
      Canvas.MoveTo(tmpX + FLenX - tmpStartX, tmpY + tmpStartY);
      Canvas.LineTo(tmpX + FLenX - tmpStartX - tmpStartLenX, tmpY + tmpStartY);
      Canvas.MoveTo(tmpX + FLenX - tmpStartX, tmpY + tmpStartY);
      Canvas.LineTo(tmpX + FLenX - tmpStartX, tmpY + tmpStartY + tmpStartLenY);
      Canvas.MoveTo(tmpX + FLenX - tmpStartX, tmpY + FLenY - tmpStartY);
      Canvas.LineTo(tmpX + FLenX - tmpStartX - tmpStartLenX, tmpY + FLenY - tmpStartY);
      Canvas.MoveTo(tmpX + FLenX - tmpStartX, tmpY + FLenY - tmpStartY);
      Canvas.LineTo(tmpX + FLenX - tmpStartX, tmpY + FLenY - tmpStartY - tmpStartLenY);
    end;
  end;
end;

procedure TWuziChessGame.DrawBoard;
var
  tmpX, tmpY, i: Integer;
begin
  inherited;
  if Self.IsTempGame then Exit;

  tmpX := FPaintBox.Width;
  tmpY := FPaintBox.Height;
  with FPaintBox do
  begin
    Canvas.Brush.Color := FBackgroundColor;
    Canvas.Pen.Color := FGridColor;
    Canvas.Pen.Width := 1;
    Canvas.Rectangle(0, 0, tmpX, tmpY);
    for i := 0 to Self.FSize - 1 do
    begin
      Canvas.MoveTo(0, FLenY * i);
      Canvas.LineTo(tmpX, FLenY * i);
      Canvas.MoveTo(FLenX * i, 0);
      Canvas.LineTo(FLenX * i, tmpY);
    end;
  end;
end;


procedure TWuziChessGame.DrawLastMove;
var
  tmpX, tmpY, tmpStartX, tmpStartY, tmpStartLenX, tmpStartLenY: Integer;
begin
  if Self.IsTempGame then Exit;
  DrawPiece(GetPiece(FOldLastMove.x, FOldLastMove.y), FOldLastMove.x, FOldLastMove.y);
  if not IsPlaying then exit;
  tmpX := JToX(FLastMove.y);
  tmpY := IToY(FLastMove.x);
  tmpStartX := 3;
  tmpStartY := 3;
  tmpStartLenX := 5;
  tmpStartLenY := 5;
  with FPaintBox do
  begin
    Canvas.Pen.Color := clRed;
    Canvas.Pen.Width := 3;
    Canvas.MoveTo(tmpX + tmpStartX, tmpY + tmpStartY);
    Canvas.LineTo(tmpX + tmpStartX, tmpY + tmpStartY + tmpStartLenY);
    Canvas.MoveTo(tmpX + tmpStartX, tmpY + tmpStartY);
    Canvas.LineTo(tmpX + tmpStartX + tmpStartLenX, tmpY + tmpStartY);
    Canvas.MoveTo(tmpX + tmpStartX, tmpY + FLenY - tmpStartY);
    Canvas.LineTo(tmpX + tmpStartX, tmpY + FLenY - tmpStartY - tmpStartLenY);
    Canvas.MoveTo(tmpX + tmpStartX, tmpY + FLenY - tmpStartY);
    Canvas.LineTo(tmpX + tmpStartX + tmpStartLenX, tmpY + FLenY - tmpStartY);
    Canvas.MoveTo(tmpX + FLenX - tmpStartX, tmpY + tmpStartY);
    Canvas.LineTo(tmpX + FLenX - tmpStartX - tmpStartLenX, tmpY + tmpStartY);
    Canvas.MoveTo(tmpX + FLenX - tmpStartX, tmpY + tmpStartY);
    Canvas.LineTo(tmpX + FLenX - tmpStartX, tmpY + tmpStartY + tmpStartLenY);
    Canvas.MoveTo(tmpX + FLenX - tmpStartX, tmpY + FLenY - tmpStartY);
    Canvas.LineTo(tmpX + FLenX - tmpStartX - tmpStartLenX, tmpY + FLenY - tmpStartY);
    Canvas.MoveTo(tmpX + FLenX - tmpStartX, tmpY + FLenY - tmpStartY);
    Canvas.LineTo(tmpX + FLenX - tmpStartX, tmpY + FLenY - tmpStartY - tmpStartLenY);
  end;
end;

procedure TWuziChessGame.DrawPiece(piece: TPiece; i, j: integer);
var
  tmpX, tmpY: Integer;
begin
  if Self.IsTempGame then Exit;
  with FPaintBox do
  begin
    Canvas.Pen.Color := FGridColor;
    Canvas.Pen.Width := 1;
    Canvas.Brush.Color := FBackgroundColor;
    tmpX := JToX(j);
    tmpY := IToY(i);
    FLenX := FPaintBox.Width div FSize;
    FLenY := FPaintBox.Height div FSize;
    Canvas.Rectangle(tmpX, tmpY, tmpX + FLenX, tmpY + FLenY);
    if FBoard[i, j] <> PIECE_BLANK then
    begin
      if FBoard[i, j] = PIECE_WHITE then
        Canvas.Brush.Color := FWhiteColor
      else if FBoard[i, j] = PIECE_BLACK then
        Canvas.Brush.Color := FBlackColor;
      Canvas.Pen.Color := Canvas.Brush.Color;
      Canvas.Pen.Width := 1;
      tmpX := JToX(j);
      tmpY := IToY(i);
      Canvas.Ellipse(tmpX + 5, tmpY + 5, tmpX + FLenX - 5, tmpY + FLenY - 5);
      if (i = FLastMove.x) and (j = FLastMove.y) then
        DrawLastMove();
    end;
  end;
end;

function TWuziChessGame.GetAllAvailableMove(var data: TListOfPoints;
  piece: TPiece): Integer;
var
  i, j: integer;
  tmpPoint: TPoint;
begin
  data := TListOfPoints.Create;
  data.Clear;
  for i := 0 to FSize - 1 do
    for j := 0 to FSize - 1 do
    begin
      if FBoard[i, j] = PIECE_BLANK then
      begin
        if IsAvailableMove(i, j, piece) then
        begin
          tmpPoint.x := i;
          tmpPoint.y := j;
          data.Add(tmpPoint);
        end;
      end;
    end;
  result := data.Count;
end;

function TWuziChessGame.GetPieceFromTurn(turn: TTurn): TPiece;
begin
  if turn = WHITE then
    result := PIECE_WHITE
  else
    result := PIECE_BLACK;
end;

function TWuziChessGame.GoToLevel(var x, y: Integer;
  piece: TPiece): boolean;
var
  k, k1, k2, k3, k4, k5, k6: Integer;
  tmpNumber, tmpNumber1, tmpNumber2, tmpNumber3, tmpNumber4, tmpNumber5, tmpNumber6: Integer;
  tmpData, tmpData1, tmpData2, tmpData3, tmpData4, tmpData5, tmpData6: TListOfPoints;
  tmpGame, tmpGame1, tmpGame2, tmpGame3, tmpGame4, tmpGame5, tmpGame6: TWuziChessGame;

  tmpCurrentNode: TStateNode;
  tmpPoint: TPoint;
  tmpHead: TBoardGame;
begin
  //create the State Tree and save all possible chess state into the tree
  OutputDebugString('Analyzing by loop...');

  tmpHead := TWuziChessGame.Create(Self);
  GStateTree := TWuziChessStateTree.Create(tmpHead);
  GStateTree.ListBox := Self.ListBox;
  tmpCurrentNode := GStateTree.Head;

  //get all possible moves for piece.
  tmpNumber := GetAllAvailableMove(tmpData, piece);

  if tmpNumber > 0 then
  begin
    FProgressBar.Min := 0;
    FProgressBar.Max := tmpNumber - 1;
    FProgressBar.Position := 0;
    FProgressBar.Visible := True;
    for k := 0 to tmpNumber - 1 do
    begin
      FProgressBar.Position := k;
      Application.ProcessMessages;
      tmpGame := TWuziChessGame.Create(Self);
      tmpGame.IsTempGame := True;
      tmpPoint := tmpData[k];
      tmpGame.PlayAtMove(tmpPoint.x, tmpPoint.y, piece);
      tmpCurrentNode := GStateTree.InsertTheNode(tmpGame, tmpPoint.x, tmpPoint.y, tmpCurrentNode);

      //==========Level 1=============
      //get all possible moves for the opponent of piece.
      tmpNumber1 := tmpGame.GetAllAvailableMove(tmpData1, TWuziChessGame.GetOpponent(piece));
      if tmpNumber1 > 0 then
      begin
        for k1 := 0 to tmpNumber1 - 1 do
        begin
          tmpGame1 := TWuziChessGame.Create(tmpGame);
          tmpGame1.IsTempGame := True;
          tmpPoint := tmpData1[k1];
          tmpGame1.PlayAtMove(tmpPoint.x, tmpPoint.y, TWuziChessGame.GetOpponent(piece
          ));
          tmpPoint := tmpData[k];
          tmpCurrentNode := GStateTree.InsertTheNode(tmpGame1, tmpPoint.x, tmpPoint.y, tmpCurrentNode);

          //get all possible moves for the piece
          tmpNumber2 := tmpGame1.GetAllAvailableMove(tmpData2, piece);
          if tmpNumber2 > 0 then
          begin
            for k2 := 0 to tmpNumber2 - 1 do
            begin
              tmpGame2 := TWuziChessGame.Create(tmpGame1);
              tmpGame2.IsTempGame := True;
              tmpPoint := tmpData2[k2];
              tmpGame2.PlayAtMove(tmpPoint.X, tmpPoint.Y, piece);
              tmpPoint := tmpData[k];
              tmpCurrentNode := GStateTree.InsertTheNode(tmpGame2, tmpPoint.X, tmpPoint.Y, tmpCurrentNode);

              //==============Level 2===============

              //get all possible moves for the opponent of piece
              tmpNumber3 := tmpGame2.GetAllAvailableMove(tmpData3, TWuziChessGame.GetOpponent(piece
              ));
              if tmpNumber3 > 0 then
              begin
                for k3 := 0 to tmpNumber3 - 1 do
                begin
                  tmpGame3 := TWuziChessGame.Create(tmpGame2);
                  tmpGame3.IsTempGame := True;
                  tmpPoint := tmpData3[k3];
                  tmpGame3.PlayAtMove(tmpPoint.X, tmpPoint.Y, TWuziChessGame.GetOpponent(piece));
                  tmpPoint := tmpData[k];
                  tmpCurrentNode := GStateTree.InsertTheNode(tmpGame3, tmpPoint.X, tmpPoint.Y, tmpCurrentNode);

                  //get all possible moves for piece
                  tmpNumber4 := tmpGame3.GetAllAvailableMove(tmpData4, piece);
                  if tmpNumber4 > 0 then
                  begin
                    for k4 := 0 to tmpNumber4 - 1 do
                    begin
                      tmpGame4 := TWuziChessGame.Create(tmpGame3);
                      tmpGame4.IsTempGame := True;
                      tmpPoint := tmpData4[k4];
                      tmpGame4.PlayAtMove(tmpPoint.X, tmpPoint.Y, piece);
                      tmpPoint := tmpData[k];
                      tmpCurrentNode := GStateTree.InsertTheNode(tmpGame4, tmpPoint.X, tmpPoint.Y, tmpCurrentNode);
                      {
                      //================Level 3 begin================
                      //get all possible moves for the opponent of piece
                      tmpNumber5 := tmpGame4.GetAllAvailableMove(tmpData5, TBlackWhiteGame.GetOpponent(piece));
                      if tmpNumber5 > 0 then
                      begin
                        for k5 := 0 to tmpNumber5 - 1 do
                        begin
                          tmpGame5 := TWuziChessGame.Create(tmpGame4);
                          tmpGame5.IsTempGame := True;
                          tmpPoint := tmpData5[k5];
                          tmpGame5.PlayAtMove(tmpPoint.X, tmpPoint.Y, TBlackWhiteGame.GetOpponent(piece));
                          tmpPoint := tmpData[k];
                          tmpCurrentNode := GStateTree.InsertTheNode(tmpGame5, tmpPoint.X, tmpPoint.Y, tmpCurrentNode);

                          //get all possible moves for piece
                          tmpNumber6 := tmpGame5.GetAllAvailableMove(tmpData6, piece);
                          if tmpNumber6 > 0 then
                          begin
                            for k6 := 0 to tmpNumber6 - 1 do
                            begin
                              tmpGame6 := TWuziChessGame.Create(tmpGame5);
                              tmpGame6.IsTempGame := True;
                              tmpPoint := tmpData6[k6];
                              tmpGame6.PlayAtMove(tmpPoint.X, tmpPoint.Y, piece);
                              tmpPoint := tmpData[k];
                              tmpCurrentNode := GStateTree.InsertTheNode(tmpGame6, tmpPoint.X, tmpPoint.Y, tmpCurrentNode);
                              tmpCurrentNode := tmpCurrentNode.parentNode;
                            end;
                          end;
                          FreeAndNil(tmpData6);
                          tmpCurrentNode := tmpCurrentNode.parentNode;
                        end;
                      end;
                      FreeAndNil(tmpData5);
                      //==============Level 3 end============
                      }
                      tmpCurrentNode := tmpCurrentNode.parentNode;
                    end;
                  end;
                  FreeAndNil(tmpData4);
                  tmpCurrentNode := tmpCurrentNode.parentNode;
                end;
              end;
              FreeAndNil(tmpData3);
              //================Level 2 end================
              tmpCurrentNode := tmpCurrentNode.parentNode;
            end;
          end;
          FreeAndNil(tmpData2);
          tmpCurrentNode := tmpCurrentNode.parentNode;
        end;
      end;
      FreeAndNil(tmpData1);
      //==================Level 1 end=================
      tmpCurrentNode := tmpCurrentNode.parentNode;
    end;

    //return a optimal result by analyzing the state tree.
    tmpPoint := Self.AnalyzeTheStateTree(GStateTree, Self.Turn);
    x := tmpPoint.X;
    y := tmpPoint.Y;
    OutputDebugString(PChar(Format('Choose: %d, %d', [x + 1, y + 1])));
    Result := true;
    FreeAndNil(tmpData);
    GStateTree.Print;
    FreeAndNil(GStateTree);
    FProgressBar.Visible := False;
    exit;
  end else
  begin
    result := False;
    FreeAndNil(tmpData);
    GStateTree.Print;
    FreeAndNil(GStateTree);
    FProgressBar.Visible := False;
  end;
end;

procedure TWuziChessGame.LoadGame;
begin
  inherited;

end;

procedure TWuziChessGame.NewGame;
var
  i, j: integer;
begin
  inherited;
  for i := 0 to FSize - 1 do
    for j := 0 to FSize - 1 do
      FBoard[i, j] := PIECE_BLANK;
  Self.PlayAtMove(4, 4, PIECE_WHITE);
  Self.PlayAtMove(4, 5, PIECE_BLACK);
  Self.PlayAtMove(5, 5, PIECE_WHITE);
  Self.PlayAtMove(5, 4, PIECE_BLACK);
  Self.Turn := WHITE;
  Self.IsPlaying := True;
  Self.DrawAllAvailableMoves(WHITE);
end;

procedure TWuziChessGame.NextLevel(const totalLevel: Integer;
  piece: TPiece; const step: TPoint; gameOld: TWuziChessGame;
  var stateTree: TStateTree; var currentNode: TStateNode);
var
  tmpNumberOld, tmpNumberNew: Integer;
  kOld, kNew: Integer;
  tmpGameOld, tmpGameNew: TWuziChessGame;
  tmpDataOld, tmpDataNew: TListOfPoints;
  tmpPoint: TPoint;
begin
  if TStateTree.getLevel(currentNode) >= 2 * totalLevel then
    exit;

  tmpGameOld := gameOld;
  //get all possible moves for the opponent of piece
  tmpNumberOld := tmpGameOld.GetAllAvailableMove(tmpDataOld, TWuziChessGame.GetOpponent(piece));
  if tmpNumberOld > 0 then
  begin
    for kOld := 0 to tmpNumberOld - 1 do
    begin
      tmpGameNew := TWuziChessGame.Create(tmpGameOld);
      tmpGameNew.IsTempGame := True;
      tmpPoint := tmpDataOld[kOld];
      tmpGameNew.PlayAtMove(tmpPoint.x, tmpPoint.y, TWuziChessGame.GetOpponent(piece));
      currentNode := GStateTree.InsertTheNode(tmpGameNew, tmpPoint.x, tmpPoint.y, currentNode);

      //get all possible moves for piece
      tmpNumberNew := tmpGameNew.GetAllAvailableMove(tmpDataNew, piece);
      if tmpNumberNew > 0 then
      begin
        for kNew := 0 to tmpNumberNew - 1 do
        begin
          tmpGameOld := TWuziChessGame.Create(tmpGameNew);
          tmpGameOld.IsTempGame := True;
          tmpPoint := tmpDataNew[kNew];
          tmpGameOld.PlayAtMove(tmpPoint.X, tmpPoint.Y, piece);
          tmpPoint := step;
//          self.Print(currentNode);
          currentNode := GStateTree.InsertTheNode(tmpGameOld, tmpPoint.X, tmpPoint.Y, currentNode);
//          self.Print(currentNode);
          if TStateTree.getLevel(currentNode) < 2 * totalLevel then
            self.NextLevel(totalLevel, piece, step, tmpGameOld, stateTree, currentNode);
          currentNode := currentNode.parentNode;
        end;
      end;
      FreeAndNil(tmpDataNew);

      currentNode := currentNode.parentNode;
    end;
  end;
  FreeAndNil(tmpDataOld);
end;

procedure TWuziChessGame.PlayAtMove(i, j: Integer; piece: TPiece);
begin
  SetPiece(piece, i, j);
  DrawPiece(piece, i, j);
  FOldLastMove := FLastMove;
  FLastMove.x := i;
  FLastMove.y := j;
  DrawLastMove();
  if piece = PIECE_BLACK then
    FLastTurn := BLACK
  else
    FLastTurn := WHITE;
end;

procedure TWuziChessGame.Print(node: TStateNode);
var
  tmpLevel: Integer;
begin
  tmpLevel := TStateTree.getLevel(node);
  OutputDebugString(PChar(Format('CurrentNode.level: %d', [tmpLevel])));
end;

procedure TWuziChessGame.Refresh;
var
  i, j: Integer;
begin
  Self.DrawBoard;
  for i := 0 to FSize - 1 do
    for j := 0 to FSize - 1 do
      DrawPiece(FBoard[i, j], i, j);
  DrawLastMove();
  DrawAllAvailableMoves(Self.Turn);
end;

procedure TWuziChessGame.ResetGame;
begin
  Self.CloseGame;
  Self.NewGame;
  Self.Refresh;
end;

procedure TWuziChessGame.SaveGame;
begin
  inherited;

end;

procedure TWuziChessGame.SetIsPlaying(const Value: Boolean);
begin
  inherited;
  DrawAllAvailableMoves(Turn);
  DrawLastMove();
  Application.ProcessMessages;
end;

procedure TWuziChessGame.Swap;
begin
  inherited;

end;

function TWuziChessGame.GetPiecesNumber(piece: TPiece): Integer;
var
  tmpCount, i, j: Integer;
begin
  tmpCount := 0;
  for i := 0 to FSize - 1 do
    for j := 0 to FSize - 1 do
    begin
      if FBoard[i, j] = piece then inc(tmpCount);
    end;
  result := tmpCount;
end;

//these two methods defines the rule of the game!!!!
function TWuziChessGame.TimeToCheck: Boolean;
var
  i, j, one, two, tmpI, tmpJ: integer;
  tmpChess: TPiece;
  tmpPos: TPoint;
begin
  result := false;
  //判断水平方向是否有连续五子
  one := 1;
  two := 0;
  tmpPos := Self.LastMove;
  OutputDebugString(PChar(Format('x: %d, y: %d', [tmpPos.X + 1, tmpPos.y + 1])));
  tmpI := tmpPos.x;
  tmpJ := tmpPos.y;
  tmpChess := Self.GetPiece(tmpI, tmpJ);
  for i := 1 to 4 do
  begin
    if (tmpI + i <= NUMBER - 1) and (Self.GetPiece(tmpI + i, tmpJ) = tmpChess) then
      inc(one)
    else
      break;
  end;
  for i := 1 to 4 do
  begin
    if (tmpI - i >= 0) and (Self.GetPiece(tmpI - i, tmpJ) = tmpChess) then
      inc(two)
    else
      break;
  end;
  if one + two >= 5 then
  begin
    result := true;
    OutputDebugString('5 pieces |');
    exit;
  end;
  //判断垂直方向是否有连续五子
  one := 1;
  two := 0;
  for j := 1 to 4 do
  begin
    if (tmpJ + j <= NUMBER - 1) and (Self.GetPiece(tmpI, tmpJ + j) = tmpChess) then
      inc(one)
    else
      break;
  end;
  for j := 1 to 4 do
  begin
    if (tmpJ - j >= 0) and (Self.GetPiece(tmpI, tmpJ - j) = tmpChess) then
      inc(two)
    else
      break;
  end;
  if one + two >= 5 then
  begin
    result := true;
    OutputDebugString('5 pieces -');
    exit;
  end;
  //判断斜方向是否有连续五子
  one := 1;
  two := 0;
  for i := 1 to 4 do
  begin
    if (tmpI + i <= NUMBER - 1) and (tmpJ + i <= NUMBER - 1) and (Self.GetPiece(tmpI + i, tmpJ + i) = tmpChess) then
      inc(one)
    else
      break;
  end;
  for i := 1 to 4 do
  begin
    if (tmpI - i >= 0) and (tmpJ - i >= 0) and (Self.GetPiece(tmpI - i, tmpJ - i) = tmpChess) then
      inc(two)
    else
      break;
  end;
  if one + two >= 5 then
  begin
    result := true;
    OutputDebugString('5 pieces \');
    exit;
  end;
  //判断反斜方向是否有连续五子
  one := 1;
  two := 0;
  for i := 1 to 4 do
  begin
    if (tmpI - i >= 0) and (tmpJ + i <= NUMBER - 1) and (Self.GetPiece(tmpI - i, tmpJ + i) = tmpChess) then
      inc(one)
    else
      break;
  end;
  for i := 1 to 4 do
  begin
    if (tmpI + i <= NUMBER - 1) and (tmpJ - i >= 0) and (Self.GetPiece(tmpI + i, tmpJ - i) = tmpChess) then
      inc(two)
    else
      break;
  end;
  if one + two >= 5 then
  begin
    result := true;
    OutputDebugString('5 pieces /');
    exit;
  end;
end;

//these two methods defines the rule of the game!!!!
function TWuziChessGame.IsAvailableMove(i, j: Integer;
  piece: TPiece): Boolean;
begin
  Result := False;
  if Self.GetPiece(i, j) = PIECE_BLANK then
    Result := True;
end;

procedure TWuziChessGame.EndGameAndPrint;
begin
  inherited;
  if Self.LastTurn = BLACK then
  begin
    MessageBox(Application.Handle, PChar('Black Win!'), 'Result', MB_OK);
  end else begin
    MessageBox(Application.Handle, PChar('White Win!'), 'Result', MB_OK);
  end;
  IsPlaying := False;
  DrawLastMove();
  DrawAllAvailableMoves(Turn);
end;

end.
