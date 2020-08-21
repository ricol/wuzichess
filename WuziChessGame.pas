unit WuziChessGame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, ExtCtrls, StdCtrls, ToolWin, Common, BoardGame, StateNode, StateTree;

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
    constructor Create(PaintBox: TPaintBox; sizeX: Integer; sizeY: Integer; TempObject: Boolean); overload; override;
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
  WuziChessStateTree;

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
  tree: TStateTree;
  allLeaves: TListOfNodes;
  currentNode: TStateNode;
  currentGame: TWuziChessGame;
  max, k, l, blackNum, whiteNum, res: Integer;
  point: TPoint;
  i, j: Integer;
begin
  //analyze the state tree and find the optimum node
  tree := stateTree;
  allLeaves := tree.getAllLeaves(True);

  max := 0;
  l := 0;
  OutputDebugString(PChar(Format('Analyzing...Total Leaves: %d', [allLeaves.Count])));

  repeat
    i := random(NUMBER_X);
    j := random(NUMBER_Y);
  until self.GetPiece(i, j) = PIECE_BLANK;
  Result.X := i;
  Result.Y := j;
  exit;

  if turn = WHITE then
  begin
    for k := 0 to allLeaves.Count - 1 do
    begin
      currentNode := allLeaves[k];

      currentGame := TWuziChessGame(currentNode.data);

      blackNum := currentGame.GetPiecesNumber(PIECE_BLACK);
      whiteNum := currentGame.GetPiecesNumber(PIECE_WHITE);
      res := whiteNum - blackNum;

      if blackNum <= 0 then
      begin
        l := k;
        OutputDebugString(PChar(Format('#######: Found The Optimal Result! Level %d', [TStateTree.getLevel(currentNode)])));
        OutputDebugString(PChar(Format('tmpResult: %d', [res])));
        Beep;
        break;
      end;

      if res >= max then
      begin
        l := k;
        max := res;
        OutputDebugString(PChar(Format('tmpMax: %d', [max])));
      end;
    end;
  end else begin
    for k := 0 to allLeaves.Count - 1 do
    begin
      currentNode := allLeaves[k];
      currentGame := TWuziChessGame(currentNode.data);

      blackNum := currentGame.GetPiecesNumber(PIECE_BLACK);
      whiteNum := currentGame.GetPiecesNumber(PIECE_WHITE);
      res := blackNum - whiteNum;

      if whiteNum <= 0 then
      begin
        l := k;
        OutputDebugString(PChar(Format('#######: Found The Optimal Result! Level %d', [TStateTree.getLevel(currentNode)])));
        OutputDebugString(PChar(Format('tmpResult: %d', [res])));
        Beep;
        break;
      end;

      if res >= max then
      begin
        l := k;
        max := res;
        OutputDebugString(PChar(Format('tmpMax: %d', [max])));
      end;
    end;
  end;

  currentNode := allLeaves[l];
  point.X := currentNode.step_i;
  point.Y := currentNode.step_j;
  FreeAndNil(allLeaves);

  Result := point;
end;


function TWuziChessGame.AnalyzeToLevel(level: Integer; var x, y: Integer;
  piece: TPiece): boolean;
var
  k: Integer;
  num: Integer;
  points: TListOfPoints;
  game: TWuziChessGame;

  currentNode: TStateNode;
  point: TPoint;
  head: TBoardGame;
begin
  //create the State Tree and save all possible chess state into the tree
  OutputDebugString('Analyzing by recursive...');

  head := TWuziChessGame.Create(Self);
  GStateTree := TWuziChessStateTree.Create(head);
  GStateTree.ListBox := Self.ListBox;
  currentNode := GStateTree.Head;

  //get all possible moves for piece.
  num := GetAllAvailableMove(points, piece);

  if num > 0 then
  begin
    FProgressBar.Min := 0;
    FProgressBar.Max := num - 1;
    FProgressBar.Position := 0;
    FProgressBar.Visible := True;
    for k := 0 to num - 1 do
    begin
      FProgressBar.Position := k;
      Application.ProcessMessages;
      game := TWuziChessGame.Create(Self);
      game.IsTempGame := True;
      point := points[k];
      game.PlayAtMove(point.x, point.y, piece);
      currentNode := GStateTree.InsertTheNode(game, point.x, point.y, currentNode);

      self.NextLevel(level, piece, point, game, GStateTree, currentNode);

      currentNode := currentNode.parentNode;
    end;
    //return a optimal result by analyzing the state tree.
    point := Self.AnalyzeTheStateTree(GStateTree, Self.Turn);
    x := point.X;
    y := point.Y;
    OutputDebugString(PChar(Format('Choose: %d, %d', [x + 1, y + 1])));
    Result := true;
    FreeAndNil(points);
    GStateTree.Print;
    FreeAndNil(GStateTree);
    FProgressBar.Visible := False;
    exit;
  end else
  begin
    result := False;
    FreeAndNil(points);
    GStateTree.Print;
    FreeAndNil(GStateTree);
    FProgressBar.Visible := False;
  end;
end;

function TWuziChessGame.AutoPlay(var i, j: Integer; piece: TPiece;
  way: TWay): Boolean;
begin
  if way = LOOP then
    Result := self.GoToLevel(i, j, piece)
  else
    Result := self.AnalyzeToLevel(GComputerCalculateLevel, i, j, piece);
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
  game: TWuziChessGame;
begin
  inherited Create(wuziChess);
  if wuziChess is TWuziChessGame then
  begin
    game := TWuziChessGame(wuziChess);
    FGridColor := game.FGridColor;
    FBlackColor := game.FBlackColor;
    FWhiteColor := game.FWhiteColor;
    FTurn := game.FTurn;
    FLastMove := game.FLastMove;
    FOldLastMove := game.FOldLastMove;
    FOldAvailableMoveNumber := game.FOldAvailableMoveNumber;
    if not FIsTempGame then
    begin
      FOldAvailableMoveData := TListOfPoints.Create(game.FOldAvailableMoveData);
      FAvailableMoveData := TListOfPoints.Create(game.FAvailableMoveData);
    end;
    FProgressBar := game.FProgressBar;
    FListBox := game.FListBox;
  end else
    OutputDebugString('Error!');
end;


constructor TWuziChessGame.Create(PaintBox: TPaintBox; sizeX: Integer; sizeY: Integer;
  TempObject: Boolean);
var
  i, j: integer;
begin
  inherited Create(PaintBox, sizeX, sizeY, TempObject);
  for i := 0 to sizeX - 1 do
    for j := 0 to sizeY - 1 do
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
  i, j, k: Integer;
  point: TPoint;
  x, y, startX, startY, startLenX, startLenY: Double;
begin
  if Self.IsTempGame then Exit;
  for k := 0 to FOldAvailableMoveNumber - 1 do
  begin
    point := FOldAvailableMoveData[k];
    i := point.x;
    j := point.y;
    DrawPiece(GetPiece(i, j), i, j);
  end;
  if not IsPlaying then exit;
  for k := 0 to FAvailableMoveNumber - 1 do
  begin
    point := FAvailableMoveData[k];
    i := point.x;
    j := point.y;
    x := IToX(i);
    y := JToY(j);
    startX := 3;
    startY := 3;
    startLenX := 5;
    startLenY := 5;
    with FPaintBox do
    begin
      if Turn = WHITE then
        Canvas.Pen.Color := COLOR_AVAILABLEMOVE_WHITE
      else
        Canvas.Pen.Color := COLOR_AVAILABLEMOVE_BLACK;
      Canvas.Pen.Width := 1;
      Canvas.MoveTo(Trunc(x + startX), Trunc(y + startY));
      Canvas.LineTo(Trunc(x + startX), Trunc(y + startY + startLenY));
      Canvas.MoveTo(Trunc(x + startX), Trunc(y + startY));
      Canvas.LineTo(Trunc(x + startX + startLenX), Trunc(y + startY));
      Canvas.MoveTo(Trunc(x + startX), Trunc(y + FLenY - startY));
      Canvas.LineTo(Trunc(x + startX), Trunc(y + FLenY - startY - startLenY));
      Canvas.MoveTo(Trunc(x + startX), Trunc(y + FLenY - startY));
      Canvas.LineTo(Trunc(x + startX + startLenX), Trunc(y + FLenY - startY));
      Canvas.MoveTo(Trunc(x + FLenX - startX), Trunc(y + startY));
      Canvas.LineTo(Trunc(x + FLenX - startX - startLenX), Trunc(y + startY));
      Canvas.MoveTo(Trunc(x + FLenX - startX), Trunc(y + startY));
      Canvas.LineTo(Trunc(x + FLenX - startX), Trunc(y + startY + startLenY));
      Canvas.MoveTo(Trunc(x + FLenX - startX), Trunc(y + FLenY - startY));
      Canvas.LineTo(Trunc(x + FLenX - startX - startLenX), Trunc(y + FLenY - startY));
      Canvas.MoveTo(Trunc(x + FLenX - startX), Trunc(y + FLenY - startY));
      Canvas.LineTo(Trunc(x + FLenX - startX), Trunc(y + FLenY - startY - startLenY));
    end;
  end;
end;

procedure TWuziChessGame.DrawBoard;
var
  x, y, i: Integer;
begin
  inherited;
  if Self.IsTempGame then Exit;

  x := FPaintBox.Width;
  y := FPaintBox.Height;
  with FPaintBox do
  begin
    Canvas.Brush.Color := FBackgroundColor;
    Canvas.Pen.Color := FGridColor;
    Canvas.Pen.Width := 1;
    Canvas.Rectangle(0, 0, x, y);
    for i := 0 to Self.FSizeX - 1 do
    begin
      Canvas.MoveTo(0, Trunc(FLenY * i));
      Canvas.LineTo(x, Trunc(FLenY * i));
      Canvas.MoveTo(Trunc(FLenX * i), 0);
      Canvas.LineTo(Trunc(FLenX * i), y);
    end;
  end;
end;


procedure TWuziChessGame.DrawLastMove;
var
  x, y, startX, startY, startLenX, startLenY: Double;
begin
  if Self.IsTempGame then Exit;
  DrawPiece(GetPiece(FOldLastMove.x, FOldLastMove.y), FOldLastMove.x, FOldLastMove.y);
  if not IsPlaying then exit;
  x := IToX(FLastMove.x);
  y := JToY(FLastMove.y);
  startX := 3;
  startY := 3;
  startLenX := 5;
  startLenY := 5;
  with FPaintBox do
  begin
    Canvas.Pen.Color := clRed;
    Canvas.Pen.Width := 3;
    Canvas.MoveTo(Trunc(x + startX), Trunc(y + startY));
    Canvas.LineTo(Trunc(x + startX), Trunc(y + startY + startLenY));
    Canvas.MoveTo(Trunc(x + startX), Trunc(y + startY));
    Canvas.LineTo(Trunc(x + startX + startLenX), Trunc(y + startY));
    Canvas.MoveTo(Trunc(x + startX), Trunc(y + FLenY - startY));
    Canvas.LineTo(Trunc(x + startX), Trunc(y + FLenY - startY - startLenY));
    Canvas.MoveTo(Trunc(x + startX), Trunc(y + FLenY - startY));
    Canvas.LineTo(Trunc(x + startX + startLenX), Trunc(y + FLenY - startY));
    Canvas.MoveTo(Trunc(x + FLenX - startX), Trunc(y + startY));
    Canvas.LineTo(Trunc(x + FLenX - startX - startLenX), Trunc(y + startY));
    Canvas.MoveTo(Trunc(x + FLenX - startX), Trunc(y + startY));
    Canvas.LineTo(Trunc(x + FLenX - startX), Trunc(y + startY + startLenY));
    Canvas.MoveTo(Trunc(x + FLenX - startX), Trunc(y + FLenY - startY));
    Canvas.LineTo(Trunc(x + FLenX - startX - startLenX), Trunc(y + FLenY - startY));
    Canvas.MoveTo(Trunc(x + FLenX - startX), Trunc(y + FLenY - startY));
    Canvas.LineTo(Trunc(x + FLenX - startX), Trunc(y + FLenY - startY - startLenY));
  end;
end;

procedure TWuziChessGame.DrawPiece(piece: TPiece; i, j: integer);
var
  x, y: Double;
begin
  if Self.IsTempGame then Exit;
  with FPaintBox do
  begin
    Canvas.Pen.Color := FGridColor;
    Canvas.Pen.Width := 1;
    Canvas.Brush.Color := FBackgroundColor;
    x := IToX(i);
    y := JToY(j);
    FLenX := FPaintBox.Width div FSizeX;
    FLenY := FPaintBox.Height div FSizeY;
    Canvas.Rectangle(Trunc(x), Trunc(y), Trunc(x + FLenX), Trunc(y + FLenY));
    if FBoard[i, j] <> PIECE_BLANK then
    begin
      if FBoard[i, j] = PIECE_WHITE then
        Canvas.Brush.Color := FWhiteColor
      else if FBoard[i, j] = PIECE_BLACK then
        Canvas.Brush.Color := FBlackColor;
      Canvas.Pen.Color := Canvas.Brush.Color;
      Canvas.Pen.Width := 1;
      x := IToX(i);
      y := JToY(j);
      Canvas.Ellipse(Trunc(x + 5), Trunc(y + 5), Trunc(x + FLenX - 5), Trunc(y + FLenY - 5));
      if (i = FLastMove.x) and (j = FLastMove.y) then
        DrawLastMove();
    end;
  end;
end;

function TWuziChessGame.GetAllAvailableMove(var data: TListOfPoints;
  piece: TPiece): Integer;
var
  i, j: integer;
  point: TPoint;
begin
  data := TListOfPoints.Create;
  data.Clear;
  for i := 0 to FSizeX - 1 do
    for j := 0 to FSizeY - 1 do
    begin
      if FBoard[i, j] = PIECE_BLANK then
      begin
        if IsAvailableMove(i, j, piece) then
        begin
          point.x := i;
          point.y := j;
          data.Add(point);
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
  number, number1, number2, number3, number4, number5, number6: Integer;
  data, data1, data2, data3, data4, data5, data6: TListOfPoints;
  game, game1, game2, game3, game4, game5, game6: TWuziChessGame;

  currentNode: TStateNode;
  point: TPoint;
  head: TBoardGame;
begin
  //create the State Tree and save all possible chess state into the tree
  OutputDebugString('Analyzing by loop...');

  head := TWuziChessGame.Create(Self);
  GStateTree := TWuziChessStateTree.Create(head);
  GStateTree.ListBox := Self.ListBox;
  currentNode := GStateTree.Head;

  //get all possible moves for piece.
  number := GetAllAvailableMove(data, piece);

  if number > 0 then
  begin
    FProgressBar.Min := 0;
    FProgressBar.Max := number - 1;
    FProgressBar.Position := 0;
    FProgressBar.Visible := True;
    for k := 0 to number - 1 do
    begin
      FProgressBar.Position := k;
      Application.ProcessMessages;
      game := TWuziChessGame.Create(Self);
      game.IsTempGame := True;
      point := data[k];
      game.PlayAtMove(point.x, point.y, piece);
      currentNode := GStateTree.InsertTheNode(game, point.x, point.y, currentNode);

      //==========Level 1=============
      //get all possible moves for the opponent of piece.
      number1 := game.GetAllAvailableMove(data1, TWuziChessGame.GetOpponent(piece));
      if number1 > 0 then
      begin
        for k1 := 0 to number1 - 1 do
        begin
          game1 := TWuziChessGame.Create(game);
          game1.IsTempGame := True;
          point := data1[k1];
          game1.PlayAtMove(point.x, point.y, TWuziChessGame.GetOpponent(piece
          ));
          point := data[k];
          currentNode := GStateTree.InsertTheNode(game1, point.x, point.y, currentNode);

          //get all possible moves for the piece
          number2 := game1.GetAllAvailableMove(data2, piece);
          if number2 > 0 then
          begin
            for k2 := 0 to number2 - 1 do
            begin
              game2 := TWuziChessGame.Create(game1);
              game2.IsTempGame := True;
              point := data2[k2];
              game2.PlayAtMove(point.X, point.Y, piece);
              point := data[k];
              currentNode := GStateTree.InsertTheNode(game2, point.X, point.Y, currentNode);

              //==============Level 2===============

              //get all possible moves for the opponent of piece
              number3 := game2.GetAllAvailableMove(data3, TWuziChessGame.GetOpponent(piece
              ));
              if number3 > 0 then
              begin
                for k3 := 0 to number3 - 1 do
                begin
                  game3 := TWuziChessGame.Create(game2);
                  game3.IsTempGame := True;
                  point := data3[k3];
                  game3.PlayAtMove(point.X, point.Y, TWuziChessGame.GetOpponent(piece));
                  point := data[k];
                  currentNode := GStateTree.InsertTheNode(game3, point.X, point.Y, currentNode);

                  //get all possible moves for piece
                  number4 := game3.GetAllAvailableMove(data4, piece);
                  if number4 > 0 then
                  begin
                    for k4 := 0 to number4 - 1 do
                    begin
                      game4 := TWuziChessGame.Create(game3);
                      game4.IsTempGame := True;
                      point := data4[k4];
                      game4.PlayAtMove(point.X, point.Y, piece);
                      point := data[k];
                      currentNode := GStateTree.InsertTheNode(game4, point.X, point.Y, currentNode);
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
                      currentNode := currentNode.parentNode;
                    end;
                  end;
                  FreeAndNil(data4);
                  currentNode := currentNode.parentNode;
                end;
              end;
              FreeAndNil(data3);
              //================Level 2 end================
              currentNode := currentNode.parentNode;
            end;
          end;
          FreeAndNil(data2);
          currentNode := currentNode.parentNode;
        end;
      end;
      FreeAndNil(data1);
      //==================Level 1 end=================
      currentNode := currentNode.parentNode;
    end;

    //return a optimal result by analyzing the state tree.
    point := Self.AnalyzeTheStateTree(GStateTree, Self.Turn);
    x := point.X;
    y := point.Y;
    OutputDebugString(PChar(Format('Choose: %d, %d', [x + 1, y + 1])));
    Result := true;
    FreeAndNil(data);
    GStateTree.Print;
    FreeAndNil(GStateTree);
    FProgressBar.Visible := False;
    exit;
  end else
  begin
    result := False;
    FreeAndNil(data);
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
  for i := 0 to FSizeX - 1 do
    for j := 0 to FSizeY - 1 do
      FBoard[i, j] := PIECE_BLANK;
//  Self.PlayAtMove(4, 4, PIECE_WHITE);
//  Self.PlayAtMove(4, 5, PIECE_BLACK);
//  Self.PlayAtMove(5, 5, PIECE_WHITE);
//  Self.PlayAtMove(5, 4, PIECE_BLACK);
  Self.Turn := WHITE;
  Self.IsPlaying := True;
  Self.DrawAllAvailableMoves(WHITE);
end;

procedure TWuziChessGame.NextLevel(const totalLevel: Integer;
  piece: TPiece; const step: TPoint; gameOld: TWuziChessGame;
  var stateTree: TStateTree; var currentNode: TStateNode);
var
  oldNum, newNum: Integer;
  kOld, kNew: Integer;
  oldGame, newGame: TWuziChessGame;
  oldData, newData: TListOfPoints;
  point: TPoint;
begin
  if TStateTree.getLevel(currentNode) >= 2 * totalLevel then
    exit;

  oldGame := gameOld;
  //get all possible moves for the opponent of piece
  oldNum := oldGame.GetAllAvailableMove(oldData, TWuziChessGame.GetOpponent(piece));
  if oldNum > 0 then
  begin
    for kOld := 0 to oldNum - 1 do
    begin
      newGame := TWuziChessGame.Create(oldGame);
      newGame.IsTempGame := True;
      point := oldData[kOld];
      newGame.PlayAtMove(point.x, point.y, TWuziChessGame.GetOpponent(piece));
      currentNode := GStateTree.InsertTheNode(newGame, point.x, point.y, currentNode);

      //get all possible moves for piece
      newNum := newGame.GetAllAvailableMove(newData, piece);
      if newNum > 0 then
      begin
        for kNew := 0 to newNum - 1 do
        begin
          oldGame := TWuziChessGame.Create(newGame);
          oldGame.IsTempGame := True;
          point := newData[kNew];
          oldGame.PlayAtMove(point.X, point.Y, piece);
          point := step;
//          self.Print(currentNode);
          currentNode := GStateTree.InsertTheNode(oldGame, point.X, point.Y, currentNode);
//          self.Print(currentNode);
          if TStateTree.getLevel(currentNode) < 2 * totalLevel then
            self.NextLevel(totalLevel, piece, step, oldGame, stateTree, currentNode);
          currentNode := currentNode.parentNode;
        end;
      end;
      FreeAndNil(newData);

      currentNode := currentNode.parentNode;
    end;
  end;
  FreeAndNil(oldData);
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
  lvl: Integer;
begin
  lvl := TStateTree.getLevel(node);
  OutputDebugString(PChar(Format('CurrentNode.level: %d', [lvl])));
end;

procedure TWuziChessGame.Refresh;
var
  i, j: Integer;
begin
  Self.DrawBoard;
  for i := 0 to FSizeX - 1 do
    for j := 0 to FSizeY - 1 do
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
  count, i, j: Integer;
begin
  count := 0;
  for i := 0 to FSizeX - 1 do
    for j := 0 to FSizeY - 1 do
    begin
      if FBoard[i, j] = piece then inc(count);
    end;
  result := count;
end;

//these two methods defines the rule of the game!!!!
function TWuziChessGame.TimeToCheck: Boolean;
var
  i, j, one, two, m, n: integer;
  chess: TPiece;
  pos: TPoint;
begin
  result := false;
  //判断水平方向是否有连续五子
  one := 1;
  two := 0;
  pos := Self.LastMove;
  OutputDebugString(PChar(Format('x: %d, y: %d', [pos.X + 1, pos.y + 1])));
  m := pos.x;
  n := pos.y;
  chess := Self.GetPiece(m, n);
  for i := 1 to 4 do
  begin
    if (m + i <= NUMBER_X - 1) and (Self.GetPiece(m + i, n) = chess) then
      inc(one)
    else
      break;
  end;
  for i := 1 to 4 do
  begin
    if (m - i >= 0) and (Self.GetPiece(m - i, n) = chess) then
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
    if (n + j <= NUMBER_Y - 1) and (Self.GetPiece(m, n + j) = chess) then
      inc(one)
    else
      break;
  end;
  for j := 1 to 4 do
  begin
    if (n - j >= 0) and (Self.GetPiece(m, n - j) = chess) then
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
    if (m + i <= NUMBER_X - 1) and (n + i <= NUMBER_Y - 1) and (Self.GetPiece(m + i, n + i) = chess) then
      inc(one)
    else
      break;
  end;
  for i := 1 to 4 do
  begin
    if (m - i >= 0) and (n - i >= 0) and (Self.GetPiece(m - i, n - i) = chess) then
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
    if (m - i >= 0) and (n + i <= NUMBER_Y - 1) and (Self.GetPiece(m - i, n + i) = chess) then
      inc(one)
    else
      break;
  end;
  for i := 1 to 4 do
  begin
    if (m + i <= NUMBER_X - 1) and (n - i >= 0) and (Self.GetPiece(m + i, n - i) = chess) then
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
