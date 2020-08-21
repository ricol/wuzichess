unit WuziChessStateTree;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, ExtCtrls, ToolWin, StdCtrls, Common, BoardGame, StateNode, StateTree, Generics.Collections;

type
  TWuziChessStateTree = class(TStateTree)
  protected
    procedure PrintNode(node: TStateNode; var list: TStringList); override;
  end;

implementation

uses
  WuziChessGame;

{ TWuziChessStateTree }

procedure TWuziChessStateTree.PrintNode(node: TStateNode;
  var list: TStringList);
var
  str, strTurn: String;
  lvl: Integer;
  i: Integer;
  game: TWuziChessGame;
  lastMove: TPoint;
  blackCount, blankCount, whiteCount, res: Integer;
begin
  inherited;
  if node <> nil then
  begin
    str := '|';
    lvl := getLevel(node);
    game := TWuziChessGame(node.data);
    blackCount := game.GetPiecesNumber(PIECE_BLACK);
    whiteCount := game.GetPiecesNumber(PIECE_WHITE);
    blankCount := game.GetPiecesNumber(PIECE_BLANK);
    res := blackCount - whiteCount;
    lastMove := game.LastMove;
    for i := 0 to lvl - 1 do
    begin
      str := str + '-----|';
    end;
    if game.LastTurn = BLACK then
    begin
      strTurn := 'BLACK';
    end
    else
    begin
      strTurn := 'WHITE';
      res := res * -1;
    end;
    list.Add(str + Format(' [%x] %s Played at (%d, %d) -> (White: %d, Black: %d, Blank: %d) - (Result: %d)',
          [Integer(node), strTurn, lastMove.X + 1, lastMove.Y + 1, whiteCount, blackCount, blankCount, res]));
  end;
end;

end.