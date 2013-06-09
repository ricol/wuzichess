unit UnitTWuziChessStateTree;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, ExtCtrls, ToolWin, StdCtrls, UnitCommon, UnitTBoardGame, UnitTStateNode, UnitTStateTree, Generics.Collections;

type
  TWuziChessStateTree = class(TStateTree)
  protected
    procedure PrintNode(node: TStateNode; var list: TStringList); override;
  end;

implementation

uses
  UnitTWuziChessGame;

{ TWuziChessStateTree }

procedure TWuziChessStateTree.PrintNode(node: TStateNode;
  var list: TStringList);
var
  tmpString, tmpStringTurn: String;
  tmpLevel: Integer;
  i: Integer;
  tmpGame: TWuziChessGame;
  tmpLastMove: TPoint;
  tmpNumberOfBlack, tmpNumberOfBlank, tmpNumberOfWhite, tmpResult: Integer;
begin
  inherited;
  if node <> nil then
  begin
    tmpString := '|';
    tmpLevel := getLevel(node);
    tmpGame := TWuziChessGame(node.data);
    tmpNumberOfBlack := tmpGame.GetPiecesNumber(PIECE_BLACK);
    tmpNumberOfWhite := tmpGame.GetPiecesNumber(PIECE_WHITE);
    tmpNumberOfBlank := tmpGame.GetPiecesNumber(PIECE_BLANK);
    tmpResult := tmpNumberOfBlack - tmpNumberOfWhite;
    tmpLastMove := tmpGame.LastMove;
    for i := 0 to tmpLevel - 1 do
    begin
      tmpString := tmpString + '-----|';
    end;
    if tmpGame.LastTurn = BLACK then
    begin
      tmpStringTurn := 'BLACK';
    end
    else
    begin
      tmpStringTurn := 'WHITE';
      tmpResult := tmpResult * -1;
    end;
    list.Add(tmpString + Format(' [%x] %s Played at (%d, %d) -> (White: %d, Black: %d, Blank: %d) - (Result: %d)',
          [Integer(node), tmpStringTurn, tmpLastMove.X + 1, tmpLastMove.Y + 1, tmpNumberOfWhite, tmpNumberOfBlack, tmpNumberOfBlank, tmpResult]));
  end;
end;

end.