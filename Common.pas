unit Common;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, ExtCtrls, ToolWin, Generics.Collections;

const
  NUMBER_X = 9;
  NUMBER_Y = 9;
  COLOR_AVAILABLEMOVE_WHITE = clWhite;
  COLOR_AVAILABLEMOVE_BLACK = clBlack;
  COLOR_GRID = clBlack;
  COLOR_BACKGROUND = clGreen;
  COLOR_WHITE = clWhite;
  COLOR_BLACK = clBlack;
  DELAYTIME = 0;

var
  GShowStateTreeInfor: Boolean = False;
  GComputerCalculateLevel: Integer = 1;

type
  TWay = (LOOP, RECURSIVE);
  TListOfPoints = TList<TPoint>;
  TDifficulties = (DIFF_LOW, DIFF_MEDIUM, DIFF_HARD);
  TTurn = (WHITE, BLACK);
  TPiece = (PIECE_WHITE, PIECE_BLACK, PIECE_BLANK);
  TBoard = array of array of TPiece;

implementation

end.
