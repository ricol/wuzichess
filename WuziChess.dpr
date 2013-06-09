program WuziChess;

uses
  Forms,
  UnitMain in 'UnitMain.pas' {FormMain},
  UnitCommon in 'UnitCommon.pas',
  UnitTBoardGame in 'UnitTBoardGame.pas',
  UnitTStateTree in 'UnitTStateTree.pas',
  UnitTStateNode in 'UnitTStateNode.pas',
  UnitFormData in 'UnitFormData.pas' {FormStateTreeData},
  UnitTWuziChessStateTree in 'UnitTWuziChessStateTree.pas',
  UnitTWuziChessGame in 'UnitTWuziChessGame.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'WuziChess';
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormStateTreeData, FormStateTreeData);
  Application.Run;
end.
