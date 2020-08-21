program WuziChess;

uses
  Forms,
  fMain in 'fMain.pas' {FormMain},
  Common in 'Common.pas',
  BoardGame in 'BoardGame.pas',
  StateTree in 'StateTree.pas',
  StateNode in 'StateNode.pas',
  FormData in 'FormData.pas' {FormStateTreeData},
  WuziChessStateTree in 'WuziChessStateTree.pas',
  WuziChessGame in 'WuziChessGame.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'WuziChess';
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormStateTreeData, FormStateTreeData);
  Application.Run;
end.
