unit UnitTStateTree;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Generics.Defaults, Generics.Collections,
  Dialogs, Menus, ComCtrls, ExtCtrls, ToolWin, StdCtrls, UnitCommon, UnitTBoardGame, UnitTStateNode;

type
  TStateTree = class abstract
  protected
    FListBox: TListBox;
    FCurrent: TStateNode;
    FHead: TStateNode;
    function getAllChildrenNodesForTheNode(node: TStateNode; LevelPriority: Boolean): TListOfNodes;
    procedure SetListBox(const Value: TListBox);
    procedure SetCurrent(const Value: TStateNode);
    procedure SetHead(const Value: TStateNode);
    procedure ReleaseNode(var node: TStateNode);
    procedure PrintNode(node: TStateNode; var list: TStringList); virtual; abstract;
    procedure PrintRecursively(node: TStateNode; var list: TStringList);
    function getLevelInfor(listOfNodes: TListOfNodes): String;
  public
    constructor Create(var initialGame: TBoardGame);
    destructor Destroy(); override;
    property ListBox: TListBox read FListBox write SetListBox;
    property Head: TStateNode read FHead write SetHead;
    property Current: TStateNode read FCurrent write SetCurrent;
    function getAllLeaves(LevelPriority: Boolean): TListOfNodes;
    function InsertTheNode(ValueForTheNode: TBoardGame; i, j: Integer; InsertUnderTheNode: TStateNode): TStateNode;
    function getAllNodes(LevelPriority: Boolean): TListOfNodes;
    procedure Print;
    class function getLevel(node: TStateNode): Integer;
  end;

implementation

{ TStateTree }

function CompareNodes(const node1, node2: TStateNode): Integer;
  var
  tmpLevel1, tmpLevel2: Integer;
begin
  tmpLevel1 := TStateTree.getLevel(node1);
  tmpLevel2 := TStateTree.getLevel(node2);
  result := tmpLevel1 - tmpLevel2;
end;

constructor TStateTree.Create(var initialGame: TBoardGame);
begin
  inherited Create();
  head := TStateNode.Create;
  head.data := initialGame;
  head.step_i := -1;
  head.step_j := -1;
  head.parentNode := nil;
  head.nextNodes := nil;
end;

destructor TStateTree.Destroy;
begin
  ReleaseNode(FHead);
  inherited;
end;

function TStateTree.getAllChildrenNodesForTheNode(node: TStateNode; LevelPriority: Boolean): TListOfNodes;
var
  i, j: Integer;
  tmpNextNodes: TListOfNodes;
  tmpArrayOfNodes: TListOfNodes;
  tmpList: TListOfNodes;
begin
  if node <> nil then
  begin
    if LevelPriority then
    begin
      Result := getAllChildrenNodesForTheNode(node, False);
      Result.Sort(TComparer<TStateNode>.Construct(CompareNodes));
    end else begin
      tmpNextNodes := node.nextNodes;
      if tmpNextNodes <> nil then
      begin
        tmpList := TList<TStateNode>.Create();
        tmpList.Clear;
        for i := 0 to tmpNextNodes.Count - 1 do
        begin
          tmpList.Add(tmpNextNodes[i]);  //add the current one to the list
          tmpArrayOfNodes := getAllChildrenNodesForTheNode(tmpNextNodes[i], LevelPriority);
          if tmpArrayOfNodes <> nil then
          begin
            //add all children nodes to the list
            for j := 0 to tmpArrayOfNodes.Count - 1 do
            begin
              tmpList.Add(tmpArrayOfNodes[j]);
            end;
            FreeAndNil(tmpArrayOfNodes);
          end;
        end;
        Result := tmpList;
        exit;
      end else begin
        result := TListOfNodes.Create;
        exit;
      end;
    end;
  end else begin
    result := TListOfNodes.Create;
    exit;
  end;
end;

function TStateTree.getAllLeaves(LevelPriority: Boolean): TListOfNodes;
var
  tmpArray_AllNodes: TListOfNodes;
  i: Integer;
  tmpNode: TStateNode;
  tmpList: TList<TStateNode>;
begin
  if head <> nil then
  begin
    tmpArray_AllNodes := getAllNodes(LevelPriority);

//    OutputDebugString(PChar(getLevelInfor(tmpArray_AllNodes)));

    tmpList := TList<TStateNode>.Create();
    for i := 0 to tmpArray_AllNodes.Count - 1 do
    begin
      tmpNode := tmpArray_AllNodes[i];
      if tmpNode.nextNodes = nil then
        tmpList.Add(tmpNode);
    end;

//    OutputDebugString(PChar(getLevelInfor(tmpList)));

    FreeAndNil(tmpArray_AllNodes);
    Result := tmpList;
    Exit;
  end else begin
    result := TListOfNodes.Create;
    exit;
  end;
end;

function TStateTree.getAllNodes(LevelPriority: Boolean): TListOfNodes;
begin
  if head <> nil then
  begin
    Result := getAllChildrenNodesForTheNode(head, LevelPriority);
    Result.Insert(0, head);
    exit;
  end else
  begin
    result := TListOfNodes.Create;
    exit;
  end;
end;

function TStateTree.InsertTheNode(ValueForTheNode: TBoardGame; i, j: Integer; InsertUnderTheNode: TStateNode): TStateNode;
var
  tmpNodeNew: TStateNode;
  tmpNextNodes: TListOfNodes;
begin
  tmpNodeNew := TStateNode.Create;
  tmpNodeNew.parentNode := InsertUnderTheNode;
  tmpNodeNew.data := ValueForTheNode;
  tmpNodeNew.step_i := i;
  tmpNodeNew.step_j := j;
  tmpNodeNew.nextNodes := nil;

  tmpNextNodes := InsertUnderTheNode.nextNodes;
  if tmpNextNodes = nil then
    tmpNextNodes := TListOfNodes.Create();
  tmpNextNodes.Add(tmpNodeNew);
  InsertUnderTheNode.nextNodes := tmpNextNodes;
  Result := tmpNodeNew;
end;

procedure TStateTree.Print;
var
  tmpList: TStringList;
begin
  // Print the State Tree
  if not GShowStateTreeInfor then
    Exit;
  if Self.ListBox <> nil then
  begin
    tmpList := TStringList.Create;
    PrintRecursively(Head, tmpList);
    Self.ListBox.Items.Clear;
    Self.ListBox.Items.AddStrings(tmpList);
    OutputDebugString(PChar(Format('Print: %d nodes.', [tmpList.Count])));
    FreeAndNil(tmpList);
    Application.ProcessMessages;
  end;
end;

procedure TStateTree.PrintRecursively(node: TStateNode; var list: TStringList);
var
  tmpNode: TStateNode;
  tmpNextNodes: TListOfNodes;
  i: Integer;
begin
  if node <> nil then
  begin
    PrintNode(node, list);
    tmpNextNodes := node.nextNodes;
    if tmpNextNodes <> nil then
    begin
      for i := 0 to tmpNextNodes.Count - 1 do
      begin
        tmpNode := tmpNextNodes[i];
        PrintRecursively(tmpNode, list);
      end;
    end;
  end;
end;

procedure TStateTree.ReleaseNode(var node: TStateNode);
var
  tmpNextNodes: TListOfNodes;
  i: Integer;
  tmpNode: TStateNode;
begin
  if node <> nil then
  begin
    //release children nodes first
    tmpNextNodes := node.nextNodes;
    if tmpNextNodes <> nil then
    begin
      //release each child node one by one
      for i := 0 to tmpNextNodes.Count - 1 do
      begin
        tmpNode := tmpNextNodes[i];
        ReleaseNode(tmpNode);
      end;
      FreeAndNil(tmpNextNodes);
      node.nextNodes := nil;
    end;
    FreeAndNil(node);
  end;
end;

class function TStateTree.getLevel(node: TStateNode): Integer;
var
  tmpLevel: Integer;
  tmpNode: TStateNode;
begin
  tmpLevel := 0;
  if node = nil then
  begin
    Result := -1;
    exit;
  end;
  tmpNode := node.parentNode;
  while tmpNode <> nil do
  begin
    inc(tmpLevel);
    tmpNode := tmpNode.parentNode;
  end;
  Result := tmpLevel;
end;

function TStateTree.getLevelInfor(listOfNodes: TListOfNodes): String;
var
  tmpString: string;
  i: Integer;
  tmpCurrentNode: TStateNode;
begin
  tmpString := Format('(%d) - ', [listOfNodes.Count]);
  for i := 0 to listOfNodes.Count - 1 do
  begin
    tmpCurrentNode := listOfNodes[i];
    tmpString := tmpString + Format('%d', [getLevel(tmpCurrentNode)]);
  end;
  Result := tmpString;
end;

procedure TStateTree.SetCurrent(const Value: TStateNode);
begin
  FCurrent := Value;
end;

procedure TStateTree.SetHead(const Value: TStateNode);
begin
  FHead := Value;
end;

procedure TStateTree.SetListBox(const Value: TListBox);
begin
  FListBox := Value;
end;

end.
