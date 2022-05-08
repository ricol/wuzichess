unit StateTree;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Generics.Defaults, Generics.Collections,
  Dialogs, Menus, ComCtrls, ExtCtrls, ToolWin, StdCtrls, Common, BoardGame, StateNode;

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
  lvl1, lvl2: Integer;
begin
  lvl1 := TStateTree.getLevel(node1);
  lvl2 := TStateTree.getLevel(node2);
  result := lvl1 - lvl2;
end;

constructor TStateTree.Create(var initialGame: TBoardGame);
begin
  inherited Create();
  Head := TStateNode.Create;
  with Head do
  begin
    data := initialGame;
    step_i := -1;
    step_j := -1;
    parentNode := nil;
    nextNodes := nil;
  end;
end;

destructor TStateTree.Destroy;
begin
  ReleaseNode(FHead);
  inherited;
end;

function TStateTree.getAllChildrenNodesForTheNode(node: TStateNode; LevelPriority: Boolean): TListOfNodes;
var
  i, j: Integer;
  nextNodes: TListOfNodes;
  nodes: TListOfNodes;
  list: TListOfNodes;
begin
  if node <> nil then
  begin
    if LevelPriority then
    begin
      result := getAllChildrenNodesForTheNode(node, False);
      result.Sort(TComparer<TStateNode>.Construct(CompareNodes));
    end
    else
    begin
      nextNodes := node.nextNodes;
      if nextNodes <> nil then
      begin
        list := TList<TStateNode>.Create();
        list.Clear;
        for i := 0 to nextNodes.Count - 1 do
        begin
          list.Add(nextNodes[i]); // add the current one to the list
          nodes := getAllChildrenNodesForTheNode(nextNodes[i], LevelPriority);
          if nodes <> nil then
          begin
            // add all children nodes to the list
            for j := 0 to nodes.Count - 1 do
            begin
              list.Add(nodes[j]);
            end;
            FreeAndNil(nodes);
          end;
        end;
        result := list;
        exit;
      end
      else
      begin
        result := TListOfNodes.Create;
        exit;
      end;
    end;
  end
  else
  begin
    result := TListOfNodes.Create;
    exit;
  end;
end;

function TStateTree.getAllLeaves(LevelPriority: Boolean): TListOfNodes;
var
  allNodes: TListOfNodes;
  i: Integer;
  node: TStateNode;
  list: TList<TStateNode>;
begin
  if Head <> nil then
  begin
    allNodes := getAllNodes(LevelPriority);

    // OutputDebugString(PChar(getLevelInfor(tmpArray_AllNodes)));

    list := TList<TStateNode>.Create();
    for i := 0 to allNodes.Count - 1 do
    begin
      node := allNodes[i];
      if node.nextNodes = nil then
        list.Add(node);
    end;

    // OutputDebugString(PChar(getLevelInfor(tmpList)));

    FreeAndNil(allNodes);
    result := list;
    exit;
  end
  else
  begin
    result := TListOfNodes.Create;
    exit;
  end;
end;

function TStateTree.getAllNodes(LevelPriority: Boolean): TListOfNodes;
begin
  if Head <> nil then
  begin
    result := getAllChildrenNodesForTheNode(Head, LevelPriority);
    result.Insert(0, Head);
    exit;
  end
  else
  begin
    result := TListOfNodes.Create;
    exit;
  end;
end;

function TStateTree.InsertTheNode(ValueForTheNode: TBoardGame; i, j: Integer; InsertUnderTheNode: TStateNode): TStateNode;
var
  nodeNew: TStateNode;
  nextNodes: TListOfNodes;
begin
  nodeNew := TStateNode.Create;
  with nodeNew do
  begin
    parentNode := InsertUnderTheNode;
    data := ValueForTheNode;
    step_i := i;
    step_j := j;
    nextNodes := nil;
  end;

  nextNodes := InsertUnderTheNode.nextNodes;
  if nextNodes = nil then
    nextNodes := TListOfNodes.Create();
  nextNodes.Add(nodeNew);
  InsertUnderTheNode.nextNodes := nextNodes;
  result := nodeNew;
end;

procedure TStateTree.Print;
var
  list: TStringList;
begin
  // Print the State Tree
  if not GShowStateTreeInfor then
    exit;
  if Self.ListBox <> nil then
  begin
    list := TStringList.Create;
    PrintRecursively(Head, list);
    Self.ListBox.Items.Clear;
    Self.ListBox.Items.AddStrings(list);
    OutputDebugString(PChar(Format('Print: %d nodes.', [list.Count])));
    FreeAndNil(list);
    Application.ProcessMessages;
  end;
end;

procedure TStateTree.PrintRecursively(node: TStateNode; var list: TStringList);
var
  n: TStateNode;
  next: TListOfNodes;
  i: Integer;
begin
  if node <> nil then
  begin
    PrintNode(node, list);
    next := node.nextNodes;
    if next <> nil then
    begin
      for i := 0 to next.Count - 1 do
      begin
        n := next[i];
        PrintRecursively(n, list);
      end;
    end;
  end;
end;

procedure TStateTree.ReleaseNode(var node: TStateNode);
var
  nextNode: TListOfNodes;
  i: Integer;
  n: TStateNode;
begin
  if node <> nil then
  begin
    // release children nodes first
    nextNode := node.nextNodes;
    if nextNode <> nil then
    begin
      // release each child node one by one
      for i := 0 to nextNode.Count - 1 do
      begin
        n := nextNode[i];
        ReleaseNode(n);
      end;
      FreeAndNil(nextNode);
      node.nextNodes := nil;
    end;
    FreeAndNil(node);
  end;
end;

class function TStateTree.getLevel(node: TStateNode): Integer;
var
  lvl: Integer;
  n: TStateNode;
begin
  lvl := 0;
  if node = nil then
  begin
    result := -1;
    exit;
  end;
  n := node.parentNode;
  while n <> nil do
  begin
    inc(lvl);
    n := n.parentNode;
  end;
  result := lvl;
end;

function TStateTree.getLevelInfor(listOfNodes: TListOfNodes): String;
var
  s: string;
  i: Integer;
  node: TStateNode;
begin
  s := Format('(%d) - ', [listOfNodes.Count]);
  for i := 0 to listOfNodes.Count - 1 do
  begin
    node := listOfNodes[i];
    s := s + Format('%d', [getLevel(node)]);
  end;
  result := s;
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
