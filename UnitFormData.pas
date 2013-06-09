unit UnitFormData;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFormStateTreeData = class(TForm)
    ListBoxMain: TListBox;
    EditSearch: TEdit;
    btnNext: TButton;
    procedure btnNextClick(Sender: TObject);
    procedure EditSearchChange(Sender: TObject);
  private
    { Private declarations }
    currentMatch: Integer;
  public
    { Public declarations }
  end;

var
  FormStateTreeData: TFormStateTreeData;

implementation

{$R *.dfm}

procedure TFormStateTreeData.btnNextClick(Sender: TObject);
var
  tmpStr, tmpStrItem: string;
  I: Integer;
  found: Boolean;
begin
  tmpStr := EditSearch.Text;
  found := false;
  if tmpStr <> '' then
  begin
    for I := currentMatch + 1 to ListBoxMain.Items.Count - 1 do
    begin
      tmpStrItem := ListBoxMain.Items[i];
      if Pos(tmpStr, tmpStrItem) > 0 then
      begin
        ListBoxMain.ItemIndex := i;
        currentMatch := i;
        found := True;
        Break;
      end;
    end;
  end;

  if not found then
  begin
    currentMatch := 0;
    Beep;
  end;
end;

procedure TFormStateTreeData.EditSearchChange(Sender: TObject);
begin
  currentMatch := 0;
end;

end.
