unit FormData;

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
  str, strItem: string;
  I: Integer;
  bFound: Boolean;
begin
  str := EditSearch.Text;
  bFound := false;
  if str <> '' then
  begin
    for I := currentMatch + 1 to ListBoxMain.Items.Count - 1 do
    begin
      strItem := ListBoxMain.Items[i];
      if Pos(str, strItem) > 0 then
      begin
        ListBoxMain.ItemIndex := i;
        currentMatch := i;
        bFound := True;
        Break;
      end;
    end;
  end;

  if not bFound then
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
