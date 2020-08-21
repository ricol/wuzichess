object FormStateTreeData: TFormStateTreeData
  Left = 787
  Top = 137
  Caption = 'Form State Tree Data'
  ClientHeight = 517
  ClientWidth = 535
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clYellow
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  PixelsPerInch = 96
  TextHeight = 13
  object ListBoxMain: TListBox
    Left = 0
    Top = 37
    Width = 535
    Height = 480
    Color = clBlack
    Font.Charset = ANSI_CHARSET
    Font.Color = clLime
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ItemHeight = 13
    ParentFont = False
    TabOrder = 2
  end
  object EditSearch: TEdit
    Left = 8
    Top = 8
    Width = 442
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnChange = EditSearchChange
  end
  object btnNext: TButton
    Left = 456
    Top = 6
    Width = 75
    Height = 25
    Caption = 'next'
    TabOrder = 1
    OnClick = btnNextClick
  end
end
