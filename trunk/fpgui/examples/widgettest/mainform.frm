object MainForm: TMainForm
  Text = 'Widget tests'
  BorderWidth = 8
  object Box: TBoxLayout
    Spacing = 8
    Orientation = Vertical
    object Label: TLabel
      Text = 'Choose a test form:'
    end
    object CheckboxBtn: TButton
      Text = 'Check boxes'
      OnClick = CheckboxBtnClicked
    end
    object RadioButtonBtn: TButton
      Text = 'Radio buttons'
      OnClick = RadioButtonBtnClicked
    end
    object GroupBoxBtn: TButton
      Text = 'Group boxes'
      OnClick = GroupBoxBtnClicked
    end
    object EditBtn: TButton
      Text = 'Edit fields'
      OnClick = EditBtnClicked
    end
    object ScrollBarBtn: TButton
      Text = 'Scroll bars'
      OnClick = ScrollBarBtnClicked
    end
    object ScrollBoxBtn: TButton
      Text = 'Scroll boxes'
      OnClick = ScrollBoxBtnClicked
    end
    object ListBoxBtn: TButton
      Text = 'List boxes'
      OnClick = ListBoxBtnClicked
    end
    object ComboBoxBtn: TButton
      Text = 'Combo boxes'
      OnClick = ComboBoxBtnClicked
    end
    object GridBtn: TButton
      Text = 'Grids'
      OnClick = GridBtnClicked
    end
    object Separator: TSeparator
    end
    object ExitBtn: TButton
      Text = 'Exit'
      OnClick = ExitBtnClicked
    end
  end
end
