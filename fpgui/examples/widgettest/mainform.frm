object MainForm: TMainForm
  Text = 'Widget tests'
  BorderWidth = 8
  object Box: TBoxLayout
    CanExpandWidth = False
    Spacing = 8
    Orientation = Vertical
    object Label: TLabel
      CanExpandWidth = True
      Text = 'Choose a test form:'
    end
    object CheckboxBtn: TButton
      CanExpandWidth = True
      Text = 'Check boxes'
      OnClick = CheckboxBtnClick
    end
    object RadioButtonBtn: TButton
      CanExpandWidth = True
      Text = 'Radio buttons'
      OnClick = RadioButtonBtnClick
    end
    object GroupBoxBtn: TButton
      CanExpandWidth = True
      Text = 'Group boxes'
      OnClick = GroupBoxBtnClick
    end
    object EditBtn: TButton
      CanExpandWidth = True
      Text = 'Edit fields'
      OnClick = EditBtnClick
    end
    object ScrollBarBtn: TButton
      CanExpandWidth = True
      Text = 'Scroll bars'
      OnClick = ScrollBarBtnClick
    end
    object ScrollBoxBtn: TButton
      CanExpandWidth = True
      Text = 'Scroll boxes'
      OnClick = ScrollBoxBtnClick
    end
    object ListBoxBtn: TButton
      CanExpandWidth = True
      Text = 'List boxes'
      OnClick = ListBoxBtnClick
    end
    object ComboBoxBtn: TButton
      CanExpandWidth = True
      Text = 'Combo boxes'
      OnClick = ComboBoxBtnClick
    end
    object GridBtn: TButton
      CanExpandWidth = True
      Text = 'Grids'
      OnClick = GridBtnClick
    end
    object Separator: TSeparator
    end
    object ExitBtn: TButton
      CanExpandWidth = True
      Text = 'Exit'
      OnClick = ExitBtnClick
    end
  end
end
