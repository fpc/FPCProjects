object EditForm: TEditForm
  BorderWidth = 8
  Text = 'Edit field test'
  object Grid: TGridLayout
    ColCount = 3
    RowCount = 4
    GridPositions = <
      item
	Widget = Label1
      end
      item
	x = 1
	Widget = Edit1
      end
      item
	x = 2
	Widget = GrayCheckBox1
      end
      item
        y = 1
	width = 3
	Widget = Separator
      end
      item
        y = 2
	Widget = Label2
      end
      item
	x = 1
        y = 2
	Widget = Edit2
      end
      item
	x = 2
        y = 2
	Widget = GrayCheckBox2
      end
      item
        x = 1
	y = 3
	Widget = PasswordDisplay
      end>
    object Label1: TLabel
      Text = 'Normal edit field:'
    end
    object Edit1: TEdit
      Text = 'Edit1'
    end
    object GrayCheckBox1: TCheckBox
      Text = 'Disabled'
      OnClick = GrayCheckBox1Click
    end
    object Separator: TSeparator
    end
    object Label2: TLabel
      Text = 'Password edit field:'
    end
    object Edit2: TEdit
      PasswordChar = '*'
      Text = 'Edit2'
      OnChange = Edit2Change
    end
    object GrayCheckBox2: TCheckBox
      Text = 'Disabled'
      OnClick = GrayCheckBox2Click
    end
    object PasswordDisplay: TLabel
      Text = '(Password field)'
    end
  end
end
