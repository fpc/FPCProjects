object GroupBoxForm: TGroupBoxForm
  BorderWidth = 8
  Text = 'Group box test'
  object HorzBox: TBoxLayout
    VertAlign = vertTop
    object GroupBox1: TGroupBox
      Text = 'Group box #1'
      object VertBox1: TBoxLayout
        Orientation = Vertical
        object GrayCheckBox: TCheckBox
          Text = 'Gray other group box'
	  OnClick = GrayCheckBoxClick
        end
        object Button: TButton
          Text = 'Reset radio buttons'
	  OnClick = ButtonClick
	end
      end
    end
    object GroupBox2: TGroupBox
      Text = 'Group box #2'
      object VertBox2: TBoxLayout
        Orientation = Vertical
	object Radio1: TRadioButton
	  Checked = True
	  Text = 'Option 1'
	end
	object Radio2: TRadioButton
	  Text = 'Option 2'
	end
	object Radio3: TRadioButton
	  Text = 'Option 3'
	end
	object Radio4: TRadioButton
	  Text = 'Option 4'
	end
	object Radio5: TRadioButton
	  Text = 'Option 5'
	end
      end
    end
  end
end
