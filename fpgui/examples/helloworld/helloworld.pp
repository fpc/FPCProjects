program HelloWorld;
uses fpGUI;

type
  TMainForm = class(TForm)
    TextLabel: TLabel;
  end;

var
  MainForm: TMainForm;

begin
  Application.Title := 'HelloWorld';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
  MainForm.Free;
end.
