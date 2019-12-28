import { Directive, TemplateRef } from '@angular/core';

@Directive({
  selector: '[appEditMode]'
})
export class EditModeDirective {

  constructor(public tpl: TemplateRef<any>) { };

}
