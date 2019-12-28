import { Directive, TemplateRef } from '@angular/core';

@Directive({
  selector: '[appViewMode]'
})
export class ViewModeDirective {

  constructor(public tpl: TemplateRef<any>) { }

}
