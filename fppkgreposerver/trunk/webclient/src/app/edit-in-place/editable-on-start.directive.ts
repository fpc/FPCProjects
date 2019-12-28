import { Directive, OnInit } from '@angular/core';
import { EditableComponent } from './editable/editable.component'

@Directive({
  selector: '[appEditableOnStart]'
})
export class EditableOnStartDirective implements OnInit {

  constructor(private editable: EditableComponent) {
  }

  ngOnInit() {
    this.editable.toEditMode();
  }
}
