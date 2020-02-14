// Large parts are copied/based on a blog Netanel Basal from april 2019
// https://netbasal.com/keeping-it-simple-implementing-edit-in-place-in-angular-4fd92c4dfc70
import { Component, OnInit, Output, ContentChild, ElementRef, EventEmitter, Input } from '@angular/core';
import { EditModeDirective } from '../edit-mode.directive';
import { ViewModeDirective } from '../view-mode.directive';
import { Subject } from 'rxjs/Subject';
import { Observable } from 'rxjs';
import { filter, take, switchMapTo } from 'rxjs/operators';

@Component({
  selector: 'app-editable-component',
  templateUrl: './editable.component.html',
})
export class EditableComponent implements OnInit {

  @Output() update = new EventEmitter();
  @Input() allowEdit: boolean = true;
  @ContentChild(ViewModeDirective) viewModeTpl: ViewModeDirective;
  @ContentChild(EditModeDirective) editModeTpl: EditModeDirective;

  mode: 'view' | 'edit' = 'view';

  constructor(private host: ElementRef) {
  }

  get currentView() {
    return this.mode === 'view' ? this.viewModeTpl.tpl : this.editModeTpl.tpl;
  }

  ngOnInit() {
    this.viewModeHandler();
    this.editModeHandler();
  }

  editMode = new Subject();
  editMode$ = this.editMode.asObservable();

  private viewModeHandler() {
    Observable.fromEvent(this.element, 'dblclick').pipe(
      //untilDestroyed(this)
    ).subscribe(() => {
      if (this.allowEdit) {
        this.mode = 'edit';
        this.editMode.next(true);
      }
    });
  }

  private editModeHandler() {
    const clickOutside$ = Observable.fromEvent(document, 'click').pipe(
      filter(({ target }) => this.element.contains(target) === false),
      take(1)
    )

    this.editMode$.pipe(
      switchMapTo(clickOutside$),
      //untilDestroyed(this)
    ).subscribe(event => {
      this.update.next();
      this.mode = 'view';
    });
  }

  private get element() {
    return this.host.nativeElement;
  }

  toViewMode() {
    this.update.next();
    this.mode = 'view';
  }

  toEditMode() {
    if (this.allowEdit) {
      this.mode = 'edit';
    }
  }

}
