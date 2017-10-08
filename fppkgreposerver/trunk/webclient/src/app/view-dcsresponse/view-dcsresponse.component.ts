import { Component, Input } from '@angular/core';

@Component({
  selector: 'app-view-dcsresponse',
  templateUrl: './view-dcsresponse.component.html',
  styleUrls: ['./view-dcsresponse.component.css']
})
export class ViewDCSResponseComponent {

  @Input() dcsResponse;
}
