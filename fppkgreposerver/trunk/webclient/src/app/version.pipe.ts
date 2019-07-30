import { Pipe, PipeTransform } from '@angular/core';
import { Version } from './version';
import { VersionUtils } from './version';

@Pipe({
  name: 'version'
})
export class VersionPipe implements PipeTransform {

  transform(value: Version, args?: any): string {
    return VersionUtils.toVersionString(value);
  }

}
