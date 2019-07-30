export interface Version {
  major: number;
  minor: number;
  micro: number;
  build: number;
}

export class VersionUtils {
  static toVersionString(value: Version) {
    var res: string = '';
    if (value.major > -1) {
      res = res.concat(value.major.toString());
    };
    if (value.minor > -1) {
      res = res.concat('.').concat(value.minor.toString());
    };
    if (value.micro > -1) {
      res = res.concat('.').concat(value.micro.toString());
    };
    if (value.build > -1) {
      res = res.concat('-').concat(value.build.toString());
    };
    return res;
  }

  static compare(val1: Version, val2: Version) {
    if (val1.major > val2.major) {
      return -1;
    } else if (val1.major < val2.major) {
      return 1;
    } else {
      if (val1.minor > val2.minor) {
        return -1;
      } else if (val1.minor < val2.minor) {
        return 1;
      } else {
        if (val1.micro > val2.micro) {
          return -1;
        } else if (val1.micro < val2.micro) {
          return 1;
        } else {
          if (val1.build > val2.build) {
            return -1;
          } else if (val1.build < val2.build) {
            return 1;
          } else {
            return 0
          }
        }
      }
    }
  }

}