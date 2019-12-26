import { Packageversion } from './packageversion';

export interface Package {
  name: string;
  ownerid: string;
  packagestate: string;
  packageversionlist: Packageversion[];
  keywords: string[];
  category: string;
  support: string;
}
