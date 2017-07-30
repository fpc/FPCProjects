import { FPPKGRepositoryPage } from './app.po';

describe('fppkg-repository App', () => {
  let page: FPPKGRepositoryPage;

  beforeEach(() => {
    page = new FPPKGRepositoryPage();
  });

  it('should display welcome message', () => {
    page.navigateTo();
    expect(page.getParagraphText()).toEqual('Welcome to app!!');
  });
});
