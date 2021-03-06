// The file contents for the current environment will overwrite these during build.
// The build system defaults to the dev environment which uses `environment.ts`, but if you do
// `ng build --env=prod` then `environment.prod.ts` will be used instead.
// The list of which env maps to which file can be found in `.angular-cli.json`.

export const environment = {
  production: false,
  identityServerUrl: 'http://localhost:5000',
  webclientUrl: 'http://localhost:4200',
  buildAgentUrl: 'http://localhost:8080',
  packageManagerUrl: 'http://localhost:8088',
  categoryUrl: 'http://localhost:8484',
  buildManagerUrl: 'http://localhost:8181',
  repositoryUrl:  'http://localhost:8089',
  fppkgRepositoryUrl: 'http://localhost:8282'
};
