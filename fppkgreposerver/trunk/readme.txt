Possible commands:

Command: 
  test
Parameters: 
  packagename - Name of a package that is available in the repository
  packageurl - Url wich points to the package-source zip 
Description:
  Test if the package builds against the repository

Command:
  initialize
Description:
  Setup the test-environment; update, recompile and install the compiler and
  rtl, and setup all settings needed for building and testing packages 

Command:
  update
Description:
  Run 'fppkg update' in the test-environment.
	
Command:
  quit
Description:
  Terminate the fppkg-reposerver

Command:
  addpackage
Parameters:
  packageurl - Url wich points to the package-source zip
  repositoryfpcversion - The fpc-version of the repository to add the package 
    to. Optional, default 'trunk'
  commitmessage - Optional SVN-commit message 
Description:
  Add package to the repository-svn.

Command:
  updatepackage
Parameters:
  packageurl - Url wich points to the package-source zip
  repositoryfpcversion - The fpc-version of the repository to add the package 
    to. Optional, default 'trunk'
  commitmessage - SVN-commit message 
Description:
  Update package in the repository-svn.

Command:
  tagpackage
Parameters:
  packagename - Name of a package that is available in the repository-svn
  repositoryfpcversion - The fpc-version of the repository to add the package 
    to. Optional, default 'trunk'
  commitmessage - Optional SVN-commit message 
Description:
  Add a tag for the package with it's current version.

Command:
  releasepackage
Parameters:
  packagename - Name of a package that is tagged in the repository-svn
  version - Version the package is tagged with
  repository - The repository the package has to be added to. Default:
    'testing'
  repositoryfpcversion - The fpc-version of the repository to add the package 
    to. Optional, default 'trunk'
  commitmessage - Optional SVN-commit message 
Description:
  Release the package, add a tag of a package to a repository 

Command:
  publishrepository
Parameters:
  repository - The repository the package has to be added to. Default:
    'testing'
  repositoryfpcversion - The fpc-version of the repository to add the package 
    to. Optional, default 'trunk'
Description:
  Update the online repository with the repository in svn 


Example on how to add a package (googleapi):

{command:"test",packageurl:"http://www.freepascal.org/~joost/googleapi-3.1.1.source.zip"}
{command:"addpackage",packageurl:"http://www.freepascal.org/~joost/googleapi-3.1.1.source.zip"}
{command:"tagpackage",packagename:"googleapi"}
{command:"releasepackage",packagename:"googleapi",version:"3.1.1"}
{command:"publishrepository"}

