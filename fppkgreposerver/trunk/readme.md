FPPKG Repository Server
========
Version 0.2, Joost van der Sluis (CNOC) -- 17 Aug 2018

Introduction
------------

Fppkg is the package system from Free Pascal. One of the features is the
ability to install packages from a central repository.
The goal of this application is to create a web-based system to manage such
a central repository. Which may lead to a central database of available
Free Pascal packages.

The idea is to build the server using separate components, working together.
Docker is used for the isolation of the build-agens and to ease the
deployment.

Components
------------

*   ####[IdentityServer]
    The IdentityServer is an OIDC-resource provider to handle the
    authentication and authorization. Originally It was written in C#, using
    the [IdentityServer framework]. It is being replaced by a [Lazarus]
    application.

*   ####[BuildAgent]
    A [Lazarus] console application which can handle package-builds. It
    handles requests sequentially and uses a pristine build-environment
    for each build.

*   ####[WebClient]
    An [Angular-2] web-application which serves as front-end.

*   ####[Packagemanger]
    A [Lazarus] application which keeps track of all packages which are
    available.

*   ####[Buildmanager]
    A [Lazarus] application which keeps a list of all BuildAgents, who
    register themselves into the buildmanager. It is possible to send
    requests to build a package to the buildmanager, which will then
    handle all necessary tasks, and show the result.

*   ####[Repository]
    A [Lazarus] application which contains git-repositories for all packages
    ...

[IdentityServer framework]: https://github.com/IdentityServer
[Lazarus]: http://www.lazarus-ide.org/
[Angular-2]: https://angular.io/

How to start
------------

### Install the dependencies

What you need to setup the test-environment:

*   Linux OS
*   An svn-client (subversion)
*   The [Angular CLI]
*   Free Pascal
*   Some extra Fppkg packages
*   Docker and Docker-compose

[Angular CLI]: https://cli.angular.io/

### Setup a test environment

Checkout the Fppkg Repository sources from svn.

    svn checkout https://svn.freepascal.org/svn/fpcprojects/fppkgreposerver/trunk fppkgreposerver

Compile the buildagent in fppkgreposerver/buildagent. (Make sure all
dependencies are installed, see the buildagent readme for details)

    cd fppkgreposerver/buildagent
    fpc buildagent.lpr

Now build the web-client. You need the Angular 2-CLI (client-interface) for
this.

    cd ../webclient
    ng build

Now build the docker images, and start those images.

    docker-compose build
    docker-compose up

### Do a test-build of a package

*   Open a web-browser and browse to <http://localhost:4200/>
*   Log in using 'joost' and 'jachtluipaard'
*   Select 'admin' from the top-menu
*   Re-build the FPC-test environment. (This can take a while)
*   Go to the main-screen
*   Select a Fppkg-source archive, and upload it to do a test-build

If your package could be compiled, the system will response with a success
message.

If you want to continue from here, take a look at the description of the
individual packages:

*   [IdentityServer]
*   [IdentityServer]
*   [WebClient]

[IdentityServer]: /identityserver_readme.html
[BuildAgent]: /buildagent_readme.html
[WebClient]: /webclient_readme.html

Configurations
------------

The docker-setup has three different configurations:

*   ####Debug (default)

    Each docker is mapped to a different port on localhost. Some dockers may
    communicate directly to each other through the internal network.
    Because each component uses it's own port, CORS is configured in such a
    way that the webclient can access all components.
    With this setup it is possible to stop one or more dockers, and run the
    components on the main system. This way it is possible to debug them.

    Specific command to build using this configuration:

        ng build
        docker-compose build
        docker-compose up

*   ####QA

    Only the webclient docker is mapped to a port (4200) on localhost. The
    other components are proxied by the webclient docker. Still Some dockers
    may communicate directly to each other through the internal network. It is
    not possible to connect to the components directly.
    All component use the same base-url so there's no need for CORS.

    Specific command to build using this configuration:

        ng build -prod --environment qa
        docker-compose -f docker-compose.yml -f docker-compose.qa.yml build
        docker-compose -f docker-compose.yml -f docker-compose.qa.yml up

*   ####Production

    Basicly the same as QA, except for the port being port 80, and it does
    not reference localhost, but http://fpc.cnoc.nl.

    Specific command to build using this configuration:

        ng build -prod --environment prod
        docker-compose -f docker-compose.yml -f docker-compose.prod.yml build
        docker-compose -f docker-compose.yml -f docker-compose.prod.yml up

How to debug single components
------------

It is possible to run the BuildAgent in a debugger when the BuildAgent
container (docker) is stopped first. Then the BuildAgent in the debugger
will bind to localhost, and will be used by the WebClient. This does only work
in the debug-configuration.

    docker stop buildagent

The same principle holds for the webclient. It is possible to review changes
in the webclient almost immediately after they are saved by running an
internal web-server, using the Angular-CLI. You have to stop the docker-
container first

    docker stop webclient
    ng serve

While 'ng serve' is running, after each change of one of the sourcefiles of
the weblient is changed, the site will be re-compiled. And when the site is
open in a browser, it will be refreshed automatically.

It is not that simple to run the IdentityServer in a debugger, because other
components (the BuildAgent) will try to connect through it directly on the
internal Docker-network. So it can only be run in a docker-container.

Wanna help?
------------

Do you have any questions, bugs or a patch? Please contact me at
<joost@cnoc.nl>. Maybe we can work together.

License
------------

This source is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your option)
any later version.

This code is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

A copy of the GNU General Public License is available on the World Wide Web
at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
Boston, MA 02110-1335, USA.