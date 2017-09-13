OpenID Connect library
========
Version 1.0, Joost van der Sluis (CNOC) -- 13 Sep 2017

Introduction
------------

This library makes it possible to verify OpenID Connect tokens against an
OpenID Provider.

How to use?
------------

The TcnocOpenIDConnect class has functions to verify OpenID-JWT's. It retrieves
the necessary keys from the OpenID-Provider. Note that at this moment only
RSA-signed keys are supported.

Example
------------

For an example, take a look at the BuildAgent-application from the
[FPPKGRepoServer package].

[FPPKGRepoServer package]: https://svn.freepascal.org/cgi-bin/viewvc.cgi/fppkgreposerver/trunk/?root=fpcprojects

Wanna help?
------------

Do you have any questions, bugs or a patch? Please contact me at
<joost@cnoc.nl>. Also if you want to extend this library, please tell me.
Maybe we can work together.

License
------------

This library is free software; you can redistribute it and/or modify it
under the terms of the GNU Library General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at your
option) any later version with the following modification:

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent modules,and
to copy and distribute the resulting executable under terms of your choice,
provided that you also meet, for each linked independent module, the terms
and conditions of the license of that module. An independent module is a
module which is not derived from or based on this library. If you modify
this library, you may extend this exception to your version of the library,
but you are not obligated to do so. If you do not wish to do so, delete this
exception statement from your version.

This is the same license as from the Free Pascal RTL and FCL, and the Lazarus
LCL.
