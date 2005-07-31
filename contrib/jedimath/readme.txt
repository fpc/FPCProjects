===============================================================================
Jedi Math
Release Alpha 1.02                                                  27-Mar-2004
===============================================================================
Jedi Math Alpha 1.02 was released by: 
   ralphkmuench@users.sourceforge.net
===============================================================================

This file contains the following topics:
* Installation of Jedi Math Alpha 1.02
* Help files
* Use of Jedi Math in Delphi
* Use of Jedi Math in Kylix
* Demonstration program for Delphi
* Demonstration program for Kylix
* The visual component in JmHistogram
* Contained files in the Jedi Math Release Alpha 1.02
* Changes relative to release Alpha 1.01
* Miscellaneous

===============================================================================

* Installation of Jedi Math Alpha 1.02
======================================

To install this version, extract all the files contained in 
jedimath_alpha_1_02.zip to your desired directory. This 
documentation is based on the directories: 
Delphi:   "C:\JediMath\Alpha_1_02\" 
Kylix:    "/home/username/JediMath/Alpha_1_02/" 
          where "username" of course varies from user to user.

* Help files
============

The help files are stored in various formats in the 
subfolder "Help". They are named:

Alpha 1.02.pdf
Alpha 1.02.rtf
Alpha 1.02.hlp

The HTML-Help starting page is "HTML Help/index.html

* Use of Jedi Math in Delphi
============================

In Delphi make sure you have the path "c:\jedimath\Alpha_1_02\Source" 
included in the directory search path. 
Do do so, go
"Project"->"Options"->"Directories/Conditionals"->"Search Path"
and type "c:\jedimath\Alpha_1_02\Source" in to the search path.

* Use of Jedi Math in Kylix
===========================

In Kylix make sure you have the path "~/JediMath/Alpha_1_02/Source/" 
included in the directory search path. 
Do do so, go:
"Project"->"Options"->"Directories/Conditionals"->"Search Path"
and type "~/JediMath/Alpha_1_02/Source/" in to the search path.

2 additional tips for Kylix:

1.) Paths, Filenames and Units are case sensitive. E.g. if you 
    type ...jedimath... into the pathname instead of ...JediMath...
    then the units will not be found.

2.) If you wish to get rid of the nagscreen every time you 
    run the program, go Run->Parameters->Parameters and type in "-ns" 
    (without the quotation marks).

* Demonstration program for Delphi
==================================

The demonstration program in "Demo Delphi" does nothing but 
compiles all the units of Project Jedi Maths.

* Demonstration program for Kylix
=================================

The demonstration program in "Demo Kylix" does nothing but 
compiles all the units of Project Jedi Maths.

* The visual component in JmHistogram
=====================================

From the Author Patrick van Laake:
The histogram component should be built and installed by the user 
The procedure is easy: On the Component menu select New Component. 
In the dialog, locate the JmHistogram unit. Select a name for the 
package and click OK. On the package dialog which appears click 
Compile. You may have to set a search path from the Options tab. 
After successful compilation, Install in the IDE. 

* Contained files in the Jedi Math Release Alpha 1.02
=====================================================

//The *.inc files contain compiler options and are 
//used by many of the *.pas files in this package.

JmHistogram.inc
JmJcl.inc
JmJedi.inc
JediMath.inc

//These *.pas files which are sometimes used by 
//other *.pas units in the project.

Jm8087.pas
JmTypes.pas
JmMathBasics.pas
JmLogic.pas
JmComplex.pas

//These *.pas files do not get referred to 
//by other units.

JmDSP.pas
JmLargeInt.pas
JmMatrix.pas
JmStatistics.pas
JmSymbolics.pas
JmHistogram.pas

* Changes relative to release Alpha 1.01
========================================

This version is a standalone compilable version 
of Jedi Math and compilable demo-projects for 
Delphi and Kylix are in the directories 
"C:\jedimath\Alpha_1_02\Demo_Delphi\" and 
"C:\jedimath\Alpha_1_02\Demo_Kylix\".

The files JmJcl.inc and JmHistogram.inc have been 
included and help files have been added.

* Miscellaneous
===============

The programs and units were tested in Delphi 6.0 
running in Windows 98 and Kylix 2 running in 
SuSE 8.1 and RedHat 7.3.