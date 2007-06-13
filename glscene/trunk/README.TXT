==========================================
 
   GLScene For Lazarus 1.0

==========================================

Installation:
#############

ZIP:
* in "lazarus/components/" folder execute "unzip glscene-1.0.0.1.zip"
  This will create the lazarus/componets/glscene/ folder

SVN:
* in "lazarus/components/" folder execute "svn co http://svn.freepascal.org/svn/fpcprojects/glscene/trunk glscene"
  This will create the lazarus/componets/glscene/ folder

Finish the installation:
* copy glscene/globallinks/*.lpl to lazarus/packager/globallinks/
* start lazarus
* select "Configure installed packages ..." from the "Components" menu
* select "glscenelazarus" from the "Available packages" list
* click "Install selection" button
* click "Save and rebuild IDE" button
* repeat the last 4 steps for the other packages you want to use

List of packages:
#################
* glscenelazarus     The main package.
* glscenecgshader    Cg Shader components
* glsceneode         ODE components
* glss_fmodlaz       FMOD component
* gls_winonly        Windows only, contains sound support over mmsystem, BASS support and GLScreenSaver component

More Information:
#################

* Browse the svn repository at "http://www.freepascal.org/cgi-bin/viewcvs.cgi/glscene/trunk/?root=fpcprojects"
* Visit and extend the lazarus wiki page at "http://wiki.lazarus.freepascal.org/GLScene"
* Visit the webpage of the original GLScene project at "http://glscene.org/"
* join freenode.net IRC and chat with
** other glscene for lazarus users on "#glscene" channel
** lazarus users and developers on "#lazarus-ide" channel
** freepascal users and developers on "#fpc" channel

Have fun!
