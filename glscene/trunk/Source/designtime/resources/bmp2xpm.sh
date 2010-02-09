#!/bin/bash
#new thing:
# ok, that works. But bmp is much bigger than xpm blowing up the
# resources and therefore executable size.
# So better stick with the old style
#cd objects
#lazres ../../gllazarusobjects.lrs `find -iname "*.bmp"`
#cd ..

#old thing:
find objects/ -iname "*.bmp" -exec cp {} objects_lcl/ \;
cd objects_lcl
for nbmp in `find -iname "*.bmp"`; do
  nxpm=`echo "$nbmp" | sed 's@bmp$@xpm@'`
  echo "$nbmp to $nxpm"
  convert $nbmp $nxpm
done
rm *.bmp
lazres ../../gllazarusobjects.lrs *.xpm
cd ..
# end good old thing

# now vcl
# TODO: try to use the bmp files here, too

find vcl/ -iname "*.bmp" -exec cp {} lcl/ \;
cd lcl
for nbmp in `find -iname "*.bmp"`; do
  nxpm=`echo "$nbmp" | sed 's@^./glscene_icon_@./@' |sed 's@bmp$@xpm@'`
  echo "$nbmp to $nxpm"
  convert $nbmp $nxpm
done
rm *.bmp
lazres ../../glsceneregister.lrs *.xpm

cd ..
echo "Remember to run the following commands after adding the new *.xpm to svn:"
echo 'find -name "*.xpm" -exec svn ps svn:eol-style "native" {} \;'
echo 'find -name "*.xpm" -exec svn ps svn:mime-type "image/x-xpixmap" {} \;'
