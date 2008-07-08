#!/bin/bash
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

# now vcl

find vcl/ -iname "*.bmp" -exec cp {} lcl/ \;
cd lcl
for nbmp in `find -iname "*.bmp"`; do
  nxpm=`echo "$nbmp" | sed 's@^./glscene_icon_@./@' |sed 's@bmp$@xpm@'`
  echo "$nbmp to $nxpm"
  convert $nbmp $nxpm
done
rm *.bmp
lazres ../../gllazarusregister.lrs *.xpm


