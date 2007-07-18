#! /bin/bash
#
# Converts all *.bmp files in dir into *.xpm using ImageMagick convert command
#

CONVERT=convert

# t=${path_name%/*.*}

L=( "`find . -type f -name '*.bmp' | xargs echo`" )

for x in $L
do
  `${CONVERT} $x ${x%.bmp}.xpm`
done
