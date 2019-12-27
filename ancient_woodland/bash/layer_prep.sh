#!/bin/bash

unzip -n *.zip 

a=1
for i in *.tif
do
  new=$(printf "%02d.tif" "$a")
  mv -i -- "$i" "$new"
  let a=a+1
done

for i in *.tif
do 
  gdalwarp -of GTiff -cutline england.shp -cl england -crop_to_cutline $i $i-eng.tif
done

for i in *.tif-eng.tif
do
  mv $i ${i%.tif-eng.tif}-eng.tif
done

rm -f {01..19..1}.tif



