#!/bin/bash

cd path/to/files

gdal_translate file.tif file.xyz
sed 's/ \+/,/g' file.xyz > file.csv

# Free up space
rm file.xyz