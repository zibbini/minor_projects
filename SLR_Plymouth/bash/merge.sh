#!/bin/bash

cd path/to/files

unzip '*.zip'

gdalbuildvrt merged.vrt *.asc
gdal_translate merged.vrt merged.tif

