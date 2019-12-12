#!/bin/bash

cd path/to/files

unzip '*.zip'

# Requires gdal 
gdalbuildvrt merged.vrt *.asc
gdal_translate merged.vrt merged.tif

