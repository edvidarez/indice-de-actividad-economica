
#!/bin/bash

# Este archivo se corre desde la carpeta actual (.../imco/scripts) como:
# $ ./0\ descargar_viirs.sh 
# La dirección la copiamos directo de la página:
# http://ngdc.noaa.gov/eog/viirs/download_monthly.html 
# (Diciembre 2014, Tile 1, VCMSLCFG) ... se tarda. 

direccion=$1
# https://data.ngdc.noaa.gov/instruments/remote-sensing/passive/
#   spectrometers-radiometers/imaging/viirs/dnb_composites/v10//201412/
#   vcmslcfg/SVDNB_npp_20141201-20141231_75N180W_vcmslcfg_v10_c201502231126.tgz

# direccion=https://data.ngdc.noaa.gov/instruments/remote-sensing/passive/spectrometers-radiometers/imaging/viirs/dnb_composites/v10//201412/vcmslcfg/SVDNB_npp_20141201-20141231_75N180W_vcmslcfg_v10_c201502231126.tgz

# nombre_largo=compositor_satelite_fechas_region_config_version_creado.extension
# Se puede dividir con el siguiente comando.  
# Funciona con bash shell, mas no con shell zsh. 
IFS='/' read -r -a direccion_strip <<< "$direccion"

compositor="SVDNB"
fecha=${direccion_strip[12]}
config=${direccion_strip[13]}
nombre=${direccion_strip[14]}
echo $nombre

IFS='.' read -r -a nombre_0 <<< "$nombre"
nombre_0=${nombre_0[0]}
nombre_corto="${compositor}_${fecha}_${config}"

# Descargar archivos.
curl -o "../data/viirs/raw_raster/zipped/$nombre_corto.tgz" "$direccion"

# Descomprimir tar.
tar -C ../data/viirs/raw_raster/unzipped/ -xvf \
  "../data/viirs/raw_raster/zipped/$nombre_corto.tgz"
rename "s/$nombre_0/$nombre_corto/" ../data/viirs/raw_raster/unzipped/*

## Recortamos los datos de la RepMex y guardamos en la carpeta correspondiente.
# Utiliza GDALWARP
# gdalwarp -q -cutline shapefile -crop_to_cutline fuente destino

gdalwarp -q -cutline ../data/inegi/marco_geo/raw/areas_geoestadisticas_estatales.shp \
  -crop_to_cutline --config GDALWARP_IGNORE_BAD_CUTLINE YES \
  "../data/viirs/raw_raster/unzipped/"$nombre_corto".cf_cvg.tif" \
  "../data/viirs/raw_raster/final/"$nombre_corto".cf_cvg.tif"






