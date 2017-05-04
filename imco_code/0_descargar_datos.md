Diego Villamil, OPI
CDMX, 6 de enero de 2017
Feliz día de reyes


Descargamos datos de los siguientes sitios. 

CNBV, INEGI, NOAA, BIE, y datos de referencia. 


## CNBV
Vínculo:  http://portafolioinfo.cnbv.gob.mx/PUBLICACIONES/IO/Paginas/bm.aspx
Archivos desde 2011 abril, hasta el más reciente. 
Guardar en: data/cnbv/raw/

## INEGI: 
Vínculo:  http://www.inegi.org.mx/sistemas/bie/
Camino:   - Indicadores económicos de coyuntura > ITAEE > 
  Series desestacionalizadas > todas
Guardar en:  ../data/bie/raw/itaee.csv

Camino:   - Cuentas nacionales > PIBE > Act. Eco. y Ent. Federativa > 
    Precios constantes 2008 > Total de Act. Eco. 
Guardar en:  ../data/bie/raw/pibe.csv

Marco Geoestadístico:
http://www.inegi.org.mx/geo/contenidos/geoestadistica/m_geoestadistico.aspx
Seleccionar Datos vectoriales > descarga. 
Descomprimir en: ../data/inegi/
En terminal correr 
$ mv ../data/inegi/702825217341_s/conjunto_de_datos/* ../data/inegi/marco_geo/raw/


## NOAA
Visita el sitio de la NOAA, https://ngdc.noaa.gov/eog/viirs/
Sigue los clics y descarga el archivo (puede tarda unos minutos),
VIIRS DNB Nighttime Lights > 2014 > Monthly > 201412 > Tile1_75N180W > VCMSLCFG 
Guardar en: ../data/viirs/

Descomprimir el archivo y renombrar como: 
../data/viirs/vcmslcfg_201412.tif

Para recortar la foto a la República Mexicana ejecuta en terminal,
.../imco_code$ gdalwarp -q -cutline ../data/inegi/marco_geo/raw/areas_geoestadisticas_estatales.shp \
  -crop_to_cutline --config GDALWARP_IGNORE_BAD_CUTLINE YES \
  "../data/viirs/vcmslcfg_201412.tif" \
  "../data/viirs/luminosidad_mexico.tif"

Ejecuta el siguiente comando en terminal, 
$ ./0\ descargar_viirs.sh https://data.ngdc.noaa.gov/instruments/remote-sensing/passive/spectrometers-radiometers/imaging/viirs/dnb_composites/v10//201412/vcmslcfg/SVDNB_npp_20141201-20141231_75N180W_vcmslcfg_v10_c201502231126.tgz

Si marca error es posible que la dirección haya cambiado, Visita el sitio de la NOAA, 
https://ngdc.noaa.gov/eog/viirs/
VIIRS DNB Nighttime Lights > 2014 > Monthly > 201412 > Tile1_75N180W > VCMSLCFG

Con el botón derecho copia la dirección del archivo y sustitúyela en el comando anterior.

El script en terminal descarga y procesa el archivo. (copiar URL)


### Procesar datos de NOAA.

Los datos de la NOAA se procesan aparte en QGIS. 

1. Abrir capa de ráster del .tif generado en: ../data/viirs/raw_raster/final/
2. Aplicar raster-calculator:  viirs_175 = viirs * (viirs <= 175), 
    - guardar en ../data/viirs/processed/tope_175.tif
3. Abrir capa vectorial de .shp en ../data/inegi/marco_geo/raw/
    - poligonos_localidades_urbanas_y_rurales
4. Guardar capa de localidades con proyección de ráster
    - botón derecho > guardar como > CRS = EPS:4326, 
      ../data/inegi/marco_geo/processed/datos_localidades.shp
5. Habilitar plugin, de zonal-statistics y aplicar función:  
    - viirs_175 suma con datos_localidades
    - columna de salida: _175.
    
    
# Referencia

Los datos de referencia se pueden descargar de: 
aws s3 cp --recursive s3://opi-data-science/play/imco/referencias/ ../data/referencias/


# Configuración
Se utiliza el software QGIS y R con RStudio. 

Algunos paquetes se cargan automáticamente al abrir el proyecto-R. 
Verificar la instalación previa de stringr, data.table, lubridate, 
readr, dtplyr, tidyr, ggplot2, dplyr, magrittr. 








