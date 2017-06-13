


# Configuración
Se utiliza el software QGIS y R con RStudio.  También el paquete GDAL por terminal.
En los sitios correspondientes se indica cómo instalarlos. 

Algunos paquetes de R se cargan automáticamente al abrir el proyecto. 
Verificar la instalación previa de stringr, data.table, lubridate, 
readr, dtplyr, tidyr, ggplot2, dplyr, magrittr. 
  

# Descarga de datos
Descargamos datos de los siguientes sitios. 

CNBV, INEGI, NOAA, BIE, y datos de referencia. 


## CNBV
Vínculo:  http://portafolioinfo.cnbv.gob.mx/PUBLICACIONES/IO/Paginas/bm.aspx
Archivos desde abril 2011, hasta el más reciente.  
Se puede correr el archivo de R `0_descargar_cnbv.R` que los guarda en `../data/cnbv/raw/`

## INEGI: 
Vínculo:  http://www.inegi.org.mx/sistemas/bie/
Camino:   - Indicadores económicos de coyuntura > ITAEE > 
  Series desestacionalizadas > todas ... 
  Desde 2010, como CSV, y demás opciones tal cual. 
Guárdala en:  ../data/bie/raw/itaee.csv
Una vez guardada, borra la selección anterior.

Camino:   - Cuentas nacionales > PIBE > Act. Eco. y Ent. Federativa > 
  Precios constantes 2008 > Total de Act. Eco. 
  Desde 2010, como CSV, y demás opciones tal cual.
Guardar en:  ../data/bie/raw/pibe.csv

Marco Geoestadístico:
http://www.inegi.org.mx/geo/contenidos/geoestadistica/m_geoestadistico.aspx
Seleccionar Datos vectoriales > descarga. 
Descomprimir en: ../data/inegi/
Y ejecutar el siguiente comando para copiar los datos
$ mv ../data/inegi/conjunto_de_datos/* ../data/inegi/marco_geo/raw/


## NOAA
Visita el sitio de la NOAA, https://ngdc.noaa.gov/eog/viirs/
VIIRS DNB Nighttime Lights > 2014 > Monthly > 201412 > Tile1_75N180W > VCMSLCFG 
Guardar en: ../data/viirs/raw/  (Puede tardar varios minutos)

Descomprimir el archivo y renombrar el que termina con "avg_rade0.tif" como: 
../data/viirs/raw/luminosidad_tile1.tif

Para recortar la foto a la República Mexicana ejecuta en terminal,
$ gdalwarp -q -cutline ../data/inegi/marco_geo/raw/areas_geoestadisticas_estatales.shp \
  -crop_to_cutline --config GDALWARP_IGNORE_BAD_CUTLINE YES \
  "../data/viirs/raw/luminosidad_tile1.tif" \
  "../data/viirs/raw/luminosidad_mexico.tif"

  
# Procesar datos de NOAA.

Los datos de la NOAA se procesan aparte en QGIS. 
Topamos a 175 debido a zonas con luminosidad extrema.  (Ver ficha técnica)

0. Empezar nuevo proyecto en QGIS como: ../data/qgis/imco_magda.qgs
1. Abrir capa de ráster del .tif generado en: ../data/viirs/luminosidad_mexico.tif
    - Renombrar como viirs
2. Aplicar raster-calculator:  viirs * ( viirs <= 175), 
    - guardar en ../data/viirs/processed/tope_175.tif
3. Abrir capas vectoriales de .shp en ../data/inegi/marco_geo/raw/
    - poligonos_localidades_urbanas_y_rurales.shp
    - areas_geoestadisticas_municipales.shp
4. Guardar capa de localidades y municipios con proyección de ráster
    - botón derecho > guardar como > General > CRS = EPS:4326, 
      ../data/inegi/marco_geo/processed/datos_localidades.shp
      ../data/inegi/marco_geo/processed/datos_municipios.shp
5. Habilitar plugin, de zonal-statistics y aplicar función en el menú Raster. 
    - viirs_175 suma con datos_localidades y datos_municipios.
    - columna de salida: 175_
    
    
# Referencia

Los datos de referencia vienen incluidos en el repo de Git. 










