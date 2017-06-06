
# Índice MAGDA
La Medición de Actividad con Grandes Datos es desarrollada mediante el trabajo conjunto de [IMCO](www.imco.org.mx) y [OPI](www.opi.la) con el apoyo de [Fundación Friedrich Naumann](http://www.la.fnst.org). 

Las características principales de este índice son desagregación, frecuencia, accesibilidad, **participación**.
Si deseas tener más información sobre la elaboración del índice, consulta la [documentación](visualization/ficha_tecnica/ficha_tecnica.pdf).  O si prefieres seguir los pasos de la elaboración del índice puedes seguir con la lectura de este README.   

En este repo encontrarás el código que utilizamos para crear el índice MAGDA.  Es decir, incluimos el código necesario para su elaboración y algunos elementos adicionales que hicieron este proceso interesante.  Sin embargo vale la pena mencionar que el proceso incluye muchas pruebas de aprendizaje o intentos de índíces que no se usan en el índice final.  Los _scripts_ correspondientes no los incluimos en este repo. 

El objetivo de este repo es hacer accesible el código para que colegas analistas sigan el proceso, e incluso los más dedicados tengan su propio laboratorio de actividad económica. 


## Manos a la obra

En este proyecto se utiliza mayoritariamente R, un programa y lenguaje para cómputo estadístico, el sitio de internet es (https://www.R-project.org/).  El _software_ [QGIS](http://www.qgis.org/en/site/) también es necesario y junto con él la dependencia geoespacial [GDAL](http://www.gdal.org/).

Nuestras instrucciones siguen el formato de terminal de Linux o Mac. El código está escrito en su mayoría en español, pero también contiene algunas referencias en inglés. 

Sugerimos clonar el repositorio en un directorio local
```
$ cd directorio_local
$ git clone git@github.com:opintel/indice-de-actividad-economica.git .
```

Para seguir, abre el archivo `imco_code/0_descargar_datos.txt` en donde especifica los datos que hay que descargar.  Las agencias que proveen esos datos son:  INEGI, CNBV, NOAA, en los sitios respectivos. 


## Licencia

<a rel="license" href="http://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/4.0/88x31.png" /></a><br />Se utiliza la licencia: <a rel="license" href="http://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International License</a>.

