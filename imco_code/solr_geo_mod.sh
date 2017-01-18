#!/bin/bash

# El comando supone un archivo separado por comas con, al menos, tres columnas: lugar_id, x, y.
# La salida del archivo es : lugar_id, cvegeo.
# NOTA: la agregación geográfica está fija a 4 (5 en la base de datos).
# Presupone la existencia de dos variables de entorno: SOLR_HOST y SOLR_COLLECTION.

if [ -n "$SOLR_HOST" ] && [ -n "$SOLR_COLLECTION" ]; then
  awk 'BEGIN { FS="," }
             { print $1,$2,$3 }' | 
    while read id lat long; do
      resp=$(curl -G "$SOLR_HOST:8983/solr/$SOLR_COLLECTION/select" 
      --data "q=ag_geo_id:3&fl=cvegeo&fq="'{!geofilt%20sfield=geom}'"&pt=${lat}%2C${long}&d=.001&rows=3&wt=csv&indent=true")
      resp_str=($resp)
      resp_n=${#resp_str[@]}
      case resp_n in
        0) echo $id >> tmp/no_buscados ;;
        1) echo $id >> tmp/no_encontrados ;;
        2) echo "$resp" | sed 1d | sed -r "s/^/${id},/g;" >> tmp/procesados ;;
        *) echo "$resp" | sed 1d | sed -r "s/^/${id},/g;" >> tmp/repetidos ;;
      esac 
    done
else
  echo "Define SOLR_HOST y SOLR_COLLECTION.
Si aún así no regresa datos, pregúntale a Paco que qué pex."
fi


