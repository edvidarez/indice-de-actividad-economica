# Diego Villamil, OPI
# CDMX, 30 de mayo de 2017

library(dplyr)
library(multidplyr)
library(seasonal)
library(purrr)

# Leemos la tabla que ya trae la info por municipio de Banamex y
# Bancomer, generada por Diego

spr_src <- read_csv("../data/cnbv/processed" %>% file.path(
    "grupos_municipios_prex11.csv"), col_types = cols()) %>% 
  mutate(CVEMUN = CVEMUN %>% str_pad(5, "left", "0")) %>% 
  gather("banco", "trans", bancomer:otros) 

empieza <- spr_src$fecha %>% min %>% {c(year(.), month(.))}
# c(2011, 4)

# Identificamos las que son todas 0. 
muns_cero <- spr_src %>% 
  group_by(CVEMUN, banco) %>% 
  summarize(es_cero = all(trans == 0)) %>% 
  filter(es_cero)


# Para el resto de las series, queremos hacer una lista de series
t_series <- spr_src %>%
  anti_join(muns_cero, by = c("CVEMUN", "banco")) %>%
  mutate(trans = trans + 1) %>%
  group_by(CVEMUN, banco) %>%
  do(serie = ts(.$trans, start = empieza, frequency = 12))

# Prueba con una serie. 
# estacion_0 <- t_series[[1, "serie"]] %>% seas(x11 = "")
  

# Se ejectua para todas y se guarda en un objeto R. 
# El que sigue tarda mucho.  

cache_estacionales <- "../data/cache/rowwise_de_x11.RDS"
if ( !file.exists(cache_estacionales) ) {
  estacionales_ <- t_series %>% 
    group_by(CVEMUN, banco) %>% 
    do(estacional = map(.$serie, safely(. %>% seas(x11 = "")) ) %>% 
        extract2(1) ) 
  saveRDS(estacionales_, "../data/cache/rowwise_de_x11.RDS")
  
  estacionales <- estacionales_ %>% 
    group_by(CVEMUN, banco) %>% 
    do(result = .$estacional %>% map("result") %>% extract2(1),
       error  = .$estacional %>% map("error")  %>% extract2(1))
  saveRDS(estacionales, "../data/cache/rowwise_de_x11.RDS")
} else {
  estacionales <- read_rds(cache_estacionales)
}
  

## Dependiendo el caso se toman series aditivas o multiplicativas


componentes <- estacionales %>% 
  filter(!is.null(result)) %>% select(-error) %>% 
  group_by(CVEMUN, banco) %>% 
  do(trend = map(.$result, trend) %>% extract2(1), 
     cycle = map(.$result, c("series", "d10")) %>% extract2(1),
     final = map(.$result, final) %>% extract2(1))

# Para cada entrada, distinguimos si es aditiva o multiplicativa
# dependiendo de si la máxima entrada del ciclo es mayor o menor a 2. 
procesos <- componentes %>% 
  group_by(CVEMUN, banco) %>% 
  do(es_mult = map(.$cycle, 
        safely(. %>% abs() %>% max() %>% is_less_than(2) )) %>% 
      unlist)

sin_outlier <- inner_join(procesos, componentes, 
    by = c("CVEMUN", "banco")) %>% group_by(CVEMUN, banco) %>% 
  do(filtro_x11 = invoke_map( ifelse( .$es_mult, `*`, `+`),
      .$trend, .$cycle) %>% unlist())
