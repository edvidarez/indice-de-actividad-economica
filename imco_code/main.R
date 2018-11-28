source('0_imports.R')
source('0_descargar_cnbv.R')

download_cnbv <- FALSE 

cnbv_first_month <-"2011-04-01"
cnbv_last_month <- "2017-12-01"


if (download_cnbv) {
    cnbv_meses_seq <- seq(as.Date(cnbv_first_month), as.Date(cnbv_last_month) , by = "1 month") %>% format("%Y%m")
    lapply(cnbv_meses_seq,descarga_bm,bm_raw_dir)
    source('1_guardar_cnbv.R')
}
source('1_guardar_luces.R')
source('1_guardar_pibe_itaee.R')

estadoIdToAnalisis <- "14"
source('2a_limpiar_cnbv_x11.R')

#Aqui hay una variable de entrena filtro
source('2c_selecciona_serie_magda.R')

#importa_tabla <- TRUE
source('2d_guardar_selecto_x11.R')

#Aqui hay una variable de entrena modelo
source('3_calcular_crecimientos_cnbv.R')

year_pib <- "2016-12-01"

source('3_calcular_niveles_luces.R')

source('4_juntar_niveles_crecimiento.R')


