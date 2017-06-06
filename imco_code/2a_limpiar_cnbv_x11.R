### Actividad económica municipal (limpia)

# library(tidyr)
library(dplyr)
library(seasonal)

# leemos la tabla que ya trae la info por municipio de Banamex y Bancomer, generada por Diego

spr_src <- read_csv("por_municipio_x11.csv" %>% 
    file.path("../data/cnbv/processed", .)) %>% 
  mutate(CVEMUN = CVEMUN %>% str_pad(5, "left", "0")) %>% 
  gather("banco", "trans", bancomer:otros) %>% 
  filter(fecha != "2011-03-01")

empieza <- spr_src$fecha %>% min %>% {c(year(.), month(.))}


# Identificamos las que son todas 0. 
muns_cero <- spr_src %>% 
  group_by(CVEMUN, banco) %>% 
  summarize(es_cero = all(trans == 0))

spr_x13 <- spr_src %>% 
  left_join(muns_cero, by=c("CVEMUN", "banco")) %>% 
  filter(!es_cero) %>% select(-es_cero) %>% 
  mutate(trans = trans + 1) %>% 
  spread(fecha, trans, fill = 1)
  

# Para el resto de las series, queremos hacer una lista de series

ls1 <- list()
for(j in 1:nrow(spr_x13)){
  act   <- spr_x13[j,3:69] %>% t() %>% as.vector()
  colti <- paste0(spr_x13[j,1], "_", spr_x13[j,2])
  tss   <- ts(act, start = empieza, frequency = 12)
  ls1[[colti]] <- tss
}   


# Loop over data
l1 <- lapply(ls1, function(e)
  try(seas(e, x11 = "", x11.save = c("d10", "d12", "trend"))))

saveRDS(l1, "../data/cache/lista_de_x11.RDS")


# list failing models
is.err <- sapply(l1, class) == "try-error"
summary(l1[is.err])
length(summary(l1[is.err]))


# return final series of successful evaluations

# Hacemos esta función par distinguir las que se tienen que sumar 
# de las que se tienen que multiplicar

fechas <- spr_src$fecha %>% unique()

trend <- lapply(l1[!is.err], trend) %>% 
  {do.call(cbind, .)} %>% 
  t() %>% as.data.frame() %>% 
  set_colnames(fechas)

cycle <- lapply(l1[!is.err], . %>% {.$series$d10}) %>% 
  {do.call(cbind, .)} %>% 
  t() %>% as.data.frame() 

trend$colti <- rownames(trend)

colnames(cycle) <- fechas
cycle$colti < -rownames(cycle)

cycle$mulsum <- cycle[,1:67] %>% apply(1, . %>% abs %>% max)

cycle <- cycle %>% 
  mutate(mulsum = ifelse(mulsum < 2, "mul", "sum"))

mul_cycle <- cycle %>% filter(mulsum == "mul") %>% select(-mulsum)

mul_trend <- trend %>% filter(colti %in% mul_cycle$colti)

mul <- as.matrix(mul_cycle[,1:67]) * as.matrix(mul_trend[,1:67]) %>%
  as_data_frame
mul$colti <- mul_cycle$colti
mul <- mul %>% separate(colti, c("CVEMUN", "banco"), sep = "_")


sum_cycle <- cycle %>% filter(mulsum == "sum") %>% select(-mulsum)
sum_trend <- trend %>% filter(colti %in% sum_cycle$colti)

sumadas <- as.matrix(sum_cycle[,1:67]) + as.matrix(sum_trend[,1:67]) %>%
  as_data_frame 
sumadas$colti <- sum_cycle$colti
sumadas <- sumadas %>% separate(colti, c("CVEMUN", "banco"), sep = "_")

x11_tentativas <- bind_rows(sumadas, mul)


#### Escoger series originales o X11 #### 

spr_idx <- spr_src %>% select(CVEMUN, banco) %>% unique

spr_final_tent_ <- spr_idx %>% 
  left_join(x11_positivas, by=c("CVEMUN", "banco"))

spr_negativas <- spr_final_tent_ %>% 
  filter(is.na(`2014-11-01`)) %>% select(CVEMUN, banco) %>% 
  left_join(spr_src, by=c("CVEMUN", "banco")) %>% 
  spread(fecha, trans, fill = 0)

spr_final_tent <- spr_final_tent_ %>% 
  filter(!is.na(`2014-12-01`)) %>% 
  bind_rows(spr_negativas) %>% 
  gather("fecha", "trans", starts_with("20"))

write_csv(spr_final_tent, 
  "../data/cnbv/processed/municipios_todos_x11.csv")
# En S3 se llama municipios_x11_preinput.csv.


# Comprobar los x11 generados y las fuentes. 

cnbv_original <- spr_src %>% 
  mutate(fecha = as.character(fecha))

cnbv_x11 <- spr_final_tent

comparacion_x11 <- left_join(cnbv_x11, cnbv_original, 
  by = c("CVEMUN", "banco", "fecha"), suffix = c("_x11", "_or")) %>% 
  mutate(dif_abs = abs(trans_x11 - trans_or), 
         dif_prc = dif_abs/trans_or)

corte_mun <- comparacion_x11 %>% 
  group_by(CVEMUN, banco) %>% 
  summarize(mediana = median(dif_prc, na.rm = TRUE)) 

corte_sust <- corte_mun %>% filter(mediana > 0.7) %>% 
  select(CVEMUN, banco) %>% 
  left_join(spr_src, by = c("CVEMUN", "banco")) %>% 
  spread(fecha, trans, fill = 0)

spr_final <- spr_final_tent %>% 
  anti_join(corte_sust, by = c("CVEMUN", "banco")) %>% 
  spread(fecha, trans, fill = 0) %>% 
  bind_rows(corte_sust)

# write_csv(spr_final, 
#   "../data/cnbv/processed/municipios_select_x11.csv")


# spr_final (en vez de read_csv... ) 
cnbv_input_ <- read_csv("municipios_select_x11.csv" %>% 
      file.path("../data/cnbv/processed", .)) %>% 
  gather(fecha, cnbv_x11, starts_with("20"), convert = TRUE) %>% 
  spread(banco, cnbv_x11, fill = 0) %>% 
  transmute(CVEMUN = CVEMUN, fecha = fecha, 
      todos_x11 = valor, 
      otros_x11 = otros, 
      suma_x11  = bancomer + banamex + otros,
      banamex_x11 = banamex, 
      bancomer_x11 = bancomer,
      menosbancomer_x11 = valor - bancomer, 
      sinbancomer_x11 = banamex + otros) %>% 
  filter(!str_detect(CVEMUN, "bla")) %>% 
  filter(fecha < "2016-10-01")
cnbv_input <- cnbv_input_ %>% 
  mutate(trimestre = fecha %>% ymd %>% floor_date("quarter") %>% 
      add(2 %>% months)) %>% 
  group_by(trimestre, CVEMUN) %>% 
  summarize_at(vars(todos_x11, suma_x11, banamex_x11, otros_x11, 
      sinbancomer_x11, bancomer_x11, menosbancomer_x11), 
      funs(. %>% sum(na.rm=TRUE)))

write_csv(cnbv_input, 
  "../data/cnbv/processed/municipios_x11_input.csv")




##############################################################
#### Separar en otro archivo 

metros_input <- read_csv("zonas_metro_estado_ok.csv" %>% 
      file.path("../data/referencias", .)) %>% 
  select(CVEMET, nombre_corto, CVEMUN) %>% 
  mutate(CVEMET = CVEMET %>% str_pad(3, "left", "0"), 
         CVEMUN = CVEMUN %>% str_pad(5, "left", "0")) %>% 
  left_join(cnbv_input, by = "CVEMUN") %>% select(-CVEMUN) %>% 
  group_by(trimestre, CVEMET, nombre_corto) %>% 
  summarize_all(funs(sum)) %>% ungroup %>% 
  rename(zona_metro = nombre_corto)

write_csv(metros_input, 
  "../data/cnbv/processed/zonas_metro_x11_input.csv")


estados_input <- cnbv_input %>%
  mutate(CVEENT = CVEMUN %>% str_sub(1, 2)) %>% select(-CVEMUN) %>% 
  group_by(trimestre, CVEENT) %>% 
  summarize_all(funs(sum))

write_csv(estados_input, 
  "../data/cnbv/processed/estados_x11_input.csv")
  
  
  
  
  
  
  
  
  
