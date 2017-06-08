library(dplyr)
library(multidplyr)
library(seasonal)
library(purrr)


entrena_filtro <- TRUE  # Primero se entrenan y después se
                        # actualizan los filtros.

empieza_base <- ymd("2011-04-01")  # Verificar en la serie. 
termina_base <- ymd("2016-10-10")  # De acuerdo al primer modelo. 


# Leemos la tabla que ya trae la info por municipio de Banamex y
# Bancomer, generada por Diego
spr_src <- read_csv("../data/cnbv/processed" %>% file.path(
    "grupos_municipios_prex11.csv"), col_types = cols()) %>% 
  mutate(CVEMUN = CVEMUN %>% str_pad(5, "left", "0")) %>% 
  gather("banco", "trans", bancomer, banamex, valor, otros)
# Valor es equivalente a total, otros es la diferencia entre éste y 
# los otros bancos particulares. 


# Identificamos las que son todas 0, y las quitamos. 
muns_cero <- spr_src %>% 
  group_by(CVEMUN, banco) %>% 
  summarize(es_cero = all(trans == 0)) %>% 
  filter(es_cero)

# La tabla queda en formato de matriz con las 
spr_x11 <- spr_src %>% 
  anti_join(muns_cero, by = c("CVEMUN", "banco")) %>% 
  mutate(trans = trans + 1) %>% 
  spread(fecha, trans, fill = 1) 


### Modificamos las series de tiempo y aplicamos los filtros ###

names_ls <- sprintf("%s_%s", spr_x11$CVEMUN, spr_x11$banco)
termina_col <- if (entrena_filtro) {
  which(names(spr_x11) == termina_base) 
  } else { ncol(spr_x11) }


# Lista de series de tiempo por municipio y banco.
ls1 <- vector(mode = "list", length(names_ls))
for (i in 1:nrow(spr_x11)) {
  ls1[[i]] <- spr_x11[i, 3:termina_col] %>% t() %>% as.vector() %>% 
      ts(start = empieza, frequency = 12)
}
names(ls1) <- names_ls1


static_x11 <- "../data/cnbv/x11_models/static_x11s.RDS"
if (entrena_filtro) {
  # Guardamos los modelos estacionales x11, en modalidad directa, 
  # y estática que sirve para actualizar la serie con nuevos datos. 
  # Se guardan en un mismo ciclo, por motivos 
  cache_x11  <- "../data/cnbv/x11_models/lista_x11s.RDS"
  if (not(file.exists(cache_x11))) {
    l1  <- vector("list", length(names_ls))
    st1 <- vector("list", length(names_ls))
    for (i in 1175:length(names_ls)) {
      seas_i  <- try (seas(ls1[[i]], x11 = ""))
      if (class(seas_i) != "try-error") {
        l1 [[i]] <- seas_i
        st1[[i]] <- static(seas_i, evaluate = TRUE) 
      } else {   # error
        l1 [[i]] <- st1[[i]] <- seas_i 
      } 
    }
    names(l1 ) <- names_ls1
    names(st1) <- names_ls1
    saveRDS(l1,  cache_x11)     
    saveRDS(st1, static_x11)
  } else {
    l1  <- readRDS(cache_x11)
    st1 <- readRDS(static_x11) 
  }
  
} else {  # Actualizamos los nuevos datos con los modelos estáticos.
  if (not(file.exists(static_x11))) 
    stop ("No hay lista de modelos estáticos entrenados.")
  
  update_x11 <- "../data/cnbv/x11_models/update_x11s.RDS"
  if (not(file.exists(update_x11))) {
    
    
  } else {
    l1 <- readRDS(update_x11)
  }
}



# Distinguimos las series estacionales exitosas de las que marcaron 
# error
is.err <- sapply(l1, class) == "try-error"
summary(l1[is.err])
length(summary(l1[is.err]))


# También distinguir las que se suman y se multiplican.

fechas <- spr_src$fecha %>% unique()

trend_ <- lapply(l1[!is.err], trend) %>%
  {do.call(cbind, .)} %>%
  t() %>% as.data.frame() %>%
  set_colnames(fechas)

cycle_ <- lapply(l1[!is.err], . %>% {.$series$d10}) %>%
  {do.call(cbind, .)} %>%
  t() %>% as.data.frame()

trend_$colti <- rownames(trend_)

colnames(cycle_) <- fechas
cycle_$colti <- rownames(cycle_)

cycle_$mulsum <- cycle_[,1:67] %>% apply(1, . %>% abs %>% max)

cycle_ <- cycle_ %>%
  mutate(mulsum = ifelse(mulsum < 2, "mul", "sum"))

mul_cycle <- cycle_ %>% filter(mulsum == "mul") %>% select(-mulsum)

mul_trend <- trend_ %>% filter(colti %in% mul_cycle$colti)

mul <- as.matrix(mul_cycle[,1:67]) * as.matrix(mul_trend[,1:67]) %>%
  as_data_frame
mul$colti <- mul_cycle$colti
mul <- mul %>% separate(colti, c("CVEMUN", "banco"), sep = "_")


sum_cycle <- cycle_ %>% filter(mulsum == "sum") %>% select(-mulsum)
sum_trend <- trend_ %>% filter(colti %in% sum_cycle$colti)

sumadas <- as.matrix(sum_cycle[,1:67]) + as.matrix(sum_trend[,1:67]) %>%
  as_data_frame 
sumadas$colti <- sum_cycle$colti
sumadas <- sumadas %>% separate(colti, c("CVEMUN", "banco"), sep = "_")


#
x11_positivas  <- bind_rows(sumadas, mul)
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

write_csv(spr_final,
  "../data/cnbv/processed/municipios_select_x11.csv")


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
  filter(!str_detect(CVEMUN, "bla"), fecha < "2016-10-01")
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
  
  
  
  
  
  
  
  
  
