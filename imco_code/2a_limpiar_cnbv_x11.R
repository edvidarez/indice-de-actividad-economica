library(dplyr)
library(seasonal)
# library(purrr)


entrena_filtro <- TRUE  # Primero se entrenan (filtro == TRUE) y después 
                        # se actualizan los filtros.

# El sufijo BASE se refiere al perioddo de entrenamiento. 
# Incluso con datos nuevos. 
empieza_base <- ymd("2011-04-01")  # Verificar en la serie. 
termina_base <- ymd("2016-10-01")  # De acuerdo al primer modelo. 


# Leemos la tabla que ya trae la info por municipio de Banamex y
# Bancomer, generada por Diego
spr_src <- read_csv("../data/cnbv/processed" %>% file.path(
    "grupos_municipios_prex11.csv"), 
    col_types = cols("c", "D", "n", "n", "n", "n")) %>% 
  mutate(CVEMUN = CVEMUN %>% str_pad(5, "left", "0")) %>% 
  filter(fecha >= empieza_base) %>% 
  gather("banco", "trans", bancomer, banamex, valor, otros)

# Valor es equivalente a total, otros es la diferencia entre éste y 
# los otros bancos particulares. 

# Este es sólo para comparar. 
spr_src_temp <- spr_src %>%
  filter(fecha >= "2011-04-01") %>%
  group_by(CVEENT = CVEMUN %>% str_sub(1,2), banco, fecha) %>%
  summarize(trans = sum(trans, na.rm = T))


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
n_fechas <- if (entrena_filtro) {
  which(names(spr_x11) == as.character(termina_base)) - 2 
} else { ncol(spr_x11) - 2 }


# Lista de series de tiempo por municipio y banco.
ls1 <- vector(mode = "list", length(names_ls))
for (i in 1:nrow(spr_x11)) {
  ls1[[i]] <- spr_x11[i, 3:(n_fechas + 2)] %>% t() %>% as.vector() %>% 
    ts(start = empieza_base %>% {c(year(.), month(.))}, frequency = 12)
}
names(ls1) <- names_ls


static_x11 <- "../data/cnbv/x11_models/static_x11s.RDS"
# Ver https://github.com/christophsax/seasonal


if (entrena_filtro) {
  # Guardamos los modelos estacionales x11, en modalidad directa, 
  # y estática que sirve para actualizar la serie con nuevos datos. 
  # Se guardan en un mismo ciclo, por motivos 
  cache_x11  <- "../data/cnbv/x11_models/lista_x11s.RDS"
  if (not(file.exists(cache_x11))) {
    l1  <- vector("list", length(names_ls))
    st1 <- vector("list", length(names_ls))
    for (i in 1:length(names_ls)) {
      # Necesita tener un nombre genérico SEAS_i para llamarlo después. 
      seas_i   <- try (seas(ls1[[i]], x11 = ""))
      l1 [[i]] <- seas_i
      st1[[i]] <- try (static(seas_i, evaluate = TRUE))
    }
    names(l1 ) <- names_ls
    names(st1) <- names_ls
    saveRDS(l1,  cache_x11)     
    saveRDS(st1, static_x11)
  } else {
    l1  <- readRDS(cache_x11)
  }
  
} else {  # Actualizamos los nuevos datos con los modelos estáticos.
  if (not(file.exists(static_x11))) 
    stop ("No hay lista de modelos estáticos entrenados.")
  
  
  update_x11 <- "../data/cnbv/x11_models/update_x11s.RDS"
  if (not(file.exists(update_x11))) {
      
    # No siguen el mismo orden NAMES_LS que ST1 que se había guardado
    # anteriormente. 
    st1 <- readRDS(static_x11)
    
    l1 <- vector("list", length(names_ls))
    for (i in seq_along(names_ls)) {  # 1:5){ #
      nom_i  <- names_ls[i]
      seas_i <- st1[[nom_i]]
      print (paste(i, nom_i))
      if (class(seas_i) == "seas") {
        print ("Actualizando la serie")
        l1[[i]] <- update(seas_i, x = ls1[[nom_i]]) 
      } else {
        l1[[i]] <- list(NULL)
        class(l1[[i]]) <- "try-error"
      }
    }
    names(l1) <- names_ls
    saveRDS(l1, update_x11)
  } else {
    l1 <- readRDS(update_x11)
  }
}



# Al comparar las series resultantes pueden variar de una computadora 
# a otra

error_rds <- "../data/referencias/errores_x11.rds"
if (entrena_filtro) {
  is.err <- sapply(l1, class) == "try-error"
  saveRDS(is.err, error_rds)
} else {
  is.err <- readRDS(error_rds)
}




# También distinguir las que se suman y se multiplican.

fechas <- spr_src$fecha %>% unique() %>% extract(1:n_fechas)
# fechas <- fechas[1:68]  # para 2016-10

trend_ <- lapply(l1[!is.err], trend) %>%
  {do.call(cbind, .)} %>%
  t() %>% as.data.frame() %>%
  set_colnames(fechas) %>% 
  mutate(colti = rownames(.))

cycle_ <- lapply(l1[!is.err], . %>% {.$series$d10}) %>%
  {do.call(cbind, .)} %>%
  t() %>% as.data.frame() %>% 
  set_colnames(fechas) %>% 
  mutate(colti = rownames(.))

cols_series <- length(fechas)

cycle_$mulsum <- cycle_[, 1:cols_series] %>% 
  apply(1, . %>% abs %>% max)

cycle_ <- cycle_ %>%
  mutate(mulsum = ifelse(mulsum < 2, "mul", "sum"))




# Usar transform function.

mul_cycle <- cycle_ %>% filter(mulsum == "mul") %>% select(-mulsum)
mul_trend <- trend_ %>% filter(colti %in% mul_cycle$colti)

mul <- as_data_frame(
      as.matrix(mul_cycle[,1:cols_series]) * 
      as.matrix(mul_trend[,1:cols_series]) ) %>% 
  mutate(colti = mul_cycle$colti) %>% 
  separate(colti, c("CVEMUN", "banco"), sep = "_")


sum_cycle <- cycle_ %>% filter(mulsum == "sum") %>% select(-mulsum)
sum_trend <- trend_ %>% filter(colti %in% sum_cycle$colti)

sumadas <- as_data_frame(
       as.matrix(sum_cycle[, 1:cols_series]) + 
       as.matrix(sum_trend[, 1:cols_series])) %>% 
  mutate(colti = sum_cycle$colti) %>% 
  separate(colti, c("CVEMUN", "banco"), sep = "_")


#### Escoger series originales o X11 #### 

# Positivas son tentativas
x11_positivas <- bind_rows(sumadas, mul)


spr_idx <- spr_src %>% select(CVEMUN, banco) %>% unique


spr_final_tent_ <- spr_idx %>% 
  left_join(x11_positivas, by = c("CVEMUN", "banco"))

spr_negativas <- spr_final_tent_ %>% 
  filter(is.na(`2014-11-01`)) %>% 
  select(CVEMUN, banco) %>% 
  left_join(spr_src, by=c("CVEMUN", "banco")) %>% 
  spread(fecha, trans, fill = 0) %>% 
  extract(1:nrow(.), 1:(n_fechas + 2))

spr_final_tent <- spr_final_tent_ %>% 
  filter(!is.na(`2014-12-01`)) %>% 
  bind_rows(spr_negativas) %>% 
  gather("fecha", "trans", starts_with("20"))

write_csv(spr_final_tent, 
  "../data/cnbv/processed/municipios_todos_x11.csv")



spr_final_prueba <- spr_final_tent %>% 
  group_by(CVEENT = str_sub(CVEMUN, 1, 2), banco, fecha) %>% 
  summarize(trans = sum(trans, na.rm = T))

View(spr_final_prueba)



### Comprobar los x11 generados y las fuentes. 

cnbv_original <- spr_src %>% 
  mutate_at("fecha", as.character)

cnbv_x11 <- spr_final_tent

comparacion_x11 <- left_join(cnbv_x11, cnbv_original, 
      by = c("CVEMUN", "banco", "fecha"), 
      suffix = c("_x11", "_or")) %>%
  mutate(dif_abs = abs(trans_x11 - trans_or), 
         dif_prc = dif_abs/trans_or)

corte_mun <- comparacion_x11 %>% 
  group_by(CVEMUN, banco) %>% 
  summarize(mediana = median(dif_prc, na.rm = TRUE)) 

# Si la mediana de la diferencia porcentual es mayor que 70%,
# elegimos la serie de SPR_SRC, que es la original. 
corte_sust <- corte_mun %>% filter(mediana > 0.7) %>% 
  select(CVEMUN, banco) %>% 
  left_join(spr_src, by = c("CVEMUN", "banco")) %>% 
  spread(fecha, trans, fill = 0)

# De lo contrario escogemos las series de SPR_FINAL_TENT. 
spr_final <- spr_final_tent %>% 
  anti_join(corte_sust, by = c("CVEMUN", "banco")) %>% 
  spread(fecha, trans, fill = 0) %>% 
  bind_rows(corte_sust)

write_csv(spr_final,
  "../data/cnbv/processed/municipios_select_x11.csv")


cnbv_input_ <- read_csv("../data/cnbv/processed" %>% file.path(
      "municipios_select_x11.csv")) %>% 
  gather("fecha", "cnbv_x11", starts_with("20"), convert = TRUE) %>% 
  spread(banco, cnbv_x11, fill = 0) %>% 
  transmute(CVEMUN = CVEMUN, fecha = fecha, 
      todos_x11 = valor, 
      otros_x11 = otros, 
      suma_x11  = bancomer + banamex + otros,
      banamex_x11 = banamex, 
      bancomer_x11 = bancomer,
      menosbancomer_x11 = valor - bancomer, 
      sinbancomer_x11 = banamex + otros) %>% 
  filter(!str_detect(CVEMUN, "bla"))

cnbv_input <- cnbv_input_ %>% 
  mutate(trimestre = fecha %>% ymd %>% floor_date("quarter") %>% 
      add(2 %>% months)) %>% 
  group_by(trimestre, CVEMUN) %>% 
  summarize_at(vars(todos_x11, suma_x11, banamex_x11, otros_x11, 
      sinbancomer_x11, bancomer_x11, menosbancomer_x11), 
      funs(. %>% sum(na.rm=TRUE)))

write_csv(cnbv_input, 
  "../data/cnbv/processed/municipios_x11_input.csv")


spr_prueba <- cnbv_input %>% 
  gather(banco, trans, ends_with("x11")) %>% 
  group_by(CVEENT = CVEMUN %>% str_sub(1, 2), banco, 
        trimestre) %>% 
  summarize(trans = sum(trans))
gg_prueba <- spr_prueba %>% 
  ggplot(aes(trimestre, trans, color = banco)) + 
  facet_wrap(~CVEENT, scales = "free_y") +
  geom_line()
print(gg_prueba)

ggsave(plot = gg_prueba, 
  "../visualization/figures/cnbv_confiltros.png",
  height = 9, width = 16, dpi = 100)


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
  
  
  
  
  
  
  
  
  

