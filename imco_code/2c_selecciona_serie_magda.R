### Verificamos cuál de las series se ajusta mejor para cada estado ###

entrena_filtro <- FALSE


x11 <- read.csv("../data/cnbv/processed/municipios_x11_input.csv",
    stringsAsFactors = F, 
    colClasses = c("character", "character", "numeric", "numeric",
    "numeric", "numeric", "numeric", "numeric", "numeric")) %>%
  mutate(otros_x11 = suma_x11 - bancomer_x11 - banamex_x11,
    banban_x11 = bancomer_x11 + banamex_x11,
    otrosbbv_x11 = otros_x11 + bancomer_x11)


if (!entrena_filtro) { 
  sel_x11 <- read_csv("../data/referencias/correlaciones_x11_cnbv.csv")

} else {
  itaee <- read.csv("../data/bie/processed/itaee.csv",
      stringsAsFactors = F) %>% 
    mutate(CVEENT = str_pad(CVEENT, 2, "left", "0"),
           itaee  = original) %>% 
    rename(trimestre = fecha)
  
  # probamos a nivel estatal
  prb <- x11 %>% 
    mutate(CVEENT = substr(CVEMUN, 1, 2)) %>% 
    select(-CVEMUN) %>%
    group_by(CVEENT, trimestre) %>% 
    summarise_each(funs(sum)) %>%
    inner_join(itaee, by = c("CVEENT", "trimestre")) %>%
    gather("predictor_x11", "valor", 
        todos_x11:otrosbbv_x11, convert = T) %>%
    group_by(CVEENT,Estado,predictor_x11) %>%
    mutate(corr_x11 = cor(valor, itaee)) %>%
    ungroup()
  
  sel_x11 <- prb %>% 
    select(CVEENT, Estado,predictor_x11,corr_x11) %>% unique() %>%
    group_by(CVEENT, Estado) %>%
    arrange(corr_x11)%>%
    mutate(ones = 1, rank = cumsum(ones)) %>% 
    ungroup() %>%
    filter((predictor_x11 =="banamex_x11" & corr_x11>.95 ) | 
        rank == 9 ) %>%
    group_by(CVEENT, Estado) %>%
    arrange(corr_x11)%>%
    mutate(ones = 1, rank = cumsum(ones)) %>% ungroup() %>%
    filter(rank == 1)
  
  x11_selecto <- x11 %>% mutate(CVEENT= substr(CVEMUN,1,2)) %>%
    gather(predictor_x11, valor, todos_x11:otrosbbv_x11,convert= T) %>%
    inner_join(sel_x11, by = c("CVEENT", "predictor_x11")) %>% 
    select(-rank, -ones, -Estado, -CVEENT, -corr_x11) %>%
    rename(selecto_x11 = valor)
  
  # después de hacer las pruebas vamos a hacer algunos cambios proque 
  # no cuadran en los municipios:
  sel_x11[sel_x11$CVEENT=="08", 3] <- "banamex_x11"
  sel_x11[sel_x11$CVEENT=="02", 3] <- "banamex_x11"
  sel_x11[sel_x11$CVEENT=="09", 3] <- "sinbancomer_x11"
  sel_x11[sel_x11$CVEENT=="15", 3] <- "banamex_x11"
} 

x11_selecto <- x11 %>% 
  mutate(CVEENT = substr(CVEMUN, 1, 2)) %>%
  gather("predictor_x11", "valor", todos_x11:otrosbbv_x11,
      convert = TRUE)%>%
  inner_join(sel_x11, by = c("CVEENT", "predictor_x11")) %>% 
  select(-rank, -ones, -Estado, -CVEENT, -corr_x11) %>%
  rename(selecto_x11 = valor)

write.csv(x11_selecto, row.names = FALSE,
  "../data/cnbv/processed/predictor_x11_selecto_estatal_corregido.csv")



