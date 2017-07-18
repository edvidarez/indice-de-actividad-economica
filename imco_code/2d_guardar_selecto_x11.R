# Diego Villamil, OPI
# CDMX, 15 de enero de 2017


# Empata las series de x11 con el predictor correspondiente. 

# La tabla se refiere a la referencia de los grupos.
importa_tabla <- TRUE

if (!importa_tabla) {
  muns_selecto <- read.csv(
    "../data/cnbv/processed/predictor_x11_selecto_estatal_corregido.csv",
    colClasses = c("character", "character", "character", "numeric"), 
    stringsAsFactors = FALSE)
} else {
  predictores <- read_csv("../data/referencias/series_x11_cnbv.csv") %>%
    select(CVEENT, Estado, predictor_x11) %>% 
    mutate(CVEENT = CVEENT %>% str_pad(2, "left", "0")) %$% {
      predictor_x11 %>% set_names(CVEENT) }
   
  muns_selecto <- read_csv("../data/cnbv/processed" %>% 
      file.path("municipios_x11_input.csv" )) %>% 
    gather(clave_tmp, selecto_x11, ends_with("_x11")) %>% 
    mutate(CVEENT = CVEMUN %>% str_sub(1, 2),
      predictor_x11 = predictores[CVEENT]) %>% select(-CVEENT) %>%
    filter(clave_tmp == predictor_x11) %>% select(-clave_tmp)
}


estados_selecto <- muns_selecto %>% 
  mutate(CVEENT = str_sub(CVEMUN, 1, 2)) %>% 
  group_by(trimestre, CVEENT) %>% 
  summarize(atm_selecto = sum(selecto_x11), 
      predictor_x11 = max(predictor_x11))

gg_selecto <- estados_selecto %>% 
  ggplot(aes(trimestre, atm_selecto)) + 
  facet_wrap(~CVEENT, scales = "free_y") +
  geom_line()



write_csv(estados_selecto, 
  "../data/cnbv/processed/x11_selecto_estados_martes.csv")

metros_selecto <- read_csv(
      "../data/referencias/zonas_metro_estado_ok.csv") %>% 
  mutate(CVEMET = CVEMET %>% str_pad(3, "left", "0"), 
         CVEMUN = CVEMUN %>% str_pad(5, "left", "0"), 
         CVEENT = CVEENT %>% str_pad(2, "left", "0")) %>% 
  select(CVEMET, zona_metro = nombre_corto, CVEMUN, CVEENT) %>% 
  left_join(muns_selecto, by = "CVEMUN") %>% 
  group_by(trimestre, CVEENT, CVEMET, zona_metro) %>% 
  summarize(atm_selecto = sum(selecto_x11), 
      predictor_x11 = max(predictor_x11))

write_csv(metros_selecto, 
  "../data/cnbv/processed/x11_selecto_zonas_metro_martes.csv")
  










