# Diego Villamil, OPI
# CDMX, 15 de diciembre de 2016

# Leer archivos del PIBE e ITAEE, simplificando los formatos de INEGI.


estados <- read_csv("../data/referencias/estados.csv", 
      col_types = cols()) %>% 
  mutate(CVEENT = str_pad(cveent, 2, "left", "0")) %>% 
  select(BIE, Estado, CVEENT)

itaee_ <- read_csv("../data/bie/raw/itaee.csv", skip = 1, 
    locale = locale(encoding="latin3")) 

malas_filas <- itaee_[, 2] %>% {sum(is.na(.))}

itaee <- itaee_ %>% 
  head(-malas_filas) %>% 
  select(-contains("Variación")) %>% 
  set_names(names(.) %>% 
      str_replace_all("(.*ciclo > | Índice.*)", "")) %>% 
  gather("estado_efecto", "serie", 2:ncol(.)) %>% 
  separate(estado_efecto, c("estado", "efecto"), sep=" > ") %>% 
  mutate(efecto = efecto %>% str_replace_all(c(
      "Serie original.*" = "original", 
      "Serie desestacionalizada" = "desest", 
      "Tendencia-ciclo" = "ciclo")), 
    fecha = Periodo %>% str_replace_all(c(
      "/04"="/12", "/03"="/09", "/02"="/06", "/01" = "/03")) %>% 
          str_c("/01") %>% ymd) %>% 
  spread(efecto, serie) %>% 
  select(fecha, estado, original, desest, ciclo) %>% 
  left_join(estados, by = c("estado"="BIE")) %>% select(-estado)
  

pibe <- read_csv("../data/bie/raw/pibe_constantes.csv", col_types = cols(),
      skip=3, locale=locale(encoding="latin3")) %>% 
  head(-malas_filas) %>% 
  set_names(names(.) %>% str_replace(" r1.*", "")) %>% 
  gather(Estado, pibe, Aguascalientes:Zacatecas) %>% 
  mutate(año = Periodo %>% str_c("1201") %>% ymd) %>% 
  select(año, Estado, pibe) %>% 
  left_join(estados, by=c("Estado"="BIE")) %>% select(-Estado) %>% 
  rename(Estado = Estado.y)


write_csv(itaee, "../data/bie/processed/itaee.csv")  
  
write_csv(pibe, "../data/bie/processed/pibe.csv")


# La población de CONAPO no se utilizó finalmente. 
# poblacion <- read_csv("../data/conapo/conapo_municipio.csv") %>% 
#   select(CVEMUN = cvegeo, fecha, total) %>% 
#   filter(fecha <= "2016-01-01") %>% 
#   mutate(CVEMUN = CVEMUN %>% str_pad(5, "left", "0"), 
#          CVEENT = str_sub(CVEMUN, 1, 2), 
#          trimestre = fecha %>% ymd %>% add(months(11))) %>% 
#   group_by(CVEENT, trimestre) %>% 
#   summarize(poblacion = sum(total, na.rm=T)) %>% ungroup

# data_pibe <- pibe %>% 
#   group_by(Estado, CVEENT) %>% arrange(trimestre) %>% 
#   mutate(pibe_crec = pibe/lag(pibe) - 1) %>% 
#   filter(year(trimestre) == 2015) %>% ungroup %>% 
#   left_join(poblacion, by = c("CVEENT", "trimestre")) %>% 
#   mutate(pibe_percap = pibe/poblacion)
# 
# gg_pibe_vars <- data_pibe %>% 
#   ggplot(aes(pibe_percap, pibe_crec, color = Estado)) + 
#     geom_point() +
#     geom_text_repel(aes(label = Estado)) + 
#     scale_color_manual(values = brewer.pal(8, "Dark2") %>% rep(4)) +
#     theme(legend.position = "none")
# 
# print(gg_pibe_vars)
# 
# ggsave(plot = gg_pibe_vars, 
#   "../visualization/figures/pibe_transform.png", 
#   width = 16, height = 9, dpi = 100)



