# Diego Villamil, OPI
# CDMX, a 16 de diciembre de 2016. 

  
  
aplicar_crecimiento <- function(base, crec, columnas) {
  # base <- metros_0; crec <- metros_crec; 
  # columnas <- c("CVEENT", "CVEMET", "zona_metro")
  # base <- muns_0; crec <- muns_crec; columnas <- c("CVEMUN")
  
  datos <- inner_join(base, crec, by=columnas) %>% 
    mutate(acteco = NA) %>% 
    arrange_(.dots = columnas %>% c("trimestre"))
  
  calc_crec <- datos %>% 
    filter(!is.na(crec_fit)) %>% 
    group_by_(.dots = columnas) %>% 
    mutate(es_2014 = (trimestre >= "2014-12-01") & 
            (lag(trimestre) < "2014-12-01"), 
        crec_acum = cumprod(1 + crec_fit),
        crec_14 = sum(crec_acum[es_2014]) %>% 
            replace(. == 0, 1), 
        acteco = ae_175*crec_acum/crec_14)
  
  datos[!is.na(datos$crec_fit), "acteco"] <- calc_crec$acteco
  return (datos)
}


crecimiento_vector <- function(crecimiento, base, indice) {
  if (any(crecimiento %>% is.na)) {
    warning ("Hay NAs en crecimiento")
    crecimiento <- crecimiento %>% replace(is.na(.), 0)
  }
  crec_0 <- cumprod(1 + crecimiento)
  crec_1 <- crec_0/crec_0[indice]*base
}


## Por zonas metropolitanas. 

metros_0 <- read_csv("por_zonas_metro.csv" %>% 
      file.path("../data/resultados/acteco", .)) %>% 
  mutate(CVEMET = CVEMET %>% str_pad(3, "left", "0"),
         CVEENT = CVEENT %>% str_pad(2, "left", "0")) %>%
  select(CVEMET, CVEENT, zona_metro, ae_175)

metros_crec <- read_csv("selecto_zona_metro_martes.csv" %>% 
  file.path("../data/resultados/crecimiento", .)) %>% 
  mutate(CVEMET = CVEMET %>% str_pad(3, "left", "0"),
         CVEENT = CVEENT %>% str_pad(2, "left", "0")) %>% 
  filter(!is.na(trimestre))

cols_crec <- c("CVEENT", "CVEMET", "zona_metro")

metros_eco <- left_join(metros_0, metros_crec, 
  by = cols_crec) %>% 
  group_by_(.dots = cols_crec) %>% 
  arrange_(.dots = cols_crec %>% c("trimestre")) %>% 
  mutate(acteco = crecimiento_vector(crec_fit, ae_175, 
              trimestre == "2014-12-01"))

write_csv(metros_eco %>% select(-ae_175), 
  "../data/resultados/integrado/selecto_zm_edo_martes.csv")

# Checar VAR_ANUAL y comparar con CRECIMIENTO_ACUMULADO de INEGI. 
# y reportes.  
metros_eco_2 <- metros_eco %>%
  group_by(trimestre, CVEMET, zona_metro) %>% 
  summarize(magda = sum(acteco, na.rm = TRUE)) %>% 
  group_by(CVEMET, zona_metro) %>% arrange(trimestre) %>% 
  mutate(var_trim  = magda/lag(magda) - 1,   
         var_anual = magda/lag(magda, 4) - 1, 
      anual_acum = (var_anual + lag(var_anual) + 
          lag(var_anual,2) + lag(var_anual,3))/4) %>% 
  arrange(CVEMET, zona_metro)

write_csv(metros_eco_2, 
  "../data/resultados/integrado/selecto_zona_metro_martes.csv")


metros_formato <- metros_eco_2 %>% ungroup %>% 
  select(trimestre, zona_metro, magda, anual_acum) %>% 
  filter(month(trimestre) == 12) %>% 
  mutate(año = year(trimestre)) %>% select(-trimestre) %>% 
  gather("medida", "valor", magda, anual_acum) %>% 
  filter(!is.na(valor)) %>% 
  unite("medida_año", medida, año) %>% 
  spread(medida_año, valor)

write_csv(metros_formato, "../data/resultados/integrado/reportando.csv")  


## Por estado. 

edo_0 <- read_csv("../data/bie/processed/pibe.csv") %>% 
  filter(año == "2014-12-01") %>% rename(ae_175 = pibe)

edo_crec <- read_csv("../data/resultados/crecimiento" %>% file.path(
  "selecto_estado_martes.csv"))

edo_eco <- aplicar_crecimiento(edo_0, edo_crec, 
    c("CVEENT", "Estado"))

write_csv(edo_eco, 
  "../data/resultados/integrado/selecto_estado.csv")


gg_estados <- edo_eco %>% 
    rename(magda = acteco) %>% 
    gather("indice", "valor", itaee, magda, factor_key = TRUE) %>% 
    group_by(Estado, indice) %>% 
    mutate(valor_ = valor/mean(valor), 
           trimestre = trimestre + months(2)) %>% 
  ggplot(aes(trimestre, valor_)) +
    facet_wrap(~ Estado, nrow = 5) +
    geom_line(aes(color = indice)) + 
    scale_x_date("") + 
    scale_y_continuous("", labels = NULL) +
    scale_color_brewer(palette = "Dark2") + 
    theme(legend.position = c(5/7, 1/7), 
          axis.text.x = element_text(angle=45, hjust=1))
print(gg_estados)

ggsave(plot = gg_estados, 
  "../visualization/figures/estados_x11_selecto_v2.eps", 
  width = 16, height = 9, dpi = 100)


### Esta gráfica es para las zonas metros ###
# Hay que modificarla porque ahorita no jala bien. 


# agrupa_metros <- metros_eco_2 %>% 
#   group_by(CVEMET, zona_metro) %>% 
#   summarize(prom_crec = mean(acteco)) %>% 
#   ungroup %>% 
#   mutate(grupo = cut(rank(-prom_crec), 6, FALSE)) 
# 
# gg_series_metros <- metros_eco_2 %>% ungroup %>% 
#   left_join(by = c("CVEMET", "nombre_corto"), agrupa_metros) %>% 
#   mutate(nombre_corto = factor(nombre_corto, 
#     levels = agrupa_metros$nombre_corto[
#       order(-agrupa_metros$prom_crec)])) %>%
#   ggplot(aes(trimestre, ind_crec, color = nombre_corto)) + 
#     facet_wrap(~ grupo, nrow = 3, scales = "free_y") + 
#     geom_line() + 
#     geom_text_repel(data = ungroup(crec_metro) %>% 
#         filter(trimestre == min(trimestre)) %>% 
#         left_join(agrupa_metros, by = c("CVEMET", "nombre_corto")), 
#       aes(label = nombre_corto), size = 4, segment.color = NA) + 
#     scale_y_continuous(name = "Índice ($ miles de millones)", 
#         labels = . %>% divide_by(1e3)) + 
#     scale_color_manual(values = brewer.pal(9, "Set1") %>% rep(10)) +
#     theme(legend.position = "none")
# print(gg_series_metros)
# 










