# Diego Villamil, OPI
# CDMX, 2 de enero de 2017

library(broom)

entrena_modelo <- TRUE


itaee <- read_csv("../data/bie/processed/itaee.csv") %>% 
  select(trimestre = fecha, CVEENT, Estado, itaee = original)

cnbv <- read_csv("../data/cnbv/processed" %>% file.path(
  "x11_selecto_estados_martes.csv")) %>%
  select(trimestre, CVEENT, atm_1 = atm_selecto) %>% 
  filter(trimestre > "2011-03-01")

gg_cnbv <- cnbv %>% 
  filter(trimestre < "2016-12_01") %>% 
  ggplot(aes(trimestre, atm_1)) + 
  facet_wrap(~CVEENT, scales = "free_y") +
  geom_line()
print(gg_cnbv)
  

formula_crec <- "itaee_crec ~ CVEENT + CVEENT:atm_1_crec + " %>% 
  str_c("CVEENT:atm_crec_4 - 1")  
  # Lag en fórmula no respeta a group_by. 


if (entrena_modelo) {
  datos_regresion <- inner_join(itaee, cnbv, 
      by = c("trimestre", "CVEENT")) %>% 
    group_by(CVEENT) %>% arrange(trimestre) %>% 
    mutate_at(vars(itaee, atm_1), 
      funs(crec = . %>% divide_by(lag(., 1)) %>% subtract(1))) %>%
    mutate(atm_crec_4 = lag(atm_1_crec, 4), 
      atm_1_anual = atm_1/lag(atm_1, 4) - 1) %>% 
    filter(trimestre < "2016-01-01", trimestre > "2012-12-01")
  
  # Seguimos el modelo 
  
  modelo_crec <- glm(data = datos_regresion, 
      formula = as.formula(formula_crec), 
      family=gaussian(), na.action = na.omit)
  tidy_crec_1 <- modelo_crec %>% tidy
  
  #modelo_crec_ <- lm(data = datos_regresion, na.action = na.omit, 
   # formula = as.formula(formula_crec))
  
  # glance_crec <- glance(modelo_crec_)
  
  # write_csv(glance_crec, 
  #   "../data/resultados/indicadores/resumen_crecimiento.csv")
  
  
  # tidy_estados <- datos_regresion %>% 
  #   group_by(CVEENT, Estado) %>% 
  #   do(lm(data = ., 
  #         na.action = na.omit, 
  #         formula = as.formula(formula_individual)) %>% 
  #      tidy) %>% 
  #   select(CVEENT, Estado, term, estimate) %>% 
  #   spread(term, estimate)
  
  # glance_estados <- datos_regresion %>% 
  #   group_by(CVEENT, Estado) %>% 
  #   do(lm(data = ., 
  #         na.action = na.omit, 
  #         formula = as.formula(formula_individual)) %>% 
  #      glance) %>% 
  #   select(CVEENT, Estado, r.squared)
  # 
  
  if (str_detect(formula_crec, "CVEENT:atm_1_crec")) {
    tidy_crec_2 <- tidy_crec_1 %>% 
      select(term, estimate) %>% 
      mutate(term = term %>% {
        ifelse(str_detect(., "[0-9]{2}$"), str_c(., ":uno"), .)}) %>% 
      separate(term, c("estado", "termino"), sep = ":") %>% 
      spread(termino, estimate) 
  } else {
    tidy_crec_2 <- NA
  }
  
  write_csv(tidy_crec_2, 
    "../data/resultados/indicadores/coeficientes_v2.csv")
  
  saveRDS(modelo_crec, "../models/modelo_crecimiento_nuevo.rds")
} else {
  estados_ <- itaee %>% 
    select(CVEENT, Estado) %>% unique
  
  datos_regresion <- cnbv %>% 
    inner_join(estados_, by = "CVEENT") %>% 
    group_by(CVEENT) %>% arrange(trimestre) %>% 
    mutate_at(vars(atm_1), 
      funs(atm_1_crec = . %>% {./lag(.) - 1})) %>%
    mutate(atm_crec_4 = lag(atm_1_crec, 4), 
      atm_1_anual = atm_1/lag(atm_1, 4) - 1) %>% 
    filter(trimestre > "2011-07-01")
  
  modelo_crec <- readRDS("../data/referencias/modelo_crecimiento.rds")
}
  

gg_cnbv <- datos_regresion %>% 
  ggplot(aes(trimestre, atm_1)) + 
  facet_wrap(~Estado, scales = "free_y") + 
  geom_line()
print(gg_cnbv)

# Guardamos proyecciones


estado_fit <- augment(modelo_crec, newdata = datos_regresion) %>% 
  select(one_of(c("itaee", "itaee_crec")), 
         trimestre, CVEENT, Estado, crec_fit = .fitted) 

gg_estado <- estado_fit %>% 
    gather("crecimiento", "valor", itaee_crec, crec_fit) %>% 
  ggplot(aes(trimestre, valor, color = crecimiento)) +
    facet_wrap(~ Estado, nrow = 5) +
    geom_line() + 
    ggtitle(formula_crec) +
    scale_color_brewer(palette = "Dark2")

print(gg_estado)

ggsave(plot = gg_estado, 
  "../visualization/figures/modelo_x11_selecto_v2.eps", 
  width = 16, height = 9, dpi = 100)


metro_data <- read_csv("x11_selecto_zonas_metro_martes.csv" %>% 
      file.path("../data/cnbv/processed", .)) %>%
  mutate(CVEMET = CVEMET %>% str_pad(3, "left", "0"), 
      CVEENT = CVEENT %>% str_pad(2, "left", "0")) %>% 
  rename(atm_1 = atm_selecto) %>% 
  group_by(CVEENT, CVEMET) %>% arrange(trimestre) %>% 
  mutate(atm_1_crec = atm_1/lag(atm_1) - 1, 
      atm_crec_4 = lag(atm_1_crec, 4), 
      atm_1_anual = atm_1/lag(atm_1, 4) - 1) %>% ungroup %>% 
  filter(!is.na(atm_crec_4))

metro_fit <- augment(modelo_crec, newdata = metro_data) 
if ("nombre_corto" %in% names(metro_fit)) {
  rename(metro_fit, zona_metro = nombre_corto) }
metro_fit <- metro_fit %>% 
  select(trimestre, CVEENT, CVEMET, zona_metro, crec_fit = .fitted)
  
write_csv(metro_fit, 
  "../data/resultados/crecimiento/selecto_zona_metro_martes.csv")

write_csv(estado_fit, 
  "../data/resultados/crecimiento/selecto_estado_martes.csv")


#### Generamos tasas de crecimiento anualizado acumulado. ####

metro_acumulado <- metro_fit %>%
  mutate(año = year(trimestre), 
    uno_mas = 1 + crec_fit) %>% 
  group_by(CVEENT, CVEMET, zona_metro) %>% arrange(trimestre) %>% 
  mutate(
    prod_4 = uno_mas*lag(uno_mas)*lag(uno_mas,2)*lag(uno_mas,3)) %>% 
  ungroup %>% 
  group_by(CVEENT, CVEMET, zona_metro, año) %>% 
  summarize(acum_anual = sum(prod_4)/4) 
  
View(metro_acumulado)









