# Diego Villamil, OPI
# CDMX, 2 de enero de 2017

library(broom)

entrena_modelo <- TRUE
lag <- dplyr::lag

itaee <- read_csv("../data/bie/processed/itaee.csv") %>% 
  select(trimestre = fecha, CVEENT, Estado, itaee = original)

cnbv <- read_csv("../data/cnbv/processed" %>% file.path(
  "x11_selecto_estados_martes.csv")) %>%
  select(trimestre, CVEENT, atm_1 = atm_selecto) %>% 
  filter(trimestre > "2011-03-01")

cnbv_muns <- read_csv("../data/cnbv/processed" %>% file.path(
  "x11_selecto_estado_seleccionado.csv")) %>%
  select(trimestre, CVEMUN, atm_1 = atm_selecto) %>% 
  filter(CVEMUN %>% str_sub(1, 2) == "14") %>% 
  filter(trimestre > "2011-03-01")

gg_cnbv <- cnbv %>% 
  filter(trimestre < "2018-12-01") %>% 
  ggplot(aes(trimestre, atm_1)) + 
  facet_wrap(~CVEENT, scales = "free_y") +
  geom_line()
print(gg_cnbv)


g_cnbv <- cnbv_muns %>% 
  filter(trimestre < "2018-09-01") %>% 
  ggplot(aes(trimestre, atm_1)) + 
  facet_wrap(~CVEMUN, scales = "free_y") +
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
    filter(trimestre < "2018-03-01", trimestre > "2012-12-01")
  
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
    filter(trimestre > cnbv_first_month)
  #municipios edvidarez
  cnbv_muns <- cnbv_muns %>% mutate(
    CVEENT = CVEMUN %>% str_sub(0,2)
  )
  
  
  datos_regresion_muns <- inner_join(itaee, cnbv_muns, 
                                by = c("trimestre", "CVEENT")) %>% 
    group_by(CVEENT) %>% arrange(trimestre) %>% 
    mutate_at(vars(itaee, atm_1), 
              funs(crec = . %>% divide_by(dplyr::lag(., 1)) %>% subtract(1))) %>%
    mutate(atm_crec_4 = dplyr::lag(atm_1_crec, 4), 
           atm_1_anual = atm_1/dplyr::lag(atm_1, 4) - 1) %>% 
    filter(trimestre < cnbv_last_month, trimestre > "2012-12-01")
  modelo_crec <- readRDS("../data/referencias/modelo_crecimiento.rds")
}
  

gg_cnbv <- datos_regresion %>% 
  ggplot(aes(trimestre, atm_1)) + 
  facet_wrap(~Estado, scales = "free_y") + 
  geom_line()
print(gg_cnbv)

if (exists("datos_regresion_muns")) {
  gg_cnbv_muns <- datos_regresion_muns %>% 
    ggplot(aes(trimestre, atm_1)) + 
    facet_wrap(~CVEMUN, scales = "free_y") + 
    geom_line()
  print(gg_cnbv_muns)
}
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

if (exists("datos_regresion_muns")) {
  estado_seleccionado_fit <- augment(modelo_crec, newdata = datos_regresion_muns) %>% 
    mutate(
      Estado = CVEMUN
    ) %>%
    select(one_of(c("itaee", "itaee_crec")), 
           trimestre, CVEENT, Estado, crec_fit = .fitted) 
  
  gg_estado_seleccionado <- estado_seleccionado_fit %>% 
    gather("crecimiento", "valor", itaee_crec, crec_fit) %>% 
    ggplot(aes(trimestre, valor, color = crecimiento)) +
    facet_wrap(~ Estado, nrow = 5) +
    geom_line() + 
    ggtitle(formula_crec) +
    scale_color_brewer(palette = "Dark2")
  
  print(gg_estado_seleccionado)
  
  ggsave(plot = gg_estado_seleccionado, 
         "../visualization/figures/modelo_x11_selecto_estado_seleccionado_v2.eps", 
         width = 26, height = 26, dpi = 100)
  muns_data <- read_csv("x11_selecto_estado_seleccionado.csv" %>% 
                          file.path("../data/cnbv/processed", .)) %>%
    rename(atm_1 = atm_selecto) %>% 
    group_by(CVEMUN) %>% arrange(trimestre)%>%
    filter(CVEMUN %>% str_sub(1, 2) == "14") %>% 
    mutate( CVEENT = "14",
            CVEMET = CVEMUN,
            zona_metro = CVEMUN
    ) %>% 
    mutate(atm_1_crec = atm_1/lag(atm_1) - 1, 
           atm_crec_4 = lag(atm_1_crec, 4), 
           atm_1_anual = atm_1/lag(atm_1, 4) - 1) %>% ungroup %>% 
    filter(!is.na(atm_crec_4),is.finite(atm_crec_4)) %>% 
    mutate(CVEMUN = NULL ) %>%
    .[c(1,4,5,6,2,3,7,8,9)]
  muns_fit <- augment(modelo_crec, newdata = muns_data) 
  write_csv(estado_seleccionado_fit, 
            "../data/resultados/crecimiento/selecto_estado_estado_seleccionado_martes.csv")
  
  write_csv(muns_fit, 
            "../data/resultados/crecimiento/selecto_estado_seleccionado.csv")
  muns_acumulado <- muns_fit %>%
    mutate(año = year(trimestre), 
           uno_mas = 1 + crec_fit) %>% 
    group_by(CVEENT, CVEMET, zona_metro) %>% arrange(trimestre) %>% 
    mutate(
      prod_4 = uno_mas*lag(uno_mas)*lag(uno_mas,2)*lag(uno_mas,3)) %>% 
    ungroup %>% 
    group_by(CVEENT, CVEMET, zona_metro, año) %>% 
    summarize(acum_anual = sum(prod_4)/4) 
  
  View(muns_acumulado)
}
 
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










