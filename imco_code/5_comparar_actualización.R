# Diego Villamil, OPI
# CDMX, 5 de julio de 2017

# Objetivo-  comparar los resultados de la actualización del índice MAGDA. 
# Los tres resultados para comparar son:  
#     el cálculo que se hizo inicialmente, 
#     la actualización del índice con nuevos datos y 
#     la actualización con una recalibración del modelo. 
# Entonces comparamos los tres, y el agente causal de las diferencias, aka.
# las transacciones de la CNBV. 


library(readxl)


### Lectura de CNBV ###

cnbv_0 <- read_csv("../data/cnbv/processed" %>% file.path(
    "x11_selecto_estados_enero.csv")) %>% 
  mutate_at("CVEENT", . %>% str_pad(2, "left", "0"))
cnbv_1 <- read_csv("../data/cnbv/processed/x11_selecto_estados_martes.csv")

cnbv_compara <- bind_rows(
  enero = cnbv_0, junio = cnbv_1, .id = "tabla")

gg_cnbv <- cnbv_compara %>%
  ggplot(aes(trimestre, atm_selecto, color = tabla)) + 
    facet_wrap(~CVEENT, scales = "free") +
    geom_line()
print(gg_cnbv)

ggsave(plot = gg_cnbv, "../visualization/figures/cnbv_confiltros_2.png",
       height = 9, width = 16, dpi = 100)


### Estimaciones MAGDA ###

magda_0 <- read_excel("../data/resultados/integrado" %>% 
    file.path("20170116_modelo_final.xlsx"), 
    sheet = "estatal_code", n_max = 78) %>% 
  gather("medida_año", "valor", 2:ncol(.)) %>% 
  mutate_at("medida_año", . %>% str_replace("_2", "#2")) %>% 
  separate(medida_año, c("medida", "año"), sep = "#") %>% 
  spread(medida, valor) 
  
magda_1 <- read_excel("../data/resultados/integrado" %>% 
      file.path("reportando_actualizado.xlsx")) %>% 
  gather("medida_año", "valor", 2:ncol(.)) %>% 
  mutate_at("medida_año", . %>% str_replace("_2", "#2")) %>% 
  separate(medida_año, c("medida", "año"), sep = "#") %>% 
  spread(medida, valor)
  
magda_2 <- read_excel("../data/resultados/integrado" %>% 
    file.path("reportando_recalibrado.xlsx")) %>% 
  gather("medida_año", "valor", 2:ncol(.)) %>% 
  mutate_at("medida_año", . %>% str_replace("_20", "#20")) %>% 
  separate(medida_año, c("medida", "año"), sep = "#") %>% 
  spread(medida, valor)
  
junta_magdas <- bind_rows(.id = "tabla",
  enero = magda_0, actualiza = magda_1, recalibra = magda_2)


zonas_metros <- junta_magdas %>% 
  group_by(zona_metro) %>% 
  summarize(magda = mean(magda, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(grupo = ntile(magda, 18)) %>% 
  arrange(-magda) %>% select(-magda)

gg_magdas <- junta_magdas %>%
  mutate(año = as.integer(año), 
    zona_metro = factor(zona_metro, levels = zonas_metros$zona_metro)) %>%
  ggplot(aes(año, magda, color = tabla)) + 
    facet_wrap(~zona_metro, scales = "free_y") +
    geom_line() + 
    scale_y_continuous(labels = . %>% divide_by(1000)) + 
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
print(gg_magdas)

ggsave(plot = gg_magdas, 
  "../visualization/figures/show_and_tell/comparacion_magdas.png",
  height = 9, width = 16, dpi = 100)

gg_acum <- junta_magdas %>%
  mutate(año = as.integer(año),
    zona_metro = factor(zona_metro, levels = zonas_metros$zona_metro)) %>%
    filter(!is.na(anual_acum)) %>% 
  ggplot(aes(año, anual_acum, color = tabla)) + 
    facet_wrap(~zona_metro) +
    geom_line() + 
    theme(axis.text.x = element_text(angle=30, hjust=1))
print(gg_acum)

ggsave(plot = gg_acum, height = 9, width = 16, dpi = 100, 
    "../visualization/figures/show_and_tell/comparacion_crecimientos.png")

