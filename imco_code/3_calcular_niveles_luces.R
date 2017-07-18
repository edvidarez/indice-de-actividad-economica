# Diego Villamil, OPI
# CDMX, 15 de diciembre de 2016

library(ggrepel)
library(RColorBrewer)

filter <- dplyr::filter

# Los datos de luces llegan a variar con QGIS. 
# USAR_REFERENCIA para utilizar el dato original.
usar_referencia <- TRUE

pibe_14 <- read_csv("../data/bie/processed/pibe.csv") %>% 
  filter(año == "2014-12-01")

if (usar_referencia) {
  muns_acteco <- read_csv("../data/referencias/mun_luces_175.csv",
        col_types = "ccdddddd") %>% 
    mutate(CVEENT = CVEMUN %>% str_sub(1, 2)) %>% 
    left_join(by="CVEENT", 
        pibe_14 %>% select(CVEENT, Estado, pibe)) %>% 
    group_by(CVEENT, Estado) %>% 
    mutate(ae_175 = pibe*x175_loc/sum(x175_loc)) %>%
    ungroup 
} else {
  muns_acteco <- read_csv("../data/viirs/processed/mun_luces_175.csv",
        col_types = "ccdddd") %>% 
    mutate(CVEENT = CVEMUN %>% str_sub(1, 2)) %>% 
    left_join(by="CVEENT", 
        pibe_14 %>% select(CVEENT, Estado, pibe)) %>% 
    group_by(CVEENT, Estado) %>% 
    mutate(ae_175 = pibe*x175_loc/sum(x175_loc)) %>%
    ungroup 
}


write_csv(muns_acteco %>% select(-pibe, -CVEENT), 
  "../data/resultados/acteco/por_municipio.csv")


## Gráfica de Luminosidad vs, PIBE, juntando ZMVM

edo_acteco <- muns_acteco %>% 
  select(CVEMUN, x175_loc, CVEENT, Estado, pibe) %>% 
  group_by(CVEENT, Estado) %>% 
  summarize(x175_loc = sum(x175_loc), 
            pibe = max(pibe)) 

edo_zmvm <- edo_acteco %>%
  mutate(zmvm = ifelse(str_detect(Estado, "México"), 
      "ZMVM", "no_ZMVM")) %>% 
  group_by(zmvm) %>% 
  summarize_at(vars(x175_loc, pibe), funs(sum)) %>% 
  filter(zmvm == "ZMVM") %>% rename(Estado = zmvm) %>% 
  mutate(CVEENT = "09_15")

edo_acteco_ <- edo_acteco %>% 
  filter(!str_detect(Estado, "México")) %>% 
  bind_rows(edo_zmvm)

 
gg_lineup <- edo_acteco_ %>% 
  ggplot(aes(x175_loc, pibe, color = Estado)) + 
    geom_point() + 
    geom_smooth(method = "lm", color = "cornflowerblue", se=FALSE) +
    scale_x_log10("Luminosidad (escala log)", 
        labels = . %>% divide_by(1e3)) +
    scale_y_log10("PIBE (escala log)", labels = . %>% divide_by(1e3)) + 
    scale_color_manual(values = brewer.pal(8, "Dark2") %>% rep(4)) +
    geom_text_repel(aes(label = Estado)) + 
    theme(legend.position = "none")
print(gg_lineup)

ggsave(plot = gg_lineup, 
    "../visualization/figures/scatter_luces_log.png", 
    width = 16, height = 9, dpi = 100)

muns_metro <- read_csv("../data/referencias" %>% file.path(
      "zonas_metro_estado_ok.csv")) %>% 
  mutate(CVEMET = CVEMET %>% str_pad(3, "left", "0"),
      CVEENT = CVEENT %>% str_pad(2, "left", "0"),
      CVEMUN = CVEMUN %>% str_pad(5, "left", "0")) %>% 
  select(CVEMET, CVEENT, CVEMUN, zona_metro = nombre_corto)

acteco_metro <- muns_acteco %>% select(-CVEENT, -Estado) %>% 
  right_join(muns_metro, by="CVEMUN") %>% 
  group_by(CVEENT, CVEMET, zona_metro) %>% 
  summarize_at(.funs = funs(sum), 
    .cols = vars(ae_175, area, area_loc))

write_csv(acteco_metro, 
  "../data/resultados/acteco/por_zonas_metro.csv")














