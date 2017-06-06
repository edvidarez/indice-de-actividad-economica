# Diego y Alejandro, OPI
# CDMX, 14 de enero de 2017


itaee <- read_csv("../data/bie/processed/itaee.csv") %>% 
  select(trimestre = fecha, CVEENT, Estado, itaee = original)
cnbv <- read_csv("../data/cnbv/processed/por_estados.csv") %>% 
  select(trimestre, CVEENT, atm_1)
x11 <- read_csv("../data/cnbv/processed/estados_x11_input.csv") 

datos_crec <- itaee %>% 
  inner_join(cnbv, by = c("CVEENT", "trimestre")) %>% 
  inner_join(x11,  by = c("CVEENT", "trimestre"))


## Una linda grafiquilla. 

gg_crecimiento <- datos_crec %>%
    rename(cajeros = atm_1) %>% 
    gather("Serie", "valor", factor_key = TRUE, 
        itaee, banamex_x11) %>% 
    group_by(Estado, Serie) %>% 
    mutate(valor_1 = valor/mean(valor)*100) %>%
  ggplot(aes(trimestre, valor_1)) + 
    facet_wrap(~ Estado, nrow = 5) + 
    geom_line(aes(color = Serie)) + 
    ggtitle("Crecimiento de actividad") + 
    labs(x = "", y = "") +
    scale_color_brewer(palette = "Dark2") +
    theme(axis.text.x = element_text(angle=45, hjust=1), 
        legend.position = c(5/7, 1/7))
print(gg_crecimiento)


