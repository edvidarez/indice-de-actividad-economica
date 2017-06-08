# Diego Villamil, OPI
# CDMX, 7 de junio de 2017

col_mun_0 <- cols("CVEMUN"="c", "nombre"="c", "LUMEN"="-", 
  "area"="d", "x175"="d", "LUMEN_loc"="-", "area_loc"="-",
  "x175_loc"="-")
col_loc_0 <- cols("CVELOC"="c", "nombre"="c", "LUMEN"="-", 
  "area"="d", "x175"="d")

col_loc_1 <- cols("CVELOC"="c", "nombre"="c", "area"="d", 
  "x175"="d")
col_mun_1 <- cols("CVEMUN"="c", "nombre"="c", "area"="d", "x175"="d",
  "area_loc"="-", "x175_loc"="-")

luces_0 <- read_csv("../data/referencias/mun_luces_175.csv", 
  col_types = col_mun_0)
luces_1 <- read_csv("../data/viirs/processed/mun_luces_175.csv", 
  col_types = col_mun_1) %>%
  mutate_at("CVEMUN", . %>% str_pad(5, "left", "0"))


luces_all <- full_join(luces_0, luces_1, by = "CVEMUN", 
  suffix = c("_0", "_1")) %>% 
  mutate(area_dif = (area_1 - area_0), 
      nombre_igual = (nombre_1 == nombre_0),
      luces_dif = (x175_1 - x175_0), 
      luces_na = x175_0 %>% replace(. == 0, NA), 
      area_cut = cut(area_0, labels = FALSE,
          breaks = quantile(area_0, (1:8)/8)))

areas_resumen <- luces_all %>% 
  group_by(area_sgn = sign(area_dif), area_cut) %>% 
  summarize(
      cuenta   = n(),
      area_tot = sum(area_0),
      area_dif = mean(area_dif), 
      area_rel = mean(area_dif/area_0), 
      area_abs = mean(abs(area_dif)), 
      area_abs_rel = mean(abs(area_dif/area_0)))

luces_resumen_3 <- luces_all %>% 
  group_by(area_sgn = sign(area_dif), area_cut) %>% 
  summarize(
      cuenta    = n(),
      area_tot  = sum(area_0),
      luces_dif = mean(luces_dif), 
      luces_rel = mean(luces_dif/luces_na, na.rm = T), 
      luces_abs = mean(abs(luces_dif)), 
      luces_abs_rel = mean(abs(luces_dif/luces_na), na.rm = T))

luces_resumen_2 <- luces_all %>% 
  group_by(area_cut) %>% 
  summarize(
      cuenta    = n(),
      area_tot  = sum(area_0),
      luces_dif = mean(luces_dif), 
      luces_rel = mean(luces_dif/luces_na, na.rm = T), 
      luces_abs = mean(abs(luces_dif)), 
      luces_abs_rel = mean(abs(luces_dif/luces_na), na.rm = T))

luces_resumen_1 <- luces_all %>% 
  group_by() %>% 
  summarize(
      cuenta    = n(),
      area_tot  = sum(area_0),
      luces_dif = mean(luces_dif), 
      luces_rel = mean(luces_dif/luces_na, na.rm = T), 
      luces_abs = mean(abs(luces_dif)), 
      luces_abs_rel = mean(abs(luces_dif/luces_na), na.rm = T))



