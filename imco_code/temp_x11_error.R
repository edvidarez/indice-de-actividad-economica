
library(seasonal)

data_url <- "https://raw.githubusercontent.com/opintel/indice-de-actividad-economica/master/data/cnbv/processed/grupos_municipios_prex11.csv"
download.file(data_url, "temp.csv")

temp_df <- read_csv("temp.csv")

ts_0 <- temp_df %>% 
  filter(CVEMUN  == "01001") %$% 
  ts(banamex, start = c(2011, 4), frequency = 12)

seas_0 <- seas(ts_0, x11 = "")
