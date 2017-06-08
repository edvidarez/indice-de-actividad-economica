# Diego Villamil, OPI
# CDMX, 4 de mayo de 2017
# May the fourth be with you


# Verificar cuál es el último mes disponible en 
# http://portafolioinfo.cnbv.gob.mx/PUBLICACIONES/IO/Paginas/bm.aspx
ultimo_mes <- as.Date("2017-03-01")


tags_meses <- seq(as.Date("2011-04-01"), ultimo_mes, by="1 month") %>% 
  format("%Y%m")

url_bm <- "http://portafolioinfo.cnbv.gob.mx/" %>% str_c(
  "PortafolioInformacion/BM_Operativa_%s.xls")
archivo_bm <- "../data/cnbv/raw/BM_Operativa_%s.xls"

descarga_bm <- function (tag, url_temp, archivo_temp, ...) {
  url_ <- sprintf(url_temp, tag)
  archivo <- sprintf(archivo_temp, tag)
  download.file(url_, archivo, method="curl")
}

lapply(tags_meses, descarga_bm, url_bm, archivo_bm)




