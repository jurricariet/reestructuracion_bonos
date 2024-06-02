# Tomo la evolución del CER y su pronóstico en el REM de la API del BCRA

# API BCRA: https://www.bcra.gob.ar/Catalogo/apis.asp?fileName=principales-variables-v1
library(httr)
library(tidyverse)
library(jsonlite)

variables <- GET(glue::glue('https://api.bcra.gob.ar/estadisticas/v2.0/PrincipalesVariables'))
lista_variables <- as.data.frame(fromJSON(rawToChar(variables$content)))

# La v2.0 de la API devuelve solo 1 año de observaciones

# Divido en períodos de un año:
fechas <- data.frame(fecha_inicio = seq.Date(as.Date('2020-01-01'),as.Date('2024-01-01'),by='1 year'),
                     fecha_fin = c(seq.Date(as.Date('2020-12-31'),as.Date('2023-12-31'),by='1 year'),Sys.Date()))
id_variable <- 30 # Variable CER


data = list()
for (i in 1:nrow(fechas)){

data_api <- GET(glue::glue('https://api.bcra.gob.ar/estadisticas/v2.0/DatosVariable/{id_variable}/{fechas[i,1]}/{fechas[i,2]}'))

data[[i]] <-  as.data.frame(fromJSON(rawToChar(data_api$content))$results) %>% 
  mutate(fecha = as.Date(fecha),
         valor = as.numeric(valor))
}

cer_serie <- do.call(bind_rows,data)

writexl::write_xlsx(cer_serie %>% select(-idVariable),'cer_serie.xlsx')


cer_serie %>% filter(fecha %in% (lubridate::dmy(c('4/9/2020',
                                                 '9/5/2021',
                                  '9/11/2021',
                                  '9/5/2022',
                                  '9/11/2022',
                                  '9/5/2023',
                                  '9/11/2023',
                                  '9/5/2024',
                                  '9/11/2024'
))-lubridate::days(10)))
