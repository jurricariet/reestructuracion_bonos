# Precio GGAL vs. tasa de política monetaria
library(tidyverse)
library(yfR)
library(lubridate)

ticker <- c('GGAL.BA')
fecha_desde <- Sys.Date() - months(36)
fecha_hasta <- Sys.Date()

#Descargo los datos de los tickers seleccionados

df_acciones <- yf_get(tickers = ticker,
                           first_date = fecha_desde,
                           last_date = fecha_hasta)

df_acciones <- df_acciones %>%
  select(ticker,fecha=ref_date,precio = price_adjusted)

# Consulta en la API del BCRA la tasa de política monetaria

library(jsonlite)
library(httr)
variables <- GET(glue::glue('https://api.bcra.gob.ar/estadisticas/v2.0/PrincipalesVariables'))
lista_variables <- as.data.frame(fromJSON(rawToChar(variables$content)))

# La v2.0 de la API devuelve solo 1 año de observaciones
fechas <- data.frame(fecha_desde = seq.Date(as.Date('2021-01-01'),as.Date('2024-01-01'),by='1 year'),
            fecha_hasta = seq.Date(as.Date('2021-12-31'),as.Date('2024-12-31'),by='1 year'))
id_variable <- 6

bcra <- list()
for (i in 1:nrow(fechas)){
  datos <- GET(glue::glue('https://api.bcra.gob.ar/estadisticas/v2.0/DatosVariable/{id_variable}/{fechas[i,"fecha_desde"]}/{fechas[i,"fecha_hasta"]}'))
  bcra[[i]] <-  as.data.frame(fromJSON(rawToChar(datos$content)))
}

data_bcra <- do.call(rbind,bcra) %>% 
  select(fecha = results.fecha,
         valor = results.valor) %>% 
  arrange(fecha) %>% 
  mutate(var_diaria = valor/lag(valor,1)-1,
         var = 'tasa_de_politica',
         fecha = as.Date(fecha)) 

df <- df_acciones %>% 
  arrange(fecha) %>% 
  mutate(var = 'precio GGAL',
         var_diaria = precio/lag(precio,1)-1) %>% 
  select(fecha, valor=precio,var_diaria,var) %>% 
  bind_rows(data_bcra)

# Selecciono las fechas con suba de tasas
subas_tasa <- data_bcra %>%
  filter(var_diaria >0) %>% 
  pull(fecha)

# 

c(subas_tasa,subas_tasa+days(1),subas_tasa+days(2)) %>% sort()

df_fechas <- data.frame(fecha = subas_tasa %>% sort(),
                        episodio = 1:length(subas_tasa))
# Para esas fechas, veo la variación del precio de GGAL (promedio 3 días desde la suba)
df_episodios <-  df %>% 
  left_join(df_fechas)
  
pre_episodios <- df[which(!is.na(df_episodios$episodio))-1,] %>% 
  filter(!is.na(var_diaria) & var == 'precio GGAL') %>% 
  mutate(episodio = 1:n(),
         momento = 'pre') 

episodios <- df %>% 
  left_join(df_fechas) %>% 
  group_by(var) %>% 
  fill(everything(), .direction = "down") %>% 
  group_by(episodio) %>% 
  slice(1:3) %>% 
  group_by(episodio) %>% 
  summarise(valor = mean(valor)) %>% 
  mutate(momento='post')

var_episodios <- pre_episodios %>% 
 bind_rows(episodios) %>% 
  filter(!is.na(episodio)) %>% 
  group_by(episodio) %>% 
  mutate(var_post = valor[momento=='post']/valor[momento=='pre']-1)

var_episodios %>% 
  filter(!is.na(fecha)) %>% 
  ggplot(aes(x=fecha,y=var_post))+
  geom_col(fill='#D5B048')+
  labs(x='',y='',title='Precio de la acción GGAL',
       subtitle='Variación del precio de la acción 3 días post episodios de subas de tasas')+
  theme_minimal()+
  scale_y_continuous(labels = scales::percent_format())+
  scale_x_date(date_labels = '%d-%m-%y')+
 theme(#axis.text.x = element_text(angle=90)
    legend.position='top')
#####################
# Gráfico de episodios
df_fechas_grafico <- c(subas_tasa,subas_tasa+days(1),subas_tasa+days(2),
                                        subas_tasa+days(3),subas_tasa+days(4),subas_tasa+days(5),
                       subas_tasa -days(1),subas_tasa - days(2), subas_tasa - days(3),
                                        subas_tasa - days(4),subas_tasa - days(5)) %>% sort()
df_fechas_grafico <- list()

for (i in 1:length(subas_tasa)){
  df_fechas_grafico[[i]] <- data.frame(fecha = seq.Date(subas_tasa[i]-days(7),subas_tasa[i]+days(7),by='1 day'),episodio= i)
}

df_fechas_grafico <- do.call(rbind,df_fechas_grafico)

df_fechas_grafico %>% 
  left_join(df) %>% 
  filter(!is.na(var_diaria)) %>% 
  ggplot(aes(x=fecha,y=var_diaria,group=var,color=var))+
  geom_line()+
  facet_wrap(~episodio,scales='free_x')+
  labs(x='',y='',title='Precio de la acción GGAL y tasa de política monetaria',
       subtitle='Variación diaria. Episodios de subas de tasas')+
  theme_minimal()+
  scale_y_continuous(labels = scales::percent_format())+
  scale_x_date(date_labels = '%d-%m-%y')+
  scale_color_manual(values=c('#173E69','#D5B048'),
                     labels=c('Tasa de política monetaria','precio GGAL'),
                     name='')+
  theme(#axis.text.x = element_text(angle=90)
    legend.position='top')
