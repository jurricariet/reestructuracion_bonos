library(tidyverse)
library(ggtext)
data <- readr::read_csv('https://api.alphacast.io/datasets/5293/data?apiKey=ak_b4clHOA8NEK7Gv5q5S5R&$format=csv')

embi_selec <- data %>% 
  filter(Date > '2023-01-01') %>% 
  filter(country %in% c('Latin','Argentina','Brasil','Uruguay','Peru','México','Colombia')) %>% 
  janitor::clean_names()

embi_selec %>% 
  mutate(embi_global_diversified_subindices = 100*embi_global_diversified_subindices)%>% 
  ggplot(aes(x=date,y=embi_global_diversified_subindices,group=country,color=country))+
  geom_line()+
  theme_minimal()+
  theme(legend.position = 'bottom')+
  ggthemes::scale_color_tableau(name='')+
  scale_x_date(date_breaks = '6 months',date_labels = '%d/%m/%Y')+
  labs(x='',y='',title='JP Morgan EMBI index',
       subtitle='Países seleccionados y Latinoamérica')

ggsave('embi.jpg',scale=4)

#
#https://www.bcra.gob.ar/pdfs/PublicacionesEstadisticas/proyecciones-de-la-balanza-comercial-2024-2030.pdf

expo_proy <- data.frame(
  stringsAsFactors = FALSE,
                V1 = c("var","Balance comercial",
                       "Exportaciones totales","Minería","Combustibles",
                       "Granos y derivados","Resto de bienes","Importaciones totales"),
                V2 = c(2024L,22431L,89545L,6067L,
                       10411L,35379L,37688L,67114L),
                V3 = c(2025L,25242L,96526L,7727L,
                       13083L,35643L,40073L,71284L),
                V4 = c(2026L,29047L,104862L,8724L,
                       17619L,3591L,42609L,75815L),
                V5 = c(2027L,29242L,110438L,10263L,
                       18692L,36178L,45305L,81196L),
                V6 = c(2028L,31551L,118882L,11063L,
                       23199L,36449L,48172L,87331L),
                V7 = c(2029L,36390L,130731L,13715L,
                       29075L,36721L,5122L,94341L),
                V8 = c(2030L,41792L,143801L,15605L,
                       36739L,36996L,54461L,102008L)
)
names(expo_proy) <- expo_proy[1,]

expo_long <- pivot_longer(expo_proy,cols=-var,values_to='valor',names_to='anio')
expo_long %>% 
  filter(var %in% c('Exportaciones totales','Importaciones totales')) %>% 
  ggplot(aes(x=anio,y=valor,group=var,color=var))+
  geom_line()+
  geom_point()+
  geom_col(data = expo_long %>% 
             filter(var %in% c('Balance comercial')),aes(x=anio,y=valor),fill= '#99B898',color='#99B898')+
  labs(title='Proyección del Balance Comercial',
       subtitle="Millones de dólares.  <span style='color: #006C84'>Exportaciones</span>, <span style='color: #C67D58'>Importaciones</span> y <span style='color: #99B898'>Balanza</span>",
       caption = 'Fuente: Banco Central de la República Argentina',x='',y='')+
  scale_y_continuous(labels = scales::number_format(big.mark = '.'))+
  theme_minimal()+
  theme(legend.position = 'none',
        plot.subtitle = element_markdown())+
  scale_color_manual(values=c('#006C84','#C67D58'),name='')

ggsave('expo_proy.jpg',scale=4)
