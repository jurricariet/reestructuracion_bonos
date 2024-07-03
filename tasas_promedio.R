library(tidyverse)

# Creo una lista vacía para guardar los cash flows
bonos <- list()


ruta <- 'Cashflow - Bonos - TABLEAU.xlsx'

# Cantidad de hojas en el excel

hojas <- readxl::excel_sheets(ruta )
# Lista de bonos
lista_bonos <- data.frame(simbolo=c('AL29',
                 'AL30',
                 'AL35',
                 'AE38',
                 'AL41',
                 'GD29',
                 'GD30',
                 'GD35',
                 'GD38',
                 'GD41',
                 'TAN34',
                 'TAN35',
                 'TAN43',
                 'TAN44',
                 'SOB34',
                 'SOB35',
                 'SOB43',
                 'SOB44',
                 'VM43',
                 'SOB43'))

hojas <- hojas[hojas %in% lista_bonos$simbolo]

# Recorro las hojas con los cashflow y las voy guardando en la lista
for (i in 1:length(hojas)){
  print(hojas[i])
  tabla <- readxl::read_excel(ruta,sheet=hojas[i])
  tabla <- tabla %>%  select(c('Plazo','TNA'))
  tabla$simbolo = hojas[i]
  #tabla$ley = lista_bonos[i-1,'ley'] %>% pull(ley)
  bonos[[i]] <- tabla
  rm(tabla)
}


# Uno todos los cashflow en un solo data frame

bonos_df <- do.call(rbind,bonos) %>% 
  filter(!is.na(Plazo) & (!is.na(TNA))) %>% 
  mutate(ley = ifelse(simbolo %in% c('AL29',
                                     'AL30',
                                     'AL35',
                                     'AE38',
                                     'AL41',
                                     'TAN34',
                                     'TAN35',
                                     'TAN43',
                                     'TAN44'),'Argentina','Extranjera'),
         original = ifelse(simbolo %in% c('AL29',
                                          'AL30',
                                          'AL35',
                                          'AE38',
                                          'AL41',
                                          'GD29',
                                          'GD30',
                                          'GD35',
                                          'GD38',
                                          'GD41'),'Actual','Reestructuración')) %>% 
  group_by(fecha = Plazo,ley,original) %>% 
  summarise(tasa_prom = mean(TNA))

bonos_df %>% 
  ggplot(aes(x=fecha,y=tasa_prom,group=original,color=original))+
  geom_line()+
  facet_wrap(~ley)+
  labs(x='',y='',title='Tasa de interés promedio de los bonos')+
  theme_minimal()+
  ggthemes::scale_color_tableau(name='')+
  scale_y_continuous(labels = scales::percent_format())+
  theme(legend.position = 'top')
