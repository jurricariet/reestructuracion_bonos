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

bonos_data <- do.call(rbind,bonos) %>% 
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
                                          'GD41'),'Actual','Reestructuración'))
bonos_df <- bonos_data %>% 
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


# Recálculo con nuevos Bonos Soberanía
SOB34 <- readxl::read_excel('Bonos SOBERANÍA.xlsx',sheet='SOB34') %>% 
  select(c('Plazo','TNA1','TNA2','TNA3','TNA4')) %>% 
  slice(-1) %>% 
  #filter(is.numeric(Plazo)) %>% 
  mutate(fecha = as.Date(as.numeric(Plazo),origin = "1899-12-30")) %>% 
  filter(!is.na(fecha)) %>% 
  select(fecha,SOB34_1=TNA1,SOB34_2=TNA2,SOB34_3=TNA3,SOB34_4=TNA4)

SOB44 <- readxl::read_excel('Bonos SOBERANÍA.xlsx',sheet='SOB44') %>% 
  select(c('Plazo','TNA1','TNA2','TNA3','TNA4')) %>% 
  slice(-1) %>% 
  #filter(is.numeric(Plazo)) %>% 
  mutate(fecha = as.Date(as.numeric(Plazo),origin = "1899-12-30")) %>% 
  filter(!is.na(fecha)) %>% 
  select(fecha,SOB44_1=TNA1,SOB44_2=TNA2,SOB44_3=TNA3,SOB44_4=TNA4)

# Tomamos promedio con opciones SOB34 Y SOB44:

opcion1 <- bonos_data %>% 
  filter(!simbolo %in% c('SOB34','SOB44')) %>% 
  bind_rows(SOB34 %>% select(Plazo=fecha,TNA=SOB34_1) %>%
              mutate(TNA = as.numeric(TNA)) %>% 
              mutate(simbolo='SOB34_1',
                     ley='Extranjera',
                     original='Reestructuración')) %>% 
  bind_rows(SOB44 %>% select(Plazo=fecha,TNA=SOB44_1) %>%
              mutate(TNA = as.numeric(TNA)) %>% 
              mutate(simbolo='SOB44_1',
                     ley='Extranjera',
                     original='Reestructuración')) %>% 
  group_by(fecha = Plazo,ley,original) %>% 
  summarise(tasa_prom = mean(TNA)) %>% 
  mutate(opcion = 'opción 1')

opcion2 <- bonos_data %>% 
  filter(!simbolo %in% c('SOB34','SOB44')) %>% 
  bind_rows(SOB34 %>% select(Plazo=fecha,TNA=SOB34_2) %>%
              mutate(TNA = as.numeric(TNA)) %>% 
              mutate(simbolo='SOB34_2',
                     ley='Extranjera',
                     original='Reestructuración')) %>% 
  bind_rows(SOB44 %>% select(Plazo=fecha,TNA=SOB44_2) %>%
              mutate(TNA = as.numeric(TNA)) %>% 
              mutate(simbolo='SOB44_2',
                     ley='Extranjera',
                     original='Reestructuración')) %>% 
  group_by(fecha = Plazo,ley,original) %>% 
  summarise(tasa_prom = mean(TNA)) %>% 
  mutate(opcion = 'opción 2')
opcion3 <- bonos_data %>% 
  filter(!simbolo %in% c('SOB34','SOB44')) %>% 
  bind_rows(SOB34 %>% select(Plazo=fecha,TNA=SOB34_3) %>%
              mutate(TNA = as.numeric(TNA)) %>% 
              mutate(simbolo='SOB34_3',
                     ley='Extranjera',
                     original='Reestructuración')) %>% 
  bind_rows(SOB44 %>% select(Plazo=fecha,TNA=SOB44_3) %>%
              mutate(TNA = as.numeric(TNA)) %>% 
              mutate(simbolo='SOB44_3',
                     ley='Extranjera',
                     original='Reestructuración')) %>% 
  group_by(fecha = Plazo,ley,original) %>% 
  summarise(tasa_prom = mean(TNA)) %>% 
  mutate(opcion = 'opción 3')
opcion4 <- bonos_data %>% 
  filter(!simbolo %in% c('SOB34','SOB44')) %>% 
  bind_rows(SOB34 %>% select(Plazo=fecha,TNA=SOB34_4) %>%
              mutate(TNA = as.numeric(TNA)) %>% 
              mutate(simbolo='SOB34_4',
                     ley='Extranjera',
                     original='Reestructuración')) %>% 
  bind_rows(SOB44 %>% select(Plazo=fecha,TNA=SOB44_4) %>%
              mutate(TNA = as.numeric(TNA)) %>% 
              mutate(simbolo='SOB44_4',
                     ley='Extranjera',
                     original='Reestructuración')) %>% 
  group_by(fecha = Plazo,ley,original) %>% 
  summarise(tasa_prom = mean(TNA)) %>% 
  mutate(opcion = 'opción 4')

opciones <- bind_rows(opcion1,opcion2) %>% 
  bind_rows(opcion3) %>% 
  bind_rows(opcion4) %>% 
  mutate()

opciones %>% 
  filter(ley=='Extranjera') %>% 
  ggplot(aes(x=fecha,y=tasa_prom,group=original,color=original))+
  geom_line()+
  facet_wrap(~opcion)+
  labs(x='',y='',title='Tasa de interés promedio de los bonos ley extranjera')+
  theme_minimal()+
  ggthemes::scale_color_tableau(name='')+
  scale_y_continuous(labels = scales::percent_format())+
  theme(legend.position = 'top')
##############
# Grafico de todos

todos <- bonos_data %>% 
  #filter(!simbolo %in% c('SOB34','SOB44')) %>% 
  bind_rows(SOB34 %>% 
              mutate(across(-fecha,as.numeric)) %>% 
              pivot_longer(cols=-fecha,names_to='simbolo',values_to='TNA') %>% 
              mutate(TNA = as.numeric(TNA)) %>% 
              mutate(ley='Extranjera',
                     original='Reestructuración',
                     Plazo=fecha)) %>% 
  bind_rows(SOB44 %>%
              mutate(across(-fecha,as.numeric)) %>% 
              pivot_longer(cols=-fecha,names_to='simbolo',values_to='TNA') %>% 
              mutate(TNA = as.numeric(TNA)) %>% 
              mutate(ley='Extranjera',
                     original='Reestructuración',
                     Plazo=fecha)) 


todos %>% 
  #filter(ley=='Extranjera') %>% 
  ggplot(aes(x=Plazo,y=TNA,group=simbolo,color=original))+
  geom_line()+
  facet_wrap(~ley)+
  labs(x='',y='',title='Tasa de interés de los bonos ley extranjera')+
  theme_minimal()+
  ggthemes::scale_color_tableau(name='')+
  scale_y_continuous(labels = scales::percent_format())+
  theme(legend.position = 'top')+
  ggrepel::geom_text_repel(data=todos %>% group_by(simbolo) %>% filter(Plazo == max(Plazo) | Plazo==min(Plazo)),
             aes(label=simbolo),show.legend =F,size=3,)

todos %>% 
  filter(original == 'Actual') %>% 
  ggplot(aes(x=Plazo,y=TNA,group=simbolo,color=simbolo))+
  geom_line()+
  facet_wrap(~ley)+
  labs(x='',y='',title='Tasa de interés de los bonos a reestructurar')+
  theme_minimal()+
  ggthemes::scale_color_tableau()+
  #ghibli::scale_color_ghibli_d(name = '')+
  scale_y_continuous(labels = scales::percent_format())+
  theme(legend.position = 'none')+
  ggrepel::geom_text_repel(data=todos %>% group_by(simbolo) %>% filter(original == 'Actual',Plazo == max(Plazo) | Plazo==min(Plazo)),
                           aes(label=simbolo),show.legend =F,size=3,)

