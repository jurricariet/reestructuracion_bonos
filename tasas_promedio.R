library(tidyverse)

# Creo una lista vacía para guardar los cash flows
bonos <- list()


ruta <- 'Propuesta Bonos.xlsx'

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

hojas <- hojas[str_detect(hojas, paste(lista_bonos$simbolo, collapse="|"))]

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
  mutate(simbolo = substr(simbolo,6,nchar(simbolo))) %>% 
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

# Guardo los datos en excel
#openxlsx::write.xlsx(bonos_df,'tasas_promedio_bonos.xlsx')


bonos_df %>% 
  ggplot(aes(x=fecha,y=tasa_prom,group=original,color=original))+
  geom_line()+
  facet_wrap(~ley)+
  labs(x='',y='',title='Tasa de interés promedio de los bonos')+
  theme_minimal()+
  ggthemes::scale_color_tableau(name='')+
  scale_y_continuous(labels = scales::percent_format())+
  theme(legend.position = 'top')


ggsave('tna_prom.jpg',width=8,height=4)

# Grafico de todos

bonos_data %>% 
  #filter(ley=='Extranjera') %>% 
  ggplot(aes(x=Plazo,y=TNA,group=simbolo,color=original))+
  geom_line()+
  facet_wrap(~ley)+
  labs(x='',y='',title='Tasa de interés de los bonos ley extranjera')+
  theme_minimal()+
  ggthemes::scale_color_tableau(name='')+
  scale_y_continuous(labels = scales::percent_format())+
  theme(legend.position = 'top')+
  geom_text(data=bonos_data %>% group_by(simbolo) %>% filter(Plazo == max(Plazo) | Plazo==min(Plazo)),
             aes(label=simbolo),show.legend =F,vjust=-.5)
ggsave('tna_todos.jpg',width=12,height=6)
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

