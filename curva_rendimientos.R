library(tidyverse)



ruta <- 'C:/Users/jiurr/Downloads/tp_bonos.xlsx'

lista_bonos <- readxl::read_excel(ruta,sheet=1)


lista_bonos %>% 
  filter(moneda=='USD') %>% 
  ggplot(aes(x=duration_mod,y=tir,color=ley))+
  geom_point()+
  geom_text(aes(label=simbolo),vjust=1.2,show.legend = F)+
  geom_smooth(method = "glm", formula = y~x,
              method.args = list(family = gaussian(link = 'log')),linetype='dashed',se = F)+
  scale_y_continuous(labels=scales::percent_format())+
  theme_minimal()+
  scale_color_manual(name='',values = c('#75aadb','#fcbf45'))+
  theme(legend.position = 'top')+
  labs(y='TIR (%)',x='Duration modificada',title='Bonos en USD',
       subtitle = 'Curva de rendimientos según ley')
  