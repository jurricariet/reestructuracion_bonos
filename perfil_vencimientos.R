library(tidyverse)

# Levanto los cashflows en USD para calcular los perfiles de vencimientos
ruta <- 'Cashflow - Bonos - TABLEAU.xlsx'
hojas <- readxl::excel_sheets(ruta )

actual <- readxl::read_excel(ruta,sheet='1.2. Cashflow unificado - c Mon')

bonos_actual <- select(actual,c(fecha = 1,cupon_nacional = 'LEY NACIONAL',amortizacion_nacional='...13',
                                cupon_extranjera='LEY EXTRANJERA',amortizacion_extranjera='...26')) %>% 
  filter(!is.na(fecha)) %>% 
  mutate(cat = 'actual')


nuevos <- readxl::read_excel(ruta,sheet='4.2. Cashflow unificado - c Mon')
bonos_nuevos <- select(nuevos,c(fecha = 1,cupon_nacional = 'LEY NACIONAL',amortizacion_nacional='...11',
                                cupon_extranjera='LEY EXTRANJERA',amortizacion_extranjera='...24')) %>% 
  filter(!is.na(fecha))%>% 
  mutate(cat = 'reestructuración')


bonos_todos <- bind_rows(bonos_actual,bonos_nuevos)

bonos_todos_long <- bonos_todos %>% 
  pivot_longer(cols = c(cupon_nacional,cupon_extranjera,amortizacion_nacional,amortizacion_extranjera),
               names_to='var',values_to = 'valor') %>% 
  separate_wider_delim(var, "_", names = c("tipo", "legislacion"))


bonos_long_anio <- bonos_todos_long %>% 
  mutate(anio = lubridate::year(fecha)) %>% 
  group_by(anio,cat,legislacion) %>% 
  summarise(valor = sum(as.numeric(valor),na.rm=T))
  
ggplot()+
  geom_col(aes(x=anio,y=valor,fill=cat,group=cat),
           data = bonos_long_anio %>% filter(cat=='reestructuración'),
           width=.5,
           alpha=1,fill='#446590FF')+
  geom_col(aes(x=anio,y=valor,fill=cat,group=cat),
           data = bonos_long_anio %>% filter(cat=='actual'),
           width=.8,
           alpha=.3,fill='#AD8152FF')+
  facet_wrap(~legislacion)+
  scale_y_continuous(labels = scales::number_format(scale = 1/1e+6,suffix='M'))+
  theme_minimal()+
  theme(legend.position = 'top')+
    labs(x='',y='',title='Perfil de vencimientos de los bonos en dólares',
         subtitle = 'Millones de dólares. Incluye intereses y amortización')
ggsave('perfil_vencimientos.jpg',width = 8,height = 4)
