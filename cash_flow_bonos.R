library(tidyverse)

# Creo una lista vacía para guardar los cash flows
bonos <- list()


ruta <- 'C:/Users/jurricariet/Downloads/tp_bonos_montos_totales.xlsx'

# Cantidad de hojas en el excel

hojas <- length( readxl::excel_sheets(ruta ))

# Lista de bonos en la primera hoja
lista_bonos <- readxl::read_excel(ruta,sheet=1)

# Recorro las hojas con los cashflow y las voy guardando en la lista
for (i in 2:hojas){
tabla <- readxl::read_excel(ruta,sheet=i)
tabla$simbolo = lista_bonos[i-1,'simbolo'] %>% pull(simbolo)
tabla$ley = lista_bonos[i-1,'ley'] %>% pull(ley)
bonos[[i-1]] <- tabla
rm(tabla)
}

# Uno todos los cashflow en un solo data frame

bonos_df <- do.call(rbind,bonos)

# Guardo en excel

openxlsx::write.xlsx(bonos_df,'cashflow_unificados.xlsx')

# AGrupo los vencimientos por fecha
bonos_fecha <- bonos_df %>% 
  group_by(fecha,ley) %>% 
  summarise(intereses = sum(monto_interes),
            amortizacion = sum(monto_amortizacion))

# Guardo datos de vencimientos por fecha

openxlsx::write.xlsx(bonos_fecha,'venicimientos_fecha.xlsx')


bonos_fecha %>%
  pivot_longer(cols=c(intereses,amortizacion),names_to="tipo",values_to = "val") %>%
  ggplot(aes(as.Date(fecha),y=val,group=tipo,fill=tipo))+
  geom_col()+
  theme_minimal()+
  ggthemes::scale_fill_economist(name='')+
  scale_y_continuous(labels = scales::number_format(scale = 1/1e+6,big.mark = '.'))+
  theme(legend.position = 'top')+
  scale_x_date(date_breaks='1 year',date_labels = '%Y')+
  labs(x='',y='',title='Vencimientos de bonos en dólares',
       subtitle='En millones de USD')


bonos_fecha %>%
  pivot_longer(cols=c(intereses,amortizacion),names_to="tipo",values_to = "val") %>%
  ggplot(aes(as.Date(fecha),y=val,group=tipo,fill=tipo))+
  geom_col()+
  theme_minimal()+
  ggthemes::scale_fill_economist(name='')+
  scale_y_continuous(labels = scales::number_format(scale = 1/1e+6,big.mark = '.'))+
  theme(legend.position = 'top')+
  scale_x_date(date_breaks='3 years',date_labels = '%Y')+
  facet_wrap(~ley)+
  labs(x='',y='',title='Vencimientos de bonos en dólares',
       subtitle='En millones de USD')



bonos_anio <- bonos_df %>% 
  mutate(anio = lubridate::year(fecha)) %>% 
  group_by(anio,ley) %>% 
  summarise(intereses = sum(monto_interes),
            amortizacion = sum(monto_amortizacion)) %>% 
  ungroup() 

bonos_anio %>%
  pivot_longer(cols=c(intereses,amortizacion),names_to="tipo",values_to = "val") %>%
  ggplot(aes(as.factor(anio),y=val,group=tipo,fill=tipo))+
  geom_col()+
  theme_minimal()+
  ggthemes::scale_fill_economist(name='')+
  scale_y_continuous(labels = scales::number_format(scale = 1/1e+6,big.mark = '.'))+
  theme(legend.position = 'top',
        axis.text.x = element_text(angle=90))+
  facet_wrap(~ley)+
  labs(x='',y='',title='Vencimientos de bonos en dólares',
       subtitle='En millones de USD. Por jurisdicción.')

