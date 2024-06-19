install.packages('wbstats')
library(wbstats)
library(tidyverse)

debt<- wb_search("external debt")

debt_data <- wb_data('FI.RES.TOTL.DT.ZS')

top_10_2022 <- debt_data %>% 
  filter(date == 2022) %>% 
  arrange(FI.RES.TOTL.DT.ZS) %>% 
  slice(1:10)

library(directlabels)

debt_data %>% 
  filter(country %in% top_10_2022$country ) %>% 
  ggplot(aes(x=date,y=FI.RES.TOTL.DT.ZS,group=country,color=country))+
  geom_line()+
  coord_cartesian(clip='off')+
  geom_dl(aes(label = country), method = list(dl.combine( "last.points")), cex = 0.8)+
  theme_minimal()+
  theme(legend.position = 'none')

library(highcharter)

hchart(debt_data %>% 
         filter(country %in% top_10_2022$country,
                ),type = 'line',hcaes(x = date, y = FI.RES.TOTL.DT.ZS, group = country))
##########3
deuda <- 	wb_data('DT.DOD.DSTC.XP.ZS') %>% 
  filter(DT.DOD.DSTC.XP.ZS<100 & DT.DOD.DSTC.XP.ZS > 5 &
           date > 2002)

deuda %>% 
  ggplot(aes(x=as.factor(date),y=DT.DOD.DSTC.XP.ZS,group=country,color=country))+
  geom_line(alpha = ifelse(deuda$country == 'Argentina',1,.3))+
  scale_color_manual(values = c('Argentina'='blue','grey'))+
  coord_cartesian(clip='off')+
  geom_dl(aes(label = country), method = list(dl.combine( "last.points")), cex = 0.8)+
  theme_minimal()+
  theme(legend.position = 'none')

###########
cc<- wb_search("current account")
cc_data <- 	wb_data('BN.CAB.XOKA.GD.ZS') %>% 
  filter(date > 2002) %>%
  filter(country %in% c('Argentina','Brazil','Uruguay','Chile','Bolivia','Paraguay','Venezuela'))

cc_data %>% 
   ggplot(aes(x=as.factor(date),y=BN.CAB.XOKA.GD.ZS,group=country,color=country))+
  geom_line(alpha = ifelse(cc_data$country == 'Argentina',1,.7))+
  #scale_color_manual(values = c('Argentina'='blue','grey'))+
  geom_hline(yintercept = 0,color='black')+
  coord_cartesian(clip='off')+
  geom_dl(aes(label = country), method = list(dl.combine( "last.points")), cex = 0.8)+
  theme_minimal()+
  theme(legend.position = 'none')
