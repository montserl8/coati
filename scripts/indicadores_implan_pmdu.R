library(tidyverse)
library(sf)
theme_set(theme_minimal())
update_theme(text = element_text(family = 'Comic Sans'))
library(extrafont)

mz <- read_sf(implan, 
        Id(schema = 'base',
           table = 'manzanas'))

irregulares <- read_sf(siginplan,
                       Id(schema = '02_diagnostico',
                          table = 'asentamientos_irregulares'))

densidad_pob <- dbGetQuery(implan, 'with cte as ( select ano, sum(pobtot) as pobtot, sum(st_area(st_transform(a.geometry, 32616)))/10000 as area_ha
              from pmdu_espacial.censos_2010_2020 b
              inner join pmdu_espacial.manzanas_cvegeo a
              on a.cvegeo = b.cvegeo
              group by ano) 

select * , pobtot/area_ha as densidad_pob from cte') 

theme_update(axis.text = element_text(size = 14),
             axis.title = element_text(size = 16,
                                       face = 'bold'))
ggplot(densidad_pob) +
  geom_bar(aes(ano, 
                densidad_pob),
           stat = 'identity',
           fill = '#0097A9')+
  scale_x_continuous(labels = c (2010, 2020),
                     breaks = c (2010, 2020)) +
  scale_y_continuous(limits = c(0, 80),
                     breaks = seq (0, 80, 5))+
  labs(x = 'Año',
       y = 'Densidad poblacional (personas / hectárea)') +
  theme(text = element_text(family = 'Lato')) 

ggsave(filename = 'plots/a1-densidad-poblacional.png')
