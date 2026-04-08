# Librerías -----
library(tidyverse)
library(sf)
library(gghighlight)
library(remotes)
library(ggsankey)
library(foreign)
library(terra)
library(tmap)
library(units)
library(biscale)
library(cowplot)

# Conexiones -----
supermanzanas <- read_sf(implan, 
                         Id (schema = 'base',
                             table = 'supermanzanas'))

censo_2020 <- tbl(src = implan,
                  Id (schema = 'coati',
                      table = 'censo_2020'))

manzanas <- tbl(siginplan,
                Id(schema = '00_base',
                   table = 'manzanas_codigos'))

iter_nal <- read_csv('../procesamiento-coati/datos/iter/iter_nal2020.csv') %>% 
  rename_with(tolower) %>% 
  mutate(across(c(longitud:last_col()),
                as.numeric))


# Dependencia económica -----

dependencia_economica <- iter_nal %>%
  filter(entidad == '23',
         mun == '005',
         nom_loc == 'Total del Municipio') %>%
  summarise(dependientes = p_0a2 + p_3a5 + p_6a11 + p_12a14 + p_60ymas,
            independientes = pea,
            dependencia = (dependientes) / independientes * 100) %>% 
  pivot_longer(c(dependientes, independientes, dependencia),
               names_to = 'situacion_economica',
               values_to = 'cantidad') %>% 
  mutate(porcentajes = ifelse(situacion_economica == 'dependencia', 
                              NA,
                              round (cantidad / sum(cantidad)*100, 1)))

dbWriteTable(implan,
             Id (schema = 'coati_tablas_finales',
                 table = 'g_dependencia_economica'),
             dependencia_economica,
             overwrite = T)

# Ingresos promedio de las personas 
