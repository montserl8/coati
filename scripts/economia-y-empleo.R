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
# Funciones ----


# PIB de la ciudad
#  se mide sumando el valor de mercado de todos los bienes y servicios finales producidos en un país durante un periodo (generalmente un año o trimestre). La forma más común es el método del gasto: 


# Dependencia económica 
# (personas 0 a 12 años + viejitos / PEA)

iter %>% 
  filter(nom_ent == '23' & nom_mun == '005' & nom_loc == 'Total municipio') %>% 
  summarise(idk = p_0a2 + p)

