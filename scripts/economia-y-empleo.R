# Librerías -----
library(tidyverse)
library(sf)
library(gghighlight)
library(remotes)
library(ggsankey)

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
