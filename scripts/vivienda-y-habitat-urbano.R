
# Librerías! ----
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

# Conexiones y carga de datos ----

iter <- read_csv('../procesamiento-coati/datos/iter/iter_nal2020.csv') %>% 
  rename_with(tolower) %>% 
  mutate(across(c(longitud:last_col()),
                as.numeric))

agebs <- read_sf(implan, 
                 Id (schema = 'base',
                     table = 'agebs'))

supermanzanas <- read_sf(implan, 
                         Id (schema = 'base',
                             table = 'supermanzanas'))

limite_municipal <- read_sf(implan,
                            Id (schema = 'base',
                                table = 'limite_municipal'))

df_viviendas <- read_csv('../procesamiento-coati/datos/cuestionarios_ampliados/2020/Viviendas00.CSV')


# procesamiento del cálculo de la mancha urbana -----

manchas <- list.files(path = "../procesamiento-coati/datos/mancha_urbana/",
                      pattern = ".tif$",
                      recursive = T,
                      full.names = T)

manchas_procesadas <- lapply(manchas, function(mancha){ 
  
  print(mancha)
  raster_manchas <- rast(mancha)
  
  limite_municipal <- limite_municipal %>% 
    st_transform(crs(raster_manchas)) %>% 
    vect()
  # lo pasa a vect que es la clase para los vectores de terra
  corte_a_bj <- crop(raster_manchas, limite_municipal)
  
  
  manchas_urbanas <- app(corte_a_bj, function(x) ifelse(x == 0, NA, x))
  
  manchas_urbanas <- project(manchas_urbanas, "epsg:4326")
  
  #vectorizo
  mancha_vectorizada <- as.polygons(manchas_urbanas) %>% 
    st_as_sf() %>% 
    mutate(layer = mancha) %>% 
    group_by(layer) %>% 
    summarise(area = sum(st_area(geometry)))
  
  return(mancha_vectorizada)
})

manchas_procesadas <- manchas_procesadas %>% 
  bind_rows() %>% 
  arrange(desc(layer))

#aquí extraemos los dígitos de la variable layer para hacer una nueva columna (anio) con los años por layer
manchas_procesadas$anio <- str_extract(manchas_procesadas$layer,
                                       '\\d+')
#aquí cambiamos el tipo de dato de la variable anio a entero as.integer()
manchas_procesadas$anio <- as.integer(manchas_procesadas$anio)

# aquí cambiamos el área de m2 a ha con la función set units()
manchas_procesadas$area_ha <- set_units(manchas_procesadas$area, 'ha')

view(manchas_procesadas)

ggplot() +
  geom_sf(data =manchas_procesadas,
          aes(fill = anio),
          color = 'transparent') 
scale_fill_viridis_c()

# ttm es para cambiar entre plot e interactivo
ttm()  
qtm(manchas_procesadas,
    fill = 'layer')


# TO DO: 
# 1. sacar el tamaño poblacional de cancún de 1975
# 2. buscar tampob y viviendas habitadas particulares por año desde 1975,
# 3. ponerlos en un df

tampobcun <-tribble(~entidad, ~anio, ~tampob,
                    'Cancún', '1975', 15122,
                    'Cancún', '1980', 37190,
                    'Cancún', '1990', 311696,
                    'Cancún', '1995', 419815,
                    'Cancún', '2000', 572973,
                    'Cancún', '2010', 661176,
                    'Cancún', '2020', 911503)

tampobcun$anio <- as.integer(tampobcun$anio)

densidades <- manchas_procesadas %>%
  st_drop_geometry() %>% 
  select(-layer,
         -area) %>% 
  inner_join(tampobcun)

densidades$densidad_poblacional <- densidades$tampob/densidades$area_ha


# Viviendas totales en BJ (fuente: ITER y base.supermanzanas ) -----
# 1980: 8,429
# 1990: 41,557 
# 1995: 78832
# 2000: 105,530
# 2005: 147,914
# 2010: 246,307
# 2020: 319,754  

# Prueba de tamaños poblacionales de ciudades parecidas a Cancún, sigue en proceso
{library(tidyverse)
  
  iter <- read_csv('../procesamiento-coati/datos/iter/iter_nal2020.csv')
  
  colnames(iter)
  
  iter %>% 
    filter(NOM_LOC == 'Total del Municipio' &
             NOM_ENT %in% c( 'Jalisco', 'Quintana Roo', 'Michoacán de Ocampo') &
             NOM_MUN %in% c ('Benito Juárez', 'Puerto Vallarta', 'Lázaro Cárdenas') ) %>% 
    view()
  
  supermanzanas <- read_sf(implan, 
                           Id (schema = 'base',
                               table = 'supermanzanas'))
  censo_2020 <- tbl(src = implan,
                    Id (schema = 'coati',
                        table = 'censo_2020'))
  supermanzanas %>% 
    colnames()
}

{df <- read.dbf(file = "../procesamiento-coati/datos/iter/",
                as.is = T) %>% 
    as.tibble()
  
  colnames(df)
  
  df <- df [,1:130] 
  
  df_05 <- df %>% 
    subset(ENTIDAD == '23' &
             MUN == '005')
  
  as.integer(df_05[1,c('T_VIVHAB')])
}

# Asiganción de variables
po <- 41557
px <- 319754
t <- 2020-1990

# Cálculo de la tasa de crecimiento anual de 1990 a 2020
(((px/po)^(1/t))-1)*100

# Crecimiento de la vivienda en Cancún desde 1990 ---

viviendas <-tribble( ~anio, ~numero_viviendas,
                     1980, 8429,
                     1990, 41557,
                     1995, 78832,
                     2000, 106891,
                     2005, 147914,
                     2010, 246307,
                     2020, 319754)
densidades <- densidades %>% 
  inner_join(viviendas) 

densidades$densidad_viviendas <- densidades$numero_viviendas/densidades$area_ha

e
# Densidad de vivienda en Cancún y en Puerto Vallarta ¿por qué puerto Vallarta? ps pq se me antojó ----

df <- read_csv(file = "../procesamiento-coati/datos/iter/iter_nal2020.csv")
names(df)

df %>% 
  filter(LOC == '0000') %>% 
  filter(str_detect(string = NOM_MUN,
                    pattern = 'Puerto Vallarta')|
           NOM_MUN == 'Benito Juárez' &
           ENTIDAD == '23')


# Crecimiento porcentual de la vivienda en Cancún 
crecimiento_viviendas <- crecimiento_viviendas %>% 
  mutate(crecimiento = round(numero_viviendas - lag(numero_viviendas),1)/(numero_viviendas)* 100,
         etiqueta = paste(lag(anio), anio, sep = '-'))


dbWriteTable(implan,
             name = Id (schema = 'coati_tablas_finales',
                        table = 'ja_crecimiento_vivienda'),
             value = crecimiento_viviendas)
### Crecimiento de la mancha urbana  ----- 
### Infraestructura urbana vías primarias y secundarias ----
# las hizo Adrián 


### Espacios públicos con función de infraestructura urbana -----
muelles <- read_sf(implan,
                   Id (schema = 'coati_tablas_finales',
                       table = 'jc_muelles_malecon'))

vias <- read_sf(implan,
                Id (schema = 'coati_tablas_finales',
                    table = 'jb_vialidades'))

muelles <- muelles %>% 
  select(-fid)

vias <- vias %>% 
  rename(tipo = tipo_vialidad)

ep_infraestructura <- bind_rows(muelles, vias)
view(ep_infraestructura)

st_write(obj = ep_infraestructura,
         dsn = implan,
         layer = Id (schema = 'coati_tablas_finales',
                     table = 'j_ep_infraestructura'))

### Espacios públcos con función de equipamiento público -----

espacio_publico <- read_sf (siginplan,
                            Id (schema = '97_Datos_TrabajoContinuo',
                                table = 'espacio_publico_clasificacion'))
view(espacio_publico)
colnames(espacio_publico)
espaciopublico_equipamiento <- espacio_publico %>% 
  select(area_ha, geom, nombre, clasificacion_funcion) 

espacio_publico <- ggplot() +
  geom_sf(data = agebs, 
          fill = '#FCFCD3', 
          linewidth = 0.1) +
  geom_sf(data = espacio_publico, 
          aes(fill = area_ha), 
          color = 'grey20', 
          linewidth = 0.1) +
  scale_fill_gradientn(colours = c('#A1D991', '#358747', '#1b5e20'),
                       name = "Áreas verdes y
                       espacios públicos") +
  theme_minimal() 

st_write(obj = espaciopublico_equipamiento,
         dsn = implan,
         layer = Id (schema = 'coati_tablas_finales',
                     name = 'jd_espacio_publico_ep'),
         delete_layer = T)

### Tasa de crecimiento de la vivienda en Cancún ----
crecimiento_vivienda <- tbl(src = implan,
                            Id (schema = 'coati_tablas_finales',
                                table = 'je_tasa_crecimiento_vivienda'))
crecimiento_vivienda %>% 
  collect()

ggplot(crecimiento_vivienda) +
  geom_line(aes(ano, 
                viviendas,
                colour = tasa,
                linewidth = 3))

cambio_viviendas <- read_sf (implan,
                             Id (schema = 'coati_tablas_finales',
                                 table = 'je_cambio_viviendas_ageb'))

### Viviendas con energía eléctrica -----
# geom 
supermanzanas <- read_sf(implan,
                         Id (schema = 'base',
                             table = 'supermanzanas'))
viv_energia <- supermanzanas %>% 
  mutate(viviendas_totales = as.integer(tvivparhab),
         viviendas_con_el_servicio = as.integer(vph_c_elec),
         porcentaje_con_el_servicio = (viviendas_con_el_servicio/viviendas_totales) * 100) %>% 
  select(viviendas_con_el_servicio,
         porcentaje_con_el_servicio,
         id_supermanzana) %>% 
  mutate(servicio = 'Energía eléctrica') %>% 
  group_by()


st_write(obj = viv_energia,
         dsn = implan,
         layer = Id(schema = 'coati_tablas_finales',
                    table = 'jf_viviendas_energia_ubi'),
         delete_layer = T)

dbSendQuery(implan, 'drop table coati_tablas_finales.jf_viviendas_energia_ubi')
View(viv_energia)

### Viviendas por tipo de abastecimiento de agua  -----
# geom 
viv_agua <- supermanzanas %>% 
  mutate(viviendas_totales = as.integer(tvivparhab),
         viviendas_con_el_servicio = as.integer(vph_aeasp),
         viviendas_sin_agua_entubada = as.integer(vph_aguafv),
         porcentaje_con_el_servicio = (viviendas_con_el_servicio/viviendas_totales) * 100)%>% 
  select(viviendas_con_el_servicio,
         porcentaje_con_el_servicio,
         id_supermanzana) %>% 
  mutate(servicio = 'Abastecimiento de agua') %>% 
  group_by()

st_write(obj = viv_agua,
         dsn = implan,
         layer = Id (schema = 'coati_tablas_finales',
                     table = 'jg_viviendas_agua_ubi'),
         delete_layer = T)

### Viviendas con drenaje -----
#geom
viv_drenaje <- supermanzanas %>% 
  mutate(viviendas_totales = as.integer(tvivparhab),
         viviendas_con_el_servicio = as.integer(vph_drenaj),
         viviendas_sin_drenaje = as.integer(vph_nodren),
         porcentaje_con_el_servicio = (viviendas_con_el_servicio /viviendas_totales) * 100)%>% 
  select(viviendas_con_el_servicio,
         porcentaje_con_el_servicio,
         id_supermanzana) %>% 
  mutate(servicio = 'Drenaje') %>% 
  group_by()

st_write(obj = viv_drenaje,
         dsn = implan,
         layer = Id (schema = 'coati_tablas_finales',
                     table = 'jh_viviendas_drenaje_ubi'),
         delete_layer = T)

## juntamos los servicios (agua, energía y drenaje a pesar de que ya había un valor así pipipi)

services <- bind_rows(viv_agua, 
                      viv_drenaje,
                      viv_energia)

view(services)

st_write(obj = services,
         dsn = implan,
         layer = Id (schema = 'coati_tablas_finales',
                     table = 'jf_servicios_espacializado'))

dbSendQuery(implan, 'drop table coati_tablas_finales.jh_viviendas_drenaje_ubi')
### Servicios ----
servicios <- tbl(src = implan,
                 Id (schema = 'coati_tablas_finales',
                     table = 'jf_servicios'))

servicios_viv <- servicios %>% 
  rename(cuenta = servicio)  %>%
  group_by(cuenta) %>% 
  mutate(servicio = case_when(cuenta == 'Sí cuenta con electricidad' ~ 'Electricidad',
                              cuenta == 'No cuenta con electricidad' ~ 'Electricidad',
                              n == '534' ~ 'Electricidad',
                              n == '1714' ~ 'Electricidad',
                              n == '109155' ~ 'Electricidad',
                              cuenta == 'Sí cuenta con agua entubada dentro de la vivienda' ~ 'Abastecimiento de agua',
                              cuenta == 'Cuenta con agua entubada sólo en el patio o terreno' ~ 'Abastecimiento de agua',
                              cuenta == 'No tienen agua entubada' ~ 'Abastecimiento de agua',
                              n == '600' ~ 'Abastecimiento de agua',
                              n == '1735' ~ 'Abastecimiento de agua',
                              n == '112746' ~ 'Abastecimiento de agua',
                              cuenta == 'Cuenta con drenaje a la red pública' ~ 'Drenaje',
                              cuenta == 'Su drenaje da a alguna fosa séptica, barranca, río, lago o mar' ~ 'Drenaje',
                              cuenta == 'No tiene drenaje' ~ 'Drenaje',
                              n == '717' ~ 'Drenaje',
                              n == '1800' ~ 'Drenaje',
                              n == '114796' ~ 'Drenaje')) %>% 
  rename(situacion = cuenta) %>% 
  collect()

dbWriteTable(conn = implan,
             name = Id (schema = 'coati_tablas_finales',
                        table = 'jf_servicios'),
             value = servicios_viv,
             overwrite = T)

## Estado de la vivienda  -----

estado_vivienda <- tbl(src = implan,
                       Id (schema = 'coati',
                           table = 'vivienda_2020')) %>% 
  filter(ENT == '23',
         MUN == '005') %>% 
  mutate(estado_de_vivienda = case_when(TENENCIA == '1' ~ 'Propia',
                                        TENENCIA == '2' ~ 'Rentada',
                                        TRUE ~ 'No especificado')) %>% 
  group_by(estado_de_vivienda) %>% 
  count(wt = FACTOR) %>% 
  group_by() %>% 
  mutate(porcentaje = n/sum(n)*100) 

estado_vivienda <- collect(estado_vivienda) %>% 
  view()

dbWriteTable(conn = implan,
             name = Id (schema = 'coati_tablas_finales',
                        name = 'ji_estado_de_vivienda'),
             value = estado_vivienda)

## Costo de la vivienda  ----- 
costo_vivienda <- read_sf (implan,
                           Id (schema = 'coati_tablas_finales',
                               table = 'jj_costo_vivienda_hex'))
## ENIGH Adrián


## Porcentaje de viviendas desocupadas  -----  

viv_desocupadas <- supermanzanas %>% 
  select(tvivpar, vivpar_des) %>% 
  mutate(porcentaje_de_viviendas_desocupadas = vivpar_des/tvivpar *100,
         viviendas_desocupadas = as.integer(vivpar_des)) %>% 
  select(-vivpar_des, -tvivpar)

ggplot()+
  geom_sf(data = viv_desocupadas,
          aes(fill= porcentaje_de_viviendas_desocupadas)) +
  scale_fill_viridis_c(limits =c(0,100))

st_write(obj = viv_desocupadas,
         dsn = implan,
         layer = Id (schema = 'coati_tablas_finales',
                     table = 'jk_viviendas_desocupadas'))

## Total hogares  -----  

total_hogares <- supermanzanas %>% 
  rename(hogares_totales = tothog) %>% 
  select(hogares_totales) %>% 
  summarise(total_hogares =(sum(hogares_totales,
                                na.rm = TRUE)))

st_write(obj = total_hogares,
         dsn = implan,
         layer = Id (schema = 'coati_tablas_finales',
                     table = 'jl_total_hogares'))

hogares <- sum(total_hogares$tothog, na.rm = T)
sum(total_hogares$vivtot, na.rm = T)

### Total viviendas -----

viviendas_tot <- supermanzanas %>% 
  select(vivtot) %>% 
  mutate(vivtot = as.integer(vivtot)) %>% 
  rename(viviendas_totales = vivtot)

st_crs(agebs)
st_crs(viviendas_tot)

ggplot ()+
  geom_sf(data = agebs,
          fill =  NA,
          color = 'pink',
          linewidth = 0.1) +
  geom_sf(data = viviendas_tot,
          aes(fill = viviendas_totales),
          color = NA) +
  scale_fill_gradientn(
    colours = c ('#5AADE9', '#004779', '#002E60'),
    name = "Viviendas totales") +
  
  
  st_write(obj = viviendas_tot, 
           dsn = implan, 
           layer = Id (schema = 'coati_tablas_finales',
                       table = 'jb_viviendas_totales'))


volumen <- rast('../procesamiento-coati/datos/volumen_construido/GHS_BUILT_V_E2025_GLOBE_R2023A_54009_100_V1_0_R7_C10.tif')

vol_limitebj <- volumen %>% 
  crop(st_transform(limite_municipal,
                    st_crs(volumen)))

vol_limitebj <- vol_limitebj %>% 
  project('epsg:4326')

supermanzanas
agebs

# estadística zonal: por cada zona se puede aplicar una estadística
suma_volumen <- zonal(vol_limitebj,
                      vect(agebs),
                      fun = sum) %>%
  as_tibble()

supermanzanas$volumen <- suma_volumen$GHS_BUILT_V_E2025_GLOBE_R2023A_54009_100_V1_0_R7_C10
agebs$volumen <- suma_volumen$GHS_BUILT_V_E2025_GLOBE_R2023A_54009_100_V1_0_R7_C10

qtm(supermanzanas, fill = 'densidad_volumen')
# ttm es un switch (entre plot y view)
ttm()

supermanzanas$densidad_volumen <- supermanzanas$pobtot/supermanzanas$volumen

mapa_bivariado <- function(df,
                           style,
                           dim,
                           pal){
  
  df_bivariado <- bi_class(df %>% 
                             filter(!is.na(pobtot)),
                           x = pobtot,
                           y = volumen,
                           style = style,
                           dim = dim,
                           keep_factors = F,
                           dig_lab = 1,
                           na_rm = T)
  leyenda <- bi_legend(
    pal = pal,
    dim = dim,
    xlab = "Población",
    ylab = 'Volumen',
    size = 8
  )
  
  mapa_bivariado <- ggplot()+
    geom_sf(data = df_bivariado,
            mapping = aes(fill = bi_class),
            color = "white",
            size = 0.1,
            show.legend = F)+
    bi_scale_fill(pal = pal, dim = 2)+
    labs(
      title = "Población y volumen de vivienda",
      subtitle = 'Benito Juárez, Q. Roo',
      caption = 'Fuente: INEGI (Censo, 2020) y Global Human Settlement')+
    bi_theme(
    )
  
  # combine map with legend
  finalPlot <- ggdraw() +
    draw_plot(mapa_bivariado, 0, 0, 1, 1) +
    draw_plot(leyenda, 0.2, .65, 0.2, 0.2)
  
  return(finalPlot)
  
}

mapa_bivariado(height, 'jenks', 3, pal = 'PinkGrn')


st_drivers(what = 'vector',
           regex = 'parquet')

altura <- read_sf(implan,
                  Id (schema = 'base',
                      table = 'altura_edificios'))

height <- st_join(altura %>% 
                    st_make_valid(),
                  supermanzanas %>% 
                    select(id_supermanzana,
                           pobtot),
                  join = st_intersects,
                  left = F) %>% 
  st_drop_geometry() %>% 
  group_by(id_supermanzana) %>% 
  summarise(height = sum(height))

height <- height %>% 
  left_join(supermanzanas) 

height <- height %>% 
  st_as_sf()

names(height)


# y cómo haces los cortes a mano?



# viviendas con acceso a por lo menos 1 espacio público a menos de 300 m según la onu ------
supermanzanas %>% 
  select(id_supermanzana, 
         vivtot) %>% 
  st_contains(geom())

deep <- read_sf(implan,
                Id(schema = 'deep',
                   table = 'espacios_publicos'))


# Hacinamiento ----
df_viviendas <- read_csv('../procesamiento-coati/datos/cuestionarios_ampliados/2020/Viviendas00.CSV')

hacinamiento <- df_viviendas %>% 
  filter(ENT == '23' &
           MUN == '005') %>% 
  mutate(numero_personas_por_cuarto = NUMPERS/CUADORM,
         hacinamiento = ifelse(numero_personas_por_cuarto > 2.5, yes = 'Hacinamiento',
                               no = 'No hacinamiento')) %>% 
  group_by(hacinamiento) %>% 
  count(wt = FACTOR) %>% 
  ungroup() %>% 
  mutate(porcentaje = (n/sum(n))*100,
         porcentaje = round(porcentaje, 1)) 

dbWriteTable(implan,
             Id(schema = 'coati_tablas_finales',
                table = 'j_hacinamiento'),
             hacinamiento)

# Materiales de construcción de vivienda -----
# cuando paredes techos o pisos sean nulos ~ 'no especificado'

df_viviendas %>% 
  filter(ENT == '23' & MUN == '005') %>%
  mutate(rezago = case_when(PAREDES %in% c('1','2','3','4','5','6')|
                              TECHOS  %in% c('01','02','03','04','06','07','09') |
                              PISOS == '1' |
                              SERSAN %in% c ('2', '3') ~  'Rezago habitacional',
                            PAREDES %in% c('9', 'Nulo') &
                              TECHOS %in% c('99','Nulo') &
                              PISOS %in% c('9', 'Nulo') &
                              SERSAN %in% c('9', 'Nulo') ~ 'No especificado',
                            T ~ 'No rezago')) %>% 
  group_by(rezago) %>% 
  count(wt = FACTOR) %>% 
  view()


# si las paredes, techos y pisos de una habitación cumplen con estas características, entonces es rezago habitacional, si no, no
# si cualquiera tiene una ya es rezago


sum(supermanzanas$vivpar_ut, na.rm = T)
# 291083
# Viviendas de uso temporal ---------
ut <- iter %>%
  filter(entidad == '23' & mun == '005' & nom_loc == 'Total del Municipio') %>% 
  st_drop_geometry() %>%
  summarise(uso_temporal = sum(vivpar_ut, na.rm = T),
            deshabitadas = sum(vivpar_des, na.rm = T),
            habitadas = sum(tvivhab, na.rm = T)) %>% 
  pivot_longer(c(uso_temporal, deshabitadas, habitadas), 
               names_to = 'viviendas',
               values_to = 'cantidad') %>% 
  mutate(porcentajes = cantidad/sum(cantidad)*100) 

dbWriteTable(implan,
             Id(schema = 'coati_tablas_finales',
                table = 'j_porcentaje_viviendas'),
             value = ut)

viviendas <- supermanzanas %>% 
  select(vivpar_ut, vivpar_des, tvivhab, vivtot) %>% 
  mutate(porcentaje_usotemporal = (vivpar_ut/vivtot)*100,
         porcentaje_deshabitadas = (vivpar_des/vivtot)*100,
         porcentaje_habitadas = (tvivhab/vivtot)*100,
         across(contains('porcentaje'),
                \(x) ifelse(is.na(x)| is.nan(x), 0, x)),
         across(contains('porcentaje'),
                \(x)round(x,1))) %>% 
  select(-vivtot)

st_write(obj = viviendas,
         dsn = implan,
         layer = Id (schema = 'coati_tablas_finales',
                     table = 'j_porcentaje_viviendas_sf'))

ggplot()+
  geom_sf(data = viviendas,
          aes(fill= porcentaje_usotemporal)) +
  scale_fill_viridis_c(limits =c(0,100))


# Densidad de vivienda ------


# Viviendas habitadas por AGEB (tabla de resultados por AGEB)

df_iter <- read_csv('../procesamiento-coati/datos/iter/iter_nal2020.csv')

# Shapefile de AGEBs de Cancún
ageb_shp <- st_read('../datos/sig/ageb/cancun_ageb_urbana.shp')

densidad_vivienda <- df_iter %>%
  filter(ENT == '23' & MUN == '005') %>%
  select(CVEGEO, VIVPAR_HAB) %>%
  left_join(ageb_shp %>%
              mutate(area_ha = as.numeric(st_area(geometry)) / 10000) %>%
              st_drop_geometry() %>%
              select(CVEGEO, area_ha),
            by = 'CVEGEO'
  ) %>%
  mutate(
    viviendas_x_ha = VIVPAR_HAB / area_ha,
    viviendas_x_ha = round(viviendas_x_ha, 2)
  ) %>%
  filter(!is.na(viviendas_x_ha) & area_ha > 0)

# Rezago habitacional  ----
df_viviendas <- read_csv('../procesamiento-coati/datos/cuestionarios_ampliados/2020/Viviendas00.CSV')

df_personas <- read_csv(
  '../procesamiento-coati/datos/cuestionarios_ampliados/2020/Personas00.CSV'
)

# Componentes del rezago habitacional (metodología CONAVI)
# 1. Vivienda con materiales precarios en paredes o techos
# 2. Hacinamiento (> 2.5 personas por cuarto)
# 3. Sin agua entubada o drenaje

rezago_componentes <- df_viviendas %>%
  filter(ENT == '23' & MUN == '005') %>%
  mutate(
    precariedad_pared = MATPARED %in% c(1, 2, 3),   # materiales precarios
    precariedad_techo = MATTECHO %in% c(1, 2, 3),
    sin_agua          = DISPOCUA %in% c(3, 4),
    sin_drenaje       = DRENAJE  == 3,
    hacinamiento      = (NUMPERS / CUADORM) > 2.5,
    rezago            = precariedad_pared | precariedad_techo |
      sin_agua | sin_drenaje | hacinamiento
  ) %>%
  group_by(rezago) %>%
  count(wt = FACTOR) %>%
  ungroup() %>%
  mutate(
    porcentaje = round((n / sum(n)) * 100, 1)
  )

# Rezago por componente (para desglose) -----

rezago_desglose <- df_viviendas %>%
  filter(ENT == '23' & MUN == '005') %>%
  mutate(precariedad_pared = MATPARED %in% c(1, 2, 3),
         precariedad_techo = MATTECHO %in% c(1, 2, 3),
         sin_agua = DISPOCUA %in% c(3, 4),
         sin_drenaje = DRENAJE  == 3,
         hacinamiento = (NUMPERS / CUADORM) > 2.5) %>%
  summarise(
    across(
      c(precariedad_pared, precariedad_techo, sin_agua, sin_drenaje, hacinamiento),
      ~ sum(. * FACTOR, na.rm = TRUE) / sum(FACTOR) * 100
    )
  ) %>%
  pivot_longer(everything(), names_to = 'componente', values_to = 'porcentaje') %>%
  mutate(porcentaje = round(porcentaje, 1))

# Densidad habitacional ------
# Igual que densidad de vivienda pero con toda vivienda (habitada + deshabitada)
df_ageb <- read_csv('../datos/censo2020/ageb_urbana_23.csv')

ageb_shp <- st_read('../datos/sig/ageb/cancun_ageb_urbana.shp') %>%
  mutate(area_ha = as.numeric(st_area(geometry)) / 10000) %>%
  st_drop_geometry() %>%
  select(CVEGEO, area_ha)

densidad_habitacional <- df_ageb %>%
  filter(ENT == '23' & MUN == '005') %>%
  select(CVEGEO, TVIVPARHAB, VIVPAR_DES, VIVPAR_HAB) %>%
  left_join(ageb_shp, by = 'CVEGEO') %>%
  mutate(
    viv_total_x_ha   = round(TVIVPARHAB / area_ha, 2),
    viv_hab_x_ha     = round(VIVPAR_HAB  / area_ha, 2),
    tasa_desocupacion = round((VIVPAR_DES / TVIVPARHAB) * 100, 1)
  ) %>%
  filter(!is.na(viv_total_x_ha) & area_ha > 0)