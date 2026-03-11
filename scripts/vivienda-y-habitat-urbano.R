install.packages("foreign")

# Librerías! ----
library(tidyverse)
library(sf)
library(gghighlight)
library(remotes)
library(ggsankey)
library(foreign)



# Vivienda y hábitat urbano ----

# viviendas totales en Benito Juárez en 1990: 41557 lo sacamos del ITER_23XLS90 ----
# viviendas totales en Benito Juárez en 2020: 319754 lo sacamos de base.supermanzanas ---- 

files <- list.files('..')
df <- read.dbf(file = "../procesamiento-coati/datos/iter/iter_nal1990.dbf",
               as.is = T)
df <- read.dbf(file = "../procesamiento-coati/datos/iter/iter_nal1995.dbf",
               as.is = T)
df <- read.dbf(file = "../procesamiento-coati/datos/iter/iter_nal2000.dbf",
               as.is = T)
df <- read.dbf(file = "../procesamiento-coati/datos/iter/iter_nal2005.dbf",
               as.is = T)
df <- read.dbf(file = "../procesamiento-coati/datos/iter/iter_nal2010.dbf",
               as.is = T) %>% 
  as.tibble() 

df <- df [,1:46] 

df <- df %>% 
  subset(ENTIDAD == '23' &
           MUN == '005')

as.integer(df[1,c('T_VIVHAB')])

colnames(df)
# Cálculo de la tasa de crecimiento anual de 1990 a 2020
(((px/po)^(1/t))-1)*100

# Asiganción de variables
po <- 117
px <- 911503
t <- 50

# Crecimiento de la vivienda en Cancún desde 1980 ---

tampobcun <-tribble(~anio, ~numero_viviendas,
                    '1970', 117,
                    '1971', 845,
                    '1973', 2780,
                    '1975', 15122,
                    '1980', 37190,
                    '1990', 311696,
                    '1995', 419815,
                    '2000', 572973,
                    '2010', 661176,
                    '2020', 911503,)

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

