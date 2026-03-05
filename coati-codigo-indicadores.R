# Librerías ----
library(tidyverse)
library(sf)
library(gghighlight)
library(remotes)
library(ggsankey)

supermanzanas <- read_sf(implan, 
                         Id (schema = 'base',
                             table = 'supermanzanas'))
censo_2020 <- tbl(src = implan,
                  Id (schema = 'coati',
                      table = 'censo_2020'))

manz_ageb <- tbl(siginplan,
                 Id(schema = '00_base',
                    table = 'manzanas_codigos'))

911503/1857985 * 100

porcentaje_tamano_pob <- porcentaje_tamano_pob %>% 
  slice(1:32) %>% 
  mutate(porcentajes = poblacion_total / sum(poblacion_total)*100)

view(porcentaje_tamano_pob)

# Población y sociodemográfica ----
## Tamaño poblacional de los municipios del estado de quintana roo 
tam_pob <- read_sf(implan, 
                   Id (schema = 'coati',
                       table = 'tamano_poblacion'))

# Cálculo del tamaño poblacional:
#Lo hicimos desde qgis:1) unimos capas vectoriales (entidades y municipios), 
#                      2) cambiamos los nombres de los campos (POB1 a poblacion_total y NOMGEO a entidad)
#                      3) conservamos campos de entidad y poblacion y eliminamos todas las otras columnas
#                      4) asignamos proyección (SRC 4326) y reproyectamos
#                      5) exportamos a PostgreSQL a conexión implan y esquema coati

tam_pob <- read_sf (implan,
                    Id ( schema = 'coati_tablas_finales',
                         table = 'a_tamano_poblacional'))

crecimientolatam <- tbl (src = implan,
                         Id (schema = 'coati_tablas_finales',
                             table = 'aa_tasa_crecimiento_AL'))
view(crecimientolatam)

## Tamaño_población total por edad y sexo ----
censo_2020 <- tbl(src = implan,
                  Id(schema =  'coati',
                     table = 'censo_2020')) 

ppl <- censo_2020 %>% 
  filter(ent == '23',
         mun == '005') %>%  
  mutate(sexo = case_when( sexo == '1' ~ 'Hombres',
                           sexo == '3' ~ 'Mujeres',
                           TRUE ~ 'No especificado'))
class(ppl)

ppl_df <- ppl %>% collect()
view(ppl_df)
ppl_df <- ppl_df %>%
  mutate(grupo_edad = cut(edad,
                          breaks = c(0, 4, 9, 14, 19, 24, 29, 34, 39, 44,
                                     49, 54, 59, 64, 69, 74, 79, 150),
                          labels = c("1-4", "5-9", "10-14", "15-19", "20-24",
                                     "25-29", "30-34", "35-39", "40-44", "45-49",
                                     "50-54", "55-59", "60-64", "65-69", "70-74",
                                     "75-79", "80+"),
                          right = TRUE,
                          include.lowest = TRUE)) %>%
  filter(!is.na(grupo_edad), sexo %in% c("Hombres", "Mujeres")) %>%
  group_by(grupo_edad, sexo) %>% 
  summarise(poblacion = sum(factor, na.rm = TRUE))

view(ppl_df)

ppl_plt <- ppl_df %>%
  mutate(poblacion_plot = if_else (sexo == "Hombres", -poblacion, poblacion)) %>%
  ggplot(aes(x = grupo_edad, y = poblacion_plot, fill = sexo)) +
  geom_col(width = 0.9) +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  labs(
    title = "Pirámide poblacional, Cancún – Benito Juárez (Q. Roo)",
    x = "Grupo de edad",
    y = "Población",
    fill = "Sexo") +
  theme_minimal() +
  theme(legend.position = "bottom")

ppl_plt

dbWriteTable(conn = implan, 
             name = Id (schema = 'coati_tablas_finales',
                        table = 'a_piramide_poblacional'),
             value = ppl_df)



# CIP´s

cip <- read_csv(file = 'datos/clasificaciones/cip.csv',
                col_names = TRUE)
cip <- cip %>% 
  mutate(tam_pob = as.integer(tampob)) %>% 
  select(-tampob) %>% 
  collect()

cip <- cip %>% 
  ungroup() %>% 
  group_by(cip_nombre) %>% 
  arrange(cip_nombre) %>% 
  mutate(tasa_crecimiento = round(tam_pob - lag(tam_pob),1)/(tam_pob)* 100 ) %>% 
  view()

cip<- cip %>% 
  group_by(cip_nombre) %>% 
  mutate(crecimiento = (tam_pob/lag(tam_pob))-1 ) %>% 
  group_by(cip_nombre) %>% 
  transmute(cambio = round((tam_pob - lag(tam_pob)), 1) / lag(tam_pob) * 100) %>% View()
view()

ggplot(cip) +
  geom_line(aes(anio, 
                tam_pob,
                color = cip_nombre,
                group = cip_nombre),
            linewidth = 1)

dbWriteTable(implan,
             name = Id(schema = 'coati_tablas_finales',
                       table = 'a_cip'),
             value = cip,
             overwrite = T)

p
### Densidad poblacional por zona y comparación con AL y México -
densidad_poblacion <- read_csv(file = 'datos/API_EN.POP.DNST_DS2_en_csv_v2_2718/API_EN.POP.DNST_DS2_en_csv_v2_2718.csv',
                               skip = 4)

densidad_region <- read_csv(file = 'datos/API_EN.POP.DNST_DS2_en_csv_v2_2718/Metadata_Country_API_EN.POP.DNST_DS2_en_csv_v2_2718.csv')

densidad_pob_AL <- densidad_poblacion %>% 
  left_join(densidad_region %>% 
              select(`Country Code`, Region)) %>% 
  filter(Region == 'Latin America & Caribbean') %>% 
  select(`Country Code`, `Country Name`, `1990`, `2000`, `2010`, `2020`)  %>% 
  pivot_longer(cols = where(is.numeric))

## Tasa de creimiento anual ----
densidad_pob_AL <- densidad_pob_AL %>% 
  arrange(`Country Name`, name) %>% 
  group_by(`Country Name`) %>% 
  summarise(tasa_de_crecimiento = value/lag(value), 
            tasa_de_crecimiento = tasa_de_crecimiento^(1/10),
            tasa_de_crecimiento = tasa_de_crecimiento - 1,
            tasa_de_crecimiento = tasa_de_crecimiento * 100,
            periodo = paste(lag(name), '-', name)) %>% 
  filter(!is.na(tasa_de_crecimiento)) %>% 
  rename(entidad = `Country Name`)

tasa_crecimiento_poblacional_cancun <-tribble(~entidad, ~periodo, ~tasa_de_crecimiento,
                                              'Cancún', '1990 - 2000', 9.1,
                                              'Cancún', '2000 - 2010', 4.5,
                                              'Cancún', '2010 - 2020', 3.3,)

densidades <- bind_rows(densidad_pob_AL,
                        tasa_crecimiento_poblacional_cancun)

densidades <- densidades %>% 
  ungroup() %>% 
  mutate(periodo = factor(periodo,
                          levels = c('1990 - 2000',
                                     '2000 - 2010',
                                     '2010 - 2020')))
dbWriteTable(conn = implan,
             name = Id (schema = 'coati_tablas_finales',
                        table = 'aa_tasa_crecimiento_AL'),
             value = densidades)

x <- tbl(src = implan,
         Id (schema = 'coati_tablas_finales',
             table = 'aa_tasa_de_crecimiento'))

tasa_crecimiento_poblacional_cancun

p <- ggplot() +
  geom_line(data = densidades %>% 
              filter(!`Country Name` %in% c('Mexico',
                                            'Cancún')),
            aes(periodo,
                tasa_de_crecimiento,
                color = `Country Name`,
                group = `Country Name`),
            show.legend = F,
            alpha = 0.5,
            linewidth = 0.3) +
  geom_line(data = densidades %>% 
              filter(`Country Name` %in% c('Mexico')),
            aes(periodo,
                tasa_de_crecimiento,
                color = `Country Name`,
                group = `Country Name`),
            show.legend = F,
            alpha = 1,
            linewidth = 1) +
  geom_line(data = densidades %>% 
              filter(`Country Name` %in% c('Cancún')),
            aes(periodo,
                tasa_de_crecimiento,
                color = `Country Name`,
                group = `Country Name`),
            show.legend = F,
            alpha = 1,
            linewidth = 2)

### Población total de Cancún por año desde 1980
# los números 1980 son del INEGI, hoja: Población total, por entidad federativa y municipio, según sexo
# y el resto de 1970 a 1975 son del libro QUINTANA ROO: CUATRO DÉCADAS DE VIDA MUNICIPIO INDEPENDIENTE

tampobcun <-tribble(~entidad, ~anio, ~tampob,
                    'Cancún', '1970', 117,
                    'Cancún', '1971', 845,
                    'Cancún', '1973', 2780,
                    'Cancún', '1975', 15122,
                    'Cancún', '1980', 37190,
                    'Cancún', '1990', 311696,
                    'Cancún', '1995', 419815,
                    'Cancún', '2000', 572973,
                    'Cancún', '2010', 661176,
                    'Cancún', '2020', 911503,)
plot(tampobcun, )
dbWriteTable(implan, 
             name = Id (schema = 'coati_tablas_finales',
                        table = ' aa_crecimiento_pob_cun'),
             value = tampobcun)

### Tasa de crecimiento necesita tener inicios y finales para poder hacer un join -
tasa_crecimiento <- tasa_crecimiento_poblacional_cancun %>%
  mutate(anio_inicio = as.integer(str_sub(periodo, 1, 4)),
         anio_fin    = as.integer(str_sub(periodo, 8, 11)))

tasa_crecimiento <- tasa_crecimiento %>%
  left_join(cantidad_poblacion_por_año %>%
              rename(poblacion_inicial = poblacion),
            by = c("Country Name" = "Country Name", 
                   "anio_inicio" = "periodo")) %>% 
  left_join(cantidad_poblacion_por_año %>%
              rename(poblacion_final = poblacion),
            by = c("Country Name" = "Country Name",
                   "anio_fin" = "periodo")) 
tasa_crecimiento <- tasa_crecimiento %>%
  mutate(incremento_absoluto = poblacion_final - poblacion_inicial)

dbWriteTable(conn = implan,
             name = Id (schema = 'coati_tablas_finales',
                        table = 'aa_tasa_de_crecimiento'),
             value = tasa_crecimiento)

plotly::ggplotly(p)


## Migración  ----
censo_2020 <- tbl(src = implan,
                  Id(schema = 'coati',
                     table = 'censo_2020'))

migra <- censo_2020 %>%
  filter(ent == '23', mun == '005') %>%
  select(ent_pais_nac, factor, sexo) %>%
  mutate(factor = as.numeric(factor),
         sexo = case_when(sexo == '3' ~ 'Mujer',
                          sexo == '1' ~ 'Hombre',
                          T ~ 'No especificado')) %>%   
  count(sexo, ent_pais_nac, wt = factor, name = "personas") %>%
  collect() 

pais <- read_csv("datos/clasificaciones/ENT_PAIS.csv",
                 locale = locale(encoding = "latin1"))

migra_entidad <- migra %>%
  left_join(pais %>% mutate(CLAVE = as.numeric(CLAVE)),
            by = c("ent_pais_nac" = "CLAVE")) %>%
  mutate(entidad = case_when(is.na(DESCRIPCION) ~ 'No especificado',
                             DESCRIPCION %in% c('No especificado', 
                                                'No especificado de Entidad Federativa', 
                                                'NA') ~ 'No especificado',
                             T ~ DESCRIPCION)) %>% 
  group_by(entidad, sexo) %>%
  summarise(personas = sum(personas, na.rm = TRUE)) %>%
  arrange(desc(personas))

migra_entidad$entidad[migra_entidad$entidad == 'México'] = 'Estado de México'

view(migra_entidad)

por_sexo <- migra_entidad %>% 
  pivot_wider(names_from = sexo, values_from = personas, values_fill = 0) %>%
  mutate(diferencia_h_m = Hombre - Mujer,
         poblacion_total = Hombre + Mujer) %>% 
  rename(hombre = Hombre,
         mujer = Mujer) %>% 
  collect() 

view(por_sexo)  

dbWriteTable(conn = implan,
             name = Id (schema = 'coati_tablas_finales',
                        table = 'c_migracion'),
             value = por_sexo,
             overwrite = T)

### Migración interna espacializada --
supermanzanas <- read_sf (implan, 
                          Id (schema = 'base',
                              table = 'supermanzanas')) 

migracion_interna <- supermanzanas %>% 
  select(pnacent, pnacoe, id_supermanzana) %>% 
  rename(id_sm = id_supermanzana) %>% 
  mutate(poblacion_nacida_cun = as.integer(pnacent),
         poblacion_foranea = as.integer(pnacoe),
         porcentaje_foraneos = (poblacion_foranea / (poblacion_foranea + poblacion_nacida_cun)) * 100,
         porcentaje_foraneos = ifelse(is.nan(porcentaje_foraneos), NA, porcentaje_foraneos)) %>% 
  select(-pnacent, -pnacoe)

st_write(obj = migracion_interna, 
         dsn = implan, 
         layer = Id (schema = 'coati_tablas_finales',
                     table = 'c_migracion_interna_ubicacion'),
         delete_layer = T)

colnames(manzanas_codigos)
migration <- manzanas_codigos %>% 
  select(NUM_SM, PNACENT, PNACOE, POBTOT, geometry) %>% 
  mutate(pob_nacida_cun = as.integer(PNACENT),
         pob_foraneo = as.integer(PNACOE))

migration <- migration %>% 
  mutate (porcentaje_foraneos = (PNACOE/(PNACENT+PNACOE))*100)

repaso_migra <- read_sf (implan,
                         Id (schema = 'coati_tablas_finales',
                             table = 'c_migracion'))

## Discapacidad ----
supermanzanas <- read_sf (implan, 
                          Id (schema = 'base',
                              table = 'supermanzanas'))

discapacitados <- supermanzanas %>% 
  select(pcon_disc, pobtot, geom, id_supermanzana) %>% 
  rename(id_sm =id_supermanzana) %>% 
  mutate(personas_discapacitadas = as.integer(pcon_disc),
         total_personas = as.integer(pobtot)) %>% 
  st_transform(4326) %>% 
  mutate(area_m2 = st_area(geom),
         area_ha = as.numeric(area_m2) / 10000,
         porcentaje_discapacitados = personas_discapacitadas/na_if(total_personas, 0) * 100) %>% 
  filter(!is.na(porcentaje_discapacitados)) %>% 
  select(-pcon_disc,
         -pobtot)

st_write(obj = discapacitados, 
         dsn = implan, 
         layer = Id (schema = 'coati_tablas_finales',
                     table = 'd_discapacidad'),
         delete_layer = T)


## Escolaridad -----
####  1990 

censo_1990 <- tbl(src = implan,
                  Id (schema = 'coati',
                      table = 'censo_1990'))

escolaridad90 <- censo_1990 %>% 
  mutate (ubicacion = ifelse(ENT=='23' & MUN== '005',
                             yes = 'Cancún',
                             no = 'Resto del país'),
          edad = as.integer(ANO_CUMP)) %>% 
  filter(edad >= 15) %>% 
  group_by(ubicacion, NIV_EST) %>% 
  count() 

escolaridad_1990 <- escolaridad90 %>% 
  group_by(ubicacion) %>% 
  mutate(porcentaje = n / sum(n) * 100 , 
         porcentaje = round (porcentaje, 2)) %>% 
  mutate(nivel_escolar = case_when(NIV_EST == '0'~ 'No especificado',
                                   NIV_EST == '1'~ 'Primaria',
                                   NIV_EST == '2'~ 'Secundaria',
                                   NIV_EST == '3'~ 'Preparatoria',
                                   NIV_EST == '4'~ 'Profesional',
                                   NIV_EST == '5'~ 'Posgrado')) %>%                                
  collect()

### grado de escolaridad 1990 ---

grado_promedio_1990 <- censo_1990 %>% 
  mutate(ubicacion = ifelse(ENT == '23' & MUN == '005',
                            yes = 'Cancún', 
                            no = 'Resto del país'),
         NIV_EST = as.integer(NIV_EST),     
         aprobado = as.integer(ANO_APRO),
         edad = as.integer(ANO_CUMP)) %>%  
  mutate(escolaridad = case_when( NIV_EST == 1 ~ aprobado,
                                  NIV_EST == 2 ~ aprobado + 6,
                                  NIV_EST == 3 ~ aprobado + 9,
                                  NIV_EST == 4 ~ aprobado + 12,
                                  NIV_EST == 5 ~ aprobado + 16,
                                  T ~ 99)) %>% 
  filter(edad >= 15,
         !is.na(escolaridad)) %>% 
  group_by(ubicacion) %>% 
  summarise(grado_promedio = mean(escolaridad, na.rm = TRUE)) %>% 
  mutate (anio = 1990L) %>% 
  collect()


####  2000 

censo_2000 <- tbl(src = implan,
                  Id (schema = 'coati',
                      table = 'censo_2000'))

escolaridad_2000 <- censo_2000 %>% 
  mutate (ubicacion = ifelse(ENT=='23' & MUN== '005',
                             yes = 'Cancún',
                             no = 'Resto del país')) %>%  
  group_by(ubicacion, NIVACAD) %>% 
  count(wt = as.integer(FACTOR))

escolaridad_2000 <- collect(escolaridad_2000)
unique(escolaridad_2000$NIVACAD)
escolaridad_2000

escolaridad_2000<- escolaridad_2000 %>% 
  group_by(ubicacion) %>% 
  mutate(porcentaje = n / sum(n) * 100 , 
         porcentaje = round (porcentaje, 2)) %>% 
  mutate(nivel_escolar = case_when(NIVACAD %in% c('0','9','b', '0,b') ~ 'No especificado',
                                   is.na(NIVACAD) ~ 'No especificado',
                                   NIVACAD == '1'~ 'Preescolar o Kinder',
                                   NIVACAD == '2'~ 'Primaria',
                                   NIVACAD == '3'~ 'Secundaria',
                                   NIVACAD == '4'~ 'Preparatoria',
                                   NIVACAD %in% c('5','6','7') ~ 'Profesional',
                                   NIVACAD == '8'~ 'Posgrado')) 

escolaridad_2000 <- escolaridad_2000 %>% 
  group_by(ubicacion,
           nivel_escolar) %>% 
  count(wt = n) %>% 
  group_by(ubicacion) %>% 
  mutate(porcentaje = n / sum(n) * 100) 

### grado promedio 2000 
grado_promedio_2000 <- censo_2000 %>% 
  mutate(ubicacion = ifelse(ENT == '23' & MUN == '005',
                            yes ='Cancún',
                            no = 'Resto del país'),
         ei = as.integer(ESCOACUM),
         pi = as.integer(FACTOR),
         edad = as.integer(EDAD)) %>% 
  filter(edad >= 15,
         !is.na(ei),
         !is.na(pi),
         pi > 0 ) %>% 
  group_by(ubicacion) %>% 
  summarise(grado_promedio =(sum(ei * pi)/ sum(pi))) %>% 
  mutate(grado_promedio = round(grado_promedio, 2),
         anio = 2000L) %>% 
  collect()

censo_2000 %>% 
  filter()

####  2010 --
censo2010 <- tbl(src = implan,
                 Id (schema = 'coati',
                     name = 'censo_2010'))

escolaridad_2010 <- censo2010 %>% 
  mutate(nivel_escolar = case_when(nivacad == '00' ~ 'No especificado',
                                   nivacad == '01' ~ 'Preescolar o Kinder',
                                   nivacad == '02' ~ 'Primaria',
                                   nivacad == '03' ~ 'Secundaria',
                                   nivacad == '04' ~ 'Preparatoria',
                                   nivacad %in% c('05',
                                                  '06',
                                                  '07',
                                                  '08',
                                                  '09',
                                                  '10') ~ 'Profesional',
                                   nivacad %in% c('11',
                                                  '12') ~ 'Posgrado',
                                   TRUE ~ 'No especificado'),
         ubicacion = ifelse(ent == '23' &
                              mun == '005',
                            yes = 'Cancún',
                            no = 'Resto del país')) %>% 
  group_by(ubicacion,
           nivel_escolar) %>% 
  count(wt = factor) %>% 
  group_by(ubicacion) %>% 
  mutate(porcentaje = n/sum(n)*100) %>% 
  collect()

### grado promedio 2010 -
grado_promedio_2010 <- censo2010 %>% 
  mutate(ubicacion = ifelse(ent == '23' & mun == '005',
                            yes ='Cancún',
                            no = 'Resto del país'),
         ei = as.numeric(escoacum),
         pi = as.numeric(factor),
         edad = as.integer(edad)) %>% 
  filter(edad >= 15,
         !is.na(ei),
         !is.na(pi),
         pi > 0) %>% 
  group_by(ubicacion) %>% 
  summarise(grado_promedio = sum(ei * pi) / sum(pi)) %>% 
  mutate(grado_promedio = round(grado_promedio, 2), anio = 2010L) %>% 
  collect()


####  2020 ---

censo_2020 <- tbl (src = implan,
                   Id (schema = 'coati',
                       table = 'censo_2020'))
escolaridad_2020 <- censo_2020 %>% 
  mutate(nivel_escolar = case_when(nivacad == '00' ~ 'No especificado',
                                   nivacad == '01' ~ 'Preescolar o Kinder',
                                   nivacad == '02' ~ 'Primaria',
                                   nivacad == '03' ~ 'Secundaria',
                                   nivacad == '04' ~ 'Preparatoria',
                                   nivacad %in% c('05',
                                                  '06',
                                                  '07',
                                                  '08',
                                                  '09',
                                                  '10',
                                                  '11') ~ 'Profesional',
                                   nivacad %in% c('12',
                                                  '13',
                                                  '14') ~ 'Posgrado',
                                   TRUE ~ 'No especificado'),
         ubicacion = ifelse(ent == '23' &
                              mun == '005',
                            yes = 'Cancún',
                            no = 'Resto del país')) %>%
  group_by(ubicacion,
           nivel_escolar) %>% 
  count(wt = factor) %>% 
  group_by(ubicacion) %>% 
  mutate(porcentaje = n / sum(n)* 100) %>% 
  collect()

escolaridad_1990$anio <- 1990L

class(1990) == 1990.0
class(1990.1L)


dbWriteTable(implan,
             Id(schema = 'coati_tablas_finales',
                table = 'e_escolaridad'),
             escolaridad_1990 %>% 
               select(-NIV_EST))

escolaridad_2000$anio <- 2000L

dbWriteTable(implan,
             Id(schema = 'coati_tablas_finales',
                table = 'e_escolaridad'),
             escolaridad_2000,
             append = T)

escolaridad_2010 <- escolaridad_2010 %>% 
  mutate(anio = 2010L)

escolaridad_dosmiles <- bind_rows(escolaridad_2010,
                                  escolaridad_2020 %>% 
                                    mutate(anio = 2020L))
view(escolaridad_dosmiles)
dbWriteTable(conn = implan,
             name = Id (schema = 'coati_tablas_finales',
                        table = 'e_escolaridad'),
             value = escolaridad_dosmiles,
             append= T)

## grado escolar 2020 (escolaridad acumulada) --
grado_promedio_2020 <- censo_2020 %>% 
  mutate(ubicacion = ifelse(ent == '23' & mun == '005',
                            yes ='Cancún',
                            no = 'Resto del país'),
         ei = as.numeric(escoacum),
         pi = as.numeric(factor),
         edad = as.integer(edad)) %>% 
  filter(edad >= 15,
         !is.na(ei),
         !is.na(pi),
         pi > 0) %>% 
  group_by(ubicacion) %>% 
  summarise(grado_promedio = sum(ei * pi) / sum(pi)) %>% 
  mutate(grado_promedio = round(grado_promedio, 2),
         anio = 2020L) %>% 
  collect()

censo_2010 %>% 
  mutate(ubicacion = ifelse(ent == '23' & mun == '005',
                            yes ='Cancún',
                            no = 'Resto del país'),
         ei = as.numeric(escoacum),
         pi = as.numeric(factor),
         edad = as.integer(edad)) %>% 
  filter(edad >= 15,
         !is.na(ei),
         !is.na(pi),
         pi > 0) %>% 
  group_by(ubicacion) %>% 
  summarise(grado_promedio = sum(ei * pi) / sum(pi)) %>% 
  mutate(grado_promedio = round(grado_promedio, 2),
         anio = 2010L)  
collect()

## escolaridad grado 
grado_2020 <- censo_2020 %>% 
  filter(ent == '23' & mun == '005') %>% 
  select(factor, edad, sexo, escolari) %>% 
  group_by(sexo)

## tabla comparativa de grado promedio a través del tiempo --

evolucion_grado_promedio <- bind_rows(grado_promedio_2000,
                                      grado_promedio_2010,
                                      grado_promedio_2020) %>% 
  select(anio, ubicacion, grado_promedio) %>% 
  arrange(ubicacion, anio)

evolucion_grado_promedio

library(ggplot2)

ggplot(evolucion_grado_promedio,
       aes(x = anio,
           y = grado_promedio,
           color = ubicacion,
           group = ubicacion)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = c(2000, 2010, 2020)) +
  labs(title = 'Grado promedio de escolaridad', 
       subtitle = 'Benito Juárez vs resto del país', 
       x = 'Año',
       y = 'Años de escolaridad',
       color = 'Ubicación') +
  theme_minimal()

dbWriteTable(conn = implan,
             name = Id (schema = 'coati_tablas_finales',
                        table = 'e_evolucion_escolaridad'),
             value = evolucion_grado_promedio)

e_escolaridad <- tbl (src = implan,
                      Id (schema = 'coati_tablas_finales',
                          table = 'e_escolaridad'))

view(e_escolaridad)

e_evolucion_escolaridad <- tbl (src = implan,
                                Id (schema = 'coati_tablas_finales',
                                    table = 'e_evolucion_escolaridad'))
view(e_evolucion_escolaridad)

## ¿Cuántos municipios totales hay en toda la república mexicana?

### Ranking asistencia escolar Cancún ----
censo_2020 <- tbl(src = implan,
                  Id (schema = 'coati',
                      table = 'censo_2020'))
asistencia <- censo_2020 %>%  
  filter(ent == '23' & mun =='005') %>% 
  filter(between(edad, 6, 18)) %>%
  mutate(asistencia = case_when(asisten == '1' ~ 'Sí',
                                asisten == '3' ~ 'No',
                                TRUE ~ NA_character_),
         factor = as.numeric(factor)) %>%
  filter(!is.na(asistencia)) %>%
  group_by(ent, mun, asistencia) %>%
  summarise(n = sum(factor)) %>% 
  mutate(porcentaje = n/sum(n)*100) %>% 
  collect()

# Asistencia escolar comparando con media nacional
asistencia_nal <- censo_2020 %>%  
  mutate(ubicacion = ifelse(ent == '23' & mun =='005',
                            yes = 'Cancún',
                            no = 'Media Nacional')) %>% 
  filter(between(edad, 6, 18)) %>% 
  mutate(asistencia = case_when(asisten == '1' ~ 'Sí van a la escuela',
                                asisten == '3' ~ 'No van a la escuela',
                                TRUE ~ NA_character_),
         factor = as.numeric(factor)) %>%
  filter(!is.na(asistencia)) %>% 
  select(ubicacion, asistencia, factor) %>% 
  group_by(ubicacion, asistencia) %>% 
  count(wt = factor) %>% 
  group_by(ubicacion) %>% 
  mutate(porcentaje = n/sum(n)*100) %>% 
  collect()

### Asistencia escolar comparando con el resto de Quintana Roo -----
asistencia_cun_qroo <- censo_2020 %>%  
  filter(ent == '23') %>% 
  filter(between(edad, 6, 18)) %>% 
  mutate(ubicacion = ifelse(ent == '23' & mun =='005',
                            yes = 'Cancún',
                            no = 'Resto de Quintana Roo'))
mutate(asistencia = case_when(asisten == '1' ~ 'Sí van a la escuela',
                              asisten == '3' ~ 'No van a la escuela',
                              TRUE ~ NA_character_),
       factor = as.numeric(factor)) %>%
  filter(!is.na(asistencia)) %>% 
  filter(ubicacion != 'Cancún') %>% 
  select(ubicacion, asistencia, factor) %>% 
  group_by(ubicacion, asistencia) %>% 
  count(wt = factor) %>% 
  group_by(ubicacion) %>% 
  mutate(porcentaje = n/sum(n)*100) %>% 
  collect()


asistencias <- bind_rows(asistencia_cun_qroo,
                         asistencia_nal) 

dbWriteTable(conn = implan,
             name = Id(schema = 'coati_tablas_finales',
                       table = 'e_asistencia_escolar'),
             value = asistencia_cun_qroo)
#Ranking de asistencia escolar de Benito Juárez en el estado de Quintana Roo
censo_2020 %>% 
  filter(ent == '23') %>% 
  mutate(municipio = case_when(mun == '001' ~ 'Cozumel',
                               mun == '002' ~ 'Felipe Carrillo Puerto',
                               mun == '003' ~ 'Isla Mujeres',
                               mun == '004' ~ 'Othón P. Blanco',
                               mun == '005' ~ 'Benito Juárez',
                               mun == '006' ~ 'José María Morelos',
                               mun == '007' ~ 'Lázaro Cárdenas',
                               mun == '008' ~ 'Solidaridad',
                               mun == '009' ~ 'Tulum',
                               mun == '010' ~ 'Bacalar',
                               mun == '011' ~ 'Puerto Morelos',
                               T ~ 'No especificado')) %>% 
  filter(between(edad, 6, 18)) %>%
  mutate(asistencia = case_when(asisten == '1' ~ 'Sí van a la escuela',
                                asisten == '3' ~ 'No van a la escuela',
                                TRUE ~ NA_character_),
         factor = as.numeric(factor)) %>%
  filter(!is.na(asistencia)) %>% 
  select(municipio, asistencia, factor) %>% 
  group_by(municipio, asistencia) %>% 
  count(wt = factor) %>% 
  group_by(municipio) %>% 
  mutate(porcentaje = n/sum(n)*100) %>% 
  arrange(desc(porcentaje)) %>% 
  view()

asistencia <- asistencia %>% 
  group_by(ent,
           mun) %>% 
  mutate(total = sum(n, na.rm = T)) %>% 
  filter(total > 195000) %>% 
  mutate(porcentaje_asistencia = n / total * 100) %>%
  ungroup() %>% 
  arrange(desc (porcentaje_asistencia)) %>% 
  collect()
view(asistencia)


pivot_wider(names_from = asistencia, values_from = porcentaje_asistencia) %>% 
  view() 
ggplot(mapping = aes(x = total, y = Sí)) + 
  geom_point()

### Asistencia escolar espacializada ----
asistencia_espacializada <- supermanzanas %>% 
  select(p3a5_noa,
         p6a11_noa,
         p12a14noa,
         p_3a5,
         p_6a11,
         p_12a14,
         geom,
         id_supermanzana) %>% 
  filter(!is.na(p3a5_noa),
         !is.na(p6a11_noa),
         !is.na(p12a14noa)) %>% 
  mutate(personas_no_asisten = p3a5_noa +p6a11_noa + p12a14noa,
         personas_totales = p_3a5 + p_6a11 + p_12a14,
         porcentaje_no_asisten = (personas_no_asisten/personas_totales)*100,
         porcentaje_3a5_que_no_asisten = (p3a5_noa/p_3a5)*100,
         porcentaje_6a11_que_no_asisten = (p6a11_noa/p_6a11)*100,
         porcentaje_12a14_que_no_asisten = (p12a14noa/p_12a14)*100) %>% 
  # select(-p3a5_noa,
  
  #       -p6a11_noa,
  #        -p12a14noa,
  #        -p_3a5,
  #        -p_6a11,
  #        -p_12a14) %>% 
  arrange(desc(personas_no_asisten))


asistencia_escolar_espacializada <- asistencia_espacializada %>% 
  mutate(across(contains('porcentaje'),
                \(x)round(x, 2)))

view(asistencia_espacializada)

st_write(obj = asistencia_espacializada,
         dsn = implan,
         layer = Id (schema = 'coati_tablas_finales',
                     table = 'e_escolaridad_espacializada'))

dbSendQuery(implan, 'drop table coati_tablas_finales.e_escolaridad_espacializada')

# Ranking nivel escolar a nivel estado de quintana roo

censo_2020 %>% 
  filter(ent == '23') %>% 
  mutate(ei = as.numeric(escoacum),
         pi = as.numeric(factor),
         edad = as.integer(edad)) %>% 
  filter(edad >= 15,
         !is.na(ei),
         !is.na(pi),
         pi > 0) %>% 
  group_by(ent, mun) %>% 
  summarise(grado_promedio = sum(ei * pi, na.rm = T ) / sum(pi, na.rm = T)) %>% 
  mutate(grado_promedio = round(grado_promedio, 2),
         anio = 2020L) %>% 
  collect() %>% 
  arrange(desc(grado_promedio)) %>% 
  view()


### Alfabetismo -----
censo_2020 <- tbl(src = implan,
                  Id (schema = 'coati',
                      table = 'censo_2020'))
alfabeta <- censo_2020 %>%  
  filter(ent == '23', mun == '005') %>% 
  filter(edad >= 15) %>%
  mutate(alfabeta = case_when(alfabet == '1' ~ 'Sabe leer y escribir (alfabeta)',
                              alfabet == '3' ~ 'No sabe leer ni escribir (analfabeta)',
                              TRUE ~ NA_character_),
         factor = as.numeric(factor)) %>%
  filter(!is.na(alfabeta)) %>%
  group_by(ent, mun, alfabeta) %>%
  summarise(n = sum(factor)) %>% 
  mutate(porcentajes = (n/sum(n))*100)

## personas de 8 a 14 años --- filter(between(edad, 6, 18)) 
censo_2020 %>% 
  filter(ent == '23', mun == '005') %>% 
  filter(between(edad, 3, 14)) %>%
  count(wt = factor)


alfabeta <- alfabeta %>% 
  group_by(ent,
           mun) %>% 
  mutate(total = sum(n, na.rm = T)) %>% 
  filter(total > 195000) %>% 
  mutate(porcentaje_alfabeta = n / total * 100) %>%
  ungroup() %>% 
  arrange(desc (porcentaje_alfabeta)) %>% 
  collect()
view(alfabeta)

### Analfabetismo espacializado ----
analfabetismo_espacializado <- supermanzanas %>% 
  select(p15ym_an,
         pob15_64,
         pob65_mas,
         id_supermanzana) %>% 
  filter(!is.na(p15ym_an),
         !is.na(pob15_64),
         !is.na(pob65_mas)) %>% 
  mutate(poblacion_total = pob15_64 + pob65_mas,
         porcentajes = (p15ym_an/poblacion_total) *100) %>% 
  rename(poblacion_analfabeta = p15ym_an) %>% 
  filter(!is.nan(porcentajes)) %>%
  arrange(desc(poblacion_analfabeta)) %>% 
  select(-pob15_64,
         -pob65_mas) %>% 
  view()

dbWriteTable(implan,
             name = Id(schema = 'coati_tablas_finales',
                       table = 'e_analfabetismo'),
             value = analfabetismo_espacializado,
             overwrite = T)

## Derechohabiencia comparado con la media nacional

censo_2020 <- tbl(src = implan,
                  Id (schema = 'coati',
                      table = 'censo_2020'))

derechohabiencia <- censo_2020 %>% 
  mutate(ubicacion = ifelse( ent == '23' & mun == '005', 
                             yes = 'Municipio de Benito Juárez, Quintana Roo',
                             no = 'Resto del país')) %>% 
  select(dhsersal1, dhsersal2, factor, ubicacion) %>%
  pivot_longer(cols = c(dhsersal1, dhsersal2),
               names_to = "variable",
               values_to = "codigo") %>% 
  filter(!is.na(codigo)) %>% 
  mutate(factor = as.numeric(factor),
         nombre_institucion = case_when(codigo %in% c(1, 5, 6) ~ 'IMSS',
                                        codigo %in% c(2, 3) ~ 'ISSSTE',
                                        codigo %in% c(4, 8) ~ 'Otra institución',
                                        codigo == 7 ~ 'Seguro privado',
                                        codigo == 9 ~ 'Sin afiliación',
                                        codigo == 99 ~ 'No especificado')) %>% 
  group_by(ubicacion, nombre_institucion) %>% 
  summarise(personas = sum(factor, na.rm = TRUE)) %>%  
  group_by(ubicacion) %>% 
  mutate(porcentaje = round(personas / sum(personas) * 100, 2)) %>% 
  arrange(desc(personas)) %>% 
  collect()

dbWriteTable(conn = implan,
             name = Id (schema = 'coati_tablas_finales',
                        table = 'f_derechohabiencia_por_institucion'),
             value = derechohabiencia,
             overwrite = TRUE)

### Derechohabiencia por sector  ----

dh <- censo_2020 %>% 
  filter(ent == '23', mun == '005') %>% 
  select(dhsersal1, dhsersal2, factor) %>%
  pivot_longer(!factor) %>% 
  filter(!is.na(value)) %>% 
  mutate(nombre_institucion = case_when(name == 'dhsersal1' & value == 1 ~ 'IMSS',
                                        name == 'dhsersal1' & value == 2 ~ 'ISSSTE',
                                        name == 'dhsersal1' & value == 3 ~ 'ISSSTE Estatal',
                                        name == 'dhsersal1' & value == 4 ~ 'PEMEX, Defensa o Marina',
                                        name == 'dhsersal1' & value == 5 ~ 'Seguro Popular, Siglo XXI o Bienestar',
                                        name == 'dhsersal1' & value == 6 ~ 'IMSS-BIENESTAR (antes IMSS-PROSPERA)',
                                        name == 'dhsersal1' & value == 7 ~ 'Seguro privado',
                                        name == 'dhsersal1' & value == 8 ~ 'Otra institución',
                                        name == 'dhsersal1' & value == 9 ~ 'Sin afiliación',
                                        name == 'dhsersal1' & value == 99 ~ 'No especificado',
                                        name == 'dhsersal2' & value == 1 ~ 'IMSS',
                                        name == 'dhsersal2' & value == 2 ~ 'ISSSTE',
                                        name == 'dhsersal2' & value == 3 ~ 'ISSSTE Estatal',
                                        name == 'dhsersal2' & value == 4 ~ 'PEMEX, Defensa o Marina',
                                        name == 'dhsersal2' & value == 5 ~ 'Seguro Popular, Siglo XXI o Bienestar',
                                        name == 'dhsersal2' & value == 6 ~ 'IMSS-BIENESTAR (antes IMSS-PROSPERA)',
                                        name == 'dhsersal2' & value == 7 ~ 'Seguro privado',
                                        name == 'dhsersal2' & value == 8 ~ 'Otra institución'),
         publico = case_when(nombre_institucion == 'Sin afiliación' ~ 'Sin afiliación',
                             nombre_institucion == 'Seguro privado' ~ 'Seguro privado',
                             nombre_institucion == 'Otra institución' ~ 'Otra institución',
                             nombre_institucion == 'No especificado' ~ 'No especificado',
                             TRUE ~ 'Público'))  %>% 
  group_by(publico) %>% 
  rename(afiliacion = publico) %>% 
  count(wt=factor) %>%
  ungroup() %>% 
  mutate(porcentajes = n / sum(n) * 100) %>% 
  collect()  

dh

dbWriteTable(conn = implan,
             name = Id (schema = 'coati_tablas_finales',
                        table = 'f_derechohabiencia'),
             value = dh)

## Derechohabiencia a salud espacializado ----
supermanzanas <- read_sf(implan,
                         Id (schema = 'base',
                             table = 'supermanzanas'))
dh_densidad 

dh_densidad <- supermanzanas %>% 
  select(psinder, pder_ss, pobtot, geom, id_supermanzana) %>% 
  filter(!is.na(pobtot)) %>% 
  filter(!is.na(psinder)) %>% 
  filter(!is.na(pder_ss)) %>% 
  filter(!pobtot == '0') %>% 
  filter(!id_supermanzana == '052') %>% 
  rename(id_sm = id_supermanzana) %>% 
  mutate(porcentaje_poblacion_sin_dh = (as.integer(psinder)/as.integer(pobtot))*100,
         porcentaje_con_dh = (as.integer(pder_ss)/as.integer(pobtot))*100) %>% 
  arrange(desc(porcentaje_poblacion_sin_dh)) %>% 
  view()

mutate(poblacion_condh = as.integer(pder_ss),
       poblacion_sindh = as.integer(psinder),
       poblacion_total = as.integer(pobtot)) %>% 
  st_transform(32616) %>%  ## para cálculo de densidad
  mutate(area_m2 = st_area(geom),
         area_ha = as.numeric(area_m2) / 10000,
         porcentaje_condh = (poblacion_condh /na_if(poblacion_total, 0)) *100,
         porcentaje_sin_derechohabiencia = (poblacion_sindh / na_if (poblacion_total, 0))*100,
         densidad_sin_dh = porcentaje_sin_derechohabiencia / na_if(area_ha,0),
         densidad_dh_ha = poblacion_condh/na_if(area_ha, 0)) %>% 
  select(-psinder,
         -pder_ss,
         -pobtot) %>% 
  filter(!is.na(densidad_dh_ha))
view()
ggplot(dh_densidad) +
  geom_sf(aes(fill = dh_densidad), color = NA) +
  scale_fill_viridis_c( option = 'C',
                        name = 'Densidad de derechohabiencia \n(personas / ha)',
                        na.value = 'transparent') +
  labs(title = 'Densidad de población con derechohabiencia a salud',
       subtitle = 'Personas con derechohabiencia por hectárea',
       caption = 'Fuente: INEGI – AGEBS') +
  theme_minimal()

st_write (obj = dh_densidad,
          dsn = implan,
          layer = Id (schema = 'coati_tablas_finales',
                      table = 'f_derechohabiencia_espacializado'),
          delete_layer = T)

## Derechohabiencia espacializado por tipo

## Población Económicamente Activa ----
# personas chambeadoras espacializadas

manz_ageb <- collect(manz_ageb)

pea <- manz_ageb |> 
  select(PEA, PE_INAC) |> 
  mutate(PEA = as.integer(PEA), 
         PE_INAC = as.integer(PE_INAC)) |> 
  summarise(PEA = sum(PEA,
                      na.rm = T),
            NOT_PEA = sum(PE_INAC,
                          na.rm = T),
            poblacion_total = PEA + NOT_PEA,
            porcentaje_pea = (PEA / poblacion_total) * 100,
            pocentaje_nopea = (NOT_PEA/poblacion_total)*100,
            ano = 2020)


dbWriteTable(conn = implan, 
             name = Id (schema = 'coati_tablas_finales',
                        table = 'g_pea'),
             value = pea)

## PEA de la encuesta

pea_cun <- censo_2020 %>% 
  filter(ent =='23',
         mun == '005') %>% 
  mutate(pea = case_when (conact ==  10 ~ 'Población económicamente activa',
                          conact ==  13 ~ 'Población económicamente activa',
                          conact ==  14 ~ 'Población económicamente activa',
                          conact ==  15 ~ 'Población económicamente activa',
                          conact ==  16 ~ 'Población económicamente activa',
                          conact ==  17 ~ 'Población económicamente activa',
                          conact ==  18 ~ 'Población económicamente activa',
                          conact ==  19 ~ 'Población económicamente activa',
                          conact ==  20 ~ 'Población económicamente activa',
                          conact ==  30 ~ 'Población desocupada',
                          conact ==  40 ~ 'Población desocupada',
                          conact ==  50 ~ 'Población desocupada',
                          conact ==  60 ~ 'Población desocupada',
                          conact ==  70 ~ 'Población desocupada',
                          conact ==  80 ~ 'Población desocupada',
                          conact ==  99 ~ 'Población desocupada',
                          conact ==  99 ~ 'Población desocupada',
                          TRUE ~ NA),
         sexo = case_when(sexo == '1' ~ 'Hombre',
                          sexo == '3' ~ 'Mujer')) %>% 
  filter(!is.na(pea))

pea_cun <- pea_cun %>% 
  collect() %>% 
  mutate(grupo_edad = cut(edad,
                          seq(0, 
                              130, 
                              5))) %>% 
  group_by(grupo_edad, sexo, pea) %>% 
  count(wt=factor)

pea_cun <- pea_cun %>% 
  group_by(pea) %>% 
  mutate(porcentaje = (n/sum(n))*100)

view(pea_cun)
dbWriteTable(conn = implan, 
             name = Id (schema = 'coati_tablas_finales',
                        table = 'g_pea_edad'),
             value = pea_cun)


pea_cun <- pea_cun %>% 
  ungroup()

pea_geom <- supermanzanas %>% 
  select(pea, pobtot, geom, id_supermanzana) %>% 
  rename(id_sm = id_supermanzana) %>% 
  mutate(porcentaje_pea = (pea/pobtot)*100) %>% 
  select(-pobtot)

st_write(obj = pea_geom,
         dsn = implan,
         layer = Id (schema = 'coati_tablas_finales',
                     table = 'g_pea_geom'),
         delete_layer = T)

## Estudiantes, ocupados y jubilados por rango de edad -------
censo_2020 %>% 
  filter(ent == '23' & mun == '23') %>% 
  mutate()


pob <- dbGetQuery(implan, "SELECT conact, edad, factor, sexo,
 
case 
	when conact = 10 then 'Trabaja'
	when conact = 13 then 'Trabaja'
	when conact = 14 then 'Jubilado'
	when conact = 15 then 'Estudia'
	when conact = 16 then 'Se dedica a los quehaceres del hogar'
	when conact = 17 then 'Trabaja'
	when conact = 18 then 'Trabaja'
	when conact = 19 then 'Trabaja' 
	when conact = 20 then 'Trabaja'
	when conact = 30 then 'Busca trabajo'
	when conact = 40 then 'Jubilado'
	when conact = 50 then 'Estudia'
	when conact = 60 then 'Se dedica a los quehaceres del hogar'
	when conact = 70 then 'Está incapacitado para trabajar'
	when conact = 80 then 'No trabaja'
	else 'No especificado'
	end as actividad,
case 
	when edad >18 and edad <35 then 1
	when edad >=35 and edad <60 then 2
	when edad >=60 then 3
	else 0
end as grupo_edad

FROM coati.censo_2020
WHERE ent = '23' AND mun = '005' 
and conact is not null") 


pob$breaks <- cut(pob$edad,
                  seq(min(pob$edad),
                      max(pob$edad),
                      5),
                  include.lowest=T)

pob <- pob %>%
  group_by(breaks,
           actividad,
           sexo) %>% 
  count(wt = factor)  %>%
  #mutate(
  #       etapa = case_when(grupo_edad == 0 ~ 'Infante (0-18 años)',
  #                         grupo_edad == 1 ~ 'Joven (18-35 años)',
  #                         grupo_edad == 2 ~ 'Adulto (35-60 años)',
  #                         grupo_edad == 3 ~ 'Persona mayor (60 años o más)')) %>% 
  group_by(breaks)%>% 
  mutate(genre = case_when(sexo == 1 ~ 'Hombres',
                           sexo == 3 ~ 'Mujeres'))

gente <- pob %>% 
  ungroup() %>% 
  group_by(genre,
           breaks) %>% 
  mutate(porcentajes = n/sum(n)*100)

p <- ggplot(gente %>% 
              filter(!actividad %in% c('No especificado',
                                       'Desempleado',
                                       'Está incapacitado para trabajar')), 
            aes(x = breaks, 
                node = actividad,
                fill = actividad,
                value = n,
                group = actividad,
                label = actividad)) +
  geom_sankey_bump(type = 'alluvial') +
  facet_wrap(~sexo,
             ncol = 1)+
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.grid.major.y = element_line(color = 'grey80'),
    panel.grid.minor = element_line(color = 'grey80',
    )
  ) +
  theme_sankey()
view(gente) 

dbWriteTable(conn = implan,
             name = Id(schema = 'coati_tablas_finales',
                       table = 'g_estudia_trabaja_jubilado'),
             value = gente)

## Marginación -------
indice_marginacion <- read_sf (siginplan,
                               Id (schema ='02_diagnostico',
                                   table ='Indice_marginacion'))
im_sm <- indice_marginacion_sm %>% 
  select(pobtot, gm_2020, geom, id_supermanzana) %>% 
  rename(poblacion_total = pobtot,
         grado_marginacion = gm_2020,
         id_sm = id_supermanzana) %>% 
  filter(poblacion_total>0) %>% 
  filter(!is.na(grado_marginacion))

view(im_sm)

st_write (obj = im_sm,
          dsn = implan,
          layer = Id (schema = 'coati_tablas_finales',
                      table = 'h_marginacion'),
          delete_layer = T)

dbSendQuery(implan, 'drop table base.indice_marginacion_sm')

porcentajes_por_gm <- indice_marginacion %>% 
  select(GM_2020, POB_TOTAL) %>% 
  group_by(GM_2020) %>%
  rename(grado_marginacion = GM_2020) %>% 
  summarise(personas_porgm = sum(as.integer(POB_TOTAL), na.rm = TRUE)) %>% 
  mutate(porcentaje_marginacion = personas_porgm / sum(personas_porgm) * 100) 

margi <- read_sf (implan,
                  Id (schema ='coati_tablas_finales',
                      table = 'h_marginacion_porcentaje'))
dbWriteTable(implan,
             Id(schema = 'coati_tablas_finales',
                name = 'h_marginacion_porcentaje'),
             value = porcentajes_por_gm,
             overwrite = T )

marginacion <- st_transform(x = marginacion,
                            crs = 4326)
ggplot(margi) +
  geom_sf(aes(fill = margi), color = NA) +
  scale_fill_viridis_c( option = 'C',
                        name = 'Densidad de derechohabiencia \n(personas / ha)',
                        na.value = 'transparent') +
  labs(title = 'Densidad de población con derechohabiencia a salud',
       subtitle = 'Personas con derechohabiencia por hectárea',
       caption = 'Fuente: INEGI – AGEBS') +
  theme_minimal()

st_write(obj = marginacion,
         dsn = implan,
         layer = Id (schema = 'coati_tablas_finales',
                     table = 'h_marginacion'))

## Población afrodescendiente e indígena en Cancún
censo_2020 %>% 
  filter(ent == '23',
         mun == '005') %>% 
  select(factor,
         afrodes,
         perte_indigena) %>% 
  mutate(afroindigena = case_when(afrodes == 1 &
                                    perte_indigena == 1  ~ 'Afroindigena',
                                  afrodes == 1 & perte_indigena == 3  ~ 'Afro',
                                  afrodes == 3 & perte_indigena == 1 ~ 'Indigena',
                                  afrodes == 3 & perte_indigena == 3 ~ 'Mestizo',
                                  T ~ NA)) %>% 
  group_by(
    afroindigena
  ) %>% 
  count(wt = factor)

## Población afrodescendiente e ind

## Indígena o afrodescendiente ----
# Población afrodescendiente e indígena nacional
afros <- censo_2020 %>% 
  select(afrodes, factor) %>% 
  pivot_longer(!factor) %>% 
  filter(!is.na(value)) %>% 
  group_by(value) %>% 
  count(wt = factor) %>% 
  group_by() %>% 
  mutate(ascendencia = case_when(value == '1' ~ 'Afrodescendiente',
                                 value == '3' ~ 'No Afrodescendiente',
                                 T ~ 'No especificado'),
         porcentaje = n/sum(n)*100) %>% 
  mutate(ubicacion = 'Resto de México') %>% 
  collect()

indigenas <- censo_2020 %>% 
  select(perte_indigena, factor) %>% 
  pivot_longer(!factor) %>% 
  filter(!is.na(value)) %>% 
  group_by(value) %>% 
  count(wt = factor) %>% 
  group_by() %>% 
  mutate(ascendencia = case_when(value == '1' ~ 'Indígena',
                                 value == '3' ~ 'No indígena',
                                 T ~ 'No especificado'),
         porcentaje = n/sum(n)*100,
         ubicacion = 'Resto de México') %>% 
  collect()

ascendencia <- bind_rows (afros, indigenas) %>% 
  select(-value)

### afrodescendientes de Cancún
afro_cun <-censo_2020 %>% 
  filter(ent == '23' & mun == '005') %>% 
  select(afrodes, factor) %>% 
  pivot_longer(!factor) %>% 
  filter(!is.na(value)) %>% 
  group_by(value) %>% 
  count(wt = factor) %>% 
  group_by() %>% 
  mutate(ascendencia = case_when(value == '1' ~ 'Afrodescendiente',
                                 value == '3' ~ 'No Afrodescendiente',
                                 T ~ 'No especificado'),
         porcentaje = n/sum(n)*100,
         ubicacion = 'Benito Juárez, Q.Roo') %>% 
  collect()

### indígenas de Cancún
ind_cun <- censo_2020 %>% 
  filter(ent == '23' & mun == '005') %>% 
  select(perte_indigena, factor) %>% 
  pivot_longer(!factor) %>% 
  filter(!is.na(value)) %>% 
  group_by(value) %>% 
  count(wt = factor) %>% 
  group_by() %>% 
  mutate(ascendencia = case_when(value == '1' ~ 'Indígena',
                                 value == '3' ~ 'No indígena',
                                 T ~ 'No especificado'),
         porcentaje = n/sum(n)*100,
         ubicacion = 'Benito Juárez, Q.Roo') %>% 
  collect()

ascendencia_cun <- bind_rows(ind_cun, afro_cun) %>% 
  select(-value)

ascen <- bind_rows(ascendencia_cun, ascendencia) %>% 
  arrange(desc(porcentaje))

dbWriteTable(implan,
             Id (schema = 'coati_tablas_finales',
                 table = 'i_afroind_cun_nacional'),
             value = ascen)


## Personas que hablan lengua indígena en Benito Juárez ----
pob_lengua <-censo_2020 %>% 
  filter(ent =='23' & mun == '005') %>% 
  select(edad, sexo, hlengua, factor) %>% 
  filter(!is.na(hlengua)) %>% 
  mutate(sexo = case_when(sexo == '3' ~ 'Mujer',
                          sexo == '1' ~ 'Hombre'),
         habla = case_when(hlengua == '1' ~ 'Habla lengua indígena',
                           hlengua == '3' ~ 'No habla lengua indígena',
                           T ~ 'No especificado')) %>% 
  group_by(sexo, habla) %>% 
  count(wt = factor) %>% 
  group_by(sexo) %>% 
  mutate(porcentajes = n/ sum(n)*100)

## Personas en Benito Juárez que hablan lengua indígena por edades y sexo 
lengua_edades <- censo_2020 %>% 
  filter(ent =='23' & mun == '005') %>% 
  select(edad, sexo, hlengua, factor) %>% 
  filter(!is.na(hlengua)) %>% 
  mutate(sexo = case_when(sexo == '3' ~ 'Mujer',
                          sexo == '1' ~ 'Hombre'),
         habla = case_when(hlengua == '1' ~ 'Habla lengua indígena',
                           hlengua == '3' ~ 'No habla lengua indígena',
                           T ~ 'No especificado')) %>% 
  collect() %>% 
  mutate(grupo_edad = cut(edad,
                          seq(0, 
                              130, 
                              5))) %>% 
  group_by(sexo, habla, grupo_edad) %>% 
  count(wt=factor) 

## Lenguas habladas por la población de Benito Juárez:
inali <- read_csv('datos/clasificaciones/INALI.csv',
                  locale = locale(encoding = 'latin1'))

lengua <- censo_2020 %>% 
  filter(ent == '23' & mun == '005') %>%
  select(qdialect_inali, sexo, edad, factor) %>% 
  mutate(factor = as.numeric(factor),
         sexo = case_when(sexo == '3' ~ 'Mujer',
                          sexo == '1' ~ 'Hombre',
                          T ~ 'No especificado')) %>% 
  count(wt = factor, sexo, qdialect_inali) %>% 
  collect()

lenguas <- lengua %>% 
  left_join(inali,
            by = c ('qdialect_inali' = 'CLAVE')) %>% 
  filter(!is.na(DESCRIPCION)) 

lenguas <- lenguas %>% 
  mutate(DESCRIPCION = case_when(qdialect_inali == '0615' ~ "K’iche'",
                                 qdialect_inali == '0608' ~ "Q’anjob’al",
                                 T ~ DESCRIPCION)) %>% 
  view()

dbWriteTable(conn = implan,
             name = Id (schema = 'coati_tablas_finales',
                        table = 'i_lenguas'),
             value = lenguas)

# Vivienda y hábitat urbano ----

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



# Movilidad y Accesibilidad  ----- 
## Medios de tranporte ----

modo_traslado <- censo_2020 %>% 
  filter(ent == '23' & mun == '005') %>% 
  select(factor, med_traslado_trab1, med_traslado_trab2, med_traslado_trab3,
         med_traslado_esc1, med_traslado_esc2, med_traslado_esc3, sexo) %>% 
  mutate(across(contains('med_traslado'),
                \(x) case_when(x == '01' ~ 'Caminando',
                               x == '02' ~ 'Bicicleta',
                               x %in% c('03','04','05') ~ 'No aplica',
                               x == '06' ~ 'Camión, autobús, combi, colectivo',
                               x == '07' ~ 'Transporte de personal',
                               x == '08' ~ 'Taxi (sitio, calle, otro)',
                               x == '09' ~ 'Taxi (App Internet)',
                               x == '10' ~ 'Motocicleta o motoneta',
                               x == '11' ~ 'Automóvil o camioneta',
                               x == '12' ~ 'Otro',
                               x == '99' ~ 'No especificado',
                               is.na(x) ~ 'No especificado'))) %>% 
  mutate(sexo = case_when(sexo == '1' ~ 'Hombre',
                          sexo == '3' ~ 'Mujer',
                          T ~ 'No especificado')) %>%
  collect()

### Trabajo ---
dbGetQuery(implan,
           'select sum(factor) from coati.censo_2020
           where ent = \'23\' and mun = \'005\' and med_traslado_trab1 is not null') 

factor_expansion <- 394795 

traslado_trabajo <- modo_traslado %>% 
  select(contains('trab'), factor, sexo) 

traslado_trabajo <- traslado_trabajo  %>% 
  pivot_longer(! c(factor, sexo)) %>%  
  filter(!is.na(value)) %>%
  group_by(value, sexo) %>% 
  count(wt = factor) %>% 
  view()

traslado_trabajo <- traslado_trabajo %>% 
  filter(!(value == 'No especificado')) %>% 
  view()

modo_traslado %>% 
  filter(med_traslado_trab1 != 'No especificado') %>% pull(factor) %>% sum() #393736

traslado_trabajo <- traslado_trabajo %>% 
  mutate(porcentaje = n/393736*100,
         destino = 'Trabajo') %>%
  collect()

### Escuela ---
dbGetQuery(implan,
           'select sum(factor) from coati.censo_2020
           where ent = \'23\' and mun = \'005\' and med_traslado_esc1 is not null') 

factor_expansion <- 210534

traslado_escuela <- modo_traslado %>% 
  select(contains('esc'), factor, sexo) 

modo_traslado %>% 
  filter(!is.na(med_traslado_esc1)) %>% pull(factor) %>% sum() #909366
group_by(med_traslado_esc1) %>% 
  count(wt = factor) %>% 
  ungroup() %>% 
  mutate(p = n / sum(n) * 100)

modo_traslado %>% 
  filter(med_traslado_esc1 != 'No especificado') %>% pull(factor) %>% sum() #209697

traslado_escuela <- traslado_escuela %>%  
  pivot_longer(!factor) %>% 
  filter(!is.na(value)) %>%
  group_by(value) %>% 
  count(wt = factor) 

traslado_escuela <- traslado_escuela %>% 
  mutate(porcentaje = n/209697*100) %>% 
  filter(value != 'No especificado') %>% 
  collect()

traslado_escuela <- traslado_escuela %>% 
  mutate(destino = 'Escuela' )

traslado_escuela <- collect(traslado_escuela)

traslados <- bind_rows(traslado_escuela,
                       traslado_trabajo)

dbWriteTable(conn = implan,
             name = Id(schema = 'coati_tablas_finales',
                       table = 'ka_medio_traslado'),
             value = traslados)
medio_traslado <- tbl(src = implan,
                      Id (schema = 'coati_tablas_finales',
                          table = 'ka_medio_traslado'))
view(medio_traslado)

## Tiempo de traslado  -----
tiempos_traslado <- censo_2020 %>% 
  filter(ent == '23' & mun == '005') %>% 
  select(factor, tie_traslado_escu, tie_traslado_trab, sexo) %>% 
  mutate(across(contains('tie'),
                \(x) case_when(x == '1' ~ 'Hasta 15 minutos',
                               x == '2' ~ '16 a 30 minutos',
                               x == '3' ~ '31 minutos a 1 hora',
                               x == '4' ~ 'Más de 1 hora y hasta 2 horas',
                               x == '5' ~ 'Más de 2 horas',
                               x == '6' ~ 'No se traslada',
                               is.na(x) ~ 'No especificado')))
### Trabajo --- 

dbGetQuery(implan,
           'select sum(factor) from coati.censo_2020
           where ent = \'23\' and mun = \'005\' and tie_traslado_trab is not null') 

factor_expansion <- 435706 

tiempo_trabajo <- tiempos_traslado %>% 
  select(contains('trab'), factor) 

tiempo_trabajo <- tiempo_trabajo %>%  
  pivot_longer(!factor) %>%  
  filter(!is.na(value)) %>%
  group_by(value) %>% 
  count(wt = factor) 

tiempos_traslado %>% 
  filter(tie_traslado_trab != 'No especificado') %>% pull(factor) %>% sum() # = 394795

tiempo_trabajo <- tiempo_trabajo %>% 
  mutate(porcentaje = as.integer(n/394795*100),
         destino = 'Trabajo') %>%
  filter(value != 'No especificado') %>% 
  collect()

### Escuela ---
dbGetQuery(implan,
           'select sum(factor) from coati.censo_2020
           where ent = \'23\' and mun = \'005\' and tie_traslado_escu is not null') 

factor_expansion <- 214057 

tiempo_escuela <- tiempos_traslado %>% 
  select(contains('esc'), factor) 

tiempos_traslado %>% 
  filter(!is.na(tie_traslado_escu)) %>% pull(factor) %>% sum() # 908201

tiempos_traslado %>% 
  filter(tie_traslado_escu != 'No especificado') %>% pull(factor) %>% sum() # 212892

tiempo_escuela <- tiempo_escuela %>%  
  pivot_longer(!factor) %>% 
  filter(!is.na(value)) %>%
  group_by(value) %>% 
  count(wt = factor) 

tiempo_escuela <- tiempo_escuela %>% 
  mutate(porcentaje = as.integer(n/212892*100)) %>% 
  filter(value != 'No especificado') %>% 
  collect()

tiempo_escuela <- tiempo_escuela %>% 
  mutate(destino = 'Escuela' ) 

tiempos_traslado <- bind_rows(tiempo_trabajo,
                              tiempo_escuela)


dbWriteTable(conn = implan,
             name = Id(schema = 'coati_tablas_finales',
                       table = 'kn_tiempo_traslado'),
             value = tiempos_traslado,
             overwrite = TRUE )


## Medio de transporte por vivienda  ----- 
supermanzanas %>%
  select(id_supermanzana, geom, vph_ndacmm, vph_autom, vph_moto, vph_bici, tvivparhab) %>% 
  
  mutate(porcentajes)