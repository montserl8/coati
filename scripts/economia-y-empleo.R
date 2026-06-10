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

# Población estudiando
censo_2020 %>% 
  filter(ent == '23' & mun=='005'& conact %in% c('50', '15'))  %>% 
  count(wt = factor) %>% 
  mutate(porcentaje = n/909366*100)

# Porcentaje de población económicamente activa
censo_2020 %>% 
  filter(ent == '23' & mun=='005')%>% 
  collect() %>% 
  filter(str_detect(conact, '^1')) %>% 
  count(wt = factor) %>% 
  mutate(porcentaje = n/909366*100)

# Población jubilada / porcentaje pensionada 
censo_2020 %>% 
  filter(ent == '23' & mun=='005'& conact=='40')  %>% 
  count(wt = factor) %>% 
  mutate(porcentaje = n/909366*100)

# Población estudiando, trabajando y jubilada

# Dependencia económica -----

dependencia_economica <- iter_nal %>%
  filter(entidad == '23',
         mun == '005',
         nom_loc == 'Total del Municipio') %>%
  summarise(dependientes = p_0a2 + p_3a5 + p_6a11 + p_12a14 + pob65_mas,
            independientes = pob15_64,
            dependencia = (dependientes) / independientes * 100,
            independencia = 100-dependencia)

dbWriteTable(implan,
             Id (schema = 'coati_tablas_finales',
                 table = 'g_dependencia_economica'),
             dependencia_economica,
             overwrite = T)

# PIB de la ciudad y por sector ----- 

# Gasto turístico ----

# Ocupación de la pea ----
ocupaciones <- read_csv('../procesamiento-coati/datos/cuestionarios_ampliados/2020/clasificaciones/OCUPACION.csv', locale = locale(encoding = 'latin1'))


ocupacion <- censo_2020 %>% 
  filter(ent == '23', mun == '005' ) %>% 
  select(ocupacion_c, factor) %>% 
  group_by(ocupacion_c) %>% 
  summarise(poblacion_total = sum(factor, na.rm = T)) %>% 
  collect() %>% 
  left_join(ocupaciones, by = c("ocupacion_c" = "CLAVE")) 

ocupacion <- ocupacion %>% 
  mutate(ocup = str_extract(string = ocupacion_c,
                            pattern = '\\d'),
         ocupaciones = case_when(ocup == '1' ~ 'Directores, funcionarios, gerentes, coordinadores y jefes de área',
                                 ocup == '2' ~ 'Profesionistas y técnicos',
                                 ocup == '3' ~ 'Trabajadores auxiliares en actividades administrativas',
                                 ocup == '4' ~ 'Comerciantes, empleados en ventas y agentes de ventas en establecimientos',
                                 ocup == '5' ~ 'Trabajadores en otros servicios',
                                 ocup == '6' ~ 'Trabajadores en actividades agrícolas, ganaderas, forestales, caza y pesca',
                                 ocup == '7' ~ 'Trabajadores artesanales',
                                 ocup == '8' ~ 'Operadores de maquinaria industrial, ensambladores, choferes y conductores de transporte',
                                 str_detect(ocupacion_c, pattern = '^95') ~ 'Vendedores ambulantes',
                                 ocupacion_c %in% c('961', '962', '960') ~ 'Trabajadores de limpieza / domésticos',
                                 ocup == '9' ~ 'Trabajadores en actividades elementales y de apoyo',
                                 T ~ DESCRIPCION )) %>% 
  group_by(ocupaciones) %>% 
  count(wt = poblacion_total) %>% 
  view()

# Posición en el trabajo por sexo y edad -----

censo_2020 %>% 
  select(sittra, factor, sexo) %>% 
  group_by(sittra, sexo) %>% 
  mutate(sexo = ifelse(sexo == '1', 'Hombre', 
                       'Mujer')) %>% 
  summarise(poblacion_total = sum(factor, na.rm = T)) %>% 
  mutate(posicion = case_when(sittra == '1' ~ 'Empleada(o) u obrero',
                              sittra == '2' ~ 'Jornalera(o) o peón(a)',
                              sittra == '3' ~ 'Ayudante con pago',
                              sittra == '4' ~ 'Patrón(a) o empleador(a)',
                              sittra == '5' ~ 'Trabajador(a) por cuenta propia',
                              sittra == '6' ~ 'Trabajador(a) sin pago',
                              T ~ 'No especificado')) %>% 
  group_by(posicion) %>% 
  collect()

# Ingresos promedio ----

ingreso_por_sexo <- censo_2020 %>%
  filter(ent == '23', mun == '005') %>%
  filter(!is.na(ingtrmen), ingtrmen > 0) %>%
  select(sexo, ingtrmen, 
         sittra,factor) %>%       
  collect() %>%                         
  mutate(sexo = ifelse(sexo == '1', 'Hombre', 'Mujer')) %>%
  group_by(sexo) %>% 
  summarise(ingreso_prom = weighted.mean(ingtrmen, w = factor)) %>% 
  view()

# Prestaciones laborales ----

censo_2020 %>% 
  filter(ent == '23' & mun =='005') %>% 
  select(aguinaldo,
         vacaciones,
         servicio_medico,
         utilidades,
         incap_sueldo,
         sar_afore,
         credito_vivienda,
         factor,
         conact) %>% 
  collect() %>%
  filter(str_detect(conact, '^1')) %>% 
  mutate(prestacion = ifelse(aguinaldo == '1' &
                               vacaciones == '3'&
                               servicio_medico == '5' &
                               utilidades == '7' & 
                               incap_sueldo == '1' &
                               sar_afore == '3' &
                               credito_vivienda == '5',
                             'Sí', 
                             'No')) %>% 
  filter(prestacion == 'Sí') %>% 
  count(wt = factor)

censo_2020 %>% 
  filter(ent == '23' & mun =='005') %>% 
  count(wt = factor)

# Porcentaje de personas a nivel nacional que tienen prestaciones
prestaciones_nal <- (12061844/125515554)*100
prestaciones_bj <- (153299/909366)*100

# Horas trabajadas ----
ent_mun <- iter_nal %>% 
  select(nom_ent, nom_mun, entidad, mun) %>% 
  mutate(entidad = str_pad(entidad,
                           width = 2,
                           side = 'left',
                           pad = '0'),
         mun = str_pad(mun,
                       width = 3,
                       side = 'left',
                       pad = '0'))


horas_trabajadas_mx <- censo_2020 %>%  
  select(hortra, factor, ent) %>% 
  collect() %>% 
  filter(hortra >= 0 & hortra <= 999) %>% 
  group_by(ent) %>% 
  summarise(horas = sum(hortra*factor,
                        na.rm = T) / sum(factor,
                                         na.rm = T)) %>% 
  left_join(ent_mun %>% 
              select(entidad, nom_ent) %>% 
              distinct(),
            by = c ('ent' = 'entidad')) 

horas_trabajadas_mx <- horas_trabajadas_mx %>% 
  arrange(desc(horas))

horas_trabajadas_qroo <- censo_2020 %>%  
  filter(ent == '23') %>% 
  select(hortra, factor, mun) %>% 
  collect() %>% 
  filter(hortra >= 0 & hortra <= 999) %>% 
  group_by(mun) %>% 
  summarise(horas = sum(hortra*factor,
                        na.rm = T) / sum(factor,
                                         na.rm = T)) %>% 
  left_join(ent_mun %>% 
              filter(entidad == '23') %>% 
              select(mun, nom_mun) %>% 
              distinct(),
            by = c('mun'='mun')) %>% 
  arrange(desc(horas)) %>% 
  view()

# Población económicamente activa ----
# Empleo formal e informal ----
# Ingresos promedio por (persona/ocupación/sexo/por edad) ------
ocupacion_ingreso_sexo_edad <- censo_2020 %>%
  filter(ent == '23', mun == '005') %>%
  collect() %>% 
  filter(!is.na(ingtrmen), ingtrmen > 0,
         str_detect(conact, '^1')) %>%
  select(sexo, ingtrmen, edad, ocupacion_c, factor) %>%    
  mutate(sexo = ifelse(sexo == '1', 'Hombre', 'Mujer')) %>%
  left_join(ocupaciones, by = c('ocupacion_c'='CLAVE')) %>% 
  mutate (ocup = str_extract(string = ocupacion_c,
                             pattern = '\\d'),
          ocupaciones = case_when(ocup == '1' ~ 'Directores, funcionarios, gerentes, coordinadores y jefes de área',
                                  ocup == '2' ~ 'Profesionistas y técnicos',
                                  ocup == '3' ~ 'Trabajadores auxiliares en actividades administrativas',
                                  ocup == '4' ~ 'Comerciantes, empleados en ventas y agentes de ventas en establecimientos',
                                  ocup == '5' ~ 'Trabajadores en otros servicios',
                                  ocup == '6' ~ 'Trabajadores en actividades agrícolas, ganaderas, forestales, caza y pesca',
                                  ocup == '7' ~ 'Trabajadores artesanales',
                                  ocup == '8' ~ 'Operadores de maquinaria industrial, ensambladores, choferes y conductores de transporte',
                                  str_detect(ocupacion_c, pattern = '^95') ~ 'Vendedores ambulantes',
                                  ocupacion_c %in% c('961', '962', '960') ~ 'Trabajadores de limpieza / domésticos',
                                  ocup == '9' ~ 'Trabajadores en actividades elementales y de apoyo',
                                  T ~ DESCRIPCION),
          grupo_edad = cut(edad, c(min(edad),24,54,max(edad)))) %>% 
  group_by(ocupaciones, sexo, grupo_edad) %>% 
  summarise(ingreso_prom = weighted.mean(ingtrmen, w = factor)) %>% 
  view()




