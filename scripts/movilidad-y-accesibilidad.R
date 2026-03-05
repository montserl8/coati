
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