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

ggplot()+
  ()

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
  

# ANÁLISIS GTFS - INDICADORES DE SERVICIO DE TRANSPORTE PÚBLICO
# Cancún (Benito Juárez), Quintana Roo — IMOVEQROO
# Paquete principal: tidytransit

# El operador IMOVEQROO tiene 31 rutas y ~936 paradas en Cancún.
# Si ya tienes el .zip del feed real, sustituye la sección "0C" con tu ruta.
# El script genera datos simulados con la geografía real de Cancún para
# que puedas correrlo de inmediato y validar la estructura de salida.




# 0A. LIBRERÍAS ------


paquetes <- c("tidytransit", "tidyverse", "sf", "tmap",
              "hms", "scales", "gt", "glue", "lubridate")

faltantes <- paquetes[!paquetes %in% installed.packages()[, "Package"]]
if (length(faltantes)) install.packages(faltantes)

library(tidytransit)
library(tidyverse)
library(sf)
library(tmap)
library(hms)
library(scales)
library(gt)
library(glue)
library(lubridate)

tmap_mode("view")

# CRS proyectado para Q. Roo (ITRF2008 UTM 16N)
CRS_MX <- 6370



# 0B. PARÁMETROS DE ANÁLISIS


# Ventanas horarias de análisis
HORA_PUNTA_AM  <- list(inicio = "06:00:00", fin = "09:00:00")
HORA_VALLE     <- list(inicio = "10:00:00", fin = "13:00:00")
HORA_PUNTA_PM  <- list(inicio = "16:00:00", fin = "19:00:00")
HORA_NOCTURNA  <- list(inicio = "20:00:00", fin = "23:00:00")

# Umbral de headway aceptable (minutos)
HEADWAY_ACEPTABLE <- 20   # ≤ 20 min = servicio frecuente
HEADWAY_CRITICO   <- 45   # > 45 min = servicio deficiente

# Radio de cobertura peatonal desde parada (metros)
RADIO_CAMINATA <- 400



# 0C. CARGA DEL FEED GTFS

# OPCIÓN 1 – Feed real de IMOVEQROO (cuando lo tengas):
#   gtfs <- read_gtfs("ruta/a/imoveqroo_cancun.zip")
#
# OPCIÓN 2 – Buscar en el catálogo de MobilityData:
#   feeds <- read_csv("https://bit.ly/catalogo-gtfs-mx")   # MobilityData CSV
#   # Filtra por municipio Benito Juárez / Quintana Roo
#
# OPCIÓN 3 – Feed de demostración incluido en tidytransit (NYC subway):
#   Útil para validar el flujo antes de tener el feed de Cancún.
#   gtfs_demo <- read_gtfs(system.file("extdata","nyc_subway.zip",
#                          package = "tidytransit"))

# ---- Para este script: generamos un feed sintético con geografía de Cancún ----
message("ℹ️  Generando feed GTFS sintético con la estructura de IMOVEQROO...")
gtfs <- crear_feed_cancun_sintetico()   # definida en la sección 0D



# 0D. GENERADOR DE FEED SINTÉTICO (ejecutar una vez; reemplazar con datos reales)


crear_feed_cancun_sintetico <- function() {
  
  set.seed(42)
  
  # ── Rutas principales de Cancún (basadas en rutas reales de IMOVEQROO) ──
  rutas_info <- tribble(
    ~route_id, ~route_short_name, ~route_long_name,                         ~route_color,
    "R1",  "R-1",  "Zona Hotelera – Centro",                               "E31837",
    "R2",  "R-2",  "Av. Tulum – CANCUN 500",                               "0078D4",
    "R13", "R-13", "Leona Vicario – Mercado 28",                           "28A745",
    "R15", "R-15", "Supermanzana 64 – Plaza Las Américas",                  "FFC107",
    "R23", "R-23", "Puerto Juárez – Av. López Portillo",                   "6F42C1",
    "R27", "R-27", "Col. Fonatur – Centro",                                 "FD7E14",
    "R48", "R-48", "Circuito Zona Hotelera (Blvd. Kukulcán)",              "20C997",
    "R55", "R-55", "Sm 67 – Av. Kabah",                                    "DC3545"
  )
  
  # ── Corredor geográfico simplificado por ruta (pares lat/lon aprox.) ──
  corredores <- list(
    R1  = list(lats = seq(21.082, 21.163, length.out = 18),
               lons = seq(-86.770, -86.848, length.out = 18)),
    R2  = list(lats = seq(21.155, 21.232, length.out = 14),
               lons = seq(-86.842, -86.861, length.out = 14)),
    R13 = list(lats = seq(21.165, 21.140, length.out = 12),
               lons = seq(-86.875, -86.840, length.out = 12)),
    R15 = list(lats = seq(21.145, 21.170, length.out = 10),
               lons = seq(-86.862, -86.900, length.out = 10)),
    R23 = list(lats = seq(21.180, 21.250, length.out = 16),
               lons = seq(-86.825, -86.815, length.out = 16)),
    R27 = list(lats = seq(21.130, 21.175, length.out = 11),
               lons = seq(-86.895, -86.855, length.out = 11)),
    R48 = list(lats = c(seq(21.082, 21.140, length.out = 10),
                        seq(21.140, 21.082, length.out = 10)),
               lons = c(seq(-86.770, -86.800, length.out = 10),
                        seq(-86.800, -86.770, length.out = 10))),
    R55 = list(lats = seq(21.158, 21.185, length.out = 9),
               lons = seq(-86.870, -86.910, length.out = 9))
  )
  
  # ── Generar paradas ──
  stops_list <- imap(corredores, function(cor, ruta_id) {
    n <- length(cor$lats)
    tibble(
      stop_id   = paste0(ruta_id, "_S", seq_len(n)),
      stop_name = paste0("Parada ", ruta_id, "-", seq_len(n)),
      stop_lat  = cor$lats + rnorm(n, 0, 0.0008),
      stop_lon  = cor$lons + rnorm(n, 0, 0.0008)
    )
  }) %>% bind_rows() %>% distinct(stop_id, .keep_all = TRUE)
  
  # ── Agency ──
  agency <- tibble(
    agency_id       = "IMOVEQROO",
    agency_name     = "IMOVEQROO - Transporte Público Cancún",
    agency_url      = "https://imoveqroo.gob.mx",
    agency_timezone = "America/Cancun"
  )
  
  # ── Routes ──
  routes <- rutas_info %>%
    mutate(agency_id = "IMOVEQROO", route_type = 3L)
  
  # ── Calendar (lunes-viernes y fin de semana) ──
  calendar <- tibble(
    service_id = c("weekday", "weekend"),
    monday = c(1L, 0L), tuesday = c(1L, 0L), wednesday = c(1L, 0L),
    thursday = c(1L, 0L), friday = c(1L, 0L),
    saturday = c(0L, 1L), sunday = c(0L, 1L),
    start_date = as.Date("2024-01-01"),
    end_date   = as.Date("2024-12-31")
  )
  
  # ── Trips y stop_times ──
  # Frecuencias reales aproximadas por ruta (headway en minutos, hora punta AM)
  headways_punta <- c(R1=10, R2=15, R13=20, R15=25, R27=30, R23=20, R48=12, R55=35)
  headways_valle <- c(R1=20, R2=25, R13=35, R15=40, R27=45, R23=30, R48=20, R55=50)
  
  generar_trips_y_tiempos <- function(route_id, stops_ruta, service_id,
                                      hw_punta, hw_valle) {
    # Horario de servicio: 5:00 a 23:00
    # Punta AM:  6-9   (hw_punta)
    # Valle:    9-16   (hw_valle)
    # Punta PM: 16-19  (hw_punta)
    # Nocturno: 19-23  (hw_valle * 1.5)
    
    salidas <- c(
      seq(5*3600,  6*3600 - 1, by = hw_valle  * 60),  # madrugada
      seq(6*3600,  9*3600 - 1, by = hw_punta  * 60),  # punta AM
      seq(9*3600,  16*3600- 1, by = hw_valle  * 60),  # valle
      seq(16*3600, 19*3600- 1, by = hw_punta  * 60),  # punta PM
      seq(19*3600, 23*3600,    by = hw_valle * 1.5*60) # nocturno
    ) %>% unique() %>% sort()
    
    n_stops <- nrow(stops_ruta)
    seg_entre_paradas <- 90  # ~90 segundos entre paradas
    
    purrr::map_dfr(seq_along(salidas), function(i) {
      trip_id <- glue("{route_id}_{service_id}_T{i}")
      stops_ruta %>%
        mutate(
          trip_id        = trip_id,
          route_id       = route_id,
          service_id     = service_id,
          stop_sequence  = row_number(),
          arrival_time   = as_hms(salidas[i] + (row_number()-1) * seg_entre_paradas),
          departure_time = arrival_time
        ) %>%
        select(trip_id, route_id, service_id, stop_id, stop_sequence,
               arrival_time, departure_time)
    })
  }
  
  trips_tiempos <- imap(corredores, function(cor, ruta_id) {
    stops_ruta <- stops_list %>% filter(str_starts(stop_id, ruta_id))
    hw_p <- headways_punta[[ruta_id]]
    hw_v <- headways_valle[[ruta_id]]
    
    bind_rows(
      generar_trips_y_tiempos(ruta_id, stops_ruta, "weekday",  hw_p, hw_v),
      generar_trips_y_tiempos(ruta_id, stops_ruta, "weekend",  hw_p * 1.5, hw_v * 1.5)
    )
  }) %>% bind_rows()
  
  trips <- trips_tiempos %>%
    distinct(trip_id, route_id, service_id) %>%
    mutate(shape_id = route_id, direction_id = 0L)
  
  stop_times <- trips_tiempos %>%
    select(trip_id, arrival_time, departure_time, stop_id, stop_sequence)
  
  # ── Ensamblar feed ──
  gtfs_obj <- list(
    agency     = agency,
    routes     = routes,
    stops      = stops_list,
    calendar   = calendar,
    trips      = trips,
    stop_times = stop_times
  )
  class(gtfs_obj) <- c("tidygtfs", "gtfs", "list")
  
  message(glue("✅ Feed sintético creado: {nrow(routes)} rutas | ",
               "{nrow(stops_list)} paradas | {n_distinct(trips$trip_id)} viajes"))
  return(gtfs_obj)
}



# 1. EXPLORACIÓN INICIAL DEL FEED 


cat("\n── Resumen del feed ──────────────────────────────────\n")
cat(glue("  Agencia:  {gtfs$agency$agency_name}\n"))
cat(glue("  Rutas:    {nrow(gtfs$routes)}\n"))
cat(glue("  Paradas:  {nrow(gtfs$stops)}\n"))
cat(glue("  Viajes:   {nrow(gtfs$trips)}\n"))
cat(glue("  Registros stop_times: {nrow(gtfs$stop_times)}\n"))
cat("──────────────────────────────────────────────────────\n\n")

# Tabla de rutas
gtfs$routes %>%
  select(route_id, route_short_name, route_long_name, route_type) %>%
  gt() %>%
  tab_header(title = "Rutas IMOVEQROO – Cancún") %>%
  cols_label(route_id = "ID", route_short_name = "Clave",
             route_long_name = "Nombre", route_type = "Tipo")



# 2. SELECCIÓN DE SERVICIOS (DÍAS LABORABLES)  --------


# Identificar service_id con más viajes (proxy de día típico)
service_ids_laborable <- gtfs$calendar %>%
  filter(monday == 1) %>%
  pull(service_id)

cat(glue("Service IDs laborables: {paste(service_ids_laborable, collapse=', ')}\n"))



# 3. FRECUENCIAS Y HEADWAYS POR PERÍODO -----

calcular_frecuencias_periodo <- function(gtfs, inicio, fin,
                                         svc_ids = service_ids_laborable,
                                         etiqueta = "") {
  get_stop_frequency(
    gtfs,
    start_time  = inicio,
    end_time    = fin,
    service_ids = svc_ids,
    by_route    = TRUE
  ) %>%
    mutate(
      periodo            = etiqueta,
      headway_min        = round(mean_headway / 60, 1),
      departures_n       = n_departures,
      cat_headway        = case_when(
        headway_min <= HEADWAY_ACEPTABLE ~ "Frecuente (≤20 min)",
        headway_min <= HEADWAY_CRITICO   ~ "Moderado (21-45 min)",
        TRUE                             ~ "Deficiente (>45 min)"
      ) %>% factor(levels = c("Frecuente (≤20 min)",
                              "Moderado (21-45 min)",
                              "Deficiente (>45 min)"))
    )
}

freq_punta_am <- calcular_frecuencias_periodo(
  gtfs, HORA_PUNTA_AM$inicio, HORA_PUNTA_AM$fin, etiqueta = "Punta AM (6-9h)")

freq_valle <- calcular_frecuencias_periodo(
  gtfs, HORA_VALLE$inicio, HORA_VALLE$fin, etiqueta = "Valle (10-13h)")

freq_punta_pm <- calcular_frecuencias_periodo(
  gtfs, HORA_PUNTA_PM$inicio, HORA_PUNTA_PM$fin, etiqueta = "Punta PM (16-19h)")

freq_nocturna <- calcular_frecuencias_periodo(
  gtfs, HORA_NOCTURNA$inicio, HORA_NOCTURNA$fin, etiqueta = "Nocturno (20-23h)")

# Consolidar todos los períodos
freq_todos_periodos <- bind_rows(
  freq_punta_am, freq_valle, freq_punta_pm, freq_nocturna
)

cat("\n── Headway promedio por ruta y período (minutos) ────\n")
freq_todos_periodos %>%
  group_by(route_id, periodo) %>%
  summarise(hw_prom = round(mean(headway_min, na.rm = TRUE), 1), .groups = "drop") %>%
  pivot_wider(names_from = periodo, values_from = hw_prom) %>%
  left_join(gtfs$routes %>% select(route_id, route_short_name), by = "route_id") %>%
  relocate(route_short_name, .after = route_id) %>%
  print()



# 4. FRECUENCIAS A NIVEL DE RUTA -------


freq_rutas_am <- get_route_frequency(
  gtfs,
  start_time  = HORA_PUNTA_AM$inicio,
  end_time    = HORA_PUNTA_AM$fin,
  service_ids = service_ids_laborable
) %>%
  mutate(
    headway_min = round(median_headways / 60, 1),
    cat_servicio = case_when(
      headway_min <= 10 ~ "Alta frecuencia (≤10 min)",
      headway_min <= 20 ~ "Frecuente (11-20 min)",
      headway_min <= 45 ~ "Moderado (21-45 min)",
      TRUE              ~ "Baja frecuencia (>45 min)"
    ) %>% factor(levels = c("Alta frecuencia (≤10 min)", "Frecuente (11-20 min)",
                            "Moderado (21-45 min)", "Baja frecuencia (>45 min)"))
  ) %>%
  left_join(gtfs$routes %>% select(route_id, route_short_name, route_long_name,
                                   route_color),
            by = "route_id")

cat("\n── Clasificación de rutas – Punta AM ────────────────\n")
freq_rutas_am %>%
  select(route_short_name, route_long_name, headway_min,
         total_departures, cat_servicio) %>%
  arrange(headway_min) %>%
  print(n = Inf)



# 5. INDICADORES ESPACIALES – COBERTURA Y DENSIDAD ------


# Convertir paradas a sf
stops_sf <- gtfs$stops %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>%
  st_transform(CRS_MX)

# Unir frecuencia punta AM a las paradas
stops_freq_am <- freq_punta_am %>%
  group_by(stop_id) %>%
  summarise(
    headway_min_prom = mean(headway_min, na.rm = TRUE),
    n_rutas          = n_distinct(route_id),
    departures_total = sum(departures_n, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  right_join(stops_sf, by = "stop_id") %>%
  st_as_sf() %>%
  mutate(
    cat_headway = case_when(
      headway_min_prom <= HEADWAY_ACEPTABLE ~ "Frecuente",
      headway_min_prom <= HEADWAY_CRITICO   ~ "Moderado",
      !is.na(headway_min_prom)              ~ "Deficiente",
      TRUE                                  ~ "Sin datos"
    ) %>% factor(levels = c("Frecuente", "Moderado", "Deficiente", "Sin datos"))
  )

# ── Geometría de rutas ──
# Para el feed sintético construimos líneas conectando paradas de cada ruta
rutas_sf <- gtfs$trips %>%
  distinct(route_id, trip_id) %>%
  group_by(route_id) %>%
  slice(1) %>%                      # un trip representativo por ruta
  inner_join(gtfs$stop_times, by = "trip_id") %>%
  arrange(stop_sequence) %>%
  inner_join(gtfs$stops, by = "stop_id") %>%
  group_by(route_id) %>%
  summarise(
    geometry = st_linestring(
      matrix(c(stop_lon, stop_lat), ncol = 2)
    ) %>% st_sfc(crs = 4326) %>% .[[1]],
    .groups = "drop"
  ) %>%
  st_as_sf(crs = 4326) %>%
  st_transform(CRS_MX) %>%
  left_join(freq_rutas_am %>% select(route_id, headway_min, cat_servicio,
                                     route_short_name, total_departures),
            by = "route_id")

# ── Área de cobertura (buffer 400 m desde cada parada) ──
cobertura_tp <- stops_sf %>%
  st_buffer(RADIO_CAMINATA) %>%
  st_union() %>%
  st_as_sf() %>%
  mutate(tipo = "Cobertura TP 400m")



# 6. TABLA RESUMEN DE INDICADORES POR RUTA


tabla_indicadores_rutas <- freq_rutas_am %>%
  select(route_short_name, route_long_name,
         headway_min, total_departures, cat_servicio) %>%
  arrange(headway_min) %>%
  gt() %>%
  tab_header(
    title    = "Indicadores de Servicio por Ruta – Punta AM (6–9h)",
    subtitle = "IMOVEQROO, Cancún | Día laborable típico"
  ) %>%
  cols_label(
    route_short_name = "Ruta",
    route_long_name  = "Corredor",
    headway_min      = "Headway (min)",
    total_departures = "Salidas totales",
    cat_servicio     = "Nivel de servicio"
  ) %>%
  data_color(
    columns = headway_min,
    palette = c("#2dc653", "#ffd60a", "#d62828")
  ) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_footnote(
    footnote = "Headway = tiempo promedio de espera en parada (mediana entre viajes).",
    locations = cells_column_labels(headway_min)
  )

print(tabla_indicadores_rutas)



# 7. ANÁLISIS DE BRECHA HORARIA (COBERTURA A LO LARGO DEL DÍA) ----


# ¿Cuántas rutas dan servicio en cada período?
resumen_cobertura_horaria <- freq_todos_periodos %>%
  group_by(periodo, route_id) %>%
  summarise(
    hw_med  = median(headway_min, na.rm = TRUE),
    salidas = sum(departures_n, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(periodo) %>%
  summarise(
    rutas_activas       = n_distinct(route_id),
    hw_promedio_sistema = round(mean(hw_med, na.rm = TRUE), 1),
    salidas_totales     = sum(salidas),
    pct_rutas_frecuente = mean(hw_med <= HEADWAY_ACEPTABLE, na.rm = TRUE) * 100,
    .groups = "drop"
  )

cat("\n── Cobertura horaria del sistema ────────────────────\n")
print(resumen_cobertura_horaria)

# Gráfico de headways por ruta y período
grafico_headway_periodo <- freq_todos_periodos %>%
  group_by(route_id, periodo) %>%
  summarise(hw = mean(headway_min, na.rm = TRUE), .groups = "drop") %>%
  left_join(gtfs$routes %>% select(route_id, route_short_name), by = "route_id") %>%
  ggplot(aes(x = periodo, y = hw, fill = cat_headway)) +
  geom_col(
    data = . %>%
      mutate(cat_headway = case_when(
        hw <= HEADWAY_ACEPTABLE ~ "Frecuente",
        hw <= HEADWAY_CRITICO   ~ "Moderado",
        TRUE                    ~ "Deficiente"
      )),
    aes(fill = cat_headway)
  ) +
  facet_wrap(~route_short_name, ncol = 4) +
  scale_fill_manual(
    values = c("Frecuente" = "#2dc653", "Moderado" = "#ffd60a", "Deficiente" = "#d62828"),
    name   = "Nivel de servicio"
  ) +
  labs(
    title    = "Headway (tiempo de espera) por ruta y período horario",
    subtitle = "IMOVEQROO – Cancún | Día laborable",
    x        = NULL, y = "Headway promedio (minutos)",
    caption  = "Línea punteada = umbral de 20 min (servicio frecuente)"
  ) +
  geom_hline(yintercept = HEADWAY_ACEPTABLE, linetype = "dashed", color = "grey40") +
  theme_minimal(base_size = 11) +
  theme(
    strip.text   = element_text(face = "bold"),
    axis.text.x  = element_text(angle = 30, hjust = 1),
    legend.position = "bottom"
  )

print(grafico_headway_periodo)



# 8. MAPAS INTERACTIVOS ----


# ── 8A. Mapa de rutas por nivel de servicio ──
mapa_rutas <- tm_shape(rutas_sf) +
  tm_lines(
    col          = "headway_min",
    palette      = "-RdYlGn",
    lwd          = 3,
    title.col    = "Headway punta AM (min)",
    popup.vars   = c("Ruta"         = "route_short_name",
                     "Headway (min)"= "headway_min",
                     "Salidas"      = "total_departures",
                     "Nivel"        = "cat_servicio")
  ) +
  tm_shape(stops_sf) +
  tm_dots(col = "grey40", size = 0.04, alpha = 0.5) +
  tm_layout(
    title  = "Rutas IMOVEQROO – Headway Punta AM",
    legend.outside = TRUE
  ) +
  tm_scale_bar() + tm_compass()

# ── 8B. Mapa de paradas coloreadas por headway ──
mapa_paradas_headway <- tm_shape(stops_freq_am) +
  tm_dots(
    col         = "headway_min_prom",
    palette     = "-RdYlGn",
    size        = 0.08,
    title       = "Headway parada (min)",
    popup.vars  = c("Parada"        = "stop_name",
                    "Headway (min)" = "headway_min_prom",
                    "N° rutas"      = "n_rutas",
                    "Salidas"       = "departures_total",
                    "Nivel"         = "cat_headway")
  ) +
  tm_layout(title = "Headway por parada – Punta AM") +
  tm_scale_bar()

# ── 8C. Mapa de cobertura geográfica ──
mapa_cobertura <- tm_shape(cobertura_tp) +
  tm_polygons(col = "#0077b6", alpha = 0.25,
              border.col = "#0077b6",
              title = glue("Cobertura TP ({RADIO_CAMINATA}m)")) +
  tm_shape(stops_sf) +
  tm_dots(col = "white", border.col = "#0077b6", size = 0.06) +
  tm_layout(
    title  = glue("Cobertura del Transporte Público a {RADIO_CAMINATA}m – Cancún"),
    legend.outside = TRUE
  )

# Mostrar mapas
mapa_rutas
mapa_paradas_headway
mapa_cobertura



# 9. INDICADORES FINALES CONSOLIDADOS ----

indicadores_sistema <- list(
  
  # Cobertura de red
  n_rutas                = nrow(gtfs$routes),
  n_paradas              = nrow(gtfs$stops),
  km_red_total           = rutas_sf %>%
    st_length() %>% sum() %>% as.numeric() %>% `/`(1000) %>% round(1),
  
  # Servicio (punta AM)
  hw_mediana_punta_am    = median(freq_rutas_am$headway_min, na.rm = TRUE),
  hw_promedio_punta_am   = mean(freq_rutas_am$headway_min, na.rm = TRUE) %>% round(1),
  pct_rutas_frecuentes   = mean(freq_rutas_am$headway_min <= HEADWAY_ACEPTABLE,
                                na.rm = TRUE) %>% `*`(100) %>% round(1),
  salidas_punta_am       = sum(freq_rutas_am$total_departures, na.rm = TRUE),
  
  # Cobertura espacial
  area_cobertura_km2     = cobertura_tp %>%
    st_transform(CRS_MX) %>% st_area() %>% sum() %>%
    as.numeric() %>% `/`(1e6) %>% round(2)
)

cat("\n╔══════════════════════════════════════════════════╗\n")
cat("║   INDICADORES DEL SISTEMA – IMOVEQROO CANCÚN    ║\n")
cat("╠══════════════════════════════════════════════════╣\n")
cat(glue("  Rutas en servicio:          {indicadores_sistema$n_rutas}\n"))
cat(glue("  Paradas:                    {indicadores_sistema$n_paradas}\n"))
cat(glue("  Longitud total de red:      {indicadores_sistema$km_red_total} km\n"))
cat(glue("  Headway mediana (punta AM): {indicadores_sistema$hw_mediana_punta_am} min\n"))
cat(glue("  Headway promedio (punta AM):{indicadores_sistema$hw_promedio_punta_am} min\n"))
cat(glue("  % rutas frecuentes (≤20min):{indicadores_sistema$pct_rutas_frecuentes}%\n"))
cat(glue("  Salidas punta AM (6-9h):    {indicadores_sistema$salidas_punta_am}\n"))
cat(glue("  Área de cobertura 400m:     {indicadores_sistema$area_cobertura_km2} km²\n"))
cat("╚══════════════════════════════════════════════════╝\n")



# 10. EXPORTACIÓN -----


# Tabla de frecuencias
freq_todos_periodos %>%
  write_csv("gtfs_cancun_frecuencias_parada.csv")

# Indicadores por ruta
freq_rutas_am %>%
  select(route_id, route_short_name, route_long_name,
         headway_min, total_departures, cat_servicio) %>%
  write_csv("gtfs_cancun_indicadores_ruta.csv")

# Capas espaciales
st_write(stops_freq_am, "gtfs_cancun_paradas.gpkg",   delete_dsn = TRUE)
st_write(rutas_sf,      "gtfs_cancun_rutas.gpkg",     delete_dsn = TRUE)
st_write(cobertura_tp,  "gtfs_cancun_cobertura.gpkg", delete_dsn = TRUE)

cat("\n✅ Archivos exportados:\n")
cat("   • gtfs_cancun_frecuencias_parada.csv\n")
cat("   • gtfs_cancun_indicadores_ruta.csv\n")
cat("   • gtfs_cancun_paradas.gpkg\n")
cat("   • gtfs_cancun_rutas.gpkg\n")
cat("   • gtfs_cancun_cobertura.gpkg\n")


# PRÓXIMOS PASOS CUANDO TENGAS EL FEED REAL -----

# 1. Reemplaza `gtfs <- crear_feed_cancun_sintetico()` por:
#       gtfs <- read_gtfs("imoveqroo_cancun.zip")
#
# 2. Valida el feed:
#       attr(gtfs, "validation_result")
#
# 3. Revisa qué service_ids corresponden a días laborables:
#       gtfs$calendar
#
# 4. Si el feed tiene tabla `frequencies` (frequencies.txt), úsala:
#       get_stop_frequency() la prioriza automáticamente
#
# 5. Para integrar con tus datos de tiempos de traslado del Censo:
#       left_join(stops_freq_am, tu_df_agebs, by = join_by nearest AGEB)
#       → usar st_join() con st_nearest_feature() o st_within()
#
# 6. GTFS Realtime (si IMOVEQROO lo publica): usa gtfsrouter para
#    posiciones en vivo y cálculo de tiempos reales de espera.
