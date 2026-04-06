# =============================================================================
# SECCIÓN: DINÁMICA TURÍSTICA Y PRESIÓN ESTACIONAL
# Municipio de referencia: Benito Juárez, Quintana Roo (ent = '23', mun = '005')
# Fuentes: censo_2020 (INEGI), datatur (SECTUR/DATATUR mensual)
# =============================================================================
# Indicadores:
#   1. IVUT  — Índice de Viviendas de Uso Temporal
#   2. TDET  — Tasa de Dependencia Económica Turística
#   3. IE    — Índice de Estacionalidad Mensual
# =============================================================================

library(tidyverse)
censo_2020 <- tbl(src = implan,
                  Id (schema = 'coati',
                      table = 'censo_2020'))
censo_2020 %>% head(3) %>% collect() %>% glimpse()
# -----------------------------------------------------------------------------
# INDICADOR 1: IVUT — Índice de Viviendas de Uso Temporal
# -----------------------------------------------------------------------------
# Concepto:
#   Proporción de viviendas catalogadas como "de uso temporal" sobre el total
#   de viviendas particulares. Funciona como proxy de la presión turística
#   sobre el stock habitacional del municipio.
#
# Variable clave:
#   clase_v: clase de vivienda particular
#     1 = Habitada
#     2 = No habitada (desocupada)
#     3 = De uso temporal  <-- viviendas vacacionales / turísticas
#     4 = Refugio
#     5 = Local no construido para habitación
#
# Resultado: % de viviendas de uso temporal sobre el total
# -----------------------------------------------------------------------------

ivut <- censo_2020 %>%
  filter(ent == '23', mun == '005') %>%
  select(clase_v, factor) %>%
  filter(!is.na(clase_v)) %>%
  mutate(
    tipo_vivienda = case_when(
      clase_v == 1 ~ 'Habitada',
      clase_v == 2 ~ 'No habitada',
      clase_v == 3 ~ 'Uso temporal',
      clase_v == 4 ~ 'Refugio',
      clase_v == 5 ~ 'Local no construido',
      TRUE          ~ 'No especificado'
    )
  ) %>%
  group_by(tipo_vivienda) %>%
  count(wt = factor) %>%
  ungroup() %>%
  mutate(porcentaje = n / sum(n) * 100) %>%
  rename(total_viviendas = n) %>%
  collect()

# Valor síntesis del indicador (solo viviendas de uso temporal)
ivut_valor <- ivut %>%
  filter(tipo_vivienda == 'Uso temporal') %>%
  pull(porcentaje)

cat("IVUT:", round(ivut_valor, 2), "% de viviendas son de uso temporal\n")


# -----------------------------------------------------------------------------
# INDICADOR 2: TDET — Tasa de Dependencia Económica Turística
# -----------------------------------------------------------------------------
# Concepto:
#   Proporción de la población ocupada que trabaja en sectores directamente
#   vinculados al turismo (SCIAN 71–72): alojamiento, preparación de alimentos
#   y bebidas, y servicios de entretenimiento. Mide cuánto depende la economía
#   local del sector turístico.
#
# Variable clave:
#   rama_act: rama de actividad económica (2 dígitos SCIAN)
#     71 = Servicios de esparcimiento, culturales y deportivos
#     72 = Servicios de alojamiento temporal y de preparación de alimentos
#
# Nota: rama_act puede venir como carácter o como entero según la versión
#       del censo; ajustar el filtro según corresponda.
#
# Resultado: % de ocupados en turismo sobre total de ocupados
# -----------------------------------------------------------------------------

tdet <- censo_2020 %>%
  filter(ent == '23', mun == '005') %>%
  select(rama_act, sit_tra, factor) %>%
  filter(!is.na(rama_act), sit_tra %in% c(1, 2, 3, 4)) %>%  # solo ocupados
  mutate(
    sector = case_when(
      rama_act %in% c('71', '72') ~ 'Turístico',
      TRUE                         ~ 'Otros sectores'
    )
  ) %>%
  group_by(sector) %>%
  count(wt = factor) %>%
  ungroup() %>%
  mutate(porcentaje = n / sum(n) * 100) %>%
  rename(total_ocupados = n) %>%
  collect()

# Valor síntesis del indicador
tdet_valor <- tdet %>%
  filter(sector == 'Turístico') %>%
  pull(porcentaje)

cat("TDET:", round(tdet_valor, 2), "% de los ocupados trabajan en sectores turísticos\n")


# -----------------------------------------------------------------------------
# INDICADOR 3: IE — Índice de Estacionalidad Mensual
# -----------------------------------------------------------------------------
# Concepto:
#   Mide la distribución relativa de la afluencia turística mes a mes,
#   comparando cada mes contra el promedio mensual del año. Valores > 100
#   indican temporada alta; valores < 100 indican temporada baja.
#   Complementariamente, se calcula el Coeficiente de Variación (CV) como
#   medida sintética de la presión estacional global.
#
# Fuente: datatur — tabla de afluencia mensual de SECTUR/DATATUR
# Variables esperadas en datatur:
#   anio     : año del registro
#   mes      : número de mes (1–12)
#   nom_mes  : nombre del mes
#   afluencia: número de turistas o cuartos-noche ocupados en el mes
#
# Resultado: índice mensual (100 = promedio) + CV anual de la estacionalidad
# -----------------------------------------------------------------------------

ie <- datatur %>%
  filter(cve_mun == '23005', anio == 2023) %>%        # ajustar año según disponibilidad
  select(mes, nom_mes, afluencia) %>%
  filter(!is.na(afluencia)) %>%
  mutate(
    promedio_anual = mean(afluencia, na.rm = TRUE),
    indice_estacionalidad = (afluencia / promedio_anual) * 100,
    temporada = case_when(
      indice_estacionalidad >= 120 ~ 'Alta',
      indice_estacionalidad >= 80  ~ 'Media',
      TRUE                          ~ 'Baja'
    )
  ) %>%
  arrange(mes) %>%
  collect()

# Coeficiente de Variación como medida síntesis de presión estacional
cv_estacionalidad <- ie %>%
  summarise(
    cv = (sd(afluencia, na.rm = TRUE) / mean(afluencia, na.rm = TRUE)) * 100
  ) %>%
  pull(cv)

cat("IE — Coeficiente de Variación estacional:", round(cv_estacionalidad, 2), "%\n")
cat("  (CV > 50%: alta estacionalidad | CV 25-50%: moderada | CV < 25%: baja)\n")

# Vista del índice por mes
print(ie %>% select(nom_mes, afluencia, indice_estacionalidad, temporada))


# -----------------------------------------------------------------------------
# TABLA RESUMEN DE LOS 3 INDICADORES
# -----------------------------------------------------------------------------

resumen_indicadores <- tibble(
  indicador   = c('IVUT', 'TDET', 'IE (CV)'),
  descripcion = c(
    'Índice de Viviendas de Uso Temporal (%)',
    'Tasa de Dependencia Económica Turística (%)',
    'Coeficiente de Variación estacional (%)'
  ),
  valor       = c(
    round(ivut_valor, 2),
    round(tdet_valor, 2),
    round(cv_estacionalidad, 2)
  ),
  fuente      = c('Censo 2020 — INEGI', 'Censo 2020 — INEGI', 'DATATUR — SECTUR')
)

print(resumen_indicadores)
