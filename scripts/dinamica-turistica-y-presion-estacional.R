
library(tidyverse)
censo_2020 <- tbl(src = implan,
                  Id (schema = 'coati',
                      table = 'censo_2020'))

df_parque_habitacional <- read_csv('../procesamiento-coati/datos/parque_habitacional/parque_habitacional.csv')
censo_2020 %>%
  filter(ent == '23', mun == '005') %>%
  select(clavivp, factor) %>%
  filter(!is.na(clavivp)) %>%
  mutate(
    tipo_vivienda = case_when(
      clavivp == 1 ~ 'Habitada',
      clavivp == 2 ~ 'No habitada',
      clavivp == 3 ~ 'Uso temporal',
      clavivp == 4 ~ 'Refugio',
      clavivp == 5 ~ 'Local no construido',
      TRUE          ~ 'No especificado'
    )
  ) %>%
  group_by(tipo_vivienda) %>%
  count(wt = factor) %>%
  ungroup() %>%
  mutate(porcentaje = round((n / sum(n) * 100), 1)) %>%
  rename(total_viviendas = n) %>%
  collect()


# Cálculo de la dependencia económica turística 
# aquí tengo que hacer un join con la clave de ocupación
ocupacion <- read_csv('../procesamiento-coati/datos/cuestionarios_ampliados/2020/clasificaciones/OCUPACION.csv') 

censo_2020 %>%
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


