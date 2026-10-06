# Librerías -----
library(tidyverse)
library(data.table)
library(dtplyr)
library(janitor)
library(readr)


# Conexiones -------
p_censo_2020 <- tbl(implan,
                    Id(schema = 'coati',
                       table = 'censo_2020')) %>% 
  filter(ent == '23' & mun == '005') %>% 
  collect

salud20 <- read_delim(clipboard(),
                      col_names = T)

salud25 <- read_delim(clipboard(),
                      col_names = T)

intercensal <- read_csv(file = 'datos/intercensal/personas00.csv',
                        locale = locale(encoding = "latin1")) 

intercensal <- intercensal %>% 
  rename_with(tolower) %>% 
  filter(cve_ent == '23' & cve_mun == '005') 
 
dh20 <- p_censo_2020 %>%  
  select(dhsersal1,
         dhsersal2, 
         factor) %>% 
  pivot_longer(!factor) %>% 
  filter(!is.na(value)) %>% 
  left_join(salud20 %>% mutate(`valor ` = as.numeric(`valor `)), 
            by = c(value = 'valor ')) %>% 
  group_by(afiliacion) %>% 
  count(wt = factor) %>%
  ungroup() %>% 
  mutate(p = n/sum(n, T)* 100) 

p_intercensal <- fread('datos/intercensal/personas00.csv')

p_intercensal_cun <- p_intercensal %>% 
  filter(CVE_ENT == '23' & CVE_MUN == '5') %>% 
  as_tibble() %>% 
  janitor::clean_names()



dh25$anio <- 2025
dh20$anio <- 2020

dh25 <- p_intercensal_cun %>% 
  select(dhsersal1,
         dhsersal2, 
         factor) %>% 
  pivot_longer(!factor) %>% 
  filter(!is.na(value)) %>% 
  left_join(salud25, by =c(value = 'valor')) %>% 
  group_by(afiliacion) %>% 
  count(wt = factor) %>%
  ungroup() %>% 
  mutate(p = n/sum(n, T)* 100) 

dh <- bind_rows(dh25,
                dh20)

dh %>% 
  filter(!is.na(afiliacion)) %>% 
  mutate(afiliacion = fct_reorder(afiliacion,
                                  p,
                                  .desc = F)) %>% 
  ggplot(aes(afiliacion,
             p,
             fill = factor(anio))) +
  geom_col(position = 'dodge') +
  # geom_text(aes(value,
  #               max(porcentaje) + 10,
  #               label = round(porcentaje, 1),
  #               group = factor(ano)),
  #           position = position_dodge(width = 1))+
  scale_x_discrete(labels = \(x) str_wrap(x,
                                          20)) +
  scale_y_continuous(breaks = seq(0, 70, 5))+
  scale_fill_manual(values = c('#a4afc4', '#243b64'))+
  theme_minimal() +
  theme(legend.position = 'inside',
        legend.position.inside = c(0.8, 0.2),
        axis.text = element_text(size = 11))+
  labs(x = 'Institución',
       y = 'Porcentaje',
       title = 'Derechohabiencia a salud',
       fill = NULL) +
  coord_flip()

# Traslado por sexo -----
p_censo_2020 %>% 
  group_by()

modo_traslado

medio <- read_delim(clipboard(),
                    col_names = T)

medio$valor <- as.integer(medio$valor)

p_intercensal_cun %>% 
  select(factor, 
         med_traslado_trab1,
         sexo) %>% 
  mutate(sexo = case_when(sexo == '1' ~ 'Hombre',
                          sexo == '3' ~ 'Mujer',
                          T ~ 'No especificado')) %>%  
  filter(!is.na(med_traslado_trab1)) %>% 
  group_by(med_traslado_trab1, sexo) %>% 
  count(wt = factor) %>% 
  left_join(medio %>% mutate(valor = as.numeric(valor)), 
            by = c(med_traslado_trab1 = 'valor')) %>% 
  ungroup() %>% 
  group_by(medio) %>% 
  filter(!is.na(medio)) %>% 
  select(-med_traslado_trab1) %>% 
  mutate(p = n/sum(n)*100,
         anio = 2025)

# Asistencia escolar -----
asistencia25 <- p_intercensal_cun %>% 
  select(asisten,
         factor,
         edad) %>% 
  filter(!is.na(asisten)) %>% 
  mutate(asiste = case_when(asisten == 1 ~ 'Asiste a la escuela',
                            asisten == 3 ~'No asiste a la escuela',
                            T ~ 'No especificado'),
         grado = case_when(edad %in% c(1, 2, 3, 4, 5) ~ 'Kínder',
                           edad >= 6 & edad <= 12  ~ 'Primaria',
                           edad >= 13 & edad <= 15 ~ 'Secundaria',
                           edad >= 16 & edad <= 18 ~ 'Preparatoria',
                           edad >= 18 & edad <= 24 ~ 'Educación superior',
                           T ~ 'No especificado')) %>% 
  group_by(asiste,
           grado) %>% 
  count(wt = factor) %>% 
  filter(asiste == 'Asiste a la escuela') %>% 
  ungroup() %>% 
  mutate(p = n/sum(n)*100)

asistencia20 <- p_censo_2020 %>% 
  filter(mun == '005' & ent == '23') %>% 
  select(asisten,
         factor,
         edad) %>% 
  filter(!is.na(asisten)) %>% 
  mutate(asiste = case_when(asisten == 1 ~ 'Asiste a la escuela',
                            asisten == 3 ~'No asiste a la escuela',
                            T ~ 'No especificado'),
         grado = case_when(edad %in% c(1, 2, 3, 4, 5) ~ 'Kínder',
                           edad >= 6 & edad <= 12  ~ 'Primaria',
                           edad >= 13 & edad <= 15 ~ 'Secundaria',
                           edad >= 16 & edad <= 18 ~ 'Preparatoria',
                           edad >= 18 & edad <= 24 ~ 'Educación superior',
                           T ~ 'No especificado')) %>% 
  group_by(asiste, 
           grado) %>% 
  count(wt = factor) %>% 
  filter(asiste == 'Asiste a la escuela') %>% 
  ungroup() %>% 
  mutate(p = n/sum(n)*100) 

asistencia20$anio <- 2020
asistencia25$anio <- 2025

asistencias <- bind_rows(asistencia20,
                         asistencia25)


asistencias %>% 
  filter(grado != 'No especificado') %>% 
  mutate(grado = factor(grado, levels = c('Kínder', 
                                          'Primaria', 
                                          'Secundaria', 
                                          'Preparatoria', 
                                          'Educación superior'))) %>% 
  ggplot(aes(grado,
             p,
             fill = factor(anio))) +
  geom_col(position = 'dodge') +
  scale_x_discrete(labels = \(x) str_wrap(x,
                                          10)) +
  scale_y_continuous(breaks = seq(0, 70, 10))+
  scale_fill_manual(values = c('#dab6ba', '#8e1b3f'))+
  theme_minimal() +
  theme(legend.position = 'inside',
        legend.text = element_text(size = 20),
        legend.position.inside = c(0.9, 0.9),
        axis.text = element_text(size = 20))+
  labs(x = 'Grado',
       y = 'Porcentaje',
       title = 'Asistencia escolar',
       fill = NULL) 

# Razón viejites / jóvenes -----
p_censo_2020 %>%
  filter(!is.na(edad), edad != 999) %>%
  filter(ent == '23' & mun == '005') %>% 
  summarise(adultos_mayores = sum(factor[edad >= 60], na.rm = T),
            jovenes = sum(factor[edad >= 15 & edad <= 24], na.rm = T),
            relacion = adultos_mayores/jovenes)

# Estructura demográfica --------

piramide_poblacional <- intercensal %>% 
  count(wt = factor) %>% 
  summarise(tampob = sum(n, na.rm = T))

piramide_poblacional <- intercensal %>%  
  mutate(sexo = case_when( sexo == '1' ~ 'Hombres',
                           sexo == '3' ~ 'Mujeres',
                           T ~ 'No especificado')) %>%
  collect() %>% 
  mutate(grupo_edad = cut(edad,
                          breaks = c(0, 4, 9, 14, 19, 24, 29, 34, 39, 44,
                                     49, 54, 59, 64, 69, 74, 79, 84, 150),
                          labels = c("0 a 4", "5 a 9", "10 a 14", "15 a 19", "20 a 24",
                                     "25 a 29", "30 a 34", "35 a 39", "40 a 44", "45 a 49",
                                     "50 a 54", "55 a 59", "60 a 64", "65 a 69", "70 a 74",
                                     "75 a 79", "80 a 84", ">85"),
                          right = T,
                          include.lowest = T)) %>%
  filter(!is.na(grupo_edad), sexo %in% c("Hombres", "Mujeres")) %>%
  group_by(grupo_edad, sexo) %>% 
  summarise(poblacion = sum(factor, na.rm = T)) 

piramide_poblacional %>%
  mutate(poblacion_plot = if_else (sexo == "Hombres", -poblacion, poblacion)) %>%
  ggplot(aes(x = grupo_edad, y = poblacion_plot, fill = sexo)) +
  geom_col(width = 0.9) +
  coord_flip() +
  scale_y_continuous(labels = function(x) scales::comma(abs(x))) +
  scale_fill_manual(
    values = c('Hombres' = '#0097A9', 'Mujeres' = '#B371AD')) +
  labs(x = 'Grupo de edad',
       y = 'Población',
       fill = 'Sexo') +
  theme_minimal() +
  theme(legend.position = c(0.85, 0.83),
        legend.background = element_rect(fill = 'white', color = NA))


dbWriteTable(conn = implan, 
             name = Id (schema = 'coati_tablas_finales',
                        table = 'a_piramide_poblacional'),
             value = ppl_df)

piramide25 <- intercensal %>% 
  mutate(rango = cut(edad,
                     seq(0, max(edad), 5),
                     right = T,
                     include.lowest = T),
         sexo = ifelse(sexo == 1, 'Hombre', 'Mujer')) %>% 
  group_by(sexo, rango) %>% 
  count(wt = factor) 

# Rangos de edad -----
intercensal %>% 
  mutate(grupo = case_when(between(edad, 0, 14)~'0-14',
                           between(edad, 15, 24)~'15-24',
                           between(edad, 25, 39)~'25-39',
                           between(edad, 25, 29)~'25-29',
                           between(edad, 20, 39)~'20-39',
                           between(edad, 40, 59)~'40-59',
                           T ~ '60 y más')) %>% 
  group_by(grupo) %>% 
  count(wt = factor) %>%
  ungroup() %>% 
  mutate(pcent = n / sum(n)*100)


write_csv(piramide25, 'datos/intercensal/piramide_poblacional.csv')
