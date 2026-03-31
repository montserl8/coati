
df_vic1 <- read.csv('../procesamiento-coati/datos/envipe/TPer_Vic1.csv')

df_vic2 <- read.csv('../procesamiento-coati/datos/envipe/TPer_Vic2.csv')

names(df_vic2)

df_tvivienda %>% 
  filter(AREAM == '41') %>% 
  view()

# Percepción sobre seguridad pública ----
  
## Seguridad en términos de delincuencia ------

df_vic1 %>%
  filter(AREAM == '41') %>%
  select(contains('AP4_4_'), FAC_ELE_AM, SEXO) %>%
  mutate(across(contains('AP4_4_'),
                \(x) case_when(x == 1 ~ 'Se siente seguro(a)', 
                               x == 2 ~ 'Se siente inseguro(a)',
                               x == 3 ~ 'No aplica',
                               x == 9 ~ 'No sabe / No responde'))) %>% 
  mutate(sexo = ifelse(SEXO == 1, 'Hombre', 'Mujer')) %>% 
  pivot_longer(contains('AP4_4_')) %>% 
  mutate(percepcion_seguridad = recode(name,
                                       'AP4_4_01' = 'En su casa',
                                       'AP4_4_02' = 'En su trabajo',
                                       'AP4_4_03' = 'En la calle',
                                       'AP4_4_04' = 'En la escuela',
                                       'AP4_4_05' = 'En el mercado',
                                       'AP4_4_06' = 'En el centro comercial',
                                       'AP4_4_07' = 'En el banco',
                                       'AP4_4_08' = 'En el cajero automático localizado en la vía pública',
                                       'AP4_4_09' = 'En el transporte público',
                                       'AP4_4_10' = 'En el automóvil',
                                       'AP4_4_11' = 'En la carretera',
                                       'AP4_4_12' = 'En el parque o centro recreativo',
                                       'AP4_4_A'  = 'Caminando solo(a) por la noche en los alrededores de su vivienda')) %>% 
  group_by(percepcion_seguridad, value, sexo) %>% 
  count(wt = FAC_ELE_AM) %>% 
  view()

## Conducta antisocial ------
percepcion_antisocial <- df_vic1 %>% 
  filter(AREAM == '41') %>% 
  select(contains('AP4_5_'), 
         FAC_ELE_AM) %>% 
  mutate(across(contains('AP4_5_'),
                \(x)ifelse(x == 1, 'Sí','No'))) %>% 
  pivot_longer(!FAC_ELE_AM) %>% 
  group_by(name, value) %>% 
  count(wt = FAC_ELE_AM) %>% 
  mutate(actividad_antisocial = case_when(name == 'AP4_5_01' ~ 'Se consume alcohol en la calle',
                                          name == 'AP4_5_02' ~ 'Existe pandillerismo o bandas violentas',
                                          name == 'AP4_5_03' ~ 'Hay riñas entre vecinos',
                                          name == 'AP4_5_04' ~ 'Existe venta ilegal de alcohol',
                                          name == 'AP4_5_05' ~ 'Se venden productos piratas',
                                          name == 'AP4_5_06' ~ 'Ha habido violencia policiaca contra ciudadanos(as)',
                                          name == 'AP4_5_07' ~ 'Hay invasión de predios',
                                          name == 'AP4_5_08' ~ 'Se consume droga',
                                          name == 'AP4_5_09' ~ 'Existen robos o asaltos frecuentes',
                                          name == 'AP4_5_10' ~ 'Se vende droga',
                                          name == 'AP4_5_11' ~ 'Ha habido disparos frecuentes',
                                          name == 'AP4_5_12' ~ 'Hay prostitución',
                                          name == 'AP4_5_13' ~ 'Ha habido secuestros',
                                          name == 'AP4_5_14' ~ 'Ha habido homicidios',
                                          name == 'AP4_5_15' ~ 'Ha habido extorsiones (o cobro de piso)',
                                          name == 'AP4_5_16' ~ 'Robo o venta ilegal de gasolina o diésel (huachicol)',
                                          name == 'AP4_5_17' ~ 'Tomas irregulares de luz (energía eléctrica) o diablitos',
                                          name == 'AP4_5_18' ~ 'Ninguna',
                                          name == 'AP4_5_99' ~ 'No sabe / no responde')) %>% 
  ungroup() %>% 
  group_by(actividad_antisocial) %>% 
  mutate(porcentajes = round(n/sum(n)*100,1)) 

percepcion_antisocial <- percepcion_antisocial %>% 
  ungroup()

percepcion_antisocial %>% 
  mutate(actividad_antisocial = fct_reorder(actividad_antisocial, 
                                            n,
                                            .fun = \(x)sum(x[percepcion_antisocial$value == 'Sí'
                                            ]
                                            ))) %>% 
  ggplot()+
  geom_bar(aes(actividad_antisocial, 
               n,
               fill = value),
           stat = 'identity')
# Victimización ------

df_delitos_2015_2025 <- read_csv('../procesamiento-coati/datos/sesnsp/Municipal-Delitos-2015-2025_feb2026/Municipal-Delitos-2015-2025_feb2026.csv',
                                 locale = locale(encoding = "latin1"))

df_victimas_2026 <- read_csv('../procesamiento-coati/datos/sesnsp/victimas_municipal_2026.csv',
                             locale = locale(encoding="latin1"))

df_delitos_2015_2025 %>% 
  filter(Clave_Ent == 23,
         `Cve. Municipio` == 23005) %>% 
  select(Año, `Tipo de delito`) %>% 
  view()
