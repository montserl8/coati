
df_vic1 <- read.csv('../procesamiento-coati/datos/envipe/TPer_Vic1.csv')
df_tvivienda <- read_csv('../procesamiento-coati/datos/envipe/TVivienda.csv')
df_vic2 <- read.csv('../procesamiento-coati/datos/envipe/TPer_Vic2.csv')

names(df_vic2)

df_tvivienda %>% 
  filter(AREAM == '41') %>% 
  view()

# Percepción de conducta antisocial ------
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
  
  
# Tipos de delitos ------
df_vic1 %>% 
  filter(AREAM == '41') %>% 
  select(contains('AP4_4_'), FAC_ELE_AM) %>% 
  mutate(across(contains('AP4_4_'),
                \(x) ifelse(x == 1, 'Sí', 'No'))) %>% 
  pivot_longer(!FAC_ELE_AM) %>% 
  mutate(name = recode(name,
                       AP4_4_01 = 'Robo de vehículo',
                       AP4_4_02 = 'Robo de accesorios de vehículo',
                       AP4_4_03 = 'Robo a casa habitación',
                       AP4_4_04 = 'Robo a transeúnte',
                       AP4_4_05 = 'Robo a negocio',
                       AP4_4_06 = 'Fraude',
                       AP4_4_07 = 'Extorsión',
                       AP4_4_08 = 'Amenazas',
                       AP4_4_09 = 'Lesiones',
                       AP4_4_10 = 'Secuestro',
                       AP4_4_11 = 'Delito sexual',
                       AP4_4_12 = 'Otro delito'
  )) %>% 
  group_by(name, value) %>% 
  count(wt = FAC_ELE_AM)