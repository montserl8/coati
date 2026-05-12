library(tidyverse)
library(sf)
theme_set(theme_minimal())
update_theme(text = element_text(family = 'Comic Sans'))
library(extrafont)

mz <- read_sf(implan, 
              Id(schema = 'base',
                 table = 'manzanas'))

irregulares <- read_sf(siginplan,
                       Id(schema = '02_diagnostico',
                          table = 'asentamientos_irregulares'))

densidad_pob <- dbGetQuery(implan, 'with cte as ( select ano, sum(pobtot) as pobtot, sum(st_area(st_transform(a.geometry, 32616)))/10000 as area_ha
              from pmdu_espacial.censos_2010_2020 b
              inner join pmdu_espacial.manzanas_cvegeo a
              on a.cvegeo = b.cvegeo
              group by ano) 

select * , pobtot/area_ha as densidad_pob from cte') 

theme_update(axis.text = element_text(size = 14),
             axis.title = element_text(size = 16,
                                       face = 'bold'))
ggplot(densidad_pob) +
  geom_bar(aes(ano, 
               densidad_pob),
           stat = 'identity',
           fill = '#0097A9')+
  scale_x_continuous(labels = c (2010, 2020),
                     breaks = c (2010, 2020)) +
  scale_y_continuous(limits = c(0, 80),
                     breaks = seq (0, 80, 5))+
  labs(x = 'Año',
       y = 'Densidad poblacional (personas / hectárea)') +
  theme(text = element_text(family = 'Lato')) 

ggsave(filename = 'plots/a1-densidad-poblacional.png')


# % de viviendas con piso de tierra -----
censos <- tbl(implan,
              Id(schema = 'pmdu_espacial',
                 table = 'censos_2010_2020'))

colnames(censos)

tierra <- censos %>% 
  select(ano,
         cvegeo,
         vph_pisoti) %>%  
  collect()

mza <- read_sf(implan,
               Id(schema = 'pmdu_espacial',
                  tale = 'manzanas_cvegeo'))

mza_tierra <- inner_join(tierra,
                         mza) %>%  
  st_as_sf()

irregulares <- read_sf(siginplan,
                       Id(schema = '02_diagnostico',
                          table = 'asentamientos_irregulares_22')) %>%  
  st_transform(4326)


mza_tierra$irregular <- ifelse(lengths(st_intersects(mza_tierra, irregulares)) > 0,
                               'Irregular', 
                               'Normal')

mza_tierra %>%  
  st_drop_geometry() %>%  
  group_by(ano,
           irregular) %>%  
  summarise(viviendas = sum(vph_pisoti,
                            na.rm = T)) |> 
  group_by(ano) |> 
  mutate(porcentaje_viviendas = viviendas / sum(viviendas) * 100)

mza_tierra |> 
  select(-irregular) |> 
  pivot_wider(id_cols = c(cvegeo,
                          geometry),
              values_from = vph_pisoti,
              names_from = ano) |> 
  mutate(across(c(`2010`, 
                  `2020`),
                ~ replace_na(.x, 0)),
         cambio = `2020` - `2010`,
         cambio = ifelse(between(cambio,
                                 -5,
                                 5),
                         NA,
                         cambio)) %>% 
  ggplot() +
  geom_sf(aes(fill = cambio)) +
  scale_fill_gradient2(
  )

# % de viviendas con hacinamiento ----

hac <- censos %>% 
  filter(entidad == '23', mun == '005') %>% 
  select(ano,
         cvegeo,
         pro_ocup_c,
         tvivparhab) %>% 
  collect()

hac %>%
  mutate(hacinamiento = ifelse(pro_ocup_c > 2.5,
                               'Hacinados', 
                               'Normal')) %>% 
  st_drop_geometry() %>% 
  select(-cvegeo) %>% 
  group_by(ano) %>% 
  summarise(viviendas_hacinadas = sum(tvivparhab[hacinamiento == 'Hacinados'], na.rm = T),
            viviendas_totales = sum(tvivparhab, na.rm = T),
            porcentaje_hacinadas = viviendas_hacinadas/viviendas_totales * 100)


ggplot(viv %>% filter(ent == '23', mun == '005', pro_ocup_c < 10), # Filtro de outliers para ver mejor
       aes(x = pro_ocup_c)) +
  geom_density(fill = "#69b3a2", alpha = 0.4) +
  geom_vline(xintercept = 2.5, linetype = "dashed", color = "red") +
  annotate("text", x = 3.5, y = 0.1, label = "Umbral CONEVAL (2.5)", color = "red") +
  labs(
    title = "Distribución del Promedio de Ocupantes por Cuarto",
    x = "Ocupantes por cuarto",
    y = "Densidad"
  ) +
  theme_minimal()

# % de viviendas deshabitadas por ZF ----

zf <- read_sf(implan,
              Id(schema = 'pmdu_espacial',
                 table = 'zonas_funcionales_v7'))
colnames(zf
         )
viv <- censos %>% 
  select(cvegeo,
         vivpar_des,
         vivtot) %>% 
  collect()

viv %>% 
  inner_join(mza) %>% 
  st_as_sf() %>% 
  st_join(zf %>% 
            select(zona) %>% 
            st_make_valid()) %>% 
  group_by(zona) %>% 
  summarise(porcentaje_viv_deshabitadas = sum(vivpar_des, na.rm = T)/sum(vivtot, na.rm = T)*100,
            viviendas_totales = sum(vivtot, na.rm = T),
            viviendas_deshabitadas = sum(vivpar_des, na.rm = T))
  
  