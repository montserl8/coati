library(tidyverse)
library(sf)
library(foreign)


# Dominio: Sociedad ----
## Tasa de mortalidad de menores de 5 años ----
defun <- read.dbf(file = "../scripts-coati/global_urban_monitoring/defunciones_base_datos_2024_dbf/DEFUN24.dbf",
         as.is = T) %>% 
  rename_with(tolower)

defun %>% 
  

# Dominio: Economía ----
# Dominio: Medio Ambiente ----
# Dominio: Cultura ----
# Dominio: Gobernanza e implementación ----