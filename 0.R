library(reader)
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(kableExtra)
library(knitr)
library(lubridate)
library(highcharter)
library(janitor)


#preparado base de datos----
plantas <- read_excel("planta_total.xlsx",.name_repair = "minimal")

#esto lo hice porque tengo columnas con nombres idem 
plantas <- plantas %>% 
  clean_names()


regiones <- read_excel("Efectores y regiones.xlsx")
regiones <- regiones %>% 
  clean_names()

#join para las regiones-----
plantas_regiones <- plantas %>% 
  left_join(
    regiones %>% select(unidad, region),
    by = "unidad")


#control
table(plantas_regiones$region)

#Prof de interés--------------------------------------------------------------------------------
#filtrado----
prof_interes <- plantas_regiones %>% 
  filter(funcion %in% c("MEDICO PEDIATRA","MEDICO TOCOGINECOLOGO","LICENCIADO EN OBSTETRICIA"))

###########PROFESIONES POR ZONA #####################

#tabla
tabla_profesiones_regiones <- prof_interes %>% 
  group_by(funcion, region) %>%  
  summarise(
    Total = n_distinct(legajo),
    .groups = "drop")


orden_regiones <- tabla_profesiones_regiones %>% 
  group_by(region) %>% 
  summarise(total_region = sum(Total)) %>% 
  arrange(desc(total_region))

#grafico
grafico_profesiones_regiones <- tabla_profesiones_regiones %>% 
  mutate(
    region = factor(region, levels = orden_regiones$region)
  ) %>% 
  ggplot(aes(x = region, y = Total, fill = funcion)) +
  geom_col() +
  theme_classic()

grafico_profesiones_regiones



