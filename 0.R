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
library(glue)
library(DT)

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
  filter(funcion %in% c("MEDICO PEDIATRA","MEDICO TOCOGINECOLOGO","LICENCIADO EN OBSTETRICIA")) %>% 
  filter(region != "Subsecretaría de Salud") %>% 
  filter(unidad != "ZONA SANITARIA III") 
#mutate nombres
prof_interes <- prof_interes %>% 
  mutate(funcion=case_when(
    funcion=="MEDICO PEDIATRA"~"Pediatría",
    funcion=="MEDICO TOCOGINECOLOGO"~"Tocoginecología",
    funcion=="LICENCIADO EN OBSTETRICIA"~"Obstetricia"))

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
#grafico_profesiones_regiones <- tabla_profesiones_regiones %>% 
#  mutate(
#    region = factor(region, levels = orden_regiones$region)) %>% 
#  ggplot(aes(x = region, y = Total, fill = funcion)) +
#  geom_col() +
#  theme_classic()

#grafico_profesiones_regiones


#PROFESIONES-----------------
#tabla con profesiones SIN region 

# ================================
# TABLA DE PROFESIONES (SIN REGIÓN)
# ================================

tabla_profesiones <- prof_interes %>% 
  group_by(funcion) %>%  
  summarise(
    Total = n_distinct(legajo),
    .groups = "drop"
  ) %>% 
  mutate(
    color = case_when(
      funcion == "MEDICO PEDIATRA" ~ "#8FBC8F",
      funcion == "MEDICO TOCOGINECOLOGO" ~ "#4682B4",
      funcion == "LICENCIADO EN OBSTETRICIA" ~ "#5F9EA0"))


# ================================
# PREPARAR DATOS PARA HIGHCHART
# ================================

#puntos_treemap <- tabla_profesiones %>%
#  mutate(
#    name = funcion,
#    value = Total) %>%
#  select(name, value, color) %>%
#  purrr::pmap(function(name, value, color) {
#    list(
#      name = name,
#      value = value,
#      color = color)
#    })


# ================================
# GRÁFICO TREEMAP
# ================================

#treemap_profesiones <- highchart() %>%
#  hc_chart(type = "treemap") %>%
#  hc_title(
#    text = "Cantidad total de profesionales por especialidad (Pediatría, Obstetricia y Tocoginecología). N=359",
#    align = "left",
#    style = list(
#     fontFamily = "Manrope",
#      fontSize = "20px",
#      fontWeight = "bold")) %>%
#  hc_add_series(
#    type = "treemap",
#    layoutAlgorithm = "squarified",
#    data = puntos_treemap,
#    dataLabels = list(
#      enabled = TRUE,
#      align = "center",
#      verticalAlign = "middle",
#      format = "{point.name}<br><b>{point.value}</b>",
#      style = list(
#        fontSize = "14px",
#        textOutline = "none")))

# MOSTRAR
#treemap_profesiones



## ================================
# TOTAL PROVINCIAL POR REGIONES
# ================================

#pivot la tabla 
tabla_prof_wide <- tabla_profesiones_regiones %>% 
  tidyr::pivot_wider(
    names_from = funcion,
    values_from = Total,
    values_fill = 0)

tabla_prof_wide <- tabla_prof_wide %>%
  mutate(
    Total = Pediatría + Obstetricia + Tocoginecología) %>%
  arrange(desc(Total)) #para ordenar el grafico de mayor a menor

total_prof_region <- sum(tabla_profesiones_regiones$Total)
 
titulo_dinamico1 <- glue("Distribución de profesionales por región sanitaria. N={total_prof_region}")
  
  


#grafico

grafico_profesiones_regiones <- highchart() %>%
  hc_chart(type = "bar") %>%
  hc_title(
    text = titulo_dinamico1,
    align = "left",
    style = list(
      fontFamily = "Manrope",
      fontSize = "20px",
      fontWeight = "bold")) %>%
  hc_xAxis(
    categories = tabla_prof_wide$region,
    title = list(text = NULL)) %>%
  hc_yAxis(
    title = list(text = "Cantidad de profesionales"),
    gridLineWidth = 0) %>%
  hc_plotOptions(
    series = list(
      stacking = "normal",
      dataLabels = list(enabled = FALSE))) %>%
  hc_add_series(
    name = "Pediatría",
    data = tabla_prof_wide$Pediatría,
    color = "#90dd93") %>%
  hc_add_series(
    name = "Obstetricia",
    data = tabla_prof_wide$Obstetricia,
    color = "#4682B4") %>%
  hc_add_series(
    name = "Tocoginecología",
    data = tabla_prof_wide$Tocoginecología,
    color = "#5F9EA0") %>%
  hc_tooltip(shared = TRUE)
grafico_profesiones_regiones
