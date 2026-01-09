# =============================================
# TABLA DE PROFESIONES POR EFECTOR Y REGIONES
# ============================================

tabla_prof_efector_regiones <- prof_interes %>% 
  filter(region != "Subsecretaría de Salud") %>% 
  group_by(region, unidad, funcion) %>% 
  summarise(
    Total = n_distinct(legajo),
    .groups = "drop") %>% 
  mutate(unidad = str_to_title(tolower(unidad))) %>% 
  mutate(
    unidad = case_when(
      unidad == "Hospital Dr.castro Rendon" ~ "HPN",
      TRUE ~ unidad))

 

#tabla general

tabla_prof_efector_regiones_2 <- datatable(
  tabla_prof_efector_regiones,
  filter = "top",   #  ESTO pone los filtros arriba
  rownames = FALSE,
  options = list(
    pageLength = 10,
    autoWidth = TRUE),
  colnames = c(
    "Región sanitaria",
    "Efector",
    "Profesión",
    "Cantidad"))
tabla_prof_efector_regiones_2






#======================================================================
#======TABLAS POR REGION===============================================
#======================================================================


#========CONFLUENCIA========================
confluencia <- tabla_prof_efector_regiones %>% 
  filter(region%in% c("Región Confluencia", "HPN"))

confluencia <- confluencia %>% 
  mutate(
    region  = factor(region),
    unidad  = factor(unidad),
    funcion = factor(funcion))

 
tabla_confluencia <- datatable(
  confluencia,
  filter = "top",
  rownames = FALSE,
  extensions = "Buttons",
  options = list(
    dom = "Bfrtip",     
    buttons = list(
      list(
        extend = "excel",
        text = "Exportar a Excel",
        filename = "profesionales_confluencia",
        title = "Distribución de profesionales – Región Confluencia")),
    paging = FALSE,
    autoWidth = TRUE),
  colnames = c(
    "Región sanitaria",
    "Efector",
    "Profesión",
    "Cantidad"))

tabla_confluencia





#========COMARCA Y DEL LIMAY========================
 
comarca_limay <- tabla_prof_efector_regiones %>% 
  filter(region%in% c("Región de la Comarca", "Región del Limay"))

comarca_limay <- comarca_limay %>% 
  mutate(
    region  = factor(region),
    unidad  = factor(unidad),
    funcion = factor(funcion))

 

tabla_comarca_limay <- datatable(
  comarca_limay,
  filter = "top",
  rownames = FALSE,
  extensions = "Buttons",
  options = list(
    dom = "Bfrtip",     
    buttons = list(
      list(
        extend = "excel",
        text = "Exportar a Excel",
        filename = "profesionales_comarca_limay",
        title = "Distribución de profesionales – Región de la Comarca y Región del Limay")),
    paging = FALSE,
    autoWidth = TRUE),
  colnames = c(
    "Región sanitaria",
    "Efector",
    "Profesión",
    "Cantidad"))

 
tabla_comarca_limay

#========DEL PEHUEN========================
pehuen <- tabla_prof_efector_regiones %>% 
  filter(region=="Región del Pehuén")

pehuen <- pehuen %>% 
  mutate(
    region  = factor(region),
    unidad  = factor(unidad),
    funcion = factor(funcion))


tabla_pehuen <- datatable(
  pehuen,
  filter = "top",
  rownames = FALSE,
  extensions = "Buttons",
  options = list(
    dom = "Bfrtip",     
    buttons = list(
      list(
        extend = "excel",
        text = "Exportar a Excel",
        filename = "profesionales_pehuen",
        title = "Distribución de profesionales – Región del Pehuén")),
    paging = FALSE,
    autoWidth = TRUE),
  colnames = c(
    "Región sanitaria",
    "Efector",
    "Profesión",
    "Cantidad"))
tabla_pehuen


#========LAGOS DEL SUR========================
lagos <- tabla_prof_efector_regiones %>% 
  filter(region=="Región de los Lagos del Sur")

lagos <- lagos %>% 
  mutate(
    region  = factor(region),
    unidad  = factor(unidad),
    funcion = factor(funcion))


tabla_lagos <- datatable(
  lagos,
  filter = "top",
  rownames = FALSE,
  extensions = "Buttons",
  options = list(
    dom = "Bfrtip",     
    buttons = list(
      list(
        extend = "excel",
        text = "Exportar a Excel",
        filename = "profesionales_lagos",
        title = "Distribución de profesionales – Región de los Lagos del Sur")),
    paging = FALSE,
    autoWidth = TRUE),
  colnames = c(
    "Región sanitaria",
    "Efector",
    "Profesión",
    "Cantidad"))
tabla_lagos


#========VACA MUERTA========================
vaca_muerta <- tabla_prof_efector_regiones %>% 
  filter(region=="Región Vaca Muerta")


vaca_muerta <- vaca_muerta %>% 
  mutate(
    region  = factor(region),
    unidad  = factor(unidad),
    funcion = factor(funcion))

tabla_vaca_muerta <- datatable(
  vaca_muerta,
  filter = "top",
  rownames = FALSE,
  extensions = "Buttons",
  options = list(
    dom = "Bfrtip",     
    buttons = list(
      list(
        extend = "excel",
        text = "Exportar a Excel",
        filename = "profesionales_vacamuerta",
        title = "Distribución de profesionales – Región Vaca Muerta")),
    paging = FALSE,
    autoWidth = TRUE),
  colnames = c(
    "Región sanitaria",
    "Efector",
    "Profesión",
    "Cantidad"))
tabla_vaca_muerta

#========ALTO NQN========================
alto_nqn <- tabla_prof_efector_regiones %>% 
  filter(region=="Región Alto Neuquén")


alto_nqn  <- alto_nqn %>% 
  mutate(
    region  = factor(region),
    unidad  = factor(unidad),
    funcion = factor(funcion))


tabla_alto_nqn <- datatable(
  alto_nqn,
  filter = "top",
  rownames = FALSE,
  extensions = "Buttons",
  options = list(
    dom = "Bfrtip",     
    buttons = list(
      list(
        extend = "excel",
        text = "Exportar a Excel",
        filename = "profesionales_alto_nqn",
        title = "Distribución de profesionales – Región Alto Neuquén")),
    paging = FALSE,
    autoWidth = TRUE),
  colnames = c(
    "Región sanitaria",
    "Efector",
    "Profesión",
    "Cantidad"))
tabla_vaca_muerta
tabla_alto_nqn





