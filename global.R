# CARGA DE PAQUETES

library(tidyverse)     
library(readxl)        
library(bslib)         
library(showtext)
library(shiny)
library(DT)
library(plotly)
library(highcharter)
library(bsicons)
library(jsonlite)
library(shinyjs)
library(shinyjs)


# CARGA DE DATOS
datos <- read_excel("base_indicadores_eficiencia.xlsx")


# NIVELES ESCOLARES ORDENADOS

# Orden que necesito para cursos de primaria y secundaria
# Esto se hace porque en algunas provincias 7º grado es primaria, mientras que en otras es secundaria
orden_primaria <- c("1º Grado","2º Grado","3º Grado","4º Grado","5º Grado","6º Grado","7º Grado")
orden_secundaria <- c("7º Grado","1º Año","2º Año","3º Año","4º Año","5º Año")

datos <- datos %>%
  mutate(
    curso = case_when(
      curso == "Total"      ~ "Total",
      nivel == "Primaria"   ~ factor(curso, levels = orden_primaria),
      nivel == "Secundaria" ~ factor(curso, levels = orden_secundaria),
      TRUE ~ curso
    ),
    
    # Por otro lado. por una cuestion estetica redondeo la variable "valor" a 2 decimales
    valor = round(valor, 2)
  )

# LISTAS AUXILIARES PARA LOS INPUTS

niveles_disponibles <- unique(datos$nivel)
años_disponibles <- sort(unique(datos$Año), decreasing = T)
provincias_disponibles <- unique(datos$Provincia)

# Lista oficial de provincias
provincias_oficiales <- c(
  "Buenos Aires", "Catamarca", "Chaco", "Chubut", "Ciudad de Buenos Aires",
  "Córdoba", "Corrientes", "Entre Ríos", "Formosa", "Jujuy", "La Pampa",
  "La Rioja", "Mendoza", "Misiones", "Neuquén", "Río Negro", "Salta",
  "San Juan", "San Luis", "Santa Cruz", "Santa Fe", "Santiago del Estero",
  "Tierra del Fuego", "Tucumán"
)


# DISEÑO VISUAL DEL DASHBOARD - COLORES DGE

tema_dashboard <- bs_theme(
  bootswatch = "flatly",
  base_font = font_google("Montserrat"),
  heading_font = font_google("Montserrat"),
  primary = "#338090",     
  bg = "#f5f5f5",           
  fg = "#18243f",           
  success = "#6FC1B5",      
  info = "#FCC309",        
  warning = "#c8a977",      
  danger = "#8A68A6"        
)

# TIPOGRAFÍA INSTITUCIONAL DGE

font_add_google("Montserrat", "montserrat")
showtext_auto()


# PALETAS DE COLORES INSTITUCIONALES

# Colores fijos para indicadores (Mendoza en comparaciones)
colores_dge <- list(
  "Abandono interanual" = "#8A68A6", # violeta
  "Repitencia"          = "#F1D477", # amarillo
  "Sobreedad"           = "#0594A4", # azul
  "Promocion Efectiva"  = "#A1C16D", # verde
  comparada             = "#444444"  # gris genérico para comparación
)

# Paletas graduales para mapas
paletas_mapas <- list(
  "Abandono interanual" = c("#f0e7f6", "#caa9de", "#8A68A6"),
  "Repitencia"          = c("#fff5cc", "#f7df88", "#F1D477"),
  "Sobreedad"           = c("#d9f3f7", "#7dcad4", "#0594A4"),
  "Promocion Efectiva"  = c("#edf5df", "#c8dea2", "#A1C16D")
)

library(jsonlite)

# --- Ruta robusta al GeoJSON (evita problemas con acentos/espacios en Windows) ---
# 1) Intenta ruta relativa
rel_path <- file.path("www", "ar-all.geo.json")

# 2) Si existe, conviértela a ruta absoluta y "corta" (8.3) para evitar problemas
if (file.exists(rel_path)) {
  abs_path <- normalizePath(rel_path, winslash = "\\", mustWork = TRUE)
  safe_path <- utils::shortPathName(abs_path)  # <-- clave en Windows con tildes
} else {
  # Fallback por si alguien ejecuta desde otro WD:
  abs_path <- "C:/Users/JOntivero/Desktop/R/Shiny/Clases Shiny/Trabajo Final/App_Educación/www/ar-all.geo.json"
  safe_path <- if (file.exists(abs_path)) utils::shortPathName(abs_path) else abs_path
}

# 3) Leer el GeoJSON
ar_map <- jsonlite::read_json(safe_path)

# (opcional) Validación rápida
if (is.null(ar_map$features)) stop("El GeoJSON se leyó pero no tiene 'features'. ¿Archivo corrupto?")