server <- function(input, output, session) {
      # --- Observe Event para actualizar cursos según nivel ---
  observeEvent(input$id_nivel, {
    cursos_filtrados <- datos %>%
      filter(nivel == input$id_nivel) %>%
      pull(curso) %>%
      unique() %>%
      as.character()
    
    updateSelectInput(session, "id_curso",
                      choices = sort(cursos_filtrados),
                      selected = sort(cursos_filtrados)[1])
  }, ignoreInit = FALSE)
  
  # --- Cursos válidos (para validar Nivel–Curso en gráficos) ---
  cursos_validos <- reactive({
    if (input$id_nivel == "Primaria") c(orden_primaria, "Total") else c(orden_secundaria, "Total")
  })

# Primer Hoja: MAPAS POR INDICADOR 

  
  # Mapa 1: Abandono interanual
  output$mapa_1 <- renderHighchart({
    df <- datos %>%
      filter(
        Indicador == "Abandono interanual",
        nivel == input$nivel_mapa,
        Año == input$anio_mapa,
        curso == "Total",
        Provincia %in% provincias_oficiales
      ) %>%
      group_by(Provincia) %>%
      summarise(valor = first(valor), .groups = "drop")
    
    if (nrow(df) == 0) return(NULL)
    
    highchart(type = "map") %>%
      hc_add_series_map(
        map = ar_map,
        df = df,
        value = "valor",
        joinBy = c("name", "Provincia"),
        name = "Abandono interanual"
      ) %>%
      hc_colorAxis(
        min = min(df$valor, na.rm = TRUE),
        max = max(df$valor, na.rm = TRUE),
        stops = list(
          list(0,   paletas_mapas[["Abandono interanual"]][1]),
          list(0.5, paletas_mapas[["Abandono interanual"]][2]),
          list(1,   paletas_mapas[["Abandono interanual"]][3])
        ),
        nullColor = "#f5f5f5"
      ) %>%
      hc_tooltip(
        useHTML = TRUE,
        pointFormat = "Provincia: <b>{point.name}</b><br>Abandono interanual: <b>{point.value:.2f}%</b>"
      ) %>%
      hc_title(text = "Abandono interanual")
  })
  
  
  # Mapa 2: Repitencia
  output$mapa_2 <- renderHighchart({
    df <- datos %>%
      filter(
        Indicador == "Repitencia",
        nivel == input$nivel_mapa,
        Año == input$anio_mapa,
        curso == "Total",
        Provincia %in% provincias_oficiales
      ) %>%
      group_by(Provincia) %>%
      summarise(valor = first(valor), .groups = "drop")
    
    if (nrow(df) == 0) return(NULL)
    
    highchart(type = "map") %>%
      hc_add_series_map(
        map = ar_map,
        df = df,
        value = "valor",
        joinBy = c("name", "Provincia"),
        name = "Repitencia"
      ) %>%
      hc_colorAxis(
        min = min(df$valor, na.rm = TRUE),
        max = max(df$valor, na.rm = TRUE),
        stops = list(
          list(0,   paletas_mapas[["Repitencia"]][1]),
          list(0.5, paletas_mapas[["Repitencia"]][2]),
          list(1,   paletas_mapas[["Repitencia"]][3])
        ),
        nullColor = "#f5f5f5"
      ) %>%
      hc_tooltip(
        useHTML = TRUE,
        pointFormat = "Provincia: <b>{point.name}</b><br>Repitencia: <b>{point.value:.2f}%</b>"
      ) %>%
      hc_title(text = "Repitencia")
  })
  
  
  # Mapa 3: Sobreedad
  output$mapa_3 <- renderHighchart({
    df <- datos %>%
      filter(
        Indicador == "Sobreedad",
        nivel == input$nivel_mapa,
        Año == input$anio_mapa,
        curso == "Total",
        Provincia %in% provincias_oficiales
      ) %>%
      group_by(Provincia) %>%
      summarise(valor = first(valor), .groups = "drop")
    
    if (nrow(df) == 0) return(NULL)
    
    highchart(type = "map") %>%
      hc_add_series_map(
        map = ar_map,
        df = df,
        value = "valor",
        joinBy = c("name", "Provincia"),
        name = "Sobreedad"
      ) %>%
      hc_colorAxis(
        min = min(df$valor, na.rm = TRUE),
        max = max(df$valor, na.rm = TRUE),
        stops = list(
          list(0,   paletas_mapas[["Sobreedad"]][1]),
          list(0.5, paletas_mapas[["Sobreedad"]][2]),
          list(1,   paletas_mapas[["Sobreedad"]][3])
        ),
        nullColor = "#f5f5f5"
      ) %>%
      hc_tooltip(
        useHTML = TRUE,
        pointFormat = "Provincia: <b>{point.name}</b><br>Sobreedad: <b>{point.value:.2f}%</b>"
      ) %>%
      hc_title(text = "Sobreedad")
  })
  
  
  # Mapa 4: Promoción Efectiva
  output$mapa_4 <- renderHighchart({
    df <- datos %>%
      filter(
        Indicador == "Promocion Efectiva",
        nivel == input$nivel_mapa,
        Año == input$anio_mapa,
        curso == "Total",
        Provincia %in% provincias_oficiales
      ) %>%
      group_by(Provincia) %>%
      summarise(valor = first(valor), .groups = "drop")
    
    if (nrow(df) == 0) return(NULL)
    
    highchart(type = "map") %>%
      hc_add_series_map(
        map = ar_map,
        df = df,
        value = "valor",
        joinBy = c("name", "Provincia"),
        name = "Promoción Efectiva"
      ) %>%
      hc_colorAxis(
        min = min(df$valor, na.rm = TRUE),
        max = max(df$valor, na.rm = TRUE),
        stops = list(
          list(0,   paletas_mapas[["Promocion Efectiva"]][1]),
          list(0.5, paletas_mapas[["Promocion Efectiva"]][2]),
          list(1,   paletas_mapas[["Promocion Efectiva"]][3])
        ),
        nullColor = "#f5f5f5"
      ) %>%
      hc_tooltip(
        useHTML = TRUE,
        pointFormat = "Provincia: <b>{point.name}</b><br>Promoción Efectiva: <b>{point.value:.2f}%</b>"
      ) %>%
      hc_title(text = "Promoción Efectiva")
  })
  
# Segunda Hoja: COMPARATIVA DE INDICADORES CON OTRAS PROVINCIAS
  # ---------- COMPARATIVA ----------
  data_comparativa <- reactive({
    req(input$id_indicador, input$id_nivel, input$id_anio, input$id_curso)
    
    datos %>%
      filter(
        Indicador == input$id_indicador,
        nivel == input$id_nivel,
        (curso == input$id_curso | curso == "Total"),
        Provincia %in% c("Mendoza", input$id_provincia_comp)
      )
  })
  
  output$grafico_comparativo_lineas <- renderPlotly({
    # Validación: curso debe corresponder al nivel
    req(input$id_curso %in% cursos_validos())
    
    df <- data_comparativa() %>%
      filter(curso == input$id_curso) %>%
      mutate(Año = as.integer(Año))
    
    colores <- setNames(
      c(colores_dge[[input$id_indicador]], colores_dge$comparada),
      c("Mendoza", input$id_provincia_comp)
    )
    
    grafico_lineas <- ggplot(df, aes(x = Año, y = valor, color = Provincia)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2.5) +
      scale_color_manual(values = colores) +
      scale_x_continuous(breaks = sort(unique(df$Año))) +
      labs(x = "Año", y = paste0(input$id_indicador, " (%)")) +
      theme_minimal(base_family = "montserrat")
    
    ggplotly(grafico_lineas, tooltip = c("x", "y", "color")) %>%
      layout(
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor  = 'rgba(0,0,0,0)'
      )
  })
  
  output$grafico_comparativo_barras <- renderPlotly({
    df <- datos %>%
      filter(
        Indicador == input$id_indicador,
        nivel == input$id_nivel,
        Año == input$id_anio,
        curso != "Total",
        Provincia %in% c("Mendoza", input$id_provincia_comp)
      )
    
    niveles <- if (input$id_nivel == "Primaria") orden_primaria else orden_secundaria
    df$curso <- factor(df$curso, levels = niveles)
    
    colores <- setNames(c(colores_dge[[input$id_indicador]], colores_dge$comparada),
                        c("Mendoza", input$id_provincia_comp))
    
    grafico_barras <- ggplot(df, aes(x = curso, y = valor, fill = Provincia)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = colores) +
      labs(x = "Curso", y = paste0(input$id_indicador, " (%)")) +
      theme_minimal(base_family = "montserrat")
    
    ggplotly(grafico_barras, tooltip = c("x", "y", "fill")) %>%
      layout(
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor  = 'rgba(0,0,0,0)'
      )
  })
  
  output$tabla_comparativa <- DT::renderDT({
    data_comparativa() %>%
      filter(curso == "Total") %>%
      select(Provincia, Año, valor) %>%
      arrange(desc(Año)) %>%
      pivot_wider(names_from = Provincia, values_from = valor) %>%
      mutate(Indicador = input$id_indicador, .before = 1)
  }, extensions = "Buttons", options = list(dom = "Bfrtip", buttons = c("copy", "excel")))
  
  # ---------- INDICADORES EN EL TIEMPO ----------
  data_multi <- reactive({
    req(input$id_provincia, input$id_nivel, input$id_indicadores_multi)
    
    datos %>%
      filter(
        Provincia == input$id_provincia,
        nivel == input$id_nivel,
        curso == "Total",
        Indicador %in% input$id_indicadores_multi
      ) %>%
      mutate(Año = as.integer(Año), provincia_label = Provincia)
  })
  
  output$grafico_multi_lineas <- renderPlotly({
    df <- data_multi()
    
    grafico_multi <- ggplot(df, aes(x = Año, y = valor, color = Indicador, text = provincia_label)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2.5) +
      scale_color_manual(values = colores_dge[input$id_indicadores_multi]) +
      scale_x_continuous(breaks = sort(unique(df$Año))) +
      labs(x = "Año", y = "Indicador (%)") +
      theme_minimal(base_family = "montserrat")
    
    ggplotly(grafico_multi, tooltip = c("x", "y", "color", "text")) %>%
      layout(
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor  = 'rgba(0,0,0,0)'
      )
  })
  observeEvent(input$boton_fuente, {
    showModal(modalDialog(
      title = "Consideraciones metodológicas",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Cerrar"),
      
      tags$div(style = "font-size: 14px; line-height: 1.5;",
               
               tags$h5("¿De dónde provienen los datos?"),
               tags$p(
                 "Los datos utilizados en este tablero provienen del Ministerio de Capital Humano de la Nación Argentina, ",
                 "específicamente del Área de Información y Evaluación Educativa. ",
                 "Estos indicadores se elaboran a partir del Relevamiento Anual, un operativo de carácter censal",
                 "cuya unidad de relevamiento y análisis son las unidades educativa (escuelas) recuperando las principales variables",
                 "del sistema educativo."
               ),
               tags$p(
                 "Podés acceder a las bases oficiales y su documentación metodológica en: ",
                 tags$a(href = "https://www.argentina.gob.ar/educacion/evaluacion-e-informacion-educativa/indicadores",
                        target = "_blank", "Indicadores Educativos")
               ),
               
               tags$hr(),
               tags$h5("Definiciones metodológicas de los indicadores"),
               tags$ul(
                 tags$li(tags$b("Tasa de Abandono Interanual:"), " Porcentaje de estudiantes que no se matriculan en el año lectivo siguiente."),
                 tags$li(tags$b("Tasa de Repitencia:"), " Porcentaje de estudiantes que se matriculan como repitentes en el año lectivo siguiente."),
                 tags$li(tags$b("Tasa de Sobreedad:"), " Porcentaje de estudiantes cuya edad excede la edad teórica correspondiente al año de estudio que cursan."),
                 tags$li(tags$b("Tasa de Promoción Efectiva:"), " Porcentaje de estudiantes que se matriculan en el año de estudio siguiente en el ciclo lectivo siguiente.")
               ),
               
               tags$hr(),
               tags$h5("Estructura del sistema educativo"),
               tags$p("Las estructuras educativas pueden variar entre provincias."),
               tags$ul(
                 tags$li(tags$b("Primaria 7 – Secundaria 5 años:"), "Río Negro, Neuquén, Santa Cruz, Mendoza, Santa Fe, La Rioja, Santiago del Estero, Chaco, Misiones, Salta, Jujuy, CABA."),
                 tags$li(tags$b("Primaria 6 – Secundaria 6 años:"), "Formosa, Tucumán, Catamarca, San Juan, San Luis, Córdoba, Corrientes, Entre Ríos, La Pampa, Buenos Aires, Chubut, Tierra del Fuego.")
               ),
               
               tags$hr(),
               tags$h5("Limitaciones de los indicadores"),
               tags$p(
                 "Las tasas de transición derivadas de los modelos de flujo de alumnos permiten analizar los movimientos entre dos años lectivos consecutivos y la trayectoria escolar de las cohortes."
               ),
               tags$p(
                 "Este modelo parte del supuesto de que el sistema educativo es cerrado, es decir, no incorpora estudiantes de otras cohortes entre dos ciclos lectivos consecutivos. ",
                 "Sin embargo, en la práctica este supuesto puede verse afectado por factores como la reinserción de alumnos tras períodos fuera del sistema o las transferencias entre instituciones, gestiones o jurisdicciones."
               ),
               tags$p(
                 "Estas situaciones pueden generar sesgos en las tasas de promoción efectiva, repitencia y abandono interanual, ",
                 "dando lugar en algunos casos a valores de promoción superiores al 100% o a valores negativos de abandono."
               ),
               
               tags$hr(),
               tags$h5("Procesamiento de datos"),
               tags$p(
                 "Los datos fueron procesados y agregados a nivel provincial y nacional para su visualización en este tablero. ",
                 "El procedimiento completo de limpieza, cálculo y agregación se encuentra documentado en el siguiente repositorio:"
               ),
               tags$p(tags$a(href = "https://github.com/JavierOnti/APP_INDICADORES_EDUCACION", target = "_blank", "👉 Repositorio en GitHub")),
               
               tags$hr(),
               tags$h5("Contacto"),
               tags$p("📧 ontivero.chino@gmail.com"),
               tags$p(
                 tags$a(
                   href = "https://www.linkedin.com/in/javier-ontivero95",
                   target = "_blank",
                   style = "text-decoration: none; color: #0077b5; font-weight: bold;",
                   tags$img(src = "https://cdn.jsdelivr.net/gh/simple-icons/simple-icons/icons/linkedin.svg", 
                            height = "18px", style = "vertical-align: middle; margin-right: 8px;"),
                   "Javier Ontivero"
                 )
               )
      )
    ))
  })
  observeEvent(input$entrar_app, {
    runjs("document.getElementById('splash').style.display = 'none';")
  })
  
} 