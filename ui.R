ui <- tagList(
  useShinyjs(),
  
  # --- Pantalla de bienvenida ---
  div(
    id = "splash",
    style = "
    position: fixed;
    top: 0; left: 0; right: 0; bottom: 0;
    background-color: rgba(140, 190, 195, 0.85);
    color: white;
    z-index: 9999;
    display: flex;
    align-items: center;
    justify-content: center;
    padding: 40px;
    font-family: 'Montserrat', sans-serif;
    text-align: center;
  ",
    div(
      style = "max-width: 850px;",
      
      # Imagen de las personas arriba (6 DE 10)
      tags$img(
        src = "personitas.png",
        style = "max-width: 520px; margin-bottom: 50px;"
      ),
      
      tags$h1(
        style = "font-weight: 600; font-size: 30px; line-height: 1.5; margin-bottom: 40px;",
        HTML("¿Sabías que, a nivel nacional,<br>solo 63 de cada 100 estudiantes terminan la secundaria <strong>en el tiempo esperado</strong> según el calendario escolar?")
      ),
      
      tags$p(
        style = "font-size: 18px; line-height: 1.8; margin-bottom: 25px;",
        HTML("Este tablero interactivo permite explorar los indicadores clave de eficiencia interna del sistema educativo argentino: <strong>Promoción efectiva</strong>, <strong>Repitencia</strong>, <strong>Abandono interanual</strong> y <strong>Sobreedad</strong>.")
      ),
      
      tags$p(
        style = "font-size: 18px; line-height: 1.8;",
        "Los datos están desagregados por provincia, año, nivel educativo y curso.",
        tags$br(), tags$br(),
        "Utilizá el menú superior para comparar provincias, analizar tendencias históricas y explorar trayectorias educativas."
      ),
      
      actionButton("entrar_app", "Explorar la app", class = "btn btn-light btn-lg", style = "margin-top: 40px; font-size: 18px; padding: 10px 30px;")
    )
  ),
  
  # --- App principal ---
  div(
    style = "position: relative; z-index: 1;",
    bslib::page_navbar(
      title = "Indicadores de Eficiencia Interna",
      theme = tema_dashboard,
      
      header = div(
        style = "display: flex; justify-content: flex-end; padding: 10px;",
        actionButton("boton_fuente", "Fuente de datos", icon = icon("info-circle"), class = "btn btn-secondary")
      ),
      
      # --- Pestaña 1: MAPA POR INDICADOR ---
      bslib::nav_panel(
        "Mapa por Indicador",
        icon = bs_icon("globe"),
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            width = 300,
            selectInput("nivel_mapa", "Nivel educativo", choices = niveles_disponibles, selected = "Secundaria"),
            selectInput("anio_mapa", "Año lectivo", choices = años_disponibles, selected = 2022),
            br(),
            div(style = "font-size: 12px; color: #BBB;",
                HTML("Fuente: Ministerio de Capital Humano — <a href='https://www.argentina.gob.ar/educacion/evaluacion-e-informacion-educativa/indicadores' target='_blank'>indicadores oficiales</a>")
            ),
            br(),
            div(style = "font-size: 12px; color: #555;",
                "Nota: Las provincias en blanco/vacías indican falta de datos actualizados al día de la fecha.")
          ),
          bslib::layout_columns(
            col_widths = c(6, 6),
            bslib::card(full_screen = TRUE, highchartOutput("mapa_1", height = "520px")),
            bslib::card(full_screen = TRUE, highchartOutput("mapa_2", height = "520px")),
            bslib::card(full_screen = TRUE, highchartOutput("mapa_3", height = "520px")),
            bslib::card(full_screen = TRUE, highchartOutput("mapa_4", height = "520px"))
          )
        )
      ),
      
      # --- Pestaña 2: COMPARATIVA PROVINCIAL ---
      bslib::nav_panel(
        "Comparativa Provincial",
        icon = bs_icon("bar-chart"),
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            width = 300,
            selectInput("id_nivel", "Nivel educativo", choices = niveles_disponibles, selected = "Secundaria"),
            selectInput("id_anio", "Año lectivo", choices = años_disponibles, selected = 2022),
            selectInput("id_curso", "Curso", choices = NULL),
            selectInput("id_indicador", "Indicador", choices = unique(datos$Indicador)),
            selectInput("id_provincia_comp", "Comparar con:", choices = setdiff(provincias_disponibles, "Mendoza"), selected = "Buenos Aires"),
            br(),
            div(style = "font-size: 11px; color: #555;", "Nota: Las estructuras educativas pueden variar entre provincias."),
            br()
          ),
          tags$div(
            h4("Evolución anual del indicador"),
            plotlyOutput("grafico_comparativo_lineas", height = "300px"),
            br(), br(),
            h4("Detalle por curso (año seleccionado)"),
            plotlyOutput("grafico_comparativo_barras", height = "300px"),
            br(), br(),
            h4("Tabla resumen"),
            DTOutput("tabla_comparativa"),
            br(), br()
          )
        )
      ),
      
      # --- Pestaña 3: INDICADORES EN EL TIEMPO ---
      bslib::nav_panel(
        "Indicadores en el tiempo",
        icon = bs_icon("clock-history"),
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            width = 300,
            selectInput("id_provincia", "Provincia", choices = provincias_disponibles, selected = "Mendoza"),
            selectInput("id_nivel", "Nivel educativo", choices = niveles_disponibles, selected = "Secundaria"),
            checkboxGroupInput("id_indicadores_multi", "Indicadores a comparar:", choices = unique(datos$Indicador), selected = unique(datos$Indicador))
          ),
          tags$div(
            h4("Evolución temporal de los indicadores seleccionados"),
            plotlyOutput("grafico_multi_lineas", height = "500px")
          )
        )
      )
    )
  ),
  
  # -Cierre por inactividad 
  tags$script(HTML("
    var idleTimer;
    function resetTimer() {
      clearTimeout(idleTimer);
      idleTimer = setTimeout(function(){
        Shiny.setInputValue('cerrar_app', Math.random());
      }, 180000);
    }
    window.onload = resetTimer;
    document.onmousemove = resetTimer;
    document.onkeypress = resetTimer;
  "))
)
