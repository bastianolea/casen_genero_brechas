library(shiny)
library(fresh)
library(shinyWidgets)

color_fondo = "#3c1b1f"
color_secundario = "#db8d6c"
color_detalle = "#62393e"
color_texto = "#fecea8"
color_destacado = "#b21e4b"
color_enlaces = "#cc3865"

ui <- fluidPage(
  title = "Brechas de género", 
  lang = "es",
  
  #tema ----
  use_googlefont("Urbanist"), #cargar fuente o tipo de letra
  use_googlefont("DM Serif Display"),
  
  use_theme(
    create_theme(
      theme = "default",
      bs_vars_input(bg = color_fondo),
      bs_vars_global(body_bg = color_fondo, 
                     text_color = color_texto, 
                     link_color = color_destacado
      ),
      bs_vars_font(size_base = "19px", #aumentar globalmente tamaño de letra  
                   family_sans_serif = "Urbanist" #cargar fuente o tipo de letra
      ), 
      bs_vars_modal(content_bg = color_fondo, 
                    content_border_color = color_detalle, 
                    backdrop_bg = color_fondo, 
                    backdrop_opacity = "60%"
      ),
      bs_vars_wells(bg = color_detalle
      ),
      bs_vars_button(
        default_color = color_fondo,
        default_bg = color_destacado,
        default_border = color_fondo, 
        border_radius_base = "6px"
      )
    )
  ),
  
  
  #titulo ----
  titlePanel("Brechas de género"),
  
  #sidebar ----
  sidebarLayout(
    sidebarPanel(
      pickerInput("tema",
                  "Escoja una temática",
                  choices = c("Ingresos",
                              "Trabajo",
                              "Vivienda",
                              "Pensiones",
                              "Educación"
                              )
      ),
      pickerInput("variable",
                  "Escoja una variable",
                  choices = NULL
      )
    ),
    
    
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

#—----
server <- function(input, output) {
  
  
}


shinyApp(ui = ui, server = server)
