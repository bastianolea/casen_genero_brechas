library(shiny)
library(fresh)
library(ggplot2)
library(dplyr)
library(forcats)
library(stringr)
library(tidyr)
library(ggnewscale)
library(colorspace)
library(glue)
library(shinyWidgets)

color_texto = "white"
color_secundario = "#95008f"

color_positivo = "#00718d" #048486" #"#10607f" #"#007c81"
color_negativo = "#912c6f" #"#7e1d1f" 
color_neutro = "#3d169f"

color_fondo = "#1c0e44"
color_recuadro = "#261060"

color_femenino =  "#6e47c5" #"#7f40c5" #"#9a75fb"
color_masculino = "#770072" #"#d8006b"

color_detalle = "#770072"
color_texto = "#c1abfb"
color_destacado = "#d8006b"
color_enlaces = color_destacado


casen_genero <- readr::read_csv2("casen_genero_regiones.csv")
  
source("variables.R")


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
      bs_vars_wells(bg = color_recuadro
      ),
      bs_vars_button(
        default_color = color_texto,
        default_bg = color_recuadro,
        default_border = color_fondo, 
        border_radius_base = "6px"
      )
    )
  ),
  
  # css ----
  #separador
  tags$style(paste0("
                    hr {
  border-top: 3px solid ", color_recuadro, ";
                    }")),
  
  
  #titulo ----
  fluidRow(
    column(12,
  h1("Brechas de género", style = glue("color: {color_destacado}")),
  
  p("Esta es una aplicación muy bonita y divertida")
    )
  ),
  
  # selectores ----
  fluidRow(
    column(12,
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
                  selected = "pobreza_p", multiple = F,
                  choices = variables
      )
    )
  ),
    
    
  fluidRow(
    column(12,
      plotOutput("grafico_regiones")
    )
  ),
  
  # firma ----
  fluidRow(
    column(12,
           hr(),
           p("Diseñado y programado por",
             tags$a("Bastián Olea Herrera.", target = "_blank", href = "https://bastian.olea.biz")),
           p(
             "Código de fuente de esta app y del procesamiento de los datos",
             tags$a("disponible en GitHub.", target = "_blank", href = "https://github.com/bastianolea/casen_relacionador")
           ),
           div(style = "height: 40px")
           
    )
  )
)

#—----
server <- function(input, output) {
  
  # datos ----
  datos <- reactive({
    # req(length(input$selector_regiones) > 0)
    
    # .variable = "hacinamiento_p"
    .variable = input$variable
    
    message("variable: ", .variable)
    
    # browser()
    
    dato <- casen_genero |>
      select(region, sexo, variable = all_of(.variable)) |> 
      pivot_wider(id_cols = region, names_from = "sexo", values_from = variable) |>
      rowwise() |> 
      mutate(brecha = Femenino - Masculino,
             valor_min = min(c(Femenino, Masculino)),
             valor_max = max(c(Femenino, Masculino)),
             brecha_dir = ifelse(brecha <= 0, "negativa", "positiva"),
             brecha_end = ifelse(brecha_dir == "positiva", valor_min + brecha, valor_min - brecha),
             brecha_chica = ifelse(abs(brecha) < 0.015, "chica", "normal"),
             brecha_hay = ifelse(abs(brecha) > 0.01, "sí", "no")) |> 
      ungroup() |> 
      pivot_longer(cols = c(Masculino, Femenino), names_to = "sexo", values_to = "variable") |> 
      mutate(variable_p = scales::percent(variable, accuracy = 1),
             brecha_p = scales::percent(brecha, accuracy = 1.1)) |> 
      group_by(region) |>
      mutate(valor_pos = ifelse(variable == max(variable), "arriba", "abajo"),
             valor_prom = mean(variable)) |> 
      ungroup() |> 
      mutate(valor_prom_pais = mean(variable, na.rm = T)) |> 
      relocate(starts_with("brecha"), .after = 0) |> 
      mutate(region = region |> str_remove("Región de |Región del |Región ") |> str_wrap(16) |> as.factor(),
             region = region |> fct_reorder(variable, .desc = T)
      )
    return(dato)
  })
  
  
  #grafico ----
  output$grafico_regiones <- renderPlot({
    # browser()
    # dev.new()
    datos() |> 
      # print(n=Inf) |> 
      ggplot(aes(region, variable)) + 
      #fondos redondeados
      geom_segment(aes(xend = region, y = min(variable), yend = max(variable)+0.001),
                   color = color_recuadro, linewidth = 18, lineend = "round", alpha = 0.7) +
      #linea horizontal de promedio
      geom_hline(aes(yintercept = valor_prom_pais), 
                 color = color_fondo, linewidth = 1) +
      #linea vertical de brechas
      geom_segment(aes(xend = region, y = valor_min, yend = brecha_end, color = brecha_dir), 
                   linewidth = 4, lineend = "round", show.legend = F, alpha = 0.8) +
      scale_color_manual(values = c("positiva" = color_positivo, "negativa" = color_negativo)) + #escala para lineas verticales de brecha
      new_scale_colour() +
      #puntos principales por región y sexo
      # geom_point(aes(color = sexo, size = variable*2),
      #            alpha = 1, fill = "red") + #color_neutro) +
      geom_point(aes(color = sexo, size = variable),
                 alpha = 1) +
      scale_color_manual(values = c(color_femenino, color_masculino)) + #escala para puntos principales
      #texto brechas normales
      geom_text(data = datos() |> filter(brecha_hay == "sí" & brecha_chica == "normal"), 
                aes(label = scales::percent(variable, accuracy = 1)),
                color = "white", size = 3.4) +
      #texto brechas chicas (mas separado hacia los extremos para que no se peguen
      geom_text(data = datos() |> filter(brecha_hay == "sí" & brecha_chica == "chica"),
                aes(label = variable_p, y = ifelse(valor_pos == "arriba", variable + 0.0025, variable - 0.0025)), color = "white", size = 3) +
      #texto sin brecha (un solo texto para ambos puntos)
      geom_text(data = datos() |> filter(brecha_hay == "no" & sexo == "Femenino"), 
                aes(y = valor_prom, label = scales::percent(variable, accuracy = 1)),
                color = "white", size = 3.4, check_overlap = TRUE) +
      scale_fill_manual(values = c("positiva" = color_positivo, "negativa" = color_negativo)) +
      #label brecha
      geom_label(data = datos() |> filter(sexo == "Femenino" & abs(brecha) > 0.01),
                 aes(label = brecha_p, fill = brecha_dir, 
                     y = ifelse(valor_min < mean(variable), 
                                valor_max + (valor_max*0.07), #etiquetas arriba
                                valor_min - (valor_min*0.07)) #etiquetas abajo
                 ),
                 color = "white", alpha = 0.9, label.size = 0,
                 size = 3) +
      geom_point(aes(fill = brecha_dir), alpha = 0) +
      #label sin brecha
      geom_label(data = datos() |> filter(brecha_hay == "no" & sexo == "Femenino"),
                 aes(label = "0%", y = valor_max + (valor_max*0.07)),
                 fill = color_neutro, color = "white", size = 3, label.size = 0) +
      scale_size(range = c(12, 15), guide = NULL) +
      scale_alpha_continuous(range = c(0, 1)) + 
      scale_y_continuous(expand = expansion(c(0.1, 0.1)), n.breaks = 6, labels = ~scales::percent(.x), trans = "log10") +
      coord_cartesian(clip = "off") +
      # guides(fill = guide_legend(override.aes = list(size=10, label = "", fill = color_fondo, alpha = 1, color = c(color_positivo, color_negativo)), title = "brecha", reverse = T),
             # colour = guide_legend(override.aes = list(size=10), title = "género")) +
      theme_minimal() +
      theme(text = element_text(color = color_texto), 
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, margin = margin(t = -3), color = color_texto), axis.title.x = element_blank(),
            axis.text.y = element_text(margin = margin(r = -4, l = 5)), axis.title.y = element_text(color = color_secundario),
            panel.grid = element_blank(),
            legend.box.margin = margin(l=-5), legend.title = element_text(color = color_secundario),
            panel.background = element_rect(fill = color_fondo, linewidth = 0),
            plot.background = element_rect(fill = color_fondo, linewidth = 0),
            legend.background = element_rect(fill = "transparent", linewidth = 0)
      )
    
  })
  
}


shinyApp(ui = ui, server = server)
