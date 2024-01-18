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
library(shinycssloaders)

#colores ----
color_fondo = "#1c0e44"
color_recuadro = "#261060"

color_texto = "#c1abfb"
color_destacado = "#d8006b"

color_femenino =  "#6e47c5" #"#7f40c5" #"#9a75fb"
color_masculino = "#770072" #"#d8006b"

color_positivo = "#00718d" #048486" #"#10607f" #"#007c81"
color_negativo = "#912c6f" #"#7e1d1f" 
color_neutro = "#431f9e"

color_secundario = "#95008f"
color_detalle = "#770072"


#cargar datos ----
# setwd("app/")
casen_genero <- readr::read_csv2("casen_genero_regiones.csv", 
                                 col_types = c("f", "c", "n"), locale = readr::locale(decimal_mark = ","))

#cargar variables
source("variables.R")

css_p_inline = "display: inline; color: white; border-radius: 4px; padding-left: 4px; padding-right: 4px;"

#ui ----
ui <- fluidPage(
  title = "Brechas de género", 
  lang = "es",
  
  ##tema ----
  use_googlefont("Urbanist"), #cargar fuente o tipo de letra
  # use_googlefont("DM Serif Display"),
  
  use_theme(
    create_theme(
      theme = "default",
      bs_vars_input(bg = color_fondo, 
                    color = color_texto, 
                    color_placeholder = color_recuadro, 
                    border_focus = color_destacado),
      bs_vars_global(body_bg = color_fondo, 
                     link_hover_color = color_femenino,
                     text_color = color_texto, 
                     link_color = color_destacado
      ),
      bs_vars_font(size_base = "18px", #aumentar globalmente tamaño de letra  
                   family_sans_serif = "Urbanist" #cargar fuente o tipo de letra
      ), 
      # bs_vars_modal(content_bg = color_fondo, 
      #               content_border_color = color_detalle, 
      #               backdrop_bg = color_fondo, 
      #               backdrop_opacity = "60%"
      # ),
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
  
  ## css ----
  #separador
  tags$style(paste0("
                    hr {
  border-top: 3px solid ", color_recuadro, ";
                    }")),
  
  
  #texto de pickers
  tags$style(paste0("
                    .btn.dropdown-toggle {
                   font-size: 85%;
                   }")),
  
  #colores pickers
  tags$style(paste0("
         .dropdown-menu,  .divider {
         background: ", color_recuadro, " !important;
         }
  
         .dropdown-header {
         font-weight: bold;
         font-size: 120%;
         }
         
         .text {
         color: ", color_texto, " !important;
         }
         
         .form-control {
         color: ", color_texto, " !important;
         box-shadow: none;
         }
         
         .no-results {
         color: black !important;
         background: ", color_destacado, " !important;
         }
         
         /*color de fondo de opción elegida*/
         .dropdown-item.selected {
         background-color: ", color_destacado, " !important;
         color: black !important;
         }
         
         /*color del fondo de la opción en hover*/
         .dropdown-item:hover {
         color: red;
         background-color: ", color_femenino, " !important;
         }
         ")),
  # .dropdown-menu>li>a:hover, .dropdown-menu>li>a:focus {
  
  
  ## header ----
  fluidRow(
    column(12,
           div(style = "margin-bottom: 16px;",
               h1("Brechas de género", style = glue("color: {color_destacado}")),
               p("por", em("Bastián Olea Herrera"))
           ),
           
           #texto
           div(
             style = "display: inline; padding-bottom: 12px;",
             p("Este visualizador produce gráficos regionales de brechas de género a partir de los datos de la",
               tags$a("Encuesta de caracterización socioeconómica nacional (Casen) 2022.", target = "_blank", href = "https://observatorio.ministeriodesarrollosocial.gob.cl/encuesta-casen-2022"),
               "Un gráfico de brecha de género visualiza un mismo dato dividido por sexo, explicitando la diferencia o brecha entre hombres y mujeres en dicha medición."),
             p("Seleccione una o varias temáticas, y luego elija una variable para visualizar las posibles brechas de género en ese dato."),
             p("En el gráfico, cada columna representa a una región del país, y los círculos representan el porcentaje de",
               p("mujeres", style = glue("{css_p_inline} background: {color_femenino};")), 
               "y",
               p("hombres", style = glue("{css_p_inline} background: {color_masculino};")), 
               "que viven situación o condición de vida indicada por la variable. En las variables que refieren a hogares o viviendas, el porcentaje representa hogares encabezados por el género correspondiente. La línea vertical que conecta ambos círculos es la brecha: ",  
               p("positiva", style = glue("{css_p_inline} background: {color_positivo};")), 
               "si hay mas mujeres que hombres, o",
               p("negativa", style = glue("{css_p_inline} background: {color_negativo};")), 
               "si hay más hombres que mujeres.",
               style = "display: inline;")
           ),
           hr()
    )
  ),
  
  # selectores ----
  fluidRow(
    column(12, style = "width: 600px;",
           
           pickerInput("tematica",
                       "Escoja las temáticas de su interés", width = "100%",
                       choices = names(variables), 
                       selected = c(names(variables)[1], names(variables)[3]),
                       multiple = T, 
                       options = pickerOptions(noneSelectedText = "Sin selección")
           ),
           pickerInput("variable",
                       "Escoja una variable", width = "100%",
                       selected = variables$Viviendas[10],
                       multiple = F,
                       choices = NULL #sample(c(variables[[1]], variables[[3]]), 1)
           )
    )
  ),
  
  #grafico ----
  fluidRow(
    column(12, 
           style = "min-height: 720px; width: 1024px; overflow-x: scroll;",
           hr(),
           plotOutput("grafico_regiones", height = "720px") |> 
             withSpinner(type = 8, color = color_femenino, proxy.height = "720px")
    )
  ),
  
  
  #region y comunas ----
  # #pone las comunas en el selector de comunas según la región elegida
  # lista_comunas <- reactive({
  #   
  #     
  #   }  
  #   return(lista_comunas)
  # }) |> bindEvent(input$selector_regiones)
  # 
  # 
  # observeEvent(input$selector_regiones, {
  #   req(length(input$selector_regiones) > 0)
  #   
  #   updatePickerInput(session,
  #                     inputId = "selector_comunas",
  #                     options = list( `live-search` = TRUE),
  #                     selected = c("La Florida", "La Pintana", "Ñuñoa", "Vitacura", "Providencia"),
  #                     choices = lista_comunas()
  #   )
  # })
  
  # fluidRow(
  #   column(12,
  #          pickerInput("region",
  #                      "Escoja una región", width = "100%",
  #                      selected = "Región Metropolitana de Santiago", multiple = F,
  #                      choices = unique(casen_genero$region)
  #          )
  #          
  #   )
  # ),
  
  # firma ----
  fluidRow(
    column(12,
           hr(),
           p("Diseñado y programado por",
             tags$a("Bastián Olea Herrera.", target = "_blank", href = "https://bastian.olea.biz")),
           p(
             "Código de fuente de esta app y del procesamiento de los datos",
             tags$a("disponible en GitHub.", target = "_blank", href = "https://github.com/bastianolea/casen_genero_brechas")
           ),
           div(style = "height: 40px")
           
    )
  )
)

#—----
server <- function(input, output, session) {
  
  #variables ----
  #filtrar variables según temática elegida
  observeEvent(input$tematica, {
    # observe({
    req(length(input$tematica) > 0)
    # temas <- c(names(variables)[4], names(variables)[5])
    # variables[temas]
    temas <- input$tematica
    
    message("temáticas: ", paste(temas, collapse = ", "))
    
    updatePickerInput(session, 
                      "variable",
                      "Escoja una variable",
                      choices = variables[temas]
    )
  }, suspended = F)
  
  
  
  # datos ----
  datos <- reactive({
    req(length(input$variable) > 0)
    # req(input$variable %in% names(casen_genero))
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
             brecha_dir = ifelse(brecha <= 0, " Negativa", " Positiva"),
             brecha_end = ifelse(brecha_dir == " Positiva", valor_min + brecha, valor_min - brecha),
             brecha_chica = ifelse(abs(brecha) < 0.02, "chica", "normal"),
             brecha_hay = ifelse(abs(brecha) > 0.008, "sí", "no")) |> 
      ungroup() |> 
      pivot_longer(cols = c(Masculino, Femenino), names_to = "sexo", values_to = "variable") |> 
      mutate(variable_p = scales::percent(variable, accuracy = 1),
             brecha_p = scales::percent(brecha, accuracy = 1)) |> 
      group_by(region) |>
      mutate(valor_pos = ifelse(variable == max(variable), "arriba", "abajo"),
             valor_prom = mean(variable)) |> 
      ungroup() |> 
      mutate(valor_prom_pais = mean(variable, na.rm = T)) |> 
      relocate(starts_with("brecha"), .after = 0) |> 
      mutate(region = region |> str_remove("Región de |Región del |Región ") |> str_wrap(16) |> as.factor(),
             region = region |> fct_reorder(variable, .desc = T)
      )
    
    # print(dato, n=Inf)
    return(dato)
  }) |> bindEvent(input$tematicas, input$variable)
  
  
  #grafico ----
  output$grafico_regiones <- renderPlot({
    req(datos())
    # browser()
    # dev.new()
    
    paleta_brechas <- c("positiva" = color_positivo, "negativa" = color_negativo)
    paleta_brechas_filtrada <- paleta_brechas[names(paleta_brechas) %in% unique(datos()$brecha_dir)]
    
    rango <- max(datos()$valor_max) - min(datos()$valor_min)
    
    espaciado_texto_valores_pegados = 0.001
    espaciado_label_brecha = 0.035*(rango*2.5)
    
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
                   linewidth = 4, lineend = "round", show.legend = T, alpha = 0.8) +
      scale_color_manual(values = c(" Positiva" = color_positivo, " Negativa" = color_negativo), name = "brecha: ") + #escala para lineas verticales de brecha
      new_scale_colour() +
      #puntos de sombra
      geom_point(data = datos() |> filter(valor_pos == "arriba"), 
                 aes(color = sexo, size = variable), color = color_recuadro, size = 15, alpha = 0.2) +
      geom_point(data = datos() |> filter(valor_pos == "abajo"),
                 aes(color = sexo, size = variable, y = variable-0.0008), color = color_fondo, size = 13, alpha = 0.4) +
      #puntos principales por región y sexo
      geom_point(aes(color = sexo, size = variable),
                 alpha = 1) +
      scale_color_manual(values = c("Femenino" = color_femenino, "Masculino" = color_masculino)) + #escala para puntos principales
      #texto brechas normales
      geom_text(data = datos() |> filter(brecha_hay == "sí"), #|> filter(brecha_chica == "normal"), 
                aes(label = scales::percent(variable, accuracy = 1)),
                color = "white", size = 3.4) +
      # #texto brechas chicas (mas separado hacia los extremos para que no se peguen
      # geom_text(data = datos() |> filter(brecha_hay == "sí" & brecha_chica == "chica"),
      #           aes(label = variable_p, y = ifelse(valor_pos == "arriba", variable + espaciado_texto_valores_pegados, variable - espaciado_texto_valores_pegados)), color = "white", size = 3) +
      #texto sin brecha (un solo texto para ambos puntos)
      geom_point(data = datos() |> filter(brecha_hay == "no" & sexo == "Femenino"),
                 aes(y = valor_prom),
                 color = color_femenino, size = 10, alpha = .6) +
      geom_text(data = datos() |> filter(brecha_hay == "no" & sexo == "Femenino"),
                aes(y = valor_prom, label = scales::percent(variable, accuracy = 1)),
                color = "white", size = 3.4, check_overlap = TRUE) +
      scale_fill_manual(values = c(" Positiva" = color_positivo, " Negativa" = color_negativo), name = "brecha: ") +
      #label brecha
      geom_label(data = datos() |> filter(sexo == "Femenino" & abs(brecha) > 0.01),
                 aes(label = brecha_p, fill = brecha_dir, 
                     y = ifelse(valor_min <= valor_prom_pais, 
                                valor_max + espaciado_label_brecha, #etiquetas arriba
                                valor_min - espaciado_label_brecha) #etiquetas abajo
                 ),
                 color = "white", alpha = 0.9, label.size = 0, size = 3, show.legend = F) +
      #label sin brecha
      # geom_label(data = datos() |> filter(brecha_hay == "no" & sexo == "Femenino"),
      #            aes(label = "OK", 
      #                # y = valor_max + (valor_max*0.07)
      #                y = ifelse(valor_min <= valor_prom_pais, 
      #                           valor_max + espaciado_label_brecha, #etiquetas arriba
      #                           valor_min - espaciado_label_brecha) #etiquetas abajo
      #                ),
      #            fill = color_neutro, color = "white", size = 3, label.size = 0) +
      scale_size(range = c(12, 12), guide = NULL) +
      scale_alpha_continuous(range = c(0, 1)) + 
      scale_y_continuous(expand = expansion(c(0, 0)), labels = ~scales::percent(.x, accuracy = 1)) + #, trans = "log10") +
      coord_cartesian(clip = "off") +
      guides(
        # fill = guide_legend(override.aes = list(size=10, label = "", fill = color_fondo, alpha = 1, 
        #                                              values = paleta_brechas_filtrada), title = "brecha"),
        # fill = "none",     
        # fill = guide_legend(override.aes = list(size = 6, label = ""), title = "brecha:", nrow = 1, order = 99),
        # fill = guide_legend(override.aes = list(size = 10, label = "", fill = NA, alpha = 1), title = "Brecha:", nrow = 1),
        fill = guide_legend(override.aes = list(size = 10, alpha = 1), title = "brecha: ", label.theme = element_text(margin = margin(l = 10)), nrow = 1),
        colour = guide_legend(override.aes = list(size = 10), title = "género:", nrow = 1, order = 1)
      ) +
      labs(y = "Porcentaje de la población perteneciente a cada género") +
      theme_minimal() +
      #leyenda
      theme(panel.grid = element_blank(),
            legend.position = "top", 
            legend.box.margin = margin(b=15),
            legend.title = element_text(color = color_secundario, face = "bold", size = 12),
            legend.text = element_text(size = 10, margin = margin(r = 10))) +
      #textos
      theme(text = element_text(color = color_texto), 
            axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = 0.5, 
                                       margin = margin(t = 20, b=0), color = color_texto), 
            axis.title.x = element_blank(),
            axis.text.y = element_text(size = 10, margin = margin(l = 3, r = -6), color = color_texto), 
            axis.title.y = element_text(color = color_secundario, size = 12)) +
      #fondos
      theme(panel.background = element_rect(fill = color_fondo, linewidth = 0),
            plot.background = element_rect(fill = color_fondo, linewidth = 0),
            legend.background = element_rect(fill = "transparent", linewidth = 0)
      )
    
    # if (rango < 0.2) {
    #   p <- p +
    #     # coord_cartesian(clip = "off", )
    #     scale_y_continuous(expand = expansion(c(0.9, 0.9)))
    # }
    
    # p
    
  }, res = 90)
  
}


shinyApp(ui = ui, server = server)
