library(ggplot2)
library(dplyr)
library(forcats)
library(stringr)
library(tidyr)
library(ggnewscale)
library(colorspace)
setwd("app/")
casen_genero <- readr::read_csv2("casen_genero_regiones.csv")
# casen_genero <- readr::read_csv2("app/casen_genero_regiones.csv")
datos <- function() casen_genero

color_fondo = "#1c0e44"
color_texto = "white"
color_secundario = "#95008f"
color_femenino = "#6d3ee4"
color_masculino = "#d1008a"
color_positivo = "#2ea146"
color_negativo = "#a42f2f"
color_neutro = "#3d169f"


.variable = "pobreza_p"
.variable = "hacinamiento_p"

# gráfico de barras con barra de brecha en medio ----
datos() |>
  select(region, sexo, all_of(.variable)) |> 
  pivot_wider(id_cols = region, names_from = "sexo", values_from = .variable) |>
  rowwise() |> 
  mutate(brecha = Femenino - Masculino,
         valor_min = min(c(Femenino, Masculino)),
         valor_max = max(c(Femenino, Masculino)),
         brecha_dir = ifelse(brecha <= 0, "negativa", "positiva"),
         brecha_end = ifelse(brecha_dir == "positiva", valor_min + brecha, valor_min - brecha)) |> 
  pivot_longer(cols = c(Masculino, Femenino), names_to = "sexo", values_to = .variable) |> 
  relocate(starts_with("brecha"), .after = 0) |> 
  mutate(region = region |> str_remove("Región de ") |> str_wrap(16) |>  as.factor() |> fct_reorder(!!sym(.variable), .desc = T)) |> 
  print() |> 
  ggplot(aes(region, .data[[.variable]])) +
  geom_col(aes(fill = sexo), position = position_dodge(),
           width = 0.5) +
  scale_fill_manual(values = c(color_femenino, color_masculino)) +
  new_scale_colour() +
  geom_segment(aes(xend = region, y = valor_min, yend = brecha_end, color = brecha_dir), 
               linewidth = 3) +
  scale_color_manual(values = c("positiva" = color_positivo, "negativa" = color_negativo)) +
  theme_minimal() +
  theme(text = element_text(color = color_texto), 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, margin = margin(t = -3), color = color_texto), axis.title.x = element_blank(),
        axis.text.y = element_text(margin = margin(r = -8, l = 5)), axis.title.y = element_text(color = color_secundario),
        panel.grid = element_blank(),
        legend.box.margin = margin(l=-15), legend.title = element_text(color = color_secundario),
        panel.background = element_rect(fill = color_fondo, linewidth = 0),
        plot.background = element_rect(fill = color_fondo, linewidth = 0),
        legend.background = element_rect(fill = color_fondo, linewidth = 0))


#gráficos de puntos con línea de brecha que los conecta ----
datos2 <- datos() |>
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
         ) |> 
  print(n=Inf)
  


datos2 |> 
  print(n=Inf) |> 
  ggplot(aes(region, variable)) + 
  #fondos redondeados
  geom_segment(aes(xend = region, y = min(variable), yend = max(variable)+0.001),
                   color = color_neutro, linewidth = 18, lineend = "round", alpha = 0.1) +
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
  geom_text(data = datos2 |> filter(brecha_hay == "sí" & brecha_chica == "normal"), 
            aes(label = scales::percent(variable, accuracy = 1)),
            color = "white", size = 3.4) +
  #texto brechas chicas (mas separado hacia los extremos para que no se peguen
  geom_text(data = datos2 |> filter(brecha_hay == "sí" & brecha_chica == "chica"),
            aes(label = variable_p, y = ifelse(valor_pos == "arriba", variable + 0.0025, variable - 0.0025)), color = "white", size = 3) +
  #texto sin brecha (un solo texto para ambos puntos)
  geom_text(data = datos2 |> filter(brecha_hay == "no" & sexo == "Femenino"), 
            aes(y = valor_prom, label = scales::percent(variable, accuracy = 1)),
            color = "white", size = 3.4, check_overlap = TRUE) +
  scale_fill_manual(values = c("positiva" = color_positivo, "negativa" = color_negativo)) +
  #label brecha
  geom_label(data = datos2 |> filter(sexo == "Femenino" & abs(brecha) > 0.01),
             aes(label = brecha_p, fill = brecha_dir, 
                 y = ifelse(valor_min < mean(variable), 
                            valor_max + (valor_max*0.07), #etiquetas arriba
                            valor_min - (valor_min*0.07)) #etiquetas abajo
                 ),
            color = "white", alpha = 0.9, label.size = 0,
            size = 3) +
  geom_point(aes(fill = brecha_dir), alpha = 0) +
  #label sin brecha
  geom_label(data = datos2 |> filter(brecha_hay == "no" & sexo == "Femenino"),
             aes(label = "0%", y = valor_max + (valor_max*0.07)),
             fill = color_neutro, color = "white", size = 3, label.size = 0) +
  scale_size(range = c(12, 15), guide = NULL) +
  scale_alpha_continuous(range = c(0, 1)) + 
  scale_y_continuous(expand = expansion(c(0.1, 0.1)), n.breaks = 6, labels = ~scales::percent(.x), trans = "log10") +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(override.aes = list(size=10, label = "", fill = color_fondo, alpha = 1, color = c(color_positivo, color_negativo)), title = "brecha", reverse = T),
         colour = guide_legend(override.aes = list(size=10), title = "género")) +
  theme_minimal() +
  theme(text = element_text(color = color_texto), 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, margin = margin(t = -3), color = color_texto), axis.title.x = element_blank(),
        axis.text.y = element_text(margin = margin(r = -8, l = 5)), axis.title.y = element_text(color = color_secundario),
        panel.grid = element_blank(),
        legend.box.margin = margin(l=-15), legend.title = element_text(color = color_secundario),
        panel.background = element_rect(fill = color_fondo, linewidth = 0),
        plot.background = element_rect(fill = color_fondo, linewidth = 0),
        legend.background = element_rect(fill = color_fondo, linewidth = 0)
        )
