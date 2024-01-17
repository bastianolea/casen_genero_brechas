library(ggplot2)
library(dplyr)
library(forcats)
library(stringr)
library(tidyr)
library(ggnewscale)

setwd("app/")

casen_genero <- readr::read_csv2("casen_genero_regiones.csv")

datos <- function() {
  casen_genero
}

.variable = "pobreza_p"
.variable = "hacinamiento_p"

# gráfico de barras con barra de brecha en medio
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
  mutate(region = region |> str_remove("Región de ") |>  as.factor() |> fct_reorder(!!sym(.variable), .desc = T)) |> 
  print() |> 
  ggplot(aes(region, .data[[.variable]])) +
  geom_col(aes(fill = sexo), position = position_dodge(),
           width = 0.5) +
  scale_fill_manual(values = c("purple", "orange")) +
  new_scale_colour() +
  geom_segment(aes(xend = region, y = valor_min, yend = brecha_end, color = brecha_dir), 
               linewidth = 3) +
  scale_color_manual(values = c("positiva" = "green", "negativa" = "red"))


#gráficos de puntos con línea de brecha que los conecta
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
  relocate(starts_with("brecha"), .after = 0) |> 
  mutate(region = region |> str_remove("Región de |Región del |Región ") |> str_wrap(16) |> as.factor(),
         region = region |> fct_reorder(variable, .desc = T)
         ) |> 
  print(n=Inf)
  


datos2 |> 
  print(n=Inf) |> 
  ggplot(aes(region, variable)) + 
  #fondos redondeados
  geom_segment(aes(xend = region, y = min(variable), yend = max(variable)),
                   color = "black", linewidth = 18, lineend = "round", alpha = 0.1) +
  #linea horizontal de promedio
  geom_hline(aes(yintercept = mean(variable)[1]), 
             color = "white", linewidth = 2) +
  #linea vertical de brechas
  geom_segment(aes(xend = region, y = valor_min, yend = brecha_end, color = brecha_dir), 
               linewidth = 3, lineend = "round", show.legend = F) +
  scale_color_manual(values = c("positiva" = "green", "negativa" = "red")) + #escala para lineas verticales de brecha
  new_scale_colour() +
  #puntos principales por región y sexo
  geom_point(aes(color = sexo, size = variable),
             alpha = 0.7) +
  scale_color_manual(values = c("purple", "orange")) + #escala para puntos principales
  #texto brechas normales
  geom_text(data = datos2 |> filter(brecha_hay == "sí" & brecha_chica == "normal"), 
            aes(label = scales::percent(variable, accuracy = 1)),
            color = "white", size = 3.4) +
  #texto brechas chicas
  geom_text(data = datos2 |> filter(brecha_hay == "sí" & brecha_chica == "chica" & valor_pos == "arriba"),
            aes(label = variable_p), nudge_y = 0.005, color = "white", size = 3) +
  geom_text(data = datos2 |> filter(brecha_hay == "sí" & brecha_chica == "chica" & valor_pos == "abajo"),
            aes(label = variable_p), nudge_y = -0.005, color = "white", size = 3) +
  #texto sin brecha (un solo texto para ambos puntos)
  geom_text(data = datos2 |> filter(brecha_hay == "no" & sexo == "Femenino"), 
            aes(y = valor_prom, label = scales::percent(variable, accuracy = 1)),
            color = "white", size = 3.4, check_overlap = TRUE) +
  #numero brecha label
  geom_label(data = datos2 |> filter(sexo == "Femenino" & abs(brecha) > 0.01),
             aes(label = brecha_p,
                 y = ifelse(valor_min < mean(variable), valor_max + (variable*0.08), valor_min - (variable*0.07))),
            fill = "black", color = "white", size = 3.4) +
  #label sin brecha
  geom_label(data = datos2 |> filter(brecha_hay == "no" & sexo == "Femenino"),
             aes(label = "OK",
                 #y = valor_max + (variable*0.08)),
                 y = valor_prom + 0.035),
             fill = "green", color = "black", size = 3.4) +
  scale_size(range = c(12, 15), guide = NULL) +
  scale_alpha_continuous(range = c(0, 1)) + 
  # guides(alpha = "none") + 
  scale_y_continuous(expand = expansion(c(0.1, 0.1)), n.breaks = 6, labels = ~scales::percent(.x), trans = "log10") +
  coord_cartesian(clip = "off") +
  guides(colour = guide_legend(override.aes = list(size=10), title = NULL)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank())
