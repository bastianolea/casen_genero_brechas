
# browser()
dev.new()

datos2 <- datos() |> 
  mutate(variable = ifelse(sexo == "Masculino", 0-variable, variable))

datos2 |> 
  # print(n=Inf) |> 
  ggplot(aes(y = region, x = variable)) +
  geom_col(aes(fill = sexo), width = 0.6) +
  geom_text(data = datos2 |> filter(sexo == "Femenino"),
            aes(label = variable_p, x = variable-0.01), 
            hjust = 1, color = color_texto, size = 3) +
  geom_text(data = datos2 |> filter(sexo == "Masculino"),
            aes(label = variable_p, x = variable+0.01), 
            hjust = 0, color = color_texto, size = 3) +
  geom_vline(xintercept = 0, color = color_fondo, linewidth = 1.6) +
  scale_fill_manual(values = c("Femenino" = color_femenino, "Masculino" = color_masculino)) + #escala para puntos principales
  scale_x_continuous(limits = c(0-max(datos()$variable), max(datos()$variable)),
                     expand = expansion(c(0, 0))) +
  labs(x = "Porcentaje de la población perteneciente a cada género") +
  theme_minimal() +
  #leyenda
  theme(panel.grid = element_blank(),
        legend.position = "top", 
        legend.box.margin = margin(b=15),
        legend.title = element_text(color = color_secundario, face = "bold", size = 12),
        legend.text = element_text(size = 10, margin = margin(r = 10))) +
  #textos
  theme(text = element_text(color = color_texto), 
        axis.text.y = element_text(size = 10,  hjust = 0,
                                   margin = margin(t = 0, r = -30), 
                                   color = color_texto), 
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_text(color = color_secundario, size = 12)) +
  #fondos
  theme(panel.background = element_rect(fill = color_fondo, linewidth = 0),
        plot.background = element_rect(fill = color_fondo, linewidth = 0),
        legend.background = element_rect(fill = "transparent", linewidth = 0)
  ) +
  guides(fill = guide_legend(reverse = T, title = "género: "))

           