#en este script se cargan los datos de la casen preprocesados en casen2022_procesar.r, 
#y se seleccionan las variables a utilizar en el visualizador, calculando las medidas 
#necesarias para optimizar el rendimiento en vivo de la app.

library(dplyr)
library(purrr)
library(stringr)

#cargar datos de casen2022_procesar.r
casen2022_2 <- arrow::read_parquet("datos/casen2022.parquet")


variables_casen <- c(
  "comuna",
  "cut_comuna",
  "region",
  "area",
  "expc",                    #factor de expansión comunal
  "expr",                    #factor de expansión regional
  
  "pco1",                    #jefe de hogar
  
  "sexo",                    #género
  "esc",                     #años de escolaridad
  "edad",                    #edad
  "educ",                    #Nivel de escolaridad
  "s13",                     #s13. ¿A qué sistema previsional de salud pertenece?
  
  #económicas y de salario
  "ytotcorh",                #Ingreso total del hogar corregido
  "ytotcor",                 #Ingreso total corregido
  "yoprcor",                 #Ingreso ocupación principal
  "ypc",                     #Ingreso total per cápita del hogar corregido
  "ytrabajocor",             #ingreso del trabajo
  "ytrabajocorh",            #ingreso del trabajo del hogar
  "y2803",                   #Jubilación o pensión de vejez
  "y0101",                   #Asalariados principal - Sueldos y salarios monetario
  "ytot",                    #Ingreso total
  "dau",                     #Decil autónomo nacional
  "qaut",                    #Quintil autónomo nacional
  "dautr",                   #Decil autónomo regional
  "qautr",                   #Quintil autónomo regional
  
  #social
  "pobreza",                 #pobreza
  "pobreza_multi_5d",        #pobreza multidimensional
  "r1a",                     #nacionalidad
  "r3",                      #pertenencia a pueblos originarios
  
  #laboral
  "activ",                   #actividad
  "contrato",                #Tiene contrato de trabajo
  
  #vivienda
  "numper",                  #numero de personas en el hogar
  "v12mt",                   #metros cuadrados
  "v12",                     #metros cuadrados
  "p9",                      #p9. Incluyéndose a Ud., ¿cuántas personas viven habitualmente en la vivienda?
  # "indmat", (no está)                  #índice de materialidad de la vivienda
  "p2",                      #Indique estado de edificios y casas del sector
  "p3",                      #Presencia de basura en el sector
  "p4",                      #Vandalismo, grafiti o daño deliberado a la propiedad en el sector
  "men18c",                  #Presencia de menores de 18 años en el hogar (excluye SDPA)
  "may60c",                  #Presencia de mayores de 60 años en el hogar (excluye SDPA)
  "v1",                      #v1. ¿Cuál es el tipo de vivienda que ocupa la persona entrevistada?
  "v13",                     #v13. Su hogar, ¿bajo qué situación ocupa la vivienda?
  "hh_d_estado",             #	Hogar carente en estado de la vivienda
  "hh_d_servbas",            #	Hogar carente en servicios básicos
  "o15",                     #	o15. En su trabajo o negocio principal, ¿trabaja como?
  "rama1",
  "rama4"
)

variables_numericas_personas <- c(
  "esc",                     #años de escolaridad
  "edad",                    #edad
  "ytotcor",                 #Ingreso total corregido
  "yoprcor",                 #Ingreso ocupación principal
  "ytrabajocor",             #ingreso del trabajo
  "y2803",                   #Jubilación o pensión de vejez
  "y0101",                   #Asalariados principal - Sueldos y salarios monetario
  "ytot",                    #Ingreso total
  "dau",                     #Decil autónomo nacional
  "qaut",                    #Quintil autónomo nacional
  "dautr",                   #Decil autónomo regional
  "qautr"                    #Quintil autónomo regional
)

variables_numericas_hogar <- c(
  "ytotcorh",                #Ingreso total del hogar corregido
  "ytrabajocorh",            #ingreso del trabajo del hogar
  "ypc",                     #Ingreso total per cápita del hogar corregido
  "numper",                  #numero de personas en el hogar
  "v12mt",                   #metros cuadrados
  "p9"                      #p9. Incluyéndose a Ud., ¿cuántas personas viven habitualmente en la vivienda?
)


# filtrar variables y aplicar factor de expansión ----
casen2022_regiones <- casen2022_2 |> 
  select(any_of(c(variables_casen, variables_numericas_personas, variables_numericas_hogar))) |> 
  tidyr::uncount(weights = expr) |>  #factor de expansión
  # modificar variables
  mutate(sexo = recode(sexo, 
                       "1. Hombre" = "Masculino",
                       "2. Mujer" = "Femenino")) |> 
  mutate(dautr = as.integer(dautr)) |> 
  mutate(qautr = as.integer(qautr)) |> 
  mutate(dau = recode(dau,
                      "I" = 1,
                      "II" = 2,
                      "III" = 3,
                      "IV" = 4,
                      "V" = 5,
                      "VI" = 6,
                      "VII" = 7,
                      "VIII" = 8,
                      "IX" = 9,
                      "X" = 10)) |> 
  mutate(qaut = recode(qaut,
                       "I" = 1,
                       "II" = 2,
                       "III" = 3,
                       "IV" = 4,
                       "V" = 5)) |> 
  mutate(v12mt = readr::parse_integer(as.character(v12mt)))


## probar variables ----
# casen2022_regiones |> count(v12mt)
# casen2022_regiones |> count(area)
# casen2022_regiones |> count(pco1)
# casen2022_regiones |> count(sexo)
# casen2022_regiones |> count(dau)
# casen2022_regiones |> count(qaut)
# casen2022_regiones |> count(dautr)
# casen2022_regiones |> count(v12)
# # casen2022_regiones |> count(indmat)
# casen2022_regiones |> count(p2)
# casen2022_regiones |> count(v13)
# casen2022_regiones |> count(educ)
# 
# median(casen2022_regiones$yoprcor, na.rm = T)
# median(casen2022_regiones$ytrabajocor, na.rm = T)
# median(casen2022_regiones$ytotcor, na.rm = T)
# median(casen2022_regiones$y2803, na.rm = T) #Jubilación o pensión de vejez
# casen2022_regiones |> filter(pco1 == "1. Jefatura de Hogar") |> summarize(median(ypc, na.rm=T),
#                                                                           median(ytrabajocorh, na.rm=T),
#                                                                           median(ytotcorh, na.rm=T))


## graficar variables ----

# options(scipen=99999)
# library(ggplot2)
# casen2022_regiones |> 
#   filter(ytrabajocor < 2000000) |> 
#   filter(yoprcor < 2000000) |> 
#   ggplot() +
#   geom_histogram(aes(ytrabajocor))
#  # geom_histogram(aes(yoprcor))

# calcular variables numéricas ----
# casen2022_numericos_hogar <- casen2022_regiones |> 
#   filter(pco1 == "1. Jefatura de Hogar") |> 
#   # mutate(jefatura = ifelse(pco1 == "1. Jefatura de Hogar" & sexo == "2. Mujer", "femenina", "masculina")) |> 
#   # mutate(sexo = ifelse(pco1 == "1. Jefatura de Hogar" & sexo == "2. Mujer", "Femenino", "Masculino")) |> 
#   group_by(region, sexo) |> 
#   summarize(across(any_of(variables_numericas_hogar |> unname()),
#                    ~mean(.x, na.rm = TRUE)), 
#             .groups = "drop")
# 
# casen2022_numericos_personas <- casen2022_regiones |> 
#   group_by(region, sexo) |> 
#   summarize(across(any_of(variables_numericas_personas |> unname()),
#                    ~mean(.x, na.rm = TRUE)), 
#             .groups = "drop")


# calcular variables de conteo ----
# jefatura_femenina <- casen2022_regiones |> 
#   group_by(region, sexo) |> 
#   count(pco1) |> 
#   ungroup() |> 
#   filter(pco1 == "1. Jefatura de Hogar" & sexo == "2. Mujer") |> 
#   select(region, hogar_jefatura_femenina = n)

# jefatura_femenina <- casen2022_regiones |> 
#   group_by(region, sexo) |> 
#   count(pco1) |> 
#   group_by(pco1, sexo) |> 
#   filter(pco1 == "1. Jefatura de Hogar") |> 
#   tidyr::pivot_wider(id_cols = region, names_from = sexo, values_from = n) |> 
#   rename(jefatura_femenina = 2, jefatura_masculina = 3)

jefatura_femenina <- casen2022_regiones |>
  filter(pco1 == "1. Jefatura de Hogar") |> 
  # mutate(jefatura = ifelse(pco1 == "1. Jefatura de Hogar" & sexo == "2. Mujer", "femenina", "masculina")) |> 
  # mutate(sexo = ifelse(pco1 == "1. Jefatura de Hogar" & sexo == "2. Mujer", "2. Mujer", "1. Hombre")) |> 
  group_by(region, sexo) |> 
  summarize(jefe_de_hogar = n())


# casen2022_regiones |> count(educ)

##variables por persona ----
casen2022_regiones_personas <- casen2022_regiones %>%
  group_by(region, sexo) %>%
  summarize(poblacion = n(),
            #demografía
            originario = sum(r3 != "11. No pertenece a ninguno de estos pueblos indígenas", na.rm = TRUE),
            extranjero = sum(r1a == "3. Otro país (extranjeros)", na.rm=TRUE),
            #pobreza
            pobreza = sum(pobreza == "Pobreza extrema" | pobreza == "Pobreza no extrema", na.rm = TRUE),
            pobreza_multi = sum(pobreza_multi_5d == "Pobreza", na.rm=TRUE),
            #trabajo
            inactivos = sum(activ == "Inactivos", na.rm=TRUE),
            desocupados = sum(activ == "Desocupados", na.rm=TRUE),
            independientes = sum(o15 == "2. Trabajador(a) por cuenta propia", na.rm=T),
            trabajo_familiar_no_remunerado = sum(o15 == "9. Familiar no remunerado", na.rm=T),
            trabajo_servicio_domestico = sum(o15 == "6. Servicio doméstico puertas adentro" | o15 == "7. Servicio doméstico puertas afuera", na.rm=T),
            trabajo_empleador = sum(o15 == "1. Patrón(a) o empleador(a)", na.rm=T),
            trabajo_domestico = sum(rama4 == "Actividades de los hogares como empleadores de personal doméstico", na.rm =T),
            #salud
            fonasa = sum(str_detect(s13, "FONASA"), na.rm=TRUE),
            isapre = sum(str_detect(s13, "Isapre"), na.rm=TRUE), 
            #educación
            estudios_basica_o_menos = sum(edad >= 18 & educ %in% c("Básica completa", "Básica incompleta", "Sin educación formal", "No sabe"), na.rm=T), 
            estudios_sin_media_o_menos = sum(edad >= 18 & educ %in% c("Básica completa", "Básica incompleta", "Sin educación formal", "No sabe", "Media humanista incompleta", "Media técnica profesional incompleta"), na.rm=T), 
            estudios_superiores = sum(educ %in% c("Técnico nivel superior completo", "Profesional completo", "Posgrado incompleto", "Posgrado completo"), na.rm=T),
            estudios_superiores_incompletos = sum(educ %in% c("Técnico nivel superior incompleta", "Profesional incompleto", "Posgrado incompleto"), na.rm=T),
            #ingresos
            ingreso_ocup_princ_menor_mediana = sum(yoprcor <= 500000, na.rm=T), #ingresos totales
            ingreso_ocup_princ_menor_2medianas = sum(yoprcor <= 1000000, na.rm=T), #ingresos totales
            ingreso_ytrabajocor_menor_mediana = sum(ytrabajocor <= 500000, na.rm=T), #ingresos del trabajo
            ingreso_ytrabajocor_menor_2medianas = sum(ytrabajocor <= 1000000, na.rm=T), #ingresos del trabajo
            #pensiones
            pension_menor_mediana = sum(y2803 <= 230000, na.rm = T), #jubilación o pensión de vejez
            pension_menor_salario_minimo = sum(y2803 <= 500000, na.rm = T), #jubilación o pensión de vejez
            .groups = "drop")


##variables por hogar ----
casen2022_regiones_hogares <- casen2022_regiones %>%
  filter(pco1 == "1. Jefatura de Hogar") |> 
  group_by(region, sexo) %>%
  summarize(hogares = n(),
            hacinamiento =  sum(p9 >= 4, na.rm=TRUE),
            men18c = sum(men18c == "Sí", na.rm=TRUE),
            rural = sum(area == "Rural", na.rm=TRUE),
            may60c = sum(may60c == "Sí", na.rm=TRUE),
            vivienda_propia = sum(v13 == "1. Propia", na.rm=TRUE),
            vivienda_arrendada = sum(v13 == "2. Arrendada", na.rm=TRUE),
            vivienda_pequeña = sum(v12 == "1. Menos de 30 m2", na.rm=TRUE),
            sector_malo = sum(p2 == "4. Malo" | p2 == "5. Muy malo", na.rm = T),
            sector_dañado = sum(p4 == "1. Mucho (observa 5 o más áreas con grafitis o daño deliberado)" | p4 == "2. Más o menos (observa 3 o 4 áreas con grafitis o daño deliberado)", na.rm=T),
            ingreso_percapita_hogar_menor_mediana = sum(ypc <= 450000, na.rm = T),
            ingreso_total_hogar_menor_mediana = sum(ytotcorh <= 1100000, na.rm = T),
            ingreso_trabajo_hogar_menor_mediana = sum(ytrabajocorh <= 700000, na.rm = T),
            carencia_estado_vivienda = sum(hh_d_estado == "Carente", na.rm = T),
            carencia_servicios_basicos = sum(hh_d_servbas == "Carente", na.rm = T),
            .groups = "drop"
  )


#agregar variables hechas por separado
casen2022_regiones_4 <- casen2022_regiones_personas |> 
  # left_join(casen2022_numericos_hogar) |> 
  # left_join(casen2022_numericos_personas) |> 
  left_join(casen2022_regiones_hogares) |> 
  left_join(jefatura_femenina)


# porcentajes ----
casen2022_regiones_5 <- casen2022_regiones_4 |> 
  group_by(region, sexo) |> 
  #porcentaje en relación a población
  # mutate(across(c(pobreza, originario, extranjero, inactivos, desocupados, pobreza_multi, fonasa, isapre, estudios_superiores),
  mutate(across(any_of(casen2022_regiones_personas |> select(where(is.numeric)) |> names()),
                ~.x/poblacion, .names = "{.col}_p")) |> 
  #porcentaje en relación a viviendas
  # mutate(across(c(men18c, may60c, vivienda_propia, vivienda_pequeña, hacinamiento, jefe_de_hogar),
  mutate(across(any_of(casen2022_regiones_hogares |> select(where(is.numeric)) |> names()),
                ~.x/hogares, .names = "{.col}_p"))

# casen2022_regiones_5 |> View()
# casen2022_regiones_5 |> names() |> cat(sep = "\n")

# casen2022_regiones |> 
#   count(sexo)
# casen2022_regiones |> 
#   count(v12mt)
# casen2022_regiones |> count(v12mt)

glimpse(casen2022_regiones_5)

#revisar variables ----
variables_casen
# "Jubilación o pensión de vejez (promedio)" = "y28_2c",
# "Número de hijos vivos" = "s4",

casen2022_regiones_5 |> count(v12mt)
# casen2022_regiones_5 |> count(area)
casen2022_regiones_5 |> count(hacinamiento)
casen2022_regiones_5 |> count(y2803)


#probar todas las variables
# walk(unlist(variables_casen), ~{
#   if (.x %in% variables_casen[1:10]) return()
#   message("probando ", .x)
#   .variable <- unname(.x)
#   
#   conteo <- casen2022_regiones_5 |> count(!!sym(.variable))
#   message(nrow(conteo))
# })


# nombre_variable("hacinamiento")

casen2022_regiones_6 <- casen2022_regiones_5 |> 
  select(region, sexo, ends_with("_p"),
         -poblacion_p,
         -hogares_p)

#guardar datos preparados para su uso en la app
readr::write_csv2(casen2022_regiones_6, "app/casen_genero_regiones.csv")
