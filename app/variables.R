variables <- list(
  "Viviendas" = c(
    #"Hogares con jefatura femenina" = "hogar_jefatura_femenina_p",
    "Hogares en situación de hacinamiento (4 personas o más)" = "hacinamiento_p",
    "Hogares con personas menores de 18 años" = "men18c_p",
    "Hogares con personas mayores de 60 años" = "may60c_p",
    "Viviendas propias" = "vivienda_propia_p",
    "Viviendas arrendadas" = "vivienda_arrendada_p",
    "Viviendas pequeñas (30m cuadrados o menos)" = "vivienda_pequeña_p",
    "Hogares en sectores en mal estado" = "sector_malo_p",
    "Hogares en sectores con mucho daño deliberado a la propiedad" = "sector_dañado_p",
    "Hogares en zonas rurales" = "rural_p",
    "Viviendas en mal estado" = "carencia_estado_vivienda_p",
    "Viviendas carentes de servicios básicos" = "carencia_servicios_basicos_p"
  ),
  
  "Ingresos de las personas" = c(
    "Ingresos personales por ocupación principal menores a la mediana ($500.000)" = "ingreso_ocup_princ_menor_mediana_p",
    "Ingresos personales por ocupación principal menores a $1.000.000" = "ingreso_ocup_princ_menor_2medianas_p",
    "Ingresos personales producto del trabajo menores a la mediana ($500.000)" = "ingreso_ytrabajocor_menor_mediana_p",
    "Ingresos personales producto del trabajo menores a $1.000.000" = "ingreso_ytrabajocor_menor_2medianas_p"
  ),
  
  "Ingresos de los hogares" = c(
    "Ingresos del hogar percapita menores a la mediana ($450.000)" = "ingreso_percapita_hogar_menor_mediana_p",
    "Ingresos del hogar totales menores a la mediana ($1.110.000)" = "ingreso_total_hogar_menor_mediana_p",
    "Ingresos del hogar producto del trabajo menores a la mediana ($700.000)" = "ingreso_trabajo_hogar_menor_mediana_p"
  ),
  
  "Trabajo" = c(
    "Trabajadores independientes" = "independientes_p",
    "Personas en situación laboral inactiva" = "inactivos_p",
    "Personas en situación laboral desocupada" = "desocupados_p",
    "Trabaja como familiar no remunerado" = "trabajo_familiar_no_remunerado_p",
    "Trabaja en servicio doméstico puertas adentro o afuera" = "trabajo_servicio_domestico_p",
    "Trabaja como empleador" = "trabajo_empleador_p",
    "Trabajadores del hogar o servicio doméstico" = "trabajo_domestico_p"
  ),
  
  "Pensiones" = c(
    "Jubilación o pensión menor o igual a la mediana ($230.000)" = "pension_menor_mediana_p",
    "Jubilación o pensión menor o igual al salario mínimo ($500.000)" = "pension_menor_salario_minimo_p"
  ),
  
  "Educación" = c(
    "Personas con estudios superiores (técnico o profesional)" = "estudios_superiores_p",
    "Nivel educacional máximo: básica o menor" = "estudios_basica_o_menos_p",
    "Nivel educacional máximo: media incompleta o menor" = "estudios_sin_media_o_menos_p",
    "Nivel educacional máximo: estudios superiores incompletos" = "estudios_superiores_incompletos_p"
  ),
  
  "Pobreza" = c(
    "Personas en situación de pobreza" = "pobreza_p",
    "Personas en situación de pobreza multidimensional" = "pobreza_multi_p"
  ),
  
  "Demografía" = c(
    "Personas pertenecientes a pueblos originarios" = "originario_p",
    "Personas de origen extranjero" = "extranjero_p"
  ),
  
  "Salud" = c(
    "Afiliados a previsión de salud Fonasa" = "fonasa_p",
    "Afiliados a previsión de salud Isapre" = "isapre_p"
  )
)
