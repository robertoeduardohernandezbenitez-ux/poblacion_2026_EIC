# construccion de piramide hasta +85 años de edad -------------------------
library(dplyr)

etiquetas_piram <- c( #Etiquetas para cada grupo quinquenal hasta los 85 años
  "00-04 años", "05-09 años", "10-14 años", "15-19 años",
  "20-24 años", "25-29 años", "30-34 años", "35-39 años",
  "40-44 años", "45-49 años", "50-54 años", "55-59 años",
  "60-64 años", "65-69 años", "70-74 años", "75-79 años",
  "80-85 años", "85 años y más"
)

"dat/eic_micro_municipio.csv" |> read.csv() -> micro_eic2025
micro_eic2025 <- micro_eic2025[,-1] #Elimino columna con la clave del estado (13 = Hidalgo)
micro_eic2025<- micro_eic2025 |>  
  dplyr::mutate(SEXO = dplyr::case_when(
    SEXO == 1 ~ "hombres",
    SEXO == 3 ~ "mujeres")) |>#Se cambia la etiqueta numerica por la de genero
  dplyr::rename(sexo=SEXO)

micro_eic2025 |>
  dplyr::filter(EDAD != 999) |> #se eliminan datos no especificados 
  dplyr::mutate(grup_quin = cut(EDAD, breaks = c(seq(0, 85, 5),Inf), right = FALSE, labels = etiquetas_piram)) |>
 #Se crean los grupos quinquenales en intervalos [k:k+5) con el grupo de +85 absorviendo el resto del a poblacion
  dplyr::group_by(CVE_MUN, sexo, grup_quin, .drop = FALSE) |>
  dplyr::summarise(abs = sum(FACTOR, na.rm = TRUE), .groups = "drop") |> 
  dplyr::mutate(grup_quin = as.character(grup_quin)) -> municipios_micro

municipios_micro<-split(municipios_micro,municipios_micro$CVE_MUN)

# Graficacion de la piramide poblacional ----------------------------------
library(ggplot2)
library(tidyr)
library(stringr)
library(ggnewscale)

# 0. Configuración global
dir_salida <- "piramides_mun"
dir.create(dir_salida, showWarnings = FALSE)

niveles_edad <- etiquetas_piram
  
# 1. Función: preparar datos
preparar_piramide <- function(df_municipio, niveles) {
  df_municipio |> 
  group_by(sexo) |> 
    mutate(pct = abs / sum(abs) * 100) |> 
    ungroup() |> 
    mutate(
      edad = factor(grup_quin, levels = niveles, ordered = TRUE),
      poblacion_plot = ifelse(sexo == "hombres", -pct, pct),
      etiqueta       = sprintf("%s — %.1f%%",
                               format(abs, big.mark = ","), pct),
      sexo = factor(sexo, levels = c("hombres", "mujeres"))
    )
}
# 2. Función: construir el plot

graficar_piramide <- function(piramide) {
  max_pop <- max(piramide$pct)
  
  ggplot(piramide, aes(x = edad, y = poblacion_plot)) +
    geom_col(data = subset(piramide, sexo == "hombres"),
             aes(fill = pct), width = 0.9) +
    scale_fill_gradient(
      low = "#D3FEE9", high = "#646746", name = "Hombres (%)",
      guide = guide_colorbar(barwidth = 8, barheight = 0.6)
    ) +
    new_scale_fill() +
    geom_col(data = subset(piramide, sexo == "mujeres"),
             aes(fill = pct), width = 0.9) +
    scale_fill_gradient(
      low = "#FBD0DE", high = "#89283f", name = "Mujeres (%)",
      guide = guide_colorbar(barwidth = 8, barheight = 0.6)
    ) +
  geom_text(
    aes(label = etiqueta,
        hjust = ifelse(sexo == "hombres", 1.1, -0.1)),
    size = 3.5, color = "grey8"
  ) +
    
    coord_flip() +
    scale_y_continuous(
      labels = function(x) paste0(abs(x), "%"),
      breaks = scales::pretty_breaks(n = 8),
      limits = c(-max_pop * 1.45, max_pop * 1.45)
    ) +
    labs(x = "Grupos quinquenales de edad",
         y = "Porcentaje de la población municipal") +
    theme_classic(base_size = 12) +
    theme(
      legend.position    = "top",
      legend.box         = "horizontal",      
      legend.direction   = "horizontal",   
      legend.spacing.x   = unit(1.5, "cm"),    
      legend.title       = element_text(size = 10),
      panel.grid.major.y = element_blank(),
      panel.background   = element_rect(fill = "transparent", color = NA),
      plot.background    = element_rect(fill = "transparent", color = NA),
      legend.background  = element_rect(fill = "transparent", color = NA),
      legend.key         = element_rect(fill = "transparent", color = NA)
    )
}
#Grafica de prueba
municipios_micro[["1"]] |> 
  preparar_piramide(niveles_edad) |> 
  graficar_piramide() |> 
  ggsave(
    filename = file.path(dir_salida,
                         paste0("01", "_piramide.png")),
    width = 10, height = 7, dpi = 300, bg = "transparent"
  )

# Creación de las piramides por municipio mediante paralelizacion ---------
library(purrr)

walk(names(municipios_micro), function(nombre) {
  message("Procesando: ", nombre)
  
  nombre_limpio <- gsub('[/\\\\:**?"<>|]', "_", nombre)
  
  municipios_micro[[nombre]] |> 
    preparar_piramide(niveles_edad) |> 
    graficar_piramide() |> 
    ggsave(
      filename = file.path(dir_salida,
                           paste0(nombre_limpio, "_piramide.png")),
      width = 10, height = 7, dpi = 300, bg = "transparent"
    )
})
