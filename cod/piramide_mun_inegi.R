library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(zoo)
inegi<-readxl::read_xlsx("datos/eic2025_poblacion_13.xlsx",sheet = 3)
names(inegi) = paste(
  inegi[5,] |> as.vector() |>  
    zoo::na.locf(na.rm = T) |>  
    unlist() |>  
    as.character(), 
  inegi[6,]
) |> gsub(pattern = "NA", replacement = " ") |> 
  stringr::str_squish()
inegi<- inegi |>  dplyr::filter(!is.na(Municipio))
inegi<- inegi |> dplyr::slice(-1)
inegi<- inegi |> dplyr::filter(Estimador=="Valor")

# Resumen estadistico -----------------------------------------------------
inegi |>  dplyr::mutate(pob = as.numeric(`Población total en viviendas particulares habitadas`)) |> 
  group_by(`Grupos quinquenales de edad`) |>
  summarise(
    Min.      = min(pob, na.rm = TRUE),
    `1st Qu.` = quantile(pob, 0.25, na.rm = TRUE),
    Median    = median(pob, na.rm = TRUE),
    Mean      = mean(pob, na.rm = TRUE),
    `3rd Qu.` = quantile(pob, 0.75, na.rm = TRUE),
    Max.      = max(pob, na.rm = TRUE),
    .groups   = "drop",
    suma = sum(pob)
  ) -> resumen

municipios<-split(inegi,inegi$Municipio)

# Grafica de piramide sencilla --------------------------------------------

piramide <- municipios$`001 Acatlán` |> 
  select(edad = `Grupos quinquenales de edad`,mujeres = `Mujeres Absolutos`,hombres = `Hombres Absolutos`) |> 
  mutate(mujeres = as.numeric(mujeres),hombres = as.numeric(hombres)) |> 
  filter(!str_detect(edad, regex("Total", ignore_case = TRUE))) |> 
  mutate(hombres = hombres/sum(hombres)*100, mujeres = mujeres/sum(mujeres)*100) |> 
  mutate(edad = factor(edad,levels = unique(inegi$`Grupos quinquenales de edad`),ordered = TRUE)) |> 
  pivot_longer(cols = c(hombres,mujeres),names_to = "sexo",values_to = "poblacion") |> 
  mutate(poblacion_plot = ifelse(sexo == "hombres", -poblacion, poblacion),sexo = factor(sexo, levels = c("hombres", "mujeres")))

ggplot(piramide, aes(x = edad, y = poblacion_plot, fill = sexo)) +
  geom_col(width = 0.9) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) format(abs(x), big.mark = ","),
    breaks = scales::pretty_breaks(n = 8)
  ) +
  scale_fill_manual(
    values = c("hombres" = "#42AABA", "mujeres" = "#FF508A"),
    labels = c("Hombres", "Mujeres"),
    name   = NULL
  ) +
  labs(
    x        = "Grupos quinquenales de edad",
    y        = "Porcentaje de la población",
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position   = "top",
    panel.grid.major.y = element_blank(),
    plot.title        = element_text(face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(hjust = 0.5)
  )

# Grafica de piramide personalizada ---------------------------------------

library(ggnewscale) #Nos permite crear la escala de color para la grafica

piramide <- municipios$`001 Acatlán` |> 
  select(
    edad    = `Grupos quinquenales de edad`,
    mujeres = `Mujeres Absolutos`,
    hombres = `Hombres Absolutos`
  ) |> 
  mutate(
    mujeres = as.numeric(mujeres),
    hombres = as.numeric(hombres)
  ) |> 
  filter(!str_detect(edad, regex("Total|No especificado", ignore_case = TRUE))) |> 
  mutate(
    # ── Absolutos (los conservamos) ──
    mujeres_abs = mujeres,
    hombres_abs = hombres,
    # ── Porcentajes ──
    mujeres_pct = mujeres / sum(mujeres) * 100,
    hombres_pct = hombres / sum(hombres) * 100
  ) |> 
  mutate(
    edad = factor(
      edad,
      levels  = unique(inegi$`Grupos quinquenales de edad`),
      ordered = TRUE
    )
  ) |> 
  pivot_longer(
    cols      = c(hombres_abs, mujeres_abs, hombres_pct, mujeres_pct),
    names_to  = c("sexo", ".value"),
    names_pattern = "(hombres|mujeres)_(abs|pct)"
  ) |> 
  mutate(
    # Valor para graficar (porcentaje, negativo en hombres)
    poblacion_plot = ifelse(sexo == "hombres", -pct, pct),
    # Etiqueta combinada: "1,234 — 5.2%"
    etiqueta = sprintf("%s — %.1f%%", format(abs, big.mark = ","), pct),
    sexo = factor(sexo, levels = c("hombres", "mujeres"))
  )

max_pop <- max(piramide$pct)

ggplot(piramide, aes(x = edad, y = poblacion_plot)) +
  
  # Hombres
  geom_col(
    data = subset(piramide, sexo == "hombres"),
    aes(fill = pct), width = 0.9
  ) +
  scale_fill_gradient(
    low = "#D6EEF2", high = "#1B7A8C", name = "Hombres (%)",
    guide = guide_colorbar(barwidth = 8, barheight = 0.6)
  ) +
  new_scale_fill() +
  
  # Mujeres
  geom_col(
    data = subset(piramide, sexo == "mujeres"),
    aes(fill = pct), width = 0.9
  ) +
  scale_fill_gradient(
    low = "#FBD0DE", high = "#B01E4E", name = "Mujeres (%)",
    guide = guide_colorbar(barwidth = 8, barheight = 0.6)
  ) +
  
  # Etiqueta combinada: absoluto — porcentaje
  geom_text(
    aes(
      label = etiqueta,
      hjust = ifelse(sexo == "hombres", 1.1, -0.1)
    ),
    size = 2.8, 
    color = "grey20"
  ) +
  
  coord_flip() +
  scale_y_continuous(
    labels = function(x) paste0(abs(x), "%"),
    breaks = scales::pretty_breaks(n = 8),
    limits = c(-max_pop * 1.45, max_pop * 1.45)
  ) +
  labs(
    x = "Grupos quinquenales de edad",
    y = "Porcentaje de la población municipal"
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position    = "top",
    legend.title       = element_text(size = 10),
    panel.grid.major.y = element_blank()
  )

ggsave(
  filename = "piramidesxmunicipio/piramide_acaxochitlan.png",
  plot     = p,
  width    = 8,      # pulgadas
  height   = 6,
  units    = "in",
  dpi      = 300,    # calidad para presentación
  bg       = "transparent"
)

# grafica de todos los municipios -------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(ggnewscale)

# 0. Configuración global
dir_salida <- "piramidexmunicipio"
dir.create(dir_salida, showWarnings = FALSE)

niveles_edad <- unique(inegi$`Grupos quinquenales de edad`)

# 1. Función: preparar datos
preparar_piramide <- function(df_municipio, niveles) {
  df_municipio |> 
    select(
      edad    = `Grupos quinquenales de edad`,
      mujeres = `Mujeres Absolutos`,
      hombres = `Hombres Absolutos`
    ) |> 
    mutate(
      mujeres = as.numeric(mujeres),
      hombres = as.numeric(hombres)
    ) |> 
    filter(!str_detect(edad, regex("Total|No especificado", ignore_case = TRUE))) |> 
    mutate(
      mujeres_abs = mujeres,
      hombres_abs = hombres,
      mujeres_pct = mujeres / sum(mujeres) * 100,
      hombres_pct = hombres / sum(hombres) * 100
    ) |> 
    mutate(
      edad = factor(edad, levels = niveles, ordered = TRUE)
    ) |> 
    pivot_longer(
      cols          = c(hombres_abs, mujeres_abs, hombres_pct, mujeres_pct),
      names_to      = c("sexo", ".value"),
      names_pattern = "(hombres|mujeres)_(abs|pct)"
    ) |> 
    mutate(
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
      low = "#D6EEF2", high = "#1B7A8C", name = "Hombres (%)",
      guide = guide_colorbar(barwidth = 8, barheight = 0.6)
    ) +
    new_scale_fill() +
    geom_col(data = subset(piramide, sexo == "mujeres"),
             aes(fill = pct), width = 0.9) +
    scale_fill_gradient(
      low = "#FBD0DE", high = "#B01E4E", name = "Mujeres (%)",
      guide = guide_colorbar(barwidth = 8, barheight = 0.6)
    ) +
    geom_text(
      aes(label = etiqueta,
          hjust = ifelse(sexo == "hombres", 1.1, -0.1)),
      size = 2.8, color = "grey20"
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
      legend.title       = element_text(size = 10),
      panel.grid.major.y = element_blank(),
      panel.background   = element_rect(fill = "transparent", color = NA),
      plot.background    = element_rect(fill = "transparent", color = NA),
      legend.background  = element_rect(fill = "transparent", color = NA),
      legend.key         = element_rect(fill = "transparent", color = NA)
    )
}

# Creación de las piramides por municipio ---------------------------------
library(purrr)

walk(names(municipios_micro), function(nombre) {
  message("Procesando: ", nombre)
  
  nombre_limpio <- gsub('[/\\\\:**?"<>|]', "_", nombre)
  
  municipios[[nombre]] |> 
    preparar_piramide(niveles_edad) |> 
    graficar_piramide() |> 
    ggsave(
      filename = file.path(dir_salida,
                           paste0(nombre_limpio, "_piramide.png")),
      width = 10, height = 7, dpi = 300, bg = "transparent"
    )
})