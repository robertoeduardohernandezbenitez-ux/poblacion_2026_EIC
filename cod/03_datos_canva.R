library(openxlsx)

# 1. Definir la carpeta donde están los PNG
carpeta_imgs <- "piramides_mun"

# 2. Obtener la lista de todas las imágenes
archivos <- list.files(carpeta_imgs, pattern = "\\.png$", full.names = TRUE)

# 3. Extraer la clave numérica del nombre del archivo
# Extraemos solo los primeros dígitos antes del guion bajo
cves_numericas <- as.numeric(gsub("^([0-9]+).*", "\\1", basename(archivos)))

# 4. Reconstruir las claves como texto con 3 dígitos (ej. 1 -> "001")
cves_texto <- sprintf("%03d", cves_numericas)

# 5. Ordenar los archivos y las claves para que el Excel quede ordenado
orden <- order(cves_numericas)
archivos <- archivos[orden]
cves_texto <- cves_texto[orden]

# 6. Crear el data.frame base
datos_excel <- data.frame(
  CVE_MUN = cves_texto,
  ruta_imagen = archivos,
  stringsAsFactors = FALSE
)

# 7. Construcción del Excel -----------------------------------------------

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "Piramides")

# Escribir la columna de CVE_MUN
openxlsx::writeData(wb, "Piramides", datos_excel[, "CVE_MUN", drop = FALSE], startCol = 1)

# Escribir el encabezado de la columna de imágenes
openxlsx::writeData(wb, "Piramides", "Piramides", startCol = 2, startRow = 1)

# IMPORTANTE: Dar formato de "Texto" a la columna A para que Excel no quite los ceros
estilo_texto <- openxlsx::createStyle(numFmt = "@")
openxlsx::addStyle(wb, sheet = "Piramides", style = estilo_texto,
                   rows = 2:(nrow(datos_excel) + 1), cols = 1)

# Ajustar el tamaño de las columnas y filas
openxlsx::setColWidths(wb, "Piramides", cols = 1, widths = 15)
openxlsx::setColWidths(wb, "Piramides", cols = 2, widths = 35) # Columna ancha para la imagen
openxlsx::setRowHeights(wb, "Piramides", rows = 2:(nrow(datos_excel) + 1), heights = 200)

# 8. Insertar las imágenes en un bucle
for (i in seq_len(nrow(datos_excel))) {
  
  openxlsx::insertImage(
    wb = wb,
    sheet = "Piramides",
    file = datos_excel$ruta_imagen[i],
    startRow = i + 1,       # +1 por el encabezado
    startCol = 2,           # Columna B
    width = 3,              
    height = 2.5,           
    units = "in",           
    dpi = 300               
  )
  
  if (i %% 10 == 0) message("Insertadas ", i, " imágenes de ", nrow(datos_excel))
}

# 9. Guardar el archivo final
openxlsx::saveWorkbook(wb, "piramides_canva.xlsx", overwrite = TRUE)