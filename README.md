# Pirámides poblacionales · Hidalgo 2026

> Código en R para generar **pirámides poblacionales por municipio** del estado de Hidalgo a partir de los microdatos de la Encuesta Intercensal (EIC 2026) y de los datos agregados de INEGI. La metodología es extrapolable a cualquier otro estado cargando los insumos correspondientes.

![R](https://img.shields.io/badge/R-%3E%3D%204.2-276DC3?logo=r&logoColor=white)
![ggplot2](https://img.shields.io/badge/ggplot2-3.4+-1f4257?logo=tidyverse&logoColor=white)
![License](https://img.shields.io/badge/license-MIT-green)
![Status](https://img.shields.io/badge/status-en%20desarrollo-yellow)

---

## 📑 Tabla de contenidos

1. [Descripción](#-descripción)
2. [Requisitos](#-requisitos)
3. [Estructura del proyecto](#-estructura-del-proyecto)
4. [Flujo de trabajo](#-flujo-de-trabajo)
5. [Uso rápido](#-uso-rápido)
6. [Roadmap](#-roadmap)
7. [Notas metodológicas](#-notas-metodológicas)
8. [Autor y licencia](#-autor-y-licencia)

---

## 🧭 Descripción

Este repositorio contiene el código y los insumos necesarios para **construir, graficar y exportar** pirámides poblacionales municipales usando `ggplot2`, con las siguientes características:

- Pirámide clásica **hombres | mujeres** con porcentajes por grupo quinquenal.
- **Gradiente de color** proporcional al peso de cada grupo de edad dentro de su sexo.
- **Etiquetas combinadas** (`absolutos — %`) sobre cada barra.
- Exportación a **PNG con fondo transparente**, listo para presentaciones.
- Procesamiento **esquemático** de todos los municipios en un solo bucle.

---

## ⚙️ Requisitos

- **R ≥ 4.2** (probado con 4.3)
- **RStudio** (recomendado; el flujo está pensado para trabajar en él)
- Paquetes:

```r
install.packages(c(
  "dplyr", "tidyr", "ggplot2", "stringr",
  "purrr", "ggnewscale", "scales"
))
```

---

## 📂 Estructura del proyecto

```
poblacion_2026_EIC/
├── datos/
│   ├── eic_micro_municipio.csv        # microdatos EIC 2026
│   └── inegi_grupos_quinquenales.csv  # datos agregados INEGI
├── R/
│   ├── piramides_mun_inegi.R          # v1: datos agregados INEGI
│   ├── construccion_pob_85.R          # reconstrucción desde microdatos
│   └── piramides_mun_micro.R          # v2 (principal): desde microdatos
├── salida/                            # PNGs generados (no versionado)
├── README.md
└── LICENSE
```

---

## 🔄 Flujo de trabajo

El proyecto evolucionó en tres etapas. Cada script es **autocontenido** y puede ejecutarse por separado.

### 1. `piramides_mun_inegi.R` — versión inicial
Procesamiento incremental en 4 pasos:

1. **Carga y filtrado** de datos de INEGI.
2. **Pirámide sencilla** con población absoluta.
3. **Pirámide con gradiente** proporcional al peso del grupo quinquenal.
4. **Esquematización** para procesar los 84 municipios de Hidalgo.

> ⚠️ **Limitación**: los datos agregados de INEGI 2026 agrupan la población a partir de **75 años y más**, por lo que la pirámide se detiene ahí. El código funciona correctamente si ese agrupamiento es aceptable para tu análisis.

### 2. `construccion_pob_85.R` — reconstrucción desde microdatos
Recalcula la población por grupo quinquenal **hasta 85 años y más** a partir de los microdatos, y **valida** que la reconstrucción coincida con los datos agregados de INEGI.

### 3. `piramides_mun_micro.R` — versión principal ⭐
Toma los microdatos ya procesados y genera las pirámides definitivas (hasta 85+) para cada municipio, con exportación transparente a `salida/`.

---

## 🚀 Uso rápido

```r
# 1. Cargar microdatos y etiquetas
micro_eic2025 <- read.csv("datos/eic_micro_municipio.csv")[, -1]

etiquetas_piram <- c(
  "00-04 años", "05-09 años", "10-14 años", "15-19 años",
  "20-24 años", "25-29 años", "30-34 años", "35-39 años",
  "40-44 años", "45-49 años", "50-54 años", "55-59 años",
  "60-64 años", "65-69 años", "70-74 años", "75-79 años",
  "80-85 años", "85 años y más"
)

# 2. Construir el data frame por municipio
municipios_micro <- micro_eic2025 |>
  dplyr::filter(EDAD != 999) |>
  dplyr::mutate(
    sexo     = dplyr::case_when(SEXO == 1 ~ "hombres", SEXO == 3 ~ "mujeres"),
    grup_quin = cut(EDAD,
                    breaks = c(seq(0, 85, 5), Inf),
                    right  = FALSE,
                    labels = etiquetas_piram)
  ) |>
  dplyr::group_by(CVE_MUN, sexo, grup_quin, .drop = FALSE) |>
  dplyr::summarise(abs = sum(FACTOR, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(grup_quin = as.character(grup_quin)) |>
  split(~ CVE_MUN)

# 3. Ejecutar el pipeline (ver R/piramides_mun_micro.R)
source("R/piramides_mun_micro.R")
```

Los PNG se guardan en `salida/` con el formato:

```
salida/
├── 001_piramide.png
├── 002_piramide.png
└── ...
```

---

## 🗺️ Roadmap

- [x] Pirámide básica con `ggplot2`
- [x] Gradiente proporcional al peso del grupo
- [x] Etiquetas `absolutos — %`
- [x] Exportación PNG transparente
- [x] Procesamiento por lotes de municipios
- [x] Extensión hasta 85 años y más vía microdatos
- [ ] Versión parametrizable por estado (`--estado`)
- [ ] Reporte HTML con `Rmarkdown` / `Quarto`
- [ ] Tests con `testthat` para validar sumas contra INEGI

---

## 📝 Notas metodológicas

- **Factor de expansión**: se usa `FACTOR` para ponderar cada registro de los microdatos.
- **Edad desconocida**: se descartan los registros con `EDAD == 999`.
- **Cálculo de porcentajes**: se hace **dentro de cada sexo**, de modo que hombres y mujeres suman 100 % por separado. Esto permite comparar la estructura por edades **entre sexos** sin que el tamaño absoluto de cada grupo domine la lectura.
- **Validación**: la reconstrucción desde microdatos se contrasta contra los datos agregados de INEGI para garantizar consistencia.
- **Transparencia**: todos los gráficos se exportan con `bg = "transparent"` y con `panel.background`, `plot.background` y leyendas sin relleno.

---

## 👤 Autor y licencia

Proyecto desarrollado por **[Tu nombre]** · [tu@correo]

Distribuido bajo licencia **MIT**. Consulta el archivo [LICENSE](LICENSE) para más detalles.

---

<p align="center">
  <em>Hecho con ❤️ y R · Hidalgo, 2026</em>
</p>
