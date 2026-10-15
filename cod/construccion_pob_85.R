library(dplyr)
"datos/eic_micro_municipio.csv" |> read.csv() -> micro_eic2025

eic2025<- eic2025 |> dplyr::filter(CVE_ENT == 13, ESTIMADOR == "Valor")
names(eic2025)

etiquetas_inegi <- c(
  "00-04 años", "05-09 años", "10-14 años", "15-19 años",
  "20-24 años", "25-29 años", "30-34 años", "35-39 años",
  "40-44 años", "45-49 años", "50-54 años", "55-59 años",
  "60-64 años", "65-69 años", "70-74 años", "75 años y más"
)

micro_eic2025 <- micro_eic2025[,-1]
micro_eic2025<- micro_eic2025 |>  dplyr::mutate(SEXO = dplyr::case_when(
  SEXO == 1 ~ "hombres",
  SEXO == 3 ~ "mujeres")) 


pob_municipio <- micro_eic2025 |>
  dplyr::mutate(grup_quin = cut(EDAD, breaks = c(seq(0, 75, 5), Inf), right = FALSE, labels =etiquetas_inegi)) |>
  dplyr::group_by(CVE_MUN, SEXO, grup_quin, .drop = FALSE) |>
  dplyr::summarise(pob = sum(FACTOR, na.rm = TRUE), .groups = "drop")

pob_municipio<-pob_municipio |> mutate(poblacion_plot = ifelse(SEXO == "hombres", -pob, pob))
pob_municipio<-pob_municipio |> split(pob_municipio$CVE_MUN)

# comparacion datos inegi vs micro ----------------------------------------

#municipios de inegi en formato vertical
inegi |>   select(Municipio ,edad = `Grupos quinquenales de edad`,mujeres = `Mujeres Absolutos`,hombres = `Hombres Absolutos`) |> 
  mutate(edad = factor(edad,levels = unique(inegi$`Grupos quinquenales de edad`),ordered = TRUE)) |>
  filter(!str_detect(edad,regex("Total|No especificado", ignore_case = TRUE))) |> 
  filter(!str_detect(Municipio,regex("Total", ignore_case = TRUE))) |> 
  mutate(Municipio = substr(Municipio, start = 1, stop = 3)) |> 
  mutate(Municipio = as.integer(Municipio)) |>
  pivot_longer(cols = c(hombres,mujeres),names_to = "sexo",values_to = "poblacion") |> 
  mutate(poblacion = as.integer(poblacion)) |> 
  mutate(edad = as.character(edad))-> municipios_inegi
# municipios_micro 
micro_eic2025 |>
  filter(EDAD != 999) |> 
  dplyr::mutate(grup_quin = cut(EDAD, breaks = c(seq(0, 75, 5),Inf), right = FALSE, labels = etiquetas_inegi)) |>
  dplyr::group_by(CVE_MUN, SEXO, grup_quin, .drop = FALSE) |>
  dplyr::summarise(pob = sum(FACTOR, na.rm = TRUE), .groups = "drop") |> 
  dplyr::mutate(grup_quin = as.character(grup_quin))->municipios_micro
#left join para corroborar mismos datos
inegi_micro<-dplyr::left_join(municipios_inegi, municipios_micro,
                              by = c("Municipio" = "CVE_MUN", "edad" = "grup_quin","sexo" = "SEXO"))
inegi_micro<-inegi_micro |> mutate(diferencia = pob-poblacion)
#ya que la columna diferencia entre la poblacion de inegi y micro es identicamente cero los datos son iguales
