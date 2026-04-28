# ==============================================================================
# ANÁLISIS DE INGRESOS Y DESIGUALDAD: ENIGH 2024
# Autor: Santiago Francisco Robles Tamayo
# ==============================================================================

# 1. Paquetería ----
library(skimr)
library(tidyverse)
library(haven) 
library(srvyr) 
library(survey)
library(dplyr)
library(convey)
library(ineq)
library(ggplot2)
library(writexl)
library(data.table)
library(scales) 

# Crear carpetas de salida si no existen
dir.create("tablas/2024", recursive = TRUE, showWarnings = FALSE)
dir.create("graficos", showWarnings = FALSE)

# 2. Cargar bases de datos: 2024 ----
concentrado_24 <- read_dta("Bases/ENIGH_24/concentradohogar.dta")
poblacion_24 <- read_dta("Bases/ENIGH_24/poblacion.dta")

# 3. Definición de variables ----

## Población objetivo: excluye huéspedes y trabajadores domésticos
poblacion1 <- poblacion_24 %>%
  filter(!(parentesco >= 400 & parentesco < 500 | parentesco >= 700 & parentesco < 800))

# Escala de equivalencia
poblacion1 <- as.data.table(poblacion1)[, ind := 1][, tot_ind := sum(ind, na.rm = T), by = .(folioviv, foliohog)]

poblacion1 <- poblacion1 %>%
  mutate(tamhogesc = case_when(
    tot_ind == 1 ~ 1,
    edad <= 5 ~ 0.7031,
    edad >= 6 & edad <= 12 ~ 0.7382,
    edad >= 13 & edad <= 18 ~ 0.7057,
    edad >= 19 & !is.na(edad) ~ 0.9945
  )) %>%
  select(folioviv, foliohog, numren, tamhogesc)

poblacion_agg <- as.data.table(poblacion1)[, list(tamhogesc = sum(tamhogesc, na.rm = T)), by = .(folioviv, foliohog)]

## Unión de bases
conc_pob_24 <- concentrado_24 %>%
  left_join(poblacion_agg, by = c("folioviv", "foliohog"))

# Evitar duplicidad de variables al unir con población completa
var_rep <- intersect(names(conc_pob_24), names(poblacion_24))
var_off <- setdiff(var_rep, c("folioviv", "foliohog"))

conc_pob_24 <- conc_pob_24 %>%
  left_join(poblacion_24 %>% select(-all_of(var_off)), by = c("folioviv", "foliohog"))

## Variables geográficas
conc_pob_24 <- conc_pob_24 %>%
  mutate(
    folioviv = str_pad(folioviv, 10, "left", pad = "0"),
    ent = as.numeric(str_sub(folioviv, 1, 2)),
    entidad = case_when(
      ent == 1 ~ "Aguascalientes", ent == 2 ~ "Baja California", ent == 3 ~ "Baja California Sur",
      ent == 4 ~ "Campeche", ent == 5 ~ "Coahuila", ent == 6 ~ "Colima", ent == 7 ~ "Chiapas",
      ent == 8 ~ "Chihuahua", ent == 9 ~ "Ciudad de México", ent == 10 ~ "Durango",
      ent == 11 ~ "Guanajuato", ent == 12 ~ "Guerrero", ent == 13 ~ "Hidalgo", ent == 14 ~ "Jalisco",
      ent == 15 ~ "EdoMex", ent == 16 ~ "Michoacán", ent == 17 ~ "Morelos", ent == 18 ~ "Nayarit",
      ent == 19 ~ "Nuevo León", ent == 20 ~ "Oaxaca", ent == 21 ~ "Puebla", ent == 22 ~ "Querétaro",
      ent == 23 ~ "Quintana Roo", ent == 24 ~ "San Luis Potosí", ent == 25 ~ "Sinaloa",
      ent == 26 ~ "Sonora", ent == 27 ~ "Tabasco", ent == 28 ~ "Tamaulipas", ent == 29 ~ "Tlaxcala",
      ent == 30 ~ "Veracruz", ent == 31 ~ "Yucatán", ent == 32 ~ "Zacatecas"
    ),
    region_e = case_when(
      ent %in% c(2, 26, 8, 28, 5, 19) ~ "N",
      ent %in% c(3, 25, 18, 10, 32) ~ "NO",
      ent %in% c(14, 1, 6, 16, 24) ~ "CN",
      ent %in% c(11, 22, 13, 15, 17, 29, 21, 9) ~ "C",
      ent %in% c(12, 20, 7, 30, 27, 4, 31, 23) ~ "S"
    )
  )

## Deflactor 2024 (Base 2024 = 1)
inpc_24 <- 1 

conc_pob_24 <- conc_pob_24 %>%
  mutate(
    ing_total = ing_cor / inpc_24,
    ing_trab = ingtrab / inpc_24,
    ing_totalm = ing_total / 3,
    ing_trabm = ing_trab / 3,
    # Variables sociodemográficas
    sexo1 = ifelse(sexo == 1, "Hombre", "Mujer"),
    hli = ifelse(hablaind == 1, 1, 0),
    rural = ifelse(tam_loc == 4, 1, 0),
    anesc_educ = as.numeric(case_when(
      educa_jefe %in% c("01", "02") ~ 0, educa_jefe == "03" ~ 3,
      educa_jefe == "04" ~ 6, educa_jefe == "05" ~ 7.5,
      educa_jefe == "06" ~ 9, educa_jefe == "07" ~ 10.5,
      educa_jefe == "08" ~ 12, educa_jefe == "09" ~ 14.5,
      educa_jefe == "10" ~ 16.5, educa_jefe == "11" ~ 19
    ))
  )

educ_prom <- weighted.mean(conc_pob_24$anesc_educ, w = conc_pob_24$factor, na.rm = TRUE)

conc_pob_24 <- conc_pob_24 %>%
  mutate(
    ge = case_when(
      edad <= 5 ~ 1, edad >= 6 & edad <= 11 ~ 2,
      edad >= 12 & edad <= 29 ~ 3, edad >= 30 & edad <= 64 ~ 4,
      edad >= 65 ~ 5
    ),
    id_ge = c("Primera infancia", "6 a 11 años", "Jóvenes", "30 a 64 años", "Población adulta mayor")[ge],
    tipos = case_when(
      (sexo == 2 & hli == 1 & region_e == "S" & anesc_educ < educ_prom & rural == 1) ~ 1,
      (sexo == 1 & hli == 0 & region_e == "N" & anesc_educ > educ_prom & rural == 0) ~ 2,
      (sexo == 2 & hli == 0 & region_e == "N" & anesc_educ > educ_prom & rural == 0) ~ 3,
      (sexo == 1 & hli == 1 & region_e == "S" & anesc_educ < educ_prom & rural == 1) ~ 4,
      TRUE ~ 5
    )
  )

# 4. Deciles ----
generar_deciles <- function(df, var_ingreso, nombre_decil) {
  df <- df[order(df[[var_ingreso]]), ]
  df$ACUMULA <- cumsum(df$factor)
  tot_hog <- sum(df$factor)
  tam_dec <- trunc(tot_hog / 10)
  
  df[[nombre_decil]] <- findInterval(df$ACUMULA, seq(tam_dec, tam_dec * 9, by = tam_dec)) + 1
  df[[nombre_decil]][df[[nombre_decil]] > 10] <- 10
  return(df)
}

conc_pob_24 <- generar_deciles(conc_pob_24, "ing_total", "decil")
conc_pob_24 <- generar_deciles(conc_pob_24, "ing_trab", "decil_l")

# 5. Diseño Muestral y Tablas ----
dm <- conc_pob_24 %>%
  as_survey_design(ids = upm, strata = est_dis, weights = factor)

grupos <- c("decil", "decil_l", "region_e", "hli", "rural", "sexo1", "tipos")

for (grupo in grupos) {
  res <- dm %>%
    group_by(!!sym(grupo)) %>%
    summarize(
      ing_totalp = survey_mean(ing_total, vartype = c("se"), level = 0.95),
      ing_trabp = survey_mean(ing_trab, vartype = c("se"), level = 0.95)
    ) %>%
    mutate(LCI = ing_totalp - (1.96 * ing_totalp_se),
           LCS = ing_totalp + (1.96 * ing_totalp_se))
  
  assign(paste0("ing_24_", grupo), res)
}

# GINI 2024
grupos_gini <- c("id_ge", "region_e", "rural", "tipos")
for (g in grupos_gini) {
  res_total <- svyby(~ing_total, as.formula(paste0("~", g)), dm, svygini, na.rm = TRUE)
  res_trab <- svyby(~ing_trab, as.formula(paste0("~", g)), dm, svygini, na.rm = TRUE)
  
  final_gini <- res_total %>%
    rename(gini_total = ing_total, se_total = se) %>%
    left_join(res_trab %>% rename(gini_trab = ing_trab, se_trab = se), by = g)
  
  write_xlsx(final_gini, path = paste0("tablas/2024/gini_", g, "_2024.xlsx"))
}

# 4. Gráficos 2024 (Formato corregido) ----
library(ggplot2)
library(scales)

# Cálculo de promedios nacionales para las líneas de referencia
ing_total_nacional_24 <- weighted.mean(conc_pob_24$ing_total, w = conc_pob_24$factor, na.rm = TRUE)
ing_trab_nacional_24 <- weighted.mean(conc_pob_24$ing_trab, w = conc_pob_24$factor, na.rm = TRUE)

## Figura 1: Ingreso total mensual promedio por región 2024 ----
G1_ing_total_region_24 <- ggplot(ing_24_region_e, aes(x = reorder(region_e, ing_totalp), y = ing_totalp)) +
  # Líneas de error
  geom_errorbar(aes(ymin = LCI, ymax = LCS), width = 0.3) +
  # Puntos centrales
  geom_point(color = "firebrick", size = 2.5) +
  # ETIQUETAS DE DATOS
  geom_text(aes(label = dollar(ing_totalp * (1/3) / 1000, accuracy = 0.1)), 
            vjust = -0.2,
            hjust = -0.4,
            fontface = "bold", 
            size = 4.5) +
  # Línea de ingreso medio nacional (calculado con datos 2024)
  geom_hline(aes(yintercept = ing_total_nacional_24, color = "Ingreso promedio nacional"), 
             linetype = "dashed", linewidth = 0.8) +
  # Personalización de la leyenda
  scale_color_manual(name = NULL, values = c("Ingreso promedio nacional" = "black")) +
  # Eje Y: Mensual, en miles y con "$"
  scale_y_continuous(labels = label_number(scale = (1/3)/1000, prefix = "$"),
                     expand = expansion(mult = c(0.05, 0.15))) + 
  # Nombres de regiones
  scale_x_discrete(labels = c("S" = "Sur", 
                              "C" = "Centro", 
                              "NO" = "Norte-Oeste", 
                              "CN" = "Centro-Norte", 
                              "N" = "Norte")) +
  # Estética y Tamaños
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 0, face = "bold", color = "black", size = 12), 
    axis.text.y = element_text(face = "bold", color = "black", size = 12),
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(face = "bold", size = 17),
    axis.title.x = element_text(face = "bold", size = 17, margin = margin(t = 15)),
    axis.title.y = element_text(face = "bold", size = 17, margin = margin(r = 15)),
    plot.caption = element_text(face = "bold", size = 12, hjust = 1, margin = margin(t = 15)),
    legend.position = c(0.85, 0.15), 
    legend.background = element_rect(fill = "white", color = "lightgrey"),
    legend.text = element_text(size = 11)
  ) +
  labs(
    title = "Figura 1: ingreso total mensual promedio por región\nen México, 2024",
    subtitle = "Precios corrientes de 2024",
    x = "Región",
    y = "Ingreso mensual (miles de pesos)",
    caption = "Fuente: elaboración propia con datos de la ENIGH 2024 de INEGI"
  )

ggsave(filename = "graficos/G1_ing_total_region_2024.png", 
       plot = G1_ing_total_region_24, width = 10, height = 8, dpi = 300, bg = "white")


## Figura 2: Ingreso laboral mensual promedio por región 2024 ----
G2_ing_trab_region_24 <- ggplot(ing_24_region_e, aes(x = reorder(region_e, ing_trabp), y = ing_trabp)) +
  # Líneas de error
  geom_errorbar(aes(ymin = ing_trabp - (1.96 * ing_totalp_se), ymax = ing_trabp + (1.96 * ing_totalp_se)), width = 0.3) +
  # Puntos centrales
  geom_point(color = "firebrick", size = 2.5) +
  # ETIQUETAS DE DATOS
  geom_text(aes(label = dollar(ing_trabp * (1/3) / 1000, accuracy = 0.1)), 
            vjust = -0.1,
            hjust = -0.4,
            fontface = "bold", 
            size = 4.5) +
  # Línea de ingreso medio laboral nacional (calculado con datos 2024)
  geom_hline(aes(yintercept = ing_trab_nacional_24, color = "Ingreso promedio laboral nacional"), 
             linetype = "dashed", linewidth = 0.8) +
  # Personalización de la leyenda
  scale_color_manual(name = NULL, values = c("Ingreso promedio laboral nacional" = "black")) +
  # Eje Y: Mensual, en miles y con "$"
  scale_y_continuous(labels = label_number(scale = (1/3)/1000, prefix = "$"),
                     expand = expansion(mult = c(0.05, 0.15))) + 
  # Nombres de regiones
  scale_x_discrete(labels = c("S" = "Sur", 
                              "C" = "Centro", 
                              "NO" = "Norte-Oeste", 
                              "CN" = "Centro-Norte", 
                              "N" = "Norte")) +
  # Estética y Tamaños
  theme_minimal() +
  theme(
    axis.line.x = element_line(color = "black", linewidth = 0.5),
    axis.line.y = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 0, face = "bold", color = "black", size = 12), 
    axis.text.y = element_text(face = "bold", color = "black", size = 12),
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(face = "bold", size = 17),
    axis.title.x = element_text(face = "bold", size = 17, margin = margin(t = 15)),
    axis.title.y = element_text(face = "bold", size = 17, margin = margin(r = 15)),
    plot.caption = element_text(face = "bold", size = 12, hjust = 1, margin = margin(t = 15)),
    legend.position = c(0.85, 0.15), 
    legend.background = element_rect(fill = "white", color = "lightgrey"),
    legend.text = element_text(size = 11)
  ) +
  labs(
    title = "Figura 2: ingreso laboral mensual promedio por región\nen México, 2024",
    subtitle = "Precios corrientes de 2024",
    x = "Región",
    y = "Ingreso mensual (miles de pesos)",
    caption = "Fuente: elaboración propia con datos de la ENIGH 2024 de INEGI"
  )

ggsave(filename = "graficos/G2_ing_trab_region_2024.png", 
       plot = G2_ing_trab_region_24, width = 10, height = 8, dpi = 300, bg = "white")

# 7. Tablas de Deciles 2024 ----
tabla_regiones_24 <- dm %>%
  group_by(decil, region_e) %>%
  summarise(ing_totalp = survey_mean(ing_total, na.rm = TRUE))

write_xlsx(tabla_regiones_24, path = "tablas/2024/tabla_deciles_regiones_2024.xlsx")
