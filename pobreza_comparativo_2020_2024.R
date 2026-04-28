# ==============================================================================
# ANÁLISIS COMPARATIVO POBREZA MULTIDIMENSIONAL: ENIGH 2020 vs 2024
# Autor: Santiago Francisco Robles Tamayo
# ==============================================================================

library(tidyverse)
library(haven)
library(srvyr)
library(writexl)
library(stringr) # Para el ajuste de texto en renglones

# 1. PARÁMETROS GLOBALES Y ETIQUETAS ----
variables <- c("ic_rezedu", "ic_asalud", "ic_segsoc", "ic_cv", "ic_sbv", "ic_ali_nc")
etiquetas <- c(ic_rezedu = "Rezago educativo",
               ic_asalud = "Acceso a los servicios de salud",
               ic_segsoc = "Acceso a la seguridad social",
               ic_cv     = "Calidad y espacios de la vivienda",
               ic_sbv    = "Servicios básicos en la vivienda",
               ic_ali_nc = "Acceso a la alimentación nutritiva y de calidad")

# Definir un orden fijo para las carencias (del eje Y)
orden_carencias <- rev(etiquetas)

# Paleta de colores: 2020 (Azul), 2024 (Verde)
colores_personalizados <- c("2020" = "#1927B9", "2024" = "#009C3F")

# Función de formato común homogeneizado
tema_homogeneo <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(size = 14),
      # Eje X: Grande y negrita
      axis.title.x = element_text(face = "bold", size = 14),
      axis.text.x = element_text(face = "bold", size = 12, color = "black"),
      # Eje Y: Texto cortado en renglones, un poco más pequeño y sin título
      axis.title.y = element_blank(), 
      axis.text.y = element_text(face = "bold", size = 10, color = "black", lineheight = 0.9),
      legend.position = "bottom",
      legend.text = element_text(size = 12, face = "bold"),
      panel.grid.minor = element_blank()
    )
}

# 2. PROCESAMIENTO ENIGH 2020 ----
pobreza_20 <- read_dta("Bases/ENIGH_20/pobreza_20.dta")

dm_svy_20 <- pobreza_20 %>%
  filter(!is.na(pobreza)) %>%
  as_survey_design(ids = upm, strata = est_dis, weights = factor) %>%
  mutate(
    ent = as.numeric(str_sub(str_pad(folioviv, 10, "left", "0"), 1, 2)),
    region_e = case_when(
      ent %in% c(2, 26, 8, 28, 5, 19) ~ "N",
      ent %in% c(3, 25, 18, 10, 32) ~ "NO",
      ent %in% c(14, 1, 6, 16, 24)  ~ "CN",
      ent %in% c(11, 22, 13, 15, 17, 29, 21, 9) ~ "C",
      ent %in% c(12, 20, 7, 30, 27, 4, 31, 23)  ~ "S")
  )

resumen_nacional_20 <- map_dfr(variables, function(var) {
  dm_svy_20 %>%
    summarise(media = survey_mean(!!sym(var), na.rm = TRUE) * 100) %>%
    mutate(carencia = var, id_carencia = etiquetas[var], año = "2020")
})

# 3. PROCESAMIENTO ENIGH 2024 ----
pobreza_24 <- read_dta("Bases/ENIGH_24/pobreza_24.dta")

dm_svy_24 <- pobreza_24 %>%
  filter(!is.na(pobreza)) %>%
  as_survey_design(ids = upm, strata = est_dis, weights = factor) %>%
  mutate(
    ent = as.numeric(str_sub(str_pad(folioviv, 10, "left", "0"), 1, 2)),
    region_e = case_when(
      ent %in% c(2, 26, 8, 28, 5, 19) ~ "N",
      ent %in% c(3, 25, 18, 10, 32) ~ "NO",
      ent %in% c(14, 1, 6, 16, 24)  ~ "CN",
      ent %in% c(11, 22, 13, 15, 17, 29, 21, 9) ~ "C",
      ent %in% c(12, 20, 7, 30, 27, 4, 31, 23)  ~ "S")
  )

resumen_nacional_24 <- map_dfr(variables, function(var) {
  dm_svy_24 %>%
    summarise(media = survey_mean(!!sym(var), na.rm = TRUE) * 100) %>%
    mutate(carencia = var, id_carencia = etiquetas[var], año = "2024")
})

# 4. CONSOLIDACIÓN COMPARATIVA ----
comparativo_nacional <- bind_rows(resumen_nacional_20, resumen_nacional_24) %>%
  mutate(id_carencia = factor(id_carencia, levels = orden_carencias))

# 5. GUARDAR RESULTADOS ----
write_xlsx(list("Nacional_Comp" = comparativo_nacional), 
           path = "tablas/Comparativo_ENIGH_20_24.xlsx")

# 6. GRÁFICA COMPARATIVA NACIONAL (FIGURA 6) ----
grafica_comparativa <- ggplot(comparativo_nacional, 
                              aes(x = id_carencia, 
                                  y = media, 
                                  fill = fct_rev(as.factor(año)))) +
  geom_col(position = "dodge") +
  coord_flip() +
  # str_wrap(x, 25) corta el texto a los 25 caracteres para crear los dos renglones
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
  scale_fill_manual(values = colores_personalizados, name = "Año") +
  guides(fill = guide_legend(reverse = TRUE)) +
  tema_homogeneo() +
  labs(title = "Figura 8: Comparativo nacional de carencias sociales, 2020 - 2024",
       subtitle = "Porcentaje de la población nacional",
       y = "Porcentaje (%)",
       caption = "Fuente: elaboración propia con datos de ENIGH 2020 y 2024") +
  geom_text(aes(label = paste0(round(media, 1), "%")), 
            position = position_dodge(width = 0.9), hjust = -0.1, size = 4, fontface = "bold") +
  expand_limits(y = max(comparativo_nacional$media) + 10)

ggsave("graficos/comparativo_nacional_20_24.png", grafica_comparativa, 
       width = 15, height = 8, dpi = 300, bg = "white")


# 7. GRÁFICAS COMPARATIVAS POR REGIÓN (FIGURAS 7 EN ADELANTE) ----

regiones_lista <- c("N", "NO", "CN", "C", "S")
nombres_regiones <- c("N" = "Norte", "NO" = "Norte-Occidente", "CN" = "Centro-Norte", "C" = "Centro", "S" = "Sur")

n_figura <- 9

for (reg in regiones_lista) {
  
  res_reg_20 <- map_dfr(variables, function(var) {
    dm_svy_20 %>% filter(region_e == reg) %>%
      summarise(media = survey_mean(!!sym(var), na.rm = TRUE) * 100) %>%
      mutate(carencia = var, id_carencia = etiquetas[var], año = "2020")
  })
  
  res_reg_24 <- map_dfr(variables, function(var) {
    dm_svy_24 %>% filter(region_e == reg) %>%
      summarise(media = survey_mean(!!sym(var), na.rm = TRUE) * 100) %>%
      mutate(carencia = var, id_carencia = etiquetas[var], año = "2024")
  })
  
  comparativo_reg_data <- bind_rows(res_reg_20, res_reg_24) %>%
    mutate(id_carencia = factor(id_carencia, levels = orden_carencias))
  
  grafica_reg <- ggplot(comparativo_reg_data, 
                        aes(x = id_carencia, 
                            y = media, 
                            fill = fct_rev(as.factor(año)))) +
    geom_col(position = "dodge") +
    coord_flip() +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
    scale_fill_manual(values = colores_personalizados, name = "Año") +
    guides(fill = guide_legend(reverse = TRUE)) +
    tema_homogeneo() +
    labs(
      title = paste0("Figura ", n_figura, ": Comparativo de carencias sociales, región ", nombres_regiones[reg]),
      subtitle = "Porcentaje de la población nacional (2020 - 2024)",
      y = "Porcentaje (%)",
      caption = "Fuente: elaboración propia con datos de ENIGH 2020 y 2024"
    ) +
    geom_text(aes(label = paste0(round(media, 1), "%")), 
              position = position_dodge(width = 0.9), hjust = -0.1, size = 4, fontface = "bold") +
    expand_limits(y = max(comparativo_reg_data$media) + 12)
  
  ggsave(paste0("graficos/figura_", n_figura, "_region_", reg, ".png"), 
         grafica_reg, width = 12, height = 8, dpi = 300, bg = "white")
  
  n_figura <- n_figura + 1
}


# 8. ANÁLISIS ESPECÍFICO: ESTADO DE SONORA (FIGURA 9) ----

res_sonora_20 <- map_dfr(variables, function(var) {
  dm_svy_20 %>% 
    filter(ent == 26) %>% 
    summarise(media = survey_mean(!!sym(var), na.rm = TRUE) * 100) %>%
    mutate(carencia = var, id_carencia = etiquetas[var], año = "2020")
})

res_sonora_24 <- map_dfr(variables, function(var) {
  dm_svy_24 %>% 
    filter(ent == 26) %>% 
    summarise(media = survey_mean(!!sym(var), na.rm = TRUE) * 100) %>%
    mutate(carencia = var, id_carencia = etiquetas[var], año = "2024")
})

comparativo_sonora <- bind_rows(res_sonora_20, res_sonora_24) %>%
  mutate(id_carencia = factor(id_carencia, levels = orden_carencias))

grafica_sonora <- ggplot(comparativo_sonora, 
                         aes(x = id_carencia, 
                             y = media, 
                             fill = fct_rev(as.factor(año)))) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
  scale_fill_manual(values = colores_personalizados, name = "Año") +
  guides(fill = guide_legend(reverse = TRUE)) +
  tema_homogeneo() +
  labs(
    title = "Figura 9: Comparativo de Carencias Sociales en el Estado de Sonora",
    subtitle = "Evolución porcentual 2020 vs 2024",
    y = "Porcentaje (%)",
    caption = "Fuente: Elaboración propia con datos de ENIGH 2020 y 2024 de INEGI"
  ) +
  geom_text(aes(label = paste0(round(media, 1), "%")), 
            position = position_dodge(width = 0.9), 
            hjust = -0.1, 
            size = 4, 
            fontface = "bold") +
  expand_limits(y = max(comparativo_sonora$media) + 10)

ggsave("graficos/figura_9_sonora_20_24.png", 
       grafica_sonora, width = 12, height = 8, dpi = 300, bg = "white")

write_xlsx(list("Sonora_Comp" = comparativo_sonora), 
           path = "tablas/2024/Comparativo_Sonora_20_24.xlsx")