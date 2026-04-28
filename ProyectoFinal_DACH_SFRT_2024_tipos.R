
# 1. Paquetería ----
#install.packages("tidyverse")
#install.packages("haven")
#install.packages("srvyr")
#install.packages("kableExtra")

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
library(scales) # Necesaria para el formato de miles


# 2. Cargar bases de datos: 2020 y 2024 ----

## 2020 ----
concentrado_24 <- read_dta("Bases/ENIGH_24/concentradohogar.dta")
poblacion_24 <- read_dta("Bases/ENIGH_24/poblacion.dta")
# pobreza_20 <- read_dta("Bases/ENIGH_20/pobreza_20.dta")


# 3. Definición de variables ----

## Población objetivo: no se incluye a huéspedes ni trabajadores domésticos ----
poblacion1 <- filter(poblacion_24, !(parentesco>=400 & parentesco <500 | 
                                       parentesco>=700 & parentesco <800))

#Total de integrantes del hogar
poblacion1 <- as.data.table(poblacion1)[, c("ind"):=.(1)][, c("tot_ind"):=.(sum(ind, na.rm = T)), by=.(folioviv, foliohog)]

poblacion1 <-mutate(poblacion1,
                    tamhogesc=case_when(tot_ind==1 ~ 1,
                                        edad<=5 ~ .7031,
                                        edad>=6 & edad<=12 ~ .7382,
                                        edad>=13 & edad<=18 ~ .7057,
                                        edad>=19 & !is.na(edad) ~ .9945)) %>%
  select(folioviv, foliohog, numren, tamhogesc)

poblacion1 <- as.data.table(poblacion1)[,list(tamhogesc=sum(tamhogesc, na.rm = T)),
                                        by=.(folioviv, foliohog)]

## Uniendo con concetrado: ----
concentrado_24 <- left_join(concentrado_24, poblacion1, by = c("folioviv", "foliohog"))

## Identificación de variables comunes exceptuando las llaves de unión ----
var_rep <- intersect(names(concentrado_24), names(poblacion_24))
var_off <- setdiff(var_rep, c("folioviv", "foliohog"))

conc_pob_24 <- concentrado_24 %>%
  left_join(poblacion_24 %>% select(-all_of(var_off)), 
            by = c("folioviv", "foliohog"))

# var_rep <- intersect(names(conc_pob_20), names(pobreza_20))
# var_off <- setdiff(var_rep, c("folioviv", "foliohog"))
# 
# 
# conc_pob_20 <- conc_pob_20 %>%
#   left_join(pobreza_20 %>% select(-all_of(var_off)), 
#             by = c("folioviv", "foliohog"))


## Creando la variable para las entidades: ----
conc_pob_24 <- mutate(conc_pob_24,
                      folioviv=str_pad(folioviv, 10, "left", pad = "0"),
                      ent=as.numeric(str_sub(folioviv, 1,2)))

## Entidad federativa ----
conc_pob_24 <- conc_pob_24 %>% mutate(
  entidad= case_when(ent==1  ~ "Aguascalientes",
                     ent==2  ~ "Baja California",
                     ent==3  ~ "Baja California Sur",
                     ent==4  ~ "Campeche",
                     ent==5  ~ "Coahuila",
                     ent==6  ~ "Colima",
                     ent==7  ~ "Chiapas",
                     ent==8  ~ "Chihuahua",
                     ent==9  ~ "Ciudad de México",
                     ent==10 ~ "Durango",
                     ent==11 ~  "Guanajuato",
                     ent==12 ~ "Guerrero",
                     ent==13 ~ "Hidalgo",
                     ent==14 ~  "Jalisco",
                     ent==15 ~ "EdoMex",
                     ent==16 ~ "Michoacán",
                     ent==17 ~ "Morelos",
                     ent==18 ~ "Nayarit",
                     ent==19 ~ "Nuevo León",
                     ent==20 ~ "Oaxaca",
                     ent==21 ~  "Puebla",
                     ent==22 ~ "Querétaro",
                     ent==23 ~   "Quintana Roo",
                     ent==24 ~   "San Luis Potosí",
                     ent==25 ~   "Sinaloa",
                     ent==26 ~   "Sonora",
                     ent==27 ~   "Tabasco",
                     ent==28 ~   "Tamaulipas",
                     ent==29 ~   "Tlaxcala",
                     ent==30 ~   "Veracruz",
                     ent==31 ~   "Yucatán",
                     ent==32 ~   "Zacatecas"))

## Regionalización (Norte, Norte-Occidente, Centro-Norte, Centro, Sur) ----

conc_pob_24 <- conc_pob_24 %>% mutate(
  region= case_when(
    ent %in% c("2", "26","8", "28", "5", "19") ~ 1, #Norte"
    ent %in% c("3","25","18","10","32") ~ 2,#Norte-Occidente
    ent %in% c("14","1","6","16","24") ~ 3, #Centro-Norte
    ent %in% c("11","22","13","15","17","29","21","9") ~ 4, #Centro
    ent %in% c("12","20","7","30","27","4","31","23") ~ 5), # Sur
  
  #Generación de etiquetas        
  region_e= case_when(
    region == 1 ~ "N",
    region == 2 ~ "NO",
    region == 3 ~ "CN",
    region == 4 ~ "C",
    region == 5 ~ "S"))




## Deflactor para ingresos ----


#INPC

#2020
# inpc_20 = 0.79306390 
#2022
#inpc_22 = 0.910229169
#2024
inpc_24 = 1 

#Ingreso corriente deflactado (base agosto 2024= 100)
conc_pob_24 <- conc_pob_24 %>% mutate (
  ing_total= ing_cor/inpc_24, #ingreso total del hogar real
  ing_trab= ingtrab/inpc_24, #ingreso laboral real
  ing_totalpc= ing_total/tot_integ, #ingreso per capita
  ing_trabpc= ing_trab/tot_integ, #ingreso laboral per cápita
  ing_trabper= ing_trab/perc_ocupa, #ingreso laboral por individuo ocupado
  ing_totalpct= ing_total/tamhogesc, #ingreso total por tamaño ajustado
  
  #Variables mensuales
  
  ing_totalm= ing_total/ 3, #ingreso total del hogar real mensual
  ing_trabm=  ing_trab/ 3)#ingreso laboral del hogar real mensual



## Etiqueta por sexo del jefe de hogar (Hombrs y mujeres) ----

conc_pob_24 <- conc_pob_24 %>%
  mutate(sexo1 = ifelse(sexo== 1, "Hombre", "Mujer"), 
         
         #Etiqueta por origen indígena por adscripción de lengua
         
         hli= ifelse(hablaind== 1, 1, 0),
         
         #Etiqueta si el hogar recibe beneficios de gobierno
         
         benegob_d= case_when(
           bene_gob > 0 ~ 1, #recibe
           bene_gob == 0 ~ 0, #no recibe
           TRUE ~ NA_real_),
         #Etiqueta de localidad rural (localidad rural=1 , localidad urbana= 0)
         rural= case_when(
           tam_loc == 4 ~ 1,
           tam_loc %in% c(2,3,1) ~ 0,
           TRUE ~ NA_real_),
         #Etiquetas educativas 
         anesc_educ= as.numeric(case_when(
           educa_jefe %in% c("01", "02") ~ 0,
           educa_jefe== "03" ~ 3,
           educa_jefe== "04" ~ 6,
           educa_jefe== "05" ~ 7.5,
           educa_jefe== "06" ~ 9,
           educa_jefe== "07" ~ 10.5,
           educa_jefe== "08" ~ 12,
           educa_jefe== "09" ~ 14.5,
           educa_jefe== "10" ~ 16.5,
           educa_jefe== "11" ~ 19,
           TRUE ~ NA_real_)))

## Promedio nacional de años de escolaridad ----

educ_prom <- weighted.mean(conc_pob_24$anesc_educ, w= conc_pob_24$factor)

#Etiqueta de grupo etario (edad)
conc_pob_24 <- conc_pob_24 %>%
  mutate(ge = case_when(edad>=0 & edad<=5 ~ 1,
                        edad>=6 & edad<=11 ~ 2,
                        edad>=12 & edad<=29 ~ 3,
                        edad>=30 & edad<=64 ~4,
                        edad>= 65 ~ 5))

#Etiquetando en otra variable a ge:
etiquetas <- c("Primera infancia",
               "6 a 11 años",
               "Jóvenes",
               "30 a 64 años",
               "Población adulta mayor",
               "45 a 60 años")

conc_pob_24 <- conc_pob_24 %>%
  mutate(id_ge = etiquetas[ge])
## Generacion de tipos ----
conc_pob_24 <- conc_pob_24 %>%
  mutate(tipos= case_when(
    (sexo==2 & hli==1 & region==5 & anesc_educ < educ_prom & rural==1) ~ 1,
    (sexo== 1 & hli==0 & region==1 & anesc_educ > educ_prom & rural==0) ~ 2,
    (sexo==2 & hli==0 & region==1 & anesc_educ > educ_prom & rural==0) ~ 3, 
    (sexo==1 & hli==1 & region==5 & anesc_educ < educ_prom & rural==1) ~ 4,
    TRUE ~ 5))


## Divisiones (deciles, etc.) ----

### Deciles para ingreso total ----

#Creando una bandera:
conc_pob_24$Nhog <- 1

#Suma del total de los hogares:
tot_hog <- sum(conc_pob_24$factor)

#Tamaño de la distribución:
tam_dec<-trunc(tot_hog/10)

#Incorportando a base:
conc_pob_24$tam_dec=tam_dec

#Creando auxiliar del ingreso:
conc_pob_24$ing_total2 <- conc_pob_24$ing_total

#Ordenando:
conc_pob_24 <- conc_pob_24[with(conc_pob_24, order(rank(ing_total2))),]

#Sea la suma acumulada del factor:
conc_pob_24$ACUMULA <- cumsum(conc_pob_24$factor)

#Bucle para generar los deciles para ingreso total:
for(i in 1:9){
  a1<-conc_pob_24[dim(conc_pob_24[conc_pob_24$ACUMULA<tam_dec*i,])[1]+1,]$factor
  conc_pob_24<-rbind(conc_pob_24[1:(dim(conc_pob_24[conc_pob_24$ACUMULA<tam_dec*i,])[1]+1),],
                     conc_pob_24[(dim(conc_pob_24[conc_pob_24$ACUMULA<tam_dec*i,])[1]+1):dim(conc_pob_24[1])[1],])
  
  b1<-tam_dec*i-conc_pob_24[dim(conc_pob_24[conc_pob_24$ACUMULA<tam_dec*i,])[1],]$ACUMULA
  conc_pob_24[(dim(conc_pob_24[conc_pob_24$ACUMULA<tam_dec*i,])[1]+1),]$factor<-b1
  conc_pob_24[(dim(conc_pob_24[conc_pob_24$ACUMULA<tam_dec*i,])[1]+2),]$factor<-(a1-b1)
}

conc_pob_24$ACUMULA2<-cumsum(conc_pob_24$factor)
conc_pob_24$decil <- 0
conc_pob_24[(conc_pob_24$ACUMULA2<=tam_dec),]$decil <- 1

for(i in 1:9){
  conc_pob_24[((conc_pob_24$ACUMULA2>tam_dec*i)&(conc_pob_24$ACUMULA2<=tam_dec*(i+1))),]$decil <- (i+1)
}

conc_pob_24[conc_pob_24$decil%in%"0",]$decil <- 10

### Deciles para ingreso laboral REAL----

#Creando una bandera:
conc_pob_24$Nhog <- 1

#Suma del total de los hogares:
tot_hog <- sum(conc_pob_24$factor)

#Tamaño de la distribución:
tam_dec<-trunc(tot_hog/10)

#Incorportando a base:
conc_pob_24$tam_dec=tam_dec

#Creando auxiliar del ingreso:
conc_pob_24$ing_trab2 <- conc_pob_24$ing_trab

#Ordenando:
conc_pob_24 <- conc_pob_24[with(conc_pob_24, order(rank(ing_trab2))),]

#Sea la suma acumulada del factor:
conc_pob_24$ACUMULA <- cumsum(conc_pob_24$factor)

#Bucle para generar los deciles para ingreso total:
for(i in 1:9){
  a1<-conc_pob_24[dim(conc_pob_24[conc_pob_24$ACUMULA<tam_dec*i,])[1]+1,]$factor
  conc_pob_24<-rbind(conc_pob_24[1:(dim(conc_pob_24[conc_pob_24$ACUMULA<tam_dec*i,])[1]+1),],
                     conc_pob_24[(dim(conc_pob_24[conc_pob_24$ACUMULA<tam_dec*i,])[1]+1):dim(conc_pob_24[1])[1],])
  
  b1<-tam_dec*i-conc_pob_24[dim(conc_pob_24[conc_pob_24$ACUMULA<tam_dec*i,])[1],]$ACUMULA
  conc_pob_24[(dim(conc_pob_24[conc_pob_24$ACUMULA<tam_dec*i,])[1]+1),]$factor<-b1
  conc_pob_24[(dim(conc_pob_24[conc_pob_24$ACUMULA<tam_dec*i,])[1]+2),]$factor<-(a1-b1)
}

conc_pob_24$ACUMULA2<-cumsum(conc_pob_24$factor)
conc_pob_24$decil_l <- 0
conc_pob_24[(conc_pob_24$ACUMULA2<=tam_dec),]$decil_l <- 1

for(i in 1:9){
  conc_pob_24[((conc_pob_24$ACUMULA2>tam_dec*i)&(conc_pob_24$ACUMULA2<=tam_dec*(i+1))),]$decil_l <- (i+1)
}

conc_pob_24[conc_pob_24$decil_l%in%"0",]$decil_l <- 10

### Diseño muestral ----
dm <- conc_pob_24 %>%
  as_survey_design(ids = upm,
                   strata = est_dis,
                   weights = factor)
## Generación de tablas (región, decil, hli, sexo1) ----

grupos <- c("decil", "decil_l", "region_e", "hli", "rural", "sexo1", "tipos")
for (grupo in grupos) {
  
  res <- dm %>%
    group_by(!!sym(grupo)) %>%
    summarize(
      ing_totalp = survey_mean(ing_total, vartype = c("se", "cv"), level = 0.95),
      ing_trabp= survey_mean(ing_trab, vartype = c("se", "cv"), level= 0.95)
    )
  
  assign(paste0("ing_total_", grupo), res)
}




## Estimación de ingreso promedio por región  ----
ing_total_region_e$LCI <- ing_total_region_e$ing_totalp - (1.96 * ing_total_region_e$ing_totalp_se)
ing_total_region_e$LCS <- ing_total_region_e$ing_totalp + (1.96 * ing_total_region_e$ing_totalp_se)





# cl <- cl %>%
#   mutate(participacion = (ing_cor/tot_ing)*100,
#          acum_pp = cumsum(participacion),
#          eq = decil*10,
#          declab = factor(decil,
#                          levels = 1:10,
#                          labels = c("I","II","III","IV","V",
#                                     "VI","VII","VIII","IX","X")))





## Generación de tablas (región, decil_l, hli, sexo1) ----

grupos <- c("decil_l", "region_e", "hli", "rural", "sexo1")
for (grupo in grupos) {
  
  res <- dm %>%
    group_by(!!sym(grupo)) %>%
    summarize(
      ing_trabp = survey_mean(ing_trab, vartype = c("se", "cv"), level = 0.95),
    )
  
  assign(paste0("ing_trab_", grupo), res)
}

write_xlsx(ing_total_tipos, path = ("tablas/2024/ing_total_tipos_2024.xlsx"))



## Estimación de ingreso promedio por región  ----
ing_trab_region_e$LCI <- ing_trab_region_e$ing_trabp - (1.96 * ing_trab_region_e$ing_trabp_se)
ing_trab_region_e$LCS <- ing_trab_region_e$ing_trabp + (1.96 * ing_trab_region_e$ing_trabp_se)


# Indice de GINI ----

grupos1 <- c("id_ge", "region_e", "rural", "tipos")

for (grupo in grupos1) {
  
  f_gini <- as.formula(paste0("~", grupo))
  
  res_gini_total <- svyby(~ing_total,  f_gini, dm, svygini, na.rm = TRUE)
  res_gini_trab <- svyby(~ing_trab,  f_gini, dm, svygini, na.rm = TRUE)
  
  
  res_gini_final <-  res_gini_total %>%
    rename(
      gini_total = ing_total,
      se_total = se
    ) %>%
    left_join(
      res_gini_trab %>% 
        select(!!sym(grupo), ing_trab, se) %>% 
        rename(
          gini_trab = ing_trab,
          se_trab = se
        ),
      by = grupo
    )
  
  assign(paste0("gini_", grupo), res_gini_final)
  
  rm(res_gini_total, res_gini_trab, res_gini_final)
}


write_xlsx(gini_id_ge, path = ("tablas/gini_id_ge_2024.xlsx"))
write_xlsx(gini_region_e, path = ("tablas/gini_region_e_2024.xlsx"))
write_xlsx(gini_rural, path = ("tablas/gini_rural_2024.xlsx"))
write_xlsx(gini_tipos, path = ("tablas/gini_tipos_2024.xlsx"))



# cl <- cl %>%
#   mutate(participacion = (ing_cor/tot_ing)*100,
#          acum_pp = cumsum(participacion),
#          eq = decil*10,
#          declab = factor(decil,
#                          levels = 1:10,
#                          labels = c("I","II","III","IV","V",
#                                     "VI","VII","VIII","IX","X")))
# 


# 4. Gráficos ----
## Ingreso total mensual promedio por region 2024 ----
ing_total_nacional <- weighted.mean(conc_pob_24$ing_total, w= conc_pob_24$factor)

library(ggplot2)
library(scales)

G1_ing_total_region_e_24 <- ggplot(ing_total_region_e, aes(x = reorder(region_e, ing_totalp), y = ing_totalp)) +
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
  # Línea de ingreso medio nacional
  geom_hline(aes(yintercept = 68450, color = "Ingreso promedio nacional"), 
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
    # TAMAÑO DE ETIQUETAS
    axis.text.x = element_text(angle = 0, face = "bold", color = "black", size = 12), 
    axis.text.y = element_text(face = "bold", color = "black", size = 12),
    # Título Principal
    plot.title = element_text(face = "bold", size = 20),
    # Subtítulo
    plot.subtitle = element_text(face = "bold", size = 17),
    # Títulos de Ejes con margen
    axis.title.x = element_text(face = "bold", size = 17, margin = margin(t = 15)),
    axis.title.y = element_text(face = "bold", size = 17, margin = margin(r = 15)),
    # Fuente (Caption) con margen superior para empujarlo hacia abajo
    plot.caption = element_text(face = "bold", size = 12, hjust = 1, margin = margin(t = 15)),
    # Leyenda
    legend.position = c(0.85, 0.15), 
    legend.background = element_rect(fill = "white", color = "lightgrey"),
    legend.text = element_text(size = 11)
  ) +
  labs(
    title = "Figura 3: ingreso total mensual promedio por región\nen México, 2024",
    subtitle = "Precios de 2024",
    x = "Región",
    y = "Ingreso mensual (miles de pesos)",
    caption = "Fuente: elaboración propia con datos de la ENIGH 2024 de INEGI"
  )

G1_ing_total_region_e_24


# Guardar el gráfico con las dimensiones y formato deseados
ggsave(
  filename = "graficos/G1_ing_total_region_e_24.png", 
  plot = G1_ing_total_region_e_24, 
  width = 10, 
  height = 8, 
  units = "in", 
  dpi = 300,
  bg = "white" # Asegura que el fondo sea blanco al exportar
)



## Ingreso laboral mensual promedio por region 2024 ----
ing_laboral_nacional <- weighted.mean(conc_pob_24$ing_trab, w= conc_pob_24$factor)

library(ggplot2)
library(scales)

G2_ing_trab_region_e_24 <- ggplot(ing_trab_region_e, aes(x = reorder(region_e, ing_trabp), y = ing_trabp)) +
  # Líneas de error
  geom_errorbar(aes(ymin = LCI, ymax = LCS), width = 0.3) +
  # Puntos centrales
  geom_point(color = "firebrick", size = 2.5) +
  # ETIQUETAS DE DATOS
  geom_text(aes(label = dollar(ing_trabp * (1/3) / 1000, accuracy = 0.1)), 
            vjust = -0.1,
            hjust = -0.4,
            fontface = "bold", 
            size = 4.5) +
  # Línea de ingreso medio nacional
  geom_hline(aes(yintercept = 45961, color = "Ingreso promedio laboral nacional"), 
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
    # AGREGAR LÍNEA DEL EJE Y (y mantener el X en blanco si así lo prefieres)
    axis.line.x = element_line(color = "black", linewidth = 0.5),
    axis.line.y = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    # TAMAÑO DE ETIQUETAS
    axis.text.x = element_text(angle = 0, face = "bold", color = "black", size = 12), 
    axis.text.y = element_text(face = "bold", color = "black", size = 12),
    # Título Principal
    plot.title = element_text(face = "bold", size = 20),
    # Subtítulo
    plot.subtitle = element_text(face = "bold", size = 17),
    # Títulos de Ejes
    axis.title.x = element_text(face = "bold", size = 17, margin = margin(t = 15)),
    axis.title.y = element_text(face = "bold", size = 17, margin = margin(r = 15)),
    # Fuente (Caption)
    plot.caption = element_text(face = "bold", size = 12, hjust = 1, margin = margin(t = 15)),
    # Leyenda
    legend.position = c(0.85, 0.15), 
    legend.background = element_rect(fill = "white", color = "lightgrey"),
    legend.text = element_text(size = 11)
  ) +
  labs(
    title = "Figura 5: ingreso laboral mensual promedio por región\nen México, 2024",
    subtitle = "Precios de 2024",
    x = "Región",
    y = "Ingreso mensual (miles de pesos)",
    caption = "Fuente: elaboración propia con datos de la ENIGH 2024 de INEGI"
  )

G2_ing_trab_region_e_24

# Guardar el gráfico con las dimensiones y formato deseados
ggsave(
  filename = "graficos/G2_ing_trab_region_e_24.png", 
  plot = G2_ing_trab_region_e_24, 
  width = 10, 
  height = 8, 
  units = "in", 
  dpi = 300,
  bg = "white" # Asegura que el fondo sea blanco al exportar
)

# 6.Tablas ----

## Nacional ----
tabla <- dm %>%
  group_by(decil) %>%
  summarise(ing_totalp = survey_mean(ing_total,
                                     na.rm = TRUE))

##

tabla <- dm %>%
  group_by(decil) %>%
  summarise(ing_totalp = survey_mean(ing_total,
                                     na.rm = TRUE))

## Por regiones ----

### Tabla de deciles y regiones del ingresto total 2020 ----
tabla_deciles_regiones_24 <- dm %>%
  group_by(decil, region_e) %>%
  summarise(ing_totalp = survey_mean(ing_total,
                                     na.rm = TRUE))

write_xlsx(tabla_deciles_regiones_24, path = "tabla_deciles_regiones_24.xlsx")


### Tabla de deciles y regiones del ingresto laboral 2024 ----
tabla_deciles_regiones_ing_trab_24 <- dm %>%
  group_by(decil_l, region_e) %>%
  summarise(ing_trabp = survey_mean(ing_total,
                                    na.rm = TRUE))

write_xlsx(tabla_deciles_regiones_ing_trab_24, path = "tabla_deciles_regiones_ing_trab_24.xlsx")


### Tabla de deciles en región Centro C ----
# tabla_deciles_reg_C_20 <- tabla_deciles_regiones_20 %>%
#   filter(region_e == "C")
# 
# ### Tabla de deciles en región Centro Norte CN ----
# tabla_deciles_reg_CN_20 <- tabla_deciles_regiones_20 %>%
#   filter(region_e == "CN")
# 
# write_xlsx(tabla_deciles_reg_CN_20, path = here("tabla_deciles_reg_CN_20.xlsx"))
# 
# ### Tabla de deciles en región Norte N ----
# tabla_deciles_reg_N_20 <- tabla_deciles_regiones_20 %>%
#   filter(region_e == "N")
# 
# write_xlsx(tabla_deciles_reg_CN_20, path = here("tabla_deciles_reg_CN_20.xlsx"))
# 
# 
# ### Tabla de deciles en región Norte-Oeste NO ----
# tabla_deciles_reg_NO_20 <- tabla_deciles_regiones_20 %>%
#   filter(region_e == "NO")
# 
# write_xlsx(tabla_deciles_reg_CN_20, path = here("tabla_deciles_reg_CN_20.xlsx"))
# 
# 
# ### Tabla de deciles en región Sur S ----
# tabla_deciles_reg_S_20 <- tabla_deciles_regiones_20 %>%
#   filter(region_e == "S")
# 
# write_xlsx(tabla_deciles_reg_CN_20, path = here("tabla_deciles_reg_CN_20.xlsx"))
# 




# ==============================================================================
# ANÁLISIS DE PIB REGIONAL (2003-2024) - VERSIÓN FINAL CON LEYENDA LEGIBLE
# ==============================================================================

library(tidyverse)
library(readxl)
library(scales)

# 1. Cargar el dataframe original
df_pib <- read_excel("tablas/pib_regional.xlsx")

# 2. Convertir todas las columnas a numérico
df_pib <- df_pib %>% mutate(across(everything(), as.numeric))

# 3. Generar la gráfica
G10_pib_regional <- df_pib %>%
  select(Año, N, NO, CN, C, S) %>%
  pivot_longer(cols = -Año, names_to = "Region", values_to = "Indice") %>%
  ggplot(aes(x = Año, y = Indice, color = Region)) +
  
  # Líneas y puntos
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.5) +
  
  # Colores y etiquetas de regiones (name = NULL quita "Región" de la leyenda)
  scale_color_manual(
    values = c("N" = "#1B4F72", "NO" = "#2E86C1", "CN" = "#A93226", "C" = "#1E8449", "S" = "#D4AC0D"),
    labels = c("N" = "Norte", "NO" = "Norte-Oeste", "CN" = "Centro-Norte", "C" = "Centro", "S" = "Sur"),
    name = NULL 
  ) +
  
  # Ejes: Años horizontales y cada uno presente
  scale_x_continuous(breaks = 2003:2024) + 
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  
  # Estética y Formato
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    # Etiquetas horizontales legibles
    axis.text.x = element_text(angle = 0, face = "bold", color = "black", size = 8.5), 
    axis.text.y = element_text(face = "bold", color = "black", size = 12),
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(face = "bold", size = 17),
    axis.title.x = element_text(face = "bold", size = 17, margin = margin(t = 15)),
    axis.title.y = element_text(face = "bold", size = 17, margin = margin(r = 15)),
    # LEYENDA (Tamaño aumentado a 13)
    legend.position = "bottom",
    legend.background = element_rect(fill = "white", color = "lightgrey"),
    legend.text = element_text(size = 13, face = "bold"),
    # Fuente con el texto solicitado
    plot.caption = element_text(face = "bold", size = 10, margin = margin(t = 15), hjust = 1)
  ) +
  labs(
    title = "Figura 1: Evolución del PIB Regional en México, 2003-2024",
    subtitle = "Índice de volumen físico (Base 2003 = 100)",
    x = "Año",
    y = "Índice del PIB",
    caption = "Fuente: Elaboración propia con datos del Banco de Información Económica (BIE) del INEGI"
  )

# 4. Mostrar y guardar
print(G10_pib_regional)

ggsave(
  filename = "graficos/G10_pib_regional_final_v2.png", 
  plot = G10_pib_regional, 
  width = 14, 
  height = 8, 
  units = "in", 
  dpi = 300, 
  bg = "white"
)



