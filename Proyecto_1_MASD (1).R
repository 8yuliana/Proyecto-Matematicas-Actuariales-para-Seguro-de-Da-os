# ==============================
# BIBLIOTECAS
# ==============================

install.packages("tidyverse")
install.packages("readxl")
install.packages("fitdistrplus")
install.packages("actuar")
install.packages("moments")
install.packages("writexl")
install.packages("goftest")
install.packages("flexsurv")
install.packages("rriskDistributions")
install.packages("expint")

library(tidyverse)
library(readxl)
library(fitdistrplus)
library(actuar)
library(moments)
library(writexl) # para guardar la base de datos limpia
library(goftest)
library(flexsurv)
library(rriskDistributions)
library(expint)

# ==============================
# CARGAR DATOS
# ==============================

# Asumimos que estamos en el directorio general, el del repositorio de Github

ruta <- "data/Basehistorica_2000_a_2024.xlsx"

# Solo consideraremos "Riesgos Hidrometeorológico"

df_original <- read_excel(ruta) %>% 
  filter(`Clasificación del fenómeno` == "Hidrometeorológico")

# ==============================
# LIMPIEZA DE LOS DATOS
# ==============================

# Sólo trbajamos con datos en los que tenemos el valor de las pérdidas

df_original <- df_original %>%
  mutate(`Total de daños (millones de pesos)` = as.numeric(`Total de daños (millones de pesos)`))

df_original <- df_original %>%
  filter(!is.na(`Total de daños (millones de pesos)`), 
         `Total de daños (millones de pesos)` > 0)

df_original <- df_original %>%
  # Paso 0: Correcciones manuales
  mutate(
    `Fecha de Inicio` = case_when(
      row_number() == 2171 ~ "43699",
      row_number() == 2219 ~ "43796",
      TRUE ~ as.character(`Fecha de Inicio`)
    ),
    `Fecha de Fin` = case_when(
      row_number() == 2171 ~ "43699",
      row_number() == 2219 ~ "43796",
      TRUE ~ as.character(`Fecha de Fin`)
    )
  ) %>%
  
  # Paso 1: Corregir el caso especial "29-Sep"
  mutate(
    `Fecha de Inicio` = if_else(
      `Fecha de Fin` == "29-Sep",
      "37163",
      as.character(`Fecha de Inicio`)
    ),
    `Fecha de Fin` = if_else(
      `Fecha de Fin` == "29-Sep",
      "37163",
      as.character(`Fecha de Fin`)
    )
  ) %>%
  
  # Paso 2: Convertir serial de Excel a Date
  mutate(
    `Fecha de Inicio` = as.Date(
      as.numeric(ifelse(!is.na(`Fecha de Inicio`) & grepl("^[0-9]+$", `Fecha de Inicio`),
                        `Fecha de Inicio`, NA)),
      origin = "1899-12-30"
    ),
    `Fecha de Fin` = as.Date(
      as.numeric(ifelse(!is.na(`Fecha de Fin`) & grepl("^[0-9]+$", `Fecha de Fin`),
                        `Fecha de Fin`, NA)),
      origin = "1899-12-30"
    )
  ) %>%
  
  # Paso 3: Corregir Fechas
  mutate(
    `Fecha de Inicio` = if_else(is.na(`Fecha de Inicio`), `Fecha de Fin`, `Fecha de Inicio`),
    `Fecha de Fin`    = if_else(is.na(`Fecha de Fin`),    `Fecha de Inicio`, `Fecha de Fin`)
  )

df_hidro <- df_original %>%
  # Renombrar columnas 
  rename(
    anio = `Año`,
    clasificacion = `Clasificación del fenómeno`,
    tipo_fenomeno = `Tipo de fenómeno`,
    estado = `Estado`,
    perdidas = `Total de daños (millones de pesos)`
  ) %>%
  
  # Quitamos columnas que no nos sirvan 
  dplyr::select(
    -clasificacion,          
    -`Municipios Afectados`, 
    -`Descripcion general de los daños`, 
    -`Fuente`,             
    -`Defunciones`,          
    -`Población afectada`,               
    -`Viviendas dañadas`,              
    -`Escuelas`,              
    -`Hospitales`,          
    -`Comercios`,       
    -`Area de cultivo dañada / pastizales (h)` 
  ) %>%
  
  # Casos particulares de Varios Estados
  
  mutate(
    estado = if_else(
      row_number() == 1365,  # ajusta el número si cambió tras eliminar fila 33
      "Sonora, Sinaloa, Chihuahua, Durango, Zacatecas, Veracruz",
      estado
    )
  ) %>%
  
  # Limpiar estados
  mutate(
    estado = str_replace_all(estado, " y ", ","),
    estado = str_trim(estado)
  ) %>%
  
  # Contar estados
  mutate(n_estados = str_count(estado, ",") + 1) %>%
  
  # Separar filas
  separate_rows(estado, sep = ",") %>%
  
  # Limpiar otra vez
  mutate(estado = str_trim(estado)) %>%
  
  # Ajustar daños 
  mutate(
    perdidas = perdidas / n_estados
    # La parte de Municipios Afectados ya no es necesaria porque eliminaste esa columna
  ) %>%
  
  dplyr::select(-n_estados)

# Quitamos la fila con clasificación de "Varios Estados sin detalle"
df_hidro <- df_hidro[-34, ]

# Quitamos la fila con clasificación de "otros"
df_hidro <- df_hidro[-5, ]


# ==============================
# Primer Análisis
# ==============================

p <- df_hidro$perdidas
min(p)
max(p)

# Rango muestral 
max(p)-min(p)

# Cuartiles
quantile(p,c(0.25,0.50,0.75))

median(p)
mean(p)

var(p)
sd(p)

# Interválo de Confianza del 68%

cat(mean(p)-sd(p),mean(p)+sd(p))

# Interválo de Confianza del 95%

cat(mean(p)-sd(p)*2,mean(p)+sd(p)*2)

# Interválo de Confianza del 99%

cat(mean(p)-sd(p)*3,mean(p)+sd(p)*3)

# Coeficiente de Variación

sd(p)/mean(p)

# Coeficiente de Asímetria

skewness(p)

# Curtouis

kurtosis(p)

# Análisis Gráfico 

# Regla de Sturges
k_sturges <- nclass.Sturges(df_hidro$perdidas)

# Histograma
hist(p,breaks=k_sturges)

# Boxplot
boxplot(p)

# Visualización de Outliers

stats <- boxplot.stats(df_hidro$perdidas)

limite_inf <- stats$stats[1] # Extremo del bigote inferior
limite_sup <- stats$stats[5] # Extremo del bigote superior

outliers_menores <- df_hidro %>% filter(perdidas < limite_inf)
outliers_mayores <- df_hidro %>% filter(perdidas > limite_sup)

n_menores <- nrow(outliers_menores)
n_mayores <- nrow(outliers_mayores)
n_total   <- nrow(df_hidro)

# Calcular proporciones
prop_menores <- (n_menores / n_total) * 100
prop_mayores <- (n_mayores / n_total) * 100
prop_total_outliers <- ((n_menores + n_mayores) / n_total) * 100


cat("Cantidad de datos totales:", n_total, "\n\n")
cat("Outliers MENORES:", n_menores, "(", round(prop_menores, 2), "% )\n")
cat("Outliers MAYORES:", n_mayores, "(", round(prop_mayores, 2), "% )\n")
cat("Proporción TOTAL de outliers:", round(prop_total_outliers, 2), "%\n")

# Función de Distribución
plot(ecdf(p))

# ==============================
# DRIVER DE CLASIFIFCACIÓN 
# ==============================

class(df_hidro)
str(df_hidro)
head(df_hidro)
tail(df_hidro)

# Observemos todas los "Tipos de fenomenos que ocurren"

unique(df_hidro$tipo_fenomeno)

# Proponemos la siguiente clasificación
# Nos basamos en el principal factor destructivo de cada uno de ellos

df_hidro <- df_hidro %>%
  
  mutate(
    fenomeno_clasificado = case_when(
      tipo_fenomeno %in% c("Inundación", "Lluvias", "Lluvia", 
                           "Tormenta tropical", "Tormenta severa", 
                           "Tormenta eléctrica", "Marea de tormenta", 
                           "Mar de fondo") ~ "Agua",
      
      tipo_fenomeno %in% c("Ciclón tropical", "Fuertes vientos", "Tornado") ~ "Viento",
      
      tipo_fenomeno %in% c("Bajas temperaturas", "Helada", 
                           "Nevada", "Granizada") ~ "Temp_Fria",
      
      tipo_fenomeno %in% c("Sequía", "Temperatura extrema") ~ "Temp_Calida",
      
      TRUE ~ "Otros"
    )
  ) %>%
  
  mutate(
    estado_clasificado = case_when(
      estado %in% c("Baja California", "Baja California Sur", "Sonora", "Sinaloa",
                    "Nayarit", "Jalisco", "Colima", "Michoacán", "Guerrero",
                    "Oaxaca", "Chiapas", "Tamaulipas", "Veracruz",
                    "Tabasco", "Campeche", "Yucatán", "Quintana Roo") ~ "Con_Costa",
      TRUE ~ "Sin_Costa"
    )
  ) %>%
  
  mutate(
    ubicacion_estado = case_when(
      # Costa Pacífico
      estado %in% c("Baja California", "Baja California Sur", "Sonora", "Sinaloa",
                    "Nayarit", "Jalisco", "Colima", "Michoacán", "Michoacan", "Guerrero",
                    "Oaxaca", "Chiapas") ~ "Pacifico",
      
      # Costa Golfo / Caribe
      estado %in% c("Tamaulipas", "Veracruz", "Tabasco", 
                    "Campeche", "Yucatán", "Quintana Roo") ~ "Golfo",
      
      # Sin Costa Norte
      estado %in% c("Chihuahua", "Coahuila", "Nuevo León", "Nuevo Leon", "Durango",
                    "Zacatecas", "San Luis Potosí", "Aguascalientes") ~ "Norte",
      
      # Sin Costa Sur/Centro
      estado %in% c("Ciudad de México", "Estado de México", "Puebla", "Tlaxcala",
                    "Hidalgo", "Morelos", "Querétaro",
                    "Guanajuato", "México") ~ "Sur",
    )
  ) %>%
  
  relocate(fenomeno_clasificado, .after = tipo_fenomeno) %>% 
  relocate(estado_clasificado, .after = estado) %>% 
  relocate(ubicacion_estado, .after = estado_clasificado)

# Guarda la base de datos limpia
write_xlsx(df_hidro, "data/df_limpia.xlsx")

# ==============================
# AJUSTE INFLACIONARIO
# ==============================

# Factores de inflación anual (Diciembre a Diciembre)
# Nota: Se incluye 2024 (4.21%) para valuar a cierre de ese año.
inflacion_anual <- c(
  "2001" = 4.40, "2002" = 5.70, "2003" = 3.98, "2004" = 5.19,
  "2005" = 3.33, "2006" = 4.05, "2007" = 3.76, "2008" = 6.53,
  "2009" = 3.57, "2010" = 4.40, "2011" = 3.82, "2012" = 3.57,
  "2013" = 3.97, "2014" = 4.08, "2015" = 2.13, "2016" = 3.36,
  "2017" = 6.77, "2018" = 4.83, "2019" = 2.83, "2020" = 3.15,
  "2021" = 7.36, "2022" = 7.82, "2023" = 4.66, "2024" = 4.21
)

# Convertir tasas a factores (1 + r)
factores_anuales <- 1 + (inflacion_anual / 100)

# Función para calcular el factor acumulado desde el año X hasta DIC-2024
# Si el dinero es de DIC-2000, debe multiplicarse por inflacion 2001...2024
obtener_factor_acumulado <- function(anio_origen) {
  if (anio_origen >= 2024) return(1.0)
  
  # Los años de inflación que le afectan son desde el siguiente hasta 2024
  anios_a_aplicar <- as.character((anio_origen + 1):2024)
  return(prod(factores_anuales[anios_a_aplicar]))
}

# Crear el dataframe de factores para el join
df_factores <- data.frame(
  anio = 2000:2024,
  factor_inflacion = sapply(2000:2024, obtener_factor_acumulado)
)

# Join + ajuste
df_hidro <- df_hidro %>%
  left_join(df_factores, by = "anio") %>%
  mutate(
    perdidas_actualizadas = perdidas * factor_inflacion
  ) %>%
  dplyr::select(-factor_inflacion)

# ==============================
# TRANSFORMACIÓN LN 
# ==============================

# Suavizar las pérdidas

df_hidro <- df_hidro %>%
  mutate(
    log_perdidas = log(perdidas)
  )

# ==============================
# VISUALIZACIÓN DE LA CLASIFICACIÓN POR 2 CASOS
# ==============================

# POR FENÓMENO CLASIFICADO

par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3, 1)) 

segmentos <- unique(df_hidro$fenomeno_clasificado)

for (i in 1:4) {
  nombre_seg <- segmentos[i]
  datos_log <- df_hidro$log_perdidas[df_hidro$fenomeno_clasificado == nombre_seg]
  
  hist(datos_log, 
       breaks = nclass.Sturges(datos_log), 
       col = hcl.colors(4, "Dark 3")[i], 
       border = "white",
       main = paste("Tipo Fenómeno:", nombre_seg),
       xlab = "Log-Pérdidas (Suavizadas)", 
       ylab = "Frecuencia",
       cex.main = 1)
  
  abline(v = mean(datos_log, na.rm = TRUE), col = "red", lwd = 2, lty = 2)   # Media 
  abline(v = median(datos_log, na.rm = TRUE), col = "black", lwd = 2)        # Mediana 
}

# Título 
mtext("Comparativa de Severidad Suavizada por Fenómeno Clasificado", 
      outer = TRUE, cex = 1.2, font = 2)

par(mfrow = c(1, 1))

# POR UBICACIÓN

par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3, 1), oma = c(0, 0, 2, 0))

regiones_orden <- c("Norte", "Sur", "Pacifico", "Golfo")

for (i in 1:4) {
  reg <- regiones_orden[i]
  datos_log <- df_hidro$log_perdidas[df_hidro$ubicacion_estado == reg]
  
  hist(datos_log, 
       breaks = nclass.Sturges(datos_log), 
       col = hcl.colors(4, "Temps")[i], 
       border = "white",
       main = paste("Zona:", reg),
       xlab = "Log-Pérdidas", 
       ylab = "Frecuencia",
       cex.main = 1.1)
  
  abline(v = mean(datos_log), col = "red", lwd = 2, lty = 2) # Media
  abline(v = median(datos_log), col = "black", lwd = 2) # Mediana 
}

# Título 
mtext("Comparativa de Severidad Suavizada por Región Geográfica", 
      outer = TRUE, cex = 1.2, font = 2)

par(mfrow = c(1, 1))

# ==============================
# SEGMENTACIÓN DE CLASIFICACIÓN POR FENÓMENO CLASIFICADO
# ==============================

df_hidrotemp_calida  <- df_hidro %>% filter(fenomeno_clasificado == "Temp_Calida")
df_hidrotemp_fria     <- df_hidro %>% filter(fenomeno_clasificado == "Temp_Fria")
df_hidroagua <- df_hidro %>% filter(fenomeno_clasificado == "Agua")
df_hidroviento    <- df_hidro %>% filter(fenomeno_clasificado == "Viento")

table(df_hidro$tipo_fenomeno)
table(df_hidro$fenomeno_clasificado)

# Tamaño
cat("Registros por tabla:\n",
    "Temp_Calida:   ", nrow(df_hidrotemp_calida), "\n",
    "Temp_Fria:     ", nrow(df_hidrotemp_fria), "\n",
    "Agua:", nrow(df_hidroagua), "\n",
    "Viento:   ", nrow(df_hidroviento))

# ==============================
#       Temperatura Cálida
# ==============================

# Análsis Descriptivo 

p_temp_calida <- df_hidrotemp_calida$perdidas
logp_temp_calida <- df_hidrotemp_calida$log_perdidas

# Estadísticos 
resumen <- data.frame(
  n = length(p_temp_calida),
  Media = mean(p_temp_calida),
  Mediana = median(p_temp_calida),
  Desv_Est = sd(p_temp_calida),
  Coef_Var = sd(p_temp_calida) / mean(p_temp_calida),
  Min = min(p_temp_calida),
  Max = max(p_temp_calida)
)

# Estadísticos de la Variable Suavizada

Cuartiles = quantile(logp_temp_calida, c(0.25, 0.50, 0.75))

resumen_log <- data.frame(
  m_p = mean(logp_temp_calida),
  s_p = sd(logp_temp_calida),
  Rango_Muestral = max(logp_temp_calida) - min(logp_temp_calida),
  Varianza = var(logp_temp_calida),
  Asimetria = skewness(logp_temp_calida),
  Curtosis = kurtosis(logp_temp_calida)
)

# Intervalos de Confianza
m_p <- mean(logp_temp_calida)
s_p <- sd(logp_temp_calida)

intervalos <- data.frame(
  Nivel = c("68%", "95%", "99%"),
  Inferior = c(m_p - s_p, m_p - s_p * 2, m_p - s_p * 3),
  Superior = c(m_p + s_p, m_p + s_p * 2, m_p + s_p * 3)
)

# Frecuencia de los tipos de fenómenos
tabla_estado <- table(df_hidrotemp_calida$ubicacion_estado)
tabla_tipofen <- table(df_hidrotemp_calida$tipo_fenomeno)

cat("REPORTE TEMPERATURA CÁLIDA")
print(resumen)
cat("Pérdidas suavizadas")
print(resumen_log)
print(Cuartiles)
cat("Intervalos de Confianza Muestrales")
print(intervalos)
cat("Distribución de Fenómenos")
print(tabla_estado)
print(tabla_tipofen)

# Análisis gráfico

#Histograma

hist(logp_temp_calida, 
     breaks = nclass.Sturges(logp_temp_calida), 
     col = hcl.colors(4, "Temps")[4],
     border = "white",
     main = "Severidad Temperatura Cálida",
     xlab = "Log-Pérdidas", 
     ylab = "Frecuencia",
     cex.main = 1.2,
     font.main = 2)

abline(v = mean(logp_temp_calida), col = "red", lwd = 2, lty = 2)   # Media 
abline(v = median(logp_temp_calida), col = "black", lwd = 2)        # Mediana 

# Boxplot

boxplot(logp_temp_calida, 
        col = hcl.colors(4, "Temps")[4], 
        border = "black",
        main = "Dispersión de Temperatura Cálida",
        ylab = "Log-Pérdidas",
        outline = TRUE) 

# Media
points(1, mean(logp_temp_calida, na.rm = TRUE), col = "red", pch = 18, cex = 2)

# Visualización de Outliers

stats <- boxplot.stats(logp_temp_calida)

limite_inf <- stats$stats[1] 
limite_sup <- stats$stats[5]

outliers_menores <- df_hidrotemp_calida %>% filter(logp_temp_calida < limite_inf)
outliers_mayores <- df_hidrotemp_calida %>% filter(logp_temp_calida > limite_sup)

n_menores <- nrow(outliers_menores)
n_mayores <- nrow(outliers_mayores)
n_total   <- nrow(df_hidrotemp_calida)

# Proporciones 
prop_menores <- (n_menores / n_total) * 100
prop_mayores <- (n_mayores / n_total) * 100
prop_total_outliers <- ((n_menores + n_mayores) / n_total) * 100

cat("Cantidad de datos totales:", n_total, "\n\n")
cat("Outliers MENORES:", n_menores, "(", round(prop_menores, 2), "% )\n")
cat("Outliers MAYORES:", n_mayores, "(", round(prop_mayores, 2), "% )\n")
cat("Proporción TOTAL de outliers:", round(prop_total_outliers, 2), "%\n")

# Eliminar outliers
df_hidrotemp_calida <- df_hidrotemp_calida %>%
  filter(log_perdidas >= limite_inf & log_perdidas <= limite_sup)


# Función de Distribución
plot(ecdf(logp_temp_calida))

#---------
# Modelos
#--------

#Trasladamos para tener valores positivos

desplazamiento_tempc <- abs(min(df_hidrotemp_calida$log_perdidas)) + 0.1

df_hidrotemp_calida <- df_hidrotemp_calida %>%
  mutate(log_tempc_ajustada = log_perdidas + desplazamiento_tempc)
  
logp_tempc_ajustada <- df_hidrotemp_calida$log_tempc_ajustada

# Veamos cual nos conviene revisar
fit.cont(logp_tempc_ajustada)

# Modelo Pareto

mod_1tc <- fitdist(logp_tempc_ajustada, "pareto", method = "mle")
summary(mod_1tc)

denscomp(mod_1tc)
cdfcomp(mod_1tc)
qqcomp(mod_1tc)
ppcomp(mod_1tc)

hip_1pr <- ad.test(logp_tempc_ajustada, 
                   "ppareto", 
                   shape = mod_1tc$estimate["shape"], 
                   scale = mod_1tc$estimate["scale"])

hip_2pr <- ks.test(logp_tempc_ajustada, 
                   "ppareto", 
                   shape = mod_1tc$estimate["shape"], 
                   scale = mod_1tc$estimate["scale"])

# Resultados
print(hip_1pr)
print(hip_2pr)

# Modelo Normal

mod_2tc <- fitdist(logp_tempc_ajustada, "norm", method = "mle")
summary(mod_2tc)

denscomp(mod_2tc)
cdfcomp(mod_2tc)
qqcomp(mod_2tc)
ppcomp(mod_2tc)

hip_1nm <- ad.test(logp_tempc_ajustada, 
                   "pnorm", 
                   mean = mod_2tc$estimate["mean"], 
                   sd   = mod_2tc$estimate["sd"])


hip_2nm <- ks.test(logp_tempc_ajustada, 
                   "pnorm", 
                   mean = mod_2tc$estimate["mean"], 
                   sd   = mod_2tc$estimate["sd"])
# Resultados
print(hip_1nm)
print(hip_2nm)


# Contraste de modelos
gofstat(list(mod_1tc,mod_2tc),fitnames = c("Pareto","Normal"))

# Nos quedamos con el modelo Normal

# Comparación de Esperanza teórica con empírica

esp_teorica <- mod_2tc$estimate["mean"]
media_empirica <- mean(logp_tempc_ajustada)

print(esp_teorica)
print(media_empirica)

# Esperanza en millones de pesos
esp_teorica_perdida_tc <- exp(esp_teorica - desplazamiento_tempc)
print(esp_teorica_perdida_tc)

# ==============================
#       Temperatura Fría
# ==============================

# Análsis Descriptivo 

p_temp_fria <- df_hidrotemp_fria$perdidas
logp_temp_fria <- df_hidrotemp_fria$log_perdidas

# Estadísticos 
resumen <- data.frame(
  n = length(p_temp_fria),
  Media = mean(p_temp_fria),
  Mediana = median(p_temp_fria),
  Desv_Est = sd(p_temp_fria),
  Coef_Var = sd(p_temp_fria) / mean(p_temp_fria),
  Min = min(p_temp_fria),
  Max = max(p_temp_fria)
)

# Estadísticos de la Variable Suavizada

Cuartiles = quantile(logp_temp_fria, c(0.25, 0.50, 0.75))

resumen_log <- data.frame(
  m_p = mean(logp_temp_fria),
  s_p = sd(logp_temp_fria),
  Rango_Muestral = max(logp_temp_fria) - min(logp_temp_fria),
  Varianza = var(logp_temp_fria),
  Asimetria = skewness(logp_temp_fria),
  Curtosis = kurtosis(logp_temp_fria)
)

# Intervalos de Confianza
m_p <- mean(logp_temp_fria)
s_p <- sd(logp_temp_fria)

intervalos <- data.frame(
  Nivel = c("68%", "95%", "99%"),
  Inferior = c(m_p - s_p, m_p - s_p * 2, m_p - s_p * 3),
  Superior = c(m_p + s_p, m_p + s_p * 2, m_p + s_p * 3)
)

# Frecuencia de los tipos de fenómenos
tabla_estado <- table(df_hidrotemp_fria$ubicacion_estado)
tabla_tipofen <- table(df_hidrotemp_fria$tipo_fenomeno)

cat("REPORTE TEMPERATURA CÁLIDA")
print(resumen)
cat("Pérdidas suavizadas")
print(resumen_log)
print(Cuartiles)
cat("Intervalos de Confianza Muestrales")
print(intervalos)
cat("Distribución de Fenómenos")
print(tabla_estado)
print(tabla_tipofen)

# Análisis gráfico

#Histograma

hist(logp_temp_fria, 
     breaks = nclass.Sturges(logp_temp_fria), 
     col = hcl.colors(4, "Temps")[1], 
     border = "white",
     main = "Severidad Temperatura Fría", 
     xlab = "Log-Pérdidas", 
     ylab = "Frecuencia",
     cex.main = 1.2,
     font.main = 2)

abline(v = mean(logp_temp_fria), col = "red", lwd = 2, lty = 2)   # Media 
abline(v = median(logp_temp_fria), col = "black", lwd = 2)        # Mediana 

# Boxplot

boxplot(logp_temp_fria, 
        col = hcl.colors(4, "Temps")[1],
        border = "black",
        main = "Dispersión de Temperatura Fría",
        ylab = "Log-Pérdidas",
        outline = TRUE)

# Media
points(1, mean(logp_temp_fria), col = "red", pch = 18, cex = 2)

# Visualización de Outliers

stats <- boxplot.stats(logp_temp_fria)

limite_inf <- stats$stats[1] 
limite_sup <- stats$stats[5]

outliers_menores <- df_hidrotemp_fria %>% filter(logp_temp_fria < limite_inf)
outliers_mayores <- df_hidrotemp_fria %>% filter(logp_temp_fria > limite_sup)

n_menores <- nrow(outliers_menores)
n_mayores <- nrow(outliers_mayores)
n_total   <- nrow(df_hidrotemp_fria)

# Proporciones 
prop_menores <- (n_menores / n_total) * 100
prop_mayores <- (n_mayores / n_total) * 100
prop_total_outliers <- ((n_menores + n_mayores) / n_total) * 100

cat("Cantidad de datos totales:", n_total, "\n\n")
cat("Outliers MENORES:", n_menores, "(", round(prop_menores, 2), "% )\n")
cat("Outliers MAYORES:", n_mayores, "(", round(prop_mayores, 2), "% )\n")
cat("Proporción TOTAL de outliers:", round(prop_total_outliers, 2), "%\n")

# Eliminar outliers
df_hidrotemp_fria <- df_hidrotemp_fria %>%
  filter(log_perdidas >= limite_inf & log_perdidas <= limite_sup)


# Función de Distribución
plot(ecdf(logp_temp_fria))

#---------
# Modelos
#--------

#Trasladamos para tener valores positivos

desplazamiento_tempf <- abs(min(df_hidrotemp_fria$log_perdidas)) + 0.1

df_hidrotemp_fria <- df_hidrotemp_fria %>%
  mutate(log_tempf_ajustada = log_perdidas + desplazamiento_tempf)

logp_tempf_ajustada <- df_hidrotemp_fria$log_tempf_ajustada

# Veamos cual nos conviene revisar
fit.cont(logp_tempf_ajustada)

# Modelo Logística

mod_1tf <- fitdist(logp_tempf_ajustada, "logis", method = "mle")
summary(mod_1tf)

denscomp(mod_1tf)
cdfcomp(mod_1tf)
qqcomp(mod_1tf)
ppcomp(mod_1tf)

hip_1lg <- ad.test(logp_tempf_ajustada, 
                   "plogis", 
                   location = mod_1tf$estimate["location"], 
                   scale    = mod_1tf$estimate["scale"])

hip_2lg <- ks.test(logp_tempf_ajustada, 
                   "plogis", 
                   location = mod_1tf$estimate["location"], 
                   scale    = mod_1tf$estimate["scale"])

# Resultados
print(hip_1lg)
print(hip_2lg)

# Modelo Normal

mod_2tf <- fitdist(logp_tempf_ajustada, "norm", method = "mle")
summary(mod_2tf)

denscomp(mod_2tf)
cdfcomp(mod_2tf)
qqcomp(mod_2tf)
ppcomp(mod_2tf)

hip_1nm <- ad.test(logp_tempf_ajustada, 
                   "pnorm", 
                   mean = mod_2tf$estimate["mean"], 
                   sd   = mod_2tf$estimate["sd"])


hip_2nm <- ks.test(logp_tempf_ajustada, 
                   "pnorm", 
                   mean = mod_2tf$estimate["mean"], 
                   sd   = mod_2tf$estimate["sd"])
# Resultados
print(hip_1nm)
print(hip_2nm)


# Contraste de modelos
gofstat(list(mod_1tf,mod_2tf),fitnames = c("Logística","Normal"))

# Nos quedamos con el modelo Logístico

# Comparación de Esperanza teórica con empírica

esp_teorica <- mod_1tf$estimate["location"]
media_empirica <- mean(logp_tempf_ajustada)

print(esp_teorica)
print(media_empirica)

# Esperanza en millones de pesos
esp_teorica_perdida_tf <- exp(esp_teorica - desplazamiento_tempf)
print(esp_teorica_perdida_tf)

# ==============================
#             Agua
# ==============================

# Análsis Descriptivo 

p_agua <- df_hidroagua$perdidas
logp_agua <- df_hidroagua$log_perdidas

# Estadísticos 
resumen <- data.frame(
  n = length(p_agua),
  Media = mean(p_agua),
  Mediana = median(p_agua),
  Desv_Est = sd(p_agua),
  Coef_Var = sd(p_agua) / mean(p_agua),
  Min = min(p_agua),
  Max = max(p_agua)
)

# Estadísticos de la Variable Suavizada

Cuartiles = quantile(logp_agua, c(0.25, 0.50, 0.75))

resumen_log <- data.frame(
  m_p = mean(logp_agua),
  s_p = sd(logp_agua),
  Rango_Muestral = max(logp_agua) - min(logp_agua),
  Varianza = var(logp_agua),
  Asimetria = skewness(logp_agua),
  Curtosis = kurtosis(logp_agua)
)

# Intervalos de Confianza
m_p <- mean(logp_agua)
s_p <- sd(logp_agua)

intervalos <- data.frame(
  Nivel = c("68%", "95%", "99%"),
  Inferior = c(m_p - s_p, m_p - s_p * 2, m_p - s_p * 3),
  Superior = c(m_p + s_p, m_p + s_p * 2, m_p + s_p * 3)
)

# Frecuencia de los tipos de fenómenos
tabla_estado <- table(df_hidroagua$ubicacion_estado)
tabla_tipofen <- table(df_hidroagua$tipo_fenomeno)

cat("REPORTE TEMPERATURA CÁLIDA")
print(resumen)
cat("Pérdidas suavizadas")
print(resumen_log)
print(Cuartiles)
cat("Intervalos de Confianza Muestrales")
print(intervalos)
cat("Distribución de Fenómenos")
print(tabla_estado)
print(tabla_tipofen)

# Análisis gráfico

#Histograma

hist(logp_agua, 
     breaks = nclass.Sturges(logp_agua), 
     col = "steelblue1", 
     border = "white",
     main = "Severidad de Agua", 
     xlab = "Log-Pérdidas", 
     ylab = "Frecuencia",
     cex.main = 1.2,
     font.main = 2)

abline(v = mean(logp_agua), col = "red", lwd = 2, lty = 2)   # Media 
abline(v = median(logp_agua), col = "black", lwd = 2)        # Mediana 

# Boxplot

boxplot(logp_agua, 
        col = "steelblue1", 
        border = "black",
        main = "Dispersión de Agua",
        ylab = "Log-Pérdidas",
        outline = TRUE)

# Media
points(1, mean(logp_agua), col = "red", pch = 18, cex = 2)

# Visualización de Outliers

stats <- boxplot.stats(logp_agua)

limite_inf <- stats$stats[1] 
limite_sup <- stats$stats[5]

outliers_menores <- df_hidroagua %>% filter(logp_agua < limite_inf)
outliers_mayores <- df_hidroagua %>% filter(logp_agua > limite_sup)

n_menores <- nrow(outliers_menores)
n_mayores <- nrow(outliers_mayores)
n_total   <- nrow(df_hidroagua)

# Proporciones 
prop_menores <- (n_menores / n_total) * 100
prop_mayores <- (n_mayores / n_total) * 100
prop_total_outliers <- ((n_menores + n_mayores) / n_total) * 100

cat("Cantidad de datos totales:", n_total, "\n\n")
cat("Outliers MENORES:", n_menores, "(", round(prop_menores, 2), "% )\n")
cat("Outliers MAYORES:", n_mayores, "(", round(prop_mayores, 2), "% )\n")
cat("Proporción TOTAL de outliers:", round(prop_total_outliers, 2), "%\n")

# Eliminar outliers
df_hidroagua <- df_hidroagua %>%
  filter(log_perdidas >= limite_inf & log_perdidas <= limite_sup)

# Función de Distribución
plot(ecdf(logp_agua))

#---------
# Modelos
#--------

desplazamiento_agua <- abs(min(df_hidroagua$log_perdidas)) + 0.1

df_hidroagua <- df_hidroagua %>%
  mutate(log_agua_ajustada = log_perdidas + desplazamiento_agua)

logp_agua_ajustada <- df_hidroagua$log_agua_ajustada

# Veamos cual nos conviene revisar
fit.cont(logp_agua_ajustada)

# Modelo Gamma

mod_1ag <- fitdist(logp_agua_ajustada, "gamma", method = "mle")
summary(mod_1ag)

denscomp(mod_1ag)
cdfcomp(mod_1ag)
qqcomp(mod_1ag)
ppcomp(mod_1ag)

hip_1gm <- ad.test(logp_agua_ajustada, 
                   "pgamma", 
                   shape = mod_1ag$estimate["shape"], 
                   rate  = mod_1ag$estimate["rate"])

hip_2gm <- ks.test(logp_agua_ajustada, 
                   "pgamma", 
                   shape = mod_1ag$estimate["shape"], 
                   rate  = mod_1ag$estimate["rate"])

# Resultados
print(hip_1gm)
print(hip_2gm)

# Modelo Log-normal

mod_2ag <- fitdist(logp_agua_ajustada, "lnorm", method = "mle")
summary(mod_2ag)

denscomp(mod_2ag)
cdfcomp(mod_2ag)
qqcomp(mod_2ag)
ppcomp(mod_2ag)

hip_1ln <- ad.test(logp_agua_ajustada,
                   "plnorm",
                   meanlog = mod_2ag$estimate["meanlog"],
                   sdlog   = mod_2ag$estimate["sdlog"])


hip_2ln <- ks.test(logp_agua_ajustada,
                   "plnorm",
                   meanlog = mod_2ag$estimate["meanlog"],
                   sdlog   = mod_2ag$estimate["sdlog"])
# Resultados
print(hip_1ln)
print(hip_2ln)

# Modelo Gamma-Generalizada

mod_3ag <- fitdist(logp_agua_ajustada, "gengamma", 
                   start = list(mu = 1, sigma = 1, Q = 1), 
                   method = "mle")

summary(mod_3ag)

denscomp(mod_3ag)
cdfcomp(mod_3ag)
qqcomp(mod_3ag)
ppcomp(mod_3ag)

hip_1gg <- ad.test(logp_agua_ajustada, 
                   "pgengamma", 
                   mu    = mod_3ag$estimate["mu"], 
                   sigma = mod_3ag$estimate["sigma"], 
                   Q     = mod_3ag$estimate["Q"])

hip_2gg <- ks.test(logp_agua_ajustada, 
                   "pgengamma", 
                   mu    = mod_3ag$estimate["mu"], 
                   sigma = mod_3ag$estimate["sigma"], 
                   Q     = mod_3ag$estimate["Q"])

# Resultados
print(hip_1gg)
print(hip_2gg)

# Contraste de modelos
gofstat(list(mod_1ag,mod_2ag,mod_3ag),fitnames = c("Gamma","Lnormal","Gamma-gen"))

# Nos quedamos con el modelo Gamma-Generaliza

# Comparación de Esperanza teórica con empírica

esp_teorica <- mod_1ag$estimate["shape"] / mod_1ag$estimate["rate"]
media_empirica <- mean(logp_agua_ajustada)

print(esp_teorica)
print(media_empirica)

# Esperanza en millones de pesos
esp_teorica_perdida_agua <- exp(esp_teorica - desplazamiento_agua)
print(esp_teorica_perdida_agua)

# ==============================
#             Viento
# ==============================

# Análsis Descriptivo 

p_viento <- df_hidroviento$perdidas
logp_viento <- df_hidroviento$log_perdidas

# Estadísticos 
resumen <- data.frame(
  n = length(p_viento),
  Media = mean(p_viento),
  Mediana = median(p_viento),
  Desv_Est = sd(p_viento),
  Coef_Var = sd(p_viento) / mean(p_viento),
  Min = min(p_viento),
  Max = max(p_viento)
)

# Estadísticos de la Variable Suavizada

Cuartiles = quantile(logp_viento, c(0.25, 0.50, 0.75))

resumen_log <- data.frame(
  m_p = mean(logp_viento),
  s_p = sd(logp_viento),
  Rango_Muestral = max(logp_viento) - min(logp_viento),
  Varianza = var(logp_viento),
  Asimetria = skewness(logp_viento),
  Curtosis = kurtosis(logp_viento)
)

# Intervalos de Confianza
m_p <- mean(logp_viento)
s_p <- sd(logp_viento)

intervalos <- data.frame(
  Nivel = c("68%", "95%", "99%"),
  Inferior = c(m_p - s_p, m_p - s_p * 2, m_p - s_p * 3),
  Superior = c(m_p + s_p, m_p + s_p * 2, m_p + s_p * 3)
)

# Frecuencia de los tipos de fenómenos
tabla_estado <- table(df_hidroviento$ubicacion_estado)
tabla_tipofen <- table(df_hidroviento$tipo_fenomeno)

cat("REPORTE VIENTO")
print(resumen)
cat("Pérdidas suavizadas")
print(resumen_log)
print(Cuartiles)
cat("Intervalos de Confianza Muestrales")
print(intervalos)
cat("Distribución de Fenómenos")
print(tabla_estado)
print(tabla_tipofen)

# Análisis gráfico

hist(logp_viento, 
     breaks = nclass.Sturges(logp_viento), 
     col = hcl.colors(6, "Zissou 1")[3], 
     border = "white",
     main = "Severidad Viento",
     xlab = "Log-Pérdidas (Escala Logarítmica)", 
     ylab = "Frecuencia de Eventos",
     cex.main = 1.2,
     font.main = 2)

abline(v = mean(logp_viento), col = "red", lwd = 2, lty = 2)   # Media (Punteada Roja)
abline(v = median(logp_viento), col = "black", lwd = 2)        # Mediana (Sólida Negra)

# Notamos que el histograma pareciera tener 2 modas diferentes, veamos que causa conflcito

table(df_hidroviento$tipo_fenomeno)

# Dado que tornado solo tiene un suceso, despreciaremos el datos y separaremos los Ciclones tropicales de Fuertes vientos

df_hidrociclontr <- df_hidroviento %>% filter(tipo_fenomeno == "Ciclón tropical")
df_hidrofuertev <- df_hidroviento %>% filter(tipo_fenomeno == "Fuertes vientos")

df_hidro <- df_hidro[df_hidro$tipo_fenomeno != "Tornado", ]

# ==============================
#        Ciclón tropical
# ==============================

# Análsis Descriptivo 

p_ciclontr <- df_hidrociclontr$perdidas
logp_ciclontr <- df_hidrociclontr$log_perdidas

# Estadísticos 
resumen <- data.frame(
  n = length(p_ciclontr),
  Media = mean(p_ciclontr),
  Mediana = median(p_ciclontr),
  Desv_Est = sd(p_ciclontr),
  Coef_Var = sd(p_ciclontr) / mean(p_ciclontr),
  Min = min(p_ciclontr),
  Max = max(p_ciclontr)
)

# Estadísticos de la Variable Suavizada

Cuartiles = quantile(logp_ciclontr, c(0.25, 0.50, 0.75))

resumen_log <- data.frame(
  m_p = mean(logp_ciclontr),
  s_p = sd(logp_ciclontr),
  Rango_Muestral = max(logp_ciclontr) - min(logp_ciclontr),
  Varianza = var(logp_ciclontr),
  Asimetria = skewness(logp_ciclontr),
  Curtosis = kurtosis(logp_ciclontr)
)

# Intervalos de Confianza
m_p <- mean(logp_ciclontr)
s_p <- sd(logp_ciclontr)

intervalos <- data.frame(
  Nivel = c("68%", "95%", "99%"),
  Inferior = c(m_p - s_p, m_p - s_p * 2, m_p - s_p * 3),
  Superior = c(m_p + s_p, m_p + s_p * 2, m_p + s_p * 3)
)

# Frecuencia de los tipos de fenómenos
tabla_estado <- table(df_hidroagua$ubicacion_estado)
tabla_tipofen <- table(df_hidroagua$tipo_fenomeno)

cat("REPORTE TEMPERATURA CÁLIDA")
print(resumen)
cat("Pérdidas suavizadas")
print(resumen_log)
print(Cuartiles)
cat("Intervalos de Confianza Muestrales")
print(intervalos)
cat("Distribución de Fenómenos")
print(tabla_estado)
print(tabla_tipofen)

# Análisis gráfico

#Histograma

hist(logp_ciclontr, 
     breaks = nclass.Sturges(logp_ciclontr), 
     col = "dodgerblue4", 
     border = "white",
     main = "Severidad de Ciclón Tropical", 
     xlab = "Log-Pérdidas", 
     ylab = "Frecuencia",
     cex.main = 1.2,
     font.main = 2)

abline(v = mean(logp_ciclontr), col = "red", lwd = 2, lty = 2)   # Media 
abline(v = median(logp_ciclontr), col = "black", lwd = 2)        # Mediana 

# Boxplot

boxplot(logp_ciclontr, 
        col = "dodgerblue4", 
        border = "black",
        main = "Dispersión de Ciclón Tropical",
        ylab = "Log-Pérdidas",
        outline = TRUE)

# Media
points(1, mean(logp_ciclontr), col = "red", pch = 18, cex = 2)

# Visualización de Outliers

stats <- boxplot.stats(logp_ciclontr)

limite_inf <- stats$stats[1] 
limite_sup <- stats$stats[5]

outliers_menores <- df_hidrociclontr %>% filter(logp_ciclontr < limite_inf)
outliers_mayores <- df_hidrociclontr %>% filter(logp_ciclontr > limite_sup)

n_menores <- nrow(outliers_menores)
n_mayores <- nrow(outliers_mayores)
n_total   <- nrow(df_hidrociclontr)

# Proporciones 
prop_menores <- (n_menores / n_total) * 100
prop_mayores <- (n_mayores / n_total) * 100
prop_total_outliers <- ((n_menores + n_mayores) / n_total) * 100

cat("Cantidad de datos totales:", n_total, "\n\n")
cat("Outliers MENORES:", n_menores, "(", round(prop_menores, 2), "% )\n")
cat("Outliers MAYORES:", n_mayores, "(", round(prop_mayores, 2), "% )\n")
cat("Proporción TOTAL de outliers:", round(prop_total_outliers, 2), "%\n")

# Eliminar outliers
df_hidrociclontr <- df_hidrociclontr %>%
  filter(log_perdidas >= limite_inf & log_perdidas <= limite_sup)

# Función de Distribución
plot(ecdf(logp_ciclontr))

#---------
# Modelos
#--------

desplazamiento_ciclontr <- abs(min(df_hidrociclontr$log_perdidas)) + 0.1

df_hidrociclontr <- df_hidrociclontr %>%
  mutate(log_ciclontr_ajustada = log_perdidas + desplazamiento_ciclontr)

logp_ciclontr_ajustada <- df_hidrociclontr$log_ciclontr_ajustada

# Veamos cual nos conviene revisar
fit.cont(logp_ciclontr_ajustada)

# Modelo Gompertz

mod_1ctr <- fitdist(logp_ciclontr_ajustada, "gompertz", 
                    method = "mle", 
                    start = list(shape = 0.01, rate = 0.01))
summary(mod_1ctr)

denscomp(mod_1ctr)
cdfcomp(mod_1ctr)
qqcomp(mod_1ctr)
ppcomp(mod_1ctr)

hip_1gp <- ad.test(logp_ciclontr_ajustada, 
                   "pgompertz", 
                   shape = mod_1ctr$estimate["shape"],
                   rate  = mod_1ctr$estimate["rate"])

hip_2gp <- ks.test(logp_ciclontr_ajustada, 
                   "pgompertz", 
                   shape = mod_1ctr$estimate["shape"],
                   rate  = mod_1ctr$estimate["rate"])

# Resultados
print(hip_1gp)
print(hip_2gp)

# Modelo Normal

mod_2ctr <- fitdist(logp_ciclontr_ajustada, "norm", method = "mle")
summary(mod_2ctr)

denscomp(mod_2ctr)
cdfcomp(mod_2ctr)
qqcomp(mod_2ctr)
ppcomp(mod_2ctr)

hip_1nm <- ad.test(logp_ciclontr_ajustada, 
                   "pnorm", 
                   mean = mod_2ctr$estimate["mean"], 
                   sd   = mod_2ctr$estimate["sd"])


hip_2nm <- ks.test(logp_ciclontr_ajustada, 
                   "pnorm", 
                   mean = mod_2ctr$estimate["mean"], 
                   sd   = mod_2ctr$estimate["sd"])
# Resultados
print(hip_1nm)
print(hip_2nm)

# Modelo Logística

mod_3ctr <- fitdist(logp_ciclontr_ajustada, "logis", method = "mle")
summary(mod_3ctr)

denscomp(mod_3ctr)
cdfcomp(mod_3ctr)
qqcomp(mod_3ctr)
ppcomp(mod_3ctr)

hip_1lg <- ad.test(logp_ciclontr_ajustada, 
                   "plogis", 
                   location = mod_3ctr$estimate["location"], 
                   scale    = mod_3ctr$estimate["scale"])

hip_2lg <- ks.test(logp_ciclontr_ajustada, 
                   "plogis", 
                   location = mod_3ctr$estimate["location"], 
                   scale    = mod_3ctr$estimate["scale"])

# Resultados
print(hip_1lg)
print(hip_2lg)

# Contraste de modelos
gofstat(list(mod_1ctr,mod_2ctr,mod_3ctr),fitnames = c("Gompertz","Normal","Logística"))

# Nos quedamos con el modelo Gompertz

# Comparación de Esperanza teórica con empírica

shape_est <- mod_1ctr$estimate["shape"]
rate_est  <- mod_1ctr$estimate["rate"]

z <- rate_est / shape_est
esp_teorica <- (1 / shape_est) * exp(z) * expint::expint_E1(z)

media_empirica <- mean(logp_ciclontr_ajustada)

print(esp_teorica)
print(media_empirica)

# Esperanza en millones de pesos
esp_teorica_perdida_ciclontr <- exp(esp_teorica - desplazamiento_ciclontr)
print(esp_teorica_perdida_ciclontr)

# ==============================
#        Fuertes vientos
# ==============================

# Análsis Descriptivo 

p_fuertev <- df_hidrofuertev$perdidas
logp_fuertev <- df_hidrofuertev$log_perdidas

# Estadísticos 
resumen <- data.frame(
  n = length(p_fuertev),
  Media = mean(p_fuertev),
  Mediana = median(p_fuertev),
  Desv_Est = sd(p_fuertev),
  Coef_Var = sd(p_fuertev) / mean(p_fuertev),
  Min = min(p_fuertev),
  Max = max(p_fuertev)
)

# Estadísticos de la Variable Suavizada

Cuartiles = quantile(logp_fuertev, c(0.25, 0.50, 0.75))

resumen_log <- data.frame(
  m_p = mean(logp_fuertev),
  s_p = sd(logp_fuertev),
  Rango_Muestral = max(logp_fuertev) - min(logp_fuertev),
  Varianza = var(logp_fuertev),
  Asimetria = skewness(logp_fuertev),
  Curtosis = kurtosis(logp_fuertev)
)

# Intervalos de Confianza
m_p <- mean(logp_fuertev)
s_p <- sd(logp_fuertev)

intervalos <- data.frame(
  Nivel = c("68%", "95%", "99%"),
  Inferior = c(m_p - s_p, m_p - s_p * 2, m_p - s_p * 3),
  Superior = c(m_p + s_p, m_p + s_p * 2, m_p + s_p * 3)
)

# Frecuencia de los tipos de fenómenos
tabla_estado <- table(df_hidrofuertev$ubicacion_estado)
tabla_tipofen <- table(df_hidrofuertev$tipo_fenomeno)

cat("REPORTE TEMPERATURA CÁLIDA")
print(resumen)
cat("Pérdidas suavizadas")
print(resumen_log)
print(Cuartiles)
cat("Intervalos de Confianza Muestrales")
print(intervalos)
cat("Distribución de Fenómenos")
print(tabla_estado)
print(tabla_tipofen)

# Análisis gráfico

#Histograma

hist(logp_fuertev, 
     breaks = nclass.Sturges(logp_fuertev), 
     col = "lightgray", 
     border = "white",
     main = "Severidad de Fuertes Vientos", 
     xlab = "Log-Pérdidas", 
     ylab = "Frecuencia",
     cex.main = 1.2,
     font.main = 2)

abline(v = mean(logp_fuertev), col = "red", lwd = 2, lty = 2)   # Media 
abline(v = median(logp_fuertev), col = "black", lwd = 2)        # Mediana 

# Boxplot

boxplot(logp_fuertev, 
        col = "lightgray", 
        border = "black",
        main = "Dispersión de Fuertes Vientos",
        ylab = "Log-Pérdidas",
        outline = TRUE)

# Media
points(1, mean(logp_fuertev), col = "red", pch = 18, cex = 2)

# Visualización de Outliers

stats <- boxplot.stats(logp_fuertev)

limite_inf <- stats$stats[1] 
limite_sup <- stats$stats[5]

outliers_menores <- df_hidrofuertev %>% filter(logp_fuertev < limite_inf)
outliers_mayores <- df_hidrofuertev %>% filter(logp_fuertev > limite_sup)

n_menores <- nrow(outliers_menores)
n_mayores <- nrow(outliers_mayores)
n_total   <- nrow(df_hidrofuertev)

# Proporciones 
prop_menores <- (n_menores / n_total) * 100
prop_mayores <- (n_mayores / n_total) * 100
prop_total_outliers <- ((n_menores + n_mayores) / n_total) * 100

cat("Cantidad de datos totales:", n_total, "\n\n")
cat("Outliers MENORES:", n_menores, "(", round(prop_menores, 2), "% )\n")
cat("Outliers MAYORES:", n_mayores, "(", round(prop_mayores, 2), "% )\n")
cat("Proporción TOTAL de outliers:", round(prop_total_outliers, 2), "%\n")

# Eliminar outliers
df_hidrofuertev <- df_hidrofuertev %>%
  filter(log_perdidas >= limite_inf & log_perdidas <= limite_sup)

# Función de Distribución
plot(ecdf(logp_fuertev))

#---------
# Modelos
#--------

#Trasladamos para tener valores positivos

desplazamiento_fuertev <- abs(min(df_hidrofuertev$log_perdidas)) + 0.1

df_hidrofuertev <- df_hidrofuertev %>%
  mutate(log_fuertev_ajustada = log_perdidas + desplazamiento_fuertev)

logp_fuertev_ajustada <- df_hidrofuertev$log_fuertev_ajustada

# Veamos cual nos conviene revisar
fit.cont(logp_fuertev_ajustada)

# Modelo Logística

mod_1fv <- fitdist(logp_fuertev_ajustada, "logis", method = "mle")
summary(mod_1fv)

denscomp(mod_1fv)
cdfcomp(mod_1fv)
qqcomp(mod_1fv)
ppcomp(mod_1fv)

hip_1lg <- ad.test(logp_fuertev_ajustada, 
                   "plogis", 
                   location = mod_1fv$estimate["location"], 
                   scale    = mod_1fv$estimate["scale"])

hip_2lg <- ks.test(logp_fuertev_ajustada, 
                   "plogis", 
                   location = mod_1fv$estimate["location"], 
                   scale    = mod_1fv$estimate["scale"])

# Resultados
print(hip_1lg)
print(hip_2lg)

# Modelo Normal

mod_2fv <- fitdist(logp_fuertev_ajustada, "norm", method = "mle")
summary(mod_2fv)

denscomp(mod_2fv)
cdfcomp(mod_2fv)
qqcomp(mod_2fv)
ppcomp(mod_2fv)

hip_1nm <- ad.test(logp_fuertev_ajustada, 
                   "pnorm", 
                   mean = mod_2fv$estimate["mean"], 
                   sd   = mod_2fv$estimate["sd"])


hip_2nm <- ks.test(logp_fuertev_ajustada, 
                   "pnorm", 
                   mean = mod_2fv$estimate["mean"], 
                   sd   = mod_2fv$estimate["sd"])
# Resultados
print(hip_1nm)
print(hip_2nm)

# Modelo Weibull

mod_3fv <- fitdist(logp_fuertev_ajustada, "weibull", method = "mle")
summary(mod_3fv)

denscomp(mod_3fv)
cdfcomp(mod_3fv)
qqcomp(mod_3fv)
ppcomp(mod_3fv)

hip_1wb <- ad.test(logp_fuertev_ajustada, 
                   "pweibull", 
                   shape = mod_3fv$estimate["shape"], 
                   scale = mod_3fv$estimate["scale"])


hip_2wb <- ks.test(logp_fuertev_ajustada, 
                   "pweibull", 
                   shape = mod_3fv$estimate["shape"], 
                   scale = mod_3fv$estimate["scale"])
# Resultados
print(hip_1wb)
print(hip_2wb)


# Contraste de modelos
gofstat(list(mod_1fv,mod_2fv,mod_3fv),fitnames = c("Logística","Normal","Weibull"))

# Nos quedamos con la Weibull 

# Comparación de Esperanza teórica con empírica

esp_teorica <- mod_3fv$estimate["scale"] * gamma(1 + 1/mod_3fv$estimate["shape"])
media_empirica <- mean(logp_fuertev_ajustada)

print(esp_teorica)
print(media_empirica)

# Esperanza en millones de pesos
esp_teorica_perdida_fuertev <- exp(esp_teorica - desplazamiento_fuertev)
print(esp_teorica_perdida_fuertev)


# ==============================
#     Promedio de Pérdida
# ==============================

# Calculo de proporciones

n_total <- nrow(df_hidro) 

prop_temp_calida <- nrow(df_hidrotemp_calida) / nrow(df_hidro) 
prop_temp_fria <- nrow(df_hidrotemp_fria) / nrow(df_hidro) 
prop_agua <- nrow(df_hidroagua) / nrow(df_hidro) 
prop_ciclontr <- nrow(df_hidrociclontr) / nrow(df_hidro) 
prop_fuertev <- nrow(df_hidrofuertev) / nrow(df_hidro) 

# Calculo de la pérdida proporcional 

perdida <- (prop_temp_calida * esp_teorica_perdida_tc + 
              prop_temp_fria * esp_teorica_perdida_tf + 
              prop_agua * esp_teorica_perdida_agua + 
              prop_ciclontr * esp_teorica_perdida_ciclontr + 
              prop_fuertev * esp_teorica_perdida_fuertev)

cat("Pérdida esperada en Millones de Pesos: ", perdida, "\n")

# Pérdida esperada para el 2025 

# Factor de actualización oficial 
factor_ajuste <- 143.042 / 137.949  # aprox 1.03692

# Aplicar a tu pérdida esperada
perdida_final <- perdida * factor_ajuste

cat("Pérdida esperada en Millones de Pesos para el 2025: ", perdida_final, "\n")
