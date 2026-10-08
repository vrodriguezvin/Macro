
### PROYECTO SOBRE MACROECONOMÍA ###


# ==============================================================
## 1. CARGAR DATOS
# ==============================================================

library(readxl)  # leer archivos Excel
library(dplyr)   # seleccionar, filtrar y transformar
library(tidyr)   # cambiar la estructura de las tablas
library(stringr) # trabajar con texto
library(lubridate)
library(ggplot2)

# Ruta del archivo del BCU
ruta_excel <- "BCU_datos.xlsx"


# Cargar datos del BCU
datos_bcu <- read_excel(
  ruta_excel,
  sheet = "Valores_K",  # hoja que queremos leer
  skip = 7              # ignoramos las primeras 7 filas
)


# Ver datos originales
View(datos_bcu)

# ==============================================================
## 2. ORDENAR Y LIMPIAR
# ==============================================================


# Renombramos la segunda columna porque contiene
# los nombres de las variables
names(datos_bcu)[2] <- "variable"


# Pasamos los trimestres de columnas a filas
datos_largos <- datos_bcu %>%
  pivot_longer(
    cols = starts_with(c("I ", "II ", "III ", "IV ")),
    names_to = "trimestre",
    values_to = "valor"
  )


View(datos_largos)


# Ver nombres disponibles
unique(datos_largos$variable)


# Nos quedamos con las variables que nos interesan
datos_macro <- datos_largos %>%
  filter(
    variable %in% c(
      "PRODUCTO INTERNO BRUTO",
      "Formación Bruta de Capital",
      "Exportaciones de bienes y servicios",
      "(-) Importaciones de bienes y servicios",
      "Hogares",
      "Gobierno e ISFLSH"
    )
  )


# Ponemos cada variable como columna
datos_macro <- datos_macro %>%
  select(
    variable,
    trimestre,
    valor
  ) %>%
  pivot_wider(
    names_from = variable,
    values_from = valor
  )


# Renombramos variables para trabajar más fácilmente
datos_macro <- datos_macro %>%
  rename(
    pbi = `PRODUCTO INTERNO BRUTO`,
    inversion = `Formación Bruta de Capital`,
    exportaciones = `Exportaciones de bienes y servicios`,
    importaciones = `(-) Importaciones de bienes y servicios`,
    consumo = `Hogares`,
    gasto_publico = `Gobierno e ISFLSH`
  )


# Eliminamos el * de algunos trimestres
datos_macro <- datos_macro %>%
  mutate(
    trimestre = gsub("\\*", "", trimestre)
  )


# Separamos trimestre y año
datos_macro <- datos_macro %>%
  separate(
    trimestre,
    into = c("trim", "año"),
    sep = " ",
    remove = FALSE
  )


# Convertimos año a número y trimestre romano a número
datos_macro <- datos_macro %>%
  mutate(
    
    año = as.integer(año),
    
    trimestre_num = case_when(
      trim == "I" ~ 1,
      trim == "II" ~ 2,
      trim == "III" ~ 3,
      trim == "IV" ~ 4
    )
  )


# Nos quedamos con las columnas necesarias
datos_macro <- datos_macro %>%
  select(
    trimestre,
    año,
    trimestre_num,
    pbi,
    consumo,
    gasto_publico,
    inversion,
    exportaciones,
    importaciones
  )


# Ordenamos cronológicamente
datos_macro <- datos_macro %>%
  arrange(
    año,
    trimestre_num
  )


View(datos_macro)

# ==============================================================
## 3i. AGREGAR INFORMACIÓN --> DESEMPLEO
# ==============================================================

# Ruta del archivo del INE
ruta_desempleo <- "INE_desempleo.csv"


# Leer archivo del INE
lineas <- readLines(
  ruta_desempleo,
  encoding = "latin1",
  warn = FALSE
)


# Separar filas por ;
filas <- strsplit(
  lineas[-c(1:5)],
  ";",
  fixed = TRUE
)


# Nos quedamos con las primeras 10 columnas útiles
matriz_desempleo <- t(
  sapply(
    filas,
    function(x) x[1:10]
  )
)


# Convertimos a data frame
desempleo_raw <- as.data.frame(
  matriz_desempleo,
  stringsAsFactors = FALSE
)


# Ponemos nombres claros
names(desempleo_raw) <- c(
  "periodo",
  "actividad_total",
  "actividad_hombres",
  "actividad_mujeres",
  "empleo_total",
  "empleo_hombres",
  "empleo_mujeres",
  "desempleo",
  "desempleo_hombres",
  "desempleo_mujeres"
)


View(desempleo_raw)


# Creamos una base trimestral de desempleo
desempleo_trimestral <- desempleo_raw %>%
  mutate(
    
    periodo = trimws(periodo),
    
    trimestre_num = case_when(
      grepl("Enero.*Marzo", periodo) ~ 1,
      grepl("Abril.*Junio", periodo) ~ 2,
      grepl("Julio.*Setiembre|Julio.*Septiembre", periodo) ~ 3,
      grepl("Octubre.*Diciembre", periodo) ~ 4,
      TRUE ~ NA_real_
    ),
    
    año = 2000 + as.integer(
      sub(
        ".*\\/([0-9]{2}).*",
        "\\1",
        periodo
      )
    ),
    
    desempleo = as.numeric(
      gsub(
        ",",
        ".",
        desempleo,
        fixed = TRUE
      )
    )
  ) %>%
  
  filter(
    !is.na(año),
    !is.na(trimestre_num)
  ) %>%
  
  select(
    año,
    trimestre_num,
    desempleo
  ) %>%
  
  distinct(
    año,
    trimestre_num,
    .keep_all = TRUE
  ) %>%
  
  arrange(
    año,
    trimestre_num
  )


View(desempleo_trimestral)


# Comprobamos que no haya trimestres repetidos
desempleo_trimestral %>%
  count(
    año,
    trimestre_num
  ) %>%
  filter(n > 1)


# Eliminamos cualquier columna de desempleo anterior
# por si volvemos a correr esta parte del script
datos_macro <- datos_macro %>%
  select(
    -matches("^desempleo")
  )


# Unimos desempleo a datos_macro
datos_macro <- datos_macro %>%
  left_join(
    desempleo_trimestral,
    by = c(
      "año",
      "trimestre_num"
    )
  )


View(datos_macro)


# Comprobamos que no se hayan agregado filas
nrow(datos_macro)


# Ver cuántos datos de desempleo faltan
sum(is.na(datos_macro$desempleo))


# Ver exactamente qué trimestres no tienen desempleo
datos_macro %>%
  filter(
    is.na(desempleo)
  ) %>%
  select(
    trimestre,
    año,
    trimestre_num
  )

# ==============================================================
## 3.ii AGREGAR INFORMACIÓN --> VARIABLE INFLACIÓN
# ==============================================================

# Ruta del archivo del INE
ruta_inflacion <- "INE_inflacion.csv"

# Cargar archivo
inflacion_raw <- read.csv2(
  ruta_inflacion,
  skip = 7,
  header = TRUE,
  stringsAsFactors = FALSE,
  fileEncoding = "latin1"
)

View(inflacion_raw)
names(inflacion_raw)

# Crear tabla trimestral de inflación

inflacion_trimestral <- inflacion_raw %>%
  mutate(
    mes = substr(Mes.y.año, 1, 3),
    
    año = as.integer(
      paste0("20", substr(Mes.y.año, 5, 6))
    )
  ) %>%
  
  filter(
    mes %in% c("mar", "jun", "sep", "dic")
  ) %>%
  
  mutate(
    trimestre_num = case_when(
      mes == "mar" ~ 1,
      mes == "jun" ~ 2,
      mes == "sep" ~ 3,
      mes == "dic" ~ 4
    ),
    
    inflacion = Trimestre
  ) %>%
  
  select(
    año,
    trimestre_num,
    inflacion
  ) %>%
  
  distinct(
    año,
    trimestre_num,
    .keep_all = TRUE
  ) %>%
  
  arrange(
    año,
    trimestre_num
  )

View(inflacion_trimestral)

# Eliminar inflación anterior si ya existe
datos_macro <- datos_macro %>%
  select(-matches("^inflacion"))

# Unir inflación a datos_macro
datos_macro <- datos_macro %>%
  left_join(
    inflacion_trimestral,
    by = c("año", "trimestre_num")
  )

View(datos_macro)

# ==============================================================
## 3.iii AGREGAR INFORMACIÓN --> VARIABLE TIPO DE CAMBIO
# ==============================================================

# 1. Leer y unir todos los archivos CSV

carpeta_tc <- "datos_tc"

archivos_tc <- list.files(
  carpeta_tc,
  pattern = "\\.csv$",
  full.names = TRUE
)

tipo_cambio_raw <- archivos_tc %>%
  lapply(function(x) {
    read.csv2(
      x,
      stringsAsFactors = FALSE,
      fileEncoding = "latin1"
    )
  }) %>%
  bind_rows()


# Ver resultado
View(tipo_cambio_raw)

# Ver nombres de columnas
names(tipo_cambio_raw)

# Ver cantidad total de filas
nrow(tipo_cambio_raw)

# 2. Convertir Fecha a formato fecha

tipo_cambio_raw <- tipo_cambio_raw %>%
  mutate(
    Fecha = dmy(Fecha)
  )

# 3. Crear año y trimestre

tipo_cambio_raw <- tipo_cambio_raw %>%
  mutate(
    año = year(Fecha),
    trimestre_num = quarter(Fecha)
  )


View(tipo_cambio_raw)


# 4. Convertir Venta a número

tipo_cambio_raw <- tipo_cambio_raw %>%
  mutate(
    Venta = as.numeric(
      gsub(",", ".", Venta, fixed = TRUE)
    )
  )

# 5. Crear promedio trimestral del tipo de cambio

tipo_cambio_trimestral <- tipo_cambio_raw %>%
  group_by(
    año,
    trimestre_num
  ) %>%
  summarise(
    tipo_cambio = mean(
      Venta,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(
    año,
    trimestre_num
  )


View(tipo_cambio_trimestral)

# 6. Revisar que no haya trimestres repetidos

tipo_cambio_trimestral %>%
  count(
    año,
    trimestre_num
  ) %>%
  filter(n > 1)

# 7. Eliminar tipo de cambio previo si ya existe

datos_macro <- datos_macro %>%
  select(
    -matches("^tipo_cambio")
  )


# 8. Unir tipo de cambio a la base principal

datos_macro <- datos_macro %>%
  left_join(
    tipo_cambio_trimestral,
    by = c(
      "año",
      "trimestre_num"
    )
  )


# Ver resultado final
View(datos_macro)

# 9. Comprobaciones

# La cantidad de filas debería seguir siendo 41
nrow(datos_macro)

# Ver cuántos faltantes hay en tipo de cambio
sum(is.na(datos_macro$tipo_cambio))

# Ver qué trimestres no encontraron tipo de cambio
datos_macro %>%
  filter(
    is.na(tipo_cambio)
  ) %>%
  select(
    trimestre,
    año,
    trimestre_num
  )

# ==============================================================
## 4. CREAR VARIABLES PARA EL ANÁLISIS
# ==============================================================
    # Ordenamos cronológicamente y creamos variaciones interanuales

datos_macro <- datos_macro %>%
  arrange(año, trimestre_num) %>%
  mutate(
    
# Crecimiento interanual del PBI
  crecimiento_pbi = 100 * (pbi / lag(pbi, 4) - 1),
    
# Crecimiento interanual de la inversión
  crecimiento_inversion = 100 * (inversion / lag(inversion, 4) - 1),
    
# Crecimiento interanual de las exportaciones
  crecimiento_exportaciones = 100 * (exportaciones / lag(exportaciones, 4) - 1),
    
# Variación interanual del tipo de cambio
  variacion_tipo_cambio = 100 * (tipo_cambio / lag(tipo_cambio, 4) - 1)
  )

# --> la base del análisis ahora tendría: creicimiento_pbi (variable dependiente)
  #inflación, desempleo, crecimiento_inversion. crecimiento_exportaciones, variacion_tipo_cambio
  #queda más coherente que comprar el crecimeinto PBI con exportaciones e inversiones simplemente en niveles

# ==============================================================
## 5. VARIABLES DE RESUMEN
# ==============================================================

library(ggplot2)

resumen_variables <- datos_macro %>%
  select(
    crecimiento_pbi,
    inflacion,
    desempleo,
    crecimiento_inversion,
    crecimiento_exportaciones,
    variacion_tipo_cambio
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "valor"
  ) %>%
  group_by(variable) %>%
  summarise(
    media = mean(valor, na.rm = TRUE),
    mediana = median(valor, na.rm = TRUE),
    desvio = sd(valor, na.rm = TRUE),
    minimo = min(valor, na.rm = TRUE),
    maximo = max(valor, na.rm = TRUE)
  )

View(resumen_variables)

# ==============================================================
## 6. VISUALIZACIÓN GRÁFICA
# ==============================================================

## 1. PREPARAMOS LOS DATOS PARA LOS GRÁFICOS

# Crear una fecha a partir del año y trimestre
datos_macro <- datos_macro %>%
  mutate(
    fecha = as.Date(
      paste0(
        año,
        "-",
        case_when(
          trimestre_num == 1 ~ "01-01",
          trimestre_num == 2 ~ "04-01",
          trimestre_num == 3 ~ "07-01",
          trimestre_num == 4 ~ "10-01"
        )
      )
    )
  )

## 2. GRÁFICOS DE EVOLUCIÓN EN EL TIEMPO

# A. Evolución del crecimiento del PBI

ggplot(
  datos_macro,
  aes(x = fecha, y = crecimiento_pbi)
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Evolución del crecimiento interanual del PBI",
    x = "Fecha",
    y = "Crecimiento del PBI (%)",
    caption = "Fuente: elaboración propia con datos del BCU"
  ) +
  theme_minimal()

# B. Evolución de la inflación

ggplot(
  datos_macro,
  aes(x = fecha, y = inflacion)
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Evolución de la inflación",
    x = "Fecha",
    y = "Inflación (%)",
    caption = "Fuente: INE"
  ) +
  theme_minimal()

# C. Evolución del desempleo

ggplot(
  datos_macro,
  aes(x = fecha, y = desempleo)
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Evolución de la tasa de desempleo",
    x = "Fecha",
    y = "Desempleo (%)",
    caption = "Fuente: INE"
  ) +
  theme_minimal()

# D. Evolución de la inversión

ggplot(
  datos_macro,
  aes(x = fecha, y = crecimiento_inversion)
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Evolución del crecimiento de la inversión",
    x = "Fecha",
    y = "Crecimiento de la inversión (%)",
    caption = "Fuente: elaboración propia con datos del BCU"
  ) +
  theme_minimal()

# E. Evolución de las exportaciones

ggplot(
  datos_macro,
  aes(x = fecha, y = crecimiento_exportaciones)
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Evolución del crecimiento de las exportaciones",
    x = "Fecha",
    y = "Crecimiento de las exportaciones (%)",
    caption = "Fuente: elaboración propia con datos del BCU"
  ) +
  theme_minimal()

# F. Evolución del tipo de cambio

ggplot(
  datos_macro,
  aes(x = fecha, y = variacion_tipo_cambio)
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Evolución de la variación del tipo de cambio",
    x = "Fecha",
    y = "Variación del tipo de cambio (%)",
    caption = "Fuente: elaboración propia con datos del BCU"
  ) +
  theme_minimal()

## 3. DISTRIBUCIÓN Y VALORES EXTREMOS

# A. Histograma del crecimiento del PBI

ggplot(
  datos_macro,
  aes(x = crecimiento_pbi)
) +
  geom_histogram(
    bins = 10
  ) +
  labs(
    title = "Distribución del crecimiento del PBI",
    x = "Crecimiento del PBI (%)",
    y = "Frecuencia"
  ) +
  theme_minimal()

# B. Boxplot del crecimiento del PBI

ggplot(
  datos_macro,
  aes(y = crecimiento_pbi)
) +
  geom_boxplot() +
  labs(
    title = "Valores extremos del crecimiento del PBI",
    x = "",
    y = "Crecimiento del PBI (%)"
  ) +
  theme_minimal()


## 4. RELACIÓN ENTRE EL PBI Y LAS VARIABLES EXPLICATIVAS

# A. Crecimiento del PBI vs desempleo

ggplot(
  datos_macro,
  aes(
    x = desempleo,
    y = crecimiento_pbi
  )
) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  labs(
    title = "Relación entre crecimiento del PBI y desempleo",
    x = "Tasa de desempleo (%)",
    y = "Crecimiento del PBI (%)"
  ) +
  theme_minimal()

# B. Crecimiento del PBI vs inflación

ggplot(
  datos_macro,
  aes(
    x = inflacion,
    y = crecimiento_pbi
  )
) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  labs(
    title = "Relación entre crecimiento del PBI e inflación",
    x = "Inflación (%)",
    y = "Crecimiento del PBI (%)"
  ) +
  theme_minimal()

# C. Crecimiento del PBI vs inversión

ggplot(
  datos_macro,
  aes(
    x = crecimiento_inversion,
    y = crecimiento_pbi
  )
) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  labs(
    title = "Relación entre crecimiento del PBI e inversión",
    x = "Crecimiento de la inversión (%)",
    y = "Crecimiento del PBI (%)"
  ) +
  theme_minimal()

# D. Crecimiento del PBI vs exportaciones

ggplot(
  datos_macro,
  aes(
    x = crecimiento_exportaciones,
    y = crecimiento_pbi
  )
) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  labs(
    title = "Relación entre crecimiento del PBI y exportaciones",
    x = "Crecimiento de las exportaciones (%)",
    y = "Crecimiento del PBI (%)"
  ) +
  theme_minimal()

# E. Crecimiento del PBI vs tipo de cambio

ggplot(
  datos_macro,
  aes(
    x = variacion_tipo_cambio,
    y = crecimiento_pbi
  )
) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  labs(
    title = "Relación entre crecimiento del PBI y tipo de cambio",
    x = "Variación del tipo de cambio (%)",
    y = "Crecimiento del PBI (%)"
  ) +
  theme_minimal()


# ==============================================================
## 7. CORRELACIONES
# ==============================================================

datos_correlacion <- datos_macro %>%
  select(
    crecimiento_pbi,
    inflacion,
    desempleo,
    crecimiento_inversion,
    crecimiento_exportaciones,
    variacion_tipo_cambio
  )

correlaciones <- cor(
  datos_correlacion,
  use = "complete.obs"
)

round(correlaciones, 2)
View(correlaciones)

# ==============================================================
## 8. MODELO DE REGRESIÓN LINEAL MÚLTIPLE
# ==============================================================

# Creamos una base solo con las variables del modelo
datos_modelo <- datos_macro %>%
  select(
    crecimiento_pbi,
    inflacion,
    desempleo,
    crecimiento_inversion,
    crecimiento_exportaciones,
    variacion_tipo_cambio
  ) %>%
  drop_na()

# Ver cuántas observaciones entran efectivamente al modelo
nrow(datos_modelo)

# 1. Estimar el modelo

modelo <- lm(
  crecimiento_pbi ~
    inflacion +
    desempleo +
    crecimiento_inversion +
    crecimiento_exportaciones +
    variacion_tipo_cambio,
  data = datos_modelo
)

# Ver resultados
summary(modelo)

# ==============================================================
## 8. MODELO DE REGRESIÓN LINEAL MÚLTIPLE
# ==============================================================

# Creamos una base solo con las variables del modelo
datos_modelo <- datos_macro %>%
  select(
    crecimiento_pbi,
    inflacion,
    desempleo,
    crecimiento_inversion,
    crecimiento_exportaciones,
    variacion_tipo_cambio
  ) %>%
  drop_na()

# Ver cuántas observaciones entran efectivamente al modelo
nrow(datos_modelo)


# Estimar el modelo

modelo <- lm(
  crecimiento_pbi ~
    inflacion +
    desempleo +
    crecimiento_inversion +
    crecimiento_exportaciones +
    variacion_tipo_cambio,
  data = datos_modelo
)

# Ver resultados
summary(modelo)

# ==============================================================
## 9. DIAGNÓSTICO DEL MODELO
# ==============================================================

# 1. Gráficos básicos de diagnóstico
par(mfrow = c(2, 2))
plot(modelo)
par(mfrow = c(1, 1))

# 2. Multicolinealidad
library(car)
vif(modelo)

# VIF cercano a 1 = bien
# VIF > 5 = puede haber problema
# VIF > 10 = problema fuerte

# 3. Heterocedasticidad
library(lmtest)
bptest(modelo)

# p-value > 0.05 = no hay evidencia de heterocedasticidad
# p-value < 0.05 = hay evidencia de heterocedasticidad

# 4. Autocorrelación de residuos
dwtest(modelo)

# estadístico Durbin-Watson ≈ 2 = bien
# muy por debajo de 2 = autocorrelación positiva
# muy por encima de 2 = autocorrelación negativa