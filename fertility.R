###############################################
# Tasa de natalidad mundial, últimos 60 años
# 
# Fuente: https://data.worldbank.org/indicator/SP.DYN.TFRT.IN?end=2020&start=1960&view=chart
# 2023-04-26  version actual:   2023-05-01
# GRUPO H 
###############################################


# Cargamos Librerias
install.packages("ggplot2")
install.packages("reshape2")
install.packages("countrycode")
install.packages("clipr")
install.packages("tidyr")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("ggrepel")


library(ggplot2)
library(reshape2)
library(countrycode)
library(clipr)
library(tidyr)
library(rnaturalearth)
library(dplyr)
library(ggrepel)

# Cargamos el dataset API_SP.DYN.TFRT.IN_DS2_en_csv_v2_5358686.csv. Evito las primeras 3 filas que poseen metadatos innecesarios.
datos_raw <- read.csv("API_SP.DYN.TFRT.IN_DS2_en_csv_v2_5358686.csv", skip = 3, header = TRUE)

# Eliminamos ciertas columnas con más metadatos innecesarios para mi gráfico, usando la función "subset."
datos_clean <- subset(datos_raw, select = -c(3:4))
# También eliminamos columnas que no poseen datos, que en este caso son solo 2 años al final de la tabla.
# Pisamos el data frame solo en este caso por ser parte del mismo "paso" y para no crear tantos data frames innecesarios.
datos_clean <- datos_clean[, !apply(is.na(datos_clean), 2, all)]

# Eliminamos países que no tienen datos para ningun año
datos_ready <- datos_clean[apply(datos_clean[,-1], 1, function(x) any(!is.na(x))),]

# Visualizamos el data frame
View(datos_ready)

# Revisamos y confirmamos las clases de las variables (columnas) del data frame.
class(datos_ready)
column_classes <- sapply(datos_ready, class)
print(column_classes)

###########################################################################################
# Queremos realizar un gráfico estilo heatmap como el que utiliza Github. (Ver ejemplo -> https://script.gs/content/images/2022/04/labnol-github-aug-2019.png)
# Al ser muchos países, queremos agrupar por continente. El data set original ya contiene los continentes como filas, 
# pero a fines de demostrar las capacidades de R en este TP, vamos a tomar el camino más largo de agrupar los países por continente,
# y sacar la media aritmética por continente.

# Utilizaremos la librería countrycode para obtener el continente de cada país.
# Primero creo una nueva columna 'continente' en mi data frame
datos_por_continente <- datos_ready
datos_por_continente$Continent <- NA

# Al usar la librería countrycode voy a realizar 2 pasos: 
# Asignar un continente a cada país válido, 
# y asignar NA como continente a aquellas filas que no sean países.
# Producira un WARNING en consola por aquellos valores invalidos.
datos_por_continente$Continent <- countrycode(datos_por_continente$Country.Code,
                                              origin = "iso3c",
                                              destination = "continent",
                                              nomatch = NA)

# Muevo la columna continente al comienzo del data frame
datos_por_continente <- datos_por_continente[, c(ncol(datos_por_continente), 1:(ncol(datos_por_continente)-1))]

# Uso na.omit para eliminar las filas que no correspondan a un continente válido
datos_por_continente <- na.omit(datos_por_continente)

# Finalmente, agrupo las filas por continente calculando la media aritmética de cada uno, omitiendo los valores NA
columnas_year <- colnames(datos_por_continente)[!colnames(datos_por_continente) %in% c("Country.Name", "Country.Code", "Continent")]

# Create a new data frame with one row per continent, calculating the arithmetic mean for each year
media_aritmetica_por_year <- aggregate(datos_por_continente[columnas_year],
                                       by = list(Continent = datos_por_continente$Continent),
                                       FUN = mean, na.rm = TRUE)

# Reformateamos el data frame a formato "long" usando melt para que sea compatible con las funciones de ploteo de ggplot2
# Esto nos servira para generar nuestro gráfico estilo "heatmap."
long_data <- melt(media_aritmetica_por_year, id.vars = "Continent", variable.name = "Year", value.name = "Value")

# Convertimos las variables de año a tipo de dato "factor", para que aparezcan ordenados correctamente en el eje x.
long_data$Year <- factor(long_data$Year, levels = unique(long_data$Year))

# Finalmente, creamos el gráfico de heatmap usando ggplot.
# Hay 60 campos por cada continente, pero lo mostramos a modo de gradiente para visualizar mejor la información.
# También reducimos la cantidad de etiquetas que muestra ggplot para la variable año, para mejorar la legibilidad,
# imprimiento solo 1 etiqueta cada 10 años (a pesar de que el gráfico contiene información con granularidad de cada año.)
heatmap <- ggplot(long_data, aes(x = Year, y = Continent, fill = Value)) +
  geom_tile(color = NA) +
  scale_fill_gradient2(name = "Tasa", low = "white", mid = "lightyellow", high = "orange", midpoint = median(long_data$Value), guide = "colorbar") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 65, vjust = 0.5, hjust=1),
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  xlab("Año") +
  ylab("Continente") +
  scale_x_discrete(labels = function(x) {
    labels <- gsub("^X", "", x)
    ifelse(seq_along(labels) == 1 | (seq_along(labels) - 1) %% 10 == 0, labels, "")
  }) +
  ggtitle("Tasa de fertilidad mundial") +
  labs(caption = "Nacimientos por mujer desde 1960")

# Mostar el heatmap
print(heatmap)


# Identify the year columns and other columns you want to keep
year_columns <- colnames(datos_ready)[3:ncol(datos_ready)]
keep_columns <- c("Country.Name", "Country.Code")

# Calculate the row-wise average for the year columns
row_average <- rowMeans(datos_ready[year_columns], na.rm = TRUE)

# Create a new data frame with the desired columns and the calculated average
datos_avg <- data.frame(datos_ready[keep_columns], Average = row_average)

# Get the world map data using the rnaturalearth library
world_map <- ne_countries(scale = "medium", returnclass = "sf")

# Merge the world map data with the datos_avg data frame using the ISO3C country codes
world_map_avg <- merge(world_map, datos_avg, by.x = "iso_a3", by.y = "Country.Code")

# Create a map plot with ggplot2, using colors to represent the average values
ggplot(data = world_map_avg) +
  geom_sf(aes(fill = Average)) +
  scale_fill_gradient(low = "lightblue", high = "darkred", na.value = "gray90", name = "Average") +
  theme_minimal() +
  ggtitle("Average values by country")
