url<- "https://raw.githubusercontent.com/rominicky/materiales/main/assets/Provincias.csv"
destino <- "./provincias.csv"
download.file(url, destino)
provincias <- read.csv (destino)

library(tidyverse)
View(provincias)
provincias <- provincias %>%
  rename(viviendas_particulares = Personas.en.viviendas.particulares..2022.)

View(provincias)
media_viviendas_particulares <- provincias %>%
  group_by(Nombre.de.provincia) %>%
  summarise(media_viviendas_particulares = mean(`Viviendas.particulares..2022.`, na.rm = TRUE))

ggplot(provincias, aes(x = reorder(Nombre.de.provincia, `Viviendas.particulares..2022.`), y = `Viviendas.particulares..2022.`)) + 
  geom_bar(stat = "identity", na.rm = TRUE) + 
  coord_flip() +  
  labs(title = "Personas por Viviendas particulares", 
       x = "Provincias",
       y = "Cantidad de personas en viviendas particulares") +
  theme_light()

ggplot(subset(media_viviendas_particulares, media_viviendas_particulares > 0), 
       aes(x = reorder(Nombre.de.provincia, media_viviendas_particulares), y = media_viviendas_particulares, fill = media_viviendas_particulares)) +
  geom_bar(stat = "identity") +
  labs(title = "Valor medio de viviendas particulares por provincia",
       x = "Provincia",
       y = "Valor medio de viviendas particulares") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")  

# Calcular las medias de personas en viviendas particulares para 2010 y 2022
media_personas_viviendas <- provincias %>%
  group_by(Nombre.de.provincia) %>%
  summarise(media_personas_2010 = mean(`Personas.en.viviendas.particulares..2010.`, na.rm = TRUE),
            media_personas_2022 = mean(`Viviendas.particulares..2022.`, na.rm = TRUE))

# Convertir los datos a formato largo para ggplot
media_personas_viviendas_long <- media_personas_viviendas %>%
  pivot_longer(cols = c(media_personas_2010, media_personas_2022), 
               names_to = "Año", 
               values_to = "Media_Personas")
library(ggplot2)

# Graficar las medias de personas en viviendas particulares
ggplot(media_personas_viviendas_long, aes(x = Nombre.de.provincia, y = Media_Personas, fill = Año)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Medias de Personas en Viviendas Particulares (2010 vs 2022)",
       x = "Provincia",
       y = "Media de Personas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Filtrar las provincias de la región de Cuyo
provincias_cuyo <- provincias %>%
  filter(Nombre.de.provincia %in% c("Mendoza", "San Juan", "San Luis"))

# Calcular las medias de personas en viviendas particulares para 2010 y 2022 para la región de Cuyo
media_personas_viviendas_cuyo <- provincias_cuyo %>%
  group_by(Nombre.de.provincia) %>%
  summarise(media_personas_2010 = mean(`Personas.en.viviendas.particulares..2010.`, na.rm = TRUE),
            media_personas_2022 = mean(`Viviendas.particulares..2022.`, na.rm = TRUE))

# Convertir los datos a formato largo para ggplot
media_personas_viviendas_cuyo_long <- media_personas_viviendas_cuyo %>%
  pivot_longer(cols = c(media_personas_2010, media_personas_2022), 
               names_to = "Año", 
               values_to = "Media_Personas")

# Graficar las medias de personas en viviendas particulares para la región de Cuyo
ggplot(media_personas_viviendas_cuyo_long, aes(x = Nombre.de.provincia, y = Media_Personas, fill = Año)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Medias de Personas en Viviendas Particulares en la Región de Cuyo (2010 vs 2022)",
       x = "Provincia",
       y = "Media de Personas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



  