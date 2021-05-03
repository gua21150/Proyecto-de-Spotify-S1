"Elaborado por:
  Mariel Alejandra Guamuche Recinos
  Diana Paola Díaz Márquez 
  Pedro Javier Marroquin Abrego 
  Paulo Raul Sánchez González
"

#Librerias a utilizar
library(dplyr)
library(ggplot2)
library(ggthemes)

theme_set(theme_economist())

# Datasets disponibles de Spotify
spotify <- read.csv("data.csv")
spotifyyear <- read.csv("data_by_year.csv")
spotifyartist <- read.csv("data_by_artist.csv")
spotifygenres <- read.csv("data_by_genres.csv")

#----- ANALISIS DE DATOS -------

# 1. Como ha cambiado la popularidad con el acousticness en el paso de las décadas
  #1920's
  dec20=filter(spotifyyear, grepl("192", year))
  ggplot(dec20, aes(x=acousticness, popularity))+ 
    geom_line()+
    labs(title="Década de los 20")
  summarise(dec20, mean(acousticness), mean(popularity))
  
  #1930's
  dec30=filter(spotifyyear, grepl("193", year))
  ggplot(dec30, aes(x=acousticness, popularity))+
    geom_line()+
    labs(title="Década de los 30")
  summarise(dec30, mean(acousticness), mean(popularity))
  
  #1940's
  dec40=filter(spotifyyear, grepl("194", year))
  summarise(dec40, mean(acousticness),  mean(popularity))
  ggplot(dec40, aes(x=acousticness, popularity))+ 
    geom_line()+
    labs(title="Década de los 40")
  
  #1950's
  dec50=filter(spotifyyear, grepl("195", year))
  summarise(dec50, mean(acousticness), mean(popularity))
  ggplot(dec50, aes(x=acousticness, popularity))+
    geom_line()+
    labs(title="Década de los 50")
  
  #1960's
  dec60=filter(spotifyyear, grepl("196", year))
  summarise(dec60, mean(acousticness), mean(popularity))
  ggplot(dec60, aes(x=acousticness, popularity))+
    geom_line()+
    labs(title="Década de los 60")
  
  #1970's
  dec70=filter(spotifyyear, grepl("197", year))
  summarise(dec70, mean(acousticness), mean(popularity))
  ggplot(dec70, aes(acousticness, popularity))+ 
    geom_line()+
    labs(title="Década de los 70")
  
  #1980's
  dec80=filter(spotifyyear, grepl("198", year))
  summarise(dec80, mean(acousticness), mean(popularity))
  ggplot(dec80, aes(acousticness, popularity))+
    geom_line()+
    labs(title="Década de los 80")
  
  #1990's
  dec90=filter(spotifyyear, grepl("199", year))
  summarise(dec90, mean(acousticness), mean(popularity))
  ggplot(dec90, aes(acousticness, popularity))+
    geom_line()+
    labs(title="Década de los 90")
  
  #2000's
  dec00=filter(spotifyyear, grepl("200", year))
  summarise(dec00, mean(acousticness), mean(popularity))
  ggplot(dec00, aes(acousticness, popularity))+
    geom_line()+
    labs(title="Década de los 2000")
  
  #2010's
  dec10=filter(spotifyyear, grepl("201", year))
  summarise(dec10, mean(acousticness), mean(popularity))
  ggplot(dec10, aes(acousticness, popularity))+ 
    geom_line()+
    labs(title="Década de los 2010")
  
  #2020?s
  dec21=filter(spotifyyear, grepl("202", year))
  summarise(dec21, mean(acousticness), mean(popularity))
  ggplot(dec21, aes(acousticness, popularity))+
    geom_line()+
    labs(title="Década de los 2020")

# 2. Que decada tuvo la acusticidad más alta 
  acusmasalta20s<-filter (spotifyyear,year>= "1920", year<="1929")
  promacus20s<- summarise(acusmasalta20s, mean(acousticness))
  acusmasalta30s<-filter (spotifyyear,year>= "1930", year<="1939")
  promacus30s<-summarise(acusmasalta30s, mean(acousticness))
  acusmasalta40s<-filter (spotifyyear,year>= "1940", year<="1949")
  promacus40s<-summarise(acusmasalta40s, mean(acousticness))
  acusmasalta50s<-filter (spotifyyear,year>= "1950", year<="1959")
  promacus50s<-summarise(acusmasalta50s, mean(acousticness))
  acusmasalta60s<-filter (spotifyyear,year>= "1960", year<="1969")
  promacus60s<-summarise(acusmasalta60s, mean(acousticness))
  acusmasalta70s<-filter (spotifyyear,year>= "1970", year<="1979")
  promacus70s<-summarise(acusmasalta70s, mean(acousticness))
  acusmasalta80s<-filter (spotifyyear,year>= "1980", year<="1989")
  promacus80s<-summarise(acusmasalta80s, mean(acousticness))
  acusmasalta90s<-filter (spotifyyear,year>= "1990", year<="1999")
  promacus90s<-summarise(acusmasalta90s, mean(acousticness))
  acusmasalta2000s<-filter (spotifyyear,year>= "2000", year<="2009")
  promacus2000s<-summarise(acusmasalta2000s, mean(acousticness))
  acusmasalta2010s<-filter (spotifyyear,year>= "2010", year<="2019")
  promacus2010s<-summarise(acusmasalta2010s, mean(acousticness))
  maxdecacus<-(max(promacus20s,promacus30s, promacus40s, promacus50s, promacus60s, promacus70s, promacus80s, promacus90s, promacus2000s, promacus2010s))
  
  "La acustica más alta se dio durante los años 40s con 0.8783936"

# 3. Acusticidad vs popularidad
  acustic_popu=select(spotifyyear, acousticness, popularity)
  ggplot(acustic_popu, aes(x=acousticness, popularity))+ geom_line()  
  
# 4. ¿Tiene relación el beat de la canción con la bailabilidad de esta?
 
   # tempo -  beat, danceability - bailabilidad
  relacion_bailabilidad <- spotify %>% select(danceability, tempo) %>% group_by(tempo) %>% summarise(medias = mean(danceability))
  # Grafica con poblacion 
  gg2 <- ggplot(relacion_bailabilidad, aes(x=tempo, y=medias)) + 
    geom_point(aes(col=tempo)) +
    geom_smooth(method="loess", se=F) + 
    labs(subtitle="Población total", 
         y="Media bailabilidad", 
         x="Beat", 
         title="Grafica bailabilidad y tempo")
  plot(gg2)
  
  # Grafica con muestra aleatoria 
  sample_frac()
  x <- sample_frac(relacion_bailabilidad, .005) # 0.005 corresponde al 0.05%
  x2 <- ggplot(x, aes(x=tempo, y=medias)) + 
    geom_point(aes(col=tempo))  +
    geom_smooth(method="loess", se=F) + 
    labs(subtitle="Muestra del 0.5% de los datos", 
         y="Media bailabilidad", 
         x="Beat", 
         title="Grafica bailabilidad y tempo")
  plot(x2)
  
# 5. Relación entre speechiness y loudness
  spelou<- select(spotifygenres,speechiness,loudness)
  #Población total 
  gg2 <- ggplot(spelou, aes(x=speechiness, y=loudness)) + 
    geom_point(aes(col=speechiness)) +
    geom_smooth(method="loess", se=F) + 
    labs(subtitle="Poblacion total", 
         y="Loudness", 
         x="Speechiness", 
         title="Grafica Loudness y Spechiness")
  plot(gg2)
  
# 6. Duración vs danceability
  tiempo_dance=select(spotifyyear, duration_ms, danceability )
  ggplot(tiempo_dance, aes(x=duration_ms, y=danceability)) +geom_line()  
  
# 7. Duración vs popularidad
  spotifyred=select(spotifyyear, duration_ms, popularity)
  ggplot(spotifyred, aes(x=duration_ms, y=popularity)) +geom_line() 

# 8. ¿Ha aumentado la explicitud de las canciones durante los años?
"Se saco la media de explicitud de cada año para ver si hay un aumento o no"
  explicitness <-spotify %>% select(explicit, year) %>% group_by(year) %>% summarise(media_explicitud = mean(explicit)) 
  
  #lectura de información de máximo, mínimo, media, mediano y quartiles
  info_expl <- explicitness %>% summarise(quantile(media)) 
  #cada dato dentro de info_expl es el mínimo, el primer cuartil, el mediano, el tercer cuartil y el máximo
  
  mínimo_expli = filter(explicitness, media<= 0) #todos los años cuya explicitud no fue registrada o no tienen explicidad en absoluto
  máx_expl = filter(explicitness, media >= 0.452155625) #año con explicitud máxima
  
  #promedio del explicitness entre todos los años
  promedio_explicitness <- explicitness %>% summarise(mean(media))
  # Grafica
   ggplot(explicitness, aes(x=year, y=media_explicitud)) + 
    geom_col() +
    labs(title="Aumento de explicitud en canciones", subtitle="",y="Media de explicitud", x="Año")
  mayor <- summarise(explicitness, max(media_explicitud))
  
# 9. En qué año o década hubo más lanzamientos de canciones
  year <- spotify %>% group_by(year) %>% summarise(total_canciones_year = sum(year))
  year1 <- year %>% summarise(max(total_canciones_year))
  year2 <- year %>% filter(total_canciones_year %in% year1)
    # Grafica
  ggplot(year, aes(x=year, y=total_canciones_year)) + 
      geom_area() +
      labs(title="Relación de lanzamiento de canciones", subtitle="Canciones por año", y="Cantidad de canciones", x="Año")

# 10. género vs popularidad
  max(spotifygenres$popularity)
  genre2=filter(spotifygenres, popularity=="79")
  
# 11. ¿Qué años tuvieron mayor popularidad?
  #agrupamos la popularidad de todos los años y los promediamos 
  popu_por_año <- spotify %>% select(popularity, year) %>% group_by(year) %>% summarise(popularidad = mean(popularity))
  
  #sacar top 5 de años más populares
  max_1 <- popu_por_año %>% summarise(máximo = max(popularidad))
  top_1 <- filter(popu_por_año, popularidad >= 50.8735) #el año con más popularidad es 2001
  
  max_2 <- popu_por_año %>% filter(popularidad <=50.8735) %>% summarise(máximo = max(popularidad))
  top_2 <- filter(popu_por_año, popularidad >= 47.96892 & popularidad < 50.8735) #el segundo año con más popularidad es 2004
  
  max_3 <- popu_por_año %>% filter(popularidad <= 47.96892) %>% summarise (máximo = max(popularidad))
  top_3 <- filter(popu_por_año, popularidad >= 46.2590 & popularidad < 47.96892) #el tercer año con más popularidad es 2000
  
  max_4 <- popu_por_año %>% filter(popularidad <= 46.2590) %>% summarise (máximo = max(popularidad))
  top_4 <- filter(popu_por_año, popularidad >= 45.76740 & popularidad < 46.2590) #el cuarto año con más popularidad es 2003
  
  max_5 <- popu_por_año %>% filter(popularidad <= 45.76740) %>% summarise (máximo = max(popularidad))
  top_5 <- filter(popu_por_año, popularidad >= 45.67542 & popularidad < 45.76740) #el quinto año con más popularidad es 2002
  
  
  #crear una nueva tabla de datos únicamente con los top 5 años de mayor popularidad
  year<-c(2001, 2004, 2000, 2003, 2002)
  popularity <- c(max_1$máximo, max_2$máximo, max_3$máximo, max_4$máximo, max_5$máximo)
  mejores_5_unidos <- data.frame(cbind(year, popularity))
  
  #hacer una gráfica de toda la popularidad
  ggplot(popu_por_año,aes(x=year, y = popularidad), title = "popularidad de cada año") + geom_col()
  
  #gráfica de los top 5
  ggplot(mejores_5_unidos,aes(x=year, y = popularity), title = "5 años con mayor popularidad") + geom_col()