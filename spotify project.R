"Elaborado por:
  Mariel Alejandra Guamuche Recinos
  Diana Paola D�az M�rquez 
  Pedro Javier Marroquin Abrego 
  Paulo Raul S�nchez Gonz�lez
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

# 1. Como ha cambiado la popularidad con el acousticness en el paso de las d�cadas
  #1920's
  dec20=filter(spotifyyear, grepl("192", year))
  ggplot(dec20, aes(x=acousticness, popularity))+ 
    geom_line()+
    labs(title="D�cada de los 20")
  summarise(dec20, mean(acousticness), mean(popularity))
  
  #1930's
  dec30=filter(spotifyyear, grepl("193", year))
  ggplot(dec30, aes(x=acousticness, popularity))+
    geom_line()+
    labs(title="D�cada de los 30")
  summarise(dec30, mean(acousticness), mean(popularity))
  
  #1940's
  dec40=filter(spotifyyear, grepl("194", year))
  summarise(dec40, mean(acousticness),  mean(popularity))
  ggplot(dec40, aes(x=acousticness, popularity))+ 
    geom_line()+
    labs(title="D�cada de los 40")
  
  #1950's
  dec50=filter(spotifyyear, grepl("195", year))
  summarise(dec50, mean(acousticness), mean(popularity))
  ggplot(dec50, aes(x=acousticness, popularity))+
    geom_line()+
    labs(title="D�cada de los 50")
  
  #1960's
  dec60=filter(spotifyyear, grepl("196", year))
  summarise(dec60, mean(acousticness), mean(popularity))
  ggplot(dec60, aes(x=acousticness, popularity))+
    geom_line()+
    labs(title="D�cada de los 60")
  
  #1970's
  dec70=filter(spotifyyear, grepl("197", year))
  summarise(dec70, mean(acousticness), mean(popularity))
  ggplot(dec70, aes(acousticness, popularity))+ 
    geom_line()+
    labs(title="D�cada de los 70")
  
  #1980's
  dec80=filter(spotifyyear, grepl("198", year))
  summarise(dec80, mean(acousticness), mean(popularity))
  ggplot(dec80, aes(acousticness, popularity))+
    geom_line()+
    labs(title="D�cada de los 80")
  
  #1990's
  dec90=filter(spotifyyear, grepl("199", year))
  summarise(dec90, mean(acousticness), mean(popularity))
  ggplot(dec90, aes(acousticness, popularity))+
    geom_line()+
    labs(title="D�cada de los 90")
  
  #2000's
  dec00=filter(spotifyyear, grepl("200", year))
  summarise(dec00, mean(acousticness), mean(popularity))
  ggplot(dec00, aes(acousticness, popularity))+
    geom_line()+
    labs(title="D�cada de los 2000")
  
  #2010's
  dec10=filter(spotifyyear, grepl("201", year))
  summarise(dec10, mean(acousticness), mean(popularity))
  ggplot(dec10, aes(acousticness, popularity))+ 
    geom_line()+
    labs(title="D�cada de los 2010")
  
  #2020?s
  dec21=filter(spotifyyear, grepl("202", year))
  summarise(dec21, mean(acousticness), mean(popularity))
  ggplot(dec21, aes(acousticness, popularity))+
    geom_line()+
    labs(title="D�cada de los 2020")

# 2. Que decada tuvo la acusticidad m�s alta 
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
  
  "La acustica m�s alta se dio durante los a�os 40s con 0.8783936"

# 3. Acusticidad vs popularidad
  acustic_popu=select(spotifyyear, acousticness, popularity)
  ggplot(acustic_popu, aes(x=acousticness, popularity))+ geom_line()  
  
# 4. �Tiene relaci�n el beat de la canci�n con la bailabilidad de esta?
 
   # tempo -  beat, danceability - bailabilidad
  relacion_bailabilidad <- spotify %>% select(danceability, tempo) %>% group_by(tempo) %>% summarise(medias = mean(danceability))
  # Grafica con poblacion 
  gg2 <- ggplot(relacion_bailabilidad, aes(x=tempo, y=medias)) + 
    geom_point(aes(col=tempo)) +
    geom_smooth(method="loess", se=F) + 
    labs(subtitle="Poblaci�n total", 
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
  
# 5. Relaci�n entre speechiness y loudness
  spelou<- select(spotifygenres,speechiness,loudness)
  #Poblaci�n total 
  gg2 <- ggplot(spelou, aes(x=speechiness, y=loudness)) + 
    geom_point(aes(col=speechiness)) +
    geom_smooth(method="loess", se=F) + 
    labs(subtitle="Poblacion total", 
         y="Loudness", 
         x="Speechiness", 
         title="Grafica Loudness y Spechiness")
  plot(gg2)
  
# 6. Duraci�n vs danceability
  tiempo_dance=select(spotifyyear, duration_ms, danceability )
  ggplot(tiempo_dance, aes(x=duration_ms, y=danceability)) +geom_line()  
  
# 7. Duraci�n vs popularidad
  spotifyred=select(spotifyyear, duration_ms, popularity)
  ggplot(spotifyred, aes(x=duration_ms, y=popularity)) +geom_line() 

# 8. �Ha aumentado la explicitud de las canciones durante los a�os?
"Se saco la media de explicitud de cada a�o para ver si hay un aumento o no"
  explicitness <-spotify %>% select(explicit, year) %>% group_by(year) %>% summarise(media_explicitud = mean(explicit)) 
  
  #lectura de informaci�n de m�ximo, m�nimo, media, mediano y quartiles
  info_expl <- explicitness %>% summarise(quantile(media)) 
  #cada dato dentro de info_expl es el m�nimo, el primer cuartil, el mediano, el tercer cuartil y el m�ximo
  
  m�nimo_expli = filter(explicitness, media<= 0) #todos los a�os cuya explicitud no fue registrada o no tienen explicidad en absoluto
  m�x_expl = filter(explicitness, media >= 0.452155625) #a�o con explicitud m�xima
  
  #promedio del explicitness entre todos los a�os
  promedio_explicitness <- explicitness %>% summarise(mean(media))
  # Grafica
   ggplot(explicitness, aes(x=year, y=media_explicitud)) + 
    geom_col() +
    labs(title="Aumento de explicitud en canciones", subtitle="",y="Media de explicitud", x="A�o")
  mayor <- summarise(explicitness, max(media_explicitud))
  
# 9. En qu� a�o o d�cada hubo m�s lanzamientos de canciones
  year <- spotify %>% group_by(year) %>% summarise(total_canciones_year = sum(year))
  year1 <- year %>% summarise(max(total_canciones_year))
  year2 <- year %>% filter(total_canciones_year %in% year1)
    # Grafica
  ggplot(year, aes(x=year, y=total_canciones_year)) + 
      geom_area() +
      labs(title="Relaci�n de lanzamiento de canciones", subtitle="Canciones por a�o", y="Cantidad de canciones", x="A�o")

# 10. g�nero vs popularidad
  max(spotifygenres$popularity)
  genre2=filter(spotifygenres, popularity=="79")
  
# 11. �Qu� a�os tuvieron mayor popularidad?
  #agrupamos la popularidad de todos los a�os y los promediamos 
  popu_por_a�o <- spotify %>% select(popularity, year) %>% group_by(year) %>% summarise(popularidad = mean(popularity))
  
  #sacar top 5 de a�os m�s populares
  max_1 <- popu_por_a�o %>% summarise(m�ximo = max(popularidad))
  top_1 <- filter(popu_por_a�o, popularidad >= 50.8735) #el a�o con m�s popularidad es 2001
  
  max_2 <- popu_por_a�o %>% filter(popularidad <=50.8735) %>% summarise(m�ximo = max(popularidad))
  top_2 <- filter(popu_por_a�o, popularidad >= 47.96892 & popularidad < 50.8735) #el segundo a�o con m�s popularidad es 2004
  
  max_3 <- popu_por_a�o %>% filter(popularidad <= 47.96892) %>% summarise (m�ximo = max(popularidad))
  top_3 <- filter(popu_por_a�o, popularidad >= 46.2590 & popularidad < 47.96892) #el tercer a�o con m�s popularidad es 2000
  
  max_4 <- popu_por_a�o %>% filter(popularidad <= 46.2590) %>% summarise (m�ximo = max(popularidad))
  top_4 <- filter(popu_por_a�o, popularidad >= 45.76740 & popularidad < 46.2590) #el cuarto a�o con m�s popularidad es 2003
  
  max_5 <- popu_por_a�o %>% filter(popularidad <= 45.76740) %>% summarise (m�ximo = max(popularidad))
  top_5 <- filter(popu_por_a�o, popularidad >= 45.67542 & popularidad < 45.76740) #el quinto a�o con m�s popularidad es 2002
  
  
  #crear una nueva tabla de datos �nicamente con los top 5 a�os de mayor popularidad
  year<-c(2001, 2004, 2000, 2003, 2002)
  popularity <- c(max_1$m�ximo, max_2$m�ximo, max_3$m�ximo, max_4$m�ximo, max_5$m�ximo)
  mejores_5_unidos <- data.frame(cbind(year, popularity))
  
  #hacer una gr�fica de toda la popularidad
  ggplot(popu_por_a�o,aes(x=year, y = popularidad), title = "popularidad de cada a�o") + geom_col()
  
  #gr�fica de los top 5
  ggplot(mejores_5_unidos,aes(x=year, y = popularity), title = "5 a�os con mayor popularidad") + geom_col()