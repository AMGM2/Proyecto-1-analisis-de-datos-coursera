# instalando los paquetes

install.packages("tidyverse")
install.packages("skimr")
install.packages("janitor")
install.packages("dplyr")
install.packages("plyr")
install.packages("ggplot2")

# Activandolos

library(tidyverse)
library(skimr)
library(janitor)
library(dplyr)
library(plyr)
library(ggplot2)


# Leyendo y uniendo los 12 archivos en uno solo para asi poder limpiarlo

Trip_data_2021_Sucio <- list.files("C:/Users/Sol PC/Downloads/Proyecto_1_Analisis_De_Datos_Coursera/tripdata_proyect" , pattern = "*.csv" , full.names =  TRUE) %>%  lapply(read.csv) %>%  bind_rows


#Mirar los primeras filas
head(Trip_data_2021_Sucio) 

#Mirar las ultimas filas
tail(Trip_data_2021_Sucio)
# Muestra todo los datos de las columnas
glimpse(Trip_data_2021_Sucio)
# Nos fijamos la estructura de las columnas
str(Trip_data_2021_Sucio)
# Cuenta y cuenta la minima , media y maxima de las columnas como otros datos interesantes
summary(Trip_data_2021_Sucio)

#Mirar todo
View(Trip_data_2021_Sucio)

#leyendo el csv que creamos
Trip_data_2021 <- read_csv("C:/Users/Sol PC/Downloads/Proyectos/Proyecto_1_Analisis_De_Datos_Coursera/Proyecto_1_Analisis_De_Datos/tripdata/Trip_data_2021_sin_limpiar.csv")

# Organizar y filtrar datos
ride_lenght <- Trip_data_2021_Sucio$started_at - Trip_data_2021_Sucio$ended_at
Trip_data_2021_Sucio <- cbind(Trip_data_2021_Sucio[,  c(1,2)], started_at, ended_at)

arrange(member_casual)

#Limpiando los datos

#Quitando todos los NA

Trip_data_2021_Limpio <- drop_na(Trip_data_2021_Sucio)
                                


# Añadiendo columna a la lista de fecha .mes y dia de cada viaje
# Nis permitira agregar por fecha mes y dia

tripdata_ttm_clean$date <- as.Date(tripdata_ttm_clean$started_at) 
tripdata_ttm_clean$month <- format(as.Date(tripdata_ttm_clean$date), "%m")
tripdata_ttm_clean$day <- format(as.Date(tripdata_ttm_clean$date), "%d")
tripdata_ttm_clean$year <- format(as.Date(tripdata_ttm_clean$date), "%Y")
tripdata_ttm_clean$day_of_week <- format(as.Date(tripdata_ttm_clean$date), "%A")

# Añade "ride_length" calculo (en segundos)

Trip_data_2021_Limpio$ride_length <- difftime(Trip_data_2021_Limpio$ended_at,Trip_data_2021_Limpio$started_at)




str(Trip_data_2021_Limpio)


Trip_data_2021_Limpio_V2 <- Trip_data_2021_Limpio[!(Trip_data_2021_Limpio$start_station_name == "HQ QR" | Trip_data_2021_Limpio$ride_length<0),]
#Guardamos los datos limpios para su uso otro dia 
write.csv(Trip_data_2021_Limpio_V2, "Trip_data_2021_Limpio_V2.csv")

# Analizar

mean(Trip_data_2021_Limpio_V2$ride_length) #(total ride length / rides)
median(Trip_data_2021_Limpio_V2$ride_length)
max(Trip_data_2021_Limpio_V2$ride_length) # El viaje mas largo
min(Trip_data_2021_Limpio_V2) #El viaje mas corto

summary(Trip_data_2021_Limpio_V2$ride_length)


aggregate(Trip_data_2021_Limpio_V2$ride_length~all_trips_v2$member_casual, FUN = mean)





Trip_data_2021_Limpio_V2 %>% 
  group_by(member_casual) %>% 
  summarise(avg_ride_lenth = mean(ride_length), median_ride_length = median(ride_length), max_ride_length = max(ride_length), min_ride_length = min(ride_length))




Trip_data_2021_Limpio_V2 %>% 
  group_by(member_casual) %>% 
  summarise(avg_ride_lenth = mean(ride_length), median_ride_length = median(ride_length), max_ride_length = max(ride_length), min_ride_length = min(ride_length))




Trip_data_2021_Limpio_V2$day_of_week <- ordered(Trip_data_2021_Limpio_V2$day_of_week, levels=c("Domingo", "Lunes", "Martes", "Miercoles", "Jueves", "Viernes", "Sabado"))




aggregate(Trip_data_2021_Limpio_V2$ride_length ~ Trip_data_2021_Limpio_V2$member_casual + Trip_data_2021_Limpio_V2$day_of_week, FUN = mean)





Trip_Data_2021_Limpio_V2.1 %>% 
  group_by(member_casual) %>% 
  summarise(avg_ride_lenth = mean(ride_length), median_ride_length = median(ride_length), max_ride_length = max(ride_length), min_ride_length = min(ride_length)) <- Trip_data_2021_Limpio_V2 %>% 
  group_by(member_casual, day_of_week, rideable_type) %>%  
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>%         
  arrange(member_casual, day_of_week, rideable_type)

write.csv(Trip_Data_2021_Limpio_V2.1, "Trip_Data_2021_Limpio_V2.1.csv")



head(Trip_Data_2021_Limpio_V2.1)




Trip_Data_2021_Limpio_V2.1 %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") + labs(title='Promedio semanal duracion de los viajes por cada tipo de usuario')


Trip_Data_2021_Limpio_V2.1 %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + labs(title='Average Weekly Number of Rides for each customer type')



# Concusiones
